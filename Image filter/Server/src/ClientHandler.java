import java.io.*;
import java.net.Socket;
import java.util.ArrayList;
import java.util.concurrent.ExecutorService;

class ClientHandler implements Runnable {

    public ClientHandler(Socket client, ExecutorService executorService) {
        this.client = client;
        this.tasks = new ArrayList<>();
        this.executorService = executorService;
    }

    @Override
    public void run() {
        try {
            printLog("Connected");
            filters = Server.getFilters();
            in = new DataInputStream(client.getInputStream());
            out = new DataOutputStream(client.getOutputStream());

            long time = System.currentTimeMillis();

            while (!client.isInputShutdown() && !client.isOutputShutdown()) {
                int resultCode = getMessage();
                if (resultCode == -1) {
                    break;
                }
                if (resultCode == 0) {
                    printLog("Unknown request code.");
                }

                /* Send progress/result about all processing images to client. */
                if (System.currentTimeMillis() - time > PERIOD_OF_PROGRESS_UPDATING && tasks.size() > 0) {
                    FilterHandler[] doneTasks = new FilterHandler[tasks.size()];
                    int index = 0;
                    for (FilterHandler task : tasks) {
                        int imageID = task.getImageID();
                        if (task.isBreak() || task.isDone()) {
                            doneTasks[index++] = task;
                        }
                        else {
                            sendMessage(PROCESSING_PROGRESS_CODE, null, imageID, task.getPercentWork());
                        }
                    }

                    /* Delete done tasks. */
                    for (int i = 0; i < index; i++) {
                        tasks.remove(doneTasks[i]);
                    }

                    time = System.currentTimeMillis();
                }
            }

            in.close();
            out.close();
            client.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    /*
     * Message format (from client):
     * 1. Getting request to close seance:
     *   "q"
     *
     * 2. Getting request to process image:
     *   "image" <Filter ID> <Image ID> <Image width> <Image height> <Bytes number> <Bytes>
     *
     * 3. Getting request to cancel task:
     *   "cancel" <Image ID>
     *
     * 4. Getting request to send filters list:
     *   "filters"
     */
    private int getMessage() throws IOException {
        if (in.available() > 0) {
            String entry = in.readUTF();
            if (entry.equals(DISCONNECTED_CLIENT_CODE)) {
                printLog("Disconnected");
                return -1;
            }
            if (entry.equals(FILTERS_LIST_CODE)) {
                printLog("Filters list request");
                sendMessage(FILTERS_LIST_CODE, null);
                return 1;
            }
            if (entry.equals(IMAGE_CODE)) {
                int filterID = in.readInt();
                int imageID = in.readInt();
                int imageWidth = in.readInt();
                int imageHeight = in.readInt();
                int byteArrayLength = in.readInt();

                printLog("filterID= " + filterID +
                        ", imageID= " + imageID +
                        ", imageSize= " + byteArrayLength +
                        " bytes, imageWidth= " + imageWidth +
                        ", imageHeight= " + imageHeight);

                byte[] byteArray = new byte[byteArrayLength];
                in.readFully(byteArray);

                /*
                 * 1. Add filtering task to ArrayList
                 * 2. Add filtering task to ExecutorService (for execution)
                 */
                FilterHandler filterHandler = new FilterHandler(this, imageWidth, imageHeight, byteArray, filterID, imageID);
                tasks.add(filterHandler);
                executorService.execute(filterHandler);
                return 1;
            }
            if (entry.equals(CANCELED_TASK_ID_CODE)) {
                int imageID = in.readInt();
                findAndStopTaskByImageId(imageID);
                printLog("Canceling of task " + imageID);
                return 1;
            }
            return 0;
        }
        return 1;
    }

    /*
     * Message format (to client):
     * 1. Send filters list:
     *   "filters" <N> <String> ... <String>
     *                 |_________N_________|
     *
     * 2. Send filtering progress:
     *   "progress" <Image ID> <%>
     *
     * 3. Send processed image:
     *   "image" <Image ID> <Bytes number> <Bytes>
     */
    public synchronized void sendMessage(String code, byte[] data, int... values) throws IOException {
        out.writeUTF(code);
        switch (code) {
            case FILTERS_LIST_CODE:
                out.writeInt(filters.length);
                for (int i = 0; i < filters.length; i++) {
                    out.writeUTF(filters[i]);
                }
                printLog("Filters list are sent");
                break;
            case PROCESSING_PROGRESS_CODE:
                out.writeInt(values[0]);
                out.writeInt(values[1]);
                break;
            case IMAGE_CODE:
                out.writeInt(values[0]);
                out.writeInt(values[1]);
                out.write(data);
        }
        out.flush();
    }

    private boolean findAndStopTaskByImageId(int id) {
        for (FilterHandler task : tasks) {
            if (task.getImageID() == id) {
                task.stop();
                return true;
            }
        }
        return false;
    }

    private void printLog(String message) {
        System.out.println("Client [port=" + client.getPort() + ", localport=" +
                client.getLocalPort() + "]: " + message);
    }

    private Socket client;
    private DataInputStream in;
    private DataOutputStream out;
    private ArrayList<FilterHandler> tasks;
    private ExecutorService executorService;
    private String[] filters;

    private static final long PERIOD_OF_PROGRESS_UPDATING = 1_000;

    private static final String FILTERS_LIST_CODE = "filters";
    private static final String PROCESSING_PROGRESS_CODE = "progress";
    public static final String IMAGE_CODE = "image";
    private static final String CANCELED_TASK_ID_CODE = "cancel";
    private static final String DISCONNECTED_CLIENT_CODE = "q";
}