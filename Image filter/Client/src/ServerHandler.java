import java.io.*;
import java.net.Socket;
import java.util.ArrayList;

public class ServerHandler implements Runnable {

    public ServerHandler(String host, int port) {
        try {
            this.server = new Socket(host, port);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        loadedImages = new ArrayList<>();
        processedImages = new ArrayList<>();
        newImageID = 0;
    }

    @Override
    public void run() {
        try {
            in = new DataInputStream(server.getInputStream());
            out = new DataOutputStream(server.getOutputStream());

            System.out.println("Welcome! Press 'q' for exit.");

            requestAndGetFiltersList();

            for (int i = 0; i < filters.length; i++) {
                System.out.println((i + 1) + ". " + filters[i]);
            }

            long time = System.currentTimeMillis();

            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            while (!server.isInputShutdown()) {

                /* Read from console and send message to server. */
                if (System.currentTimeMillis() - time > 1000) {
                    if (reader.ready()) {
                        String entry = reader.readLine();
                        out.writeUTF(entry);
                        out.flush();
                        if (entry.equals("q")) {
                            break;
                        }
                    }
                    sendImage("pictures/example_4.png", 2);
                    time = System.currentTimeMillis();
                }

                getMessage();
            }

            System.out.println("loaded images: " + loadedImages.size() + "    processed images: " + processedImages.size());

            in.close();
            out.close();
            server.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    /*
     * Message format (from server):
     * 1. Getting filters list:
     *   "filters" <N> <String> ... <String>
     *                 |_________N_________|
     *
     * 2. Getting filtering progress:
     *   "progress" <Image ID> <%>
     *
     * 3. Getting processed image:
     *   "image" <Image ID> <Bytes number> <Bytes>
     */
    private void getMessage() throws IOException {
        if (in.available() > 0) {
            String entry = in.readUTF();
            if (entry.equals(PROCESSING_PROGRESS_CODE)) {
                int imageID = in.readInt();
                int percentWork = in.readInt();
                Image image = findImageById(loadedImages, imageID);
                if (image != null) {
                    image.setPercentWork(percentWork);
                    System.out.println("ID=" + imageID + " : Percent is " + percentWork);
                }
                else {
                    System.out.println("Image == null!");
                }
                return;
            }
            if (entry.equals(IMAGE_CODE)) {
                int imageID = in.readInt();
                int imageSize = in.readInt();
                byte[] byteArray = new byte[imageSize];
                in.readFully(byteArray);
                Image image = findImageById(loadedImages, imageID);
                if (image != null) {
                    loadedImages.remove(image);
                    image.setData(byteArray);
                    processedImages.add(image);
                    image.saveImage("saves/");

                    System.out.println("ID=" + imageID + " : Result image size is " + imageSize + "   In fact: " + byteArray.length);
                }
                else {
                    System.out.println("Image == null!");
                }
            }
        }
    }

    /*
     * Message format (to server):
     * 1. Send request to receive filters list:
     *   "filters"
     *
     * 2. Send image for filtering:
     *   "image" <Filter ID> <Image ID> <Image width> <Image height> <Bytes number> <Bytes>
     *
     * 3. Cancel last loaded image:
     *   "cancel" <Image ID>
     */
    private void sendImage (String fileName, int filterID) throws IOException {
        Image image = new Image(fileName, newImageID++);
        loadedImages.add(image);
        out.writeUTF(IMAGE_CODE);
        out.writeInt(filterID);
        out.writeInt(image.getID());
        out.writeInt(image.getWidth());
        out.writeInt(image.getHeight());
        out.writeInt(image.getSizeInBytes());
        out.write(image.getSourceBytes());
        out.flush();
    }

    private void cancelLastTask() throws IOException {
        Image image = loadedImages.get(loadedImages.size() - 1);
        if (!image.isReady()) {
            out.writeUTF(CANCELED_TASK_ID_CODE);
            out.writeInt(image.getID());
            loadedImages.remove(image);
        }
    }

    private void requestAndGetFiltersList() throws IOException {
        out.writeUTF(FILTERS_LIST_CODE);
        out.flush();
        if (in.readUTF().equals(FILTERS_LIST_CODE)) {
            int filtersNumber = in.readInt();
            filters = new String[filtersNumber];
            for (int i = 0; i < filtersNumber; i++) {
                filters[i] = in.readUTF();
            }
        }
    }

    private Image findImageById(ArrayList<Image> images, int id) {
        for (Image image : images) {
            if (image.getID() == id) {
                return image;
            }
        }
        return null;
    }

    private DataInputStream in;
    private DataOutputStream out;
    private Socket server;
    private String[] filters;
    private ArrayList<Image> loadedImages;
    private ArrayList<Image> processedImages;
    private int newImageID;

    private static final String IMAGE_CODE = "image";
    private static final String CANCELED_TASK_ID_CODE = "cancel";
    private static final String FILTERS_LIST_CODE = "filters";
    private static final String PROCESSING_PROGRESS_CODE = "progress";
}
