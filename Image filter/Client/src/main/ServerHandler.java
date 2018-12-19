package main;

import data.Repository;

import java.io.*;
import java.net.Socket;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;

public class ServerHandler implements Runnable {

    public ServerHandler(String host, int port) {
        try {
            this.server = new Socket(host, port);
            loadedImages = new ArrayList<>();
            newImageID = 0;
            queries = Repository.getQueriesQueue();
            images = Repository.getImagesArrayList();
            isUpdated = new AtomicBoolean(false);
            isConnected = new AtomicBoolean(false);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void update() {
        isUpdated.set(false);
    }

    /* Getters */
    public String[] getFiltersList() {
        return filters;
    }
    public boolean isConnected() {
        return isConnected.get();
    }
    public boolean isUpdated() {
        return isUpdated.get();
    }

    @Override
    public void run() {
        try {
            in = new DataInputStream(server.getInputStream());
            out = new DataOutputStream(server.getOutputStream());

            while (!server.isInputShutdown()) {
                getQuery();
                getMessage();
            }

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
     *   "progress" <main.Image ID> <%>
     *
     * 3. Getting processed image:
     *   "image" <main.Image ID> <Bytes number> <Bytes>
     */
    private void getMessage() throws IOException {
        if (in.available() > 0) {
            String entry = in.readUTF();
            if (entry.equals(Query.FILTERS_LIST_CODE)) {
                int filtersNumber = in.readInt();
                filters = new String[filtersNumber];
                for (int i = 0; i < filtersNumber; i++) {
                    filters[i] = in.readUTF();
                }
                isConnected.set(true);
                return;
            }
            if (entry.equals(Query.PROCESSING_PROGRESS_CODE)) {
                int imageID = in.readInt();
                int percentWork = in.readInt();
                Image image = findImageById(loadedImages, imageID);
                if (image != null) {
                    image.setStatus(Image.UPDATED);
                    image.setPercentWork(percentWork);
                    isUpdated.set(true);
                }
                return;
            }
            if (entry.equals(Query.IMAGE_SENDING_CODE)) {
                int imageID = in.readInt();
                int imageSize = in.readInt();
                byte[] byteArray = new byte[imageSize];
                in.readFully(byteArray);
                Image image = findImageById(loadedImages, imageID);
                if (image != null) {
                    image.setStatus(Image.PROCESSED);
                    loadedImages.remove(image);
                    image.setData(byteArray);
                    isUpdated.set(true);
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
     *   "image" <Filter ID> <main.Image ID> <main.Image width> <main.Image height> <Bytes number> <Bytes>
     *
     * 3. Cancel last loaded image:
     *   "cancel" <main.Image ID>
     */
    private void getQuery() {
        Query query = queries.poll();
        if (query != null) {
            try {
                switch (query.getCode()) {
                    case Query.DISCONNECTED_CLIENT_CODE:
                        request(Query.DISCONNECTED_CLIENT_CODE);
                        break;
                    case Query.IMAGE_SENDING_CODE:
                        if (loadedImages.size() < MAX_LOADIND_IMAGES_NUMBER) {
                            sendImage(query.getFileName(), query.getID());
                        }
                        break;
                    case Query.CANCELED_TASK_ID_CODE:
                        cancelLastTask();
                        break;
                    case Query.FILTERS_LIST_CODE:
                        request(Query.FILTERS_LIST_CODE);
                        break;
                    case Query.SAVE_IMAGE_CODE:
                        int imageID = query.getID();
                        if (imageID < images.size()) {
                            images.get(imageID).saveImage(query.getFileName());
                        }
                        break;
                }
            }
            catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private void sendImage (String fileName, int filterID) throws IOException {
        Image image = new Image(fileName, newImageID++);
        loadedImages.add(image);
        images.add(image);
        image.setStatus(Image.UPDATED);
        out.writeUTF(Query.IMAGE_SENDING_CODE);
        out.writeInt(filterID);
        out.writeInt(image.getID());
        out.writeInt(image.getWidth());
        out.writeInt(image.getHeight());
        out.writeInt(image.getSizeInBytes());
        out.write(image.getSourceBytes());
        out.flush();
        isUpdated.set(true);
    }

    private void cancelLastTask() throws IOException {
        int index = loadedImages.size() - 1;
        if (index >= 0) {
            Image image = loadedImages.get(index);
            if (image.getStatus() != Image.PROCESSED) {
                out.writeUTF(Query.CANCELED_TASK_ID_CODE);
                out.writeInt(image.getID());
                loadedImages.remove(image);
                image.setStatus(Image.CANCELED);
                isUpdated.set(true);
            }
        }
    }

    private void request(String code) throws IOException {
        out.writeUTF(code);
        out.flush();
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
    private int newImageID;
    private ArrayList<Image> loadedImages;
    private CopyOnWriteArrayList<Image> images;
    private AtomicBoolean isConnected;
    private AtomicBoolean isUpdated;
    private ConcurrentLinkedQueue<Query> queries;

    private static final int MAX_LOADIND_IMAGES_NUMBER = 3;
}
