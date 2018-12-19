package data;

import main.Image;
import main.Query;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;

public class Repository {

    public static synchronized CopyOnWriteArrayList<Image> getImagesArrayList() {
        if (images == null) {
            images = new CopyOnWriteArrayList<>();
        }
        return images;
    }

    public static synchronized ConcurrentLinkedQueue<Query> getQueriesQueue() {
        if (queries == null) {
            queries = new ConcurrentLinkedQueue<>();
        }
        return queries;
    }

    private static ConcurrentLinkedQueue<Query> queries;
    private static CopyOnWriteArrayList<Image> images;
}
