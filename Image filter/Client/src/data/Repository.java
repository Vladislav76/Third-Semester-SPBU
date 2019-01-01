package data;

import main.Image;
import main.Query;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;

public class Repository {

    public synchronized CopyOnWriteArrayList<Image> getImagesArrayList() {
        if (images == null) {
            images = new CopyOnWriteArrayList<>();
        }
        return images;
    }

    public synchronized ConcurrentLinkedQueue<Query> getQueriesQueue() {
        if (queries == null) {
            queries = new ConcurrentLinkedQueue<>();
        }
        return queries;
    }

    private ConcurrentLinkedQueue<Query> queries;
    private CopyOnWriteArrayList<Image> images;
}
