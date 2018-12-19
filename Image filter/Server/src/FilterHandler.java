import Filters.BlurFilter;
import Filters.Filter;
import Filters.NegativeFilter;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class FilterHandler implements Runnable {

    public FilterHandler(int imageWidth, int imageHeight, byte[] sourceBytes, int filterID, int imageID) {
        this.sourceBytes = sourceBytes;
        this.imageWidth = imageWidth;
        this.imageHeight = imageHeight;
        this.resultBytes = new byte[sourceBytes.length];
        this.isDone = new AtomicBoolean(false);
        this.isBreak = new AtomicBoolean(false);
        this.currentLineNumber = new AtomicInteger(0);
        this.filter = getFilter(filterID);
        this.imageID = imageID;
    }

    private Filter getFilter(int id) {
        Filter filter = null;
        switch (id) {
            case BLUR_FILTER_ID:
                filter = new BlurFilter(imageWidth, imageHeight, sourceBytes, resultBytes);
                break;
            case NEGATIVE_FILTER_ID:
                filter = new NegativeFilter(imageWidth, imageHeight, sourceBytes, resultBytes);
                break;
        }
        return filter;
    }

    @Override
    public void run() {
        int line = currentLineNumber.get();
        while (line < imageHeight && !isBreak.get()) {
            for (int j = 0; j < imageWidth; j++) {
                filter.process(j, line);
            }
            line = currentLineNumber.incrementAndGet();
        }
        if (!isBreak.get()) {
            isDone.set(true);
        }
    }

    /* Setters */
    public void stop() {
        isBreak.set(true);
    }

    /* Getters */
    public byte[] getResultBytes() {
        if (isDone.get()) {
            return resultBytes;
        }
        return null;
    }
    public int getImageID() {
        return imageID;
    }
    public boolean isDone() {
        return isDone.get();
    }
    public boolean isBreak() {
        return isBreak.get();
    }
    public int getPercentWork() {
        return (int) (100.0 * currentLineNumber.get() / imageHeight);
    }

    private byte[] sourceBytes;
    private byte[] resultBytes;
    private Filter filter;
    private AtomicBoolean isDone;
    private AtomicBoolean isBreak;
    private AtomicInteger currentLineNumber;
    private int imageWidth;
    private int imageHeight;
    private int imageID;

    private static final int BLUR_FILTER_ID = 0;
    private static final int NEGATIVE_FILTER_ID = 1;
}
