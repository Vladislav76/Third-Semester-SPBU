package main;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

public class Image {

    public Image(String fileName, int id) throws IOException {
        File file = new File(fileName);
        this.format = getFileExtension(file);
        this.image = ImageIO.read(file);
        this.id = id;
        this.fileName = file.getName();
        this.status = new AtomicInteger(0);
    }

    private static String getFileExtension(File file) {
        String fileName = file.getName();
        if (fileName.lastIndexOf(".") != -1 && fileName.lastIndexOf(".") != 0) {
            return fileName.substring(fileName.lastIndexOf(".") + 1);
        }
        else return DEFAULT_FORMAT;
    }

    public void saveImage(String fileName) throws IOException {
        ImageIO.write(image, format, new File(fileName));
    }

    /* Setters */
    public void setStatus(int status) {
        this.status.set(status);
    }
    public void setPercentWork(int percent) {
        this.percentWork = percent;
    }
    public void setData(byte[] bytes) {
        int size = getWidth() * getHeight();
        int bytesPerPixel = bytes.length / size;
        int[] rgbArray = new int[size];

        for (int i = 0; i < size; i++) {
            for (int k = 0; k < bytesPerPixel; k++) {
                rgbArray[i] |= ((bytes[i * bytesPerPixel + k] & 0xFF) << (k * 8));
            }
        }
        image.setRGB(0, 0, getWidth(), getHeight(), rgbArray, 0, getWidth());
    }

    /* Getters */
    public String getFileName() {
        return fileName;
    }
    public BufferedImage getImage() {
        return image;
    }
    public int getWidth() {
        return image.getWidth();
    }
    public int getHeight() {
        return image.getHeight();
    }
    public int getID() {
        return id;
    }
    public int getPercentWork() {
        return percentWork;
    }
    public int getSizeInBytes() {
        return ((DataBufferByte) image.getRaster().getDataBuffer()).getData().length;
    }
    public byte[] getSourceBytes() {
        return ((DataBufferByte) image.getRaster().getDataBuffer()).getData();
    }
    public int getStatus() {
        return status.get();
    }

    private BufferedImage image;
    private int id;
    private int percentWork;
    private String format;
    private String fileName;
    private AtomicInteger status;

    private static final String DEFAULT_FORMAT = "png";
    public static final int UPDATED = 0;
    public static final int CANCELED = 1;
    public static final int PROCESSED = 2;
    public static final int UNCHANGED = 3;
    public static final int READY = 4;
}
