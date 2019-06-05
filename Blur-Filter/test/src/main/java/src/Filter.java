package src;

import javax.imageio.ImageIO;
import java.awt.image.*;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

public class Filter {

    public Filter(int threadsNumber) {
        if (threadsNumber > 0) {
            this.threadsNumber = threadsNumber;
            threads = new Thread[threadsNumber - 1];
        }
        else {
            System.out.println("ERROR. Threads number must be positive integer!");
            threads = new Thread[0];
        }
    }

    public void setImage(BufferedImage image) {
        this.sourceImage = image;
        imageSizeX = image.getWidth();
        imageSizeY = image.getHeight();
        resultImage = new BufferedImage(imageSizeX, imageSizeY, sourceImage.getType());
    }

    public void setImage(String fileName, String dirName) {
        try {
            setImage(ImageIO.read(new File(dirName, fileName)));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public BufferedImage getResultImage() {
        return resultImage;
    }

    public void process(int mode) {
        if (sourceImage != null && threadsNumber > 0) {
            currentIndex.set(0);
            switch (mode) {
                case HORIZONTAL_PROCESSING_MODE:
                    maxIndex = imageSizeY;
                    break;
                case VERTICAL_PROCESSING_MODE:
                    maxIndex = imageSizeX;
                    break;
                default:
                    maxIndex = 0;
            }
            Runnable r = () -> {
                while (currentIndex.get() < maxIndex) {
                    processLine(mode, currentIndex.getAndAdd(1));
                }
            };

            for (int k = 0; k < threads.length; k++) {
                threads[k] = new Thread(r);
                threads[k].start();
            }

            r.run();

            try {
                for (int k = 0; k < threads.length; k++) {
                    threads[k].join();
                }
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    private void processPixel(int x, int y) {
        int[] pixelColor = new int[RGB];
        int pixelNumber = 0;
        for (int i = -PIXEL_RADIUS; i <= PIXEL_RADIUS; i++) {
            for (int j = -PIXEL_RADIUS; j <= PIXEL_RADIUS; j++) {
                if (isWithinImageArea(x + j, y + i)) {
                    pixelNumber++;
                    int tempColor = sourceImage.getRGB(x + j, y + i);
                    for (int k = 0; k < RGB; k++) {
                        pixelColor[k] += ((tempColor >> (8 * k)) & 0xff);
                    }
                }
            }
        }
        for (int k = 0; k < RGB; k++) {
            pixelColor[k] /= pixelNumber;
        }
        int color = 0;
        for (int k = RGB - 1; k > 0; k--) {
            color |= pixelColor[k];
            color <<= 8;
        }
        color |= pixelColor[0];
        resultImage.setRGB(x, y, color);
    }

    private void processLine(int mode, int index) {
        if (mode == HORIZONTAL_PROCESSING_MODE) {
            for (int j = 0; j < imageSizeX; j++) {
                processPixel(j, index);
            }
        }
        else if (mode == VERTICAL_PROCESSING_MODE) {
            for (int i = 0; i < imageSizeY; i++) {
                processPixel(index, i);
            }
        }
    }

    private boolean isWithinImageArea(int x, int y) {
        return (x >= 0 && y >= 0 && x < imageSizeX && y < imageSizeY);
    }

    public static final int HORIZONTAL_PROCESSING_MODE = 1;
    public static final int VERTICAL_PROCESSING_MODE = 2;

    private static final int PIXEL_RADIUS = 3;
    private static final int RGB = 3;

    private BufferedImage sourceImage;
    private BufferedImage resultImage;
    private int imageSizeX;
    private int imageSizeY;
    private int maxIndex;
    private int threadsNumber;
    private AtomicInteger currentIndex = new AtomicInteger();
    private Thread[] threads;
}