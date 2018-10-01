package org.src;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class Main {
    public static void filterSetup(String dirName, String fileName, Filter filter) {
        try {
            filter.setImage(ImageIO.read(new File(dirName, fileName)));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        String dirName = "resources";
        String fileName = "Glasses.jpeg";
        Filter filter = new Filter();

        filterSetup(dirName, fileName, filter);
        filter.process(Filter.HORIZONTAL_PROCESSING_MODE, 4);

        BufferedImage resultImage = filter.getResultImage();
        try {
            ImageIO.write(resultImage, "jpg", new File(dirName, "~" + fileName));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
