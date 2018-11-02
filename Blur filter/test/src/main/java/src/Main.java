package src;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;

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
        String inputDirectoryName = "test/src/main/input";
        String outputDirectoryName = "test/src/main/output";
        String fileName = "Ak.jpg";
        Filter filter = new Filter();

        filterSetup(inputDirectoryName, fileName, filter);
        filter.process(Filter.HORIZONTAL_PROCESSING_MODE, 6);

        try {
            BufferedImage resultImage = filter.getResultImage();
            ImageIO.write(resultImage, "jpg", new File(outputDirectoryName, "~" + fileName));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
