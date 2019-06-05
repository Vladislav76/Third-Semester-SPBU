package src;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;

public class Main {

    public static void main(String[] args) {
        String inputDirectoryName = "test/src/main/input";
        String outputDirectoryName = "test/src/main/output";
        String fileName = "3840x2160.jpg";

        Filter filter = new Filter(4);
        filter.setImage(fileName, inputDirectoryName);
        filter.process(Filter.HORIZONTAL_PROCESSING_MODE);

        try {
            BufferedImage resultImage = filter.getResultImage();
            ImageIO.write(resultImage, "jpg", new File(outputDirectoryName, "~" + fileName));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}
