import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;

public class Image {

    public Image(String fileName, int id) throws IOException {
        File file = new File(fileName);
        this.format = getFileExtension(file);
        this.image = ImageIO.read(file);
        this.isReady = false;
        this.id = id;
        this.fileName = file.getName();
    }

    private static String getFileExtension(File file) {
        String fileName = file.getName();
        if (fileName.lastIndexOf(".") != -1 && fileName.lastIndexOf(".") != 0) {
            return fileName.substring(fileName.lastIndexOf(".") + 1);
        }
        else return DEFAULT_FORMAT;
    }

    /* Setters */
    public void setPercentWork(int percent) {
        this.percentWork = percent;
        if (percent == 100) {
            isReady = true;
        }
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
    public int getWidth() {
        return image.getWidth();
    }
    public int getHeight() {
        return image.getHeight();
    }
    public int getID() {
        return id;
    }
    public int getSizeInBytes() {
        return ((DataBufferByte) image.getRaster().getDataBuffer()).getData().length;
    }
    public byte[] getSourceBytes() {
        return ((DataBufferByte) image.getRaster().getDataBuffer()).getData();
    }
    public boolean isReady() {
        return isReady;
    }

    public void saveImage(String dirName) throws IOException {
        ImageIO.write(image, format, new File(dirName, fileName));
    }

    private BufferedImage image;
    private int id;
    private boolean isReady;
    private int percentWork;
    private String format;
    private String fileName;

    private static final String DEFAULT_FORMAT = "png";
}
