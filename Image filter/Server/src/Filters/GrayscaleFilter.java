package Filters;

public class GrayscaleFilter extends Filter {

    public GrayscaleFilter(int width, int height, byte[] sourceBytes, byte[] resultBytes) {
        super(width, height, sourceBytes, resultBytes);
    }

    @Override
    public void process(int x, int y) {
        int colorsSum = 0;
        for (int k = 0; k < bytesPerPixel; k++) {
            int index = bytesPerPixel * (y * width + x) + k;
            colorsSum += (sourceBytes[index] & 0xFF);
        }
        for (int k = 0; k < bytesPerPixel; k++) {
            int index = bytesPerPixel * (y * width + x) + k;
            resultsBytes[index] = (byte) (colorsSum / 3);
        }
    }
}
