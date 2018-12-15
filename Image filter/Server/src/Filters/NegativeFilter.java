package Filters;

public class NegativeFilter extends Filter {

    public NegativeFilter(int width, int height, byte[] sourceBytes, byte[] resultBytes) {
        super(width, height, sourceBytes, resultBytes);
    }

    @Override
    public void process(int x, int y) {
        for (int k = 0; k < bytesPerPixel; k++) {
            int index = bytesPerPixel * (y * width + x) + k;
            resultsBytes[index] = (byte) (255 - sourceBytes[index]);
        }
    }
}
