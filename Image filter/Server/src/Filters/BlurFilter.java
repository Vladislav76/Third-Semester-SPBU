package Filters;

public class BlurFilter extends Filter {

    public BlurFilter(int width, int height, byte[] sourceBytes, byte[] resultBytes) {
        super(width, height, sourceBytes, resultBytes);
    }

    @Override
    public void process(int x, int y) {
        int neighborsNumber = 0;
        int[] colors = new int[bytesPerPixel];
        for (int i = -PIXEL_RADIUS; i <= PIXEL_RADIUS; i++) {
            for (int j = -PIXEL_RADIUS; j <= PIXEL_RADIUS; j++) {
                int currentX = x + j;
                int currentY = y + i;
                if (isWithinImageArea(currentX, currentY)) {
                    neighborsNumber++;
                    for (int k = 0; k < bytesPerPixel; k++) {
                        colors[k] += (sourceBytes[bytesPerPixel * (currentY * width + currentX) + k]) & 0xFF;
                    }
                }
            }
        }
        for (int k = 0; k < bytesPerPixel; k++) {
            resultsBytes[bytesPerPixel * (y * width + x) + k] = (byte) (colors[k] / neighborsNumber);
        }
    }

    private static final int PIXEL_RADIUS = 3;
}
