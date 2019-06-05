package filters;

abstract public class Filter {

    public Filter(int width, int height, byte[] sourceBytes, byte[] resultBytes) {
        this.width = width;
        this.height = height;
        this.sourceBytes = sourceBytes;
        this.resultsBytes = resultBytes;
        this.bytesPerPixel = sourceBytes.length / (width * height);
    }

    protected boolean isWithinImageArea(int x, int y) {
        return (x >= 0 && y >= 0 && x < width && y < height);
    }

    abstract public void process(int x, int y);

    protected int width;
    protected int height;
    protected byte[] sourceBytes;
    protected byte[] resultsBytes;
    protected int bytesPerPixel;
}
