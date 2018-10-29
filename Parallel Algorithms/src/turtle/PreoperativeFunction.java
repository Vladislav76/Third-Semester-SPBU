package turtle;

import general.Processable;

public class PreoperativeFunction implements Processable {

    public PreoperativeFunction(int[] alpha, int[] n, double[] x, double[] y) {
        this.alpha = alpha;
        this.n = n;
        this.x = x;
        this.y = y;
    }

    @Override
    public void process(int position, int offset) {
        x[position] = n[position] * Math.cos(alpha[position] * Math.PI / 180);
        y[position] = (double) (n[position]) * Math.sin(alpha[position] * Math.PI / 180);
    }

    private double[] x;
    private double[] y;
    private int[] alpha;
    private int[] n;
}
