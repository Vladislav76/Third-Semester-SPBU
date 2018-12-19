package src.turtle;

import src.general.Processable;

public class OperateFunction implements Processable {

    public OperateFunction(int[] alpha, double[] x, double[] y) {
        this.alpha = alpha;
        this.x = x;
        this.y = y;
    }

    @Override
    public void process(int position, int offset) {
        double cos = Math.cos(alpha[position - offset] * Math.PI / 180);
        double sin = Math.sin(alpha[position - offset] * Math.PI / 180);
        double tempX = x[position - offset] + x[position] * cos - y[position] * sin;
        double tempY = y[position - offset] + x[position] * sin + y[position] * cos;
        alpha[position] += alpha[position - offset];
        x[position] = tempX;
        y[position] = tempY;
    }

    private double[] x;
    private double[] y;
    private int[] alpha;
}
