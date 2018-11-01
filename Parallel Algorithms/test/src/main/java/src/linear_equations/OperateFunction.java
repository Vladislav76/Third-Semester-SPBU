package src.linear_equations;

import src.general.Processable;

public class OperateFunction implements Processable {

    public OperateFunction(double[] a, double[] b) {
        this.a = a;
        this.b = b;
    }

    @Override
    public void process(int position, int offset) {
        b[position] = b[position] + b[position - offset] * a[position];
        a[position] = a[position] * a[position - offset];
    }

    private double[] a;
    private double[] b;
}
