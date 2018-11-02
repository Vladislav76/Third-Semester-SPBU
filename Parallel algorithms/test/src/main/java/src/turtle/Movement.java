package src.turtle;

import src.general.ParallelProcessing;
import javafx.util.Pair;

public class Movement {

    public Movement() {
        work = new ParallelProcessing(0);
    }

    private void init(int[] alpha, int[] n, int threadsNumber) {
        work.setThreadsNumber(threadsNumber);
        this.alpha = alpha.clone();
        this.n = n.clone();
        if (alpha.length == n.length) {
            maxLength = alpha.length;
        }
        else {
            System.out.println("Error: Arrays must be equal length");
            maxLength = 0;
        }
        x = new double[maxLength];
        y = new double[maxLength];
    }

    public Pair<Double, Double> getLocation(int[] alpha, int[] n, int threadsNumber) {
        init(alpha, n, threadsNumber);
        if (threadsNumber > 1) {
            parallelCount();
            parallelOperating();
            return new Pair<>(x[maxLength - 1], y[maxLength - 1]);
        }
        else if (threadsNumber == 1) {
            return sequentialOperating();
        }
        else {
            System.out.println("Error: Number of threads can't be negative");
            return null;
        }
    }

    private void parallelCount() {
        PreoperativeFunction fun = new PreoperativeFunction(alpha, n, x, y);
        work.segmentProcess(maxLength, 0, 0, fun);
    }

    private void parallelOperating() {
        OperateFunction fun = new OperateFunction(alpha, x, y);
        work.segmentProcess(maxLength, 0, 1, fun);
    }

    private Pair<Double, Double> sequentialOperating() {
        double currentX = 0;
        double currentY = 0;
        int currentAlpha = 0;
        for (int i = 0; i < maxLength; i++) {
            currentAlpha += alpha[i];
            currentX += n[i] * Math.cos(currentAlpha * Math.PI / 180);
            currentY += n[i] * Math.sin(currentAlpha * Math.PI / 180);
        }
        return new Pair<>(currentX, currentY);
    }

     private void printCoordinates() {
        System.out.print("x: ");
        for (int i = 0; i < x.length; i++) {
            System.out.printf("%6.2f ", x[i]);
        }
        System.out.print("\ny: ");
        for (int i = 0; i < y.length; i++) {
            System.out.printf("%6.2f ", y[i]);
        }
        System.out.print("\n\n");
    }

    private int[] alpha;
    private int[] n;
    private double[] x;
    private double[] y;
    private int maxLength;
    private ParallelProcessing work;
}
