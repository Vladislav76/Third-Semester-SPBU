package src.linear_equations;

import src.general.ParallelProcessing;

public class Solution {

    public Solution() {
        work = new ParallelProcessing(0);
    }

    private void init(double[] a, double[] b, int threadsNumber) {
        work.setThreadsNumber(threadsNumber);
        this.a = a.clone();
        this.b = b.clone();
        if (a.length == b.length) {
            maxLength = a.length;
            this.a[0] = 1;
        }
        else {
            System.out.println("Error: Arrays must be equal length");
            maxLength = 0;
        }
    }

    public double getSolution(double[] arrayA, double[] arrayB, int threadsNumber) {
        init(arrayA, arrayB, threadsNumber);
        if (threadsNumber > 1) {
            parallelOperating();
            return b[maxLength - 1];
        }
        else if (threadsNumber == 1) {
            return sequentialOperating();
        }
        else {
            System.out.println("Error: Number of threads can't be negative");
            return 0;
        }
    }

    private void parallelOperating() {
        OperateFunction fun = new OperateFunction(a, b);
        work.segmentProcess(maxLength, 0, 1, fun);
    }

    private double sequentialOperating() {
        double result = 0;
        for (int i = 0; i < maxLength; i++) {
            result = a[i] * result + b[i];
        }
        return result;
    }

    private void printResults() {
        System.out.print("a: ");
        for (int i = 0; i < maxLength; i++) {
            System.out.print(a[i] + " ");
        }
        System.out.print("\nb: ");
        for (int i = 0; i < maxLength; i++) {
            System.out.print(b[i] + " ");
        }
        System.out.print("\n\n");
    }

    private double[] a;
    private double[] b;
    private int maxLength;
    private ParallelProcessing work;
}
