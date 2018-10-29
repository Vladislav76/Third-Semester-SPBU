package parentheses;

import general.ParallelProcessing;

public class Analysis {

    public Analysis() {
        work = new ParallelProcessing(0);
    }

    private void init(String s, int threadsNumber) {
        work.setThreadsNumber(threadsNumber);
        maxLength = s.length();
        this.s = s;
        left = new int[maxLength];
        right = new int[maxLength];
    }

    public boolean isCorrect(String s, int threadsNumber) {
        init(s, threadsNumber);
        if (threadsNumber > 1) {
            parallelCount();
            parallelOperating();
            return left[maxLength - 1] == 0 && right[maxLength - 1] == 0;
        }
        else if (threadsNumber == 1) {
            return sequentialOperating();
        }
        else {
            System.out.println("Error: Number of threads can't be negative");
            return false;
        }
    }

    private void parallelCount() {
        CountFunction fun = new CountFunction(s, left, right);
        work.process(1, maxLength, 0, fun);
    }

    private void parallelOperating() {
        OperateFunction fun = new OperateFunction(left, right);
        work.parallelOperating(fun, maxLength);
    }

    private boolean sequentialOperating() {
        int i = 0;
        int rank = 0;
        while (i < maxLength) {
            if (s.charAt(i) == '(') {
                rank++;
            }
            else if (s.charAt(i) == ')') {
                rank--;
                if (rank < 0) {
                    return false;
                }
            }
            i++;
        }
        return rank == 0;
    }

    private void printResults() {
        System.out.print("L: ");
        for (int i = 0; i < maxLength; i++) {
            System.out.print(left[i] + " ");
        }
        System.out.print("\nR: ");
        for (int i = 0; i < maxLength; i++) {
            System.out.print(right[i] + " ");
        }
        System.out.print("\n\n");
    }

    private int[] left;
    private int[] right;
    private String s;
    private int maxLength;
    private ParallelProcessing work;
}
