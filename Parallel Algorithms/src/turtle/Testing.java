package turtle;

import javafx.util.Pair;

public class Testing {

    private Movement movement = new Movement();

    private boolean test_1_1() {
        int[] alpha = new int[] {45, 30, 105, 90};
        int[] n = new int[] {40, 50, 40, 20};
        double correctX = 1.22;
        double correctY = 56.58;
        return testAndCheck(alpha, n, correctX, correctY,4) &&
                testAndCheck(alpha, n, correctX, correctY,1);
    }

    private boolean test_1_2() {
        int[] alpha = new int[] {45, 30, 105, 90, 0, 90};
        int[] n = new int[] {40, 50, 40, 20, 20, 99};
        double correctX = 100.22;
        double correctY = 36.58;
        return testAndCheck(alpha, n, correctX, correctY,4) &&
                testAndCheck(alpha, n, correctX, correctY,1);
    }

    public void start() {
        System.out.println("Test 1: " + test_1_1());
        System.out.println("Test 2: " + test_1_2());
    }

    private boolean testAndCheck(int[] alpha, int[] n, double correctResultX, double correctResultY, int threadsNumber) {
        Pair<Double, Double> result = movement.getLocation(alpha, n, threadsNumber);
        double resultX = result.getKey();
        double resultY = result.getValue();
        double differenceX = Math.abs(correctResultX - resultX);
        double differenceY = Math.abs(correctResultY - resultY);
        return differenceX < 1 && differenceY < 1;
    }
}
