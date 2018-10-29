package linear_equations;

public class Testing {

    private Solution solution = new Solution();

    private boolean test_1_1() {
        double[] a = new double[] {1, 2, 3, 4, 5, 6, 7, 8};
        double[] b = new double[] {1, 2, 3 ,4, 5, 6, 7, 8};
        double answer = 109600;
        return testAndCheck(a, b, answer,4) &&
                testAndCheck(a, b, answer,1);
    }

    private boolean test_1_2() {
        double[] a = new double[] {1, 2, 3, 4};
        double[] b = new double[] {1, 2, 3 ,4};
        double answer = 64;
        return testAndCheck(a, b, answer,4) &&
                testAndCheck(a, b, answer,1);
    }

    public void start() {
        System.out.println("Test 1: " + test_1_1());
        System.out.println("Test 2: " + test_1_2());
    }

    private boolean testAndCheck(double[] a, double[] b, double correctResult, int threadsNumber) {
        double result = solution.getSolution(a, b, threadsNumber);
        return Math.abs(result - correctResult) < 1;
    }
}
