package src.linear_equations;

import javafx.util.Pair;

import static src.general.FileReader.readFileAsPairOfDoubleArrays;

public class Testing {

    private Solution solution = new Solution();

    private boolean test_1_1() {
        String fileName = "test/src/main/input/equations_test_1";
        double correctResult = 64;
        return testAndCheck(fileName, correctResult);
    }

    private boolean test_1_2() {
        String fileName = "test/src/main/input/equations_test_2";
        double correctResult = 109600;
        return testAndCheck(fileName, correctResult);
    }

    public void start() {
        System.out.println("Test 1: " + test_1_1());
        System.out.println("Test 2: " + test_1_2());
    }

    private boolean testAndCheck(String fileName, double correctResult) {
        try {
            Pair<double[], double[]> pair = readFileAsPairOfDoubleArrays(fileName);
            double result1 = solution.getSolution(pair.getKey(), pair.getValue(), 1);
            double result2 = solution.getSolution(pair.getKey(), pair.getValue(), 4);
            return Math.abs(result1 - correctResult) < 1 && Math.abs(result2 - correctResult) < 1;
        }
        catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }
}
