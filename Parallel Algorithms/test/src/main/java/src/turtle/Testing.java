package src.turtle;

import javafx.util.Pair;

import static src.general.FileReader.readFileAsPairOfIntArrays;

public class Testing {

    private Movement movement = new Movement();

    private boolean test_1_1() {
        String fileName = "test/src/main/java/input/turtle_test_1";
        double correctX = 1.22;
        double correctY = 56.58;
        return testAndCheck(fileName, correctX, correctY);
    }

    private boolean test_1_2() {
        String fileName = "test/src/main/java/input/turtle_test_2";
        double correctX = 100.22;
        double correctY = 36.58;
        return testAndCheck(fileName, correctX, correctY);
    }

    public void start() {
        System.out.println("Test 1: " + test_1_1());
        System.out.println("Test 2: " + test_1_2());
    }

    private boolean testAndCheck(String fileName, double correctResultX, double correctResultY) {
        try {
            Pair<int[], int[]> pair = readFileAsPairOfIntArrays(fileName);
            Pair<Double, Double> result1 = movement.getLocation(pair.getKey(), pair.getValue(), 1);
            Pair<Double, Double> result2 = movement.getLocation(pair.getKey(), pair.getValue(), 2);
            double differenceX1 = Math.abs(result1.getKey() - correctResultX);
            double differenceY1 = Math.abs(result1.getValue() - correctResultY);
            double differenceX2 = Math.abs(result2.getKey() - correctResultX);
            double differenceY2 = Math.abs(result2.getValue() - correctResultY);
            return differenceX1 < 1 && differenceY1 < 1 && differenceX2 < 1 && differenceY2 < 1;
        }
        catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }
}
