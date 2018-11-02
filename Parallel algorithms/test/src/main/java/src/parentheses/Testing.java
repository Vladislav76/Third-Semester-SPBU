package src.parentheses;

import static src.general.FileReader.readFileAsString;

public class Testing {

    private Analysis analysis = new Analysis();

    private boolean test_1_1() {
        String fileName = "test/src/main/input/parentheses_example_1";
        return testAndCheck(fileName, false);
    }

    private boolean test_1_2() {
        String fileName = "test/src/main/input/parentheses_example_2";
        return testAndCheck(fileName, true);
    }

    private boolean test_1_3() {
        String fileName = "test/src/main/input/parentheses_example_3";
        return testAndCheck(fileName, false);
    }

    private boolean test_1_4() {
        String fileName = "test/src/main/input/parentheses_example_4";
        return testAndCheck(fileName, true);
    }

    public void start() {
        System.out.println("Test 1: " + test_1_1());
        System.out.println("Test 2: " + test_1_2());
        System.out.println("Test 3: " + test_1_3());
        System.out.println("Test 4: " + test_1_4());
    }

    private boolean testAndCheck(String fileName, boolean correctResult) {
        try {
            String s = readFileAsString(fileName);
            boolean result1 = analysis.isCorrect(s, 1);
            boolean result2 = analysis.isCorrect(s, 2);
            boolean result3 = analysis.isCorrect(s, 3);
            boolean result4 = analysis.isCorrect(s, 4);
            return result1 == correctResult && result1 == result2 && result2 == result3 && result3 == result4;
        }
        catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }
}
