package parentheses;

public class Testing {

    private Analysis analysis = new Analysis();

    private boolean test_1_1() {
        String s = "(()))(()";
        return testAndCheck(s, false);
    }

    private boolean test_1_2() {
        String s = ")(())()(";
        return testAndCheck(s, false);
    }

    private boolean test_1_3() {
        String s = "(((())))";
        return testAndCheck(s, true);
    }

    private boolean test_1_4() {
        String s = "())(()";
        return testAndCheck(s, false);
    }

    private boolean test_1_5() {
        String s = "((()()()))";
        return testAndCheck(s, true);
    }

    public void start() {
        System.out.println("Test 1: " + test_1_1());
        System.out.println("Test 2: " + test_1_2());
        System.out.println("Test 3: " + test_1_3());
        System.out.println("Test 4: " + test_1_4());
        System.out.println("Test 5: " + test_1_5());
    }

    private boolean testAndCheck(String s, boolean correctResult) {
        boolean result1 = analysis.isCorrect(s, 1);
        boolean result2 = analysis.isCorrect(s, 2);
        boolean result3 = analysis.isCorrect(s, 3);
        boolean result4 = analysis.isCorrect(s, 4);
        return result1 == correctResult && result1 == result2 && result2 == result3 && result3 == result4;
    }
}
