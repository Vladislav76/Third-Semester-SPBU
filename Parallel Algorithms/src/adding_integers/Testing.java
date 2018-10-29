package adding_integers;

import java.util.Arrays;

public class Testing {

    private Addition addition = new Addition();

    private boolean test_1_1() {
        String num1 = "97531";
        String num2 = "32186";
        String answ = "20718";
        return testAndCheck(num1, num2, answ);
    }

    private boolean test_1_2() {
        String num1 = "9819119988191";
        String num2 = "1111111111111";
        String answ = "0030320100303";
        return testAndCheck(num1, num2, answ);
    }

    private boolean test_1_3() {
        String num1 = "999999999999999";
        String num2 = "1";
        String answ = "0000000000000001";
        return testAndCheck(num1, num2, answ);
    }

    private boolean test_1_4() {
        String num1 = "999991";
        String num2 = "111108";
        String answ = "0111001";
        return testAndCheck(num1, num2, answ);
    }

    public void start() {
        System.out.println("Test 1: " + test_1_1());
        System.out.println("Test 2: " + test_1_2());
        System.out.println("Test 3: " + test_1_3());
        System.out.println("Test 4: " + test_1_4());
    }

    private boolean testAndCheck(String num1, String num2, String num3) {
        BigInteger a = new BigInteger(toBytesArray(num1));
        BigInteger b = new BigInteger(toBytesArray(num2));
        byte[] result = addition.add(a, b, 1).getDigits();
        byte[] result2 = addition.add(a, b, 4).getDigits();
        byte[] correctResult = toBytesArray(num3);
        return Arrays.equals(result, correctResult) && Arrays.equals(result2, correctResult);
    }

    private byte[] toBytesArray(String s) {
        byte[] result = new byte[s.length()];
        for (int i = 0; i < result.length; i++) {
            result[i] = (byte) (s.codePointAt(i) - '0');
        }
        return result;
    }
}
