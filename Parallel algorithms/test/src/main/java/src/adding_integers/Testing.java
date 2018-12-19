package src.adding_integers;

import javafx.util.Pair;

import java.util.Arrays;

import static src.general.FileReader.readFileAsPairOfByteArrays;
import static src.general.FileReader.toBytesArray;

public class Testing {

    private Addition addition = new Addition();

    private boolean test_1_1() {
        String fileName = "test/src/main/input/integers_test_1";
        String correctResult = "20718";
        return testAndCheck(fileName, correctResult);
    }

    private boolean test_1_2() {
        String fileName = "test/src/main/input/integers_test_2";
        String correctResult = "0030320100303";
        return testAndCheck(fileName, correctResult);
    }

    private boolean test_1_3() {
        String fileName = "test/src/main/input/integers_test_3";
        String correctResult = "00000009999029";
        return testAndCheck(fileName, correctResult);
    }

    private boolean test_1_4() {
        String fileName = "test/src/main/input/integers_test_4";
        String correctResult = "0111001";
        return testAndCheck(fileName, correctResult);
    }

    public void start() {
        System.out.println("Test 1: " + test_1_1());
        System.out.println("Test 2: " + test_1_2());
        System.out.println("Test 3: " + test_1_3());
        System.out.println("Test 4: " + test_1_4());
    }

    private boolean testAndCheck(String fileName, String stringResult) {
        Pair<byte[], byte[]> pair = readFileAsPairOfByteArrays(fileName);
        BigInteger a = new BigInteger(pair.getKey());
        BigInteger b = new BigInteger(pair.getValue());
        byte[] result1 = addition.add(a, b, 1).getDigits();
        byte[] result2 = addition.add(a, b, 4).getDigits();
        byte[] correctResult = toBytesArray(stringResult);
        return Arrays.equals(result1, correctResult) && Arrays.equals(result2, correctResult);
    }
}
