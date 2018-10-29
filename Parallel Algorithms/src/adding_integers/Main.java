package adding_integers;

public class Main {

    public static void printScanResult(byte[] result) {
        for (int i = 0; i < result.length; i++) {
            char symbol;
            switch (result[i]) {
                case Addition.CARRY:
                    symbol = 'C';
                    break;
                case Addition.MAYBE:
                    symbol = 'M';
                    break;
                case Addition.NEVER:
                    symbol = 'N';
                    break;
                default:
                    symbol = '?';
            }
            System.out.printf("%c ", symbol);
        }
        System.out.println();
    }

    public static void printResult(byte[] result) {
        for (int i = 0; i < result.length; i++) {
            System.out.printf("%d ", result[i]);
        }
        System.out.println();
    }

    public static void main(String[] args) {
        new Testing().start();
    }
}
