package src.adding_integers;

public class BigInteger {

    private byte[] digits;

    public BigInteger(byte[] digits) {
        this.digits = digits;
    }

    public byte[] getDigits() {
        return digits;
    }
}
