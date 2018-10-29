package adding_integers;

import general.Processable;
import static adding_integers.Addition.*;

public class SumFunction implements Processable {

    public SumFunction(byte[] a, byte[] b, byte[] c, byte[] result) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.result = result;
    }

    @Override
    public void process(int position, int offset) {
        byte tempSum = 0;
        if (position < a.length) {
            tempSum = a[position];
        }
        if (position < b.length) {
            tempSum += b[position];
        }
        if (position > 0 && c[position - 1] == CARRY) {
            tempSum++;
        }
        result[position] = (byte) (tempSum % 10);
    }

    private byte[] a;
    private byte[] b;
    private byte[] c;
    private byte[] result;
}
