package src.adding_integers;

import src.general.Processable;
import static src.adding_integers.Addition.*;

public class CountFunction implements Processable {

    public CountFunction(byte[] a, byte[] b, byte[] c) {
        this.a = a;
        this.b = b;
        this.c = c;
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
        if (tempSum >= 10) {
            c[position] = CARRY;
        }
        else if (tempSum == 9) {
            c[position] = MAYBE;
        }
        else {
            c[position] = NEVER;
        }
    }

    private byte[] a;
    private byte[] b;
    private byte[] c;
}
