package src.adding_integers;

import src.general.Processable;
import static src.adding_integers.Addition.*;

public class OperateFunction implements Processable {

    public OperateFunction(byte[] c) {
        this.c = c;
    }

    @Override
    public void process(int position, int offset) {
        if (c[position] == MAYBE) {
            c[position] = c[position - offset];
        }
    }

    private byte[] c;
}
