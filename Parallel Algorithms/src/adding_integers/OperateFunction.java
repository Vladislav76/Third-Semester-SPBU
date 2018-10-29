package adding_integers;

import general.Processable;
import static adding_integers.Addition.*;

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
