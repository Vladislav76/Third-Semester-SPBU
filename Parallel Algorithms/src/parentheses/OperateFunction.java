package parentheses;

import general.Processable;

public class OperateFunction implements Processable {

    public OperateFunction(int[] left, int[] right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public void process(int position, int offset) {
        if (left[position - offset] >= right[position]) {
            left[position] = left[position - offset] + left[position] - right[position];
            right[position] = right[position - offset];
        }
        else {
            right[position] = right[position - offset] + right[position] - left[position - offset];
        }
    }

    private int[] left;
    private int[] right;
}
