package parentheses;

import general.Processable;

public class CountFunction implements Processable {

    public CountFunction(String s, int[] left, int[] right) {
        this.s = s;
        this.left = left;
        this.right = right;
    }

    @Override
    public void process(int position, int offset) {
        if (s.charAt(position) == '(') {
            left[position] = 1;
        }
        else if (s.charAt(position) == ')') {
            right[position] = 1;
        }
    }

    private int[] left;
    private int[] right;
    private String s;
}
