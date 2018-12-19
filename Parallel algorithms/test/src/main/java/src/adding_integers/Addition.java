package src.adding_integers;

import src.general.ParallelProcessing;

import java.util.Arrays;

public class Addition {

    public Addition() {
        work = new ParallelProcessing(0);
    }

    private void init(BigInteger a, BigInteger b, int threadsNumber) {
        work.setThreadsNumber(threadsNumber);
        mDigitsA = a.getDigits();
        mDigitsB = b.getDigits();
        maxLength = Math.max(mDigitsA.length, mDigitsB.length);
        mDigitsC = new byte[maxLength];
    }

    public BigInteger add(BigInteger a, BigInteger b, int threadsNumber) {
        init(a, b, threadsNumber);
        if (threadsNumber > 1) {
            parallelCount();
            parallelPrefixScan();
            parallelSum();
        }
        else if (threadsNumber == 1) {
            sequentialSum();
        }
        else {
            return null;
        }
        return new BigInteger(sum);
    }

    private void parallelCount() {
        CountFunction fun = new CountFunction(mDigitsA, mDigitsB, mDigitsC);
        work.segmentProcess(maxLength, 0, 0, fun);
    }

    private void parallelPrefixScan() {
        OperateFunction fun = new OperateFunction(mDigitsC);

        //COLLECT PHASE
        int step = 2;
        int currentOperationsNumber = maxLength / step;
        while (currentOperationsNumber > 0) {
            work.binaryProcess(step, currentOperationsNumber, step - 1, fun);
            step *= 2;
            currentOperationsNumber = maxLength / step;
        }
        int currentPosition = step / 2 - 1;
        for (int i = step / 4; i >= 1; i /= 2) {
            int nextPosition = currentPosition + i;
            if (nextPosition < maxLength) {
                fun.process(nextPosition, i);
                currentPosition = nextPosition;
            }
        }

        //DISTRIBUTE PHASE
        step /= 2;
        while (step >= 2) {
            currentOperationsNumber = maxLength / step;
            if (maxLength % step < step / 2) {
                currentOperationsNumber--;
            }
            work.binaryProcess(step, currentOperationsNumber,step + (step / 2) - 1, fun);
            step /= 2;
        }
    }

    private void parallelSum() {
        if (mDigitsC[maxLength - 1] == CARRY) {
            maxLength++;
        }
        sum = new byte[maxLength];
        SumFunction fun = new SumFunction(mDigitsA, mDigitsB, mDigitsC, sum);
        work.segmentProcess(maxLength, 0, 0, fun);
    }

    private void sequentialSum() {
        sum = new byte[maxLength];
        int carry = 0;
        for (int i = 0; i < maxLength; i++) {
            int tempSum = carry;
            if (i < mDigitsA.length) {
                tempSum += mDigitsA[i];
            }
            if (i < mDigitsB.length) {
                tempSum += mDigitsB[i];
            }
            sum[i] = (byte) (tempSum % 10);
            carry = tempSum / 10;
        }
        if (carry == 1) {
            maxLength++;
            sum = Arrays.copyOf(sum, maxLength);
            sum[maxLength - 1] = 1;
        }
    }

    private void printResults() {
        System.out.print("C: ");
        for (int i = 0; i < mDigitsC.length; i++) {
            System.out.print(mDigitsC[i] + " ");
        }
        System.out.println();
    }

    private byte[] mDigitsA;
    private byte[] mDigitsB;
    private byte[] mDigitsC;
    private byte[] sum;
    private int maxLength;
    private ParallelProcessing work;

    public static final byte CARRY = 1;
    public static final byte MAYBE = 2;
    public static final byte NEVER = 3;
}
