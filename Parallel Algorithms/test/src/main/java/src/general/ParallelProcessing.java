package src.general;

public class ParallelProcessing {

    public ParallelProcessing(int threadsNumber) {
        if (threadsNumber >= 0) {
            threads = new Thread[threadsNumber];
        }
    }

    public void setThreadsNumber(int threadsNumber) {
        if (threadsNumber != threads.length && threadsNumber >= 1) {
            threads = new Thread[threadsNumber];
        }
    }

    public void binaryProcess(int step, int currentOperationsNumber, int currentPosition, Processable fun) {
        int operationsNumberOfThread = currentOperationsNumber / threads.length;
        int remainingOperationsNumber = currentOperationsNumber % threads.length;
        int currentWorkingThreadsNumber = threads.length;

        if (operationsNumberOfThread == 0) {
            currentWorkingThreadsNumber = remainingOperationsNumber;
        }

        for (int i = 0; i < currentWorkingThreadsNumber; i++) {
            int startPosition = currentPosition;
            int operationsNumber = operationsNumberOfThread - 1;
            if (remainingOperationsNumber > 0) {
                operationsNumber++;
                remainingOperationsNumber--;
            }
            int finishPosition = startPosition + operationsNumber * step;

            int shift = step;
            threads[i] = new Thread(() -> {
                for (int k = startPosition; k <= finishPosition; k += shift) {
                    fun.process(k, step / 2);
                }
            });
            currentPosition = finishPosition + step;
        }

        for (int i = 0; i < currentWorkingThreadsNumber; i++) {
            threads[i].start();
        }

        for (int i = 0; i < currentWorkingThreadsNumber; i++) {
            try {
                threads[i].join();
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    public void segmentProcess(int currentOperationsNumber, int currentPosition, int offset, Processable fun) {
        int operationsNumberOfThread = currentOperationsNumber / threads.length;
        int remainingOperationsNumber = currentOperationsNumber % threads.length;
        int currentWorkingThreadsNumber = threads.length;

        if (operationsNumberOfThread == 0) {
            currentWorkingThreadsNumber = remainingOperationsNumber;
        }

        int[] finishPositions = new int[currentWorkingThreadsNumber];

        for (int i = 0; i < currentWorkingThreadsNumber; i++) {
            int startPosition = currentPosition;
            int operationsNumber = operationsNumberOfThread - 1;
            if (remainingOperationsNumber > 0) {
                operationsNumber++;
                remainingOperationsNumber--;
            }
            int finishPosition = startPosition + operationsNumber;
            finishPositions[i] = finishPosition;
            int shift = (offset == 0) ? 0 : 1;
            threads[i] = new Thread(() -> {
                for (int k = startPosition + shift; k <= finishPosition; k++) {
                    fun.process(k, offset);
                }
            });
            currentPosition = finishPosition + 1;
        }

        for (int i = 0; i < currentWorkingThreadsNumber; i++) {
            threads[i].start();
        }

        for (int i = 0; i < currentWorkingThreadsNumber; i++) {
            try {
                threads[i].join();
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        if (offset != 0) {
            for (int i = 1; i < finishPositions.length; i++) {
                fun.process(finishPositions[i], finishPositions[i] - finishPositions[i - 1]);
            }
        }
    }

    private Thread[] threads;
}
