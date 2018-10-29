package general;

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

    public void process(int step, int currentOperationsNumber, int currentPosition, Processable fun) {
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

    public void parallelOperating(Processable fun, int maxLength) {
        int step = 2;
        int currentOperationsNumber = maxLength / step;
        while (currentOperationsNumber > 0) {
            process(step, currentOperationsNumber, step - 1, fun);
            step *= 2;
            currentOperationsNumber = maxLength / step;
        }
        int offset = maxLength - 1 - (step / 2 - 1);
        if (offset > 0) {
            fun.process(maxLength - 1, offset);
        }
    }

    private Thread[] threads;
}
