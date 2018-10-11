public class Main {

    private static final int THREADS_NUMBER = 3;
    private static final int DATA_ARRAY_SIZE = 10;

    private int totalSum;
    volatile private int currentIndex;
    private int[] dataArray;
    private CustomLock lock = new CustomLock(THREADS_NUMBER);

    private Main() {
        dataArray = new int[DATA_ARRAY_SIZE];
        for (int i = 0; i < DATA_ARRAY_SIZE; i++) {
            dataArray[i] = i + 1;
        }
        totalSum = 0;
        currentIndex = 0;
    }

    private void run() {
        for (int i = 0; i < THREADS_NUMBER; i++) {
            final int k = i;
            new Thread(() -> {
                while (currentIndex < DATA_ARRAY_SIZE) {
                    try {
                        addElement(k);
                        Thread.sleep(1000);
                    }
                    catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }).start();
        }
    }

    private void addElement(int threadID) {
        lock.lock(threadID);
        try {
            if (currentIndex < DATA_ARRAY_SIZE) {
                totalSum += dataArray[currentIndex++];
                System.out.print(Thread.currentThread());
                System.out.println(" currentSum: " + totalSum);
            }
        }
        finally {
            lock.unlock(threadID);
        }
    }

    public static void main(String[] args) {
        Main main = new Main();
        main.run();
    }
}
