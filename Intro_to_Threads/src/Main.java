public class Main {

    private static int totalSum;
    private static volatile int currentIndex;
    private static Lock lock;
    private static int maxNumber;

    public static void run(int threadsNumber, int maxN, Lock lock) {
        totalSum = 0;
        currentIndex = 1;
        maxNumber = maxN;
        Main.lock = lock;

        Runnable r = new Runnable() {
            @Override
            public void run() {
                while (currentIndex <= maxNumber) {
                    addElement();
                }
            }
        };

        Thread[] threads = new Thread[threadsNumber];
        for (int k = 0; k < threadsNumber; k++) {
            threads[k] = new Thread(r);
            threads[k].start();
        }

        for (int k = 0; k < threadsNumber; k++) {
            try {
                threads[k].join();
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    private static void addElement() {
        lock.lock();
        try {
            if (currentIndex <= maxNumber) {
                totalSum += currentIndex++;
                System.out.print(Thread.currentThread());
                System.out.println(" currentSum: " + totalSum);
            }
        }
        finally {
            lock.unlock();
        }
    }

    public static void main(String[] args) {
        Main.run(3, 1000, new TASLock());
    }
}
