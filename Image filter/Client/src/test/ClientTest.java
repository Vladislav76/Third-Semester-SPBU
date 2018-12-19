package test;

import java.util.Arrays;

public class ClientTest {

    private static void test(int clientsNumber, String fileName, int filterID, int queriesNumber) {
        Thread[] threads = new Thread[clientsNumber];
        TestServerHandler[] clients = new TestServerHandler[clientsNumber];
        for (int i = 0; i < clientsNumber; i++) {
            clients[i] = new TestServerHandler("", 1488, queriesNumber, fileName, filterID);
            threads[i] = new Thread(clients[i]);
        }

        long time = System.currentTimeMillis();

        for (int i = 0; i < clientsNumber; i++) {
            threads[i].start();
        }
        try {
            for (int i = 0; i < clientsNumber; i++) {
                threads[i].join();
            }
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
        long elapsed = System.currentTimeMillis() - time;

        long allTime = 0;
        long[] allTimes = new long[queriesNumber * clientsNumber];
        int index = 0;
        for (int i = 0; i < clientsNumber; i++) {
            long[] times = clients[i].getTimes();
            for (int k = 0; k < queriesNumber; k++) {
                allTime += times[k];
                allTimes[index] = times[k];
                index++;
            }
        }

        for (int i = 0; i < allTimes.length; i++) {
            System.out.print(allTimes[i] + " ");
        }
        System.out.println();

                Arrays.sort(allTimes);

        int k1 = allTimes.length / 2;
        int k2 = (allTimes.length - 1) / 2;
        long medianTime = (allTimes[k1] + allTimes[k2]) / 2;
        long averageTime = allTime / allTimes.length;

        System.out.println("Queries number=" + clientsNumber * queriesNumber +
                " | Execution time=" + elapsed + " ms" +
                " | All time=" + allTime + " ms" +
                " | Average time=" + averageTime + " ms" +
                " | Median time=" + medianTime + " ms");
    }

    public static void main(String[] args) {
        test(3, "pictures/example_3.jpg", 1, 10);
    }
}
