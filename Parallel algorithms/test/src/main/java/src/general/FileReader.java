package src.general;

import javafx.util.Pair;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Scanner;

public class FileReader {
    public static Pair<int[], int[]> readFileAsPairOfIntArrays(String fileName) {
        ArrayList<Integer> listA = new ArrayList<>();
        ArrayList<Integer> listB = new ArrayList<>();
        try {
            File file = new File(fileName);
            Scanner sc = new Scanner(file);
            int k = 0;
            while (sc.hasNextInt()) {
                if (k == 0) {
                    listA.add(sc.nextInt());
                }
                else {
                    listB.add(sc.nextInt());
                }
                k = 1 - k;
            }
            int[] a = new int[listA.size()];
            int[] b = new int[listB.size()];
            for (int i = 0; i < a.length; i++) {
                a[i] = listA.get(i);
                b[i] = listB.get(i);
            }
            sc.close();
            return new Pair<>(a, b);
        }
        catch (IOException e) {
            e.printStackTrace();
            return new Pair<>(null, null);
        }
    }

    public static Pair<double[], double[]> readFileAsPairOfDoubleArrays(String fileName) throws Exception {
        ArrayList<Double> listA = new ArrayList<>();
        ArrayList<Double> listB = new ArrayList<>();
        File file = new File(fileName);
        Scanner sc = new Scanner(file);
        int k = 0;
        while (sc.hasNextDouble()) {
            if (k == 0) {
                listA.add(sc.nextDouble());
            }
            else {
                listB.add(sc.nextDouble());
            }
            k = 1 - k;
        }
        double[] a = new double[listA.size()];
        double[] b = new double[listB.size()];
        for (int i = 0; i < a.length; i++) {
            a[i] = listA.get(i);
            b[i] = listB.get(i);
        }
        sc.close();
        return new Pair<>(a,b);
    }

    public static Pair<byte[], byte[]> readFileAsPairOfByteArrays(String fileName) {
        try {
            File file = new File(fileName);
            Scanner sc = new Scanner(file);
            byte[] a = null;
            if (sc.hasNextLine()) {
                a = toBytesArray(sc.nextLine());
            }
            byte[] b = null;
            if (sc.hasNextLine()) {
                b = toBytesArray(sc.nextLine());
            }
            sc.close();
            return new Pair<>(a, b);
        }
        catch (IOException e) {
            e.printStackTrace();
            return new Pair<>(null, null);
        }
    }

    public static byte[] toBytesArray(String s) {
        byte[] result = new byte[s.length()];
        for (int i = 0; i < result.length; i++) {
            result[i] = (byte) (s.codePointAt(i) - '0');
        }
        return result;
    }

    public static String readFileAsString(String fileName) throws Exception {
        String data = "";
        data = new String(Files.readAllBytes(Paths.get(fileName)));
        return data;
    }
}
