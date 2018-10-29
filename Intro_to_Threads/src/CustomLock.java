import java.util.Arrays;

public class CustomLock {

    private boolean[] flags;
    private int victim;

    public CustomLock(int threadsNumber) {
        flags = new boolean[threadsNumber];
        Arrays.fill(flags, false);
    }

    private boolean areOthersWaiting(int threadID) {
        for (int i = 0; i < flags.length; i++) {
            if (flags[i] && i != threadID) {
                return true;
            }
        }
        return false;
    }

    public void lock(int threadID) {
        flags[threadID] = true;
        victim = threadID;
        while (areOthersWaiting(threadID) && victim == threadID) {}
    }

    public void info() {
        for (int i = 0; i < flags.length; i++) {
            System.out.print(flags[i] + " ");
        }
        System.out.println(victim);
    }

    public void unlock(int threadID) {
        flags[threadID] = false;
    }
}
