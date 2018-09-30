public class CustomLock {

     volatile private int[] level;
     volatile private int[] victim;

    public CustomLock(int threadsNumber) {
        level = new int[threadsNumber];
        victim = new int[threadsNumber];
    }

    private boolean isOther(int threadID, int l) {
        for (int i = 0; i < level.length; i++) {
            if (i != threadID && level[i] >= l) {
                return true;
            }
        }
        return false;
    }

    public void lock(int threadID) {
        for (int l = 1; l < level.length; l++) {
            level[threadID] = l;
            victim[l] = threadID;
            while (isOther(threadID, l) && victim[l] == threadID) {}
        }
    }

    public void info() {
        String s = "";
        StringBuffer sb = new StringBuffer();
        StringBuffer sb2 = new StringBuffer();
        for (int i = 0; i < level.length; i++) {
            sb.append(level[i]);
            sb2.append(victim[i]);
        }
        System.out.println(sb.toString() + " " + sb2.toString());
    }

    public void unlock(int threadID) {
        level[threadID] = 0;
    }
}
