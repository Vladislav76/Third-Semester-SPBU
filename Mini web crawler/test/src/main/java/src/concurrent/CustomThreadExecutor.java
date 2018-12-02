package src.concurrent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

public class CustomThreadExecutor implements ExecutorService {

    class mTasksExecutor implements Runnable {
        @Override
        public void run() {
            while (!isShutdown) {
                Runnable task = workQueue.poll();
                if (task != null) {
                    task.run();
                }
            }
        }
    }

    public CustomThreadExecutor(int threadsNumber) {
        this.threadsNumber = threadsNumber;
        threads = new Thread[threadsNumber];
        workQueue = new CustomConcurrentLinkedQueue<>();
        isShutdown = false;

        for (int i = 0; i < this.threadsNumber; i++) {
            threads[i] = new Thread(new mTasksExecutor());
            threads[i].start();
        }
    }

    @Override
    public void execute(Runnable task) {
        if (!isShutdown()) {
            workQueue.add(task);
        }
    }

    @Override
    public void shutdown() {
        isShutdown = true;
    }

    @Override
    public boolean isShutdown() {
        return isShutdown;
    }

    private int threadsNumber;
    private CustomConcurrentLinkedQueue<Runnable> workQueue;
    private Thread[] threads;
    private volatile boolean isShutdown;

    //STUBS
    public <T> Future<T> submit(Runnable task, T result) {return null;}
    public Future<?> submit(Runnable task) {return null;}
    public <T> Future<T> submit(Callable<T> task) {return null;}
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) {return null;}
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks) {return null;}
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) {return null;}
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks) {return null;}
    public List<Runnable> shutdownNow() {return null;}
    public boolean isTerminated() {return false;}
    public boolean awaitTermination(long timeout, TimeUnit unit) {return false;}
}
