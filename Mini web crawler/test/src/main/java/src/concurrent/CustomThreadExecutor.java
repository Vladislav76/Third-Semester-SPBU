package src.concurrent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

public class CustomThreadExecutor implements ExecutorService {

    class mTasksExecutor implements Runnable {
        @Override
        public void run() {
            while (!isShutdown.get()) {
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
        isShutdown = new AtomicBoolean(false);

        for (int i = 0; i < this.threadsNumber; i++) {
            threads[i] = new Thread(new mTasksExecutor());
            threads[i].start();
        }
    }

    @Override
    public void execute(Runnable task) {
        if (!isShutdown.get()) {
            workQueue.add(task);
        }
    }

    @Override
    public void shutdown() {
        isShutdown.set(true);
    }

    @Override
    public boolean isShutdown() {
        return isShutdown.get();
    }

    private int threadsNumber;
    private CustomConcurrentLinkedQueue<Runnable> workQueue;
    private Thread[] threads;
    private AtomicBoolean isShutdown;

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
