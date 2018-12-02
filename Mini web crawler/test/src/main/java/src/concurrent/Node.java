package src.concurrent;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Node<E> {

    public E item;
    public Node<E> next;
    public Node<E> pred;
    public int key;
    private Lock lock;

    public Node(E e) {
        item = e;
        pred = null;
        next = null;
        lock = new ReentrantLock();
    }

    public void lock() {
        lock.lock();
    }

    public void unlock() {
        lock.unlock();
    }
}
