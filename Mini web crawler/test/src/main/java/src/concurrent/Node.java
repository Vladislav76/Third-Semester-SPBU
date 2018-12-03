package src.concurrent;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Node<E> {

    public Node(E e) {
        item = e;
        lock = new ReentrantLock();
    }

    public void setItem(E item) {
        this.item = item;
    }
    public void setKey(int key) {
        this.key = key;
    }
    public void setNext(Node<E> next) {
        this.next = next;
    }

    public Node<E> getNext() {
        return next;
    }
    public E getItem() {
        return item;
    }
    public int getKey() {
        return key;
    }

    public void lock() {
        lock.lock();
    }
    public void unlock() {
        lock.unlock();
    }

    private E item;
    private Node<E> next;
    private int key;
    private Lock lock;
}
