package src.concurrent;

public class CustomConcurrentSkipListSet<E> {

    public CustomConcurrentSkipListSet() {
        head = new Node<>(null);
        tail = new Node<>(null);
        head.setNext(tail);
        size = 0;
    }

    public int size() {
        return size;
    }

    public boolean add(E e) {
        int key = e.hashCode();
        head.lock();
        head.getNext().lock();
        Node<E> pred = head;
        Node<E> curr = head.getNext();
        while (curr != tail && curr.getKey() <= key) {
            if (e.equals(curr.getItem())) {
                pred.unlock();
                curr.unlock();
                return false;
            }
            pred.unlock();
            pred = curr;
            curr = curr.getNext();
            curr.lock();
        }
        Node<E> newNode = new Node<>(e);
        size++;
        newNode.setKey(key);
        newNode.setNext(curr);
        pred.setNext(newNode);
        pred.unlock();
        curr.unlock();
        return true;
    }

    public boolean contains(E e) {
        int key = e.hashCode();
        head.lock();
        head.getNext().lock();
        Node<E> pred = head;
        Node<E> curr = head.getNext();
        try {
            curr = head.getNext();
            while (curr != tail && curr.getKey() <= key) {
                if (e.equals(curr.getItem())) {
                    return true;
                }
                pred.unlock();
                curr = curr.getNext();
                curr.lock();
            }
            return false;
        }
        finally {
            pred.unlock();
            curr.unlock();
        }
    }

    private Node<E> head;
    private Node<E> tail;
    private volatile int size;
}


