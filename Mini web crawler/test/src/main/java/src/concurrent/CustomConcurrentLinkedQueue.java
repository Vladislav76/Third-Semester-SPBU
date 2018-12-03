package src.concurrent;

public class CustomConcurrentLinkedQueue<E> {

    public CustomConcurrentLinkedQueue() {
        head = new Node<>(null);
        tail = head;
        size = 0;
    }

    public int size() {
        return size;
    }

    public boolean add(E e) {
        tail.lock();
        size++;
        Node<E> newNode = new Node<>(e);
        newNode.lock();
        Node<E> oldTail = tail;
        tail.setNext(newNode);
        tail = newNode;
        oldTail.unlock();
        newNode.unlock();
        return true;
    }

    public E poll() {
        head.lock();
        try {
            if (head == tail) {
                return null;
            }
            size--;
            Node<E> result = head.getNext();
            if (result == tail) {
                tail = head;
                head.setNext(null);
            }
            else {
                head.setNext(result.getNext());
            }
            return result.getItem();
        }
        finally {
            head.unlock();
        }
    }

    private Node<E> head;
    private Node<E> tail;
    private volatile int size;
}
