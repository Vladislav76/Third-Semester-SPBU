package src.concurrent;

public class CustomConcurrentLinkedQueue<E> {

    public CustomConcurrentLinkedQueue() {
        head = new Node<>(null);
        tail = new Node<>(null);
        head.next = tail;
        tail.pred = head;
        size = 0;
    }

    public int size() {
        return size;
    }

    public boolean add(E e) {
        tail.lock();
        Node<E> newNode = new Node<>(e);
        size++;
        tail.pred.next = newNode;
        newNode.next = tail;
        newNode.pred = tail.pred;
        tail.pred = newNode;
        tail.unlock();
        return true;
    }

    public E poll() {
        head.lock();
        if (head.next != tail) {
            E result = head.next.item;
            head.next = head.next.next;
            head.next.pred = head;
            size--;
            head.unlock();
            return result;
        }
        else {
            head.unlock();
            return null;
        }
    }

    private Node<E> head;
    private Node<E> tail;
    private int size;

//    public static void main(String[] args) {
//
//        for (int i = 0; i < 10; i++) {
//        CustomConcurrentLinkedQueue<String> set = new CustomConcurrentLinkedQueue<>();
//
//        Thread thread1 = new Thread(() -> {
//            set.add("A");
//            set.add("C");
//        });
//
//        Thread thread2 = new Thread(() -> {
//            set.add("B");
//            System.out.println(set.poll());
//            set.add("C");
//            System.out.println(set.poll());
//        });
//
//        thread1.start();
//        thread2.start();
//
//        try {
//            thread1.join();
//            thread2.join();
//        }
//        catch (InterruptedException e) {
//            e.printStackTrace();
//        }
//
//        System.out.println(set.size());
//        }
//    }
}
