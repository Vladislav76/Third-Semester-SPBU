package src.concurrent;

public class CustomConcurrentSkipListSet<E> {

    public CustomConcurrentSkipListSet() {
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
        int key = e.hashCode();
        head.lock();
        head.next.lock();
        Node<E> curr = head.next;
        while (curr != tail && curr.key <= key) {
            if (e.equals(curr.item)) {
                curr.pred.unlock();
                curr.unlock();
                return false;
            }
            curr.pred.unlock();
            curr = curr.next;
            curr.lock();
        }
        Node<E> newNode = new Node<>(e);
        size++;
        newNode.key = key;
        newNode.next = curr;
        newNode.pred = curr.pred;
        curr.pred.next = newNode;
        curr.pred = newNode;
        curr.unlock();
        newNode.pred.unlock();
        return true;
    }

    public boolean contains(E e) {
        int key = e.hashCode();
        Node<E> curr = null;
        try {
            head.lock();
            curr = head.next;
            head.next.lock();
            while (curr != tail && curr.key <= key) {
                if (e.equals(curr.item)) {
                    return true;
                }
                curr.pred.unlock();
                curr = curr.next;
                curr.lock();
            }
            return false;
        }
        finally {
            curr.pred.unlock();
            curr.unlock();
        }
    }

    private Node<E> head;
    private Node<E> tail;
    private int size;

//    public static void main(String[] args) {
//
//        for (int i = 0; i < 100; i++) {
//            CustomConcurrentSkipListSet<String> set = new CustomConcurrentSkipListSet<>();
//            try {
//                Thread thread1 = new Thread(() -> {
//                    set.add("A");
//                    set.add("B");
//                    set.add("C");
//                });
//
//                Thread thread2 = new Thread(() -> {
//                    set.add("A");
//                    set.add("C");
//                    set.add("D");
//                    set.add("E");
//                    set.add("A");
//                });
//
//                thread1.start();
//                thread2.start();
//
//                thread1.join();
//                thread2.join();
//            } catch (InterruptedException e) {
//                e.printStackTrace();
//            }
//
//            System.out.println(set.contains("B"));
//        }
//    }
}


