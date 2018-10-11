package org.src;

import java.util.concurrent.atomic.AtomicBoolean;

public class TTASLock implements Lock {

    AtomicBoolean obj = new AtomicBoolean(false);

    @Override
    public void lock() {
        while (true) {
            while (obj.get()) {}
            if (!obj.getAndSet(true)) {
                return;
            }
        }
    }

    @Override
    public void unlock() {
        obj.set(false);
    }
}
