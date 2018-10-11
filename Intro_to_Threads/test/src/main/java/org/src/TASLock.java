package org.src;

import java.util.concurrent.atomic.AtomicBoolean;

public class TASLock implements Lock {

    private AtomicBoolean obj = new AtomicBoolean(false);

    @Override
    public void lock() {
        while (obj.getAndSet(true)) {}
    }

    @Override
    public void unlock() {
        obj.set(false);
    }
}
