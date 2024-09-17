
/**
 * Set implementation using an array.
 */
package Sets;

import dataStructures.set.Set;
import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringJoiner;

public class MyArraySet<T> implements Set<T> {

    private static final int INITIAL_CAPACITY = 1;

    /**
     * <strong>Representation invariant:</strong>
     * <p>
     * 1. the slice {@code 0 <= i < size} of array {@code elements} stores the contents of the set,
     * in any order, with no duplicates
     * <p>
     * 2. the slice {@code size <= i < elements.length} is available
     * <p>
     * 3. {@code 0 <= size <= elements.length}
     * <p>
     * <strong>Example:</strong>
     * <pre>
     *      elements = {7 ,1, 5, 3, -4, 2}
     *      size = 6
     *  </pre>
     */
    private T[] elements;
    private int size;

    @SuppressWarnings("unchecked")
    public MyArraySet() {
        elements = (T[]) new Object[INITIAL_CAPACITY];
        size = 0;
    }

    private void ensureCapacity() {
        if (size == elements.length) {
            elements = Arrays.copyOf(elements, 2 * elements.length);
        }
    }

    private int locate(T x) {
        int i = 0;
        while (i < size && !elements[i].equals(x)) {
            i++;
        }
        return i;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isElem(T x) {
        int i = locate(x);
        return i < size;
    }

    @Override
    public void insert(T x) {
        if (!isElem(x)) {
            ensureCapacity();
            elements[size] = x;
            size++;
        }
    }

    @Override
    public void delete(T x) {
        int i = locate(x);
        if (i < size) {
            elements[i] = elements[size - 1];
            elements[size - 1] = null;
            size--;
        }
    }

    @Override
    public String toString() {
        StringJoiner sj = new StringJoiner(",", "Conjunto(", ")");
        for (int i = 0; i < size; i++) {
            sj.add(elements[i].toString());
        }
        return sj.toString();
    }

    @Override
    public Iterator<T> iterator() {
        return  new MyArraySetIterator();
    }

    private class MyArraySetIterator implements Iterator<T>{
        private int current;

        public MyArraySetIterator(){
            current = 0;
        }

        @Override
        public boolean hasNext() {
            return current<size;
        }

        @Override
        public T next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            } else {
                current++;
                return elements[current];

            }
        }
    }
}
