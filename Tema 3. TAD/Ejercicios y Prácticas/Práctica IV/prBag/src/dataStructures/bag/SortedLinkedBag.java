package dataStructures.bag;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringJoiner;
//Algo no va bien aquí.
public class SortedLinkedBag<T extends Comparable<? super T>> implements Bag<T> {

    private static class Node<E> {
        private E elem;
        private int count;
        private Node<E> next;

        public Node(E x, Node<E> nextNode) {
            elem = x;
            count = 1;
            next = nextNode;
        }
    }

    /**
     * <strong>Representation invariant:</strong>
     * <p>
     * 1. the linked sequence referenced by {@code first} must be sorted by {@code elem}, with no duplicates
     * <p>
     * 2. {@code count} in every {@code Node} must be positive
     * <p>
     * 3. {@code size} must be equal to the sum of all {@code count} in the linked sequence
     * referenced by {@code first}
     * <p>
     * <strong>Example:</strong>
     * <pre>
     *    first -> ('a', 5) -> ('d', 1) -> ('t', 3) -> ('z', 2)
     *    size = 11
     * </pre>
     */
    private Node<T> first;
    private int size;

    public SortedLinkedBag() {
        first = null;
        size = 0;
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
    public void insert(T x) {
        Node<T> previous = null;
        Node<T> current = first;
        while (current != null && current.elem.compareTo(x)<0){
            previous = current;
            current = current.next;
        }
        boolean found = (current != null) && (current.elem.compareTo(x)==0);
        if (!found) {
            if (previous == null) { //caso para la bolsa vacía
                first = new Node<>(x, first);
            } else {
                previous.next = new Node<>(x, current);
            }
        } else {
            current.count++;
        }
        size++;
    }

    @Override
    public int occurrences(T x) {
        Node<T> previous = null;
        Node<T> current = first;
        int res;
        while (current != null && current.elem.compareTo(x)<0){
            previous = current;
            current = current.next;
        }
        boolean found = (current != null) && (current.elem.compareTo(x)==0);
        if (!found) {
                res = 0;
        } else {
                res = current.count;
        }
        return res;
    }


    @Override
    public void delete(T x) {
        Node<T> previous = null;
        Node<T> current = first;
        while (current != null && current.elem.compareTo(x)<0){
            previous = current;
            current = current.next;
        }
        boolean found = (current != null) && (current.elem.compareTo(x)==0);
        if (found) {
            size = size - current.count;
            current = current.next;
        }
    }

    @Override
    public String toString() {
        StringJoiner sj = new StringJoiner(" ", "Bag(", ")");
        for (Node<T> current = first; current != null; current = current.next) {
            sj.add(String.format("(%s, %s)", current.elem, current.count));
        }
        return sj.toString();
    }

    @Override
    public Iterator<T> iterator() {
        return new SortedLinkedBagIterator();
    }

    private class SortedLinkedBagIterator implements Iterator<T>{
        private Node<T> current;

        public SortedLinkedBagIterator(){
            current = first;
        }

        @Override
        public boolean hasNext() {
            return current.next == null;
        }

        @Override
        public T next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            } else {
                current = current.next;
                return current.next.elem;

            }
        }
    }
}
