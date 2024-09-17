/**
 * Estructuras de Datos. Grado en Informática, IS e IC. UMA.
 * Examen de Febrero 2015.
 *
 * Implementación del TAD Deque
 *
 * Apellidos:
 * Nombre:
 * Grado en Ingeniería ...
 * Grupo:
 * Número de PC:
 */

package dataStructures.doubleEndedQueue;

public class LinkedDoubleEndedQueue<T> implements DoubleEndedQueue<T> {

    private static class Node<E> {
        private E elem;
        private Node<E> next;
        private Node<E> prev;

        public Node(E x, Node<E> nxt, Node<E> prv) {
            elem = x;
            next = nxt;
            prev = prv;
        }
    }

    private Node<T> first, last;

    /**
     *  Invariants:
     *  if queue is empty then both first and last are null
     *  if queue is non-empty:
     *      * first is a reference to first node and last is ref to last node
     *      * first.prev is null
     *      * last.next is null
     *      * rest of nodes are doubly linked
     */

    /**
     * Complexity: O(1)
     */
    public LinkedDoubleEndedQueue() {
        this.first = null;
        this.last = null;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public boolean isEmpty() {
        return first == null && last == null;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addFirst(T x) {
        Node<T> nF = new Node<>(x, first, null); //Creo nodo con el elemento
        if (isEmpty()){
            first = nF;
            last = nF;
        } else {
            first.prev = nF;
            first = nF;
        }
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addLast(T x) {
        Node<T> nL = new Node<>(x, null, last);
        if (isEmpty()) {
            first = nL;
            last = nL;
        } else {
            last.next = nL;
            last = nL;
        }
    }

    /**
     * Complexity: o(1)
     */
    @Override
    public T first() {
        return first.elem;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public T last() {
        return last.elem;
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteFirst() {
        if (!isEmpty()){
            if (first.next==null){
                first = null;
                last = null;
            } else {
                first.next.prev = null;
                first = first.next;
            }
        } else {
            throw new EmptyDoubleEndedQueueException("error: delete applied on empty deque");
        }
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteLast() {
        if (!isEmpty()){
            if (last.prev==null){
                first = null;
                last = null;
            } else {
                last.prev.next = null;
                last = last.prev;
            }
        } else {
            throw new EmptyDoubleEndedQueueException("error: delete applied on empty deque");
        }
    }

    /**
     * Returns representation of queue as a String.
     */
    @Override
    public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);
        String s = className+"(";
        for (Node<T> node = first; node != null; node = node.next)
            s += node.elem + (node.next != null ? "," : "");
        s += ")";
        return s;
    }
}
