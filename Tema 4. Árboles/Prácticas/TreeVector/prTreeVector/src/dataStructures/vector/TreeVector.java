/*
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 * PRACTICA 7. Vectores
 * APELLIDOS, NOMBRE: Soria Gil, Ángel Manuel Soria Gil
 */

package dataStructures.vector;

import com.sun.source.tree.LiteralTree;
import dataStructures.list.ArrayList;
import dataStructures.list.List;

public class TreeVector<T> {

    private final int size;
    private final Tree<T> root;

    private interface Tree<E> {
        E get(int index);

        void set(int index, E x);

        List<E> toList();
    }

    private static class Leaf<E> implements Tree<E> {
        private E value;

        public Leaf(E x) {
            value = x;
        }

        @Override
        public E get(int i) {
            return this.value;
        }

        @Override
        public void set(int i, E x) {
            value = x;
        }

        @Override
        public List<E> toList() {
            List <E> res = new ArrayList<E>();
            res.append(this.value);
            return res;
        }
    }

    private static class Node<E> implements Tree<E> {
        private Tree<E> left;
        private Tree<E> right;

        public Node(Tree<E> l, Tree<E> r) {
            left = l;
            right = r;
        }

        @Override
        public E get(int i) {
            if (i%2==0){
                return left.get(i/2);
            } else {
                return right.get(i/2);
            }
        }


        @Override
        public void set(int i, E x) {
            if (i%2==0){
                left.set(i/2, x);
            } else {
                right.set(i/2, x);
            }
        }

        @Override
        public List<E> toList() {
            return intercalate(left.toList(), right.toList());
        }
    }

    public TreeVector(int n, T value) {
        size = power (2,n);
        if (n==0){
            root = new Leaf<>(value);
        } else {
            root = nodeAux(n, value);
        }
    }

    private int power (int base, int exp){
        if (exp==0){
            return 1;
        } else if (exp==1){
            return base;
        } else {
            return base*power(base, exp-1);
        }
    }

    private TreeVector(int n, Tree<T> t){
        size = n;
        root = t;
    }


    private Tree<T> nodeAux (int n, T value){
        if (n==0){
            return new Leaf<>(value);
        } else {
            return  new Node<>(nodeAux(n-1, value), nodeAux(n-1, value));
        }
    }

    public int size() {
        return size;
    }

    public T get(int i) {
        return root.get(i);
    }

    public void set(int i, T x) {
        root.set(i, x);
    }

    public List<T> toList() {
        return root.toList();
    }

    protected static <E> List<E> intercalate(List<E> xs, List<E> ys) {
        List<E> res = new ArrayList<>();
        int izq = 0;
        int dr = 0;
        for (int n = 0; n<2*xs.size(); n++){
            if (n%2==0){
                res.append(xs.get(izq));
                izq++;
            } else {
                res.append(ys.get(dr));
                dr++;
            }
        }
        return res;
    }

    static protected boolean isPowerOfTwo(int n) {
        boolean res;
        if (n<=0){
            res = false;
        } else if (n==1){
            res = true;
        } else {
            res = (n%2==0) && isPowerOfTwo(n/2);
        }
        return res;
    }

    public static <E> TreeVector<E> fromList(List<E> l) {
        TreeVector<E> res;
        if (l.size()==0){
            throw new VectorException("Error: empty list");
        } else if (l.size()==1){
            res = new TreeVector<>(1, l.get(0));
        } else {
            res = new TreeVector<>(log2(l.size()), btAux(l));
        }
        return res;
    }

    private static int log2(int n){
        int res;
        if (!isPowerOfTwo(n)){
            res = -1;
        } else if (n==1){
            res = 0;
        } else {
            res = 1 + log2(n/2);
        }
        return res;
    }

    private static <E> Tree<E> btAux (List<E> l){
        if (l.size() == 1) {
            return new Leaf<>(l.get(0));
        } else {
            return new Node<>(btAux(l1(l)), btAux(l2(l)));
        }
    }

    private static <E> List<E> l1 (List<E> l){
        List<E> res = l2(l);
        if (l.isEmpty()){
            return l;
        } else {
            E el = l.get(0);
            l.remove(0);
            res.insert(0, el);
            return res;
        }
    }

    private static <E> List<E> l2 (List<E> l){
        if (l.isEmpty()){
            return l;
        } else {
            l.remove(0);
            return l1(l);
        }
    }
}
