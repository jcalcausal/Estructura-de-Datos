/******************************************************************************
 * Student's name: Ángel Manuel Soria Gil
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. de Informática UMA.
******************************************************************************/

package dataStructures.vector;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

public class SparseVector<T> implements Iterable<T> {

    protected interface Tree<T> {

        T get(int sz, int i);

        Tree<T> set(int sz, int i, T x);
    }

    // Unif Implementation

    protected static class Unif<T> implements Tree<T> {

        private T elem;

        public Unif(T e) {
            elem = e;
        }

        @Override
        public T get(int sz, int i) {
            if (i<0 || i>=sz){
                throw new VectorException("error: IndexOutOfBounds");
            }
            return this.elem;
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            Tree<T> res;
            if (i<0 || i>=sz){
                throw new VectorException("error: IndexOutOfBounds");
            }
            if (this.elem.equals(x)){
                res = this;
            } else if (sz == 1){
                res = new Unif<>(x);
            } else {
                int mid = sz/2;
                int ni1 = i;
                int ni2 = i - mid;
                if (i<mid) {
                    res = new Node<>(this.set(mid, ni1, x), this);
                } else {
                    res = new Node<>(this, this.set(mid, ni2, x));
                }
            }
            return res;
        }
        @Override
        public String toString() {
            return "Unif(" + elem + ")";
        }
    }

    // Node Implementation

    protected static class Node<T> implements Tree<T> {

        private Tree<T> left, right;

        public Node(Tree<T> l, Tree<T> r) {
            left = l;
            right = r;
        }

        @Override
        public T get(int sz, int i) {
            T res;
            if (i<0 || i>=sz){
                throw new VectorException("error: IndexOutOfBounds");
            }
            int mid = sz/2;
            int ni1 = i;
            int ni2 = i - mid;
            if (i<mid) {
                res = this.left.get(mid, ni1);
            } else {
                res = this.right.get(mid, ni2);
            }
            return res;
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if (i<0 || i>=sz){
                throw new VectorException("error: IndexOutOfBounds");
            }
            int mid = sz/2;
            int ni1 = i;
            int ni2 = i - mid;
            if (i<mid) {
                this.left = this.left.set(mid, ni1, x);
            } else {
                this.right = this.right.set(mid, ni2, x);
            }
            return simplify();
        }

        protected Tree<T> simplify() {
            Tree<T> res;
            if (this.left instanceof Unif<T> && this.right instanceof Unif<T>){
                if (((Unif<T>) this.left).elem.equals(((Unif<T>) this.right).elem)){
                    res = this.left;
                } else {
                    res = this;
                }
            } else {
                res = this;
            }
            return res;
        }

        @Override
        public String toString() {
            return "Node(" + left + ", " + right + ")";
        }
    }

    // SparseVector Implementation

    private int size;
    private Tree<T> root;

    public SparseVector(int n, T elem) {
        if (n<0){
            throw new VectorException("error: not fractional size");
        }
        this.size = (int) Math.pow(2, n);
        this.root = new Unif<>(elem);
    }

    public int size() {
        return this.size;
    }

    public T get(int i) {
        if (i<0 || i>=this.size){
            throw new VectorException("error: IndexOutOfBounds");
        }
        return root.get(this.size, i);
    }

    public void set(int i, T x) {
        if (i<0 || i>=this.size){
            throw new VectorException("error: IndexOutOfBounds");
        }
        root = root.set(size, i, x);
    }

    @Override
    public Iterator<T> iterator() {
        List<T> res = new ArrayList<>();
            for (int i = 0; i<this.size; i++){
                res.add(this.get(i));
            }
        return res.iterator();
    }

    @Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }
}
