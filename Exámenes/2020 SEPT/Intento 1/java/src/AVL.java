/**
 * Student's name: Juan Carlos Alcausa Luque
 *
 * Student's group:
 */

import dataStructures.list.List;
import dataStructures.list.LinkedList;

import java.util.Iterator;
import java.util.NoSuchElementException;


class Bin {
    private int remainingCapacity; // capacity left for this bin
    private List<Integer> weights; // weights of objects included in this bin

    public Bin(int initialCapacity) {
        remainingCapacity = initialCapacity;
        weights = new LinkedList<>();
    }

    // returns capacity left for this bin
    public int remainingCapacity() {
        return remainingCapacity;
    }

    // adds a new object to this bin
    public void addObject(int weight) {
        if (weight <= remainingCapacity){
            weights.append(weight);
            remainingCapacity -=weight;
        }
    }

    // returns an iterable through weights of objects included in this bin
    public Iterable<Integer> objects() {
        return new Iterable<Integer>() {
            public Iterator<Integer> iterator() {
                return weights.iterator();
            }
        };
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        sb.append(remainingCapacity);
        sb.append(", ");
        sb.append(weights.toString());
        sb.append(")");
        return sb.toString();
    }
}

// Class for representing an AVL tree of bins
public class AVL {
    static private class Node {
        Bin bin; // Bin stored in this node
        int height; // height of this node in AVL tree
        int maxRemainingCapacity; // max capacity left among all bins in tree rooted at this node
        Node left, right; // left and right children of this node in AVL tree

        // recomputes height of this node
        void setHeight() {
            if (bin == null)
                height = 0;
            else if (left == null && right == null)
                height = 1;
            else if (left == null)
                height = 1 + height(right);
            else if (right == null)
                height = 1 + height(left);
            else
                height = 1 + Math.max(height(left), height(right));

        }

        // recomputes max capacity among bins in tree rooted at this node
        void setMaxRemainingCapacity() {
            if (bin == null)
                maxRemainingCapacity = 0;
            else if (left == null && right == null)
                maxRemainingCapacity = bin.remainingCapacity();
            else if (left == null)
                maxRemainingCapacity = Math.max (bin.remainingCapacity(), maxRemainingCapacity(right));
            else if (right == null)
                maxRemainingCapacity = Math.max (bin.remainingCapacity(), maxRemainingCapacity(left));
            else
                maxRemainingCapacity = Math.max(maxRemainingCapacity(left),
                        Math.max (bin.remainingCapacity(), maxRemainingCapacity(right)));
        }

        // left-rotates this node. Returns root of resulting rotated tree
        Node rotateLeft() {
            Bin c = bin;
            Bin x = right.bin;
            Node l = left;
            Node r1 = right.left;
            Node r2 = right.right;
            Node res = new Node();
            res.bin = x;
            res.left = new Node();
            res.left.bin = c;
            res.left.left = l;
            res.left.right = r1;
            res.right = r2;
            res.setHeight();
            res.setMaxRemainingCapacity();
            return res;
        }
    }

    private static int height(Node node) {
        if (node == null)
            return 0;
        return node.maxRemainingCapacity;
    }

    private static int maxRemainingCapacity(Node node) {
        if (node == null)
            return 0;
        return node.maxRemainingCapacity;
    }

    private Node root; // root of AVL tree

    public AVL() {
        this.root = null;
    }

    // adds a new bin at the end of right spine.

    private void addNewBin(Bin bin) {
        root = addNewBinRec(bin, root);
    }

    private Node addNewBinRec(Bin bin, Node root) {
        if (root == null) {
            root = new Node();
            root.bin = bin;
            return root;
        }
        else if (root.right == null) {
            root.right = new Node();
            root.right.bin = bin;
            root.right.setHeight();
            root.right.setMaxRemainingCapacity();
        }
        else{
                root.right = addNewBinRec(bin, root.right);
                if (height(root.right) - height(root.left) > 1){
                    root.rotateLeft();
                }
        }
        return root;
    }


    // adds an object to first suitable bin. Adds
    // a new bin if object cannot be inserted in any existing bin
    public void addFirst(int initialCapacity, int weight) {
        addFirstRec(initialCapacity, weight, root);
    }

    private void addFirstRec (int initialCapacity, int weight, Node n) {
        if (root == null || weight > maxRemainingCapacity(root)){
            Bin b = new Bin(initialCapacity);
            b.addObject(weight);
            addNewBin(b);
        }
        else if (maxRemainingCapacity(root.left) >= weight)
            addFirstRec(initialCapacity, weight, root.left);
        else if (root.bin.remainingCapacity() >= weight){
            root.bin.addObject(weight);
            root.setHeight();
            root.setMaxRemainingCapacity();
        }
        else {
            addFirstRec(initialCapacity, weight, root.right);
        }
    }
    public void addAll(int initialCapacity, int[] weights) {
        for (int w : weights)
            addFirst(initialCapacity, w);
    }

    public List<Bin> toList() {
        return toListRec(root);
    }

    public List<Bin> toListRec(Node n) {
        List<Bin> res = new LinkedList<>();
        if (n == null)
            return (res);
        else if (n.left == null && n.right == null)
            res.append(n.bin);
        else {
            for (Bin b : toListRec(n.left))
                res.append(b);
            res.append(n.bin);
            for (Bin b : toListRec(n.right))
                res.append(b);
        }
        return res;
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        stringBuild(sb, root);
        sb.append(")");
        return sb.toString();
    }

    private static void stringBuild(StringBuilder sb, Node node) {
        if(node==null)
            sb.append("null");
        else {
            sb.append(node.getClass().getSimpleName());
            sb.append("(");
            sb.append(node.bin);
            sb.append(", ");
            sb.append(node.height);
            sb.append(", ");
            sb.append(node.maxRemainingCapacity);
            sb.append(", ");
            stringBuild(sb, node.left);
            sb.append(", ");
            stringBuild(sb, node.right);
            sb.append(")");
        }
    }
}

class LinearBinPacking {
    public static List<Bin> linearBinPacking(int initialCapacity, List<Integer> weights) {
        List<Bin> res = new LinkedList<>();

        for (int w : weights) {
            boolean placed = false;

            //Intenta colocar el peso en alguno de los nodos existentes
            Iterator<Bin> it = res.iterator();
            while (it.hasNext()){
                Bin b = it.next();
                if (b.remainingCapacity() >= w) {
                    b.addObject(w);
                    placed = true;
                }

                //Si no hemos conseguido colocar el peso creamos un nuevo cubo
                if (!placed) {
                    Bin newBin = new Bin(initialCapacity);
                    newBin.addObject(w);
                    res.append(newBin);
                }
            }
        }
        return res;
    }
	
	public static Iterable<Integer> allWeights(Iterable<Bin> bins) {
        return new Iterable<Integer>() {
            @Override
            public Iterator<Integer> iterator() {
                return new Iterator<Integer>() {
                    private Iterator<Bin> binIterator = bins.iterator(); // Iterator para los bins
                    private Iterator<Integer> weightIterator = null; // Iterator para los pesos de los bins

                    @Override
                    public boolean hasNext() {
                        // Si hay un iterador de pesos actual que tenga elementos, o si el binIterator tiene más elementos
                        return (weightIterator != null && weightIterator.hasNext()) || binIterator.hasNext();
                    }

                    @Override
                    public Integer next() {
                        // Si no hay más pesos en el bin actual, avanzar al siguiente bin
                        if (weightIterator == null || !weightIterator.hasNext()) {
                            if (!binIterator.hasNext()) {
                                throw new NoSuchElementException();
                            }
                            // Avanzamos al siguiente bin y obtenemos su iterador de pesos
                            Bin currentBin = binIterator.next();
                            weightIterator = currentBin.objects().iterator(); // Suponiendo que objects() devuelve Iterable<Integer>
                        }
                        return weightIterator.next();
                    }
                };
            }
        };
	}
}