/**
 * Student's name: Ángel Manuel Soria Gil
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 */

import dataStructures.list.List;
import dataStructures.list.LinkedList;

import java.util.Iterator;


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
        if (remainingCapacity-weight>=0){
            remainingCapacity = remainingCapacity-weight;
            weights.append(weight);
        }
    }

    // returns an iterable through weights of objects included in this bin
    public Iterable<Integer> objects() {
        //  SOLO PARA ALUMNOS SIN EVALUACIÓN CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return weights;
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
            if (bin == null){
                height = 0;
            } else if (left == null && right == null){
                height = 1;
            } else if (left == null){
                height = 1 + right.height;
            } else if (right == null){
                height = 1 + left.height;
            } else {
                height = Math.max(height(left), height(right)) + 1;
            }

        }

        // recomputes max capacity among bins in tree rooted at this node
        void setMaxRemainingCapacity() {
            if (bin == null){
                maxRemainingCapacity = 0;
            } else if (left == null && right == null){
                maxRemainingCapacity = bin.remainingCapacity();
            } else if (left == null){
                maxRemainingCapacity = Math.max(maxRemainingCapacity(right), bin.remainingCapacity());
            } else if (right == null){
                maxRemainingCapacity = Math.max(maxRemainingCapacity(left), bin.remainingCapacity());
            } else {
                maxRemainingCapacity = Math.max(Math.max(maxRemainingCapacity(left), maxRemainingCapacity(right)),
                        bin.remainingCapacity());
            }

        }

        // left-rotates this node. Returns root of resulting rotated tree
        Node rotateLeft() {
            Bin r = right.bin;
            Bin c = bin;
            Node lc = left;
            Node rc = right.left;
            Node rr = right.right;
            Node node = new Node();
            node.bin = r;
            node.right = rr;
            node.left.bin = c;
            node.left.left = lc;
            node.left.right = rc;
            node.setHeight();
            node.setMaxRemainingCapacity();
            return node;
        }
    }

    private static int height(Node node) {
        if (node == null){
            return 0;
        } else {
            return node.height;
        }
    }

    private static int maxRemainingCapacity(Node node) {
        if (node == null){
            return 0;
        } else {
            return node.maxRemainingCapacity;
        }
    }

    private Node root; // root of AVL tree

    public AVL() {
        this.root = null;
    }

    // adds a new bin at the end of right spine.
    private void addNewBin(Bin bin) {
        if (root == null){
            root = new Node();
            root.bin = bin;
            root.left = new Node();
            root.right = new Node();
            root.setMaxRemainingCapacity();
            root.setHeight();
        } else {
            addNewBinRec(bin, root);
        }
    }

    private void addNewBinRec(Bin bin, Node root) {
        if (root.right==null){
            root.right=new Node();
            root.right.bin = bin;
            root.right.setHeight();
            root.right.setMaxRemainingCapacity();
        } else {
            addNewBinRec(bin, root.right);
            if (height(root.right) - height(root.left) > 1){
                root.rotateLeft();
                root.left.setMaxRemainingCapacity();
                root.left.setHeight();
                root.right.setMaxRemainingCapacity();
                root.right.setHeight();
            }
        }

    }

    // adds an object to first suitable bin. Adds
    // a new bin if object cannot be inserted in any existing bin
    public void addFirst(int initialCapacity, int weight) {
        if (maxRemainingCapacity(root)<weight){
            Bin b = new Bin(initialCapacity);
            b.addObject(weight);
            addNewBin(b);
        } else {
            addFirstRec(initialCapacity, weight, root);
        }
    }

    private void addFirstRec(int initialCapacity, int weight, Node root) {
        if (maxRemainingCapacity(root.left)>=weight){
            addFirstRec(initialCapacity, weight, root.left);
        } else if (maxRemainingCapacity(root)>=weight){
            root.bin.addObject(weight);
            root.setHeight();
            root.setMaxRemainingCapacity();
        } else {
            addFirstRec(initialCapacity, weight, root.right);
        }
    }

    public void addAll(int initialCapacity, int[] weights) {
        for (int i = 0; i<weights.length; i++){
            addFirst(initialCapacity, weights[i]);
        }
    }

    public List<Bin> toList() {
        return toListRec(root);
    }

    private List<Bin> toListRec(Node root) {
        List<Bin> res = new LinkedList<>();
        if (root==null){
            return res;
        } else if (root.right == null && root.left == null){
            res.append(root.bin);
        } else {
            for (Bin b : toListRec(root.left)){
                res.append(b);
            }
            res.append(root.bin);
            for (Bin b : toListRec(root.right)){
                res.append(b);
            }
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
        // todo
        //  SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return null;
    }

	public static Iterable<Integer> allWeights(Iterable<Bin> bins) {
        // todo
        //  SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return null;
	}
}