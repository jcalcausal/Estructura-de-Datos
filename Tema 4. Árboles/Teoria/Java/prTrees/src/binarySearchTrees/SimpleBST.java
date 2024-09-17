package binarySearchTrees;/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 4. Árboles
  Pablo López
*/

import dataStructures.searchTree.BST;

public class SimpleBST<K extends Comparable<? super K>, V> {

    private static class Tree<C, D> {
        C key;
        D value;
        Tree<C, D> left;
        Tree<C, D> right;

        public Tree(C key, D value, Tree<C, D> left, Tree<C, D> right) {
            this.key = key;
            this.value = value;
            this.left = left;
            this.right = right;
        }
    }

    private Tree<K, V> root;

    public SimpleBST() {
        root = null;
    }

    public void insert(K k, V v) {
        root = insertRec (k, v, root);
    }

    private Tree<K,V> insertRec (K k, V v, Tree<K,V> t){
        Tree<K,V> res = t;
        if (t==null){
            res = new Tree<>(k, v, null, null);
        } else {
            if (t.key.compareTo(k)==0){
                res.key = k;
                res.value = v;
            } else if (t.key.compareTo(k)<0){
                res.right = insertRec(k, v, t.right);
            } else {
                res.left = insertRec(k, v, t.left);
            }
        }
        return res;
    }

    public V search(K k) {
        return searchRec(k, root);
    }

    private V searchRec(K k, Tree<K,V> t){
        V val = null;
        if (t==null){
            val = null;
        } else {
            if (t.key.compareTo(k)==0){
                val = t.value;
            } else if (t.key.compareTo(k)<0){
                val = searchRec(k, t.right);
            } else {
                val = searchRec(k, t.left);
            }
        }
        return val;
    }

    public void delete(K k) {
        root = deleteRec(k, root);
    }

    private Tree<K,V> deleteRec(K k, Tree<K,V> t){
        Tree<K,V> res = t;
        if (t==null){
            res = null;
        } else {
            if (t.key.compareTo(k)==0){
                res = combine (t.left, t.right);
            } else if (t.key.compareTo(k)<0){
                res.right = deleteRec(k, t.right);
            } else {
                res.left = deleteRec(k, t.left);
            }
        }
        return res;
    }

    private Tree<K,V> combine (Tree<K,V> l, Tree<K,V> r){
        Tree<K,V> res;
        if (l == null && r == null){
            res = null;
        } else if (l == null && !(r == null)){
            res = r;
        } else if (!(l==null) && r==null){
            res = l;
        } else {
            K keyRoot = KMinR (r);
            V valRoot = VMinR (r);
            res = new Tree<>(keyRoot, valRoot, l, split(r));
        }
        return res;
    }

    private K KMinR (Tree<K,V> t){
        while (t.left != null){
            t = t.left;
        }
        return  t.key;
    }

    private V VMinR (Tree<K,V> t){
        while (t.left != null){
            t = t.left;
        }
        return  t.value;
    }

    private Tree<K,V> split (Tree<K,V> tree){
        if (tree.left == null){
            return tree.right;
        } else {
            tree.left = split(tree.left);
            return tree;
        }
    }

    /**
     * Returns representation of tree as a String.
     */
    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + toStringRec(root) + ")";
    }

    private static <K,V> String toStringRec(Tree<K,V> tree) {
        if (tree == null) {
            return "";
        }
        else {
            String left = toStringRec(tree.left);
            left += String.format("(%s, %s) ", tree.key, tree.value);
            return left + toStringRec(tree.right);
        }
    }

    /**
     * Returns a String with the representation of tree in DOT (graphviz).
     */
    public String toDot(String treeName) {
        final StringBuffer sb = new StringBuffer();
        sb.append(String.format("digraph \"%s\" {\n", treeName));
        sb.append("node [fontname=\"Arial\", fontcolor=red, shape=circle, style=filled, color=\"#66B268\", fillcolor=\"#AFF4AF\" ];\n");
        sb.append("edge [color = \"#0070BF\"];\n");
        toDotRec(root, sb);
        sb.append("}");
        return sb.toString();
    }

    private static void toDotRec(Tree<?,?> current, StringBuffer sb) {
        if (current != null) {
            final int currentId = System.identityHashCode(current);
            sb.append(String.format("%d [label=\"%s\"];\n", currentId, current.key));
            if (!isLeaf(current)) {
                processChild(current.left, sb, currentId);
                processChild(current.right, sb, currentId);
            }
        }
    }

    private static boolean isLeaf(Tree<?,?> current) {
        return current != null && current.right == null && current.left == null;
    }

    private static void processChild(Tree<?,?> child, StringBuffer sb, int parentId) {
        if (child == null) {
            sb.append(String.format("l%d [style=invis];\n", parentId));
            sb.append(String.format("%d -> l%d;\n", parentId, parentId));
        } else {
            sb.append(String.format("%d -> %d;\n", parentId, System.identityHashCode(child)));
            toDotRec(child, sb);
        }
    }
}
