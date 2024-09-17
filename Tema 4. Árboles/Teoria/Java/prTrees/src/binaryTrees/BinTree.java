package binaryTrees;/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 4. Árboles
  Profesor: Pablo López
  Alumno: Ángel Manuel Soria Gil
*/

import dataStructures.list.ArrayList;
import dataStructures.list.List;

public class BinTree<T> {

    private static class Tree<E> {
        private final E elem;
        private final Tree<E> left;
        private final Tree<E> right;

        public Tree(E e, Tree<E> l, Tree<E> r) {
            elem = e;
            left = l;
            right = r;
        }
    }

    private final Tree<T> root;

    public BinTree() {
        root = null;
    }

    public BinTree(T x) {
        root = new Tree<>(x, null, null);
    }

    public BinTree(T x, BinTree<T> l, BinTree<T> r) {
        root = new Tree<>(x, l.root, r.root);
    }

    private static boolean isLeaf(Tree<?> current) {
        return current != null && current.left == null && current.right == null;
    }

    public int weight() {
        // Número de nodos.
        return weightAux(root);
    }

    private int weightAux (Tree<T> tree){
        int res = 0;
        if (tree == null){
            res = 0;
        } else if (tree.left == null && tree.right == null){
            res = 1;
        } else {
            res = 1 + weightAux(tree.left) + weightAux(tree.right);
        }
        return res;
    }

    public int height() {
        return heightAux(root);
    }

    private int heightAux (Tree<T> tree){
        int res = 0;
        if (tree == null){
            res = 0;
        } else if (tree.left == null && tree.right == null){
            res = 1;
        } else {
            res = 1 + Math.max(heightAux(tree.left), heightAux(tree.right));
        }
        return res;
    }

    public List<T> border() {
        return borderAux(root);
    }

    private List<T> borderAux(Tree<T> tree){
        List<T> res = new ArrayList<>();
        if (tree == null){
            return res;
        } else if (tree.right == null && tree.left == null){
            res.append(tree.elem);
        } else {
            res = borderAux(tree.left);
            List<T> resaux = borderAux(tree.right);
            for (int i = 0; i<resaux.size(); i++){
                res.append(resaux.get(i));
            }
        }
        return res;
    }

    public boolean isElem(T x) {
        return isElemAux (x, root);
    }

    private  boolean isElemAux (T x, Tree<T> tree){
        boolean res = false;
        if (tree == null){
            res = false;
        } else {
            res = x.equals(tree.elem) || isElemAux(x, tree.left) || isElemAux(x, tree.right);
        }
        return res;
    }

    public List<T> atLevel(int i) {
        return atLevelAux(i, root);
    }

    private List<T> atLevelAux (int i, Tree<T> tree){ // Aquí tomamos el nivel raíz como 1
        List<T> res = new ArrayList<>();
        if (tree == null) {
            res = res;
        } else if (i==1) {
            res.append(tree.elem);
        } else {
            List<T> resl = atLevelAux(i-1, tree.left);
            List<T> resr = atLevelAux( i-1, tree.right);
            for (int j = 0; j<resl.size(); j++){
                res.append(resl.get(j));
            }
            for (int j = 0; j<resr.size(); j++){
                res.append(resr.get(j));
            }
        }
        return res;
    }

    public List<T> inOrder() {
        return inOrderAux(root);
    }

    private List<T> inOrderAux(Tree<T> tree){
        List<T> res = new ArrayList<>();
        if (tree == null) {
            res = res;
        } else if (tree.left == null && tree.right == null){
            res.append(tree.elem);
        } else {
            List<T> resl = inOrderAux(tree.left);
            List<T> resr = inOrderAux(tree.right);
            for (int j = 0; j<resl.size(); j++){
                res.append(resl.get(j));
            }
            res.append(tree.elem);
            for (int j = 0; j<resr.size(); j++){
                res.append(resr.get(j));
            }
        }
        return res;
    }

    /**
     * Returns representation of tree as a String.
     */
    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + toStringRec(this.root) + ")";
    }

    private static String toStringRec(Tree<?> tree) {
        return tree == null ? "null" : "Node<" + toStringRec(tree.left) + ","
                + tree.elem + "," + toStringRec(tree.right)
                + ">";
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

    private static void toDotRec(Tree<?> current, StringBuffer sb) {
        if (current != null) {
            final int currentId = System.identityHashCode(current);
            sb.append(String.format("%d [label=\"%s\"];\n", currentId, current.elem));
            if (!isLeaf(current)) {
                processChild(current.left, sb, currentId);
                processChild(current.right, sb, currentId);
            }
        }
    }

    private static void processChild(Tree<?> child, StringBuffer sb, int parentId) {
        if (child == null) {
            sb.append(String.format("l%d [style=invis];\n", parentId));
            sb.append(String.format("%d -> l%d;\n", parentId, parentId));
        } else {
            sb.append(String.format("%d -> %d;\n", parentId, System.identityHashCode(child)));
            toDotRec(child, sb);
        }
    }
}
