/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 4. Árboles
  Pablo López
*/

package binarios;

import dataStructures.list.LinkedList;
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

    public int weight() { //BinTree no es recursivo
        return weight(root);
    }

    private static weight (Tree<?> t){ //Tree es recursivo
        if (t==Null){
            return 0;
        }else {
            return 1 + weight(t.left) + weight(t.right);
        }

    }
    public int height() {
        // TODO
        return 0;
    }

    public List<T> border() {
        // TODO
        return null;
    }

    public boolean isElem(T x) {
        // TODO
        return false;
    }

    public List<T> atLevel(int i) {
        // TODO
        return null;
    }

    public List<T> inOrder() {
        // TODO
        return null;
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
