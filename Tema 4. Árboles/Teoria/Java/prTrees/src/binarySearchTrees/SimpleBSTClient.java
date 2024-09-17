package binarySearchTrees;/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 4. Árboles
  Pablo López
*/

import binarySearchTrees.SimpleBST;

import java.io.FileNotFoundException;
import java.io.PrintWriter;

public class SimpleBSTClient {

    public static void main(String[] args) {
        SimpleBST<Integer, String> bst = new SimpleBST<>();

        bst.insert(5, "cinco");
        bst.insert(2, "dos");
        bst.insert(8, "ocho");
        bst.insert(1, "uno");
        bst.insert(10, "diez");
        bst.insert(6, "seis");

        System.out.println(bst);
        saveTreeToDot("bst", bst);

        for (int i = 0; i < 12; i++) {
            System.out.printf("el %s se dice %s\n", i, bst.search(i));
        }

        bst.delete(5);
        bst.delete(1);
        bst.delete(8);
        System.out.println(bst);
        saveTreeToDot("bst-deleted", bst);
    }

    private static void saveTreeToDot(String name, SimpleBST<?, ?> tree) {
        try (PrintWriter pw = new PrintWriter("dot/" + name + ".dot")) {
            pw.println(tree.toDot(name));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
