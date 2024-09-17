/**
 * Estructuras de Datos
 * Práctica 9 - Orden topológico en digrafos (sin clonar el grafo)
 * Pepe Gallardo, Pablo López
 */

import dataStructures.graph.DiGraph;
import dataStructures.graph.DictionaryDiGraph;
import topSort.TopologicalSortingDic;
import topSort.TopologicalSortingDicPar;

public class TopSortDemo {
    public static void main(String[] args) {

        // choose graph
        // DiGraph<Integer> g = mkDiGraphG1();
         DiGraph<Integer> g = mkDiGraphG2();
        // DiGraph<Integer> g = mkDiGraphG3();

        System.out.println(g);

        TopologicalSortingDic<Integer> topSort = new TopologicalSortingDic<>(g);
        if (!topSort.hasCycle()) {
            System.out.println("A topological sorting: " + topSort.order());
        } else {
            System.out.println("DiGraph is cyclic ");
        }

        TopologicalSortingDicPar<Integer> topSortPar = new TopologicalSortingDicPar<>(g);
        if (!topSortPar.hasCycle()) {
            System.out.println("A topological sorting: " + topSortPar.order());
        } else {
            System.out.println("DiGraph is cyclic ");
        }
    }

    private static DiGraph<Integer> mkDiGraphG1() {
        DiGraph<Integer> g1 = new DictionaryDiGraph<>();

        g1.addVertex(2);
        g1.addVertex(3);
        g1.addVertex(5);
        g1.addVertex(7);
        g1.addVertex(8);
        g1.addVertex(9);
        g1.addVertex(10);
        g1.addVertex(11);

        g1.addDiEdge(7, 5);
        g1.addDiEdge(7, 8);
        g1.addDiEdge(5, 11);
        g1.addDiEdge(3, 8);
        g1.addDiEdge(3, 10);
        g1.addDiEdge(11, 2);
        g1.addDiEdge(11, 9);
        g1.addDiEdge(11, 10);
        g1.addDiEdge(8, 9);
        return g1;
    }

    private static DiGraph<Integer> mkDiGraphG2() {
        DiGraph<Integer> g2 = new DictionaryDiGraph<>();

        for (int i = 0; i < 13; i++) {
            g2.addVertex(i);
        }

        g2.addDiEdge(0, 1);
        g2.addDiEdge(0, 2);
        g2.addDiEdge(0, 3);
        g2.addDiEdge(0, 5);
        g2.addDiEdge(0, 6);
        g2.addDiEdge(2, 3);
        g2.addDiEdge(3, 4);
        g2.addDiEdge(3, 5);
        g2.addDiEdge(4, 9);
        g2.addDiEdge(6, 4);
        g2.addDiEdge(6, 9);
        g2.addDiEdge(7, 6);
        g2.addDiEdge(8, 7);
        g2.addDiEdge(9, 10);
        g2.addDiEdge(9, 11);
        g2.addDiEdge(9, 12);
        g2.addDiEdge(11, 12);
        return g2;
    }

    private static DiGraph<Integer> mkDiGraphG3() {
        DiGraph<Integer> g3 = new DictionaryDiGraph<>();

        for (int i = 0; i < 5; i++) {
            g3.addVertex(i);
        }

        g3.addDiEdge(0, 1);
        g3.addDiEdge(1, 2);
        g3.addDiEdge(2, 3);
        g3.addDiEdge(3, 4);
        g3.addDiEdge(4, 2);
        return g3;
    }
}
