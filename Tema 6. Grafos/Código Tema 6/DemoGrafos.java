/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 6. Grafos
  Pablo López
*/

import dataStructures.graph.Graph;
import dataStructures.graph.DictionaryGraph;
import dataStructures.graph.Traversal;
import dataStructures.graph.DepthFirstTraversal;
import dataStructures.graph.BreadthFirstTraversal;
import dataStructures.graph.ConnectedComponents;
import dataStructures.set.Set;

public class DemoGrafos {

    public static void main(String[] args) {
        Graph<Integer> g = new DictionaryGraph<>();

        g.addVertex(1);
        g.addVertex(2);
        g.addVertex(3);
        g.addVertex(4);
        g.addVertex(5);
        g.addVertex(6);
        g.addVertex(7);

        g.addEdge(1, 2);
        g.addEdge(1, 5);
        g.addEdge(2, 5);
        g.addEdge(3, 1);
        g.addEdge(3, 2);
        g.addEdge(3, 6);
        g.addEdge(5, 3);
        g.addEdge(5, 4);
        g.addEdge(6, 7);

        // añadir componentes

/*
        g.addVertex(100);
        g.addVertex(101);
        g.addVertex(102);
        g.addVertex(103);

        g.addEdge(100, 101);
        g.addEdge(101, 103);
        g.addEdge(101, 102);

        g.addVertex(200);
        g.addVertex(201);
        g.addVertex(202);
        g.addVertex(203);

        g.addEdge(202, 201);
        g.addEdge(203, 202);
        g.addEdge(200, 202);

        g.addVertex(500);

*/

        System.out.println("number of vertices: " + g.numVertices());
        System.out.println("number of edges: " + g.numEdges());
        for (Integer v : g.vertices()) {
            System.out.printf("\nvertex: %d degree: %d successors: ", v, g.degree(v));
            for (Integer suc : g.successors(v)) {
                System.out.printf("%d ", suc);
            }
        }

        System.exit(0);

        System.out.println("\n\ntraversals:");

        final int INI = 1;

        Traversal<Integer> dfs = new DepthFirstTraversal<>(g, INI); // dft + dftPaths

        System.out.println("Depth first traversal from node " + INI + ":");
        for (Integer vertex : dfs.vertices())  // dft :: [a]
            System.out.println(vertex);

        System.out.println("Depth first traversal paths from node " + INI + ":");
        for (Iterable<Integer> path : dfs.paths())  // dftPaths :: [[a]]
            System.out.println(path);

        Traversal<Integer> bfs = new BreadthFirstTraversal<>(g, INI);

        System.out.println("Breadth first traversal from node " + INI + ":");
        for (Integer vertex : bfs.vertices())
            System.out.println(vertex);

        System.out.println("Breadth first traversal paths from node " + INI + ":");
        for (Iterable<Integer> path : bfs.paths())
            System.out.println(path);

        System.exit(0);

        System.out.println("Connected Components:");

        ConnectedComponents<Integer> components = new ConnectedComponents<>(g);

        for (Set<Integer> c : components.components()) {
            System.out.println(c);
        }
    }
}
