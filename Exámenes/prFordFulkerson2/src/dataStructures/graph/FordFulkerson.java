/**
 * Student's name: Ángel Manuel Soria Gil
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 */

package dataStructures.graph;

import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

import java.util.Iterator;

public class FordFulkerson<V> {
    private WeightedDiGraph<V,Integer> g; // Initial graph 
    private List<WDiEdge<V,Integer>> sol; // List of edges representing maximal flow graph
    private V src; 			  // Source
    private V dst; 		  	  // Sink
	
    /**
     * Constructors and methods
     */

    public static <V> int maxFlowPath(List<WDiEdge<V,Integer>> path) {
        int res;
        if (path.isEmpty()){
            res = 0;
        } else {
            res = path.get(0).getWeight();
            for (WDiEdge<V,Integer> edge : path){
                if (edge.getWeight()<=res){
                    res = edge.getWeight();
                }
            }
        }
        return res;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdge(V x, V y, Integer p, List<WDiEdge<V,Integer>> edges) {
        boolean thereissomeone = false;
        for (int i = 0; i<edges.size(); i++){
            WDiEdge<V,Integer> edge = edges.get(i);
            if (edge.getSrc().equals(x) && edge.getDst().equals(y)){
                thereissomeone = true;
                if (edge.getWeight()+p == 0){
                    edges.remove(i);
                } else {
                    WDiEdge<V,Integer> nedge = new WDiEdge<>(x, (edge.getWeight())+p, y);
                    edges.insert(i, nedge);
                }
            }
        }
        if (!thereissomeone){
            edges.append(new WDiEdge<>(x, p, y));
        }
        return edges;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdges(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> edges) {
        if (path.isEmpty()) {
            return edges;
        }
        for (WDiEdge<V,Integer> edge : path){
            edges = updateEdge(edge.getSrc(), edge.getDst(), p, edges);
        }
        return edges;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlow(V x, V y, Integer p, List<WDiEdge<V,Integer>> sol) {
        boolean thereissomeone = false;
        for (int i = 0; i < sol.size(); i++){
            WDiEdge<V,Integer> edge = sol.get(i);
            if (edge.getSrc().equals(x) && edge.getDst().equals(y)){
                thereissomeone = true;
                Integer nw = edge.getWeight()+p;
                sol.insert(i,new WDiEdge<>(x, nw, y));
            } else if (edge.getSrc().equals(y) && edge.getDst().equals(x) && edge.getWeight()==p){
                thereissomeone = true;
                sol.remove(i);
            } else if (edge.getSrc().equals(y) && edge.getDst().equals(x) && edge.getWeight()<p){
                thereissomeone = true;
                Integer nw = p - edge.getWeight();
                sol.insert(i, new WDiEdge<>(x, nw, y));
            } else {
                if (edge.getSrc().equals(y) && edge.getDst().equals(x) && edge.getWeight()>p){
                    thereissomeone = true;
                    Integer nw = edge.getWeight()-p;
                    sol.insert(i, new WDiEdge<>(x, nw, y));
                }
            }
        }
        if (!thereissomeone){
            sol.append(new WDiEdge<>(x, p, y));
        }
        return sol;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlows(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> sol) {
        if (path.isEmpty()) {
            return sol;
        }
        for (WDiEdge<V,Integer> edge : path){
            sol = addFlow(edge.getSrc(), edge.getDst(), p, sol);
        }
        return sol;
    }

    public FordFulkerson(WeightedDiGraph<V,Integer> g, V src, V dst) {
        this.g = g;
        this.src = src;
        this.dst = dst;
        List<WDiEdge<V,Integer>> sol = new LinkedList<>();
        WeightedDiGraph<V,Integer> wdg = g;
        WeightedBreadthFirstTraversal<V, Integer> wdgt = new WeightedBreadthFirstTraversal<>(g, src);
        List<WDiEdge<V,Integer>> path = wdgt.pathTo(dst);
        while (!path.isEmpty()){
            Integer mf = maxFlowPath(path);
            List<WDiEdge<V,Integer>> edges = wdg.wDiEdges();
            edges = updateEdges(path, -mf, edges);
            edges = updateEdges (invert(path), mf, edges);
            wdg = new WeightedDictionaryDiGraph<>(wdg.vertices(), edges);
            sol = addFlows(path, mf, sol);
            wdgt = new WeightedBreadthFirstTraversal<>(wdg, src);
            path = wdgt.pathTo(dst);
        }
        this.sol = sol;
    }

    private List<WDiEdge<V, Integer>> invert(List<WDiEdge<V, Integer>> path) {
        List<WDiEdge<V,Integer>> res = new LinkedList<>();
        for (WDiEdge<V,Integer> edge : path){
            WDiEdge<V,Integer> nedge = new WDiEdge<>(edge.getDst(), edge.getWeight(), edge.getSrc());
            res.prepend(nedge);
        }
        return res;
    }


    public int maxFlow() {
        int res = 0;
        for (WDiEdge<V,Integer> edge : sol){
            if (edge.getSrc().equals(src)){
                res = res + edge.getWeight();
            }
        }
        return res;
    }

    public int maxFlowMinCut(Set<V> set) {
        int res = 0;
        Set<V> otherSet = buildDisjointSet(set);
        int suma1 = 0;
        int suma2 = 0;
        Iterator<V> it1 = set.iterator();
        Iterator<V> it2 = otherSet.iterator();
        while (it1.hasNext()){
            for (Tuple2<V,Integer> tup : g.successors(it1.next())){
                if (otherSet.isElem(tup._1())){
                    suma1 = suma1 + tup._2();
                }
            }
        }
        while (it2.hasNext()){
            for (Tuple2<V,Integer> tup : g.successors(it2.next())){
                if (set.isElem(tup._1())){
                    suma2 = suma2 + tup._2();
                }
            }
        }
        if (set.isElem(src)){
            res = suma1 - suma2;
        } else {
            res = suma2 - suma1;
        }
        return res;
    }

    private Set<V> buildDisjointSet(Set<V> set) {
        Set<V> res = new HashSet<>();
        for (V v : g.vertices()){
            if (!set.isElem(v)){
                res.insert(v);
            }
        }
        return res;
    }


    /**
     * Provided auxiliary methods
     */
    public List<WDiEdge<V, Integer>> getSolution() {
        return sol;
    }
	
    /**********************************************************************************
     * A partir de aquí SOLO para estudiantes a tiempo parcial sin evaluación continua.
     * ONLY for part time students.
     * ********************************************************************************/

    public static <V> boolean localEquilibrium(WeightedDiGraph<V,Integer> g, V src, V dst) {
        // TO DO
        return false;
    }
    public static <V,W> Tuple2<List<V>,List<V>> sourcesAndSinks(WeightedDiGraph<V,W> g) {
        // TO DO
        return null;
    }

    public static <V> void unifySourceAndSink(WeightedDiGraph<V,Integer> g, V newSrc, V newDst) {
        // TO DO
    }
}
