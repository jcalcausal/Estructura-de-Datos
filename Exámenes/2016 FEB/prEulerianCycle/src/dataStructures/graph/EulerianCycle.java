/**
 * Student's name: Ángel Manuel Soria Gil
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. Informática. UMA.
 */

package dataStructures.graph;
import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.set.*;
import dataStructures.list.*;

public class EulerianCycle<V> {
    private List<V> eCycle;

    @SuppressWarnings("unchecked")
    public EulerianCycle(Graph<V> g) {
        Graph<V> graph = (Graph<V>) g.clone();
        eCycle = eulerianCycle(graph);
    }

    public boolean isEulerian() {
        return eCycle != null;
    }

    public List<V> eulerianCycle() {
        return eCycle;
    }

    // J.1
    private static <V> boolean isEulerian(Graph<V> g) {
        Boolean res = true;
        for (V v : g.vertices()){
            if (g.degree(v)%2!=0){
                res = false;
            }
        }
        return res;
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {
        g.deleteEdge(v, u);
        if (g.degree(v)==0){
            g.deleteVertex(v);
        }
        if (g.degree(u)==0){
            g.deleteVertex(u);
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
        List<V> res = new LinkedList<>();
        res.append(v0);
        V actual = v0;
        V next = g.successors(v0).iterator().next();
        while (!next.equals(actual)){
            res.append(next);
            remove(g, actual, next);
            actual = next;
            next = g.successors(actual).iterator().next();
        }
        return res;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {
        if (xs.isEmpty()){
            xs = ys;
        } else {
            List <V> res = new LinkedList<>();
            int i = 0;
            V elem = ys.get(i);
            while (!xs.get(i).equals(elem)){
                res.append(xs.get(i));
                i++;
            }
            for (V vertex: ys){
                res.append(vertex);
            }
            for (i=i+1; i<xs.size(); i++){
                res.append(xs.get(i));
            }
            xs = res;
        }

    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
        V res = null;
        Set<V> vertex = g.vertices();
        for (V v : cycle){
            while (vertex.iterator().hasNext()){
                if (vertex.iterator().next().equals(v)){
                    res = v;
                    break;
                }
            }
            break;
        }
        return res;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        List<V> res = new LinkedList<>();
        if (isEulerian(g)){
            V v = g.vertices().iterator().next();
            res = extractCycle(g,v);
            while (!g.isEmpty()){
                V u = vertexInCommon(g, res);
                List<V> c2 = extractCycle(g, u);
                connectCycles(res, c2);
            }
        } else {
            //throw new GraphException("error: applying eulerianCycle on not eulerian graph");
            res = null;
        }
        return res;
    }
}
