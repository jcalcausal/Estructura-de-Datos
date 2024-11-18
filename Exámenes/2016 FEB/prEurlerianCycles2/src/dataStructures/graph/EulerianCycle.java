/**
 * Student's name: Ángel Manuel Soria Gil
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. Grado en Informática. UMA.
 */

package dataStructures.graph;

import dataStructures.list.*;

import java.util.Iterator;

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
        boolean res = true;
        for (V v : g.vertices()){
            res = res && g.degree(v)%2==0;
        }
        return res;
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {
        g.deleteEdge(v, u);
        if (g.degree(v)<1){
            g.deleteVertex(v);
        }
        if (g.degree(u)<1){
            g.deleteVertex(u);
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
        List<V> res = new LinkedList<>();
        Iterator<V> it = g.successors(v0).iterator();
        V v = v0;
        V u = it.next();
        res.append(u);
        remove(g, v, u);
        while (!u.equals(v0)){
            it = g.successors(u).iterator();
            v = u;
            u = it.next();
            res.append(u);
            remove(g, v, u);
        }
        return res;
    }

    // J.4
    public static <V> void connectCycles(List<V> xs, List<V> ys) {
        if(xs.isEmpty()){
            xs = ys;
        }else{
            List<V> lista = new ArrayList<>();
            Iterator<V> it = xs.iterator();
            while(it.hasNext()){
                V v = it.next();
                lista.append(v);
                if(v==ys.get(0)){
                    for(V y : ys){
                        lista.append(y);
                    }
                }
            }
            xs = lista;
        }
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
        for (V v : cycle){
            if (g.vertices().isElem(v)){
                return v;
            }
        }
        return null;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        if (!isEulerian(g)) {
            return null;
        }
        if (g.isEmpty()){
            return new LinkedList<>();
        }
        List<V> res;
        Iterator<V> it = g.vertices().iterator();
        V v = it.next();
        res = extractCycle(g, v);
        while (!g.isEmpty()){
            V v1 = vertexInCommon(g, res);
            List<V> cycle = extractCycle(g, v1);
            connectCycles(res, cycle);
        }
        return res;
    }
}
