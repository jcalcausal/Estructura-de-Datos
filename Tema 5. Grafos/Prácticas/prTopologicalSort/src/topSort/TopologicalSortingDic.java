/**
 * Estructuras de Datos
 * Práctica 9 - Orden topológico en digrafos (sin clonar el grafo)
 * Ángel Manuel Soria Gil
 */

package topSort;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.graph.DiGraph;
import dataStructures.graph.Graph;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.queue.LinkedQueue;
import dataStructures.queue.Queue;

public class TopologicalSortingDic<V> {

    private final List<V> topSort = new LinkedList<>();
    private boolean hasCycle;

    public TopologicalSortingDic(DiGraph<V> graph) {
        hasCycle = false;
        Dictionary <V, Integer> dic = initialDictionary(graph);
        while (!hasCycle && !dic.isEmpty()){
            List <V> sources = selectSources(dic);
            if (sources.isEmpty()){
                hasCycle = true;
            } else {
                dic = removeAList (dic, sources);
                for (V vertex : sources){
                    topSort.append(vertex);
                }
                decrementSources(dic, sources, graph);
            }
        }
    }

    public boolean hasCycle() {
        return hasCycle;
    }

    public List<V> order() {
        return hasCycle ? null : topSort;
    }

    private Dictionary<V, Integer> initialDictionary(DiGraph<V> graph) {
        Dictionary <V,Integer> pendingPred = new HashDictionary<>();
        for (V vertex : graph.vertices()){
            int pending = graph.predecessors(vertex).size();
            pendingPred.insert(vertex, pending);
        }
        return pendingPred;
    }

    private List<V> selectSources(Dictionary<V, Integer> dic) {
        List<V> sources = new LinkedList<>();
        for (V vertex : dic.keys()){
            if (dic.valueOf(vertex)==0){
                sources.append(vertex);
            }
        }
        return sources;
    }

    private Dictionary<V, Integer> removeAList(Dictionary<V, Integer> dic, List<V> sources) {
        for (V vertex : sources){
            dic.delete(vertex);
        }
        return  dic;
    }

    private void decrementSources(Dictionary<V, Integer> dic, List<V> sources, DiGraph<V> graph) {
        for (V vertex : sources){
            for (V sucVertex : graph.successors(vertex)){
                if (dic.isDefinedAt(sucVertex)) {
                    Integer newPend = dic.valueOf(sucVertex) - 1;
                    dic.insert(sucVertex, newPend);
                }
            }
        }
    }
}
