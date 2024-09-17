/**
 * Estructuras de Datos
 * Práctica 9 - Orden topológico en digrafos (sin clonar el grafo)
 * Ángel Manuel Soria Gil
 */

package topSort;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.graph.DiGraph;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;

public class TopologicalSortingDicPar<V> {

    private final List<Set<V>> topSort = new LinkedList<>();
    private boolean hasCycle;

    public TopologicalSortingDicPar(DiGraph<V> graph) {
        hasCycle = false;
        Dictionary <V, Integer> dic = initialDictionary(graph);
        while (!hasCycle && !dic.isEmpty()){
            Set <V> sources = selectSources(dic);
            if (sources.isEmpty()){
                hasCycle = true;
            } else {
                dic = removeAList (dic, sources);
                topSort.append(sources);
                decrementSources(dic, sources, graph);
            }
        }
    }

    public boolean hasCycle() {
        return hasCycle;
    }

    public List<Set<V>> order() {
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

    private Set<V> selectSources(Dictionary<V, Integer> dic) {
        Set<V> sources = new HashSet<>();
        for (V vertex : dic.keys()){
            if (dic.valueOf(vertex)==0){
                sources.insert(vertex);
            }
        }
        return sources;
    }

    private Dictionary<V, Integer> removeAList(Dictionary<V, Integer> dic, Set<V> sources) {
        for (V vertex : sources){
            dic.delete(vertex);
        }
        return dic;
    }

    private void decrementSources(Dictionary<V, Integer> dic, Set<V> sources, DiGraph<V> graph) {
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
