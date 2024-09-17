/**----------------------------------------------
 * -- Estructuras de Datos.  2018/19
 * -- 2º Curso del Grado en Ingeniería [Informática | del Software | de Computadores].
 * -- Escuela Técnica Superior de Ingeniería en Informática. UMA
 * --
 * -- Examen 4 de febrero de 2019
 * --
 * -- ALUMNO/NAME: Ángel Manuel Soria Gil
 * -- GRADO/STUDIES: Doble Grado Matemáticas e Ingeniería Informática
 * -- NÚM. MÁQUINA/MACHINE NUMBER:
 * --
 * ----------------------------------------------
 */

import dataStructures.graph.WeightedGraph;
import dataStructures.graph.WeightedGraph.WeightedEdge;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.priorityQueue.LinkedPriorityQueue;
import dataStructures.set.Set;
import dataStructures.set.HashSet;

public class Kruskal {
	public static <V,W> Set<WeightedEdge<V,W>> kruskal(WeightedGraph<V,W> g) {
		//inicializamos diccionario
		Dictionary<V, V> dict = new HashDictionary<>();
		for (V v : g.vertices()){
			dict.insert(v,v);
		}
		//inicializamos cola
		PriorityQueue<WeightedEdge<V,W>> pq = new LinkedPriorityQueue<>();
		for (WeightedEdge<V,W> edge : g.edges()){
			pq.enqueue(edge);
		}
		//inicializamos solución
		Set<WeightedEdge<V,W>> res = new HashSet<>();
		while (!pq.isEmpty()){
			WeightedEdge<V,W> edge = pq.first();
			V org = edge.source();
			V dst = edge.destination();
			if (!representante(org, dict).equals(representante(dst, dict))){
				dict.insert(representante(dst, dict), org);
				res.insert(edge);
			}
			pq.dequeue();
		}
		return res;
	}

	private static <V,W> V representante (V vertex, Dictionary<V,V> d){
		V res;
		if (d.valueOf(vertex).equals(vertex)){
			res = vertex;
		} else {
			res = representante(d.valueOf(vertex), d);
		}
		return res;
	}

	// Sólo para evaluación continua / only for part time students
	public static <V,W> Set<Set<WeightedEdge<V,W>>> kruskals(WeightedGraph<V,W> g) {

		// COMPLETAR
		
		return null;
	}
}
