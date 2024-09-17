/** ------------------------------------------------------------------------------
  * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
  * Diámetro de un grafo conexo
  * (completa y sustituye los siguientes datos)
  * Titulación: Doble Grado en Matemáticas e Ingeniería Informática
  * Alumno: Ángel Manuel Soria Gil
  * -------------------------------------------------------------------------------
  */

import dataStructures.graph.BreadthFirstTraversal;
import dataStructures.graph.Graph;
import dataStructures.list.ArrayList;

import java.util.Iterator;


public class GraphUtil {

	/**
	 * LENGTH: Calcula el número de elementos que contiene un iterable
	 * 
	 * @param it  El iterador
	 * @return   Número de elementos en el iterador
	 */
	public static <T> int length(Iterable<T> it) {
		int res = 0;
		for (T elem : it){
			res++;
		}
		return res;
	}

	/**
	 * ECCENTRICITY: Calcula la excentricidad de un vértice en un grafo El algoritmo toma la
	 * longitud del camino máximo en un recorrido en profundidad del grafo
	 * comenzando en el vértice dado.
	 * 
	 * @param graph    Grafo
	 * @param v        Vértice del grafo
	 * @return         Excentricidad del vértice
	 */
	public static <T> int eccentricity(Graph<T> graph, T v) {
		int res = 0;
		BreadthFirstTraversal<T> bft = new BreadthFirstTraversal<>(graph, v);
		for (Iterable<T> path : bft.paths()){
			if (length(path)>=res){
				res = length(path);
			}
		}
		return res-1;
	}

	/**
	 * DIAMETER: Se define como la máxima excentricidad de los vértices del grafo.
	 * 
	 * @param graph
	 * @return
	 */

	public static <T> int diameter(Graph<T> graph) {
		dataStructures.list.List <Integer> eccentricities = new ArrayList<>();
		for (T vertex : graph.vertices()){
			eccentricities.append(eccentricity(graph, vertex));
		}
		int res = 0;
		for (Integer i : eccentricities){
			if (i>=res){
				res = i;
			}
		}
	    return res;
	}
	
	/** 
	 * Estima y justifica la complejidad del método diameter
	 */
	//Comezamos directamente con un bucle que recorre todos los vértices del grafo, luego ya llevamos #V mínimo
	//Después llamamos a eccentricity dentro del bucle. Analicemos la complejidad de esta.
	//En eccentricity creamos un bft que tiene complejidad menor a #V, pero luego realizamos un recorrido por los caminos
	//y por un mismo camino, luego como en un camino pueden estar todos los vértices, al final también tiene complejidad
	// del orden #V^2
	//Por último recorremos eccentricities que tendrá longitud #V, pero ya lo hacemos fuera del otro bucle
	//De este modo concluimos que aproximadamente tendrá orden V^3+V que es aproximadamente también V^3
}
