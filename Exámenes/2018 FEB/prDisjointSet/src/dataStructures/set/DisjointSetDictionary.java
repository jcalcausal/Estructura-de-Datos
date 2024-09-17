/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de febrero de 2018.
 * Apellidos, Nombre: Ángel Manuel Soria Gil
 * Titulacion, Grupo: Doble Grado Matemáticas e Ingeniería Informática.
 */

package dataStructures.set;

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.ArrayList;
import dataStructures.list.LinkedList;
import dataStructures.list.List;

public class DisjointSetDictionary<T extends Comparable<? super T>> implements DisjointSet<T> {

    private Dictionary<T, T> dic;

    /**
     * Inicializa las estructuras necesarias.
     */
    public DisjointSetDictionary() {
        this.dic = new AVLDictionary<>();
    }

    /**
     * Devuelve {@code true} si el conjunto no contiene elementos.
     */
    @Override
    public boolean isEmpty() {
        return dic.isEmpty();
    }

    /**
     * Devuelve {@code true} si {@code elem} es un elemento del conjunto.
     */
    @Override
    public boolean isElem(T elem) {
        return dic.isDefinedAt(elem);
    }

    /**
     * Devuelve el numero total de elementos del conjunto.
     */

    @Override
    public int numElements() {
        return dic.size();
    }

    /**
     * Agrega {@code elem} al conjunto. Si {@code elem} no pertenece al
     * conjunto, crea una nueva clase de equivalencia con {@code elem}. Si
     * {@code elem} pertencece al conjunto no hace nada.
     */
    @Override
    public void add(T elem) {
        if (!dic.isDefinedAt(elem)){
            dic.insert(elem, elem);
        }
    }

    /**
     * Devuelve el elemento canonico (la raiz) de la clase de equivalencia la
     * que pertenece {@code elem}. Si {@code elem} no pertenece al conjunto
     * devuelve {@code null}.
     */
    private T root(T elem) {
        T res = null;
        if (dic.isDefinedAt(elem)){
            if (dic.valueOf(elem).equals(elem)){
                res = elem;
            } else {
                res = root(dic.valueOf(elem));
            }
        }
        return res;
    }

    /**
     * Devuelve {@code true} si {@code elem} es el elemento canonico (la raiz)
     * de la clase de equivalencia a la que pertenece.
     */
    private boolean isRoot(T elem) {
        return root(elem).equals(elem);
    }

    /**
     * Devuelve {@code true} si {@code elem1} y {@code elem2} estan en la misma
     * clase de equivalencia.
     */
    @Override
    public boolean areConnected(T elem1, T elem2) {
        if (!dic.isDefinedAt(elem1)|| !dic.isDefinedAt(elem2)){
            return false;
        }
        return root(elem1).equals(root(elem2));
    }

    /**
     * Devuelve una lista con los elementos pertenecientes a la clase de
     * equivalencia en la que esta {@code elem}. Si {@code elem} no pertenece al
     * conjunto devuelve la lista vacia.
     */
    @Override
    public List<T> kind(T elem) {
        List<T> res = new LinkedList<>();
        for (T x : dic.keys()){
            if (areConnected(elem, x)){
                res.append(x);
            }
        }
        return res;
    }

    /**
     * Une las clases de equivalencias de {@code elem1} y {@code elem2}. Si
     * alguno de los dos argumentos no esta en el conjunto lanzara una excepcion
     * {@code IllegalArgumenException}.
     */
    @Override
    public void union(T elem1, T elem2) {
        if (!dic.isDefinedAt(elem1) || !dic.isDefinedAt(elem2)){
            throw new IllegalArgumentException("error: you shouldn't have used this here");
        } else {
            if (root(elem1).compareTo(root(elem2))>0){
                dic.insert(root(elem1), root(elem2));
            } else {
                dic.insert(root(elem2), root(elem1));
            }
        }
    }

    // ====================================================
    // A partir de aqui solo para alumnos a tiempo parcial
    // que no sigan el proceso de evaluacion continua.
    // ====================================================

    /**
     * Aplana la estructura de manera que todos los elementos se asocien
     * directamente con su representante canonico.
     */
    @Override
    public void flatten() {
        for (T key : dic.keys()){
            dic.insert(key, root(key));
        }
    }

    /**
     * Devuelve una lista que contiene las clases de equivalencia del conjunto
     * como listas.
     */
    @Override
    public List<List<T>> kinds() {
        List<List<T>> res = new LinkedList<>();
        flatten();
        for (T key : dic.keys()){
            if (isRoot(key)){
                res.append(kind(key));
            }
        }
        return res;
    }

    private int appearences (List<T> elem, List<List<T>> list) {
        int res = 0;
        for (List<T> x : list){
            if (elem.equals(x)){
                res++;
            }
        }
        return res;
    }

    /**
     * Devuelve una representacion del conjunto como una {@code String}.
     */
    @Override
    public String toString() {
        return "DisjointSetDictionary(" + dic.toString() + ")";
    }
}
