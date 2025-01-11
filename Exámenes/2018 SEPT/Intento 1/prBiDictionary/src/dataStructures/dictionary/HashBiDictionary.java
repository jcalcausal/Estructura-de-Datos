package dataStructures.dictionary;
import dataStructures.list.List;

import dataStructures.list.ArrayList;
import dataStructures.set.AVLSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de septiembre de 2018.
 *
 * Apellidos, Nombre:
 * Titulacion, Grupo:
 */
public class HashBiDictionary<K,V> implements BiDictionary<K,V>{
	private Dictionary<K,V> bKeys;
	private Dictionary<V,K> bValues;
	
	public HashBiDictionary() {
		bKeys = new HashDictionary<>();
		bValues = new HashDictionary<>();
	}
	
	public boolean isEmpty() {
		return bKeys.isEmpty() && bValues.isEmpty();
	}
	
	public int size() {
		return bKeys.size();
	}
	
	public void insert(K k, V v) {
		if (bKeys.isDefinedAt(k))
			bValues.delete(bKeys.valueOf(k));
		if (bValues.isDefinedAt(v))
			bKeys.delete(bValues.valueOf(v));
		bKeys.insert(k,v);
		bValues.insert(v,k);
	}
	
	public V valueOf(K k) {
		return bKeys.valueOf(k);
	}
	
	public K keyOf(V v) {
		return bValues.valueOf(v);
	}
	
	public boolean isDefinedKeyAt(K k) {
		return bKeys.isDefinedAt(k);
	}
	
	public boolean isDefinedValueAt(V v) {
		return bValues.isDefinedAt(v);
	}
	
	public void deleteByKey(K k) {
		if (bKeys.isDefinedAt(k)) {
			bKeys.delete(k);
			bValues.delete(valueOf(k));
		} else {
			throw new NoSuchElementException("deletebyKey");
		}
	}
	
	public void deleteByValue(V v) {
		if (bValues.isDefinedAt(v)) {
			bValues.delete(v);
			bKeys.delete(keyOf(v));
		} else {
			throw new NoSuchElementException("deletebyKey");
		}
	}
	
	public Iterable<K> keys() {
		return bKeys.keys();
	}
	
	public Iterable<V> values() {
		return bValues.keys();
	}
	
	public Iterable<Tuple2<K, V>> keysValues() {
		return bKeys.keysValues();
	}
	
		
	public static <K,V extends Comparable<? super V>> BiDictionary<K, V> toBiDictionary(Dictionary<K,V> dict) {
		BiDictionary<K, V> res = new HashBiDictionary<K, V>();
		for (Tuple2<K, V> t : dict.keysValues()) {
			if (res.isDefinedValueAt(t._2()))
				throw new IllegalArgumentException("El diccionario no es inyectivo");
			res.insert(t._1(), t._2());
		}
		return res;
	}
	
	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> bdic) {
		BiDictionary<K, W> res = new HashBiDictionary<>();
		for (Tuple2<K,V> t : bKeys.keysValues()) {
			if (bdic.isDefinedKeyAt(t._2()))
				res.insert(t._1(), bdic.valueOf(t._2()));
		}
		return res;
	}
		
	public static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> bd) {
		Set <K> set1 = new AVLSet<>();
		Set <K> set2 = new AVLSet<>();
		boolean isPermutation = true;

		for (Tuple2<K,K> t : bd.keysValues()) {
			set1.insert(t._1());
			set2.insert(t._2());
		}

		Iterator<K> it = set1.iterator();
		while (it.hasNext() && isPermutation) {
			if (!set2.isElem(it.next())) {
				isPermutation = false;
			}
		}
		return isPermutation;
	}
	
	// Solo alumnos con evaluación por examen final.
    // =====================================
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		// TODO
		return null;
	}
	
	public static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
		// TODO
		return null;
	}

    // =====================================
	
	
	@Override
	public String toString() {
		return "HashBiDictionary [bKeys=" + bKeys + ", bValues=" + bValues + "]";
	}
	
	
}
