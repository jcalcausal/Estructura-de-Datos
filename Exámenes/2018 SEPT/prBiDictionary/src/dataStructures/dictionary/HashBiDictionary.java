package dataStructures.dictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;

import dataStructures.list.ArrayList;
import dataStructures.set.AVLSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

import java.util.Iterator;

/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de septiembre de 2018.
 *
 * Apellidos, Nombre: Soria Gil, Ángel Manuel
 * Titulacion, Grupo: Doble Grado Matemáticas e Ingeniería Informática
 */
public class HashBiDictionary<K,V> implements BiDictionary<K,V>{
	private Dictionary<K,V> bKeys;
	private Dictionary<V,K> bValues;
	
	public HashBiDictionary() {
		this.bKeys = new HashDictionary<>();
		this.bValues = new HashDictionary<>();
	}
	
	public boolean isEmpty() {
		return bKeys.isEmpty() && bValues.isEmpty();
	}
	
	public int size() {
		return bKeys.size();
	}
	
	public void insert(K k, V v) {
		if (bKeys.isDefinedAt(k)){
			bValues.delete(bKeys.valueOf(k));
		}
		if (bValues.isDefinedAt(v)){
			bKeys.delete(bValues.valueOf(v));
		}
		bKeys.insert(k,v);
		bValues.insert(v, k);
	}

	public V valueOf(K k) {
		V res = null;
		if (bKeys.isDefinedAt(k)){
			res = bKeys.valueOf(k);
		}
		return res;
	}
	
	public K keyOf(V v) {
		K res = null;
		if (bValues.isDefinedAt(v)){
			return bValues.valueOf(v);
		}
		return res;
	}
	
	public boolean isDefinedKeyAt(K k) {
		return bKeys.isDefinedAt(k);
	}
	
	public boolean isDefinedValueAt(V v) {
		return bValues.isDefinedAt(v);
	}
	
	public void deleteByKey(K k) {
		if (isDefinedKeyAt(k)){
			V val = bKeys.valueOf(k);
			bKeys.delete(k);
			bValues.delete(val);
		}
	}
	
	public void deleteByValue(V v) {
		if (isDefinedValueAt(v)){
			K key = bValues.valueOf(v);
			bValues.delete(v);
			bKeys.delete(key);
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
	
		
	public static <K,V extends Comparable<? super V>> BiDictionary<K, V> toBDictionary (Dictionary<K,V> dict) {
		BiDictionary<K,V> res = new HashBiDictionary<>();
		if (isInyective(dict)){
			for (Tuple2<K,V> tup : dict.keysValues()){
				res.insert(tup._1(), tup._2());
			}
		} else {
			throw new IllegalArgumentException("error: toBiDictionary applied on non inyective dictionary");
		}
		return res;
	}

	private static <K, V extends Comparable<? super V>> boolean isInyective(Dictionary<K,V> dict) {
		boolean res = true;
		for (V val : dict.values()){
			int cont = 0;
			for (V valAux : dict.values()){
				if (val.equals(valAux)){
					cont++;
				}
			}
			if (cont>1){
				res = res && false;
			}
		}
		return res;
	}


	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> bdic) {
		BiDictionary<K,W> res = new HashBiDictionary<>();
		List<Tuple2<K,W>> listAux = new LinkedList<>();
		for (K key : this.bKeys.keys()){
			if (bdic.isDefinedKeyAt(this.valueOf(key))){
				listAux.append(new Tuple2<>(key, bdic.valueOf(this.valueOf(key))));
			}
		}
		for (Tuple2<K,W> tup : listAux){
			res.insert(tup._1(), tup._2());
		}
		return res;
	}
		
	public static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> bd) {
		boolean res = false;
		Set <K> set1 = new AVLSet<>();
		Set <K> set2 = new AVLSet<>();
		for (K key : bd.keys()){
			set1.insert(key);
		}
		for (K val : bd.values()){
			set2.insert(val);
		}
		if(set1.size()==set2.size()){
			int cont = 0;
			for (K key : set1){
				if (set2.isElem(key)){
					cont ++;
				}
			}
			if (cont == set1.size()){
				res = true;
			} else {
				res = false;
			}
		} else {
			res = false;
		}
		return res;
	}
	
	// Solo alumnos con evaluación por examen final.
    // =====================================
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		if (!isPermutation(bd)){
			throw new IllegalArgumentException("error: orbitOf applied on non permutation bidictionaryh");
		}
		List<K> res = new LinkedList<>();
		K initK = k;
		do {
			res.append(k);
			k = bd.valueOf(k);
		} while (!k.equals(initK));
		return res;
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
