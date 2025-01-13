
/**
 * Huffman trees and codes.
 *
 * Data Structures, Grado en Informatica. UMA.
 *
 *
 * Student's name:
 * Student's group:
 */

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.priorityQueue.BinaryHeapPriorityQueue;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.tuple.Tuple2;

public class Huffman {

    // Exercise 1
    public static Dictionary<Character, Integer> weights(String s) {
        Dictionary<Character, Integer> res = new AVLDictionary<>();
        for (int i = 0; i < s.length(); ++i){
            if (res.isDefinedAt(s.charAt(i))) {
                res.insert(s.charAt(i), res.valueOf(s.charAt(i)) + 1);
            } else {
                res.insert(s.charAt(i), 1);
            }
        }
        return res;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
    	PriorityQueue<WLeafTree<Character>> res = new BinaryHeapPriorityQueue<>();
        Dictionary<Character, Integer> w = weights(s);
        for (Character c: w.keys()) {
            res.enqueue(new WLeafTree(c, w.valueOf(c)));
        }
        return res;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
    	if (weights(s).size() < 2)
            throw new HuffmanException("Hay menos de dos caracteres distintos");
        PriorityQueue<WLeafTree<Character>> pq = huffmanLeaves(s);
        return huffmanTreeAux(pq.first(), huffmanLeaves(s));
    }

    public static WLeafTree<Character> huffmanTreeAux (WLeafTree<Character> first, PriorityQueue<WLeafTree<Character>> pq) {
        pq.dequeue();
        if (pq.isEmpty()) {
            return first;
        } else {
            return huffmanTreeAux(merge (first, pq.first()), pq);
        }
    }

    public static WLeafTree<Character> merge (WLeafTree<Character> left, WLeafTree<Character> right) {
        return new WLeafTree<>(left, right);
    }

    // Exercise 3.a
    public static Dictionary<Character, List<Integer>> joinDics(Dictionary<Character, List<Integer>> d1, Dictionary<Character, List<Integer>> d2) {
        Dictionary<Character, List<Integer>> res = new AVLDictionary<>();
        for (Tuple2<Character,List<Integer>> t : d1.keysValues()){
            res.insert(t._1(), t._2());
        }
        for (Tuple2<Character,List<Integer>> t : d2.keysValues()){
            res.insert(t._1(), t._2());
        }
        return res;
    }

    // Exercise 3.b
    public static Dictionary<Character, List<Integer>> prefixWith(int i, Dictionary<Character, List<Integer>> d) {
        Dictionary<Character, List<Integer>> res = new AVLDictionary<>();
        for (Tuple2<Character,List<Integer>> t : d.keysValues()){
            List<Integer> l2 = t._2();
            l2.insert(0, i);
            res.insert(t._1(),t._2());
        }
        return res;
    }

    // Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        Dictionary<Character, List<Integer>> res = new AVLDictionary<>();
        if (ht.isLeaf()){
            res.insert(ht.elem(), new LinkedList<>());
        } else {
            Dictionary<Character, List<Integer>> d1 = prefixWith(0, huffmanCode(ht.leftChild()));
            Dictionary<Character, List<Integer>> d2 = prefixWith(1, huffmanCode(ht.rightChild()));
            res = joinDics(d1, d2);
        }
        return res;
    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        //to do 
    	return null;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        //to do 
    	return null;
    }
}
