
/**
 * Huffman trees and codes.
 *
 * Data Structures, Grado en Informatica. UMA.
 *
 *
 * Student's name:Ángel Manuel Soria Gil
 * Student's group:Doble Grado Matemáticas e Ingeniería informática
 */

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.priorityQueue.BinaryHeapPriorityQueue;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.tuple.Tuple2;

public class Huffman {

    // Exercise 1 (No es el algoritmo más eficiente pero lo más sencillo)
    public static Dictionary<Character, Integer> weights(String s) {
        Dictionary<Character, Integer> dict = new AVLDictionary<>();
    	for (Character c : s.toCharArray()){
            int cont = 0;
            for (int i = 0; i<s.length(); i++){
                if (c.equals(s.charAt(i))){
                    cont++;
                }
            }
            dict.insert(c,cont);
        }
        return dict;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
    	PriorityQueue<WLeafTree<Character>> res = new BinaryHeapPriorityQueue<>();
        Dictionary <Character, Integer> w = weights(s);
        for (Character c: w.keys()){
            WLeafTree<Character> leaf = new WLeafTree<>(c, w.valueOf(c));
            res.enqueue(leaf);
        }
        return res;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
    	WLeafTree<Character> res = null;
        PriorityQueue<WLeafTree<Character>> queue = huffmanLeaves(s);
        int n = weights(s).size();
        if (n<2){
            throw new HuffmanException("error: applied huffmanTree on String with less than 2 different values");
        } else {
            while (n!=1){
                WLeafTree<Character> t1 = queue.first();
                queue.dequeue();
                WLeafTree<Character> t2 = queue.first();
                queue.dequeue();
                res = new WLeafTree<>(t1, t2);
                queue.enqueue(res);
                n--;
            }
        }
    	return res;
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
        List<Integer> res = new LinkedList<>();
        List<Integer> list;
        for (Character c: s.toCharArray()){
            list = hc.valueOf(c);
            for (Integer i : list){
                res.append(i);
            }
        }
    	return res;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        String s = "";
        WLeafTree<Character> aux = ht;
        for (Integer i : bits){
            if (aux.isLeaf()){
                s = s + aux.elem();
                aux = ht;
            }
            if (i==0){
                aux = ht.leftChild();
            } else {
                aux = ht.rightChild();
            }
        }
    	return s;
    }
}
