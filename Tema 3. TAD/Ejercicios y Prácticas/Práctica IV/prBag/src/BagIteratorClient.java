/**
 * A simple test driver for the Bag ADT.
 * <p>
 * Tests for iterator and mostFrequent.
 */

import dataStructures.bag.Bag;
import dataStructures.bag.SortedArrayBag;
import dataStructures.bag.SortedLinkedBag;

import java.util.Iterator;

public class BagIteratorClient {

    public static void main(String[] args) {

        // choose implementation to test

        Bag<Character> empty = new SortedLinkedBag<>();
        Bag<Character> singleton = new SortedLinkedBag<>();
        Bag<Character> bag = new SortedLinkedBag<>();

//        Bag<Character> empty = new SortedArrayBag<>();
//        Bag<Character> singleton = new SortedArrayBag<>();
//        Bag<Character> bag = new SortedArrayBag<>();

        System.out.println("tests for class " + bag.getClass().getSimpleName());

        singleton.insert('S');

        String text = "En un lugar de la Mancha";
        for (int i = 0; i < text.length(); i++) {
            if (Character.isAlphabetic(text.charAt(i))) {
                bag.insert(text.charAt(i));
            }
        }

        // test bag iterator

        System.out.println("\n*** Testing the Bag iterator");
        System.out.print("contents of the empty bag:");
        for (char c : empty) {
            System.out.print(c);
        }
        System.out.println();

        System.out.print("contents of the singleton bag: ");
        for (char c : singleton) {
            System.out.print(c);
        }
        System.out.println();

        System.out.print("contents of the sample bag: ");
        for (char c : bag) {
            System.out.print(c);
        }
        System.out.println();

        // test mostFrequent

        System.out.println("\n*** Testing mostFrequent()");
        System.out.println("empty = " + empty);
        System.out.println("most frequent char = " + mostFrequent(empty));

        System.out.println("singleton = " + singleton);
        System.out.println("most frequent char = " + mostFrequent(singleton));

        System.out.println("original = " + bag);
        System.out.println("most frequent char = " + mostFrequent(bag));

        bag.insert('n');
        System.out.println("original + 'n' = " + bag);
        System.out.println("most frequent char = " + mostFrequent(bag));
    }

    /**
     * Returns the most frequent element in the {@code bag}. If there are several
     * elements with the same number of occurrences, returns the maximum of them. If
     * the {@code bag} is empty returns {@code null} (which is usually a bad idea).
     *
     * @param bag the bag to scan for the most frequent element
     * @return the most frequent element in {@code bag}
     */
    public static <T extends Comparable<? super T>> T mostFrequent(Bag<T> bag) {
        if (bag.isEmpty()){
            return null;
        } else {
            Iterator<T> it = bag.iterator();
            T res = it.next();
            T pos;
            int conR = bag.occurrences(res);
            int conP;
            while (it.hasNext()){
                pos = it.next();
                conP = bag.occurrences(pos);
                if (conP>conR || (conP==conR && pos.compareTo(res)>0)){
                    res = pos;
                    conR = conP;
                }
            }
            return res;
        }
    }
}
