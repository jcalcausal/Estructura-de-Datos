/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 4. Árboles
  Pablo López
*/

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.tuple.Tuple2;

public class DictClient {

    public static void main(String[] args) {
        Dictionary<Integer, String> d = new AVLDictionary<>();
        d.insert(1, "one");
        d.insert(2, "two");
        d.insert(3, "three");
        d.insert(4, "four");
        d.insert(5, "five");

        System.out.print("keys= ");
        for (Integer k : d.keys()) {
            System.out.print(k + " ");
        }
        System.out.println();

        System.out.print("values= ");
        for (String v : d.values()) {
            System.out.print(v + " ");
        }
        System.out.println();

        System.out.print("keys and values= ");
        for (Tuple2<Integer, String> t : d.keysValues()) {
            System.out.printf("(%s, %s) ", t._1(), t._2());
        }
        System.out.println();
    }
}
