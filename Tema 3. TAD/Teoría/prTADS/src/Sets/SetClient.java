/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 3. TAD Lineales
  Pablo López
*/
package Sets;
import dataStructures.set.Set;
import dataStructures.set.SortedLinkedSet;

import java.util.Iterator;
import java.util.List;

public class SetClient {

    public static void main(String[] args) {
        Set<Integer> s = new SortedLinkedSet<>();

        List<Integer> toInsert = List.of(2, 4, 6, 8, 7, 5, 3, 1);
        System.out.println("to insert: " + toInsert);
        for (int x : toInsert) {
            s.insert(x);
        }
        System.out.println("size = " + s.size() + ", contents: " + s);

        for (int i = 0; i < 10; i++) {
            System.out.print("isElem(" + i + "): " + s.isElem(i) + ", ");
        }
        System.out.println();

        List<Integer> toDelete = List.of(5, 1, 8, -10, 10, 5, 4, 7, 2);
        System.out.println("to delete:" + toDelete);
        Iterator<Integer> iter = toDelete.iterator();
        while (iter.hasNext()) {
            s.delete(iter.next());
        }
        System.out.println("size = " + s.size() + ", contents: " + s);

        // Ejercicio
        System.out.println("sum = " + sum(s));
    }

    // Ejercicio: define un método estático que devuelva la suma de los
    // elementos de un Set<Integer>
    public static Integer sum(Set<Integer> set) {
        Integer value = 0;
        Iterator<Integer> it = set.iterator();

        while (it.hasNext()){
            value += it.next();
        }
        return value;
    }
}
