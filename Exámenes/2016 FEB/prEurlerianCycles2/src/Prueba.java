import dataStructures.graph.DictionaryGraph;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.graph.EulerianCycle;

public class Prueba {
    public static void main(String[] args){
        List<Character> list1 = new LinkedList<>();
        List<Character> list2 = new LinkedList<>();
        list1.append('a');
        list1.append('c');
        list1.append('d');
        list1.append('a');
        list2.append('c');
        list2.append('b');
        list2.append('e');
        list2.append('c');
        EulerianCycle<Character> ec = new EulerianCycle<>(new DictionaryGraph<>());
        ec.connectCycles(list1, list2);
        for (Character c: list1){
            System.out.println(c);
        }

    }
}
