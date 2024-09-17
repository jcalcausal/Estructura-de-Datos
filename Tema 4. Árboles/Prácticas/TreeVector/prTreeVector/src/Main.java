import com.sun.source.tree.Tree;
import dataStructures.list.ArrayList;
import dataStructures.vector.TreeVector;
import dataStructures.list.List;
public class Main {
    public static void main(String[] args) {
        List<Integer> l1 = new ArrayList<>();
        List<Integer> l2 = new ArrayList<>();
        for (int i = 0; i<5; i++){
            l1.append(i);
            l2.append((2*i));
        }
        System.out.println(l1);
        System.out.println(l2);
        //System.out.println(TreeVector.intercalate(l1, l2));
    }
}