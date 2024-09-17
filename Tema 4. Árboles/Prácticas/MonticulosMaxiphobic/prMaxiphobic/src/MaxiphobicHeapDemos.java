import dataStructures.heap.Heap;
import dataStructures.heap.MaxiphobicHeapEfficient;
import java.io.FileNotFoundException;
import java.io.PrintWriter;

public class MaxiphobicHeapDemos {

    public static void main(String[] args) {
        drawStepwise("amasa");
        drawStepwise("murcielago");
    }

    private static void drawStepwise(String str) {
        // Heap<Character> h = new MaxiphobicHeap<>();
        Heap<Character> h = new MaxiphobicHeapEfficient<>();
        System.out.printf("drawing heap for \"%s\" using %s%n", str, h.getClass().getSimpleName());
        for (int i = 0; i < str.length(); i++) {
            h.insert(str.charAt(i));
            saveHeapToDot("Seq-" + str.substring(0, i + 1), h);
        }
    }

    private static <T extends Comparable<? super T>> void saveHeapToDot(String fileName, Heap<T> heap) {
        try (PrintWriter pw = new PrintWriter("dot/" + fileName + ".dot")) {
            pw.println(heap.toDot(fileName));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
