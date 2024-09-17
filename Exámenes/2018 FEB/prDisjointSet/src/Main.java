import dataStructures.list.ArrayList;
import dataStructures.list.List;
import dataStructures.set.DisjointSet;
import dataStructures.set.DisjointSetDictionary;

public class Main {
    public static void main(String[] args) {
        DisjointSet<String> dj = new DisjointSetDictionary<>();
        String[] pals = { "hola", "a", "todos", "como", "estais", "por", "aqui", "bien" };
        for (String s : pals) {
            dj.add(s);
        }
        System.out.println(dj);
        System.out.println(dj.numElements());
        dj.union("hola", "a");
        System.out.println(dj);
        dj.union("como", "estais");
        System.out.println(dj);
        dj.union("por", "aqui");
        System.out.println(dj);
        dj.union("hola", "bien");
        System.out.println(dj);
        dj.union("estais", "por");
        System.out.println(dj);
        System.out.println(dj.kind("bien"));
        System.out.println(dj.areConnected("hola", "por"));
        System.out.println(dj.areConnected("hola", "adios"));
        System.out.println(dj.areConnected("no", "esta"));

        // Solo alumnos sin evaluacion continua.
        // =====================================
        // Quitar comentarios a las lineas siguientes
        // para probar flatten() y kinds()

        dj.flatten();
        System.out.println(dj);
        System.out.println(dj.kinds());

        // Esto es una prueba mía para comprobar si mi método appearences funciona
        // parece que no muy bien, que hay que hacer cositas, el resto funciona todo bien
        System.out.println("miprueba....................");
        List<String> list1 = new ArrayList<>();
        list1.append("hola");
        list1.append("miau");
        list1.append("es");
        list1.append("hola");
        List<String> list2 = new ArrayList<>();
        list1.append("hola");
        list1.append("miau");
        list1.append("es");
        List<List<String>> list = new ArrayList<>();
        list.append(list2);
        list.append(list2);
        list.append(list1);
        System.out.println(appearences(list2, list));

    }
    public static int appearences (List<String> elem, List<List<String>> list) {
        int res = 0;
        for (List<String> x : list){
            if (elem.equals(x)){
                res++;
            }
        }
        return res;
    }
}
