// Estructuras de Datos
// Tema 1. Introducción a la Programación Funcional

public class GenericidadRestringida {

    /**
     * En Java podemos imponer una restricción sobre un tipo {@code T} de la siguiente manera:
     * <p>
     * {@code T extends Comparable<T>}
     * <p>
     * que indica que el tipo {@code T} debe implementar la interfaz {@code Comparable}.
     * <p>
     * Similarmente, en Haskell podemos imponer una restrcción sobre un tipo {@code a} con un contexto:
     * <p>
     * {@code Ord a =>}
     * <p>
     * que indica que el tipo {@code a} debe ser instancia de la clase {@code Ord}.
     */

    public static <T extends Comparable<T>> T maximo(T x, T y) { // En Haskell: maximo :: Ord a => a -> a -> a
        T result;
        if (x.compareTo(y) >= 0)
            result = x;
        else
            result = y;
        return result;
    }

    public static void main(String[] args) {
        System.out.println(maximo(5, 8));  // T = Integer
        System.out.println(maximo('r', 'f')); // T = Character
        System.out.println(maximo("Haskell", "Java")); // T = String
    }
}
