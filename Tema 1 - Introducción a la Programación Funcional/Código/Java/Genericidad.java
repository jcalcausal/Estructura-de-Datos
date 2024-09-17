/*
Estructuras de Datos
Grado en Ingeniería Informática, del Software y de Computadores
Tema 1. Introducción a la Programación Funcional
Pablo López
*/

public class Genericidad {

    // Estas funciones solo se distinguen por el tipo
    public static int primeroI(int x, int y) {
        return x;
    }

    public static char primeroC(char x, char y) {
        return x;
    }

    /**
     * <p>Podemos abstraer el tipo de las funciones y escribir una sola función.</p>
     * <p>
     * En Java: {@code <T>}
     * <p>
     * genericidad - {@code T} es un parámetro genérico que representa cualquier tipo
     * </p>
     * <p>
     * En Haskell: {@code a}
     * <p>
     * polimorfismo - {@code a} es una variable de tipo que representa cualquier tipo
     * </p>
     */

    public static <T> T primero(T x, T y) {
        return x;
    }  // En Haskell: primero :: a -> a -> a

    public static void main(String[] args) {
        System.out.println(primeroI(3, 5));
        System.out.println(primeroC('t', 'x'));
        System.out.println(primero(3, 5));
        System.out.println(primero('t', 'x'));
        System.out.println(primero(true, false));
    }
}
