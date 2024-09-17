/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 3. TAD Lineales
  Pablo López
*/

package Stacks;

import dataStructures.stack.ArrayStack;
import dataStructures.stack.Stack;

public class StackSize {

    public static void main(String[] args) {
        // interfaz          implementación
        Stack<Integer> s = new ArrayStack<>();

        for (int i = 1; i <= 10; i++) {
            s.push(i);
        }

        System.out.println(s);
        System.out.println(size(s));
        // System.out.println(depth(s));
        System.out.println(s);
    }

    private static int size(Stack<Integer> s) {
        int result = 0;
        while (!s.isEmpty()) {
            result++;
            s.pop();
        }
        return result;
    }

    private static int depth(Stack<Integer> s) {
        int result = 0;
        Stack<Integer> aux = new ArrayStack<>();
        while (!s.isEmpty()) {
            result++;
            aux.push(s.top());
            s.pop();  // s es mutable
        }
        // s está vacía
        while (!aux.isEmpty()) {
            s.push(aux.top());
            aux.pop();
        }
        return result;
    }
}
