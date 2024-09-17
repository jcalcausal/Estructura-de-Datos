/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 3. TAD Lineales
  Pablo López
*/

package Stacks;

import dataStructures.stack.LinkedStack;
import dataStructures.stack.Stack;

public class StackClient {

    public static void main(String[] args) {
        Stack<Integer> s = new LinkedStack<>();

        s.push(5);
        s.push(1);
        s.push(3);

        System.out.println(s);

        while (!s.isEmpty()) {
            System.out.println(s.top());
            s.pop();
        }
    }
}
