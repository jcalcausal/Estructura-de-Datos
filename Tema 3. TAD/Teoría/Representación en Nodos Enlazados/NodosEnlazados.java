/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 3. TAD Lineales
  Pablo López
*/

public class NodosEnlazados {

    static class Node<E> {  // semejante al constructor 'Node Int Seq' de Haskell
        E elem;        // el valor almacenado
        Node<E> next;  // el siguiente en la secuencia (recursividad)

        public Node(E x, Node<E> nextNode) {
            elem = x;
            next = nextNode;
        }
    }

    public static void main(String[] args) {
        /* el valor Haskell:

              Node 7 (Node 3 (Node 5 (Node 9 Empty)))

           se puede representar en Java de la siguiente manera
           (observa que usamos 'null' en lugar de 'Empty')
         */

        Node<Integer> first = new Node<>(7, new Node<>(3, new Node<>(5, new Node<>(9,null))));

        /* el siguiente bucle recorre la secuencia y escribe sus valores */

        Node<Integer> currentNode = first;
        while (currentNode != null) {
            System.out.println(currentNode.elem);
            currentNode = currentNode.next;
        }
    }
}
