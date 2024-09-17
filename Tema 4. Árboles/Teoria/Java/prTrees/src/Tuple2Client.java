/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 4. Árboles
  Pablo López
*/

import dataStructures.tuple.Tuple2;

public class Tuple2Client {
    public static void main(String[] args) {
        Tuple2<Integer, String> tuple = new Tuple2<>(5, "cinco");  // constructor
        System.out.println("primera componente: " + tuple._1());        // acceso a primera componente
        System.out.println("segunda componente: " + tuple._2());        // acceso a segunda componente
        System.out.println(tuple);                                      // toString
    }
}
