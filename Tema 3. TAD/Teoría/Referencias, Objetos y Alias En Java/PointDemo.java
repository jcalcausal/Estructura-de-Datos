/*
  Estructuras de Datos
  Grado en Ingeniería Informática, del Software y de Computadores
  Tema 3. TAD Lineales
  Pablo López
*/

// Para este ejemplo utilizamos las colecciones del API de Java

import java.util.ArrayList;
import java.util.List;

/**
 * Ejemplo simple de cliente de TAD en Java
 * <p>
 * Este programa ilustra los conceptos de referencia y objeto en Java.
 * También hace uso de alias (referencias que acceden al mismo objeto).
 * <p>
 * Asegúrate de que entiendes el programa y de que puedes predecir la
 * salida que genera.
 */

public class PointDemo {

    public static void main(String[] args) {

        Point x = new Point(0, 0);

        x.move(1, 2);

        ///// A
        System.out.println("*** A:");
        System.out.println("x= " + x);
        System.exit(0); // detener programa

        Point y = x; // alias

        y.move(5, 5);

        ///// B
        System.out.println("*** B:");
        System.out.println("x= " + x);
        System.out.println("y= " + y);
        System.exit(0); // detener programa

        x = null;

        ///// C
        System.out.println("*** C:");
        System.out.println("x= " + x);
        System.out.println("y= " + y);
        System.exit(0); // detener programa

        Point z = new Point(1, 3);
        List<Point> points = new ArrayList<>();
        points.add(x);
        points.add(y);
        points.add(z);

        ///// D
        System.out.println("*** D:");
        System.out.println("points= " + points);
        System.exit(0); // detener programa

        y.move(-3, -3);

        ///// E
        System.out.println("*** E:");
        System.out.println("y= " + y);
        System.out.println("points= " + points);
    }
}
