package dataStructures.bag;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringJoiner;

public class SortedArrayBag<T extends Comparable<? super T>> implements Bag<T> {

    private final static int INITIAL_CAPACITY = 1;

    /**
     * <strong>Representation invariant:</strong>
     * <p>
     * 1. {@code nextFree} refers to the first available position in the arrays {@code value} and {@code count}
     * <p>
     * 2. the slice {@code [0..nextFree)} of the array {@code value} must be sorted, with no duplicates
     * <p>
     * 3. the slice {@code [0..nextFree)} of the array {@code count} must store positive integers
     * <p>
     * 4. {@code size} is the sum of the integers in the slice {@code [0..nextFree)} of the array {@code count}
     * <p>
     * {@code value[i]} stores the <em>ith</em> item in the bag, while {@code count[i]} stores its number of occurrences
     * <p>
     * <strong>Example:</strong>
     * <pre>
     *    value -> {'a', 'd', 't', 'z'}
     *    count -> { 5 ,  1,   3,   2 }
     *    nextFree = 4
     *    size = 11
     * </pre>
     */
    private T[] value;
    private int[] count;
    private int nextFree;
    private int size;

    @SuppressWarnings("unchecked")
    public SortedArrayBag() {
        //Esta línea de abajo también da un error.
        value = (T[]) new Comparable[INITIAL_CAPACITY];
        count = new int[INITIAL_CAPACITY];
        nextFree = 0;
        size = 0;
    }

    private void ensureCapacity() {
        if (nextFree >= value.length){
            value = Arrays.copyOf(value, 2*value.length);
            count = Arrays.copyOf(count, 2*count.length);
        }
    }

    @Override
    public boolean isEmpty(){
        return size == 0;
    }

    @Override
    public int size() {
        return size;
    }

    /**
     * Localiza la posición donde está o debería estar un elemento.
     * <p>
     * Si {@code item} aparece en el array {@code value}, devuelve su índice;
     * en otro caso devuelve el índice donde {@code item} debería estar.
     *
     * @param item el elemento a localizar.
     * @return índice donde está o debería estar {@code item}
     */
    private int locate(T item) {
        int lower = 0;
        int upper = nextFree - 1;
        int mid = 0;
        boolean found = false;

        // Búsqueda binaria
        while (lower <= upper && !found) {
            mid = lower + ((upper - lower) / 2); // == (lower + upper) / 2;
            found = value[mid].equals(item);
            if (!found) {
                if (value[mid].compareTo(item) > 0) {
                    upper = mid - 1;
                } else {
                    lower = mid + 1;
                }
            }
        }

        if (found) {
            return mid; // el índice donde "item" está almacenado
        } else {
            return lower; // el índice donde "item" debería insertarse
        }
    }

    @Override
    public void insert(T item) {
        this.ensureCapacity();                  //Aseguramos capacidad de los arrays
        int pos = locate(item);                 //Identificamos donde deberíamos insertar el elemento
        if (pos==nextFree){                     //En caso de no estar aún en la mochila
            value[pos] = item;                      //Añadimos el elemento a lista de elementos en la mochila
            count[pos] = 1;                         //Ponemos su contador a uno, pues insertamos de uno en uno
            nextFree++;                             //Actualizamos la siguiente posición libre
        } else {                                //En caso de ya estar o de deber insertarse en un lugar ocupado debe ser pos < nextFree, pero pos>nextFree no se da
            if (value[pos]==item){                  //En caso de ya estar
                count[pos]++;                           //Actualizamos el contador de dicho elemento, el siguiente libre sigue igual
            } else {                                //En caso de no estar
                for (int i = nextFree; i>pos; i--){  //Para cada elemento que haya desde nuestra posición deseada en adelante,
                    value[i] = value[i-1];              //lo desplazamos a la siguiente, dejando así libre pos para insertar el nuevo valor
                    count[i] = count[i-1];
                }
                value[pos] = item;                      //Actualizamos el valor
                count[pos] = 1;                         //Actualizamos el contador
                nextFree++;                             //Actualizamos el siguiente libre
            }
        }
        size++;                                 //Actualizamos el tamaño de la mochila
    }

    @Override
    public int occurrences(T item) {
        int res = 0;
        int pos = locate(item);
        if (value[pos]==item){
            res = count[pos];
        }
        return res;
    }

    @Override
    public void delete(T item) {
        int pos = locate(item);
        if (pos<nextFree){
            if (value[pos]==item){
                size = size - count[pos];
                for (int i = pos; i<nextFree; i++){
                    value[i] = value[i+1];
                    count[i] = count[i+1];
                }
                nextFree--;
            }
        }
    }

    @Override
    public String toString() {
        StringJoiner sj = new StringJoiner(" ", "Bag(", ")");
        for (int i = 0; i < nextFree; i++) {
            sj.add(String.format("(%s, %s)", value[i], count[i]));
        }
        return sj.toString();
    }

    @Override
    public Iterator<T> iterator() {
        return new SortedArrayBagIterator();
    }
    private class SortedArrayBagIterator implements Iterator<T>{
        private int current;

        public SortedArrayBagIterator(){
            current = 0;
        }

        @Override
        public boolean hasNext() {
            return current<nextFree;
        }

        @Override
        public T next() {
            if (!hasNext()){
                throw new NoSuchElementException();
            } else {
                current++;
                return value[current];
            }
        }
    }

}
