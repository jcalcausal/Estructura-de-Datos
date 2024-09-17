package dataStructures.bag;

/**
 * Interface for the Bag ADT.
 *
 * @param <T> Type of elements in bag. Note that {@code T} could be
 *            {@code Comparable} in some implementations.
 */
public interface Bag<T> extends Iterable<T> {

    /**
     * Test the bag for emptiness.
     *
     * @return {@code true} if bag is empty, {@code false} otherwise.
     */
    boolean isEmpty();

    /**
     * Retrieves total number of elements in bag (its cardinal).
     *
     * @return total number of elements in bag.
     */
    int size();

    /**
     * Inserts one occurrence of {@code item} in the bag.
     *
     * @param item the element to insert.
     */
    void insert(T item);

    /**
     * Returns the number of occurrences of {@code item} in the bag.
     *
     * @param item the element to be counted.
     * @return number of occurrences of {@code item}, 0 if {@code item} is not in the bag.
     */
    int occurrences(T item);

    /**
     * Removes one occurrence of {@code item} from the bag. If {@code item} is not in the bag,
     * the bag is not modified.
     *
     * @param item the element to remove, if present.
     */
    void delete(T item);
}
