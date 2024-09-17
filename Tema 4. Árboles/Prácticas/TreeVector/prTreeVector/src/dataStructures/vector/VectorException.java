/**
 * @author Pepe Gallardo, Data Structures, Grado en Informática. UMA.
 * <p>
 * Exceptions for vectors
 */

package dataStructures.vector;

public class VectorException extends RuntimeException {
    private static final long serialVersionUID = 671985445147206456L;

    public VectorException() {
        super();
    }

    public VectorException(String msg) {
        super(msg);
    }
}
