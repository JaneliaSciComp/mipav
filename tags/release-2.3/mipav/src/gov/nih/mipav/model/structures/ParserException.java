package gov.nih.mipav.model.structures;

/**
 * Exception to indicate that a problem was encountered parsing a string.
 * @author Evan McCreedy
 * @version 1.0
 */
public class ParserException extends Exception {
    /**
     * Create a new exception.
     */
    public ParserException() {}

    /**
     * Create a new exception with a specific message.
     * @param msg  the exception message
     */
    public ParserException( String msg ) {
        super( msg );
    }
}
