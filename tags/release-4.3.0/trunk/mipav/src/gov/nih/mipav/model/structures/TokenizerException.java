package gov.nih.mipav.model.structures;


/**
 * Exception to indicate that a problem was encountered parsing a string.
 *
 * @author   Evan McCreedy
 * @version  1.0
 */
public class TokenizerException extends Exception {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5430332821046122039L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a new exception.
     */
    public TokenizerException() { }

    /**
     * Create a new exception with a specific message.
     *
     * @param  msg  the exception message
     */
    public TokenizerException(String msg) {
        super(msg);
    }
}
