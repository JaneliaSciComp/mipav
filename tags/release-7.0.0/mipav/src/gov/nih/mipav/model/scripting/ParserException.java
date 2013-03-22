package gov.nih.mipav.model.scripting;


/**
 * Provides information about an error encountered while parsing a script.
 */
public class ParserException extends Exception {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7330285082866837830L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The line number in the script where this error occurred. */
    private int lineNumber;

    /** The file name of the script where this error occurred ('-' if no corresponding file on disk). */
    private String parsedFileName;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParserException object.  Defaults the error occurring on line 0 of non-file '-'.
     *
     * @param  message  A message describing the problem.
     */
    public ParserException(String message) {
        super(message);
        parsedFileName = "-";
        lineNumber = 0;
    }

    /**
     * Creates a new ParserException object.
     *
     * @param  file     The file the script was read in from on disk ('-' if no file).
     * @param  line     The line in the script where the problem happened.
     * @param  message  A message describing the problem.
     */
    public ParserException(String file, int line, String message) {
        super(message);
        parsedFileName = file;
        lineNumber = line;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return the line number in the script where the problem occurred.
     *
     * @return  The line number in the script where the problem occurred.
     */
    public int getLineNumber() {
        return lineNumber;
    }

    /**
     * Return the file that the script was read in from ('-' if no file on disk).
     *
     * @return  The file that the script was read in from.
     */
    public String getParsedFileName() {
        return parsedFileName;
    }

    /**
     * Changes the line number in the script where the problem happened.
     *
     * @param  line  The line number in the script with the error.
     */
    public void setLineNumber(int line) {
        lineNumber = line;
    }

    /**
     * Changes the file from which the script was read in ('-' if no file on disk).
     *
     * @param  file  The file from which the script was read in.
     */
    public void setParsedFileName(String file) {
        parsedFileName = file;
    }

    /**
     * Converts the information contained in this exception to a String suitable for display to the user.
     *
     * @return  Information about the error encountered.
     */
    public String toString() {
        return getParsedFileName() + ":" + getLineNumber() + ": " + getMessage();
    }
}
