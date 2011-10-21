package gov.nih.mipav.model.algorithms.t2mapping.org.j_paine.formatter;

/* This was originally in Formatter.java, but I needed to be able to
 * refer to this from outside the package.
 * --kgs
 */

public class EndOfFileWhenStartingReadException extends InputFormatException
{
  public EndOfFileWhenStartingReadException( int vecptr,
                                             String format,
                                             String line,
                                             int line_number
                                           )
  {
    this( "End of file when starting read of formatted data:\n" +
          "  Index  = " + vecptr + "\n" +
          "  Format = " + format + "\n" +
          "Last line was number " + line_number + ":\n" +
          line
        );
  }

  public EndOfFileWhenStartingReadException( String s )
  {
    super( s );
  }

  public EndOfFileWhenStartingReadException( )
  {
    super( );
  }
}
