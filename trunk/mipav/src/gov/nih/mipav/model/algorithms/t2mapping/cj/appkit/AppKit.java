package gov.nih.mipav.model.algorithms.t2mapping.cj.appkit;

import javax.swing.*;
import javax.imageio.stream.*;

import java.net.*;
import java.util.*;
import java.util.zip.*;
import java.io.*;
import java.awt.*;
import java.nio.*;
import gov.nih.mipav.model.algorithms.t2mapping.gnu.getopt.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;

/**
 * AppKit is an abstract class providing base functionality that is useful
 * for most applications.  It can be subclassed to quickly create command
 * line programs with basic argument parsing built-in, and it also contains
 * static utility methods for commonly-used operations.
 */

abstract public class AppKit {

// Implement these for sure

/**
 * Returns the name of the application.
 * <p>
 * <B>Must</B> be overloaded by concrete subclasses.  Used in error messages
 * and help messages.
 */
    abstract public String getAppName();

/**
 * Returns the window title of the default file open dialog.
 * <p>
 * <B>Must</B> be overloaded by concrete subclasses.  A file open dialog is
 * opened when a directory is specified rather than a file.
 */
    abstract public String getDialogName();

/**
 * Prints default help message.
 * <p>
 * <B>Must</B> be overloaded by concrete subclasses.  Can use
 * <code>printOptions()</code> to print the automatically generated list
 * of command-line options.
 *
 * @see #printOptions()
 */
	abstract public void help();

/**
 * Main function.
 * <p>
 * <B>Must</B> be overloaded by concrete subclasses.  Typically,
 * <code>run()</code> will loop over the <code>otherArgs</code> array.
 *
 * @see #otherArgs
 */
    abstract public void run();

// Optional for additional arguments

/**
 * Inserts additional command-line options.
 * <p>
 * Should be overloaded to provide more command-line options than the default
 * <code>-h</code> and <code>-v</code>.  Use <code>addOpt_noArg()</code>
 * and <code>addOpt_reqArg()</code> to add new options here.
 *
 * @see #addOpt_noArg(String,int,String)
 * @see #addOpt_reqArg(String,int,String)
 */
    protected void moreOpts() {}

/**
 * Handles each command-line option.
 * <p>
 * Must be overloaded if any options are added in <code>moreOpts()</code>.
 * Typically, this function consists of a <code>switch</code> statement that
 * assigns subclass member variables appropriately.  Expected to return
 * <code>true</code> iff the option was validated.
 * <p>
 * Can use <code>getIntArg()</code>, <code>getFloatArg()</code>, etc., to
 * get the values of parameters without having to catch
 * <code>NumberFormatException</code>.
 *
 * @param c The corresponding short character code
 *
 * @see #moreOpts()
 * @see #getIntArg(String)
 * @see #getShortArg(String)
 * @see #getLongArg(String)
 * @see #getFloatArg(String)
 * @see #getDoubleArg(String)
 * @see #getStringArg(String)
 */
    protected boolean parseArg(int c) { parseArg(c, this.g); return true; }

 /**
  * This allows FilterAppKit to add additional flags.  PLEASE do not use
  * outside of AppKit packages.
  */
    boolean parseArgBase(int c) { return parseArg(c); }

/**
 * @deprecated - Implement the new Getopt-less parseArg.
 */
    protected void parseArg(int c, Getopt g) {}

// These member variables are available

/**
 * Command-line arguments (typically filenames).
 * <p>
 * <code>otherArgs</code> contains all command-line arguments other
 * than hyphenated command-line options, which are parsed separately.
 */
    protected String[] otherArgs;

/**
 * Verbose level.
 * <p>
 * <code>verbose</code> denotes the user-defined verbose level, which
 * is incremented for each instance of the -v option.
 */
    protected int verbose = 0;

	private String option_help = "";
    private Vector vec_opts = new Vector();

    private String short_opts = "";
    private LongOpt[] long_opts;
	private Getopt g;
	private int offset=0;
	private String[] argv;
	private String[] numstr = {"0th","1st","2nd","3rd"};

/**
 * Error handler for command-line options.
 * <p>
 * By default, prints an error message and halts.
 * Is called by <code>getShortArg()</code>, <code>getFloatArg()</code>, etc.
 *
 * @param name  Descriptive name of the command-line option
 */
	protected void doParseError(String name) {
		System.out.println(getAppName() + ": could not parse " + name + " '" +
			getOptarg(name) + "'");
		help();
		System.exit(-1);
	}

	private String getOptarg(String name) {
		if (offset == -1)
			return g.getOptarg();
		else {
			int gi = g.getOptind() + offset;
			if (gi >= argv.length) { doArgMissing(name); return null; }
			else return argv[gi];
		}
	}

	protected void doArgMissing(String name) {
		int foo = offset+2;
		String s = (foo>3)?foo+"th":numstr[foo];

		System.out.println(getAppName() + ": requires " + s + " argument " + name);
		help();
		System.exit(-1);
	}
/**
 * Returns a short integer-valued command-line parameter.
 * <p>
 * @param name Descriptive name of the parameter for error messages
 *
 * @see #parseArg(int)
 */
    protected short getShortArg(String name) {
		short temp = 0;
		try { temp = Short.parseShort(getOptarg(name)); }
		catch ( NumberFormatException e ) { doParseError(name); }
		offset++;
		return temp;
	}

/**
 * Returns an integer-valued command-line parameter.
 * <p>
 * @param name Descriptive name of the parameter for error messages
 *
 * @see #parseArg(int)
 */
    protected int getIntArg(String name) {
		int temp = 0;
		try { temp = Integer.parseInt(getOptarg(name)); }
		catch ( NumberFormatException e ) { doParseError(name); }
		offset++;
		return temp;
	}

/**
 * Returns a long integer-valued command-line parameter.
 * <p>
 * @param name Descriptive name of the parameter for error messages
 *
 * @see #parseArg(int)
 */
    protected long getLongArg(String name) {
		long temp = 0;
		try { temp = Long.parseLong(getOptarg(name)); }
		catch ( NumberFormatException e ) { doParseError(name); }
		offset++;
		return temp;
	}

/**
 * Returns a single precision real-valued command-line parameter.
 * <p>
 * @param name Descriptive name of the parameter for error messages
 *
 * @see #parseArg(int)
 */
    protected float getFloatArg(String name) {
		float temp = 0;
		try { temp = Float.parseFloat(getOptarg(name)); }
		catch ( NumberFormatException e ) { doParseError(name); }
		offset++;
		return temp;
	}

/**
 * Returns a double precision real-valued command-line parameter.
 * <p>
 * @param name Descriptive name of the parameter for error messages
 *
 * @see #parseArg(int)
 */
    protected double getDoubleArg(String name) {
		double temp = 0;
		try { temp = Double.parseDouble(getOptarg(name)); }
		catch ( NumberFormatException e ) { doParseError(name); }
		offset++;
		return temp;
	}

    protected char getCharArg(String name) {
		String s = getOptarg(name);
		if (s.length() != 1) { doParseError(name); }
		offset++;
		return s.charAt(0);
	}

/**
 * Returns a string-valued command-line parameter.
 * <p>
 * @param name Descriptive name of the parameter for error messages
 *
 * @see #parseArg(int)
 */
    protected String getStringArg(String name) {
		String temp = getOptarg(name);
		offset++;
		return temp;
    }

/**
 * Installs a command-line option handler with no arguments.
 * <p>
 * All calls to <code>addOpt_noArg()</code> should be made within
 * <code>moreOpts()</code>
 *
 * @param name long form (e.g. "maximum")
 * @param val  short form (e.g. 'm', or numeric code &gt; 300)
 * @param desc descriptive sentence (e.g. "computes the maximum")
 *
 * @see #moreOpts()
 */
    protected void addOpt_noArg(String name, int val, String desc) {
        vec_opts.add(new LongOpt(name,LongOpt.NO_ARGUMENT,null,val));
		if (val < 256)
		{
        	short_opts = short_opts + (char)val;
			option_help += "   [-" + (char)val + "|--" + name + "] - " + desc + "\n";
		}
		else
		{
			option_help += "   [--" + name + "] - " + desc + "\n";
		}
    }

/**
 * Installs a command-line option handler with a required argument.
 * <p>
 * All calls to <code>addOpt_reqArg()</code> should be made within
 * <code>moreOpts()</code>
 *
 * @param name long form (e.g. "radius")
 * @param val  short form (e.g. 'r' or numeric code &gt; 300)
 * @param desc descriptive sentence (e.g. "radius of filter window")
 *
 * @see #moreOpts()
 */
    protected void addOpt_reqArg(String name, int val, String desc) {
		addOpt_reqArg(name,val,1,desc);
    }
    
    protected void addOpt_reqArg(String name, int val, int nargs, String desc) {
		String argstr = null;
		if (nargs == 1) argstr = "<arg>";
		else if (nargs == 2) argstr = "<arg> <arg>";
		else argstr = "<args" + nargs + ">";

        vec_opts.add(new LongOpt(name,LongOpt.REQUIRED_ARGUMENT,null,val));
		if (val < 256)
		{
        	short_opts = short_opts + (char)val + ':';
			option_help += "   [-" + (char)val + "|--" + name + "] "+argstr+" - " + desc + "\n";
		}
		else
		{
			option_help += "   [--" + name + "] "+argstr+" - " + desc + "\n";
		}
    }

/**
 * Prints a help message for command-line options.
 * <p>
 * Can be called for convenience within <code>help()</code> implemenation.
 *
 * @see #help()
 */
	protected void printOptions() {
		System.out.println("Options:");
		System.out.println(option_help);
	}

    private void initOpts() {
        moreOpts();

		addOpt_noArg("verbose",'v',"Verbose mode.");
		addOpt_noArg("help",'h',"Show this help message.");

        long_opts = new LongOpt[vec_opts.size()];
        vec_opts.toArray(long_opts);
    }

/**
 * Main AppKit event loop (cannot be overloaded).
 * <p>
 * AppKit handles command-line parsing automatically.  The default behaviour
 * is to assume all non-option arguments are filenames.  If a directory name
 * is specified, a JPanel dialog is brought up to select a file within that
 * directory (this behaviour will probably change in the next release!).
 * This routine is usually called by the AppKit wrapper class.
 *
 * Example:
 * <pre>
 * public class Bar {
 *   public static void main(String argv[]) { new BarKit().main(argv); }
 * }
 *
 * public class BarKit extends AppKit { ... }
 * </pre>
 *
 * @param argv Command-line argument list.
 *
 * @see #run()
 */
    final public void main(String argv[]) {
		int c;
		this.argv = argv;

        initOpts();
		g = new Getopt(getAppName(), argv, short_opts, long_opts);

		while( (c=g.getopt()) != -1 ) {
			offset = -1;

			switch(c) {
				case 'h':
					help();
					System.exit(0);
				break;

				case 'v':
					verbose++;
				break;

                default:
					if (!parseArgBase(c)) { help(); System.exit(-1); }
                break;
			}

			if (offset > 0) g.setOptind(g.getOptind()+offset);
		}

		otherArgs = new String[ argv.length - g.getOptind() ];

		for(int ii=g.getOptind(); ii<argv.length; ii++) 
		{
			otherArgs[ii-g.getOptind()] = argv[ii];
		}

		if (otherArgs.length == 0)
		{
			help(); System.exit(0);
		}
		//-------------------------------------------------------
		//
		//  If the filename is a directory, then give the user the
		//  opportunity to find the file.
		//
        if( otherArgs.length == 1 && (new File(otherArgs[0])).isDirectory() ) {
            //
            //  Bring up the dialog to get the file.
            //
            JFrame dialogFrame = new JFrame(getDialogName());
            dialogFrame.setFont( new Font("Helvetica", Font.PLAIN, 12 ) );

            JFileChooser fd = new JFileChooser(otherArgs[0]);
            fd.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
            fd.setFont( new Font("Helvetica", Font.PLAIN, 12 ) );
            int returnVal = fd.showOpenDialog(dialogFrame);                   

            //  if they hit cancel then we quit.
            if( returnVal == JFileChooser.CANCEL_OPTION ) {
                System.exit(0);
            }
			
            otherArgs[0] = fd.getSelectedFile().getAbsolutePath();
        }                                                     

        run();
	}

/**
 * @deprecated - Use openRead(String filename)
 */
	static public LEDataInputStream openInput(String filename)
	{
		LEDataInputStream ldis;

		InputStream fis = privOpenInput(filename);
		if (fis == null) return null;

		fis = new BufferedInputStream( fis, 1000000 );
		ldis = new LEDataInputStream( fis );
		
		return ldis;
	}

/**
 * Utility function: open a file for reading.
 * <p>
 * Opens the specified filename for input and returns an ImageInputStream.
 * If the filename ends in ".gz", the file is automatically opened as an
 * uncompressed stream.  On error, <code>openInput()</code> discards the
 * exception and returns <code>null</code>.
 * <p>
 * ImageInputStream is a superset of <code>LEDataInputStream</code>,
 * allowing seeks and bulk-reads of all primitive data types, as well as
 * bit-level access.  It also has a non-deprecated implementation of
 * <code>readLine()</code>.
 *
 * @see #openWrite(String)
 */
	static public ImageInputStream openRead(String filename)
		{ return openRead(filename, true); }

	static public ImageInputStream openRead(String filename, boolean littleEndian)
	{
		ImageInputStream fcis;

		InputStream fis = privOpenInput(filename);
		if (fis == null) return null;
		try { 	
			fis = new BufferedInputStream(fis, 1000000); 
			fcis = new FileCacheImageInputStream(fis,null); 
		}
		catch (IOException e) { return null; }

		if (littleEndian) fcis.setByteOrder(ByteOrder.LITTLE_ENDIAN);

		return fcis;
	}

	static private InputStream privOpenInput(String filename)
	{
		InputStream fis = privOpenInputFixed(filename);
		if (fis != null) return fis;

		return ( filename.endsWith(".gz") ) ?
			privOpenInputFixed(filename.substring(0,filename.length()-3)) :
			privOpenInputFixed(filename+".gz") ;
	}

	static private InputStream privOpenInputFixed(String filename)
	{
		InputStream fis = null;

		try {
			fis = new FileInputStream(new File( filename ) );
			if( filename.endsWith(".gz") )
				{ fis = new GZIPInputStream( fis ); }
			fis = new BufferedInputStream(fis, 1000000);
		} catch (IOException e) { return null; }

		return fis;
	}

/**
 * Utility function: open a file for writing.
 * <p>
 * Opens the specified filename for output and returns an ImageOutputStream.
 * If the filename ends in ".gz", the file is automatically opened as a
 * compressed stream.  On error, <code>openWrite()</code> discards the
 * exception and returns <code>null</code>.
 */
	static public ImageOutputStream openWrite(String filename)
		{ return openWrite(filename, true); }

	static public class CJFileCacheImageOutputStream extends FileCacheImageOutputStream
	{
		OutputStream temp; 

		public CJFileCacheImageOutputStream(OutputStream stream, File cacheDir) throws IOException
		{
			super(stream, cacheDir);
			temp = stream;
		}

		public void close() throws IOException {
			super.close();
			temp.close();
		}
	}

	static public ImageOutputStream openWrite(String filename, boolean littleEndian)
	{
		ImageOutputStream fcos = null;
		OutputStream fos = privOpenOutput(filename);

		if (fos == null) return null;

//		try 
//		{ 
			fcos = new MemoryCacheImageOutputStream(fos); 
//			fcos = new MemoryCacheImageOutputStream(fos,null); 
//		}
//		catch (IOException e) { return null; }

		if (littleEndian) fcos.setByteOrder(ByteOrder.LITTLE_ENDIAN);

		return fcos;
	}

	static public ImageOutputStream openReadWrite(String filename)
		{ return openReadWrite(filename, true); }

	static public ImageOutputStream openReadWrite(String filename, boolean littleEndian)
	{
		ImageOutputStream fcos;

		if( filename.endsWith(".gz") )
		{
			throw new IllegalArgumentException("openReadWrite can't handle gzip files");
		}

		File f = new File(filename);

		try 
		{ 
			fcos = new FileImageOutputStream(f); 
		}
		catch (IOException e) { return null; }

		if (littleEndian) fcos.setByteOrder(ByteOrder.LITTLE_ENDIAN);

		return fcos;
	}
/**
 * @deprecated - Use openWrite(String filename)
 */
	static public LEDataOutputStream openOutput(String filename)
	{
		LEDataOutputStream ldos;
		OutputStream fos = privOpenOutput(filename);

		if (fos == null) return null;

		fos = new BufferedOutputStream( fos );
		ldos = new LEDataOutputStream( fos );

		return ldos;
	}

	static private OutputStream privOpenOutput(String filename)
	{
		OutputStream fos = null;

		try {
		fos = new FileOutputStream(new File(filename));
		if( filename.endsWith(".gz") )
			{ fos = new GZIPOutputStream( fos ); }
		fos = new BufferedOutputStream( fos );
		} catch (IOException e) { return null; }

		return fos;
	}

/**
 * Utility function: report status of memory and garbage collector.
 * <p>
 * Prints out the total, used, and free memory of the runtime environment.
 * If <code>gc</code> is <code>true</code>, also forces garbage collection
 * and prints a second report.
 *
 * @param ps <code>PrintStream</code> to send output to (e.g.
 * <code>System.out</code>)
 * @param gc Whether or not to perform garbage collection.
 */
	static public void memStatus(PrintStream ps, boolean gc)
	{
		Runtime rt = Runtime.getRuntime();
        ps.println("====");
        ps.println("Total Mem = "+rt.totalMemory());
        ps.println("Free Mem =  "+rt.freeMemory());
        ps.println("Used Mem =  "+(rt.totalMemory()-rt.freeMemory()));
		if (!gc) return;
		rt.gc();
        ps.println("----");
        ps.println("Total Mem = "+rt.totalMemory());
        ps.println("Free Mem =  "+rt.freeMemory());
        ps.println("Used Mem =  "+(rt.totalMemory()-rt.freeMemory()));
	}

/**
 * Utility function: returns simple host name.
 * <p>
 * Returns the unqualified machine name (e.g. "fissure").  In case of error,
 * the exception is discarded and <code>null</code> is returned.
 */
    static public String getHostName()
	{
		String host;
		try
		{
			host = InetAddress.getLocalHost().getHostName();
			int pos = host.indexOf('.');
			if (pos >= 0) host = host.substring(0, pos);
			return host;
		}
		catch (UnknownHostException uhe)
		{
			return null;
		}
	}
}
