package gov.nih.mipav.model.algorithms.t2mapping.cj.appkit;

import java.io.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;

abstract public class FilterAppKit extends AppKit {

	public static int READ_ONLY = 0;
	public static int READ_WRITE = 1;

	protected String filename;
    protected int filterMode = READ_WRITE;

	protected String outputDirectory = "./";

    public void printArgs() {}
    public void initParams(MCVolume v) {}
    public void initParams(MRIFile b) {initParams(b.getMCVolume());}

    abstract public MCVolume filter(MCVolume v);

	public FilterAppKit()
	{
		super();

		addOpt_reqArg("output-directory", 888, "Output directory (def: '.').");
	}

	public void help()
	{
		System.out.println(getAppName() + " [options] <filename> [<filename> ...]");
		printOptions();
	}

	boolean parseArgBase(int c)
	{
		switch(c) {
			case 888:
				setOutputDirectory(getStringArg("outdir"));
			break;

			default:
				if (!parseArg(c)) return false;
			break;
		}
		return true;
	}

    // Make sure to implement these for READ_WRITE
    public String getOutputSuffix() { return "%"; }
    public String getComment() { return ""; }

	public void setOutputDirectory(String directory)
	{
		//  First let's make sure it exists...
		File f = new File(directory);
		if( !f.isDirectory() ) 
		{ 	
			System.out.println(directory + " is not a directory."); 
			System.exit(-1);
		}

		outputDirectory = directory + File.separator;

	}

    public String getOutputFilename(String filename)
	{
		String in = (new File(filename)).getName();

		int ext_index = in.lastIndexOf(".");

		if( ext_index > 0 && in.endsWith(".gz") ) {
			ext_index = in.lastIndexOf(".", ext_index-1);
		}

        if (ext_index < 0) return outputDirectory + in + getOutputSuffix();
		else return outputDirectory + in.substring(0, ext_index) + getOutputSuffix() + 
		     	    in.substring(ext_index);
	}

    public void pre() { filterMode = READ_WRITE; }

    public void preSave(MRIFile b)
	{
		if( b.getFileType() == MRIFile.FILETYPE_BFF )
			{ ((BFF)b).addComment(getComment()); }
	}

    public void post() { }

	public void run() 
	{
        pre();

		for(int ii=0; ii<otherArgs.length; ii++) 
		{
			filename = otherArgs[ii];

			if (verbose > 1) { System.out.println("Filename is " + filename); }

			MRIFile b = MRIFile.readMRIFile(filename);

			if (b == null)
			{
				System.err.println( getAppName() + "::FilterAppKit: Couldn't open " + filename + ", skipping...");
				continue;
			}

			initParams(b);

			MCVolume dm = b.getMCVolume();

            if (verbose > 0) { printArgs(); }

            dm = filter(dm);

			if (dm == null)
			{
				continue;
			}

			if (filterMode == READ_WRITE)
				{
				b.setMCVolume(dm);

                preSave(b);
				//
				//  Now write it out.
				//
                String filename_out = getOutputFilename(filename);

				if( verbose > 1 ) {
					System.out.println("Output Filename is " + filename_out);
				}

				b.write(filename_out);
			}
		}

		post();

        // Temporary hack for java not exiting due to persistent JFrame(?)
		System.exit(0);
	}
}
