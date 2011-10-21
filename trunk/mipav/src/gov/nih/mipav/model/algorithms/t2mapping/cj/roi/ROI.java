package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import java.lang.*;
import java.io.*;
import java.net.*;
import java.text.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

/**
 *  Manage a "new" ROI file format (ie for LTFU etc).
 *
 *  The ROI saved on the disk is made up of the following lines
 *
 *  e.g.
 * 
 *  <pre>
 *  #<<<<<<<ROIS>>>>>>>#
 *  UniqueID  :autoseed:1:1
 *  TimeStamp :27-Mar-2002
 *  UserID    :resadmin
 *  IsDeleted :false
 *  TissueType:wm
 *  Anatomy   :
 *  Algorithm :user
 *  Comments  :
 *  MIFsize   :250 250 24
 *  PixelSize :1000 1000 5000
 *  Centre    :127 148 3
 *  127 148 3 1
 *  </pre>
 */
public class ROI extends Region implements Cloneable {

	static int counter=0;

	/**
	 *  Setup the appropriate things.
	 */
	public ROI()
	{
		Xs = new AltVec(); Ys = new AltVec(); Zs = new AltVec();
		Fs = new AltVec();
		associations = new Vector();
		clear();
	}

	/**
	 *  Setup the appropriate things.
	 */
	public ROI(LEDataInputStream ldis) throws IOException
	{
		Xs = new AltVec(); Ys = new AltVec(); Zs = new AltVec();
		Fs = new AltVec();
		associations = new Vector();

		try
		{
			read(ldis);
		}
		catch( IOException e )
		{
			throw(e);
		}
	}

	/**
	 *  Cloning needs help.
	 */
	public Object clone()
	{
		Object o = null;

		try {
			o = super.clone();
		}
		catch( CloneNotSupportedException e) {
			System.out.println("ROI can not clone.");
		}

		System.out.println("ROI ** Warning **:  Cloning may not be right.");

		return o;
	}

	/**
	 *  Pad with zeros.
	 */
	private String pad(int i, int len)
	{
		String t = Integer.toString(i);
		int tlen = t.length();
		while(tlen < len) { t = "0"+t; tlen++; }
		return t;
	}

	/**
	 *  Create the identifier.
	 */
	private void createIdentifier()
	{
		Date d = new Date();
		String host, user = System.getProperty("user.name");

		try {
			host = InetAddress.getLocalHost().getHostName();
			if (host.indexOf('.') >= 0)
				host = host.substring(0, host.indexOf('.'));
		}
		catch(UnknownHostException e) {
			System.err.println("Could not determine hostname");
			host = "";
		}

		SimpleDateFormat df = new SimpleDateFormat("yyyy:MM:dd:HH:mm:ss:SSS");

		identifier =
			"J01_" + df.format(d) + "_" + host + "_" + user + "_" + pad(counter,5);

		counter++;
	}

	/**
	 *  Size of the ROI?
	 */
	public int size() { return size; }

	/**
	 *  Get a data point.
	 */
	public double getData(int row, int col, int slice)
	{
		double toreturn = 0.0;

		for(int ii=0; ii<Xs.size(); ii++)
		{
			if( Xs.getShort(ii) == row && Ys.getShort(ii) == col && 
			                              Zs.getShort(ii) == slice )
				toreturn = Math.max(toreturn, (double)Fs.getFloat(ii));
		}

		return toreturn;
	}

	/**
	 *  Get a volume.
	 */
	public Volume getVolume(int nrows, int ncols, int nslices)
	{
		Volume v = new Volume(nrows, ncols, nslices);

		for(int ii=0; ii<Xs.size(); ii++)
		{
			v.setData(Fs.getFloat(ii), Xs.getShort(ii), Ys.getShort(ii), 
			          Zs.getShort(ii));
		}

		return v;
	}


	/**
	 *  Add a point into the set of points.
	 */
	public void add(short x, short y, short z, float f) 
	{
		Xs.add(x); Ys.add(y); Zs.add(z); Fs.add(f); size++;
	}

	/**
	 *  Add a POI into the set of points.
	 */
	public void add(POI p) {
		add( p.x, p.y, p.z, p.f );
	}

	/**
	 *  Get a copy of the list of points.
	 */
	public POI[] getPOI() {
		POI[] temp = new POI[size];
		for(int i=0;i<size;i++) temp[i] = getPOI(i);
		return temp;
	}

	/**
	 *  Get a copy of the list of points.
	 */
	public void getPOI(POI[] p) {
		for(int i=0;i<size;i++) getPOI(i,p[i]);
	}

	/**
	 *  Get a specific POI.
	 */
	public POI getPOI(int i) {
		POI p = new POI(); getPOI(i,p); return p;
	}

	/**
	 *  Get a specific POI.
	 */
	public void getPOI(int i, POI p) {
		p.x = Xs.getShort(i);
		p.y = Ys.getShort(i);
		p.z = Zs.getShort(i);
		p.f = Fs.getFloat(i);
	}

	/**
	 *  See if the specified point is in the ROI.
	 */
	final public boolean contains(final int x, final int y, final int z)
	{
		for(int ii=0; ii<size; ii++) 
		{
			if ( x == Xs.getShort(ii) && y == Ys.getShort(ii) && z == Zs.getShort(ii) )
			{
				return true;
			}
		}

		return false;
	}

	/**
	 *  Transpose the X and Y coordinates.
	 */
	void transpose() { AltVec temp = Ys; Ys = Xs; Xs = temp; }

	/**
	 *  Is this a grown region.
	 */
	public boolean isGrown() { return !algorithm.equals("user"); }

	// Get and Set on variables.
	final public String getIdentifier() { return identifier; }
	final public String getDate() { return date; }
	final public String getName() { return name; }
	final public String getComments() { return comments; }
	final public String getTissueCode() { return tissue_code; }
	final public String getStructureCode() { return structure_code; }
	final public String getAlgorithm() { return algorithm; }

	final public Slice getSlice(int slice_number) 
	{
		System.out.println("ROI: slice is defined as 256, 256");
		Slice s = new Slice(256,256);

		POI[] p = getPOI();

		for(int ii=0; ii<p.length; ii++)
		{
			if( p[ii].z == slice_number )
			{
				s.setData(100.0, p[ii].x, p[ii].y);
			}
		}

		return s;
	}

	final public void setDate( ) { this.date = (new Date()).toString(); }
	final public void setDate( String date ) { this.date = date; }
	final public void setName( String name ) { this.name = name; }
	final public void setComments( String comments ) { this.comments = comments; }
	final public void setTissueCode( String tissue_code ) { this.tissue_code = tissue_code; }
	final public void setStructureCode( String structure_code ) { this.structure_code = structure_code; }
	final public void setAlgorithm( String algorithm) { this.algorithm = algorithm; }
	final public void setMIFSize( int x, int y, int z ) { mifsize = x + " " + y + " " + z; }
	final public void setPixelSize( int x, int y, int z ) { pixelsize = x + " " + y + " " + z; }
	final public void setMIFAssoc( String mifassoc ) { this.mifassoc = mifassoc; }

	final public String[] getAssociation()
	{
		String[] s = new String[associations.size()];
		associations.toArray(s); return s;
	}

	final public void setAssociation( final ROI r )
	{
		associations.add( r.getIdentifier() );	
	}

	// Clear
	public void clear()
	{
		associations.clear();
		Xs.clear(); Ys.clear(); Zs.clear();
		Fs.clear();
		size = 0;

		identifier = "";
		date = "";
		name = "";
		comments = "";
		tissue_code = "";
		structure_code = "";
		algorithm = "";
		isDeleted = "false";

		createIdentifier();
	}

	/**
	 *  Read in the ROI.
	 *
	 *  The assumption is that the header line has already
	 *  been read and therefore we are starting with the first
	 *  real line of interest.
	 */
	public void read( LEDataInputStream ldis ) throws IOException
	{
		clear();

		String line;
		
		/*
		 *  Continue reading all lines till we find a line starting
		 *  with a number.
		 */
        try {
            do {
                line = ldis.readLine();

				if (line == null) throw new EOFException();

				if( line.startsWith("UniqueID  :") ) { identifier = line.substring(11); }
				else if( line.startsWith("TimeStamp :") ) { date = line.substring(11); }
				else if( line.startsWith("UserID    :") ) { name = line.substring(11); }
				else if( line.startsWith("IsDeleted :") ) { isDeleted = line.substring(11); }
				else if( line.startsWith("TissueType:") ) { tissue_code = line.substring(11); }
				else if( line.startsWith("Anatomy   :") ) { structure_code = line.substring(11); }
				else if( line.startsWith("Algorithm :") ) { algorithm = line.substring(11); }
				else if( line.startsWith("Comments  :") ) { comments = line.substring(11); }
				else if( line.startsWith("MIFsize   :") ) { mifsize = line.substring(11); }
				else if( line.startsWith("PixelSize :") ) { pixelsize = line.substring(11); }
				else if( line.startsWith("Centre    :") ) { centre = line.substring(11); }
				else if( line.startsWith("MIF_Assoc :") ) { mifassoc = line.substring(11); }
				else if( line.startsWith("ROI_Depend:") ) { associations.add(line.substring(11)); }
				else if( line.startsWith("SessnRole :") ) { session_role = line.substring(11); }
				else if( !startsWithNumber( line ) )
					System.err.println("ROI: Unrecognised line " + line);

			} while (!startsWithNumber(line));

			/* 
			 *  Now parse the lines that begin with a number until
			 *  we reach a line that is starting with the next
			 *  #<<<...
			 */
			while(!line.startsWith("#<<<"))
			{
				if (!line.startsWith("#<<<"))
				{
					int index = 0;
					int x =  Integer.parseInt(line.substring(index, line.indexOf(' ', index)));
					index = line.indexOf(' ', index)+1;
					int y =  Integer.parseInt(line.substring(index, line.indexOf(' ', index)));
					index = line.indexOf(' ', index)+1;
					int z =  Integer.parseInt(line.substring(index, line.indexOf(' ', index)));
					index = line.indexOf(' ', index)+1;
					float f = Float.parseFloat(line.substring(index));
					
					add((short)(x-1), (short)(y-1), (short)(z-1), f);
				}

				line = ldis.readLine();
			}
        }
        catch( IOException e ) {
			throw(e);
        }
	}
			
	public boolean isDeleted()
	{
		return (isDeleted.indexOf("true") != -1);
	}

	/**
	 *  Determine if the line begins with a number.
	 */
	private boolean startsWithNumber(final String line)
	{
		boolean out = false;

		if( line.startsWith("1") ) out = true;
		if( line.startsWith("2") ) out = true;
		if( line.startsWith("3") ) out = true;
		if( line.startsWith("4") ) out = true;
		if( line.startsWith("5") ) out = true;
		if( line.startsWith("6") ) out = true;
		if( line.startsWith("7") ) out = true;
		if( line.startsWith("8") ) out = true;
		if( line.startsWith("9") ) out = true;

		return out;
	}

	public String toString()
	{
		OutputStream b = new ByteArrayOutputStream(200);
		PrintStream p = new PrintStream(b);
		try {write(p);} catch(IOException e) {return null;}
		p.flush();
		return b.toString();
	}

	public void write( PrintStream of ) throws IOException
	{
		of.println("#<<<<<<<ROIS>>>>>>>#");
		of.println("UniqueID  :" + identifier);
		of.println("TimeStamp :" + date);
		of.println("UserID    :" + name);
		of.println("IsDeleted :" + isDeleted);
		of.println("TissueType:" + tissue_code);
		of.println("Anatomy   :" + structure_code);
		of.println("Algorithm :" + algorithm);
		of.println("Comments  :" + comments);
		of.println("MIFsize   :" + mifsize);
		of.println("PixelSize :" + pixelsize);
		of.println("Centre    :" + getCentre());
		of.println("MIF_Assoc :" + mifassoc);
		for(int i=0; i<associations.size(); i++)
		{
			of.println("ROI_Depend:" + associations.get(i));
		}

		POI p = new POI();
		for(int i=0; i<size; i++)
		{
			getPOI(i,p);
			of.println(p);
		}
	}

	// TEMPORARY IMPLEMENTATION
	private POI getCentre()
	{
		return getPOI(0);
	}

	/** 
	 *  Get the number of rows
	 */
	public int getNRows()
	{
		int ind = mifsize.indexOf(" ");

		return Integer.parseInt( mifsize.substring(0, ind).trim() );
	}

	/** 
	 *  Get the number of columns
	 */
	public int getNCols()
	{
		int ind = mifsize.indexOf(" ");
		int ind2 = mifsize.indexOf(" ", ind+1);

		return Integer.parseInt( mifsize.substring(ind, ind2).trim() );
	}

	/** 
	 *  Get the number of slices
	 */
	public int getNSlices()
	{
		int ind = mifsize.indexOf(" ");
		int ind2 = mifsize.indexOf(" ", ind+1);

		return Integer.parseInt( mifsize.substring(ind2).trim() );
	}

	/** 
	 *  Get the dimension in mm.
	 */
	public double getXDim()
	{
		int ind = pixelsize.indexOf(" ");

		return Double.parseDouble( pixelsize.substring(0, ind).trim() )/1000.0;
	}

	/** 
	 *  Get the dimension in mm.
	 */
	public double getYDim()
	{
		int ind = pixelsize.indexOf(" ");
		int ind2 = pixelsize.indexOf(" ", ind+1);

		return Double.parseDouble( pixelsize.substring(ind, ind2).trim() )/1000.0;
	}

	/** 
	 *  Get the dimension in mm.
	 */
	public double getZDim()
	{
		int ind = pixelsize.indexOf(" ");
		int ind2 = pixelsize.indexOf(" ", ind+1);

		return Double.parseDouble( pixelsize.substring(ind2).trim() )/1000.0;
	}

	//
	//  Private variables.
	//
	private String identifier;
	private String date;
	private String name;
	private String comments;
	private String tissue_code;
	private String structure_code;
	private String algorithm;
	private String isDeleted;
	private String pixelsize;
	private String mifsize;
	private String centre;
	private String mifassoc;
	private String session_role;
	private Vector associations;
	private AltVec Xs, Ys, Zs;  // short
	private AltVec Fs;          // float
	private int size;

public static void main(String[] argv) throws IOException
{
	ROI r = new ROI(), r2 = new ROI(), r3 = new ROI();
	System.out.print(r3.toString());
}

}

