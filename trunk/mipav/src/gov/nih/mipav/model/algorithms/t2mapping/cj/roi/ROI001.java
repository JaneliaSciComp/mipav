package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import java.lang.*;
import java.io.*;
import java.net.*;
import java.text.*;
import java.util.*;
import gov.nih.mipav.model.algorithms.t2mapping.cmp.LEDataStream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

public class ROI001 extends Region implements Cloneable {

	static int counter=0;

	public ROI001()
	{
		Xs = new AltVec(); Ys = new AltVec(); Zs = new AltVec();
		Fs = new AltVec();
		associations = new Vector();
		clear();
	}

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

	private String pad(int i, int len)
	{
		String t = Integer.toString(i);
		int tlen = t.length();
		while(tlen < len) { t = "0"+t; tlen++; }
		return t;
	}

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

	public int size() { return size; }

	public void add(short x, short y, short z, float f) 
	{
		Xs.add(x); Ys.add(y); Zs.add(z); Fs.add(f); size++;
	}

	public void add(POI p) {
		add( p.x, p.y, p.z, p.f );
	}

	public POI[] getPOI() {
		POI[] temp = new POI[size];
		for(int i=0;i<size;i++) temp[i] = getPOI(i);
		return temp;
	}

	public void getPOI(POI[] p) {
		for(int i=0;i<size;i++) getPOI(i,p[i]);
	}

	public POI getPOI(int i) {
		POI p = new POI(); getPOI(i,p); return p;
	}

	public double getData(int row, int col, int slice) {
		System.out.println("getData not implemented yet");
		return 0.0;
	}

	public void getPOI(int i, POI p) {
		p.x = Xs.getShort(i);
		p.y = Ys.getShort(i);
		p.z = Zs.getShort(i);
		p.f = Fs.getFloat(i);
	}

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

	void transpose() { AltVec temp = Ys; Ys = Xs; Xs = temp; }

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

	final public String[] getAssociation()
	{
		String[] s = new String[associations.size()];
		associations.toArray(s); return s;
	}

	final public void setAssociation( final ROI001 r )
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

		createIdentifier();
	}

	// Reading and writing.
	public void read( LEDataInputStream ldis ) throws IOException
	{
		clear();

		String line;
		
		//  Read in the header line, should start with "ROI"
		try {
			line = ldis.readLine();
			if (line==null) throw new EOFException("ROI header line");

			if( !line.startsWith("ROI") )
			{
				System.out.println("ROI: First line of roi did not start with ROI");	
				System.exit(-1);
			}
		}
        catch( EOFException e ) {
			throw e;
		}
		catch (IOException e) {
            System.out.println("ROI: Could not read in the header line.");
            System.out.println(e);
            System.exit(0);
        }

		//  Continue reading all lines till we find an "R:"
        try {
            do {
                line = ldis.readLine();
				// Determine which line it is...

				if( line.startsWith("I:") ) { identifier = line.substring(3); }

				else if( line.startsWith("D:") ) { date = line.substring(3); }

				else if( line.startsWith("N:") ) { name = line.substring(3); }

				else if( line.startsWith("C:") ) { comments = line.substring(3); }

				else if( line.startsWith("T:") ) { tissue_code = line.substring(3); }

				else if( line.startsWith("S:") ) { structure_code = line.substring(3); }
				else if( line.startsWith("A:") ) { algorithm = line.substring(3); }
				else if( line.startsWith("P:") ) 
				{ 
					//  If it is the associations, then read each of them in
					int num_assoc = Integer.parseInt(line.substring(3));

					associations.clear();
					for(int ii=0; ii<num_assoc; ii++) 
					{
						line = ldis.readLine();
						associations.add(line.substring(3));
					}
				}

				else if( line.startsWith("R:") ) { }

				else
					System.err.println("ROI: Unrecognised line " + line);

			} while (!line.startsWith("R:"));

			// Now we need to find out how many points there are and read in each POI.
			int num_points = Integer.parseInt(line.substring(3));

			for( int ii=0; ii< num_points; ii++ )
			{
				line = ldis.readLine();

				if (!line.startsWith("r: "))
					System.err.println("ROI: invalid POI line " + line);

				int index = line.indexOf(' ')+1;
				short x = (short) Integer.parseInt(line.substring(index, line.indexOf(' ', index)));
				index = line.indexOf(' ', index)+1;
				short y = (short) Integer.parseInt(line.substring(index, line.indexOf(' ', index)));
				index = line.indexOf(' ', index)+1;
				short z = (short) Integer.parseInt(line.substring(index, line.indexOf(' ', index)));
				index = line.indexOf(' ', index)+1;
				float f = Float.parseFloat(line.substring(index));
				
				add(x, y, z, f);
			}
        }
        catch( IOException e ) {
			throw(e);
        }
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
		of.println("ROI:ASCII:0.0.1");
		of.println("I: " + identifier);
		of.println("D: " + date);
		of.println("N: " + name);
		of.println("C: " + comments);
		of.println("T: " + tissue_code);
		of.println("S: " + structure_code);
		of.println("A: " + algorithm);
		of.println("P: " + associations.size());
		for(int i=0; i<associations.size(); i++)
		{
			of.println("p: " + associations.get(i));
		}
		of.println("R: " + size);

		POI p = new POI();
		for(int i=0; i<size; i++)
		{
			getPOI(i,p);
			of.println("r: "+p);
		}
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
	private Vector associations;
	private AltVec Xs, Ys, Zs;  // short
	private AltVec Fs;          // float
	private int size;

public static void main(String[] argv) throws IOException
{
	ROI001 r = new ROI001(), r2 = new ROI001(), r3 = new ROI001();
	System.out.print(r3.toString());
}

}

