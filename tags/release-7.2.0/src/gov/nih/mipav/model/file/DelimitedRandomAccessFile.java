package gov.nih.mipav.model.file;

import java.io.EOFException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Random;

/**
 * This class is a random access file that has an internal delimiter for fast
 * processing of text files.  It can be used to read in legacy text files, such
 * as comma-separated VOIs. 
 * 
 * @author Justin Senseney
 */

public class DelimitedRandomAccessFile extends RandomAccessFile {

	/** The delimiter that separates each str chunk. */
	private char delim;
	
	/**
	 * This constructor first performs the same actions as RandomAccessFile(String name, String mode),
	 * it then sets the file's delimiter to <code>delim</code>. 
	 * 
	 * @see RandomAccessFile(String name, String mode)
	 */
	public DelimitedRandomAccessFile(String name, String mode, char delim)
			throws FileNotFoundException {
		super(name, mode);
		
		this.delim = delim;
	}

	/**
	 * This constructor first performs the same actions as RandomAccessFile(File file, String mode),
	 * it then sets the file's delimiter to <code>delim</code>. 
	 * 
	 * @see RandomAccessFile(File file, String mode)
	 */
	public DelimitedRandomAccessFile(File file, String mode, char delim)
			throws FileNotFoundException {
		super(file, mode);
		
		this.delim = delim;
	}
	
	/**
	 * Writes a string terminated by the current delimiter.
	 * 
	 * @param str the string to be written
	 * @throws IOException
	 * 
	 * @see RandomAccessFile.writeChars(String)
	 */
	public void writeDelimitedBytes(String str) throws IOException {
		writeBytes(str);
		writeByte(delim);
	}
	
	/**
	 * Allows for writing with a temporary delimiter.
	 * 
	 * @param tempDelim delimiter to be used for this method call only
	 * @see writeDelimitedBytes(String)
	 */
	public void writeDelimitedBytes(String str, char tempDelim) throws IOException {
		char oldDelim = delim;
		delim = tempDelim;
		try {
			writeDelimitedBytes(str);
		} finally {
			//need to reset delimiter even though
			//exception should not be dealt with
			delim = oldDelim;
		}
	}
	
	
	/**
	 * Reads a string from the file up to the current delimiter.  Note that an EOF exception
	 * is caught, with the final string of the file returned
	 * 
	 * @return the read string
	 * @throws IOException
	 * 
	 * @see EOFException
	 */
	public String readDelimitedBytes() throws IOException {
		String newString = new String();
		char next;
		try {
			while((next = ((char)readByte())) != delim) {
				newString += next;
			}
		} catch (EOFException e) {
			e.printStackTrace();
		}
		
		return newString;
	}
	
	/**
	 * This method retrieves a String consisting of all characters from
	 * the current pointer location in the random access file and the 
	 * temporary delimiter.
	 * 
	 * @param tempDelim the temporary delimiter for this class
	 * @return the string up to tempDelim (exclusive)
	 * @throws IOException 
	 */
	public String readDelimitedBytes(char tempDelim) throws IOException {
		char oldDelim = delim;
		delim = tempDelim;
		String newString = new String();
		try {
			newString = readDelimitedBytes();
		} finally {
			//need to reset delimiter even though
			//exception should not be dealt with
			delim = oldDelim;
		}
		
		return newString;
	}
	
	public char getDelimiter() {
		return delim;
	}
	
	public void setDelimiter(char delim) {
		this.delim = delim;
	}
	
	public static void main(String[] args) throws IOException {
		//Shows DelimitedRandomAccessFile's functionality
		int numPoints = 1000;
		File f = new File("delimTry.tmp");
		char delim1 = ';';
		char delim2 = ':';
		char delim3 = ',';
		Random r = new Random();
		double x, y, z;
		String c;
		//Suppose that abnormal calcification points are saved in the following way:
		//Point alpha: x-coord, y-coord, z-coord, Comment goes here;
		//These can be written like so
		DelimitedRandomAccessFile d = new DelimitedRandomAccessFile(f, "rw", delim2);
		for(int i=0; i<numPoints; i++) {
			x = r.nextDouble();
			y = r.nextDouble();
			z = r.nextDouble();
			c = "Comment for "+i;
			d.writeDelimitedBytes("Point "+i);
			d.writeDelimitedBytes(Double.toString(x), delim3);
			d.writeDelimitedBytes(Double.toString(y), delim3);
			d.writeDelimitedBytes(Double.toString(z), delim3);
			d.setDelimiter(delim1);
			d.writeDelimitedBytes(c);
		}
		d.close();
		
		//These can be read like so
		DelimitedRandomAccessFile d2 = new DelimitedRandomAccessFile(f, "rw", delim2);
		String[] xPoints = new String[numPoints];
		String[] yPoints = new String[numPoints];
		String[] zPoints = new String[numPoints];
		String[] comments = new String[numPoints];
		String[] name = new String[numPoints];
		for(int i=0; i<numPoints; i++) {
			name[i] = d2.readDelimitedBytes();
			xPoints[i] = d2.readDelimitedBytes(delim3);
			yPoints[i] = d2.readDelimitedBytes(delim3);
			zPoints[i] = d2.readDelimitedBytes(delim3);
			d2.setDelimiter(delim1);
			comments[i] = d2.readDelimitedBytes();
		}
		d2.close();
	
		//Printing out for testing purposes
		for(int i=0; i<numPoints; i++) {
			System.out.println(name[i]+delim2+" "+xPoints[i]+delim3+" "+yPoints[i]+delim3+" "+zPoints[i]+delim3+" "+
								comments[i]+delim1);
		}
	}
}
