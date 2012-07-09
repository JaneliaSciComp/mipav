package WildMagic.ApplicationDemos.GLEssentialSupport.UtilSrc;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.net.URL;

import javax.media.opengl.GL;

import com.jogamp.common.nio.Buffers;

public class demoModel
{
	public int numVertcies;

	public float[] positions;
	public int positionType;
	public int positionSize;
	public int positionArraySize;

	public float[] texcoords;
	public int texcoordType;
	public int texcoordSize;
	public int texcoordArraySize;

	public float[] normals;
	public int normalType;
	public int normalSize;
	public int normalArraySize;

	public int[] elements;
	public int elementType;
	public int numElements;
	public int elementArraySize;

	public int primType;

	public demoModel( final String filepathname, String fileName )
	{
		URL fileURL = getClass().getClassLoader().getResource(filepathname);
		if ( fileURL != null )
		{
			String path = fileURL.getPath().substring( 0, fileURL.getPath().lastIndexOf("/") + 1 );
			//System.err.println( path );
		RandomAccessFile file = null;
		try {
			file = new RandomAccessFile( path + filepathname, "r" );
			//file = new RandomAccessFile( path + fileName, "r" );
			// read header:
			byte[] header = new byte[32];
			file.read( header, 0, 32 );
			String s = new String(header);
			System.err.println( s + " " +  s.length() );
			System.err.println( readInt(file) );
			System.err.println( readInt(file) );
			

			int attribHeaderSize = readInt(file);
			int byteElementOffset = readInt(file);
			int bytePositionOffset = readInt(file);
			int byteTexcoordOffset = readInt(file);
			int byteNormalOffset = readInt(file);
			
			file.seek( byteElementOffset );
			
			int byteSize = readInt(file);
			int datatype = readInt(file);
			int primType = readInt(file); 
			int sizePerElement = readInt(file);
			int numElements = readInt(file);
			

			this.elementArraySize = byteSize;
			this.elementType = datatype;
			this.numElements = numElements;
			
			// OpenGL ES cannot use UNSIGNED_INT elements
			// So if the model has UI element...
			if(GL.GL_UNSIGNED_INT == this.elementType)
			{
				//...Load the UI elements and convert to UNSIGNED_SHORT
				this.elements = new int[this.numElements];
				for ( int i = 0; i < this.numElements; i++ )
				{
					this.elements[i] = readInt(file);
				}			
				
				this.elementType  =GL.GL_UNSIGNED_INT;
				this.elementArraySize = this.numElements * Buffers.SIZEOF_INT;
			}

			file.seek( bytePositionOffset );
			byteSize = readInt(file);
			datatype = readInt(file);
			primType = readInt(file); 
			sizePerElement = readInt(file);
			numElements = readInt(file);

			this.positionArraySize = byteSize;
			this.positionType = datatype;
			this.positionSize = sizePerElement;
			this.numVertcies = numElements;
			this.positions = new float[this.numVertcies*this.positionSize];

			for ( int i = 0; i < this.positions.length; i++ )
			{
				this.positions[i] = readFloat(file);
			}
			
			file.seek(byteTexcoordOffset);
			byteSize = readInt(file);
			datatype = readInt(file);
			primType = readInt(file); 
			sizePerElement = readInt(file);
			numElements = readInt(file);

			this.texcoordArraySize = byteSize;
			this.texcoordType = datatype;
			this.texcoordSize = sizePerElement;			
			//Must have the same number of texcoords as positions
			if (this.numVertcies != numElements)
			{
				System.err.println( "error reading tex coords" );
			}
			
			this.texcoords = new float[this.numVertcies*this.texcoordSize];
			for ( int i = 0; i < this.texcoords.length; i++ )
			{
				this.texcoords[i] = readFloat(file);
			}
			
			file.seek( byteNormalOffset );
			byteSize = readInt(file);
			datatype = readInt(file);
			primType = readInt(file); 
			sizePerElement = readInt(file);
			numElements = readInt(file);
			
			this.normalArraySize = byteSize;
			this.normalType = datatype;
			this.normalSize = sizePerElement;

			//Must have the same number of normals as positions
			if(this.numVertcies != numElements)
			{
				System.err.println( "error reading normals" );
			}
				
			this.normals = new float[this.numVertcies*this.normalSize];
			for ( int i = 0; i < this.normals.length; i++ )
			{
				this.normals[i] = readFloat(file);
			}							
			
		} catch (FileNotFoundException e) {
			System.err.println("Unable to find the file " + filepathname);
			e.printStackTrace();
		} catch (IOException e) {
			System.err.println("Problem reading the file " + filepathname);
			e.printStackTrace();
		} finally {
			try {
				if (file != null) {
					file.close();
				}
			} catch (IOException closee) {}
		}
		}
	}
	
	private int readInt(RandomAccessFile in) throws IOException
	{
        byte[] buffer = new byte[4];

        in.read(buffer);
        int index = 0;
        int b1 = buffer[index++] & 0xff;
        int b2 = buffer[index++] & 0xff;
        int b3 = buffer[index++] & 0xff;
        int b4 = buffer[index++] & 0xff;

        return  ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
	}
	
	private float readFloat(RandomAccessFile in) throws IOException
	{
        byte[] buffer = new byte[4];

        in.read(buffer);
        int index = 0;
        int b1 = buffer[index++] & 0xff;
        int b2 = buffer[index++] & 0xff;
        int b3 = buffer[index++] & 0xff;
        int b4 = buffer[index++] & 0xff;

        return Float.intBitsToFloat((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
	}
	
	public demoModel()
	{
		positions = new float[] {
				-200.0f, 0.0f, -200.0f,
				200.0f, 0.0f, -200.0f,
				200.0f, 0.0f,  200.0f,
				-200.0f, 0.0f,  200.0f
		};

		texcoords = new float[] { 
				0.0f,  1.0f,
				1.0f,  1.0f,
				1.0f,  0.0f,
				0.0f,  0.0f
		};

		normals = new float[] {
				0.0f, 0.0f, 1.0f,
				0.0f, 0.0f, 1.0f,
				0.0f, 0.0f, 1.0f,
				0.0f, 0.0f, 1.0f,
		};

		elements = new int[] {
				0, 2, 1,
				0, 3, 2
		};

		positionType = GL.GL_FLOAT;
		positionSize = 3;
		positionArraySize = Buffers.SIZEOF_FLOAT * positions.length * positionSize;

		texcoordType = GL.GL_FLOAT;
		texcoordSize = 2;
		texcoordArraySize = Buffers.SIZEOF_FLOAT * texcoords.length * texcoordSize;

		normalType = GL.GL_FLOAT;
		normalSize = 3;
		normalArraySize = Buffers.SIZEOF_FLOAT * normals.length * normalSize;

		elementArraySize = Buffers.SIZEOF_INT * elements.length;

		primType = GL.GL_TRIANGLES;


		numElements = elements.length;
		elementType = GL.GL_UNSIGNED_INT;
		numVertcies = 4;

	}

	public void dispose()
	{
		elements = null;
		positions = null;
		normals = null;
		texcoords = null;
	}
} 