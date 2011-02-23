package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;





public class PlyReader {
	public static int MAX_USER_DATA = 256;
	
	

	public final void readPlyAsciiMesh(TriMesh mesh, String directory, String filename) {
		
		// TriMesh mesh;
		 File file;
		 int i;
	        int iVertexCount = 0;
	        int iTriangleCount = 0;
	        boolean readHeader = true;
	        float x = 0f, y = 0f, z = 0f;
	        // int idx1 = 0, idx2 = 0, idx3 = 0;
			
	        Vector<Point3> vertexArray = new Vector<Point3>();
	        
	        
	        file = openFiles(directory, filename);
	        
	        
	        
	        try {
	            DataInputStream in = new DataInputStream(new BufferedInputStream(new FileInputStream(file)));
	            String s, token;
	            while ( (s = readLine(in)).length() > 0) {
	                StringTokenizer st = new StringTokenizer(s);
	                while ( st.hasMoreTokens() && readHeader ) {
	                    // System.err.print(st.nextToken() + " ");
	                    token = st.nextToken();
	                    if ( token.equals("vertex")) {
	                        iVertexCount = Integer.valueOf(st.nextToken());
	                    } else if ( token.equals("face") ) {
	                        iTriangleCount = Integer.valueOf(st.nextToken());
	                        readLine(in);
	                        readLine(in);  // skip two lines follow the face count attribute in PLY file format.
	                        readHeader = false;
	                        break;
	                    }
	                }
	                if ( readHeader == false) break;
	            }
	        
	        
	            // read Vertex 
	            for ( i = 0; i < iVertexCount; i++ ) {
	                s = readLine(in);
	                StringTokenizer st = new StringTokenizer(s);
	                x = Float.valueOf(st.nextToken());
	                y = Float.valueOf(st.nextToken());
	                z = Float.valueOf(st.nextToken());
	                vertexArray.add(new Point3(x, y, z));
	            }
			     
	           
	            mesh.setVertex(vertexArray);
	            
		       
	            
	        } catch (FileNotFoundException e) {
	            e.printStackTrace();
	        } catch (IOException e) {
	            e.printStackTrace();
	        }
	}
	
	 /** Read a line of ASCII text from the input stream. 
     * @param in InputStream
     * @return line of the ascii file as a String
     * @throws IOException I/O exception
     */
    private static final String readLine(InputStream in) throws IOException
    {
        StringBuffer buf = new StringBuffer();
        int c;
        while ((c = in.read()) > -1 && c != '\n')
        {
            buf.append((char) c);
        }
        return buf.toString();
    }
    
    private static final File openFiles(String directory, String filename) {

    	String fname = new String(directory + File.separator + filename);
    	System.err.println(fname);
    	File file = new File(fname);
    	return file;

       
    }

    
    
}






class LoadPly_FaceAux {
	
	char size;
	int[] v = new int[512];
	int flags;
	float q;
	float[] texcoord = new float[32];
	char ntexcoord;
	int texcoordind;
	float[] colors = new float[32];
	char ncolors;
	
	char r;
	char g;
	char b;
	
	char[] data = new char[256];
	
}


class LoadPly_TristripAux
{
	int size;
	int[] v;
	char[] data = new char[256];  
}

class LoadPly_VertAux
{
	Point3[] p = new Point3[3];
	Point3[] n = new Point3[3];
	int flags;
	float q;
	char r;
	char g;
	char b;
	char[] data = new char[256];  
}
