package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;


import java.io.File;

import java.io.FileWriter;
import java.io.IOException;

import java.io.PrintWriter;
import java.util.*;




public class PlyWriter {

    public class NormalVector extends Vector<Point3>
    {
        NormalVector()
        {
            super();
        }
    }
	public final void writePlyAsciiMesh(TriMesh mesh, String directory, String filename) {
	    

        
        int iTriangleCount = mesh.fn;
        int iVertexCount = mesh.vert.size();
        
	    //calc per-vertex normals:
	    NormalVector[] akNormals = new NormalVector[iVertexCount];
	    int iV0, iV1, iV2;
	    for ( int i = 0; i < iTriangleCount; i++ )
	    {
            Face f = mesh.face.get(i);
            iV0 = f.getV_index(0);
            iV1 = f.getV_index(1);
            iV2 = f.getV_index(2);
            if ( akNormals[iV0] == null )
            {
                akNormals[iV0] = new NormalVector();
            }
            akNormals[iV0].add( f.N() );
            if ( akNormals[iV1] == null )
            {
                akNormals[iV1] = new NormalVector();
            }
            akNormals[iV1].add( f.N() );
            if ( akNormals[iV2] == null )
            {
                akNormals[iV2] = new NormalVector();
            }
            akNormals[iV2].add( f.N() );
	    }

        for (int i = 0; i < iVertexCount; i++)
        {
            Vertex v = mesh.vert.get(i);
            Point3 kAvg = v.N();
            if ( akNormals[i] != null )
            {
                for ( int j = 0; j < akNormals[i].size(); j++ )
                {
                    kAvg.add_into( akNormals[i].get(j) );
                }
                kAvg.div_into((float)akNormals[i].size());
                kAvg.normalize();
            }
            else
            {
                //Some vertices are not part of triangle faces...
                //System.err.println( i + " null " );
            }
        }
	    
		File file = null;
		
		file = openFiles(directory, filename);
    	         
        try {
            PrintWriter kOut = new PrintWriter(new FileWriter(file));
            
            
            
	        
            // write header
            kOut.println("ply"); // object is TriMesh
            kOut.println("format ascii 1.0");
            kOut.println("element vertex " + iVertexCount);
            kOut.println("property float32 x");
            kOut.println("property float32 y");
            kOut.println("property float32 z");
            kOut.println("property float nx");
            kOut.println("property float ny");
            kOut.println("property float nz");
            kOut.println("element face " + iTriangleCount);
            kOut.println("property list uint8 int32 vertex_indices");
            kOut.println("end_header");

            for (int i = 0; i < iVertexCount; i++) {
                Vertex v = mesh.vert.get(i);
                kOut.print(v.get(0));
                kOut.print(' ');
                kOut.print(v.get(1));
                kOut.print(' ');
                kOut.print(v.get(2));
                
                kOut.print(' ');
                // write vertex normals
                kOut.print(v.N().get(0));
                kOut.print(' ');
                kOut.print(v.N().get(1));
                kOut.print(' ');
                kOut.println(v.N().get(2));
            }
	        	        
            for (int i = 0; i < iTriangleCount; i++) {
            	Face f = mesh.face.get(i);
                kOut.print('3');
                kOut.print(' ');
                kOut.print(f.getV_index(0));
                kOut.print(' ');
                kOut.print(f.getV_index(1));
                kOut.print(' ');
                kOut.println(f.getV_index(2));
            }
	        
            kOut.close();
            
            
        } catch (IOException error) {
            error.printStackTrace();
        }
	        
		

        
	}
	

    private final File openFiles(String directory, String filename) {
    	String fname = new String(directory + File.separator + filename);
    	File file = new File(fname);
    	return file;
    }
	
}