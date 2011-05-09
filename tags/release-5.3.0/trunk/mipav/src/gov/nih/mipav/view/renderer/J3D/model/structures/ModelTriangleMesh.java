package gov.nih.mipav.view.renderer.J3D.model.structures;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.model.file.*;

import java.awt.*;

import java.io.*;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.media.j3d.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * A simple triangle mesh that represents a level surface. The mesh only stores vertex positions and vertex normals. The
 * surface viewer creates a Shape3D object whose geometry is an object from ModelTriangleMesh. The surface color is
 * provided by attaching to the Shape3D object an appearance that contains a material. The vertex normals in
 * ModelTriangleMesh are used by the lighting system in conjunction with the surface material to produce the surface
 * color.
 */

public class ModelTriangleMesh extends IndexedTriangleArray {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static int[] direction = new int[3];

    /** DOCUMENT ME! */
    private static float[] startLocation = new float[3];

    /** DOCUMENT ME! */
    private static float[] box = new float[3];

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    Integer kInvalid;

    /** DOCUMENT ME! */
    float[] m_afCurvature;

    /** DOCUMENT ME! */
    UnorderedSetInt[] m_akAdjacent;

    /** DOCUMENT ME! */
    Vector3f[] m_akSNormal;

    /** DOCUMENT ME! */
    Vector3f[] m_akSTangent;

    /** DOCUMENT ME! */
    Vector3f[] m_akVMean;

    /** DOCUMENT ME! */
    Vector3f[] m_akVNormal;

    /** DOCUMENT ME! */
    float m_fEParam, m_fFParam;

    /** DOCUMENT ME! */
    float m_fMeanEdgeLength;

    /** DOCUMENT ME! */
    float m_fStiffness;

    /** DOCUMENT ME! */
    HashMap m_kEMap;

    /** used in smoothing. */
    private HashSet[] connections;

    /** DOCUMENT ME! */
    private Vector3f m_kE0, m_kE1, m_kN;

    /** The ModelClodMesh object that generated the ModelTriangleMesh object, null otherwise. */
    private Object m_kGenerator;

    /** temporary variables to avoid 'new' calls. */
    private Point3f m_kV0, m_kV1, m_kV2, m_kV3;

    /** store the Material properties of the mesh: */
    private Material m_kMaterial = null;
    /** store the mesh transparency: */
    private float m_fTransparency = 0f;

    /** Store the mesh colors: */
    private Color4f[] m_kColors;
    
    /** Per vertex color array, which store the color data saved in .sur file. */
    private Color4f[] perVertexColor = null;
    
    /** support for vtk **/
    protected double[][] vertexData;
    
    /** support for vtk **/
	protected double[][] cellData;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * A triangle mesh whose vertex normal vectors are computed from the geometry of the mesh itself. The normal at a
     * vertex is the normalized average of normals of the triangles that share the vertex.
     *
     * @param  akVertex   array of vertices in the mesh
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public ModelTriangleMesh(Point3f[] akVertex, int[] aiConnect) {
        super(akVertex.length, IndexedTriangleArray.COORDINATES | IndexedTriangleArray.NORMALS | IndexedTriangleArray.TEXTURE_COORDINATE_3 | IndexedTriangleArray.COLOR_4,
              2, new int[]{0, 0},
              aiConnect.length);

        init();

        setCoordinates(0, akVertex);
        setCoordinateIndices(0, aiConnect);
        computeNormals();
        setNormalIndices(0, aiConnect);
        m_kColors = new Color4f[ akVertex.length ];
        for ( int i = 0; i < akVertex.length; i++ )
        {
            m_kColors[i] = new Color4f( 1f, 1f, 1f, 1f );
        }
        setColors( 0, m_kColors );
        setColorIndices(0, aiConnect);
        m_kGenerator = null;
    }

    /**
     * A triangle mesh whose vertex normal vectors have been precomputed.
     *
     * @param  akVertex   array of vertices in the mesh
     * @param  akNormal   array of vertex normals for the mesh
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public ModelTriangleMesh(Point3f[] akVertex, Vector3f[] akNormal, int[] aiConnect) {
        super(akVertex.length, IndexedTriangleArray.COORDINATES | IndexedTriangleArray.NORMALS | IndexedTriangleArray.TEXTURE_COORDINATE_3 | IndexedTriangleArray.COLOR_4,
              2, new int[]{0, 0},
              aiConnect.length);

        init();

        setCoordinates(0, akVertex);
        setCoordinateIndices(0, aiConnect);
        setNormals(0, akNormal);
        setNormalIndices(0, aiConnect);
        m_kColors = new Color4f[ akVertex.length ];
        for ( int i = 0; i < akVertex.length; i++ )
        {
            m_kColors[i] = new Color4f( 1f, 1f, 1f, 1f );
        }
        setColors( 0, m_kColors );
        setColorIndices(0, aiConnect);
        m_kGenerator = null;
    }

    /**
     * A triangle mesh whose vertex normal vectors have been precomputed.
     *
     * @param  akVertex   array of vertices in the mesh
     * @param  akNormal   array of vertex normals for the mesh
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public ModelTriangleMesh(Point3f[] akVertex, Vector3f[] akNormal, int[] aiConnect, Color4f[] vertexColor) {
        super(akVertex.length, IndexedTriangleArray.COORDINATES | IndexedTriangleArray.NORMALS | IndexedTriangleArray.TEXTURE_COORDINATE_3 | IndexedTriangleArray.COLOR_4,
              2, new int[]{0, 0},
              aiConnect.length);

        init();

        setCoordinates(0, akVertex);
        setCoordinateIndices(0, aiConnect);
        setNormals(0, akNormal);
        setNormalIndices(0, aiConnect);
       
        m_kColors = new Color4f[ akVertex.length ];
        if ( vertexColor != null ) {
          perVertexColor = new Color4f[ akVertex.length ];
        }
        for ( int i = 0; i < akVertex.length; i++ )
        {
            m_kColors[i] = new Color4f( 1f, 1f, 1f, 1f );
            if ( vertexColor != null ) {
               perVertexColor[i] = vertexColor[i];
            }
            // m_kColors[i] = new Color4f( vertexColor[i].x, vertexColor[i].y, vertexColor[i].z, vertexColor[i].w  );
            // System.err.println("x = " + m_kColors[i].x + " y = " + m_kColors[i].y + " z = " + m_kColors[i].z + " w = " + m_kColors[i].w);
        }
        
        
        setColors( 0, m_kColors );
        setColorIndices(0, aiConnect);
        m_kGenerator = null;
    }
    
    /**
     * A triangle mesh whose vertex normal vectors have been precomputed.
     *
     * @param  akVertex   array of vertices in the mesh
     * @param  akNormal   array of vertex normals for the mesh
     * @param  akColor    array of vertex colors
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public ModelTriangleMesh(Point3f[] akVertex, Vector3f[] akNormal, Color4f[] akColor, int[] aiConnect) {
        super(akVertex.length,
              IndexedTriangleArray.COORDINATES | IndexedTriangleArray.NORMALS | IndexedTriangleArray.TEXTURE_COORDINATE_3 | IndexedTriangleArray.COLOR_4,
              2, new int[]{0, 0},
              aiConnect.length);

        init();

        setCoordinates(0, akVertex);
        setCoordinateIndices(0, aiConnect);

        if ( akNormal == null )
        {
            computeNormals();
        }
        else
        {
            setNormals(0, akNormal);
        }
        setNormalIndices(0, aiConnect);
        m_kColors = new Color4f[ akVertex.length ];
        for ( int i = 0; i < akVertex.length; i++ )
        {
            m_kColors[i] = new Color4f( 1f, 1f, 1f, 1f );
            if ( akColor != null )
            {
                m_kColors[i].x = akColor[i].x;
                m_kColors[i].y = akColor[i].y;
                m_kColors[i].z = akColor[i].z;
                m_kColors[i].w = akColor[i].w;
            }
        }
        setColors(0, m_kColors);
        setColorIndices(0, aiConnect);
        m_kGenerator = null;
    }

    /**
     * A triangle mesh whose vertex normal vectors have been precomputed.
     *
     * @param  akVertex   array of vertices in the mesh
     * @param  akNormal   array of vertex normals for the mesh
     * @param  akColor    array of vertex colors
     * @param  akTexCoord array of Texture Coordinates
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public ModelTriangleMesh(Point3f[] akVertex, Vector3f[] akNormal, TexCoord3f[] akTexCoord, int[] aiConnect) {
        super(akVertex.length,
              IndexedTriangleArray.COORDINATES | IndexedTriangleArray.NORMALS | IndexedTriangleArray.TEXTURE_COORDINATE_3 | IndexedTriangleArray.COLOR_4,
              2, new int[]{0, 0},
              aiConnect.length);

        init();

        setCoordinates(0, akVertex);
        setCoordinateIndices(0, aiConnect);
        setNormals(0, akNormal);
        setNormalIndices(0, aiConnect);
        m_kColors = new Color4f[ akVertex.length ];
        for ( int i = 0; i < akVertex.length; i++ )
        {
            m_kColors[i] = new Color4f( 1f, 1f, 1f, 1f );
        }
        setColors( 0, m_kColors );
        setColorIndices(0, aiConnect);
        setTextureCoordinates( 0, 0, akTexCoord );
        setTextureCoordinateIndices( 0, 0, aiConnect);
        m_kGenerator = null;
    }

    /**
     * A triangle mesh whose vertex normal vectors have been precomputed.
     *
     * @param  akVertex   array of vertices in the mesh
     * @param  akNormal   array of vertex normals for the mesh
     * @param  akColor    array of vertex colors
     * @param  akTexCoord array of Texture Coordinates
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public ModelTriangleMesh(Point3f[] akVertex, Vector3f[] akNormal, Color4f[] akColor, TexCoord3f[] akTexCoord, int[] aiConnect) {
        super(akVertex.length,
              IndexedTriangleArray.COORDINATES | IndexedTriangleArray.NORMALS | IndexedTriangleArray.TEXTURE_COORDINATE_3 | IndexedTriangleArray.COLOR_4,
              2, new int[]{0, 0},
              aiConnect.length);

        init();

        setCoordinates(0, akVertex);
        setCoordinateIndices(0, aiConnect);
        setNormals(0, akNormal);
        setNormalIndices(0, aiConnect);
        m_kColors = new Color4f[ akVertex.length ];
        for ( int i = 0; i < akVertex.length; i++ )
        {
            m_kColors[i] = new Color4f( 1f, 1f, 1f, 1f );
            if ( akColor != null )
            {
                m_kColors[i].x = akColor[i].x;
                m_kColors[i].y = akColor[i].y;
                m_kColors[i].z = akColor[i].z;
                m_kColors[i].w = akColor[i].w;
            }
        }
        setColors( 0, m_kColors );
        setColorIndices(0, aiConnect);
        setTextureCoordinates( 0, 0, akTexCoord );
        setTextureCoordinateIndices( 0, 0, aiConnect);
        m_kGenerator = null;
    }

    /**
     * Copies a ModelTriangleMesh by copying it's components.
     *
     * @param  ModelTriangleMesh   ModelTriangleMesh to copy
     */
    public ModelTriangleMesh( ModelTriangleMesh kMesh )
    {
        super( kMesh.getVertexCount(),
              IndexedTriangleArray.COORDINATES | IndexedTriangleArray.NORMALS | IndexedTriangleArray.TEXTURE_COORDINATE_3 | IndexedTriangleArray.COLOR_4,
              2, new int[]{0, 0},
               kMesh.getIndexCount() );
        
        init();
        int numVertices = kMesh.getVertexCount();
        int numIndex = kMesh.getIndexCount();
        Point3f[] akVertex = new Point3f[ numVertices ];
        Vector3f[] akNormal = new Vector3f[ numVertices ];
        m_kColors = new Color4f[ numVertices ];
        TexCoord3f[] akTexCoord = new TexCoord3f[ numVertices ];
        int[] aiConnect = kMesh.getIndexCopy();

        kMesh.getCopies( akVertex, akNormal, m_kColors, akTexCoord );

        setCoordinates(0, akVertex);
        setCoordinateIndices(0, aiConnect);
        setNormals(0, akNormal);
        setNormalIndices(0, aiConnect);
        setColors( 0, m_kColors );
        setColorIndices(0, aiConnect);
        setTextureCoordinates( 0, 0, akTexCoord );
        setTextureCoordinateIndices( 0, 0, aiConnect);
        m_kGenerator = null;
    }
    

    /**
     * A triangle mesh whose vertex normal vectors are computed from the geometry of the mesh itself. The normal at a
     * vertex is the normalized average of normals of the triangles that share the vertex.
     *
     * @param  akVertex   array of vertices in the mesh
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public ModelTriangleMesh(WildMagic.LibFoundation.Mathematics.Vector3f[] akVertex, int[] aiConnect) {
    	super(akVertex.length, IndexedTriangleArray.COORDINATES | IndexedTriangleArray.NORMALS | IndexedTriangleArray.TEXTURE_COORDINATE_3 | IndexedTriangleArray.COLOR_4,
    			2, new int[]{0, 0},
    			aiConnect.length);

    	init();

    	Point3f[] akVertices = new Point3f[ akVertex.length ];
    	for ( int i = 0; i < akVertex.length; i++ )
    	{
    		akVertices[i] = new Point3f( akVertex[i].X, akVertex[i].Y, akVertex[i].Z );
    	}

    	setCoordinates(0, akVertices);
    	setCoordinateIndices(0, aiConnect);
    	computeNormals();
    	setNormalIndices(0, aiConnect);
    	m_kColors = new Color4f[ akVertex.length ];
    	for ( int i = 0; i < akVertex.length; i++ )
    	{
    		m_kColors[i] = new Color4f( 1f, 1f, 1f, 1f );
    	}
    	setColors( 0, m_kColors );
    	setColorIndices(0, aiConnect);
    	m_kGenerator = null;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Initializes capabilities and temporary variables.
     */
    private void init()
    {
        setCapability(Geometry.ALLOW_INTERSECT);
        setCapability(GeometryArray.ALLOW_FORMAT_READ);
        setCapability(GeometryArray.ALLOW_COUNT_READ);
        setCapability(GeometryArray.ALLOW_COUNT_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_COORDINATE_INDEX_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_COORDINATE_INDEX_READ);
        setCapability(GeometryArray.ALLOW_NORMAL_READ);
        setCapability(GeometryArray.ALLOW_NORMAL_WRITE);
        setCapability(GeometryArray.ALLOW_COORDINATE_READ);
        setCapability(GeometryArray.ALLOW_COORDINATE_WRITE);
        setCapability(GeometryArray.ALLOW_TEXCOORD_READ);
        setCapability(GeometryArray.ALLOW_TEXCOORD_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_TEXCOORD_INDEX_WRITE);
        setCapability(GeometryArray.ALLOW_COLOR_READ);
        setCapability(GeometryArray.ALLOW_COLOR_WRITE);

        // temporary variables to avoid 'new' calls
        m_kV0 = new Point3f();
        m_kV1 = new Point3f();
        m_kV2 = new Point3f();
        m_kV3 = new Point3f();
        m_kE0 = new Vector3f();
        m_kE1 = new Vector3f();
        m_kN = new Vector3f();

    }


    public Color4f[] getPerVertexColor() {
    	return perVertexColor;
    }
    
    
    public double[][] getVertexData(){
		return vertexData;
	}
	public double[] getVertexData(int i){
		if(vertexData==null){
			return null;
		} else {
			return vertexData[i];
		}
	}
	
	public void setVertexData(double[][] data){
		this.vertexData=data;
	}
	
	public void setVertexData(int i,double val){
		vertexData[i]=new double[]{val};
	}
	public void setVertexData(int i,int j,double val){
		vertexData[i][j]=val;
	}
	public void setVertexData(int i,double[] array){
		vertexData[i]=array;
	}
	
	public void setCellData(double[][] cellData) {
		this.cellData = cellData;
	}
	
	public double[][] getCellData() {
		return cellData;
	}
    
    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static int[] getDirection() {
        return direction;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static float[] getStartLocation() {
        return startLocation;
    }

    /**
     * Load the triangle mesh from a binary file. The caller must have already opened the file and read the mesh type (0
     * = ModelTriangleMesh, 1 = ModelClodMesh) and the number of meshes in the file. The caller then calls this function
     * for each mesh. The format for a mesh is
     *
     * <pre>
       int vCount;  // number of vertices
       Point3f vertices[vCount];
       Point3f normals[vCount];
       int iCount;  // number of indices in the connectivity array
       int indices[iCount];
     * </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kIn    the file from which the triangle mesh is loaded
     * @param      pBar   DOCUMENT ME!
     * @param      added  DOCUMENT ME!
     * @param      total  DOCUMENT ME!
     *
     * @return     the loaded triangle mesh
     *
     * @exception  IOException  if there is an error reading from the file
     */
    public static ModelTriangleMesh loadTMesh(RandomAccessFile kIn, JProgressBar pBar, int added, int total)
            throws IOException {

        try {
            int i, index, tmpInt;
            int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
            int actions;
            boolean flip;
            boolean dicom;
            long c1 = 0, c2 = 0, c3 = 0, c4 = 0, c5 = 0, c6 = 0, c7 = 0, c8 = 0;
            long tmpLong;
            int j;
            //double[][] inverseDicomArray;
            TransMatrix inverseDicomMatrix = new TransMatrix(4);
            float[] tCoord = new float[3];
            float[] coord = new float[3];

            actions = kIn.readInt();

            if ((actions == 1) || (actions == 3)) {
                flip = true;
            } else {
                flip = false;
            }
            
            if ((actions == 2) || (actions == 3)) {
                dicom = true;
            } else {
                dicom = false;
            }
            

            direction[0] = kIn.readInt();
            direction[1] = kIn.readInt();
            direction[2] = kIn.readInt();

            byte[] buffer = new byte[24];

            kIn.read(buffer);
            index = 0;
            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[0] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[1] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[2] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[0] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[1] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[2] = Float.intBitsToFloat(tmpInt);

            if (dicom) {
                buffer = new byte[128];
                kIn.read(buffer);
                index = 0;
                //inverseDicomArray = new double[4][4];

                for (i = 0; i <= 3; i++) {

                    for (j = 0; j <= 3; j++) {
                        c1 = buffer[index++] & 0xffL;
                        c2 = buffer[index++] & 0xffL;
                        c3 = buffer[index++] & 0xffL;
                        c4 = buffer[index++] & 0xffL;
                        c5 = buffer[index++] & 0xffL;
                        c6 = buffer[index++] & 0xffL;
                        c7 = buffer[index++] & 0xffL;
                        c8 = buffer[index++] & 0xffL;
                        tmpLong = ((c1 << 56) | (c2 << 48) | (c3 << 40) | (c4 << 32) | (c5 << 24) | (c6 << 16) |
                                       (c7 << 8) | c8);
                        inverseDicomMatrix.set(i,j, Double.longBitsToDouble(tmpLong));
                    }
                }

            } // if (dicom)


            int iVertexCount = kIn.readInt();
            Point3f[] akVertex = new Point3f[iVertexCount];
            int bufferSize = 12 * iVertexCount;
            byte[] bufferVertex = new byte[bufferSize];
            byte[] bufferNormal = new byte[bufferSize];

            // progress.setLocation(200, 200);
            // progress.setVisible(true);
            // read vertices
            kIn.read(bufferVertex);
            kIn.read(bufferNormal);

            for (i = 0, index = 0; i < iVertexCount; i++) {
                akVertex[i] = new Point3f();

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].x = Float.intBitsToFloat(tmpInt);

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].y = Float.intBitsToFloat(tmpInt);

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].z = Float.intBitsToFloat(tmpInt);

                if (dicom) {
                    tCoord[0] = akVertex[i].x - startLocation[0];
                    tCoord[1] = akVertex[i].y - startLocation[1];
                    tCoord[2] = akVertex[i].z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    akVertex[i].x = (coord[0] * direction[0]) + startLocation[0];
                    akVertex[i].y = (coord[1] * direction[1]) + startLocation[1];
                    akVertex[i].z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {

                    // Flip (kVertex.y - startLocation[1], but
                    // don't flip startLocation[1]
                    akVertex[i].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[i].y;
                    akVertex[i].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[i].z;
                } // if (flip)

            }

            // read normals
            Vector3f[] akNormal = new Vector3f[iVertexCount];

            for (i = 0, index = 0; i < iVertexCount; i++) {
                akNormal[i] = new Vector3f();

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].x = Float.intBitsToFloat(tmpInt);

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].y = Float.intBitsToFloat(tmpInt);

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].z = Float.intBitsToFloat(tmpInt);
            }

            // read connectivity
            int iIndexCount = kIn.readInt();

            // System.out.println("connect count = " + iIndexCount);
            int[] aiConnect = new int[iIndexCount];
            byte[] bufferConnect = new byte[iIndexCount * 4];

            kIn.read(bufferConnect);

            for (i = 0, index = 0; i < iIndexCount; i++) {
                b1 = bufferConnect[index++] & 0x000000ff;
                b2 = bufferConnect[index++] & 0x000000ff;
                b3 = bufferConnect[index++] & 0x000000ff;
                b4 = bufferConnect[index++] & 0x000000ff;

                aiConnect[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                // System.out.println("connect[" + i + "]" + aiConnect[i]);
            }

            ModelTriangleMesh mesh = new ModelTriangleMesh(akVertex, akNormal, aiConnect);

            // mesh.getConsistentComponents();
            // ModelTriangleMesh mesh2 = new ModelTriangleMesh(mesh.getVertexCopy(), mesh.getIndexCopy());
            return mesh;
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Load the triangle mesh from a binary file. The caller must have already opened the file and read the mesh type (0
     * = ModelTriangleMesh, 1 = ModelClodMesh) and the number of meshes in the file. The caller then calls this function
     * for each mesh. The format for a mesh is
     *
     * <pre>
       int vCount;  // number of vertices
       Point3f vertices[vCount];
       Point3f normals[vCount];
       int iCount;  // number of indices in the connectivity array
       int indices[iCount];
     * </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kIn        the file from which the triangle mesh is loaded
     * @param      progress   DOCUMENT ME!
     * @param      added      DOCUMENT ME!
     * @param      total      DOCUMENT ME!
     * @param      isVisible  DOCUMENT ME!
     *
     * @return     the loaded triangle mesh
     *
     * @exception  IOException  if there is an error reading from the file
     */
    public static ModelTriangleMesh loadTMesh(RandomAccessFile kIn, ViewJProgressBar progress, int added, int total,
                                              boolean isVisible) throws IOException {

        try {
            int i, index, tmpInt;
            int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
            int actions;
            boolean flip;
            boolean dicom;
            long c1 = 0, c2 = 0, c3 = 0, c4 = 0, c5 = 0, c6 = 0, c7 = 0, c8 = 0;
            long tmpLong;
            int j;
            //double[][] inverseDicomArray;
            TransMatrix inverseDicomMatrix = new TransMatrix(4);
            float[] tCoord = new float[3];
            float[] coord = new float[3];

            actions = kIn.readInt();

            if ((actions == 1) || (actions == 3)) {
                flip = true;
            } else {
                flip = false;
            }
            System.err.println("flip = " + flip);
            if ((actions == 2) || (actions == 3)) {
                dicom = true;
            } else {
                dicom = false;
            }
            System.err.println("dicom = " + dicom);

            direction[0] = kIn.readInt();
            direction[1] = kIn.readInt();
            direction[2] = kIn.readInt();

            byte[] buffer = new byte[24];

            kIn.read(buffer);
            index = 0;
            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[0] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[1] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            startLocation[2] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[0] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[1] = Float.intBitsToFloat(tmpInt);

            b1 = buffer[index++] & 0xff;
            b2 = buffer[index++] & 0xff;
            b3 = buffer[index++] & 0xff;
            b4 = buffer[index++] & 0xff;

            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            box[2] = Float.intBitsToFloat(tmpInt);

            if (dicom) {
                buffer = new byte[128];
                kIn.read(buffer);
                index = 0;
                //inverseDicomArray = new double[4][4];

                for (i = 0; i <= 3; i++) {

                    for (j = 0; j <= 3; j++) {
                        c1 = buffer[index++] & 0xffL;
                        c2 = buffer[index++] & 0xffL;
                        c3 = buffer[index++] & 0xffL;
                        c4 = buffer[index++] & 0xffL;
                        c5 = buffer[index++] & 0xffL;
                        c6 = buffer[index++] & 0xffL;
                        c7 = buffer[index++] & 0xffL;
                        c8 = buffer[index++] & 0xffL;
                        tmpLong = ((c1 << 56) | (c2 << 48) | (c3 << 40) | (c4 << 32) | (c5 << 24) | (c6 << 16) |
                                       (c7 << 8) | c8);
                        inverseDicomMatrix.set(i,j, Double.longBitsToDouble(tmpLong));
                    }
                }


            } // if (dicom)

            int iVertexCount = kIn.readInt();
            Point3f[] akVertex = new Point3f[iVertexCount];
            int bufferSize = 12 * iVertexCount;
            byte[] bufferVertex = new byte[bufferSize];
            byte[] bufferNormal = new byte[bufferSize];

            progress.setLocation(200, 200);
            progress.setVisible(isVisible);

            // read vertices
            kIn.read(bufferVertex);
            kIn.read(bufferNormal);

            for (i = 0, index = 0; i < iVertexCount; i++) {
                akVertex[i] = new Point3f();

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].x = Float.intBitsToFloat(tmpInt);

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].y = Float.intBitsToFloat(tmpInt);

                b1 = bufferVertex[index++] & 0xff;
                b2 = bufferVertex[index++] & 0xff;
                b3 = bufferVertex[index++] & 0xff;
                b4 = bufferVertex[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akVertex[i].z = Float.intBitsToFloat(tmpInt);

                if (dicom) {
                    tCoord[0] = akVertex[i].x - startLocation[0];
                    tCoord[1] = akVertex[i].y - startLocation[1];
                    tCoord[2] = akVertex[i].z - startLocation[2];
                    inverseDicomMatrix.transform(tCoord, coord);
                    akVertex[i].x = (coord[0] * direction[0]) + startLocation[0];
                    akVertex[i].y = (coord[1] * direction[1]) + startLocation[1];
                    akVertex[i].z = (coord[2] * direction[2]) + startLocation[2];
                } // if (dicom)

                if (flip) {

                    // Flip (kVertex.y - startLocation[1], but
                    // don't flip startLocation[1]
                    akVertex[i].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[i].y;
                    akVertex[i].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[i].z;
                }

            }

            progress.updateValueImmed(added + (33 / total));

            // read normals
            Vector3f[] akNormal = new Vector3f[iVertexCount];

            for (i = 0, index = 0; i < iVertexCount; i++) {
                akNormal[i] = new Vector3f();

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].x = Float.intBitsToFloat(tmpInt);

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].y = Float.intBitsToFloat(tmpInt);

                b1 = bufferNormal[index++] & 0xff;
                b2 = bufferNormal[index++] & 0xff;
                b3 = bufferNormal[index++] & 0xff;
                b4 = bufferNormal[index++] & 0xff;

                tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                akNormal[i].z = Float.intBitsToFloat(tmpInt);
            }

            progress.updateValueImmed(added + (66 / total));

            // read connectivity
            int iIndexCount = kIn.readInt();

            // System.out.println("connect count = " + iIndexCount);
            int[] aiConnect = new int[iIndexCount];
            byte[] bufferConnect = new byte[iIndexCount * 4];

            kIn.read(bufferConnect);

            for (i = 0, index = 0; i < iIndexCount; i++) {
                b1 = bufferConnect[index++] & 0x000000ff;
                b2 = bufferConnect[index++] & 0x000000ff;
                b3 = bufferConnect[index++] & 0x000000ff;
                b4 = bufferConnect[index++] & 0x000000ff;

                aiConnect[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                // System.out.println("connect[" + i + "]" + aiConnect[i]);
            }

            // read per vertex color array
            int R, G, B, A;
            int isPerVertexColor = kIn.readInt();
            Color4f[] perVertexColor = null;
            if ( isPerVertexColor == 1 ) {
	            perVertexColor = new Color4f[iVertexCount];
	            byte[] bufferPerVertexColor = new byte[iVertexCount * 4 * 4];
	            kIn.read(bufferPerVertexColor);
	            for (i = 0, index = 0; i < iVertexCount; i++) {
	            	perVertexColor[i] = new Color4f();
	
	                b1 = bufferPerVertexColor[index++] & 0xff;
	                b2 = bufferPerVertexColor[index++] & 0xff;
	                b3 = bufferPerVertexColor[index++] & 0xff;
	                b4 = bufferPerVertexColor[index++] & 0xff;
	
	                R = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
	
	                perVertexColor[i].x = Float.intBitsToFloat(R);
	
	                b1 = bufferPerVertexColor[index++] & 0xff;
	                b2 = bufferPerVertexColor[index++] & 0xff;
	                b3 = bufferPerVertexColor[index++] & 0xff;
	                b4 = bufferPerVertexColor[index++] & 0xff;
	
	                G = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
	
	                perVertexColor[i].y = Float.intBitsToFloat(G);
	
	                b1 = bufferPerVertexColor[index++] & 0xff;
	                b2 = bufferPerVertexColor[index++] & 0xff;
	                b3 = bufferPerVertexColor[index++] & 0xff;
	                b4 = bufferPerVertexColor[index++] & 0xff;
	
	                B = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
	
	                perVertexColor[i].z = Float.intBitsToFloat(B);
	                
	                b1 = bufferPerVertexColor[index++] & 0xff;
	                b2 = bufferPerVertexColor[index++] & 0xff;
	                b3 = bufferPerVertexColor[index++] & 0xff;
	                b4 = bufferPerVertexColor[index++] & 0xff;
	
	                A = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
	
	                perVertexColor[i].w = Float.intBitsToFloat(A);
	                
	            }
            }
            
            progress.updateValueImmed(added + (100 / total));

            ModelTriangleMesh mesh = new ModelTriangleMesh(akVertex, akNormal, aiConnect, perVertexColor);

            // mesh.getConsistentComponents();
            // ModelTriangleMesh mesh2 = new ModelTriangleMesh(mesh.getVertexCopy(), mesh.getIndexCopy());
            return mesh;
        } catch (IOException e) {
            return null;
        }
    }

    /**
	 * Save an array of triangle meshes to a binary file. The format for the file is
	 *
	 * <pre>
	   int type;  // 0 = ModelTriangleMesh, 1 = ModelClodMesh
	   int aCount;  // number of array elements
	   array of 'aCount' meshes, each of the form:
	   int vCount;  // number of vertices
	   Point3f vertices[vCount];
	   Point3f normals[vCount];
	   int iCount;  // number of indices in the connectivity array
	   int indices[iCount];
	 * </pre>
	 *
	 * with 4-byte quantities stored in Big Endian format.
	 *
	 * @param      kName              the name of the file to which the components are saved
	 * @param      akComponent        the array of mesh components to be saved
	 * @param      flip               if the y and z axes should be flipped - true in extract and in save of
	 *                                JDialogSurface To have proper orientations in surface file if flip is true flip y
	 *                                and z on reading.
	 * @param      direction          either 1 or -1 for each axis
	 * @param      startLocation      DOCUMENT ME!
	 * @param      box                (dim-1)*res
	 * @param      inverseDicomMatrix  dicom transform
	 * @param 	   perVertexColorArray  color per vertex array.
	 *
	 * @exception  IOException  if there is an error writing to the file
	 */
	public static void save(String kName, ModelTriangleMesh[] akComponent, boolean flip, int[] direction,
	                        float[] startLocation, float[] box, TransMatrix inverseDicomMatrix, Color4f[][] perVertexColorArray) throws IOException {
	
	    if (akComponent.length == 0) {
	        return;
	    }
	
	    int i = kName.lastIndexOf('.');
	    Color3f color = new Color3f(Color.blue);
	    String extension;
	
	    if ((i > 0) && (i < (kName.length() - 1))) {
	        extension = kName.substring(i + 1).toLowerCase();
	
	        if (extension.equals("txt")) {
	            saveAsTextFile(kName, akComponent);
	        } else if (extension.equals("wrl")) {
	            PrintWriter kOut = new PrintWriter(new FileWriter(kName));
	            saveAsPortableVRML(kOut, akComponent, flip, direction, startLocation, box, color);
	            kOut.close();
	        } else if (extension.equals("sur")) {
	            RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");
	            kOut.writeInt(0); // objects are ModelTriangleMesh
	            kOut.writeInt(akComponent.length);
	
	            for (i = 0; i < akComponent.length; i++) {
	            	if (perVertexColorArray != null)
	            		akComponent[i].save(kOut, flip, direction, startLocation, box, inverseDicomMatrix, perVertexColorArray[i]);
	            	else
	            		akComponent[i].save(kOut, flip, direction, startLocation, box, inverseDicomMatrix, null);
	            }
	        }
	    }
	    /*
	     * if ( Preferences.isDebug() ) { print( JDialogBase.makeImageName( kName, "_sur.txt" ), akComponent );} */
	}

	/**
     * Load the triangle mesh from a VRML file specifically written by MIPAV!. The caller must have already opened the
     * file. Must be made more robust to better parse the file. It only reads VRML 2.0 that MIPAV has written.
     *
     * @param      kIn       the file from which the triangle mesh is loaded
     * @param      progress  DOCUMENT ME!
     * @param      added     DOCUMENT ME!
     * @param      total     DOCUMENT ME!
     * @param      flag      DOCUMENT ME!
     *
     * @return     the loaded triangle mesh
     *
     * @exception  IOException  if there is an error reading from the file
     */
    public static ModelTriangleMesh loadVRMLMesh(RandomAccessFile kIn, ViewJProgressBar progress, int added, int total,
                                                 boolean flag) throws IOException {

        String str = null;
        String str2 = null;
        String str3 = null;
        boolean flip = true;
        StringTokenizer stoken = null;
        try {
            //progress.setLocation(200, 200);
            //progress.setVisible(true);

            // read vertices
            if (flag) {

                if (kIn.getFilePointer() == 0) {
                    str = kIn.readLine().trim();
                    str2 = kIn.readLine().trim();
                    str3 = kIn.readLine().trim();

                    if (!str.equals("#VRML V2.0 utf8") || !str2.equals("#MIPAV") ||
                            (str3.indexOf("#Number of shapes =") == -1)) {
                        MipavUtil.displayWarning("File doesn't appear to be a VRML 2.0 file written by MIPAV");
                        Preferences.debug("ModelTriangleMesh.loadVRMLMesh: File doesn't appear to be a VRML 2.0 file written by MIPAV.\n");

                        // or throw error.
                        return null;
                    }

                    // read flip line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#flip")) {
                        stoken.nextToken();

                        int flipInt = Integer.valueOf(stoken.nextToken()).intValue();

                        if (flipInt == 1) {
                            flip = true;
                        } else {
                            flip = false;
                        }
                    }

                    // read direction line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#direction")) {
                        stoken.nextToken();
                        direction[0] = Integer.valueOf(stoken.nextToken()).intValue();
                        direction[1] = Integer.valueOf(stoken.nextToken()).intValue();
                        direction[2] = Integer.valueOf(stoken.nextToken()).intValue();
                    }

                    // read start location line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#startLocation")) {
                        stoken.nextToken();
                        startLocation[0] = Float.valueOf(stoken.nextToken()).floatValue();
                        startLocation[1] = Float.valueOf(stoken.nextToken()).floatValue();
                        startLocation[2] = Float.valueOf(stoken.nextToken()).floatValue();
                    }

                    // read box line
                    str = kIn.readLine().trim();
                    stoken = new StringTokenizer(str);

                    if (stoken.nextToken().equals("#box")) {
                        stoken.nextToken();
                        box[0] = Float.valueOf(stoken.nextToken()).floatValue();
                        box[1] = Float.valueOf(stoken.nextToken()).floatValue();
                        box[2] = Float.valueOf(stoken.nextToken()).floatValue();
                    }
                }
            }

            str = kIn.readLine().trim();

            if (!str.equals("Shape")) {
                return null;
            }

            str = kIn.readLine();

            str = kIn.readLine().trim();
            stoken = new StringTokenizer(str);

            Material kMaterial = null;
            float transparency = 0f;
            if (stoken.nextToken().equals("appearance")) {
                kMaterial = new Material();
                kMaterial.setCapability(Material.ALLOW_COMPONENT_READ);
                kMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);

                str = kIn.readLine(); // material Material {
                str = kIn.readLine().trim(); // emissive Color
                stoken = new StringTokenizer(str);  stoken.nextToken();
                float red = Float.valueOf( stoken.nextToken() ).floatValue();
                float green = Float.valueOf( stoken.nextToken() ).floatValue();
                float blue = Float.valueOf( stoken.nextToken() ).floatValue();
                kMaterial.setEmissiveColor( new Color3f( red, green, blue ) );

                str = kIn.readLine().trim(); // diffuse color
                stoken = new StringTokenizer(str);  stoken.nextToken();
                red = Float.valueOf( stoken.nextToken() ).floatValue();
                green = Float.valueOf( stoken.nextToken() ).floatValue();
                blue = Float.valueOf( stoken.nextToken() ).floatValue();
                kMaterial.setDiffuseColor( red, green, blue );

                str = kIn.readLine().trim(); // specular Color
                stoken = new StringTokenizer(str);  stoken.nextToken();
                red = Float.valueOf( stoken.nextToken() ).floatValue();
                green = Float.valueOf( stoken.nextToken() ).floatValue();
                blue = Float.valueOf( stoken.nextToken() ).floatValue();
                kMaterial.setSpecularColor( new Color3f( red, green, blue ) );

                str = kIn.readLine().trim(); // transparency
                stoken = new StringTokenizer(str);  stoken.nextToken();
                transparency = Float.valueOf( stoken.nextToken() ).floatValue();

                str = kIn.readLine(); // }
                str = kIn.readLine(); // }
            }

            str = kIn.readLine().trim();

            if (!str.equals("geometry IndexedFaceSet")) {
                return null;
            }

            str = kIn.readLine();

            str = kIn.readLine().trim();

            if (!str.equals("coord Coordinate")) {
                return null;
            }

            str = kIn.readLine();
            str = kIn.readLine().trim();

            if (!str.equals("point [")) {
                return null;
            }

            ArrayList vertexPts = new ArrayList(1000);
            ArrayList connArray = new ArrayList(1000);

            boolean readMore = true;
            Point3f fPt;

            while (readMore) {

                str = kIn.readLine().trim();

                if (str.equals("]")) {
                    break;
                }

                stoken = new StringTokenizer(str);
                fPt = new Point3f();
                fPt.x = Float.valueOf(stoken.nextToken()).floatValue();
                fPt.y = Float.valueOf(stoken.nextToken()).floatValue();
                fPt.z = Float.valueOf(stoken.nextToken()).floatValue();

                vertexPts.add(fPt);
            }

            progress.updateValueImmed(added + (25 / total));

            Point3f[] akVertex = new Point3f[vertexPts.size()];

            for (int i = 0; i < vertexPts.size(); i++) {
                akVertex[i] = (Point3f) (vertexPts.get(i));

                if (flip) {
                    akVertex[i].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[i].y;
                    akVertex[i].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[i].z;
                }
            }

            vertexPts = null;
            System.gc();
            progress.updateValueImmed(added + (50 / total));

            str = kIn.readLine().trim();
            str = kIn.readLine().trim();

            if (!str.equals("coordIndex [")) {
                return null;
            }

            while (readMore) {

                str = kIn.readLine().trim();

                if (str.equals("]")) {
                    break;
                }

                stoken = new StringTokenizer(str);

                Integer iConn;

                iConn = Integer.valueOf(stoken.nextToken());
                connArray.add(iConn);

                iConn = Integer.valueOf(stoken.nextToken());
                connArray.add(iConn);

                iConn = Integer.valueOf(stoken.nextToken());
                connArray.add(iConn);
            }

            progress.updateValueImmed(added + (75 / total));

            long position = kIn.getFilePointer();

            while ((str != null) && (str.indexOf("Shape") == -1)) {
                position = kIn.getFilePointer();
                str = kIn.readLine();
            }

            kIn.seek(position);

            int[] aiConnect = new int[connArray.size()];

            for (int i = 0; i < connArray.size(); i++) {
                aiConnect[i] = ((Integer) (connArray.get(i))).intValue();
            }

            progress.updateValueImmed(added + (100 / total));
            
            ModelTriangleMesh kMesh = new ModelTriangleMesh(akVertex, aiConnect);
            kMesh.setMaterial( kMaterial );
            kMesh.setTransparency( transparency );
            return kMesh;
        } catch (IOException e) {
            return null;
        }
    }
    
    
    /**
     * 
     * @param kIn
     * @param progress
     * @param added
     * @param total
     * @param flag
     * @return
     * @throws IOException
     */
    public static ModelTriangleMesh loadVTKLegacyMesh(RandomAccessFile kIn, ViewJProgressBar progress, int added, int total,
            boolean flag, String fileName) throws IOException {
    	ModelTriangleMesh kMesh;
    	
    	System.out.println(fileName);
    	
    	// When reading VTK surface file format, set the default direction to (1, 1, 1).  
    	direction[0] = 1;
        direction[1] = 1;
        direction[2] = 1;
        
    	StringBuffer buff = new StringBuffer();
    	try {
    		//progress.setLocation(200, 200);
            progress.setVisible(true);
			String str;
			// Read file as string
			while ((str = kIn.readLine()) != null) {
				buff.append(str+"\n");
			}
		} catch (Exception e) {
			System.err.println("Error occured while reading parameter file:\n"+e.getMessage());
			e.printStackTrace();
			return null;
		}
    	Pattern header=Pattern.compile("POINTS\\s\\d+\\sfloat");
    	Matcher m=header.matcher(buff);
		int vertexCount=0;
		int indexCount=0;
		Point3f[] points;
		int[] indices;
		if(m.find()){
			String head=buff.substring(m.start(),m.end());
			String[] vals=head.split("\\D+");
			if(vals.length>0){
				try {
					vertexCount=Integer.parseInt(vals[vals.length-1]);
				} catch(NumberFormatException e){
					System.err.println("CANNOT DETERMINE VERTEX COUNT");
					return null;
				}
			}
			points=new Point3f[vertexCount];
			System.out.println("vertex count is " + vertexCount);
			progress.updateValueImmed(added + (25 / total));
			System.out.println(m.end());
			System.out.println(buff.length());
			String[] strs=buff.substring(m.end(),buff.length()).split("\\s+",vertexCount*3+2);
			System.out.println(strs[0]);
			System.out.println(strs[1]);
			for(int i=1;i<strs.length-1;i+=3){
				try {
					Point3f p=new Point3f();
					p.x=Float.parseFloat(strs[i]);
					p.y=Float.parseFloat(strs[i+1]);
					p.z=Float.parseFloat(strs[i+2]);
					points[(i-1)/3]=p;
					//System.out.println(i/3+")"+p);
				} catch(NumberFormatException e){
					System.err.println("CANNOT FORMAT VERTS");
					return null;
				}
			}
		} else {
			return null;
		}
		
		progress.updateValueImmed(added + (50 / total));
		
		header=Pattern.compile("POLYGONS\\s+\\d+\\s+\\d+");
		m=header.matcher(buff);
		if(m.find()){
			String head=buff.substring(m.start(),m.end());
			String[] vals=head.split("\\D+");
			if(vals.length>1){
				try {
					indexCount=Integer.parseInt(vals[1]);
				} catch(NumberFormatException e){
					System.err.println("CANNOT DETERMINE INDEX COUNT");
					return null;
				}
			}
			indices=new int[indexCount*3];
			System.out.println("INDICES "+indexCount);
			String[] strs=buff.substring(m.end(),buff.length()).split("\\s+",indexCount*4+2);	
			int count=0;
			System.out.println(strs[0]);
			System.out.println(strs[1]);
			for(int i=1;i<strs.length-1;i+=4){			
				try {
					if(Integer.parseInt(strs[i]) != 3) {
						System.err.println("CANNOT FORMAT INDICES");
						return null;
					}
					indices[count++]=Integer.parseInt(strs[i+1]);
					indices[count++]=Integer.parseInt(strs[i+2]);
					indices[count++]=Integer.parseInt(strs[i+3]);
				} catch(NumberFormatException e){
					System.err.println("CANNOT FORMAT INDICES");
					return null;
				}
			}
		} else {
			return null;
		}
		
		header=Pattern.compile("POINT_DATA\\s+\\d+\\D+float\\s+\\d+\\nLOOKUP_TABLE\\s");
		m=header.matcher(buff);
		double[][] dat;
		int count=0;
		int dim=0;
		if(m.find()){
			String head=buff.substring(m.start(),m.end());
			String[] vals=head.split("\\D+");
			if(vals.length>0){
				try {
					count=Integer.parseInt(vals[1]);
					dim=Integer.parseInt(vals[2]);
				} catch(NumberFormatException e){
					System.err.println("CANNOT DETERMINE DATA POINTS");
					return null;
				}
			}
			dat=new double[count][dim];
			System.out.println("DATA POINTS "+count+" by "+dim);
			String[] strs=buff.substring(m.end(),buff.length()).split("\\s+",count*dim+2);
			int index=0;
			for(int i=1;i<strs.length&&index<count*dim;i++){
				try {		
					dat[index/dim][index%dim]=Double.parseDouble(strs[i]);
					index++;
				} catch(NumberFormatException e){
					System.err.println("CANNOT FORMAT DATA ["+strs[i]+"]");
					//return null;
				}
			}
			//System.out.println(index+" "+count);
			
			progress.updateValueImmed(added + (100 / total));
			
			kMesh=new ModelTriangleMesh(points,indices);
			kMesh.setVertexData(dat);
			kMesh.setName(fileName);

		} else { 
			kMesh=new ModelTriangleMesh(points,indices);
			kMesh.setName(fileName);

		}
    	return kMesh;
    }
    
    
    
    
    /**
     * 
     * @param kIn
     * @param progress
     * @param added
     * @param total
     * @param flag
     * @param fileName
     * @return
     * @throws IOException
     */
    public static ModelTriangleMesh loadVTKXMLMesh(String absPath, ViewJProgressBar progress, int added, int total, boolean flag, String fileName, String dir) throws IOException {
    	ModelTriangleMesh kMesh;

    	// When reading VTK surface file format, set the default direction to (1, 1, 1).  
    	direction[0] = 1;
        direction[1] = 1;
        direction[2] = 1;
        
        FileSurfaceVTKXML_J3D surfaceVTKXML = new FileSurfaceVTKXML_J3D(fileName, dir);
    	kMesh = surfaceVTKXML.readXMLSurface(absPath);

        return kMesh;
    }
    
    
    
    

    /**
     * Parses the VRML to see how many surfaces there are.
     *
     * @param   kIn  the file to parse
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException             DOCUMENT ME!
     * @throws  NoSuchElementException  DOCUMENT ME!
     */
    public static int parseVRMLMesh(RandomAccessFile kIn) throws IOException, NoSuchElementException {
        String str = kIn.readLine();

        str = kIn.readLine();
        str = kIn.readLine();

        StringTokenizer token = new StringTokenizer(str);

        token.nextToken();
        token.nextToken();
        token.nextToken();
        token.nextToken();

        return Integer.valueOf(token.nextToken()).intValue();
    }

    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format (text format).
     *
     * @param      kOut           the name of file to which the triangle mesh is saved
     * @param      akComponent    DOCUMENT ME!
     * @param      flip           if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                            To have proper orientations in surface file if flip is true flip y and z on reading.
     * @param      direction      1 or -1 for each axis
     * @param      startLocation  DOCUMENT ME!
     * @param      box            (dim-1)*resolution
     * @param      color          DOCUMENT ME!
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public static void saveAsPortableVRML(PrintWriter kOut, ModelTriangleMesh[] akComponent, boolean flip,
                                          int[] direction, float[] startLocation, float[] box, Color3f color)
            throws IOException {

        if (akComponent.length == 0) {
            return;
        }

        kOut.println("#VRML V2.0 utf8"); // object is ModelTriangleMesh
        kOut.println("#MIPAV");
        kOut.println("#Number of shapes = " + akComponent.length);
        for (int i = 0; i < akComponent.length; i++) {
            akComponent[i].saveAsPortableVRML(kOut, flip, direction, startLocation, box, color);
        }
        // kOut.close();
    }

    /**
     * Save an array of triangle meshes to a text file. The format for the file is
     *
     * <pre>
       int type;  // 0 = ModelTriangleMesh, 1 = ModelClodMesh
       int aCount;  // number of array elements
       array of 'aCount' meshes, each of the form:
       int vCount;  // number of vertices
       vertex[0].x vertex[0].y vertex[0].z;
       :
       normal[0].x normal[0].y normal[0].z;
       :
       int tCount;  // number of triangles
       index[0] index[1] index[2]
       :
       index[3*(tCount-1)] index[3*(tCount-1)+1] index[3*(tCount-1)+2]
     * </pre>
     *
     * @param      kName        the name of the file to which the components are saved
     * @param      akComponent  the array of mesh components to save
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public static void saveAsTextFile(String kName, ModelTriangleMesh[] akComponent) throws IOException {

        if (akComponent.length == 0) {
            return;
        }

        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println('0'); // objects are ModelTriangleMesh
        kOut.println(akComponent.length);

        for (int i = 0; i < akComponent.length; i++) {
            akComponent[i].print(kOut);
        }

        kOut.close();
    }

    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format (text format).
     *
     * @param      kName          the name of file to which the triangle mesh is saved
     * @param      akComponent    DOCUMENT ME!
     * @param      flip           if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                            To have proper orientations in surface file if flip is true flip y and z on reading.
     *                            param direction 1 or -1 for each axis param startLocation param box (dim-1)*resolution
     * @param      direction      DOCUMENT ME!
     * @param      startLocation  DOCUMENT ME!
     * @param      box            DOCUMENT ME!
     * @param      color          DOCUMENT ME!
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public static void saveAsVRML(String kName, ModelTriangleMesh[] akComponent, boolean flip, int[] direction,
                                  float[] startLocation, float[] box, Color3f color) throws IOException {

        if (akComponent.length == 0) {
            return;
        }

        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println("#VRML V2.0 utf8"); // object is ModelTriangleMesh
        kOut.println("#MIPAV");
        kOut.println("#Number of shapes = " + akComponent.length);

        for (int i = 0; i < akComponent.length; i++) {
            akComponent[i].saveAsVRML(kOut, flip, direction, startLocation, box, color);
        }

        kOut.close();
    }

    /**
     * The input 4x4 homogeneous matrix H is assumed to multiply vectors V as H*V where V = (x,y,z,1). Let the upper
     * left 3x3 block of H be A. Let the upper right 3x1 block of H be B. The last row is [0 0 0 1]. Let the upper 3x1
     * block of V be X. The lower 1x1 block is 1. The product H*V = A*X + B.
     *
     * @param  aafH  Affine transformation matrix.
     */
    public void affineTransform(double[][] aafH) {

        for (int iV = 0; iV < getVertexCount(); iV++) {
            getCoordinate(iV, m_kV0);

            // transform within image space
            float fX = (float) ((aafH[0][0] * m_kV0.x) + (aafH[0][1] * m_kV0.y) + (aafH[0][2] * m_kV0.z) + aafH[0][3]);
            float fY = (float) ((aafH[1][0] * m_kV0.x) + (aafH[1][1] * m_kV0.y) + (aafH[1][2] * m_kV0.z) + aafH[1][3]);
            float fZ = (float) ((aafH[2][0] * m_kV0.x) + (aafH[2][1] * m_kV0.y) + (aafH[2][2] * m_kV0.z) + aafH[2][3]);

            m_kV0.set(fX, fY, fZ);
            setCoordinate(iV, m_kV0);
        }
    }

    /**
     * Rotation R is 3x3, translation T is 3x1, and scale S is 3x1. The product with 3x1 vector X is Diag(S)*R*X + T
     * where Diag(S) is the diagonal matrix whose diagonal entries are the values of S. As a homogeneous matrix (see
     * notation above), A = diag(S)*R and B = T.
     *
     * @param  afRot    Affine rotation matrix.
     * @param  afTrn    Affine translation matrix.
     * @param  afScale  Affine scale matrix.
     */
    public void affineTransform(float[][] afRot, float[] afTrn, float[] afScale) {

        for (int iV = 0; iV < getVertexCount(); iV++) {
            getCoordinate(iV, m_kV0);

            // transform within image space
            float fX = (afScale[0] * ((afRot[0][0] * m_kV0.x) + (afRot[0][1] * m_kV0.y) + (afRot[0][2] * m_kV0.z))) +
                       afTrn[0];
            float fY = (afScale[1] * ((afRot[1][0] * m_kV0.x) + (afRot[1][1] * m_kV0.y) + (afRot[1][2] * m_kV0.z))) +
                       afTrn[1];
            float fZ = (afScale[2] * ((afRot[2][0] * m_kV0.x) + (afRot[2][1] * m_kV0.y) + (afRot[2][2] * m_kV0.z))) +
                       afTrn[2];

            m_kV0.set(fX, fY, fZ);
            setCoordinate(iV, m_kV0);
        }
    }

    /**
     * Calculate the surface mesh area. Each surface mesh is composed of triangles. Calculate the surface area from the
     * summation of the each triangle area. Based on the Area by Stokes' Theorem. Area(S) = 1/2 * Normal dot ( Sum from
     * i = 0 to n-1 of ( V1 cross V2) ) ==> Area(S) = 1/2 * ( Sum from i = 0 to n-1 of ( Normal dot ( V1 cross V2) ) )
     *
     * @return  float
     */
    public float area() {

        float fSum = 0.0f;

        for (int iT = 0; iT < getIndexCount(); /**/) {

            // get indices to triangle vertices
            int iV0 = getCoordinateIndex(iT++);
            int iV1 = getCoordinateIndex(iT++);
            int iV2 = getCoordinateIndex(iT++);

            // get vertices
            getCoordinate(iV0, m_kV0);
            getCoordinate(iV1, m_kV1);
            getCoordinate(iV2, m_kV2);

            // Area of a triangle = || P0 X P1 + P1 X P2 + P2 X P0 ||/2
            // Area = 0.5* (det1 + det2 + det3), where
            // det1, det2, and det3 are 3 by 3 determinants where
            // i, j, and k are unit normal vectors along the x, y, and z axes and
            // |     i        j       k      |
            // det1 = |  m_kV0.x  m_kV0.y  m_kV0.z  |
            // |  m_kV1.x  m_kV1.y  m_kV1.z  |
            // |     i        j       k      |
            // det2 = |  m_kV1.x  m_kV1.y  m_kV1.z  |
            // |  m_kV2.x  m_kV2.y  m_kV2.z  |
            // |     i        j       k      |
            // det3 = |  m_kV2.x  m_kV2.y  m_kV2.z  |
            // |  m_kV0.x  m_kV0.y  m_kV0.z  |
            // vx, vy, and vz are the x, y, and z components of the vector in the area expression
            double vx, vy, vz;
            float triangleArea;
            vx = ((m_kV0.y * m_kV1.z) - (m_kV1.y * m_kV0.z) + (m_kV1.y * m_kV2.z) - (m_kV2.y * m_kV1.z) +
                  (m_kV2.y * m_kV0.z) - (m_kV0.y * m_kV2.z));
            vy = ((m_kV0.z * m_kV1.x) - (m_kV1.z * m_kV0.x) + (m_kV1.z * m_kV2.x) - (m_kV2.z * m_kV1.x) +
                  (m_kV2.z * m_kV0.x) - (m_kV0.z * m_kV2.x));
            vz = ((m_kV0.x * m_kV1.y) - (m_kV1.x * m_kV0.y) + (m_kV1.x * m_kV2.y) - (m_kV2.x * m_kV1.y) +
                  (m_kV2.x * m_kV0.y) - (m_kV0.x * m_kV2.y));
            triangleArea = (float) (0.5 * Math.sqrt((vx * vx) + (vy * vy) + (vz * vz)));
            fSum += triangleArea;
            /* The surface mesh is generated by the triangular isosurfaces.
             * The surface area of the object is just the sum of the area of these triangular isosurfaces. Formula to
             * calculate the area of an arbitrary triangle, assume the length of the sides of an arbitrary triangle are
             * a, b and c. Area = Sqrt(p(p-a)(p-b)(p-c)), where p = 1/2(a + b + c) To find the length of the sides of a
             * triangle, use the vector formula as the vertices of the triangle are in 3D space. Let the vertices of one
             * side of the triangle are (x1, y1, z1) and (x2, y2, z2). The length of the side is equal to  side =
             * Sqrt((x2-x1)^2+(y2-y1)^2+(z2-z1)^2)
             */
            /*double a, b, c, p;
             * a = Math.sqrt((double)((m_kV1.x-m_kV0.x)*(m_kV1.x-m_kV0.x) + (m_kV1.y-m_kV0.y)*(m_kV1.y-m_kV0.y) +
             * (m_kV1.z-m_kV0.z)*(m_kV1.z-m_kV0.z))); b = Math.sqrt((double)((m_kV2.x-m_kV1.x)*(m_kV2.x-m_kV1.x) +
             * (m_kV2.y-m_kV1.y)*(m_kV2.y-m_kV1.y) + (m_kV2.z-m_kV1.z)*(m_kV2.z-m_kV1.z))); c =
             * Math.sqrt((double)((m_kV0.x-m_kV2.x)*(m_kV0.x-m_kV2.x) + (m_kV0.y-m_kV2.y)*(m_kV0.y-m_kV2.y) +
             * (m_kV0.z-m_kV2.z)*(m_kV0.z-m_kV2.z))); p = ( a + b + c ) / 2; float fProd =
             * (float)(Math.sqrt(p*(p-a)*(p-b)*(p-c)));
             *
             *fSum += fProd;*/
        }

        return fSum;
    }

    /**
     * Calculates and returns the center point of the Mesh:
     *
     * @return  DOCUMENT ME!
     */
    public Point3f center() {
        float xSum = 0f, ySum = 0f, zSum = 0f;
        int iNumVerts = getVertexCount();
        Point3f kVert = new Point3f();

        for (int iVert = 0; iVert < iNumVerts; iVert++) {
            getCoordinate(iVert, kVert);
            xSum += kVert.x;
            ySum += kVert.y;
            zSum += kVert.z;
        }

        kVert = null;

        return new Point3f(xSum / iNumVerts, ySum / iNumVerts, zSum / iNumVerts);
    }

    /**
     * A normal vector at a vertex is computed by averaging the triangle normals for all triangles sharing the vertex.
     * This routine computes all vertex normals.
     */
    public void computeNormals() {

        // maintain a running sum of triangle normals at each vertex
        Vector3f[] akSum = new Vector3f[getVertexCount()];
        int i;

        for (i = 0; i < getVertexCount(); i++) {
            akSum[i] = new Vector3f(0.0f, 0.0f, 0.0f);
        }

        for (i = 0; i < getIndexCount(); /**/) {

            // get indices to triangle vertices
            int iV0 = getCoordinateIndex(i++);
            int iV1 = getCoordinateIndex(i++);
            int iV2 = getCoordinateIndex(i++);

            // get vertices
            getCoordinate(iV0, m_kV0);
            getCoordinate(iV1, m_kV1);
            getCoordinate(iV2, m_kV2);

            // compute unit length triangle normal
            m_kE0.sub(m_kV1, m_kV0);
            m_kE1.sub(m_kV2, m_kV0);
            m_kN.cross(m_kE0, m_kE1);

            // normalize (set to zero if cross product is nearly zero)
            float fLength = m_kN.length();

            if (fLength > 1e-06) {
                m_kN.scale(1.0f / fLength);
            } else {
                m_kN.x = 0.0f;
                m_kN.y = 0.0f;
                m_kN.z = 0.0f;
            }

            // maintain the sum of normals at each vertex
            akSum[iV0].add(m_kN);
            akSum[iV1].add(m_kN);
            akSum[iV2].add(m_kN);
        }

        // The normal vector storage was used to accumulate the sum of
        // triangle normals.  Now these vectors must be rescaled to be
        // unit length.
        for (i = 0; i < getVertexCount(); i++) {
            akSum[i].normalize();
            setNormal(i, akSum[i]);
        }
    }

    /**
     * Construct the connected components of the triangle mesh.
     *
     * @return  An array of connected components of the triangle mesh. Each component has its own vertex and
     *          connectivity index arrays.
     */
    public ModelTriangleMesh[] getComponents() {

        // Compute the connected components.  The parameters passed to the
        // ModelSuraceTopology constructor are designed to help reduce memory
        // usage when the ModelTriangleMesh object is very large.  The test
        // case where we had problems was a mesh with 825K vertices and
        // 1.6M triangles.  These input parameters may be varied for debugging
        // and testing of other large surfaces.
        int iVCapacity = 2 * getVertexCount();
        float fVLoad = 0.9f;
        int iECapacity = 2 * getIndexCount();
        float fELoad = 0.9f;
        int iTCapacity = 2 * getIndexCount() / 3;
        float fTLoad = 0.9f;
        ModelSurfaceTopology kTopo = new ModelSurfaceTopology(iVCapacity, fVLoad, iECapacity, fELoad, iTCapacity,
                                                              fTLoad);

        int i;

        for (i = 0; i < getIndexCount(); /**/) {
            int iV0 = getCoordinateIndex(i++);
            int iV1 = getCoordinateIndex(i++);
            int iV2 = getCoordinateIndex(i++);

            kTopo.insertTriangle(iV0, iV1, iV2);
        }

        // DEBUG.  Uncomment this for information about the mesh.
        // ModelSurfaceTopology.Statistics kStatistics =
        // new ModelSurfaceTopology.Statistics(kTopo);
        // kStatistics.print();
        // END DEBUG.

        // decompose the connectivity array into disjoint subarrays

        // OLD CODE.  Compute all components at once.
        // Vector kIndex = new Vector();  // vector of <Integer>
        // int[] aiSortedConnect = kTopo.getComponents(kIndex);
        // int iNumComponents = kIndex.size()-1;
        // END OLD CODE.
        //
        // NEW CODE.  Compute components one at a time.
        Vector kIndex = new Vector(); // vector of <Integer>

        kIndex.add(new Integer(0));

        int iITotalQuantity = 3 * kTopo.getTriangleQuantity();
        int[] aiSortedConnect = new int[iITotalQuantity];
        int iNumComponents = 0;

        for (int iIQuantity = 0; iIQuantity < iITotalQuantity; /**/) {
            int iComponentIQuantity = kTopo.removeComponent(iIQuantity, aiSortedConnect);

            iIQuantity += iComponentIQuantity;
            kIndex.add(new Integer(iIQuantity));
            iNumComponents++;

            // For large data sets, this might help free up memory, but the
            // call does significantly slow down the program.
            // System.gc();
        }
        // END NEW CODE.

        ModelTriangleMesh[] akComponent = new ModelTriangleMesh[iNumComponents];
        boolean[] abVSet = new boolean[getIndexCount()];
        int[] aiMap = new int[getIndexCount()];

        for (i = 0; i < akComponent.length; i++) {

            // generate component connectivity
            int jMin = ((Integer) kIndex.get(i)).intValue();
            int jMax = ((Integer) kIndex.get(i + 1)).intValue();
            int iIQuantity = jMax - jMin;
            int[] aiSubConnect = new int[iIQuantity];
            int j, k;

            // Generate component vertex indices, store in a set.
            // TO DO:  Is there a Java equivalent of memset so that I can
            // set all array members to 'false' with one call?
            int iVQuantity = 0;

            for (j = 0; j < getIndexCount(); j++) {
                abVSet[j] = false;
            }

            for (j = 0; j < iIQuantity; j++) {
                k = aiSortedConnect[jMin + j];

                if (!abVSet[k]) {
                    abVSet[k] = true;
                    iVQuantity++;
                    aiSubConnect[j] = k;
                }
            }

            // Generate the component vertices.  This set of vertices is
            // indexed differently than the original, so the connectivity
            // array for the component must be remapped based on the vertex
            // index mapping.
            Point3f[] akSubVertex = new Point3f[iVQuantity];

            k = 0;

            for (int iVIndex = 0; iVIndex < getIndexCount(); iVIndex++) {

                if (abVSet[iVIndex]) {
                    akSubVertex[k] = new Point3f();
                    getCoordinate(iVIndex, akSubVertex[k]);
                    aiMap[iVIndex] = k++;
                }
            }
            // assert: k == iVQuantity

            for (j = 0; j < iIQuantity; j++) {
                aiSubConnect[j] = aiMap[aiSortedConnect[jMin + j]];
                // assert: aiSubConnect[j] < iVQuantity
            }

            akComponent[i] = new ModelTriangleMesh(akSubVertex, aiSubConnect);
        }

        return akComponent;
    }

    /**
     * Construct the connected components of the triangle mesh.
     */
    public void getConsistentComponents() {

        // Compute the connected components.  The parameters passed to the
        // ModelSuraceTopology constructor are designed to help reduce memory
        // usage when the ModelTriangleMesh object is very large.  The test
        // case where we had problems was a mesh with 825K vertices and
        // 1.6M triangles.  These input parameters may be varied for debugging
        // and testing of other large surfaces.

        int iVCapacity = 2 * getVertexCount();
        float fVLoad = 0.9f;
        int iECapacity = 2 * getIndexCount();
        float fELoad = 0.9f;
        int iTCapacity = 2 * getIndexCount() / 3;
        float fTLoad = 0.9f;
        ModelSurfaceTopology kTopo = new ModelSurfaceTopology(iVCapacity, fVLoad, iECapacity, fELoad, iTCapacity,
                                                              fTLoad);

        int i;

        for (i = 0; i < getIndexCount();) {
            int iV0 = getCoordinateIndex(i++);
            int iV1 = getCoordinateIndex(i++);
            int iV2 = getCoordinateIndex(i++);

            kTopo.insertTriangle(iV0, iV1, iV2);
        }

        HashMap triMap = kTopo.getTriangleMap();
        ModelSurfaceTopology.Triangle kT;
        Iterator kTIter = triMap.keySet().iterator();

        i = 0;

        while (kTIter.hasNext()) {
            kT = (ModelSurfaceTopology.Triangle) kTIter.next();
            setCoordinateIndex(i++, kT.m_iV0);
            setCoordinateIndex(i++, kT.m_iV1);
            setCoordinateIndex(i++, kT.m_iV2);
        }

    }

    /**
     * Support for regenerating an ModelTriangleMesh from an ModelClodMesh whenever the level of detail is changed for
     * the ModelClodMesh. If the mesh was generated from an ModelClodMesh, the generator member refers to that
     * ModelClodMesh object. If the mesh was not generated from an ModelClodMesh, the generator member is null.
     *
     * @return  the generator of this mesh
     */
    public Object getGenerator() {
        return m_kGenerator;
    }

    /**
     * Make a copy of the connectivity array of a triangle mesh.
     *
     * @return  A copy of the array of connectivity indices of the triangle mesh.
     */
    public int[] getIndexCopy() {
        int[] aiConnect = new int[getIndexCount()];

        getCoordinateIndices(0, aiConnect);

        return aiConnect;
    }

    /**
     * Make a copy of the normals of a triangle mesh.
     *
     * @return  A copy of the array of normals of the triangle mesh.
     */
    public Vector3f[] getNormalCopy() {
        Vector3f[] akNormal = new Vector3f[getVertexCount()];

        for (int i = 0; i < getVertexCount(); i++) {
            akNormal[i] = new Vector3f();
        }

        getNormals(0, akNormal);

        return akNormal;
    }

    /**
     * Make a copy of the vertices of a triangle mesh.
     *
     * @return  A copy of the array of vertices of the triangle mesh.
     */
    public Point3f[] getVertexCopy() {
        Point3f[] akVertex = new Point3f[getVertexCount()];

        for (int i = 0; i < getVertexCount(); i++) {
            akVertex[i] = new Point3f();
        }

        getCoordinates(0, akVertex);

        return akVertex;
    }


    /**
     * Make a copy of the vertices of a triangle mesh.
     *
     * @return  A copy of the array of vertices of the triangle mesh.
     */
    public WildMagic.LibFoundation.Mathematics.Vector3f[] getVertexCopyAsVector3f() {
    	WildMagic.LibFoundation.Mathematics.Vector3f[] akVertex = 
    		new WildMagic.LibFoundation.Mathematics.Vector3f[getVertexCount()];

    	Point3f kVertex = new Point3f();
        for (int i = 0; i < getVertexCount(); i++) {
        	getCoordinate( i, kVertex );
            akVertex[i] = new WildMagic.LibFoundation.Mathematics.Vector3f( kVertex.x, kVertex.y, kVertex.z );
        }
        return akVertex;
    }

    /**
     * Make a copy of the colors of a triangle mesh.
     *
     * @return  A copy of the array of colors of the triangle mesh.
     */
    public Color4f[] getColorCopy() {
        Color4f[] akColor = new Color4f[getVertexCount()];

        for (int i = 0; i < getVertexCount(); i++) {
            akColor[i] = new Color4f();
        }

        getColors(0, akColor);

        return akColor;
    }

    /**
     * Copies the components of a ModelTriangleMesh into the parameters. More
     * efficient than copying the components individually.
     * @param akVertex the array of coordinates to copy into.
     * @param akNormal the array of normals to copy into.
     * @param akColor the array of colors to copy into.
     * @param akTexCoord the array of texture coordinates to copy into.
     */
    public void getCopies( Point3f[] akVertex, Vector3f[] akNormal, Color4f[] akColor, TexCoord3f[] akTexCoord  )
    {
        for (int i = 0; i < getVertexCount(); i++) {
            akVertex[i] = new Point3f();
            akNormal[i] = new Vector3f();
            akColor[i] = new Color4f();
            akTexCoord[i] = new TexCoord3f();
        }
        getCoordinates(0, akVertex);
        getNormals(0, akNormal);
        getColors(0, akColor);
        getTextureCoordinates(0, 0, akTexCoord);
    }

    /**
     * Make a copy of the texture coordinates of a triangle mesh.
     *
     * @return  A copy of the array of texture coordinates of the triangle mesh.
     */
    public TexCoord3f[] getTexCoordCopy() {
        TexCoord3f[] akTexCoords = new TexCoord3f[getVertexCount()];

        for (int i = 0; i < getVertexCount(); i++) {
            akTexCoords[i] = new TexCoord3f();
        }

        getTextureCoordinates(0, 0, akTexCoords);

        return akTexCoords;
    }


    /**
     * Used for fast color update while painting the triangle mesh, stores the
     * color update information for later loading into the surface.
     * @param index the vertex index to set the color for
     * @param kColor the new color
     */
    public void setColorDelay( int index, Color4f kColor )
    {
        m_kColors[index].x = kColor.x;
        m_kColors[index].y = kColor.y;
        m_kColors[index].z = kColor.z;
        m_kColors[index].w = kColor.w;
    }

    /**
     * Sets the color and stores the color update information in a local copy.
     * @param index the vertex index to set the color for
     * @param kColor the new color
     */
    public void setColor( int index, Color4f kColor )
    {
        m_kColors[index].x = kColor.x;
        m_kColors[index].y = kColor.y;
        m_kColors[index].z = kColor.z;
        m_kColors[index].w = kColor.w;
        super.setColor( index, kColor );
    }

    /**
     * Gets the stored local color.
     * @param index the vertex index to get the color for.
     * @param kColor the stored local color.
     */
    public void getColorLocal( int index, Color4f kColor )
    {
        kColor.x = m_kColors[index].x;
        kColor.y = m_kColors[index].y;
        kColor.z = m_kColors[index].z;
        kColor.w = m_kColors[index].w;
    }

    /**
     * Used for fast color update while painting. After paint operations, set
     * the color of the triangle mesh with the stored colors.
     */
    public void setColorUpdate()
    {
        setColors( 0, m_kColors );
    }


    /**
     * Save the triangle mesh to a binary file. The format for the file is
     *
     * <pre>
       int type;  // 0 = ModelTriangleMesh, 1 = ModelClodMesh
       int aCount;  // 1, write the entire mesh as a single component
       int vCount;  // number of vertices
       Point3f vertices[vCount];
       Point3f normals[vCount];
       int iCount;  // number of indices in the connectivity array
       int indices[iCount];
     * </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kName              the name of the file to which the triangle mesh is saved
     * @param      flip               if the y and z axes should be flipped - true in extract and in save of
     *                                JDialogSurface To have proper orientations in surface file if flip is true flip y
     *                                and z on reading.
     * @param      direction          equal 1 or -1 for each axis
     * @param      startLocation      DOCUMENT ME!
     * @param      box                (dim-1)*res
     * @param      inverseDicomMatrix  DOCUMENT ME!
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public void save(String kName, boolean flip, int[] direction, float[] startLocation, float[] box,
    				TransMatrix inverseDicomMatrix) throws IOException {


        int i = kName.lastIndexOf('.');
        String extension;
        Color3f color = new Color3f(Color.blue);
        String surfaceFileName;

        if ((i > 0) && (i < (kName.length() - 1))) {
        	surfaceFileName = kName.substring(0, i);
            extension = kName.substring(i + 1).toLowerCase();
            if ( extension.equals("stlb")) {
            	saveAsSTLBinaryFile(kName);
            } else if ( extension.equals("stla")) {
            	saveAsSTLAsciiFile(kName);
            } else if ( extension.equals("smf")) {
            	saveAsSMFAsciiFile(kName);
            } else if (extension.equals("txt")) {
                saveAsTextFile(kName);
            } else if (extension.equals("ply")) {
                saveAsPlyFile(kName);
            } else if(extension.equals("vtk")) {
            	saveAsVTKLegacy(kName);
            } else if (extension.equals("wrl")) {
                PrintWriter kOut = new PrintWriter(new FileWriter(kName));
                saveAsPortableVRML(kOut, flip, 1, direction, startLocation, box, color);
                kOut.close();
            } else if (extension.equals("sur")) {
                RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");
                kOut.writeInt(0); // object is ModelTriangleMesh
                kOut.writeInt(1); // one component
                save(kOut, flip, direction, startLocation, box, inverseDicomMatrix, null);
                kOut.close();
            } else if (extension.equals("xml")) {
            	// saveAsXML( kName, direction, startLocation, box);
            	saveXMLHeader(kName);
            	surfaceFileName = surfaceFileName + ".sur";
            	RandomAccessFile kOut = new RandomAccessFile(new File(surfaceFileName), "rw");
                kOut.writeInt(0); // object is ModelTriangleMesh
                kOut.writeInt(1); // one component
                save(kOut, flip, direction, startLocation, box, inverseDicomMatrix, null);
                kOut.close();
            } else if (extension.equals("vtp")) {
            	saveAsVTKXML(kName);
            }
        }
        /*
         * if ( Preferences.isDebug() ) { saveAsTextFile( JDialogBase.makeImageName( kName, "_sur.txt" ) );} */
    }

    public void saveXMLHeader(String kName) {
    	try {
    	   FileSurfaceRefXML_J3D kSurfaceXML = new FileSurfaceRefXML_J3D(null, null);
    	   Material material = new Material();
       	// For the material, opacity, LOD, save as default values. 
           kSurfaceXML.writeXMLsurface(kName, material, 0f, 100);
    	} catch (IOException kError) { 
    		kError.printStackTrace();
    	}
    }
    
    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format (text format).
     *
     * @param      kOut            the name of file to which the triangle mesh is saved
     * @param      flip            if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                             To have proper orientations in surface file if flip is true flip y and z on reading.
     * @param      numberOfShapes  DOCUMENT ME!
     * @param      direction       1 or -1 for each axis
     * @param      startLocation   DOCUMENT ME!
     * @param      box             (dimension-1)*resolution
     * @param      color           DOCUMENT ME!
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public void saveAsPortableVRML(PrintWriter kOut, boolean flip, int numberOfShapes, int[] direction,
                                   float[] startLocation, float[] box, Color3f color) throws IOException {

        kOut.println("#VRML V2.0 utf8"); // object is ModelTriangleMesh
        kOut.println("#MIPAV");
        kOut.println("#Number of shapes = " + numberOfShapes);
        saveAsPortableVRML(kOut, flip, direction, startLocation, box, color);
        // kOut.close();
    }
    
    
    
    /**
     * Saves the triangle mesh to VTK Legacy format
     * @param kName
     * @throws IOException
     */
    public void saveAsVTKLegacy(String kName) throws IOException {
    	PrintWriter kOut = new PrintWriter(new FileWriter(kName));
    	int pointCount = getVertexCount();
		int indexCount = getIndexCount();
		kOut.println("# vtk DataFile Version 2.0");
		kOut.println(getName());
		kOut.println("ASCII");
		kOut.println("DATASET POLYDATA");
		kOut.println("POINTS "+pointCount+" float");
		Point3f p=new Point3f();
		String tmp;
		for(int i=0;i<pointCount;i++){
			getCoordinate(i,p);
			tmp=String.format("%.5f %.5f %.5f\n", p.x,p.y,p.z);
			kOut.print(tmp);
		}
		kOut.println("POLYGONS "+indexCount/3+" "+(4*indexCount/3));
		for(int i=0;i<indexCount;i+=3){
			kOut.println(3+" "+getCoordinateIndex(i)+" "+getCoordinateIndex(i+1)+" "+getCoordinateIndex(i+2));
		}
		
		double[][] scalars=getVertexData();
		if(scalars!=null&&scalars.length>0&&scalars[0].length>0){
			kOut.print("POINT_DATA "+scalars.length+"\n"
					+"SCALARS EmbedVertex float "+scalars[0].length+"\n"
					+"LOOKUP_TABLE default\n");
			for(int i=0;i<scalars.length;i++){
				for(int j=0;j<scalars[i].length;j++){
					kOut.print(scalars[i][j]+" ");
				} 
				kOut.println();
			}
		}
		
		double[][] cells=getCellData();
		if(cells!=null&&cells.length>0&&cells[0].length>0){
			kOut.print("CELL_DATA "+cells.length+"\n"
					+"SCALARS EmbedCell float "+cells[0].length+"\n"
					+"LOOKUP_TABLE default\n");
			for(int i=0;i<cells.length;i++){
				for(int j=0;j<cells[i].length;j++){
					kOut.print(cells[i][j]+" ");
				} 
				kOut.println();
			}
		}
		
		kOut.close();
	
    }
    
    
    
    /**
     * saves thr triangle mesh to VTK XML format
     * @param kName
     * @throws IOException
     */
    public void saveAsVTKXML(String fileName) throws IOException{
    	try {
        	FileSurfaceVTKXML_J3D surfaceVTKXML = new FileSurfaceVTKXML_J3D(null, null);
        	surfaceVTKXML.writeXMLsurface(fileName, this);
        } catch (IOException kError) { }
    }
    

    /**
     * Save the triangle mesh to a text file. The format for the file is
     *
     * <pre>
       int type;  // 0 = ModelTriangleMesh, 1 = ModelClodMesh, 2 = ModelQuadMesh
       int aCount;  // 1, write the entire mesh as a single component
       int vCount;  // number of vertices
       vertex[0].x vertex[0].y vertex[0].z;
       :
       normal[0].x normal[0].y normal[0].z;
       :
       int tCount;  // number of triangles
       index[0] index[1] index[2]
       :
       index[3*(tCount-1)] index[3*(tCount-1)+1] index[3*(tCount-1)+2]
     * </pre>
     *
     * @param      kName  the name of file to which the triangle mesh is saved
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public void saveAsTextFile(String kName) throws IOException {
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println('0'); // object is ModelTriangleMesh
        kOut.println('1'); // one component
        print(kOut);
        kOut.close();
    }
    
    
    
    public void saveAsPlyFile(String kName) throws IOException {
    	Point3f kVertex = new Point3f();
        int iTriangleCount = getIndexCount() / 3;
        int iVertexCount = getVertexCount();
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));
        
        kOut.println("ply"); // object is ModelTriangleMesh
        kOut.println("format ascii 1.0");
        kOut.println("element vertex " + iVertexCount);
        kOut.println("property float32 x");
        kOut.println("property float32 y");
        kOut.println("property float32 z");
        kOut.println("element face " + iTriangleCount);
        kOut.println("property list uint8 int32 vertex_indices");
        kOut.println("end_header");

        int i;

        for (i = 0; i < iVertexCount; i++) {
            getCoordinate(i, kVertex);
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
        }
        

        for (i = 0; i < iTriangleCount; i++) {
        	kOut.print('3');
        	kOut.print(' ');
            kOut.print(getCoordinateIndex(3 * i));
            kOut.print(' ');
            kOut.print(getCoordinateIndex((3 * i) + 1));
            kOut.print(' ');
            kOut.println(getCoordinateIndex((3 * i) + 2));
        }
        
        kOut.close();
    }
    
    
    
    /**
     * Sate the SMF ACII file format. 
     * @param kName  file name
     * @throws IOException
     */
    public void saveAsSMFAsciiFile(String kName) throws IOException {
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));
        printSMFAscii(kOut);
        kOut.close();
    }
    
    /**
     * Sate the STIL ACII file format. 
     * @param kName  file name
     * @throws IOException
     */
    public void saveAsSTLAsciiFile(String kName) throws IOException {
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));
        printSTLAscii(kOut);
        kOut.close();
    }
    
    /**
     * Sate the STIL Binary file format. 
     * @param kName  file name
     * @throws IOException
     */
    public void saveAsSTLBinaryFile(String kName) throws IOException {
    	RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");;
        printSTLBinary(kOut);
        kOut.close();
    }
    
    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format (text format).
     *
     * @param      kName           the name of file to which the triangle mesh is saved
     * @param      flip            if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                             To have proper orientations in surface file if flip is true flip y and z on reading.
     * @param      numberOfShapes  DOCUMENT ME!
     * @param      direction       1 or -1 for each axis
     * @param      startLocation   DOCUMENT ME!
     * @param      box             (dimension-1)*resolution
     * @param      color           DOCUMENT ME!
     *
     * @exception  IOException  if the specified file could not be opened for writing
     */
    public void saveAsVRML(String kName, boolean flip, int numberOfShapes, int[] direction, float[] startLocation,
                           float[] box, Color3f color) throws IOException {
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println("#VRML V2.0 utf8"); // object is ModelTriangleMesh
        kOut.println("#MIPAV");
        kOut.println("#Number of shapes = " + numberOfShapes);
        saveAsVRML(kOut, flip, direction, startLocation, box, color);
        kOut.close();
    }

    /**
     * Support for regenerating an ModelTriangleMesh from an ModelClodMesh whenever the level of detail is changed for
     * the ModelClodMesh. If the mesh was generated from an ModelClodMesh, the generator member refers to that
     * ModelClodMesh object. If the mesh was not generated from an ModelClodMesh, the generator member is null.
     *
     * @param  kGenerator  the generator of this mesh
     */
    public void setGenerator(Object kGenerator) {
        m_kGenerator = kGenerator;
    }

    /**
     * Replace the verticies with a new set.
     *
     * @param  akVertex  DOCUMENT ME!
     */
    public void setVerticies(Point3f[] akVertex) {
        setCoordinates(0, akVertex);
        computeNormals();
    }

    /**
     * Smooth mesh. The formula can be found in "The Visualization Toolkit" by Will Schoeder, Ken Martin, and Bill
     * Lorensen, p. 389. Mesh smoothing moves the verticies of the mesh closer to an average of the points. Each point
     * is moved so that it is the average of the points around it. The formula is:
     *
     * <pre>
            xi+1 = xi + (alpha * (Sum from j=0 to n of {xj - xi}))
     *  </pre>
     *
     * where xi+1 is the new point, xi is the orginal point, and the xjs are points that are connected to xi. Alpha is
     * some smoothing factor between .01 and .10. This formula is run for a number of iterations to obtain a smooth
     * mesh. Usually the iterations will be between 50 and 100.
     *
     * @param  iteration      Number of times to run smoothing formula on data set.
     * @param  alpha          Smoothing factor.
     * @param  volumeLimit    if true stop iterations when the present volume is volumePercent or more different from
     *                        the initial
     * @param  volumePercent  percentage from initial volume for stopping iterations
     * @param  pVisible       if true display progress bar
     */
    public void smoothMesh(int iteration, float alpha, boolean volumeLimit, float volumePercent, boolean pVisible) {
        int i;
        int num;
        float initialVolume = 0.0f;
        float presentVolume;
        boolean noVolumeLimit = true;
        float presentPercent;
        ViewJProgressBar progressBar = null;

        if (connections == null) {
            buildConnections();
        }

        Point3f[] m_kVA = new Point3f[getVertexCount()];

        for (int n = 0; n < m_kVA.length; n++) {
            m_kVA[n] = new Point3f();
        }

        if (volumeLimit) {
            initialVolume = volume();
            // System.out.println("ModelTriangleMesh.smoothMesh -- Initial volume " + initialVolume);
        }

        if (pVisible) {
            progressBar = new ViewJProgressBar("smoothMesh", "iteration 1", 0, 100, false, null, null);

            int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
            int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;

            progressBar.setLocation(xScreen / 2, yScreen / 2);
            progressBar.setVisible(true);
        }

        // repeat for however many iterations
        for (int k = 0; (k < iteration) && noVolumeLimit; k++) {

            if (progressBar != null) {
                progressBar.setMessage("iteration " + String.valueOf(k + 1));
                progressBar.updateValue((100 * (k + 1)) / iteration, false);
            }

            // for each coordinate vertex
            // System.out.println("ModelTriangleMesh.smoothMesh -- Iteration = " + k);
            for (i = 0; i < getVertexCount(); i++) {

                // m_kV0 is a sum; m_kV1 is a temporary variable
                m_kV0.x = 0f;
                m_kV0.y = 0f;
                m_kV0.z = 0f;
                num = 0;
                getCoordinate(i, m_kV3);

                // get all the verticies that are connected to this one (at i)
                for (Iterator iter = connections[i].iterator(); iter.hasNext();) {
                    int index = ((Integer) iter.next()).intValue();

                    // Sum of (xj - xi) where j ranges over all the points connected to xi
                    // xj = m_kV2; xi = m_kV3
                    getCoordinate(index, m_kV2);
                    m_kV0.x += (m_kV2.x - m_kV3.x);
                    m_kV0.y += (m_kV2.y - m_kV3.y);
                    m_kV0.z += (m_kV2.z - m_kV3.z);
                    num++;
                }
                // xi+1 = xi + (alpha)*(sum of(points xi is connected to - xi))

                if (num > 1) {
                    m_kV0.x /= num;
                    m_kV0.y /= num;
                    m_kV0.z /= num;
                }

                m_kVA[i].x = m_kV3.x + (alpha * m_kV0.x);
                m_kVA[i].y = m_kV3.y + (alpha * m_kV0.y);
                m_kVA[i].z = m_kV3.z + (alpha * m_kV0.z);
            }

            for (i = 0; i < getVertexCount(); i++) {
                setCoordinate(i, m_kVA[i]);
            }

            if (volumeLimit) {
                presentVolume = volume();

                // System.out.println("ModelTriangleMesh.smoothMesh -- present volume " + presentVolume);
                presentPercent = Math.abs(100.0f * (presentVolume - initialVolume) / initialVolume);

                if (presentPercent >= volumePercent) {
                    noVolumeLimit = false;
                }
            } // if (doVolumeLimit)
        }

        if (progressBar != null) {
            progressBar.dispose();
        }

        System.gc();
    }

    /**
     * Smooth mesh. This method smoothes without shrinking by 2 Gaussian smoothing steps. First, a Gaussian smoothing is
     * performed with a positive scale factor lambda. Second, a Gaussian smoothing is performed with a negative scale
     * factor mu, which is greater in magnitude than lambda. To produce a significant smoothing, these steps must be
     * repeated a number of times. 3 references for this smoothing: 1.) Curve and Surface Smoothing Without Shrinkage by
     * Gabriel Taubin, Technical Report RC-19536, IBM Research, April, 1994. (also in Proceedings, Fifth International
     * Conference on Computer Vision, pages 852-857, June, 1995). 2.) A Signal Processing Approach to Fair Surface
     * Design by Gabriel Taubin, Computer Graphics, pages 351-358, August, 1995 (Proceedings SIGGRAPH '95). 3.) Optimal
     * Surface Smoothing as Filter Design by Gabriel Taubin, Tong Zhang, and Gene Golub, IBM Research Report RC-20404
     * (#90237). Usually the iterations will be between 30 and 100.
     *
     * @param  iteration  Number of times to run smoothing formula on data set.
     * @param  lambda     positive scale factor
     * @param  mu         negative scale factor
     * @param  pVisible   if true display progress bar Require: 0 < lambda < -mu (1/lambda) + (1/mu) < 2
     */
    public void smoothThree(int iteration, float lambda, float mu, boolean pVisible) {
        int i;
        int num;
        ViewJProgressBar progressBar = null;

        if (connections == null) {
            buildConnections();
        }

        Point3f[] m_kVA = new Point3f[getVertexCount()];

        for (int n = 0; n < m_kVA.length; n++) {
            m_kVA[n] = new Point3f();
        }

        if (pVisible) {
            progressBar = new ViewJProgressBar("smoothMesh", "iteration 1", 0, 100, false, null, null);

            int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
            int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;

            progressBar.setLocation(xScreen / 2, yScreen / 2);
            progressBar.setVisible(true);
        }

        // repeat for however many iterations
        for (int k = 0; (k < iteration); k++) {

            if (progressBar != null) {
                progressBar.setMessage("iteration " + String.valueOf(k + 1));
                progressBar.updateValue((100 * (k + 1)) / iteration, false);
            }

            // for each coordinate vertex
            // System.out.println("ModelTriangleMesh.smoothMesh -- Iteration = " + k);
            for (i = 0; i < getVertexCount(); i++) {

                // m_kV0 is a sum; m_kV1 is a temporary variable
                m_kV0.x = 0f;
                m_kV0.y = 0f;
                m_kV0.z = 0f;
                num = 0;
                getCoordinate(i, m_kV3);

                // get all the verticies that are connected to this one (at i)
                for (Iterator iter = connections[i].iterator(); iter.hasNext();) {
                    int index = ((Integer) iter.next()).intValue();

                    // Sum of (xj - xi) where j ranges over all the points connected to xi
                    // xj = m_kV2; xi = m_kV3
                    getCoordinate(index, m_kV2);
                    m_kV0.x += (m_kV2.x - m_kV3.x);
                    m_kV0.y += (m_kV2.y - m_kV3.y);
                    m_kV0.z += (m_kV2.z - m_kV3.z);
                    num++;
                }

                if (num > 1) {
                    m_kV0.x /= num;
                    m_kV0.y /= num;
                    m_kV0.z /= num;
                }
                // xi+1 = xi + lambda*(average of sum of(points xi is connected to - xi))

                m_kVA[i].x = m_kV3.x + (lambda * m_kV0.x);
                m_kVA[i].y = m_kV3.y + (lambda * m_kV0.y);
                m_kVA[i].z = m_kV3.z + (lambda * m_kV0.z);
            } // for (i = 0; i < getVertexCount(); i++)

            for (i = 0; i < getVertexCount(); i++) {
                setCoordinate(i, m_kVA[i]);
            }

            for (i = 0; i < getVertexCount(); i++) {

                // m_kV0 is a sum; m_kV1 is a temporary variable
                m_kV0.x = 0f;
                m_kV0.y = 0f;
                m_kV0.z = 0f;
                num = 0;
                getCoordinate(i, m_kV3);

                // get all the verticies that are connected to this one (at i)
                for (Iterator iter = connections[i].iterator(); iter.hasNext();) {
                    int index = ((Integer) iter.next()).intValue();

                    // Sum of (xj - xi) where j ranges over all the points connected to xi
                    // xj = m_kV2; xi = m_kV3
                    getCoordinate(index, m_kV2);
                    m_kV0.x += (m_kV2.x - m_kV3.x);
                    m_kV0.y += (m_kV2.y - m_kV3.y);
                    m_kV0.z += (m_kV2.z - m_kV3.z);
                    num++;
                }

                if (num > 1) {
                    m_kV0.x /= num;
                    m_kV0.y /= num;
                    m_kV0.z /= num;
                }
                // xi+1 = xi + mu*(average of sum of(points xi is connected to - xi))

                m_kVA[i].x = m_kV3.x + (mu * m_kV0.x);
                m_kVA[i].y = m_kV3.y + (mu * m_kV0.y);
                m_kVA[i].z = m_kV3.z + (mu * m_kV0.z);
            } // for (i = 0; i < getVertexCount(); i++)

            for (i = 0; i < getVertexCount(); i++) {
                setCoordinate(i, m_kVA[i]);
            }

        } // for (int k = 0; (k<iteration); k++)

        if (progressBar != null) {
            progressBar.dispose();
        }

        System.gc();
    }

    /**
     * Derived from the first 2 of the 3 components of AlgorithmBrainExtraction Note that m_fStiffness does not increase
     * and then decrease as in AlgorithmBrainExtraction but instead remains constant. This is because the change in
     * m_fStiffness only applies to the third component of AlgorithmBrainExtraction which uses image intensities.
     *
     * @param  iterations     DOCUMENT ME!
     * @param  m_fStiffness   DOCUMENT ME!
     * @param  volumeLimit    if true stop iterations when the present volume is volumePercent or more different from
     *                        the initial
     * @param  volumePercent  percentage from initial volume for stopping iterations
     * @param  pVisible       if true display progress bar
     */
    public void smoothTwo(int iterations, float m_fStiffness, boolean volumeLimit, float volumePercent,
                          boolean pVisible) {
        int i;
        float initialVolume = 0.0f;
        float presentVolume;
        boolean noVolumeLimit = true;
        float presentPercent;
        ViewJProgressBar progressBar = null;

        this.m_fStiffness = m_fStiffness;
        m_akVMean = new Vector3f[getVertexCount()];
        m_akVNormal = new Vector3f[getVertexCount()];
        m_akSNormal = new Vector3f[getVertexCount()];
        m_akSTangent = new Vector3f[getVertexCount()];
        m_afCurvature = new float[getVertexCount()];
        m_akAdjacent = new UnorderedSetInt[getVertexCount()];

        // System.out.println("I am entering smoothTwo");
        for (i = 0; i < getVertexCount(); i++) {
            m_akVMean[i] = new Vector3f();
            m_akVNormal[i] = new Vector3f();
            m_akSNormal[i] = new Vector3f();
            m_akSTangent[i] = new Vector3f();
            m_akAdjacent[i] = new UnorderedSetInt(6, 1);
        }

        m_kEMap = new HashMap();
        kInvalid = new Integer(-1);

        for (int iT = 0; iT < getIndexCount();) {

            // get the vertices of the triangle
            int iP0 = getCoordinateIndex(iT++);
            int iP1 = getCoordinateIndex(iT++);
            int iP2 = getCoordinateIndex(iT++);

            m_kEMap.put(new Edge(iP0, iP1), kInvalid);
            m_kEMap.put(new Edge(iP1, iP2), kInvalid);
            m_kEMap.put(new Edge(iP2, iP0), kInvalid);

            m_akAdjacent[iP0].insert(iP1);
            m_akAdjacent[iP0].insert(iP2);
            m_akAdjacent[iP1].insert(iP0);
            m_akAdjacent[iP1].insert(iP2);
            m_akAdjacent[iP2].insert(iP0);
            m_akAdjacent[iP2].insert(iP1);
        }

        if (volumeLimit) {
            initialVolume = volume();
        }

        if (pVisible) {
            progressBar = new ViewJProgressBar("smoothTwo", "iteration 1", 0, 100, false, null, null);

            int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
            int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;

            progressBar.setLocation(xScreen / 2, yScreen / 2);
            progressBar.setVisible(true);
        }

        for (i = 1; (i <= iterations) && noVolumeLimit; i++) {

            if (progressBar != null) {
                progressBar.setMessage("iteration " + String.valueOf(i));
                progressBar.updateValue((100 * i) / iterations, false);
            }

            updateMesh();

            if (volumeLimit) {
                presentVolume = volume();
                presentPercent = Math.abs(100.0f * (presentVolume - initialVolume) / initialVolume);

                if (presentPercent >= volumePercent) {
                    noVolumeLimit = false;
                }
            } // if (doVolumeLimit)
        }

        if (progressBar != null) {
            progressBar.dispose();
        }
    }

    /**
     * Calculates volume of triangle mesh. The mesh consists of triangle faces and encloses a bounded region. Face j, 0
     * <= j <= n-1 has verticies P0, P1, and P2. The order of the verticies is counterclockwise as you view the face
     * from outside the bounded region. The mesh is closed and manifold in the sense that each edge is shared by exactly
     * two triangles. The volume of the bounded region is:<br>
     *
     * <pre>
             V = 1/6 (Sum from j=0 to n-1 of {P0 dot P1 cross P2})
     *   </pre>
     *
     * The terms of the summation can be positive, negative, or zero. The term is positive if the face is
     * counterclockwise when viewed from the zero vector, or zero if the face appears to be a line segment when viewed
     * from the zero vector. NOTICE THAT THERE ARE 2 DIFFERENT DEFINITIONS OF COUNTERCLOCKWISE, COUNTERCLOCKWISE AS
     * VIEWED FROM OUTSIDE THE BOUNDED REGION AND COUNTERCLOCKWISE AS VIEWED FROM THE ZERO VECTOR.
     *
     * <p>A 3D image on a rectangular lattice contains points (i0, i1, i2) where 0 <= ik < Bk for specified dimension
     * bounds Bk. These are just indicies. The actual physical measurements are provided by scaling factors Dk > 0. For
     * example, a 256x256x256 MRI has B0 = B1 = B2 = 256. If each voxel is 1 mm in x, 1 mm in y, and 5 mm in z, then D0
     * = D1 = 1 and D2 = 5. The 3D image encloses a rectangular region [0,C0] x [0,C1] x [0,C2] in <em>physical
     * space</em> where Ck = Dk*Bk. In the example, C0 = D0*B0 = 256 mm in x, C1 = D1*B1 = 256 mm in y, and C2 = D2*B2 =
     * 1280 mm in z. Volume calculations are required to use physical measurements. In the example, volume will be in
     * cubic millimeters.</p>
     *
     * <p>The surface extraction is performed by mapping [0,C0] x [0,C1] x [0,C2] into [-1,1] x [-1,1] x [-1,1] using
     * uniform scaling. This is done to keep the floating point values within order 1 to avoid the floating point errors
     * that occur if you were to use the index values themselves. The topology of a level surface is invariant under any
     * scaling (not just uniform), but the continuous level of detail algorithm for triangle decimation does edge
     * collapses based on various geometric measurements of the mesh representing the level surface. The geometric
     * measurements are not invariant under nonuniform scaling. Map the image into a cube using uniform scaling so that
     * the triangle collapse order is invariant. The uniform scaling is done so that the largest image dimension [0,M]
     * is mapped to [-1,1]. The other ranges are mapped to intervals of the form [-L,L) where L < 1. If (i0,i1,i2) is in
     * [0,B0) x [0,B1) x [0,B2), the corresponding (x0,x1,x2) in [-1,1) is:<br>
     * </p>
     *
     * <pre>
                     2*Dk*ik - Ck
             xk =    ------------
                     max{C0,C1,C2}
     *   </pre>
     *
     * <p>However, we want to map from [0,Bk) to an inclusive interval [-Rk,Rk], where 0 < Rk < 1 and Rk = 1 -
     * Dk/max{C0,C1,C2}. This ensures that surfaces begin in the center of a voxel rather than at the (0,0,0) corner of
     * the voxel. The problem is easiest to see in the Z direction: a surface that should cover the full Z range will
     * end before the last slice. Therefore, the formula should be:<br>
     * </p>
     *
     * <pre>
                     2*Dk*ik - Ck + Dk
             xk =    -----------------
                       max{C0,C1,C2}
     *   </pre>
     *
     * <p>Once a closed manifold triangle mesh is extracted, the problem is now to compute its volume in physical space.
     * Note that ik are indicies in the original image, but the true physical length that is measured (relative to other
     * index locations) is yk = Dk*ik. Any triangle mesh (x0,x1,x2) must be mapped to (y0,y1,y2) before the volume
     * calculation. The mapping is:<br>
     * </p>
     *
     * <pre>
                     max{C0,C1,C2}*xk + Ck - Dk
             yk =    --------------------------
                                  2
     *   </pre>
     *
     * <p>The volume calculations use the previously mentioned formula where the P points are the (y0,y1,y2) values.</p>
     *
     * @return  The volume of the surface.
     */
    public float volume() {

        float fSum = 0.0f;

        for (int iT = 0; iT < getIndexCount(); /**/) {

            // get indices to triangle vertices
            int iV0 = getCoordinateIndex(iT++);
            int iV1 = getCoordinateIndex(iT++);
            int iV2 = getCoordinateIndex(iT++);

            // get vertices
            getCoordinate(iV0, m_kV0);
            getCoordinate(iV1, m_kV1);
            getCoordinate(iV2, m_kV2);

            // Since the differences between the points is in pixels*resolutions,
            // there is no need to map into physical space.

            // compute triple scalar product
            // The scalar triple product of three vectors A, B, and C is denoted
            // [A,B,C] and defined by
            // [A,B,C] = A dot ( B x C)
            // = B dot ( C x A)
            // = C dot ( A x B)
            // = det (A (B C) )
            // = | A1 A2 A3 |
            // | B1 B2 B3 |
            // | C1 C2 C3 |
            // V = 1/6 (Sum from j=0 to n-1 of {P0 dot P1 cross P2})
            // P0 = y0, P1 = y1, P2 = y2
            // fProd = P0 dot (P1 x P2)
            // fSum = sum of fProds
            // volume returned = 1/6 fSum
            float fProd = (m_kV0.x * ((m_kV1.y * m_kV2.z) - (m_kV1.z * m_kV2.y))) +
                          (m_kV0.y * ((m_kV1.z * m_kV2.x) - (m_kV1.x * m_kV2.z))) +
                          (m_kV0.z * ((m_kV1.x * m_kV2.y) - (m_kV1.y * m_kV2.x)));

            fSum += fProd;
        }

        return Math.abs(fSum / 6.0f);
    }

    /**
     * Internal support for 'void print (String)' and 'void print (String, ModelTriangleMesh[])'. ModelTriangleMesh uses
     * this function to print vertices, normals, and connectivity indices to the file. ModelClodMesh overrides this to
     * additionally print collapse records to the file
     *
     * @param      kOut  the file to which the triangle mesh is saved
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected void print(PrintWriter kOut) throws IOException {
        Point3f kVertex = new Point3f();
        Vector3f kNormal = new Vector3f();

        // write vertices
        kOut.println(getVertexCount());
        kOut.println("Vertices");

        int i;

        for (i = 0; i < getVertexCount(); i++) {
            getCoordinate(i, kVertex);
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
        }

        kOut.println("Normals");

        // write normals
        for (i = 0; i < getVertexCount(); i++) {
            getNormal(i, kNormal);
            kOut.print(kNormal.x);
            kOut.print(' ');
            kOut.print(kNormal.y);
            kOut.print(' ');
            kOut.println(kNormal.z);
        }

        kOut.println("Connectivity");

        // write connectivity
        int iTriangleCount = getIndexCount() / 3;

        kOut.println(iTriangleCount);

        for (i = 0; i < iTriangleCount; i++) {
            kOut.print(getCoordinateIndex(3 * i));
            kOut.print(' ');
            kOut.print(getCoordinateIndex((3 * i) + 1));
            kOut.print(' ');
            kOut.println(getCoordinateIndex((3 * i) + 2));
        }
    }
    
    protected void printSMFAscii(PrintWriter kOut) throws IOException {
    	Point3f kVertex = new Point3f();
        
        int vertices = getVertexCount();
        int iTriangleCount = getIndexCount() / 3;
        int faces = iTriangleCount; 
        int index1, index2, index3;
    	
        kOut.println("#$SMF 1.0");
        kOut.println("#$vertices " + vertices);
        kOut.println("#$faces " + faces);
        kOut.println("#");
        kOut.println("# --- The canonical view ---");
        kOut.println("#");
        kOut.println("# (data originally from powerflip, not avalon)");
        kOut.println("#");
        
        int i;
        /*
        for (i = 0; i < getVertexCount(); i++) {
            getCoordinate(i, kVertex);
            kOut.print("v");
            kOut.print(' ');
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
        }
        */
        for (i = 0; i < iTriangleCount; i++) {
        	index1 = getCoordinateIndex(3 * i);    
            index2 = getCoordinateIndex((3 * i) + 1);
            index3 = getCoordinateIndex((3 * i) + 2);
            
            getCoordinate(index1, kVertex);
            kOut.print("v");
            kOut.print(' ');
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
            
            getCoordinate(index2, kVertex);;
            kOut.print("v");
            kOut.print(' ');
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
            
            getCoordinate(index3, kVertex);;
            kOut.print("v");
            kOut.print(' ');
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
        }
        // write connectivity

        for (i = 0; i < iTriangleCount; i++) {
        	kOut.print("f");
            kOut.print(' ');
            kOut.print(getCoordinateIndex(3 * i));
            kOut.print(' ');
            kOut.print(getCoordinateIndex((3 * i) + 1));
            kOut.print(' ');
            kOut.println(getCoordinateIndex((3 * i) + 2));
        }
        
        
        
        
    }
    
    /**
	 * Print the STL file format in ASCII. 
	 * STLA Examples:
	 * 
	 * solid MYSOLID 
	 * facet normal 0.4 0.4 0.2 
	 * outerloop 
	 * vertex 1.0 2.1 3.2
	 * vertex 2.1 3.7 4.5 
	 * vertex 3.1 4.5 6.7 
	 * endloop endfacet 
	 * ... 
	 * facet normal 0.2 0.2 0.4 
	 * outerloop 
	 * vertex 2.0 2.3 3.4 
	 * vertex 3.1 3.2 6.5 
	 * vertex 4.1 5.5 9.0 
	 * endloop 
	 * endfacet 
	 * endsolid MYSOLID
	 * 
	 * @param kOut
	 * @throws IOException
	 */
    protected void printSTLAscii(PrintWriter kOut) throws IOException {
        int i;
        int index1, index2, index3;
        int iTriangleCount = getIndexCount() / 3;
    	Point3f kVertex = new Point3f();
        Vector3f kNormal = new Vector3f();
        Vector3f kNormal1 = new Vector3f();
        Vector3f kNormal2 = new Vector3f();
        Vector3f kNormal3 = new Vector3f();

        kOut.println("solid");
        
        for (i = 0; i < iTriangleCount; i++) {
        	index1 = getCoordinateIndex(3 * i);    
            index2 = getCoordinateIndex((3 * i) + 1);
            index3 = getCoordinateIndex((3 * i) + 2);
            
            getNormal(index1, kNormal1);
            getNormal(index2, kNormal2);
            getNormal(index3, kNormal3);
            
            // Compute facet normal
            kNormal.set(0f, 0f, 0f);
            kNormal.add(kNormal1);
            kNormal.add(kNormal2);
            kNormal.add(kNormal3);
            kNormal.scale(1f/3f);
            
            kOut.print(" facet normal  ");
            kOut.print(kNormal.x);
            kOut.print(' ');
            kOut.print(kNormal.y);
            kOut.print(' ');
            kOut.println(kNormal.z);
            
            kOut.println("   outer loop");
            // index 1
            getCoordinate(index1, kVertex);;
            kOut.print("     vertex ");
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
            // index 2
            getCoordinate(index2, kVertex);;
            kOut.print("     vertex ");
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
            // index 3
            getCoordinate(index3, kVertex);;
            kOut.print("     vertex ");
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
            
            kOut.println("   endloop");
            kOut.println(" endfacet");
        }
        kOut.println("endsolid");
    }
    
    /**
     * Print the STL file format in binary ( little endian ). 
     * STLB Example:
     *
	 * 80 byte string = header containing nothing in particular
     *
     * 4 byte int = number of faces
     *
     * For each face:
     *
	 * 3 4-byte floats = components of normal vector to face;
     * 3 4-byte floats = coordinates of first node;
     * 3 4-byte floats = coordinates of second node;
     * 3 4-byte floats = coordinates of third and final node;
     * 2-byte int = attribute, whose value is 0.
     * @param kOut  binary file output 
     * @throws IOException
     */
    protected void printSTLBinary(RandomAccessFile kOut) throws IOException {
        int i;
        int index1, index2, index3;
        byte[] attribute = new byte[2];
        int iTriangleCount = getIndexCount() / 3;
    	Point3f kVertex = new Point3f();
        Vector3f kNormal = new Vector3f();
        Vector3f kNormal1 = new Vector3f();
        Vector3f kNormal2 = new Vector3f();
        Vector3f kNormal3 = new Vector3f();
        
        // nothing header
        byte[] header = new byte[80];
        kOut.write(header);
       
        // number of facets
        byte[] buff = new byte[4];
        kOut.write(FileBase.intToBytes(iTriangleCount, false, buff));
        byte[] buff2 = new byte[4];
        for (i = 0; i < iTriangleCount; i++) {
        	index1 = getCoordinateIndex(3 * i);    
            index2 = getCoordinateIndex((3 * i) + 1);
            index3 = getCoordinateIndex((3 * i) + 2);
            
            getNormal(index1, kNormal1);
            getNormal(index2, kNormal2);
            getNormal(index3, kNormal3);
            
            // Compute facet normal
            kNormal.set(0f, 0f, 0f);
            kNormal.add(kNormal1);
            kNormal.add(kNormal2);
            kNormal.add(kNormal3);
            kNormal.scale(1f/3f);
        
            // facet normal
            kOut.write(FileBase.floatToBytes(kNormal.x, false,buff2));
            kOut.write(FileBase.floatToBytes(kNormal.y, false,buff2));
            kOut.write(FileBase.floatToBytes(kNormal.z, false,buff2));
            
            // index 1
            getCoordinate(index1, kVertex);;            
            kOut.write(FileBase.floatToBytes(kVertex.x, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.y, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.z, false,buff2));
                       
            // index 2
            getCoordinate(index2, kVertex);;
            kOut.write(FileBase.floatToBytes(kVertex.x, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.y, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.z, false,buff2));

            // index 3
            getCoordinate(index3, kVertex);
            kOut.write(FileBase.floatToBytes(kVertex.x, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.y, false,buff2));
            kOut.write(FileBase.floatToBytes(kVertex.z, false,buff2));
            
            // 2 byte attribute == 0
            kOut.write(attribute);
            
        }
        // kOut.close();
    }


    /**
     * Internal support for 'void save (String)' and 'void save (String, ModelTriangleMesh[])'. ModelTriangleMesh uses
     * this function to write vertices, normals, and connectivity indices to the XML file. ModelClodMesh overrides this to
     * additionally write collapse records to the file
     *
     * @param      fileName           the file to which the triangle mesh is saved
     * @param      direction          either 1 or -1 for each axis
     * @param      startLocation      image origin coordinate
     * @param      box                (dim-1)*res
     * @exception  IOException  if there is an error writing to the file
     */ 
    protected void saveAsXML( String fileName, int[] direction, float[] startLocation, float[] box) {    	 
         float maxBox = Math.max(box[0], Math.max(box[1], box[2]));
         Point3f[]  akVertex = null;
         akVertex = getVertexCopy();
         
         
         // Transfer the current vertex coordinate from the image coordinate to display coordinate. 
         for (int j = 0; j < akVertex.length; j++) {
        	 // flip y, z
             akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
             akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;
        	 // The mesh files save the verticies as
             // pt.x*resX*direction[0] + startLocation
             // The loaded vertices go from -1 to 1
             // The loaded vertex is at (2.0f*pt.x*xRes - (xDim-1)*xRes)/((dim-1)*res)max
        	    akVertex[j].x = ((2.0f * (akVertex[j].x - startLocation[0]) / direction[0]) -
			                     (box[0])) / maxBox;
			    akVertex[j].y = ((2.0f * (akVertex[j].y - startLocation[1]) / direction[1]) -
			                     (box[1])) / maxBox;
			    akVertex[j].z = ((2.0f * (akVertex[j].z - startLocation[2]) / direction[2]) -
			                     (box[2])) / maxBox; 
         }
         
         setCoordinates(0, akVertex);
    	try {
        	FileSurfaceXML_J3D kSurfaceXML = new FileSurfaceXML_J3D(null, null);
        	Material material = new Material();
        	// For the material, opacity, LOD, save as default values. 
            kSurfaceXML.writeXMLsurface(fileName, this, material, 0f, 100);
        } catch (IOException kError) { }
    }
    
    /**
     * Internal support for 'void save (String)' and 'void save (String, ModelTriangleMesh[])'. ModelTriangleMesh uses
     * this function to write vertices, normals, and connectivity indices to the file. ModelClodMesh overrides this to
     * additionally write collapse records to the file
     *
     * @param      kOut               the file to which the triangle mesh is saved
     * @param      flip               if the y and z axes should be flipped - true in extraction algorithms and in
     *                                JDialogSurface. To have proper orientations in surface file if flip is true flip y
     *                                and z on reading.
     * @param      direction          either 1 or -1 for each axis
     * @param      startLocation      DOCUMENT ME!
     * @param      box                (dim-1)*res
     * @param      inverseDicomArray  DOCUMENT ME!
     * @param 	   perVertexColorArray   color per vertex array.
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected void save(RandomAccessFile kOut, boolean flip, int[] direction, float[] startLocation, float[] box,
    			TransMatrix inverseDicomMatrix, Color4f[] perVertexColorArray) throws IOException {
        Point3f[] akVertex = getVertexCopy();
        Point3f kVertex = new Point3f();
        Vector3f kNormal = new Vector3f();

        
        if (inverseDicomMatrix == null) {

            if (flip) {
                kOut.writeInt(1);
            } else {
                kOut.writeInt(0);
            }
        } else {

            if (flip) {
                kOut.writeInt(3);
            } else {
                kOut.writeInt(2);
            }
        }

        kOut.writeInt(direction[0]);
        kOut.writeInt(direction[1]);
        kOut.writeInt(direction[2]);

        byte[] bufferByte = new byte[getVertexCount() * 24];
        byte[] buffer = new byte[24];
        int i, index, tmpInt;
        int j;
        long tmpLong;

        index = 0;
        tmpInt = Float.floatToIntBits(startLocation[0]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(startLocation[1]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(startLocation[2]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(box[0]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(box[1]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        tmpInt = Float.floatToIntBits(box[2]);
        buffer[index++] = (byte) (tmpInt >>> 24);
        buffer[index++] = (byte) (tmpInt >>> 16);
        buffer[index++] = (byte) (tmpInt >>> 8);
        buffer[index++] = (byte) (tmpInt & 0xff);
        kOut.write(buffer);

        if (inverseDicomMatrix != null) {
            buffer = new byte[128];
            index = 0;

            for (i = 0; i <= 3; i++) {

                for (j = 0; j <= 3; j++) {
                    tmpLong = Double.doubleToLongBits(inverseDicomMatrix.get(i, j));
                    buffer[index++] = (byte) (tmpLong >>> 56);
                    buffer[index++] = (byte) (tmpLong >>> 48);
                    buffer[index++] = (byte) (tmpLong >>> 40);
                    buffer[index++] = (byte) (tmpLong >>> 32);
                    buffer[index++] = (byte) (tmpLong >>> 24);
                    buffer[index++] = (byte) (tmpLong >>> 16);
                    buffer[index++] = (byte) (tmpLong >>> 8);
                    buffer[index++] = (byte) (tmpLong & 0xff);
                }
            }

            kOut.write(buffer);
        }

        // write vertices
        kOut.writeInt(getVertexCount());

        for (i = 0, index = 0; i < getVertexCount(); i++) {
            kVertex = akVertex[i];

            tmpInt = Float.floatToIntBits(kVertex.x);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kVertex.y);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kVertex.z);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

        }

        /* if (flip) {
         * setCoordinates(0,akVertex); computeNormals(); }*/

        // write normals
        for (i = 0; i < getVertexCount(); i++) {
            getNormal(i, kNormal);

            tmpInt = Float.floatToIntBits(kNormal.x);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kNormal.y);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

            tmpInt = Float.floatToIntBits(kNormal.z);
            bufferByte[index++] = (byte) (tmpInt >>> 24);
            bufferByte[index++] = (byte) (tmpInt >>> 16);
            bufferByte[index++] = (byte) (tmpInt >>> 8);
            bufferByte[index++] = (byte) (tmpInt & 0xff);

        }

        kOut.write(bufferByte);
        // System.err.println("buffer byte size = " + index + "  actual size = " + (getVertexCount() * 24));
        // write connectivity
        kOut.writeInt(getIndexCount());

        byte[] bufferInt = new byte[getIndexCount() * 4];

        for (i = 0, index = 0; i < getIndexCount(); i++) {
            tmpInt = getCoordinateIndex(i);
            bufferInt[index++] = (byte) (tmpInt >>> 24);
            bufferInt[index++] = (byte) (tmpInt >>> 16);
            bufferInt[index++] = (byte) (tmpInt >>> 8);
            bufferInt[index++] = (byte) (tmpInt & 0xff);

        }

        kOut.write(bufferInt);
        
       
        if ( perVertexColorArray == null )  {
        	kOut.writeInt(0);
        	return;
        } else {
        	kOut.writeInt(1);
        }
        
        byte[] bufferColor = new byte[getVertexCount() * 4 * 4];
        Color4f vertexColor = new Color4f();
        int R, G, B ,A;
        for (i = 0, index = 0; i < getVertexCount(); i++) {
        	 vertexColor = perVertexColorArray[i];
        	
        	 R = Float.floatToIntBits(vertexColor.x);
        	 bufferColor[index++] = (byte) (R >>> 24);
        	 bufferColor[index++] = (byte) (R >>> 16);
        	 bufferColor[index++] = (byte) (R >>> 8);
        	 bufferColor[index++] = (byte) (R & 0xff);

             G = Float.floatToIntBits(vertexColor.y);
             bufferColor[index++] = (byte) (G >>> 24);
             bufferColor[index++] = (byte) (G >>> 16);
             bufferColor[index++] = (byte) (G >>> 8);
             bufferColor[index++] = (byte) (G & 0xff);

             B = Float.floatToIntBits(vertexColor.z);
             bufferColor[index++] = (byte) (B >>> 24);
             bufferColor[index++] = (byte) (B >>> 16);
             bufferColor[index++] = (byte) (B >>> 8);
             bufferColor[index++] = (byte) (B & 0xff);
             
             A = Float.floatToIntBits(vertexColor.w);
             bufferColor[index++] = (byte) (A >>> 24);
             bufferColor[index++] = (byte) (A >>> 16);
             bufferColor[index++] = (byte) (A >>> 8);
             bufferColor[index++] = (byte) (A & 0xff);
        	
        }
        kOut.write(bufferColor);
        // System.err.println("bufferColor = "  + bufferColor.length + " index length = " + index);
    }

    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format. File name should end with ".wrl"
     *
     * @param      kOut           the file to which the triangle mesh is saved
     * @param      flip           if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                            To have proper orientations in surface file if flip is true flip y and z on reading.
     * @param      direction      1 or -1 for each axis
     * @param      startLocation  DOCUMENT ME!
     * @param      box            (dimension-1)*resolution
     * @param      color          DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected void saveAsPortableVRML(PrintWriter kOut, boolean flip, int[] direction, float[] startLocation,
                                      float[] box, Color3f color) throws IOException {
        Point3f kVertex = new Point3f();

        kOut.print("#flip { ");

        if (flip) {
            kOut.print(1);
        } else {
            kOut.print(0);
        }

        kOut.print(" }\n");

        kOut.print("#direction { ");
        kOut.print(direction[0]);
        kOut.print(' ');
        kOut.print(direction[1]);
        kOut.print(' ');
        kOut.print(direction[2]);
        kOut.print(" }\n");

        kOut.print("#startLocation { ");
        kOut.print(startLocation[0]);
        kOut.print(' ');
        kOut.print(startLocation[1]);
        kOut.print(' ');
        kOut.print(startLocation[2]);
        kOut.print(" }\n");

        kOut.print("#box { ");
        kOut.print(box[0]);
        kOut.print(' ');
        kOut.print(box[1]);
        kOut.print(' ');
        kOut.print(box[2]);
        kOut.print(" }\n");
        
        kOut.print("Shape\n{\n");
        kOut.print("\t appearance Appearance {\n");
        kOut.print("\t\tmaterial Material {\n");
        kOut.print("\t\t\temissiveColor\t");
        kOut.print(color.x + " ");
        kOut.print(color.y + " ");
        kOut.print(color.z);
        kOut.print("\n\t\t\tdiffuseColor\t");
        kOut.print(color.x + " ");
        kOut.print(color.y + " ");
        kOut.print(color.z);
        kOut.print("\n\t\t\tspecularColor\t");
        kOut.print(color.x + " ");
        kOut.print(color.y + " ");
        kOut.print(color.z);
        kOut.print("\n\t\t\ttransparency 0.5\n");
        kOut.print("\t\t}\n");
        kOut.print("\t}\n");
        kOut.print("\t\tgeometry IndexedFaceSet\n\t{\n");

        /*
         * kOut.print("\tflip \n\t{\n"); if (flip) { kOut.print(1); } else { kOut.print(0); }
         * kOut.print("\n\t\t\t\t]\n"); kOut.print("\tdirection \n\t{\n"); kOut.print(direction[0]); kOut.print(' ');
         * kOut.print(direction[1]); kOut.print(' '); kOut.print(direction[2]); kOut.print("\n\t\t\t\t]\n");
         * kOut.print("\tstartLocation \n\t{\n"); kOut.print(startLocation[0]); kOut.print(' ');
         * kOut.print(startLocation[1]); kOut.print(' '); kOut.print(startLocation[2]); kOut.print("\n\t\t\t\t]\n");
         * kOut.print("\tbox \n\t{\n"); kOut.print(box[0]); kOut.print(' '); kOut.print(box[1]); kOut.print(' ');
         * kOut.print(box[2]); kOut.print("\n\t\t\t\t]\n");
         */
        kOut.print("\t\tcoord Coordinate\n");
        kOut.print("\t\t{\n");
        kOut.print("\t\t\tpoint [\n\t\t\t\t");

        // write vertices
        int i;

        for (i = 0; i < getVertexCount(); i++) {
            getCoordinate(i, kVertex);
            kOut.print(kVertex.x);
            kOut.print(' ');

            kOut.print(kVertex.y);
            kOut.print(' ');

            kOut.print(kVertex.z);

            if (i < (getVertexCount() - 1)) {
                kOut.print(" ,\n\t\t\t\t");
            } else {
                kOut.print("\n\t\t\t\t]\n");
            }
        }

        // write connectivity
        kOut.print("\t\t\t}\n\t\t\tcoordIndex [\n\t\t\t\t");

        int iTriangleCount = getIndexCount() / 3;

        for (i = 0; i < iTriangleCount; i++) {
            kOut.print(getCoordinateIndex(3 * i));
            kOut.print(' ');
            kOut.print(getCoordinateIndex((3 * i) + 1));
            kOut.print(' ');
            kOut.print(getCoordinateIndex((3 * i) + 2));

            if (i < (iTriangleCount - 1)) {
                kOut.print(" -1\n\t\t\t\t");
            } else {
                kOut.print("\n\t\t\t\t]\n");
            }
        }

        kOut.print("\t\t\tconvex FALSE\n");
        kOut.print("\t\t\tcreaseAngle 1.5\n");
        kOut.print("\t}\n}\n");
    }

    /**
     * Saves the triangle mesh in VRML97 (VRML 2.0) format. File name should end with ".wrl"
     *
     * @param      kOut           the file to which the triangle mesh is saved
     * @param      flip           if the y and z axes should be flipped - true in extract and in save of JDialogSurface
     *                            To have proper orientations in surface file if flip is true flip y and z on reading.
     * @param      direction      1 or -1 for each axis
     * @param      startLocation  DOCUMENT ME!
     * @param      box            (dimension-1)*resolution
     * @param      color          DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected void saveAsVRML(PrintWriter kOut, boolean flip, int[] direction, float[] startLocation, float[] box,
                              Color3f color) throws IOException {
        Point3f kVertex = new Point3f();

        kOut.print("#flip { ");

        if (flip) {
            kOut.print(1);
        } else {
            kOut.print(0);
        }

        kOut.print(" }\n");

        kOut.print("#direction { ");
        kOut.print(direction[0]);
        kOut.print(' ');
        kOut.print(direction[1]);
        kOut.print(' ');
        kOut.print(direction[2]);
        kOut.print(" }\n");

        kOut.print("#startLocation { ");
        kOut.print(startLocation[0]);
        kOut.print(' ');
        kOut.print(startLocation[1]);
        kOut.print(' ');
        kOut.print(startLocation[2]);
        kOut.print(" }\n");

        kOut.print("#box { ");
        kOut.print(box[0]);
        kOut.print(' ');
        kOut.print(box[1]);
        kOut.print(' ');
        kOut.print(box[2]);
        kOut.print(" }\n");

        kOut.print("Shape\n{\n");
        kOut.print("\tappearance Appearance {\n");
        kOut.print("\t\tmaterial Material {\n");
        kOut.print("\t\t\temissiveColor\t");
        kOut.print(color.x + " ");
        kOut.print(color.y + " ");
        kOut.print(color.z);
        kOut.print("\n\t\t\tdiffuseColor\t");
        kOut.print(color.x + " ");
        kOut.print(color.y + " ");
        kOut.print(color.z);
        kOut.print("\n\t\t\tspecularColor\t");
        kOut.print(color.x + " ");
        kOut.print(color.y + " ");
        kOut.print(color.z);
        kOut.print("\n\t\t\ttransparency " + m_fTransparency + "\n");
        kOut.print("\t\t}\n");
        kOut.print("\t}\n");

        kOut.print("\tgeometry IndexedFaceSet\n\t{\n");

        kOut.print("\t\tcoord Coordinate\n");
        kOut.print("\t\t{\n");
        kOut.print("\t\t\tpoint [\n\t\t\t\t");

        // write vertices
        int i;

        for (i = 0; i < getVertexCount(); i++) {
            getCoordinate(i, kVertex);
            kOut.print(kVertex.x);
            kOut.print(' ');

            kOut.print(kVertex.y);
            kOut.print(' ');

            kOut.print(kVertex.z);

            if (i < (getVertexCount() - 1)) {
                kOut.print(" ,\n\t\t\t\t");
            } else {
                kOut.print("\n\t\t\t\t]\n");
            }
        }

        // write connectivity
        kOut.print("\t\t\t}\n\t\t\tcoordIndex [\n\t\t\t\t");

        int iTriangleCount = getIndexCount() / 3;

        for (i = 0; i < iTriangleCount; i++) {
            kOut.print(getCoordinateIndex(3 * i));
            kOut.print(' ');
            kOut.print(getCoordinateIndex((3 * i) + 1));
            kOut.print(' ');
            kOut.print(getCoordinateIndex((3 * i) + 2));

            if (i < (iTriangleCount - 1)) {
                kOut.print(" -1\n\t\t\t\t");
            } else {
                kOut.print("\n\t\t\t\t]\n");
            }
        }

        kOut.print("\t\t\tconvex FALSE\n");
        kOut.print("\t\t\tcreaseAngle 1.5\n");
        kOut.print("\t}\n}\n");
    }

    /**
     * Builds a list of cross-references, so that connections[i] contains all the verticies that are connected to vertex
     * at i.
     */
    private void buildConnections() {
        Iterator iter;
        int index;
        boolean addT1, addT2, addT3;

        connections = new HashSet[getVertexCount()];

        for (int i = 0; i < getIndexCount(); /**/) {
            int triangle1 = getCoordinateIndex(i++);
            int triangle2 = getCoordinateIndex(i++);
            int triangle3 = getCoordinateIndex(i++);

            if (connections[triangle1] == null) {
                connections[triangle1] = new HashSet();
            }

            addT2 = true;
            addT3 = true;

            for (iter = connections[triangle1].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == triangle2) {
                    addT2 = false;
                } else if (index == triangle3) {
                    addT3 = false;
                }
            }

            if (addT2) {
                connections[triangle1].add(new Integer(triangle2));
            }

            if (addT3) {
                connections[triangle1].add(new Integer(triangle3));
            }

            if (connections[triangle2] == null) {
                connections[triangle2] = new HashSet();
            }

            addT1 = true;
            addT3 = true;

            for (iter = connections[triangle2].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == triangle1) {
                    addT1 = false;
                } else if (index == triangle3) {
                    addT3 = false;
                }
            }

            if (addT1) {
                connections[triangle2].add(new Integer(triangle1));
            }

            if (addT3) {
                connections[triangle2].add(new Integer(triangle3));
            }

            if (connections[triangle3] == null) {
                connections[triangle3] = new HashSet();
            }

            addT1 = true;
            addT2 = true;

            for (iter = connections[triangle3].iterator(); iter.hasNext();) {
                index = ((Integer) iter.next()).intValue();

                if (index == triangle1) {
                    addT1 = false;
                } else if (index == triangle2) {
                    addT2 = false;
                }
            }

            if (addT1) {
                connections[triangle3].add(new Integer(triangle1));
            }

            if (addT2) {
                connections[triangle3].add(new Integer(triangle2));
            }
        }
    }

    /**
     * Compute the average length of all the edges in the triangle mesh.
     */
    private void computeMeanEdgeLength() {
        m_fMeanEdgeLength = 0.0f;

        Iterator kEIter = m_kEMap.entrySet().iterator();
        Map.Entry kEntry = null;
        Vector3f kEdge = new Vector3f();

        while (kEIter.hasNext()) {
            kEntry = (Map.Entry) kEIter.next();

            Edge kE = (Edge) kEntry.getKey();

            getCoordinate(kE.m_i0, m_kV0);
            getCoordinate(kE.m_i1, m_kV1);
            kEdge.sub(m_kV1, m_kV0);
            m_fMeanEdgeLength += kEdge.length();
        }

        m_fMeanEdgeLength /= m_kEMap.size();
    }

    /**
     * Let V[i] be a vertex in the triangle mesh. This function computes VMean[i], the average of the immediate
     * neighbors of V[i]. Define S[i] = VMean[i] - V[i]. The function also computes a surface normal SNormal[i], the
     * component of S[i] in the vertex normal direction. STangent[i] = S[i] - SNormal[i] is computed as an approximation
     * to a tangent to the surface. Finally, Curvature[i] is an approximation of the surface curvature at V[i].
     */
    private void computeVertexInformation() {
        float fMinCurvature = Float.POSITIVE_INFINITY;
        float fMaxCurvature = Float.NEGATIVE_INFINITY;
        float fInvMeanLength = 1.0f / m_fMeanEdgeLength;

        int i;

        for (i = 0; i < getVertexCount(); i++) {
            m_akVMean[i].set(0.0f, 0.0f, 0.0f);
        }

        Vector3f kS = new Vector3f();

        for (i = 0; i < getVertexCount(); i++) {

            // compute the mean of the vertex neighbors
            // Point3f kMean = m_akVMean[i];
            UnorderedSetInt kAdj = m_akAdjacent[i];

            for (int j = 0; j < kAdj.getQuantity(); j++) {
                getCoordinate(kAdj.get(j), m_kV0);
                m_akVMean[i].add(m_kV0);
            }

            m_akVMean[i].scale(1.0f / kAdj.getQuantity());

            // compute the normal and tangential components of mean-vertex
            getCoordinate(i, m_kV0);
            kS.sub(m_akVMean[i], m_kV0);
            m_akSNormal[i].scale(kS.dot(m_akVNormal[i]), m_akVNormal[i]);
            m_akSTangent[i].sub(kS, m_akSNormal[i]);

            // compute the curvature
            float fLength = m_akSNormal[i].length();

            m_afCurvature[i] = ((2.0f * fLength) * fInvMeanLength) * fInvMeanLength;

            if (m_afCurvature[i] < fMinCurvature) {
                fMinCurvature = m_afCurvature[i];
            }

            if (m_afCurvature[i] > fMaxCurvature) {
                fMaxCurvature = m_afCurvature[i];
            }
        }

        // compute the fractional function parameters for update2()
        m_fEParam = 0.5f * (fMinCurvature + fMaxCurvature);
        m_fFParam = 6.0f / (fMaxCurvature - fMinCurvature);
    }

    /**
     * Compute the vertex normals of the triangle mesh. Each vertex normal is the unitized average of the non-unit
     * triangle normals for those triangles sharing the vertex.
     */
    private void computeVertexNormals() {

        // maintain a running sum of triangle normals at each vertex
        int i;

        for (i = 0; i < getVertexCount(); i++) {
            m_akVNormal[i].set(0.0f, 0.0f, 0.0f);
        }

        Vector3f kEdge1 = new Vector3f();
        Vector3f kEdge2 = new Vector3f();
        Vector3f kNormal = new Vector3f();

        for (int iT = 0; iT < getIndexCount();) {

            // get the vertices of the triangle
            int iP0 = getCoordinateIndex(iT++);
            int iP1 = getCoordinateIndex(iT++);
            int iP2 = getCoordinateIndex(iT++);

            getCoordinate(iP0, m_kV0);
            getCoordinate(iP1, m_kV1);
            getCoordinate(iP2, m_kV2);

            // compute the triangle normal
            kEdge1.sub(m_kV1, m_kV0);
            kEdge2.sub(m_kV2, m_kV0);
            kNormal.cross(kEdge1, kEdge2);

            // the triangle normal partially contributes to each vertex normal
            m_akVNormal[iP0].add(kNormal);
            m_akVNormal[iP1].add(kNormal);
            m_akVNormal[iP2].add(kNormal);
        }

        for (i = 0; i < getVertexCount(); i++) {
            m_akVNormal[i].normalize();
        }
    }

    /**
     * Compute the coefficient of the surface normal for the update of the mesh vertex V[i] in the SNormal[i] direction.
     * See BrainExtraction.pdf for a description of the update.
     *
     * @param   i  the index of the vertex to update
     *
     * @return  the coefficient of SNormal[i] for the update
     */
    private float update2(int i) {
        float fArg = m_fFParam * (m_afCurvature[i] - m_fEParam);
        float fExpP = (float) Math.exp(fArg);
        float fExpN = (float) Math.exp(-fArg);
        float fTanh = (fExpP - fExpN) / (fExpP + fExpN);
        float fUpdate2 = 0.5f * m_fStiffness * (1.0f + fTanh);

        return fUpdate2;
    }

    /**
     * The heart of the segmentation. This function is responsible for the evolution of the triangle mesh that
     * approximates the brain surface. The update has a tangential component, a surface normal component, and a vertex
     * normal component for each vertex in the mesh. The first two components control the geometry of the mesh. The last
     * component is based on the MRI data itself. See BrainExtraction.pdf for a detailed description of the update
     * terms.
     */
    private void updateMesh() {
        computeMeanEdgeLength();
        computeVertexNormals();
        computeVertexInformation();

        // update the vertices
        for (int i = 0; i < getVertexCount(); i++) {
            getCoordinate(i, m_kV0);

            // tangential update
            m_kV0.scaleAdd(0.5f, m_akSTangent[i], m_kV0);

            // normal update
            float fUpdate2 = update2(i);

            m_kV0.scaleAdd(fUpdate2, m_akSNormal[i], m_kV0);
            setCoordinate(i, m_kV0);
        }
    }

    /**
     * Invert the mesh z-coord, reorder the triangles, and recompute the
     * normals:
     */
    public void invertMesh()
    {
        /* Reorder the triangles: */
        int[] aiConnect = this.getIndexCopy();
        for (int j = 0; j < aiConnect.length; j+=3 )
        {
            /* swap triangle points 2 and 1: (leave 0 untouched)*/
            int temp = aiConnect[j + 2];
            aiConnect[j + 2] = aiConnect[j + 1];
            aiConnect[j + 1] = temp;
        }
        /* set the mesh: */
        this.setCoordinateIndices(0,aiConnect);

        Point3f[] akVertex = this.getVertexCopy();
        /* Invert the z-coord: */
        for (int j = 0; j < akVertex.length; j++) {
            akVertex[j].z *= -1;
        }
        this.setVerticies(akVertex);
        setNormalIndices(0, aiConnect);
    }

    /** Sets the material properties of the mesh.
     * @param kMaterial new material properties.
     */
    public void setMaterial( Material kMaterial )
    {
        m_kMaterial = kMaterial;
    }

    /** Gets the material properties of the mesh:
     * @return material properties.
     */
    public Material getMaterial()
    {
        return m_kMaterial;
    }

    /** Sets the mesh transparency.
     * @param transparency mesh transparency
     */
    public void setTransparency( float transparency )
    {
        m_fTransparency = transparency;
    }

    /** Gets the mesh transparency.
     * @return mesh transparency.
     */
    public float getTransparency()
    {
        return m_fTransparency;
    }
    

    //~ Inner Classes --------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     */
    public class UnorderedSetInt {

        /** The array storage for the set. */
        protected int[] m_aiElement;

        /** On a reallocation, the old maximum quantity is incremented by this value. */
        protected int m_iGrow;

        /** The maximum number of elements in the array. It is always the case that m_iQuantity <= m_iMaxQuantity. */
        protected int m_iMaxQuantity;

        /** Support for remove and removeAt. */
        protected int m_iOldIndex, m_iNewIndex;

        /** The number of valid elements in the array. The valid indices are 0 <= i < m_iQuantity. */
        protected int m_iQuantity;

        /**
         * The default growth value for reallocations of the array representing the set. The application can change this
         * to whatever is appropriate for its purposes.
         */
        private int DEFAULT_GROW = 8;

        /**
         * Construct an empty unordered set. The initial maximum quantity and growth values are DEFAULT_GROW. When
         */
        public UnorderedSetInt() {
            reset();
        }

        /**
         * Create an unordered set that is a deep copy of the input set.
         *
         * @param  kSet  The input set to copy.
         */
        public UnorderedSetInt(UnorderedSetInt kSet) {
            copy(kSet);
        }

        /**
         * Construct an empty unordered set with the specified maximum quantity and growth values.
         *
         * @param  iMaxQuantity  The initial number of elements in the array. If the value is nonpositive, the initial
         *                       number is DEFAULT_GROW.
         * @param  iGrow         The growth amount for a reallocation. If a reallocation occurs, the new number of
         *                       elements is the current maximum quantity plus the growth value. If the input value is
         *                       nonpositive, the growth is set to DEFAULT_GROW.
         */
        public UnorderedSetInt(int iMaxQuantity, int iGrow) {
            reset(iMaxQuantity, iGrow);
        }

        /**
         * Append an element to the end of the storage array.
         *
         * @param   iElement  The element to append.
         *
         * @return  The array location that contains the newly appended element. A side effect of this call is
         *          reallocation of the storage array, if necessary.
         */
        public int append(int iElement) {

            if (m_iQuantity == m_iMaxQuantity) {
                int iNewMaxQuantity = m_iMaxQuantity + m_iGrow;
                int[] aiNewElement = new int[iNewMaxQuantity];

                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iMaxQuantity);
                m_iMaxQuantity = iNewMaxQuantity;
                m_aiElement = aiNewElement;
            }

            int iLocation = m_iQuantity++;

            m_aiElement[iLocation] = iElement;

            return iLocation;
        }

        /**
         * Use exactly the amount of array storage for the current elements in the set. After the call, getQuantity()
         * and getMaximumQuantity() return the same value. This call does cause a reallocation.
         */
        public void compactify() {

            if (m_iQuantity > 0) {

                // Try Catch - Matt
                int[] aiNewElement = new int[m_iQuantity];

                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iQuantity);
                m_iMaxQuantity = m_iQuantity;
                m_aiElement = aiNewElement;
            } else {
                reset();
            }
        }

        /**
         * Make a deep copy of the input set.
         *
         * @param  kSet  The set to make a deep copy of.
         */
        public void copy(UnorderedSetInt kSet) {
            m_iQuantity = kSet.m_iQuantity;
            m_iMaxQuantity = kSet.m_iMaxQuantity;
            m_iGrow = kSet.m_iGrow;
            m_aiElement = new int[m_iMaxQuantity];
            System.arraycopy(kSet.m_aiElement, 0, m_aiElement, 0, m_iMaxQuantity);
        }

        /**
         * Search the set to see if the input element currently exists.
         *
         * @param   iElement  The element to search for.
         *
         * @return  The value is true if and only if the element is found in the set.
         */
        public boolean exists(int iElement) {

            for (int i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    return true;
                }
            }

            return false;
        }

        /**
         * Retrieve the element in the array location i. It is necessary that 0 <= i < getQuantity() in order to read
         * valid elements.
         *
         * @param   i  The array location whose element is to be retrieved.
         *
         * @return  The element in array location i.
         */
        public final int get(int i) {
            return m_aiElement[i];
        }

        /**
         * The growth value for reallocations. If a reallocation must occur, the new maximum quantity is the current
         * maximum quantity plus the growth amount.
         *
         * @return  The growth value.
         */
        public final int getGrow() {
            return m_iGrow;
        }

        /**
         * The maximum quantity of elements in the set. Not all elements are necessarily used. The used quantity is
         * provided by getQuantity().
         *
         * @return  The maximum quantity of elements in the set.
         */
        public final int getMaxQuantity() {
            return m_iMaxQuantity;
        }

        /**
         * On a call to remove or removeAt, the last element in the array is potentially moved to the array location
         * vacated by the removed element. The new location of the last element is retrived by this function. However,
         * if the last element is the one that was removed, this function returns -1. If you need the value, you must
         * call this function before the next call to remove or removeAt.
         *
         * @return  The new location of the last element that was moved.
         */
        public final int getNewIndex() {
            return m_iNewIndex;
        }

        /**
         * On a call to remove or removeAt, the last element in the array is moved to the array location vacated by the
         * removed element. The old location of the last element is retrived by this function. If you need the value,
         * you must call this function before the next call to remove or removeAt.
         *
         * @return  The old location of the last element that was moved.
         */
        public final int getOldIndex() {
            return m_iOldIndex;
        }

        /**
         * The current number of valid elements in the array. This number is less than or equal to the maximum quantity.
         * The elements with indices 0 through getQuantity()-1 are the valid ones.
         *
         * @return  The current number of valid elements.
         */
        public final int getQuantity() {
            return m_iQuantity;
        }

        /**
         * Insert an element into the set.
         *
         * @param   iElement  The element to insert.
         *
         * @return  The value is true if and only if the element is inserted. The input element is not inserted if it
         *          already exists in the set. A side effect of this call is reallocation of the storage array, if
         *          necessary.
         */
        public boolean insert(int iElement) {
            int i;

            for (i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    return false;
                }
            }

            if (m_iQuantity == m_iMaxQuantity) {
                int iNewMaxQuantity = m_iMaxQuantity + m_iGrow;
                int[] aiNewElement = new int[iNewMaxQuantity];

                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iMaxQuantity);
                m_iMaxQuantity = iNewMaxQuantity;
                m_aiElement = aiNewElement;
            }

            m_aiElement[m_iQuantity++] = iElement;

            return true;
        }

        /**
         * Remove the specified element from the set.
         *
         * @param   iElement  The element to remove.
         *
         * @return  The value is true if and only if the element existed and was removed. The last element is
         *          potentially moved into the slot vacated by the specified element. If needed, the old and new
         *          locations of the last element can be retrieved by calls to getOldIndex() and getNewIndex(). If the
         *          last element was the one removed, getNewIndex() returns -1.
         */
        public boolean remove(int iElement) {

            for (int i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    m_iQuantity--;
                    m_iOldIndex = m_iQuantity;

                    if (i != m_iQuantity) {
                        m_aiElement[i] = m_aiElement[m_iQuantity];
                        m_iNewIndex = i;
                    } else {
                        m_iNewIndex = -1;
                    }

                    return true;
                }
            }

            return false;
        }

        /**
         * Remove the element from the set in the specified location.
         *
         * @param   i  The array location whose element is to be removed.
         *
         * @return  The value is true if and only if the input location is within the valid index range 0 <= i <
         *          getQuantity(). The last element is potentially moved into the slot vacated by the specified element.
         *          If needed, the old and new locations of the last element can be retrieved by calls to getOldIndex()
         *          and getNewIndex(). If the last element was the one removed, getNewIndex() returns -1.
         */
        public boolean removeAt(int i) {

            if ((0 <= i) && (i < m_iQuantity)) {
                m_iQuantity--;
                m_iOldIndex = m_iQuantity;

                if (i != m_iQuantity) {
                    m_aiElement[i] = m_aiElement[m_iQuantity];
                    m_iNewIndex = i;
                } else {
                    m_iNewIndex = -1;
                }

                return true;
            }

            return false;
        }

        /**
         * Reset the unordered set to its initial state. The old array is deleted. The new array has a maximum quantity
         * of DEFAULT_GROW and the growth value is DEFAULT_GROW.
         */
        public void reset() {
            reset(0, 0);
        }

        /**
         * Reset the unordered set to the specified state. The old array is deleted. The new array has a maximum
         * quantity and growth value as specified by the inputs.
         *
         * @param  iMaxQuantity  The new maximum quantity for the array.
         * @param  iGrow         The new growth value.
         */
        public void reset(int iMaxQuantity, int iGrow) {

            if (iMaxQuantity <= 0) {
                iMaxQuantity = DEFAULT_GROW;
            }

            if (iGrow <= 0) {
                iGrow = DEFAULT_GROW;
            }

            m_iQuantity = 0;
            m_iMaxQuantity = iMaxQuantity;
            m_iGrow = iGrow;
            m_aiElement = new int[m_iMaxQuantity];
        }

        /**
         * Assign the specified element to array location i. It is necessary that 0 <= i < getMaxQuantity().
         *
         * @param  i         The array location to assign to.
         * @param  iElement  The element to assign to array location i.
         */
        public final void set(int i, int iElement) {
            m_aiElement[i] = iElement;
        }
    }

    /**
     * A representation of an edge for the vertex-edge-triangle table. This class stores the pair of vertex indices for
     * the end points of the edge. The edges <V0,V1> and <V1,V0> are considered to be identical. To simplify
     * comparisons, the class stores the ordered indices. The class extends Object to obtain support for hashing into a
     * map of edges.
     */
    protected class Edge extends Object {

        /** DOCUMENT ME! */
        public int m_i0, m_i1;

        /**
         * Constructs an edge in the table.
         *
         * @param  i0  a vertex index for an end point
         * @param  i1  a vertex index for an end point
         */
        public Edge(int i0, int i1) {

            if (i0 < i1) {

                // i0 is minimum
                m_i0 = i0;
                m_i1 = i1;
            } else {

                // i1 is minimum
                m_i0 = i1;
                m_i1 = i0;
            }
        }

        /**
         * Support for hashing into a map of edges.
         *
         * @param   kObject  an edge for comparison to the current one
         *
         * @return  true iff the edges are identical. Because the class stores ordered indices, it is not necessary to
         *          use the more expensive test (i0 == other.i0 && i1 == other.i1) || (i0 == other.i1 && i1 ==
         *          other.i0).
         */
        public boolean equals(Object kObject) {
            Edge kE = (Edge) kObject;

            return (m_i0 == kE.m_i0) && (m_i1 == kE.m_i1);
        }

        /**
         * Support for hashing into a map of edges.
         *
         * @return  the hash key for the edge
         */
        public int hashCode() {
            return (m_i0 << 16) | m_i1;
        }
    }
}
