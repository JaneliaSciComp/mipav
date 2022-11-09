package gov.nih.mipav.view.renderer.J3D.model.structures;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * A simple quadrilateral mesh that represents a level surface. The mesh only stores vertex positions and vertex
 * normals. The surface viewer creates a Shape3D object whose geometry is an object from ModelQuadMesh. The surface
 * color is provided through the LUT. To save memory, everything is done by reference - the connection array, the vertex
 * array, the normal array, and the color array are all stored here and modified directly.
 *
 * @author  Matthew J. McAuliffe, Ph.D.
 * @author  David Eberley
 * @author  Neva Cherniavsky
 */

public class ModelQuadMesh extends IndexedQuadArray implements GeometryUpdater {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Holds colors for vertices. */
    private Color3f[] colors3f = null;

    /** Holds how vertices are connected. */
    private int[] connect = null;

    /** Scale the height of the vertice - typically relative the image size. */
    private float heightRange;

    /** Holds current LUT which dictates colors for vertices. */
    private float[][] LUT = null;

    /** DOCUMENT ME! */
    private int[] lutBufferRemapped;

    /** Temporary variables to avoid 'new' calls. */
    private Vector3f m_kE0, m_kE1, m_kN1;

    /** Temporary variables to avoid 'new' calls. */
    private Point3f m_kV0, m_kV1, m_kV2;

    /** Holds normals in the mesh. */
    private Vector3f[] normal = null;

    /** Holds vertices in the mesh. */
    private Point3f[] vertex = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * A quad mesh whose vertex normal vectors have been precomputed.
     *
     * @param  akVertex   Array of vertices in the mesh.
     * @param  akNormal   Array of vertex normals for the mesh.
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     */
    public ModelQuadMesh(Point3f[] akVertex, Vector3f[] akNormal, int[] aiConnect) {
        super(akVertex.length, IndexedQuadArray.COORDINATES | GeometryArray.COLOR_3 | IndexedQuadArray.NORMALS,
              aiConnect.length);

        setCapability(Geometry.ALLOW_INTERSECT);
        setCapability(GeometryArray.ALLOW_COUNT_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_COORDINATE_INDEX_WRITE);
        setCapability(GeometryArray.ALLOW_NORMAL_WRITE);
        setCapability(GeometryArray.ALLOW_NORMAL_READ);
        setCapability(GeometryArray.ALLOW_COUNT_READ);
        setCapability(GeometryArray.ALLOW_COORDINATE_READ);
        setCapability(GeometryArray.ALLOW_COLOR_READ);
        setCapability(GeometryArray.ALLOW_COLOR_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_COLOR_INDEX_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_REF_DATA_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_REF_DATA_READ);

        // temporary variables to avoid 'new' calls
        m_kV0 = new Point3f();
        m_kV1 = new Point3f();
        m_kV2 = new Point3f();
        m_kE0 = new Vector3f();
        m_kE1 = new Vector3f();
        m_kN1 = new Vector3f();

        connect = new int[aiConnect.length];

        for (int i = 0; i < connect.length; i++) {
            connect[i] = aiConnect[i];
        }

        vertex = new Point3f[akVertex.length];
        colors3f = new Color3f[akVertex.length];
        normal = new Vector3f[vertex.length];

        float max = -Float.MAX_VALUE;
        float min = Float.MAX_VALUE;
        float range;

        for (int i = 0; i < vertex.length; i++) {
            vertex[i] = new Point3f(akVertex[i]);

            if (vertex[i].z < min) {
                min = vertex[i].z;
            }

            if (vertex[i].z > max) {
                max = vertex[i].z;
            }
        }

        range = max - min;

        if (range == 0) {
            range = 1;
        }

        heightRange = range;

        int index;

        for (int i = 0; i < vertex.length; i++) {
            colors3f[i] = new Color3f();
            normal[i] = new Vector3f(akNormal[i]);
            index = MipavMath.round((vertex[i].z - min) / range * 255.0f);

            if (LUT != null) {
                colors3f[i].x = LUT[0][index] / 256.0f;
                colors3f[i].y = LUT[1][index] / 256.0f;
                colors3f[i].z = LUT[2][index] / 256.0f;
            }
        }

        //        setCoordRef3f(vertex);
        this.setCoordinates(0, vertex);
        setCoordinateIndices(0, connect);
        //        setColorRef3f(colors3f);
        this.setColors(0, colors3f);
        setColorIndices(0, connect);
        //        setNormalRef3f(normal);
        this.setNormals(0, normal);
        setNormalIndices(0, connect);
    }

    /**
     * A quad mesh whose vertex normal vectors are computed from the geometry of the mesh itself. The normal at a vertex
     * is the normalized average of normals of the triangles that share the vertex. This constructor makes a quad mesh
     * whose vertices and colors are set by reference.
     *
     * @param  akVertex   Array of vertices in the mesh.
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     * @param  length1    True length of vertex array.
     * @param  length2    True length of connection array.
     * @param  LUT        LUT for colors.
     * @param  hgtRange   Used to scale the height of the vertices
     */
    public ModelQuadMesh(Point3f[] akVertex, int[] aiConnect, int length1, int length2, float[][] LUT, float hgtRange) {
        super(length1, IndexedQuadArray.COORDINATES | GeometryArray.COLOR_3 | IndexedQuadArray.NORMALS, length2);

        setCapability(Geometry.ALLOW_INTERSECT);
        setCapability(GeometryArray.ALLOW_COUNT_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_COORDINATE_INDEX_WRITE);
        setCapability(GeometryArray.ALLOW_NORMAL_WRITE);
        setCapability(GeometryArray.ALLOW_NORMAL_READ);
        setCapability(GeometryArray.ALLOW_COUNT_READ);
        setCapability(GeometryArray.ALLOW_COORDINATE_READ);
        setCapability(GeometryArray.ALLOW_COLOR_READ);
        setCapability(GeometryArray.ALLOW_COLOR_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_COLOR_INDEX_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_REF_DATA_WRITE);
        setCapability(IndexedGeometryArray.ALLOW_REF_DATA_READ);

        // temporary variables to avoid 'new' calls
        m_kV0 = new Point3f();
        m_kV1 = new Point3f();
        m_kV2 = new Point3f();
        m_kE0 = new Vector3f();
        m_kE1 = new Vector3f();
        m_kN1 = new Vector3f();

        this.LUT = LUT;
        connect = new int[length2];

        for (int i = 0; i < connect.length; i++) {
            connect[i] = aiConnect[i];
        }

        vertex = new Point3f[length1];
        colors3f = new Color3f[length1];

        int index;
        heightRange = hgtRange;

        for (int i = 0; i < vertex.length; i++) {
            colors3f[i] = new Color3f();
            vertex[i] = new Point3f(akVertex[i]);
            index = MipavMath.round(((vertex[i].z + (heightRange / 2.0f)) / heightRange) * 255.0f);

            if (LUT != null) {
                colors3f[i].x = LUT[0][index] / 256.0f;
                colors3f[i].y = LUT[1][index] / 256.0f;
                colors3f[i].z = LUT[2][index] / 256.0f;
            }
        }

        setCoordinates(0, vertex);
        setCoordinateIndices(0, connect);
        computeNormals();
        setNormalIndices(0, connect);
        setColors(0, colors3f);
        setColorIndices(0, connect);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Load the quad mesh from a binary file. The caller then calls this function for each mesh. The format for a mesh
     * is
     *
     * <pre>
        int vCount;  // number of vertices
        Point3f vertices[vCount];
        Point3f normals[vCount];
        int iCount;  // number of indices in the connectivity array
        int indices[iCount];
     *  </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kIn       The file from which the triangle mesh is loaded.
     * @param      progress  DOCUMENT ME!
     * @param      added     DOCUMENT ME!
     * @param      total     DOCUMENT ME!
     *
     * @return     The loaded quad mesh.
     *
     * @exception  IOException  if there is an error reading from the file
     */
    public static ModelQuadMesh loadQMesh(RandomAccessFile kIn, ViewJProgressBar progress, int added, int total)
            throws IOException {

        Point3f[] akVertex;
        Vector3f[] akNormal;
        int[] aiConnect;
        ModelQuadMesh mq;

        try {

            // read vertices
            int iVertexCount = kIn.readInt();
            akVertex = new Point3f[iVertexCount];

            int i, index, tmpInt;
            int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
            int bufferSize = 12 * iVertexCount;
            byte[] bufferVertex = new byte[bufferSize];
            byte[] bufferNormal = new byte[bufferSize];

            progress.setLocation(200, 200);
            progress.setVisible(true);

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
            }

            progress.updateValue(added + (33 / total), true);
            bufferVertex = null;

            // read normals
            akNormal = new Vector3f[iVertexCount];

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
            bufferNormal = null;

            // read connectivity
            int iIndexCount = kIn.readInt();
            aiConnect = new int[iIndexCount];

            byte[] bufferConnect = new byte[iIndexCount * 4];
            kIn.read(bufferConnect);

            for (i = 0, index = 0; i < iIndexCount; i++) {
                b1 = bufferConnect[index++] & 0x000000ff;
                b2 = bufferConnect[index++] & 0x000000ff;
                b3 = bufferConnect[index++] & 0x000000ff;
                b4 = bufferConnect[index++] & 0x000000ff;
                aiConnect[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
            }

            progress.updateValueImmed(added + (100 / total));
            bufferConnect = null;
            mq = new ModelQuadMesh(akVertex, akNormal, aiConnect);

            for (i = 0; i < akVertex.length; i++) {
                akVertex[i] = null;
            }

            akVertex = null;

            for (i = 0; i < akNormal.length; i++) {
                akNormal[i] = null;
            }

            akNormal = null;
            aiConnect = null;

            return mq;
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Save an array of quad meshes to a text file. The format for the file is
     *
     * <pre>
       int type;  // 2 = ModelQuadMesh
       int aCount;  // number of array elements
       array of 'aCount' meshes, each of the form:
       int vCount;  // number of vertices
       vertex[0].x vertex[0].y vertex[0].z;
       :
       normal[0].x normal[0].y normal[0].z;
       :
       int tCount;  // number of quadrilaterals
       index[0] index[1] index[2] index[3]
       :
       * index[4*(tCount-1)] index[4*(tCount-1)+1] index[4*(tCount-1)+2] index[4*(tCount-1)+3]
     * </pre>
     *
     * @param      kName        The name of the file to which the components are saved.
     * @param      akComponent  The array of mesh components to save
     *
     * @exception  IOException  if the specified file could not be opened for writing.
     */
    public static void print(String kName, ModelTriangleMesh[] akComponent) throws IOException {

        if (akComponent.length == 0) {
            return;
        }

        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println('2'); // objects are ModelQuadMesh
        kOut.println(akComponent.length);

        for (int i = 0; i < akComponent.length; i++) {
            akComponent[i].print(kOut);
        }

        kOut.close();
    }

    /**
     * Save an array of quad meshes to a binary file. The format for the file is
     *
     * <pre>
        int type;        // 2 = ModelQuadMesh
        int aCount;      // number of array elements
        array of 'aCount' meshes, each of the form:
        int vCount;      // number of vertices
        Point3f vertices[vCount];
        Point3f normals[vCount];
        int iCount;      // number of indices in the connectivity array
        int indices[iCount];
     *  </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kName        The name of the file to which the components are saved.
     * @param      akComponent  The array of mesh components to be saved.
     * @param      progress     DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing to the file.
     */
    public static void save(String kName, ModelQuadMesh[] akComponent, ViewJProgressBar progress) throws IOException {

        if (akComponent.length == 0) {
            return;
        }

        RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");

        kOut.writeInt(2); // objects are ModelQuadMesh
        kOut.writeInt(akComponent.length);

        for (int i = 0; i < akComponent.length; i++) {
            akComponent[i].save(kOut, progress, i, akComponent.length);
        }

        kOut.close();
    }

    /**
     * A normal vector at a vertex is computed by averaging the triangle normals for all triangles sharing the vertex.
     * This routine computes all vertex normals.
     */
    public void computeNormals() {

        // maintain a running sum of triangle normals at each vertex
        normal = new Vector3f[vertex.length];

        int i;

        for (i = 0; i < vertex.length; i++) {
            normal[i] = new Vector3f(0.0f, 0.0f, 0.0f);
        }

        for (i = 0; i < connect.length; /**/) {

            // get indices to triangle vertices
            int iV0 = connect[i++];
            int iV1 = connect[i++];
            int iV2 = connect[i++];
            int iV3 = connect[i++];

            m_kV0 = vertex[iV0];
            m_kV1 = vertex[iV1];
            m_kV2 = vertex[iV2];
            // Pretty sure we don't need this
            // m_kV3 = vertex[iV3];

            // compute unit length triangle normal
            m_kE0.sub(m_kV1, m_kV0);
            m_kE1.sub(m_kV2, m_kV0);
            m_kN1.cross(m_kE0, m_kE1);

            // normalize (set to zero if cross product is nearly zero)
            float fLength = m_kN1.length();

            if (fLength > 1e-06) {
                m_kN1.scale(1.0f / fLength);
            } else {
                m_kN1.x = 0.0f;
                m_kN1.y = 0.0f;
                m_kN1.z = 0.0f;
            }

            // maintain the sum of normals at each vertex
            normal[iV0].add(m_kN1);
            normal[iV1].add(m_kN1);
            normal[iV2].add(m_kN1);
            normal[iV3].add(m_kN1);
        }

        // The normal vector storage was used to accumulate the sum of
        // triangle normals.  Now these vectors must be rescaled to be
        // unit length.
        for (i = 0; i < getVertexCount(); i++) {
            normal[i].normalize();
        }

        this.setNormals(0, normal);
        // setNormalRef3f(normal);
    }

    /**
     * Sets all variables to null.
     */
    public void dispose() {
        System.err.println("ModelQuadMesh.dispose()");
        m_kV0 = null;
        m_kV1 = null;
        m_kV2 = null;
        m_kE0 = null;
        m_kE1 = null;
        m_kN1 = null;
        connect = null;

        int i;

        for (i = 0; i < vertex.length; i++) {
            vertex[i] = null;
        }

        vertex = null;

        for (i = 0; i < normal.length; i++) {
            normal[i] = null;
        }

        normal = null;

        for (i = 0; i < colors3f.length; i++) {
            colors3f[i] = null;
        }

        colors3f = null;
        LUT = null;
        System.gc();
    }

    /**
     * Save the quad mesh to a text file. The format for the file is
     *
     * <pre>
       int type;    // 2 = ModelQuadMesh
       int aCount;  // 1, write the entire mesh as a single component
       int vCount;  // number of vertices
       vertex[0].x vertex[0].y vertex[0].z;
       :
       normal[0].x normal[0].y normal[0].z;
       :
       int tCount;  // number of quadrilaterals
       index[0] index[1] index[2] index[3]
       :
       index[4*(tCount-1)] index[4*(tCount-1)+1] index[4*(tCount-1)+2] index[4*(tCount-1)+3]
     * </pre>
     *
     * @param      kName  The name of file to which the triangle mesh is saved.
     *
     * @exception  IOException  If the specified file could not be opened for writing.
     */
    public void print(String kName) throws IOException {
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));
        kOut.println('2'); // object is ModelQuadMesh
        kOut.println('1'); // one component
        print(kOut);
        kOut.close();
    }

    /**
     * Save the quad mesh to a binary file. The format for the file is
     *
     * <pre>
        int type;     // 2 = ModelQuadMesh
        int aCount;   // 1, write the entire mesh as a single component
        int vCount;   // number of vertices
        Point3f vertices[vCount];
        Point3f normals[vCount];
        int iCount;  // number of indices in the connectivity array
        int indices[iCount];
     *  </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kName     The name of the file to which the triangle mesh is saved.
     * @param      progress  DOCUMENT ME!
     * @param      added     DOCUMENT ME!
     * @param      total     DOCUMENT ME!
     *
     * @exception  IOException  if the specified file could not be opened for writing.
     */
    public void save(String kName, ViewJProgressBar progress, int added, int total) throws IOException {

        RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");
        kOut.writeInt(2); // object is ModelQuadMesh
        kOut.writeInt(1); // one component
        save(kOut, progress, added, total);
        kOut.close();

        if (Preferences.debugLevel(Preferences.DEBUG_MINOR)) {
            print(JDialogBase.makeImageName(kName, "_sur.txt"));
        }
    }


    /**
     * I don't think both are needed in the new Model LUT design. /** Accessor that sets the LUT for the colors of the
     * indices.
     *
     * @param  LUT  New LUT to use when setting vertex colors.
     */
    public void setLUT(float[][] LUT) {
        this.LUT = LUT;

    }

    /**
     * Set.
     *
     * @param  lutBufferRemapped  int[]
     */
    public void setLUTBufferRemapped(int[] lutBufferRemapped) {
        this.lutBufferRemapped = lutBufferRemapped;
    }

    /**
     * This method is required for an implementor of the GeometryUpdater interface. It is not called directly by the
     * application. It sets the colors of the vertices based on the LUT. The System.gc() call is necessary because
     * otherwise the application eats up memory.
     *
     * @param  geometry  Geometry object to update (ignored).
     */
    public void updateData(Geometry geometry) {
        int index;

        float max = -Float.MAX_VALUE;
        float min = Float.MAX_VALUE;
        float range;

        for (int i = 0; i < vertex.length; i++) {

            if (vertex[i].z < min) {
                min = vertex[i].z;
            }

            if (vertex[i].z > max) {
                max = vertex[i].z;
            }
        }

        range = max - min;

        if (range == 0) {
            range = 1;
        }

        for (int i = 0; i < colors3f.length; i++) {
            index = MipavMath.round((vertex[i].z - min) / range * 255.0f);
            colors3f[i].x = LUT[0][((lutBufferRemapped[index] >> 16) & 0xff)] / 256.0f;
            colors3f[i].y = LUT[1][((lutBufferRemapped[index] >> 8) & 0xff)] / 256.0f;
            colors3f[i].z = LUT[2][(lutBufferRemapped[index] & 0xff)] / 256.0f;
        }

        setColors(0, colors3f);
        System.gc(); // tradeoff between memory usage and speed here.
    }

    /**
     * Internal support for 'void print (String)' and 'void print (String, ModelTriangleMesh[])'. ModelTriangleMesh uses
     * this function to print vertices, normals, and connectivity indices to the file. ModelClodMesh overrides this to
     * additionally print collapse records to the file
     *
     * @param      kOut  The file to which the triangle mesh is saved.
     *
     * @exception  IOException  if there is an error writing to the file.
     */
    protected void print(PrintWriter kOut) throws IOException {
        Point3f kVertex = new Point3f();
        Vector3f kNormal = new Vector3f();

        // write vertices
        kOut.println(vertex.length);

        int i;

        for (i = 0; i < vertex.length; i++) {
            kVertex = vertex[i];
            kOut.print(kVertex.x);
            kOut.print(' ');
            kOut.print(kVertex.y);
            kOut.print(' ');
            kOut.println(kVertex.z);
        }

        // write normals
        for (i = 0; i < vertex.length; i++) {
            kNormal = normal[i];
            kOut.print(kNormal.x);
            kOut.print(' ');
            kOut.print(kNormal.y);
            kOut.print(' ');
            kOut.println(kNormal.z);
        }

        // write connectivity
        int iQuadCount = connect.length / 4;
        kOut.println(iQuadCount);

        for (i = 0; i < iQuadCount; i++) {

            kOut.print(connect[4 * i]);
            kOut.print(' ');
            kOut.print(connect[(4 * i) + 1]);
            kOut.print(' ');
            kOut.print(connect[(4 * i) + 2]);
            kOut.print(' ');
            kOut.println(connect[(4 * i) + 3]);
        }
    }

    /**
     * Internal support for 'void save (String)' and 'void save (String, ModelQuadMesh[])'. ModelQuadMesh uses this
     * function to write vertices, normals, and connectivity indices to the file.
     *
     * @param      kOut      The file to which the quad mesh is saved.
     * @param      progress  DOCUMENT ME!
     * @param      added     DOCUMENT ME!
     * @param      total     DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected void save(RandomAccessFile kOut, ViewJProgressBar progress, int added, int total) throws IOException {
        Point3f kVertex = new Point3f();
        Vector3f kNormal = new Vector3f();

        // write vertices
        kOut.writeInt(vertex.length);

        byte[] bufferByte = new byte[vertex.length * 24]; // 24 = 3 (vertices) * 4 (quad) * 2 (both normal and vertex
                                                          // arrays)
        int i, index, tmpInt;

        for (i = 0, index = 0; i < vertex.length; i++) {
            kVertex = vertex[i];

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

        progress.updateValueImmed(added + (33 / total));

        // write normals
        for (i = 0; i < vertex.length; i++) {
            kNormal = normal[i];

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
        progress.updateValueImmed(added + (66 / total));

        // write connectivity
        kOut.writeInt(connect.length);

        byte[] bufferInt = new byte[connect.length * 4];

        for (i = 0, index = 0; i < connect.length; i++) {
            tmpInt = connect[i];
            bufferInt[index++] = (byte) (tmpInt >>> 24);
            bufferInt[index++] = (byte) (tmpInt >>> 16);
            bufferInt[index++] = (byte) (tmpInt >>> 8);
            bufferInt[index++] = (byte) (tmpInt & 0xff);
        }

        kOut.write(bufferInt);
        progress.updateValueImmed(added + (100 / total));
        bufferByte = null;
        bufferInt = null;
    }
}
