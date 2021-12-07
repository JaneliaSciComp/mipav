package gov.nih.mipav.view.renderer.J3D.model.structures;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * A triangle mesh that represents a level surface. The mesh is stored in its highest level of detail. The mesh also has
 * an associated sequence of collapse records that store incremental changes in the vertex quantity, the triangle
 * quantity, and the triangle connectivity array. These records are used to dynamically change the level of detail of
 * the mesh. The surface viewer creates a Shape3D object whose geometry can be objects generated from an ModelClodMesh
 * object. The surface color is provided by attaching to the Shape3D object an appearance that contains a material. The
 * vertex normals in ModelClodMesh are used by the lighting system in conjunction with the surface material to produce
 * the surface color.
 */

public class ModelClodMesh {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * Java3D supports limiting the vertices to be processed by the call setValidVertexCount(int). However, there is no
     * equivalent call that supports limiting the triangles [setValidIndexCount(int) is needed]. The dynamic edge
     * collapse partially depends on changing the number of active triangles. Since this is not supported, the
     * setLOD(int) method is designed to create an ModelTriangleMesh object on each change. Not very efficient. Better
     * would be to just allow us to change the valid index count.
     */
    private static int[] direction = new int[3];

    /** DOCUMENT ME! */
    private static float[] startLocation = new float[3];

    /** DOCUMENT ME! */
    private static float[] box = new float[3];

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] m_aiConnect;

    /** the incremental changes representing the decimation. */
    private ModelCollapseRecord[] m_akRecord;

    /** the highest level of detail data. */
    private Point3f[] m_akVertex;

    /** current level of detail record, 0 <= m_iCurrentRecord <= m_akRecord.length-1. */
    private int m_iCurrentRecord;

    /** the triangle mesh corresponding to the current level of detail. */
    private ModelTriangleMesh m_kMesh;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * A triangle mesh whose vertices have a common vertex color and vertex normal vectors that are computed from the
     * geometry of the mesh itself. The collapse records form a sequence of incremental changes that are used to
     * dynamically change the level of detail by incrementing or decrementing vertex and triangle quantities.
     *
     * @param  akVertex   Array of vertices in the mesh.
     * @param  aiConnect  Connectivity array for the triangles. Each triple of indices represents one triangle. The
     *                    triangle is counterclockwise ordered as viewed by an observer outside the mesh.
     * @param  akRecord   Array of collapse records that are computed by ModelSurfaceDecimator.
     */
    public ModelClodMesh(Point3f[] akVertex, int[] aiConnect, ModelCollapseRecord[] akRecord) {
        m_akVertex = akVertex;
        m_aiConnect = aiConnect;
        m_akRecord = akRecord;

        // Initialize the mesh by starting at lowest resolution, then
        // increasing to highest resolution.  The mesh was be allocated
        // initially in case the input array is a single triangle, in which
        // case getMaximumLOD() is zero and setLOD(getMaximumLOD()) does
        // no work, including not allocating the mesh.
        m_kMesh = new ModelTriangleMesh(akVertex, aiConnect);
        m_kMesh.setGenerator(this);
        m_iCurrentRecord = 0;
        setLOD(0);
        setLOD(getMaximumLOD());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
     * Load the clod mesh from a binary file. The caller must have already opened the file and read the mesh type (0 =
     * ModelTriangleMesh, 1 = ModelClodMesh) and the number of meshes in the file. The caller then calls this function
     * for each mesh. The format for a mesh is
     *
     * <pre>
       int vCount;  // number of vertices
       Point3f vertices[vCount];
       Point3f normals[vCount];
       int iCount;  // number of indices in the connectivity array
       int indices[iCount];
       int rCount;
       ModelCollapseRecord collapses[rCount];
     * </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kIn       the file from which the triangle mesh is loaded
     * @param      progress  DOCUMENT ME!
     * @param      added     param piece
     * @param      piece     DOCUMENT ME!
     *
     * @return     the loaded triangle mesh
     *
     * @exception  IOException  if there is an error reading from the file
     */
    public static ModelClodMesh loadCMesh(RandomAccessFile kIn, ViewJProgressBar progress, int added, int piece)
            throws IOException {

        try {
            int i, index, tmpInt, prog;
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

            // read vertices
            int iVertexCount = kIn.readInt();
            Point3f[] akVertex = new Point3f[iVertexCount];
            int bufferSize = 12 * iVertexCount;
            byte[] bufferVertex = new byte[bufferSize];
            byte[] bufferNormal = new byte[bufferSize];

            progress.setLocation(200, 200);
            progress.setVisible(true);

            kIn.read(bufferVertex);
            kIn.read(bufferNormal);

            prog = Math.round((float) iVertexCount * piece / 25);

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

                if ((prog != 0) && ((i % prog) == 0)) {
                    progress.updateValueImmed(added + (i / prog));
                }
            }

            // read normals (discarded for now)
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

                if ((prog != 0) && ((i % prog) == 0)) {
                    progress.updateValueImmed(Math.round(25 / piece) + added + (i / prog));
                }
            }

            // read connectivity
            int iIndexCount = kIn.readInt();
            int[] aiConnect = new int[iIndexCount];
            byte[] bufferConnect = new byte[iIndexCount * 4];
            kIn.read(bufferConnect);
            prog = iIndexCount * piece / 25;

            for (i = 0, index = 0; i < iIndexCount; i++) {
                b1 = bufferConnect[index++] & 0x000000ff;
                b2 = bufferConnect[index++] & 0x000000ff;
                b3 = bufferConnect[index++] & 0x000000ff;
                b4 = bufferConnect[index++] & 0x000000ff;

                aiConnect[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                if ((prog != 0) && ((i % prog) == 0)) {
                    progress.updateValueImmed(Math.round(50 / piece) + added + (i / prog));
                }
            }

            int iRecordCount = kIn.readInt();
            ModelCollapseRecord[] akRecord = new ModelCollapseRecord[iRecordCount];
            prog = iRecordCount * piece / 25;

            for (i = 0; i < iRecordCount; i++) {
                akRecord[i] = new ModelCollapseRecord();
                akRecord[i].load(kIn);

                if ((prog != 0) && ((i % prog) == 0)) {
                    progress.updateValueImmed(Math.round(75 / piece) + added + (i / prog));
                }
            }

            return new ModelClodMesh(akVertex, aiConnect, akRecord);
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * Load the clod mesh from a binary file. The caller must have already opened the file and read the mesh type (0 =
     * ModelTriangleMesh, 1 = ModelClodMesh) and the number of meshes in the file. The caller then calls this function
     * for each mesh. The format for a mesh is
     *
     * <pre>
       int vCount;  // number of vertices
       Point3f vertices[vCount];
       Point3f normals[vCount];
       int iCount;  // number of indices in the connectivity array
       int indices[iCount];
       int rCount;
       ModelCollapseRecord collapses[rCount];
     * </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kIn    the file from which the triangle mesh is loaded
     * @param      pBar   DOCUMENT ME!
     * @param      added  DOCUMENT ME!
     * @param      piece  DOCUMENT ME!
     *
     * @return     the loaded triangle mesh
     *
     * @exception  IOException  if there is an error reading from the file
     */
    public static ModelClodMesh loadCMesh(RandomAccessFile kIn, JProgressBar pBar, int added, int piece)
            throws IOException {

        try {
            int i, index, tmpInt, prog;
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

            // read vertices
            int iVertexCount = kIn.readInt();
            Point3f[] akVertex = new Point3f[iVertexCount];
            int bufferSize = 12 * iVertexCount;
            byte[] bufferVertex = new byte[bufferSize];
            byte[] bufferNormal = new byte[bufferSize];

            // progress.setLocation(200, 200);
            // progress.setVisible(true);

            kIn.read(bufferVertex);
            kIn.read(bufferNormal);

            prog = Math.round((float) iVertexCount * piece / 25);

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

                if ((prog != 0) && ((i % prog) == 0)) {
                    pBar.setValue(added + (i / prog));
                    pBar.update(pBar.getGraphics());
                }
            }

            // read normals (discarded for now)
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

                if ((prog != 0) && ((i % prog) == 0)) {
                    pBar.setValue(Math.round(25 / piece) + added + (i / prog));
                    pBar.update(pBar.getGraphics());
                }
            }

            // read connectivity
            int iIndexCount = kIn.readInt();
            int[] aiConnect = new int[iIndexCount];
            byte[] bufferConnect = new byte[iIndexCount * 4];
            kIn.read(bufferConnect);
            prog = iIndexCount * piece / 25;

            for (i = 0, index = 0; i < iIndexCount; i++) {
                b1 = bufferConnect[index++] & 0x000000ff;
                b2 = bufferConnect[index++] & 0x000000ff;
                b3 = bufferConnect[index++] & 0x000000ff;
                b4 = bufferConnect[index++] & 0x000000ff;

                aiConnect[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

                if ((prog != 0) && ((i % prog) == 0)) {
                    pBar.setValue(Math.round(50 / piece) + added + (i / prog));
                    pBar.update(pBar.getGraphics());
                }
            }

            int iRecordCount = kIn.readInt();
            ModelCollapseRecord[] akRecord = new ModelCollapseRecord[iRecordCount];
            prog = iRecordCount * piece / 25;

            for (i = 0; i < iRecordCount; i++) {
                akRecord[i] = new ModelCollapseRecord();
                akRecord[i].load(kIn);

                if ((prog != 0) && ((i % prog) == 0)) {
                    pBar.setValue(Math.round(75 / piece) + added + (i / prog));
                    pBar.update(pBar.getGraphics());
                }
            }

            return new ModelClodMesh(akVertex, aiConnect, akRecord);
        } catch (IOException e) {
            return null;
        }
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
    public static void print(String kName, ModelClodMesh[] akComponent) throws IOException {

        if (akComponent.length == 0) {
            return;
        }

        PrintWriter kOut = new PrintWriter(new FileWriter(kName));

        kOut.println('1'); // objects are ModelClodMesh
        kOut.println(akComponent.length);

        for (int i = 0; i < akComponent.length; i++) {
            akComponent[i].print(kOut);
        }

        kOut.close();
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
     * @param      flip               if the y axis should be flipped - true in extract and in save of JDialogSurface To
     *                                have proper orientations in surface file if flip is true flip y and z on reading.
     * @param      direction          1 or -1 for each axis
     * @param      startLocation      DOCUMENT ME!
     * @param      box                (dim-1)*res
     * @param      inverseDicomMatrix 
     *
     * @exception  IOException  if there is an error writing to the file
     */
    public static void save(String kName, ModelClodMesh[] akComponent, boolean flip, int[] direction,
                            float[] startLocation, float[] box, TransMatrix inverseDicomMatrix) throws IOException {

        if (akComponent.length == 0) {
            return;
        }

        RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");

        kOut.writeInt(1); // objects are ModelClodMesh
        kOut.writeInt(akComponent.length);

        for (int i = 0; i < akComponent.length; i++) {
            akComponent[i].save(kOut, flip, direction, startLocation, box, inverseDicomMatrix);
        }

        kOut.close();

        if (Preferences.debugLevel(Preferences.DEBUG_MINOR)) {
            ModelClodMesh.print(JDialogBase.makeImageName(kName, "_sur.txt"), akComponent);
        }


    }

    /**
     * The current level of detail C with 0 <= C <= getMaximumLOD().
     *
     * @return  DOCUMENT ME!
     */
    public int getLOD() {
        return m_akRecord.length - 1 - m_iCurrentRecord;
    }

    /**
     * The maximum level of detail supported by the mesh. The minimum level of detail is always zero (the first collapse
     * record).
     *
     * @return  DOCUMENT ME!
     */
    public int getMaximumLOD() {
        return m_akRecord.length - 1;
    }

    /**
     * Accessor for the triangle mesh that corresponds to the current level of detail in the ModelClodMesh.
     *
     * @return  the triangle mesh managed by the clod mesh
     */
    public ModelTriangleMesh getMesh() {
        return m_kMesh;
    }

    /**
     * Save the triangle mesh to a text file. The format for the file is
     *
     * <pre>
       int type;  // 0 = ModelTriangleMesh, 1 = ModelClodMesh
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
    public void print(String kName) throws IOException {
        PrintWriter kOut = new PrintWriter(new FileWriter(kName));
        kOut.println('1'); // object is ModelClodMesh
        kOut.println('1'); // one component
        print(kOut);
        kOut.close();
    }

    /**
     * Set the current level of detail to C with 0 <= C <= getMaximumLOD().
     *
     * @param  iLOD  the desired index of the active collapse record
     */
    public void setLOD(int iLOD) {

        if ((iLOD < 0) || (iLOD >= m_akRecord.length)) {

            // LOD out of range, do nothing
            return;
        }

        int iTargetRecord = m_akRecord.length - 1 - iLOD;

        if (m_iCurrentRecord == iTargetRecord) {

            // requested LOD is current LOD, do nothing
            return;
        }

        int i, iVQuantity = 0, iTQuantity = 0;
        ModelCollapseRecord kRecord;

        // collapse mesh (if necessary)
        while (m_iCurrentRecord < iTargetRecord) {
            m_iCurrentRecord++;

            // replace indices in connectivity array
            kRecord = m_akRecord[m_iCurrentRecord];

            for (i = 0; i < kRecord.m_iIQuantity; i++) {
                m_aiConnect[kRecord.m_aiIndex[i]] = kRecord.m_iVKeep;
            }

            // reduce vertex count (vertices are properly ordered)
            iVQuantity = kRecord.m_iVQuantity;

            // reduce triangle count (triangles are properly ordered)
            iTQuantity = kRecord.m_iTQuantity;
        }

        // expand mesh (if necessary)
        while (m_iCurrentRecord > iTargetRecord) {

            // restore indices in connectivity array
            kRecord = m_akRecord[m_iCurrentRecord];

            for (i = 0; i < kRecord.m_iIQuantity; i++) {
                m_aiConnect[kRecord.m_aiIndex[i]] = kRecord.m_iVThrow;
            }

            m_iCurrentRecord--;

            ModelCollapseRecord kPrevRecord = m_akRecord[m_iCurrentRecord];

            // increase vertex count (vertices are properly ordered)
            iVQuantity = kPrevRecord.m_iVQuantity;

            // increase triangle count (triangles are properly ordered)
            iTQuantity = kPrevRecord.m_iTQuantity;
        }

        // get valid vertices
        Point3f[] akVertex = new Point3f[iVQuantity];
        System.arraycopy(m_akVertex, 0, akVertex, 0, iVQuantity);

        // get valid indices
        int[] aiConnect = new int[3 * iTQuantity];
        System.arraycopy(m_aiConnect, 0, aiConnect, 0, 3 * iTQuantity);

        // generate the mesh corresponding to the current level of detail
        m_kMesh = new ModelTriangleMesh(akVertex, aiConnect);
        m_kMesh.setGenerator(this);
    }


    /**
     * Accessor to reset the verticies associated with this clod. Used for smoothing.
     *
     * @param  verticies  New vertices for clod mesh.
     */
    public void setVerticies(Point3f[] verticies) {
        m_akVertex = verticies;
    }

    /**
     * Internal support for 'void print (String)' and 'void print (String, ModelTriangleMesh[])'. ModelTriangleMesh uses
     * this function to print vertices, normals, and connectivity indices to the file. ModelClodMesh overrides this to
     * additionally print collapse records to the file in the form:
     *
     * <pre>
       int rCount;  // number of collapse records
       r[0].keep r[0].throw r[0].vCount r[0].tcount r[0].iCount indices...
       :
     * </pre>
     *
     * @param      kOut  the file to which the clod mesh is saved
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected void print(PrintWriter kOut) throws IOException {
        m_kMesh.print(kOut);

        // write collapse records
        kOut.println(m_akRecord.length);

        for (int i = 0; i < m_akRecord.length; i++) {
            m_akRecord[i].print(kOut);
        }
    }

    /**
     * Internal support for 'void save (String)' and 'void save (String, ModelTriangleMesh[])'. ModelTriangleMesh uses
     * this function to write vertices, normals, and connectivity indices to the file. ModelClodMesh overrides this to
     * additionally write collapse records to the file in the form:
     *
     * <pre>
       int rCount;
       ModelCollapseRecord collapses[rCount];
     * </pre>
     *
     * with 4-byte quantities stored in Big Endian format.
     *
     * @param      kOut               the file to which the clod mesh is saved
     * @param      flip               if the y axis should be flipped - true in extract and in save of JDialogSurface To
     *                                have proper orientations in surface file if flip is true flip y and z on reading.
     * @param      direction          1 or -1 for each axis
     * @param      startLocation      DOCUMENT ME!
     * @param      box                (dim-1)*res
     * @param      inverseDicomMatrix  DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing to the file
     */
    protected void save(RandomAccessFile kOut, boolean flip, int[] direction, float[] startLocation, float[] box,
    			TransMatrix inverseDicomMatrix) throws IOException {
        m_kMesh.save(kOut, flip, direction, startLocation, box, inverseDicomMatrix, null);

        // write collapse records
        kOut.writeInt(m_akRecord.length);

        for (int i = 0; i < m_akRecord.length; i++) {
            m_akRecord[i].save(kOut);
        }
    }
}
