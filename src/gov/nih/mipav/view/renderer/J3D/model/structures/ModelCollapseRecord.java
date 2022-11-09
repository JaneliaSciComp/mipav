package gov.nih.mipav.view.renderer.J3D.model.structures;


import java.io.*;


/**
 * A simple triangle mesh that represents a level surface. The mesh only stores vertex positions and vertex normals. The
 * surface viewer creates a Shape3D object whose geometry is an object from ModelTriangleMesh. The surface color is
 * provided by attaching to the Shape3D object an appearance that contains a material. The vertex normals in
 * ModelTriangleMesh are used by the lighting system in conjunction with the surface material to produce the surface
 * color.
 */

public class ModelCollapseRecord {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public int[] m_aiIndex;

    /** connectivity array indices in [0..TQ-1] that contain VThrow. */
    public int m_iIQuantity;

    /** number of triangles after edge collapse. */
    public int m_iTQuantity;

    /** edge <VKeep,VThrow> collapses so that VThrow is replaced by VKeep. */
    public int m_iVKeep;

    /** number of vertices after edge collapse. */
    public int m_iVQuantity;

    /** DOCUMENT ME! */
    public int m_iVThrow;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The default constructor is used by ModelClodMesh for creating a record before loading its data from disk. It is
     * also used by the class ModelSurfaceDecimator for creating records that need to be initialized from other records
     * already created during the edge collapse process.
     */
    public ModelCollapseRecord() {
        m_iVKeep = -1;
        m_iVThrow = -1;
        m_iVQuantity = 0;
        m_iTQuantity = 0;
        m_iIQuantity = 0;
        m_aiIndex = null;
    }

    /**
     * This constructor is only used by the class ModelSurfaceDecimator for creating records from edge collapse data
     * during the edge collapse process.
     *
     * @param  iVKeep      the index for the vertex to be kept during an edge collapse
     * @param  iVThrow     the index for the vertex to be thrown away during an edge collapse
     * @param  iVQuantity  the number of vertices that are deleted from the mesh because of the collapse of edge
     *                     <VKeep,VThrow>
     * @param  iTQuantity  the number of triangles that are deleted from the mesh because of the collapse of edge
     *                     <VKeep,VThrow>
     */
    public ModelCollapseRecord(int iVKeep, int iVThrow, int iVQuantity, int iTQuantity) {
        m_iVKeep = iVKeep;
        m_iVThrow = iVThrow;
        m_iVQuantity = iVQuantity;
        m_iTQuantity = iTQuantity;
        m_iIQuantity = 0;
        m_aiIndex = null;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Support for loading the collapse records from a binary file. This function is called by
     * ModelClodMesh.load(RandomAccessFile).
     *
     * @param      kIn  the file from which the records are loaded
     *
     * @exception  IOException  if the there is an error reading from the file
     */
    public void load(RandomAccessFile kIn) throws IOException {
        m_iVKeep = kIn.readInt();
        m_iVThrow = kIn.readInt();
        m_iVQuantity = kIn.readInt();
        m_iTQuantity = kIn.readInt();
        m_iIQuantity = kIn.readInt();

        if (m_iIQuantity > 0) {
            byte[] bufferByte = new byte[m_iIQuantity * 4];
            int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
            m_aiIndex = new int[m_iIQuantity];
            kIn.read(bufferByte);

            for (int i = 0, index = 0; i < m_iIQuantity; i++) {
                b1 = bufferByte[index++] & 0x000000ff;
                b2 = bufferByte[index++] & 0x000000ff;
                b3 = bufferByte[index++] & 0x000000ff;
                b4 = bufferByte[index++] & 0x000000ff;
                m_aiIndex[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
            }
        } else {
            m_aiIndex = null;
        }
    }

    /**
     * Support for saving the collapse records to a text file. This function is called by
     * ModelClodMesh.print(PrintWriter).
     *
     * @param      kOut  the file to which the records are saved
     *
     * @exception  IOException  if the there is an error writing to the file
     */
    public void print(PrintWriter kOut) throws IOException {
        kOut.print(m_iVKeep);
        kOut.print(' ');
        kOut.print(m_iVThrow);
        kOut.print(' ');
        kOut.print(m_iVQuantity);
        kOut.print(' ');
        kOut.print(m_iTQuantity);
        kOut.print(' ');
        kOut.print(m_iIQuantity);
        kOut.print(' ');

        for (int i = 0; i < m_iIQuantity; i++) {
            kOut.print(m_aiIndex[i]);
            kOut.print(' ');
        }

        kOut.println();
    }

    /**
     * Support for saving the collapse records to a binary file. This function is called by
     * ModelClodMesh.save(RandomAccessFile).
     *
     * @param      kOut  the file to which the records are saved
     *
     * @exception  IOException  if the there is an error writing to the file
     */
    public void save(RandomAccessFile kOut) throws IOException {
        kOut.writeInt(m_iVKeep);
        kOut.writeInt(m_iVThrow);
        kOut.writeInt(m_iVQuantity);
        kOut.writeInt(m_iTQuantity);
        kOut.writeInt(m_iIQuantity);

        byte[] bufferInt = new byte[m_iIQuantity * 4];
        int tmpInt;

        for (int i = 0, index = 0; i < m_iIQuantity; i++) {
            tmpInt = m_aiIndex[i];
            bufferInt[index++] = (byte) (tmpInt >>> 24);
            bufferInt[index++] = (byte) (tmpInt >>> 16);
            bufferInt[index++] = (byte) (tmpInt >>> 8);
            bufferInt[index++] = (byte) (tmpInt & 0xff);

        }

        kOut.write(bufferInt);
    }
}
