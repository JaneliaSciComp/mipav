package gov.nih.mipav.view.renderer.surfaceview.flythruview;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.*;

import java.io.*;

import javax.swing.JProgressBar;

import javax.vecmath.*;
import javax.media.j3d.*;


/**
 * Class used to load a surface stored in an file using the SUR format.
 * Only loads the first mesh in the file if there is more than one.
 */
public class SurfaceLoaderSUR
{
    /** FileFilter instance for this file loader which works with the JFileChooser. */
    static private FileFilterExt ms_kFileFilter = null;

    /** Triangle mesh representation for the surface. */
    private ModelTriangleMesh m_kTriangleMesh = null;

    /**
     * Get access to FileFilter (to be used in JFileChooser) for the types
     * of files that can be loaded by this class.
     * @return FileFilter-derived instance to select SUR files
     */
    static public FileFilterExt getFileFilter()
    {
        // Create the static instance only upon first access.
        if (null == ms_kFileFilter)
        {
            ms_kFileFilter = new FileFilterExt("Surface Files (*.sur)");
            ms_kFileFilter.addExtension(".sur");
        }

        return ms_kFileFilter;
    }

    /**
     * Open the specified file and load the surface data, stored in the
     * SUR surface format, from it.
     * @param kFilename String name of the file from which the surface data is
     * to be loaded
     * @throws IOException if an error occurs when opening the
     * surface file or reading data from it
     */
    public SurfaceLoaderSUR (String kFilename) throws IOException {
        JProgressBar rendererProgressBar = ViewJFrameVolumeView.getRendererProgressBar();
        
        // Open the specified file.
        RandomAccessFile kFile = new RandomAccessFile(kFilename, "r");

        // Read in the type and number of meshes.
        int iMeshType = kFile.readInt();
        int iMeshCount = kFile.readInt();

        // Only load the first mesh.
        if (iMeshCount > 0)
        {
            // Meshes are a triangle mesh?
            if (0 == iMeshType)
            {
                m_kTriangleMesh = ModelTriangleMesh.loadTMesh(kFile, rendererProgressBar, 1, 1);
            }

            // Meshes are a CLOD mesh?
            else if (1 == iMeshType)
            {
                ModelClodMesh kClodMesh = ModelClodMesh.loadCMesh(kFile, rendererProgressBar, 1, 1);
                m_kTriangleMesh = kClodMesh.getMesh();
            }
        }

        // Recompute the vertex normals by regenerating the mesh
        // without the previously loaded normals.
        if (null != m_kTriangleMesh)
        {
            Point3f[] akVertex = new Point3f[m_kTriangleMesh.getVertexCount()];
            for (int iVertex = 0; iVertex < akVertex.length; iVertex++)
            {
                akVertex[iVertex] = new Point3f();
            }
            m_kTriangleMesh.getCoordinates(0, akVertex);

            int[] aiConnect = new int[m_kTriangleMesh.getIndexCount()];
            m_kTriangleMesh.getCoordinateIndices(0, aiConnect);

            m_kTriangleMesh = new ModelTriangleMesh(akVertex, aiConnect);
        }
    }

    /**
     * Access the triangle mesh representation for the loaded surface data.
     * @return ModelTriangleMesh-derived instance for the loaded surface data
     */
    public ModelTriangleMesh getTriangleMesh()
    {
        return m_kTriangleMesh;
    }
}
