package gov.nih.mipav.view.renderer.J3D.model.structures;

import WildMagic.LibImagics.Extraction.ExtractSurfaceTetra;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.util.*;

import javax.vecmath.*;


/**
 * A level surface extractor that is based on decomposing voxels into tetrahedra, assuming a linear interpolation on the
 * tetrahedra, and extracting triangular level sets for those tetrahedra. The resulting level surface is a triangle
 * mesh. A detailed discussion of the algorithm is found in <a href=../../../LevelSetExtraction.pdf>Level Set
 * Extraction</a>
 */

public class ModelSurfaceExtractor extends ExtractSurfaceTetra {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private TransMatrix dicomMatrix = null;

    /** DOCUMENT ME! */
    private int[] direction;

    /** DOCUMENT ME! */
    private float m_fXDelta, m_fYDelta, m_fZDelta;

    /** DOCUMENT ME! */
    private float[] startLocation;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a level surface extractor for a 3D image. The delta input values are important when the voxels are not
     * cubic. For example, a typical MRI might have z-slice spacing about 5 times that of the x and y spacing. In this
     * case, dx = 1, dy = 1, and dz = 5. The idea is that 1 is a 'voxel unit' and the z-slices are spaced by 5 voxel
     * units.
     *
     * @param  iXBound        the number of columns in the 3D image
     * @param  iYBound        the number of rows in the 3D image
     * @param  iZBound        the number of slices in the 3D image
     * @param  aiData         The image data stored in order of slice indices, each slice stored in row-major order.
     *                        That is, slice z=0 is stored first, slice z=1 is stored next, and so on. In slice z=0, the
     *                        y=0 row is stored first, the y=1 row is stored next, and so on.
     * @param  fXDelta        the relative voxel x-size (in voxel units)
     * @param  fYDelta        the relative voxel y-size (in voxel units)
     * @param  fZDelta        the relative voxel z-size (in voxel units)
     * @param  direction      array of direction factors == 1 or -1.
     * @param  startLocation  array of startLocation
     * @param  dicomMatrix    DOCUMENT ME!
     */
    public ModelSurfaceExtractor(int iXBound, int iYBound, int iZBound, int[] aiData, float fXDelta, float fYDelta,
                                 float fZDelta, int[] direction, float[] startLocation, TransMatrix dicomMatrix) {
        super(iXBound,iYBound,iZBound,aiData);

        m_fXDelta = fXDelta;
        m_fYDelta = fYDelta;
        m_fZDelta = fZDelta;
        this.direction = direction;
        this.startLocation = startLocation;
        this.dicomMatrix = dicomMatrix;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Construct a level surface from the 3D image managed by the extractor.
     *
     * @param   iLevel       the desired level value, in [min(image),max(image)]
     * @param   progressBar  DOCUMENT ME!
     *
     * @return  a triangle mesh that represents the level surface
     */
    public ModelTriangleMesh get(float iLevel, ViewJProgressBar progressBar) {


        HashSet<Triangle> kTSet = new HashSet<Triangle>();
        HashMap<Vertex,Integer> kVMap = new HashMap<Vertex,Integer> ();
        super.ExtractContour((int)iLevel, kVMap, kTSet);

        // The extraction assumes linear interpolation (decomposition of image
        // domain into tetrahedra).  To make the extraction fast, the arrays
        // make no attempt to store only unique values.  This supports rapid
        // interactive exploration of various level sets in an image until a
        // desired one is found.  At that time call makeUnique to eliminate
        // the redundant information.
        float[] coord = new float[3];
        float[] tCoord = new float[3];

        // pack vertices and triangle connectivity into arrays
        int iVQuantity = m_kVMap.size();
        int iTQuantity = m_kTSet.size();

        if ((iVQuantity == 0) || (iTQuantity == 0)) {
            return null;
        }

        Point3f[] akVertex = new Point3f[iVQuantity];
        Iterator kVIter = m_kVMap.entrySet().iterator();
        Map.Entry kEntry = null;

        while (kVIter.hasNext()) {
            kEntry = (Map.Entry) kVIter.next();

            Vertex kV = (Vertex) kEntry.getKey();
            Integer kInt = (Integer) kEntry.getValue();

            // Get floating point vertex coordinates
            float fX = (kV.m_iXNumer / (float) kV.m_iXDenom);
            float fY = (kV.m_iYNumer / (float) kV.m_iYDenom);

            // In AlgorithmExtractSurface.extractSurface only padded
            // in the z direction with 1 blank slice in front and
            // 1 blank slice in back.
            float fZ = (kV.m_iZNumer / (float) kV.m_iZDenom) - 1.0f;

            if (dicomMatrix != null) {

                // Change the voxel coordinate into millimeters
                coord[0] = fX * m_fXDelta;
                coord[1] = fY * m_fYDelta;
                coord[2] = fZ * m_fZDelta;

                // Convert the point to axial millimeter DICOM space
                dicomMatrix.transform(coord, tCoord);

                // Add in the DICOM origin
                tCoord[0] = tCoord[0] + startLocation[0];
                tCoord[1] = tCoord[1] + startLocation[1];
                tCoord[2] = tCoord[2] + startLocation[2];
                akVertex[kInt.intValue()] = new Point3f(tCoord[0], tCoord[1], tCoord[2]);
            } else {
                fX = (fX * m_fXDelta * direction[0]) + startLocation[0];
                fY = (fY * m_fYDelta * direction[1]) + startLocation[1];
                fZ = (fZ * m_fZDelta * direction[2]) + startLocation[2];

                akVertex[kInt.intValue()] = new Point3f(fX, fY, fZ);
            }
        }

        int[] aiConnect = new int[3 * iTQuantity];
        int iIndex = 0;

        Iterator kTIter = m_kTSet.iterator();

        while (kTIter.hasNext()) {
            Triangle kT = (Triangle) kTIter.next();
            aiConnect[iIndex++] = kT.m_iV0;
            aiConnect[iIndex++] = kT.m_iV1;
            aiConnect[iIndex++] = kT.m_iV2;
        }

        return new ModelTriangleMesh(akVertex, aiConnect);
    }
}
