package gov.nih.mipav.view.renderer.J3D.model.structures;

import WildMagic.LibImagics.Extraction.ExtractSurfaceCubes;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.util.*;

import javax.vecmath.*;


/**
 * A level surface extractor that is based on decomposing voxels into cubes, assuming a linear interpolation on the
 * cubes, and extracting triangular level sets for those cubes. The resulting level surface is a triangle mesh.
 */

public class ModelSurfaceExtractorCubes extends ExtractSurfaceCubes {

    /** DOCUMENT ME! */
    private TransMatrix dicomMatrix;

    /** DOCUMENT ME! */
    private float[] m_afStartLocation;

    /** DOCUMENT ME! */
    private int[] m_aiDirection;

    /** DOCUMENT ME! */
    private float m_fXDelta, m_fYDelta, m_fZDelta;

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
    public ModelSurfaceExtractorCubes(int iXBound, int iYBound, int iZBound, int[] aiData, float fXDelta, float fYDelta,
                                      float fZDelta, int[] direction, float[] startLocation, TransMatrix dicomMatrix) {
        super(iXBound,iYBound,iZBound,aiData);
        m_fXDelta = fXDelta;
        m_fYDelta = fYDelta;
        m_fZDelta = fZDelta;
        m_aiDirection = direction;
        m_afStartLocation = startLocation;
        m_aiData = aiData;
        this.dicomMatrix = dicomMatrix;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Construct a level surface from the 3D image managed by the extractor.
     *
     * @param   iLevel       the desired level value, in [min(image),max(image)]
     * @param   progressBar  update to display progress during extraction
     *
     * @return  a triangle mesh that represents the level surface
     */
    public ModelTriangleMesh getLevelSurface(int iLevel, ViewJProgressBar progressBar) {
        // The extraction assumes linear interpolation by decomposition of image domain into cubes.

        // The extraction algorithm assumes that no vertex value in the
        // image is the same as the selected level surface value.  Since
        // the image contains integer values, then apply a small adjustment
        // to the input level surface value in order to avoid this.
        float[] coord = new float[3];
        float[] tCoord = new float[3];
        float fLevel = iLevel + 0.1f;

        HashSet<Triangle> kTSet = new HashSet<Triangle>();
        HashMap<WildMagic.LibFoundation.Mathematics.Vector3f,Integer> kVMap = 
        	new HashMap<WildMagic.LibFoundation.Mathematics.Vector3f,Integer> ();
        super.ExtractContour(fLevel, kVMap, kTSet);

        // pack vertices and triangle connectivity into arrays
        int iVQuantity = kVMap.size();
        int iTQuantity = kTSet.size();

        if ((iVQuantity == 0) || (iTQuantity == 0)) {
            return null;
        }

        Point3f[] akVertex = new Point3f[iVQuantity];
        Iterator kVIter = kVMap.entrySet().iterator();
        Map.Entry kEntry = null;

        while (kVIter.hasNext()) {
            kEntry = (Map.Entry) kVIter.next();

            WildMagic.LibFoundation.Mathematics.Vector3f kV =
                (WildMagic.LibFoundation.Mathematics.Vector3f) kEntry.getKey();
            Integer kInt = (Integer) kEntry.getValue();

            if (dicomMatrix != null) {

                // Change the voxel coordinate into millimeters
                coord[0] = kV.X * m_fXDelta;
                coord[1] = kV.Y * m_fYDelta;
                coord[2] = kV.Z * m_fZDelta;

                // Convert the point to axial millimeter DICOM space
                dicomMatrix.transform(coord, tCoord);

                // Add in the DICOM origin
                tCoord[0] = tCoord[0] + m_afStartLocation[0];
                tCoord[1] = tCoord[1] + m_afStartLocation[1];
                tCoord[2] = tCoord[2] + m_afStartLocation[2];
                akVertex[kInt.intValue()] = new Point3f(tCoord[0], tCoord[1], tCoord[2]);
            } else {
                kV.X = (kV.X * m_fXDelta * m_aiDirection[0]) + m_afStartLocation[0];
                kV.Y = (kV.Y * m_fYDelta * m_aiDirection[1]) + m_afStartLocation[1];
                kV.Z = (kV.Z * m_fZDelta * m_aiDirection[2]) + m_afStartLocation[2];
                akVertex[kInt.intValue()] = new Point3f( kV.X, kV.Y, kV.Z );
            }
        }

        int[] aiConnect = new int[3 * iTQuantity];
        int iIndex = 0;
        Iterator kTIter = kTSet.iterator();

        while (kTIter.hasNext()) {
            Triangle kT = (Triangle) kTIter.next();
            aiConnect[iIndex++] = kT.m_iV0;
            aiConnect[iIndex++] = kT.m_iV1;
            aiConnect[iIndex++] = kT.m_iV2;
        }

        return new ModelTriangleMesh(akVertex, aiConnect);
    }
}
