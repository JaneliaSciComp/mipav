package gov.nih.mipav.model.structures;


import gov.nih.mipav.view.*;

import java.util.*;

import javax.vecmath.*;


/**
 * A level surface extractor that is based on decomposing voxels into cubes, assuming a linear interpolation on the
 * cubes, and extracting triangular level sets for those cubes. The resulting level surface is a triangle mesh.
 */

public class ModelSurfaceExtractorCubes {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Bit-field identifiers. */
    private static final int EI_XMIN_YMIN = 0;

    /** DOCUMENT ME! */
    private static final int EI_XMIN_YMAX = 1;

    /** DOCUMENT ME! */
    private static final int EI_XMAX_YMIN = 2;

    /** DOCUMENT ME! */
    private static final int EI_XMAX_YMAX = 3;

    /** DOCUMENT ME! */
    private static final int EI_XMIN_ZMIN = 4;

    /** DOCUMENT ME! */
    private static final int EI_XMIN_ZMAX = 5;

    /** DOCUMENT ME! */
    private static final int EI_XMAX_ZMIN = 6;

    /** DOCUMENT ME! */
    private static final int EI_XMAX_ZMAX = 7;

    /** DOCUMENT ME! */
    private static final int EI_YMIN_ZMIN = 8;

    /** DOCUMENT ME! */
    private static final int EI_YMIN_ZMAX = 9;

    /** DOCUMENT ME! */
    private static final int EI_YMAX_ZMIN = 10;

    /** DOCUMENT ME! */
    private static final int EI_YMAX_ZMAX = 11;

    /** DOCUMENT ME! */
    private static final int FI_XMIN = 12;

    /** DOCUMENT ME! */
    private static final int FI_XMAX = 13;

    /** DOCUMENT ME! */
    private static final int FI_YMIN = 14;

    /** DOCUMENT ME! */
    private static final int FI_YMAX = 15;

    /** DOCUMENT ME! */
    private static final int FI_ZMIN = 16;

    /** DOCUMENT ME! */
    private static final int FI_ZMAX = 17;

    /** Bit mask identifiers. */
    private static final int EB_XMIN_YMIN = 1 << EI_XMIN_YMIN;

    /** DOCUMENT ME! */
    private static final int EB_XMIN_YMAX = 1 << EI_XMIN_YMAX;

    /** DOCUMENT ME! */
    private static final int EB_XMAX_YMIN = 1 << EI_XMAX_YMIN;

    /** DOCUMENT ME! */
    private static final int EB_XMAX_YMAX = 1 << EI_XMAX_YMAX;

    /** DOCUMENT ME! */
    private static final int EB_XMIN_ZMIN = 1 << EI_XMIN_ZMIN;

    /** DOCUMENT ME! */
    private static final int EB_XMIN_ZMAX = 1 << EI_XMIN_ZMAX;

    /** DOCUMENT ME! */
    private static final int EB_XMAX_ZMIN = 1 << EI_XMAX_ZMIN;

    /** DOCUMENT ME! */
    private static final int EB_XMAX_ZMAX = 1 << EI_XMAX_ZMAX;

    /** DOCUMENT ME! */
    private static final int EB_YMIN_ZMIN = 1 << EI_YMIN_ZMIN;

    /** DOCUMENT ME! */
    private static final int EB_YMIN_ZMAX = 1 << EI_YMIN_ZMAX;

    /** DOCUMENT ME! */
    private static final int EB_YMAX_ZMIN = 1 << EI_YMAX_ZMIN;

    /** DOCUMENT ME! */
    private static final int EB_YMAX_ZMAX = 1 << EI_YMAX_ZMAX;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private TransMatrix dicomMatrix;

    /** DOCUMENT ME! */
    private float[] m_afStartLocation;

    /** DOCUMENT ME! */
    private int[] m_aiData;

    /** DOCUMENT ME! */
    private int[] m_aiDirection;

    /** DOCUMENT ME! */
    private float m_fXDelta, m_fYDelta, m_fZDelta;

    /** bounds on each dimension of 3D data set and data itself. */
    private int m_iXBound, m_iYBound, m_iZBound;

    /** DOCUMENT ME! */
    private int m_iXYBound, m_iXYZBound;

    /** DOCUMENT ME! */
    private Point3f m_kCentroid = new Point3f();

    /** temporary variables to avoid 'new' calls. */
    private Vector3f m_kE0 = new Vector3f();

    /** DOCUMENT ME! */
    private Vector3f m_kE1 = new Vector3f();

    /** DOCUMENT ME! */
    private Vector3f m_kGradient = new Vector3f(0.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    private Vector3f m_kN = new Vector3f();

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
        m_iXBound = iXBound;
        m_iYBound = iYBound;
        m_iZBound = iZBound;
        m_iXYBound = iXBound * iYBound;
        m_iXYZBound = m_iXYBound * m_iZBound;
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
     * Interpolation of the image. The voxel containing the input point is determined. Within that voxel, the correct
     * tetrahedron containing the point is determined. A barycentric combination of the function values at the vertices
     * of the tetrahedron is used for the interpolated value. The barycentric coordinates are those of the input point
     * relative to the vertices of the tetrahedron.
     *
     * @param   kP  Point3f the point to be interpolated
     *
     * @return  the interpolated function value
     */
    public float getFunction(Point3f kP) {
        int iX = (int) kP.x;

        if ((iX < 0) || (iX >= (m_iXBound - 1))) {
            return 0.0f;
        }

        int iY = (int) kP.y;

        if ((iY < 0) || (iY >= (m_iYBound - 1))) {
            return 0.0f;
        }

        int iZ = (int) kP.z;

        if ((iZ < 0) || (iZ >= (m_iZBound - 1))) {
            return 0.0f;
        }

        // get image values at corners of voxel
        int i000 = iX + (m_iXBound * (iY + (m_iYBound * iZ)));
        int i100 = i000 + 1;
        int i010 = i000 + m_iXBound;
        int i110 = i010 + 1;
        int i001 = i000 + m_iXYBound;
        int i101 = i001 + 1;
        int i011 = i001 + m_iXBound;
        int i111 = i011 + 1;
        float fF000 = (float) m_aiData[i000];
        float fF100 = (float) m_aiData[i100];
        float fF010 = (float) m_aiData[i010];
        float fF110 = (float) m_aiData[i110];
        float fF001 = (float) m_aiData[i001];
        float fF101 = (float) m_aiData[i101];
        float fF011 = (float) m_aiData[i011];
        float fF111 = (float) m_aiData[i111];

        float fX1 = kP.x - iX;
        float fY1 = kP.y - iY;
        float fZ1 = kP.z - iZ;

        float fX0 = 1.0f - fX1;
        float fY0 = 1.0f - fY1;
        float fZ0 = 1.0f - fZ1;

        float fTmp00 = (fX0 * fF000) + (fX1 * fF100);
        float fTmp01 = (fX0 * fF001) + (fX1 * fF101);
        float fTmp10 = (fX0 * fF010) + (fX1 * fF110);
        float fTmp11 = (fX0 * fF011) + (fX1 * fF111);

        float fTmp0 = (fY0 * fTmp00) + (fY1 * fTmp10);
        float fTmp1 = (fY0 * fTmp01) + (fY1 * fTmp11);

        return (fZ0 * fTmp0) + (fZ1 * fTmp1);
    }

    /**
     * Interpolation of the gradient vector of the image. The voxel containing the input point is determined. A weighted
     * combination of the gradient values at the vertices is used for the interpolated value. This vector is used to
     * determine consistent normals for the triangles in the mesh representing the level surface. This is faster than
     * using ModelSurfaceTopology.getConsistentComponents(), but may fail on some pathological cases. These cases are
     * rare in 3D images from real applications.
     *
     * @param   kP  the sample space coordinates of the point to be interpolated
     *
     * @return  the gradient at the input point
     */
    public Vector3f getGradient(Point3f kP) {
        int iX = (int) kP.x;

        if ((iX < 0) || (iX >= (m_iXBound - 1))) {
            m_kGradient.x = 0.0f;
            m_kGradient.y = 0.0f;
            m_kGradient.z = 0.0f;

            return m_kGradient;
        }

        int iY = (int) kP.y;

        if ((iY < 0) || (iY >= (m_iYBound - 1))) {
            m_kGradient.x = 0.0f;
            m_kGradient.y = 0.0f;
            m_kGradient.z = 0.0f;

            return m_kGradient;
        }

        int iZ = (int) kP.z;

        if ((iZ < 0) || (iZ >= (m_iZBound - 1))) {
            m_kGradient.x = 0.0f;
            m_kGradient.y = 0.0f;
            m_kGradient.z = 0.0f;

            return m_kGradient;
        }

        // get image values at corners of voxel
        int i000 = iX + (m_iXBound * (iY + (m_iYBound * iZ)));
        int i100 = i000 + 1;
        int i010 = i000 + m_iXBound;
        int i110 = i010 + 1;
        int i001 = i000 + m_iXYBound;
        int i101 = i001 + 1;
        int i011 = i001 + m_iXBound;
        int i111 = i011 + 1;
        float fF000 = (float) m_aiData[i000];
        float fF100 = (float) m_aiData[i100];
        float fF010 = (float) m_aiData[i010];
        float fF110 = (float) m_aiData[i110];
        float fF001 = (float) m_aiData[i001];
        float fF101 = (float) m_aiData[i101];
        float fF011 = (float) m_aiData[i011];
        float fF111 = (float) m_aiData[i111];

        float fDX = kP.x - iX;
        float fDY = kP.y - iY;
        float fDZ = kP.z - iZ;

        float fOmX = 1.0f - fDX;
        float fOmY = 1.0f - fDY;
        float fOmZ = 1.0f - fDZ;

        float fTmp0 = (fOmY * (fF100 - fF000)) + (fDY * (fF110 - fF010));
        float fTmp1 = (fOmY * (fF101 - fF001)) + (fDY * (fF111 - fF011));
        m_kGradient.x = (fOmZ * fTmp0) + (fDZ * fTmp1);

        fTmp0 = (fOmX * (fF010 - fF000)) + (fDX * (fF110 - fF100));
        fTmp1 = (fOmX * (fF011 - fF001)) + (fDX * (fF111 - fF101));
        m_kGradient.y = (fOmZ * fTmp0) + (fDZ * fTmp1);

        fTmp0 = (fOmX * (fF001 - fF000)) + (fDX * (fF101 - fF100));
        fTmp1 = (fOmX * (fF011 - fF010)) + (fDX * (fF111 - fF110));
        m_kGradient.z = (fOmY * fTmp0) + (fDY * fTmp1);

        return m_kGradient;
    }

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

        HashMap kVMap = new HashMap(); // map of <Point3f,Integer>
        HashSet kTSet = new HashSet(); // set of <Triangle>
        VETable kTable = new VETable();

        for (int iZ = 0, iZP = 1; iZ < (m_iZBound - 1); iZ++, iZP++) {

            if (progressBar != null) {

                // progressBar.setMessage("Extraction on slice # " + (iZ + 1));
                progressBar.setMessage("Extraction on slice # " + iZ); // was showing numSlices + 1 as last slice
                                                                       // extracted
                progressBar.updateValue(15 + ((iZ * 35) / (m_iZBound - 1)), true);
            }

            for (int iY = 0, iYP = 1; iY < (m_iYBound - 1); iY++, iYP++) {

                for (int iX = 0, iXP = 1; iX < (m_iXBound - 1); iX++, iXP++) {

                    // get vertices on edges of box (if any)
                    int iType = getVertices(fLevel, iX, iY, iZ, kTable);

                    if (iType != 0) {

                        // get edges on faces of box
                        getXMinEdges(iX, iY, iZ, iType, kTable);
                        getXMaxEdges(iX, iY, iZ, iType, kTable);
                        getYMinEdges(iX, iY, iZ, iType, kTable);
                        getYMaxEdges(iX, iY, iZ, iType, kTable);
                        getZMinEdges(iX, iY, iZ, iType, kTable);
                        getZMaxEdges(iX, iY, iZ, iType, kTable);

                        // ear-clip the wireframe to get the triangles
                        Triangle kTri;

                        while (null != (kTri = kTable.getNextTriangle())) {
                            Point3f kV0 = kTable.get(kTri.m_iV0);
                            Point3f kV1 = kTable.get(kTri.m_iV1);
                            Point3f kV2 = kTable.get(kTri.m_iV2);

                            int iV0 = addVertex(kV0, kVMap);
                            int iV1 = addVertex(kV1, kVMap);
                            int iV2 = addVertex(kV2, kVMap);

                            // compute triangle normal assuming counterclockwise ordering
                            m_kE0.sub(kV1, kV0);
                            m_kE1.sub(kV2, kV0);
                            m_kN.cross(m_kE0, m_kE1);

                            // choose triangle orientation based on gradient direction
                            m_kCentroid.add(kV0, kV1);
                            m_kCentroid.add(kV2);
                            m_kCentroid.scale(1.0f / 3.0f);

                            Vector3f kGradient = getGradient(m_kCentroid);

                            if (kGradient.dot(m_kN) <= 0.0f) {
                                kTri.m_iV0 = iV0;
                                kTri.m_iV1 = iV1;
                                kTri.m_iV2 = iV2;
                            } else {
                                kTri.m_iV0 = iV0;
                                kTri.m_iV1 = iV2;
                                kTri.m_iV2 = iV1;
                            }

                            kTSet.add(kTri);
                        }
                    }
                }
            }
        }

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

            Point3f kV = (Point3f) kEntry.getKey();
            Integer kInt = (Integer) kEntry.getValue();

            if (dicomMatrix != null) {

                // Change the voxel coordinate into millimeters
                coord[0] = kV.x * m_fXDelta;
                coord[1] = kV.y * m_fYDelta;
                coord[2] = kV.z * m_fZDelta;

                // Convert the point to axial millimeter DICOM space
                dicomMatrix.transform(coord, tCoord);

                // Add in the DICOM origin
                tCoord[0] = tCoord[0] + m_afStartLocation[0];
                tCoord[1] = tCoord[1] + m_afStartLocation[1];
                tCoord[2] = tCoord[2] + m_afStartLocation[2];
                akVertex[kInt.intValue()] = new Point3f(tCoord[0], tCoord[1], tCoord[2]);
            } else {
                kV.x = (kV.x * m_fXDelta * m_aiDirection[0]) + m_afStartLocation[0];
                kV.y = (kV.y * m_fYDelta * m_aiDirection[1]) + m_afStartLocation[1];
                kV.z = (kV.z * m_fZDelta * m_aiDirection[2]) + m_afStartLocation[2];
                akVertex[kInt.intValue()] = kV;
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

    /**
     * Called to add the specified point to the hashmap<Point3f,Integer>. If the specified point already exists in the
     * hashmap, then it is not added. If it is added, the Integer value of the hashmap is set to the order of insertion
     * index. The Integer value of this point in the hashmap, if added or if already found in the hashmap, is returned.
     *
     * @param   kV     Point3f sample space coordinates of the point
     * @param   kVMap  HashMap hashmap with Point3f key and Integer value for adding the point if not already present
     *
     * @return  int insertion order index for the specified point
     */
    private static int addVertex(Point3f kV, HashMap kVMap) {

        if (kVMap.containsKey(kV)) {
            Integer kInt = (Integer) kVMap.get(kV);

            return kInt.intValue();
        } else {
            int iIndex = kVMap.size();
            kVMap.put(new Point3f(kV), new Integer(iIndex));

            return iIndex;
        }
    }

    /**
     * Determine which edges of the voxel are intersected by the level surface.
     *
     * @param   fLevel   float level surface value for the extraction
     * @param   iX       int x sample space coordinate for "bottom/left" voxel corner
     * @param   iY       int y sample space coordinate for "bottom/left" voxel corner
     * @param   iZ       int z sample space coordinate for "bottom/left" voxel corner
     * @param   rkTable  VETable vertex-edge table containing the point of intersection for edge intersected by the
     *                   level surface
     *
     * @return  int bit mask containing flag indicating which edges of the voxel are intersected by the level surface
     */
    private int getVertices(float fLevel, int iX, int iY, int iZ, VETable rkTable) {
        int iType = 0;

        // get image values at corners of voxel
        int i000 = iX + (m_iXBound * (iY + (m_iYBound * iZ)));
        int i100 = i000 + 1;
        int i010 = i000 + m_iXBound;
        int i110 = i010 + 1;
        int i001 = i000 + m_iXYBound;
        int i101 = i001 + 1;
        int i011 = i001 + m_iXBound;
        int i111 = i011 + 1;
        float fF000 = (float) m_aiData[i000];
        float fF100 = (float) m_aiData[i100];
        float fF010 = (float) m_aiData[i010];
        float fF110 = (float) m_aiData[i110];
        float fF001 = (float) m_aiData[i001];
        float fF101 = (float) m_aiData[i101];
        float fF011 = (float) m_aiData[i011];
        float fF111 = (float) m_aiData[i111];

        float fX0 = (float) iX;
        float fY0 = (float) iY;
        float fZ0 = (float) iZ;

        float fX1 = fX0 + 1.0f;
        float fY1 = fY0 + 1.0f;
        float fZ1 = fZ0 + 1.0f;

        // xmin-ymin edge
        float fDiff0 = fLevel - fF000;
        float fDiff1 = fLevel - fF001;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_XMIN_YMIN;
            rkTable.insert(EI_XMIN_YMIN, fX0, fY0, fZ0 + (fDiff0 / (fF001 - fF000)));
        }

        // xmin-ymax edge
        fDiff0 = fLevel - fF010;
        fDiff1 = fLevel - fF011;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_XMIN_YMAX;
            rkTable.insert(EI_XMIN_YMAX, fX0, fY1, fZ0 + (fDiff0 / (fF011 - fF010)));
        }

        // xmax-ymin edge
        fDiff0 = fLevel - fF100;
        fDiff1 = fLevel - fF101;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_XMAX_YMIN;
            rkTable.insert(EI_XMAX_YMIN, fX1, fY0, fZ0 + (fDiff0 / (fF101 - fF100)));
        }

        // xmax-ymax edge
        fDiff0 = fLevel - fF110;
        fDiff1 = fLevel - fF111;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_XMAX_YMAX;
            rkTable.insert(EI_XMAX_YMAX, fX1, fY1, fZ0 + (fDiff0 / (fF111 - fF110)));
        }

        // xmin-zmin edge
        fDiff0 = fLevel - fF000;
        fDiff1 = fLevel - fF010;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_XMIN_ZMIN;
            rkTable.insert(EI_XMIN_ZMIN, fX0, fY0 + (fDiff0 / (fF010 - fF000)), fZ0);
        }

        // xmin-zmax edge
        fDiff0 = fLevel - fF001;
        fDiff1 = fLevel - fF011;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_XMIN_ZMAX;
            rkTable.insert(EI_XMIN_ZMAX, fX0, fY0 + (fDiff0 / (fF011 - fF001)), fZ1);
        }

        // xmax-zmin edge
        fDiff0 = fLevel - fF100;
        fDiff1 = fLevel - fF110;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_XMAX_ZMIN;
            rkTable.insert(EI_XMAX_ZMIN, fX1, fY0 + (fDiff0 / (fF110 - fF100)), fZ0);
        }

        // xmax-zmax edge
        fDiff0 = fLevel - fF101;
        fDiff1 = fLevel - fF111;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_XMAX_ZMAX;
            rkTable.insert(EI_XMAX_ZMAX, fX1, fY0 + (fDiff0 / (fF111 - fF101)), fZ1);
        }

        // ymin-zmin edge
        fDiff0 = fLevel - fF000;
        fDiff1 = fLevel - fF100;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_YMIN_ZMIN;
            rkTable.insert(EI_YMIN_ZMIN, fX0 + (fDiff0 / (fF100 - fF000)), fY0, fZ0);
        }

        // ymin-zmax edge
        fDiff0 = fLevel - fF001;
        fDiff1 = fLevel - fF101;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_YMIN_ZMAX;
            rkTable.insert(EI_YMIN_ZMAX, fX0 + (fDiff0 / (fF101 - fF001)), fY0, fZ1);
        }

        // ymax-zmin edge
        fDiff0 = fLevel - fF010;
        fDiff1 = fLevel - fF110;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_YMAX_ZMIN;
            rkTable.insert(EI_YMAX_ZMIN, fX0 + (fDiff0 / (fF110 - fF010)), fY1, fZ0);
        }

        // ymax-zmax edge
        fDiff0 = fLevel - fF011;
        fDiff1 = fLevel - fF111;

        if ((fDiff0 * fDiff1) < 0.0f) {
            iType |= EB_YMAX_ZMAX;
            rkTable.insert(EI_YMAX_ZMAX, fX0 + (fDiff0 / (fF111 - fF011)), fY1, fZ1);
        }

        return iType;
    }

    /**
     * Process the edges on the x-max face of the current voxel.
     *
     * @param  iX       int x sample space coordinate for "bottom/left" voxel corner
     * @param  iY       int y sample space coordinate for "bottom/left" voxel corner
     * @param  iZ       int z sample space coordinate for "bottom/left" voxel corner
     * @param  iType    int bit mask containing flag indicating which edges of the voxel are intersected by the level
     *                  surface
     * @param  rkTable  VETable vertex-edge table containing the point of intersection for edge intersected by the level
     *                  surface
     */
    private void getXMaxEdges(int iX, int iY, int iZ, int iType, VETable rkTable) {
        int iFaceType = 0;

        if (0 != (iType & EB_XMAX_YMIN)) {
            iFaceType |= 0x01;
        }

        if (0 != (iType & EB_XMAX_YMAX)) {
            iFaceType |= 0x02;
        }

        if (0 != (iType & EB_XMAX_ZMIN)) {
            iFaceType |= 0x04;
        }

        if (0 != (iType & EB_XMAX_ZMAX)) {
            iFaceType |= 0x08;
        }

        switch (iFaceType) {

            case 0:
                return;

            case 3:
                rkTable.insert(EI_XMAX_YMIN, EI_XMAX_YMAX);
                break;

            case 5:
                rkTable.insert(EI_XMAX_YMIN, EI_XMAX_ZMIN);
                break;

            case 6:
                rkTable.insert(EI_XMAX_YMAX, EI_XMAX_ZMIN);
                break;

            case 9:
                rkTable.insert(EI_XMAX_YMIN, EI_XMAX_ZMAX);
                break;

            case 10:
                rkTable.insert(EI_XMAX_YMAX, EI_XMAX_ZMAX);
                break;

            case 12:
                rkTable.insert(EI_XMAX_ZMIN, EI_XMAX_ZMAX);
                break;

            case 15: {

                // four vertices, one per edge, need to disambiguate
                int i = (iX + 1) + (m_iXBound * (iY + (m_iYBound * iZ)));
                int iF00 = m_aiData[i]; // F(x,y,z)
                i += m_iXBound;

                int iF10 = m_aiData[i]; // F(x,y+1,z)
                i += m_iXYBound;

                int iF11 = m_aiData[i]; // F(x,y+1,z+1)
                i -= m_iXBound;

                int iF01 = m_aiData[i]; // F(x,y,z+1)
                int iDet = (iF00 * iF11) - (iF01 * iF10);

                if (iDet > 0) {

                    // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                    rkTable.insert(EI_XMAX_YMIN, EI_XMAX_ZMIN);
                    rkTable.insert(EI_XMAX_YMAX, EI_XMAX_ZMAX);
                } else if (iDet < 0) {

                    // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                    rkTable.insert(EI_XMAX_YMIN, EI_XMAX_ZMAX);
                    rkTable.insert(EI_XMAX_YMAX, EI_XMAX_ZMIN);
                } else {

                    // plus-sign configuration, add branch point to tessellation
                    rkTable.insert(FI_XMAX, rkTable.getX(EI_XMAX_ZMIN), rkTable.getY(EI_XMAX_ZMIN),
                                   rkTable.getZ(EI_XMAX_YMIN));

                    // add edges sharing the branch point
                    rkTable.insert(EI_XMAX_YMIN, FI_XMAX);
                    rkTable.insert(EI_XMAX_YMAX, FI_XMAX);
                    rkTable.insert(EI_XMAX_ZMIN, FI_XMAX);
                    rkTable.insert(EI_XMAX_ZMAX, FI_XMAX);
                }

                break;
            }
        }
    }

    /**
     * Process the edges on the x-min face of the current voxel.
     *
     * @param  iX       int x sample space coordinate for "bottom/left" voxel corner
     * @param  iY       int y sample space coordinate for "bottom/left" voxel corner
     * @param  iZ       int z sample space coordinate for "bottom/left" voxel corner
     * @param  iType    int bit mask containing flag indicating which edges of the voxel are intersected by the level
     *                  surface
     * @param  rkTable  VETable vertex-edge table containing the point of intersection for edge intersected by the level
     *                  surface
     */
    private void getXMinEdges(int iX, int iY, int iZ, int iType, VETable rkTable) {
        int iFaceType = 0;

        if (0 != (iType & EB_XMIN_YMIN)) {
            iFaceType |= 0x01;
        }

        if (0 != (iType & EB_XMIN_YMAX)) {
            iFaceType |= 0x02;
        }

        if (0 != (iType & EB_XMIN_ZMIN)) {
            iFaceType |= 0x04;
        }

        if (0 != (iType & EB_XMIN_ZMAX)) {
            iFaceType |= 0x08;
        }

        switch (iFaceType) {

            case 0:
                return;

            case 3:
                rkTable.insert(EI_XMIN_YMIN, EI_XMIN_YMAX);
                break;

            case 5:
                rkTable.insert(EI_XMIN_YMIN, EI_XMIN_ZMIN);
                break;

            case 6:
                rkTable.insert(EI_XMIN_YMAX, EI_XMIN_ZMIN);
                break;

            case 9:
                rkTable.insert(EI_XMIN_YMIN, EI_XMIN_ZMAX);
                break;

            case 10:
                rkTable.insert(EI_XMIN_YMAX, EI_XMIN_ZMAX);
                break;

            case 12:
                rkTable.insert(EI_XMIN_ZMIN, EI_XMIN_ZMAX);
                break;

            case 15: {

                // four vertices, one per edge, need to disambiguate
                int i = iX + (m_iXBound * (iY + (m_iYBound * iZ)));
                int iF00 = m_aiData[i]; // F(x,y,z)
                i += m_iXBound;

                int iF10 = m_aiData[i]; // F(x,y+1,z)
                i += m_iXYBound;

                int iF11 = m_aiData[i]; // F(x,y+1,z+1)
                i -= m_iXBound;

                int iF01 = m_aiData[i]; // F(x,y,z+1)
                int iDet = (iF00 * iF11) - (iF01 * iF10);

                if (iDet > 0) {

                    // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                    rkTable.insert(EI_XMIN_YMIN, EI_XMIN_ZMIN);
                    rkTable.insert(EI_XMIN_YMAX, EI_XMIN_ZMAX);
                } else if (iDet < 0) {

                    // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                    rkTable.insert(EI_XMIN_YMIN, EI_XMIN_ZMAX);
                    rkTable.insert(EI_XMIN_YMAX, EI_XMIN_ZMIN);
                } else {

                    // plus-sign configuration, add branch point to tessellation
                    rkTable.insert(FI_XMIN, rkTable.getX(EI_XMIN_ZMIN), rkTable.getY(EI_XMIN_ZMIN),
                                   rkTable.getZ(EI_XMIN_YMIN));

                    // add edges sharing the branch point
                    rkTable.insert(EI_XMIN_YMIN, FI_XMIN);
                    rkTable.insert(EI_XMIN_YMAX, FI_XMIN);
                    rkTable.insert(EI_XMIN_ZMIN, FI_XMIN);
                    rkTable.insert(EI_XMIN_ZMAX, FI_XMIN);
                }

                break;
            }
        }
    }

    /**
     * Process the edges on the y-max face of the current voxel.
     *
     * @param  iX       int x sample space coordinate for "bottom/left" voxel corner
     * @param  iY       int y sample space coordinate for "bottom/left" voxel corner
     * @param  iZ       int z sample space coordinate for "bottom/left" voxel corner
     * @param  iType    int bit mask containing flag indicating which edges of the voxel are intersected by the level
     *                  surface
     * @param  rkTable  VETable vertex-edge table containing the point of intersection for edge intersected by the level
     *                  surface
     */
    private void getYMaxEdges(int iX, int iY, int iZ, int iType, VETable rkTable) {
        int iFaceType = 0;

        if (0 != (iType & EB_XMIN_YMAX)) {
            iFaceType |= 0x01;
        }

        if (0 != (iType & EB_XMAX_YMAX)) {
            iFaceType |= 0x02;
        }

        if (0 != (iType & EB_YMAX_ZMIN)) {
            iFaceType |= 0x04;
        }

        if (0 != (iType & EB_YMAX_ZMAX)) {
            iFaceType |= 0x08;
        }

        switch (iFaceType) {

            case 0:
                return;

            case 3:
                rkTable.insert(EI_XMIN_YMAX, EI_XMAX_YMAX);
                break;

            case 5:
                rkTable.insert(EI_XMIN_YMAX, EI_YMAX_ZMIN);
                break;

            case 6:
                rkTable.insert(EI_XMAX_YMAX, EI_YMAX_ZMIN);
                break;

            case 9:
                rkTable.insert(EI_XMIN_YMAX, EI_YMAX_ZMAX);
                break;

            case 10:
                rkTable.insert(EI_XMAX_YMAX, EI_YMAX_ZMAX);
                break;

            case 12:
                rkTable.insert(EI_YMAX_ZMIN, EI_YMAX_ZMAX);
                break;

            case 15: {

                // four vertices, one per edge, need to disambiguate
                int i = iX + (m_iXBound * ((iY + 1) + (m_iYBound * iZ)));
                int iF00 = m_aiData[i]; // F(x,y,z)
                i++;

                int iF10 = m_aiData[i]; // F(x+1,y,z)
                i += m_iXYBound;

                int iF11 = m_aiData[i]; // F(x+1,y,z+1)
                i--;

                int iF01 = m_aiData[i]; // F(x,y,z+1)
                int iDet = (iF00 * iF11) - (iF01 * iF10);

                if (iDet > 0) {

                    // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                    rkTable.insert(EI_XMIN_YMAX, EI_YMAX_ZMIN);
                    rkTable.insert(EI_XMAX_YMAX, EI_YMAX_ZMAX);
                } else if (iDet < 0) {

                    // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                    rkTable.insert(EI_XMIN_YMAX, EI_YMAX_ZMAX);
                    rkTable.insert(EI_XMAX_YMAX, EI_YMAX_ZMIN);
                } else {

                    // plus-sign configuration, add branch point to tessellation
                    rkTable.insert(FI_YMAX, rkTable.getX(EI_YMAX_ZMIN), rkTable.getY(EI_XMIN_YMAX),
                                   rkTable.getZ(EI_XMIN_YMAX));

                    // add edges sharing the branch point
                    rkTable.insert(EI_XMIN_YMAX, FI_YMAX);
                    rkTable.insert(EI_XMAX_YMAX, FI_YMAX);
                    rkTable.insert(EI_YMAX_ZMIN, FI_YMAX);
                    rkTable.insert(EI_YMAX_ZMAX, FI_YMAX);
                }

                break;
            }
        }
    }

    /**
     * Process the edges on the y-min face of the current voxel.
     *
     * @param  iX       int x sample space coordinate for "bottom/left" voxel corner
     * @param  iY       int y sample space coordinate for "bottom/left" voxel corner
     * @param  iZ       int z sample space coordinate for "bottom/left" voxel corner
     * @param  iType    int bit mask containing flag indicating which edges of the voxel are intersected by the level
     *                  surface
     * @param  rkTable  VETable vertex-edge table containing the point of intersection for edge intersected by the level
     *                  surface
     */
    private void getYMinEdges(int iX, int iY, int iZ, int iType, VETable rkTable) {
        int iFaceType = 0;

        if (0 != (iType & EB_XMIN_YMIN)) {
            iFaceType |= 0x01;
        }

        if (0 != (iType & EB_XMAX_YMIN)) {
            iFaceType |= 0x02;
        }

        if (0 != (iType & EB_YMIN_ZMIN)) {
            iFaceType |= 0x04;
        }

        if (0 != (iType & EB_YMIN_ZMAX)) {
            iFaceType |= 0x08;
        }

        switch (iFaceType) {

            case 0:
                return;

            case 3:
                rkTable.insert(EI_XMIN_YMIN, EI_XMAX_YMIN);
                break;

            case 5:
                rkTable.insert(EI_XMIN_YMIN, EI_YMIN_ZMIN);
                break;

            case 6:
                rkTable.insert(EI_XMAX_YMIN, EI_YMIN_ZMIN);
                break;

            case 9:
                rkTable.insert(EI_XMIN_YMIN, EI_YMIN_ZMAX);
                break;

            case 10:
                rkTable.insert(EI_XMAX_YMIN, EI_YMIN_ZMAX);
                break;

            case 12:
                rkTable.insert(EI_YMIN_ZMIN, EI_YMIN_ZMAX);
                break;

            case 15: {

                // four vertices, one per edge, need to disambiguate
                int i = iX + (m_iXBound * (iY + (m_iYBound * iZ)));
                int iF00 = m_aiData[i]; // F(x,y,z)
                i++;

                int iF10 = m_aiData[i]; // F(x+1,y,z)
                i += m_iXYBound;

                int iF11 = m_aiData[i]; // F(x+1,y,z+1)
                i--;

                int iF01 = m_aiData[i]; // F(x,y,z+1)
                int iDet = (iF00 * iF11) - (iF01 * iF10);

                if (iDet > 0) {

                    // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                    rkTable.insert(EI_XMIN_YMIN, EI_YMIN_ZMIN);
                    rkTable.insert(EI_XMAX_YMIN, EI_YMIN_ZMAX);
                } else if (iDet < 0) {

                    // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                    rkTable.insert(EI_XMIN_YMIN, EI_YMIN_ZMAX);
                    rkTable.insert(EI_XMAX_YMIN, EI_YMIN_ZMIN);
                } else {

                    // plus-sign configuration, add branch point to tessellation
                    rkTable.insert(FI_YMIN, rkTable.getX(EI_YMIN_ZMIN), rkTable.getY(EI_XMIN_YMIN),
                                   rkTable.getZ(EI_XMIN_YMIN));

                    // add edges sharing the branch point
                    rkTable.insert(EI_XMIN_YMIN, FI_YMIN);
                    rkTable.insert(EI_XMAX_YMIN, FI_YMIN);
                    rkTable.insert(EI_YMIN_ZMIN, FI_YMIN);
                    rkTable.insert(EI_YMIN_ZMAX, FI_YMIN);
                }

                break;
            }
        }
    }

    /**
     * Process the edges on the z-max face of the current voxel.
     *
     * @param  iX       int x sample space coordinate for "bottom/left" voxel corner
     * @param  iY       int y sample space coordinate for "bottom/left" voxel corner
     * @param  iZ       int z sample space coordinate for "bottom/left" voxel corner
     * @param  iType    int bit mask containing flag indicating which edges of the voxel are intersected by the level
     *                  surface
     * @param  rkTable  VETable vertex-edge table containing the point of intersection for edge intersected by the level
     *                  surface
     */
    private void getZMaxEdges(int iX, int iY, int iZ, int iType, VETable rkTable) {
        int iFaceType = 0;

        if (0 != (iType & EB_XMIN_ZMAX)) {
            iFaceType |= 0x01;
        }

        if (0 != (iType & EB_XMAX_ZMAX)) {
            iFaceType |= 0x02;
        }

        if (0 != (iType & EB_YMIN_ZMAX)) {
            iFaceType |= 0x04;
        }

        if (0 != (iType & EB_YMAX_ZMAX)) {
            iFaceType |= 0x08;
        }

        switch (iFaceType) {

            case 0:
                return;

            case 3:
                rkTable.insert(EI_XMIN_ZMAX, EI_XMAX_ZMAX);
                break;

            case 5:
                rkTable.insert(EI_XMIN_ZMAX, EI_YMIN_ZMAX);
                break;

            case 6:
                rkTable.insert(EI_XMAX_ZMAX, EI_YMIN_ZMAX);
                break;

            case 9:
                rkTable.insert(EI_XMIN_ZMAX, EI_YMAX_ZMAX);
                break;

            case 10:
                rkTable.insert(EI_XMAX_ZMAX, EI_YMAX_ZMAX);
                break;

            case 12:
                rkTable.insert(EI_YMIN_ZMAX, EI_YMAX_ZMAX);
                break;

            case 15: {

                // four vertices, one per edge, need to disambiguate
                int i = iX + (m_iXBound * (iY + (m_iYBound * (iZ + 1))));
                int iF00 = m_aiData[i]; // F(x,y,z)
                i++;

                int iF10 = m_aiData[i]; // F(x+1,y,z)
                i += m_iXBound;

                int iF11 = m_aiData[i]; // F(x+1,y+1,z)
                i--;

                int iF01 = m_aiData[i]; // F(x,y+1,z)
                int iDet = (iF00 * iF11) - (iF01 * iF10);

                if (iDet > 0) {

                    // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                    rkTable.insert(EI_XMIN_ZMAX, EI_YMIN_ZMAX);
                    rkTable.insert(EI_XMAX_ZMAX, EI_YMAX_ZMAX);
                } else if (iDet < 0) {

                    // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                    rkTable.insert(EI_XMIN_ZMAX, EI_YMAX_ZMAX);
                    rkTable.insert(EI_XMAX_ZMAX, EI_YMIN_ZMAX);
                } else {

                    // plus-sign configuration, add branch point to tessellation
                    rkTable.insert(FI_ZMAX, rkTable.getX(EI_YMIN_ZMAX), rkTable.getY(EI_XMIN_ZMAX),
                                   rkTable.getZ(EI_XMIN_ZMAX));

                    // add edges sharing the branch point
                    rkTable.insert(EI_XMIN_ZMAX, FI_ZMAX);
                    rkTable.insert(EI_XMAX_ZMAX, FI_ZMAX);
                    rkTable.insert(EI_YMIN_ZMAX, FI_ZMAX);
                    rkTable.insert(EI_YMAX_ZMAX, FI_ZMAX);
                }

                break;
            }
        }
    }

    /**
     * Process the edges on the z-min face of the current voxel.
     *
     * @param  iX       int x sample space coordinate for "bottom/left" voxel corner
     * @param  iY       int y sample space coordinate for "bottom/left" voxel corner
     * @param  iZ       int z sample space coordinate for "bottom/left" voxel corner
     * @param  iType    int bit mask containing flag indicating which edges of the voxel are intersected by the level
     *                  surface
     * @param  rkTable  VETable vertex-edge table containing the point of intersection for edge intersected by the level
     *                  surface
     */
    private void getZMinEdges(int iX, int iY, int iZ, int iType, VETable rkTable) {
        int iFaceType = 0;

        if (0 != (iType & EB_XMIN_ZMIN)) {
            iFaceType |= 0x01;
        }

        if (0 != (iType & EB_XMAX_ZMIN)) {
            iFaceType |= 0x02;
        }

        if (0 != (iType & EB_YMIN_ZMIN)) {
            iFaceType |= 0x04;
        }

        if (0 != (iType & EB_YMAX_ZMIN)) {
            iFaceType |= 0x08;
        }

        switch (iFaceType) {

            case 0:
                return;

            case 3:
                rkTable.insert(EI_XMIN_ZMIN, EI_XMAX_ZMIN);
                break;

            case 5:
                rkTable.insert(EI_XMIN_ZMIN, EI_YMIN_ZMIN);
                break;

            case 6:
                rkTable.insert(EI_XMAX_ZMIN, EI_YMIN_ZMIN);
                break;

            case 9:
                rkTable.insert(EI_XMIN_ZMIN, EI_YMAX_ZMIN);
                break;

            case 10:
                rkTable.insert(EI_XMAX_ZMIN, EI_YMAX_ZMIN);
                break;

            case 12:
                rkTable.insert(EI_YMIN_ZMIN, EI_YMAX_ZMIN);
                break;

            case 15: {

                // four vertices, one per edge, need to disambiguate
                int i = iX + (m_iXBound * (iY + (m_iYBound * iZ)));
                int iF00 = m_aiData[i]; // F(x,y,z)
                i++;

                int iF10 = m_aiData[i]; // F(x+1,y,z)
                i += m_iXBound;

                int iF11 = m_aiData[i]; // F(x+1,y+1,z)
                i--;

                int iF01 = m_aiData[i]; // F(x,y+1,z)
                int iDet = (iF00 * iF11) - (iF01 * iF10);

                if (iDet > 0) {

                    // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                    rkTable.insert(EI_XMIN_ZMIN, EI_YMIN_ZMIN);
                    rkTable.insert(EI_XMAX_ZMIN, EI_YMAX_ZMIN);
                } else if (iDet < 0) {

                    // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                    rkTable.insert(EI_XMIN_ZMIN, EI_YMAX_ZMIN);
                    rkTable.insert(EI_XMAX_ZMIN, EI_YMIN_ZMIN);
                } else {

                    // plus-sign configuration, add branch point to tessellation
                    rkTable.insert(FI_ZMIN, rkTable.getX(EI_YMIN_ZMIN), rkTable.getY(EI_XMIN_ZMIN),
                                   rkTable.getZ(EI_XMIN_ZMIN));

                    // add edges sharing the branch point
                    rkTable.insert(EI_XMIN_ZMIN, FI_ZMIN);
                    rkTable.insert(EI_XMAX_ZMIN, FI_ZMIN);
                    rkTable.insert(EI_YMIN_ZMIN, FI_ZMIN);
                    rkTable.insert(EI_YMAX_ZMIN, FI_ZMIN);
                }

                break;
            }
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * The triangle just stores vertex indices and is unconcerned about the actual vertex locations. The class extends
     * Object for hashing support.
     */
    private class Triangle extends Object {

        /** indices into the vertex array. */
        public int m_iV0, m_iV1, m_iV2;

        /**
         * Create a triangle. The triangle <V0,V1,V2> is considered to be different than <V0,V2,V1>. To minimize
         * computation time in comparisons during hashing, the triangle is stored so that the minimum index occurs
         * first.
         *
         * @param  iV0  a triangle vertex
         * @param  iV1  a triangle vertex
         * @param  iV2  a triangle vertex
         */
        public Triangle(int iV0, int iV1, int iV2) {

            if (iV0 < iV1) {

                if (iV0 < iV2) {

                    // v0 is minimum
                    m_iV0 = iV0;
                    m_iV1 = iV1;
                    m_iV2 = iV2;
                } else {

                    // v2 is minimum
                    m_iV0 = iV2;
                    m_iV1 = iV0;
                    m_iV2 = iV1;
                }
            } else {

                if (iV1 < iV2) {

                    // v1 is minimum
                    m_iV0 = iV1;
                    m_iV1 = iV2;
                    m_iV2 = iV0;
                } else {

                    // v2 is minimum
                    m_iV0 = iV2;
                    m_iV1 = iV0;
                    m_iV2 = iV1;
                }
            }
        }

        /**
         * Support for hashing into a map of triangles.
         *
         * @param   kObject  a triangle for comparison to the current one
         *
         * @return  true iff the triangles are identical
         */
        public boolean equals(Object kObject) {
            Triangle kT = (Triangle) kObject;

            return (m_iV0 == kT.m_iV0) &&
                       (((m_iV1 == kT.m_iV1) && (m_iV2 == kT.m_iV2)) || ((m_iV1 == kT.m_iV2) && (m_iV2 == kT.m_iV1)));
        }

        /**
         * Support for hashing into a map of triangles.
         *
         * @return  the hash key for the triangle
         */
        public int hashCode() {
            int iCmp;

            if (m_iV1 < m_iV2) {
                iCmp = (m_iV2 << 8) ^ ((m_iV0 << 16) | m_iV1);
            } else {
                iCmp = (m_iV1 << 8) ^ ((m_iV0 << 16) | m_iV2);
            }

            return iCmp;
        }
    }

    /**
     * Vertex-edge table to support mesh topology.
     */
    private class VETable {

        /** DOCUMENT ME! */
        private Vertex[] m_akVertex = new Vertex[18];

        /**
         * Allocate a table for all possible edge vertices.
         */
        public VETable() {

            for (int i = 0; i < m_akVertex.length; i++) {
                m_akVertex[i] = new Vertex();
            }
        }

        /**
         * Access the specified edge vertex.
         *
         * @param   i  int index of the specific edge vertex
         *
         * @return  Point3f sample space coordinates of the stored vertex
         */
        public Point3f get(int i) {
            return m_akVertex[i].p;
        }

        /**
         * Loop through all edge connections extracting triangles.
         *
         * @return  Triangle new index containing the indices into the array of edge vertices, one index for each vertex
         *          in the triangle.
         */
        public Triangle getNextTriangle() {

            for (int i = 0; i < m_akVertex.length; i++) {
                Vertex rkV = m_akVertex[i];

                if (rkV.valid && (rkV.adjQuantity == 2)) {
                    Triangle rkTri = new Triangle(i, rkV.adj[0], rkV.adj[1]);
                    removeVertex(i);

                    return rkTri;
                }
            }

            // when there are no more triangles to be extracted, then
            // go through and reset all the vertices in the table so
            // that they can be reused.
            for (int i = 0; i < m_akVertex.length; i++) {
                m_akVertex[i].reset();
            }

            return null;
        }

        /**
         * Access the x sample space coordinates for the specified edge vertex.
         *
         * @param   i  int index of the specified edge vertex
         *
         * @return  float x sample space coordinate for vertex
         */
        public float getX(int i) {
            return m_akVertex[i].p.x;
        }

        /**
         * Access the y sample space coordinates for the specified edge vertex.
         *
         * @param   i  int index of the specified edge vertex
         *
         * @return  float y sample space coordinate for vertex
         */
        public float getY(int i) {
            return m_akVertex[i].p.y;
        }

        /**
         * Access the z sample space coordinates for the specified edge vertex.
         *
         * @param   i  int index of the specified edge vertex
         *
         * @return  float z sample space coordinate for vertex
         */
        public float getZ(int i) {
            return m_akVertex[i].p.z;
        }

        /**
         * Insert an edge connecting the specified vertices.
         *
         * @param  i0  int index indicating one edge in the voxel
         * @param  i1  int index indicating second edge in the voxel
         */
        public void insert(int i0, int i1) {
            Vertex rkV0 = m_akVertex[i0];
            Vertex rkV1 = m_akVertex[i1];

            rkV0.adj[rkV0.adjQuantity++] = i1;
            rkV1.adj[rkV1.adjQuantity++] = i0;
        }

        /**
         * Insert a vertex along the specified edge.
         *
         * @param  i  int index indicating an edge in the voxel
         * @param  x  float x sample space coordinates for the vertex
         * @param  y  float y sample space coordinates for the vertex
         * @param  z  float z sample space coordinates for the vertex
         */
        public void insert(int i, float x, float y, float z) {
            Vertex rkV = m_akVertex[i];
            rkV.p.set(x, y, z);
            rkV.valid = true;
        }

        /**
         * Mark the specified edge vertex as no longer being used to create triangles.
         *
         * @param  i  int index of the edge vertex
         */
        private void removeVertex(int i) {
            Vertex rkV0 = m_akVertex[i];

            int iA0 = rkV0.adj[0], iA1 = rkV0.adj[1];
            Vertex rkVA0 = m_akVertex[iA0];
            Vertex rkVA1 = m_akVertex[iA1];

            for (int j = 0; j < rkVA0.adjQuantity; j++) {

                if (rkVA0.adj[j] == i) {
                    rkVA0.adj[j] = iA1;

                    break;
                }
            }

            for (int j = 0; j < rkVA1.adjQuantity; j++) {

                if (rkVA1.adj[j] == i) {
                    rkVA1.adj[j] = iA0;

                    break;
                }
            }

            rkV0.valid = false;

            if (rkVA0.adjQuantity == 2) {

                if (rkVA0.adj[0] == rkVA0.adj[1]) {
                    rkVA0.valid = false;
                }
            }

            if (rkVA1.adjQuantity == 2) {

                if (rkVA1.adj[0] == rkVA1.adj[1]) {
                    rkVA1.valid = false;
                }
            }
        }

        /**
         * Vertex class which stores the coordinates of an edge-vertex along with the indices of adjacent vertices which
         * comprise the edges intersected by the level surface.
         */
        private class Vertex {

            /** DOCUMENT ME! */
            public int[] adj = new int[4];

            /** DOCUMENT ME! */
            public int adjQuantity;

            /** DOCUMENT ME! */
            public Point3f p = new Point3f();

            /** DOCUMENT ME! */
            public boolean valid;

            /**
             * Creates a new Vertex object.
             */
            public Vertex() {
                reset();
            }

            /**
             * DOCUMENT ME!
             */
            public void reset() {
                valid = false;
                adjQuantity = 0;
            }
        }
    }

}
