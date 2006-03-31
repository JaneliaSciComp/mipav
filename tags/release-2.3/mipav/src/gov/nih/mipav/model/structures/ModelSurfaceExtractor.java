package gov.nih.mipav.model.structures;

import java.util.*;
import javax.vecmath.*;
import gov.nih.mipav.view.*;

/**
 * A level surface extractor that is based on decomposing voxels into
 * tetrahedra, assuming a linear interpolation on the tetrahedra, and
 * extracting triangular level sets for those tetrahedra.  The resulting
 * level surface is a triangle mesh.  A detailed discussion of the algorithm
 * is found in
 * <a href=../../../LevelSetExtraction.pdf>Level Set Extraction</a>
 */

public class ModelSurfaceExtractor
{
    /**
     * Create a level surface extractor for a 3D image.  The delta input
     * values are important when the voxels are not cubic.  For example, a
     * typical MRI might have z-slice spacing about 5 times that of the
     * x and y spacing.  In this case, dx = 1, dy = 1, and dz = 5.  The idea
     * is that 1 is a 'voxel unit' and the z-slices are spaced by 5 voxel
     * units.
     *
     * @param iXBound the number of columns in the 3D image
     * @param iYBound the number of rows in the 3D image
     * @param iZBound the number of slices in the 3D image
     * @param aiData The image data stored in order of slice indices,
     *   each slice stored in row-major order.  That is, slice z=0 is
     *   stored first, slice z=1 is stored next, and so on.  In slice z=0,
     *   the y=0 row is stored first, the y=1 row is stored next, and so on.
     * @param fXDelta the relative voxel x-size (in voxel units)
     * @param fYDelta the relative voxel y-size (in voxel units)
     * @param fZDelta the relative voxel z-size (in voxel units)
     * @param direction array of direction factors == 1 or -1.
     * @param startLocation array of startLocation
     * @param dicomMatrix
     */
    public ModelSurfaceExtractor (int iXBound, int iYBound, int iZBound,
        int[] aiData, float fXDelta, float fYDelta, float fZDelta,
        int[] direction, float[] startLocation,
        TransMatrix dicomMatrix)
    {
        m_iXBound = iXBound;
        m_iYBound = iYBound;
        m_iZBound = iZBound;
        m_fXDelta = fXDelta;
        m_fYDelta = fYDelta;
        m_fZDelta = fZDelta;
        this.direction = direction;
        this.startLocation = startLocation;
        this.dicomMatrix = dicomMatrix;
        m_iXYProduct = m_iXBound*m_iYBound;
        m_iXYZProduct = m_iXYProduct*m_iZBound;
        m_aiData = aiData;

        m_kVMap = new HashMap();
        m_kTSet = new HashSet();

        // temporary variables to avoid 'new' calls
        m_kV0 = new Point3f();
        m_kV1 = new Point3f();
        m_kV2 = new Point3f();
        m_kE0 = new Vector3f();
        m_kE1 = new Vector3f();
        m_kN = new Vector3f();
        m_kCentroid = new Point3f();
        m_kGradient = new Vector3f(0.0f,0.0f,0.0f);
    }

    /**
     * Interpolation of the image.  The voxel containing the input point is
     * determined.  Within that voxel, the correct tetrahedron containing the
     * point is determined.  A barycentric combination of the function values
     * at the vertices of the tetrahedron is used for the interpolated value.
     * The barycentric coordinates are those of the input point relative to
     * the vertices of the tetrahedron.
     *
     * @param kP - (x,y,z) the point to be interpolated
     * @return the interpolated function value
     */
    public float getFunction (Point3f kP)
    {
        int iX = (int) kP.x;
        if ( iX < 0 || iX >= m_iXBound-1 )
            return 0.0f;

        int iY = (int) kP.y;
        if ( iY < 0 || iY >= m_iYBound-1 )
            return 0.0f;

        int iZ = (int) kP.z;
        if ( iZ < 0 || iZ >= m_iZBound-1 )
            return 0.0f;

        float fDX = kP.x - iX, fDY = kP.y - iY, fDZ = kP.z - iZ;

        int i000 = iX + m_iXBound*(iY + m_iYBound*iZ);
        int i100 = i000 + 1;
        int i010 = i000 + m_iXBound;
        int i110 = i100 + m_iXBound;
        int i001 = i000 + m_iXYProduct;
        int i101 = i100 + m_iXYProduct;
        int i011 = i010 + m_iXYProduct;
        int i111 = i110 + m_iXYProduct;
        float fF000 = (float) m_aiData[i000];
        float fF100 = (float) m_aiData[i100];
        float fF010 = (float) m_aiData[i010];
        float fF110 = (float) m_aiData[i110];
        float fF001 = (float) m_aiData[i001];
        float fF101 = (float) m_aiData[i101];
        float fF011 = (float) m_aiData[i011];
        float fF111 = (float) m_aiData[i111];
        float fC0, fC1, fC2, fInterp;

        if ( ((iX & 1) ^ (iY & 1) ^ (iZ & 1)) != 0 )
        {
            if ( fDX - fDY - fDZ >= 0.0f )
            {
                // 1205
                fInterp =
                    (1.0f-(1.0f-fDX)-fDY-fDZ)*fF100 +
                    (1.0f-fDX)*fF000 +
                    fDY*fF110 +
                    fDZ*fF101;
            }
            else if ( fDX - fDY + fDZ <= 0.0f )
            {
                // 3027
                fInterp =
                    (1.0f-fDX-(1.0f-fDY)-fDZ)*fF010 +
                    fDX*fF110 +
                    (1.0f-fDY)*fF000 +
                    fDZ*fF011;
            }
            else if ( fDX + fDY - fDZ <= 0.0f )
            {
                // 4750
                fInterp =
                    (1.0f-fDX-fDY-(1-fDZ))*fF001 +
                    fDX*fF101 +
                    fDY*fF011 +
                    (1.0f-fDZ)*fF000;
            }
            else if ( fDX + fDY + fDZ >= 2.0f )
            {
                // 6572
                fInterp =
                    (1.0f-(1.0f-fDX)-(1.0f-fDY)-(1.0f-fDZ))*fF111 +
                    (1.0f-fDX)*fF011 +
                    (1.0f-fDY)*fF101 +
                    (1.0f-fDZ)*fF110;
            }
            else
            {
                // 0752
                fC0 = 0.5f*(-fDX+fDY+fDZ);
                fC1 = 0.5f*(fDX-fDY+fDZ);
                fC2 = 0.5f*(fDX+fDY-fDZ);
                fInterp =
                    (1.0f-fC0-fC1-fC2)*fF000 +
                    fC0*fF011 +
                    fC1*fF101 +
                    fC2*fF110;
            }
        }
        else
        {
            if ( fDX + fDY + fDZ <= 1.0f )
            {
                // 0134
                fInterp =
                    (1.0f-fDX-fDY-fDZ)*fF000 +
                    fDX*fF100 +
                    fDY*fF010 +
                    fDZ*fF001;
            }
            else if ( fDX + fDY - fDZ >= 1.0f )
            {
                // 2316
                fInterp =
                    (1.0f-(1.0f-fDX)-(1.0f-fDY)-fDZ)*fF110 +
                    (1.0f-fDX)*fF010 +
                    (1.0f-fDY)*fF100 +
                    fDZ*fF111;
            }
            else if ( fDX - fDY + fDZ >= 1.0f )
            {
                // 5461
                fInterp =
                    (1.0f-(1.0f-fDX)-fDY-(1.0f-fDZ))*fF101 +
                    (1.0f-fDX)*fF001 +
                    fDY*fF111 +
                    (1.0f-fDZ)*fF100;
            }
            else if ( -fDX + fDY + fDZ >= 1.0f )
            {
                // 7643
                fInterp =
                    (1.0f-fDX-(1.0f-fDY)-(1.0f-fDZ))*fF011 +
                    fDX*fF111 +
                    (1.0f-fDY)*fF001 +
                    (1.0f-fDZ)*fF010;
            }
            else
            {
                // 6314
                fC0 = 0.5f*((1.0f-fDX)-(1.0f-fDY)+(1.0f-fDZ));
                fC1 = 0.5f*(-(1.0f-fDX)+(1.0f-fDY)+(1.0f-fDZ));
                fC2 = 0.5f*((1.0f-fDX)+(1.0f-fDY)-(1.0f-fDZ));
                fInterp =
                    (1.0f-fC0-fC1-fC2)*fF111 +
                    fC0*fF010 +
                    fC1*fF100 +
                    fC2*fF001;
            }
        }

        return fInterp;
    }

    /**
     * Interpolation of the gradient vector of the image.  The voxel
     * containing the input point is determined.  Within that voxel, the
     * correct tetrahedron containing the point is determined.  A barycentric
     * combination of the gradient values at the vertices of the tetrahedron
     * is used for the interpolated value.  The barycentric coordinates are
     * those of the input point relative to the vertices of the tetrahedron.
     * <br>
     * This vector is used to determine consistent normals for the triangles
     * in the mesh representing the level surface.  This is faster than using
     * ModelSurfaceTopology.getConsistentComponents(), but may fail on some
     * pathological cases.  These cases are rare in 3D images from real
     * applications.
     *
     * @param kP the point to be interpolated
     * @return the gradient at the input point
     */
    public Vector3f getGradient (Point3f kP)
    {
        int iX = (int) kP.x;
        if ( iX < 0 || iX >= m_iXBound-1 )
        {
            m_kGradient.x = 0.0f;
            m_kGradient.y = 0.0f;
            m_kGradient.z = 0.0f;
            return m_kGradient;
        }

        int iY = (int) kP.y;
        if ( iY < 0 || iY >= m_iYBound-1 )
        {
            m_kGradient.x = 0.0f;
            m_kGradient.y = 0.0f;
            m_kGradient.z = 0.0f;
            return m_kGradient;
        }

        int iZ = (int) kP.z;
        if ( iZ < 0 || iZ >= m_iZBound-1 )
        {
            m_kGradient.x = 0.0f;
            m_kGradient.y = 0.0f;
            m_kGradient.z = 0.0f;
            return m_kGradient;
        }

        float fDX = kP.x - iX, fDY = kP.y - iY, fDZ = kP.z - iZ;

        int i000 = iX + m_iXBound*(iY + m_iYBound*iZ);
        int i100 = i000 + 1;
        int i010 = i000 + m_iXBound;
        int i110 = i100 + m_iXBound;
        int i001 = i000 + m_iXYProduct;
        int i101 = i100 + m_iXYProduct;
        int i011 = i010 + m_iXYProduct;
        int i111 = i110 + m_iXYProduct;
        float fF000 = (float) m_aiData[i000];
        float fF100 = (float) m_aiData[i100];
        float fF010 = (float) m_aiData[i010];
        float fF110 = (float) m_aiData[i110];
        float fF001 = (float) m_aiData[i001];
        float fF101 = (float) m_aiData[i101];
        float fF011 = (float) m_aiData[i011];
        float fF111 = (float) m_aiData[i111];

        if ( ((iX & 1) ^ (iY & 1) ^ (iZ & 1)) != 0 )
        {
            if ( fDX - fDY - fDZ >= 0.0f )
            {
                // 1205
                m_kGradient.x = + fF100 - fF000;
                m_kGradient.y = - fF100 + fF110;
                m_kGradient.z = - fF100 + fF101;
            }
            else if ( fDX - fDY + fDZ <= 0.0f )
            {
                // 3027
                m_kGradient.x = - fF010 + fF110;
                m_kGradient.y = + fF010 - fF000;
                m_kGradient.z = - fF010 + fF011;
            }
            else if ( fDX + fDY - fDZ <= 0.0f )
            {
                // 4750
                m_kGradient.x = - fF001 + fF101;
                m_kGradient.y = - fF001 + fF011;
                m_kGradient.z = + fF001 - fF000;
            }
            else if ( fDX + fDY + fDZ >= 2.0f )
            {
                // 6572
                m_kGradient.x = + fF111 - fF011;
                m_kGradient.y = + fF111 - fF101;
                m_kGradient.z = + fF111 - fF110;
            }
            else
            {
                // 0752
                m_kGradient.x = 0.5f*(-fF000-fF011+fF101+fF110);
                m_kGradient.y = 0.5f*(-fF000+fF011-fF101+fF110);
                m_kGradient.z = 0.5f*(-fF000+fF011+fF101-fF110);
            }
        }
        else
        {
            if ( fDX + fDY + fDZ <= 1.0f )
            {
                // 0134
                m_kGradient.x = - fF000 + fF100;
                m_kGradient.y = - fF000 + fF010;
                m_kGradient.z = - fF000 + fF001;
            }
            else if ( fDX + fDY - fDZ >= 1.0f )
            {
                // 2316
                m_kGradient.x = + fF110 - fF010;
                m_kGradient.y = + fF110 - fF100;
                m_kGradient.z = - fF110 + fF111;
            }
            else if ( fDX - fDY + fDZ >= 1.0f )
            {
                // 5461
                m_kGradient.x = + fF101 - fF001;
                m_kGradient.y = - fF101 + fF111;
                m_kGradient.z = + fF101 - fF100;
            }
            else if ( -fDX + fDY + fDZ >= 1.0f )
            {
                // 7643
                m_kGradient.x = - fF011 + fF111;
                m_kGradient.y = + fF011 - fF001;
                m_kGradient.z = + fF011 - fF010;
            }
            else
            {
                // 6314
                m_kGradient.x = 0.5f*(fF111-fF010+fF100-fF001);
                m_kGradient.y = 0.5f*(fF111+fF010-fF100-fF001);
                m_kGradient.z = 0.5f*(fF111-fF010-fF100+fF001);
            }
        }

        return m_kGradient;
    }

    /**
     * Construct a level surface from the 3D image managed by the extractor.
     *
     * @param iLevel the desired level value, in [min(image),max(image)]
     * @return a triangle mesh that represents the level surface
     */
    public ModelTriangleMesh get (float iLevel, ViewJProgressBar progressBar)
    {
        // The extraction assumes linear interpolation (decomposition of image
        // domain into tetrahedra).  To make the extraction fast, the arrays
        // make no attempt to store only unique values.  This supports rapid
        // interactive exploration of various level sets in an image until a
        // desired one is found.  At that time call makeUnique to eliminate
        // the redundant information.
        float coord[] = new float[3];
        float tCoord[] = new float[3];

        m_kVMap.clear();
        m_kTSet.clear();
        m_iNextIndex = 0;

        // adjust image so level set is F(x,y,z) = 0
        int iIndex;
        for (iIndex = 0; iIndex < m_iXYZProduct; iIndex++)
            m_aiData[iIndex] -= iLevel;

        for (int iZ = 0, iZP = 1; iZ < m_iZBound-1; iZ++, iZP++)
        {
            if (progressBar != null) {
                //progressBar.setMessage("Extraction on slice # " + (iZ + 1));
                progressBar.setMessage("Extraction on slice # " + iZ); // was showing numSlices + 1 as last slice extracted
                progressBar.updateValue(15 + (iZ*35)/(m_iZBound - 1), true);
            }
            int iZParity = (iZ & 1);
            for (int iY = 0, iYP = 1; iY < m_iYBound-1; iY++, iYP++)
            {
                int iYParity = (iY & 1);
                for (int iX = 0, iXP = 1; iX < m_iXBound-1; iX++, iXP++)
                {
                    int iXParity = (iX & 1);

                    int iI000 = iX + m_iXBound*(iY + m_iYBound*iZ);
                    int iI100 = iI000 + 1;
                    int iI010 = iI000 + m_iXBound;
                    int iI110 = iI100 + m_iXBound;
                    int iI001 = iI000 + m_iXYProduct;
                    int iI101 = iI100 + m_iXYProduct;
                    int iI011 = iI010 + m_iXYProduct;
                    int iI111 = iI110 + m_iXYProduct;
                    int iF000 = m_aiData[iI000];
                    int iF100 = m_aiData[iI100];
                    int iF010 = m_aiData[iI010];
                    int iF110 = m_aiData[iI110];
                    int iF001 = m_aiData[iI001];
                    int iF101 = m_aiData[iI101];
                    int iF011 = m_aiData[iI011];
                    int iF111 = m_aiData[iI111];

                    if ( (iXParity ^ iYParity ^ iZParity) != 0 )
                    {
                        // 1205
                        processTetrahedron(iLevel,iXP,iY,iZ,iF100,iXP,iYP,iZ,
                            iF110,iX,iY,iZ,iF000,iXP,iY,iZP,iF101);

                        // 3027
                        processTetrahedron(iLevel,iX,iYP,iZ,iF010,iX,iY,iZ,
                            iF000,iXP,iYP,iZ,iF110,iX,iYP,iZP,iF011);

                        // 4750
                        processTetrahedron(iLevel,iX,iY,iZP,iF001,iX,iYP,iZP,
                            iF011,iXP,iY,iZP,iF101,iX,iY,iZ,iF000);

                        // 6572
                        processTetrahedron(iLevel,iXP,iYP,iZP,iF111,iXP,iY,
                            iZP,iF101,iX,iYP,iZP,iF011,iXP,iYP,iZ,iF110);

                        // 0752
                        processTetrahedron(iLevel,iX,iY,iZ,iF000,iX,iYP,iZP,
                            iF011,iXP,iY,iZP,iF101,iXP,iYP,iZ,iF110);
                    }
                    else
                    {
                        // 0134
                        processTetrahedron(iLevel,iX,iY,iZ,iF000,iXP,iY,iZ,
                            iF100,iX,iYP,iZ,iF010,iX,iY,iZP,iF001);

                        // 2316
                        processTetrahedron(iLevel,iXP,iYP,iZ,iF110,iX,iYP,iZ,
                            iF010,iXP,iY,iZ,iF100,iXP,iYP,iZP,iF111);

                        // 5461
                        processTetrahedron(iLevel,iXP,iY,iZP,iF101,iX,iY,iZP,
                            iF001,iXP,iYP,iZP,iF111,iXP,iY,iZ,iF100);

                        // 7643
                        processTetrahedron(iLevel,iX,iYP,iZP,iF011,iXP,iYP,
                            iZP,iF111,iX,iY,iZP,iF001,iX,iYP,iZ,iF010);

                        // 6314
                        processTetrahedron(iLevel,iXP,iYP,iZP,iF111,iX,iYP,
                            iZ,iF010,iXP,iY,iZ,iF100,iX,iY,iZP,iF001);
                    }
                }
            }
        }

        // readjust image so level set is F(x,y,z) = L
        for (iIndex = 0; iIndex < m_iXYZProduct; iIndex++)
            m_aiData[iIndex] += iLevel;

        // pack vertices and triangle connectivity into arrays
        int iVQuantity = m_kVMap.size();
        int iTQuantity = m_kTSet.size();
        if ( iVQuantity == 0 || iTQuantity == 0 ) {
            return null;
    	}

        Point3f[] akVertex = new Point3f[iVQuantity];
        Iterator kVIter = m_kVMap.entrySet().iterator();
        Map.Entry kEntry = null;
        while ( kVIter.hasNext() )
        {
            kEntry = (Map.Entry) kVIter.next();
            Vertex kV = (Vertex) kEntry.getKey();
            Integer kInt = (Integer) kEntry.getValue();

            // Get floating point vertex coordinates
            float fX = (kV.m_iXNumer/(float)kV.m_iXDenom);
            float fY = (kV.m_iYNumer/(float)kV.m_iYDenom);
            // In AlgorithmExtractSurface.extractSurface only padded
            // in the z direction with 1 blank slice in front and
            // 1 blank slice in back.
            float fZ = (kV.m_iZNumer/(float)kV.m_iZDenom) - 1.0f;
            if (dicomMatrix != null) {
              // Change the voxel coordinate into millimeters
              coord[0] = fX * m_fXDelta;
              coord[1] = fY * m_fYDelta;
              coord[2] = fZ * m_fZDelta;
              // Convert the point to axial millimeter DICOM space
              dicomMatrix.transform(coord,tCoord);
              // Add in the DICOM origin
              tCoord[0] = tCoord[0] + startLocation[0];
              tCoord[1] = tCoord[1] + startLocation[1];
              tCoord[2] = tCoord[2] + startLocation[2];
              akVertex[kInt.intValue()] = new Point3f(tCoord[0],tCoord[1],
                                                      tCoord[2]);
            }
            else {
              fX = fX * m_fXDelta * direction[0] + startLocation[0];
              fY = fY * m_fYDelta * direction[1] + startLocation[1];
              fZ = fZ * m_fZDelta * direction[2] + startLocation[2];

              akVertex[kInt.intValue()] = new Point3f(fX, fY, fZ);
            }
        }

        int[] aiConnect = new int[3*iTQuantity];
        iIndex = 0;
        Iterator kTIter = m_kTSet.iterator();
        while ( kTIter.hasNext() )
        {
            Triangle kT = (Triangle) kTIter.next();
            aiConnect[iIndex++] = kT.m_iV0;
            aiConnect[iIndex++] = kT.m_iV1;
            aiConnect[iIndex++] = kT.m_iV2;
        }

        return new ModelTriangleMesh(akVertex,aiConnect);
    }

    /**
     * Find the triangles that are the level set for a linear function
     * defined on a single tetrahedron.
     *
     * @param iLevel the desired level value
     * @param (iX0,iY0,iZ0;iF0) a tetrahedron vertex and function value
     * @param (iX1,iY1,iZ1;iF1) a tetrahedron vertex and function value
     * @param (iX2,iY2,iZ2;iF2) a tetrahedron vertex and function value
     * @param (iX3,iY3,iZ3;iF3) a tetrahedron vertex and function value
     */
    private void processTetrahedron (float iLevel, int iX0, int iY0, int iZ0,
        int iF0, int iX1, int iY1, int iZ1, int iF1, int iX2, int iY2,
        int iZ2, int iF2, int iX3, int iY3, int iZ3, int iF3)
    {
        int iXN0, iYN0, iZN0, iD0;
        int iXN1, iYN1, iZN1, iD1;
        int iXN2, iYN2, iZN2, iD2;
        int iXN3, iYN3, iZN3, iD3;

        if ( iF0 != 0 )
        {
            // convert to case +***
            if ( iF0 < 0 )
            {
                iF0 = -iF0;
                iF1 = -iF1;
                iF2 = -iF2;
                iF3 = -iF3;
            }

            if ( iF1 > 0 )
            {
                if ( iF2 > 0 )
                {
                    if ( iF3 > 0 )
                    {
                        // ++++
                        return;
                    }
                    else if ( iF3 < 0 )
                    {
                        // +++-
                        iD0 = iF0 - iF3;
                        iXN0 = iF0*iX3 - iF3*iX0;
                        iYN0 = iF0*iY3 - iF3*iY0;
                        iZN0 = iF0*iZ3 - iF3*iZ0;
                        iD1 = iF1 - iF3;
                        iXN1 = iF1*iX3 - iF3*iX1;
                        iYN1 = iF1*iY3 - iF3*iY1;
                        iZN1 = iF1*iZ3 - iF3*iZ1;
                        iD2 = iF2 - iF3;
                        iXN2 = iF2*iX3 - iF3*iX2;
                        iYN2 = iF2*iY3 - iF3*iY2;
                        iZN2 = iF2*iZ3 - iF3*iZ2;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    }
                    else
                    {
                        // +++0
                        //addVertex(iX3,1,iY3,1,iZ3,1);
                    }
                }
                else if ( iF2 < 0 )
                {
                    iD0 = iF0 - iF2;
                    iXN0 = iF0*iX2 - iF2*iX0;
                    iYN0 = iF0*iY2 - iF2*iY0;
                    iZN0 = iF0*iZ2 - iF2*iZ0;
                    iD1 = iF1 - iF2;
                    iXN1 = iF1*iX2 - iF2*iX1;
                    iYN1 = iF1*iY2 - iF2*iY1;
                    iZN1 = iF1*iZ2 - iF2*iZ1;

                    if ( iF3 > 0 )
                    {
                        // ++-+
                        iD2 = iF3 - iF2;
                        iXN2 = iF3*iX2 - iF2*iX3;
                        iYN2 = iF3*iY2 - iF2*iY3;
                        iZN2 = iF3*iZ2 - iF2*iZ3;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    }
                    else if ( iF3 < 0 )
                    {
                        // ++--
                        iD2 = iF0 - iF3;
                        iXN2 = iF0*iX3 - iF3*iX0;
                        iYN2 = iF0*iY3 - iF3*iY0;
                        iZN2 = iF0*iZ3 - iF3*iZ0;
                        iD3 = iF1 - iF3;
                        iXN3 = iF1*iX3 - iF3*iX1;
                        iYN3 = iF1*iY3 - iF3*iY1;
                        iZN3 = iF1*iZ3 - iF3*iZ1;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                        addTriangle(
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN3,iD3,iYN3,iD3,iZN3,iD3,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    }
                    else
                    {
                        // ++-0
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX3,1,iY3,1,iZ3,1);
                    }
                }
                else
                {
                    if ( iF3 > 0 )
                    {
                        // ++0+
                        //addVertex(iX2,1,iY2,1,iZ2,1);
                    }
                    else if ( iF3 < 0 )
                    {
                        // ++0-
                        iD0 = iF0 - iF3;
                        iXN0 = iF0*iX3 - iF3*iX0;
                        iYN0 = iF0*iY3 - iF3*iY0;
                        iZN0 = iF0*iZ3 - iF3*iZ0;
                        iD1 = iF1 - iF3;
                        iXN1 = iF1*iX3 - iF3*iX1;
                        iYN1 = iF1*iY3 - iF3*iY1;
                        iZN1 = iF1*iZ3 - iF3*iZ1;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX2,1,iY2,1,iZ2,1);
                    }
                    else
                    {
                        // ++00
                        // addEdge(iX2,1,iY2,1,iZ2,1,iX3,1,iY3,1,iZ3,1);
                    }
                }
            }
            else if ( iF1 < 0 )
            {
                if ( iF2 > 0 )
                {
                    iD0 = iF0 - iF1;
                    iXN0 = iF0*iX1 - iF1*iX0;
                    iYN0 = iF0*iY1 - iF1*iY0;
                    iZN0 = iF0*iZ1 - iF1*iZ0;
                    iD1 = iF2 - iF1;
                    iXN1 = iF2*iX1 - iF1*iX2;
                    iYN1 = iF2*iY1 - iF1*iY2;
                    iZN1 = iF2*iZ1 - iF1*iZ2;

                    if ( iF3 > 0 )
                    {
                        // +-++
                        iD2 = iF3 - iF1;
                        iXN2 = iF3*iX1 - iF1*iX3;
                        iYN2 = iF3*iY1 - iF1*iY3;
                        iZN2 = iF3*iZ1 - iF1*iZ3;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    }
                    else if ( iF3 < 0 )
                    {
                        // +-+-
                        iD2 = iF0 - iF3;
                        iXN2 = iF0*iX3 - iF3*iX0;
                        iYN2 = iF0*iY3 - iF3*iY0;
                        iZN2 = iF0*iZ3 - iF3*iZ0;
                        iD3 = iF2 - iF3;
                        iXN3 = iF2*iX3 - iF3*iX2;
                        iYN3 = iF2*iY3 - iF3*iY2;
                        iZN3 = iF2*iZ3 - iF3*iZ2;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                        addTriangle(
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN3,iD3,iYN3,iD3,iZN3,iD3,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    }
                    else
                    {
                        // +-+0
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX3,1,iY3,1,iZ3,1);
                    }
                }
                else if ( iF2 < 0 )
                {
                    iD0 = iF1 - iF0;
                    iXN0 = iF1*iX0 - iF0*iX1;
                    iYN0 = iF1*iY0 - iF0*iY1;
                    iZN0 = iF1*iZ0 - iF0*iZ1;
                    iD1 = iF2 - iF0;
                    iXN1 = iF2*iX0 - iF0*iX2;
                    iYN1 = iF2*iY0 - iF0*iY2;
                    iZN1 = iF2*iZ0 - iF0*iZ2;

                    if ( iF3 > 0 )
                    {
                        // +--+
                        iD2 = iF1 - iF3;
                        iXN2 = iF1*iX3 - iF3*iX1;
                        iYN2 = iF1*iY3 - iF3*iY1;
                        iZN2 = iF1*iZ3 - iF3*iZ1;
                        iD3 = iF2 - iF3;
                        iXN3 = iF2*iX3 - iF3*iX2;
                        iYN3 = iF2*iY3 - iF3*iY2;
                        iZN3 = iF2*iZ3 - iF3*iZ2;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                        addTriangle(
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN3,iD3,iYN3,iD3,iZN3,iD3,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    }
                    else if ( iF3 < 0 )
                    {
                        // +---
                        iD2 = iF3 - iF0;
                        iXN2 = iF3*iX0 - iF0*iX3;
                        iYN2 = iF3*iY0 - iF0*iY3;
                        iZN2 = iF3*iZ0 - iF0*iZ3;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    }
                    else
                    {
                        // +--0
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX3,1,iY3,1,iZ3,1);
                    }
                }
                else
                {
                    iD0 = iF1 - iF0;
                    iXN0 = iF1*iX0 - iF0*iX1;
                    iYN0 = iF1*iY0 - iF0*iY1;
                    iZN0 = iF1*iZ0 - iF0*iZ1;

                    if ( iF3 > 0 )
                    {
                        // +-0+
                        iD1 = iF1 - iF3;
                        iXN1 = iF1*iX3 - iF3*iX1;
                        iYN1 = iF1*iY3 - iF3*iY1;
                        iZN1 = iF1*iZ3 - iF3*iZ1;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX2,1,iY2,1,iZ2,1);
                    }
                    else if ( iF3 < 0 )
                    {
                        // +-0-
                        iD1 = iF3 - iF0;
                        iXN1 = iF3*iX0 - iF0*iX3;
                        iYN1 = iF3*iY0 - iF0*iY3;
                        iZN1 = iF3*iZ0 - iF0*iZ3;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX2,1,iY2,1,iZ2,1);
                    }
                    else
                    {
                        // +-00
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iX2,1,iY2,1,iZ2,1,
                            iX3,1,iY3,1,iZ3,1);
                    }
                }
            }
            else
            {
                if ( iF2 > 0 )
                {
                    if ( iF3 > 0 )
                    {
                        // +0++
                        //addVertex(iX1,1,iY1,1,iZ1,1);
                    }
                    else if ( iF3 < 0 )
                    {
                        // +0+-
                        iD0 = iF0 - iF3;
                        iXN0 = iF0*iX3 - iF3*iX0;
                        iYN0 = iF0*iY3 - iF3*iY0;
                        iZN0 = iF0*iZ3 - iF3*iZ0;
                        iD1 = iF2 - iF3;
                        iXN1 = iF2*iX3 - iF3*iX2;
                        iYN1 = iF2*iY3 - iF3*iY2;
                        iZN1 = iF2*iZ3 - iF3*iZ2;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX1,1,iY1,1,iZ1,1);
                    }
                    else
                    {
                        // +0+0
                        // addEdge(iX1,1,iY1,1,iZ1,1,iX3,1,iY3,1,iZ3,1);
                    }
                }
                else if ( iF2 < 0 )
                {
                    iD0 = iF2 - iF0;
                    iXN0 = iF2*iX0 - iF0*iX2;
                    iYN0 = iF2*iY0 - iF0*iY2;
                    iZN0 = iF2*iZ0 - iF0*iZ2;

                    if ( iF3 > 0 )
                    {
                        // +0-+
                        iD1 = iF2 - iF3;
                        iXN1 = iF2*iX3 - iF3*iX2;
                        iYN1 = iF2*iY3 - iF3*iY2;
                        iZN1 = iF2*iZ3 - iF3*iZ2;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX1,1,iY1,1,iZ1,1);
                    }
                    else if ( iF3 < 0 )
                    {
                        // +0--
                        iD1 = iF0 - iF3;
                        iXN1 = iF0*iX3 - iF3*iX0;
                        iYN1 = iF0*iY3 - iF3*iY0;
                        iZN1 = iF0*iZ3 - iF3*iZ0;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iXN1,iD1,iYN1,iD1,iZN1,iD1,
                            iX1,1,iY1,1,iZ1,1);
                    }
                    else
                    {
                        // +0-0
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iX1,1,iY1,1,iZ1,1,
                            iX3,1,iY3,1,iZ3,1);
                    }
                }
                else
                {
                    if ( iF3 > 0 )
                    {
                        // +00+
                        // addEdge(iX1,1,iY1,1,iZ1,1,iX2,1,iY2,1,iZ2,1);
                    }
                    else if ( iF3 < 0 )
                    {
                        // +00-
                        iD0 = iF0 - iF3;
                        iXN0 = iF0*iX3 - iF3*iX0;
                        iYN0 = iF0*iY3 - iF3*iY0;
                        iZN0 = iF0*iZ3 - iF3*iZ0;
                        addTriangle(
                            iXN0,iD0,iYN0,iD0,iZN0,iD0,
                            iX1,1,iY1,1,iZ1,1,
                            iX2,1,iY2,1,iZ2,1);
                    }
                    else
                    {
                        // +000
                        addTriangle(
                            iX1,1,iY1,1,iZ1,1,
                            iX2,1,iY2,1,iZ2,1,
                            iX3,1,iY3,1,iZ3,1);
                    }
                }
            }
        }
        else if ( iF1 != 0 )
        {
            // convert to case 0+**
            if ( iF1 < 0 )
            {
                iF1 = -iF1;
                iF2 = -iF2;
                iF3 = -iF3;
            }

            if ( iF2 > 0 )
            {
                if ( iF3 > 0 )
                {
                    // 0+++
                    //addVertex(iX0,1,iY0,1,iZ0,1);
                }
                else if ( iF3 < 0 )
                {
                    // 0++-
                    iD0 = iF2 - iF3;
                    iXN0 = iF2*iX3 - iF3*iX2;
                    iYN0 = iF2*iY3 - iF3*iY2;
                    iZN0 = iF2*iZ3 - iF3*iZ2;
                    iD1 = iF1 - iF3;
                    iXN1 = iF1*iX3 - iF3*iX1;
                    iYN1 = iF1*iY3 - iF3*iY1;
                    iZN1 = iF1*iZ3 - iF3*iZ1;
                    addTriangle(
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX0,1,iY0,1,iZ0,1);
                }
                else
                {
                    // 0++0
                    // addEdge(iX0,1,iY0,1,iZ0,1,iX3,1,iY3,1,iZ3,1);
                }
            }
            else if ( iF2 < 0 )
            {
                iD0 = iF2 - iF1;
                iXN0 = iF2*iX1 - iF1*iX2;
                iYN0 = iF2*iY1 - iF1*iY2;
                iZN0 = iF2*iZ1 - iF1*iZ2;

                if ( iF3 > 0 )
                {
                    // 0+-+
                    iD1 = iF2 - iF3;
                    iXN1 = iF2*iX3 - iF3*iX2;
                    iYN1 = iF2*iY3 - iF3*iY2;
                    iZN1 = iF2*iZ3 - iF3*iZ2;
                    addTriangle(
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX0,1,iY0,1,iZ0,1);
                }
                else if ( iF3 < 0 )
                {
                    // 0+--
                    iD1 = iF1 - iF3;
                    iXN1 = iF1*iX3 - iF3*iX1;
                    iYN1 = iF1*iY3 - iF3*iY1;
                    iZN1 = iF1*iZ3 - iF3*iZ1;
                    addTriangle(
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX0,1,iY0,1,iZ0,1);
                }
                else
                {
                    // 0+-0
                    addTriangle(
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iX0,1,iY0,1,iZ0,1,
                        iX3,1,iY3,1,iZ3,1);
                }
            }
            else
            {
                if ( iF3 > 0 )
                {
                    // 0+0+
                    // addEdge(iX0,1,iY0,1,iZ0,1,iX2,1,iY2,1,iZ2,1);
                }
                else if ( iF3 < 0 )
                {
                    // 0+0-
                    iD0 = iF1 - iF3;
                    iXN0 = iF1*iX3 - iF3*iX1;
                    iYN0 = iF1*iY3 - iF3*iY1;
                    iZN0 = iF1*iZ3 - iF3*iZ1;
                    addTriangle(
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iX0,1,iY0,1,iZ0,1,
                        iX2,1,iY2,1,iZ2,1);
                }
                else
                {
                    // 0+00
                    addTriangle(
                        iX0,1,iY0,1,iZ0,1,
                        iX2,1,iY2,1,iZ2,1,
                        iX3,1,iY3,1,iZ3,1);
                }
            }
        }
        else if ( iF2 != 0 )
        {
            // convert to case 00+*
            if ( iF2 < 0 )
            {
                iF2 = -iF2;
                iF3 = -iF3;
            }

            if ( iF3 > 0 )
            {
                // 00++
                // addEdge(iX0,1,iY0,1,iZ0,1,iX1,1,iY1,1,iZ1,1);
            }
            else if ( iF3 < 0 )
            {
                // 00+-
                iD0 = iF2 - iF3;
                iXN0 = iF2*iX3 - iF3*iX2;
                iYN0 = iF2*iY3 - iF3*iY2;
                iZN0 = iF2*iZ3 - iF3*iZ2;
                addTriangle(
                    iXN0,iD0,iYN0,iD0,iZN0,iD0,
                    iX0,1,iY0,1,iZ0,1,
                    iX1,1,iY1,1,iZ1,1);
            }
            else
            {
                // 00+0
                addTriangle(
                    iX0,1,iY0,1,iZ0,1,
                    iX1,1,iY1,1,iZ1,1,
                    iX3,1,iY3,1,iZ3,1);
            }
        }
        else if ( iF3 != 0 )
        {
            // cases 000+ or 000-
            addTriangle(
                iX0,1,iY0,1,iZ0,1,
                iX1,1,iY1,1,iZ1,1,
                iX2,1,iY2,1,iZ2,1);
        }
        else
        {
            // case 0000
            addTriangle(
                iX0,1,iY0,1,iZ0,1,
                iX1,1,iY1,1,iZ1,1,
                iX2,1,iY2,1,iZ2,1);
            addTriangle(
                iX0,1,iY0,1,iZ0,1,
                iX1,1,iY1,1,iZ1,1,
                iX3,1,iY3,1,iZ3,1);
            addTriangle(
                iX0,1,iY0,1,iZ0,1,
                iX2,1,iY2,1,iZ2,1,
                iX3,1,iY3,1,iZ3,1);
            addTriangle(
                iX1,1,iY1,1,iZ1,1,
                iX2,1,iY2,1,iZ2,1,
                iX3,1,iY3,1,iZ3,1);
        }
    }

    /**
     * Add a level set vertex that was found in processTetrahedron.
     *
     * @param (iXN/iXD,iYN/iYD,iZN/iZD) the vertex in rational form
     * @return the index of that added vertex, used in addTriangle
     */
    private int addVertex (int iXN, int iXD, int iYN, int iYD, int iZN,
        int iZD)
    {
        Vertex kV = new Vertex(iXN,iXD,iYN,iYD,iZN,iZD);
        if ( m_kVMap.containsKey(kV) )
        {
            Integer kInt = (Integer) m_kVMap.get(kV);
            return kInt.intValue();
        }
        else
        {
            int i = m_iNextIndex++;
            m_kVMap.put(kV,new Integer(i));
            return i;
        }
    }

    /**
     * Add a level set triangle that was found in processTetrahedron.
     *
     * @param (iXN0/iXD0,iYN0/iYD0,iZN0/iZD0) a vertex in rational form
     * @param (iXN1/iXD1,iYN1/iYD1,iZN1/iZD1) a vertex in rational form
     * @param (iXN2/iXD2,iYN2/iYD2,iZN2/iZD2) a vertex in rational form
     */
    private void addTriangle (int iXN0, int iXD0, int iYN0, int iYD0,
        int iZN0, int iZD0, int iXN1, int iXD1, int iYN1, int iYD1, int iZN1,
        int iZD1, int iXN2, int iXD2, int iYN2, int iYD2, int iZN2, int iZD2)
    {
        int iV0 = addVertex(iXN0,iXD0,iYN0,iYD0,iZN0,iZD0);
        int iV1 = addVertex(iXN1,iXD1,iYN1,iYD1,iZN1,iZD1);
        int iV2 = addVertex(iXN2,iXD2,iYN2,iYD2,iZN2,iZD2);

        // compute triangle normal assuming counterclockwise ordering
        m_kV0.x = iXN0/(float)iXD0;
        m_kV0.y = iYN0/(float)iYD0;
        m_kV0.z = iZN0/(float)iZD0;

        m_kV1.x = iXN1/(float)iXD1;
        m_kV1.y = iYN1/(float)iYD1;
        m_kV1.z = iZN1/(float)iZD1;

        m_kV2.x = iXN2/(float)iXD2;
        m_kV2.y = iYN2/(float)iYD2;
        m_kV2.z = iZN2/(float)iZD2;

        m_kE0.sub(m_kV1,m_kV0);
        m_kE1.sub(m_kV2,m_kV0);
        m_kN.cross(m_kE0,m_kE1);

        // choose triangle orientation based on gradient direction
        m_kCentroid.add(m_kV0,m_kV1);
        m_kCentroid.add(m_kV2);
        m_kCentroid.scale(1.0f/3.0f);
        Vector3f kGradient = getGradient(m_kCentroid);
        if ( kGradient.dot(m_kN) <= 0.0f )
            m_kTSet.add(new Triangle(iV0,iV1,iV2));
        else
            m_kTSet.add(new Triangle(iV0,iV2,iV1));
    }

    /**
     * A rational representation of a level set vertex.  The rational form
     * allows for exact look-ups in a hash map of vertices.  The class extends
     * Object for hashing support.
     */
    private class Vertex extends Object
    {
        // Vertices are stored as rational numbers (xn/xd,yn/yd,zn/zd).  This
        // supports removing redundant vertices that occur because of
        // extraction in adjacent voxels.
        public int m_iXNumer, m_iXDenom;
        public int m_iYNumer, m_iYDenom;
        public int m_iZNumer, m_iZDenom;

        /**
         * Create a vertex (invalid one since the denominators are zero).
         */
        public Vertex ()
        {
            m_iXNumer = 0;
            m_iXDenom = 0;
            m_iYNumer = 0;
            m_iYDenom = 0;
            m_iZNumer = 0;
            m_iZDenom = 0;
        }

        /**
         * Create a vertex.
         *
         * @param (iXNumer/iXDenom,iYNumer/iYDenom,iZNumer/iZDenom) a
         *   vertex in rational form
         */
        public Vertex (int iXNumer, int iXDenom, int iYNumer, int iYDenom,
            int iZNumer, int iZDenom)
        {
            if ( iXDenom > 0 )
            {
                m_iXNumer = iXNumer;
                m_iXDenom = iXDenom;
            }
            else
            {
                m_iXNumer = -iXNumer;
                m_iXDenom = -iXDenom;
            }

            if ( iYDenom > 0 )
            {
                m_iYNumer = iYNumer;
                m_iYDenom = iYDenom;
            }
            else
            {
                m_iYNumer = -iYNumer;
                m_iYDenom = -iYDenom;
            }

            if ( iZDenom > 0 )
            {
                m_iZNumer = iZNumer;
                m_iZDenom = iZDenom;
            }
            else
            {
                m_iZNumer = -iZNumer;
                m_iZDenom = -iZDenom;
            }
        }

        /**
         * Support for hashing into a map of vertices.
         *
         * @return the hash key for the vertex
         */
        public int hashCode ()
        {
            int iXPair = (m_iXNumer << 16) | m_iXDenom;
            int iYPair = (m_iYNumer << 16) | m_iYDenom;
            int iZPair = (m_iZNumer << 16) | m_iZDenom;
            return iXPair ^ iYPair ^ iZPair;
        }

        /**
         * Support for hashing into a map of vertics.
         *
         * @param kObject a vertex for comparison to the current one
         * @return true iff the vertices are identical
         */
        public boolean equals (Object kObject)
        {
            Vertex kV = (Vertex) kObject;
            return m_iXNumer*kV.m_iXDenom == m_iXDenom*kV.m_iXNumer
                && m_iYNumer*kV.m_iYDenom == m_iYDenom*kV.m_iYNumer
                && m_iZNumer*kV.m_iZDenom == m_iZDenom*kV.m_iZNumer;
        }
    }

    /**
     * A rational representation of a level set triangle.  The triangle just
     * stores vertex indices and is unconcerned about the actual vertex
     * locations.  The class extends Object for hashing support.
     */
    private class Triangle extends Object
    {
        // indices into the vertex array
        public int m_iV0, m_iV1, m_iV2;

        /**
         * Create a triangle (invalid one since the indices are negative).
         */
        public Triangle ()
        {
            m_iV0 = -1;
            m_iV1 = -1;
            m_iV2 = -1;
        }

        /**
         * Create a triangle.  The triangle <V0,V1,V2> is considered to be
         * different than <V0,V2,V1>.  To minimize computation time in
         * comparisons during hashing, the triangle is stored so that the
         * minimum index occurs first.
         *
         * @param iV0 a triangle vertex
         * @param iV1 a triangle vertex
         * @param iV2 a triangle vertex
         */
        public Triangle (int iV0, int iV1, int iV2)
        {
            if ( iV0 < iV1 )
            {
                if ( iV0 < iV2 )
                {
                    // v0 is minimum
                    m_iV0 = iV0;
                    m_iV1 = iV1;
                    m_iV2 = iV2;
                }
                else
                {
                    // v2 is minimum
                    m_iV0 = iV2;
                    m_iV1 = iV0;
                    m_iV2 = iV1;
                }
            }
            else
            {
                if ( iV1 < iV2 )
                {
                    // v1 is minimum
                    m_iV0 = iV1;
                    m_iV1 = iV2;
                    m_iV2 = iV0;
                }
                else
                {
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
         * @return the hash key for the triangle
         */
        public int hashCode ()
        {
            int iCmp;

            if ( m_iV1 < m_iV2 )
                iCmp = (m_iV2 << 8)^((m_iV0 << 16) | m_iV1);
            else
                iCmp = (m_iV1 << 8)^((m_iV0 << 16) | m_iV2);

            return iCmp;
        }

        /**
         * Support for hashing into a map of triangles.
         *
         * @param kObject a triangle for comparison to the current one
         * @return true iff the triangles are identical
         */
        public boolean equals (Object kObject)
        {
            Triangle kT = (Triangle) kObject;
            return (m_iV0 == kT.m_iV0) &&
                  ((m_iV1 == kT.m_iV1 && m_iV2 == kT.m_iV2) ||
                   (m_iV1 == kT.m_iV2 && m_iV2 == kT.m_iV1));
        }
    }

    // bounds on each dimension of 3D data set and data itself
    private int m_iXBound, m_iYBound, m_iZBound;
    private int m_iXYProduct, m_iXYZProduct;
    private int[] m_aiData;
    private float m_fXDelta, m_fYDelta, m_fZDelta;
    private int[] direction;
    private float[] startLocation;
    private TransMatrix dicomMatrix = null;

    private float m_fXTrueBound, m_fYTrueBound, m_fZTrueBound;

    // temporary storage for use in extraction
    private HashMap m_kVMap;  // map of <Vertex,Integer>
    private HashSet m_kTSet;  // set of <Triangle>

    // for unique indexing of vertices
    private int m_iNextIndex;

    // temporary variables to eliminate 'new' calls for Point3f, Vector3f
    private Point3f m_kV0, m_kV1, m_kV2, m_kCentroid;
    private Vector3f m_kE0, m_kE1, m_kN, m_kGradient;
}
