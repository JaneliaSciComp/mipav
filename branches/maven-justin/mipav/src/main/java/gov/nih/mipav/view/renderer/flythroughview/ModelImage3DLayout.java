package gov.nih.mipav.view.renderer.flythroughview;

import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.Point3D;


/**
 * Used to store the information and layout which describes a volume of data.
 */
public class ModelImage3DLayout {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag set to reverse order of "real" X coordinates assigned to samples. */
    public final boolean m_bReverseX;

    /** Flag set to reverse order of "real" Y coordinates assigned to samples. */
    public final boolean m_bReverseY;

    /** Flag set to reverse order of "real" Z coordinates assigned to samples. */
    public final boolean m_bReverseZ;

    /** "Real" coordinate of sample at the 0 index along the X axis. */
    public final float m_fOffsetX;

    /** "Real" coordinate of sample at the 0 index along the X axis. */
    public final float m_fOffsetY;

    /** "Real" coordinate of sample at the 0 index along the X axis. */
    public final float m_fOffsetZ;

    /** Spacing between each sample along X axis. */
    public final float m_fSpacingX;

    /** Spacing between each sample along Y axis. */
    public final float m_fSpacingY;

    /** Spacing between each sample along Z axis. */
    public final float m_fSpacingZ;

    /** Number of samples in volume along X axis. */
    public final int m_iDimX;

    /** Number of samples in volume along Y axis. */
    public final int m_iDimY;

    /** Number of samples in volume along Z axis. */
    public final int m_iDimZ;

    /**
     * Array of exact Euclidean distances (in sample coordinates) to each of the 26-connected neighbors corresponding to
     * the relative indexes in m_aiOffsetNeighbors27 array.
     */
    private final float[] m_afDistanceNeighbors27;

    /**
     * Array of exact Euclidean distances to each of the 26-connected neighbors scaled by the size of each sample along
     * each axis and corresponding to the relative indexes in m_aiOffsetNeighbors27 array.
     */
    private final float[] m_afRealDistanceNeighbors27;

    /** Array of 27 index offsets used by the getOffsetXYZNeighbors27 method. */
    private final int[] m_aiOffsetNeighbors27;

    /** Array of 8 index offsets used by the getIndexNeighborsVoxel method. */
    private final int[] m_aiOffsetNeighborsVoxel;

    /** Array of 27 index offsets used by the getIndexNeighbors27 method. */
    private final Point3D[] m_akOffsetXYZNeighbors27;

    /** Index offsets used by the getIndexNeighborsGradient method. */
    private final GradientNeighbors m_kOffsetNeighborsGradient;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a volume with the specified dimensions.
     *
     * @param  iDimX      number of samples along the X axis
     * @param  iDimY      number of samples along the Y axis
     * @param  iDimZ      number of samples along the Z axis
     * @param  fSpacingX  spacing between samples along the X axis. Value can be negative to indicated a reverse
     *                    direction for real coords.
     * @param  fSpacingY  spacing between samples along the Y axis Value can be negative to indicated a reverse
     *                    direction for real coords.
     * @param  fSpacingZ  spacing between samples along the Z axis Value can be negative to indicated a reverse
     *                    direction for real coords.
     * @param  fOffsetX   X coordinate of samples at X sample index 0
     * @param  fOffsetY   Y coordinate of samples at Y sample index 0
     * @param  fOffsetZ   Z coordinate of samples at Z sample index 0
     */
    public ModelImage3DLayout(int iDimX, int iDimY, int iDimZ, float fSpacingX, float fSpacingY, float fSpacingZ,
                              float fOffsetX, float fOffsetY, float fOffsetZ) {

        // Save the specified dimensions, spacing, and offsets.
        m_iDimX = iDimX;
        m_iDimY = iDimY;
        m_iDimZ = iDimZ;
        m_bReverseX = fSpacingX < 0.0f;
        m_bReverseY = fSpacingY < 0.0f;
        m_bReverseZ = fSpacingZ < 0.0f;
        m_fSpacingX = m_bReverseX ? -fSpacingX : fSpacingX;
        m_fSpacingY = m_bReverseY ? -fSpacingY : fSpacingY;
        m_fSpacingZ = m_bReverseZ ? -fSpacingZ : fSpacingZ;
        m_fOffsetX = fOffsetX;
        m_fOffsetY = fOffsetY;
        m_fOffsetZ = fOffsetZ;

        // Setup the offsets into the linear array for locating the
        // neighboring samples to use when computing gradients.
        // These offsets are for the computing a central difference.
        // The m_fDelta* members are set to the size of each
        // sample along its respective axis.
        m_kOffsetNeighborsGradient = new GradientNeighbors();
        m_kOffsetNeighborsGradient.m_iX0 = getIndex(-1, 0, 0);
        m_kOffsetNeighborsGradient.m_iX1 = getIndex(+1, 0, 0);
        m_kOffsetNeighborsGradient.m_iY0 = getIndex(0, -1, 0);
        m_kOffsetNeighborsGradient.m_iY1 = getIndex(0, +1, 0);
        m_kOffsetNeighborsGradient.m_iZ0 = getIndex(0, 0, -1);
        m_kOffsetNeighborsGradient.m_iZ1 = getIndex(0, 0, +1);

        // Setup the array of offsets into the linear array for
        // locating a specified sample and its other connected
        // neighbors that comprise a voxel.
        m_aiOffsetNeighborsVoxel = new int[8];

        int iIndexNeighborsVoxel = 0;

        for (int iZ = 0; iZ <= 1; ++iZ) {

            for (int iY = 0; iY <= 1; ++iY) {

                for (int iX = 0; iX <= 1; ++iX) {

                    // Note that the getIndex method will handle negative
                    // indexes although we are using it to compute offsets
                    // in this case where normally the getIndex method
                    // would never be called with negative indexes.
                    m_aiOffsetNeighborsVoxel[iIndexNeighborsVoxel++] = getIndex(iX, iY, iZ);
                }
            }
        }

        // Setup the array of offsets into the linear array for
        // locating a specified sample and its 26-connected neighbors.
        // At the same time, setup the relative distances from
        // the center sample to each of its 26-connected neighbors.
        // The relative distances are based on the size of the sample
        // along each dimension.
        m_aiOffsetNeighbors27 = new int[27];
        m_akOffsetXYZNeighbors27 = new Point3D[27];
        m_aiOffsetNeighbors27[0] = 0;
        m_akOffsetXYZNeighbors27[0] = new Point3D(0, 0, 0);
        m_afDistanceNeighbors27 = new float[27];
        m_afRealDistanceNeighbors27 = new float[27];
        m_afDistanceNeighbors27[0] = 0.0f;
        m_afRealDistanceNeighbors27[0] = 0.0f;

        int iIndexNeighbors27 = 1;

        for (int iZ = -1; iZ <= 1; ++iZ) {

            for (int iY = -1; iY <= 1; ++iY) {

                for (int iX = -1; iX <= 1; ++iX) {

                    // Skip the case where (iX,iY,iZ)=(0,0,0) since
                    // we already handled it as a special case.
                    if ((0 == iX) && (0 == iY) && (0 == iZ)) {
                        continue;
                    }

                    // Note that the getIndex method will handle negative
                    // indexes although we are using it to compute offsets
                    // in this case where normally the getIndex method
                    // would never be called with negative indexes.
                    m_akOffsetXYZNeighbors27[iIndexNeighbors27] = new Point3D(iX, iY, iZ);
                    m_aiOffsetNeighbors27[iIndexNeighbors27] = getIndex(iX, iY, iZ);

                    // Compute the Euclidean distance (in sample coordinates)
                    // to the next sample
                    float fDx = iX;
                    float fDy = iY;
                    float fDz = iZ;
                    m_afDistanceNeighbors27[iIndexNeighbors27] = (float) Math.sqrt((fDx * fDx) + (fDy * fDy) +
                                                                                   (fDz * fDz));

                    // Compute the real Euclidean distance to the next sample.
                    fDx *= fSpacingX;
                    fDy *= fSpacingY;
                    fDz *= fSpacingZ;
                    m_afRealDistanceNeighbors27[iIndexNeighbors27] = (float) Math.sqrt((fDx * fDx) + (fDy * fDy) +
                                                                                       (fDz * fDz));

                    ++iIndexNeighbors27;
                }
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Fill an array of distances to the 26-connected neighbors for any given sample where the distances are the
     * Euclidean distances in sample coordinates.
     *
     * @param  afDistances  must be an array with at least 27 entries where these entries will be filled in with the
     *                      Euclidean distances for the 26-connected neighbors of any given sample in the volume. The
     *                      ordering of the distances corresponds to the ordering of the relative linear array indexes
     *                      returned by the getOffsetNeighbors27 method. Note that the [0] entry is for the center
     *                      sample (always a distance of zero), and the remaining 26 entries are for the neighbor
     *                      samples distances.
     */
    public void getDistanceNeighbors27(float[] afDistances) {
        System.arraycopy(m_afDistanceNeighbors27, 0, afDistances, 0, 27);
    }

    /**
     * Compute the index in a linear data array dimensioned to store all the samples of this volume. The index is
     * determined by the specified 3D coordinate indexes. No checking is made to ensure that the coordinate indexes are
     * in their respective ranges.
     *
     * @param   iX  sample X coordinate index
     * @param   iY  sample Y coordinate index
     * @param   iZ  sample Z coordinate index
     *
     * @return  1D index into linear array
     */
    public int getIndex(int iX, int iY, int iZ) {
        return iX + (m_iDimX * (iY + (m_iDimY * iZ)));
    }

    /**
     * Compute the index in a linear data array dimensioned to store all the samples of this volume. The index is
     * determined by the specified 3D coordinate indexes. Each coordinate index is checked to ensure that it is within
     * its respective coordinate ranges.
     *
     * @param   iX  sample X coordinate index
     * @param   iY  sample Y coordinate index
     * @param   iZ  sample Z coordinate index
     *
     * @return  1D index into linear array or -1 if any of the indexes are outside their respective coordinate ranges
     */
    public int getIndexChecked(int iX, int iY, int iZ) {

        if ((iX < 0) || (iY < 0) || (iZ < 0) || (iX >= m_iDimX) || (iY >= m_iDimY) || (iZ >= m_iDimZ)) {
            return -1;
        }

        return iX + (m_iDimX * (iY + (m_iDimY * iZ)));
    }

    /**
     * Get the amount to increment the linear array index for each increment in the x coordinate index.
     *
     * @return  int Positive linear array index increment.
     */
    public int getIndexIncX() {
        return 1;
    }

    /**
     * Get the amount to increment the linear array index for each increment in the y coordinate index.
     *
     * @return  int Positive linear array index increment.
     */
    public int getIndexIncY() {
        return m_iDimX;
    }

    /**
     * Get the amount to increment the linear array index for each increment in the z coordinate index.
     *
     * @return  int Positive linear array index increment.
     */
    public int getIndexIncZ() {
        return m_iDimX * m_iDimY;
    }

    /**
     * Fill an array of indexes for a linear array of volume values. These indexes are for the specified sample and the
     * indexes of the immediate 26-connected neighbors.
     *
     * <p>Unexpected results occur, e.g. invalid sample indexes, if the center sample specified by the indexes is for
     * one on the border of the volume.</p>
     *
     * @param  iIndexCenter  linear volume data array index center sample
     * @param  aiIndexes     must be an array with at least 27 entries where these entries will be filled in with the
     *                       indexes for linear volume data array for the specified sample and its 26-connected
     *                       neighbors. Note that the [0] entry is for the center sample, but there is no specified
     *                       ordering for the 26 neighbor samples.
     */
    public void getIndexNeighbors27(int iIndexCenter, int[] aiIndexes) {

        // Add the already computed offsets to this index.
        for (int iIndex = 0; iIndex < m_aiOffsetNeighbors27.length; ++iIndex) {
            aiIndexes[iIndex] = iIndexCenter + m_aiOffsetNeighbors27[iIndex];
        }
    }

    /**
     * Fill an array of indexes for a linear array of volume values. These indexes are for the specified sample and the
     * indexes of the immediate 26-connected neighbors.
     *
     * <p>Unexpected results occur, e.g. invalid sample indexes, if the center sample specified by the indexes is for
     * one on the border of the volume.</p>
     *
     * @param  iX         sample X coordinate index for center sample
     * @param  iY         sample Y coordinate index for center sample
     * @param  iZ         sample Z coordinate index for center sample
     * @param  aiIndexes  must be an array with at least 27 entries where these entries will be filled in with the
     *                    indexes for linear volume data array for the specified sample and its 26-connected neighbors.
     *                    Note that the [0] entry is for the center sample, but there is no specified ordering for the
     *                    26 neighbor samples.
     */
    public void getIndexNeighbors27(int iX, int iY, int iZ, int[] aiIndexes) {
        getIndexNeighbors27(getIndex(iX, iY, iZ), aiIndexes);
    }

    /**
     * Retrieve the indexes for the 6-connected neighbors of the specified sample which are used to compute the
     * gradient. If the sample is along any border of the volume, then the indexes for a forward/reverse finite
     * difference are returned which would involve the sample itself but only for the axis where the sample is along its
     * corresponding border. Otherwise, the samples are returned for computing finite central difference.
     *
     * <p>Unexpected results occur, e.g. invalid sample indexes, if the center sample specified by the indexes is for
     * one on the border of the volume.</p>
     *
     * @param  iX          sample X coordinate index for center sample
     * @param  iY          sample Y coordinate index for center sample
     * @param  iZ          sample Z coordinate index for center sample
     * @param  kNeighbors  contains the indexes for the 6-connected neighbors used to compute the gradient
     */
    public void getIndexNeighborsGradient(int iX, int iY, int iZ, GradientNeighbors kNeighbors) {

        // Get the index for the "center" sample.
        int iIndexCenter = getIndex(iX, iY, iZ);

        // Recall that these offsets are for computing a central difference.
        kNeighbors.m_iX0 = iIndexCenter + m_kOffsetNeighborsGradient.m_iX0;
        kNeighbors.m_iX1 = iIndexCenter + m_kOffsetNeighborsGradient.m_iX1;
        kNeighbors.m_iY0 = iIndexCenter + m_kOffsetNeighborsGradient.m_iY0;
        kNeighbors.m_iY1 = iIndexCenter + m_kOffsetNeighborsGradient.m_iY1;
        kNeighbors.m_iZ0 = iIndexCenter + m_kOffsetNeighborsGradient.m_iZ0;
        kNeighbors.m_iZ1 = iIndexCenter + m_kOffsetNeighborsGradient.m_iZ1;

        // Delta's for central differences are 2.
        kNeighbors.m_fDeltaX = 2.0f;
        kNeighbors.m_fDeltaY = 2.0f;
        kNeighbors.m_fDeltaZ = 2.0f;

        // If the requested sample is on the border of the volume,
        // the compute a corresponding forward or reverse difference
        // involving the sample itself.
        if (0 >= iX) {
            kNeighbors.m_iX0 = iIndexCenter;
            kNeighbors.m_fDeltaX = 1.0f;
        } else if (m_iDimX <= (iX + 1)) {
            kNeighbors.m_iX1 = iIndexCenter;
            kNeighbors.m_fDeltaX = 1.0f;
        }

        if (0 >= iY) {
            kNeighbors.m_iY0 = iIndexCenter;
            kNeighbors.m_fDeltaY = 1.0f;
        } else if (m_iDimY <= (iY + 1)) {
            kNeighbors.m_iY1 = iIndexCenter;
            kNeighbors.m_fDeltaY = 1.0f;
        }

        if (0 >= iZ) {
            kNeighbors.m_iZ0 = iIndexCenter;
            kNeighbors.m_fDeltaZ = 1.0f;
        } else if (m_iDimZ <= (iZ + 1)) {
            kNeighbors.m_iZ1 = iIndexCenter;
            kNeighbors.m_fDeltaZ = 1.0f;
        }
    }

    /**
     * Fill an array of indexes for a linear array of volume values. These indexes are for the specified sample and the
     * indexes of the immediate neighbor samples that comprise a voxel. Of the 8 neighboring samples returned for a
     * voxel, the specified sample will have the smallest of the coordinate indexes, i.e., the "minimum" corner sample.
     *
     * <p>Unexpected results occur, e.g. invalid sample indexes, if the specified sample is along any of the maximum
     * borders of the volume (given by an sample coordinate index being the maximum for the corresponding axis).</p>
     *
     * @param  iX         sample X coordinate index for "minimum" corner sample
     * @param  iY         sample Y coordinate index for "minimum" corner sample
     * @param  iZ         sample Z coordinate index for "minimum" corner sample
     * @param  aiIndexes  must be an array with at least 8 entries where these entries will be filled in with the
     *                    indexes for linear volume data array for the specified sample and its voxel neighbors. Note
     *                    that the [0] entry is for the specified sample, but there is no specified ordering for the
     *                    other 7 neighbor samples.
     */
    public void getIndexNeighborsVoxel(int iX, int iY, int iZ, int[] aiIndexes) {

        // Get the index for the "center" sample.
        int iIndexCenter = getIndex(iX, iY, iZ);

        // Add the already computed offsets to this index.
        for (int iIndex = 0; iIndex < m_aiOffsetNeighborsVoxel.length; ++iIndex) {
            aiIndexes[iIndex] = iIndexCenter + m_aiOffsetNeighborsVoxel[iIndex];
        }
    }

    /**
     * Get X sample coordinate from a linear array index for volume data.
     *
     * @param   iIndex  linear array index
     *
     * @return  X sample coordinate
     */
    public int getIndexX(int iIndex) {
        return (iIndex % (m_iDimX * m_iDimY)) % m_iDimX;
    }

    /**
     * Get Y sample coordinate from a linear array index for volume data.
     *
     * @param   iIndex  linear array index
     *
     * @return  Y sample coordinate
     */
    public int getIndexY(int iIndex) {
        return (iIndex % (m_iDimX * m_iDimY)) / m_iDimX;
    }

    /**
     * Get Z sample coordinate from a linear array index for volume data.
     *
     * @param   iIndex  linear array index
     *
     * @return  Z sample coordinate
     */
    public int getIndexZ(int iIndex) {
        return iIndex / (m_iDimX * m_iDimY);
    }

    /**
     * Query the total number of samples for a volume with these parameters.
     *
     * @return  number of voxels
     */
    public int getNumSamples() {
        return m_iDimX * m_iDimY * m_iDimZ;
    }

    /**
     * Fill in the array of offsets to the 26-connected neighbors for any given sample in the volume.
     *
     * @param  akOffsetXYZ  must be an array with at least 27 enties where these entries will be filled in with the XYZ
     *                      index offsets for the 26-connected neighbors of any given sample in the volume. The ordering
     *                      of the offsets corresponds to the ordering of the relative linear array indexes returned by
     *                      the getIndexNeighbors27 method. Note that the [0] entry is for the center sample (always XYZ
     *                      offsets of zero), and the remaining 26 entries are for the neighbor samples.
     */
    public void getOffsetXYZNeighbors27(Point3D[] akOffsetXYZ) {
        System.arraycopy(m_akOffsetXYZNeighbors27, 0, akOffsetXYZ, 0, 27);
    }

    /**
     * Given the coordinates of two points in sample space of the volume, compute the real distance betwen them by
     * taking into consideration the spacing between samples in the volume along each axis.
     *
     * @param   iX1  X-axis sample coordinate of first point
     * @param   iY1  Y-axis sample coordinate of first point
     * @param   iZ1  Z-axis sample coordinate of first point
     * @param   iX2  X-axis sample coordinate of second point
     * @param   iY2  Y-axis sample coordinate of second point
     * @param   iZ2  Z-axis sample coordinate fo second point
     *
     * @return  computed "real" distance between two samples in volume
     */
    public float getRealDistance(int iX1, int iY1, int iZ1, int iX2, int iY2, int iZ2) {
        float fDeltaX = (iX2 - iX1);
        float fDeltaY = (iY2 - iY1);
        float fDeltaZ = (iZ2 - iZ1);

        double fVx = (fDeltaX * m_fSpacingX);
        double fVy = (fDeltaY * m_fSpacingY);
        double fVz = (fDeltaZ * m_fSpacingZ);

        return (float) Math.sqrt((fVx * fVx) + (fVy * fVy) + (fVz * fVz));
    }

    /**
     * Fill an array of distances to the 26-connected neighbors for any given sample where the distances are the real
     * Euclidean distances scaled by the size of the sample along each axis.
     *
     * @param  afDistances  must be an array with at least 27 entries where these entries will be filled in with the
     *                      Euclidean distances for the 26-connected neighbors of any given sample in the volume. The
     *                      ordering of the distances corresponds to the ordering of the relative linear array indexes
     *                      returned by the getOffsetNeighbors27 method. Note that the [0] entry is for the center
     *                      sample (always a distance of zero), and the remaining 26 entries are for the neighbor
     *                      samples distances.
     */
    public void getRealDistanceNeighbors27(float[] afDistances) {
        System.arraycopy(m_afRealDistanceNeighbors27, 0, afDistances, 0, 27);
    }

    /**
     * Compute the "real" coordinates of the sample in the volume at the specified sample coordinate indexes.
     *
     * @param  iX      sample X coordinate index
     * @param  iY      sample Y coordinate index
     * @param  iZ      sample Z coordinate index
     * @param  kPoint  Point3f into which the computed coordinates are stored
     */
    public void getRealPoint(int iX, int iY, int iZ, Vector3f kPoint) {

        // Do we need to reverse the sense?
        if (m_bReverseX) {
            iX = m_iDimX - 1 - iX;
        }

        if (m_bReverseY) {
            iY = m_iDimY - 1 - iY;
        }

        if (m_bReverseZ) {
            iZ = m_iDimZ - 1 - iZ;
        }

        kPoint.X = m_fOffsetX + (m_fSpacingX * iX);
        kPoint.Y = m_fOffsetY + (m_fSpacingY * iY);
        kPoint.Z = m_fOffsetZ + (m_fSpacingZ * iZ);
    }

    /**
     * Convert the input "real" coordinates into sample coordinates.
     *
     * @param   kRealPoint  Point3f These are the "real" coordinates.
     *
     * @return  Point3f This is a new instance containing the interpolated sample coordinates converted from the input
     *          real coordinates.
     */
    public Vector3f getSamplePoint(float x, float y, float z) {
        Vector3f kSamplePoint = new Vector3f();

        kSamplePoint.X = (x - m_fOffsetX) / m_fSpacingX;
        kSamplePoint.Y = (y - m_fOffsetY) / m_fSpacingY;
        kSamplePoint.Z = (z - m_fOffsetZ) / m_fSpacingZ;

        kSamplePoint.X = kSamplePoint.X / (m_iDimX - 1);
        kSamplePoint.Y = kSamplePoint.Y / (m_iDimY - 1);
        kSamplePoint.Z = kSamplePoint.Z / (m_iDimZ - 1);

        // Do we need to reverse the sense?
        if (m_bReverseX) {
            kSamplePoint.X = 1 - kSamplePoint.X;
        }

        if (m_bReverseY) {
            kSamplePoint.Y = 1 - kSamplePoint.Y;
        }

        if (m_bReverseZ) {
            kSamplePoint.Z = 1 - kSamplePoint.Z;
        }

        return kSamplePoint;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Stores the indexes into a linear array for the samples to use in computing finite difference based gradients as
     * follows:
     *
     * <p>grad.x = (array[m_iX1] - array[m_iX0]) / m_fDeltaX; grad.y = (array[m_iY1] - array[m_iY0]) / m_fDeltaY; grad.z
     * = (array[m_iZ1] - array[m_iZ0]) / m_fDeltaZ;</p>
     */
    public static class GradientNeighbors {

        /** Scale factors to use for normalizing the finite differences by axis. */
        public float m_fDeltaX;

        /** DOCUMENT ME! */
        public float m_fDeltaY;

        /** DOCUMENT ME! */
        public float m_fDeltaZ;

        /** Indexes into linear array for X-axis finite difference computed by array[m_iX1] - array[m_iX0];. */
        public int m_iX0;

        /** DOCUMENT ME! */
        public int m_iX1;

        /** Indexes into linear array for Y-axis finite difference computed by array[m_iY1] - array[m_iY0];. */
        public int m_iY0;

        /** DOCUMENT ME! */
        public int m_iY1;

        /** Indexes into linear array for Z-axis finite difference computed by array[m_iZ1] - array[m_iZ0];. */
        public int m_iZ0;

        /** DOCUMENT ME! */
        public int m_iZ1;
    }
}
