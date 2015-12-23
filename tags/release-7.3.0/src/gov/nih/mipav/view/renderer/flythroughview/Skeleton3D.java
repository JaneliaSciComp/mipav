package gov.nih.mipav.view.renderer.flythroughview;


import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.util.*;



/**
 * Implementation of the 3D skeletonization of a binary volume based on the paper "Penalized-distance volumetric
 * skeleton algorithm"; I. Bitter, A. Kaufman, M. Sato; IEEE Transactions on Visualization and Computer Graphics, Vol.
 * 7, No. 3, pp. 195-206, July-September 2001.
 */
public class Skeleton3D {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Set this flag to allow the output of volume file which result as part of the computation. */
    private static final boolean DEBUG = false;

    /**
     * These are bit masks used in the result volume which identifies how samples were classified at different steps
     * during the processing. RESULT_MASK_INSIDE - subset of all samples; those that are inside
     * RESULT_MASK_NONUNIFORM_GRADIENT - subset of samples marked with RESULT_MASK_INSIDE that have nonuniform boundary
     * distance gradients RESULT_MASK_FLAGGED - subset of samples marked with RESULT_MASK_INSIDE and including all
     * samples marked with RESULT_MASK_NONUNIFORM_GRADIENT are those samples that connected those samples marked with
     * RESULT_MASK_NONUNIFORM_GRADIENT RESULT_MASK_CONNECTED - subset of samples marked with RESULT_MASK_FLAGGED that
     * are largest 26-connected collection of samples marked with RESULT_MASK_FLAGGED RESULT_MASK_SKELETON - subset of
     * samples marked with RESULT_MASK_CONNECTED that are the samples which make up the the skeleton of the binary
     * volume RESULT_MASK_SKELETON_NEIGHBOR - the subset of samples marked RESULT_MARK_CONNECTED that within a certain
     * radius of samples marked as RESULT_MARK_SKELETON.
     */
    private static final short RESULT_MASK_INSIDE = 0x0001;

    /** DOCUMENT ME! */
    private static final short RESULT_MASK_NONUNIFORM_GRADIENT = 0x0002;

    /** DOCUMENT ME! */
    private static final short RESULT_MASK_FLAGGED = 0x0004;

    /** DOCUMENT ME! */
    private static final short RESULT_MASK_CONNECTED = 0x0008;

    /** DOCUMENT ME! */
    private static final short RESULT_MASK_SKELETON_NEIGHBOR = 0x0010;

    /** DOCUMENT ME! */
    private static final short RESULT_MASK_SKELETON = 0x0020;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Array of Euclidean distances to the nearest boundary for each inside voluem sample. Distances are in sample
     * coordinates. Also compute the maximum of these values.
     */
    private float[] m_afBoundarySampleDistMin;

    /**
     * Volume of bitmasked values associated with each sample. The bitmask values are used to classify each sample
     * during the various stages of the skeletonization process.
     */
    private short[] m_asVolumeData;

    /** DOCUMENT ME! */
    private float m_fMaxBoundarySampleDistMin;

    /** This the linear array index for the sample from which all computed paths will originate. */
    private int m_iIndexPathStart;

    /** Contains the image being skeletonized. */
    private ModelImage m_kImage;

    /** Describes the organization of the volume data in a linear array. */
    private ModelImage3DLayout m_kVolumeLayout;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a 3D skeletonization of the specified binary volume of data.
     *
     * @param   kImage         ModelImage contains the volume layout and its data
     * @param   kVolumeLayout  ModelImage3DLayout Contains the mapping between sample coordiantes and real coordinates
     *                         for the input volume.
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
    public Skeleton3D(ModelImage kImage, ModelImage3DLayout kVolumeLayout) {

        // Remember these inputs.
        m_kImage = kImage;
        m_kVolumeLayout = kVolumeLayout;

        // What is the total number of samples in the volume.
        final int iNumSamples = m_kVolumeLayout.getNumSamples();

        // Create the volume arrays we will need.
        m_asVolumeData = new short[iNumSamples];
        m_afBoundarySampleDistMin = new float[iNumSamples];


        // Extract the data from the volume.
        try {
            kImage.exportData(0, m_asVolumeData.length, m_asVolumeData);
        } catch (Exception e) {
            throw new IllegalArgumentException("Skeleton3D: IOException on loading data - " + e.getMessage());
        }

        // Mark the samples that are considered to be "inside"
        // the boundary.  Do not consider any samples along
        // the bounds of the volume.
        for (int iZ = 0; iZ < m_kVolumeLayout.m_iDimZ; ++iZ) {

            for (int iY = 0; iY < m_kVolumeLayout.m_iDimY; ++iY) {

                for (int iX = 0; iX < m_kVolumeLayout.m_iDimX; ++iX) {
                    int iIndex = m_kVolumeLayout.getIndex(iX, iY, iZ);

                    if ((0 == iZ) || ((m_kVolumeLayout.m_iDimZ - 1) == iZ) || (0 == iY) ||
                            ((m_kVolumeLayout.m_iDimY - 1) == iY) || (0 == iX) ||
                            ((m_kVolumeLayout.m_iDimX - 1) == iX) || (0 == m_asVolumeData[iIndex])) {
                        m_asVolumeData[iIndex] = 0;
                    } else {
                        m_asVolumeData[iIndex] = RESULT_MASK_INSIDE;
                    }
                }
            }
        }


        // Common items that will be used throughout this method.
        float[] afDistNeighbors27 = new float[27];
        m_kVolumeLayout.getDistanceNeighbors27(afDistNeighbors27);


        // Section 4.1.3.  Compute Distance from Boundary Field
        // This step also computes the penalty at each sample based on
        // its distance to the nearest boundary.
        // Section 4.1.4.  Compute Gradient Vector Field
        Vector3f[] akVolumeBoundaryDistGradient = new Vector3f[iNumSamples];
        computeVolumeBoundaryDist(akVolumeBoundaryDistGradient);

        // DEBUG - output file with the boundary distance volume
        if (DEBUG) {

            // Compute the minimum of the sample scale factors.
            // Iterate with this minimum sample scale factor until it
            // is in the range [10,100).  We will want to scale the
            // floating point Euclidean distances by this distance
            // before we truncate them to an integer so that we get
            // at least two decimal digits of distance for Euclidean
            // distance between the two closest samples in the volume.
            final float fMinSampleScale = Math.min(m_kVolumeLayout.m_fSpacingX,
                                                   Math.min(m_kVolumeLayout.m_fSpacingY, m_kVolumeLayout.m_fSpacingZ));
            float fScaleDist = 1.0f;

            while ((fMinSampleScale * fScaleDist) < 10.0f) {
                fScaleDist *= 10.0f;
            }

            while ((fMinSampleScale * fScaleDist) >= 100.0f) {
                fScaleDist *= 0.1;
            }

            short[] asVolumeBoundaryDist = new short[iNumSamples];

            for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {
                asVolumeBoundaryDist[iIndex] = (short) Math.round(fScaleDist * m_afBoundarySampleDistMin[iIndex]);
            }

            // rendererProgressBar.setMessage("Saving outputBoundaryDist volume file ...");
            saveVolume("outputBoundaryDist.img", FileUtility.XML, asVolumeBoundaryDist);
        }


        // Section 4.1.5.  Flag Nonuniform Gradient Neighborhoods.
        // Section 4.1.6.  Connect Flagged Voxels.
        markPossibleSkeletonSamples(akVolumeBoundaryDistGradient);


        // Section 4.1.7.  Compute Distance from Source Field.
        // Use the first flagged sample in the linear array as the
        // reference source sample.  The sample at the furthest
        // end of the maximum of the minimum cost paths is the
        // the one which must be the starting endpoint for the path.
        int iIndexSource = -1;

        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {

            if (RESULT_MASK_CONNECTED == (RESULT_MASK_CONNECTED & m_asVolumeData[iIndex])) {
                iIndexSource = iIndex;

                break;
            }
        }

        // kProgress.setMessage("Finding start point for branch extraction ...");
        DijkstraMinCostPath kMinCostPath = new DijkstraMinCostPath(m_kVolumeLayout, m_asVolumeData,
                                                                   RESULT_MASK_CONNECTED, iIndexSource,
                                                                   m_afBoundarySampleDistMin, 0, // do not apply penalty
                                                                   0 // do not apply penalty
                                                                  );
        m_iIndexPathStart = kMinCostPath.getIndexFurthestDistance();
        kMinCostPath = null;


        // See getPathGraph method for the remainder of the steps.
        // These steps vary depending on the minimum length of
        // a branch to include in the path graph.
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Called to free up resources.
     */
    public void dispose() {
        m_kImage = null;
        m_kVolumeLayout = null;
        m_asVolumeData = null;
        m_afBoundarySampleDistMin = null;
    }

    /**
     * Called by the garbage collector.
     */
    public void finalize() {
        dispose();
    }

    /**
     * Access the maximum distance of any "inside" point to the boundary.
     *
     * @return  float Maximum of distances.
     */
    public float getMaxBoundaryDistance() {
        return m_fMaxBoundarySampleDistMin;
    }

    /**
     * Given previously determined starting point, compute the tree-structured skeleton path through the volume where no
     * branch is shorter than the specified amount.
     *
     * @param   iMaxBranches      int Maximum number of branches to extract.
     * @param   fMinBranchLength  float All branches must be longer than this length.
     *
     * @return  FlyPathGraphSamples A tree-structured skeleton path where each path is represented by its samples.
     */
    public FlyPathGraphSamples getPathGraph(int iMaxBranches, float fMinBranchLength) {

        // What is the total number of samples in the volume.
        final int iNumSamples = m_kVolumeLayout.getNumSamples();

        // Reset tags assigned to all samples previously marked as being
        // part of the skeleton.
        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {

            if (RESULT_MASK_CONNECTED == (RESULT_MASK_CONNECTED & m_asVolumeData[iIndex])) {
                m_asVolumeData[iIndex] &= ~(RESULT_MASK_SKELETON_NEIGHBOR | RESULT_MASK_SKELETON);
            }
        }

        // Create the path graph.
        FlyPathGraphSamples kPathGraph = new FlyPathGraphSamples();

        for (int iBranch = 0; iBranch < iMaxBranches; ++iBranch) {

            // Section 4.1.8.  Compute Penalized Distance from Root Field.
            // The root sample in the volume is the first centerline endpoint
            // found by the previous step.  This time we use a penalty volume
            // where the positive distance values are replaced with their
            // additive inverses added to the maximum distance for any
            // sample found in the volume.
            // kProgress.setMessage("Extracting branch " + Integer.toString(iBranch+1) + " ...");
            DijkstraMinCostPath kMinCostPath = new DijkstraMinCostPath(m_kVolumeLayout, m_asVolumeData,
                                                                       RESULT_MASK_CONNECTED, m_iIndexPathStart,
                                                                       m_afBoundarySampleDistMin, RESULT_MASK_SKELETON,
                                                                       RESULT_MASK_SKELETON_NEIGHBOR);
            int[] aiIndexPath = kMinCostPath.getIndexPath();
            kMinCostPath = null;

            // Section 4.1.9.  Minimum Cost Path (Centerline).
            // Trace the path through the connected sample between the
            // endpoints that were identified by the Dijkstra minimum
            // cost path algorithms.  Extract the samples that belong
            // to the path, but start from the end with the first
            // sample that is part of a previously defined skeleton.
            // Compute the length of the path (by length of polyline segments).
            int iNumPathSamples = 0;

            for (int iPath = aiIndexPath.length - 1; iPath >= 0; iPath--) {
                ++iNumPathSamples;

                // Get linear array index of next sample along the path.
                int iIndex = aiIndexPath[iPath];

                if (RESULT_MASK_SKELETON == (RESULT_MASK_SKELETON & m_asVolumeData[iIndex])) {
                    break;
                }
            }

            int[] aiPathIndex = new int[iNumPathSamples];
            Vector3f[] akPathPoint = new Vector3f[iNumPathSamples];
            float[] afPathPointBoundaryMinDist = new float[iNumPathSamples];
            float fPathLength = 0.0f;

            for (int iPath = 0; iPath < iNumPathSamples; ++iPath) {

                // Get linear array index of next sample along the path.
                int iIndex = aiIndexPath[iPath + aiIndexPath.length - iNumPathSamples];
                aiPathIndex[iPath] = iIndex;

                // Get the real sample coordinates for the path point.
                // Since we are walking from the end back to the starting
                // point, we fill in the path point array starting at
                // the end.
                int iX = m_kVolumeLayout.getIndexX(iIndex);
                int iY = m_kVolumeLayout.getIndexY(iIndex);
                int iZ = m_kVolumeLayout.getIndexZ(iIndex);
                Vector3f kPathPoint = new Vector3f();
                m_kVolumeLayout.getRealPoint(iX, iY, iZ, kPathPoint);
                akPathPoint[iPath] = kPathPoint;

                // Get the distance of this point to the nearest and
                // furthest boundaries.
                afPathPointBoundaryMinDist[iPath] = m_afBoundarySampleDistMin[iIndex];

                // Accumulate the distance between this point and the previous
                // along the path as an estimate of the total length of
                // this branch.
                if (iPath > 0) {
                    fPathLength += akPathPoint[iPath].distance(akPathPoint[iPath - 1]);
                }
            }

            // Only keep track of the longest path.
            if (fPathLength > fMinBranchLength) {

                // Once we add a branch to the path, then transform the
                // samples along the branch to being part of the skeleton.
                for (int iPath = 0; iPath < akPathPoint.length; iPath++) {
                    markSkeletonPoint(aiPathIndex[iPath], 1.25f * afPathPointBoundaryMinDist[iPath]);
                }

                kPathGraph.add(aiPathIndex, akPathPoint);
            } else {
                break;
            }
        }


        // DEBUG - output file with the classification volume
        if (DEBUG) {
            short[] asVolumeOutput = new short[iNumSamples];

            for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {

                if (RESULT_MASK_SKELETON == (RESULT_MASK_SKELETON & m_asVolumeData[iIndex])) {
                    asVolumeOutput[iIndex] = 4;
                } else if (RESULT_MASK_CONNECTED == (RESULT_MASK_CONNECTED & m_asVolumeData[iIndex])) {
                    asVolumeOutput[iIndex] = 3;
                } else if (RESULT_MASK_FLAGGED == (RESULT_MASK_FLAGGED & m_asVolumeData[iIndex])) {
                    asVolumeOutput[iIndex] = 2;
                } else if (RESULT_MASK_INSIDE == (RESULT_MASK_INSIDE & m_asVolumeData[iIndex])) {
                    asVolumeOutput[iIndex] = 1;
                } else {
                    asVolumeOutput[iIndex] = 0;
                }
            }

            // rendererProgressBar.setMessage("Saving outputResults volume file ...");
            saveVolume("outputResults.img", FileUtility.XML, asVolumeOutput);
        }

        return kPathGraph;
    }
    
    /**
     * Given previously determined starting point, compute the tree-structured skeleton path through the volume where no
     * branch is shorter than the specified amount.
     *
     * @param   iMaxBranches      int Maximum number of branches to extract.
     * @param   fMinBranchLength  float All branches must be longer than this length.
     *
     * @return  FlyPathGraphSamples A tree-structured skeleton path where each path is represented by its samples.
     */
    public short[] getSkeletonizedArray(int iMaxBranches, float fMinBranchLength) {

        // What is the total number of samples in the volume.
        final int iNumSamples = m_kVolumeLayout.getNumSamples();

        // Reset tags assigned to all samples previously marked as being
        // part of the skeleton.
        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {

            if (RESULT_MASK_CONNECTED == (RESULT_MASK_CONNECTED & m_asVolumeData[iIndex])) {
                m_asVolumeData[iIndex] &= ~(RESULT_MASK_SKELETON_NEIGHBOR | RESULT_MASK_SKELETON);
            }
        }

        // Create the path graph.
        FlyPathGraphSamples kPathGraph = new FlyPathGraphSamples();

        for (int iBranch = 0; iBranch < iMaxBranches; ++iBranch) {

            // Section 4.1.8.  Compute Penalized Distance from Root Field.
            // The root sample in the volume is the first centerline endpoint
            // found by the previous step.  This time we use a penalty volume
            // where the positive distance values are replaced with their
            // additive inverses added to the maximum distance for any
            // sample found in the volume.
            // kProgress.setMessage("Extracting branch " + Integer.toString(iBranch+1) + " ...");
            DijkstraMinCostPath kMinCostPath = new DijkstraMinCostPath(m_kVolumeLayout, m_asVolumeData,
                                                                       RESULT_MASK_CONNECTED, m_iIndexPathStart,
                                                                       m_afBoundarySampleDistMin, RESULT_MASK_SKELETON,
                                                                       RESULT_MASK_SKELETON_NEIGHBOR);
            int[] aiIndexPath = kMinCostPath.getIndexPath();
            kMinCostPath = null;

            // Section 4.1.9.  Minimum Cost Path (Centerline).
            // Trace the path through the connected sample between the
            // endpoints that were identified by the Dijkstra minimum
            // cost path algorithms.  Extract the samples that belong
            // to the path, but start from the end with the first
            // sample that is part of a previously defined skeleton.
            // Compute the length of the path (by length of polyline segments).
            int iNumPathSamples = 0;

            for (int iPath = aiIndexPath.length - 1; iPath >= 0; iPath--) {
                ++iNumPathSamples;

                // Get linear array index of next sample along the path.
                int iIndex = aiIndexPath[iPath];

                if (RESULT_MASK_SKELETON == (RESULT_MASK_SKELETON & m_asVolumeData[iIndex])) {
                    break;
                }
            }

            int[] aiPathIndex = new int[iNumPathSamples];
            Vector3f[] akPathPoint = new Vector3f[iNumPathSamples];
            float[] afPathPointBoundaryMinDist = new float[iNumPathSamples];
            float fPathLength = 0.0f;

            for (int iPath = 0; iPath < iNumPathSamples; ++iPath) {

                // Get linear array index of next sample along the path.
                int iIndex = aiIndexPath[iPath + aiIndexPath.length - iNumPathSamples];
                aiPathIndex[iPath] = iIndex;

                // Get the real sample coordinates for the path point.
                // Since we are walking from the end back to the starting
                // point, we fill in the path point array starting at
                // the end.
                int iX = m_kVolumeLayout.getIndexX(iIndex);
                int iY = m_kVolumeLayout.getIndexY(iIndex);
                int iZ = m_kVolumeLayout.getIndexZ(iIndex);
                Vector3f kPathPoint = new Vector3f();
                m_kVolumeLayout.getRealPoint(iX, iY, iZ, kPathPoint);
                akPathPoint[iPath] = kPathPoint;

                // Get the distance of this point to the nearest and
                // furthest boundaries.
                afPathPointBoundaryMinDist[iPath] = m_afBoundarySampleDistMin[iIndex];

                // Accumulate the distance between this point and the previous
                // along the path as an estimate of the total length of
                // this branch.
                if (iPath > 0) {
                    fPathLength += akPathPoint[iPath].distance(akPathPoint[iPath - 1]);
                }
            }

            // Only keep track of the longest path.
            if (fPathLength > fMinBranchLength) {

                // Once we add a branch to the path, then transform the
                // samples along the branch to being part of the skeleton.
                for (int iPath = 0; iPath < akPathPoint.length; iPath++) {
                    markSkeletonPoint(aiPathIndex[iPath], 1.25f * afPathPointBoundaryMinDist[iPath]);
                }

                kPathGraph.add(aiPathIndex, akPathPoint);
            } else {
                break;
            }
        }


        short[] asVolumeOutput = new short[iNumSamples];

        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {

            if (RESULT_MASK_SKELETON == (RESULT_MASK_SKELETON & m_asVolumeData[iIndex])) {
                asVolumeOutput[iIndex] = 1;
            } else {
                asVolumeOutput[iIndex] = 0;
            }
        }

        return asVolumeOutput;
    }

    /**
     * Compute the Euclidean distance from each sample "inside" the volume to the nearest point on the surface
     * surrounding the volume. Based on the algorithm described in "New algorithms for Euclidean distance transformation
     * of an n-dimensional digitized picture with applications", by T. Saito and J. Toriwaki, Pattern Recognition, Vol.
     * 27, No. 11, pp. 1551-1565, 1994. Fills in the m_afBoundarySampleDistMin linear array of floating point values
     * with the Euclidean distance measures; zero values will be stored in the array corresponding to samples outside
     * the surface. Compute the penalty value associated with each sample based on its relative distance to the
     * boundary. Compute the gradient of the distance field at each sample inside the surface (indicated by a positive
     * distance measure).
     *
     * @param  akVolumeBoundaryDistGradient  Vector3f[] Array of normalized boundary distance gradients computed at each
     *                                       sample. The input array is assumed to have all entries null. Upon return,
     *                                       only those samples that are inside the volume will have a gradient value
     *                                       stored.
     */
    private void computeVolumeBoundaryDist(Vector3f[] akVolumeBoundaryDistGradient) {

        // What is the total number of samples in the volume.
        final int iNumSamples = m_kVolumeLayout.getNumSamples();

        // *** NOTE ***
        // The notation here is set to match that in the referenced paper!
        // Only exception is that all indexing is zero-based here as opposed
        // to being one-based in the paper.

        // Setup.
        // Minimum boundary distance is in sample coordinates so
        // use default XYZ spacing of 1.0.
        // Maximum boundary distance is in real coordinates so
        // use anisotropic spacing of the volume.
        //final float fSpacingX = m_kVolumeLayout.m_fSpacingX;
        //final float fSpacingY = m_kVolumeLayout.m_fSpacingY;
        //final float fSpacingZ = m_kVolumeLayout.m_fSpacingZ;
        final int L = m_kVolumeLayout.m_iDimX;
        final int M = m_kVolumeLayout.m_iDimY;
        final int N = m_kVolumeLayout.m_iDimZ;
        final int n = Math.max(L, Math.max(M, N));
        final int iIndexIncX = m_kVolumeLayout.getIndexIncX();
        final int iIndexIncY = m_kVolumeLayout.getIndexIncY();
        final int iIndexIncZ = m_kVolumeLayout.getIndexIncZ();
        float[] buff = new float[n];

        // Setup up progress
        // Step 1 = N iterations (outer loops over Z coordinate)
        // Step 2 = N iterations (outer loops over Z coordinate)
        // Step 3 = M iterations (outer loops over Y coordinate)
        //final int iProgressOffsetStep1 = 0;
        //final int iProgressOffsetStep2 = iProgressOffsetStep1 + N;
        //final int iProgressOffsetStep3 = iProgressOffsetStep2 + N;
        //final int iProgressOffsetLast = iProgressOffsetStep3 + M;
        // rendererProgressBar.setMessage("Computing boundary distance ...");
        // kProgress.setRange(0,iProgressOffsetLast-1);

        // Step 1: forward and reverse scans iterating over X
        for (int k = 0, iIndexZ = 0; k < N; ++k, iIndexZ += iIndexIncZ) {

            for (int j = 0, iIndexYZ = iIndexZ; j < M; ++j, iIndexYZ += iIndexIncY) {

                // forward scan
                float df = 0.0f;

                for (int i = 0, iIndex = iIndexYZ; i < L; ++i, iIndex += iIndexIncX) {

                    if (0 == m_asVolumeData[iIndex]) {
                        df = 0.0f;
                    } else {
                        df = df + 1.0f;
                    }

                    m_afBoundarySampleDistMin[iIndex] = df * df;
                }

                // reverse scan
                float db = 0.0f;

                for (int i = L - 1, iIndex = iIndexYZ + ((L - 1) * iIndexIncX); i >= 0; --i, iIndex -= iIndexIncX) {

                    if (0 == m_asVolumeData[iIndex]) {
                        db = 0.0f;
                    } else {
                        db = db + 1.0f;
                    }

                    float fDist = db * db;

                    if (fDist < m_afBoundarySampleDistMin[iIndex]) {
                        m_afBoundarySampleDistMin[iIndex] = fDist;
                    }
                }
            }
        }

        // Step 2: iterate over Y
        for (int k = 0, iIndexZ = 0; k < N; ++k, iIndexZ += iIndexIncZ) {

            for (int i = 0, iIndexXZ = iIndexZ; i < L; ++i, iIndexXZ += iIndexIncX) {

                // minimum
                for (int j = 0, iIndex = iIndexXZ; j < M; ++j, iIndex += iIndexIncY) {
                    buff[j] = m_afBoundarySampleDistMin[iIndex];
                }

                for (int j = 0, iIndex = iIndexXZ; j < M; ++j, iIndex += iIndexIncY) {
                    float d = buff[j];

                    if (d > 0.0) {
                        int rMax = (int) (Math.sqrt(d)) + 1;
                        int rStart = (rMax < j) ? rMax : j;
                        int rEnd = M - 1 - j;

                        if (rMax < rEnd) {
                            rEnd = rMax;
                        }

                        for (int r = -rStart; r <= rEnd; ++r) {
                            float rDist = r;
                            float w = buff[j + r] + (rDist * rDist);

                            if (w < d) {
                                d = w;
                            }
                        }

                        m_afBoundarySampleDistMin[iIndex] = d;
                    }
                }
            }
        }

        // Step 3: iterate over Z
        for (int j = 0, iIndexY = 0; j < M; ++j, iIndexY += iIndexIncY) {

            for (int i = 0, iIndexXY = iIndexY; i < L; ++i, iIndexXY += iIndexIncX) {

                // minimum
                for (int k = 0, iIndex = iIndexXY; k < N; ++k, iIndex += iIndexIncZ) {
                    buff[k] = m_afBoundarySampleDistMin[iIndex];
                }

                for (int k = 0, iIndex = iIndexXY; k < N; ++k, iIndex += iIndexIncZ) {
                    float d = buff[k];

                    if (d > 0.0) {
                        int rMax = (int) (Math.sqrt(d)) + 1;
                        int rStart = (rMax < k) ? rMax : k;
                        int rEnd = N - 1 - k;

                        if (rMax < rEnd) {
                            rEnd = rMax;
                        }

                        for (int r = -rStart; r <= rEnd; ++r) {
                            float rDist = r;
                            float w = buff[k + r] + (rDist * rDist);

                            if (w < d) {
                                d = w;
                            }
                        }

                        m_afBoundarySampleDistMin[iIndex] = (float) Math.sqrt(d);
                    } else {
                        m_afBoundarySampleDistMin[iIndex] = 0.0f;
                    }
                }
            }
        }

        // Compute the maximum of the distances to the nearest boundary.
        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) { }


        // Loop through each sample in the volume to compute
        // - normalized gradient of minimum boundary distance
        // - maximum of minimum boundary distances
        // rendererProgressBar.setMessage("Computing boundary distance gradient ...");
        // rendererProgressBar.setRange(0,m_kVolumeLayout.m_iDimZ-1);
        m_fMaxBoundarySampleDistMin = 0.0f;

        ModelImage3DLayout.GradientNeighbors kIndexGradient = new ModelImage3DLayout.GradientNeighbors();

        for (int iZ = 0; iZ < m_kVolumeLayout.m_iDimZ; ++iZ) {

            for (int iY = 0; iY < m_kVolumeLayout.m_iDimY; ++iY) {

                for (int iX = 0; iX < m_kVolumeLayout.m_iDimX; ++iX) {

                    // Get the index of the sample for which the
                    // gradient is to be computed.
                    int iIndex = m_kVolumeLayout.getIndex(iX, iY, iZ);

                    if (m_fMaxBoundarySampleDistMin < m_afBoundarySampleDistMin[iIndex]) {
                        m_fMaxBoundarySampleDistMin = m_afBoundarySampleDistMin[iIndex];
                    }

                    // Only compute the gradient for samples that are
                    // considered to be "inside" the surface.
                    if (m_afBoundarySampleDistMin[iIndex] <= 0.0f) {
                        continue;
                    }

                    // Get the indexes of the 6-connected neighbors to
                    // be used in computing finite differences for
                    // the gradient.
                    m_kVolumeLayout.getIndexNeighborsGradient(iX, iY, iZ, kIndexGradient);

                    // Compute the finite differences along each dimension.
                    float fX0 = m_afBoundarySampleDistMin[kIndexGradient.m_iX0];
                    float fX1 = m_afBoundarySampleDistMin[kIndexGradient.m_iX1];
                    float fY0 = m_afBoundarySampleDistMin[kIndexGradient.m_iY0];
                    float fY1 = m_afBoundarySampleDistMin[kIndexGradient.m_iY1];
                    float fZ0 = m_afBoundarySampleDistMin[kIndexGradient.m_iZ0];
                    float fZ1 = m_afBoundarySampleDistMin[kIndexGradient.m_iZ1];

                    // Compute and store the gradient.
                    // Make sure the gradient vector is normalized.
                    Vector3f kGradient = new Vector3f((fX1 - fX0) / kIndexGradient.m_fDeltaX,
                                                      (fY1 - fY0) / kIndexGradient.m_fDeltaY,
                                                      (fZ1 - fZ0) / kIndexGradient.m_fDeltaZ);
                    akVolumeBoundaryDistGradient[iIndex] = kGradient;
                }
            }
        }
    }

    /**
     * Mark all inside samples that have nonuniform boundary distance gradients and then "connect" such samples by
     * following the gradient. Updates the m_asVolumeData linear array of 16-bit signed bit mask values representing the
     * state of each sample in the skeletonziation algorithm. The RESULT_MASK_* constant bit masks are OR'ed into this
     * field. Any sample having nonuniform gradients will be marked with the RESULT_MASK_NONUNIFORM_GRADIENT bit mask
     * and the RESULT_MASK_FLAGGED bit mask. Any sample along the gradient path of connecting the nonuniform gradient
     * samples will be marked only with the RESULT_MASK_FLAGGED bit mask. This array has values already set upon input
     * and only OR's in additional bit masks.
     *
     * @param  akVolumeBoundaryDistGradient  linear array of Vector3f instances representing the (unnormalized) boundary
     *                                       distance gradient vectors for inside samples. The null reference will be
     *                                       stored for outside samples.
     */
    private void markPossibleSkeletonSamples(Vector3f[] akVolumeBoundaryDistGradient) {

        // What is the total number of samples in the volume.
        final int iNumSamples = m_kVolumeLayout.getNumSamples();

        // Common items that will be used throughout this method.
        int[] aiIndexNeighbors27 = new int[27];


        // This is done by looking at each 2^3 voxel positions for which
        // each sample in the voxel is "inside" the surface.  Compute the
        // normalized average gradient distance field vector for each voxel,
        // i.e., an average of 8 gradient vectors.  Then test if the
        // dot-product between this average vector and any of the gradient
        // vectors for the 8 voxel samples is negative or zero.  Label all 8
        // voxel samples that have non-positive dot-products as
        // having non-uniform gradient neighbors.
        // rendererProgressBar.setMessage("Marking nonuniform gradients ...");
        // rendererProgressBar.setRange(0,m_kVolumeLayout.m_iDimZ-1);
        int[] aiIndexNeighborsVoxel = new int[8];
        Vector3f kAvgGradient = new Vector3f();

        for (int iZ = 1; iZ < m_kVolumeLayout.m_iDimZ; ++iZ) {

            for (int iY = 1; iY < m_kVolumeLayout.m_iDimY; ++iY) {

                for (int iX = 1; iX < m_kVolumeLayout.m_iDimX; ++iX) {

                    // Get the indexes of the samples which make up
                    // the current voxel.  Note that the
                    // getIndexNeighborsVoxel will indexes where the
                    // specific identifying one is the one in the
                    // "minimum" corner of the voxel.  But since
                    // we started our sample index couting at 1
                    // for all axes, then we need to subtract 1 here
                    // to make sure we get the correct indexes.
                    m_kVolumeLayout.getIndexNeighborsVoxel(iX - 1, iY - 1, iZ - 1, aiIndexNeighborsVoxel);

                    // Make sure that all the samples for this voxel
                    // are "inside" the volume and not on the boundary
                    // of the surface.  If not all the samples are "inside"
                    // the surface, then skip this voxel.
                    boolean bAllInside = true;

                    for (int iIndex = 0; iIndex < 8; ++iIndex) {

                        if (null == akVolumeBoundaryDistGradient[aiIndexNeighborsVoxel[iIndex]]) {
                            bAllInside = false;

                            break;
                        }
                    }

                    if (!bAllInside) {
                        continue;
                    }

                    final boolean bUseAlgorithm1 = false;
                    final boolean bUseAlgorithm2 = !bUseAlgorithm1;

                    if (bUseAlgorithm1) {

                        // Look at each pairing of the 8 neighbors.
                        // If the dot-product of their (unnormalized) gradient
                        // vectors is negative or zero, then mark the entire
                        // set of neighbors as having nonuniform gradients.
                        boolean bNonUniform = false;

                        for (int iIndexA = 0; iIndexA < 8; ++iIndexA) {
                            int iIndexNeighborA = aiIndexNeighborsVoxel[iIndexA];
                            Vector3f kGradientA = akVolumeBoundaryDistGradient[iIndexNeighborA];

                            for (int iIndexB = iIndexA + 1; iIndexB < 8; ++iIndexB) {

                                int iIndexNeighborB = aiIndexNeighborsVoxel[iIndexB];
                                Vector3f kGradientB = akVolumeBoundaryDistGradient[iIndexNeighborB];

                                if (kGradientA.dot(kGradientB) <= 0.0f) {
                                    bNonUniform = true;
                                }
                            }
                        }

                        // If at least one pair is nonuniform, then mark all
                        // as being nonuniform.
                        if (bNonUniform) {

                            for (int iIndex = 0; iIndex < 8; ++iIndex) {
                                m_asVolumeData[aiIndexNeighborsVoxel[iIndex]] |= RESULT_MASK_NONUNIFORM_GRADIENT;
                            }
                        }
                    }

                    if (bUseAlgorithm2) {

                        // Compute the average gradient for the samples
                        // the comprise the voxel.
                        kAvgGradient.set(0.0f, 0.0f, 0.0f);

                        for (int iIndex = 0; iIndex < 8; ++iIndex) {
                            kAvgGradient.add(akVolumeBoundaryDistGradient[aiIndexNeighborsVoxel[iIndex]]);
                        }

                        kAvgGradient.normalize();

                        // Check to see if any sample in the voxel set of
                        // samples has a nonuniform gradient relative to
                        // the average.
                        boolean bNonUniform = false;

                        for (int iIndex = 0; iIndex < 8; ++iIndex) {

                            if (kAvgGradient.dot(akVolumeBoundaryDistGradient[aiIndexNeighborsVoxel[iIndex]]) <= 0.0f) {
                                bNonUniform = true;

                                break;
                            }
                        }

                        // If at least one is nonuniform, then mark all
                        // as being nonuniform.
                        if (bNonUniform) {

                            for (int iIndex = 0; iIndex < 8; ++iIndex) {
                                m_asVolumeData[aiIndexNeighborsVoxel[iIndex]] |= RESULT_MASK_NONUNIFORM_GRADIENT;
                            }
                        }
                    }
                }
            }
        }


        // Section 4.1.6.  Connect Flagged Voxels.
        // Start with a set of all the volume samples that were marked as
        // having nonuniform gradients.  A TreeSet is used because
        // the entries are ordered.
        TreeSet<Integer> kTreeSetFlaggedSamples = new TreeSet<Integer>();

        for (int iZ = 0; iZ < m_kVolumeLayout.m_iDimZ; ++iZ) {

            for (int iY = 0; iY < m_kVolumeLayout.m_iDimY; ++iY) {

                for (int iX = 0; iX < m_kVolumeLayout.m_iDimX; ++iX) {
                    int iIndex = m_kVolumeLayout.getIndex(iX, iY, iZ);

                    if (RESULT_MASK_NONUNIFORM_GRADIENT == (RESULT_MASK_NONUNIFORM_GRADIENT & m_asVolumeData[iIndex])) {
                        kTreeSetFlaggedSamples.add(new Integer(iIndex));
                        m_asVolumeData[iIndex] |= RESULT_MASK_FLAGGED;
                    }
                }
            }
        }

        Vector3f kUnitGradient = new Vector3f();

        while (!kTreeSetFlaggedSamples.isEmpty()) {

            // Get and remove any one of the remaining flagged samples that
            // have not been processed yet.  Get the XYZ sample
            // coordinates from its linear array index.
            Integer kInteger = kTreeSetFlaggedSamples.first();
            kTreeSetFlaggedSamples.remove(kInteger);

            int iIndex = kInteger.intValue();
            int iX = m_kVolumeLayout.getIndexX(iIndex);
            int iY = m_kVolumeLayout.getIndexY(iIndex);
            int iZ = m_kVolumeLayout.getIndexZ(iIndex);

            // Follow the gradient to the next sample inside the volume
            // until another flagged sample is reached.
            while (true) {

                // Get the gradient, but check to make sure that we have
                // not gone outside the volume (which is indicated by
                // a sample not having its gradient computed).
                Vector3f kGradient = akVolumeBoundaryDistGradient[iIndex];

                if (null == kGradient) {
                    break;
                }

                // The gradient is not unit length, but if we are to
                // follow the gradient as we walk through the samples,
                // then we need a unit length version of the gradient.
                kUnitGradient.copy(kGradient).normalize();

                // Proceed to the next sample by following the gradient.
                // Use rounding because unless the gradient is aligned
                // with an orthogonal axis, none of the gradient vector's
                // elements will have a magnitude > 1.  Use our own
                // rounding to avoid the function call to Math.round.
                if (kUnitGradient.X >= 0.0f) {
                    iX += (int) (kUnitGradient.X + 0.5f);
                } else {
                    iX -= (int) (-kUnitGradient.X + 0.5f);
                }

                if (kUnitGradient.Y >= 0.0f) {
                    iY += (int) (kUnitGradient.Y + 0.5f);
                } else {
                    iY -= (int) (-kUnitGradient.Y + 0.5f);
                }

                if (kUnitGradient.Z >= 0.0f) {
                    iZ += (int) (kUnitGradient.Z + 0.5f);
                } else {
                    iZ -= (int) (-kUnitGradient.Z + 0.5f);
                }

                // Get the index in the linear array but check to make
                // sure that we do not go beyond the volume boundary.
                iIndex = m_kVolumeLayout.getIndexChecked(iX, iY, iZ);

                if (iIndex < 0) {
                    break;
                }

                // Reached a sample that is already flagged?
                if (RESULT_MASK_FLAGGED == (RESULT_MASK_FLAGGED & m_asVolumeData[iIndex])) {

                    // If sample is still in the list of those
                    // to be set of those to be processed, just
                    // use it and continue following the gradient
                    // from here.  If not, then find another sample from
                    // which to start following the gradient because we
                    // have already processed this sample.
                    if (!kTreeSetFlaggedSamples.remove(new Integer(iIndex))) {
                        break;
                    }
                }

                // Flag the unflagged sample as we visit it.
                m_asVolumeData[iIndex] |= RESULT_MASK_FLAGGED;
            }
        }


        // Label the connected flagged samples.
        // Keep count of how many samples have each label.
        // kProgress.setMessage("Finding largest connection ...");
        kTreeSetFlaggedSamples = new TreeSet<Integer>();

        short[] asVolumeLabeledConnections = new short[iNumSamples];

        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {
            asVolumeLabeledConnections[iIndex] = -1;

            if (RESULT_MASK_FLAGGED == (RESULT_MASK_FLAGGED & m_asVolumeData[iIndex])) {
                kTreeSetFlaggedSamples.add(new Integer(iIndex));
            }
        }

        // kProgress.setRange(kTreeSetFlaggedSamples.size()-1,0);
        short sConnectLabel = 0;
        ArrayList<Integer> kConnectLabelCount = new ArrayList<Integer>();
        //int len = kTreeSetFlaggedSamples.size() - 1;

        while (!kTreeSetFlaggedSamples.isEmpty()) {

            // Keep count of how many samples in this connection.
            int iConnectCount = 0;

            // Get any remaining sample that has been flagged.
            // That sample is identified by its linear array index.
            // Easiest to just get the first.
            TreeSet<Integer> kTreeSet = new TreeSet<Integer>();
            kTreeSet.add(kTreeSetFlaggedSamples.first());

            while (!kTreeSet.isEmpty()) {
                Integer kInteger = kTreeSet.first();
                kTreeSet.remove(kInteger);

                if (kTreeSetFlaggedSamples.remove(kInteger)) {
                    int iIndexCenter = kInteger.intValue();

                    // Label sample as being part of the current connection.
                    asVolumeLabeledConnections[iIndexCenter] = sConnectLabel;
                    ++iConnectCount;

                    // Get linear array indexes of 26-connected neighbors.
                    m_kVolumeLayout.getIndexNeighbors27(iIndexCenter, aiIndexNeighbors27);

                    for (int iIndex = 1; iIndex < 27; ++iIndex) {
                        kInteger = new Integer(aiIndexNeighbors27[iIndex]);

                        if (kTreeSetFlaggedSamples.contains(kInteger)) {
                            kTreeSet.add(kInteger);
                        }
                    }
                }
            }

            // Store the number of samples with this label.
            kConnectLabelCount.add(sConnectLabel++, new Integer(iConnectCount));
        }

        int iLabelMaxCount = 0;

        for (int iLabel = 1; iLabel < kConnectLabelCount.size(); ++iLabel) {
            int iCountA = kConnectLabelCount.get(iLabelMaxCount).intValue();
            int iCountB = kConnectLabelCount.get(iLabel).intValue();

            if (iCountA < iCountB) {
                iLabelMaxCount = iLabel;
            }
        }

        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {

            if (iLabelMaxCount == asVolumeLabeledConnections[iIndex]) {
                m_asVolumeData[iIndex] |= RESULT_MASK_CONNECTED;
            }
        }

        asVolumeLabeledConnections = null;
    }

    /**
     * Mark the specified volume sample as being part of the skeleton and mark all the connected samples within the
     * specified radius of the specified sample as being neighbors.
     *
     * @param  iIndex           int Identifies the sample in the volume to be markes as being on the skeleton path.
     * @param  fNeighborRadius  float Radius of connected neighbors to mark as being skeleton neighbors.
     */
    private void markSkeletonPoint(int iIndex, float fNeighborRadius) {

        // Mark the sample as being part of the skeletonization.
        m_asVolumeData[iIndex] |= RESULT_MASK_SKELETON;

        // Get the distance to the nearest boundary and use that
        // as a radius to mark all connected neighbors of this
        // skeleton as being skeleton neighbors.
        float fNeighborRadiusSquared = fNeighborRadius * fNeighborRadius;
        int iRadius = (int) Math.ceil(fNeighborRadius);
        int iXMid = m_kVolumeLayout.getIndexX(iIndex);
        int iYMid = m_kVolumeLayout.getIndexY(iIndex);
        int iZMid = m_kVolumeLayout.getIndexZ(iIndex);
        int iXMin = iXMid - iRadius;
        int iXMax = iXMid + iRadius;
        int iYMin = iYMid - iRadius;
        int iYMax = iYMid + iRadius;
        int iZMin = iZMid - iRadius;
        int iZMax = iZMid + iRadius;

        if (iXMin < 0) {
            iXMin = 0;
        }

        if (iYMin < 0) {
            iYMin = 0;
        }

        if (iZMin < 0) {
            iZMin = 0;
        }

        if (iXMax >= m_kVolumeLayout.m_iDimX) {
            iXMax = m_kVolumeLayout.m_iDimX - 1;
        }

        if (iYMax >= m_kVolumeLayout.m_iDimY) {
            iYMax = m_kVolumeLayout.m_iDimY - 1;
        }

        if (iZMax >= m_kVolumeLayout.m_iDimZ) {
            iZMax = m_kVolumeLayout.m_iDimZ - 1;
        }

        for (int iZ = iZMin; iZ <= iZMax; ++iZ) {

            for (int iY = iYMin; iY <= iYMax; ++iY) {

                for (int iX = iXMin; iX <= iXMax; ++iX) {
                    int iIndexNeighbor = m_kVolumeLayout.getIndex(iX, iY, iZ);

                    // Ignore any samples NOT connected.
                    if (RESULT_MASK_CONNECTED != (RESULT_MASK_CONNECTED & m_asVolumeData[iIndexNeighbor])) {
                        continue;
                    }

                    // Ignore any samples already marked
                    // as being a skeleton neighbor.
                    if (RESULT_MASK_SKELETON_NEIGHBOR ==
                            (RESULT_MASK_SKELETON_NEIGHBOR & m_asVolumeData[iIndexNeighbor])) {
                        continue;
                    }

                    // Compute distance to current path sample.
                    // Mark neighbors as those being with radius
                    // defined by nearest boundary distance.
                    // Use distance-squared measurement to avoid
                    // unnecessary sqrt calculations.
                    int iDX = iX - iXMid;
                    int iDY = iY - iYMid;
                    int iDZ = iZ - iZMid;
                    float fDistSquared = (iDX * iDX) + (iDY * iDY) + (iDZ * iDZ);

                    if (fDistSquared <= fNeighborRadiusSquared) {
                        m_asVolumeData[iIndexNeighbor] |= RESULT_MASK_SKELETON_NEIGHBOR;
                    }
                }
            }
        }
    }

    /**
     * Write the specified volume to the file of the specified name.
     *
     * @param  kFilename     name of the file for storing the volume
     * @param  iFileType     type of file selected from one of the constants defined in FileBase class.
     * @param  asVolumeData  linear array of signed 16-bit values containing the volume data at each sample where the
     *                       samples are identified by the specified volume layout
     */
    private void saveVolume(String kFilename, int iFileType, short[] asVolumeData) {

        // Create a ModelImage for short values.  Even though the
        // generateShortImage method creates an image with values
        // initialized to the VOI information, we will end up ignoring it
        // by overwriting it.
        ModelImage kImage = (ModelImage) m_kImage.clone();

        // Load our volume data values into the ModelImage instance.
        // Tell it to recalculate min/max.
        try {
            kImage.importData(0, asVolumeData, true);
            kImage.saveImage("", kFilename, iFileType, false);
        } catch (Exception e) {
            return;
        }
    }
}
