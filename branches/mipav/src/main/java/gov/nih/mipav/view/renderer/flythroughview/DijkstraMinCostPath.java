package gov.nih.mipav.view.renderer.flythroughview;


import gov.nih.mipav.model.structures.*;

import java.util.*;


/**
 * Implementation of the Dijkstra minimum cost path algorithm through a binary volume.
 */
public class DijkstraMinCostPath {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Array of cost information for each sample. If an item in the array is null, then that means the sample in the
     * volume is not being considered for being in the path.
     */
    private DijkstraCostItem[] m_akVolumeCost;

    /** Linear array index of the sample that is the furthest away from the starting sample. */
    private int m_iIndexFurthestDistance;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Compute the minimum cost path through the 26-connected specified volume of samples, only considering those
     * samples which are marked with the specified mask and starting at the specified sample.
     *
     * @param  kVolumeLayout              description of the layout of the volume
     * @param  asVolumeData               linear array of signed 16-bit data values for each sample in the volume. The
     *                                    values are assumed to be bit masks which are being used to classify each
     *                                    sample.
     * @param  iMaskConnected             bit mask used to select which classification of samples to consider for the
     *                                    path
     * @param  iIndexStart                linear array index into the volume for the sample which is assumed to be the
     *                                    starting point for all paths to be considered
     * @param  afBoundaryDist             linear array of floating distances to the nearest boundary. This can be used
     *                                    to create a penalty cost for being close to the boundary, depending on whether
     *                                    the iMaskPreviousPath and iMaskPreviousPathNeighbor are not zero.
     * @param  iMaskPreviousPath          bit mask used to classify which samples are already part of a previously
     *                                    defined path. If this value is zero, then the penalty cost is not applied.
     * @param  iMaskPreviousPathNeighbor  bit mask used to classify which samples are neighbors of a previously defined
     *                                    path. If this value is zero, then the penalty cost is not applied.
     */
    DijkstraMinCostPath(ModelImage3DLayout kVolumeLayout, short[] asVolumeData, int iMaskConnected, int iIndexStart,
                        float[] afBoundaryDist, int iMaskPreviousPath, int iMaskPreviousPathNeighbor) {

        // Constants
        final int iNumSamples = kVolumeLayout.getNumSamples();

        // If a penalty volume is defined, then determine its maximum value.
        final float fPenaltyScale = 10.0f;
        float fMaxBoundaryDist = 0.0f;

        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {

            if (fMaxBoundaryDist < afBoundaryDist[iIndex]) {
                fMaxBoundaryDist = afBoundaryDist[iIndex];
            }
        }

        boolean bApplyPenaltyCost = ((0 != iMaskPreviousPath) && (0 != iMaskPreviousPathNeighbor));


        // These arrays are used to compute costs associated
        // with the 26-connected neighbors about the current
        // sample in the iteration being considered.
        int[] aiIndexNeighbors27 = new int[27];
        float[] afDistNeighbors27 = new float[27];
        kVolumeLayout.getDistanceNeighbors27(afDistNeighbors27);

        // Allocate array to store the cost information for each
        // sample being considered.
        m_akVolumeCost = new DijkstraCostItem[iNumSamples];

        TreeSet<DijkstraCostItem> kTreeSetSourceCostRemain = new TreeSet<DijkstraCostItem>();

        for (int iIndex = 0; iIndex < iNumSamples; ++iIndex) {

            // Create a set that contains all the samples that still
            // need their final distances computed.
            if (iMaskConnected == (iMaskConnected & asVolumeData[iIndex])) {
                DijkstraCostItem kCostItem = new DijkstraCostItem(iIndex);
                m_akVolumeCost[iIndex] = kCostItem;

                if (iIndex != iIndexStart) {
                    kTreeSetSourceCostRemain.add(kCostItem);
                } else {
                    m_akVolumeCost[iIndexStart].markStart();
                }
            }
        }

        // Loop until all the possible paths have been computed.
        // Keep track of the sample that ends up being the furthest
        // away from the starting point.
        // ViewJFrameVolumeView.getRendererProgressBar().setRange(kTreeSetSourceCostRemain.size(),0);
        int iIndexMinDist = iIndexStart;
        int iIndexFurthestDistance = iIndexStart;
        //int len = kTreeSetSourceCostRemain.size() - 1;

        while (!kTreeSetSourceCostRemain.isEmpty()) {

            // Get the accumulated cost to the current sample.
            final float fCurrentCost = m_akVolumeCost[iIndexMinDist].getCost();

            // Get the accumulated distance to the current sample.
            final float fCurrentDist = m_akVolumeCost[iIndexMinDist].getDistance();

            // Get the accumulated number of samples along
            // the minimum cost path so far to this current sample.
            final int iCurrentNumPrev = m_akVolumeCost[iIndexMinDist].getNumPrev();

            // Loop through each of the 26-connected neighbors surrounding
            // this sample, and only those that need their distances
            // finalized.  Add the accmulated distance from this sample
            // to the distance to the neighbor sample and track the
            // minimum distance to each neighbor sample.
            kVolumeLayout.getIndexNeighbors27(iIndexMinDist, aiIndexNeighbors27);

            for (int iIndexNeighbor = 1; iIndexNeighbor < 27; ++iIndexNeighbor) {
                int iIndex = aiIndexNeighbors27[iIndexNeighbor];
                DijkstraCostItem kCostItem = m_akVolumeCost[iIndex];

                // A null DijkstraCostItem reference at the neighbor
                // sample indicates that the neighbor sample is outside.
                if ((null != kCostItem) && !kCostItem.isFinalized()) {
                    float fDist = fCurrentDist;
                    float fCost = fCurrentCost;


                    // Are we doing a penalized distance calcuation?
                    if (bApplyPenaltyCost) {

                        // Sample is already part of previous path?
                        if (iMaskPreviousPath == (iMaskPreviousPath & asVolumeData[iIndex])) {
                            // no cost
                            // no distance
                        }

                        // Sample is a neighbor of a previously defined path?
                        else if (iMaskPreviousPathNeighbor == (iMaskPreviousPathNeighbor & asVolumeData[iIndex])) {

                            // Compute maximum penalty.
                            float fPenaltyCost = fPenaltyScale * fMaxBoundaryDist;

                            // add cost
                            fCost += afDistNeighbors27[iIndexNeighbor];
                            fCost += fPenaltyCost;

                            // no distance
                        }

                        // Sample is possible for path.
                        else {

                            // Compute penalty based on boundary distance;
                            // more penalty the closer to the boundary.
                            float fPenaltyCost = fPenaltyScale * (fMaxBoundaryDist - afBoundaryDist[iIndex]);

                            // add cost
                            fCost += afDistNeighbors27[iIndexNeighbor];
                            fCost += fPenaltyCost;

                            // add distance
                            fDist += afDistNeighbors27[iIndexNeighbor];
                        }
                    } else {
                        fDist += afDistNeighbors27[iIndexNeighbor];
                        fCost += afDistNeighbors27[iIndexNeighbor];
                    }

                    // If we update the distance, then we need to remove
                    // the sample from the TreeSet, update the accmulated
                    // cost for the sample, note the index of the previous
                    // sample to get to this sample along the shortest
                    // path, and then reinsert the sample into the TreeSet
                    // so that it gets properly reordered based on minimum
                    // cost.
                    if (fCost < kCostItem.getCost()) {
                        kTreeSetSourceCostRemain.remove(kCostItem);
                        kCostItem.update(fCost, fDist, iIndexMinDist, 1 + iCurrentNumPrev);
                        kTreeSetSourceCostRemain.add(kCostItem);
                    }
                }
            }

            // Find the sample still in the set of those that need
            // to have their computed distances finalized, the sample
            // having the smallest distance computed thus far.
            // Remove this min distance sample from the set of those
            // remaining to have their computed distance finalized.
            // By removing it from the set, this sample is now considered
            // to have its computed distance finalized.
            DijkstraCostItem kRemainMinDistItem = kTreeSetSourceCostRemain.first();
            kTreeSetSourceCostRemain.remove(kRemainMinDistItem);
            iIndexMinDist = kRemainMinDistItem.getKey();
            kRemainMinDistItem.markFinalized();

            if (kRemainMinDistItem.getDistance() > m_akVolumeCost[iIndexFurthestDistance].getDistance()) {
                iIndexFurthestDistance = kRemainMinDistItem.getKey();
            }
        }

        // When we are done, we will also have found the point along
        // any path which is the furthest from the starting point along
        // a path of minimum cost.
        m_iIndexFurthestDistance = iIndexFurthestDistance;

        // Cleanup
        aiIndexNeighbors27 = null;
        afDistNeighbors27 = null;
        kTreeSetSourceCostRemain = null;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Called to free up object memory for member data.
     */
    public void dispose() {
        m_akVolumeCost = null;
    }

    /**
     * Called by garbage collector to free up object memory.
     */
    public void finalize() {
        dispose();
    }

    /**
     * Get the index of the volume sample stored in a linear array which is at the end of the longest minimum cost path.
     *
     * @return  int Index of volume sample stored in a linear array.
     */
    public int getIndexFurthestDistance() {
        return m_iIndexFurthestDistance;
    }

    /**
     * Get the indexes of the samples extracted along the longest minimum cost path.
     *
     * @return  int[] Array of indexes into volume stored in a linear array for the samples extracted along the longest
     *          minimum cost path. The order of the points in the array defines the connectivity of the points to create
     *          the path.
     */
    public int[] getIndexPath() {

        // Trace the path through the connected sample between the
        // endpoints that were identified by the Dijkstra minimum
        // cost path algorithms and extract the index of the samples
        // which were along the longest minimum cost path.
        DijkstraCostItem kCostItem = m_akVolumeCost[m_iIndexFurthestDistance];
        int[] aiIndexPath = new int[kCostItem.getNumPrev() + 1];
        int iIndex = aiIndexPath.length;

        while (null != kCostItem) {
            aiIndexPath[--iIndex] = kCostItem.getKey();

            // Traverse to the previous sample along the path.
            if (DijkstraCostItem.KEY_UNDEFINED == kCostItem.getKeyPrev()) {
                kCostItem = null;
            } else {
                kCostItem = m_akVolumeCost[kCostItem.getKeyPrev()];
            }
        }

        return aiIndexPath;
    }
}
