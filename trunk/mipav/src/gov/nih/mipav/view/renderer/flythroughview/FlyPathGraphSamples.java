package gov.nih.mipav.view.renderer.flythroughview;


import WildMagic.LibFoundation.Mathematics.*;
import java.util.*;


/**
 * Extension of the FlyPathGraph class which allows for the storage of the samples used to generate the individual
 * curves where curves are the nodes of the graph.
 */
public class FlyPathGraphSamples extends FlyPathGraph {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Storage for the arrays of keys and , positions for each curve. The size of these arrays must match and the
     * entries must correspond for the same index. The size of these arrays must also match the size of the arrays in
     * the base class, and must correspond for the same index.
     */
    protected ArrayList<int[]> m_kListArrayPointKey; // array of int[]

    /** DOCUMENT ME! */
    protected ArrayList<Vector3f[]> m_kListArrayPointPosition; // array of Vector3f[]

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor. Initially empty path with no branches.
     */
    public FlyPathGraphSamples() {
        m_kListArrayPointKey = new ArrayList<int[]>();
        m_kListArrayPointPosition = new ArrayList<Vector3f[]>();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Add the following information for a "branch" path of the graph: an array of key values for each point along the
     * path, an array of 3D coordinates assoicated with each point, and an array of distances each point is from its
     * nearest boundary.
     *
     * @param   aiPointKey       int[] Array of unique integer key values associated with each point along the path.
     * @param   akPointPosition  Point3f[] Array of 3D positions associated with each point along the path.
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
    public void add(int[] aiPointKey, Vector3f[] akPointPosition) {

        // All arrays must be valid.
        if ((null == aiPointKey) || (null == akPointPosition)) {
            throw new IllegalArgumentException("FlyPathGraphSamples.add: arrays must not be null.");
        }

        // Arrays must have the same positive size.
        if ((aiPointKey.length < 2) || (aiPointKey.length != akPointPosition.length)) {
            throw new IllegalArgumentException("FlyPathGraphSamples.add: arrays must be same positive size.");
        }

        // Determine where this branch occurs relative to an already
        // existing branch.
        int iBranchBranch = -1;
        float fNormalizedBranchDist = 0.0f;

        if (getNumBranches() > 0) {

            // Take the key value from the first point in the input branch.
            // Search through all of the branches to find the first one
            // where this branch is to be attached.
            int iBranchKey = aiPointKey[0];
            int iBranchPoint = -1;
            Vector3f[] akBranchPosition = null;

LoopBranchSearch:
            for (int iBranch = 0; iBranch < getNumBranches(); iBranch++) {
                int[] aiBranchKey = m_kListArrayPointKey.get(iBranch);

                for (int iPoint = 0; iPoint < aiBranchKey.length; ++iPoint) {

                    if (iBranchKey == aiBranchKey[iPoint]) {
                        iBranchBranch = iBranch;
                        iBranchPoint = iPoint;
                        akBranchPosition = m_kListArrayPointPosition.get(iBranch);

                        break LoopBranchSearch;
                    }
                }
            }

            // All arrays must be valid.
            if (-1 == iBranchBranch) {
                throw new IllegalArgumentException("FlyPathGraphSamples.add: common branch point not found");
            }

            // Compute the relative distance along the curve in the range
            // [0,1] where the branch point was found.
            float fBranchLength = 0.0f;
            float fBranchPointDist = 0.0f;

            for (int iPoint = 1; iPoint < akBranchPosition.length; iPoint++) {
                float fLength = akBranchPosition[iPoint].Distance(akBranchPosition[iPoint - 1]);
                fBranchLength += fLength;

                if (iPoint <= iBranchPoint) {
                    fBranchPointDist += fLength;
                }
            }

            fNormalizedBranchDist = fBranchPointDist / fBranchLength;
        }

        add(iBranchBranch, fNormalizedBranchDist);

        // Add arrays to corresponding list of arrays.
        m_kListArrayPointKey.add(aiPointKey);
        m_kListArrayPointPosition.add(akPointPosition);
    }

    /**
     * Access the array of 3D coordinates for the points which define the specified branch.
     *
     * @param   iBranch  int Index which identifies the branch.
     *
     * @return  Point3f[] Array of 3D coordinates for the points.
     */
    public Vector3f[] getArrayPointPosition(int iBranch) {
        return m_kListArrayPointPosition.get(iBranch);
    }
}
