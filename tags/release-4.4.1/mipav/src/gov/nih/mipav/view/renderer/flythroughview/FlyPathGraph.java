package gov.nih.mipav.view.renderer.flythroughview;


import java.util.*;


/**
 * Basic graph structure which identifies relationship between nodes by their indexes.
 */
public class FlyPathGraph {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Each item is a TreeMap instance which stores all of the child branches for a given branch. The key value is a
     * Float which stores the normalized path distance (in range [0,1]) which is where a branch (or more than one
     * branch) starts. The keys are sortable so that the branch points can be fined in order. Each value is a Set
     * instance which contains Integer's which identify the unique index of the child branch. array of TreeMap
     * (key=Float(fNormalizedPathDist)), value=Set(Integer(iChildBranch)))
     */
    protected ArrayList<TreeMap<Float,Set<Integer>>> m_kListBranchChildList;

    /**
     * Each item is an Integer which contains the index of the parent branch to which this branch is connected. A parent
     * branch of -1 is used to indicate there is no parent branch, i.e., the root branch. This ArrayList has the same
     * number of entries as the m_kListBranchParentNormalizedDist ArrayList and the entries correspond.
     */
    protected ArrayList<Integer> m_kListBranchParentIndex; // array of Integer

    /**
     * Each item in a Float which contains the normalized path distance (in range [0,1]) for the parent branch where
     * this branch started. If there is no parent branch, this value is considered undefined. This ArrayList has the
     * same number of entries as the m_kListBranchParentIndex ArrayList and the entries correspond.
     */
    protected ArrayList<Float> m_kListBranchParentNormalizedDist; // array of Float

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     */
    protected FlyPathGraph() {
        m_kListBranchParentIndex = new ArrayList<Integer>();
        m_kListBranchParentNormalizedDist = new ArrayList<Float>();
        m_kListBranchChildList = new ArrayList<TreeMap<Float,Set<Integer>>>();
    }

    /**
     * Copy constructor.
     *
     * @param  that  FlyPathGraph Instance to be duplicated.
     */
    protected FlyPathGraph(FlyPathGraph that) {
        this.m_kListBranchParentIndex = (ArrayList<Integer>) that.m_kListBranchParentIndex.clone();

        this.m_kListBranchParentNormalizedDist = (ArrayList) that.m_kListBranchParentNormalizedDist.clone();

        this.m_kListBranchChildList = (ArrayList) that.m_kListBranchChildList.clone();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return the index of the parent branch for the specified branch. See also getBranchParentNormalizedDist() method.
     *
     * @param   iBranch  int Index which specifies the branch for returning its parent branch index.
     *
     * @return  int Index of the parent branch. A value of -1 is returned if the specified branch has no parent.
     */
    public int getBranchParentIndex(int iBranch) {
        return m_kListBranchParentIndex.get(iBranch).intValue();
    }

    /**
     * Return the normalized distance (in range [0,1]) along the parent branch where the specified branch starts. See
     * also getBranchParentIndex() method.
     *
     * @param   iBranch  int Index which specifies the branch for returning the normalized distance along the parent
     *                   branch where this branch starts.
     *
     * @return  float Normalized distance (in range [0,1]) along parent branch where the specified branch starts. A
     *          value of 0.0f is returned if the specified branch has no parent.
     */
    public float getBranchParentNormalizedDist(int iBranch) {
        return m_kListBranchParentNormalizedDist.get(iBranch).floatValue();
    }

    /**
     * Return the array of branch indexes for all of the child branches which start at the specified point along the
     * specified parent branch.
     *
     * @param   iBranch       int Index which specifies the branch for which its child branches are to be returned.
     * @param   iBranchPoint  int Index of branch point along the specified branch. The vvalid range for this index is
     *                        determined by array returned by getBranchPoints() method for same input branch.
     *
     * @return  int[] Array of branch indexes which identify the child branches for the specified branch point. This
     *          array have one entry for each branch which starts at the specified branch point. The order of the child
     *          branches referenced by this array is the order in which the branches were found which is by order of
     *          longest to shortest.
     */
    public int[] getBranchPointBranches(int iBranch, int iBranchPoint) {
        Map.Entry[] akBranchPointEntry = getBranchPointEntryArray(iBranch);
        Set<Integer> kChildBranchSet = (Set<Integer>) akBranchPointEntry[iBranchPoint].getValue();
        Integer[] akChildBranch = new Integer[kChildBranchSet.size()];
        int[] aiChildBranch = new int[kChildBranchSet.size()];
        kChildBranchSet.toArray(akChildBranch);

        for (int i = 0; i < akChildBranch.length; i++) {
            aiChildBranch[i] = akChildBranch[i].intValue();
        }

        akChildBranch = null;
        kChildBranchSet = null;
        akBranchPointEntry = null;

        return aiChildBranch;
    }

    /**
     * Return array of relative path distances (in range [0,1]) where branch points occur along the specified input
     * branch.
     *
     * @param   iBranch  int Index which specifies the branch for returning its branch points.
     *
     * @return  float[] Array of relative path distances, where each value is in the range [0,1] and the values in the
     *          array are increasing (i.e., no duplicates). The number of entries in the array is the number of branch
     *          points, where each branch point may have more than one branch.
     */
    public float[] getBranchPoints(int iBranch) {
        Map.Entry[] akBranchPointEntry = getBranchPointEntryArray(iBranch);
        float[] afBranchPointDist = new float[akBranchPointEntry.length];

        for (int i = 0; i < akBranchPointEntry.length; i++) {
            Float kDist = (Float) akBranchPointEntry[i].getKey();
            afBranchPointDist[i] = kDist.floatValue();
        }

        akBranchPointEntry = null;

        return afBranchPointDist;
    }

    /**
     * Return the number of branches stored for this path graph. A branch is actually a node in the graph.
     *
     * @return  int Number of path graph branches.
     */
    public int getNumBranches() {

        // The size of the array for m_kListBranchParentIndex must be the
        // same, so the size of either could be returned.
        return m_kListBranchChildList.size();
    }

    /**
     * Add a branch to the already existing branch at the specified distance along the branch.
     *
     * @param  iBranchParent        int Index of already existing branch which the new branch will become connected to.
     *                              Negative value to indicate that the branch has no parent.
     * @param  fNormalizedPathDist  float Proportional distance along a polyline path (relative to total path length)
     *                              where the branch point occurs. Value is ignored if the branch has no parent.
     */
    protected void add(int iBranchParent, float fNormalizedPathDist) {

        // Get index of this new branch.
        int iNewBranch = getNumBranches();

        // Store parent for this new branch.
        m_kListBranchParentIndex.add(new Integer(iBranchParent));
        m_kListBranchParentNormalizedDist.add(new Float(fNormalizedPathDist));

        // Store branch as child of its parent, if it has a parent.
        if (iBranchParent >= 0) {
            TreeMap<Float,Set<Integer>> kBranchChildList = m_kListBranchChildList.get(iBranchParent);
            Float kKey = new Float(fNormalizedPathDist);
            Set<Integer> kChildSet = kBranchChildList.get(kKey);

            if (null != kChildSet) {
                kChildSet.add(new Integer(iNewBranch));
            } else {
                kChildSet = new HashSet<Integer>();
                kChildSet.add(new Integer(iNewBranch));
                kBranchChildList.put(kKey, kChildSet);
            }
        }

        m_kListBranchChildList.add(new TreeMap<Float,Set<Integer>>());
    }

    /**
     * Return the array of TreeMap entries which stores all of the child branch information for the specified input
     * branch.
     *
     * @param   iBranch  int Index of branch for which its child branch information is to be returned.
     *
     * @return  Map.Entry[] Array of TreeMap Entries where key is a Float for the relative path distance of each branch
     *          point, and value is a Set of the Integer for the index of each child branch starting at that branch
     *          point.
     */
    protected Map.Entry[] getBranchPointEntryArray(int iBranch) {
        TreeMap<Float, Set<Integer>> kTreeMap = m_kListBranchChildList.get(iBranch);
        Set kEntrySet = kTreeMap.entrySet();
        Map.Entry[] akMapEntry = new Map.Entry[kEntrySet.size()];
        kEntrySet.toArray(akMapEntry);

        kEntrySet = null;
        kTreeMap = null;

        return akMapEntry;
    }
}
