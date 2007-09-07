package gov.nih.mipav.view.renderer.surfaceview.flythruview;


import gov.nih.mipav.model.structures.*;

import java.util.*;

import javax.vecmath.*;


/**
 * This class is an implementation of a bidirectional graph of nodes where each node is a curve segment used to define
 * branched paths. Each curve is represented by a Curve3 instance.
 */
public class FlyPathGraphCurve extends FlyPathGraph {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Storage for the arrays of position for each curve. The size of these arrays must match and the entries must
     * correspond for the same index. The size of these arrays must also match the size of the arrays in the base class,
     * and must correspond for the same index.
     */
    protected ArrayList m_kListCurvePosition; // array of Curve3

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct and empty list of curves.
     */
    public FlyPathGraphCurve() { }

    /**
     * Create a new graph with the same structure as this graph but with each node having a BSpline approximation for
     * each of the points (and boundary distances) for each node.
     *
     * @param   kGraphSamples              FlyPathGraphSamples Instance of graph of samples to be replicated but with
     *                                     BSpline approximated curves.
     * @param   fFractionNumControlPoints  float This is the value in range [0,1] which represents how many control
     *                                     points to select for the curve fit of the path, as a fraction of the number
     *                                     of the total number of points along the path.
     * @param   iDegree                    int BSpline degree to use for all approximated curves.
     *
     * @return  FlyPathGraphCurve New instance of the graph with the BSpline approximated curves.
     */
    public FlyPathGraphCurve(FlyPathGraphSamples kGraphSamples, float fFractionNumControlPoints, int iDegree) {

        // Copy the graph structure.
        super(kGraphSamples);

        // Allocate storage for curves
        m_kListCurvePosition = new ArrayList();

        // Iterate over each curve in the input graph.
        int iNumBranches = kGraphSamples.getNumBranches();

        for (int iBranch = 0; iBranch < iNumBranches; iBranch++) {

            // Access the array of point positions for the branch.
            Point3f[] akPointPosition = kGraphSamples.getArrayPointPosition(iBranch);

            // The number of BSpline control points is a fraction
            // of the total number of path samples.
            // Use the same number of control points and the same degree
            // for all curve fits generated here.
            // Make sure the minimum number of control points are used
            // given the degree of the BSpline.
            int iNumControlPoints = (int) Math.ceil(akPointPosition.length * fFractionNumControlPoints);
            iNumControlPoints = Math.max(iNumControlPoints, BSplineBasisf.getMinNumControlPoints(iDegree));

            // Create each curve approximation and add it to the graph.
            m_kListCurvePosition.add(BSplineCurve3.createApproximation(akPointPosition, iNumControlPoints, iDegree));
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Access the entire array of curves which define the positions of points along the path in 3D that is associated
     * with this node.
     *
     * @return  Curve3[] Array of parameterized curves for 3D positions.
     */
    public Curve3[] getArrayCurvePosition() {
        Curve3[] akCurve = new Curve3[getNumBranches()];

        return (Curve3[]) m_kListCurvePosition.toArray(akCurve);
    }

    /**
     * Access the curve which defines the positions of points along the path in 3D that is associated with this node.
     *
     * @param   iIndex  int Index into the list of stored curves.
     *
     * @return  Curve3 Parameterized curve for 3D positions.
     */
    public Curve3 getCurvePosition(int iIndex) {
        return (Curve3) m_kListCurvePosition.get(iIndex);
    }
}
