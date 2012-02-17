package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.view.*;


/**
 * This algorithm calculates the arc-length of a Bspline fit to user defined control points. This algorithm calculates
 * the entire curve length and also is able to calculate the arc-length between two points.
 *
 * @version  0.1 April 27, 1998
 */
public class AlgorithmArcLength extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int INTEGRAL_ORDER = 10;

    /** DOCUMENT ME! */
    public static final int INVLEN_MAXITER = 200;

    /** DOCUMENT ME! */
    public static final float INVLEN_TOLERANCE = (float) 0.0025;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmBSpline bSpline = new AlgorithmBSpline();

    /** DOCUMENT ME! */
    private float[][] rom = new float[INTEGRAL_ORDER][INTEGRAL_ORDER];

    /** DOCUMENT ME! */
    private float totalLen;

    /** DOCUMENT ME! */
    private float[] xPoints;

    /** DOCUMENT ME! */
    private float[] yPoints;
    
    /** DOCUMENT ME! */
    private float[] zPoints = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Algorithm constructor.
     *
     * @param  xPts  x coordinate control points
     * @param  yPts  y coordinate control points
     */
    public AlgorithmArcLength(float[] xPts, float[] yPts) {
        setPoints(xPts, yPts);
    }
    
    /**
     * Algorithm constructor.
     *
     * @param  xPts  x coordinate control points
     * @param  yPts  y coordinate control points
     */
    public AlgorithmArcLength(float[] xPts, float[] yPts, float[] zPts) {
        setPoints(xPts, yPts, zPts);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns total arc-length of the segment.
     *
     * @return  DOCUMENT ME!
     */
    public float getTotalArcLength() {
        return totalLen;
    }

    /**
     * Finds the point at some percentage along the curve using simple midpoint root finding algorithm.
     *
     * @param   pct  percentage indicating where one would
     *
     * @return  position indicates
     */

    public float invlen(float pct) {
        int i;
        int nPts;
        float lenQ, difference;
        float q;
        float top;
        float bottom = 2;

        if ((pct < 0.0) || (pct > 1.0)) {
            return (-1); // p must be >= 0 and p <= 1
        }

        nPts = xPoints.length - 4;

        q = (pct * (nPts - 1)) + 2;
        top = (nPts - 1) + 2;

        for (i = 0; i < INVLEN_MAXITER; i++) {
            lenQ = length(2, q);
            difference = (lenQ / totalLen) - pct;

            if (Math.abs(difference) < INVLEN_TOLERANCE) {
                return q;
            } else if (difference < 0) {
                bottom = q;
                q = q + ((top - q) / 2);
            } else {
                top = q;
                q = q - ((q - bottom) / 2);
            }
        }

        displayError("exceeded maximum iterations in method AlgorithmArcLength:invlen");

        return (-1);
    }

    /**
     * Finds the point at some percentage along the curve using Newtons method of root finding.
     *
     * @param   pct  percentage indicating where one would
     *
     * @return  position indicates
     */
    public float invlenNewton(float pct) {
        int i;
        int nPts;
        float lenQ;
        float diff, oldDiff;
        float q, qn;
        float oldQ;
        float f, fx;

        if ((pct < 0.0) || (pct > 1.0)) {
            return (-1); // p must be >= 0 and p <= 1
        }

        nPts = xPoints.length - 4;

        q = (pct * (nPts - 1)) + 2;
        oldQ = (nPts - 1) + 2;
        oldDiff = (float) 1.0;

        for (i = 0; i < INVLEN_MAXITER; i++) {
            lenQ = length(2, q);
            diff = (lenQ / totalLen) - pct;

            f = diff;
            fx = (oldDiff - diff) / ((oldQ - q) / ((nPts - 1) + 2));

            if (Math.abs(diff) < INVLEN_TOLERANCE) {
                return q;
            } else if (diff > 0) {
                qn = (q / ((nPts - 1) + 2)) - Math.abs(f / fx);

            } else {
                qn = (q / ((nPts - 1) + 2)) + Math.abs(f / fx);
            }

            oldDiff = diff;
            oldQ = q;
            q = qn * ((nPts - 1) + 2);

            if (q < 2) {
                q = 2;
            }

            if (q > ((nPts - 1) + 2)) {
                q = nPts - 1 + 2;
            }
        }

        Preferences.debug("exceeded maximum iterations in method 'invlen'\n", Preferences.DEBUG_ALGORITHM);

        return (-1);
    }


    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() { }


    /**
     * Sets the controls points of the Bspline and calculates the total arc-length.
     *
     * @param  xPts  x coordinate control points
     * @param  yPts  y coordinate control points
     */
    public void setPoints(float[] xPts, float[] yPts) {
        setPoints(xPts,yPts,null);
    }


    /**
     * Sets the controls points of the Bspline and calculates the total arc-length.
     *
     * @param  xPts  x coordinate control points
     * @param  yPts  y coordinate control points
     */
    public void setPoints(float[] xPts, float[] yPts, float[] zPts) {

        xPoints = xPts;
        yPoints = yPts;
        zPoints = zPts;

        totalLen = length(2, yPoints.length - 3);
    }


    /**
     * The length is calculated between to points a,b using Romberg integration.
     *
     * @param   a  starting position
     * @param   b  ending position b > a and b range
     *
     * @return  length returns arc-length of the segment
     */
    private float length(float a, float b) {
        int i, j, k;
        int ipower, kpower;
        float sum;
        float h = b - a;
        float result;

        if (b == a) {
            return (float) 0.0;
        }

        rom[0][0] = h * (speed(a) + speed(b)) / 2;

        for (i = 2, ipower = 1; i <= INTEGRAL_ORDER; i++, ipower *= 2, h /= 2) {
            sum = 0;

            for (j = 1; j <= ipower; j++) {
                sum += speed(a + (h * (j - (float) 0.5)));
            }

            rom[1][0] = (rom[0][0] + (h * sum)) / 2;

            for (k = 1, kpower = 4; k < i; k++, kpower *= 4) {
                rom[1][k] = ((kpower * rom[1][k - 1]) - rom[0][k - 1]) / (kpower - 1);
            }

            for (j = 0; j < i; j++) {
                rom[0][j] = rom[1][j];
            }
        }

        result = rom[0][INTEGRAL_ORDER - 1];

        return result;
    }


    /**
     * Calculates the speed at some point q.
     *
     * @param   q  the point where the speed is calculated
     *
     * @return  speed the speed ( magnitude ) is returned
     */
    private float speed(float q) {
        float speed;
        
        if ( zPoints == null )
        {
            Vector2f pt = bSpline.bSplineJetXY(1, q, xPoints, yPoints); // calc 1st deriv.
            speed = (float) Math.sqrt((pt.X * pt.X) + (pt.Y * pt.Y));
        }
        else
        {
            Vector3f pt = bSpline.bSplineJetXYZ(1, q, xPoints, yPoints, zPoints); // calc 1st deriv.
            speed = pt.Length();            
        }
        return speed;
    }
}
