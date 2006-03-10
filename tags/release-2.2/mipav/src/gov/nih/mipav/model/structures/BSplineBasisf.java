package gov.nih.mipav.model.structures;

 /**
  * This class is an implementation of an open uniform B-Spline given
  * a specified number of control points and a degree for the basis
  * functions.  The knot array is interally generated with equally
  * spaced elements.
  */
 public class BSplineBasisf {

     /**
      * Create a set of B-Spline uniform open basis functions for the
      * given number of control points and degree of the basis functions.
      * Initially, the knot vectors are uniformly spaced although
      * they can be modified.
      * @param iNumControlPoints int Number of control points.
      * @param iDegree int B-Spline degree of basis functions.
      */
     public BSplineBasisf (int iNumControlPoints, int iDegree) {

         // Check preconditions.
         if (!(iNumControlPoints >= 2)) {
             throw new IllegalArgumentException("BSplineBasisf: Number of control points must be >= 2.");
         }
         if (!(1 <= iDegree && iDegree < iNumControlPoints)) {
             throw new IllegalArgumentException("BSplineBasisf: Degree must be at least 1 and less than number of control points.");
         }

         m_iNumControlPoints = iNumControlPoints;
         m_iDegree = iDegree;
         m_iNumKnots = m_iNumControlPoints + m_iDegree + 1;

         // Allocate 2D array to store D0 and D1 basis function computations.
         // The entries in [m_iDegree][*] are the final results and
         // all other entries are intermediate results.  The entries
         // for [m_iDegree][*] are not allocated now, but are specified
         // in the call to the ComputeD0 method.
         m_aafBD0 = new float[m_iDegree + 1][];
         m_aafBD1 = new float[m_iDegree + 1][];
         m_aafBD2 = new float[m_iDegree + 1][];
         for (int i = 0; i < m_iDegree; i++) {
             m_aafBD0[i] = new float[m_iNumKnots - 1];
             m_aafBD1[i] = new float[m_iNumKnots - 1];
             m_aafBD2[i] = new float[m_iNumKnots - 1];
         }

         // Create the uniformly spaced knots.
         int k;
         float fFactor = 1.0f / (m_iNumControlPoints - m_iDegree);
         m_afKnot = new float[m_iNumKnots];
         for (k = 0; k <= m_iDegree; k++) {
             m_afKnot[k] = 0.0f;
         }
         for ( /**/; k < m_iNumControlPoints; k++) {
             m_afKnot[k] = (k - m_iDegree) * fFactor;
         }
         for ( /**/; k < m_iNumKnots; k++) {
             m_afKnot[k] = 1.0f;
         }
     }

     /**
      * Cleanup memory
      * @throws Throwable
      */
     public void finalize() throws Throwable  {
         m_afKnot = null;

         Deallocate2D(m_aafBD0);
         m_aafBD0 = null;

         Deallocate2D(m_aafBD1);
         m_aafBD1 = null;

         Deallocate2D(m_aafBD2);
         m_aafBD2 = null;

         super.finalize();
     }

     /**
      * Called by finalize to deallocate memory for a 2D array of data.
      * @param aafData float[][] Reference to 2D array of data.
      */
     protected void Deallocate2D (float[][] aafData) {
         if (null != aafData) {
             for (int i = 0; i < aafData.length; i++) {
                 aafData[i] = null;
             }
         }
     }

     /**
      * Given a BSpline degree, return the minimum number of control points
      * that is required.
      * @param iDegree int Input degree of the BSpline.
      * @return int Minimum number of control points required.
      */
     static public int getMinNumControlPoints (int iDegree) {
         int iMinNumControlPoints = iDegree+1;
         if (iMinNumControlPoints < 2) {
             iMinNumControlPoints = 2;
         }
         return iMinNumControlPoints;
     }

     /**
      * Return indication of whether two BSpline basis instances are the same.
      * @param that BSplineBasisf BSpline basis instance to check for
      * similarity.
      * @return boolean True if the two BSpline basis instances represent
      * the same set of functions.
      */
     public boolean isSameAs (BSplineBasisf that) {
         return
             (this.m_iDegree == that.m_iDegree) &&
             (this.m_iNumControlPoints == that.m_iNumControlPoints);
     }

     /**
      * Return number of knots which depends on the number of control points
      * and the degree of the basis functions.
      * @return int Number of knots.
      */
     public int getNumKnots() {
         return m_iNumKnots;
     }

     /**
      * Return the number of control points specified in the constructor.
      * @return int Number of control points.
      */
     public int getNumControlPoints() {
         return m_iNumControlPoints;
     }

     /**
      * Return the degree of the basis functions specified in the constructor.
      * @return int
      */
     public int getDegree() {
         return m_iDegree;
     }

     /**
      * Return the knot value at the specified control point interval index.
      * The entire set of knot values cannot be accessed since
      * - the knot values in the [0,degree] range are fixed at zero.
      * - the knot valeus in the [numControl,numKnots) range are fixed at one.
      * @param iControlInterval int Must be an index in the [0,numControl-1)
      * range.  Used to specify the relative spacing between control points at
      * iControlInterval and iControlInterval+1.
      * @return float Knot value at the specified control point interval index.
      */
     public float getKnot(int iControlInterval) {
         return m_afKnot[iControlInterval + m_iDegree + 1];
     }

     /**
      * Set the knot value at the specified control point interval index.
      * @param iControlInterval int Must be an index in the [0,numControl-1)
      * range.  Used to specify the relative spacing between control points at
      * iControlInterval and iControlInterval+1.
      * @param fValue float Knot value to set at the specified control point
      * interval index.  Note that the knot values must not be decreasing.
      */
     public void setKnot(int iControlInterval, float fValue) {
         int iKnot = iControlInterval + m_iDegree + 1;
         if (!( (m_iDegree + 1) <= iKnot && iKnot <= (m_iNumControlPoints - 1))) {
             throw new IllegalArgumentException("BSplineBasisf::setKnot: Specified knot cannot be modified.");
         }
         if (fValue < 0.0f || fValue > 1.0f) {
             throw new IllegalArgumentException("BSplineBasisf::setKnot: Specified knot value must be in [0,1] range.");
         }
         m_afKnot[iKnot] = fValue;
     }

     /**
      * Set the knot values for all of the control point intervals.
      * @param afKnotValues float[] Must be an array with at least
      * numControl-1 elements.  The values in this array should be
      * in the range of [0,1] and must not be decreasing.
      */
     public void setKnots(float[] afKnotValues) {
         for (int iControl = 1; iControl < m_iNumControlPoints; iControl++) {
             setKnot(iControl - 1, afKnotValues[iControl - 1]);
         }
     }

     /**
      * Return the knot index i for which knot[i] <= fU < knot[i+1].
      * @param fU float Input value in the [0,1] range.
      * @return int Knot index in the range [0,getNumKnots()-1].
      */
     public int getKnotIndex(float fU) {
         if (fU <= 0.0f) {
             return m_iDegree;
         }
         else if (fU >= 1.0f) {
             return m_iNumControlPoints - 1;
         }
         else {
             int iKnot = m_iDegree+1;
             while ((iKnot <= m_iNumControlPoints) && (fU >= m_afKnot[iKnot])) {
                 iKnot++;
             }
             return iKnot-1;
         }
     }

     /**
      * Evaluate basis functions for the given input and return
      * the knot index i for which knot[i] <= fU < knot[i+1].
      * It is then that indexes i-degree <= k <= i have non-zero
      * basis evaluations.
      * @param fU float Input value in the [0,1] range.
      * @param afBD0 float[] Array to be filled with position basis evaluation
      * at each of the knot values for the given fU input.  This array must
      * have enough elements to store a value for each control point.
      * This array must not be null.
      * @param afBD1 float[] Array to be filled with first derivative basis
      * evaluation at each of the knot values for the given fU input.  This
      * array may be null meaning that the first derivative basis evaluation
      * is not computed, but if the array is defined it must have enough
      * elements to store a value for each control point.  If this array is
      * null, then the afBD2 array must also be null.
      * @param afBD2 float[] Array to be filled with second derivative basis
      * evaluation at each of the knot values for the given fU input.  This
      * array may be null meaning that the second derivative basis evaluation
      * is not computed, but if the array is defined it must have enough
      * elements to store a value for each control point.  If this array
      * is not null, then the afBD1 array must not be null.
      * @return int The knot index i for which knot[i] <= fU < knot[i+1].
      */
     public int compute (float fU, float[] afBD0, float[] afBD1, float[] afBD2)
     {
         // Find the knot index i for which knot[i] <= rfTime < knot[i+1]
         // where open splines clamp to [0,1]
         if (fU <= 0.0f) {
             fU = 0.0f;
         }
         else if (fU >= 1.0f) {
             fU = 1.0f;
         }
         int i = getKnotIndex(fU);
         float fN0 = fU - m_afKnot[i];
         float fN1 = m_afKnot[i + 1] - fU;

         // The array for D0 must always be specified.
         // This array is hooked in as the last row in m_aafBD0
         // so that it ends up with the final computed evaulations.
         int iOrder = 0;
         if (null == afBD0) {
             throw new IllegalArgumentException("BSplineBasisf::compute: afBD0 array is null.");
         }
         for (int iIndex = 0; iIndex < afBD0.length; iIndex++) {
             afBD0[iIndex] = 0.0f;
         }
         m_aafBD0[m_iDegree] = afBD0;
         m_aafBD0[0][i] = 1.0f;

         // Do we compute first derivatives?
         // This array is hooked in as the last row in m_aafBD1
         // so that it ends up with the final computed evaulations.
         if (null != afBD1) {
             iOrder = 1;
             for (int iIndex = 0; iIndex < afBD1.length; iIndex++) {
                 afBD1[iIndex] = 0.0f;
             }
             m_aafBD1[m_iDegree] = afBD1;
             m_aafBD1[0][i] = 0.0f;

             // Do we compute second derivatives?
             // This array is hooked in as the last row in m_aafBD2
             // so that it ends up with the final computed evaluations.
             if (null != afBD2) {
                 iOrder = 2;
                 for (int iIndex = 0; iIndex < afBD2.length; iIndex++) {
                     afBD2[iIndex] = 0.0f;
                 }
                 m_aafBD2[m_iDegree] = afBD2;
             }
         }


         for (int j = 1; j <= m_iDegree; j++) {
             float fInvD0 = 1.0f / (m_afKnot[i + j] - m_afKnot[i]);
             float fInvD1 = 1.0f / (m_afKnot[i + 1] - m_afKnot[i - j + 1]);

             m_aafBD0[j][i] = fN0 * m_aafBD0[j - 1][i] * fInvD0;
             m_aafBD0[j][i - j] = fN1 * m_aafBD0[j - 1][i - j + 1] * fInvD1;

             if (iOrder >= 1) {
                 m_aafBD1[j][i] = (fN0*m_aafBD1[j-1][i]+m_aafBD0[j-1][i])*fInvD0;
                 m_aafBD1[j][i-j] = (fN1*m_aafBD1[j-1][i-j+1]-m_aafBD0[j-1][i-j+1])
                     *fInvD1;

                 if (iOrder >= 2)
                 {
                     m_aafBD2[j][i] =
                         (fN0 * m_aafBD2[j - 1][i] +
                          2.0f * m_aafBD1[j - 1][i]) * fInvD0;
                     m_aafBD2[j][i - j] =
                         (fN1 * m_aafBD2[j - 1][i - j + 1] -
                          2.0f * m_aafBD1[j - 1][i - j + 1]) * fInvD1;
                 }
             }
         }

         for (int j = 2; j <= m_iDegree; j++) {
             for (int k = i - j + 1; k < i; k++) {
                 fN0 = fU - m_afKnot[k];
                 fN1 = m_afKnot[k + j + 1] - fU;
                 float fInvD0 = 1.0f / (m_afKnot[k + j] - m_afKnot[k]);
                 float fInvD1 = 1.0f / (m_afKnot[k + j + 1] - m_afKnot[k + 1]);

                 m_aafBD0[j][k] =
                     fN0 * m_aafBD0[j - 1][k] * fInvD0 +
                     fN1 * m_aafBD0[j - 1][k + 1] * fInvD1;

                 if (iOrder >= 1) {
                     m_aafBD1[j][k] =
                         (fN0*m_aafBD1[j-1][k]+m_aafBD0[j-1][k])*fInvD0 +
                         (fN1*m_aafBD1[j-1][k+1]-m_aafBD0[j-1][k+1])*fInvD1;

                     if (iOrder >= 2)
                     {
                         m_aafBD2[j][k] =
                             (fN0 * m_aafBD2[j - 1][k] +
                              2.0f * m_aafBD1[j - 1][k]) * fInvD0 +
                             (fN1 * m_aafBD2[j - 1][k + 1] -
                              2.0f * m_aafBD1[j - 1][k + 1]) * fInvD1;
                     }
                 }
             }
         }

         return i;
     }

     protected int m_iNumControlPoints; // n+1
     protected int m_iDegree;           // d
     protected int m_iNumKnots;         // n+d+2
     protected float[] m_afKnot;        // knot[n+d+2]

     // Storage for the basis functions.  The basis array is always
     // allocated by the constructor calls.
     protected float[][] m_aafBD0;             // bd0[d+1][n+d+1]
     protected float[][] m_aafBD1;             // bd1[d+1][n+d+1]
     protected float[][] m_aafBD2;             // bd2[d+1][n+d+1]
 }
