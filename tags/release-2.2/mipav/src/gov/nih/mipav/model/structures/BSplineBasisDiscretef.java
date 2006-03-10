 package gov.nih.mipav.model.structures;


 /**
  * This class is an implementation of an open uniform B-Spline with
  * equally spaced samples taken over the input [0,1] interval.  The
  * basis function evaluations are precomputed at each sample and for
  * each control point so that getting the position for an input value
  * is simply a table lookup and does not involve any calculations.
  */
 public class BSplineBasisDiscretef
     extends BSplineBasisf {

     /**
      * Create a set of B-Spline uniform open basis functions for the
      * given number of control points and degree of the basis functions.
      * These basis functions have been sampled equally over the [0,1]
      * input interval.
      * @param iNumControlPoints int Number of control points.
      * @param iDegree int B-Spline degree of basis functions.
      * @param iNumSamples int Number of equally spaced samples.
      */
     public BSplineBasisDiscretef(int iNumCtrlPoints, int iDegree, int iNumSamples) {
         super(iNumCtrlPoints, iDegree);

         // Check preconditions.
         if (!(iNumSamples > iNumCtrlPoints)) {
             throw new IllegalArgumentException("BSplineBasisDiscretef: Number of samples must be more than number of control points.");
         }

         m_iNumSamples = iNumSamples;

         // Precompute the basis functions at each sample and store result
         // in 2D array for quicker computations later.  In the process,
         // determine the range of sample indexes each control point affects.
         m_aafSampleD0 = new float[iNumSamples][iNumCtrlPoints];
         m_aafSampleD1 = new float[iNumSamples][iNumCtrlPoints];
         m_aafSampleD2 = new float[iNumSamples][iNumCtrlPoints];
         m_aiControlPointSampleIndexMin = new int[iNumSamples];
         m_aiControlPointSampleIndexMax = new int[iNumSamples];
         for (int i = 0; i < iNumCtrlPoints; i++) {
             m_aiControlPointSampleIndexMin[i] = iNumSamples - 1;
             m_aiControlPointSampleIndexMax[i] = 0;
         }
         for (int i = 0; i < iNumSamples; i++) {
             float fU = (float) (i) / (float) (iNumSamples - 1);
             int iMaxU = compute(fU, m_aafSampleD0[i], m_aafSampleD1[i], m_aafSampleD2[i]);
             int iMinU = iMaxU - m_iDegree;

             for (int iControl = iMinU; iControl <= iMaxU; iControl++) {
                 if (i < m_aiControlPointSampleIndexMin[iControl]) {
                     m_aiControlPointSampleIndexMin[iControl] = i;
                 }
                 if (i > m_aiControlPointSampleIndexMax[iControl]) {
                     m_aiControlPointSampleIndexMax[iControl] = i;
                 }
             }
         }
     }

     /**
      * Cleanup memory
      * @throws Throwable
      */
     public void finalize() throws Throwable  {

         Deallocate2D(m_aafSampleD0);
         m_aafSampleD0 = null;

         Deallocate2D(m_aafSampleD1);
         m_aafSampleD1 = null;

         Deallocate2D(m_aafSampleD2);
         m_aafSampleD2 = null;

         m_aiControlPointSampleIndexMin = null;
         m_aiControlPointSampleIndexMax = null;

         super.finalize();
     }

     /**
      * Return the number of samples specified in the constructor.
      * @return int Number of samples.
      */
     public int getNumSamples() {
         return m_iNumSamples;
     }

     /**
      * Return the minimum index of the range of samples affected by
      * the specified control point.
      * @param iControl int Index of a specified control point.
      * @return int Minimum index of affected sample range.
      */
     public int getControlPointSampleIndexMin(int iControl) {
         return m_aiControlPointSampleIndexMin[iControl];
     }

     /**
      * Return the maximum index of the range of samples affected by
      * the specified control point.
      * @param iControl int Index of a specified control point.
      * @return int Maximum index of affected sample range.
      */
     public int getControlPointSampleIndexMax(int iControl) {
         return m_aiControlPointSampleIndexMax[iControl];
     }

     /**
      * Return the position basis function evaluation which determines
      * how much the specified control point affects the specified sample.
      * @param iControl int Index of a specified control point.
      * @param iSample int Index of a specified sample.
      * @return float Associated position basis function evaluation.
      */
     public float getD0(int iControl, int iSample) {
         return m_aafSampleD0[iSample][iControl];
     }

     /**
      * Return the first derivative basis function evaluation which determines
      * how much the specified control point affects the specified sample.
      * @param iControl int Index of a specified control point.
      * @param iSample int Index of a specified sample.
      * @return float Associated first derivative basis function evaluation.
      */
     public float getD1(int iControl, int iSample) {
         return m_aafSampleD1[iSample][iControl];
     }

     /**
      * Return the second derivative basis function evaluation which determines
      * how much the specified control point affects the specified sample.
      * @param iControl int Index of a specified control point.
      * @param iSample int Index of a specified sample.
      * @return float Associated second derivative basis function evaluation.
      */
     public float getD2(int iControl, int iSample) {
         return m_aafSampleD2[iSample][iControl];
     }


     protected int m_iNumSamples;// over [0,1] range
     protected float[][] m_aafSampleD0;// [iNumSamples][iNumCtrlPoints]
     protected float[][] m_aafSampleD1;// [iNumSamples][iNumCtrlPoints]
     protected float[][] m_aafSampleD2;// [iNumSamples][iNumCtrlPoints]
     protected int[] m_aiControlPointSampleIndexMin;	// [iNumCtrlPoints]
     protected int[] m_aiControlPointSampleIndexMax;	// [iNumCtrlPoints]
 }

