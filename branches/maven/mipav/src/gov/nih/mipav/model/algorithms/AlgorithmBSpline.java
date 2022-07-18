package gov.nih.mipav.model.algorithms;

import javax.vecmath.Vector3d;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * 
 * 	
 * Modified code from Aaron Carass's Java implementation of
 * Philippe Thevenaz's Cubic B-spline Interpolation. Code is
 * simply wrapped to allow for calls consistent with MIPAV
 * style as well as allow for 2D images. 
 * 
 * B-Spline interpolation.
 * 
 * @author Philippe Thevenaz
 * @author Aaron Carass
 * 
 * Jet code is untouched from when the new code was implemented on June 24th, 2014.
 * 
 * Fourth-order Bspline for 1-3D lines and 2D surface. This class is based on code from
 * Dave Eberly's MAGIC C++ library.
 *
 */
public class AlgorithmBSpline extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Blending matrix for a 4th degree Bspline. */
    private static double[][] mat4 = {
        { 0.04166666666666667, -0.1666666666666667, 0.25, -0.1666666666666667, 0.04166666666666667 },
        { 0.4583333333333333, -0.50000000, -0.25, 0.50000000, -0.1666666666666667 },
        { 0.4583333333333333, 0.50000000, -0.25, -0.50000000, 0.25000000 },
        { 0.04166666666666667, 0.1666666666666667, 0.25, 0.1666666666666667, -0.1666666666666667 },
        { 0.0, 0.0, 0.0, 0.0, 0.04166666666666667 }
    };
    
    private final double DBL_EPSILON = 2.2204460492503131e-16;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int degree = -1;

    /** DOCUMENT ME! */
    private double[] dt = new double[5];
    
    /** DOCUMENT ME! */
    private double[] dt_double = new double[5];

    /** DOCUMENT ME! */
    private double[] geoms = new double[5];

    /** DOCUMENT ME! */
    private double[] geomx = new double[5];

    /** DOCUMENT ME! */
    private double[] geomy = new double[5];

    /** DOCUMENT ME! */
    private double[] geomz = new double[5];

    /** DOCUMENT ME! */
    private int width, height, depth;

    /** DOCUMENT ME! */
    private int xold_tbase = -1;

    /** DOCUMENT ME! */
    private int xyold_tbase = -1;
    
    private ColorInterpolation colorInter;
    
    private double[][][] imData;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * default constructor.
     */
    public AlgorithmBSpline() { }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * This method is out of date, as it provides smoothed interpolation as opposed
     * to precise interpolation. For precise interpolation, use the analogous method
     * in AlgorithmBSpline3D instead, as that provides true B-spline interpolation.
     * 
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   orderDx  derivative order in x direction (n <= 4 )
     * @param   orderDy  derivative order in y direction (n <= 4 )
     * @param   x        double point index along the Bspline indicating point of interest
     * @param   y        double point index along the Bspline indicating point of interest
     *
     * @return  the Bspline interpolated data point
     */
    
    public double bSpline2D(int orderDx, int orderDy, double x, double y){
		return interpolateBSpline2D(imData, x, y, degree);
	}
	
    /**
     * This method is out of date, as it provides smoothed interpolation as opposed
     * to precise interpolation. For precise interpolation, use the analogous method
     * in AlgorithmBSpline3D instead, as that provides true B-spline interpolation.
     * 
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   orderDx  derivative order in x direction (n <= 4 )
     * @param   orderDy  derivative order in y direction (n <= 4 )
     * @param   x        double point index along the Bspline indicating point of interest
     * @param   y        double point index along the Bspline indicating point of interest
     *
     * @return  the Bspline interpolated data point
     */

	public double[] bSpline2DC(int orderDx, int orderDy, double x, double y){
		return colorInter.interpolate2D(x, y);
	}
	
    
    /**
     * This method is out of date, as it provides smoothed interpolation as opposed
     * to precise interpolation. For precise interpolation, use the analogous method
     * in AlgorithmBSpline3D instead, as that provides true B-spline interpolation.
     * 
     * 3D graph Bspline for black and white. This method can also be used to calculate derivatives of the Bspline.
     * WARNING - programmer must call setup3DBSpline before using this function!!!
     *
     * @param   orderDx  derivative order in x direction (n <= 4 )
     * @param   orderDy  derivative order in y direction (n <= 4 )
     * @param   orderDz  derivative order in z direction (n <= 4 )
     * @param   x        double point index along the Bspline indicating point of interest
     * @param   y        double point index along the Bspline indicating point of interest
     * @param   z        double point index along the Bspline indicating pointof interest
     *
     * @return  the Bspline interpolated data point
     */

	public double bSpline3D(int orderDx, int orderDy, int orderDz, double x, double y, double z){
		return interpolateBSpline(imData, x, y, z, degree);
	}
	
    /**
     * This method is out of date, as it provides smoothed interpolation as opposed
     * to precise interpolation. For precise interpolation, use the analogous method
     * in AlgorithmBSpline3D instead, as that provides true B-spline interpolation.
     * 
     * 3D graph Bspline for color. This method can also be used to calculate derivatives of the Bspline. WARNING -
     * programmer must call setup3DBSpline before using this function!!!
     *
     * @param   orderDx  derivative order in x direction (n <= 4 )
     * @param   orderDy  derivative order in y direction (n <= 4 )
     * @param   orderDz  derivative order in z direction (n <= 4 )
     * @param   x        double point index along the Bspline indicating point of interest
     * @param   y        double point index along the Bspline indicating point of interest
     * @param   z        double point index along the Bspline indicating point of interest
     *
     * @return  the Bspline interpolated data point
     */
	
	public double[] bSpline3DC(int orderDx, int orderDy, int orderDz, double x, double y, double z){
		return colorInter.interpolate(x, y, z);
	}

    /**
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   derivOrder  derivative order (n <= 4 )
     * @param   t           float point index along the Bspline indicating point of interest
     * @param   dataX       control points for the Bspline
     *
     * @return  the Bspline interpolated data point
     */
    public float bSplineJet1D(byte derivOrder, float t, float[] dataX) {

        float tdiff, tdiff2;
        int tbase;
        float x;
        int i;
        int i0, j0, k0;

        tbase = (int) t;

        if (tbase != xold_tbase) {
            xold_tbase = tbase;

            for (i0 = 0; i0 <= 4; i0++) {
                geoms[i0] = 0;

                for (j0 = 0, k0 = tbase - 1; j0 <= 4; j0++, k0++) {
                    geoms[i0] += mat4[j0][i0] * dataX[k0];
                }
            }
        }

        tdiff = t - tbase;

        switch (derivOrder) {

            case 0:
                dt[0] = 1;
                dt[1] = tdiff;
                dt[2] = tdiff * dt[1];
                dt[3] = tdiff * dt[2];
                dt[4] = tdiff * dt[3];
                break;

            case 1:
                tdiff2 = tdiff * tdiff;
                dt[1] = 1;
                dt[2] = 2 * tdiff;
                dt[3] = 3 * tdiff2;
                dt[4] = 4 * tdiff * tdiff2;
                break;

            case 2:
                dt[2] = 2;
                dt[3] = 6 * tdiff;
                dt[4] = 12 * tdiff * tdiff;
                break;

            case 3:
                dt[3] = 6;
                dt[4] = 24 * tdiff;
                break;

            case 4:
                dt[4] = 24;
                break;
        }

        x = 0;

        for (i = derivOrder; i <= 4; i++) {
            x += dt[i] * geoms[i];
        }

        return x;
    }

    /**
     * This method can also be used to calculate derivatives of the Bspline.
     * Used for smoothing VOI lines
     *
     * @param   derivOrder  derivative order (n <= 4 )
     * @param   t           float point index along the Bspline indicating point of interest
     * @param   dataX       control points for the Bspline
     * @param   dataY       control points for the Bspline
     *
     * @return  the Bspline interpolated data point
     */
    public Vector2f bSplineJetXY(int derivOrder, float t, float[] dataX, float[] dataY) {
        float tdiff, tdiff2;
        int tbase;
        int i;
        int i0, j0, k0;
        float x, y;

        tbase = (int) t;

        if (tbase != xyold_tbase) {
            xyold_tbase = tbase;

            for (i0 = 0; i0 <= 4; i0++) {
                geomx[i0] = 0;
                geomy[i0] = 0;

                for (j0 = 0, k0 = tbase - 2; j0 <= 4; j0++, k0++) {
                    geomx[i0] += mat4[j0][i0] * dataX[k0];
                    geomy[i0] += mat4[j0][i0] * dataY[k0];
                }
            }
        }

        tdiff = t - tbase;

        switch (derivOrder) {

            case 0:
                dt[0] = 1;
                dt[1] = tdiff;
                dt[2] = tdiff * dt[1];
                dt[3] = tdiff * dt[2];
                dt[4] = tdiff * dt[3];
                break;

            case 1:
                tdiff2 = tdiff * tdiff;
                dt[1] = 1;
                dt[2] = 2 * tdiff;
                dt[3] = 3 * tdiff2;
                dt[4] = 4 * tdiff * tdiff2;
                break;

            case 2:
                dt[2] = 2;
                dt[3] = 6 * tdiff;
                dt[4] = 12 * tdiff * tdiff;
                break;

            case 3:
                dt[3] = 6;
                dt[4] = 24 * tdiff;
                break;

            case 4:
                dt[4] = 24;
                break;
        }

        x = 0;
        y = 0;

        for (i = derivOrder; i <= 4; i++) {
            x += dt[i] * geomx[i];
            y += dt[i] * geomy[i];
        }

        return (new Vector2f(x, y));
    }

    /**
     * This method can also be used to calculate derivatives of the Bspline.
     * Used for smoothing VOI lines
     *
     * @param   derivOrder  derivative order (n <= 4 )
     * @param   t           float point index along the Bspline indicating point of interest
     * @param   dataX       control points for the Bspline
     * @param   dataY       control points for the Bspline
     * @param   dataZ       control points for the Bspline
     *
     * @return  the Bspline interpolated data point
     */
    public Vector3f bSplineJetXYZ(int derivOrder, float t, float[] dataX, float[] dataY, float[] dataZ) {
        float tdiff, tdiff2;
        int tbase;
        int i;
        int i0, j0, k0;
        float x, y, z;

        tbase = (int) t;

        if (tbase != xyold_tbase) {
            xyold_tbase = tbase;

            for (i0 = 0; i0 <= 4; i0++) {
                geomx[i0] = 0;
                geomy[i0] = 0;
                geomz[i0] = 0;

                for (j0 = 0, k0 = tbase - 2; j0 <= 4; j0++, k0++) {
                    geomx[i0] += mat4[j0][i0] * dataX[k0];
                    geomy[i0] += mat4[j0][i0] * dataY[k0];
                    geomz[i0] += mat4[j0][i0] * dataZ[k0];
                }
            }
        }

        tdiff = t - tbase;

        switch (derivOrder) {

            case 0:
                dt[0] = 1;
                dt[1] = tdiff;
                dt[2] = tdiff * dt[1];
                dt[3] = tdiff * dt[2];
                dt[4] = tdiff * dt[3];
                break;

            case 1:
                tdiff2 = tdiff * tdiff;
                dt[1] = 1;
                dt[2] = 2 * tdiff;
                dt[3] = 3 * tdiff2;
                dt[4] = 4 * tdiff * tdiff2;
                break;

            case 2:
                dt[2] = 2;
                dt[3] = 6 * tdiff;
                dt[4] = 12 * tdiff * tdiff;
                break;

            case 3:
                dt[3] = 6;
                dt[4] = 24 * tdiff;
                break;

            case 4:
                dt[4] = 24;
                break;
        }

        x = 0;
        y = 0;
        z = 0;

        for (i = derivOrder; i <= 4; i++) {
            x += dt[i] * geomx[i];
            y += dt[i] * geomy[i];
            z += dt[i] * geomz[i];
        }

        return (new Vector3f(x, y, z));
    }
    
    /**
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   derivOrder  derivative order (n <= 4 )
     * @param   t           float point index along the Bspline indicating point of interest
     * @param   dataX       control points for the Bspline
     * @param   dataY       control points for the Bspline
     * @param   dataZ       control points for the Bspline
     *
     * @return  the Bspline interpolated data point
     */
    public Vector3d bSplineJetXYZ_double(int derivOrder, float t, float[] dataX, float[] dataY, float[] dataZ) {
        double tdiff, tdiff2;
        int tbase;
        int i;
        int i0, j0, k0;
        double x, y, z;

        tbase = (int) t;
        
        double[] geomx = new double[5];
        double[] geomy = new double[5];
        double[] geomz = new double[5];
        
        double[][] mat4 = {
                { 0.04166666667, -0.16666666667, 0.25, -0.16666666667, 0.04166666667 },
                { 0.45833333, -0.50000000, -0.25, 0.50000000, -0.16666666667 },
                { 0.45833333, 0.50000000, -0.25, -0.50000000, 0.25000000 },
                { 0.04166666667, 0.16666666667, 0.25, 0.16666666667, -0.16666666667 },
                { 0, 0, 0, 0, 0.04166666667 }
            };

        if (tbase != xyold_tbase) {
            xyold_tbase = tbase;

            for (i0 = 0; i0 <= 4; i0++) {
                geomx[i0] = 0;
                geomy[i0] = 0;
                geomz[i0] = 0;

                for (j0 = 0, k0 = tbase - 2; j0 <= 4; j0++, k0++) {
                    geomx[i0] += mat4[j0][i0] * (double)dataX[k0];
                    geomy[i0] += mat4[j0][i0] * (double)dataY[k0];
                    geomz[i0] += mat4[j0][i0] * (double)dataZ[k0];
                }
            }
        }

        tdiff = t - tbase;

        switch (derivOrder) {

            case 0:
            	dt_double[0] = 1;
            	dt_double[1] = tdiff;
            	dt_double[2] = tdiff * dt_double[1];
            	dt_double[3] = tdiff * dt_double[2];
            	dt_double[4] = tdiff * dt_double[3];
                break;

            case 1:
                tdiff2 = tdiff * tdiff;
                dt_double[1] = 1;
                dt_double[2] = 2 * tdiff;
                dt_double[3] = 3 * tdiff2;
                dt_double[4] = 4 * tdiff * tdiff2;
                break;

            case 2:
            	dt_double[2] = 2;
            	dt_double[3] = 6 * tdiff;
            	dt_double[4] = 12 * tdiff * tdiff;
                break;

            case 3:
            	dt_double[3] = 6;
            	dt_double[4] = 24 * tdiff;
                break;

            case 4:
            	dt_double[4] = 24;
                break;
        }

        x = 0;
        y = 0;
        z = 0;

        for (i = derivOrder; i <= 4; i++) {
            x += dt[i] * geomx[i];
            y += dt[i] * geomy[i];
            z += dt[i] * geomz[i];
        }

        return (new Vector3d(x, y, z));
    }
    
    /**
     * Clean up memeory.
     */
    public void finalize() {
        geoms = null;
        geomx = null;
        geomy = null;
        geomz = null;
        dt = null;

        super.finalize();
    }

    /**
     * Resets parameters used by the Bsplines.
     */
    public void resetBspline() {
        xold_tbase = -1;
        xyold_tbase = -1;
    }

    /**
     * Default method that is not really appropriate for this class but must be defined because this class extends
     * AlgorithmBase.
     */
    public void runAlgorithm() { }
    
    /**
     * Allocates memory and constructs arrays needed for BSpline.
     *
     * @param  vol      volume comprising control points for the Bspline
     * @param  extents  vol extents (xDim,yDim)
     * @param  _degree  degree of spline (3 or 4)
     */
    
    public void setup2DBSpline(double[] vol, int[] extents, int _degree){
		width = extents[0];
		height = extents[1];
		depth = 1;
		degree = _degree;
		imData = new double[width][height][depth];
		
		int ind = 0;
		for(int j=0;j<height;j++){
			for(int i=0;i<width;i++, ind++){
				imData[i][j][0] = vol[ind];
			}
		}
		
		SamplesToCoefficients2D(imData, degree);
		
	}
    
    
    /**
     * Allocates memory and constructs arrays needed for BSpline.
     *
     * @param  vol      volume comprising control points for the Bspline
     * @param  extents  vol extents (xDim,yDim)
     * @param  _degree  degree of spline (3 or 4)
     */
	public void setup2DBSplineC(double[] vol, int[] extents, int _degree){
		width = extents[0];
		height = extents[1];
		depth = 1;
		degree = _degree;
		double[][][][] data = new double[4][width][height][depth];
		
		int ind = 0;
		for(int k=0;k<depth;k++){
			for(int j=0;j<height;j++){
				for(int i=0;i<width;i++){
					for(int c=0;c<4;c++,ind++){
						data[c][i][j][k] = vol[ind];
					}
				}
			}
		}
		
		colorInter = new ColorInterpolation(data);
		colorInter.setup2D();
	}

	/**
     * setup3DBSpline - setup 3D Bspline for black and white. Allocates memory and constructs arrays needed for BSpline
     *
     * @param  vol      volume comprising control points for the Bspline
     * @param  extents  vol extents (xDim,yDim, zDim)
     * @param  _degree  degree of spline (3 or 4)
     */
	public void setup3DBSpline(double[] vol, int[] extents, int _degree){
		width = extents[0];
		height = extents[1];
		depth = extents[2];
		degree = _degree;
		imData = new double[width][height][depth];
		
		int ind = 0;
		for(int k=0;k<depth;k++){
			for(int j=0;j<height;j++){
				for(int i=0;i<width;i++, ind++){
					imData[i][j][k] = vol[ind];
				}
			}
		}
		
		SamplesToCoefficients(imData, degree);
	}
	

    /**
     * Allocates memory and constructs arrays needed for BSpline.
     *
     * @param  vol      volume comprising control points for the Bspline
     * @param  extents  vol extents (xDim,yDim, zDim)
     * @param  _degree  degree of spline (3 or 4)
     */
	public void setup3DBSplineC(double[] vol, int[] extents, int _degree){
		width = extents[0];
		height = extents[1];
		depth = extents[2];
		degree = _degree;
		double[][][][] data = new double[4][width][height][depth];
		
		int ind = 0;
		for(int k=0;k<depth;k++){
			for(int j=0;j<height;j++){
				for(int i=0;i<width;i++){
					for(int c=0;c<4;c++,ind++){
						data[c][i][j][k] = vol[ind];
					}
				}
			}
		}
		colorInter = new ColorInterpolation(data);
		colorInter.setup();
		
	}

	/**
	 * Do the B-Spline Interpolation
	 *
	 * @param Bcoeff        B-spline array of coefficients
	 * @param Width         Width of the image
	 * @param Height        Height of the image
	 * @param Depth         Depth of the image
	 * @param x             x coordinate where to interpolate
	 * @param y             y coordinate where to interpolate
	 * @param z             z coordinate where to interpolate
	 * @param SplineDegree  Degree of the B-Spline
	 */
	private double interpolateBSpline(double[][][] image, double x, double y, double z, int SplineDegree) {
		double xWeight[] = new double[6];
		double yWeight[] = new double[6];
		double zWeight[] = new double[6];
		double interpolated = 0.0;
		double w, w2, w4, t, t0, t1;

		long xIndex[] = new long[6];
		long yIndex[] = new long[6];
		long zIndex[] = new long[6];
		long width2  = 2L * width - 2L;
		long height2 = 2L * height - 2L;
		long depth2  = 2L * depth - 2L;

		/**
		 * @param i  Index for x
		 * @param j  Index for y
		 * @param k  Index for z
		 * @param s  Index for Spline degree
		 */
		int i, j, k, s;


		if ((SplineDegree % 2) == 1) {
			/**
			 * Odd Degree
			 */
			i = (int) Math.floor(x) - SplineDegree / 2;
			j = (int) Math.floor(y) - SplineDegree / 2;
			k = (int) Math.floor(z) - SplineDegree / 2;
		} else {
			/**
			 * Even Degree
			 */
			i = (int) Math.floor(x + 0.5) - SplineDegree / 2;
			j = (int) Math.floor(y + 0.5) - SplineDegree / 2;
			k = (int) Math.floor(z + 0.5) - SplineDegree / 2;
		}


		for (s = 0; s <= SplineDegree; s++) {
			xIndex[s] = i++;
			yIndex[s] = j++;
			zIndex[s] = k++;
		}


		switch (SplineDegree) {
			case 2:
				/* x */
				w = x - (double)xIndex[1];
				xWeight[1] = 3.0 / 4.0 - w * w;
				xWeight[2] = (1.0 / 2.0) * (w - xWeight[1] + 1.0);
				xWeight[0] = 1.0 - xWeight[1] - xWeight[2];

				/* y */
				w = y - (double)yIndex[1];
				yWeight[1] = 3.0 / 4.0 - w * w;
				yWeight[2] = (1.0 / 2.0) * (w - yWeight[1] + 1.0);
				yWeight[0] = 1.0 - yWeight[1] - yWeight[2];

				/* z */
				w = z - (double)zIndex[1];
				zWeight[1] = 3.0 / 4.0 - w * w;
				zWeight[2] = (1.0 / 2.0) * (w - zWeight[1] + 1.0);
				zWeight[0] = 1.0 - yWeight[1] - zWeight[2];

				break;

			case 3:
				/* x */
				w = x - (double)xIndex[1];
				xWeight[3] = (1.0 / 6.0) * w * w * w;
				xWeight[0] = (1.0 / 6.0) + (1.0 / 2.0) * w * (w - 1.0) - xWeight[3];
				xWeight[2] = w + xWeight[0] - 2.0 * xWeight[3];
				xWeight[1] = 1.0 - xWeight[0] - xWeight[2] - xWeight[3];

				/* y */
				w = y - (double)yIndex[1];
				yWeight[3] = (1.0 / 6.0) * w * w * w;
				yWeight[0] = (1.0 / 6.0) + (1.0 / 2.0) * w * (w - 1.0) - yWeight[3];
				yWeight[2] = w + yWeight[0] - 2.0 * yWeight[3];
				yWeight[1] = 1.0 - yWeight[0] - yWeight[2] - yWeight[3];

				/* z */
				w = z - (double)zIndex[1];
				zWeight[3] = (1.0 / 6.0) * w * w * w;
				zWeight[0] = (1.0 / 6.0) + (1.0 / 2.0) * w * (w - 1.0) - zWeight[3];
				zWeight[2] = w + zWeight[0] - 2.0 * zWeight[3];
				zWeight[1] = 1.0 - zWeight[0] - zWeight[2] - zWeight[3];

				break;

			case 4:
				/* x */
				w = x - (double)xIndex[2];
				w2 = w * w;
				t = (1.0 / 6.0) * w2;
				xWeight[0] = 1.0 / 2.0 - w;
				xWeight[0] *= xWeight[0];
				xWeight[0] *= (1.0 / 24.0) * xWeight[0];
				t0 = w * (t - 11.0 / 24.0);
				t1 = 19.0 / 96.0 + w2 * (1.0 / 4.0 - t);
				xWeight[1] = t1 + t0;
				xWeight[3] = t1 - t0;
				xWeight[4] = xWeight[0] + t0 + (1.0 / 2.0) * w;
				xWeight[2] = 1.0 - xWeight[0] - xWeight[1] - xWeight[3] - xWeight[4];

				/* y */
				w = y - (double)yIndex[2];
				w2 = w * w;
				t = (1.0 / 6.0) * w2;
				yWeight[0] = 1.0 / 2.0 - w;
				yWeight[0] *= yWeight[0];
				yWeight[0] *= (1.0 / 24.0) * yWeight[0];
				t0 = w * (t - 11.0 / 24.0);
				t1 = 19.0 / 96.0 + w2 * (1.0 / 4.0 - t);
				yWeight[1] = t1 + t0;
				yWeight[3] = t1 - t0;
				yWeight[4] = yWeight[0] + t0 + (1.0 / 2.0) * w;
				yWeight[2] = 1.0 - yWeight[0] - yWeight[1] - yWeight[3] - yWeight[4];

				/* z */
				w = z - (double)zIndex[2];
				w2 = w * w;
				t = (1.0 / 6.0) * w2;
				zWeight[0] = 1.0 / 2.0 - w;
				zWeight[0] *= zWeight[0];
				zWeight[0] *= (1.0 / 24.0) * zWeight[0];
				t0 = w * (t - 11.0 / 24.0);
				t1 = 19.0 / 96.0 + w2 * (1.0 / 4.0 - t);
				zWeight[1] = t1 + t0;
				zWeight[3] = t1 - t0;
				zWeight[4] = zWeight[0] + t0 + (1.0 / 2.0) * w;
				zWeight[2] = 1.0 - zWeight[0] - zWeight[1] - zWeight[3] - zWeight[4];

				break;

			case 5:
				/* x */
				w = x - (double)xIndex[2];
				w2 = w * w;
				xWeight[5] = (1.0 / 120.0) * w * w2 * w2;
				w2 -= w;
				w4 = w2 * w2;
				w -= 1.0 / 2.0;
				t = w2 * (w2 - 3.0);
				xWeight[0] = (1.0 / 24.0) * (1.0 / 5.0 + w2 + w4) - xWeight[5];
				t0 = (1.0 / 24.0) * (w2 * (w2 - 5.0) + 46.0 / 5.0);
				t1 = (-1.0 / 12.0) * w * (t + 4.0);
				xWeight[2] = t0 + t1;
				xWeight[3] = t0 - t1;
				t0 = (1.0 / 16.0) * (9.0 / 5.0 - t);
				t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
				xWeight[1] = t0 + t1;
				xWeight[4] = t0 - t1;

				/* y */
				w = y - (double)yIndex[2];
				w2 = w * w;
				yWeight[5] = (1.0 / 120.0) * w * w2 * w2;
				w2 -= w;
				w4 = w2 * w2;
				w -= 1.0 / 2.0;
				t = w2 * (w2 - 3.0);
				yWeight[0] = (1.0 / 24.0) * (1.0 / 5.0 + w2 + w4) - yWeight[5];
				t0 = (1.0 / 24.0) * (w2 * (w2 - 5.0) + 46.0 / 5.0);
				t1 = (-1.0 / 12.0) * w * (t + 4.0);
				yWeight[2] = t0 + t1;
				yWeight[3] = t0 - t1;
				t0 = (1.0 / 16.0) * (9.0 / 5.0 - t);
				t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
				yWeight[1] = t0 + t1;
				yWeight[4] = t0 - t1;

				/* z */
				w = z - (double)zIndex[2];
				w2 = w * w;
				zWeight[5] = (1.0 / 120.0) * w * w2 * w2;
				w2 -= w;
				w4 = w2 * w2;
				w -= 1.0 / 2.0;
				t = w2 * (w2 - 3.0);
				zWeight[0] = (1.0 / 24.0) * (1.0 / 5.0 + w2 + w4) - zWeight[5];
				t0 = (1.0 / 24.0) * (w2 * (w2 - 5.0) + 46.0 / 5.0);
				t1 = (-1.0 / 12.0) * w * (t + 4.0);
				zWeight[2] = t0 + t1;
				zWeight[3] = t0 - t1;
				t0 = (1.0 / 16.0) * (9.0 / 5.0 - t);
				t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
				zWeight[1] = t0 + t1;
				zWeight[4] = t0 - t1;

				break;

			default:
				System.out.println("\n\nInvalid Spline Degree.\n\n");
				return(0.0f);
		}


		/**
		 * Boundary Conditions
		 */
		if (width == 1) {
			for (s = 0; s <= SplineDegree; s++) {
				xIndex[s] = 0;
			}
		}


		if (height == 1) {
			for (s = 0; s <= SplineDegree; s++) {
				yIndex[s] = 0;
			}
		}


		if (depth == 1) {
			for (s = 0; s <= SplineDegree; s++) {
				zIndex[s] = 0;
			}
		}


		for (s = 0; s <= SplineDegree; s++) {
			xIndex[s] = (xIndex[s] < 0) ? (-xIndex[s] - width2 * ((-xIndex[s]) / width2)) : (xIndex[s] - width2 * (xIndex[s] / width2));

			if (width <= xIndex[s]) {
				xIndex[s] = width2 - xIndex[s];
			}


			yIndex[s] = (yIndex[s] < 0) ? (-yIndex[s] - height2 * ((-yIndex[s]) / height2)) : (yIndex[s] - height2 * (yIndex[s] / height2));

			if (height <= yIndex[s]) {
				yIndex[s] = height2 - yIndex[s];
			}


			zIndex[s] = (zIndex[s] < 0) ? (-zIndex[s] - depth2 * ((-zIndex[s]) / depth2)) : (zIndex[s] - depth2 * (zIndex[s] / depth2));

			if (depth <= zIndex[s]) {
				zIndex[s] = depth2 - zIndex[s];
			}
		}


		/**
		 * At last, perform interpolation.
		 */
		interpolated = 0.0;

		for (k = 0; k <= SplineDegree; k++) {
			t = 0;

			for (j = 0; j <= SplineDegree; j++) {
				w = 0.0;

				for (i = 0; i <= SplineDegree; i++) {
					w += ((double) xWeight[i]) * (image[(int) xIndex[i]][(int) yIndex[j]][(int) zIndex[k]]);
				}

				t += yWeight[j] * w;
			}

			interpolated += zWeight[k] * t;
		}



		return interpolated;
	}
	
	private double interpolateBSpline2D(double[][][] image, double x, double y, int SplineDegree) {
		double xWeight[] = new double[6];
		double yWeight[] = new double[6];
		double interpolated = 0.0;
		double w, w2, w4, t, t0, t1;

		long xIndex[] = new long[6];
		long yIndex[] = new long[6];
		long width2  = 2L * width - 2L;
		long height2 = 2L * height - 2L;

		/**
		 * @param i  Index for x
		 * @param j  Index for y
		 * @param k  Index for z
		 * @param s  Index for Spline degree
		 */
		int i, j, s;


		if ((SplineDegree % 2) == 1) {
			/**
			 * Odd Degree
			 */
			i = (int) Math.floor(x) - SplineDegree / 2;
			j = (int) Math.floor(y) - SplineDegree / 2;
		} else {
			/**
			 * Even Degree
			 */
			i = (int) Math.floor(x + 0.5) - SplineDegree / 2;
			j = (int) Math.floor(y + 0.5) - SplineDegree / 2;
		}


		for (s = 0; s <= SplineDegree; s++) {
			xIndex[s] = i++;
			yIndex[s] = j++;
		}


		switch (SplineDegree) {
			case 2:
				/* x */
				w = x - (double)xIndex[1];
				xWeight[1] = 3.0 / 4.0 - w * w;
				xWeight[2] = (1.0 / 2.0) * (w - xWeight[1] + 1.0);
				xWeight[0] = 1.0 - xWeight[1] - xWeight[2];

				/* y */
				w = y - (double)yIndex[1];
				yWeight[1] = 3.0 / 4.0 - w * w;
				yWeight[2] = (1.0 / 2.0) * (w - yWeight[1] + 1.0);
				yWeight[0] = 1.0 - yWeight[1] - yWeight[2];

				break;

			case 3:
				/* x */
				w = x - (double)xIndex[1];
				xWeight[3] = (1.0 / 6.0) * w * w * w;
				xWeight[0] = (1.0 / 6.0) + (1.0 / 2.0) * w * (w - 1.0) - xWeight[3];
				xWeight[2] = w + xWeight[0] - 2.0 * xWeight[3];
				xWeight[1] = 1.0 - xWeight[0] - xWeight[2] - xWeight[3];

				/* y */
				w = y - (double)yIndex[1];
				yWeight[3] = (1.0 / 6.0) * w * w * w;
				yWeight[0] = (1.0 / 6.0) + (1.0 / 2.0) * w * (w - 1.0) - yWeight[3];
				yWeight[2] = w + yWeight[0] - 2.0 * yWeight[3];
				yWeight[1] = 1.0 - yWeight[0] - yWeight[2] - yWeight[3];

				break;

			case 4:
				/* x */
				w = x - (double)xIndex[2];
				w2 = w * w;
				t = (1.0 / 6.0) * w2;
				xWeight[0] = 1.0 / 2.0 - w;
				xWeight[0] *= xWeight[0];
				xWeight[0] *= (1.0 / 24.0) * xWeight[0];
				t0 = w * (t - 11.0 / 24.0);
				t1 = 19.0 / 96.0 + w2 * (1.0 / 4.0 - t);
				xWeight[1] = t1 + t0;
				xWeight[3] = t1 - t0;
				xWeight[4] = xWeight[0] + t0 + (1.0 / 2.0) * w;
				xWeight[2] = 1.0 - xWeight[0] - xWeight[1] - xWeight[3] - xWeight[4];

				/* y */
				w = y - (double)yIndex[2];
				w2 = w * w;
				t = (1.0 / 6.0) * w2;
				yWeight[0] = 1.0 / 2.0 - w;
				yWeight[0] *= yWeight[0];
				yWeight[0] *= (1.0 / 24.0) * yWeight[0];
				t0 = w * (t - 11.0 / 24.0);
				t1 = 19.0 / 96.0 + w2 * (1.0 / 4.0 - t);
				yWeight[1] = t1 + t0;
				yWeight[3] = t1 - t0;
				yWeight[4] = yWeight[0] + t0 + (1.0 / 2.0) * w;
				yWeight[2] = 1.0 - yWeight[0] - yWeight[1] - yWeight[3] - yWeight[4];

				break;

			case 5:
				/* x */
				w = x - (double)xIndex[2];
				w2 = w * w;
				xWeight[5] = (1.0 / 120.0) * w * w2 * w2;
				w2 -= w;
				w4 = w2 * w2;
				w -= 1.0 / 2.0;
				t = w2 * (w2 - 3.0);
				xWeight[0] = (1.0 / 24.0) * (1.0 / 5.0 + w2 + w4) - xWeight[5];
				t0 = (1.0 / 24.0) * (w2 * (w2 - 5.0) + 46.0 / 5.0);
				t1 = (-1.0 / 12.0) * w * (t + 4.0);
				xWeight[2] = t0 + t1;
				xWeight[3] = t0 - t1;
				t0 = (1.0 / 16.0) * (9.0 / 5.0 - t);
				t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
				xWeight[1] = t0 + t1;
				xWeight[4] = t0 - t1;

				/* y */
				w = y - (double)yIndex[2];
				w2 = w * w;
				yWeight[5] = (1.0 / 120.0) * w * w2 * w2;
				w2 -= w;
				w4 = w2 * w2;
				w -= 1.0 / 2.0;
				t = w2 * (w2 - 3.0);
				yWeight[0] = (1.0 / 24.0) * (1.0 / 5.0 + w2 + w4) - yWeight[5];
				t0 = (1.0 / 24.0) * (w2 * (w2 - 5.0) + 46.0 / 5.0);
				t1 = (-1.0 / 12.0) * w * (t + 4.0);
				yWeight[2] = t0 + t1;
				yWeight[3] = t0 - t1;
				t0 = (1.0 / 16.0) * (9.0 / 5.0 - t);
				t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
				yWeight[1] = t0 + t1;
				yWeight[4] = t0 - t1;

				break;

			default:
				System.out.println("\n\nInvalid Spline Degree.\n\n");
				return(0.0f);
		}


		/**
		 * Boundary Conditions
		 */
		if (width == 1) {
			for (s = 0; s <= SplineDegree; s++) {
				xIndex[s] = 0;
			}
		}


		if (height == 1) {
			for (s = 0; s <= SplineDegree; s++) {
				yIndex[s] = 0;
			}
		}

		for (s = 0; s <= SplineDegree; s++) {
			xIndex[s] = (xIndex[s] < 0) ? (-xIndex[s] - width2 * ((-xIndex[s]) / width2)) : (xIndex[s] - width2 * (xIndex[s] / width2));

			if (width <= xIndex[s]) {
				xIndex[s] = width2 - xIndex[s];
			}


			yIndex[s] = (yIndex[s] < 0) ? (-yIndex[s] - height2 * ((-yIndex[s]) / height2)) : (yIndex[s] - height2 * (yIndex[s] / height2));

			if (height <= yIndex[s]) {
				yIndex[s] = height2 - yIndex[s];
			}


		}


		/**
		 * At last, perform interpolation.
		 */
		interpolated = 0.0;

		for (j = 0; j <= SplineDegree; j++) {
			w = 0.0;

			for (i = 0; i <= SplineDegree; i++) {
				w += ((double) xWeight[i]) * (image[(int) xIndex[i]][(int) yIndex[j]][0]);
			}

			interpolated += yWeight[j] * w;
		}

		return interpolated;
	}


	/**
	 * @param c  Samples
	 * @param DataLength Number of samples or coefficients
	 * @param z  Poles
	 * @param NbPoles  Number of poles
	 * @param Tolerance  Admissible relative error
	 */
	private void ConvertToInterpolationCoefficients(double c[], int DataLength, double z[], int NbPoles, double Tolerance) {
		double Lambda = 1.0;
		int n, k;


		/**
		 * Special case required by mirror boundaries.
		 */
		if (DataLength == 1) {
			return;
		}


		/**
		 * Compute the overall gain
		 */
		for (k = 0; k < NbPoles; k++) {
			Lambda = Lambda * (1.0 - z[k]) * (1.0 - 1.0 / z[k]);
		}


		/**
		 * Apply the gain
		 */
		for (n = 0; n < DataLength; n++) {
			c[n] *= Lambda;
		}


		/**
		 * Loop over all poles
		 */
		for (k = 0; k < NbPoles; k++) {
			c[0] = InitialCausalCoefficient(c, DataLength, z[k], Tolerance);

			for (n = 1; n < DataLength; n++) {
				c[n] += z[k] * c[n - 1];
			}


			c[DataLength - 1] = InitialAntiCausalCoefficient(c, DataLength, z[k]);

			for (n = DataLength - 2; 0 <= n; n--) {
				c[n] = z[k] * (c[n + 1] - c[n]);
			}
		}
	}


	private double InitialCausalCoefficient(double c[], int DataLength, double z, double Tolerance) {
		double Sum, zn, z2n, iz;
		int n, Horizon;

		Horizon = DataLength;
		if (Tolerance > 0.0) {
			/**
			 * Java Math.ceil() returns a double, which seems silly.
			 */
			Horizon = (int) Math.ceil(Math.log(Tolerance) / Math.log(Math.abs(z)));
		}


		if (Horizon < DataLength) {
			/**
			 * Accelerated Loop
			 */
			zn = z;
			Sum = c[0];

			for (n = 1; n < Horizon; n++) {
				Sum += zn * c[n];
				zn *= z;
			}

			return(Sum);
		} else {
			/**
			 * Full Loop
			 */
			zn = z;
			iz = 1.0 / z;
			z2n = Math.pow(z, (DataLength - 1));
			Sum = c[0] + z2n * c[DataLength - 1];
			z2n *= z2n * iz;
			for (n = 1; n <= DataLength - 2; n++) {
				Sum += (zn + z2n) * c[n];
				zn *= z;
				z2n *= iz;
			}

			return(Sum / (1.0 - zn * zn));
		}
	}


	private double InitialAntiCausalCoefficient(double c[], int DataLength, double z) {
		return((z / (z * z - 1.0)) * (z * c[DataLength - 2] + c[DataLength - 1]));
	}


	private double[] GetRow(int y, int z, double Image[][][], int width){
		double Line[] = new double[width];
		int i = 0;


		for (i = 0; i < width; i++) {
			Line[i] = Image[i][y][z];
		}

		return Line;
	}


	private void PutRow(double Line[], int y, int z, double Image[][][]){
		int i = 0;


		for (i = 0; i < width; i++) {
			Image[i][y][z] = Line[i];
		}

		return;
	}


	private double[] GetColumn(int x, int z, double Image[][][]) {
		double Line[] = new double[height];
		int j = 0;


		for (j = 0; j < height; j++) {
			Line[j] = Image[x][j][z];
		}

		return Line;
	}


	private void PutColumn(double Line[], int x, int z, double Image[][][]) {
		int j = 0;


		for (j = 0; j < height; j++) {
			Image[x][j][z] = Line[j];
		}

		return;
	}


	private double[] GetVertical(int x, int y, double Image[][][]) {
		double Line[] = new double[depth];
		int k = 0;

		for (k = 0; k < depth; k++){
			Line[k] = Image[x][y][k];
		}

		return Line;
	}


	private void PutVertical(double Line[], int x, int y, double Image[][][]) {
		int k = 0;

		for (k = 0; k < depth; k++){
			Line[k] = Image[x][y][k];
			Image[x][y][k] = Line[k];
		}

		return;
	}


	private int SamplesToCoefficients(double Image[][][], int SplineDegree) {
		double Line[];
		double Pole[] = new double[2];
		int NbPoles;
		int x, y, z;



		switch (SplineDegree) {
			case 2:
				NbPoles = 1;
				Pole[0] = Math.sqrt(8.0) - 3.0;
				break;

			case 3:
				NbPoles = 1;
				Pole[0] = Math.sqrt(3.0) - 2.0;
				break;

			case 4:
				NbPoles = 2;
				Pole[0] = Math.sqrt(664.0 - Math.sqrt(438976.0)) + Math.sqrt(304.0) - 19.0;
				Pole[1] = Math.sqrt(664.0 + Math.sqrt(438976.0)) - Math.sqrt(304.0) - 19.0;
				break;

			case 5:
				NbPoles = 2;
				Pole[0] = Math.sqrt(135.0 / 2.0 - Math.sqrt(17745.0 / 4.0)) + Math.sqrt(105.0 / 4.0) - 13.0 / 2.0;
				Pole[1] = Math.sqrt(135.0 / 2.0 + Math.sqrt(17745.0 / 4.0)) - Math.sqrt(105.0 / 4.0) - 13.0 / 2.0;
				break;

			default:
				System.out.print("\n\nInvalid spline degree:" + SplineDegree + "\n\n");
				return(1);
		}


		/**
		 * Convert the image samples into interpolation coefficients
		 * Separable process: X-Axis
		 */
		for (z = 0; z < depth; z++){
			for (y = 0; y < height; y++) {
				Line = GetRow(y, z, Image, width);
				ConvertToInterpolationCoefficients(Line, width, Pole, NbPoles, DBL_EPSILON);
				PutRow(Line, y, z, Image);
			}
		}


		/**
		 * Convert the image samples into interpolation coefficients
		 * Separable process: Y-Axis
		 */
		for (z = 0; z < depth; z++){
			for (x = 0; x < width; x++) {
				Line = GetColumn(x, z, Image);
				ConvertToInterpolationCoefficients(Line, height, Pole, NbPoles, DBL_EPSILON);
				PutColumn(Line, x, z, Image);
			}
		}


		/**
		 * Convert the image samples into interpolation coefficients
		 * Separable process: Z-Axis
		 */
		for (y = 0; y < height; y++) {
			for (x = 0; x < width; x++) {
				Line = GetVertical(x, y, Image);
				ConvertToInterpolationCoefficients(Line, depth, Pole, NbPoles, DBL_EPSILON);
				PutVertical(Line, x, y, Image);
				
			}
		}

		return(0);
	}
	
	private int SamplesToCoefficients2D(double Image[][][], int SplineDegree) {
		double Line[];
		double Pole[] = new double[2];
		int NbPoles;
		int x, y;



		switch (SplineDegree) {
			case 2:
				NbPoles = 1;
				Pole[0] = Math.sqrt(8.0) - 3.0;
				break;

			case 3:
				NbPoles = 1;
				Pole[0] = Math.sqrt(3.0) - 2.0;
				break;

			case 4:
				NbPoles = 2;
				Pole[0] = Math.sqrt(664.0 - Math.sqrt(438976.0)) + Math.sqrt(304.0) - 19.0;
				Pole[1] = Math.sqrt(664.0 + Math.sqrt(438976.0)) - Math.sqrt(304.0) - 19.0;
				break;

			case 5:
				NbPoles = 2;
				Pole[0] = Math.sqrt(135.0 / 2.0 - Math.sqrt(17745.0 / 4.0)) + Math.sqrt(105.0 / 4.0) - 13.0 / 2.0;
				Pole[1] = Math.sqrt(135.0 / 2.0 + Math.sqrt(17745.0 / 4.0)) - Math.sqrt(105.0 / 4.0) - 13.0 / 2.0;
				break;

			default:
				System.out.print("\n\nInvalid spline degree:" + SplineDegree + "\n\n");
				return(1);
		}


		/**
		 * Convert the image samples into interpolation coefficients
		 * Separable process: X-Axis
		 */

		for (y = 0; y < height; y++) {
			Line = GetRow(y, 0, Image, width);
			ConvertToInterpolationCoefficients(Line, width, Pole, NbPoles, DBL_EPSILON);
			PutRow(Line, y, 0, Image);
		}
		


		/**
		 * Convert the image samples into interpolation coefficients
		 * Separable process: Y-Axis
		 */
		for (x = 0; x < width; x++) {
			Line = GetColumn(x, 0, Image);
			ConvertToInterpolationCoefficients(Line, height, Pole, NbPoles, DBL_EPSILON);
			PutColumn(Line, x, 0, Image);
		}

		return(0);
	}
	
	private class ColorInterpolation { 
		
		private double[][][][] colorData;
		
		private ColorInterpolation(double[][][][] data){
			colorData = data;
		}
		
		private void setup(){
			for(int i=0;i<4;i++){
				SamplesToCoefficients(colorData[i], degree);
			}
		}
		
		private void setup2D(){
			for(int i=0;i<4;i++){
				SamplesToCoefficients2D(colorData[i], degree);
			}
		}
		
		private double[] interpolate(double x, double y, double z){
			double[] value = new double[4];
			for(int i=0;i<4;i++){
				value[i] = interpolateBSpline(colorData[i], x, y, z, degree);
			}
			return value;
		}
		
		private double[] interpolate2D(double x, double y){
			double[] value = new double[4];
			for(int i=0;i<4;i++){
				value[i] = interpolateBSpline2D(colorData[i], x, y, degree);
			}
			return value;
		}
	}

}
