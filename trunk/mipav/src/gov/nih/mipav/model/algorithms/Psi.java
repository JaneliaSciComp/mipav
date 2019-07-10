package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.Preferences;

/**
 * <p>
 * This code calculates the psi function of an input argument x.
 * </p>
 * 
 * A typical real variable usage would be: <blockquote>
 * 
 * <pre>
 * double result[] = new double[1];
 * Psi psiTest = new Psi(5.0, result);
 * psiTest.run();
 * Preferences.debug(&quot;Psi(5.0) = &quot; + result[0] + &quot;\n&quot;);
 * UI.setDataText(&quot;Psi(5.0) = &quot; + result[0] + &quot;\n&quot;);
 * </pre>
 * 
 * </blockquote>
 * </p>
 * 
 * <p>
 * A typical complex variable usage would be: <blockquote>
 * 
 * <pre>
 * double realResult[] = new double[1];
 * double imagResult[] = new double[1];
 * Psi psiTest = new Psi(5.0, -1.0, realResult, imagResult);
 * psiTest.run();
 * Preferences.debug(&quot;Psi(5.0,-1.0) = &quot; + realResult[0] + &quot;, &quot; + imagResult[0] + &quot;\n&quot;);
 * </pre>
 * 
 * </blockquote>
 * </p>
 * <hr>
 * 
 * <p>
 * The FORTRAN code this class is based upon is from Computation of Special Functions by Shanjie Zhang and Jianming Jin
 * and copyright 1996 John Wiley &amp; Sons, Inc.
 * </p>
 * 
 * <p>
 * From the diskette that the FORTRAN code came on: <blockquote>
 * 
 * <pre>
 * DISCLAIMER OF WARRANTY
 * 
 * Although we have made a great effort to test and validate the 
 * computer programs, we make no warranties, express or implied, that 
 * these  programs  are  free  of  error,  or are consistent with any 
 * particular  standard  of  merchantability,  or that they will meet 
 * your requirements for any particular application.  They should not 
 * be relied on for  solving problems  whose incorrect solution could 
 * result in  injury to  a person or loss of property.  If you do use 
 * the programs in such a manner, it is at your own risk. The authors 
 * and publisher  disclaim all liability for  direct or consequential 
 * damages resulting from your use of the programs.
 * </pre>
 * 
 * </blockquote>
 * </p>
 */
public class Psi {

    public static final int REAL_VERSION = 1;

    public static final int COMPLEX_VERSION = 2;

    /** input argument */
    private double x;

    /** output result */
    private double result[];

    /** REAL_VERSION or COMPLEX_VERSION */
    private int version;

    /** Real part of input argument */
    @SuppressWarnings("unused")
    private double realX;

    /** Imaginary part of input argument */
    @SuppressWarnings("unused")
    private double imagX;

    /** Real part of output result */
    @SuppressWarnings("unused")
    private double realResult[];

    /** Imaginary part of output result */
    @SuppressWarnings("unused")
    private double imagResult[];
    
    public Psi() {
    	
    }

    /**
     * 
     * @param x Input argument
     * @param result outputted psi(x)
     */
    public Psi(final double x, final double result[]) {
        this.x = x;
        this.result = result;
        version = Psi.REAL_VERSION;
    }

    /**
     * 
     * @param realX real part of input argument
     * @param imagX imaginary part of input argument
     * @param realResult real part of outputted psi(x)
     * @param imagResult imaginary part of outputted psi(x)
     */
    public Psi(final double realX, final double imagX, final double realResult[], final double imagResult[]) {
        this.realX = realX;
        this.imagX = imagX;
        this.realResult = realResult;
        this.imagResult = imagResult;
        version = Psi.COMPLEX_VERSION;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Cleanup memory.
     * 
     * @throws Throwable DOCUMENT ME!
     */
    public void finalize() throws Throwable {

        super.finalize();
    }

    public void run() {
        if (version == Psi.REAL_VERSION) {
            realPsi();
        } else if (version == Psi.COMPLEX_VERSION) {
           complexPsi();
        }
    }
    
    public void testrealPsi() {
        result = new double[1];
        double xtest[] = new double[]{0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,
     		   2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4.0,4.1,4.2,4.3,4.4,4.5,
     		   4.6,4.7,4.8,4.9,5.0,-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,
     		   -0.7,-0.8,-0.9,-1.1,-1.2,-1.3,-1.4,-1.5,-1.6,-1.7,-1.8,-1.9,-2.1,-2.2,-2.3,-2.4,-2.5,-2.6,-2.7,-2.8,
     		   -2.9,-3.1,-3.2,-3.3,-3.4,-3.5,-3.6,-3.7,-3.8,-3.9,-4.1,-4.2,-4.3,-4.4,-4.5,-4.6,-4.7,-4.8,-4.9};
        double answer[] = new double[]{-1.04237549E1,-5.28903990,-3.50252422,-2.56138454,-1.96351003,-1.54061921,
        		-1.22002355,-.96500857,-.75492695,-.57721566,-.42375494,-.2890398966,-.169190889,-.0613845446,
        		.0364899740,.1260474528,.2085478749,.2849914333,.35618416,.4227843351,.48533597,.54429344,.60003988,.65290117,
        		.70315664,.75104745,.79678317,.84054699,.88249995,.92278434,.96152644,.99883889,1.03482249,1.06956784,
        		1.10315664,1.13566284,1.16715354,1.19768985,1.22732754,1.25611767,1.28410709,1.31133889,1.33785279,
        		1.36368548,1.38887093,1.41344062,1.43742381,1.46084774,1.48373779,1.50611767,9.2450731,4.0349914,
        		2.1133098,9.5938079E-1,3.6489974E-2,-8.9471788E-1,-2.0739528,-4.0390399,-9.3126438,1.0154164E1,
        		4.8683248,2.8825405,1.6736665,7.0315664E-1,-2.6971788E-1,-1.4857175,-3.4834843,-8.7863280,1.0630354E1,
        		5.3228702,3.3173232,2.0903332,1.1031566,1.1489751E-1,-1.1153471,-3.1263415,-8.4415005,1.0952935E1,
        		5.6353702,3.6203535,2.3844508,1.3888709,3.9267528E-1,-8.4507686E-1,-2.8631836,-8.1850902,1.1196838E1,
        		5.8734655,3.8529116,2.6117235,1.6110931,6.1006659E-1,-6.3231090E-1,-2.6548503,-7.9810086};
        int i;
        int errorsDetected = 0;
        for (i = 0; i < xtest.length; i++) {
     	   x = xtest[i];
     	   realPsi();
     	   Preferences.debug("x = " + x + " result = " + result[0] + "answer = " + answer[i] + "\n", Preferences.DEBUG_ALGORITHM);
     	   if ((result[0]/answer[i] < 1 - 1.0E-8) || (result[0]/answer[i] > 1 + 1.0E-8)) {
     		   Preferences.debug("Error detected\n",Preferences.DEBUG_ALGORITHM);
     		   errorsDetected++;
     	   }
        }
        Preferences.debug(errorsDetected + " errors found in " + xtest.length + " runs of realPsi()\n", Preferences.DEBUG_ALGORITHM);
        System.out.println(errorsDetected + " errors found in " + xtest.length + " runs of realPsi()");
     }
    
    public void testcomplexPsi() {
    	realResult = new double[1];
    	imagResult = new double[1];
    	double realXtest[] = new double[]{0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.0,1.0,1.0,
    			1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,
    			5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,5.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,
    			10.0,10.0,10.0,10.0};
    	double imagXtest[] = new double[]{0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0,0.0,0.5,1.0,
    			1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,
    			6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0,0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0};
    	double realAnswer[] = new double[]{-1.9635100,-8.6810736E-1,-5.1761651E-2,3.8496912E-1,6.8218670E-1,9.0941749E-1,1.0938865,
    			1.2493107,1.3836607,1.5020015,1.6077593,1.7033626,1.7905964,1.8708118,1.9450567,2.0141600,2.0787887,2.1394881,2.1967091,
    			2.2508292,2.3021677,-5.7721566E-1,-3.2888636E-1,9.4650321E-2,4.4469794E-1,7.1459152E-1,9.2985784E-1,1.1079807,1.2596236,
    			1.3915363,1.5082135,1.6127848,1.7075122,1.7940808,1.8737793,1.9476143,2.0163872,2.0807457,2.1412212,2.1982547,2.2522162,
    			2.3034193,1.5061177,1.5121800,1.5299444,1.5582350,1.5953607,1.6394074,1.6884935,1.7409416,1.7953598,1.8506554,1.9060082,
    			1.9608259,2.0146976,2.0673500,2.1186128,2.1683898,2.2166379,2.2633514,2.3085496,2.3522690,2.3945567,2.2517526,2.2531319,
    			2.2572473,2.2640322,2.2733794,2.2851469,2.2991644,2.3152406,2.3331715,2.3527467,2.3737567,2.3959973,2.4192741,2.4434055,
    			2.4682244,2.4935790,2.5193336,2.5453677,2.5715756,2.5978656,2.6241585};
    	double imagAnswer[] = new double[]{0.0,1.4406595,1.5649405,1.5705428,1.5707854,1.5707959,1.5707963,1.5707963,1.5707963,1.5707963,
    			1.5707963,1.5707963,1.5707963,1.5707963,1.5707963,1.5707963,1.5707963,1.5707963,1.5707963,1.5707963,1.5707963,0.0,
    			7.1268857E-1,1.0766740,1.2377165,1.3208073,1.3707968,1.4041297,1.4279392,1.4457963,1.4596852,1.4707963,1.4798872,
    			1.4874630,1.4938732,1.4993678,1.5041297,1.5082963,1.5119728,1.5152408,1.5181647,1.5207963,0.0,1.1021823E-1,2.1785052E-1,
    			3.2065296E-1,4.1696113E-1,5.0577280E-1,5.8669378E-1,6.5980394E-1,7.2550221E-1,7.8436941E-1,8.3706480E-1,8.8425699E-1,
    			9.2658288E-1,9.6462644E-1,9.9891032E-1,1.0298952,1.0579833,1.0835238,1.1068188,1.1281298,1.1476826,0.0,5.2534916E-2,
    			1.0478222E-1,1.5646359E-1,2.0731859E-1,2.5711185E-1,3.0563846E-1,3.5272734E-1,3.9824258E-1,4.4208304E-1,4.8418043E-1,
    			5.2449634E-1,5.6301861E-1,5.9975739E-1,6.3474116E-1,6.6801292E-1,6.9962679E-1,7.2964489E-1,7.5813479E-1,7.8516732E-1,
    			8.1081483E-1};
    	int i;
    	int errorsDetected = 0;
    	boolean realErrorDetected;
    	for (i = 0; i < realXtest.length; i++) {
    		realX = realXtest[i];
    		imagX = imagXtest[i];
    		realErrorDetected = false;
    		complexPsi();
    		Preferences.debug("realX = " + realX + " imagX = " + imagX + " realResult = " + realResult[0] + " realAnswer = " + realAnswer[i] +     		
	        		" imagResult = " + imagResult[0] + " imagAnswer = " + imagAnswer[i] + "\n",Preferences.DEBUG_ALGORITHM);
    		if (realAnswer[i] != 0.0) {
		        if ((realResult[0]/realAnswer[i] < 1-1.0E-7) || (realResult[0]/realAnswer[i] > 1+1.0E-7) || (Double.isNaN(realResult[0]))) {
		     		   Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;
		     		   realErrorDetected = true;
		        }
	        }
	        else {
	        	if (Math.abs(realResult[0]) > 1.0E-7) {
	        		Preferences.debug("Error detected in real part\n",Preferences.DEBUG_ALGORITHM);
		     		   errorsDetected++;
		     		   realErrorDetected = true;
	        	}
	        }
	        
	        if (imagAnswer[i] != 0.0) {
		        if ((imagResult[0]/imagAnswer[i] < 1-1.0E-7) || (imagResult[0]/imagAnswer[i] > 1+1.0E-7) || (Double.isNaN(imagResult[0]))) {
		     		   Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
		     		   if (!realErrorDetected) {
		     		       errorsDetected++;
		     		   }
		        }
	        }
	        else {
	        	if (Math.abs(imagResult[0]) > 1.0E-7) {
	        		Preferences.debug("Error detected in imaginary part\n",Preferences.DEBUG_ALGORITHM);
	        		   if (!realErrorDetected) {
		     		       errorsDetected++;
	        		   }
	        	}
	        }
    	}
    	 Preferences.debug(errorsDetected + " errors detected in " + realXtest.length + " tests\n", Preferences.DEBUG_ALGORITHM);
         System.out.println(errorsDetected + " errors detected in " + realXtest.length + " tests");
    }

    /**
     * This code is a port of the FORTRAN routine PSI from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 58-59.
     */
    public void realPsi() {
        double xa;
        double eL;
        double s;
        int n;
        int k;
        double x2;
        final double a1 = -8.3333333333333333E-2;
        final double a2 = 8.3333333333333333E-3;
        final double a3 = -3.9682539682539683E-3;
        final double a4 = 4.1666666666666667E-3;
        final double a5 = -7.5757575757575758E-3;
        final double a6 = 2.1092796092796093E-2;
        final double a7 = -8.3333333333333333E-2;
        final double a8 = 0.4432598039215686;

        xa = Math.abs(x);
        eL = 0.5772156649015329;
        s = 0.0;
        if ( (x == (int) x) && (x <= 0.0)) {
            result[0] = Double.POSITIVE_INFINITY;
            return;
        } else if (xa == (int) xa) {
            // For |x| == n, psi(n) = -gamma + sum from k = 1 to n-1 of (1/k)
            n = (int) xa;
            for (k = 1; k <= n - 1; k++) {
                s = s + 1.0 / k;
            }
            result[0] = -eL + s;
        } // else if (xa == (int)xa)
        else if ( (xa + 0.5) == (int) (xa + 0.5)) {
            // For |x| = n + 1/2, psi(n+1/2) = -gamma - 2ln2 + sum from k = 1 to n of
            // (2/(2k-1))
            n = (int) (xa - 0.5);
            for (k = 1; k <= n; k++) {
                s = s + 1.0 / (2.0 * k - 1.0);
            }
            result[0] = -eL + 2.0 * s - 1.386294361119891;
        } // else if ((xa + 0.5) == (int)(xa + 0.5))
        else {
            if (xa < 10.0) {
                // When |x| < 10, add an integer to |x| such that |x| + n > 10
                n = 10 - (int) xa;
                for (k = 0; k <= n - 1; k++) {
                    // psi(z+n) = psi(z) + sum from k = 0 to n-1 of (1/(z+k)) for n = 1, 2, 3,...
                    s = s + 1.0 / (xa + k);
                }
                xa = xa + n;
            } // if (xa < 10.0)
            x2 = 1.0 / (xa * xa);
            // Calculate psi(|x| + n) using
            // When |z| -> infinity and |arg z| < PI,
            // psi(z) equal approximately ln(z) - 1/(2z) - 1/(12Z**2) + 1/(120z**4)
            // - 1/(252z**6) + 1/(240z**8) - 1/(132z**10) + 691/(32760z**12)
            // - 1/(12z**14) + 3617/(8160z**16)
            result[0] = Math.log(xa) - 0.5 / xa + x2
                    * ( ( ( ( ( ( (a8 * x2 + a7) * x2 + a6) * x2 + a5) * x2 + a4) * x2 + a3) * x2 + a2) * x2 + a1);
            // Calculate psi|x| using
            // psi(z+n) = psi(z) + sum from k = 0 to n-1 of (1/(z+k)) for n = 1, 2, 3,...
            result[0] = result[0] - s;
        } // else
        if (x < 0.0) {
            result[0] = result[0] - Math.PI * Math.cos(Math.PI * x) / Math.sin(Math.PI * x) - 1.0 / x;
        }
        return;
    } // realPsi
    
    /**
     * This code is a port of the FORTRAN routine CPSI from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 59-60.
     */
    public void complexPsi() {
        double a[] = new double [] {-8.3333333333333333E-2, 8.3333333333333333E-3,
                                    -3.9682539682539683E-3, 4.1666666666666667E-3,
                                    -7.5757575757575758E-3, 2.1092796092796093E-2,
                                    -8.3333333333333333E-2, 0.4432598039215686};
        double x1 = 0.0;
        double y1 = 0.0;
        double x0;
        int n = 0;
        double th = 0.0;
        double z2;
        double z0;
        int k;
        double rr;
        double ri;
        double tn;
        double tm;
        double ct2;
        
        if (imagX == 0.0 && realX == (int)realX && realX <= 0.0) {
            realResult[0] = Double.POSITIVE_INFINITY;
            imagResult[0] = 0.0;
        }
        else {
            if (realX < 0.0) {
                // When Re(z) < 0, let z = -z
                x1 = realX;
                y1 = imagX;
                realX = -realX;
                imagX = -imagX;
            } // if (realX < 0.0)
            x0 = realX;
            if (realX < 8.0) {
                // When realX < 8, add an integer to realX such that realX + n > 8
                n = 8 - (int)realX;
                x0 = realX + n;
            } // if (realX < 8.0)
            if (x0 == 0.0 && imagX != 0.0) {
                th = 0.5*Math.PI;
            }
            if (x0 != 0.0) {
                th = Math.atan(imagX/x0);
            }
            z2 = x0*x0 + imagX*imagX;
            z0 = Math.sqrt(z2);
            // Calculate psi(|x| + n) using
            // When |z| -> infinity and |arg z| < PI, 
            // psi(z) equal approximately ln(z) - 1/(2z) - 1/(12Z**2) + 1/(120z**4) 
            // - 1/(252z**6) + 1/(240z**8) - 1/(132z**10) + 691/(32760z**12) 
            // - 1/(12z**14) + 3617/(8160z**16)
            realResult[0] = Math.log(z0)-0.5*x0/z2;
            imagResult[0] = th + 0.5*imagX/z2;
            for (k = 1; k <= 8; k++) {
                realResult[0] = realResult[0] + a[k-1]*Math.pow(z2,-k)*Math.cos(2.0*k*th);
                imagResult[0] = imagResult[0] - a[k-1]*Math.pow(z2,-k)*Math.sin(2.0*k*th);
            }
            if (realX < 8.0) {
                // Calculate psi(|x| + n) using
                // When |z| -> infinity and |arg z| < PI, 
                // psi(z) equal approximately ln(z) - 1/(2z) - 1/(12Z**2) + 1/(120z**4) 
                // - 1/(252z**6) + 1/(240z**8) - 1/(132z**10) + 691/(32760z**12) 
                // - 1/(12z**14) + 3617/(8160z**16)
                rr = 0.0;
                ri = 0.0;
                for (k = 1; k <= n; k++) {
                    rr = rr + (x0-k)/((x0-k)*(x0-k) + imagX*imagX);
                    ri = ri + imagX/((x0-k)*(x0-k) + imagX*imagX);
                }
                realResult[0] = realResult[0] - rr;
                imagResult[0] = imagResult[0] + ri;
            } // if (realX < 8.0)
            if (x1 < 0.0) {
                // When realX < 0, apply
                // psi(-z) = psi(z) + 1/2 + pi*cot(pi*z)
                tn = Math.tan(Math.PI*realX);
                tm = Math.tanh(Math.PI*imagX);
                ct2 = tn*tn + tm*tm;
                realResult[0] = realResult[0] + realX/(realX*realX + imagX*imagX)
                                + Math.PI*(tn - tn*tm*tm)/ct2;
                imagResult[0] = imagResult[0] - imagX/(realX*realX + imagX*imagX)
                                - Math.PI*tm*(1.0+tn*tn)/ct2;
                realX = x1;
                imagX = y1;
            } // if (x1 < 0.0)
        } // else
        return;
    } // complexPsi


}
