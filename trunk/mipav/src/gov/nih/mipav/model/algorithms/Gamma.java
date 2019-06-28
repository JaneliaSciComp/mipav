package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


/**
 * <p>
 * This code calculates the gamma function of an input argument x.
 * </p>
 * 
 * <p>
 * A typical usage would be: <blockquote>
 * 
 * <pre>
 * double result[] = new double[1];
 * Gamma gammaTest = new Gamma(5.0, result);
 * gammaTest.run();
 * Preferences.debug(&quot;Gamma(5.0) = &quot; + result[0] + &quot;\n&quot;);
 * UI.setDataText(&quot;Gamma(5.0) = &quot; + result[0] + &quot;\n&quot;);
 * </pre>
 * 
 * </blockquote>
 * </p>
 * 
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
public class Gamma {

    /** Compute gamma function for real x, x is not equal to 0 or a negative integer */
    public static final int GAMMA = 1;

    /** Compute gamma function or log of gamma function for real x > 0 */
    public static final int LGAMMA = 2;

    /** Compute gamma function or log of gamma function for complex arguments */
    public static final int CGAMMA = 3;

    /** Compute incompete gamma functions and regularized gamma P function */
    public static final int INCOG = 4;

    /**
     * input argument Note that the gamma function is not defined for x equal to zero or a negative integer
     */
    private double x;

    /** Real part of input argument */
    @SuppressWarnings("unused")
    private double realX;

    /** Imaginary part of input argument */
    @SuppressWarnings("unused")
    private double imagX;

    /**
     * In lgamma and cgamma calculate gamma(x) for functionCode = 1 and calulate the ln(gamma(x)) for functionCode = 0
     */
    private int functionCode;

    /** output result */
    private double result[];

    /** Real part of outputted result */
    @SuppressWarnings("unused")
    private double realResult[];

    /** Imaginary part of outputted result */
    @SuppressWarnings("unused")
    private double imagResult[];

    /** GAMMA, LGAMMA, CGAMMA, or INCOG */
    private int version;

    private double a;

    private double lowerIncompleteGamma[];

    private double upperIncompleteGamma[];

    private double regularizedGammaP[];
    
    public Gamma() {
    	
    }

    /**
     * 
     * @param x Input argument
     * @param result outputted gamma(x)
     */
    public Gamma(final double x, final double result[]) {
        this.x = x;
        this.result = result;
        this.version = Gamma.GAMMA;
    }

    /**
     * 
     * @param x input argument, must have x > 0
     * @param functionCode 1 for gamma(x), 0 for ln(gamma(x))
     * @param result outputted gamma(x) or ln(gamma(x))
     */
    public Gamma(final double x, final int functionCode, final double result[]) {
        this.x = x;
        this.functionCode = functionCode;
        this.result = result;
        this.version = Gamma.LGAMMA;
    }

    /**
     * 
     * @param realX real part of input argument
     * @param imagX imaginary part of input argument
     * @param functionCode 1 for gamma(x),0 for ln(gamma(x))
     * @param realResult real part of outputted gamma(x) or ln(gamma(x))
     * @param imagResult imaginary part of outputted gamma(x) or ln(gamma(x))
     */
    public Gamma(final double realX, final double imagX, final int functionCode, final double realResult[],
            final double imagResult[]) {
        this.realX = realX;
        this.imagX = imagX;
        this.functionCode = functionCode;
        this.realResult = realResult;
        this.imagResult = imagResult;
        this.version = Gamma.CGAMMA;
    }

    /**
     * 
     * @param a
     * @param x
     * @param lowerIncompleteGamma
     * @param upperIncompleteGamma
     * @param regularizedGammaP
     */
    public Gamma(final double a, final double x, final double lowerIncompleteGamma[],
            final double upperIncompleteGamma[], final double regularizedGammaP[]) {
        this.a = a;
        this.x = x;
        this.lowerIncompleteGamma = lowerIncompleteGamma;
        this.upperIncompleteGamma = upperIncompleteGamma;
        this.regularizedGammaP = regularizedGammaP;
        this.version = Gamma.INCOG;

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
    
    public void testgamma() {
       result = new double[1];
       double xtest[] = new double[]{0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,
    		   2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4.0,4.1,4.2,4.3,4.4,4.5,
    		   4.6,4.7,4.8,4.9,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,
    		   23.0,24.0,25.0,26.0,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,
    		   54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,
    		   88,89,90,91,92,93,94,95,96,97,98,99,100,101,111,121,131,141,151,161,171,-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,
    		   -0.7,-0.8,-0.9,-1.1,-1.2,-1.3,-1.4,-1.5,-1.6,-1.7,-1.8,-1.9,-2.1,-2.2,-2.3,-2.4,-2.5,-2.6,-2.7,-2.8,
    		   -2.9,-3.1,-3.2,-3.3,-3.4,-3.5,-3.6,-3.7,-3.8,-3.9,-4.1,-4.2,-4.3,-4.4,-4.5,-4.6,-4.7,-4.8,-4.9};
       double answer[] = new double[]{9.51350770,4.59084371,2.99156899,2.21815954,1.77245385,1.48919225,1.29805533,
    		   1.16422971,1.06862870,1.0,.95135077,.91816874,.89747070,.88726382,.88622693,.89351535,.90863873,
    		   .93138377,.96176583,1.0,1.04648585,1.10180249,1.16671191,1.24216934,1.32934039,1.42962456,1.54468585,
    		   1.67649079,1.82735508,2.0,2.19762028,2.42396548,2.68343738,2.98120643,3.32335097,3.71702385,
    		   4.17065178,4.69417421,5.29932973,6.0,6.81262286,7.75668954,8.85534336,10.13610185,11.63172840,
    		   13.38128587,15.43141160,17.83786198,20.66738596,24.0,120.0,720.0,5.04E3,4.032E4,3.6288E5,3.6288E6,
    		   3.99168E7,4.790016E8,6.2270208E9,8.71782912E10,1.307674368E12,2.0922789888E13,3.55687428096E14,
    		   6.402373705728E15,1.21645100408832E17,2.43290200817664E18,5.109094217170944E19,1.12400072777760768E21,
    		   2.585201673888497664E22,6.2044840173323943936E23,1.5511210043330985984E25,4.03291461126605635584E26,
    		   1.08888694504183521608E28,3.04888344611713860502E29,8.84176199373970195454E30,2.65252859812191058636E32,
    		   8.22283865417792281773E33,2.63130836933693530167E35,8.68331761881188649552E36,2.95232799039604140848E38,
    		   1.03331479663861449297E40,3.71993326789901217468E41,1.37637530912263450463E43,5.2302261746660111176E44,
    		   2.03978820811974433586E46,8.15915283247897734366E47,3.34525266131638071082E49,1.40500611775287989854E51,
    		   6.04152630633738356374E52,2.65827157478844876804E54,1.19622220865480194562E56,5.50262215981208894985E57,
    		   2.58623241511168180643E59,1.24139155925360726709E61,6.08281864034267560872E62,3.04140932017133780436E64,
    		   1.55111875328738228022E66,8.06581751709438785717E67,4.2748832840600255643E69,2.30843697339241380472E71,
    		   1.2696403353658275926E73,7.10998587804863451854E74,4.05269195048772167557E76,2.35056133128287857183E78,
    		   1.38683118545689835738E80,8.32098711274139014428E81,5.07580213877224798801E83,3.14699732603879375257E85,
    		   1.98260831540444006412E87,1.26886932185884164103E89,8.24765059208247066672E90,5.44344939077443064004E92,
    		   3.64711109181886852882E94,2.4800355424368305996E96,1.71122452428141311372E98,1.19785716699698917961E100,
    		   8.50478588567862317521E101,6.12344583768860868615E103,4.47011546151268434089E105,3.307885441561938641226E107,
    		   2.48091408113953980919E109,1.88549470166605025499E111,1.45183092028285869634E113,1.13242811782062978315E115,
    		   8.94618213078297528685E116,7.15694570462638022948E118,5.79712602074736798588E120,4.75364333701284174842E122,
    		   3.94552396972065865119E124,3.314240134565353267E126,2.81710411438055027695E128,2.42270953836727323818E130,
    		   2.10775729837952771721E132,1.85482642257398439115E134,1.65079551609084610812E136,1.48571596448176149731E138,
    		   1.35200152767840296255E140,1.24384140546413072555E142,1.15677250708164157476E144,1.08736615665674308027E146,
    		   1.03299784882390592626E148,9.91677934870949689210E149,9.61927598824821198533E151,9.42689044888324774563E153,
    		   9.33262154439441526817E155,9.33262154439441526817E157,1.58824554152274294042E178,6.68950291344912705758E198,
    		   6.46685548922047367250E219,1.34620124757175246058E241,5.71338395644585459047E262,4.71472363599206132240E284,
    		   7.25741561530799896739E306,-10.68628702,-5.82114857,-4.32685111,-3.72298062,-3.54490770,-3.69693257,
    		   -4.27366998,-5.73855464,-10.57076411,9.71480638,4.85095714,3.32834701,2.65927187,2.36327180,2.31058286,
    		   2.51392352,3.18808591,5.56345479,-4.62609828,-2.20498052,-1.44710739,-1.10802995,-.94530872,-.88868571,
    		   -.93108278,-1.13860211,-1.91843269,1.49228977,.68905641,.43851739,.32589116,.27008821,.24685714,.25164400,
    		   .29963213,.49190582,-.36397311,-.16406105,-.10198079,-.07406617,-.06001960,-.05366460,-.05354128,-0.6242336,
    		   -.10038894};
       int i;
       int errorsDetected = 0;
       for (i = 0; i < xtest.length; i++) {
    	   x = xtest[i];
    	   gamma();
    	   Preferences.debug("x = " + x + " result = " + result[0] + "answer = " + answer[i] + "\n", Preferences.DEBUG_ALGORITHM);
    	   if (Math.abs(result[0]-answer[i])/answer[i] >= 1.0E-8) {
    		   Preferences.debug("Error detected\n",Preferences.DEBUG_ALGORITHM);
    		   errorsDetected++;
    	   }
       }
       Preferences.debug(errorsDetected + " errors found in " + xtest.length + " runs of gamma()\n", Preferences.DEBUG_ALGORITHM);
       System.out.println(errorsDetected + " errors found in " + xtest.length + " runs of gamma()");
    }

    public void run() {
        if (version == Gamma.GAMMA) {
            gamma();
        } else if (version == Gamma.LGAMMA) {
            lgamma();
        } else if (version == Gamma.CGAMMA) {
            // cgamma();
        } else if (version == Gamma.INCOG) {
            incog();
        }
    }

    /**
     * This code is a port of the FORTRAN routine GAMMA from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 49-50.
     * x is not 0 or a negative integer
     */
    private void gamma() {
        int m1;
        int k;
        double z;
        int m;
        if (x > 171) {
        	MipavUtil.displayError("171 is the largest integer whose gamma value does not exceed Double.MAX_VALUE");
        	return;
        }
        double r = 1.0;
        final double g[] = new double[] {1.0, 0.5772156649015329, -0.6558780715202538, -0.420026350340952E-1,
                0.1665386113822915, -0.421977345555443E-1, -0.96219715278770E-2, 0.72189432466630E-2,
                -0.11651675918591E-2, -0.2152416741149E-3, 0.1280502823882E-3, -0.201348547807E-4, -0.12504934821E-5,
                0.11330272320E-5, -0.2056338417E-6, 0.61160950E-8, 0.50020075E-8, -0.11812746E-8, 0.1043427E-9,
                0.77823E-11, -0.36968E-11, 0.51E-12, -0.206E-13, -0.54E-14, 0.14E-14};
        double gr;
        if (x == (int) x) {
            if (x > 0.0) { // Use gamma(x) == (-1)! for x a positive integer
                result[0] = 1.0;
                m1 = (int) (x) - 1;
                for (k = 2; k <= m1; k++) {
                    result[0] = result[0] * k;
                }
            } // if (x > 0.0)
            else {
                result[0] = Double.POSITIVE_INFINITY; // for x 0 or a negative integer
            }
        } // if (x == (int)x)
        else {
            if (Math.abs(x) > 1.0) {
                // When abs(x) > 1, use gamma(z) = (z-1)gamma(z-1) =
                // (z-1)(z-2)...(z-n)gamma(z-n)
                z = Math.abs(x);
                m = (int) z;
                r = 1.0;
                for (k = 1; k <= m; k++) {
                    r = r * (z - k);
                }
                z = z - m;
            } // if (Math.abs(x) > 1.0)
            else {
                z = x;
            }
            gr = g[24];
            // Calculate 1/gamma(x) = sum from k = 1 to 25 of g[k-1]*z**k
            for (k = 23; k >= 0; k--) {
                gr = gr * z + g[k];
            }
            result[0] = 1.0 / (gr * z);
            if (Math.abs(x) > 1.0) {
                // When abs(x) > 1, use gamma(z) = (z-1)gamma(z-1) =
                // (z-1)(z-2)...(z-n)gamma(z-n)
                result[0] = result[0] * r;
                if (x < 0.0) {
                    // gamma(-z) = -PI/(z*gamma(z)*sin(PI*z)) for z not equal to an integer
                    result[0] = -Math.PI / (x * result[0] * Math.sin(Math.PI * x));
                } // if (x < 0.0)
            } // if (Math.abs(x) > 1.0)
        } // else
        return;
    } // gamma

    /**
     * This code is a port of the FORTRAN routine LGAMA from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 50-51. This routine calculates the gamma(x) for functionCode =
     * 1 or the ln(gamma(x)) for the function code = 0. x must be greater than zero.
     */
    private void lgamma() {
        final double a[] = new double[] {8.333333333333333E-2, -2.777777777777778E-3, 7.936507936507937E-4,
                -5.952380952380952E-4, 8.417508417508418E-4, -1.917526917526918E-3, 6.410256410256410E-3,
                -2.955065359477124E-2, 1.796443723688307E-1, -1.392432216905900};
        double x0;
        int n = 0;
        double x2;
        double xp;
        double gl0;
        int k;

        x0 = x;
        if ( (x == 1.0) || (x == 2.0)) {
            // Treat special case of x = 1 and 2
            result[0] = 0.0;
            if (functionCode == 1) {
                // Calculate gamma(x)
                result[0] = Math.exp(result[0]);
            }
            return;
        } // if ((x == 1.0) || (x == 2.0))
        else if (x <= 7.0) {
            // When x <= 7, add an integer to x such that x + n > 7
            n = (int) (7 - x);
            x0 = x + n;
        } // else if (x <= 7.0)
        x2 = 1.0 / (x0 * x0);
        // Calculate ln(gamma(x+n)) using
        // ln(gamma(z)), when |z| -> infinity and |arg z| < PI, approaches:
        // (z - 1/2)ln(z) - z + (1/2)ln(2*PI) + 1/(12*z) - 1/(360*z**3) + 1/(1260*z**5)
        // - 1/(1680*z**7) + 1/(1188*z**9) - 691/(360360*z**11) + 7/(1092*z**13)
        // - 3617/(122400*z**15) + ...
        xp = 2.0 * Math.PI;
        gl0 = a[9];
        for (k = 8; k >= 0; k--) {
            gl0 = gl0 * x2 + a[k];
        }
        result[0] = gl0 / x0 + 0.5 * Math.log(xp) + (x0 - 0.5) * Math.log(x0) - x0;
        if (x <= 7.0) {
            // Calculate ln(gamma(x)) using
            // gamma(z) = (z-1)gamma(z-1) = (z-1)*(z-2)...*(z-n)*gamma(z-n)
            for (k = 1; k <= n; k++) {
                result[0] = result[0] - Math.log(x0 - 1.0);
                x0 = x0 - 1.0;
            } // for (k = 1; k <= n; k++)
        } // if (x <= 7.0)
        if (functionCode == 1) {
            // Calculate gamma(x)
            result[0] = Math.exp(result[0]);
        } // if (functionCode == 1)
        return;
    } // lgamma

    /**
     * This code is a port of the FORTRAN routine CGAMA from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 51-52. This routine calculates the gamma(x) for functionCode =
     * 1 or the ln(gamma(x)) for the function code = 0. x is complex and the output is complex.
     */
    /*
     * private void cgamma() { double a[] = new double[]{8.333333333333333E-2, -2.777777777777778E-3,
     * 7.936507936507937E-4, -5.952380952380952E-4, 8.417508417508418E-4, -1.917526917526918E-3, 6.410256410256410E-3,
     * -2.955065359477124E-2, 1.796443723688307E-1, -1.392432216905900}; double x1 = 0.0; double y1 = 0.0; double x0;
     * int na = 0; double z1; double th; int k; double t; double gr1; double gi1; int j; double th1; double sr; double
     * si; double th2; double g0; double z2;
     * 
     * if (imagX == 0.0 && realX == (int)realX && realX <= 0.0) { realResult[0] = Double.POSITIVE_INFINITY;
     * imagResult[0] = 0.0; return; } // if (imagX == 0.0 && realX == (int)realX && realX <= 0.0) else if (realX < 0.0) { //
     * When realX < 0.0, let x = -x x1 = realX; y1 = imagX; realX = -realX; imagX = -imagX; } // else if (realX < 0.0)
     * x0 = realX; if (realX <= 7.0) { // When realX <= 7, add an integer to realX such that realX + na > 7 na = (int)(7 -
     * realX); x0 = realX + na; } // if (realX <= 7.0) // Calculate ln(gamma(x+n)) using // ln(gamma(z)), when |z| ->
     * infinity and |arg z| < PI, approaches: // (z - 1/2)ln(z) - z + (1/2)ln(2*PI) + 1/(12*z) - 1/(360*z**3) +
     * 1/(1260*z**5) // - 1/(1680*z**7) + 1/(1188*z**9) - 691/(360360*z**11) + 7/(1092*z**13) // - 3617/(122400*z**15) +
     * ... z1 = Math.sqrt(x0*x0 + imagX*imagX); th = Math.atan(imagX/x0); realResult[0] = (x0-0.5)*Math.log(z1) -
     * th*imagX - x0 + 0.5*Math.log(2.0*Math.PI); imagResult[0] = th*(x0-0.5) + imagX*Math.log(z1) - imagX; for (k = 1;
     * k <= 10; k++) { t = Math.pow(z1,(1-2*k)); realResult[0] = realResult[0] + a[k-1]*t*Math.cos((2.0*k-1.0)*th);
     * imagResult[0] = imagResult[0] - a[k-1]*t*Math.sin((2.0*k-1.0)*th); } // for (k = 1; k <= 10; k++) if (realX <=
     * 7.0) { // Calculate ln(gamma(x)) using // gamma(z) = (z-1)gamma(z-1) = (z-1)*(z-2)...*(z-n)*gamma(z-n) gr1 = 0.0;
     * gi1 = 0.0; for (j = 0; j <= na-1; j++) { gr1 = gr1 + 0.5*Math.log((realX+j)*(realX+j) + imagX*imagX); gi1 = gi1 +
     * Math.atan(imagX/(realX+j)); } // for (j = 0; j <= na-1; j++) realResult[0] = realResult[0] - gr1; imagResult[0] =
     * imagResult[0] - gi1; } // if (realX <= 7.0) if (x1 < 0.0) { // When realX < 0, use gamma(-z) =
     * -pi/(z*gamma(z)*sin(PI*z)) for // z not equal to 0, +-1, +-2, ... z1 = Math.sqrt(realX*realX + imagX*imagX); th1 =
     * Math.atan(imagX/realX); sr = -Math.sin(Math.PI*realX)*Math.cosh(Math.PI*imagX); si =
     * -Math.cos(Math.PI*realX)*Math.sinh(Math.PI*imagX); z2 = Math.sqrt(sr*sr + si*si); th2 = Math.atan(si/sr); if (sr <
     * 0.0) { th2 = Math.PI + th2; } realResult[0] = Math.log(Math.PI/(z1*z2)) - realResult[0]; imagResult[0] = -th1 -
     * th2 - imagResult[0]; realX = x1; imagX = y1; } // if (x1 < 0.0) if (functionCode == 1) { // Calculate gamma(x) g0 =
     * Math.exp(realResult[0]); realResult[0] = g0*Math.cos(imagResult[0]); imagResult[0] = g0*Math.sin(imagResult[0]); } //
     * if (functionCode == 1) return; } // cgamma
     */

    /**
     * This code is a port of the FORTRAN routine INCOG from the book Computation of Special Functions by Shanjie Zhang
     * and Jianming Jin, John Wiley & Sons, Inc., 1996, pp. 63-64. It caclulates the incomplete gamma functions and the
     * regularized gamma function P. For a > 100 use code deried from Numercial Recipes The Art of Scientific Computing
     * Third Edition Chapter 6 Special Functions from routine gammpapprox P. 262.
     */
    private void incog() {
        double xam;
        Gamma gamm;
        final double ga[] = new double[1];
        double s;
        double r;
        int k;
        double t0;
        double a1;
        double lna1;
        double sqrta1;
        final double gln[] = new double[1];
        double xu;
        double sum;
        int j;
        double t;
        double ans;
        final double y[] = new double[] {0.0021695375159141994, 0.011413521097787704, 0.027972308950302116,
                0.051727015600492421, 0.082502225484340941, 0.12007019910960293, 0.16415283300752470,
                0.21442376986779355, 0.27051082840644336, 0.33199876341447887, 0.39843234186401943,
                0.46931971407375483, 0.54413605556657973, 0.62232745288031077, 0.70331500465597174,
                0.78649910768313447, 0.87126389619061517, 0.95698180152629142};
        final double w[] = new double[] {0.0055657196642445571, 0.012915947284065419, 0.020181515297735382,
                0.027298621498568734, 0.034213810770299537, 0.040875750923643261, 0.047235083490265582,
                0.053244713977759692, 0.058860144245324798, 0.064039797355015485, 0.068745323835736408,
                0.072941885005653087, 0.076598410645870640, 0.079687828912071670, 0.082187266704339706,
                0.084078218979661945, 0.085346685739338721, 0.085983275670394821};
        if (a > 100.0) {
            a1 = a - 1.0;
            lna1 = Math.log(a1);
            sqrta1 = Math.sqrt(a1);
            gamm = new Gamma(a, 0, gln);
            gamm.run();
            if (x > a1) {
                xu = Math.max(a1 + 11.5 * sqrta1, x + 6.0 * sqrta1);
            } else {
                xu = Math.max(0.0, Math.min(a1 - 7.5 * sqrta1, x - 5.0 * sqrta1));
            }
            sum = 0.0;
            for (j = 0; j < 18; j++) {
                t = x + (xu - x) * y[j];
                sum += w[j] * Math.exp( - (t - a1) + a1 * (Math.log(t) - lna1));
            }
            ans = sum * (xu - x) * Math.exp(a1 * (lna1 - 1.0) - gln[0]);
            if (x > a1) {
                regularizedGammaP[0] = 1.0 - ans;
            } else {
                regularizedGammaP[0] = -ans;
            }
            // ga[0], lowerIncompleteGamma[0], and upperIncompleteGamma[0] may be too large to compute
            gamm = new Gamma(a, ga);
            gamm.run();
            lowerIncompleteGamma[0] = regularizedGammaP[0] * ga[0];
            upperIncompleteGamma[0] = ga[0] - lowerIncompleteGamma[0];
            return;
        } // if (a > 100.0)
        xam = -x + a * Math.log(x);
        if (xam > 700.0) {
            MipavUtil.displayError("a and/or x too large in incog");
            return;
        }
        if (x == 0.0) {
            lowerIncompleteGamma[0] = 0;
            gamm = new Gamma(a, ga);
            gamm.run();
            upperIncompleteGamma[0] = ga[0];
            regularizedGammaP[0] = 0.0;
            return;
        } // if (x == 0.0)
        else if (x <= 1.0 + a) {
            s = 1.0 / a;
            r = s;
            for (k = 1; k <= 60; k++) {
                r = r * x / (a + k);
                s = s + r;
                if (Math.abs(r / s) < 1.0E-15) {
                    break;
                }
            } // for (k = 1; k <= 60; k++)
            lowerIncompleteGamma[0] = Math.exp(xam) * s;
            gamm = new Gamma(a, ga);
            gamm.run();
            regularizedGammaP[0] = lowerIncompleteGamma[0] / ga[0];
            upperIncompleteGamma[0] = ga[0] - lowerIncompleteGamma[0];
            return;
        } // else if (x <= 1.0+a)
        else { // else x > 1.0 + a
            t0 = 0.0;
            for (k = 60; k >= 1; k--) {
                t0 = (k - a) / (1.0 + k / (x + t0));
            } // for (k = 60; k >= 1; k--)
            upperIncompleteGamma[0] = Math.exp(xam) / (x + t0);
            gamm = new Gamma(a, ga);
            gamm.run();
            lowerIncompleteGamma[0] = ga[0] - upperIncompleteGamma[0];
            regularizedGammaP[0] = 1.0 - upperIncompleteGamma[0] / ga[0];
            return;
        } // else x > 1.0 + a
    }

}
