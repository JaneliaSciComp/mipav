package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.*;


/**
 * 
 * @author ilb
 * The code for EllipticIntegralType = COMPLETE1, COMPLETE2, INCOMPLETE1, and INCOMPLETE2 is ported from a C++ program in 
 * Figures 1 and 2 of "Numerical Calculation of the Elliptic Integrals of the First and Second Kinds with Complex Modulus"
 * by Tohru Morita, Interdisciplinary Information Sciences, Vol. 6, No. 1, 2000, pp. 67-74.
 */

public class EllipticIntegral {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    // K(k)
    public static final int COMPLETE1 = 1;
    
    // E(k)
    public static final int COMPLETE2 = 2;
    
    // F(phi, k)
    public static final int INCOMPLETE1 = 3;
    
    // E(phi, k)
    public static final int INCOMPLETE2 = 4;
    
    //  ~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Number of repetitions = N - 1
    private int N = 12; // 6
    
    private double TINY = 3.0E-138;  // 3.0E-38
    
    private double BIG = 3.0E137; // 3.0E37
    
    private double C1 = 1.0/24.0;
    
    private double C2 = 0.1;
    
    private double C3 = 3.0/44.0;
    
    private double C4 = 1.0/14.0;
    
    private double TINYd = 1.0E-125; // 1.0E-25
    
    private double BIGd = 4.5E121; // 4.5E21
    
    private double C1d = 3.0/14.0;
    
    private double C2d = 1.0/6.0;
    
    private double C3d = 9.0/22.0;
    
    private double C4d = 3.0/26.0;
    
    private double C5d = 0.25 * C3d;
    
    private double C6d = 1.5 * C4d;
    
    private double pihf = 0.5 * Math.PI;
    
    private double RERR = 1.0E-12; // 1.0E-6
    
    private double ERRTOL = 8.0E-3; // 8.0E-2
    
    private double ERRTOLb = 1.5E-6; // 1.5E-3
    
    private int EllipticIntegralType;
    
    private int[] errorFlag;
    
    //  ~ Constructors ---------------------------------------------------------------------------------------------------
    
    public EllipticIntegral(int EllipticIntegralType, int[] errorFlag) {
        this.EllipticIntegralType = EllipticIntegralType;
        this.errorFlag = errorFlag;
    }
    
    public void run() {
        
        if (errorFlag == null) {
            MipavUtil.displayError("Array errorFlag must not be null");

            return;
        }
        
        if ((EllipticIntegralType != COMPLETE1) && (EllipticIntegralType != COMPLETE2) &&
            (EllipticIntegralType != INCOMPLETE1) && (EllipticIntegralType != INCOMPLETE2)) {
            MipavUtil.displayError("Elliptic Intgral Type must be COMPLETE1, COMPLETE2, INCOMPLETE1, or INCOMPLETE2");
            errorFlag[0] = 1;
            return;
        }
    }

}
