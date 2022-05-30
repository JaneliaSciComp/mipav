package gov.nih.mipav.model.algorithms;

import de.jtem.mfc.field.Complex;
import de.jtem.mfc.specialFunctions.HyperGeometric2F1;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


public class Statistics {
    
    /** Also known as the normal probability function */
    public static final int GAUSSIAN_PROBABILITY_FUNCTION = 1;
    
    public static final int GAUSSIAN_PROBABILITY_INTEGRAL = 2;
    
    public static final int GAUSSIAN_INVERSE_CUMULATIVE_DISTRIBUTION_FUNCTION = 3;
    
    public static final int STUDENTS_T_DISTRIBUTION_PROBABILITY_DENSITY_FUNCTION = 4;
    
    public static final int STUDENTS_T_DISTRIBUTION_CUMULATIVE_DISTRIBUTION_FUNCTION = 5;
    
    public static final int STUDENTS_T_INVERSE_CUMULATIVE_DISTRIBUTION_FUNCTION = 6;
    
    public static final int CHI_SQUARED_PROBABILITY_DENSITY_FUNCTION = 7;
    
    public static final int CHI_SQUARED_CUMULATIVE_DISTRIBUTION_FUNCTION = 8;
    
    private int statisticType;
    
    private double x;
    
    private double v;
    
    private double ansR[];
    
    public Statistics(int statisticType, double x, double degreesOfFreedom, double ansR[]) {
        this.statisticType = statisticType;
        this.x = x;
        v = degreesOfFreedom;
        this.ansR = ansR;
    }
    
    public void run() {
        switch(statisticType) {
            case GAUSSIAN_PROBABILITY_FUNCTION:
                gaussianProbabilityFunction();
                break;
            case GAUSSIAN_PROBABILITY_INTEGRAL:
                gaussianProbabilityIntegral();
                break;
            case GAUSSIAN_INVERSE_CUMULATIVE_DISTRIBUTION_FUNCTION:
            	GaussianInverseCumulativeDistributionFunction();
            	break;
            case STUDENTS_T_DISTRIBUTION_PROBABILITY_DENSITY_FUNCTION:
                tProbabilityFunction();
                break;
            case STUDENTS_T_DISTRIBUTION_CUMULATIVE_DISTRIBUTION_FUNCTION:
                tCumulativeDistributionFunction();
                break;
            case STUDENTS_T_INVERSE_CUMULATIVE_DISTRIBUTION_FUNCTION:
            	tInverseCumulativeDistributionFunction();
            	break;
            case CHI_SQUARED_PROBABILITY_DENSITY_FUNCTION:
                chiSquaredProbabilityDensityFunction();
                break;
            case CHI_SQUARED_CUMULATIVE_DISTRIBUTION_FUNCTION:
                chiSquaredCumulativeDistributionFunction();
                break;
        }
        return;
    }
    
    private void gaussianProbabilityFunction() {
        ansR[0] = (1.0/Math.sqrt(2.0*Math.PI))*Math.exp(-x*x/2.0);
        return;
    }
    
    /** 
     * Formula found in Computation of Special Functions by Shanjie Zhang and Jianming Jin Chapter 16.3
     * Gaussian Probability Integral page 625.
     * |error(x)| <= 7.5E-8
     *
     */
    private void gaussianProbabilityIntegral() {
        double t;
        t = 1.0/(1.0 + 0.2316419*Math.abs(x));
        ansR[0] = 1.0 - (1.0/Math.sqrt(2.0*Math.PI))*Math.exp(-x*x/2.0)*t*((((1.330274429*t - 1.821255978)*t
                  + 1.781477937)*t - 0.356563782)*t + 0.31938153);
        if (x < 0.0) {
            ansR[0] = 1.0 - ansR[0];
        }
        return;
    }
    
    private void tProbabilityFunction() {
         Gamma gamm;
         double g1[] = new double[1];
         double g2[] = new double[1];
         gamm = new Gamma((v + 1.0)/2.0, g1);
         gamm.run();
         // For large degrees of freedom, the t distribution approaches the normal distribution
         if (Double.isInfinite(g1[0])) {
          gaussianProbabilityFunction();
          return;
         }
         gamm = new Gamma(v/2.0, g2);
         gamm.run();
         ansR[0] = (g1[0]/(Math.sqrt(v*Math.PI)*g2[0]))*Math.pow(1.0 + x*x/v, -(v+1.0)/2.0);
         return;
    }
    
    private void tCumulativeDistributionFunction() {
        Gamma gamm;
        double g1[] = new double[1];
        double g2[] = new double[1];
        Hypergeometric hyper;
        double result[] = new double[1];
        gamm = new Gamma((v + 1.0)/2.0, g1);
        gamm.run();
        // For large degrees of freedom, the t distribution approaches the normal distribution
        if (Double.isInfinite(g1[0])) {
         gaussianProbabilityIntegral();
         return;
        }
        gamm = new Gamma(v/2.0, g2);
        gamm.run();
        hyper = new Hypergeometric(0.5, (v + 1.0)/2.0, 1.5, -x*x/v, result);
        hyper.run();
        ansR[0] = 0.5 + x*(g1[0]/(Math.sqrt(v*Math.PI)*g2[0]))*result[0];
        // In fact JTEM produces error message Z is not inner point of the unit disc when I run circle generation.
        // JTEM Solution (Produces same result testR.getRe() = result[0].
        //Complex testR = HyperGeometric2F1.evaluateSeries( new Complex(0.5), new Complex((v + 1.0)/2.0),
        		//new Complex(1.5), new Complex(-x*x/v) );
        //System.err.println( result[0] + "   " + testR.getRe() );
        return;
    }
    
    private void GaussianInverseCumulativeDistributionFunction() {
    	double desiredtCum = x;
    	double epsilon = 1.0E-10;
    	x = 1.0;
    	double xlow = 1.0;
    	double xhigh = 1.0;
    	double tCum;
    	gaussianProbabilityIntegral();
    	tCum = ansR[0];
    	if (Math.abs(tCum-desiredtCum) < epsilon) {
    		ansR[0] = x;
    		return;
    	}
    	if (tCum < desiredtCum) {
    		while (tCum < desiredtCum) {
    			xlow = x;
    			x *= 2.0;
    			gaussianProbabilityIntegral();
    	    	tCum = ansR[0];
    		}
    		xhigh = x;
    	}
    	else {
    		while (tCum > desiredtCum) {
    			xhigh = x;
    			x /= 2.0;
    			gaussianProbabilityIntegral();
    	    	tCum = ansR[0];
    		}
    		xlow = x;
    	}
    	while (Math.abs(tCum - desiredtCum) >= epsilon) {
    		x = Math.sqrt(xlow * xhigh);
    		gaussianProbabilityIntegral();
	    	tCum = ansR[0];
    		if (tCum < desiredtCum) {
    			xlow = x;
    		}
    		else {
    			xhigh = x;
    		}
    	}
    	ansR[0] = x;
    	return;
    }
    
    private void tInverseCumulativeDistributionFunction() {
    	double desiredtCum = x;
    	double epsilon = 1.0E-10;
    	x = 1.0;
    	double xlow = 1.0;
    	double xhigh = 1.0;
    	double tCum;
    	tCumulativeDistributionFunction();
    	tCum = ansR[0];
    	if (Math.abs(tCum-desiredtCum) < epsilon) {
    		ansR[0] = x;
    		return;
    	}
    	if (tCum < desiredtCum) {
    		while (tCum < desiredtCum) {
    			xlow = x;
    			x *= 2.0;
    			tCumulativeDistributionFunction();
    	    	tCum = ansR[0];
    		}
    		xhigh = x;
    	}
    	else {
    		while (tCum > desiredtCum) {
    			xhigh = x;
    			x /= 2.0;
    			tCumulativeDistributionFunction();
    	    	tCum = ansR[0];
    		}
    		xlow = x;
    	}
    	while (Math.abs(tCum - desiredtCum) >= epsilon) {
    		x = Math.sqrt(xlow * xhigh);
    		tCumulativeDistributionFunction();
	    	tCum = ansR[0];
    		if (tCum < desiredtCum) {
    			xlow = x;
    		}
    		else {
    			xhigh = x;
    		}
    	}
    	ansR[0] = x;
    	return;
    }
    
    
    
    private void chiSquaredProbabilityDensityFunction() {
        Gamma gamm;
        double g1[] = new double[1];
        if (x <= 0) {
            ansR[0] = 0.0;
        }
        else {
            gamm = new Gamma(v/2.0, g1);
            gamm.run();
            ansR[0] = (1.0/(Math.pow(2.0, v/2.0)* g1[0]))*Math.pow(x,v/2.0 - 1.0)*Math.exp(-x/2.0);
        }
        return;
    }
    
    private void chiSquaredCumulativeDistributionFunction() {
        Gamma gamm;
        double lowerIncompleteGamma[] = new double[1];
        double upperIncompleteGamma[] = new double[1];
        double regularizedGammaP[] = new double[1];
        gamm = new Gamma(v/2.0, x/2.0, lowerIncompleteGamma, upperIncompleteGamma, regularizedGammaP);
        gamm.run();
        ansR[0] = regularizedGammaP[0];
        return;
    }
}