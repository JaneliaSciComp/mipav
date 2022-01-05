package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


/**
 * <p>
 * This code calculates the Ksquared statistic of an input array x[].
 * KSquared = Z1(g1)**2 + Z2(g2)**2
 * If the null hypothesis of normality is true, then Ksquared is approximately
 * chiSquared distributed with 2 degrees of freedom
 *                Expected value       Standard deviation 95% quantile
 * n = 20         1.971                2.339              6.373
 * n = 50         2.017                2.308              6.339
 * n = 100        2.026                2.267              6.271
 * n = 250        2.012                2.174              6.129
 * n = 500        2.009                2.113              6.063
 * n = 1000       2.000                2.062              6.038
 * chSquared(2)   2.000                2.000              5.991
 * distribution
 * </p>
 * 
 * <p>
 * A typical usage would be: <blockquote>
 * 
 * <pre>
 * double result[] = new double[1];
 * DAgostinosKsquaredTest dkt = new DAgostinosKsquaredTest(x, result);
 * dkt.run();
 
 */
public class DAgostinosKsquaredTest {

    
    private double x[];

    /** output result */
    private double result[];
    
    public DAgostinosKsquaredTest() {
    	
    }

    /**
     * 
     * @param x Input argument
     * @param result outputted Ksquared test value of x
     */
    public DAgostinosKsquaredTest(final double x[], final double result[]) {
        this.x = x;
        this.result = result;
    }

    
    public void run() {
        long n = x.length;
        double nd = (double)n;
        double xsum = 0.0;
        double xmean = 0.0;
        int i;
        double xdiff;
        double xdiffSquared;
        double xdiffSquaredSum = 0.0;
        double xdiffCubed;
        double xdiffCubedSum = 0.0;
        double xdiffFourth;
        double xdiffFourthSum = 0.0;
        double m2;
        double m3;
        double m4;
        double g1;
        double g2;
        double mu2g1;
        double gamma2g1;
        double Wsquared;
        double W;
        double delta;
        double alphaSquared;
        double alpha;
        double arg;
        double Z1g1;
        double mu1g2;
        double mu2g2;
        double gamma1g2;
        double A;
        double arg2;
        double Z2g2;
        
        for (i = 0; i < n; i++) {
        	xsum += x[i];
        }
        
        xmean = (double)xsum/nd;
        for (i = 0; i < n; i++) {
        	xdiff = x[i] - xmean;
        	xdiffSquared = xdiff * xdiff;
        	xdiffSquaredSum += xdiffSquared;
        	xdiffCubed = xdiffSquared * xdiff;
        	xdiffCubedSum += xdiffCubed;
        	xdiffFourth = xdiffSquared * xdiffSquared;
        	xdiffFourthSum += xdiffFourth;
        }
        
        m2 = xdiffSquaredSum/nd;
        m3 = xdiffCubedSum/nd;
        m4 = xdiffFourthSum/nd;
        g1 = m3/Math.pow(m2,1.5);
        g2 = m4/(m2*m2) - 3.0;
        mu2g1 = 6.0*(nd - 2.0)/((nd+1.0)*(nd+3.0));
        gamma2g1 = 36.0*(nd-7.0)*(nd*nd + 2.0*nd - 5.0)/
        		((nd-2.0)*(nd+5.0)*(nd+7.0)*(nd+9.0));
        Wsquared = Math.sqrt(2.0*gamma2g1 + 4.0) - 1.0;
        W = Math.sqrt(Wsquared);
        delta = 1.0/Math.sqrt(Math.log(W));
        alphaSquared = 2.0/(Wsquared - 1.0);
        alpha = Math.sqrt(alphaSquared);
        arg = g1/(alpha*Math.sqrt(mu2g1));
        Z1g1 = delta * Math.log(arg + Math.sqrt(arg*arg + 1.0));
        
        mu1g2 = -6.0/(nd + 1.0);
        mu2g2 = 24.0*nd*(nd-2.0)*(nd-3.0)/
        		((nd+1.0)*(nd+1.0)*(nd+3.0)*(nd+5.0));
        arg = Math.sqrt(6.0*(nd+3.0)*(nd+5.0)/(nd*(nd-2.0)*(nd-3.0)));
        gamma1g2 = 6.0*(nd*nd - 5.0*nd + 2.0)*arg/
        		   ((nd+7.0)*(nd+9.0));
        A = 6.0 + (8.0/gamma1g2)*((2.0/gamma1g2) + Math.sqrt(1.0 + 4.0/(gamma1g2*gamma1g2)));
        arg = (1.0 - 2.0/A)/(1.0 + ((g2-mu1g2)/Math.sqrt(mu2g2))*Math.sqrt(2.0/(A-4.0)));
        arg2 = 9.0*A/2.0;
        Z2g2 = Math.sqrt(arg2)*(1.0 - 1.0/arg2 - Math.pow(arg, 1.0/3.0));
        
        result[0] = Z1g1*Z1g1 + Z2g2*Z2g2;
    }
    
    public void selfTest() {
    	// From www.statext.com/practice/NormalityTest04.php
    	x = new double[] {303,338,406,457,461,469,474,489,515,583};
    	result = new double[1];
    	run();
    	System.out.println("Obtained Ksquared value = " + result[0]);
    	System.out.println("Correct Ksquared value = 0.66");
    }


}
