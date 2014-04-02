	package gov.nih.mipav.model.structures;

import gov.nih.mipav.util.DoubleDouble;
	
	/**
	 
	 *
	 * @version  0.1 April 2, 2014
	 * @author   William Gandler
	 * 
	 * This a a port of vnl_real_polynomial.h and vnl_real_polynomial.cxx.

  
	 */
	public class RealPolynomial  {
		
		private double coeff[];
	
		public RealPolynomial(double a) {
		    this(new double[]{a});	
		}
	    
	    public RealPolynomial(double a[]) {
	        coeff = a.clone();
	    }
	    
	    public RealPolynomial(RealPolynomial a) {
	    	this.coeff = a.coeff.clone();
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------
	    
	    public RealPolynomial multiply(RealPolynomial y) {
	    	 return (new RealPolynomial(this)).selfMultiply(y);
	    }
	    
	    private RealPolynomial selfMultiply(RealPolynomial y) {
	    	int d1 = coeff.length-1;
	    	int d2 = y.coeff.length-1;
	    	int d = d1 + d2;
	    	double sum[] = new double[d+1];
	    	
	    	for (int i = 0; i <= d1; i++) {
	    		for (int j = 0; j <= d2; j++) {
	    			sum[d - (i + j)] += (coeff[d1 - i] * y.coeff[d2-j]);
	    		}
	    	}
	        this.coeff = sum.clone();
	        return this;	
	    }
	    
	    public RealPolynomial add(RealPolynomial y) {
	    	 return (new RealPolynomial(this)).selfAdd(y);
	    }
	    
	    private RealPolynomial selfAdd(RealPolynomial y) {
	      int d1 = coeff.length - 1;
	      int d2 = y.coeff.length - 1;
	      int d = d1;
	      if (d2 > d) {
	    	  d = d2;
	      }
	      double sum[] = new double[d+1];
	      for (int i = 0; i <= d; i++) {
	    	  if (i <= d1) {
	    		  sum[d-i] += coeff[d1 - i];
	    	  }
	    	  if (i <= d2) {
	    		  sum[d-i] += y.coeff[d2-i];
	    	  }
	      }
	      this.coeff = sum.clone();
	      return this;
	    }
	    
	    public double[] getCoefficients() {
	    	return coeff;
	    }

	    /**
	     * Prepares this class for destruction.
	     */
	    public void finalize() {
	        
	    }
	    
	   
}

