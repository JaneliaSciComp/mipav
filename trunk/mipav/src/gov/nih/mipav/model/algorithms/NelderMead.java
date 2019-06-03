package gov.nih.mipav.model.algorithms;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import gov.nih.mipav.view.Preferences;

/**
* Nelder-Mead original code copyright 2017 by Matteo Maggioni under the MIT Licence.
* Ported from C to Java by William Gandler
	
	*/




public abstract class NelderMead 
{
 
 private double RHO = 1.0;
 private double CHI = 2.0;
 private double GAMMA = 0.5;
 private double SIGMA = 0.5;
 
 private int n; // the dimension of the data
 private double x[]; // initial point of dimension n (unchanged in output)
 // tolx, tolf, max_iter, and max_eval are the optimization settings
 private double tolx = 0.001; // tolerance on the simplex solutions coorindates
 private double tolf = 0.001; // tolerance on the function value coordinates
 private int max_iter = 1000; // maximum number of allowed iterations
 private int max_eval = 1000; // maximum number of allowed evaluations
 private boolean verbose = false; // toggle verbose output during minimization
 private boolean self_test = false;
 private final int ackleyNum = 1;
 private int costNum = ackleyNum;
 private param_t params;
 

  public NelderMead(int n, double x[], double tolx, double tolf, int max_iter, int max_eval, boolean verbose) 
  {
    this.n = n;
    this.x = x;
    this.tolx = tolx;
    this.tolf = tolf;
    this.max_iter = max_iter;
    this.max_eval = max_eval;
    this.verbose = verbose;
  }
    
  public void driver() { 
	  int i;
      point_t start;
      optimset_t optimset;
      point_t solution;
      
      start = new point_t();
      start.x = new double[n];
      for (i = 0; i < n; i++) {
    	  start.x[i] = x[i];
      }
      
      // cost function parameters
      if ((self_test) && (costNum == ackleyNum)) {
          params = new param_t(); // parameters of ackley cost function
          params.a = 20.0;
          params.b = 0.2;
          params.c = 2.0 * Math.PI;
      }
      
      // call optimization methods
      solution = new point_t(); // container for the solution of the optimization
      nelder_mead(start, solution);
      if (self_test) {
    	  System.out.println("Initial point");
    	  for (i = 0; i < n; i++) {
    		  System.out.println("x["+i+"] = " + start.x[i]);
    	  }
    	  System.out.println("Starting function value = " + start.fx);
    	  // Print solution
    	  System.out.println("Solution point");
    	  for (i = 0; i < n; i++) {
    		  System.out.println("x["+i+"] = " + solution.x[i]);
    	  }
    	  System.out.println("Solution function value = " + solution.fx);
    	  
    	  // Free memory
    	  start.x = null;
    	  solution.x = null;
    	  start = null;
    	  solution = null;
    	  return;
      }
      
  }
  
 private void nelder_mead(point_t start, point_t solution) {
     // internal points
	 point_t point_r = new point_t();
	 point_t point_e = new point_t();
	 point_t point_c = new point_t();
	 point_t centroid = new point_t();
	 
	 // allocate memory for internal points
	 point_r.x = new double[n];
	 point_e.x = new double[n];
	 point_c.x = new double[n];
	 centroid.x = new double[n];
	 
	 int iter_count = 0;
	 int eval_count = 0;
	 
	 // initial simplex has size n + 1 where n is the dimensionality of the data
	 simplex_t simplex = new simplex_t();
	 simplex.n = n;
	 simplex.p = new point_t[n+1];
	 for (int i = 0; i < n + 1; i++) {
	     simplex.p[i].x = new double[n];
	     for (int j = 0; j < n; j++) {
	    	 simplex.p[i].x[j] = (i - 1 == j) ? (start.x[j] != 0.0 ? 1.05 * start.x[j] : 0.00025) : start.x[j];
	     }
	     if (self_test) {
			 cost_fun(n, simplex.p[i], params);
		 }
	     else {
	    	 cost_function(n, simplex.p[i]);
	     }
	     eval_count++;
	 } // for (int i = 0; i < n + 1; i++)
	 // sort points in the simplex so that simplex.p[0] is the point having
	 // minimum fx and simplex.p[n] is the one having maximum fx
	 simplex_sort(simplex);
	 // Compute the simplex centroid
	 get_centroid(simplex, centroid);
	 iter_count++;
	 
	 // continue minimization unitl stop conditions are met
	 while (continue_minimization(simplex, eval_count, iter_count)) {
	     boolean shrink = false;
	     if (verbose) {
	    	 System.out.println("Iteration = " + iter_count);
	    	 Preferences.debug("Iteration = " + iter_count + "\n", Preferences.DEBUG_ALGORITHM);
	     }
	     update_point(simplex, centroid, RHO, point_r);
	     if (self_test) {
	    	 cost_fun(n, point_r, params);
	     }
	     else {
	    	 cost_function(n, point_r);
	     }
	     eval_count++;
	     if (point_r.fx < simplex.p[0].fx) {
	         update_point(simplex, centroid, RHO * CHI, point_e);
	         if (self_test) {
		    	 cost_fun(n, point_e, params);
		     }
		     else {
		    	 cost_function(n, point_e);
		     }
	         eval_count++;
	         if (point_e.fx < point_r.fx) {
	            // expand
	        	 if (verbose) {
	        		 System.out.println("expand  ");
	        		 Preferences.debug("expand  \n", Preferences.DEBUG_ALGORITHM);
	        	 }
	        	 copy_point(n, point_e, simplex.p[n]);
	         } // if (point_e.fx < point_r.fx)
	         else {
	        	 // reflect
	        	 if (verbose) {
	        		 System.out.println("reflect  ");
	        		 Preferences.debug("reflect  \n", Preferences.DEBUG_ALGORITHM);
	        	 }
	        	 copy_point(n, point_r, simplex.p[n]);
	         }
	     } // if (point_r.fx < simplex.p[0].fx)
	     else {
	    	 if (point_r.fx < simplex.p[n-1].fx) {
	    		 // reflect
	    		 if (verbose) {
	        		 System.out.println("reflect  ");
	        		 Preferences.debug("reflect  \n", Preferences.DEBUG_ALGORITHM);
	        	 }
	    		 copy_point(n, point_r, simplex.p[n]);
	    	 }
	    	 else {
	    		 if (point_r.fx < simplex.p[n].fx) {
	    			 update_point(simplex, centroid, RHO * GAMMA, point_c);
	    			 if (self_test) {
	    		    	 cost_fun(n, point_c, params);
	    		     }
	    		     else {
	    		    	 cost_function(n, point_c);
	    		     }
	    			 eval_count++;
	    			 if (point_c.fx <= point_r.fx) {
	    				 // contract outside
	    				 if (verbose) {
	    	        		 System.out.println("contract out  ");
	    	        		 Preferences.debug("contract out  \n", Preferences.DEBUG_ALGORITHM);
	    	        	 }
	    				 copy_point(n, point_c, simplex.p[n]);
	    			 }
	    			 else {
	    				 // shrink
	    				 if (verbose) {
	    	        		 System.out.println("shrink  ");
	    	        		 Preferences.debug("shrink  \n", Preferences.DEBUG_ALGORITHM);
	    	        	 }
	    				 shrink = true;
	    			 }
	    		 }
	    		 else {
                     update_point(simplex, centroid, -GAMMA, point_c);	
                     if (self_test) {
	    		    	 cost_fun(n, point_c, params);
	    		     }
	    		     else {
	    		    	 cost_function(n, point_c);
	    		     }
                     eval_count++;
                     if (point_c.fx <= simplex.p[n].fx) {
                    	 // contract inside
                    	 if (verbose) {
	    	        		 System.out.println("contract in  ");
	    	        		 Preferences.debug("contract in  \n", Preferences.DEBUG_ALGORITHM);
	    	        	 }
                    	 copy_point(n, point_c, simplex.p[n]);
                     }
                     else {
                    	 // shrink
                    	 if (verbose) {
	    	        		 System.out.println("shrink  ");
	    	        		 Preferences.debug("shrink  \n", Preferences.DEBUG_ALGORITHM);
	    	        	 }
	    				 shrink = true;
                     }
	    		 }
	    	 }
	     }
	     if (shrink) {
	         for (int i = 1; i < n + 1; i++) {
	        	 for (int j = 0; j < n; j++) {
	        		 simplex.p[i].x[j] = simplex.p[0].x[j] + SIGMA * (simplex.p[i].x[j] - simplex.p[0].x[j]);
	        	 }
	        	 if (self_test) {
    		    	 cost_fun(n, simplex.p[i], params);
    		     }
    		     else {
    		    	 cost_function(n, simplex.p[i]);
    		     }
	        	 eval_count++;
	         }
	         simplex_sort(simplex);
	     } // if (shrink)
	     else {
	    	 for (int i = n-1; i >= 0 && simplex.p[i+1].fx < simplex.p[i].fx; i--) {
	    		 swap_points(n, simplex.p[i+1], simplex.p[i]);
	    	 }
	     }
	     get_centroid(simplex, centroid);
	     iter_count++;
	     if (verbose) {
	    	 // print current minimum
	    	 System.out.println("Current minimum points:");
	    	 Preferences.debug("current minimum points:\n", Preferences.DEBUG_ALGORITHM);
	    	 for (int i = 0; i < n; i++) {
	    	     System.out.println("x["+i+"] = " + simplex.p[0].x[i]);
	    	     Preferences.debug("x["+i+"] = " + simplex.p[0].x[i] + "\n", Preferences.DEBUG_ALGORITHM);
	    	 }
	    	 System.out.println("Current minimum value = " + simplex.p[0].fx);
	    	 Preferences.debug("Current minimum value = " + simplex.p[0].fx);
	     }
	     
	 } // while (continue_minimization(simplex, eval_count, iter_count))
	 
	// save solution in output argument
     solution.x = new double[n];
     copy_point(n, simplex.p[0], solution);
     
     // free memory
     centroid.x = null;
     centroid = null;
     point_r.x = null;
     point_r = null;
     point_e.x = null;
     point_e = null;
     point_c.x = null;
     point_c = null;
     for (int i = 0; i < n + 1; i++) {
    	 simplex.p[i].x = null;
     }
     simplex.p = null;
     simplex = null;
 }
 
 private void simplex_sort(simplex_t simplex) {
	 int i;
	 ArrayList<point_t> pointList = new ArrayList<point_t>();
	 for (i = 0; i < n+1; i++) {
		 pointList.add(simplex.p[i]);
	 }
	 Collections.sort(pointList, new pointComparator());
	 for (i = 0; i < n+1; i++) {
		 simplex.p[i] = pointList.get(i);
	 }
 }
 
 private class pointComparator implements Comparator<point_t> {

     /**
      * DOCUMENT ME!
      * 
      * @param o1 DOCUMENT ME!
      * @param o2 DOCUMENT ME!
      * 
      * @return DOCUMENT ME!
      */
     public int compare(final point_t arg1, final point_t arg2) {
         final double fx1 = arg1.fx;
         final double fx2 = arg2.fx;

         if (fx1 == fx2) {
        	 return 0;
         }
         else {
        	 return (fx1 < fx2) ? -1 : 1;
         }
     }

 }
 
 // Get centroid (average position) of simplex
 private void get_centroid(simplex_t simplex, point_t centroid) {
	 int i, j;
	 for (j = 0; j < simplex.n; j++) {
		 centroid.x[j] = 0;
		 for (i = 0; i < simplex.n; i++) {
			 centroid.x[j] += simplex.p[i].x[j];
		 }
		 centroid.x[j] /= simplex.n;
	 }
 }
 
 // Assess if simplex satisfies the minimization requirements
 private boolean continue_minimization(simplex_t simplex, int eval_count, int iter_count) {
	 int i, j;
	 double temp;
	 if (eval_count > max_eval || iter_count > max_iter) {
		 // stop if #evals or #iters are greater than the max allowed
		 return false;
	 }
	 double condx = -1.0;
	 double condf = -1.0;
	 for (i = 1; i < simplex.n + 1; i++) {
		 temp = Math.abs(simplex.p[0].fx - simplex.p[i].fx);
		 if (condf < temp) {
			 condf = temp;
		 }
	 }
	 for (i = 1; i < simplex.n + 1; i++) {
		 for (j = 0; j < simplex.n; j++) {
			 temp = Math.abs(simplex.p[0].x[j] - simplex.p[i].x[j]);
			 if (condx < temp) {
				 condx = temp;
			 }
		 }
	 }
	 // Continue if either tolx or tolf condition is not met
	 return condx > tolx || condf > tolf;
 }
 
 // Update current point
 private void update_point(simplex_t simplex, point_t centroid, double lambda, point_t point) {
	 int j;
	 int n = simplex.n;
	 for (j = 0; j < n; j++) {
		 point.x[j] = (1.0 + lambda) * centroid.x[j] - lambda * simplex.p[n].x[j];
	 }
 }
 
 // Simple point_t manipulation utilities
 private void copy_point(int n, point_t src, point_t dst) {
	 int j;
	 for (j = 0; j < n; j++) {
		 dst.x[j] = src.x[j];
	 }
	 dst.fx = src.fx;
 }
 
 private void swap_points(int n, point_t p1, point_t p2) {
	 double temp;
	 int j;
	 for (j = 0; j < n; j++) {
		 temp = p1.x[j];
		 p1.x[j] = p2.x[j];
		 p2.x[j] = temp;
	 }
	 temp = p1.fx;
	 p1.fx = p2.fx;
	 p2.fx = temp;
 }
  
  private double SQUARE(double x) {
	  return (x * x);
  }
  
  // Define a generic point structure containing a position (x) and a value (fx)
  private class point_t {
	  double x[];
	  double fx;
  }
  
  // Define a simplex struct containing an array of n+1 points (p)
  // each having dimension (n)
  private class simplex_t {
	  point_t p[];
	  int n;
  }
  
  // Define optimization settings
  private class optimset_t {
	  double tolx;
	  double tolf;
	  int max_iter;
	  int max_eval;
	  boolean verbose;
  }
  
  private class param_t {
	  double a;
	  double b;
	  double c;
  }
  
  // Implementation of a cost function f: R^n->R compatible with fun_t
  // In this instance we use the Ackley Function as it allows us to demonstrate
  // the use of the optional fixed arguments
  // More details on the function at http://www.sfu.ca/%7Essurjano/ackley.html
  
  private void cost_fun(int n, point_t point, param_t params) {
	  switch(costNum) {
	  case ackleyNum:
	  // cost function computation for arguments of exp
	  double sum_squares = 0;
	  double sum_cos = 0;
	  for (int i = 0; i < n; i++) {
		  sum_squares += SQUARE(point.x[i]);
		  sum_cos += Math.cos(params.c * point.x[i]);
	  }
	  
	  // final result
	  point.fx = -params.a * Math.exp(-params.b * Math.sqrt(sum_squares/n)) - Math.exp(sum_cos/n) + params.a + Math.exp(1.0);
	  break;
	  default:
		  point.fx = 0;
	  }
  }
  
  public abstract void cost_function(int n, point_t point);
 
}