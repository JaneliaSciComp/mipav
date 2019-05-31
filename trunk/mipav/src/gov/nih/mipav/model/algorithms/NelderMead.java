package gov.nih.mipav.model.algorithms;
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
 
 private int n;
 private double x[];
 private double tolx = 0.001; // tolerance on the simplex solutions coorindates
 private double tolf = 0.001; // tolerance on the function value coordinates
 private int max_iter = 1000; // maximum number of allowed iterations
 private int max_eval = 1000; // maximum number of allowed evaluations
 private boolean verbose = false; // toggle verbose output during minimization
 private boolean ackley_test = false;
 private ackley_param_t ackley_params;
 

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
      optimset = new optimset_t();
      optimset.tolx = tolx; // tolerance on the simplex solutions coordinates
      optimset.tolf = tolf; // tolerance on the function value;
      optimset.max_iter = max_iter; // maximum number of allowed iterations
      optimset.max_eval = max_eval; // maximum number of allowed function evaluations
      optimset.verbose = verbose; // toggle verbose output during minimization
      
      // cost function parameters
      if (ackley_test) {
          ackley_params = new ackley_param_t(); // parameters of ackley cost function
          ackley_params.a = 20.0;
          ackley_params.b = 0.2;
          ackley_params.c = 2.0 * Math.PI;
      }
      
      // call optimization methods
      solution = new point_t(); // container for the solution of the optimization
      nelder_mead(n, start, solution, optimset);
      if (ackley_test) {
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
  
 private void nelder_mead(int n, point_t start, point_t solution, optimset_t optimset) {
	 
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
  
  private class ackley_param_t {
	  double a;
	  double b;
	  double c;
  }
  
  // Implementation of a cost function f: R^n->R compatible with fun_t
  // In this instance we use the Ackley Function as it allows us to demonstrate
  // the use of the optional fixed arguments
  // More details on the function at http://www.sfu.ca/%7Essurjano/ackley.html
  
  private void ackley_fun(int n, point_t point, ackley_param_t params) {
	
	  // cost function computation for arguments of exp
	  double sum_squares = 0;
	  double sum_cos = 0;
	  for (int i = 0; i < n; i++) {
		  sum_squares += SQUARE(point.x[i]);
		  sum_cos += Math.cos(params.c * point.x[i]);
	  }
	  
	  // final result
	  point.fx = -params.a * Math.exp(-params.b * Math.sqrt(sum_squares/n)) - Math.exp(sum_cos/n) + params.a + Math.exp(1.0);
  }
  
  public abstract void cost_fun(int n, point_t point);
 
}