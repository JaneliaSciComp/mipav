package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve;
//- try out the linear programming
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.*;
public class Test implements constant{

  public static void main(String args[]) {
    solve lpSolve = new solve();
    int rows = 0;
    int cols = 4+6;
    lprec lpIn = new lprec(rows, cols);
/*
    lpIn.debug = TRUE;
    lpIn.verbose = TRUE;
    lpIn.trace = TRUE;
*/
    double v[] = {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1};
    double rhs;
    lpSolve.set_obj_fn(lpIn, v);

	/* First Row */
    v[1] = 565;
    v[2] = 435;
    v[3] = 532;
    v[4] = 635;
    v[5] = 1;
    v[6] = -1;
    v[7] = 0;
    v[8] = 0;
    v[9] = 0;
    v[10] = 0;
    rhs = 634;
    lpSolve.add_constraint(lpIn, v, EQ, rhs);

	/* Second Row */
    v[1] = 245;
    v[2] = 292;
    v[3] = 488;
    v[4] = 333;
    v[5] = 0;
    v[6] = 0;
    v[7] = 1;
    v[8] = -1;
    v[9] = 0;
    v[10] = 0;
    rhs = 335;
    lpSolve.add_constraint(lpIn, v, EQ, rhs);

	/* Third Row */
    v[1] = 119;
    v[2] = 131;
    v[3] = 127;
    v[4] = 245;
    v[5] = 0;
    v[6] = 0;
    v[7] = 0;
    v[8] = 0;
    v[9] = 1;
    v[10] = -1;
    rhs = 240;
    lpSolve.add_constraint(lpIn, v, EQ, rhs);

	/*  Set upper and lower bound for the REAL variables. */
	for( int ii=1; ii <= 4; ii++) {
		lpSolve.set_upbo(lpIn, ii, 1e10);
		lpSolve.set_lowbo(lpIn, ii, 0);
	}

	/*  Set upper and lower bound for the SLACK variables. */
	for( int ii=5; ii <= 10; ii++) {
		lpSolve.set_upbo(lpIn, ii, 1e10);
		lpSolve.set_lowbo(lpIn, ii, 0);
	}

    int result = lpSolve.solve(lpIn);
    if (result == constant.OPTIMAL)
      lpSolve.print_solution(lpIn);
    else
      System.out.println("no optimal solution");
  } // end of main
} // end of class TestLp
