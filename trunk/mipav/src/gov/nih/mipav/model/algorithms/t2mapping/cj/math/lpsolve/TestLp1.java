package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve;
//- try out the linear programming

import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.constant;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.lprec;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.solve;

public class TestLp1 implements constant {

  public static void main(String args[]) {
    solve lpSolve = new solve();
    lprec lpIn = new lprec(2, 2);
/*
    lpIn.debug = TRUE;
    lpIn.verbose = TRUE;
    lpIn.trace = TRUE;
*/
    double v[] = new double[3];
    double rhs;
    v[1] = -1;
    v[2] = 2;
    lpSolve.set_obj_fn(lpIn, v);
    lpSolve.set_maxim(lpIn);
    v[1] = 2;
    v[2] = 1;
    rhs = 5;
    lpSolve.add_constraint(lpIn, v, LE, rhs);
    v[1] = -4;
    v[2] = 4;
    rhs = 5;
    lpSolve.add_constraint(lpIn, v, LE, rhs);
    lpSolve.set_int(lpIn, 1, true);
    lpSolve.set_int(lpIn, 2, true);
    int result = lpSolve.solve(lpIn);
    if (result == constant.OPTIMAL)
      lpSolve.print_solution(lpIn);
    else
      System.out.println("no optimal solution");
  } // end of main
} // end of class TestLp
