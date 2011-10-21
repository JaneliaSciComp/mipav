package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve;
//- try out the linear programming
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.lprec;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.constant;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.solve;

public class TestLp6 implements constant {

  public static void main(String args[]) {
    solve lpSolve = new solve();
    int rows = 0;
    int cols = 7;
    lprec lpIn = new lprec(rows, cols);
/*
    lpIn.debug = TRUE;
    lpIn.verbose = TRUE;
    lpIn.trace = TRUE;
*/
    double v[] = {0, 592, 381, 273, 55, 48, 37, 23};
    double rhs;
    lpSolve.set_obj_fn(lpIn, v);
    lpSolve.set_maxim(lpIn);
    v[1] = 3534;
    v[2] = 2356;
    v[3] = 1767;
    v[4] = 589;
    v[5] = 528;
    v[6] = 451;
    v[7] = 304;
    rhs = 119567;
    lpSolve.add_constraint(lpIn, v, LE, rhs);
    lpSolve.set_int(lpIn, 1, true);
    lpSolve.set_int(lpIn, 2, true);
    lpSolve.set_int(lpIn, 3, true);
    lpSolve.set_int(lpIn, 4, true);
    lpSolve.set_int(lpIn, 5, true);
    lpSolve.set_int(lpIn, 6, true);
    lpSolve.set_int(lpIn, 7, true);
    int result = lpSolve.solve(lpIn);
    if (result == constant.OPTIMAL)
      lpSolve.print_solution(lpIn);
    else
      System.out.println("no optimal solution");
  } // end of main
} // end of class TestLp
