package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve;
//- try out the linear programming
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.*;
public class TestLp3 implements constant{

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
    v[1] = 3;
    v[2] = 13;
    lpSolve.set_obj_fn(lpIn, v);
    lpSolve.set_maxim(lpIn);
    v[1] = 2;
    v[2] = 9;
    rhs = 40;
    lpSolve.add_constraint(lpIn, v, LE, rhs);
    v[1] = 11;
    v[2] = -8;
    rhs = 82;
    lpSolve.add_constraint(lpIn, v, LE, rhs);
/*
    lpSolve.set_int(lpIn, 1, TRUE);
    lpSolve.set_int(lpIn, 2, TRUE);
*/
    int result = lpSolve.solve(lpIn);
    if (result == constant.OPTIMAL)
      lpSolve.print_solution(lpIn);
    else
      System.out.println("no optimal solution");
  } // end of main
} // end of class TestLp
