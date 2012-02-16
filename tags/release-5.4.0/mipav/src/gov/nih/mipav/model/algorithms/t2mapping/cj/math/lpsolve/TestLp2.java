package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve;
//- try out the linear programming
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.constant;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.lprec;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.solve;


public class TestLp2 implements constant{

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
    v[1] = 8;
    v[2] = 15;
    lpSolve.set_obj_fn(lpIn, v);
    lpSolve.set_maxim(lpIn);
    v[1] = 10;
    v[2] = 21;
    rhs = 156;
    lpSolve.add_constraint(lpIn, v, LE, rhs);
    v[1] = 2;
    v[2] = 1;
    rhs = 22;
    lpSolve.add_constraint(lpIn, v, LE, rhs);
    int result = lpSolve.solve(lpIn);
    if (result == constant.OPTIMAL)
      lpSolve.print_solution(lpIn);
    else
      System.out.println("no optimal solution");
  } // end of main
} // end of class TestLp
