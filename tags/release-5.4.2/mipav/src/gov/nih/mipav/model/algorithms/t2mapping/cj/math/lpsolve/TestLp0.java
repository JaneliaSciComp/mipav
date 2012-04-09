package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve;
//- try out the linear programming
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.constant;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.lprec;
import gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp.solve;

public class TestLp0 implements constant {
  public static void main(String args[]) {
    solve lpSolve = new solve();
    lprec lpIn = new lprec(0, 16);
    double v[] = new double[17];
    double rhs;
    String s;
    s = "0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ";
    lpSolve.str_set_obj_fn(lpIn, s);
    lpSolve.set_maxim(lpIn);
    rhs = 1;
    String str[] = { "0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0",
               "0 0 0 0 0 0 0 0 1 1 0 0 1 1 0 0",
               "0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0",
               "0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0",
               "0 0 0 0 0 0 0 0 1 1 0 1 0 0 0 0",
               "0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0",
               "0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0",
               "0 1 0 0 0 0 0 0 0 1 0 0 0 1 0 0",
               "0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0",
               "0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0"
               };

    for (int i = 0; i < 10; i++ ) {
      lpSolve.str_add_constraint(lpIn, str[i], LE, rhs);
    }
    for (int i = 0; i <= lpIn.getRows(); i++) {
      lpSolve.get_row(lpIn, i, v);
      for (int index = 1; index <= lpIn.getColumns(); index++) {
    System.out.print(v[index] + " ");
      }
      System.out.println();
    }
    int result = lpSolve.solve(lpIn);
    if (result == constant.OPTIMAL)
      lpSolve.print_solution(lpIn);
    else
      System.out.println("no optimal solution");
  } // end of main
} // end of class TestLp0
