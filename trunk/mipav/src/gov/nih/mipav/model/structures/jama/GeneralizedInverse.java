package gov.nih.mipav.model.structures.jama;


import gov.nih.mipav.view.*;

public class GeneralizedInverse {
    // This is a port of the FORTRAN suborutine GINV2, A Simple Algorithm for Computing the Generalized Inverse
    // of a Matrix by B. Rust, W. R. Burrus, and C. Schneeberger, CACM 9(5): 381-387 (May, 1966)
    private double A[][];
    
    // First dimension of A
    private int MR;
    // Number of rows in A
    private int NR;
    // Number of columns in A
    private int NC;
    
    public GeneralizedInverse(double A1[][], int NR) {
        int i;
        int j;
        this.NR = NR;
        MR = A1.length;
        NC = A1[0].length;
        A = new double[NR][NC];
        for (i = 0; i < NR; i++) {
            for (j = 0; j < NC; j++) {
                A[i][j] = A1[i][j];
            }
        }
    }
    
    public double[][] ginv() {
      int i;
      double Ainv[][] = new double[NR][NC];
      double U[][] = new double[NC][NC];
      double aflag[] = new double[NC];
      double atemp[] = new double[NC];
      double fac;
      int n;
      double tol;
      int j;
      double dot1;
      int jm1;
      int L;
      int k;
      double dot2;
      
      for (i = 0; i < NC; i++) {
          U[i][i] = 1.0;
      }
      fac = dot(A, NR, 1, 1);
      fac = 1.0/Math.sqrt(fac);
      for (i = 0; i < NR; i++) {
          A[i][0] = A[i][0] * fac;
      }
      for (i = 0; i < NC; i++) {
          U[i][0] = U[i][0] * fac;
      }
      aflag[0] = 1.0;
      
      // Dependent column tolerance for n bit floating point precision
      n = 27;
      tol = 10.0 * Math.pow(0.5, n);
      tol = tol * tol;
      for (j = 2; j <= NC; j++) {
          dot1 = dot(A, NR, j, j);
          jm1 = j - 1;
          for (L = 1; L <= 2; L++) {
              for (k = 1; k <= jm1; k++) {
                  atemp[k-1] = dot(A, NR, j, k);    
              } // for (k = 1; k <= jm1; k++)
              for (k = 1; k <= jm1; k++) {
                  for (i = 1; i <= NR; i++) {
                      A[i-1][j-1] = A[i-1][j-1] - atemp[k-1]*A[i-1][k-1]*aflag[k-1];    
                  } // for (i = 1; i <= NR; i++)
                  for (i = 1; i <= NC; i++) {
                      U[i-1][j-1] = U[i-1][j-1] - atemp[k-1]*U[i-1][k-1];
                  } // for (i = 1; i <= NC; i++)
              } // for (k = 1; k <= jm1; k++)
          } // for (L = 1; L <= 2; L++)
          dot2 = dot(A, NR, j, j);
          if (((dot2/dot1) - tol) <= 0.0) {
              for (i = 1; i <= jm1; i++) {
                  atemp[i-1] = 0.0;
                  for (k = 1; k <= i; k++) {
                      atemp[i - 1] = atemp[i - 1] + U[k-1][i-1]*U[k-1][j-1];
                  } // for (k = 1; k <= i; k++)
              } // for (i = 1; i <= jm1; i++)
              for (i = 1; i <= NR; i++) {
                  A[i-1][j-1] = 0.0;
                  for (k = i; k <= jm1; k++) {
                      A[i-1][j-1] = A[i-1][j-1] - A[i-1][k-1]*atemp[k-1]*aflag[k-1];    
                  } // for (k = i; k <= jm1; k++)
              } // for (i = 1; i <= NR; i++)
              aflag[j-1] = 0.0;
              fac = dot(U, NC, j, j);
              fac = 1.0/Math.sqrt(fac);
          } // if (((dot2/dot1) - tol) <= 0.0)
          else { // ((dot2/dot1) - tol) > 0.0
              aflag[j-1] = 1.0;
              fac = 1.0/Math.sqrt(dot2);
          } // else ((dot2/dot1) - tol) > 0.0)
          for (i = 1; i <= NR; i++) {
              A[i-1][j-1] = A[i-1][j-1]*fac;
          } // for (i = 1; i <= NR; i++)
          for (i = 1; i <= NC; i++) {
              U[i-1][j-1] = U[i-1][j-1]*fac;
          } // for (i = 1; i <= NC; i++)
      } // for (j = 2; j <= NC; j++)
      for (j = 1; j <= NC; j++) {
          for (i = 1; i <= NR; i++) {
              fac = 0.0;
              for (k = j; k <= NC; k++) {
                  fac = fac + A[i-1][k-1]*U[j-1][k-1];
              } // for (k = j; k <= NC; k++)
              A[i-1][j-1] = fac;
          } // for (i = 1; i <= NR; i++)
      } // for (j = 1; j <= NC; j++)
      for (i = 1; i <= NC; i++) {
          for (j = 1; j <= NR; j++) {
              Ainv[i-1][j-1] = A[j-1][i-1];
          }
      }
      return Ainv;
    }
    
    // Computes the inner product of columns JC and KC
    private double dot(double A[][], int NR, int JC, int KC) {
        double dot = 0.0;
        int i;
        for (i = 0; i < NR; i++) {
            dot += A[i][JC-1]*A[i][KC-1];
        }
        return dot;
    }
}