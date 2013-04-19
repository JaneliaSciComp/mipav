package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;

import java.io.IOException;
import java.util.BitSet;

/**
 * This is a port of the files anitgradient2.m and  antigradient2.c created by Gunnar Farneback in the
 * Spatial domain toolbox at http://www.imt.liu.se/mi/Tools.  Email permission to do so was granted by 
 * Gunnar Farneback:
 * On 03/15/2013 08:24 PM, Gandler, William (NIH/CIT) [E] wrote:
> Dear Gunnar Farneback:
>
> I am a Java programmer for the MIPAV software project at the National 
> Institutes of Health. I would like to port your inverse gradient C and 
> MATLAB routines in the Spatial Domain Toolbox to Java for inclusion in 
> the MIPAV imaging analysis package. However, while MIPAV is open 
> source, it does not distribute its code under the GNU General public 
> license and has no plans to do so. Our open source license is:
>
> http://mipav.cit.nih.gov/license/license.html
>
> Would it be possible to obtain permission to distribute a port of your 
> inverse gradient code in MIPAV without the GNU General public license 
> as long as we give full credit to you?

Yes, go ahead.

/Gunnar

function f = antigradient2(g, mask, mu, n)
    
% ANTIGRADIENT2   Reconstruction from gradients in 2D and 3D.
%
% This differs from antigradient in that it can handle arbitrarily
% shaped domains.
%
% f = antigradient2(g) computes the function f which has a gradient
% as close as possible to g. The vector field g must either be an
% MxNx2 array or an MxNxPx3 array.
%
% f = antigradient2(g, mask) does the same thing but restricted to
% points where mask is nonzero. For meaningful results the domain must
% be simply connected (otherwise run it multiple times for each
% connected component). Certain shapes may give poor convergence; in
% general almost convex shapes can be expected to have the best
% properties. The mask should have size MxN or MxNxP. It can also be
% set to an empty matrix or omitted, in which case all points are
% included in the domain.
%
% f = antigradient2(g, mask, mu) sets the unrecoverable
% mean of f to mu. When omitted, the mean of f is set to zero.
%
% f = antigradient2(g, mask, mu, n) uses n multigrid iterations. The
% default value  of 2 iterations is fast but only moderately accurate.
% The algorithm converges quickly, however, so usually 10-15
% iterations are sufficient to reach full convergence. This may depend
% on the shape of the domain, however.
%
% This function is based on transforming the inverse gradient problem to a
% PDE - a Poisson equation with Neumann boundary conditions. This equation
% is solved by an implementation of the full multigrid algorithm.
%
% The implementation is most efficient when all sides are of approximately
% the same size. The worst case is when two sides are large and the
% third side much smaller. The reason is that only uniform
% downsampling is implemented and when the smallest side becomes
% smaller than 4, a direct solution is applied. It makes no big
% difference whether the sides are odd or even, although sides which
% are of the form 2^k+1 give somewhat faster convergence.
%
% Author: Gunnar Farneback
%         Medical Informatics
%         Linkoping University, Sweden
%         gunnar@imt.liu.se

 *
 */

public class AlgorithmAntigradient2 extends AlgorithmBase {
    
    private static final int RECURSION_SIZE_LIMIT = 4;
    
    /* This can without cost be overdimensioned. */
    public static final int MAX_LEVELS = 30;
    
    private boolean entireImage;
    
    private BitSet mask = null;
    
    private double fMean = 0.0;
    
    private int number_of_iterations = 2;
    
    private int xDim;
    private int yDim;
    private int zDim;
    private double g[];
    private dataStruct data;
    
    public AlgorithmAntigradient2(ModelImage destImage, ModelImage srcImg, boolean entireImage, 
                                  double fMean, int number_of_iterations) {
        super(destImage, srcImg);
        this.entireImage = entireImage;
        this.fMean = fMean;
        this.number_of_iterations = number_of_iterations;
        if (!entireImage) {
            mask = srcImage.generateVOIMask();
        }
    }
    
    public void runAlgorithm() {
        int nDims;
        nDims = srcImage.getNDims();
        if (nDims == 4) {
            antigradient3D();
        }
        
    }
    
    private void antigradient3D() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int volume = sliceSize * zDim;
        double rhs[];
        double lhs[];
        double weight[];
        int x;
        int y;
        int z;
        int index1;
        int index2;
        int index3;
        double d;
        int N_missing;
        int S_missing;
        int W_missing;
        int E_missing;
        int U_missing;
        int D_missing;
        int ival;
        double f_out[];
        double sum;
        int num_samples_in_mask;
        double mean;
        int i;
        
        try {
            g = new double [3 * volume];
        }
        catch (final OutOfMemoryError e) {
            displayError("AlgorithmAntigradient2: Out of memory on g = new double[3 * volume]");

            setCompleted(false);

            return;
        }
        
        try {
            srcImage.exportData(0, 3 * volume, g);
        }
        catch (IOException e) {
                displayError("AlgorithmAntigradient2: IOException on srcImage.exportData(0, 3*volume, g)");

                setCompleted(false);

                return;
        }
        
        // Compute left and right hand sides of Poisson problem with Neumann
        // boundary conditions, discretized by finite differences.
        rhs = new double[volume];
        lhs = new double[27 * volume];
        data = new dataStruct();
        data.lhs[0] = lhs;
        weight = new double[volume];
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    index1 = (z * yDim + y) * xDim + x;
                    index2 = index1 + volume;
                    index3 = index1 + 2 * volume;
                    d = 0.0;
                    N_missing = 0;
                    S_missing = 0;
                    W_missing = 0;
                    E_missing = 0;
                    U_missing = 0;
                    D_missing = 0;
                    
                    if (!entireImage && !mask.get(index1)) {
                        continue;
                    }
                    
                    weight[index1] = 1;
                    
                    if (x == 0 || (!entireImage && !mask.get(index1-1))) {
                        N_missing = 1;
                    }
                    
                    if (x == xDim - 1 || (!entireImage && !mask.get(index1+1))) {
                        S_missing = 1;
                    }
                    
                    if (y == 0 || (!entireImage && !mask.get(index1 - xDim))) {
                        W_missing = 1;
                    }
                    
                    if (y == yDim - 1 || (!entireImage && !mask.get(index1 + xDim))) {
                        E_missing = 1;
                    }
                    
                    if (z == 0 || (!entireImage && !mask.get(index1 - sliceSize))) {
                        U_missing = 1;
                    }
                    
                    if (z == zDim - 1 || (!entireImage && !mask.get(index1 + sliceSize))) {
                        D_missing = 1;
                    }
                    
                    if ((N_missing == 1) && (S_missing == 0)) {
                        d = g[index1 + 1] + g[index1];
                    }
                    else if ((N_missing == 0) && (S_missing == 1)) {
                        d = -g[index1] - g[index1-1];
                    }
                    else if ((N_missing == 0) && (S_missing == 0)) {
                        d = 0.5 * (g[index1 + 1] - g[index1 - 1]);
                    }
                    
                    if ((W_missing == 1) && (E_missing == 0)) {
                        d += g[index2 + xDim] + g[index2];
                    }
                    else if ((W_missing == 0) && (E_missing == 1)) {
                        d += -g[index2] - g[index2-xDim];
                    }
                    else if ((W_missing == 0) && (E_missing == 0)) {
                        d += 0.5 * (g[index2 + xDim] - g[index2 - xDim]);
                    }
                    
                    if ((U_missing == 1) && (D_missing == 0)) {
                        d += g[index3 + sliceSize] + g[index3];
                    }
                    else if ((U_missing == 0) && (D_missing == 1)) {
                        d += -g[index3] - g[index3 - sliceSize];
                    }
                    else if ((U_missing == 0) && (D_missing == 0)) {
                        d += 0.5 * (g[index3 + sliceSize] - g[index3 - sliceSize]);
                    }
                    
                    rhs[index1] = d;
                    
                    ival = 0;
                    if ((N_missing == 0) || (S_missing == 0)) {
                        ival = ival - 2;
                    }
                    if ((W_missing == 0) || (E_missing == 0)) {
                        ival = ival - 2;
                    }
                    if ((U_missing == 0) || (D_missing == 0)) {
                        ival = ival - 2;
                    }
                    lhs[27*index1 + 13] = ival;
                    
                    if (N_missing == 0) {
                        lhs[27*index1 + 12] = 1 + S_missing;
                    }
                    
                    if (S_missing == 0) {
                        lhs[27*index1 + 14] = 1 + N_missing;
                    }
                    
                    if (W_missing == 0) {
                        lhs[27*index1 + 10] = 1 + E_missing;
                    }
                    
                    if (E_missing == 0) {
                        lhs[27*index1 + 16] = 1 + W_missing;
                    }
                    
                    if (U_missing == 0) {
                        lhs[27*index1 + 4] = 1 + D_missing;
                    }
                    
                    if (D_missing == 0) {
                        lhs[27*index1 + 22] = 1 + U_missing;
                    }
                    
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        
        // Solve the equation system with the full multigrid algorithm.
        // Use W cycles and 2 presmoothing and 2 postsmoothing
        // Gauss-Seidel iterations.
        
        f_out = new double[volume];
        poisson_full_multigrid3D(0, rhs, weight, number_of_iterations, xDim, yDim, zDim, f_out);
        
        // Fix the mean value
        sum = 0.0;
        num_samples_in_mask = 0;
        for (i = 0; i < volume; i++) {
            if (weight[i] > 0.0) {
                sum += f_out[i];
                num_samples_in_mask++;
            }
        }
        
        mean = sum/ num_samples_in_mask;
        
        for (i = 0; i < volume; i++) {
            if (weight[i] > 0.0) {
                f_out[i] -= mean;
                f_out[i] += fMean;
            }
        }
        
        try {
            destImage.importData(0, f_out, true);
        }
        catch(IOException e) {
            displayError("AlgorithmAntigradient2: IOException on destImage.importData(0, f_out, true)");

            setCompleted(false);

            return;  
        }
        
        rhs = null;
        weight = null;
        for (i = 0; i < MAX_LEVELS; i++) {
            if (data.lhs[i] != null) {
                data.lhs[i] = null;
            }
        }
        data.lhs = null;
        data.A_array = null;
    } // antigradient3D()
    
    /*
     * It is assumed that f_out is initialized to zero when called
     */
    private void poisson_full_multigrid3D(int level, double[] rhs, double[] weight, int number_of_iterations,
                                          int M, int N, int P, double[] f_out) {
        double rhs_downsampled[];
        double coarse_weight[];
        double f_coarse[];
        int k;
        int Mhalf;
        int Nhalf;
        int Phalf;
        
        // Unless already coarsest scale, first recurse to coarser scale
        if (M >= RECURSION_SIZE_LIMIT  && N >= RECURSION_SIZE_LIMIT && P >= RECURSION_SIZE_LIMIT) {
            // Downsample right hand size
            Mhalf = (M + 1)/2;
            Nhalf = (N + 1)/2;
            Phalf = (P + 1)/2;
            rhs_downsampled = new double[Mhalf * Nhalf * Phalf];
            coarse_weight = new double[Mhalf * Nhalf * Phalf];
            downsample3D(rhs, M, N, P, rhs_downsampled, Mhalf, Nhalf, Phalf,
                         weight, coarse_weight);
            galerkin3D(level, M, N, P, Mhalf, Nhalf, Phalf, weight, coarse_weight);

            f_coarse = new double[Mhalf * Nhalf * Phalf];
            poisson_full_multigrid3D(level + 1, rhs_downsampled, coarse_weight,
                    number_of_iterations,
                    Mhalf, Nhalf, Phalf, f_coarse);
            // Upsample the coarse result.
            upsample3D(M, N, P, f_coarse, Mhalf, Nhalf, Phalf, f_out,
                    weight, coarse_weight);

            f_coarse = null;
            coarse_weight = null;
            rhs_downsampled = null;
        } // if (M >= RECURSION_SIZE_LIMIT  && N >= RECURSION_SIZE_LIMIT && P >= RECURSION_SIZE_LIMIT)
    } // poisson_full-multigrid3D
    
    /* Recursive multigrid function.*/
    private void poisson_multigrid3D(int level, double[] rhs, double[] weight,
                int n1, int n2, int nm,
                double[] f_out,
                int M, int N, int P, int[] directly_solved)
    {
      int k;
      double r[];
      double r_downsampled[];
      double coarse_weight[];
      double lhs[] = data.lhs[level];
      double v[];
      int Mhalf;
      int Nhalf;
      int Phalf;
      int MN = M * N;
      
      /* Solve a sufficiently small problem directly. */
      if (M < RECURSION_SIZE_LIMIT
          || N < RECURSION_SIZE_LIMIT
          || P < RECURSION_SIZE_LIMIT)
      {
        solve_directly3D(lhs, rhs, f_out, M, N, P);
        directly_solved[0] = 1;
        return;
      }
      directly_solved[0] = 0;
      
      /* Pre-smoothing. */
      for (k = 0; k < n1; k++)
        //gauss_seidel3D(f_out, lhs, rhs, M, N, P);
      
      /* Compute residual. */
      r = new double[M * N * P];
      //compute_residual3D(r, lhs, rhs, f_out, M, N, P);
      
      /* Downsample residual. */
      Mhalf = (M + 1) / 2;
      Nhalf = (N + 1) / 2;
      Phalf = (P + 1) / 2;
      r_downsampled = new double[Mhalf * Nhalf * Phalf];
      coarse_weight = new double[Mhalf * Nhalf * Phalf];
      //downsample3D(r, M, N, P, r_downsampled, Mhalf, Nhalf, Phalf,
               //weight, coarse_weight);
      galerkin3D(level, M, N, P, Mhalf, Nhalf, Phalf, weight, coarse_weight);

      /* Recurse to compute a correction. */
      v = new double[Mhalf * Nhalf * Phalf];
      for (k = 0; k < nm; k++)
      {
        poisson_multigrid3D(level + 1, r_downsampled, coarse_weight,
                n1, n2, nm, v, Mhalf, Nhalf, Phalf, directly_solved);
        if (directly_solved[0] != 0)
          break;
      }
      
      upsample3D(M, N, P, v, Mhalf, Nhalf, Phalf, f_out, weight, coarse_weight);
      
      /* Post-smoothing. */
      //for (k = 0; k < n2; k++)
        //gauss_seidel3D(f_out, lhs, rhs, M, N, P);
      
      /* Set the mean value to zero.
       *
       * FIXME: This should not be needed (I believe) and might indicate
       * some bug elsewhere.
       */
      boolean needed = false;
      if (needed)
      {
        double sum = 0.0;
        int num_samples_in_mask = 0;
        double mean;
        int i;
        
        for (i = 0; i < M * N * P; i++)
          if (weight[i] != 0)
          {
        sum += f_out[i];
        num_samples_in_mask++;
          }
        
        mean = sum / num_samples_in_mask;
        for (i = 0; i < M * N * P; i++)
          if (weight[i] != 0)
        f_out[i] -= mean;
      }
      
      r = null;
      r_downsampled = null;
      coarse_weight = null;
      v = null;
    }
    
    private void solve_directly3D(double[] lhs, double[] rhs, double[] f_out, int M, int N, int P)
    {
      int s = M * N * P;
      int MN = M * N;
      int dims[] = new int[2];
      double b_array[];
      double b[];
      int i, j, p;
      double x_array[];
      double input_arrays[][] = new double[2][];

      if (data.A_array == null)
      {
        double A[];
        
        //dims[0] = s;
        //dims[1] = s;
        data.A_array = new double[s * s];
        A = data.A_array;
      
        for (p = 0; p < P; p++)
          for (j = 0; j < N; j++)
        for (i = 0; i < M; i++)
        {
          int index = (p * N + j) * M + i;
          int k;
          if (lhs[27 * index + 13] == 0)
            A[index + s * index] = 1;
          else
          {
            for (k = 0; k < 27; k++)
            {
              int u = (k % 3) - 1;
              int v = ((k / 3) % 3) - 1;
              int w = ((k / 9) % 3) - 1;
              if (lhs[27 * index + k] != 0)
            A[index + s * (index + u + v * M + w * MN)] = lhs[27 * index + k];
            }
          }
        }
        
        for (i = 0; i < s*s; i++)
          A[i] += 1.0 / (s*s);
      }
      
      //dims[0] = s;
      //dims[1] = 1;
      b_array = new double[s];
      b = b_array;
      for (i = 0; i < s; i++) {
          b[i] = rhs[i];
      }
        
      input_arrays[0] = data.A_array;
      input_arrays[1] = b_array;
      //mexCallMATLAB(1, &x_array, 2, input_arrays, "\\");
      for (i = 0; i < s; i++) {
          //f_out[i] = x_array[i];
      }
      x_array = null;
      b_array = null;
    }


    
    
    private void downsample3D(double[] rhs, int M, int N, int P,
                 double[] rhs_coarse, int Mhalf, int Nhalf, int Phalf,
                 double[] weight, double []coarse_weight)
    {
      int i, j, p;
      int index1;
      int index2;
      int MN = M * N;
      double w[][][] = new double[3][3][3];
      double sum;
      
      if (M % 2 == 0 && N % 2 == 0 && P % 2 == 0)
      {
        for (p = 0; p < Phalf; p++)
        {
          for (j = 0; j < Nhalf; j++)
          {
            for (i = 0; i < Mhalf; i++)
            {
              index1 = (p * Nhalf + j) * Mhalf + i;
              index2 = (2 * p * N + 2 * j) * M + 2 * i;
              
              w[1][1][1] = weight[index2];
              w[1][1][2] = weight[index2 + MN];
              w[1][2][1] = weight[index2 + M];
              w[1][2][2] = weight[index2 + MN + M];
              w[2][1][1] = weight[index2 + 1];
              w[2][1][2] = weight[index2 + MN + 1];
              w[2][2][1] = weight[index2 + M + 1];
              w[2][2][2] = weight[index2 + MN + M + 1];
              
              sum = w[1][1][1] + w[1][1][2] + w[1][2][1] + w[1][2][2] + w[2][1][1] + w[2][1][2] + w[2][2][1] + w[2][2][2];
              coarse_weight[index1] = sum;
              
              if (sum > 0)
              {
                double result = 0;
                if (w[1][1][1] > 0)
                  result += w[1][1][1] * rhs[index2];
                if (w[1][1][2] > 0)
                  result += w[1][1][2] * rhs[index2 + MN];
                if (w[1][2][1] > 0)
                  result += w[1][2][1] * rhs[index2 + M];
                if (w[1][2][2] > 0)
                  result += w[1][2][2] * rhs[index2 + MN + M];
                if (w[2][1][1] > 0)
                  result += w[2][1][1] * rhs[index2 + 1];
                if (w[2][1][2] > 0)
                  result += w[2][1][2] * rhs[index2 + MN + 1];
                if (w[2][2][1] > 0)
                  result += w[2][2][1] * rhs[index2 + M + 1];
                if (w[2][2][2] > 0)
                  result += w[2][2][2] * rhs[index2 + MN + M + 1];
                
                rhs_coarse[index1] = 8 / sum * result;
              }
            }
          }
        }
      }
      
      if (M % 2 == 1 && N % 2 == 0 && P % 2 == 0)
      {
        for (p = 0; p < Phalf; p++)
        {
          for (j = 0; j < Nhalf; j++)
          {
            for (i = 0; i < Mhalf; i++)
            {
              index1 = (p * Nhalf + j) * Mhalf + i;
              index2 = (2 * p * N + 2 * j) * M + 2 * i;
              
              w[0][1][1] = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
              w[0][1][2] = 0.5 * (i > 0 ? weight[index2 + MN - 1] : 0.0);
              w[0][2][1] = 0.5 * (i > 0 ? weight[index2 + M - 1] : 0.0);
              w[0][2][2] = 0.5 * (i > 0 ? weight[index2 + MN + M - 1] : 0.0);
              w[1][1][1] = weight[index2];
              w[1][1][2] = weight[index2 + MN];
              w[1][2][1] = weight[index2 + M];
              w[1][2][2] = weight[index2 + MN + M];
              w[2][1][1] = 0.5 * (i < Mhalf - 1 ?  weight[index2 + 1] : 0.0);
              w[2][1][2] = 0.5 * (i < Mhalf - 1 ?  weight[index2 + MN + 1] : 0.0);
              w[2][2][1] = 0.5 * (i < Mhalf - 1 ?  weight[index2 + M + 1] : 0.0);
              w[2][2][2] = 0.5 * (i < Mhalf - 1 ?  weight[index2 + MN + M + 1] : 0.0);
              
              sum = w[0][1][1] + w[0][1][2] + w[0][2][1] + w[0][2][2] + w[1][1][1] + w[1][1][2] + 
                    w[1][2][1] + w[1][2][2] + w[2][1][1] + w[2][1][2] + w[2][2][1] + w[2][2][2];
              coarse_weight[index1] = sum;
              
              if (sum > 0)
              {
                double result = 0;
                if (w[0][1][1] > 0)
                  result += w[0][1][1] * rhs[index2 - 1];
                if (w[0][1][2] > 0)
                  result += w[0][1][2] * rhs[index2 + MN - 1];
                if (w[0][2][1] > 0)
                  result += w[0][2][1] * rhs[index2 + M - 1];
                if (w[0][2][2] > 0)
                  result += w[0][2][2] * rhs[index2 + MN + M - 1];
                if (w[1][1][1] > 0)
                  result += w[1][1][1] * rhs[index2];
                if (w[1][1][2] > 0)
                  result += w[1][1][2] * rhs[index2 + MN];
                if (w[1][2][1] > 0)
                  result += w[1][2][1] * rhs[index2 + M];
                if (w[1][2][2] > 0)
                  result += w[1][2][2] * rhs[index2 + MN + M];
                if (w[2][1][1] > 0)
                  result += w[2][1][1] * rhs[index2 + 1];
                if (w[2][1][2] > 0)
                  result += w[2][1][2] * rhs[index2 + MN + 1];
                if (w[2][2][1] > 0)
                  result += w[2][2][1] * rhs[index2 + M + 1];
                if (w[2][2][2] > 0)
                  result += w[2][2][2] * rhs[index2 + MN + M + 1];
                
                rhs_coarse[index1] = 8 / sum * result;
              }
            }
          }
        }
      }
      
      if (M % 2 == 0 && N % 2 == 1 && P % 2 == 0)
      {
        for (p = 0; p < Phalf; p++)
        {
          for (j = 0; j < Nhalf; j++)
          {
            for (i = 0; i < Mhalf; i++)
            {
              index1 = (p * Nhalf + j) * Mhalf + i;
              index2 = (2 * p * N + 2 * j) * M + 2 * i;
              
              w[1][0][1] = 0.5 * (j > 0 ?  weight[index2 - M] : 0.0);
              w[1][0][2] = 0.5 * (j > 0 ? weight[index2 + MN - M] : 0.0);
              w[1][1][1] = weight[index2];
              w[1][1][2] = weight[index2 + MN];
              w[1][2][1] = 0.5 * (j < Nhalf - 1 ?  weight[index2 + M] : 0.0);
              w[1][2][2] = 0.5 * (j < Nhalf - 1 ?  weight[index2 + MN + M] : 0.0);
              w[2][0][1] = 0.5 * (j > 0 ?  weight[index2 - M + 1] : 0.0);
              w[2][0][2] = 0.5 * (j > 0 ?  weight[index2 + MN - M + 1] : 0.0);
              w[2][1][1] = weight[index2 + 1];
              w[2][1][2] = weight[index2 + MN + 1];
              w[2][2][1] = 0.5 * (j < Nhalf - 1 ?  weight[index2 + M + 1] : 0.0);
              w[2][2][2] = 0.5 * (j < Nhalf - 1 ?  weight[index2 + MN + M + 1] : 0.0);
              
              sum = w[1][0][1] + w[1][0][2] + w[1][1][1] + w[1][1][2] + w[1][2][1] + w[1][2][2] + 
                    w[2][0][1] + w[2][0][2] + w[2][1][1] + w[2][1][2] + w[2][2][1] + w[2][2][2];
              coarse_weight[index1] = sum;
              
              if (sum > 0)
              {
                double result = 0;
                if (w[1][0][1] > 0)
                  result += w[1][0][1] * rhs[index2 - M];
                if (w[1][0][2] > 0)
                  result += w[1][0][2] * rhs[index2 + MN - M];
                if (w[1][1][1] > 0)
                  result += w[1][1][1] * rhs[index2];
                if (w[1][1][2] > 0)
                  result += w[1][1][2] * rhs[index2 + MN];
                if (w[1][2][1] > 0)
                  result += w[1][2][1] * rhs[index2 + M];
                if (w[1][2][2] > 0)
                  result += w[1][2][2] * rhs[index2 + MN + M];
                if (w[2][0][1] > 0)
                  result += w[2][0][1] * rhs[index2 - M + 1];
                if (w[2][0][2] > 0)
                  result += w[2][0][2] * rhs[index2 + MN - M + 1];
                if (w[2][1][1] > 0)
                  result += w[2][1][1] * rhs[index2 + 1];
                if (w[2][1][2] > 0)
                  result += w[2][1][2] * rhs[index2 + MN + 1];
                if (w[2][2][1] > 0)
                  result += w[2][2][1] * rhs[index2 + M + 1];
                if (w[2][2][2] > 0)
                  result += w[2][2][2] * rhs[index2 + MN + M + 1];
                
                rhs_coarse[index1] = 8 / sum * result;
              }
            }
          }
        }
      }
      
      if (M % 2 == 1 && N % 2 == 1 && P % 2 == 0)
      {
        for (p = 0; p < Phalf; p++)
        {
          for (j = 0; j < Nhalf; j++)
          {
            for (i = 0; i < Mhalf; i++)
            {
              index1 = (p * Nhalf + j) * Mhalf + i;
              index2 = (2 * p * N + 2 * j) * M + 2 * i;
              
              w[0][0][1] = 0.25 * (i > 0 && j > 0 ? weight[index2 - M - 1] : 0.0);
              w[0][0][2] = 0.25 * (i > 0 && j > 0 ? weight[index2 + MN - M - 1] : 0.0);
              w[0][1][1] = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
              w[0][1][2] = 0.5 * (i > 0 ? weight[index2 + MN - 1] : 0.0);
              w[0][2][1] = 0.25 * (i > 0 && j < Nhalf - 1 ? weight[index2 + M - 1] : 0.0);
              w[0][2][2] = 0.25 * (i > 0 && j < Nhalf - 1 ? weight[index2 + MN + M - 1] : 0.0);
              w[1][0][1] = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
              w[1][0][2] = 0.5 * (j > 0 ? weight[index2 + MN - M] : 0.0);
              w[1][1][1] = weight[index2];
              w[1][1][2] = weight[index2 + MN];
              w[1][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
              w[1][2][2] = 0.5 * (j < Nhalf - 1 ? weight[index2 + MN + M] : 0.0);
              w[2][0][1] = 0.25 * (i < Mhalf - 1 && j > 0 ? weight[index2 - M + 1] : 0.0);
              w[2][0][2] = 0.25 * (i < Mhalf - 1 && j > 0 ? weight[index2 + MN - M + 1] : 0.0);
              w[2][1][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
              w[2][1][2] = 0.5 * (i < Mhalf - 1 ? weight[index2 + MN + 1] : 0.0);
              w[2][2][1] = 0.25 * (i < Mhalf - 1 && j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
              w[2][2][2] = 0.25 * (i < Mhalf - 1 && j < Nhalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
              
              sum = w[0][0][1] + w[0][0][2] + w[0][1][1] + w[0][1][2] + w[0][2][1] + w[0][2][2] + w[1][0][1] 
                  + w[1][0][2] + w[1][1][1] + w[1][1][2] + w[1][2][1] + w[1][2][2] + w[2][0][1] + w[2][0][2] 
                  + w[2][1][1] + w[2][1][2] + w[2][2][1] + w[2][2][2];
              coarse_weight[index1] = sum;
              
              if (sum > 0)
              {
                double result = 0;
                if (w[0][0][1] > 0)
                  result += w[0][0][1] * rhs[index2 - M - 1];
                if (w[0][0][2] > 0)
                  result += w[0][0][2] * rhs[index2 + MN - M - 1];
                if (w[0][1][1] > 0)
                  result += w[0][1][1] * rhs[index2 - 1];
                if (w[0][1][2] > 0)
                  result += w[0][1][2] * rhs[index2 + MN - 1];
                if (w[0][2][1] > 0)
                  result += w[0][2][1] * rhs[index2 + M - 1];
                if (w[0][2][2] > 0)
                  result += w[0][2][2] * rhs[index2 + MN + M - 1];
                if (w[1][0][1] > 0)
                  result += w[1][0][1] * rhs[index2 - M];
                if (w[1][0][2] > 0)
                  result += w[1][0][2] * rhs[index2 + MN - M];
                if (w[1][1][1] > 0)
                  result += w[1][1][1] * rhs[index2];
                if (w[1][1][2] > 0)
                  result += w[1][1][2] * rhs[index2 + MN];
                if (w[1][2][1] > 0)
                  result += w[1][2][1] * rhs[index2 + M];
                if (w[1][2][2] > 0)
                  result += w[1][2][2] * rhs[index2 + MN + M];
                if (w[2][0][1] > 0)
                  result += w[2][0][1] * rhs[index2 - M + 1];
                if (w[2][0][2] > 0)
                  result += w[2][0][2] * rhs[index2 + MN - M + 1];
                if (w[2][1][1] > 0)
                  result += w[2][1][1] * rhs[index2 + 1];
                if (w[2][1][2] > 0)
                  result += w[2][1][2] * rhs[index2 + MN + 1];
                if (w[2][2][1] > 0)
                  result += w[2][2][1] * rhs[index2 + M + 1];
                if (w[2][2][2] > 0)
                  result += w[2][2][2] * rhs[index2 + MN + M + 1];
                
                rhs_coarse[index1] = 8 / sum * result;
              }
            }
          }
        }
      }
      
      if (M % 2 == 0 && N % 2 == 0 && P % 2 == 1)
      {
        for (p = 0; p < Phalf; p++)
        {
          for (j = 0; j < Nhalf; j++)
          {
            for (i = 0; i < Mhalf; i++)
            {
              index1 = (p * Nhalf + j) * Mhalf + i;
              index2 = (2 * p * N + 2 * j) * M + 2 * i;
              
              w[1][1][0] = 0.5 * (p > 0 ? weight[index2 - MN] : 0.0);
              w[1][1][1] = weight[index2];
              w[1][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN] : 0.0);
              w[1][2][0] = 0.5 * (p > 0 ? weight[index2 - MN + M] : 0.0);
              w[1][2][1] = weight[index2 + M];
              w[1][2][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + M] : 0.0);
              w[2][1][0] = 0.5 * (p > 0 ? weight[index2 - MN + 1] : 0.0);
              w[2][1][1] = weight[index2 + 1];
              w[2][1][2] = 0.5 * (p < Phalf - 1 ?  weight[index2 + MN + 1] : 0.0);
              w[2][2][0] = 0.5 * (p > 0 ?  weight[index2 - MN + M + 1] : 0.0);
              w[2][2][1] = weight[index2 + M + 1];
              w[2][2][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
              
              sum = w[1][1][0] + w[1][1][1] + w[1][1][2] + w[1][2][0] + w[1][2][1] + w[1][2][2] 
                  + w[2][1][0] + w[2][1][1] + w[2][1][2] + w[2][2][0] + w[2][2][1] + w[2][2][2];
              coarse_weight[index1] = sum;
              
              if (sum > 0)
              {
                double result = 0;
                if (w[1][1][0] > 0)
                  result += w[1][1][0] * rhs[index2 - MN];
                if (w[1][1][1] > 0)
                  result += w[1][1][1] * rhs[index2];
                if (w[1][1][2] > 0)
                  result += w[1][1][2] * rhs[index2 + MN];
                if (w[1][2][0] > 0)
                  result += w[1][2][0] * rhs[index2 - MN + M];
                if (w[1][2][1] > 0)
                  result += w[1][2][1] * rhs[index2 + M];
                if (w[1][2][2] > 0)
                  result += w[1][2][2] * rhs[index2 + MN + M];
                if (w[2][1][0] > 0)
                  result += w[2][1][0] * rhs[index2 - MN + 1];
                if (w[2][1][1] > 0)
                  result += w[2][1][1] * rhs[index2 + 1];
                if (w[2][1][2] > 0)
                  result += w[2][1][2] * rhs[index2 + MN + 1];
                if (w[2][2][0] > 0)
                  result += w[2][2][0] * rhs[index2 - MN + M + 1];
                if (w[2][2][1] > 0)
                  result += w[2][2][1] * rhs[index2 + M + 1];
                if (w[2][2][2] > 0)
                  result += w[2][2][2] * rhs[index2 + MN + M + 1];
                
                rhs_coarse[index1] = 8 / sum * result;
              }
            }
          }
        }
      }
      
      if (M % 2 == 1 && N % 2 == 0 && P % 2 == 1)
      {
        for (p = 0; p < Phalf; p++)
        {
          for (j = 0; j < Nhalf; j++)
          {
            for (i = 0; i < Mhalf; i++)
            {
              index1 = (p * Nhalf + j) * Mhalf + i;
              index2 = (2 * p * N + 2 * j) * M + 2 * i;
              
              w[0][1][0] = 0.25 * (i > 0 && p > 0 ? weight[index2 - MN - 1] : 0.0);
              w[0][1][1] = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
              w[0][1][2] = 0.25 * (i > 0 && p < Phalf - 1 ?  weight[index2 + MN - 1] : 0.0);
              w[0][2][0] = 0.25 * (i > 0 && p > 0 ? weight[index2 - MN + M - 1] : 0.0);
              w[0][2][1] = 0.5 * (i > 0 ? weight[index2 + M - 1] : 0.0);
              w[0][2][2] = 0.25 * (i > 0 && p < Phalf - 1 ? weight[index2 + MN + M - 1] : 0.0);
              w[1][1][0] = 0.5 * (p > 0 ?  weight[index2 - MN] : 0.0);
              w[1][1][1] = weight[index2];
              w[1][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN] : 0.0);
              w[1][2][0] = 0.5 * (p > 0 ? weight[index2 - MN + M] : 0.0);
              w[1][2][1] = weight[index2 + M];
              w[1][2][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + M] : 0.0);
              w[2][1][0] = 0.25 * (i < Mhalf - 1 && p > 0 ? weight[index2 - MN + 1] : 0.0);
              w[2][1][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
              w[2][1][2] = 0.25 * (i < Mhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + 1] : 0.0);
              w[2][2][0] = 0.25 * (i < Mhalf - 1 && p > 0 ? weight[index2 - MN + M + 1] : 0.0);
              w[2][2][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + M + 1] : 0.0);
              w[2][2][2] = 0.25 * (i < Mhalf - 1 && p < Phalf - 1 ?  weight[index2 + MN + M + 1] : 0.0);
              
              sum = w[0][1][0] + w[0][1][1] + w[0][1][2] + w[0][2][0] + w[0][2][1] + w[0][2][2] 
                  + w[1][1][0] + w[1][1][1] + w[1][1][2] + w[1][2][0] + w[1][2][1] + w[1][2][2] 
                  + w[2][1][0] + w[2][1][1] + w[2][1][2] + w[2][2][0] + w[2][2][1] + w[2][2][2];
              coarse_weight[index1] = sum;
              
              if (sum > 0)
              {
                double result = 0;
                if (w[0][1][0] > 0)
                  result += w[0][1][0] * rhs[index2 - MN - 1];
                if (w[0][1][1] > 0)
                  result += w[0][1][1] * rhs[index2 - 1];
                if (w[0][1][2] > 0)
                  result += w[0][1][2] * rhs[index2 + MN - 1];
                if (w[0][2][0] > 0)
                  result += w[0][2][0] * rhs[index2 - MN + M - 1];
                if (w[0][2][1] > 0)
                  result += w[0][2][1] * rhs[index2 + M - 1];
                if (w[0][2][2] > 0)
                  result += w[0][2][2] * rhs[index2 + MN + M - 1];
                if (w[1][1][0] > 0)
                  result += w[1][1][0] * rhs[index2 - MN];
                if (w[1][1][1] > 0)
                  result += w[1][1][1] * rhs[index2];
                if (w[1][1][2] > 0)
                  result += w[1][1][2] * rhs[index2 + MN];
                if (w[1][2][0] > 0)
                  result += w[1][2][0] * rhs[index2 - MN + M];
                if (w[1][2][1] > 0)
                  result += w[1][2][1] * rhs[index2 + M];
                if (w[1][2][2] > 0)
                  result += w[1][2][2] * rhs[index2 + MN + M];
                if (w[2][1][0] > 0)
                  result += w[2][1][0] * rhs[index2 - MN + 1];
                if (w[2][1][1] > 0)
                  result += w[2][1][1] * rhs[index2 + 1];
                if (w[2][1][2] > 0)
                  result += w[2][1][2] * rhs[index2 + MN + 1];
                if (w[2][2][0] > 0)
                  result += w[2][2][0] * rhs[index2 - MN + M + 1];
                if (w[2][2][1] > 0)
                  result += w[2][2][1] * rhs[index2 + M + 1];
                if (w[2][2][2] > 0)
                  result += w[2][2][2] * rhs[index2 + MN + M + 1];
                
                rhs_coarse[index1] = 8 / sum * result;
              }
            }
          }
        }
      }
      
      if (M % 2 == 0 && N % 2 == 1 && P % 2 == 1)
      {
        for (p = 0; p < Phalf; p++)
        {
          for (j = 0; j < Nhalf; j++)
          {
            for (i = 0; i < Mhalf; i++)
            {
              index1 = (p * Nhalf + j) * Mhalf + i;
              index2 = (2 * p * N + 2 * j) * M + 2 * i;
              
              w[1][0][0] = 0.25 * (j > 0 && p > 0 ? weight[index2 - MN - M] : 0.0);
              w[1][0][1] = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
              w[1][0][2] = 0.25 * (j > 0 && p < Phalf - 1 ? weight[index2 + MN - M] : 0.0);
              w[1][1][0] = 0.5 * (p > 0 ? weight[index2 - MN] : 0.0);
              w[1][1][1] = weight[index2];
              w[1][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN] : 0.0);
              w[1][2][0] = 0.25 * (j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M] : 0.0);
              w[1][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
              w[1][2][2] = 0.25 * (j < Nhalf - 1 && p < Phalf - 1 ?  weight[index2 + MN + M] : 0.0);
              w[2][0][0] = 0.25 * (j > 0 && p > 0 ?  weight[index2 - MN - M + 1] : 0.0);
              w[2][0][1] = 0.5 * (j > 0 ?  weight[index2 - M + 1] : 0.0);
              w[2][0][2] = 0.25 * (j > 0 && p < Phalf - 1 ? weight[index2 + MN - M + 1] : 0.0);
              w[2][1][0] = 0.5 * (p > 0 ? weight[index2 - MN + 1] : 0.0);
              w[2][1][1] = weight[index2 + 1];
              w[2][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + 1] : 0.0);
              w[2][2][0] = 0.25 * (j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M + 1] : 0.0);
              w[2][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
              w[2][2][2] = 0.25 * (j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
              
              sum = w[1][0][0] + w[1][0][1] + w[1][0][2] + w[1][1][0] + w[1][1][1] + w[1][1][2] 
                  + w[1][2][0] + w[1][2][1] + w[1][2][2] + w[2][0][0] + w[2][0][1] + w[2][0][2] 
                  + w[2][1][0] + w[2][1][1] + w[2][1][2] + w[2][2][0] + w[2][2][1] + w[2][2][2];
              coarse_weight[index1] = sum;
              
              if (sum > 0)
              {
                double result = 0;
                if (w[1][0][0] > 0)
                  result += w[1][0][0] * rhs[index2 - MN - M];
                if (w[1][0][1] > 0)
                  result += w[1][0][1] * rhs[index2 - M];
                if (w[1][0][2] > 0)
                  result += w[1][0][2] * rhs[index2 + MN - M];
                if (w[1][1][0] > 0)
                  result += w[1][1][0] * rhs[index2 - MN];
                if (w[1][1][1] > 0)
                  result += w[1][1][1] * rhs[index2];
                if (w[1][1][2] > 0)
                  result += w[1][1][2] * rhs[index2 + MN];
                if (w[1][2][0] > 0)
                  result += w[1][2][0] * rhs[index2 - MN + M];
                if (w[1][2][1] > 0)
                  result += w[1][2][1] * rhs[index2 + M];
                if (w[1][2][2] > 0)
                  result += w[1][2][2] * rhs[index2 + MN + M];
                if (w[2][0][0] > 0)
                  result += w[2][0][0] * rhs[index2 - MN - M + 1];
                if (w[2][0][1] > 0)
                  result += w[2][0][1] * rhs[index2 - M + 1];
                if (w[2][0][2] > 0)
                  result += w[2][0][2] * rhs[index2 + MN - M + 1];
                if (w[2][1][0] > 0)
                  result += w[2][1][0] * rhs[index2 - MN + 1];
                if (w[2][1][1] > 0)
                  result += w[2][1][1] * rhs[index2 + 1];
                if (w[2][1][2] > 0)
                  result += w[2][1][2] * rhs[index2 + MN + 1];
                if (w[2][2][0] > 0)
                  result += w[2][2][0] * rhs[index2 - MN + M + 1];
                if (w[2][2][1] > 0)
                  result += w[2][2][1] * rhs[index2 + M + 1];
                if (w[2][2][2] > 0)
                  result += w[2][2][2] * rhs[index2 + MN + M + 1];
                
                rhs_coarse[index1] = 8 / sum * result;
              }
            }
          }
        }
      }
      
      if (M % 2 == 1 && N % 2 == 1 && P % 2 == 1)
      {
        for (p = 0; p < Phalf; p++)
        {
          for (j = 0; j < Nhalf; j++)
          {
            for (i = 0; i < Mhalf; i++)
            {
              index1 = (p * Nhalf + j) * Mhalf + i;
              index2 = (2 * p * N + 2 * j) * M + 2 * i;
              
              w[0][0][0] = 0.125 * (i > 0 && j > 0 && p > 0 ? weight[index2 - MN - M - 1] : 0.0);
              w[0][0][1] = 0.25 * (i > 0 && j > 0 ? weight[index2 - M - 1] : 0.0);
              w[0][0][2] = 0.125 * (i > 0 && j > 0 && p < Phalf - 1 ? weight[index2 + MN - M - 1] : 0.0);
              w[0][1][0] = 0.25 * (i > 0 && p > 0 ? weight[index2 - MN - 1] : 0.0);
              w[0][1][1] = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
              w[0][1][2] = 0.25 * (i > 0 && p < Phalf - 1 ? weight[index2 + MN - 1] : 0.0);
              w[0][2][0] = 0.125 * (i > 0 && j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M - 1] : 0.0);
              w[0][2][1] = 0.25 * (i > 0 && j < Nhalf - 1 ? weight[index2 + M - 1] : 0.0);
              w[0][2][2] = 0.125 * (i > 0 && j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M - 1] : 0.0);
              w[1][0][0] = 0.25 * (j > 0 && p > 0 ? weight[index2 - MN - M] : 0.0);
              w[1][0][1] = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
              w[1][0][2] = 0.25 * (j > 0 && p < Phalf - 1 ? weight[index2 + MN - M] : 0.0);
              w[1][1][0] = 0.5 * (p > 0 ? weight[index2 - MN] : 0.0);
              w[1][1][1] = weight[index2];
              w[1][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN] : 0.0);
              w[1][2][0] = 0.25 * (j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M] : 0.0);
              w[1][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
              w[1][2][2] = 0.25 * (j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M] : 0.0);
              w[2][0][0] = 0.125 * (i < Mhalf - 1 && j > 0 && p > 0 ? weight[index2 - MN - M + 1] : 0.0);
              w[2][0][1] = 0.25 * (i < Mhalf - 1 && j > 0 ? weight[index2 - M + 1] : 0.0);
              w[2][0][2] = 0.125 * (i < Mhalf - 1 && j > 0 && p < Phalf - 1 ? weight[index2 + MN - M + 1] : 0.0);
              w[2][1][0] = 0.25 * (i < Mhalf - 1 && p > 0 ? weight[index2 - MN + 1] : 0.0);
              w[2][1][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
              w[2][1][2] = 0.25 * (i < Mhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + 1] : 0.0);
              w[2][2][0] = 0.125 * (i < Mhalf - 1 && j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M + 1] : 0.0);
              w[2][2][1] = 0.25 * (i < Mhalf - 1 && j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
              w[2][2][2] = 0.125 * (i < Mhalf - 1 && j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
              
              sum = w[0][0][0] + w[0][0][1] + w[0][0][2] + w[0][1][0] + w[0][1][1] + w[0][1][2] + w[0][2][0] 
                  + w[0][2][1] + w[0][2][2] + w[1][0][0] + w[1][0][1] + w[1][0][2] + w[1][1][0] + w[1][1][1] 
                  + w[1][1][2] + w[1][2][0] + w[1][2][1] + w[1][2][2] + w[2][0][0] + w[2][0][1] + w[2][0][2] 
                  + w[2][1][0] + w[2][1][1] + w[2][1][2] + w[2][2][0] + w[2][2][1] + w[2][2][2];
              coarse_weight[index1] = sum;
              
              if (sum > 0)
              {
                double result = 0;
                if (w[0][0][0] > 0)
                  result += w[0][0][0] * rhs[index2 - MN - M - 1];
                if (w[0][0][1] > 0)
                  result += w[0][0][1] * rhs[index2 - M - 1];
                if (w[0][0][2] > 0)
                  result += w[0][0][2] * rhs[index2 + MN - M - 1];
                if (w[0][1][0] > 0)
                  result += w[0][1][0] * rhs[index2 - MN - 1];
                if (w[0][1][1] > 0)
                  result += w[0][1][1] * rhs[index2 - 1];
                if (w[0][1][2] > 0)
                  result += w[0][1][2] * rhs[index2 + MN - 1];
                if (w[0][2][0] > 0)
                  result += w[0][2][0] * rhs[index2 - MN + M - 1];
                if (w[0][2][1] > 0)
                  result += w[0][2][1] * rhs[index2 + M - 1];
                if (w[0][2][2] > 0)
                  result += w[0][2][2] * rhs[index2 + MN + M - 1];
                if (w[1][0][0] > 0)
                  result += w[1][0][0] * rhs[index2 - MN - M];
                if (w[1][0][1] > 0)
                  result += w[1][0][1] * rhs[index2 - M];
                if (w[1][0][2] > 0)
                  result += w[1][0][2] * rhs[index2 + MN - M];
                if (w[1][1][0] > 0)
                  result += w[1][1][0] * rhs[index2 - MN];
                if (w[1][1][1] > 0)
                  result += w[1][1][1] * rhs[index2];
                if (w[1][1][2] > 0)
                  result += w[1][1][2] * rhs[index2 + MN];
                if (w[1][2][0] > 0)
                  result += w[1][2][0] * rhs[index2 - MN + M];
                if (w[1][2][1] > 0)
                  result += w[1][2][1] * rhs[index2 + M];
                if (w[1][2][2] > 0)
                  result += w[1][2][2] * rhs[index2 + MN + M];
                if (w[2][0][0] > 0)
                  result += w[2][0][0] * rhs[index2 - MN - M + 1];
                if (w[2][0][1] > 0)
                  result += w[2][0][1] * rhs[index2 - M + 1];
                if (w[2][0][2] > 0)
                  result += w[2][0][2] * rhs[index2 + MN - M + 1];
                if (w[2][1][0] > 0)
                  result += w[2][1][0] * rhs[index2 - MN + 1];
                if (w[2][1][1] > 0)
                  result += w[2][1][1] * rhs[index2 + 1];
                if (w[2][1][2] > 0)
                  result += w[2][1][2] * rhs[index2 + MN + 1];
                if (w[2][2][0] > 0)
                  result += w[2][2][0] * rhs[index2 - MN + M + 1];
                if (w[2][2][1] > 0)
                  result += w[2][2][1] * rhs[index2 + M + 1];
                if (w[2][2][2] > 0)
                  result += w[2][2][2] * rhs[index2 + MN + M + 1];
                
                rhs_coarse[index1] = 8 / sum * result;
              }
            }
          }
        }
      }
    }
    
    private void galerkin3D(int level, int M, int N, int P, int Mhalf, int Nhalf, int Phalf,
            double[] weight, double[] coarse_weight)
 {
   int i, j, p;
   int k;
   double lhs[];
   double lhs_coarse[];
   int MN = M * N;
   int MNhalf = Mhalf * Nhalf;
   double lw[][][] = new double[3][3][3];
   double mean;
   
   if (data.lhs[level + 1] != null)
     return;
   
   data.lhs[level + 1] = new double[27 * Mhalf * Nhalf * Phalf];
   lhs = data.lhs[level];
   lhs_coarse = data.lhs[level + 1];
   
   for (p = 0; p < Phalf; p++)
   {
     for (j = 0; j < Nhalf; j++)
     {
       for (i = 0; i < Mhalf; i++)
       {
         int index1 = ((p * Nhalf + j) * Mhalf + i);
         int index2 = ((2 * p * N + 2 * j) * M + 2 * i);
         double stencil1[][][] = new double[3][3][3];
         double stencil2[][][] = new double[5][5][5];
         double stencil3[][][] = new double[3][3][3];
         double mask1[][][] = new double[3][3][3];
         int u, v, w;
         
         for (u = 0; u < 5; u++)
         {
           for (v = 0; v < 5; v++)
           {
             for (w = 0; w < 5; w++)
             {
               stencil2[u][v][w] = 0;
               if (u < 3 && v < 3 && w < 3)
               {
                 stencil1[u][v][w] = 0;
                 stencil3[u][v][w] = 0;
               }
             }
           }
         }
         mask1[0][0][0] = (i > 0 && j > 0 && p > 0 ?
                              coarse_weight[index1 - MNhalf - Mhalf - 1] : 0.0);
         mask1[0][0][1] = (i > 0 && j > 0 ?
                              coarse_weight[index1 - Mhalf - 1] : 0.0);
         mask1[0][0][2] = (i > 0 && j > 0 && p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf - Mhalf - 1] : 0.0);
         mask1[0][1][0] = (i > 0 && p > 0 ?
                              coarse_weight[index1 - MNhalf - 1] : 0.0);
         mask1[0][1][1] = (i > 0 ?
                              coarse_weight[index1 - 1] : 0.0);
         mask1[0][1][2] = (i > 0 && p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf - 1] : 0.0);
         mask1[0][2][0] = (i > 0 && j < Nhalf - 1 && p > 0 ?
                              coarse_weight[index1 - MNhalf + Mhalf - 1] : 0.0);
         mask1[0][2][1] = (i > 0 && j < Nhalf - 1 ?
                              coarse_weight[index1 + Mhalf - 1] : 0.0);
         mask1[0][2][2] = (i > 0 && j < Nhalf - 1 && p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf + Mhalf - 1] : 0.0);
         mask1[1][0][0] = (j > 0 && p > 0 ?
                              coarse_weight[index1 - MNhalf - Mhalf] : 0.0);
         mask1[1][0][1] = (j > 0 ?
                              coarse_weight[index1 - Mhalf] : 0.0);
         mask1[1][0][2] = (j > 0 && p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf - Mhalf] : 0.0);
         mask1[1][1][0] = (p > 0 ?
                              coarse_weight[index1 - MNhalf] : 0.0);
         mask1[1][1][1] = coarse_weight[index1];
         mask1[1][1][2] = (p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf] : 0.0);
         mask1[1][2][0] = (j < Nhalf - 1 && p > 0 ?
                              coarse_weight[index1 - MNhalf + Mhalf] : 0.0);
         mask1[1][2][1] = (j < Nhalf - 1 ?
                              coarse_weight[index1 + Mhalf] : 0.0);
         mask1[1][2][2] = (j < Nhalf - 1 && p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf + Mhalf] : 0.0);
         mask1[2][0][0] = (i < Mhalf - 1 && j > 0 && p > 0 ?
                              coarse_weight[index1 - MNhalf - Mhalf + 1] : 0.0);
         mask1[2][0][1] = (i < Mhalf - 1 && j > 0 ?
                              coarse_weight[index1 - Mhalf + 1] : 0.0);
         mask1[2][0][2] = (i < Mhalf - 1 && j > 0 && p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf - Mhalf + 1] : 0.0);
         mask1[2][1][0] = (i < Mhalf - 1 && p > 0 ?
                              coarse_weight[index1 - MNhalf + 1] : 0.0);
         mask1[2][1][1] = (i < Mhalf - 1 ?
                              coarse_weight[index1 + 1] : 0.0);
         mask1[2][1][2] = (i < Mhalf - 1 && p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf + 1] : 0.0);
         mask1[2][2][0] = (i < Mhalf - 1 && j < Nhalf - 1 && p > 0 ?
                              coarse_weight[index1 - MNhalf + Mhalf + 1] : 0.0);
         mask1[2][2][1] = (i < Mhalf - 1 && j < Nhalf - 1 ?
                              coarse_weight[index1 + Mhalf + 1] : 0.0);
         mask1[2][2][2] = (i < Mhalf - 1 && j < Nhalf - 1 && p < Phalf - 1 ?
                              coarse_weight[index1 + MNhalf + Mhalf + 1] : 0.0);
         
         if (M % 2 == 0 && N % 2 == 0 && P % 2 == 0)
         {
           lw[1][1][1] = weight[index2];
           lw[1][1][2] = weight[index2 + MN];
           lw[1][2][1] = weight[index2 + M];
           lw[1][2][2] = weight[index2 + MN + M];
           lw[2][1][1] = weight[index2 + 1];
           lw[2][1][2] = weight[index2 + MN + 1];
           lw[2][2][1] = weight[index2 + M + 1];
           lw[2][2][2] = weight[index2 + MN + M + 1];
           
           mean = (lw[1][1][1] + lw[1][1][2] + lw[1][2][1] + lw[1][2][2] + lw[2][1][1] + lw[2][1][2] + lw[2][2][1] + lw[2][2][2]) / 8;
           
           if (mean == 0)
             continue;
           
           stencil1[1][1][1] = lw[1][1][1] / mean;
           stencil1[1][1][2] = lw[1][1][2] / mean;
           stencil1[1][2][1] = lw[1][2][1] / mean;
           stencil1[1][2][2] = lw[1][2][2] / mean;
           stencil1[2][1][1] = lw[2][1][1] / mean;
           stencil1[2][1][2] = lw[2][1][2] / mean;
           stencil1[2][2][1] = lw[2][2][1] / mean;
           stencil1[2][2][2] = lw[2][2][2] / mean;
         }
         
         if (M % 2 == 1 && N % 2 == 0 && P % 2 == 0)
         {
           lw[0][1][1] = 0.5 * (i > 0 ? weight[index2 - 1] : 0);
           lw[0][1][2] = 0.5 * (i > 0 ? weight[index2 + MN - 1] : 0.0);
           lw[0][2][1] = 0.5 * (i > 0 ? weight[index2 + M - 1] : 0.0);
           lw[0][2][2] = 0.5 * (i > 0 ? weight[index2 + MN + M - 1] : 0.0);
           lw[1][1][1] = weight[index2];
           lw[1][1][2] = weight[index2 + MN];
           lw[1][2][1] = weight[index2 + M];
           lw[1][2][2] = weight[index2 + MN + M];
           lw[2][1][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
           lw[2][1][2] = 0.5 * (i < Mhalf - 1 ? weight[index2 + MN + 1] : 0.0);
           lw[2][2][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + M + 1] : 0.0);
           lw[2][2][2] = 0.5 * (i < Mhalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
           
           mean = (lw[0][1][1] + lw[0][1][2] + lw[0][2][1] + lw[0][2][2] + lw[1][1][1] + lw[1][1][2] 
                 + lw[1][2][1] + lw[1][2][2] + lw[2][1][1] + lw[2][1][2] + lw[2][2][1] + lw[2][2][2]) / 8;
           
           if (mean == 0)
             continue;
           
           stencil1[0][1][1] = lw[0][1][1] / mean;
           stencil1[0][1][2] = lw[0][1][2] / mean;
           stencil1[0][2][1] = lw[0][2][1] / mean;
           stencil1[0][2][2] = lw[0][2][2] / mean;
           stencil1[1][1][1] = lw[1][1][1] / mean;
           stencil1[1][1][2] = lw[1][1][2] / mean;
           stencil1[1][2][1] = lw[1][2][1] / mean;
           stencil1[1][2][2] = lw[1][2][2] / mean;
           stencil1[2][1][1] = lw[2][1][1] / mean;
           stencil1[2][1][2] = lw[2][1][2] / mean;
           stencil1[2][2][1] = lw[2][2][1] / mean;
           stencil1[2][2][2] = lw[2][2][2] / mean;
         }
         
         if (M % 2 == 0 && N % 2 == 1 && P % 2 == 0)
         {
           lw[1][0][1] = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
           lw[1][0][2] = 0.5 * (j > 0 ? weight[index2 + MN - M] : 0.0);
           lw[1][1][1] = weight[index2];
           lw[1][1][2] = weight[index2 + MN];
           lw[1][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
           lw[1][2][2] = 0.5 * (j < Nhalf - 1 ? weight[index2 + MN + M] : 0.0);
           lw[2][0][1] = 0.5 * (j > 0 ? weight[index2 - M + 1] : 0.0);
           lw[2][0][2] = 0.5 * (j > 0 ? weight[index2 + MN - M + 1] : 0.0);
           lw[2][1][1] = weight[index2 + 1];
           lw[2][1][2] = weight[index2 + MN + 1];
           lw[2][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
           lw[2][2][2] = 0.5 * (j < Nhalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
           
           mean = (lw[1][0][1] + lw[1][0][2] + lw[1][1][1] + lw[1][1][2] + lw[1][2][1] + lw[1][2][2] 
                 + lw[2][0][1] + lw[2][0][2] + lw[2][1][1] + lw[2][1][2] + lw[2][2][1] + lw[2][2][2]) / 8;
           
           if (mean == 0)
             continue;
           
           stencil1[1][0][1] = lw[1][0][1] / mean;
           stencil1[1][0][2] = lw[1][0][2] / mean;
           stencil1[1][1][1] = lw[1][1][1] / mean;
           stencil1[1][1][2] = lw[1][1][2] / mean;
           stencil1[1][2][1] = lw[1][2][1] / mean;
           stencil1[1][2][2] = lw[1][2][2] / mean;
           stencil1[2][0][1] = lw[2][0][1] / mean;
           stencil1[2][0][2] = lw[2][0][2] / mean;
           stencil1[2][1][1] = lw[2][1][1] / mean;
           stencil1[2][1][2] = lw[2][1][2] / mean;
           stencil1[2][2][1] = lw[2][2][1] / mean;
           stencil1[2][2][2] = lw[2][2][2] / mean;
         }
         
         if (M % 2 == 1 && N % 2 == 1 && P % 2 == 0)
         {
           lw[0][0][1] = 0.25 * (i > 0 && j > 0 ? weight[index2 - M - 1] : 0.0);
           lw[0][0][2] = 0.25 * (i > 0 && j > 0 ? weight[index2 + MN - M - 1] : 0.0);
           lw[0][1][1] = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
           lw[0][1][2] = 0.5 * (i > 0 ? weight[index2 + MN - 1] : 0.0);
           lw[0][2][1] = 0.25 * (i > 0 && j < Nhalf - 1 ? weight[index2 + M - 1] : 0.0);
           lw[0][2][2] = 0.25 * (i > 0 && j < Nhalf - 1 ? weight[index2 + MN + M - 1] : 0.0);
           lw[1][0][1] = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
           lw[1][0][2] = 0.5 * (j > 0 ? weight[index2 + MN - M] : 0.0);
           lw[1][1][1] = weight[index2];
           lw[1][1][2] = weight[index2 + MN];
           lw[1][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
           lw[1][2][2] = 0.5 * (j < Nhalf - 1 ? weight[index2 + MN + M] : 0.0);
           lw[2][0][1] = 0.25 * (i < Mhalf - 1 && j > 0 ? weight[index2 - M + 1] : 0.0);
           lw[2][0][2] = 0.25 * (i < Mhalf - 1 && j > 0 ? weight[index2 + MN - M + 1] : 0.0);
           lw[2][1][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
           lw[2][1][2] = 0.5 * (i < Mhalf - 1 ? weight[index2 + MN + 1] : 0.0);
           lw[2][2][1] = 0.25 * (i < Mhalf - 1 && j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
           lw[2][2][2] = 0.25 * (i < Mhalf - 1 && j < Nhalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
           
           mean = (lw[0][0][1] + lw[0][0][2] + lw[0][1][1] + lw[0][1][2] + lw[0][2][1] + lw[0][2][2] 
                 + lw[1][0][1] + lw[1][0][2] + lw[1][1][1] + lw[1][1][2] + lw[1][2][1] + lw[1][2][2] 
                 + lw[2][0][1] + lw[2][0][2] + lw[2][1][1] + lw[2][1][2] + lw[2][2][1] + lw[2][2][2]) / 8;
           
           if (mean == 0)
             continue;
           
           stencil1[0][0][1] = lw[0][0][1] / mean;
           stencil1[0][0][2] = lw[0][0][2] / mean;
           stencil1[0][1][1] = lw[0][1][1] / mean;
           stencil1[0][1][2] = lw[0][1][2] / mean;
           stencil1[0][2][1] = lw[0][2][1] / mean;
           stencil1[0][2][2] = lw[0][2][2] / mean;
           stencil1[1][0][1] = lw[1][0][1] / mean;
           stencil1[1][0][2] = lw[1][0][2] / mean;
           stencil1[1][1][1] = lw[1][1][1] / mean;
           stencil1[1][1][2] = lw[1][1][2] / mean;
           stencil1[1][2][1] = lw[1][2][1] / mean;
           stencil1[1][2][2] = lw[1][2][2] / mean;
           stencil1[2][0][1] = lw[2][0][1] / mean;
           stencil1[2][0][2] = lw[2][0][2] / mean;
           stencil1[2][1][1] = lw[2][1][1] / mean;
           stencil1[2][1][2] = lw[2][1][2] / mean;
           stencil1[2][2][1] = lw[2][2][1] / mean;
           stencil1[2][2][2] = lw[2][2][2] / mean;
         }
         
         if (M % 2 == 0 && N % 2 == 0 && P % 2 == 1)
         {
           lw[1][1][0] = 0.5 * (p > 0 ? weight[index2 - MN] : 0.0);
           lw[1][1][1] = weight[index2];
           lw[1][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN] : 0.0);
           lw[1][2][0] = 0.5 * (p > 0 ? weight[index2 - MN + M] : 0.0);
           lw[1][2][1] = weight[index2 + M];
           lw[1][2][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + M] : 0.0);
           lw[2][1][0] = 0.5 * (p > 0 ? weight[index2 - MN + 1] : 0.0);
           lw[2][1][1] = weight[index2 + 1];
           lw[2][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + 1] : 0.0);
           lw[2][2][0] = 0.5 * (p > 0 ? weight[index2 - MN + M + 1] : 0.0);
           lw[2][2][1] = weight[index2 + M + 1];
           lw[2][2][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
           
           mean = (lw[1][1][0] + lw[1][1][1] + lw[1][1][2] + lw[1][2][0] + lw[1][2][1] + lw[1][2][2] 
                 + lw[2][1][0] + lw[2][1][1] + lw[2][1][2] + lw[2][2][0] + lw[2][2][1] + lw[2][2][2]) / 8;
           
           if (mean == 0)
             continue;
           
           stencil1[1][1][0] = lw[1][1][0] / mean;
           stencil1[1][1][1] = lw[1][1][1] / mean;
           stencil1[1][1][2] = lw[1][1][2] / mean;
           stencil1[1][2][0] = lw[1][2][0] / mean;
           stencil1[1][2][1] = lw[1][2][1] / mean;
           stencil1[1][2][2] = lw[1][2][2] / mean;
           stencil1[2][1][0] = lw[2][1][0] / mean;
           stencil1[2][1][1] = lw[2][1][1] / mean;
           stencil1[2][1][2] = lw[2][1][2] / mean;
           stencil1[2][2][0] = lw[2][2][0] / mean;
           stencil1[2][2][1] = lw[2][2][1] / mean;
           stencil1[2][2][2] = lw[2][2][2] / mean;
         }
         
         if (M % 2 == 1 && N % 2 == 0 && P % 2 == 1)
         {
           lw[0][1][0] = 0.25 * (i > 0 && p > 0 ? weight[index2 - MN - 1] : 0.0);
           lw[0][1][1] = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
           lw[0][1][2] = 0.25 * (i > 0 && p < Phalf - 1 ? weight[index2 + MN - 1] : 0.0);
           lw[0][2][0] = 0.25 * (i > 0 && p > 0 ? weight[index2 - MN + M - 1] : 0.0);
           lw[0][2][1] = 0.5 * (i > 0 ? weight[index2 + M - 1] : 0.0);
           lw[0][2][2] = 0.25 * (i > 0 && p < Phalf - 1 ? weight[index2 + MN + M - 1] : 0.0);
           lw[1][1][0] = 0.5 * (p > 0 ? weight[index2 - MN] : 0.0);
           lw[1][1][1] = weight[index2];
           lw[1][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN] : 0.0);
           lw[1][2][0] = 0.5 * (p > 0 ? weight[index2 - MN + M] : 0.0);
           lw[1][2][1] = weight[index2 + M];
           lw[1][2][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + M] : 0.0);
           lw[2][1][0] = 0.25 * (i < Mhalf - 1 && p > 0 ? weight[index2 - MN + 1] : 0.0);
           lw[2][1][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
           lw[2][1][2] = 0.25 * (i < Mhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + 1] : 0.0);
           lw[2][2][0] = 0.25 * (i < Mhalf - 1 && p > 0 ? weight[index2 - MN + M + 1] : 0.0);
           lw[2][2][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + M + 1] : 0.0);
           lw[2][2][2] = 0.25 * (i < Mhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
           
           mean = (lw[0][1][0] + lw[0][1][1] + lw[0][1][2] + lw[0][2][0] + lw[0][2][1] + lw[0][2][2] 
                 + lw[1][1][0] + lw[1][1][1] + lw[1][1][2] + lw[1][2][0] + lw[1][2][1] + lw[1][2][2] 
                 + lw[2][1][0] + lw[2][1][1] + lw[2][1][2] + lw[2][2][0] + lw[2][2][1] + lw[2][2][2]) / 8;
           
           if (mean == 0)
             continue;
           
           stencil1[0][1][0] = lw[0][1][0] / mean;
           stencil1[0][1][1] = lw[0][1][1] / mean;
           stencil1[0][1][2] = lw[0][1][2] / mean;
           stencil1[0][2][0] = lw[0][2][0] / mean;
           stencil1[0][2][1] = lw[0][2][1] / mean;
           stencil1[0][2][2] = lw[0][2][2] / mean;
           stencil1[1][1][0] = lw[1][1][0] / mean;
           stencil1[1][1][1] = lw[1][1][1] / mean;
           stencil1[1][1][2] = lw[1][1][2] / mean;
           stencil1[1][2][0] = lw[1][2][0] / mean;
           stencil1[1][2][1] = lw[1][2][1] / mean;
           stencil1[1][2][2] = lw[1][2][2] / mean;
           stencil1[2][1][0] = lw[2][1][0] / mean;
           stencil1[2][1][1] = lw[2][1][1] / mean;
           stencil1[2][1][2] = lw[2][1][2] / mean;
           stencil1[2][2][0] = lw[2][2][0] / mean;
           stencil1[2][2][1] = lw[2][2][1] / mean;
           stencil1[2][2][2] = lw[2][2][2] / mean;
         }
         
         if (M % 2 == 0 && N % 2 == 1 && P % 2 == 1)
         {
           lw[1][0][0] = 0.25 * (j > 0 && p > 0 ? weight[index2 - MN - M] : 0.0);
           lw[1][0][1] = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
           lw[1][0][2] = 0.25 * (j > 0 && p < Phalf - 1 ? weight[index2 + MN - M] : 0.0);
           lw[1][1][0] = 0.5 * (p > 0 ? weight[index2 - MN] : 0.0);
           lw[1][1][1] = weight[index2];
           lw[1][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN] : 0.0);
           lw[1][2][0] = 0.25 * (j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M] : 0.0);
           lw[1][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
           lw[1][2][2] = 0.25 * (j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M] : 0.0);
           lw[2][0][0] = 0.25 * (j > 0 && p > 0 ? weight[index2 - MN - M + 1] : 0.0);
           lw[2][0][1] = 0.5 * (j > 0 ? weight[index2 - M + 1] : 0.0);
           lw[2][0][2] = 0.25 * (j > 0 && p < Phalf - 1 ? weight[index2 + MN - M + 1] : 0.0);
           lw[2][1][0] = 0.5 * (p > 0 ? weight[index2 - MN + 1] : 0.0);
           lw[2][1][1] = weight[index2 + 1];
           lw[2][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN + 1] : 0.0);
           lw[2][2][0] = 0.25 * (j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M + 1] : 0.0);
           lw[2][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
           lw[2][2][2] = 0.25 * (j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
           
           mean = (lw[1][0][0] + lw[1][0][1] + lw[1][0][2] + lw[1][1][0] + lw[1][1][1] + lw[1][1][2] 
                 + lw[1][2][0] + lw[1][2][1] + lw[1][2][2] + lw[2][0][0] + lw[2][0][1] + lw[2][0][2] 
                 + lw[2][1][0] + lw[2][1][1] + lw[2][1][2] + lw[2][2][0] + lw[2][2][1] + lw[2][2][2]) / 8;
           
           if (mean == 0)
             continue;
           
           stencil1[1][0][0] = lw[1][0][0] / mean;
           stencil1[1][0][1] = lw[1][0][1] / mean;
           stencil1[1][0][2] = lw[1][0][2] / mean;
           stencil1[1][1][0] = lw[1][1][0] / mean;
           stencil1[1][1][1] = lw[1][1][1] / mean;
           stencil1[1][1][2] = lw[1][1][2] / mean;
           stencil1[1][2][0] = lw[1][2][0] / mean;
           stencil1[1][2][1] = lw[1][2][1] / mean;
           stencil1[1][2][2] = lw[1][2][2] / mean;
           stencil1[2][0][0] = lw[2][0][0] / mean;
           stencil1[2][0][1] = lw[2][0][1] / mean;
           stencil1[2][0][2] = lw[2][0][2] / mean;
           stencil1[2][1][0] = lw[2][1][0] / mean;
           stencil1[2][1][1] = lw[2][1][1] / mean;
           stencil1[2][1][2] = lw[2][1][2] / mean;
           stencil1[2][2][0] = lw[2][2][0] / mean;
           stencil1[2][2][1] = lw[2][2][1] / mean;
           stencil1[2][2][2] = lw[2][2][2] / mean;
         }
         
         if (M % 2 == 1 && N % 2 == 1 && P % 2 == 1)
         {
           lw[0][0][0] = 0.125 * (i > 0 && j > 0 && p > 0 ? weight[index2 - MN - M - 1] : 0.0);
           lw[0][0][1] = 0.25 * (i > 0 && j > 0 ? weight[index2 - M - 1] : 0.0);
           lw[0][0][2] = 0.125 * (i > 0 && j > 0 && p < Phalf - 1 ? weight[index2 + MN - M - 1] : 0.0);
           lw[0][1][0] = 0.25 * (i > 0 && p > 0 ? weight[index2 - MN - 1] : 0.0);
           lw[0][1][1] = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
           lw[0][1][2] = 0.25 * (i > 0 && p < Phalf - 1 ? weight[index2 + MN - 1] : 0.0);
           lw[0][2][0] = 0.125 * (i > 0 && j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M - 1] : 0.0);
           lw[0][2][1] = 0.25 * (i > 0 && j < Nhalf - 1 ? weight[index2 + M - 1] : 0.0);
           lw[0][2][2] = 0.125 * (i > 0 && j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M - 1] : 0.0);
           lw[1][0][0] = 0.25 * (j > 0 && p > 0 ? weight[index2 - MN - M] : 0.0);
           lw[1][0][1] = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
           lw[1][0][2] = 0.25 * (j > 0 && p < Phalf - 1 ? weight[index2 + MN - M] : 0.0);
           lw[1][1][0] = 0.5 * (p > 0 ? weight[index2 - MN] : 0.0);
           lw[1][1][1] = weight[index2];
           lw[1][1][2] = 0.5 * (p < Phalf - 1 ? weight[index2 + MN] : 0.0);
           lw[1][2][0] = 0.25 * (j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M] : 0.0);
           lw[1][2][1] = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
           lw[1][2][2] = 0.25 * (j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M] : 0.0);
           lw[2][0][0] = 0.125 * (i < Mhalf - 1 && j > 0 && p > 0 ? weight[index2 - MN - M + 1] : 0.0);
           lw[2][0][1] = 0.25 * (i < Mhalf - 1 && j > 0 ? weight[index2 - M + 1] : 0.0);
           lw[2][0][2] = 0.125 * (i < Mhalf - 1 && j > 0 && p < Phalf - 1 ? weight[index2 + MN - M + 1] : 0.0);
           lw[2][1][0] = 0.25 * (i < Mhalf - 1 && p > 0 ? weight[index2 - MN + 1] : 0.0);
           lw[2][1][1] = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
           lw[2][1][2] = 0.25 * (i < Mhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + 1] : 0.0);
           lw[2][2][0] = 0.125 * (i < Mhalf - 1 && j < Nhalf - 1 && p > 0 ? weight[index2 - MN + M + 1] : 0.0);
           lw[2][2][1] = 0.25 * (i < Mhalf - 1 && j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
           lw[2][2][2] = 0.125 * (i < Mhalf - 1 && j < Nhalf - 1 && p < Phalf - 1 ? weight[index2 + MN + M + 1] : 0.0);
           
           mean = (lw[0][0][0] + lw[0][0][1] + lw[0][0][2] + lw[0][1][0] + lw[0][1][1] + lw[0][1][2] + lw[0][2][0] 
                 + lw[0][2][1] + lw[0][2][2] + lw[1][0][0] + lw[1][0][1] + lw[1][0][2] + lw[1][1][0] + lw[1][1][1] 
                 + lw[1][1][2] + lw[1][2][0] + lw[1][2][1] + lw[1][2][2] + lw[2][0][0] + lw[2][0][1] + lw[2][0][2] 
                 + lw[2][1][0] + lw[2][1][1] + lw[2][1][2] + lw[2][2][0] + lw[2][2][1] + lw[2][2][2]) / 8;
           
           if (mean == 0)
             continue;
           
           stencil1[0][0][0] = lw[0][0][0] / mean;
           stencil1[0][0][1] = lw[0][0][1] / mean;
           stencil1[0][0][2] = lw[0][0][2] / mean;
           stencil1[0][1][0] = lw[0][1][0] / mean;
           stencil1[0][1][1] = lw[0][1][1] / mean;
           stencil1[0][1][2] = lw[0][1][2] / mean;
           stencil1[0][2][0] = lw[0][2][0] / mean;
           stencil1[0][2][1] = lw[0][2][1] / mean;
           stencil1[0][2][2] = lw[0][2][2] / mean;
           stencil1[1][0][0] = lw[1][0][0] / mean;
           stencil1[1][0][1] = lw[1][0][1] / mean;
           stencil1[1][0][2] = lw[1][0][2] / mean;
           stencil1[1][1][0] = lw[1][1][0] / mean;
           stencil1[1][1][1] = lw[1][1][1] / mean;
           stencil1[1][1][2] = lw[1][1][2] / mean;
           stencil1[1][2][0] = lw[1][2][0] / mean;
           stencil1[1][2][1] = lw[1][2][1] / mean;
           stencil1[1][2][2] = lw[1][2][2] / mean;
           stencil1[2][0][0] = lw[2][0][0] / mean;
           stencil1[2][0][1] = lw[2][0][1] / mean;
           stencil1[2][0][2] = lw[2][0][2] / mean;
           stencil1[2][1][0] = lw[2][1][0] / mean;
           stencil1[2][1][1] = lw[2][1][1] / mean;
           stencil1[2][1][2] = lw[2][1][2] / mean;
           stencil1[2][2][0] = lw[2][2][0] / mean;
           stencil1[2][2][1] = lw[2][2][1] / mean;
           stencil1[2][2][2] = lw[2][2][2] / mean;
         }
         
         for (u = 0; u < 3; u++)
         {
           for (v = 0; v < 3; v++)
           {
             for (w = 0; w < 3; w++)
             {
               if (stencil1[u][v][w] != 0)
               {
                 int index = 27 * (index2 + ((w - 1) * N + v - 1) * M + u - 1);
                 if (lhs[index + 13] != 0)
                 {
                   for (k = 0; k < 27; k++)
                   {
                     int a = (k % 3);
                     int b = ((k / 3) % 3);
                     int c = ((k / 9) % 3);
                     stencil2[u + a][v + b][w + c] += stencil1[u][v][w] * lhs[index + k];
                   }
                 }
               }
             }
           }
         }
         
         if (M % 2 == 0 && N % 2 == 0 && P % 2 == 0)
         {
           for (u = 1; u < 5; u++)
           {
             for (v = 1; v < 5; v++)
             {
               for (w = 1; w < 5; w++)
               {
                 double alpha1, alpha2, alpha3;
                 double unw, dnw, une, dne, usw, dsw, use, dse;
                 int uu, vv, ww;
                 double sum;
                 
                 if (u % 2 == 0)
                   alpha1 = 0.75;
                 else
                   alpha1 = 0.25;
                 
                 if (v % 2 == 0)
                   alpha2 = 0.75;
                 else
                   alpha2 = 0.25;
                 
                 if (w % 2 == 0)
                   alpha3 = 0.75;
                 else
                   alpha3 = 0.25;
                 
                 uu = (u - 1) / 2;
                 vv = (v - 1) / 2;
                 ww = (w - 1) / 2;
                 unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                        mask1[uu][vv][ww]);
                 dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                        (alpha3 > 0 ?
                            mask1[uu][vv][ww + 1] : 0));
                 une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                        (alpha2 > 0 ?
                            mask1[uu][vv + 1][ww] : 0.0));
                 dne = ((1 - alpha1) * alpha2 * alpha3 *
                        (alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu][vv + 1][ww + 1] : 0.0));
                 usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                        (alpha1 > 0 ?
                            mask1[uu + 1][vv][ww] : 0.0));
                 dsw = (alpha1 * (1 - alpha2) * alpha3 *
                        (alpha1 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv][ww + 1] : 0.0));
                 use = (alpha1 * alpha2 * (1 - alpha3) *
                        (alpha1 > 0 && alpha2 > 0 ?
                            mask1[uu + 1][vv + 1][ww] : 0.0));
                 dse = (alpha1 * alpha2 * alpha3 *
                        (alpha1 > 0 && alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv + 1][ww + 1] : 0.0));
                 
                 sum = unw + dnw + une + dne + usw + dsw + use + dse;
                 
                 if (sum > 0)
                 {
                   if (unw > 0)
                     stencil3[uu][vv][ww] += unw * stencil2[u][v][w] / sum;
                   if (dnw > 0)
                     stencil3[uu][vv][ww + 1] += dnw * stencil2[u][v][w] / sum;
                   if (une > 0)
                     stencil3[uu][vv + 1][ww] += une * stencil2[u][v][w] / sum;
                   if (dne > 0)
                     stencil3[uu][vv + 1][ww + 1] += dne * stencil2[u][v][w] / sum;
                   if (usw > 0)
                     stencil3[uu + 1][vv][ww] += usw * stencil2[u][v][w] / sum;
                   if (dsw > 0)
                     stencil3[uu + 1][vv][ww + 1] += dsw * stencil2[u][v][w] / sum;
                   if (use > 0)
                     stencil3[uu + 1][vv + 1][ww] += use * stencil2[u][v][w] / sum;
                   if (dse > 0)
                     stencil3[uu + 1][vv + 1][ww + 1] += dse * stencil2[u][v][w] / sum;
                 }
               }
             }
           }
         }
         
         if (M % 2 == 1 && N % 2 == 0 && P % 2 == 0)
         {
           for (u = 0; u < 5; u++)
           {
             for (v = 1; v < 5; v++)
             {
               for (w = 1; w < 5; w++)
               {
                 double alpha1, alpha2, alpha3;
                 double unw, dnw, une, dne, usw, dsw, use, dse;
                 int uu, vv, ww;
                 double sum;
                 
                 if (u % 2 == 0)
                   alpha1 = 0;
                 else
                   alpha1 = 0.5;
                 
                 if (v % 2 == 0)
                   alpha2 = 0.75;
                 else
                   alpha2 = 0.25;
                 
                 if (w % 2 == 0)
                   alpha3 = 0.75;
                 else
                   alpha3 = 0.25;
                 
                 uu = u / 2;
                 vv = (v - 1) / 2;
                 ww = (w - 1) / 2;
                 unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                        mask1[uu][vv][ww]);
                 dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                        (alpha3 > 0 ?
                            mask1[uu][vv][ww + 1] : 0.0));
                 une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                        (alpha2 > 0 ?
                            mask1[uu][vv + 1][ww] : 0.0));
                 dne = ((1 - alpha1) * alpha2 * alpha3 *
                        (alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu][vv + 1][ww + 1] : 0.0));
                 usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                        (alpha1 > 0 ?
                            mask1[uu + 1][vv][ww] : 0.0));
                 dsw = (alpha1 * (1 - alpha2) * alpha3 *
                        (alpha1 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv][ww + 1] : 0.0));
                 use = (alpha1 * alpha2 * (1 - alpha3) *
                        (alpha1 > 0 && alpha2 > 0 ?
                            mask1[uu + 1][vv + 1][ww] : 0.0));
                 dse = (alpha1 * alpha2 * alpha3 *
                        (alpha1 > 0 && alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv + 1][ww + 1] : 0.0));
                 
                 sum = unw + dnw + une + dne + usw + dsw + use + dse;
                 
                 if (sum > 0)
                 {
                   if (unw > 0)
                     stencil3[uu][vv][ww] += unw * stencil2[u][v][w] / sum;
                   if (dnw > 0)
                     stencil3[uu][vv][ww + 1] += dnw * stencil2[u][v][w] / sum;
                   if (une > 0)
                     stencil3[uu][vv + 1][ww] += une * stencil2[u][v][w] / sum;
                   if (dne > 0)
                     stencil3[uu][vv + 1][ww + 1] += dne * stencil2[u][v][w] / sum;
                   if (usw > 0)
                     stencil3[uu + 1][vv][ww] += usw * stencil2[u][v][w] / sum;
                   if (dsw > 0)
                     stencil3[uu + 1][vv][ww + 1] += dsw * stencil2[u][v][w] / sum;
                   if (use > 0)
                     stencil3[uu + 1][vv + 1][ww] += use * stencil2[u][v][w] / sum;
                   if (dse > 0)
                     stencil3[uu + 1][vv + 1][ww + 1] += dse * stencil2[u][v][w] / sum;
                 }
               }
             }
           }
         }
         
         if (M % 2 == 0 && N % 2 == 1 && P % 2 == 0)
         {
           for (u = 1; u < 5; u++)
           {
             for (v = 0; v < 5; v++)
             {
               for (w = 1; w < 5; w++)
               {
                 double alpha1, alpha2, alpha3;
                 double unw, dnw, une, dne, usw, dsw, use, dse;
                 int uu, vv, ww;
                 double sum;
                 
                 if (u % 2 == 0)
                   alpha1 = 0.75;
                 else
                   alpha1 = 0.25;
                 
                 if (v % 2 == 0)
                   alpha2 = 0;
                 else
                   alpha2 = 0.5;
                 
                 if (w % 2 == 0)
                   alpha3 = 0.75;
                 else
                   alpha3 = 0.25;
                 
                 uu = (u - 1) / 2;
                 vv = v / 2;
                 ww = (w - 1) / 2;
                 unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                        mask1[uu][vv][ww]);
                 dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                        (alpha3 > 0 ?
                            mask1[uu][vv][ww + 1] : 0.0));
                 une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                        (alpha2 > 0 ?
                            mask1[uu][vv + 1][ww] : 0.0));
                 dne = ((1 - alpha1) * alpha2 * alpha3 *
                        (alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu][vv + 1][ww + 1] : 0.0));
                 usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                        (alpha1 > 0 ?
                            mask1[uu + 1][vv][ww] : 0.0));
                 dsw = (alpha1 * (1 - alpha2) * alpha3 *
                        (alpha1 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv][ww + 1] : 0.0));
                 use = (alpha1 * alpha2 * (1 - alpha3) *
                        (alpha1 > 0 && alpha2 > 0 ?
                            mask1[uu + 1][vv + 1][ww] : 0.0));
                 dse = (alpha1 * alpha2 * alpha3 *
                        (alpha1 > 0 && alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv + 1][ww + 1] : 0.0));
                 
                 sum = unw + dnw + une + dne + usw + dsw + use + dse;
                 
                 if (sum > 0)
                 {
                   if (unw > 0)
                     stencil3[uu][vv][ww] += unw * stencil2[u][v][w] / sum;
                   if (dnw > 0)
                     stencil3[uu][vv][ww + 1] += dnw * stencil2[u][v][w] / sum;
                   if (une > 0)
                     stencil3[uu][vv + 1][ww] += une * stencil2[u][v][w] / sum;
                   if (dne > 0)
                     stencil3[uu][vv + 1][ww + 1] += dne * stencil2[u][v][w] / sum;
                   if (usw > 0)
                     stencil3[uu + 1][vv][ww] += usw * stencil2[u][v][w] / sum;
                   if (dsw > 0)
                     stencil3[uu + 1][vv][ww + 1] += dsw * stencil2[u][v][w] / sum;
                   if (use > 0)
                     stencil3[uu + 1][vv + 1][ww] += use * stencil2[u][v][w] / sum;
                   if (dse > 0)
                     stencil3[uu + 1][vv + 1][ww + 1] += dse * stencil2[u][v][w] / sum;
                 }
               }
             }
           }
         }
         
         if (M % 2 == 1 && N % 2 == 1 && P % 2 == 0)
         {
           for (u = 0; u < 5; u++)
           {
             for (v = 0; v < 5; v++)
             {
               for (w = 1; w < 5; w++)
               {
                 double alpha1, alpha2, alpha3;
                 double unw, dnw, une, dne, usw, dsw, use, dse;
                 int uu, vv, ww;
                 double sum;
                 
                 if (u % 2 == 0)
                   alpha1 = 0;
                 else
                   alpha1 = 0.5;
                 
                 if (v % 2 == 0)
                   alpha2 = 0;
                 else
                   alpha2 = 0.5;
                 
                 if (w % 2 == 0)
                   alpha3 = 0.75;
                 else
                   alpha3 = 0.25;
                 
                 uu = u / 2;
                 vv = v / 2;
                 ww = (w - 1) / 2;
                 unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                        mask1[uu][vv][ww]);
                 dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                        (alpha3 > 0 ?
                            mask1[uu][vv][ww + 1] : 0.0));
                 une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                        (alpha2 > 0 ?
                            mask1[uu][vv + 1][ww] : 0.0));
                 dne = ((1 - alpha1) * alpha2 * alpha3 *
                        (alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu][vv + 1][ww + 1] : 0.0));
                 usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                        (alpha1 > 0 ?
                            mask1[uu + 1][vv][ww] : 0.0));
                 dsw = (alpha1 * (1 - alpha2) * alpha3 *
                        (alpha1 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv][ww + 1] : 0.0));
                 use = (alpha1 * alpha2 * (1 - alpha3) *
                        (alpha1 > 0 && alpha2 > 0 ?
                            mask1[uu + 1][vv + 1][ww] : 0.0));
                 dse = (alpha1 * alpha2 * alpha3 *
                        (alpha1 > 0 && alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv + 1][ww + 1] : 0.0));
                 
                 sum = unw + dnw + une + dne + usw + dsw + use + dse;
                 
                 if (sum > 0)
                 {
                   if (unw > 0)
                     stencil3[uu][vv][ww] += unw * stencil2[u][v][w] / sum;
                   if (dnw > 0)
                     stencil3[uu][vv][ww + 1] += dnw * stencil2[u][v][w] / sum;
                   if (une > 0)
                     stencil3[uu][vv + 1][ww] += une * stencil2[u][v][w] / sum;
                   if (dne > 0)
                     stencil3[uu][vv + 1][ww + 1] += dne * stencil2[u][v][w] / sum;
                   if (usw > 0)
                     stencil3[uu + 1][vv][ww] += usw * stencil2[u][v][w] / sum;
                   if (dsw > 0)
                     stencil3[uu + 1][vv][ww + 1] += dsw * stencil2[u][v][w] / sum;
                   if (use > 0)
                     stencil3[uu + 1][vv + 1][ww] += use * stencil2[u][v][w] / sum;
                   if (dse > 0)
                     stencil3[uu + 1][vv + 1][ww + 1] += dse * stencil2[u][v][w] / sum;
                 }
               }
             }
           }
         }
         
         if (M % 2 == 0 && N % 2 == 0 && P % 2 == 1)
         {
           for (u = 1; u < 5; u++)
           {
             for (v = 1; v < 5; v++)
             {
               for (w = 0; w < 5; w++)
               {
                 double alpha1, alpha2, alpha3;
                 double unw, dnw, une, dne, usw, dsw, use, dse;
                 int uu, vv, ww;
                 double sum;
                 
                 if (u % 2 == 0)
                   alpha1 = 0.75;
                 else
                   alpha1 = 0.25;
                 
                 if (v % 2 == 0)
                   alpha2 = 0.75;
                 else
                   alpha2 = 0.25;
                 
                 if (w % 2 == 0)
                   alpha3 = 0;
                 else
                   alpha3 = 0.5;
                 
                 uu = (u - 1) / 2;
                 vv = (v - 1) / 2;
                 ww = w / 2;
                 unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                        mask1[uu][vv][ww]);
                 dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                        (alpha3 > 0 ?
                            mask1[uu][vv][ww + 1] : 0.0));
                 une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                        (alpha2 > 0 ?
                            mask1[uu][vv + 1][ww] : 0.0));
                 dne = ((1 - alpha1) * alpha2 * alpha3 *
                        (alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu][vv + 1][ww + 1] : 0.0));
                 usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                        (alpha1 > 0 ?
                            mask1[uu + 1][vv][ww] : 0.0));
                 dsw = (alpha1 * (1 - alpha2) * alpha3 *
                        (alpha1 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv][ww + 1] : 0.0));
                 use = (alpha1 * alpha2 * (1 - alpha3) *
                        (alpha1 > 0 && alpha2 > 0 ?
                            mask1[uu + 1][vv + 1][ww] : 0.0));
                 dse = (alpha1 * alpha2 * alpha3 *
                        (alpha1 > 0 && alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv + 1][ww + 1] : 0.0));
                 
                 sum = unw + dnw + une + dne + usw + dsw + use + dse;
                 
                 if (sum > 0)
                 {
                   if (unw > 0)
                     stencil3[uu][vv][ww] += unw * stencil2[u][v][w] / sum;
                   if (dnw > 0)
                     stencil3[uu][vv][ww + 1] += dnw * stencil2[u][v][w] / sum;
                   if (une > 0)
                     stencil3[uu][vv + 1][ww] += une * stencil2[u][v][w] / sum;
                   if (dne > 0)
                     stencil3[uu][vv + 1][ww + 1] += dne * stencil2[u][v][w] / sum;
                   if (usw > 0)
                     stencil3[uu + 1][vv][ww] += usw * stencil2[u][v][w] / sum;
                   if (dsw > 0)
                     stencil3[uu + 1][vv][ww + 1] += dsw * stencil2[u][v][w] / sum;
                   if (use > 0)
                     stencil3[uu + 1][vv + 1][ww] += use * stencil2[u][v][w] / sum;
                   if (dse > 0)
                     stencil3[uu + 1][vv + 1][ww + 1] += dse * stencil2[u][v][w] / sum;
                 }
               }
             }
           }
         }
         
         if (M % 2 == 1 && N % 2 == 0 && P % 2 == 1)
         {
           for (u = 0; u < 5; u++)
           {
             for (v = 1; v < 5; v++)
             {
               for (w = 0; w < 5; w++)
               {
                 double alpha1, alpha2, alpha3;
                 double unw, dnw, une, dne, usw, dsw, use, dse;
                 int uu, vv, ww;
                 double sum;
                 
                 if (u % 2 == 0)
                   alpha1 = 0;
                 else
                   alpha1 = 0.5;
                 
                 if (v % 2 == 0)
                   alpha2 = 0.75;
                 else
                   alpha2 = 0.25;
                 
                 if (w % 2 == 0)
                   alpha3 = 0;
                 else
                   alpha3 = 0.5;
                 
                 uu = u / 2;
                 vv = (v - 1) / 2;
                 ww = w / 2;
                 unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                        mask1[uu][vv][ww]);
                 dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                        (alpha3 > 0 ?
                            mask1[uu][vv][ww + 1] : 0.0));
                 une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                        (alpha2 > 0 ?
                            mask1[uu][vv + 1][ww] : 0.0));
                 dne = ((1 - alpha1) * alpha2 * alpha3 *
                        (alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu][vv + 1][ww + 1] : 0.0));
                 usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                        (alpha1 > 0 ?
                            mask1[uu + 1][vv][ww] : 0.0));
                 dsw = (alpha1 * (1 - alpha2) * alpha3 *
                        (alpha1 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv][ww + 1] : 0.0));
                 use = (alpha1 * alpha2 * (1 - alpha3) *
                        (alpha1 > 0 && alpha2 > 0 ?
                            mask1[uu + 1][vv + 1][ww] : 0.0));
                 dse = (alpha1 * alpha2 * alpha3 *
                        (alpha1 > 0 && alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv + 1][ww + 1] : 0.0));
                 
                 sum = unw + dnw + une + dne + usw + dsw + use + dse;
                 
                 if (sum > 0)
                 {
                   if (unw > 0)
                     stencil3[uu][vv][ww] += unw * stencil2[u][v][w] / sum;
                   if (dnw > 0)
                     stencil3[uu][vv][ww + 1] += dnw * stencil2[u][v][w] / sum;
                   if (une > 0)
                     stencil3[uu][vv + 1][ww] += une * stencil2[u][v][w] / sum;
                   if (dne > 0)
                     stencil3[uu][vv + 1][ww + 1] += dne * stencil2[u][v][w] / sum;
                   if (usw > 0)
                     stencil3[uu + 1][vv][ww] += usw * stencil2[u][v][w] / sum;
                   if (dsw > 0)
                     stencil3[uu + 1][vv][ww + 1] += dsw * stencil2[u][v][w] / sum;
                   if (use > 0)
                     stencil3[uu + 1][vv + 1][ww] += use * stencil2[u][v][w] / sum;
                   if (dse > 0)
                     stencil3[uu + 1][vv + 1][ww + 1] += dse * stencil2[u][v][w] / sum;
                 }
               }
             }
           }
         }
         
         if (M % 2 == 0 && N % 2 == 1 && P % 2 == 1)
         {
           for (u = 1; u < 5; u++)
           {
             for (v = 0; v < 5; v++)
             {
               for (w = 0; w < 5; w++)
               {
                 double alpha1, alpha2, alpha3;
                 double unw, dnw, une, dne, usw, dsw, use, dse;
                 int uu, vv, ww;
                 double sum;
                 
                 if (u % 2 == 0)
                   alpha1 = 0.75;
                 else
                   alpha1 = 0.25;
                 
                 if (v % 2 == 0)
                   alpha2 = 0;
                 else
                   alpha2 = 0.5;
                 
                 if (w % 2 == 0)
                   alpha3 = 0;
                 else
                   alpha3 = 0.5;
                 
                 uu = (u - 1) / 2;
                 vv = v / 2;
                 ww = w / 2;
                 unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                        mask1[uu][vv][ww]);
                 dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                        (alpha3 > 0 ?
                            mask1[uu][vv][ww + 1] : 0.0));
                 une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                        (alpha2 > 0 ?
                            mask1[uu][vv + 1][ww] : 0.0));
                 dne = ((1 - alpha1) * alpha2 * alpha3 *
                        (alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu][vv + 1][ww + 1] : 0.0));
                 usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                        (alpha1 > 0 ?
                            mask1[uu + 1][vv][ww] : 0.0));
                 dsw = (alpha1 * (1 - alpha2) * alpha3 *
                        (alpha1 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv][ww + 1] : 0.0));
                 use = (alpha1 * alpha2 * (1 - alpha3) *
                        (alpha1 > 0 && alpha2 > 0 ?
                            mask1[uu + 1][vv + 1][ww] : 0.0));
                 dse = (alpha1 * alpha2 * alpha3 *
                        (alpha1 > 0 && alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv + 1][ww + 1] : 0.0));
                 
                 sum = unw + dnw + une + dne + usw + dsw + use + dse;
                 
                 if (sum > 0)
                 {
                   if (unw > 0)
                     stencil3[uu][vv][ww] += unw * stencil2[u][v][w] / sum;
                   if (dnw > 0)
                     stencil3[uu][vv][ww + 1] += dnw * stencil2[u][v][w] / sum;
                   if (une > 0)
                     stencil3[uu][vv + 1][ww] += une * stencil2[u][v][w] / sum;
                   if (dne > 0)
                     stencil3[uu][vv + 1][ww + 1] += dne * stencil2[u][v][w] / sum;
                   if (usw > 0)
                     stencil3[uu + 1][vv][ww] += usw * stencil2[u][v][w] / sum;
                   if (dsw > 0)
                     stencil3[uu + 1][vv][ww + 1] += dsw * stencil2[u][v][w] / sum;
                   if (use > 0)
                     stencil3[uu + 1][vv + 1][ww] += use * stencil2[u][v][w] / sum;
                   if (dse > 0)
                     stencil3[uu + 1][vv + 1][ww + 1] += dse * stencil2[u][v][w] / sum;
                 }
               }
             }
           }
         }
         
         if (M % 2 == 1 && N % 2 == 1 && P % 2 == 1)
         {
           for (u = 0; u < 5; u++)
           {
             for (v = 0; v < 5; v++)
             {
               for (w = 0; w < 5; w++)
               {
                 double alpha1, alpha2, alpha3;
                 double unw, dnw, une, dne, usw, dsw, use, dse;
                 int uu, vv, ww;
                 double sum;
                 
                 if (u % 2 == 0)
                   alpha1 = 0;
                 else
                   alpha1 = 0.5;
                 
                 if (v % 2 == 0)
                   alpha2 = 0;
                 else
                   alpha2 = 0.5;
                 
                 if (w % 2 == 0)
                   alpha3 = 0;
                 else
                   alpha3 = 0.5;
                 
                 uu = u / 2;
                 vv = v / 2;
                 ww = w / 2;
                 unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                        mask1[uu][vv][ww]);
                 dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                        (alpha3 > 0 ?
                            mask1[uu][vv][ww + 1] : 0.0));
                 une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                        (alpha2 > 0 ?
                            mask1[uu][vv + 1][ww] : 0.0));
                 dne = ((1 - alpha1) * alpha2 * alpha3 *
                        (alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu][vv + 1][ww + 1] : 0.0));
                 usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                        (alpha1 > 0 ?
                            mask1[uu + 1][vv][ww] : 0.0));
                 dsw = (alpha1 * (1 - alpha2) * alpha3 *
                        (alpha1 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv][ww + 1] : 0.0));
                 use = (alpha1 * alpha2 * (1 - alpha3) *
                        (alpha1 > 0 && alpha2 > 0 ?
                            mask1[uu + 1][vv + 1][ww] : 0.0));
                 dse = (alpha1 * alpha2 * alpha3 *
                        (alpha1 > 0 && alpha2 > 0 && alpha3 > 0 ?
                            mask1[uu + 1][vv + 1][ww + 1] : 0.0));
                 
                 sum = unw + dnw + une + dne + usw + dsw + use + dse;
                 
                 if (sum > 0)
                 {
                   if (unw > 0)
                     stencil3[uu][vv][ww] += unw * stencil2[u][v][w] / sum;
                   if (dnw > 0)
                     stencil3[uu][vv][ww + 1] += dnw * stencil2[u][v][w] / sum;
                   if (une > 0)
                     stencil3[uu][vv + 1][ww] += une * stencil2[u][v][w] / sum;
                   if (dne > 0)
                     stencil3[uu][vv + 1][ww + 1] += dne * stencil2[u][v][w] / sum;
                   if (usw > 0)
                     stencil3[uu + 1][vv][ww] += usw * stencil2[u][v][w] / sum;
                   if (dsw > 0)
                     stencil3[uu + 1][vv][ww + 1] += dsw * stencil2[u][v][w] / sum;
                   if (use > 0)
                     stencil3[uu + 1][vv + 1][ww] += use * stencil2[u][v][w] / sum;
                   if (dse > 0)
                     stencil3[uu + 1][vv + 1][ww + 1] += dse * stencil2[u][v][w] / sum;
                 }
               }
             }
           }
         }
         
         for (k = 0; k < 27; k++)
         {
           int a = (k % 3);
           int b = ((k / 3) % 3);
           int c = ((k / 9) % 3);
           lhs_coarse[27 * index1 + k] = stencil3[a][b][c];
         }
         
         for (u = 0; u < 27; u++)
         {
           if (u != 13 && lhs_coarse[27 * index1 + u] < 0)
           {
             lhs_coarse[27 * index1 + 13] += lhs_coarse[27 * index1 + u];
             lhs_coarse[27 * index1 + u] = 0;
           }
         }
       }
     }
   }
 }
    
    private void upsample3D(int M, int N, int P,
            double[] v, int Mhalf, int Nhalf, int Phalf,
            double[] f_out, double[] weight, double[] coarse_weight)
 {
   int i, j, p;
   int index1;
   int index2;
   int MNhalf = Mhalf * Nhalf;
   
   if (M % 2 == 0 && N % 2 == 0 && P % 2 == 0)
   {
     for (p = 0; p < P; p++)
     {
       for (j = 0; j < N; j++)
       {
         for (i = 0; i < M; i++)
         {
           double alpha1, alpha2, alpha3;
           double unw, dnw, une, dne, usw, dsw, use, dse;
           double sum;
           
           index1 = (p * N + j) * M + i;
           index2 = (((p + 1) / 2 - 1) * Nhalf + (j + 1) / 2 - 1) * Mhalf + (i + 1) / 2 - 1;
           
           if (weight[index1] == 0)
             continue;
           
           if (i % 2 == 0)
             alpha1 = 0.75;
           else
             alpha1 = 0.25;
           
           if (j % 2 == 0)
             alpha2 = 0.75;
           else
             alpha2 = 0.25;
           
           if (p % 2 == 0)
             alpha3 = 0.75;
           else
             alpha3 = 0.25;
           
           unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                  (i > 0 && j > 0 && p > 0 ?
                      coarse_weight[index2] : 0.0));
           dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                  (i > 0 && j > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf] : 0.0));
           une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                  (i > 0 && j < N - 1 && p > 0 ?
                      coarse_weight[index2 + Mhalf] : 0.0));
           dne = ((1 - alpha1) * alpha2 * alpha3 *
                  (i > 0 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf] : 0.0));
           usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                  (i < M - 1 && j > 0 && p > 0 ?
                      coarse_weight[index2 + 1] : 0.0));
           dsw = (alpha1 * (1 - alpha2) * alpha3 *
                  (i < M - 1 && j > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + 1] : 0.0));
           use = (alpha1 * alpha2 * (1 - alpha3) *
                  (i < M - 1 && j < N - 1 && p > 0 ?
                      coarse_weight[index2 + Mhalf + 1] : 0.0));
           dse = (alpha1 * alpha2 * alpha3 *
                 (i < M - 1 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf + 1] : 0.0));
           
           sum = unw + dnw + une + dne + usw + dsw + use + dse;
           
           if (sum > 0)
           {
             double contribution = 0;
             
             if (unw > 0)
               contribution += unw * v[index2];
             if (dnw > 0)
               contribution += dnw * v[index2 + MNhalf];
             if (une > 0)
               contribution += une * v[index2 + Mhalf];
             if (dne > 0)
               contribution += dne * v[index2 + MNhalf + Mhalf];
             if (usw > 0)
               contribution += usw * v[index2 + 1];
             if (dsw > 0)
               contribution += dsw * v[index2 + MNhalf + 1];
             if (use > 0)
               contribution += use * v[index2 + Mhalf + 1];
             if (dse > 0)
               contribution += dse * v[index2 + MNhalf + Mhalf + 1];
             
             f_out[index1] += contribution / sum;
           }
         }
       }
     }
   }
   
   if (M % 2 == 1 && N % 2 == 0 && P % 2 == 0)
   {
     for (p = 0; p < P; p++)
     {
       for (j = 0; j < N; j++)
       {
         for (i = 0; i < M; i++)
         {
           double alpha1, alpha2, alpha3;
           double unw, dnw, une, dne, usw, dsw, use, dse;
           double sum;
           
           index1 = (p * N + j) * M + i;
           index2 = (((p + 1) / 2 - 1) * Nhalf + (j + 1) / 2 - 1) * Mhalf + i / 2;
           
           if (weight[index1] == 0)
             continue;
           
           if (i % 2 == 0)
             alpha1 = 0;
           else
             alpha1 = 0.5;
           
           if (j % 2 == 0)
             alpha2 = 0.75;
           else
             alpha2 = 0.25;
           
           if (p % 2 == 0)
             alpha3 = 0.75;
           else
             alpha3 = 0.25;
           
           unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                  (j > 0 && p > 0 ?
                      coarse_weight[index2] : 0.0));
           dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                  (j > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf] : 0.0));
           une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                  (j < N - 1 && p > 0 ?
                      coarse_weight[index2 + Mhalf] : 0.0));
           dne = ((1 - alpha1) * alpha2 * alpha3 *
                  (j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf] : 0.0));
           usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                  (i < M - 1 && j > 0 && p > 0 ?
                      coarse_weight[index2 + 1] : 0.0));
           dsw = (alpha1 * (1 - alpha2) * alpha3 *
                  (i < M - 1 && j > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + 1] : 0.0));
           use = (alpha1 * alpha2 * (1 - alpha3) *
                  (i < M - 1 && j < N - 1 && p > 0 ?
                      coarse_weight[index2 + Mhalf + 1] : 0.0));
           dse = (alpha1 * alpha2 * alpha3 *
                  (i < M - 1 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf + 1] : 0.0));
           
           sum = unw + dnw + une + dne + usw + dsw + use + dse;
           
           if (sum > 0)
           {
             double contribution = 0;
             
             if (unw > 0)
               contribution += unw * v[index2];
             if (dnw > 0)
               contribution += dnw * v[index2 + MNhalf];
             if (une > 0)
               contribution += une * v[index2 + Mhalf];
             if (dne > 0)
               contribution += dne * v[index2 + MNhalf + Mhalf];
             if (usw > 0)
               contribution += usw * v[index2 + 1];
             if (dsw > 0)
               contribution += dsw * v[index2 + MNhalf + 1];
             if (use > 0)
               contribution += use * v[index2 + Mhalf + 1];
             if (dse > 0)
               contribution += dse * v[index2 + MNhalf + Mhalf + 1];
             
             f_out[index1] += contribution / sum;
           }
         }
       }
     }
   }
   
   if (M % 2 == 0 && N % 2 == 1 && P % 2 == 0)
   {
     for (p = 0; p < P; p++)
     {
       for (j = 0; j < N; j++)
       {
         for (i = 0; i < M; i++)
         {
           double alpha1, alpha2, alpha3;
           double unw, dnw, une, dne, usw, dsw, use, dse;
           double sum;
           
           index1 = (p * N + j) * M + i;
           index2 = (((p + 1) / 2 - 1) * Nhalf + j / 2) * Mhalf + (i + 1) / 2 - 1;
           
           if (weight[index1] == 0)
             continue;
           
           if (i % 2 == 0)
             alpha1 = 0.75;
           else
             alpha1 = 0.25;
           
           if (j % 2 == 0)
             alpha2 = 0;
           else
             alpha2 = 0.5;
           
           if (p % 2 == 0)
             alpha3 = 0.75;
           else
             alpha3 = 0.25;
           
           unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                  (i > 0 && p > 0 ?
                      coarse_weight[index2] : 0.0));
           dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                  (i > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf] : 0.0));
           une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                  (i > 0 && j < N - 1 && p > 0 ?
                      coarse_weight[index2 + Mhalf] : 0.0));
           dne = ((1 - alpha1) * alpha2 * alpha3 *
                  (i > 0 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf] : 0.0));
           usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                  (i < M - 1 && p > 0 ?
                      coarse_weight[index2 + 1] : 0.0));
           dsw = (alpha1 * (1 - alpha2) * alpha3 *
                  (i < M - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + 1] : 0.0));
           use = (alpha1 * alpha2 * (1 - alpha3) *
                  (i < M - 1 && j < N - 1 && p > 0 ?
                      coarse_weight[index2 + Mhalf + 1] : 0.0));
           dse = (alpha1 * alpha2 * alpha3 *
                  (i < M - 1 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf + 1] : 0.0));
           
           sum = unw + dnw + une + dne + usw + dsw + use + dse;
           
           if (sum > 0)
           {
             double contribution = 0;
             
             if (unw > 0)
               contribution += unw * v[index2];
             if (dnw > 0)
               contribution += dnw * v[index2 + MNhalf];
             if (une > 0)
               contribution += une * v[index2 + Mhalf];
             if (dne > 0)
               contribution += dne * v[index2 + MNhalf + Mhalf];
             if (usw > 0)
               contribution += usw * v[index2 + 1];
             if (dsw > 0)
               contribution += dsw * v[index2 + MNhalf + 1];
             if (use > 0)
               contribution += use * v[index2 + Mhalf + 1];
             if (dse > 0)
               contribution += dse * v[index2 + MNhalf + Mhalf + 1];
             
             f_out[index1] += contribution / sum;
           }
         }
       }
     }
   }
   
   if (M % 2 == 1 && N % 2 == 1 && P % 2 == 0)
   {
     for (p = 0; p < P; p++)
     {
       for (j = 0; j < N; j++)
       {
         for (i = 0; i < M; i++)
         {
           double alpha1, alpha2, alpha3;
           double unw, dnw, une, dne, usw, dsw, use, dse;
           double sum;
           
           index1 = (p * N + j) * M + i;
           index2 = (((p + 1) / 2 - 1) * Nhalf + j / 2) * Mhalf + i / 2;
           
           if (weight[index1] == 0)
             continue;
           
           if (i % 2 == 0)
             alpha1 = 0;
           else
             alpha1 = 0.5;
           
           if (j % 2 == 0)
             alpha2 = 0;
           else
             alpha2 = 0.5;
           
           if (p % 2 == 0)
             alpha3 = 0.75;
           else
             alpha3 = 0.25;
           
           unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                  (p > 0 ?
                      coarse_weight[index2] : 0.0));
           dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                  (p < P - 1 ?
                      coarse_weight[index2 + MNhalf] : 0.0));
           une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                  (j < N - 1 && p > 0 ?
                      coarse_weight[index2 + Mhalf] : 0.0));
           dne = ((1 - alpha1) * alpha2 * alpha3 *
                  (j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf] : 0.0));
           usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                  (i < M - 1 && p > 0 ?
                      coarse_weight[index2 + 1] : 0.0));
           dsw = (alpha1 * (1 - alpha2) * alpha3 *
                  (i < M - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + 1] : 0.0));
           use = (alpha1 * alpha2 * (1 - alpha3) *
                  (i < M - 1 && j < N - 1 && p > 0 ?
                      coarse_weight[index2 + Mhalf + 1] : 0.0));
           dse = (alpha1 * alpha2 * alpha3 *
                  (i < M - 1 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf + 1] : 0.0));
           
           sum = unw + dnw + une + dne + usw + dsw + use + dse;
           
           if (sum > 0)
           {
             double contribution = 0;
             
             if (unw > 0)
               contribution += unw * v[index2];
             if (dnw > 0)
               contribution += dnw * v[index2 + MNhalf];
             if (une > 0)
               contribution += une * v[index2 + Mhalf];
             if (dne > 0)
               contribution += dne * v[index2 + MNhalf + Mhalf];
             if (usw > 0)
               contribution += usw * v[index2 + 1];
             if (dsw > 0)
               contribution += dsw * v[index2 + MNhalf + 1];
             if (use > 0)
               contribution += use * v[index2 + Mhalf + 1];
             if (dse > 0)
               contribution += dse * v[index2 + MNhalf + Mhalf + 1];
             
             f_out[index1] += contribution / sum;
           }
         }
       }
     }
   }
   
   if (M % 2 == 0 && N % 2 == 0 && P % 2 == 1)
   {
     for (p = 0; p < P; p++)
     {
       for (j = 0; j < N; j++)
       {
         for (i = 0; i < M; i++)
         {
           double alpha1, alpha2, alpha3;
           double unw, dnw, une, dne, usw, dsw, use, dse;
           double sum;
           
           index1 = (p * N + j) * M + i;
           index2 = ((p / 2) * Nhalf + (j + 1) / 2 - 1) * Mhalf + (i + 1) / 2 - 1;
           
           if (weight[index1] == 0)
             continue;
           
           if (i % 2 == 0)
             alpha1 = 0.75;
           else
             alpha1 = 0.25;
           
           if (j % 2 == 0)
             alpha2 = 0.75;
           else
             alpha2 = 0.25;
           
           if (p % 2 == 0)
             alpha3 = 0;
           else
             alpha3 = 0.5;
           
           unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                  (i > 0 && j > 0 ?
                      coarse_weight[index2] : 0.0));
           dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                  (i > 0 && j > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf] : 0.0));
           une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                  (i > 0 && j < N - 1 ?
                      coarse_weight[index2 + Mhalf] : 0.0));
           dne = ((1 - alpha1) * alpha2 * alpha3 *
                  (i > 0 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf] : 0.0));
           usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                  (i < M - 1 && j > 0 ?
                      coarse_weight[index2 + 1] : 0.0));
           dsw = (alpha1 * (1 - alpha2) * alpha3 *
                  (i < M - 1 && j > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + 1] : 0.0));
           use = (alpha1 * alpha2 * (1 - alpha3) *
                  (i < M - 1 && j < N - 1 ?
                      coarse_weight[index2 + Mhalf + 1] : 0.0));
           dse = (alpha1 * alpha2 * alpha3 *
                  (i < M - 1 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf + 1] : 0.0));
           
           sum = unw + dnw + une + dne + usw + dsw + use + dse;
           
           if (sum > 0)
           {
             double contribution = 0;
             
             if (unw > 0)
               contribution += unw * v[index2];
             if (dnw > 0)
               contribution += dnw * v[index2 + MNhalf];
             if (une > 0)
               contribution += une * v[index2 + Mhalf];
             if (dne > 0)
               contribution += dne * v[index2 + MNhalf + Mhalf];
             if (usw > 0)
               contribution += usw * v[index2 + 1];
             if (dsw > 0)
               contribution += dsw * v[index2 + MNhalf + 1];
             if (use > 0)
               contribution += use * v[index2 + Mhalf + 1];
             if (dse > 0)
               contribution += dse * v[index2 + MNhalf + Mhalf + 1];
             
             f_out[index1] += contribution / sum;
           }
         }
       }
     }
   }
   
   if (M % 2 == 1 && N % 2 == 0 && P % 2 == 1)
   {
     for (p = 0; p < P; p++)
     {
       for (j = 0; j < N; j++)
       {
         for (i = 0; i < M; i++)
         {
           double alpha1, alpha2, alpha3;
           double unw, dnw, une, dne, usw, dsw, use, dse;
           double sum;
           
           index1 = (p * N + j) * M + i;
           index2 = ((p / 2) * Nhalf + (j + 1) / 2 - 1) * Mhalf + i / 2;
           
           if (weight[index1] == 0)
             continue;
           
           if (i % 2 == 0)
             alpha1 = 0;
           else
             alpha1 = 0.5;
           
           if (j % 2 == 0)
             alpha2 = 0.75;
           else
             alpha2 = 0.25;
           
           if (p % 2 == 0)
             alpha3 = 0;
           else
             alpha3 = 0.5;
           
           unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                  (j > 0 ?
                      coarse_weight[index2] : 0.0));
           dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                  (j > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf] : 0.0));
           une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                  (j < N - 1 ?
                      coarse_weight[index2 + Mhalf] : 0.0));
           dne = ((1 - alpha1) * alpha2 * alpha3 *
                  (j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf] : 0.0));
           usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                  (i < M - 1 && j > 0 ?
                      coarse_weight[index2 + 1] : 0.0));
           dsw = (alpha1 * (1 - alpha2) * alpha3 *
                  (i < M - 1 && j > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + 1] : 0.0));
           use = (alpha1 * alpha2 * (1 - alpha3) *
                  (i < M - 1 && j < N - 1 ?
                      coarse_weight[index2 + Mhalf + 1] : 0.0));
           dse = (alpha1 * alpha2 * alpha3 *
                  (i < M - 1 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf + 1] : 0.0));
           
           sum = unw + dnw + une + dne + usw + dsw + use + dse;
           
           if (sum > 0)
           {
             double contribution = 0;
             
             if (unw > 0)
               contribution += unw * v[index2];
             if (dnw > 0)
               contribution += dnw * v[index2 + MNhalf];
             if (une > 0)
               contribution += une * v[index2 + Mhalf];
             if (dne > 0)
               contribution += dne * v[index2 + MNhalf + Mhalf];
             if (usw > 0)
               contribution += usw * v[index2 + 1];
             if (dsw > 0)
               contribution += dsw * v[index2 + MNhalf + 1];
             if (use > 0)
               contribution += use * v[index2 + Mhalf + 1];
             if (dse > 0)
               contribution += dse * v[index2 + MNhalf + Mhalf + 1];
             
             f_out[index1] += contribution / sum;
           }
         }
       }
     }
   }
   
   if (M % 2 == 0 && N % 2 == 1 && P % 2 == 1)
   {
     for (p = 0; p < P; p++)
     {
       for (j = 0; j < N; j++)
       {
         for (i = 0; i < M; i++)
         {
           double alpha1, alpha2, alpha3;
           double unw, dnw, une, dne, usw, dsw, use, dse;
           double sum;
           
           index1 = (p * N + j) * M + i;
           index2 = ((p / 2) * Nhalf + j / 2) * Mhalf + (i + 1) / 2 - 1;
           
           if (weight[index1] == 0)
             continue;
           
           if (i % 2 == 0)
             alpha1 = 0.75;
           else
             alpha1 = 0.25;
           
           if (j % 2 == 0)
             alpha2 = 0;
           else
             alpha2 = 0.5;
           
           if (p % 2 == 0)
             alpha3 = 0;
           else
             alpha3 = 0.5;
           
           unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                  (i > 0 ?
                      coarse_weight[index2] : 0.0));
           dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                  (i > 0 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf] : 0.0));
           une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                  (i > 0 && j < N - 1 ?
                      coarse_weight[index2 + Mhalf] : 0.0));
           dne = ((1 - alpha1) * alpha2 * alpha3 *
                  (i > 0 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf] : 0.0));
           usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                  (i < M - 1 ?
                      coarse_weight[index2 + 1] : 0.0));
           dsw = (alpha1 * (1 - alpha2) * alpha3 *
                  (i < M - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + 1] : 0.0));
           use = (alpha1 * alpha2 * (1 - alpha3) *
                  (i < M - 1 && j < N - 1 ?
                      coarse_weight[index2 + Mhalf + 1] : 0.0));
           dse = (alpha1 * alpha2 * alpha3 *
                  (i < M - 1 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf + 1] : 0.0));
           
           sum = unw + dnw + une + dne + usw + dsw + use + dse;
           
           if (sum > 0)
           {
             double contribution = 0;
             
             if (unw > 0)
               contribution += unw * v[index2];
             if (dnw > 0)
               contribution += dnw * v[index2 + MNhalf];
             if (une > 0)
               contribution += une * v[index2 + Mhalf];
             if (dne > 0)
               contribution += dne * v[index2 + MNhalf + Mhalf];
             if (usw > 0)
               contribution += usw * v[index2 + 1];
             if (dsw > 0)
               contribution += dsw * v[index2 + MNhalf + 1];
             if (use > 0)
               contribution += use * v[index2 + Mhalf + 1];
             if (dse > 0)
               contribution += dse * v[index2 + MNhalf + Mhalf + 1];
             
             f_out[index1] += contribution / sum;
           }
         }
       }
     }
   }
   
   if (M % 2 == 1 && N % 2 == 1 && P % 2 == 1)
   {
     for (p = 0; p < P; p++)
     {
       for (j = 0; j < N; j++)
       {
         for (i = 0; i < M; i++)
         {
           double alpha1, alpha2, alpha3;
           double unw, dnw, une, dne, usw, dsw, use, dse;
           double sum;
           
           index1 = (p * N + j) * M + i;
           index2 = ((p / 2) * Nhalf + j / 2) * Mhalf + i / 2;
           
           if (weight[index1] == 0)
             continue;
           
           if (i % 2 == 0)
             alpha1 = 0;
           else
             alpha1 = 0.5;
           
           if (j % 2 == 0)
             alpha2 = 0;
           else
             alpha2 = 0.5;
           
           if (p % 2 == 0)
             alpha3 = 0;
           else
             alpha3 = 0.5;
           
           unw = ((1 - alpha1) * (1 - alpha2) * (1 - alpha3) *
                  coarse_weight[index2]);
           dnw = ((1 - alpha1) * (1 - alpha2) * alpha3 *
                  (p < P - 1 ?
                      coarse_weight[index2 + MNhalf] : 0.0));
           une = ((1 - alpha1) * alpha2 * (1 - alpha3) *
                  (j < N - 1 ?
                      coarse_weight[index2 + Mhalf] : 0.0));
           dne = ((1 - alpha1) * alpha2 * alpha3 *
                  (j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf] : 0.0));
           usw = (alpha1 * (1 - alpha2) * (1 - alpha3) *
                  (i < M - 1 ?
                      coarse_weight[index2 + 1] : 0.0));
           dsw = (alpha1 * (1 - alpha2) * alpha3 *
                  (i < M - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + 1] : 0.0));
           use = (alpha1 * alpha2 * (1 - alpha3) *
                  (i < M - 1 && j < N - 1 ?
                      coarse_weight[index2 + Mhalf + 1] : 0.0));
           dse = (alpha1 * alpha2 * alpha3 *
                  (i < M - 1 && j < N - 1 && p < P - 1 ?
                      coarse_weight[index2 + MNhalf + Mhalf + 1] : 0.0));
           
           sum = unw + dnw + une + dne + usw + dsw + use + dse;
           
           if (sum > 0)
           {
             double contribution = 0;
             
             if (unw > 0)
               contribution += unw * v[index2];
             if (dnw > 0)
               contribution += dnw * v[index2 + MNhalf];
             if (une > 0)
               contribution += une * v[index2 + Mhalf];
             if (dne > 0)
               contribution += dne * v[index2 + MNhalf + Mhalf];
             if (usw > 0)
               contribution += usw * v[index2 + 1];
             if (dsw > 0)
               contribution += dsw * v[index2 + MNhalf + 1];
             if (use > 0)
               contribution += use * v[index2 + Mhalf + 1];
             if (dse > 0)
               contribution += dse * v[index2 + MNhalf + Mhalf + 1];
             
             f_out[index1] += contribution / sum;
           }
         }
       }
     }
   }
 }


    
}
    
    class dataStruct {
        double[] A_array;
        double lhs[][] = new double[AlgorithmAntigradient2.MAX_LEVELS][];
    }