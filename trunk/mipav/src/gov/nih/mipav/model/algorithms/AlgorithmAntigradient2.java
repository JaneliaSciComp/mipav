package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;

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
% Author: Gunnar Farnebäck
%         Medical Informatics
%         Linköping University, Sweden
%         gunnar@imt.liu.se

 *
 */

public class AlgorithmAntigradient2 extends AlgorithmBase {
    
    private static final int RECURSION_SIZE_LIMIT = 4;
    
    /* This can without cost be overdimensioned. */
    private static final int MAX_LEVELS = 30;
    
    BitSet mask = null;
    
    double fMean = 0.0;
    
    int multigridIterations = 2;
    
    public AlgorithmAntigradient2(ModelImage destImage, ModelImage srcImg, double fMean, int multigridIterations) {
        super(destImage, srcImg);
        this.fMean = fMean;
        this.multigridIterations = multigridIterations;
    }
    
    public void runAlgorithm() {
        int nDims;
        mask = srcImage.generateVOIMask();
        nDims = srcImage.getNDims();
        
    }
    
    private class dataStruct {
        double[] A_array;
        double lhs[][] = new double[MAX_LEVELS][];
    }
    
}