package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.jama.LinearEquations2;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.File;
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
 * 
 * On 04/29/2013 11:32 PM, Gandler, William (NIH/CIT) [E] wrote:
> Gunnar,
>
>    I have ported Antigradient2.c to MIPAV and found its performance to be very impressive.  
     Would there be any reason to port Antigradient.c as well?  Does Antigradient.c have any capability
     not possessed by Antigradient2.c?
>
>                                                                                                
> Sincerely,
>
>                                                                                          
> William Gandler

The difference between antigradient.c and antigradient2.c is that the former doesn't allow
a mask and therefore only can handle rectangular domains. Not having a mask is a big limitation
but it does simplify the algorithm drastically, making the code simpler and faster. On the other hand,
for rectangular domains it's entirely possible that there are even faster transform-based methods.

I mostly implemented antigradient.c to learn the multigrid approach. 
Computing inverse gradients on irregular domains was central to my research at the time.

I'm happy to hear that it performs well.

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

On 04/23/2013 12:08 AM, Gandler, William (NIH/CIT) [E] wrote:
> Dear Gunnar:
>
>    In Antigradient2.c I note the 2 differences in poisson_multigrid2D and poisson_multigrid3D:
>
> 1.) poisson_multigrid2D has:
> // Initialize solution. 
>    memcpy(f_out, f, M * N * sizeof(*f_out)); but no corresponding line 
> is found in poisson_multigrid3D.

Looks like an artefact from some earlier development phase of the code or a not completely removed experiment. 
In all cases when poisson_multigrid2D is called, f_out and f point to the same memory, so the memcpy call doesn't do anything.

> 2.) poisson_multigrid2D has if (1) at the end but poisson_multigrid3D has if (0) at the end.

That's definitely leftovers from frustrated attempts to work around a bug that long plagued the code. 
I never checked if it still had any effect after finally tracking the bug down. It's not supposed to make a difference.

/Gunnar

Reference:
"Efficient Computation of the Inverse Gradient on Irregular Domains" by Gunnar Farneback,
Joakim Rydell, Tino Ebbers, Mats Andersson, and Hans Knutsson.
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
    private ModelImage testImage;
    boolean testMode = false;
    
    public AlgorithmAntigradient2(ModelImage destImage, ModelImage srcImg, boolean entireImage, 
                                  double fMean, int number_of_iterations) {
        this(destImage, srcImg, entireImage, fMean, number_of_iterations, false);
    }
    
    public AlgorithmAntigradient2(ModelImage destImage, ModelImage srcImg, boolean entireImage, 
                                  double fMean, int number_of_iterations, boolean testMode) {
        super(destImage, srcImg);
        this.entireImage = entireImage;
        this.fMean = fMean;
        this.number_of_iterations = number_of_iterations;
        if (!entireImage) {
           mask = srcImage.generateVOIMask();
        }
        this.testMode = testMode;
    }
    
    // Constructor used for creating gradient images for self test
    public AlgorithmAntigradient2() {
    }
    
    public void run2DSelfTest() {
        testMode = true;
        FileIO fileIO;
        boolean multiFile = false;
        String directory = new String("C:" + File.separatorChar + "images" + File.separatorChar);
        String fileName = new String("cap17black.fits");
        try {
            fileIO = new FileIO();

            testImage =  fileIO.readImage(fileName, directory, multiFile, null);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return;
        }
        number_of_iterations = 15;
        int origXDim = testImage.getExtents()[0];
        int origYDim = testImage.getExtents()[1];
        xDim = origXDim - 2;
        yDim = origYDim - 2;
        int origSliceSize = origXDim * origYDim;
        double buffer[] = new double[origSliceSize];
        int x;
        int y;
        double sum;
        int sliceSize = xDim * yDim;
        double gx[] = new double[sliceSize];
        double gy[] = new double[sliceSize];
        int srcExtents[] = new int[3];
        int destExtents[] = new int[2];
        AlgorithmAntigradient2 algTest;
        try {
            testImage.exportData(0, origSliceSize, buffer);
        }
        catch (IOException e) {
                displayError("AlgorithmAntigradient2: IOException on testImage.exportData(0, origSliceSize, buffer)");

                setCompleted(false);

                return;
        }
        sum = 0.0;
        for (y = 1; y < origYDim - 1; y++) {
            for (x = 1; x < origXDim - 1; x++) {
                sum += buffer[x + y*origXDim];
                gx[x - 1 + (y - 1)*xDim] = buffer[x+1 + (y-1)*origXDim] - buffer[x-1 + (y-1)*origXDim]
                                              + 2.0 * buffer[x+1 + y*origXDim] - 2.0*buffer[x-1 + y*origXDim]
                                              + buffer[x+1 + (y+1)*origXDim] - buffer[x-1 + (y+1)*origXDim];
                gy[x - 1 + (y - 1)*xDim] = buffer[x-1 + (y+1)*origXDim] - buffer[x-1 + (y-1)*origXDim]
                                              + 2.0*buffer[x + (y+1)*origXDim] - 2.0*buffer[x + (y-1)*origXDim]
                                              + buffer[x+1 + (y+1)*origXDim] - buffer[x+1 + (y-1)*origXDim];
            }
        }
        fMean = sum/sliceSize;
        srcExtents[0] = xDim;
        srcExtents[1] = yDim;
        srcExtents[2] = 2;
        ModelImage srcImage = new ModelImage(ModelStorageBase.DOUBLE, srcExtents, "test_source");
        try {
            srcImage.importData(0, gx, false);
        }
        catch(IOException e) {
            displayError("AlgorithmAntigradient2: IOException on srcImage.importData(0, gx, false)");

            setCompleted(false);

            return;  
        }
        
        try {
            srcImage.importData(sliceSize, gy, true);
        }
        catch(IOException e) {
            displayError("AlgorithmAntigradient2: IOException on srcImage.importData(sliceSize, gy, true)");

            setCompleted(false);

            return;  
        }
        destExtents[0] = xDim;
        destExtents[1] = yDim;
        ModelImage destImage = new ModelImage(ModelStorageBase.DOUBLE, destExtents, "test_destination");
        algTest = new AlgorithmAntigradient2(destImage, srcImage, true, fMean, number_of_iterations, testMode);
        algTest.run();
        return;
    }
    
    public void run3DSelfTest() {
        testMode = true;
        FileIO fileIO;
        boolean multiFile = false;
        String directory = new String("C:" + File.separatorChar + "images" + File.separatorChar);
        String fileName = new String("genormcor.fits");
        try {
            fileIO = new FileIO();

            testImage =  fileIO.readImage(fileName, directory, multiFile, null);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return;
        }
        number_of_iterations = 15;
        int origXDim = testImage.getExtents()[0];
        int origYDim = testImage.getExtents()[1];
        int origZDim = testImage.getExtents()[2];
        xDim = origXDim - 2;
        yDim = origYDim - 2;
        zDim = origZDim - 2;
        int origSliceSize = origXDim * origYDim;
        int origVolume = origSliceSize * origZDim;
        double buffer[] = new double[origVolume];
        int x;
        int y;
        int z;
        double sum;
        int sliceSize = xDim * yDim;
        int volume = sliceSize * zDim;
        double gx[] = new double[volume];
        double gy[] = new double[volume];
        double gz[] = new double[volume];
        int srcExtents[] = new int[4];
        int destExtents[] = new int[3];
        AlgorithmAntigradient2 algTest;
        try {
            testImage.exportData(0, origVolume, buffer);
        }
        catch (IOException e) {
                displayError("AlgorithmAntigradient2: IOException on testImage.exportData(0, origVolume, buffer)");

                setCompleted(false);

                return;
        }
        sum = 0.0;
        for (z = 1; z < origZDim - 1; z++) {
            for (y = 1; y < origYDim - 1; y++) {
                for (x = 1; x < origXDim - 1; x++) {
                    sum += buffer[x + y*origXDim + z*origSliceSize];
                    gx[x - 1 + (y - 1)*xDim + (z-1)*sliceSize] = 
                      buffer[x+1 + (y-1)*origXDim + (z-1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x+1 + (y-1)*origXDim + z*origSliceSize] - 2.0*buffer[x-1 + (y-1)*origXDim + z*origSliceSize]
                    + buffer[x+1 + (y-1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z+1)*origSliceSize]
                    + 2.0 * buffer[x+1 + y*origXDim + (z-1)*origSliceSize] - 2.0 * buffer[x-1 + y*origXDim + (z-1)*origSliceSize] 
                    + 4.0 * buffer[x+1 + y*origXDim + z*origSliceSize] - 4.0 * buffer[x-1 + y*origXDim + z*origSliceSize] 
                    + 2.0 * buffer[x+1 + y*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x-1 + y*origXDim + (z+1)*origSliceSize] 
                    + buffer[x+1 + (y+1)*origXDim + (z-1)*origSliceSize] - buffer[x-1 + (y+1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x+1 + (y+1)*origXDim + z*origSliceSize] - 2.0*buffer[x-1 + (y+1)*origXDim + z*origSliceSize]
                    + buffer[x+1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y+1)*origXDim + (z+1)*origSliceSize];
                    
                    gy[x - 1 + (y - 1)*xDim + (z-1)*sliceSize] = 
                      buffer[x-1 + (y+1)*origXDim + (z-1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x-1 + (y+1)*origXDim + z*origSliceSize] - 2.0 * buffer[x-1 + (y-1)*origXDim + z*origSliceSize] 
                    + buffer[x-1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z+1)*origSliceSize]
                    + 2.0 * buffer[x + (y+1)*origXDim + (z-1)*origSliceSize] - 2.0 * buffer[x + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 4.0 * buffer[x + (y+1)*origXDim + z*origSliceSize] - 4.0 * buffer[x + (y-1)*origXDim + z*origSliceSize] 
                    + 2.0 * buffer[x + (y+1)*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x + (y-1)*origXDim + (z+1)*origSliceSize]
                    + buffer[x+1 + (y+1)*origXDim + (z-1)*origSliceSize] - buffer[x+1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x+1 + (y+1)*origXDim + z*origSliceSize] - 2.0 * buffer[x+1 + (y-1)*origXDim + z*origSliceSize] 
                    + buffer[x+1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x+1 + (y-1)*origXDim + (z+1)*origSliceSize];
                    
                    gz[x - 1 + (y - 1)*xDim + (z-1)*sliceSize] = 
                      buffer[x-1 + (y-1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x-1 + y*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x-1 + y*origXDim + (z-1)*origSliceSize]
                    + buffer[x-1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x-1 + (y+1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x + (y-1)*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 4.0 * buffer[x + y*origXDim + (z+1)*origSliceSize] - 4.0 * buffer[x + y*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x + (y+1)*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x + (y+1)*origXDim + (z-1)*origSliceSize]
                    + buffer[x+1 + (y-1)*origXDim + (z+1)*origSliceSize] - buffer[x+1 + (y-1)*origXDim + (z-1)*origSliceSize]
                    + 2.0 * buffer[x+1 + y*origXDim + (z+1)*origSliceSize] - 2.0 * buffer[x+1 + y*origXDim + (z-1)*origSliceSize]
                    + buffer[x+1 + (y+1)*origXDim + (z+1)*origSliceSize] - buffer[x+1 + (y+1)*origXDim + (z-1)*origSliceSize];
                }
            }
        }
        fMean = sum/sliceSize;
        srcExtents[0] = xDim;
        srcExtents[1] = yDim;
        srcExtents[2] = zDim;
        srcExtents[3] = 3;
        ModelImage srcImage = new ModelImage(ModelStorageBase.DOUBLE, srcExtents, "test_source");
        try {
            srcImage.importData(0, gx, false);
        }
        catch(IOException e) {
            displayError("AlgorithmAntigradient2: IOException on srcImage.importData(0, gx, false)");

            setCompleted(false);

            return;  
        }
        
        try {
            srcImage.importData(volume, gy, false);
        }
        catch(IOException e) {
            displayError("AlgorithmAntigradient2: IOException on srcImage.importData(volume, gy, false)");

            setCompleted(false);

            return;  
        }
        
        try {
            srcImage.importData(2*volume, gz, true);
        }
        catch(IOException e) {
            displayError("AlgorithmAntigradient2: IOException on srcImage.importData(2*volume, gz, true)");

            setCompleted(false);

            return;  
        }
        destExtents[0] = xDim;
        destExtents[1] = yDim;
        destExtents[2] = zDim;
        ModelImage destImage = new ModelImage(ModelStorageBase.DOUBLE, destExtents, "test_destination");
        algTest = new AlgorithmAntigradient2(destImage, srcImage, true, fMean, number_of_iterations, testMode);
        algTest.run();
        return;
    }
    
    public void runAlgorithm() {
        fireProgressStateChanged(0, srcImage.getImageName(), "Inverse gradient on image ...");
        int nDims;
        nDims = srcImage.getNDims();
        if (nDims == 4) {
            antigradient3D();
        }
        else if (nDims == 3) {
            antigradient2D();
        }
        
    }
    
    private void antigradient2D() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1]; 
        int area = xDim * yDim;
        double rhs[];
        double lhs[];
        double weight[];
        double sum;
        double mean;
        int i;
        int num_samples_in_mask;
        int x;
        int y;
        int index1;
        int index2;
        double d;
        int N_missing;
        int S_missing;
        int W_missing;
        int E_missing;
        int ival;
        double f_out[];
        
        try {
            g = new double [2 * area];
        }
        catch (final OutOfMemoryError e) {
            displayError("AlgorithmAntigradient2: Out of memory on g = new double[2 * area]");

            setCompleted(false);

            return;
        }
        
        try {
            srcImage.exportData(0, 2 * area, g);
        }
        catch (IOException e) {
                displayError("AlgorithmAntigradient2: IOException on srcImage.exportData(0, 2*area, g)");

                setCompleted(false);

                return;
        }

        /* Compute left and right hand sides of Poisson problem with Neumann
         * boundary conditions, discretized by finite differences.
         */
        rhs = new double[area];
        lhs = new double[9 * area];
        data = new dataStruct();
        data.lhs[0] = lhs;
        weight = new double[area];
        for (y = 0; y < yDim; y++)
          for (x = 0; x < xDim; x++)
          {
            index1 = y * xDim + x;
            index2 = index1 + area;
            d = 0.0;
            N_missing = 0;
            S_missing = 0;
            W_missing = 0;
            E_missing = 0;

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
            
            rhs[index1] = d;

            ival = 0;
            if ((N_missing == 0) || (S_missing == 0)) {
                ival = ival - 2;
            }
            if ((W_missing == 0) || (E_missing == 0)) {
                ival = ival - 2;
            }
            lhs[9 * index1] = ival;
            if (N_missing == 0) {
                lhs[9*index1 + 1] = 1 + S_missing;
            }
            
            if (S_missing == 0) {
                lhs[9*index1 + 2] = 1 + N_missing;
            }
            
            if (W_missing == 0) {
                lhs[9*index1 + 3] = 1 + E_missing;
            }
            
            if (E_missing == 0) {
                lhs[9*index1 + 4] = 1 + W_missing;
            }
            
          }
        
          // Solve the equation system with the full multigrid algorithm.
          // Use W cycles and 2 presmoothing and 2 postsmoothing
          // Gauss-Seidel iterations.
       
        f_out = new double[area];
        poisson_full_multigrid2D(0, rhs, weight, number_of_iterations, xDim, yDim, f_out);
        
        /* Fix the mean value. */
        sum = 0.0;
        num_samples_in_mask = 0;
        for (i = 0; i < area; i++)
          if (weight[i] > 0.0)
          {
            sum += f_out[i];
            num_samples_in_mask++;
          }
        
        mean = sum / num_samples_in_mask;
        for (i = 0; i < area; i++)
          if (weight[i] > 0.0)
          {
            f_out[i] -= mean;
            f_out[i] += fMean;
          }
        
        try {
            destImage.importData(0, f_out, true);
        }
        catch(IOException e) {
            displayError("AlgorithmAntigradient2: IOException " + e + " on destImage.importData(0, f_out, true)");

            setCompleted(false);

            return;  
        }
        
        if (testMode) {
            new ViewJFrameImage(destImage);
            FileWriteOptions opts = new FileWriteOptions(true);
            opts.setFileType(FileUtility.XML);
            opts.setFileDirectory("C:" + File.separatorChar + "images" + File.separatorChar);
            opts.setFileName("cap17black_gradient.xml");
            opts.setBeginSlice(0);
            opts.setEndSlice(1);
            opts.setTimeSlice(0);
            opts.setEndTime(0);
            opts.setOptionsSet(true);
            FileIO fileIO = new FileIO();
            fileIO.writeImage(srcImage, opts);
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
        setCompleted(true);
        return;
    }
    
    /* It is assumed that f_out is initialized to zero when called. */
    private void poisson_full_multigrid2D(int level, double[] rhs, double[] weight,
                 int number_of_iterations, int M, int N, double[] f_out)
    {
      double rhs_downsampled[];
      double coarse_weight[];
      double f_coarse[];
      int k;
      int Mhalf;
      int Nhalf;
      
      /* Unless already coarsest scale, first recurse to coarser scale. */
      if (M >= RECURSION_SIZE_LIMIT && N >= RECURSION_SIZE_LIMIT)
      {
        /* Downsample right hand side. */
        Mhalf = (M + 1) / 2;
        Nhalf = (N + 1) / 2;
        rhs_downsampled = new double[Mhalf * Nhalf];
        coarse_weight = new double[Mhalf * Nhalf];
        downsample2D(rhs, M, N, rhs_downsampled, Mhalf, Nhalf,
             weight, coarse_weight);
        galerkin2D(level, M, N, Mhalf, Nhalf, weight, coarse_weight);
        f_coarse = new double[Mhalf * Nhalf];
        poisson_full_multigrid2D(level + 1, rhs_downsampled, coarse_weight,
                     number_of_iterations, Mhalf, Nhalf, f_coarse);
        
        /* Upsample the coarse result. */
        upsample2D(M, N, f_coarse, Mhalf, Nhalf, f_out,
               weight, coarse_weight);

        f_coarse = null;
        coarse_weight = null;
        rhs_downsampled = null;
      }
      
      /* Perform number_of_iterations standard multigrid cycles. */
      for (k = 0; k < number_of_iterations; k++)
      {
        int directly_solved[] = new int[1];
        poisson_multigrid2D(f_out, level, rhs, weight, 2, 2, 2, f_out, M, N,
                directly_solved);
        if (directly_solved[0] != 0)
          break;
      }
    }
    
    private void poisson_multigrid2D(double[] f, int level, double[] rhs, double[] weight,
            int n1, int n2, int nm,
            double[] f_out,
            int M, int N, int[] directly_solved)
{
  int k;
  double r[];
  double r_downsampled[];
  double coarse_weight[];
  double lhs[] = data.lhs[level];
  double v[];
  int Mhalf;
  int Nhalf;

  /* Solve a sufficiently small problem directly. */
  if (M < RECURSION_SIZE_LIMIT || N < RECURSION_SIZE_LIMIT)
  {
    solve_directly2D(lhs, rhs, f_out, M, N);
    directly_solved[0] = 1;
    return;
  }
  directly_solved[0] = 0;
  
  /* Pre-smoothing. */
  for (k = 0; k < n1; k++)
    gauss_seidel2D(f_out, lhs, rhs, M, N);
  
  /* Compute residual. */
  r = new double[M * N];
  compute_residual2D(r, lhs, rhs, f_out, M, N);

  /* Downsample residual. */
  Mhalf = (M + 1) / 2;
  Nhalf = (N + 1) / 2;
  r_downsampled = new double[Mhalf * Nhalf];
  coarse_weight = new double[Mhalf * Nhalf];
  downsample2D(r, M, N, r_downsampled, Mhalf, Nhalf, weight, coarse_weight);
  galerkin2D(level, M, N, Mhalf, Nhalf, weight, coarse_weight);
  
  /* Recurse to compute a correction. */
  v = new double[Mhalf * Nhalf];
  for (k = 0; k < nm; k++)
  {
   
    poisson_multigrid2D(v, level + 1, r_downsampled, coarse_weight,
            n1, n2, nm, v, Mhalf, Nhalf, directly_solved);
    if (directly_solved[0] != 0)
      break;
  }
  
  upsample2D(M, N, v, Mhalf, Nhalf, f_out, weight, coarse_weight);
  
  /* Post-smoothing. */
  for (k = 0; k < n2; k++)
    gauss_seidel2D(f_out, lhs, rhs, M, N);

  /* Set the mean value to zero.
   *
   * FIXME: This should not be needed (I believe) and might indicate
   * some bug elsewhere.
   */
  boolean needed = true;
  if (needed)
  {
    double sum = 0.0;
    int num_samples_in_mask = 0;
    double mean;
    int i;
    
    for (i = 0; i < M * N; i++)
      if (weight[i] != 0)
      {
    sum += f_out[i];
    num_samples_in_mask++;
      }
    
    mean = sum / num_samples_in_mask;
    for (i = 0; i < M * N; i++)
      if (weight[i] != 0)
    f_out[i] -= mean;
  }
  
  r = null;
  r_downsampled = null;
  coarse_weight = null;
  v = null;

}
    
    private void solve_directly2D(double []lhs, double []rhs, double []f_out, int M, int N)
    {
      int s = M * N;
      double b[][];
      double A_arr[][];
      LinearEquations2 le;
      int ipiv[];
      int info[] = new int[1];
      int i, j;

      if (data.A_array == null)
      {
        double A[];
        
        //dims[0] = s;
        //dims[1] = s;
        data.A_array = new double[s * s];
        A = data.A_array;
      
        for (j = 0; j < N; j++)
          for (i = 0; i < M; i++)
          {
        int index = j * M + i;
        A[index + s * index] = lhs[9 * index];
        if (lhs[9 * index] == 0.0) {
            A[index + s * index]++;    
        }
        if (i > 0)
          A[index + s * (index - 1)] = lhs[9 * index + 1];
        if (i < M-1)
          A[index + s * (index + 1)] = lhs[9 * index + 2];
        if (j > 0)
          A[index + s * (index - M)] = lhs[9 * index + 3];
        if (j < N-1)
          A[index + s * (index + M)] = lhs[9 * index + 4];
        if (i > 0 && j > 0)
          A[index + s * (index - M - 1)] = lhs[9 * index + 5];
        if (i > 0 && j < N-1)
          A[index + s * (index + M - 1)] = lhs[9 * index + 6];
        if (i < M-1 && j > 0)
          A[index + s * (index - M + 1)] = lhs[9 * index + 7];
        if (i < M-1 && j < N-1)
          A[index + s * (index + M + 1)] = lhs[9 * index + 8];
          }
        
        for (i = 0; i < s*s; i++)
          A[i] += 1.0 / (s*s);
      }
      
      //dims[0] = s;
      //dims[1] = 1;
      b = new double[s][1];
      for (i = 0; i < s; i++) {
          b[i][0] = rhs[i];
      }
      // In the original code C is calling MATLAB, so while data would be in row major order in C,
      // it must be put in column major order for MATLAB.
      // Solving Ax = b
      A_arr = new double[s][s];
      for (j = 0; j < s; j++) {
          for (i = 0; i < s; i++) {
              A_arr[i][j] = data.A_array[i + j*s];
          }
      }
      le = new LinearEquations2();
      ipiv = new int[s];
      le.dgesv(s, 1, A_arr, s, ipiv, b, s, info);
      if (info[0] != 0) {
          Preferences.debug("In solve_directly2D le.dgesv had info[0] = " + info[0], Preferences.DEBUG_ALGORITHM);
          return;
      }
      
      for (i = 0; i < s; i++) {
          f_out[i] = b[i][0];
      }
      
      
    }
    
    /* Gauss-Seidel smoothing iteration. Red-black ordering. */
    private void gauss_seidel2D(double[] f, double[] A, double[] d, int M, int N)
    {
      int pass;
      int i, j;
      int index;
      
      for (pass = 0; pass <= 1; pass++)
      {
        for (j = 0; j < N; j++)
          for (i = 0; i < M; i++)
          {
        double new_f;

        if ((i + j) % 2 != pass)
          continue;
        
        index = i + j * M;
        if (A[9 * index] == 0.0)
          continue;
        
        new_f = d[index];
        if (i > 0)
          new_f -= A[9 * index + 1] * f[index - 1];

        if (i < M-1)
          new_f -= A[9 * index + 2] * f[index + 1];

        if (j > 0)
          new_f -= A[9 * index + 3] * f[index - M];

        if (j < N-1)
          new_f -= A[9 * index + 4] * f[index + M];
        
        if (i > 0 && j > 0)
          new_f -= A[9 * index + 5] * f[index - 1 - M];

        if (i > 0 && j < N-1)
          new_f -= A[9 * index + 6] * f[index - 1 + M];

        if (i < M-1 && j > 0)
          new_f -= A[9 * index + 7] * f[index + 1 - M];

        if (i < M-1 && j < N-1)
          new_f -= A[9 * index + 8] * f[index + 1 + M];

        f[index] = new_f / A[9 * index];
          }
      }
    }
    
    private void compute_residual2D(double []r, double []A, double []d, double []f, int M, int N)
    {
      int i, j;

      for (j = 0; j < N; j++)
        for (i = 0; i < M; i++)
          {
          int index = j * M + i;
          double residual = 0.0;
          if (A[9 * index] != 0.0)
          {
        residual = d[index] - A[9 * index] * f[index];
        if (i > 0)
          residual -= A[9 * index + 1] * f[index - 1];
        
        if (i < M-1)
          residual -= A[9 * index + 2] * f[index + 1];
        
        if (j > 0)
          residual -= A[9 * index + 3] * f[index - M];
        
        if (j < N-1)
          residual -= A[9 * index + 4] * f[index + M];
        
        if (i > 0 && j > 0)
          residual -= A[9 * index + 5] * f[index - 1 - M];
        
        if (i > 0 && j < N-1)
          residual -= A[9 * index + 6] * f[index - 1 + M];
        
        if (i < M-1 && j > 0)
          residual -= A[9 * index + 7] * f[index + 1 - M];
        
        if (i < M-1 && j < N-1)
          residual -= A[9 * index + 8] * f[index + 1 + M];
          }
          
          r[index] = residual;
        }
    }




    
    private void downsample2D(double[] rhs, int M, int N,
            double[] rhs_coarse, int Mhalf, int Nhalf,
            double[] weight, double[] coarse_weight)
   {
     int i, j;
     int index1;
     int index2;
     double c, n, s, w, e, nw, ne, sw, se;
     double sum;
     
     if (M % 2 == 0 && N % 2 == 0)
     {
       for (j = 0; j < Nhalf; j++)
         for (i = 0; i < Mhalf; i++)
         {
       index1 = (j * Mhalf + i);
       index2 = (2 * j * M + 2 * i);

       nw = weight[index2];
       ne = weight[index2 + M];
       sw = weight[index2 + 1];
       se = weight[index2 + M + 1];
       sum = nw + ne + sw + se;
       coarse_weight[index1] = sum;

       if (sum > 0)
       {
         rhs_coarse[index1] = 4 / sum * (nw * rhs[index2]
                         + ne * rhs[index2 + M]
                         + sw * rhs[index2 + 1]
                         + se * rhs[index2 + M + 1]);
       }
         }
     }
     
     if (M % 2 == 1 && N % 2 == 0)
     {
       for (j = 0; j < Nhalf; j++)
         for (i = 0; i < Mhalf; i++)
         {
       double result;
       index1 = (j * Mhalf + i);
       index2 = (2 * j * M + 2 * i);
       
       nw = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
       ne = 0.5 * (i > 0 ?  weight[index2 + M - 1] : 0.0);
       w  = weight[index2];
       e  = weight[index2 + M];
       sw = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
       se = 0.5 * (i < Mhalf - 1 ? weight[index2 + M + 1] : 0.0);
       sum = nw + ne + w + e + sw + se;
       coarse_weight[index1] = sum;

       if (sum > 0)
       {
         result = w * rhs[index2] + e * rhs[index2 + M];
         if (i > 0)
           result += nw * rhs[index2 - 1] + ne * rhs[index2 + M - 1];
         if (i < Mhalf - 1)
           result += sw * rhs[index2 + 1] + se * rhs[index2 + M + 1];
         
         rhs_coarse[index1] = 4 / sum * result;
       }
         }
     }
     
     if (M % 2 == 0 && N % 2 == 1)
     {
       for (j = 0; j < Nhalf; j++)
         for (i = 0; i < Mhalf; i++)
         {
       double result;
       index1 = (j * Mhalf + i);
       index2 = (2 * j * M + 2 * i);
       
       nw = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
       sw = 0.5 * (j > 0 ? weight[index2 - M + 1] : 0.0);
       n  = weight[index2];
       s  = weight[index2 + 1];
       ne = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
       se = 0.5 * (j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
       sum = nw + sw + n + s + ne + se;
       coarse_weight[index1] = sum;

       if (sum > 0)
       {
         result = n * rhs[index2] + s * rhs[index2 + 1];
         if (j > 0)
           result += nw * rhs[index2 - M] + sw * rhs[index2 - M + 1];
         if (j < Nhalf - 1)
           result += ne * rhs[index2 + M] + se * rhs[index2 + M + 1];
         
         rhs_coarse[index1] = 4 / sum * result;
       }
         }
     }

     if (M % 2 == 1 && N % 2 == 1)
     {
       for (j = 0; j < Nhalf; j++)
         for (i = 0; i < Mhalf; i++)
         {
       double result;
       index1 = (j * Mhalf + i);
       index2 = (2 * j * M + 2 * i);
       
       c  = weight[index2];
       n  = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
       s  = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
       w  = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
       e  = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
       nw = 0.25 * (i > 0 && j > 0 ? weight[index2 - M - 1] : 0.0);
       ne = 0.25 * (i > 0 && j < Nhalf - 1 ? weight[index2 + M - 1] : 0.0);
       sw = 0.25 * (i < Mhalf - 1 && j > 0 ? weight[index2 - M + 1] : 0.0);
       se = 0.25 * (i < Mhalf - 1 && j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
       sum = c + n + s + w + e + nw + ne + sw + se;
       coarse_weight[index1] = sum;

       if (sum > 0)
       {
         result = c * rhs[index2];
         if (n > 0)
           result += n * rhs[index2 - 1];
         if (s > 0)
           result += s * rhs[index2 + 1];
         if (w > 0)
           result += w * rhs[index2 - M];
         if (e > 0)
           result += e * rhs[index2 + M];
         if (nw > 0)
           result += nw * rhs[index2 - M - 1];
         if (ne > 0)
           result += ne * rhs[index2 + M - 1];
         if (sw > 0)
           result += sw * rhs[index2 - M + 1];
         if (se > 0)
           result += se * rhs[index2 + M + 1];
       
         rhs_coarse[index1] = 4 / sum * result;
       }
         }
     }
   }
    
    private void galerkin2D(int level, int M, int N, int Mhalf, int Nhalf,
            double[] weight, double[] coarse_weight)
     {
       int i, j;
       double lhs[];
       double lhs_coarse[];

       if (data.lhs[level + 1] != null)
         return;

       data.lhs[level + 1] = new double[9 * Mhalf * Nhalf];
       lhs = data.lhs[level];
       lhs_coarse = data.lhs[level + 1];
       
       for (j = 0; j < Nhalf; j++)
         for (i = 0; i < Mhalf; i++)
         {
           int index1 = (j * Mhalf + i);
           int index2 = (2 * j * M + 2 * i);
           double stencil1[][] = new double[3][3];
           double stencil2[][] = new double[5][5];
           double stencil3[][] = new double[3][3];
           double mask1[][] = new double[3][3];
           int u, v;

           for (u = 0; u < 5; u++)
         for (v = 0; v < 5; v++)
         {
           stencil2[u][v] = 0;
           if (u < 3 && v < 3)
           {
             stencil1[u][v] = 0;
             stencil3[u][v] = 0;
           }
         }

           mask1[1][1] = coarse_weight[index1];
           mask1[0][1] = (i > 0 ? coarse_weight[index1 - 1] : 0.0);
           mask1[2][1] = (i < Mhalf - 1 ? coarse_weight[index1 + 1] : 0.0);
           mask1[1][0] = (j > 0 ? coarse_weight[index1 - Mhalf] : 0.0);
           mask1[1][2] = (j < Nhalf - 1 ? coarse_weight[index1 + Mhalf] : 0.0);
           mask1[0][0] = (i > 0 && j > 0 ? coarse_weight[index1 - Mhalf - 1] : 0.0);
           mask1[0][2] = (i > 0 && j < Nhalf - 1 ? coarse_weight[index1 + Mhalf - 1] : 0.0);
           mask1[2][0] = (i < Mhalf - 1 && j > 0 ? coarse_weight[index1 - Mhalf + 1] : 0.0);
           mask1[2][2] = (i < Mhalf - 1 && j < Nhalf - 1 ? coarse_weight[index1 + Mhalf + 1] : 0.0);
           
           if (M % 2 == 0 && N % 2 == 0)
           {
         double nw = weight[index2];
         double ne = weight[index2 + M];
         double sw = weight[index2 + 1];
         double se = weight[index2 + M + 1];

         double mean = (nw + sw + ne + se) / 4;

         /* If mean is 0 here we can short-circuit. */
         if (mean == 0)
           continue;
         
         stencil1[1][1] = nw / mean;
         stencil1[1][2] = ne / mean;
         stencil1[2][1] = sw / mean;
         stencil1[2][2] = se / mean;
           }
           
           if (M % 2 == 1 && N % 2 == 0)
           {
         double nw = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
         double ne = 0.5 * (i > 0 ? weight[index2 + M - 1] : 0.0);
         double w  = weight[index2];
         double e  = weight[index2 + M];
         double sw = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
         double se = 0.5 * (i < Mhalf - 1 ? weight[index2 + M + 1] : 0.0);

         double mean = (w + e + nw + ne + sw + se) / 4;

         /* If mean is 0 here we can short-circuit. */
         if (mean == 0)
           continue;
         
         stencil1[1][1] = w / mean;
         stencil1[1][2] = e / mean;
         stencil1[2][1] = sw / mean;
         stencil1[2][2] = se / mean;
         stencil1[0][1] = nw / mean;
         stencil1[0][2] = ne / mean;
           }

           if (M % 2 == 0 && N % 2 == 1)
           {
         double nw = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
         double sw = 0.5 * (j > 0 ? weight[index2 - M + 1] : 0.0);
         double n  = weight[index2];
         double s  = weight[index2 + 1];
         double ne = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
         double se = 0.5 * (j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);
         
         double mean = (n + s + nw + ne + sw + se) / 4;

         /* If mean is 0 here we can short-circuit. */
         if (mean == 0)
           continue;
         
         stencil1[1][1] = n / mean;
         stencil1[2][1] = s / mean;
         stencil1[1][2] = ne / mean;
         stencil1[2][2] = se / mean;
         stencil1[1][0] = nw / mean;
         stencil1[2][0] = sw / mean;
           }
           
           if (M % 2 == 1 && N % 2 == 1)
           {
         double c  = weight[index2];
         double n  = 0.5 * (i > 0 ? weight[index2 - 1] : 0.0);
         double s  = 0.5 * (i < Mhalf - 1 ? weight[index2 + 1] : 0.0);
         double w  = 0.5 * (j > 0 ? weight[index2 - M] : 0.0);
         double e  = 0.5 * (j < Nhalf - 1 ? weight[index2 + M] : 0.0);
         double nw = 0.25 * (i > 0 && j > 0 ? weight[index2 - M - 1] : 0.0);
         double ne = 0.25 * (i > 0 && j < Nhalf - 1 ? weight[index2 + M - 1] : 0.0);
         double sw = 0.25 * (i < Mhalf - 1 && j > 0 ? weight[index2 - M + 1] : 0.0);
         double se = 0.25 * (i < Mhalf - 1 && j < Nhalf - 1 ? weight[index2 + M + 1] : 0.0);

         double mean = (c + s + e + w + n + se + sw + ne + nw) / 4;

         /* If mean is 0 here we can short-circuit. */
         if (mean == 0)
           continue;
         
         stencil1[0][0] = nw / mean;
         stencil1[0][1] = n / mean;
         stencil1[0][2] = ne / mean;
         stencil1[1][0] = w / mean;
         stencil1[1][1] = c / mean;
         stencil1[1][2] = e / mean;
         stencil1[2][0] = sw / mean;
         stencil1[2][1] = s / mean;
         stencil1[2][2] = se / mean;
           }

           for (u = 0; u < 3; u++)
         for (v = 0; v < 3; v++)
         {
           if (stencil1[u][v] != 0)
           {
             int index = 9 * (index2 + (u-1) + M*(v-1));
             if (lhs[index] != 0.0)
             {
               stencil2[u+1][v+1] += stencil1[u][v] * lhs[index];
               stencil2[u  ][v+1] += stencil1[u][v] * lhs[index + 1];
               stencil2[u+2][v+1] += stencil1[u][v] * lhs[index + 2];
               stencil2[u+1][v  ] += stencil1[u][v] * lhs[index + 3];
               stencil2[u+1][v+2] += stencil1[u][v] * lhs[index + 4];
               stencil2[u  ][v  ] += stencil1[u][v] * lhs[index + 5];
               stencil2[u  ][v+2] += stencil1[u][v] * lhs[index + 6];
               stencil2[u+2][v  ] += stencil1[u][v] * lhs[index + 7];
               stencil2[u+2][v+2] += stencil1[u][v] * lhs[index + 8];
             }
           }
         }

           if (M % 2 == 0 && N % 2 == 0)
           {
         for (u = 1; u < 5; u++)
           for (v = 1; v < 5; v++)
           {
             double alpha1, alpha2;
             double nw, ne, sw, se;
             int uu, vv;
             double sum;
             
             if (u % 2 == 0)
               alpha1 = 0.75;
             else
               alpha1 = 0.25;

             if (v % 2 == 0)
               alpha2 = 0.75;
             else
               alpha2 = 0.25;

             uu = (u-1)/2;
             vv = (v-1)/2;
             nw = (1 - alpha1) * (1 - alpha2) * mask1[uu  ][vv  ];
             ne = (1 - alpha1) *      alpha2  * mask1[uu  ][vv+1];
             sw =      alpha1  * (1 - alpha2) * mask1[uu+1][vv  ];
             se =      alpha1  *      alpha2  * mask1[uu+1][vv+1];

             sum = nw + ne + sw + se;
             if (sum > 0)
             {
               stencil3[uu  ][vv  ] += nw * stencil2[u][v] / sum;
               stencil3[uu  ][vv+1] += ne * stencil2[u][v] / sum;
               stencil3[uu+1][vv  ] += sw * stencil2[u][v] / sum;
               stencil3[uu+1][vv+1] += se * stencil2[u][v] / sum;
             }
           }
           }

           if (M % 2 == 1 && N % 2 == 0)
           {
         for (u = 0; u < 5; u++)
           for (v = 1; v < 5; v++)
           {
             double alpha1, alpha2;
             double nw, ne, sw, se;
             int uu, vv;
             double sum;
             if (u % 2 == 0)
               alpha1 = 0;
             else
               alpha1 = 0.5;
             
             if (v % 2 == 0)
               alpha2 = 0.75;
             else
               alpha2 = 0.25;
             
             uu = u/2;
             vv = (v-1)/2;
             nw = (1 - alpha1) * (1 - alpha2) * mask1[uu  ][vv  ];
             ne = (1 - alpha1) *      alpha2  * mask1[uu  ][vv+1];
             sw = 0;
             se = 0;
             if (alpha1 > 0)
             {
               sw = alpha1  * (1 - alpha2) * mask1[uu+1][vv  ];
               se = alpha1  *      alpha2  * mask1[uu+1][vv+1];
             }

             sum = nw + ne + sw + se;
             if (sum > 0)
             {
               stencil3[uu  ][vv  ] += nw * stencil2[u][v] / sum;
               stencil3[uu  ][vv+1] += ne * stencil2[u][v] / sum;
               if (alpha1 > 0)
               {
             stencil3[uu+1][vv  ] += sw * stencil2[u][v] / sum;
             stencil3[uu+1][vv+1] += se * stencil2[u][v] / sum;
               }
             }
           }
           }

           if (M % 2 == 0 && N % 2 == 1)
           {
         for (u = 1; u < 5; u++)
           for (v = 0; v < 5; v++)
           {
             double alpha1, alpha2;
             double nw, ne, sw, se;
             int uu, vv;
             double sum;
             if (u % 2 == 0)
               alpha1 = 0.75;
             else
               alpha1 = 0.25;
             
             if (v % 2 == 0)
               alpha2 = 0;
             else
               alpha2 = 0.5;
             
             uu = (u-1)/2;
             vv = v/2;
             nw = (1 - alpha1) * (1 - alpha2) * mask1[uu  ][vv  ];
             sw =      alpha1  * (1 - alpha2) * mask1[uu+1][vv  ];
             ne = 0;
             se = 0;
             if (alpha2 > 0)
             {
               ne = (1 - alpha1) * alpha2 * mask1[uu  ][vv+1];
               se =      alpha1  * alpha2 * mask1[uu+1][vv+1];
             }

             sum = nw + ne + sw + se;
             if (sum > 0)
             {
               stencil3[uu  ][vv  ] += nw * stencil2[u][v] / sum;
               stencil3[uu+1][vv  ] += sw * stencil2[u][v] / sum;
               if (alpha2 > 0)
               {
             stencil3[uu  ][vv+1] += ne * stencil2[u][v] / sum;
             stencil3[uu+1][vv+1] += se * stencil2[u][v] / sum;
               }
             }
           }
           }

           if (M % 2 == 1 && N % 2 == 1)
           {
         for (u = 0; u < 5; u++)
           for (v = 0; v < 5; v++)
           {
             double alpha1, alpha2;
             double nw, ne, sw, se;
             int uu, vv;
             double sum;
             if (u % 2 == 0)
               alpha1 = 0;
             else
               alpha1 = 0.5;
             
             if (v % 2 == 0)
               alpha2 = 0;
             else
               alpha2 = 0.5;

             uu = u/2;
             vv = v/2;
             nw = (1 - alpha1) * (1 - alpha2) * mask1[uu  ][vv  ];
             sw = 0;
             if (alpha1 > 0)
               sw =      alpha1  * (1 - alpha2) * mask1[uu+1][vv  ];
             ne = 0;
             if (alpha2 > 0)
               ne = (1 - alpha1) * alpha2 * mask1[uu  ][vv+1];
             se = 0;
             if (alpha1 > 0 && alpha2 > 0)
               se =      alpha1  * alpha2 * mask1[uu+1][vv+1];

             sum = nw + ne + sw + se;
             if (sum > 0)
             {
               stencil3[uu  ][vv  ] += nw * stencil2[u][v] / sum;
               if (alpha1 > 0)
             stencil3[uu+1][vv  ] += sw * stencil2[u][v] / sum;
               if (alpha2 > 0)
             stencil3[uu  ][vv+1] += ne * stencil2[u][v] / sum;
               if (alpha1 > 0 && alpha2 > 0)
             stencil3[uu+1][vv+1] += se * stencil2[u][v] / sum;
             }
           }
           }

           lhs_coarse[9 * index1]     = stencil3[1][1];
           lhs_coarse[9 * index1 + 1] = stencil3[0][1];
           lhs_coarse[9 * index1 + 2] = stencil3[2][1];
           lhs_coarse[9 * index1 + 3] = stencil3[1][0];
           lhs_coarse[9 * index1 + 4] = stencil3[1][2];
           lhs_coarse[9 * index1 + 5] = stencil3[0][0];
           lhs_coarse[9 * index1 + 6] = stencil3[0][2];
           lhs_coarse[9 * index1 + 7] = stencil3[2][0];
           lhs_coarse[9 * index1 + 8] = stencil3[2][2];

           for (u = 1; u < 9; u++)
         if (lhs_coarse[9 * index1 + u] < 0)
         {
           lhs_coarse[9 * index1] += lhs_coarse[9 * index1 + u];
           lhs_coarse[9 * index1 + u] = 0;
         }
         }
     }
    
    private void upsample2D(int M, int N,
            double[] v, int Mhalf, int Nhalf,
            double[] f_out, double[] weight, double[] coarse_weight)
     {
       int i, j;
       int index1, index2;
       
       if (M % 2 == 0 && N % 2 == 0)
       {
         for (j = 0; j < N; j++)
           for (i = 0; i < M; i++)
           {
         double alpha1, alpha2;
         double nw, ne, sw, se;
         double sum;
         
         index1 = j * M + i;
         index2 = ((j + 1) / 2 - 1) * Mhalf + (i + 1) / 2 - 1;

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
         
         nw = (1 - alpha1) * (1 - alpha2) * (i > 0 && j > 0 ? coarse_weight[index2] : 0.0);
         ne = (1 - alpha1) *      alpha2  * (i > 0 && j < N - 1 ? coarse_weight[index2 + Mhalf] : 0.0);
         sw =      alpha1  * (1 - alpha2) * (i < M - 1 && j > 0 ? coarse_weight[index2 + 1] : 0.0);
         se =      alpha1  *      alpha2  * (i < M - 1 && j < N - 1 ? coarse_weight[index2 + Mhalf + 1] : 0.0);
         
         sum = nw + ne + sw + se;

         if (sum > 0)
         {
           double contribution = 0;
           
           if (nw > 0)
             contribution += nw * v[index2];
           if (ne > 0)
             contribution += ne * v[index2 + Mhalf];
           if (sw > 0)
             contribution += sw * v[index2 + 1];
           if (se > 0)
             contribution += se * v[index2 + Mhalf + 1];
           
           f_out[index1] += contribution / sum;
         }
           }
       }
         
       if (M % 2 == 1 && N % 2 == 0)
       {
         for (j = 0; j < N; j++)
           for (i = 0; i < M; i++)
           {
         double alpha1, alpha2;
         double nw, ne, sw, se;
         double sum;
         
         index1 = j * M + i;
         index2 = ((j + 1) / 2 - 1) * Mhalf + i / 2;

         if (weight[index1] == 0)
           continue;

         if (i % 2 == 0)
           alpha1 = 0.0;
         else
           alpha1 = 0.5;
         
         if (j % 2 == 0)
           alpha2 = 0.75;
         else
           alpha2 = 0.25;
         
         nw = (1 - alpha1) * (1 - alpha2) * (j > 0 ? coarse_weight[index2] : 0.0);
         ne = (1 - alpha1) *      alpha2  * (j < N - 1 ? coarse_weight[index2 + Mhalf] : 0.0);
         sw =      alpha1  * (1 - alpha2) * (i < M - 1 && j > 0 ? coarse_weight[index2 + 1] : 0.0);
         se =      alpha1  *      alpha2  * (i < M - 1 && j < N - 1 ? coarse_weight[index2 + Mhalf + 1] : 0.0);
         
         sum = nw + ne + sw + se;

         if (sum > 0)
         {
           double contribution = 0;
           
           if (nw > 0)
             contribution += nw * v[index2];
           if (ne > 0)
             contribution += ne * v[index2 + Mhalf];
           if (sw > 0)
             contribution += sw * v[index2 + 1];
           if (se > 0)
             contribution += se * v[index2 + Mhalf + 1];
           
           f_out[index1] += contribution / sum;
         }
           }
       }
       
       if (M % 2 == 0 && N % 2 == 1)
       {
         for (j = 0; j < N; j++)
           for (i = 0; i < M; i++)
           {
         double alpha1, alpha2;
         double nw, ne, sw, se;
         double sum;
         
         index1 = j * M + i;
         index2 = (j / 2) * Mhalf + (i + 1) / 2 - 1;

         if (weight[index1] == 0)
           continue;

         if (i % 2 == 0)
           alpha1 = 0.75;
         else
           alpha1 = 0.25;
         
         if (j % 2 == 0)
           alpha2 = 0.0;
         else
           alpha2 = 0.5;
         
         nw = (1 - alpha1) * (1 - alpha2) * (i > 0 ? coarse_weight[index2] : 0.0);
         ne = (1 - alpha1) *      alpha2  * (i > 0 && j < N - 1 ? coarse_weight[index2 + Mhalf] : 0.0);
         sw =      alpha1  * (1 - alpha2) * (i < M - 1 ? coarse_weight[index2 + 1] : 0.0);
         se =      alpha1  *      alpha2  * (i < M - 1 && j < N - 1 ? coarse_weight[index2 + Mhalf + 1] : 0.0);
         
         sum = nw + ne + sw + se;

         if (sum > 0)
         {
           double contribution = 0;
           
           if (nw > 0)
             contribution += nw * v[index2];
           if (ne > 0)
             contribution += ne * v[index2 + Mhalf];
           if (sw > 0)
             contribution += sw * v[index2 + 1];
           if (se > 0)
             contribution += se * v[index2 + Mhalf + 1];
           
           f_out[index1] += contribution / sum;
         }
           }
       }
       
       if (M % 2 == 1 && N % 2 == 1)
       {
         for (j = 0; j < N; j++)
           for (i = 0; i < M; i++)
           {
         double alpha1, alpha2;
         double nw, ne, sw, se;
         double sum;
         
         index1 = j * M + i;
         index2 = (j / 2) * Mhalf + i / 2;

         if (weight[index1] == 0)
           continue;

         if (i % 2 == 0)
           alpha1 = 0.0;
         else
           alpha1 = 0.5;
         
         if (j % 2 == 0)
           alpha2 = 0.0;
         else
           alpha2 = 0.5;
         
         nw = (1 - alpha1) * (1 - alpha2) * coarse_weight[index2];
         ne = (1 - alpha1) *      alpha2  * (j < N - 1 ? coarse_weight[index2 + Mhalf] : 0.0);
         sw =      alpha1  * (1 - alpha2) * (i < M - 1 ? coarse_weight[index2 + 1] : 0.0);
         se =      alpha1  *      alpha2  * (i < M - 1 && j < N - 1 ? coarse_weight[index2 + Mhalf + 1] : 0.0);
         
         sum = nw + ne + sw + se;

         if (sum > 0)
         {
           double contribution = 0;
           
           if (nw > 0)
             contribution += nw * v[index2];
           if (ne > 0)
             contribution += ne * v[index2 + Mhalf];
           if (sw > 0)
             contribution += sw * v[index2 + 1];
           if (se > 0)
             contribution += se * v[index2 + Mhalf + 1];
           
           f_out[index1] += contribution / sum;
         }
           }
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
        
        if (testMode) {
            new ViewJFrameImage(destImage);
            FileWriteOptions opts = new FileWriteOptions(true);
            opts.setFileType(FileUtility.XML);
            opts.setFileDirectory("C:" + File.separatorChar + "images" + File.separatorChar);
            opts.setFileName("genormcor_gradient.xml");
            opts.setBeginSlice(0);
            opts.setEndSlice(zDim-1);
            opts.setTimeSlice(0);
            opts.setEndTime(2);
            opts.setOptionsSet(true);
            FileIO fileIO = new FileIO();
            fileIO.writeImage(srcImage, opts);
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
        setCompleted(true);
        return;
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
        
        // Perform number_of_iterations standard multigrid cycles.
        for (k = 0; k < number_of_iterations; k++)
        {
          int directly_solved[] = new int[1];
          poisson_multigrid3D(level, rhs, weight, 2, 2, 2, f_out, M, N, P,
                  directly_solved);
          if (directly_solved[0] != 0)
            break;
        }

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
        gauss_seidel3D(f_out, lhs, rhs, M, N, P);
      
      /* Compute residual. */
      r = new double[M * N * P];
      compute_residual3D(r, lhs, rhs, f_out, M, N, P);
      
      /* Downsample residual. */
      Mhalf = (M + 1) / 2;
      Nhalf = (N + 1) / 2;
      Phalf = (P + 1) / 2;
      r_downsampled = new double[Mhalf * Nhalf * Phalf];
      coarse_weight = new double[Mhalf * Nhalf * Phalf];
      downsample3D(r, M, N, P, r_downsampled, Mhalf, Nhalf, Phalf,
               weight, coarse_weight);
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
      for (k = 0; k < n2; k++)
        gauss_seidel3D(f_out, lhs, rhs, M, N, P);
      
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
    
    private void gauss_seidel3D(double[] f, double[] A, double[] d, int M, int N, int P)
    {
      int pass;
      int i, j, p;
      int index;
      int MN = M * N;
      
      for (pass = 0; pass <= 1; pass++)
      {
        for (p = 0; p < P; p++)
          for (j = 0; j < N; j++)
        for (i = 0; i < M; i++)
        {
          double new_f;
          int k;
          
          if ((i + j + p) % 2 != pass)
            continue;
          
          index = (p * N + j) * M + i;

          if (A[27 * index + 13] == 0)
            continue;
          
          new_f = d[index];
          for (k = 0; k < 27; k++)
          {
            int u = (k % 3) - 1;
            int v = ((k / 3) % 3) - 1;
            int w = ((k / 9) % 3) - 1;

            if (k != 13 && A[27 * index + k] != 0)
              new_f -= A[27 * index + k] * f[index + u + v * M + w * MN];
          }

          f[index] = new_f / A[27 * index + 13];
        }
      }
    }
    
    private void compute_residual3D(double[] r, double[] A, double[] d, double[] f,
            int M, int N, int P)
{
int i, j, p;
int MN = M * N;

for (p = 0; p < P; p++)
for (j = 0; j < N; j++)
for (i = 0; i < M; i++)
{
int index = (p * N + j) * M + i;
double residual = 0;
if (A[27 * index + 13] != 0)
{
int k;
residual = d[index];
for (k = 0; k < 27; k++)
{
 int u = (k % 3) - 1;
 int v = ((k / 3) % 3) - 1;
 int w = ((k / 9) % 3) - 1;

 if (A[27 * index + k] != 0)
   residual -= A[27 * index + k] * f[index + u + v * M + w * MN];
}
}

r[index] = residual;
}
}


    
    private void solve_directly3D(double[] lhs, double[] rhs, double[] f_out, int M, int N, int P)
    {
      int s = M * N * P;
      int MN = M * N;
      double b[][];
      int i, j, p;
      double A_arr[][];
      LinearEquations2 le;
      int ipiv[];
      int info[] = new int[1];

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
      b = new double[s][1];
      for (i = 0; i < s; i++) {
          b[i][0] = rhs[i];
      }
      // In the original code C is calling MATLAB, so while data would be in row major order in C,
      // it must be put in column major order for MATLAB.
      // Solving Ax = b
      A_arr = new double[s][s];
      for (j = 0; j < s; j++) {
          for (i = 0; i < s; i++) {
              A_arr[i][j] = data.A_array[i + j*s];
          }
      }
      le = new LinearEquations2();
      ipiv = new int[s];
      le.dgesv(s, 1, A_arr, s, ipiv, b, s, info);
      if (info[0] != 0) {
          Preferences.debug("In solve_directly3D le.dgesv had info[0] = " + info[0], Preferences.DEBUG_ALGORITHM);
          return;
      }
      
      for (i = 0; i < s; i++) {
          f_out[i] = b[i][0];
      }
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