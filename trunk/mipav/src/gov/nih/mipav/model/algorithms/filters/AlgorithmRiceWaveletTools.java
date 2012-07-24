package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;

import java.io.IOException;
import java.util.Arrays;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

public class AlgorithmRiceWaveletTools extends AlgorithmBase {
    
    public static final int MINIMUM_PHASE = 1;
    
    public static final int MID_PHASE = 2;
    
    public static final int MAXIMUM_PHASE = 3;
    
    private int filterLength;
    
    private int filterType;
    
    private double[] scalingFilter;
    
    private double[] waveletFilter;
    
    private int nDims;
    
    private int extents[];
    
    private int xDim;
    
    private int yDim;
    
    private int zDim;
    
    private int arrayLength;
    
    private double aArray[];
    
    public AlgorithmRiceWaveletTools(ModelImage srcImg, int filterLength, int filterType) {
        super(null, srcImg);
        this.filterLength = filterLength;
        this.filterType = filterType;
    }
    
    
    public void runAlgorithm() {
        
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        if ((filterLength % 2) == 1) {
            displayError("No Daubechies filter exists for odd length");
            
            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Importing source image...");
        
        nDims = srcImage.getNDims();
        extents = srcImage.getExtents();
        xDim = extents[0];
        yDim = extents[1];

        if (nDims > 2) {
            zDim = extents[2];
        }

        arrayLength = 1;

        for (int i = 0; i < nDims; i++) {
            arrayLength *= extents[i];
        }

        try {
            aArray = new double[arrayLength];
        } catch (final OutOfMemoryError e) {
            aArray = null;
            System.gc();
            displayError("AlgorithmRiceWaveletTools: Out of memory creating a");

            setCompleted(false);

            return;
        }

        try {
            srcImage.exportData(0, arrayLength, aArray);
        } catch (final IOException error) {
            displayError("AlgorithmRiceWaveletTools: Source image is locked");

            setCompleted(false);

            return;
        }
        
        scalingFilter = new double[filterLength];
        
        waveletFilter = new double[filterLength];
        
        daubcqf();
        
    }
    
    private void daubcqf() {
        int j, m;
        int k = filterLength/2;
        double a = 1.0;
        double p[] = new double[]{1.0};
        double q[] = new double[]{1.0};
        double h_0[] = new double[]{1.0, 1.0};
        double oldp[];
        double oldq[];
        double oldh_0[];
        double A[][];
        double[] eigenvalue;
        double[][] V;
        double[] e1;
        double[] qt;
        
        
        for (j = 1; j <= k-1; j++) {
            a = -a * 0.25 * (j + k - 1.0)/(double)j;
            oldh_0 = new double[h_0.length];
            for (m = 0; m < h_0.length; m++) {
                oldh_0[m] = h_0[m];
            }
            h_0 = new double[h_0.length + 1];
            for (m = 0; m < h_0.length - 1; m++) {
                h_0[m+1] += oldh_0[m];
                h_0[m] += oldh_0[m];
            }
            oldp = new double[p.length];
            for (m = 0; m < p.length; m++) {
                oldp[m] = p[m];
            }
            p = new double[p.length+1];
            for (m = 0; m < p.length-1; m++) {
                p[m+1] += -oldp[m];
                p[m] += oldp[m];
            }
            oldp = new double[p.length];
            for (m = 0; m < p.length; m++) {
                oldp[m] = p[m];
            }
            p = new double[p.length+1];
            for (m = 0; m < p.length-1; m++) {
                p[m+1] += -oldp[m];
                p[m] += oldp[m];
            }
            oldq = new double[q.length];
            for (m = 0; m < q.length; m++) {
                oldq[m] = q[m];
            }
            q = new double[q.length+2];
            for (m = 0; m < q.length-2; m++) {
                q[m+1] = oldq[m];    
            }
            for (m = 0; m < p.length; m++) {
                q[m] += a*p[m];
            }
        } // for (j = 1; j <= k-1; j++) 
        
        A = new double[q.length-1][q.length-1];
        for (m = 1; m < q.length-1; m++) {
            A[m][m-1] = 1.0;
        }
        for (m = 0; m < q.length -1; m++) {
            A[0][m] = -q[m+1]/q[0];
        }
        eigenvalue = new double[q.length-1];
        V = new double[q.length-1][q.length-1];
        e1 = new double[q.length-1];
        // A = V * (diagonal eigenvalues) * V'
        // In EigevalueDecomposition the columns of V represent the eigenvectors
        // Whitening matrix = v * 1/sqrt(diagonal eigenvalues) * V'
        Eigenvalue.decompose( A, V, eigenvalue, e1 );
        // Sort into ascending order
        Arrays.sort(eigenvalue);
        qt = new double[k-1];
        for (m = 0; m < k-1; m++) {
            qt[m] = eigenvalue[m];
        }
    } // daubcqf()
    
}