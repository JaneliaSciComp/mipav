package gov.nih.mipav.model.algorithms;

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import Jama.Matrix;
import gov.nih.mipav.model.algorithms.filters.AlgorithmHilbertTransform;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.jama.SparseEigenvalue;
import gov.nih.mipav.model.structures.jama.LUSOL;
import gov.nih.mipav.util.MipavMath;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * Compute globalPb, the globalized probability of boundary of an image
 * Reference: Contour Detection and Hierarchical Image Segmentation by Pablo Arbelaez, Michael Maire,
 * Charless Fowlkes, and Jitendra Malik, IEEE Transactions on Pattern Analysis and Machine Intelligence,
 * Volume 33, Issue 5, August, 2010, pp. 898-916.
 * 
 * BSR package code written in MATLAB and C++ is ported to Java with the permission of 
 * Pablo Andres Arbelaez Escalante.
 * 
 * With 12 gigabytes allocated to Eclipse, a 256 by 256 image gives an Out of Memory Java heap space error in
 * spectralPb at P = new double[mL][nL] after lu.lu1fac
 * With a  128 by 128 image dsaupd -> dsaup2 -> second dsaitr call: At
 * ge.dgemv('N', n, dsaitr_j, -one, V, ldv, buffer2, 1, one, resid, 1);
 * very large finite numbers generate infinities in resid.
 * Scaling intensity values to be all less than one did not prevent this.
 */

public class AlgorithmGlobalPb extends AlgorithmBase {
	
	private ViewUserInterface UI = ViewUserInterface.getReference();
	
	DecimalFormat nf = new DecimalFormat("0.00000E0");
	
	// If not null or empty, automatic file saves occur with an outFile String base.
	private String outFile;
	
	// Resizing factor in (0, 1], to speed up eigenvector
	private double rsz = 1.0;
	
	private int xDim;
	
	private int yDim;
	
	private int sliceSize;
	
	// Maximum fraction of items in k-means clusterer
	private double maxFraction = 1.0;
	
	// Minimum number of items for frac sample
	private int minSample = 1;
	
	// Maximum # items (0 for unlimited)
	private int maxItems = 0;
	
	// Desired number of clusters
	private int kmeansK = 64;
	
	private final int L2_metric = 1;
	
	private int metric = L2_metric;
	
	private int max_iterations;
	
	private ModelImage gPbOrientImage;
	
	private ModelImage gPbThinImage;
	
	private ModelImage textonImage;
	
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmGlobalPb - default constructor.
     */
    public AlgorithmGlobalPb() { }
    
    /**
     * 
     * @param destImg
     * @param srcImg
     */
    public AlgorithmGlobalPb(ModelImage gPbOrientImage, ModelImage gPbThinImage, ModelImage textonImage, 
    		ModelImage srcImg, String outFile, double rsz) {
    	super(null, srcImg);
    	this.gPbOrientImage = gPbOrientImage;
    	this.gPbThinImage = gPbThinImage;
    	this.textonImage = textonImage;
    	this.outFile = outFile;
    	this.rsz = rsz;
    }
    
    /**
     * Original globalPb.m by Pablo Arbelaez on December, 2010
     */
    public void runAlgorithm() {
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        ModelImage inputImage;
      	AlgorithmChangeType changeTypeAlgo;
      	FileInfoBase[] fileInfo;
      	double weights[] = new double[13];
      	int i;
      	double l1, l2, l3, a1, a2, a3, b1, b2, b3, t1, t2, t3, sc;
      	int x, y, z;
      	int pruningPix;
      	boolean entireImage;
      	AlgorithmMorphology2D algoMorph2D;
      	ModelImage gPbThinByteImage;
      	boolean err[] = new boolean[1];
      	double array[];
      	int extents[];
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        if ((rsz <= 0.0) || (rsz > 1.0)) {
        	MipavUtil.displayError("Resizing factor rsz out of range (0, 1]");
        	setCompleted(false);
        	return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Global Pb ...");
        
        if (srcImage.isColorImage()) {
        	inputImage = new ModelImage(ModelStorageBase.ARGB_FLOAT,
      				srcImage.getExtents(), "changeTypeImage");
        }
        else {
        	inputImage = new ModelImage(ModelStorageBase.DOUBLE,
      				srcImage.getExtents(), "changeTypeImage");
        }
  		inputImage.getFileInfo(0).setEndianess(FileBase.LITTLE_ENDIAN);
  		changeTypeAlgo = new AlgorithmChangeType(inputImage,
				srcImage, srcImage.getMin(),
				srcImage.getMax(), 0.0, 1.0, image25D);
  		changeTypeAlgo.run();
  		changeTypeAlgo.finalize();
  		changeTypeAlgo = null;
  		fileInfo = inputImage.getFileInfo();
        fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(inputImage.getExtents());
        fileInfo[0].setMax(inputImage.getMax());
        fileInfo[0].setMin(inputImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        
        // Default feature weights
        if (srcImage.isColorImage()) {
        	weights[0] = 0.0;
        	weights[1] = 0.0;
        	weights[2] = 0.0039;
        	weights[3] = 0.0050;
        	weights[4] = 0.0058;
        	weights[5] = 0.0069;
        	weights[6] = 0.0040;
        	weights[7] = 0.0044;
        	weights[8] = 0.0049;
        	weights[9] = 0.0024;
        	weights[10] = 0.0027;
        	weights[11] = 0.0170;
        	weights[12] = 0.0074;
        }
        else {
        	weights[0] = 0.0;
        	weights[1] = 0.0;
        	weights[2] = 0.0054;
        	weights[3] = 0.0;
        	weights[4] = 0.0;
        	weights[5] = 0.0;
        	weights[6] = 0.0;
        	weights[7] = 0.0;
        	weights[8] = 0.0;
        	weights[9] = 0.0048;
        	weights[10] = 0.0049;
        	weights[11] = 0.0264;
        	weights[12] = 0.0090;
        }
        
        // mPb
        int textons[] = new int[xDim * yDim];
        double bg1[][][] = new double[8][yDim][xDim];
        double bg2[][][] = new double[8][yDim][xDim];
        double bg3[][][] = new double[8][yDim][xDim];
        double cga1[][][] = new double[8][yDim][xDim];
        double cga2[][][] = new double[8][yDim][xDim];
        double cga3[][][] = new double[8][yDim][xDim];
        double cgb1[][][] = new double[8][yDim][xDim];
        double cgb2[][][] = new double[8][yDim][xDim];
        double cgb3[][][] = new double[8][yDim][xDim];
        double tg1[][][] = new double[8][yDim][xDim];
        double tg2[][][] = new double[8][yDim][xDim];
        double tg3[][][] = new double[8][yDim][xDim];
        double mPb[][] = new double[yDim][xDim];
        double mPb_rsz[][];
        if (rsz < 1) {
        	mPb_rsz = new double[(int)Math.floor(rsz * yDim)][(int)Math.floor(rsz * xDim)];
        }
        else {
        	mPb_rsz = new double[yDim][xDim];
        }
        multiscalePb(mPb, mPb_rsz, bg1, bg2, bg3, cga1, cga2, cga3, cgb1, cgb2, cgb3, tg1, tg2, tg3, textons, inputImage, err);
        if (err[0]) {
        	setCompleted(false);
        	return;
        }
        
        String outFile2 = null;
        if ((outFile != null) && (outFile.length() > 0)) {
        	outFile2 = outFile + "_spb.xml";	
        }
        int nvec = 17;
        double sPb[][][] = new double[8][yDim][xDim];
        spectralPb(sPb, mPb_rsz, srcImage.getExtents(), outFile2, nvec, err);
        if (err[0]) {
        	setCompleted(false);
        	return;
        }
        double gPb_orient[][][] = new double[8][yDim][xDim];
        double gPb[][] = new double[yDim][xDim];
        double gPb_thin[] = new double[sliceSize];
        byte gPb_thin_byte[] = new byte[sliceSize];
    	for (y = 0; y < yDim; y++) {
    	    for (x = 0; x < xDim; x++) {
    	    	gPb[y][x] = -Double.MAX_VALUE;
    	    	for (i = 0; i < 8; i++) {
        	    	l1 = weights[0] * bg1[i][y][x];
        	    	l2 = weights[1] * bg2[i][y][x];
        	    	l3 = weights[2] * bg3[i][y][x];
        	    	
        	    	a1 = weights[3] * cga1[i][y][x];
        	    	a2 = weights[4] * cga2[i][y][x];
        	    	a3 = weights[5] * cga3[i][y][x];
        	    	
        	    	b1 = weights[6] * cgb1[i][y][x];
        	    	b2 = weights[7] * cgb2[i][y][x];
        	    	b3 = weights[8] * cgb3[i][y][x];
        	    	
        	    	t1 = weights[9] * tg1[i][y][x];
        	    	t2 = weights[10] * tg2[i][y][x];
        	    	t3 = weights[11] * tg3[i][y][x];
        	    	
        	    	sc = weights[12] * sPb[i][y][x];
        	    	gPb_orient[i][y][x] = l1 + a1 + b1 + t1 + l2 + a2 + b2 + t2 +
        	    			              l3 + a3 + b3 + t3 + sc;
        	    	if (gPb_orient[i][y][x] > gPb[y][x]) {
        	    		gPb[y][x] = gPb_orient[i][y][x];
        	    	}
        	    } // for (i = 0; i < 8; i++)
    	    	if (mPb[y][x] > 0.05) {
    	    		gPb_thin[x + y * xDim] = gPb[y][x];
    	    		gPb_thin_byte[x + y * xDim] = 1;
    	    	}
    	    	else {
    	    		gPb_thin[x + y * xDim] = 0.0;
    	    		gPb_thin_byte[x + y * xDim] = 0;
    	    	}
        	}
        } // for (y = 0; y < yDim; y++)
    	pruningPix = 0;
        entireImage = true;
        gPbThinByteImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), "gPbThinByte");
        try {
        	gPbThinByteImage.importData(0, gPb_thin_byte, true);
        }
        catch (IOException e) {
        	gPbThinByteImage.disposeLocal();
        	gPbThinByteImage = null;
        	MipavUtil.displayError("IOException " + e + " on gPbThinNyteImage.importData(0, gPb_thin_byte, true)");
        	setCompleted(false);
        	return;
        }
        algoMorph2D = new AlgorithmMorphology2D(gPbThinByteImage, 0, 0.0f, AlgorithmMorphology2D.SKELETONIZE, 0, 0,
        		pruningPix, 0, entireImage);
        algoMorph2D.run();
        algoMorph2D.finalize();
        
        try {
            gPbThinByteImage.exportData(0, sliceSize, gPb_thin_byte);
        }
        catch (IOException e) {
        	gPbThinByteImage.disposeLocal();
        	gPbThinImage = null;
        	MipavUtil.displayError("IOException " + e + " on gPbThinByteImage.exportData(0, sliceSize, gPb_thin_byte)");
        	setCompleted(false);
        	return;	
        }
        
        gPbThinByteImage.disposeLocal();
        gPbThinByteImage = null;
        for (i = 0; i < sliceSize; i++) {
        	if (gPb_thin_byte[i] == 0) {
        		gPb_thin[i] = 0.0;
        	}
        }
        
        //gPbThinImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), "gPbThin");
    	try {
        	gPbThinImage.importData(0, gPb_thin, true);
        }
        catch (IOException e) {
        	gPbThinImage.disposeLocal();
        	gPbThinImage = null;
        	MipavUtil.displayError("IOException " + e + " on gPbThinImage.importData(0, gPb_thin, true)");
        	setCompleted(false);
        	return;
        }
    	
    	array = new double[8 * sliceSize];
        for (z = 0; z < 8; z++) {
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		array[x + y * xDim + z * sliceSize] = gPb_orient[z][y][x];
            	}
            }
        }
    	
    	extents = new int[3];
        extents[0] = xDim;
        extents[1] = yDim;
        extents[2] = 8;
        
        //gPbOrientImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "gPbOrient");
        try {
        	gPbOrientImage.importData(0, array, true);
        }
        catch (IOException e) {
        	gPbOrientImage.disposeLocal();
        	gPbOrientImage = null;
        	MipavUtil.displayError("IOException " + e + " on gPbOrientImage.importData(0, array, true)");
        	setCompleted(false);
        	return;	
        }
        
        //textonImage = new ModelImage(ModelStorageBase.INTEGER, srcImage.getExtents(), "texton");
        try {
        	textonImage.importData(0, textons, true);
        }
        catch (IOException e) {
        	textonImage.disposeLocal();
        	textonImage = null;
        	MipavUtil.displayError("IOException " + e + " on textonImage.importData(0, textons, true)");
        	setCompleted(false);
        	return;	
        }
        
        if ((outFile != null) && (outFile.length() > 0)) {
        	
        	
        	try {
                gPbThinImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), outFile + "_gPb_thin.xml",
                                   FileUtility.XML, true);
            } catch (OutOfMemoryError error) {

                if (gPbThinImage != null) {
                    gPbThinImage.disposeLocal();
                }

                gPbThinImage = null;
                MipavUtil.displayError("Error on gPbThinImage.saveImage");

                setCompleted(false);

                return;
            }
            
            try {
                gPbOrientImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), outFile + "_gPb_orient.xml",
                                   FileUtility.XML, true);
            } catch (OutOfMemoryError error) {

                if (gPbOrientImage != null) {
                    gPbOrientImage.disposeLocal();
                }

                gPbOrientImage = null;
                MipavUtil.displayError("Error on gPbOrientImage.saveImage");

                setCompleted(false);

                return;
            }
            
            try {
                textonImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), outFile + "_texton.xml",
                                   FileUtility.XML, true);
            } catch (OutOfMemoryError error) {

                if (textonImage != null) {
                    textonImage.disposeLocal();
                }

                textonImage = null;
                MipavUtil.displayError("Error on textonImage.saveImage");

                setCompleted(false);

                return;
            }
        	
        } // if ((outFile != null) && (outFile.length() > 0))
     
        setCompleted(true);
        return;
    }
    
    /**
     * 
     * Description: Global contour cue from local mPb.
     * Computes Intervening Contour with BSE code by Charless Fowlkes:
     * http://www.cs.berkeley.edu/~fowlkes/BSE/
     * Pablo Arbelaez <arbelaez@eecs.berkeley.edu>
     * December 2010
     * @param sPb
     * @param mPb
     * @param orig_sz
     * @param outFile
     * @param nvec
     * @param error
     */
    private void spectralPb(double sPb[][][], double mPb[][], int orig_sz[], String outFile, int nvec, boolean err[]) {
    	int x;
    	int y;
    	int i;
    	int j;
    	int k;
    	boolean found;
    	int n;
    	int nev;
    	int ncv;
    	int wx;
    	//int wy;
    	int xvec[];
    	String bmat;
    	String which;
    	int lworkl;
    	double tol[] = new double[1];
    	int ido[] = new int[1];
        int info[] = new int[1];
        int ishfts;
        int maxitr;
        int mode;
        int iparam[] = new int[11];
        SparseEigenvalue se = new SparseEigenvalue();
        double resid[];
        int ldv;
        double v[][];
        int ipntr[];
        double workd[];
        double workd2[][];
        double workl[];
        boolean rvec;
        boolean select[];
        double ds[];
        double sigma;
        int ierr[];
        int index;
        double d[][];
        int nconv;
        double EigVal[];
        double EigVect[][];
        int ss;
        int ty = mPb.length;
        int tx = mPb[0].length;
        double l[][][] = new double[2][][];
        int lena;
        int nelem;
        double a[];
        int indr[];
        int indc[];
        int mL;
        int nL;
        int luparm[];
        double epsilon;
        double neweps;
        double parmlu[];
        double factol;
        int p[];
        int q[];
        int lenc[];
        int lenr[];
        int locc[];
        int locr[];
        int iploc[];
        int iqloc[];
        int ipinv[];
        int iqinv[];
        double w[];
        int inform[];
        LUSOL lu;
        double P[][];
        double PW[];
        double wout[];
        int tyo;
        int txo;
        double vect[][][];
        int vv;
        double evTranspose[][];
        double evScale[][];
        double minVect;
        double maxVect;
        double range;
        boolean dohil;
        int deriv;
        int support;
        int norient;
        double dtheta;
        int ch_per[];
        double vec[][];
        double theta;
        int hsz;
        int sz;
        double f[][];
        double fim[][];
        double array[];
        int extents[];
        int slsize;
        int z;
        ModelImage spectralImage;
        
        l[0] = new double[ty+1][tx];
        for (y = 0; y < ty; y++) {
        	for (x = 0; x < tx; x++) {
        		l[0][y+1][x] = mPb[y][x];
        	}
        }
        l[1] = new double[ty][tx+1];
        for (y = 0; y < ty; y++) {
        	for (x = 0; x < tx; x++) {
        		l[1][y][x+1] = mPb[y][x];
        	}
        }
        
        // Build the pairwise affinity matrix
        SMatrix W = buildW(l[0], l[1], err);
        if (err[0]) {
        	System.err.println("spectralPb call to buildW falied");
        	return;
        }
        // From end of buildW
        // Pack sparse matrix
        // Compute total number of nonzero entries
        int nnz = 0;
        for (i = 0; i < W.n; i++) {
        	nnz += W.nz[i];
        }
        // col index
        int J[] = new int[nnz];
        // row index
        int I[] = new int[nnz];
        // Values
        double val[] = new double[nnz];
        int ct = 0;
        // This matrix is already sparse
        for (int row = 0; row < W.n; row++) {
        	for (i = 0; i < W.nz[row]; i++) {
        		I[ct+i] = row;
        		J[ct+i] = W.col[row][i];
        		val[ct+i] = W.values[row][i];
        	}
        	ct = ct + W.nz[row];
        }
        W.dispose();
        W = null;
        // Finished end of buildW
        int numElements = val.length;
       
        int imax = -1;
        int jmax = -1;
        for (i = 0, j = 0; i < numElements; i++) {
            I[j] = I[i];
            if (I[j] > imax) {
            	imax = I[j];
            }
            J[j] = J[i];
            if (J[j] > jmax) {
            	jmax = J[j];
            }
            val[j++] = val[i];
        }
        // [J, I, val] = buildW(l[0],l[1]);
        //Wsp = sparse(J, I, val);
        //buildW calls computeAffinites2 which calls symmetrize so should have wx = xy
        wx = jmax + 1;
        //wy = imax + 1;
        xvec = new int[wx];
        for (i = 1; i <= wx; i++) {
        	xvec[i-1] = i;
        }
        // For each Isp add up the values
        // S is the sum of the columns of the full matrix W
        // but there are wx columns and xvec has length wx.
        // D = sparse(xvec, xvec, S, wx, wx) requires xvec and S
        // to be the same length so we had better have length(S) = wx.
        // All nonzero values of S are placed along the diagonal of 
        // the sparse matrix D.
        // To do D - W, D must be the same size as W so D must be Wx by Wx
        double S[] = new double[wx];
        for (i = 0; i < numElements; i++) {
        	S[J[i]] += val[i];
        }
        int numIgnored = 0;
        boolean ignore[] = new boolean[wx];
        for (i = 0; i < wx; i++) {
        	if (S[i] == 0) {
        	    ignore[i] = true;
        	    numIgnored++;
        	}
        }
        int sparseElementsD = wx - numIgnored;
        int IJDsp[] = new int[sparseElementsD];
        double valspD[] = new double[sparseElementsD];
        for (i = 0, j = 0; i < wx; i++) {
        	if (!ignore[i]) {
        		IJDsp[j] = i;
        		valspD[j++] = S[i];
        	}
        }
        // Find the sparse D - W
        int sparseElementsDmW = numElements + sparseElementsD;
        for (i = 0; i < numElements; i++) {
        	if (I[i] == J[i]) {
        		found = false;
        		for (j = 0; j < sparseElementsD && (!found); j++) {
        			if (I[i] == IJDsp[j]) {
        				found = true;
        				sparseElementsDmW--;
        				if ((valspD[j] - val[i]) == 0.0) {
        					sparseElementsDmW--;
        				}
        			}
        		}
        	} // if (Isp[i] == Jsp[i])
        } // for (i = 0; i < sparseElements; i++)
        int IDmWsp[] = new int[sparseElementsDmW];
        int JDmWsp[] = new int[sparseElementsDmW];
        double valspDmW[] = new double[sparseElementsDmW];
        ignore = new boolean[sparseElementsD];
        for (i = 0, k = 0; i < numElements; i++) {
        	if (I[i] == J[i]) {
        		found = false;
        		for (j = 0; j < sparseElementsD && (!found); j++) {
        			if (I[i] == IJDsp[j]) {
        				found = true;
        				ignore[j] = true;
        				if ((valspD[j] - val[i]) != 0) {
        				    IDmWsp[k] = I[i];
        				    JDmWsp[k] = J[i];
        				    valspDmW[k++] = valspD[j] - val[i];
        				}
        			}
        		} // for (j = 0; j < sparseElementsD && (!found); j++)
        		if (!found) {
        			IDmWsp[k] = I[i];
    				JDmWsp[k] = J[i];
    				valspDmW[k++] = -val[i];	
        		}
        	} // if (Isp[i] == Jsp[i])
        	else {
        		IDmWsp[k] = I[i];
				JDmWsp[k] = J[i];
				valspDmW[k++] = -val[i];		
        	}
        } // for (i = 0, k = 0; i < sparseElements; i++)
        
        for (i = 0; i < sparseElementsD; i++) {
        	if (!ignore[i]) {
        		IDmWsp[k] = IJDsp[i];
        		JDmWsp[k] = IJDsp[i];
        		valspDmW[k++] = valspD[i];
        	}
        }
        
        // LU factor A - sigma B
        // LU factor D - W
        // Do L * U = P * (D-W) * Q, where P is row permutation and Q is column permutation
        // L, U, P, and Q have the same dimensions as (D-W).
        // dgetrf does not handle sparse matrices
        // Use lu1fac in LUSOL
        // Go from 0 based row and column indices to 1 based row and column indices
        nelem = sparseElementsDmW;
        // Number of rows
        mL = 0;
        // Number of columns
        nL = 0;
        for (k = 0; k < sparseElementsDmW; k++) {
        	if ((IDmWsp[k]+1) > mL) {
        		mL = IDmWsp[k]+1;
        	}
        	if ((JDmWsp[k]+1) > nL) {
        		nL = JDmWsp[k] + 1;
        	}
        }
        // lena = Math.max(2*nelem, Math.max(10*mL, 10*nL));
        lena = 10*Math.max(2*nelem, Math.max(10*mL, 10*nL));
        a = new double[lena];
        // The row indices i must be in indc and
        // the column indices j must be in indr.
        indc = new int[lena];
        indr = new int[lena];
        for (k = 0; k < nelem; k++) {
        	a[k] = valspDmW[k];
        	indr[k] = JDmWsp[k]+1;
        	indc[k] = IDmWsp[k]+1;
        }
        luparm = new int[30];
        // lprint   Print level.               
	    //                   <  0 suppresses output.
	    //                   =  0 gives error messages.
	    //                  >= 10 gives statistics about the LU factors.
	    //                  >= 50 gives debug output from lu1fac
	    //                        (the pivot row and column and the
	    //                        no. of rows and columns involved at
	    //                        each elimination step).
        luparm[1] = 0;
        // maxcol   lu1fac: maximum number of columns
	    //                        searched allowed in a Markowitz-type
	    //                        search for the next pivot element.
	    //                        For some of the factorization, the
	    //                        number of rows searched is
	    //                        maxrow = maxcol - 1.
        luparm[2] = 5;
        luparm[5] = 0;       // Threshold Pivoting: 0 = TPP, 1 = TRP, 2 = TCP
        // keepLU   lu1fac: keepLU = 1 means the numerical
        	    //                        factors will be computed if possible.
        	    //                        keepLU = 0 means L and U will be discarded
        	    //                        but other information such as the row and
        	    //                        column permutations will be returned.
        	    //                        The latter option requires less storage.
        luparm[7] = 1;
        parmlu = new double[30];
        factol = 2.0;    // > 1.0
        parmlu[0] = factol;     // Ltol1:  max |Lij| during Factor
	  	parmlu[1] = factol;     // Ltol2:  max |Lij| during Update 
	    parmlu[2] = 3.0e-13;    // small:  drop tolerance
	  	parmlu[3] = 3.7e-11;    // Utol1:  absolute tol for small Uii
	  	parmlu[4] = 3.7e-11;    // Utol2:  relative tol for small Uii
	  	parmlu[5] = 3.0;        // Uspace: 
	  	parmlu[6] = 0.3;        // dens1
	  	parmlu[7] = 0.5;        // dens2
	  	p = new int[mL];
	  	q = new int[nL];
	  	lenc = new int[nL];
	  	lenr = new int[mL];
	  	locc = new int[nL];
	  	locr = new int[mL];
	  	iploc = new int[nL];
	  	iqloc = new int[mL];
	  	ipinv = new int[mL];
	  	iqinv = new int[nL];
	  	w = new double[nL];
	  	inform = new int[1];
	  	lu = new LUSOL();
	  	
	  	lu.lu1fac( mL    , nL    , nelem, lena , luparm, parmlu,
	               a    , indc , indr , p    , q     , 
	               lenc , lenr , locc , locr ,           
	               iploc, iqloc, ipinv, iqinv, w     , inform );
	  	
	  	if (inform[0] > 1) {
       	 UI.setDataText("Error in lu1fac inform[0] = " + inform[0] + "\n");
       	 err[0] = true;
       	 return;
        }
	  	P = new double[mL][nL];
	  	for (i = 0; i < mL; i++) {
	  		P[i][p[i]-1] = 1.0;
        }
        
        // Use DSDRV4 for Generalized Eigenvalues
        // Shift and invert mode
        // A*x = lamda*M*x
        // Op = inv(A - sigma*M)*M
        // B = M
        
        // Dimension of matrix
        n = wx;
        // Number of eigenvalues to be approximated
        nev = nvec;
        // The length of the Lanczos factorization. This represents the
        // maximum number of Lanczos vectors used.
        // ncv >= nev + 1
        // As a rule of thumb, ncv = 2 * nev is reasonable.
        ncv = Math.min(Math.max(2 * nev, 20),n);
        // Problem is generalized eigenvalue
        bmat = "G";
        // Calculate smallest magnitude eigenvalues
        // This is done by using "LM" in mode 3
        which = "LM";
        // The work array workl is used in dsaupd as workspace.  
        // Its dimension lworkl is set as illustrated below.
        lworkl = 100*ncv*(ncv+8);
        //lworkl = ncv*(ncv+8);
        // The parameter tol determines the stopping criterion.  
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)
        tol[0] = epsilon;
        // The variable ido is used for reverse communication and is initially set to 0. 
        ido[0] = 0;
        // Setting info[0] =0 indicates that a random vector is
        // generated in dsaupd to start the Arnoldi iteration.                                 
        info[0] = 0;
        
        // This program uses exact shifts with respect to
        // the current Hessenberg matrix (iparam[0] = 1).
        ishfts = 1;
        // iparam[2] specifies the maximum number of Arnoldi iterations allowed.
        maxitr = Math.max(300, (int)Math.ceil(2.0*n/Math.max(ncv,1)));
        // Mode 3 of dsaupd is used (iparam[6] = 3).
        mode   = 3;
  
        iparam[0] = ishfts;
        iparam[2] = maxitr; 
        iparam[6] = mode;
        resid = new double[n];
        ldv = n;
        v = new double[ldv][ncv];
        ipntr = new int[11];
        workd = new double[3*n];
        workd2 = new double[n][3];
        workl = new double[lworkl];
        select = new boolean[ncv];
        ds = new double[2 * ncv];
        sigma = 0.0;
        ierr = new int[1];
        d = new double[ncv][2];
        nconv = 0;
        
        EigVal = new double[nvec];
        EigVect = new double[n][nvec];
        PW = new double[n];
        wout = new double[n];
        
        
        //     %-------------------------------------------%
        //     | M A I N   L O O P (Reverse communication) |
        //     %-------------------------------------------%
        
        while (true) {
        
        //        %---------------------------------------------%
        //        | Repeatedly call the routine dsaupd and take | 
        //        | actions indicated by parameter ido until    |
        //        | either convergence is indicated or maxitr   |
        //        | has been exceeded.                          |
        //        %---------------------------------------------%
                 
                 se.dsaupd ( ido, bmat, n, which, nev, tol, resid, 
                          ncv, v, ldv, iparam, ipntr, workd, workl,
                          lworkl, info );
                 index = 0;
            	 for (j = 0; j < 3; j++) {
            		 for (i = 0; i < n; i++) {
            		     workd2[i][j] = workd[index++];	 
            		 }
            	 }
                 
                
                 
                 if (ido[0] == -1) {
                 
                 //           %--------------------------------------------%
                 //           | Perform  y <--- OP*x = inv[A-SIGMA*M]*M*x  |
                 //           | to force the starting vector into the      |
                 //           | range of OP.  The user should supply       |
                 //           | his/her own matrix vector multiplication   |
                 //           | routine and a linear system solver here.   |
                 //           | The matrix vector multiplication routine   |
                 //           | takes workd(ipntr(1)) as the input vector. |
                 //           | The final result is returned to            |
                 //           | workd(ipntr(2)).                           |
                 //          %--------------------------------------------%
                 //
                	 
                	 for (i = 0; i < n; i++) {
                		 workd2[i][1] = 0;
                	 }
                	 for (k = 0; k < sparseElementsD; k++) {
                		 i = IJDsp[k];
                		 workd2[i][1] = valspD[k]*workd2[i][0];
                	 } 
                     // workd2(Q,1) = U \ (L \ (P * workd2[i][1]));
                	 // X = A \ B is the solution to A*X = B computed by Gaussian elimination.
                	 // lu6L   solves   L v = v(input).
                	 // lu6U   solves   U w = v.          v  is not altered.
                	 for (i = 0; i < n; i++) {
                		 PW[i] = 0.0;
                		 for (j = 0; j < n; j++) {
                		     PW[i] += P[i][j] * workd2[j][1];
                		 }
                	 }
                	 // lu6L sets inform to 0 on entry.
                	 lu.lu6L(inform, mL, nL, PW, lena, luparm, parmlu, a, indc, indr, lenc);
                	 // lu6U sets inform to 0 on entry.
                	 // lu6U sets inform to 1 in overdetermined systems with a residual > 0.
                	 // The lu6U residual is put in parmlu[19].
                	 lu.lu6U(inform, mL, nL, PW, wout, lena, luparm, parmlu, a, indr, p, q, lenr, locr);
                	 for (i = 0; i < n; i++) {
                		 workd2[q[i]-1][1] = wout[i];
                	 }
                	 index = 0;
                	 for (j = 0; j < 3; j++) {
                		 for (i = 0; i < n; i++) {
                		     workd[index++] = workd2[i][j];	 
                		 }
                	 }

                 }
                 else if (ido[0] == 1) {
                 
                 //           %-----------------------------------------%
                 //           | Perform y <-- OP*x = inv[A-sigma*M]*M*x |
                 //           | M*x has been saved in workd(ipntr(3)).  |
                 //           | the user only needs the linear system   |
                 //           | solver here that takes workd(ipntr(3)   |
                 //           | as input, and returns the result to     |
                 //           | workd(ipntr(2)).                        | 
                 //           %-----------------------------------------%
                 //
                     //workd2(Q,1) = U \ (L \ (P * workd2[i][2]));
                	// X = A \ B is the solution to A*X = B computed by Gaussian elimination.
                	 // lu6L   solves   L v = v(input).
                	 // lu6U   solves   U w = v.          v  is not altered.
                	 for (i = 0; i < n; i++) {
                		 PW[i] = 0.0;
                		 for (j = 0; j < n; j++) {
                		     PW[i] += P[i][j] * workd2[j][2];
                		 }
                	 }
                	 // lu6L sets inform to 0 on entry.
                	 lu.lu6L(inform, mL, nL, PW, lena, luparm, parmlu, a, indc, indr, lenc);
                	 // lu6U sets inform to 0 on entry.
                	 // lu6U sets inform to 1 in overdetermined systems with a residual > 0.
                	 // The lu6U residual is put in parmlu[19].
                	 lu.lu6U(inform, mL, nL, PW, wout, lena, luparm, parmlu, a, indr, p, q, lenr, locr);
                	 for (i = 0; i < n; i++) {
                		 workd2[q[i]-1][1] = wout[i];
                	 }
                	 index = 0;
                	 for (j = 0; j < 3; j++) {
                		 for (i = 0; i < n; i++) {
                		     workd[index++] = workd2[i][j];	 
                		 }
                	 }
                 }
                 else if (ido[0] == 2) {
                 
                 //           %-----------------------------------------%
                 //           |          Perform  y <--- M*x            |
                 //           | Need the matrix vector multiplication   |
                 //           | routine here that takes workd(ipntr(1)) |
                 //           | as the input and returns the result to  |
                 //           | workd(ipntr(2)).                        |
                 //           %-----------------------------------------%
                	 for (i = 0; i < n; i++) {
                		 workd2[i][1] = 0;
                	 }
                	 for (k = 0; k < sparseElementsD; k++) {
                		 i = IJDsp[k];
                		 workd2[i][1] = valspD[k]*workd2[i][0];
                	 }
                	 index = 0;
                	 for (j = 0; j < 3; j++) {
                		 for (i = 0; i < n; i++) {
                		     workd[index++] = workd2[i][j];	 
                		 }
                	 }
                 } // else if (ido[0] == 2)

                 else {
                	 break;
                 }
        } // while (true)
        
        
        
        //     %-----------------------------------------%
        //     | Either we have convergence, or there is |
        //     | an error.                               |
        //     %-----------------------------------------%
        
              if ( info[0] < 0 ) {
        
        //        %--------------------------%
        //        | Error message, check the |
        //        | documentation in DSAUPD  |
        //        %--------------------------%
        
                 UI.setDataText("Error in spectralPb with dsaupd info[0] = " + info[0] + "\n");
                 err[0] = true;
                 return;
              } // if (info[0] < 0)
              else  { // info[0] >= 0
        
        //        %-------------------------------------------%
        //        | No fatal errors occurred.                 |
        //        | Post-Process using DSEUPD.                |
        //        |                                           |
        //        | Computed eigenvalues may be extracted.    |  
        //        |                                           |
        //        | Eigenvectors may also be computed now if  |
        //        | desired.  (indicated by rvec = .true.)    | 
        //        %-------------------------------------------%
                   
                 rvec = true;
        
                 se.dseupd ( rvec, "A", select, ds, v, ldv, sigma, 
                     bmat, n, which, nev, tol[0], resid, ncv, v, ldv, 
                     iparam, ipntr, workd, workl, lworkl, ierr );
                 // eigenvalue and eigenvector reversals done here in eigs
                 // and after eigs just leave out both reversals
                 
                 index = 0;
                 for (j = 0; j < 2; j++) {
                	 for (i = 0; i < ncv; i++) {
                	     d[i][j] = ds[index++]; 
                	 }
                 }
                 for (i = 0; i < nvec; i++) {
                	 EigVal[i] = d[i][0];
                 }
                 for (i = 0; i < n; i++) {
                	 for (j = 0; j < nvec; j++) {
                		 EigVect[i][j] = v[i][j];
                	 }
                 }
        
        //        %----------------------------------------------%
        //        | Eigenvalues are returned in the first column |
        //        | of the two dimensional array D and the       |
        //        | corresponding eigenvectors are returned in   |
        //        | the first NEV columns of the two dimensional |
        //        | array V if requested.  Otherwise, an         |
        //        | orthogonal basis for the invariant subspace  |
        //        | corresponding to the eigenvalues in D is     |
        //        | returned in V.                               |
        //        %----------------------------------------------%
        
                 if (ierr[0] != 0) { 
        
        //           %------------------------------------%
        //           | Error condition:                   |
        //           | Check the documentation of DSEUPD. |
        //           %------------------------------------%
         
                    UI.setDataText("Error in spectralPb with dseupd ierr[0] = " + ierr[0] + "\n");
                    if (ierr[0] == 2) {
                        ss = 0;
                        for (i = 0; i < ncv; i++) {
                        	if (select[i]) {
                        		ss++;
                        	}
                        }
                        if (ss < nvec) {
                        	UI.setDataText("The boolean array select was only set " + ss + " times instead of " + nvec + " times\n");
                        }
                    }
                    else if (ierr[0] == 1) {
                    	UI.setDataText("The Schur form could not be reordered\n");
                    }
                    else if (ierr[0] == -14) {
                    	UI.setDataText("dsaupd did not find any eigenvalues to sufficient accuracy\n");
                    }
                 } // if (ierr[0] != 0)
                 else  { // ierr[0] == 0
        
                    nconv =  iparam[4];
                    
                 } // else ierr[0] == 0
                 //        %------------------------------------------%
                 //        | Print additional convergence information |
                 //        %------------------------------------------%
                          
                          if ( info[0] == 1) {
                          	UI.setDataText("Maximum number of iterations reached.\n");
                           }
                           else if ( info[0] == 3) {
                          	UI.setDataText("No shifts could be applied during implicit Arnoldi update, try increasing NCV.\n");
                           }     
                  
                           Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("SparseEigenvalue\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("======\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("Size of the matrix = " +  n + "\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("The number of Ritz values requested = " +  nev + "\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("The number of Arnoldi vectors generated ncv = " +  ncv + "\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("What portion of the spectrum: " +  which + "\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("The number of converged Ritz values = " +nconv + "\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("The number of Implicit Arnoldi update iterations taken = " +  iparam[2] + "\n",
                        		   Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("The number of OP*x = " +  iparam[8] + "\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("The convergence criterion = " + tol[0] + "\n", Preferences.DEBUG_ALGORITHM);
                           Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
              } // else info[0] >= 0
              if (ierr[0] != 0) {
            	  err[0] = true;
            	  return;
              }
              
              // Reversal done in eigs just just leave out eigs reversal and this reversal
              // EigVal(1:end) = EigVal(end:-1:1)
              // EigVect(:, 1: end) = EigVect(:, end:-1:1)
              txo = orig_sz[0];
              tyo = orig_sz[1];
              vect = new double[tyo][txo][nvec];
              if (n != (tx*ty)) {
            	  MipavUtil.displayError("n != tx*ty as required for reshaping");
              }

              evTranspose = new double[ty][tx];
              evScale = new double[tyo][txo];
              index = 0;
              for (vv = 1; vv < nvec; vv++) {
                  for (j = 0 ; j < ty; j++) {
                	  for ( i = 0; i < tx; i++) {
                	      evTranspose[j][i] = EigVect[index++][vv];     
                	      imresize(evTranspose, evScale);
                	  }
                  }
                  minVect = Double.MAX_VALUE;
                  maxVect = -Double.MAX_VALUE;
                  // spectral Pb
                  for (i = 0; i < tyo; i++) {
                	  for (j = 0; j < txo; j++) {
                		  vect[i][j][vv] = evScale[i][j];
                		  if (vect[i][j][vv] < minVect) {
                			  minVect = vect[i][j][vv];
                		  }
                		  if (vect[i][j][vv] > maxVect) {
                			  maxVect = vect[i][j][vv];
                		  }
                	  }
                  }
                  range = maxVect - minVect;
                  for (i = 0; i < tyo; i++) {
                	  for (j = 0; j < txo; j++) {
                          vect[i][j][vv] = (vect[i][j][vv] - minVect)/range;  
                	  }
                  }
              } // for (vv = 1; vv < nvec; vv++)
              
              // OE parameters
              dohil = false;
              deriv = 1;
              support = 3;
              sigma = 1.0;
              norient = 8;
              dtheta = Math.PI/norient;
              ch_per = new int[]{4,3,2,1,8,7,6,5};
              vec = new double[tyo][txo];
              // Calculate filter size, make sure it's odd
         	  hsz = (int)Math.ceil(sigma * support);
              sz = 2 * hsz + 1;
              f = new double[sz][sz];
              fim = new double[tyo][txo];
              
              for (vv = 0; vv < nvec; vv++) {
                  if (EigVal[vv] > 0.0)	{
                      for (i = 0; i < tyo; i++) {
                    	  for (j = 0; j < txo; j++) {
                    		  vec[i][j] = vect[i][j][vv]/Math.sqrt(EigVal[vv]);
                    	  }
                      } // for (i = 0; i < tyo; i++)
                      for ( k = 1; k <= norient; k++) {
                          theta = dtheta * k;  
                          oeFilter(f, sigma, support, theta, deriv, dohil);
                          fbRun(fim, f, vec);
                          for (i = 0; i < tyo; i++) {
                        	  for (j = 0; j < txo; j++) {
                        		 fbRun(fim, f, vec);
                        	     sPb[ch_per[k]-1][i][j] = sPb[ch_per[k]-1][i][j] + Math.abs(fim[i][j]);
                        	  }
                          } // for (i = 0; i < tyo; i++)
                      } // for ( k = 1; k <= norient; k++)
                  } // if (EigVal[vv] > 0.0)
              } // for (vv = 0; vv < nvec; vv++)
              
              if ((outFile != null) && (outFile.length() > 0)) {
            	  // save sPb to outFile
            	  array = new double[tyo * txo * norient];
            	  extents = new int[3];
            	  extents[0] = txo;
            	  extents[1] = tyo;
            	  extents[2] = norient;
            	  slsize = txo * tyo;
            	  for (z = 0; z < norient; z++) {
            	      for (y = 0; y < tyo; y++) {
            	    	  for (x = 0; x < txo; x++) {
            	    		  array[x + y * txo + z * slsize] = sPb[z][y][x];
            	    	  }
            	      }
            	  }
            	  spectralImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "spectral");
            	  try {
            		  spectralImage.importData(0, array, true);
            	  }
            	  catch (IOException e) {
            		  if (spectralImage != null) {
                          spectralImage.disposeLocal();
                      }

                      spectralImage = null;
            		  MipavUtil.displayError("IOException " + e + " on spectralImage.importData(0, array, true)");
            		  err[0] = true;
            		  return;
            	  }
            	  try {
                      spectralImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), outFile,
                                         FileUtility.XML, true);
                  } catch (OutOfMemoryError error) {

                      if (spectralImage != null) {
                          spectralImage.disposeLocal();
                      }

                      spectralImage = null;
                      MipavUtil.displayError("Error on spectralImage.saveImage");

                      err[0] = true;

                      return;
                  }
            	  spectralImage.disposeLocal();
                  spectralImage = null;
              } // if ((outFile != null) && (outFile.length() > 0))
             
              return;
    }  // spectralPb
    
    private void fbRun(double fim[][], double fb[][], double im[][]) {
        // Run a filterbank on an image with reflected boundary conditions
    	
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	int maxsz;
    	int r;
    	double impad[][];
    	double fimpad[][];
    	int y;
    	int x;
    	maxsz = Math.max(fb.length, fb[0].length);
    	
    	// Pad the image
    	r = (int)Math.floor(maxsz/2);
    	impad = new double[im.length+2*r][im[0].length + 2*r];
    	padReflect(impad, im, r);
    	
    	// Run the filterbank on the padded image, and crop the result back to the original image size
    	fimpad = new double[impad.length][impad[0].length];
    	if (fb.length < 50) {
    	    conv2(impad,fb,fimpad);	
    	}
    	else {
    	    fftconv2(fimpad, impad, fb);	
    	}
    	for (y = r; y < impad.length - r; y++) {
    	    for (x = r; x < impad[0].length - r; x++) {
    	    	fim[y-r][x-r] = fimpad[y][x];
    	    }
    	}
    }
    
    private void conv2(double A[][], double B[][], double Cout[][]) {
		double L[][];
		double S[][];
		int ml;
		int nl;
		int ms;
		int ns;
		int y;
		int x;
		int y2;
		int x2;
		double C[][];
		double small;
		int yoff;
		int xoff;
		if (A.length * A[0].length >= B.length * B[0].length) {
			ml = A.length;
			nl = A[0].length;
			L = A;
			ms = B.length;
			ns = B[0].length;
			S = B;
		} else {
			ml = B.length;
			nl = B[0].length;
			L = B;
			ms = A.length;
			ns = A[0].length;
			S = A;
		}
		C = new double[ml + ms - 1][nl + ns - 1];
		for (y = 0; y < ms; y++) {
			for (x = 0; x < ns; x++) {
				small = S[y][x];
				if (small != 0.0) {
					for (y2 = 0; y2 < ml; y2++) {
						for (x2 = 0; x2 < nl; x2++) {
							C[y + y2][x + x2] += L[y2][x2] * small;
						}
					}
				}
			}
		}
		yoff = (int) Math.floor(B.length / 2.0);
		xoff = (int) Math.floor(B[0].length / 2.0);
		for (y = 0; y < A.length; y++) {
			for (x = 0; x < A[0].length; x++) {
				Cout[y][x] = C[y + yoff][x + xoff];
			}
		}
		return;
	}
    
    private void fftconv2(double fim[][], double im[][], double f[][]) {
    	double padf[][];
    	int r;
    	int y;
    	int x;
    	FFTUtility fft;
    	int i;
    	// Convolution using fft
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	
    	// Wrap the filter around the origin and pad with zeros
    	padf = new double[im.length][im[0].length];
    	r = (int)Math.floor(f.length/2);
    	for (y = r; y < f.length; y++) {
    		for (x = r; x < f[0].length; x++) {
    			padf[y-r][x-r] = f[y][x];
    		}
    	}
    	for (y = r+1; y < f.length; y++) {
    		for (x = 0; x < r; x++) {
    			padf[y - r - 1][im[0].length - r + x] = f[y][x];
    		}
    	}
    	for (y = 0; y < r; y++) {
    		for (x = r+1; x < f[0].length; x++) {
    			padf[im.length - r + y][x - r - 1] = f[y][x];
    		}
    	}
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < r; x++) {
    			padf[im.length - r + y][im[0].length - r + x] = f[y][x];
    		}
    	}
    	
    	// Magic
    	double imFFT[] = new double[im.length * im[0].length];
    	double imFFTImag[] = new double[im.length * im[0].length];
    	double padfFFT[] = new double[im.length * im[0].length];
    	double padfFFTImag[] = new double[im.length * im[0].length];
    	double prod[] = new double[im.length * im[0].length];
    	double prodImag[] = new double[im.length * im[0].length];
    	for (y = 0; y < im.length; y++) {
    		for (x = 0; x < im[0].length; x++) {
    			imFFT[x + y * im[0].length] = im[y][x];
    			padfFFT[x + y * im[0].length] = padf[y][x];
    		}
    	}
    	fft = new FFTUtility(imFFT, imFFTImag, im.length, im[0].length, 1,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(imFFT, imFFTImag, 1, im.length, im[0].length,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		
		fft = new FFTUtility(padfFFT, padfFFTImag, im.length, im[0].length, 1,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(padfFFT, padfFFTImag, 1, im.length, im[0].length,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		
		for (i = 0; i < im.length * im[0].length; i++) {
			prod[i] = imFFT[i]*padfFFT[i] - imFFTImag[i]*padfFFTImag[i];
			prodImag[i] = imFFT[i]*padfFFTImag[i] + imFFTImag[i]*padfFFT[i];
		}
		// Inverse fft
		fft = new FFTUtility(prod, prodImag, im.length, im[0].length, 1,
				1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(prod, prodImag, 1, im.length, im[0].length,
				1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		for (y = 0; y < im.length; y++) {
			for (x = 0; x < im[0].length; x++) {
				fim[y][x] = prod[x + y * im[0].length];
			}
		}
		return;
    }
    
    private void padReflect(double impad[][], double im[][], int r) {
    	// Pad an image with a border of size r, and reflect the image into the border
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	int x;
    	int y;
    	// Middle
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[y + r][x + r] = im[y][x];
    		}
    	}
    	// Top
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[r - y - 1][x + r] = im[y][x];
    		}
    	}
    	// Bottom
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[2*yDim + r - y - 1][x + r] = im[y][x];
    		}
    	}
    	// Left
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < r; x++) {
    			impad[y+r][r - x - 1] = im[y][x];
    		}
    	}
    	// Right
    	for (y = 0; y < yDim; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[y+r][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
    	// Top-left
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < r; x++) {
    			impad[r - y - 1][r - x - 1] = im[y][x];
    			
    			
    			
 
    		}
    	}
    	// Top-right
    	for (y = 0; y < r; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[r - y - 1][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
    	// Bottom-left
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = 0; x < r; x++) {
    			impad[2*yDim + r - y - 1][r - x - 1] = im[y][x];
    		}
    	}
    	// Bottom-right
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[2*yDim + r - y - 1][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
    }
    
    /**
     * Compute unit L1- norm 2D filter.
     * The filter is a Gaussian in the x direction
     * The filter is a Gaussian derivative with optional Hilbert transform in the y direction.
     * The filter is zero-meaned if deriv > 0.
     * @param f output [sz][sz] square filter
     * @param sigma
     * @param support Make filter +/- this many sigma
     * @param theta Orientation of x axis, in radians
     * @param deriv Degree of y derivative, one of {0, 1, 2}.
     * @param dohil Do Hilbert transform in y direction?
     */
    private void oeFilter(double f[][], double sigma, int support, double theta,
    		int deriv, boolean dohil) {
    	// Original MATLAB code David R. Martin <dmartin@eecs.berkeley.edu>
    	// March 2003
    	int hsz;
    	int sz;
    	int maxsamples;
    	int maxrate;
    	int frate;
    	int rate;
    	int samples;
    	double r;
    	double dom[];
    	double stepSize;
    	int i;
    	double sx[][];
    	double sy[][];
    	int x;
    	int y;
    	int mx[][];
    	int my[][];
    	int membership[][];
    	double su[][];
    	double sv[][];
    	double R;
    	int fsamples;
    	double fdom[];
    	double gap;
    	double fx[];
    	double fy[];
    	double denom;
    	int xi[][];
    	int yi[][];
    	double fprecursor[][];
    	int v;
    	double fsum;
    	double fmean;
    	double fsumabs;
    	int paddedfsamples;
    	double paddedfy[];
    	AlgorithmHilbertTransform ht;
    
    	if ((deriv < 0) || (deriv > 2)) {
    		MipavUtil.displayError("deriv = " + deriv + "in oeFilter");
    		return;
    	}
    	
    	 // Calculate filter size, make sure it's odd
    	 hsz = (int)Math.ceil(sigma * support);
         sz = 2 * hsz + 1;
         
         // Sampling limits
         // Max samples in each dimension
         maxsamples = 1000;
         // Maximum sampling rate
         maxrate = 10;
         // Over-sampling rate for function evaluation
         frate = 10;
         
         // Calculate sampling rate and number of samples
         rate = (int)Math.min(maxrate,  Math.max(1, Math.floor(maxsamples/sz)));
         samples = sz * rate;
         
         // The 2D sampling grid
         r = Math.floor(sz/2.0) + 0.5 * (1.0 - 1.0/rate);
         dom = new double[samples];
         dom[0] = -r;
         dom[samples-1] = r;
         stepSize = (2.0*r)/(samples - 1.0);
         for (i = 1; i < samples-1; i++) {
             dom[i] = -r + i * stepSize;	 
         }
         sx = new double[samples][samples];
         sy = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 sx[y][x] = dom[x];
        		 sy[y][x] = dom[y];
        	 }
         }
         // Bin membership for 2D grid points
         mx = new int[samples][samples];
         my = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 mx[y][x] = (int)Math.round(sx[y][x]);
        		 my[y][x] = (int)Math.round(sy[y][x]);
        	 }
         }
         membership = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        	     membership[y][x] = (mx[y][x] + hsz + 1) + (my[y][x]+ hsz)*sz;	 
        	 }
         }
         
         // Rotate the 2D sampling grid by theta
         
         su = new double[samples][samples];
         sv = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 su[y][x] = sx[y][x]*Math.sin(theta) + sy[y][x]*Math.cos(theta);
        		 sv[y][x] = sx[y][x]*Math.cos(theta) - sy[y][x]*Math.sin(theta);
        	 }
         }
         
         // Evaluate the function separably on a finer grid
         // Radius of domain, enlarged by > sqrt(2)
         R = r * Math.sqrt(2.0) * 1.01;
         // Number of samples
         fsamples = (int)Math.ceil(R * rate * frate);
         // Must be odd
         fsamples = fsamples + ((fsamples+1)%2);
         // Domain for function evaluation
         fdom = new double[fsamples];
         fdom[0] = -R;
         fdom[fsamples-1] = R;
         // Distance between samples
         gap = (2.0*R)/(fsamples - 1.0);
         for (i = 1; i < fsamples-1; i++) {
             fdom[i] = -R + i * gap;	 
         } 
         
         // The function is a Gaussian in the x direction
         fx = new double[fsamples];
         denom = 2.0*sigma*sigma;
         for (i = 0; i < fsamples; i++) {
        	 fx[i] = Math.exp(-fdom[i]*fdom[i]/denom);
         }
         // .. and a Gaussian derivative in the y direction
         fy = new double[fsamples];
         denom = 2.0*sigma*sigma;
         for (i = 0; i < fsamples; i++) {
        	 fy[i] = Math.exp(-fdom[i]*fdom[i]/denom);
         }
         switch (deriv) {
         case 1:
        	 denom = sigma*sigma;
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = fy[i] * (-fdom[i]/denom);
        	 }
        	 break;
         case 2:
        	 denom = sigma *sigma;
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = fy[i] *(fdom[i]*fdom[i]/denom - 1.0);
        	 }
         } // switch(deriv)
         // an optional Hilbert transform
         if (dohil) {
        	 paddedfsamples = MipavMath.findMinimumPowerOfTwo(fsamples);
        	 paddedfy = new double[2 * paddedfsamples];
        	 for (i = 0; i < fsamples; i++) {
        		 paddedfy[2*i] = fy[i];
        	 }
        	 ht = new AlgorithmHilbertTransform(paddedfy, paddedfsamples);
        	 ht.run();
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = paddedfy[2*i+1];
        	 }
         }
         
         // Evaluate the function with NN interpolation
         xi = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 xi[y][x] =(int) Math.round(su[y][x]/gap) + (int)Math.floor(fsamples/2) + 1;
        	 }
         }
         yi = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 yi[y][x] =(int) Math.round(sv[y][x]/gap) + (int)Math.floor(fsamples/2) + 1;
        	 }
         }
         fprecursor = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        	     fprecursor[y][x] = fx[xi[y][x]-1] * fy[yi[y][x]-1]; 
        	 }
         }
         // Accumulate the samples into each bin
         for (y = 0; y < sz; y++) {
        	 for (x = 0; x < sz; x++) {
        	     f[y][x] = 0.0;	 
        	 }
         }
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 v = membership[y][x];
        		 if (v < 1) {
        			 continue;
        		 }
        		 if (v > sz*sz) {
        			 continue;
        		 }
        		 f[(v-1)%sz][(v-1)/sz] += fprecursor[y][x];
        	 }
         }
         
         // Zero mean
         if (deriv > 0) {
	         fsum = 0.0;
	         for (y = 0; y < sz; y++) {
	        	 for (x = 0; x < sz; x++) {
	        	     fsum += f[y][x];	 
	        	 }
	         }
	         fmean = fsum/(sz*sz);
	         for (y = 0; y < sz; y++) {
	        	 for (x = 0; x < sz; x++) {
	        		 f[y][x] -= fmean;
	        	 }
	         }
         } // if (deriv > 0)
         
         // Unit L1-norm
         fsumabs = 0.0;
         for (y = 0; y < sz; y++) {
        	 for (x = 0; x < sz; x++) {
        		 fsumabs += Math.abs(f[y][x]);
        	 }
         }
         if (fsumabs > 0.0) {
        	 for (y = 0; y < sz; y++) {
        		 for (x = 0; x < sz; x++) {
        			 f[y][x] = f[y][x]/fsumabs;
        		 }
        	 }
         }
    }
    
    private class DualLattice{
    	public double H[][];
    	public double V[][];
    	public int width;
    	public int height;
    	
    	public DualLattice() {
    		
    	}
    }
    
    private class PointIC {
    	
    	public PointIC() {
    		
    	}
    	public int x,y;
    	public double sim;
    }
    
    private SMatrix buildW(double H[][], double V[][], boolean error[]) {
    	int x;
    	int y;
    	int dthresh = 5;
    	double sigma = 0.1;
    	// Copy edge info into lattice struct
    	DualLattice boundaries = new DualLattice();
        int H_h = H[0].length;
        int H_w = H.length;
        boundaries.H = new double[H_h][H_w];
        for (y = 0; y < H_h; y++) {
        	for (x = 0; x < H_w; x++) {
        		boundaries.H[y][x] = H[x][y];
        	}
        }
        int V_h = V[0].length;
        int V_w = V.length;
        boundaries.V = new double[V_h][V_w];
        for (y = 0; y < V_h; y++) {
        	for (x = 0; x < V_w; x++) {
        		boundaries.V[y][x] = V[x][y];
        	}
        }
        boundaries.width = boundaries.H.length;
        boundaries.height = boundaries.V[0].length;
        PointIC ic[][][] = new PointIC[boundaries.width][boundaries.height][];
        
        computeSupport(boundaries, dthresh, 1.0, ic, error);
        if (error[0]) {
        	System.err.println("Error in buildW call to computeSupport");
        	return null;
        }
        SMatrix W[] = new SMatrix[1];
        computeAffinities2(ic, sigma, dthresh, W);
        if (W[0] == null) {
        	System.err.println("computeAffinities2 call failed in buildW");
        	error[0] = true;
        	return null;
        }
        else {
        	return W[0];
        }
    }
    
    /**
     * Compute similarities for the set of "true" pixels in region.
     * affinity matrix is ordered in scanline order
     * @param icmap
     * @param sigma
     * @param dthresh
     * @param affinities
     */
    private void computeAffinities2(final PointIC icmap[][][], final double sigma, final double dthresh,
    		SMatrix affinities[]) {
    	int i;
    	int x;
    	int y;
        int width = icmap.length;
        int height = icmap[0].length;
        
        // Build a scanline order index
        int numPixels = 0;
        int index[][] = new int[width][height];
        for (y = 0; y < height; y++) {
        	for (x = 0; x < width; x++) {
        		index[x][y] = numPixels;
        		numPixels++;
        	}
        }
        
        // Sparse matrix data
        // Number of non-zero entries in each row
        int nz[] = new int[numPixels];
        // The values in each row
        double vals[][] = new double[numPixels][];
        // The column number for each value in the row
        int col[][] = new int[numPixels][];
        
        int dthreshi = (int)Math.ceil(dthresh);
        // Window diameter
        int wd = 2*dthreshi+1;
        PointIC connections[] = new PointIC[wd*wd];
        for (i = 0; i < wd*wd; i++) {
        	connections[i] = new PointIC();
        }
        
        int row = 0;
        for (x = 0; x < width; x++) {
        	for (y = 0; y < height; y++) {
        		// The row we are now working on
        		row = index[x][y];
        		// Connection count for row i
        		nz[row] = 0;
        		// Index into sparse supportMap
        		int icIndex = 0;
        		for (int u = -dthreshi; u <= dthreshi; u++) {
        			int yy = y + u;
        			for (int v = -dthreshi; v <= dthreshi; v++) {
        				int xx = x + v;
        				if (xx < 0 || xx >= width) {continue;}
        				if (yy < 0 || yy >= height) {continue;}
        				if (u*u+v*v > dthresh*dthresh) {continue;}
        				
        				// Increment our index into the support map
        				while (icIndex < icmap[x][y].length && icmap[x][y][icIndex].y < yy) {
        					icIndex++;
        				}
        				while (icIndex < icmap[x][y].length && icmap[x][y][icIndex].x < xx) {
        					icIndex++;
        				}
        				
        				// Connection strength
        				double pss = 0.0;
        				if ((u == 0) && (v == 0)) {
        					pss = 1.0;
        				}
        				else {
        					double icsim = 0.0;
        					if (icIndex < icmap[x][y].length && icmap[x][y][icIndex].x == xx &&
        							icmap[x][y][icIndex].y == yy) {
        						icsim = icmap[x][y][icIndex].sim;
        						icIndex++;
        					}
        					pss = C_IC_SS(1.0 - icsim);
        				}
        				
        				connections[nz[row]].x = xx;
        				connections[nz[row]].y = yy;
        				connections[nz[row]].sim = pss;
        				nz[row]++;
        			} // for (int v = -dthreshi; v <= dthreshi; v++)
        		} // for (int u = -dthreshi; u <= dthreshi; u++)
        		
        		// Fill in entries of sparse matrix
        		vals[row] = new double[nz[row]];
        		col[row] = new int[nz[row]];
        		
        		for (int j = 0; j < nz[row]; j++) {
        			double val = Math.exp(-(1.0 - connections[j].sim)/sigma);
        			if ((val < 0) || (val > 1.0)) {
        				MipavUtil.displayError("val out of range in computeAffinities2");
        				return;
        			}
        			vals[row][j] = val;
        			col[row][j] = index[connections[j].x][connections[j].y];
        		}
        	} // for (y = 0; y < height; y++)
        } // for (x = 0; x < width; x++)
        affinities[0] = new SMatrix(numPixels, nz, col, vals);
        affinities[0].symmetrize();
    }
    
    /**
     * probability-of-same-segment logistic fits for color and grayscale
     * optimized for dthresh = 20
     * @param ic
     * @return
     */
    private double C_IC_SS(double ic) {
        double val = 1.8682;
        val += (ic / 0.3130)*-1.3113;
        final double post = 1.0/(1.0 + Math.exp(-val));
        if (Double.isInfinite(post)) { return 0.0; }
        if (post < 0.0) { return 0.0; }
        if (post > 1.0) { return 1.0; }
        return post;
    }
    
    private class SMatrix {
    	public int n;
    	public int nz[];
    	public int col[][];
    	public double values[][];
    	// Number of nonzero entries in sparse matrix
    	//public int nnz;
    	
    	public SMatrix(int n, int nz[], int col[][], double values[][]) {
    		this.n = n;
    		this.nz = nz;
    		this.col = col;
    		this.values = values;
    		//nnz = 0;
    		//for (int i = 0; i < n; i++) {
    		    //nnz += nz[i];	
    		//}
    	}
    	
    	public void symmetrize()
    	{
    	  int tail[] = new int[n];  
    	  for (int r = 0; r < n; r++) 
    	  {
    	    int offset = 0;
    	    while ((offset < nz[r]) && (col[r][offset] < r+1))
    	    {
    	      offset++;
    	    }
    	    for (int i = offset; i < nz[r]; i++) 
    	    {
    	      int c = col[r][i];
    	      assert( col[c][tail[c]] == r ); 
    	      double v_rc = values[r][i];
    	      double v_cr = values[c][tail[c]];
    	      values[r][i] = 0.5*(v_rc+v_cr);
    	      values[c][tail[c]] = 0.5*(v_rc+v_cr);
    	      tail[c]++;
    	    }
    	  }  
    	}
    	
    	public void dispose() {
    		int i;
    		nz = null;
    		for (i = 0; i < col.length; i++) {
    			col[i] = null;
    		}
    		col = null;
    		for (i = 0; i < values.length; i++) {
    			values[i] = null;
    		}
    		values = null;
    	}

    }

    
    /**
     * Given a pb image and window radius, compute a support map for each
     * pixel out to the given radius.
     * @param boundaries
     * @param wr
     * @param thresh
     * @param support
     * @param error
     */
    private void computeSupport(final DualLattice boundaries, final int wr, final double thresh,
    		PointIC support[][][], boolean error[]) {
    	int x;
    	int y;
        //support = new PointIC[boundaries.width][boundaries.height][];
        PointIC adj[] = new PointIC[(2*wr+1)*(2*wr+1)];
        int count[] = new int[1];
        for (x = 0; x < boundaries.width; x++) {
        	for (y = 0; y < boundaries.height; y++) {
        		interveningContour(boundaries, thresh, x, y, wr, adj, count, error);
        		if (error[0]) {
        			System.err.println("Error in computeSupport call to interveningContour for x = " + x + " y = " + y);
        			return;
        		}
        		PointIC map[] = new PointIC[count[0]];
        		for (int i = 0; i < count[0]; i++) {
        			map[i] = adj[i];
        			final int ix = map[i].x;
        			final int iy = map[i].y;
        			if ((ix < 0) || (ix >= boundaries.width)) {
        				System.err.println("ix out of range in computeSupport");
        				error[0] = true;
        				return;
        			}
        			if ((iy < 0) || (iy >= boundaries.height)) {
        				System.err.println("iy out of range in computeSupport");
        				error[0] = true;
        				return;
        			}
        		}
        		support[x][y] = map;
        	}
        }
    }
    
    /**
     * Given a pb image, a pixel (x0, y0), and a pb threshold, compute the intervening-contour
     * weight to all pixels inside a given box of radius "wr" subject to the threshold "thresh".
     * pb is max-accumulated along bresenham lines.  The result is stored in scanline order as
     * a list of points and thier pb value.
     * @param boundaries
     * @param thresh
     * @param x0
     * @param y0
     * @param wr
     * @param adj
     * @param count
     * @param error
     */
    private void interveningContour(final DualLattice boundaries, final double thresh, final int x0, final int y0,
    		final int wr, PointIC adj[], int count[], boolean error[]) {
    	final int width = boundaries.width;
    	final int height = boundaries.height;
    	
    	// Make usre (x0,y0) is valid
    	if (x0 < 0 || x0 >= width) {
    		System.err.println("x0 = " + x0 + " is invalid in interveningContour");
    		error[0] = true;
    		return;
    	}
    	if (y0 < 0 || y0 >= height) {
    		MipavUtil.displayError("y0 = " + y0 + " is invalid in interveningContour");
    		error[0] = true;
    		return;
    	}
        // Make sure adj array is big enough
    	//adj = new PointIC[(2*wr+1)*(2*wr+1)];
    	
    	// Allocate space for lists of pixels; this operation is O(1) 
    	// since the space need not be initialized
    	PointIC scanLines[][] = new PointIC[2*wr+1][2*wr+1];
    	
    	// We need to keep track of the length of the scan lines
    	int scanCount[] = new int[2*wr+1];
    	
    	// Scratch space for ic_walk() function
    	PointIC scratch[] = new PointIC[4*wr+2];
    	
    	// The rectangle of interest, a square with edge of length
    	// 2*wr+1 clipped to the image dimensions
    	final int rxa = Math.max(0, x0-wr);
    	final int rya = Math.max(0, y0-wr);
    	final int rxb = Math.min(x0+wr, width-1);
    	final int ryb = Math.min(y0+wr, height-1);
    	
    	// Walk around the boundary, collecting points in the scanline array.
    	// First walk around the rectangle boundary clockwise for theta = [pi, 0]
    	if (x0 > rxa) { // left
    		if ((y0 > 0) && (y0 < ryb)) {
    			// ic_walk #1
    			ic_walk(boundaries, thresh, x0, y0, rxa, y0 - 1, rxa, y0,
    					rxa, y0+1, wr, scratch, scanCount, scanLines, error);
    			if (error[0]) {
    				System.err.println("Error on ic_walk #1 in interveningContour");
    				System.err.println("x0 = " + x0 + " rxa = " + rxa);
    				System.err.println("y0 = " + y0 + " ryb = " + ryb);
    				return;
    			}
    		}
    		for (int y = y0-1; y > rya; y--) {
    			// ic_walk #2
    			ic_walk(boundaries, thresh, x0, y0, rxa, y-1, rxa, y,
    					rxa, y+1, wr, scratch, scanCount, scanLines, error);
    			if (error[0]) {
    				System.err.println("Error on ic_walk #2 in interveningContour");
    				System.err.println("x0 = " + x0 + " rxa = " + rxa);
    				System.err.println("y0 = " + y0 + " rya = " + rya);
    				return;
    			}
    		}
    	} // if (x0 > rxa)
    	
    	if (x0 > rxa+1 || y0 > rya+1 || ((x0 > rxa) && (y0 > rya))) { // top-left
    		// ic_walk #3
    		ic_walk(boundaries, thresh, x0, y0, rxa, rya+1, rxa, rya,
    				rxa+1, rya, wr, scratch, scanCount, scanLines, error);
    		if (error[0]) {
				System.err.println("Error on ic_walk #3 in interveningContour");
				System.err.println("x0 = " + x0 + " rxa = " + rxa);
				System.err.println("y0 = " + y0 + " rya = " + rya);
				return;
			}
    	}
    	if (((x0 == rxa) && (y0 == rya+1))  || ((x0 == rxa+1) && (y0 == rya))) {
    	    PointIC pnt = new PointIC();
    	    pnt.x = rxa;
    	    pnt.y = rya;
    	    pnt.sim = 1.0;
    	    final int yind = pnt.y - y0 + wr;
    	    scanLines[yind][scanCount[yind]++] = pnt;
    	}
    	
    	if (y0 > rya) { // top
    		for (int x = rxa+1; x < rxb; x++) {
    			// ic_walk #4
    			ic_walk(boundaries, thresh, x0, y0, x-1, rya, x, rya, x+1,
    					rya, wr, scratch, scanCount, scanLines, error);
    			if (error[0]) {
    				System.err.println("Error on ic_walk #4 in interveningContour");
    				System.err.println("x0 = " + x0 + " rxa = " + rxa + " rxb = " + rxb);
    				System.err.println("y0 = " + y0 + " rya = " + rya);
    				return;
    			}
    		}
    	}
    	
    	if ((y0 > rya+1) || (x0 < rxb-1) || ((y0 > rya) && (x0 < rxb))) { // top-right
    		// ic_walk #5
    		ic_walk(boundaries, thresh, x0, y0, rxb-1, rya, rxb, rya,
    				rxb, rya+1, wr, scratch, scanCount, scanLines, error);
    		if (error[0]) {
				System.err.println("Error on ic_walk #5 in interveningContour");
				System.err.println("x0 = " + x0 + " rxb = " + rxb);
				System.err.println("y0 = " + y0 + " rya = " + rya);
				return;
			}
    	}
    	if (((x0 == rxb-1) && (y0 == rya)) || ((x0 == rxb) && (y0 == rya+1))) {
    		PointIC pnt = new PointIC();
    		pnt.x = rxb;
    		pnt.y = rya;
    		pnt.sim = 1.0;
    		final int yind = pnt.y - y0 + wr;
    		scanLines[yind][scanCount[yind]++] = pnt;
    	}
    	
    	if (x0 < rxb) { // right
    		for (int y = rya+1; y < y0; y++) {
    			// ic_walk #6
    			ic_walk(boundaries, thresh, x0, y0, rxb, y-1, rxb, y,
    					rxb, y+1, wr, scratch, scanCount, scanLines, error);
    			if (error[0]) {
    				System.err.println("Error on ic_walk #6 in interveningContour");
    				System.err.println("x0 = " + x0 + " rxb = " + rxb);
    				System.err.println("y0 = " + y0 + " rya = " + rya);
    				return;
    			}
    		}
    	}
    	
    	// Now counterclockwise for theta = (pi, 0)
    	if (x0 > rxa) { // left
    		for (int y = y0+1; y < ryb; y++) {
    			// ic_walk #7
    			ic_walk(boundaries, thresh, x0, y0, rxa, y-1, rxa, y,
    					rxa, y+1, wr, scratch, scanCount, scanLines, error);
    			if (error[0]) {
    				System.err.println("Error on ic_walk #7 in interveningContour");
    				System.err.println("x0 = " + x0 + " rxa = " + rxa);
    				System.err.println("y0 = " + y0 + " ryb = " + ryb);
    				return;
    			}
    		}
    	}
    	
    	if ((x0 > rxa+1) || (y0 < ryb-1) || ((x0 > rxa) && (y0 < ryb))) { // bottom-left
    		// ic_walk #8
    		ic_walk(boundaries, thresh, x0, y0, rxa, ryb-1, rxa, ryb,
    				rxa+1, ryb, wr, scratch, scanCount, scanLines, error);
    		if (error[0]) {
				System.err.println("Error on ic_walk #8 in interveningContour");
				System.err.println("x0 = " + x0 + " rxa = " + rxa);
				System.err.println("y0 = " + y0 + " ryb = " + ryb);
				return;
			}
    	}
    	if (((x0 == rxa) && (y0 == ryb-1)) || ((x0 == rxa+1) && (y0 == ryb))) {
    		PointIC pnt = new PointIC();
    		pnt.x = rxa;
    		pnt.y = ryb;
    		pnt.sim = 1.0;
    		final int yind = pnt.y - y0 + wr;
    		scanLines[yind][scanCount[yind]++] = pnt;
    	}
    	if (y0 < ryb) { // bottom
    		for (int x = rxa+1; x < rxb; x++) {
    			// ic_walk #9
    			ic_walk(boundaries, thresh, x0, y0, x-1, ryb, x, ryb, x+1,
    					ryb, wr, scratch, scanCount, scanLines, error);
    			if (error[0]) {
    				System.err.println("Error on ic_walk #9 in interveningContour");
    				System.err.println("x0 = " + x0 + " rxa = " + rxa + " rxb = " + rxb);
    				System.err.println("y0 = " + y0 + " ryb = " + ryb);
    				return;
    			}
    		}
    	}
    	
    	if ((y0 < ryb-1) || (x0 < rxb-1) || ((y0 < ryb) && (x0 < rxb))) { // bottom-right
    		// ic_walk #10
    		ic_walk(boundaries, thresh, x0, y0, rxb-1, ryb, rxb, ryb,
    				rxb, ryb-1, wr, scratch, scanCount, scanLines, error);
    		if (error[0]) {
				System.err.println("Error on ic_walk #10 in interveningContour");
				System.err.println("x0 = " + x0 + " rxb = " + rxb);
				System.err.println("y0 = " + y0 + " ryb = " + ryb);
				return;
			}
    	}
    	if (((x0 == rxb-1) && (y0 == ryb)) || ((x0 == rxb) && (y0 == ryb-1))) {
    		PointIC pnt = new PointIC();
    		pnt.x = rxb;
    		pnt.y = ryb;
    		pnt.sim = 1.0;
    		final int yind = pnt.y - y0 + wr;
    		scanLines[yind][scanCount[yind]++] = pnt;
    	}
    	
    	if (x0 < rxb) { // right
    		for (int y = ryb-1; y > y0; y--) {
    			// ic_walk #11
    			ic_walk(boundaries, thresh, x0, y0, rxb, y-1, rxb, y,
    					rxb, y+1, wr, scratch, scanCount, scanLines, error);
    			if (error[0]) {
    				System.err.println("Error on ic_walk #11 in interveningContour");
    				System.err.println("x0 = " + x0 + " rxb = " + rxb);
    				System.err.println("y0 = " + y0 + " ryb = " + ryb);
    				return;
    			}
    		}
    		if ((y0 > 0) && (y0 < ryb)) {
    			// ic_walk #12
    			ic_walk(boundaries, thresh, x0, y0, rxb, y0-1, rxb, y0,
    					rxb, y0+1, wr, scratch, scanCount, scanLines, error);
    			if (error[0]) {
    				System.err.println("Error on ic_walk #12 in interveningContour");
    				System.err.println("x0 = " + x0 + " rxb = " + rxb);
    				System.err.println("y0 = " + y0 + " ryb = " + ryb);
    				return;
    			}
    		}
    	}
    	
    	for (int y = 0; y < 2*wr+1; y++) {
    		int len = scanCount[y];
    		if (len < 0 || len > 2*wr+1) {
    			System.err.println("len = " + len + " out of bounds in interveningContour");
    			error[0] = true;
    			return;
    		}
    		
    		if (y + y0 - wr < 0) {
    			if (len != 0) {
    				System.err.println("len != 0 as expected in interveningContour");
    				error[0] = true;
    				return;
    			}
    		}
    		
    		// Check that pixels are in the right row
    		for (int i = 0; i < len; i++) {
    			if (scanLines[y][i].y != y+y0-wr) {
    				System.err.println("scanLines[y][i].y != y+y0-wr in interveningContour");
    				error[0] = true;
    				return;
    			}
    		}
    		
    		// Check that pixels in each row are in increasing order
    		for (int i = 0; i < len-1; i++) {
    			if (scanLines[y][i].x >= scanLines[y][i+1].x) {
    				System.err.println("scanLines[y][i].x >= scanLines[y][i+1].x in interveningContour");
    				error[0] = true;
    				return;
    			}
    		}
    	} // for (int y = 0; y < 2*wr+1; y++)
    	
    	// Construct the adjacency list
    	count[0] = 0;
    	for (int y = 0; y < 2*wr+1; y++) {
    		int len = scanCount[y];
    		for (int i = 0; i < len; i++) {
    			adj[count[0]++] = scanLines[y][i];    
    		}
    	}
    }
    
    /**
     * Walk the bresenham line from (x0,y0) to (x2,y2) ignoring any points outside
     * a circular window of radius wr.
     * (x1,y1) and (x3,y3) should be on either side of (x2,y2)
     * 
     * For each line, stop if the max-accumulated pb is > thresh.
     * 
     * Append any points on the line (for which the line is the best approximant
     * and distance from x0 is less than wr) to scanlines array.
     * 
     * points is preallocated scratch space.
     * scanCount and scanLines store the results
     * 
     * @param boundaries
     * @param thresh
     * @param x0
     * @param y0
     * @param x1
     * @param y1
     * @param x2
     * @param y2
     * @param x3
     * @param y3
     * @param wr
     * @param points
     * @param scanCount
     * @param scanLines
     * @param error
     */
    private void ic_walk(final DualLattice boundaries, final double thresh,
    		final int x0, final int y0,
    		final int x1, final int y1,
    		final int x2, final int y2,
    		final int x3, final int y3,
    		final int wr,
    		PointIC points[],
    		int scanCount[],
    		PointIC scanLines[][], boolean error[]) {
        final int width = boundaries.width;
        final int height = boundaries.height;
        
        // The predicate that uses long longs will overflow if the image
        // is too long.
        if (2*wr +1 >= 1000) {
        	System.err.println("2*wr+1 >= 1000 in ic_walk");
        	error[0] = true;
        	return;
        }
        
        // Make sure points array is big enough
        if (points.length < 4*wr+2) {
        	System.err.println("points.length < 4*wr+2 in ic_walk");
        	error[0] = true;
        	return;
        }
        
        // Make sure scan arrays are the right size
        if (scanCount.length != 2*wr+1) {
        	System.err.println("scanCount.length != 2*wr+1 in ic_walk");
        	error[0] = true;
        	return;
        }
        if (scanLines.length != 2*wr+1) {
        	System.err.println("scanLines.length != 2*wr+1 in ic_walk");
        	error[0] = true;
        	return;
        }
        if (scanLines[0].length != 2*wr+1) {
        	System.err.println("scanLines[0].length != 2*wr+1 in ic_walk");
        	error[0] = true;
        	return;	
        }
        
        // Sanity check the points
        if (x0 < 0 || x0 >= width) {
        	System.err.println("x0 out of range in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (y0 < 0 || y0 >= height) {
        	System.err.println("y0 out of range in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (x1 < 0 || x1 >= width) {
        	System.err.println("x1 out of range in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (y1 < 0 || y1 >= height) {
        	System.err.println("y1 out of range in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (x2 < 0 || x2 >= width) {
        	System.err.println("x2 out of range in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (y2 < 0 || y2 >= height) {
        	System.err.println("y2 out of range in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (x3 < 0 || x3 >= width) {
        	System.err.println("x3 out of range in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (y3 < 0 || y3 >= height) {
        	System.err.println("y3 out of range in ic_walk");
        	error[0] = true;
        	return;
        }
        
        // Make sure points are all distinct
        if (x0 == x1 && y0 == y1) {
        	System.err.println("(x0,y0) is the same as (x1,y1) in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (x0 == x2 && y0 == y2) {
        	System.err.println("(x0,y0) is the same as (x2,y2) in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (x0 == x3 && y0 == y3) {
        	System.err.println("(x0,y0) is the same as (x3,y3) in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (x1 == x2 && y1 == y2) {
        	System.err.println("(x1,y1) is the same as (x2,y2) in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (x1 == x3 && y1 == y3) {
        	System.err.println("(x1,y1) is the same as (x3,y3) in ic_walk");
        	error[0] = true;
        	return;
        }
        
        if (x2 == x3 && y2 == y3) {
        	System.err.println("(x2,y2) is the same as (x3,y3) in ic_walk");
        	error[0] = true;
        	return;
        }
        
        // Constants used in testing whether this is the best path
        final long dx1 = x1 - x0;
        final long dy1 = y1 - y0;
        final long dx2 = x2 - x0;
        final long dy2 = y2 - y0;
        final long dx3 = x3 - x0;
        final long dy3 = y3 - y0;
        final long dot11 = dx1 * dx1 + dy1 * dy1;
        final long dot22 = dx2 * dx2 + dy2 * dy2;
        final long dot33 = dx3 * dx3 + dy3 * dy3;
        
        // Compute dx, dy for the bresenham line
        final int dx = x2 - x0;
        final int dy = y2 - y0;
        final int adx = Math.abs(dx);
        final int ady = Math.abs(dy);
        
        // Figure out what octant we're in for the bresenham algorithm
        // Octant i covers pi/4 * [i,i+1)
        int octant = -1;
        if (dx > 0 && dy >= 0) { // Quadrant 0
        	octant = (adx > ady) ? 0: 1;
        }
        else if (dx <= 0 && dy > 0) { // Quadrant 1
        	octant = (adx < ady) ? 2: 3;
        }
        else if (dy <= 0 && dx < 0) { // Quadrant 2
        	octant = (adx > ady) ? 4 : 5;
        }
        else if (dx >= 0 && dy < 0) { // Quadrant 3
        	octant = (adx < ady) ? 6 : 7;
        }
        else {
        	System.err.println("Octant error in ic_walk");
        	System.err.println("dx = " + dx + " dy = " + dy);
        	error[0] = true;
        	return;
        }
        
        // t is our bresenham counter
        int t = 0;
        switch (octant) {
        case 0: t = -adx; break;
        case 1: t = -ady; break;
        case 2: t = -ady; break;
        case 3: t = -adx; break;
        case 4: t = -adx; break;
        case 5: t = -ady; break;
        case 6: t = -ady; break;
        case 7: t = -adx; break;
        }
        
        // maxpb contains the max-accumulation of pb from (x0,y0) to (x,y)
        // on the bresenham line.
        double maxpb = 0.0;
        
        // (xi,yi) is our current location on the bresenham line
        int xi = x0;
        int yi = y0;
        
        // Accumulate points in the order we find them
        int count = 0;
        int oldx = xi;
        int oldy = yi;
        
        // Walk the line
        while (xi != x2 || yi != y2) {
        	// Step one pixel on the bresenham line
        	switch (octant) {
        	case 0:
        		xi++; t += (ady << 1);
        		if (t > 0) {yi++; t -= (adx << 1); }
        		break;
        	case 1:
        		yi++; t += (adx << 1);
        		if (t > 0) {xi++; t -= (ady << 1); }
        		break;
        	case 2:
        		yi++; t += (adx << 1);
        		if (t > 0) {xi--; t -= (ady << 1); }
        		break;
        	case 3:
        		xi--; t += (ady << 1);
        		if (t > 0) {yi++; t -= (adx << 1); }
        		break;
        	case 4:
        		xi--; t += (ady << 1);
        		if (t > 0) {yi--; t -= (adx << 1); }
        		break;
        	case 5:
        		yi--; t += (adx << 1);
        		if (t > 0) {xi--; t -= (ady << 1); }
        		break;
        	case 6:
        		yi--; t += (adx << 1);
        		if (t > 0) {xi++; t -= (ady << 1); }
        		break;
        	case 7:
        		xi++; t += (ady << 1);
        		if (t > 0) {yi--; t-= (adx << 1); }
        		break;
        	}
        	
        	// Figure out if the bresenham line from (x0,y0) to (x2,y2) is the
        	// best approximant we will see for the line from (x0,y0) to (xi,yi).
        	// We need:
        	//          T(i,2) < T(i,1) && T(i,2) <= T(i,3)
        	// Where T(a,b) is the angle between the two lines (x0,y0) - (xa,ya)
        	// and (x0,y0) - (xb,yb).
        	// We can compute an exact integer predicate; let C be the square
        	// of the cosine of T:
        	//      C(i,2) > C(i,1) && C(i,2) >= C(i,3)
        	// Use the identity:
        	//      cos(t) = a.b/|a||b|
        	// Square and cross-multiply to get rid of the divides and square
        	// roots.
        	// Note that we first check to see if T(i,2) == 0, in which case
        	// the line is a perfect approximant.
        	
        	final long dxi = xi - x0;
        	final long dyi = yi - y0;
        	final long dotii = dxi * dxi + dyi * dyi;
        	final long doti1 = dxi * dx1 + dyi * dy1;
        	final long doti2 = dxi * dx2 + dyi * dy2;
        	final long doti3 = dxi * dx3 + dyi * dy3;
        	
        	final boolean good = (doti2*doti2 == dotii*dot22) 
        			|| (dot11*doti2*doti2 > dot22*doti1*doti1 && dot33*doti2*doti2 >= dot22*doti3*doti3);
        	
        	// Otherwise accumulate the pb value if we've crossed an edge
        	double intersected = 0.0;
        	if (oldx == xi) {
        		if (yi > oldy) {
        			intersected = boundaries.H[xi][yi];
        		}
        		else if (yi < oldy) {
        			intersected = boundaries.H[oldx][oldy];
        		}
        	}
        	else if (oldy == yi) {
        		if (xi > oldx) {
        			intersected = boundaries.V[xi][yi];
        		}
        		else if (xi < oldx) {
        			intersected = boundaries.V[oldx][oldy];
        		}
        	}
        	else {
        		if ((xi > oldx) && (yi > oldy)) { // down to right
        			intersected = Math.max(boundaries.H[oldx][yi], intersected);
        			intersected = Math.max(boundaries.H[xi][yi], intersected);
        			intersected = Math.max(boundaries.V[xi][oldy], intersected);
        			intersected = Math.max(boundaries.V[xi][yi], intersected);
        		}
        		else if ((xi > oldx) && (yi < oldy)) { // up to right
        			intersected = Math.max(boundaries.H[oldx][oldy], intersected);
        			intersected = Math.max(boundaries.H[xi][oldy], intersected);
        			intersected = Math.max(boundaries.V[xi][oldy], intersected);
        			intersected = Math.max(boundaries.V[xi][yi], intersected);
        		}
        		else if ((xi < oldx) && (yi > oldy)) { // down to left
        			intersected = Math.max(boundaries.H[oldx][yi], intersected);
        			intersected = Math.max(boundaries.H[xi][yi], intersected);
        			intersected = Math.max(boundaries.V[oldx][oldy], intersected);
        			intersected = Math.max(boundaries.V[oldx][yi], intersected);
        		}
        		else if ((xi < oldx) && (yi < oldy)) { // up to left
        			intersected = Math.max(boundaries.H[oldx][oldy], intersected);
        			intersected = Math.max(boundaries.H[xi][oldy], intersected);
        			intersected = Math.max(boundaries.V[oldx][oldy], intersected);
        			intersected = Math.max(boundaries.V[oldx][yi], intersected);
        		}
        	}
        	maxpb = Math.max(maxpb,  intersected);
        	oldx = xi;
        	oldy = yi;
        	
        	// If the approximation is not good, then skip this point
        	if (!good) { continue; }
        	
        	// If the accumulated pb is too high, then stop
        	if (maxpb > thresh) {
        		break;
        	}
        	
        	// Record this connection
        	PointIC p = new PointIC();
        	p.x = xi;
        	p.y = yi;
        	p.sim = 1.0 - maxpb;
        	points[count] = p;
        	count++;
        }
        
        // Add our list of points to the scanLines; we have to reverse 
        // the order in octants 2,3,4,5
        switch (octant) {
        case 0:
        case 1:
        case 6:
        case 7:
        	for (int i = 0; i < count; i++) {
        		final int yind = points[i].y - y0 + wr;
        		scanLines[yind][scanCount[yind]++] = points[i];
        	}
        	break;
        case 2:
        case 3:
        case 4:
        case 5:
        	for (int i = count - 1; i >= 0; i--) {
        		final int yind = points[i].y - y0 + wr;
        		scanLines[yind][scanCount[yind]++] = points[i];
        	}
        	break;
        }
        return;
    }
    
    /**
     * Compute local contour cues of an image
     * gradients by Michael Maire <mmaire@eecs.berkeley.edu>
     * Original MATLAB code by Pablo Arbelaez <arbelaez@eecs.berkeley.edu> December, 2010
     * @param im
     */
    private void multiscalePb(double mPb_nmax[][], double mPb_nmax_rsz[][],
    		double [][][] bg1, double [][][] bg2, double[][][] bg3, double cga1[][][],
    		double cga2[][][], double cga3[][][], double cgb1[][][], double cgb2[][][],
    		double cgb3[][][], double tg1[][][], double tg2[][][], double tg3[][][], int[] textons, ModelImage im, boolean err[]) {
        double weights[] = new double[12];
        double buffer[];
        double red[] = new double[sliceSize];
        double green[] = new double[sliceSize];
        double blue[] = new double[sliceSize];
        int ini;
        int outi;
        int x;
        int y;
        int i;
        double l1;
        double l2;
        double l3;
        double a1;
        double a2;
        double a3;
        double b1;
        double b2;
        double b3;
        double t1;
        double t2;
        double t3;
        if (im.isColorImage()) {
        	buffer = new double[4*sliceSize];
        	try {
        	    im.exportData(0, 4* sliceSize, buffer);
        	}
        	catch (IOException e) {
        	    e.printStackTrace();
        	    err[0] = true;
        	    return;
        	}
        	im.disposeLocal();
        	im = null;
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			ini = x + y * xDim;
        			outi = y + x * yDim;
            	    red[outi] = buffer[4*ini+1];
            	    green[outi] = buffer[4*ini+2];
            	    blue[outi] = buffer[4*ini+3];
        		}
            }
            weights[0] = 0.0146;
            weights[1] = 0.0145;
            weights[2] = 0.0163;
            weights[3] = 0.0210;
            weights[4] = 0.0243;
            weights[5] = 0.0287;
            weights[6] = 0.0166;
            weights[7] = 0.0185;
            weights[8] = 0.0204;
            weights[9] = 0.0101;
            weights[10] = 0.0111;
            weights[11] = 0.0141;
        }
        else {
        	buffer = new double[sliceSize];
        	try {
        	    im.exportData(0, sliceSize, buffer);
        	}
        	catch (IOException e) {
        	    e.printStackTrace();
        	    err[0] = true;
        	    return;
        	}
        	im.disposeLocal();
        	im = null;
        	for (x = 0; x < xDim; x++) {
        		for (y = 0; y < yDim; y++) {
        			ini = x + y * xDim;
        			outi = y + x * yDim;
        			red[outi] = buffer[ini];
        			green[outi] = buffer[ini];
        			blue[outi] = buffer[ini];
        		}
        	}
        	weights[0] = 0.0245;
        	weights[1] = 0.0220;
        	weights[2] = 0.0233;
        	weights[3] = 0.0;
        	weights[4] = 0.0;
        	weights[5] = 0.0;
        	weights[6] = 0.0;
        	weights[7] = 0.0;
        	weights[8] = 0.0;
        	weights[9] = 0.0208;
        	weights[10] = 0.0210;
        	weights[11] = 0.0229;
        }
        
        // Get gradients
        det_mPb(bg1, bg2, bg3, cga1, cga2, cga3, cgb1, cgb2, cgb3, tg1, tg2, tg3, textons, red, green, blue);
        
        // Smooth cues
    	double gtheta[] = new double[]{1.5708, 1.1781, 0.7854, 0.3927, 0.0, 2.7489, 2.3562, 1.9635};
    	double radii[] = new double[]{3.0, 5.0, 10.0, 20.0};
    	double filters[][][][][] = new double[radii.length][gtheta.length][][][];
    	make_filters(filters, radii, gtheta);
    	for (i = 0; i < 8; i++) {
    	    bg1[i] = fitparab(bg1[i], 3.0, 0.75, gtheta[i], filters[0][i]);	
    	    bg2[i] = fitparab(bg2[i], 5.0, 1.25, gtheta[i], filters[1][i]);
    	    bg3[i] = fitparab(bg3[i], 10.0, 2.5, gtheta[i], filters[2][i]);
    	    
    	    cga1[i] = fitparab(cga1[i], 5.0, 1.25, gtheta[i], filters[1][i]);
    	    cga2[i] = fitparab(cga2[i], 10.0, 2.5, gtheta[i], filters[2][i]);
    	    cga3[i] = fitparab(cga3[i], 20.0, 5.0, gtheta[i], filters[3][i]);
    	    
    	    cgb1[i] = fitparab(cgb1[i], 5.0, 1.25, gtheta[i], filters[1][i]);
    	    cgb2[i] = fitparab(cgb2[i], 10.0, 2.5, gtheta[i], filters[2][i]);
    	    cgb3[i] = fitparab(cgb3[i], 20.0, 5.0, gtheta[i], filters[3][i]);
    	    
    	    tg1[i] = fitparab(tg1[i], 5.0, 1.25, gtheta[i], filters[1][i]);
    	    tg2[i] = fitparab(tg2[i], 10.0, 2.5, gtheta[i], filters[2][i]);
    	    tg3[i] = fitparab(tg3[i], 20.0, 5.0, gtheta[i], filters[3][i]);
    	} // for (i = 0; i < 8; i++)
    	
    	// Compute mPb at full scale
    	double mPb_all[][][] = new double[8][yDim][xDim];
    	for (i = 0; i < 8; i++) {
    	    for (x = 0; x < xDim; x++) {
    	    	for (y = 0; y < yDim; y++) {
    	    		l1 = weights[0] * bg1[i][y][x];
    	    		l2 = weights[1] * bg2[i][y][x];
    	    		l3 = weights[2] * bg3[i][y][x];
    	    		
    	    		a1 = weights[3] * cga1[i][y][x];
    	    		a2 = weights[4] * cga2[i][y][x];
    	    		a3 = weights[5] * cga3[i][y][x];
    	    		
    	    		b1 = weights[6] * cgb1[i][y][x];
    	    		b2 = weights[7] * cgb2[i][y][x];
    	    		b3 = weights[8] * cgb3[i][y][x];
    	    		
    	    		t1 = weights[9] * tg1[i][y][x];
    	    		t2 = weights[10] * tg2[i][y][x];
    	    		t3 = weights[11] * tg3[i][y][x];
    	    		
    	    		mPb_all[i][y][x] = l1 + a1 + b1 + t1 + l2 + a2 + b2 + t2 + l3 + a3 + b3 + t3;
    	    	}
    	    }
    	} // for (i = 0; i < 8; i++)
    	
    	// non-maximum suppression
    	nonmax_channels(mPb_nmax, mPb_all, Math.PI/8.0);
    	for (y = 0; y < mPb_nmax.length; y++) {
    		for (x = 0; x < mPb_nmax[0].length; x++) {
    		    mPb_nmax[y][x] = Math.max(0.0, Math.min(1.0, 1.2 * mPb_nmax[y][x]));
    		}
    	}
    	
    	// Compute mPb_nmax resized if necessary
    	double mPb_all_resized[][][] = new double[8][][];
    	if (rsz < 1) {
    		for (i = 0; i < 8; i++) {
    		    mPb_all_resized[i] = imresize(mPb_all[i], rsz);
    		}
    	    nonmax_channels(mPb_nmax_rsz, mPb_all_resized, Math.PI/8.0);
        	for (y = 0; y < mPb_nmax_rsz.length; y++) {
        		for (x = 0; x < mPb_nmax_rsz[0].length; x++) {
        		    mPb_nmax_rsz[y][x] = Math.max(0.0, Math.min(1.0, 1.2 * mPb_nmax_rsz[y][x]));
        		}
        	}
    	} // if (rsz < 1)
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			mPb_nmax_rsz[y][x] = mPb_nmax[y][x];	
    		}
    	}
    	
    }
    
    
    private double[][]imresize(double A[][], double scale) {
    	int i;
    	int j;
    	int p;
    	int cols[] = null;
    	double u[][];
    	double v[][];
    	int x;
    	int y;
    	double XN;
    	double C;
    	double C31;
    	double C32;
    	double FI;
    	if (scale >= 1) {
    		MipavUtil.displayError("Routine only for scale factor < 1");
    		return null;
    	}
    	int xDimA = A[0].length;
    	int yDimA = A.length;
    	// filter length in samples
    	int xDimB = (int)Math.floor(scale * xDimA);
    	int yDimB = (int)Math.floor(scale * yDimA);
    	double B[][] = new double[yDimB][xDimB];
    	// Create an antialiasing filter
    	// NF = filter length in samples
    	// Filter order = NF - 1
    	int NF = 11;
    	// N is half the length of the symmetric filter
    	int N = (NF+1)/2;
    	// Calculate cutoff frequencies
    	// Normalized cutoff frequencies go from 0.0 to 0.5
    	double FC1 = (0.5 * yDimB)/yDimA;
    	double FC2 = (0.5 * xDimB)/xDimA;
    	// IE0 is an even, odd indicator 
    	int IE0 = NF % 2;
    	// h1 and h2 will have the dimension 1 and dimension 2 impulse responses
    	// These are symmmetric responses
    	double h1[] = new double[N];
    	double h2[] = new double[N];
    	if (IE0 == 1) {
    		h1[0] = 2.0 * FC1;
    		h2[0] = 2.0 * FC2;
    	}
    	int I1 = IE0 + 1;
    	for (i = I1; i <= N; i++) {
    	    XN = i - 1;
    	    if (IE0 == 0) {
    	    	XN = XN + 0.5;
    	    }
    	    C = Math.PI * XN;
    	    C31 = 2.0 * C * FC1;
    	    C32 = 2.0 * C * FC2;
    	    h1[i-1] = Math.sin(C31)/C;
    	    h2[i-1] = Math.sin(C32)/C;
    	} // for (i = I1; i <= N; i++)
    	
    	double W[] = new double[N];
    	
    	// Hamming window
    	double alpha = 0.54;
    	double beta = 1.0 - alpha;
    	double FN = NF - 1.0;
    	for (i = 1; i <= N; i++) {
    	    FI = i - 1.0;
    	    if (IE0 == 0) {
    	    	FI = FI + 0.5;
    	    }
    	    W[i-1] = alpha + beta * Math.cos(2.0 * Math.PI * FI/FN);
    	}
    	for (i = 0; i < N; i++) {
    		h1[i] = h1[i] * W[i];
    		h2[i] = h2[i] * W[i];
    	}
    	double h2x[][] = new double[1][N];
    	for (i = 0; i < N; i++) {
    		h2x[0][i] = h2[i];
    	}
    	boolean isCropped = true;
    	boolean isStrict = false;
    	double ax[][] = compute_conv_2D(A, h2x, isCropped, isStrict);
    	double h1y[][] = new double[N][1];
    	for (i = 0; i < N; i++) {
    		h1y[i][0] = h1[i];
    	}
    	double a[][] = compute_conv_2D(ax, h1y, isCropped, isStrict);
    	double ain[] = new double[a.length * a[0].length];
    	for (y = 0; y < a.length; y++) {
    	    for (x = 0; x < a[0].length; x++) {
    	    	ain[x + y*a[0].length] = a[y][x];
    	    }
    	}
    	AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
    	boolean doClip = true;
    	int inExtents[] = new int[]{a[0].length, a.length};
        CLag.setup2DCubicLagrangian(ain, inExtents, doClip);
    	double uu[] = new double[xDimB];
    	for (i = 0; i < xDimB; i++) {
    		uu[i] = i*(xDimA - 1.0)/(xDimB - 1.0);
    	}
    	double vv[] = new double[yDimB];
    	for (i = 0; i < yDimB; i++) {
    		vv[i] = i*(yDimA - 1.0)/(yDimB - 1.0);
    	}
    	
    	// Interpolate in blocks
    	int insize[] = new int[]{yDimB, xDimB};
    	int blk[] = bestblk(insize, 100);
    	int nblks[] = new int[2];
    	nblks[0] = yDimB/blk[0];
    	nblks[1] = xDimB/blk[1];
    	int nrem[] = new int[2];
    	nrem[0] = yDimB - nblks[0]*blk[0];
    	nrem[1] = xDimB - nblks[1]*blk[1];
    	int mblocks = nblks[0];
    	int nblocks = nblks[1];
    	int mb = blk[0];
    	int nb = blk[1];
    	int rows[] = new int[blk[0]];
    	for (i = 0; i < blk[0]; i++) {
    		rows[i] = i;
    	}
    	for (i = 0; i <= mblocks; i++) {
    	    if (i == mblocks) {
    	    	rows = null;
    	    	rows = new int[nrem[0]];
    	    	for (p = 0; p < nrem[0]; p++) {
    	    		rows[p] = p;
    	    	}
    	    } // if (i == mblocks)
    	    for (j = 0; j <= nblocks; j++) {
    	        if (j == 0) {
    	        	cols = new int[blk[1]];
    	        	for (p = 0; p < blk[1]; p++) {
    	        		cols[p] = p;
    	        	}
    	        } // if (j == 0)
    	        else if (j == nblocks) {
    	        	cols = new int[nrem[1]];
    	        	for (p = 0; p < nrem[1]; p++) {
    	        		cols[p] = p;
    	        	}
    	        } // else if (j == nblocks)
    	        if ((rows.length != 0) && (cols.length != 0)) {
    	            u = new double[rows.length][cols.length];
    	            v = new double[rows.length][cols.length];
    	            for (y = 0; y < rows.length; y++) {
    	            	for (x = 0; x < cols.length; x++) {
    	            		u[y][x] = uu[j*nb + cols[x]];
    	            		v[y][x] = vv[i*mb + rows[y]];
    	            	}
    	            }
    	            // Cubic Lagrangian interpolation of points instead of bicubic interpolation in original
    	            for (y = 0; y < rows.length; y++) {
    	            	for (x = 0; x < cols.length; x++) {
    	            		B[i*mb + rows[y]][j*nb + cols[x]] = CLag.cubicLagrangian2D(u[y][x], v[y][x]);	
    	            		//B[i*mb + rows[y]][j*nb + cols[x]] = interp2(a, u[y][x], v[y][x], 'cubic');
    	            	}
    	            }
    	        } // if ((rows.length != 0) && (cols.length != 0))
    	    } // for (j = 0; j <= nblocks; j++)
    	} // for (i = 0; i <= mblocks; i++)
    	return B;
    }
    
    private  void imresize(double A[][], double B[][]) {
    	int i;
    	int j;
    	int p;
    	int cols[] = null;
    	double u[][];
    	double v[][];
    	int x;
    	int y;
    	double XN;
    	double C;
    	double C31;
    	double C32;
    	double FI;
    	
    	int xDimA = A[0].length;
    	int yDimA = A.length;
    	// filter length in samples
    	int xDimB = B[0].length;
    	int yDimB = B.length;
    	// Create an antialiasing filter if xDimB < xDimA and yDimB < yDimA.
    	boolean antialias = false;
    	if ((xDimB < xDimA) && (yDimB < yDimA)) {
    		antialias = true;
    	}
    	double ain[];
    	int inExtents[];
    	if (antialias) {
	    	// NF = filter length in samples
	    	// Filter order = NF - 1
	    	int NF = 11;
	    	// N is half the length of the symmetric filter
	    	int N = (NF+1)/2;
	    	// Calculate cutoff frequencies
	    	// Normalized cutoff frequencies go from 0.0 to 0.5
	    	double FC1 = (0.5 * yDimB)/yDimA;
	    	double FC2 = (0.5 * xDimB)/xDimA;
	    	// IE0 is an even, odd indicator 
	    	int IE0 = NF % 2;
	    	// h1 and h2 will have the dimension 1 and dimension 2 impulse responses
	    	// These are symmmetric responses
	    	double h1[] = new double[N];
	    	double h2[] = new double[N];
	    	if (IE0 == 1) {
	    		h1[0] = 2.0 * FC1;
	    		h2[0] = 2.0 * FC2;
	    	}
	    	int I1 = IE0 + 1;
	    	for (i = I1; i <= N; i++) {
	    	    XN = i - 1;
	    	    if (IE0 == 0) {
	    	    	XN = XN + 0.5;
	    	    }
	    	    C = Math.PI * XN;
	    	    C31 = 2.0 * C * FC1;
	    	    C32 = 2.0 * C * FC2;
	    	    h1[i-1] = Math.sin(C31)/C;
	    	    h2[i-1] = Math.sin(C32)/C;
	    	} // for (i = I1; i <= N; i++)
	    	
	    	double W[] = new double[N];
	    	
	    	// Hamming window
	    	double alpha = 0.54;
	    	double beta = 1.0 - alpha;
	    	double FN = NF - 1.0;
	    	for (i = 1; i <= N; i++) {
	    	    FI = i - 1.0;
	    	    if (IE0 == 0) {
	    	    	FI = FI + 0.5;
	    	    }
	    	    W[i-1] = alpha + beta * Math.cos(2.0 * Math.PI * FI/FN);
	    	}
	    	for (i = 0; i < N; i++) {
	    		h1[i] = h1[i] * W[i];
	    		h2[i] = h2[i] * W[i];
	    	}
	    	double h2x[][] = new double[1][N];
	    	for (i = 0; i < N; i++) {
	    		h2x[0][i] = h2[i];
	    	}
	    	boolean isCropped = true;
	    	boolean isStrict = false;
	    	double ax[][] = compute_conv_2D(A, h2x, isCropped, isStrict);
	    	double h1y[][] = new double[N][1];
	    	for (i = 0; i < N; i++) {
	    		h1y[i][0] = h1[i];
	    	}
	    	double a[][] = compute_conv_2D(ax, h1y, isCropped, isStrict);
	    	ain = new double[a.length * a[0].length];
	    	for (y = 0; y < a.length; y++) {
	    	    for (x = 0; x < a[0].length; x++) {
	    	    	ain[x + y*a[0].length] = a[y][x];
	    	    }
	    	}
	    	inExtents = new int[]{a[0].length, a.length};
    	} // if (antalias)
    	else {
    		ain = new double[A.length * A[0].length];
    		for (y = 0; y < A.length; y++) {
	    	    for (x = 0; x < A[0].length; x++) {
	    	    	ain[x + y*A[0].length] = A[y][x];
	    	    }
	    	}
    		inExtents = new int[]{A[0].length, A.length};
    	}
    	AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
    	boolean doClip = true;
    	
        CLag.setup2DCubicLagrangian(ain, inExtents, doClip);
    	double uu[] = new double[xDimB];
    	for (i = 0; i < xDimB; i++) {
    		uu[i] = i*(xDimA - 1.0)/(xDimB - 1.0);
    	}
    	double vv[] = new double[yDimB];
    	for (i = 0; i < yDimB; i++) {
    		vv[i] = i*(yDimA - 1.0)/(yDimB - 1.0);
    	}
    	
    	// Interpolate in blocks
    	int insize[] = new int[]{yDimB, xDimB};
    	int blk[] = bestblk(insize, 100);
    	int nblks[] = new int[2];
    	nblks[0] = yDimB/blk[0];
    	nblks[1] = xDimB/blk[1];
    	int nrem[] = new int[2];
    	nrem[0] = yDimB - nblks[0]*blk[0];
    	nrem[1] = xDimB - nblks[1]*blk[1];
    	int mblocks = nblks[0];
    	int nblocks = nblks[1];
    	int mb = blk[0];
    	int nb = blk[1];
    	int rows[] = new int[blk[0]];
    	for (i = 0; i < blk[0]; i++) {
    		rows[i] = i;
    	}
    	for (i = 0; i <= mblocks; i++) {
    	    if (i == mblocks) {
    	    	rows = null;
    	    	rows = new int[nrem[0]];
    	    	for (p = 0; p < nrem[0]; p++) {
    	    		rows[p] = p;
    	    	}
    	    } // if (i == mblocks)
    	    for (j = 0; j <= nblocks; j++) {
    	        if (j == 0) {
    	        	cols = new int[blk[1]];
    	        	for (p = 0; p < blk[1]; p++) {
    	        		cols[p] = p;
    	        	}
    	        } // if (j == 0)
    	        else if (j == nblocks) {
    	        	cols = new int[nrem[1]];
    	        	for (p = 0; p < nrem[1]; p++) {
    	        		cols[p] = p;
    	        	}
    	        } // else if (j == nblocks)
    	        if ((rows.length != 0) && (cols.length != 0)) {
    	            u = new double[rows.length][cols.length];
    	            v = new double[rows.length][cols.length];
    	            for (y = 0; y < rows.length; y++) {
    	            	for (x = 0; x < cols.length; x++) {
    	            		u[y][x] = uu[j*nb + cols[x]];
    	            		v[y][x] = vv[i*mb + rows[y]];
    	            	}
    	            }
    	            // Cubic Lagrangian interpolation of points instead of bicubic interpolation in original
    	            for (y = 0; y < rows.length; y++) {
    	            	for (x = 0; x < cols.length; x++) {
    	            		B[i*mb + rows[y]][j*nb + cols[x]] = CLag.cubicLagrangian2D(u[y][x], v[y][x]);	
    	            		//B[i*mb + rows[y]][j*nb + cols[x]] = interp2(a, u[y][x], v[y][x], 'cubic');
    	            	}
    	            }
    	        } // if ((rows.length != 0) && (cols.length != 0))
    	    } // for (j = 0; j <= nblocks; j++)
    	} // for (i = 0; i <= mblocks; i++)
    	return;
    }
    
    private int[] bestblk(int siz[], int k) {
    	int i;
    	// Define acceptableblock sizes
    	int minm = (int)Math.floor(Math.min(Math.ceil(siz[0]/10.0), k/2.0));
    	int m[] = new int[k - minm + 1];
    	for (i = 0; i < m.length; i++) {
    		m[i] = k - i;
    	}
    	int minn = (int)Math.floor(Math.min(Math.ceil(siz[1]/10.0), k/2.0));
    	int n[] = new int[k - minn + 1];
    	for (i = 0; i < n.length; i++) {
    		n[i] = k - i;
    	}
    	// Choose the largest acceptable block that has the minimum padding
    	int blk[] = new int[2];
    	double mVal[] = new double[m.length];
    	double minVal = Double.MAX_VALUE;
    	for (i = 0; i < m.length; i++) {
    		mVal[i] = Math.ceil(siz[0]/m[i])*m[i]-siz[0];
    		if (mVal[i] < minVal) {
    			minVal = mVal[i];
    			blk[0] = m[i];
    		}
    	}
    	double nVal[] = new double[n.length];
        minVal = Double.MAX_VALUE;
    	for (i = 0; i < n.length; i++) {
    		nVal[i] = Math.ceil(siz[1]/n[i])*n[i]-siz[1];
    		if (nVal[i] < minVal) {
    			minVal = nVal[i];
    			blk[1] = n[i];
    		}
    	}
    	return blk;
    }
    
    // Given NxMxnum_ori oriented channels, compute oriented nonmax suppression
    private void nonmax_channels(double nmax2D[][], double pb[][][], double nonmax_ori_tol) {
	    //if (nargin < 2), nonmax_ori_tol = pi/8; end
    	int y_size = nmax2D.length;
    	int x_size = nmax2D[0].length;
	    int n_ori = pb.length;
        double oris[] = new double[n_ori];
        int n;
        for (n = 0; n < n_ori; n++) {
        	oris[n] = (n * Math.PI)/n_ori;
        }
        double pbmax[] = new double[x_size * y_size];
        double maxAngle[] = new double[x_size * y_size];
        int index;
        int i;
        int x;
        int y;
        for (x = 0; x < x_size; x++) {
        	for (y = 0; y < y_size; y++) {
        	    pbmax[x*y_size + y] = -Double.MAX_VALUE;
        	    index = -1;
        	    for (i = 0; i < n_ori; i++) {
        	    	if (pb[i][y][x] > pbmax[x*y_size + y]) {
        	    		pbmax[x*y_size + y] = pb[i][y][x];
        	    		index = i;
        	    	}
        	    } // for (i = 0; i < n_ori; i++)
        	    maxAngle[x*y_size + y] = oris[index];
        	    if (pbmax[x*y_size + y] < 0.0) {
        	    	pbmax[x*y_size + y] = 0.0;
        	    }
        	} // for (y = 0; y < y_size; y++)
        } // for (x = 0; x < x_size; x++)
        boolean allow_boundary = false;
	    nonmax_oriented_2D(nmax2D, pbmax, maxAngle, nonmax_ori_tol, allow_boundary);
	    return;
    }
    
    /*
     * Oriented non-max suppression (2D).
     *
     * Perform non-max suppression orthogonal to the specified orientation on
     * the given 2D matrix using linear interpolation in a 3x3 neighborhood.
     *
     * A local maximum must be greater than the interpolated values of its 
     * adjacent elements along the direction orthogonal to this orientation.
     *
     * Elements which have a neighbor on only one side of this direction are 
     * only considered as candidates for a local maximum if the flag to allow 
     * boundary candidates is set.
     *
     * The same orientation angle may be specified for all elements, or the 
     * orientation may be specified for each matrix element.
     *
     * If an orientation is specified per element, then the elements themselves
     * may optionally be treated as oriented vectors by specifying a value less 
     * than pi/2 for the orientation tolerance.  In this case, neighboring 
     * vectors are projected along a line in the local orientation direction and
     * the lengths of the projections are used in determining local maxima.
     * When projecting, the orientation tolerance is subtracted from the true
     * angle between the vector and the line (with a result less than zero
     * causing the length of the projection to be the length of the vector).
     *
     * Non-max elements are assigned a value of zero.
     *
     * NOTE: The original matrix must be nonnegative.
     */

    private void nonmax_oriented_2D(double nmax2D[][],
       final double m[], final double m_ori[], double o_tol, boolean allow_boundary)
    {
       int x;
       int y;
       int size_y = nmax2D.length;
       int size_x = nmax2D[0].length;
       /* intialize result matrix */
       double nmax[] = new double[size_x *  size_y];
       /* perform oriented non-max suppression at each element */
       int n = 0;
       for (x = 0; x < size_x; x++) {
          for (y = 0; y < size_y; y++) {
             /* compute direction (in [0,pi)) along which to suppress */
             double ori = m_ori[n];
             double theta = ori + Math.PI/2.0;
             theta -= Math.floor(theta/Math.PI) * Math.PI;
             /* check nonnegativity */
             double v = m[n];
             if (v < 0) {
                MipavUtil.displayError("matrix must be nonnegative in nonmax_orientd_2D");
                return;
             }
             /* initialize indices of values in local neighborhood */
             int ind0a = 0, ind0b = 0, ind1a = 0, ind1b = 0;
             /* initialize distance weighting */
             double d = 0;
             /* initialize boundary flags */
             boolean valid0 = false, valid1 = false;
             /* compute interpolation indicies */
             if (theta == 0) {
                valid0 = (x > 0); valid1 = (x < (size_x-1));
                if (valid0) { ind0a = n-size_y; ind0b = ind0a; }
                if (valid1) { ind1a = n+size_y; ind1b = ind1a; }
             } else if (theta < Math.PI/4.0) {
                d = Math.tan(theta);
                valid0 = ((x > 0) && (y > 0));
                valid1 = ((x < (size_x-1)) && (y < (size_y-1)));
                if (valid0) { ind0a = n-size_y; ind0b = ind0a-1; }
                if (valid1) { ind1a = n+size_y; ind1b = ind1a+1; }
             } else if (theta < Math.PI/2.0) {
                d = Math.tan(Math.PI/2.0 - theta);
                valid0 = ((x > 0) && (y > 0));
                valid1 = ((x < (size_x-1)) && (y < (size_y-1)));
                if (valid0) { ind0a = n-1; ind0b = ind0a-size_y; }
                if (valid1) { ind1a = n+1; ind1b = ind1a+size_y; }
             } else if (theta == Math.PI/2.0) {
                valid0 = (y > 0); valid1 = (y < (size_y-1));
                if (valid0) { ind0a = n-1; ind0b = ind0a; }
                if (valid1) { ind1a = n+1; ind1b = ind1a; }
             } else if (theta < (3.0*Math.PI/4.0)) {
                d = Math.tan(theta - Math.PI/2.0);
                valid0 = ((x < (size_x-1)) && (y > 0));
                valid1 = ((x > 0) && (y < (size_y-1)));
                if (valid0) { ind0a = n-1; ind0b = ind0a+size_y; }
                if (valid1) { ind1a = n+1; ind1b = ind1a-size_y; }
             } else /* (theta < Math.PI) */ {
                d = Math.tan(Math.PI - theta);
                valid0 = ((x < (size_x-1)) && (y > 0));
                valid1 = ((x > 0) && (y < (size_y-1)));
                if (valid0) { ind0a = n+size_y; ind0b = ind0a-1; }
                if (valid1) { ind1a = n-size_y; ind1b = ind1a+1; }
             }
             /* check boundary conditions */
             if (allow_boundary || (valid0 && valid1)) {
                /* initialize values in local neighborhood */
                double v0a = 0,   v0b = 0,   v1a = 0,   v1b = 0;
                /* initialize orientations in local neighborhood */
                double ori0a = 0, ori0b = 0, ori1a = 0, ori1b = 0;
                /* grab values and orientations */
                if (valid0) {
                   v0a = m[ind0a];
                   v0b = m[ind0b];
                   ori0a = m_ori[ind0a] - ori;
                   ori0b = m_ori[ind0b] - ori;
                }
                if (valid1) {
                   v1a = m[ind1a];
                   v1b = m[ind1b];
                   ori1a = m_ori[ind1a] - ori;
                   ori1b = m_ori[ind1b] - ori;
                }
                /* place orientation difference in [0,pi/2) range */
                ori0a -= Math.floor(ori0a/(2*Math.PI)) * (2*Math.PI);
                ori0b -= Math.floor(ori0b/(2*Math.PI)) * (2*Math.PI);
                ori1a -= Math.floor(ori1a/(2*Math.PI)) * (2*Math.PI);
                ori1b -= Math.floor(ori1b/(2*Math.PI)) * (2*Math.PI);
                if (ori0a >= Math.PI) { ori0a = 2*Math.PI - ori0a; }
                if (ori0b >= Math.PI) { ori0b = 2*Math.PI - ori0b; }
                if (ori1a >= Math.PI) { ori1a = 2*Math.PI - ori1a; }
                if (ori1b >= Math.PI) { ori1b = 2*Math.PI - ori1b; }
                if (ori0a >= Math.PI/2.0) { ori0a = Math.PI - ori0a; }
                if (ori0b >= Math.PI/2.0) { ori0b = Math.PI - ori0b; }
                if (ori1a >= Math.PI/2.0) { ori1a = Math.PI - ori1a; }
                if (ori1b >= Math.PI/2.0) { ori1b = Math.PI - ori1b; }
                /* correct orientation difference by tolerance */
                ori0a = (ori0a <= o_tol) ? 0 : (ori0a - o_tol);
                ori0b = (ori0b <= o_tol) ? 0 : (ori0b - o_tol);
                ori1a = (ori1a <= o_tol) ? 0 : (ori1a - o_tol);
                ori1b = (ori1b <= o_tol) ? 0 : (ori1b - o_tol);
                /* interpolate */
                double v0 =
                   (1.0-d)*v0a*Math.cos(ori0a) + d*v0b*Math.cos(ori0b);
                double v1 =
                   (1.0-d)*v1a*Math.cos(ori1a) + d*v1b*Math.cos(ori1b);
                /* suppress non-max */
                if ((v > v0) && (v > v1))
                   nmax[n] = v;
             }
             /* increment linear coordinate */
             n++;
          }
       }
       for (x = 0, n = 0; x < size_x; x++) {
           for (y = 0; y < size_y; y++, n++) {
        	   nmax2D[y][x] = nmax[n];
           }
       }
       return;
    }



    
    /**
     * Compute image gradients.  Implementation by Michael Maire.
     * @param im
     */
    private void det_mPb(double bg1[][][], double bg2[][][], double bg3[][][], double cga1[][][],
    		double cga2[][][], double cga3[][][], double cgb1[][][],
    		double cgb2[][][], double cgb3[][][], double tg1[][][], double tg2[][][],
    		double tg3[][][], int textons[], 
    		double red[], double green[], double blue[]) {
        // Compute pb
    	mex_pb_parts_final_selected(bg1, bg2, bg3, cga1, cga2, cga3, cgb1, cgb3, cgb3, tg1, tg2, tg3, textons, red, green, blue);
    }
    
    private double[][] fitparab(double z[][], double ra, double rb, double theta, double filt[][][]) {
    		// function [a,b,c] = fitparab(z,ra,rb,theta)
    		//
    		// Fit cylindrical parabolas to elliptical patches of z at each
    		// pixel.  
    		//
    		// INPUT
    		//	z	Values to fit.
    		//	ra,rb	Radius of elliptical neighborhood, ra=major axis.
    		//	theta	Orientation of fit (i.e. of minor axis).
    		//
    		// OUTPUT
    		//	a,b,c	Coefficients of fit: a + bx + cx^2
    		//


    		// compute the interior quickly with convolutions
    	    int x; 
    	    int y;
    	    double a[][];
    	    double filt2D[][] = new double[filt.length][filt[0].length];
    	    for (x = 0; x < filt.length; x++) {
    	    	for (y = 0; y < filt[0].length; y++) {
    	    		filt2D[x][y] = filt[x][y][0];
    	    	}
    	    }
    	    boolean isCropped = true;
    	    boolean isStrict = false;
    	    a = compute_conv_2D(z, filt2D, isCropped, isStrict);
    		//fix border with mex file
    	    double aout[] = new double[a.length * a[0].length];
    	    double ain[] = new double[a.length * a[0].length];
    	    for (x = 0; x < a.length; x++) {
    	    	for (y = 0; y < a[0].length; y++) {
    	    		ain[x * a[0].length + y] = a[x][y];
    	    	}
    	    }
    	    double zin[] = new double[z.length * z[0].length];
    	    for (x = 0; x < z.length; x++) {
    	    	for (y = 0; y < z[0].length; y++) {
    	    		zin[x * z[0].length + y] = z[x][y];
    	    	}
    	    }
    	    savgol(ain, zin, ra, rb, theta, a.length, a[0].length, aout);
    	    for (x = 0; x < a.length; x++) {
    	    	for (y = 0; y < a[0].length; y++) {
    	    		a[x][y] = aout[x * a[0].length + y];
    	    	}	
    	    }
    	    return a;
    }
    
    private void  savgol(double a_in[], double z[], double ra, double rb, final double theta, 
    		final int h, final int w, double[] a_out) {
        
        ra = Math.max(1.5, ra);
        rb = Math.max(1.5, rb);
        final double ira2 = 1 / Math.pow(ra, 2);
        final double irb2 = 1 / Math.pow(rb, 2);
        final int wr = (int) Math.floor(Math.max(ra, rb));
        final double sint = Math.sin(theta);
        final double cost = Math.cos(theta);
        double d0, d1, d2, d3, d4, v0, v1, v2;
        int xi, yi, x, y, cpi;
        double di, ei, zi, di2, detA;
        final double eps = Math.exp(-300);
        
        for (int cp = 0; cp<(w*h); cp++){
            y = cp%h; x=cp/h;
            if ((x>=wr) && (x<(w-wr)) && (y>=wr) && (y<(h-wr))){
                a_out[cp] = a_in[cp];
            }
            else{
                
                d0=0; d1=0; d2=0; d3=0; d4=0;
                v0=0; v1=0; v2=0;
                for ( int u = -wr; u <= wr; u++ ){
                    xi = x + u;
                    if ((xi<0) || (xi>=w))
                        continue;
                    for ( int v = -wr; v <= wr; v++ ){
                        yi = y + v;
                        if ((yi<0) || (yi>=h))
                            continue;
                        di = -u*sint + v*cost;
                        ei = u*cost + v*sint;
                        if ( (di*di*ira2 + ei*ei*irb2) > 1)
                            continue;
                        cpi = yi+xi*h;
                        zi = z[cpi];
                        di2 = di*di;
                        d0 = d0 + 1;
                        d1 = d1 + di;
                        d2 = d2 + di2;
                        d3 = d3 + di*di2;
                        d4 = d4 + di2*di2;
                        v0 = v0 + zi;
                        v1 = v1 + zi*di;
                        v2 = v2 + zi*di2;
                    }
                }
                
                detA = -d2*d2*d2 + 2*d1*d2*d3 - d0*d3*d3 - d1*d1*d4 + d0*d2*d4;
                if (detA>eps)
                    a_out[cp] = ((-d3*d3+d2*d4)*v0 + (d2*d3-d1*d4)*v1 + (-d2*d2+d1*d3)*v2)/ detA;
                
            }
        }
        
    }

    
    private void make_filters(double filters[][][][][], double radii[], double gtheta[]) {
    	int r;
    	int t;
    	double ra;
    	double rb;
    	double theta;
    	double ira2;
    	double irb2;
    	int wr;
    	int wd;
    	double sint;
    	double cost;
    	double xx[];
    	int u;
    	int v;
    	double ai;
    	double bi;
    	double A[][];
    	double yy[][];
    	int i;
    	int j;
    	Matrix matA;
    	Matrix matyy;
    	double prod[][];
    	int d = 2;
    	for (r = 0; r < radii.length; r++) {
    	    for (t = 0; t < gtheta.length; t++) {
    	        ra = radii[r];
    	        rb = ra/4.0;
    	        theta = gtheta[t];
    	        
    	        ra = Math.max(1.5, ra);
    	    	rb = Math.max(1.5, rb);
    	    	ira2 = 1.0/(ra*ra);
    	    	irb2 = 1.0/(rb*rb);
    	    	wr = (int)Math.floor(Math.max(ra,rb));
    	    	wd = 2*wr+1;
    	    	sint = Math.sin(theta);
    	    	cost = Math.cos(theta);
    	    	
    	    	// 1. Compute linear filters for coefficients
    	    	// (a) Compute inverse of least-squares problem matrix
    	    	filters[r][t] = new double[wd][wd][d+1];
    	    	xx = new double[2*d+1];
    	    	for (u = -wr; u <= wr; u++) {
    	    		for (v = -wr; v <= wr; v++) {
    	    			// Distance along major axis
    	    			ai = -u*sint + v*cost;
    	    			// Distance along minor axis
    	    			bi = u*cost + v*sint;
    	    			if (ai*ai*ira2 + bi*bi*irb2 > 1) {
    	    				continue;
    	    			}
    	    			xx[0] = xx[0] + 1;
    	    			for (i = 1; i <= 2*d; i++) {
    	    			    xx[i] = xx[i] + Math.pow(ai,i);	
    	    			}
    	    		}
    	    	}
    	    	A = new double[d+1][d+1];
    	    	for (i = 0; i <= d; i++) {
    	    		for (j = i; j <= i+d; j++) {
    	    		    A[j-i][i] = xx[j];	
    	    		}
    	    	}
    	    	matA = new Matrix(A);
    	    	A = (matA.inverse()).getArray();
    	    	matA = new Matrix(A);
    	    	// (b) solve least-squares problem for delta function at each pixel
    	    	for (u = -wr; u <= wr; u++) {
    	    		for (v = -wr; v <= wr; v++) {
    	    		    yy = new double[d+1][1];
    	    		    // Distance along major axis
    	    		    ai = -u*sint + v*cost;
    	    		    // Distance along minor axis
    	    		    bi = u*cost + v*sint;
    	    		    if (ai*ai*ira2 + bi*bi*irb2 > 1) {
    	    		    	continue;
    	    		    }
    			    	yy[0][0] = 1;
    			    	for (i = 1; i <= d; i++) {
    			    		yy[i][0] = Math.pow(ai,i); 
    			    	}
    			    	matyy = new Matrix(yy);
    			    	prod = (matA.times(matyy)).getArray();
    			    	for (i = 0; i < d+1; i++) {
    			    		filters[r][t][v+wr][u+wr][i] = prod[i][0];
    			    	}
    	    		}
    	    	}
    	    } // for (t = 0; t < gtheta.length; t++)
    	} // for (r = 0; r < radii.length; r++)
    }
    
    private void mex_pb_parts_final_selected(double bg1[][][], double bg2[][][], double bg3[][][],double cga1[][][],
    		double cga2[][][], double cga3[][][], double cgb1[][][], double cgb2[][][],
    		double cgb3[][][], double tg1[][][], double tg2[][][], double tg3[][][], int textons[], 
    		double L[], double a[], double b[]) {
    	int i;
    	int x;
    	int y;
    	// Quantizing color channels
    	// # bins for bg
    	int numLBins = 25;
    	// # bins for cg_a
    	int numaBins = 25;
    	// # bins for cg_b
    	int numbBins = 25;
    	// bg histogram smoothing sigma
    	double bg_smooth_sigma = 0.1;
    	double bg_smooth_kernel[] = gaussian(bg_smooth_sigma*numLBins, 0, false);
    	// cg histogram smoothing sigma
    	double cg_smooth_sigma = 0.05;
    	double cga_smooth_kernel[] = gaussian(cg_smooth_sigma*numaBins, 0, false);
    	double cgb_smooth_kernel[] = gaussian(cg_smooth_sigma*numbBins, 0, false);
    	// Border pixels
    	int border = 30;
    	// Mirror border
    	int dstXDim = xDim + 2 * border;
    	int dstYDim = yDim + 2 * border;
    	double L_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(L, L_border, border, border, xDim, yDim);
    	double a_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(a, a_border, border, border, xDim, yDim);
    	double b_border[] = new double[dstXDim * dstYDim];
    	compute_border_mirror_2D(b, b_border, border, border, xDim, yDim);
    	// Converting RGB to grayscale
    	double gray[] = new double[L_border.length];
    	grayscale(gray, L_border, a_border, b_border);
    	// Gamma correct
    	rgb_gamma_correct(L_border, a_border, b_border, 2.5);
    	// Converting RGB to Lab
    	rgb_to_lab(L_border, a_border, b_border);
    	lab_normalize(L_border, a_border, b_border);
    	int Lq[] = new int[L_border.length];
    	quantize_values(Lq, L_border, numLBins);
    	int aq[] = new int[a_border.length];
    	quantize_values(aq, a_border, numaBins);
    	int bq[] = new int[b_border.length];
    	quantize_values(bq, b_border, numbBins);
    	// Computing filter set for textons
    	// Number of orientations
    	int n_ori = 8;
    	// sigma for small tg filters
    	double sigma_tg_filt_sm = 2.0;
    	ArrayList <double[][]> filters_small = new ArrayList<double[][]>();
    	texton_filters(filters_small, n_ori, sigma_tg_filt_sm);
    	// Sigma for large tg filters
    	double sigma_tg_filt_lg = 2.0 * Math.sqrt(2.0);
    	ArrayList <double[][]> filters_large = new ArrayList<double[][]>();
    	texton_filters(filters_large, n_ori, sigma_tg_filt_lg);
    	ArrayList <double[][]> filters = new ArrayList<double[][]>();
    	for (i = 0; i < filters_small.size(); i++) {
    		filters.add(filters_small.get(i));
    	}
    	filters_small.clear();
    	for (i = 0; i < filters_large.size(); i++) {
    		filters.add(filters_large.get(i));
    	}
    	filters_large.clear();
    	// Compute textons
    	ArrayList<double[]>textonsAL = new ArrayList<double[]>();
    	int iterations = 10;
    	double subsampling = 0.10;
    	double gray2D[][] = new double[dstXDim][dstYDim];
    	for (x = 0; x < dstXDim; x++) {
    		for (y = 0; y < dstYDim; y++) {
    			gray2D[x][y] = gray[y + x * dstYDim];
    		}
    	}
    	int t_assign[] = textons_routine(gray2D, filters, textonsAL, 64, iterations, subsampling);
    	int t_assign_trim[] = new int[xDim*yDim];
    	compute_border_trim_2D(t_assign, t_assign_trim, border, border, xDim, yDim, dstYDim);
    	// Return textons to globalPb
    	for (x = 0; x < xDim; x++) {
    		for (y = 0; y < yDim; y++) {
    			textons[x + y * xDim] = t_assign_trim[y + x * yDim];
    		}
    	}
    	compute_border_mirror_2D(t_assign_trim, t_assign, border, border, xDim, yDim);
    	// Compute bg at each radius
    	int n_bg = 3;
    	int r_bg[] = new int[]{3, 5, 10};
    	ArrayList<double[]> bgs;
    	double bufm[] = new double[xDim * yDim];
    	for (int rnum = 0; rnum < n_bg; rnum++) {
    	    Preferences.debug("Computing bg for r = " + r_bg[rnum] + "\n", Preferences.DEBUG_ALGORITHM);
    	    bgs = hist_gradient_2D(Lq, dstXDim, dstYDim, r_bg[rnum], n_ori, bg_smooth_kernel);
    	    // Return bg
    	    if (rnum == 0) {
	    	    for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(bgs.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			bg1[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }
    	    } // if (rnum == 0)
    	    else if (rnum == 1) {
    	    	for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(bgs.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			bg2[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }	
    	    } // else if (rnum == 1)
    	    else if (rnum == 2) {
    	    	for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(bgs.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			bg3[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }		
    	    } // else if (rnum == 2)
    	} // for (int rnum = 0; rnum < n_bg; rnum++)
    	// Compute cga at each radius
    	int n_cg = 3;
    	int r_cg[] = new int[]{5, 10, 20};
    	ArrayList<double[]>cgs_a;
    	for (int rnum = 0; rnum < n_cg; rnum++) {
    		Preferences.debug("Computing cga for r = " + r_cg[rnum] + "\n", Preferences.DEBUG_ALGORITHM);
    		cgs_a = hist_gradient_2D(aq, dstXDim, dstYDim, r_cg[rnum], n_ori, cga_smooth_kernel);
    		// Return cga
    		if (rnum == 0) {
	    	    for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(cgs_a.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			cga1[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }
    	    } // if (rnum == 0)
    	    else if (rnum == 1) {
    	    	for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(cgs_a.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			cga2[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }	
    	    } // else if (rnum == 1)
    	    else if (rnum == 2) {
    	    	for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(cgs_a.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			cga3[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }		
    	    } // else if (rnum == 2)
    	} // for (int rnum = 0; rnum < n_cg; rnum++)
    	// Compute cgb at each radius
    	ArrayList<double[]>cgs_b;
    	for (int rnum = 0; rnum < n_cg; rnum++) {
    		Preferences.debug("Computing cgb for r = " + r_cg[rnum] + "\n", Preferences.DEBUG_ALGORITHM);
    		cgs_b = hist_gradient_2D(bq, dstXDim, dstYDim, r_cg[rnum], n_ori, cgb_smooth_kernel);
    		// Return cgb
    		if (rnum == 0) {
	    	    for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(cgs_b.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			cgb1[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }
    	    } // if (rnum == 0)
    	    else if (rnum == 1) {
    	    	for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(cgs_b.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			cgb2[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }	
    	    } // else if (rnum == 1)
    	    else if (rnum == 2) {
    	    	for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(cgs_b.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			cgb3[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }		
    	    } // else if (rnum == 2)
    	} // for (int rnum = 0; rnum < n_cg; rnum++)
    	// Compute tg at each radius
    	int n_tg = 3;
    	int r_tg[] = new int[]{5, 10, 20};
    	ArrayList<double[]>tgs;
    	for (int rnum = 0; rnum < n_tg; rnum++) {
    		Preferences.debug("Computing tg for r = " + r_tg[rnum] + "\n", Preferences.DEBUG_ALGORITHM);
    		tgs = hist_gradient_2D(t_assign, dstXDim, dstYDim, r_tg[rnum], n_ori, null);
    		// Return tg
    		if (rnum == 0) {
	    	    for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(tgs.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			tg1[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }
    	    } // if (rnum == 0)
    	    else if (rnum == 1) {
    	    	for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(tgs.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			tg2[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }	
    	    } // else if (rnum == 1)
    	    else if (rnum == 2) {
    	    	for (int n = 0; n < n_ori; n++) {
	    	    	compute_border_trim_2D(tgs.get(n),bufm, border, border, xDim, yDim, dstYDim);
	    	    	for (x = 0; x < xDim; x++) {
	    	    		for (y = 0; y < yDim; y++) {
	    	    			tg3[n][x][y] = bufm[y + x * yDim];
	    	    		}
	    	    	}
	    	    }		
    	    } // else if (rnum == 2)
    	} // for (int rnum = 0; rnum < n_tg; rnum++)
    }
    
    /*
     * Compute the distance between histograms of label values in oriented
     * half-dics of the specified radius centered at each location in the 2D
     * matrix.  Return one distance matrix per orientation.
     *
     * Alternatively, instead of specifying label values at each point, the user
     * may specify a histogram at each point, in which case the histogram for
     * a half-disc is the sum of the histograms at points in that half-disc.
     *
     * The half-disc orientations are k*pi/n for k in [0,n) where n is the 
     * number of orientation requested.
     *
     * The user may optionally specify a nonempty 1D smoothing kernel to 
     * convolve with histograms prior to computing the distance between them.
     *
     * The user may also optionally specify a custom functor for computing the
     * distance between histograms.
     */
    private ArrayList<double[]> hist_gradient_2D(
       int[] labels,int labelsXDim, int labelsYDim,
       int  r,
       int n_ori,
       double[] smoothing_kernel)
    {
       /* construct weight matrix for circular disc */
       double weights[] = weight_matrix_disc(r);
       int w_size_x = 2*r + 1;
       int w_size_y = 2*r + 1;
       /* compute oriented gradient histograms */
       return hist_gradient_2D(
          labels, labelsXDim, labelsYDim, weights, w_size_x, w_size_y, n_ori, smoothing_kernel
       );
    }
    
    /*
     * Construct weight matrix for circular disc of the given radius.
     */
    private double[] weight_matrix_disc(int r) {
       /* initialize matrix */
       int size = 2*r + 1;
       double weights[] = new double[size * size];
       /* set values in disc to 1 */
       int radius = r;
       int r_sq = radius * radius;
       int ind = 0;
       for (int x = -radius; x <= radius; x++) {
          int x_sq = x * x;
          for (int y = -radius; y <= radius; y++) {
             /* check if index is within disc */
             int y_sq = y * y;
             if ((x_sq + y_sq) <= r_sq)
                weights[ind] = 1;
             /* increment linear index */
             ind++;
          }
       }
       return weights;
    }

    
    /*
     * Compute the distance between histograms of label values in oriented
     * half-regions centered at each location in the 2D matrix.  Return one
     * distance matrix per orientation.
     *
     * The given 2D weight matrix (which must have odd dimensions) defines the
     * half-regions.  Each label adds the weight at the corresponding position
     * in this matrix to its histogram bin.
     *
     * If the user specifies a histogram at each point instead of a label, then 
     * the histogram for a half-region is a weighted sum of histograms.
     *
     * The above version of hist_gradient_2D which specifies a radius r is 
     * equivalent to calling this version with a (2*r+1) x (2*r+1) weight matrix
     * in which elements within a distance r from the center have weight one and
     * all other elements have weight zero.
     */
    private ArrayList<double[]> hist_gradient_2D(
       int[] labels, int labelsXDim, int labelsYDim,
       double[] weights, int w_size_x, int w_size_y,
       int n_ori,
       double[] smoothing_kernel)
    {
       int i;
       /* allocate result gradient matrices */
      ArrayList<double[]> gradients = new ArrayList<double[]>();
       /* check that result is nontrivial */
       if (n_ori == 0)
          return gradients;
       /* initialize result gradient matrices */
       gradients.ensureCapacity(n_ori);
       for (int n = 0; n < n_ori; n++) {
          gradients.add(new double[labelsXDim * labelsYDim]);
       }
       /* check that result is nonempty */
       if ((labelsXDim == 0) || (labelsYDim == 0))
          return gradients;
       /* allocate matrices to hold histograms of each slice */
       ArrayList<double[]>slice_hist = new ArrayList<double[]>(); 
       int maxLabels = -1;
       for (i = 0; i < labels.length; i++) {
    	   if (labels[i] > maxLabels) {
    		   maxLabels = labels[i];
    	   }
       }
       int hist_length = maxLabels + 1;
       slice_hist.ensureCapacity(2*n_ori);
       /* initialize slice histogram matrices */
       for (i = 0; i < 2*n_ori; i++) {
    	   slice_hist.add(new double[hist_length]);
       }
       int slice_histXDim = 1;
       int slice_histYDim = hist_length;
       /* build orientation slice lookup map */
       int[] slice_map = orientation_slice_map(
          w_size_x, w_size_y, n_ori
       );
       /* compute histograms and histogram differences at each location */
       compute_hist_gradient_2D(
          labels, labelsXDim, labelsYDim, weights, w_size_x, w_size_y, slice_map, smoothing_kernel,
          slice_hist, slice_histXDim, slice_histYDim, gradients
       );
       return gradients;
    }
    
    /*
     * Construct orientation slice lookup map.
     */
    private int[] orientation_slice_map(
       int size_x, int size_y, int n_ori)
    {
       /* initialize map */
       int[] slice_map = new int[size_x * size_y];
       /* compute orientation of each element from center */
       int ind = 0;
       double x = -size_x/2.0;
       for (int n_x = 0; n_x < size_x; n_x++) {
          double y = -size_y/2.0;
          for (int n_y = 0; n_y < size_y; n_y++) {
             /* compute orientation index */
             double ori = Math.atan2(y, x) + Math.PI;
             int idx = (int)Math.floor(ori / Math.PI * n_ori);
             if (idx >= (2*n_ori))
                idx = 2*n_ori - 1;
             slice_map[ind] = idx;
             /* increment position */
             ind++;
             y++;
          }
          /* increment x-coordinate */
          x++;
       }
       return slice_map;
    }


    /*
     * Compute histograms and histogram differences at each location.
     */
    private void compute_hist_gradient_2D(
       int[] labels, int size0_x, int size0_y,
       double[] weights, int size1_x, int size1_y,
       int[] slice_map,
       double[] smoothing_kernel,
       ArrayList<double[]> slice_hist, /* hist per slice */
       int slice_histXDim, int slice_histYDim,
       ArrayList<double[]> gradients  /* matrix per ori */)
    {
       int i;
       /* get number of orientations */
       int n_ori = gradients.size();
       /* set start position for gradient matrices */
       int pos_start_x = size1_x/2;
       int pos_start_y = size1_y/2;
       int pos_bound_y = pos_start_y + size0_y;
       /* initialize position in result */
       int pos_x = pos_start_x;
       int pos_y = pos_start_y;
       /* compute initial range of offset_x */
       int offset_min_x =
          ((pos_x + 1) > size0_x) ? (pos_x + 1 - size0_x) : 0;
       int offset_max_x =
          (pos_x < size1_x) ? pos_x : (size1_x - 1);
       int ind0_start_x = (pos_x - offset_min_x) * size0_y;
       int ind1_start_x = (offset_min_x) * size1_y;
       int size = labels.length;
       /* determine whether to use smoothing kernel */
       boolean use_smoothing = !(smoothing_kernel == null);
       double temp_conv[] = new double[slice_histXDim * slice_histYDim];
       int size_hist = slice_hist.get(0).length;
       /* allocate half disc histograms */
       double[] hist_left = new double[slice_histXDim * slice_histYDim];
       double[] hist_right = new double[slice_histXDim * slice_histYDim];
       for (int n = 0; n < size; n++) {
          /* compute range of offset_y */
          int offset_min_y =
             ((pos_y + 1) > size0_y) ? (pos_y + 1 - size0_y) : 0;
          int offset_max_y =
             (pos_y < size1_y) ? pos_y : (size1_y - 1);
          int offset_range_y = offset_max_y - offset_min_y;
          /* initialize indices */
          int ind0 = ind0_start_x + (pos_y - offset_min_y);
          int ind1 = ind1_start_x + offset_min_y;
          /* clear histograms */
          for (int n_hist = 0; n_hist < 2*n_ori; n_hist++) {
             double sl[] = slice_hist.get(n_hist);
             for (i = 0; i < sl.length; i++) {
            	 sl[i] = 0.0;
             }
          }
          /* update histograms */
          for (int o_x = offset_min_x; o_x <= offset_max_x; o_x++) {
             for (int o_y = offset_min_y; o_y < offset_max_y; o_y++) {
                /* update histogram value */
            	double sl[] = slice_hist.get(slice_map[ind1]);
            	sl[labels[ind0]] += weights[ind1];
                /* update linear positions */
                ind0--;
                ind1++;
             }
             /* update last histogram value */
             double sl[] = slice_hist.get(slice_map[ind1]);
         	 sl[labels[ind0]] += weights[ind1];
             /* update linear positions */
             ind0 = ind0 + offset_range_y - size0_y;
             ind1 = ind1 - offset_range_y + size1_y;
          }
          /* smooth bins */
          if (use_smoothing) {
             for (int o = 0; o < 2*n_ori; o++) {
                double sh[] = slice_hist.get(o);
                for (i = 0; i < temp_conv.length; i++) {
                	temp_conv[i] = 0.0;
                }
                conv_in_place_1D(sh, smoothing_kernel, temp_conv);
                for (int nh = 0; nh < size_hist; nh++)
                   sh[nh] = temp_conv[nh];
             }
          }
          /* L1 normalize bins */
          for (int o = 0; o < 2*n_ori; o++) {
        	 double sum_slice_hist = 0.0;
        	 double sl[] = slice_hist.get(o);
        	 for (i = 0; i < sl.length; i++) {
        		 sum_slice_hist += sl[i];
        	 }
             if (sum_slice_hist != 0)
            	for (i = 0; i < sl.length; i++) {
                    sl[i] /= sum_slice_hist;
            	}
          }
          /* compute circular gradients - initialize histograms */
          for (i = 0; i < hist_left.length; i++) {
        	  hist_left[i] = 0.0;
          }
          for (i = 0; i < hist_right.length; i++) {
        	  hist_right[i] = 0.0;
          }
          for (int o = 0; o < n_ori; o++) {
        	 double sl[] = slice_hist.get(o);
        	 double sr[] = slice_hist.get(o+n_ori);
        	 for (i = 0; i < hist_left.length; i++) {
        		 hist_left[i] += sl[i];
        	 }
        	 for (i = 0; i < hist_right.length; i++) {
        		 hist_right[i] += sr[i];
        	 }
          }
          /* compute circular gradients - spin the disc */
          for (int o = 0; o < n_ori; o++) {
        	 double gr[] = gradients.get(o);
             gr[n] = matrix_X2_distance(hist_left, hist_right);
             double sl[] = slice_hist.get(o);
        	 double sr[] = slice_hist.get(o+n_ori);
        	 for (i = 0; i < hist_left.length; i++) {
        		 hist_left[i] -= sl[i];
        		 hist_left[i] += sr[i];
        	 }
             for (i = 0; i < hist_right.length; i++) {
            	 hist_right[i] += sl[i];
            	 hist_right[i] -= sr[i];
             }
          }
          /* update position */
          pos_y++;
          if (pos_y == pos_bound_y) {
             /* reset y position, increment x position */
             pos_y = pos_start_y;
             pos_x++;
             /* update range of offset_x */
             offset_min_x = ((pos_x + 1) > size0_x) ? (pos_x + 1 - size0_x) : 0;
             offset_max_x = (pos_x < size1_x) ? pos_x : (size1_x - 1);
             ind0_start_x = (pos_x - offset_min_x) * size0_y;
             ind1_start_x = (offset_min_x) * size1_y;
          }
       }
    }
    
    /*
     * Chi-squared distance between matrices.
     */
    private double matrix_X2_distance(double m0[], double m1[]) {
          double dist = 0.0;
          for (int n = 0; n < m0.length; n++) {
             double diff = m1[n] - m0[n];
             double sum  = m1[n] + m0[n];
             if (diff != 0.0)
                dist += diff*diff / sum;
          }
          return dist/2.0;
       }

    
    /*
     * Compute convolution in place (for 1D matrices).
     */
    private void conv_in_place_1D(
       double[] m0,
       double[] m1,
       double[] m)
    {
       /* get size of each matrix */
       int size0 = m0.length;
       int size1 = m1.length;
       /* set dimensions for result matrix no larger than left input */
       int size = ((size0 > 0) && (size1 > 0)) ? (size0) : 0;
       /* set start position for result matrix no larger than left input */
       int pos_start = size1/2;
       /* initialize position in result */
       int pos = pos_start;
       for (int n = 0; n < size; n++) {
          /* compute range of offset */
          int offset_min = ((pos + 1) > size0) ? (pos + 1 - size0) : 0;
          int offset_max = (pos < size1) ? pos : (size1 - 1);
          /* multiply and add corresponing elements */
          int ind0 = pos - offset_min;
          int ind1 = offset_min;
          while (ind1 <= offset_max) {
             /* update result value */
             m[n] += m0[ind0] * m1[ind1];
             /* update linear positions */
             ind0--;
             ind1++;
          }
          /* update position */
          pos++;
       }
    }


    
    private int[] textons_routine(double m[][], ArrayList<double[][]> filters, ArrayList<double[]> textons,
    		int K, int max_iter, double subsampling) {
    	int i;
    	int x;
    	int y;
    	double array[];
    	// Convolve image with filters
    	// Return the convolution of the image with each of the filters so that
    	// the result is the same size as the original image
        ArrayList<double[][]> responses = new ArrayList<double[][]>();
        double Cout[][];
        boolean is_cropped = true;
        boolean is_strict = false;
        for (i = 0; i < filters.size(); i++) {
        	Cout = compute_conv_2D(m, filters.get(i), is_cropped, is_strict);
        	responses.add(Cout);
        }
        maxFraction = subsampling;
        minSample = K;
        kmeansK = K;
        metric = L2_metric;
        max_iterations = max_iter;
        ArrayList<metricCentroid>centroid = new ArrayList<metricCentroid>();
        // textons.size() == 0 here
        for (i = 0; i < textons.size(); i++) {
            centroid.add(new metricCentroid(textons.get(i), 1.0));	
        }
        ArrayList<double[]> responses2 = new ArrayList<double[]>();
        int xout = m.length;
        int yout = m[0].length;
        for (x = 0; x < xout; x++) {
    		for (y = 0; y < yout; y++) {
    			array = new double[responses.size()];
    			for (i = 0; i < responses.size(); i++) {
    				array[i] = responses.get(i)[x][y];
    			}
    			responses2.add(array);
    		}
    	}
        int[] assign = cluster(responses2, centroid);
        return assign;
        //sample_clusterer< matrix<> >(
                //kmeans::matrix_clusterer<>(K, max_iter, matrix_metrics<>::L2_metric()),
                //subsampling,
                //K
             //)
    }
      
    /*
     * Clustering.
     * Return the cluster assignments and cluster centroids.
     */
    private int []cluster(
       final ArrayList<double[]> items,
       ArrayList<metricCentroid> centroids) 
    {
    	int i;
    	int elremoved;
    	int j;
       // compute number of items to cluster
       int n_items = items.size();
       int samp_size = sample_size(n_items); 
       // check if clustering the full set
       if (samp_size == n_items) {
          return kmeans_cluster(items, centroids);
       } else {
          // randomize item order
    	   RandomNumberGen randomGen = new RandomNumberGen();
    	   ArrayList<doubleIntegerItem> randomList = new ArrayList<doubleIntegerItem>();
    	   for (i = 0; i < n_items; i++) {
    		   randomList.add(new doubleIntegerItem(randomGen.genUniformRandomNum(0.0, 1.0), i));
    	   }
    	   Collections.sort(randomList, new doubleIntegerComparator());
    	   int idx_map[] = new int[n_items];
    	   int idx_map2[] = new int[n_items];
    	   for (i = 0; i < n_items; i++) {
    	       idx_map[i] = randomList.get(i).getiN();
    	       idx_map2[i] = idx_map[i];
    	   }
    	   ArrayList<double[]>items_subset = new ArrayList<double[]>();
    	   for (i = 0; i < samp_size; i++) {
    		   elremoved = idx_map2[i];
    		   items_subset.add(items.remove(elremoved));
    		   for (j = i+1; j < idx_map2.length; j++) {
    			   if (idx_map2[j] > elremoved) {
    				   idx_map2[j]--;
    			   }
    		   }
    	   } // for (i = 0; i < samp_size; i++)
          // cluster subset
          int[] assign_subset = kmeans_cluster(
             items_subset, centroids
          );
          // compute assignments for remaining items
          int[] assign_array = assign_cluster(
             items, centroids
          );
          // combine assignment arrays
          int assign[] = new int[n_items];
          for (int n = 0; n < samp_size; n++)
             assign[idx_map[n]] = assign_subset[n];
          for (int n = samp_size; n < n_items; n++)
             assign[idx_map[n]] = assign_array[n - samp_size];
          return assign;
       }
    }
    
    private int[] assign_cluster(ArrayList<double[]>items, ArrayList<metricCentroid>centroids) {
    	   int n_items = items.size();
    	   int assignments[] = new int[n_items];
    	   if (n_items > 0) {
    	      cluster_assigner(
    	         0, n_items - 1, items, centroids, assignments
    	      );
    	   }
    	   return assignments;
    }
    
    private void cluster_assigner(int start, int end, ArrayList<double[]>items, ArrayList<metricCentroid>centroids,
    		int assignments[]) {
    	for (int n = start; n <= end; n++)
            assignments[n] = kmeans_assign_cluster(items.get(n), centroids);
	    return;
    }
    
    /*
     * Cluster assignment.
     * Return the id of the cluster centroid closest to the item.
     * Note that the centroid array must not be empty.
     */
    private int kmeans_assign_cluster(
       double item[], 
       ArrayList<metricCentroid>centroids) 
    {
       // Initialize id and distance
       int id = 0;
       double min_dist = metricDistance(item, centroids.get(0));
       // Find closest centroid
       int n_items = centroids.size();
       for (int n = 1; n < n_items; n++) {
          double dist = metricDistance(item, centroids.get(n));
          if (dist < min_dist) {
             min_dist = dist;
             id = n;
          }
       }
       return id;
    }

    
    private int[] kmeans_cluster(ArrayList<double[]>items, ArrayList<metricCentroid>centroids) {
    	int i;
    	int n_items = items.size();
    	double weights[] = new double[n_items];
    	for (i = 0; i < n_items; i++) {
    	    weights[i] = 1.0;	
    	}
    	RandomNumberGen randomGen = new RandomNumberGen();
 	    ArrayList<doubleIntegerItem> randomList = new ArrayList<doubleIntegerItem>();
 	    for (i = 0; i < n_items; i++) {
 		    randomList.add(new doubleIntegerItem(randomGen.genUniformRandomNum(0.0, 1.0), i));
 	    }
 	    Collections.sort(randomList, new doubleIntegerComparator());
 	    int idx_map[] = new int[n_items];
 	    for (i = 0; i < n_items; i++) {
	        idx_map[i] = randomList.get(i).getiN();
	    }
 	    return kmeans_cluster(items, weights, idx_map, centroids);
    }
    
    private int[] kmeans_cluster(ArrayList<double[]>items, double[] weights, int[] idx_map, ArrayList<metricCentroid>centroids) {
    	int i;
    	int n_items = idx_map.length;
    	// Randomize item order using the specified random permutation
    	ArrayList<double[]>items_array = new ArrayList<double[]>();
    	double weights_array[] = new double[n_items];
    	for (i = 0; i < n_items; i++) {
    		items_array.add(items.get(idx_map[i]).clone());
    		weights_array[i] = weights[idx_map[i]];
    	}
    	// Compute maximum number of clusters to return 
    	int K = (kmeansK < n_items) ? kmeansK : n_items;
    	if ((n_items > 0) && (K == 0)) {
    	      MipavUtil.displayError(
    	         "K must be nonzero during K-means clustering of nonempty collection"
    	      );
    	      return null;
    	   }
    	   // Allocate collection of centroids
    	   centroids.clear();
    	   /* return now if there are no items to cluster */
    	   if (n_items == 0)
    	      return null;
    	   // Initialize assignments (to undefined cluster # K)
    	   // Initialize cluster membership for centroids
    	   int assign[] = new int[n_items];
    	   for (i = 0; i < n_items; i++) {
    		   assign[i] = K;
    	   }
    	   double [][] cluster_items = new double[K][];
    	   double[] cluster_weights = new double[K];
    	   for (int n = 0; n < K; n++) {
    	      cluster_items[n] = items_array.get(n).clone();
    	      cluster_weights[n] =  weights_array[n];
    	   }
    	   // Initialize centroids 
    	   for (int n = 0; n < K; n++) {
    		   centroids.add(new metricCentroid(cluster_items[n], cluster_weights[n]));
    	   }
    	   // Allocate arrays for distances
    	   
    	   ArrayList<ArrayList<Double> > distances = new ArrayList<ArrayList<Double>>(n_items);
    	   for (int n = 0; n < n_items; n++) {
    		   distances.add(new ArrayList<Double>());
    	   }
    	   dist_resizer(
    	      0, n_items - 1, K, distances
    	   );
    	   // Initially mark the undefined cluster # K as changed
    	   boolean[] has_changed = new boolean[K+1];
    	   for (i = 0; i < K; i++) {
    		   has_changed[i] = false;
    	   }
    	   has_changed[K] = true;
    	   int[] changed_ids;
    	   // Compute sizes of coarse problems
    	   int[] coarse_sizes = coarse_problem_sizes(n_items, K, 0.5);
    	   int n_problems = coarse_sizes.length;
    	   // Solve coarse to fine clustering problems
    	   int n_items_prev = 0;
    	   for (int prob = 0; prob < n_problems; prob++) {
    	      // Get problem size
    	      int n_items_curr = coarse_sizes[prob];
    	      // Compute distances between new items and unchanged clusters
    	      {
    	         boolean[] has_not_changed = new boolean[K];
    	         for (int n = 0; n < K; n++)
    	            has_not_changed[n] = !(has_changed[n]);
    	         int[] unchanged_ids = compute_changed_ids(has_not_changed);
    	         distance_updater(metric,
    	            n_items_prev,
    	            n_items_curr - 1,
    	            items_array,
    	            centroids,
    	            unchanged_ids,
    	            distances
    	         );
    	      }
    	      // Initialize any empty clusters with new items
    	      for (int n = 0, next_item = n_items_prev;
    	           ((n < K) && (next_item < n_items_curr)); n++)
    	      {
    	         if (cluster_items[n] == null) {
    	            // Add item to cluster
    	               cluster_items[n] = items_array.get(next_item).clone();
    	               cluster_weights[n] = weights_array[next_item];
    	            // Compute centroid
    	            metricCentroid cntrd = new metricCentroid(
    	               cluster_items[n],
    	               cluster_weights[n]
    	            );
    	            metricCentroid cntrd_old = centroids.set(n, cntrd);
    	            cntrd_old.deleteClusterItems();
    	            cntrd_old = null;
    	            // Indicate that cluster has changed
    	            has_changed[n] = true;
    	            next_item++;
    	         }
    	      } // for (int n = 0, next_item = n_items_prev;
    	      // Recompute changed ids to include any filled empty clusters
    	      changed_ids = compute_changed_ids(has_changed, K);
    	      // Iteratively update assignments and centroids
    	      for (int n_iter = 0; 
    	           ((n_iter < max_iterations) || (max_iterations == 0));
    	           n_iter++)
    	      {
    	         // Store old assignments
    	    	 int assign_old[] = new int[n_items_curr];
    	    	 for (i = 0; i < n_items_curr; i++) {
    	    		 assign_old[i] = assign[i];
    	    	 }
    	         // Update distances
    	         distance_updater(
    	            metric,
    	            0,
    	            n_items_curr - 1,
    	            items_array,
    	            centroids,
    	            changed_ids,
    	            distances
    	         );
    	         // Update assignments
    	         assignment_updater(
    	            0,
    	            n_items_curr - 1,
    	            K,
    	            has_changed,
    	            changed_ids,
    	            distances,
    	            assign
    	         );
    	         // Compute which clusters have changed
    	         for (int n = 0; n < K; n++)
    	            has_changed[n] = false;
    	         for (int n = 0; n < n_items_curr; n++) {
    	            int assign_id     = assign[n];
    	            int assign_id_old = assign_old[n];
    	            if (assign_id != assign_id_old) {
    	               has_changed[assign_id]     = true;
    	               has_changed[assign_id_old] = true;
    	            }
    	         }
    	         // Compute ids of changed clusters
    	         changed_ids = compute_changed_ids(
    	            has_changed, K
    	         );
    	         // Finish if no clusters changed
    	         int n_changed = changed_ids.length;
    	         if (n_changed == 0)
    	            break;
    	         // Update cluster membership
    	         for (int n = 0; n < K; n++) {
    	        	cluster_items[n] = null;
    	            cluster_weights[n] = 0.0;
    	         }
    	         for (int n = 0; n < n_items_curr; n++) {
    	            int assign_id = assign[n];
    	            cluster_items[assign_id] = items_array.get(n).clone();
    	            cluster_weights[assign_id] = weights_array[n];
    	         }
    	         // Update centroids
    	         centroid_updater(
    	            metric,
    	            0,
    	            n_changed - 1,
    	            changed_ids,
    	            cluster_items,
    	            cluster_weights,
    	            centroids
    	         );   
    	      } // for (int n_iter = 0;
    	      // Update previous problem size
    	      n_items_prev = n_items_curr;
    	   }
    	   // Drop empty clusters
    	   int remap[] = new int[K];
    	   // Init remap to invalid cluster #K
    	   for (i = 0; i < K; i++) {
    		   remap[i] = K;
    	   }
    	   int n_clusters = 0;
    	   for (int n = 0; n < K; n++) {
    		  metricCentroid cntrd = centroids.remove(0);
    	      if (!(cluster_items[n] == null)) {
    	         remap[n] = n_clusters;
    	         centroids.add(cntrd);
    	         n_clusters++;
    	      }
    	   }
    	   // Retrieve original assignment order
    	   int assignments[] = new int[n_items];
    	   for (int n = 0; n < n_items; n++)
    	      assignments[idx_map[n]] = remap[assign[n]];
    	   return assignments;
    }
    
    private void centroid_updater(int metric, int start, int end, int changed_ids[], double cluster_items[][],
    		double cluster_weights[], ArrayList<metricCentroid>centroids) {
    	 // update centroids sequentially
        for (int n = start; n <= end; n++) {
           int id = changed_ids[n];
           if (!(cluster_items[id] == null)) {
              metricCentroid cntrd = new metricCentroid(
                 cluster_items[id],
                 cluster_weights[id]
              );
              metricCentroid cntrd_old = centroids.set(id, cntrd);
              cntrd_old.deleteClusterItems();
              cntrd_old = null;
           }
        }
	
    }
    
    private void assignment_updater(int start, int end, int n_centroids, boolean has_changed[],
    		int changed_ids[], ArrayList<ArrayList<Double>> distances, int assignments[]) {
    	/* update assignments sequentially */
        int n_changed = changed_ids.length;
        for (int n = start; n <= end; n++) {
           /* get distance array and current assignment */
           ArrayList<Double> distance = distances.get(n);
           int assign_id  = assignments[n];
           /* check if cluster to which item is assigned has changed */
           if (has_changed[assign_id]) {
              /* search all distances to find minimum */
              assign_id = 0;
              double min_dist = distance.get(0);
              for (int id = 1; id < n_centroids; id++) {
                 if (distance.get(id) < min_dist) {
                    min_dist = distance.get(id);
                    assign_id = id;
                 }
              }
           } else {
              /* search only distances to current and changed clusters */
              double min_dist = distance.get(assign_id);
              for (int n_id = 0; n_id < n_changed; n_id++) {
                 int id = changed_ids[n_id];
                 if (distance.get(id) < min_dist) {
                    min_dist = distance.get(id);
                    assign_id = id;
                 }
              }
           }
           assignments[n] = assign_id;
        }
	
    }
    
    /*
     * Compute ids of changed centroids given change flags.
     */
    int[] compute_changed_ids(
       boolean[] has_changed)
    {
       int n_clusters = has_changed.length;
       return compute_changed_ids(has_changed, n_clusters);
    }


    /*
     * Compute ids of changed centroids given change flags.
     * Specify the number of centroids to consider.
     */
    int[] compute_changed_ids(
       boolean[] has_changed, int n_clusters)
    {
       /* compute how many centroids have changed */
       int n_changed = 0;
       for (int n = 0; n < n_clusters; n++) {
          if (has_changed[n])
             n_changed++;
       }
       /* get array of changed centroid ids */
       int[] changed_ids =  new int[n_changed];
       for (int n = 0, chngd = 0; chngd < n_changed; n++) {
          if (has_changed[n]) {
             changed_ids[chngd] = n;
             chngd++;
          }
       }
       return changed_ids;
    }
    
    private void distance_updater(int metric, int start, int end, ArrayList<double[]>items,
    		ArrayList<metricCentroid>centroids, int[] changed_ids, ArrayList<ArrayList<Double> > distances) {
    	/* update distances sequentially */
        int n_changed = changed_ids.length;
        for (int n = start; n <= end; n++) {
           /* get item and distance array */
           double[] item      = items.get(n);
           ArrayList<Double> distance = distances.get(n);
           /* update distance to changed clusters */
           for (int n_id = 0; n_id < n_changed; n_id++) {
              int id = changed_ids[n_id];
              if (distance.size() -1 >=  id) {
                  distance.set(id, metricDistance(item, centroids.get(id)));
              }
              else {
            	  distance.add(id, metricDistance(item, centroids.get(id)));
              }
           }
        }
	
    }
    
    private double metricDistance(double[] item, metricCentroid centroid) {
        double cluster_item[] = centroid.getCluster_items();	
        if (item.length != cluster_item.length) {
        	MipavUtil.displayError("item.length = " + item.length + " != cluster_item.length = " + cluster_item.length);
        	return Double.NaN;
        }
        double dist = 0.0;
        double diff;
        for (int x = 0; x < item.length; x++) {
        	    diff = item[x] - cluster_item[x];
        	    dist += diff * diff;
        }
        return Math.sqrt(dist);
    }

    
    /*
     * Return the size of each problem in a series of coarse to fine
     * clustering problems.
     * @param n_items number of items
     * @param min_size minimum problem size
     * @param factor coarsening factor in [0,1)
     */
    int[] coarse_problem_sizes(
       int n_items, int min_size, double factor)
    {
       /* check arguments */
       if ((factor < 0) || (factor >= 1)) {
          MipavUtil.displayError("coarsening factor must be in [0,1)");
          return null;
       }
       /* compute number of problems */
       int n_problems = 0;
       int curr_size = n_items;
       do {
          n_problems++;
          curr_size = (int)Math.floor(curr_size * factor);
       } while (curr_size >= min_size);
       /* store size of each problem */
       int[] coarse_sizes = new int[n_problems];
       curr_size = n_items;
       do {
          n_problems--;
          coarse_sizes[n_problems] = curr_size;
          curr_size = (int)Math.floor(curr_size * factor);
       } while (curr_size >= min_size);
       return coarse_sizes;
    }

    
    private void dist_resizer(int start, int end, int n_centroids, ArrayList<ArrayList<Double>>distances) {
    	// Resize distances sequentially
        for (int n = start; n <= end; n++)
           distances.get(n).ensureCapacity(n_centroids);

    }
    
    private class metricCentroid {
    	private double[] cluster_items;
    	
    	private double cluster_weights;
    	
    	public metricCentroid(double[] cluster_items,double cluster_weights) {
    		this.cluster_items = cluster_items;
    		this.cluster_weights = cluster_weights;
    	}
    	
    	public double[] getCluster_items() {
    	    return cluster_items;		
    	}
    	
    	public void deleteClusterItems() {
    	    cluster_items = null;
    	}
    }
    
    private class doubleIntegerComparator implements Comparator<doubleIntegerItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final doubleIntegerItem o1, final doubleIntegerItem o2) {
            final double a = o1.getrN();
            final double b = o2.getrN();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }
	
	private class doubleIntegerItem {

        /** DOCUMENT ME! */
        private final double rN;

        /** DOCUMENT ME! */
        private final int iN;

        /**
         * Creates a new doubleIntegerItem object.
         * 
         * @param rN
         * @param iN
         */
        public doubleIntegerItem(final double rN, final int iN) {
            this.rN = rN;
            this.iN = iN;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getrN() {
            return rN;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getiN() {
            return iN;
        }

    }
       
       /*
        * Compute size of the sample to cluster given the total number of items.
        */
        private int sample_size(int n_items) {
          /* initialize sample size */
          int samp_size = n_items;
          /* enforce maximum items constraint */
          if (maxItems > 0) {
             if(maxItems < samp_size) { samp_size = maxItems; }
          }
          /* enforce maximum fractional constraint */
          int n_frac = (int)Math.ceil(maxFraction * n_items);
          if (n_frac < minSample) { n_frac = minSample; }
          if (n_frac < samp_size) { samp_size = n_frac; }
          return samp_size;
       }



    
    /*
     * Compute convolution (for 2D matrices).
     */
    private double[][] compute_conv_2D(
       final double m02D[][],
       final double m12D[][],
       boolean is_cropped,
       boolean is_strict)
    {
       /* get size of each matrix */
       int size0_x = m02D.length;
       int size0_y = m02D[0].length;
       int size1_x = m12D.length;
       int size1_y = m12D[0].length;
       double m0[] = new double[size0_x * size0_y];
       double m1[] = new double[size1_x * size1_y];
       int x;
       int y;
       for (x = 0; x < size0_x; x++) {
    	   for (y = 0; y < size0_y; y++) {
    		   m0[y + x * size0_y] = m02D[x][y];
    	   }
       }
       for (x = 0; x < size1_x; x++) {
    	   for (y = 0; y < size1_y; y++) {
    		   m1[y + x * size1_y] = m12D[x][y];
    	   }
       }
       /* compute dimensions of resulting matrix and also     */ 
       /* compute range of position within full result matrix */
       int size_x = 0;
       int size_y = 0;
       int pos_start_x = 0;
       int pos_start_y = 0;
       if (!is_cropped) {
          /* set dimensions for full result matrix (start position is zero) */
          size_x = ((size0_x > 0) && (size1_x > 0)) ? (size0_x + size1_x - 1) : 0;
          size_y = ((size0_y > 0) && (size1_y > 0)) ? (size0_y + size1_y - 1) : 0;
       } else if (!is_strict) {
          /* set dimensions for result matrix no larger than left input */
          size_x = ((size0_x > 0) && (size1_x > 0)) ? (size0_x) : 0;
          size_y = ((size0_y > 0) && (size1_y > 0)) ? (size0_y) : 0;
          /* set start position for result matrix no larger than left input */
          pos_start_x = size1_x/2;
          pos_start_y = size1_y/2;
       } else {
          /* set dimensions for central portion of result matrix */
          size_x =
             ((size0_x >= size1_x) && (size1_x > 0)) ? (size0_x - size1_x + 1) : 0;
          size_y =
             ((size0_y >= size1_y) && (size1_y > 0)) ? (size0_y - size1_y + 1) : 0;
          /* set start position for central portion of result matrix */
          pos_start_x = (size1_x > 0) ? (size1_x - 1) : 0;
          pos_start_y = (size1_y > 0) ? (size1_y - 1) : 0;
       }
       int pos_bound_y = pos_start_y + size_y;
       /* initialize result */
       double m2D[][] = new double[size_x][size_y];
       double m[] = new double[size_x * size_y];
       if (m.length == 0)
          return null;
       /* initialize position in result */
       int pos_x = pos_start_x;
       int pos_y = pos_start_y;
       /* compute initial range of offset_x */
       int offset_min_x =
          ((pos_x + 1) > size0_x) ? (pos_x + 1 - size0_x) : 0;
       int offset_max_x =
          (pos_x < size1_x) ? pos_x : (size1_x - 1);
       int ind0_start_x = (pos_x - offset_min_x) * size0_y;
       int ind1_start_x = (offset_min_x) * size1_y;
       for (int n = 0; n < m.length; n++) {
          /* compute range of offset_y */
          int offset_min_y =
             ((pos_y + 1) > size0_y) ? (pos_y + 1 - size0_y) : 0;
          int offset_max_y =
             (pos_y < size1_y) ? pos_y : (size1_y - 1);
          int offset_range_y = offset_max_y - offset_min_y;
          /* initialize indices */
          int ind0 = ind0_start_x + (pos_y - offset_min_y);
          int ind1 = ind1_start_x + offset_min_y;
          for (int o_x = offset_min_x; o_x <= offset_max_x; o_x++) {
             for (int o_y = offset_min_y; o_y < offset_max_y; o_y++) {
                /* update result value */
                m[n] += m0[ind0] * m1[ind1];
                /* update linear positions */
                ind0--;
                ind1++;
             }
             /* update last result value */
             m[n] += m0[ind0] * m1[ind1];
             /* update linear positions */
             ind0 = ind0 + offset_range_y - size0_y;
             ind1 = ind1 - offset_range_y + size1_y;
          }
          /* update position */
          pos_y++;
          if (pos_y == pos_bound_y) {
             /* reset y position, increment x position */
             pos_y = pos_start_y;
             pos_x++;
             /* update range of offset_x */
             offset_min_x = ((pos_x + 1) > size0_x) ? (pos_x + 1 - size0_x) : 0;
             offset_max_x = (pos_x < size1_x) ? pos_x : (size1_x - 1);
             ind0_start_x = (pos_x - offset_min_x) * size0_y;
             ind1_start_x = (offset_min_x) * size1_y;
          }
       }
       for (x = 0; x < size_x; x++) {
    	   for (y = 0; y < size_y; y++) {
    		   m2D[x][y] = m[y + x * size_y];
    	   }
       }
       return m2D;
    }
    
    /*
     * Compute border trimmed matrix.
     */
    private void compute_border_trim_2D(int src[], int dst[], int borderX, int borderY, int xDstSize, int yDstSize,
    		int ySrcSize)
    {
       /* compute step sizes in source matrix */
       int ind_init = borderX * ySrcSize + borderY;
       int ind_step = 2*borderY;
       /* trim border */
       for (int n = 0, ind = ind_init, x = 0; x < xDstSize; x++) {
          for (int y = 0; y < yDstSize; y++, n++, ind++)
             dst[n] = src[ind];
          ind += ind_step;
       }
    }
    
    /*
     * Compute border trimmed matrix.
     */
    private void compute_border_trim_2D(double src[], double dst[], int borderX, int borderY, int xDstSize, int yDstSize,
    		int ySrcSize)
    {
       /* compute step sizes in source matrix */
       int ind_init = borderX * ySrcSize + borderY;
       int ind_step = 2*borderY;
       /* trim border */
       for (int n = 0, ind = ind_init, x = 0; x < xDstSize; x++) {
          for (int y = 0; y < yDstSize; y++, n++, ind++)
             dst[n] = src[ind];
          ind += ind_step;
       }
    }

    
    private void compute_border_mirror_2D(double src[], double dst[], int borderX, int borderY, int xSrcSize, int ySrcSize) {
    	// Compute destination size
    	int xDstSize = xSrcSize + 2 * borderX;
    	int yDstSize = ySrcSize + 2 * borderY;
    	// Compute step sizes in destination matrix (for copying interior)
    	int indInit = borderX * yDstSize + borderY;
    	int indStep = 2 * borderY;
    	int n;
    	int ind;
    	int x;
    	int y;
    	int indIntr;
    	int indBdr;
    	
    	// Copy interior
    	for (n = 0, ind = indInit, x = 0; x < xSrcSize; x++) {
    		for (y = 0; y < ySrcSize; y++, n++, ind++) {
    			dst[ind] = src[n];
    		}
    		ind += indStep;
    	}
    	
    	// Mirror top
    	indIntr = borderX * yDstSize;
    	indBdr = indIntr + yDstSize;
    	for (x = 0; x < borderX; x++) {
    	    indBdr -= 2 * yDstSize;
    	    for (y = 0; y < yDstSize; y++) {
    	    	dst[indBdr++] = dst[indIntr++];
    	    }
    	}
    	
    	// Mirror bottom
    	indBdr = (xSrcSize + borderX) * yDstSize;
    	indIntr = indBdr + yDstSize;
    	for (x = 0; x < borderX; x++) {
    		indIntr -= 2*yDstSize;
    		for (y = 0; y < yDstSize; y++) {
    			dst[indBdr++] = dst[indIntr++];
    		}
    	}
    	
    	// Mirror left
    	indBdr = 0;
    	indIntr = 2 * borderY;
    	for (x = 0; x < xDstSize; x++) {
    		for (y = 0; y < borderY; y++) {
    		    dst[indBdr++] = dst[--indIntr];	
    		}
    		indBdr += (yDstSize - borderY);
    		indIntr += (yDstSize + borderY);
    	}
    	
    	// Mirror right
    	indBdr = yDstSize - borderY;
    	indIntr = indBdr;
    	for (x = 0; x < xDstSize; x++) {
    		for (y = 0; y < borderY; y++) {
    			dst[indBdr++] = dst[--indIntr];
    		}
    		indBdr += (yDstSize - borderY);
    		indIntr += (yDstSize + borderY);
    	}
    	return;
    }
    
    private void compute_border_mirror_2D(int src[], int dst[], int borderX, int borderY, int xSrcSize, int ySrcSize) {
    	// Compute destination size
    	int xDstSize = xSrcSize + 2 * borderX;
    	int yDstSize = ySrcSize + 2 * borderY;
    	// Compute step sizes in destination matrix (for copying interior)
    	int indInit = borderX * yDstSize + borderY;
    	int indStep = 2 * borderY;
    	int n;
    	int ind;
    	int x;
    	int y;
    	int indIntr;
    	int indBdr;
    	
    	// Copy interior
    	for (n = 0, ind = indInit, x = 0; x < xSrcSize; x++) {
    		for (y = 0; y < ySrcSize; y++, n++, ind++) {
    			dst[ind] = src[n];
    		}
    		ind += indStep;
    	}
    	
    	// Mirror top
    	indIntr = borderX * yDstSize;
    	indBdr = indIntr + yDstSize;
    	for (x = 0; x < borderX; x++) {
    	    indBdr -= 2 * yDstSize;
    	    for (y = 0; y < yDstSize; y++) {
    	    	dst[indBdr++] = dst[indIntr++];
    	    }
    	}
    	
    	// Mirror bottom
    	indBdr = (xSrcSize + borderX) * yDstSize;
    	indIntr = indBdr + yDstSize;
    	for (x = 0; x < borderX; x++) {
    		indIntr -= 2*yDstSize;
    		for (y = 0; y < yDstSize; y++) {
    			dst[indBdr++] = dst[indIntr++];
    		}
    	}
    	
    	// Mirror left
    	indBdr = 0;
    	indIntr = 2 * borderY;
    	for (x = 0; x < xDstSize; x++) {
    		for (y = 0; y < borderY; y++) {
    		    dst[indBdr++] = dst[--indIntr];	
    		}
    		indBdr += (yDstSize - borderY);
    		indIntr += (yDstSize + borderY);
    	}
    	
    	// Mirror right
    	indBdr = yDstSize - borderY;
    	indIntr = indBdr;
    	for (x = 0; x < xDstSize; x++) {
    		for (y = 0; y < borderY; y++) {
    			dst[indBdr++] = dst[--indIntr];
    		}
    		indBdr += (yDstSize - borderY);
    		indIntr += (yDstSize + borderY);
    	}
    	return;
    }
    
    private void grayscale(double m[], double r[], double g[], double b[]) {
    	int n;
    	for (n = 0; n < m.length; n++) {
    		m[n] = (0.29894 * r[n]) + (0.58704 * g[n]) + (0.11402 * b[n]);
    	}
    	return;
    }
    
    private void rgb_gamma_correct(double r[], double g[], double b[], double gamma) {
    	int n;
    	for (n = 0; n < r.length; n++) {
    	    r[n] = Math.pow(r[n], gamma);
    	    g[n] = Math.pow(g[n], gamma);
    	    b[n] = Math.pow(b[n], gamma);
    	}
    	return;
    }
    
    /**
     * Convert from RGB color space to Lab color space
     * @param r_l
     * @param g_a
     * @param b_b
     */
    private void rgb_to_lab(double r_l[], double g_a[], double b_b[]) {
    	rgb_to_xyz(r_l, g_a, b_b);
    	xyz_to_lab(r_l, g_a, b_b);
    }
    
    /**
     * Convert from RGB color space to XYZ color space
     * @param r_x
     * @param g_y
     * @param b_z
     */
    private void rgb_to_xyz(double r_x[], double g_y[], double b_z[]) {
    	int n;
    	double r;
    	double g;
    	double b;
    	for (n = 0; n < r_x.length; n++) {
    		r = r_x[n];
    		g = g_y[n];
    		b = b_z[n];
    		r_x[n] = (0.412453 * r) + (0.357580 * g) + (0.180423 * b);
    		g_y[n] = (0.212671 * r) + (0.715160 * g) + (0.072169 * b);
    		b_z[n] = (0.019334 * r) + (0.119193 * g) + (0.950227 * b);
    	}
    	return;
    }
    
    /**
     * Convert from XYZ color space to Lab color space
     * @param x_l
     * @param y_a
     * @param z_b
     */
    private void xyz_to_lab(double x_l[], double y_a[], double z_b[]) {
        // D65 white point reference
    	final double xRef = 0.950456;
    	final double yRef = 1.000000;
    	final double zRef = 1.088754;
    	// Threshold value
    	final double threshold = 0.008856;
    	// Convert XYZ to Lab
    	int n;
    	double x;
    	double y;
    	double z;
    	double fx;
    	double fy;
    	double fz;
    	for (n = 0; n < x_l.length; n++) {
    		// Extract xyz color value and normalize by reference point
    		x = x_l[n]/xRef;
    		y = y_a[n]/yRef;
    		z = z_b[n]/zRef;
    		// Compute fx, fy, fz
    		fx = (x > threshold) ? Math.pow(x, (1.0/3.0)) : (7.787 * x + (16.0/116.0));
    		fy = (y > threshold) ? Math.pow(y, (1.0/3.0)) : (7.787 * y + (16.0/116.0));
    		fz = (z > threshold) ? Math.pow(z, (1.0/3.0)) : (7.787 * z + (16.0/116.0));
    		// Compute Lab color value
    		x_l[n] = (y > threshold) ? (116.0 * Math.pow(y, (1.0/3.0)) - 16.0) : (903.3 * y);
    		y_a[n] = 500.0 * (fx - fy);
    		z_b[n] = 200.0 * (fy - fz);
    	}
    	return;
    }
    
    /**
     * Normalize an Lab image so that values for each channel lie in [0,1].
     * @param l
     * @param a
     * @param b
     * @return
     */
    private void lab_normalize(double l[], double a[], double b[]) {
        // Range for a, b channels
    	final double abMin = -73.0;
    	final double abMax = 95.0;
    	final double abRange = abMax - abMin;
    	// Normalize Lab image
    	int n;
    	double lVal;
    	double aVal;
    	double bVal;
    	for (n = 0; n < l.length; n++) {
    		lVal = l[n]/100.0;
    		aVal = (a[n] - abMin)/abRange;
    		bVal = (b[n] - abMin)/abRange;
    		if (lVal < 0.0) {
    			lVal = 0.0;
    		}
    		else if (lVal > 1.0) {
    			lVal = 1.0;
    		}
    		if (aVal < 0.0) {
    			aVal = 0.0;
    		}
    		else if (aVal > 1.0) {
    			aVal = 1.0;
    		}
    		if (bVal < 0.0) {
    			bVal = 0.0;
    		}
    		else if (bVal > 1.0) {
    			bVal = 1.0;
    		}
    		l[n] = lVal;
    		a[n] = aVal;
    		b[n] = bVal;
    	}
    }
    
    private void quantize_values(int assign[], double m[], int n_bins) {
    	int n;
    	int bin;
    	for (n = 0; n < m.length; n++) {
    	    bin = (int)Math.floor(m[n]*n_bins);
	    	if (bin == n_bins) {
	    		bin = n_bins-1;
	    	}
	    	assign[n] = bin;
    	}
    }
    
    /**
     * Filters for computing textons.
     * 
     * The set of texton filters is the union of the even and odd-symmetric
     * filter sets in aditon to a center-surround difference of Gaussians filter.
     * 
     * Each returned filter is an (s + 1) x (s + 1) matrix where s = 3 * sigma and
     * sigma is the specified standard deviation.
     * 
     * Filters are created in parallel when possible
     * @param filters
     * @param n_ori
     * @param sigma
     */
    private void texton_filters(ArrayList <double[][]> filters, int n_ori, double sigma) {
    	// Allocate collection to hold filters
    	int i;
    	ArrayList <double[][]> filters_even = new ArrayList <double[][]>();
    	ArrayList <double[][]> filters_odd = new ArrayList <double[][]>();
    	oe_filters_even(n_ori, sigma, filters_even);
    	oe_filters_odd(n_ori, sigma, filters_odd);
    	for (i = 0; i < filters_even.size(); i++) {
    		filters.add(filters_even.get(i));
    	}
    	filters_even.clear();
    	for (i = 0; i < filters_odd.size(); i++) {
    		filters.add(filters_odd.get(i));
    	}
    	filters_odd.clear();
    	/* compute center surround filter */
    	int support = (int)Math.ceil(3*sigma);
    	double [][]f_cs = gaussian_cs_2D(sigma, sigma, 0, Math.sqrt(2.0), support, support);
    	/* add center surround filter to collection */
    	filters.add(f_cs);
    	return;
    }
    
    private void oe_filters_even(int n_ori, double sigma, ArrayList <double[][]> filters_even) {
    	gaussian_filters(n_ori, sigma, 2, false, 3.0, filters_even);
    	return;
    }
    
    private void oe_filters_odd(int n_ori, double sigma, ArrayList <double[][]> filters_odd) {
    	gaussian_filters(n_ori, sigma, 2, true, 3.0, filters_odd);
    	return;
    }
    
    private void gaussian_filters(int n_ori, double sigma, int deriv, boolean hlbrt, double elongation,
    		ArrayList <double[][]> filters) {
    	double oris[] = new double[n_ori];
    	standard_filter_orientations(n_ori, oris);
    	gaussian_filters(oris, sigma, deriv, hlbrt, elongation, filters);
    	return;
    }
    
    private void standard_filter_orientations(int n_ori, double[] oris) {
    	int n;
    	double ori = 0.0;
    	double ori_step = (n_ori > 0) ? (Math.PI/(double)n_ori) : 0;
    	for (n = 0; n < n_ori; n++, ori += ori_step) {
    		oris[n] = ori;
    	}
    	return;
    }
    
    /**
     * Oriented Gaussian derivative filters
     * 
     * Create a filter set consisting of rotated versions of the Gaussian
     * derivative filter with the specified parameters.
     * 
     * Specify the standard deviation (sigma) along the longer principle axis.
     * The standard deviation along the other principle axisw is sigma/r where
     * r is the elongation ratio.
     * 
     * Each returned filter is an (s+1) x (s+1) matrix where s = 3 sigma.                       
     * @param oris
     * @param sigma
     * @param deriv
     * @param hlbrt
     * @param elongation
     * @param filters
     */
    private void gaussian_filters(double[] oris, double sigma, int deriv, boolean hlbrt, double elongation,
        ArrayList <double[][]> filters) {
    	int n;
    	// Compute support from sigma
    	int support = (int)Math.ceil(3 * sigma);
    	double sigmaX = sigma;
    	double sigmaY = sigmaX/elongation;
    	// Setup filter creators
    	int n_ori = oris.length;
    	for (n = 0; n < n_ori; n++) {
    	    double f[][] = gaussian_2D(sigmaX, sigmaY, oris[n], deriv, hlbrt, support, support);	
    	    filters.add(f);
    	}
    	return;
    }
    
    /**
     * Gaussian kernel (2D)
     * The kernel is normalized to have unit L1 norm.  If returning a 1st or 2nd derivative,
     * the kernel has zero mean.
     * @param sigmaX standard deviation along x axis
     * @param sigmaY standard deviation along y axis
     * @param ori orientation in radians
     * @param deriv The 1st or 2nd derivative can be taken along the y-axis prior to rotation
     * @param hlbrt The hilbert transform can be taken along the y-axis prior to rotation
     * @param supportX
     * @param supportY
     * @return
     */
    private double[][] gaussian_2D(double sigmaX, double sigmaY, double ori, int deriv, boolean hlbrt, int supportX,
    		int supportY) {
    	double m[][] = null;
    	// Compute size of larger grid for axis-aligned gaussian
    	// Reverse rotate corners of bounding box by orientation
    	int support_x_rot = support_x_rotated(supportX, supportY, -ori);
    	int support_y_rot = support_y_rotated(supportX, supportY, -ori);
    	// Compute 1D kernels
    	double mx[] = gaussian(sigmaX, 0, false, support_x_rot);
    	double my[] = gaussian(sigmaY, deriv, hlbrt, support_y_rot);
    	// Compute 2D kernel from product of 1D kernels
    	m = new double[mx.length][my.length];
    	for (int nx = 0; nx < mx.length; nx++) {
    		for (int ny = 0; ny < my.length; ny++) {
    			m[nx][ny] = mx[nx] * my[ny];
    		}
    	}
    	// Rotate 2D kernel by orientation
    	m = rotate_2D_crop(m, ori, 2*supportX + 1, 2*supportY + 1);
    	// Make zero mean (if returning derivative)
    	if (deriv > 0) {
    		double sum = 0.0;
    		for (int nx = 0; nx < m.length; nx++) {
    			for (int ny = 0; ny < m[0].length; ny++) {
    				sum += m[nx][ny];
    			}
    		}
    		double mean = sum/(m.length * m[0].length);
    		for (int nx = 0; nx < m.length; nx++) {
    			for (int ny = 0; ny < m[0].length; ny++) {
    				m[nx][ny] -= mean;
    			}
    		}
    	} // if (deriv > 0)
    	// Make unit L1 norm
    	double sum = 0.0;
    	for (int nx = 0; nx < m.length; nx++) {
			for (int ny = 0; ny < m[0].length; ny++) {
				sum += Math.abs(m[nx][ny]);
			}
		}
    	for (int nx = 0; nx < m.length; nx++) {
			for (int ny = 0; ny < m[0].length; ny++) {
			    m[nx][ny] = m[nx][ny]/sum;	
			}
    	}
    	return m;
    }
    
    /**
     * Gaussian center-surround kernel (2D).
     * 
     * Specify the standard deviation along each axis, the
     * orientation in radians, and the support.
     * 
     * The center-surround kernel is the difference of a Gaussian with the
     * specified standard deivation and one with a standard deviation scaled
     * by the specified factor.
     * 
     * The kernel is normalized to have unit L1 norm and zero mean.
     * @param sigmaX
     * @param sigmaY
     * @param ori
     * @param scaleFactor
     * @param supportX
     * @param supportY
     * @return
     */
    private double[][] gaussian_cs_2D(double sigmaX, double sigmaY, double ori, double scaleFactor,
    		int supportX, int supportY) {
    	// Compute standard deviation for center kernel
    	double sigmaXC = sigmaX/scaleFactor;
    	double sigmaYC = sigmaY/scaleFactor;
    	// Compute center and surround kernels
    	double m_center[][] = gaussian_2D(sigmaXC, sigmaYC, ori, 0, false, supportX, supportY);
    	double m_surround[][] = gaussian_2D(sigmaX, sigmaY, ori, 0, false, supportX, supportY);
    	// Compute center-surround kernel
    	double m[][] = new double[m_center.length][m_center[0].length];
    	int x;
    	int y;
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    			m[x][y] = m_surround[x][y] - m_center[x][y];
    		}
    	}
    	// Make zero mean and unit L1 norm
    	double sum = 0.0;
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    			sum += m[x][y];
    		}
    	}
    	double mean = sum/(m.length * m[0].length);
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    		    m[x][y] -= mean;	
    		}
        }
    	sum = 0.0;
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    			sum += Math.abs(m[x][y]);
    		}
    	}
    	for (x = 0; x < m.length; x++) {
    		for (y = 0; y < m[0].length; y++) {
    		    m[x][y] /= sum;	
    		}
    	}
    	return m;
    }
    
    /*
     * Rotate and pad with zeros, but crop the result to the specified size.
     */
    private double[][] rotate_2D_crop(
       double m[][], double ori, int size_x, int size_y)
    {
       /* compute rotation */
       double m_rot[][] = new double[size_x][size_y];
       compute_rotate_2D(m, m_rot, ori);
       return m_rot;
    }
    
    /*
     * Compute rotated 2D matrix using bilinear interpolation.
     */
    private void compute_rotate_2D(
       double[][]      m_src,       /* source matrix */
       double[][]      m_dst,       /* destination matrix */
       double ori)                /* orientation */
    {
       int size_x_src = m_src.length;
       int size_y_src = m_src[0].length;
       int size_x_dst = m_dst.length;
       int size_y_dst = m_dst[0].length;
       /* check that matrices are nonempty */
       if ((size_x_src > 0) && (size_y_src > 0) &&
           (size_x_dst > 0) && (size_y_dst > 0))
       {
          /* compute sin and cos of rotation angle */
          final double cos_ori = Math.cos(ori);
          final double sin_ori = Math.sin(ori);
          /* compute location of origin in src */
          final double origin_x_src = (size_x_src - 1.0)/ 2.0;
          final double origin_y_src = (size_y_src - 1.0) / 2.0;
          /* rotate */
          double u = -((size_x_dst - 1.0) / 2.0);
          int n = 0;
          for (int dst_x = 0; dst_x < size_x_dst; dst_x++) {
             double v = -((size_y_dst - 1.0) / 2.0);
             for (int dst_y = 0; dst_y < size_y_dst; dst_y++) {
                /* reverse rotate by orientation and shift by origin offset */
                double x = u * cos_ori + v * sin_ori + origin_x_src;
                double y = v * cos_ori - u * sin_ori + origin_y_src;
                /* check that location is in first quadrant */
                if ((x >= 0) && (y >= 0)) {
                   /* compute integer bounds on location */
                   int x0 = (int)Math.floor(x);
                   int x1 = (int)Math.ceil(x);
                   int y0 = (int)Math.floor(y);
                   int y1 = (int)Math.ceil(y);
                   /* check that location is within src matrix */
                   if ((0 <= x0) && (x1 < size_x_src) &&
                       (0 <= y0) && (y1 < size_y_src))
                   {
                      /* compute distances to bounds */
                      double dist_x0 = x - x0;
                      double dist_x1 = x1 - x;
                      double dist_y0 = y - y0;
                      double dist_y1 = y1 - y;
                      /* grab matrix elements */
                      final double m00 = m_src[x0][y0];
                      final double m01 = m_src[x0][y1];
                      final double m10 = m_src[x1][y0];
                      final double m11 = m_src[x1][y1];
                      /* interpolate in x-direction */
                      final double t0 =
                         (x0 != x1) ? (dist_x1 * m00 + dist_x0 * m10) : m00;
                      final double t1 =
                         (x0 != x1) ? (dist_x1 * m01 + dist_x0 * m11) : m01;
                      /* interpolate in y-direction */
                      m_dst[n/size_y_dst][n % size_y_dst] = (y0 != y1) ? (dist_y1 * t0 + dist_y0 * t1) : t0;
                   }
                }
                /* increment coordinate */
                n++;
                v++;
             }
             u++;
          }
       }
    }


    
    /*
     * Compute integer-valued supports for rotated 2D matrix.
     */
    private int support_x_rotated(
       int support_x, int support_y, double ori)
    {
       return (int)(
          Math.ceil(support_x_rotated(
             (double)(support_x), (double)(support_y), ori
          ))
       );
    }

    private int support_y_rotated(
       int support_x, int support_y, double ori)
    {
       return (int)(
          Math.ceil(support_y_rotated(
             (double)(support_x), (double)(support_y), ori
          ))
       );
    }

    
    private double support_x_rotated(double support_x, double support_y, double ori) {
    	   final double sx_cos_ori = support_x * Math.cos(ori);
    	   final double sy_sin_ori = support_y * Math.sin(ori);
    	   double x0_mag = Math.abs(sx_cos_ori - sy_sin_ori);
    	   double x1_mag = Math.abs(sx_cos_ori + sy_sin_ori);
    	   return (x0_mag > x1_mag) ? x0_mag : x1_mag;
    	}

    private double support_y_rotated(double support_x, double support_y, double ori) {
    	   final double sx_sin_ori = support_x * Math.sin(ori);
    	   final double sy_cos_ori = support_y * Math.cos(ori);
    	   double y0_mag = Math.abs(sx_sin_ori - sy_cos_ori);
    	   double y1_mag = Math.abs(sx_sin_ori + sy_cos_ori);
    	   return (y0_mag > y1_mag) ? y0_mag : y1_mag;
    	}
    
    private double[] gaussian(
    		   double sigma, int deriv, boolean hlbrt)
    		{
    		   int support = (int)Math.ceil(3*sigma);
    		   return gaussian(sigma, deriv, hlbrt, support);
    		}

    
    /**
     * Gaussian kernel (1D).
     * The length of the returned vector is 2*support + 1.
     * The kernel is normalized to have unit L1 norm.
     * If returning a 1st or 2nd derivative, the kernel has zero mean
     * @param sigma
     * @param deriv
     * @param hlbrt
     * @param support
     * @return
     */
    private double[] gaussian(double sigma, int deriv, boolean hlbrt, int support) {
    	double m[] = null;
    	int n;
    	int i;
    	// Enlarge support so that hilbert transform can be done efficiently.
    	int support_big = support;
    	if (hlbrt) {
    		support_big = 1;
    		int temp = support;
    		while (temp > 0) {
    			support_big *= 2;
    			temp /= 2;
    		}
    	}
    	// Compute constants
    	final double sigma2_inv = 1.0/(sigma * sigma);
    	final double neg_two_sigma2_inv = -0.5 * sigma2_inv;
    	// Compute gaussian (or gaussian derivative).
    	int size = 2*support_big + 1;
    	m = new double[size];
    	double x = -(double)support_big;
    	if (deriv == 0) {
    		// Compute gaussian
    		for (n = 0; n < size; n++, x++) {
    		    m[n] = Math.exp(x*x*neg_two_sigma2_inv);	
    		}
    	}
    	else if (deriv == 1) {
    		// Compute gaussian first derivative
    		for (n = 0; n < size; n++, x++) {
    			m[n] = Math.exp(x*x*neg_two_sigma2_inv) * (-x);
    		}
    	}
    	else if (deriv == 2) {
    		// Compute gaussian second derivative
    		for (n = 0; n < size; n++, x++) {
    			double x2 = x * x;
    			m[n] = Math.exp(x2 * neg_two_sigma2_inv) * (x2*sigma2_inv - 1);
    		}
    	}
    	else {
    		MipavUtil.displayError("Only derivatives 0, 1, and 2 supported");
    		return null;
    	}
    	// Take hilbert transform (if requested)
    	if (hlbrt) {
    		// Grab power of two sized submatrix (ignore last element)
    		double mtemp[] = new double[m.length-1];
    		for (i = 0; i < m.length-1; i++) {
    			mtemp[i] = m[i];
    		}
    		m = null;
    		m = new double[mtemp.length];
    		for (i = 0; i < mtemp.length; i++) {
    			m[i] = mtemp[i];
    		}
    		mtemp = null;
    		// Grab desired submatrix after hilbert transform
    		int start = support_big - support;
    		int end = start + support + support;
    		m = hilbert(m);
       	    mtemp = new double[end - start + 1];
       	    for (i = start; i <= end; i++) {
       	    	mtemp[i-start] = m[i];
       	    }
       	    m = null;
       	    m = new double[mtemp.length];
       	    for (i = 0; i < mtemp.length; i++) {
       	    	m[i] = mtemp[i];
       	    }
       	    mtemp = null;
    	}
    	double sum;
    	// Make zero mean (if returning derivative)
    	if (deriv > 0) {
    		 sum = 0.0;
    		for (i = 0; i < m.length; i++) {
    			sum += m[i];
    		}
    		double mean = sum/m.length; 
    		for (i = 0; i < m.length; i++) {
    			m[i] = m[i] - mean;
    		}
    	} // if (deriv > 0)
    	// Make unit L1 norm
    	sum = 0.0;
    	for (i = 0; i < m.length; i++) {
    		sum += Math.abs(m[i]);
    	}
    	for (i = 0; i < m.length; i++) {
    		m[i] = m[i]/sum;
    	}
    	return m;
    }
    
    private double[] hilbert(double[] m) {
    	   /* get # steps along dimension */
    	   double mImag[] = new double[m.length];
    	   int n_steps = m.length;
    	   int half_n_steps = n_steps/2;
    	   /* compute fourier transform */
    	   FFTUtility fft = new FFTUtility(m, mImag, 1, m.length, 1,
    		-1, FFTUtility.FFT);
    	   fft.setShowProgress(false);
    	   fft.run();
    	   fft.finalize();
    	   fft = null;
    	   /* compute step size along dimension */
    	   int step_size = 1;
    	   /* double positive frequencies   */
    	   /* zero out negative frequencies */
    	   /* leave dc component            */
    	   boolean is_mult_two = (n_steps == (half_n_steps*2));
    	   int pos_freq_bound_ind = (n_steps+1)/2;
    	    int ind = 0;
    	    /* double positive frequencies */
    	    ind++;
    	    for (int n_step = 1; n_step < pos_freq_bound_ind; n_step++)
    	    {
    	       m[ind] *= 2;
    	       mImag[ind] *= 2;
    	       ind++;
    	    }
    	    /* zero out negative frequencies */
    	    ind += (is_mult_two ? step_size : 0);
    	    for (int n_step = (half_n_steps+1); n_step < n_steps; n_step++)
    	    {
    	       m[ind] = 0;
    	       mImag[ind] = 0;
    	       ind++;
    	    }
    	   /* compute inverse fourier transform */
    	    fft = new FFTUtility(m, mImag, 1, m.length, 1,
					+1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
    	   /* grab and return imaginary component */
    	   return mImag;
    	}
    
    
    


    
}
