package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlurSep;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.*;
import java.util.Arrays;

/**
This is a port of the MATLAB files HMRF_EM.m and MRF_MAP.m written by Quan Wang to Java.

%   Copyright by Quan Wang, 2012/04/25
%   Please cite: Quan Wang. HMRF-EM-image: Implementation of the 
%   Hidden Markov Random Field Model and its Expectation-Maximization 
%   Algorithm. arXiv:1207.3510 [cs.CV], 2012.
 
Copyright (c) 2012, Quan Wang
All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are 
met:

    * Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright 
      notice, this list of conditions and the following disclaimer in 
      the documentation and/or other materials provided with the distribution
      
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.*/


public class AlgorithmHMRF_EM extends AlgorithmBase {
    
    private final static int EQUAL_WEIGHTS = 1;
    
    private final static int GRAPHIC_WEIGHTS = 2;
    
    private int colorWeighting = 1;
    
    private int numberClusters = 2;
    
    private float gaussianSigma = 3.0f;
    
    private int maxEMIterations = 10;
    
    private int maxMAPIterations = 10;
    
    private boolean show_plot  = false;
    
    private String resultsFileName;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHMRF_EM - default constructor.
     */
    public AlgorithmHMRF_EM() { }

    /**
     * AlgorithmHMRF_EM.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param colorWeighting
     * @param numberClusters
     * @param gaussianSigma
     * @param maxEMIterations
     * @param maxMAPIterations
     * @param show_plot
     * @param resultsFileName
     */
    public AlgorithmHMRF_EM(ModelImage destImg, ModelImage srcImg, int colorWeighting, int numberClusters, float gaussianSigma,
                            int maxEMIterations, int maxMAPIterations, boolean show_plot, String resultsFileName) {
        super(destImg, srcImg);
        this.colorWeighting = colorWeighting;
        this.numberClusters = numberClusters;
        this.gaussianSigma = gaussianSigma;
        this.maxEMIterations = maxEMIterations;
        this.maxMAPIterations = maxMAPIterations;
        this.show_plot = show_plot;
        this.resultsFileName = resultsFileName;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        int xDim;
        int yDim;
        int sliceSize;
        float colorBuffer[];
        double srcBuffer[];
        ModelImage entropicEdgeImage;
        AlgorithmEntropicEdgeDetection entropicAlgo;
        ModelImage grayImage;
        AlgorithmGaussianBlurSep gaussAlgo;
        boolean wholeImage = true;
        boolean image25D = false;
        float sigmas[] = new float[]{gaussianSigma, gaussianSigma};
        float blurredBuffer[];
        boolean bwSegmentedImage = true;
        double scale[];
        double y[];
        int nPoints;
        int groupNum[];
        double weight[];
        double pos[][];
        int nval;
        double centroidPos[][];
        AlgorithmKMeans kMeansAlgo;
        int algoSelection = AlgorithmKMeans.K_MEANS;
        int distanceMeasure = AlgorithmKMeans.EUCLIDEAN_SQUARED;
        String kMeansFileName = srcImage.getImageFileName() +  "_kmeans.txt";
        int initSelection = AlgorithmKMeans.BRADLEY_FAYYAD_INIT;
        float redBuffer[] = null;
        float greenBuffer[] = null;
        float blueBuffer[] = null;
        // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
        double scaleMax = 255.0;
        boolean useColorHistogram = false;
        boolean scaleVariablesToUnitVariance = false;
        double axesRatio[] = null;
        boolean showKMeansSegmentedImage = false;
        // groupMean[kMeansNDims][numberClusters] = groupMean[1][2]
        double groupMean[][];
        // groupStdDev[kMeansNDims][numberClusters] = groupStdDev[1][2]
        double groupStdDev[][];
        double P_lyi[][];
        double sum_U[];
        int it;
        byte X[];
        double U[][];
        double sum_U_MAP[];
        int it2;
        double U1[][];
        double U2[][];
        int L;
        double yi[];
        double mu[];
        double sigma[];
        double temp1[];
        double temp2[];
        double temp3[];
        int j;
        int ind;
        int xpos;
        int ypos;
        double u;
        double u2;
        byte Z[];
        double temp[];
        double latest3Sum;
        double latest3Mean;
        double latest3SumOfSquares;
        double diff;
        double latest3StdDev;
        float xInit[];
        float yInit[];
        double a;
        double b;
        double sumP;
        File file;
        RandomAccessFile raFile;
        String dataString;
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running HMRF_EM ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        srcBuffer = new double[sliceSize];
        int i;
        
        if (srcImage.isColorImage()) {
            colorBuffer = new float[4 * sliceSize];
            
            try {
                srcImage.exportData(0, 4 * sliceSize, colorBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
    
                setCompleted(false);
    
                return;
            }
            
            for (i = 0; i < sliceSize; i++) {
                if (colorWeighting == EQUAL_WEIGHTS) {
                    srcBuffer[i] = (1.0/3.0) * (colorBuffer[4*i + 1] + colorBuffer[4*i + 2] + colorBuffer[4*i + 3]);
                }
                else {
                    srcBuffer[i] = 0.299 * colorBuffer[4*i + 1] + 0.587 * colorBuffer[4*i + 2] + 0.114 * colorBuffer[4*i + 3];
                }
            }
            
            grayImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_gray");
            
            try {
                grayImage.importData(0, srcBuffer, true);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on grayImage.importData");
                
                setCompleted(false);
    
                return;    
            }
        } // if (srcImage.isColorImage())
        else {
            try {
                srcImage.exportData(0, sliceSize, srcBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
    
                setCompleted(false);
    
                return;
            } 
            
            grayImage = srcImage;
        }
        
        // A Canny edge detector in the original requires a high threshold, a low threshold, and a sigma
        // Use an entropic edge detector which requires no input parameters
        entropicEdgeImage = new ModelImage(ModelStorageBase.SHORT, srcImage.getExtents(), srcImage.getImageName()+"_entropicEdge");
        entropicAlgo = new AlgorithmEntropicEdgeDetection(entropicEdgeImage, grayImage);
        entropicAlgo.run();
        entropicAlgo.finalize();
        entropicAlgo = null;
        Z = new byte[sliceSize];
        try {
            entropicEdgeImage.exportData(0, sliceSize, Z);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on entropicEdgeImage.exportData(0, sliceSize, Z)");
            setCompleted(false);
            return;
        }
        
        gaussAlgo = new AlgorithmGaussianBlurSep(grayImage, sigmas, wholeImage, image25D);
        gaussAlgo.run();
        blurredBuffer = gaussAlgo.getResultBuffer();
        gaussAlgo.finalize();
        gaussAlgo = null;
        
        bwSegmentedImage = true;
        scale = new double[1];
        scale[0] = 1.0;
        y = new double[sliceSize];
        for (i = 0; i < sliceSize; i++) {
            y[i] = blurredBuffer[i];
        }
        Arrays.sort(y);
        nPoints = 1;
        for (i = 1; i < sliceSize; i++) {
            if (y[i] > y[i-1]) {
                nPoints++;
            }
        }
        groupNum = new int[nPoints];
        weight = new double[nPoints];
        pos = new double[1][nPoints];
        nval = 0;
        weight[nval] = 1.0;
        pos[0][nval] = y[0];
        for (i = 1; i < sliceSize; i++) {
            if (y[i] == y[i-1]) {
                weight[nval] += 1.0;
            }
            else {
                nval++;
                weight[nval] = 1.0;
                pos[0][nval] = y[i];
            }
        }
        for (i = 0; i < sliceSize; i++) {
            y[i] = blurredBuffer[i];
        }
        
        /*segmentedImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(),
                srcImage.getImageFileName() +  "_segmented"); 
        segmentedImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());*/
        centroidPos = new double[1][numberClusters];
        
        kMeansAlgo = new AlgorithmKMeans(destImage,algoSelection,distanceMeasure,pos,scale,groupNum,weight,centroidPos,kMeansFileName,
                initSelection,redBuffer, greenBuffer, blueBuffer, scaleMax,
                useColorHistogram, scaleVariablesToUnitVariance, axesRatio,
                bwSegmentedImage, y, showKMeansSegmentedImage);
        kMeansAlgo.run();
        groupMean = kMeansAlgo.getGroupMean();
        mu = groupMean[0];
        groupStdDev = kMeansAlgo.getGroupStdDev();
        sigma = groupStdDev[0];
        kMeansAlgo.finalize();
        kMeansAlgo = null;
        X = new byte[sliceSize];
        try {
            destImage.exportData(0, sliceSize, X);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.exportData(0, sliceSize, X)");
            setCompleted(false);
            return;
        }
        
        P_lyi = new double[numberClusters][sliceSize];
        sum_U = new double[maxEMIterations];
        sum_U_MAP = new double[maxMAPIterations];
        yi = new double[sliceSize];
        temp1 = new double[sliceSize];
        temp2 = new double[sliceSize];
        U = new double[sliceSize][numberClusters];
        U1 = new double[sliceSize][numberClusters];
        U2 = new double[sliceSize][numberClusters];
        temp = new double[sliceSize];
        temp3 = new double[sliceSize];
        
        for (it = 1; it <= maxEMIterations; it++) {
            Preferences.debug("EM iteraton = " + it + "\n", Preferences.DEBUG_ALGORITHM); 
            // Update X
            for (i = 0; i < sliceSize; i++) {
                for (j = 0; j < numberClusters; j++) {
                    U[i][j] = 0.0;
                }
            }
            for (i = 0; i < maxMAPIterations; i++) {
                sum_U_MAP[i] = 0.0;
            }
            for (it2 = 1; it2 <= maxMAPIterations; it2++) {
                Preferences.debug("Inner MAP iteration = " + it2 + "\n", Preferences.DEBUG_ALGORITHM);
                for (i = 0; i < sliceSize; i++) {
                    for (j = 0; j < numberClusters; j++) {
                        U1[i][j] = U[i][j];
                        U2[i][j] = U[i][j];
                    }
                }
                
                for (L = 0; L < numberClusters; L++) {
                    for (i = 0; i < sliceSize; i++) {
                        yi[i] = y[i] - mu[L];
                        temp1[i] = (yi[i] * yi[i])/(2.0 * sigma[L] * sigma[L]);
                        temp1[i] = temp1[i] + Math.log(sigma[L]);
                        U1[i][L] = U1[i][L] + temp1[i];
                    }
                    
                    for (ind = 0; ind < sliceSize; ind++) {
                        xpos = ind % xDim;
                        ypos = ind / xDim;
                        u2 = 0.0;
                        if ((xpos > 0) && (Z[xpos - 1 + xDim*ypos] == 0)) {
                            if (L != X[xpos - 1 + xDim*ypos]) {
                                u2 = u2 + 0.5;
                            }
                        }
                        if ((xpos < xDim - 1) && (Z[xpos + 1 + xDim*ypos] == 0)) {
                            if (L != X[xpos + 1 + xDim*ypos]) {
                                u2 = u2 + 0.5;
                            }
                        }
                        if ((ypos > 0) && (Z[xpos + xDim*(ypos-1)] == 0)) {
                            if (L != X[xpos + xDim*(ypos-1)]) {
                                u2 = u2 + 0.5;
                            }
                        }
                        if ((ypos < yDim - 1) && (Z[xpos + xDim*(ypos+1)] == 0)) {
                            if (L != X[xpos + xDim*(ypos+1)]) {
                                u2 = u2 + 0.5;
                            }
                        }
                        U2[ind][L] = u2;
                    } // for (ind = 0; ind < sliceSize; ind++)
                } // for (L = 0; L < numberClusters; L++)
                for (i = 0; i < sliceSize; i++) {
                    temp[i] = Double.MAX_VALUE;
                    X[i] = -1;
                    for (j = 0; j < numberClusters; j++) {
                        U[i][j] = U1[i][j] + U2[i][j];
                        if (U[i][j] < temp[i]) {
                            temp[i] = U[i][j];
                            X[i] = (byte)j;
                        }
                    }
                    sum_U_MAP[it2-1] += temp[i];
                } // for (i = 0; i < sliceSize; i++)
                
                if (it2 >= 3) {
                    latest3Sum = sum_U_MAP[it2-1] + sum_U_MAP[it2-2] + sum_U_MAP[it2-3];
                    latest3Mean = latest3Sum/3.0;
                    latest3SumOfSquares = 0.0;
                    for (i = 1; i <= 3; i++) {
                        diff = sum_U_MAP[it2-i] - latest3Mean;
                        latest3SumOfSquares += (diff * diff);
                    }
                    latest3StdDev = Math.sqrt(latest3SumOfSquares/2.0);
                    if ((latest3StdDev/sum_U_MAP[it2-1]) < 0.0001) {
                        break;
                    }
                } // if (it2 >= 3)
            } // for (it2 = 1; it2 <= maxMAPIterations; it2++)
            sum_U[it-1] = 0.0;
            for (ind = 0; ind < sliceSize; ind++) {
                sum_U[it-1] = sum_U[it-1] + U[ind][X[ind]];
            }
            if (show_plot) {
                xInit = new float[it2];
                for (i = 1; i <= it2; i++) {
                    xInit[i-1] = (float)i;
                }
                yInit = new float[it2];
                for (i = 0; i < it2; i++) {
                    yInit[i] = (float)sum_U_MAP[i];
                }
                new ViewJFrameGraph(xInit, yInit, "sum U MAP", "MAP iteration", "sum U MAP", Color.red);
            } // if (show_plot)
            
            // Update mu and sigma
            // Get P_lyi
            for (L = 0; L < numberClusters; L++) {
                a = 2.0 * sigma[L] * sigma[L];
                b = 1.0/Math.sqrt(a * Math.PI);
                for (i = 0; i < sliceSize; i++) {
                    diff = y[i] - mu[L];
                    temp1[i] = b * Math.exp(-diff * diff/a);
                    temp2[i] = 0;
                } // for (i = 0; i < sliceSize; i++)
                
                for (ind = 0; ind < sliceSize; ind++) {
                    xpos = ind % xDim;
                    ypos = ind / xDim;
                    u = 0.0;
                    if ((xpos > 0) && (Z[xpos - 1 + xDim*ypos] == 0)) {
                        if (L != X[xpos - 1 + xDim*ypos]) {
                            u = u + 0.5;
                        }
                    }
                    if ((xpos < xDim - 1) && (Z[xpos + 1 + xDim*ypos] == 0)) {
                        if (L != X[xpos + 1 + xDim*ypos]) {
                            u = u + 0.5;
                        }
                    }
                    if ((ypos > 0) && (Z[xpos + xDim*(ypos-1)] == 0)) {
                        if (L != X[xpos + xDim*(ypos-1)]) {
                            u = u + 0.5;
                        }
                    }
                    if ((ypos < yDim - 1) && (Z[xpos + xDim*(ypos+1)] == 0)) {
                        if (L != X[xpos + xDim*(ypos+1)]) {
                            u = u + 0.5;
                        }
                    }
                    temp2[ind] = u;
                } // for (ind = 0; ind < sliceSize; ind++)
                for (i = 0; i < sliceSize; i++) {
                    P_lyi[L][i] = temp1[i] * Math.exp(-temp2[i]);
                }
            } // for (L = 0; L < numberClusters; L++)
            for (i = 0; i < sliceSize; i++) {
                temp3[i] = P_lyi[0][i];
                for (j = 1; j < numberClusters; j++) {
                    temp3[i] += P_lyi[j][i];
                }
            } // for (i = 0; i < sliceSize; i++)
            for (i = 0; i < sliceSize; i++) {
                for (j = 0; j < numberClusters; j++) {
                    P_lyi[j][i] = P_lyi[j][i]/temp3[i];
                }
            } // for (i = 0; i < sliceSize; i++)
            
            // get mu and sigma
            for (L = 0; L < numberClusters; L++) {
                mu[L] = 0.0;
                sumP = 0.0;
                for (i = 0; i < sliceSize; i++) {
                    mu[L] += P_lyi[L][i] * y[i];
                    sumP += P_lyi[L][i];
                }
                mu[L] = mu[L]/sumP;
                sigma[L] = 0.0;
                for (i = 0; i < sliceSize; i++) {
                    diff = y[i] - mu[L];
                    sigma[L] += P_lyi[L][i] * diff * diff;
                }
                sigma[L] = sigma[L]/sumP;
                sigma[L] = Math.sqrt(sigma[L]);
            } // for (L = 0; L < numberClusters; L++)
            if (it >= 3) {
                latest3Sum = sum_U[it-1] + sum_U[it-2] + sum_U[it-3];
                latest3Mean = latest3Sum/3.0;
                latest3SumOfSquares = 0.0;
                for (i = 1; i <= 3; i++) {
                    diff = sum_U[it-i] - latest3Mean;
                    latest3SumOfSquares += (diff * diff);
                }
                latest3StdDev = Math.sqrt(latest3SumOfSquares/2.0);
                if ((latest3StdDev/sum_U[it-1]) < 0.0001) {
                    break;
                }
            } // if (it >= 3)
        } // for (it = 1; it <= maxEMIterations; it++)
        
        xInit = new float[it];
        for (i = 1; i <= it; i++) {
            xInit[i-1] = (float)i;
        }
        yInit = new float[it];
        for (i = 0; i < it; i++) {
            yInit[i] = (float)sum_U[i];
        }
        new ViewJFrameGraph(xInit, yInit, "sum of U in each EM iteration", "EM iteration", "sum of U");
        
        try {
            destImage.importData(0, X, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData(0, X, true)");
            setCompleted(false);
            return;
        }
        
        dataString = "Source image = " + srcImage.getImageFileName() + "\n";
        
        dataString += "Number of Clusters = " + numberClusters + "\n";
        
        for (L = 0; L < numberClusters; L++) {
            dataString += "Cluster number = " + (L+1) + " mean = " + mu[L] + " standard deviation " + sigma[L] + "\n";
            Preferences.debug("Cluster number = " + (L+1) + " mean = " + mu[L] + " standard deviation " + sigma[L] + "\n",
                    Preferences.DEBUG_ALGORITHM);
            System.out.println("Cluster number = " + (L+1) + " mean = " + mu[L] + " standard deviation " + sigma[L]);
        }
        
        Preferences.debug("Putting results in " + resultsFileName + "\n", Preferences.DEBUG_ALGORITHM);
        System.out.println("Putting results in " + resultsFileName);
        file = new File(resultsFileName);
        try {
        raFile = new RandomAccessFile( file, "rw" );
        }
        catch(FileNotFoundException e) {
            MipavUtil.displayError("new RandomAccessFile gave FileNotFoundException " + e);
            setCompleted(false);
            return;
        }
        
        // Necessary so that if this is an overwritten file there isn't any junk at the end
        try {
            raFile.setLength( 0 );
        }
        catch (IOException e) {
            MipavUtil.displayError("raFile.setLength(0) gave IOException " + e);
            setCompleted(false);
            return; 
        }
        
        try {
            raFile.write(dataString.getBytes());
        }
        catch (IOException e) {
            MipavUtil.displayError("raFile.write gave IOException " + e);
            setCompleted(false);
            return;     
        }
        try {
            raFile.close();
        }
        catch (IOException e) {
            MipavUtil.displayError("raFile.close gave IOException " + e);
            setCompleted(false);
            return;     
        }
        
        setCompleted(true);
        return;
    }
}