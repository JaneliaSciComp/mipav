package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlurSep;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

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
    
    private float gaussianSigma = 3.0f;
    
    private int maxEMIterations = 10;
    
    private int maxMAPIterations = 10;
    
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
     * @param gaussianSigma
     * @param maxEMIterations
     * @param maxMAPIterations
     */
    public AlgorithmHMRF_EM(ModelImage destImg, ModelImage srcImg, int colorWeighting, float gaussianSigma,
                            int maxEMIterations, int maxMAPIterations) {
        super(destImg, srcImg);
        this.colorWeighting = colorWeighting;
        this.gaussianSigma = gaussianSigma;
        this.maxEMIterations = maxEMIterations;
        this.maxMAPIterations = maxMAPIterations;
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
        ModelImage blurredImage;
        boolean wholeImage = true;
        boolean image25D = false;
        float sigmas[] = new float[]{gaussianSigma, gaussianSigma};
        float blurredBuffer[];
        int numberClusters = 2;
        boolean bwSegmentedImage = true;
        double scale[];
        double doubleBuffer[];
        int nPoints;
        int groupNum[];
        double weight[];
        double pos[][];
        int nval;
        ModelImage segmentedImage;
        double centroidPos[][];
        AlgorithmKMeans kMeansAlgo;
        int algoSelection = AlgorithmKMeans.K_MEANS;
        int distanceMeasure = AlgorithmKMeans.EUCLIDEAN_SQUARED;
        String resultsFileName = srcImage.getImageFileName() +  "_kmeans.txt";
        int initSelection = AlgorithmKMeans.BRADLEY_FAYYAD_INIT;
        float redBuffer[] = null;
        float greenBuffer[] = null;
        float blueBuffer[] = null;
        // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
        double scaleMax = 255.0;
        boolean useColorHistogram = false;
        boolean scaleVariablesToUnitVariance = false;
        double axesRatio[] = null;
        boolean showSegmentedImage = true;
        // groupMean[kMeansNDims][numberClusters] = groupMean[1][2]
        double groupMean[][];
        // groupStdDev[kMeansNDims][numberClusters] = groupStdDev[1][2]
        double groupStdDev[][];
        
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
        
        blurredImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_blurred");
        try {
            blurredImage.importData(0, srcBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on blurredImage.importData");
            
            setCompleted(false);

            return;    
        }
        gaussAlgo = new AlgorithmGaussianBlurSep(blurredImage, sigmas, wholeImage, image25D);
        gaussAlgo.run();
        blurredBuffer = gaussAlgo.getResultBuffer();
        gaussAlgo.finalize();
        gaussAlgo = null;
        
        try {
            blurredImage.importData(0, blurredBuffer, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on blurredImage.importData");
            
            setCompleted(false);

            return;       
        }
        
        bwSegmentedImage = true;
        scale = new double[1];
        scale[0] = 1.0;
        doubleBuffer = new double[sliceSize];
        for (i = 0; i < sliceSize; i++) {
            doubleBuffer[i] = blurredBuffer[i];
        }
        Arrays.sort(doubleBuffer);
        nPoints = 1;
        for (i = 1; i < sliceSize; i++) {
            if (doubleBuffer[i] > doubleBuffer[i-1]) {
                nPoints++;
            }
        }
        groupNum = new int[nPoints];
        weight = new double[nPoints];
        pos = new double[1][nPoints];
        nval = 0;
        weight[nval] = 1.0;
        pos[0][nval] = doubleBuffer[0];
        for (i = 1; i < sliceSize; i++) {
            if (doubleBuffer[i] == doubleBuffer[i-1]) {
                weight[nval] += 1.0;
            }
            else {
                nval++;
                weight[nval] = 1.0;
                pos[0][nval] = doubleBuffer[i];
            }
        }
        for (i = 0; i < sliceSize; i++) {
            doubleBuffer[i] = blurredBuffer[i];
        }
        
        segmentedImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(),
                srcImage.getImageFileName() +  "_kmeans"); 
        segmentedImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        centroidPos = new double[1][numberClusters];
        
        kMeansAlgo = new AlgorithmKMeans(segmentedImage,algoSelection,distanceMeasure,pos,scale,groupNum,weight,centroidPos,resultsFileName,
                initSelection,redBuffer, greenBuffer, blueBuffer, scaleMax,
                useColorHistogram, scaleVariablesToUnitVariance, axesRatio,
                bwSegmentedImage, doubleBuffer, showSegmentedImage);
        kMeansAlgo.run();
        groupMean = kMeansAlgo.getGroupMean();
        groupStdDev = kMeansAlgo.getGroupStdDev();
        kMeansAlgo.finalize();
        kMeansAlgo = null;
    }
}