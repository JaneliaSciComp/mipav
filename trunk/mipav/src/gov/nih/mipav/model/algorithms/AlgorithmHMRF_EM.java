package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

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
     */
    public AlgorithmHMRF_EM(ModelImage destImg, ModelImage srcImg, int colorWeighting) {
        super(destImg, srcImg);
        this.colorWeighting = colorWeighting;
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
        } // if (srcImage.isColorImage())
        else {
            try {
                srcImage.exportData(0, sliceSize, srcBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
    
                setCompleted(false);
    
                return;
            }    
        }
    }
}