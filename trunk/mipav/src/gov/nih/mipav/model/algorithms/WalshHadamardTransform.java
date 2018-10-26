package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

/**Copyright (c) 2016, suresh
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
POSSIBILITY OF SUCH DAMAGE. 
*/

public class WalshHadamardTransform extends AlgorithmBase {

 // program to find the 2D Walsh-Hadamard Transform
 // specially helpful for grayscale images

 // Lots more @ www.surkur.blogspot.com

 // Please choose small images, large images take too long time for execution
 // Image size M=N square image
 // M=N=2 power of m
	
	public WalshHadamardTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg) {
		super(null, srcImg);
	}
	
	public void runAlgorithm() {
		int xDim;
    	int yDim;
    	int zDim;
    	int length;
    	int maxIters;
    	int X2;
    	int Y2;
    	int xTest;
    	int yTest;
    	int buffer[];
    	int z;
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        buffer = new int[length];
        zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        
     // Image must have even dimensions
        if (((xDim % 2) == 1) || ((yDim % 2) == 1)) {
        	MipavUtil.displayError("Image must have even dimensions");
        	setCompleted(false);
        	return;
        }
        
        X2 = 0;
        xTest = xDim;
        while ((xTest % 2) == 0) {
        	X2++;
        	xTest = xTest/2;
        }
        if (xTest != 1) {
        	MipavUtil.displayError("X dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        Y2 = 0;
        yTest = yDim;
        while ((yTest % 2) == 0) {
        	Y2++;
        	yTest = yTest/2;
        }
        if (yTest != 1) {
        	MipavUtil.displayError("Y dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Walsh-Hadamard Transform: Image(s) locked", true);

                return;
            }
        } // for (z = 0; z < zDim; z++)
	}
}
