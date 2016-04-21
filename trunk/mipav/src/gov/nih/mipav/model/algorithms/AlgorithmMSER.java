package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;

/**VLFeat is distributed under the BSD license:
Copyright (C) 2007-11, Andrea Vedaldi and Brian Fulkerson
Copyright (C) 2012-13, The VLFeat Team
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/** 
 * 
 * This is ported from files by Andrea Vedaldi and Brian Fulkerson Copyright 2007-2013.
 * These files are part of the VLFeat library and are made available under the terms
 * of the BSD license.
 *
 */


public class AlgorithmMSER extends AlgorithmBase {
	
	// Must be a non-negative number
	private double delta;
	
	private double epsilon;
	
	// Keep or remove duplicates
	private boolean duplicates;
	
	// Maximum region (relative) area.  Must be in the [0,1] range.
	private double max_area;
	
	// Minimum region (relative) area.  Must be in the [0,1] range.
	private double min_area;
	
	// Maximum absolute region stability.  Must be a non-negative number.
	private double max_variation;
	
	// Must be in the [0,1] range.
	private double min_diversity;
	
	// Enable or disable bright_on_dark regions.  bright_on_dark must be 0 or 1.
	private int bright_on_dark = 1;
	
	// Enable or disable dark_on_bright regions.  dark_on_bright must be 0 or 1.
	private int dark_on_bright = 1;
	
	private int ndims = 2;
	
	public AlgorithmMSER(ModelImage destImage, ModelImage srcImage, double delta, double epsilon, boolean duplicates,
			double max_area, double min_area, double max_variation, double min_diversity, int bright_on_dark, int dark_on_bright) {
	    super(destImage, srcImage);	
	    this.delta = delta;
	    this.epsilon = epsilon;
	    this.duplicates = duplicates;
	    this.max_area = max_area;
	    this.min_area = min_area;
	    this.max_variation = max_variation;
	    this.min_diversity = min_diversity;
	    this.bright_on_dark = bright_on_dark;
	    this.dark_on_bright = dark_on_bright;
	}
	
	
	/**
     * Start algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("MSER: Source Image is null");

            return;
        }
    }
}