package gov.nih.mipav.model.algorithms;

import java.io.IOException;
import java.util.Collections;
import java.util.Vector;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;

public class PyramidToolbox extends AlgorithmBase {
	
	/**
	 * 
	 This code is a port of the MATLAB Pyramid Toolbox
	===========================  matlabPyrTools ============================

			This package contains some MatLab tools for multi-scale image
			processing.  Briefly, the tools include:
			  - Recursive multi-scale image decompositions (pyramids), including
			    Laplacian pyramids, QMFs, Wavelets, and steerable pyramids.  These
			    operate on 1D or 2D signals of arbitrary dimension.  Data
			    structures are compatible with the MatLab wavelet toolbox.
			  - Fast 2D convolution routines, with subsampling and boundary-handling.
			  - Fast point-operations, histograms, histogram-matching.
			  - Fast synthetic image generation: sine gratings, zone plates, fractals, etc.
			  - Display routines for images and pyramids.  These include several
			    auto-scaling options, rounding to integer zoom factors to avoid 
			    resampling artifacts, and useful labeling (dimensions and gray-range).

			The package is available as a gnu-zipped UNIX "tar" file, accessible
			from the web page:   http://www.cns.nyu.edu/~lcv/software.html
			
			
    The MIT License (MIT)

	Copyright (c) 2015 LabForComputationalVision
	
	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:
	
	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.
	
	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
	*/


	
	public PyramidToolbox() {
		
	}
	
	public void runAlgorithm() {
		
	}
	
        // [PYR, INDICES, STEERMTX, HARMONICS] = buildSpyr(IM, HEIGHT, FILTFILE, EDGES)
		
		// Construct a steerable pyramid on matrix IM.  Convolutions are
		// done with spatial filters.
		
		// HEIGHT (optional) specifies the number of pyramid levels to build. Default
		// is maxPyrHt(size(IM),size(FILT)). 
		// You can also specify 'auto' to use this value.
		
		// FILTFILE (optional) should be a string referring to an m-file that
		// returns the rfilters.  (examples: 'sp0Filters', 'sp1Filters',
		// 'sp3Filters','sp5Filters'.  default = 'sp1Filters'). EDGES specifies
		// edge-handling, and defaults to 'reflect1' (see corrDn).
		
		// PYR is a vector containing the N pyramid subbands, ordered from fine
		// to coarse.  INDICES is an Nx2 matrix containing the sizes of
		// each subband.  This is compatible with the MatLab Wavelet toolbox.
		// See the function STEER for a description of STEERMTX and HARMONICS.

		// Eero Simoncelli, 6/96.
		// See http://www.cis.upenn.edu/~eero/steerpyr.html for more
		// information about the Steerable Pyramid image decomposition.
	    // filtfile default sp1Filters
	    // edges default = reflect1

		public void buildSpyr(Vector<Integer>pyr, Vector<Integer>pind, Vector<Integer> harmonics, Vector<Double>steermtx, double im[][],
				int ht, String filtfile, String edges) {
			int i,r,c;
            double hi0filt[] = null;
            double lo0filt[] = null;
            double lofilt[] = null;
            double bfiltstranspose[][] = null;
            double bfilts[][] = null;
            // Steerable pyramid filters.  Transform described  in:
        	
        	// @INPROCEEDINGS{Simoncelli95b,
        	//	TITLE = "The Steerable Pyramid: A Flexible Architecture for
        	//		 Multi-Scale Derivative Computation",
        	//	AUTHOR = "E P Simoncelli and W T Freeman",
        	//	BOOKTITLE = "Second Int'l Conf on Image Processing",
        	//	ADDRESS = "Washington, DC", MONTH = "October", YEAR = 1995 }
        	
        	// Filter kernel design described in:
        	//
        	// @INPROCEEDINGS{Karasaridis96,
        	//	TITLE = "A Filter Design Technique for 
        	//		Steerable Pyramid Image Transforms",
        	//	AUTHOR = "A Karasaridis and E P Simoncelli",
        	//	BOOKTITLE = "ICASSP",	ADDRESS = "Atlanta, GA",
        	//	MONTH = "May",	YEAR = 1996 }

        	// Eero Simoncelli, 6/96.
            if (filtfile.equalsIgnoreCase("sp0Filters")) {
            	Collections.addAll(harmonics, new Integer[] { 0 });

            	lo0filt = new double[] {
            	-4.514000e-04, -1.137100e-04, -3.725800e-04, -3.743860e-03, -3.725800e-04, -1.137100e-04, -4.514000e-04,
            	-1.137100e-04, -6.119520e-03 ,-1.344160e-02, -7.563200e-03, -1.344160e-02, -6.119520e-03, -1.137100e-04,
            	-3.725800e-04, -1.344160e-02, 6.441488e-02, 1.524935e-01, 6.441488e-02, -1.344160e-02, -3.725800e-04, 
            	-3.743860e-03, -7.563200e-03, 1.524935e-01, 3.153017e-01, 1.524935e-01, -7.563200e-03, -3.743860e-03, 
            	-3.725800e-04, -1.344160e-02, 6.441488e-02, 1.524935e-01, 6.441488e-02, -1.344160e-02, -3.725800e-04, 
            	-1.137100e-04, -6.119520e-03, -1.344160e-02, -7.563200e-03, -1.344160e-02, -6.119520e-03, -1.137100e-04, 
            	-4.514000e-04, -1.137100e-04, -3.725800e-04, -3.743860e-03, -3.725800e-04, -1.137100e-04, -4.514000e-04};

            	lofilt = new double[]{
            	-2.257000e-04, -8.064400e-04, -5.686000e-05, 8.741400e-04, -1.862800e-04, -1.031640e-03, -1.871920e-03, 
            	-1.031640e-03, -1.862800e-04, 8.741400e-04, -5.686000e-05, -8.064400e-04, -2.257000e-04, 
            	-8.064400e-04, 1.417620e-03, -1.903800e-04, -2.449060e-03, -4.596420e-03, -7.006740e-03, -6.948900e-03,
            	-7.006740e-03, -4.596420e-03, -2.449060e-03, -1.903800e-04, 1.417620e-03, -8.064400e-04, 
            	-5.686000e-05, -1.903800e-04, -3.059760e-03, -6.401000e-03, -6.720800e-03, -5.236180e-03, -3.781600e-03,
            	-5.236180e-03, -6.720800e-03, -6.401000e-03, -3.059760e-03, -1.903800e-04, -5.686000e-05, 
            	8.741400e-04, -2.449060e-03, -6.401000e-03, -5.260020e-03, 3.938620e-03, 1.722078e-02, 2.449600e-02,
            	1.722078e-02, 3.938620e-03, -5.260020e-03, -6.401000e-03, -2.449060e-03, 8.741400e-04, 
            	-1.862800e-04, -4.596420e-03, -6.720800e-03, 3.938620e-03, 3.220744e-02, 6.306262e-02, 7.624674e-02,
            	6.306262e-02, 3.220744e-02, 3.938620e-03, -6.720800e-03, -4.596420e-03, -1.862800e-04, 
            	-1.031640e-03, -7.006740e-03, -5.236180e-03, 1.722078e-02, 6.306262e-02, 1.116388e-01, 1.348999e-01,
            	1.116388e-01, 6.306262e-02, 1.722078e-02, -5.236180e-03, -7.006740e-03, -1.031640e-03, 
            	-1.871920e-03, -6.948900e-03, -3.781600e-03, 2.449600e-02, 7.624674e-02, 1.348999e-01, 1.576508e-01,
            	1.348999e-01, 7.624674e-02, 2.449600e-02, -3.781600e-03, -6.948900e-03, -1.871920e-03, 
            	-1.031640e-03, -7.006740e-03, -5.236180e-03, 1.722078e-02, 6.306262e-02, 1.116388e-01, 1.348999e-01,
            	1.116388e-01, 6.306262e-02, 1.722078e-02, -5.236180e-03, -7.006740e-03, -1.031640e-03, 
            	-1.862800e-04, -4.596420e-03, -6.720800e-03, 3.938620e-03, 3.220744e-02, 6.306262e-02, 7.624674e-02,
            	6.306262e-02, 3.220744e-02, 3.938620e-03, -6.720800e-03, -4.596420e-03, -1.862800e-04,
            	8.741400e-04, -2.449060e-03, -6.401000e-03, -5.260020e-03, 3.938620e-03, 1.722078e-02, 2.449600e-02,
            	1.722078e-02, 3.938620e-03, -5.260020e-03, -6.401000e-03, -2.449060e-03, 8.741400e-04, 
            	-5.686000e-05, -1.903800e-04, -3.059760e-03, -6.401000e-03, -6.720800e-03, -5.236180e-03, -3.781600e-03,
            	-5.236180e-03, -6.720800e-03, -6.401000e-03, -3.059760e-03, -1.903800e-04, -5.686000e-05, 
            	-8.064400e-04, 1.417620e-03, -1.903800e-04, -2.449060e-03, -4.596420e-03, -7.006740e-03, -6.948900e-03,
            	-7.006740e-03, -4.596420e-03, -2.449060e-03, -1.903800e-04, 1.417620e-03, -8.064400e-04, 
            	-2.257000e-04, -8.064400e-04, -5.686000e-05, 8.741400e-04, -1.862800e-04, -1.031640e-03, -1.871920e-03,
            	-1.031640e-03, -1.862800e-04, 8.741400e-04, -5.686000e-05, -8.064400e-04, -2.257000e-04}; 

            	Collections.addAll(steermtx, new Double[] { 1.000000 });

            	hi0filt = new double[] {
            	5.997200e-04, -6.068000e-05, -3.324900e-04, -3.325600e-04, -2.406600e-04, -3.325600e-04, -3.324900e-04, -6.068000e-05, 5.997200e-04,
            	-6.068000e-05, 1.263100e-04, 4.927100e-04, 1.459700e-04, -3.732100e-04, 1.459700e-04, 4.927100e-04, 1.263100e-04, -6.068000e-05, 
            	-3.324900e-04, 4.927100e-04, -1.616650e-03, -1.437358e-02, -2.420138e-02, -1.437358e-02, -1.616650e-03, 4.927100e-04, -3.324900e-04, 
            	-3.325600e-04, 1.459700e-04, -1.437358e-02, -6.300923e-02, -9.623594e-02, -6.300923e-02, -1.437358e-02, 1.459700e-04, -3.325600e-04, 
            	-2.406600e-04, -3.732100e-04, -2.420138e-02, -9.623594e-02, 8.554893e-01, -9.623594e-02, -2.420138e-02, -3.732100e-04, -2.406600e-04, 
            	-3.325600e-04, 1.459700e-04, -1.437358e-02, -6.300923e-02, -9.623594e-02, -6.300923e-02, -1.437358e-02, 1.459700e-04, -3.325600e-04, 
            	-3.324900e-04, 4.927100e-04, -1.616650e-03, -1.437358e-02, -2.420138e-02, -1.437358e-02, -1.616650e-03, 4.927100e-04, -3.324900e-04, 
            	-6.068000e-05, 1.263100e-04, 4.927100e-04, 1.459700e-04, -3.732100e-04, 1.459700e-04, 4.927100e-04, 1.263100e-04, -6.068000e-05, 
            	5.997200e-04, -6.068000e-05, -3.324900e-04, -3.325600e-04, -2.406600e-04, -3.325600e-04, -3.324900e-04, -6.068000e-05, 5.997200e-04 };

            	bfiltstranspose = new double[][] {
            	{-9.066000e-05, -1.738640e-03, -4.942500e-03, -7.889390e-03, -1.009473e-02, -7.889390e-03, -4.942500e-03, -1.738640e-03, -9.066000e-05,
            	-1.738640e-03, -4.625150e-03, -7.272540e-03, -7.623410e-03, -9.091950e-03, -7.623410e-03, -7.272540e-03, -4.625150e-03, -1.738640e-03,
            	-4.942500e-03, -7.272540e-03, -2.129540e-02, -2.435662e-02, -3.487008e-02, -2.435662e-02, -2.129540e-02, -7.272540e-03, -4.942500e-03,
            	-7.889390e-03, -7.623410e-03, -2.435662e-02, -1.730466e-02, -3.158605e-02, -1.730466e-02, -2.435662e-02, -7.623410e-03, -7.889390e-03,
            	-1.009473e-02, -9.091950e-03, -3.487008e-02, -3.158605e-02, 9.464195e-01, -3.158605e-02, -3.487008e-02, -9.091950e-03, -1.009473e-02,
            	-7.889390e-03, -7.623410e-03, -2.435662e-02, -1.730466e-02, -3.158605e-02, -1.730466e-02, -2.435662e-02, -7.623410e-03, -7.889390e-03,
            	-4.942500e-03, -7.272540e-03, -2.129540e-02, -2.435662e-02, -3.487008e-02, -2.435662e-02, -2.129540e-02, -7.272540e-03, -4.942500e-03,
            	-1.738640e-03, -4.625150e-03, -7.272540e-03, -7.623410e-03, -9.091950e-03, -7.623410e-03, -7.272540e-03, -4.625150e-03, -1.738640e-03,
            	-9.066000e-05, -1.738640e-03, -4.942500e-03, -7.889390e-03, -1.009473e-02, -7.889390e-03, -4.942500e-03, -1.738640e-03, -9.066000e-05}};
            	bfilts = new double[bfiltstranspose[0].length][bfiltstranspose.length];
				for (r = 0; r < bfilts.length; r++) {
					for (c = 0; c < bfilts[0].length; c++) {
						bfilts[r][c] = bfiltstranspose[c][r];
					}
				}
	
            }
            else if (filtfile.equalsIgnoreCase("sp1Filters")) {
            	
            }
            else if (filtfile.equalsIgnoreCase("sp3Filters")) {
            	
            }
            else if (filtfile.equalsIgnoreCase("sp5Filters")) {
				Collections.addAll(harmonics,new Integer[] {1, 3, 5});
	
				Collections.addAll(steermtx, new Double[] {
				    0.3333,    0.2887,    0.1667,    0.0000,   -0.1667,   -0.2887,
				    0.0000,    0.1667,    0.2887,    0.3333,    0.2887,    0.1667,
				    0.3333,   -0.0000,   -0.3333,   -0.0000,    0.3333,   -0.0000,
				    0.0000,    0.3333,    0.0000,   -0.3333,    0.0000,    0.3333,
				    0.3333,   -0.2887,    0.1667,   -0.0000,   -0.1667,    0.2887,
				   -0.0000,    0.1667,   -0.2887,    0.3333,   -0.2887,    0.1667});
	
				hi0filt = new double[] { 
				-0.00033429, -0.00113093, -0.00171484, -0.00133542, -0.00080639, -0.00133542, -0.00171484, -0.00113093, -0.00033429,
				-0.00113093, -0.00350017, -0.00243812, 0.00631653, 0.01261227, 0.00631653, -0.00243812, -0.00350017, -0.00113093,
				-0.00171484, -0.00243812, -0.00290081, -0.00673482, -0.00981051, -0.00673482, -0.00290081, -0.00243812, -0.00171484,
				-0.00133542, 0.00631653, -0.00673482, -0.07027679, -0.11435863, -0.07027679, -0.00673482, 0.00631653, -0.00133542,
				-0.00080639, 0.01261227, -0.00981051, -0.11435863, 0.81380200, -0.11435863, -0.00981051, 0.01261227, -0.00080639,
				-0.00133542, 0.00631653, -0.00673482, -0.07027679, -0.11435863, -0.07027679, -0.00673482, 0.00631653, -0.00133542,
				-0.00171484, -0.00243812, -0.00290081, -0.00673482, -0.00981051, -0.00673482, -0.00290081, -0.00243812, -0.00171484,
				-0.00113093, -0.00350017, -0.00243812, 0.00631653, 0.01261227, 0.00631653, -0.00243812, -0.00350017, -0.00113093,
				-0.00033429, -0.00113093, -0.00171484, -0.00133542, -0.00080639, -0.00133542, -0.00171484, -0.00113093, -0.00033429};
	
	
				lo0filt = new double[] {
				0.00341614, -0.01551246, -0.03848215, -0.01551246, 0.00341614,
				-0.01551246, 0.05586982, 0.15925570, 0.05586982, -0.01551246,
				-0.03848215, 0.15925570, 0.40304148, 0.15925570, -0.03848215,
				-0.01551246, 0.05586982, 0.15925570, 0.05586982, -0.01551246,
				0.00341614, -0.01551246, -0.03848215, -0.01551246, 0.00341614};
	
				lofilt = new double[] {
				0.00085404, -0.00244917, -0.00387812, -0.00944432, -0.00962054, -0.00944432, -0.00387812, -0.00244917, 0.00085404,
				-0.00244917, -0.00523281, -0.00661117, 0.00410600, 0.01002988, 0.00410600, -0.00661117, -0.00523281, -0.00244917,
				-0.00387812, -0.00661117, 0.01396746, 0.03277038, 0.03981393, 0.03277038, 0.01396746, -0.00661117, -0.00387812,
				-0.00944432, 0.00410600, 0.03277038, 0.06426333, 0.08169618, 0.06426333, 0.03277038, 0.00410600, -0.00944432,
				-0.00962054, 0.01002988, 0.03981393, 0.08169618, 0.10096540, 0.08169618, 0.03981393, 0.01002988, -0.00962054,
				-0.00944432, 0.00410600, 0.03277038, 0.06426333, 0.08169618, 0.06426333, 0.03277038, 0.00410600, -0.00944432,
				-0.00387812, -0.00661117, 0.01396746, 0.03277038, 0.03981393, 0.03277038, 0.01396746, -0.00661117, -0.00387812,
				-0.00244917, -0.00523281, -0.00661117, 0.00410600, 0.01002988, 0.00410600, -0.00661117, -0.00523281, -0.00244917,
				0.00085404, -0.00244917, -0.00387812, -0.00944432, -0.00962054, -0.00944432, -0.00387812, -0.00244917, 0.00085404};
				for (i = 0; i < lofilt.length; i++) {
					lofilt[i] = 2.0*lofilt[i];
				}
	
				bfiltstranspose = new double[][] {
				 {0.00277643, 0.00496194, 0.01026699, 0.01455399, 0.01026699, 0.00496194, 0.00277643,
				-0.00986904, -0.00893064, 0.01189859, 0.02755155, 0.01189859, -0.00893064, -0.00986904,
				-0.01021852, -0.03075356, -0.08226445, -0.11732297, -0.08226445, -0.03075356, -0.01021852,
				 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000,
				 0.01021852, 0.03075356, 0.08226445, 0.11732297, 0.08226445, 0.03075356, 0.01021852,
				 0.00986904, 0.00893064, -0.01189859, -0.02755155, -0.01189859, 0.00893064, 0.00986904,
				-0.00277643, -0.00496194, -0.01026699, -0.01455399, -0.01026699, -0.00496194, -0.00277643},
				
				{-0.00343249, -0.00640815, -0.00073141, 0.01124321, 0.00182078, 0.00285723, 0.01166982,
				-0.00358461, -0.01977507, -0.04084211, -0.00228219, 0.03930573, 0.01161195, 0.00128000,
				 0.01047717, 0.01486305, -0.04819057, -0.12227230, -0.05394139, 0.00853965, -0.00459034,
				 0.00790407, 0.04435647, 0.09454202, -0.00000000, -0.09454202, -0.04435647, -0.00790407,
				 0.00459034, -0.00853965, 0.05394139, 0.12227230, 0.04819057, -0.01486305, -0.01047717,
				-0.00128000, -0.01161195, -0.03930573, 0.00228219, 0.04084211, 0.01977507, 0.00358461,
				-0.01166982, -0.00285723, -0.00182078, -0.01124321, 0.00073141, 0.00640815, 0.00343249},
			
				 {0.00343249, 0.00358461, -0.01047717, -0.00790407, -0.00459034, 0.00128000, 0.01166982,
				 0.00640815, 0.01977507, -0.01486305, -0.04435647, 0.00853965, 0.01161195, 0.00285723,
				 0.00073141, 0.04084211, 0.04819057, -0.09454202, -0.05394139, 0.03930573, 0.00182078,
				-0.01124321, 0.00228219, 0.12227230, -0.00000000, -0.12227230, -0.00228219, 0.01124321,
				-0.00182078, -0.03930573, 0.05394139, 0.09454202, -0.04819057, -0.04084211, -0.00073141,
				-0.00285723, -0.01161195, -0.00853965, 0.04435647, 0.01486305, -0.01977507, -0.00640815,
				-0.01166982, -0.00128000, 0.00459034, 0.00790407, 0.01047717, -0.00358461, -0.00343249},
				
				{-0.00277643, 0.00986904, 0.01021852, -0.00000000, -0.01021852, -0.00986904, 0.00277643,
				-0.00496194, 0.00893064, 0.03075356, -0.00000000, -0.03075356, -0.00893064, 0.00496194,
				-0.01026699, -0.01189859, 0.08226445, -0.00000000, -0.08226445, 0.01189859, 0.01026699,
				-0.01455399, -0.02755155, 0.11732297, -0.00000000, -0.11732297, 0.02755155, 0.01455399,
				-0.01026699, -0.01189859, 0.08226445, -0.00000000, -0.08226445, 0.01189859, 0.01026699,
				-0.00496194, 0.00893064, 0.03075356, -0.00000000, -0.03075356, -0.00893064, 0.00496194,
				-0.00277643, 0.00986904, 0.01021852, -0.00000000, -0.01021852, -0.00986904, 0.00277643},
			
				{-0.01166982, -0.00128000, 0.00459034, 0.00790407, 0.01047717, -0.00358461, -0.00343249,
				-0.00285723, -0.01161195, -0.00853965, 0.04435647, 0.01486305, -0.01977507, -0.00640815,
				-0.00182078, -0.03930573, 0.05394139, 0.09454202, -0.04819057, -0.04084211, -0.00073141,
				-0.01124321, 0.00228219, 0.12227230, -0.00000000, -0.12227230, -0.00228219, 0.01124321,
				 0.00073141, 0.04084211, 0.04819057, -0.09454202, -0.05394139, 0.03930573, 0.00182078,
				 0.00640815, 0.01977507, -0.01486305, -0.04435647, 0.00853965, 0.01161195, 0.00285723,
				 0.00343249, 0.00358461, -0.01047717, -0.00790407, -0.00459034, 0.00128000, 0.01166982},
			
				{-0.01166982, -0.00285723, -0.00182078, -0.01124321, 0.00073141, 0.00640815, 0.00343249,
				-0.00128000, -0.01161195, -0.03930573, 0.00228219, 0.04084211, 0.01977507, 0.00358461,
				 0.00459034, -0.00853965, 0.05394139, 0.12227230, 0.04819057, -0.01486305, -0.01047717,
				 0.00790407, 0.04435647, 0.09454202, -0.00000000, -0.09454202, -0.04435647, -0.00790407,
				 0.01047717, 0.01486305, -0.04819057, -0.12227230, -0.05394139, 0.00853965, -0.00459034,
				-0.00358461, -0.01977507, -0.04084211, -0.00228219, 0.03930573, 0.01161195, 0.00128000,
				-0.00343249, -0.00640815, -0.00073141, 0.01124321, 0.00182078, 0.00285723, 0.01166982}};
				bfilts = new double[bfiltstranspose[0].length][bfiltstranspose.length];
				for (r = 0; r < bfilts.length; r++) {
					for (c = 0; c < bfilts[0].length; c++) {
						bfilts[r][c] = bfiltstranspose[c][r];
					}
				}
		
			} // if (filtfile.equalsIgnoreCase("sp5Filters"))
			else {
			  System.err.println("Use buildSFpyr for pyramids with arbitrary numbers of orientation bands");
			  System.err.println("filtfile argument must be the name of an M-file containing SPYR filters.");
			}
	
			/*max_ht = maxPyrHt(size(im), size(lofilt,1));
			if ( (exist('ht') ~= 1) | (ht == 'auto') )
			  ht = max_ht;
			else
			  if (ht > max_ht)
			    error(sprintf('Cannot build pyramid higher than %d levels.',max_ht));
			  end
			end
	
			%-----------------------------------------------------------------
	
			hi0 = corrDn(im, hi0filt, edges);
			lo0 = corrDn(im, lo0filt, edges);
	
			[pyr,pind] = buildSpyrLevs(lo0, ht, lofilt, bfilts, edges);
	
			pyr = [hi0(:) ; pyr];
			pind = [size(hi0); pind];*/
		}
			  

	
}