package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;


/**
 This is a port of openCVtrilateralFilter.h and openCVtrilateralFilter.cpp 
 written by Tobi Vaudrey.  These files were released under the 3-clause BSD license
References: 1.) "Fast Trilateral Filtering" by Tobi Vaudrey and Reinhard Klette,
                 Conference Paper in Lecture Notes in Computer Science, September,
                 2009, DOI: 10.1007/978-3-642-03767-2_66
            2.) "The Trilateral Filter for High Contrast Images and Meshes" by
                 Prasun Choudhury and Jack Tumblin, Eurographics Symposium on
                 Rendering 2003, pp. 1-11, Per Christensen and Daniel Cohen-Or (Editors)
                 
By downloading, copying, installing or using the software you agree to this license.
If you do not agree to this license, do not download, install,
copy or use the software.


                          License Agreement
               For Open Source Computer Vision Library
                       (3-clause BSD License)

Copyright (C) 2000-2020, Intel Corporation, all rights reserved.
Copyright (C) 2009-2011, Willow Garage Inc., all rights reserved.
Copyright (C) 2009-2016, NVIDIA Corporation, all rights reserved.
Copyright (C) 2010-2013, Advanced Micro Devices, Inc., all rights reserved.
Copyright (C) 2015-2016, OpenCV Foundation, all rights reserved.
Copyright (C) 2015-2016, Itseez Inc., all rights reserved.
Copyright (C) 2019-2020, Xperience AI, all rights reserved.
Third party copyrights are property of their respective owners.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  * Neither the names of the copyright holders nor the names of the contributors
    may be used to endorse or promote products derived from this software
    without specific prior written permission.

This software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are disclaimed.
In no event shall copyright holders or contributors be liable for any direct,
indirect, incidental, special, exemplary, or consequential damages
(including, but not limited to, procurement of substitute goods or services;
loss of use, data, or profits; or business interruption) however caused
and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of
the use of this software, even if advised of the possibility of such damage.
© 2021 GitHub, Inc.
*/

    public class AlgorithmTrilateralFilter extends AlgorithmBase {
    	
    	private double sigmaC;
    	
    	private double epsilon;
    	
    	private final int TYPE_LUT = 0;
    	
    	private final int TYPE_FAST = 1;
    	
    	private int filterType = TYPE_FAST;
    	private int xDim;
    	private int yDim;
    	private int length;
    	
    	//~ Constructors ---------------------------------------------------------------------------------------------------
        
        public AlgorithmTrilateralFilter() {
        	
        }
        
        public AlgorithmTrilateralFilter(ModelImage destImg, ModelImage srcImg, double sigmaC,
        		double epsilon, int filterType) {
        	super(destImg, srcImg);
        	this.sigmaC = sigmaC;
        	this.epsilon = epsilon;
        	this.filterType = filterType;
        }
        
        private double getNorm(double g1, double g2)  {
            double norm = Math.sqrt(g1*g1 + g2*g2);
            return norm;
        }
        
        // Default is roundUp = false
        private int log2(int input, boolean roundUp) {
            double temp = Math.log10( (double)(input) ) / Math.log10(2.0);
            if (roundUp) {
                return (int)( Math.ceil(temp) );
            }
            else {
                return (int)( temp );
            }
       }


        // Cannot be color image
    	public void runAlgorithm() {
    	   ModelImage outputImage;
      	   if (destImage != null) {
      		   outputImage = destImage;
      	   }
      	   else {
      		   outputImage = srcImage;
      	   }
      	   
      	   xDim = srcImage.getExtents()[0];
      	   yDim = srcImage.getExtents()[1];
      	   length = xDim * yDim;
      	   
      	   double buffer[] = new double[length];
      	   
      	   try {
		    	srcImage.exportData(0, length, buffer);
		    }
		    catch (IOException e) {
		    	MipavUtil.displayError("IOException on srcImage.exportData(0, length, buffer)");
		    	setCompleted(false);
		    	return;
		    }
    	  } // public void runAlgorithm
    }