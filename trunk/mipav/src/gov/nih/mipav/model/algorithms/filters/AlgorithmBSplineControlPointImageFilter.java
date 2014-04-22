	package gov.nih.mipav.model.algorithms.filters;
	
	
	import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.BSplineKernelFunction;
import gov.nih.mipav.model.structures.CoxDeBoorBSplineKernelFunction;
import gov.nih.mipav.model.structures.ModelImage;
	






import gov.nih.mipav.model.structures.ModelStorageBase;

	import java.io.IOException;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3d;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;
	
	
	/**
	 
	 *
	 * @version  0.1 March 30, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkBSplineControlPointImageFilter.txx from the itk package.  Here is the original itk header
	  * from the itkBSplineControlPointImageFIlter.h file:
	  * Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBSplineControlPointImageFilter.h,v $
  Language:  C++
  Date:      $Date: 2009/05/04 14:10:34 $
  Version:   $Revision: 1.5 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

	 */
	public class AlgorithmBSplineControlPointImageFilter extends AlgorithmBase {
		private int nDims;
		private int splineOrder[];
		private boolean doMultiLevel;
		private int maximumNumberOfLevels;
		private float origin[];
		private float resolutions[];
		private int extents[];
		private double direction[][];
		private int numberOfControlPoints[];
		private CoxDeBoorBSplineKernelFunction kernel[];
		private BSplineKernelFunction kernelOrder0;
		private BSplineKernelFunction kernelOrder1;
		private BSplineKernelFunction kernelOrder2;
		private BSplineKernelFunction kernelOrder3;
		private int numberOfLevels[];
		private int closeDimension[];
		// Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        //epsilon = 1.0;
        //neweps = 1.0;

        //while (true) {

        //     if (1.0 == (1.0 + neweps)) {
        //         break;
        //     } else {
        //         epsilon = neweps;
        //        neweps = neweps / 2.0;
        //     }
        // } // while(true)
		private double epsilon = 2.2204460e-16;
		private double BSplineEpsilon;
		private ModelImage inputImage = null;
		private int extentsLength;
		private int extentsSlice;
		private ModelImage outputImage = null;
	
		/**
	     * Constructor which sets the source and destination images
	     *
	     * @param  destImg   the destination image
	     * @param  srcImg    the source image
	     * @param  confidenceImage
	     * @param  maskFlag  the mask flag
	     */
	    public AlgorithmBSplineControlPointImageFilter(ModelImage destImg, ModelImage srcImg) {
	        super(destImg, srcImg);
	    }
	    
	    public AlgorithmBSplineControlPointImageFilter(int nDims) {
	    	this.nDims = nDims;
	    	splineOrder = new int[nDims];
	        int i;
	        for (i = 0; i < nDims; i++) {
	        	splineOrder[i] = 3;
	        }
	        doMultiLevel = false;
	        maximumNumberOfLevels = 1;
	        origin = new float[nDims];
	        resolutions = new float[nDims];
	        for (i = 0; i < nDims; i++) {
	        	resolutions[i] = 1.0f;
	        }
	        extents = new int[nDims];
	        direction = new double[nDims][nDims];
	        for (i = 0; i < nDims; i++) {
	        	direction[i][i] = 1.0;
	        }
	        numberOfControlPoints = new int[nDims];
	        kernel = new CoxDeBoorBSplineKernelFunction[nDims];
	        for (i = 0; i < nDims; i++) {
	        	numberOfControlPoints[i] = splineOrder[i] + 1;
	        	kernel[i] = new CoxDeBoorBSplineKernelFunction(splineOrder[i]);	
	        }
	        kernelOrder0 = new BSplineKernelFunction(0);
	        kernelOrder1 = new BSplineKernelFunction(1);
	        kernelOrder2 = new BSplineKernelFunction(2);
	        kernelOrder3 = new BSplineKernelFunction(3);
	        numberOfLevels = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	numberOfLevels[i] = 1;
	        }
	        closeDimension = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	closeDimension[i] = 0;
	        }
	        BSplineEpsilon = epsilon;
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------
	    
	    public void setInput(ModelImage inputImage) {
	    	this.inputImage = inputImage;
	    }
	    
	    public void setOrigin(float[] origin) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.origin[i] = origin[i];
	    	}
	    }
	    
	    public void setResolutions(float[] resolutions) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.resolutions[i] = resolutions[i];
	    	}
	    }
	    
	    public void setDirection(double[][] direction) {
	        for (int i = 0; i < nDims; i++) {
	        	for (int j = 0; j < nDims; j++) {
	        		this.direction[i][j] = direction[i][j];
	        	}
	        }
	    }
	    
	    public void setExtents(int[] extents) {
	    	extentsLength = 1;
	    	for (int i = 0; i < nDims; i++) {
	    		this.extents[i] = extents[i];
	    		extentsLength *= extents[i];
	    	}
	    	extentsSlice = extents[0] * extents[1];
	    }
	    
	    public void generateData() {
	    	beforeThreadedGenerateData();
	    	threadedGenerateData();
	    }
	    
	    private void beforeThreadedGenerateData() {
	        for (int i = 0; i < nDims; i++) {
	        	if (extents[i] == 0) {
	        		MipavUtil.displayError("Extents must be specified");
	        		return;
	        	}
	        }
	        
	        outputImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "outputImage");
	        int fileInfoLength = outputImage.getFileInfo().length;
	    	for (int i = 0; i < fileInfoLength; i++) {
	    		outputImage.getFileInfo(i).setOrigin(origin);
	    		outputImage.getFileInfo(i).setResolutions(resolutions);
	    	}
	    	
	    	for (int i = 0; i < nDims; i++) {
                for (int j = 0; j < nDims; j++) {
                    outputImage.getMatrix().set(i, j, (float)direction[i][j]);
                }
            }
	    	
	    	int maximumNumberOfSpans = 0;
	    	for (int d = 0; d < nDims; d++) {
	    		int numberOfSpans = numberOfControlPoints[d] - splineOrder[d];
	    		numberOfSpans <<= (numberOfLevels[d] - 1);
	    		if (numberOfSpans > maximumNumberOfSpans) {
	    			maximumNumberOfSpans = numberOfSpans;
	    		}
	    	} // for (int d = 0; d < nDims; d++)
	    	BSplineEpsilon = 100.0 * epsilon;
	    	while ((double)maximumNumberOfSpans == (double)maximumNumberOfSpans - BSplineEpsilon) {
	    		BSplineEpsilon *= 10.0;
	    	}
	    	
	    	for (int i = 0; i < nDims; i++) {
	    		numberOfControlPoints[i] = inputImage.getExtents()[i];
	    	}
	    }
	    
	    private void threadedGenerateData() {
	    	
	    }

	    /**
	     * Prepares this class for destruction.
	     */
	    public void finalize() {
	        destImage = null;
	        srcImage = null;
	        super.finalize();
	    }

	    /**
	     * Starts the program.
	     */
	    public void runAlgorithm() {
	        

	        if (srcImage == null) {
	            displayError("Source Image is null");
	            finalize();

	            return;
	        }
	    }
}

