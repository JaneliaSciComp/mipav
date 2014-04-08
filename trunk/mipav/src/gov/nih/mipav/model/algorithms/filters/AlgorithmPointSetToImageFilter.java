	package gov.nih.mipav.model.algorithms.filters;
	
	
	import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.BSplineKernelFunction;
import gov.nih.mipav.model.structures.CoxDeBoorBSplineKernelFunction;
import gov.nih.mipav.model.structures.ModelImage;
	






	import java.io.IOException;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3d;
import gov.nih.mipav.view.ViewJProgressBar;
	
	
	/**
	 
	 *
	 * @version  0.1 April 8, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkPointSetToImageFilter.hxx from the itk package.  Here is the original itk header
	  * from the itkPointSetToImageFilter.hxx file:
	  *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *

  
	 */
	public class AlgorithmPointSetToImageFilter extends AlgorithmBase {
		
		protected int nDims;
		protected int numberOfRequiredInputs;
		protected int extents[];
		protected float origin[];
		protected float resolutions[];
		protected double direction[][];
		protected double insideValue;
		protected double outsideValue;
		
		/**
	     * Constructor which sets the source and destination images
	     *
	     * @param  destImg   the destination image
	     * @param  srcImg    the source image
	     * @param  confidenceImage
	     * @param  maskFlag  the mask flag
	     */
	    public AlgorithmPointSetToImageFilter(ModelImage destImg, ModelImage srcImg) {
	        super(destImg, srcImg);
	    }
	    
	    public AlgorithmPointSetToImageFilter(int nDims) {
	    	int i;
	        this.nDims = nDims;
	        numberOfRequiredInputs = 1;
	        extents = new int[nDims];
	        origin = new float[nDims];
	        resolutions = new float[nDims];
	        for (i = 0; i < nDims; i++) {
	        	resolutions[i] = 1.0f;
	        }
	        direction = new double[nDims][nDims];
	        for (i = 0; i < nDims; i++) {
	        	direction[i][i] = 1.0;
	        }
	        insideValue = 1.0;
	        outsideValue = 0.0; 
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------

	    /**
	     * Prepares this class for destruction.
	     */
	    public void finalize() {
	        destImage = null;
	        srcImage = null;
	        super.finalize();
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
	    
	    public void setExtents(int[] extents) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.extents[i] = extents[i];
	    	}
	    }
	    
	    public void setDirection(double[][] direction) {
	        for (int i = 0; i < nDims; i++) {
	        	for (int j = 0; j < nDims; j++) {
	        		this.direction[i][j] = direction[i][j];
	        	}
	        }
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

