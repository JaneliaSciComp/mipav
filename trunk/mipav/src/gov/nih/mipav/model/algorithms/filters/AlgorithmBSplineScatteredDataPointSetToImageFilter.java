	package gov.nih.mipav.model.algorithms.filters;
	
	
	import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.ModelImage;
	




	import java.io.IOException;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3d;
import gov.nih.mipav.view.ViewJProgressBar;
	
	
	/**
	 
	 *
	 * @version  0.1 April 1, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkBSplineScatteredDataPointSetToImageFilter.hxx from the itk package.  Here is the original itk header
	  * from the itkBSplineScatteredDataPointSetToImageFilter.hxx file:
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
	public class AlgorithmBSplineScatteredDataPointSetToImageFilter extends AlgorithmBase {
		
		private int nDims;
		private int splineOrder[];
		private int numberOfControlPoints[];
	
		/**
	     * Constructor which sets the source and destination images
	     *
	     * @param  destImg   the destination image
	     * @param  srcImg    the source image
	     * @param  confidenceImage
	     * @param  maskFlag  the mask flag
	     */
	    public AlgorithmBSplineScatteredDataPointSetToImageFilter(ModelImage destImg, ModelImage srcImg) {
	        super(destImg, srcImg);
	    }
	    
	    public AlgorithmBSplineScatteredDataPointSetToImageFilter(int nDims) {
	        this.nDims = nDims;
	        splineOrder = new int[nDims];
	        int i;
	        for (i = 0; i < nDims; i++) {
	        	splineOrder[i] = 3;
	        }
	        numberOfControlPoints = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	numberOfControlPoints[i] = splineOrder[i] + 1;
	        }
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

