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
import gov.nih.mipav.view.MipavUtil;
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
	public class AlgorithmBSplineScatteredDataPointSetToImageFilter extends AlgorithmPointSetToImageFilter {
		
		private int splineOrder[];
		private int numberOfControlPoints[];
		private CoxDeBoorBSplineKernelFunction kernel[];
		private BSplineKernelFunction kernelOrder0;
		private BSplineKernelFunction kernelOrder1;
		private BSplineKernelFunction kernelOrder2;
		private BSplineKernelFunction kernelOrder3;
		private int closeDimension[];
		private boolean doMultiLevel;
		private boolean generateOutputImage;
		private int numberOfLevels[];
		private int maximumNumberOfLevels;
		private boolean constructPhiLattice;
		ModelImage phiLattice;
		private boolean constructPsiLattice;
		ModelImage psiLattice;
		private double[] inputPointdData[];
		private double[] outputPointData[];
		private double[] pointWeights[];
		private boolean usePointWeights;
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
		private double splineEpsilon;
		private boolean isFittingComplete;
	
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
	        super(nDims);
	        splineOrder = new int[nDims];
	        int i;
	        for (i = 0; i < nDims; i++) {
	        	splineOrder[i] = 3;
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
	        closeDimension = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	closeDimension[i] = 0;
	        }
	        doMultiLevel = false;
	        generateOutputImage = true;
	        numberOfLevels = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	numberOfLevels[i] = 1;
	        }
	        maximumNumberOfLevels = 1;
	        // itk only needs data type and extents to construct an image, but MIPAV needs extents[] as well.
	        constructPhiLattice = false;
	        phiLattice = null;
	        constructPsiLattice = true;
	        psiLattice = null;
	        // Create inputPointData[] and outputPointData[]
	        usePointWeights = false;
	        splineEpsilon = epsilon;
	        isFittingComplete = false;
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
	    
	    public void setGenerateOutputImage(boolean generateOutputImage) {
	    	this.generateOutputImage = generateOutputImage;
	    }
	    
	    public void setNumberOfLevels(int nLevels) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.numberOfLevels[i] = nLevels;
	    	}
	    	setNumberOfLevels(numberOfLevels);
	    }
	    
	    public void setNumberOfLevels(int[] numberOfLevels) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.numberOfLevels[i] = numberOfLevels[i];
	    		if (numberOfLevels[i] > maximumNumberOfLevels) {
	    			maximumNumberOfLevels = numberOfLevels[i];
	    		}
	    	}
	    	if (maximumNumberOfLevels > 1) {
    			doMultiLevel = true;
    		}
    		else {
    			doMultiLevel = false;
    		}
	    	setSplineOrder(splineOrder);
	    }
	    
	    public void setSplineOrder(int sOrder) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.splineOrder[i] = sOrder;
	    	}
	    	setSplineOrder(splineOrder);
	    	for (int i = 0; i < nDims; i++ ) {
	            if (splineOrder[i] == 0 ) {
	                MipavUtil.displayError("The spline order in each dimension must be greater than 0");
	                return;
	            }
	            kernel[i] = new CoxDeBoorBSplineKernelFunction(splineOrder[i]);
	            if (doMultiLevel) {
	                double C[][] = kernel[i].getShapeFunctionsInZeroToOneInterval();	
	            } // if (doMultiLevel)
	    	} // for (int i = 0; i < nDims; i++)

	       
	        /*if( this->m_DoMultilevel )
	          {
	          typename KernelType::MatrixType C;
	          C = this->m_Kernel[i]->GetShapeFunctionsInZeroToOneInterval();

	          vnl_matrix<RealType> R;
	          vnl_matrix<RealType> S;
	          R.set_size( C.rows(), C.cols() );
	          S.set_size( C.rows(), C.cols() );
	          for( unsigned int j = 0; j < C.rows(); j++ )
	            {
	            for( unsigned int k = 0; k < C.cols(); k++ )
	              {
	              R(j, k) = S(j, k) = static_cast<RealType>( C(j, k) );
	              }
	            }
	          for( unsigned int j = 0; j < C.cols(); j++ )
	            {
	            RealType c = vcl_pow( static_cast<RealType>( 2.0 ),
	              static_cast<RealType>( C.cols() ) - j - 1 );

	            for( unsigned int k = 0; k < C.rows(); k++ )
	              {
	              R(k, j) *= c;
	              }
	            }
	          R = R.transpose();
	          R.flipud();
	          S = S.transpose();
	          S.flipud();

	          this->m_RefinedLatticeCoefficients[i] =
	            ( vnl_svd<RealType>( R ).solve( S ) ).extract( 2, S.cols() );
	          }
	        }*/

	    }
	    
	    public void setSplineOrder(int[] splineOrder) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.splineOrder[i] = splineOrder[i];
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

