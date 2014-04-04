	package gov.nih.mipav.model.structures;
	
	
	
	
	/**
	 
	 *
	 * @version  0.1 April 4, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkBSplineKernelFunction.h from the itk package.  Here is the original itk header
	  * from the itkBSplineKernelFunction.h file:
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
	public class BSplineKernelFunction  {
		
		private int splineOrder;
	
		
	    
	    public BSplineKernelFunction(int splineOrder) {
	        this.splineOrder = splineOrder;
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------

	    /**
	     * Prepares this class for destruction.
	     */
	    public void finalize() {
	        
	    }
	    
}

