	package gov.nih.mipav.model.algorithms.filters;
	
	
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector4d;
	
	
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
		protected Vector<Double> pointData;
		protected Vector<Vector4d> pointLocation;
		protected ModelImage outputImage;
		protected int extentsLength;
		protected int extentsSlice;
		protected int xyzExtents;
	    
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
	        direction = new double[Math.min(nDims,3)][Math.min(nDims,3)];
	        for (i = 0; i < Math.min(nDims,3); i++) {
	        	direction[i][i] = 1.0;
	        }
	        insideValue = 1.0;
	        outsideValue = 0.0;
	        pointData = new Vector<Double>();
	        pointLocation = new Vector<Vector4d>();
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------

	    /**
	     * Prepares this class for destruction.
	     */
	    public void finalize() {
	    	int i;
	        extents = null;
	        origin = null;
	        resolutions = null;
	        if (direction != null) {
	        	for (i = 0; i < direction.length; i++) {
	        		direction[i] = null;
	        	}
	        	direction = null;
	        }
	        if (pointData != null) {
	        	pointData.clear();
	        	pointData = null;
	        }
	        if (pointLocation != null) {
	        	pointLocation.clear();
	        	pointLocation = null;
	        }
	        outputImage = null;
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
	    	extentsLength = 1;
	    	for (int i = 0; i < nDims; i++) {
	    		this.extents[i] = extents[i];
	    		extentsLength *= extents[i];
	    	}
	    	extentsSlice = extents[0] * extents[1];
	    	xyzExtents = extentsSlice;
	    	if (nDims > 2) {
	    		xyzExtents *= extents[2];
	    	}
	    }
	    
	    public void setDirection(double[][] direction) {
	        for (int i = 0; i < Math.min(nDims,3); i++) {
	        	for (int j = 0; j < Math.min(nDims,3); j++) {
	        		this.direction[i][j] = direction[i][j];
	        	}
	        }
	    }
	    
	    public void setPointData(Vector<Double> pointData) {
	    	this.pointData.clear();
	    	for (int i = 0; i < pointData.size(); i++) {
	    		this.pointData.add(pointData.get(i));
	    	}
	    }
	    
	    public void setPointLocation(Vector<Vector4d> pointLocation) {
	    	this.pointLocation.clear();
	    	for (int i = 0; i < pointLocation.size(); i++) {
	    		this.pointLocation.add(pointLocation.get(i));
	    	}
	    }
	    
	    public void generateData() {
	    	outputImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "outputImage");
	    	if (outputImage.getFileInfo() != null) {
	    		FileInfoBase[] fileInfo = outputImage.getFileInfo();
	    		for (int i = 0; i < fileInfo.length; i++) {
	    			fileInfo[i].setOrigin(origin);
	    			fileInfo[i].setResolutions(resolutions);
	    		}
	    	}
	    	for (int i = 0; i < Math.min(nDims,3); i++) {
                for (int j = 0; j < Math.min(nDims,3); j++) {
                    outputImage.getMatrix().set(i, j, (float)direction[i][j]);
                }
            }
	    	return;
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

