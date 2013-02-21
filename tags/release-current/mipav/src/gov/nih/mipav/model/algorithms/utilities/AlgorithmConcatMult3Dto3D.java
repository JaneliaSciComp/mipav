package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.structures.ModelImage;

public class AlgorithmConcatMult3Dto3D extends AlgorithmConcatMult {

	/**
	 * constructor
	 * @param images
	 * @param destImage
	 * @param copyAllInfo 
	 */
	public AlgorithmConcatMult3Dto3D(ModelImage[] images, ModelImage destImage, boolean copyAllInfo) {
		this.images = images;
		this.destImage = destImage;
		this.copyAllInfo = copyAllInfo;
		
	}
	
	/**
     * constructor
     * @param images
     * @param destImage
     */
    public AlgorithmConcatMult3Dto3D(ModelImage[] images, ModelImage destImage) {
        this(images, destImage, false);
    }
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		try{
			float[] buffer;
			int cFactor = 1;
			
			int xDim = images[0].getExtents()[0];
	        int yDim = images[0].getExtents()[1];
	
	        if (images[0].isColorImage()) {
	            cFactor = 4;
	        }
	        else if (images[0].isComplexImage()) {
	        	cFactor = 2;
	        }
	
	        int length = cFactor * xDim * yDim;
	        
	        buffer = new float[length];
	        
	        
	        int counter = 0;
	        for(int i=0;i<images.length;i++) {
	        	ModelImage img = images[i];
	        	for(int k=0;k<img.getExtents()[2];k++) {
	        		img.exportData(k * buffer.length, buffer.length, buffer);
	        		destImage.importData(counter * buffer.length, buffer, false);
	        		counter++;
	        	}
	        }
	        
	        destImage.calcMinMax();

	        float[] resols = new float[3];
	        float[] origins = new float[3];
	        

	        resols[0] = images[0].getFileInfo()[0].getResolutions()[0];
	        resols[1] = images[0].getFileInfo()[0].getResolutions()[1];
	        resols[2] = images[0].getFileInfo()[0].getResolutions()[2];

	        origins[0] = images[0].getFileInfo()[0].getOrigin(0);
	        origins[1] = images[0].getFileInfo()[0].getOrigin(1);
	        origins[2] = images[0].getFileInfo()[0].getOrigin(2);

	        FileInfoBase[] fileInfo = new FileInfoBase[destImage.getExtents()[2]];
	        

	        if (copyAllInfo) {
	            counter = 0;
	            for(int i=0; i<images.length; i++) {
    	            ModelImage img = images[i];
                    for(int k=0;k<img.getExtents()[2];k++) {
                        fireProgressStateChanged((100 * counter)/(destImage.getExtents()[2]));
                        
                        if(images[i].isDicomImage()) {
                            copyDicomInfo(fileInfo, images[i].getFileInfo(0), resols, k, i, counter); 
                        } else {
                            fileInfo[counter] = (FileInfoBase) images[i].getFileInfo(k).clone();
                            copyBaseInfo(fileInfo, images[i].getFileInfo(k), resols, counter); //used for copying resolution inof
                        }
                        counter++; 
                    }
		        }

		        destImage.setFileInfo(fileInfo);

	        } else {
	            fileInfo = destImage.getFileInfo();

	             for (int i = 0; (i < (destImage.getExtents()[2] * destImage.getExtents()[3])); i++) {
	                 fireProgressStateChanged((100 * i)/(destImage.getExtents()[3]));
	                 copyBaseInfo(fileInfo, images[0].getFileInfo()[0], resols, i);
	             }

	            counter = 0;
		        for(int i=0;i<images.length;i++) {
		        	ModelImage img = images[i];
		        	for(int k=0;k<img.getExtents()[2];k++) {
		     
		        		if(img.getFileInfo()[0] instanceof FileInfoImageXML) {
		        			if (((FileInfoImageXML) img.getFileInfo()[k]).getPSetHashtable() != null) {
		                        ((FileInfoImageXML) fileInfo[counter]).setPSetHashtable(((FileInfoImageXML) img.getFileInfo()[k]).getPSetHashtable());
		                    }
		        		}
		        		counter++;
		        	}
		        }
	        }

	        setCompleted(true);
	        fileInfo = null;

		}catch(Exception e) {
			e.printStackTrace();
		}
        
		
		

	}

}
