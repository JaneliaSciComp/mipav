package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.structures.ModelImage;

public class AlgorithmConcatMult3Dto3D extends AlgorithmBase {

	
	/** array of 3D images **/
	private ModelImage[] images;
	
	/** final image **/
	private ModelImage destImage;

	
	
	/**
	 * comstructor
	 * @param images
	 * @param destImage
	 * @param copyAllInfo 
	 */
	public AlgorithmConcatMult3Dto3D(ModelImage[] images, ModelImage destImage, boolean copyAllInfo) {
		this.images = images;
		this.destImage = destImage;
		
	}
	
	/**
     * comstructor
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

	        
	        
	        boolean isFileInfoDicom = true;
	        for(int i=0;i<images.length;i++) {
	        	if(!(images[i].getFileInfo()[0] instanceof FileInfoDicom)) {
	        		isFileInfoDicom = false;
	        		break;
	        	}
	        }
	        FileInfoBase[] fileInfo = null;
	        FileInfoDicom[] fileInfoDicom = null;
	        

	        if (isFileInfoDicom) {
	            fileInfoDicom = new FileInfoDicom[destImage.getExtents()[2]];
	            
	            counter = 0;
		        for(int i=0;i<images.length;i++) {
		        	ModelImage img = images[i];
		        	for(int k=0;k<img.getExtents()[2];k++) {
		        		fileInfoDicom[counter] = (FileInfoDicom) (((FileInfoDicom) img.getFileInfo()[k]).clone());
		        		fileInfoDicom[counter].setResolutions(resols);
		                fileInfoDicom[counter].setOrigin(origins);
		        		
		        		
		        		counter++;
		        	}
		        }

	            destImage.setFileInfo(fileInfoDicom);

	        } else {
	            fileInfo = destImage.getFileInfo();

	            for (int i = 0; (i < destImage.getExtents()[2]); i++) {
	                fileInfo[i].setModality(images[0].getFileInfo()[0].getModality());
	                fileInfo[i].setFileDirectory(images[0].getFileInfo()[0].getFileDirectory());
	                fileInfo[i].setEndianess(images[0].getFileInfo()[0].getEndianess());
	                fileInfo[i].setUnitsOfMeasure(images[0].getFileInfo()[0].getUnitsOfMeasure());
	                fileInfo[i].setResolutions(resols);
	                fileInfo[i].setExtents(destImage.getExtents());
	                fileInfo[i].setMax(destImage.getMax());
	                fileInfo[i].setMin(destImage.getMin());
	                fileInfo[i].setImageOrientation(images[0].getImageOrientation());
	                fileInfo[i].setPixelPadValue(images[0].getFileInfo()[0].getPixelPadValue());
	                fileInfo[i].setPhotometric(images[0].getFileInfo()[0].getPhotometric());
	                fileInfo[i].setAxisOrientation(images[0].getAxisOrientation());
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
	        fileInfoDicom = null;

		}catch(Exception e) {
			e.printStackTrace();
		}
        
		
		

	}

}
