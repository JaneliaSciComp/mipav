package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.structures.ModelImage;





/**
 * @author pandyan
 * This algorithm concats multiple 2D images of same type to a 3D image
 *
 */
public class AlgorithmConcatMult2Dto3D extends AlgorithmBase {
	
	
	
	
	
	/** array of 2D images **/
	private ModelImage[] images;
	
	/** final 3D image **/
	private ModelImage destImage;
	

	/**
	 * constructor
	 * @param images
	 * @param destImage
	 * @param copyAllInfo 
	 */
	public AlgorithmConcatMult2Dto3D(ModelImage[] images, ModelImage destImage, boolean copyAllInfo) {
		this.images = images;
		this.destImage = destImage;
		
		
	}
	
	/**
     * constructor
     * @param images
     * @param destImage
     */
    public AlgorithmConcatMult2Dto3D(ModelImage[] images, ModelImage destImage) {
        this(images, destImage, false);
    }
	
	
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
	        
	        
	     
	        for(int i=0;i<images.length;i++) {
	        	ModelImage img = images[i];
	        	img.exportData(0, buffer.length, buffer);
	        	destImage.importData(i * buffer.length, buffer, false);
	        	
	        }
	        
	        destImage.calcMinMax();
	        
	        float[] resols = new float[3];
	        float[] origins = new float[3];
	        
	       
	        
	        resols[0] = images[0].getFileInfo()[0].getResolutions()[0];
	        resols[1] = images[0].getFileInfo()[0].getResolutions()[1];
	        resols[2] = 1;
	        origins[0] = images[0].getFileInfo()[0].getOrigin(0);
	        origins[1] = images[0].getFileInfo()[0].getOrigin(1);
	        origins[2] = 0;

	        
	        
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
	            
	 
		        for(int i=0;i<images.length;i++) {
		        	ModelImage img = images[i];
		        	fileInfoDicom[i] = (FileInfoDicom) (((FileInfoDicom) img.getFileInfo()[0]).clone());
		        	fileInfoDicom[i].setResolutions(resols);
		            fileInfoDicom[i].setOrigin(origins);
		        }

	            destImage.setFileInfo(fileInfoDicom);

	        } else {
	            fileInfo = destImage.getFileInfo();
	            int[] units = new int[3];
	            units[0] = images[0].getFileInfo()[0].getUnitsOfMeasure()[0];
	            units[1] = images[0].getFileInfo()[0].getUnitsOfMeasure()[1];
	            units[2] = images[0].getFileInfo()[0].getUnitsOfMeasure()[1];
	            for (int i = 0; i < destImage.getExtents()[2]; i++) {
	                fileInfo[i].setModality(images[0].getFileInfo()[0].getModality());
	                fileInfo[i].setFileDirectory(images[0].getFileInfo()[0].getFileDirectory());
	                fileInfo[i].setEndianess(images[0].getFileInfo()[0].getEndianess());
	                fileInfo[i].setUnitsOfMeasure(units);
	                fileInfo[i].setResolutions(resols);
	                fileInfo[i].setExtents(destImage.getExtents());
	                fileInfo[i].setMax(destImage.getMax());
	                fileInfo[i].setMin(destImage.getMin());
	                fileInfo[i].setImageOrientation(images[0].getImageOrientation());
	                fileInfo[i].setPixelPadValue(images[0].getFileInfo()[0].getPixelPadValue());
	                fileInfo[i].setPhotometric(images[0].getFileInfo()[0].getPhotometric());
	                fileInfo[i].setAxisOrientation(images[0].getAxisOrientation());
	            }
	            
	            

		        for(int i=0;i<images.length;i++) {
		        	ModelImage img = images[i];
	        		if(img.getFileInfo()[0] instanceof FileInfoImageXML) {
	        			if (((FileInfoImageXML) img.getFileInfo()[0]).getPSetHashtable() != null) {
	                        ((FileInfoImageXML) fileInfo[i]).setPSetHashtable(((FileInfoImageXML) img.getFileInfo()[0]).getPSetHashtable());
	                    }
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
