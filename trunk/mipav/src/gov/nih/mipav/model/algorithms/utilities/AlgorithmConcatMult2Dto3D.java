package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;





/**
 * @author senseneyj
 * This algorithm concats multiple 2D images of same type to a 3D image
 *
 */
public class AlgorithmConcatMult2Dto3D extends AlgorithmConcatMult {

	/**
	 * constructor
	 * @param images
	 * @param destImage
	 * @param copyAllInfo 
	 */
	public AlgorithmConcatMult2Dto3D(ModelImage[] images, ModelImage destImage, boolean copyAllInfo) {
		this.images = images;
		this.destImage = destImage;
		
		this.copyAllInfo = copyAllInfo;
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
	        
	        destImage.setExtents(new int[]{xDim, yDim, images.length});
	        
	        float[] resols = new float[3];
	        float[] origins = new float[3];
	        int[] units = new int[4];
	        
	       
	        
	        resols[0] = images[0].getFileInfo()[0].getResolutions()[0];
	        resols[1] = images[0].getFileInfo()[0].getResolutions()[1];
	        resols[2] = 1;
	        origins[0] = images[0].getFileInfo()[0].getOrigin(0);
	        origins[1] = images[0].getFileInfo()[0].getOrigin(1);
	        origins[2] = 0;
	        units[0] = images[0].getFileInfo()[0].getUnitsOfMeasure()[0];
            units[1] = images[0].getFileInfo()[0].getUnitsOfMeasure()[1];
            units[2] = Unit.UNKNOWN_MEASURE.getLegacyNum();
	        
	         FileInfoBase destFileInfo[] = null;
	         int numInfos = destImage.getExtents()[2];
	         int j;

	         destFileInfo = new FileInfoBase[numInfos];

	         if(copyAllInfo) {  
	             int sliceCounter = 0; //Keeps track of every slice to populate tag
                   for (int z = 0; z <destImage.getExtents()[2] ; z++) {
                       fireProgressStateChanged((100 * sliceCounter)/(destImage.getExtents()[2]));
                       
                       if(images[z].isDicomImage()) {
                           copyDicomInfo(destFileInfo, images[z].getFileInfo(0), resols, 0, z, sliceCounter); 
                           
                       } else {
                           destFileInfo[sliceCounter] = (FileInfoBase) images[z].getFileInfo(0).clone();
                           copyBaseInfo(destFileInfo, images[z].getFileInfo(0), resols, units, sliceCounter); //used for copying resolution inof
                       }
                       sliceCounter++; 
                   }
	               
	               destImage.setFileInfo(destFileInfo);
	         } else {
	             destFileInfo = destImage.getFileInfo();

	             for (int i = 0; i < destImage.getExtents()[2]; i++) {
	                 fireProgressStateChanged((100 * i)/(destImage.getExtents()[2]));
	                 copyBaseInfo(destFileInfo, images[i].getFileInfo()[0], resols, units, i);
	             }

	             int counter = 0;
	             for(int i=0;i<images.length;i++) {
	                 ModelImage img = images[i];
	          
                     if(img.getFileInfo()[0] instanceof FileInfoImageXML) {
                         if (((FileInfoImageXML) img.getFileInfo()[0]).getPSetHashtable() != null) {
                             ((FileInfoImageXML) destFileInfo[counter]).setPSetHashtable(((FileInfoImageXML) img.getFileInfo()[0]).getPSetHashtable());
                         }
                     }
                     counter++;
                 }
             }

	        setCompleted(true);
	        destFileInfo = null;

		}catch(Exception e) {
			e.printStackTrace();
		}

	}

}
