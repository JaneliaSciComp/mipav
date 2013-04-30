package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.structures.ModelImage;



/**
 * @author senseneyj
 * This algorithm concats multiple 3D images of same type to a 4D image
 *
 */
public class AlgorithmConcatMult3Dto4D extends AlgorithmConcatMult {

	/**
	 * constructor
	 * @param images
	 * @param destImage
	 * @param copyAllInfo 
	 */
	public AlgorithmConcatMult3Dto4D(ModelImage[] images, ModelImage destImage, boolean copyAllInfo) {
		this.images = images;
		this.destImage = destImage;
		this.copyAllInfo = copyAllInfo;
	}
	
	/**
     * constructor
     * @param images
     * @param destImage
     */
    public AlgorithmConcatMult3Dto4D(ModelImage[] images, ModelImage destImage) {
        this(images, destImage, false);
    }
	
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		try{
			float[] buffer;
			int cFactor = 1;
			
			xDim = images[0].getExtents()[0];
	        yDim = images[0].getExtents()[1];
	
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

	        float[] resols = new float[4];
	        float[] origins = new float[4];
	        int[] units = new int[4];
	        

	        resols[0] = images[0].getFileInfo()[0].getResolutions()[0];
	        resols[1] = images[0].getFileInfo()[0].getResolutions()[1];
	        resols[2] = images[0].getFileInfo()[0].getResolutions()[2];
	        resols[3] = 1;
	        origins[0] = images[0].getFileInfo()[0].getOrigin(0);
	        origins[1] = images[0].getFileInfo()[0].getOrigin(1);
	        origins[2] = images[0].getFileInfo()[0].getOrigin(2);
	        origins[3] = 0;
            units[0] = images[0].getFileInfo()[0].getUnitsOfMeasure()[0];
            units[1] = images[0].getFileInfo()[0].getUnitsOfMeasure()[1];
            units[2] = images[0].getFileInfo()[0].getUnitsOfMeasure()[2];
            units[3] = Unit.SECONDS.getLegacyNum();
      
         FileInfoBase destFileInfo[] = null;
         int numInfos = destImage.getExtents()[3]*destImage.getExtents()[2];
         int j;

         destFileInfo = new FileInfoBase[numInfos];

         if(copyAllInfo) {  
               int sliceCounter = 0; //Keeps track of every slice to populate tag
               for (int t = 0; t < destImage.getExtents()[3]; t++) {
                   for (int z = 0; z <destImage.getExtents()[2] ; z++) {
                       fireProgressStateChanged((100 * sliceCounter)/(destImage.getExtents()[3]));
                       
                       if(images[t].isDicomImage()) {
                           copyDicomInfo(destFileInfo, images[t].getFileInfo(0), resols, z, t, sliceCounter); 
                           
                       } else {
                           destFileInfo[sliceCounter] = (FileInfoBase) images[t].getFileInfo(z).clone();
                           copyBaseInfo(destFileInfo, images[t].getFileInfo(z), resols, origins, units, sliceCounter); //used for copying resolution inof
                       }
                       sliceCounter++; 
                   }
               }
               
               destImage.setFileInfo(destFileInfo);
         } else {
             destFileInfo = destImage.getFileInfo();

             for (int i = 0; (i < (destImage.getExtents()[2] * destImage.getExtents()[3])); i++) {
                 fireProgressStateChanged((100 * i)/(destImage.getExtents()[3]));
                 copyBaseInfo(destFileInfo, images[0].getFileInfo()[0], resols, origins, units, i);
             }

             counter = 0;
             for(int i=0;i<images.length;i++) {
                 ModelImage img = images[i];
                 for(int k=0;k<img.getExtents()[2];k++) {
          
                     if(img.getFileInfo()[0] instanceof FileInfoImageXML) {
                         if (((FileInfoImageXML) img.getFileInfo()[k]).getPSetHashtable() != null) {
                             ((FileInfoImageXML) destFileInfo[counter]).setPSetHashtable(((FileInfoImageXML) img.getFileInfo()[k]).getPSetHashtable());
                         }
                     }
                     counter++;
                 }
             }
         }

        setCompleted(true);
        destFileInfo = null;

		}catch(Exception e) {
			e.printStackTrace();
		}
        
		
		

	}
	
	

}
