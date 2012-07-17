package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.structures.ModelImage;



/**
 * @author pandyan
 * This algorithm concats multiple 3D images of same type to a 4D image
 *
 */
public class AlgorithmConcatMult3Dto4D extends AlgorithmBase {
	
	/** array of 3D images **/
	private ModelImage[] images;
	
	/** final image **/
	private ModelImage destImage;

	/** x and y dimensions of image */
    private int xDim, yDim;

	/** Whether to copy all file information */
    private boolean copyAllInfo = false;
	
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
	        

	        resols[0] = images[0].getFileInfo()[0].getResolutions()[0];
	        resols[1] = images[0].getFileInfo()[0].getResolutions()[1];
	        resols[2] = images[0].getFileInfo()[0].getResolutions()[2];
	        resols[3] = 1;
	        origins[0] = images[0].getFileInfo()[0].getOrigin(0);
	        origins[1] = images[0].getFileInfo()[0].getOrigin(1);
	        origins[2] = images[0].getFileInfo()[0].getOrigin(2);
	        origins[3] = 0;

	        FileInfoBase[] fileInfo = null;
	        

            
      
         FileInfoBase destFileInfo[] = null;
         int numInfos = destImage.getExtents()[3]*destImage.getExtents()[2];
         int j;

         destFileInfo = new FileInfoBase[numInfos];

         if(copyAllInfo) {  
             int sliceCounter = 0; //Keeps track of every slice to populate tag
               for (int t = 0; t < destImage.getExtents()[3]; t++) {
                   for (int z = 0; z <destImage.getExtents()[2] ; z++) {
                       fireProgressStateChanged((100 * sliceCounter)/(destImage.getExtents()[3]));
                       j = (t*destImage.getExtents()[2]) + z;
                       
                       if(images[0].isDicomImage()) {
                           copyDicomInfo(destFileInfo, resols, j, z, t, sliceCounter); 
                           
                       } else {
                           destFileInfo[sliceCounter] = (FileInfoBase) images[t].getFileInfo(z).clone();
                           copyBaseInfo(destFileInfo, images[t].getFileInfo(z), resols, sliceCounter); //used for copying resolution inof
                       }
                       sliceCounter++; 
                   }
               }
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
             
               
        destImage.setFileInfo(destFileInfo);

        setCompleted(true);
        fileInfo = null;

		}catch(Exception e) {
			e.printStackTrace();
		}
        
		
		

	}
	
	private void copyBaseInfo(FileInfoBase[] fileInfo, FileInfoBase srcFileInfo, float[] resols, int i) {
	    fileInfo[i].setModality(srcFileInfo.getModality());
        fileInfo[i].setFileDirectory(srcFileInfo.getFileDirectory());
        fileInfo[i].setEndianess(srcFileInfo.getEndianess());
        fileInfo[i].setUnitsOfMeasure(srcFileInfo.getUnitsOfMeasure());
        fileInfo[i].setResolutions(resols);
        fileInfo[i].setExtents(destImage.getExtents());
        fileInfo[i].setMax(destImage.getMax());
        fileInfo[i].setMin(destImage.getMin());
        fileInfo[i].setImageOrientation(srcFileInfo.getImageOrientation());
        fileInfo[i].setPixelPadValue(srcFileInfo.getPixelPadValue());
        fileInfo[i].setPhotometric(srcFileInfo.getPhotometric());
        fileInfo[i].setAxisOrientation(srcFileInfo.getAxisOrientation());
	}

	/**
	 * Most efficient way of creating DICOM tags for 4-D. Uses pointers based on srcimage dicom tags  
	 * 
	 * @param destFileInfo
	 * @param resols
	 * @param j
	 * @param z
	 * @param t
	 * @param sliceCounter
	 */
    private void copyDicomInfo(FileInfoBase[] destFileInfo, float[] resols, int j, int z, int t, int sliceCounter) {
        FileInfoDicom oldDicomInfo = (FileInfoDicom) images[t].getFileInfo(z);
        if (z == 0) {
            destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                            oldDicomInfo.getFileFormat());
            ((FileInfoDicom)destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type());     
        }
        else {
            destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                            oldDicomInfo.getFileFormat(), (FileInfoDicom) destFileInfo[t*(destImage.getExtents()[2])]);
            
            ((FileInfoDicom)destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type()); 

           
        }
         
        double sliceResolution = 0.0;
        
         FileDicomTagTable newTagTable = ((FileInfoDicom) destFileInfo[j]).getTagTable();
         if (newTagTable.getValue("0018,0088") != null) {
             String sliceGapString = ((String) ((FileInfoDicom) destFileInfo[j]).getTagTable().getValue("0018,0088")).trim();
             sliceResolution = new Double(sliceGapString.trim()).doubleValue();
         }          
         
         fireProgressStateChanged((((100 * (t*2)))/(destImage.getExtents()[2]+1)));
         resols[0] = images[t].getFileInfo(z).getResolutions()[0];
         resols[1] = images[t].getFileInfo(z).getResolutions()[1];
         resols[2] = images[t].getFileInfo(z).getResolutions()[2];
         resols[3] = (float)sliceResolution;
         destFileInfo[sliceCounter].setResolutions(resols);
         destFileInfo[sliceCounter].setExtents(destImage.getExtents());
         destFileInfo[sliceCounter].setAxisOrientation(images[t].getFileInfo(z).getAxisOrientation()[0], 0);
         destFileInfo[sliceCounter].setAxisOrientation(images[t].getFileInfo(z).getAxisOrientation()[1], 1);
         destFileInfo[sliceCounter].setAxisOrientation(images[t].getFileInfo(z).getAxisOrientation()[2], 2);
         destFileInfo[sliceCounter].setImageOrientation(images[t].getFileInfo(z).getImageOrientation());  
         ((FileInfoDicom) destFileInfo[j]).getTagTable().importTags((FileInfoDicom) images[t].getFileInfo(z));
         ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0011", new Short((short) xDim), 2); // columns
         ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0010", new Short((short) yDim), 2); // rows                 
         ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0020,0013", Short.toString((short) (t + 1)),
                                                  Short.toString((short) (t + 1)).length()); // instance number
         ((FileInfoDicom) destFileInfo[j]).getTagTable().removeTag("0019,100A");// Removes NumberofImages in Mosaic Tag
         
        
    }

}
