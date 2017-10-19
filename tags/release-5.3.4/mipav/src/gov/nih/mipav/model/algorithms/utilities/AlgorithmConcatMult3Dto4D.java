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

	
	
	/**
	 * constructor
	 * @param images
	 * @param destImage
	 */
	public AlgorithmConcatMult3Dto4D(ModelImage[] images, ModelImage destImage) {
		this.images = images;
		this.destImage = destImage;
		
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




	        fileInfo = images[1].getFileInfo();
	            
            FileInfoBase destFileInfo[] = null;
            int numInfos = destImage.getExtents()[3]*destImage.getExtents()[2];
            FileInfoDicom oldDicomInfo = null;
            FileDicomTagTable[] childTagTables = null;
            int j;

            destFileInfo = new FileInfoBase[numInfos];
            oldDicomInfo = (FileInfoDicom) images[1].getFileInfo(0);
            childTagTables = new FileDicomTagTable[numInfos - 1];
            int sliceCounter = 0; //Keeps track of every slice to populate tag
            double sliceResolution = 1.0;

           // Most efficient way of creating DICOM tags for 4-D. Uses parentTagTables based on srcimage dicom tags    
           for (int t = 0; t < destImage.getExtents()[3]; t++) {
               for (int z = 0; z <destImage.getExtents()[2] ; z++) {
                   j = (t*destImage.getExtents()[2]) + z;
                   if (j == 0) {
                       // create a new reference file info
                       destFileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                       oldDicomInfo.getFileFormat());
                       ((FileInfoDicom)destFileInfo[0]).setVr_type(oldDicomInfo.getVr_type());    
                   }
                   else {

                       // all other slices are children of the first file info..
                       destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                       oldDicomInfo.getFileFormat(), (FileInfoDicom) destFileInfo[0]);
                       
                       ((FileInfoDicom)destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type());
                       childTagTables[j - 1] = ((FileInfoDicom) destFileInfo[j]).getTagTable();
                   }
                    
                    FileDicomTagTable newTagTable = ((FileInfoDicom) destFileInfo[t]).getTagTable();
                    if (newTagTable.getValue("0018,0088") != null) {
                        String sliceGapString = ((String) ((FileInfoDicom) destFileInfo[t]).getTagTable().getValue("0018,0088")).trim();
                        sliceResolution = new Double(sliceGapString.trim()).doubleValue();
                    }

                              
                    fireProgressStateChanged(((100 * t))/(destImage.getExtents()[3]-1));
                        resols[0] = images[1].getFileInfo(0).getResolutions()[0];
                        resols[1] = images[1].getFileInfo(0).getResolutions()[1];
                        resols[2] = 1.0f;
                        resols[3] = 1.0f;
                        //resols[4] = 1;
                        destFileInfo[sliceCounter].setResolutions(resols);
                        destFileInfo[sliceCounter].setExtents(destImage.getExtents());
                        ((FileInfoDicom) destFileInfo[t]).getTagTable().setValue("0028,0011", new Short((short) xDim), 2); // columns
                        ((FileInfoDicom) destFileInfo[t]).getTagTable().setValue("0028,0010", new Short((short) yDim), 2); // rows                 
                        ((FileInfoDicom) destFileInfo[t]).getTagTable().setValue("0020,0013", Short.toString((short) (t + 1)),
                                                                 Short.toString((short) (t + 1)).length()); // instance number
                        ((FileInfoDicom) destFileInfo[t]).getTagTable().importTags((FileInfoDicom) fileInfo[t]);
                        ((FileInfoDicom) destFileInfo[t]).getTagTable().removeTag("0019,100A");// Removes NumberofImages in Mosaic Tag
                         
                         sliceCounter++;  
                }
               
           }
           
            ((FileInfoDicom) destFileInfo[0]).getTagTable().attachChildTagTables(childTagTables);
            destImage.setFileInfo(destFileInfo);
            
            /*Old algorithm to get all dicom tags for each slice
             * fileInfoDicom = new FileInfoDicom[destImage.getExtents()[2] * destImage.getExtents()[3]];
            
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

            destImage.setFileInfo(fileInfoDicom);*/
        

	        } else {
	            fileInfo = destImage.getFileInfo();

	            for (int i = 0; (i < (destImage.getExtents()[2] * destImage.getExtents()[3])); i++) {
	                fireProgressStateChanged((100 * i)/(destImage.getExtents()[3]));
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