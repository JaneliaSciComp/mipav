package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import javax.swing.JTextField;

/**
 * @author pandyan
 * Vista format reader and writer
 * 
 * Spec:  http://static.cbs.mpg.de/lipsia/START/index3.html
 * 
 * 2/1/2011
 */
public class FileVista extends FileBase {
	
	/** The extensions of VISTA file. */
    public static final String[] EXTENSIONS = { ".v" };
    
    /** FileInfo Vista **/
    private FileInfoVista fileInfo;
    
    /** offset to image data **/
    private int offset;
    
    /** image **/
    private ModelImage image;
    
    /** file name and file dir **/
    private String fileName, fileDir;
    
    
    /**
     * constructor
     * @param fileName
     * @param fileDir
     */
    public FileVista(String fileName, String fileDir) {
    	this.fileName = fileName;
    	this.fileDir = fileDir;
    }
    
    
    /**
     * reads the header
     * @param imageFileName
     * @param fileDir
     * @return
     * @throws IOException
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
    	ArrayList<HashMap<String,String>> imagesInfo = new ArrayList<HashMap<String,String>>();
    	ArrayList<String> historyInfo = new ArrayList<String>();
    	
    	 File fileHeader = new File(fileDir + imageFileName);
    	 if (fileHeader.exists() == false) {
             Preferences.debug(fileDir + imageFileName + " cannot be found.\n");
             return false;
         }
    	 
    	 
    	//Open the Header File//
         try {
             raFile = new RandomAccessFile(fileHeader, "r");
         } catch (FileNotFoundException e) {
             Preferences.debug("raFile = new RandomAccessFile(fileHeader, r) gave " + "FileNotFoundException " + e);
             throw new IOException("Error on raFile = new RandomAccessFile(fileHeader,r)");
         }
         
         String nextLine;
         
         while(!((nextLine = raFile.readLine().trim()).equals("}"))) {
             if(nextLine.equals("image: image {")) {
            	 HashMap<String,String> imageHashMap = new HashMap<String,String>();
            	 while(!((nextLine = raFile.readLine()).trim().equals("}"))) {
            		 String[] splits = nextLine.split(":", 2);
            		 if(splits.length != 2) {
            			 return false;
            		 }
            		 imageHashMap.put(splits[0].trim(), splits[1].trim());
            	 }
            	 imagesInfo.add(imageHashMap);
             }else if(nextLine.equals("history: {")) {
            	 
            	 while(!((nextLine = raFile.readLine()).trim().equals("}"))) {
            		 historyInfo.add(nextLine);
            	 }
            	 
             }
             
         }
         
         //the header should have all been read in by now
         fileInfo.setImagesInfo(imagesInfo);
         fileInfo.setHistoryInfo(historyInfo);
         
         int[] extents;
         
         int yDim = Integer.parseInt(imagesInfo.get(0).get("nrows"));
         int xDim = Integer.parseInt(imagesInfo.get(0).get("ncolumns"));
         int zDim = Integer.parseInt(imagesInfo.get(0).get("nbands"));
         
         if(zDim == 1) {
        	 extents = new int[] { xDim, yDim};
         }else {
        	 if(imagesInfo.size() > 1) {
        		int z = imagesInfo.size();
        		int t = Integer.parseInt(imagesInfo.get(0).get("ntimesteps"));
        		 extents = new int[] { xDim, yDim, z, t};
        	 }else {
        		 extents = new int[] { xDim, yDim, zDim};
        	 }
         }
             
         fileInfo.setExtents(extents);  
             
    	 String orientation = imagesInfo.get(0).get("orientation");
    	 String convention = imagesInfo.get(0).get("convention");
    	 String resolutions = imagesInfo.get(0).get("voxel");
    	 resolutions = resolutions.substring(resolutions.indexOf("\"")+1, resolutions.lastIndexOf("\""));
    	 String[] res = resolutions.split("\\s+");
    	 float res0 = Float.valueOf(res[0]).floatValue();
    	 float res1 = Float.valueOf(res[1]).floatValue();
    	 float res2 = Float.valueOf(res[2]).floatValue();
    	 fileInfo.setResolutions(res0,0);
    	 fileInfo.setResolutions(res1,1);
    	 fileInfo.setResolutions(res2,2);
    	 if(extents.length == 4) {
    		 fileInfo.setResolutions(1,3);
    	 }
    	 fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
         fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
         fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 2);
     	 if(orientation.equals("axial")) {
     		fileInfo.setImageOrientation(FileInfoBase.AXIAL);
     		if(convention.equals("natural")) {
     			fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 0);
     		}else {
     			fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 0);
     		}
            fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 1);
            fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE, 2);
            
            
     	 }else if(orientation.equals("sagittal")) {
     		 fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
     		 
     		fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 0);
            fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE, 1);
            if(convention.equals("natural")) {
     			fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 2);
     		}else {
     			fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 2);
     		}
            
            
     	 }else if(orientation.equals("coronal")) {
     		 fileInfo.setImageOrientation(FileInfoBase.CORONAL);
     		 if(convention.equals("natural")) {
      			fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 2);
      		}else {
      			fileInfo.setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 2);
      		}
             fileInfo.setAxisOrientation(FileInfoBase.ORI_S2I_TYPE, 1);
             fileInfo.setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 2);
     	 }else {
     		 fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
     	 }
    	 

     	 fileInfo.setEndianess(BIG_ENDIAN);
     	 

    	 String type = imagesInfo.get(0).get("repn");
    	 if(type.equals("bit")) {
    		 fileInfo.setDataType(ModelStorageBase.BOOLEAN);
    	 }else if(type.equals("ubyte")) {
    		 fileInfo.setDataType(ModelStorageBase.UBYTE);
    	 }else if(type.equals("short")) {
    		 fileInfo.setDataType(ModelStorageBase.SHORT);
    	 }else if(type.equals("long")) {
    		 fileInfo.setDataType(ModelStorageBase.LONG);
    	 }else if(type.equals("float")) {
    		 fileInfo.setDataType(ModelStorageBase.FLOAT);
    	 }else if(type.equals("double")) {
    		 fileInfo.setDataType(ModelStorageBase.DOUBLE);
    	 }
    	 
    	 
    	 float []o;
         if(imagesInfo.size() > 1) {
             o = new float[4];
             for(int j=0;j<4;j++) o[j]=0;
         } else {
             o = new float[3];
             for(int j=0;j<3;j++) o[j]=0;
         }
         fileInfo.setOrigin(o);
         
         
    	 offset = (int)raFile.getFilePointer();

    	 raFile.close();
    	 
    	return true;
    }
    
    
    
    /**
     * reads the image
     * @return
     * @throws IOException
     * @throws OutOfMemoryError
     */
    public ModelImage readImage() throws IOException, OutOfMemoryError {
    	
    	
    	fileInfo = new FileInfoVista(fileName, fileDir, FileUtility.VISTA);
    	
    	
    	if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" Vista header file error"));
        }
    	
    	
    	image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
    	
    	
    	int[] extents = fileInfo.getExtents();
        if (image.getNDims() == 2) {
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image

            for (int i = 0; i < extents[2]; i++) {
                FileInfoVista newFileInfo = (FileInfoVista) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image
            for (int i = 0; i < (extents[2] * extents[3]); i++) {
            	FileInfoVista newFileInfo = (FileInfoVista) fileInfo.clone();
            	float[] newOrig = {0,0,0,i};
                newFileInfo.setOrigin(newOrig);
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    	
        
        FileRaw rawFile;
        rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
        linkProgress(rawFile);
        
        if(image.is3DImage() || image.is2DImage()) {
        	rawFile.readImage(image, offset);
        }else {
        	
        	if(fileInfo.getDataType() == ModelStorageBase.DOUBLE) {
        		double[] buffer;	
        		buffer = new double[extents[0] * extents[1]];
        		int length = extents[0] * extents[1];
        		int numVolumes = extents[3];
        		int numSlices = extents[2] * extents[3];
        		int counter1 = 0;
        		int counter2 = 0;
        		int location = 0;
            	for(int i=0;i<numSlices;i++) {
            		
            		rawFile.readImage(buffer, offset);
            		
            		
            		if(counter1 == numVolumes) {
            			counter1 = 0;
            			counter2++;
            		}
            		location = (counter1 * extents[2]) + counter2;
            		counter1++;
            		
            		
            		image.importData(location * length, buffer, false);
            		
            		
            		
            		offset = (int)rawFile.getRaFile().getFilePointer();
            		
            	}
            	image.calcMinMax();
        	}else {
        		short[] buffer;	
        		buffer = new short[extents[0] * extents[1]];
        		int length = extents[0] * extents[1];
        		int numVolumes = extents[3];
        		int numSlices = extents[2] * extents[3];
        		int counter1 = 0;
        		int counter2 = 0;
        		int location = 0;
            	for(int i=0;i<numSlices;i++) {
            		
            		rawFile.readImage(buffer, offset, fileInfo.getDataType());
            		
            		
            		if(counter1 == numVolumes) {
            			counter1 = 0;
            			counter2++;
            		}
            		location = (counter1 * extents[2]) + counter2;
            		counter1++;
            		
            		
            		image.importData(location * length, buffer, false);
            		
            		
            		
            		offset = (int)rawFile.getRaFile().getFilePointer();
            		
            	}
            	image.calcMinMax();
        	}
        	
        	
        	
        	
        }
      
        
        rawFile.close();
        

    	
    	return image;
    }
    
    
    
    
    /**
     * writes the header
     * @param image
     * @param rawFile
     * @param vistaParamFields
     * @return
     * @throws IOException
     */
    public boolean writeHeader(ModelImage image, FileRaw rawFile, ArrayList<JTextField> vistaParamFields) throws IOException {

    	 rawFile.writeBytes("V-data 2 {\n");
    	 if(image.getFileInfo(0)instanceof  FileInfoVista) {
    		 ArrayList<String> historyInfo = ((FileInfoVista)(image.getFileInfo(0))).getHistoryInfo();
    		 if(historyInfo != null && historyInfo.size() > 0) {
    			 rawFile.writeBytes("\thistory: {\n");
    			 for(int i=0;i<historyInfo.size();i++) {
    				 rawFile.writeBytes("\t\t" + historyInfo.get(i) + "\n");
    			 }
    			 rawFile.writeBytes("\t}\n");
    		 }
 
    	 }
    		 if(image.is3DImage()) {
    			 String key,value;
    			 rawFile.writeBytes("\timage: image {\n");
    			 
    			 key = "data";
    			 value = "0";
    			 rawFile.writeBytes("\t\t" + key + ":" + value + "\n");
    			 
    			 int factor = 1;
    			 if(image.getFilterType() == ModelStorageBase.BYTE) {
    				 factor = 1;
    			 }else if(image.getFilterType() == ModelStorageBase.SHORT) {
    				 factor = 2;
    			 }else if(image.getFilterType() == ModelStorageBase.LONG) {
    				 factor = 8;
    			 }else if(image.getFilterType() == ModelStorageBase.FLOAT) {
    				 factor = 4;
    			 }else if(image.getFilterType() == ModelStorageBase.DOUBLE) {
    				 factor = 8;
    			 }
    			 
    			 
    			 int len = image.getExtents()[0] * image.getExtents()[1] * image.getExtents()[2] * factor;
    			 
    			 key = "length";
    			 value = String.valueOf(len);
    			 rawFile.writeBytes("\t\t" + key + ":" + value + "\n");
    			 
    			 
    			 for(int i=0;i<vistaParamFields.size();i++) {
    				 JTextField field = vistaParamFields.get(i);
    				 key = field.getName();
    				 value = field.getText().trim();
    				 
    				 if(!value.equals("")) {
    					 rawFile.writeBytes("\t\t" + key + ":" + value + "\n");
    				 }
    				 
    			 }
    			 
    			 
    		
    			 
    			 rawFile.writeBytes("\t}\n");
    		 }else {
    			 int data = 0;
    			 for(int i=0;i<image.getExtents()[2];i++) {
    				 
    				 String key,value;
        			 rawFile.writeBytes("\timage: image {\n");
        			 
        			 key = "data";
        			 value = String.valueOf(data);
        			 rawFile.writeBytes("\t\t" + key + ":" + value + "\n");
        			 
        			 int factor = 1;
        			 if(image.getFilterType() == ModelStorageBase.BYTE) {
        				 factor = 1;
        			 }else if(image.getFilterType() == ModelStorageBase.SHORT) {
        				 factor = 2;
        			 }else if(image.getFilterType() == ModelStorageBase.LONG) {
        				 factor = 8;
        			 }else if(image.getFilterType() == ModelStorageBase.FLOAT) {
        				 factor = 4;
        			 }else if(image.getFilterType() == ModelStorageBase.DOUBLE) {
        				 factor = 8;
        			 }
        			 
        			 
        			 int len = image.getExtents()[0] * image.getExtents()[1] * image.getExtents()[2] * factor;
        			 data = data + len;
        			 
        			 key = "length";
        			 value = String.valueOf(len);
        			 rawFile.writeBytes("\t\t" + key + ":" + value + "\n");
        			 
        			 
        			 for(int k=0;k<vistaParamFields.size();k++) {
        				 JTextField field = vistaParamFields.get(k);
        				 key = field.getName();
        				 value = field.getText().trim();
        				 
        				 if(!value.equals("")) {
        					 rawFile.writeBytes("\t\t" + key + ":" + value + "\n");
        				 }
        				 
        			 }

        			 rawFile.writeBytes("\t}\n");

    			 }
    		 }
    	 rawFile.writeBytes("}\n");
    	 
    	 return true;

    }
    
    
    /**
     * writes the image
     * @param image
     * @param options
     * @param vistaParamFields
     * @throws IOException
     */
    public void writeImage(ModelImage image, FileWriteOptions options, ArrayList<JTextField> vistaParamFields) throws IOException {

    	FileInfoXML tempInfo = new FileInfoImageXML(options.getFileName(), options.getFileDirectory(), FileUtility.RAW);
    	tempInfo.setEndianess(FileBase.BIG_ENDIAN);
        FileRaw rawFile = new FileRaw(fileName, fileDir, tempInfo, FileBase.READ_WRITE);
        
        writeHeader(image, rawFile, vistaParamFields);
        

        
        linkProgress(rawFile);
        rawFile.setStartPosition(rawFile.getRaFile().length());
        rawFile.setZeroLengthFlag(false);
        
        if(image.is4DImage()) {
        	RandomAccessFile ra = rawFile.getRaFile();
        	
        	 FileRawChunk rawChunkFile;
             rawChunkFile = new FileRawChunk(ra, image.getFileInfo(0));
        	
        	if(image.getFileInfo(0).getDataType() == ModelStorageBase.BYTE) {
        		
        	}
             
             
            short[] buffer;	
            int[] extents = image.getExtents();
     		buffer = new short[extents[0] * extents[1]];
     		int length = extents[0] * extents[1];
     		int numVolumes = extents[3];
     		int numSlices = extents[2] * extents[3];
     		int counter1 = 0;
     		int counter2 = 0;
     		int location = 0;
         	for(int i=0;i<numSlices;i++) {
         		
         		if(counter1 == numVolumes) {
         			counter1 = 0;
         			counter2++;
         		}
         		location = (counter1 * extents[2]) + counter2;
         		counter1++;
         		
         		
         		image.exportData(location * length, buffer.length, buffer);
         		
         		
         		rawChunkFile.writeBufferShort(buffer, 0, length, FileBase.BIG_ENDIAN);
         		

         		
         	}

        }else {
        	rawFile.writeImage(image, options);
        }
        
        

        rawFile.close();
    }
    
    
    
    
    
    
    

}
