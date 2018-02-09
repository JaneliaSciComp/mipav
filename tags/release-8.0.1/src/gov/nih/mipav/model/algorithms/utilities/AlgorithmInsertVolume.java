package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;

import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Dimension;
import java.io.IOException;
import java.text.DecimalFormat;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;


public class AlgorithmInsertVolume extends AlgorithmBase {
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------
    
    /** Weighted average slice type. */
    public static final int WEIGHTED_AVERAGE = 0;

    /** Average slice type - the inserted slice is set equal to the mean of the 2 surrounding slices. */
    public static final int AVERAGE = 1;

    /** Blank slice type - the inserted slice is blank. */
    public static final int BLANK = 2;

    /** Original slice type - a 2D image is inserted. */
    public static final int INSERTED_IMAGE = 3;

    /** Copy adjacent slice. */
    public static final int ADJACENT_BACK = 4;

    /** Copy adjacent slice. */
    public static final int ADJACENT_NEXT = 5;


    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Image inserted for slice type == ORIGINAL. */
    private ModelImage insertedImage;

    /** Number of slice before which another slice is inserted. */
    private int insertSlice;

    /** Original T dimension of the image. */
    private int Tdim;

    /** Area of a slice (Xdim * Ydim). */
    private int sliceArea;

    /** X dimension of the image. */
    private int Xdim;

    /** Y dimension of the image. */
    private int Ydim;

    /** Z dimension of the image. */
    private int Zdim;

    private int volumeType;
    
    private int insertVolumePos;
    
    /** For weighted averaging. */
    private float weightPrevious = 0.5f;

    private float[] imageBuffer2;
    
    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Import source and destination images into the class.
     * 
     * @param srcImage source image (image to clip from)
     * @param destImage destination image (image to paste to)
     * @param volumeType WEIGHTED_AVERAGE or AVERAGE or BLANK or ADJACENT_BACK or ADJACENT_NEXT
     * @param insertVolumePos position of volume to be inserted 
     * @param insertedImage (image designated by user to be inserted)
     */
    public AlgorithmInsertVolume(ModelImage srcImage, ModelImage destImage, int volumeType, int insertVolumePos,
            ModelImage insertedImage) {
        super(destImage, srcImage);
        // this.insertSlice = insertSlice;
        this.volumeType = volumeType;
        this.insertVolumePos = insertVolumePos;
        this.insertedImage = insertedImage;

        // get local attributes from this.srcImage
        Xdim = srcImage.getExtents()[0];
        Ydim = srcImage.getExtents()[1];
        Zdim = srcImage.getExtents()[2];
        Tdim = srcImage.getExtents()[3];
        sliceArea = Xdim * Ydim; // one slice has sliceArea number of pixels

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Runs algorithm.
     */
    public void runAlgorithm() {
        int t;
        int z; // z is slice-depth of srcImage
        int Z = 0;
        int sliceCounter = 0; // T is slice-depth of destination
        int sliceCounter2 = 0;
        float[] imageBuffer;
        int tDim;
        int colorFactor;
        int tNewOffset;
        DecimalFormat nf;
        int[] destImageExtents;
        double sliceResolution = 1.0;
        float resolutions[] = null;
        
        destImageExtents = new int[4];
        destImageExtents[0] = srcImage.getExtents()[0];
        destImageExtents[1] = srcImage.getExtents()[1];
        destImageExtents[2] = srcImage.getExtents()[2];
        destImageExtents[3] = srcImage.getExtents()[3] + 1;
        
        

        // create <FileInfoBase fileInfoBuffer;> buffer; may be of type FileInfoDicom
        try {
            nf = new DecimalFormat("##0.000000");

            if (srcImage.getNDims() == 4) {
                tDim = srcImage.getExtents()[3];
            } else {
                tDim = 1;
            }

            if (srcImage.isColorImage()) {
                imageBuffer = new float[4 * sliceArea];
                colorFactor = 4;
            } else if (srcImage.isComplexImage()) {
            	imageBuffer = new float[2 * sliceArea];
            	colorFactor = 2;
            } else {
                imageBuffer = new float[sliceArea];
                imageBuffer2 = new float[sliceArea];

                colorFactor = 1;
            }

        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            errorCleanUp("Algorithm Insert Slice reports: Out of memory", true);

            return;
        }
        
        if (volumeType != INSERTED_IMAGE){
            for (t = 0; (t < Tdim +1); t++) {

                for (z = 0; (z < Zdim); z++) {
                    
                    if ( t != insertVolumePos){
                        try {

                        tNewOffset =((Zdim *t) +z);
                        srcImage.exportData(sliceCounter*colorFactor*sliceArea, colorFactor*sliceArea, imageBuffer);
                        destImage.importData((tNewOffset* colorFactor * sliceArea), imageBuffer, false);
                        sliceCounter++;
                        
                        } catch (IOException error) {
                            errorCleanUp("Algorithm InsertVolume3 reports: Destination image already locked.", false);
                            return;
                        }
                        
                    }
                    
                    else{
                        if (volumeType == BLANK){
                            for (int i = 0; i < sliceArea; i++) {
                                imageBuffer[i] = 0.0f;
                            }
    
                            try {
                                tNewOffset =((Zdim *t) +z);
                                destImage.importData((tNewOffset* colorFactor * sliceArea), imageBuffer, false);
                            }catch (IOException error) {
                                errorCleanUp("Algorithm InsertVolume4 reports: Destination image already locked.", false);
                                return;
                            }
                        }
                        
                    }
                    
                   if (srcImage.isDicomImage() != true ){
                        FileInfoBase fileInfoBuffer; // buffer of any old type
                        fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo(z).clone();
                        fileInfoBuffer.setExtents(destImageExtents);
                        fileInfoBuffer.setResolutions(srcImage.getFileInfo(0).getResolutions());
                        destImage.setFileInfo(fileInfoBuffer, (Zdim *t) +z);
                    }
                   
   
                    
                    }
                
                
                }
            
        }
        else if (volumeType == INSERTED_IMAGE){

        if ( (insertedImage.getExtents()[0] == srcImage.getExtents()[0])
                && (insertedImage.getExtents()[1] == srcImage.getExtents()[1])) {
            for (t = 0; (t < Tdim +1); t++) {
                
                if ( t != insertVolumePos){
                
                    for (z = 0; (z < (srcImage.getExtents()[2])); z++) { // for all slices in the old image
                        tNewOffset =((Zdim *t) +z);
    
                       try {
                    	   srcImage.exportData(sliceCounter*colorFactor*sliceArea, colorFactor*sliceArea, imageBuffer);
                           destImage.importData((tNewOffset* colorFactor * sliceArea), imageBuffer, false);
                           sliceCounter++;
                        } catch (IOException error) {
                            errorCleanUp("Algorithm InsertVolume2 reports: Destination image already locked.", false);
    
                            return;
                        }
                        
                        if (srcImage.isDicomImage() != true ){
                            
                            FileInfoBase fileInfoBuffer; // buffer of any old type
                            fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo(z).clone();
                            fileInfoBuffer.setExtents(destImageExtents);
                            fileInfoBuffer.setResolutions(srcImage.getFileInfo(0).getResolutions());
                            destImage.setFileInfo(fileInfoBuffer, tNewOffset);
                        }
                    }
                    sliceCounter2++;
                    
                    }
                
                else{
                
                    for (z = 0; (z < (insertedImage.getExtents()[2])); z++) {
                        tNewOffset =((Zdim *t) +z);
                        
                        try {
                        	insertedImage.exportData(z*colorFactor*sliceArea, colorFactor*sliceArea, imageBuffer);
                            destImage.importData((tNewOffset* colorFactor * sliceArea), imageBuffer, false);
                        } catch (IOException error) {
                            errorCleanUp("Algorithm InsertVolume1 reports: Destination image already locked.", false);
                            return;
                        }
                        
                        if (insertedImage.isDicomImage() != true ){
                            FileInfoBase fileInfoBuffer; // buffer of any old type
                            fileInfoBuffer = (FileInfoBase) insertedImage.getFileInfo(z).clone();
                            fileInfoBuffer.setExtents(destImageExtents);
                            fileInfoBuffer.setResolutions(srcImage.getFileInfo(0).getResolutions());
                            destImage.setFileInfo(fileInfoBuffer, tNewOffset);
                        }
    
                }
                }
    



            }
            }
        
        }
        else {
            MipavUtil.displayError("Inserted Image X&Y Dim must equal Source Image X&Y Dim");
            
            
            
            
            
            
        }
        destImage.calcMinMax();
        
            if (srcImage.getFileInfo()[0] instanceof FileInfoDicom) {
                //4-D Destination dicom images
                    FileInfoBase destFileInfo[] = null;
                    FileInfoDicom oldDicomInfo = null;   
                    int j;
                    destFileInfo = new FileInfoBase[sliceArea];
                    sliceCounter = 0; //Keeps track of every slice to populate tag

               // Creating DICOM tags for 4-D based on srcimage and inserted image dicom tags    
               for (t = 0; t < Tdim+1; t++) {
                   for (z = 0; z < Zdim ; z++) {
                       j = (t*Zdim) + z;
                       if (volumeType == INSERTED_IMAGE && t != insertVolumePos ){                      
                           oldDicomInfo = (FileInfoDicom) insertedImage.getFileInfo(z);  
                       }
                       else if (volumeType != INSERTED_IMAGE && t< insertVolumePos){
                               oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo((t*srcImage.getExtents()[2])+z);  
                           }
                       else if (volumeType != INSERTED_IMAGE && t == insertVolumePos){
                               oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo((t*srcImage.getExtents()[2])+z); 
                           }
                       else if (volumeType != INSERTED_IMAGE && t> insertVolumePos){
                            oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(((t-1)*srcImage.getExtents()[2])+z);  
                           }

                           destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                           oldDicomInfo.getFileFormat());
                           ((FileInfoDicom)destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type());     

                        FileDicomTagTable newTagTable = ((FileInfoDicom) destFileInfo[j]).getTagTable();
                        if (newTagTable.getValue("0018,0088") != null) {
                            String sliceGapString = ((String) ((FileInfoDicom) destFileInfo[j]).getTagTable().getValue("0018,0088")).trim();
                            sliceResolution = new Double(sliceGapString.trim()).doubleValue();
                        }                    
                            //fireProgressStateChanged((((100 * (t*2)))/(destImage.getExtents()[2]+1)));
                            destFileInfo[j].setResolutions(srcImage.getFileInfo(0).getResolutions());
                            destFileInfo[j].setExtents(destImageExtents);
                            destFileInfo[j].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[0], 0);
                            destFileInfo[j].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[1], 1);
                            destFileInfo[j].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[2], 2);
                            destFileInfo[j].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());               
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().importTags((FileInfoDicom) oldDicomInfo);
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0011", new Short((short) Xdim), 2); // columns
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0010", new Short((short) Ydim), 2); // rows                 
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0020,0013", Short.toString((short) (t + 1)),
                                                                     Short.toString((short) (t + 1)).length()); // instance number
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().removeTag("0019,100A");// Removes NumberofImages in Mosaic Tag
                            sliceCounter++;  
                                                     
                   }
               }
               destImage.setFileInfo(destFileInfo);
            }



        //}




        setCompleted(true);

    }

}
