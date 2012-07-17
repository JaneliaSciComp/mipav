package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;

public abstract class AlgorithmConcatMult extends AlgorithmBase {

    /** array of images **/
    protected ModelImage[] images;
    
    /** final image **/
    protected ModelImage destImage;

    /** x and y dimensions of image */
    protected int xDim, yDim;

    /** Whether to copy all file information */
    protected boolean copyAllInfo = false;
    
    protected void copyBaseInfo(FileInfoBase[] fileInfo, FileInfoBase srcFileInfo, float[] resols, int i) {
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
    protected void copyDicomInfo(FileInfoBase[] destFileInfo, float[] resols, int z, int t, int sliceCounter) {
        FileInfoDicom oldDicomInfo = (FileInfoDicom) images[t].getFileInfo(z);
        if (z == 0) {
            destFileInfo[sliceCounter] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                            oldDicomInfo.getFileFormat());
            ((FileInfoDicom)destFileInfo[sliceCounter]).setVr_type(oldDicomInfo.getVr_type());     
        }
        else {
            destFileInfo[sliceCounter] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                            oldDicomInfo.getFileFormat(), (FileInfoDicom) destFileInfo[t*(destImage.getExtents()[2])]);
            
            ((FileInfoDicom)destFileInfo[sliceCounter]).setVr_type(oldDicomInfo.getVr_type()); 

           
        }
         
        double sliceResolution = 0.0;
        
         FileDicomTagTable newTagTable = ((FileInfoDicom) destFileInfo[sliceCounter]).getTagTable();
         if (newTagTable.getValue("0018,0088") != null) {
             String sliceGapString = ((String) ((FileInfoDicom) destFileInfo[sliceCounter]).getTagTable().getValue("0018,0088")).trim();
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
         ((FileInfoDicom) destFileInfo[sliceCounter]).getTagTable().importTags((FileInfoDicom) images[t].getFileInfo(z));
         ((FileInfoDicom) destFileInfo[sliceCounter]).getTagTable().setValue("0028,0011", new Short((short) xDim), 2); // columns
         ((FileInfoDicom) destFileInfo[sliceCounter]).getTagTable().setValue("0028,0010", new Short((short) yDim), 2); // rows                 
         ((FileInfoDicom) destFileInfo[sliceCounter]).getTagTable().setValue("0020,0013", Short.toString((short) (t + 1)),
                                                  Short.toString((short) (t + 1)).length()); // instance number
         ((FileInfoDicom) destFileInfo[sliceCounter]).getTagTable().removeTag("0019,100A");// Removes NumberofImages in Mosaic Tag
    }

}
