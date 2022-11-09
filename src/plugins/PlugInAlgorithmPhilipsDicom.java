
import java.text.DecimalFormat;

import gov.nih.mipav.model.algorithms.AlgorithmBase;

import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;

import gov.nih.mipav.view.ViewUserInterface;


/**
 * Rescales philips dicom images according to the private tag 2005,100E while doing the normal
 * rescaling indicated by tags 0028,1052 and 0028,1053
 * 
 * @author senseneyj
 *
 */

public class PlugInAlgorithmPhilipsDicom extends AlgorithmBase {

    /** Slice size for xDim*yDim */
    //private int sliceSize;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmPhilipsDicom(ModelImage resultImage, ModelImage srcImg) {
        super(resultImage, srcImg);
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

    	fireProgressStateChanged("Processing data...");
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        calcRescale();
    } // end runAlgorithm()
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    private void calcRescale() {
        
        FileInfoImageXML fileInfoImage = new FileInfoImageXML(srcImage.getImageName()+"_rescale", null, FileUtility.RAW);
        fileInfoImage.setDataType(DataType.DOUBLE.getLegacyNum());
        fileInfoImage.setExtents(srcImage.getFileInfo()[0].getExtents());
        fileInfoImage.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfoImage.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfoImage.setEndianess(srcImage.getFileInfo()[0].getEndianess());
        fileInfoImage.setOffset(srcImage.getFileInfo()[0].getOffset());
        fileInfoImage.setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        destImage = ViewUserInterface.getReference().createBlankImage(fileInfoImage);
        destImage.getParentFrame().setVisible(false);
        
        double scaleSlopeTag = ((Number) ((FileInfoDicom)srcImage.getFileInfo()[0]).getTagTable().getValue("2005,100E", false)).doubleValue();
        double rescaleInterceptTag = Double.valueOf(((FileInfoDicom)srcImage.getFileInfo()[0]).getTagTable().getValue("0028,1052").toString());
        double rescaleSlopeTag =  Double.valueOf(((FileInfoDicom)srcImage.getFileInfo()[0]).getTagTable().getValue("0028,1053").toString());
        
        Preferences.data("Tag values\n2005,100E:\t"+scaleSlopeTag+"\n"+
                            "0028,1052:\t"+rescaleInterceptTag+"\n"+
                            "0028,1053:\t"+rescaleSlopeTag+"\n");
        
        fireProgressStateChanged(70);
        
        for(int i=0; i<srcImage.getFileInfo().length; i++) {
            FileInfoDicom fileInfo = ((FileInfoDicom)srcImage.getFileInfo(i));

            fileInfo.getTagTable().setValue("0028,1052", (rescaleInterceptTag / (scaleSlopeTag*rescaleSlopeTag)) / 10.0);
            fileInfo.getTagTable().setValue("0028,1053", (rescaleSlopeTag / (scaleSlopeTag*rescaleSlopeTag)) / 10.0);
            fileInfo.getTagTable().setValue("2005,100E", 1);
            fileInfo.setRescaleIntercept((rescaleInterceptTag / (scaleSlopeTag*rescaleSlopeTag)) / 10.0);
            fileInfo.setRescaleSlope((rescaleSlopeTag / (scaleSlopeTag*rescaleSlopeTag)) / 10.0);
            destImage.setFileInfo(fileInfo, i);
            srcImage.getFileInfo()[i] = null;
        }
        
        Preferences.data("Philips scale slope tag (2005,100E) set to 1\nNew rescale slope tag value: "+(rescaleSlopeTag / (scaleSlopeTag*rescaleSlopeTag)) / 10.0);
        
        double newRescaleInterceptTag = Double.valueOf(((FileInfoDicom)destImage.getFileInfo()[0]).getTagTable().getValue("0028,1052").toString());
        double newRescaleSlopeTag =  Double.valueOf(((FileInfoDicom)destImage.getFileInfo()[0]).getTagTable().getValue("0028,1053").toString());
                
        double x = 0;
        for(int i=0; i<srcImage.getDataSize(); i++) {
            if(i%1000 == 0) {
                fireProgressStateChanged((int)(i/1200.0));
            }
            x = srcImage.getDouble(i);
            if(x != 0) {
                //System.out.print("At y:"+i%srcImage.getExtents()[0]+"x: "+i/srcImage.getExtents()[0]+" the value: "+x+" is changed to ");
                
                x = x - rescaleInterceptTag;
                x = (int)(x/rescaleSlopeTag);
                x = x*newRescaleSlopeTag;
                x = x+newRescaleInterceptTag;
                //System.out.println(x);
            }
            destImage.set(i, x);
        }
        
        destImage.calcMinMax();
        destImage.setImageName(srcImage.getImageName()+"_rescaled", true);
        ViewUserInterface.getReference().registerImage(destImage);

        srcImage.getParentFrame().setVisible(false);
        srcImage.getParentFrame().close();
        ViewUserInterface.getReference().unRegisterImage(srcImage);
        srcImage.disposeLocal();
        
        fireProgressStateChanged(70);     
		
		setCompleted(true);
    }
}
