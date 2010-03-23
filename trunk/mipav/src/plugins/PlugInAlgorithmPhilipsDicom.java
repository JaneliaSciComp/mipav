import java.text.DecimalFormat;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;


/**
 * Rescales philips dicom images according to the private tag 2005,100E while doing the normal
 * rescaling indicated by tags 0028,1052 and 0028,1053
 * 
 * @author senseneyj
 *
 */

public class PlugInAlgorithmPhilipsDicom extends AlgorithmBase {

	/** X dimension of the MRI image */
    private int xDim;

    /** Y dimension of the MRI image */
    private int yDim;
    
    /** Optional Z dimension of the MRI image */
    private int zDim;

    /** Optional T dimension of the MRI image */
    private int tDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;
    
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
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        if(srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }
        
        if(srcImage.getNDims() > 3) {
            tDim = srcImage.getExtents()[3];
        } else {
            tDim = 1;
        }
        
        destImage = (ModelImage)srcImage.clone(srcImage.getImageName()+"_rescale");
        
        //Already tested to cast correctly in setVariables
        FileInfoDicom fileInfo = (FileInfoDicom)srcImage.getFileInfo()[0];
    	
        Float scaleSlopeTag = (Float)fileInfo.getTagTable().getValue("2005,100E");
    	String rescaleInterceptTag = (String)fileInfo.getTagTable().getValue("0028,1052");
    	String rescaleSlopeTag = (String)fileInfo.getTagTable().getValue("0028,1053");
    	
    	ViewUserInterface.getReference().getMessageFrame().append("Tag values\n2005,100E:\t"+scaleSlopeTag+"\n"+
    	                                                            "0028,1052:\t"+rescaleInterceptTag+"\n"+
    	                                                            "0028,1053:\t"+rescaleSlopeTag+"\n", ViewJFrameMessage.DATA);
        
    	double scaleSlope = Double.valueOf(scaleSlopeTag);
    	double rescaleIntercept = Double.valueOf(rescaleInterceptTag);
    	double rescaleSlope = Double.valueOf(rescaleSlopeTag);
    	
    	DecimalFormat dec = new DecimalFormat("0.######");
		String appMessage = "Numerator:\timageData*"+dec.format(rescaleSlope)+"+"+rescaleIntercept+
							"\nDenominator:\t"+dec.format(rescaleSlope)+"*"+dec.format(scaleSlope)+"\n";
	
		ViewUserInterface.getReference().getMessageFrame().append(appMessage, ViewJFrameMessage.DATA);
		
		
		double pix = 0.0, srcpix = 0.0;
		final int srcDim = srcImage.getNDims();
		final int destDim  = srcImage.getNDims();
		
		for(int t=0; t<tDim; t++) {
    		for(int k=0; k<zDim; k++) {
        		for(int j=0; j<yDim; j++) {
        		    for(int i=0; i<xDim; i++) {
        		        switch(srcDim) {
                        case 2:
                            srcpix = srcImage.get(i, j).doubleValue();
                            break;
                        
                        case 3:
                            srcpix = srcImage.get(i, j, k).doubleValue();
                            break;
           
                        case 4:
                            srcpix = srcImage.get(i, j, k, t).doubleValue();
                            break;
                        }
        		        
        			    pix = ((srcpix*rescaleSlope) + rescaleIntercept) / 
        			                    (rescaleSlope*scaleSlope);
        			    switch(destDim) {
        			    case 2:
        			        destImage.set(i, j, pix);
        			        break;
        			    
            		    case 3:
                            destImage.set(i, j, k, pix);
                            break;
           
                		case 4:
                            destImage.set(i, j, k, t, pix);
                            break;
        			    }
        			}
        		}
    		}
		}
		
		fireProgressStateChanged(70);
		
		FileDicomTagTable destTable = ((FileInfoDicom)destImage.getFileInfo()[0]).getTagTable();
		destTable.setValue("2005,100E", new Float(1));
		
		ViewUserInterface.getReference().getMessageFrame().append("Rescale slope tag (2005,100E) set to 1\n", ViewJFrameMessage.DATA);
		
		destImage.calcMinMax();
		fireProgressStateChanged(85);
		

        ViewJFrameImage resultFrame = new ViewJFrameImage(destImage);
        resultFrame.setTitle(destImage.getImageName());
        resultFrame.setVisible(true);
		
		setCompleted(true);
    }
}
