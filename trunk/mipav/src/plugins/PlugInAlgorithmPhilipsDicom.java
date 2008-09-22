import java.text.DecimalFormat;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;



public class PlugInAlgorithmPhilipsDicom extends AlgorithmBase {

	/** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

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

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            calc3D();
        }
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
    
    private void calc2D() {
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        //Already tested to cast correctly in setVariables
        FileInfoDicom fileInfo = (FileInfoDicom)srcImage.getFileInfo()[0];
    	
        Float tag1 = (Float)fileInfo.getTagTable().getValue("2005,100E");
    	String tag2 = (String)fileInfo.getTagTable().getValue("0028,1052");
    	String tag3 = (String)fileInfo.getTagTable().getValue("0028,1053");
    	
    	double double100E = Double.valueOf(tag1);
    	double double1052 = Double.valueOf(tag2);
    	double double1053 = Double.valueOf(tag3);
    	
    	double intercept = (double1052 / (double100E * double1053)) / 1000;
    	double slope = (double1053 / (double100E * double1053)) / 1000;
    	
    	DecimalFormat dec = new DecimalFormat("0.######");
		String appMessage = "Slope:\t"+dec.format(slope)+
							"\nIntercept:\t"+dec.format(intercept)+"\n";
	
		ViewUserInterface.getReference().getMessageFrame().append(appMessage, ViewJFrameMessage.DATA);
		
		AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(destImage, 7, 0, 65536, 0, 65536, false);
		changeTypeAlgo.run();
		
		for(int i=0; i<xDim; i++) 
			for(int j=0; j<yDim; j++) 
				destImage.set(i, j, srcImage.get(i, j).doubleValue()*slope + intercept);
		
		fireProgressStateChanged(70);
		
		FileDicomTagTable destTable = ((FileInfoDicom)destImage.getFileInfo()[0]).getTagTable();
		destTable.setValue("2005,100E", new Float(1));
		destTable.setValue("0028,1052", dec.format(intercept));
		destTable.setValue("0028,1053", dec.format(slope));
		
		destImage.calcMinMax();
		fireProgressStateChanged(85);
		
		setCompleted(true);
    }
    
    private void calc3D() {

    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        int zDim = srcImage.getExtents()[2];
        
        //Already tested to cast correctly in setVariables
        FileInfoDicom fileInfo = (FileInfoDicom)srcImage.getFileInfo()[0];
    	
    	Float tag1 = (Float)fileInfo.getTagTable().getValue("2005,100E");
    	String tag2 = (String)fileInfo.getTagTable().getValue("0028,1052");
    	String tag3 = (String)fileInfo.getTagTable().getValue("0028,1053");
    	
    	double double100E = Double.valueOf(tag1);
    	double double1052 = Double.valueOf(tag2);
    	double double1053 = Double.valueOf(tag3);
    	
    	double intercept = (double1052 / (double100E * double1053)) / 1000;
    	double slope = (double1053 / (double100E * double1053)) / 1000;
    	
    	DecimalFormat dec = new DecimalFormat("0.######");
    	String appMessage = "Slope:\t"+dec.format(slope)+
							"\nIntercept:\t"+dec.format(intercept)+"\n";
		ViewUserInterface.getReference().getMessageFrame().append(appMessage, ViewJFrameMessage.DATA);
		
		AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(destImage, 7, 0, 65536, 0, 65536, false);
		changeTypeAlgo.run();
		
		for(int k=0; k<zDim; k++) {
			for(int i=0; i<xDim; i++) {
				for(int j=0; j<yDim; j++) {
					destImage.set(i, j, k, srcImage.get(i, j, k).doubleValue()*slope + intercept);				
				}
			}
			fireProgressStateChanged((k/zDim)*70);
		}

		for(int k=0; k<zDim; k++) {
			FileDicomTagTable destTable = ((FileInfoDicom)destImage.getFileInfo()[k]).getTagTable();
			destTable.setValue("2005,100E", new Float(1));
			destTable.setValue("0028,1052", dec.format(intercept));
			destTable.setValue("0028,1053", dec.format(slope));
			fireProgressStateChanged(70 + (k/zDim)*10);
		}
		
		destImage.calcMinMax();
		fireProgressStateChanged(85);

		setCompleted(true);
    }
}
