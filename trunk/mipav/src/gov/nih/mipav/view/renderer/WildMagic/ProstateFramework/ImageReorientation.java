package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;


public class ImageReorientation implements AlgorithmInterface {

    private     AlgorithmRotate         algoRotate = null;
    private     ModelImage              image;
    private     ModelImage              resultImage;

    public static final int AXIAL_INDEX = 0;
    
    public static final int CORONAL_INDEX = 1;
    
    public static final int SAGITTAL_INDEX = 2;
    
    public static final int USER_INDEX = 3;
    
	// storage for header information
	FileInfoBase 	fileInfo;
	FileInfoNIFTI   fileInfoNIFTI;
	
	private     int[]       or = new int[3];
	private     int[]       newOr = new int[3];
    
	public ImageReorientation(ModelImage im, int orientationIndex) {
		image = im;
		
		switch (orientationIndex) {
		case AXIAL_INDEX:
			set_axial_orientation();
			break;
		case CORONAL_INDEX:
			set_coronal_orientation();
			break;
		case SAGITTAL_INDEX:
			set_sagittal_orientation();
			break;
		case USER_INDEX:
		default:
			;
		}
		
		setVariables();
	}
	
	public void preformOrientation() {
         int[] axisOrder = { 0, 1, 2, 3 };
         boolean[] axisFlip = { false, false, false, false };
         if ( MipavCoordinateSystems.matchOrientation(newOr, or, axisOrder, axisFlip ) )
         {
             algoRotate = new AlgorithmRotate(image, axisOrder, axisFlip );
             algoRotate.addListener(this);
             algoRotate.run();
         }	
	}
    
    /** 
    *	This method is required if the AlgorithmPerformed interface is implemented. 
    *   It is called by the algorithm when it has completed or failed to to complete, 
    *   so that the dialog can be display the result image and/or clean up.
    *   @param algorithm   Algorithm that caused the event.
    */
    public void algorithmPerformed(AlgorithmBase algorithm) {
                
		
        if ( algorithm instanceof AlgorithmRotate) {
            resultImage = algoRotate.returnImage();
			if (algorithm.isCompleted() == true && resultImage != null) {
                // The algorithm has completed and produced a new image to be displayed.
				if (image.getFileInfo()[0].getFileFormat() == FileUtility.NIFTI) {
					fileInfoNIFTI.setMin(resultImage.getMin());
					fileInfoNIFTI.setMax(resultImage.getMax());
                    if (resultImage.getNDims() == 3) {
                    	for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                    		resultImage.setFileInfo((FileInfoNIFTI)fileInfoNIFTI.clone(),i);
                    	}
                    }
                    else if (resultImage.getNDims() == 4) {
                    	for (int i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
                    		resultImage.setFileInfo((FileInfoNIFTI)fileInfoNIFTI.clone(),i);
                    	}	
                    }
				}
				
                resultImage.clearMask();
				resultImage.calcMinMax();

            } else if (resultImage != null) {
                //algorithm failed but result image still has garbage
                resultImage = null;
                //System.gc();
            }
       }
       algorithm.finalize();
       algorithm = null;
    }  // end AlgorithmPerformed()
	
    protected void set_axial_orientation() {
        newOr[0] = FileInfoBase.ORI_R2L_TYPE;
        newOr[1] = FileInfoBase.ORI_A2P_TYPE;
        newOr[2] = FileInfoBase.ORI_I2S_TYPE;
    }
    
    protected void set_sagittal_orientation() {
    	newOr[0] = FileInfoBase.ORI_A2P_TYPE;
        newOr[1] = FileInfoBase.ORI_S2I_TYPE;
        newOr[2] = FileInfoBase.ORI_R2L_TYPE;
    }
    
    protected void set_coronal_orientation() {
    	newOr[0] = FileInfoBase.ORI_R2L_TYPE;
        newOr[1] = FileInfoBase.ORI_S2I_TYPE;
        newOr[2] = FileInfoBase.ORI_A2P_TYPE;
    }
    
    /**
    *	Use the GUI results to set up the variables needed to run the algorithm.
    *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
    */
    private boolean setVariables() {
        int rl, is, ap;
        int i;
        
        for (i = 0; i <=2; i++) {
            or[i] = image.getFileInfo()[0].getAxisOrientation()[i];   
        }
        
        rl = 0;
        ap = 0;
        is = 0;
        for (i = 0; i <= 2; i++) {
            if ((or[i] == FileInfoBase.ORI_L2R_TYPE) || (or[i] == FileInfoBase.ORI_R2L_TYPE)) {
                rl++;
            }
            else if ((or[i] == FileInfoBase.ORI_A2P_TYPE) || (or[i] == FileInfoBase.ORI_P2A_TYPE)) {
                ap++;
            }
            else if ((or[i] == FileInfoBase.ORI_I2S_TYPE) || (or[i] == FileInfoBase.ORI_S2I_TYPE)) {
                is++;
            }
        }
        if ((rl != 1) || (ap != 1) || (is != 1)) {
            MipavUtil.displayError("Error! Present orientation must have one RL, one AP, and one IS axis");
            return false;
        }
        
        rl = 0;
        ap = 0;
        is = 0;
        for (i = 0; i <= 2; i++) {
            if ((newOr[i] == FileInfoBase.ORI_L2R_TYPE) || (newOr[i] == FileInfoBase.ORI_R2L_TYPE)) {
                rl++;
            }
            else if ((newOr[i] == FileInfoBase.ORI_A2P_TYPE) || (newOr[i] == FileInfoBase.ORI_P2A_TYPE)) {
                ap++;
            }
            else if ((newOr[i] == FileInfoBase.ORI_I2S_TYPE) || (newOr[i] == FileInfoBase.ORI_S2I_TYPE)) {
                is++;
            }
        }
        if ((rl != 1) || (ap != 1) || (is != 1)) {
            MipavUtil.displayError("Error! New orientation must have one RL, one AP, and one IS axis");
            return false;
        }
		
		//System.out.println(getParameterString("|"));
        
    	return true;  	
    }   // end setVariables()
    
   
    public ModelImage getResultImage() {
    	return resultImage;
    }
}
