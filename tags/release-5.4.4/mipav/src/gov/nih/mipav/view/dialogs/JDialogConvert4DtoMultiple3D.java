package gov.nih.mipav.view.dialogs;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.EnumSet;
import java.util.Set;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.ActionMetadata.ImageRequirements;

public class JDialogConvert4DtoMultiple3D extends JDialogScriptableBase implements AlgorithmInterface {

	
	  /** DOCUMENT ME! */
    private ModelImage image = null; // source image


    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
	
	
	 /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogConvert4DtoMultiple3D() {}

    /**
     * Creates new dialog, but dialog is not visible.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogConvert4DtoMultiple3D(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
    }
	
	
	

	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub

	}
	
	
	
	
	
	public void callAlgorithm() {
		int[] destExtents = new int[3];
        destExtents[0] = image.getExtents()[0];
        destExtents[1] = image.getExtents()[1];
        destExtents[2] = image.getExtents()[2];
        AlgorithmSubset subsetAlgo;
        int tLength = image.getExtents()[3];
        for(int i=0;i<tLength;i++) {
        	String resultString = image.getImageName() + "_T" + i;
        	ModelImage resultImage = new ModelImage(image.getType(), destExtents, resultString);
        	subsetAlgo = new AlgorithmSubset(image, resultImage, AlgorithmSubset.REMOVE_T, i);
        	subsetAlgo.run();
        	new ViewJFrameImage(resultImage);
        }
        dispose();

	}


	protected void setGUIFromParams() {
		image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

	}

	
	protected void storeParamsFromGUI() throws ParserException {
		 scriptParameters.storeInputImage(image);

	}

	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}




}
