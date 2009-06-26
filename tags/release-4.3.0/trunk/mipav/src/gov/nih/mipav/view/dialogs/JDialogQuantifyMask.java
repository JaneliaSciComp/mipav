package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmQuantifyMask;
import gov.nih.mipav.model.scripting.ParserException;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;


import java.awt.*;
import java.awt.event.*;


/**
 * Simple dialog to change the number of colors in the histogram LUT.
 *
 * @author   Neva Cherniavsky
 * @version  1.0 June 1, 2002
 * @see      ViewJFrameHistoLUT
 */
public class JDialogQuantifyMask 
  extends JDialogScriptableBase implements AlgorithmInterface{

	
	private ModelImage image;
	
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates modal dialog for entering number of colors for histogram LUT.
     *
     * @param  parent  Parent frame.
     */
    public JDialogQuantifyMask(Frame parent, ModelImage im) {
        super(parent, true);
        image = im;
        callAlgorithm();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
   
    /**
     *    Calls the algorithm.
     */
    protected void callAlgorithm() {
    	AlgorithmQuantifyMask algo = new AlgorithmQuantifyMask(image);
    	
    	if (runInSeparateThread) {
            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (algo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        }
        else {
        	algo.run();
        }
    	
    }
    
    public void actionPerformed(ActionEvent e) {
    	
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
       
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
    	image = scriptParameters.retrieveInputImage();

        parentFrame = image.getParentFrame();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
    }
    
}
