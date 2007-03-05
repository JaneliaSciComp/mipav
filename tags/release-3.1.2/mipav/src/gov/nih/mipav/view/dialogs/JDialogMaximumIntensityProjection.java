package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;



/**
 * Dialog to call Maximum Intensity Projection. This dialog will not be visible because it does not require 
 * user input at this time. It was made a dialog object because it may, in the future require user input and
 * to be consistent with the dialog/algorithm paradigm. It should be noted that algorithms are executed in 
 * own thread. 
 * 
 * @author joshim2
 * 
 */
public class JDialogMaximumIntensityProjection extends JDialogScriptableBase implements AlgorithmInterface {
	
	// Instance Fields -------------------------------------------------------------------------------------
	private AlgorithmMaximumIntensityProjection mipAlgo;
	
	/** Source Image */
	private ModelImage image = null; 
	
	private ViewUserInterface userInterface;
	
	
	

	// Constructors ----------------------------------------------------------------------------------------
	
	/**
	 * Empty contructor needed for dynamic instantiation (used during scripting).
	 */
	
	public JDialogMaximumIntensityProjection () { }
	
	/**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is not
     * necessary at present.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMaximumIntensityProjection(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
    }
    
    // Methods -----------------------------------------------------------------------------------------------
    
    /**
     * Does nothing.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {}
    
//  ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmMaximumIntensityProjection) {
        	
        	ArrayList resultImages = mipAlgo.getResultImage();
        	ModelImage XresultImage = (ModelImage) resultImages.get(0);
        	ModelImage YresultImage = (ModelImage) resultImages.get(1);
        	ModelImage ZresultImage = (ModelImage) resultImages.get(2);
        	
        	/* Display the result */
        	
        	try {
        		new ViewJFrameImage(XresultImage, null, new Dimension(610, 200));
        	} catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
        	
        	try {
        		new ViewJFrameImage(YresultImage, null, new Dimension(610, 200));
        	} catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
        	
        	try {
        		new ViewJFrameImage(ZresultImage, null, new Dimension(610, 200));
        	} catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            insertScriptLine();
            
        }

        mipAlgo.finalize();
        mipAlgo = null;

    }
    
    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            mipAlgo = new AlgorithmMaximumIntensityProjection(image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            mipAlgo.addListener(this);

            
            createProgressBar(image.getImageName(), mipAlgo);
            
            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (mipAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
            	mipAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog VOI Extraction: unable to allocate enough memory");

            return;
        }
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
    }
	
}
