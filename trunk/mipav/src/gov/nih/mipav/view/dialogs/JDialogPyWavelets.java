package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 *
 */
public class JDialogPyWavelets extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID =;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private PyWavelets waveletAlgo;

    /** Source image. */
    private ModelImage image;

    /** Result image. */
    private ModelImage resultImage = null;
    
    private int tType;
    
    private PyWavelets.WAVELET_NAME names[];
    
    private int orders[];
    
    private PyWavelets.MODE modes[];
    
    private int axes[];
    
    private int filterType[];
    
    private double filterVal1[];
    
    private double filterVal2[];
    
    private boolean showTransform;
    
    private boolean showFilteredTransform;
    
    private int levels;
    
    private int start_level;

    /** DOCUMENT ME! */
    private JPanel paramsPanel;
    
    
    private JLabel mainLabel;
    
    private JLabel mainLabel2;
    
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogPyWavelets() { }

    /**
     * Construct the PyWavelets dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogPyWavelets(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        //loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("PyWavelets");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PyWavelets) {
            Preferences.debug("PyWavelets: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((waveletAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                if (resultImage.isColorImage()) {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // if (algorithm instanceof AlgorithmBarrelDistortion)

        

        if (waveletAlgo != null) {
            waveletAlgo.finalize();
            waveletAlgo = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    
    /**
     * Once all the necessary variables are set, call the Barrel/Pincushion Distortion
     * Correction algorithm based on what type of image this is
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_filter");
        
        try {

            // Make result image
        	resultImage = new ModelImage(ModelImage.DOUBLE, image.getExtents(), name);

            // Make algorithm
            waveletAlgo = new PyWavelets(resultImage, image, tType, names, orders, modes, axes, filterType, filterVal1, filterVal2,
            		showTransform, showFilteredTransform, levels, start_level);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            waveletAlgo.addListener(this);

            createProgressBar(image.getImageName(), waveletAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (waveletAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                waveletAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("PyWavelets: unable to allocate enough memory");

            return;
        }
            
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        
        
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);

        
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("PyWavelets");
        

        System.gc();
    }
    
    private JPanel buildWaveletPanel() {
    	 final JPanel waveletPanel = new JPanel();
    	 return waveletPanel;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        
        return true;
    }
}
