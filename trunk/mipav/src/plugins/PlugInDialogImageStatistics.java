import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import javax.swing.*;

/**
 * @version  July 11, 2007
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 */

public class PlugInDialogImageStatistics extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private ModelImage sourceImage; // source image
    
    /** DOCUMENT ME! */
    private PlugInAlgorithmImageStatistics imageStatsAlgo = null;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogImageStatistics() { }

    /**
     * Creates new dialog for computing image statistics using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogImageStatistics(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        sourceImage = im;
        parentFrame = theParentFrame;
        init();
    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            //if (setVariables()) {
                callAlgorithm();
            //}
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }
    } // end actionPerformed(...)

    
    
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

        if (algorithm instanceof PlugInAlgorithmImageStatistics) {
            Preferences.debug("Image Statistics Elapsed: " + algorithm.getElapsedTime());
            sourceImage.clearMask();
            
            if ((imageStatsAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(sourceImage, resultImage);

                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage);
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

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }

            dispose();
        }

    } // end AlgorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            String name = makeImageName(sourceImage.getImageName(), "_MuscleFat Image");
            resultImage = (ModelImage) sourceImage.clone();
            resultImage.setImageName(name);
            imageStatsAlgo = new PlugInAlgorithmImageStatistics(resultImage, sourceImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            imageStatsAlgo.addListener(this);
//           createProgressBar(sourceImage.getImageName(), " ...", imageStatsAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (imageStatsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
            	imageStatsAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Image Statistics: unable to allocate enough memory");

            return;
        }
    } // end callAlgorithm()


    protected void setGUIFromParams() {
    // TODO Auto-generated method stub, no params yet
    }

    protected void storeParamsFromGUI() throws ParserException {
    // TODO Auto-generated method stub, no params yet
    }

    
    private void init() {
        setForeground(Color.black);
        setTitle("Image Statistics");

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
    } // end init()

} // end class PlugInDialogImageStatistics
