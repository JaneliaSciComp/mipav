import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * JDialogBase class.
 *
 * @version  March 29, 2004
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogFISHAnalysis.java $ $Revision: 2 $ $Date: 1/25/06 4:59p $</p>
 */
public class PlugInDialogFISHAnalysis extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5344141440812459432L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private PlugInAlgorithmFISHAnalysis regionDistAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor required for dynamic instantiation during script execution.
     */
    public PlugInDialogFISHAnalysis() { }

    /**
     * Creates new dialog for region distances within a cell using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogFISHAnalysis(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (!im.isColorImage()) {
            MipavUtil.displayError("Source Image must be Color");
            dispose();

            return;
        }

        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
            callAlgorithm();
            /*
             * if (setVariables()) { callAlgorithm(); }
             */
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
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

        if (algorithm instanceof PlugInAlgorithmFISHAnalysis) {
            image.clearMask();

            if (regionDistAlgo.isCompleted() == true) {
                insertScriptLine();
            }

            dispose();
        }
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {
            regionDistAlgo = new PlugInAlgorithmFISHAnalysis(image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            regionDistAlgo.addListener(this);

            createProgressBar(image.getImageName(), " ...", regionDistAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (regionDistAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                regionDistAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("FISHAnalysis: unable to allocate enough memory");

            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (!image.isColorImage()) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "Source Image must be Color");
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("FISHAnalysis");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Input parameters"));

        JLabel labelVOI = new JLabel("Shall I find the FISH VOIs for you?");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        mainPanel.add(labelVOI, gbc);

        if (image.getNDims() > 2) {
            JLabel labelSlices = new JLabel("Propagate VOIs to desired slices");
            labelSlices.setForeground(Color.black);
            labelSlices.setFont(serif12);
            gbc.gridy = 1;
            mainPanel.add(labelSlices, gbc);
        } // if (image.getNDims() > 2)

        getContentPane().add(mainPanel, BorderLayout.CENTER);

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
    }
}
