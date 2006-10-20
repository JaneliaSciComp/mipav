package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions. User can indicate whether to have algorithm applied to whole image or to the VOI regions. Algorithms are
 * executed in their own thread.
 *
 * @see  gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur
 */
public class JDialogEdgeNMSuppression extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7682705151759726551L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage edgeImage;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** false = apply algorithm only to VOI regions. */
    private boolean image25D = false; // Flag for applying to every slice

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private AlgorithmEdgeNMSuppression nmSupAlgo;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private JPanelSigmas sigmaPanel;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogEdgeNMSuppression() { }

    /**
     * Creates new dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogEdgeNMSuppression(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmEdgeNMSuppression) {
            image.clearMask();

            edgeImage = nmSupAlgo.getZeroXMask();

            if ((nmSupAlgo.isCompleted() == true) && (edgeImage != null)) {

                // updateFileInfo(image, resultImage);
                // resultImage.clearMask();
                // The algorithm has completed and produced a new image to be displayed.
                try {
                    // resultImage.setImageName("EdgeNMSup");
                    // imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610,200), userInterface);

                    edgeImage = nmSupAlgo.getZeroXMask();

                    // edgeImage.setImageName("Edge");
                    new ViewJFrameImage(edgeImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (edgeImage != null) {

                // algorithm failed but result image still has garbage
                edgeImage.disposeLocal(); // clean up memory
                edgeImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        nmSupAlgo.finalize();
        nmSupAlgo = null;
        dispose();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged - method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == image25DCheckbox) {
            sigmaPanel.enable3DComponents(!image25DCheckbox.isSelected());
        }
    }

    /**
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Once all the necessary variables are set, call the algorithm based on what type of image this is and whether or
     * not there is a separate destination image.
     */
    protected void callAlgorithm() {
        System.gc();

        String name = makeImageName(image.getImageName(), "_edgeNM");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            try {

                // Make result image of float type
                ModelImage resultImage = new ModelImage(ModelImage.FLOAT, destExtents, " EdgeNMSup", userInterface);
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ",
                                                                            26); // Secondary Capture SOP UID
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ",
                                                                            26);
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                }

                // Make algorithm
                nmSupAlgo = new AlgorithmEdgeNMSuppression(resultImage, image, sigmas,
                                                           outputPanel.isProcessWholeImageSet(), image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                nmSupAlgo.addListener(this);
                createProgressBar(image.getImageName(), nmSupAlgo);

                // Hide dialog
                setVisible(false);

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (nmSupAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog EdgeNMSup: unable to allocate enough memory");

                if (nmSupAlgo.getZeroXMask() != null) {
                    nmSupAlgo.getZeroXMask().disposeLocal(); // Clean up memory of result image
                }

                return;
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            try {

                // Make result image of float type
                ModelImage resultImage = new ModelImage(ModelImage.FLOAT, destExtents, "EdgeNMSup", userInterface);
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0002",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0008,0016",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26);
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0013", "MIPAV--NIH", 10); //
                    }
                }

                // Make algorithm
                nmSupAlgo = new AlgorithmEdgeNMSuppression(resultImage, image, sigmas,
                                                           outputPanel.isProcessWholeImageSet(), image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                nmSupAlgo.addListener(this);

                createProgressBar(image.getImageName(), nmSupAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (nmSupAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    nmSupAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog EdgeNMSup: unable to allocate enough memory");

                if (nmSupAlgo.getZeroXMask() != null) {
                    nmSupAlgo.getZeroXMask().disposeLocal(); // Clean up image memory
                }

                return;
            }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(edgeImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        sigmaPanel = new JPanelSigmas(image);
        scriptParameters.setSigmasGUI(sigmaPanel);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        image25D = scriptParameters.doProcess3DAs25D();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(edgeImage, outputPanel.isOutputNewImageSet());

        scriptParameters.storeProcessWholeImage(outputPanel.isProcessWholeImageSet());

        scriptParameters.storeSigmas(sigmaPanel);

        scriptParameters.storeProcess3DAs25D(image25D);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("EdgeNMSuppression");

        sigmaPanel = new JPanelSigmas(image);

        image25DCheckbox = WidgetFactory.buildCheckBox("Process each slice independently (2.5D)", false, this);

        if (image.getNDims() != 3) { // if the source image is 3D then allow
            image25DCheckbox.setEnabled(false);
        }

        PanelManager optionsPanelManager = new PanelManager("Options");
        optionsPanelManager.add(image25DCheckbox);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        outputPanel.setOutputImageOptionsEnabled(false);
        outputPanel.setOutputNewImage(true);

        PanelManager mainPanelManager = new PanelManager();
        mainPanelManager.add(sigmaPanel);
        mainPanelManager.addOnNextLine(optionsPanelManager.getPanel());
        mainPanelManager.addOnNextLine(outputPanel);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        }

        if (!sigmaPanel.testSigmaValues()) {
            return false;
        }

        return true;
    }

}
