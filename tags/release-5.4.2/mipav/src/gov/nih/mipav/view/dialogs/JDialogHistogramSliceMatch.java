package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input reference slice.  All other slices transformed to match histogram of
 * reference slice. Algorithms are executed in their own thread.
 */
public class JDialogHistogramSliceMatch extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private AlgorithmHistogramSliceMatch matchHistogramSliceAlgo = null;
    
    private int referenceSlice;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** or if the source image is to be replaced. */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JTextField textSlice;
    
    private int nSlices;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogHistogramSliceMatch() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogHistogramSliceMatch(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = im;
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
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
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

        if (algorithm instanceof AlgorithmHistogramSliceMatch) {
            srcImage.clearMask();

            if ((matchHistogramSliceAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(srcImage, resultImage);
                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = srcImage.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));

                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                srcImage.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        matchHistogramSliceAlgo.finalize();
        matchHistogramSliceAlgo = null;
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
    
    /**
     * Accessor which lets you specify the reference slice.
     *
     * @param  referenceSlice reference slice number
     */
    public void setReferenceSlice(int referenceSlice) {
        this.referenceSlice = referenceSlice;
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
        String name = makeImageName(srcImage.getImageName(), "_histSliceMatch");

        if (displayLoc == NEW) {

            try {
                resultImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), name);

                // Make algorithm
                matchHistogramSliceAlgo = new AlgorithmHistogramSliceMatch(resultImage, srcImage, referenceSlice);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                matchHistogramSliceAlgo.addListener(this);

                createProgressBar(srcImage.getImageName(), matchHistogramSliceAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (matchHistogramSliceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    matchHistogramSliceAlgo.run();
                }
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                System.gc();
                MipavUtil.displayError("Dialog Histogram slice match: unable to allocate enough memory");

                return;
            }
        } else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                matchHistogramSliceAlgo = new AlgorithmHistogramSliceMatch(srcImage, referenceSlice);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                matchHistogramSliceAlgo.addListener(this);

                createProgressBar(srcImage.getImageName(), matchHistogramSliceAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the match image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = srcImage.getImageFrameVector();
                titles = new String[imageFrames.size()];
                userInterface = ViewUserInterface.getReference();

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (matchHistogramSliceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    matchHistogramSliceAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                System.gc();
                MipavUtil.displayError("Dialog Histogram Slice match: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        srcImage = scriptParameters.retrieveImage("src_image");
        userInterface = ViewUserInterface.getReference();
        parentFrame = srcImage.getParentFrame();
        nSlices = srcImage.getExtents()[2];

        setReferenceSlice(scriptParameters.getParams().getInt("reference_slice"));
        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeImage(srcImage, "src_image");
        scriptParameters.getParams().put(ParameterFactory.newParameter("reference_slice", referenceSlice));
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
    }

    

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Histogram slice matching");
        
        nSlices = srcImage.getExtents()[2];
        JLabel sliceLabel = new JLabel("Reference slice (0-" + String.valueOf(nSlices-1) + ")");
        sliceLabel.setFont(serif12);
        sliceLabel.setForeground(Color.black);
        
        textSlice = new JTextField(5);
        textSlice.setText("");
        textSlice.setFont(serif12);
        textSlice.setEnabled(true);
        textSlice.addFocusListener(this);

        JPanel imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        imagePanel.add(sliceLabel);
        imagePanel.add(textSlice);

        JPanel destinationPanel = new JPanel(new GridLayout(2, 1));

        // destination panel setup
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        newImage.setEnabled(true);
        destinationGroup.add(newImage); // add the button to the grouping
        destinationPanel.add(newImage); // add the button to the component

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        replaceImage.setEnabled(true);
        destinationGroup.add(replaceImage); // add the button to the grouping
        destinationPanel.add(replaceImage); // add the button to the component

        // buildOKButton();
        // buildCancelButton();

        // JPanel buttonPanel = new JPanel();
        // buttonPanel.add(OKButton);
        // buttonPanel.add(cancelButton);


        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(destinationPanel, BorderLayout.CENTER);

        // getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = textSlice.getText();

        if (testParameter(tmpStr, 0, (nSlices-1))) {
            referenceSlice = Integer.parseInt(tmpStr);
        } else {
            textSlice.requestFocus();
            textSlice.selectAll();

            return false;
        }

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        userInterface = ViewUserInterface.getReference();
        
        return true;
    }

}
