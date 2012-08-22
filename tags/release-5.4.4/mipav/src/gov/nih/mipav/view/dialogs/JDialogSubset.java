package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.*;

import javax.swing.*;


/**
 * Creates the dialog to create a 3D subset image from a 4D image. User selects dimension to be eliminated and the value
 * of that dimension in the 3D subset. Allows only 4D images; 2D or 3D images would not make sense with this operation.
 */
public class JDialogSubset extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7757821847126335458L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] destExtents; // length along an axis of the destination image

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelSlice;

    /** DOCUMENT ME! */
    private int removeDim;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String resultString;

    /** DOCUMENT ME! */
    private int sliceNum;

    /** DOCUMENT ME! */
    private AlgorithmSubset subsetAlgo;

    /** DOCUMENT ME! */
    private JTextField textSlice;

    /** DOCUMENT ME! */
    private String textString;

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private int tSlices; // number of t slices in image

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton xButton, yButton, zButton, tButton;

    /** DOCUMENT ME! */
    private int xSlices;

    /** DOCUMENT ME! */
    private int ySlices;

    /** DOCUMENT ME! */
    private int zSlices;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSubset() {}

    /**
     * Creates new dialog for getting subset.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogSubset(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im; // set the image from the arguments to an image in this class
        userInterface = ViewUserInterface.getReference();
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("Extract")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("U4003");
            MipavUtil.showWebHelp("4_D_tools#Extracting_3D_subset_from_4D");
        } else if (command.equals("XAxis")) {
            labelSlice.setText("Select index from 0 to " + (xSlices - 1));
        } else if (command.equals("YAxis")) {
            labelSlice.setText("Select index from 0 to " + (ySlices - 1));
        } else if (command.equals("ZAxis")) {
            labelSlice.setText("Select index from 0 to " + (zSlices - 1));
        } else if (command.equals("TAxis")) {
            labelSlice.setText("Select index from 0 to " + (tSlices - 1));
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmSubset) {

            if ( (subsetAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // put the new image into a new frame
                    new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("JDialogSubset reports: out of memory; " + "unable to open a new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
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

        // save the completion status for later
        setComplete(algorithm.isCompleted());

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
        subsetAlgo.finalize();
        subsetAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the Dimension to remove according to the parameter.
     * 
     * @param dim Which dimension to remove (either REMOVE_X, REMOVE_Y, REMOVE_Z, REMOVE_T)
     */
    public void setRemoveDim(final int dim) {
        removeDim = dim;
    }

    /**
     * Accessor that sets the slice number to be used to the parameter.
     * 
     * @param slice The slice index number to be use
     */
    public void setSliceNum(final int slice) {
        sliceNum = slice;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    public void callAlgorithm() {

        destExtents = new int[3];

        xSlices = image.getExtents()[0];
        ySlices = image.getExtents()[1];
        zSlices = image.getExtents()[2];
        tSlices = image.getExtents()[3];

        if (removeDim == AlgorithmSubset.REMOVE_X) {
            destExtents[0] = ySlices;
            destExtents[1] = zSlices;
            destExtents[2] = tSlices;

        } else if (removeDim == AlgorithmSubset.REMOVE_Y) {
            destExtents[0] = xSlices;
            destExtents[1] = zSlices;
            destExtents[2] = tSlices;

        } else if (removeDim == AlgorithmSubset.REMOVE_Z) {
            destExtents[0] = xSlices;
            destExtents[1] = ySlices;
            destExtents[2] = tSlices;

        } else {
            destExtents[0] = xSlices;
            destExtents[1] = ySlices;
            destExtents[2] = zSlices;
        }

        if (sliceNum < 0) {
            MipavUtil.displayError("Slice number must be at least 0");
            textSlice.requestFocus();
            textSlice.selectAll();

            return;
        } else if ( (removeDim == AlgorithmSubset.REMOVE_X) && (sliceNum > (xSlices - 1))) {
            MipavUtil.displayError("X number must not exceed " + (xSlices - 1));
            textSlice.requestFocus();
            textSlice.selectAll();

            return;
        } else if ( (removeDim == AlgorithmSubset.REMOVE_Y) && (sliceNum > (ySlices - 1))) {
            MipavUtil.displayError("Y number must not exceed " + (ySlices - 1));
            textSlice.requestFocus();
            textSlice.selectAll();

            return;
        } else if ( (removeDim == AlgorithmSubset.REMOVE_Z) && (sliceNum > (zSlices - 1))) {
            MipavUtil.displayError("Z number must not exceed " + (zSlices - 1));
            textSlice.requestFocus();
            textSlice.selectAll();

            return;
        } else if ( (removeDim == AlgorithmSubset.REMOVE_T) && (sliceNum > (tSlices - 1))) {
            MipavUtil.displayError("T number must not exceed " + (tSlices - 1));
            textSlice.requestFocus();
            textSlice.selectAll();

            return;
        }

        if (removeDim == AlgorithmSubset.REMOVE_X) {
            resultString = image.getImageName() + "X=" + textString;
        } else if (removeDim == AlgorithmSubset.REMOVE_Y) {
            resultString = image.getImageName() + "Y=" + textString;
        } else if (removeDim == AlgorithmSubset.REMOVE_Z) {
            resultString = image.getImageName() + "Z=" + textString;
        } else {
            resultString = image.getImageName() + "T=" + textString;
        }

        try {

            // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
            resultImage = new ModelImage(image.getType(), destExtents, resultString);

            // Make algorithm:
            subsetAlgo = new AlgorithmSubset(image, resultImage, removeDim, sliceNum);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            subsetAlgo.addListener(this);

            createProgressBar(image.getImageName(), subsetAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (subsetAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                subsetAlgo.run();
            }

        } catch (final OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up image memory
                resultImage = null;
            }

            MipavUtil.displayError("JDialogSubset reports: unable to allocate enough memory");

            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        removeDim = scriptParameters.getParams().getInt("remove_dim");
        sliceNum = scriptParameters.getParams().getInt("slice_num");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(resultImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("remove_dim", removeDim));
        scriptParameters.getParams().put(ParameterFactory.newParameter("slice_num", sliceNum));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Extract 3D subset");
        setForeground(Color.black);

        xSlices = image.getExtents()[0];
        ySlices = image.getExtents()[1];
        zSlices = image.getExtents()[2];
        tSlices = image.getExtents()[3];

        final JPanel removePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        final ButtonGroup removeGroup = new ButtonGroup();
        xButton = new JRadioButton("X", false);
        xButton.setFont(serif12);
        removeGroup.add(xButton);
        removePanel.add(xButton);
        xButton.addActionListener(this);
        xButton.setActionCommand("XAxis");

        yButton = new JRadioButton("Y", false);
        yButton.setFont(serif12);
        removeGroup.add(yButton);
        removePanel.add(yButton);
        yButton.addActionListener(this);
        yButton.setActionCommand("YAxis");

        zButton = new JRadioButton("Z", true);
        zButton.setFont(serif12);
        removeGroup.add(zButton);
        removePanel.add(zButton);
        zButton.addActionListener(this);
        zButton.setActionCommand("ZAxis");

        tButton = new JRadioButton("T", false);
        tButton.setFont(serif12);
        removeGroup.add(tButton);
        removePanel.add(tButton);
        tButton.addActionListener(this);
        tButton.setActionCommand("TAxis");
        tButton.setSelected(true);

        final JPanel textPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        textSlice = new JTextField(5);
        textSlice.setText("0");
        textSlice.setFont(serif12);
        textSlice.setEnabled(true);
        textSlice.addFocusListener(this);
        textPanel.add(textSlice);

        //default is tButton selected
        labelSlice = new JLabel("Select index from 0 to " + (tSlices - 1));
        labelSlice.setForeground(Color.black);
        labelSlice.setFont(serif12);
        labelSlice.setEnabled(true);
        textPanel.add(labelSlice);

        final JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.add(removePanel);
        mainPanel.add(textPanel);
        mainPanel.setBorder(buildTitledBorder("Select removed dimension"));

        final JPanel buttonPanel = new JPanel(new FlowLayout());

        /*
         * buildOKButton(); OKButton.setText("Remove"); buttonPanel.add(OKButton); buildCancelButton();
         * buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());
        OKButton.setText("Extract");

        final JPanel panel = new JPanel(new BorderLayout());
        panel.add(mainPanel);
        panel.add(buttonPanel, BorderLayout.SOUTH);
        panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(panel);
        pack();
        labelSlice.setText("Select index from 0 to " + (tSlices - 1));
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (xButton.isSelected()) {
            removeDim = AlgorithmSubset.REMOVE_X;
        } else if (yButton.isSelected()) {
            removeDim = AlgorithmSubset.REMOVE_Y;
        } else if (zButton.isSelected()) {
            removeDim = AlgorithmSubset.REMOVE_Z;
        } else {
            removeDim = AlgorithmSubset.REMOVE_T;
        }

        textString = textSlice.getText();
        sliceNum = Integer.parseInt(textString);

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Utilities.4D tools");
            }

            public String getDescription() {
                return new String("Removes one of the dimensions from a 4D dataset to produce a 3D volume.");
            }

            public String getDescriptionLong() {
                return new String("Removes one of the dimensions from a 4D dataset to produce a 3D volume.");
            }

            public String getShortLabel() {
                return new String("Extract3DSubset");
            }

            public String getLabel() {
                return new String("Extract 3D subset from 4D volume");
            }

            public String getName() {
                return new String("Extract 3D subset from 4D volume");
            }

            public Set<ImageRequirements> getInputImageRequirements() {
                return EnumSet.of(ImageRequirements.NDIM_4);
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt("remove_dim", AlgorithmSubset.REMOVE_T));
            table.put(new ParameterInt("slice_num", 0));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                return getResultImage().getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
}
