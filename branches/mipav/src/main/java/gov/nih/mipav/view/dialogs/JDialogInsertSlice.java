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

import javax.swing.*;


/**
 * Creates the dialog to insert slice into an image. Dialog asks the user to enter the slice number before which the new
 * slice is to be inserted or for the last slice number plus one if the slice is to be added to the end. The user may
 * select either average or blank or original for the new slice. The slices are assumed to be parallel and uniformly
 * spaced, so average so calculates the mean of the 2 surrounding slices or duplicates an end slice if the new slice is
 * to be placed at the beginnig or end. Blank creates a new slice with all the pixels set to zero. Original inserts a 2D
 * image of the same dimensions selected from a pop up menu. Must insert black and white with black and white and color
 * with color.
 */
public class JDialogInsertSlice extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7157371462824445245L;

    /** DOCUMENT ME! */
    public static final int AVERAGE_SLICE = 1;

    /** DOCUMENT ME! */
    public static final int BLANK_SLICE = 2;

    /** DOCUMENT ME! */
    public static final int ORIGINAL_SLICE = 3;

    /** DOCUMENT ME! */
    public static final int ADJACENT_DOWN_SLICE = 4;

    /** DOCUMENT ME! */
    public static final int ADJACENT_UP_SLICE = 5;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton adjacentDown, adjacentUp;

    /** DOCUMENT ME! */
    private JRadioButton average;

    /** DOCUMENT ME! */
    private JRadioButton blank;

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage insertedImage = null; // inserted image for ORIGINAL_SLICE

    /** DOCUMENT ME! */
    private int insertSlice;

    /** DOCUMENT ME! */
    private AlgorithmInsertSlice insertSliceAlgo;

    /** DOCUMENT ME! */
    private int nSlices; // number of slices in image

    /** DOCUMENT ME! */
    private JRadioButton original;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int sliceType;

    /** DOCUMENT ME! */
    private JTextField textSlice;

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogInsertSlice() { }

    /**
     * Creates new dialog for inserting a slice.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogInsertSlice(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im; // set the image from the arguments to an image in this class
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
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
            //MipavUtil.showHelp("U4055");
            MipavUtil.showWebHelp("Slice_tools#Insert_Missing_Slices");
        } else if ((source == average) || (source == blank) || (source == original) || (source == adjacentUp) ||
                       (source == adjacentDown)) {

            if (original.isSelected()) {
                comboBoxImage.setEnabled(true);
            } else {
                comboBoxImage.setEnabled(false);
            }
        } else {
            super.actionPerformed(event);
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

        if (algorithm instanceof AlgorithmInsertSlice) {

            if ((insertSliceAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // put the new image into a new frame
                    new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                    Preferences.debug("\nHave inserted ");

                    if (sliceType == AVERAGE_SLICE) {
                        Preferences.debug("average slice ");
                    } else if (sliceType == BLANK_SLICE) {
                        Preferences.debug("blank slice ");
                    } else if (sliceType == ORIGINAL_SLICE) {
                        Preferences.debug("original slice from " + insertedImage.getImageName() + "\n");
                    } else if (sliceType == ADJACENT_DOWN_SLICE) {
                        Preferences.debug("adjacent down slice ");
                    } else {
                        Preferences.debug("adjacent up slice ");
                    }

                    Preferences.debug("as new slice number " + insertSlice + "\n");

                    if (image.getNDims() == 3) {
                        Preferences.debug("into " + image.getFileInfo()[0].getExtents()[2] + " slice 3D " +
                                          image.getImageName() + "\n");
                        Preferences.debug("to create\n");
                        Preferences.debug(resultImage.getFileInfo()[0].getExtents()[2] + " slice 3D " +
                                          resultImage.getImageName() + "\n");
                    } // if (image.getNDims() == 3)
                    else { // image.getNDims() == 4
                        Preferences.debug("into " + image.getFileInfo()[0].getExtents()[2] + " slice " +
                                          image.getFileInfo()[0].getExtents()[3] + " volume 4D " +
                                          image.getImageName() + "\n");
                        Preferences.debug("to create\n");
                        Preferences.debug(resultImage.getFileInfo()[0].getExtents()[2] + " slice " +
                                          resultImage.getFileInfo()[0].getExtents()[3] + " volume 4D " +
                                          resultImage.getImageName() + "\n");
                    } // image.getNDims() == 4
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
                
                resultImage.getMatrixHolder().replaceMatrices(image.getMatrixHolder().getMatrices());
                resultImage.getFileInfo(0).setOrigin(image.getFileInfo(0).getOrigin());
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);

                if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                    Preferences.debug("\nHave inserted ");

                    if (sliceType == AVERAGE_SLICE) {
                        Preferences.debug("average slice ");
                    } else if (sliceType == BLANK_SLICE) {
                        Preferences.debug("blank slice ");
                    } else if (sliceType == ORIGINAL_SLICE) {
                        Preferences.debug("original slice from " + insertedImage.getImageName() + "\n");
                    } else if (sliceType == ADJACENT_DOWN_SLICE) {
                        Preferences.debug("adjacent down slice ");
                    } else {
                        Preferences.debug("adjacent up slice ");
                    }

                    Preferences.debug("as new slice number " + insertSlice + "\n");

                    if (image.getNDims() == 3) {
                        Preferences.debug("into " + (image.getFileInfo()[0].getExtents()[2] - 1) + " slice 3D " +
                                          image.getImageName() + "\n");
                        Preferences.debug("to create\n");
                        Preferences.debug(image.getFileInfo()[0].getExtents()[2] + " slice 3D " + image.getImageName() +
                                          "\n");
                    } // if (image.getNDims() == 3)
                    else { // image.getNDims() == 4
                        Preferences.debug("into " + (image.getFileInfo()[0].getExtents()[2] - 1) + " slice " +
                                          image.getFileInfo()[0].getExtents()[3] + " volume 4D " +
                                          image.getImageName() + "\n");
                        Preferences.debug("to create\n");
                        Preferences.debug(image.getFileInfo()[0].getExtents()[2] + " slice " +
                                          image.getFileInfo()[0].getExtents()[3] + " volume 4D " +
                                          image.getImageName() + "\n");
                    } // image.getNDims() == 4
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))

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

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
        insertSliceAlgo.finalize();
        insertSliceAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor to specify the 2D image used as an inserted slice for ORGINAL_SLICE.
     *
     * @param  insertedImage  DOCUMENT ME!
     */
    public void setInsertedImage(ModelImage insertedImage) {
        this.insertedImage = insertedImage;
    }

    /**
     * Accessor which lets you change where to insert the slice.
     *
     * @param  num  slice number before which the new slice is to be inserted
     */
    public void setInsertSliceNumber(int num) {
        insertSlice = num;
    }

    /**
     * Accessor which lets you change the type of slice to be inserted.
     *
     * @param  type  the type of slice to be inserted (either AVERAGE_SLICE or BLANK_Slice)
     */
    public void setSliceType(int type) {
        sliceType = type;
    }

    /**
     * Once all the necessary variables are set, call the Insert Slice algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        nSlices = image.getExtents()[2];

        int[] destExtents = null; // length along an axis of the destination image

        try {

            if (image.getNDims() == 3) {
                destExtents = new int[3];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2] + 1;
            } else { // NDims == 4
                destExtents = new int[4];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2] + 1;
                destExtents[3] = image.getExtents()[3];
            }

            // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
            resultImage = new ModelImage(image.getType(), destExtents, image.getImageName());

            // Make algorithm:
            insertSliceAlgo = new AlgorithmInsertSlice(image, resultImage, insertSlice, sliceType, insertedImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            insertSliceAlgo.addListener(this);

            createProgressBar(image.getImageName(), insertSliceAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (insertSliceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                insertSliceAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up image memory
                resultImage = null;
            }

            MipavUtil.displayError("Insert Slice reports: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        setInsertSliceNumber(scriptParameters.getParams().getInt("insert_slice_at_position"));
        setSliceType(scriptParameters.getParams().getInt("slice_type_to_insert"));

        if ((sliceType == ORIGINAL_SLICE) && scriptParameters.getParams().containsParameter("insert_from_image")) {
            setInsertedImage(scriptParameters.retrieveImage("insert_from_image"));
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());

        if ((sliceType == ORIGINAL_SLICE) && (insertedImage != null)) {
            scriptParameters.storeImage(insertedImage, "insert_from_image");
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("insert_slice_at_position", insertSlice));
        scriptParameters.getParams().put(ParameterFactory.newParameter("slice_type_to_insert", sliceType));
    }

    /**
     * Builds a list of images to register to the template image.
     */
    private void buildComboBox() {
        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);
        comboBoxImage.addItemListener(this);
        comboBoxImage.setEnabled(false);

        Enumeration<String> names = userInterface.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = names.nextElement();
            ModelImage namedImage = userInterface.getRegisteredImageByName(name);

            if ((!image.getImageName().equals(name)) && (userInterface.getFrameContainingImage(namedImage) != null) &&
                    (namedImage.getNDims() == 2) && (namedImage.getExtents()[0] == image.getExtents()[0]) &&
                    (namedImage.getExtents()[1] == image.getExtents()[1]) &&
                    (namedImage.isColorImage() == image.isColorImage()) &&
                    (namedImage.isComplexImage() == image.isComplexImage())) {
                comboBoxImage.addItem(name);
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        nSlices = image.getExtents()[2];

        setTitle("Insert slice");
        setForeground(Color.black);

        JPanel slicePanel = new JPanel(new GridBagLayout());
        slicePanel.setBorder(buildTitledBorder("Insert slice"));

        JLabel sliceLabel = new JLabel("Insert before slice #(0-" + String.valueOf(nSlices-1) + ") or enter " +
                                       String.valueOf(nSlices) + " for new last slice");
        sliceLabel.setFont(serif12);
        sliceLabel.setForeground(Color.black);

        textSlice = new JTextField(5);
        textSlice.setText("");
        textSlice.setFont(serif12);
        textSlice.setEnabled(true);
        textSlice.addFocusListener(this);

        ButtonGroup sliceGroup = new ButtonGroup();
        average = new JRadioButton("Average", true);
        average.setFont(serif12);
        average.addActionListener(this);
        sliceGroup.add(average);

        adjacentDown = new JRadioButton("Copy previous adjacent ", false);
        adjacentDown.setFont(serif12);
        adjacentDown.addActionListener(this);
        sliceGroup.add(adjacentDown);

        adjacentUp = new JRadioButton("Copy next adjacent", false);
        adjacentUp.setFont(serif12);
        adjacentUp.addActionListener(this);
        sliceGroup.add(adjacentUp);

        blank = new JRadioButton("Blank", false);
        blank.setFont(serif12);
        blank.addActionListener(this);
        sliceGroup.add(blank);

        original = new JRadioButton("Original", false);
        original.setFont(serif12);
        original.addActionListener(this);
        sliceGroup.add(original);

        buildComboBox();

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(3, 3, 3, 3);

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 0;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.NONE;
        slicePanel.add(sliceLabel, gbc);

        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        slicePanel.add(textSlice, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        slicePanel.add(average, gbc);

        gbc.gridy = 2;
        gbc.gridwidth = 1;
        slicePanel.add(adjacentDown, gbc);

        gbc.gridy = 3;
        slicePanel.add(adjacentUp, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        slicePanel.add(blank, gbc);

        gbc.gridy = 5;
        gbc.gridwidth = 1;
        slicePanel.add(original, gbc);
        gbc.gridx = 1;
        slicePanel.add(comboBoxImage, gbc);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        mainPanel.add(slicePanel);

        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buttonPanel.add(OKButton); buildCancelButton(); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
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

        if (testParameter(tmpStr, 0, (nSlices))) {
            insertSlice = Integer.parseInt(tmpStr);
        } else {
            textSlice.requestFocus();
            textSlice.selectAll();

            return false;
        }

        if (average.isSelected()) {
            sliceType = AVERAGE_SLICE;
        } else if (blank.isSelected()) {
            sliceType = BLANK_SLICE;
        } else if (adjacentUp.isSelected()) {
            sliceType = ADJACENT_UP_SLICE;
        } else if (adjacentDown.isSelected()) {
            sliceType = ADJACENT_DOWN_SLICE;
        } else {
            sliceType = ORIGINAL_SLICE;

            String selectName = (String) (comboBoxImage.getSelectedItem());

            if (selectName != null) {
                insertedImage = userInterface.getRegisteredImageByName(selectName);
            } else {
                insertedImage = null;
            }
        }
        if (insertSlice < 0) {
            MipavUtil.displayError("Slice you insert before must be at least 0");
            textSlice.requestFocus();
            textSlice.selectAll();

            return false;
        } else if (insertSlice > (nSlices)) {
            MipavUtil.displayError("Slice number cannot exceed " + String.valueOf(nSlices) +
                                   " for addition to end");
            textSlice.requestFocus();
            textSlice.selectAll();

            return false;
        } else if ((sliceType == ORIGINAL_SLICE) && (insertedImage == null)) {
            MipavUtil.displayError("No other image to insert.");

            return false;
        }

        return true;
    }
}
