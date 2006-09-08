package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the option to generate a
 * new image or replace the source image. In addition the user can indicate if you wishes to have the algorithm applied
 * to whole image or to the VOI regions. In should be noted that the algorithms are executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmUnsharpMask
 */
public class JDialogUnsharpMask extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3776616289889677111L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private final String DELIMITER = ",";

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private ButtonGroup imageVOIGroup;

    /** DOCUMENT ME! */
    private JPanel imageVOIPanel;

    /** DOCUMENT ME! */
    private JLabel labelCorrected;

    /** DOCUMENT ME! */
    private JLabel labelGaussX;

    /** DOCUMENT ME! */
    private JLabel labelGaussY;

    /** DOCUMENT ME! */
    private JLabel labelGaussZ;

    /** DOCUMENT ME! */
    private JLabel labelWeight;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private float normFactor = 1; // normalization factor to adjust for resolution

    /** or if the source image is to be replaced. */
    private boolean regionFlag; // true = apply algorithm to the whole image

    // false = apply algorithm only to VOI regions

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** difference between x,y resolutions (in plane) and z resolution (between planes). */
    private JCheckBox resolutionCheckbox;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JPanel scalePanel;

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float scaleZ;

    /** DOCUMENT ME! */
    private JTextField textGaussX;

    /** DOCUMENT ME! */
    private JTextField textGaussY;

    /** DOCUMENT ME! */
    private JTextField textGaussZ;

    /** DOCUMENT ME! */
    private JTextField textWeight;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private String tmpStr;

    /** DOCUMENT ME! */
    private AlgorithmUnsharpMask unsharpMaskAlgo;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private float weight;

    /** DOCUMENT ME! */
    private JPanel weightPanel;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogUnsharpMask() { }

    /**
     * Constructor.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogUnsharpMask(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogUnsharpMask(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
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
            MipavUtil.showHelp("10025");
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

        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmUnsharpMask) {
            image.clearMask();

            if ((unsharpMaskAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    // resultImage.setImageName("Unsharp mask");
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

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
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }

        saveDefaults();

        unsharpMaskAlgo.finalize();
        unsharpMaskAlgo = null;
        dispose();
    }

    /**
     * focusLost - when the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  event that triggers this function
     */
    public void focusLost(FocusEvent event) {
        Object source = event.getSource();
        float tempNum;

        if (source == textGaussZ) {

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
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
        float tempNum;

        if (source == resolutionCheckbox) {

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        } else if (source == image25DCheckbox) {

            if (image25DCheckbox.isSelected()) {
                resolutionCheckbox.setEnabled(false); // Image is only 2D or 2.5D, thus this checkbox
                labelGaussZ.setEnabled(false); // is not relevent
                textGaussZ.setEnabled(false);
                labelCorrected.setEnabled(false);
            } else {
                resolutionCheckbox.setEnabled(true);
                labelGaussZ.setEnabled(true);
                textGaussZ.setEnabled(true);
                labelCorrected.setEnabled(true);
            }
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, DELIMITER);

                textGaussX.setText(st.nextToken());
                textGaussY.setText(st.nextToken());
                textGaussZ.setText(st.nextToken());
                textWeight.setText(st.nextToken());

                if (MipavUtil.getBoolean(st)) {
                    newImage.setSelected(true);
                } else {
                    replaceImage.setSelected(true);
                }

                if (MipavUtil.getBoolean(st)) {
                    wholeImage.setSelected(true);
                } else {
                    VOIRegions.setSelected(true);
                }

                resolutionCheckbox.setSelected(MipavUtil.getBoolean(st));
                image25DCheckbox.setSelected(MipavUtil.getBoolean(st));
            } catch (NoSuchElementException nsee) {
                return;
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                System.out.println("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = scaleX + DELIMITER;
        defaultsString += scaleY + DELIMITER;
        defaultsString += textGaussZ.getText() + DELIMITER;
        defaultsString += textWeight.getText() + DELIMITER;
        defaultsString += newImage.isSelected() + DELIMITER;
        defaultsString += wholeImage.isSelected() + DELIMITER;
        defaultsString += resolutionCheckbox.isSelected() + DELIMITER;
        defaultsString += image25DCheckbox.isSelected();

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
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
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Accessor that sets the region flag.
     *
     * @param  flag  <code>true</code> indicates the whole image is blurred, <code>false</code> indicates a region.
     */
    public void setRegionFlag(boolean flag) {
        regionFlag = flag;
    }

    /**
     * Accessor that sets the x scale.
     *
     * @param  scale  Value to set x scale to (should be between 0.0 and 10.0).
     */
    public void setScaleX(float scale) {
        scaleX = scale;
    }

    /**
     * Accessor that sets the y scale.
     *
     * @param  scale  Value to set y scale to (should be between 0.0 and 10.0).
     */
    public void setScaleY(float scale) {
        scaleY = scale;
    }

    /**
     * Accessor that sets the z scale.
     *
     * @param  scale  Value to set z scale to (should be between 0.0 and 10.0).
     */
    public void setScaleZ(float scale) {
        scaleZ = scale;
    }

    /**
     * Accessor that sets the weight.
     *
     * @param  w  Value to set weight to.
     */
    public void setWeight(float w) {
        weight = w;
    }

    /**
     * Once all the necessary variables are set, call the UnsharpMark algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_unsharp");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = new float[2];
            sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
            sigmas[1] = scaleY;

            if (displayLoc == NEW) {

                try {

                    // Make result image of float type
                    // Will not work for UBYTE and USHORT
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0002",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0008,0016",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26);
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                    }

                    // Make algorithm
                    unsharpMaskAlgo = new AlgorithmUnsharpMask(resultImage, image, sigmas, weight, regionFlag, false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    unsharpMaskAlgo.addListener(this);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (unsharpMaskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        if (!userInterface.isAppFrameVisible()) {
                            unsharpMaskAlgo.setProgressBarVisible(false);
                        }

                        unsharpMaskAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Unsharp Mask: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    unsharpMaskAlgo = new AlgorithmUnsharpMask(image, sigmas, weight, regionFlag, false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    unsharpMaskAlgo.addListener(this);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (unsharpMaskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        if (!userInterface.isAppFrameVisible()) {
                            unsharpMaskAlgo.setProgressBarVisible(false);
                        }

                        unsharpMaskAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Unsharp Mask: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = new float[3];
            sigmas[0] = scaleX;
            sigmas[1] = scaleY;
            sigmas[2] = scaleZ; // normalized  - scaleZ * resolutionX/resolutionZ; !!!!!!!

            if (displayLoc == NEW) {

                try {

                    // Make result image of float type
                    // Will not work for UBYTE and USHORT
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0002",
                                                                                    "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0008,0016",
                                                                                    "1.2.840.10008.5.1.4.1.1.7 ", 26);
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0012", "1.2.840.34379.17",
                                                                                    16); // bogus Implementation UID
                                                                                         // made up by Matt
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0013", "MIPAV--NIH", 10); //
                        }
                    }

                    // Make algorithm
                    unsharpMaskAlgo = new AlgorithmUnsharpMask(resultImage, image, sigmas, weight, regionFlag,
                                                               image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    unsharpMaskAlgo.addListener(this);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (unsharpMaskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        if (!userInterface.isAppFrameVisible()) {
                            unsharpMaskAlgo.setProgressBarVisible(false);
                        }

                        unsharpMaskAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Unsharp Mask: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    unsharpMaskAlgo = new AlgorithmUnsharpMask(image, sigmas, weight, regionFlag, image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    unsharpMaskAlgo.addListener(this);

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (isRunInSeparateThread()) {

                        if (unsharpMaskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        if (!userInterface.isAppFrameVisible()) {
                            unsharpMaskAlgo.setProgressBarVisible(false);
                        }

                        unsharpMaskAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Unsharp Mask: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        scaleX = scriptParameters.getParams().getFloat("scaleX");
        scaleY = scriptParameters.getParams().getFloat("scaleY");
        scaleZ = scriptParameters.getParams().getFloat("scaleZ");
        weight = scriptParameters.getParams().getFloat("weight");
        image25D = scriptParameters.doProcess3DAs25D();
        regionFlag = scriptParameters.doProcessWholeImage();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, displayLoc == NEW);

        scriptParameters.getParams().put(ParameterFactory.newParameter("scaleX", scaleX));
        scriptParameters.getParams().put(ParameterFactory.newParameter("scaleY", scaleY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("scaleZ", scaleZ));
        scriptParameters.getParams().put(ParameterFactory.newParameter("weight", weight));
        scriptParameters.storeProcess3DAs25D(image25D);
        scriptParameters.storeProcessWholeImage(regionFlag);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Unsharp Mask");

        JPanel mainPanel;
        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        scalePanel = new JPanel(new GridLayout(3, 2));
        scalePanel.setForeground(Color.black);
        scalePanel.setBorder(buildTitledBorder("Scale of the Gaussian"));
        mainPanel.add(scalePanel, gbc);

        labelGaussX = createLabel("X Dimension (0.0 - 10.0) ");
        scalePanel.add(labelGaussX);
        textGaussX = createTextField("1.0");
        scalePanel.add(textGaussX);

        labelGaussY = createLabel("Y Dimension (0.0 - 10.0) ");
        scalePanel.add(labelGaussY);
        textGaussY = createTextField("1.0");
        scalePanel.add(textGaussY);

        labelGaussZ = createLabel("Z Dimension (0.0 - 10.0) ");
        scalePanel.add(labelGaussZ);
        textGaussZ = createTextField("1.0");
        scalePanel.add(textGaussZ);

        JPanel resPanel = new JPanel(new BorderLayout());
        resPanel.setBorder(buildTitledBorder("Resolution options"));
        resolutionCheckbox = new JCheckBox("Use image resolutions to normalize Z scale.");
        resolutionCheckbox.setFont(serif12);
        resPanel.add(resolutionCheckbox, BorderLayout.NORTH);
        resolutionCheckbox.setSelected(true);

        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D).");
        image25DCheckbox.setFont(serif12);
        resPanel.add(image25DCheckbox, BorderLayout.SOUTH);
        image25DCheckbox.setSelected(false);
        image25DCheckbox.addItemListener(this);

        if (image.getNDims() == 3) { // if the source image is 3D then allow
            resolutionCheckbox.setEnabled(true); // the user to indicate if it wishes to
            resolutionCheckbox.addItemListener(this); // use the correction factor
            textGaussZ.addFocusListener(this);
            textGaussZ.setEnabled(true);
        } else {
            resolutionCheckbox.setEnabled(false); // Image is only 2D, thus this checkbox
            labelGaussZ.setEnabled(false); // is not relevent
            textGaussZ.setEnabled(false);
            image25DCheckbox.setEnabled(false);
        }

        if (image.getNDims() == 3) { // Source image is 3D, thus show correction factor

            int index = image.getExtents()[2] / 2;
            float xRes = image.getFileInfo(index).getResolutions()[0];
            float zRes = image.getFileInfo(index).getResolutions()[2];
            normFactor = xRes / zRes; // Calculate correction factor
            labelCorrected = new JLabel("      Corrected scale = " +
                                        String.valueOf(normFactor * Float.valueOf(textGaussZ.getText()).floatValue()));
            labelCorrected.setForeground(Color.black);
            labelCorrected.setFont(serif12);
            resPanel.add(labelCorrected, BorderLayout.CENTER);
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(resPanel, gbc);

        weightPanel = new JPanel();
        weightPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        weightPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        weightPanel.setForeground(Color.black);
        weightPanel.setBorder(buildTitledBorder("Weight of blurred image"));
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(weightPanel, gbc);

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        labelWeight = createLabel("Image - (k * blurred image) where 0 < k < 1");
        weightPanel.add(labelWeight, gbc2);

        gbc2.gridx = 1;
        gbc2.gridy = 0;
        textWeight = createTextField("0.75");
        weightPanel.add(textWeight, gbc2);

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        imageVOIPanel = new JPanel();
        imageVOIPanel.setLayout(new BorderLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Process"));
        outputOptPanel.add(imageVOIPanel);

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);
        imageVOIPanel.add(wholeImage, BorderLayout.NORTH);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);
        imageVOIPanel.add(VOIRegions, BorderLayout.CENTER);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(outputOptPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
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

        System.gc();

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (wholeImage.isSelected()) {
            regionFlag = true;
        } else if (VOIRegions.isSelected()) {
            regionFlag = false;
        }

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        }

        tmpStr = textGaussX.getText();

        if (testParameter(tmpStr, 0.0, 10.0)) {
            scaleX = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussX.requestFocus();
            textGaussX.selectAll();

            return false;
        }

        tmpStr = textGaussY.getText();

        if (testParameter(tmpStr, 0.0, 10.0)) {
            scaleY = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussY.requestFocus();
            textGaussY.selectAll();

            return false;
        }

        tmpStr = textGaussZ.getText();

        if (testParameter(tmpStr, 0.0, 10.0)) {
            scaleZ = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussZ.requestFocus();
            textGaussZ.selectAll();

            return false;
        }

        tmpStr = textWeight.getText();

        if (testParameter(tmpStr, 0, 0.999)) {
            weight = Float.valueOf(tmpStr).floatValue();
        } else {
            textWeight.requestFocus();
            textWeight.selectAll();

            return false;
        }

        // Apply normalization if requested!
        if (resolutionCheckbox.isSelected()) {
            scaleZ = scaleZ * normFactor;
        }

        return true;
    }

}
