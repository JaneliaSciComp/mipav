package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. User can indicate whether to have algorithm applied to whole image or to the VOI regions. Algorithms
 * are executed in their own thread.
 *
 * @version  0.1 February 17, 2005
 * @author   William Gandler
 * @see      AlgorithmMorphologicalFilter
 */
public class JDialogMorphologicalFilter extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -523142924576794921L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private JLabel labelSizeX;

    /** DOCUMENT ME! */
    private JLabel labelSizeY;

    /** DOCUMENT ME! */
    private JLabel labelSizeZ;

    /** DOCUMENT ME! */
    private AlgorithmMorphologicalFilter mfAlgo;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JPanel sizePanel;

    /** DOCUMENT ME! */
    private int sizeX;

    /** DOCUMENT ME! */
    private int sizeY;

    /** DOCUMENT ME! */
    private int sizeZ = 1;

    /** DOCUMENT ME! */
    private JTextField textSizeX;

    /** DOCUMENT ME! */
    private JTextField textSizeY;

    /** DOCUMENT ME! */
    private JTextField textSizeZ;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMorphologicalFilter() { }

    /**
     * Creates a new JDialogMorphologicalFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMorphologicalFilter(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
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
            // MipavUtil.showHelp( "" );
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
        Preferences.debug("Morphological Filter: " + algorithm.getElapsedTime());

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmMorphologicalFilter) {
            image.clearMask();

            if ((mfAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
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
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (mfAlgo != null) {
            mfAlgo.finalize();
            mfAlgo = null;
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
     * Resets labels if checkboxes are checked or unchecked.
     *
     * @param  event  Event that cause the method to fire.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == image25DCheckbox) {

            if (image25DCheckbox.isSelected()) {
                labelSizeZ.setEnabled(false); // is not relevant
                textSizeZ.setEnabled(false);
            } else {
                labelSizeZ.setEnabled(true);
                textSizeZ.setEnabled(true);
            }
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (outputPanel != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                outputPanel.setProcessWholeImage(MipavUtil.getBoolean(st));

                image25DCheckbox.setSelected(MipavUtil.getBoolean(st));

                textSizeX.setText("" + MipavUtil.getInt(st));
                textSizeY.setText("" + MipavUtil.getInt(st));
                textSizeZ.setText("" + MipavUtil.getInt(st));

                outputPanel.setOutputNewImage(MipavUtil.getBoolean(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }

    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String delim = ",";

        String defaultsString = outputPanel.isProcessWholeImageSet() + delim;
        defaultsString += image25D + delim;
        defaultsString += sizeX + delim;
        defaultsString += sizeY + delim;
        defaultsString += sizeZ + delim;
        defaultsString += outputPanel.isOutputNewImageSet();

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
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
     * Accessor that sets the x size.
     *
     * @param  sizeX  Value to set x size
     */
    public void setSizeX(int sizeX) {
        this.sizeX = sizeX;
    }

    /**
     * Accessor that sets the y size.
     *
     * @param  sizeY  Value to set y size
     */
    public void setSizeY(int sizeY) {
        this.sizeY = sizeY;
    }

    /**
     * Accessor that sets the z size.
     *
     * @param  sizeZ  Value to set z size
     */
    public void setSizeZ(int sizeZ) {
        this.sizeZ = sizeZ;
    }

    /**
     * Once all the necessary variables are set, call the Sizeian Blur algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_morfilter");

        if (image.getNDims() == 2) { // source image is 2D

            int[] sizes = new int[2];

            sizes[0] = sizeX;
            sizes[1] = sizeY;

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = new ModelImage(image.getType(), image.getExtents(), name);

                    // resultImage = (ModelImage)image.clone();
                    // resultImage.setImageName(name);
                    /*if ( ( resultImage.getFileInfo()[0] ).getFileFormat() == FileUtility.DICOM ) {
                     *  ( (FileInfoDicom) ( resultImage.getFileInfo( 0 ) ) ).setSecondaryCaptureTags();}*/
                    // Make algorithm
                    mfAlgo = new AlgorithmMorphologicalFilter(resultImage, image, sizes,
                                                              outputPanel.isProcessWholeImageSet(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    mfAlgo.addListener(this);

                    createProgressBar(image.getImageName(), mfAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (mfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        mfAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Morphological filter: unable to allocate enough memory");

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
                    mfAlgo = new AlgorithmMorphologicalFilter(image, sizes, outputPanel.isProcessWholeImageSet(),
                                                              false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    mfAlgo.addListener(this);

                    createProgressBar(image.getImageName(), mfAlgo);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (mfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        mfAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Morphological filter: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {

            int[] sizes = new int[3];

            sizes[0] = sizeX;
            sizes[1] = sizeY;
            sizes[2] = sizeZ; // normalized  - sizeZ * resolutionX/resolutionZ; !!!!!!!

            if (outputPanel.isOutputNewImageSet()) {

                try {
                    // Make result image of float type

                    // resultImage = new ModelImage(ModelImage.FLOAT, image.getExtents(), name,
                    // userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    /* if ( ( resultImage.getFileInfo()[0] ).getFileFormat() == FileUtility.DICOM ) {
                     *   for ( int i = 0; i < resultImage.getExtents()[2]; i++ ) {      ( (FileInfoDicom) (
                     * resultImage.getFileInfo( i ) ) ).setValue( "0002,0002",              "1.2.840.10008.5.1.4.1.1.7
                     * ", 26 ); // Secondary Capture SOP UID      ( (FileInfoDicom) ( resultImage.getFileInfo( i ) )
                     * ).setValue( "0008,0016",              "1.2.840.10008.5.1.4.1.1.7 ", 26 );      ( (FileInfoDicom)
                     * ( resultImage.getFileInfo( i ) ) ).setValue( "0002,0012",              "1.2.840.34379.17", 16 );
                     * // bogus Implementation UID made up by Matt      ( (FileInfoDicom) ( resultImage.getFileInfo( i )
                     * ) ).setValue( "0002,0013", "MIPAV--NIH", 10 ); //  } }*/
                    // Make algorithm
                    mfAlgo = new AlgorithmMorphologicalFilter(resultImage, image, sizes,
                                                              outputPanel.isProcessWholeImageSet(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    mfAlgo.addListener(this);

                    createProgressBar(image.getImageName(), mfAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (mfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        mfAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Morphological filter: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    mfAlgo = new AlgorithmMorphologicalFilter(image, sizes, outputPanel.isProcessWholeImageSet(),
                                                              image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    mfAlgo.addListener(this);

                    createProgressBar(image.getImageName(), mfAlgo);

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (mfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        mfAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Morphological filter: unable to allocate enough memory");

                    return;
                }
            }
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        setSizeX(scriptParameters.getParams().getInt("filter_size_x"));
        setSizeY(scriptParameters.getParams().getInt("filter_size_y"));
        setSizeZ(scriptParameters.getParams().getInt("filter_size_z"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());
        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), image25D);

        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_size_x", sizeX));
        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_size_y", sizeY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_size_z", sizeZ));
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Morphological Filter");

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

        sizePanel = new JPanel(new GridLayout(3, 2));
        sizePanel.setForeground(Color.black);
        sizePanel.setBorder(buildTitledBorder("Filter Size"));
        mainPanel.add(sizePanel, gbc);

        labelSizeX = new JLabel("X ");
        labelSizeX.setForeground(Color.black);
        labelSizeX.setFont(serif12);
        sizePanel.add(labelSizeX);

        textSizeX = new JTextField();
        textSizeX.setText("5");
        textSizeX.setFont(serif12);
        sizePanel.add(textSizeX);

        labelSizeY = new JLabel("Y ");
        labelSizeY.setForeground(Color.black);
        labelSizeY.setFont(serif12);
        sizePanel.add(labelSizeY);

        textSizeY = new JTextField();
        textSizeY.setText("5");
        textSizeY.setFont(serif12);
        sizePanel.add(textSizeY);

        labelSizeZ = new JLabel("Z ");
        labelSizeZ.setForeground(Color.black);
        labelSizeZ.setFont(serif12);
        sizePanel.add(labelSizeZ);

        textSizeZ = new JTextField();
        textSizeZ.setText("5");
        textSizeZ.setFont(serif12);
        sizePanel.add(textSizeZ);

        JPanel optPanel = new JPanel(new BorderLayout());

        optPanel.setBorder(buildTitledBorder("Options"));


        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        optPanel.add(image25DCheckbox, BorderLayout.SOUTH);
        image25DCheckbox.setSelected(false);
        image25DCheckbox.addItemListener(this);

        if (image.getNDims() == 3) { // if the source image is 3D then allow
            textSizeZ.setEnabled(true);
        } else {
            labelSizeZ.setEnabled(false); // is not relevent
            textSizeZ.setEnabled(false);
            image25DCheckbox.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(optPanel, gbc);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(outputPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        // setVisible(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        System.gc();

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        }

        tmpStr = textSizeX.getText();

        if (testParameter(tmpStr, 1, image.getExtents()[0])) {
            sizeX = Integer.valueOf(tmpStr).intValue();
        } else {
            textSizeX.requestFocus();
            textSizeX.selectAll();

            return false;
        }

        if ((sizeX % 2) != 1) {
            MipavUtil.displayError("X size must be an odd number");
            textSizeX.requestFocus();
            textSizeX.selectAll();

            return false;
        }

        tmpStr = textSizeY.getText();

        if (testParameter(tmpStr, 1, image.getExtents()[1])) {
            sizeY = Integer.valueOf(tmpStr).intValue();
        } else {
            textSizeY.requestFocus();
            textSizeY.selectAll();

            return false;
        }

        if ((sizeY % 2) != 1) {
            MipavUtil.displayError("Y size must be an odd number");
            textSizeY.requestFocus();
            textSizeY.selectAll();

            return false;
        }


        if (image.getNDims() > 2) {
            tmpStr = textSizeZ.getText();

            if (testParameter(tmpStr, 1, image.getExtents()[2])) {
                sizeZ = Integer.valueOf(tmpStr).intValue();
            } else {
                textSizeZ.requestFocus();
                textSizeZ.selectAll();

                return false;
            }

            if ((sizeZ % 2) != 1) {
                MipavUtil.displayError("Z size must be an odd number");
                textSizeZ.requestFocus();
                textSizeZ.selectAll();

                return false;
            }
        } // if (image.getNDims() > 2)

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
                return new String("Algorithms.Morphological");
            }

            public String getDescription() {
                return new String("Performs morphological filtering on black and white images.");
            }

            public String getDescriptionLong() {
                return new String("Performs morphological filtering on black and white images. " +
                		"Corrects for non-uniform illumination and non-uniform camera sensitivity.");
            }

            public String getShortLabel() {
                return new String("MorphologicalFilter");
            }

            public String getLabel() {
                return new String("Morphological Filter");
            }

            public String getName() {
                return new String("Morphological Filter");
            }
            
            public Set<ImageRequirements> getInputImageRequirements() {
                return EnumSet.of(ImageRequirements.GRAYSCALE);
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterInt("filter_size_x", 5));
            table.put(new ParameterInt("filter_size_y", 5));
            table.put(new ParameterInt("filter_size_z", 5));
            
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
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
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
