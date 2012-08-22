package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
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
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user can indicate if you wishes to have the algorithm applied to whole image or to the
 * VOI regions. In should be noted, that the algorithms are executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmMask
 */
public class JDialogMask extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7339956453691640420L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private JRadioButton exteriorFill;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton interiorFill;

    /** DOCUMENT ME! */
    private AlgorithmMask maskAlgo;

    /** DOCUMENT ME! */
    private double max;

    /** DOCUMENT ME! */
    private double min;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** or if the source image is to be replaced. */
    private boolean polarity;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textValue;

    /** DOCUMENT ME! */
    private JTextField textValueB;

    /** DOCUMENT ME! */
    private JTextField textValueG;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private float value;

    /** DOCUMENT ME! */
    private float valueB = 0.0f;

    /** DOCUMENT ME! */
    private float valueG = 0.0f;

    private boolean useVOI = true;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMask() { }

    /**
     * Creates a new JDialogMask object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMask(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        setMinMax();
        init();
    }

    /**
     * Creates a new JDialogMask object.
     *
     * @param  im           The image to process.
     * @param  interactive  Whether the algorithm should be started immediately (usually false if calling this
     *                      constructor).
     * @param  pol          Whether to mask inside the VOIs (false == mask outside VOIs).
     */
    public JDialogMask(ModelImage im, boolean interactive, boolean pol) {
        super(false);

        userInterface = ViewUserInterface.getReference();
        image = im;
        setMinMax();
        parentFrame = im.getParentFrame();

        if (interactive == false) {
            callAlgorithmNonInteractive(pol);
        }

    }
    
    public JDialogMask(ModelImage im, boolean interactive, boolean pol, boolean useVOI) {
        super(false);

        userInterface = ViewUserInterface.getReference();
        image = im;
        setMinMax();
        parentFrame = im.getParentFrame();
        this.useVOI = useVOI;

        if (interactive == false) {
            callAlgorithmNonInteractive(pol);
        }

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
            //MipavUtil.showHelp("U4025");
            MipavUtil.showWebHelp("Masking_(filling)_images");
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

        if (algorithm instanceof AlgorithmMask) {
            image.clearMask();

            if ((maskAlgo.isCompleted() == true) && (resultImage != null)) {

                // copy over the file info from the original image, was just copying the data over before.
                // now the new image is "of the same image type" as the original
                FileInfoBase fileInfo;

                for (int i = 0; i < image.getFileInfo().length; i++) {
                    fileInfo = (FileInfoBase) (image.getFileInfo(i).clone());
                    resultImage.setFileInfo(fileInfo, i);
                }

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
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame)
                                                                                        (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame(parentFrame);
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

        maskAlgo.finalize();
        maskAlgo = null;
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
     * Accessor that sets the polarity flag.
     *
     * @param  flag  <code>true</code> indicates polarity, <code>false</code> otherwise.
     */
    public void setPolarity(boolean flag) {
        polarity = flag;
    }

    /**
     * Accessor that sets the mask value or mask red value.
     *
     * @param  scale  Value to set mask value to (must be between min and max of image).
     */
    public void setValue(float scale) {
        value = scale;
    }

    /**
     * Accessor that sets the mask blue value.
     *
     * @param  scale  Blue value to set mask value to (must be between min and max of image).
     */
    public void setValueB(float scale) {
        valueB = scale;
    }

    /**
     * Accessor that sets the mask green value.
     *
     * @param  scale  Green value to set mask value to (must be between min and max of image).
     */
    public void setValueG(float scale) {
        valueG = scale;
    }

    /**
     * Once all the necessary variables are set, call the Mask algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        if (displayLoc == NEW) {

            try {

                // Make result image of source type
                resultImage = new ModelImage(image.getType(), image.getExtents(),
                                             makeImageName(image.getImageName(), "_mask"));

                // Make algorithm
                if (image.isColorImage()) {
                    maskAlgo = new AlgorithmMask(resultImage, image, value, valueG, valueB, polarity, useVOI);
                } else if (image.isComplexImage()) {
                	maskAlgo = new AlgorithmMask(resultImage, image, value, valueG, polarity, useVOI);	
                } else {
                    maskAlgo = new AlgorithmMask(resultImage, image, value, polarity, useVOI);
                }
                if ( !useVOI )
                {
                    maskAlgo.setMask(image.getMask());
                }

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                maskAlgo.addListener(this);

                createProgressBar(image.getImageName(), maskAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (maskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    maskAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog mask: unable to allocate enough memory");

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
                if (image.isColorImage()) {
                    maskAlgo = new AlgorithmMask(image, value, valueG, valueB, polarity, useVOI);
                } else if (image.isComplexImage()) {
                	maskAlgo = new AlgorithmMask(image, value, valueG, polarity, useVOI);	
                } else {
                    maskAlgo = new AlgorithmMask(image, value, polarity, useVOI);
                }
                if ( !useVOI )
                {
                    maskAlgo.setMask(image.getMask());
                }

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                maskAlgo.addListener(this);

                createProgressBar(image.getImageName(), maskAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (maskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    maskAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog mask: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * method is meant to be used when the algorithm needs to be performed non-interactively, hence the hardcoding of
     * the variables.
     *
     * @param  pol  DOCUMENT ME!
     */
    protected void callAlgorithmNonInteractive(boolean pol) {
    	
    	if (Preferences.is(Preferences.PREF_QUICK_MASK_NEW)) {
    		displayLoc = NEW;
    	} else {
    		displayLoc = REPLACE;
    	}
        polarity = pol;
        value = 0;
        callAlgorithm();
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
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        setMinMax();

        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setPolarity(scriptParameters.getParams().getBoolean("do_fill_interior"));
        setValue(scriptParameters.getParams().getFloat("fill_value_or_red_if_color"));
        setValueG(scriptParameters.getParams().getFloat("fill_value_green"));
        setValueB(scriptParameters.getParams().getFloat("fill_value_blue"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_fill_interior", polarity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value_or_red_if_color", value));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value_green", valueG));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value_blue", valueB));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Fill image");

        JLabel labelValue;
        JLabel labelValueG = null;
        JLabel labelValueB = null;

        if (image.isColorImage()) {
            labelValue = new JLabel("Red value used to fill VOI(s)");
        } else if (image.isComplexImage()) {
        	labelValue = new JLabel("Real value used to fill VOI(s)");
        } else {
            labelValue = new JLabel("Value used to fill VOI(s)");
        }

        labelValue.setForeground(Color.black);
        labelValue.setFont(serif12);

        textValue = new JTextField(10);
        textValue.setText("0");
        textValue.setFont(serif12);

        if (image.isColorImage()) {
            labelValueG = new JLabel("Green value used to fill VOI(s)");
            labelValueG.setForeground(Color.black);
            labelValueG.setFont(serif12);

            textValueG = new JTextField(10);
            textValueG.setText("0");
            textValueG.setFont(serif12);

            labelValueB = new JLabel("Blue value used to fill VOI(s)");
            labelValueB.setForeground(Color.black);
            labelValueB.setFont(serif12);

            textValueB = new JTextField(10);
            textValueB.setText("0");
            textValueB.setFont(serif12);
        } // if (image.isColorImage())
        else if (image.isComplexImage()) {
        	labelValueG = new JLabel("Imaginary value used to fill VOI(s)");
            labelValueG.setForeground(Color.black);
            labelValueG.setFont(serif12);

            textValueG = new JTextField(10);
            textValueG.setText("0");
            textValueG.setFont(serif12);	
        }

        ButtonGroup fillGroup = new ButtonGroup();
        interiorFill = new JRadioButton("Interior fill", false);
        interiorFill.setFont(serif12);
        fillGroup.add(interiorFill);

        exteriorFill = new JRadioButton("Exterior fill", true);
        exteriorFill.setFont(serif12);
        fillGroup.add(exteriorFill);

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        GridBagConstraints gbc = new GridBagConstraints();

        JPanel maskPanel = new JPanel(new GridBagLayout());
        maskPanel.setBorder(buildTitledBorder("Parameters"));

        int yPos = 0;
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 5, 0, 5);
        maskPanel.add(textValue, gbc);
        gbc.gridx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(labelValue, gbc);

        if ((image.isColorImage()) || (image.isComplexImage())) {
            gbc.gridy = yPos++;
            gbc.gridx = 0;
            gbc.gridwidth = 1;
            gbc.weightx = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            maskPanel.add(textValueG, gbc);
            gbc.gridx = 1;
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.NONE;
            maskPanel.add(labelValueG, gbc);
        }
        if (image.isColorImage()) {
            gbc.gridy = yPos++;
            gbc.gridx = 0;
            gbc.gridwidth = 1;
            gbc.weightx = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            maskPanel.add(textValueB, gbc);
            gbc.gridx = 1;
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.NONE;
            maskPanel.add(labelValueB, gbc);
        }

        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.weightx = 1;
        maskPanel.add(interiorFill, gbc);
        gbc.gridy = yPos++;
        maskPanel.add(exteriorFill, gbc);

        JPanel destinationPanel = new JPanel(new GridBagLayout());
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        destinationPanel.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanel.add(replaceImage, gbc);

        // JPanel buttonPanel = new JPanel();
        // buildOKButton();
        // buttonPanel.add(OKButton);
        // buildCancelButton();
        // buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        maskPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        destinationPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainPanel.add(maskPanel);
        mainPanel.add(destinationPanel);
        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Sets the min max values based on image type.
     */
    private void setMinMax() {

        if (image.getType() == ModelStorageBase.BOOLEAN) {
            min = 0;
            max = 1;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            min = -128;
            max = 127;
        } else if ((image.getType() == ModelStorageBase.UBYTE) || (image.getType() == ModelStorageBase.ARGB)) {
            min = 0;
            max = 255;
        } else if (image.getType() == ModelStorageBase.SHORT) {
            min = -32768;
            max = 32767;
        } else if ((image.getType() == ModelStorageBase.USHORT) || (image.getType() == ModelStorageBase.ARGB_USHORT)) {
            min = 0;
            max = 65535;
        } else if (image.getType() == ModelStorageBase.INTEGER) {
            min = Integer.MIN_VALUE;
            max = Integer.MAX_VALUE;
        } else if (image.getType() == ModelStorageBase.UINTEGER) {
            min = 0;
            max = 4294967295L;
        } else if (image.getType() == ModelStorageBase.LONG) {
            min = Long.MIN_VALUE;
            max = Long.MAX_VALUE;

        } else if ((image.getType() == ModelStorageBase.FLOAT) || (image.getType() == ModelStorageBase.ARGB_FLOAT) ||
        		   (image.getType() == ModelStorageBase.COMPLEX)) {
            min = -Float.MAX_VALUE;
            max = Float.MAX_VALUE;
        } else if ((image.getType() == ModelStorageBase.DOUBLE) || (image.getType() == ModelStorageBase.DCOMPLEX)) {
            min = -Double.MAX_VALUE;
            max = Double.MAX_VALUE;
        }
    }


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        tmpStr = textValue.getText();

        if (testParameter(tmpStr, min, max)) {
            value = Float.valueOf(tmpStr).floatValue();
        } else {
            textValue.requestFocus();
            textValue.selectAll();

            return false;
        }

        if ((image.isColorImage()) || (image.isComplexImage())) {
            tmpStr = textValueG.getText();

            if (testParameter(tmpStr, min, max)) {
                valueG = Float.valueOf(tmpStr).floatValue();
            } else {
                textValueG.requestFocus();
                textValueG.selectAll();

                return false;
            }
        }
        
        if (image.isColorImage()) {

            tmpStr = textValueB.getText();

            if (testParameter(tmpStr, min, max)) {
                valueB = Float.valueOf(tmpStr).floatValue();
            } else {
                textValueB.requestFocus();
                textValueB.selectAll();

                return false;
            }

        } // if (image.isColorImage())

        if (interiorFill.isSelected()) {
            polarity = true;
        } else {
            polarity = false;
        }

        return true;
    }

}
