package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
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
 * @version  1.0 Nov 9, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogThreshold extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8143576591075984708L;

    /** DOCUMENT ME! */
    private static final int DEFAULT = 0;

    /** DOCUMENT ME! */
    private static final int OTSU = 1;

    /** DOCUMENT ME! */
    private static final int MAX_ENT = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private float fillValue;

    /** DOCUMENT ME! */
    private ModelHistogram histogram;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JCheckBox inverseCheckbox;

    /** DOCUMENT ME! */
    private boolean isInverse = true;

    /** DOCUMENT ME! */
    private JLabel labelThres2;

    /** DOCUMENT ME! */
    private JCheckBox maxEntCheckbox;

    /** DOCUMENT ME! */
    private float min, max;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JCheckBox otsuCheckbox;

    /** DOCUMENT ME! */
    private double outMax = 0;

    /** DOCUMENT ME! */
    private double outMin = 0;

    /** Holds the choices for the outputType. */
    private JComboBox outputBox;


    /** Same data-type, binary, or unsigned byte mask. */
    private int outputType = 0;

    /** or if the source image is to be replaced. */
    private boolean regionFlag; // true = apply algorithm to the whole image

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textFill;

    /** DOCUMENT ME! */
    private JTextField textThres1;

    /** DOCUMENT ME! */
    private JTextField textThres2;

    /** DOCUMENT ME! */
    private float thres1 = 0.0f;

    /** DOCUMENT ME! */
    private float thres2 = 0.0f;

    /** DOCUMENT ME! */
    private AlgorithmThresholdDual thresholdAlgo;

    /** DOCUMENT ME! */
    private int thresholdType = DEFAULT; // 0 = default, 1 = otsu, 2 = max entropy

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogThreshold() { }

    /**
     * Creates a new JDialogThreshold object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogThreshold(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        min = (float) im.getMin();
        max = (float) im.getMax();
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
        } 
        else if (command.equals("Cancel")) {
            dispose();
        }
        else if (command.equals("Help")) {
            //MipavUtil.showHelp("19074");
            MipavUtil.showWebHelp("Threshold");
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

        if (algorithm instanceof AlgorithmThresholdDual) {
            image.clearMask();

            if ((thresholdAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                if ((outputType == AlgorithmThresholdDual.BINARY_TYPE) ||
                        (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE)) {
                    updateFileInfoOtherModality(image, resultImage);
                } else {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

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

        thresholdAlgo.finalize();
        thresholdAlgo = null;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    public void calcHistogram() {
        int[] dimExtents = new int[1];
        //int type = image.getType();

        dimExtents[0] = 256;
        
   //     if ((type == ModelStorageBase.UBYTE) || (type == ModelStorageBase.BYTE)) {
   //         dimExtents[0] = 256;
   //     } else if ((type != ModelStorageBase.FLOAT) && (type != ModelStorageBase.DOUBLE) &&
   //                    (type != ModelStorageBase.COMPLEX)) {
   //         dimExtents[0] = (int) (image.getMax() - image.getMin() + 0.5) + 1;
//
   //         if (dimExtents[0] < 256) {
    //            dimExtents[0] = 256;
   //         }
   //     } else {
   //         dimExtents[0] = 256;
   //     }

        histogram = new ModelHistogram(ModelStorageBase.INTEGER, dimExtents);

        AlgorithmHistogram histoAlgoA = new AlgorithmHistogram(histogram, image, true);
        histoAlgoA.setRunningInSeparateThread(false);
        histoAlgoA.run();
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
     * Sets the fill value text box to enabled or disabled depending on if the binary checkbox is selected.
     *
     * @param  event  Event that cause the method to fire.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == outputBox) {

            if (outputBox.getSelectedIndex() == AlgorithmThresholdDual.ORIGINAL_TYPE) {
                textFill.setEnabled(true);
            } else {
                textFill.setEnabled(false);
            }
        } else if (source == otsuCheckbox || source == maxEntCheckbox) {

            if (otsuCheckbox.isSelected() || maxEntCheckbox.isSelected()) {

                if ((source == otsuCheckbox) && maxEntCheckbox.isSelected()) {
                    maxEntCheckbox.removeItemListener(this);
                    maxEntCheckbox.setSelected(false);
                    maxEntCheckbox.addItemListener(this);
                } else if ((source == maxEntCheckbox) && otsuCheckbox.isSelected()) {
                	otsuCheckbox.removeItemListener(this);
                    otsuCheckbox.setSelected(false);
                    otsuCheckbox.addItemListener(this);
                }

                if (maxEntCheckbox.isSelected()) {
                	thresholdType = MAX_ENT;
                } else if (otsuCheckbox.isSelected()) {
                	thresholdType = OTSU;
                }
                
                
                if (histogram == null) {
                    calcHistogram();
                }
                
                

                int maxBin = 0;
                if (thresholdType == OTSU) {
                	maxBin = histogram.getOtsuThreshold();
                } else {
                	maxBin = histogram.getMaxEntropyThreshold();
                }
                
            	double dif = image.getMax() - image.getMin();
            	
            	double factor = dif / histogram.getExtents()[0];
            	            	
            	double thresVal = ((maxBin * factor) + image.getMin());
                
                if (inverseCheckbox.isSelected()) {
                    textThres1.setText(new Double(thresVal).toString());
                    textThres2.setText(new Double(image.getMax()).toString());
                } else {
                    textThres1.setText(new Double(image.getMin()).toString());
                    textThres2.setText(new Double(thresVal).toString());
                }

                textThres1.setEnabled(false);
                textThres2.setEnabled(false);
            } else {
                thresholdType = DEFAULT;
                textThres1.setEnabled(true);
                textThres2.setEnabled(true);
            }

            textThres1.setCaretPosition(0);
            textThres2.setCaretPosition(0);
        } 

    }

    /**
     * Function for setting up/running the threshold algorithm from the ViewJFrameHistoLUT.
     *
     * @param  im           ModelImage image
     * @param  lowerThres   float lower threshold
     * @param  upperThres   float upper threshold
     * @param  fillV        float fill value
     * @param  output_type  output type: same, binary, unsigned byte mask
     * @param  inverseFlag  normal or inverse threshold
     */
    public void runFromLUTFrame(ModelImage im, float lowerThres, float upperThres, float fillV, int output_type,
                                boolean inverseFlag) {
        this.image = im;
        this.userInterface = ViewUserInterface.getReference();
        this.thres1 = lowerThres;
        this.thres2 = upperThres;
        this.fillValue = fillV;
        this.isInverse = inverseFlag;
        this.outputType = output_type;
        regionFlag = true;
        setDisplayLocNew();
        setSeparateThread(true);
        callAlgorithm();
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
     * Accessor that sets the fill value. Will not be used if writing out a binary image.
     *
     * @param  scale  Value to set fill value to.
     */
    public void setFillValue(float scale) {
        fillValue = scale;
    }

    /**
     * Accessor that sets the output type.
     *
     * @param  output_type  0 is normal, 1 is binary, 2 is unsigned byte mask
     */
    public void setOutputType(int output_type) {
        outputType = output_type;
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
     * Accessor that sets the first threshold.
     *
     * @param  threshold  Value to set threshold to (between min and max of image).
     */
    public void setThres1(float threshold) {
        thres1 = threshold;
    }

    /**
     * Accessor that sets the second threshold.
     *
     * @param  threshold  Value to set threshold to (between min and max of image).
     */
    public void setThres2(float threshold) {
        thres2 = threshold;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        String name = makeImageName(image.getImageName(), "_threshold");
        float[] thresholds = new float[2];
        int[] destExtents = null;
        thresholds[0] = thres1;
        thresholds[1] = thres2;

        int end;

        // System.err.println("Name is: " + name + " lower thresh: " + thresholds[0] + " upper thresh: " +
        // thresholds[1]);

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
            end = 1;
        } else if (image.getNDims() == 3) {
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
            end = destExtents[2];
        } else { // Dims = 4
            destExtents = new int[4];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
            destExtents[3] = image.getExtents()[3];
            end = destExtents[2] * destExtents[3];
        }

        if (displayLoc == NEW) {

            try {

                if (outputType == AlgorithmThresholdDual.BINARY_TYPE) {
                    resultImage = new ModelImage(ModelImage.BOOLEAN, destExtents, name);
                } else {

                    // resultImage      = new ModelImage(image.getType(), destExtents, " Threshold", userInterface);
                    resultImage = (ModelImage) image.clone();

                    // reallocate to UBYTE type is doing a unsigned byte mask
                    if (outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE) {
                        resultImage.reallocate(ModelStorageBase.UBYTE);
                    }

                    resultImage.setImageName(name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                        for (int i = 0; i < end; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }
                }

                if ((outputType == AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE) ||
                        (outputType == AlgorithmThresholdDual.BINARY_TYPE)) {

                    for (int i = 0; i < end; i++) {
                        (resultImage.getFileInfo(i)).setModality(FileInfoBase.OTHER);
                    }
                }

                // Make algorithm
                thresholdAlgo = new AlgorithmThresholdDual(resultImage, image, thresholds, fillValue, outputType,
                                                           regionFlag, isInverse);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                thresholdAlgo.addListener(this);

                createProgressBar(image.getImageName(), thresholdAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (thresholdAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    thresholdAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

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
                thresholdAlgo = new AlgorithmThresholdDual(image, thresholds, fillValue, outputType, regionFlag,
                                                           isInverse);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                thresholdAlgo.addListener(this);

                createProgressBar(image.getImageName(), thresholdAlgo);

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

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (thresholdAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    thresholdAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * {@inheritDoc}
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

        min = (float) image.getMin();
        max = (float) image.getMax();

        if (scriptParameters.doOutputNewImage()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
        }

        regionFlag = scriptParameters.doProcessWholeImage();
        thresholdType = scriptParameters.getParams().getInt("threshold_type");

        setFillValue(scriptParameters.getParams().getFloat("fill_value"));
        setOutputType(scriptParameters.getParams().getInt("output_type"));
        isInverse = scriptParameters.getParams().getBoolean("is_inverse_threshold");

        if (thresholdType == DEFAULT) {
            setThres1(scriptParameters.getParams().getFloat("min_threshold"));
            setThres2(scriptParameters.getParams().getFloat("max_threshold"));
        } else {

            // using otsu or max entropy threshold, so calculate histogram
            calcHistogram();
            int maxBin = 0;
            if (thresholdType == OTSU) {
            	maxBin = histogram.getOtsuThreshold();
            } else {
            	maxBin = histogram.getMaxEntropyThreshold();
            }
        	double dif = image.getMax() - image.getMin();
        	double factor = dif / histogram.getNDims();
        	
        	float thresVal = (float)((maxBin * factor) + image.getMin());
            
            
            if (isInverse) {
            	setThres1(thresVal);
                
                setThres2((float) image.getMax());
            } else {
                setThres1((float) image.getMin());
                setThres2(thresVal);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, displayLoc == NEW);

        scriptParameters.storeProcessWholeImage(regionFlag);
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_type", thresholdType));

        if (thresholdType == DEFAULT) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("min_threshold", thres1));
            scriptParameters.getParams().put(ParameterFactory.newParameter("max_threshold", thres2));
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("output_type", outputType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value", fillValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("is_inverse_threshold", isInverse));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   imageType  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String getRangeString(int imageType) {

        String fillString = new String("Values outside threshold ");

        if (imageType == ModelStorageBase.BOOLEAN) {
            fillString += "(0 or 1):";
            outMin = 0;
            outMax = 1;
        } else if (imageType == ModelStorageBase.BYTE) {
            fillString += "(-128 to 127):";
            outMin = -128;
            outMax = 127;
        } else if (imageType == ModelStorageBase.UBYTE) {
            fillString += "(0 to 255):";
            outMin = 0;
            outMax = 255;
        } else if (imageType == ModelStorageBase.SHORT) {
            fillString += "(-32768 to 32767):";
            outMin = -32768;
            outMax = 32767;
        } else if (imageType == ModelStorageBase.USHORT) {
            fillString += "(0 to 65535):";
            outMin = 0;
            outMax = 65535;
        } else if (imageType == ModelStorageBase.INTEGER) {
            fillString += "(-2.147 E+9 to 2.147 E+9):";
            outMin = Integer.MIN_VALUE;
            outMax = Integer.MAX_VALUE;
        } else if (imageType == ModelStorageBase.UINTEGER) {
            fillString += "(0 to 4.29 E+9):";
            outMin = 0;
            outMax = 4294967295L;
        } else if (imageType == ModelStorageBase.LONG) {
            fillString += "(-9.22 E+18 to 9.22 E+18):";
            outMin = Long.MIN_VALUE;
            outMax = Long.MAX_VALUE;
        } else if (imageType == ModelStorageBase.FLOAT) {
            fillString += "(-3.40 E+38  to 3.40 E+38):";
            outMin = -Float.MAX_VALUE;
            outMax = Float.MAX_VALUE;
        } else if (imageType == ModelStorageBase.DOUBLE) {
            fillString += "(-1.8 E+308 to 1.8 E+308):";
            outMin = -Double.MAX_VALUE;
            outMax = Double.MAX_VALUE;
        }

        return fillString;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	//setForeground(Color.black);
        setTitle("Threshold");
        //setResizable(false);

        JPanel scalePanel = new JPanel(new GridBagLayout());
        scalePanel.setBorder(buildTitledBorder("Thresholds"));

        String tempStr = new String("Lower limit ( " + makeString(min, 6) + " - " + makeString(max, 6) + " ):");
        JLabel labelThres1 = new JLabel(tempStr);
        labelThres1.setForeground(Color.black);
        labelThres1.setFont(serif12);

        textThres1 = new JTextField(6);
        //textThres1.setText(makeString((max + min) / 3, 6));
        textThres1.setText(makeString(min, 6));
        textThres1.setFont(serif12);
        textThres1.setCaretPosition(0);

        textThres1.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke("ENTER"), "checkValue");
        textThres1.getActionMap().put("checkValue", new CheckValueAction());

        tempStr = new String("Upper limit ([lower limit] - " + makeString(max, 6) + " ).");

        labelThres2 = new JLabel(tempStr);
        labelThres2.setForeground(Color.black);
        labelThres2.setFont(serif12);

        textThres2 = new JTextField(6);
        //textThres2.setText(makeString((max + min) / 1.5f, 6));
        textThres2.setText(makeString(max, 6));
        textThres2.setFont(serif12);
        textThres2.setCaretPosition(0);

        textThres2.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke("ENTER"), "checkValue");
        textThres2.getActionMap().put("checkValue", new CheckValueAction());

        tempStr = new String("Output image type:");

        JLabel outputLabel = new JLabel(tempStr);
        outputLabel.setForeground(Color.black);
        outputLabel.setFont(serif12);

        String[] outputChoices = new String[] { image.getTypeString(), "Binary", "Unsigned Byte" };

        outputBox = new JComboBox(outputChoices);
        outputBox.setFont(MipavUtil.font12);
        outputBox.addItemListener(this);


        inverseCheckbox = new JCheckBox("Inverse threshold");
        inverseCheckbox.setFont(serif12);
        inverseCheckbox.setSelected(true);
        inverseCheckbox.addItemListener(this);

        otsuCheckbox = new JCheckBox("Otsu threshold");
        otsuCheckbox.setFont(serif12);
        otsuCheckbox.setSelected(false);
        otsuCheckbox.addItemListener(this);

        maxEntCheckbox = new JCheckBox("Maximum entropy threshold");
        maxEntCheckbox.setFont(serif12);
        maxEntCheckbox.setSelected(false);
        maxEntCheckbox.addItemListener(this);

        JLabel labelFill = new JLabel(getRangeString(image.getType()));
        labelFill.setForeground(Color.black);
        labelFill.setFont(serif12);

        textFill = new JTextField(5);
        textFill.setText(makeString(min, 6));
        textFill.setFont(serif12);

        if (outputBox.getSelectedIndex() == AlgorithmThresholdDual.ORIGINAL_TYPE) {
            textFill.setEnabled(true);
        } else {
            textFill.setEnabled(false);
        }

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.5f;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);
        scalePanel.add(labelThres1, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        scalePanel.add(textThres1, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.NONE;
        scalePanel.add(labelThres2, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        scalePanel.add(textThres2, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        scalePanel.add(labelFill, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        scalePanel.add(textFill, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        scalePanel.add(outputLabel, gbc);

        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        scalePanel.add(outputBox, gbc);

        gbc.gridx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridy = 4;
        scalePanel.add(otsuCheckbox, gbc);
        gbc.gridy = 5;
        scalePanel.add(maxEntCheckbox, gbc);
        gbc.gridy = 6;
        scalePanel.add(inverseCheckbox, gbc);

        JPanel destinationPanel = new JPanel(new GridBagLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setBounds(10, 42, 120, 25);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        destinationPanel.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanel.add(replaceImage, gbc);

        JPanel imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Threshold"));

        ButtonGroup imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        gbc.gridx = 0;
        gbc.gridy = 0;
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        mainPanel.add(scalePanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(destinationPanel, gbc);
        gbc.gridx = 1;
        mainPanel.add(imageVOIPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());
        //buildOKButton();
        //buttonPanel.add(OKButton);
        //buildCancelButton();
        //buttonPanel.add(cancelButton);

        mainDialogPanel.add(mainPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

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

        tmpStr = textThres1.getText();

        if (testParameter(tmpStr, min - 1, max + 1)) {
            thres1 = Float.valueOf(tmpStr).floatValue();
        } else {
            textThres1.requestFocus();
            textThres1.selectAll();

            return false;
        }

        tmpStr = textThres2.getText();

        if (testParameterMin(tmpStr, thres1 - 1)) {
        	if(Float.valueOf(tmpStr).floatValue() > max) {
        		thres2 = max;
        	}else {
        		thres2 = Float.valueOf(tmpStr).floatValue();
        	}
        } else {
        	
        	textThres2.requestFocus();
            textThres2.selectAll();

            return false;
        }

        outputType = outputBox.getSelectedIndex();

        if (outputBox.getSelectedIndex() == AlgorithmThresholdDual.ORIGINAL_TYPE) {
            tmpStr = textFill.getText();

            if (testParameter(tmpStr, outMin, outMax)) {
                fillValue = Float.valueOf(tmpStr).floatValue();
            } else {
                textFill.requestFocus();
                textFill.selectAll();

                return false;
            }
        }

        isInverse = inverseCheckbox.isSelected();

        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    protected class CheckValueAction extends AbstractAction {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -1531686009578681460L;

        /**
         * DOCUMENT ME!
         *
         * @param  event  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent event) {
            JTextField field = (JTextField) event.getSource();
            String text = field.getText();

            try {
                Float.parseFloat(textThres1.getText());
            } catch (Exception ex) {
                MipavUtil.displayError("Please enter a number.");
                textThres1.requestFocus();
                textThres1.selectAll();

                return;
            }

            try {
                Float.parseFloat(textThres2.getText());
            } catch (Exception ex) {
                MipavUtil.displayError("Please enter a number.");
                textThres2.requestFocus();
                textThres2.selectAll();

                return;
            }

            if (field == textThres1) {

                if (!testParameter(text, min, max)) {
                    textThres1.requestFocus();
                    textThres1.selectAll();
                }
            } else {

                if (!testParameterMin(text, Float.parseFloat(textThres1.getText()))) {
                    textThres2.requestFocus();
                    textThres2.selectAll();
                }
            }
        }
    }

}
