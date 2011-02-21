package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;


/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogMSFuzzyCMeans extends JDialogScriptableBase implements AlgorithmInterface, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5469772983205216898L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmMSpectralFuzzyCMeans afcmAlgo;

    /** DOCUMENT ME! */
    private float[] centroids;

    /** DOCUMENT ME! */
    private boolean changeRemoveIndex = true;

    /** DOCUMENT ME! */
    private JButton chooserButton;

    /** DOCUMENT ME! */
    private JPanelColorChannels colorPanel;

    /** DOCUMENT ME! */
    private boolean cropBackground;

    /** DOCUMENT ME! */
    private JCheckBox cropCheckbox;

    /** DOCUMENT ME! */
    private int[] destExtents;

    /** DOCUMENT ME! */
    private float endTol;

    /** DOCUMENT ME! */
    private JRadioButton fuzzyOnly;

    /** DOCUMENT ME! */
    private JRadioButton hardFuzzyBoth;

    /** DOCUMENT ME! */
    private JRadioButton hardOnly;

    /** DOCUMENT ME! */
    private JList imageList;

    /** DOCUMENT ME! */
    private JPanel imagePanel;

    /** DOCUMENT ME! */
    private ButtonGroup imageVOIGroup;

    /** DOCUMENT ME! */
    private JPanel imageVOIPanel;

    /** DOCUMENT ME! */
    private JLabel labelEndTol;

    /** DOCUMENT ME! */
    private JLabel labelExpo;

    /** DOCUMENT ME! */
    private JLabel labelMaxIter;

    /** DOCUMENT ME! */
    private JLabel labelNClasses;

    /** DOCUMENT ME! */
    private int maxIter;

    /** DOCUMENT ME! */
    private DefaultListModel model;

    /** DOCUMENT ME! */
    private int nClasses;

    /** DOCUMENT ME! */
    private int nPyramid = 4;

    /** DOCUMENT ME! */
    private int oneJacobiIter = 2;

    /** DOCUMENT ME! */
    private float oneSmooth = 5e4f;

    /** private JCheckBox calcGainFieldCheckbox;. */
    private boolean outputGainField = false;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private int presentNumber;

    /** DOCUMENT ME! */
    private float q;

    /** DOCUMENT ME! */
    private boolean regionFlag; // true = apply algorithm to the whole image

    /** DOCUMENT ME! */
    private JButton removeButton;

    /** DOCUMENT ME! */
    private int removeIndex;

    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private int resultNumber;

    /** DOCUMENT ME! */
    private int segmentation;

    /** DOCUMENT ME! */
    private ButtonGroup segmentationGroup;

    /** DOCUMENT ME! */
    private JPanel segmentationPanel;

    /** DOCUMENT ME! */
    private ModelImage[] srcImage = null; // all source images

    /** DOCUMENT ME! */
    private int srcNumber = 1;

    /** DOCUMENT ME! */
    private ModelImage[] tempImage = null;

    /** DOCUMENT ME! */
    private JTextField textEndTol;

    /** DOCUMENT ME! */
    private JTextField textExpo;

    /** DOCUMENT ME! */
    private JTextField textMaxIter;

    /** DOCUMENT ME! */
    private JTextField textNClasses;

    /** DOCUMENT ME! */
    private float[] threshold;

    /** DOCUMENT ME! */
    private int twoJacobiIter = 3;

    /** DOCUMENT ME! */
    private float twoSmooth = 5e5f;

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
    public JDialogMSFuzzyCMeans() { }

    // false = apply algorithm only to VOI regions

    /**
     * Creates a new JDialogMSFuzzyCMeans object.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogMSFuzzyCMeans(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = new ModelImage[1];
        srcImage[0] = im;
        userInterface = ViewUserInterface.getReference();

        if (srcImage[0].getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = srcImage[0].getExtents()[0]; // X dim
            destExtents[1] = srcImage[0].getExtents()[1]; // Y dim
        } else { // srcImage[0].getNDims)() == 3
            destExtents = new int[3];
            destExtents[0] = srcImage[0].getExtents()[0];
            destExtents[1] = srcImage[0].getExtents()[1];
            destExtents[2] = srcImage[0].getExtents()[2];
        }

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

        int i, j;

        if (command.equals("Choose")) {
            ModelImage newImage = open();

            if (!checkImage(newImage)) {
                return;
            }

            srcNumber++;
            tempImage = new ModelImage[srcNumber - 1];

            for (i = 0; i < (srcNumber - 1); i++) {
                tempImage[i] = srcImage[i];
            }

            srcImage = null;
            srcImage = new ModelImage[srcNumber];

            for (i = 0; i < (srcNumber - 1); i++) {
                srcImage[i] = tempImage[i];
            }

            tempImage = null;
            srcImage[srcNumber - 1] = newImage;
            model.addElement(srcImage[srcNumber - 1].getImageName());

            newImage = null;
            removeButton.setEnabled(true);
        } // if (command.equals("Choose"))
        else if ((command.equals("Remove")) && (removeIndex == 0)) {

            // Cannot remove original image
            MipavUtil.displayError("Cannot remove original loaded image");
        } else if ((command.equals("Remove")) && (removeIndex >= 1) && (removeIndex <= (srcNumber - 1))) {

            // changeRemoveIndex = false is needed because the model.removeElement
            // line causes valueChanged to execute.  Without changeRemoveIndex an
            // unselected element causes removeIndex to be changed to -1.
            changeRemoveIndex = false;
            model.removeElement(srcImage[removeIndex].getImageName());
            tempImage = new ModelImage[srcNumber - 1];

            for (i = 0, j = 0; i < srcNumber; i++) {

                if (i != removeIndex) {
                    tempImage[j++] = srcImage[i];
                }
            } // for ( i = 0, j=0; i < srcNumber; i++)

            srcImage[removeIndex].disposeLocal();
            srcImage[removeIndex] = null;
            changeRemoveIndex = true;
            srcImage = null;
            srcImage = new ModelImage[srcNumber - 1];

            for (i = 0; i < (srcNumber - 1); i++) {
                srcImage[i] = tempImage[i];
            }

            tempImage = null;
            srcNumber--;

            if (srcNumber == 1) {
                removeButton.setEnabled(false);
            }
        } // else if ((command.equals("Remove"))  && (removeIndex >= 1) && (removeIndex <= (srcNumber - 1)))
        else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Crop")) {

            if (cropCheckbox.isSelected()) {
                wholeImage.setEnabled(false);
                VOIRegions.setEnabled(false);
                wholeImage.setSelected(true);
                VOIRegions.setSelected(false);
            } else {
                wholeImage.setEnabled(true);
                VOIRegions.setEnabled(true);
            }
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10026");
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

        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[resultNumber];

        if (algorithm instanceof AlgorithmMSpectralFuzzyCMeans) {
            srcImage[0].clearMask();

            if ((afcmAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                // Take resultImage out of array form or null pointer errors can
                // result in one of the resultImages after another of the resultImages
                // has been deleted.
                for (i = 0; i < resultNumber; i++) {
                    updateFileInfo(srcImage[0], resultImage[i]);
                    resultImage[i].clearMask();

                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200 + (i * 20)));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new resultImage0 frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }

                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the centroids.
     *
     * @param  centroids  Value to set centroids to.
     */
    public void setCentroids(float[] centroids) {
        this.centroids = centroids;
    }

    /**
     * Accessor that sets the crop background flag.
     *
     * @param  flag  <code>true</code> indicates crop the background, <code>false</code> otherwise.
     */
    public void setCrop(boolean flag) {
        cropBackground = flag;
    }

    /**
     * Accessor that sets the end tol.
     *
     * @param  scale  Value to set end tol to.
     */
    public void setEndTol(float scale) {
        endTol = scale;
    }

    /**
     * Accessor that sets the max iterations.
     *
     * @param  max  The max iterations
     */
    public void setMaxIter(int max) {
        maxIter = max;
    }

    /**
     * Accessor that sets the number of classes.
     *
     * @param  classes  The number of classes.
     */
    public void setNClasses(int classes) {
        nClasses = classes;
    }

    /**
     * Accessor that sets the q variable.
     *
     * @param  scale  Value to set q variable to.
     */
    public void setQ(float scale) {
        q = scale;
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
     * Accessor that sets the segmentation type (BOTH_FUZZY_HARD, FUZZY, or HARD).
     *
     * @param  type  The segmentation type.
     */
    public void setSegmentationType(int type) {
        segmentation = type;
    }

    /**
     * Accessor that sets the array of source images.
     *
     * @param  images  new source images.
     */
    public void setSourceImage(ModelImage[] images) {
        srcImage = images;
    }

    /**
     * Accessor that sets the threshold.
     *
     * @param  threshold  Value to set threshold to.
     */
    public void setThreshold(float[] threshold) {
        this.threshold = threshold;
    }

    /**
     * Sets the remove index based on the selected index in the list.
     *
     * @param  evt  Event that caused this method to fire.
     */
    public void valueChanged(ListSelectionEvent evt) {

        if (changeRemoveIndex) {
            JList source = (JList) evt.getSource();
            removeIndex = source.getSelectedIndex();
        }
    }

    /**
     * Once all the necessary variables are set, call the Fuzzy C Means algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int i;
        System.gc();

        // Calculate the number of result images.
        resultNumber = 0;

        if ((segmentation == AlgorithmMSpectralFuzzyCMeans.HARD_ONLY) ||
                (segmentation == AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD)) {
            resultNumber++; // segmented image
        } // if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD))

        if ((segmentation == AlgorithmMSpectralFuzzyCMeans.FUZZY_ONLY) ||
                (segmentation == AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD)) {

            // if (outputGainField) {
            // resultNumber++;
            // }
            resultNumber += nClasses;
        } // if ((segmentation == FUZZY_ONLY) || (segmentation == BOTH_FUZZY_HARD))

        try {
            resultImage = new ModelImage[resultNumber];
            presentNumber = 0;

            if ((segmentation == AlgorithmMSpectralFuzzyCMeans.FUZZY_ONLY) ||
                    (segmentation == AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD)) {

                for (i = 0; i < nClasses; i++) {
                    String name = makeImageName(srcImage[0].getImageName(), "_class" + (i + 1));

                    resultImage[presentNumber++] = new ModelImage(ModelStorageBase.FLOAT, destExtents, name);
                }
                /* if (outputGainField) {
                 *  resultImage[presentNumber++] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
                 * makeImageName(srcImage[0].getImageName(), "_mult"), userInterface);        } */
            }

            if ((segmentation == AlgorithmMSpectralFuzzyCMeans.HARD_ONLY) ||
                    (segmentation == AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD)) {
                resultImage[presentNumber++] = new ModelImage(ModelStorageBase.UBYTE, destExtents,
                                                              makeImageName(srcImage[0].getImageName(), "_seg"));
            }

            // Make algorithm
            afcmAlgo = new AlgorithmMSpectralFuzzyCMeans(resultImage, srcImage, nClasses, nPyramid, oneJacobiIter,
                                                         twoJacobiIter, q, oneSmooth, twoSmooth, outputGainField,
                                                         segmentation, cropBackground, maxIter, endTol,
                                                         colorPanel.isRedProcessingRequested(),
                                                         colorPanel.isGreenProcessingRequested(),
                                                         colorPanel.isBlueProcessingRequested(), regionFlag);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            afcmAlgo.addListener(this);


            createProgressBar(srcImage[0].getImageName(), afcmAlgo);


            if (regionFlag == false) {
                afcmAlgo.setMask(srcImage[0].generateVOIMask());
            }

            // if not previously set by script file, call the dialog to set now
            if ((centroids == null) && (threshold == null)) {

                // if returned false, dialog was cancelled
                if (!getCentroidsThreshold()) {
                    return;
                }
            }

            afcmAlgo.setCentroids(centroids);
            afcmAlgo.setThreshold(threshold);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (afcmAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                afcmAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog MS Fuzzy CMeans: unable to allocate enough memory");

            return;
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        for (int i = 0; i < resultNumber; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        int numInputImages = scriptParameters.getParams().getInt("number_of_input_images");
        srcImage = new ModelImage[numInputImages];

        for (int i = 1; i <= numInputImages; i++) {
            srcImage[i - 1] = scriptParameters.retrieveInputImage(i);
        }

        if (srcImage[0].getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = srcImage[0].getExtents()[0]; // X dim
            destExtents[1] = srcImage[0].getExtents()[1]; // Y dim
        } else { // srcImage[0].getNDims)() == 3
            destExtents = new int[3];
            destExtents[0] = srcImage[0].getExtents()[0];
            destExtents[1] = srcImage[0].getExtents()[1];
            destExtents[2] = srcImage[0].getExtents()[2];
        }

        userInterface = ViewUserInterface.getReference();
        parentFrame = srcImage[0].getParentFrame();

        resultNumber = scriptParameters.getParams().getInt("number_of_result_images");

        setRegionFlag(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE));
        setNClasses(scriptParameters.getParams().getInt("number_of_classes"));
        setQ(scriptParameters.getParams().getFloat("exponent_q"));
        setCrop(scriptParameters.getParams().getBoolean("do_crop_background"));
        setThreshold(scriptParameters.getParams().getList("thresholds").getAsFloatArray());
        setEndTol(scriptParameters.getParams().getFloat("end_tolerance"));
        setMaxIter(scriptParameters.getParams().getInt("max_iterations"));
        setSegmentationType(scriptParameters.getParams().getInt("segmentation_type"));
        setCentroids(scriptParameters.getParams().getList("centroids").getAsFloatArray());

        colorPanel = new JPanelColorChannels(srcImage[0]);
        scriptParameters.setColorOptionsGUI(colorPanel);

        for (int i = 0; i < srcImage.length; i++) {

            if (!checkImage(srcImage[i])) {
                return;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_input_images", srcImage.length));

        for (int i = 0; i < srcImage.length; i++) {
            scriptParameters.storeInputImage(srcImage[i]);
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_result_images", resultNumber));

        for (int i = 0; i < resultNumber; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }

        scriptParameters.storeProcessWholeImage(regionFlag);
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_classes", nClasses));
        scriptParameters.getParams().put(ParameterFactory.newParameter("exponent_q", q));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_crop_background", cropBackground));
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresholds", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("end_tolerance", endTol));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_iterations", maxIter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("segmentation_type", segmentation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("centroids", centroids));
        scriptParameters.storeColorOptions(colorPanel);
    }

    /**
     * Checks the color and dimensionality of the new image vs. the original source image. All new images should have
     * the same color modality as the source and be of the same dimensions.
     *
     * @param   testImage  DOCUMENT ME!
     *
     * @return  Flag indicating if the image checks out.
     */
    private boolean checkImage(ModelImage testImage) {

        if (testImage == null) {
            return false;
        }

        if ((srcImage[0].isColorImage() == true) && (testImage.isColorImage() == false)) {
            MipavUtil.displayError("Cannot load a color (" + testImage.getImageName() +
                                   ") unless the original file is color.");

            return false;
        }

        if (srcImage[0].getNDims() != testImage.getNDims()) {
            MipavUtil.displayError("Error! " + srcImage[0].getImageName() + " is " + srcImage[0].getNDims() +
                                   "D, while " + testImage.getImageName() + " is " + testImage.getNDims() + "D");

            return false;
        }

        for (int i = 0; i < srcImage[0].getNDims(); i++) {

            if ((testImage != null) && (destExtents[i] != testImage.getExtents()[i])) {
                MipavUtil.displayError("Error! For dimension = " + i + " " + srcImage[0].getImageName() +
                                       " has length = " + destExtents[i] + " while " + testImage.getImageName() +
                                       " has length = " + testImage.getExtents()[i]);

                return false;
            }
        }

        return true;

    }

    /**
     * Gets the minimum and maximum of each image and initializes the centroids dialog appropriately.
     *
     * @return  Flag indicating a successful get.
     */
    private boolean getCentroidsThreshold() {
        int imageNumber = srcImage.length;
        int spectraNumber = 0;
        int i, j, k, m, x, y, z, kVol, mVol, newXDim, newYDim, newZDim, kClass, index, newSliceSize, newVolSize;

        float[] minimum;
        float[] maximum;
        float[] tBuffer;
        float[] buffer;
        float[] buffer2;

        int xDim = srcImage[0].getExtents()[0];
        int yDim = srcImage[0].getExtents()[1];
        int zDim;

        if (srcImage[0].getNDims() > 2) {
            zDim = srcImage[0].getExtents()[2];
        } else {
            zDim = 1;
        }

        // int orgXDim     = xDim;
        // int orgYDim     = yDim;
        int sliceSize = xDim * yDim;
        int volSize = xDim * yDim * zDim;
        // int orgSlice    = sliceSize;
        // int orgVol      = volSize;

        for (i = 0; i < srcImage.length; i++) {

            if (srcImage[i].isColorImage()) {

                if (colorPanel.isRedProcessingRequested()) {
                    spectraNumber++;
                }

                if (colorPanel.isGreenProcessingRequested()) {
                    spectraNumber++;
                }

                if (colorPanel.isBlueProcessingRequested()) {
                    spectraNumber++;
                }
            } else {
                spectraNumber++;
            }
        }

        try {

            minimum = new float[spectraNumber];
            maximum = new float[spectraNumber];
            threshold = new float[spectraNumber];
            tBuffer = new float[volSize];
            buffer = new float[spectraNumber * volSize];

            for (i = 0, j = 0; i < imageNumber; i++) {
                srcImage[i].calcMinMax();

                if (srcImage[i].isColorImage()) {

                    if (colorPanel.isRedProcessingRequested()) {
                        minimum[j] = (float) srcImage[i].getMinR();
                        maximum[j] = (float) srcImage[i].getMaxR();
                        j++;
                    }

                    if (colorPanel.isGreenProcessingRequested()) {
                        minimum[j] = (float) srcImage[i].getMinG();
                        maximum[j] = (float) srcImage[i].getMaxG();
                        j++;
                    }

                    if (colorPanel.isBlueProcessingRequested()) {
                        minimum[j] = (float) srcImage[i].getMinB();
                        maximum[j] = (float) srcImage[i].getMaxB();
                        j++;
                    }
                } else {
                    minimum[j] = (float) srcImage[i].getMin();
                    maximum[j] = (float) srcImage[i].getMax();
                    j++;
                }
            } // for (i = 0,j = 0; i < imageNumber;i++)

            for (i = 0, k = 0; i < imageNumber; i++) {

                if (srcImage[i].isColorImage()) {

                    if (colorPanel.isRedProcessingRequested()) {
                        srcImage[i].exportRGBData(1, 0, sliceSize, tBuffer);

                        kVol = k * volSize;

                        for (j = 0; j < sliceSize; j++) {
                            buffer[kVol + j] = tBuffer[j];
                        }

                        k++;
                    } // if (colorPanel.isRedProcessingRequested())

                    if (colorPanel.isGreenProcessingRequested()) {
                        srcImage[i].exportRGBData(2, 0, sliceSize, tBuffer);

                        kVol = k * volSize;

                        for (j = 0; j < sliceSize; j++) {
                            buffer[kVol + j] = tBuffer[j];
                        }

                        k++;
                    } // if (colorPanel.isGreenProcessingRequested())

                    if (colorPanel.isBlueProcessingRequested()) {
                        srcImage[i].exportRGBData(3, 0, sliceSize, tBuffer);

                        kVol = k * volSize;

                        for (j = 0; j < sliceSize; j++) {
                            buffer[kVol + j] = tBuffer[j];
                        }

                        k++;
                    } // if (colorPanel.isBlueProcessingRequested())
                } else { // not color
                    srcImage[i].exportData(0, volSize, tBuffer);

                    kVol = k * volSize;

                    for (j = 0; j < volSize; j++) {
                        buffer[kVol + j] = tBuffer[j];
                    }

                    k++;
                } // else not color
            } // for (i = 0, k = 0; i < imageNumber; i++)

            if (!regionFlag) {

                for (k = 0; k < spectraNumber; k++) {
                    maximum[k] = -Float.MAX_VALUE;
                    minimum[k] = Float.MAX_VALUE;
                    kVol = k * volSize;

                    for (i = 0; i < volSize; i++) {

                        if (afcmAlgo.getMask().get(i)) {

                            if (buffer[i + kVol] > maximum[k]) {
                                maximum[k] = buffer[i + kVol];
                            }

                            if (buffer[i + kVol] < minimum[k]) {
                                minimum[k] = buffer[i + kVol];
                            }
                        }
                    }
                } // for (k = 0; k < spectraNumber; k++)
            } // if (!wholeImage)

            tBuffer = null;
            System.gc();

            float[] tCentroids = new float[nClasses];
            centroids = new float[spectraNumber * nClasses];

            for (i = 0, k = 0; i < imageNumber; i++) {

                if (srcImage[i].isColorImage()) {

                    if (colorPanel.isRedProcessingRequested()) {
                        JDialogCentroidThreshold dialogCentroidThreshold = new JDialogCentroidThreshold(parentFrame,
                                                                                                        "RED " +
                                                                                                        srcImage[i].getImageName(),
                                                                                                        nClasses,
                                                                                                        minimum[k],
                                                                                                        maximum[k]);

                        if (dialogCentroidThreshold.isCancelled()) {
                            return false;
                        } else {
                            tCentroids = dialogCentroidThreshold.getCentroids();
                            threshold[k] = dialogCentroidThreshold.getThreshold();
                            kClass = k * nClasses;

                            for (j = 0; j < nClasses; j++) {
                                centroids[kClass + j] = tCentroids[j];
                            }

                            k++;
                        }
                    } // if (colorPanel.isRedProcessingRequested())

                    if (colorPanel.isGreenProcessingRequested()) {
                        JDialogCentroidThreshold dialogCentroidThreshold = new JDialogCentroidThreshold(parentFrame,
                                                                                                        "GREEN " +
                                                                                                        srcImage[i].getImageName(),
                                                                                                        nClasses,
                                                                                                        minimum[k],
                                                                                                        maximum[k]);

                        if (dialogCentroidThreshold.isCancelled()) {
                            return false;
                        } else {
                            tCentroids = dialogCentroidThreshold.getCentroids();
                            threshold[k] = dialogCentroidThreshold.getThreshold();
                            kClass = k * nClasses;

                            for (j = 0; j < nClasses; j++) {
                                centroids[kClass + j] = tCentroids[j];
                            }

                            k++;
                        }
                    } // if (colorPanel.isGreenProcessingRequested())

                    if (colorPanel.isBlueProcessingRequested()) {
                        JDialogCentroidThreshold dialogCentroidThreshold = new JDialogCentroidThreshold(parentFrame,
                                                                                                        "BLUE " +
                                                                                                        srcImage[i].getImageName(),
                                                                                                        nClasses,
                                                                                                        minimum[k],
                                                                                                        maximum[k]);

                        if (dialogCentroidThreshold.isCancelled()) {
                            return false;
                        } else {
                            tCentroids = dialogCentroidThreshold.getCentroids();
                            threshold[k] = dialogCentroidThreshold.getThreshold();
                            kClass = k * nClasses;

                            for (j = 0; j < nClasses; j++) {
                                centroids[kClass + j] = tCentroids[j];
                            }

                            k++;
                        }
                    } // if (colorPanel.isBlueProcessingRequested())
                } else { // not color

                    JDialogCentroidThreshold dialogCentroidThreshold = new JDialogCentroidThreshold(parentFrame,
                                                                                                    srcImage[i].getImageName(),
                                                                                                    nClasses,
                                                                                                    minimum[k],
                                                                                                    maximum[k]);

                    if (dialogCentroidThreshold.isCancelled()) {
                        return false;
                    } else {
                        tCentroids = dialogCentroidThreshold.getCentroids();
                        threshold[k] = dialogCentroidThreshold.getThreshold();
                        kClass = k * nClasses;

                        for (j = 0; j < nClasses; j++) {
                            centroids[kClass + j] = tCentroids[j];
                        }

                        k++;
                    }
                } // else not color
            } // for (i = 0, k = 0; i < imageNumber; i++)

            tCentroids = null;

            int xLow = 0;
            int yLow = 0;
            int zLow = 0;
            int xHigh = xDim - 1;
            int yHigh = yDim - 1;
            int zHigh = zDim - 1;
            int zStepIn, zStepOut, yStepIn, yStepOut, mStepIn, mStepOut;

            if (cropBackground) {

                // Find the smallest bounding box for the 1st data set
                // If cropBackground is true wholeImage is constrained to be true
                xLow = xDim - 1;
                yLow = yDim - 1;
                zLow = zDim - 1;
                xHigh = 0;
                yHigh = 0;
                zHigh = 0;

                for (z = 0; z < zDim; z++) {
                    zStepIn = z * sliceSize;

                    for (y = 0; y < yDim; y++) {
                        yStepIn = (y * xDim) + zStepIn;

                        for (x = 0; x < xDim; x++) {
                            index = x + yStepIn;

                            if (buffer[index] >= threshold[0]) {

                                if (x < xLow) {
                                    xLow = x;
                                }

                                if (x > xHigh) {
                                    xHigh = x;
                                }

                                if (y < yLow) {
                                    yLow = y;
                                }

                                if (y > yHigh) {
                                    yHigh = y;
                                }

                                if (z < zLow) {
                                    zLow = z;
                                }

                                if (z > zHigh) {
                                    zHigh = z;
                                }
                            } // if (buffer[index] > threshold[0])
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)
                } // for (z = 0; z < zDim; z++)

                if ((xLow > 0) || (xHigh < (xDim - 1)) || (yLow > 0) || (yHigh < (yDim - 1)) || (zLow > 0) ||
                        (zHigh < (zDim - 1))) {

                    // A smaller bounding box has been found for the data
                    // Recopy area to smaller data array to save space
                    newXDim = xHigh - xLow + 1;
                    newYDim = yHigh - yLow + 1;
                    newZDim = zHigh - zLow + 1;
                    newSliceSize = newXDim * newYDim;
                    newVolSize = newSliceSize * newZDim;
                    buffer2 = new float[spectraNumber * newVolSize];

                    for (m = 0; m < spectraNumber; m++) {
                        mStepOut = m * volSize;
                        mStepIn = (m * newVolSize) - xLow - (yLow * newXDim) - (zLow * newSliceSize);

                        for (z = zLow; z <= zHigh; z++) {
                            zStepOut = (z * sliceSize) + mStepOut;
                            zStepIn = (z * newSliceSize) + mStepIn;

                            for (y = yLow; y <= yHigh; y++) {
                                yStepOut = (y * xDim) + zStepOut;
                                yStepIn = (y * newXDim) + zStepIn;

                                for (x = xLow; x <= xHigh; x++) {
                                    buffer2[x + yStepIn] = buffer[x + yStepOut];
                                } // for (x = xLow; x <= xHigh; x++)
                            } // for (y = yLow; y <= yHigh; y++)
                        } // for (z = zLow; z <= zHigh; z++)
                    } // for (m = 0; m < spectraNumber; m++)

                    xDim = newXDim;
                    yDim = newYDim;
                    zDim = newZDim;
                    sliceSize = xDim * yDim;
                    volSize = sliceSize * zDim;

                    int totalSize = spectraNumber * volSize;
                    buffer = null;
                    buffer = new float[totalSize];

                    for (i = 0; i < totalSize; i++) {
                        buffer[i] = buffer2[i];
                    }

                    buffer2 = null;

                    // Find the new minimum
                    for (m = 0; m < spectraNumber; m++) {
                        minimum[m] = Float.MAX_VALUE;
                        mVol = m * volSize;

                        for (i = 0; i < volSize; i++) {

                            if (buffer[mVol + i] < minimum[m]) {
                                minimum[m] = buffer[mVol + i];
                            } // if (buffer[mSlice + i] < minimum[m])
                        } // for (i = 0; i < sliceSize; i++)
                    } // for (m = 0; m < spectralNumber; m++) {
                } // if ((xLow > 0) || (xHigh < (xDim-1)) || (yLow > 0) || (yHigh < (yDim - 1)))
            } // if (cropBackground)
        } catch (java.io.IOException ioe) {
            tBuffer = null;
            buffer = null;
            buffer2 = null;
            System.gc();
            MipavUtil.displayError("Dialog MSFuzzyCMeans: Error in trying to find centroids.");

            return false;
        } catch (OutOfMemoryError error) {
            tBuffer = null;
            buffer = null;
            buffer2 = null;
            System.gc();
            MipavUtil.displayError("Algorithm FuzzyCMeans reports:\n" + error.toString());

            return false;
        }

        tBuffer = null;
        buffer = null;
        buffer2 = null;
        System.gc();

        return true;
    }

    /**
     * A private helper function to get the current used FileFilter from JFileChooser.
     *
     * @param   chooser  DOCUMENT ME!
     * @param   index    the index of the choosable file filters.
     *
     * @return  the current used file filter.
     */
    private FileFilter getFileFilter(JFileChooser chooser, int index) {
        FileFilter[] filters = chooser.getChoosableFileFilters();
        String[] descriptions = ViewImageFileFilter.getDescriptions();

        for (int i = 0; i < filters.length; i++) {

            if (filters[i].getDescription().equals(descriptions[index])) {
                return filters[i];
            }
        }

        return null;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Fuzzy C-means");

        labelNClasses = new JLabel("Number of desired classes");
        labelNClasses.setForeground(Color.black);
        labelNClasses.setFont(serif12);

        textNClasses = new JTextField(5);
        textNClasses.setText("3");
        textNClasses.setFont(serif12);

        /* labelNPyramid = new JLabel("Number of pyramid levels.");
         *    labelNPyramid.setForeground(Color.black); labelNPyramid.setFont(serif12); textNPyramid = new
         * JTextField(5); textNPyramid.setText("4");textNPyramid.setFont(serif12);*/

        labelExpo = new JLabel("Desired exponent value");
        labelExpo.setForeground(Color.black);
        labelExpo.setFont(serif12);

        textExpo = new JTextField(5);
        textExpo.setText("2");
        textExpo.setFont(serif12);

        cropCheckbox = new JCheckBox("Background cropping");
        cropCheckbox.setFont(serif12);
        cropCheckbox.setSelected(false);
        cropCheckbox.addActionListener(this);
        cropCheckbox.setActionCommand("Crop");

        labelEndTol = new JLabel("End tolerance");
        labelEndTol.setForeground(Color.black);
        labelEndTol.setFont(serif12);

        textEndTol = new JTextField(5);
        textEndTol.setText("0.01");
        textEndTol.setFont(serif12);

        labelMaxIter = new JLabel("Maximum number of iterations");
        labelMaxIter.setForeground(Color.black);
        labelMaxIter.setFont(serif12);

        textMaxIter = new JTextField(5);
        textMaxIter.setText("200");
        textMaxIter.setFont(serif12);

        JPanel upperPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelNClasses, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textNClasses, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelExpo, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textExpo, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelEndTol, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textEndTol, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelMaxIter, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMaxIter, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 2;
        upperPanel.add(cropCheckbox, gbc);

        /* labelSmooth = new JLabel("1st and 2nd order smoothness.");
         *    labelSmooth.setForeground(Color.black); labelSmooth.setFont(serif12); textOneSmooth = new JTextField(5);
         * textOneSmooth.setText("50000"); textOneSmooth.setFont(serif12); textTwoSmooth = new JTextField(5);
         * textTwoSmooth.setText("500000"); textTwoSmooth.setFont(serif12); labelJacobi = new JLabel("Number of Jacobi
         * iterations.");   labelJacobi.setForeground(Color.black); labelJacobi.setFont(serif12); textOneJacobiIter =
         * new JTextField(5); textOneJacobiIter.setText("2"); textOneJacobiIter.setFont(serif12); textTwoJacobiIter =
         * new JTextField(5); textTwoJacobiIter.setText("3");textTwoJacobiIter.setFont(serif12); */

        imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("1st image region"));

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        segmentationGroup = new ButtonGroup();
        hardOnly = new JRadioButton("Hard only", false);
        hardOnly.setFont(serif12);
        segmentationGroup.add(hardOnly);

        fuzzyOnly = new JRadioButton("Fuzzy only", false);
        fuzzyOnly.setFont(serif12);
        segmentationGroup.add(fuzzyOnly);

        hardFuzzyBoth = new JRadioButton("Hard and fuzzy both", true);
        hardFuzzyBoth.setFont(serif12);
        segmentationGroup.add(hardFuzzyBoth);

        segmentationPanel = new JPanel(new GridBagLayout());
        segmentationPanel.setBorder(buildTitledBorder("Segmentation"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        segmentationPanel.add(hardOnly, gbc);
        gbc.gridy = 1;
        segmentationPanel.add(fuzzyOnly, gbc);
        gbc.gridy = 2;
        segmentationPanel.add(hardFuzzyBoth, gbc);

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        int ypos = 0;
        gbc.gridy = ypos++;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        paramPanel.add(upperPanel, gbc);
        gbc.gridy = ypos++;
        gbc.gridwidth = 1;
        paramPanel.add(imageVOIPanel, gbc);
        gbc.gridx = 1;
        paramPanel.add(segmentationPanel, gbc);

        colorPanel = new JPanelColorChannels(srcImage[0]);

        if (srcImage[0].isColorImage()) {
            gbc.gridx = 0;
            gbc.gridy = ypos++;
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            paramPanel.add(colorPanel, gbc);
        } // if (doColor)

        /*calcGainFieldCheckbox = new JCheckBox("Generate gain field.");
         * calcGainFieldCheckbox.setBounds(10, 425, 240, 25); calcGainFieldCheckbox.setFont(serif12);
         * paramPanel.add(calcGainFieldCheckbox);calcGainFieldCheckbox.setSelected(false); */

        imagePanel = new JPanel(new BorderLayout());
        imagePanel.setBorder(buildTitledBorder("Load Image(s)"));

        model = new DefaultListModel();
        model.addElement(srcImage[0].getImageName());
        imageList = new JList(model);
        imageList.setVisibleRowCount(6);
        imageList.setPreferredSize(new Dimension(300, 120));
        imageList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        imageList.addListSelectionListener(this);
        imagePanel.add(imageList);

        JPanel chooserPanel = new JPanel();
        chooserButton = new JButton("Load");
        chooserButton.setPreferredSize(MipavUtil.defaultButtonSize);
        chooserButton.setFont(serif12B);
        chooserPanel.add(chooserButton);
        chooserButton.addActionListener(this);
        chooserButton.setActionCommand("Choose");

        removeButton = new JButton("Remove");
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeButton.setFont(serif12B);
        removeButton.setEnabled(false);
        chooserPanel.add(removeButton);
        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");

        imagePanel.add(chooserPanel, BorderLayout.SOUTH);

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = ypos++;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(imagePanel, gbc);

        getContentPane().add(paramPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Open an image based on the suffix of the file.
     *
     * @return  The image.
     */
    private ModelImage open() {
        JFileChooser chooser = null;
        FileIO fileIO = null;
        boolean multiFile = false;
        String fileName;
        String directory;

        try {

            chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

            FileFilter currentFileFilter = getFileFilter(chooser, Preferences.getFileFilter());
            chooser.setFileFilter(currentFileFilter);

            chooser.setDialogTitle("Open Image");

            int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return null;
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        try {
            fileIO = new FileIO();

            return fileIO.readImage(fileName, directory, multiFile, null);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if (wholeImage.isSelected()) {
            regionFlag = true;
        } else if (VOIRegions.isSelected()) {
            regionFlag = false;
        }

        tmpStr = textNClasses.getText();

        if (testParameter(tmpStr, 1.0, 12.0)) {
            nClasses = Integer.valueOf(tmpStr).intValue();
        } else {
            textNClasses.requestFocus();
            textNClasses.selectAll();

            return false;
        }

        /* tmpStr = textNPyramid.getText();
         *  if ( testParameter(tmpStr, 1.0, 6.0) ){     nPyramid = Integer.valueOf(tmpStr).intValue(); } else{
         * textNPyramid.requestFocus();     textNPyramid.selectAll();     return false; } */

        tmpStr = textExpo.getText();

        if (testParameter(tmpStr, 1.1, 5.0)) {
            q = Float.valueOf(tmpStr).floatValue();
        } else {
            textExpo.requestFocus();
            textExpo.selectAll();

            return false;
        }

        if (cropCheckbox.isSelected()) {
            cropBackground = true;
        } else {
            cropBackground = false;
        }

        tmpStr = textEndTol.getText();

        if (testParameter(tmpStr, Float.MIN_VALUE, 1.0)) {
            endTol = Float.valueOf(tmpStr).floatValue();
        } else {
            textEndTol.requestFocus();
            textEndTol.selectAll();

            return false;
        }

        tmpStr = textMaxIter.getText();

        if (testParameter(tmpStr, 1.0, 10000.0)) {
            maxIter = Integer.valueOf(tmpStr).intValue();
        } else {
            textMaxIter.requestFocus();
            textMaxIter.selectAll();

            return false;
        }

        /*   tmpStr = textOneSmooth.getText();
         *  if (testParameter(tmpStr,2.0e2,2.0e6) ) {     oneSmooth = Float.valueOf(tmpStr).floatValue(); } else {
         * textOneSmooth.requestFocus();     textOneSmooth.selectAll();     return false; } tmpStr =
         * textTwoSmooth.getText(); if (testParameter(tmpStr,2.0e3,2.0e7) ) {     twoSmooth =
         * Float.valueOf(tmpStr).floatValue(); } else {     textTwoSmooth.requestFocus();     textTwoSmooth.selectAll();
         *   return false; } tmpStr = textOneJacobiIter.getText(); if (testParameter(tmpStr,1.0,10.0) ) { oneJacobiIter
         * = Integer.valueOf(tmpStr).intValue(); } else {     textOneJacobiIter.requestFocus();
         * textOneJacobiIter.selectAll();     return false; } tmpStr = textTwoJacobiIter.getText(); if
         * (testParameter(tmpStr,1.0,10.0) ) {     twoJacobiIter = Integer.valueOf(tmpStr).intValue(); } else {
         * textTwoJacobiIter.requestFocus();     textTwoJacobiIter.selectAll();     return false; } */

        if (hardOnly.isSelected()) {
            segmentation = AlgorithmMSpectralFuzzyCMeans.HARD_ONLY;
        } else if (fuzzyOnly.isSelected()) {
            segmentation = AlgorithmMSpectralFuzzyCMeans.FUZZY_ONLY;
        } else {
            segmentation = AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD;
        }

        /* if (calcGainFieldCheckbox.isSelected()) {
         *  outputGainField = true;   }   else { outputGainField = false;   } */

        return true;
    }

}
