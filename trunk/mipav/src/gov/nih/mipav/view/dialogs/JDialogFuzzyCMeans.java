package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 *
 * @see  AlgorithmFuzzyCMeans
 */
public class JDialogFuzzyCMeans extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7191124209694457297L;

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] centroids;

    /** DOCUMENT ME! */
    private boolean cropBackground;

    /** DOCUMENT ME! */
    private JCheckBox cropCheckbox;

    /** DOCUMENT ME! */
    private int[] destExtents;

    /** DOCUMENT ME! */
    private float endTol;

    /** DOCUMENT ME! */
    private AlgorithmFuzzyCMeans fcmAlgo;

    /** DOCUMENT ME! */
    private JRadioButton fuzzyOnly;

    /** DOCUMENT ME! */
    private JRadioButton hardFuzzyBoth;

    /** DOCUMENT ME! */
    private JRadioButton hardOnly;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

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
    private JLabel labelSignal;

    /** DOCUMENT ME! */
    private int maxIter;

    /** DOCUMENT ME! */
    private int nClasses;

    /** DOCUMENT ME! */
    private int nPyramid = 4;

    /** DOCUMENT ME! */
    private int oneJacobiIter = 1;

    /** DOCUMENT ME! */
    private float oneSmooth = 2e4f;

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
    private JTextField textEndTol;

    /** DOCUMENT ME! */
    private JTextField textExpo;

    /** DOCUMENT ME! */
    private JTextField textMaxIter;

    /** DOCUMENT ME! */
    private JTextField textNClasses;

    /** DOCUMENT ME! */
    private JTextField textSignal;

    /** DOCUMENT ME! */
    private float threshold;

    /** DOCUMENT ME! */
    private int twoJacobiIter = 2;

    /** DOCUMENT ME! */
    private float twoSmooth = 2e5f;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFuzzyCMeans() { }

    // false = apply algorithm only to VOI regions

    /**
     * Creates a new JDialogFuzzyCMeans object.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogFuzzyCMeans(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10026");
            MipavUtil.showWebHelp("Fuzzy_C-Means:_Multispectral_and_Single_Channel_Algorithms");
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
        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[resultNumber];

        if (algorithm instanceof AlgorithmFuzzyCMeans) {
            image.clearMask();

            if ((fcmAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                for (i = 0; i < resultNumber; i++) {
                    updateFileInfo(image, resultImage[i]);
                    resultImage[i].clearMask();

                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200 + (i * 20)));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new frame", "Error",
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                    
                	// Copy original source NIFTI matrices to result images
                    resultImage[i].getMatrixHolder().replaceMatrices(image.getMatrixHolder().getMatrices());
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
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        fcmAlgo.finalize();
        fcmAlgo = null;
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
     * Accessor that sets the threshold.
     *
     * @param  scale  Value to set the threshold to.
     */
    public void setThreshold(float scale) {
        threshold = scale;
    }

    /**
     * Once all the necessary variables are set, call the Fuzzy C Means algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int i;

        // Calculate the number of result images.
        resultNumber = 0;

        if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD)) {
            resultNumber++; // segmented image
        } // if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD))

        if ((segmentation == FUZZY_ONLY) || (segmentation == BOTH_FUZZY_HARD)) {

            // if (outputGainField) {
            // resultNumber++;
            // }
            resultNumber += nClasses;
        } // if ((segmentation == FUZZY_ONLY) || (segmentation == BOTH_FUZZY_HARD))

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
        } else { // image.getNDims)() == 3
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
        }

        try {
        	if (resultImage == null){
        		resultImage = new ModelImage[resultNumber];
        	}
            presentNumber = 0;

            if ((segmentation == FUZZY_ONLY) || (segmentation == BOTH_FUZZY_HARD)) {

                for (i = 0; i < nClasses; i++) {	
                    resultImage[presentNumber++] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
                                                    makeImageName(image.getImageName(),
                                                    "_class" + (i + 1)));
                }
                /* if (outputGainField) {
                 *  resultImage[presentNumber++] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
                 * image.getImageName() + "mult", userInterface);              } */
            }

            if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD)) {
                resultImage[presentNumber++] = new ModelImage(ModelStorageBase.UBYTE, destExtents,
                                                          makeImageName(image.getImageName(), "_seg"));
            }

            // Make algorithm
            fcmAlgo = new AlgorithmFuzzyCMeans(resultImage, image, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q,
                                               oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground,
                                               threshold, maxIter, endTol, regionFlag);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            fcmAlgo.addListener(this);

            createProgressBar(image.getImageName(), fcmAlgo);

            if (regionFlag == false) {
                fcmAlgo.setMask(image.generateVOIMask());
                // if non null, were set by script file
            }

            if (centroids == null) {

                // if false dialog was cancelled
                if (!getCentroids()) {
                    return;
                }
            }

            fcmAlgo.setCentroids(centroids);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (fcmAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                fcmAlgo.run();
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
            MipavUtil.displayError("Dialog FuzzyCMeans: unable to allocate enough memory");

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
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        resultNumber = scriptParameters.getParams().getInt("number_of_result_images");

        setRegionFlag(scriptParameters.doProcessWholeImage());
        setNClasses(scriptParameters.getParams().getInt("number_of_classes"));
        setQ(scriptParameters.getParams().getFloat("exponent_q"));
        setCrop(scriptParameters.getParams().getBoolean("do_crop_background"));
        setThreshold(scriptParameters.getParams().getFloat("threshold"));
        setEndTol(scriptParameters.getParams().getFloat("end_tolerance"));
        setMaxIter(scriptParameters.getParams().getInt("max_iterations"));
        setSegmentationType(scriptParameters.getParams().getInt("segmentation_type"));
        setCentroids(scriptParameters.getParams().getList("centroids").getAsFloatArray());
        
        resultImage = new ModelImage[resultNumber];
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_result_images", resultNumber));

        for (int i = 0; i < resultNumber; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }

        scriptParameters.storeProcessWholeImage(regionFlag);
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_classes", nClasses));
        scriptParameters.getParams().put(ParameterFactory.newParameter("exponent_q", q));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_crop_background", cropBackground));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("end_tolerance", endTol));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_iterations", maxIter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("segmentation_type", segmentation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("centroids", centroids));
    }

    /**
     * Gets the minimum and maximum of each image and initializes the centroids dialog appropriately.
     *
     * @return  Flag indicating a successful get.
     */
    private boolean getCentroids() {
        int i;
        float minimum, maximum;
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim;

        if (image.getNDims() > 2) {
            zDim = image.getExtents()[2];
        } else {
            zDim = 1;
        }

        int sliceSize = xDim * yDim;
        int volSize = xDim * yDim * zDim;
        float[] buffer = null;
        int yStepIn, yStepOut, zStepIn, zStepOut;
        int x, y, z, index, newXDim, newYDim, newZDim, newSliceSize;

        try {
            buffer = new float[volSize];
            image.exportData(0, volSize, buffer);

            image.calcMinMax();
            minimum = (float) image.getMin();
            maximum = (float) image.getMax();

            if (!regionFlag) {
                maximum = -Float.MAX_VALUE;
                minimum = Float.MAX_VALUE;

                for (i = 0; i < volSize; i++) {

                    if (fcmAlgo.getMask().get(i)) {

                        if (buffer[i] > maximum) {
                            maximum = buffer[i];
                        }

                        if (buffer[i] < minimum) {
                            minimum = buffer[i];
                        }
                    }
                }
            } // if (!wholeImage)

            int xLow = 0;
            int yLow = 0;
            int zLow = 0;
            int xHigh = xDim - 1;
            int yHigh = yDim - 1;
            int zHigh = zDim - 1;

            if (cropBackground) {

                // Find the smallest bounding box for the data
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

                            if (buffer[index] >= threshold) {

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
                            } // if (buffer[index] > threshold)
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

                    float[] buffer2 = new float[newXDim * newYDim * newZDim];
                    newSliceSize = newXDim * newYDim;

                    for (z = zLow; z <= zHigh; z++) {
                        zStepOut = z * sliceSize;
                        zStepIn = ((z - zLow) * newSliceSize) - xLow - (yLow * newXDim);

                        for (y = yLow; y <= yHigh; y++) {
                            yStepIn = (y * newXDim) + zStepIn;
                            yStepOut = (y * xDim) + zStepOut;

                            for (x = xLow; x <= xHigh; x++) {
                                buffer2[x + yStepIn] = buffer[x + yStepOut];
                            } // for (x = xLow; x <= xHigh; x++)
                        } // for (y = yLow; y <= yHigh; y++)
                    } // for (z = zLow; z <= zHigh; z++)

                    xDim = newXDim;
                    yDim = newYDim;
                    zDim = newZDim;
                    sliceSize = xDim * yDim;
                    volSize = sliceSize * zDim;
                    buffer = new float[volSize];

                    for (i = 0; i < sliceSize; i++) {
                        buffer[i] = buffer2[i];
                    }

                    buffer2 = null;

                    // Find the new minimum
                    minimum = maximum;

                    for (i = 0; i < volSize; i++) {

                        if (buffer[i] < minimum) {
                            minimum = buffer[i];
                        } // if (buffer[i] < minimum)
                    } // for (i = 0; i < sliceSize; i++)
                } // if ((xLow > 0) || (xHigh < (xDim-1)) || (yLow > 0) || (yHigh < (yDim - 1)))
            } // if (cropBackground)
        } catch (java.io.IOException ioe) {
            buffer = null;
            System.gc();
            MipavUtil.displayError("Error trying to get centroids.");

            return false;
        } catch (OutOfMemoryError error) {
            buffer = null;
            System.gc();
            MipavUtil.displayError("Algorithm FuzzyCMeans reports:\n" + error.toString());

            return false;
        }

        buffer = null;
        System.gc();

        // Autodetect initial centroids
        centroids = new float[nClasses];

        JDialogInitialCentroids dialogInitialCentroids = new JDialogInitialCentroids(parentFrame, nClasses, minimum,
                                                                                     maximum);

        if (dialogInitialCentroids.isCancelled()) {
            centroids = null;

            return false;
        } else {

            for (i = 0; i < centroids.length; i++) {
                centroids[i] = dialogInitialCentroids.getCentroids()[i];
            }
        }

        return true;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
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

        labelSignal = new JLabel("Signal threshold");
        labelSignal.setForeground(Color.black);
        labelSignal.setFont(serif12);

        textSignal = new JTextField(5);
        textSignal.setText("0.0");
        textSignal.setFont(serif12);

        cropCheckbox = new JCheckBox("Background cropping");
        cropCheckbox.setFont(serif12);
        cropCheckbox.setSelected(false);
        cropCheckbox.addActionListener(this);
        cropCheckbox.setActionCommand("Crop");

        labelEndTol = new JLabel("End tolerance.");
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
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelSignal, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textSignal, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
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
        imageVOIPanel.setBorder(buildTitledBorder("Region"));

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

        /*calcGainFieldCheckbox = new JCheckBox("Generate gain field.");
         * calcGainFieldCheckbox.setBounds(10, 425, 240, 25); calcGainFieldCheckbox.setFont(serif12);
         * paramPanel.add(calcGainFieldCheckbox);calcGainFieldCheckbox.setSelected(false); */

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        paramPanel.add(upperPanel, gbc);
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        paramPanel.add(imageVOIPanel, gbc);
        gbc.gridx = 1;
        paramPanel.add(segmentationPanel, gbc);

        getContentPane().add(paramPanel);
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
        // int    i;

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
         *       if ( testParameter(tmpStr, 1.0, 6.0) ){ nPyramid = Integer.valueOf(tmpStr).intValue();      }
         * else{ textNPyramid.requestFocus(); textNPyramid.selectAll(); return;      } */

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

        tmpStr = textSignal.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            threshold = Float.valueOf(tmpStr).floatValue();
        } else {
            textSignal.requestFocus();
            textSignal.selectAll();

            return false;
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

        /*  tmpStr = textOneSmooth.getText();
         * if (testParameter(tmpStr,2.0e2,2.0e6) ) {   oneSmooth = Float.valueOf(tmpStr).floatValue(); } else {
         * textOneSmooth.requestFocus();   textOneSmooth.selectAll();   return; } tmpStr = textTwoSmooth.getText(); if
         * (testParameter(tmpStr,2.0e3,2.0e7) ) {   twoSmooth = Float.valueOf(tmpStr).floatValue(); } else {
         * textTwoSmooth.requestFocus();   textTwoSmooth.selectAll();   return; } tmpStr = textOneJacobiIter.getText();
         * if (testParameter(tmpStr,1.0,10.0) ) {   oneJacobiIter = Integer.valueOf(tmpStr).intValue(); } else {
         * textOneJacobiIter.requestFocus();   textOneJacobiIter.selectAll();   return; } tmpStr =
         * textTwoJacobiIter.getText(); if (testParameter(tmpStr,1.0,10.0) ) {   twoJacobiIter =
         * Integer.valueOf(tmpStr).intValue(); } else {   textTwoJacobiIter.requestFocus();
         * textTwoJacobiIter.selectAll();   return;} */

        if (hardOnly.isSelected()) {
            segmentation = HARD_ONLY;
        } else if (fuzzyOnly.isSelected()) {
            segmentation = FUZZY_ONLY;
        } else {
            segmentation = BOTH_FUZZY_HARD;
        }

        /* if (calcGainFieldCheckbox.isSelected()) {
         *  outputGainField = true;      }      else { outputGainField = false;      } */

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
                return new String("Algorithms.Segmentation");
            }

            public String getDescription() {
                return new String("Applies a fuzzy C-means filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a fuzzy C-means filter.");
            }

            public String getShortLabel() {
                return new String("FuzzyCMeans");
            }

            public String getLabel() {
                return new String("Fuzzy C-means");
            }

            public String getName() {
                return new String("Fuzzy C-means");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterInt("number_of_result_images", 4));
            table.put(new ParameterInt("number_of_classes",3));
            table.put(new ParameterFloat("exponent_q", 2f));
            table.put(new ParameterBoolean("do_crop_background", false));
            table.put(new ParameterFloat("threshold",0f));
            table.put(new ParameterFloat("end_tolerance", .01f));
            table.put(new ParameterInt("max_iterations", 200));
            table.put(new ParameterInt("segmentation_type", 0));
            table.put(new ParameterList("centroids", Parameter.PARAM_FLOAT, "-318.5,387.0,1092.5"));
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
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"1"));
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"2"));
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"3"));
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"4"));
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
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"1")) {
        	return resultImage[0].getImageName();
        }
        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"2")) {
        	return resultImage[1].getImageName();
        }
        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"3")) {
        	return resultImage[2].getImageName();
        }
        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"4")) {
        	return resultImage[3].getImageName();
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
