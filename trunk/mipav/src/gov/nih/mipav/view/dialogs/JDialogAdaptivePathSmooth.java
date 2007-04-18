package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
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
 * source image. Algorithms are executed in their own thread.
 *
 * @see  AlgorithmAdaptivePathSmooth
 */
public class JDialogAdaptivePathSmooth extends JDialogScriptableBase
        implements AlgorithmInterface, ScriptableActionInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3709702364792587183L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmAdaptivePathSmooth adaptivePathSmoothAlgo;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private float diff;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = true;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckBox;

    /** DOCUMENT ME! */
    private boolean includeNeighbors = false;

    /** DOCUMENT ME! */
    private JLabel labelRadiusCb;

    /** DOCUMENT ME! */
    private JLabel labelRadiusCr;

    /** DOCUMENT ME! */
    private JLabel labelRadiusY;

    /** DOCUMENT ME! */
    private JLabel labelThreshold;

    /** DOCUMENT ME! */
    private JCheckBox neighborCheckBox;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private float radiusCb = 5.0f;

    /** DOCUMENT ME! */
    private float radiusCr = 4.0f;

    /** DOCUMENT ME! */
    private float radiusY = 2.0f;

    /** DOCUMENT ME! */
    private boolean reduce = false;

    /** DOCUMENT ME! */
    private JCheckBox reduceCheckBox;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textRadiusCb;

    /** DOCUMENT ME! */
    private JTextField textRadiusCr;

    /** DOCUMENT ME! */
    private JTextField textRadiusY;

    /** DOCUMENT ME! */
    private JTextField textThreshold;

    /** DOCUMENT ME! */
    private float threshold;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAdaptivePathSmooth() { }

    /**
     * Creates a new JDialogAdaptivePathSmooth object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogAdaptivePathSmooth(Frame theParentFrame, ModelImage im) {
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
            MipavUtil.showHelp("10042");
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

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmAdaptivePathSmooth) {
            image.clearMask();

            if ((adaptivePathSmoothAlgo.isCompleted() == true) && (resultImage != null)) {

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
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        adaptivePathSmoothAlgo.finalize();
        adaptivePathSmoothAlgo = null;
        dispose();
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += radiusY + delim;
        str += radiusCr + delim;
        str += radiusCb + delim;
        str += threshold + delim;
        str += includeNeighbors + delim;
        str += reduce + delim;
        str += image25D;

        return str;
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
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (newImage != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                textRadiusY.setText("" + MipavUtil.getFloat(st));
                radiusCr = MipavUtil.getFloat(st);
                radiusCb = MipavUtil.getFloat(st);
                textThreshold.setText("" + MipavUtil.getFloat(st));
                neighborCheckBox.setSelected(MipavUtil.getBoolean(st));
                reduce = MipavUtil.getBoolean(st);
                image25DCheckBox.setSelected(MipavUtil.getBoolean(st));

                if (MipavUtil.getBoolean(st)) {
                    newImage.setSelected(true);
                } else {
                    replaceImage.setSelected(true);
                }

                if (image.isColorImage()) {
                    textRadiusCr.setText(Float.toString(radiusCr));
                    textRadiusCb.setText(Float.toString(radiusCb));
                    reduceCheckBox.setSelected(reduce);
                }
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," + newImage.isSelected());

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
     * Accessor that sets whether 3D images are 3D or 2.5D filtered.
     *
     * @param  image25D  true for 2.5D filtering
     */
    public void setImage25D(boolean image25D) {
        this.image25D = image25D;
    }

    /**
     * Accessor that sets whether or not nearest neighbors of pixels along the low cost path are included.
     *
     * @param  includeNeighbors  DOCUMENT ME!
     */
    public void setIncludeNeighbors(boolean includeNeighbors) {
        this.includeNeighbors = includeNeighbors;
    }

    /**
     * Accessor that sets the Cb radius.
     *
     * @param  radius  Value to set Cb radius to.
     */
    public void setRadiusCb(float radius) {
        radiusCb = radius;
    }

    /**
     * Accessor that sets the Cr radius.
     *
     * @param  radius  Value to set Cr radius to.
     */
    public void setRadiusCr(float radius) {
        radiusCr = radius;
    }

    /**
     * Accessor that sets the Y radius.
     *
     * @param  radius  Value to set Y radius to.
     */
    public void setRadiusY(float radius) {
        radiusY = radius;
    }

    /**
     * Accessor that sets whether or not Cr and Cb are filtered at halved dimensions @ param reduc Value to set reduce
     * to.
     *
     * @param  reduc  DOCUMENT ME!
     */
    public void setReduce(boolean reduc) {
        reduce = reduc;
    }

    /**
     * Accessor that sets the threshold at which pixels cannot be strung together.
     *
     * @param  threshold  DOCUMENT ME!
     */
    public void setThreshold(float threshold) {
        this.threshold = threshold;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_AdaptivePathSmooth");
        int[] destExtents;

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
        } else {
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
        }

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                if (image.isColorImage()) {
                    resultImage = new ModelImage(ModelImage.ARGB, destExtents, name);
                } else {
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);
                }

                // resultImage = (ModelImage)image.clone();
                // resultImage.setImageName(name);
                // Make algorithm
                adaptivePathSmoothAlgo = new AlgorithmAdaptivePathSmooth(resultImage, image, radiusY, radiusCr,
                                                                         radiusCb, threshold, includeNeighbors, reduce,
                                                                         image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                adaptivePathSmoothAlgo.addListener(this);
                createProgressBar(image.getImageName(), adaptivePathSmoothAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (adaptivePathSmoothAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    adaptivePathSmoothAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AdaptivePathSmooth: unable to allocate enough memory");

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
                adaptivePathSmoothAlgo = new AlgorithmAdaptivePathSmooth(null, image, radiusY, radiusCr, radiusCb,
                                                                         threshold, includeNeighbors, reduce, image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                adaptivePathSmoothAlgo.addListener(this);
                createProgressBar(image.getImageName(), adaptivePathSmoothAlgo);

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

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (adaptivePathSmoothAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    adaptivePathSmoothAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AdaptivePathSmooth: unable to allocate enough memory");

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
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        radiusY = scriptParameters.getParams().getFloat("radius_Y");
        radiusCr = scriptParameters.getParams().getFloat("radius_Cr");
        radiusCb = scriptParameters.getParams().getFloat("radius_Cb");
        threshold = scriptParameters.getParams().getFloat("threshold");
        includeNeighbors = scriptParameters.getParams().getBoolean("include_neighbors");
        reduce = scriptParameters.getParams().getBoolean("do_reduce_dims");
        image25D = scriptParameters.doProcess3DAs25D();
    }


    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("radius_Y", radiusY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("radius_Cr", radiusCr));
        scriptParameters.getParams().put(ParameterFactory.newParameter("radius_Cb", radiusCb));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("include_neighbors", includeNeighbors));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_reduce_dims", reduce));
        scriptParameters.storeProcess3DAs25D(image25D);
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Adaptive Path Smooth");

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

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        mainPanel.add(paramPanel, gbc);

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        if (image.isColorImage()) {
            labelRadiusY = createLabel("Y radius ");
        } else {
            labelRadiusY = createLabel("Radius ");
        }

        paramPanel.add(labelRadiusY, gbc2);

        gbc2.gridx = 1;
        textRadiusY = createTextField("2.0");
        paramPanel.add(textRadiusY, gbc2);

        if (image.isColorImage()) {
            gbc2.gridx = 0;
            gbc2.gridy = 1;
            labelRadiusCr = createLabel("Cr radius ");
            paramPanel.add(labelRadiusCr, gbc2);

            gbc2.gridx = 1;
            textRadiusCr = createTextField("4.0");
            paramPanel.add(textRadiusCr, gbc2);

            gbc2.gridx = 0;
            gbc2.gridy = 2;
            labelRadiusCb = createLabel("Cb radius ");
            paramPanel.add(labelRadiusCb, gbc2);

            gbc2.gridx = 1;
            textRadiusCb = createTextField("5.0");
            paramPanel.add(textRadiusCb, gbc2);

            gbc2.gridx = 0;
            gbc2.gridy = 3;
        } // if (image.isColorImage())
        else {
            gbc2.gridx = 0;
            gbc2.gridy = 1;
        }

        labelThreshold = createLabel("Threshold ");
        paramPanel.add(labelThreshold, gbc2);

        gbc2.gridx = 1;
        textThreshold = new JTextField();

        if (image.isColorImage()) {
            diff = (float) (Math.max(image.getMaxR(), Math.max(image.getMaxG(), image.getMaxB())) -
                            Math.min(image.getMinR(), Math.min(image.getMinG(), image.getMinB())));
        } else {
            diff = (float) (image.getMax() - image.getMin());
        }

        threshold = 0.1f * diff;
        textThreshold.setText(String.valueOf(threshold));
        textThreshold.setFont(serif12);
        paramPanel.add(textThreshold, gbc2);

        if (image.isColorImage()) {
            gbc2.gridx = 0;
            gbc2.gridy = 4;
            gbc2.gridwidth = 2;
        } else {
            gbc2.gridx = 0;
            gbc2.gridy = 2;
            gbc2.gridwidth = 2;
        }

        neighborCheckBox = new JCheckBox("Include neighbors of low cost path");
        neighborCheckBox.setFont(serif12);
        neighborCheckBox.setSelected(false);
        paramPanel.add(neighborCheckBox, gbc2);

        if (image.isColorImage()) {
            gbc2.gridx = 0;
            gbc2.gridy = 5;
            gbc2.gridwidth = 2;
            reduceCheckBox = new JCheckBox("Filter Cr and Cb at halved dimensions");
            reduceCheckBox.setFont(serif12);
            reduceCheckBox.setSelected(false);
            paramPanel.add(reduceCheckBox, gbc2);
        } // if (image.isColorImage())

        if (image.getNDims() > 2) {

            if (image.isColorImage()) {
                gbc2.gridx = 0;
                gbc2.gridy = 6;
                gbc2.gridwidth = 2;
            } else {
                gbc2.gridx = 0;
                gbc2.gridy = 3;
                gbc2.gridwidth = 2;
            }

            image25DCheckBox = new JCheckBox("Process each slice independently (2.5D)");
            image25DCheckBox.setFont(serif12);
            paramPanel.add(image25DCheckBox, gbc2);
            image25DCheckBox.setSelected(false);
        } // if (image.getNDims > 2)

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setBounds(10, 16, 120, 25);
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

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(outputOptPanel, gbc);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

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

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        tmpStr = textRadiusY.getText();

        if (testParameter(tmpStr, 0.1, 1000.0)) {
            radiusY = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("Radius must be between 0.1 and 1000.0");
            textRadiusY.requestFocus();
            textRadiusY.selectAll();

            return false;
        }

        if (image.isColorImage()) {
            tmpStr = textRadiusCr.getText();

            if (testParameter(tmpStr, 0.1, 1000.0)) {
                radiusCr = Float.valueOf(tmpStr).floatValue();
            } else {
                MipavUtil.displayError("Radius must be between 0.1 and 1000.0");
                textRadiusCr.requestFocus();
                textRadiusCr.selectAll();

                return false;
            }

            tmpStr = textRadiusCb.getText();

            if (testParameter(tmpStr, 0.1, 1000.0)) {
                radiusCb = Float.valueOf(tmpStr).floatValue();
            } else {
                MipavUtil.displayError("Radius must be between 0.1 and 1000.0");
                textRadiusCb.requestFocus();
                textRadiusCb.selectAll();

                return false;
            }
        } // if (image.isColorImage())

        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, 0.0, diff)) {
            threshold = Float.valueOf(tmpStr).floatValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }

        includeNeighbors = neighborCheckBox.isSelected();

        if (image.isColorImage()) {
            reduce = reduceCheckBox.isSelected();
        }

        if (image.getNDims() > 2) {
            image25D = image25DCheckBox.isSelected();
        }

        return true;
    }

}
