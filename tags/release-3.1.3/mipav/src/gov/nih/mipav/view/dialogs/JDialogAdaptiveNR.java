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
 * @see  AlgorithmAdaptiveNR
 */
public class JDialogAdaptiveNR extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1276771512966999332L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmAdaptiveNR adaptiveNRAlgo;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private float distWeight;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelDistWeight;

    /** DOCUMENT ME! */
    private JLabel labelRadiusCb;

    /** DOCUMENT ME! */
    private JLabel labelRadiusCr;

    /** DOCUMENT ME! */
    private JLabel labelRadiusY;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private float radiusCb;

    /** DOCUMENT ME! */
    private float radiusCr;

    /** DOCUMENT ME! */
    private float radiusY;

    /** DOCUMENT ME! */
    private boolean reduce;

    /** DOCUMENT ME! */
    private JCheckBox reduceCheckBox;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textDistWeight;

    /** DOCUMENT ME! */
    private JTextField textRadiusCb;

    /** DOCUMENT ME! */
    private JTextField textRadiusCr;

    /** DOCUMENT ME! */
    private JTextField textRadiusY;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAdaptiveNR() { }

    /**
     * Creates a new JDialogAdaptiveNR object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogAdaptiveNR(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
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
            MipavUtil.showHelp("10005");
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

        if (algorithm instanceof AlgorithmAdaptiveNR) {
            image.clearMask();

            if ((adaptiveNRAlgo.isCompleted() == true) && (resultImage != null)) {

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

        insertScriptLine();
        adaptiveNRAlgo.finalize();
        adaptiveNRAlgo = null;
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
     * Accessor that sets the edge presevation strength.
     *
     * @param  weight  Value to set distWeight to.
     */
    public void setDistWeight(float weight) {
        distWeight = weight;
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
     * Accessor that sets whether or not Cr and Cb are filtered at halved dimensions.
     *
     * @param  reduc  DOCUMENT ME!
     */
    public void setReduce(boolean reduc) {
        reduce = reduc;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_adaptiveNR");
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
                    resultImage = new ModelImage(ModelImage.ARGB, destExtents, name, userInterface);
                } else {
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                }

                // resultImage = (ModelImage)image.clone();
                // resultImage.setImageName(name);
                // Make algorithm
                adaptiveNRAlgo = new AlgorithmAdaptiveNR(resultImage, image, radiusY, radiusCr, radiusCb, distWeight,
                                                         reduce);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                adaptiveNRAlgo.addListener(this);

                createProgressBar(image.getImageName(), adaptiveNRAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (adaptiveNRAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    adaptiveNRAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AdaptiveNR: unable to allocate enough memory");

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
                adaptiveNRAlgo = new AlgorithmAdaptiveNR(null, image, radiusY, radiusCr, radiusCb, distWeight, reduce);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                adaptiveNRAlgo.addListener(this);

                createProgressBar(image.getImageName(), adaptiveNRAlgo);

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
                    if (adaptiveNRAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    adaptiveNRAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AdaptiveNR: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * {@inheritDoc}
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

        distWeight = scriptParameters.getParams().getFloat("edge_preservation_strength");
        radiusCb = scriptParameters.getParams().getFloat("radius_Cb");
        radiusCr = scriptParameters.getParams().getFloat("radius_Cr");
        radiusY = scriptParameters.getParams().getFloat("radius_Y");
        reduce = scriptParameters.getParams().getBoolean("do_reduce_dims");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("edge_preservation_strength", distWeight));
        scriptParameters.getParams().put(ParameterFactory.newParameter("radius_Cb", radiusCb));
        scriptParameters.getParams().put(ParameterFactory.newParameter("radius_Cr", radiusCr));
        scriptParameters.getParams().put(ParameterFactory.newParameter("radius_Y", radiusY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_reduce_dims", reduce));
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Adaptive Noise Reduction");

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

        labelRadiusY = new JLabel("Y radius ");
        labelRadiusY.setForeground(Color.black);
        labelRadiusY.setFont(serif12);
        paramPanel.add(labelRadiusY, gbc2);

        gbc2.gridx = 1;
        textRadiusY = new JTextField();
        textRadiusY.setText("2.0");
        textRadiusY.setFont(serif12);
        paramPanel.add(textRadiusY, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 1;
        labelRadiusCr = new JLabel("Cr radius ");
        labelRadiusCr.setForeground(Color.black);
        labelRadiusCr.setFont(serif12);

        if (!image.isColorImage()) {
            labelRadiusCr.setEnabled(false);
        }

        paramPanel.add(labelRadiusCr, gbc2);

        gbc2.gridx = 1;
        textRadiusCr = new JTextField();
        textRadiusCr.setText("4.0");
        textRadiusCr.setFont(serif12);

        if (!image.isColorImage()) {
            textRadiusCr.setEnabled(false);
        }

        paramPanel.add(textRadiusCr, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 2;
        labelRadiusCb = new JLabel("Cb radius ");
        labelRadiusCb.setForeground(Color.black);
        labelRadiusCb.setFont(serif12);

        if (!image.isColorImage()) {
            labelRadiusCb.setEnabled(false);
        }

        paramPanel.add(labelRadiusCb, gbc2);

        gbc2.gridx = 1;
        textRadiusCb = new JTextField();
        textRadiusCb.setText("5.0");
        textRadiusCb.setFont(serif12);

        if (!image.isColorImage()) {
            textRadiusCb.setEnabled(false);
        }

        paramPanel.add(textRadiusCb, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 3;
        labelDistWeight = new JLabel("Edge preservation strength ");
        labelDistWeight.setForeground(Color.black);
        labelDistWeight.setFont(serif12);
        paramPanel.add(labelDistWeight, gbc2);

        gbc2.gridx = 1;
        textDistWeight = new JTextField();

        if (image.isColorImage()) {
            textDistWeight.setText("3072.0");
        } else {
            distWeight = (float) ((12.0f / 255.0f) * (image.getMax() - image.getMin()));
            textDistWeight.setText(String.valueOf(distWeight));
        }

        textDistWeight.setFont(serif12);
        paramPanel.add(textDistWeight, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 4;
        gbc2.gridwidth = 2;
        reduceCheckBox = new JCheckBox("Filter Cr and Cb at halved dimensions");
        reduceCheckBox.setFont(serif12);
        reduceCheckBox.setSelected(false);

        if (!image.isColorImage()) {
            reduceCheckBox.setEnabled(false);
        }

        paramPanel.add(reduceCheckBox, gbc2);

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
        setVisible(true);

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

        if (testParameter(tmpStr, 0.0, 1000.0)) {
            radiusY = Float.valueOf(tmpStr).floatValue();
        } else {
            textRadiusY.requestFocus();
            textRadiusY.selectAll();

            return false;
        }

        tmpStr = textRadiusCr.getText();

        if (testParameter(tmpStr, 0.0, 1000.0)) {
            radiusCr = Float.valueOf(tmpStr).floatValue();
        } else {
            textRadiusCr.requestFocus();
            textRadiusCr.selectAll();

            return false;
        }

        tmpStr = textRadiusCb.getText();

        if (testParameter(tmpStr, 0.0, 1000.0)) {
            radiusCb = Float.valueOf(tmpStr).floatValue();
        } else {
            textRadiusCb.requestFocus();
            textRadiusCb.selectAll();

            return false;
        }

        tmpStr = textDistWeight.getText();

        if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
            distWeight = Float.valueOf(tmpStr).floatValue();
        } else {
            textDistWeight.requestFocus();
            textDistWeight.selectAll();

            return false;
        }

        reduce = reduceCheckBox.isSelected();

        return true;
    }

}
