package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
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
 * @version  1.0 February 26, 2008
 * @author   William Gandler
 */
public class JDialogCenterOfMass extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    /** DOCUMENT ME! */
    private static final int DEFAULT = 0;

    /** DOCUMENT ME! */
    private static final int OTSU = 1;

    /** DOCUMENT ME! */
    private static final int MAX_ENT = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------


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
    private JCheckBox otsuCheckbox;

    /** or if the source image is to be replaced. */
    private boolean regionFlag; // true = apply algorithm to the whole image

    /** DOCUMENT ME! */
    private JTextField textThres1;

    /** DOCUMENT ME! */
    private JTextField textThres2;

    /** DOCUMENT ME! */
    private float thres1 = 0.0f;

    /** DOCUMENT ME! */
    private float thres2 = 0.0f;

    /** DOCUMENT ME! */
    private AlgorithmCenterOfMass comAlgo;

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
    public JDialogCenterOfMass() { }

    /**
     * Creates a new JDialogCenterOfMass object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCenterOfMass(Frame theParentFrame, ModelImage im) {
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
            //MipavUtil.showHelp("");
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

        if (algorithm instanceof AlgorithmCenterOfMass) {
            image.clearMask();

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
        }
            

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        comAlgo.finalize();
        comAlgo = null;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    public void calcHistogram() {
        int[] dimExtents = new int[1];
        int type = image.getType();

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

        if (source == otsuCheckbox || source == maxEntCheckbox) {

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
     * 
     */
    protected void callAlgorithm() {

        float[] thresholds = new float[2];
        thresholds[0] = thres1;
        thresholds[1] = thres2;

        // System.err.println("Name is: " + name + " lower thresh: " + thresholds[0] + " upper thresh: " +
        // thresholds[1]);

        try {

            // No need to make new image space because the user has choosen to replace the source image
            // Make the algorithm class
            comAlgo = new AlgorithmCenterOfMass(image, thresholds, regionFlag, isInverse);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            comAlgo.addListener(this);

            createProgressBar(image.getImageName(), comAlgo);

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

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (comAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                comAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Center Of Mass: unable to allocate enough memory");

            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

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

        regionFlag = scriptParameters.doProcessWholeImage();
        thresholdType = scriptParameters.getParams().getInt("threshold_type");

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

        scriptParameters.storeProcessWholeImage(regionFlag);
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_type", thresholdType));

        if (thresholdType == DEFAULT) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("min_threshold", thres1));
            scriptParameters.getParams().put(ParameterFactory.newParameter("max_threshold", thres2));
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("is_inverse_threshold", isInverse));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Center of Mass");
        setResizable(false);

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

        inverseCheckbox = new JCheckBox("Inverse threshold");
        inverseCheckbox.setFont(serif12);
        inverseCheckbox.setSelected(false);
        inverseCheckbox.addItemListener(this);

        otsuCheckbox = new JCheckBox("Otsu threshold");
        otsuCheckbox.setFont(serif12);
        otsuCheckbox.setSelected(false);
        otsuCheckbox.addItemListener(this);

        maxEntCheckbox = new JCheckBox("Maximum entropy threshold");
        maxEntCheckbox.setFont(serif12);
        maxEntCheckbox.setSelected(false);
        maxEntCheckbox.addItemListener(this);

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
        
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridy = 2;
        scalePanel.add(otsuCheckbox, gbc);
        gbc.gridy = 3;
        scalePanel.add(maxEntCheckbox, gbc);
        gbc.gridy = 4;
        scalePanel.add(inverseCheckbox, gbc);

        JPanel imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Process"));

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

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        mainPanel.add(scalePanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
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
