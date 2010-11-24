package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;


/**
 * Dialog to call Maximum Intensity Projection. It should be noted that algorithms are executed in own thread.
 *
 * @author  joshim2
 */
public class JDialogMaximumIntensityProjection extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -586175799570928868L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Source Image. */
    private ModelImage image = null;
    
    /** Dialog border size */
    private int borderSize = 0;
    
    /** JTextfield for minimum value of threshold. */
    private JTextField minInput;
    
    /** JTextfield for maximum value of threshold. */
    private JTextField maxInput;
    
    /** Minimum intensity. */
    private float minIntensity;
    
    /** Maximum intensity. */
    private float maxIntensity;
    
    private JTextField minInputR;
    private JTextField maxInputR;
    private JTextField minInputG;
    private JTextField maxInputG;
    private JTextField minInputB;
    private JTextField maxInputB;
    
    private float minIntensityR;
    private float maxIntensityR;
    private float minIntensityG;
    private float maxIntensityG;
    private float minIntensityB;
    private float maxIntensityB;
    
    /** Algorithm instance. */
    private AlgorithmMaximumIntensityProjection mipAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    // Constructors ----------------------------------------------------------------------------------------

    /**
     * Empty contructor needed for dynamic instantiation (used during scripting).
     */

    public JDialogMaximumIntensityProjection() { }

    /**
     * Sets the appropriate variables.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMaximumIntensityProjection(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    // Methods -----------------------------------------------------------------------------------------------

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes Dialog box when the OK button is pressed and calls the algorithm.
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
            // MipavUtil.showHelp("");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmMaximumIntensityProjection) {

            ArrayList resultImages = mipAlgo.getResultImage();
            ModelImage XresultImage = (ModelImage) resultImages.get(0);
            ModelImage YresultImage = (ModelImage) resultImages.get(1);
            ModelImage ZresultImage = (ModelImage) resultImages.get(2);

            /* Display the result */

            try {
                new ViewJFrameImage(XresultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            try {
                new ViewJFrameImage(YresultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            try {
                new ViewJFrameImage(ZresultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            insertScriptLine();

        }

        mipAlgo.finalize();
        mipAlgo = null;

    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            if (image.isColorImage()) {
               mipAlgo = new AlgorithmMaximumIntensityProjection(image, minIntensityR, maxIntensityR,
                             minIntensityG, maxIntensityG, minIntensityB, maxIntensityB);    
            }
            else {
                mipAlgo = new AlgorithmMaximumIntensityProjection(image, minIntensity, maxIntensity);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            mipAlgo.addListener(this);


            createProgressBar(image.getImageName(), mipAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (mipAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                mipAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog VOI Extraction: unable to allocate enough memory");

            return;
        }
    }
    
    /**
     * When one of the text inputs has been left blank, trying to convert them to ints results in throwing a null
     * pointer exception. This method determines which one of the JTextFields threw the null pointer Exception.
     *
     * @return  The text field that returned null.
     */
    protected JTextField determineNull() {
        String t;

        try {
            t = minInput.getText();

            if (t.equals("")) {
                return minInput;
            }

            t = maxInput.getText();

            if (t.equals("")) {
                return maxInput;
            }
            return minInput;
            
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogMaximumIntensityProjection reports: Unknown Error");

            return minInput; // gotta have some thing returned
        }
    }
    
    /**
     * Accessor that sets the minimum value.
     *
     * @param  x  Value to set minimum value to.
     */
    public void setMin(float x) {
        minIntensity = x;
    }
    
    /**
     * Accessor that sets the maximum value.
     *
     * @param  x  Value to set maximum value to.
     */
    public void setMax(float x) {
        maxIntensity = x;
    }
    
    public void setMinR(float minR) {
        minIntensityR = minR;
    }
    
    public void setMaxR(float maxR) {
        maxIntensityR = maxR;
    }
    
    public void setMinG(float minG) {
        minIntensityG = minG;
    }
    
    public void setMaxG(float maxG) {
        maxIntensityG = maxG;
    }
    
    public void setMinB(float minB) {
        minIntensityB = minB;
    }
    
    public void setMaxB(float maxB) {
        maxIntensityB = maxB;
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        borderSize = scriptParameters.getParams().getInt("border_size");
        
        if (image.isColorImage()) {
            setMinR(scriptParameters.getParams().getFloat("min_valuer"));
            setMaxR(scriptParameters.getParams().getFloat("max_valuer"));
            setMinG(scriptParameters.getParams().getFloat("min_valueg"));
            setMaxG(scriptParameters.getParams().getFloat("max_valueg"));
            setMinB(scriptParameters.getParams().getFloat("min_valueb"));
            setMaxB(scriptParameters.getParams().getFloat("max_valueb"));
            
            if (minIntensityR < 0) {
                throw new ParameterException("min_valuer", "Cannot have red minimum intensity < 0");
            }
            
            if (minIntensityR < image.getMinR()) {
                throw new ParameterException("min_valuer", "Cannot have red minimum intensity < red image minimum");
            }
            
            if (minIntensityR > image.getMaxR()) {
                throw new ParameterException("min_valuer", "Cannot have red minimum intensity > red image maximum");
            }
            
            if (maxIntensityR < image.getMinR()) {
                throw new ParameterException("max_valuer", "Cannot have red maximum intensity < red image minimum");
            }
            
            if (maxIntensityR > image.getMaxR()) {
                throw new ParameterException("max_valuer", "Cannot have red minimum intensity > red image maximum");
            }
            
            if (minIntensityG < 0) {
                throw new ParameterException("min_valueg", "Cannot have green minimum intensity < 0");
            }
            
            if (minIntensityG < image.getMinG()) {
                throw new ParameterException("min_valueg", "Cannot have green minimum intensity < green image minimum");
            }
            
            if (minIntensityG > image.getMaxG()) {
                throw new ParameterException("min_valueg", "Cannot have green minimum intensity > green image maximum");
            }
            
            if (maxIntensityG < image.getMinG()) {
                throw new ParameterException("max_valueg", "Cannot have green maximum intensity < green image minimum");
            }
            
            if (maxIntensityG > image.getMaxG()) {
                throw new ParameterException("max_valueg", "Cannot have green minimum intensity > green image maximum");
            }
            
            if (minIntensityB < 0) {
                throw new ParameterException("min_valueb", "Cannot have blue minimum intensity < 0");
            }
            
            if (minIntensityB < image.getMinB()) {
                throw new ParameterException("min_valueb", "Cannot have blue minimum intensity < blue image minimum");
            }
            
            if (minIntensityB > image.getMaxB()) {
                throw new ParameterException("min_valueb", "Cannot have blue minimum intensity > blue image maximum");
            }
            
            if (maxIntensityB < image.getMinB()) {
                throw new ParameterException("max_valueb", "Cannot have blue maximum intensity < blue image minimum");
            }
            
            if (maxIntensityB > image.getMaxB()) {
                throw new ParameterException("max_valueb", "Cannot have blue minimum intensity > blue image maximum");
            }
        }
        else {
            setMin(scriptParameters.getParams().getFloat("min_value"));
            setMax(scriptParameters.getParams().getFloat("max_value"));
            if (minIntensity < 0) {
                throw new ParameterException("min_value", "Cannot have minimum intensity < 0");
            }
            
            if (minIntensity < image.getMin()) {
                throw new ParameterException("min_value", "Cannot have minimum intensity < image minimum");
            }
            
            if (minIntensity > image.getMax()) {
                throw new ParameterException("min_value", "Cannot have minimum intensity > image maximum");
            }
            
            if (maxIntensity < 0) {
                throw new ParameterException("max_value", "Cannot have maximum intensity < 0");
            }
            
            if (maxIntensity < image.getMin()) {
                throw new ParameterException("max_value", "Cannot have maximum intensity < image minimum");
            }
            
            if (maxIntensity > image.getMax()) {
                throw new ParameterException("max_value", "Cannot have minimum intensity > image maximum");
            }
        }
        
       
        
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("borderr_size", borderSize));
        if (image.isColorImage()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("min_valuer", minIntensityR));
            scriptParameters.getParams().put(ParameterFactory.newParameter("max_valuer", maxIntensityR));
            scriptParameters.getParams().put(ParameterFactory.newParameter("min_valueg", minIntensityG));
            scriptParameters.getParams().put(ParameterFactory.newParameter("max_valueg", maxIntensityG));
            scriptParameters.getParams().put(ParameterFactory.newParameter("min_valueb", minIntensityB));
            scriptParameters.getParams().put(ParameterFactory.newParameter("max_valueb", maxIntensityB));
        }
        else {
            scriptParameters.getParams().put(ParameterFactory.newParameter("min_value", minIntensity));
            scriptParameters.getParams().put(ParameterFactory.newParameter("max_value", maxIntensity));
        }
    }
    
    /**
     * Initializes the GUI components and places them in dialog.
     */
    private void init() {
        boolean haveFloat;
        if ((image.getType() == ModelStorageBase.FLOAT) || (image.getType() == ModelStorageBase.DOUBLE) ||
            (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
            haveFloat = true;
        }
        else {
            haveFloat = false;
        }
        
        boolean numericsPeriod = true;    
    	
    	setTitle("Maximum Intensity Projection");
        setSize(350, 230);
        setForeground(Color.black);
        
        Box contentBox = new Box(BoxLayout.Y_AXIS);
        JPanel thresholdPanel = new JPanel();
        
    	//make border
        thresholdPanel.setBorder(buildTitledBorder("Threshold Image"));
        contentBox.add(thresholdPanel);
        
        //set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        thresholdPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        
        if (image.isColorImage()) {
            // For minR
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            JLabel minLabelR = new JLabel("Red threshold minimum:");
            minLabelR.setFont(serif12);
            minLabelR.setForeground(Color.black);
            minLabelR.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(minLabelR, gbc);
            thresholdPanel.add(minLabelR);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            minInputR = new JTextField("0", 12);
            minInputR.addActionListener(this);
            if (haveFloat) {
                minInputR.setText(Double.toString(image.getMinR()));
            }
            else {
                minInputR.setText(Integer.toString((int)image.getMinR()));
            }
            MipavUtil.makeNumericsOnly(minInputR, numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(minInputR, gbc);
            thresholdPanel.add(minInputR);
            
            //For maxR
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            JLabel maxLabelR = new JLabel("Red threshold maximum:");
            maxLabelR.setFont(serif12);
            maxLabelR.setForeground(Color.black);
            maxLabelR.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(maxLabelR, gbc);
            thresholdPanel.add(maxLabelR);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            maxInputR = new JTextField("0", 12);
            maxInputR.addActionListener(this);
            if (haveFloat) {
                maxInputR.setText(Double.toString(image.getMaxR()));
            }
            else {
                maxInputR.setText(Integer.toString((int)image.getMaxR()));
            }
            MipavUtil.makeNumericsOnly(maxInputR, numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(maxInputR, gbc);
            thresholdPanel.add(maxInputR);
            
            // For minG
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            JLabel minLabelG = new JLabel("Green threshold minimum:");
            minLabelG.setFont(serif12);
            minLabelG.setForeground(Color.black);
            minLabelG.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(minLabelG, gbc);
            thresholdPanel.add(minLabelG);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            minInputG = new JTextField("0", 12);
            minInputG.addActionListener(this);
            if (haveFloat) {
                minInputG.setText(Double.toString(image.getMinG()));
            }
            else {
                minInputG.setText(Integer.toString((int)image.getMinG()));
            }
            MipavUtil.makeNumericsOnly(minInputG, numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(minInputG, gbc);
            thresholdPanel.add(minInputG);
            
            //For maxG
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            JLabel maxLabelG = new JLabel("Green threshold maximum:");
            maxLabelG.setFont(serif12);
            maxLabelG.setForeground(Color.black);
            maxLabelG.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(maxLabelG, gbc);
            thresholdPanel.add(maxLabelG);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            maxInputG = new JTextField("0", 12);
            maxInputG.addActionListener(this);
            if (haveFloat) {
                maxInputG.setText(Double.toString(image.getMaxG()));
            }
            else {
                maxInputG.setText(Integer.toString((int)image.getMaxG()));
            }
            MipavUtil.makeNumericsOnly(maxInputG, numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(maxInputG, gbc);
            thresholdPanel.add(maxInputG);
            
            // For minB
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            JLabel minLabelB = new JLabel("Blue threshold minimum:");
            minLabelB.setFont(serif12);
            minLabelB.setForeground(Color.black);
            minLabelB.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(minLabelB, gbc);
            thresholdPanel.add(minLabelB);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            minInputB = new JTextField("0", 12);
            minInputB.addActionListener(this);
            if (haveFloat) {
                minInputB.setText(Double.toString(image.getMinB()));
            }
            else {
                minInputB.setText(Integer.toString((int)image.getMinB()));
            }
            MipavUtil.makeNumericsOnly(minInputB, numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(minInputB, gbc);
            thresholdPanel.add(minInputB);
            
            //For maxB
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            JLabel maxLabelB = new JLabel("Blue threshold maximum:");
            maxLabelB.setFont(serif12);
            maxLabelB.setForeground(Color.black);
            maxLabelB.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(maxLabelB, gbc);
            thresholdPanel.add(maxLabelB);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            maxInputB = new JTextField("0", 12);
            maxInputB.addActionListener(this);
            if (haveFloat) {
                maxInputB.setText(Double.toString(image.getMaxB()));
            }
            else {
                maxInputB.setText(Integer.toString((int)image.getMaxB()));
            }
            MipavUtil.makeNumericsOnly(maxInputB, numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(maxInputB, gbc);
            thresholdPanel.add(maxInputB);
        } // if (image.isColorImage())
        else { // black and white image
        
            //For min
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            JLabel minLabel = new JLabel("Threshold minimum:");
            minLabel.setFont(serif12);
            minLabel.setForeground(Color.black);
            minLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(minLabel, gbc);
            thresholdPanel.add(minLabel);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            minInput = new JTextField("0", 12);
            minInput.addActionListener(this);
            if (haveFloat) {
                minInput.setText(Double.toString(image.getMin()));
            }
            else {
                minInput.setText(Long.toString((long)image.getMin()));
            }
            MipavUtil.makeNumericsOnly(minInput, numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(minInput, gbc);
            thresholdPanel.add(minInput);
            
            //For max
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            JLabel maxLabel = new JLabel("Threshold maximum:");
            maxLabel.setFont(serif12);
            maxLabel.setForeground(Color.black);
            maxLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(maxLabel, gbc);
            thresholdPanel.add(maxLabel);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            maxInput = new JTextField("0", 12);
            maxInput.addActionListener(this);
            if (haveFloat) {
                maxInput.setText(Double.toString(image.getMax()));
            }
            else {
                maxInput.setText(Long.toString((long)image.getMax()));
            }
            MipavUtil.makeNumericsOnly(maxInput, numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(maxInput, gbc);
            thresholdPanel.add(maxInput);
        } // else black and white image
        
        contentBox.add(buildButtons());
        mainDialogPanel.add(contentBox);
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
    	
    	if (image.isColorImage()) {
            try {
                minIntensityR = Float.parseFloat(minInputR.getText()); // Minimum red intensity value.
                maxIntensityR = Float.parseFloat(maxInputR.getText()); // Maximum red intensity value.
            } catch (NumberFormatException nfe) {
                // an empty text-field.  decide which one is empty, then alert the user to correct
                JTextField t = determineNull();
                MipavUtil.displayError("Improper number!");
                t.requestFocus();
                t.selectAll();
    
                return false;
            }
            
            if (minIntensityR < image.getMinR()) {
                MipavUtil.displayError("Cannot have minimum red intensity value < red image minimum");
                minInputR.requestFocus();
                minInputR.selectAll();
    
                return false;
            }
            
            if (minIntensityR > image.getMaxR()) {
                MipavUtil.displayError("Cannot have red minimum intensity value > red image maximum");
                minInputR.requestFocus();
                minInputR.selectAll();
    
                return false;
            }
            
            if (maxIntensityR < image.getMinR()) {
                MipavUtil.displayError("Cannot have red maximum intensity value < red image minimum");
                maxInputR.requestFocus();
                maxInputR.selectAll();
    
                return false;
            }
            
            if (maxIntensityR > image.getMaxR()) {
                MipavUtil.displayError("Cannot have red maximum intensity value > red image maximum");
                maxInputR.requestFocus();
                maxInputR.selectAll();
    
                return false;
            }    
            
            try {
                minIntensityG = Float.parseFloat(minInputG.getText()); // Green minimum intensity value.
                maxIntensityG = Float.parseFloat(maxInputG.getText()); // Green maximum intensity value.
            } catch (NumberFormatException nfe) {
                // an empty text-field.  decide which one is empty, then alert the user to correct
                JTextField t = determineNull();
                MipavUtil.displayError("Improper number!");
                t.requestFocus();
                t.selectAll();
    
                return false;
            }
            
            if (minIntensityG < image.getMinG()) {
                MipavUtil.displayError("Cannot have green minimum intensity value < green image minimum");
                minInputG.requestFocus();
                minInputG.selectAll();
    
                return false;
            }
            
            if (minIntensityG > image.getMaxG()) {
                MipavUtil.displayError("Cannot have green minimum intensity value > green image maximum");
                minInputG.requestFocus();
                minInputG.selectAll();
    
                return false;
            }
            
            if (maxIntensityG < image.getMinG()) {
                MipavUtil.displayError("Cannot have green maximum intensity value < green image minimum");
                maxInputG.requestFocus();
                maxInputG.selectAll();
    
                return false;
            }
            
            if (maxIntensityG > image.getMaxG()) {
                MipavUtil.displayError("Cannot have green maximum intensity value > green image maximum");
                maxInputG.requestFocus();
                maxInputG.selectAll();
    
                return false;
            }    
            
            try {
                minIntensityB = Float.parseFloat(minInputB.getText()); // Blue minimum intensity value.
                maxIntensityB = Float.parseFloat(maxInputB.getText()); // Blue maximum intensity value.
            } catch (NumberFormatException nfe) {
                // an empty text-field.  decide which one is empty, then alert the user to correct
                JTextField t = determineNull();
                MipavUtil.displayError("Improper number!");
                t.requestFocus();
                t.selectAll();
    
                return false;
            }
            
            if (minIntensityB < image.getMinB()) {
                MipavUtil.displayError("Cannot have blue minimum intensity value < blue image minimum");
                minInputB.requestFocus();
                minInputB.selectAll();
    
                return false;
            }
            
            if (minIntensityB > image.getMaxB()) {
                MipavUtil.displayError("Cannot have blue minimum intensity value > blue image maximum");
                minInputB.requestFocus();
                minInputB.selectAll();
    
                return false;
            }
            
            if (maxIntensityB < image.getMinB()) {
                MipavUtil.displayError("Cannot have blue maximum intensity value < blue image minimum");
                maxInputB.requestFocus();
                maxInputB.selectAll();
    
                return false;
            }
            
            if (maxIntensityB > image.getMaxB()) {
                MipavUtil.displayError("Cannot have blue maximum intensity value > blue image maximum");
                maxInputB.requestFocus();
                maxInputB.selectAll();
    
                return false;
            }    
        } // if (image.isColorImage())
        else { // black and white image    
            try {
        		minIntensity = Float.parseFloat(minInput.getText()); // Minimum intensity value.
        		maxIntensity = Float.parseFloat(maxInput.getText()); // Maximum intensity value.
        	} catch (NumberFormatException nfe) {
        		// an empty text-field.  decide which one is empty, then alert the user to correct
                JTextField t = determineNull();
                MipavUtil.displayError("Improper number!");
                t.requestFocus();
                t.selectAll();
    
                return false;
        	}
        	
        	if (minIntensity < 0) {
                MipavUtil.displayError("Cannot have minimum intensity value < 0");
                minInput.requestFocus();
                minInput.selectAll();
    
                return false;
            }
        	
        	if (minIntensity < image.getMin()) {
                MipavUtil.displayError("Cannot have minimum intensity value < image minimum");
                minInput.requestFocus();
                minInput.selectAll();
    
                return false;
            }
        	
        	if (minIntensity > image.getMax()) {
                MipavUtil.displayError("Cannot have minimum intensity value > image maximum");
                minInput.requestFocus();
                minInput.selectAll();
    
                return false;
            }
        	
        	if (maxIntensity < 0) {
                MipavUtil.displayError("Cannot have maximum intensity value < 0");
                maxInput.requestFocus();
                maxInput.selectAll();
    
                return false;
            }
        	
        	if (maxIntensity < image.getMin()) {
                MipavUtil.displayError("Cannot have maximum intensity value < image minimum");
                maxInput.requestFocus();
                maxInput.selectAll();
    
                return false;
            }
        	
        	if (maxIntensity > image.getMax()) {
                MipavUtil.displayError("Cannot have maximum intensity value > image maximum");
                maxInput.requestFocus();
                maxInput.selectAll();
    
                return false;
            }
        } // else black and white image
    	
    	return true;
    }
     

}
