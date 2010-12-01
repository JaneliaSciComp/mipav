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
import javax.swing.JCheckBox;
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
    private JTextField[] minInput;
    
    /** JTextfield for maximum value of threshold. */
    private JTextField[] maxInput;
    
    /** JTextfield for minimum value of threshold. */
    private JTextField[] startInput;
    
    /** JTextfield for maximum value of threshold. */
    private JTextField[] stopInput;
    
    /** Minimum intensity. */
    private float[] minIntensity;
    
    /** Maximum intensity. */
    private float[] maxIntensity;
    
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

    private JCheckBox[] maximumCheck;
    private JCheckBox[] minimumCheck;
    private boolean[] maximum;
    private boolean[] minimum;    
    private int[] startSlice;
    private int[] stopSlice;
    private int projection = 0;
    
    private int nDims;
    int[] extents;
    
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
        nDims = image.getExtents().length > 2 ? 3 : 1;
	    extents = new int[nDims];
	    extents[0] = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
	    extents[1] = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
	    extents[2] = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
        if ( image.isColorImage() )
        {
        	init();
        }
        else
        {
        	init2();
        }
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

        	Vector<ModelImage> resultImages = mipAlgo.getResultImage();
            for ( int i = 0; i < resultImages.size(); i++ )
            {
            	ModelImage resultImage = resultImages.get(i);
            	if ( resultImage != null )
            	{
            	try {
            		new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            	} catch (OutOfMemoryError error) {
            		System.gc();
            		MipavUtil.displayError("Out of memory: unable to open new frame");
            	}
            	}
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
    	if ( projection == 0 )
    	{
    		// No images to project.
    		return;
    	}
        try {
            System.gc();

            // Make algorithm
            if (image.isColorImage()) {
               mipAlgo = new AlgorithmMaximumIntensityProjection(image, minIntensityR, maxIntensityR,
                             minIntensityG, maxIntensityG, minIntensityB, maxIntensityB);    
            }
            else {
                mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
                		startSlice, stopSlice,
                		minIntensity, maxIntensity,
                		maximum, minimum, projection );
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
            MipavUtil.displayError("Dialog MIP: unable to allocate enough memory");

            return;
        }
    }
    
    /**
     * When one of the text inputs has been left blank, trying to convert them to ints results in throwing a null
     * pointer exception. This method determines which one of the JTextFields threw the null pointer Exception.
     *
     * @return  The text field that returned null.
     */
    protected JTextField determineNull( JTextField min, JTextField max ) {
        String t;

        try {
            t = min.getText();

            if (t.equals("")) {
                return min;
            }

            t = max.getText();

            if (t.equals("")) {
                return max;
            }
            return min;
            
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogMaximumIntensityProjection reports: Unknown Error");

            return min; // gotta have some thing returned
        }
    }
    
    /**
     * Accessor that sets the minimum value.
     *
     * @param  x  Value to set minimum value to.
     */
    public void setMin(int i, float x) {
        minIntensity[i] = x;
    }
    
    /**
     * Accessor that sets the maximum value.
     *
     * @param  x  Value to set maximum value to.
     */
    public void setMax(int i, float x) {
        maxIntensity[i] = x;
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
        nDims = image.getExtents().length > 2 ? 3 : 1;
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
        	for ( int i = 0; i < nDims; i++ )
        	{
        		setMin(i, scriptParameters.getParams().getFloat("min_value"));
        		setMax(i, scriptParameters.getParams().getFloat("max_value"));
        		if (minIntensity[i] < 0) {
        			throw new ParameterException("min_value", "Cannot have minimum intensity < 0");
        		}

        		if (minIntensity[i] < image.getMin()) {
        			throw new ParameterException("min_value", "Cannot have minimum intensity < image minimum");
        		}

        		if (minIntensity[i] > image.getMax()) {
        			throw new ParameterException("min_value", "Cannot have minimum intensity > image maximum");
        		}

        		if (maxIntensity[i] < 0) {
        			throw new ParameterException("max_value", "Cannot have maximum intensity < 0");
        		}

        		if (maxIntensity[i] < image.getMin()) {
        			throw new ParameterException("max_value", "Cannot have maximum intensity < image minimum");
        		}

        		if (maxIntensity[i] > image.getMax()) {
        			throw new ParameterException("max_value", "Cannot have minimum intensity > image maximum");
        		}
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
    	/*
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
        */
    }
    /**
     * Initializes the GUI components and places them in dialog.
     */
    private void init2() {
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
        
        maximumCheck = new JCheckBox[nDims];
        minimumCheck = new JCheckBox[nDims];
        minInput = new JTextField[nDims];
        maxInput = new JTextField[nDims];
		maximum = new boolean[nDims];
		minimum = new boolean[nDims];
		maxIntensity = new float[nDims];
		minIntensity = new float[nDims];
		startInput = new JTextField[nDims];
		stopInput = new JTextField[nDims];
	    startSlice = new int[nDims];
	    stopSlice = new int[nDims];
	    
        
        String[] projectionLables = new String[]{ "Z Projection", "Y Projection", "X Projection" };
        for ( int i = 0; i < nDims; i++ )
        {
            JPanel thresholdPanel = new JPanel();        
        	//make border
            thresholdPanel.setBorder(buildTitledBorder("Threshold Image " + projectionLables[i]));
            contentBox.add(thresholdPanel);        
            //set layout
            GridBagLayout gbl = new GridBagLayout();
            GridBagConstraints gbc = new GridBagConstraints();
            thresholdPanel.setLayout(gbl);
            gbc.anchor = GridBagConstraints.NORTHWEST;
            
            maximumCheck[i] = new JCheckBox( "Compute Maximum", true );
            maximumCheck[i].addActionListener(this);
            gbc.gridwidth = 2;
            gbl.setConstraints(maximumCheck[i], gbc);
            thresholdPanel.add(maximumCheck[i]);
            minimumCheck[i] = new JCheckBox( "Compute Minimum", false );
            minimumCheck[i].addActionListener(this);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(minimumCheck[i], gbc);
            thresholdPanel.add(minimumCheck[i]);

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
    
            minInput[i] = new JTextField("0", 12);
            minInput[i].addActionListener(this);
            if (haveFloat) {
                minInput[i].setText(Double.toString(image.getMin()));
            }
            else {
                minInput[i].setText(Long.toString((long)image.getMin()));
            }
            MipavUtil.makeNumericsOnly(minInput[i], numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(minInput[i], gbc);
            thresholdPanel.add(minInput[i]);
            
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
    
            maxInput[i] = new JTextField("0", 12);
            maxInput[i].addActionListener(this);
            if (haveFloat) {
                maxInput[i].setText(Double.toString(image.getMax()));
            }
            else {
                maxInput[i].setText(Long.toString((long)image.getMax()));
            }
            MipavUtil.makeNumericsOnly(maxInput[i], numericsPeriod);
    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(maxInput[i], gbc);
            thresholdPanel.add(maxInput[i]);
            
            
            


            // Start Slice:
            thresholdPanel.add(Box.createHorizontalStrut(10));    
            JLabel startLabel = new JLabel("Start Slice:");
            startLabel.setFont(serif12);
            startLabel.setForeground(Color.black);
            startLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(startLabel, gbc);
            thresholdPanel.add(startLabel);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            startInput[i] = new JTextField("0", 12);
            startInput[i].addActionListener(this);
            startInput[i].setText(Integer.toString(0));
            MipavUtil.makeNumericsOnly(startInput[i], numericsPeriod);    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(startInput[i], gbc);
            thresholdPanel.add(startInput[i]);
            
            // Stop SLice
            thresholdPanel.add(Box.createHorizontalStrut(10));    
            JLabel stopLabel = new JLabel("End slice:");
            stopLabel.setFont(serif12);
            stopLabel.setForeground(Color.black);
            stopLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(stopLabel, gbc);
            thresholdPanel.add(stopLabel);
            thresholdPanel.add(Box.createHorizontalStrut(10));
    
            stopInput[i] = new JTextField("0", 12);
            stopInput[i].addActionListener(this);
            stopInput[i].setText(Integer.toString(extents[i]));
            MipavUtil.makeNumericsOnly(stopInput[i], numericsPeriod);    
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(stopInput[i], gbc);
            thresholdPanel.add(stopInput[i]);
            
        }
                
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
                JTextField t = determineNull(minInputR, maxInputR );
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
                JTextField t = determineNull(minInputG, maxInputG);
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
                JTextField t = determineNull(minInputB, maxInputB);
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
        	for ( int i = 0; i < nDims; i++ )
        	{

                try {
            		startSlice[i] = Integer.parseInt(startInput[i].getText());
            		stopSlice[i] = Integer.parseInt(stopInput[i].getText());
            	} catch (NumberFormatException nfe) {
            		// an empty text-field.  decide which one is empty, then alert the user to correct
                    JTextField t = determineNull(startInput[i], stopInput[i]);
                    MipavUtil.displayError("Improper number!");
                    t.requestFocus();
                    t.selectAll();        
                    return false;
            	}
            	if ( startSlice[i] < 0 )
            	{
            		MipavUtil.displayError("Cannot have start slice < 0");
            		startInput[i].requestFocus();
            		startInput[i].selectAll();
            		return false;
            	}
            	if ( startSlice[i] > extents[i] )
            	{
            		MipavUtil.displayError("Cannot have start slice greater than " + extents[i]);
            		startInput[i].requestFocus();
            		startInput[i].selectAll();
            		return false;
            	}
            	if ( startSlice[i] > stopSlice[i] )
            	{
            		MipavUtil.displayError("Cannot have start slice greater than end slice" );
            		startInput[i].requestFocus();
            		startInput[i].selectAll();
            		return false;
            	}
            	if ( stopSlice[i] < 0 )
            	{
            		MipavUtil.displayError("Cannot have stop slice < 0");
            		stopInput[i].requestFocus();
            		stopInput[i].selectAll();
            		return false;
            	}
            	if ( stopSlice[i] > extents[i] )
            	{
            		MipavUtil.displayError("Cannot have stop slice greater than " + extents[i]);
            		startInput[i].requestFocus();
            		startInput[i].selectAll();
            		return false;
            	}
        		
            try {
        		minIntensity[i] = Float.parseFloat(minInput[i].getText()); // Minimum intensity value.
        		maxIntensity[i] = Float.parseFloat(maxInput[i].getText()); // Maximum intensity value.
        	} catch (NumberFormatException nfe) {
        		// an empty text-field.  decide which one is empty, then alert the user to correct
                JTextField t = determineNull(minInput[i], maxInput[i]);
                MipavUtil.displayError("Improper number!");
                t.requestFocus();
                t.selectAll();
    
                return false;
        	}
        	
        	if (minIntensity[i] < 0) {
                MipavUtil.displayError("Cannot have minimum intensity value < 0");
                minInput[i].requestFocus();
                minInput[i].selectAll();
    
                return false;
            }
        	
        	if (minIntensity[i] < image.getMin()) {
                MipavUtil.displayError("Cannot have minimum intensity value < image minimum");
                minInput[i].requestFocus();
                minInput[i].selectAll();
    
                return false;
            }
        	
        	if (minIntensity[i] > image.getMax()) {
                MipavUtil.displayError("Cannot have minimum intensity value > image maximum");
                minInput[i].requestFocus();
                minInput[i].selectAll();
    
                return false;
            }
        	
        	if (maxIntensity[i] < 0) {
                MipavUtil.displayError("Cannot have maximum intensity value < 0");
                maxInput[i].requestFocus();
                maxInput[i].selectAll();
    
                return false;
            }
        	
        	if (maxIntensity[i] < image.getMin()) {
                MipavUtil.displayError("Cannot have maximum intensity value < image minimum");
                maxInput[i].requestFocus();
                maxInput[i].selectAll();
    
                return false;
            }
        	
        	if (maxIntensity [i]> image.getMax()) {
                MipavUtil.displayError("Cannot have maximum intensity value > image maximum");
                maxInput[i].requestFocus();
                maxInput[i].selectAll();
    
                return false;
            }
        	}
        } // else black and white image

    	if ( maximumCheck[0].isSelected() || minimumCheck[0].isSelected() )
    	{
    		projection |= AlgorithmMaximumIntensityProjection.Z_PROJECTION;
    	}
    	if ( (image.getNDims() > 2) && (maximumCheck[1].isSelected() || minimumCheck[1].isSelected()) )
    	{
    		projection |= AlgorithmMaximumIntensityProjection.Y_PROJECTION;
    	}
    	if ( (image.getNDims() > 2) && (maximumCheck[2].isSelected() || minimumCheck[2].isSelected()) )
    	{
    		projection |= AlgorithmMaximumIntensityProjection.X_PROJECTION;
    	}
    	for ( int i = 0; i < nDims; i++ )
    	{
    		maximum[i] = maximumCheck[i].isSelected();
    		minimum[i] = minimumCheck[i].isSelected();
    	}

    	return true;
    }
     

}
