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
    
    /** Jtextfield for minimum value of threshold. */
    private JTextField minInput;
    
    /** Jtextfield for maximum value of threshold. */
    private JTextField maxInput;
    
    /** Minimum intensity. */
    private int minIntensity;
    
    /** Maximum intensity. */
    private int maxIntensity;
    
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
            mipAlgo = new AlgorithmMaximumIntensityProjection(image, minIntensity, maxIntensity);

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
    public void setMin(int x) {
        minIntensity = x;
    }
    
    /**
     * Accessor that sets the maximum value.
     *
     * @param  x  Value to set maximum value to.
     */
    public void setMax(int x) {
        maxIntensity = x;
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        borderSize = scriptParameters.getParams().getInt("border_size");
        
        setMin(scriptParameters.getParams().getInt("min_value"));
        setMax(scriptParameters.getParams().getInt("max_value"));
        
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

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("borderr_size", borderSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_value", minIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_value", maxIntensity));
    }
    
    /**
     * Initializes the GUI components and places them in dialog.
     */
    private void init() {
    	
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

        minInput = new JTextField("0", 4);
        minInput.addActionListener(this);
        minInput.setText(Integer.toString((int)image.getMin()));
        MipavUtil.makeNumericsOnly(minInput, false);

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

        maxInput = new JTextField("0", 4);
        maxInput.addActionListener(this);
        maxInput.setText(Integer.toString((int)image.getMax()));
        MipavUtil.makeNumericsOnly(maxInput, false);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(maxInput, gbc);
        thresholdPanel.add(maxInput);
        
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
    	
    	try {
    		minIntensity = Integer.parseInt(minInput.getText()); // Minimum intensity value.
    		maxIntensity = Integer.parseInt(maxInput.getText()); // Maximum intensity value.
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
    	
    	return true;
    }
     

}
