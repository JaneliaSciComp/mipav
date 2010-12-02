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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;


/**
 * Dialog to call Maximum Intensity Projection. It should be noted that algorithms are executed in own thread.
 *
 * @author  joshim2
 */
public class JDialogMaximumIntensityProjection extends JDialogScriptableBase 
implements AlgorithmInterface, ChangeListener, KeyListener {

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

	private JTextField[] minInputR;
	private JTextField[] maxInputR;
	private JTextField[] minInputG;
	private JTextField[] maxInputG;
	private JTextField[] minInputB;
	private JTextField[] maxInputB;

	private float[] minIntensityR;
	private float[] maxIntensityR;
	private float[] minIntensityG;
	private float[] maxIntensityG;
	private float[] minIntensityB;
	private float[] maxIntensityB;

	private JCheckBox[] maximumCheck;
	private JCheckBox[] minimumCheck;

	private JLabel[] windowLabel;
	private JSlider[] windowSlider;
	
	
	private boolean[] maximum;
	private boolean[] minimum;    
	private int[] startSlice;
	private int[] stopSlice;
	private int[] window;
	private boolean[] projection;

	private int nDims;
	private int[] extents;

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
		nDims = image.getNDims();
		if ( nDims != 3 )
		{
			 MipavUtil.displayError("Intensity Projection is for 3D images only.");
			 dispose();
			 return;
		}
		extents = image.getExtents();
		init2();
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

			Vector<ModelImage> resultImages = ((AlgorithmMaximumIntensityProjection)algorithm).getResultImage();
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
		algorithm.finalize();

	}

	/**
	 * Calls the algorithm.
	 */
	public void callAlgorithm() {

		AlgorithmMaximumIntensityProjection mipAlgo = null;
		for ( int i = 0; i < nDims; i++ )
		{
			if ( projection[i] )
			{
				try {
					// Make algorithm
					if (image.isColorImage()) {
						mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
								startSlice[i], stopSlice[i], window[i],
								minIntensityR[i], maxIntensityR[i],
								minIntensityG[i], maxIntensityG[i], 
								minIntensityB[i], maxIntensityB[i],
								maximum[i], minimum[i], i);    
					}
					else {
						mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
								startSlice[i], stopSlice[i], window[i],
								minIntensity[i], maxIntensity[i],
								maximum[i], minimum[i], i );
					}

					// This is very important. Adding this object as a listener allows the algorithm to
					// notify this object when it has completed of failed. See algorithm performed event.
					// This is made possible by implementing AlgorithmedPerformed interface
					mipAlgo.addListener(this);

					createProgressBar(image.getImageName(), mipAlgo);

					if (isRunInSeparateThread())
					{
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
		}
		// Hide dialog
		setVisible(false);
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

	public void setMinR(int i, float minR) {
		minIntensityR[i] = minR;
	}

	public void setMaxR(int i, float maxR) {
		maxIntensityR[i] = maxR;
	}

	public void setMinG(int i, float minG) {
		minIntensityG[i] = minG;
	}

	public void setMaxG(int i, float maxG) {
		maxIntensityG[i] = maxG;
	}

	public void setMinB(int i, float minB) {
		minIntensityB[i] = minB;
	}

	public void setMaxB(int i, float maxB) {
		maxIntensityB[i] = maxB;
	}

	/**
	 * {@inheritDoc}
	 */
	 protected void setGUIFromParams() {
		 image = scriptParameters.retrieveInputImage();
		 nDims = image.getExtents().length > 2 ? 3 : 1;
		 parentFrame = image.getParentFrame();
		 borderSize = scriptParameters.getParams().getInt("border_size");

		 for ( int i = 0; i < nDims; i++ )
		 {
		 if (image.isColorImage()) {
			 setMinR(i, scriptParameters.getParams().getFloat("min_valuer"));
			 setMaxR(i, scriptParameters.getParams().getFloat("max_valuer"));
			 setMinG(i, scriptParameters.getParams().getFloat("min_valueg"));
			 setMaxG(i, scriptParameters.getParams().getFloat("max_valueg"));
			 setMinB(i, scriptParameters.getParams().getFloat("min_valueb"));
			 setMaxB(i, scriptParameters.getParams().getFloat("max_valueb"));

			 if (minIntensityR[i] < 0) {
				 throw new ParameterException("min_valuer", "Cannot have red minimum intensity < 0");
			 }

			 if (minIntensityR[i] < image.getMinR()) {
				 throw new ParameterException("min_valuer", "Cannot have red minimum intensity < red image minimum");
			 }

			 if (minIntensityR[i] > image.getMaxR()) {
				 throw new ParameterException("min_valuer", "Cannot have red minimum intensity > red image maximum");
			 }

			 if (maxIntensityR[i] < image.getMinR()) {
				 throw new ParameterException("max_valuer", "Cannot have red maximum intensity < red image minimum");
			 }

			 if (maxIntensityR[i] > image.getMaxR()) {
				 throw new ParameterException("max_valuer", "Cannot have red minimum intensity > red image maximum");
			 }

			 if (minIntensityG[i] < 0) {
				 throw new ParameterException("min_valueg", "Cannot have green minimum intensity < 0");
			 }

			 if (minIntensityG[i] < image.getMinG()) {
				 throw new ParameterException("min_valueg", "Cannot have green minimum intensity < green image minimum");
			 }

			 if (minIntensityG[i] > image.getMaxG()) {
				 throw new ParameterException("min_valueg", "Cannot have green minimum intensity > green image maximum");
			 }

			 if (maxIntensityG[i] < image.getMinG()) {
				 throw new ParameterException("max_valueg", "Cannot have green maximum intensity < green image minimum");
			 }

			 if (maxIntensityG[i] > image.getMaxG()) {
				 throw new ParameterException("max_valueg", "Cannot have green minimum intensity > green image maximum");
			 }

			 if (minIntensityB[i] < 0) {
				 throw new ParameterException("min_valueb", "Cannot have blue minimum intensity < 0");
			 }

			 if (minIntensityB[i] < image.getMinB()) {
				 throw new ParameterException("min_valueb", "Cannot have blue minimum intensity < blue image minimum");
			 }

			 if (minIntensityB[i] > image.getMaxB()) {
				 throw new ParameterException("min_valueb", "Cannot have blue minimum intensity > blue image maximum");
			 }

			 if (maxIntensityB[i] < image.getMinB()) {
				 throw new ParameterException("max_valueb", "Cannot have blue maximum intensity < blue image minimum");
			 }

			 if (maxIntensityB[i] > image.getMaxB()) {
				 throw new ParameterException("max_valueb", "Cannot have blue minimum intensity > blue image maximum");
			 }
		 }
		 else {
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

		 setTitle("Intensity Projection");
		 setSize(350, 230);
		 setForeground(Color.black);

		 Box contentBox = new Box(BoxLayout.Y_AXIS);

		 maximumCheck = new JCheckBox[nDims];
		 minimumCheck = new JCheckBox[nDims];
		 maximum = new boolean[nDims];
		 minimum = new boolean[nDims];
		 startInput = new JTextField[nDims];
		 stopInput = new JTextField[nDims];
		 startSlice = new int[nDims];
		 stopSlice = new int[nDims];
		 windowLabel = new JLabel[nDims];
		 windowSlider = new JSlider[nDims];
		 window = new int[nDims];
		 projection = new boolean[nDims];

		 if ( image.isColorImage() )
		 {
			 minInputR = new JTextField[nDims];
			 maxInputR = new JTextField[nDims];
			 minInputG = new JTextField[nDims];
			 maxInputG = new JTextField[nDims];
			 minInputB = new JTextField[nDims];
			 maxInputB = new JTextField[nDims];

			 minIntensityR = new float[nDims];
			 maxIntensityR = new float[nDims];
			 minIntensityG = new float[nDims];
			 maxIntensityG = new float[nDims];
			 minIntensityB = new float[nDims];
			 maxIntensityB = new float[nDims];
		 }
		 else
		 {
			 minInput = new JTextField[nDims];
			 maxInput = new JTextField[nDims];
			 maxIntensity = new float[nDims];
			 minIntensity = new float[nDims];
		 }


		 String[] projectionLables = new String[]{ "X Projection", "Y Projection", "Z Projection" };
		 
		 JTabbedPane tabbedPane = new JTabbedPane();
		 contentBox.add(tabbedPane);
		 for ( int i = 0; i < nDims; i++ )
		 {
			 JPanel thresholdPanel = new JPanel();        
			 tabbedPane.addTab(projectionLables[i], null, thresholdPanel);        
			 //set layout
			 GridBagLayout gbl = new GridBagLayout();
			 GridBagConstraints gbc = new GridBagConstraints();
			 thresholdPanel.setLayout(gbl);
			 gbc.anchor = GridBagConstraints.NORTHWEST;

			 maximumCheck[i] = new JCheckBox( "Compute Maximum", false );
			 maximumCheck[i].addActionListener(this);
			 gbc.gridwidth = 2;
			 gbl.setConstraints(maximumCheck[i], gbc);
			 thresholdPanel.add(maximumCheck[i]);
			 minimumCheck[i] = new JCheckBox( "Compute Minimum", false );
			 minimumCheck[i].addActionListener(this);
			 gbc.gridwidth = GridBagConstraints.REMAINDER;
			 gbl.setConstraints(minimumCheck[i], gbc);
			 thresholdPanel.add(minimumCheck[i]);

			 if ( image.isColorImage() )
			 {

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

				 minInputR[i] = new JTextField("0", 12);
				 minInputR[i].addActionListener(this);
				 if (haveFloat) {
					 minInputR[i].setText(Double.toString(image.getMinR()));
				 }
				 else {
					 minInputR[i].setText(Integer.toString((int)image.getMinR()));
				 }
				 MipavUtil.makeNumericsOnly(minInputR[i], numericsPeriod);

				 gbc.gridwidth = GridBagConstraints.REMAINDER;
				 gbl.setConstraints(minInputR[i], gbc);
				 thresholdPanel.add(minInputR[i]);

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

				 maxInputR[i] = new JTextField("0", 12);
				 maxInputR[i].addActionListener(this);
				 if (haveFloat) {
					 maxInputR[i].setText(Double.toString(image.getMaxR()));
				 }
				 else {
					 maxInputR[i].setText(Integer.toString((int)image.getMaxR()));
				 }
				 MipavUtil.makeNumericsOnly(maxInputR[i], numericsPeriod);

				 gbc.gridwidth = GridBagConstraints.REMAINDER;
				 gbl.setConstraints(maxInputR[i], gbc);
				 thresholdPanel.add(maxInputR[i]);

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

				 minInputG[i] = new JTextField("0", 12);
				 minInputG[i].addActionListener(this);
				 if (haveFloat) {
					 minInputG[i].setText(Double.toString(image.getMinG()));
				 }
				 else {
					 minInputG[i].setText(Integer.toString((int)image.getMinG()));
				 }
				 MipavUtil.makeNumericsOnly(minInputG[i], numericsPeriod);

				 gbc.gridwidth = GridBagConstraints.REMAINDER;
				 gbl.setConstraints(minInputG[i], gbc);
				 thresholdPanel.add(minInputG[i]);

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

				 maxInputG[i] = new JTextField("0", 12);
				 maxInputG[i].addActionListener(this);
				 if (haveFloat) {
					 maxInputG[i].setText(Double.toString(image.getMaxG()));
				 }
				 else {
					 maxInputG[i].setText(Integer.toString((int)image.getMaxG()));
				 }
				 MipavUtil.makeNumericsOnly(maxInputG[i], numericsPeriod);

				 gbc.gridwidth = GridBagConstraints.REMAINDER;
				 gbl.setConstraints(maxInputG[i], gbc);
				 thresholdPanel.add(maxInputG[i]);

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

				 minInputB[i] = new JTextField("0", 12);
				 minInputB[i].addActionListener(this);
				 if (haveFloat) {
					 minInputB[i].setText(Double.toString(image.getMinB()));
				 }
				 else {
					 minInputB[i].setText(Integer.toString((int)image.getMinB()));
				 }
				 MipavUtil.makeNumericsOnly(minInputB[i], numericsPeriod);

				 gbc.gridwidth = GridBagConstraints.REMAINDER;
				 gbl.setConstraints(minInputB[i], gbc);
				 thresholdPanel.add(minInputB[i]);

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

				 maxInputB[i] = new JTextField("0", 12);
				 maxInputB[i].addActionListener(this);
				 if (haveFloat) {
					 maxInputB[i].setText(Double.toString(image.getMaxB()));
				 }
				 else {
					 maxInputB[i].setText(Integer.toString((int)image.getMaxB()));
				 }
				 MipavUtil.makeNumericsOnly(maxInputB[i], numericsPeriod);

				 gbc.gridwidth = GridBagConstraints.REMAINDER;
				 gbl.setConstraints(maxInputB[i], gbc);
				 thresholdPanel.add(maxInputB[i]);
			 }
			 else
			 {
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
			 }




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
			 startInput[i].addKeyListener(this);
			 startInput[i].addFocusListener(this);
			 MipavUtil.makeNumericsOnly(startInput[i], numericsPeriod);    
			 gbc.gridwidth = GridBagConstraints.REMAINDER;
			 gbl.setConstraints(startInput[i], gbc);
			 thresholdPanel.add(startInput[i]);

			 // Stop Slice
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
			 stopInput[i].setText(Integer.toString(extents[i]-1));
			 stopInput[i].addKeyListener(this);
			 stopInput[i].addFocusListener(this);
			 MipavUtil.makeNumericsOnly(stopInput[i], numericsPeriod);    
			 gbc.gridwidth = GridBagConstraints.REMAINDER;
			 gbl.setConstraints(stopInput[i], gbc);
			 thresholdPanel.add(stopInput[i]);
			 
			 // Windowning slider:
			 thresholdPanel.add(Box.createHorizontalStrut(10));    
			 windowLabel[i] = new JLabel("# slices in bracket: " + (extents[i]-1) );
			 windowLabel[i].setFont(serif12);
			 windowLabel[i].setForeground(Color.black);
			 windowLabel[i].setRequestFocusEnabled(false);
			 gbc.gridwidth = 2;
			 gbl.setConstraints(windowLabel[i], gbc);
			 thresholdPanel.add(windowLabel[i]);
			 thresholdPanel.add(Box.createHorizontalStrut(10));
			 
			 windowSlider[i] = new JSlider(0, extents[i]-1, extents[i]-1 );
			 windowSlider[i].addChangeListener(this); 
			 gbc.gridwidth = GridBagConstraints.REMAINDER;
			 gbl.setConstraints(windowSlider[i], gbc);
			 thresholdPanel.add(windowSlider[i]);
			 
		 }

		 maximumCheck[2].setSelected(true);

		 tabbedPane.setSelectedIndex(2);
		 tabbedPane.addChangeListener(this);;

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
			 window[i] = windowSlider[i].getValue();

			 if (image.isColorImage()) {
				 try {
					 minIntensityR[i] = Float.parseFloat(minInputR[i].getText()); // Minimum red intensity value.
					 maxIntensityR[i] = Float.parseFloat(maxInputR[i].getText()); // Maximum red intensity value.
				 } catch (NumberFormatException nfe) {
					 // an empty text-field.  decide which one is empty, then alert the user to correct
					 JTextField t = determineNull(minInputR[i], maxInputR[i] );
					 MipavUtil.displayError("Improper number!");
					 t.requestFocus();
					 t.selectAll();

					 return false;
				 }

				 if (minIntensityR[i] < image.getMinR()) {
					 MipavUtil.displayError("Cannot have minimum red intensity value < red image minimum");
					 minInputR[i].requestFocus();
					 minInputR[i].selectAll();
					 return false;
				 }

				 if (minIntensityR[i] > image.getMaxR()) {
					 MipavUtil.displayError("Cannot have red minimum intensity value > red image maximum");
					 minInputR[i].requestFocus();
					 minInputR[i].selectAll();
					 return false;
				 }

				 if (maxIntensityR[i] < image.getMinR()) {
					 MipavUtil.displayError("Cannot have red maximum intensity value < red image minimum");
					 maxInputR[i].requestFocus();
					 maxInputR[i].selectAll();
					 return false;
				 }

				 if (maxIntensityR[i] > image.getMaxR()) {
					 MipavUtil.displayError("Cannot have red maximum intensity value > red image maximum");
					 maxInputR[i].requestFocus();
					 maxInputR[i].selectAll();
					 return false;
				 }    

				 try {
					 minIntensityG[i] = Float.parseFloat(minInputG[i].getText()); // Green minimum intensity value.
					 maxIntensityG[i] = Float.parseFloat(maxInputG[i].getText()); // Green maximum intensity value.
				 } catch (NumberFormatException nfe) {
					 // an empty text-field.  decide which one is empty, then alert the user to correct
					 JTextField t = determineNull(minInputG[i], maxInputG[i]);
					 MipavUtil.displayError("Improper number!");
					 t.requestFocus();
					 t.selectAll();
					 return false;
				 }

				 if (minIntensityG[i] < image.getMinG()) {
					 MipavUtil.displayError("Cannot have green minimum intensity value < green image minimum");
					 minInputG[i].requestFocus();
					 minInputG[i].selectAll();
					 return false;
				 }

				 if (minIntensityG[i] > image.getMaxG()) {
					 MipavUtil.displayError("Cannot have green minimum intensity value > green image maximum");
					 minInputG[i].requestFocus();
					 minInputG[i].selectAll();
					 return false;
				 }

				 if (maxIntensityG[i] < image.getMinG()) {
					 MipavUtil.displayError("Cannot have green maximum intensity value < green image minimum");
					 maxInputG[i].requestFocus();
					 maxInputG[i].selectAll();
					 return false;
				 }

				 if (maxIntensityG[i] > image.getMaxG()) {
					 MipavUtil.displayError("Cannot have green maximum intensity value > green image maximum");
					 maxInputG[i].requestFocus();
					 maxInputG[i].selectAll();
					 return false;
				 }    

				 try {
					 minIntensityB[i] = Float.parseFloat(minInputB[i].getText()); // Blue minimum intensity value.
					 maxIntensityB[i] = Float.parseFloat(maxInputB[i].getText()); // Blue maximum intensity value.
				 } catch (NumberFormatException nfe) {
					 // an empty text-field.  decide which one is empty, then alert the user to correct
					 JTextField t = determineNull(minInputB[i], maxInputB[i]);
					 MipavUtil.displayError("Improper number!");
					 t.requestFocus();
					 t.selectAll();
					 return false;
				 }

				 if (minIntensityB[i] < image.getMinB()) {
					 MipavUtil.displayError("Cannot have blue minimum intensity value < blue image minimum");
					 minInputB[i].requestFocus();
					 minInputB[i].selectAll();
					 return false;
				 }

				 if (minIntensityB[i] > image.getMaxB()) {
					 MipavUtil.displayError("Cannot have blue minimum intensity value > blue image maximum");
					 minInputB[i].requestFocus();
					 minInputB[i].selectAll();
					 return false;
				 }

				 if (maxIntensityB[i] < image.getMinB()) {
					 MipavUtil.displayError("Cannot have blue maximum intensity value < blue image minimum");
					 maxInputB[i].requestFocus();
					 maxInputB[i].selectAll();
					 return false;
				 }

				 if (maxIntensityB[i] > image.getMaxB()) {
					 MipavUtil.displayError("Cannot have blue maximum intensity value > blue image maximum");
					 maxInputB[i].requestFocus();
					 maxInputB[i].selectAll();
					 return false;
				 }    
			 } // if (image.isColorImage())
			 else
			 {
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

		 for ( int i = 0; i < nDims; i++ )
		 {
			 projection[i] = maximumCheck[i].isSelected() | minimumCheck[i].isSelected();
			 maximum[i] = maximumCheck[i].isSelected();
			 minimum[i] = minimumCheck[i].isSelected();
		 }

		 return true;
	 }

	@Override
	public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();
        for ( int i = 0; i < nDims; i++ )
        {
        	if ( source == windowSlider[i] )
        	{
        		windowLabel[i].setText("# slices in bracket: " + windowSlider[i].getValue() );
        	}
        }
	}

	@Override
	public void keyTyped(KeyEvent e) {}

	@Override
	public void keyPressed(KeyEvent e) {
        Object source = e.getSource();
        int keyCode = e.getKeyCode();
        if (keyCode == KeyEvent.VK_ENTER) {
        	for ( int i = 0; i < nDims; i++ )
        	{
        		if ( source == stopInput[i] || source == startInput[i] )
        		{
        			int newStop = Integer.parseInt( stopInput[i].getText() );
        			int newStart = Integer.parseInt( startInput[i].getText() );
            		if ( source == startInput[i] && newStart > newStop )
            		{
            			newStart = newStop;
            			startInput[i].setText(Integer.toString(newStop));
            		}
            		if ( source == stopInput[i] && newStart > newStop )
            		{
            			newStop = newStart;
            			stopInput[i].setText(Integer.toString(newStart));
            		}
        			int range = newStop-newStart;
        			windowSlider[i].setMaximum(range);
        			if ( windowSlider[i].getValue() > range )
        			{
        				windowSlider[i].setValue(range);
        			}
        		}
        	}
        }
	}

	@Override
	public void keyReleased(KeyEvent e) {}
	
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.dialogs.JDialogBase#focusLost(java.awt.event.FocusEvent)
     */
    public void focusLost(FocusEvent e)
    {
        super.focusLost(e);
        Object source = e.getSource();
        for ( int i = 0; i < nDims; i++ )
        {
        	if ( source == stopInput[i] || source == startInput[i] )
        	{
        		int newStop = Integer.parseInt( stopInput[i].getText() );
        		int newStart = Integer.parseInt( startInput[i].getText() );
        		if ( source == startInput[i] && newStart > newStop )
        		{
        			newStart = newStop;
        			startInput[i].setText(Integer.toString(newStop));
        		}
        		if ( source == stopInput[i] && newStart > newStop )
        		{
        			newStop = newStart;
        			stopInput[i].setText(Integer.toString(newStart));
        		}
        		int range = newStop-newStart;
        		windowSlider[i].setMaximum(range);
        		if ( windowSlider[i].getValue() > range )
        		{
        			windowSlider[i].setValue(range);
        		}
        	}
        }
    }

}
