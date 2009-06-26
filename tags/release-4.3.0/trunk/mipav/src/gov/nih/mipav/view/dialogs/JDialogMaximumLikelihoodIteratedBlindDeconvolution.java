package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.view.*;
import java.util.Arrays;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

/**
 * Dialog that launches the Maximum likelihood iterated blind deconvolution
 * algorithm. The dialog allows the user to set six parameters to the algorithm:
 * 
 * numberIterations: the number of times to iterate in the blind deconvolution
 * algorithm showProgress: Display the deconvolved image every showProgress
 * images objectiveNumericalAperature: a property of the lens used to image the
 * sample wavelength: the wavelength of the reflected (for brightfield) or
 * fluorescence light refractiveIndex: the index of refraction of the sample.
 * useMicroscopeSettings: When true, the lens NA, wavelength, and refractive
 * index are used in the deconvolution process
 * 
 * See AlgorithmMaximumLikelihoodIteratedBlindDeconvolution.java for algorithm
 * details.
 */
public class JDialogMaximumLikelihoodIteratedBlindDeconvolution extends JDialogScriptableBase implements AlgorithmInterface {

	// ~ Static fields/initializers
	// -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = 7390265931793649147L;

	// ~ Instance fields
	// ------------------------------------------------------------------------------------------------

	/** The MaximumLikelihood Blind Deconvolution algorithm: */
	AlgorithmMaximumLikelihoodIteratedBlindDeconvolution ibdAlgor;

	/** The number of iterations for calculating the deconvolution: */
	private int numberIterations = 10;

	/** Display the deconvolved image every X images to show progress: */
	private int showProgress = 5;

	/**
	 * The following are physical properties of the imaging lens and sample:
	 */
	/**
	 * The object numerical aperature, a property of the lens used to image the
	 * sample:
	 */
	private float objectiveNumericalAperature = 1.4f;

	/** The reflected or fluorescence wavelength of the sample: */
	private float wavelength = 520f;

	/** The index of refraction of the sample: */
	private float refractiveIndex = 1.518f;

	/**
	 * When true the lens NA, wavelength, and refractive index are used in the
	 * deconvolution process
	 */
	private boolean useMicroscopeSettings = true;

	/** The original data image to be reconstructed: */
	private ModelImage originalImage;
	
	/** The extents of original image */
	private int[] extents;

	/** Number of dimensions in an image e.g 2D, 3D */
	private int dim;

	/** The extents of transformed (to the power of 2) image */
	private int[] newExtents = new int[3];

	/**
	 * The boolean value to determine whether to resample the original image or not. */
	private boolean doResample;

	/** User-Interface components: */
	/** User-Interface for entering the number of deconvolution iterations: */
	private JTextField textNumberIterations;

	/** User-Interface for entering the number of intermediate images to show: */
	private JTextField textShowProgress;

	/** User-Interface for entering the objective numerical aperature: */
	private JTextField textObjectiveNumericalAperature;

	/** User-Interface for entering the sample wavelength: */
	private JTextField textWavelength;

	/** User-Interface for entering the index of refraction: */
	private JTextField textRefractiveIndex;

	/** User-Interface for displaying the original extent X */
	private JTextField textOriginalExtentX;

	/** User-Interface for displaying the original extent Y */
	private JTextField textOriginalExtentY;

	/** User-Interface for displaying the original extent Z */
	private JTextField textOriginalExtentZ;

	/** User-Interface for entering the expected extent X */
	private JTextField textExpectedExtentX;

	/** User-Interface for entering the expected extent Y */
	private JTextField textExpectedExtentY;

	/** User-Interface for entering the expected extent Z */
	private JTextField textExpectedExtentZ;

	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	/**
	 * Creates a new JDialogMaximumLikelihoodIteratedBlindDeconvolution object.
	 * 
	 * @param theParentFrame
	 *            The frame that launched this dialog
	 * @param im
	 *            The source image to be reconstructed
	 */
	public JDialogMaximumLikelihoodIteratedBlindDeconvolution(
			Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);

		originalImage = im;
		extents = originalImage.getExtents();
		dim = extents.length;

		for (int i = 0; i < dim; i++) {
			if (isPowerOf2(extents[i])) {
				newExtents[i] = extents[i];
			} else {
				newExtents[i] = transformDimPowerOf2(extents[i]);
			}
		}

		if (Arrays.equals(newExtents, extents)) {
			doResample = false;
		} else {
			doResample = true;
		}

		init();
		
		
		
		
		
		
	} // end JDialogMaximumLikelihoodIteratedBlindDeconvolution(...)

	// ~ Methods
	// --------------------------------------------------------------------------------------------------------

	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 * 
	 * @param event
	 *            Event that triggers function.
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
			MipavUtil.showHelp("19003");
		} else if (command.equals("MicroscopeOptions")) {
			useMicroscopeSettings = ((JCheckBox) event.getSource()).isSelected();
			textObjectiveNumericalAperature.setEnabled(useMicroscopeSettings);
			textWavelength.setEnabled(useMicroscopeSettings);
			textRefractiveIndex.setEnabled(useMicroscopeSettings);
		} // else if (command.equals("MicroscopeOptions") )
	} // actionPerformed(...)

	/**
	 * Called when the AlgorithmMaximumLikelihoodIteratedBlindDeconvolution has
	 * finished processing the source image. If the algorithm is completed, the
	 * resulting reconstructed image is displayed.
	 * 
	 * @param algorithm
	 *            The algorithm that has finished processing.
	 */
	/* (non-Javadoc)
	 * @see gov.nih.mipav.model.algorithms.AlgorithmInterface#algorithmPerformed(gov.nih.mipav.model.algorithms.AlgorithmBase)
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if ((algorithm instanceof AlgorithmMaximumLikelihoodIteratedBlindDeconvolution)
				&& (ibdAlgor.isCompleted() == true)) {

			ModelImage resultImage = ((AlgorithmMaximumLikelihoodIteratedBlindDeconvolution) algorithm).getReconstructedImage();
			updateFileInfo(originalImage, resultImage);
			resultImage.clearMask();

			/* Display the result: */
			try {
				new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
			} catch (OutOfMemoryError error) {
				System.gc();
				MipavUtil
						.displayError("Out of memory: unable to open new frame for result image");
			}

			ModelImage psfImage = ((AlgorithmMaximumLikelihoodIteratedBlindDeconvolution) algorithm).getPSFImage();
			updateFileInfo(originalImage, psfImage);
			psfImage.clearMask();

			/* Display PSF image; for testing purposes. 
			try {
				new ViewJFrameImage(psfImage, null, new Dimension(610, 220));
			} catch (OutOfMemoryError error) {
				System.gc();
				MipavUtil.displayError("Out of memory: unable to open new frame for psfImage");
			} */ 

			insertScriptLine();

			ibdAlgor.finalize();
			ibdAlgor = null;
		}
	} // end algorithmPerformed(...)

	/**
	 * {@inheritDoc}
	 */
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(originalImage);

		scriptParameters.storeNumIterations(numberIterations);
		scriptParameters.getParams().put(ParameterFactory.newParameter("display_every_n_slices", showProgress));
		scriptParameters.getParams().put(ParameterFactory.newParameter("objective_numerical_aperature", objectiveNumericalAperature));
		scriptParameters.getParams().put(ParameterFactory.newParameter("wavelength_nm", wavelength));
		scriptParameters.getParams().put(ParameterFactory.newParameter("refractive_index", refractiveIndex));
		scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_microscope_settings", useMicroscopeSettings));
	}

	/**
	 * {@inheritDoc}
	 */
	protected void setGUIFromParams() {
		originalImage = scriptParameters.retrieveInputImage();

		numberIterations = scriptParameters.getNumIterations();
		showProgress = scriptParameters.getParams().getInt("display_every_n_slices");
		objectiveNumericalAperature = scriptParameters.getParams().getFloat("objective_numerical_aperature");
		wavelength = scriptParameters.getParams().getFloat("wavelength_nm");
		refractiveIndex = scriptParameters.getParams().getFloat("refractive_index");
		useMicroscopeSettings = scriptParameters.getParams().getBoolean("do_use_microscope_settings");
	}

	/**
	 * Store the result image in the script runner's image table now that the
	 * action execution is finished.
	 */
	protected void doPostAlgorithmActions() {
		AlgorithmParameters.storeImageInRunner(ibdAlgor.getReconstructedImage());
	}

	/**
	 * Once all the necessary variables are set, call the algorithm based on
	 * what type of image this is and whether or not there is a separate
	 * destination image.
	 */
	protected void callAlgorithm() {
		try {
			ibdAlgor = new AlgorithmMaximumLikelihoodIteratedBlindDeconvolution(
					originalImage, numberIterations, showProgress,
					objectiveNumericalAperature, wavelength, refractiveIndex,
					useMicroscopeSettings, newExtents, doResample);

			// This is very important. Adding this object as a listener allows
			// the algorithm to
			// notify this object when it has completed or failed. See algorithm
			// performed event.
			// This is made possible by implementing AlgorithmedPerformed
			// interface
			ibdAlgor.addListener(this);

			createProgressBar("Blind Deconvolution", "Computing iterative blind deconvolution...", ibdAlgor);

			if (isRunInSeparateThread()) {

				// Start the thread as a low priority because we wish to still
				// have user interface work fast.
				if (ibdAlgor.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				ibdAlgor.run();
			} // end if (runInSeperateThread) {} else {}

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog MaximumLikelihoodIteratedBlindDeconvolution: unable to allocate enough memory");
			return;
		}

		dispose();
	} // end callAlgorithm()

	/**
	 * Initializes the user-interface components of the dialog:
	 */
	private void init() {
		setForeground(Color.black);
		setTitle("Maximum Likelihood Iterated Blind Deconvolution");

		JPanel parameterPanel = new JPanel(new GridBagLayout());

		parameterPanel.setForeground(Color.black);
		parameterPanel.setBorder(buildTitledBorder("Parameters"));

		// Maximum number of iterations
		JLabel labelNumberIterations = new JLabel("Number of Iterations ");
		labelNumberIterations.setFont(serif12);

		textNumberIterations = new JTextField();
		textNumberIterations.setColumns(5);
		textNumberIterations.setMaximumSize(labelNumberIterations.getPreferredSize());
		textNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
		textNumberIterations.setText(Integer.toString(numberIterations));
		textNumberIterations.setFont(serif12);

		// Show deconvolved image, every X images:
		JLabel labelShowProgress = new JLabel(
				"Display intermediate images every: ");
		labelShowProgress.setFont(serif12);

		textShowProgress = new JTextField();
		textShowProgress.setColumns(5);
		textShowProgress.setMaximumSize(labelShowProgress.getPreferredSize());
		textShowProgress.setHorizontalAlignment(JTextField.RIGHT);
		textShowProgress.setText(Integer.toString(showProgress));
		textShowProgress.setFont(serif12);

		// Objective numerical aperature
		JLabel labelObjectiveNumericalAperature = new JLabel("Objective Numerical Aperture ");
		labelObjectiveNumericalAperature.setFont(serif12);

		textObjectiveNumericalAperature = new JTextField();
		textObjectiveNumericalAperature.setColumns(5);
		textObjectiveNumericalAperature.setMaximumSize(labelObjectiveNumericalAperature.getPreferredSize());
		textObjectiveNumericalAperature.setHorizontalAlignment(JTextField.RIGHT);
		textObjectiveNumericalAperature.setText(Float.toString(objectiveNumericalAperature));
		textObjectiveNumericalAperature.setFont(serif12);

		// Wavelength
		JLabel labelWavelength = new JLabel("Wavelength (nm)");
		labelWavelength.setFont(serif12);

		textWavelength = new JTextField();
		textWavelength.setColumns(5);
		textWavelength.setMaximumSize(labelWavelength.getPreferredSize());
		textWavelength.setHorizontalAlignment(JTextField.RIGHT);
		textWavelength.setText(Float.toString(wavelength));
		textWavelength.setFont(serif12);

		// Refractive Index
		JLabel labelRefractiveIndex = new JLabel("Refractive Index ");
		labelRefractiveIndex.setFont(serif12);

		textRefractiveIndex = new JTextField();
		textRefractiveIndex.setColumns(5);
		textRefractiveIndex.setMaximumSize(labelRefractiveIndex.getPreferredSize());
		textRefractiveIndex.setHorizontalAlignment(JTextField.RIGHT);
		textRefractiveIndex.setText(Float.toString(refractiveIndex));
		textRefractiveIndex.setFont(serif12);

		JCheckBox checkAdvancedOptions = new JCheckBox("Use microscope settings");
		checkAdvancedOptions.setEnabled(true);
		checkAdvancedOptions.setSelected(true);
		checkAdvancedOptions.addActionListener(this);
		checkAdvancedOptions.setActionCommand("MicroscopeOptions");

		// Panel for Orginal Extents
		JPanel originalExtentsPanel = new JPanel(new GridBagLayout());

		originalExtentsPanel.setForeground(Color.black);
		originalExtentsPanel.setBorder(buildTitledBorder("Original Extents"));

		// Orginal Extent X
		JLabel labelOriginalExtentX = new JLabel("extent X: ");
		labelOriginalExtentX.setFont(serif12);

		textOriginalExtentX = new JTextField();
		textOriginalExtentX.setColumns(5);
		textOriginalExtentX.setMaximumSize(labelOriginalExtentX.getPreferredSize());
		textOriginalExtentX.setHorizontalAlignment(JTextField.RIGHT);
		textOriginalExtentX.setText(Integer.toString(extents[0]));
		textOriginalExtentX.setFont(serif12);
		textOriginalExtentX.setEnabled(false);

		// Orginal Extent Y
		JLabel labelOriginalExtentY = new JLabel("extent Y: ");
		labelOriginalExtentY.setFont(serif12);

		textOriginalExtentY = new JTextField();
		textOriginalExtentY.setColumns(5);
		textOriginalExtentY.setMaximumSize(labelOriginalExtentY.getPreferredSize());
		textOriginalExtentY.setHorizontalAlignment(JTextField.RIGHT);
		textOriginalExtentY.setText(Integer.toString(extents[1]));
		textOriginalExtentY.setFont(serif12);
		textOriginalExtentY.setEnabled(false);

		// Orginal Extent Z
		JLabel labelOriginalExtentZ = new JLabel("extent Z: ");
		labelOriginalExtentZ.setFont(serif12);

		textOriginalExtentZ = new JTextField();
		textOriginalExtentZ.setColumns(5);
		textOriginalExtentZ.setMaximumSize(labelOriginalExtentZ.getPreferredSize());
		textOriginalExtentZ.setHorizontalAlignment(JTextField.RIGHT);
		if (dim >= 3) {
			textOriginalExtentZ.setText(Integer.toString(extents[2]));
			textOriginalExtentZ.setFont(serif12);
		}
		textOriginalExtentZ.setEnabled(false);

		// Panel for Expected Extents
		JPanel expectedExtentsPanel = new JPanel(new GridBagLayout());

		expectedExtentsPanel.setForeground(Color.black);
		expectedExtentsPanel.setBorder(buildTitledBorder("Expected Extents"));

		// Expected Extent X
		JLabel labelExpectedExtentX = new JLabel("extent X: ");
		labelExpectedExtentX.setFont(serif12);

		textExpectedExtentX = new JTextField();
		textExpectedExtentX.setColumns(5);
		textExpectedExtentX.setMaximumSize(labelExpectedExtentX.getPreferredSize());
		textExpectedExtentX.setHorizontalAlignment(JTextField.RIGHT);
		textExpectedExtentX.setText(Integer.toString(newExtents[0]));
		textExpectedExtentX.setFont(serif12);
		if (!doResample) {
			textExpectedExtentX.setEnabled(false);
		}

		// Expected Extent Y
		JLabel labelExpectedExtentY = new JLabel("extent Y: ");
		labelExpectedExtentY.setFont(serif12);

		textExpectedExtentY = new JTextField();
		textExpectedExtentY.setColumns(5);
		textExpectedExtentY.setMaximumSize(labelExpectedExtentY.getPreferredSize());
		textExpectedExtentY.setHorizontalAlignment(JTextField.RIGHT);
		textExpectedExtentY.setText(Integer.toString(newExtents[1]));
		textExpectedExtentY.setFont(serif12);
		if (!doResample) {
			textExpectedExtentY.setEnabled(false);
		}

		// Expected Extent Z
		JLabel labelExpectedExtentZ = new JLabel("extent Z: ");
		labelExpectedExtentZ.setFont(serif12);

		textExpectedExtentZ = new JTextField();
		textExpectedExtentZ.setColumns(5);
		textExpectedExtentZ.setMaximumSize(labelExpectedExtentZ.getPreferredSize());
		textExpectedExtentZ.setHorizontalAlignment(JTextField.RIGHT);
		textExpectedExtentZ.setText(Integer.toString(newExtents[2]));
		textExpectedExtentZ.setFont(serif12);
		if (!doResample || dim < 3)  {
			textExpectedExtentZ.setEnabled(false);
		}

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.anchor = GridBagConstraints.WEST;
		gbc.gridheight = 1;
		gbc.gridwidth = 1;
		gbc.insets = new Insets(3, 3, 3, 3);

		gbc.gridy = 0;
		gbc.gridx = 0;
		parameterPanel.add(labelNumberIterations, gbc);
		gbc.gridx = 1;
		parameterPanel.add(textNumberIterations, gbc);

		gbc.gridy++;
		gbc.gridx = 0;
		parameterPanel.add(labelShowProgress, gbc);
		gbc.gridx = 1;
		parameterPanel.add(textShowProgress, gbc);

		gbc.gridy++;
		gbc.gridx = 0;
		parameterPanel.add(labelObjectiveNumericalAperature, gbc);
		gbc.gridx = 1;
		parameterPanel.add(textObjectiveNumericalAperature, gbc);

		gbc.gridy++;
		gbc.gridx = 0;
		parameterPanel.add(labelWavelength, gbc);
		gbc.gridx = 1;
		parameterPanel.add(textWavelength, gbc);

		gbc.gridy++;
		gbc.gridx = 0;
		parameterPanel.add(labelRefractiveIndex, gbc);
		gbc.gridx = 1;
		parameterPanel.add(textRefractiveIndex, gbc);

		gbc.gridy++;
		gbc.gridx = 0;
		parameterPanel.add(checkAdvancedOptions, gbc);

		gbc.gridx = 0;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.fill = GridBagConstraints.HORIZONTAL;

		GridBagConstraints gbc1 = new GridBagConstraints();
		gbc1.anchor = GridBagConstraints.WEST;
		gbc1.gridheight = 1;
		gbc1.gridwidth = 1;
		gbc1.insets = new Insets(3, 3, 3, 3);

		gbc1.gridy++;
		gbc1.gridx = 0;
		originalExtentsPanel.add(labelOriginalExtentX, gbc1);
		gbc1.gridx = 1;
		originalExtentsPanel.add(textOriginalExtentX, gbc1);

		gbc1.gridy++;
		gbc1.gridx = 0;
		originalExtentsPanel.add(labelOriginalExtentY, gbc1);
		gbc1.gridx = 1;
		originalExtentsPanel.add(textOriginalExtentY, gbc1);

		gbc1.gridy++;
		gbc1.gridx = 0;
		originalExtentsPanel.add(labelOriginalExtentZ, gbc1);
		gbc1.gridx = 1;
		originalExtentsPanel.add(textOriginalExtentZ, gbc1);

		gbc1.gridx = 0;
		gbc1.weightx = 1.0;
		gbc1.weighty = 1.0;
		gbc1.fill = GridBagConstraints.VERTICAL;

		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.anchor = GridBagConstraints.WEST;
		gbc2.gridheight = 1;
		gbc2.gridwidth = 1;
		gbc2.insets = new Insets(3, 3, 3, 3);

		gbc2.gridy++;
		gbc2.gridx = 2;
		expectedExtentsPanel.add(labelExpectedExtentX, gbc2);
		gbc2.gridx = 3;
		expectedExtentsPanel.add(textExpectedExtentX, gbc2);

		gbc2.gridy++;
		gbc2.gridx = 2;
		expectedExtentsPanel.add(labelExpectedExtentY, gbc2);
		gbc2.gridx = 3;
		expectedExtentsPanel.add(textExpectedExtentY, gbc2);

		gbc2.gridy++;
		gbc2.gridx = 2;
		expectedExtentsPanel.add(labelExpectedExtentZ, gbc2);
		gbc2.gridx = 3;
		expectedExtentsPanel.add(textExpectedExtentZ, gbc2);

		gbc2.gridx = 1;
		gbc2.weightx = 1.0;
		gbc2.weighty = 1.0;
		gbc2.fill = GridBagConstraints.VERTICAL;

		JPanel mainPanel = new JPanel(new GridBagLayout());

		gbc.gridy = 0;
		mainPanel.add(parameterPanel, gbc);

		JPanel resamplePanel = new JPanel(new GridBagLayout());

		resamplePanel.setForeground(Color.black);
		resamplePanel.setBorder(buildTitledBorder("Resample to the power of 2"));

		gbc.gridy = 1;
		mainPanel.add(resamplePanel, gbc);

		resamplePanel.add(originalExtentsPanel, gbc1);
		resamplePanel.add(expectedExtentsPanel, gbc2);

		getContentPane().add(mainPanel);
		getContentPane().add(buildButtons(), BorderLayout.SOUTH);

		pack();
		setVisible(true);
		//setResizable(false);
	}

	/**
	 * Gets the user-determined parameters for the algorithm from the
	 * user-interface components.
	 * 
	 * @return true
	 */
	private boolean setVariables() {
		String tmpStr;
		tmpStr = textNumberIterations.getText();
		numberIterations = Integer.parseInt(tmpStr);

		tmpStr = textShowProgress.getText();
		showProgress = Integer.parseInt(tmpStr);

		tmpStr = textObjectiveNumericalAperature.getText();
		objectiveNumericalAperature = Float.parseFloat(tmpStr);

		tmpStr = textWavelength.getText();
		wavelength = Float.parseFloat(tmpStr);

		tmpStr = textRefractiveIndex.getText();
		refractiveIndex = Float.parseFloat(tmpStr);
		
		tmpStr = textExpectedExtentX.getText();
		newExtents[0] = Integer.parseInt(tmpStr);
		if (!isPowerOf2(newExtents[0])) {
			newExtents[0] = transformDimPowerOf2(newExtents[0]);
		}
		
		tmpStr = textExpectedExtentY.getText();
		newExtents[1] = Integer.parseInt(tmpStr);
		if (!isPowerOf2(newExtents[1])) {
			newExtents[1] = transformDimPowerOf2(newExtents[1]);
		}
		
		if (dim >= 3) {
			tmpStr = textExpectedExtentZ.getText();
			newExtents[2] = Integer.parseInt(tmpStr);
			if (!isPowerOf2(newExtents[2])) {
				newExtents[2] = transformDimPowerOf2(newExtents[2]);
			}
		}
		
		if (Arrays.equals(newExtents, extents)) {
			doResample = false;
		} else {
			doResample = true;
		}

		return true;
	} // end setVariables()

	/**
	 * Check if the dimension value is already a power of 2.
	 * 
	 * @param dimValue
	 *            dimension value
	 * 
	 * @return isPower2 true if dimValue is a power of 2, false otherwise.
	 */

	private boolean isPowerOf2(int dimValue) {

		if ((dimValue & -dimValue) == dimValue) { // true if dimValue is power
													// of 2
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Calculate the dimension value to power of 2.
	 * 
	 * @param dimValue     dimension value
	 * 
	 * @return newDimValue dimension value in power of 2.
	 */

	private int transformDimPowerOf2(int dimValue) {

		String binaryStr;
		int indexMSBtoLSB;
		int indexLSBtoMSB;
		int newDimValue;

		binaryStr = Integer.toBinaryString(dimValue);
		indexMSBtoLSB = binaryStr.indexOf('1');
		indexLSBtoMSB = binaryStr.length() - indexMSBtoLSB - 1;
		
		// Find the nearest power of 2. Find the MSB k that is "1" from the binary representation of dimValue.
		// k = 0 is LSB. If bit k-1 is zero, the result is 2^k. Otherwise the result is 2^(k+1).
		if (indexLSBtoMSB == 0) {
			newDimValue = 1;
		} else if (binaryStr.charAt(indexMSBtoLSB + 1) == '1') {
			newDimValue = (int) Math.pow(2, indexLSBtoMSB + 1);
		} else {
			newDimValue = (int) Math.pow(2, indexLSBtoMSB);
		}

		return newDimValue;
	}

} // end class JDialogMaximumLikelihoodIteratedBlindDeconvolution
