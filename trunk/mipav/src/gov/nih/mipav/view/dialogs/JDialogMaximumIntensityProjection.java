package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterFloat;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;
import java.util.*;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;


/**
 * Dialog to call Maximum Intensity Projection. It should be noted that algorithms are executed in own thread.
 * The Maximum Intensity Projection algorithm computes the maximum or the minimum intensity along each projection of a 3D image. 
 * The user can specify theshold values in image intensity for the calculation, as well as
 * which projection to compute along (X, Y, or Z), the start and end slice used in the calculation,
 * the size of a sliding 'window' in number of slices to use in the calculation, and whether to calculate the minimum,
 * maximum or both projections.
 * 
 * When the sliding window size is not equal to the number of slices used in the calculation the output is a 3D image,
 * where each slice of the output image is computed using the number of slices in the sliding window. When the
 * sliding window size is equal to the number of slices used in the calculation the output image is a 2D image.
 *
 * @author  joshim2
 */
public class JDialogMaximumIntensityProjection extends JDialogScriptableBase 
		implements ActionDiscovery, AlgorithmInterface, ChangeListener, KeyListener, ViewImageUpdateInterface {

	//~ Static fields/initializers -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = -586175799570928868L;

	private static final String PREVIEW = "PREVIEW";

	private static final String PREV_ON = "On Prev";
	
	private static final String PREV_OFF = "Off Prev";

	//~ Instance fields ------------------------------------------------------------------------------------------------

	/** Source Image. */
	private ModelImage image = null;

	/** JTextfield for minimum value of threshold. */
	private JTextField[] minInput;

	/** JTextfield for maximum value of threshold. */
	private JTextField[] maxInput;

	/** JTextfield for start slice of the computation. */
	private JTextField[] startInput;

	/** JTextfield for end slice of the computation. */
	private JTextField[] stopInput;

	/** Minimum intensity threshold. */
	private float[] minIntensity;

	/** Maximum intensity threshold. */
	private float[] maxIntensity;

	/** JTextfield for minimum Red value of threshold. */
	private JTextField[] minInputR;
	/** JTextfield for maximum Red value of threshold. */
	private JTextField[] maxInputR;
	/** JTextfield for minimum Green value of threshold. */
	private JTextField[] minInputG;
	/** JTextfield for maximum Green value of threshold. */
	private JTextField[] maxInputG;
	/** JTextfield for minimum Blue value of threshold. */
	private JTextField[] minInputB;
	/** JTextfield for maximum Blue value of threshold. */
	private JTextField[] maxInputB;

	/** Minimum Red intensity threshold. */
	private float[] minIntensityR;
	/** Maximum Red intensity threshold. */
	private float[] maxIntensityR;
	/** Minimum Green intensity threshold. */
	private float[] minIntensityG;
	/** Maximum Green intensity threshold. */
	private float[] maxIntensityG;
	/** Minimum Blue intensity threshold. */
	private float[] minIntensityB;
	/** Maximum Blue intensity threshold. */
	private float[] maxIntensityB;

	/** CheckBox turns on/off calculating the maximum projection. */
	private JCheckBox[] maximumCheck;
	/** CheckBox turns on/off calculating the minimum projection. */
	private JCheckBox[] minimumCheck;

	/** Label displays the number of slices in the sliding window. */
	private JLabel[] windowLabel;
	/** Slider enables user to change the size of the sliding window. */
	private ViewJSlider[] windowSlider;

	/** When true, calculate the maximum (X,Y,Z) slices. */
	private boolean[] maximum;
	/** When true, calculate the minimum (X,Y,Z) slices. */
	private boolean[] minimum;    
	/** Value of the start slice for the calculation (X,Y,Z). */
	private int[] startSlice;
	/** Value of the end slice for the calculation (X,Y,Z). */
	private int[] endSlice;
	/** Value of the window size for the calculation (X,Y,Z). */
	private int[] window;
	/** When true output the X,Y,Z projections. */
	private boolean[] projection;
	/** Image dimension, must be 3. */
	private int nDims = 3;
	/** Image extents. */
	private int[] extents;
	/** Single slice preview images */
	private ModelImage minSlicePreview, maxSlicePreview;
	/** The tabbed pane that holds each dimension */
	private JTabbedPane tabbedPane;
	/** Windows containing preview images */
	private ViewJFrameImage minSliceWindow, maxSliceWindow;
	/** Cached value of selected tab */
	private int cachedTab = 2;
	/** Current slice of image */
	private int slice = 0;
	/** Whether preview mode is turned on. */
	private boolean doPreview = false;
	
	/**
	 * Empty constructor needed for dynamic instantiation (used during scripting).
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
		Vector<ViewImageUpdateInterface> updateList = image.getImageFrameVector();
		for(ViewImageUpdateInterface inter : updateList) {
			if(inter instanceof ViewJFrameImage) {
				ViewJFrameImage frame = (ViewJFrameImage)inter;
				this.slice = frame.getComponentImage().getSlice();
			}
		}
		image.addImageDisplayListener(this);
		nDims = image.getNDims();
		if ( nDims != 3 )
		{
			MipavUtil.displayError("Intensity Projection is for 3D images only.");
			dispose();
			return;
		}
		extents = image.getExtents();
		initData();
		init();
	}

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

		} else if (command.equals(PREVIEW)) {
			if(helpButton.getText().equals(PREV_OFF)) {
				helpButton.setText(PREV_ON);
				doPreview = true;
				int i = tabbedPane.getSelectedIndex();
				boolean doPopulate = false;
				if(minimumCheck[i].isSelected()) {
					initMinPreview();
					doPopulate = true;
				} else {
					if(minSliceWindow != null) {
						minSliceWindow.setVisible(false);
					}
				}
				if(maximumCheck[i].isSelected()) {
					initMaxPreview();
					doPopulate = true;
				} else {
					if(maxSliceWindow != null) {
						maxSliceWindow.setVisible(false);
					}
				}
				if(doPopulate) {
					populatePreview();
				}
			} else {
				helpButton.setText(PREV_OFF);
				doPreview = false;
				destroyPreview();
			}
			
		} else if(event.getSource() instanceof JCheckBox) { 
			boolean sourceFound = false;
			for(int i=0; i<minimumCheck.length; i++) {
				if(event.getSource() == minimumCheck[i]) {
					sourceFound = true;
					if(doPreview && minimumCheck[i].isSelected()) {
						initMinPreview();
						populatePreview();
					} else if(minSliceWindow != null) {
						minSliceWindow.setVisible(false);
					}
					
					break;
				}
			}
			
			if(!sourceFound) {
				for(int i=0; i<maximumCheck.length; i++) {
					if(event.getSource() == maximumCheck[i]) {
						sourceFound = true;
						if(doPreview && maximumCheck[i].isSelected()) {
							initMaxPreview();
							populatePreview();
						} else if(minSliceWindow != null) {
							maxSliceWindow.setVisible(false);
						}
						
						break;
					}
				}
			}
			
			if(!sourceFound) {
				System.err.println("Unable to determine if checkbox should be instantiated");
			}
		} else {
            super.actionPerformed(event);
        }
	} 

	private int[] getPreviewExtents() {
		int dim = tabbedPane.getSelectedIndex();
		int[] extents = new int[2];
		if(dim == 0) { //x project
			extents[0] = image.getExtents()[1];
			extents[1] = image.getExtents()[2];
		} else if(dim == 1) { //y project
			extents[0] = image.getExtents()[0];
			extents[1] = image.getExtents()[2];
		} else if(dim == 2) { //z project
			extents[0] = image.getExtents()[0];
			extents[1] = image.getExtents()[1];
		}
		
		return extents;
	}
	
	private void initMinPreview() {
		minSlicePreview = new ModelImage(image.getType(), getPreviewExtents(), "MinPreview_"+image.getImageName());
		minSliceWindow = new ViewJFrameImage(minSlicePreview);
		minSliceWindow.setVisible(false);
	}
	
	private void initMaxPreview() {
		maxSlicePreview = new ModelImage(image.getType(), getPreviewExtents(), "MaxPreview_"+image.getImageName());
		maxSliceWindow = new ViewJFrameImage(maxSlicePreview);
		maxSliceWindow.setVisible(false);
	}
	
	/**
	 * Previews the currently active slice would loosely go with either min or max sliding window.
	 * Current slice is <code>this.slice</code>
	 */
	private void populatePreview() {
		
		int dim = tabbedPane.getSelectedIndex();
		boolean doMin = minimumCheck[dim].isSelected();
		boolean doMax = maximumCheck[dim].isSelected();
		int window = windowSlider[dim].getValue();
		int sliceSize = minSlicePreview != null ? minSlicePreview.getSliceSize() : maxSlicePreview.getSliceSize();
		
		if(doMin || doMax) {
			double[] buffer;
			double[] sliceBuffer;
			try {
				int length = image.getSize();
				buffer = new double[length];
				sliceBuffer = new double[sliceSize];
				image.exportData(0, length, buffer);
			} catch (IOException error) {
				buffer = null;
				System.err.println("Algorithm Maximum Intensity Projection: Image(s) Locked");
				return;
			} catch (OutOfMemoryError e) {
				buffer = null;
				System.err.println("Algorithm Maximum Intensity Projection: Out of Memory");
				return;
			}
			
			int subWindow = window;
			if(window % 2 == 0) {
				subWindow--;
			}
			int offsetBegin = 0, offsetEnd = 0;
			int beginSlice = slice - ((subWindow)/2);
			int minSlice = Integer.valueOf(startInput[dim].getText());
			if(beginSlice < minSlice) {
				offsetBegin = minSlice - beginSlice;
			}
			int endSlice = slice + (window/2); 
			int maxSlice = Integer.valueOf(stopInput[dim].getText());
			if(endSlice > maxSlice) {
				offsetEnd = endSlice - maxSlice;
			}
			
			if(offsetBegin > 0 && offsetEnd > 0) {
				System.err.println("Sliding window exceeds available images, change min/max slices to use entire sliding window.");
				beginSlice = minSlice;
				endSlice = maxSlice;
			} else if(offsetBegin > 0) {
				beginSlice += offsetBegin;
				endSlice += offsetBegin;
			} else if(offsetEnd > 0) {
				beginSlice -= offsetEnd;
				endSlice -= offsetEnd;
			} else {} //no offsets need to be applied in this case
		
			double lowerBound = Double.valueOf(minInput[dim].getText()) - Double.MIN_NORMAL;
			double upperBound = Double.valueOf(maxInput[dim].getText()) + Double.MIN_NORMAL;
			
			if(doMin) { //TODO: add support for complex, color images, and non-z projections
				double smallestVal, currentVal;
				for(int index=0; index<sliceSize; index++) {
					smallestVal = upperBound;
					currentVal = 0.0;
					for(int i=beginSlice; i<=endSlice; i++) {
						currentVal = buffer[index + sliceSize*i];
						if(currentVal < smallestVal && currentVal > lowerBound) {
							smallestVal = currentVal;
						}
					}
					sliceBuffer[index] = smallestVal;
				}
				try {
					minSlicePreview.importData(0, sliceBuffer, false);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			
			if(doMax) {
				double largestVal, currentVal;
				for(int index=0; index<sliceSize; index++) {
					largestVal = lowerBound;
					currentVal = 0.0;
					for(int i=beginSlice; i<=endSlice; i++) {
						currentVal = buffer[index + sliceSize*i];
						if(currentVal > largestVal && currentVal < upperBound) {
							largestVal = currentVal;
						}
					}
					sliceBuffer[index] = largestVal;
				}
				try {
					maxSlicePreview.importData(0, sliceBuffer, false);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		
		if(minSliceWindow != null) {
			minSlicePreview.calcMinMax();
			minSliceWindow.updateImages(true);
			minSliceWindow.setVisible(doMin);
		}
		
		if(maxSliceWindow != null) {
			maxSlicePreview.calcMinMax();
			maxSliceWindow.updateImages(true);
			maxSliceWindow.setVisible(doMax);
		}
		
		image.getParentFrame().setVisible(true);
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
						ViewJFrameImage kFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
						kFrame.updateFrame( image.getParentFrame().getComponentImage().getZoomX(), 
								image.getParentFrame().getComponentImage().getZoomX() );
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
								startSlice[i], endSlice[i], window[i],
								minIntensityR[i], maxIntensityR[i],
								minIntensityG[i], maxIntensityG[i], 
								minIntensityB[i], maxIntensityB[i],
								maximum[i], minimum[i], i);    
					}
					else {
						mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
								startSlice[i], endSlice[i], window[i],
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
	 * @param min one of the JTextFields the threw the null exception.
	 * @param max one of the JTextFields the threw the null exception.
	 * @return either min or max.
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
	 * Sets the minimum value for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param x Value to set minimum value to.
	 */
	public void setMin(int i, float x) {
		minIntensity[i] = x;
	}
	/**
	 * Sets the maximum value for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param x Value to set maximum value to.
	 */
	public void setMax(int i, float x) {
		maxIntensity[i] = x;
	}
	
	/**
	 * Sets the minimum Red value for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param minR Value to set minimum Red value to.
	 */
	public void setMinR(int i, float minR) {
		minIntensityR[i] = minR;
	}
	/**
	 * Sets the maximum Red value for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param maxR Value to set maximum Red value to.
	 */
	public void setMaxR(int i, float maxR) {
		maxIntensityR[i] = maxR;
	}
	
	/**
	 * Sets the minimum Green value for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param minG Value to set minimum Green value to.
	 */
	public void setMinG(int i, float minG) {
		minIntensityG[i] = minG;
	}
	/**
	 * Sets the maximum Green value for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param maxG Value to set maximum Green value to.
	 */
	public void setMaxG(int i, float maxG) {
		maxIntensityG[i] = maxG;
	}
	
	/**
	 * Sets the minimum Blue value for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param minB Value to set minimum Blue value to.
	 */
	public void setMinB(int i, float minB) {
		minIntensityB[i] = minB;
	}
	/**
	 * Sets the maximumBlue value for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param maxB Value to set maximum Blue value to.
	 */
	public void setMaxB(int i, float maxB) {
		maxIntensityB[i] = maxB;
	}

	/**
	 * Sets the start slice for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param start value.
	 */
	public void setStartSlice( int i, int start )
	{
		startSlice[i] = start;
	}
	/**
	 * Sets the end slice for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param end value.
	 */
	public void setEndSlice( int i, int end )
	{
		endSlice[i] = end;
	}

	/**
	 * Sets the window size for X, Y, or Z processing.
	 * @param i index (X, Y, or Z)
	 * @param size, new window size.
	 */
	public void setWindow( int i, int size )
	{
		window[i] = size;
	}

	/**
	 * Turns computing the maximum projection on for X, Y, or Z processing.
	 * @param i index representing (X,Y,Z) projection.
	 * @param compute when true the computation is turned on.
	 */
	public void setComputeMax( int i, boolean compute )
	{
		maximum[i] = compute;
	}
	/**
	 * Turns computing the minimum projection on for X, Y, or Z processing.
	 * @param i index representing (X,Y,Z) projection.
	 * @param compute when true the computation is turned on.
	 */
	public void setComputeMin( int i, boolean compute )
	{
		minimum[i] = compute;
	}
	
	/**
	 * {@inheritDoc}
	 */
	protected void setGUIFromParams() {
		image = scriptParameters.retrieveInputImage();
		nDims = image.getExtents().length > 2 ? 3 : 1;
		parentFrame = image.getParentFrame();
		initData();

		for ( int i = 0; i < nDims; i++ )
		{
			setStartSlice( i, scriptParameters.getParams().getInt("startSlice"+i) );
			setEndSlice( i, scriptParameters.getParams().getInt("endSlice"+i) );
			setWindow( i, scriptParameters.getParams().getInt("window"+i) );
			setComputeMax( i, scriptParameters.getParams().getBoolean("compute_max"+i) );
			setComputeMin( i, scriptParameters.getParams().getBoolean("compute_min"+i) );			
			
			if (image.isColorImage()) {
				setMinR(i, scriptParameters.getParams().getFloat("min_valuer"+i));
				setMaxR(i, scriptParameters.getParams().getFloat("max_valuer"+i));
				setMinG(i, scriptParameters.getParams().getFloat("min_valueg"+i));
				setMaxG(i, scriptParameters.getParams().getFloat("max_valueg"+i));
				setMinB(i, scriptParameters.getParams().getFloat("min_valueb"+i));
				setMaxB(i, scriptParameters.getParams().getFloat("max_valueb"+i));

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
				setMin(i, scriptParameters.getParams().getFloat("min_value"+i));
				setMax(i, scriptParameters.getParams().getFloat("max_value"+i));

				if (minIntensity[i] < image.getMin()) {
					throw new ParameterException("min_value", "Cannot have minimum intensity < image minimum");
				}

				if (minIntensity[i] > image.getMax()) {
					throw new ParameterException("min_value", "Cannot have minimum intensity > image maximum");
				}
				
				if (maxIntensity[i] < image.getMin()) {
					throw new ParameterException("max_value", "Cannot have maximum intensity < image minimum");
				}

				if (maxIntensity[i] > image.getMax()) {
					throw new ParameterException("max_value", "Cannot have minimum intensity > image maximum");
				}
			}
			projection[i] = maximum[i] | minimum[i];
		}
	}

	/**
	 * {@inheritDoc}
	 */
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(image);
		for ( int i = 0; i < nDims; i++ )
		{
			scriptParameters.getParams().put(ParameterFactory.newParameter("startSlice"+i, startSlice[i] ) );
			scriptParameters.getParams().put(ParameterFactory.newParameter("endSlice"+i, endSlice[i] ) );
			scriptParameters.getParams().put(ParameterFactory.newParameter("window"+i, window[i] ) );
			scriptParameters.getParams().put(ParameterFactory.newParameter("compute_max"+i, maximum[i] ) );
			scriptParameters.getParams().put(ParameterFactory.newParameter("compute_min"+i, minimum[i] ) );
			
			if (image.isColorImage()) {
				scriptParameters.getParams().put(ParameterFactory.newParameter("min_valuer"+i, minIntensityR[i]));
				scriptParameters.getParams().put(ParameterFactory.newParameter("max_valuer"+i, maxIntensityR[i]));
				scriptParameters.getParams().put(ParameterFactory.newParameter("min_valueg"+i, minIntensityG[i]));
				scriptParameters.getParams().put(ParameterFactory.newParameter("max_valueg"+i, maxIntensityG[i]));
				scriptParameters.getParams().put(ParameterFactory.newParameter("min_valueb"+i, minIntensityB[i]));
				scriptParameters.getParams().put(ParameterFactory.newParameter("max_valueb"+i, maxIntensityB[i]));
			}
			else {
				scriptParameters.getParams().put(ParameterFactory.newParameter("min_value"+i, minIntensity[i]));
				scriptParameters.getParams().put(ParameterFactory.newParameter("max_value"+i, maxIntensity[i]));
			}
		}
	}

	/**
	 * Allocates local memory.
	 */
	private void initData()
	{
		maximumCheck = new JCheckBox[nDims];
		minimumCheck = new JCheckBox[nDims];
		maximum = new boolean[nDims];
		minimum = new boolean[nDims];
		startInput = new JTextField[nDims];
		stopInput = new JTextField[nDims];
		startSlice = new int[nDims];
		endSlice = new int[nDims];
		windowLabel = new JLabel[nDims];
		windowSlider = new ViewJSlider[nDims];
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

		setTitle("Intensity Projection");
		setSize(350, 230);
		setForeground(Color.black);

		mainDialogPanel.setLayout(new GridBagLayout());
		GridBagConstraints gbcPanel = new GridBagConstraints();
		gbcPanel.weightx = 1;
		gbcPanel.weighty = .9;
		gbcPanel.gridy = 0;
		gbcPanel.fill = GridBagConstraints.BOTH;

		String[] projectionLables = new String[]{ "X Projection", "Y Projection", "Z Projection" };

		tabbedPane = new JTabbedPane();
		
		mainDialogPanel.add(tabbedPane, gbcPanel);
		for ( int i = 0; i < nDims; i++ )
		{
			JPanel thresholdPanel = new JPanel();        
			tabbedPane.addTab(projectionLables[i], null, thresholdPanel);        
			//set layout
			GridBagLayout gbl = new GridBagLayout();
			
			GridBagConstraints gbcCheck = new GridBagConstraints();
			gbcCheck.anchor = GridBagConstraints.NORTHWEST;
			gbcCheck.weightx = .5;
			thresholdPanel.setLayout(gbl);	

			maximumCheck[i] = new JCheckBox( "Compute Maximum", false );
			maximumCheck[i].addActionListener(this);
			gbl.setConstraints(maximumCheck[i], gbcCheck);
			thresholdPanel.add(maximumCheck[i]);
			
			minimumCheck[i] = new JCheckBox( "Compute Minimum", false );
			minimumCheck[i].addActionListener(this);
			gbcCheck.gridwidth = GridBagConstraints.REMAINDER;
			gbl.setConstraints(minimumCheck[i], gbcCheck);
			thresholdPanel.add(minimumCheck[i]);

			GridBagConstraints gbcLabel = new GridBagConstraints();
            gbcLabel.anchor = GridBagConstraints.NORTHWEST;
            gbcLabel.gridx = 0;
            gbcLabel.weightx = .1;
            gbcLabel.insets = new Insets(2, 2, 2, 0);
            
            GridBagConstraints gbcField = new GridBagConstraints();
            gbcField.anchor = GridBagConstraints.NORTHWEST;
            gbcField.gridwidth = GridBagConstraints.REMAINDER;
            gbcField.weightx = .9;
            gbcField.fill = GridBagConstraints.HORIZONTAL;
            gbcField.insets = new Insets(2, 0, 2, 2);
			
			if ( image.isColorImage() )
			{

				// For minR
				JLabel minLabelR = new JLabel("Red threshold minimum:");
				minLabelR.setFont(serif12);
				minLabelR.setForeground(Color.black);
				minLabelR.setRequestFocusEnabled(false);
				gbl.setConstraints(minLabelR, gbcLabel);
				thresholdPanel.add(minLabelR);

				minInputR[i] = new JTextField("0", 12);
				minInputR[i].addActionListener(this);
				if (haveFloat) {
					minInputR[i].setText(Double.toString(image.getMinR()));
				}
				else {
					minInputR[i].setText(Integer.toString((int)image.getMinR()));
				}
				MipavUtil.makeNumericsOnly(minInputR[i], numericsPeriod);
				gbl.setConstraints(minInputR[i], gbcField);
				thresholdPanel.add(minInputR[i]);

				//For maxR
				JLabel maxLabelR = new JLabel("Red threshold maximum:");
				maxLabelR.setFont(serif12);
				maxLabelR.setForeground(Color.black);
				maxLabelR.setRequestFocusEnabled(false);
				gbl.setConstraints(maxLabelR, gbcLabel);
				thresholdPanel.add(maxLabelR);

				maxInputR[i] = new JTextField("0", 12);
				maxInputR[i].addActionListener(this);
				if (haveFloat) {
					maxInputR[i].setText(Double.toString(image.getMaxR()));
				}
				else {
					maxInputR[i].setText(Integer.toString((int)image.getMaxR()));
				}
				MipavUtil.makeNumericsOnly(maxInputR[i], numericsPeriod);
				gbl.setConstraints(maxInputR[i], gbcField);
				thresholdPanel.add(maxInputR[i]);

				// For minG
				JLabel minLabelG = new JLabel("Green threshold minimum:");
				minLabelG.setFont(serif12);
				minLabelG.setForeground(Color.black);
				minLabelG.setRequestFocusEnabled(false);
				gbl.setConstraints(minLabelG, gbcLabel);
				thresholdPanel.add(minLabelG);

				minInputG[i] = new JTextField("0", 12);
				minInputG[i].addActionListener(this);
				if (haveFloat) {
					minInputG[i].setText(Double.toString(image.getMinG()));
				}
				else {
					minInputG[i].setText(Integer.toString((int)image.getMinG()));
				}
				MipavUtil.makeNumericsOnly(minInputG[i], numericsPeriod);
				gbl.setConstraints(minInputG[i], gbcField);
				thresholdPanel.add(minInputG[i]);

				//For maxG
				JLabel maxLabelG = new JLabel("Green threshold maximum:");
				maxLabelG.setFont(serif12);
				maxLabelG.setForeground(Color.black);
				maxLabelG.setRequestFocusEnabled(false);
				gbl.setConstraints(maxLabelG, gbcLabel);
				thresholdPanel.add(maxLabelG);

				maxInputG[i] = new JTextField("0", 12);
				maxInputG[i].addActionListener(this);
				if (haveFloat) {
					maxInputG[i].setText(Double.toString(image.getMaxG()));
				}
				else {
					maxInputG[i].setText(Integer.toString((int)image.getMaxG()));
				}
				MipavUtil.makeNumericsOnly(maxInputG[i], numericsPeriod);
				gbl.setConstraints(maxInputG[i], gbcField);
				thresholdPanel.add(maxInputG[i]);

				// For minB
				JLabel minLabelB = new JLabel("Blue threshold minimum:");
				minLabelB.setFont(serif12);
				minLabelB.setForeground(Color.black);
				minLabelB.setRequestFocusEnabled(false);
				gbl.setConstraints(minLabelB, gbcLabel);
				thresholdPanel.add(minLabelB);

				minInputB[i] = new JTextField("0", 12);
				minInputB[i].addActionListener(this);
				if (haveFloat) {
					minInputB[i].setText(Double.toString(image.getMinB()));
				}
				else {
					minInputB[i].setText(Integer.toString((int)image.getMinB()));
				}
				MipavUtil.makeNumericsOnly(minInputB[i], numericsPeriod);
				gbl.setConstraints(minInputB[i], gbcField);
				thresholdPanel.add(minInputB[i]);

				//For maxB
				JLabel maxLabelB = new JLabel("Blue threshold maximum:");
				maxLabelB.setFont(serif12);
				maxLabelB.setForeground(Color.black);
				maxLabelB.setRequestFocusEnabled(false);
				gbl.setConstraints(maxLabelB, gbcLabel);
				thresholdPanel.add(maxLabelB);

				maxInputB[i] = new JTextField("0", 12);
				maxInputB[i].addActionListener(this);
				if (haveFloat) {
					maxInputB[i].setText(Double.toString(image.getMaxB()));
				}
				else {
					maxInputB[i].setText(Integer.toString((int)image.getMaxB()));
				}
				MipavUtil.makeNumericsOnly(maxInputB[i], numericsPeriod);
				gbl.setConstraints(maxInputB[i], gbcField);
				thresholdPanel.add(maxInputB[i]);
			}
			else
			{
				//For min
				JLabel minLabel = new JLabel("Threshold minimum:");
				minLabel.setFont(serif12);
				minLabel.setForeground(Color.black);
				minLabel.setRequestFocusEnabled(false);
				gbl.setConstraints(minLabel, gbcLabel);
				thresholdPanel.add(minLabel);

				minInput[i] = new JTextField("0", 12);
				minInput[i].addActionListener(this);
				if (haveFloat) {
					minInput[i].setText(Double.toString(image.getMin()));
				}
				else {
					minInput[i].setText(Long.toString((long)image.getMin()));
				}
				MipavUtil.makeNumericsOnly(minInput[i], numericsPeriod);
				gbl.setConstraints(minInput[i], gbcField);
				thresholdPanel.add(minInput[i]);

				//For max
				JLabel maxLabel = new JLabel("Threshold maximum:");
				maxLabel.setFont(serif12);
				maxLabel.setForeground(Color.black);
				maxLabel.setRequestFocusEnabled(false);
				gbl.setConstraints(maxLabel, gbcLabel);
				thresholdPanel.add(maxLabel);

				maxInput[i] = new JTextField("0", 12);
				maxInput[i].addActionListener(this);
				if (haveFloat) {
					maxInput[i].setText(Double.toString(image.getMax()));
				}
				else {
					maxInput[i].setText(Long.toString((long)image.getMax()));
				}
				MipavUtil.makeNumericsOnly(maxInput[i], numericsPeriod);
				gbl.setConstraints(maxInput[i], gbcField);
				thresholdPanel.add(maxInput[i]);
			}
			
			




			// Start Slice:   
			JLabel startLabel = new JLabel("Start Slice:");
			startLabel.setFont(serif12);
			startLabel.setForeground(Color.black);
			startLabel.setRequestFocusEnabled(false);
			gbl.setConstraints(startLabel, gbcLabel);
			thresholdPanel.add(startLabel);

			startInput[i] = new JTextField("0", 12);
			startInput[i].addActionListener(this);
			startInput[i].setText(Integer.toString(0));
			startInput[i].addKeyListener(this);
			startInput[i].addFocusListener(this);
			MipavUtil.makeNumericsOnly(startInput[i], numericsPeriod);    
			gbl.setConstraints(startInput[i], gbcField);
			thresholdPanel.add(startInput[i]);

			// Stop Slice  
			JLabel stopLabel = new JLabel("End slice:");
			stopLabel.setFont(serif12);
			stopLabel.setForeground(Color.black);
			stopLabel.setRequestFocusEnabled(false);
			gbl.setConstraints(stopLabel, gbcLabel);
			thresholdPanel.add(stopLabel);

			stopInput[i] = new JTextField("0", 12);
			stopInput[i].addActionListener(this);
			stopInput[i].setText(Integer.toString(extents[i]-1));
			stopInput[i].addKeyListener(this);
			stopInput[i].addFocusListener(this);
			MipavUtil.makeNumericsOnly(stopInput[i], numericsPeriod);    
			gbl.setConstraints(stopInput[i], gbcField);
			thresholdPanel.add(stopInput[i]);

			// Windowing slider:
			windowLabel[i] = new JLabel("# slices in bracket: " + extents[i] );
			windowLabel[i].setFont(serif12);
			windowLabel[i].setForeground(Color.black);
			windowLabel[i].setRequestFocusEnabled(false);
			gbcLabel.insets = new Insets(4, 2, 8, 0);
			gbl.setConstraints(windowLabel[i], gbcLabel);
			thresholdPanel.add(windowLabel[i]);

			windowSlider[i] = new ViewJSlider( ViewJSlider.SLICE, 1, extents[i], extents[i] );
			windowSlider[i].addChangeListener(this); 
			gbcField.insets = new Insets(4, 0, 8, 2);
			gbl.setConstraints(windowSlider[i], gbcField);
			thresholdPanel.add(windowSlider[i]);

		}

		maximumCheck[2].setSelected(true);

		tabbedPane.setSelectedIndex(2);
		tabbedPane.addChangeListener(this);;

		gbcPanel.weightx = 1;
        gbcPanel.weighty = .1;
        gbcPanel.gridy = 1;
        gbcPanel.fill = GridBagConstraints.HORIZONTAL;
		mainDialogPanel.add(buildButtons(), gbcPanel);
		helpButton.setText(PREV_OFF);
		helpButton.setActionCommand(PREVIEW);
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
				endSlice[i] = Integer.parseInt(stopInput[i].getText());
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
			if ( startSlice[i] > (extents[i]-1) )
			{
				MipavUtil.displayError("Cannot have start slice greater than " + (extents[i]-1));
				startInput[i].requestFocus();
				startInput[i].selectAll();
				return false;
			}
			if ( startSlice[i] > endSlice[i] )
			{
				MipavUtil.displayError("Cannot have start slice greater than end slice" );
				startInput[i].requestFocus();
				startInput[i].selectAll();
				return false;
			}
			if ( endSlice[i] < 0 )
			{
				MipavUtil.displayError("Cannot have end slice < 0");
				stopInput[i].requestFocus();
				stopInput[i].selectAll();
				return false;
			}
			if ( endSlice[i] > (extents[i]-1) )
			{
				MipavUtil.displayError("Cannot have end slice greater than " + (extents[i]-1));
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

				if (minIntensity[i] < image.getMin()) {
					MipavUtil.displayError("Cannot have minimum intensity value < " + image.getMin());
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
			maximum[i] = maximumCheck[i].isSelected();
			minimum[i] = minimumCheck[i].isSelected();
			projection[i] = maximum[i] | minimum[i];
		}

		return true;
	}

	@Override
	public void stateChanged(ChangeEvent e) {
		Object source = e.getSource();
		ViewJSlider sourceSlider = null;
		for ( int i = 0; i < nDims; i++ )
		{
			if ( source == windowSlider[i] )
			{
				windowLabel[i].setText("# slices in bracket: " + windowSlider[i].getValue() );
				sourceSlider = windowSlider[i];
				if(doPreview) {
					populatePreview();
				}
				break;
			}
		}
		
		if(source == tabbedPane) {
			int i = tabbedPane.getSelectedIndex();
			if(i != cachedTab) {
				if(doPreview) {
			
					if(minimumCheck[i].isSelected()) {
						initMinPreview();
					}
					if(maximumCheck[i].isSelected()) {
						initMaxPreview();
					}
					populatePreview();
				} else {
					destroyPreview();
				}
			}
		}
	}
	
	private void destroyPreview() {
		if(minSlicePreview != null) {
			minSlicePreview.disposeLocal(false);
			minSlicePreview = null;
		}
		
		if(minSliceWindow != null) {
			minSliceWindow.dispose();
			minSliceWindow = null;
		}
		
		if(maxSlicePreview != null) {
			maxSlicePreview.disposeLocal(false);
			maxSlicePreview = null;
		}
		
		if(maxSliceWindow != null) {
			maxSliceWindow.dispose();
			maxSliceWindow = null;
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
					int range = Math.max( 0, Math.min( extents[i], 1 + newStop-newStart ) );
					windowSlider[i].setMaximum(range);
					if ( windowSlider[i].getValue() > range )
					{
						windowSlider[i].setValue(range);
					}
					helpButton.setText(PREV_OFF);
					destroyPreview();
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
				int range = Math.max( 0, Math.min( extents[i], 1 + newStop-newStart ) );
				windowSlider[i].setMaximum(range);
				if ( windowSlider[i].getValue() > range )
				{
					windowSlider[i].setValue(range);
				}
			}
		}
	}

	@Override
	public ActionMetadata getActionMetadata() { 
		return new MipavActionMetadata() {
			public String getCategory() {
				return new String("Utilities");
			}

			public String getDescription() {
				return new String("Computes the maximum and/or minimum intensity projection of an image.");
			}

			public String getDescriptionLong() {
				return new String("Computes the maximum and/or minimum intensity projection of an image.");
			}

			public String getShortLabel() {
				return new String("IntensityProjection");
			}

			public String getLabel() {
				return new String("Intensity Projection");
			}

			public String getName() {
				return new String("Intensity Projection");
			}
		};
	}

	@Override
	public ParameterTable createInputParameters() {
		final ParameterTable table = new ParameterTable();

		try {
			table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
			String[] labels = new String[]{ "X", "Y", "Z" };
			for ( int i = 0; i < nDims; i++ )
			{
				table.put( new ParameterInt( "startSlice_"+labels[i]+"_projection" ) );
				table.put( new ParameterInt( "endSlice_"+labels[i]+"_projection" ) );
				table.put( new ParameterInt( "window_"+labels[i]+"_projection" ) );
				table.put( new ParameterBoolean( "output_maximum_image_"+labels[i]+"_projection" ) );
				table.put( new ParameterBoolean( "output_minimum_image_"+labels[i]+"_projection" ) );
				
				table.put( new ParameterFloat("minimum_threshold_value_"+labels[i]+"_projection" ));
				table.put( new ParameterFloat("max_threshold_value_"+labels[i]+"_projection" ));
			}
		} catch (final ParserException e) {
			// this shouldn't really happen since there isn't any real parsing going on...
			e.printStackTrace();
		}

		return table;
	}

	@Override
	public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
        	table.put(new ParameterImage("X_Minimum_Projection"));
        	table.put(new ParameterImage("X_Maximum_Projection"));
        	table.put(new ParameterImage("Y_Minimum_Projection"));
        	table.put(new ParameterImage("Y_Maximum_Projection"));
        	table.put(new ParameterImage("Z_Minimum_Projection"));
        	table.put(new ParameterImage("Z_Maximum_Projection"));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;

	}

	@Override
	public String getOutputImageName(String imageParamName) {
        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);
        return null;

	}

	@Override
	public boolean isActionComplete() {
		return isComplete();
	}

	@Override
	public void setSlice(int slice) {
		this.slice = slice;
		if(doPreview) {
			populatePreview();
		}
	}

	@Override
	public void setTimeSlice(int tSlice) {
		return; //algorithm not implemented for 4D images
	}

	@Override
	public boolean updateImageExtents() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean updateImages() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean updateImages(boolean flag) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag,
			int interpMode) {
		// TODO Auto-generated method stub
		return false;
	}

}
