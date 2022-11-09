package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;

/**
 * Dialog to get user input, then call the crop algorithm.
 * 
 * @version 1.0 June 10, 1999
 * @author Matthew J. McAuliffe, Ph.D.
 */
public class JDialogCrop extends JDialogScriptableBase implements
		AlgorithmInterface {

	// ~ Static fields/initializers
	// -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = 9000755868801202116L;

	// ~ Instance fields
	// ------------------------------------------------------------------------------------------------

	/** DOCUMENT ME! */
	private int borderSize;

	/** DOCUMENT ME! */
	private AlgorithmAddMargins cropAlgo;

	/** DOCUMENT ME! */
	private ModelImage image; // source image

	/** DOCUMENT ME! */
	private float[] imgRes, startPos;

	/** DOCUMENT ME! */
	private float lowXmm, // lower X coordinate in millimeters
			lowYmm, // lower Y coordinate in millimeters
			lowZmm; // lower Z coordinate in millimeters

	/** DOCUMENT ME! */
	private int nDims;

	/** DOCUMENT ME! */
	private ModelImage resultImage = null; // result image

	/** DOCUMENT ME! */
	private int resXUnit, // X resolution, Unit of Measure
			resYUnit; // Y resolution, Unit of Measure

	/** DOCUMENT ME! */
	private JTextField textBorderSize;

	/** DOCUMENT ME! */
	private JTextField textWidth, // in pixels
			textHeight, // in pixels
			textDepth; // in pixels

	/** DOCUMENT ME! */
	private JTextField textXmin, // in pixels
			textYmin, // in pixels
			textZmin, // in pixels
			textLowerXmm, // in millimeters
			textLowerYmm, // in millimeters
			textLowerZmm; // in millimeters

	/** DOCUMENT ME! */
	private ViewUserInterface userInterface;

	/** DOCUMENT ME! */
	private int[] xBounds = new int[2];

	/** DOCUMENT ME! */
	private int[] yBounds = new int[2];

	/** DOCUMENT ME! */
	private int[] zBounds = new int[2];

	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	/**
	 * Empty constructor needed for dynamic instantiation (used during
	 * scripting).
	 */
	public JDialogCrop() {
	}

	/**
	 * Used primarily for the script to store variables and run the algorithm.
	 * 
	 * @param im
	 *            Source image.
	 */
	public JDialogCrop(ModelImage im) {
		super(false);
		userInterface = ViewUserInterface.getReference();
		image = im;
		parentFrame = image.getParentFrame();
	}

	/**
	 * Creates new dialog for cropping image.
	 * 
	 * @param theParentFrame
	 *            Parent frame
	 * @param im
	 *            Source image
	 */
	public JDialogCrop(Frame theParentFrame, ModelImage im) {
		this(theParentFrame, im, false);
	}

	/**
	 * Constructor for doing non-interactive cropping (border will be zero)
	 * primarily used by VOI's pop-up menu
	 * 
	 * @param theParentFrame
	 *            the parent frame
	 * @param im
	 *            the image
	 * @param noninteractive
	 *            if this is true, no dialog will pop up and the algorithm will
	 *            run
	 */
	public JDialogCrop(Frame theParentFrame, ModelImage im,
			boolean noninteractive) {
		super(theParentFrame, false);
		userInterface = ViewUserInterface.getReference();
		image = im;

		if (noninteractive) {
			if (findBounds()) {
				callAlgorithm();
			}
		} else {
			if (calcLowerResol()) {
				init();
			}

		}

	}

	// ~ Methods
	// --------------------------------------------------------------------------------------------------------

	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 * 
	 * @param event
	 *            Event that triggers function
	 */
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();

		String tmpStr;

		if (source == OKButton) {
			System.gc();

			tmpStr = textBorderSize.getText();

			if (testParameter(tmpStr, 0, 50)) {
				borderSize = Integer.valueOf(tmpStr).intValue();
			} else {
				textBorderSize.requestFocus();
				textBorderSize.selectAll();

				return;
			}

			if (!findBounds()) {
				dispose();

				return;
			}

			callAlgorithm();
		} else if (source == cancelButton) {
			dispose();
		} else if (source == helpButton) {
			//MipavUtil.showHelp("U4021");
		    MipavUtil.showWebHelp("Cropping_images");
		} else {
            super.actionPerformed(event);
        }
	}

	// ************************************************************************
	// ************************** Algorithm Events ****************************
	// ************************************************************************

	/**
	 * This method is required if the AlgorithmPerformed interface is
	 * implemented. It is called by the algorithms when it has completed or
	 * failed to to complete, so that the dialog can be display the result image
	 * and/or clean up.
	 * 
	 * @param algorithm
	 *            Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {

		if (algorithm instanceof AlgorithmAddMargins) {

			if ((cropAlgo.isCompleted() == true) && (resultImage != null)) { // in
																				// StoreInDest;
																				// "new
																				// Image"

				try {
					new ViewJFrameImage(resultImage, null, new Dimension(610,
							200));
				} catch (OutOfMemoryError error) {
					MipavUtil
							.displayError("Out of memory: unable to open new frame");
				}

				insertScriptLine();

			} else if ((cropAlgo.isCompleted() == true)
					&& (resultImage == null)) {

				image = cropAlgo.getSrcImage();

				try {
					new ViewJFrameImage(image, null, new Dimension(610, 200));
				} catch (OutOfMemoryError error) {
					System.gc();
					MipavUtil
							.displayError("Out of memory: unable to open new frame");
				}

				insertScriptLine();

			} else if (cropAlgo.isCompleted() == false) {

				// algorithm failed but result image still has garbage
				if (resultImage != null) {
					resultImage.disposeLocal(); // clean up memory
				}

				resultImage = null;

				Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

				for (int i = 0; i < imageFrames.size(); i++) {
					((Frame) (imageFrames.elementAt(i))).setEnabled(true);

					if ((((Frame) (imageFrames.elementAt(i))) != parentFrame)
							&& (parentFrame != null)) {
						userInterface.registerFrame((Frame) (imageFrames
								.elementAt(i)));
					}
				}
			}
		}

		cropAlgo.finalize();
		cropAlgo = null;
		dispose();
	}

	/**
	 * DOCUMENT ME!
	 */
	public void callAlgorithm() {

		try {
			int[] destExtents = null;

			if (image.getNDims() == 2) {
				destExtents = new int[2];
				destExtents[0] = Math.abs(xBounds[1] - xBounds[0])
						+ (2 * borderSize);
				destExtents[1] = Math.abs(yBounds[1] - yBounds[0])
						+ (2 * borderSize);
			} else if (image.getNDims() == 3) {

				if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 3D image
																// to 2D image
					destExtents = new int[2];
					destExtents[0] = Math.abs(xBounds[1] - xBounds[0])
							+ (2 * borderSize);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0])
							+ (2 * borderSize);
				} else {
					destExtents = new int[3];
					destExtents[0] = Math.abs(xBounds[1] - xBounds[0])
							+ (2 * borderSize);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0])
							+ (2 * borderSize);
					destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
				}
			} else if (image.getNDims() == 4) {

				if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 4D image
																// to 3D image
					destExtents = new int[3];
					destExtents[0] = Math.abs(xBounds[1] - xBounds[0])
							+ (2 * borderSize);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0])
							+ (2 * borderSize);
					destExtents[2] = image.getExtents()[3];
				} else {
					destExtents = new int[4];
					destExtents[0] = Math.abs(xBounds[1] - xBounds[0])
							+ (2 * borderSize);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0])
							+ (2 * borderSize);
					destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
					destExtents[3] = image.getExtents()[3];
				}
			} else {
				return;
			}

			// Make result image
			resultImage = new ModelImage(image.getType(), destExtents,
					makeImageName(image.getImageName(), "_crop"));

			int[] xCrop = new int[]{ 0, 0 };
			int[] yCrop = new int[]{ 0, 0 };
			int[] zCrop = new int[]{ 0, 0 };
            if ( destExtents.length > 0 )
            {
                xCrop[0] = -1 * (xBounds[0] - borderSize);
                xCrop[1] = -1 * (xBounds[1] - destExtents[0] - 1);
            }
            if ( destExtents.length > 1 )
            {
                yCrop[0] = -1 * (yBounds[0] - borderSize);
                yCrop[1] = -1 * (yBounds[1] - destExtents[1] - 1);
            }
            if ( destExtents.length > 2 )
            {
                zCrop[0] = -1 * (zBounds[0]);
                zCrop[1] = -1 * (zBounds[1] - destExtents[2] - 1);
            }
            else // 3D to 2D
            {
                zCrop[0] = -1 * (zBounds[0]);
                zCrop[1] = -1 * (zBounds[1]+1);
            }

            cropAlgo = new AlgorithmAddMargins(image, resultImage, 
                    xCrop, yCrop, zCrop);

			// This is very important. Adding this object as a listener allows
			// the algorithm to
			// notify this object when it has completed of failed. See algorithm
			// performed event.
			// This is made possible by implementing AlgorithmedPerformed
			// interface
			cropAlgo.addListener(this);

			createProgressBar(image.getImageName(), cropAlgo);

			// Hide the dialog since the algorithm is about to run.
			setVisible(false);

			if (isRunInSeparateThread()) {

				// Start the thread as a low priority because we wish to still
				// have user interface work fast.
				if (cropAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				cropAlgo.run();
			}
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Crop: unable to allocate enough memory");

			return;
		}

	}

	/**
	 * Accessor that returns the image.
	 * 
	 * @return The result image.
	 */
	public ModelImage getResultImage() {
		return resultImage;
	}

	/**
	 * Accessor that sets the borderSize.
	 * 
	 * @param borderSize
	 *            DOCUMENT ME!
	 */
	public void setBorderSize(int borderSize) {
		this.borderSize = borderSize;
	}

	/**
	 * Accessor that set xBounds.
	 * 
	 * @param xBounds
	 *            DOCUMENT ME!
	 */
	public void setXBounds(int[] xBounds) {
		this.xBounds = xBounds;
	}

	/**
	 * Accessor that set yBounds.
	 * 
	 * @param yBounds
	 *            DOCUMENT ME!
	 */
	public void setYBounds(int[] yBounds) {
		this.yBounds = yBounds;
	}

	/**
	 * Accessor that set zBounds.
	 * 
	 * @param zBounds
	 *            DOCUMENT ME!
	 */
	public void setZBounds(int[] zBounds) {
		this.zBounds = zBounds;
	}

	/**
	 * Store the result image in the script runner's image table now that the
	 * action execution is finished.
	 */
	protected void doPostAlgorithmActions() {

		if (getResultImage() != null) {
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

		borderSize = scriptParameters.getParams().getInt("border_size");

		// fill xBounds, yBounds, zBounds
		if (!findBounds()) {
			throw new ParameterException(AlgorithmParameters
					.getInputImageLabel(1),
					"Unable to determine image cropping bounds.");
		}
	}

	/**
	 * {@inheritDoc}
	 */
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(image);
		scriptParameters.storeOutputImageParams(resultImage,
				(getResultImage() != null));

		scriptParameters.getParams().put(
				ParameterFactory.newParameter("border_size", borderSize));
	}

	/**
	 * This method calculates the lower X, lower Y, and lower Z
	 * coordinates(millimeters) for the CROP VOI region.
	 */
	private boolean calcLowerResol() {

		if (!findBounds()) {
			dispose();
			return false;
		}

		nDims = image.getNDims();
		startPos = (float[]) image.getFileInfo(0).getOrigin();
		imgRes = (float[]) image.getFileInfo(0).getResolutions();
		lowXmm = startPos[0] + (imgRes[0] * xBounds[0]);
		lowYmm = startPos[1] + (imgRes[1] * yBounds[0]);

		if (nDims == 2) {
			lowZmm = 0;
		} else if (nDims >= 3) {
			lowZmm = startPos[2] + (imgRes[2] * zBounds[0]);
		}

		return true;

	}

	/**
	 * This method finds the bounding box when CROP apply to 2D, 3D VOI images.
	 * 
	 * @return <code>True</code> if the bounds were calculated successfully,
	 *         <code>false</code> otherwise.
	 */
	private boolean findBounds() {
		int voiNum, nVOI;
		int nContourVOI = 0;
		ViewVOIVector VOIs = image.getVOIs();
		nVOI = VOIs.size();

		if (nVOI == 0) {
			MipavUtil.displayError("No VOI found in the image.");

			return false;
		}

		for (voiNum = 0; voiNum < nVOI; voiNum++) {

			if (VOIs.VOIAt(voiNum).getCurveType() == VOI.CONTOUR) {
				nContourVOI++;
			}
		}

		if (nContourVOI == 0) {
			MipavUtil.displayError("No VOI contours found in the image.");

			return false;
		} else if (nContourVOI == 1) {

			for (voiNum = 0; voiNum < nVOI; voiNum++) {

				if (VOIs.VOIAt(voiNum).getCurveType() == VOI.CONTOUR) {
					break;
				}
			}
		} else {

			for (voiNum = 0; voiNum < nVOI; voiNum++) {

				if ((VOIs.VOIAt(voiNum).isActive() == true)
						&& (VOIs.VOIAt(voiNum).getCurveType() == VOI.CONTOUR)) {
					break;
				}
			}

			if (voiNum == nVOI) {
				MipavUtil.displayError("A VOI must be selected");

				return false;
			}
		} // else if (nContourVOI > 1)

		VOIs.VOIAt(voiNum).getBounds(xBounds, yBounds, zBounds);

		return true;
	}

	/**
	 * Sets up GUI variables and displays dialog.
	 */
	private void init() {
		setTitle("Crop");

		// Border panel of VOI
		JPanel borderPanel = new JPanel(new GridLayout(1, 2));
		borderPanel.setForeground(Color.black);
		borderPanel.setBorder(buildTitledBorder("Border size of VOI"));

		JLabel labelBorderSize = new JLabel("Border size (0 - 50) pixels ");
		labelBorderSize.setForeground(Color.black);
		labelBorderSize.setFont(serif12);
		borderPanel.add(labelBorderSize);

		textBorderSize = new JTextField(10);
		textBorderSize.setText("0");
		textBorderSize.setFont(serif12);
		borderPanel.add(textBorderSize);

		calcLowerResol();

		// Coordinates panel of VOI
		JPanel coordinatesPanel = new JPanel(new GridLayout(3, 3));
		coordinatesPanel.setForeground(Color.gray);
		coordinatesPanel.setBorder(buildTitledBorder("Coordinates of VOI"));

		JLabel labelXmin = new JLabel("Xmin (pixels) ", JLabel.RIGHT);
		labelXmin.setForeground(Color.black);
		labelXmin.setFont(serif12);
		coordinatesPanel.add(labelXmin);

		textXmin = new JTextField(10);
		textXmin.setText(String.valueOf(xBounds[0]));
		textXmin.setFont(serif12);

		textXmin.setEditable(false);
		coordinatesPanel.add(textXmin);

		JLabel labelYmin = new JLabel("Ymin (pixels) ", JLabel.RIGHT);
		labelYmin.setForeground(Color.black);
		labelYmin.setFont(serif12);
		coordinatesPanel.add(labelYmin);

		textYmin = new JTextField(10);
		textYmin.setText(String.valueOf(yBounds[0]));
		textYmin.setFont(serif12);

		textYmin.setEditable(false);
		coordinatesPanel.add(textYmin);

		JLabel labelZmin = new JLabel("Zmin (pixels) ", JLabel.RIGHT);
		labelZmin.setForeground(Color.black);
		labelZmin.setFont(serif12);
		coordinatesPanel.add(labelZmin);

		textZmin = new JTextField(10);
		textZmin.setText(String.valueOf(zBounds[0]));
		textZmin.setFont(serif12);

		textZmin.setEditable(false);
		coordinatesPanel.add(textZmin);

		resXUnit = image.getFileInfo(0).getUnitsOfMeasure(0);

		JLabel labelLowerXmm = new JLabel("Xmin ("
				+ Unit.getUnitFromLegacyNum(resXUnit).getAbbrev() + ") ",
				JLabel.RIGHT);
		labelLowerXmm.setForeground(Color.black);
		labelLowerXmm.setFont(serif12);
		coordinatesPanel.add(labelLowerXmm);

		textLowerXmm = new JTextField(10);
		textLowerXmm.setText(String.valueOf(lowXmm));
		textLowerXmm.setFont(serif12);

		textLowerXmm.setEditable(false);
		coordinatesPanel.add(textLowerXmm);

		resYUnit = image.getFileInfo(0).getUnitsOfMeasure(1);

		JLabel labelLowerYmm = new JLabel("Ymin ("
				+ Unit.getUnitFromLegacyNum(resYUnit).getAbbrev() + ") ",
				JLabel.RIGHT);
		labelLowerYmm.setForeground(Color.black);
		labelLowerYmm.setFont(serif12);
		coordinatesPanel.add(labelLowerYmm);

		textLowerYmm = new JTextField(10);
		textLowerYmm.setText(String.valueOf(lowYmm));
		textLowerYmm.setFont(serif12);

		textLowerYmm.setEditable(false);
		coordinatesPanel.add(textLowerYmm);

		JLabel labelLowerZmm = new JLabel("Zmin ("
				+ Unit.getUnitFromLegacyNum(resYUnit).getAbbrev() + ") ",
				JLabel.RIGHT);
		labelLowerZmm.setForeground(Color.black);
		labelLowerZmm.setFont(serif12);
		coordinatesPanel.add(labelLowerZmm);

		textLowerZmm = new JTextField(10);
		textLowerZmm.setText(String.valueOf(lowZmm));
		textLowerZmm.setFont(serif12);

		textLowerZmm.setEditable(false);
		coordinatesPanel.add(textLowerZmm);

		JLabel labelWidth = new JLabel("Width (pixels) ", JLabel.RIGHT);
		labelWidth.setForeground(Color.black);
		labelWidth.setFont(serif12);
		coordinatesPanel.add(labelWidth);

		textWidth = new JTextField(10);
		textWidth.setText(String.valueOf(xBounds[1] - xBounds[0]));
		textWidth.setFont(serif12);

		textWidth.setEditable(false);
		coordinatesPanel.add(textWidth);

		JLabel labelHeight = new JLabel("Height (pixels) ", JLabel.RIGHT);
		labelHeight.setForeground(Color.black);
		labelHeight.setFont(serif12);
		coordinatesPanel.add(labelHeight);

		textHeight = new JTextField(10);
		textHeight.setText(String.valueOf(yBounds[1] - yBounds[0]));
		textHeight.setFont(serif12);

		textHeight.setEditable(false);
		coordinatesPanel.add(textHeight);

		JLabel labelDepth = new JLabel("Depth (pixels) ", JLabel.RIGHT);
		labelDepth.setForeground(Color.black);
		labelDepth.setFont(serif12);
		coordinatesPanel.add(labelDepth);

		textDepth = new JTextField(10);
		textDepth.setText(String.valueOf(zBounds[1] - zBounds[0]));
		textDepth.setFont(serif12);

		textDepth.setEditable(false);
		coordinatesPanel.add(textDepth);

		JPanel buttonPanel = new JPanel();

		// buttonPanel.add(OKButton);
		// buttonPanel.add(cancelButton);
		buttonPanel.add(buildButtons());

		JPanel centerPanel = new JPanel(new BorderLayout());
		centerPanel.add(coordinatesPanel, BorderLayout.NORTH);

		JPanel mainPanel = new JPanel(new BorderLayout());
		mainPanel.add(borderPanel, BorderLayout.NORTH);
		mainPanel.add(centerPanel);

		getContentPane().add(mainPanel);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		pack();
		setVisible(true);
	}
}
