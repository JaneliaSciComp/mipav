package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;

public class JDialogProstateTestFeatures extends JDialogScriptableBase
		implements AlgorithmInterface, DialogDefaultsInterface {

	// ~ Static fields/initializers
	// -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = 2408406330754526954L;

	/** Red channel. */
	private static final int RED_OFFSET = 1;

	/** Green channel. */
	private static final int GREEN_OFFSET = 2;

	/** Blue channel. */
	private static final int BLUE_OFFSET = 3;

	// ~ Instance fields
	// ------------------------------------------------------------------------------------------------

	/** DOCUMENT ME! */
	private JPanel colorPanel;

	/** DOCUMENT ME! */
	private ButtonGroup colorGroup;

	/** DOCUMENT ME! */
	private JRadioButton redButton;

	/** DOCUMENT ME! */
	private JRadioButton greenButton;

	/** DOCUMENT ME! */
	private JRadioButton blueButton;

	/** DOCUMENT ME! */
	private int RGBOffset = RED_OFFSET;

	/** DOCUMENT ME! */
	private boolean asm = false;

	/** DOCUMENT ME! */
	private JCheckBox asmCheckBox;

	/** DOCUMENT ME! */
	private boolean contrast = true;

	/** DOCUMENT ME! */
	private JCheckBox contrastCheckBox;

	/** DOCUMENT ME! */
	private boolean correlation = false;

	/** DOCUMENT ME! */
	private JCheckBox correlationCheckBox;

	/** DOCUMENT ME! */
	private boolean dissimilarity = false;

	/** DOCUMENT ME! */
	private JCheckBox dissimilarityCheckBox;

	/** DOCUMENT ME! */
	private boolean energy = false;

	/** DOCUMENT ME! */
	private JCheckBox energyCheckBox;

	/** DOCUMENT ME! */
	private boolean entropy = false;

	/** DOCUMENT ME! */
	private JCheckBox entropyCheckBox;

	/** DOCUMENT ME! */
	private boolean ew = false;

	/** DOCUMENT ME! */
	private JCheckBox ewCheckBox;

	/** DOCUMENT ME! */
	private boolean homogeneity = false;

	/** DOCUMENT ME! */
	private JCheckBox homogeneityCheckBox;

	/** DOCUMENT ME! */
	private ModelImage image; // source image

	/** DOCUMENT ME! */
	private JCheckBox invariantCheckBox;

	/** DOCUMENT ME! */
	private boolean invariantDir = false;

	/** DOCUMENT ME! */
	private boolean inverseOrder1 = false;

	/** DOCUMENT ME! */
	private JCheckBox inverseOrder1CheckBox;

	/** DOCUMENT ME! */
	private JLabel labelOffsetDistance;

	/** DOCUMENT ME! */
	private JLabel labelWindowSize;

	/** DOCUMENT ME! */
	private boolean maxProbability = false;

	/** DOCUMENT ME! */
	private JCheckBox maxProbabilityCheckBox;

	/** DOCUMENT ME! */
	private boolean mean = false;

	/** DOCUMENT ME! */
	private JCheckBox meanCheckBox;

	/** DOCUMENT ME! */
	private boolean nesw = false;

	/** DOCUMENT ME! */
	private JCheckBox neswCheckBox;

	/** DOCUMENT ME! */
	private boolean ns = true;

	/** DOCUMENT ME! */
	private JCheckBox nsCheckBox;

	/** DOCUMENT ME! */
	private int numDirections = 0;

	/** DOCUMENT ME! */
	private int numOperators = 0;

	/** DOCUMENT ME! */
	private int offsetDistance;

	/** DOCUMENT ME! */
	private ModelImage[] resultImage = null; // result image

	/** DOCUMENT ME! */
	private int resultNumber;

	/** DOCUMENT ME! */
	private JPanel scalePanel;

	/** DOCUMENT ME! */
	private boolean senw = false;

	/** DOCUMENT ME! */
	private JCheckBox senwCheckBox;

	/** DOCUMENT ME! */
	private boolean standardDeviation = false;

	/** DOCUMENT ME! */
	private JCheckBox standardDeviationCheckBox;

	/** DOCUMENT ME! */
	private JTextField textOffsetDistance;

	/** DOCUMENT ME! */
	private AlgorithmProstateFeatures textureAlgo;

	/** DOCUMENT ME! */
	private JTextField textWindowSize;

	/** DOCUMENT ME! */
	private boolean variance = false;

	/** DOCUMENT ME! */
	private JCheckBox varianceCheckBox;

	private boolean shade = false;

	private JCheckBox shadeCheckBox;

	private boolean promenance = false;

	private JCheckBox promenanceCheckBox;

	/** DOCUMENT ME! */
	private int windowSize;

	private JLabel labelRescaling;

	private JTextField textRescaling;

	/** Number of grey levels used if data must be rescaled */
	private int greyLevels;

	private JPanel haralickPanel;

	private JPanel savedFilePanel;

	private boolean gaborFilter = false;

	private JTextField textSavedFileName;

	private JButton buttonSavedFileName;

	private String savedFileDirAbs;
	private String savedFileName;

	private int haralickImagesNumber = 0;
	
	private int imageOriginNumber = 1; 

	// Gabor filter
	/** DOCUMENT ME! */

	/** DOCUMENT ME! */
	private JPanel filterPanel;

	/** DOCUMENT ME! */
	private float freqU;

	/** DOCUMENT ME! */
	private float freqV;

	/** DOCUMENT ME! */
	private JLabel labelFU;

	/** DOCUMENT ME! */
	private JLabel labelFV;

	/** DOCUMENT ME! */
	private JLabel labelSU;

	/** DOCUMENT ME! */
	private JLabel labelSV;

	/** DOCUMENT ME! */
	private JLabel labelTheta;

	/** DOCUMENT ME! */
	private float sigmaU;

	/** DOCUMENT ME! */
	private float sigmaV;

	/** DOCUMENT ME! */
	private JTextField textFU;

	/** DOCUMENT ME! */
	private JTextField textFV;

	/** DOCUMENT ME! */
	private JTextField textSU;

	/** DOCUMENT ME! */
	private JTextField textSV;

	/** DOCUMENT ME! */
	private JTextField textTheta;

	/** DOCUMENT ME! */
	private float theta;

	private JPanel gaborPanel;

	private int numberFiltersAdditional = 0;
	
	private JCheckBox gaborFilterCheckBox;
	
	private JCheckBox imageOriginCheckBox;
	
	private JPanel imageOriginPanel;
	
	private boolean imageOriginFilter = true;

	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	/**
	 * Empty constructor needed for dynamic instantiation.
	 */
	public JDialogProstateTestFeatures() {
	}

	/**
	 * Creates a new JDialogHaralickTexture object.
	 * 
	 * @param theParentFrame
	 *            Parent frame.
	 * @param im
	 *            Source image.
	 */
	public JDialogProstateTestFeatures(Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);
		image = im;
		init();
		loadDefaults();
		setVisible(true);
	}

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
			MipavUtil.showHelp("Haral1001");
		} else if (command.equalsIgnoreCase("saveFileBrowse")) {
			JFileChooser chooser = new JFileChooser();

			chooser.setDialogTitle("Choose Saved Features File Name");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				savedFileDirAbs = chooser.getSelectedFile().getAbsolutePath();
				savedFileName = chooser.getSelectedFile().getName();
				// FileIO fileIO = new FileIO();
				// fileIO.setQuiet(true);
				System.err.println("savedFileDir = " + savedFileDirAbs);
				System.err.println("savedFileName = " + savedFileName);
				textSavedFileName.setText(savedFileDirAbs);

			}
		}
	}

	// ************************************************************************
	// ************************** Algorithm Events ****************************
	// ************************************************************************

	/**
	 * This method is required if the AlgorithmPerformed interface is
	 * implemented. It is called by the algorithm when it has completed or
	 * failed to to complete, so that the dialog can be display the result image
	 * and/or clean up.
	 * 
	 * @param algorithm
	 *            Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		int i;
		ViewJFrameImage[] imageFrame = new ViewJFrameImage[resultNumber];

		if (algorithm instanceof AlgorithmProstateFeatures) {
			image.clearMask();

			if ((textureAlgo.isCompleted() == true) && (resultImage != null)) {

				// The algorithm has completed and produced a new image to be
				// displayed.
				// Take resultImage out of array form or null pointer errors can
				// result in one of the resultImages after another of the
				// resultImages
				// has been deleted.
				for (i = 0; i < resultNumber; i++) {
					updateFileInfo(image, resultImage[i]);
					resultImage[i].clearMask();

					try {
						imageFrame[i] = new ViewJFrameImage(resultImage[i],
								null, new Dimension(610, 200));
					} catch (OutOfMemoryError error) {
						System.gc();
						JOptionPane
								.showMessageDialog(
										null,
										"Out of memory: unable to open new resultImage frame",
										"Error", JOptionPane.ERROR_MESSAGE);
					}
					haralickImagesNumber = textureAlgo.getHaralickImagesNumber();
					saveFeatureSpaceValue(resultImage[i]);
				}

			} else if (resultImage != null) {

				// algorithm failed but result image still has garbage
				for (i = 0; i < resultNumber; i++) {

					if (resultImage[i] != null) {
						resultImage[i].disposeLocal(); // Clean up memory of
						// result image
						resultImage[i] = null;
					}
				}

				resultImage = null;
				System.gc();
			}
		}

		if (algorithm.isCompleted()) {
			insertScriptLine();
		}

		dispose();

	}

	public void saveFeatureSpaceValue(ModelImage resultImage) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int sliceSize = xDim * yDim;
		int i, z, k;
		int classify;
		int index;
		float value;
		int zDim;
		// resultNumber ?????
		int numImages = 1 + haralickImagesNumber;
		System.err.println("haralickImagesNumber = " + haralickImagesNumber);

		float[] resultBuffer = new float[sliceSize];
		float[] resultBufferClass = new float[sliceSize];

		Vector[] features = new Vector[numImages];

		for (i = 0; i < numImages; i++) {
			features[i] = new Vector();
		}

		if (image.getNDims() == 2) {
			zDim = 1;
		} else {
			zDim = image.getExtents()[2];
		}

		// boolean test = false;
		for (i = 0; i < numImages; i++) {
			// test = false;
			for (z = 0; z < zDim; z++) {
				try {
					// import and export ??????????????
					resultImage.exportData((i) * zDim * sliceSize + z
							* sliceSize, sliceSize, resultBuffer);
					

					for (k = 0; k < sliceSize; k++) {
						classify = 0;
						value = resultBuffer[k];
						index = i;
						// if ( classify == 1 /* && test == false */ ) {
						// System.err.println("classify = " + classify + " index
						// = " + index + " value = " + value);
						features[i].add(new Feature(classify, index, value));
						// test = true;
						// }
					}

				} catch (IOException error) {
					MipavUtil
							.displayError("JDialogProstateSaveFeatures: IOException on destImage["
									+ i
									+ "].importData(0,resultBuffer["
									+ i
									+ "],false)");

					return;
				}
			} // for (i = 0; i < resultNumber; i++)
		}


        // Save feature space into a file
        // savedFileDirAbs
        try {
        	savedFileDirAbs = textSavedFileName.getText();
        	
        	File file = new File(savedFileDirAbs);
        	PrintWriter output = new PrintWriter(file);
        	Feature feature, featureTemp;
        	int size, j;
        	size = features[1].size();
        	System.err.println("size = " + size);
            boolean printClassify = false; 
    		for ( j = 0; j < size; j++ ) {
    		    printClassify = false;
        	    for ( i = imageOriginNumber; i < features.length; i++ ) {
        		   feature = (Feature)features[i].get(j);
        		   classify = feature.classify;
        		   value = feature.value;
        		   index = feature.index;
        		   if ( i == 0 ) {
        			 featureTemp = (Feature)features[i+1].get(j);
        		     classify = featureTemp.classify;
        		   }
        		   if ( printClassify == false ) {
         			  output.print(classify);
         			  printClassify = true;
         		   }
        		   output.print(" " + i + ":" + value + " ");  
        		}
        	    output.println();
        	}
    		output.close();
        } catch (Exception e ) {
        	e.printStackTrace();
        }


	}

	/**
	 * Construct a delimited string that contains the parameters to this
	 * algorithm.
	 * 
	 * @param delim
	 *            the parameter delimiter (defaults to " " if empty)
	 * 
	 * @return the parameter string
	 */
	public String getParameterString(String delim) {

		if (delim.equals("")) {
			delim = " ";
		}

		String str = new String();
		if (image.isColorImage()) {
			str += RGBOffset + delim;
		}
		str += windowSize + delim;
		str += offsetDistance + delim;
		str += greyLevels + delim;
		str += ns + delim;
		str += nesw + delim;
		str += ew + delim;
		str += senw + delim;
		str += invariantDir + delim;
		str += contrast + delim;
		str += dissimilarity + delim;
		str += homogeneity + delim;
		str += inverseOrder1 + delim;
		str += asm + delim;
		str += energy + delim;
		str += maxProbability + delim;
		str += entropy + delim;
		str += mean + delim;
		str += variance + delim;
		str += standardDeviation + delim;
		str += correlation + delim;
		str += shade + delim;
		str += promenance;

		return str;
	}

	/**
	 * Accessor that returns the image.
	 * 
	 * @return The result image.
	 */
	public ModelImage[] getResultImage() {
		return resultImage;
	}

	/**
	 * Loads the default settings from Preferences to set up the dialog.
	 */
	public void loadDefaults() {
		String defaultsString = Preferences.getDialogDefaults(getDialogName());

		if (defaultsString != null) {

			try {
				StringTokenizer st = new StringTokenizer(defaultsString, ",");
				textWindowSize.setText("" + MipavUtil.getInt(st));
				textOffsetDistance.setText("" + MipavUtil.getInt(st));
				textRescaling.setText("" + MipavUtil.getInt(st));
				nsCheckBox.setSelected(MipavUtil.getBoolean(st));
				neswCheckBox.setSelected(MipavUtil.getBoolean(st));
				ewCheckBox.setSelected(MipavUtil.getBoolean(st));
				senwCheckBox.setSelected(MipavUtil.getBoolean(st));
				invariantCheckBox.setSelected(MipavUtil.getBoolean(st));
				contrastCheckBox.setSelected(MipavUtil.getBoolean(st));
				dissimilarityCheckBox.setSelected(MipavUtil.getBoolean(st));
				homogeneityCheckBox.setSelected(MipavUtil.getBoolean(st));
				inverseOrder1CheckBox.setSelected(MipavUtil.getBoolean(st));
				asmCheckBox.setSelected(MipavUtil.getBoolean(st));
				energyCheckBox.setSelected(MipavUtil.getBoolean(st));
				maxProbabilityCheckBox.setSelected(MipavUtil.getBoolean(st));
				entropyCheckBox.setSelected(MipavUtil.getBoolean(st));
				meanCheckBox.setSelected(MipavUtil.getBoolean(st));
				varianceCheckBox.setSelected(MipavUtil.getBoolean(st));
				standardDeviationCheckBox.setSelected(MipavUtil.getBoolean(st));
				correlationCheckBox.setSelected(MipavUtil.getBoolean(st));
				shadeCheckBox.setSelected(MipavUtil.getBoolean(st));
				promenanceCheckBox.setSelected(MipavUtil.getBoolean(st));
			} catch (Exception ex) {

				// since there was a problem parsing the defaults string, start
				// over with the original defaults
				Preferences.debug("Resetting defaults for dialog: "
						+ getDialogName());
				Preferences.removeProperty(getDialogName());
			}
		}
	}

	/**
	 * Saves the default settings into the Preferences file.
	 */
	public void saveDefaults() {
		String defaultsString = new String(getParameterString(","));
		Preferences.saveDialogDefaults(getDialogName(), defaultsString);
	}

	/**
	 * Accessor that sets if asm operator is calculated.
	 * 
	 * @param asm
	 *            boolean
	 */
	public void setASM(boolean asm) {
		this.asm = asm;
	}

	/**
	 * Accessor that sets if the contrast operator is calculated.
	 * 
	 * @param contrast
	 *            boolean
	 */
	public void setContrast(boolean contrast) {
		this.contrast = contrast;
	}

	/**
	 * Accessor that sets if gray level coordinate matrix correlation is
	 * calculated.
	 * 
	 * @param correlation
	 *            boolean
	 */
	public void setCorrelation(boolean correlation) {
		this.correlation = correlation;
	}

	/**
	 * Accessor that sets if dissimilarity operator is calculated.
	 * 
	 * @param dissimilarity
	 *            boolean
	 */
	public void setDissimilarity(boolean dissimilarity) {
		this.dissimilarity = dissimilarity;
	}

	/**
	 * Accessor that sets if energy operator is calculated.
	 * 
	 * @param energy
	 *            boolean
	 */
	public void setEnergy(boolean energy) {
		this.energy = energy;
	}

	/**
	 * Accessor that sets if entropy operator is calculated.
	 * 
	 * @param entropy
	 *            boolean
	 */
	public void setEntropy(boolean entropy) {
		this.entropy = entropy;
	}

	/**
	 * Accessor that sets if east west offset direction is calculated.
	 * 
	 * @param ew
	 *            boolean
	 */
	public void setEW(boolean ew) {
		this.ew = ew;
	}

	/**
	 * Accessor that sets if homogeneity operator is calculated.
	 * 
	 * @param homogeneity
	 *            boolean
	 */
	public void setHomogeneity(boolean homogeneity) {
		this.homogeneity = homogeneity;
	}

	/**
	 * Accessor that sets if spatially invariant offset direction is performed.
	 * 
	 * @param invariantDir
	 *            boolean
	 */
	public void setInvariant(boolean invariantDir) {
		this.invariantDir = invariantDir;
	}

	/**
	 * Accessor that sets if the inverse difference moment of order 1 operator
	 * is called.
	 * 
	 * @param inverseOrder1
	 *            boolean
	 */
	public void setInverseOrder1(boolean inverseOrder1) {
		this.inverseOrder1 = inverseOrder1;
	}

	/**
	 * Accessor that sets if maximum probability operator is calculated.
	 * 
	 * @param maxProbability
	 *            boolean
	 */
	public void setMaxProbability(boolean maxProbability) {
		this.maxProbability = maxProbability;
	}

	/**
	 * Accessor that set if the gray level coordinate matrix mean is calculated.
	 * 
	 * @param mean
	 *            boolean
	 */
	public void setMean(boolean mean) {
		this.mean = mean;
	}

	/**
	 * Accessor that sets if northeast-southest offset direction is calculated.
	 * 
	 * @param nesw
	 *            boolean
	 */
	public void setNESW(boolean nesw) {
		this.nesw = nesw;
	}

	/**
	 * Accessor that sets if north south offset direction is calculated.
	 * 
	 * @param ns
	 *            boolean
	 */
	public void setNS(boolean ns) {
		this.ns = ns;
	}

	/**
	 * Accessor that sets the offset distance.
	 * 
	 * @param offsetDistance
	 *            int
	 */
	public void setOffsetDistance(int offsetDistance) {
		this.offsetDistance = offsetDistance;
	}

	/**
	 * Accessor that sets if southeast-northwest offset direction is calculated.
	 * 
	 * @param senw
	 *            boolean
	 */
	public void setSENW(boolean senw) {
		this.senw = senw;
	}

	/**
	 * Accessor that sets if gray level coordinate matrix standard deviation is
	 * calculated.
	 * 
	 * @param standardDeviation
	 *            boolean
	 */
	public void setStandardDeviation(boolean standardDeviation) {
		this.standardDeviation = standardDeviation;
	}

	/**
	 * Accessor that sets if the gray level coordinate matrix variance is
	 * calculated.
	 * 
	 * @param variance
	 *            boolean
	 */
	public void setVariance(boolean variance) {
		this.variance = variance;
	}

	/**
	 * Accessor that sets if the cluster shade is calculated
	 * 
	 * @param shade
	 */
	public void setShade(boolean shade) {
		this.shade = shade;
	}

	/**
	 * Accessor that sets the window size.
	 * 
	 * @param windowSize
	 *            int
	 */
	public void setWindowSize(int windowSize) {
		this.windowSize = windowSize;
	}

	/**
	 * Accessor that sets the number of grey levels if rescaling used
	 * 
	 * @param greyLevels
	 */
	public void setGreyLevels(int greyLevels) {
		this.greyLevels = greyLevels;
	}

	/**
	 * Accessor that sets the RGBOffset.
	 * 
	 * @param RGBoffset
	 *            DOCUMENT ME!
	 */
	public void setRGBOffset(int RGBoffset) {
		this.RGBOffset = RGBoffset;
	}

	/**
	 * Once all the necessary variables are set, call the Gaussian Haralick
	 * feature algorithm.
	 */
	protected void callAlgorithm() {
		int i, j, k, index = 0;
		resultNumber = 1;

		String[] name = null;
		String[] imageNameArray = null;
		String dirString = null;
		String opString = null;
		boolean doneNS = false;
		boolean doneNESW = false;
		boolean doneEW = false;
		boolean doneSENW = false;
		boolean doneInvariant = false;
		boolean doneContrast;
		boolean doneDissimilarity;
		boolean doneHomogeneity;
		boolean doneInverseOrder1;
		boolean doneASM;
		boolean doneEnergy;
		boolean doneMaxProbability;
		boolean doneEntropy;
		boolean doneMean;
		boolean doneVariance;
		boolean doneStandardDeviation;
		boolean doneCorrelation;
		boolean doneShade;
		boolean donePromenance;
		boolean doneGaborFilter;
		int zDim;
		int tDim;
		int newExtents[];

		try {
			resultImage = new ModelImage[resultNumber];
			name = new String[resultNumber];

			if (image.getNDims() == 2) {
				name[0] = makeImageName(image.getImageName(), "_Haralick");
				zDim = 1 + numDirections * numOperators + numberFiltersAdditional;
				newExtents = new int[3];
				newExtents[0] = image.getExtents()[0];
				newExtents[1] = image.getExtents()[1];
				newExtents[2] = zDim;
				resultImage[0] = new ModelImage(ModelStorageBase.FLOAT,
						newExtents, name[0]);
				tDim = 1 + numDirections * numOperators + numberFiltersAdditional;
				// imageNameArray = new String[zDim];
				imageNameArray = new String[tDim];
				imageNameArray[0] = name[0];
			} // if (image.getNDims() == 2)
			else { // image.getNDims() == 3
				name[0] = makeImageName(image.getImageName(), "_Haralick");
				tDim = 1 + numDirections * numOperators + numberFiltersAdditional;
				newExtents = new int[4];
				newExtents[0] = image.getExtents()[0];
				newExtents[1] = image.getExtents()[1];
				newExtents[2] = image.getExtents()[2];
				newExtents[3] = tDim;
				resultImage[0] = new ModelImage(ModelStorageBase.FLOAT,
						newExtents, name[0]);
				imageNameArray = new String[newExtents[2] * newExtents[3]];
				for (i = 0; i < image.getExtents()[2]; i++) {
					imageNameArray[i] = name[0];
				}
			}

			for (i = 0; i < numDirections; i++) {

				if (ns && (!doneNS)) {
					dirString = "_ns";
					doneNS = true;
				} else if (nesw && (!doneNESW)) {
					dirString = "_nesw";
					doneNESW = true;
				} else if (ew && (!doneEW)) {
					dirString = "_ew";
					doneEW = true;
				} else if (senw && (!doneSENW)) {
					dirString = "_senw";
					doneSENW = true;
				} else if (invariantDir && (!doneInvariant)) {
					dirString = "_invariantDir";
					doneInvariant = true;
				}

				doneContrast = false;
				doneDissimilarity = false;
				doneHomogeneity = false;
				doneInverseOrder1 = false;
				doneASM = false;
				doneEnergy = false;
				doneMaxProbability = false;
				doneEntropy = false;
				doneMean = false;
				doneVariance = false;
				doneStandardDeviation = false;
				doneCorrelation = false;
				doneShade = false;
				donePromenance = false;
				doneGaborFilter = false;
				
				for (j = 0; j < numOperators; j++) {
					index = j + (i * (numOperators));

					if (contrast && (!doneContrast)) {
						opString = "_contrast";
						doneContrast = true;
					} else if (dissimilarity && (!doneDissimilarity)) {
						opString = "_dissimilarity";
						doneDissimilarity = true;
					} else if (homogeneity && (!doneHomogeneity)) {
						opString = "_homogeneity";
						doneHomogeneity = true;
					} else if (inverseOrder1 && (!doneInverseOrder1)) {
						opString = "_inverseOrder1";
						doneInverseOrder1 = true;
					} else if (asm && (!doneASM)) {
						opString = "_asm";
						doneASM = true;
					} else if (energy && (!doneEnergy)) {
						opString = "_energy";
						doneEnergy = true;
					} else if (maxProbability && (!doneMaxProbability)) {
						opString = "_maxProbability";
						doneMaxProbability = true;
					} else if (entropy && (!doneEntropy)) {
						opString = "_entropy";
						doneEntropy = true;
					} else if (mean && (!doneMean)) {
						opString = "_mean";
						doneMean = true;
					} else if (variance && (!doneVariance)) {
						opString = "_variance";
						doneVariance = true;
					} else if (standardDeviation && (!doneStandardDeviation)) {
						opString = "_standardDeviation";
						doneStandardDeviation = true;
					} else if (correlation && (!doneCorrelation)) {
						opString = "_correlation";
						doneCorrelation = true;
					} else if (shade && (!doneShade)) {
						opString = "_shade";
						doneShade = true;
					} else if (promenance && (!donePromenance)) {
						opString = "_promenance";
						donePromenance = true;
					} 
					/*else if (gaborFilter && ( !doneGaborFilter )) {
                        opString = "_Gabor";
                        doneGaborFilter = true;
                    } 
                    */
                    
					if (image.getNDims() == 2) {
						imageNameArray[index + 1] = makeImageName(image
								.getImageName(), dirString + opString);
					} else {
						for (k = 0; k < image.getExtents()[2]; k++) {
							imageNameArray[(index + 1) * image.getExtents()[2]
									+ k] = makeImageName(image.getImageName(),
									dirString + opString);
						}
					}
					

				} // for (j = 0; j < numOperators; j++)
			} // for (i = 0; i < numDirections; i++)

			// do additional operation
			
			doneGaborFilter = false;
			
			if (gaborFilter && (!doneGaborFilter)) {
				opString = "_Gabor";
				doneGaborFilter = true;

				if (image.getNDims() == 2) {
					// index + 2 for the additional filters. 
					imageNameArray[index + 1 + numberFiltersAdditional] = makeImageName(image
							.getImageName(), dirString + opString);
				} else {
					for (k = 0; k < image.getExtents()[2]; k++) {
						imageNameArray[(index + 2) * image.getExtents()[2] + k] = makeImageName(
								image.getImageName(), dirString + opString);
					}
				}

			}
            
			resultImage[0].setImageNameArray(imageNameArray);

			if (image.isColorImage()) {
				textureAlgo = new AlgorithmProstateFeatures(resultImage, image,
						RGBOffset, windowSize, offsetDistance, greyLevels, ns,
						nesw, ew, senw, invariantDir, contrast, dissimilarity,
						homogeneity, inverseOrder1, asm, energy,
						maxProbability, entropy, mean, variance,
						standardDeviation, correlation, shade, promenance,
						true, gaborFilter, freqU, freqV, sigmaU, sigmaV, theta,
						numberFiltersAdditional);
			} else {
				textureAlgo = new AlgorithmProstateFeatures(resultImage, image,
						windowSize, offsetDistance, greyLevels, ns, nesw, ew,
						senw, invariantDir, contrast, dissimilarity,
						homogeneity, inverseOrder1, asm, energy,
						maxProbability, entropy, mean, variance,
						standardDeviation, correlation, shade, promenance,
						true, gaborFilter, freqU, freqV, sigmaU, sigmaV, theta,
						numberFiltersAdditional);
			}

			// This is very important. Adding this object as a listener allows
			// the algorithm to
			// notify this object when it has completed of failed. See algorithm
			// performed event.
			// This is made possible by implementing AlgorithmedPerformed
			// interface
			textureAlgo.addListener(this);
			createProgressBar(image.getImageName(), textureAlgo);

			// Hide dialog
			setVisible(false);

			if (isRunInSeparateThread()) {

				// Start the thread as a low priority because we wish to still
				// have user interface work fast.
				if (textureAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil
							.displayError("A thread is already running on this object");
				}
			} else {
				textureAlgo.run();
			}
		} catch (OutOfMemoryError x) {

			if (resultImage != null) {

				for (i = 0; i < resultNumber; i++) {

					if (resultImage[i] != null) {
						resultImage[i].disposeLocal(); // Clean up memory of
						// result image
						resultImage[i] = null;
					}
				}

				resultImage = null;
			}

			System.gc();
			MipavUtil
					.displayError("Dialog Haralick Texture: unable to allocate enough memory");

			return;
		}

	}

	/**
	 * Store the result image in the script runner's image table now that the
	 * action execution is finished.
	 */
	protected void doPostAlgorithmActions() {

		for (int i = 0; i < resultNumber; i++) {
			AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	protected void setGUIFromParams() {
		image = scriptParameters.retrieveInputImage();
		parentFrame = image.getParentFrame();

		if (image.isColorImage()
				&& scriptParameters.getParams().containsParameter("RGB_offset")) {
			RGBOffset = scriptParameters.getParams().getInt("RGB_offset");
		} else if (image.isColorImage()) {
			throw new ParameterException(
					"RGB_offset",
					"This parameter (RGB_offset) is required for the processing of color images.  Please re-record this script using a color image.");
		}
		setWindowSize(scriptParameters.getParams().getInt("window_size"));
		setOffsetDistance(scriptParameters.getParams()
				.getInt("offset_distance"));
		setGreyLevels(scriptParameters.getParams().getInt("grey_levels"));
		setNS(scriptParameters.getParams()
				.getBoolean("do_calc_north_south_dir"));
		setNESW(scriptParameters.getParams().getBoolean("do_calc_NE_SW_dir"));
		setEW(scriptParameters.getParams().getBoolean("do_calc_east_west_dir"));
		setSENW(scriptParameters.getParams().getBoolean("do_calc_SE_NW_dir"));
		setInvariant(scriptParameters.getParams().getBoolean(
				"do_calc_invariant_dir"));
		setContrast(scriptParameters.getParams().getBoolean("do_calc_contrast"));
		setDissimilarity(scriptParameters.getParams().getBoolean(
				"do_calc_dissimilarity"));
		setHomogeneity(scriptParameters.getParams().getBoolean(
				"do_calc_homogeneity"));
		setInverseOrder1(scriptParameters.getParams().getBoolean(
				"do_inverse_order_1"));
		setASM(scriptParameters.getParams().getBoolean("do_calc_ASM"));
		setEnergy(scriptParameters.getParams().getBoolean("do_calc_energy"));
		setMaxProbability(scriptParameters.getParams().getBoolean(
				"do_calc_max_probability"));
		setEntropy(scriptParameters.getParams().getBoolean("do_calc_entropy"));
		setMean(scriptParameters.getParams().getBoolean("do_calc_mean"));
		setVariance(scriptParameters.getParams().getBoolean("do_calc_variance"));
		setStandardDeviation(scriptParameters.getParams().getBoolean(
				"do_calc_standard_deviation"));
		setCorrelation(scriptParameters.getParams().getBoolean(
				"do_calc_correlation"));
		setShade(scriptParameters.getParams().getBoolean("do_calc_shade"));

		if ((windowSize % 2) == 0) {
			throw new ParameterException("window_size",
					"Window size must not be even");
		}

		if (windowSize > image.getExtents()[0]) {
			throw new ParameterException("window_size",
					"Window size must not excced image width of "
							+ image.getExtents()[0]);
		}

		if (windowSize > image.getExtents()[1]) {
			throw new ParameterException("window_size",
					"Window size must not excced image height of "
							+ image.getExtents()[1]);
		}

		numDirections = getNumDirections();
		numOperators = getNumOperators();
	}

	/**
	 * {@inheritDoc}
	 */
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(image);

		for (int i = 0; i < resultNumber; i++) {
			scriptParameters.storeImageInRecorder(getResultImage()[i]);
		}

		if (image.isColorImage()) {
			scriptParameters.getParams().put(
					ParameterFactory.newParameter("RGB_offset", RGBOffset));
		}
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("window_size", windowSize));
		scriptParameters.getParams().put(
				ParameterFactory
						.newParameter("offset_distance", offsetDistance));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("grey_levels", greyLevels));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_north_south_dir", ns));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_NE_SW_dir", nesw));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_east_west_dir", ew));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_SE_NW_dir", senw));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_invariant_dir",
						invariantDir));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_contrast", contrast));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_dissimilarity",
						dissimilarity));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_homogeneity",
						homogeneity));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_inverse_order_1",
						inverseOrder1));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_ASM", asm));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_energy", energy));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_max_probability",
						maxProbability));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_entropy", entropy));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_mean", mean));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_variance", variance));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_standard_deviation",
						standardDeviation));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_correlation",
						correlation));
		scriptParameters.getParams().put(
				ParameterFactory.newParameter("do_calc_shade", shade));
		scriptParameters.getParams()
				.put(
						ParameterFactory.newParameter("do_calc_promenance",
								promenance));

	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @return DOCUMENT ME!
	 */
	private int getNumDirections() {
		int numDirs = 0;

		if (ns) {
			numDirs++;
		}

		if (nesw) {
			numDirs++;
		}

		if (ew) {
			numDirs++;
		}

		if (senw) {
			numDirs++;
		}

		if (invariantDir) {
			numDirs++;
		}

		return numDirs;
	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @return DOCUMENT ME!
	 */
	private int getNumOperators() {
		int numOps = 0;

		if (contrast) {
			numOps++;
		}

		if (dissimilarity) {
			numOps++;
		}

		if (homogeneity) {
			numOps++;
		}

		if (inverseOrder1) {
			numOps++;
		}

		if (asm) {
			numOps++;
		}

		if (energy) {
			numOps++;
		}

		if (maxProbability) {
			numOps++;
		}

		if (entropy) {
			numOps++;
		}

		if (mean) {
			numOps++;
		}

		if (variance) {
			numOps++;
		}

		if (standardDeviation) {
			numOps++;
		}

		if (correlation) {
			numOps++;
		}

		if (shade) {
			numOps++;
		}

		if (promenance) {
			numOps++;
		}
		
		if ( gaborFilter ) {
			numberFiltersAdditional++;
		}
		
		
		return numOps;
	}

	/**
	 * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
	 */
	private void init() {

		setForeground(Color.black);

		setTitle("Prostate Features");
		// getContentPane().setLayout(new BorderLayout());

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		buildImageOriginPanel();
		buildHaralickPanel();
		buildGaborPanel();
		buildSavedFilePanel();

		JPanel otherFeatures = new JPanel();
		otherFeatures.setLayout(new BorderLayout());

		otherFeatures.add(gaborPanel, BorderLayout.NORTH);
		otherFeatures.add(savedFilePanel, BorderLayout.CENTER);
		
		JPanel leftPanel = new JPanel();
		leftPanel.setLayout(new BoxLayout(leftPanel, BoxLayout.Y_AXIS));
		
		leftPanel.add(imageOriginPanel);
		leftPanel.add(haralickPanel);
		
		mainPanel.add(leftPanel, BorderLayout.WEST);
		mainPanel.add(otherFeatures, BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);

		// setVisible( true );

		System.gc();
	}

	private void buildGaborPanel() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;

		filterPanel = new JPanel(new GridBagLayout());
		filterPanel
				.setBorder(buildTitledBorder("Frequency domain filter specifications"));

		labelFU = new JLabel("Prerotation horizontal frequency -1.0 to 1.0 ");
		labelFU.setForeground(Color.black);
		labelFU.setFont(serif12);
		labelFU.setEnabled(true);

		textFU = new JTextField(10);
		freqU = image.getFreqU();
		textFU.setText(String.valueOf(freqU));
		textFU.setFont(serif12);
		textFU.setEnabled(true);

		labelFV = new JLabel("Prerotation vertical frequency -1.0 to 1.0 ");
		labelFV.setForeground(Color.black);
		labelFV.setFont(serif12);
		labelFV.setEnabled(true);

		textFV = new JTextField(10);
		freqV = image.getFreqV();
		textFV.setText(String.valueOf(freqV));
		textFV.setFont(serif12);
		textFV.setEnabled(true);

		labelSU = new JLabel("Prerotation horizontal standard deviation > 0.0 ");
		labelSU.setForeground(Color.black);
		labelSU.setFont(serif12);
		labelSU.setEnabled(true);

		textSU = new JTextField(10);
		sigmaU = image.getSigmaU();
		textSU.setText(String.valueOf(sigmaU));
		textSU.setFont(serif12);
		textSU.setEnabled(true);

		labelSV = new JLabel("Prerotation vertical standard deviation > 0.0 ");
		labelSV.setForeground(Color.black);
		labelSV.setFont(serif12);
		labelSV.setEnabled(true);

		textSV = new JTextField(10);
		sigmaV = image.getSigmaV();
		textSV.setText(String.valueOf(sigmaV));
		textSV.setFont(serif12);
		textSV.setEnabled(true);

		labelTheta = new JLabel("Rotation angle in degrees ");
		labelTheta.setForeground(Color.black);
		labelTheta.setFont(serif12);
		labelTheta.setEnabled(true);

		textTheta = new JTextField(10);
		theta = (float) (image.getTheta() * 180.0 / Math.PI);
		textTheta.setText(String.valueOf(theta));
		textTheta.setFont(serif12);
		textTheta.setEnabled(true);

		gaborFilterCheckBox = new JCheckBox("Select Gabor Filter");
		gaborFilterCheckBox.setFont(serif12);
		gaborFilterCheckBox.setSelected(false);
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.weightx = .5;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		filterPanel.add(labelFU, gbc);
		gbc.gridx = 1;
		filterPanel.add(textFU, gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		filterPanel.add(labelFV, gbc);
		gbc.gridx = 1;
		filterPanel.add(textFV, gbc);
		gbc.gridx = 0;
		gbc.gridy = 2;
		filterPanel.add(labelSU, gbc);
		gbc.gridx = 1;
		filterPanel.add(textSU, gbc);
		gbc.gridx = 0;
		gbc.gridy = 3;
		filterPanel.add(labelSV, gbc);
		gbc.gridx = 1;
		filterPanel.add(textSV, gbc);
		gbc.gridx = 0;
		gbc.gridy = 4;
		filterPanel.add(labelTheta, gbc);
		gbc.gridx = 1;
		filterPanel.add(textTheta, gbc);
		gbc.gridx = 0;
		gbc.gridy = 5;
		filterPanel.add(gaborFilterCheckBox, gbc);
		
		
		gaborPanel = new JPanel(new GridBagLayout());
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gaborPanel.add(filterPanel, gbc);

	}

	private void buildSavedFilePanel() {
		savedFilePanel = new JPanel();
		savedFilePanel.setBorder(buildTitledBorder("Saved Features File"));
		savedFilePanel.setLayout(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;

		JLabel labelSavedFileName = WidgetFactory.buildLabel("File Name");
		textSavedFileName = WidgetFactory.buildTextField("");
		textSavedFileName.setColumns(10);

		buttonSavedFileName = new JButton("Browse");
		buttonSavedFileName.addActionListener(this);
		buttonSavedFileName.setActionCommand("saveFileBrowse");
		buttonSavedFileName.setMinimumSize(MipavUtil.defaultButtonSize);
		buttonSavedFileName.setPreferredSize(MipavUtil.defaultButtonSize);
		buttonSavedFileName.setFont(serif12B);

		gbc.gridx = 0;
		gbc.gridy = 0;
		savedFilePanel.add(labelSavedFileName, gbc);
		gbc.gridx = 1;
		savedFilePanel.add(textSavedFileName, gbc);
		gbc.gridx = 2;
		savedFilePanel.add(buttonSavedFileName, gbc);

	}

	
	private void buildImageOriginPanel() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 3;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;

		imageOriginPanel = new JPanel(new GridBagLayout());
		imageOriginPanel
				.setBorder(buildTitledBorder("Original Image Selection"));
		
		imageOriginCheckBox = new JCheckBox("Select Original Image");
		imageOriginCheckBox.setFont(serif12);
		imageOriginCheckBox.setSelected(true);
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.weightx = .5;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		
		imageOriginPanel.add(imageOriginCheckBox, gbc);
		
	}
	
	private void buildHaralickPanel() {
		int ypos = 0;

		haralickPanel = new JPanel();
		haralickPanel.setBorder(buildTitledBorder("Haralick"));
		haralickPanel.setLayout(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;

		if (image.isColorImage()) {
			colorPanel = new JPanel(new GridLayout(3, 1));
			colorPanel.setForeground(Color.black);
			colorPanel.setBorder(buildTitledBorder("Colors"));

			colorGroup = new ButtonGroup();
			redButton = new JRadioButton("Red", true);
			redButton.setFont(serif12);
			redButton.setForeground(Color.black);
			redButton.addActionListener(this);
			colorGroup.add(redButton);
			colorPanel.add(redButton);

			greenButton = new JRadioButton("Green", false);
			greenButton.setFont(serif12);
			greenButton.setForeground(Color.black);
			greenButton.addActionListener(this);
			colorGroup.add(greenButton);
			colorPanel.add(greenButton);

			blueButton = new JRadioButton("Blue", false);
			blueButton.setFont(serif12);
			blueButton.setForeground(Color.black);
			blueButton.addActionListener(this);
			colorGroup.add(blueButton);
			colorPanel.add(blueButton);
			gbc.gridx = 0;
			gbc.gridy = ypos++;
			haralickPanel.add(colorPanel, gbc);
		} // if (image.isColorImage())

		scalePanel = new JPanel(new GridLayout(3, 2));
		scalePanel.setForeground(Color.black);
		scalePanel.setBorder(buildTitledBorder("Sizes"));
		gbc.gridx = 0;
		gbc.gridy = ypos++;
		haralickPanel.add(scalePanel, gbc);

		labelWindowSize = new JLabel("Window size - odd");
		labelWindowSize.setForeground(Color.black);
		labelWindowSize.setFont(serif12);
		scalePanel.add(labelWindowSize);
		textWindowSize = new JTextField(10);
		textWindowSize.setText("7");
		textWindowSize.setFont(serif12);
		textWindowSize.setForeground(Color.black);
		scalePanel.add(textWindowSize);

		labelOffsetDistance = new JLabel("Offset distance");
		labelOffsetDistance.setForeground(Color.black);
		labelOffsetDistance.setFont(serif12);
		scalePanel.add(labelOffsetDistance);
		textOffsetDistance = new JTextField(10);
		textOffsetDistance.setText("1");
		textOffsetDistance.setFont(serif12);
		textOffsetDistance.setForeground(Color.black);
		scalePanel.add(textOffsetDistance);

		labelRescaling = new JLabel("Grey levels if rescaled (8-64)");
		labelRescaling.setForeground(Color.black);
		labelRescaling.setFont(serif12);
		scalePanel.add(labelRescaling);
		textRescaling = new JTextField(10);
		textRescaling.setText("32");
		textRescaling.setFont(serif12);
		textRescaling.setForeground(Color.black);
		scalePanel.add(textRescaling);

		JPanel directionPanel = new JPanel(new GridBagLayout());
		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.gridwidth = 1;
		gbc2.gridheight = 1;
		gbc2.anchor = GridBagConstraints.WEST;
		gbc2.weightx = 1;
		gbc2.insets = new Insets(3, 3, 3, 3);
		gbc2.fill = GridBagConstraints.HORIZONTAL;
		directionPanel.setBorder(buildTitledBorder("Offset directions"));

		nsCheckBox = new JCheckBox("North-South");
		nsCheckBox.setFont(serif12);
		gbc2.gridx = 0;
		gbc2.gridy = 0;
		directionPanel.add(nsCheckBox, gbc2);
		nsCheckBox.setSelected(false);

		neswCheckBox = new JCheckBox("NE-SW");
		neswCheckBox.setFont(serif12);
		gbc2.gridx = 0;
		gbc2.gridy = 1;
		directionPanel.add(neswCheckBox, gbc2);
		neswCheckBox.setSelected(false);

		ewCheckBox = new JCheckBox("East-West");
		ewCheckBox.setFont(serif12);
		gbc2.gridx = 0;
		gbc2.gridy = 2;
		directionPanel.add(ewCheckBox, gbc2);
		ewCheckBox.setSelected(false);

		senwCheckBox = new JCheckBox("SE-NW");
		senwCheckBox.setFont(serif12);
		gbc2.gridx = 0;
		gbc2.gridy = 3;
		directionPanel.add(senwCheckBox, gbc2);
		senwCheckBox.setSelected(false);

		invariantCheckBox = new JCheckBox("Spatially invariant");
		invariantCheckBox.setFont(serif12);
		gbc2.gridx = 0;
		gbc2.gridy = 4;
		directionPanel.add(invariantCheckBox, gbc2);
		invariantCheckBox.setSelected(true);

		gbc.gridx = 0;
		gbc.gridy = ypos++;
		haralickPanel.add(directionPanel, gbc);

		JPanel operatorPanel = new JPanel(new GridBagLayout());
		GridBagConstraints gbc3 = new GridBagConstraints();
		gbc3.gridwidth = 1;
		gbc3.gridheight = 1;
		gbc3.anchor = GridBagConstraints.WEST;
		gbc3.weightx = 1;
		gbc3.insets = new Insets(3, 3, 3, 3);
		gbc3.fill = GridBagConstraints.HORIZONTAL;
		operatorPanel.setBorder(buildTitledBorder("Texture operators"));

		contrastCheckBox = new JCheckBox(
				"Contrast (Inertia) (Sum of Squares Variance)");
		contrastCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 0;
		operatorPanel.add(contrastCheckBox, gbc3);
		contrastCheckBox.setSelected(false);

		dissimilarityCheckBox = new JCheckBox("Dissimilarity");
		dissimilarityCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 1;
		operatorPanel.add(dissimilarityCheckBox, gbc3);
		dissimilarityCheckBox.setSelected(false);

		homogeneityCheckBox = new JCheckBox(
				"Homogeneity (Inverse Difference Moment of Order 2)");
		homogeneityCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 2;
		operatorPanel.add(homogeneityCheckBox, gbc3);
		homogeneityCheckBox.setSelected(false);

		inverseOrder1CheckBox = new JCheckBox(
				"Inverse Difference Moment of Order 1");
		inverseOrder1CheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 3;
		operatorPanel.add(inverseOrder1CheckBox, gbc3);
		inverseOrder1CheckBox.setSelected(false);

		asmCheckBox = new JCheckBox("Angular Second Moment (ASM)");
		asmCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 4;
		operatorPanel.add(asmCheckBox, gbc3);
		asmCheckBox.setSelected(false);

		energyCheckBox = new JCheckBox("Energy (Uniformity)");
		energyCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 5;
		operatorPanel.add(energyCheckBox, gbc3);
		energyCheckBox.setSelected(false);

		maxProbabilityCheckBox = new JCheckBox("Maximum Probability (MAX)");
		maxProbabilityCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 6;
		operatorPanel.add(maxProbabilityCheckBox, gbc3);
		maxProbabilityCheckBox.setSelected(false);

		entropyCheckBox = new JCheckBox("Entropy (ENT)");
		entropyCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 7;
		operatorPanel.add(entropyCheckBox, gbc3);
		entropyCheckBox.setSelected(false);

		meanCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Mean");
		meanCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 8;
		operatorPanel.add(meanCheckBox, gbc3);
		meanCheckBox.setSelected(false);

		varianceCheckBox = new JCheckBox(
				"Gray Level Co-occurrence Matrix Variance");
		varianceCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 9;
		operatorPanel.add(varianceCheckBox, gbc3);
		varianceCheckBox.setSelected(false);

		standardDeviationCheckBox = new JCheckBox(
				"Gray Level Co-occurrence Matrix Standard Deviation");
		standardDeviationCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 10;
		operatorPanel.add(standardDeviationCheckBox, gbc3);
		standardDeviationCheckBox.setSelected(false);

		correlationCheckBox = new JCheckBox(
				"Gray Level Co-occurrence Matrix Correlation");
		correlationCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 11;
		operatorPanel.add(correlationCheckBox, gbc3);
		correlationCheckBox.setSelected(false);

		shadeCheckBox = new JCheckBox("Cluster shade");
		shadeCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 12;
		operatorPanel.add(shadeCheckBox, gbc3);
		shadeCheckBox.setSelected(false);

		promenanceCheckBox = new JCheckBox("Cluster promenance");
		promenanceCheckBox.setFont(serif12);
		gbc3.gridx = 0;
		gbc3.gridy = 13;
		operatorPanel.add(promenanceCheckBox, gbc3);
		promenanceCheckBox.setSelected(false);

		gbc.gridx = 0;
		gbc.gridy = ypos++;
		haralickPanel.add(operatorPanel, gbc);

		getContentPane().add(haralickPanel, BorderLayout.CENTER);
	}

	// *******************************************************************
	// ************************* Item Events ****************************
	// *******************************************************************

	/**
	 * Use the GUI results to set up the variables needed to run the algorithm.
	 * 
	 * @return <code>true</code> if parameters set successfully,
	 *         <code>false</code> otherwise.
	 */
	private boolean setVariables() {
		String tmpStr;
		numDirections = 0;
		numOperators = 0;

		if (image.isColorImage()) {

			if (redButton.isSelected()) {
				RGBOffset = RED_OFFSET;
			} else if (greenButton.isSelected()) {
				RGBOffset = GREEN_OFFSET;
			} else {
				RGBOffset = BLUE_OFFSET;
			}
		} // if (image.isColorImage())

		tmpStr = textWindowSize.getText();

		if (testParameter(tmpStr, 3, Integer.MAX_VALUE)) {
			windowSize = Integer.valueOf(tmpStr).intValue();
		} else {
			textWindowSize.requestFocus();
			textWindowSize.selectAll();

			return false;
		}

		if ((windowSize % 2) == 0) {
			MipavUtil.displayError("Window size must not be even");
			textWindowSize.requestFocus();
			textWindowSize.selectAll();

			return false;
		}

		if (windowSize > image.getExtents()[0]) {
			MipavUtil
					.displayError("Window size must not excced image width of "
							+ image.getExtents()[0]);
			textWindowSize.requestFocus();
			textWindowSize.selectAll();

			return false;
		}

		if (windowSize > image.getExtents()[1]) {
			MipavUtil
					.displayError("Window size must not excced image height of "
							+ image.getExtents()[1]);
			textWindowSize.requestFocus();
			textWindowSize.selectAll();

			return false;
		}

		tmpStr = textOffsetDistance.getText();

		if (testParameter(tmpStr, 1, windowSize - 1)) {
			offsetDistance = Integer.valueOf(tmpStr).intValue();
		} else {
			textOffsetDistance.requestFocus();
			textOffsetDistance.selectAll();

			return false;
		}

		tmpStr = textRescaling.getText();

		if (testParameter(tmpStr, 8, 64)) {
			greyLevels = Integer.valueOf(tmpStr).intValue();
		} else {
			textRescaling.requestFocus();
			textRescaling.selectAll();

			return false;
		}

		ns = nsCheckBox.isSelected();

		nesw = neswCheckBox.isSelected();

		ew = ewCheckBox.isSelected();

		senw = senwCheckBox.isSelected();

		invariantDir = invariantCheckBox.isSelected();

		numDirections = getNumDirections();

		if (numDirections == 0) {
			MipavUtil.displayError("At least one direction must be checked");

			return false;
		}

		contrast = contrastCheckBox.isSelected();

		dissimilarity = dissimilarityCheckBox.isSelected();

		homogeneity = homogeneityCheckBox.isSelected();

		inverseOrder1 = inverseOrder1CheckBox.isSelected();

		asm = asmCheckBox.isSelected();

		energy = energyCheckBox.isSelected();

		maxProbability = maxProbabilityCheckBox.isSelected();

		entropy = entropyCheckBox.isSelected();

		mean = meanCheckBox.isSelected();

		variance = varianceCheckBox.isSelected();

		standardDeviation = standardDeviationCheckBox.isSelected();

		correlation = correlationCheckBox.isSelected();

		shade = shadeCheckBox.isSelected();

		promenance = promenanceCheckBox.isSelected();
		
		gaborFilter = gaborFilterCheckBox.isSelected();
		
		imageOriginFilter = imageOriginCheckBox.isSelected();
		
		if ( imageOriginFilter ) {
			imageOriginNumber = 0;
		} else {
			imageOriginNumber = 1;
		}

		numOperators = getNumOperators();

		if (numOperators == 0) {
			MipavUtil
					.displayError("At least 1 texture operator must be selected");

			return false;
		}

		// Gabor filter
		tmpStr = textFU.getText();

		if (testParameter(tmpStr, -1.0, 1.0)) {
			freqU = Float.valueOf(tmpStr).floatValue();
		} else {
			MipavUtil
					.displayError("Horizontal frequency must be between -1.0 and 1.0");
			textFU.requestFocus();
			textFU.selectAll();

			return false;
		}

		tmpStr = textFV.getText();

		if (testParameter(tmpStr, -1.0, 1.0)) {
			freqV = Float.valueOf(tmpStr).floatValue();
		} else {
			MipavUtil
					.displayError("Vertical frequency must be between -1.0 and 1.0");
			textFV.requestFocus();
			textFV.selectAll();

			return false;
		}

		tmpStr = textSU.getText();

		if (testParameter(tmpStr, 1.0E-6, 1.0E6)) {
			sigmaU = Float.valueOf(tmpStr).floatValue();
		} else {
			MipavUtil
					.displayError("Horizontal standard deviation must be between 1.0E-6 and 1.0e6");
			textSU.requestFocus();
			textSU.selectAll();

			return false;
		}

		tmpStr = textSV.getText();

		if (testParameter(tmpStr, 1.0E-6, 1.0E6)) {
			sigmaV = Float.valueOf(tmpStr).floatValue();
		} else {
			MipavUtil
					.displayError("Vertical standard deviation must be between 1.0E-6 and 1.0E6");
			textSV.requestFocus();
			textSV.selectAll();

			return false;
		}

		tmpStr = textTheta.getText();
		theta = (float) (Float.valueOf(tmpStr).floatValue() * Math.PI / 180.0);

		return true;
	}

}
