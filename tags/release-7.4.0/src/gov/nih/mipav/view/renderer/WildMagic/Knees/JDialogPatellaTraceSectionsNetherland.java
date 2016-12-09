package gov.nih.mipav.view.renderer.WildMagic.Knees;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.view.renderer.WildMagic.AAM.*;
import gov.nih.mipav.util.*;

/**
 * Patella segmentation for Netherland data. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogPatellaTraceSectionsNetherland extends JDialogBase implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445422224259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** AAM model reference. */
	private C_AAMMODEL model = new C_AAMMODEL();

	/** GUI interface */
	/** key image directory. */
	private JLabel labelModel;
	private JTextField textFieldModel;
	private JButton buttonModel;

	/** Target image directory. */
	private JLabel labelImageFAT;
	private JTextField textFieldImageFAT;
	private JButton buttonImageFAT;

	/** GRE image directory. */
	private JLabel labelImageGRE;
	private JTextField textFieldImageGRE;
	private JButton buttonImageGRE;

	private JPanel imageSelectionPanel;
	private JPanel buttonPanel;

	/** key images variables. */
	private JFileChooser modelChooser = new JFileChooser();
	private String modelDirectory;

	/** target image variables. */
	private JFileChooser fatImageChooser = new JFileChooser();
	private String fatImageName;
	private String fatImageDirectory;

	private JFileChooser greImageChooser = new JFileChooser();
	private String greImageName;
	private String greImageDirectory;

	private ModelImage fatImage;

	private ModelImage greImage;

	/**
	 * Cropped image region.
	 */
	private int boxYmin, boxYmax;
	private int boxXmin, boxXmax;

	/** Cropped region bounding. */
	private int[] xBounds = new int[2];
	private int[] yBounds = new int[2];
	private int[] zBounds = new int[2];

	/** axis region */
	private JComboBox axisList;
	private JLabel labelAxis;

	/** axis orientation. Will be used later. */
	private static int Axial = 0;
	private static int Saggital = 1;
	private static int Coronal = 2;
	private int axis = Axial;

	private static final String[] statsToCalculate = new String[] { VOIStatisticalProperties.areaDescription, VOIStatisticalProperties.perimeterDescription,
			VOIStatisticalProperties.avgIntensity, VOIStatisticalProperties.eccentricityDescription, VOIStatisticalProperties.majorAxisDescription,
			VOIStatisticalProperties.minorAxisDescription };

	private static final boolean[] checkList = new boolean[VOIStatisticalProperties.numberOfStatistics];

	private static final String MipavCoordinateSystem = null;

	private static int MidToBegin = 0;
	private static int MidToEnd = 1;
	
	static {
		String[] statDescr = VOIStatisticalProperties.statisticDescription;
		for (int i = 0; i < statDescr.length; i++) {
			checkList[i] = false;
			for (int j = 0; j < statsToCalculate.length; j++) {
				if (statDescr[i].equals(statsToCalculate[j])) {
					checkList[i] = true;
					break;
				}
			}
		}
	}

	private JPanel mainPanel;

	private int startSlice = 5;
	private int endSlice = 75;

	private Vector<String> endSliceImageNames = new Vector<String>();
	private Vector<String> endSliceRangeNames = new Vector<String>();

	private Vector<ModelImage> endSliceImages = new Vector<ModelImage>();
	private Vector<Range> endSliceRange = new Vector<Range>();

	private Vector<Vector<ModelImage>> imageStackFuzzyC = new Vector<Vector<ModelImage>>();

	// private ModelImage gaussianMap;

	private static int rowNumber[] = { -1, -1, -1, 0, 0, 1, 1, 1 };
	private static int colNumber[] = { -1, 0, 1, -1, 1, -1, 0, 1 };

	private int startPt;
	private int midPt;
	private int endPt;

	private VOIVector voiVectorFinal = new VOIVector();
	VOI voiNewFinal = new VOI((short) 0, "ImageVOI");

	private static int FuzzyC = 0;
	private static int Class1 = 1;
	private static int Class2 = 2;
	private static int Class3 = 3;
	private static int GRE = 5;
	private static int Class1_weak = 6;
    private static int Class1_lowInten = 7;
	private static int Class1_corner = 8;
	private static int FuzzyC_ALL = 9;
	public static int GRE_GREY = 11;
	public static int GRE_BLACK = 12;
	private static int GRE_SECTION3_DARK = 13;
	private static int GRE_SECTION4_HIGH_THRESHOLD = 14;
	
	
	// red color section
	static float section1_degree_start;
	static float section1_degree_end;

	// green color section
	static float section2_degree_start;
	static float section2_degree_end;

	// blue color section
	static float section3_degree_start;
	static float section3_degree_end;

	// pink color section
	static float section4_degree_start;
	static float section4_degree_end;

	// yellow color section
	static float section5_degree_upperHalf_start;
	static float section5_degree_upperHalf_end;

	static float section5_degree_lowerHalf_start;
	static float section5_degree_lowerHalf_end;

	// black color section
	static float section6_degree_start;
	static float section6_degree_end;

	boolean firstAssigned = false;

	int numSteps;
	int tenPercent;
	// int fivePercent;
	int twentyPercent;

	private RegularEdgePattern edgePattern = new RegularEdgePattern();
	private HolesPattern holesPattern = new HolesPattern();
	private WeakConnectedEdgePatternHorizontal weakEdgePatternHorizontal = new WeakConnectedEdgePatternHorizontal();
	private CornerEdgePattern cornerEdgePattern = new CornerEdgePattern();
	private WeakConnectedEdgePatternVertical weakEdgePatternVertical = new WeakConnectedEdgePatternVertical();
	private RegionPattern regionPattern = new RegionPattern();
	
	/**
	 * Constructor. the parent frame
	 * 
	 * @param theParentFrame
	 */
	public JDialogPatellaTraceSectionsNetherland(Frame theParentFrame, ModelImage srcImage) {
		super(theParentFrame, false);
		greImage = srcImage;

		int slices[] = new int[3];
		configVOIsNumbers(srcImage, slices);
		startPt = slices[0];
		midPt = slices[1];
		endPt = slices[2];

		// System.err.println("startPt = " + startPt + " midPt = " + midPt +
		// " endPt = " + endPt);

		UI = ViewUserInterface.getReference();
		init();

	}

	public void configVOIsNumbers(ModelImage image, int[] slices) {

		VOIVector src = image.getVOIs();
		int zDim = image.getExtents()[2];
		int count = 0;

		Vector<VOIBase>[] vArray = src.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);
		for (int i = 0; i < zDim; i++) {
			if (vArray[i].size() > 0) {
				VOIBase v = vArray[i].get(0);
				if (v != null && v.size() > 0) {
					slices[count] = i;
					// System.out.println(" i = " + slices[count]);
					count++;
				}
			}
		} // end for i loop

		int min, max, mid = 0;
		min = Math.min(slices[2], Math.min(slices[0], slices[1]));
		max = Math.max(slices[2], Math.max(slices[0], slices[1]));
		for (int i = 0; i < 3; i++) {
			if (slices[i] != min && slices[i] != max) {
				mid = slices[i];
				break;
			}
		}
		slices[0] = min;
		slices[1] = mid;
		slices[2] = max;

	}

	public void algorithmPerformed(AlgorithmBase algorithm) {
		// dispose();
	}

	/**
	 * actionPerformed handler.
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();

		if (command.equals("OK")) {
			setVisible(false);
			doSegmentation(true);
		} else if (command.equals("quickSegOK")) {
			setVisible(false);
			doSegmentation(false);
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("ChooseFATImage")) {
			readFATImage();
		}
	}

	/**
	 * Init() function to create the GUI dialog.
	 */
	public void init() {

		setTitle("Knees Segmentation Auto");

		mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		imageSelectionPanel = new JPanel();
		imageSelectionPanel.setLayout(new GridLayout(1, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Auto Segmentation"));

		gbc.gridx = 0;
		gbc.gridy = 0;

		// target image directory
		labelImageFAT = new JLabel("FAT Image: ");
		labelImageFAT.setFont(serif12);
		labelImageFAT.setForeground(Color.black);

		imageSelectionPanel.add(labelImageFAT, gbc);

		textFieldImageFAT = new JTextField(20);
		textFieldImageFAT.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldImageFAT, gbc);

		buttonImageFAT = new JButton("Choose");
		buttonImageFAT.addActionListener(this);
		buttonImageFAT.setActionCommand("ChooseFATImage");
		buttonImageFAT.setFont(serif12B);
		buttonImageFAT.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonImageFAT, gbc);

		// button Panel
		buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridLayout(1, 3));
		gbc.gridx = 0;
		gbc.gridy = 0;
		buttonPanel.add(buildOKButton(), gbc);
		gbc.gridy = 1;
		buttonPanel.add(buildCancelButton(), gbc);
		gbc.gridy = 2;
		buttonPanel.add(buildHelpButton(), gbc);

		mainPanel.add(imageSelectionPanel);
		mainPanel.add(buttonPanel);
		getContentPane().add(mainPanel);
		pack();
		setVisible(true);

	}

	/**
	 * First time the dialog called, this function is invoked to do
	 * segmentation.
	 */
	public void doSegmentation(boolean doCropImage) {
		long startTime = System.currentTimeMillis();

		ModelImage cedImage = runCED(fatImage);
		doFuzzyCmean(cedImage, imageStackFuzzyC);

		segmentationAuto();

		// transformVOI();
		new ViewJFrameImage(fatImage);

		// disposeLocal();

		long endTime = System.currentTimeMillis();
		long diffTime = endTime - startTime;
		float seconds = ((float) diffTime) / 1000;
		System.err.println("** Algorithm took  " + (float) (seconds) + "  seconds \n");

	}

	public ModelImage generateGaussianMap() {
		ModelImage resultImage = null;
		float[] sigmas = new float[3];
		sigmas[0] = 5.0f;
		sigmas[1] = 5.0f;
		sigmas[2] = 1.0f;
		boolean image25D = true;
		boolean isProcessWholeImageSet = true;
		AlgorithmGaussianBlurSep gaussianBlurSepAlgo = new AlgorithmGaussianBlurSep(greImage, sigmas, isProcessWholeImageSet, image25D);
		gaussianBlurSepAlgo.addListener(this);
		gaussianBlurSepAlgo.setRed(false);
		gaussianBlurSepAlgo.setGreen(false);
		gaussianBlurSepAlgo.setBlue(false);
		gaussianBlurSepAlgo.run();
		if (gaussianBlurSepAlgo.isCompleted()) {
			resultImage = (ModelImage) greImage.clone();
			try {
				resultImage.importData(0, gaussianBlurSepAlgo.getResultBuffer(), true);
			} catch (final IOException e) {
				resultImage.disposeLocal();
				MipavUtil.displayError("Algorithm Gausssian Blur importData: Image(s) Locked.");
			}
		}
		return resultImage;
	}

	public void doFuzzyCmean(ModelImage coherenceEnhancingDiffusionImage, Vector<Vector<ModelImage>> imageStackFuzzyC) {

		int xDim = coherenceEnhancingDiffusionImage.getExtents()[0];
		int yDim = coherenceEnhancingDiffusionImage.getExtents()[1];
		int zDim = coherenceEnhancingDiffusionImage.getExtents()[2];

		int[] destExtents = new int[2];
		destExtents[0] = coherenceEnhancingDiffusionImage.getExtents()[0];
		destExtents[1] = coherenceEnhancingDiffusionImage.getExtents()[1];

		int sliceSize = xDim * yDim;
		int midSlice = (int) ((zDim) / 2f);
		System.out.println("loadMask: midSlice = " + midSlice);
		for (int z = 0; z < zDim; z++) {

			try {
				// currentSlice = z;
				int start = z * sliceSize;
				double[] sourceBuffer = new double[sliceSize];
				ModelImage[] resultImageFuzzyC = null;
				coherenceEnhancingDiffusionImage.exportData(start, sliceSize, sourceBuffer);

				ModelImage testImageFuzzyC = new ModelImage(coherenceEnhancingDiffusionImage.getType(), destExtents, "testFuzzyC" + z);
				testImageFuzzyC.importData(0, sourceBuffer, true);
				boolean endianness = coherenceEnhancingDiffusionImage.getFileInfo(0).getEndianess();
				int modality = coherenceEnhancingDiffusionImage.getFileInfo(0).getModality();
				float[] res = coherenceEnhancingDiffusionImage.getFileInfo(0).getResolutions();
				float[] newRes = { res[0], res[1] };
				int[] exts = coherenceEnhancingDiffusionImage.getFileInfo(0).getExtents();
				int[] newExts = { exts[0], exts[1] };
				int[] units = coherenceEnhancingDiffusionImage.getFileInfo(0).getUnitsOfMeasure();
				int[] newUnits = { units[0], units[1] };
				int imageOrient = coherenceEnhancingDiffusionImage.getFileInfo(0).getImageOrientation();
				int[] axisOrient = coherenceEnhancingDiffusionImage.getFileInfo(0).getAxisOrientation();
				int[] newAxisOrient = { axisOrient[0], axisOrient[1] };
				float[] origin = coherenceEnhancingDiffusionImage.getFileInfo(0).getOrigin();
				float[] newOrigin = { origin[0], origin[1] };

				FileInfoImageXML fileInfo = new FileInfoImageXML(coherenceEnhancingDiffusionImage.getImageName(), null, FileUtility.XML);
				fileInfo.setDataType(ModelStorageBase.FLOAT);
				fileInfo.setEndianess(endianness);
				fileInfo.setExtents(newExts);
				fileInfo.setModality(modality);
				fileInfo.setResolutions(newRes);
				fileInfo.setUnitsOfMeasure(newUnits);
				fileInfo.setImageOrientation(imageOrient);
				fileInfo.setAxisOrientation(newAxisOrient);
				fileInfo.setOrigin(newOrigin);

				FileInfoImageXML[] fileInfos = { fileInfo };
				testImageFuzzyC.setFileInfo(fileInfos);

				boolean regionFlag = true;
				int nClasses = 3;
				float q = 2.0f;
				boolean cropBackground = false;
				float endTol = 0.01f;
				int maxIter = 200;
				int segmentation = AlgorithmFuzzyCMeans.BOTH_FUZZY_HARD;
				int resultNumber = 4;
				int nPyramid = 4;
				int oneJacobiIter = 1;
				float oneSmooth = 20000;
				boolean outputGainField = false;
				int twoJacobiIter = 2;
				float twoSmooth = 200000;
				float threshold = 800;

				try {
					resultImageFuzzyC = new ModelImage[resultNumber];
					int presentNumber = 0;

					for (int i = 0; i < nClasses; i++) {
						String name = makeImageName(testImageFuzzyC.getImageName(), "_class" + (i + 1));
						resultImageFuzzyC[presentNumber++] = new ModelImage(ModelStorageBase.FLOAT, destExtents, name);
					}
					resultImageFuzzyC[presentNumber++] = new ModelImage(ModelStorageBase.UBYTE, destExtents, makeImageName(testImageFuzzyC.getImageName(),
							"_seg"));

					// fuzzyC parameters

					// Make algorithm
					AlgorithmFuzzyCMeans fcmAlgo = new AlgorithmFuzzyCMeans(resultImageFuzzyC, testImageFuzzyC, nClasses, nPyramid, oneJacobiIter,
							twoJacobiIter, q, oneSmooth, twoSmooth, outputGainField, segmentation, cropBackground, threshold, maxIter, endTol, regionFlag);

					fcmAlgo.addListener(this);

					float[] centroids = null;
					centroids = getCentroids(testImageFuzzyC, fcmAlgo, centroids);

					fcmAlgo.setCentroids(centroids);
					// fcmAlgo.setThreshold(threshold);
					fcmAlgo.run();

					centroids = null;

					int ii;
					testImageFuzzyC.clearMask();
					Vector<ModelImage> imageStackElement = new Vector<ModelImage>();
					for (ii = 0; ii < resultNumber; ii++) {
						updateFileInfo(testImageFuzzyC, resultImageFuzzyC[ii]);
						resultImageFuzzyC[ii].clearMask();
						imageStackElement.add(resultImageFuzzyC[ii]);
					}
					imageStackFuzzyC.add(imageStackElement);
					testImageFuzzyC.disposeLocal();
					testImageFuzzyC = null;

					fcmAlgo.setCompleted(true);
					fcmAlgo.finalize();
					fcmAlgo = null;

				} catch (OutOfMemoryError x) {

					if (resultImageFuzzyC != null) {
						for (int i = 0; i < resultNumber; i++) {
							if (resultImageFuzzyC[i] != null) {
								resultImageFuzzyC[i].disposeLocal();
								resultImageFuzzyC[i] = null;
							}
						}
						resultImageFuzzyC = null;
					}

					// System.gc();
					MipavUtil.displayError("MS Fuzzy CMeans: unable to allocate enough memory");
					return;
				}

				// z = endSlice; // debug ending condition.
				sourceBuffer = null;

			} catch (IOException e) {
				e.printStackTrace();
			}

			// z = endSlice;

		} // end z loop

	}

	private float[] getCentroids(ModelImage srcImage, AlgorithmFuzzyCMeans fcmAlgo, float[] centroids) {
		boolean regionFlag = true;
		int nClasses = 5;
		float q = 2.0f;
		boolean cropBackground = false;
		float endTol = 0.01f;
		int maxIter = 200;
		int segmentation = AlgorithmFuzzyCMeans.BOTH_FUZZY_HARD;
		int resultNumber = 6;
		int nPyramid = 4;
		int oneJacobiIter = 1;
		float oneSmooth = 20000;
		boolean outputGainField = false;
		int twoJacobiIter = 2;
		float twoSmooth = 200000;
		float threshold = 0;

		int i;
		float minimum, maximum;
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int zDim;

		if (srcImage.getNDims() > 2) {
			zDim = srcImage.getExtents()[2];
		} else {
			zDim = 1;
		}

		int sliceSize = xDim * yDim;
		int volSize = xDim * yDim * zDim;
		float[] buffer = null;
		int yStepIn, yStepOut, zStepIn, zStepOut;
		int x, y, z, index, newXDim, newYDim, newZDim, newSliceSize;

		try {
			buffer = new float[volSize];
			srcImage.exportData(0, volSize, buffer);

			srcImage.calcMinMax();
			minimum = (float) srcImage.getMin();
			maximum = (float) srcImage.getMax();

			if (!regionFlag) {
				maximum = -Float.MAX_VALUE;
				minimum = Float.MAX_VALUE;

				for (i = 0; i < volSize; i++) {

					if (fcmAlgo.getMask().get(i)) {

						if (buffer[i] > maximum) {
							maximum = buffer[i];
						}

						if (buffer[i] < minimum) {
							minimum = buffer[i];
						}
					}
				}
			} // if (!wholeImage)

			int xLow = 0;
			int yLow = 0;
			int zLow = 0;
			int xHigh = xDim - 1;
			int yHigh = yDim - 1;
			int zHigh = zDim - 1;

			if (cropBackground) {

				// Find the smallest bounding box for the data
				xLow = xDim - 1;
				yLow = yDim - 1;
				zLow = zDim - 1;
				xHigh = 0;
				yHigh = 0;
				zHigh = 0;

				for (z = 0; z < zDim; z++) {
					zStepIn = z * sliceSize;

					for (y = 0; y < yDim; y++) {
						yStepIn = (y * xDim) + zStepIn;

						for (x = 0; x < xDim; x++) {
							index = x + yStepIn;

							if (buffer[index] >= threshold) {

								if (x < xLow) {
									xLow = x;
								}

								if (x > xHigh) {
									xHigh = x;
								}

								if (y < yLow) {
									yLow = y;
								}

								if (y > yHigh) {
									yHigh = y;
								}

								if (z < zLow) {
									zLow = z;
								}

								if (z > zHigh) {
									zHigh = z;
								}
							} // if (buffer[index] > threshold)
						} // for (x = 0; x < xDim; x++)
					} // for (y = 0; y < yDim; y++)
				} // for (z = 0; z < zDim; z++)

				if ((xLow > 0) || (xHigh < (xDim - 1)) || (yLow > 0) || (yHigh < (yDim - 1)) || (zLow > 0) || (zHigh < (zDim - 1))) {

					// A smaller bounding box has been found for the data
					// Recopy area to smaller data array to save space
					newXDim = xHigh - xLow + 1;
					newYDim = yHigh - yLow + 1;
					newZDim = zHigh - zLow + 1;

					float[] buffer2 = new float[newXDim * newYDim * newZDim];
					newSliceSize = newXDim * newYDim;

					for (z = zLow; z <= zHigh; z++) {
						zStepOut = z * sliceSize;
						zStepIn = ((z - zLow) * newSliceSize) - xLow - (yLow * newXDim);

						for (y = yLow; y <= yHigh; y++) {
							yStepIn = (y * newXDim) + zStepIn;
							yStepOut = (y * xDim) + zStepOut;

							for (x = xLow; x <= xHigh; x++) {
								buffer2[x + yStepIn] = buffer[x + yStepOut];
							} // for (x = xLow; x <= xHigh; x++)
						} // for (y = yLow; y <= yHigh; y++)
					} // for (z = zLow; z <= zHigh; z++)

					xDim = newXDim;
					yDim = newYDim;
					zDim = newZDim;
					sliceSize = xDim * yDim;
					volSize = sliceSize * zDim;
					buffer = new float[volSize];

					for (i = 0; i < sliceSize; i++) {
						buffer[i] = buffer2[i];
					}

					buffer2 = null;

					// Find the new minimum
					minimum = maximum;

					for (i = 0; i < volSize; i++) {

						if (buffer[i] < minimum) {
							minimum = buffer[i];
						} // if (buffer[i] < minimum)
					} // for (i = 0; i < sliceSize; i++)
				} // if ((xLow > 0) || (xHigh < (xDim-1)) || (yLow > 0) ||
					// (yHigh < (yDim - 1)))
			} // if (cropBackground)
		} catch (java.io.IOException ioe) {
			buffer = null;
			// System.gc();
			MipavUtil.displayError("Error trying to get centroids.");

			return null;
		} catch (OutOfMemoryError error) {
			buffer = null;
			// System.gc();
			MipavUtil.displayError("Algorithm FuzzyCMeans reports:\n" + error.toString());

			return null;
		}

		buffer = null;

		// Autodetect initial centroids
		centroids = new float[nClasses];

		JDialogInitialCentroids dialogInitialCentroids = new JDialogInitialCentroids(parentFrame, nClasses, minimum, maximum, false);
		dialogInitialCentroids.run();

		if (dialogInitialCentroids.isCancelled()) {
			centroids = null;
			return null;
		} else {

			for (i = 0; i < centroids.length; i++) {
				centroids[i] = dialogInitialCentroids.getCentroids()[i];
			}
		}

		return centroids;
	}

	/**
	 * Polygon shape info class to do shape comparison after the AAM
	 * classification applied to do the segmentation.
	 * 
	 * @author ruida
	 * 
	 */
	class PolygonShapeInfo {
		public poly polygon;
		public float area;

		public PolygonShapeInfo(poly _polygon, float _area) {
			polygon = _polygon;
			area = _area;
		}
	}

	/**
	 * BSpline smooth the final auto segmentation VOI contours.
	 */
	public void transformVOI() {
		float[] xPtsCurrent;
		float[] yPtsCurrent;
		float[] zPtsCurrent;

		VOIVector voiVectorNew = new VOIVector();
		VOI voiNew = new VOI((short) 0, "ImageVOI");
		voiVectorNew.add(voiNew);
		int currentSlice;
		int end = endSlice - startSlice;
		for (int i = 0; i <= end; i++) {
			currentSlice = i + startSlice;

			// System.err.println("curentSlice = " + currentSlice);
			if (fatImage.getVOIs().VOIAt(i) != null) {
				VOIBaseVector current_va = fatImage.getVOIs().VOIAt(i).getCurves();
				if (current_va.size() > 0) {
					VOIBase current_v = current_va.get(0);

					VOIBase vTemp = (VOIBase) current_v.clone();

					int nPtsCurrent = current_v.size();
					xPtsCurrent = new float[nPtsCurrent];
					yPtsCurrent = new float[nPtsCurrent];
					zPtsCurrent = new float[nPtsCurrent];
					current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);

					Vector3f[] BsplinePoints = new Vector3f[nPtsCurrent];
					for (int w = 0; w < nPtsCurrent; w++) {
						BsplinePoints[w] = new Vector3f(xPtsCurrent[w], yPtsCurrent[w], 0);
					}

					BSplineCurve3f curve = BSplineCurve3f.CreateApproximation(BsplinePoints, nPtsCurrent - 5, 2);
					float minTime = curve.GetMinTime();
					float maxTime = curve.GetMaxTime();
					float step = (maxTime - minTime) / 30f;

					Vector<Point> extractedPoints = new Vector<Point>();

					for (float t = minTime; t <= maxTime; t += step) {
						Vector3f pt = curve.GetPosition(t);
						extractedPoints.add(new Point((int) pt.X, (int) pt.Y));
					}

					int nPts = extractedPoints.size();

					float[] zSlice = new float[nPts];

					for (int j = 0; j < nPts; j++) {
						Point p = extractedPoints.get(j);
						xPtsCurrent[j] = p.x;
						yPtsCurrent[j] = p.y;
						zSlice[j] = currentSlice;
					}

					vTemp.importArrays(xPtsCurrent, yPtsCurrent, zSlice, nPts);
					voiNew.importCurve(vTemp);
					vTemp = null;
				}
			}
		}

		fatImage.getVOIs().removeAllElements();
		fatImage.addVOIs(voiVectorNew);
	}

	public void generateInOutContours(VOIBase startVOI, float[] innerDist, float[] outerDist, Vector3f pt_inner[], Vector3f pt_outer[], Vector3f pt_voi[],
			float[] center) {

		float posX, posY;
		int nPtsCurrent = startVOI.size();

		float stepX, stepY;
		int index_inner;
		int index_outer;

		int inward_steps;
		int outward_steps;

		float traceX, traceY;
		float centerX = 0f, centerY = 0f;

		// zero out the z dimension VOI
		float[] xPts = new float[nPtsCurrent];
		float[] yPts = new float[nPtsCurrent];
		float[] zPts = new float[nPtsCurrent];

		startVOI.exportArrays(xPts, yPts, zPts);

		for (int i = 0; i < nPtsCurrent; i++) {
			centerX += xPts[i];
			centerY += yPts[i];
		}

		centerX /= nPtsCurrent;
		centerY /= nPtsCurrent;

		center[0] = centerX;
		center[1] = centerY;

		for (int i = 0; i < nPtsCurrent; i++) {

			posX = xPts[i];
			posY = yPts[i];

			pt_voi[i] = new Vector3f(posX, posY, 0f);

			stepX = (posX - centerX) / 400f;
			stepY = (posY - centerY) / 400f;

			index_inner = 0;
			index_outer = 0;

			traceX = posX;
			traceY = posY;

			inward_steps = 20;
			outward_steps = 20;

			while (index_inner < inward_steps) {
				traceX -= stepX;
				traceY -= stepY;
				index_inner++;
			}
			pt_inner[i] = new Vector3f(traceX, traceY, 0f);
			innerDist[i] = (float) Math.sqrt((traceX - centerX) * (traceX - centerX) + (traceY - centerY) * (traceY - centerY));

			traceX = posX;
			traceY = posY;

			while (index_outer < outward_steps) {
				traceX += stepX;
				traceY += stepY;
				index_outer++;
			}
			pt_outer[i] = new Vector3f(traceX, traceY, 0f);
			outerDist[i] = (float) Math.sqrt((traceX - centerX) * (traceX - centerX) + (traceY - centerY) * (traceY - centerY));

		} // end i loop;
	}

	/**
	 * Driver to do the auto segmentation with AAM model.
	 */
	public void segmentationAuto() {

		int[] extents = new int[3];
		extents = fatImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];
		float[] buffer = new float[size];

		voiVectorFinal.add(voiNewFinal);

		boolean endianness = greImage.getFileInfo(0).getEndianess();
		int modality = greImage.getFileInfo(0).getModality();
		float[] res = greImage.getFileInfo(0).getResolutions();
		float[] newRes = { res[0], res[1] };
		int[] exts = greImage.getFileInfo(0).getExtents();
		int[] newExts = { exts[0], exts[1] };
		int[] units = greImage.getFileInfo(0).getUnitsOfMeasure();
		int[] newUnits = { units[0], units[1] };
		int imageOrient = greImage.getFileInfo(0).getImageOrientation();
		int[] axisOrient = greImage.getFileInfo(0).getAxisOrientation();
		int[] newAxisOrient = { axisOrient[0], axisOrient[1] };
		float[] origin = greImage.getFileInfo(0).getOrigin();
		float[] newOrigin = { origin[0], origin[1] };

		FileInfoImageXML fileInfoGRE = new FileInfoImageXML(greImage.getImageName(), null, FileUtility.XML);
		fileInfoGRE.setDataType(ModelStorageBase.FLOAT);
		fileInfoGRE.setEndianess(endianness);
		fileInfoGRE.setExtents(newExts);
		fileInfoGRE.setModality(modality);
		fileInfoGRE.setResolutions(newRes);
		fileInfoGRE.setUnitsOfMeasure(newUnits);
		fileInfoGRE.setImageOrientation(imageOrient);
		fileInfoGRE.setAxisOrientation(newAxisOrient);
		fileInfoGRE.setOrigin(newOrigin);

		FileInfoImageXML[] fileInfosGRE = { fileInfoGRE };

		boolean endiannessFAT = fatImage.getFileInfo(0).getEndianess();
		int modalityFAT = fatImage.getFileInfo(0).getModality();
		float[] resFAT = fatImage.getFileInfo(0).getResolutions();
		float[] newResFAT = { res[0], res[1] };
		int[] extsFAT = fatImage.getFileInfo(0).getExtents();
		int[] newExtsFAT = { exts[0], exts[1] };
		int[] unitsFAT = fatImage.getFileInfo(0).getUnitsOfMeasure();
		int[] newUnitsFAT = { units[0], units[1] };
		int imageOrientFAT = fatImage.getFileInfo(0).getImageOrientation();
		int[] axisOrientFAT = fatImage.getFileInfo(0).getAxisOrientation();
		int[] newAxisOrientFAT = { axisOrient[0], axisOrient[1] };
		float[] originFAT = fatImage.getFileInfo(0).getOrigin();
		float[] newOriginFAT = { origin[0], origin[1] };
		FileInfoImageXML fileInfoFAT = new FileInfoImageXML(fatImage.getImageName(), null, FileUtility.XML);
		fileInfoFAT.setDataType(ModelStorageBase.FLOAT);
		fileInfoFAT.setEndianess(endiannessFAT);
		fileInfoFAT.setExtents(newExtsFAT);
		fileInfoFAT.setModality(modalityFAT);
		fileInfoFAT.setResolutions(newResFAT);
		fileInfoFAT.setUnitsOfMeasure(newUnitsFAT);
		fileInfoFAT.setImageOrientation(imageOrientFAT);
		fileInfoFAT.setAxisOrientation(newAxisOrientFAT);
		fileInfoFAT.setOrigin(newOriginFAT);

		FileInfoImageXML[] fileInfosFAT = { fileInfoFAT };

		VOI resultVOIInner = new VOI((short) 0, "result-VOI" + "_inner", VOI.CONTOUR, -1);
		VOI resultVOIOuter = new VOI((short) 0, "result-VOI" + "_outer", VOI.CONTOUR, -1);
		VOI resultVOIBoundary = new VOI((short) 0, "result-VOI" + "_inner", VOI.CONTOUR, -1);

		VOI midVOIInner = new VOI((short) 0, "mid-VOI" + "_inner", VOI.CONTOUR, -1);
		VOI midVOIOuter = new VOI((short) 0, "mid-VOI" + "_outer", VOI.CONTOUR, -1);
		VOI midVOIBoundary = new VOI((short) 0, "mid-VOI" + "_inner", VOI.CONTOUR, -1);

		startSlice = midPt;
		endSlice = startPt;

		ModelImage greImageSliceMid = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + midPt));
		greImageSliceMid.setFileInfo(fileInfosGRE);

		ModelImage fatImageSliceMid = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "target" + midPt));
		fatImageSliceMid.setFileInfo(fileInfosFAT);

		try {
			greImage.exportData(midPt * size, size, buffer);
			greImageSliceMid.importData(0, buffer, true);

			fatImage.exportData(midPt * size, size, buffer);
			fatImageSliceMid.importData(0, buffer, true);

		} catch (IOException e) {
			e.printStackTrace();
		}

		// tracingDFS_mid(greImageSliceMid, fatImageSliceMid, resultVOIBoundary,
		// resultVOIInner, resultVOIOuter, true);
		tracingDFS_mid(greImageSliceMid, fatImageSliceMid, midVOIBoundary, midVOIInner, midVOIOuter, true, MidToBegin);
		resultVOIBoundary = (VOI) midVOIBoundary.clone();
		resultVOIInner = (VOI) midVOIInner.clone();
		resultVOIOuter = (VOI) midVOIOuter.clone();
      
		for (int i = startSlice - 1; i >= endSlice; i--) {
		// for (int i = startSlice - 1; i >= startSlice - 25; i--) {
			// System.err.println("slice = " + i);

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);


			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);


			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			tracingDFS(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, i, resultVOIBoundary,
					resultVOIInner, resultVOIOuter, MidToBegin);

		} // end for i loop
        
		
		// tracingDFS_mid(greImageSliceMid, fatImageSliceMid, resultVOIBoundary,
		// resultVOIInner, resultVOIOuter, false);
		resultVOIBoundary = (VOI) midVOIBoundary.clone();
		resultVOIInner = (VOI) midVOIInner.clone();
		resultVOIOuter = (VOI) midVOIOuter.clone();

		startSlice = midPt;
		endSlice = endPt;

		
		for (int i = startSlice + 1; i <= endSlice; i++) {
		// for (int i = startSlice + 1; i <= startSlice + 15; i++) {

			System.err.println("slice = " + i);

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);


			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);


			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			tracingDFS(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, i, resultVOIBoundary,
					resultVOIInner, resultVOIOuter, MidToEnd);

		} // end for i loop
         
		// strain();
		// strain1();
		/*
		 * startSlice = startPt; endSlice = endPt;
		 * 
		 * for (int i = startSlice; i <= endSlice; i++) {
		 * 
		 * ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT,
		 * newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
		 * fatImageSlice.setFileInfo(fileInfosFAT);
		 * 
		 * ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT,
		 * newExtents, makeImageName(greImage.getImageName(), "target" + i));
		 * greImageSlice.setFileInfo(fileInfosGRE);
		 * 
		 * ModelImage gaussianImageSlice = new
		 * ModelImage(ModelStorageBase.FLOAT, newExtents,
		 * makeImageName(gaussianMap.getImageName(), "gsblur" + i));
		 * gaussianImageSlice.setFileInfo(fileInfosGRE);
		 * 
		 * try { fatImage.exportData(i * size, size, buffer);
		 * fatImageSlice.importData(0, buffer, true);
		 * 
		 * greImage.exportData(i * size, size, buffer);
		 * greImageSlice.importData(0, buffer, true);
		 * 
		 * gaussianMap.exportData(i * size, size, buffer);
		 * gaussianImageSlice.importData(0, buffer, true); // new
		 * ViewJFrameImage(targetImageSlice);
		 * 
		 * } catch (IOException e) { e.printStackTrace(); }
		 * 
		 * ModelImage class1Image = imageStackFuzzyC.get(i).get(0); ModelImage
		 * class2Image = imageStackFuzzyC.get(i).get(1); ModelImage class3Image
		 * = imageStackFuzzyC.get(i).get(2); ModelImage fuzzyCImage =
		 * imageStackFuzzyC.get(i).get(3);
		 * 
		 * displaySlices(fuzzyCImage, class1Image, class2Image, class3Image,
		 * greImageSlice, gaussianImageSlice, i);
		 * 
		 * }
		 */
		greImage.getVOIs().removeAllElements();
		greImage.addVOIs(voiVectorFinal);
	}

	private void strain1() {

		for (int sliceNumber = startPt; sliceNumber <= endPt - 5; sliceNumber += 5) {

			for (int j = 0; j < 60; j++) {

				if ((j >= 10 && j <= 20) || (j >= 40 && j <= 50)) {

					float[] heightArr = new float[5];

					Hashtable<Integer, Line> linePoints;
					Line line;

					// from each slice number get the relevant line points
					linePoints = slicesPts.get(sliceNumber);

					for (int i = j - 2; i <= j + 2; i++) {
						line = linePoints.get(i);
						heightArr[0] = line.dist;

						line = linePoints.get(i + 1);
						heightArr[1] = line.dist;

						line = linePoints.get(i + 2);
						heightArr[2] = line.dist;

						line = linePoints.get(i + 3);
						heightArr[3] = line.dist;

						line = linePoints.get(i + 4);
						heightArr[4] = line.dist;

						float avg, sum = 0;
						int k;
						for (k = 0; k < 5; k++) {
							sum += heightArr[k];
						}
						avg = sum / 5.0f;

						for (k = 0; k < 5; k++) {

							if (Math.abs(heightArr[k] - avg) > avg * (0.15f)) {
								line = linePoints.get(k + i);
								float dist = line.dist;
								float centerX = line.startX;
								float centerY = line.startY;
								float posX = line.endX;
								float posY = line.endY;
								float stepX, stepY;

								stepX = (posX - centerX) / 100f;
								stepY = (posY - centerY) / 100f;

								if ((heightArr[k] - avg) > 0) {

									while (dist > avg) {

										posX -= stepX;
										posY -= stepY;

										dist = (float) Math.sqrt((posX - centerX) * (posX - centerX) + (posY - centerY) * (posY - centerY));
									}

									linePoints.get(k + i).setPoint(posX, posY);

								} else if ((heightArr[k] - avg) < 0) {

									while (dist < avg) {

										posX += stepX;
										posY += stepY;

										dist = (float) Math.sqrt((posX - centerX) * (posX - centerX) + (posY - centerY) * (posY - centerY));
									}

									linePoints.get(k + i).setPoint(posX, posY);
								}
							}

						}

					}

				}

			} // end j < 60 loop
		}
	}

	private void strain() {

		for (int j = 0; j < 120; j++) {

			float[] heightArr = new float[5];

			for (int sliceNumber = startPt; sliceNumber <= endPt - 5; sliceNumber += 5) {

				Hashtable<Integer, Line> linePoints;
				Line line;

				linePoints = slicesPts.get(sliceNumber);
				line = linePoints.get(j);
				heightArr[0] = line.dist;

				linePoints = slicesPts.get(sliceNumber + 1);
				line = linePoints.get(j);
				heightArr[1] = line.dist;

				linePoints = slicesPts.get(sliceNumber + 2);
				line = linePoints.get(j);
				heightArr[2] = line.dist;

				linePoints = slicesPts.get(sliceNumber + 3);
				line = linePoints.get(j);
				heightArr[3] = line.dist;

				linePoints = slicesPts.get(sliceNumber + 4);
				line = linePoints.get(j);
				heightArr[4] = line.dist;

				float avg, sum = 0;
				int k;
				for (k = 0; k < 5; k++) {
					sum += heightArr[k];
				}
				avg = sum / 5.0f;

				for (k = 0; k < 5; k++) {

					if (Math.abs(heightArr[k] - avg) > avg * (0.3f)) { // > 1/5
																		// avg
																		// dist

						line = slicesPts.get(sliceNumber + k).get(j);
						float dist = line.dist;
						float centerX = line.startX;
						float centerY = line.startY;
						float posX = line.endX;
						float posY = line.endY;
						float stepX, stepY;

						stepX = (posX - centerX) / 100f;
						stepY = (posY - centerY) / 100f;

						if ((heightArr[k] - avg) > 0) {

							while (dist > avg) {

								posX -= stepX;
								posY -= stepY;

								dist = (float) Math.sqrt((posX - centerX) * (posX - centerX) + (posY - centerY) * (posY - centerY));
							}

							slicesPts.get(sliceNumber + k).get(j).setPoint(posX, posY);

						} else if ((heightArr[k] - avg) < 0) {

							while (dist < avg) {

								posX += stepX;
								posY += stepY;

								dist = (float) Math.sqrt((posX - centerX) * (posX - centerX) + (posY - centerY) * (posY - centerY));
							}

							slicesPts.get(sliceNumber + k).get(j).setPoint(posX, posY);
						}
					}
				}

			}
		}

	}

	private void runLevelSet(ModelImage fuzzyCImage) {

		float[] sigmas = new float[2];
		sigmas[0] = 5.0f; // set standard deviations (sigma) in X and Y
		sigmas[1] = 5.0f;
		int movement = 1;
		int iters = 10000;
		float deltaT = 0.05f;
		float epsilon = 0.05f;
		float edgeAttract = 20.0f;
		int testIters = 100;
		boolean image25D = false;

		AlgorithmLevelSet levelSetAlgo = new AlgorithmLevelSet(fuzzyCImage, sigmas, movement, iters, deltaT, epsilon, edgeAttract, testIters, image25D);
		levelSetAlgo.addListener(this);
		levelSetAlgo.run();

	}

	public void createOneContours(ModelImage targetImageSlice) {
		// smoothVOI60(targetImageSlice, targetImageSlice);
		smoothVOI60DualContour(targetImageSlice, targetImageSlice);
		VOI voiNew = targetImageSlice.getVOIs().elementAt(0);

		VOIBaseVector curves = voiNew.getCurves();
		VOIBase srcContour = null;
		for (int index = 0; index < curves.size(); index++) {
			srcContour = curves.elementAt(index);
			generateInnerContour(srcContour, targetImageSlice);

		}
	}

	private void generateInnerContour(VOIBase srcContour, ModelImage targetImageSlice) {
		int sidePointsForTangent = 1;
		int innerDistance = 25;
		boolean doInner = true;
		// boolean doOuter = true;
		int index;
		int i;
		int j;
		int k;
		int m;
		if (srcContour == null || (srcContour.size() == 0)) {
			return;
		}
		int slice = (int) srcContour.elementAt(0).Z;

		int nPoints = srcContour.size();
		Vector3f point;
		float xPoints[] = new float[nPoints + 2 * sidePointsForTangent];
		float yPoints[] = new float[nPoints + 2 * sidePointsForTangent];
		Vector<Vector3f> innerV = new Vector<Vector3f>();
		Vector<Vector3f> outerV = new Vector<Vector3f>();
		float tangentX;
		float tangentY;
		float xCenteredPoints[] = new float[2 * sidePointsForTangent + 1];
		float yCenteredPoints[] = new float[2 * sidePointsForTangent + 1];
		double xSqSum;
		double ySqSum;
		double xySum;
		double var;
		double x1t;
		double x2t;
		double y1t;
		double y2t;
		double slope;
		double d1;
		double d2;
		double xDist;
		double yDist;
		for (i = 0; i < nPoints; i++) {
			point = (srcContour.get(i));
			xPoints[i + sidePointsForTangent] = point.X;
			yPoints[i + sidePointsForTangent] = point.Y;
		}
		for (i = sidePointsForTangent - 1, j = 0; i >= 0; i--, j++) {
			xPoints[i] = xPoints[nPoints - 1 - j];
			yPoints[i] = yPoints[nPoints - 1 - j];
		}
		for (i = nPoints, j = 0; i <= nPoints + sidePointsForTangent - 1; i++, j++) {
			xPoints[i] = xPoints[j];
			yPoints[i] = yPoints[j];
		}
		for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent + nPoints - 1; i++, j++) {
			if (sidePointsForTangent == 1) {
				tangentX = (xPoints[i + 1] - xPoints[i - 1]) / 2.0f;
				tangentY = (yPoints[i + 1] - yPoints[i - 1]) / 2.0f;
				if (tangentY == 0.0f) {
					slope = Double.POSITIVE_INFINITY;
				} else {
					slope = -tangentX / tangentY;
				}
			} // if (sidePointsForTangent == 1)
			else { // sidePointsForTangent > 1
					// Center all points for tangent point touching curve at (0,
					// 0)
					// That is, use an x axis and a y axis going thru the
					// tangent point
				for (k = 0, m = i - sidePointsForTangent; m <= i + sidePointsForTangent; m++, k++) {
					xCenteredPoints[k] = xPoints[m] - xPoints[i];
					yCenteredPoints[k] = yPoints[m] - yPoints[i];
				}
				xSqSum = 0.0;
				ySqSum = 0.0;
				xySum = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					xSqSum += xCenteredPoints[k] * xCenteredPoints[k];
					ySqSum += yCenteredPoints[k] * yCenteredPoints[k];
					xySum += xCenteredPoints[k] * yCenteredPoints[k];
				}
				if (xySum != 0.0) {
					var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
					x1t = 0.5 * ((-ySqSum + xSqSum + var) / xySum);
					x2t = 0.5 * ((-ySqSum + xSqSum - var) / xySum);
					y1t = 1.0;
					y2t = 1.0;
				} else {
					// If all points are symmetric to either this new x axis or
					// this new y axis, then
					// their product sum is 0 and the tangentX, tangentY must be
					// 1,0 or 0,1
					x1t = 1.0;
					x2t = 0.0;
					y1t = 0.0;
					y2t = 1.0;
				}
				// x1t, y1t and x2t, y2t are perpindicular. To find the
				// solution, calculate the sum of
				// distances from the curve points to the line for the 2 cases
				// The shortest distance is the correct solution
				// Distance from AX + BY + C = 0 to P1 is
				// abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
				// Here A = slope, B = -1, and C = 0.
				d1 = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					if (x1t == 0.0) {
						// Infinite slope thru (0,0)
						d1 += Math.abs(yCenteredPoints[k]);
					} else if (y1t == 0.0) {
						// Zero slope thru (0, 0)
						d1 += Math.abs(xCenteredPoints[k]);
					} else {
						slope = y1t / x1t;
						d1 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k]) / Math.sqrt(slope * slope + 1));
					}
				}
				d2 = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					if (x2t == 0.0) {
						// Infinite slope thru (0,0)
						d2 += Math.abs(yCenteredPoints[k]);
					} else if (y2t == 0.0) {
						// Zero slope thru (0, 0)
						d2 += Math.abs(xCenteredPoints[k]);
					} else {
						slope = y2t / x2t;
						d2 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k]) / Math.sqrt(slope * slope + 1));
					}
				}
				if (d1 < d2) {
					tangentX = (float) x1t;
					tangentY = (float) y1t;
				} else {
					tangentX = (float) x2t;
					tangentY = (float) y2t;
				}
				if (tangentY == 0.0f) {
					slope = Double.POSITIVE_INFINITY;

				} else {
					slope = -tangentX / tangentY;
				}
			} // else sidePointsForTangent > 1
			if (doInner) {
				if (Double.isInfinite(slope)) {
					if (srcContour.contains(xPoints[i], yPoints[i] + innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] + innerDistance, slice);
						innerV.add(point);
					} else if (srcContour.contains(xPoints[i], yPoints[i] - innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] - innerDistance, slice);
						innerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = innerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (srcContour.contains((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist), slice);
						innerV.add(point);
					} else if (srcContour.contains((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist), slice);
						innerV.add(point);
					}
				}
			} // if (doInner)

		} // for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent +
			// nPoints - 1; i++, j++)

		short sID = (short) (targetImageSlice.getVOIs().getUniqueID());
		String kName = srcContour.getClass().getName();
		index = kName.lastIndexOf('.') + 1;
		kName = kName.substring(index);
		VOI resultVOIInner = new VOI(sID, kName + "_inner" + sID, srcContour.getType(), -1);

		if (doInner) {
			Vector3f pt[] = new Vector3f[innerV.size()];
			for (i = 0; i < innerV.size(); i++) {
				pt[i] = innerV.elementAt(i);
			}
			resultVOIInner.importCurve(pt);
		}

		// targetImageSlice.getVOIs().removeElementAt(0);

		targetImageSlice.getVOIs().removeAllElements();

		targetImageSlice.registerVOI(resultVOIInner);
		condenseVOI(targetImageSlice, 0);

		smoothVOI60Single(targetImageSlice, targetImageSlice);
	}

	public ModelImage runCED(ModelImage targetImage) {
		int numIterations;
		float diffusitivityDenom;
		float derivativeScale;
		float gaussianScale;
		boolean do25D;
		boolean entireImage;

		numIterations = 10;
		diffusitivityDenom = 0.001f;
		derivativeScale = 0.5f;
		gaussianScale = 2.0f;
		do25D = true;
		entireImage = true;

		ModelImage cedImage = (ModelImage) targetImage.clone();
		cedImage.setType(ModelStorageBase.FLOAT);
		cedImage.reallocate(ModelStorageBase.FLOAT);

		AlgorithmCoherenceEnhancingDiffusion coherenceEnhancingDiffusionAlgo = new AlgorithmCoherenceEnhancingDiffusion(cedImage, targetImage, numIterations,
				diffusitivityDenom, derivativeScale, gaussianScale, do25D, entireImage);
		coherenceEnhancingDiffusionAlgo.addListener(this);
		coherenceEnhancingDiffusionAlgo.run();
		coherenceEnhancingDiffusionAlgo.setCompleted(true);
		coherenceEnhancingDiffusionAlgo.finalize();
		coherenceEnhancingDiffusionAlgo = null;

		return cedImage;
	}

	public void createTwoContours(ModelImage targetImageSlice) {
		// smoothVOI60(targetImageSlice, targetImageSlice);
		// smoothVOI60DualContour(targetImageSlice, targetImageSlice);
		VOI voiNew = targetImageSlice.getVOIs().elementAt(0);

		VOIBaseVector curves = voiNew.getCurves();
		VOIBase srcContour = null;
		for (int index = 0; index < curves.size(); index++) {
			srcContour = curves.elementAt(index);
			generateBoundaryContours(srcContour, targetImageSlice);

		}
	}

	private void generateBoundaryContours(VOIBase srcContour, ModelImage targetImageSlice) {
		int sidePointsForTangent = 1;
		int innerDistance = 20;
		int outerDistance = 5;
		boolean doInner = true;
		boolean doOuter = true;
		int index;
		int i;
		int j;
		int k;
		int m;
		if (srcContour == null || (srcContour.size() == 0)) {
			return;
		}
		int slice = (int) srcContour.elementAt(0).Z;

		int nPoints = srcContour.size();
		Vector3f point;
		float xPoints[] = new float[nPoints + 2 * sidePointsForTangent];
		float yPoints[] = new float[nPoints + 2 * sidePointsForTangent];
		Vector<Vector3f> innerV = new Vector<Vector3f>();
		Vector<Vector3f> outerV = new Vector<Vector3f>();
		float tangentX;
		float tangentY;
		float xCenteredPoints[] = new float[2 * sidePointsForTangent + 1];
		float yCenteredPoints[] = new float[2 * sidePointsForTangent + 1];
		double xSqSum;
		double ySqSum;
		double xySum;
		double var;
		double x1t;
		double x2t;
		double y1t;
		double y2t;
		double slope;
		double d1;
		double d2;
		double xDist;
		double yDist;
		for (i = 0; i < nPoints; i++) {
			point = (srcContour.get(i));
			xPoints[i + sidePointsForTangent] = point.X;
			yPoints[i + sidePointsForTangent] = point.Y;
		}
		for (i = sidePointsForTangent - 1, j = 0; i >= 0; i--, j++) {
			xPoints[i] = xPoints[nPoints - 1 - j];
			yPoints[i] = yPoints[nPoints - 1 - j];
		}
		for (i = nPoints, j = 0; i <= nPoints + sidePointsForTangent - 1; i++, j++) {
			xPoints[i] = xPoints[j];
			yPoints[i] = yPoints[j];
		}
		for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent + nPoints - 1; i++, j++) {
			if (sidePointsForTangent == 1) {
				tangentX = (xPoints[i + 1] - xPoints[i - 1]) / 2.0f;
				tangentY = (yPoints[i + 1] - yPoints[i - 1]) / 2.0f;
				if (tangentY == 0.0f) {
					slope = Double.POSITIVE_INFINITY;
				} else {
					slope = -tangentX / tangentY;
				}
			} // if (sidePointsForTangent == 1)
			else { // sidePointsForTangent > 1
					// Center all points for tangent point touching curve at (0,
					// 0)
					// That is, use an x axis and a y axis going thru the
					// tangent point
				for (k = 0, m = i - sidePointsForTangent; m <= i + sidePointsForTangent; m++, k++) {
					xCenteredPoints[k] = xPoints[m] - xPoints[i];
					yCenteredPoints[k] = yPoints[m] - yPoints[i];
				}
				xSqSum = 0.0;
				ySqSum = 0.0;
				xySum = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					xSqSum += xCenteredPoints[k] * xCenteredPoints[k];
					ySqSum += yCenteredPoints[k] * yCenteredPoints[k];
					xySum += xCenteredPoints[k] * yCenteredPoints[k];
				}
				if (xySum != 0.0) {
					var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
					x1t = 0.5 * ((-ySqSum + xSqSum + var) / xySum);
					x2t = 0.5 * ((-ySqSum + xSqSum - var) / xySum);
					y1t = 1.0;
					y2t = 1.0;
				} else {
					// If all points are symmetric to either this new x axis or
					// this new y axis, then
					// their product sum is 0 and the tangentX, tangentY must be
					// 1,0 or 0,1
					x1t = 1.0;
					x2t = 0.0;
					y1t = 0.0;
					y2t = 1.0;
				}
				// x1t, y1t and x2t, y2t are perpindicular. To find the
				// solution, calculate the sum of
				// distances from the curve points to the line for the 2 cases
				// The shortest distance is the correct solution
				// Distance from AX + BY + C = 0 to P1 is
				// abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
				// Here A = slope, B = -1, and C = 0.
				d1 = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					if (x1t == 0.0) {
						// Infinite slope thru (0,0)
						d1 += Math.abs(yCenteredPoints[k]);
					} else if (y1t == 0.0) {
						// Zero slope thru (0, 0)
						d1 += Math.abs(xCenteredPoints[k]);
					} else {
						slope = y1t / x1t;
						d1 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k]) / Math.sqrt(slope * slope + 1));
					}
				}
				d2 = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					if (x2t == 0.0) {
						// Infinite slope thru (0,0)
						d2 += Math.abs(yCenteredPoints[k]);
					} else if (y2t == 0.0) {
						// Zero slope thru (0, 0)
						d2 += Math.abs(xCenteredPoints[k]);
					} else {
						slope = y2t / x2t;
						d2 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k]) / Math.sqrt(slope * slope + 1));
					}
				}
				if (d1 < d2) {
					tangentX = (float) x1t;
					tangentY = (float) y1t;
				} else {
					tangentX = (float) x2t;
					tangentY = (float) y2t;
				}
				if (tangentY == 0.0f) {
					slope = Double.POSITIVE_INFINITY;

				} else {
					slope = -tangentX / tangentY;
				}
			} // else sidePointsForTangent > 1
			if (doInner) {
				if (Double.isInfinite(slope)) {
					if (srcContour.contains(xPoints[i], yPoints[i] + innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] + innerDistance, slice);
						innerV.add(point);
					} else if (srcContour.contains(xPoints[i], yPoints[i] - innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] - innerDistance, slice);
						innerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = innerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (srcContour.contains((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist), slice);
						innerV.add(point);
					} else if (srcContour.contains((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist), slice);
						innerV.add(point);
					}
				}
			} // if (doInner)
			if (doOuter) {
				if (Double.isInfinite(slope)) {
					if (!srcContour.contains(xPoints[i], yPoints[i] + outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] + outerDistance, slice);
						outerV.add(point);
					} else if (!srcContour.contains(xPoints[i], yPoints[i] - outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] - outerDistance, slice);
						outerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = outerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (!srcContour.contains((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist), slice);
						outerV.add(point);
					} else if (!srcContour.contains((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist), slice);
						outerV.add(point);
					}
				}
			} // if (doOuter)
		} // for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent +
			// nPoints - 1; i++, j++)

		short sID = (short) (targetImageSlice.getVOIs().getUniqueID());
		String kName = srcContour.getClass().getName();
		index = kName.lastIndexOf('.') + 1;
		kName = kName.substring(index);
		VOI resultVOIInner = new VOI(sID, kName + "_inner" + sID, srcContour.getType(), -1);
		VOI resultVOIOuter = new VOI(sID, kName + "_outer" + sID, srcContour.getType(), -1);

		if (doInner) {
			Vector3f pt[] = new Vector3f[innerV.size()];
			for (i = 0; i < innerV.size(); i++) {
				pt[i] = innerV.elementAt(i);
			}
			resultVOIInner.importCurve(pt);
		}
		if (doOuter) {
			Vector3f pt[] = new Vector3f[outerV.size()];
			for (i = 0; i < outerV.size(); i++) {
				pt[i] = outerV.elementAt(i);
			}
			resultVOIOuter.importCurve(pt);
		}

		// targetImageSlice.getVOIs().removeElementAt(0);

		targetImageSlice.getVOIs().removeAllElements();

		targetImageSlice.registerVOI(resultVOIInner);
		targetImageSlice.registerVOI(resultVOIOuter);

		condenseVOI(targetImageSlice, 0);
		condenseVOI(targetImageSlice, 1);

		smoothVOI60DualContour(targetImageSlice, targetImageSlice);

	}

	public void condenseVOI(ModelImage targetImageSlice, int voiNumber) {
		float stepPct = (float) 20.0;
		VOIVector v = targetImageSlice.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(voiNumber).setActive(true);
		v.VOIAt(voiNumber).setAllActive(true);

		Vector<VOIBase> contours = v.VOIAt(voiNumber).getCurves();
		int nContours = contours.size();
		for (int elementNum = 0; elementNum < nContours; elementNum++) {
			VOIBase voiLine = contours.elementAt(elementNum);
			int nPts = voiLine.size();
			Vector<Vector3f> innerV = new Vector<Vector3f>();
			for (int i = 0; i < nPts; i++) {

				Vector3f startPt;
				Vector3f midPt;
				Vector3f endPt;

				if (i == 0) {
					startPt = voiLine.get(nPts - 1);
					midPt = voiLine.get(0);
					endPt = voiLine.get(1);
				} else if (i == nPts - 1) {
					startPt = voiLine.get(nPts - 2);
					midPt = voiLine.get(i);
					endPt = voiLine.get(0);
				} else {
					startPt = voiLine.get(i - 1);
					midPt = voiLine.get(i);
					endPt = voiLine.get(i + 1);
				}
				Vector3f interpPt = new Vector3f();
				Vector3f inNormPt = new Vector3f();
				Vector3f outNormPt = new Vector3f();
				Vector3f normStep = new Vector3f();

				float[] xMid = new float[1];
				float[] yMid = new float[1];

				computeNormalLine(startPt, endPt, midPt, outNormPt, inNormPt, stepPct, normStep, interpPt);
				findBestGradientChange(interpPt, normStep, 0, xMid, yMid, targetImageSlice);

				// System.err.println("xMid[0] = " + xMid[0] + "  yMid[0] = " +
				// yMid[0]);

				// voiLine.get(i).set(xMid[0], yMid[0], startPt.Z);
				innerV.add(new Vector3f(xMid[0], yMid[0], startPt.Z));

			} // end i < nPts loop

			Vector3f pt[] = new Vector3f[nPts];
			for (int i = 0; i < nPts; i++) {
				pt[i] = innerV.elementAt(i);
			}
			v.VOIAt(voiNumber).removeCurves();
			v.VOIAt(voiNumber).importCurve(pt);

		}

	}

	private void findBestGradientChange(Vector3f interpPt, Vector3f normStep, float sliceZ, float[] x, float[] y, ModelImage targetImageSlice) {

		float centerX, centerY;
		centerX = interpPt.X;
		centerY = interpPt.Y;

		double[] result = new double[2];
		double[] grad = new double[1];
		double[] gradDir = new double[1];

		double max_change = Double.MIN_VALUE;
		double change;

		float bestX = centerX, bestY = centerY;
		float stepX = normStep.X / 20f;
		float stepY = normStep.Y / 20f;

		// centerX = centerX - normStep.X / 20f * 3;
		// centerY = centerY - normStep.Y / 20f * 3;

		for (int i = 0; i < 10; i++) {
			computeGradient(centerX, centerY, sliceZ, result, grad, gradDir, targetImageSlice);
			change = grad[0];
			if (change > max_change) {
				max_change = change;
				bestX = centerX;
				bestY = centerY;
			}
			centerX -= stepX;
			centerY -= stepY;
		}

		x[0] = bestX;
		y[0] = bestY;
	}

	private void computeGradient(float xCoord, float yCoord, float zCoord, double[] result, double[] grad, double[] gradDir, ModelImage targetImageSlice) {

		double gradientXsrc = 0, gradientYsrc = 0;

		int x = (int) xCoord;
		int y = (int) yCoord;
		int z = (int) zCoord;

		int[][] GX = new int[3][3];
		int[][] GY = new int[3][3];

		// 3x3 GX Sobel mask.
		GX[0][0] = -1;
		GX[0][1] = 0;
		GX[0][2] = 1;
		GX[1][0] = -2;
		GX[1][1] = 0;
		GX[1][2] = 2;
		GX[2][0] = -1;
		GX[2][1] = 0;
		GX[2][2] = 1;

		// 3x3 GY Sobel mask.
		GY[0][0] = 1;
		GY[0][1] = 2;
		GY[0][2] = 1;
		GY[1][0] = 0;
		GY[1][1] = 0;
		GY[1][2] = 0;
		GY[2][0] = -1;
		GY[2][1] = -2;
		GY[2][2] = -1;

		for (int II = -1; II <= 1; II++) {
			for (int JJ = -1; JJ <= 1; JJ++) {

				gradientXsrc = gradientXsrc + targetImageSlice.get(x + II, y + JJ).floatValue() * GX[II + 1][JJ + 1];
				gradientYsrc = gradientYsrc + targetImageSlice.get(x + II, y + JJ).floatValue() * GY[II + 1][JJ + 1];

			}
		}

		grad[0] = Math.sqrt(gradientXsrc * gradientXsrc + gradientYsrc * gradientYsrc);
		// System.err.println("grad[0] = " + grad[0]);
		gradDir[0] = Math.atan2(gradientYsrc, gradientXsrc);
		result[0] = gradientXsrc;
		result[1] = gradientYsrc;
	}

	private void computeNormalLine(Vector3f startPt, Vector3f endPt, Vector3f midPt, Vector3f outNormPt, Vector3f inNormPt, float stepPct, Vector3f normStep,
			Vector3f interpPt) {
		float normLength;
		Vector3f tangentDir = new Vector3f();
		Vector3f normDir = new Vector3f();

		tangentDir.X = (midPt.X - startPt.X + endPt.X - midPt.X) / 2f;
		tangentDir.Y = (midPt.Y - startPt.Y + endPt.Y - midPt.Y) / 2f;

		normLength = (float) Math.sqrt((tangentDir.X * tangentDir.X) + (tangentDir.Y * tangentDir.Y));
		normDir.X = -tangentDir.Y / normLength;
		normDir.Y = tangentDir.X / normLength;

		interpPt.X = midPt.X;
		interpPt.Y = midPt.Y;

		normStep.X = stepPct * normDir.X;
		normStep.Y = stepPct * normDir.Y;

		outNormPt.X = -normStep.X + interpPt.X;
		outNormPt.Y = -normStep.Y + interpPt.Y;

		inNormPt.X = normStep.X + interpPt.X;
		inNormPt.Y = normStep.Y + interpPt.Y;
	}

	public void smoothVOI60(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 60, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(1).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	public void smoothVOI60Single(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 60, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			// VOIVector vois = new VOIVector();
			// vois.add(smoothAlgo.getResultVOI());
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			// resultImage.notifyImageDisplayListeners(null, true);
			// resultImage.getVOIs().removeAllElements();
			// resultImage.addVOIs(vois);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	public void smoothVOI150Single(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 150, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	public void smoothVOI120Single(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 120, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			// VOIVector vois = new VOIVector();
			// vois.add(smoothAlgo.getResultVOI());
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			// resultImage.notifyImageDisplayListeners(null, true);
			// resultImage.getVOIs().removeAllElements();
			// resultImage.addVOIs(vois);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	public void smoothVOI60DualContour(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		v.VOIAt(1).setActive(true);
		v.VOIAt(1).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 64, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;

			smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(1), 64, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			resultVOIs = resultImage.getVOIs();
			resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(1).removeCurves();
			resultVOIs.VOIAt(1).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	public void smoothVOI60Special(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();

		if (v.size() <= 2)
			return;
		v.VOIAt(2).setActive(true);
		v.VOIAt(2).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(2), 60, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(2).removeCurves();
			resultVOIs.VOIAt(2).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	/**
	 * After auto segmentation, update the target image with corresponding
	 * segmented VOIs.
	 * 
	 * @param sliceNumber
	 *            slice number.
	 * @param targetImageSlice
	 *            2D target image slice.
	 */
	public void updateTargetImage(int sliceNumber, ModelImage targetImageSlice) {

		VOIVector voiVectorNew = new VOIVector();
		VOIVector VOIs = targetImageSlice.getVOIs();
		VOI voiNew = new VOI((short) 0, "slice" + sliceNumber);

		// two contours finding
		// if (VOIs.size() <= 2) {
		// System.err.println("not finding VOI");
		// return;
		// }

		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, 1);

		// two contours finding
		// Vector<VOIBase>[] vArray = VOIs.VOIAt(2).getSortedCurves(
		// VOIBase.ZPLANE, 1);

		// System.err.println("vArray length = " + vArray.length);
		VOIBase v = vArray[0].get(0);
		// VOI totalVOI = v.getGroup();
		if (v != null) {
			VOIBase vTemp = (VOIBase) v.clone();

			int nPts = vTemp.size();

			// zero out the z dimension VOI
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];
			float[] zPtsZero = new float[nPts];

			for (int u = 0; u < nPts; u++) {
				zPtsZero[u] = sliceNumber;
			}

			vTemp.exportArrays(xPts, yPts, zPts);
			vTemp.importArrays(xPts, yPts, zPtsZero, nPts - 2);
			voiNew.importCurve(vTemp);
		}

		voiVectorNew.add(voiNew);
		fatImage.addVOIs(voiVectorNew);
	}

	/**
	 * Pauses the display until the user hits enter.
	 */
	public static void pause() {
		System.err.println("enter to continue: ");
		try {
			// eat any pending characters
			for (int av = System.in.available(); av > 0; av--) {
				System.in.read();
			}
			System.in.read();// wait for user to hit Enter, discard result
		} catch (IOException e) {
			System.err.println("keyboard failed: " + e);
		}

	}

	/**
	 * read target image.
	 */
	public void readFATImagesAndVOIs() {
		FileIO fatImageIO = null;

		try {
			// read target image
			fatImageIO = new FileIO();
			fatImage = fatImageIO.readImage(fatImageName, fatImageDirectory);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * File chooser to select target image directory.
	 */
	private void readFATImage() {
		fatImageChooser.setDialogTitle("Open Target Image");

		if (UI.getDefaultDirectory() != null) {
			final File file = new File(UI.getDefaultDirectory());

			if (file != null) {
				fatImageChooser.setCurrentDirectory(file);
			}
		} else {
			fatImageChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
		}

		fatImageChooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", ".XML" }));

		final int returnValue = fatImageChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			fatImageName = fatImageChooser.getSelectedFile().getName();
			fatImageDirectory = String.valueOf(fatImageChooser.getCurrentDirectory()) + File.separatorChar;
			textFieldImageFAT.setText(fatImageName);

			readFATImagesAndVOIs();

		} else {
			return;
		}

	}

	/**
	 * Read the ending slice index to guide the segmentation towards apex and
	 * base.
	 */
	private void readEndingSlicesDir() {
		String endSliceModelDir = "C:/endSlices/Slice12" + File.separator;
		processingEndSliceData(endSliceModelDir);
	}

	/**
	 * Process the ending slice atlas directory.
	 * 
	 * @param endSliceModelDir
	 *            atlas directory.
	 */
	private void processingEndSliceData(String endSliceModelDir) {
		File dir = new File(endSliceModelDir);
		String[] children = dir.list();
		int len = children.length;
		for (int i = 0; i < len; i++) {
			File file = new File(dir.getAbsoluteFile(), children[i]);
			if (children[i].startsWith("image") && children[i].endsWith(".xml")) {
				endSliceImageNames.add(dir.getAbsolutePath() + File.separator + children[i]);

			} else if (children[i].startsWith("end") && children[i].endsWith(".txt")) {
				endSliceRangeNames.add(dir.getAbsolutePath() + File.separator + children[i]);
				// System.err.println(dir.getAbsolutePath() + File.separator +
				// children[i]);
			}
		}
	}

	public void disposeLocal() {

	}

	private void computeGradient(ModelImage image, float xCoord, float yCoord, float zCoord, double[] result, double[] grad, double[] gradDir) {

		double gradientXsrc = 0, gradientYsrc = 0;

		int x = (int) xCoord;
		int y = (int) yCoord;
		int z = (int) zCoord;

		int[][] GX = new int[3][3];
		int[][] GY = new int[3][3];

		// 3x3 GX Sobel mask.
		GX[0][0] = -1;
		GX[0][1] = 0;
		GX[0][2] = 1;
		GX[1][0] = -2;
		GX[1][1] = 0;
		GX[1][2] = 2;
		GX[2][0] = -1;
		GX[2][1] = 0;
		GX[2][2] = 1;

		// 3x3 GY Sobel mask.
		GY[0][0] = 1;
		GY[0][1] = 2;
		GY[0][2] = 1;
		GY[1][0] = 0;
		GY[1][1] = 0;
		GY[1][2] = 0;
		GY[2][0] = -1;
		GY[2][1] = -2;
		GY[2][2] = -1;

		for (int II = -1; II <= 1; II++) {
			for (int JJ = -1; JJ <= 1; JJ++) {

				gradientXsrc = gradientXsrc + image.get(x + II, y + JJ, z).floatValue() * GX[II + 1][JJ + 1];
				gradientYsrc = gradientYsrc + image.get(x + II, y + JJ, z).floatValue() * GY[II + 1][JJ + 1];

			}
		}

		grad[0] = Math.sqrt(gradientXsrc * gradientXsrc + gradientYsrc * gradientYsrc);
		gradDir[0] = Math.atan2(gradientYsrc, gradientXsrc);
		result[0] = gradientXsrc;
		result[1] = gradientYsrc;
	}

	private void displaySlicesRadical(ModelImage fuzzyCImage, ModelImage class1Image, ModelImage greImageSlice, ModelImage gaussianImageSlice, int sliceNumber,
			Vector3f pos) {

		VOI resultVOI = new VOI((short) 0, "result-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(resultVOI);

		int nPtsCurrent = 120;

		float posX, posY;
		int i;

		// VOI lineVOI = new VOI( (short)greImageSlice.getVOIs().size(), "line",
		// VOI.LINE, 0 );
		// lineVOI.setColor(Color.green);
		// VOIVector voiVectorLines = new VOIVector();
		// voiVectorLines.add(lineVOI);

		// Vector3f pt[] = new Vector3f[nPtsCurrent];

		Hashtable<Integer, Line> linePoints = slicesPts.get(sliceNumber);

		VOI resultVOIInner = new VOI((short) 0, "result-VOI" + "_inner", VOI.CONTOUR, -1);

		Vector3f pt_inner[] = new Vector3f[nPtsCurrent];

		for (i = 0; i < nPtsCurrent; i++) {

			Line line = linePoints.get(i);

			posX = line.endX;
			posY = line.endY;

			pt_inner[i] = new Vector3f(posX, posY, 0f);

		} // end i loop;

		resultVOIInner.importCurve(pt_inner);

		greImageSlice.getVOIs().removeAllElements();

		greImageSlice.registerVOI(resultVOIInner);

		// new ViewJFrameImage(greImageSlice);

		smoothVOI120Single(greImageSlice, greImageSlice);
		// transform the VOI from 2D slice to 3D greimage
		VOIBaseVector current_va = greImageSlice.getVOIs().VOIAt(0).getCurves();
		if (current_va.size() > 0) {
			VOIBase current_v = current_va.get(0);

			VOIBase vTemp = (VOIBase) current_v.clone();

			// int nPtsCurrent = current_v.size();
			float[] xPtsCurrent = new float[nPtsCurrent];
			float[] yPtsCurrent = new float[nPtsCurrent];
			float[] zPtsCurrent = new float[nPtsCurrent];
			float[] zSlice = new float[nPtsCurrent];
			current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);

			/*
			 * Vector3f[] BsplinePoints = new Vector3f[nPtsCurrent]; for (int w
			 * = 0; w < nPtsCurrent; w++) { BsplinePoints[w] = new
			 * Vector3f(xPtsCurrent[w], yPtsCurrent[w], 0); }
			 * 
			 * BSplineCurve3f curve = BSplineCurve3f.CreateApproximation(
			 * BsplinePoints, nPtsCurrent - 5, 2); float minTime =
			 * curve.GetMinTime(); float maxTime = curve.GetMaxTime(); float
			 * step = (maxTime - minTime) / 90f;
			 * 
			 * Vector<Point> extractedPoints = new Vector<Point>();
			 * 
			 * for (float t = minTime; t <= maxTime; t += step) { Vector3f ptr =
			 * curve.GetPosition(t); extractedPoints.add(new Point((int) ptr.X,
			 * (int) ptr.Y)); }
			 * 
			 * int nPts = extractedPoints.size();
			 * 
			 * float[] zSlice = new float[nPts];
			 * 
			 * for (int j = 0; j < nPts; j++) { Point p =
			 * extractedPoints.get(j); xPtsCurrent[j] = p.x; yPtsCurrent[j] =
			 * p.y; zSlice[j] = sliceNumber; }
			 */

			for (int j = 0; j < nPtsCurrent; j++) {
				zSlice[j] = sliceNumber;
			}

			vTemp.importArrays(xPtsCurrent, yPtsCurrent, zSlice, nPtsCurrent);
			voiNewFinal.importCurve(vTemp);
			vTemp = null;
		}

	}

	private Vector3f displaySlices(ModelImage fuzzyCImage, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image, ModelImage greImageSlice,
			ModelImage gaussianImageSlice, int sliceNumber) {

		VOI resultVOI = new VOI((short) 0, "result-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(resultVOI);

		int nPtsCurrent = 120;

		float centerX, centerY;
		float posX, posY;
		int i;

		VOI lineVOI = new VOI((short) greImageSlice.getVOIs().size(), "line", VOI.LINE, 0);
		lineVOI.setColor(Color.green);
		VOIVector voiVectorLines = new VOIVector();
		voiVectorLines.add(lineVOI);

		Vector3f pt[] = new Vector3f[nPtsCurrent];

		Hashtable<Integer, Line> linePoints = slicesPts.get(sliceNumber);

		for (i = 0; i < nPtsCurrent; i++) {

			Line line = linePoints.get(i);

			centerX = line.startX;
			centerY = line.startY;

			posX = line.endX;
			posY = line.endY;

			// final line
			pt[i] = new Vector3f(posX, posY, 0f);

			// add the green tracing lines
			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(centerX, centerY, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));
			lineVOI.importCurve(kLine);

		} // end i loop;

		resultVOI.importCurve(pt);

		greImageSlice.addVOIs(voiVectorLines);
		gaussianImageSlice.addVOIs(voiVectorLines);
		fuzzyCImage.addVOIs(voiVectorLines);
		class1Image.addVOIs(voiVectorLines);
		class2Image.addVOIs(voiVectorLines);
		class3Image.addVOIs(voiVectorLines);

		// new ViewJFrameImage(fuzzyCImage);
		// new ViewJFrameImage(class1Image);
		// new ViewJFrameImage(class2Image);
		// new ViewJFrameImage(class3Image);
		// new ViewJFrameImage(gaussianImageSlice);
		// new ViewJFrameImage(greImageSlice);

		return resultVOI.getGeometricCenter();
	}

	private Vector2f traceSection6(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber, int currentStep,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image, ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
		while ( ( fuzzyCIntensity != 1 || fuzzyCIntensity != 2 || fuzzyCIntensity != 3 ) && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 10;
				break;
			}
			if (posY <= 0) {
				posY = 10;
				break;
			}

            // handle close to end slices
			if ((sliceNumber <= startPt + tenPercent) || (sliceNumber >= endPt - tenPercent)) {
				
				while ( distCurrent < distOuter ) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 8, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ( ( fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) && distCurrent < distOuter ) {
								posX += stepX;
								posY += stepY;
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}
					
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 )
						break;
					
					posX += stepX;
					posY += stepY;
					currentStep++;
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			// arbitrary testing for fuzzyC
			
			// int ct = 0;
			
		   	
			// walk around holes
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 10, 0, FuzzyC);
				if (findHoles) {
					// skip holes
					while ( ( fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) && distCurrent < distOuter ) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
			}
			
           			
			boolean findWeakEdge = false;
			boolean findWeakClass3 = false;
			boolean findWeakClass2 = false;
			float testX = posX, testY = posY;
			boolean lookFor3 = false;
			
			int []xResult = new int[1];
			xResult[0] = -1;
			findWeakEdge = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 5, FuzzyC, false, xResult);
			findWeakClass3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class3Image, 5, Class3, false, xResult);
			findWeakClass2 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class2Image, 5, Class2, false, xResult);
			
			if (findWeakEdge || findWeakClass3 || findWeakClass2) {
				testX = posX; testY = posY;
				lookFor3 = false;
			
				while (distCurrent < distOuter) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3  ) {
						lookFor3 = true;
						break;
					}
					testX += stepX;
					testY += stepY;
					distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
				}

				if (lookFor3) {
					posX = testX;
					posY = testY;
					break;
				}
				 
				break;
			} /* else {
				findWeakEdge = false;
				findWeakClass3 = false;
				findWeakClass2 = false;
				xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, false, xResult);
				findWeakClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 3, Class3, false, xResult);
				findWeakClass2 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class2Image, 3, Class2, false, xResult);
				
				if (findWeakEdge || findWeakClass3 || findWeakClass2) {
					testX = posX; testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3  ) {
							lookFor3 = true;
							break;
						}
						testX += stepX;
						testY += stepY;
						distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
					}
					if (lookFor3) {
						posX = testX;
						posY = testY;
						break;
					}
					break;
				}
			}  */ 
            
			// arbitrary testing for fuzzyC

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		    if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
				break;
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.01f || class1Intensity > 0.5f  || class3Intensity >= 0.01f ) {
				break;
			}
		    /*
			// arbitrary testing on class 1 and class 2
 			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
 			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
 			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
 			if (class2Intensity >= 0.1f || class1Intensity < 0.1f  || class3Intensity >= 0.02f ) {
 				testX = posX; testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter ) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if ( degree >= section4_degree_start && degree <= section4_degree_start + 20 ) {
						if (fuzzyCIntensity == 3  ) {
							lookFor3 = true;
							break;
						}	
					} else {
						if (fuzzyCIntensity == 3  || fuzzyCIntensity == 2 || class3Intensity >= 0.02f) {
							lookFor3 = true;
							break;
						}
					}
					testX += stepX;
					testY += stepY;
					distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
				}

				if (lookFor3) {
					posX = testX;
					posY = testY;
					break;
				}
 				 
 				break;
 			}
			*/			
			
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop
		
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection1(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber, int currentStep,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image, ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;
		
		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
		while ( ( fuzzyCIntensity != 1 || fuzzyCIntensity != 2 || fuzzyCIntensity != 3 ) && distCurrent < distOuter) {
		
			if (posX <= 0) {
				posX = 10;
				break;
			}
			if (posY <= 0) {
				posY = 10;
				break;
			}
			
			if ((sliceNumber <= startPt + tenPercent) || (sliceNumber >= endPt - tenPercent)) {
				while ( distCurrent < distOuter ) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 8, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ( ( fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) && distCurrent < distOuter ) {
								posX += stepX;
								posY += stepY;
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}
					
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3  )
						break;
					posX += stepX;
					posY += stepY;
					currentStep++;
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			// walk around holes
			if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 10, 0, FuzzyC);
				if (findHoles) { // skip holes
					while ( ( fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter ) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
			}
           

		
			float testX = posX, testY = posY;
			boolean lookFor3 = false;
			// if ( degree > section1_degree_end - 15 ) {
				boolean findWeakEdge = false;
				boolean findWeakEdgeClass2 = false;
				boolean findWeakEdgeClass3 = false;
				int []xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 5, FuzzyC_ALL, false, xResult);
				findWeakEdgeClass2 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class2Image, 5, Class2, false, xResult);
				findWeakEdgeClass3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class3Image, 5, Class3, false, xResult);
				if (findWeakEdge || findWeakEdgeClass2 || findWeakEdgeClass3) {
                     
					testX = posX; testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class1Intensity = class1Image.getDouble((int) testX, (int) testY);
						class2Intensity = class2Image.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 1 || fuzzyCIntensity == 3  || fuzzyCIntensity == 2 ||
								class2Intensity >= 0.04f || class1Intensity > 0.5f  || class3Intensity >= 0.01f) {
							lookFor3 = true;
							break;
						}
						testX += stepX;
						testY += stepY;
						distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
					}

					if (lookFor3) {
						posX = testX;
						posY = testY;
						break;
					}
					
					 
					break;
				}
			// }
			
				
				
			
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 1 ||fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
				break;

			// arbitrary testing on class 1 and class 2
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.01f || class1Intensity > 0.5f  || class3Intensity >= 0.01f ) {
				break;
			}
			
			currentStep++;
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			
		} // end while loop
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection2(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber, int currentStep,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image, ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
		while ((fuzzyCIntensity != 1 || fuzzyCIntensity != 2 || fuzzyCIntensity != 3)  && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 10;
				break;
			}
			if (posY <= 0) {
				posY = 10;
				break;
			}
			
			if ((sliceNumber <= startPt + tenPercent) || (sliceNumber >= endPt - tenPercent)) {
				while ( distCurrent < distOuter ) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 8, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ( ( fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) && distCurrent < distOuter ) {
								posX += stepX;
								posY += stepY;
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}
					
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 )
						break;
					posX += stepX;
					posY += stepY;
					currentStep++;
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			// walk around holes
			if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 10, 0, FuzzyC);
				if (findHoles) { // skip holes
					while (( fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
			}
			
			// arbitrary testing on class 1 and class 2
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			if (class2Intensity >= 0.01f || class1Intensity > 0.5f || class3Intensity > 0.01f ) {
				break;
			}

			
			
			// if (degree > degree > section2_degree_start + 15) {
			boolean findWeakEdge = false;
			boolean findWeakEdgeClass2 = false;
			boolean findWeakEdgeClass3 = false;
			int []xResult = new int[1];
			xResult[0] = -1;
			findWeakEdge = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 5, FuzzyC_ALL, true, xResult);
			findWeakEdgeClass2 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class2Image, 5, Class2, false, xResult);
			findWeakEdgeClass3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class3Image, 5, Class3, false, xResult);
			
			if (findWeakEdge || findWeakEdgeClass2 || findWeakEdgeClass3) {
				float testX = posX;
				float testY = posY;
				boolean lookFor3 = false;
				while (distCurrent < distOuter) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					class2Intensity = class2Image.getDouble((int) testX, (int) testY);
					class1Intensity = class1Image.getDouble((int) testX, (int) testY);
					// if ( degree >= section4_degree_start && degree <= section4_degree_start + 20 ) {
						if (fuzzyCIntensity == 1 || fuzzyCIntensity == 3  || fuzzyCIntensity == 2 || 
								class3Intensity >= 0.01f || class2Intensity >= 0.04f || class1Intensity >= 0.5f) {
							lookFor3 = true;
							break;
						}	
					// }
					testX += stepX;
					testY += stepY;
					distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
				}

				if (lookFor3) {
					posX = testX;
					posY = testY;
					break;
				}
				 
			
				break;
			}
			
			
		    
	   
			
			
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 1 ||fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
				break;

			// arbitrary testing on class 1 and class 2
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			if (class2Intensity >= 0.01f || class1Intensity > 0.5f || class3Intensity > 0.01f ) {
				break;
			}

			
			currentStep++;
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection3(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber, int currentStep,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image, ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
		while ( ( fuzzyCIntensity != 1 || fuzzyCIntensity != 2 ||  fuzzyCIntensity != 3) && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 10;
				break;
			}
			if (posY <= 0) {
				posY = 10;
				break;
			}

		
			if ((sliceNumber <= startPt + tenPercent) || (sliceNumber >= endPt - tenPercent)) {
				while ( distCurrent < distOuter ) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 8, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ( ( fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) && distCurrent < distOuter ) {
								posX += stepX;
								posY += stepY;
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}
					
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 )
						break;
					posX += stepX;
					posY += stepY;
					currentStep++;
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
	       
		    /*
			if ( degree >= (section3_degree_start + 15) && degree <= section3_degree_start + 40 ) {
				boolean findEdgeOnGRE = findEdgeOnImage((int) posX, (int) posY, greImageSlice, 5, 50, GRE_SECTION3_DARK);
				if (findEdgeOnGRE) {
				
					float testX = posX;
					float testY = posY;
					boolean lookFor3 = false;
					while (distCurrent < distOuter) {
						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class1Intensity = class1Image.getDouble((int) testX, (int) testY);
						class2Intensity = class2Image.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
	
						if (fuzzyCIntensity == 1 || fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class2Intensity >= 0.04f || class1Intensity >= 0.5f
								|| class3Intensity >= 0.01f) {
							lookFor3 = true;
							break;
						}
	
						testX += stepX;
						testY += stepY;
						distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
					}
	
					if (lookFor3) {
						posX = testX;
						posY = testY;
						break;
					}
	               
					break;
				}
			}
		    */ 
			
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			// walk around holes
			if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 10, 0, FuzzyC);
				if (findHoles) { // skip holes
					while ((fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
			}

		
			/*
			boolean findGreyRegionPattern = false;
		    findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 3, 0, GRE_GREY);
		    if ( findGreyRegionPattern ) {
		    	continue;
		    }
		    */ 
           
		    // boolean findBlackRegionPattern = false;
			// findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 3, 0, GRE_BLACK);

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 1 ||fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
				break;
	
				// arbitrary testing on class 1 and class 2
			// if (findBlackRegionPattern) {
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.01f || class1Intensity >= 0.5f || class3Intensity >= 0.01f) {
					break;
				}
			// }
	
			
			if (degree >= section3_degree_end - 30 && degree <= section3_degree_end) {
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.01f || class1Intensity >= 0.5f || class3Intensity >= 0.01f) {
					break;
				}
			}
			
			// looking for weak connected edge
			boolean findWeakEdgeClass1 = false;
			boolean findWeakEdgeFuzzyC = false;
			boolean findWeakEdgeClass3 = false;
			boolean findWeakEdgeClass2 = false;
			int []xResult = new int[1];
			xResult[0] = -1;
			
			

				if (degree >= (section3_degree_start + 15) && degree <= section3_degree_start + 40) {

					// findWeakEdgeClass1 = false;
					// findWeakEdgeClass1 =
					// findWeakConnectedEdgeOnImageHorizontal((int) posX, (int)
					// posY, class1Image, 6, Class1_weak, false, xResult);
					findWeakEdgeClass2 = false;
					findWeakEdgeClass2 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class2Image, 10, Class2, false, xResult);
					findWeakEdgeFuzzyC = false;
					findWeakEdgeFuzzyC = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 10, FuzzyC_ALL, false, xResult);
					findWeakEdgeClass3 = false;
					findWeakEdgeClass3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class3Image, 10, Class3, false, xResult);
					findWeakEdgeClass1 = false;
					findWeakEdgeClass1 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class1Image, 10, Class1, false, xResult);
					
					if ( findWeakEdgeClass1 ) break;
					
					if (findWeakEdgeFuzzyC || findWeakEdgeClass3 || findWeakEdgeClass2) {
						float testX = posX, testY = posY;
                        
						boolean lookFor3 = false;
						while (distCurrent < distOuter) {
							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							class2Intensity = class2Image.getDouble((int) testX, (int) testY);
							class1Intensity = class1Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 1 || fuzzyCIntensity == 3 ) {
								lookFor3 = true;
								break;
							}
							testX += stepX;
							testY += stepY;
							distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
						}

						if (lookFor3) {
							posX = testX;
							posY = testY;
							break;
						}
                       
						break;

					}
				}

				if (degree >= section3_degree_end - 30 && degree <= section3_degree_end) {

					findWeakEdgeClass2 = false;
					findWeakEdgeClass2 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class2Image, 10, Class2, false, xResult);
					findWeakEdgeFuzzyC = false;
					findWeakEdgeFuzzyC = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 10, FuzzyC_ALL, false, xResult);
					findWeakEdgeClass3 = false;
					findWeakEdgeClass3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class3Image, 10, Class3, false, xResult);
					findWeakEdgeClass1 = false;
					findWeakEdgeClass1 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class1Image, 10, Class1, false, xResult);
					
					if ( findWeakEdgeClass1 ) break;
					
					if (findWeakEdgeFuzzyC || findWeakEdgeClass3 || findWeakEdgeClass2) {
						float testX = posX, testY = posY;
                        
						boolean lookFor3 = false;
						while (distCurrent < distOuter) {
							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							class2Intensity = class2Image.getDouble((int) testX, (int) testY);
							class1Intensity = class1Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 1 || fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.01f || class2Intensity >= 0.04f
									|| class1Intensity > 0.5f) {
								lookFor3 = true;
								break;
							}
							testX += stepX;
							testY += stepY;
							distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
						}

						if (lookFor3) {
							posX = testX;
							posY = testY;
							break;
						}
					

						break;

					}
				}
		
			
			// arbitrary testing for fuzzyC
			// if (degree >= section3_degree_start && degree <= section3_degree_start + 20) {
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				if (fuzzyCIntensity == 1 ||fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
					break;

				// arbitrary testing on class 1 and class 2
			// if (findBlackRegionPattern) {
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.01f || class1Intensity >= 0.5f || class3Intensity >= 0.01f) {
					break;
				}
			// }

			
			if (degree >= section3_degree_end - 30 && degree <= section3_degree_end) {
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.01f || class1Intensity >= 0.5f || class3Intensity >= 0.01f) {
					break;
				}
			}
		
		
			currentStep++;
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
		} // end while loop
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection4(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber, int currentStep,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image, ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;
		double greIntensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		greIntensity = greImageSlice.getDouble((int) posX, (int) posY);

		while ((fuzzyCIntensity != 1 || fuzzyCIntensity != 2 || fuzzyCIntensity != 3) && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 10;
				break;
			}
			if (posY <= 0) {
				posY = 10;
				break;
			}

			if ((sliceNumber <= startPt + tenPercent) || (sliceNumber >= endPt - tenPercent)) {

				while (distCurrent < distOuter) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					greIntensity = greImageSlice.getDouble((int) posX, (int) posY);

					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 8, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
					
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
						break;
					// if ( greIntensity >= 3000 ) break;
					posX += stepX;
					posY += stepY;
					currentStep++;
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}
				break;
			}

		
			
			boolean findEdgeOnGREHighThresHold = findEdgeOnImage((int) posX, (int) posY, greImageSlice, 7, 50, GRE_SECTION4_HIGH_THRESHOLD);
			if (findEdgeOnGREHighThresHold) {
                
				float testX = posX;
				float testY = posY;
				boolean lookFor3 = false;
				while (distCurrent < distOuter) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class1Intensity = class1Image.getDouble((int) testX, (int) testY);
					class2Intensity = class2Image.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					greIntensity = greImageSlice.getDouble((int) testX, (int) testY);

					if (greIntensity >= 2900 || fuzzyCIntensity == 1 || fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class2Intensity >= 0.04f || class1Intensity >= 0.5f
							|| class3Intensity >= 0.01f) {
						lookFor3 = true;
						break;
					}

					testX += stepX;
					testY += stepY;
					distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
				}

				if (lookFor3) {
					posX = testX;
					posY = testY;
					break;
				}
                
				break;
			}
		     
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			// walk around holes
			if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 10, 0, FuzzyC);
				
				if (findHoles) {
					while ((fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					}
				}
			
			}
             
			
	        /*
			boolean findGreyRegionPattern = false;
		    findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 3, 0, GRE_GREY);
		    if ( findGreyRegionPattern ) {
		    	continue;
		    }
		    
		    boolean findBlackRegionPattern = false;
			findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 3, 0, GRE_BLACK);
			if ( findBlackRegionPattern ) {
		    	continue;
		    }
            */ 
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
				break;
			
			
			// if (findBlackRegionPattern) {
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.04f || class1Intensity >= 0.5f || class3Intensity >= 0.01f) {
					break;
				}
			// }
		    
			
			// looking for weak connected edge
			boolean findWeakEdge = false;
			boolean findWeakEdgeFuzzyC = false;
			boolean findWeakEdgeClass3 = false;
			boolean findWeakEdgeClass2 = false;
			boolean findWeakEdgeClass1 = false;
			int[] xResult = new int[1];
			xResult[0] = -1;
			float testX = posX, testY = posY;
			boolean lookFor3 = false;

		
			// if (findBlackRegionPattern) {
			
				findWeakEdgeFuzzyC = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 10, FuzzyC_ALL, true, xResult);
				findWeakEdgeClass3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class3Image, 10, Class3, true, xResult);
				findWeakEdgeClass2 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class2Image, 10, Class2, true, xResult);
				findWeakEdgeClass1 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class1Image, 10, Class1, true, xResult);
				greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
				
				if ( findWeakEdgeClass1 ) break;
				
				if (findWeakEdgeFuzzyC || findWeakEdgeClass3 || findWeakEdgeClass2 ) {
					
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						class2Intensity = class2Image.getDouble((int) testX, (int) testY);
						class1Intensity = class1Image.getDouble((int) testX, (int) testY);
						greIntensity = greImageSlice.getDouble((int) testX, (int) testY);
						
						if (greIntensity >= 2900 || fuzzyCIntensity == 1 || fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.01f || class2Intensity >= 0.04f
								|| class1Intensity >= 0.5f) {
							lookFor3 = true;
							break;
						}

						testX += stepX;
						testY += stepY;
						distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
					}

					if (lookFor3) {
						posX = testX;
						posY = testY;
						break;
					}
                    
					break;
				}
               
				// search section 4 on class 1 for regular edge
                
				/*
				boolean findEdgeOnGRE = findEdgeOnImage((int) posX, (int) posY, greImageSlice, 5, 50, GRE);
					if (findEdgeOnGRE ) {
				
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class1Intensity = class1Image.getDouble((int) testX, (int) testY);
						class2Intensity = class2Image.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						greIntensity = greImageSlice.getDouble((int) testX, (int) testY);
						if (greIntensity >= 2900 || fuzzyCIntensity == 1 || fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class2Intensity >= 0.04f || class1Intensity >= 0.5f
								|| class3Intensity >= 0.01f) {
							lookFor3 = true;
							break;
						}

						testX += stepX;
						testY += stepY;
						distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
					}

					if (lookFor3) {
						posX = testX;
						posY = testY;
						break;
					}
					 
					break;
				}
			   */ 
					boolean findEdgeOnGREHighThresHold2 = findEdgeOnImage((int) posX, (int) posY, greImageSlice, 5, 50, GRE_SECTION4_HIGH_THRESHOLD);
					if (findEdgeOnGREHighThresHold2) {
						testX = posX;
						testY = posY;
						lookFor3 = false;
						while (distCurrent < distOuter) {
							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class1Intensity = class1Image.getDouble((int) testX, (int) testY);
							class2Intensity = class2Image.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							greIntensity = greImageSlice.getDouble((int) testX, (int) testY);
							if (greIntensity >= 2900 || fuzzyCIntensity == 1 || fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class2Intensity >= 0.04f || class1Intensity >= 0.5f
									|| class3Intensity >= 0.01f) {
								lookFor3 = true;
								break;
							}

							testX += stepX;
							testY += stepY;
							distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
						}

						if (lookFor3) {
							posX = testX;
							posY = testY;
							break;
						}
						 
						break;
					}
						
			 	
			// }
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
			    break;
			// findBlackRegionPattern = false;
			// findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 3, 0, GRE_BLACK);
           
		
			// if (findBlackRegionPattern) {
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.04f || class1Intensity >= 0.5f || class3Intensity >= 0.01f) {
					break;
				}
			// }
		 
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection5(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber, int currentStep,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image, 
			ModelImage fuzzyCImage, int propagationDirection) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;
		double greIntensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
		while ( ( fuzzyCIntensity != 1 || fuzzyCIntensity != 3 || fuzzyCIntensity != 2 ) && distCurrent < distOuter ) {

			if (posX <= 0) {
				posX = 10;
				break;
			}
			if (posY <= 0) {
				posY = 10;
				break;
			}
            
		
			// for closed to end slices
			if ((sliceNumber <= startPt + tenPercent) || (sliceNumber >= endPt - tenPercent)) {
				while ( distCurrent < distOuter ) {
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 8, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ( ( fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) && distCurrent < distOuter ) {
								posX += stepX;
								posY += stepY;
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
					
					if (fuzzyCIntensity == 1 ||fuzzyCIntensity == 2 || fuzzyCIntensity == 3  )
						break;
					// if ( greIntensity >= 3000 ) break;
					posX += stepX;
					posY += stepY;
					currentStep++;
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
		   
			/*
			if (degree >= (section5_degree_lowerHalf_end - 30) && degree <= section5_degree_lowerHalf_end) {
				
				 if ( propagationDirection == MidToEnd ) {
					boolean findEdgeOnGREHighThresHold = findEdgeOnImage((int) posX, (int) posY, greImageSlice, 5, 50, GRE_SECTION4_HIGH_THRESHOLD);
					if (findEdgeOnGREHighThresHold) {
	
						float testX = posX;
						float testY = posY;
						
						boolean lookFor3 = false;
						while (distCurrent < distOuter) {
							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class1Intensity = class1Image.getDouble((int) testX, (int) testY);
							class2Intensity = class2Image.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							greIntensity = greImageSlice.getDouble((int) testX, (int) testY);
	
							if (greIntensity >= 2900 || fuzzyCIntensity == 1 || fuzzyCIntensity == 3 || fuzzyCIntensity == 2
									|| class2Intensity >= 0.04f || class1Intensity >= 0.5f || class3Intensity >= 0.01f  ) {
								lookFor3 = true;
								break;
							}
	
							testX += stepX;
							testY += stepY;
							distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
						}
	
						if (lookFor3) {
							posX = testX;
							posY = testY;
							break;
						}
	                    
						
	
						break;
					}
				 }

			}
			*/ 
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			// walk around holes
		
			if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 10, 0, FuzzyC);
				if (findHoles) {
					while ((fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) && distCurrent < distOuter ) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
			}
            
			
			/*
			boolean findGreyRegionPattern = false;
		    findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 3, 0, GRE_GREY);
		    if ( findGreyRegionPattern ) {
		    	continue;
		    }
			 
			
		    boolean findBlackRegionPattern = false;
			findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 3, 0, GRE_BLACK);
			if ( findBlackRegionPattern ) {
		    	continue;
		    }
		    */ 
		   
			if (degree > section5_degree_lowerHalf_end - 30 && degree <= section5_degree_lowerHalf_end )  {
				boolean findWeakEdge = false;
				boolean findWeakEdgeFuzzyC = false;
				boolean findWeakEdgeClass3 = false;
				boolean findWeakEdgeClass2 = false;
				int []xResult = new int[1];
				xResult[0] = -1;
				float testX = posX, testY = posY;
				boolean lookFor3 = false;
		
				findWeakEdgeFuzzyC = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage,5, FuzzyC_ALL, true, xResult);
				findWeakEdgeClass3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, class3Image, 5, Class3, true, xResult);
				findWeakEdgeClass2 = findWeakConnectedEdgeOnImageHorizontal((int)posX, (int)posY, class2Image, 5, Class2, true, xResult);
				if (findWeakEdgeFuzzyC || findWeakEdgeClass3 || findWeakEdgeClass2) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						class2Intensity = class2Image.getDouble((int) testX, (int) testY);
						class1Intensity = class1Image.getDouble((int) testX, (int) testY);
						greIntensity = greImageSlice.getDouble((int) testX, (int) testY);
						
						if (greIntensity >= 2900 || fuzzyCIntensity == 1 || fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.01f || class2Intensity >= 0.04f
								|| class1Intensity >= 0.5f) {
							lookFor3 = true;
							break;
						}

						testX += stepX;
						testY += stepY;
						distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
					}

					if (lookFor3) {
						posX = testX;
						posY = testY;
						break;
					}

					break;
				}
			    
				/*
			    boolean findEdgeOnGRE = false;
				findEdgeOnGRE = findEdgeOnImage((int) posX, (int) posY, greImageSlice, 7, 50, GRE);
			    if (findEdgeOnGRE  ) { 
					
				    testX = posX; testY = posY;
				    lookFor3 = false;
					while (distCurrent < distOuter ) {
						
						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class1Intensity = class1Image.getDouble((int) testX, (int) testY);
						class2Intensity = class2Image.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						
					
						if (fuzzyCIntensity == 1  ||fuzzyCIntensity == 3  || fuzzyCIntensity == 2 || 
								class2Intensity >= 0.04f || class1Intensity >= 0.5f  || class3Intensity >= 0.01f) {
							lookFor3 = true;
							break;
						}	
				
						testX += stepX;
						testY += stepY;
						distCurrent = (float) Math.sqrt((testX - center.X) * (testX - center.X) + (testY - center.Y) * (testY - center.Y));
					}

					if (lookFor3) {
						posX = testX;
						posY = testY;
						break;
					}
					
                     
					break;
				}
				*/ 
			}
		
			
		   
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		    if (fuzzyCIntensity == 1 || fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
				break;
						
		    class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.04f || class1Intensity >= 0.5f  || class3Intensity >= 0.01f) {
				break;
			}
			
			 
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}

	private void tracingDFS(ModelImage fatImageSlice, ModelImage fuzzyCImage, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage greImageSlice, int sliceNumber, VOI resultVOIBoundary, VOI resultVOIInner, VOI resultVOIOuter, int propagationDirection ) {

		// stop = false;
		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;

		float[] xPtsOuter;
		float[] yPtsOuter;
		float[] zPtsOuter;

		float[] xPtsInner;
		float[] yPtsInner;
		float[] zPtsInner;

		// Vector3f center = resultVOIBoundary.getGeometricCenter();
		// from the VOI contour, find the 6 sections range in degree
		// and tracing each section differently.
		Vector3f center = new Vector3f();
		float[][] sections = new float[7][2];
		findSections(resultVOIBoundary, sections, center);

		if (firstAssigned == false) {
			// red color section
			section1_degree_start = sections[0][0];
			section1_degree_end = sections[0][1];

			// green color section
			section2_degree_start = sections[1][0];
			section2_degree_end = sections[1][1];

			// blue color section
			section3_degree_start = sections[2][0];
			section3_degree_end = sections[2][1];

			// pink color section
			section4_degree_start = sections[3][0];
			section4_degree_end = sections[3][1];

			// yellow color section
			section5_degree_upperHalf_start = sections[4][0];
			section5_degree_upperHalf_end = sections[4][1];

			section5_degree_lowerHalf_start = sections[5][0];
			section5_degree_lowerHalf_end = sections[5][1];

			// black color section
			section6_degree_start = sections[6][0];
			section6_degree_end = sections[6][1];

			firstAssigned = true;
		}

		VOI resultVOI = new VOI((short) 0, "result-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(resultVOI);

		float x, y;
		float stepX, stepY;
		float posX, posY;
		int i;

		VOI lineVOI = new VOI((short) greImageSlice.getVOIs().size(), "line", VOI.LINE, 0);
		lineVOI.setColor(Color.green);
		VOIVector voiVectorLines = new VOIVector();
		voiVectorLines.add(lineVOI);

		Hashtable<Integer, Line> linePoints = new Hashtable<Integer, Line>();

		// 1. get outer contour limit
		VOIBaseVector outer_va = resultVOIOuter.getCurves();
		VOIBase outer_v = outer_va.get(0);

		int nPtsCurrent = outer_v.size();

		xPtsOuter = new float[nPtsCurrent];
		yPtsOuter = new float[nPtsCurrent];
		zPtsOuter = new float[nPtsCurrent];
		outer_v.exportArrays(xPtsOuter, yPtsOuter, zPtsOuter);

		// 2. get inner contour starting point
		VOIBaseVector inner_va = resultVOIInner.getCurves();
		VOIBase inner_v = inner_va.get(0);

		xPtsInner = new float[nPtsCurrent];
		yPtsInner = new float[nPtsCurrent];
		zPtsInner = new float[nPtsCurrent];
		inner_v.exportArrays(xPtsInner, yPtsInner, zPtsInner);

		Vector3f pt[] = new Vector3f[nPtsCurrent];

		for (i = 0; i < nPtsCurrent; i++) {

			int currentStep = 0;
			float distOuter = 0;
			float distInner = 0;
			float distCurrent = 0;

			float xOuter = xPtsOuter[i];
			float yOuter = yPtsOuter[i];
			distOuter = (float) Math.sqrt((xOuter - center.X) * (xOuter - center.X) + (yOuter - center.Y) * (yOuter - center.Y));

			float xInner = xPtsInner[i];
			float yInner = yPtsInner[i];
			distInner = (float) Math.sqrt((xInner - center.X) * (xInner - center.X) + (yInner - center.Y) * (yInner - center.Y));

			// numSteps = 800;

			stepX = (xInner - center.X) / 600;
			stepY = (yInner - center.Y) / 600;

			currentStep = 600;
			posX = xInner;
			posY = yInner;

			float degree;

			Vector2f v_curr_inner_cartisian = new Vector2f(xInner, yInner);
			Vector2f v_curr_inner_polar = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(v_curr_inner_cartisian, v_curr_inner_polar, center);

			degree = v_curr_inner_polar.Y;

			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			int totalSliceNumber = endPt - startPt;
			// fivePercent = (int) (totalSliceNumber * 0.05f);
			twentyPercent = (int) (totalSliceNumber * 0.2f);
			tenPercent = (int) (totalSliceNumber * 0.15f);
		
			if ((sliceNumber <= startPt + tenPercent) || (sliceNumber >= endPt - tenPercent)) {
				posX = center.X;
				posY = center.Y;
				currentStep = 0;
			}
             
			// Trace each section differently
			Vector2f resultPos = new Vector2f(0, 0);
			if (degree >= section1_degree_start && degree < section1_degree_end) {
				resultPos = traceSection1(posX, posY, stepX, stepY, center, degree, sliceNumber, currentStep, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				resultPos = traceSection2(posX, posY, stepX, stepY, center, degree, sliceNumber, currentStep, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section3_degree_start && degree < section3_degree_end) {
				resultPos = traceSection3(posX, posY, stepX, stepY, center, degree, sliceNumber, currentStep, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section4_degree_start && degree < section4_degree_end ) {
				resultPos = traceSection4(posX, posY, stepX, stepY, center, degree, sliceNumber, currentStep, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if ((degree > section5_degree_upperHalf_start && degree <= section5_degree_upperHalf_end)
					|| (degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end)) {
				resultPos = traceSection5(posX, posY, stepX, stepY, center, degree, sliceNumber, currentStep, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage, propagationDirection);
			} else { // section 6
				resultPos = traceSection6(posX, posY, stepX, stepY, center, degree, sliceNumber, currentStep, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			}

			posX = resultPos.X;
			posY = resultPos.Y;

			// final line
			pt[i] = new Vector3f(posX, posY, 0f);

			// add the green tracing lines
			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(center.X, center.Y, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));
			lineVOI.importCurve(kLine);

			Line line = new Line(center.X, center.Y, posX, posY, i);
			linePoints.put(i, line);

		} // end i loop;

		resultVOI.importCurve(pt);
		// voiNewFinal.importCurve(points);
		// slicesPts.put(sliceNumber, linePoints);
		// findBoundingContour(sliceNumber, resultVOIBoundary, resultVOIInner,
		// resultVOIOuter);

		// greImageSlice.addVOIs(voiVectorLines);
		// new ViewJFrameImage(greImageSlice);

		greImageSlice.addVOIs(voiVectorNew);
		smoothVOI150Single(greImageSlice, greImageSlice);
		double constant_length = 1.2d;
		resultVOI = greImageSlice.getVOIs().VOIAt(0);
		VOIBaseVector vVector = resultVOI.getCurves();

		VOIBase v = vVector.get(0);
		double perimeter = v.getLengthPtToPt(greImageSlice.getFileInfo(0).getResolutions());
		// System.err.println("perimeter = " + perimeter);

		// deal with near end slices, tracing the pattern from center of result
		// VOI
		if ((sliceNumber - startPt) <= 2 || (endPt - sliceNumber) <= 2) {
			constant_length = 0.5d;
		} else if (((sliceNumber - startPt) > 2 && (sliceNumber - startPt) <= 5) || (endPt - sliceNumber) > 2 && (endPt - sliceNumber) <= 5) {
			constant_length = 0.8d;
		} else {
			constant_length = 1.2d;
		}

		int numberPoints = (int) Math.round(perimeter / constant_length);

		if (numberPoints <= 5)
			return;

		float res[] = greImageSlice.getFileInfo(0).getResolutions();
		float xRes = res.length > 0 ? res[0] : 1;
		float yRes = res.length > 1 ? res[1] : 1;
		float zRes = res.length > 2 ? res[2] : 1;

		resultVOI = greImageSlice.getVOIs().VOIAt(0);
		Vector<VOIBase>[] vArray = resultVOI.getSortedCurves(VOIBase.ZPLANE, 1);
		VOIBase result_v = vArray[0].get(0);
		int nPts = result_v.size();
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];

		float[] xNew = new float[numberPoints];
		float[] yNew = new float[numberPoints];
		float[] zNew = new float[numberPoints];

		// System.err.println(" numberPoints = " + numberPoints);
		result_v.exportArrays(xPts, yPts, zPts);

		// equal distance interpolation
		float x1, y1, x2, y2, distance;
		int m = 0;
		int k = 0;
		boolean find = false;

		while (k < nPts - 5) {
			x1 = xPts[k];
			y1 = yPts[k];
			find = false;
			for (int j = k + 1; j < k + 6; j++) {
				x2 = xPts[j];
				y2 = yPts[j];
				distance = (float) Math.sqrt((x2 - x1) * (x2 - x1) * (xRes * xRes) + (y2 - y1) * (y2 - y1) * (yRes * yRes));
				if (distance >= constant_length) {
					xNew[m] = x2;
					yNew[m] = y2;
					zNew[m] = 0f;

					m++;
					k = j;
					find = true;
					break;
				}
			}
			if (find)
				continue;
			k++;
		}

		while (k < nPts) {
			x1 = xPts[k];
			y1 = yPts[k];
			find = false;
			int endPt = k + 6;
			if (k + 6 > nPts) {
				endPt = nPts;
			}
			for (int j = k + 1; j < endPt; j++) {
				x2 = xPts[j];
				y2 = yPts[j];
				distance = (float) Math.sqrt((x2 - x1) * (x2 - x1) * (xRes * xRes) + (y2 - y1) * (y2 - y1) * (yRes * yRes));
				if (distance >= constant_length) {
					xNew[m] = x2;
					yNew[m] = y2;
					zNew[m] = 0f;

					m++;
					k = j;
					find = true;
					break;
				}
			}
			if (find)
				continue;
			k++;
		}

		linePoints.clear();

		for (int z = 0; z < m; z++) {
			posX = xNew[z];
			posY = yNew[z];
			Line line = new Line(center.X, center.Y, posX, posY, z);
			linePoints.put(z, line);
		}

		slicesPts.put(sliceNumber, linePoints);

		// generate inner and outer contours
		// if ( (sliceNumber <= startPt + tenPercent) || (sliceNumber >= endPt -
		// tenPercent) ) {
		// if ((sliceNumber - startPt) <= 5 || (endPt - sliceNumber) <= 5 ) {}
		if ((sliceNumber > startPt + tenPercent) && (sliceNumber < endPt - tenPercent)) {
			findBoundingContour(sliceNumber, resultVOIBoundary, resultVOIInner, resultVOIOuter, center, fatImageSlice, propagationDirection);
		}

		// copy result VOI from 2D slice to 3D slice.
		resultVOI.removeCurves();
		VOIBase vTemp = (VOIBase) result_v.clone();

		vTemp.importArrays(xNew, yNew, zNew, m);
		resultVOI.importCurve(vTemp);
		smoothVOISingle(greImageSlice, greImageSlice, m);

		fuzzyCImage.addVOIs(voiVectorNew);
		class1Image.addVOIs(voiVectorNew);
		class2Image.addVOIs(voiVectorNew);
		new ViewJFrameImage(greImageSlice);
	    new ViewJFrameImage(class1Image);
		new ViewJFrameImage(class2Image);
		new ViewJFrameImage(class3Image);
		new ViewJFrameImage(fuzzyCImage);

		// VOIBaseVector current_va =
		// greImageSlice.getVOIs().VOIAt(2).getCurves();
		VOIBaseVector current_va = greImageSlice.getVOIs().VOIAt(0).getCurves();
		if (current_va.size() > 0) {
			VOIBase current_v = current_va.get(0);

			vTemp = (VOIBase) current_v.clone();

			nPtsCurrent = current_v.size();
			float[] xPtsCurrent = new float[nPtsCurrent];
			float[] yPtsCurrent = new float[nPtsCurrent];
			float[] zPtsCurrent = new float[nPtsCurrent];
			current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);

			for (int j = 0; j < nPtsCurrent; j++) {
				zPtsCurrent[j] = sliceNumber;
			}

			vTemp.importArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent, nPtsCurrent);
			voiNewFinal.importCurve(vTemp);
			vTemp = null;
		}

		/*
		 * greImageSlice.addVOIs(voiVectorLines);
		 * gaussianImageSlice.addVOIs(voiVectorLines);
		 * fuzzyCImage.addVOIs(voiVectorLines);
		 * class1Image.addVOIs(voiVectorLines);
		 * 
		 * new ViewJFrameImage(fuzzyCImage); new ViewJFrameImage(class1Image);
		 * new ViewJFrameImage(gaussianImageSlice); new
		 * ViewJFrameImage(greImageSlice);
		 */

		// return resultVOI.getGeometricCenter();
	}
	
	
	private boolean findGreyRegionOnImage(int x, int y, ModelImage imageSlice, int range, double intensity, int type) {

		int visited[][] = new int[512][512];
		int count[] = new int[1];
		count[0] = 1;
		int Xmin = (int) x - range;
		int Xmax = (int) x + range;
		int Ymin = (int) y - range;
		int Ymax = (int) y + range;

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		int xDim = Xmax - Xmin + 1;
		int yDim = Ymax - Ymin + 1;

		// System.err.println(" xDim = " + xDim + " yDim = " + yDim);

		int[][] map = new int[yDim][xDim];

		generateMap(x, y, imageSlice, visited, map, Xmin, Xmax, Ymin, Ymax, type);

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;

		int startX = x - Xmin;
		int startY = y - Ymin;

		int findRegion = regionPattern.searchRegion(map, startX, startY, xDim, yDim, range, type);
		if (findRegion == 1) {
			return true;
		} else {
			return false;
		}
	}
	
	private boolean findBlackRegionOnImage(int x, int y, ModelImage imageSlice, int range, double intensity, int type) {

		int visited[][] = new int[512][512];
		int count[] = new int[1];
		count[0] = 1;
		int Xmin = (int) x - range;
		int Xmax = (int) x + range;
		int Ymin = (int) y - range;
		int Ymax = (int) y + range;

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		int xDim = Xmax - Xmin + 1;
		int yDim = Ymax - Ymin + 1;

		// System.err.println(" xDim = " + xDim + " yDim = " + yDim);

		int[][] map = new int[yDim][xDim];

		generateMap(x, y, imageSlice, visited, map, Xmin, Xmax, Ymin, Ymax, type);

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;

		int startX = x - Xmin;
		int startY = y - Ymin;

		int findRegion = regionPattern.searchRegion(map, startX, startY, xDim, yDim, range, type);
		if (findRegion == 1) {
			return true;
		} else {
			return false;
		}
	}


	public void smoothVOISingle(ModelImage maskImage, ModelImage resultImage, int nPts) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), nPts, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	public static final double distance(final Vector3f pt1, final Vector3f pt2, final float[] res) {
		float xRes = res.length > 0 ? res[0] : 1;
		float yRes = res.length > 1 ? res[1] : 1;
		float zRes = res.length > 2 ? res[2] : 1;
		return Math.sqrt(((pt2.X - pt1.X) * (pt2.X - pt1.X) * (xRes * xRes)) + ((pt2.Y - pt1.Y) * (pt2.Y - pt1.Y) * (yRes * yRes))
				+ ((pt2.Z - pt1.Z) * (pt2.Z - pt1.Z) * (zRes * zRes)));
	}

	private void findBoundingContour(int sliceNumber, VOI resultVOIBoundary, VOI resultVOIInner, VOI resultVOIOuter, Vector3f center, 
			ModelImage fatImageSlice, int propagationDirection ) {

		int nPtsCurrent;

		float centerX, centerY;
		float posX, posY;
		int i;

		Hashtable<Integer, Line> linePoints = slicesPts.get(sliceNumber);
		nPtsCurrent = linePoints.size();
		Vector3f pt_inner[] = new Vector3f[nPtsCurrent];
		Vector3f pt_outer[] = new Vector3f[nPtsCurrent];
		Vector3f pt_boundary[] = new Vector3f[nPtsCurrent];

		float stepX, stepY;
		// int index_inner;
		// int index_outer;

		// int inward_steps = 60;
		// int outward_steps = 10;

		float traceX, traceY;

		float degree;
		double distance, distanceOuter = 0d, distCurrent;
		double distanceInner;

		// draw colored radical lines.
		VOI section1 = new VOI((short) 0, "line1", VOI.LINE, 0);
		section1.setColor(Color.red);
		VOIVector voiVectorLines = new VOIVector();
		voiVectorLines.add(section1);

		VOI section2 = new VOI((short) 1, "line2", VOI.LINE, 0);
		section2.setColor(Color.green);
		voiVectorLines.add(section2);

		VOI section3 = new VOI((short) 2, "line3", VOI.LINE, 0);
		section3.setColor(Color.blue);
		voiVectorLines.add(section3);

		VOI section4 = new VOI((short) 3, "line4", VOI.LINE, 0);
		section4.setColor(Color.magenta);
		voiVectorLines.add(section4);

		VOI section5 = new VOI((short) 4, "line5", VOI.LINE, 0);
		section5.setColor(Color.yellow);
		voiVectorLines.add(section5);

		VOI section6 = new VOI((short) 5, "line6", VOI.LINE, 0);
		section6.setColor(Color.cyan);
		voiVectorLines.add(section6);

		int totalSliceNumber = endPt - startPt;
		tenPercent = (int) (totalSliceNumber * 0.15f);
		// fivePercent = (int) (totalSliceNumber * 0.05f);
		twentyPercent = (int) (totalSliceNumber * 0.2f);

		for (i = 0; i < nPtsCurrent; i++) {

			Line line = linePoints.get(i);

			centerX = line.startX;
			centerY = line.startY;

			posX = line.endX;
			posY = line.endY;

			Vector2f cartesianIn = new Vector2f(posX, posY);
			Vector2f polarOut = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(cartesianIn, polarOut, center);
			degree = polarOut.Y;

			distance = Math.sqrt((posX - centerX) * (posX - centerX) + (posY - centerY) * (posY - centerY));
			distanceInner = distance * (1d - 0.10d);
			// distanceInner = distance * ( 1d - 0.17d );
			numSteps = 800;

			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(center.X, center.Y, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));
			
			if (degree >= section1_degree_start && degree < section1_degree_end) {
				distanceOuter = distance * (1d + 0.05d);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				distanceOuter = distance * (1d + 0.02d);
			} else if ((degree >= section3_degree_start && degree < section3_degree_end)) {
				distanceOuter = distance * (1d + 0.01d); // For other patients
				// distanceOuter = distance * (1d + 0.10d); // For patient 16 only
				
				
				if ( propagationDirection == MidToBegin ) {
					distanceOuter = distance * (1d + 0.01d);
					if ( degree >= section3_degree_start  && degree <= (section3_degree_start + 30 )) {
						distanceInner = distance * (1d - 0.03d);
					} else {
						distanceInner = distance * (1d - 0.03d);
					}
				} else if ( propagationDirection == MidToEnd ) {
					distanceOuter = distance * (1d + 0.01d);
					// distanceInner = distance * (1d - 0.07d);
					if ( degree >= section3_degree_start  && degree <= (section3_degree_start + 30 )) {
						distanceInner = distance * (1d - 0.07d);
					} else {
						distanceInner = distance * (1d - 0.03d);
					}
				}
				
				
			} else if ((degree >= section4_degree_start && degree < section4_degree_end )) {
				
				if ( propagationDirection == MidToBegin ) {
					distanceOuter = distance * (1d + 0.005d);
					distanceInner = distance * (1d - 0.07d);
				} else if ( propagationDirection == MidToEnd ) {
					distanceOuter = distance * (1d + 0.005d);
					distanceInner = distance * (1d - 0.08d);
				}
				
			} else if ((degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end)
					|| (degree >= section5_degree_upperHalf_start && degree < section5_degree_upperHalf_end)) {
				
				
				if ( propagationDirection == MidToBegin ) {
					distanceOuter = distance * (1d + 0.01d);
					distanceInner = distance * (1d - 0.03d);
					if ( degree >=  (section5_degree_lowerHalf_end - 30) && degree <= section5_degree_lowerHalf_end ) {
				 	  distanceOuter = distance * (1d + 0.01d);
				 	  distanceInner = distance * (1d - 0.03d);
					}
				} else if ( propagationDirection == MidToEnd ) {
					distanceOuter = distance * (1d + 0.01d);
					distanceInner = distance * (1d - 0.05d);
					if ( degree >=  (section5_degree_lowerHalf_end - 30) && degree <= section5_degree_lowerHalf_end ) {
				 	  distanceOuter = distance * (1d + 0.01d);
				 	  distanceInner = distance * (1d - 0.08d);
					}
				}
				
			} else if (degree >= section6_degree_start && degree < section6_degree_end) {
				distanceOuter = distance * (1d + 0.05d);
				distanceInner = distance * (1d - 0.05d);
			}
			// ruida
			if (degree >= section4_degree_start && degree < section4_degree_end ) {
				// distanceInner = distance * (1d - 0.20d); // ruida check
				// numSteps = 800;
			}

			if ((degree >= section1_degree_start && degree < section1_degree_end)) {
				section1.importCurve(kLine);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				section2.importCurve(kLine);
			} else if ((degree >= section3_degree_start && degree < section3_degree_end)) {
				section3.importCurve(kLine);
			} else if ((degree >= section4_degree_start && degree < section4_degree_end)) {
				section4.importCurve(kLine);
			} else if ((degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end)
					|| (degree >= section5_degree_upperHalf_start && degree < section5_degree_upperHalf_end)) {
				section5.importCurve(kLine);
			} else if (degree >= section6_degree_start && degree < section6_degree_end) {
				section6.importCurve(kLine);
			}

			pt_boundary[i] = new Vector3f(posX, posY, 0f);

			stepX = (posX - centerX) / 600f;
			stepY = (posY - centerY) / 600f;

			traceX = posX;
			traceY = posY;

			distCurrent = Math.sqrt((traceX - centerX) * (traceX - centerX) + (traceY - centerY) * (traceY - centerY));
			while (distCurrent > distanceInner) {
				traceX -= stepX;
				traceY -= stepY;
				// index_inner++;
				distCurrent = Math.sqrt((traceX - centerX) * (traceX - centerX) + (traceY - centerY) * (traceY - centerY));
			}
			pt_inner[i] = new Vector3f(traceX, traceY, 0f);

			traceX = posX;
			traceY = posY;

			distCurrent = Math.sqrt((traceX - centerX) * (traceX - centerX) + (traceY - centerY) * (traceY - centerY));
			while (distCurrent < distanceOuter) {
				traceX += stepX;
				traceY += stepY;
				distCurrent = Math.sqrt((traceX - centerX) * (traceX - centerX) + (traceY - centerY) * (traceY - centerY));
				// index_outer++;
			}
			pt_outer[i] = new Vector3f(traceX, traceY, 0f);

		} // end i loop;

		resultVOIInner.removeCurves();
		resultVOIOuter.removeCurves();
		resultVOIBoundary.removeCurves();

		resultVOIBoundary.importCurve(pt_boundary);
		resultVOIInner.importCurve(pt_inner);
		resultVOIOuter.importCurve(pt_outer);

		

		fatImageSlice.addVOIs(voiVectorLines); // cheng
		new ViewJFrameImage(fatImageSlice);

		ModelImage cloneImage = (ModelImage) fatImageSlice.clone();
		VOIVector voiVectorInOut = new VOIVector();
		voiVectorInOut.add(resultVOIInner);
		voiVectorInOut.add(resultVOIOuter);
		cloneImage.addVOIs(voiVectorInOut);
		new ViewJFrameImage(cloneImage);
	}

	private void findSections(VOI boundary, float[][] sections, Vector3f center) {

		int i;
		float posX, posY;

		// add mid slice VOI to the GRE image
		// Vector<VOIBase>[] vArray = boundary.getSortedCurves(VOIBase.ZPLANE,
		// 1);
		// VOIBase v = vArray[0].get(0);

		VOIBaseVector current_va = boundary.getCurves();
		if (current_va.size() > 0) {

			VOIBase v = current_va.get(0);

			int nPts = v.size();
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];

			v.exportArrays(xPts, yPts, zPts);

			float yMax_x = Float.MIN_VALUE, yMax_y = Float.MIN_VALUE, yMin_x = Float.MAX_VALUE, yMin_y = Float.MAX_VALUE;
			float xMax_x = Float.MIN_VALUE, xMax_y = Float.MIN_VALUE, xMin_x = Float.MAX_VALUE, xMin_y = Float.MAX_VALUE;

			for (i = 0; i < nPts; i++) {

				posX = xPts[i];
				posY = yPts[i];

				if (posY > yMax_y) {
					yMax_y = posY; // yMax_y, max Y
					yMax_x = posX;
				}

				if (posY < yMin_y) { // yMin_y, min Y
					yMin_y = posY;
					yMin_x = posX;
				}

				if (posX > xMax_x) {
					xMax_x = posX; // xMax_x, max X
					xMax_y = posY;
				}

				if (posX < xMin_x) {
					xMin_x = posX; // xMin_x, min x
					xMin_y = posY;
				}

			}

			// System.err.println("Before:  center.X = " + center.X +
			// "  center.Y = " + center.Y + "   center.Z = " + center.Z);

			center.X = (xMax_x + xMin_x) / 2.0f;
			center.Y = (yMax_y + yMin_y) / 2.0f;
			center.Z = 0f;

			// System.err.println("After:  center.X = " + center.X +
			// "  center.Y = " + center.Y + "   center.Z = " + center.Z);

			Vector2f zero_ref_point = new Vector2f(yMin_x, yMin_y);
			Vector2f zero_ref_polar = new Vector2f();
			Vector2f pi_ref_point = new Vector2f(yMax_x, yMax_y);
			Vector2f pi_ref_polar = new Vector2f();

			MipavCoordinateSystems.CartesianToPolar2D(zero_ref_point, zero_ref_polar, center);
			MipavCoordinateSystems.CartesianToPolar2D(pi_ref_point, pi_ref_polar, center);

			// red color section
			float section1_start = zero_ref_polar.Y - 20;
			float section1_end = zero_ref_polar.Y;

			// System.err.println("section1_degree_start = " +
			// section1_degree_start + "  section1_degree_end = " +
			// section1_degree_end);
			sections[0][0] = section1_start;
			sections[0][1] = section1_end;

			// green color section
			float section2_start = zero_ref_polar.Y - 40;
			float section2_end = zero_ref_polar.Y - 20;
			// System.err.println("section2_degree_start = " +
			// section2_degree_start + "  section2_degree_end = " +
			// section2_degree_end);
			sections[1][0] = section2_start;
			sections[1][1] = section2_end;

			// blue color section
			float section3_start = pi_ref_polar.Y;
			float section3_end = zero_ref_polar.Y - 40;
			// System.err.println("section3_degree_start = " +
			// section3_degree_start + "  section3_degree_end = " +
			// section3_degree_end);
			sections[2][0] = section3_start;
			sections[2][1] = section3_end;

			// pink color section
			float section4_start = pi_ref_polar.Y - 35;
			float section4_end = pi_ref_polar.Y;
			// System.err.println("section4_degree_start = " +
			// section4_degree_start + "  section4_degree_end = " +
			// section4_degree_end);
			sections[3][0] = section4_start;
			sections[3][1] = section4_end;

			// yellow color section
			float section5_upperHalf_start = zero_ref_polar.Y + 20;
			float section5_upperHalf_end = 360;
			// System.err.println("section5_degree_upperHalf_start = " +
			// section5_degree_upperHalf_start +
			// "  section5_degree_upperHalf_end = "
			// + section5_degree_upperHalf_end);
			sections[4][0] = section5_upperHalf_start;
			sections[4][1] = section5_upperHalf_end;

			float section5_lowerHalf_start = 0;
			float section5_lowerHalf_end = pi_ref_polar.Y - 35;
			// System.err.println("section5_degree_lowerHalf_start = " +
			// section5_degree_lowerHalf_start +
			// "  section5_degree_lowerHalf_end = "
			// + section5_degree_lowerHalf_end);
			sections[5][0] = section5_lowerHalf_start;
			sections[5][1] = section5_lowerHalf_end;

			// black color section
			float section6_start = zero_ref_polar.Y;
			float section6_end = zero_ref_polar.Y + 20;
			// System.err.println("section6_degree_start = " +
			// section6_degree_start + "  section6_degree_end = " +
			// section6_degree_end);
			sections[6][0] = section6_start;
			sections[6][1] = section6_end;
		}
	}
	private void tracingDFS_mid(ModelImage greImageSlice, ModelImage fatImageSliceMid, VOI boundary, VOI inner, VOI outer, 
			boolean first, int propagationDirection) {

		VOIVector src = greImage.getVOIs();
		int zDim = greImage.getExtents()[2];
		VOI sliceVOI = src.VOIAt(0);
		Vector<VOIBase>[] vArray = sliceVOI.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[midPt].get(0);

		Vector3f center = sliceVOI.getGeometricCenter();

		int nPts = v.size();
		float posX, posY;
		int i;

		Vector3f pt[] = new Vector3f[nPts];
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];

		Hashtable<Integer, Line> linePoints = new Hashtable<Integer, Line>();

		v.exportArrays(xPts, yPts, zPts);

		for (i = 0; i < nPts; i++) {

			posX = xPts[i];
			posY = yPts[i];

			// final line
			pt[i] = new Vector3f(posX, posY, 0f);

			// add the green tracing lines
			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(center.X, center.Y, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));

			Line line = new Line(center.X, center.Y, posX, posY, i);
			linePoints.put(i, line);

		} // end i loop;

		boundary.importCurve(pt);

		// VOI resultVOI = new VOI((short) 0, "result-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(boundary);

		greImageSlice.addVOIs(voiVectorNew);
		smoothVOI150Single(greImageSlice, greImageSlice);
		double constant_length = 1.2d;
		boundary = greImageSlice.getVOIs().VOIAt(0);
		VOIBaseVector vVector = boundary.getCurves();

		v = vVector.get(0);
		double perimeter = v.getLengthPtToPt(greImageSlice.getFileInfo(0).getResolutions());
		// System.err.println("perimeter = " + perimeter);
		int numberPoints = (int) Math.round(perimeter / constant_length);

		float res[] = greImageSlice.getFileInfo(0).getResolutions();
		float xRes = res.length > 0 ? res[0] : 1;
		float yRes = res.length > 1 ? res[1] : 1;
		float zRes = res.length > 2 ? res[2] : 1;

		boundary = greImageSlice.getVOIs().VOIAt(0);
		vArray = boundary.getSortedCurves(VOIBase.ZPLANE, 1);
		VOIBase result_v = vArray[0].get(0);
		nPts = result_v.size();
		xPts = new float[nPts];
		yPts = new float[nPts];
		zPts = new float[nPts];

		float[] xNew = new float[numberPoints];
		float[] yNew = new float[numberPoints];
		float[] zNew = new float[numberPoints];

		// System.err.println(" numberPoints = " + numberPoints);
		result_v.exportArrays(xPts, yPts, zPts);

		float x1, y1, x2, y2, distance;
		int m = 0;
		int k = 0;
		boolean find = false;

		while (k < nPts - 5) {
			x1 = xPts[k];
			y1 = yPts[k];
			find = false;
			for (int j = k + 1; j < k + 6; j++) {
				x2 = xPts[j];
				y2 = yPts[j];
				distance = (float) Math.sqrt((x2 - x1) * (x2 - x1) * (xRes * xRes) + (y2 - y1) * (y2 - y1) * (yRes * yRes));
				if (distance >= constant_length) {
					xNew[m] = x2;
					yNew[m] = y2;
					zNew[m] = 0f;

					m++;
					k = j;
					find = true;
					break;
				}
			}
			if (find)
				continue;
			k++;
		}

		while (k < nPts) {
			x1 = xPts[k];
			y1 = yPts[k];
			find = false;
			int endPt = k + 6;
			if (k + 6 > nPts) {
				endPt = nPts;
			}
			for (int j = k + 1; j < endPt; j++) {
				x2 = xPts[j];
				y2 = yPts[j];
				distance = (float) Math.sqrt((x2 - x1) * (x2 - x1) * (xRes * xRes) + (y2 - y1) * (y2 - y1) * (yRes * yRes));
				if (distance >= constant_length) {
					xNew[m] = x2;
					yNew[m] = y2;
					zNew[m] = 0f;

					m++;
					k = j;
					find = true;
					break;
				}
			}
			if (find)
				continue;
			k++;
		}

		linePoints.clear();

		for (int z = 0; z < m; z++) {
			posX = xNew[z];
			posY = yNew[z];
			Line line = new Line(center.X, center.Y, posX, posY, z);
			linePoints.put(z, line);
		}

		slicesPts.put(midPt, linePoints);
		findBoundingContour(midPt, boundary, inner, outer, center, fatImageSliceMid, propagationDirection);

		// add mid slice VOI to the GRE image
		if (first == true) {
			VOIBaseVector current_va = boundary.getCurves();
			if (current_va.size() > 0) {
				VOIBase current_v = current_va.get(0);

				VOIBase vTemp = (VOIBase) current_v.clone();

				int nPtsCurrent = current_v.size();
				float[] xPtsCurrent = new float[nPtsCurrent];
				float[] yPtsCurrent = new float[nPtsCurrent];
				float[] zPtsCurrent = new float[nPtsCurrent];
				current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);

				for (int j = 0; j < nPtsCurrent; j++) {
					zPtsCurrent[j] = midPt;
				}
				vTemp.importArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent, nPtsCurrent);
				voiNewFinal.importCurve(vTemp);
				vTemp = null;
			}
		}
	}

	private boolean findCornerEdgeOnImage(int x, int y, ModelImage imageSlice, int range, double intensity, int type) {

		int visited[][] = new int[512][512];
		int Xmin = (int) x - range;
		int Xmax = (int) x + range;
		int Ymin = (int) y - range;
		int Ymax = (int) y + range;

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;

		int xDim = Xmax - Xmin + 1;
		int yDim = Ymax - Ymin + 1;

		// System.err.println(" xDim = " + xDim + " yDim = " + yDim);

		int[][] map = new int[yDim][xDim];

		generateMap(x, y, imageSlice, visited, map, Xmin, Xmax, Ymin, Ymax, type);

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;

		int startX = x - Xmin;
		int startY = y - Ymin;
		
		int findCornerEdge = cornerEdgePattern.findCornerEdge(map, startX, startY, xDim, yDim, range);

		if (findCornerEdge == 1) {
			// cornerEdgePattern.printMap(map, xDim, yDim);
			return true;
		} else {
			return false;
		}
	}


	private boolean findWeakConnectedEdgeOnImageHorizontal(int x, int y, ModelImage imageSlice, int range, int type, boolean searchOnRight, int []xResult) {

		int visited[][] = new int[512][512];
		int count[] = new int[1];
		count[0] = 1;
		int Xmin = (int) x - range;
		int Xmax = (int) x + range;
		int Ymin = (int) y - range;
		int Ymax = (int) y + range;

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		int xDim = Xmax - Xmin + 1;
		int yDim = Ymax - Ymin + 1;

		// System.err.println(" xDim = " + xDim + " yDim = " + yDim);

		int[][] map = new int[yDim][xDim];

		generateMap(x, y, imageSlice, visited, map, Xmin, Xmax, Ymin, Ymax, type);

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;

		int startX = x - Xmin;
		int startY = y - Ymin;
		
		// ruida
		int []xValueHorizontal = new int[1];
		xValueHorizontal[0] = -1;
		int []xValueVertical = new int[1];
		xValueVertical[0] = -1;
		
		int findWeakEdgeHorizontal = weakEdgePatternHorizontal.findWeakEdge(map, startX, startY, xDim, yDim, range, searchOnRight, xValueHorizontal);
       if (findWeakEdgeHorizontal == 1 ) {
			// xResult[0] = Math.max(xValueHorizontal[0], xValueVertical[0]);
			// weakEdgePattern.printMap(map, xDim, yDim);
			return true;
		} else {
			return false;
		}
	}
	
	
	private boolean findWeakConnectedEdgeOnImageVertical(int x, int y, ModelImage imageSlice, int range, int type, boolean searchOnRight, int []xResult) {

		int visited[][] = new int[512][512];
		int count[] = new int[1];
		count[0] = 1;
		int Xmin = (int) x - range;
		int Xmax = (int) x + range;
		int Ymin = (int) y - range;
		int Ymax = (int) y + range;

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		int xDim = Xmax - Xmin + 1;
		int yDim = Ymax - Ymin + 1;

		// System.err.println(" xDim = " + xDim + " yDim = " + yDim);

		int[][] map = new int[yDim][xDim];

		generateMap(x, y, imageSlice, visited, map, Xmin, Xmax, Ymin, Ymax, type);

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;

		int startX = x - Xmin;
		int startY = y - Ymin;
		
		// ruida
		int []xValueHorizontal = new int[1];
		xValueHorizontal[0] = -1;
		int []xValueVertical = new int[1];
		xValueVertical[0] = -1;
		
		int findWeakEdgeVertical = weakEdgePatternVertical.findWeakEdge(map, startX, startY, xDim, yDim, range);
		if (findWeakEdgeVertical == 1) {
			// xResult[0] = Math.max(xValueHorizontal[0], xValueVertical[0]);
			// weakEdgePattern.printMap(map, xDim, yDim);
			return true;
		} else {
			return false;
		}
	}

	private boolean findHolesOnImage(int x, int y, ModelImage imageSlice, int range, double intensity, int type) {

		int visited[][] = new int[512][512];
		int count[] = new int[1];
		count[0] = 1;
		int Xmin = (int) x - range;
		int Xmax = (int) x + range;
		int Ymin = (int) y - range;
		int Ymax = (int) y + range;

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		int xDim = Xmax - Xmin + 1;
		int yDim = Ymax - Ymin + 1;

		// System.err.println(" xDim = " + xDim + " yDim = " + yDim);

		int[][] map = new int[yDim][xDim];

		generateMap(x, y, imageSlice, visited, map, Xmin, Xmax, Ymin, Ymax, type);

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;

		// edgeFinding(map, xDim, yDim);
		int startX = x - Xmin;
		int startY = y - Ymin;
		
		
		int findHoles = holesPattern.findHoles(map, startX, startY, xDim, yDim);

		if (findHoles == 1) {
		
			return true;
		} else {
			return false;
		}
	}

	private boolean findEdgeOnImage(int x, int y, ModelImage imageSlice, int range, double intensity, int type) {

		int visited[][] = new int[512][512];
		int count[] = new int[1];
		count[0] = 1;
		int Xmin = (int) x - range;
		int Xmax = (int) x + range;
		int Ymin = (int) y - range;
		int Ymax = (int) y + range;

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		int xDim = Xmax - Xmin + 1;
		int yDim = Ymax - Ymin + 1;

		// System.err.println(" xDim = " + xDim + " yDim = " + yDim);

		int[][] map = new int[yDim][xDim];

		generateMap(x, y, imageSlice, visited, map, Xmin, Xmax, Ymin, Ymax, type);

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;

		int startX = x - Xmin;
		int startY = y - Ymin;
		
		int findEdge = edgePattern.searchEdge(map, startX, startY, xDim, yDim, range);
		if (findEdge == 1) {
			return true;
		} else {
			return false;
		}
	}

	
	void generateMap(int x, int y, ModelImage imageSlice, int visited[][], int map[][], int Xmin, int Xmax, int Ymin, int Ymax, int type) {

		if (x <= 0)
			x = 10;
		if (y <= 0)
			y = 10;

		int xDim = Xmax - Xmin + 1;
		int yDim = Ymax - Ymin + 1;

		visited[y][x] = 1;
		double imgIntensity = imageSlice.getDouble(x, y);

		if (type == FuzzyC ) {
			if (imgIntensity == 1 || imgIntensity == 2 || imgIntensity == 3 ) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // fuzzyC class 1 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		if (type == FuzzyC_ALL ) {
			if (imgIntensity == 1 || imgIntensity == 2 || imgIntensity == 3) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // fuzzyC class 1 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class1) {
			if (imgIntensity >= 0.5f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy ==  1 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		if (type == Class2) {
			if (imgIntensity >= 0.04f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy ==  1 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		if (type == Class3) {
			if (imgIntensity >= 0.01f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy < 0.02 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class1_lowInten) {
			if (imgIntensity <= 0.7f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy ==  1 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class1_weak) {
			if (imgIntensity >= 0.1) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy <  0.1 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class1_corner) {
			if (imgIntensity >= 0.1f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == GRE) {
			if (imgIntensity <= 1000) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy ==  1 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == GRE_GREY) {
			if (imgIntensity >= 2500) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy == 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		if (type == GRE_BLACK) {
			if (imgIntensity <= 1900) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy == 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		
		if (type == GRE_SECTION3_DARK) {
			if (imgIntensity <= 200) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy == 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		

		if (type == GRE_SECTION4_HIGH_THRESHOLD ) {
			if (imgIntensity <= 2300) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else if ( imgIntensity >= 2900 ){ 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			} else {
				
				if ((y - Ymin - 1) >= 0 && (y - Ymin - 1) <= yDim && (x - Xmin - 1) >= 0 && (x - Xmin - 1) <= xDim) {
					map[y - Ymin - 1][x - Xmin - 1] = 0;
				} 
				if ((y - Ymin - 1) >= 0 && (y - Ymin - 1) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim) {
					map[y - Ymin - 1][x - Xmin] = 0;	
				}
				if ((y - Ymin - 1) >= 0 && (y - Ymin - 1) <= yDim && (x - Xmin + 1) >= 0 && (x - Xmin + 1) <= xDim) {
					map[y - Ymin - 1][x - Xmin + 1] = 0;
				}
				
				
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin - 1) >= 0 && (x - Xmin - 1) <= xDim) {
					map[y - Ymin][x - Xmin - 1] = 0;
				}
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim) {
					map[y - Ymin][x - Xmin] = 0;
				}
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin + 1) >= 0 && (x - Xmin + 1) <= xDim ) {
					map[y - Ymin][x - Xmin + 1] = 0;
				}
				
				
				if ((y - Ymin + 1) >= 0 && (y - Ymin + 1) <= yDim && (x - Xmin - 1) >= 0 && (x - Xmin - 1) <= xDim) {
					map[y - Ymin + 1][x - Xmin - 1] = 0;
				} 
				if ((y - Ymin + 1) >= 0 && (y - Ymin + 1) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim) {
					map[y - Ymin + 1][x - Xmin] = 0;
				} 
				if ((y - Ymin + 1) >= 0 && (y - Ymin + 1) <= yDim && (x - Xmin + 1) >= 0 && (x - Xmin + 1) <= xDim) {
					map[y - Ymin + 1][x - Xmin + 1] = 0;
				}
				
				
			}
		}
		
		
		for (int k = 0; k < 8; k++) {

			if (isSafe(x + colNumber[k], y + rowNumber[k], visited, imageSlice, Xmin, Xmax, Ymin, Ymax, xDim, yDim)) {
				generateMap(x + colNumber[k], y + rowNumber[k], imageSlice, visited, map, Xmin, Xmax, Ymin, Ymax, type);
			}

		}

	}

	boolean isSafe(int x, int y, int visited[][], ModelImage fatImageSlice, int Xmin, int Xmax, int Ymin, int Ymax, int xDim, int yDim) {
		return ((x > 0) && (x < 512) && (y > 0) && (y < 512) && (x >= Xmin) && (x < Xmax) && (y >= Ymin) && (y < Ymax) && visited[y][x] == 0);

	}



	class Line {

		float startX, startY, endX, endY;
		float dist;
		int pointID;

		public Line(float _startX, float _startY, float _endX, float _endY, int _pointID) {
			startX = _startX;
			startY = _startY;
			endX = _endX;
			endY = _endY;
			dist = (float) Math.sqrt((endX - startX) * (endX - startX) + (endY - startY) * (endY - startY));
			pointID = _pointID;
		}

		public void setPoint(float _endX, float _endY) {
			endX = _endX;
			endY = _endY;
		}

	}

	Hashtable<Integer, Hashtable<Integer, Line>> slicesPts = new Hashtable<Integer, Hashtable<Integer, Line>>();

	void DFS(int row, int col, ModelImage fuzzyCImage, int visited[][], int xmin[], int xmax[], int ymin[], int ymax[]) {

		if (row <= 0)
			row = 10;
		if (col <= 0)
			col = 10;

		visited[row][col] = 1;

		if (row < xmin[0])
			xmin[0] = row;
		if (row > xmax[0])
			xmax[0] = row;
		if (col < ymin[0])
			ymin[0] = col;
		if (col > ymax[0])
			ymax[0] = col;

		for (int k = 0; k < 8; k++) {

			if (isSafe(row + rowNumber[k], col + colNumber[k], visited, fuzzyCImage, xmin, xmax, ymin, ymax)) {
				DFS(row + rowNumber[k], col + colNumber[k], fuzzyCImage, visited, xmin, xmax, ymin, ymax);
			}

		}

	}

	boolean isSafe(int row, int col, int visited[][], ModelImage fuzzyCImage, int xmin[], int xmax[], int ymin[], int ymax[]) {

		int xval = (int) Math.abs(xmax[0] - xmin[0]);
		int yval = (int) Math.abs(ymax[0] - ymin[0]);
		int value = (int) Math.max(xval, yval);

		double fuzzyCIntensity = fuzzyCImage.getDouble(row, col);
		return ((row >= 0) && (row < 512) && (col >= 0) && (col < 512) && visited[row][col] == 0 && fuzzyCIntensity >= 1.75 && value <= 50);

	}


	void printVector(Vector<TracingPoint> v) {
		int len = v.size();
		for (int i = 0; i < len; i++) {
			System.err.print(v.get(i).grad + " ");
		}
		System.err.println();
	}

	class TracingPoint {
		double grad;
		float posX;
		float posY;

		public TracingPoint(double _grad, float _posX, float _posY) {
			grad = _grad;
			posX = _posX;
			posY = _posY;
		}

	}

	/**
	 * Ending slice start and end indexes.
	 * 
	 * @author ruida
	 * 
	 */
	class Range {
		int startSlice;
		int endSlice;

		public Range(int _startSlice, int _endSlice) {
			startSlice = _startSlice;
			endSlice = _endSlice;
		}
	}

	class ModelString {
		String imageName;
		String voiName;

		public ModelString(String _imageName, String _voiName) {
			imageName = _imageName;
			voiName = _voiName;
		}
	}

	/**
	 * shape descriptor class.
	 * 
	 * @author ruida
	 */
	class ShapeFactor {
		public float formFactor;
		public float roundness;
		public float compactness;
		public float aspectRatio;
		public float eccentricity;
		public float area;
		public float perimeter;
		public float avgIntensity;

		public ShapeFactor(float _formFactor, float _roundness, float _compactness, float _aspectRatio, float _eccentricity, float _area, float _perimeter,
				float _avgIntensity) {
			formFactor = _formFactor;
			roundness = _roundness;
			compactness = _compactness;
			aspectRatio = _aspectRatio;
			eccentricity = _eccentricity;
			area = _area;
			perimeter = _perimeter;
			avgIntensity = _avgIntensity;
		}
	}

}
