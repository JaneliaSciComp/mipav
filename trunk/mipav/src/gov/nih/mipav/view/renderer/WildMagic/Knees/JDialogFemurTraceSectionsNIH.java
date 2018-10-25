package gov.nih.mipav.view.renderer.WildMagic.Knees;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegBSpline;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegBSpline2D;
import gov.nih.mipav.model.algorithms.registration.RegistrationMeasure;
import gov.nih.mipav.model.algorithms.registration.RegistrationMeasureCorrelationRatio;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.*;
import gov.nih.mipav.view.dialogs.JDialogRegistrationBSpline.Controls;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.*;
import gov.nih.mipav.model.structures.VOIBase;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.List;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.view.renderer.WildMagic.AAM.*;
import gov.nih.mipav.util.*;

/**
 * The class is the driver for the AAM classification. User specifies the AAM
 * trained Atlas directory and target image. The algorithm performs NMI based
 * similarity measure between each 2D slice in target image and the 2D slices
 * atlas, find the closed image, then invoke the corresponding AAM model to do
 * automatic segmentation on prostate MRI image.
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogFemurTraceSectionsNIH extends JDialogBase implements AlgorithmInterface {

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

	private int whichLeg = 0;
	private static int LEFT_LEG = 0;
	private static int RIGHT_LEG = 1;

	private JLabel labelWhichLeg;
	private JRadioButton leftLegRadio;
    private JRadioButton rightLegRadio;
	
	
	private static final String[] statsToCalculate = new String[] { VOIStatisticalProperties.areaDescription, VOIStatisticalProperties.perimeterDescription,
			VOIStatisticalProperties.avgIntensity, VOIStatisticalProperties.eccentricityDescription, VOIStatisticalProperties.majorAxisDescription,
			VOIStatisticalProperties.minorAxisDescription };

	private static final boolean[] checkList = new boolean[VOIStatisticalProperties.numberOfStatistics];

	private static final String MipavCoordinateSystem = null;

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

	private ModelImage gaussianMap;

	private static int rowNumber[] = { -1, -1, -1, 0, 0, 1, 1, 1 };
	private static int colNumber[] = { -1, 0, 1, -1, 1, -1, 0, 1 };

	private int startPt;
	private int midPt;
	private int endPt;

	private VOIVector voiVectorFinal = new VOIVector();
	VOI voiNewFinal = new VOI((short) 0, "ImageVOI");

	public static int FuzzyC = 0;
	public static int Class1 = 1;
	public static int Class2 = 2;
	public static int Class3 = 3;
	public static int GRE = 5;
	public static int Class1_weak = 6;
	public static int Class1_lowInten = 7;
	public static int Class1_corner = 8;
	public static int FuzzyC_class1 = 0;
	public static int GRE_HIGH_INTEN = 9;
	public static int CLASS3_HIGH_INTEN = 10;
	public static int GRE_GREY = 11;
	public static int Class3_LowInten = 11;
    
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

	private RegularEdgePattern edgePattern = new RegularEdgePattern();
	private HolesPattern holesPattern = new HolesPattern();
	private WeakConnectedEdgePatternHorizontal weakEdgePatternHorizontal = new WeakConnectedEdgePatternHorizontal();
	private CornerEdgePattern cornerEdgePattern = new CornerEdgePattern();
	private WeakConnectedEdgePatternVertical weakEdgePatternVertical = new WeakConnectedEdgePatternVertical();
	private RegionPattern regionPattern = new RegionPattern();

	
	private int group;
	private int GROUP_1 = 1;
	private int GROUP_2 = 2;
	private int GROUP_3 = 3;
	private int GROUP_4 = 4;
	private int GROUP_5 = 5;
	private int GROUP_6 = 6;
	private int GROUP_7 = 7;
	private int GROUP_8 = 8;
	private int GROUP_9 = 9;
	private int GROUP_10 = 10;
	
	private int group_5_endSlice;
	private int group_4_endSlice;
	private int group_3_endSlice;
	private int group_2_endSlice;
	private int group_1_endSlice;
	private int group_6_endSlice;
	private int group_7_endSlice;
	private int group_8_endSlice;
	private int group_9_endSlice;
	private int group_10_endSlice;
	
	private ViewJFrameImage thisFrame;

	private Vector3f cutOffPoint;
	
	private Vector3f cutOffPointInner;
	private Vector3f cutOffPointOuter;

	int close_length = 40;
    boolean firstTimeCheckMidShaft = false;
	
	
	/**
	 * Constructor. the parent frame
	 * 
	 * @param theParentFrame
	 */
	public JDialogFemurTraceSectionsNIH(Frame theParentFrame, ModelImage srcImage) {
		super(theParentFrame, false);
		
		thisFrame = (ViewJFrameImage)theParentFrame;
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
			if ( leftLegRadio.isSelected() ) {
				whichLeg = LEFT_LEG;
			} else if ( rightLegRadio.isSelected() ) {
				whichLeg = RIGHT_LEG;
			}
			doSegmentation();
		} else if (command.equals("quickSegOK")) {
			setVisible(false);
			doSegmentation();
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
		imageSelectionPanel.setLayout(new GridLayout(2, 3));
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

		
		// Left, right leg panel
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelWhichLeg = new JLabel("Which leg: ");
		labelWhichLeg.setFont(serif12);
		labelWhichLeg.setForeground(Color.black);
		
		imageSelectionPanel.add(labelWhichLeg, gbc);
		
		 final ButtonGroup group = new ButtonGroup();
	     leftLegRadio = new JRadioButton("LEFT LEG", true);
	     leftLegRadio.setFont(MipavUtil.font12);
	     rightLegRadio = new JRadioButton("RIGHT_LEG", false);
	     rightLegRadio.setFont(MipavUtil.font12);
	     // add to grouping
	     group.add(leftLegRadio);
	     group.add(rightLegRadio);
	     gbc.gridx = 1;
	     imageSelectionPanel.add(leftLegRadio, gbc);
	     gbc.gridx = 2;
	     imageSelectionPanel.add(rightLegRadio, gbc);

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
	public void doSegmentation() {
		
		long startTime = System.currentTimeMillis();

		gaussianMap = generateGaussianMap();

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
				float threshold = -0.13277193903923035f;

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
		// tracingDFS_mid(greImageSliceMid, fatImageSliceMid, midVOIBoundary, midVOIInner, midVOIOuter, true);
		VOILine leftLine = new VOILine();
		VOILine rightLine = new VOILine();
		
		VOILine midLeftLine = new VOILine();
		VOILine midRightLine = new VOILine();
		
		
		tracingDFS_mid_cutoffLine(greImageSliceMid, fatImageSliceMid, midVOIBoundary, midVOIInner, midVOIOuter, true, midLeftLine, midRightLine);
	
		resultVOIBoundary = (VOI) midVOIBoundary.clone();
		resultVOIInner = (VOI) midVOIInner.clone();
		resultVOIOuter = (VOI) midVOIOuter.clone();
       	
		leftLine = (VOILine)midLeftLine.clone();
		rightLine = (VOILine)midRightLine.clone();
		
		for (int i = startSlice - 1; i >= endSlice; i--) {
			// for (int i = startSlice - 1; i >= startSlice - 3; i--) {
			System.err.println("slice = " + i);

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);

			ModelImage gaussianImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(gaussianMap.getImageName(), "gsblur" + i));
			gaussianImageSlice.setFileInfo(fileInfosGRE);

			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);

				gaussianMap.exportData(i * size, size, buffer);
				gaussianImageSlice.importData(0, buffer, true);
				// new ViewJFrameImage(targetImageSlice);

			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			tracingDFS(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, i, resultVOIBoundary,
					resultVOIInner, resultVOIOuter, leftLine, rightLine);

		} // end for i loop
 	    
		
	   
		// tracingDFS_mid(greImageSliceMid, fatImageSliceMid, resultVOIBoundary,
		// resultVOIInner, resultVOIOuter, false);
		resultVOIBoundary = (VOI) midVOIBoundary.clone();
		resultVOIInner = (VOI) midVOIInner.clone();
		resultVOIOuter = (VOI) midVOIOuter.clone();

		leftLine = (VOILine)midLeftLine.clone();
		rightLine = (VOILine)midRightLine.clone();

		startSlice = midPt;
		endSlice = endPt;

		
		for (int i = startSlice + 1; i <= endSlice; i++) {
			// for (int i = startSlice + 1; i <= startSlice + 3; i++) {

			System.err.println("slice = " + i);

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);

			ModelImage gaussianImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(gaussianMap.getImageName(), "gsblur" + i));
			gaussianImageSlice.setFileInfo(fileInfosGRE);

			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);

				gaussianMap.exportData(i * size, size, buffer);
				gaussianImageSlice.importData(0, buffer, true);
				// new ViewJFrameImage(targetImageSlice);

			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			tracingDFS(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, i, resultVOIBoundary,
					resultVOIInner, resultVOIOuter, leftLine, rightLine);

		} // end for i loop
        
       
		greImage.getVOIs().removeAllElements();
		// greImage.addVOIs(voiVectorFinal);
		greImage.registerVOI(voiNewFinal);
		voiNewFinal.update();
	     
		
	    /*
		// Track back, Right Leg, for GROUP_4
		if ( whichLeg == RIGHT_LEG ) {
			traceBackRightLegGroup4(fileInfosFAT, fileInfosGRE);
			// traceBackRightLegGroup7to6(fileInfosFAT, fileInfosGRE);
		} else if ( whichLeg == LEFT_LEG ) {
			traceBackLeftLegGroup7to6(fileInfosFAT, fileInfosGRE);
			// traceBackLeftLegGroup4(fileInfosFAT, fileInfosGRE);
		}
		voiNewFinal.update(); 
	    */ 
		
		// two islands
	
	    if ( whichLeg == RIGHT_LEG ) {
	    	traceTwoIslandsOnRight(fileInfosFAT, fileInfosGRE);
	    } else if ( whichLeg == LEFT_LEG){
	    	traceTwoIslandsOnLeft(fileInfosFAT, fileInfosGRE);
	    }
	    
	    
	     
	    voiNewFinal.update();
	    
	}
	
	private void traceTwoIslandsOnLeft(FileInfoImageXML[] fileInfosFAT, FileInfoImageXML[] fileInfosGRE) {
		int[] extents = new int[3];
		extents = fatImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];
		float[] buffer = new float[size];
		// Right Leg, for GROUP_4
		int zDim = extents[2];
		Vector<VOIBase>[] vArray = voiNewFinal.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOI endSliceVOI = null;

		int startSlice = group_3_endSlice;
		int endSlice = startPt;
		
		if (vArray[startSlice].size() > 0) {
			
			VOIBase v = vArray[startSlice].get(0);
			VOIBase vTemp = (VOIBase) v.clone();
			int nPts = vTemp.size();

			System.err.println("end VOI size in initial loop:  nPts = " + nPts);
			
			// zero out the z dimension VOI
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];
			float[] zPtsZero = new float[nPts];

			vTemp.exportArrays(xPts, yPts, zPts);
			vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

			endSliceVOI = new VOI((short) 0, "blank1");
			endSliceVOI.importCurve(vTemp);

		}
		
		for (int i = startSlice - 1; i >= endSlice; i--) {
			System.err.println("slice = " + i);

			
			VOI currentSliceVOI = null;

			// System.err.println("voi size test = " + vArrayCurrent[i].size());
			
			if (vArray[i].size() > 0) {
				VOIBase v = vArray[i].get(0);
				VOIBase vTemp = (VOIBase) v.clone();
				int nPts = vTemp.size();

			    System.err.println("curent VOI size in initial loop:  nPts = " + nPts);
				// zero out the z dimension VOI
				float[] xPts = new float[nPts];
				float[] yPts = new float[nPts];
				float[] zPts = new float[nPts];
				float[] zPtsZero = new float[nPts];

				vTemp.exportArrays(xPts, yPts, zPts);
				vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

				// VOIVector voiVectorNew = new VOIVector();
				currentSliceVOI = new VOI((short) 0, "blank2");
				currentSliceVOI.importCurve(vTemp);

			}

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);

			ModelImage gaussianImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(gaussianMap.getImageName(), "gsblur" + i));
			gaussianImageSlice.setFileInfo(fileInfosGRE);

			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);

				gaussianMap.exportData(i * size, size, buffer);
				gaussianImageSlice.importData(0, buffer, true);
				
				// greImageSlice.registerVOI(endSliceVOI);
				// greImageSlice.registerVOI(currentSliceVOI);
				
				// new ViewJFrameImage(greImageSlice);

			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			traceTwoIslands(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, i, currentSliceVOI, endSliceVOI, false);

		}
	
	}
	
	private void traceTwoIslandsOnRight(FileInfoImageXML[] fileInfosFAT, FileInfoImageXML[] fileInfosGRE) {
		int[] extents = new int[3];
		extents = fatImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];
		float[] buffer = new float[size];
		// Right Leg, for GROUP_4
		int zDim = extents[2];
		Vector<VOIBase>[] vArray = voiNewFinal.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOI endSliceVOI = null;

		int startSlice = group_9_endSlice;
		int endSlice = group_10_endSlice + 4;
		
		if ( endSlice >= endPt )
			endSlice = endPt;
		
		if (vArray[startSlice].size() > 0) {
			
			VOIBase v = vArray[startSlice].get(0);
			VOIBase vTemp = (VOIBase) v.clone();
			int nPts = vTemp.size();

			System.err.println("end VOI size in initial loop:  nPts = " + nPts);
			
			// zero out the z dimension VOI
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];
			float[] zPtsZero = new float[nPts];

			vTemp.exportArrays(xPts, yPts, zPts);
			vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

			endSliceVOI = new VOI((short) 0, "blank1");
			endSliceVOI.importCurve(vTemp);

		}
		
		for (int i = startSlice + 1; i <= endSlice; i++) {
			System.err.println("slice = " + i);

			
			VOI currentSliceVOI = null;

			// System.err.println("voi size test = " + vArrayCurrent[i].size());
			
			if (vArray[i].size() > 0) {
				VOIBase v = vArray[i].get(0);
				VOIBase vTemp = (VOIBase) v.clone();
				int nPts = vTemp.size();

			    System.err.println("curent VOI size in initial loop:  nPts = " + nPts);
				// zero out the z dimension VOI
				float[] xPts = new float[nPts];
				float[] yPts = new float[nPts];
				float[] zPts = new float[nPts];
				float[] zPtsZero = new float[nPts];

				vTemp.exportArrays(xPts, yPts, zPts);
				vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

				// VOIVector voiVectorNew = new VOIVector();
				currentSliceVOI = new VOI((short) 0, "blank2");
				currentSliceVOI.importCurve(vTemp);

			}

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);

			ModelImage gaussianImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(gaussianMap.getImageName(), "gsblur" + i));
			gaussianImageSlice.setFileInfo(fileInfosGRE);

			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);

				gaussianMap.exportData(i * size, size, buffer);
				gaussianImageSlice.importData(0, buffer, true);
				
				// greImageSlice.registerVOI(endSliceVOI);
				// greImageSlice.registerVOI(currentSliceVOI);
				
				// new ViewJFrameImage(greImageSlice);

			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			traceTwoIslands(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, i, currentSliceVOI, endSliceVOI, false);
			
			int[] xBound = new int[2];
			int[] yBound = new int[2];
			int[] zBound = new int[2];
			
			endSliceVOI.getBounds(xBound, yBound, zBound);
			
			if ( (xBound[1] - xBound[0]) >= 200 || (yBound[1] - yBound[0]) >= 200 ) break;
			
		}
	
	}
	
	private void traceBackLeftLegGroup4(FileInfoImageXML[] fileInfosFAT, FileInfoImageXML[] fileInfosGRE) {
		int[] extents = new int[3];
		extents = fatImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];
		float[] buffer = new float[size];
		// Right Leg, for GROUP_4
		int zDim = extents[2];
		Vector<VOIBase>[] vArray = voiNewFinal.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOI endSliceVOI = null;

		int startSlice = group_7_endSlice;
		int endSlice = (int)(group_6_endSlice - 2);
		
		if (vArray[startSlice].size() > 0) {
			
			VOIBase v = vArray[startSlice].get(0);
			VOIBase vTemp = (VOIBase) v.clone();
			int nPts = vTemp.size();

			System.err.println("end VOI size in initial loop:  nPts = " + nPts);
			
			// zero out the z dimension VOI
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];
			float[] zPtsZero = new float[nPts];

			vTemp.exportArrays(xPts, yPts, zPts);
			vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

			endSliceVOI = new VOI((short) 0, "blank1");
			endSliceVOI.importCurve(vTemp);

		}
		
		for (int i = startSlice - 1; i >= endSlice; i--) {
			System.err.println("slice = " + i);

			
			VOI currentSliceVOI = null;

			// System.err.println("voi size test = " + vArrayCurrent[i].size());
			
			if (vArray[i].size() > 0) {
				VOIBase v = vArray[i].get(0);
				VOIBase vTemp = (VOIBase) v.clone();
				int nPts = vTemp.size();

			    System.err.println("curent VOI size in initial loop:  nPts = " + nPts);
				// zero out the z dimension VOI
				float[] xPts = new float[nPts];
				float[] yPts = new float[nPts];
				float[] zPts = new float[nPts];
				float[] zPtsZero = new float[nPts];

				vTemp.exportArrays(xPts, yPts, zPts);
				vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

				// VOIVector voiVectorNew = new VOIVector();
				currentSliceVOI = new VOI((short) 0, "blank2");
				currentSliceVOI.importCurve(vTemp);

			}

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);

			ModelImage gaussianImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(gaussianMap.getImageName(), "gsblur" + i));
			gaussianImageSlice.setFileInfo(fileInfosGRE);

			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);

				gaussianMap.exportData(i * size, size, buffer);
				gaussianImageSlice.importData(0, buffer, true);
				
				// greImageSlice.registerVOI(endSliceVOI);
				// greImageSlice.registerVOI(currentSliceVOI);
				
				// new ViewJFrameImage(greImageSlice);

			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			tracingDFSBackward(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, i, currentSliceVOI, endSliceVOI, false);

		}
	
	}
	
	private void traceBackLeftLegGroup7to6(FileInfoImageXML[] fileInfosFAT, FileInfoImageXML[] fileInfosGRE) {
		int[] extents = new int[3];
		extents = fatImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];
		float[] buffer = new float[size];
		// Right Leg, for GROUP_4
		int zDim = extents[2];
		Vector<VOIBase>[] vArray = voiNewFinal.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOI endSliceVOI = null;

		int startSlice = group_4_endSlice;
		int endSlice = (int)(group_5_endSlice + (midPt - group_5_endSlice) / 2f);
		
		if (vArray[startSlice].size() > 0) {
			
			VOIBase v = vArray[startSlice].get(0);
			VOIBase vTemp = (VOIBase) v.clone();
			int nPts = vTemp.size();

			System.err.println("end VOI size in initial loop:  nPts = " + nPts);
			
			// zero out the z dimension VOI
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];
			float[] zPtsZero = new float[nPts];

			vTemp.exportArrays(xPts, yPts, zPts);
			vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

			endSliceVOI = new VOI((short) 0, "blank1");
			endSliceVOI.importCurve(vTemp);

		}
		
		for (int i = startSlice + 1; i <= endSlice; i++) {
			System.err.println("slice = " + i);

			
			VOI currentSliceVOI = null;

			// System.err.println("voi size test = " + vArrayCurrent[i].size());
			
			if (vArray[i].size() > 0) {
				VOIBase v = vArray[i].get(0);
				VOIBase vTemp = (VOIBase) v.clone();
				int nPts = vTemp.size();

			    System.err.println("curent VOI size in initial loop:  nPts = " + nPts);
				// zero out the z dimension VOI
				float[] xPts = new float[nPts];
				float[] yPts = new float[nPts];
				float[] zPts = new float[nPts];
				float[] zPtsZero = new float[nPts];

				vTemp.exportArrays(xPts, yPts, zPts);
				vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

				// VOIVector voiVectorNew = new VOIVector();
				currentSliceVOI = new VOI((short) 0, "blank2");
				currentSliceVOI.importCurve(vTemp);

			}

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);

			ModelImage gaussianImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(gaussianMap.getImageName(), "gsblur" + i));
			gaussianImageSlice.setFileInfo(fileInfosGRE);

			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);

				gaussianMap.exportData(i * size, size, buffer);
				gaussianImageSlice.importData(0, buffer, true);
				
				// greImageSlice.registerVOI(endSliceVOI);
				// greImageSlice.registerVOI(currentSliceVOI);
				
				// new ViewJFrameImage(greImageSlice);

			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			tracingDFSBackward(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, i, currentSliceVOI, endSliceVOI, true);
			
		}
	
	}
	private void traceBackRightLegGroup7to6(FileInfoImageXML[] fileInfosFAT, FileInfoImageXML[] fileInfosGRE) {
		int[] extents = new int[3];
		extents = fatImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];
		float[] buffer = new float[size];
		// Right Leg, for GROUP_4
		int zDim = extents[2];
		Vector<VOIBase>[] vArray = voiNewFinal.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOI endSliceVOI = null;

		int startSlice = (int)(group_6_endSlice + ( group_7_endSlice - group_6_endSlice) / 2f);
		int endSlice = (int)(midPt + ( group_6_endSlice - midPt) / 2f);
		
		if (vArray[startSlice].size() > 0) {
			
			VOIBase v = vArray[startSlice].get(0);
			VOIBase vTemp = (VOIBase) v.clone();
			int nPts = vTemp.size();

			System.err.println("end VOI size in initial loop:  nPts = " + nPts);
			
			// zero out the z dimension VOI
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];
			float[] zPtsZero = new float[nPts];

			vTemp.exportArrays(xPts, yPts, zPts);
			vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

			endSliceVOI = new VOI((short) 0, "blank1");
			endSliceVOI.importCurve(vTemp);

		}
		
		for (int i = startSlice - 1; i >= endSlice; i--) {
			System.err.println("slice = " + i);

			
			VOI currentSliceVOI = null;

			// System.err.println("voi size test = " + vArrayCurrent[i].size());
			
			if (vArray[i].size() > 0) {
				VOIBase v = vArray[i].get(0);
				VOIBase vTemp = (VOIBase) v.clone();
				int nPts = vTemp.size();

			    System.err.println("curent VOI size in initial loop:  nPts = " + nPts);
				// zero out the z dimension VOI
				float[] xPts = new float[nPts];
				float[] yPts = new float[nPts];
				float[] zPts = new float[nPts];
				float[] zPtsZero = new float[nPts];

				vTemp.exportArrays(xPts, yPts, zPts);
				vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

				currentSliceVOI = new VOI((short) 0, "blank2");
				currentSliceVOI.importCurve(vTemp);

			}

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);

			ModelImage gaussianImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(gaussianMap.getImageName(), "gsblur" + i));
			gaussianImageSlice.setFileInfo(fileInfosGRE);

			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);

				gaussianMap.exportData(i * size, size, buffer);
				gaussianImageSlice.importData(0, buffer, true);
				
				// greImageSlice.registerVOI(endSliceVOI);
				// greImageSlice.registerVOI(currentSliceVOI);
				
				// new ViewJFrameImage(greImageSlice);

			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			tracingDFSBackward(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, i, currentSliceVOI, endSliceVOI, true);

		}
	
	}
	
	private void traceBackRightLegGroup4(FileInfoImageXML[] fileInfosFAT, FileInfoImageXML[] fileInfosGRE) {
		int[] extents = new int[3];
		extents = fatImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];
		float[] buffer = new float[size];
		// Right Leg, for GROUP_4
		int zDim = extents[2];
		Vector<VOIBase>[] vArray = voiNewFinal.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOI endSliceVOI = null;

		if (vArray[group_4_endSlice].size() > 0) {
			
			VOIBase v = vArray[group_4_endSlice].get(0);
			VOIBase vTemp = (VOIBase) v.clone();
			int nPts = vTemp.size();

			System.err.println("end VOI size in initial loop:  nPts = " + nPts);
			
			// zero out the z dimension VOI
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];
			float[] zPtsZero = new float[nPts];

			vTemp.exportArrays(xPts, yPts, zPts);
			vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

			endSliceVOI = new VOI((short) 0, "blank1");
			endSliceVOI.importCurve(vTemp);

		}
		
		for (int i = group_4_endSlice + 1; i <= group_5_endSlice; i++) {
			System.err.println("slice = " + i);

			
			VOI currentSliceVOI = null;

			// System.err.println("voi size test = " + vArrayCurrent[i].size());
			
			if (vArray[i].size() > 0) {
				VOIBase v = vArray[i].get(0);
				VOIBase vTemp = (VOIBase) v.clone();
				int nPts = vTemp.size();

			    System.err.println("curent VOI size in initial loop:  nPts = " + nPts);
				// zero out the z dimension VOI
				float[] xPts = new float[nPts];
				float[] yPts = new float[nPts];
				float[] zPts = new float[nPts];
				float[] zPtsZero = new float[nPts];

				vTemp.exportArrays(xPts, yPts, zPts);
				vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

				// VOIVector voiVectorNew = new VOIVector();
				currentSliceVOI = new VOI((short) 0, "blank2");
				currentSliceVOI.importCurve(vTemp);

			}

			ModelImage fatImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(fatImage.getImageName(), "fatImage" + i));
			fatImageSlice.setFileInfo(fileInfosFAT);

			ModelImage greImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(greImage.getImageName(), "target" + i));
			greImageSlice.setFileInfo(fileInfosGRE);

			ModelImage gaussianImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, makeImageName(gaussianMap.getImageName(), "gsblur" + i));
			gaussianImageSlice.setFileInfo(fileInfosGRE);

			try {
				fatImage.exportData(i * size, size, buffer);
				fatImageSlice.importData(0, buffer, true);

				greImage.exportData(i * size, size, buffer);
				greImageSlice.importData(0, buffer, true);

				gaussianMap.exportData(i * size, size, buffer);
				gaussianImageSlice.importData(0, buffer, true);
				
				// greImageSlice.registerVOI(endSliceVOI);
				// greImageSlice.registerVOI(currentSliceVOI);
				
				// new ViewJFrameImage(greImageSlice);

			} catch (IOException e) {
				e.printStackTrace();
			}

			ModelImage class1Image = imageStackFuzzyC.get(i).get(0);
			ModelImage class2Image = imageStackFuzzyC.get(i).get(1);
			ModelImage class3Image = imageStackFuzzyC.get(i).get(2);
			ModelImage fuzzyCImage = imageStackFuzzyC.get(i).get(3);

			tracingDFSBackward(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, i, currentSliceVOI, endSliceVOI, false);

		}
	
	}
	
	private void traceTwoIslands(ModelImage fatImageSlice, ModelImage fuzzyCImage, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage greImageSlice, ModelImage gaussianImageSlice, int sliceNumber, VOI currentVOI, VOI endVOI, boolean extendOuter ) {

		
		float[] xPtsOuter;
		float[] yPtsOuter;
		float[] zPtsOuter;

		float[] xPtsInner;
		float[] yPtsInner;
		float[] zPtsInner;

		Vector3f center = new Vector3f();
		float[][] sections = new float[7][2];
		
		System.err.println("trace backward: sliceNumber = " + sliceNumber);
		
		identifyGroups(sliceNumber);
		
		
		int[] xBound = new int[2];
		int[] yBound = new int[2];
		int[] zBound = new int[2];
		
		endVOI.getBounds(xBound, yBound, zBound);
		
		
		fatImageSlice.registerVOI(endVOI);
		
		VOI filteredVOI = new VOI((short) 0, "filtered-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(filteredVOI);
		
		findTwoIslands(fatImageSlice, xBound, yBound, zBound, filteredVOI);
		
		if ( filteredVOI.getSize() == 0 ) return;
		
		// fatImageSlice.getVOIs().removeAllElements();
		// fatImageSlice.addVOIs(voiVectorNew);
		
		// new ViewJFrameImage((ModelImage)fatImageSlice.clone());
		
		// *************************************** tracing ******************************************
		/* 
		VOIBaseVector current_curves = fatImageSlice.getVOIs().VOIAt(0).getCurves();
		int numCurves = current_curves.size();
		
		int[] extents = new int[3];
		extents = fatImage.getExtents();

		int size = extents[0] * extents[1];
		short[] buffer = new short[size];
		
		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];
		
		ModelImage testImage = new ModelImage(ModelStorageBase.SHORT, newExtents, makeImageName(fatImage.getImageName(), "testImage"));

		testImage.getVOIs().removeAllElements();
		
	
		BitSet paintBitmap = new BitSet();
		
		for (int curveIndex = 0; curveIndex < numCurves; curveIndex++) {

			VOIContour contour = (VOIContour)current_curves.get(curveIndex);
			Vector3f contourCenter = contour.getCenterOfMass(fatImageSlice);
			BitSet  seedPaintBitmap  = new BitSet();
			Point seedPt =   new Point((int)contourCenter.X, (int)contourCenter.Y); 
			float  fuzzyThreshold = -1.0f;
			boolean useVOI = false;
			boolean displayFuzzy =  false;
			
			float saveValue  = 10.0f;
			float less =  10.0f;
			float more = 10.0f;
			int  sizeLimit = -1;
			float maxDistance = -1.0f;
			boolean  variableThresholds  = false;
			
	        
	        AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(fatImageSlice, 1.0f, 1.0f);
	        regionGrowAlgo.setRunningInSeparateThread(false);

	        // under segment so that we do not get the blanket
	        regionGrowAlgo.regionGrow2D(seedPaintBitmap, seedPt, -1,
	                                    false, false, null,saveValue - less,
	                                    saveValue + more, -1, -1, false);
	        
	        paintBitmap.or(seedPaintBitmap);
	        
	        // make the abdominal label image from the volume BitSet determined in the region grow
	        for (int idx = 0; idx < size; idx++) {
	            if (paintBitmap.get(idx)) {
	            	buffer[idx] = 1;
	            } else {
	            	buffer[idx] = 0;
	            }
	        } // end for (int idx = 0; ...)

	        // save the sliceBuffer into the abdomenImage
	        try {
	        	testImage.importData(0, buffer, false);
	        } catch (IOException ex) {
	            System.err.println("labelAbdomen2D(): Error importing data");
	        }
	        
	
		}  // end curveIndex loop
		
		boolean wholeImage = true;
		AlgorithmMorphology2D idObjectsAlgo2D;
        int method = AlgorithmMorphology2D.ID_OBJECTS;
           
        idObjectsAlgo2D = new AlgorithmMorphology2D(testImage, 0, 0, method, 0, 0, 0, 0, wholeImage);
        idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;
        
        testImage.calcMinMax();
        AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(testImage);
        VOIExtractionAlgo.run();
        
        VOIVector voiVector = testImage.getVOIs();
        int VOIsSize = voiVector.size();
        
        VOI convertVOI = new VOI((short) 0, "convert-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorConvert = new VOIVector();
		voiVectorConvert.add(convertVOI);
        
        for ( int i = 0; i < VOIsSize; i++ ) {
        	VOI currVOI = voiVector.VOIAt(i);
        	VOIBaseVector vArray = currVOI.getCurves();
        	VOIContour current_v = (VOIContour) vArray.elementAt(0);
        	convertVOI.importCurve(current_v);
        }
        */ 
        //  testImage.getVOIs().removeAllElements();
        // testImage.addVOIs(voiVectorConvert);
        
        greImageSlice.getVOIs().removeAllElements();
        greImageSlice.addVOIs(voiVectorNew);
        
        // smoothVOI60Single(testImage, testImage);
        equalDistanceSmooth(greImageSlice);
        // smoothVOISingle(greImageSlice, greImageSlice);
        
        
        // new ViewJFrameImage(testImage);
       
    	
		VOIBaseVector current_va = greImageSlice.getVOIs().VOIAt(0).getCurves();
		Vector<VOIBase> curveList = voiNewFinal.getSliceCurves(sliceNumber);
		for (VOIBase curve : curveList) {
		    voiNewFinal.removeCurve(curve);
		}
		
		endVOI.removeCurves();
		
		for ( int i = 0; i < current_va.size(); i++ ) {
			VOIBase current_v = current_va.get(i);
			
			VOIBase vTemp = (VOIBase) current_v.clone();

			int nPtsCurrent = current_v.size();
			
		    if ( nPtsCurrent == 0 ) return;
		    
		    endVOI.importCurve(current_v);
		    
			float[] xPtsCurrent = new float[nPtsCurrent];
			float[] yPtsCurrent = new float[nPtsCurrent];
			float[] zPtsCurrent = new float[nPtsCurrent];
			current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);

			for (int j = 0; j < nPtsCurrent; j++) {
				zPtsCurrent[j] = sliceNumber;
			}

			vTemp.removeAllElements();
			vTemp.importArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent, nPtsCurrent);
			
			voiNewFinal.importCurve(vTemp);
			vTemp = null;
		}
				
	}
	
	public void equalDistanceSmooth(ModelImage greImageSlice ) {	
		
		// greImageSlice.addVOIs(voiVectorNew);
		// smoothVOI30Single(greImageSlice, greImageSlice);
		smoothVOI150Single(greImageSlice, greImageSlice);
		double constant_length = 2.5d;
		VOI resultVOI = greImageSlice.getVOIs().VOIAt(0);
		VOI tempVOI = (VOI)resultVOI.clone();
		tempVOI.removeCurves();
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(tempVOI);
		VOIBaseVector vVector = resultVOI.getCurves();
		
		for (int i = 0; i < vVector.size(); i++) {
			VOIBase v = vVector.get(i);
			double perimeter = v.getLengthPtToPt(greImageSlice.getFileInfo(0).getResolutions());

			if ( i == 0 ) {
				constant_length = 2.0d;
			} else if ( i == 1 ) {
				constant_length = 2.0d;
			}
			/*
			if (group == GROUP_1 || group == GROUP_2 || group == GROUP_9 || group == GROUP_10) {
				constant_length = 1.0d;
			} else {
				constant_length = 2.5d;
			}
			*/ 
			int numberPoints = (int) Math.round(perimeter / constant_length);

			// if (numberPoints <= 1)
			// 	return;

			float res[] = greImageSlice.getFileInfo(0).getResolutions();
			float xRes = res.length > 0 ? res[0] : 1;
			float yRes = res.length > 1 ? res[1] : 1;
			float zRes = res.length > 2 ? res[2] : 1;

			int nPts = v.size();
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];

			float[] xNew = new float[numberPoints];
			float[] yNew = new float[numberPoints];
			float[] zNew = new float[numberPoints];

			// System.err.println(" numberPoints = " + numberPoints);
			v.exportArrays(xPts, yPts, zPts);

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
				int endPt = k + 5;
				if (k + 5 > nPts) {
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

			VOIBase vTemp = (VOIBase)v.clone();
			vTemp.removeAllElements();
			vTemp.importArrays(xNew, yNew, zNew, m);
			tempVOI.importCurve(vTemp);
		}
		
		greImageSlice.getVOIs().removeAllElements();
		greImageSlice.addVOIs(voiVectorNew);
	}
	
	
	public ModelImage findTwoIslands(ModelImage image, int[] xBounds, int[] yBounds, int[] zBounds, VOI filteredVOI) {
		
		int minX = xBounds[0];
		int maxX = xBounds[1];
		int minY = yBounds[0];
		int maxY = yBounds[1];

		int top, bottom, left, right;
		int zDim;
		// int middleSlice;
		int x, y;
		float r, theta;
		int i;

		ModelImage cropImage, seedingImage, testImage;

		int[] extents = (int[]) image.getFileInfo(0).getExtents();
		image.getVOIs().get(0);

		VOI imageVOI = image.getVOIs().VOIAt(0);
    	VOIBaseVector image_va = imageVOI.getCurves();
    	
    	if ( image_va.size() == 0 ) return null;
    	
    	VOIBase image_v = (VOIBase)image_va.get(0);
		
		zDim = 1;
		
		VOI cropfilteredVOI = new VOI((short) 0, "crop-filtered-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(cropfilteredVOI);
		

		// middleSlice = (int) (zDim / 2);

		// **************************   Crop image from the VOI ***************************************
		// find the intersection of the lower bound with the VOI.
		
		int boxYmin = minY;
		int boxYmax = maxY + 10;

		int boxXmin = minX;
		int boxXmax = maxX + 10;

		xBounds[0] = boxXmin;
		xBounds[1] = boxXmax;

		yBounds[0] = boxYmin;
		yBounds[1] = boxYmax;

		zBounds[0] = 0;
		zBounds[1] = zDim;
		System.out.println("zBound[1] = " + zBounds[1]);

        // if ( (boxXmax - boxXmin) >= 10 || ( boxYmax - boxYmin ) >= 150 ) return null; 
		
		try {
			int[] destExtents = null;

			if (image.getNDims() == 2) {
				destExtents = new int[2];
				destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1;
				destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1;
			}  else {
				return null;
			}

			// Make result image
			cropImage = new ModelImage(image.getType(), destExtents,
					makeImageName(image.getImageName(), "_crop"));

			int[] xCrop = new int[] { 0, 0 };
			int[] yCrop = new int[] { 0, 0 };
			int[] zCrop = new int[] { 0, 0 };
			if (destExtents.length > 0) {
				xCrop[0] = -1 * (xBounds[0]);
				xCrop[1] = -1 * (xBounds[1] - destExtents[0] - 1);
			}
			if (destExtents.length > 1) {
				yCrop[0] = -1 * (yBounds[0]);
				yCrop[1] = -1 * (yBounds[1] - destExtents[1] - 1);
			}
			if (destExtents.length > 2) {
				zCrop[0] = -1 * (zBounds[0]);
				zCrop[1] = -1 * (zBounds[1] - destExtents[2] - 1);
			} else // 3D to 2D
			{
				zCrop[0] = -1 * (zBounds[0]);
				zCrop[1] = -1 * (zBounds[1] + 1);
			}
			AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(image,
					cropImage, xCrop, yCrop, zCrop);

			cropAlgo.addListener(this);

			// Hide the dialog since the algorithm is about to run.
			setVisible(false);
			cropAlgo.run();
			cropAlgo.finalize();
			cropAlgo = null;

		} catch (OutOfMemoryError e) {
			MipavUtil
					.displayError("Dialog Crop: unable to allocate enough memory");

			return null;
		}
		
		// new ViewJFrameImage(image);
		// new ViewJFrameImage(cropImage);    // Cheng1
		// pause();
		seedingImage = (ModelImage)cropImage.clone();
		seedingImage.getVOIs().removeAllElements();
		
		testImage = (ModelImage)cropImage.clone();
		testImage.getVOIs().removeAllElements();
		
		// new ViewJFrameImage(seedingImage);   // Cheng1
		
	    String name = makeImageName(seedingImage.getImageName(), "_AutoSeedWatershed");
        ModelImage resultImage = new ModelImage(ModelStorageBase.SHORT, seedingImage.getExtents(), name);
        float scaleX = 1.0f; 
        float scaleY = 1.0f;
        boolean mergeSimilar = false;
        float maxDistance = 0f;
        AlgorithmAutoSeedWatershed wsAlgo = new AlgorithmAutoSeedWatershed(resultImage, seedingImage, scaleX, scaleY, mergeSimilar, maxDistance);
        wsAlgo.run();
        
        // ********************************** Filter VOIs from seeding ***********************************
    	VOI measureVOI = cropImage.getVOIs().VOIAt(0);
    	VOIBaseVector measure_va = measureVOI.getCurves();
    	
    	
    	for ( int k = 0; k < measure_va.size(); k++ ) {
			VOIContour measure_v = (VOIContour)measure_va.get(k);
			
			int numVOIs = seedingImage.getVOIs().size();
			System.err.println("numVOIs = " + numVOIs);
			
			for (i = 0; i < numVOIs; i++) {
				VOI tempVOI = seedingImage.getVOIs().VOIAt(i);
				VOIBaseVector vArray = tempVOI.getCurves();
				int numContour = vArray.size();
				System.err.println("numContour = " + numContour);
		
				VOIContour current_v = (VOIContour) vArray.elementAt(0);
				Vector3f center = current_v.getCenterOfMass(seedingImage);
				boolean snear[] = new boolean[1];
				int i1[] = new int[1];
				int i2[] = new int[1];
				double distance = measure_v.pinpol(center.X, center.Y, snear, i1, i2);
				if (distance > 0) {
					
					cropfilteredVOI.importCurve(current_v);
					
					/*
					int nPtsCurrent = current_v.size();
					float[] xPtsCurrent = new float[nPtsCurrent];
					float[] yPtsCurrent = new float[nPtsCurrent];
					float[] zPtsCurrent = new float[nPtsCurrent];
					current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);
		
					for (int j = 0; j < nPtsCurrent; j++) {
						xPtsCurrent[j] += xBounds[0];
						yPtsCurrent[j] += yBounds[0];
						zPtsCurrent[j] = 0;
					}
					
					VOIBase vTemp = (VOIBase)image_v.clone();
					vTemp.removeAllElements();
					vTemp.importArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent, nPtsCurrent);
					 */ 
					// filteredVOI.importCurve(vTemp);
					
					System.err.println("find point inside VOI contour");
				}
			}
    	}
		
    	cropImage.getVOIs().removeAllElements();
		cropImage.addVOIs(voiVectorNew);
    	
    	// *********************************    tracing in cropped image **************************
    	VOIBaseVector current_curves = cropImage.getVOIs().VOIAt(0).getCurves();
		int numCurves = current_curves.size();
		
		extents = new int[3];
		extents = cropImage.getExtents();

		int size = extents[0] * extents[1];
		short[] buffer = new short[size];
		
		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];
		
		testImage = new ModelImage(ModelStorageBase.SHORT, newExtents, makeImageName(cropImage.getImageName(), "testImage"));

		testImage.getVOIs().removeAllElements();
		
	
		BitSet paintBitmap = new BitSet();
		
		for (int curveIndex = 0; curveIndex < numCurves; curveIndex++) {

			VOIContour contour = (VOIContour)current_curves.get(curveIndex);
			Vector3f contourCenter = contour.getCenterOfMass(cropImage);
			BitSet  seedPaintBitmap  = new BitSet();
			Point seedPt =   new Point((int)contourCenter.X, (int)contourCenter.Y); 
			float  fuzzyThreshold = -1.0f;
			boolean useVOI = false;
			boolean displayFuzzy =  false;
			
			float saveValue  = 10.0f;
			float less =  10.0f;
			float more = 10.0f;
			int  sizeLimit = -1;
		    maxDistance = -1.0f;
			boolean  variableThresholds  = false;
			
	        
	        AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(cropImage, 1.0f, 1.0f);
	        regionGrowAlgo.setRunningInSeparateThread(false);

	        // under segment so that we do not get the blanket
	        regionGrowAlgo.regionGrow2D(seedPaintBitmap, seedPt, -1,
	                                    false, false, null,saveValue - less,
	                                    saveValue + more, -1, -1, false);
	        
	        paintBitmap.or(seedPaintBitmap);
	        
	        // make the abdominal label image from the volume BitSet determined in the region grow
	        for (int idx = 0; idx < size; idx++) {
	            if (paintBitmap.get(idx)) {
	            	buffer[idx] = 1;
	            } else {
	            	buffer[idx] = 0;
	            }
	        } // end for (int idx = 0; ...)

	        // save the sliceBuffer into the abdomenImage
	        try {
	        	testImage.importData(0, buffer, false);
	        } catch (IOException ex) {
	            System.err.println("labelAbdomen2D(): Error importing data");
	        }
	        
	
		}  // end curveIndex loop
		
		boolean wholeImage = true;
		AlgorithmMorphology2D idObjectsAlgo2D;
        int method = AlgorithmMorphology2D.ID_OBJECTS;
           
        idObjectsAlgo2D = new AlgorithmMorphology2D(testImage, 0, 0, method, 0, 0, 0, 0, wholeImage);
        idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;
        
        testImage.calcMinMax();
        AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(testImage);
        VOIExtractionAlgo.run();
        
        VOIVector voiVector = testImage.getVOIs();
        int VOIsSize = voiVector.size();
        
        VOI convertVOI = new VOI((short) 0, "convert-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorConvert = new VOIVector();
		voiVectorConvert.add(convertVOI);
        
        for ( i = 0; i < VOIsSize; i++ ) {
        	VOI currVOI = voiVector.VOIAt(i);
        	VOIBaseVector vArray = currVOI.getCurves();
        	VOIContour current_v = (VOIContour) vArray.elementAt(0);
        	convertVOI.importCurve(current_v);
        }
        	
    
		 
    	VOIVector voiVectorNewResult = new VOIVector();
    	voiVectorNewResult.add(convertVOI);
    	testImage.getVOIs().removeAllElements();
        testImage.addVOIs(voiVectorNewResult);
        new ViewJFrameImage(testImage);
      
        if ( voiVectorNewResult.size() == 2 ) {
        	smoothVOI60DualContour(testImage, testImage);
        } else if ( voiVectorNewResult.size() == 1 ) {
        	smoothVOI30Single(testImage, testImage);
        }
        
		int numVOIs = testImage.getVOIs().size();
		System.err.println("numVOIs = " + numVOIs);
		
		for (i = 0; i < numVOIs; i++) {
			VOI tempVOI = testImage.getVOIs().VOIAt(i);
			VOIBaseVector vArray = tempVOI.getCurves();
			int numContour = vArray.size();
			if ( numContour == 0 )  return null;
			for ( int k = 0; k < numContour; k++ ) {
			 
				System.err.println("numContour = " + numContour);
		
				VOIContour current_v = (VOIContour) vArray.elementAt(k);
					
				int nPtsCurrent = current_v.size();
				float[] xPtsCurrent = new float[nPtsCurrent];
				float[] yPtsCurrent = new float[nPtsCurrent];
				float[] zPtsCurrent = new float[nPtsCurrent];
				current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);
	
				for (int j = 0; j < nPtsCurrent; j++) {
					xPtsCurrent[j] += xBounds[0];
					yPtsCurrent[j] += yBounds[0];
					zPtsCurrent[j] = 0;
				}
				
				VOIBase vTemp = (VOIBase)image_v.clone();
				vTemp.removeAllElements();
				vTemp.importArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent, nPtsCurrent);
				 
				filteredVOI.importCurve(vTemp);
			}
		
		}
	
		
        
        
        System.err.println("finish testing");
      
    	
		return cropImage;

	}
	
	private void tracingDFSBackward(ModelImage fatImageSlice, ModelImage fuzzyCImage, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage greImageSlice, ModelImage gaussianImageSlice, int sliceNumber, VOI currentVOI, VOI endVOI, boolean extendOuter ) {

		
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

	
		// findSections(currentVOI, sections, center);
		// findSections(currentVOI, sections, center);
		
		System.err.println("trace backward: sliceNumber = " + sliceNumber);
		
		identifyGroups(sliceNumber);
		
		
		if (whichLeg == RIGHT_LEG  && group >= GROUP_2 && group <= GROUP_9 ) {
			center.Y += 30;
		} else if ( whichLeg == LEFT_LEG && group >= GROUP_9 && group <= GROUP_2 ) {
			center.Y += 50;
		}
	 	
		
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

		VOI tempVOI = new VOI((short) 0, "temp-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorTemp = new VOIVector();
		voiVectorTemp.add(tempVOI);
		
		float x, y;
		float stepX, stepY;
		float posX, posY;
		int i;


		Hashtable<Integer, Line> linePoints = new Hashtable<Integer, Line>();

		// 1. get outer contour limit
		VOIBaseVector outer_va = endVOI.getCurves();
		VOIBase outer_v = (VOIBase)outer_va.get(0).clone();

		int nPtsCurrentOuter = outer_v.size();

		// System.err.println("inside trackDFSBackward nPtsCurrentOuter = " + nPtsCurrentOuter);
		
		xPtsOuter = new float[nPtsCurrentOuter];
		yPtsOuter = new float[nPtsCurrentOuter];
		zPtsOuter = new float[nPtsCurrentOuter];
		outer_v.exportArrays(xPtsOuter, yPtsOuter, zPtsOuter);		
		
		// 2. get inner contour starting point
		VOIBaseVector inner_va = currentVOI.getCurves();
		VOIBase inner_v = (VOIBase)inner_va.get(0).clone();

		int nPtsCurrentInner = inner_v.size();
		
		xPtsInner = new float[nPtsCurrentInner];
		yPtsInner = new float[nPtsCurrentInner];
		zPtsInner = new float[nPtsCurrentInner];
		inner_v.exportArrays(xPtsInner, yPtsInner, zPtsInner);

		int minPts = Math.min(nPtsCurrentOuter, nPtsCurrentInner);
		
		Vector3f pt[] = new Vector3f[minPts];

		for (i = 0; i < minPts; i++) {

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

			stepX = (xInner - center.X) / 600;
			stepY = (yInner - center.Y) / 600;

			currentStep = 600;
			
			posX = xOuter;
			posY = yOuter;
			
			if ( whichLeg == RIGHT_LEG ) {
				if ( extendOuter ) {
					posX = xOuter + 35 * stepX;
					posY = yOuter + 35 * stepY;
				} else {
					posX = xOuter + 30 * stepX;
					posY = yOuter + 30 * stepY;
				}
			} else if ( whichLeg == LEFT_LEG ) {
				if ( extendOuter ) {
					posX = xOuter + 20 * stepX;
					posY = yOuter + 20 * stepY;
				} else {
					posX = xOuter + 10 * stepX;
					posY = yOuter + 10 * stepY;
				}
			}
			
			float degree;

			Vector2f v_curr_inner_cartisian = new Vector2f(xInner, yInner);
			Vector2f v_curr_inner_polar = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(v_curr_inner_cartisian, v_curr_inner_polar, center);

			degree = v_curr_inner_polar.Y;

			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			// Trace each section differently
			Vector2f resultPos = new Vector2f(0, 0);
		
			if (degree >= section4_degree_start -15 && degree < section4_degree_end + 15) {
				// System.err.println("in section 4");
				resultPos = traceSection4Backward(posX, posY, xInner, yInner, stepX, stepY, center, degree, sliceNumber, currentStep, distCurrent, distInner, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
				
				posX = resultPos.X;
				posY = resultPos.Y;
			} else {
				posX = xInner;
				posY = yInner;
			}
		

			// final line
			pt[i] = new Vector3f(posX, posY, 0f);

		
			Line line = new Line(center.X, center.Y, posX, posY, i);
			linePoints.put(i, line);

		} // end i loop;

		// ****************************   Smooth just the condyle contour ***********************************
		
		// ******************************************   find the bottom condyle part contour ***************************************
		// for center contour, get the lower part, left part, and right part
		Vector3f[] ptsLower = new Vector3f[100];
		int lowerIndex = 0;
		Vector3f[] ptsLeft = new Vector3f[100];
		int leftIndex = 0;
		Vector3f[] ptsRight = new Vector3f[100];
		int rightIndex = 0;
		
		int nPts = linePoints.size();
		
		for (i = 0; i < nPts; i++) {
			
		    if ( pt[i].Y > cutOffPoint.Y ) {
		    	ptsLower[lowerIndex] = new Vector3f(pt[i].X, pt[i].Y, 0f);
		    	lowerIndex++;
		    } 
		    
		    if ( pt[i].Y <= ( cutOffPoint.Y ) && pt[i].X < cutOffPoint.X ) {
		    	ptsLeft[leftIndex] = new Vector3f(pt[i].X, pt[i].Y, 0f);
		    	leftIndex++;
		    }
		    
		    if ( pt[i].Y <= ( cutOffPoint.Y ) && pt[i].X > cutOffPoint.X ) {
		    	ptsRight[rightIndex] = new Vector3f(pt[i].X, pt[i].Y, 0f);
		    	rightIndex++;
		    }
		}
		
		System.err.println("leftIndex = " + leftIndex + "  rightIndex = " + rightIndex);
		
		float sumX = 0f, sumY = 0f;
		// center contour,  find new center on the bottom condyle 
		for ( i = 0; i < lowerIndex; i++ ) {
			sumX += ptsLower[i].X;
			sumY += ptsLower[i].Y;
		}
		Vector3f condyleCenter = new Vector3f( (sumX / (float)lowerIndex), (sumY / (float)lowerIndex), 0);
		System.err.println("condyleCenter.X = " + condyleCenter.X + "  condyleCenter.Y = " + condyleCenter.Y);
		
		
		// *********************  Smooth bottom part condyle *************************
		float leftCondylePtX = 512, leftCondylePtY = 512;
		float rightCondylePtX = 512, rightCondylePtY = 512;
		float degreeLeftCondylePt = 0, degreeRightCondylePt = 0;
		
		// Vector3f[] pt = new Vector3f[ptList.size()];
		for ( i = 0; i < lowerIndex ; i++ ) {
			
			// pt[i] = ptList.get(i);
			
			if ( ptsLower[i].X < condyleCenter.X && ptsLower[i].Y < leftCondylePtY ) {
					leftCondylePtY = ptsLower[i].Y;
					leftCondylePtX = ptsLower[i].X;
			}
			
			if ( ptsLower[i].X > condyleCenter.X && ptsLower[i].Y < rightCondylePtY ) {
					rightCondylePtY = ptsLower[i].Y;
					rightCondylePtX = ptsLower[i].X;
			}	
		}
		
		System.err.println("condyleCenter.X = " + condyleCenter.X + "  condyleCenter.Y = " + condyleCenter.Y);
		System.err.println("leftCondylePtX = " + leftCondylePtX +  "  leftCondylePtY = " + leftCondylePtY);
		System.err.println("rightCondylePtX = " + rightCondylePtX +  "  rightCondylePtY = " + rightCondylePtY);
		
		Vector2f cartesianInLeftCondylePt = new Vector2f(leftCondylePtX, leftCondylePtY);
		Vector2f polarOutLeftCondylePt = new Vector2f();
		MipavCoordinateSystems.CartesianToPolar2D(cartesianInLeftCondylePt, polarOutLeftCondylePt, condyleCenter);
		degreeLeftCondylePt = polarOutLeftCondylePt.Y;
		System.err.println("degreeLeftCondylePt = " + degreeLeftCondylePt);
		
		Vector2f cartesianInRightCondylePt = new Vector2f(rightCondylePtX, rightCondylePtY);
		Vector2f polarOutRightCondylePt = new Vector2f();
		MipavCoordinateSystems.CartesianToPolar2D(cartesianInRightCondylePt, polarOutRightCondylePt, condyleCenter);
		degreeRightCondylePt = polarOutRightCondylePt.Y;
		System.err.println("degreeRightCondylePt = " + degreeRightCondylePt);
		
		resultVOI.importCurve(ptsLower);
		greImageSlice.addVOIs(voiVectorNew);
		smoothVOI150Single(greImageSlice, greImageSlice);
		double constant_length = 2.5d;
		resultVOI = greImageSlice.getVOIs().VOIAt(0);
		VOIBaseVector vVector = resultVOI.getCurves();
		VOIBase vT = vVector.get(0);
		double perimeter = vT.getLengthPtToPt(greImageSlice.getFileInfo(0).getResolutions());
		int numberPoints = (int) Math.round(perimeter / constant_length);

		float res[] = greImageSlice.getFileInfo(0).getResolutions();
		float xRes = res.length > 0 ? res[0] : 1;
		float yRes = res.length > 1 ? res[1] : 1;
		float zRes = res.length > 2 ? res[2] : 1;

		resultVOI = greImageSlice.getVOIs().VOIAt(0);
		Vector<VOIBase>[] vArray = resultVOI.getSortedCurves(VOIBase.ZPLANE, 1);
		VOIBase result_v = vArray[0].get(0);
		int nPt = result_v.size();
		float[] xPt = new float[nPt];
		float[] yPt = new float[nPt];
		float[] zPt = new float[nPt];

		float[] xNew = new float[numberPoints];
		float[] yNew = new float[numberPoints];
		float[] zNew = new float[numberPoints];

		result_v.exportArrays(xPt, yPt, zPt);
		
		// equal distance interpolation
		float x1, y1, x2, y2, distance;
		int m = 0;
		int k = 0;
		boolean find = false;

		while (k < nPt - 5) {
			x1 = xPt[k];
			y1 = yPt[k];
			find = false;
			for (int j = k + 1; j < k + 6; j++) {
				x2 = xPt[j];
				y2 = yPt[j];
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
		
		while (k < nPt) {
			x1 = xPt[k];
			y1 = yPt[k];
			find = false;
			int endPt = k + 6;
			if (k + 6 > nPt) {
				endPt = nPt;
			}
			for (int j = k + 1; j < endPt; j++) {
				x2 = xPt[j];
				y2 = yPt[j];
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
		
		Vector<Vector3f> ptList = new Vector<Vector3f>();
		int linePointsIndex = 0;
		ptList.clear();
		linePoints.clear();
		int z;
		for (z = 0; z < m; z++) {
			
			posX = xNew[z];
			posY = yNew[z];
			
			Vector2f cartesianIn = new Vector2f(posX, posY);
			Vector2f polarOut = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(cartesianIn, polarOut, condyleCenter);
			float degreeCurrPt = polarOut.Y;
			
			if ( degreeCurrPt > degreeLeftCondylePt && degreeCurrPt < degreeRightCondylePt ) {
				continue;
			}
			
			Line line = new Line(condyleCenter.X, condyleCenter.Y, posX, posY, linePointsIndex);
			ptList.add(new Vector3f(posX, posY, 0f));
			linePoints.put(linePointsIndex, line);
			linePointsIndex++;
		}
	    
		ptList.add(new Vector3f(leftCondylePtX, leftCondylePtY, 0f));
		Line lineLeft = new Line(condyleCenter.X, condyleCenter.Y, leftCondylePtX, leftCondylePtY, linePointsIndex);
		linePoints.put(linePointsIndex, lineLeft);
		linePointsIndex++;
		
		ptList.add(new Vector3f(rightCondylePtX, rightCondylePtY, 0f));
		Line lineRight = new Line(condyleCenter.X, condyleCenter.Y, rightCondylePtX, rightCondylePtY, linePointsIndex);
		linePoints.put(linePointsIndex, lineRight);
		linePointsIndex++;
		
		// ***************************************   Add left and right lines ***************************
	    for ( i = 0; i < leftIndex; i++ ) {
	    	ptList.add(new Vector3f(ptsLeft[i].X, ptsLeft[i].Y, 0f));
			Line leftLine = new Line(condyleCenter.X, condyleCenter.Y, ptsLeft[i].X, ptsLeft[i].Y, linePointsIndex);
			linePoints.put(linePointsIndex, leftLine);
			linePointsIndex++;
	    }
	    
	    
	    for ( i = 0; i < rightIndex; i++ ) {
	    	ptList.add(new Vector3f(ptsRight[i].X, ptsRight[i].Y, 0f));
			Line rightLine = new Line(condyleCenter.X, condyleCenter.Y, ptsRight[i].X, ptsRight[i].Y, linePointsIndex);
			linePoints.put(linePointsIndex, rightLine);
			linePointsIndex++;
	    }
		
	    // **********************************  Re-orient the points according to polar degree ***********************
		Hashtable<Float, Line> tempLinePoints = new Hashtable<Float, Line>();
		int index = 0;
		float centerX, centerY;
		float degree;
		int ptsSize = ptList.size();
		for ( i = 0; i < ptsSize; i++ ) {
			
			Line line = linePoints.get(i);
			Vector3f point = ptList.get(i);
			
			centerX = line.startX;
			centerY = line.startY;
			
			posX = line.endX;
			posY = line.endY;
			
			Vector2f cartesianIn = new Vector2f(posX, posY);
			Vector2f polarOut = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(cartesianIn, polarOut, center);
			degree = polarOut.Y;
			
			tempLinePoints.put(degree, new Line(center.X, center.Y, posX, posY, index));
			index++;
		}
		
		linePoints.clear();
		ptList.clear();
		index = 0;
		
		ArrayList<Float> temp = Collections.list(tempLinePoints.keys());
		Collections.sort(temp);
		Iterator<Float> it = temp.iterator();
		
		while ( it.hasNext() ) {
			float key = it.next();
			Line line = tempLinePoints.get(key);
			if ( line != null ) {
				linePoints.put(index, line);
				// pt[index] = new Vector3f(line.endX, line.endY, 0);
				ptList.add(new Vector3f(line.endX, line.endY, 0));
				index++;
			}
		}
	
		linePoints.clear();
		ptsSize = ptList.size();
		Vector3f[] pts = new Vector3f[ptsSize];
		for (k = 0; k < ptsSize; k++ ) {
			pts[k] = ptList.get(k);
			posX = pts[k].X;
			posY = pts[k].Y;
			Line line = new Line(center.X, center.Y, posX, posY, z);
			linePoints.put(z, line);
		}
		
		slicesPts.put(sliceNumber, linePoints);

		// generate inner and outer contours
		findBoundingContour(sliceNumber, center, fatImageSlice, endVOI);
		resultVOI.removeCurves();
		
		greImageSlice.getVOIs().removeAllElements();
		greImageSlice.getVOIs().removeAllElements();
		fuzzyCImage.getVOIs().removeAllElements();
		class1Image.getVOIs().removeAllElements();
		class2Image.getVOIs().removeAllElements();
		
		voiVectorNew.removeAllElements();
		voiVectorNew.add(resultVOI);
		resultVOI.importCurve(pts);
		
		greImageSlice.addVOIs(voiVectorNew);
		fuzzyCImage.addVOIs(voiVectorNew);
		class1Image.addVOIs(voiVectorNew);
		class2Image.addVOIs(voiVectorNew);
		
		// emergency 
		// new ViewJFrameImage(greImageSlice);
		// new ViewJFrameImage(class1Image);
		// new ViewJFrameImage(class2Image);
		// new ViewJFrameImage(class3Image);
		// new ViewJFrameImage(fuzzyCImage);
		
		VOIBaseVector current_va = greImageSlice.getVOIs().VOIAt(0).getCurves();
		if (current_va.size() > 0) {
			VOIBase current_v = current_va.get(0);

			VOIBase vTemp = (VOIBase) current_v.clone();

			int nPtsCurrent = current_v.size();
			
			if ( nPtsCurrent == 0 ) return;
			
			float[] xPtsCurrent = new float[nPtsCurrent];
			float[] yPtsCurrent = new float[nPtsCurrent];
			float[] zPtsCurrent = new float[nPtsCurrent];
			current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);

			for (int j = 0; j < nPtsCurrent; j++) {
				zPtsCurrent[j] = sliceNumber;
			}

			vTemp.removeAllElements();
			vTemp.importArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent, nPtsCurrent);
			Vector<VOIBase> curveList = voiNewFinal.getSliceCurves(sliceNumber);
	        for (VOIBase curve : curveList) {
	            voiNewFinal.removeCurve(curve);
	        }
			voiNewFinal.importCurve(vTemp);
			vTemp = null;
		}
		

		
	}

	
	private Vector2f traceSection4Backward(float posX, float posY, float stopX, float stopY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber, int currentStep,
			float distCurrent, float distInner, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		boolean findBoundary = false;
		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
		// System.err.println("trace backward");

		while ( distCurrent > distInner) {

			// walk around holes
			if (fuzzyCIntensity == 1 ) {
				findBoundary = true;
				break;
			}
		
			posX -= stepX;
			posY -= stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop
		
		if ( findBoundary ) {
			// System.err.println("find boundary");
			return new Vector2f(posX, posY);
		} else {
			return new Vector2f(stopX, stopY);
		}
	}
	
	private void findBoundingContour(int sliceNumber, Vector3f center, ModelImage fatImageSlice, VOI endVOI) {

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

		VOI section5_lower = new VOI((short) 4, "line5lower", VOI.LINE, 0);
		section5_lower.setColor(Color.yellow);
		voiVectorLines.add(section5_lower);
		
		VOI section5_upper = new VOI((short) 5, "line5upper", VOI.LINE, 0);
		section5_upper.setColor(Color.lightGray);
		voiVectorLines.add(section5_upper);

		VOI section6 = new VOI((short) 6, "line6", VOI.LINE, 0);
		section6.setColor(Color.cyan);
		voiVectorLines.add(section6);


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
			/// distanceInner = distance * (1d - 0.10d);
			distanceInner = distance * ( 1d - 0.10d );

			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(center.X, center.Y, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));
		
			
			if ((degree >= section1_degree_start && degree < section1_degree_end)) {
				section1.importCurve(kLine);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				section2.importCurve(kLine);
			} else if ((degree >= section3_degree_start && degree < section3_degree_end)) {
				section3.importCurve(kLine);
			} else if ((degree >= section4_degree_start && degree < section4_degree_end)) {
				section4.importCurve(kLine);
			} else if (degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end) {
				section5_lower.importCurve(kLine);
			} else if (degree >= section5_degree_upperHalf_start && degree < section5_degree_upperHalf_end) {
				section5_upper.importCurve(kLine);
			} else if (degree >= section6_degree_start && degree < section6_degree_end) {
				section6.importCurve(kLine);
			}

			pt_boundary[i] = new Vector3f(posX, posY, 0f);

			/*
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
            */
		} // end i loop;

		endVOI.removeCurves();
		endVOI.importCurve(pt_boundary);
		
		fatImageSlice.addVOIs(voiVectorLines); // cheng
		// new ViewJFrameImage(fatImageSlice);

		// ModelImage cloneImage = (ModelImage) fatImageSlice.clone();
		// VOIVector voiVectorInOut = new VOIVector();
		// voiVectorInOut.add(resultVOIInner);
		// voiVectorInOut.add(resultVOIOuter);
		// cloneImage.addVOIs(voiVectorInOut);
		// new ViewJFrameImage(cloneImage);
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

		numIterations = 20;
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

	public void smoothVOI30Single(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 30, false);
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

	public void smoothVOISingle(ModelImage maskImage, ModelImage resultImage, int nPts) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), nPts, false);
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
	
	
	public void smoothVOISingle(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		
		int nPts = 40; 
		
		
			v.VOIAt(0).setActive(true);
			v.VOIAt(0).setAllActive(true);
	
			// new ViewJFrameImage(maskImage);
			try {
					
				AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), nPts, false);
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
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 30, false);
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

			smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(1), 30, false);
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

		// new ViewJFrameImage(greImageSlice);   // Cheng1

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

		// new ViewJFrameImage(fuzzyCImage);                 // Cheng1
		// new ViewJFrameImage(class1Image);
		// new ViewJFrameImage(class2Image);
		// new ViewJFrameImage(class3Image);
		// new ViewJFrameImage(gaussianImageSlice);
		// new ViewJFrameImage(greImageSlice);

		return resultVOI.getGeometricCenter();
	}

	private Vector2f traceSection6(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;
		double greIntensity;
        int holesDiameter = 15;
		
		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
		 if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
				) { 
         	holesDiameter = 10;
         } else {
         	holesDiameter = 10;
         }
         	
		
		while (fuzzyCIntensity != 3  && fuzzyCIntensity != 2 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
		
			
		    if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group < GROUP_8 ) )
				) {
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
							continue;
						}
						break;
					}
					

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
							|| class2Intensity >= 0.9f || class3Intensity >= 0.04 ) {
						
						boolean findHolesClass3 = false;
						findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 15, 0, Class3);
						if (findHolesClass3 ) {
							// skip holes
							while ((class3Intensity >= 0.04) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								class3Intensity = class3Image.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
							continue;
						}
						
						break;
					}
				
					posX += stepX;
					posY += stepY;
		
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
		
		   

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			if ( class2Intensity >= 0.9f || class1Intensity < 0.1f  || class3Intensity >= 0.04 ) {
				boolean findHolesClass3 = false;
				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 10, 0, Class3);
				if (findHolesClass3 ) {
					// skip holes
					while ((class3Intensity >= 0.04) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				break;
			}
		 
		    
		    boolean findGreyRegionPattern = false;
		    findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
		    if ( findGreyRegionPattern ) {
		    	continue;
		    }
		    
		    
			boolean findBlackRegionPattern = false;
			findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE);

			if ( findBlackRegionPattern ) {
				
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				
				if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) break;
				if ( greIntensity >= 90 ) break;
				if (class3Intensity >= 0.04) break;
				boolean findEdgeOnGRE = false;
				while ((class3Intensity <= 0.04) && distCurrent < distOuter) {
					posX += stepX;
					posY += stepY;

					findEdgeOnGRE = findEdgeOnImage((int)posX, (int) posY, greImageSlice, 5, 50, GRE_HIGH_INTEN);
					
					if ( findEdgeOnGRE ) break;
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}

					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					
					if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) break;
					
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}
				
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				greIntensity = greImageSlice.getDouble((int) posX, (int) posY);

				if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) break;
				
				if ( greIntensity >= 90 ) break;
				
				if ( class2Intensity >= 0.1f || class1Intensity < 0.1f || class3Intensity >= 0.04 ) {
					break;
				}
				
			}
		
			
			

			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			boolean findWeakEdge = false;
			boolean findWeakEdgeClass3 = false;
			boolean findWeakEdge7 = false;
			int[] xResult = new int[1];
			xResult[0] = -1;
			findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 7, FuzzyC, true, xResult);
			findWeakEdge7 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 7, FuzzyC, true, xResult);
			findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 7, Class3, true, xResult);

			float testX = posX, testY = posY;
			boolean lookFor3 = false;

			if (findWeakEdge) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {

					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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
			} else if (findWeakEdgeClass3) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {

					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.04f) {
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

			} else if (findWeakEdge7) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {

					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.04f) {
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
			}

			
			// arbitrary testing for fuzzyC
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
					boolean findHolesFuzzyC = false;
					findHolesFuzzyC = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
					if (findHolesFuzzyC ) {
						// skip holes
						while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
		
			
		        /*
				// arbitrary testing on class 1 and class 2
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04) {
					boolean findHolesClass1 = false;
					findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, holesDiameter, 0, Class1);
					boolean findHolesClass2 = false;
					findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, holesDiameter, 0, Class2);
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, holesDiameter, 0, Class3);
					if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
						// skip holes
						while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							
							class1Intensity = class1Image.getDouble((int) posX, (int) posY);
							class2Intensity = class2Image.getDouble((int) posX, (int) posY);
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
				*/ 
		
		    /*
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				break;
			}
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
					|| class2Intensity >= 0.9f || class3Intensity >= 0.04f) {
				break;
			}
			*/ 
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection1(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;
		double greIntensity;
		
		int holesDiameter = 10;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		greIntensity = greImageSlice.getDouble((int) posX, (int) posY);

		

        if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
				( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
			) { 
        	holesDiameter = 10;
        } else {
        	holesDiameter = 10;
        }
        	
		
		while (fuzzyCIntensity != 3 && fuzzyCIntensity != 2 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			/*
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04)
				break;
            */ 
			
			if ((whichLeg == RIGHT_LEG && (group <= GROUP_3 || group >= GROUP_8)) || 
					(whichLeg == LEFT_LEG && (group >= GROUP_3 || group < GROUP_8))) {
				while (distCurrent < distOuter) {

					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					/*
					 * findGreyRegionPattern = false; findGreyRegionPattern =
					 * findGreyRegionOnImage((int) posX, (int) posY,
					 * greImageSlice, 7, 0, GRE_GREY); if (
					 * findGreyRegionPattern ) { continue; }
					 */

					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;

								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 /* || 
							class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04f */ )
						break;

					posX += stepX;
					posY += stepY;

					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}
				break;
			}


			boolean findGreyRegionPattern = false;
			findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE_GREY);
			if (findGreyRegionPattern) {
				continue;
			}

			boolean findBlackRegionPattern = false;
			findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE);

			if (findBlackRegionPattern) {
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
				if ( greIntensity >= 90 )
					break;
				if (class3Intensity >= 0.01)
					break;
				boolean findEdgeOnGRE = false;
				while ((class3Intensity < 0.01) && distCurrent < distOuter) {
					posX += stepX;
					posY += stepY;

					findEdgeOnGRE = findEdgeOnImage((int) posX, (int) posY, greImageSlice, 5, 50, GRE_HIGH_INTEN);

					if (findEdgeOnGRE)
						break;

					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}

					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}

				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				greIntensity = greImageSlice.getDouble((int) posX, (int) posY);
				
				if ( greIntensity >= 90 ) break;

				if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.01)
				break;
			}


            fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
			 		|| class2Intensity >= 0.9f || class3Intensity >= 0.04f ) {
				boolean findHolesClass3 = false;
				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 15, 0, Class3);
				if (findHolesClass3 ) {
					// skip holes
					while ((class3Intensity >= 0.04) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				break;
			}

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			boolean findWeakEdge = false;
			boolean findWeakEdgeClass3 = false;
			boolean findWeakEdge7 = false;
			int[] xResult = new int[1];
			xResult[0] = -1;
			findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 7, FuzzyC, true, xResult);
			findWeakEdge7 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 7, FuzzyC, true, xResult);
			findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 7, Class3, true, xResult);

			float testX = posX, testY = posY;
			boolean lookFor3 = false;

			if (findWeakEdge) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {

					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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
			} else if (findWeakEdgeClass3) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {

					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.04f) {
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

			} else if (findWeakEdge7) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {

					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.04f) {
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
			}

			/*
			boolean findWeakEdge3 = false;
			findWeakEdge7 = false;
			xResult = new int[1];
			xResult[0] = -1;
			findWeakEdge3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
			findWeakEdge7 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 15, FuzzyC, true, xResult);
			testX = posX;
			testY = posY;
			lookFor3 = false;

			if (findWeakEdge3) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {

					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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
			} else {
				if (findWeakEdge7) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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

			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHolesFuzzyC = false;
				findHolesFuzzyC = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
				if (findHolesFuzzyC) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;

						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				break;
			}

			// arbitrary testing on class 1 and class 2

			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04f) {
				boolean findHolesClass1 = false;
				findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, holesDiameter, 0, Class1);
				boolean findHolesClass2 = false;
				findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, holesDiameter, 0, Class2);
				boolean findHolesClass3 = false;
				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, holesDiameter, 0, Class3);
				if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
					// skip holes
					while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;

						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						class1Intensity = class1Image.getDouble((int) posX, (int) posY);
						class2Intensity = class2Image.getDouble((int) posX, (int) posY);
						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3)
				break;

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04f)
				break;

			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection2(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {
		
		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && fuzzyCIntensity != 2 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			
            if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group < GROUP_8 ) )
				) {
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
                    /*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3 )
						break;
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3 )
						break;
					
					posX += stepX;
					posY += stepY;
			
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
         // walk around holes
         			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
         			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
         				boolean findHoles = false;
         				// find holes on fuzzyC image
         				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
         				if (findHoles) {
         					// skip holes
         					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
         						posX += stepX;
         						posY += stepY;
         						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
         						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
         					}
         					continue;
         				}
         				break;
         			}


    			    fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
    				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
    				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
    				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
    			
    				if (whichLeg == LEFT_LEG ) {
    					if ( group == GROUP_6 && ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) )
    						break;
    					else if ( group == GROUP_7 && fuzzyCIntensity == 3)
    						break;
    					else if ( group == GROUP_8 && fuzzyCIntensity == 3)
    						break;
    				}
    				
    				
    			
        			
        			if ( class3Intensity >= 0.04f ) {
        				boolean findHolesClass3 = false;
        				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 15, 0, Class3);
        				if (findHolesClass3 ) {
        					// skip holes
        					while ((class3Intensity >= 0.04) && distCurrent < distOuter) {
        						posX += stepX;
        						posY += stepY;
        						
        						if (posX <= 0) {
        							posX = 5;
        							break;
        						}
        						if (posY <= 0) {
        							posY = 5;
        							break;
        						}

        						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
        						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
        					}
        					continue;
        				}
        				break;

        			}

         			
         			boolean findGreyRegionPattern = false;
                     findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE_GREY);
                     if ( findGreyRegionPattern ) {
                     	continue;
                     }
                     
		
            boolean findBlackRegionPattern = false;
            findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE);
			
			if ( findBlackRegionPattern ) {
				
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class3Intensity >= 0.09) break;
				boolean findEdgeOnGRE = false;
				while ((class3Intensity <= 0.09) && distCurrent < distOuter) {
					posX += stepX;
					posY += stepY;

					findEdgeOnGRE = findEdgeOnImage((int)posX, (int) posY, greImageSlice, 5, 50, GRE_HIGH_INTEN);
					
					if ( findEdgeOnGRE ) break;
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}

					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}
				
				break;
			}
					
			
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			

				boolean findWeakEdge = false;
				boolean findWeakEdgeClass2 = false;
				boolean findWeakEdgeClass3 = false;
				int[] xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 7, FuzzyC, true, xResult);
				findWeakEdgeClass2 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class2Image, 7, Class2, false, xResult);
				findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 7, Class3, false, xResult);

				float testX = posX, testY = posY;
				boolean lookFor3 = false;

				if (findWeakEdge || findWeakEdgeClass2) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3  || fuzzyCIntensity == 2 ) {
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
				if (degree >= section2_degree_end - 20 && degree <= section2_degree_end) {
					if (findWeakEdgeClass3) {
						testX = posX;
						testY = posY;
						lookFor3 = false;
						while (distCurrent < distOuter) {

							if (testX <= 0) {
								testX = 5;
								break;
							}
							if (testY <= 0) {
								testY = 5;
								break;
							}

							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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
				/*
				boolean findWeakEdge3 = false;
				boolean findWeakEdge7 = false;
				xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
				findWeakEdge7 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 15, FuzzyC, true, xResult);
				testX = posX;
				testY = posY;
				lookFor3 = false;

				if (findWeakEdge3) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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
				} else {
					if (findWeakEdge7) {
						testX = posX;
						testY = posY;
						lookFor3 = false;
						while (distCurrent < distOuter) {

							if (testX <= 0) {
								testX = 5;
								break;
							}
							if (testY <= 0) {
								testY = 5;
								break;
							}

							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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
                
				boolean findEdgeOnClass1 = false;
				boolean findLowIntenClass1 = false;
				boolean findEdgeOnClass2 = false;
				boolean findEdgeOnClass3 = false;
				findEdgeOnClass1 = findEdgeOnImage((int) posX, (int) posY, class1Image, 7, 50, Class1);
				findLowIntenClass1 = findEdgeOnImage((int) posX, (int) posY, class1Image, 7, 50, Class1_lowInten);
				findEdgeOnClass2 = findEdgeOnImage((int) posX, (int) posY, class2Image, 7, 50, Class2);
				findEdgeOnClass3 = findEdgeOnImage((int) posX, (int) posY, class3Image, 7, 50, Class3);
				if (findEdgeOnClass1 || findLowIntenClass1 || findEdgeOnClass2 || findEdgeOnClass3) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
                        /*
					    if (  whichLeg == LEFT_LEG && group == GROUP_8 && degree <= section2_degree_end && degree > section2_degree_end - 30  ) { 
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.02f) {
								lookFor3 = true;
								break;
							}
					    } else {
					    	if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
								lookFor3 = true;
								break;
							}
					    }
					    */ 
						if (fuzzyCIntensity == 3 ) {
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

					// break;
				}

				// arbitrary testing on class 1 and class 2
                
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.9f || class1Intensity < 0.1f  || class3Intensity >= 0.09  ) {
					boolean findHolesClass1 = false;
					findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, 25, 0, Class1);
					boolean findHolesClass2 = false;
					findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, 25, 0, Class2);
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 25, 0, Class3);
					if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
						// skip holes
						while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.09) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;

							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}

							class1Intensity = class1Image.getDouble((int) posX, (int) posY);
							class2Intensity = class2Image.getDouble((int) posX, (int) posY);
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
			     
		
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		} // end while loop

		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection3(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			

			if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group < GROUP_8 ) )
				) {
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
					*/ 
					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					if (fuzzyCIntensity == 2 ||  fuzzyCIntensity == 3 ||  class1Intensity <= 0.1f 
							|| class2Intensity >= 0.9f )
						break;
					
					posX += stepX;
					posY += stepY;
					
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
            
			
			boolean findGreyRegionPattern = false;
            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
            if ( findGreyRegionPattern ) {
            	continue;
            }
            

			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			// walk around holes
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) { // skip holes
					while (( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
			}


			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if ( fuzzyCIntensity == 3 ) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) { // skip holes
					while ( fuzzyCIntensity == 3 && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				break;
			}
            
            
			
			// arbitrary testing on class 1 and class 2
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.9f || class1Intensity < 0.1f ) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) { // skip holes
					while (( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				break;
			}
			
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if ( fuzzyCIntensity == 3 ) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) { // skip holes
					while ( fuzzyCIntensity == 3 && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				break;
			}
            

		     /*
			// arbitrary testing on class 1 and class 2
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04) {
				boolean findHolesClass1 = false;
				findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, 25, 0, Class1);
				boolean findHolesClass2 = false;
				findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, 25, 0, Class2);
				boolean findHolesClass3 = false;
				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 25, 0, Class3);
				if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
					// skip holes
					while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						class1Intensity = class1Image.getDouble((int) posX, (int) posY);
						class2Intensity = class2Image.getDouble((int) posX, (int) posY);
						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				// break;
			}
			*/
             
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		} // end while loop
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection4(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			
        	if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group < GROUP_8 ) )
				){
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					/* Cheng1
					if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3  || class1Intensity <= 0.1f  ||  class2Intensity >= 0.9f )
						break;
					if ( fuzzyCIntensity == 3  )
						break;
					*/ 
					
					if ((whichLeg == RIGHT_LEG && (group < GROUP_2 || group > GROUP_9)) || (whichLeg == LEFT_LEG && (group > GROUP_2 || group < GROUP_9))) {
						if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f || class2Intensity >= 0.9f)
							break;
					} else {
						if (fuzzyCIntensity == 3)
							break;
					}
					
					posX += stepX;
					posY += stepY;
				
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			// walk around holes
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				 
				if (findHoles) { // skip holes
					
							while (( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
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

			if (whichLeg == LEFT_LEG ) {
			 	if ( group == GROUP_6 && fuzzyCIntensity == 2)
			 		break;
			 	else if ( group == GROUP_7 && fuzzyCIntensity == 3)
			 		break;
			}
		
			// if ( fuzzyCIntensity == 3 )  break;
			
        	boolean findGreyRegionPattern = false;
            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
            if ( findGreyRegionPattern ) {
            	continue;
            }
            
        	

			if ( class3Intensity >= 0.04f ) {
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
					boolean findHoles = false;
					// find holes on fuzzyC image
					findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
					if (findHoles) {
						// skip holes
						while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}

							
							fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					} else {
						break;
					}
				}

			}
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			// Cheng1
		
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			if ((whichLeg == RIGHT_LEG && (group < GROUP_2 || group > GROUP_9)) ||
					(whichLeg == LEFT_LEG && (group > GROUP_2 || group < GROUP_9))) {
				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f || class2Intensity >= 0.9f)
					break;
			} else {
				if (fuzzyCIntensity == 3)
					break;
			}
		    

			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop
		
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection5_upper(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
		
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
			 		|| class2Intensity >= 0.9f || class3Intensity >= 0.09f ) {
					boolean findHoles = false;
					// find holes on fuzzyC image
					findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 10, 0, FuzzyC);
					if (findHoles) {
						// skip holes
						while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}

							
							fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					} else {
						break;
					}
			}
			  
			
            if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group < GROUP_8 ) )
				) {
				
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					
					
					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					// Cheng1
					if ( (whichLeg == RIGHT_LEG && ( group < GROUP_2 || group > GROUP_9 ))  ||
						 (whichLeg == LEFT_LEG && ( group > GROUP_2 || group < GROUP_9)) ) {
						if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3 )
							break;
					} else {
						if ( fuzzyCIntensity == 3 )
							break;
					}

					posX += stepX;
					posY += stepY;
				
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}

			// walk around holes
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
				if (findHoles) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
			}
			
			
			
			 boolean findGreyRegionPattern = false;
			    findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
			    if ( findGreyRegionPattern ) {
			    	continue;
			    }
				
			    
			
            fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
			 		|| class2Intensity >= 0.9f || class3Intensity >= 0.04f ) {
				boolean findHolesClass3 = false;
				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 15, 0, Class3);
				if (findHolesClass3 ) {
					// skip holes
					while ((class3Intensity >= 0.04) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				break;
			}
           
            
			if ( class3Intensity >= 0.09f ) {
					boolean findHoles = false;
					// find holes on fuzzyC image
					findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 10, 0, FuzzyC);
					if (findHoles) {
						// skip holes
						while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}

							
							fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					} else {
						break;
					}
				

			}
		     
			
			    fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
				if (whichLeg == LEFT_LEG ) {
					if ( group == GROUP_6 && ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) )
						break;
					else if ( group == GROUP_7 && fuzzyCIntensity == 3)
						break;
					else if ( group == GROUP_8 && fuzzyCIntensity == 3)
						break;
				}
				
				
			
            boolean findBlackRegionPattern = false;
            findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE);
            
			if ( findBlackRegionPattern && class3Intensity >= 0.09f ) {
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class3Intensity >= 0.09) {
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 5, 0, Class3);
					if (findHolesClass3) {
						// skip holes
						while ((class3Intensity >= 0.09) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
			}
			
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
           
			boolean findWeakEdge = false;
			boolean findWeakEdge10 = false;
			boolean findWeakEdgeClass3 = false;
			int[] xResult = new int[1];
			xResult[0] = -1;
			findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 10, FuzzyC, true, xResult);
			// findWeakEdge10 = findWeakConnectedEdgeOnImageVertical((int)
			// posX, (int) posY, fuzzyCImage, 10, FuzzyC, true, xResult);
			findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 10, Class3, true, xResult);

			float testX = posX, testY = posY;
			boolean lookFor3 = false;

			if (findWeakEdge) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {

					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}

					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2) {
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
			} else {
				if (findWeakEdgeClass3) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.04f) {
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
				}
			}
			
			// arbitrary testing for fuzzyC
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHolesFuzzyC = false;
				findHolesFuzzyC = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
				if (findHolesFuzzyC ) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				// break;
			}

			
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}
	
	private Vector2f traceSection5_lower(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3  && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);


			if ( whichLeg == LEFT_LEG && group == GROUP_6 ) {
				if ( fuzzyCIntensity == 2 ) break;
			}
			
            if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group < GROUP_8 ) )
				) {
				
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					// Cheng1
					if ( (whichLeg == RIGHT_LEG && ( group < GROUP_2 || group > GROUP_9 ))  ||
						 (whichLeg == LEFT_LEG && ( group > GROUP_2 || group < GROUP_9)) ) {
						if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3 )
							break;
					} else {
						if ( fuzzyCIntensity == 3 )
							break;
					}

					posX += stepX;
					posY += stepY;
				
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
			
        	// walk around holes
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
			}

			 
			boolean findGreyRegionPattern = false;
            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
            if ( findGreyRegionPattern ) {
            	continue;
            }
           
           
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
			/*
			if (whichLeg == LEFT_LEG ) {
				if ( group == GROUP_6 && ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) )
					break;
				else if ( group == GROUP_7 && fuzzyCIntensity == 3)
					break;
				else if ( group == GROUP_8 && fuzzyCIntensity == 3)
					break;
			}
			*/ 
			
			if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3 ) break;
            
			if ( class3Intensity >= 0.04f ) {
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
					boolean findHoles = false;
					// find holes on fuzzyC image
					findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
					if (findHoles) {
						// skip holes
						while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}

							
							fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					} else {
						break;
					}
				}

			}
		
           
			
            boolean findBlackRegionPattern = false;
            findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE);
            
			if ( findBlackRegionPattern && class3Intensity >= 0.09f ) {
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class3Intensity >= 0.09) {
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 5, 0, Class3);
					if (findHolesClass3) {
						// skip holes
						while ((class3Intensity >= 0.09) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
			}
		
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

		
			
			// arbitrary testing for fuzzyC
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHolesFuzzyC = false;
				findHolesFuzzyC = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
				if (findHolesFuzzyC ) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				// break;
			}

			// arbitrary testing on class 1 and class 2		
		
            
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection6_condyle(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;
        int holesDiameter = 10;
		
		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		
		 if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
				) { 
         	holesDiameter = 10;
         } else {
         	holesDiameter = 25;
         }
         	
		
		while (fuzzyCIntensity != 3 && fuzzyCIntensity != 2 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
		
		
		    if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group > GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
				) {
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
							continue;
						}
						break;
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
							|| class2Intensity >= 0.9f || class3Intensity >= 0.04f  )
						break;
				
					posX += stepX;
					posY += stepY;
		
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			

			// walk around holes
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
				if (findHoles) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
			}

			
    		
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			if ( class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04 ) break;
			
            
		    boolean findGreyRegionPattern = false;
		    findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE_GREY);
		    if ( findGreyRegionPattern ) {
		    	continue;
		    }
		    
		    
			boolean findBlackRegionPattern = false;
			findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE);

			if ( findBlackRegionPattern ) {
				
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class3Intensity >= 0.01) break;
				boolean findEdgeOnGRE = false;
				while ((class3Intensity <= 0.01) && distCurrent < distOuter) {
					posX += stepX;
					posY += stepY;

					findEdgeOnGRE = findEdgeOnImage((int)posX, (int) posY, greImageSlice, 5, 50, GRE_HIGH_INTEN);
					
					if ( findEdgeOnGRE ) break;
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}

					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}
				
				break;
			}
		
			
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

				boolean findWeakEdge = false;
				boolean findWeakEdge10 = false;
				boolean findWeakEdgeClass3 = false;
				int[] xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
				// findWeakEdge10 = findWeakConnectedEdgeOnImageVertical((int)
				// posX, (int) posY, fuzzyCImage, 10, FuzzyC, true, xResult);
				findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 3, Class3, true, xResult);

				float testX = posX, testY = posY;
				boolean lookFor3 = false;

				if (findWeakEdge) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 /*
																		 * ||
																		 * class3Intensity
																		 * >=
																		 * 0.04f
																		 */) {
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
				} else {
					if (findWeakEdgeClass3) {
						testX = posX;
						testY = posY;
						lookFor3 = false;
						while (distCurrent < distOuter) {

							if (testX <= 0) {
								testX = 5;
								break;
							}
							if (testY <= 0) {
								testY = 5;
								break;
							}

							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.04f) {
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
					}
				}

				boolean findWeakEdge3 = false;
				boolean findWeakEdge7 = false;
				xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
				findWeakEdge7 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 15, FuzzyC, true, xResult);
				testX = posX;
				testY = posY;
				lookFor3 = false;

				if (findWeakEdge3) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3  || fuzzyCIntensity == 2 ) {
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
				} else {
					if (findWeakEdge7) {
						testX = posX;
						testY = posY;
						lookFor3 = false;
						while (distCurrent < distOuter) {

							if (testX <= 0) {
								testX = 5;
								break;
							}
							if (testY <= 0) {
								testY = 5;
								break;
							}

							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
					boolean findHolesFuzzyC = false;
					findHolesFuzzyC = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
					if (findHolesFuzzyC ) {
						// skip holes
						while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
		
			
		
				// arbitrary testing on class 1 and class 2
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04) {
					boolean findHolesClass1 = false;
					findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, holesDiameter, 0, Class1);
					boolean findHolesClass2 = false;
					findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, holesDiameter, 0, Class2);
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, holesDiameter, 0, Class3);
					if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
						// skip holes
						while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							
							class1Intensity = class1Image.getDouble((int) posX, (int) posY);
							class2Intensity = class2Image.getDouble((int) posX, (int) posY);
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
				
		
		
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) break;
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04f)
				break;
				
		
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection1_condyle(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;
		
		int holesDiameter = 10;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		

        if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
				( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
			) { 
        	holesDiameter = 10;
        } else {
        	holesDiameter = 20;
        }
        	
		
		while (fuzzyCIntensity != 3 && fuzzyCIntensity != 2 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		
			boolean findGreyRegionPattern = false;
            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE_GREY);
            if ( findGreyRegionPattern ) {
            	continue;
            }
			
            
            if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group > GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group < GROUP_8 ) )
				) {
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
							|| class2Intensity >= 0.9f || class3Intensity >= 0.05f )
						break;
				
					posX += stepX;
					posY += stepY;
					
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
        	// walk around holes
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
				if (findHoles) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						
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

			if ( class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04 ) break;
			

			
            boolean findBlackRegionPattern = false;
            findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE);
            
			if ( findBlackRegionPattern ) {
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class3Intensity >= 0.01) break;
				boolean findEdgeOnGRE = false;
				while ((class3Intensity <= 0.01) && distCurrent < distOuter) {
					posX += stepX;
					posY += stepY;

					findEdgeOnGRE = findEdgeOnImage((int)posX, (int) posY, greImageSlice, 5, 50, GRE_HIGH_INTEN);
					
					if ( findEdgeOnGRE ) break;
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}

					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}
				
				break;
			}
		
			
			
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			

				boolean findWeakEdge = false;
				boolean findWeakEdgeClass3 = false;
				boolean findWeakEdge7 = false;
				int[] xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
				findWeakEdge7 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 7, FuzzyC, true, xResult);
				findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 3, Class3, true, xResult);

				float testX = posX, testY = posY;
				boolean lookFor3 = false;

				if (findWeakEdge) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 /*
																		 * ||
																		 * class3Intensity
																		 * >=
																		 * 0.04f
																		 */) {
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
				} else if (findWeakEdgeClass3) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.04f) {
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

				} else if (findWeakEdge7) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.04f) {
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
				}

				boolean findWeakEdge3 = false;
				findWeakEdge7 = false;
				xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
				findWeakEdge7 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 15, FuzzyC, true, xResult);
				testX = posX;
				testY = posY;
				lookFor3 = false;

				if (findWeakEdge3) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 /*
																		 * ||
																		 * class3Intensity
																		 * >=
																		 * 0.04f
																		 */) {
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
				} else {
					if (findWeakEdge7) {
						testX = posX;
						testY = posY;
						lookFor3 = false;
						while (distCurrent < distOuter) {

							if (testX <= 0) {
								testX = 5;
								break;
							}
							if (testY <= 0) {
								testY = 5;
								break;
							}

							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 /*
																			 * ||
																			 * class3Intensity
																			 * >=
																			 * 0.04f
																			 */) {
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
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
					boolean findHolesFuzzyC = false;
					findHolesFuzzyC = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, holesDiameter, 0, FuzzyC);
					if (findHolesFuzzyC ) {
						// skip holes
						while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
			
			
			// arbitrary testing on class 1 and class 2	
		
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			
				if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04f) {
					boolean findHolesClass1 = false;
					findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, holesDiameter, 0, Class1);
					boolean findHolesClass2 = false;
					findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, holesDiameter, 0, Class2);
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, holesDiameter, 0, Class3);
					if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
						// skip holes
						while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							class1Intensity = class1Image.getDouble((int) posX, (int) posY);
							class2Intensity = class2Image.getDouble((int) posX, (int) posY);
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
				    break;
				}
				
				
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) break;
				
				fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);

				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04f)
					break;
			 
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection2_condyle(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {
		
		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && fuzzyCIntensity != 2 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

			
            if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
				) {
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
                    /*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
							|| class2Intensity >= 0.9f || class3Intensity >= 0.04f )
						break;
					
					posX += stepX;
					posY += stepY;
			
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
         // walk around holes
         			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
         			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
         				boolean findHoles = false;
         				// find holes on fuzzyC image
         				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
         				if (findHoles) {
         					// skip holes
         					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
         						posX += stepX;
         						posY += stepY;
         						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
         						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
         					}
         				}
         			}

         			boolean findGreyRegionPattern = false;
                     findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE_GREY);
                     if ( findGreyRegionPattern ) {
                     	continue;
                     }
                     
                     fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
         			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
         			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
         			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

         			if ( class3Intensity >= 0.04f) break;
                     
                     
            boolean findBlackRegionPattern = false;
            findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 2, 0, GRE);
			
			if ( findBlackRegionPattern ) {
				
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class3Intensity >= 0.01) break;
				boolean findEdgeOnGRE = false;
				while ((class3Intensity <= 0.01) && distCurrent < distOuter) {
					posX += stepX;
					posY += stepY;

					findEdgeOnGRE = findEdgeOnImage((int)posX, (int) posY, greImageSlice, 5, 50, GRE_HIGH_INTEN);
					
					if ( findEdgeOnGRE ) break;
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}

					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}
				
				break;
			}
					
			
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			
		
				boolean findWeakEdge = false;
				boolean findWeakEdgeClass2 = false;
				boolean findWeakEdgeClass3 = false;
				int[] xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
				findWeakEdgeClass2 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class2Image, 7, Class2, false, xResult);
				findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 3, Class3, false, xResult);

				float testX = posX, testY = posY;
				boolean lookFor3 = false;

				if (findWeakEdge || findWeakEdgeClass2) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 /* || fuzzyCIntensity == 2 */ ) {
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

				if (degree >= section2_degree_end - 20 && degree <= section2_degree_end) {
					if (findWeakEdgeClass3) {
						testX = posX;
						testY = posY;
						lookFor3 = false;
						while (distCurrent < distOuter) {

							if (testX <= 0) {
								testX = 5;
								break;
							}
							if (testY <= 0) {
								testY = 5;
								break;
							}

							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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

				boolean findWeakEdge3 = false;
				boolean findWeakEdge7 = false;
				xResult = new int[1];
				xResult[0] = -1;
				findWeakEdge3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
				findWeakEdge7 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 15, FuzzyC, true, xResult);
				testX = posX;
				testY = posY;
				lookFor3 = false;

				if (findWeakEdge3) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {

						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
						if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
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
				} else {
					if (findWeakEdge7) {
						testX = posX;
						testY = posY;
						lookFor3 = false;
						while (distCurrent < distOuter) {

							if (testX <= 0) {
								testX = 5;
								break;
							}
							if (testY <= 0) {
								testY = 5;
								break;
							}

							fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
							class3Intensity = class3Image.getDouble((int) testX, (int) testY);
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 /*
																			 * ||
																			 * class3Intensity
																			 * >=
																			 * 0.04f
																			 */) {
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

				boolean findEdgeOnClass1 = false;
				boolean findLowIntenClass1 = false;
				boolean findEdgeOnClass2 = false;
				boolean findEdgeOnClass3 = false;
				findEdgeOnClass1 = findEdgeOnImage((int) posX, (int) posY, class1Image, 7, 50, Class1);
				findLowIntenClass1 = findEdgeOnImage((int) posX, (int) posY, class1Image, 7, 50, Class1_lowInten);
				findEdgeOnClass2 = findEdgeOnImage((int) posX, (int) posY, class2Image, 7, 50, Class2);
				findEdgeOnClass3 = findEdgeOnImage((int) posX, (int) posY, class3Image, 7, 50, Class3);
				if (findEdgeOnClass1 || findLowIntenClass1 || findEdgeOnClass2 || findEdgeOnClass3) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
						class3Intensity = class3Image.getDouble((int) testX, (int) testY);
                        /*
					    if (  whichLeg == LEFT_LEG && group == GROUP_8 && degree <= section2_degree_end && degree > section2_degree_end - 30  ) { 
							if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 || class3Intensity >= 0.02f) {
								lookFor3 = true;
								break;
							}
					    } else {
					    	if (fuzzyCIntensity == 3 || fuzzyCIntensity == 2 ) {
								lookFor3 = true;
								break;
							}
					    }
					    */ 
						if (fuzzyCIntensity == 3 ) {
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

					// break;
				}

				// arbitrary testing on class 1 and class 2
               
				class1Intensity = class1Image.getDouble((int) posX, (int) posY);
				class2Intensity = class2Image.getDouble((int) posX, (int) posY);
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class2Intensity >= 0.9f || class1Intensity < 0.1f  || class3Intensity >= 0.04  ) {
					boolean findHolesClass1 = false;
					findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, 25, 0, Class1);
					boolean findHolesClass2 = false;
					findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, 25, 0, Class2);
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 25, 0, Class3);
					if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
						// skip holes
						while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;

							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}

							class1Intensity = class1Image.getDouble((int) posX, (int) posY);
							class2Intensity = class2Image.getDouble((int) posX, (int) posY);
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
			
		
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		} // end while loop

		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection3_condyle(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			

			if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group > GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group < GROUP_8 ) )
				) {
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
					*/ 
					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					if (fuzzyCIntensity == 2 ||  fuzzyCIntensity == 3 ||  class1Intensity <= 0.1f 
							|| class2Intensity >= 0.9f || class3Intensity >= 0.04f)
						break;
					
					posX += stepX;
					posY += stepY;
					
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
            
			
			boolean findGreyRegionPattern = false;
            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
            if ( findGreyRegionPattern ) {
            	continue;
            }
            

			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			// walk around holes
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) { // skip holes
					while (( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
			}


			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if ( fuzzyCIntensity == 3 ) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) { // skip holes
					while ( fuzzyCIntensity == 3 && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				break;
			}
            
            
			
			// arbitrary testing on class 1 and class 2
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.9f || class1Intensity < 0.1f ) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) { // skip holes
					while (( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				break;
			}
			
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if ( fuzzyCIntensity == 3 ) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
				if (findHoles) { // skip holes
					while ( fuzzyCIntensity == 3 && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				break;
			}
            

		     /*
			// arbitrary testing on class 1 and class 2
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04) {
				boolean findHolesClass1 = false;
				findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, 25, 0, Class1);
				boolean findHolesClass2 = false;
				findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, 25, 0, Class2);
				boolean findHolesClass3 = false;
				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 25, 0, Class3);
				if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
					// skip holes
					while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						class1Intensity = class1Image.getDouble((int) posX, (int) posY);
						class2Intensity = class2Image.getDouble((int) posX, (int) posY);
						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				// break;
			}
			*/
             
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		} // end while loop
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection4_condyle(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			
        	if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
				){
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					/* Cheng1
					if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3  || class1Intensity <= 0.1f  ||  class2Intensity >= 0.9f )
						break;
					if ( fuzzyCIntensity == 3  )
						break;
					*/ 
					
					if ((whichLeg == RIGHT_LEG && (group < GROUP_2 || group > GROUP_9)) || (whichLeg == LEFT_LEG && (group > GROUP_2 || group < GROUP_9))) {
						if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f || class2Intensity >= 0.9f)
							break;
					} else {
						if (fuzzyCIntensity == 3)
							break;
					}
					
					posX += stepX;
					posY += stepY;
				
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
            
        	boolean findGreyRegionPattern = false;
            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
            if ( findGreyRegionPattern ) {
            	continue;
            }
            
        	
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			// walk around holes
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
				 
				if (findHoles) { // skip holes
					
							while (( fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
					   
				}
			}
		
		
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			// Cheng1
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			
			if ((whichLeg == RIGHT_LEG && (group < GROUP_2 || group > GROUP_9)) ||
					(whichLeg == LEFT_LEG && (group > GROUP_2 || group < GROUP_9))) {
				if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f || class2Intensity >= 0.9f)
					break;
			} else {
				if (fuzzyCIntensity == 3)
					break;
			}

			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop
		
		return new Vector2f(posX, posY);
	}

	private Vector2f traceSection5_upper_condyle(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && fuzzyCIntensity != 2 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		
            if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
				) {
				
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3 || class1Intensity <= 0.1f 
							|| class2Intensity >= 0.9f || class3Intensity >= 0.04f )
						break;

					posX += stepX;
					posY += stepY;
				
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
        	// walk around holes
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
				if (findHoles) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
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

			if ( class3Intensity >= 0.04f || fuzzyCIntensity == 2 && fuzzyCIntensity == 3) break;
			
			
			boolean findGreyRegionPattern = false;
            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
            if ( findGreyRegionPattern ) {
            	continue;
            }
            
		
            boolean findBlackRegionPattern = false;
            findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE);
            
			if ( findBlackRegionPattern && class3Intensity >= 0.09f ) {
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class3Intensity >= 0.09) {
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 5, 0, Class3);
					if (findHolesClass3) {
						// skip holes
						while ((class3Intensity >= 0.09) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
			}
			
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			float testX = posX, testY = posY;
			boolean lookFor3 = false;
			boolean findWeakEdge = false;
			boolean findWeakEdgeClass2 = false;
			boolean findWeakEdgeClass3 = false;
			int []xResult = new int[1];
			xResult[0] = -1;
			findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 5, FuzzyC, true, xResult);
			findWeakEdgeClass2 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class2Image, 5, Class2, false, xResult);
			findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 5, Class3, false, xResult);
			
			if (findWeakEdge || findWeakEdgeClass2 || findWeakEdgeClass3) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {
					
					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 /* || fuzzyCIntensity == 2 */  || class3Intensity >= 0.09f ) {
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
			
			boolean findWeakEdge3 = false;
			boolean findWeakEdge7 = false;
			xResult = new int[1];
			xResult[0] = -1;
		    findWeakEdge3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
		    findWeakEdge7 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 15, FuzzyC, true, xResult);
			testX = posX; testY = posY;
			lookFor3 = false;
			
			if (findWeakEdge3 ) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {
					
					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}
					
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
			} else {
				if ( findWeakEdge7 ) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						
						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}
						
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
			}
		 
			
			boolean findEdgeOnClass1 = false;	
			boolean findLowIntenClass1 = false;
			boolean findEdgeOnClass2 = false;
			boolean findEdgeOnClass3 = false;
		    findEdgeOnClass1 = findEdgeOnImage((int) posX, (int) posY, class1Image, 7, 50, Class1);
		    findLowIntenClass1 = findEdgeOnImage((int) posX, (int) posY, class1Image, 7, 50, Class1_lowInten);
		    findEdgeOnClass2 = findEdgeOnImage((int) posX, (int) posY, class2Image, 7, 50, Class2);
		    findEdgeOnClass3 = findEdgeOnImage((int) posX, (int) posY, class3Image, 7, 50, Class3);
			if (findEdgeOnClass1 || findLowIntenClass1 ||  findEdgeOnClass2 || findEdgeOnClass3) {
				testX = posX; testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter ) {
					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}
					
					// Cheng1
					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					
					if ((whichLeg == RIGHT_LEG && (group < GROUP_2 || group > GROUP_9)) || (whichLeg == LEFT_LEG && (group > GROUP_2 || group < GROUP_9))) {
						if (fuzzyCIntensity == 3  ||  fuzzyCIntensity == 2  ||   class3Intensity >= 0.09f ) {
							lookFor3 = true;
							break;
						}
					} else {
						if (fuzzyCIntensity == 3) {
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
		     	 
			
			// arbitrary testing for fuzzyC
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHolesFuzzyC = false;
				findHolesFuzzyC = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
				if (findHolesFuzzyC ) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				// break;
			}

			// arbitrary testing on class 1 and class 2		
			/*
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04) {
				boolean findHolesClass1 = false;
				findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, 15, 0, Class1);
				boolean findHolesClass2 = false;
				findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, 15, 0, Class2);
				boolean findHolesClass3 = false;
				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 15, 0, Class3);
				if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
					// skip holes
					while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						class1Intensity = class1Image.getDouble((int) posX, (int) posY);
						class2Intensity = class2Image.getDouble((int) posX, (int) posY);
						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				break;
			}
            */ 
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}
	
	private Vector2f traceSection5_lower_condyle(float posX, float posY, float stepX, float stepY, Vector3f center, float degree, int sliceNumber,
			float distCurrent, float distOuter, ModelImage greImageSlice, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage fuzzyCImage) {

		double fuzzyCIntensity;
		double class1Intensity;
		double class2Intensity;
		double class3Intensity;

		fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
		class1Intensity = class1Image.getDouble((int) posX, (int) posY);
		class2Intensity = class2Image.getDouble((int) posX, (int) posY);
		class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		while (fuzzyCIntensity != 3 && distCurrent < distOuter) {

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}
			
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);

		
            if (   ( whichLeg == RIGHT_LEG && ( group <= GROUP_3 || group >= GROUP_8 ) ) ||
					( whichLeg == LEFT_LEG && ( group >= GROUP_3 || group <= GROUP_8 ) )
				) {
				
				while ( distCurrent < distOuter ) {
					
					if (posX <= 0) {
						posX = 5;
						break;
					}
					if (posY <= 0) {
						posY = 5;
						break;
					}
					
					/*
					findGreyRegionPattern = false;
		            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 7, 0, GRE_GREY);
		            if ( findGreyRegionPattern ) {
		            	continue;
		            }
		            */ 
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);

					// walk around holes
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
						boolean findHoles = false;
						// find holes on fuzzyC image
						findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 25, 0, FuzzyC);
						if (findHoles) {
							// skip holes
							while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
								posX += stepX;
								posY += stepY;
								
								if (posX <= 0) {
									posX = 5;
									break;
								}
								if (posY <= 0) {
									posY = 5;
									break;
								}

								
								fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
								distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
							}
						}
					}

					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
					class1Intensity = class1Image.getDouble((int) posX, (int) posY);
					class2Intensity = class2Image.getDouble((int) posX, (int) posY);
					class3Intensity = class3Image.getDouble((int) posX, (int) posY);
					
					// Cheng1
					if ( (whichLeg == RIGHT_LEG && ( group < GROUP_2 || group > GROUP_9 ))  ||
						 (whichLeg == LEFT_LEG && ( group > GROUP_2 || group < GROUP_9)) ) {
						if ( fuzzyCIntensity == 2 || fuzzyCIntensity == 3 )
							break;
					} else {
						if ( fuzzyCIntensity == 3 )
							break;
					}

					posX += stepX;
					posY += stepY;
				
					distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
				}	
				break;
			}
			
        	// walk around holes
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHoles = false;
				// find holes on fuzzyC image
				findHoles = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
				if (findHoles) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
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

			if ( class3Intensity >= 0.04f) break;

			boolean findGreyRegionPattern = false;
            findGreyRegionPattern = findGreyRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE_GREY);
            if ( findGreyRegionPattern ) {
            	continue;
            }
            
		
            boolean findBlackRegionPattern = false;
            findBlackRegionPattern = findBlackRegionOnImage((int) posX, (int) posY, greImageSlice, 5, 0, GRE);
            
			if ( findBlackRegionPattern && class3Intensity >= 0.09f ) {
				class3Intensity = class3Image.getDouble((int) posX, (int) posY);
				if (class3Intensity >= 0.09) {
					boolean findHolesClass3 = false;
					findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 5, 0, Class3);
					if (findHolesClass3) {
						// skip holes
						while ((class3Intensity >= 0.09) && distCurrent < distOuter) {
							posX += stepX;
							posY += stepY;
							
							if (posX <= 0) {
								posX = 5;
								break;
							}
							if (posY <= 0) {
								posY = 5;
								break;
							}
	
							class3Intensity = class3Image.getDouble((int) posX, (int) posY);
							distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
						}
						continue;
					}
					break;
				}
			}
			
			
			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			float testX = posX, testY = posY;
			boolean lookFor3 = false;
			boolean findWeakEdge = false;
			boolean findWeakEdgeClass2 = false;
			boolean findWeakEdgeClass3 = false;
			int []xResult = new int[1];
			xResult[0] = -1;
			findWeakEdge = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, fuzzyCImage, 5, FuzzyC, true, xResult);
			findWeakEdgeClass2 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class2Image, 5, Class2, false, xResult);
			findWeakEdgeClass3 = findWeakConnectedEdgeOnImageVertical((int) posX, (int) posY, class3Image, 5, Class3, false, xResult);
			
			if (findWeakEdge || findWeakEdgeClass2 || findWeakEdgeClass3) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {
					
					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}
					
					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					if (fuzzyCIntensity == 3 /* || fuzzyCIntensity == 2 */  || class3Intensity >= 0.09f ) {
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
			
			boolean findWeakEdge3 = false;
			boolean findWeakEdge7 = false;
			xResult = new int[1];
			xResult[0] = -1;
		    findWeakEdge3 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 3, FuzzyC, true, xResult);
		    findWeakEdge7 = findWeakConnectedEdgeOnImageHorizontal((int) posX, (int) posY, fuzzyCImage, 15, FuzzyC, true, xResult);
			testX = posX; testY = posY;
			lookFor3 = false;
			
			if (findWeakEdge3 ) {
				testX = posX;
				testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter) {
					
					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}
					
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
			} else {
				if ( findWeakEdge7 ) {
					testX = posX;
					testY = posY;
					lookFor3 = false;
					while (distCurrent < distOuter) {
						
						if (testX <= 0) {
							testX = 5;
							break;
						}
						if (testY <= 0) {
							testY = 5;
							break;
						}
						
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
			}
		 
			
			boolean findEdgeOnClass1 = false;	
			boolean findLowIntenClass1 = false;
			boolean findEdgeOnClass2 = false;
			boolean findEdgeOnClass3 = false;
		    findEdgeOnClass1 = findEdgeOnImage((int) posX, (int) posY, class1Image, 7, 50, Class1);
		    findLowIntenClass1 = findEdgeOnImage((int) posX, (int) posY, class1Image, 7, 50, Class1_lowInten);
		    findEdgeOnClass2 = findEdgeOnImage((int) posX, (int) posY, class2Image, 7, 50, Class2);
		    findEdgeOnClass3 = findEdgeOnImage((int) posX, (int) posY, class3Image, 7, 50, Class3);
			if (findEdgeOnClass1 || findLowIntenClass1 ||  findEdgeOnClass2 || findEdgeOnClass3) {
				testX = posX; testY = posY;
				lookFor3 = false;
				while (distCurrent < distOuter ) {
					if (testX <= 0) {
						testX = 5;
						break;
					}
					if (testY <= 0) {
						testY = 5;
						break;
					}
					
					// Cheng1
					fuzzyCIntensity = fuzzyCImage.getDouble((int) testX, (int) testY);
					class3Intensity = class3Image.getDouble((int) testX, (int) testY);
					
					if ((whichLeg == RIGHT_LEG && (group < GROUP_2 || group > GROUP_9)) || (whichLeg == LEFT_LEG && (group > GROUP_2 || group < GROUP_9))) {
						if (fuzzyCIntensity == 3  ||  fuzzyCIntensity == 2  ||   class3Intensity >= 0.09f ) {
							lookFor3 = true;
							break;
						}
					} else {
						if (fuzzyCIntensity == 3) {
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
		     	 
			
			// arbitrary testing for fuzzyC
			// arbitrary testing for fuzzyC
			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			if (fuzzyCIntensity == 2 || fuzzyCIntensity == 3) {
				boolean findHolesFuzzyC = false;
				findHolesFuzzyC = findHolesOnImage((int) posX, (int) posY, fuzzyCImage, 15, 0, FuzzyC);
				if (findHolesFuzzyC ) {
					// skip holes
					while ((fuzzyCIntensity == 2 || fuzzyCIntensity == 3) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
				}
				// break;
			}

			// arbitrary testing on class 1 and class 2		
			/*
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
			if (class2Intensity >= 0.9f || class1Intensity < 0.1f || class3Intensity >= 0.04) {
				boolean findHolesClass1 = false;
				findHolesClass1 = findHolesOnImage((int) posX, (int) posY, class1Image, 15, 0, Class1);
				boolean findHolesClass2 = false;
				findHolesClass2 = findHolesOnImage((int) posX, (int) posY, class2Image, 15, 0, Class2);
				boolean findHolesClass3 = false;
				findHolesClass3 = findHolesOnImage((int) posX, (int) posY, class3Image, 15, 0, Class3);
				if (findHolesClass1 || findHolesClass2 || findHolesClass3) {
					// skip holes
					while ((class1Intensity <= 0.1f || class2Intensity >= 0.9f || class3Intensity >= 0.04) && distCurrent < distOuter) {
						posX += stepX;
						posY += stepY;
						
						if (posX <= 0) {
							posX = 5;
							break;
						}
						if (posY <= 0) {
							posY = 5;
							break;
						}

						class1Intensity = class1Image.getDouble((int) posX, (int) posY);
						class2Intensity = class2Image.getDouble((int) posX, (int) posY);
						class3Intensity = class3Image.getDouble((int) posX, (int) posY);
						distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));
					}
					continue;
				}
				break;
			}
            */ 
			posX += stepX;
			posY += stepY;
			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			if (posX <= 0) {
				posX = 5;
				break;
			}
			if (posY <= 0) {
				posY = 5;
				break;
			}

			fuzzyCIntensity = fuzzyCImage.getDouble((int) posX, (int) posY);
			class1Intensity = class1Image.getDouble((int) posX, (int) posY);
			class2Intensity = class2Image.getDouble((int) posX, (int) posY);
			class3Intensity = class3Image.getDouble((int) posX, (int) posY);
		} // end while loop

		return new Vector2f(posX, posY);
	}

	
	private void identifyGroups(int sliceNumber) {
		
		int[] extents = new int[3];
		extents = fatImage.getExtents();
		int totalSlices = extents[2];
		
		if ( whichLeg == RIGHT_LEG ) {
			GROUP_1 = 1;
			GROUP_2 = 2;
			GROUP_3 = 3;
			GROUP_4 = 4;
			GROUP_5 = 5;
			GROUP_6 = 6;
			GROUP_7 = 7;
			GROUP_8 = 8;
			GROUP_9 = 9;
			GROUP_10 = 10;
		} else if ( whichLeg == LEFT_LEG ) {
			GROUP_1 = 10;
			GROUP_2 = 9;
			GROUP_3 = 8;
			GROUP_4 = 7;
			GROUP_5 = 6;
			GROUP_6 = 5;
			GROUP_7 = 4;
			GROUP_8 = 3;
			GROUP_9 = 2;
			GROUP_10 = 1;
		}
		
		
		
		group_5_endSlice = (int)(midPt - 0.1f * totalSlices);
		group_4_endSlice = (int)(group_5_endSlice - 0.10f * totalSlices);
		group_3_endSlice = (int)(group_4_endSlice - 0.15f * totalSlices);
		group_2_endSlice = (int)(group_3_endSlice - 0.05f * totalSlices);
		group_1_endSlice = (int)(group_2_endSlice - 0.05f * totalSlices);
		
		System.err.println("group_1_endSlice = " + group_1_endSlice);
		System.err.println("group_2_endSlice = " + group_2_endSlice);
		System.err.println("group_3_endSlice = " + group_3_endSlice);
		System.err.println("group_4_endSlice = " + group_4_endSlice);
		System.err.println("group_5_endSlice = " + group_5_endSlice);
		
		if ( whichLeg == RIGHT_LEG ) {
			if ( sliceNumber <= midPt && sliceNumber >  group_5_endSlice ) {
				group = GROUP_5;
				System.err.println("group 5");
			} else if ( sliceNumber <= group_5_endSlice && sliceNumber > group_4_endSlice ) {
				group = GROUP_4;
				System.err.println("group 4");
			} else if ( sliceNumber <= group_4_endSlice && sliceNumber > group_3_endSlice ) {
				group = GROUP_3;
				System.err.println("group 3");
			} else if ( sliceNumber <= group_3_endSlice && sliceNumber > group_2_endSlice ) {
				group = GROUP_2;
				System.err.println("group 2");
			} else if ( sliceNumber <= group_2_endSlice ) { 
				group = GROUP_1;
				System.err.println("group 1");
			}
		} else if ( whichLeg == LEFT_LEG ) {
			if ( sliceNumber <= midPt && sliceNumber >  group_5_endSlice ) {
				group = GROUP_6;
				System.err.println("group 6");
			} else if ( sliceNumber <= group_5_endSlice && sliceNumber > group_4_endSlice ) {
				group = GROUP_7;
				System.err.println("group 7");
			} else if ( sliceNumber <= group_4_endSlice && sliceNumber > group_3_endSlice ) {
				group = GROUP_8;
				System.err.println("group 8");
			} else if ( sliceNumber <= group_3_endSlice && sliceNumber > group_2_endSlice ) {
				group = GROUP_9;
				System.err.println("group 9");
			} else if ( sliceNumber <= group_2_endSlice ) { 
				group = GROUP_10;
				System.err.println("group 10");
			}
		}
	
	    group_6_endSlice = (int)(midPt + 0.12f * totalSlices);
	    group_7_endSlice = (int)(group_6_endSlice + 0.15f * totalSlices);
	    group_8_endSlice = (int)(group_7_endSlice + 0.07f * totalSlices);
	    group_9_endSlice = (int)(group_8_endSlice + 0.05f * totalSlices);
	    group_10_endSlice = (int)(group_9_endSlice + 0.05f * totalSlices);
	    
	    System.err.println("group_6_endSlice = " + group_6_endSlice);
		System.err.println("group_7_endSlice = " + group_7_endSlice);
		System.err.println("group_8_endSlice = " + group_8_endSlice);
		System.err.println("group_9_endSlice = " + group_9_endSlice);
		System.err.println("group_10_endSlice = " + group_10_endSlice);
		
		if ( whichLeg == RIGHT_LEG ) {
		    if ( sliceNumber > midPt && sliceNumber < group_6_endSlice ) {
		    	group = GROUP_6;
		    	System.err.println("group 6");
		    } else if ( sliceNumber >= group_6_endSlice && sliceNumber < group_7_endSlice) {
		    	group = GROUP_7;
		    	System.err.println("group 7");
		    } else if ( sliceNumber >= group_7_endSlice && sliceNumber < group_8_endSlice) {
		    	group = GROUP_8;
		    	System.err.println("group 8");
		    } else if ( sliceNumber >= group_8_endSlice && sliceNumber < group_9_endSlice) {
		    	group = GROUP_9;
		    	System.err.println("group 9");
		    } else if ( sliceNumber >= group_9_endSlice ) {
		    	group = GROUP_10;
		    	System.err.println("group 10");
		    }
		} else if ( whichLeg == LEFT_LEG ) {
			if ( sliceNumber > midPt && sliceNumber < group_6_endSlice ) {
		    	group = GROUP_5;
		    	System.err.println("group 5");
		    } else if ( sliceNumber >= group_6_endSlice && sliceNumber < group_7_endSlice) {
		    	group = GROUP_4;
		    	System.err.println("group 4");
		    } else if ( sliceNumber >= group_7_endSlice && sliceNumber < group_8_endSlice) {
		    	group = GROUP_3;
		    	System.err.println("group 3");
		    } else if ( sliceNumber >= group_8_endSlice && sliceNumber < group_9_endSlice) {
		    	group = GROUP_2;
		    	System.err.println("group 2");
		    } else if ( sliceNumber >= group_9_endSlice ) {
		    	group = GROUP_1;
		    	System.err.println("group 1");
		    }
		}
	    
	    
	    
	}
	
	private void tracingDFS_condyle(ModelImage fatImageSlice, ModelImage fuzzyCImage, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage greImageSlice, ModelImage gaussianImageSlice, int sliceNumber, VOI resultVOIBoundary, VOI resultVOIInner, VOI resultVOIOuter, 
			VOILine leftLine, VOILine rightLine ) {

		
		System.err.println("tracing condyle");
		float posX, posY;
		float stepX, stepY;
		int i;

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

		
		float[][] sections = new float[7][2];

		// center contour
		VOIBase v = resultVOIBoundary.getCurves().get(0);
		int nPts = v.size();
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		v.exportArrays(xPts, yPts, zPts);
		
		// get outer contour
		VOIBaseVector outer_va = resultVOIOuter.getCurves();
		VOIBase outer_v = outer_va.get(0);
		// int nPtsCurrent = outer_v.size();
		xPtsOuter = new float[nPts];
		yPtsOuter = new float[nPts];
		zPtsOuter = new float[nPts];
		outer_v.exportArrays(xPtsOuter, yPtsOuter, zPtsOuter);

		// get inner contour
		VOIBaseVector inner_va = resultVOIInner.getCurves();
		VOIBase inner_v = inner_va.get(0);
		xPtsInner = new float[nPts];
		yPtsInner = new float[nPts];
		zPtsInner = new float[nPts];
		inner_v.exportArrays(xPtsInner, yPtsInner, zPtsInner);

	    
	    //*************************************     Start tracing   *********************************************
		identifyGroups(sliceNumber);
		
		VOI resultVOI = new VOI((short) 0, "result-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(resultVOI);
		
		VOI lineVOI = new VOI((short) greImageSlice.getVOIs().size(), "line", VOI.LINE, 0);
		lineVOI.setColor(Color.green);
		VOIVector voiVectorLines = new VOIVector();
		voiVectorLines.add(lineVOI);

		Hashtable<Integer, Line> linePoints = new Hashtable<Integer, Line>();
	
		Vector3f center = resultVOIBoundary.getGeometricCenter();
		
		// ****************************    tracing out the bottom section contour  ********************************************* 
		Vector3f pt[] = new Vector3f[nPts];

		for (i = 0; i < nPts; i++) {

			// int currentStep = 0;
			float distOuter = 0;
			float distInner = 0;
			float distCurrent = 0;

			float xOuter = xPtsOuter[i];
			float yOuter = yPtsOuter[i];
			distOuter = (float) Math.sqrt((xOuter - center.X) * (xOuter - center.X) + (yOuter - center.Y) * (yOuter - center.Y));

			float xInner = xPtsInner[i];
			float yInner = yPtsInner[i];
			distInner = (float) Math.sqrt((xInner - center.X) * (xInner - center.X) + (yInner - center.Y) * (yInner - center.Y));

			stepX = (xInner - center.X) / 600;
			stepY = (yInner - center.Y) / 600;

			posX = xInner;
			posY = yInner;

			float degree;

			Vector2f v_curr_inner_cartisian = new Vector2f(xInner, yInner);
			Vector2f v_curr_inner_polar = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(v_curr_inner_cartisian, v_curr_inner_polar, center);

			degree = v_curr_inner_polar.Y;

			distCurrent = (float) Math.sqrt((posX - center.X) * (posX - center.X) + (posY - center.Y) * (posY - center.Y));

			// Trace each section differently
			Vector2f resultPos = new Vector2f(0, 0);
			if (degree >= section1_degree_start && degree < section1_degree_end) {
				resultPos = traceSection1_condyle(posX, posY, stepX, stepY, center, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				resultPos = traceSection2_condyle(posX, posY, stepX, stepY, center, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section3_degree_start && degree < section3_degree_end) {
				resultPos = traceSection3_condyle(posX, posY, stepX, stepY, center, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section4_degree_start && degree < section4_degree_end) {
				resultPos = traceSection4_condyle(posX, posY, stepX, stepY, center, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree > section5_degree_upperHalf_start && degree <= section5_degree_upperHalf_end) {
				resultPos = traceSection5_upper_condyle(posX, posY, stepX, stepY, center, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end) {
				resultPos = traceSection5_lower_condyle(posX, posY, stepX, stepY, center, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else { // section 6
				resultPos = traceSection6_condyle(posX, posY, stepX, stepY, center, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
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

				
		// *****************************    Post-processing   **************************************************** 
		resultVOI.importCurve(pt);
		// voiNewFinal.importCurve(points);
		// slicesPts.put(sliceNumber, linePoints);
		// findBoundingContour(sliceNumber, resultVOIBoundary, resultVOIInner,
		// resultVOIOuter);

		// greImageSlice.addVOIs(voiVectorLines);
		// new ViewJFrameImage(greImageSlice);

		greImageSlice.addVOIs(voiVectorNew);
		
		smoothVOI150Single(greImageSlice, greImageSlice);
		double constant_length = 2.5d;
		resultVOI = greImageSlice.getVOIs().VOIAt(0);
		VOIBaseVector vVector = resultVOI.getCurves();

		v = vVector.get(0);
		
		if ( v.size() <= 3 ) return;
		
		double perimeter = v.getLengthPtToPt(greImageSlice.getFileInfo(0).getResolutions());
		// System.err.println("perimeter = " + perimeter);

		// deal with near end slices, tracing the pattern from center of result
		// VOI
		/*
		if ((sliceNumber - startPt) <= 2 || (endPt - sliceNumber) <= 2) {
			constant_length = 0.5d;
		} else if (((sliceNumber - startPt) > 2 && (sliceNumber - startPt) <= 5) || (endPt - sliceNumber) > 2 && (endPt - sliceNumber) <= 5) {
			constant_length = 0.8d;
		} else {
			constant_length = 1.2d;
		}
		*/ 
		if ( group == GROUP_1 || group == GROUP_2 || group == GROUP_9 || group == GROUP_10 ) {
			constant_length = 2.0d;
		} else {
			constant_length = 2.5d;
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
		nPts = result_v.size();
		xPts = new float[nPts];
		yPts = new float[nPts];
		zPts = new float[nPts];

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

		Vector3f[] pts = new Vector3f[m];
		for (int z = 0; z < m; z++) {
			posX = xNew[z];
			posY = yNew[z];
			Line line = new Line(center.X, center.Y, posX, posY, z);
			linePoints.put(z, line);
			pts[z] = new Vector3f(posX, posY, 0);
		}
		
		resultVOI.removeCurves();
		greImageSlice.getVOIs().removeAllElements();
		voiVectorNew.removeAllElements();
		voiVectorNew.add(resultVOI);
		resultVOI.importCurve(pts);
		greImageSlice.addVOIs(voiVectorNew);

		slicesPts.put(sliceNumber, linePoints);

		// Cheng1!!!!!
		// generate inner and outer contours
	    // findBoundingContour(sliceNumber, resultVOIBoundary, resultVOIInner, resultVOIOuter, center, fatImageSlice);
	    findBoundingContour_condyle(sliceNumber, resultVOIBoundary, resultVOIInner, resultVOIOuter, center, fatImageSlice, leftLine, rightLine);

		// copy result VOI from 2D slice to 3D slice.
		// resultVOI.removeCurves();
		// VOIBase vTemp = (VOIBase) result_v.clone();

		// vTemp.importArrays(xNew, yNew, zNew, m);
		// resultVOI.importCurve(vTemp);
		// smoothVOISingle(greImageSlice, greImageSlice, m);

		fuzzyCImage.addVOIs(voiVectorNew);
		class1Image.addVOIs(voiVectorNew);
		class2Image.addVOIs(voiVectorNew);
		class3Image.addVOIs(voiVectorNew);
		// new ViewJFrameImage(greImageSlice);    // Cheng1
	    // new ViewJFrameImage(class1Image);
		// new ViewJFrameImage(class2Image);
		// new ViewJFrameImage(class3Image);
		// new ViewJFrameImage(fuzzyCImage);

		// VOIBaseVector current_va =
		// greImageSlice.getVOIs().VOIAt(2).getCurves();
		VOIBaseVector current_va = greImageSlice.getVOIs().VOIAt(0).getCurves();
		if (current_va.size() > 0) {
			VOIBase current_v = current_va.get(0);

			VOIBase vTemp = (VOIBase) current_v.clone();

			int nPtsCurrent = current_v.size();
			
			if ( nPtsCurrent == 0 ) return;
			
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

	private void tracingDFS(ModelImage fatImageSlice, ModelImage fuzzyCImage, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage greImageSlice, ModelImage gaussianImageSlice, int sliceNumber, VOI resultVOIBoundary, VOI resultVOIInner, VOI resultVOIOuter, 
			VOILine leftLine, VOILine rightLine ) {

	   
	    Vector3f lowerLeftPt = leftLine.get(0);
	    Vector3f lowerRightPt = rightLine.get(0);
		
	    Vector3f upperLeftPt = leftLine.get(1);
	    Vector3f upperRightPt = rightLine.get(1);
	    
		 // ******************************   Check the two lines lower points distance *************************
	    float shaftDistance = (float)Math.sqrt( (upperRightPt.X-upperLeftPt.X)*(upperRightPt.X-upperLeftPt.X) + 
	    		(upperRightPt.Y-upperLeftPt.Y)*(upperRightPt.Y-upperLeftPt.Y) );
	    
	    System.err.println("shaft distance = " + shaftDistance);
	    

	    if ( firstTimeCheckMidShaft == false ) {
			
			if ( shaftDistance <= 60 ) {
				if ((whichLeg == LEFT_LEG && group <= GROUP_6) || (whichLeg == RIGHT_LEG && group <= GROUP_5)) {
					if (whichLeg == LEFT_LEG) {
						close_length = 30;
					} else {
						close_length = 30;
					}
		
				}
		
				if ((whichLeg == LEFT_LEG && group >= GROUP_5) || (whichLeg == RIGHT_LEG && group >= GROUP_6)) {
					if (whichLeg == LEFT_LEG) {
						close_length = 30;
					} else {
						close_length = 30;
					}
		
				}
			} else {
				if ((whichLeg == LEFT_LEG && group <= GROUP_6) || (whichLeg == RIGHT_LEG && group <= GROUP_5)) {
					if (whichLeg == LEFT_LEG) {
						close_length = 60;
					} else {
						close_length = 60;
					}
		
				}
		
				if ((whichLeg == LEFT_LEG && group >= GROUP_5) || (whichLeg == RIGHT_LEG && group >= GROUP_6)) {
					if (whichLeg == LEFT_LEG) {
						close_length = 60;
					} else {
						close_length = 60;
					}
		
				}
			}
			firstTimeCheckMidShaft = true;
		}  
		
		identifyGroups(sliceNumber);
	   	
		if ( shaftDistance <= close_length ) {
			tracingDFS_condyle(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, sliceNumber, resultVOIBoundary,
					resultVOIInner, resultVOIOuter, leftLine, rightLine);
		} else {
			
			if ((whichLeg == RIGHT_LEG && (group <= GROUP_3 || group >= GROUP_8)) || 
					(whichLeg == LEFT_LEG && (group >= GROUP_3 || group < GROUP_8))) {
				tracingDFS_condyle(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, sliceNumber, resultVOIBoundary,
						resultVOIInner, resultVOIOuter, leftLine, rightLine);
			} else {
				tracingDFS_shaftCondyle(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, sliceNumber, resultVOIBoundary,
					resultVOIInner, resultVOIOuter, leftLine, rightLine);
			}
		}
          
	 
        /*  
		VOIBase v = resultVOIBoundary.getCurves().get(0);
		int nPts = v.size();
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		v.exportArrays(xPts, yPts, zPts);
		
		float min = 512;
		
		for ( int i = 0; i < nPts; i++ ) {
			if ( yPts[i] < min ) {
				min = yPts[i];
			}
		}
		
		if ( min >= 20 ) {
			tracingDFS_condyle(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, sliceNumber, resultVOIBoundary,
					resultVOIInner, resultVOIOuter, leftLine, rightLine);
		} else {
			tracingDFS_shaftCondyle(fatImageSlice, fuzzyCImage, class1Image, class2Image, class3Image, greImageSlice, gaussianImageSlice, sliceNumber, resultVOIBoundary,
					resultVOIInner, resultVOIOuter, leftLine, rightLine);
		}
	    */ 
		
		
	}

	private void tracingDFS_shaftCondyle(ModelImage fatImageSlice, ModelImage fuzzyCImage, ModelImage class1Image, ModelImage class2Image, ModelImage class3Image,
			ModelImage greImageSlice, ModelImage gaussianImageSlice, int sliceNumber, VOI resultVOIBoundary, VOI resultVOIInner, VOI resultVOIOuter, 
			VOILine leftLine, VOILine rightLine ) {

		
		System.err.println("tracing shaft condyle");
		float posX, posY;
		float stepX, stepY;
		int i;

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

		
		float[][] sections = new float[7][2];

		// center contour
		VOIBase v = resultVOIBoundary.getCurves().get(0);
		int nPts = v.size();
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		v.exportArrays(xPts, yPts, zPts);
		
		// get outer contour
		VOIBaseVector outer_va = resultVOIOuter.getCurves();
		VOIBase outer_v = outer_va.get(0);
		// int nPtsCurrent = outer_v.size();
		xPtsOuter = new float[nPts];
		yPtsOuter = new float[nPts];
		zPtsOuter = new float[nPts];
		outer_v.exportArrays(xPtsOuter, yPtsOuter, zPtsOuter);

		// get inner contour
		VOIBaseVector inner_va = resultVOIInner.getCurves();
		VOIBase inner_v = inner_va.get(0);
		xPtsInner = new float[nPts];
		yPtsInner = new float[nPts];
		zPtsInner = new float[nPts];
		inner_v.exportArrays(xPtsInner, yPtsInner, zPtsInner);

		// ******************************************   find the bottom condyle part contour ***************************************
		// for center contour, get the lower part, left part, and right part
		Vector3f[] ptsLower = new Vector3f[100];
		int lowerIndex = 0;
		Vector3f[] ptsLeft = new Vector3f[100];
		int leftIndex = 0;
		Vector3f[] ptsRight = new Vector3f[100];
		int rightIndex = 0;
	
		// outer, get the lower part, left part and right part 
		Vector3f[] ptsLowerOuter = new Vector3f[100];
		Vector3f[] ptsLeftOuter = new Vector3f[100];
		Vector3f[] ptsRightOuter = new Vector3f[100];
	
		// inner, get the lower part, left part and right part 
		Vector3f[] ptsLowerInner = new Vector3f[100];
		Vector3f[] ptsLeftInner = new Vector3f[100];
		Vector3f[] ptsRightInner = new Vector3f[100];
		
		for (i = 0; i < nPts; i++) {
			
		    if ( yPts[i] > cutOffPoint.Y ) {
		    	ptsLower[lowerIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	ptsLowerOuter[lowerIndex] = new Vector3f(xPtsOuter[i], yPtsOuter[i], 0f);
		    	ptsLowerInner[lowerIndex] = new Vector3f(xPtsInner[i], yPtsInner[i], 0f);
		    	lowerIndex++;
		    } 
		    
		    if ( yPts[i] <= ( cutOffPoint.Y ) && xPts[i] < cutOffPoint.X ) {
		    	ptsLeft[leftIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	ptsLeftOuter[leftIndex] = new Vector3f(xPtsOuter[i], yPtsOuter[i], 0f);
		    	ptsLeftInner[leftIndex] = new Vector3f(xPtsInner[i], yPtsInner[i], 0f);
		    	leftIndex++;
		    }
		    
		    if ( yPts[i] <= ( cutOffPoint.Y ) && xPts[i] > cutOffPoint.X ) {
		    	ptsRight[rightIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	ptsRightOuter[rightIndex] = new Vector3f(xPtsOuter[i], yPtsOuter[i], 0f);
		    	ptsRightInner[rightIndex] = new Vector3f(xPtsInner[i], yPtsInner[i], 0f);
		    	rightIndex++;
		    }
		}
		
		System.err.println("leftIndex = " + leftIndex + "  rightIndex = " + rightIndex);
		
		float sumX = 0f, sumY = 0f;
		// center contour,  find new center on the bottom condyle 
		for ( i = 0; i < lowerIndex; i++ ) {
			sumX += ptsLower[i].X;
			sumY += ptsLower[i].Y;
		}
		Vector3f condyleCenter = new Vector3f( (sumX / (float)lowerIndex), (sumY / (float)lowerIndex), 0);
		System.err.println("condyleCenter.X = " + condyleCenter.X + "  condyleCenter.Y = " + condyleCenter.Y);
		
		// **************************** test *********************
		// center,  find two lines on the shaft
				float upperLeftX = 512, upperLeftY = 512;
				float lowerLeftX = 512, lowerLeftY = 0;
				float upperRightX = 0, upperRightY = 512;
				float lowerRightX = 0, lowerRightY = 0;
				float x, y;
			
				for ( i = 0; i < leftIndex; i++ ) {
					
					x = ptsLeft[i].X;
					y = ptsLeft[i].Y;
					
					if ( y <= upperLeftY ) {
						upperLeftX = ptsLeft[i].X;
						upperLeftY = ptsLeft[i].Y;
						
					} 
					
					if ( y >= lowerLeftY ) {
						lowerLeftX =  ptsLeft[i].X;
						lowerLeftY = ptsLeft[i].Y;
					}
				}
				
				for ( i = 0; i < rightIndex; i++ ) {
					
					x = ptsRight[i].X;
					y = ptsRight[i].Y;
					
					if ( y <= upperRightY ) {
						upperRightX = x;
						upperRightY = y;
					}
					
					if ( y >= lowerRightY ) {
						lowerRightX = x;
						lowerRightY = y;
					}
				}
				
				leftLine.clear();
				leftLine.add(new Vector3f(lowerLeftX, lowerLeftY, 0f));
			    leftLine.add(new Vector3f(upperLeftX, upperLeftY, 0f));
				
			    System.err.println("Test lowerLeftX = " + lowerLeftX + "  test lowerLeftY = " + lowerLeftY);
			    System.err.println("Test upperLeftX = " + upperLeftX + " test upperLeftY = " + upperLeftY);
			    
			    rightLine.clear();
			    rightLine.add(new Vector3f(lowerRightX, lowerRightY, 0f));
			    rightLine.add(new Vector3f(upperRightX, upperRightY, 0f)); 
			    System.err.println("Test lowerRightX = " + lowerRightX + " Test lowerRightY = " + lowerRightY);
			    System.err.println("TEst upperRightX = " + upperRightX + "  test upperRightY = " + upperRightY);
			    
	    
	    //*************************************     Start tracing   *********************************************
		identifyGroups(sliceNumber);
		
		VOI resultVOI = new VOI((short) 0, "result-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(resultVOI);
		
		VOI lineVOI = new VOI((short) greImageSlice.getVOIs().size(), "line", VOI.LINE, 0);
		lineVOI.setColor(Color.green);
		VOIVector voiVectorLines = new VOIVector();
		voiVectorLines.add(lineVOI);

		Hashtable<Integer, Line> linePoints = new Hashtable<Integer, Line>();
	     
		// ****************************    tracing out the bottom section contour  ********************************************* 
		// Vector3f pt[] = new Vector3f[lowerIndex + 4];
        Vector<Vector3f> ptList = new Vector<Vector3f>();
		int linePointsIndex = 0;
		
		for (i = 0; i < lowerIndex; i++) {

			// int currentStep = 0;
			float distOuter = 0;
			float distInner = 0;
			float distCurrent = 0;

			float xOuter = ptsLowerOuter[i].X;
			float yOuter = ptsLowerOuter[i].Y;
			distOuter = (float) Math.sqrt((xOuter - condyleCenter.X) * (xOuter - condyleCenter.X) + (yOuter - condyleCenter.Y) * (yOuter - condyleCenter.Y));

			float xInner = ptsLowerInner[i].X;
			float yInner = ptsLowerInner[i].Y;
			distInner = (float) Math.sqrt((xInner - condyleCenter.X) * (xInner - condyleCenter.X) + (yInner - condyleCenter.Y) * (yInner - condyleCenter.Y));

			stepX = (xInner - condyleCenter.X) / 600f;
			stepY = (yInner - condyleCenter.Y) / 600f;

			posX = xInner;
			posY = yInner;

			float degree;

			Vector2f v_curr_inner_cartisian = new Vector2f(xInner, yInner);
			Vector2f v_curr_inner_polar = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(v_curr_inner_cartisian, v_curr_inner_polar, condyleCenter);

			degree = v_curr_inner_polar.Y;

			distCurrent = (float) Math.sqrt((posX - condyleCenter.X) * (posX - condyleCenter.X) + (posY - condyleCenter.Y) * (posY - condyleCenter.Y));

			// Trace each section differently
			
			float xPoint = ptsLower[i].X;
			float yPoint = ptsLower[i].Y;
			
			Vector2f resultPos = new Vector2f(xPoint, yPoint);
			if (degree >= section1_degree_start && degree < section1_degree_end) {
				resultPos = traceSection1(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				resultPos = traceSection2(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section3_degree_start && degree < section3_degree_end) {
				resultPos = traceSection3(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section4_degree_start && degree < section4_degree_end) {
				resultPos = traceSection4(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree > section5_degree_upperHalf_start && degree <= section5_degree_upperHalf_end) {
				resultPos = traceSection5_upper(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end) {
				resultPos = traceSection5_lower(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else { // section 6
				resultPos = traceSection6(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			}

			posX = resultPos.X;
			posY = resultPos.Y;

			// final line
			// pt[i] = new Vector3f(posX, posY, 0f);
			ptList.add(new Vector3f(posX, posY, 0f));

			// add the green tracing lines
			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(condyleCenter.X, condyleCenter.Y, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));
			lineVOI.importCurve(kLine);

			Line line = new Line(condyleCenter.X, condyleCenter.Y, posX, posY, linePointsIndex);
			linePoints.put(linePointsIndex, line);
			linePointsIndex++;

		} // end i loop;

		// *********************  Smooth bottom part condyle *************************
	
		float leftCondylePtX = 512, leftCondylePtY = 512;
		float rightCondylePtX = 512, rightCondylePtY = 512;
		float degreeLeftCondylePt = 0, degreeRightCondylePt = 0;
		
		Vector3f[] pt = new Vector3f[ptList.size()];
		for ( i = 0; i < ptList.size(); i++ ) {
			
			pt[i] = ptList.get(i);
			
			if ( pt[i].X < condyleCenter.X && pt[i].Y < leftCondylePtY ) {
					leftCondylePtY = pt[i].Y;
					leftCondylePtX = pt[i].X;
			}
			
			if ( pt[i].X > condyleCenter.X && pt[i].Y < rightCondylePtY ) {
					rightCondylePtY = pt[i].Y;
					rightCondylePtX = pt[i].X;
			}	
		}
		
		System.err.println("condyleCenter.X = " + condyleCenter.X + "  condyleCenter.Y = " + condyleCenter.Y);
		System.err.println("leftCondylePtX = " + leftCondylePtX +  "  leftCondylePtY = " + leftCondylePtY);
		System.err.println("rightCondylePtX = " + rightCondylePtX +  "  rightCondylePtY = " + rightCondylePtY);
		
		Vector2f cartesianInLeftCondylePt = new Vector2f(leftCondylePtX, leftCondylePtY);
		Vector2f polarOutLeftCondylePt = new Vector2f();
		MipavCoordinateSystems.CartesianToPolar2D(cartesianInLeftCondylePt, polarOutLeftCondylePt, condyleCenter);
		degreeLeftCondylePt = polarOutLeftCondylePt.Y;
		System.err.println("degreeLeftCondylePt = " + degreeLeftCondylePt);
		
		Vector2f cartesianInRightCondylePt = new Vector2f(rightCondylePtX, rightCondylePtY);
		Vector2f polarOutRightCondylePt = new Vector2f();
		MipavCoordinateSystems.CartesianToPolar2D(cartesianInRightCondylePt, polarOutRightCondylePt, condyleCenter);
		degreeRightCondylePt = polarOutRightCondylePt.Y;
		System.err.println("degreeRightCondylePt = " + degreeRightCondylePt);
		
		resultVOI.importCurve(pt);
		greImageSlice.addVOIs(voiVectorNew);
		smoothVOI150Single(greImageSlice, greImageSlice);
		double constant_length = 2.5d;
		resultVOI = greImageSlice.getVOIs().VOIAt(0);
		VOIBaseVector vVector = resultVOI.getCurves();
		VOIBase vT = vVector.get(0);
		
		double perimeter = vT.getLengthPtToPt(greImageSlice.getFileInfo(0).getResolutions());
		int numberPoints = (int) Math.round(perimeter / constant_length);

		float res[] = greImageSlice.getFileInfo(0).getResolutions();
		float xRes = res.length > 0 ? res[0] : 1;
		float yRes = res.length > 1 ? res[1] : 1;
		float zRes = res.length > 2 ? res[2] : 1;

		resultVOI = greImageSlice.getVOIs().VOIAt(0);
		Vector<VOIBase>[] vArray = resultVOI.getSortedCurves(VOIBase.ZPLANE, 1);
		VOIBase result_v = vArray[0].get(0);
		int nPt = result_v.size();
		float[] xPt = new float[nPt];
		float[] yPt = new float[nPt];
		float[] zPt = new float[nPt];

		float[] xNew = new float[numberPoints];
		float[] yNew = new float[numberPoints];
		float[] zNew = new float[numberPoints];

		result_v.exportArrays(xPt, yPt, zPt);
		
		// equal distance interpolation
		float x1, y1, x2, y2, distance;
		int m = 0;
		int k = 0;
		boolean find = false;

		while (k < nPt - 5) {
			x1 = xPt[k];
			y1 = yPt[k];
			find = false;
			for (int j = k + 1; j < k + 6; j++) {
				x2 = xPt[j];
				y2 = yPt[j];
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
		
		while (k < nPt) {
			x1 = xPt[k];
			y1 = yPt[k];
			find = false;
			int endPt = k + 6;
			if (k + 6 > nPt) {
				endPt = nPt;
			}
			for (int j = k + 1; j < endPt; j++) {
				x2 = xPt[j];
				y2 = yPt[j];
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

		ptList.clear();
		linePoints.clear();
		linePointsIndex = 0;
		int z;
		for (z = 0; z < m; z++) {
			
			posX = xNew[z];
			posY = yNew[z];
			
			Vector2f cartesianIn = new Vector2f(posX, posY);
			Vector2f polarOut = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(cartesianIn, polarOut, condyleCenter);
			float degreeCurrPt = polarOut.Y;
			
			if ( degreeCurrPt > degreeLeftCondylePt && degreeCurrPt < degreeRightCondylePt ) {
				continue;
			}
			
			Line line = new Line(condyleCenter.X, condyleCenter.Y, posX, posY, linePointsIndex);
			ptList.add(new Vector3f(posX, posY, 0f));
			linePoints.put(linePointsIndex, line);
			linePointsIndex++;
		}
	    
		ptList.add(new Vector3f(leftCondylePtX, leftCondylePtY, 0f));
		Line lineLeft = new Line(condyleCenter.X, condyleCenter.Y, leftCondylePtX, leftCondylePtY, linePointsIndex);
		linePoints.put(linePointsIndex, lineLeft);
		linePointsIndex++;
		
		ptList.add(new Vector3f(rightCondylePtX, rightCondylePtY, 0f));
		Line lineRight = new Line(condyleCenter.X, condyleCenter.Y, rightCondylePtX, rightCondylePtY, linePointsIndex);
		linePoints.put(linePointsIndex, lineRight);
		linePointsIndex++;
		
		
		// resultVOI.importCurve(pt);
		// greImageSlice.addVOIs(voiVectorNew);
		greImageSlice.getVOIs().removeAllElements();
		resultVOI.removeCurves();
		
		// **********************   trace out the two lines  **************************************
		
	    // ********************   find the left line points *****************************
		Vector3f[] tracedLeftPoints = new Vector3f[leftIndex];
		
		for (i = 0; i < leftIndex; i++) {

			// int currentStep = 0;
			float distOuter = 0;
			float distInner = 0;
			float distCurrent = 0;

			float xOuter = 0, yOuter = 0;
			float xInner = 0, yInner = 0;
			
			xOuter = ptsLeftOuter[i].X;
			yOuter = ptsLeftOuter[i].Y;
				
			xInner = ptsLeftInner[i].X;
			yInner = ptsLeftInner[i].Y;
			
			distOuter = (float) Math.sqrt((xOuter - condyleCenter.X) * (xOuter - condyleCenter.X) + (yOuter - condyleCenter.Y) * (yOuter - condyleCenter.Y));
			distInner = (float) Math.sqrt((xInner - condyleCenter.X) * (xInner - condyleCenter.X) + (yInner - condyleCenter.Y) * (yInner - condyleCenter.Y));

			stepX = (xInner - condyleCenter.X) / 600;
			stepY = (yInner - condyleCenter.Y) / 600;

			posX = xInner;
			posY = yInner;

			float degree;

			Vector2f v_curr_inner_cartisian = new Vector2f(xInner, yInner);
			Vector2f v_curr_inner_polar = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(v_curr_inner_cartisian, v_curr_inner_polar, condyleCenter);

			degree = v_curr_inner_polar.Y;

			distCurrent = (float) Math.sqrt((posX - condyleCenter.X) * (posX - condyleCenter.X) + (posY - condyleCenter.Y) * (posY - condyleCenter.Y));

			// Trace each section differently
			Vector2f resultPos = new Vector2f(xOuter, yOuter);
			if (degree >= section1_degree_start && degree < section1_degree_end) {
				resultPos = traceSection1(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				resultPos = traceSection2(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section3_degree_start && degree < section3_degree_end) {
				resultPos = traceSection3(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section4_degree_start && degree < section4_degree_end) {
				resultPos = traceSection4(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree > section5_degree_upperHalf_start && degree <= section5_degree_upperHalf_end) {
				resultPos = traceSection5_upper(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end) {
				resultPos = traceSection5_lower(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else { // section 6
				resultPos = traceSection6(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			}
			
			posX = resultPos.X;
			posY = resultPos.Y;

			tracedLeftPoints[i] = new Vector3f(posX, posY, 0f);
		} // end i loop;

		upperLeftX = 512; upperLeftY = 512;
		lowerLeftX = 512; lowerLeftY = 0;
	   
		double sx = 0.0, sy = 0.0, stt = 0.0, sts = 0.0;
		for (i = 0; i < leftIndex; ++i)
		{
			x = tracedLeftPoints[i].X;
			y = tracedLeftPoints[i].Y;
			
			if ( y <= upperLeftY ) {
				upperLeftX = x;
				upperLeftY = y;
				
			} 
			
			if ( y >= lowerLeftY ) {
				lowerLeftX = x;
				lowerLeftY = y;
			} 
			
			sx += x;
			sy += y;
		}
		for (i = 0; i < leftIndex; ++i)
		{
			x = tracedLeftPoints[i].X;
			y = tracedLeftPoints[i].Y;
			
			double t = x - sx/leftIndex;
			stt += t*t;
			sts += t*y;
		}

		double slope = sts/stt;
		double intercept = (sy - sx*slope)/leftIndex;
		
		upperLeftY = 0;
		upperLeftX = (float)((upperLeftY - intercept) / slope);
		System.err.println("upperLeftX = " + upperLeftX + "  upperLeftY = " + upperLeftY + " intercept = " + intercept + " slope = " + slope);
		
	
		
	
		if ( ( whichLeg == LEFT_LEG && group <= GROUP_6) ||  
				 ( whichLeg == RIGHT_LEG && group <= GROUP_5 ) ) {
			if (Math.abs(slope) <= 1.0E-10f || Float.isInfinite(Math.abs((float)slope))) {

			} else {
				if (((upperLeftY - intercept) / slope) > 1) {
					float upperLeftXTemp = (float)((upperLeftY - intercept) / slope);
					if (upperLeftXTemp < (upperLeftX + 40)) {
						upperLeftX = upperLeftXTemp;
					}
				}
			}

			if ( Float.isNaN(upperLeftX ) ) {
				upperLeftX = lowerLeftX;
			}
			
			if (upperLeftX < (lowerLeftX)) {
				upperLeftX = lowerLeftX;
			}

			if (upperLeftX > (lowerLeftX + 40)) {
				upperLeftX = lowerLeftX + 40;
			}
		}
	
	
		if ( (whichLeg == LEFT_LEG && group >= GROUP_5 ) || 
			 ( whichLeg == RIGHT_LEG && group >= GROUP_6 )) {
			if ( Math.abs(slope) <= 1.0E-10f || Float.isInfinite(Math.abs((float)slope) )) {
				
			} else {
				if (  ( ( upperLeftY - intercept ) / slope ) >  1 ) {
					float upperLeftXTemp =  (float)(( upperLeftY - intercept ) / slope);
					if ( upperLeftXTemp < (upperLeftX + 40 ) ) { 
						upperLeftX = upperLeftXTemp;
					}
				}
			}
			
			if ( Float.isNaN(upperLeftX ) ) {
				upperLeftX = lowerLeftX;
			}
			
			if ( upperLeftX < ( lowerLeftX ) ) {
				upperLeftX = lowerLeftX;
			}
			
			if ( upperLeftX > ( lowerLeftX + 40 ) ) {
				upperLeftX = lowerLeftX + 40;
			}
		}
		 
		
		
		  // ********************   find the right line points *****************************
		Vector3f[] tracedRightPoints = new Vector3f[rightIndex];
		
		for (i = 0; i < rightIndex; i++) {

			// int currentStep = 0;
			float distOuter = 0;
			float distInner = 0;
			float distCurrent = 0;

			float xOuter = 0, yOuter = 0;
			float xInner = 0, yInner = 0;
			
			xOuter = ptsRightOuter[i].X;
			yOuter = ptsRightOuter[i].Y;
				
			xInner = ptsRightInner[i].X;
			yInner = ptsRightInner[i].Y;
			
			distOuter = (float) Math.sqrt((xOuter - condyleCenter.X) * (xOuter - condyleCenter.X) + (yOuter - condyleCenter.Y) * (yOuter - condyleCenter.Y));
			distInner = (float) Math.sqrt((xInner - condyleCenter.X) * (xInner - condyleCenter.X) + (yInner - condyleCenter.Y) * (yInner - condyleCenter.Y));

			stepX = (xInner - condyleCenter.X) / 600;
			stepY = (yInner - condyleCenter.Y) / 600;

			posX = xInner;
			posY = yInner;

			float degree;

			Vector2f v_curr_inner_cartisian = new Vector2f(xInner, yInner);
			Vector2f v_curr_inner_polar = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(v_curr_inner_cartisian, v_curr_inner_polar, condyleCenter);

			degree = v_curr_inner_polar.Y;

			distCurrent = (float) Math.sqrt((posX - condyleCenter.X) * (posX - condyleCenter.X) + (posY - condyleCenter.Y) * (posY - condyleCenter.Y));

			// Trace each section differently
			Vector2f resultPos = new Vector2f(xOuter, yOuter);
			if (degree >= section1_degree_start && degree < section1_degree_end) {
				resultPos = traceSection1(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				resultPos = traceSection2(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section3_degree_start && degree < section3_degree_end) {
				resultPos = traceSection3(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section4_degree_start && degree < section4_degree_end) {
				resultPos = traceSection4(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree > section5_degree_upperHalf_start && degree <= section5_degree_upperHalf_end) {
				resultPos = traceSection5_upper(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else if (degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end) {
				resultPos = traceSection5_lower(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			} else { // section 6
				resultPos = traceSection6(posX, posY, stepX, stepY, condyleCenter, degree, sliceNumber, distCurrent, distOuter, greImageSlice,
						class1Image, class2Image, class3Image, fuzzyCImage);
			}
			
			posX = resultPos.X;
			posY = resultPos.Y;

			tracedRightPoints[i] = new Vector3f(posX, posY, 0f);
		} // end i loop;

		
		upperRightX = 0; upperRightY = 512;
		lowerRightX = 0; lowerRightY = 0;
	
		sx = 0.0; sy = 0.0; stt = 0.0; sts = 0.0;
		for (i = 0; i < rightIndex; ++i)
		{
			x = tracedRightPoints[i].X;
			y = tracedRightPoints[i].Y;
			
			if ( y <= upperRightY ) {
				upperRightX = x;
				upperRightY = y;
			}
			
			if ( y >= lowerRightY ) {
				lowerRightX = x;
				lowerRightY = y;
			}
			
			sx += x;
			sy += y;
		}
		for (i = 0; i < rightIndex; ++i)
		{
			x = tracedRightPoints[i].X;
			y = tracedRightPoints[i].Y;
			double t = x - sx/rightIndex;
			stt += t*t;
			sts += t*y;
		}

		slope = sts/stt;
		intercept = (sy - sx*slope)/rightIndex;
		
		upperRightY = 0;
		upperRightX = (float)(( upperRightY - intercept ) / slope);
		System.err.println("upperRightX = " + upperRightX + "  upperRightY = " + upperRightY + " intercept = " + intercept + " slope = " + slope);		
		
		
	    if ( ( whichLeg == LEFT_LEG && group <= GROUP_6) ||  
				 ( whichLeg == RIGHT_LEG && group <= GROUP_5 ) ) {
	    	// upperRightX = ( upperRightY - YInt ) / Slope;
	    	
	    	if ( Math.abs(slope) <= 1.0E-10f || Float.isInfinite(Math.abs((float)slope) )) {
				
			} else {
				
				if ( (( upperRightY - intercept ) / slope) > 1 ) {
					float upperRightXTemp =  (float)(( upperRightY - intercept ) / slope);
					if ( upperRightXTemp > ( upperRightX - 30 )) {
						upperRightX = upperRightXTemp;
					}
				}
				
				
			}
			
	    	if ( Float.isNaN(upperRightX) ) {
				upperRightX = lowerRightX;
			}
			
	    	
			if ( upperRightX > ( lowerRightX + 10) ) {
				upperRightX = lowerRightX + 10;
			}
			
			if ( upperRightX < ( lowerRightX - 10 ) ) {
				upperRightX = lowerRightX - 10;
			}
	    	 
	    }
	    
	    if ( (whichLeg == LEFT_LEG && group >= GROUP_5 ) || 
				 ( whichLeg == RIGHT_LEG && group >= GROUP_6 )) {
			if ( Math.abs(slope) <= 1.0E-10f || Float.isInfinite(Math.abs((float)slope) )) {
				
			} else {
				
				if ( (( upperRightY - intercept ) / slope) > 1 ) {
					float upperRightXTemp =  (float)(( upperRightY - intercept ) / slope);
					if ( upperRightXTemp > ( upperRightX - 30 )) {
						upperRightX = upperRightXTemp;
					}
				}
				
				
			}
			
			if ( Float.isNaN(upperRightX) ) {
				upperRightX = lowerRightX;
			}
			
			
			if ( upperRightX > ( lowerRightX + 5) ) {
				upperRightX = lowerRightX + 5;
			}
			
			if ( upperRightX < ( lowerRightX - 10 ) ) {
				upperRightX = lowerRightX - 10;
			}
		 
	    }
	    
	    
		// VOILine leftLine = new VOILine();
		leftLine.clear();
		leftLine.add(new Vector3f(lowerLeftX, lowerLeftY, 0f));
	    leftLine.add(new Vector3f(upperLeftX, upperLeftY, 0f));
		
	    System.err.println("lowerLeftX = " + lowerLeftX + " lowerLeftY = " + lowerLeftY);
	    System.err.println("upperLeftX = " + upperLeftX + " upperLeftY = " + upperLeftY);
	    
	    // VOILine rightLine = new VOILine();
	    rightLine.clear();
		rightLine.add(new Vector3f(lowerRightX, lowerRightY, 0f));
	    rightLine.add(new Vector3f(upperRightX, upperRightY, 0f));

	    System.err.println("lowerRightX = " + lowerRightX + " lowerRightY = " + lowerRightY);
	    System.err.println("upperRightX = " + upperRightX + " upperRightY = " + upperRightY);

		float walkStepX, walkStepY;
		float currX, currY;
		// int k;
		
		// walk from left side lower point to upper point
		walkStepX = (upperLeftX - lowerLeftX ) / 10.0f;
		walkStepY = (upperLeftY - lowerLeftY ) / 10.0f;
		
		currX = lowerLeftX;
		currY = lowerLeftY;
		
		k = 0;
		
		for ( k = 0; k <= 10; k++ ) {
			ptList.add(new Vector3f(currX, currY, 0f));
			linePoints.put(linePointsIndex, new Line(condyleCenter.X, condyleCenter.Y, currX, currY, linePointsIndex));
			linePointsIndex++;	
			
			currX += walkStepX;
			currY += walkStepY;
		}
		
		/*
		// walk from left side upper point to right side upper point
		walkStepX = ( upperRightX - upperLeftX ) / 10.0f;
		walkStepY = ( upperRightY - upperLeftY ) / 10.0f;
		
		currX = upperLeftX;
		currY = upperLeftY;
		
		for ( k = 0; k <= 10; k++ ) {
			ptList.add(new Vector3f(currX, currY, 0f));
			linePoints.put(linePointsIndex, new Line(condyleCenter.X, condyleCenter.Y, currX, currY, linePointsIndex));
			linePointsIndex++;
			
			currX += walkStepX;
			currY += walkStepY;
		}
		*/ 
	
		
		// walk from right side upper point to right side lower point
		walkStepX = ( lowerRightX - upperRightX ) / 10.0f;
		walkStepY = ( lowerRightY - upperRightY ) / 10.0f;
		
		currX = upperRightX;
		currY = upperRightY;
		// int k = 0; 
		
		for ( k = 0; k <= 10; k++ ) {
			ptList.add(new Vector3f(currX, currY, 0f));
			linePoints.put(linePointsIndex, new Line(condyleCenter.X, condyleCenter.Y, currX, currY, linePointsIndex));
			linePointsIndex++;
			
			currX += walkStepX;
			currY += walkStepY;
		}
	
		Hashtable<Float, Line> tempLinePoints = new Hashtable<Float, Line>();
		int index = 0;
		float centerX, centerY;
		float degree;
		int ptsSize = ptList.size();
		for ( i = 0; i < ptsSize; i++ ) {
			
			Line line = linePoints.get(i);
			Vector3f point = ptList.get(i);
			
			centerX = line.startX;
			centerY = line.startY;
			
			posX = line.endX;
			posY = line.endY;
			
			Vector2f cartesianIn = new Vector2f(posX, posY);
			Vector2f polarOut = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(cartesianIn, polarOut, condyleCenter);
			degree = polarOut.Y;
			
			tempLinePoints.put(degree, new Line(condyleCenter.X, condyleCenter.Y, posX, posY, index));
			index++;
		}
		
		linePoints.clear();
		ptList.clear();
		index = 0;
		
		ArrayList<Float> temp = Collections.list(tempLinePoints.keys());
		Collections.sort(temp);
		Iterator<Float> it = temp.iterator();
		
		while ( it.hasNext() ) {
			float key = it.next();
			Line line = tempLinePoints.get(key);
			if ( line != null ) {
				linePoints.put(index, line);
				// pt[index] = new Vector3f(line.endX, line.endY, 0);
				ptList.add(new Vector3f(line.endX, line.endY, 0));
				index++;
			}
		}
	
		ptsSize = ptList.size();
		Vector3f[] pts = new Vector3f[ptsSize];
		for (k = 0; k < ptsSize; k++ ) {
			pts[k] = ptList.get(k);
		}
				
		// *****************************    Post-processing   **************************************************** 
		resultVOI.removeCurves();
		greImageSlice.getVOIs().removeAllElements();
		voiVectorNew.removeAllElements();
		voiVectorNew.add(resultVOI);
		resultVOI.importCurve(pts);
		greImageSlice.addVOIs(voiVectorNew);
		
		slicesPts.put(sliceNumber, linePoints);

		// Cheng1!!!!!
		// generate inner and outer contours
	    // findBoundingContour(sliceNumber, resultVOIBoundary, resultVOIInner, resultVOIOuter, center, fatImageSlice);
	    findBoundingContour(sliceNumber, resultVOIBoundary, resultVOIInner, resultVOIOuter, condyleCenter, fatImageSlice, leftLine, rightLine);

		// copy result VOI from 2D slice to 3D slice.
		// resultVOI.removeCurves();
		// VOIBase vTemp = (VOIBase) result_v.clone();

		// vTemp.importArrays(xNew, yNew, zNew, m);
	    // resultVOI.importCurve(vTemp);
	    // smoothVOISingle(greImageSlice, greImageSlice, m);

		fuzzyCImage.addVOIs(voiVectorNew);
		class1Image.addVOIs(voiVectorNew);
		class2Image.addVOIs(voiVectorNew);
		// new ViewJFrameImage(greImageSlice);    // Cheng1  emergency
		// new ViewJFrameImage(class1Image);
		// new ViewJFrameImage(class2Image);
		// new ViewJFrameImage(class3Image);
		// new ViewJFrameImage(fuzzyCImage);

		// VOIBaseVector current_va =
		// greImageSlice.getVOIs().VOIAt(2).getCurves();
		VOIBaseVector current_va = greImageSlice.getVOIs().VOIAt(0).getCurves();
		if (current_va.size() > 0) {
			VOIBase current_v = current_va.get(0);

			VOIBase vTemp = (VOIBase) current_v.clone();

			int nPtsCurrent = current_v.size();
			
			if ( nPtsCurrent == 0 ) return;
			
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

		// pause();
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

	public static final double distance(final Vector3f pt1, final Vector3f pt2, final float[] res) {
		float xRes = res.length > 0 ? res[0] : 1;
		float yRes = res.length > 1 ? res[1] : 1;
		float zRes = res.length > 2 ? res[2] : 1;
		return Math.sqrt(((pt2.X - pt1.X) * (pt2.X - pt1.X) * (xRes * xRes)) + ((pt2.Y - pt1.Y) * (pt2.Y - pt1.Y) * (yRes * yRes))
				+ ((pt2.Z - pt1.Z) * (pt2.Z - pt1.Z) * (zRes * zRes)));
	}

	private void findBoundingContour(int sliceNumber, VOI resultVOIBoundary,
			VOI resultVOIInner, VOI resultVOIOuter, Vector3f center, ModelImage fatImageSlice,
			VOILine leftLine, VOILine rightLine ) {

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

		VOI section5_lower = new VOI((short) 4, "line5lower", VOI.LINE, 0);
		section5_lower.setColor(Color.yellow);
		voiVectorLines.add(section5_lower);
		
		VOI section5_upper = new VOI((short) 5, "line5upper", VOI.LINE, 0);
		section5_upper.setColor(Color.white);
		voiVectorLines.add(section5_upper);

		VOI section6 = new VOI((short) 6, "line6", VOI.LINE, 0);
		section6.setColor(Color.cyan);
		voiVectorLines.add(section6);

		if ( leftLine != null ) {
			VOI left = new VOI((short) 7, "left", VOI.LINE, 0);
			left.setColor(Color.pink);
			voiVectorLines.add(left);
			left.importCurve(leftLine);
		}

		if ( rightLine != null ) {
			VOI right = new VOI((short) 8, "right", VOI.LINE, 0);
			right.setColor(Color.pink);
			voiVectorLines.add(right);
			right.importCurve(rightLine);
		}

		// double distanceCutoff = Math.sqrt(( cutOffPoint.X - centerX) * (cutOffPoint.X - centerX) + (cutOffPoint.Y - centerY) * (cutOffPoint.Y - centerY));
		// double distanceInnerCutoff = distanceCutoff * (1d - 0.05d);
		// double distanceOuterCutoff = distanceCutoff * (1d + 0.05d);
	
		
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
			
			// System.err.println("degree = " + degree);

			distance = Math.sqrt((posX - centerX) * (posX - centerX) + (posY - centerY) * (posY - centerY));
			/// distanceInner = distance * (1d - 0.10d);
			distanceInner = distance * ( 1d - 0.4d );
			
			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(center.X, center.Y, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));

			if (degree >= section1_degree_start && degree < section1_degree_end) {
				 if ( whichLeg == LEFT_LEG ) {
					   if ( group == GROUP_1 ) {
						    distanceOuter = distance * (1d + 0.05d);
						    distanceInner = distance * (1d - 0.30d);
						} else if ( group == GROUP_2 ) {
						    distanceOuter = distance * (1d + 0.05d);
						    distanceInner = distance * (1d - 0.20d);
						} else if ( group == GROUP_3 ) {
						    distanceOuter = distance * (1d + 0.05d);
						    distanceInner = distance * (1d - 0.20d);
						} else if ( group == GROUP_4 ) {
						    distanceOuter = distance * (1d + 0.02d);
						    distanceInner = distance * (1d - 0.25d);
						} else if ( group == GROUP_5 ) {
						    distanceOuter = distance * (1d + 0.02d);
						    distanceInner = distance * (1d - 0.05d);
						} else if ( group == GROUP_6 ) {
						    distanceOuter = distance * (1d + 0.03d);
						    distanceInner = distance * (1d - 0.03d);
						} else if ( group == GROUP_7 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.10d);
						} else if ( group == GROUP_8 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.40d);
						} else if ( group == GROUP_9 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.40d);
						} else if ( group == GROUP_10 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.40d);
						} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.70d);
					} 
				}
				
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
						distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_8 ) {
						distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.40d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
						distanceOuter = distance * (1d + 0.10d);
						distanceInner = distance * (1d - 0.03d);
					} else if ( group == GROUP_7 ) {
						distanceOuter = distance * (1d + 0.10d);
						distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.40d);
					} 
				}
				
			} else if ((degree >= section3_degree_start && degree < section3_degree_end)) {
				 if ( whichLeg == LEFT_LEG ) {
					 if ( group == GROUP_1 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.30d);
						} else if ( group == GROUP_2 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.20d);
						} else if ( group == GROUP_3 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.15d);
						} else if ( group == GROUP_4 ) {
						    distanceOuter = distance * (1d + 0.05d);
						    distanceInner = distance * (1d - 0.15d);
						} else if ( group == GROUP_5 ) {
						    distanceOuter = distance * (1d + 0.15d);
						    distanceInner = distance * (1d - 0.05d);
						} else if ( group == GROUP_6 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.05d);
						} else if ( group == GROUP_7 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.05d);
						} else if ( group == GROUP_8 ) {
						    distanceOuter = distance * (1d + 0.15d);
						    distanceInner = distance * (1d - 0.20d);
						} else if ( group == GROUP_9 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.25d);
						} else if ( group == GROUP_10 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.80d);
						} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.80d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.30d);
					} 
				}

			} else if ((degree >= section4_degree_start && degree < section4_degree_end)) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.30d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.35d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.25d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.30d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.25d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				}
			} else if ((degree >= section5_degree_upperHalf_start && degree < section5_degree_upperHalf_end)) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.07d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.35d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_8 ) {
						distanceOuter = distance * (1d + 0.25d);
						distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.30d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} 
				} 
			} else if ((degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end) ) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.07d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.40d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_8 ) {
						distanceOuter = distance * (1d + 0.35d);
						distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.30d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.25d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} 
				} 
			} else if (degree >= section6_degree_start && degree < section6_degree_end) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.07d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.08d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.70d);
					} 
				}
			}
		
			
			
			if ((degree >= section1_degree_start && degree < section1_degree_end)) {
				section1.importCurve(kLine);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				section2.importCurve(kLine);
			} else if ((degree >= section3_degree_start && degree < section3_degree_end)) {
				section3.importCurve(kLine);
			} else if ((degree >= section4_degree_start && degree < section4_degree_end)) {
				section4.importCurve(kLine);
			} else if (degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end) {
				section5_lower.importCurve(kLine);
			} else if (degree >= section5_degree_upperHalf_start && degree < section5_degree_upperHalf_end) {
				section5_upper.importCurve(kLine);
			} else if (degree >= section6_degree_start && degree < section6_degree_end) {
				section6.importCurve(kLine);
			}
			
			pt_boundary[i] = new Vector3f(posX, posY, 0f);

			stepX = (posX - centerX) / 800f;
			stepY = (posY - centerY) / 800f;

			// System.err.println("stepX = " + stepX + "  stepY = " + stepY);
			
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

		fatImageSlice.addVOIs(voiVectorLines);
		// new ViewJFrameImage(fatImageSlice);   // Cheng1, emergency

		ModelImage cloneImage = (ModelImage) fatImageSlice.clone();
		VOIVector voiVectorInOut = new VOIVector();
		voiVectorInOut.add(resultVOIBoundary);
		voiVectorInOut.add(resultVOIInner);
		voiVectorInOut.add(resultVOIOuter);
		cloneImage.addVOIs(voiVectorInOut);
		// new ViewJFrameImage(cloneImage);         // Cheng1, emergency
	}
	
	
	private void findBoundingContour_condyle(int sliceNumber, VOI resultVOIBoundary,
			VOI resultVOIInner, VOI resultVOIOuter, Vector3f center, ModelImage fatImageSlice,
			VOILine leftLine, VOILine rightLine ) {

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

		VOI section5_lower = new VOI((short) 4, "line5lower", VOI.LINE, 0);
		section5_lower.setColor(Color.yellow);
		voiVectorLines.add(section5_lower);
		
		VOI section5_upper = new VOI((short) 5, "line5upper", VOI.LINE, 0);
		section5_upper.setColor(Color.white);
		voiVectorLines.add(section5_upper);

		VOI section6 = new VOI((short) 6, "line6", VOI.LINE, 0);
		section6.setColor(Color.cyan);
		voiVectorLines.add(section6);

		if ( leftLine != null ) {
			VOI left = new VOI((short) 7, "left", VOI.LINE, 0);
			left.setColor(Color.pink);
			voiVectorLines.add(left);
			left.importCurve(leftLine);
		}

		if ( rightLine != null ) {
			VOI right = new VOI((short) 8, "right", VOI.LINE, 0);
			right.setColor(Color.pink);
			voiVectorLines.add(right);
			right.importCurve(rightLine);
		}

		
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
			
			// System.err.println("degree = " + degree);

			distance = Math.sqrt((posX - centerX) * (posX - centerX) + (posY - centerY) * (posY - centerY));
			/// distanceInner = distance * (1d - 0.10d);
			distanceInner = distance * ( 1d - 0.4d );
			
			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(center.X, center.Y, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));

			if (degree >= section1_degree_start && degree < section1_degree_end) {
				 if ( whichLeg == LEFT_LEG ) {
					   if ( group == GROUP_1 ) {
						    distanceOuter = distance * (1d + 0.05d);
						    distanceInner = distance * (1d - 0.30d);
						} else if ( group == GROUP_2 ) {
						    distanceOuter = distance * (1d + 0.05d);
						    distanceInner = distance * (1d - 0.20d);
						} else if ( group == GROUP_3 ) {
						    distanceOuter = distance * (1d + 0.05d);
						    distanceInner = distance * (1d - 0.20d);
						} else if ( group == GROUP_4 ) {
						    distanceOuter = distance * (1d + 0.02d);
						    distanceInner = distance * (1d - 0.25d);
						} else if ( group == GROUP_5 ) {
						    distanceOuter = distance * (1d + 0.02d);
						    distanceInner = distance * (1d - 0.05d);
						} else if ( group == GROUP_6 ) {
						    distanceOuter = distance * (1d + 0.03d);
						    distanceInner = distance * (1d - 0.15d);
						} else if ( group == GROUP_7 ) {
						    distanceOuter = distance * (1d + 0.03d);
						    distanceInner = distance * (1d - 0.40d);
						} else if ( group == GROUP_8 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.40d);
						} else if ( group == GROUP_9 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.40d);
						} else if ( group == GROUP_10 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.40d);
						} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.70d);
					} 
				}
				
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    // distanceOuter = distance * (1d + 0.05d);
						if ( degree >=  section2_degree_start && degree <= section2_degree_start + 30 ) {
							distanceOuter = distance * (1d + 0.10d);
						} else {
							distanceOuter = distance * (1d + 0.02d);
						}
					    distanceInner = distance * (1d - 0.0d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_8 ) {
						if ( degree >=  section2_degree_start && degree <= section2_degree_start + 30 ) {
							distanceOuter = distance * (1d + 0.20d);
						} else {
							distanceOuter = distance * (1d + 0.05d);
						}
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.40d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    // distanceOuter = distance * (1d + 0.15d);
					    // distanceInner = distance * (1d - 0.05d);
					    if ( degree >=  section2_degree_start && degree <= section2_degree_start + 30 ) {
							distanceOuter = distance * (1d + 0.20d);
							distanceInner = distance * (1d - 0.10d);
						} else {
							distanceOuter = distance * (1d + 0.05d);
							distanceInner = distance * (1d - 0.05d);
						}
					    
					} else if ( group == GROUP_7 ) {
						if ( degree >=  section2_degree_start && degree <= section2_degree_start + 30 ) {
							distanceOuter = distance * (1d + 0.20d);
							distanceInner = distance * (1d - 0.10d);
						} else {
							distanceOuter = distance * (1d + 0.05d);
							distanceInner = distance * (1d - 0.05d);
						}
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.40d);
					} 
				}
				
			} else if ((degree >= section3_degree_start && degree < section3_degree_end)) {
				 if ( whichLeg == LEFT_LEG ) {
					 if ( group == GROUP_1 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.30d);
						} else if ( group == GROUP_2 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.20d);
						} else if ( group == GROUP_3 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.15d);
						} else if ( group == GROUP_4 ) {
						    distanceOuter = distance * (1d + 0.05d);
						    distanceInner = distance * (1d - 0.15d);
						} else if ( group == GROUP_5 ) {
						    distanceOuter = distance * (1d + 0.15d);
						    distanceInner = distance * (1d - 0.05d);
						} else if ( group == GROUP_6 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.05d);
						} else if ( group == GROUP_7 ) {
						    distanceOuter = distance * (1d + 0.10d);
						    distanceInner = distance * (1d - 0.05d);
						} else if ( group == GROUP_8 ) {
						    distanceOuter = distance * (1d + 0.15d);
						    distanceInner = distance * (1d - 0.20d);
						} else if ( group == GROUP_9 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.25d);
						} else if ( group == GROUP_10 ) {
						    distanceOuter = distance * (1d + 0.00d);
						    distanceInner = distance * (1d - 0.80d);
						} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.80d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.30d);
					} 
				}

			} else if ((degree >= section4_degree_start && degree < section4_degree_end)) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.15d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.30d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.30d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.25d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				}
			} else if ((degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end)
					|| (degree >= section5_degree_upperHalf_start && degree < section5_degree_upperHalf_end)) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.07d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.40d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.10d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_8 ) {
						
						if (degree >= section5_degree_upperHalf_start && degree < section5_degree_upperHalf_end  ) {
							distanceOuter = distance * (1d + 0.05d);
						} else {
							distanceOuter = distance * (1d + 0.30d);
						}
						 
						// distanceOuter = distance * (1d + 0.15d);
						distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.30d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} 
				} 
			} else if (degree >= section6_degree_start && degree < section6_degree_end) {
				if ( whichLeg == LEFT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.50d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.07d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.40d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.00d);
					    distanceInner = distance * (1d - 0.80d);
					} 
				} else if ( whichLeg == RIGHT_LEG ) {
					if ( group == GROUP_1 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_2 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.20d);
					} else if ( group == GROUP_3 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_4 ) {
					    distanceOuter = distance * (1d + 0.20d);
					    distanceInner = distance * (1d - 0.30d);
					} else if ( group == GROUP_5 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.15d);
					} else if ( group == GROUP_6 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.05d);
					} else if ( group == GROUP_7 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_8 ) {
					    distanceOuter = distance * (1d + 0.10d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_9 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.25d);
					} else if ( group == GROUP_10 ) {
					    distanceOuter = distance * (1d + 0.05d);
					    distanceInner = distance * (1d - 0.70d);
					} 
				}
			}
		
			
			
			if ((degree >= section1_degree_start && degree < section1_degree_end)) {
				section1.importCurve(kLine);
			} else if (degree >= section2_degree_start && degree < section2_degree_end) {
				section2.importCurve(kLine);
			} else if ((degree >= section3_degree_start && degree < section3_degree_end)) {
				section3.importCurve(kLine);
			} else if ((degree >= section4_degree_start && degree < section4_degree_end)) {
				section4.importCurve(kLine);
			} else if (degree >= section5_degree_lowerHalf_start && degree < section5_degree_lowerHalf_end) {
				section5_lower.importCurve(kLine);
			} else if (degree >= section5_degree_upperHalf_start && degree < section5_degree_upperHalf_end) {
				section5_upper.importCurve(kLine);
			} else if (degree >= section6_degree_start && degree < section6_degree_end) {
				section6.importCurve(kLine);
			}
			
			pt_boundary[i] = new Vector3f(posX, posY, 0f);

			stepX = (posX - centerX) / 800f;
			stepY = (posY - centerY) / 800f;

			// System.err.println("stepX = " + stepX + "  stepY = " + stepY);
			
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

		fatImageSlice.addVOIs(voiVectorLines);
	    // new ViewJFrameImage(fatImageSlice);   // Cheng1, emergency

		ModelImage cloneImage = (ModelImage) fatImageSlice.clone();
		VOIVector voiVectorInOut = new VOIVector();
		voiVectorInOut.add(resultVOIBoundary);
		voiVectorInOut.add(resultVOIInner);
		voiVectorInOut.add(resultVOIOuter);
		cloneImage.addVOIs(voiVectorInOut);
		// new ViewJFrameImage(cloneImage);         // Cheng1, emergency
	}

	private void findSections(Vector3f[] inContour, int contourSize, float[][] sections, Vector3f center) {

		int i;
		float posX, posY;

		// add mid slice VOI to the GRE image
		// Vector<VOIBase>[] vArray = boundary.getSortedCurves(VOIBase.ZPLANE,
		// 1);
		// VOIBase v = vArray[0].get(0);

		if (contourSize > 0) {

			int nPts = contourSize;
			
			float sumX = 0, sumY = 0;

			float yMax_x = Float.MIN_VALUE, yMax_y = Float.MIN_VALUE, yMin_x = Float.MAX_VALUE, yMin_y = Float.MAX_VALUE;
			float xMax_x = Float.MIN_VALUE, xMax_y = Float.MIN_VALUE, xMin_x = Float.MAX_VALUE, xMin_y = Float.MAX_VALUE;

			for (i = 0; i < nPts; i++) {

				posX = inContour[i].X;
				posY = inContour[i].Y;

				// sumX += posX;
				// sumY += posY;

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

			// System.err.println("Before:  center.X = " + center.X + "  center.Y = " + center.Y + "   center.Z = " + center.Z);

			center.X = (xMax_x + xMin_x) / 2.0f;
			center.Y = (yMax_y + yMin_y) / 2.0f;


			// System.err.println("After:  center.X = " + center.X +  "  center.Y = " + center.Y + "   center.Z = " + center.Z);

			Vector2f zero_ref_point = new Vector2f(center.X, yMin_y);
			Vector2f zero_ref_polar = new Vector2f();
			
			Vector2f pi_ref_point = new Vector2f(center.X, yMax_y);
			Vector2f pi_ref_polar = new Vector2f();
			
			Vector2f left_most_pt = new Vector2f(xMin_x, center.Y);
			Vector2f left_most_polar = new Vector2f();
			
			Vector2f right_most_pt = new Vector2f(xMax_x, center.Y);
			Vector2f right_most_polar = new Vector2f();

			MipavCoordinateSystems.CartesianToPolar2D(zero_ref_point, zero_ref_polar, center);
			MipavCoordinateSystems.CartesianToPolar2D(pi_ref_point, pi_ref_polar, center);
			MipavCoordinateSystems.CartesianToPolar2D(left_most_pt, left_most_polar, center);
			MipavCoordinateSystems.CartesianToPolar2D(right_most_pt, right_most_polar, center);

			// System.err.println("zero_ref_polar.Y " + zero_ref_polar.Y);
			// System.err.println("left_most_polar.Y " + left_most_polar.Y);
			// System.err.println("pi_ref_polar.Y " + pi_ref_polar.Y);
			// System.err.println("right_most_polar.Y " + right_most_polar.Y);
			
			// red color section
			float section1_start = zero_ref_polar.Y - 45;
			float section1_end = zero_ref_polar.Y;

			// System.err.println("section1_degree_start = " + section1_start + "  section1_degree_end = " + section1_end);
			sections[0][0] = section1_start;
			sections[0][1] = section1_end;

			// green color section
			float section2_start = left_most_polar.Y;
			float section2_end = zero_ref_polar.Y - 45;
			// System.err.println("section2_degree_start = " + section2_start + "  section2_degree_end = " + section2_end);
			sections[1][0] = section2_start;
			sections[1][1] = section2_end;

			// blue color section
			float section3_start = pi_ref_polar.Y;
			float section3_end = left_most_polar.Y;
			// System.err.println("section3_degree_start = " + section3_start + "  section3_degree_end = " + section3_end);
			sections[2][0] = section3_start;
			sections[2][1] = section3_end;

			// pink color section
			float section4_start = pi_ref_polar.Y - 45;
			float section4_end = pi_ref_polar.Y;
			// System.err.println("section4_degree_start = " + section4_start + "  section4_degree_end = " + section4_end);
			sections[3][0] = section4_start;
			sections[3][1] = section4_end;

			// yellow color section
			float section5_upperHalf_start = zero_ref_polar.Y + 45;
			float section5_upperHalf_end = 360;
		    // System.err.println("section5_degree_upperHalf_start = " + section5_upperHalf_start + 
		    //		"  section5_degree_upperHalf_end = "  + section5_upperHalf_end);
			sections[4][0] = section5_upperHalf_start;
			sections[4][1] = section5_upperHalf_end;

			float section5_lowerHalf_start = 0;
			float section5_lowerHalf_end = pi_ref_polar.Y - 45;
			// System.err.println("section5_degree_lowerHalf_start = " + section5_lowerHalf_start + 
			//		"  section5_degree_lowerHalf_end = " + section5_lowerHalf_end);
			sections[5][0] = section5_lowerHalf_start;
			sections[5][1] = section5_lowerHalf_end;

			// black color section
			float section6_start = zero_ref_polar.Y;
			float section6_end = zero_ref_polar.Y + 45;
			// System.err.println("section6_degree_start = " +  section6_start + "  section6_degree_end = " + section6_end);
			sections[6][0] = section6_start;
			sections[6][1] = section6_end;
		}
	}

	private void tracingDFS_mid(ModelImage greImageSlice, ModelImage fatImageSliceMid, VOI boundary, VOI inner, VOI outer, boolean first) {

		VOIVector src = greImage.getVOIs();
		int zDim = greImage.getExtents()[2];
		VOI sliceVOI = src.VOIAt(0);
		Vector<VOIBase>[] vArray = sliceVOI.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[midPt].get(0);

		Vector3f center = sliceVOI.getGeometricCenter();
		float[][] sections = new float[7][2];
	
		int nPts = v.size();
		float posX, posY;
		int i;

		Vector3f pt[] = new Vector3f[nPts];
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];

		Hashtable<Integer, Line> linePoints = new Hashtable<Integer, Line>();

		v.exportArrays(xPts, yPts, zPts);

		// re-construct the shaft and condyle parts contours
		Vector3f[] ptsLower = new Vector3f[100];
		int lowerIndex = 0;
		
		Vector3f[] ptsLeft = new Vector3f[100];
		int leftIndex = 0;
		Vector3f[] ptsRight = new Vector3f[100];
		int rightIndex = 0;
		
		float sumX = 0, sumY = 0;
		Vector3f condyleCenter;
		
		for (i = 0; i < nPts; i++) {
			
		    if ( yPts[i] >= center.Y ) {
		    	ptsLower[lowerIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	lowerIndex++;
		    } 
		    
		    if ( yPts[i] < center.Y && xPts[i] < center.X ) {
		    	ptsLeft[leftIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	leftIndex++;
		    }
		    
		    if ( yPts[i] < center.Y && xPts[i] > center.X ) {
		    	ptsRight[rightIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	rightIndex++;
		    }
		}
		
		// find new center on the bottom condyle 
		for ( i = 0; i < lowerIndex; i++ ) {
			sumX += ptsLower[i].X;
			sumY += ptsLower[i].Y;
		}
		condyleCenter = new Vector3f( (sumX / (float)lowerIndex), (sumY / (float)lowerIndex), 0);
		
		findSections(ptsLower, lowerIndex, sections, condyleCenter);
		
	
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

		
		// find two lines on the shaft
		float upperLeftX = 512, upperLeftY = 512;
		float lowerLeftX = 512, lowerLeftY = 0;
		float upperRightX = 0, upperRightY = 512;
		float lowerRightX = 0, lowerRightY = 0;
		float x, y;
		
		for ( i = 0; i < leftIndex; i++ ) {
			
			x = ptsLeft[i].X;
			y = ptsLeft[i].Y;
			
			if ( y <= upperLeftY ) {
				upperLeftX = x;
				upperLeftY = y;
				
			} 
			
			if ( y >= lowerLeftY ) {
				lowerLeftX = x;
				lowerLeftY = y;
			}
		}
		
		for ( i = 0; i < rightIndex; i++ ) {
			
			x = ptsRight[i].X;
			y = ptsRight[i].Y;
			
			if ( y <= upperRightY ) {
				upperRightX = x;
				upperRightY = y;
			}
			
			if ( y >= lowerRightY ) {
				lowerRightX = x;
				lowerRightY = y;
			}
		}
		
		VOILine leftLine = new VOILine();
		leftLine.add(new Vector3f(lowerLeftX, lowerLeftY, 0f));
	    leftLine.add(new Vector3f(upperLeftX, upperLeftY, 0f));
		
	    VOILine rightLine = new VOILine();
		rightLine.add(new Vector3f(lowerRightX, lowerRightY, 0f));
	    rightLine.add(new Vector3f(upperRightX, upperRightY, 0f));
		
	    
	    int contourPts = lowerIndex + 4;
		pt =  new Vector3f[contourPts];
		
		for (i = 0; i < lowerIndex; i++) {
			
			posX = ptsLower[i].X;
			posY = ptsLower[i].Y;

			// final line
			pt[i] = new Vector3f(posX, posY, 0f);

			// add the green tracing lines
			VOILine kLine = new VOILine();
			kLine.add(new Vector3f(condyleCenter.X, condyleCenter.Y, 0f));
			kLine.add(new Vector3f(posX, posY, 0f));

			Line line = new Line(condyleCenter.X, condyleCenter.Y, posX, posY, i);
			linePoints.put(i, line);

		} // end i loop;
		
		// sort the linePoints and pt[] in degree increasing order
		pt[i] = new Vector3f(lowerLeftX, lowerLeftY, 0f);
		linePoints.put(i, new Line(condyleCenter.X, condyleCenter.Y, lowerLeftX, lowerLeftY, i));
		
		pt[i+1] = new Vector3f(upperLeftX, upperLeftY, 0f);
		linePoints.put(i+1, new Line(condyleCenter.X, condyleCenter.Y, upperLeftX, upperLeftY, i+1));
		
		pt[i+2] = new Vector3f(upperRightX, upperRightY, 0f);
		linePoints.put(i+2, new Line(condyleCenter.X, condyleCenter.Y, upperRightX, upperRightY, i+2));
		
		pt[i+3] = new Vector3f(lowerRightX, lowerRightY, 0f);
		linePoints.put(i+3, new Line(condyleCenter.X, condyleCenter.Y, lowerRightX, lowerRightY, i+3));
		
		float centerX, centerY;
		int degree;
		Hashtable<Integer, Line> tempLinePoints = new Hashtable<Integer, Line>();
		int index = 0;
		for ( i = 0; i < contourPts; i++ ) {
			
			Line line = linePoints.get(i);
			Vector3f point = pt[i];
			
			centerX = line.startX;
			centerY = line.startY;
			
			posX = line.endX;
			posY = line.endY;
			
			Vector2f cartesianIn = new Vector2f(posX, posY);
			Vector2f polarOut = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(cartesianIn, polarOut, condyleCenter);
			degree = (int)polarOut.Y;
			
			tempLinePoints.put(degree, new Line(condyleCenter.X, condyleCenter.Y, posX, posY, index));
			index++;
		}
		
		linePoints.clear();
		index = 0;
		for ( i = 0; i <= 360; i++ ) {
			Line line = tempLinePoints.get(i);
			if ( line != null ) {
				linePoints.put(index, line);
				pt[index] = new Vector3f(line.endX, line.endY, 0);
				index++;
			}
		}
		
		boundary.importCurve(pt);

	    /*
		// VOI resultVOI = new VOI((short) 0, "result-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(boundary);

		greImageSlice.addVOIs(voiVectorNew);
		smoothVOI150Single(greImageSlice, greImageSlice);
		double constant_length = 2.5d;
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
			Line line = new Line(condyleCenter.X, condyleCenter.Y, posX, posY, z);
			linePoints.put(z, line);
		}
        */
		
		slicesPts.put(midPt, linePoints);
		
		findBoundingContour(midPt, boundary, inner, outer, condyleCenter, fatImageSliceMid, leftLine, rightLine);

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
	
	private void tracingDFS_mid_cutoffLine(ModelImage greImageSlice, ModelImage fatImageSliceMid, VOI boundary, VOI inner, VOI outer, boolean first, 
			VOILine leftLine, VOILine rightLine) {

		VOIVector src = greImage.getVOIs();
		int zDim = greImage.getExtents()[2];
		VOI sliceVOI = src.VOIAt(0);
		Vector<VOIBase>[] vArray = sliceVOI.getSortedCurves(VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[midPt].get(0);
		
		identifyGroups(midPt);
		
		System.err.println("group = " + group);
		
		float[][] sections = new float[7][2];
	
		int nPts = v.size();
		float posX, posY;
		int i;

		Vector3f pt[] = new Vector3f[nPts];
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];

		Hashtable<Integer, Line> linePoints = new Hashtable<Integer, Line>();

		v.exportArrays(xPts, yPts, zPts);

		float sumX = 0, sumY = 0;
		
		// Step 1.   Find the mid box of center VOI contour
		float yMax_x = Float.MIN_VALUE, yMax_y = Float.MIN_VALUE, yMin_x = Float.MAX_VALUE, yMin_y = Float.MAX_VALUE;
		float xMax_x = Float.MIN_VALUE, xMax_y = Float.MIN_VALUE, xMin_x = Float.MAX_VALUE, xMin_y = Float.MAX_VALUE;

		float mid_x, mid_y;
		
		for (i = 0; i < nPts; i++) {

			posX = xPts[i];
			posY = yPts[i];

			if (posY > yMax_y) {  // find max_Y   ( yMax_y )
				yMax_y = posY;
				yMax_x = posX;
			}

			if (posY < yMin_y) {  // find min_Y  ( yMin_y )
				yMin_y = posY;
				yMin_x = posX;
			}

			if (posX > xMax_x) {  // find max_x  ( xMax_x ) 
				xMax_x = posX; 
				xMax_y = posY;
			}

			if (posX < xMin_x) {   // find min_x ( xMin_x )
				xMin_x = posX; 
				xMin_y = posY;
			}

		}

		mid_x = ( xMax_x + xMin_x ) / 2f;
		mid_y = ( yMax_y + yMin_y ) / 2f; 
		
		if ( mid_y >= ( yMax_y - 175 ))
			mid_y  = yMax_y - 175; 
		
		
		// drawing the mid_y square box  
		// float[] newXPts = null, newYPts = null, newZPts = null;
		// newXPts = new float[4];
		// newYPts = new float[4];
		// newZPts = new float[4];
		
		// VOI squareVOI = new VOI((short)1, "square", VOI.CONTOUR, 0);
		// squareVOI.setColor(Color.green);
		// fatImageSliceMid.registerVOI(squareVOI);
		// VOIBase squareGT = new VOIContour(true);
		// newXPts[0] = mid_x - 2;
		// newYPts[0] = mid_y - 2;
		// newXPts[1] = mid_x + 2;
		// newYPts[1] = mid_y - 2;
		// newXPts[2] = mid_x + 2;
		// newYPts[2] = mid_y + 2;
		// newXPts[3] = mid_x - 2;
		// newYPts[3] = mid_y + 2;
		// squareGT.importArrays(newXPts, newYPts, newZPts, 4);
		// squareVOI.importCurve(squareGT);
		
		// new ViewJFrameImage(fatImageSliceMid);
		
		
		// step 2, line fitting to find the cut-off point
		Vector3f[] leftPoints = new Vector3f[100];
		Vector3f[] rightPoints = new Vector3f[100];
		int leftPointsIndex = 0, rightPointsIndex = 0;
		
		for ( i = 0; i < nPts; i++ ) {
			
			if ( yPts[i] < mid_y && xPts[i] < mid_x ) {
				leftPoints[leftPointsIndex] = new Vector3f(xPts[i], yPts[i], 0f);
				leftPointsIndex++;
			}
			
			if ( yPts[i] < mid_y && xPts[i] > mid_x ) {
				rightPoints[rightPointsIndex] = new Vector3f(xPts[i], yPts[i], 0f);
				rightPointsIndex++;
			}
		}
		
		// find two lines on the shaft
		float upperLeftX = 512, upperLeftY = 512;
		float lowerLeftX = 512, lowerLeftY = 0;
		float upperRightX = 0, upperRightY = 512;
		float lowerRightX = 0, lowerRightY = 0;
		float x, y;
	
		double sx = 0.0, sy = 0.0, stt = 0.0, sts = 0.0;
		for (i = 0; i < leftPointsIndex; ++i)
		{
			x = leftPoints[i].X;
			y = leftPoints[i].Y;
			
			if ( y <= upperLeftY ) {
				upperLeftX = x;
				upperLeftY = y;
				
			} 
			
			if ( y >= lowerLeftY ) {
				lowerLeftX = x;
				lowerLeftY = y;
			}
			
			sx += x;
			sy += y;
			
			
		}
		for (i = 0; i < leftPointsIndex; ++i)
		{
			x = leftPoints[i].X;
			y = leftPoints[i].Y;
			double t = x - sx/leftPointsIndex;
			stt += t*t;
			sts += t*y;
		}

		double slope = sts/stt;
		double intercept = (sy - sx*slope)/leftPointsIndex;
		
		// line equation 
		double X1 = lowerLeftX;
		double Y1 = lowerLeftY;
		double X2 = upperLeftX;
		double Y2 = slope * X2 + intercept;
		

	
		
			
		
		// drawing the x1, y1 and x2, y2 line  
		// VOI lineVOI = new VOI((short)1, "line", VOI.LINE, 0);
		// lineVOI.setColor(Color.lightGray);
		// fatImageSliceMid.registerVOI(lineVOI);
		
		// VOILine kLine = new VOILine();
		// kLine.add(new Vector3f(X1, Y1, 0f));
		// kLine.add(new Vector3f(X2, Y2, 0f));
	    // lineVOI.importCurve(kLine);
		
		// new ViewJFrameImage(fatImageSliceMid);
		
		
		// ****************   find the cut off point on the right line points ************Do nothing at the moemnt *********  ???????????????    
		// for ( i = 0; i < rightPointsIndex; i++ ) {
			
		// 	x = rightPoints[i].X;
		// 	y = rightPoints[i].Y;
			
		// 	if ( y <= upperRightY ) {
		// 		upperRightX = x;
		// 		upperRightY = y;
		// 	}
			
		// 	if ( y >= lowerRightY ) {
		// 		lowerRightX = x;
		// 		lowerRightY = y;
		// 	}
		// }
		
		
		// re-construct the shaft and condyle parts contours
		Vector3f center = sliceVOI.getGeometricCenter();
		Vector3f[] ptsLower = new Vector3f[100];
		int lowerIndex = 0;
		Vector3f[] ptsLeft = new Vector3f[100];
		int leftIndex = 0;
		Vector3f[] ptsRight = new Vector3f[100];
		int rightIndex = 0;
		
		float sumXCondyle = 0, sumYCondyle = 0;
		Vector3f condyleCenter;
		
		for (i = 0; i < nPts; i++) {
			
		    if ( yPts[i] > center.Y ) {
		    	ptsLower[lowerIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	lowerIndex++;
		    } 
		    
		    if ( yPts[i] <= center.Y && xPts[i] < center.X ) {
		    	ptsLeft[leftIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	leftIndex++;
		    }
		    
		    if ( yPts[i] <= center.Y && xPts[i] > center.X ) {
		    	ptsRight[rightIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	rightIndex++;
		    }
		}
		
		// find new center on the bottom condyle 
		for ( i = 0; i < lowerIndex; i++ ) {
			sumXCondyle += ptsLower[i].X;
			sumYCondyle += ptsLower[i].Y;
		}
		condyleCenter = new Vector3f( (sumXCondyle / (float)lowerIndex), (sumYCondyle / (float)lowerIndex), 0);
		
		// find the left line points according to condyle center
		leftPoints = new Vector3f[100];
		leftPointsIndex = 0;
		
		for ( i = 0; i < nPts; i++ ) {
			
			if ( yPts[i] < condyleCenter.Y && xPts[i] < condyleCenter.X ) {
				leftPoints[leftPointsIndex] = new Vector3f(xPts[i], yPts[i], 0f);
				leftPointsIndex++;
			}
			
		}
		
		// to sort from degree increasing order 
		float centerX, centerY;
		float degree;
		Hashtable<Float, Line> tempLinePoints = new Hashtable<Float, Line>();
		int index = 0;
		for ( i = 0; i < leftPointsIndex; i++ ) {
			
			Vector3f line = leftPoints[i];
			Vector3f point = pt[i];
			
			centerX = condyleCenter.X;
			centerY = condyleCenter.Y;
			
			posX = line.X;
			posY = line.Y;
			
			Vector2f cartesianIn = new Vector2f(posX, posY);
			Vector2f polarOut = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(cartesianIn, polarOut, condyleCenter);
			degree = polarOut.Y;
			
			tempLinePoints.put(degree, new Line(condyleCenter.X, condyleCenter.Y, posX, posY, index));
			index++;
		}
		

		ArrayList<Float> temp = Collections.list(tempLinePoints.keys());
		Collections.sort(temp);
		Iterator<Float> it = temp.iterator();
		
		while ( it.hasNext() ) {
			float key = it.next();
			Line line = tempLinePoints.get(key);
			if ( line != null ) {
				leftPoints[leftPointsIndex] = new Vector3f(line.endX, line.endY, 0);
				leftPointsIndex++;
			}
		}
		
		double ptLineDist;
		cutOffPoint = new Vector3f();
		for ( i = leftPointsIndex-1; i >= 0; i-- ) {
			x = leftPoints[i].X;
			y = leftPoints[i].Y;
			
			ptLineDist = pointLineDistance(x, y, X1, Y1, X2, Y2);
		
			if ( ptLineDist > 1.4f ) {
				cutOffPoint = new Vector3f(x, y, 0);
				break;
			}
			
		}
		
		// set the cut off point X coordinate to the center X coordinate
		cutOffPoint.X = center.X;
		
		ptsLower = new Vector3f[100];
		lowerIndex = 0;
		ptsLeft = new Vector3f[100];
		leftIndex = 0;
		ptsRight = new Vector3f[100];
		rightIndex = 0;
		
		for (i = 0; i < nPts; i++) {
			
		    if ( yPts[i] > cutOffPoint.Y ) {
		    	ptsLower[lowerIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	lowerIndex++;
		    } 
		    
		    if ( yPts[i] <= cutOffPoint.Y && xPts[i] < cutOffPoint.X ) {
		    	ptsLeft[leftIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	leftIndex++;
		    }
		    
		    if ( yPts[i] <= cutOffPoint.Y && xPts[i] > cutOffPoint.X ) {
		    	ptsRight[rightIndex] = new Vector3f(xPts[i], yPts[i], 0f);
		    	rightIndex++;
		    }
		}
		
		// find new center on the bottom condyle 
		float sumXNew = 0, sumYNew = 0;
		for ( i = 0; i < lowerIndex; i++ ) {
			sumXNew += ptsLower[i].X;
			sumYNew += ptsLower[i].Y;
		}
		condyleCenter = new Vector3f( (sumXNew / (float)lowerIndex), (sumYNew / (float)lowerIndex), 0);
		findSections(ptsLower, lowerIndex, sections, condyleCenter);
		
	
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

		
		// find two lines on the shaft
		upperLeftX = 512; upperLeftY = 512;
		lowerLeftX = 512; lowerLeftY = 0;
		upperRightX = 0; upperRightY = 512;
		lowerRightX = 0; lowerRightY = 0;
		
		for ( i = 0; i < leftIndex; i++ ) {
			
			x = ptsLeft[i].X;
			y = ptsLeft[i].Y;
			
			if ( y <= upperLeftY ) {
				upperLeftX = x;
				upperLeftY = y;
				
			} 
			
			if ( y >= lowerLeftY ) {
				lowerLeftX = x;
				lowerLeftY = y;
			}
		}
		
		for ( i = 0; i < rightIndex; i++ ) {
			
			x = ptsRight[i].X;
			y = ptsRight[i].Y;
			
			if ( y <= upperRightY ) {
				upperRightX = x;
				upperRightY = y;
			}
			
			if ( y >= lowerRightY ) {
				lowerRightX = x;
				lowerRightY = y;
			}
		}

		upperLeftX = 512; upperLeftY = 512;
		lowerLeftX = 512; lowerLeftY = 0;
		sx = 0.0; sy = 0.0; stt = 0.0; sts = 0.0;
		for (i = 0; i < leftIndex; ++i)
		{
			x = ptsLeft[i].X;
			y = ptsLeft[i].Y;
			
			if ( y <= upperLeftY ) {
				upperLeftX = x;
				upperLeftY = y;
				
			} 
			
			if ( y >= lowerLeftY ) {
				lowerLeftX = x;
				lowerLeftY = y;
			}
			
			sx += x;
			sy += y;
		}
		
		for ( i = 0; i < leftIndex; ++i)
		{
			x = ptsLeft[i].X;
			y = ptsLeft[i].Y;
			
			double t = x - sx/leftIndex;
			stt += t*t;
			sts += t*y;
		}

		slope = sts/stt;
		intercept = (sy - sx*slope)/leftIndex;
		upperLeftY = 0;
		upperLeftX = (float)(( upperLeftY - intercept ) / slope);
		
		System.err.println("upperLeftX = " + upperLeftX + "  upperLeftY = " + upperLeftY + " intercept = " + intercept + " slope = " + slope);
		
		
		if ( ( whichLeg == LEFT_LEG && group <= GROUP_6) ||  
				 ( whichLeg == RIGHT_LEG && group <= GROUP_5 ) ) {
			if (Math.abs(slope) <= 1.0E-10f || Float.isInfinite(Math.abs((float)slope))) {

			} else {
				if (((upperLeftY - intercept) / slope) > 1) {
					float upperLeftXTemp = (float)((upperLeftY - intercept) / slope);
					if (upperLeftXTemp < (upperLeftX + 40)) {
						upperLeftX = upperLeftXTemp;
					}
				}
			}

			if (Float.isNaN(upperLeftX) ) {
				upperLeftX = lowerLeftX;
			}

			
		}
			
		
		if ( (whichLeg == LEFT_LEG && group >= GROUP_5 ) || 
			 ( whichLeg == RIGHT_LEG && group >= GROUP_6 )) {
			if ( Math.abs(slope) <= 1.0E-10f || Float.isInfinite(Math.abs((float)slope) )) {
				
			} else {
				if (  ( ( upperLeftY - intercept ) / slope ) >  1 ) {
					float upperLeftXTemp =  (float)(( upperLeftY - intercept ) / slope);
					if ( upperLeftXTemp < (upperLeftX + 40 ) ) { 
						upperLeftX = upperLeftXTemp;
					}
				}
			}
			
			if (Float.isNaN(upperLeftX)  ) {
				upperLeftX = lowerLeftX;
			}
			
		}
		
		
		leftLine.clear();
		leftLine.add(new Vector3f(lowerLeftX, lowerLeftY, 0f));
	    leftLine.add(new Vector3f(upperLeftX, upperLeftY, 0f));
	    
	
	    upperRightX = 0; upperRightY = 512;
	    lowerRightX = 0; lowerRightY = 0;
	   
	    sx = 0.0; sy = 0.0; stt = 0.0; sts = 0.0;
		for (i = 0; i < rightIndex; ++i)
		{
			x = ptsRight[i].X;
			y = ptsRight[i].Y;
			
			if ( y <= upperRightY ) {
				upperRightX = x;
				upperRightY = y;
				
			} 
			
			if ( y >= lowerRightY ) {
				lowerRightX = x;
				lowerRightY = y;
			}
			
			sx += x;
			sy += y;
		}
		
		for ( i = 0; i < rightIndex; ++i)
		{
			x = ptsRight[i].X;
			y = ptsRight[i].Y;
			
			double t = x - sx/(double)rightIndex;
			stt += t*t;
			sts += t*y;
		}

		slope = sts/stt;
		intercept = (sy - sx*slope)/(double)rightIndex;
	    
		upperRightY = 0f;
		upperRightX = (float)(( upperRightY - intercept ) / slope);

		System.err.println("upperRightX = " + upperRightX + "  upperRightY = " + upperRightY + " intercept = " + intercept + " slope = " + slope);		
		
	    if ( ( whichLeg == LEFT_LEG && group <= GROUP_6) ||  
				 ( whichLeg == RIGHT_LEG && group <= GROUP_5 ) ) {
	    	// upperRightX = ( upperRightY - YInt ) / Slope;
	    	
	    	if ( Math.abs(slope) <= 1.0E-10f || Float.isInfinite(Math.abs((float)slope) )) {
	    		// upperRightX= lowerRightX;
	    		// System.err.println("slop == NaN");
			} else {
				
				if ( (( upperRightY - intercept ) / slope) > 1 ) {
					float upperRightXTemp =  (float)(( upperRightY - intercept ) / slope);
					if ( upperRightXTemp > ( upperRightX - 10 )) {
						upperRightX = upperRightXTemp;
					}
				}
				
				
			}
			
			if ( Float.isNaN(upperRightX)  ) {
				upperRightX = lowerRightX;
			}
			
			
	    }
	    
	    if ( (whichLeg == LEFT_LEG && group >= GROUP_5 ) || 
				 ( whichLeg == RIGHT_LEG && group >= GROUP_6 )) {
			if ( Math.abs(slope) <= 1.0E-10f || Float.isInfinite(Math.abs((float)slope) )) {
				
			} else {
				
				if ( (( upperRightY - intercept ) / slope) > 1 ) {
					float upperRightXTemp =  (float)(( upperRightY - intercept ) / slope);
					if ( upperRightXTemp > ( upperRightX - 30 )) {
						upperRightX = upperRightXTemp;
					}
				}
				
				
			}
			
			if ( Float.isNaN(upperRightX)  ) {
				upperRightX = lowerRightX;
			}
		
		 
	    }
	    
		
		
		rightLine.clear();
		rightLine.add(new Vector3f(lowerRightX, lowerRightY, 0f));
		rightLine.add(new Vector3f(upperRightX, upperRightY, 0f));
		
		
	    System.err.println("lowerLeftX = " + lowerLeftX + " lowerLeftY = " + lowerLeftY);
	    System.err.println("upperLeftX = " + upperLeftX + " upperLeftY = " + upperLeftY);

	    System.err.println("lowerRightX = " + lowerRightX + " lowerRightY = " + lowerRightY);
	    System.err.println("upperRightX = " + upperRightX + " upperRightY = " + upperRightY);
	    
		Vector<Vector3f> ptList = new Vector<Vector3f>();
		int linePointsIndex = 0;
		for (i = 0; i < lowerIndex; i++) {
			
			posX = ptsLower[i].X;
			posY = ptsLower[i].Y;

			// final line
			// pt[i] = new Vector3f(posX, posY, 0f);
			ptList.add(new Vector3f(posX, posY, 0f));
			// add the green tracing lines
			// VOILine kLineTemp = new VOILine();
			// kLineTemp.add(new Vector3f(condyleCenter.X, condyleCenter.Y, 0f));
			// kLineTemp.add(new Vector3f(posX, posY, 0f));

			Line line = new Line(condyleCenter.X, condyleCenter.Y, posX, posY, i);
			linePoints.put(linePointsIndex, line);
			linePointsIndex++;

		} // end i loop;
		
		// sort the linePoints and pt[] in degree increasing order
		// pt[i] = new Vector3f(lowerLeftX, lowerLeftY, 0f);
		float walkStepX, walkStepY;
		float currX, currY;
		int k;
		
		// ptList.add(new Vector3f(lowerLeftX, lowerLeftY, 0f));
		// linePoints.put(i, new Line(condyleCenter.X, condyleCenter.Y, lowerLeftX, lowerLeftY, i));
		
		// walk from left side lower point to upper point
		walkStepX = (upperLeftX - lowerLeftX ) / 10.0f;
		walkStepY = (upperLeftY - lowerLeftY ) / 10.0f;
		
		currX = lowerLeftX;
		currY = lowerLeftY;
		
		for ( k = 0; k <= 10; k++ ) {
			ptList.add(new Vector3f(currX, currY, 0f));
			linePoints.put(linePointsIndex, new Line(condyleCenter.X, condyleCenter.Y, currX, currY, linePointsIndex));
			linePointsIndex++;	
			
			currX += walkStepX;
			currY += walkStepY;
		}
		
		
		// walk from right side upper point to right side lower point
		walkStepX = ( lowerRightX - upperRightX ) / 10.0f;
		walkStepY = ( lowerRightY - upperRightY ) / 10.0f;
		
		currX = upperRightX;
		currY = upperRightY;
		
		for ( k = 0; k <= 10; k++ ) {
			ptList.add(new Vector3f(currX, currY, 0f));
			linePoints.put(linePointsIndex, new Line(condyleCenter.X, condyleCenter.Y, currX, currY, linePointsIndex));
			linePointsIndex++;
			
			currX += walkStepX;
			currY += walkStepY;
		}
		
		tempLinePoints = new Hashtable<Float, Line>();
		index = 0;
		int ptsSize = ptList.size();
		for ( i = 0; i < ptsSize; i++ ) {
			
			Line line = linePoints.get(i);
			Vector3f point = ptList.get(i);
			
			centerX = line.startX;
			centerY = line.startY;
			
			posX = line.endX;
			posY = line.endY;
			
			Vector2f cartesianIn = new Vector2f(posX, posY);
			Vector2f polarOut = new Vector2f();
			MipavCoordinateSystems.CartesianToPolar2D(cartesianIn, polarOut, condyleCenter);
			degree = polarOut.Y;
			
			tempLinePoints.put(degree, new Line(condyleCenter.X, condyleCenter.Y, posX, posY, index));
			index++;
		}
		
		linePoints.clear();
		ptList.clear();
		index = 0;
		
	    temp = Collections.list(tempLinePoints.keys());
		Collections.sort(temp);
		it = temp.iterator();
		
		while ( it.hasNext() ) {
			float key = it.next();
			Line line = tempLinePoints.get(key);
			if ( line != null ) {
				linePoints.put(index, line);
				// pt[index] = new Vector3f(line.endX, line.endY, 0);
				ptList.add(new Vector3f(line.endX, line.endY, 0));
				index++;
			}
		}
	
		ptsSize = ptList.size();
		Vector3f[] pts = new Vector3f[ptsSize];
		for (k = 0; k < ptsSize; k++ ) {
			pts[k] = ptList.get(k);
		}
				
		boundary.importCurve(pts);
       
		
	    /*
		// VOI resultVOI = new VOI((short) 0, "result-VOI", VOI.CONTOUR, -1);
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(boundary);

		greImageSlice.addVOIs(voiVectorNew);
		smoothVOI150Single(greImageSlice, greImageSlice);
		double constant_length = 2.5d;
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
			Line line = new Line(condyleCenter.X, condyleCenter.Y, posX, posY, z);
			linePoints.put(z, line);
		}
        */
		
		
		slicesPts.put(midPt, linePoints);
		
		findBoundingContour(midPt, boundary, inner, outer, condyleCenter, fatImageSliceMid, leftLine, rightLine);

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

	private double pointLineDistance(double x, double y, double x1, double y1, double x2, double y2) {

		  double A = x - x1;
		  double B = y - y1;
		  double C = x2 - x1;
		  double D = y2 - y1;

		  double dot = A * C + B * D;
		  double len_sq = C * C + D * D;
		  double param = -1;
		  if (len_sq != 0) //in case of 0 length line
		      param = dot / len_sq;

		  double xx, yy;

		  if (param < 0) {
		    xx = x1;
		    yy = y1;
		  }
		  else if (param > 1) {
		    xx = x2;
		    yy = y2;
		  }
		  else {
		    xx = x1 + param * C;
		    yy = y1 + param * D;
		  }

		  double dx = x - xx;
		  double dy = y - yy;
		  
		  return (double)Math.sqrt(dx * dx + dy * dy);
		  
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

	private boolean findWeakConnectedEdgeOnImageHorizontal(int x, int y, ModelImage imageSlice, int range, int type, boolean searchOnRight, int[] xResult) {

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
		int[] xValueHorizontal = new int[1];
		xValueHorizontal[0] = -1;
		int[] xValueVertical = new int[1];
		xValueVertical[0] = -1;

		int findWeakEdgeHorizontal = weakEdgePatternHorizontal.findWeakEdge(map, startX, startY, xDim, yDim, range, searchOnRight, xValueHorizontal);
		if (findWeakEdgeHorizontal == 1) {
			// xResult[0] = Math.max(xValueHorizontal[0], xValueVertical[0]);
			// weakEdgePattern.printMap(map, xDim, yDim);
			return true;
		} else {
			return false;
		}
	}

	private boolean findWeakConnectedEdgeOnImageVertical(int x, int y, ModelImage imageSlice, int range, int type, boolean searchOnRight, int[] xResult) {

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
		int[] xValueHorizontal = new int[1];
		xValueHorizontal[0] = -1;
		int[] xValueVertical = new int[1];
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

		if (type == FuzzyC_class1) {
			if (imgIntensity == 1 ) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // fuzzyC class 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		if (type == FuzzyC) {
			if (imgIntensity == 2 || imgIntensity == 3) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // fuzzyC class 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class1) {
			if (imgIntensity <= 0.15f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy == 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class2) {
			if (imgIntensity >= 0.9f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy == 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class3) {
			if (imgIntensity >= 0.04f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy < 0.02
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class3_LowInten) {
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
			} else { // intentisy == 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}

		if (type == Class1_weak) {
			if (imgIntensity >= 0.1) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy < 0.1
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
			if (imgIntensity <= 50) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy == 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		if (type == GRE_GREY) {
			if (imgIntensity >= 50) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { // intentisy == 1
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		if (type == GRE_HIGH_INTEN) {
			if (imgIntensity >= 100) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
			}
		}
		
		if (type == CLASS3_HIGH_INTEN) {
			if (imgIntensity >= 0.07f) {
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 1;
			} else { 
				if ((y - Ymin) >= 0 && (y - Ymin) <= yDim && (x - Xmin) >= 0 && (x - Xmin) <= xDim)
					map[y - Ymin][x - Xmin] = 0;
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
