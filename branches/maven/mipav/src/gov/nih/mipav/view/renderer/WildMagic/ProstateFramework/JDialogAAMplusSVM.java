package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR2D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;

import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.view.renderer.WildMagic.AAM.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.libsvm.*;

/**
 * This class is the combined Atlas based AAM and SVM model to automatically
 * segment the MRI prostate. Atlas based AAM model initializes the coarser level
 * VOI contour, then atlas based SVM model refines the prostate boundary. With
 * the new image ( target image ), the AAM model try different pose
 * configurations ( scale, translation, rotation, etc ) to find the closest VOI
 * contour. This contour is the initialization contour to the SVM model. The SVM
 * model generate the non-prostate regions and prostate boundary binary mask
 * images. By copying the initial contour to non-prostate and prostate boundary
 * images, and Coherence Enhanced Diffusion (CED) image, a dynamic narrow band
 * tracing algorithm stretches the initial contour toward the correct prostate
 * boundary based on a voting mechanism from the three images.
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogAAMplusSVM extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445452224259L;

	private Features[] featureArray;

	private FeaturesSVM[] featureArraySVM;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** AAM model reference. */
	private C_AAMMODEL model = new C_AAMMODEL();

	// GUI interface
	/** key image directory. */
	private JLabel labelModel;
	private JTextField textFieldModel;
	private JButton buttonModel;

	/** Target image directory. */
	private JLabel labelImageTarget;
	private JTextField textFieldImageTarget;
	private JButton buttonImageTarget;

	/** image panel. */
	private JPanel imageSelectionPanel;
	private JPanel buttonPanel;

	/** key images variables. */
	private JFileChooser modelChooser = new JFileChooser();
	private String modelDirectory;

	/** target image variables. */
	private JFileChooser targetImageChooser = new JFileChooser();
	private String targetImageName;
	private String targetImageDirectory;

	/** target image references. */
	private ModelImage targetImage;
	private ModelImage cropTargetImage;

	/** cropped image boundary info. */
	private int boxYmin, boxYmax;
	private int boxXmin, boxXmax;
	private int[] xBounds = new int[2];
	private int[] yBounds = new int[2];
	private int[] zBounds = new int[2];

	/** axis panel. */
	private JComboBox axisList;
	private JLabel labelAxis;

	/** axis oritentation. */
	private static int Axial = 0;
	private static int Saggital = 1;
	private static int Coronal = 2;
	private int axis = Axial;

	private ModelImage sampleImage;

	private Hashtable<Integer, Vector<ModelString>> groupTable = new Hashtable<Integer, Vector<ModelString>>();
	private Hashtable<Integer, Vector<ModelImage>> groupRefImages = new Hashtable<Integer, Vector<ModelImage>>();
	private Hashtable<Integer, Vector<ModelImage>> groupCropImages = new Hashtable<Integer, Vector<ModelImage>>();

	/** SVM model panel. */
	private JFileChooser svmModelChooser = new JFileChooser();
	private String svmModelName;
	private String svmModelDirectory;

	private JLabel labelSVMModel;
	private JTextField textFieldSVMModel;
	private JButton buttonSVMModel;

	/** SVM model vector. */
	private Hashtable<String, Hashtable<String, ImageAttribute>> modelStructure = new Hashtable<String, Hashtable<String, ImageAttribute>>();

	/** SVM model vector after sorted by name. */
	private Hashtable<Integer, Hashtable<String, ImageAttribute>> sortModelStructure = new Hashtable<Integer, Hashtable<String, ImageAttribute>>();

	/** SVM model. */
	private Hashtable<Integer, Hashtable<Integer, ImageModel>> models = new Hashtable<Integer, Hashtable<Integer, ImageModel>>();

	/** SVM model vector. */
	private Hashtable<Integer, Hashtable<Integer, ImageModelSVM>> modelsSVM = new Hashtable<Integer, Hashtable<Integer, ImageModelSVM>>();

	private ModelImage finalImage;

	private VOIVector voiVectorNewFinal;

	private VOI voiNewFinal;

	/** VOI vector to fix the wrong segmentation contour after AAM segmentation. */
	private Hashtable<Integer, Integer> markedVOI = new Hashtable<Integer, Integer>();

	/** shape descriptor vector of VOI contours after AAM segmentation. */
	private Vector<ShapeFactor> shapeDescriptor = new Vector<ShapeFactor>();

	/** statistic data corresponding to marked VOI vector. */
	private Vector<Vector<Float>> statData = new Vector<Vector<Float>>();

	/** MIPAV VOI properties parameters. */
	private static final String[] statsToCalculate = new String[] {
			VOIStatisticalProperties.areaDescription,
			VOIStatisticalProperties.perimeterDescription,
			VOIStatisticalProperties.avgIntensity,
			VOIStatisticalProperties.eccentricityDescription,
			VOIStatisticalProperties.majorAxisDescription,
			VOIStatisticalProperties.minorAxisDescription };

	private JPanel mainPanel;
	private static final String featureDirName = "featureCE"; 
	
	/**
	 * Current segmentation processing slice range, which is between silce 3 and
	 * 20. This constraint is imposed by the SVM training model. Due to the
	 * manual drawing the non-prostate regions for SVM training, the human
	 * guided training process is time consuming. Currently, we only train the
	 * SVM models from slice 3 to slice 20. Later, if we have time, we will
	 * apply to more slices.
	 */
	private int startSlice = 3;
	private int endSlice = 20;

	/**
	 * VOI properties check list.
	 */
	private static final boolean[] checkList = new boolean[VOIStatisticalProperties.numberOfStatistics];

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

	/**
	 * Ending slice check references for apex and base.
	 */
	private Vector<String> endSliceImageNames = new Vector<String>();
	private Vector<String> endSliceRangeNames = new Vector<String>();

	private Vector<ModelImage> endSliceImages = new Vector<ModelImage>();
	private Vector<Range> endSliceRange = new Vector<Range>();

	/**
	 * Constructor
	 * 
	 * @param theParentFrame
	 *            parent frame reference.
	 */
	public JDialogAAMplusSVM(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
	}

	public void algorithmPerformed(AlgorithmBase algorithm) {

	}

	/**
	 * actionPerformed handler.
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();

		if (command.equals("OK")) {
			setVisible(false);
			doSegmentation();
		} else if (command.equals("quickSegOK")) {
			setVisible(false);
			doSegmentation();
		} else if (command.equals("SetAxis")) {
			axis = axisList.getSelectedIndex();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("ChooseModel")) {
			readKeyImageDir();
			readEndingSlicesDir();
			readSampleImages();
			readEndingSlice();
			cropImage();
		} else if (command.equals("ChooseModelDir")) {
			readSVMModelDir();
			readModels();
		} else if (command.equals("ChooseTargetImage")) {
			readTargetImage();
			cropTargetImage();
			readTargetImageFeature();
		}

	}

	/**
	 * GUI panel initialization.
	 */
	public void init() {

		setTitle("Prostate AAM Classification");

		mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		imageSelectionPanel = new JPanel();
		imageSelectionPanel.setLayout(new GridLayout(4, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Auto Segmentation"));

		gbc.gridx = 0;
		gbc.gridy = 0;

		// axis label
		String[] axisStrings = { "Axial", "Saggital", "Coronal" };

		axisList = new JComboBox(axisStrings);
		axisList.setSelectedIndex(0);
		axisList.setActionCommand("SetAxis");
		axisList.addActionListener(this);

		labelAxis = new JLabel("Axis: ");
		labelAxis.setFont(serif12);
		labelAxis.setForeground(Color.black);

		imageSelectionPanel.add(labelAxis, gbc);

		gbc.gridx = 1;
		imageSelectionPanel.add(axisList, gbc);

		gbc.gridx = 2;
		JLabel emptyLabel = new JLabel("");
		imageSelectionPanel.add(emptyLabel, gbc);

		// AAM model directory
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelModel = new JLabel("AAM Model Directory: ");
		labelModel.setFont(serif12);
		labelModel.setForeground(Color.black);

		imageSelectionPanel.add(labelModel, gbc);

		textFieldModel = new JTextField(20);
		textFieldModel.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldModel, gbc);

		buttonModel = new JButton("Choose");
		buttonModel.addActionListener(this);
		buttonModel.setActionCommand("ChooseModel");
		buttonModel.setFont(serif12B);
		buttonModel.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonModel, gbc);

		// Model directory
		gbc.gridx = 0;
		gbc.gridy = 2;
		labelSVMModel = new JLabel("SVM Model Directory: ");
		labelSVMModel.setFont(serif12);
		labelSVMModel.setForeground(Color.black);

		imageSelectionPanel.add(labelSVMModel, gbc);

		textFieldSVMModel = new JTextField(20);
		textFieldSVMModel.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldSVMModel, gbc);

		buttonSVMModel = new JButton("Choose");
		buttonSVMModel.addActionListener(this);
		buttonSVMModel.setActionCommand("ChooseModelDir");
		buttonSVMModel.setFont(serif12B);
		buttonSVMModel.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonSVMModel, gbc);

		// target image directory
		gbc.gridx = 0;
		gbc.gridy = 3;
		labelImageTarget = new JLabel("Target Image: ");
		labelImageTarget.setFont(serif12);
		labelImageTarget.setForeground(Color.black);

		imageSelectionPanel.add(labelImageTarget, gbc);

		textFieldImageTarget = new JTextField(20);
		textFieldImageTarget.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldImageTarget, gbc);

		buttonImageTarget = new JButton("Choose");
		buttonImageTarget.addActionListener(this);
		buttonImageTarget.setActionCommand("ChooseTargetImage");
		buttonImageTarget.setFont(serif12B);
		buttonImageTarget.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonImageTarget, gbc);

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
	 * First time, the dialog is invoked, the init() method is called to create
	 * the GUI. Afterward, when the dialog is invoked again, this method is
	 * called, which save the Atlas AAM model and SVM model loading process.
	 */
	public void createTargetDialog() {

		getContentPane().remove(mainPanel);

		setTitle("Prostate AAM Classification");

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

		// axis label
		String[] axisStrings = { "Axial", "Saggital", "Coronal" };

		axisList = new JComboBox(axisStrings);
		axisList.setSelectedIndex(0);
		axisList.setActionCommand("SetAxis");
		axisList.addActionListener(this);

		labelAxis = new JLabel("Axis: ");
		labelAxis.setFont(serif12);
		labelAxis.setForeground(Color.black);

		imageSelectionPanel.add(labelAxis, gbc);

		gbc.gridx = 1;
		imageSelectionPanel.add(axisList, gbc);

		gbc.gridx = 2;
		JLabel emptyLabel = new JLabel("");
		imageSelectionPanel.add(emptyLabel, gbc);

		// target image directory
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelImageTarget = new JLabel("Target Image: ");
		labelImageTarget.setFont(serif12);
		labelImageTarget.setForeground(Color.black);

		imageSelectionPanel.add(labelImageTarget, gbc);

		textFieldImageTarget = new JTextField(20);
		textFieldImageTarget.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldImageTarget, gbc);

		buttonImageTarget = new JButton("Choose");
		buttonImageTarget.addActionListener(this);
		buttonImageTarget.setActionCommand("ChooseTargetImage");
		buttonImageTarget.setFont(serif12B);
		buttonImageTarget.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonImageTarget, gbc);

		// button Panel
		buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridLayout(1, 3));
		gbc.gridx = 0;
		gbc.gridy = 0;

		JButton ok = buildOKButton();
		ok.setActionCommand("quickSegOK");
		buttonPanel.add(ok, gbc);
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
	 * Driver to do automatic prostate segmentation.
	 */
	public void doSegmentation() {
		long startTime = System.currentTimeMillis();

		checkEndSlices();
		// AAM based intialization
		AAMinitialization();
		// new ViewJFrameImage(targetImage);

		evaluateShapeDescriptor();
		markErrorOne();
		fixErrorVOI();

		// new ViewJFrameImage(targetImage);

		cropTargetImage();
		// machine learning based refinement
		// targetImage.getVOIs().removeAllElements();
		finalImage = (ModelImage) (targetImage.clone());
		finalImage.getVOIs().removeAllElements();

		new ViewJFrameImage(finalImage);

		voiVectorNewFinal = new VOIVector();
		// finalImage.addVOIs(voiVectorNewFinal);
		voiNewFinal = new VOI((short) 0, "ImageVOI");
		voiVectorNewFinal.add(voiNewFinal);

		mergedClassifier();
		// transformVOI();
		finalImage.addVOIs(voiVectorNewFinal);

		dispose();

		long endTime = System.currentTimeMillis();
		long diffTime = endTime - startTime;
		float seconds = ((float) diffTime) / 1000;
		float mins = (int) seconds / 60;
		float secs = (int) seconds % 60;
		System.err.println("** Algorithm took  " + mins + "  mins " + secs
				+ " secs\n");
	}

	/**
	 * From the target 2D slice, compare it with end slices based atlas. Find
	 * the closed 2D slice image, and invoke the corresponding start and ending
	 * VOI index. This step is important, which eliminates the segmentation
	 * error toward apex and base to certain extents. There are still changes
	 * that apex and base ending index miss interpolated, resulting in the wrong
	 * segmentation. We need to improve this later.
	 */
	public void checkEndSlices() {

		int slice12 = 12;
		double cost_CRS, cost_MIS, cost_NMIS, cost_NXS;
		double mincost_CRS = Double.MAX_VALUE;
		double mincost_MIS = Double.MAX_VALUE;
		double mincost_NMIS = Double.MAX_VALUE;
		double mincost_NXS = Double.MAX_VALUE;

		// extract the target slice number
		int[] extents = new int[3];
		extents = cropTargetImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];

		ModelSimpleImage simpleImg1 = new ModelSimpleImage(newExtents);
		float[] buffer = new float[size];

		try {
			cropTargetImage.exportData(slice12 * size, size, buffer);
			simpleImg1.importData(buffer, 0, size);
		} catch (IOException e) {
			e.printStackTrace();
		}

		int bestIndex = 0;
		int len = endSliceImages.size();
		for (int i = 0; i < len; i++) {
			ModelImage img = endSliceImages.get(i);
			ModelSimpleImage simpleImg2 = new ModelSimpleImage(img);
			TransMatrix tMatrix = new TransMatrix(3);
			int costChoice;
			int bin = 256;
			float smoothSize = 1.0f;
			int count = 0;

			costChoice = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
			AlgorithmCostFunctions2D algoCost_CRS = new AlgorithmCostFunctions2D(
					simpleImg1, simpleImg2, costChoice, bin, smoothSize);
			cost_CRS = algoCost_CRS.cost(tMatrix);
			// System.err.println("cost_CRS = " + cost_CRS);
			algoCost_CRS.disposeLocal();
			algoCost_CRS = null;

			tMatrix = new TransMatrix(3);
			costChoice = AlgorithmCostFunctions2D.MUTUAL_INFORMATION_SMOOTHED;
			AlgorithmCostFunctions2D algoCost_MIS = new AlgorithmCostFunctions2D(
					simpleImg1, simpleImg2, costChoice, bin, smoothSize);
			cost_MIS = algoCost_MIS.cost(tMatrix);
			// System.err.println("cost_MIS = " + cost_MIS);
			algoCost_MIS.disposeLocal();
			algoCost_MIS = null;

			tMatrix = new TransMatrix(3);
			costChoice = AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
			AlgorithmCostFunctions2D algoCost_NMIS = new AlgorithmCostFunctions2D(
					simpleImg1, simpleImg2, costChoice, bin, smoothSize);
			cost_NMIS = algoCost_NMIS.cost(tMatrix);
			// System.err.println("cost_NMIS = " + cost_NMIS);
			algoCost_NMIS.disposeLocal();
			algoCost_NMIS = null;

			tMatrix = new TransMatrix(3);
			costChoice = AlgorithmCostFunctions2D.NORMALIZED_XCORRELATION_SMOOTHED;
			AlgorithmCostFunctions2D algoCost_NXS = new AlgorithmCostFunctions2D(
					simpleImg1, simpleImg2, costChoice, bin, smoothSize);
			cost_NXS = algoCost_NXS.cost(tMatrix);
			// System.err.println("cost_NXS = " + cost_NXS);
			algoCost_NXS.disposeLocal();
			algoCost_NXS = null;

			if (cost_CRS <= mincost_CRS)
				count++;
			if (cost_MIS <= mincost_MIS)
				count++;
			if (cost_NMIS <= mincost_NMIS)
				count++;
			if (cost_NXS <= mincost_NXS)
				count++;

			if (count >= 3) {
				mincost_CRS = cost_CRS;
				mincost_MIS = cost_MIS;
				mincost_NMIS = cost_NMIS;
				mincost_NXS = cost_NXS;
				bestIndex = i;
				Range r = endSliceRange.get(bestIndex);
				startSlice = r.startSlice;
				endSlice = r.endSlice;
			}

		} // end for img :
			// System.err.println("startSlice = " + startSlice +
			// "   endSlice = " + endSlice);

	}

	/**
	 * Evaluate the AAM segmented VOIs with MIPAV VOI properties.
	 */
	public void evaluateShapeDescriptor() {
		AlgorithmVOIProps calculator = new AlgorithmVOIProps(targetImage,
				AlgorithmVOIProps.PROCESS_PER_CONTOUR, RangeType.NO_RANGE,
				targetImage.getVOIs());
		calculator.setSelectedStatistics(checkList);
		calculator.setShowTotals(false);
		calculator.run();

		Vector<VOIStatisticalProperties> statsList = new Vector<VOIStatisticalProperties>(
				targetImage.getVOIs().size());
		for (VOI voi : targetImage.getVOIs()) {
			statsList.add(calculator.getVOIProperties(voi));
		}
		getStatsData(statsList, targetImage.getVOIs(), targetImage);
		computeShapeDescriptor();

		calculator.finalize();
		calculator = null;

	}

	/**
	 * Generate the VOI statistics data.
	 * 
	 * @param statsList
	 * @param VOIs
	 * @param img
	 * @return
	 */
	private Vector<Vector<String>> getStatsData(
			Vector<VOIStatisticalProperties> statsList, VOIVector VOIs,
			ModelImage img) {
		statData.clear();

		Vector<Vector<String>> data = new Vector<Vector<String>>();

		int voiIndex = 0;
		for (VOIStatisticalProperties prop : statsList) {
			VOIBaseVector contours = VOIs.get(voiIndex).getCurves();
			for (VOIBase voi : contours) {
				Vector<String> row = new Vector<String>();
				Vector<Float> rowData = new Vector<Float>();
				String contourLabel = voi.getLabel();
				row.add(img.getImageFileName().replaceAll("[\\t+]", ", ")
						.replaceAll("[\\n\\r+]", ":"));
				row.add(VOIs.get(voiIndex).getName().replaceAll("[\\t+]", ", ")
						.replaceAll("[\\n\\r+]", ":"));

				for (int i = 0; i < checkList.length; i++) {
					if (checkList[i]) {
						String valueString = prop
								.getProperty(
										VOIStatisticList.statisticDescription[i]
												+ contourLabel)
								.replaceAll("[\\t+]", ", ")
								.replaceAll("[\\n\\r+]", ":");
						row.add(valueString);
						rowData.add(Float.valueOf(valueString));
					}
				}
				data.add(row);
				statData.add(rowData);
				System.err.println(row.toString());
			}
			voiIndex++;

		}
		return data;
	}

	/**
	 * Compute VOI shape descriptors.
	 */
	public void computeShapeDescriptor() {

		shapeDescriptor.clear();
		Vector<Float> data;
		float area;
		float perimeter;
		float avgIntensity;
		float eccentricity;
		float majorAxis;
		float minorAxis;

		int len = statData.size();
		System.err.print("formFactor");
		System.err.print("\t\t" + "roundness");
		System.err.print("\t\t" + "compactness");
		System.err.print("\t\t" + "aspectRatio");
		System.err.println("\t\t" + "eccentricity");

		for (int i = 0; i < len; i++) {
			data = statData.get(i);
			area = data.get(0);
			perimeter = data.get(1);
			avgIntensity = data.get(2);
			eccentricity = data.get(3);
			majorAxis = data.get(4);
			minorAxis = data.get(5);

			float formFactor = (float) ((4f * Math.PI * area) / (float) Math
					.sqrt(perimeter));
			float roundness = (float) ((4f * area) / (Math.PI * Math
					.sqrt(majorAxis)));
			float compactness = (float) (Math.sqrt((4 / Math.PI) * area) / majorAxis);
			float aspectRatio = majorAxis / minorAxis;

			System.err.print(formFactor);
			System.err.print("\t\t" + roundness);
			System.err.print("\t\t" + compactness);
			System.err.print("\t\t" + aspectRatio);
			System.err.println("\t\t" + eccentricity);

			shapeDescriptor.add(new ShapeFactor(formFactor, roundness,
					compactness, aspectRatio, eccentricity, area, perimeter,
					avgIntensity));
		}

	}

	/**
	 * Mark the error segmented VOIs.
	 */
	public void markErrorOne() {

		Vector<Float> data;
		ShapeFactor shape;
		markedVOI.clear();

		float area, area_prev, area_next;
		float perimeter, perimeter_prev, perimeter_next;
		float roundness, roundness_prev, roundness_next;
		float aspectRatio, aspectRatio_prev, aspectRatio_next;
		float compactness, compactness_prev, compactness_next;

		float avg_roundness, avg_perimeter;
		float avg_aspectRatio, avg_compactness;

		float round_diff, perimeter_diff;
		float aspectRatio_diff, compactness_diff;

		int prev_index, next_index;

		float taper_rate_area = 0, taper_rate_perimeter;

		int len = statData.size();

		for (int i = 0; i < len; i++) {
			markedVOI.put(i, 0);
		}

		for (int i = 0; i < len; i++) {
			if (i == 0) {
				prev_index = i + 1;
				next_index = i + 2;
			} else if (i == len - 1) {
				prev_index = len - 2;
				next_index = len - 3;
			} else {
				prev_index = i - 1;
				next_index = i + 1;
			}

			data = statData.get(i);
			perimeter = data.get(1);
			data = statData.get(prev_index);
			perimeter_prev = data.get(1);
			data = statData.get(next_index);
			perimeter_next = data.get(1);

			shape = shapeDescriptor.get(i);
			roundness = shape.roundness;
			aspectRatio = shape.aspectRatio;
			compactness = shape.compactness;
			shape = shapeDescriptor.get(prev_index);
			roundness_prev = shape.roundness;
			aspectRatio_prev = shape.aspectRatio;
			compactness_prev = shape.compactness;
			shape = shapeDescriptor.get(next_index);
			roundness_next = shape.roundness;
			aspectRatio_next = shape.aspectRatio;
			compactness_next = shape.compactness;

			avg_roundness = (roundness_prev + roundness_next) / 2f;
			avg_perimeter = (perimeter_prev + perimeter_next) / 2f;
			avg_aspectRatio = (aspectRatio_prev + aspectRatio_next) / 2f;
			avg_compactness = (compactness_prev + compactness_next) / 2f;

			round_diff = Math.abs(roundness - avg_roundness) / roundness;
			perimeter_diff = Math.abs(perimeter - avg_perimeter) / perimeter;
			aspectRatio_diff = Math.abs(aspectRatio - avg_aspectRatio)
					/ aspectRatio;
			compactness_diff = Math.abs(compactness - avg_compactness)
					/ compactness;

			if (round_diff >= 0.5f || perimeter_diff >= 0.5f
					|| aspectRatio_diff >= 0.25f || compactness_diff >= 0.15f) {
				markedVOI.put(i, 1);
				statData.get(i).set(1, statData.get(next_index).get(1));
				shapeDescriptor.get(i).roundness = shapeDescriptor
						.get(next_index).roundness;
			}
		}

		int end = endSlice - startSlice;

		// measure area, perimeter
		int mid = (int) (end / 2);
		for (int i = mid; i < end; i++) {
			prev_index = i - 1;
			next_index = i + 1;
			data = statData.get(i);
			perimeter = data.get(1);
			area = data.get(0);
			data = statData.get(next_index);
			perimeter_next = data.get(1);
			area_next = data.get(0);
			data = statData.get(prev_index);
			perimeter_prev = data.get(1);
			area_prev = data.get(0);

			taper_rate_area = Math.abs(area_prev - area) / area;
			taper_rate_perimeter = Math.abs(perimeter_prev - perimeter)
					/ perimeter;

			if (taper_rate_area >= 0.30f || taper_rate_perimeter >= 0.30f) {

				taper_rate_area = Math.abs(area_next - area) / area;
				taper_rate_perimeter = Math.abs(perimeter_next - perimeter)
						/ perimeter;

				if (taper_rate_area >= 0.30f || taper_rate_perimeter >= 0.30f) {

					if (markedVOI.get(i) != 2) {
						markedVOI.put(i, 1);
						// markedVOI.put(next_index, 1);
						// statData.get(next_index).set(1,
						// statData.get(i).get(1));
						// statData.get(next_index).set(0,
						// statData.get(i).get(0));
					}
				}
			}
		}

		for (int i = mid; i > 0; i--) {
			prev_index = i + 1;
			next_index = i - 1;
			data = statData.get(i);
			perimeter = data.get(1);
			area = data.get(0);
			data = statData.get(next_index);
			perimeter_next = data.get(1);
			area_next = data.get(0);
			data = statData.get(prev_index);
			perimeter_prev = data.get(1);
			area_prev = data.get(0);

			taper_rate_area = Math.abs(area_prev - area) / area;
			taper_rate_perimeter = Math.abs(perimeter_prev - perimeter)
					/ perimeter;

			if (taper_rate_area >= 0.30f || taper_rate_perimeter >= 0.30f) {
				taper_rate_area = Math.abs(area_next - area) / area;
				taper_rate_perimeter = Math.abs(perimeter_next - perimeter)
						/ perimeter;

				if (taper_rate_area >= 0.30f || taper_rate_perimeter >= 0.30f) {
					if (markedVOI.get(i) != 2) {
						markedVOI.put(i, 1);
						// markedVOI.put(next_index, 1);
						// statData.get(next_index).set(1,
						// statData.get(i).get(1));
						// statData.get(next_index).set(0,
						// statData.get(i).get(0));
					}
				}
			}
		}

		for (int i = 0; i < len; i++) {
			int value = markedVOI.get(i);
			System.err.println(i + "\t" + value);
		}

	}

	/**
	 * BSpline to smooth the final segmented VOIs.
	 */
	public void transformVOIAAM() {
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

			VOIBaseVector current_va = targetImage.getVOIs().VOIAt(i)
					.getCurves();
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
					BsplinePoints[w] = new Vector3f(xPtsCurrent[w],
							yPtsCurrent[w], 0);
				}

				BSplineCurve3f curve = BSplineCurve3f.CreateApproximation(
						BsplinePoints, nPtsCurrent - 5, 2);
				float minTime = curve.GetMinTime();
				float maxTime = curve.GetMaxTime();
				// float step = (maxTime - minTime) / 30f;
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

		targetImage.getVOIs().removeAllElements();
		targetImage.addVOIs(voiVectorNew);
	}

	/**
	 * Fix the error VOIs from the Marked VOI vector.
	 */
	public void fixErrorVOI() {

		float[] xPtsNext;
		float[] yPtsNext;
		float[] zPtsNext;

		float[] xPtsCurrent;
		float[] yPtsCurrent;
		float[] zPtsCurrent;

		// Range 20 - 3, 0 -- 17
		int end = endSlice - startSlice;
		int mid = (int) end / 2;
		for (int i = mid; i > 0; i--) {
			int curr_index = i;
			int next_index = i + 1;

			if (markedVOI.get(curr_index) == 1) {

				while (markedVOI.get(next_index) == 1) {
					next_index++;
				}

				// **************** current image
				// *************************************
				VOIBaseVector current_va = targetImage.getVOIs()
						.VOIAt(curr_index).getCurves();
				VOIBase current_v = current_va.get(0);
				// **************** next imaage*********************************
				VOIBaseVector next_va = targetImage.getVOIs().VOIAt(next_index)
						.getCurves();
				VOIBase next_v = next_va.get(0);

				int nPtsCurrent = current_v.size();
				xPtsCurrent = new float[nPtsCurrent];
				yPtsCurrent = new float[nPtsCurrent];
				zPtsCurrent = new float[nPtsCurrent];
				current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);

				int nPtsNext = next_v.size();
				xPtsNext = new float[nPtsNext];
				yPtsNext = new float[nPtsNext];
				zPtsNext = new float[nPtsNext];
				next_v.exportArrays(xPtsNext, yPtsNext, zPtsNext);

				float[] zSlice = new float[nPtsNext];
				for (int j = 0; j < nPtsNext; j++) {
					zSlice[j] = zPtsCurrent[0];
				}
				current_v.importArrays(xPtsNext, yPtsNext, zSlice, nPtsNext);
				markedVOI.put(curr_index, 0);
			}
		}

		for (int i = mid; i < end; i++) {
			int curr_index = i;
			int next_index = i - 1;

			if (markedVOI.get(curr_index) == 1) {

				while (markedVOI.get(next_index) == 1) {
					next_index--;
				}

				// **************** current image
				// *************************************
				VOIBaseVector current_va = targetImage.getVOIs()
						.VOIAt(curr_index).getCurves();
				VOIBase current_v = current_va.get(0);
				// **************** next imaage*********************************
				VOIBaseVector next_va = targetImage.getVOIs().VOIAt(next_index)
						.getCurves();
				VOIBase next_v = next_va.get(0);

				int nPtsCurrent = current_v.size();
				xPtsCurrent = new float[nPtsCurrent];
				yPtsCurrent = new float[nPtsCurrent];
				zPtsCurrent = new float[nPtsCurrent];
				current_v.exportArrays(xPtsCurrent, yPtsCurrent, zPtsCurrent);

				int nPtsNext = next_v.size();
				xPtsNext = new float[nPtsNext];
				yPtsNext = new float[nPtsNext];
				zPtsNext = new float[nPtsNext];
				next_v.exportArrays(xPtsNext, yPtsNext, zPtsNext);

				float[] zSlice = new float[nPtsNext];
				for (int j = 0; j < nPtsNext; j++) {
					zSlice[j] = zPtsCurrent[0];
				}
				current_v.importArrays(xPtsNext, yPtsNext, zSlice, nPtsNext);
				markedVOI.put(curr_index, 0);
			}
		}
	}

	/**
	 * Atlas based AAM segmentation.
	 */
	public void AAMinitialization() {

		int[] bestImageNumber = new int[1];
		int[] bestSliceNumber = new int[1];

		int[] extents = new int[3];
		extents = targetImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];
		float[] buffer = new float[size];

		if (startSlice <= 3)
			startSlice = 3;
		if (endSlice >= 20)
			endSlice = 20;

		for (int i = startSlice; i <= endSlice; i++) {

			System.err.println("slice = " + i);

			ModelImage targetImageSlice = new ModelImage(targetImage.getType(),
					newExtents, makeImageName(targetImage.getImageName(),
							"target" + i));
			try {
				targetImage.exportData(i * size, size, buffer);
				targetImageSlice.importData(0, buffer, true);
				// new ViewJFrameImage(targetImageSlice);
			} catch (IOException e) {
				e.printStackTrace();
			}

			findBestImageNMI(i, bestImageNumber, bestSliceNumber);

			Vector<ModelString> modelNameVector = groupTable
					.get(bestImageNumber[0]);
			boolean ok = model.ReadModel(modelNameVector
					.get(bestSliceNumber[0]).modelName);

			try {
				FileIO sampleImageIO = null;
				sampleImageIO = new FileIO();
				sampleImage = sampleImageIO.readImage(modelNameVector
						.get(bestSliceNumber[0]).imageName);
				FileVOI fileVOI = null;
				String voiDir = modelNameVector.get(bestSliceNumber[0]).voiName;
				System.err.println("voiDir = " + voiDir);
				int index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1,
						voiDir.length()));
				fileVOI = new FileVOI(voiFileName, voiDirectory, sampleImage);
				sampleImage.registerVOI(fileVOI.readVOI(false)[0]);
				sampleImageIO = null;
				fileVOI = null;

			} catch (Exception e) {
				e.printStackTrace();
			}

			CAAMConsoleModeE e = new CAAMConsoleModeE();
			e.classification(model, targetImageSlice, sampleImage);

			// copy VOI from targetImageSlice to targetImage
			updateTargetImage(i, targetImageSlice);

			// clean memory
			// modelNameVector.clear();
			// modelNameVector = null;
			targetImageSlice.disposeLocal();
			targetImageSlice = null;

		} // end for i loop

	}

	/**
	 * Crop image for SVM classification.
	 */
	public void cropImage() {

		for (int j = startSlice; j <= endSlice; j++) {

			Vector<ModelImage> refImages = groupRefImages.get(j);
			Vector<ModelImage> cropImageVector = new Vector<ModelImage>();

			ModelImage currentImage;
			int len = refImages.size();

			// manually set the crop image starting point and ending point
			boxYmin = 124;
			boxYmax = 380 - 1;

			boxXmin = 124;
			boxXmax = 380 - 1;

			xBounds[0] = boxXmin;
			xBounds[1] = boxXmax;

			yBounds[0] = boxYmin;
			yBounds[1] = boxYmax;

			int borderSize = 0;

			int[] destExtents = null;

			// to 2D image
			destExtents = new int[2];
			destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
					+ (2 * borderSize);
			destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
					+ (2 * borderSize);

			System.err.println(" destExtents[0] = " + destExtents[0]
					+ " destExtents[1] = " + destExtents[1]);

			int[] xCrop = new int[] { 0, 0 };
			int[] yCrop = new int[] { 0, 0 };
			int[] zCrop = new int[] { 0, 0 };
			if (destExtents.length > 0) {
				xCrop[0] = -1 * (xBounds[0] - borderSize);
				xCrop[1] = -1 * (xBounds[1] - destExtents[0] - 1);
			}
			if (destExtents.length > 1) {
				yCrop[0] = -1 * (yBounds[0] - borderSize);
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

			System.err.println("xCrop[0] = " + xCrop[0] + "   xCrop[1] = "
					+ xCrop[1]);
			System.err.println("yCrop[0] = " + yCrop[0] + "   yCrop[1] = "
					+ yCrop[1]);

			for (int i = 0; i < len; i++) {

				currentImage = refImages.get(i);

				try {

					// create crop images
					ModelImage cropImage = new ModelImage(
							currentImage.getType(), destExtents, makeImageName(
									currentImage.getImageName(), "_crop"));

					AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(
							currentImage, cropImage, xCrop, yCrop, zCrop);

					cropAlgo.addListener(this);

					cropAlgo.run();

					cropImageVector.add(cropImage);

				} catch (OutOfMemoryError e) {
					e.printStackTrace();
					MipavUtil
							.displayError("Dialog Crop: unable to allocate enough memory");

					return;
				}

			}

			groupCropImages.put(j, cropImageVector);
		} // end for j loop

	}

	/**
	 * We pre-processing target image with coherence enhance diffusion (CED)
	 * filter, then extract SVM features from it and save it to the
	 * corresponding target image directory. Before apply the automatic
	 * segmentation, this method load the pre-processing CED SVM features by
	 * default. If we do CED filter and SVM feature extraction in the
	 * segmentation process, it will take much, much longer time to process the
	 * segmentation.
	 */
	public void readTargetImageFeature() {

		int i;
		int zDim;

		if (cropTargetImage.getNDims() == 2) {
			zDim = 1;
		} else {
			zDim = cropTargetImage.getExtents()[2];
		}

		featureArray = new Features[zDim];
		for (i = 0; i < zDim; i++) {
			featureArray[i] = new Features();
		}

		featureArraySVM = new FeaturesSVM[zDim];
		for (i = 0; i < zDim; i++) {
			featureArraySVM[i] = new FeaturesSVM();
		}

		String featureDir = targetImageDirectory + File.separator + featureDirName;
		System.err.println("targetImage dir feature = " + featureDir);
		String name = targetImage.getImageName();
		if (name.contains("ax") || name.contains("Axial")) {
			featureDir += File.separator + "axial";
		} else if (name.contains("cor") || name.contains("Coronal")) {
			featureDir += File.separator + "coronal";
		} else if (name.contains("sag") || name.contains("Sagittal")) {
			featureDir += File.separator + "sagittal";
		}

		File dir = new File(featureDir);
		String[] children = dir.list();
		int nr_feature = 5;
		int total = 0;
		for (i = 0; i < zDim; i++) {
			File childFile = new File(featureDir, children[i]);
			String fileName = childFile.toString();
			int begin = fileName.lastIndexOf(File.separator) + 1;
			int end = fileName.length();
			int fileNameEnd = fileName.lastIndexOf(".");

			if (fileName.substring(begin, end).startsWith("slice")) {

				String result = fileName.substring(begin + 5, fileNameEnd);
				int sliceNumber = Integer.valueOf(result);

				// read in features
				// File featureFile = new File(fileName);
				// Model model = Linear.loadModel(modelFile);
				BufferedReader reader = null;

				try {
					reader = new BufferedReader(new InputStreamReader(
							new FileInputStream(fileName), Linear.FILE_CHARSET));
					// doPredict(reader, writer, model);
					String line = null;
					while ((line = reader.readLine()) != null) {
						List<FeatureNode> x = new ArrayList<FeatureNode>();
						List<svm_node> y = new ArrayList<svm_node>();
						StringTokenizer st = new StringTokenizer(line, " \t");
						String label = st.nextToken();
						int target_label = atoi(label);

						while (st.hasMoreTokens()) {
							String[] split = Predict.COLON.split(
									st.nextToken(), 2);
							if (split == null || split.length < 2)
								exit_input_error(total + 1);

							try {
								int idx = atoi(split[0]);
								double val = atof(split[1]);

								// feature indices larger than those in training
								// are not used
								if (idx <= nr_feature) {
									featureArray[i].classAdd(0);
									FeatureNode node = new FeatureNode(idx, val);
									x.add(node);

									featureArraySVM[i].classAdd(0);
									svm_node nodeSVM = new svm_node(idx, val);
									y.add(nodeSVM);
								}
							} catch (NumberFormatException e) {
								exit_input_error(total + 1, e);
							}
						}

						FeatureNode[] nodes = new FeatureNode[x.size()];
						nodes = x.toArray(nodes);
						featureArray[sliceNumber].featureAdd(nodes);

						svm_node[] nodesSVM = new svm_node[y.size()];
						nodesSVM = y.toArray(nodesSVM);
						featureArraySVM[sliceNumber].featureAdd(nodesSVM);
					} // end while loop

				} catch (IOException e) {
					e.printStackTrace();
				}

			}

		}

	}

	/**
	 * Read SVM model atlas.
	 */
	private void readModels() {

		// slice number
		Integer[] keys = sortModelStructure.keySet().toArray(new Integer[0]);
		Arrays.sort(keys);

		try {
			// key is the slice number
			for (Integer key : keys) {
				// System.err.println("****************" + key +
				// "*********************");
				Hashtable<String, ImageAttribute> imageStructure = sortModelStructure
						.get(key);
				Hashtable<Integer, ImageModel> imageModels = new Hashtable<Integer, ImageModel>();
				Hashtable<Integer, ImageModelSVM> imageModelsSVM = new Hashtable<Integer, ImageModelSVM>();

				Set imageSet = imageStructure.keySet();

				Iterator imageItr = imageSet.iterator();

				while (imageItr.hasNext()) {
					// get the image number
					String imageKey = (String) imageItr.next();
					int imageNumber = Integer.valueOf(imageKey);
					ImageAttribute imageAttr = imageStructure.get(imageKey);
					Hashtable<String, Vector> sliceStructure = imageAttr
							.getSliceStructure();
					ImageModel imageModel;
					ImageModelSVM imageModelSVM;
					ModelImage image;

					// read the models from slice structure
					Hashtable<String, Vector<Model>> sliceModel = new Hashtable<String, Vector<Model>>();
					Hashtable<String, Vector<svm_model>> sliceModelSVM = new Hashtable<String, Vector<svm_model>>();
					Set sliceSet = sliceStructure.keySet();
					Iterator sliceItr = sliceSet.iterator();
					while (sliceItr.hasNext()) {
						String sliceKey = (String) sliceItr.next();
						// System.err.println("---------------------" + sliceKey
						// + " ----------------- ");
						Vector fileNames = sliceStructure.get(sliceKey);
						Vector<Model> modelVector = new Vector<Model>();
						Vector<svm_model> modelVectorSVM = new Vector<svm_model>();
						for (int i = 0; i < fileNames.size(); i++) {
							String fileName = (String) fileNames.get(i);
							// System.err.println("i = " + i + " fileName = " +
							// fileName);
							if (fileName.contains("Boundary")) {
								// model I/O for svmlib
								svm_model model = svm.svm_load_model(fileName);
								modelVectorSVM.add(model);
							} else {
								// model I/O for svmlinear
								File modelFile = new File(fileName);
								Model model = Linear.loadModel(modelFile);
								modelVector.add(model);
							}

						}
						sliceModel.put(sliceKey, modelVector);
						sliceModelSVM.put(sliceKey, modelVectorSVM);
					}

					// read .xml image
					FileIO file = new FileIO();
					String fileName = imageAttr.getImageName();
					int index = fileName.lastIndexOf(File.separator);
					String dirName = fileName.substring(0, index + 1);
					String fName = fileName.substring(index + 1,
							fileName.length());
					image = file.readImage(fName, dirName);

					// wrap the single image model into the imageModels array
					imageModel = new ImageModel(imageNumber, image, sliceModel);
					imageModels.put(imageNumber, imageModel);

					imageModelSVM = new ImageModelSVM(imageNumber, image,
							sliceModelSVM);
					imageModelsSVM.put(imageNumber, imageModelSVM);

				}

				models.put(key, imageModels);
				modelsSVM.put(key, imageModelsSVM);
			} // end for key : keys
		} catch (IOException e) {
			e.printStackTrace();
		}
		// System.err.println("test");
	}

	private static void exit_input_error(int line_num, Throwable cause) {
		throw new RuntimeException("Wrong input format at line " + line_num,
				cause);
	}

	private static void exit_input_error(int line_num) {
		throw new RuntimeException("Wrong input format at line " + line_num);
	}

	public static double atof(String s) {
		if (s == null || s.length() < 1)
			throw new IllegalArgumentException(
					"Can't convert empty string to integer");
		double d = Double.parseDouble(s);
		if (Double.isNaN(d) || Double.isInfinite(d)) {
			throw new IllegalArgumentException("NaN or Infinity in input: " + s);
		}
		return (d);
	}

	/**
	 * @param s
	 *            the string to parse for the integer value
	 * @throws IllegalArgumentException
	 *             if s is empty
	 * @throws NumberFormatException
	 *             see {@link Integer#parseInt(String)}
	 */
	public static int atoi(String s) throws NumberFormatException {
		if (s == null || s.length() < 1)
			throw new IllegalArgumentException(
					"Can't convert empty string to integer");
		// Integer.parseInt doesn't accept '+' prefixed strings
		if (s.charAt(0) == '+')
			s = s.substring(1);
		return Integer.parseInt(s);
	}

	/**
	 * After apply the AAM segmentation. We copy the resulting contour to SVM
	 * generated non-prostate binary mask image, prostate boundary image, and
	 * corresponding CED image. This method uses atlas SVM model to generate the
	 * binary maske image, and call a dynamic narrow band tracking algorithm to
	 * refine the final prostate boundary.
	 */
	public void mergedClassifier() {

		int sliceNumber;
		int size = featureArray[0].getFeatureArray().size();
		int zDim = cropTargetImage.getExtents()[2];
		float[] resols = cropTargetImage.getResolutions(0);
		int[] ext = cropTargetImage.getExtents();
		int xDim = ext[0];
		int yDim = ext[1];
		int[] newExtents = new int[2];
		newExtents[0] = xDim;
		newExtents[1] = yDim;

		float[] newResols = new float[2];
		newResols[0] = resols[0];
		newResols[1] = resols[1];
		// int[][] classes1 = new int[zDim][size];

		Predict predict = new Predict();

		float[] buffer = new float[size];

		// new ViewJFrameImage(cropTargetImage);

		VOIVector VOIs = cropTargetImage.getVOIs();
		int[] exten = cropTargetImage.getExtents();

		// ****************************** attention
		// ********************************************
		for (int i = startSlice; i <= endSlice; i++) {

			sliceNumber = i;

			ModelImage targetImageSlice = new ModelImage(
					cropTargetImage.getType(), newExtents, makeImageName(
							targetImage.getImageName(), "cropTarget" + i));
			try {
				cropTargetImage.exportData(i * size, size, buffer);
				targetImageSlice.importData(0, buffer, true);
				// new ViewJFrameImage(targetImageSlice);
			} catch (IOException e) {
				e.printStackTrace();
			}

			// need to add VOI
			if (VOIs.VOIAt(sliceNumber - startSlice) != null
					&& VOIs.VOIAt(sliceNumber - startSlice).getCurves().size() > 0) {
				Vector<VOIBase>[] vArray = VOIs.VOIAt(sliceNumber - startSlice)
						.getSortedCurves(VOIBase.ZPLANE, exten[2]);
				VOIBase v = vArray[sliceNumber].get(0);
				VOIBase vTemp = (VOIBase) v.clone();

				int nPts = vTemp.size();

				// zero out the z dimension VOI
				float[] xPts = new float[nPts];
				float[] yPts = new float[nPts];
				float[] zPts = new float[nPts];
				float[] zPtsZero = new float[nPts];

				vTemp.exportArrays(xPts, yPts, zPts);
				vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

				VOIVector voiVectorNew = new VOIVector();
				VOI voiNew = new VOI((short) 0, "blank");
				voiNew.importCurve(vTemp);
				voiVectorNew.add(voiNew);

				targetImageSlice.addVOIs(voiVectorNew);

				xPts = null;
				yPts = null;
				zPts = null;
				zPtsZero = null;
				// }

				// new ViewJFrameImage(targetImageSlice);
				// get the svmlinear model
				Hashtable<Integer, ImageModel> imageModels = models
						.get(sliceNumber);
				// get the svmlib model
				Hashtable<Integer, ImageModelSVM> imageModelsSVM = modelsSVM
						.get(sliceNumber);

				Set imageModelsKeySet = imageModels.keySet();
				Iterator imageModelsItr = imageModelsKeySet.iterator();

				int bestImageNumber = 0;

				ModelSimpleImage simpleImg1 = new ModelSimpleImage(newExtents,
						newResols, targetImageSlice);

				double cost_CRS, cost_MIS, cost_NMIS, cost_NXS;
				double cost_CRS_min = Double.MAX_VALUE, cost_MIS_min = Double.MAX_VALUE, cost_NMIS_min = Double.MAX_VALUE, cost_NXS_min = Double.MAX_VALUE;

				// looking for the best match image
				while (imageModelsItr.hasNext()) {
					Integer imageNumber = (Integer) imageModelsItr.next();
					// System.err.println("imageNumber = " + imageNumber);
					ImageModel imageModel = imageModels.get(imageNumber);

					ModelImage srcImage = imageModel.getImage();
					TransMatrix tMatrix = new TransMatrix(3);
					int costChoice;
					int bin = 256;
					float smoothSize = 1.0f;
					ModelSimpleImage simpleImg2 = new ModelSimpleImage(
							srcImage.getExtents(), srcImage.getFileInfo(0)
									.getResolutions(), srcImage);

					costChoice = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
					AlgorithmCostFunctions2D algoCost_CRS = new AlgorithmCostFunctions2D(
							simpleImg1, simpleImg2, costChoice, bin, smoothSize);
					cost_CRS = algoCost_CRS.cost(tMatrix);
					// System.err.println("cost_CRS = " + cost_CRS);
					algoCost_CRS.disposeLocal();
					algoCost_CRS = null;

					tMatrix = new TransMatrix(3);
					costChoice = AlgorithmCostFunctions2D.MUTUAL_INFORMATION_SMOOTHED;
					AlgorithmCostFunctions2D algoCost_MIS = new AlgorithmCostFunctions2D(
							simpleImg1, simpleImg2, costChoice, bin, smoothSize);
					cost_MIS = algoCost_MIS.cost(tMatrix);
					// System.err.println("cost_MIS = " + cost_MIS);
					algoCost_MIS.disposeLocal();
					algoCost_MIS = null;

					tMatrix = new TransMatrix(3);
					costChoice = AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
					AlgorithmCostFunctions2D algoCost_NMIS = new AlgorithmCostFunctions2D(
							simpleImg1, simpleImg2, costChoice, bin, smoothSize);
					cost_NMIS = algoCost_NMIS.cost(tMatrix);
					// System.err.println("cost_NMIS = " + cost_NMIS);
					algoCost_NMIS.disposeLocal();
					algoCost_NMIS = null;

					tMatrix = new TransMatrix(3);
					costChoice = AlgorithmCostFunctions2D.NORMALIZED_XCORRELATION_SMOOTHED;
					AlgorithmCostFunctions2D algoCost_NXS = new AlgorithmCostFunctions2D(
							simpleImg1, simpleImg2, costChoice, bin, smoothSize);
					cost_NXS = algoCost_NXS.cost(tMatrix);
					// System.err.println("cost_NXS = " + cost_NXS);
					algoCost_NXS.disposeLocal();
					algoCost_NXS = null;

					int count = 0;
					if (cost_CRS < cost_CRS_min) {
						count++;
					}
					if (cost_MIS < cost_MIS_min) {
						count++;
					}
					if (cost_NMIS < cost_NMIS_min) {
						count++;
					}
					if (cost_NXS < cost_NXS_min) {
						count++;
					}
					if (count >= 3) {
						cost_CRS_min = cost_CRS;
						cost_MIS_min = cost_MIS;
						cost_NMIS_min = cost_NMIS;
						cost_NXS_min = cost_NXS;
						bestImageNumber = imageNumber;
					}
					simpleImg2.disposeLocal(true);
					simpleImg2 = null;
				} // end imageModelsItr.hasNext()
				System.err.println("bestImageNumber  = " + bestImageNumber);

				// Integer imageNumber = (Integer)imageModelsItr.next();
				ImageModel imageModel = imageModels
						.get((Integer) bestImageNumber);

				Hashtable<String, Vector<Model>> sliceModel = imageModel.sliceModel;

				Vector<Model> aroundVector = sliceModel.get("Around");
				Vector<Model> blackVector = sliceModel.get("Black");
				Vector<Model> bottomVector = sliceModel.get("Bottom");
				Vector<Model> topVector = sliceModel.get("Top");

				// look the non-prostate regions first
				// Black regions
				int[] mask = new int[size];
				int[] centerMask = new int[size];

				double accuracy = 0;

				// black regions
				int blackSize = blackVector.size();
				int[][] classesBlack = new int[blackSize][size];

				if (blackSize > 0) {
					for (int index_black = 0; index_black < blackSize; index_black++) {
						Model model = blackVector.get(index_black);
						accuracy = predict.doPredict(classesBlack,
								featureArray, model, sliceNumber, index_black);
						System.err.println("index_black = " + index_black
								+ "  accuracy = " + accuracy);
					}

					for (int index_black = 0; index_black < blackSize; index_black++) {
						for (int j = 0; j < size; j++) {
							if (classesBlack[index_black][j] == 1) {
								mask[j] |= 1;
							} else {
								mask[j] |= 0;
							}

						}
						// maskDisplay(classesBlack[index_black], index_black);
					}
					// maskDisplay(mask, sliceNumber);
				}

				// top regions
				int topSize = topVector.size();
				int[][] classesTop = new int[topSize][size];
				if (topSize > 0) {
					for (int index_top = 0; index_top < topSize; index_top++) {
						Model model = topVector.get(index_top);
						accuracy = predict.doPredict(classesTop, featureArray,
								model, sliceNumber, index_top);
						System.err.println("index_top = " + index_top
								+ "  accuracy = " + accuracy);
					}

					// if ( accuracy < 70 ) {
					for (int index_top = 0; index_top < topSize; index_top++) {
						for (int j = 0; j < size; j++) {
							if (classesTop[index_top][j] > 0) {
								mask[j] |= 1;
							} else {
								mask[j] |= 0;
							}
						}
						// maskDisplay(classesTop[index_top], index_top);
						// }
					}
					// maskDisplay(mask, sliceNumber);
				}

				// bottom regions
				int bottomSize = bottomVector.size();
				int[][] classesBottom = new int[bottomSize][size];
				if (bottomSize > 0) {
					for (int index_bottom = 0; index_bottom < bottomSize; index_bottom++) {
						Model model = bottomVector.get(index_bottom);
						accuracy = predict.doPredict(classesBottom,
								featureArray, model, sliceNumber, index_bottom);
						System.err.println("index_bottom = " + index_bottom
								+ "  accuracy = " + accuracy);
					}

					// if ( accuracy < 70 ) {
					for (int index_bottom = 0; index_bottom < bottomSize; index_bottom++) {
						for (int j = 0; j < size; j++) {
							if (classesBottom[index_bottom][j] > 0) {
								mask[j] |= 1;
							} else {
								mask[j] |= 0;
							}
						}
						// maskDisplay(classesBottom[index_top], index_bottom);
						// }
					}
					// maskDisplay(mask, sliceNumber);
				}

				// around regions
				int aroundSize = aroundVector.size();
				int[][] classesAround = new int[aroundSize][size];
				if (aroundSize > 0) {
					for (int index_around = 0; index_around < aroundSize; index_around++) {
						Model model = aroundVector.get(index_around);
						// System.err.println(model.toString());
						accuracy = predict.doPredict(classesAround,
								featureArray, model, sliceNumber, index_around);

						System.err.println("index_around = " + index_around
								+ "  accuracy = " + accuracy);
					}

					// if ( accuracy < 70 ) {
					for (int index_around = 0; index_around < aroundSize; index_around++) {
						for (int j = 0; j < size; j++) {
							if (classesAround[index_around][j] > 0) {
								mask[j] |= 1;
							} else {
								mask[j] |= 0;
							}
						}
						// maskDisplay(classesAround[index_around],
						// index_around);
						// }
					}
					// maskDisplay(mask, sliceNumber);
				}

				System.err.println("sliceNumber = " + sliceNumber);
				ModelImage nonProstateImage = maskDisplay(mask, sliceNumber);

				imageModelsKeySet = imageModels.keySet();
				imageModelsItr = imageModelsKeySet.iterator();
				int[] boundaryMask = new int[size];

				// while (imageModelsItr.hasNext()) {

				// Integer imageNumber = (Integer) imageModelsItr.next();
				// ImageModelSVM imageModelSVM =
				// imageModelsSVM.get(imageNumber);

				ImageModelSVM imageModelSVM = imageModelsSVM
						.get(bestImageNumber);
				Hashtable<String, Vector<svm_model>> sliceModelSVM = imageModelSVM.sliceModel;
				Vector<svm_model> boundaryVector = sliceModelSVM
						.get("Boundary");
				// boundary
				int boundarySize = boundaryVector.size();
				int[][] classesBoundary = new int[boundarySize][size];

				if (boundarySize > 0) {
					for (int index_boundary = 0; index_boundary < boundarySize; index_boundary++) {
						svm_model model = boundaryVector.get(index_boundary);
						// accuracy = svm_predict.doPredict(classesBoundary,
						// featureArray, model, currentSlice)
						accuracy = svm_predict.doPredict(classesBoundary,
								featureArraySVM, model, sliceNumber,
								index_boundary);
						System.err.println("index_boundary = " + index_boundary
								+ "  accuracy = " + accuracy);
					}

					for (int index_boundary = 0; index_boundary < boundarySize; index_boundary++) {
						for (int j = 0; j < size; j++) {
							if (classesBoundary[index_boundary][j] == 1) {
								boundaryMask[j] |= 1;
							} else {
								boundaryMask[j] |= 0;
							}

						}
						// maskDisplay(classesBoundary[index_boundary],
						// index_boundary);
					}

				}
				classesBoundary = null;

				// } // end while
				ModelImage boundaryImage = maskDisplay(boundaryMask,
						sliceNumber);
				boundaryFinding(nonProstateImage, boundaryImage,
						targetImageSlice, sliceNumber);
				// pause();
				// new ViewJFrameImage(nonProstateImage);
				// new ViewJFrameImage(boundaryImage);
				// new ViewJFrameImage(targetImageSlice);
				mask = null;
				centerMask = null;
				classesBlack = null;
				classesTop = null;
				classesAround = null;
				boundaryMask = null;
			} // end if VOI.VOIat()
		} // end for i loop

		// targetImageSlice.disposeLocal();
		// targetImageSlice = null;

		buffer = null;
	}

	/**
	 * Dynamic narrow band tracing algorithm. Based on the target CED image,
	 * non-prostate binary mask image and prostate boundary mask image, the
	 * algorithm stretches the AAM initialization contour, and generate the
	 * final prostate contour.
	 * 
	 * @param nonProstateImage
	 *            non-prostate binary mask image
	 * @param boundaryImage
	 *            prostate boundary mask image.
	 * @param targetImageSlice
	 *            target CED image
	 * @param sliceNumber
	 *            current processing slice number.
	 */
	public void boundaryFinding(ModelImage nonProstateImage,
			ModelImage boundaryImage, ModelImage targetImageSlice,
			int sliceNumber) {

		AlgorithmProstateBoundaryExt boundaryAlgo = new AlgorithmProstateBoundaryExt(
				targetImageSlice, nonProstateImage, boundaryImage, sliceNumber);
		boundaryAlgo.addListener(this);
		boundaryAlgo.run();

		targetImageSlice.getVOIs().removeAllElements();

		targetImageSlice.addVOIs(boundaryAlgo.getExtractedVOI());
		smoothVOI30(targetImageSlice, targetImageSlice);
		updateResultImage(sliceNumber, targetImageSlice);

		boundaryAlgo.finalize();
		boundaryAlgo = null;

	}

	/**
	 * Smooth VOI with 30 points.
	 * 
	 * @param maskImage
	 *            mask image with VOI
	 * @param resultImage
	 *            resulting image to hold the smoothed VOI.
	 */
	public void smoothVOI30(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage,
					v.VOIAt(0), 30, false);
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
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	/**
	 * Display binary mask images. Debugging purpose.
	 * 
	 * @param mask
	 * @param sliceNumber
	 * @return
	 */
	public ModelImage maskDisplay(int[] mask, int sliceNumber) {

		int[] extents = cropTargetImage.getExtents();
		int size = extents[0] * extents[1];
		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];
		System.err.println("ext r = " + size);

		ModelImage classImageSlice = null;
		try {
			classImageSlice = new ModelImage(ModelStorageBase.INTEGER,
					newExtents, "image_" + sliceNumber);
			classImageSlice.importData(0, mask, true);
			// new ViewJFrameImage(classImageSlice);

			// }
		} catch (IOException e) {
			e.printStackTrace();
		}

		return classImageSlice;
	}

	/**
	 * NMI searching between target image and AAM atlas.
	 * 
	 * @param targetImageSliceNumber
	 *            target 2D slice number
	 * @param bestImageNumber
	 *            best matched image number
	 * @param bestSliceNumber
	 *            best matched slice number.
	 */
	private void findBestImageNMI(int targetImageSliceNumber,
			int[] bestImageNumber, int[] bestSliceNumber) {

		double cost_CRS, cost_MIS, cost_NMIS, cost_NXS;
		double mincost_CRS = Double.MAX_VALUE;
		double mincost_MIS = Double.MAX_VALUE;
		double mincost_NMIS = Double.MAX_VALUE;
		double mincost_NXS = Double.MAX_VALUE;

		// extract the target slice number
		int[] extents = new int[3];
		extents = cropTargetImage.getExtents();

		int[] newExtents = new int[2];
		newExtents[0] = extents[0];
		newExtents[1] = extents[1];

		int size = extents[0] * extents[1];

		ModelSimpleImage simpleImg1 = new ModelSimpleImage(newExtents);
		float[] buffer = new float[size];

		try {
			cropTargetImage.exportData(targetImageSliceNumber * size, size,
					buffer);
			simpleImg1.importData(buffer, 0, size);
		} catch (IOException e) {
			e.printStackTrace();
		}

		// search for the best match
		// int startIndex = ( targetImageSliceNumber > 6 ) ?
		// (targetImageSliceNumber - 1) : 6;
		// int endIndex = ( targetImageSliceNumber < 16 ) ?
		// (targetImageSliceNumber + 1) : 16;
		int startIndex = 0;
		int endIndex = 0;
		if (targetImageSliceNumber < startSlice + 3) {
			startIndex = startSlice;
			endIndex = startSlice + 3;
		} else if (targetImageSliceNumber > endSlice - 3) {
			startIndex = endSlice - 3;
			endIndex = endSlice;
		} else if (targetImageSliceNumber >= startSlice + 3
				&& targetImageSliceNumber <= endSlice - 3) {
			startIndex = targetImageSliceNumber - 1;
			endIndex = targetImageSliceNumber + 1;
		}

		for (int i = startIndex; i <= endIndex; i++) {
			Vector<ModelImage> cropImageVector = groupCropImages.get(i);
			int index = 0;
			for (ModelImage img : cropImageVector) {
				ModelSimpleImage simpleImg2 = new ModelSimpleImage(img);
				TransMatrix tMatrix = new TransMatrix(3);
				int costChoice;
				int bin = 256;
				float smoothSize = 1.0f;
				int count = 0;

				costChoice = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
				AlgorithmCostFunctions2D algoCost_CRS = new AlgorithmCostFunctions2D(
						simpleImg1, simpleImg2, costChoice, bin, smoothSize);
				cost_CRS = algoCost_CRS.cost(tMatrix);
				// System.err.println("cost_CRS = " + cost_CRS);
				algoCost_CRS.disposeLocal();
				algoCost_CRS = null;

				tMatrix = new TransMatrix(3);
				costChoice = AlgorithmCostFunctions2D.MUTUAL_INFORMATION_SMOOTHED;
				AlgorithmCostFunctions2D algoCost_MIS = new AlgorithmCostFunctions2D(
						simpleImg1, simpleImg2, costChoice, bin, smoothSize);
				cost_MIS = algoCost_MIS.cost(tMatrix);
				// System.err.println("cost_MIS = " + cost_MIS);
				algoCost_MIS.disposeLocal();
				algoCost_MIS = null;

				tMatrix = new TransMatrix(3);
				costChoice = AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
				AlgorithmCostFunctions2D algoCost_NMIS = new AlgorithmCostFunctions2D(
						simpleImg1, simpleImg2, costChoice, bin, smoothSize);
				cost_NMIS = algoCost_NMIS.cost(tMatrix);
				// System.err.println("cost_NMIS = " + cost_NMIS);
				algoCost_NMIS.disposeLocal();
				algoCost_NMIS = null;

				tMatrix = new TransMatrix(3);
				costChoice = AlgorithmCostFunctions2D.NORMALIZED_XCORRELATION_SMOOTHED;
				AlgorithmCostFunctions2D algoCost_NXS = new AlgorithmCostFunctions2D(
						simpleImg1, simpleImg2, costChoice, bin, smoothSize);
				cost_NXS = algoCost_NXS.cost(tMatrix);
				// System.err.println("cost_NXS = " + cost_NXS);
				algoCost_NXS.disposeLocal();
				algoCost_NXS = null;

				if (cost_CRS <= mincost_CRS)
					count++;
				if (cost_MIS <= mincost_MIS)
					count++;
				if (cost_NMIS <= mincost_NMIS)
					count++;
				if (cost_NXS <= mincost_NXS)
					count++;

				if (count >= 3) {
					mincost_CRS = cost_CRS;
					mincost_MIS = cost_MIS;
					mincost_NMIS = cost_NMIS;
					mincost_NXS = cost_NXS;
					bestSliceNumber[0] = index;
					bestImageNumber[0] = i;
				}
				index++;

			} // end for img :
		} // end for i loop
	}

	/**
	 * Crop target image for SVM classification.
	 * 
	 * @return
	 */
	public ModelImage cropTargetImage() {

		int[] extents = targetImage.getExtents();
		int zDim = extents[2] - 1;

		int[] xBounds = new int[2];
		int[] yBounds = new int[2];
		int[] zBounds = new int[2];

		boxYmin = 124;
		boxYmax = 380 - 1;
		boxXmin = 124;
		boxXmax = 380 - 1;

		xBounds[0] = boxXmin;
		xBounds[1] = boxXmax;
		yBounds[0] = boxYmin;
		yBounds[1] = boxYmax;
		zBounds[0] = 0;
		zBounds[1] = zDim;

		int borderSize = 0;
		try {
			int[] destExtents = null;
			if (targetImage.getNDims() == 3) {

				if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 3D
																// image
					// to 2D image
					destExtents = new int[2];
					destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
							+ (2 * borderSize);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
							+ (2 * borderSize);
				} else {
					destExtents = new int[3];
					destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
							+ (2 * borderSize);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
							+ (2 * borderSize);
					destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
				}
			} else {
				return null;
			}

			// create crop images
			cropTargetImage = new ModelImage(targetImage.getType(),
					destExtents, makeImageName(targetImage.getImageName(),
							"_crop"));

			int[] xCrop = new int[] { 0, 0 };
			int[] yCrop = new int[] { 0, 0 };
			int[] zCrop = new int[] { 0, 0 };
			if (destExtents.length > 0) {
				xCrop[0] = -1 * (xBounds[0] - borderSize);
				xCrop[1] = -1 * (xBounds[1] - destExtents[0] - 1);
			}
			if (destExtents.length > 1) {
				yCrop[0] = -1 * (yBounds[0] - borderSize);
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

			AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(targetImage,
					cropTargetImage, xCrop, yCrop, zCrop);

			cropAlgo.addListener(this);

			// Hide the dialog since the algorithm is about to run.
			// setVisible(false);

			cropAlgo.run();

			// UI.registerImage(cropKeyImage);
			// new ViewJFrameImage(cropKeyImage);

		} catch (OutOfMemoryError e) {
			MipavUtil
					.displayError("Dialog Crop: unable to allocate enough memory");

			return null;
		}

		return cropTargetImage;

	}

	/**
	 * After the target image with final segmented VOI.
	 * 
	 * @param sliceNumber
	 * @param targetImageSlice
	 */
	public void updateResultImage(int sliceNumber, ModelImage targetImageSlice) {

		VOIVector VOIs = targetImageSlice.getVOIs();

		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, 1);

		System.err.println("vArray length = " + vArray.length);
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

			/*
			 * for (int u = 0; u < nPts; u++ ) { zPtsZero[u] = sliceNumber; }
			 */
			vTemp.exportArrays(xPts, yPts, zPts);

			for (int u = 0; u < nPts; u++) {
				xPts[u] = xPts[u] + boxXmin;
				yPts[u] = yPts[u] + boxYmin;
				zPtsZero[u] = sliceNumber;
			}

			vTemp.importArrays(xPts, yPts, zPtsZero, nPts);
			voiNewFinal.importCurve(vTemp);
		}

	}

	/**
	 * Copy the 2D slice segmented VOI to target image silce.
	 * 
	 * @param sliceNumber
	 *            corresponding slice number
	 * @param targetImageSlice
	 *            target slice number.
	 */
	public void updateTargetImage(int sliceNumber, ModelImage targetImageSlice) {

		VOIVector voiVectorNew = new VOIVector();
		VOIVector VOIs = targetImageSlice.getVOIs();
		VOI voiNew = new VOI((short) 0, "slice" + sliceNumber);

		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, 1);

		System.err.println("vArray length = " + vArray.length);
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
		targetImage.addVOIs(voiVectorNew);
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
	 * Read target image
	 */
	public void readImagesAndVOIs() {
		FileIO targetImageIO = null;

		try {

			// read target image
			targetImageIO = new FileIO();
			targetImage = targetImageIO.readImage(targetImageName,
					targetImageDirectory);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Read SVM model atlas.
	 */
	private void readSVMModelDir() {
		svmModelChooser.setDialogTitle("Open Model Directory");
		svmModelChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = svmModelChooser.showOpenDialog(UI
				.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			svmModelName = svmModelChooser.getSelectedFile().getName();

			svmModelDirectory = String.valueOf(svmModelChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ svmModelName
					+ File.separatorChar;
			// UI.setDefaultDirectory(directory);
			textFieldSVMModel.setText(svmModelDirectory);

			File fileDir = new File(svmModelDirectory);
			// System.err.println("check = " + keyImageDirectory);
			svmTraverseDir(fileDir);
			svmSortModelStructure();

			// printStructure();
		} else {
			return;
		}
	}

	/**
	 * Processing SVM model atlas.
	 * 
	 * @param dir
	 */
	private void svmTraverseDir(File dir) {
		// processDir(dir);

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {

				// System.err.println("child" + i + " = " + children[i]);
				File currentDir = new File(dir + File.separator + children[i]);
				if (currentDir.isDirectory()) {

					Hashtable<String, ImageAttribute> imageStructure = new Hashtable<String, ImageAttribute>();
					String[] imagesChildren = currentDir.list();

					for (int imgIndex = 0; imgIndex < imagesChildren.length; imgIndex++) {

						File imageDir = new File(currentDir + File.separator
								+ imagesChildren[imgIndex]);

						if (imageDir.isDirectory()) {
							String[] sliceChildren = imageDir.list();

							Hashtable<String, Vector> sliceStructure = new Hashtable<String, Vector>();
							String imageName = null;
							for (int j = 0; j < sliceChildren.length; j++) {
								// System.err.println(" slice child " + j +
								// " = " + sliceChildren[j]);

								File fileDir = new File(imageDir
										+ File.separator + sliceChildren[j]);
								// imageName = imageDir + File.separator +
								// sliceChildren[j];
								Vector<String> modelFileName = new Vector<String>();
								if (fileDir.isDirectory()) {
									String[] fileChildren = fileDir.list();
									int size = fileChildren.length;
									for (int k = 0; k < size; k++) {
										if (fileChildren[k].startsWith("img")
												&& fileChildren[k]
														.endsWith(".model")) {
											modelFileName.add(fileDir
													+ File.separator
													+ fileChildren[k]);
										}
									}
									sliceStructure.put(sliceChildren[j],
											modelFileName);
								} else if (fileDir.isFile()) { // search for the
																// image--.xml
																// file
									if (fileDir.getName().endsWith(".xml")) {
										imageName = imageDir + File.separator
												+ sliceChildren[j];
										// System.err.println("imageName = " +
										// imageName);
									}
								}
							}

							imageStructure.put(imagesChildren[imgIndex],
									new ImageAttribute(
											imagesChildren[imgIndex],
											imageName, sliceStructure));
						}

					} // end imgIndex loop;
					modelStructure.put(children[i], imageStructure);
				} // end if
			} // end i loop
		}

	}

	/**
	 * sort SVM model with names.
	 */
	private void svmSortModelStructure() {
		String[] keys = modelStructure.keySet().toArray(new String[0]);
		Hashtable<Integer, String> sortedNumber = new Hashtable<Integer, String>();

		for (String key : keys) {
			// System.err.println(key);
			String[] result = key.split("_");
			Integer number = Integer.valueOf(result[0].substring(5,
					result[0].length()));
			// System.err.println("number = " + number);
			sortedNumber.put(number, key);
		}

		Integer[] numberKeys = sortedNumber.keySet().toArray(new Integer[0]);
		Arrays.sort(numberKeys);
		for (Integer numberKey : numberKeys) {
			String myKey = sortedNumber.get(numberKey);
			// System.err.println(numberKey);
			sortModelStructure.put(numberKey, modelStructure.get(myKey));
		}

	}

	/**
	 * Read target image.
	 */
	private void readTargetImage() {
		targetImageChooser.setDialogTitle("Open Target Image");

		if (UI.getDefaultDirectory() != null) {
			final File file = new File(UI.getDefaultDirectory());

			if (file != null) {
				targetImageChooser.setCurrentDirectory(file);
			}
		} else {
			targetImageChooser.setCurrentDirectory(new File(System
					.getProperty("user.dir")));
		}

		targetImageChooser.addChoosableFileFilter(new ViewImageFileFilter(
				new String[] { ".xml", ".XML" }));

		final int returnValue = targetImageChooser.showOpenDialog(UI
				.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			targetImageName = targetImageChooser.getSelectedFile().getName();
			targetImageDirectory = String.valueOf(targetImageChooser
					.getCurrentDirectory()) + File.separatorChar;
			// UI.setDefaultDirectory(directory);
			textFieldImageTarget.setText(targetImageName);

			readImagesAndVOIs();

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
				endSliceImageNames.add(dir.getAbsolutePath() + File.separator
						+ children[i]);

			} else if (children[i].startsWith("end")
					&& children[i].endsWith(".txt")) {
				endSliceRangeNames.add(dir.getAbsolutePath() + File.separator
						+ children[i]);
				// System.err.println(dir.getAbsolutePath() + File.separator +
				// children[i]);
			}
		}
	}

	/**
	 * Read AAM model atlas.
	 */
	private void readKeyImageDir() {
		String modelName;
		modelChooser.setDialogTitle("Open Model Directory");
		modelChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

		final int returnValue = modelChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			modelName = modelChooser.getSelectedFile().getName();
			modelDirectory = String.valueOf(modelChooser.getCurrentDirectory())
					+ File.separatorChar + modelName + File.separator;

			// UI.setDefaultDirectory(directory);
			textFieldModel.setText(modelDirectory);

			System.err.println("modelDirectory = " + modelDirectory);

			processingData(modelDirectory);
			// boolean ok=model.ReadModel( modelDirectory );
			// printTest();
		} else {
			return;
		}
	}

	/**
	 * Read the end slices and update the segmentation processing range.
	 */
	private void readEndingSlice() {
		FileIO imageIO = null;
		imageIO = new FileIO();
		int size = endSliceImageNames.size();
		int start = 0, end = 0;
		String imageName;
		String rangeName;

		for (int i = 0; i < size; i++) {
			imageName = endSliceImageNames.get(i);
			rangeName = endSliceRangeNames.get(i);
			endSliceImages.add(imageIO.readImage(imageName));
			try {
				BufferedReader in = new BufferedReader(
						new FileReader(rangeName));
				String line = in.readLine();
				String[] result = line.split(" ");
				start = (int) Integer.valueOf(result[0]);
				end = (int) Integer.valueOf(result[1]);

				in.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
			endSliceRange.add(new Range(start, end));
		}
	}

	/**
	 * Read the AAM model.
	 */
	private void readSampleImages() {
		FileIO imageIO = null;
		imageIO = new FileIO();

		try {
			for (int j = 3; j <= 20; j++) {
				Vector<ModelString> modelNameVector = groupTable.get(j);
				int size = modelNameVector.size();
				Vector<ModelImage> refImages = new Vector<ModelImage>();
				for (int i = 0; i < size; i++) {
					ModelString m = modelNameVector.get(i);
					refImages.add(imageIO.readImage(m.imageName));
				}
				groupRefImages.put(j, refImages);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	/**
	 * Process AAM model directory.
	 * 
	 * @param modelDir
	 */
	private void processingData(String modelDir) {
		File dir = new File(modelDir);
		traverse(dir);
	}

	/**
	 * Recursively read AAM dir, and update the AAM model structure.
	 * 
	 * @param dir
	 */
	private void traverse(File dir) {

		if (dir.isDirectory()) {
			// System.err.println("dir name = " + dir.toString());
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				// System.err.println("children[" + i + "]= " + children[i]);
				if (children[i].contains("slice")) {
					int number = Integer.valueOf(children[i].substring(5,
							children[i].length()));
					// System.err.println("number = " + number);

					Vector<ModelString> modelNameVector = new Vector<ModelString>();

					processDir(new File(dir, children[i]), modelNameVector);

					groupTable.put(number, modelNameVector);
					// traverse(new File(dir, children[i]));
				}
			}
		}
		// System.err.println("check");
	}

	/**
	 * Processing the AAM dir with AAM model string and pivot sample image name.
	 * 
	 * @param dir
	 * @param modelNameVector
	 */
	private void processDir(File dir, Vector<ModelString> modelNameVector) {
		if (dir.isDirectory()) {
			String[] childrenGroup = dir.list();
			for (int j = 0; j < childrenGroup.length; j++) {
				File groupDirFile = new File(dir + File.separator
						+ childrenGroup[j]);
				String[] children = groupDirFile.list();

				String modelName = null;
				String imageName = null;
				String voiName = null;
				for (int i = 0; i < children.length; i++) {
					File file = new File(groupDirFile.getAbsolutePath(),
							children[i]);
					if (children[i].equals("model.amf")) {
						modelName = groupDirFile.getAbsolutePath()
								+ File.separator + children[i];
					} else if (children[i].equals("sample.xml")) {
						imageName = groupDirFile.getAbsolutePath()
								+ File.separator + children[i];
					} else if (children[i].equals("samplevoi.xml")) {
						voiName = groupDirFile.getAbsolutePath()
								+ File.separator + children[i];
					}

				}
				if (modelName != null && imageName != null) {
					modelNameVector.add(new ModelString(modelName, imageName,
							voiName));
				}

			} // end for int j
				// pause();
		}
	}

	public void disposeLocal() {

	}

	/**
	 * Segmentation processing range
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

	/**
	 * Image attribute for AAM model.
	 * 
	 * @author ruida
	 * 
	 */
	class ImageAttribute {
		public String imageNumber;
		public String imageName;
		public Hashtable<String, Vector> sliceStructure;

		public ImageAttribute(String _imageNumber, String _imageName,
				Hashtable<String, Vector> _sliceStructure) {
			imageNumber = _imageNumber;
			imageName = _imageName;
			sliceStructure = _sliceStructure;
		}

		public Hashtable<String, Vector> getSliceStructure() {
			return sliceStructure;
		}

		public String getImageName() {
			return imageName;
		}

	}

	/**
	 * Image model of AAM.
	 * 
	 * @author ruida
	 * 
	 */
	class ImageModel {
		public int imageNumber;
		public ModelImage image;
		Hashtable<String, Vector<Model>> sliceModel;

		public ImageModel(int _imageNumber, ModelImage _image,
				Hashtable<String, Vector<Model>> _sliceModel) {
			imageNumber = _imageNumber;
			image = _image;
			sliceModel = _sliceModel;
		}

		public ModelImage getImage() {
			return image;
		}

	}

	/**
	 * Image SVM model.
	 * 
	 * @author ruida
	 * 
	 */
	class ImageModelSVM {
		public int imageNumber;
		public ModelImage image;
		Hashtable<String, Vector<svm_model>> sliceModel;

		public ImageModelSVM(int _imageNumber, ModelImage _image,
				Hashtable<String, Vector<svm_model>> _sliceModel) {
			imageNumber = _imageNumber;
			image = _image;
			sliceModel = _sliceModel;
		}

		public ModelImage getImage() {
			return image;
		}

	}

	/**
	 * AAM model slice struct.
	 * 
	 * @author ruida
	 * 
	 */
	class SliceSetString {
		int sliceNumber;
		Vector<String> imageVector;
		Vector<String> voiVector;

		public SliceSetString(int _number, Vector<String> _imageVector,
				Vector<String> _voiVector) {
			sliceNumber = _number;
			imageVector = _imageVector;
			voiVector = _voiVector;
		}
	}

	/**
	 * Model string struct.
	 * 
	 * @author ruida
	 * 
	 */
	class ModelString {
		String modelName;
		String imageName;
		String voiName;

		public ModelString(String _modelName, String _imageName, String _voiName) {
			modelName = _modelName;
			imageName = _imageName;
			voiName = _voiName;
		}
	}

	/**
	 * Shape factor struct.
	 * 
	 * @author ruida
	 * 
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

		public ShapeFactor(float _formFactor, float _roundness,
				float _compactness, float _aspectRatio, float _eccentricity,
				float _area, float _perimeter, float _avgIntensity) {
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
