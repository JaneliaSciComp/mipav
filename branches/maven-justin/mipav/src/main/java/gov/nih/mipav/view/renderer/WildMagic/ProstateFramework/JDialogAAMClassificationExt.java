package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.*;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.view.renderer.WildMagic.AAM.*;

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
public class JDialogAAMClassificationExt extends JDialogBase implements
		AlgorithmInterface {

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
	private JLabel labelImageTarget;
	private JTextField textFieldImageTarget;
	private JButton buttonImageTarget;

	private JPanel imageSelectionPanel;
	private JPanel buttonPanel;

	/** key images variables. */
	private JFileChooser modelChooser = new JFileChooser();
	private String modelDirectory;

	/** target image variables. */
	private JFileChooser targetImageChooser = new JFileChooser();
	private String targetImageName;
	private String targetImageDirectory;

	private ModelImage targetImage;
	private ModelImage cropTargetImage;

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

	private Vector<PolygonShapeInfo> shapeVector = new Vector<PolygonShapeInfo>();
	private ModelImage sampleImage;

	private Hashtable<Integer, Vector<ModelString>> groupTable = new Hashtable<Integer, Vector<ModelString>>();
	private Hashtable<Integer, Vector<ModelImage>> groupRefImages = new Hashtable<Integer, Vector<ModelImage>>();
	private Hashtable<Integer, Vector<ModelImage>> groupCropImages = new Hashtable<Integer, Vector<ModelImage>>();

	private Hashtable<Integer, Integer> markedVOI = new Hashtable<Integer, Integer>();
	private Vector<ShapeFactor> shapeDescriptor = new Vector<ShapeFactor>();
	private Vector<Vector<Float>> statData = new Vector<Vector<Float>>();

	private static final String[] statsToCalculate = new String[] {
			VOIStatisticalProperties.areaDescription,
			VOIStatisticalProperties.perimeterDescription,
			VOIStatisticalProperties.avgIntensity,
			VOIStatisticalProperties.eccentricityDescription,
			VOIStatisticalProperties.majorAxisDescription,
			VOIStatisticalProperties.minorAxisDescription };

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

	private JPanel mainPanel;

	private int startSlice = 3;
	private int endSlice = 20;

	private Vector<String> endSliceImageNames = new Vector<String>();
	private Vector<String> endSliceRangeNames = new Vector<String>();

	private Vector<ModelImage> endSliceImages = new Vector<ModelImage>();
	private Vector<Range> endSliceRange = new Vector<Range>();

	/** image vector to hold the image names. */
	private Vector<String> keyImageVector = new Vector<String>();
	/** image vector to hold the actual images. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();
	
	private String testImageDir = "/scratch/aam_test/fold5/";
	
	/**
	 * Constructor. the parent frame
	 * 
	 * @param theParentFrame
	 */
	public JDialogAAMClassificationExt(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
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
			doSegmentation();
		} else if (command.equals("quickSegOK")) {
			setVisible(false);
			quickSegmentation();
		} else if (command.equals("SetAxis")) {
			axis = axisList.getSelectedIndex();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("ChooseModel")) {
			try {
			System.err.println("start read image dir");
			readKeyImageDir();
			readEndingSlicesDir();
			System.err.println("start read images");
			readSampleImages();
			readEndingSlice();
			} catch ( Exception e ) {
				e.printStackTrace();
			}
			System.err.println("finish read sample images");
		} else if (command.equals("ChooseTargetImage")) {
			// readTargetImage();
			File fileDir_test = new File(testImageDir);
			traverse_80(fileDir_test);
			loadFiles();
		}

	}

	/**
	 * load image files and voi files
	 */
	public void loadFiles() {
		// readFiles();
		readFile();
		System.err.println("finish image I/O");

	}
	
	public void readFile() {

		int index;

		System.err.println("keyImageVector.size() = " + keyImageVector.size());
		
		try {
			// read key images and VOIs
			// for (int i = 0; i < keyImageVector.size(); i++) {
			int start = 0;
			int end = keyImageVector.size();
			int currentIndex = 0;
			for (int imageIndex = start; imageIndex < end; imageIndex++) {
				// read key image
				String dir = keyImageVector.get(imageIndex);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex, keyImageIO.readImage(fileName, directory));

				// new ViewJFrameImage(keyImages.get(currentIndex));
				
				currentIndex++;
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	
	private void traverse_80(File dir) {
		processDir_80(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_80(new File(dir, children[i]));
			}
		}

	}

	private void processDir_80(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			System.err.println(dir.toString());
			keyImageVector.add(dir.toString());

		}

	}

	
	/**
	 * First time this dialog is called, init() will be called to create the
	 * GUI. When user choose the dialog again, we don't need to reload the AAM
	 * atlas model again. So, we create this target dialog to allow user to just
	 * select the target image for AAM classification.
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
	 * Init() function to create the GUI dialog.
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
		imageSelectionPanel.setLayout(new GridLayout(3, 3));
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

		// Key image directory
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelModel = new JLabel("Model Directory: ");
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

		// target image directory
		gbc.gridx = 0;
		gbc.gridy = 2;
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
	 * After the first time the dialog is invoked, when user select the AAM
	 * segmentation from the menu item, the quick segmentation function is
	 * called to do the AAM classification. It omits the AAM atlas model
	 * re-loading procedure.
	 */
	public void quickSegmentation() {

		long startTime = System.currentTimeMillis();
		cropTargetImage();
		// checkEndSlices();

		segmentationAuto();
		evaluateShapeDescriptor();
		markErrorOne();
		fixErrorVOI();
		transformVOI();
		new ViewJFrameImage(targetImage);
		dispose();

		long endTime = System.currentTimeMillis();
		long diffTime = endTime - startTime;
		float seconds = ((float) diffTime) / 1000;
		System.err.println("** Algorithm took  " + (float) (seconds)
				+ "  seconds \n");
	}

	/**
	 * First time the dialog called, this function is invoked to do
	 * segmentation.
	 */
	public void doSegmentation() {
		
		boolean firstTime = true;
		long startTime = System.currentTimeMillis();

		int index1, index2;

		System.err.println("keyImageVector.size() = " + keyImageVector.size());
		
		try {
			// read key images and VOIs
			int start = 0;
			int end = keyImageVector.size();
			
			for (int imageIndex = start; imageIndex < end; imageIndex++) {
				if ( targetImage != null ) 
					targetImage = null;
				// read key image
				targetImage = keyImages.get(imageIndex);
				
				cropTargetImage();
				
				if ( firstTime ) {
					cropImage();
					checkEndSlices();
					firstTime = false;
				}
				segmentationAuto();

				evaluateShapeDescriptor();

			
				markErrorOne();
				fixErrorVOI();
				transformVOI();
				// new ViewJFrameImage(targetImage);
			
				String dir = targetImage.getImageFileName();
				index1 = dir.lastIndexOf(File.separator);
				index2 = dir.lastIndexOf(".");
				
				String fileName = new String(dir.substring(index1 + 1, index2));
				String numString = fileName.substring(5, fileName.length());
				int imageNumber = Integer.valueOf(numString);
				VOI voiNew = targetImage.getVOIs().elementAt(0);
				FileVOI fileVOI = new FileVOI("aam_voi" + imageNumber + ".xml", testImageDir, targetImage);
				fileVOI.writeVOI(voiNew, true);
				
				dispose();
			}
		
		} catch (IOException e ) {
			e.printStackTrace();
		}
		
		dispose();

		long endTime = System.currentTimeMillis();
		long diffTime = endTime - startTime;
		float seconds = ((float) diffTime) / 1000;
		System.err.println("** Algorithm took  " + (float) (seconds)
				+ "  seconds \n");

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
	 * Evaluate the auto segmentation generated VOIs shapes.
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
		// Vector<Vector<String>> columns = getStatsData(statsList,
		// targetImage.getVOIs(), targetImage);
		getStatsData(statsList, targetImage.getVOIs(), targetImage);
		computeShapeDescriptor();

	}

	/**
	 * Generate the VOI shape statistics.
	 * 
	 * @param statsList
	 *            VOI statistic properties carry out by MIPAV
	 * @param VOIs
	 *            VOIs contours
	 * @param img
	 *            image
	 * @return statistic data vetor.
	 */
	private Vector<Vector<String>> getStatsData(
			Vector<VOIStatisticalProperties> statsList, VOIVector VOIs,
			ModelImage img) {

		Vector<Vector<String>> data = new Vector<Vector<String>>();

		statData.clear();

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
	 * Compute the shape descriptor with roundness, compactness, aspectRatio,
	 * and eccentricity.
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
	 * Go through the auto segmented VOIs, filter out the irregular shape or
	 * wrong shapes. Mark those shapes with flag 1 to indicating the
	 * replacement.
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

			VOIBaseVector current_va = targetImage.getVOIs().VOIAt(i).getCurves();
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
	 * Replace the irregular shape or wrong shape VOI with adjacent neighbor
	 * VOI.
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
	 * From the target 2D slice, compare it with end slices based atlas. Find
	 * the closed 2D slice image, and invoke the corresponding start and ending
	 * VOI index. This step is important, which eliminates the segmentation error
	 * toward apex and base to certain extents. There are still changes that
	 * apex and base ending index miss interpolated, resulting in the wrong
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
		/*
		 * ModelImage bestImage = endSliceImages.get(bestIndex); new
		 * ViewJFrameImage(bestImage); System.err.println("bestIndex = " +
		 * bestIndex + "  image name = " + bestImage.getImageName());
		 */
		System.err.println("startSlice = " + startSlice + "   endSlice = "
				+ endSlice);

	}

	/**
	 * Driver to do the auto segmenation with AAM model.
	 */
	public void segmentationAuto() {

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
        
		if ( endSlice >= extents[2]-1 ) {
			endSlice = extents[2]-1;
		}
		
		
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
				// new ViewJFrameImage(sampleImage);

			} catch (Exception e) {
				e.printStackTrace();
			}

			CAAMConsoleModeE e = new CAAMConsoleModeE();
			e.classification(model, targetImageSlice, sampleImage);

			updateTargetImage(i, targetImageSlice);

		} // end for i loop

	}

	/**
	 * When we to NMI based image similarity measure, we crop the prostate 2D
	 * slice image first, then apply the measure. Two advantages, 1) cropped
	 * region focus on the central gland region of prostate, 2) cropped region
	 * comparison is much faster that the whole image slice (512x512) based
	 * comparison, yelding relative accurate NMI comparison results.
	 */
	public void cropImage() {

		for (int j = startSlice; j <= endSlice; j++) {
			// System.err.println("j = " + j);
			
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

					// Hide the dialog since the algorithm is about to run.
					setVisible(false);

					cropAlgo.run();

					cropImageVector.add(cropImage);

				} catch (OutOfMemoryError e) {
					MipavUtil
							.displayError("Dialog Crop: unable to allocate enough memory");

					return;
				}

			}

			groupCropImages.put(j, cropImageVector);
		} // end for j loop

	}

	/**
	 * NMI based 2D slice image similarity measure. For each target image 2D
	 * slice, we use NMI to search the best match image 2D slice. The search
	 * range is 3 slices around the current 2D slice index. When find the best
	 * slice, we record it, and invoke the corresponding AAM Model to do
	 * segmentation.
	 * 
	 * @param targetImageSliceNumber
	 *            target image
	 * @param bestImageNumber
	 *            best match image number
	 * @param bestSliceNumber
	 *            best match slice number.
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

				if (cost_CRS < mincost_CRS)
					count++;
				if (cost_MIS < mincost_MIS)
					count++;
				if (cost_NMIS < mincost_NMIS)
					count++;
				if (cost_NXS < mincost_NXS)
					count++;

				if (count > 3) {
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
	 * Crop the target image 2D slice to focus on central gland.
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
			setVisible(false);

			cropAlgo.run();

		} catch (OutOfMemoryError e) {
			MipavUtil
					.displayError("Dialog Crop: unable to allocate enough memory");

			return null;
		}

		return cropTargetImage;

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
	 * read target image.
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
	 * File chooser to select target image directory.
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
		String endSliceModelDir = "/scratch/endSlices/slice12" + File.separator;
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
	 * Read the AAM atlas directory.
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

			textFieldModel.setText(modelDirectory);

			System.err.println("modelDirectory = " + modelDirectory);

			processingData(modelDirectory);

		} else {
			return;
		}
	}

	/**
	 * update the ending slices.
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
	 * When read the AAM model atlas, read corresponding pivot images from atlas
	 * direction. This pivot 2D image slice will guide the NMI similarity
	 * searching.
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
	 * Process AAM atlas directory.
	 * 
	 * @param modelDir
	 */
	private void processingData(String modelDir) {
		File dir = new File(modelDir);
		traverse(dir);
	}

	/**
	 * Recursivly traverse the AAM atlas directory.
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
	 * Process dir, read AAM model and pivot images.
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
				// System.err.println("groupDirFile name = " +
				// groupDirFile.getAbsolutePath());

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
