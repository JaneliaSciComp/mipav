package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.FeatureNode;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

/**
 * After the 3D images convert to 2D slices ( 512x512 ), this class picks the 2D
 * slices, extracts the Coherence Enhanced diffusion based features, and saves
 * those features with linear SVM readable file formats. User specifies the 2D
 * slices saved directory as input, and the same directory as the output. The
 * class automatically apply the following steps, 1) picks those slices, cropped
 * region according to central prostate gland. 2) apply Coherence Enhanced filter to
 * each cropped slice. 3) Save the Hurst index and Haralick features of each 2D
 * slice to corresponding directory.
 * 
 * Those saved CE features will be used to create linear SVM models. Also, for test
 * cases, this class functions as the pre-processing steps to save CE features before
 * automatic segmentation. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateExtractCEFeature extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194239L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** SVM option button. */
	private JRadioButton radioButtonSVMBinary;
	private JRadioButton radioButtonSVMMulticlass;

	/** flag to indicate is SVM training with binary class or multi-class. */
	private boolean svmTrainingBinary = true;

	/** cropped image boundary. */
	private int boxYmin, boxYmax;
	private int boxXmin, boxXmax;

	/** x dimension bounds */
	private int[] xBounds = new int[2];

	/** y dimension bound */
	private int[] yBounds = new int[2];

	/** z dimension bound */
	private int[] zBounds = new int[2];

	/** crop image algorithm */
	private AlgorithmAddMargins cropAlgo;

	/** Algorithm to save Hurst index and Haralick features. */
	private AlgorithmProstateFeaturesSaveAutoTrain textureAlgo;

	/** image intensity. */
	private boolean imageIntensityFilter = false;

	/** Coherence Enhancing Diffusion filter. */
	private boolean coherenceEnhancingDiffusionFilter = false;

	/** Anisotropic Diffusion filter. */
	private boolean regisotropicDiffusionFilter = false;

	/** Inhomogeneity N3 correction. */
	private boolean IHN3CorrectionFilter = false;

	/** Haralick mode filter. */
	private boolean modeFilter = false;

	/** Haralick mean filter. */
	private boolean meanFilter = false;

	/** Haralick median filter. */
	private boolean medianFilter = false;

	/** Haralick invert filter. */
	private boolean invertFilter = false;

	/** haralick filter. */
	private boolean haralickFilter = true;

	/** gabor filter. */
	private boolean gaborFilter = false;

	/** Hurst index. */
	private boolean hurstFilter = true;

	/** Wavelet filter. */
	private boolean waveletFilter = false;

	/** gussian filter. */
	private boolean gaussianFilter = false;

	/** gradient magnitude filter. */
	private boolean gmFilter = false;

	/** number features being saved. */
	private int numberFeatures = 0;

	/** Haralick feature numbers. */
	private int haralickFeatureNumber = 4;

	/** 2D Slices key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	/** 2D slices image selection panel. */
	private JPanel imageSelectionPanel;

	/** 2D slice images file chooser. */
	private JFileChooser keyImageChooser = new JFileChooser();

	/** 2D slices image directory. */
	private String keyImageDirectory;

	/** axis region. */
	private JComboBox axisList;
	private JLabel labelAxis;

	private static int Axial = 0;
	private static int Saggital = 1;
	private static int Coronal = 2;
	private int axis = Axial;

	/** wavelet feature number. */
	private int waveletFeatureNumber = 1;

	/** key images names vector. */
	private Vector<String> keyImageVector = new Vector<String>();

	/** key images vector. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();

	/** key image VOI names vector. */
	private Vector<String> keyImageVOIVector = new Vector<String>();

	/** key image VOIs vector. */
	private Vector<VOI[]> keyImageVOIs = new Vector<VOI[]>();

	/** cropped key image, crop from 512x512 to central prostate gland region. */
	private Vector<ModelImage> cropKeyImages = new Vector<ModelImage>();

	/** cropped CE image vector. */
	private Vector<Vector<ModelImage>> cropKeyImagesCE = new Vector<Vector<ModelImage>>();

	public static final String modelDirName = "model";
	public static final String featureDirName = "featureCE";

	/** Save features directory. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;

	/** Saved features directory. */
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateExtractCEFeature(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
		calculateNumberFeatures();
		setVisible(true);

	}

	/** dispose memory. */
	public void disposeLocal() {
		int i;
		for (i = 0; i < keyImageVector.size(); i++) {
			String temp = keyImageVector.get(i);
			temp = null;
		}
		keyImageVector = null;

		for (i = 0; i < keyImages.size(); i++) {
			ModelImage temp = keyImages.get(i);
			temp.disposeLocal();
		}
		keyImages = null;

		for (i = 0; i < keyImageVOIVector.size(); i++) {
			String temp = keyImageVOIVector.get(i);
			temp = null;
		}
		keyImageVOIVector = null;

		for (i = 0; i < keyImageVOIs.size(); i++) {
			VOI[] temp = keyImageVOIs.get(i);
			temp = null;
		}
		keyImageVOIs = null;

		for (i = 0; i < cropKeyImages.size(); i++) {
			ModelImage temp = cropKeyImages.get(i);
			temp.disposeLocal();
		}
		cropKeyImages = null;

	}

	/**
	 * Calculate number of features being saved. Currently, only Hurst index and
	 * Haralick features being used.
	 */
	private void calculateNumberFeatures() {

		if (imageIntensityFilter == true) {
			numberFeatures++;
		}
		if (coherenceEnhancingDiffusionFilter == true) {
			numberFeatures++;
		}
		if (regisotropicDiffusionFilter == true) {
			numberFeatures++;
		}
		if (IHN3CorrectionFilter == true) {
			numberFeatures++;
		}
		if (modeFilter == true) {
			numberFeatures++;
		}
		if (meanFilter == true) {
			numberFeatures++;
		}
		if (medianFilter == true) {
			numberFeatures++;
		}
		if (invertFilter == true) {
			numberFeatures++;
		}
		if (gaborFilter == true) {
			numberFeatures++;
		}
		if (hurstFilter == true) {
			numberFeatures++;
		}

		if (waveletFilter == true) {
			numberFeatures += waveletFeatureNumber;
		}

		if (gaussianFilter == true) {
			numberFeatures++;
		}
		if (gmFilter == true) {
			numberFeatures++;
		}
		if (haralickFilter == true) {
			numberFeatures += haralickFeatureNumber;
		}

	}

	/**
	 * Action performed handler for this dialog.
	 */
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();
		String command = event.getActionCommand();
		if (command.equals("OK")) {
			callAlgorithm();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			// MipavUtil.showHelp("Haral1001");
		} else if (source == radioButtonSVMBinary) {
			svmTrainingBinary = true;
		} else if (source == radioButtonSVMMulticlass) {
			svmTrainingBinary = false;
		} else if (command.equals("ChooseKeyImageDir")) {
			readKeyImageDir();
			// printImages();
			// sortKeyImage();
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	/**
	 * Record the save 2D slice features directory.
	 */
	private void recordSaveImageDir() {
		String saveImageName;
		saveImageChooser.setDialogTitle("Open Saved Images Directory");
		saveImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = saveImageChooser.showOpenDialog(UI
				.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			saveImageName = saveImageChooser.getSelectedFile().getName();

			saveImageDirectory = String.valueOf(saveImageChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ saveImageName + File.separatorChar;
			textFieldSaveImage.setText(saveImageDirectory);

		} else {
			return;
		}
	}

	/** Read 2D slices. */
	private void readKeyImageDir() {
		String keyImageName;
		keyImageChooser.setDialogTitle("Open Key Images Directory");
		keyImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = keyImageChooser.showOpenDialog(UI
				.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			keyImageName = keyImageChooser.getSelectedFile().getName();

			keyImageDirectory = String.valueOf(keyImageChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ keyImageName
					+ File.separatorChar;
			// UI.setDefaultDirectory(directory);
			textFieldKeyImage.setText(keyImageDirectory);

			File fileDir = new File(keyImageDirectory);
			// System.err.println("check = " + keyImageDirectory);
			traverse(fileDir);
		} else {
			return;
		}

	}

	/**
	 * debuger for file names read.
	 */
	public void printImages() {
		int len = keyImageVOIVector.size();
		for (int i = 0; i < len; i++) {
			System.err.println(keyImageVector.get(i));
			System.err.println(keyImageVOIVector.get(i));
		}
	}

	/**
	 * Traverse the 2D slices directory.
	 * 
	 * @param dir
	 */
	private void traverse(File dir) {
		processDir(dir);

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse(new File(dir, children[i]));
			}
		}

	}

	/**
	 * Process directory, save the image names and voi names.
	 * 
	 * @param dir
	 */
	private void processDir(File dir) {
		String dirName = dir.toString();
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		String axisString = "";
		if (axis == Axial) {
			axisString = "ax";
		} else if (axis == Saggital) {
			axisString = "sag";
		} else if (axis == Coronal) {
			axisString = "cor";
		}
		if (dirName.substring(begin, end).startsWith("img")
				&& dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains(axisString)) {
			keyImageVector.add(dir.toString());

		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains(axisString)) {
			keyImageVOIVector.add(dir.toString());
		}

	}

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
		// dispose();
		// System.gc();
	}

	/**
	 * Driver to do automatic feature extraction.
	 */
	public void callAlgorithm() {
		long startTime = System.currentTimeMillis();

		readFiles();

		cropKeyImages();

		System.err.println("extract feature");
		calculateCoherenceEnhancingDiffusion();
		extractFeatures();

		disposeLocal();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	/**
	 * From the cropped images, apply Coherence Enhanced Diffusion filter to
	 * each 2D slices.
	 */
	private void calculateCoherenceEnhancingDiffusion() {

		int numIterations;
		float diffusitivityDenom;
		float derivativeScale;
		float gaussianScale;
		boolean do25D;
		boolean entireImage;

		derivativeScale = 0.5f;
		diffusitivityDenom = 0.001f;
		gaussianScale = 2.0f;
		numIterations = 20;
		do25D = true;
		entireImage = true;

		for (int i = 0; i < cropKeyImages.size(); i++) {
			try {
				ModelImage cropKeyImage = cropKeyImages.get(i);

				int xDim = cropKeyImage.getExtents()[0];
				int yDim = cropKeyImage.getExtents()[1];
				int zDim = cropKeyImage.getExtents()[2];
				int size = xDim * yDim;

				int[] newExtents = new int[2];
				newExtents[0] = xDim;
				newExtents[1] = yDim;

				Vector<ModelImage> ceImageVector = new Vector<ModelImage>();
				for (int j = 0; j < zDim; j++) {

					try {

						ModelImage targetImageSlice = new ModelImage(
								ModelStorageBase.FLOAT, newExtents, "target"
										+ j);
						float[] targetBuffer = new float[size];
						cropKeyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						ModelImage resultImage = (ModelImage) targetImageSlice
								.clone();

						AlgorithmCoherenceEnhancingDiffusion coherenceEnhancingDiffusionAlgo = new AlgorithmCoherenceEnhancingDiffusion(
								resultImage, targetImageSlice, numIterations,
								diffusitivityDenom, derivativeScale,
								gaussianScale, do25D, entireImage);

						coherenceEnhancingDiffusionAlgo.addListener(this);

						coherenceEnhancingDiffusionAlgo.run();

						coherenceEnhancingDiffusionAlgo.setCompleted(true);

						// new ViewJFrameImage(resultImage);

						ceImageVector.add(resultImage);

						coherenceEnhancingDiffusionAlgo.finalize();
						coherenceEnhancingDiffusionAlgo = null;
						targetBuffer = null;

					} catch (IOException e) {

					}
				}

				cropKeyImagesCE.add(ceImageVector);
				// new ViewJFrameImage(coherenceEnhancingDiffusionImage);

			} catch (OutOfMemoryError x) {
				MipavUtil
						.displayError("Dialog GaborFilter: unable to allocate enough memory");
			}
		}

	}

	/**
	 * Crop the central prostate gland region from the 512x512 slices.
	 */
	public void cropKeyImages() {

		int zDim;

		// Crop key images. VOIs
		for (int i = 0; i < keyImages.size(); i++) {

			ModelImage image = keyImages.get(i);

			int[] extents = (int[]) image.getFileInfo(0).getExtents();
			zDim = extents[2] - 1;

			// manually set the crop image starting point and ending point

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
				if (image.getNDims() == 3) {

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
					return;
				}

				System.err.println("destExtents[0] = " + destExtents[0]
						+ "  destExtents[1] = " + destExtents[1]);

				// create crop images
				cropKeyImages.add(i,
						new ModelImage(image.getType(), destExtents,
								makeImageName(image.getImageName(), "_crop")));

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

				cropAlgo = new AlgorithmAddMargins(image, cropKeyImages.get(i),
						xCrop, yCrop, zCrop);

				cropAlgo.addListener(this);

				// Hide the dialog since the algorithm is about to run.
				setVisible(false);

				cropAlgo.run();

			} catch (OutOfMemoryError e) {
				MipavUtil
						.displayError("Dialog Crop: unable to allocate enough memory");

				return;
			}
		} // end for loop
	}

	/**
	 * read 2D slices directory.
	 */
	public void readFiles() {
		int index;

		try {
			// read key images and VOIs
			for (int i = 0; i < keyImageVector.size(); i++) {

				// read key image
				String dir = keyImageVector.get(i);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				System.err.println("Key Image: fileName = " + fileName
						+ "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(i, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector.get(i);
				System.err.println("voiDir = " + voiDir);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1,
						voiDir.length()));

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory,
						keyImages.get(i));
				System.err.println("fileDirectory = " + directory
						+ " fileName = " + fileName);
				System.err.println("voiDirectory = " + voiDirectory
						+ "  voiFileName = " + voiFileName);
				keyImageVOIs.add(i, fileVOI.readVOI(false));

				keyImages.get(i).registerVOI(keyImageVOIs.get(i)[0]);

			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Extract CE features.
	 */
	public void extractFeatures() {

		String name = new String();

		int newExtents[];

		int length = cropKeyImagesCE.size();

		for (int i = 0; i < length; i++) {
			synchronized (this) {
				Vector<ModelImage> keyImageVector = cropKeyImagesCE.get(i);

				int keyImageVectorSize = keyImageVector.size();

				ModelImage srcimage = keyImages.get(i);

				for (int j = 0; j < keyImageVectorSize; j++) {

					ModelImage keyImage = keyImageVector.get(j);

					try {
						ModelImage resultImage;
						ModelImage classificationImage;
						name = makeImageName(keyImage.getImageName(),
								"_Original");
						newExtents = new int[3];
						newExtents[0] = keyImage.getExtents()[0];
						newExtents[1] = keyImage.getExtents()[1];
						newExtents[2] = numberFeatures;

						resultImage = new ModelImage(ModelStorageBase.FLOAT,
								newExtents, name);
						classificationImage = new ModelImage(
								ModelStorageBase.FLOAT, newExtents, name);

						textureAlgo = new AlgorithmProstateFeaturesSaveAutoTrain(
								resultImage, classificationImage, keyImage,
								imageIntensityFilter,
								coherenceEnhancingDiffusionFilter,
								regisotropicDiffusionFilter,
								IHN3CorrectionFilter, modeFilter, meanFilter,
								medianFilter, invertFilter, haralickFilter,
								gaborFilter, hurstFilter, waveletFilter,
								gaussianFilter, gmFilter, numberFeatures);

						textureAlgo.addListener(this);

						textureAlgo.run();

						saveFeatureSpaceValue(resultImage, classificationImage,
								keyImage, srcimage, j);

						resultImage.disposeLocal();
						resultImage = null;
						classificationImage.disposeLocal();
						classificationImage = null;
						textureAlgo.setCompleted(true);
						textureAlgo = null;

						newExtents = null;

					} catch (OutOfMemoryError x) {

						System.gc();
						MipavUtil
								.displayError("Dialog Haralick Texture: unable to allocate enough memory");

						return;
					} // end try

				}

			} // end synchronized
		}// end i loop
	}

	/**
	 * Save CE features to user specified directory.
	 * 
	 * @param resultImage
	 *            result image from feature extraction algorithm.
	 * @param classificationImage
	 *            corresponding class +1 or -1.
	 * @param keyImage
	 *            cropped key image
	 * @param srcImage
	 *            corresponding src image
	 * @param sliceNumber
	 *            slice number
	 * @return Feature vectors for the 2D slices.
	 */
	public Features[] saveFeatureSpaceValue(ModelImage resultImage,
			ModelImage classificationImage, ModelImage keyImage,
			ModelImage srcImage, int sliceNumber) {
		int xDim = keyImage.getExtents()[0];
		int yDim = keyImage.getExtents()[1];
		int sliceSize = xDim * yDim;
		int i, j, z, k;
		int classNumber;
		int index = 0;
		float value;
		int zDim;

		int numImages = numberFeatures;

		float[] resultBuffer = new float[sliceSize];
		float[] resultBufferClass = new float[sliceSize];

		if (keyImage.getNDims() == 2) {
			zDim = 1;
		} else {
			zDim = keyImage.getExtents()[2];
		}

		Features[] featureArray = new Features[zDim];
		for (i = 0; i < zDim; i++) {
			featureArray[i] = new Features();
		}

		Vector[][] features = new Vector[zDim][numImages];

		for (i = 0; i < zDim; i++) {
			for (j = 0; j < numImages; j++) {
				features[i][j] = new Vector();
			}
		}

		for (z = 0; z < zDim; z++) {

			for (i = 0; i < numImages; i++) {

				try {

					resultImage.exportData((z * numberFeatures * sliceSize) + i
							* sliceSize, sliceSize, resultBuffer);
					classificationImage.exportData(
							(z * numberFeatures * sliceSize) + i * sliceSize,
							sliceSize, resultBufferClass);

					for (k = 0; k < sliceSize; k++) {
						classNumber = (int) resultBufferClass[k];
						value = resultBuffer[k];
						index = k;

						features[z][i].add(new Feature(classNumber, index,
								value));

					}

				} catch (IOException error) {
					MipavUtil
							.displayError("Temp: JDialogProstateSaveFeatures: IOException on destImage["
									+ i
									+ "].exportData(0,resultBuffer["
									+ i
									+ "],false)");

					return null;
				}
			} // for (i = 0; i < resultNumber; i++)
		}

		String imgDir = srcImage.getImageDirectory();
		String featureDir = imgDir + File.separator + featureDirName;
		new File(featureDir).mkdir();

		String name = srcImage.getImageName();
		if (name.contains("ax") || name.contains("Axial")) {
			featureDir += File.separator + "axial";
		} else if (name.contains("cor") || name.contains("Coronal")) {
			featureDir += File.separator + "coronal";
		} else if (name.contains("sag") || name.contains("Sagittal")) {
			featureDir += File.separator + "sagittal";
		}

		File dir = new File(featureDir);
		if (!dir.isDirectory()) {
			dir.mkdir();
		}

		// Save feature space into a file
		try {
			for (z = 0; z < zDim; z++) {
				Feature feature;
				int size;
				size = features[z][0].size();

				String inputFileName = featureDir + File.separator + "slice"
						+ sliceNumber + ".in";
				File file = new File(inputFileName);
				PrintWriter output = new PrintWriter(file);

				boolean printClassify = false;
				for (j = 0; j < size; j++) {
					printClassify = false;

					// add x feature per line
					FeatureNode[] x = new FeatureNode[features[z].length];

					for (i = 0; i < features[z].length; i++) {

						feature = (Feature) features[z][i].get(j);
						classNumber = feature.classify;
						value = feature.value;
						index = feature.index;

						if (printClassify == false) {
							// if (testSample == true) {
							// output.print(classNumber);
							// } else {
							if (svmTrainingBinary) {
								if (classNumber == 1) {
									featureArray[z].classAdd(+1);
									output.print("+1");
								} else {
									featureArray[z].classAdd(-1);
									output.print("-1");
								}
							} else {
								if (classNumber == 1) {
									featureArray[z].classAdd(1);
									output.print("1");
								} else {
									featureArray[z].classAdd(2);
									output.print("2");
									// output.print(classify);
								}
							}
							// }
							printClassify = true;
						}

						if (svmTrainingBinary) {
							x[i] = new FeatureNode((i + 1), value);
							output.print(" " + (i + 1) + ":" + value);
						} else {
							x[i] = new FeatureNode((i + 1), value);
							output.print(" " + (i + 1) + ":" + value);
						}

					} // end i = 0; i < features[z].length
					output.println();
					featureArray[z].featureAdd(x);
				} // end j = 0; j < size;
				output.close();
			} // end z = 0; z < zDim;
		} catch (Exception e) {
			e.printStackTrace();
		}
		features = null;

		return featureArray;
	}

	/**
	 * Dialog GUI initialization.
	 */
	public void init() {

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		buildKeyImagePanel();

		mainPanel.add(imageSelectionPanel, BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}

	/**
	 * Build the panel.
	 */
	public void buildKeyImagePanel() {

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		imageSelectionPanel = new JPanel();
		imageSelectionPanel.setLayout(new GridLayout(3, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Auto Train"));

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
		labelKeyImage = new JLabel("Key Image Directory: ");
		labelKeyImage.setFont(serif12);
		labelKeyImage.setForeground(Color.black);

		imageSelectionPanel.add(labelKeyImage, gbc);

		textFieldKeyImage = new JTextField(20);
		textFieldKeyImage.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldKeyImage, gbc);

		buttonKeyImage = new JButton("Choose");
		buttonKeyImage.addActionListener(this);
		buttonKeyImage.setActionCommand("ChooseKeyImageDir");
		buttonKeyImage.setFont(serif12B);
		buttonKeyImage.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonKeyImage, gbc);

		// Save image directory
		gbc.gridx = 0;
		gbc.gridy = 2;
		labelSaveImage = new JLabel("Saved Image Directory: ");
		labelSaveImage.setFont(serif12);
		labelSaveImage.setForeground(Color.black);

		imageSelectionPanel.add(labelSaveImage, gbc);

		textFieldSaveImage = new JTextField(20);
		textFieldSaveImage.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldSaveImage, gbc);

		buttonSaveImage = new JButton("Choose");
		buttonSaveImage.addActionListener(this);
		buttonSaveImage.setActionCommand("ChooseSaveImageDir");
		buttonSaveImage.setFont(serif12B);
		buttonSaveImage.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonSaveImage, gbc);

	}

}