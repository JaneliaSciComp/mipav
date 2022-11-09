package gov.nih.mipav.view.renderer.WildMagic.Knees;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ImageReorientation;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import ar.com.hjg.pngj.*;

/**
 * This class converts the original knees MRI images into isotropic images. 
 * 
 * 1) Convert the VOIs into binary image masks. 
 * 2) Convert the original images into isotropic images
 * 3) Convert the isotropic images from sagittal to axial; sagittal to coronal. 
 * 4) Along each orietation, generate the CED image in addition to MRI. 
 * 5) Save all the images (isotropic axial, sagittal, coronal images with corresponding binary masks). 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogKnees_90_data_train_extraction extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/**
	 * bounding box for crop the image. Currently set from 0 to 512, the orginal
	 * image slice size.
	 */
	private int boxYmin, boxYmax;
	private int boxXmin, boxXmax;

	/** X cropped region bounds. */
	private int[] xBounds = new int[2];

	/** Y cropped region bounds. */
	private int[] yBounds = new int[2];

	/** Z cropped region bound. */
	private int[] zBounds = new int[2];

	/** crop margin algorithm. */
	private AlgorithmAddMargins cropAlgo;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** image vector to hold the actual images. */
	private Hashtable<String, ModelImage> keyImages = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> maskImages = new Hashtable<String, ModelImage>();

	private Vector<ModelImage> keyImagesCrop = new Vector<ModelImage>();
	private Vector<ModelImage> keyImagesScaleIntensity = new Vector<ModelImage>();
	
	/** voi vector to hold the actual vois. */
	private Vector<ModelImage> keyImageVOIs = new Vector<ModelImage>();

	/** cropped key image vector. */
	private Vector<ModelImage> cropKeyImages = new Vector<ModelImage>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	private AlgorithmTransform algoTrans;

	private Hashtable<String, ModelImage> axialList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> axialMaskList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> sagittalList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> sagittalMaskList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> coronalList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> coronalMaskList = new Hashtable<String, ModelImage>();

	private int axial_index = 0;
	private int sagittal_index = 0;
	private int coronal_index = 0;

	Hashtable<String, ModelImage> cedTableAxial = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTableSagittal = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTableCoronal = new Hashtable<String, ModelImage>();

	Hashtable<String, String> origTable = new Hashtable<String, String>();
	Hashtable<String, String> nameTable = new Hashtable<String, String>();
	Hashtable<String, String> nameVOITable = new Hashtable<String, String>();

	Hashtable<String, ModelImage> keyImagesTransform = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> keyImageMasksTransform = new Hashtable<String, ModelImage>();
	
	/**
	 * List of patients need work.
	 * 
	 * 2966L 3225L 6205L 6909R 9717L 9808R 3022L 8868R 5733L 8121L 8136L 9808L
	 * 7654R (wrong fem VOI, it's FAT GRE VOI) 2911L (miss patella) 5271R (miss
	 * femur) 4983L (miss femur) 5026L (miss femur,need work) 9808R (miss femur)
	 * 1959L (femur VOI wrong)
	 */

	

	private Vector<String> noCasesList = new Vector<String>();

	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogKnees_90_data_train_extraction(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();

		noCasesList.add("2966L");
		noCasesList.add("3225L");
		noCasesList.add("6205L");
		noCasesList.add("6909R");
		noCasesList.add("9717L");
		noCasesList.add("9808R");
		noCasesList.add("3022L");
		noCasesList.add("8868R");
		noCasesList.add("5733L");
		noCasesList.add("8121L");
		noCasesList.add("8136L");
		noCasesList.add("9808L");
		noCasesList.add("7654R");
		noCasesList.add("2911L");
		noCasesList.add("5271R");
		noCasesList.add("4983L");
		noCasesList.add("5026L");
		noCasesList.add("9808R");
		noCasesList.add("1959L");

		setVisible(true);

	}

	/**
	 * dispose memory
	 * */
	public void disposeLocal() {
		
		int i;

		for (i = 0; i < keyImages.size(); i++) {
			ModelImage temp = keyImages.get(i);
			temp.disposeLocal();
		}
		
	}

	/**
	 * Dialog local actionPerformed handler.
	 */
	public void actionPerformed(ActionEvent event) {

		String command = event.getActionCommand();
		if (command.equals("OK")) {
			callAlgorithm();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			
		} else if (command.equals("ChooseKeyImageDir")) {
			readKeyImageDir();
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	/**
	 * Let user specify the saved 2D slices atlas, record the save directory.
	 */
	private void recordSaveImageDir() {
		
		String saveImageName;
		saveImageChooser.setDialogTitle("Open Saved Images Directory");
		saveImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = saveImageChooser.showOpenDialog(UI.getMainFrame());

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

	/**
	 * Read 3D images atlas directory.
	 */
	private void readKeyImageDir() {

		// File fileDir = new File("/home/ruida/kneesBackup/DP_clean");
		File fileDir = new File("/home/ruida/kneesBackup/patella");
		// traverse_folder_femur(fileDir, null);
		traverse_folder_patella(fileDir, null);
		
	}

	private void traverse_folder_femur(File dir, String hashID) {

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_name(new File(dir, children[i]));

			}
		}

	}

	private void read_name(File dir) {

		String dirName = dir.toString();

		String lowerCaseName;
		String fileName;
		String hashID;

		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		lowerCaseName = dirName.toLowerCase();
		fileName = lowerCaseName.substring(begin, end);
		begin = fileName.lastIndexOf("_") + 1;
		end = fileName.lastIndexOf(".");

		hashID = fileName.substring(begin, end);

		// read GRE image file
		if (fileName.startsWith("image") && fileName.endsWith("xml")) {
			System.err.println(hashID + " => " + dirName);
			nameTable.put(hashID, dirName);
		}

		// read fem VOI
		if (fileName.startsWith("voi") && fileName.endsWith("xml")) {
			System.err.println(hashID + " => " + dirName);
			nameVOITable.put(hashID, dirName);
		}

	}

	private void traverse_folder_patella(File dir, String hashID) {

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_name_patella(new File(dir, children[i]));

			}
		}

	}
	
	private void read_name_patella(File dir) {

		String dirName = dir.toString();

		String lowerCaseName;
		String fileName;
		String hashID;
		String subString;

		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		lowerCaseName = dirName.toLowerCase();
		fileName = lowerCaseName.substring(begin, end);

		// read GRE image file
		if (fileName.startsWith("image") && fileName.endsWith("xml")) {
			
			end = fileName.lastIndexOf("_");
			subString = fileName.substring(0, end);
			
			begin = subString.lastIndexOf("_") + 1;
			end = subString.length();
			hashID = fileName.substring(begin, end);
			
			System.err.println(hashID + " => " + dirName);
			nameTable.put(hashID, dirName);
		}

		// read fem VOI
		if (fileName.startsWith("voi") && fileName.endsWith("xml")) {
			
			begin = fileName.lastIndexOf("_") + 1;
			end = fileName.lastIndexOf(".");
			hashID = fileName.substring(begin, end);
			
			System.err.println(hashID + " => " + dirName);
			nameVOITable.put(hashID, dirName);
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

	}

	/**
	 * Driver function to read image and VOIs, and convert each 3D image to 2D
	 * slices.
	 */
	public void callAlgorithm() {

		long startTime = System.currentTimeMillis();

		loadFiles();

		convertVOItoMask();

		transform();
		
		conversion();

		System.err.println("saveImage");

		runCED();

		saveOrthogonalCEDImage(); 

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		// System.gc();

	}

	
	
	public void transform() {

		Set<String> keys = maskImages.keySet();
		
		System.err.println("Transform");
		
		for (String key : keys) {
			
			    System.err.println("Transform key = " + key);
				
				ModelImage keyImage = keyImages.get(key);
				ModelImage keyImageMask = maskImages.get(key);
				keyImage.getVOIs().removeAllElements();
				
				ModelImage transKeyImage = calculateTransform(keyImage);
				keyImagesTransform.put(key, transKeyImage);
				
				ModelImage transKeyImageMask = calculateTransform(keyImageMask);
				keyImageMasksTransform.put(key, transKeyImageMask);
				

			
		}
	

	}
	
	public void convertVOItoMask() {

		Set<String> keys = keyImages.keySet();
		
		int count = 0;
		
		System.err.println("convertVOItoMask");
		
		int startIndex = 0;
		int endIndex = 10;
		
		for (String key : keys) {

			if (count >= startIndex && count <= endIndex) {

				ModelImage targetImageSlice = keyImages.get(key);

				System.err.println("key = " + key);

				new ViewJFrameImage(targetImageSlice);
				/*
				ModelImage maskImage = null;
				maskImage = targetImageSlice.generateBinaryImage();
				maskImage.getMatrixHolder().replaceMatrices(targetImageSlice.getMatrixHolder().getMatrices());
				maskImage.getFileInfo(0).setOrigin(targetImageSlice.getFileInfo(0).getOrigin());
				
				maskImages.put(key, maskImage);
				*/ 
			}
			
			count++;
			// break;
		}

		System.err.println("count = " + count);
	}

	public void saveOrthogonalCEDImage() {

		Set<String> keys = keyImagesTransform.keySet();

		System.err.println("saveOrthogonalCEDImage");

		String sliceDir = saveImageDirectory;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();

		for (String key : keys) {
			// read key image

			
			System.err.println("key = " + key);
			
				ModelImage axialImage = axialList.get(key);
				ModelImage cedAxialImage = cedTableAxial.get(key);
				ModelImage axialMaskImage = axialMaskList.get(key);
				
				String axialDir = sliceDir + File.separator + "axial" + File.separator;
				String imgName = "image_ced_" + key + ".xml";
				cedAxialImage.saveImage(axialDir, imgName, FileUtility.XML, false);
				imgName = "image_" + key + ".xml";
				axialImage.saveImage(axialDir, imgName, FileUtility.XML, false);
				imgName = "image_mask_" + key + ".xml";
				axialMaskImage.saveImage(axialDir, imgName, FileUtility.XML, false);
				
				ModelImage coronalImage = coronalList.get(key);
				ModelImage cedCoronalImage = cedTableCoronal.get(key);
				ModelImage coronalMaskImage = coronalMaskList.get(key);
				String coronalDir = sliceDir + File.separator + "coronal" 	+ File.separator;
				imgName = "image_ced_" + key + ".xml";
				cedCoronalImage.saveImage(coronalDir, imgName, FileUtility.XML, false);
				imgName = "image_" + key + ".xml";
				coronalImage.saveImage(coronalDir, imgName, FileUtility.XML, false);
				imgName = "image_mask_" + key + ".xml";
				coronalMaskImage.saveImage(coronalDir, imgName, FileUtility.XML, false);

				ModelImage sagittalImage = sagittalList.get(key);
				ModelImage cedSagittalImage = cedTableSagittal.get(key);
				ModelImage sagittalMaskImage = sagittalMaskList.get(key);
				String sagittalDir = sliceDir + File.separator + "sagittal" + File.separator;
				imgName = "image_ced_" + key + ".xml";
				cedSagittalImage.saveImage(sagittalDir, imgName, FileUtility.XML, false);
				imgName = "image_" + key + ".xml";
				sagittalImage.saveImage(sagittalDir, imgName, FileUtility.XML, false);
				imgName = "image_mask_" + key + ".xml";
				sagittalMaskImage.saveImage(sagittalDir, imgName, FileUtility.XML, false);

				
		}
		

	}

	

	public void runCED() {

		Set<String> keys = keyImagesTransform.keySet();
		
		System.err.println("runCED");
		
		for (String key : keys) {
			
				System.err.println("key = " + key);
				
				ModelImage axialImage = axialList.get(key);
				ModelImage cedAxial = calculateCoherenceEnhancingDiffusion(axialImage);
				cedTableAxial.put(key, cedAxial);

				ModelImage sagittalImage = sagittalList.get(key);
				ModelImage cedSagittal = calculateCoherenceEnhancingDiffusion(sagittalImage);
				cedTableSagittal.put(key, cedSagittal);

				ModelImage coronalImage = coronalList.get(key);
				ModelImage cedCoronal = calculateCoherenceEnhancingDiffusion(coronalImage);
				cedTableCoronal.put(key, cedCoronal);

		}

	}

	public void saveHED2DsliceCED() {

		Set<String> keys = keyImages.keySet();

		for (String key : keys) {

			ModelImage axialImage = axialList.get(key);
			ModelImage axialImageMask = axialMaskList.get(key);
			saveImage(axialImage, axialImageMask, "axial");

			ModelImage sagittalImage = sagittalList.get(key);
			ModelImage sagittalImageMask = sagittalMaskList.get(key);
			saveImage(sagittalImage, sagittalImageMask, "sagittal");

			ModelImage coronalImage = coronalList.get(key);
			ModelImage coronalImageMask = coronalMaskList.get(key);
			saveImage(coronalImage, coronalImageMask, "coronal");

		}

		keys = keyImages.keySet();

		for (String key : keys) {

			ModelImage axialImage = cedTableAxial.get(key);
			ModelImage axialImageMask = axialMaskList.get(key);
			saveImage(axialImage, axialImageMask, "axial");

			ModelImage sagittalImage = cedTableSagittal.get(key);
			ModelImage sagittalImageMask = sagittalMaskList.get(key);
			saveImage(sagittalImage, sagittalImageMask, "sagittal");

			ModelImage coronalImage = cedTableCoronal.get(key);
			ModelImage coronalImageMask = coronalMaskList.get(key);
			saveImage(coronalImage, coronalImageMask, "coronal");
		}

	}

	public void saveImage(ModelImage image, ModelImage imageMask, String orientation) {

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		float[] res = image.getResolutions(0);
		float x_res = res[0];
		float y_res = res[1];

		float[] newRes = new float[2];
		newRes[0] = x_res;
		newRes[1] = y_res;

		int size_3D = xDim * yDim * zDim;

		int[] newExtents = new int[2];
		newExtents[0] = xDim;
		newExtents[1] = yDim;

		int size = xDim * yDim;

		float[] imageBuffer = new float[size_3D];

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);
		} catch (Exception e) {
			e.printStackTrace();
		}

		float percentile_left = 0.01f;
		float percentile_right = 0.01f;

		int minIndex = (int) (size_3D * percentile_left);
		int maxIndex = (int) (size_3D * (1.0 - percentile_right));

		Arrays.sort(imageBuffer);

		float min = imageBuffer[minIndex];
		float max = imageBuffer[maxIndex];

		imageBuffer = null;

		for (int j = 0; j < zDim; j++) {

			try {

				ModelImage imageSlice = new ModelImage(ModelStorageBase.FLOAT,
						newExtents, "image" + j);
				imageSlice.getFileInfo(0).setResolutions(newRes);
				imageSlice.setResolutions(newRes);
				float[] imgBuffer = new float[size];
				image.exportData(j * size, size, imgBuffer);
				imageSlice.importData(0, imgBuffer, true);

				// new ViewJFrameImage(imageSlice);

				ModelImage maskSlice = new ModelImage(ModelStorageBase.SHORT,
						newExtents, "mask" + j);
				maskSlice.setResolutions(newRes);
				maskSlice.getFileInfo(0).setResolutions(newRes);
				short[] maskBuffer = new short[size];
				imageMask.exportData(j * size, size, maskBuffer);
				maskSlice.importData(0, maskBuffer, true);

				// new ViewJFrameImage(maskSlice);

				String sliceDir = saveImageDirectory + orientation
						+ File.separator;
				File sliceDirFile = new File(sliceDir);
				if (!sliceDirFile.isDirectory())
					sliceDirFile.mkdir();

				String imgName = null;
				String maskName = null;

				if (orientation.equals("axial")) {
					imgName = "image_" + axial_index + ".png";
					maskName = "voi_" + axial_index + ".png";
				} else if (orientation.equals("coronal")) {
					imgName = "image_" + coronal_index + ".png";
					maskName = "voi_" + coronal_index + ".png";
				} else if (orientation.equals("sagittal")) {
					imgName = "image_" + sagittal_index + ".png";
					maskName = "voi_" + sagittal_index + ".png";
				}

				if (imgName != null && maskName != null) {

					savePNGfile(sliceDir, imgName, imageSlice, min, max, xDim,
							yDim, false);
					savePNGfile(sliceDir, maskName, maskSlice, min, max, xDim,
							yDim, true);
					System.err.println("sliceDir = " + sliceDir);
					System.err.println("imgName = " + imgName);
					
					if (orientation.equals("axial")) {
						axial_index++;
					} else if (orientation.equals("coronal")) {
						coronal_index++;
					} else if (orientation.equals("sagittal")) {
						sagittal_index++;
					}
				}

				imgBuffer = null;
				imageSlice.disposeLocal();
				imageSlice = null;

				maskBuffer = null;
				maskSlice.disposeLocal();
				maskSlice = null;

			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		newExtents = null;
	}

	private ModelImage calculateCoherenceEnhancingDiffusion(ModelImage inImage) {

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
		ModelImage coherenceEnhancingDiffusionImage;

		try {
			coherenceEnhancingDiffusionImage = (ModelImage) inImage.clone();

			int type = coherenceEnhancingDiffusionImage.getType();
			coherenceEnhancingDiffusionImage.setType(ModelStorageBase.FLOAT);
			coherenceEnhancingDiffusionImage.reallocate(ModelStorageBase.FLOAT);

			AlgorithmCoherenceEnhancingDiffusion coherenceEnhancingDiffusionAlgo = new AlgorithmCoherenceEnhancingDiffusion(
					coherenceEnhancingDiffusionImage, inImage, numIterations,
					diffusitivityDenom, derivativeScale, gaussianScale, do25D,
					entireImage);

			coherenceEnhancingDiffusionAlgo.addListener(this);

			coherenceEnhancingDiffusionAlgo.run();

			coherenceEnhancingDiffusionAlgo.setCompleted(true);
			coherenceEnhancingDiffusionAlgo.finalize();
			coherenceEnhancingDiffusionAlgo = null;

		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog GaborFilter: unable to allocate enough memory");

			return null;
		}

		return coherenceEnhancingDiffusionImage;
	}

	public void conversion() {

		Set<String> keys = keyImagesTransform.keySet();
		
		System.err.println("conversion");
		
		for (String key : keys) {
				
			    System.err.println("key = " + key);

				ModelImage keyImage = keyImagesTransform.get(key);
				ModelImage keyImageMask = keyImageMasksTransform.get(key);
				
				keyImage.getVOIs().removeAllElements();
				
				// axial orientation
				ImageReorientation axial_orient = new ImageReorientation(keyImage, ImageReorientation.AXIAL_INDEX);
				axial_orient.preformOrientation();
				axialList.put(key, axial_orient.getResultImage());
				
//				JDialogReorient axial_orient = new JDialogReorient(keyImage);
//				axial_orient.setVisible(false);
//				axial_orient.set_axial_orientation();
//				axial_orient.doRun();
//				axialList.put(key, axial_orient.getResultImage());
			
				ImageReorientation axial_orient_mask = new ImageReorientation(keyImageMask, ImageReorientation.AXIAL_INDEX);
				axial_orient_mask.preformOrientation();
				axialMaskList.put(key, axial_orient_mask.getResultImage());
				
//              JDialogReorient axial_orient_mask = new JDialogReorient(keyImageMask);
//				axial_orient_mask.setVisible(false);
//				axial_orient_mask.set_axial_orientation();
//				axial_orient_mask.doRun();
//				axialMaskList.put(key, axial_orient_mask.getResultImage());
				
				// sagittal orientation
				sagittalList.put(key, keyImage);
				sagittalMaskList.put(key, keyImageMask);
			
				// coronal orientation
				ImageReorientation coronal_orient = new ImageReorientation(keyImage, ImageReorientation.CORONAL_INDEX);
				coronal_orient.preformOrientation();
				coronalList.put(key, coronal_orient.getResultImage());
				
//				JDialogReorient coronal_orient = new JDialogReorient(keyImage);
//				coronal_orient.setVisible(false);
//				coronal_orient.set_coronal_orientation();
//				coronal_orient.doRun();
//				coronalList.put(key, coronal_orient.getResultImage());
				
				ImageReorientation coronal_orient_mask = new ImageReorientation(keyImageMask, ImageReorientation.CORONAL_INDEX);
				coronal_orient_mask.preformOrientation();
				coronalMaskList.put(key, coronal_orient_mask.getResultImage());
			
//				JDialogReorient coronal_orient_mask = new JDialogReorient(keyImageMask);
//				coronal_orient_mask.setVisible(false);
//				coronal_orient_mask.set_coronal_orientation();
//				coronal_orient_mask.doRun();
//				coronalMaskList.put(key, coronal_orient_mask.getResultImage());
			
		}


	}

	public ModelImage scaleIntensity(ModelImage image) {

		int[] destExtents = new int[3];

		destExtents[0] = image.getExtents()[0];
		destExtents[1] = image.getExtents()[1];
		destExtents[2] = image.getExtents()[2];
		
		ModelImage resultImage = (ModelImage) image.clone();

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		int size_3D = xDim * yDim * zDim;
		float[] imageBuffer = new float[size_3D];
		float[] origBuffer = new float[size_3D];

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);
			image.exportData(0, imageBuffer.length, origBuffer);
		} catch (Exception e) {
			e.printStackTrace();
		}

		float percentile_10 = 0.05f;
		float percentile_75 = 0.75f;
		float percentile_25 = 0.25f;
		float percentile_50 = 0.50f;
		float percentile_90 = 0.95f;

		int index_50 = (int) (size_3D * percentile_50);
		int index_75 = (int) (size_3D * percentile_75);
		int index_25 = (int) (size_3D * percentile_25);
		int index_10 = (int) (size_3D * percentile_10);
		int index_90 = (int) (size_3D * percentile_90);

		Arrays.sort(imageBuffer);

		float inten_75 = imageBuffer[index_75];
		float inten_25 = imageBuffer[index_25];
		float median = imageBuffer[index_50];
		float min = imageBuffer[index_10];
		float max = imageBuffer[index_90];

		float contrast = median + 2f * (inten_75 - inten_25);

		for (int i = 0; i < size_3D; i++) {

			if (origBuffer[i] <= min) {
				origBuffer[i] = min;
			}

			if (origBuffer[i] >= max) {
				origBuffer[i] = max;
			}

			origBuffer[i] = 1000f * (origBuffer[i] / contrast);
		}

		try {
			resultImage.importData(0, origBuffer, true);
		} catch (IOException e) {
			e.printStackTrace();
		}

		return resultImage;
	}



	public ModelImage calculateTransform(ModelImage keyImage) {

		ModelImage resultImage;
		TransMatrix xfrm;
		int interp;
		float oXres, oYres, oZres, cXres, cYres, cZres;
		int oXdim, oYdim, oZdim, cXdim, cYdim, cZdim;
		int[] units;
		boolean doVOI, doClip, doPad, preserveFOV, doUpdateOrigin, doInvMat;
		boolean doRotateCenter;
		float fillValue = 0.0f;
		boolean isSATransform = false;
		Vector3f center = null;

		float[] dims = new float[3];
		float[] resols = new float[3];

		dims[0] = keyImage.getFileInfo()[0].getExtents()[0];
		dims[1] = keyImage.getFileInfo()[0].getExtents()[1];
		dims[2] = keyImage.getFileInfo()[0].getExtents()[2];
		resols[0] = keyImage.getFileInfo()[0].getResolutions()[0];
		resols[1] = keyImage.getFileInfo()[0].getResolutions()[1];
		resols[2] = keyImage.getFileInfo()[0].getResolutions()[2];

		doVOI = false;
		doClip = false;
		doPad = false;
		doRotateCenter = false;
		center = null;

		fillValue = 0.0f;
		doUpdateOrigin = true;
		isSATransform = false;

		interp = 0;
		xfrm = new TransMatrix(3);
		xfrm.identity();

		units = new int[3];
		units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

		float iXdim = dims[0];
		float iYdim = dims[1];
		float iZdim = dims[2];

		float iXres = resols[0];
		float iYres = resols[1];
		float iZres = resols[2];

		oXres = 0.5f;
		oYres = 0.5f;
		oZres = 0.5f;
	
		int constantFOV = 1;

		float fovX = iXres * (iXdim - constantFOV);
		float fovY = iYres * (iYdim - constantFOV);
		float fovZ = iZres * (iZdim - constantFOV);
		oXdim = Math.round(fovX / oXres) + constantFOV;
		oYdim = Math.round(fovY / oYres) + constantFOV;
		oZdim = Math.round(fovZ / oZres) + constantFOV;

		System.err.println("oXdim = " + oXdim + " oYdim = " + oYdim);

		algoTrans = new AlgorithmTransform(keyImage, xfrm, interp, oXres,
				oYres, oZres, oXdim, oYdim, oZdim, units, doVOI, doClip, doPad,
				doRotateCenter, center);
		algoTrans.setFillValue(fillValue);
		algoTrans.setUpdateOriginFlag(doUpdateOrigin);
		algoTrans.setUseScannerAnatomical(isSATransform);
		algoTrans.run();

		resultImage = algoTrans.getTransformedImage();
		resultImage.setImageOrientation(FileInfoBase.SAGITTAL);
		FileInfoBase fileInfo[] = resultImage.getFileInfo();
		
		for ( int i = 0; i < fileInfo.length; i++ ){
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 0);
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_S2I_TYPE, 1);
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_L2R_TYPE, 2);
		}
		resultImage.getMatrixHolder().replaceMatrices(keyImage.getMatrixHolder().getMatrices());
		resultImage.calcMinMax();

		algoTrans.disposeLocal();
		algoTrans = null;
		return resultImage;
	}
	
	/**
	 * Save the 2D slices and VOIs to user specified dir.
	 */
	public void saveImages() {

		int index = 0;
		for (int i = 0; i < keyImages.size(); i++) {
			try {

				ModelImage keyImage = keyImages.get(i);
				ModelImage keyImageVOI = keyImageVOIs.get(i);

				int xDim = keyImage.getExtents()[0];
				int yDim = keyImage.getExtents()[1];
				int zDim = keyImage.getExtents()[2];

				int size_3D = xDim * yDim * zDim;
				float[] imageBuffer = new float[size_3D];

				try {
					keyImage.exportData(0, imageBuffer.length, imageBuffer);
				} catch (Exception e) {
					e.printStackTrace();
				}

				float percentile_left = 0.1f;
				float percentile_right = 0.1f;

				int minIndex = (int) (size_3D * percentile_left);
				int maxIndex = (int) (size_3D * (1.0 - percentile_right));

				Arrays.sort(imageBuffer);

				float min = imageBuffer[minIndex];
				float max = imageBuffer[maxIndex];

				int size = xDim * yDim;

				int[] newExtents = new int[2];
				newExtents[0] = xDim;
				newExtents[1] = yDim;

				for (int j = 0; j <= zDim; j++) {

					try {

						System.err.println(" image number = " + (0 + i)
								+ "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(
								ModelStorageBase.FLOAT, newExtents, "target"
										+ j);
						float[] targetBuffer = new float[size];
						keyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// 1) save image
						String sliceDir = saveImageDirectory;
						File sliceDirFile = new File(sliceDir);
						if (!sliceDirFile.isDirectory())
							sliceDirFile.mkdir();

						System.err.println("index = " + index);

						String imgName = "image_" + index + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, min,
								max, xDim, yDim, false);

						// Save mask image
						ModelImage maskImage = new ModelImage(
								ModelStorageBase.INTEGER, newExtents, "voi" + j);
						int[] voiBuffer = new int[size];
						keyImageVOI.exportData(j * size, size, voiBuffer);
						maskImage.importData(0, voiBuffer, true);

						String maskName = "voi_" + index + ".png";
						savePNGfile(sliceDir, maskName, maskImage, 0, 1, xDim,
								yDim, true);

						targetBuffer = null;
						targetImageSlice.disposeLocal();
						targetImageSlice = null;

						maskImage.disposeLocal();
						maskImage = null;

						index++;

					} catch (IOException e) {

					}
				}


			} catch (OutOfMemoryError x) {
				MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			}
		}

	}

	public void saveImagesTest() {

		for (int i = 0; i < keyImages.size(); i++) {
			try {

				ModelImage keyImage = keyImages.get(i);

				int xDim = keyImage.getExtents()[0];
				int yDim = keyImage.getExtents()[1];
				int zDim = keyImage.getExtents()[2];

				int size_3D = xDim * yDim * zDim;
				float[] imageBuffer = new float[size_3D];

				try {
					keyImage.exportData(0, imageBuffer.length, imageBuffer);
				} catch (Exception e) {
					e.printStackTrace();
				}

				float percentile_left = 0.1f;
				float percentile_right = 0.1f;

				int minIndex = (int) (size_3D * percentile_left);
				int maxIndex = (int) (size_3D * (1.0 - percentile_right));

				Arrays.sort(imageBuffer);

				float min = imageBuffer[minIndex];
				float max = imageBuffer[maxIndex];

				int size = xDim * yDim;

				int[] newExtents = new int[2];
				newExtents[0] = xDim;
				newExtents[1] = yDim;

				for (int j = 0; j <= zDim; j++) {

					try {

						System.err.println(" image number = " + (0 + i)
								+ "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(
								ModelStorageBase.FLOAT, newExtents, "target"
										+ j);
						float[] targetBuffer = new float[size];
						keyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// 1) save image
						String sliceDir = saveImageDirectory + File.separator
								+ i + File.separator;
						// String sliceDir = saveImageDirectory;
						File sliceDirFile = new File(sliceDir);
						if (!sliceDirFile.isDirectory())
							sliceDirFile.mkdir();

						String imgName = "image_" + j + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, min,
								max, xDim, yDim, false);


					} catch (IOException e) {

					}
				}

			} catch (OutOfMemoryError x) {
				MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			}
		}

	}

	private void savePNGfile(String dirName, String fileName,
			ModelImage srcImage, float minIntensity, float maxIntensity,
			int xDim, int yDim, boolean isMask) {
		File file = null;
		boolean alpha = false;
		boolean gray = true;
		boolean indexed = false;
		try {

			ImageInfo imi = new ImageInfo(xDim, yDim, 8, alpha, gray, indexed);
			ImageLineByte line = new ImageLineByte(imi);
			int yMin = 0, yMax = yDim;
			int xMin = 0, xMax = xDim;
			int x, y;
			file = new File(dirName + File.separator + fileName);
			if (!file.exists()) {
				file.createNewFile();
			}

			// OutputStream os = new FileOutputStream(savedImageDir +
			// File.separator + imgName);
			OutputStream os = new FileOutputStream(file);

			PngWriter pngw = new PngWriter(os, imi);

			// System.err.println("xMin = " + xMin + "  xMax = " + xMax);

			for (int j = yMin; j < yMax; j++) {

				y = j - yMin;

				for (int i = xMin; i < xMax; i++) {

					x = i - xMin;

					if (isMask == false) {
						float intensity = srcImage.getFloat(i, j);
						float r = 0;
						if (intensity >= minIntensity
								&& intensity <= maxIntensity) {
							r = (float) ((intensity - minIntensity) * 255d / (maxIntensity - minIntensity));
						} else if (intensity > maxIntensity)
							r = 255;
						else if (intensity < minIntensity)
							r = 0;

						ImageLineHelper.setPixelGray8(line, x, (int) r);
					} else {

						short intensity = srcImage.getShort(i, j);

						if (intensity == 1) {
							// System.err.println("intensity = " + intensity);
							ImageLineHelper.setPixelGray8(line, x, (int) 255);
						} else {
							ImageLineHelper.setPixelGray8(line, x, (int) 0);
						}

					}
					// ImageLineHelper.setPixelGray8(line, x, (int) r);

				}
				// System.err.println();
				pngw.writeRow(line, y);
			}
			pngw.end();
			pngw.close();
			pngw = null;
			os.close();
			os = null;
			line = null;
			imi = null;
			// System.err.println("testing array");

		} catch (Exception e) {
			System.err.println("image find wrong : " + file.getAbsolutePath());
			e.printStackTrace();

			System.exit(0);
		}
	}

	/**
	 * load image files and voi files
	 */
	public void loadFiles() {
		readFile();
		System.err.println("finish image I/O");
	}


	public void readFile() {

		int index;
		int count = 0;
		try {
			// read key images and VOIs

			Set<String> keys = nameTable.keySet();

			for (String key : keys) {
				System.err.println("key = " + key);
				String dir = nameTable.get(key);
				if (dir == null)
					continue;
				
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					
					AlgorithmChangeType changeZ = new AlgorithmChangeType(image, ModelImage.FLOAT,
							image.getMin(), image.getMax(), image.getMin(), image.getMax(), false);
					changeZ.run();
					
					System.err.println("Key Image: fileName = " + fileName
							+ "  directory = " + directory);
					keyImages.put(key, image);
					keyImages.get(key).setImageName(key);
					
					changeZ.finalize();
					changeZ = null;
					
					count++;

				}
				keyImageIO = null;
			}
			System.err.println("image count = " + count);

			count = 0;
			Set<String> voiKeys = nameVOITable.keySet();
			VOI[] voi = null;
			for (String key : voiKeys) {
				System.err.println("key = " + key);
				String dir = nameVOITable.get(key);

				index = dir.lastIndexOf(File.separator);
				String voiName = dir.substring(index + 1, dir.length());
				String voiDirectory = dir.substring(0, index + 1);
				
				if (keyImages.get(key) != null) {
					FileVOI fileVOI = null;
					fileVOI = new FileVOI(voiName, voiDirectory,
							keyImages.get(key));
					voi = fileVOI.readVOI(false);

					if (voi[0] != null) {
						System.err.println("voiDirectory = " + voiDirectory
								+ "   " + "voiName = " + voiName);
						keyImages.get(key).registerVOI(voi[0]);
					}
					
					fileVOI = null;
					
					System.err.println("VOI count = " + count);
					count++;

				}

			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Initial panel
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
	 * Panel contains both the 3D image dir and saved 2D slices atlas dir.
	 */
	public void buildKeyImagePanel() {

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		imageSelectionPanel = new JPanel();
		imageSelectionPanel.setLayout(new GridLayout(2, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Auto Train"));

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