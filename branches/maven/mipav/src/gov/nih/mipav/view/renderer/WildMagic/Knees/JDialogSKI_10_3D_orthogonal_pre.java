package gov.nih.mipav.view.renderer.WildMagic.Knees;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmCoherenceEnhancingDiffusion;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import ar.com.hjg.pngj.*;


/*
 * This file presents the 3D-orthogonal approach to segment the MRI knees femur from SKI10 dataset.   And it is one ablation 
 * experiment we conducted for MRM paper.   
 * 
 * This file pre-processes SKI10 dataset with iso-tropic resampling (0.5 mm x 0.5 mm x 0.5 mm), converting SKI10 axial images
 * to coronal and sagittal images, generating Coherence Enhanced Diffusion (CED) filter images from corresponding MRI images. 
 * Then convert the 3D images to 2D png slices for training and testing. 10-fold cross-validation is used for validating the
 * experiments. 
 * 
 * !!!!!!!!!!!!   Strongly suggest that you use large memory to processing this step.  i.e. 128 GB memory !!!!!!!!!!!!!!!
 * 
 *  SKI 10 dataset is given actually in sagittal view, however, the image attribute of the SKI 10 dataset is axial.  We 
 *  have to bare with it.    
 *  
 *  This file creates the 10-fold cross-validation.  The file saves the corresponding png 2D slice files for each fold.  In order
 *  to distinguish the the training fold and testing fold.  We will use shell script to isolate them.  i.e. for fold 1, the rest
 *  folds from 2 to 10 compose the training fold for fold 1.   
 *  
 * @author  Ruida Cheng
 *       
 */
public class JDialogSKI_10_3D_orthogonal_pre extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	// The main user interface.
	private ViewUserInterface UI;

	// key image directory.
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;
	private JFileChooser readImageChooser = new JFileChooser();
	private String readImageDirectory;
	

	private JPanel imageSelectionPanel;


	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;
	
	Hashtable<String, String> nameTableImages = new Hashtable<String, String>();
	Hashtable<String, String> nameTableImagesCED = new Hashtable<String, String>();
	Hashtable<String, String> nameTableImagesMask = new Hashtable<String, String>();

	// During the preprocessing step, it occupies quite large amount of memory for pre-processing, 
	// i.e. CED filter.  In java environment, it is slow.  
	// One solution is to save the intermediate image into MIPAV .xml readable file format, such as
	// axial, sagittal, coronal MRI images, CED images and Ground Truth images. Then reload those images
	// and convert them into pnd 2D slices for traning.   
	private Hashtable<String, ModelImage> keyImagesOrientation = new Hashtable<String, ModelImage>();
	// private Hashtable<String, ModelImage> keyImagesOrientationCED = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> keyImagesOrientationMask = new Hashtable<String, ModelImage>();

	
	// large data processing occupy fair large amount of memory. 
	// 1) Specify which orientation to run. 
	// 2) set the corresponding index
	private int axial_index = 0;
	private int sagittal_index = 0;
	private int coronal_index = 0;
	
	// isotropic resampling algorithm
	private AlgorithmTransform algoTrans;
	
	// image table to hold the isotropic sampled images
	private Hashtable<String, ModelImage> keyImagesTransform = new Hashtable<String, ModelImage>();
	// image mask table to hold the isotropic sampled image masks. 
	private Hashtable<String, ModelImage> keyImageMasksTransform = new Hashtable<String, ModelImage>();
	
	
	// mask threshold table to hold the mask label from the ground truth.  
	// For the experiment, I only extract the femur part to compare with literature results. 
	private Hashtable<String, ModelImage> maskThresAxial = new Hashtable<String, ModelImage>();
	
	
	// axial, sagittal, coronal lists to hold the converted images from SKI10 axial view. 
	private Hashtable<String, ModelImage> axialList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> axialMaskList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> sagittalList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> sagittalMaskList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> coronalList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> coronalMaskList = new Hashtable<String, ModelImage>();
	
	
	// CED filgter table to hold the CED conversion result for axial, sagittal, and coronal images. 
	private Hashtable<String, ModelImage> cedTableAxial = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> cedTableSagittal = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> cedTableCoronal = new Hashtable<String, ModelImage>();

	private String saveImageDirectory_train;
	private String saveImageDirectory_test;
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogSKI_10_3D_orthogonal_pre(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();

		setVisible(true);

	}

	/**
	 * dispose memory
	 * */
	public void disposeLocal() {
	
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
	 * Let user specify the saved 2D slices, record the save directory.
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
	 * Read 3D images directory.
	 */
	private void readKeyImageDir() {
		
		
		String readImageName;
		readImageChooser.setDialogTitle("Open Key Images Directory");
		readImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = readImageChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			readImageName = readImageChooser.getSelectedFile().getName();

			readImageDirectory = String.valueOf(readImageChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ readImageName + File.separatorChar;
			textFieldKeyImage.setText(readImageDirectory);
			System.err.println("readImageDirectory = " + readImageDirectory);
			// File fileDir = new File("/scratch/Knee_2010_challenge/trainData");
			File fileDir = new File(readImageDirectory);
			traverse_folder(fileDir);
			
		} else {
			return;
		}
		
	}

	/**
	 * Traverse the folder
	 * @param dir
	 */
	private void traverse_folder(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_image_name(new File(dir, children[i]));
			}
		}

	}

	/**
	 * Read image full path for SKI10 dataset.  
	 * @param dir
	 */
	private void read_image_name(File dir) {

		String dirName = dir.toString();

		String lowerCaseName;
		String fileName;
		String hashID;

		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		lowerCaseName = dirName.toLowerCase();
		fileName = lowerCaseName.substring(begin, end);
		begin = fileName.lastIndexOf("-") + 1;
		end = fileName.lastIndexOf(".");

		hashID = fileName.substring(begin, end);

		if (fileName.startsWith("image") && fileName.endsWith("mhd")) {
			// System.err.println("hashID = " + hashID + "  dirName = " + dirName);
			nameTableImages.put(hashID, dirName);
		}

		if (fileName.startsWith("labels") && fileName.endsWith("mhd")) {
			// System.err.println("hashID = " + hashID + "  dirName = " + dirName);
			nameTableImagesMask.put(hashID, dirName);
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

		readFile();
		System.err.println("finish image I/O");

		threshold();
		
		transform();
		
		conversion();
		
		runCED();
		
		// saveOrthogonalCEDImage();
	
		System.err.println("saveImage");
       
		// save training 2D png slices
		save_MRI_CED_2Dslice();
		
		// save testing 2D png slices
		save_MRI_CED_2Dslice_test();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.err.println("sagittal_index = " + sagittal_index);
		
		// System.gc();

	}

	public void save_MRI_CED_2Dslice_test() {

		Set<String> keys = keyImagesTransform.keySet();

		int count = 0;
	

		System.err.println("saveOrthogonalCEDImage testing");

		String sliceDir = saveImageDirectory;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		saveImageDirectory_test = saveImageDirectory + File.separator + "test" + File.separator;
		sliceDir = saveImageDirectory_test;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		for (String key : keys) {

			System.err.println("save HED slice key = " + key);
			axial_index = 0;
			coronal_index = 0;
			sagittal_index = 0;
			
			//*********************   axial orientation  ***************************** 
			ModelImage axialImage = axialList.get(key);
			ModelImage scaledAxialImage = scaleIntensity(axialImage);

			
			if (count >= 0 && count <= 10) {	
				saveImageSlice_test(scaledAxialImage, "axial", "fold1", key);
			} else if (count >= 11 && count <= 20 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold2", key);	
			} else if (count >= 21 && count <= 30 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold3", key);	
			} else if (count >= 31 && count <= 40 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold4", key);	
			} else if (count >= 41 && count <= 50 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold5", key);	
			} else if (count >= 51 && count <= 60 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold6", key);	
			} else if (count >= 61 && count <= 70 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold7", key);	
			} else if (count >= 71 && count <= 80 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold8", key);	
			} else if (count >= 81 && count <= 90 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold9", key);	
			} else if (count >= 91 ) {
				saveImageSlice_test(scaledAxialImage, "axial", "fold10", key);	
			}
			
			
			ModelImage axialImageCED = cedTableAxial.get(key);
			ModelImage scaledAxialImageCED = scaleIntensity(axialImageCED);

			if (count >= 0 && count <= 10) {	
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold1", key);
			} else if (count >= 11 && count <= 20 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold2", key);	
			} else if (count >= 21 && count <= 30 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold3", key);	
			} else if (count >= 31 && count <= 40 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold4", key);	
			} else if (count >= 41 && count <= 50 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold5", key);	
			} else if (count >= 51 && count <= 60 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold6", key);	
			} else if (count >= 61 && count <= 70 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold7", key);	
			} else if (count >= 71 && count <= 80 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold8", key);	
			} else if (count >= 81 && count <= 90 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold9", key);	
			} else if (count >= 91 ) {
				saveImageSlice_test(scaledAxialImageCED, "axial", "fold10", key);	
			}
			
			
		    //*********************   coronal orientation *************************
			ModelImage coronalImage = coronalList.get(key);
			ModelImage scaledCoronalImage = scaleIntensity(coronalImage);
			
			if (count >= 0 && count <= 10) {	
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold1", key);
			} else if (count >= 11 && count <= 20 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold2", key);	
			} else if (count >= 21 && count <= 30 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold3", key);	
			} else if (count >= 31 && count <= 40 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold4", key);	
			} else if (count >= 41 && count <= 50 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold5", key);	
			} else if (count >= 51 && count <= 60 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold6", key);	
			} else if (count >= 61 && count <= 70 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold7", key);	
			} else if (count >= 71 && count <= 80 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold8", key);	
			} else if (count >= 81 && count <= 90 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold9", key);	
			} else if (count >= 91 ) {
				saveImageSlice_test(scaledCoronalImage, "coronal", "fold10", key);	
			}
			
			
			ModelImage cedCoronalImage = cedTableCoronal.get(key);
			ModelImage scaledCoronalImageCED = scaleIntensity(cedCoronalImage);
			ModelImage coronalImageMaskCED = coronalMaskList.get(key);
			
			if (count >= 0 && count <= 10) {	
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold1", key);
			} else if (count >= 11 && count <= 20 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold2", key);	
			} else if (count >= 21 && count <= 30 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold3", key);	
			} else if (count >= 31 && count <= 40 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold4", key);	
			} else if (count >= 41 && count <= 50 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold5", key);	
			} else if (count >= 51 && count <= 60 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold6", key);	
			} else if (count >= 61 && count <= 70 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold7", key);	
			} else if (count >= 71 && count <= 80 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold8", key);	
			} else if (count >= 81 && count <= 90 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold9", key);	
			} else if (count >= 91 ) {
				saveImageSlice_test(scaledCoronalImageCED, "coronal", "fold10", key);	
			}
			
			
			// *************************   sagittal orientation ************************
			ModelImage sagittalImage = sagittalList.get(key);
			ModelImage scaledSagittalImage = scaleIntensity(sagittalImage);
			ModelImage sagittalMaskImage = sagittalMaskList.get(key);
			
			if (count >= 0 && count <= 10) {	
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold1", key);
			} else if (count >= 11 && count <= 20 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold2", key);	
			} else if (count >= 21 && count <= 30 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold3", key);	
			} else if (count >= 31 && count <= 40 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold4", key);	
			} else if (count >= 41 && count <= 50 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold5", key);	
			} else if (count >= 51 && count <= 60 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold6", key);	
			} else if (count >= 61 && count <= 70 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold7", key);	
			} else if (count >= 71 && count <= 80 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold8", key);	
			} else if (count >= 81 && count <= 90 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold9", key);	
			} else if (count >= 91 ) {
				saveImageSlice_test(scaledSagittalImage, "sagittal", "fold10", key);	
			}
			
			
			ModelImage cedSagittalImage = cedTableSagittal.get(key);
			ModelImage scaledSagittalImageCED = scaleIntensity(cedSagittalImage);
			ModelImage sagittalImageMaskCED = sagittalMaskList.get(key);
			
			if (count >= 0 && count <= 10) {	
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold1", key);
			} else if (count >= 11 && count <= 20 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold2", key);	
			} else if (count >= 21 && count <= 30 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold3", key);	
			} else if (count >= 31 && count <= 40 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold4", key);	
			} else if (count >= 41 && count <= 50 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold5", key);	
			} else if (count >= 51 && count <= 60 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold6", key);	
			} else if (count >= 61 && count <= 70 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold7", key);	
			} else if (count >= 71 && count <= 80 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold8", key);	
			} else if (count >= 81 && count <= 90 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold9", key);	
			} else if (count >= 91 ) {
				saveImageSlice_test(scaledSagittalImageCED, "sagittal", "fold10", key);	
			}
			
			count++;
			
		}

	}
	
	public void saveImageSlice_test(ModelImage image, String orientation, String folder, String key) {

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
		
		// new ViewJFrameImage(maskSlice);
		String sliceDir = saveImageDirectory_test + File.separator + folder + File.separator;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		sliceDir = sliceDir +  orientation + File.separator;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();

		sliceDir += key + File.separator;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();

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

				
				String imgName = null;

				if (orientation.equals("axial")) {
					imgName = "image_" + axial_index + ".png";
				} else if (orientation.equals("coronal")) {
					imgName = "image_" + coronal_index + ".png";
				} else if (orientation.equals("sagittal")) {
					imgName = "image_" + sagittal_index + ".png";
				}

				if (imgName != null) {

					savePNGfile_test(sliceDir, imgName, imageSlice, min, max, xDim, yDim, false);
					
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


			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		newExtents = null;
	}
	
	private void savePNGfile_test(String dirName, String fileName,
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
	
	/*
	 * Convert the MRI and corresponding CED 3D images with ground truth into
	 * 2D png slices.  At this stage, we create the initial folders for the 10-fold
	 * cross-validation.  Each folder contains its own slices.  For training, we
	 * will use shell script to construct the training folds, which merges rest folders
	 * into one single out training fold. And the fold itself will be used to create testing
	 * fold to validate the experiment.      
	 */
	public void save_MRI_CED_2Dslice() {

		int count = 0;
		
		Set<String> keys = keyImagesTransform.keySet();

		System.err.println("saveOrthogonalCEDImage training");

		String sliceDir = saveImageDirectory;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		saveImageDirectory_train = saveImageDirectory + File.separator + "train" + File.separator;
		sliceDir = saveImageDirectory_train;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();

		for (String key : keys) {
			
				System.err.println("key = " + key);
			
				//*********************   axial orientation  ***************************** 
				ModelImage axialImage = axialList.get(key);
				ModelImage scaledAxialImage = scaleIntensity(axialImage);
				ModelImage axialImageMask = axialMaskList.get(key);
				
				if (count >= 0 && count <= 10) {	
				    saveImage(scaledAxialImage, axialImageMask, "axial", "fold1");
				} else if (count >= 11 && count <= 20 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold2");	
				} else if (count >= 21 && count <= 30 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold3");	
				} else if (count >= 31 && count <= 40 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold4");	
				} else if (count >= 41 && count <= 50 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold5");	
				} else if (count >= 51 && count <= 60 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold6");	
				} else if (count >= 61 && count <= 70 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold7");	
				} else if (count >= 71 && count <= 80 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold8");	
				} else if (count >= 81 && count <= 90 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold9");	
				} else if (count >= 91 ) {
					saveImage(scaledAxialImage, axialImageMask, "axial", "fold10");	
				}
				
				
				ModelImage axialImageCED = cedTableAxial.get(key);
				ModelImage scaledAxialImageCED = scaleIntensity(axialImageCED);
				ModelImage axialImageMaskCED = axialMaskList.get(key);
				
				if (count >= 0 && count <= 10) {	
				    saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold1");
				} else if (count >= 11 && count <= 20 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold2");	
				} else if (count >= 21 && count <= 30 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold3");	
				} else if (count >= 31 && count <= 40 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold4");	
				} else if (count >= 41 && count <= 50 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold5");	
				} else if (count >= 51 && count <= 60 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold6");	
				} else if (count >= 61 && count <= 70 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold7");	
				} else if (count >= 71 && count <= 80 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold8");	
				} else if (count >= 81 && count <= 90 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold9");	
				} else if (count >= 91 ) {
					saveImage(scaledAxialImageCED, axialImageMaskCED, "axial", "fold10");	
				}
				
				
			    //*********************   coronal orientation *************************
				ModelImage coronalImage = coronalList.get(key);
				ModelImage scaledCoronalImage = scaleIntensity(coronalImage);
				ModelImage coronalMaskImage = coronalMaskList.get(key);
				
				if (count >= 0 && count <= 10) {	
				    saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold1");
				} else if (count >= 11 && count <= 20 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold2");	
				} else if (count >= 21 && count <= 30 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold3");	
				} else if (count >= 31 && count <= 40 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold4");	
				} else if (count >= 41 && count <= 50 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold5");	
				} else if (count >= 51 && count <= 60 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold6");	
				} else if (count >= 61 && count <= 70 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold7");	
				} else if (count >= 71 && count <= 80 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold8");	
				} else if (count >= 81 && count <= 90 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold9");	
				} else if (count >= 91 ) {
					saveImage(scaledCoronalImage, coronalMaskImage, "coronal", "fold10");	
				}
				
				
				ModelImage cedCoronalImage = cedTableCoronal.get(key);
				ModelImage scaledCoronalImageCED = scaleIntensity(cedCoronalImage);
				ModelImage coronalImageMaskCED = coronalMaskList.get(key);
				
				if (count >= 0 && count <= 10) {	
				    saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold1");
				} else if (count >= 11 && count <= 20 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold2");	
				} else if (count >= 21 && count <= 30 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold3");	
				} else if (count >= 31 && count <= 40 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold4");	
				} else if (count >= 41 && count <= 50 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold5");	
				} else if (count >= 51 && count <= 60 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold6");	
				} else if (count >= 61 && count <= 70 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold7");	
				} else if (count >= 71 && count <= 80 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold8");	
				} else if (count >= 81 && count <= 90 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold9");	
				} else if (count >= 91 ) {
					saveImage(scaledCoronalImageCED, coronalImageMaskCED, "coronal", "fold10");	
				}
				
				
				// *************************   sagittal orientation ************************
				ModelImage sagittalImage = sagittalList.get(key);
				ModelImage scaledSagittalImage = scaleIntensity(sagittalImage);
				ModelImage sagittalMaskImage = sagittalMaskList.get(key);
				
				if (count >= 0 && count <= 10) {	
				    saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold1");
				} else if (count >= 11 && count <= 20 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold2");	
				} else if (count >= 21 && count <= 30 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold3");	
				} else if (count >= 31 && count <= 40 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold4");	
				} else if (count >= 41 && count <= 50 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold5");	
				} else if (count >= 51 && count <= 60 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold6");	
				} else if (count >= 61 && count <= 70 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold7");	
				} else if (count >= 71 && count <= 80 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold8");	
				} else if (count >= 81 && count <= 90 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold9");	
				} else if (count >= 91 ) {
					saveImage(scaledSagittalImage, sagittalMaskImage, "sagittal", "fold10");	
				}
				
				
				ModelImage cedSagittalImage = cedTableSagittal.get(key);
				ModelImage scaledSagittalImageCED = scaleIntensity(cedSagittalImage);
				ModelImage sagittalImageMaskCED = sagittalMaskList.get(key);
				
				if (count >= 0 && count <= 10) {	
				    saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold1");
				} else if (count >= 11 && count <= 20 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold2");	
				} else if (count >= 21 && count <= 30 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold3");	
				} else if (count >= 31 && count <= 40 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold4");	
				} else if (count >= 41 && count <= 50 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold5");	
				} else if (count >= 51 && count <= 60 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold6");	
				} else if (count >= 61 && count <= 70 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold7");	
				} else if (count >= 71 && count <= 80 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold8");	
				} else if (count >= 81 && count <= 90 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold9");	
				} else if (count >= 91 ) {
					saveImage(scaledSagittalImageCED, sagittalImageMaskCED, "sagittal", "fold10");	
				}
				
				count++;
				
		}


	}
	
	public void saveImage(ModelImage image, ModelImage imageMask, String orientation, String folder) {

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		float[] res = image.getResolutions(0);
		float x_res = res[0];
		float y_res = res[1];
		float z_res = res[2];

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

		String sliceDir = saveImageDirectory_train + File.separator + folder + File.separator;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		sliceDir = sliceDir +  orientation + File.separator;
		sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
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

					savePNGfile(sliceDir, imgName, imageSlice, min, max, xDim, yDim, false);
					savePNGfile(sliceDir, maskName, maskSlice, min, max, xDim, yDim, true);
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

	
	/**
	 * Convert the SKI10 axial image to sagittal, coronal orientation image. 
	 */
	public void conversion() {

		Set<String> keys = keyImagesTransform.keySet();
		
		System.err.println("conversion");
		
		for (String key : keys) {
				
			    System.err.println("key = " + key);
			
				ModelImage keyImage = keyImagesTransform.get(key);
				ModelImage keyImageMask = keyImageMasksTransform.get(key);

				keyImage.getVOIs().removeAllElements();
				
				// axial orientation
				axialList.put(key, keyImage);
				System.err.println("finish axial image orientation");
				axialMaskList.put(key, keyImageMask);
			    System.err.println("finish axial image mask orientation");
			
			
				// sagittal orientation
				JDialogReorient sagittal_orient = new JDialogReorient(keyImage);
				sagittal_orient.setVisible(false);
				sagittal_orient.set_sagittal_orientation();
				sagittal_orient.doRun();
				sagittalList.put(key, sagittal_orient.getResultImage());
			    System.err.println("finish sagittal image orientation");	
                
				JDialogReorient sagittal_orient_mask = new JDialogReorient(keyImageMask);
				sagittal_orient_mask.setVisible(false);
				sagittal_orient_mask.set_sagittal_orientation();
				sagittal_orient_mask.doRun();
			    sagittalMaskList.put(key, sagittal_orient_mask.getResultImage());
				System.err.println("finish sagittal image mask orientation");
			
                 
				// coronal orientation
				JDialogReorient coronal_orient = new JDialogReorient(keyImage);
				coronal_orient.setVisible(false);
				coronal_orient.set_coronal_orientation();
				coronal_orient.doRun();
				coronalList.put(key, coronal_orient.getResultImage());
				System.err.println("finish coronal image orientation");

				JDialogReorient coronal_orient_mask = new JDialogReorient(keyImageMask);
				coronal_orient_mask.setVisible(false);
				coronal_orient_mask.set_coronal_orientation();
				coronal_orient_mask.doRun();
				coronalMaskList.put(key, coronal_orient_mask.getResultImage());
				System.err.println("finish coronal image mask orientation");
            
		}

	}
	
	/*
	// To save memory, you can save intermediated axial, sagittal, coronal MRI images
	 * and CED image with corresponding ground truth image mask in MIPAV .xml image file
	 * format. 
	public void saveHED2DsliceCED() {

		Set<String> keys = keyImagesOrientation.keySet();

		for (String key : keys) {
				
				ModelImage axialImage = keyImagesOrientation.get(key);
				ModelImage axialImageMask = keyImagesOrientationMask.get(key);
				new ViewJFrameImage(axialImage);
				ModelImage scaledAxialImage = scaleIntensity(axialImage);
				new ViewJFrameImage(scaledAxialImage);
				
				
				saveImage(scaledAxialImage, axialImageMask, "sagittal");

				
				
				ModelImage axialImageCED = keyImagesOrientationCED.get(key);
				ModelImage axialImageMaskCED = keyImagesOrientationMask.get(key);
				ModelImage scaledAxialImageCED = scaleIntensity(axialImageCED);
				saveImage(scaledAxialImageCED, axialImageMaskCED, "sagittal");

				// ****************** 
				// current ModelImage disposeLocal() call the garbage collection directly, 
				// which slow down the large amount image batch processing task. 
				// comment out the disposeLocal now. 
				// axialImage.disposeLocal();
				axialImage = null;

				// axialImageCED.disposeLocal();
				axialImageCED = null;

				// scaledAxialImage.disposeLocal();
				scaledAxialImage = null;

				// scaledAxialImageCED.disposeLocal();
				scaledAxialImageCED = null;

				// axialImageMask.disposeLocal();
				axialImageMask = null;

				// axialImageMaskCED.disposeLocal();
				axialImageMaskCED = null;
			
		
			break;
			
		}

	}
	*/

	public void saveImage(ModelImage image, ModelImage imageMask, String orientation) {

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		float[] res = image.getResolutions(0);
		float x_res = res[0];
		float y_res = res[1];
		float z_res = res[2];

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

					savePNGfile(sliceDir, imgName, imageSlice, min, max, xDim, yDim, false);
					savePNGfile(sliceDir, maskName, maskSlice, min, max, xDim, yDim, true);
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

	/**
	 * Normalize the image intensity between 25% and 75% intensity range value, 
	 * then scale by a factor of 1000.  Histogram equalization alike normalization 
	 * in the MRM paper. 
	 * 
	 * @param image
	 * @return  normalized and scaled image.
	 */
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

	/**
	 * 
	 * @param dirName  saved directory name
	 * @param fileName  saved file name
	 * @param srcImage   source image
	 * @param minIntensity   source image min intensity
	 * @param maxIntensity   source image max intensity
	 * @param xDim     saved 2D png slice x dimension
	 * @param yDim     saved 2D png slice y dimension
	 * @param isMask   flag to indicate it is ground truth mask to source image
	 */
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

			OutputStream os = new FileOutputStream(file);

			PngWriter pngw = new PngWriter(os, imi);

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
							ImageLineHelper.setPixelGray8(line, x, (int) 255);
						} else {
							ImageLineHelper.setPixelGray8(line, x, (int) 0);
						}

					}

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
     * Read the SKI10 dataset ( .MHD image and .MHD ground truth labels ).
     */
	public void readFile() {

		int index;
		Set<String> keys = nameTableImages.keySet();
		String dir;
		String directory;
		String fileName;
		FileIO keyImageIO;
		
		try {
			    
			for (String key : keys) {
				
				System.err.println("axial key = " + key);
				dir = nameTableImages.get(key);
				if (dir == null)
					continue;
				index = dir.lastIndexOf(File.separator);
				directory = new String(dir.substring(0, index + 1));
				fileName = new String(dir.substring(index + 1, dir.length()));
				keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					keyImagesOrientation.put(key, image);
					keyImagesOrientation.get(key).setImageName(key);
					
				}
				keyImageIO.dispose();
				keyImageIO = null;
			
			}
			
			
			keys = nameTableImagesMask.keySet();

			for (String key : keys) {
				System.err.println("axial mask key = " + key);
				dir = nameTableImagesMask.get(key);
				if (dir == null)
					continue;
				index = dir.lastIndexOf(File.separator);
				directory = new String(dir.substring(0, index + 1));
				fileName = new String(dir.substring(index + 1, dir.length()));

				keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					
					keyImagesOrientationMask.put(key, image);
					keyImagesOrientationMask.get(key).setImageName(key);
					
				}
				keyImageIO.dispose();
				keyImageIO = null;
			}
		


		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Convert the MRI image and corresponding image mask to isotropic image. 
	 */
	public void transform() {

        Set<String> keys = keyImagesOrientation.keySet();
		
		System.err.println("Transform");
		
		for (String key : keys) {
			
			    System.err.println("Transform key = " + key);
				
				ModelImage keyImage = keyImagesOrientation.get(key);
				ModelImage keyImageMask = maskThresAxial.get(key);
				
				
				ModelImage transKeyImage = calculateTransform(keyImage);
				keyImagesTransform.put(key, transKeyImage);
				
				ModelImage transKeyImageMask = calculateTransform(keyImageMask);
				keyImageMasksTransform.put(key, transKeyImageMask);
				
		}
	}
	
	/*
	 * Extract the ground truth label for femur, tibia, femur cartilage, tibia cartilage, etc.  
	 * For MRM paper, just the femur. 
	 */
	public void threshold() {

		Set<String> keys = keyImagesOrientationMask.keySet();

		for (String key : keys) {

				System.err.println("key = " + key);
				ModelImage maskImage = keyImagesOrientationMask.get(key);
				ModelImage thresImage = calculateThreshold(maskImage);
				maskThresAxial.put(key, thresImage);
				
			
		}

	}
	
	
	/**
	 * Extract the theshold from grount truth label.  
	 * @param srcImage
	 * @return
	 */
	private ModelImage calculateThreshold(ModelImage srcImage) {
		
		float[] thresholds = new float[2];
		// femur 
    	thresholds[0] = 1.0f;
    	thresholds[1] = 1.0f;
    	
    	// femur cartilage
    	// thresholds[0] = 2.0f;
    	// thresholds[1] = 2.0f;
    	
    	// tibia cartilage
    	// thresholds[0] = 4.0f;
    	// thresholds[1] = 4.0f;
    	
    	ModelImage thresholdImage = new ModelImage(ModelImage.UBYTE, srcImage.getExtents(), srcImage.getImageName()+ "threshold");
    	(thresholdImage.getFileInfo(0)).setModality(FileInfoBase.OTHER);
    	// fillValue not used
    	float fillValue = 0.0f;
    	int outputType = AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE;
    	boolean wholeImage = true;
    	boolean isInverse = false;
    	AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(thresholdImage, srcImage, thresholds, fillValue, outputType,
                wholeImage, isInverse);
    	thresholdAlgo.run();
    	FileInfoBase[] fileInfo = thresholdImage.getFileInfo();
        fileInfo[0].setModality(FileInfoBase.OTHER);
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());

        fileInfo[0].setEndianess(srcImage.getFileInfo()[0].getEndianess());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(thresholdImage.getExtents());
        fileInfo[0].setMax(thresholdImage.getMax());
        fileInfo[0].setMin(thresholdImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        fileInfo[0].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
        fileInfo[0].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
        thresholdAlgo.finalize();
        thresholdAlgo = null;
		
		return thresholdImage;
	}
	 
	
	/** 
	 * Run Coherance Enhanced Diffusion (CED) filter on all the MRI axial, sagittal
	 * and coronal images.    
	 */
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

	
	public void saveImage() {

		Set<String> keys = keyImagesTransform.keySet();

		System.err.println("saveOrthogonalCEDImage");

		String sliceDir = saveImageDirectory;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		for (String key : keys) {
			// read key image
            	
				System.err.println("key = " + key);

				ModelImage axialImage = keyImagesTransform.get(key);
				ModelImage cedAxialImage = cedTableAxial.get(key);
				ModelImage axialMaskImage = keyImageMasksTransform.get(key);

				String axialDir = sliceDir + File.separator;
				String imgName = "image_ced_" + key + ".xml";
				cedAxialImage.saveImage(axialDir, imgName, FileUtility.XML, false);
				imgName = "image_" + key + ".xml";
				axialImage.saveImage(axialDir, imgName, FileUtility.XML, false);
				imgName = "image_mask_" + key + ".xml";
				axialMaskImage.saveImage(axialDir, imgName, FileUtility.XML, false);

				
			
			
		}
		

	}

	/**
	 * To save memory, save intermediated axial, sagittal, coronal original MRI image, generated CED image
	 * and the gruond truth label to MIPAV readable .xml image format. 
	 */
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

	/**
	 * Calculate the CED image.
	 * @param inImage source image
	 * @return  CED image
	 */
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

	
	/*
	 * Convert the image to 0.5 mm x 0.5 mm x 0.5 mm isotropic sampled images.  
	 * SKI10 images are in axial orientation.   
	 */
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
		resultImage.setImageOrientation(FileInfoBase.AXIAL);
		FileInfoBase fileInfo[] = resultImage.getFileInfo();
		
		for ( int i = 0; i < fileInfo.length; i++ ){
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 0);
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 1);
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_I2S_TYPE, 2);
			
		}
		resultImage.getMatrixHolder().replaceMatrices(keyImage.getMatrixHolder().getMatrices());
		resultImage.calcMinMax();

		algoTrans.disposeLocal();
		algoTrans = null;
		return resultImage;
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