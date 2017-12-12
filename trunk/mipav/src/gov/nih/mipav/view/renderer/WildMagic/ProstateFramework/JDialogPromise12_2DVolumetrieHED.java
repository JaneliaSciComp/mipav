package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import ar.com.hjg.pngj.*;


/**
 * For Miccai promise 12 prostate data, we apply wp segmentation using HED deep learning model. 
 * 
 * Data: given promise 12 data, we apply N4 correction, cropping, and isotropic upsampling.    
 * 
 * Steps:
 * 1. read promise 12 image data with corresponding masks after pre-processing.  
 * 2. From VOIs, generate the binary mask images. 
 * 3. Transform into isotropic image (upsampling) with resolution: 0.35m x 0.35m x 0.35m. 
 * 4. Conversion: converts axial image to axial an coronal using JDialogReoriented. 
 * 5. Generate the CED images from MR images in three orientation: axial, sagittal, coronal. 
 * 6. Each orientation, save 3D MR images, CED images with corresponding binary mask images. 
 * 
 * This is the 2D-volumetric segmentation approach.   
 * 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogPromise12_2DVolumetrieHED extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;


	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** image vector to hold the actual images. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();
	
	private Vector<ModelImage> keyImagesScaleIntensity = new Vector<ModelImage>();
	
	private Vector<ModelImage> keyImagesMask = new Vector<ModelImage>();
	private Vector<ModelImage> keyImagesN4Crop = new Vector<ModelImage>();

	private Vector<String> keyImageVector1 = new Vector<String>();
	
	private Vector<String> keyImageVector2 = new Vector<String>();
	
	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	
	private Vector<ModelImage> axialList = new Vector<ModelImage>();
	private Vector<ModelImage> axialMaskList = new Vector<ModelImage>();
	private Vector<ModelImage> sagittalList = new Vector<ModelImage>();
	private Vector<ModelImage> sagittalMaskList = new Vector<ModelImage>();
	private Vector<ModelImage> coronalList = new Vector<ModelImage>();
	private Vector<ModelImage> coronalMaskList = new Vector<ModelImage>();
	
	private int axial_index = 0;
	private int sagittal_index = 0;
	private int coronal_index = 0;
	
	Hashtable<Integer, ModelImage> cedTableAxial = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> cedTableSagittal = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> cedTableCoronal = new Hashtable<Integer, ModelImage>(); 
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogPromise12_2DVolumetrieHED(Frame theParentFrame) {
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
			sortKeyImage_1();
			sortKeyImage_2();

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

	/**
	 * Read 3D images atlas directory.
	 */
	private void readKeyImageDir() {

		File fileDir_1 = new File("/scratch/miccai/2D_Volumetric/N4TrainCropScale");
		File fileDir_2 = new File("/scratch/miccai/2D_Volumetric/N4TrainMaskCropScale");
		
		traverse_folder_1(fileDir_1);
		traverse_folder_2(fileDir_2); 

	}

	private void traverse_folder_1(File dir) {
		processDir_folder_1(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_1(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder_1(File dir) {
		String dirName = dir.toString();

		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("Case")
				&& dirName.substring(begin, end).endsWith(".mhd")
				&& !dirName.contains("segmentation")) {
			keyImageVector1.add(dir.toString());
		}

	}

	public void sortKeyImage_1() {
		int i;
		int len = keyImageVector1.size();
		String imageName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector1.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			index = Integer.parseInt(imageName.substring(start + 4, end));
			imageNameTable.put(index, imageName);
		}

		keyImageVector1.clear();
		for (i = 0; i < len; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector1.add(imageName);
			}
		}
        
		

	}

	private void traverse_folder_2(File dir) {
		processDir_folder_2(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_2(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder_2(File dir) {
		String dirName = dir.toString();
		
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("Case")
				&& dirName.substring(begin, end).endsWith(".mhd")
				&& dirName.contains("segmentation")) {
			keyImageVector2.add(dir.toString());
		}
	}

	public void sortKeyImage_2() {
		int i;
		int len = keyImageVector2.size();
		String imageName;

		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();

		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector2.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf("_");
			
			index = Integer.parseInt(imageName.substring(start + 4, end));
			imageNameTable.put(index, imageName);
		}

		keyImageVector2.clear();
		for (i = 0; i < len; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector2.add(imageName);
			}
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

		conversion();
		
		System.err.println("saveImage");
		
		runCED();
		
		saveHED2DsliceCED();
		
		// saveTestedImages();
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	public void saveHED2DsliceMRI() {
		
		for ( int i = 0; i < 50; i++ ) {
			
			ModelImage axialImage = axialList.get(i);
			ModelImage axialImageMask = axialMaskList.get(i);
			// new ViewJFrameImage(axialImage);
		    // new ViewJFrameImage(axialImageMask);
			saveImage(axialImage, axialImageMask, "axial");
			
			ModelImage sagittalImage = sagittalList.get(i);
			ModelImage sagittalImageMask = sagittalMaskList.get(i);
			// new ViewJFrameImage(sagittalImage);
			// new ViewJFrameImage(sagittalImageMask);
			saveImage(sagittalImage, sagittalImageMask, "sagittal");
			
			ModelImage coronalImage = coronalList.get(i);
			ModelImage coronalImageMask = coronalMaskList.get(i);
			// new ViewJFrameImage(coronalImage);
			// new ViewJFrameImage(coronalImageMask);
			saveImage(coronalImage, coronalImageMask, "coronal");
		}
	}
	
	public void runCED() {
				
		for (int i = 0; i < 50; i++ ) {
			
			ModelImage axialImage = axialList.get(i);
			ModelImage cedAxial = calculateCoherenceEnhancingDiffusion(axialImage);
			cedTableAxial.put(i, cedAxial);
			
			ModelImage sagittalImage = sagittalList.get(i);
			ModelImage cedSagittal = calculateCoherenceEnhancingDiffusion(sagittalImage);
			cedTableSagittal.put(i, cedSagittal);
			
			ModelImage coronalImage = coronalList.get(i);
			ModelImage cedCoronal = calculateCoherenceEnhancingDiffusion(coronalImage);
			cedTableCoronal.put(i, cedCoronal);
			
		}
		
		
	}
	
    public void saveHED2DsliceCED() {
		
		for ( int i = 0; i < 50; i++ ) {
			
		
			ModelImage axialImage = axialList.get(i);
			ModelImage axialImageMask = axialMaskList.get(i);
			// new ViewJFrameImage(axialImage);
		    // new ViewJFrameImage(axialImageMask);
			saveImage(axialImage, axialImageMask, "axial");
			
		
			ModelImage sagittalImage = sagittalList.get(i);
			ModelImage sagittalImageMask = sagittalMaskList.get(i);
			// new ViewJFrameImage(sagittalImage);
			// new ViewJFrameImage(sagittalImageMask);
			saveImage(sagittalImage, sagittalImageMask, "sagittal");
			
			
			ModelImage coronalImage = coronalList.get(i);
			ModelImage coronalImageMask = coronalMaskList.get(i);
			// new ViewJFrameImage(coronalImage);
			// new ViewJFrameImage(coronalImageMask);
			saveImage(coronalImage, coronalImageMask, "coronal");
			 
		}
		
	
		for ( int i = 0; i < 50; i++ ) {
			
			ModelImage axialImage = cedTableAxial.get(i);
			ModelImage axialImageMask = axialMaskList.get(i);
			// new ViewJFrameImage(axialImage);
		    // new ViewJFrameImage(axialImageMask);
			saveImage(axialImage, axialImageMask, "axial");
			
			ModelImage sagittalImage = cedTableSagittal.get(i);
			ModelImage sagittalImageMask = sagittalMaskList.get(i);
			// new ViewJFrameImage(sagittalImage);
			// new ViewJFrameImage(sagittalImageMask);
			saveImage(sagittalImage, sagittalImageMask, "sagittal");
			
			ModelImage coronalImage = cedTableCoronal.get(i);
			ModelImage coronalImageMask = coronalMaskList.get(i);
			// new ViewJFrameImage(coronalImage);
			// new ViewJFrameImage(coronalImageMask);
			saveImage(coronalImage, coronalImageMask, "coronal");
		}
	 
	}
	
	
    public void saveImage(ModelImage image, ModelImage imageMask,  String orientation) {
		
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];
  
		float[] res = image.getResolutions(0);
		float x_res = res[0];
		float y_res = res[1];
		// float z_res = res[2];
		
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

				ModelImage imageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "image" + j);
				imageSlice.getFileInfo(0).setResolutions(newRes);
				imageSlice.setResolutions(newRes);
				float[] imgBuffer = new float[size];
				image.exportData(j * size, size, imgBuffer);
				imageSlice.importData(0, imgBuffer, true);
				
				// new ViewJFrameImage(imageSlice);
			    
				
				ModelImage maskSlice = new ModelImage(ModelStorageBase.SHORT, newExtents, "mask" + j);
				maskSlice.setResolutions(newRes);
				maskSlice.getFileInfo(0).setResolutions(newRes);
				short[] maskBuffer = new short[size];
				imageMask.exportData(j * size, size, maskBuffer);
				maskSlice.importData(0, maskBuffer, true);
				
				// new ViewJFrameImage(maskSlice);
				
				
				String sliceDir = saveImageDirectory +  orientation + File.separator;
				File sliceDirFile = new File(sliceDir);
				if (!sliceDirFile.isDirectory())
					sliceDirFile.mkdir();
				
				String imgName = null;
				String maskName = null;
				
				if ( orientation.equals("axial")) {
					imgName = "image_" + axial_index + ".png";
					maskName = "voi_" + axial_index + ".png";
				} else if ( orientation.equals("coronal") ) {
					imgName = "image_" + coronal_index + ".png";
					maskName = "voi_" + coronal_index + ".png";
				} else if ( orientation.equals("sagittal")) {
					imgName = "image_" + sagittal_index + ".png";
					maskName = "voi_" + sagittal_index + ".png";
				}
			
				if ( imgName != null && maskName != null ) {
					
					savePNGfile(sliceDir, imgName, imageSlice, min, max, xDim, yDim, false);
					savePNGfile(sliceDir, maskName, maskSlice, min, max, xDim, yDim, true);
					System.err.println("sliceDir = " + sliceDir);
					System.err.println("imgName = " + imgName);
					// imageSlice.saveImage(sliceDir, imgName, FileUtility.JIMI, false);
					// maskSlice.saveImage(sliceDir, maskName, FileUtility.JIMI, false);
					
					if ( orientation.equals("axial")) {
						axial_index++;
					} else if ( orientation.equals("coronal") ) {
						coronal_index++;
					} else if ( orientation.equals("sagittal") ) {
						sagittal_index++;
					}
				}
				 

				imgBuffer = null;
				imageSlice.disposeLocal();
				imageSlice = null;

				maskBuffer = null;
				maskSlice.disposeLocal();
				maskSlice = null;
				
				// imgName = null;
				// maskName = null;
				// sliceDir = null;
				// break;
				
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
			// coherenceEnhancingDiffusionImage.setType(ModelStorageBase.FLOAT);
			// coherenceEnhancingDiffusionImage.reallocate(ModelStorageBase.FLOAT);
			coherenceEnhancingDiffusionImage.setType(type);
			coherenceEnhancingDiffusionImage.reallocate(type);

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

		int size = keyImages.size();

		String fileList = saveImageDirectory;
		File sliceDirFile = new File(fileList);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		
		
		for (int imageIndex = 0; imageIndex < size; imageIndex++) {

			// System.err.println("imageIndex = " + imageIndex);
			// 1. Processing each image separately.
			// if ( imageIndex == 9 ) {
			ModelImage keyImage = keyImages.get(imageIndex);
			ModelImage keyImageMask = keyImagesMask.get(imageIndex);
			// ModelImage maskImage = keyImageVOIs.get(imageIndex);

			// new ViewJFrameImage(keyImage);
			// new ViewJFrameImage(keyImageMask);
						
			// axial orientation
	        axialList.add(imageIndex, keyImage);
	        axialMaskList.add(imageIndex, keyImageMask);
	         			
 			// sagittal orientation
 			ImageReorientation sagittal_orient = new ImageReorientation(keyImage, ImageReorientation.SAGITTAL_INDEX);
 			sagittal_orient.preformOrientation();
 			sagittalList.add(imageIndex, sagittal_orient.getResultImage());
 			
 			ImageReorientation sagittal_orient_mask = new ImageReorientation(keyImageMask, ImageReorientation.SAGITTAL_INDEX);
 			sagittal_orient_mask.preformOrientation();
 			sagittalMaskList.add(imageIndex, sagittal_orient_mask.getResultImage());
 		
 			// coronal orientation
 			ImageReorientation coronal_orient = new ImageReorientation(keyImage, ImageReorientation.CORONAL_INDEX);
 			coronal_orient.preformOrientation();
 			coronalList.add(imageIndex, coronal_orient.getResultImage());
 			
 			ImageReorientation coronal_orient_mask = new ImageReorientation(keyImageMask, ImageReorientation.CORONAL_INDEX);
 			coronal_orient_mask.preformOrientation();
 			coronalMaskList.add(imageIndex, coronal_orient_mask.getResultImage());
			
//			// sagittal orientation 
//			JDialogReorient sag_orient = new JDialogReorient(keyImage);
//			sag_orient.setVisible(false);
//			sag_orient.set_sagittal_orientation();
//			sag_orient.doRun();
//			sagittalList.add(imageIndex, sag_orient.getResultImage());
//			
//		    
//			JDialogReorient sag_orient_mask = new JDialogReorient(keyImageMask);
//			sag_orient_mask.setVisible(false);
//			sag_orient_mask.set_sagittal_orientation();
//			sag_orient_mask.doRun();
//			sagittalMaskList.add(imageIndex, sag_orient_mask.getResultImage());
//			
//			// new ViewJFrameImage(sag_orient_mask.getResultImage());
//			
//			// coronal orientation
//			JDialogReorient coronal_orient = new JDialogReorient(keyImage);
//			coronal_orient.setVisible(false);
//			coronal_orient.set_coronal_orientation();
//			coronal_orient.doRun();
//			coronalList.add(imageIndex, coronal_orient.getResultImage());
//			
//			JDialogReorient coronal_orient_mask = new JDialogReorient(keyImageMask);
//			coronal_orient_mask.setVisible(false);
//			coronal_orient_mask.set_coronal_orientation();
//			coronal_orient_mask.doRun();
//			coronalMaskList.add(imageIndex, coronal_orient_mask.getResultImage());
			
			
			System.err.println("imageIndex = " + imageIndex);
		
		}

		System.err.println("text write finish");

	}
	
	
	public ModelImage scaleIntensity(ModelImage image) {

		int[] destExtents = new int[3];
		
		destExtents[0] = image.getExtents()[0];
		destExtents[1] = image.getExtents()[1];
		destExtents[2] = image.getExtents()[2];
		ModelImage resultImage = (ModelImage)image.clone();
		
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
		
        float contrast = median + 2f * ( inten_75 - inten_25);
        
        for ( int i = 0; i < size_3D; i++ ) {
        	
        	if (origBuffer[i] <=  min) {
        		origBuffer[i] = min;
        	}
        	
        	if (origBuffer[i] >= max) {
        		origBuffer[i] = max;
        	}
        	
        	origBuffer[i] = 1000f * ( origBuffer[i] / contrast);
        }
        
        try {
        	resultImage.importData(0, origBuffer, true);
        } catch (IOException e) {
        	e.printStackTrace();
        }

        return resultImage;
	}
	
	
	public void saveTestedImages() {
		

			int size = keyImagesScaleIntensity.size();
            System.err.println("size = " + size);
			for (int imageIndex = 0; imageIndex < size; imageIndex++) {
				
				ModelImage targetImageSlice = keyImagesScaleIntensity.get(imageIndex);
				// 1) save image
				String sliceDir = saveImageDirectory;
				File sliceDirFile = new File(sliceDir);
				if (!sliceDirFile.isDirectory())
					sliceDirFile.mkdir();
				sliceDir += File.separator + "N4TrainCropScale" + File.separator;
				File dir = new File(sliceDir);
				if (!dir.isDirectory()) {
					dir.mkdir();
				}
			
				String imgName = keyImageVector1.get(imageIndex);
				System.err.println("imgName = " + imgName);
				int start = imgName.lastIndexOf(File.separator) + 1;
				int end = imgName.length();
				imgName = imgName.substring(start, end);
				
			    System.err.println(sliceDir + imgName);
				targetImageSlice.saveImage(sliceDir, imgName, FileUtility.METAIMAGE, false);
		
			}

			size = keyImagesN4Crop.size();
            System.err.println("size = " + size);
			for (int imageIndex = 0; imageIndex < size; imageIndex++) {
				
				ModelImage targetImageSlice = keyImagesN4Crop.get(imageIndex);
				// 1) save image
				String sliceDir = saveImageDirectory;
				File sliceDirFile = new File(sliceDir);
				if (!sliceDirFile.isDirectory())
					sliceDirFile.mkdir();
				sliceDir += File.separator + "N4TrainMaskCropScale" + File.separator;
				File dir = new File(sliceDir);
				if (!dir.isDirectory()) {
					dir.mkdir();
				}
			
				String imgName = keyImageVector2.get(imageIndex);
				System.err.println("imgName = " + imgName);
				int start = imgName.lastIndexOf(File.separator) + 1;
				int end = imgName.length();
				imgName = imgName.substring(start, end);
				
			    System.err.println(sliceDir + imgName);
				targetImageSlice.saveImage(sliceDir, imgName, FileUtility.METAIMAGE, false);
		
			}
		
	}


	private void savePNGfile(String dirName, String fileName, ModelImage srcImage, float minIntensity, float maxIntensity, 
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
					
					
                    if ( isMask == false ) {
                    	float intensity = srcImage.getFloat(i, j);
    					float r = 0;
						if (intensity >= minIntensity && intensity <= maxIntensity) {
							r = (float) ((intensity - minIntensity) * 255d / (maxIntensity - minIntensity));
						} else if (intensity > maxIntensity)
							r = 255;
						else if (intensity < minIntensity)
							r = 0;
						
						 ImageLineHelper.setPixelGray8(line, x, (int) r);
                    } else {
                    	
                    	short intensity = srcImage.getShort(i, j);
                    	
    					if ( intensity == 1 ) {
    						ImageLineHelper.setPixelGray8(line, x, (int)255 );
						} else { 
							ImageLineHelper.setPixelGray8(line, x, (int)0 );
						}
						 
					}
				    
				}
				pngw.writeRow(line, y);
			}
			pngw.end();
			pngw.close();
			pngw = null;
			os.close();
			os = null;
			line = null;
			imi = null;

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

	
		try {
			int len = keyImageVector1.size();
			int currentIndex = 0;

			for (int imageIndex = 0; imageIndex < len; imageIndex++) {
				
				String dir = keyImageVector1.get(imageIndex);

				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				System.err.println("Key Image: fileName = " + fileName
						+ "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				keyImages.add(currentIndex,
						keyImageIO.readImage(fileName, directory));
				currentIndex++;
			}

			currentIndex = 0;
            len = keyImageVector2.size();
			for (int imageIndex = 0; imageIndex < len; imageIndex++) {

				String dir = keyImageVector2.get(imageIndex);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				System.err.println("Key Image: fileName = " + fileName
						+ "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				keyImagesMask.add(currentIndex,
						keyImageIO.readImage(fileName, directory));

				currentIndex++;
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