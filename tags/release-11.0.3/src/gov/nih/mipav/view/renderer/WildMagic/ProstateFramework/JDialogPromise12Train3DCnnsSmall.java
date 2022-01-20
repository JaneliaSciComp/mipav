package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileIO;
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


/**
 * This is the first try to 3D convolution deep learning models. 3D-Unet.
 * 
 * Miccai Promise 12 data: 
 * Preprocessed after,
 * 1. N4 correction
 * 2. Transform to 0.5m x 0.5m x 1.5m resolution images
 * 3. Intensity normalization
 * 
 * Apply 3D cube cropping to the 3D images.   The 3D cube size is 64x64x16. 
 * The MR image and corresponding binary mask images need to crop into two 3D small cubes, then merge
 * them into H5 readable volume. 
 * 
 * The cropping mechanism include three types:
 * 1) general sliding window overlap cropping. 
 * 2) random sampling based cropping inside the image. 
 * 3) ROI based random cropping, within or inside the ROI region. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogPromise12Train3DCnnsSmall extends JDialogBase implements
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
	private Vector<ModelImage> keyImagesCrop = new Vector<ModelImage>();
	private Vector<ModelImage> keyImagesScaleIntensity = new Vector<ModelImage>();
	private Vector<ModelImage> keyImagesMask = new Vector<ModelImage>();
	
	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();

	private Vector<String> keyImageVector2 = new Vector<String>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	private AlgorithmTransform algoTrans;
	
	private RandomNumberGen xRandom;
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogPromise12Train3DCnnsSmall(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();

		xRandom = new RandomNumberGen();
	
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
			// MipavUtil.showHelp("Haral1001");
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

		File fileDir_1 = new File("/scratch/miccai/Preprocessing/N4TrainCropScale");
		File fileDir_2 = new File("/scratch/miccai/Preprocessing/N4TrainMaskCropScale");
		
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

		// test for printing

		i = 0;
		for (String entry : keyImageVector1) {
			System.err.println(i + " = " + entry);
			i++;
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
	 * Debugger for test the image and VOis reading.
	 */
	public void printImages() {
		int len = keyImageVOIVector1.size();
		for (int i = 0; i < len; i++) {
			System.err.println(keyImageVector1.get(i));
			System.err.println(keyImageVOIVector1.get(i));
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

		crop();
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	public void crop() {

		int size = keyImages.size();
		
        try {
        	
			String fileList = saveImageDirectory;
			File sliceDirFile = new File(fileList);
			if (!sliceDirFile.isDirectory())
				sliceDirFile.mkdir();
			fileList += File.separator + "FileList" + File.separator;
			File dir = new File(fileList);
			if (!dir.isDirectory()) {
				dir.mkdir();
			}
			
			String listFileName = fileList + File.separator + "train_sw_crop_1.5_41_to_49.lst";
			
			PrintWriter fileWriter = new PrintWriter(new FileWriter(listFileName));
			
				for (int imageIndex = 0; imageIndex < size; imageIndex++) {
					   // System.err.println("imageIndex = " + imageIndex);
					// 1. Processing each image separately. 
				     if ( imageIndex >= 41 && imageIndex <= 49) {
						ModelImage keyImage = keyImages.get(imageIndex);
						ModelImage keyImageMask = keyImagesMask.get(imageIndex);
						// ModelImage maskImage = keyImageVOIs.get(imageIndex);
			
						// new ViewJFrameImage(keyImage);
						// new ViewJFrameImage(keyImageMask);
						
						cropImage_slidingWindow(keyImage, keyImageMask, imageIndex, fileWriter);
						// cropImage_randomSample(keyImage, keyImageMask, imageIndex, fileWriter);
						// cropImage_randomSample_voi_region(keyImage, keyImageMask, imageIndex, fileWriter);
						// keyImage.disposeLocal();
						keyImage = null;
						// keyImageMask.disposeLocal();
						keyImageMask = null;
						
						// new ViewJFrameImage(keyImageVOIsTransformN4.get(imageIndex));
						System.err.println("imageIndex = " + imageIndex);
						// break;
				    }
				}
		
		  System.err.println("count = " + count);
		// write the hdr image list and mask list. 
		
		 /*
			int len = imageTable.size();
			int i;
            String imageName, voiName;
            
			for (i = 0; i < len; i++) {
				imageName = imageTable.get(i);
				voiName = maskTable.get(i);
			    fileWriter.write(imageName + " " + voiName + "\n");
			}
		*/ 	
			fileWriter.close();
			System.err.println("text write finish");
		} catch (IOException e) {
			e.printStackTrace();
		}

	}
	
	public void scaleIntensity() {

		int size = keyImagesCrop.size();

		for (int imageIndex = 0; imageIndex < size; imageIndex++) {

			ModelImage keyImage = keyImagesCrop.get(imageIndex);
			// ModelImage keyImageN4 = keyImagesN4Crop.get(imageIndex);
			// ModelImage maskImage = keyImageVOIs.get(imageIndex);

			ModelImage transKeyImage = scaleIntensity(keyImage);
			keyImagesScaleIntensity.add(imageIndex, transKeyImage);
			
			// ModelImage transKeyImageN4 = scaleIntensity(keyImageN4);
			// keyImagesN4Crop.add(imageIndex, transKeyImageN4);
			
			// new ViewJFrameImage(keyImageVOIsTransformN4.get(imageIndex));
			System.err.println("imageIndex = " + imageIndex);
			
		}

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
	
	
	
	private void cropImage_randomSample(ModelImage image, ModelImage imageMask, int imageIndex, PrintWriter fileWriter) {

		int[] destExtents = new int[3];
		int[] xBounds = new int[2];
		int[] yBounds = new int[2];
		int[] zBounds = new int[2];
		
		int[] extents = image.getExtents();
		
		int xDim = extents[0];
		int yDim = extents[1];
		int zDim = extents[2];
		
		int index_3D = xDim * yDim * zDim - 1;
		System.err.println("index_3D = " + index_3D);
		boolean[] map = new boolean[index_3D];
		// boolean[] mapX = new boolean[xDim];
		// boolean[] mapY = new boolean[yDim];
		// boolean[] mapZ = new boolean[zDim];
		
		int xSamplePoints = (int)Math.round(xDim / 16);
		int ySamplePoints = (int)Math.round(yDim / 16);
		int zSamplePoints = (int)Math.round(zDim / 8);
		
		// int xSamplePoints = (int)Math.round(xDim * 0.06f);
		// int ySamplePoints = (int)Math.round(yDim * 0.06f);
		// int zSamplePoints = (int)Math.round(zDim * 0.06f);
		
		int totalSamplePoints = (int)(xSamplePoints * ySamplePoints * zSamplePoints * 0.06f);
		// int totalSamplePoints = 10000;
		System.err.println("totalSamplePoints = " + totalSamplePoints);
		int samplePointCount = 0;
		
		// System.err.println("totalSamplePoints = " + totalSamplePoints);
		
		xRandom = new RandomNumberGen();
		// yRandom = new RandomNumberGen();
		// zRandom = new RandomNumberGen();
		count += totalSamplePoints * 6;
		// count += totalSamplePoints;
		
	    System.err.println("count = " + count);
		
		while (true) {

			if ( samplePointCount > totalSamplePoints ) break;
			
			int rand_index = xRandom.genUniformRandomNum(0, index_3D);
          
			if ( map[rand_index] == true ) continue;
				
			int z_index = rand_index / (xDim * yDim);
			int integer_leftover = rand_index % (xDim * yDim);
			int y_index = integer_leftover / xDim;
			int x_index = integer_leftover % xDim;
			
			// int x_index = xRandom.genUniformRandomNum(0, xDim);
			// int y_index = yRandom.genUniformRandomNum(0, yDim);
			// int z_index = zRandom.genUniformRandomNum(0, zDim);
			
			if ((x_index - 16) < 0 || (x_index + 16) > xDim)
				continue;
			if ((y_index - 16) < 0 || (y_index + 16) > yDim)
				continue;
			if ((z_index - 8) < 0 || (z_index + 8) > zDim)
				continue;

			// System.err.println("x_index = " + x_index + " y_index = " + y_index + " z_index = " + z_index);
			
			
			map[rand_index] = true;
			// if ( mapX[x_index] = true && mapY[y_index] == true && mapZ[z_index] == true ) 
			// 	continue;
			
			// mapX[x_index] = true;
			// mapY[y_index] = true;
			// mapZ[z_index] = true;
			
			// *******************************************************
			// float value = imageMask.get(x_index, y_index, z_index);
			// *******************************************************
			
			int xPoint = 16;
			int yPoint = 16;
			int zPoint = 8;
			int rightSide = 16; // in pixels
			int leftSide = 16; // in pixels
			int topSide = 16; // in pixels
			int bottomSide = 16; // in pixels
			int front = 8; // in slices
			int back = 8; // in slices

			xBounds[0] = xPoint - leftSide;
			xBounds[1] = xPoint + rightSide;
			yBounds[0] = yPoint - topSide;
			yBounds[1] = yPoint + bottomSide;
			zBounds[0] = zPoint - front;
			zBounds[1] = zPoint + back;

			destExtents[0] = Math.abs(xBounds[1] - xBounds[0]);
			destExtents[1] = Math.abs(yBounds[1] - yBounds[0]);
			destExtents[2] = Math.abs(zBounds[1] - zBounds[0]);

			xPoint = x_index;
			yPoint = y_index;
			zPoint = z_index;

			rightSide = 16; // in pixels
			leftSide = 16; // in pixels
			topSide = 16; // in pixels
			bottomSide = 16; // in pixels
			front = 8; // in slices
			back = 8; // in slices

			xBounds[0] = xPoint - leftSide;
			xBounds[1] = xPoint + rightSide;
			yBounds[0] = yPoint - topSide;
			yBounds[1] = yPoint + bottomSide;
			zBounds[0] = zPoint - front;
			zBounds[1] = zPoint + back;

			destExtents[0] = Math.abs(xBounds[1] - xBounds[0]);
			destExtents[1] = Math.abs(yBounds[1] - yBounds[0]);
			destExtents[2] = Math.abs(zBounds[1] - zBounds[0]);

			
			ModelImage resultImage = new ModelImage(image.getType(),
					destExtents, makeImageName(image.getImageName(), "_crop"));

			xBounds[0] *= -1;
			xBounds[1] *= -1;
			yBounds[0] *= -1;
			yBounds[1] *= -1;
			zBounds[0] *= -1;
			zBounds[1] *= -1;
            
			
			AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(image,
					resultImage, xBounds, yBounds, zBounds);
			cropAlgo.run();

			ModelImage resultImageMask = new ModelImage(imageMask.getType(),
					destExtents, makeImageName(imageMask.getImageName(),
							"_crop"));
			AlgorithmAddMargins cropAlgoMask = new AlgorithmAddMargins(
					imageMask, resultImageMask, xBounds, yBounds, zBounds);
			cropAlgoMask.run();
            
		    
			// 1) rotate z axis 90 degree
			AlgorithmRotate rotateAlgo_90 = new AlgorithmRotate(resultImage,
					AlgorithmRotate.Z_AXIS_PLUS);
			rotateAlgo_90.setQuiet(true);
			rotateAlgo_90.run();
			ModelImage img_rotate_90 = rotateAlgo_90.returnImage();

			AlgorithmRotate rotateAlgo_90_mask = new AlgorithmRotate(
					resultImageMask, AlgorithmRotate.Z_AXIS_PLUS);
			rotateAlgo_90_mask.setQuiet(true);
			rotateAlgo_90_mask.run();
			ModelImage img_mask_rotate_90 = rotateAlgo_90_mask.returnImage();

			// 2) rotate z axis 180 degree
			AlgorithmRotate rotateAlgo_180 = new AlgorithmRotate(resultImage,
					AlgorithmRotate.Z_AXIS_180);
			rotateAlgo_180.setQuiet(true);
			rotateAlgo_180.run();
			ModelImage img_rotate_180 = rotateAlgo_180.returnImage();

			AlgorithmRotate rotateAlgo_180_mask = new AlgorithmRotate(
					resultImageMask, AlgorithmRotate.Z_AXIS_180);
			rotateAlgo_180_mask.setQuiet(true);
			rotateAlgo_180_mask.run();
			ModelImage img_mask_rotate_180 = rotateAlgo_180_mask.returnImage();

			// 3) rotate z axis 270 degree
			AlgorithmRotate rotateAlgo_270 = new AlgorithmRotate(resultImage,
					AlgorithmRotate.Z_AXIS_MINUS);
			rotateAlgo_270.setQuiet(true);
			rotateAlgo_270.run();
			ModelImage img_rotate_270 = rotateAlgo_270.returnImage();

			AlgorithmRotate rotateAlgo_270_mask = new AlgorithmRotate(
					resultImageMask, AlgorithmRotate.Z_AXIS_MINUS);
			rotateAlgo_270_mask.setQuiet(true);
			rotateAlgo_270_mask.run();
			ModelImage img_mask_rotate_270 = rotateAlgo_270_mask.returnImage();

			// 4) flip left-right
			ModelImage flip_leftright = (ModelImage) resultImage.clone();
			AlgorithmFlip flipAlgo_leftright = new AlgorithmFlip(
					flip_leftright, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE,
					true);
			flipAlgo_leftright.run();

			ModelImage flip_mask_leftright = (ModelImage) resultImageMask.clone();
			AlgorithmFlip flipAlgo_mask_leftright = new AlgorithmFlip(
					flip_mask_leftright, AlgorithmFlip.Y_AXIS,
					AlgorithmFlip.IMAGE, true);
			flipAlgo_mask_leftright.run();

			// 5) flip top-bottom
			ModelImage flip_topbottom = (ModelImage) resultImage.clone();
			AlgorithmFlip flipAlgo_topbottom = new AlgorithmFlip(
					flip_topbottom, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE,
					true);
			flipAlgo_topbottom.run();

			ModelImage flip_mask_topbottom = (ModelImage) resultImageMask
					.clone();
			AlgorithmFlip flipAlgo_mask_topbottom = new AlgorithmFlip(
					flip_mask_topbottom, AlgorithmFlip.X_AXIS,
					AlgorithmFlip.IMAGE, true);
			flipAlgo_mask_topbottom.run();
             
			// new ViewJFrameImage(resultImage);
			// new ViewJFrameImage(resultImageMask);
           
			try {
				// H5Write(resultImage, resultImageMask);
			   
			
				writeAnalyze(resultImage, resultImageMask, img_rotate_90,
				 		img_mask_rotate_90, img_rotate_180,
						img_mask_rotate_180, img_rotate_270,
						img_mask_rotate_270, flip_leftright,
						flip_mask_leftright, flip_topbottom,
						flip_mask_topbottom, xPoint, yPoint, zPoint,
						imageIndex, fileWriter);
			   
				
				// writeAnalyze(resultImage, resultImageMask, xPoint, yPoint, zPoint, imageIndex, fileWriter);
            
				cropAlgo = null;
				cropAlgoMask = null;
				// resultImage.disposeLocal();
				resultImage = null;
				// resultImageMask.disposeLocal();
				resultImageMask = null;
                 
			    
				rotateAlgo_90 = null;
				rotateAlgo_90_mask = null;
				// img_rotate_90.disposeLocal();
				img_rotate_90 = null;
				// img_mask_rotate_90.disposeLocal();
				img_mask_rotate_90 = null;

				rotateAlgo_180 = null;
				rotateAlgo_180_mask = null;
				// img_rotate_180.disposeLocal();
				img_rotate_180 = null;
				// img_mask_rotate_180.disposeLocal();
				img_mask_rotate_180 = null;

				rotateAlgo_270 = null;
				rotateAlgo_270_mask = null;
				// img_rotate_270.disposeLocal();
				img_rotate_270 = null;
				// img_mask_rotate_270.disposeLocal();
				img_mask_rotate_270 = null;

				flipAlgo_leftright = null;
				// flip_leftright.disposeLocal();
				flip_leftright = null;
				flipAlgo_mask_leftright = null;
				// flip_mask_leftright.disposeLocal();
				flip_mask_leftright = null;

				flipAlgo_topbottom = null;
				// flip_topbottom.disposeLocal();
				flip_topbottom = null;
				flipAlgo_mask_topbottom = null;
				// flip_mask_topbottom.disposeLocal();
				flip_mask_topbottom = null;
				
				// System.err.println("samplePointCount = " + samplePointCount);
				samplePointCount++;

			} catch (Exception e) {
				e.printStackTrace();
			}

		}  // end while ( true ) 
	  
		// mapX = null;
		// mapY = null;
		// mapZ = null;
		map = null;
		xRandom = null;
		
		destExtents = null;
		xBounds = null;
		yBounds = null;
		zBounds = null;
		extents = null;
		
	}
	
	private int findMaskExtents(ModelImage maskImage) {
		
		int[] extents = maskImage.getExtents();
		int xDim = extents[0];
		int yDim = extents[1];
		int zDim = extents[2];
		int length =  xDim * yDim * zDim;
		float[] buffer = new float[length];
        int samplePoints;
        int xLen, yLen, zLen;
		
        int xMin = Integer.MAX_VALUE;
		int xMax = Integer.MIN_VALUE;
        
		int yMin = Integer.MAX_VALUE;
		int yMax = Integer.MIN_VALUE;
        
		int zMin = Integer.MAX_VALUE;
		int zMax = Integer.MIN_VALUE;
		
		try {
			maskImage.exportData(0, length, buffer);
		} catch (IOException error) {
			System.out.println("IO exception");
            return -1;
		}
		
		for ( int i = 0; i < length; i++ ) {
			
			int z_index = i / (xDim * yDim);
			int integer_leftover = i % (xDim * yDim);
			int y_index = integer_leftover / xDim;
			int x_index = integer_leftover % xDim;
			
			if ( buffer[i] == 1 ) {
				if ( x_index < xMin ) {
					xMin = x_index;
				}
				if ( x_index > xMax) {
					xMax = x_index;
				}
				if ( y_index < yMin ) {
					yMin = y_index;
				}
				if ( y_index > yMax) {
					yMax = y_index;
				}
				if ( z_index < zMin ) {
					zMin = z_index;
				}
				if ( z_index > zMax) {
					zMax = z_index;
				}
			}
		}
		
		xLen = xMax - xMin;
		yLen = yMax - yMin;
		zLen = zMax - zMin;
		
	    samplePoints = Math.round(xLen * 0.06f) * Math.round(yLen * 0.06f) * Math.round(zLen * 0.06f); 
		System.err.println("samplePoints = " + samplePoints);
		return samplePoints;
		
	}
	
	int count = 0;
	
	private void cropImage_randomSample_voi_region(ModelImage image, ModelImage imageMask, int imageIndex, PrintWriter fileWriter) {

		int[] destExtents = new int[3];
		int[] xBounds = new int[2];
		int[] yBounds = new int[2];
		int[] zBounds = new int[2];
		int totalSamplePoints;
		
		
		int[] extents = image.getExtents();
		
		int xDim = extents[0];
		int yDim = extents[1];
		int zDim = extents[2];
		
		int index_3D = xDim * yDim * zDim - 1;
		boolean[] map = new boolean[index_3D];
		
		// int xSamplePoints = (int)Math.round(xDim / 40);
		// int ySamplePoints = (int)Math.round(yDim / 40);
		// int zSamplePoints = (int)Math.round(zDim / 12);
		
		// if ( xDim < 210 )
		// 	totalSamplePoints = 60;
		// else 
		// 	totalSamplePoints = 90;
		// int totalSamplePoints = xSamplePoints * ySamplePoints * zSamplePoints;
		int samplePointCount = 0;
		
		// erode image mask first to walk inward a bit
		// ModelImage erodeImage = (ModelImage)imageMask.clone();
		// erodeImage.setImageName("erode");
		// int kernelErode = AlgorithmMorphology25D.CONNECTED4;
		// float kernelSizeErode = 0.0f;
		// int iters = 4;
		// AlgorithmMorphology25D erodeAlgo25D = new AlgorithmMorphology25D(
		// 		erodeImage, kernelErode, kernelSizeErode,
		// 		AlgorithmMorphology25D.ERODE, 0, iters, 0, 0, true);
		// erodeAlgo25D.run();
		
		totalSamplePoints = findMaskExtents(imageMask);
		System.err.println("totalSamplePoints = " + totalSamplePoints);
		count += totalSamplePoints * 6;
		
		System.err.println("count = " + count);
		
		xRandom = new RandomNumberGen();
		
		while (true) {

			if ( samplePointCount > totalSamplePoints ) break;
			
			int rand_index = xRandom.genUniformRandomNum(0, index_3D);

			if (map[rand_index] == true)
				continue;

			int z_index = rand_index / (xDim * yDim);
			int integer_leftover = rand_index % (xDim * yDim);
			int y_index = integer_leftover / xDim;
			int x_index = integer_leftover % xDim;

			if ((x_index - 16) < 0 || (x_index + 16) > xDim)
				continue;
			if ((y_index - 16) < 0 || (y_index + 16) > yDim)
				continue;
			if ((z_index - 8) < 0 || (z_index + 8) > zDim)
				continue;
			
			Number value = imageMask.get(x_index, y_index, z_index);
			int intensity = value.intValue();
			
			if ( intensity != 1 ) continue;
			
			map[rand_index] = true;
			
			
			int xPoint = 16;
			int yPoint = 16;
			int zPoint = 8;
			int rightSide = 16; // in pixels
			int leftSide = 16; // in pixels
			int topSide = 16; // in pixels
			int bottomSide = 16; // in pixels
			int front = 8; // in slices
			int back = 8; // in slices

			xBounds[0] = xPoint - leftSide;
			xBounds[1] = xPoint + rightSide;
			yBounds[0] = yPoint - topSide;
			yBounds[1] = yPoint + bottomSide;
			zBounds[0] = zPoint - front;
			zBounds[1] = zPoint + back;

			destExtents[0] = Math.abs(xBounds[1] - xBounds[0]);
			destExtents[1] = Math.abs(yBounds[1] - yBounds[0]);
			destExtents[2] = Math.abs(zBounds[1] - zBounds[0]);

			xPoint = x_index;
			yPoint = y_index;
			zPoint = z_index;

			rightSide = 16; // in pixels
			leftSide = 16; // in pixels
			topSide = 16; // in pixels
			bottomSide = 16; // in pixels
			front = 8; // in slices
			back = 8; // in slices

			xBounds[0] = xPoint - leftSide;
			xBounds[1] = xPoint + rightSide;
			yBounds[0] = yPoint - topSide;
			yBounds[1] = yPoint + bottomSide;
			zBounds[0] = zPoint - front;
			zBounds[1] = zPoint + back;

			destExtents[0] = Math.abs(xBounds[1] - xBounds[0]);
			destExtents[1] = Math.abs(yBounds[1] - yBounds[0]);
			destExtents[2] = Math.abs(zBounds[1] - zBounds[0]);

			ModelImage resultImage = new ModelImage(image.getType(),
					destExtents, makeImageName(image.getImageName(), "_crop"));

			xBounds[0] *= -1;
			xBounds[1] *= -1;
			yBounds[0] *= -1;
			yBounds[1] *= -1;
			zBounds[0] *= -1;
			zBounds[1] *= -1;

			AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(image,
					resultImage, xBounds, yBounds, zBounds);
			cropAlgo.run();

			ModelImage resultImageMask = new ModelImage(imageMask.getType(),
					destExtents, makeImageName(imageMask.getImageName(),
							"_crop"));
			AlgorithmAddMargins cropAlgoMask = new AlgorithmAddMargins(
					imageMask, resultImageMask, xBounds, yBounds, zBounds);
			cropAlgoMask.run();

			// 1) rotate z axis 90 degree
			AlgorithmRotate rotateAlgo_90 = new AlgorithmRotate(resultImage,
					AlgorithmRotate.Z_AXIS_PLUS);
			rotateAlgo_90.setQuiet(true);
			rotateAlgo_90.run();
			ModelImage img_rotate_90 = rotateAlgo_90.returnImage();

			AlgorithmRotate rotateAlgo_90_mask = new AlgorithmRotate(
					resultImageMask, AlgorithmRotate.Z_AXIS_PLUS);
			rotateAlgo_90_mask.setQuiet(true);
			rotateAlgo_90_mask.run();
			ModelImage img_mask_rotate_90 = rotateAlgo_90_mask.returnImage();

			// 2) rotate z axis 180 degree
			AlgorithmRotate rotateAlgo_180 = new AlgorithmRotate(resultImage,
					AlgorithmRotate.Z_AXIS_180);
			rotateAlgo_180.setQuiet(true);
			rotateAlgo_180.run();
			ModelImage img_rotate_180 = rotateAlgo_180.returnImage();

			AlgorithmRotate rotateAlgo_180_mask = new AlgorithmRotate(
					resultImageMask, AlgorithmRotate.Z_AXIS_180);
			rotateAlgo_180_mask.setQuiet(true);
			rotateAlgo_180_mask.run();
			ModelImage img_mask_rotate_180 = rotateAlgo_180_mask.returnImage();

			// 3) rotate z axis 270 degree
			AlgorithmRotate rotateAlgo_270 = new AlgorithmRotate(resultImage,
					AlgorithmRotate.Z_AXIS_MINUS);
			rotateAlgo_270.setQuiet(true);
			rotateAlgo_270.run();
			ModelImage img_rotate_270 = rotateAlgo_270.returnImage();

			AlgorithmRotate rotateAlgo_270_mask = new AlgorithmRotate(
					resultImageMask, AlgorithmRotate.Z_AXIS_MINUS);
			rotateAlgo_270_mask.setQuiet(true);
			rotateAlgo_270_mask.run();
			ModelImage img_mask_rotate_270 = rotateAlgo_270_mask.returnImage();

			// 4) flip left-right
			ModelImage flip_leftright = (ModelImage) resultImage.clone();
			AlgorithmFlip flipAlgo_leftright = new AlgorithmFlip(
					flip_leftright, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE,
					true);
			flipAlgo_leftright.run();

			ModelImage flip_mask_leftright = (ModelImage) resultImageMask
					.clone();
			AlgorithmFlip flipAlgo_mask_leftright = new AlgorithmFlip(
					flip_mask_leftright, AlgorithmFlip.Y_AXIS,		

					AlgorithmFlip.IMAGE, true);
			flipAlgo_mask_leftright.run();

			// 5) flip top-bottom
			ModelImage flip_topbottom = (ModelImage) resultImage.clone();
			AlgorithmFlip flipAlgo_topbottom = new AlgorithmFlip(
					flip_topbottom, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE,
					true);
			flipAlgo_topbottom.run();

			ModelImage flip_mask_topbottom = (ModelImage) resultImageMask
					.clone();
			AlgorithmFlip flipAlgo_mask_topbottom = new AlgorithmFlip(
					flip_mask_topbottom, AlgorithmFlip.X_AXIS,
					AlgorithmFlip.IMAGE, true);
			flipAlgo_mask_topbottom.run();
             
			// new ViewJFrameImage(resultImage);
			// new ViewJFrameImage(resultImageMask);
             
			try {
			
				// H5Write(resultImage, resultImageMask);
				writeAnalyze(resultImage, resultImageMask, img_rotate_90,
						img_mask_rotate_90, img_rotate_180,
						img_mask_rotate_180, img_rotate_270,
						img_mask_rotate_270, flip_leftright,
						flip_mask_leftright, flip_topbottom,
						flip_mask_topbottom, xPoint, yPoint, zPoint,
						imageIndex, fileWriter);

				// erodeAlgo25D = null;
			    cropAlgo = null;
				cropAlgoMask = null;
				// resultImage.disposeLocal();
				resultImage = null;
				// resultImageMask.disposeLocal();
				resultImageMask = null;

				rotateAlgo_90 = null;
				rotateAlgo_90_mask = null;
				// img_rotate_90.disposeLocal();
				img_rotate_90 = null;
				// img_mask_rotate_90.disposeLocal();
				img_mask_rotate_90 = null;

				rotateAlgo_180 = null;
				rotateAlgo_180_mask = null;
				// img_rotate_180.disposeLocal();
				img_rotate_180 = null;
				// img_mask_rotate_180.disposeLocal();
				img_mask_rotate_180 = null;

				rotateAlgo_270 = null;
				rotateAlgo_270_mask = null;
				// img_rotate_270.disposeLocal();
				img_rotate_270 = null;
				// img_mask_rotate_270.disposeLocal();
				img_mask_rotate_270 = null;

				flipAlgo_leftright = null;
				// flip_leftright.disposeLocal();
				flip_leftright = null;
				flipAlgo_mask_leftright = null;
				// flip_mask_leftright.disposeLocal();
				flip_mask_leftright = null;

				flipAlgo_topbottom = null;
				// flip_topbottom.disposeLocal();
				flip_topbottom = null;
				flipAlgo_mask_topbottom = null;
				// flip_mask_topbottom.disposeLocal();
				flip_mask_topbottom = null;

				samplePointCount++;
 
			} catch (Exception e) {
				e.printStackTrace();
			}

		}  // end while ( true ) 
		
		map = null;
		xRandom = null;
		destExtents = null;
		xBounds = null;
		yBounds = null;
		zBounds = null;
		extents = null;
	
	}
	
	private void cropImage_slidingWindow(ModelImage image, ModelImage imageMask, int imageIndex, PrintWriter fileWriter) {

		int[] destExtents = new int[3];
		int[] xBounds = new int[2];
		int[] yBounds = new int[2];
		int[] zBounds = new int[2];
		
		int[] extents = image.getExtents();
		
		int xDim = extents[0];
		int yDim = extents[1];
		int zDim = extents[2];
		
		int xIndex, yIndex, zIndex;
		int xStart = 32, xEnd = (xDim - 32);
		int yStart = 32, yEnd = (yDim - 32);
		int zStart = 8, zEnd = (zDim - 8);
		
	    int xPoint = 32;
	    int yPoint = 32;
	    int zPoint = 8;
	    int rightSide = 32; // in pixels
	    int leftSide = 32; // in pixels
	    int topSide = 32; // in pixels
	    int bottomSide = 32; // in pixels
	    int front = 8; // in slices
	    int back = 8; // in slices
		
	    xBounds[0] = xPoint - leftSide;
        xBounds[1] = xPoint + rightSide;
        yBounds[0] = yPoint - topSide;
        yBounds[1] = yPoint + bottomSide;
        zBounds[0] = zPoint - front;
        zBounds[1] = zPoint + back;
        
        destExtents[0] = Math.abs(xBounds[1] - xBounds[0]);
        destExtents[1] = Math.abs(yBounds[1] - yBounds[0]);
        destExtents[2] = Math.abs(zBounds[1] - zBounds[0]);
      
      
		xRandom = new RandomNumberGen();
		// yRandom = new RandomNumberGen();
		// zRandom = new RandomNumberGen();
		
        
        int localCount = 0;
		
		for ( zIndex = zStart; zIndex < zEnd; zIndex += 8) {
			for ( yIndex = yStart; yIndex < yEnd; yIndex += 32 ) {
				for (xIndex = xStart; xIndex < xEnd; xIndex += 32) {
                    
					xPoint = xIndex;
					yPoint = yIndex;
					zPoint = zIndex;
					
					rightSide = 32; // in pixels
					leftSide = 32; // in pixels
					topSide = 32; // in pixels
					bottomSide = 32; // in pixels
					front = 8; // in slices
					back = 8; // in slices

					xBounds[0] = xPoint - leftSide;
					xBounds[1] = xPoint + rightSide;
					yBounds[0] = yPoint - topSide;
					yBounds[1] = yPoint + bottomSide;
					zBounds[0] = zPoint - front;
					zBounds[1] = zPoint + back;

					destExtents[0] = Math.abs(xBounds[1] - xBounds[0]);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0]);
					destExtents[2] = Math.abs(zBounds[1] - zBounds[0]);

					ModelImage resultImage = new ModelImage(image.getType(), destExtents, makeImageName(image.getImageName(), "_crop"));

					xBounds[0] *= -1;
					xBounds[1] *= -1;
					yBounds[0] *= -1;
					yBounds[1] *= -1;
					zBounds[0] *= -1;
					zBounds[1] *= -1;
					
					AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(image, resultImage, xBounds, yBounds, zBounds);
					cropAlgo.run();
					
					ModelImage resultImageMask = new ModelImage(imageMask.getType(), destExtents, makeImageName(imageMask.getImageName(), "_crop"));
					AlgorithmAddMargins cropAlgoMask = new AlgorithmAddMargins(imageMask, resultImageMask, xBounds, yBounds, zBounds);
					cropAlgoMask.run();
				
				
					// 1) rotate z axis 90 degree
					AlgorithmRotate rotateAlgo_90 = new AlgorithmRotate(resultImage, AlgorithmRotate.Z_AXIS_PLUS);
					rotateAlgo_90.setQuiet(true);
					rotateAlgo_90.run();
					ModelImage img_rotate_90 = rotateAlgo_90.returnImage();
					
					AlgorithmRotate rotateAlgo_90_mask = new AlgorithmRotate(resultImageMask, AlgorithmRotate.Z_AXIS_PLUS);
					rotateAlgo_90_mask.setQuiet(true);
					rotateAlgo_90_mask.run();
					ModelImage img_mask_rotate_90 = rotateAlgo_90_mask.returnImage();
					
					// 2) rotate z axis 180 degree
					AlgorithmRotate rotateAlgo_180 = new AlgorithmRotate(resultImage, AlgorithmRotate.Z_AXIS_180);
					rotateAlgo_180.setQuiet(true);
					rotateAlgo_180.run();
					ModelImage img_rotate_180 = rotateAlgo_180.returnImage();
					
					AlgorithmRotate rotateAlgo_180_mask = new AlgorithmRotate(resultImageMask, AlgorithmRotate.Z_AXIS_180);
					rotateAlgo_180_mask.setQuiet(true);
					rotateAlgo_180_mask.run();
					ModelImage img_mask_rotate_180 = rotateAlgo_180_mask.returnImage();
					
					// 3) rotate z axis 270 degree
					AlgorithmRotate rotateAlgo_270 = new AlgorithmRotate(resultImage, AlgorithmRotate.Z_AXIS_MINUS);
					rotateAlgo_270.setQuiet(true);
					rotateAlgo_270.run();
					ModelImage img_rotate_270 = rotateAlgo_270.returnImage();
					
					AlgorithmRotate rotateAlgo_270_mask = new AlgorithmRotate(resultImageMask, AlgorithmRotate.Z_AXIS_MINUS);
					rotateAlgo_270_mask.setQuiet(true);
					rotateAlgo_270_mask.run();
					ModelImage img_mask_rotate_270 = rotateAlgo_270_mask.returnImage();
					
					// 4) flip left-right
					ModelImage flip_leftright = (ModelImage)resultImage.clone();
					AlgorithmFlip flipAlgo_leftright = new AlgorithmFlip(flip_leftright, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE, true);
					flipAlgo_leftright.run();
					
					ModelImage flip_mask_leftright = (ModelImage)resultImageMask.clone();
					AlgorithmFlip flipAlgo_mask_leftright = new AlgorithmFlip(flip_mask_leftright, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE, true);
					flipAlgo_mask_leftright.run();
					
					// 5) flip top-bottom
					ModelImage flip_topbottom = (ModelImage)resultImage.clone();
					AlgorithmFlip flipAlgo_topbottom = new AlgorithmFlip(flip_topbottom, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE, true);
					flipAlgo_topbottom.run();
					
					ModelImage flip_mask_topbottom = (ModelImage)resultImageMask.clone();
					AlgorithmFlip flipAlgo_mask_topbottom = new AlgorithmFlip(flip_mask_topbottom, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE, true);
					flipAlgo_mask_topbottom.run();
			       
					
					try {
						// H5Write(resultImage, resultImageMask);
						
						
						writeAnalyze(resultImage, resultImageMask, 
							    img_rotate_90, img_mask_rotate_90, 
								img_rotate_180, img_mask_rotate_180,
								img_rotate_270, img_mask_rotate_270,
								flip_leftright, flip_mask_leftright,
								flip_topbottom, flip_mask_topbottom, 
								xPoint, yPoint, zPoint, imageIndex, fileWriter);
							 
						
						// writeAnalyze(resultImage, resultImageMask,  xPoint, yPoint, zPoint, imageIndex, fileWriter);
						
						cropAlgo = null;
						// cropAlgoMask.finalize();
						cropAlgoMask = null;
						// resultImage.disposeLocal();
						resultImage = null;
						// resultImageMask.disposeLocal();
						resultImageMask = null;
						
						// rotateAlgo_90.finalize();
						rotateAlgo_90 = null;
						// rotateAlgo_90_mask.finalize();
						rotateAlgo_90_mask = null;
						// img_rotate_90.disposeLocal();
						img_rotate_90 = null;
						// img_mask_rotate_90.disposeLocal();
						img_mask_rotate_90 = null;
						
						// rotateAlgo_180.finalize();
						rotateAlgo_180 = null;
						// rotateAlgo_180_mask.finalize();
						rotateAlgo_180_mask = null;
						// img_rotate_180.disposeLocal();
						img_rotate_180 = null;
						// img_mask_rotate_180.disposeLocal();
						img_mask_rotate_180 = null;
						
						// rotateAlgo_270.finalize();
						rotateAlgo_270 = null;
						// rotateAlgo_270_mask.finalize();
						rotateAlgo_270_mask = null;
						// img_rotate_270.disposeLocal();
						img_rotate_270 = null;
						// img_mask_rotate_270.disposeLocal();
						img_mask_rotate_270 = null;
						 
						
						// flipAlgo_leftright.finalize();
						flipAlgo_leftright = null;
						// flip_leftright.disposeLocal();
						flip_leftright = null;
						// flipAlgo_mask_leftright.finalize();
					    flipAlgo_mask_leftright = null;
						// flip_mask_leftright.disposeLocal();
						flip_mask_leftright = null;
						
						// flipAlgo_topbottom.finalize();
						flipAlgo_topbottom = null;
						// flip_topbottom.disposeLocal();
						flip_topbottom = null;
						// flipAlgo_mask_topbottom.finalize();
						flipAlgo_mask_topbottom = null;
						// flip_mask_topbottom.disposeLocal();
						flip_mask_topbottom = null;
				
						
						
					} catch ( Exception e ) {
						e.printStackTrace();
					}
				
					localCount++;
					// pause();
                    // break;
				} // xIndex loop
				// break;
			}  // yIndex loop
			// break;
	     }   // zIndex loop
		System.err.println("localCount = " + localCount);
		count += localCount * 6;
		
	}

	public void writeAnalyze(ModelImage image, ModelImage mask, 
			int x, int y, int z, int imageIndex, PrintWriter fileWriter) {
		
		String sliceDirTrainImage = saveImageDirectory;
		File sliceDirFile = new File(sliceDirTrainImage);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		sliceDirTrainImage += "trainImage" + File.separator;
		File dir = new File(sliceDirTrainImage);
		if (!dir.isDirectory()) {
			dir.mkdir();
		}

		String sliceDirTrainMask = saveImageDirectory;
		File sliceDirFileMask = new File(sliceDirTrainMask);
		if (!sliceDirFileMask.isDirectory())
			sliceDirFileMask.mkdir();
		sliceDirTrainMask += "trainMask" + File.separator;
		File dirMask = new File(sliceDirTrainMask);
		if (!dirMask.isDirectory()) {
			dirMask.mkdir();
		}

		String coordinate_label = "_" + x + "_" + y + "_" + z + "_";
		String imgName = keyImageVector1.get(imageIndex);
	    String maskName = keyImageVector2.get(imageIndex);
	    // 1. save image and corresponding masks
		saveImage(image, imgName, mask, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label, fileWriter);
		// 2. save rotation images
		// saveImage(img_rotate_90, imgName, img_mask_rotate_90, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_rotate_90_", fileWriter);
		// saveImage(img_rotate_180, imgName, img_mask_rotate_180, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_rotate_180_", fileWriter);
		// saveImage(img_rotate_270, imgName, img_mask_rotate_270, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_rotate_270_", fileWriter);
        // 3. save flip images
		// saveImage(img_flip_leftright, imgName, img_mask_flip_leftright, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_flip_leftright_", fileWriter);
		// saveImage(img_flip_topbottom, imgName, img_mask_flip_topbottom, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_flip_topbottom_", fileWriter);

	}
	
	public void writeAnalyze(ModelImage image, ModelImage mask, 
			ModelImage img_rotate_90, ModelImage img_mask_rotate_90, 
			ModelImage img_rotate_180, ModelImage img_mask_rotate_180,
			ModelImage img_rotate_270, ModelImage img_mask_rotate_270,
			ModelImage img_flip_leftright, ModelImage img_mask_flip_leftright,
			ModelImage img_flip_topbottom, ModelImage img_mask_flip_topbottom,
			int x, int y, int z, int imageIndex, PrintWriter fileWriter) {
		
		String sliceDirTrainImage = saveImageDirectory;
		File sliceDirFile = new File(sliceDirTrainImage);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		sliceDirTrainImage += "trainImage" + File.separator;
		File dir = new File(sliceDirTrainImage);
		if (!dir.isDirectory()) {
			dir.mkdir();
		}

		String sliceDirTrainMask = saveImageDirectory;
		File sliceDirFileMask = new File(sliceDirTrainMask);
		if (!sliceDirFileMask.isDirectory())
			sliceDirFileMask.mkdir();
		sliceDirTrainMask += "trainMask" + File.separator;
		File dirMask = new File(sliceDirTrainMask);
		if (!dirMask.isDirectory()) {
			dirMask.mkdir();
		}

		String coordinate_label = "_" + x + "_" + y + "_" + z + "_";
		String imgName = keyImageVector1.get(imageIndex);
	    String maskName = keyImageVector2.get(imageIndex);
	    // 1. save image and corresponding masks
		saveImage(image, imgName, mask, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label, fileWriter);
		// 2. save rotation images
		saveImage(img_rotate_90, imgName, img_mask_rotate_90, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_rotate_90_", fileWriter);
		saveImage(img_rotate_180, imgName, img_mask_rotate_180, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_rotate_180_", fileWriter);
		saveImage(img_rotate_270, imgName, img_mask_rotate_270, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_rotate_270_", fileWriter);
        // 3. save flip images
		saveImage(img_flip_leftright, imgName, img_mask_flip_leftright, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_flip_leftright_", fileWriter);
		saveImage(img_flip_topbottom, imgName, img_mask_flip_topbottom, maskName, sliceDirTrainImage, sliceDirTrainMask, coordinate_label+"_flip_topbottom_", fileWriter);

	}
	

	public void saveImage(ModelImage image, String imgName, ModelImage mask, String maskName, 
			      String sliceDirTrainImage, String sliceDirTrainMask, 
			      String coordinate_label, PrintWriter fileWriter) {
		
		int start = imgName.lastIndexOf(File.separator) + 1;
		int end = imgName.lastIndexOf(".");
		String imgFileName = imgName.substring(start, end) + coordinate_label + ".img";
		String imageFileName = sliceDirTrainImage + imgFileName;
		image.saveImage(sliceDirTrainImage, imgFileName, FileUtility.ANALYZE, false);
	    imgFileName = imgName.substring(start, end) + coordinate_label + ".hdr";
		imageFileName = sliceDirTrainImage + imgFileName;
		
		start = maskName.lastIndexOf(File.separator) + 1;
		end = maskName.lastIndexOf(".");
		String maskFName = maskName.substring(start, end)+ coordinate_label +".img";
		String maskFileName = sliceDirTrainMask + maskFName;
		mask.saveImage(sliceDirTrainMask, maskFName, FileUtility.ANALYZE, false);	
	    maskFName = maskName.substring(start, end)+ coordinate_label +".hdr";
		maskFileName = sliceDirTrainMask + maskFName;
	    
		System.err.println(imageFileName + " " + maskFileName + "\n");
		fileWriter.write(imageFileName + " " + maskFileName + "\n");
		
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
		oZres = 1.5f;

		// oXdim = keyImageN4.getFileInfo()[0].getExtents()[0];
		// oYdim = keyImageN4.getFileInfo()[0].getExtents()[1];
		// oZdim = keyImageN4.getFileInfo()[0].getExtents()[2];

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
		resultImage.calcMinMax();

		algoTrans.disposeLocal();
		algoTrans = null;
		return resultImage;
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
				// read key image

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