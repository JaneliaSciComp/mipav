package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
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

/**
 * This is the third attempt to apply prostate segmentation on Promise 12 data. 
 * steps:
 * 1. read the promise 12 data (image and masks) directly. ( without N4 correction ). 
 * 2. run CED to MRI image. 
 * 3. Transform both CED and MRI images to isotropic resolution(x, y resolution) images
 * 3. converts the MRI and CED slices into png file format.  
 * 
 * For testing image only. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesAtlasPngConverterMICCAI_ced_scale_test extends JDialogBase
		implements AlgorithmInterface {

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
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();
	private Vector<ModelImage> cedImages = new Vector<ModelImage>();
	private Vector<ModelImage> transKeyImages = new Vector<ModelImage>();
	private Vector<ModelImage> transCEDImages = new Vector<ModelImage>();
	private Vector<ModelImage> transVOIImages = new Vector<ModelImage>();

	private Vector<String> keyImageVector1 = new Vector<String>();
	
	/** cropped key image vector. */
	private Vector<ModelImage> cropKeyImages = new Vector<ModelImage>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;
    private AlgorithmTransform algoTrans;
	
    /**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesAtlasPngConverterMICCAI_ced_scale_test(Frame theParentFrame) {
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
			// MipavUtil.showHelp("Haral1001");
		} else if (command.equals("ChooseKeyImageDir")) {
			readKeyImageDir();
			sortKeyImage_1();
		    
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
		
	    // File fileDir_1 = new File("/data/ruida/MICAI2012/train");
		File fileDir_1 = new File("/data/ruida/MICAI2012/test");
		traverse_folder_1(fileDir_1); 
	    
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
			index = Integer.parseInt(imageName.substring(start+4, end));
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

		//cropKeyImages();
		runCED();
		runTransform();
		System.err.println("saveImage");
		// saveImagesTrain();

		
	    saveImagesTest();
		disposeLocal();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	public void runCED() {
	    
		for (int i = 0; i < keyImages.size(); i++) {
			try {
				ModelImage keyImage = keyImages.get(i);
				cedImages.add(calculateCoherenceEnhancingDiffusion(keyImage));
			} catch ( OutOfMemoryError e ) {
				e.printStackTrace();
			}
		}
	}
	
	public void runTransform() {
		
		for (int i = 0; i < keyImages.size(); i++) {
			try {
				ModelImage keyImage = keyImages.get(i);
				transKeyImages.add(calculateTransform(keyImage));
			} catch ( OutOfMemoryError e ) {
				e.printStackTrace();
			}
		}
		for (int i = 0; i < cedImages.size(); i++) {
			try {
				ModelImage cedImage = cedImages.get(i);
				transCEDImages.add(calculateTransform(cedImage));
			} catch ( OutOfMemoryError e ) {
				e.printStackTrace();
			}
		}
		
        
	}
	
	public ModelImage calculateTransform(ModelImage image) {
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
        float factor, fov;
        float userValue;
        int constantFOV = 1;
        
        factor = 1.f;
        
        dims[0] = image.getFileInfo()[0].getExtents()[0];
        dims[1] = image.getFileInfo()[0].getExtents()[1];
        dims[2] = image.getFileInfo()[0].getExtents()[2];   
        resols[0] = image.getFileInfo()[0].getResolutions()[0];
        resols[1] = image.getFileInfo()[0].getResolutions()[1];
        resols[2] = image.getFileInfo()[0].getResolutions()[2];
		
        doVOI = false;
        doClip = true;
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
        
        oXdim = 256;
        oYdim = 256;
        oZdim = (int)dims[2];
        
        oZres = resols[2];
        
        userValue = oXdim;
        factor = (userValue - constantFOV ) / ( dims[0] - constantFOV );
        fov = (dims[0] - constantFOV ) * resols[0];
        oXres = fov / ( userValue - constantFOV );
       
        dims[1] = ( dims[1] = constantFOV ) * factor + constantFOV;
        oYres = resols[1] / factor;
        
        System.err.println("oXres = " + oXres + " oYres = " + oYres);
        
		algoTrans = new AlgorithmTransform(image, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim, units,
                doVOI, doClip, doPad, doRotateCenter, center);
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
		numIterations = 50;
		do25D = true;
		entireImage = true;
		ModelImage coherenceEnhancingDiffusionImage;

		try {
			coherenceEnhancingDiffusionImage = (ModelImage) inImage.clone();
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

	
	/**
	 * Save the 2D slices and VOIs to user specified dir.
	 */
	public void saveImagesTrain() {

        int index = 0;		
		for (int i = 0; i < transKeyImages.size(); i++) {
			try {

				ModelImage keyImage = transKeyImages.get(i);
                ModelImage keyImageVOI = transVOIImages.get(i);
                
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

						System.err.println(" image number = " + (0 + i) + "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
						float[] targetBuffer = new float[size];
						keyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// 1) save image
						// String sliceDir = saveImageDirectory + File.separator + i + File.separator;
						String sliceDir = saveImageDirectory;
						File sliceDirFile = new File(sliceDir);
						if (!sliceDirFile.isDirectory())
							sliceDirFile.mkdir();

						System.err.println("index = " + index);

						String imgName = "image_" + index + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						
						// Save mask image
						ModelImage maskImage = new ModelImage(ModelStorageBase.INTEGER, newExtents, "voi" + j);
						int[] voiBuffer = new int[size];
						keyImageVOI.exportData(j * size, size, voiBuffer);
						maskImage.importData(0, voiBuffer, true);
						
						String maskName = "voi_" + index + ".png";
						savePNGfile(sliceDir, maskName, maskImage, 0, 1, xDim, yDim, true);

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
		
		for (int i = 0; i < transCEDImages.size(); i++) {
			try {

				ModelImage keyImage = transCEDImages.get(i);
                ModelImage keyImageVOI = transVOIImages.get(i);
                
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

						System.err.println(" image number = " + (0 + i) + "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
						float[] targetBuffer = new float[size];
						keyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// 1) save image
						// String sliceDir = saveImageDirectory + File.separator + i + File.separator;
						String sliceDir = saveImageDirectory;
						File sliceDirFile = new File(sliceDir);
						if (!sliceDirFile.isDirectory())
							sliceDirFile.mkdir();

						System.err.println("index = " + index);

						String imgName = "image_" + index + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						
						// Save mask image
						ModelImage maskImage = new ModelImage(ModelStorageBase.INTEGER, newExtents, "voi" + j);
						int[] voiBuffer = new int[size];
						keyImageVOI.exportData(j * size, size, voiBuffer);
						maskImage.importData(0, voiBuffer, true);
						
						String maskName = "voi_" + index + ".png";
						savePNGfile(sliceDir, maskName, maskImage, 0, 1, xDim, yDim, true);

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

        int index = 0;		
		for (int i = 0; i < transKeyImages.size(); i++) {
			try {

				ModelImage keyImage = transKeyImages.get(i);
				ModelImage cedImage = transCEDImages.get(i);
                
				int xDim = keyImage.getExtents()[0];
				int yDim = keyImage.getExtents()[1];
				int zDim = keyImage.getExtents()[2];

				int size_3D = xDim * yDim * zDim;
				float[] imageBuffer = new float[size_3D];
				float[] cedImageBuffer = new float[size_3D];
				
				try {
					keyImage.exportData(0, imageBuffer.length, imageBuffer);
					cedImage.exportData(0, cedImageBuffer.length, cedImageBuffer);
				} catch (Exception e) {
					e.printStackTrace();
				}

				float percentile_left = 0.1f;
				float percentile_right = 0.1f;

				int minIndex = (int) (size_3D * percentile_left);
				int maxIndex = (int) (size_3D * (1.0 - percentile_right));

				Arrays.sort(imageBuffer);
				Arrays.sort(cedImageBuffer);
				
				float min = imageBuffer[minIndex];
				float max = imageBuffer[maxIndex];

				float minCED = cedImageBuffer[minIndex];
				float maxCED = cedImageBuffer[maxIndex];
				
				int size = xDim * yDim;

				int[] newExtents = new int[2];
				newExtents[0] = xDim;
				newExtents[1] = yDim;

				for (int j = 0; j < zDim; j++) {

					try {

						System.err.println(" image number = " + (0 + i) + "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
						float[] targetBuffer = new float[size];
						keyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// 1) save image
						String sliceDir = saveImageDirectory + File.separator + i + File.separator;
						// String sliceDir = saveImageDirectory;
						File sliceDirFile = new File(sliceDir);
						if (!sliceDirFile.isDirectory())
							sliceDirFile.mkdir();

						System.err.println("index = " + j);

						String imgName = "image_" + j + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						index++;

					} catch (IOException e) {

					}
				}
				
				for (int j = 0; j < zDim; j++) {

					try {

						System.err.println(" image number = " + (0 + i) + "   slice number = " + (j+zDim));

						ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + (j+index));
						float[] targetBuffer = new float[size];
						cedImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// 1) save image
						String sliceDir = saveImageDirectory + File.separator + i + File.separator;
						// String sliceDir = saveImageDirectory;
						File sliceDirFile = new File(sliceDir);
						if (!sliceDirFile.isDirectory())
							sliceDirFile.mkdir();

						System.err.println("index = " + (j + zDim));

						String imgName = "image_" + (j + zDim) + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, minCED, maxCED, xDim, yDim, false);

						// index++;

					} catch (IOException e) {

					}
				}

			} catch (OutOfMemoryError x) {
				MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			}
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
			int yMin = 0, yMax = xDim;
			int xMin = 0, xMax = yDim;
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
			// System.err.println("testing array");

		} catch (Exception e) {
			System.err.println("image find wrong : " + file.getAbsolutePath());
			e.printStackTrace();

			System.exit(0);
		}
	}
	

	/**
	 * Crop key images.
	 */
	public void cropKeyImages() {

		int zDim;

		// Crop key images. VOIs
		for (int i = 0; i < keyImages.size(); i++) {

			ModelImage image = keyImages.get(i);

			int[] extents = (int[]) image.getFileInfo(0).getExtents();
			zDim = extents[2] - 1;

			// manually set the crop image starting point and ending point
			boxYmin = 0;
			boxYmax = 512 - 1;

			boxXmin = 0;
			boxXmax = 512 - 1;

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

		// crop target image

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
			int start = 0;
			int len = keyImageVector1.size();
			
			int currentIndex = 0;
			
			
			for (int imageIndex = start; imageIndex < len; imageIndex++) {
				// read key image
				String dir = keyImageVector1.get(imageIndex);
				
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex, keyImageIO.readImage(fileName, directory));
				keyImages.get(currentIndex).calcMinMax();
				
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