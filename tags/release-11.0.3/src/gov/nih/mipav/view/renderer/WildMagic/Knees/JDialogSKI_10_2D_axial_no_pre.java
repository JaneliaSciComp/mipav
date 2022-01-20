package gov.nih.mipav.view.renderer.WildMagic.Knees;

import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import ar.com.hjg.pngj.*;


/*
 * This file reads the SKI10 dataset, converts the 3D images and corresponding masks into 2D
 * png slices -- 10 folds cross-validation.  
 * 
 * The image preprocessing steps: intensity normalization, CED filter, isotropic sampling are all 
 * omitted.  The exiperment is one extra ablation study in the MRM paper.  
 * 
 * SKI10 dataset is given actually in sagittal view, however, the image orientation attribute 
 * of the SKI 10 dataset is axial.  We have to bare with it.    
 * 
 * @author Ruida Cheng
 */
public class JDialogSKI_10_2D_axial_no_pre extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;
	private JFileChooser readImageChooser = new JFileChooser();
	private String readImageDirectory;
	
	private JPanel imageSelectionPanel;

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	
	private Hashtable<String, String> nameTableImages = new Hashtable<String, String>();
	private Hashtable<String, String> nameTableImagesCED = new Hashtable<String, String>();
	private Hashtable<String, String> nameTableImagesMask = new Hashtable<String, String>();

	private Hashtable<String, ModelImage> keyImagesOrientation = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> keyImagesOrientationMask = new Hashtable<String, ModelImage>();

	
	// large data processing occupy fair large amount of memory. 
	// 1) Specify which orientation to run. 
	// 2) set the corresponding index
	private int axial_index = 0;
	private int sagittal_index = 0;
	private int coronal_index = 0;
	
	private String saveImageDirectory_train;
	private String saveImageDirectory_test;
	
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogSKI_10_2D_axial_no_pre(Frame theParentFrame) {
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
			System.err.println("saveImageDirectory = " + saveImageDirectory);

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

		// dispose();
		// System.gc();
	}

	/**
	 * Driver function to read image and VOIs, and convert each 3D image to 2D
	 * slices.
	 */
	public void callAlgorithm() {

		long startTime = System.currentTimeMillis();

		readFile();

		System.err.println("saveImage");
       
		// save training 2D png slices
		saveMRI2Dslice();
		
		// save testing 2D png slices
		saveMRI2Dslice_test();
		
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.err.println("sagittal_index = " + sagittal_index);
		
		// System.gc();

	}


	public void saveMRI2Dslice_test() {

		Set<String> keys = keyImagesOrientation.keySet();

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
			
				ModelImage axialImage = keyImagesOrientation.get(key);
				
				if (count >= 0 && count <= 10) {	
					saveImageSlice_test(axialImage, "sagittal", "fold1", key);
				} else if (count >= 11 && count <= 20 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold2", key);	
				} else if (count >= 21 && count <= 30 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold3", key);	
				} else if (count >= 31 && count <= 40 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold4", key);	
				} else if (count >= 41 && count <= 50 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold5", key);	
				} else if (count >= 51 && count <= 60 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold6", key);	
				} else if (count >= 61 && count <= 70 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold7", key);	
				} else if (count >= 71 && count <= 80 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold8", key);	
				} else if (count >= 81 && count <= 90 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold9", key);	
				} else if (count >= 91 ) {
					saveImageSlice_test(axialImage, "sagittal", "fold10", key);	
				}
				
				axialImage = null;
			
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
	
	
    /*
     * Save ski10 dataset 3D images into png 2D slices. 
     */
	public void saveMRI2Dslice() {

		Set<String> keys = keyImagesOrientation.keySet();

		int count = 0;
		
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

			System.err.println("save HED slice key = " + key);
			
				ModelImage axialImage = keyImagesOrientation.get(key);
				ModelImage axialImageMask = keyImagesOrientationMask.get(key);
				
				if (count >= 0 && count <= 10) {	
				    saveImage(axialImage, axialImageMask, "sagittal", "fold1");
				} else if (count >= 11 && count <= 20 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold2");	
				} else if (count >= 21 && count <= 30 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold3");	
				} else if (count >= 31 && count <= 40 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold4");	
				} else if (count >= 41 && count <= 50 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold5");	
				} else if (count >= 51 && count <= 60 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold6");	
				} else if (count >= 61 && count <= 70 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold7");	
				} else if (count >= 71 && count <= 80 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold8");	
				} else if (count >= 81 && count <= 90 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold9");	
				} else if (count >= 91 ) {
					saveImage(axialImage, axialImageMask, "sagittal", "fold10");	
				}
				
				axialImage = null;
				axialImageMask = null;

			
			count++;
		}

	}

	/**
	 * Save 3D image into 2D slice png files.  
	 * @param image  MRI source iamge
	 * @param imageMask   corresponding ground truth image
	 * @param orientation  orientation. 
	 * @param folder    indicate which fold in the 10-fold cross-validation. 
	 */
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