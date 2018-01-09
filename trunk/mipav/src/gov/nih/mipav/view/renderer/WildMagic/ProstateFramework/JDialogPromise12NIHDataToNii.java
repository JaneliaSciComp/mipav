package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
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
 * This is the first try to 3D convolution deep learning models. 3D-Unet.
 * 
 * Miccai Promise 12 NIH data: 
 * Preprocessed after,
 * 1. N4 correction
 * 2. Transform to 0.35m x 0.35m x 0.355m resolution images
 * 3. Intensity normalization
 * 
 * Convert the image to double type. 
 * Save the images and masks in .nii.gz format. 
 * 
 * Just convert the NIH data to nii format.   Those nii data will be used for 3D-Unet deep learning training. 
 * 
 * @author Ruida Cheng
 *
 */
public class JDialogPromise12NIHDataToNii extends JDialogBase implements
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
	private Hashtable<String, ModelImage> keyImages = new Hashtable<String, ModelImage>();
	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();


	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<String, ModelImage> cedTableAxial = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTableSagittal = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTableCoronal = new Hashtable<String, ModelImage>();

	Hashtable<String, String> origTable = new Hashtable<String, String>();
	Hashtable<String, String> nameTable = new Hashtable<String, String>();
	Hashtable<String, String> nameVOITable = new Hashtable<String, String>();
	
	
	Hashtable<String, String> nameTableImages = new Hashtable<String, String>();
	Hashtable<String, String> nameTableImagesCED = new Hashtable<String, String>();
	Hashtable<String, String> nameTableImagesMask = new Hashtable<String, String>();

	private Hashtable<String, ModelImage> keyImagesOrientation = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> keyImagesOrientationCED = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> keyImagesOrientationMask = new Hashtable<String, ModelImage>();

	private int axial_index = 0;
	private int sagittal_index = 0;
	private int coronal_index = 62496;
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogPromise12NIHDataToNii(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();

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
			// MipavUtil.showHelp("Haral1001");
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
		
		File fileDir_2 = new File("/timed/prostateXdata/trainImagesWp/121_146/axial");
		traverse_folder_2(fileDir_2, null);
		
	}

	private void traverse_folder_2(File dir, String hashID) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_name_2(new File(dir, children[i]));
			}
		}

	}

	private void read_name_2(File dir) {

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
 
		if (fileName.startsWith("image") && fileName.endsWith("xml") && 
				!fileName.contains("ced") && !fileName.contains("mask")) {
			System.err.println("hashID = " + hashID + " dirName = " + dirName);
			nameTableImages.put(hashID, dirName);
		}

		if (fileName.startsWith("image_ced") && fileName.endsWith("xml")) {
			System.err.println("hashID = " + hashID + " dirName = " + dirName);
			nameTableImagesCED.put(hashID, dirName);
		}
		
		if (fileName.startsWith("image_mask") && fileName.endsWith("xml")) {
			System.err.println("hashID = " + hashID + " dirName = " + dirName);
			nameTableImagesMask.put(hashID, dirName);
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

		// dispose();
		// System.gc();
	}

	/**
	 * Driver function to read image and VOIs, and convert each 3D image to 2D
	 * slices.
	 */
	public void callAlgorithm() {

		long startTime = System.currentTimeMillis();

		loadFiles();

		System.err.println("saveImage");
       
		saveHED2DsliceCED();

		// System.err.println("coronal_index = " + coronal_index);
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		// System.gc();

	}


	public void saveHED2DsliceCED() {

		Set<String> keys = keyImagesOrientation.keySet();
		
		String sliceDir = saveImageDirectory;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		sliceDir += File.separator;
		File dir = new File(sliceDir);
		if (!dir.isDirectory()) {
			dir.mkdir();
		}
		int count = 121;
		for (String key : keys) {

				ModelImage axialImage = keyImagesOrientation.get(key);
				ModelImage axialImageMask = keyImagesOrientationMask.get(key);
				ModelImage scaledAxialImage = scaleIntensity(axialImage);

				ModelImage keyImage = scaledAxialImage;
				ModelImage keyImageMask = axialImageMask;
			
				System.err.println(count + " => " + key);
				
				String imgName = "Case" + count + ".nii.gz";	
				keyImage.saveImage(sliceDir, imgName, FileUtility.NIFTI, false);
				
			    imgName = "Case" + count + "-label.nii.gz";	
				keyImageMask.saveImage(sliceDir, imgName, FileUtility.NIFTI, false);
				
			
				keyImage = null;
				keyImageMask = null;
				
				axialImage.disposeLocal();
				axialImage = null;

				scaledAxialImage.disposeLocal();
				scaledAxialImage = null;

				axialImageMask.disposeLocal();
				axialImageMask = null;
				
				count++;
			
		}
		System.err.println("count = " + count);

	}

	public void saveImage(ModelImage image, ModelImage imageMask, String orientation) {

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
	 * load image files and voi files
	 */
	public void loadFiles() {

		readFile();
		System.err.println("finish image I/O");

	}


	public void readFile() {

		int index;
		
		try {
			
			Set<String> keys = nameTableImages.keySet();

			for (String key : keys) {
				System.err.println("axial key = " + key);
				String dir = nameTableImages.get(key);
				if (dir == null)
					continue;
				// System.err.println("dir = " + dir);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				AlgorithmChangeType changeZ = new AlgorithmChangeType(image, ModelImage.DOUBLE,
						image.getMin(), image.getMax(), image.getMin(), image.getMax(), false);
				changeZ.run();
				
				if (image != null) {
					
					keyImagesOrientation.put(key, image);
					keyImagesOrientation.get(key).setImageName(key);
					
				}
				keyImageIO = null;
			}
			
			
			keys = nameTableImagesCED.keySet();

			/*
			for (String key : keys) {
				System.err.println("axial ced key = " + key);
				String dir = nameTableImagesCED.get(key);
				if (dir == null)
					continue;
				// System.err.println("dir = " + dir);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					
					keyImagesOrientationCED.put(key, image);
					keyImagesOrientationCED.get(key).setImageName(key);
					
				}
				keyImageIO.dispose();
				keyImageIO = null;
			}
			*/
			for (String key : keys) {
				System.err.println("axial mask key = " + key);
				String dir = nameTableImagesMask.get(key);
				if (dir == null)
					continue;
				// System.err.println("dir = " + dir);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				AlgorithmChangeType changeZ = new AlgorithmChangeType(image, ModelImage.DOUBLE,
						image.getMin(), image.getMax(), image.getMin(), image.getMax(), false);
				changeZ.run();
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