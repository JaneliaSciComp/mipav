package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;


/**
 * This is the first try to 3D convolution deep learning models. 3D-Unet.
 * 
 * Miccai Promise 12 data: 
 * Preprocessed after,
 * 1. N4 correction
 * 2. Transform to 0.5m x 0.5m x 1.5m resolution images
 * 3. Intensity normalization
 * 
 * Convert the image to double type. 
 * Save the images and masks in .nii.gz format. 
 * 
 * Apply 3D cube cropping to the 3D images.   The 3D cube size is 64x64x64. 
 * The MR image and corresponding binary mask images need to crop into two 3D small cubes, then merge
 * them into H5 readable volume. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogPromise12_mhg_to_nii extends JDialogBase implements
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
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogPromise12_mhg_to_nii(Frame theParentFrame) {
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

		File fileDir_1 = new File("/timed/miccai/Preprocessing/N4TrainCropScale");
		File fileDir_2 = new File("/timed/miccai/Preprocessing/N4TrainMaskCropScale");
		
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

		saveNiiImages();
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	public void saveNiiImages() {

		int size = keyImages.size();
		
        // try {
        	
        	String sliceDir = saveImageDirectory;
			File sliceDirFile = new File(sliceDir);
			if (!sliceDirFile.isDirectory())
				sliceDirFile.mkdir();
			sliceDir += File.separator + "nii" + File.separator;
			File dir = new File(sliceDir);
			if (!dir.isDirectory()) {
				dir.mkdir();
			}
			
				for (int imageIndex = 0; imageIndex < size; imageIndex++) {
					
				    
						ModelImage keyImage = keyImages.get(imageIndex);
						ModelImage keyImageMask = keyImagesMask.get(imageIndex);
					
						String imgName = keyImageVector1.get(imageIndex);
						System.err.println("imgName = " + imgName);
						int start = imgName.lastIndexOf(File.separator) + 1;
						int end = imgName.lastIndexOf(".");
						String numStr = imgName.substring(start+4, end);
						String nameStr = imgName.substring(start, start+4);
						int num = Integer.parseInt(numStr);
						System.err.println("name = " + nameStr + " num = " + num);
						imgName = nameStr + num + ".nii.gz";	
						
					    System.err.println(sliceDir + imgName);
					    keyImage.saveImage(sliceDir, imgName, FileUtility.NIFTI, false);
						
						
					    imgName = keyImageVector2.get(imageIndex);
						System.err.println("imgName = " + imgName);
						start = imgName.lastIndexOf(File.separator) + 1;
						end = imgName.lastIndexOf("_");
						numStr = imgName.substring(start+4, end);
						nameStr = imgName.substring(start, start+4);
						num = Integer.parseInt(numStr);
						System.err.println("name = " + nameStr + " num = " + num);
						imgName = nameStr + num + "-label.nii.gz";	
						
					    System.err.println(sliceDir + imgName);
					    keyImageMask.saveImage(sliceDir, imgName, FileUtility.NIFTI, false);
					    
					
						keyImage = null;
						keyImageMask = null;
						
						// new ViewJFrameImage(keyImageVOIsTransformN4.get(imageIndex));
						System.err.println("imageIndex = " + imageIndex);
						// break;
				   
				}
		
				System.err.println("text write finish");
		// } catch (IOException e) {
		// 	e.printStackTrace();
		// }

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
				ModelImage image = keyImageIO.readImage(fileName, directory);
				AlgorithmChangeType changeZ = new AlgorithmChangeType(image, ModelImage.DOUBLE,
						image.getMin(), image.getMax(), image.getMin(), image.getMax(), false);
				changeZ.run();
				keyImages.add(currentIndex, image);
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
				ModelImage image = keyImageIO.readImage(fileName, directory);
				AlgorithmChangeType changeZ = new AlgorithmChangeType(image, ModelImage.DOUBLE,
						image.getMin(), image.getMax(), image.getMin(), image.getMax(), false);
				changeZ.run();
				keyImagesMask.add(currentIndex, image);

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