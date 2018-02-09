package gov.nih.mipav.view.renderer.WildMagic.Knees;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;


/**
 * This class generates the knees VOI contours from the deep learning HED MRI and CED energy maps. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogKneesMapFromMRIandCED extends JDialogBase
		implements AlgorithmInterface {

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

	/** voi vector to hold the actual vois. */
	private Vector<VOI[]> keyImageVOIs = new Vector<VOI[]>();

	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();

	private Vector<String> keyImageVector2 = new Vector<String>();
	private Vector<String> keyImageVOIVector2 = new Vector<String>();

	private Vector<String> keyImageVector3 = new Vector<String>();
	private Vector<String> keyImageVOIVector3 = new Vector<String>();

	private Vector<String> keyImageVector4 = new Vector<String>();
	private Vector<String> keyImageVOIVector4 = new Vector<String>();

	private Vector<String> keyImageVector5 = new Vector<String>();
	private Vector<String> keyImageVOIVector5 = new Vector<String>();

	
	/** cropped key image vector. */
	private Vector<ModelImage> cropKeyImages = new Vector<ModelImage>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<Integer, Vector<String>> imageTable = new Hashtable<Integer, Vector<String>>();
	Hashtable<Integer, Vector<String>> newImageTable = new Hashtable<Integer, Vector<String>>();
	Hashtable<Integer, Vector<ModelImage>> maskTable = new Hashtable<Integer, Vector<ModelImage>>();
	
	Hashtable<Integer, Vector<String>> maskImageTable = new Hashtable<Integer, Vector<String>>();
	
	Hashtable<Integer, ModelImage> imageHashtable = new Hashtable<Integer, ModelImage>(); 
	Hashtable<String, VOI> voiHashtable = new Hashtable<String, VOI>(); 
	Hashtable<String, Vector<String>> imageNameHashtable = new Hashtable<String, Vector<String>>(); 
	
	Hashtable<String, String> nameTableImages = new Hashtable<String, String>();
	Hashtable<String, ModelImage> keyImagesOrientation = new Hashtable<String, ModelImage>();
	
	int startIndex = 0;
	int endIndex = 86;
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogKneesMapFromMRIandCED(Frame theParentFrame) {
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
		/*
		for (i = 0; i < keyImageVector.size(); i++) {
			String temp = keyImageVector.get(i);
			temp = null;
		}
		keyImageVector = null;
        */ 
		
		for (i = 0; i < keyImages.size(); i++) {
			ModelImage temp = keyImages.get(i);
			temp.disposeLocal();
		}
		keyImages = null;

		/*
		for (i = 0; i < keyImageVOIVector.size(); i++) {
			String temp = keyImageVOIVector.get(i);
			temp = null;
		}
		keyImageVOIVector = null;
        */ 
		
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
			sortImageTable();
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	private void sortImageTable() {
		
		for (int i = startIndex; i <= endIndex; i++  ) {
				Vector<String> stringVec = imageTable.get(i);
				Vector<String> newStringVect = new Vector<String>();
				Hashtable<Integer, String> hash = new Hashtable<Integer, String>(); 
				for ( int j = 0; j < stringVec.size(); j++ ) {
					String fullName = stringVec.get(j);
					int stopIndex = fullName.lastIndexOf("-");
					String subName = fullName.substring(0, stopIndex-1);
					stopIndex = subName.lastIndexOf("-");
					int startIdx = fullName.lastIndexOf("_");
					String numString = fullName.substring(startIdx+1, stopIndex);
					
					int num = Integer.valueOf(numString);
					
					hash.put(num, fullName);
				}
				
				for ( int j = 0; j < 1000; j++ ) {
					String name = hash.get(j);
					if ( name != null ) {
						newStringVect.add(name);
						System.err.println("name = " + name);
					}
				}
				System.err.println("test i = " + i);
				maskImageTable.put(i, newStringVect);
		}
		// pause();
		System.err.println(" finish inti read dir");
		
	}
	
	public static void pause() {
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
		
		// File fileDir_1 = new File("/data/ruida/MICAI2012/test");
		// traverse_folder_1(fileDir_1); 
		// File fileDir_1 = new File("/scratch/backup/SPIEChallenge/Training/Sorted/");
		// File fileDir_1 = new File("/scratch/backup/SPIEChallenge/Training/50cases/");
		// File fileDir_1 = new File("/scratch/backup/SPIEChallenge/Testing/Sorted/");
		// File fileDir_1 = new File("/scratch/aam_test/fold3/");
		// traverse_folder_1(fileDir_1); 
		// File fileDir_2 = new File("/home/ruida/kneesBackup/downSampling/sagittal/");
		File fileDir_image = new File("/home/ruida/kneesBackup/downSample/sagittal/");
		traverse_image_folder(fileDir_image, null);
		
		// File fileDir_5_test = new File("/scratch/backup/SPIEChallenge/NetherlandTrainDataPng/cg_ced_150_mask/");
		// File fileDir_5_test = new File("/scratch/backup/SPIEChallenge/NetherlandTrainDataPng/cg_ced_50cases_mask/");
		// File fileDir_5_test = new File("/scratch/backup/SPIEChallenge/NetherlandTrainDataPng/spie_2017_final_wp_train_mask/");
		// File fileDir_5_test = new File("/data/ruida/JMI_2017/resultMask/fold3/");
		File fileDir_map = new File("/data/ruida/knees/predictedMap/sagittal/data/");
		traverse_folder_map(fileDir_map);
	}
	
	private void traverse_image_folder(File dir, String hashID) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_image_name(new File(dir, children[i]));
			}
		}

	}

	private void read_image_name(File dir) {

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
			nameTableImages.put(hashID, dirName);
		}


	}

	
	private void traverse_folder_map(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_map(new File(dir, children[i]), i);
			}
		}
	}
	
	private void traverse_folder_map(File dir, int index) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder_map(new File(dir, children[i]), index);
			}
		}

	}

	private void processDir_folder_map(File dir, int index) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".png")
				&& dirName.contains("fuse")) {
			System.err.println("file = " + dir.toString());
			String fullName = dir.toString();
			int idxEnd = fullName.lastIndexOf("/");
			String subName = fullName.substring(0, idxEnd);
			int idxStart = subName.lastIndexOf("/");
			String numString = fullName.substring(idxStart+1, idxEnd);
			int hashIndex = Integer.valueOf(numString);
			// System.err.println(numString);
			if ( imageTable.get(hashIndex) == null ) {
				imageTable.put(hashIndex, new Vector<String>());
			}
		    imageTable.get(hashIndex).add(dir.toString());
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

		System.err.println("saveImage");
	
		generateContours();
	    
		// disposeLocal();
        
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	private void generateContours() {
		
		Set<String> keys = keyImagesOrientation.keySet();
		
		int i = 0;
		
		for (String key : keys) {
		
			System.err.println("key = " + key + "   i = " + i);
			
			if ( i >= startIndex && i <= endIndex ) {
				
				Vector<ModelImage> imageVec = maskTable.get(i);

				ModelImage srcImage = keyImagesOrientation.get(key);
				float[] srcResol = srcImage.getResolutions(0);
				
				int[] units = srcImage.getUnitsOfMeasure();


				String directory = saveImageDirectory + File.separator + i + File.separator;

				File dirFile = new File(directory);
				if (!dirFile.isDirectory()) {
					dirFile.mkdir();
				}

				VOI voiNewFinal = new VOI((short) 0, "voi_result_" + i);
				int[] extents = new int[2];
				int[] srcExtents = srcImage.getExtents();

				int[] blankExts = new int[2];
				blankExts[0] = srcExtents[0];
				blankExts[1] = srcExtents[1];


				int imageDim = srcExtents[2];

				for (int j = 0; j < imageDim; j++) {

					ModelImage image = imageVec.get(j);
					ModelImage cedImage = imageVec.get(j + imageDim);

					ModelImage blankImage = new ModelImage(image.getType(), blankExts, "_blank");
					int[] imageExt = image.getExtents();
					int imageSize = imageExt[0] * imageExt[1];
					int[] maskBuffer = new int[imageSize];

					try {
						image.exportData(0, imageSize, maskBuffer);
						
						blankImage.importData(0, maskBuffer, true);
						FileInfoBase fileInfo = blankImage.getFileInfo()[0];
						fileInfo.setResolutions(srcResol);
						fileInfo.setUnitsOfMeasure(units);
					} catch (IOException e) {
						e.printStackTrace();
					}

					ModelImage blankImage_ced = new ModelImage(image.getType(), blankExts, "_blank_ced");
					int[] imageExt_ced = cedImage.getExtents();
					int imageSize_ced = imageExt_ced[0] * imageExt_ced[1];
					int[] maskBuffer_ced = new int[imageSize_ced];

					try {
						
						cedImage.exportData(0, imageSize_ced, maskBuffer_ced);
						blankImage_ced.importData(0, maskBuffer_ced, true);
						FileInfoBase fileInfo_ced = blankImage_ced.getFileInfo()[0];
						fileInfo_ced.setResolutions(srcResol);
						fileInfo_ced.setUnitsOfMeasure(units);
					} catch (IOException e) {
						e.printStackTrace();
					}

					ModelImage maxMaskImage = new ModelImage(blankImage.getType(), blankImage.getExtents(), "_calc");
					int opType = AlgorithmImageCalculator.MAXIMUM;
					int clipMode = 0;
					String adOpString = null;
					AlgorithmImageCalculator mathAlgo = new AlgorithmImageCalculator(maxMaskImage, blankImage, blankImage_ced, opType, clipMode, true, adOpString);
					mathAlgo.run();

					extents = blankImage.getExtents();
					String name = "binaryMask_" + blankImage.getImageName();
					
					ModelImage resultImage = new ModelImage(ModelImage.BOOLEAN, extents, name);
					float[] thresholds = new float[2];
					thresholds[0] = 240;
					thresholds[1] = 255;
					float fillValue = 0f;
					boolean isInverse = false;
					boolean regionFlag = true;
					int outputType = 1;

					AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(resultImage, maxMaskImage, thresholds, fillValue, outputType, regionFlag, isInverse);
					thresholdAlgo.run();

					boolean wholeImage = true;

					AlgorithmMorphology2D idObjectsAlgo2D;
					int method = AlgorithmMorphology2D.ID_OBJECTS;

					idObjectsAlgo2D = new AlgorithmMorphology2D(resultImage, 0, 0, method, 0, 0, 0, 0, wholeImage);
					idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
					idObjectsAlgo2D.run();
					idObjectsAlgo2D.finalize();
					idObjectsAlgo2D = null;

					resultImage.calcMinMax();
					AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(resultImage);
					VOIExtractionAlgo.run();

					VOIVector v = resultImage.getVOIs();
					if (v.size() == 0)
						continue;

					if (resultImage.getVOIs() != null) {

						for (int idx = 0; idx < resultImage.getVOIs().size(); idx++) {
							VOIBaseVector current_va = resultImage.getVOIs().VOIAt(idx).getCurves();
							VOIBase current_v = current_va.get(0);
							VOIBase vTemp = (VOIBase) current_v.clone();
							int nPtsCurrent = current_v.size();


							float[] xPts = new float[nPtsCurrent];
							float[] yPts = new float[nPtsCurrent];
							float[] zPts = new float[nPtsCurrent];

							current_v.exportArrays(xPts, yPts, zPts);

							for (int k = 0; k < nPtsCurrent; k++) {
								zPts[k] = j;
							}

							vTemp.importArrays(xPts, yPts, zPts, nPtsCurrent);
							voiNewFinal.importCurve(vTemp);
						}
						
					}

					
					// thresholdAlgo.finalize();
					thresholdAlgo = null;
					// idObjectsAlgo2D.finalize();
					// idObjectsAlgo2D = null;
					
					// VOIExtractionAlgo.finalize();
					VOIExtractionAlgo = null;
					
					maskBuffer = null;
					// maxMaskImage.disposeLocal(false);
					maxMaskImage = null;
					maskBuffer_ced = null;
					maskBuffer = null;
					// blankImage.disposeLocal(false);
					blankImage = null;
					// blankImage_ced.disposeLocal(false);
					blankImage_ced = null;
					
					// mathAlgo.finalize();
					mathAlgo = null;
					
					// resultImage.disposeLocal(false);
					resultImage = null;
					
				} // end for j loop

				voiNewFinal.update();
				// save VOI contours

				int[] newExtents = new int[3];
				newExtents[0] = extents[0];
				newExtents[1] = extents[1];
				newExtents[2] = imageVec.size();

				srcImage.registerVOI(voiNewFinal);

				try {

					System.err.println("directory = " + directory);

					FileVOI fileVOI = new FileVOI("voi_" + i + ".xml",directory, srcImage);
					fileVOI.writeVOI(voiNewFinal, true);

					// srcImage.saveImage(directory, "image_" + i + ".xml", FileUtility.XML, false);
					System.err.println("source image name = " + srcImage.getImageName());
					srcImage.saveImage(directory,srcImage.getImageName() , FileUtility.XML, false);
					srcImage.disposeLocal(false);
					srcImage = null;
					fileVOI.finalize();
					fileVOI = null;

				} catch (IOException e) {
					e.printStackTrace();
				}

				imageVec.clear();
				imageVec = null;
				newExtents = null;
				System.err.println("done dispose");
			
			}	
			i++;
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

		try {
			
			int index;
		
			Set<String> keys = nameTableImages.keySet();

			for (String key : keys) {
				System.err.println("coronal key = " + key);
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
				if (image != null) {
					
					keyImagesOrientation.put(key, image);
					keyImagesOrientation.get(key).setImageName(key);
					
				}
				keyImageIO = null;
			}
	        
			
	        for(int i = startIndex; i <= endIndex; i++ ){
	        	  
					Vector<String> stringVec = maskImageTable.get(i);
					Vector<ModelImage> imageVec = new Vector<ModelImage>();

					for (int j = 0; j < stringVec.size(); j++) {
						String dir = stringVec.get(j);
						index = dir.lastIndexOf(File.separator);
						String directory = new String(dir.substring(0, index + 1));
						String fileName = new String(dir.substring(index + 1, dir.length()));
						System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
						FileIO keyImageIO = new FileIO();
						imageVec.add(keyImageIO.readImage(fileName, directory));
					}
					System.err.println("i = " + i);
					maskTable.put(i, imageVec);
        	   
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