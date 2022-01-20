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
 * This class generates the knees VOI contours from the deep learning HED MRI maps. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogSK10_MRI_map_nopre extends JDialogBase
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
	
	/** cropped key image vector. */
	private Vector<ModelImage> cropKeyImages = new Vector<ModelImage>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	
	// mask table to hold the HNN prediction probability map full paths. 
	Hashtable<String, Vector<String>> imageTable = new Hashtable<String, Vector<String>>();
	Hashtable<String, Vector<String>> newImageTable = new Hashtable<String, Vector<String>>();
	
	// mask table to hold the HNN prediction probability map images. 
	Hashtable<String, Vector<ModelImage>> maskTable = new Hashtable<String, Vector<ModelImage>>();
	
	// mask table to hold the HNN prediction probability map full paths in sorted order.  
	Hashtable<String, Vector<String>> maskImageTable = new Hashtable<String, Vector<String>>();
	
	// image table to hold the testing images full path
	Hashtable<String, String> nameTableImages = new Hashtable<String, String>();
	
	// image table to hold the testing images. 
	Hashtable<String, ModelImage> keyImagesOrientation = new Hashtable<String, ModelImage>();
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogSK10_MRI_map_nopre(Frame theParentFrame) {
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

	
	/** 
	 * Sort the testing image directory in accending order. 
	 */
	private void sortImageTable() {
		
		Set<String> keys = imageTable.keySet();
		for (String key : keys ) {
				Vector<String> stringVec = imageTable.get(key);
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
				System.err.println("test key = " + key);
				maskImageTable.put(key, newStringVect);
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
	 * Parsing 3D images directory and the HNN generated probablity maps directory
	 */
	private void readKeyImageDir() {
		
		File fileDir_image = new File("/home/ruida/Knee_2010_challenge/train_data_no_pre/axial/");
		traverse_image_folder(fileDir_image, null);
		
		File fileDir_map = new File("/home/ruida/Knee_2010_challenge/test_out_no_pre/fold3/axial/");
		traverse_folder_map(fileDir_map);
	}
	
	
	/**
	 * Traverse the test image folder
	 */
	private void traverse_image_folder(File dir, String hashID) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_image_name(new File(dir, children[i]));
			}
		}

	}

	/**
	 * Grab the test image in mipav .xml file format.   
	 * @param dir  image sub-directory name
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
		begin = fileName.lastIndexOf("_") + 1;
		end = fileName.lastIndexOf(".");

		hashID = fileName.substring(begin, end);

		if (fileName.startsWith("image") && fileName.endsWith("xml") && 
				!fileName.contains("ced") && !fileName.contains("mask")) {
			// System.err.println("hashID = " + hashID + "   fileName = " + fileName );
			nameTableImages.put(hashID, dirName);
		}


	}

	/*
	 * Traverse the HNN generated probability map directory
	 */
	private void traverse_folder_map(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_map(new File(dir, children[i]), i);
			}
		}
	}
	
	/**
	 * Traverse the HNN subdirectories. 
	 * @param dir    directory name
	 * @param index    map index
	 */
	private void traverse_folder_map(File dir, int index) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder_map(new File(dir, children[i]), index);
			}
		}

	}

	/**
	 * HNN generates multi-scaled prediction output.  We use fuse layer generated output as the energy map
	 * to inference the VOI contour. 
	 * @param dir  sub-directory name. 
	 * @param index    probablity map index
	 */
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
			String hashIndex = numString;
			System.err.println(numString);
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

		readFile();

		System.err.println("saveImage");
	
		generateContours_MRI();
	    
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}
	
	/**
	 * Generate the VOI contours from MRI prediction only.   As one ablation experiment
	 * in the MRM paper. 
	 */
	private void generateContours_MRI() {
		
		Set<String> keys = maskTable.keySet();
		
		
		for (String key : keys) {
		
			System.err.println("key = " + key);
			
				
				Vector<ModelImage> imageVec = maskTable.get(key);

				ModelImage srcImage = keyImagesOrientation.get(key);
				float[] srcResol = srcImage.getResolutions(0);
				
				int[] units = srcImage.getUnitsOfMeasure();


				String directory = saveImageDirectory + File.separator + key + File.separator;

				File dirFile = new File(directory);
				if (!dirFile.isDirectory()) {
					dirFile.mkdir();
				}

				VOI voiNewFinal = new VOI((short) 0, "voi_result_" + key);
				int[] extents = new int[2];
				int[] srcExtents = srcImage.getExtents();

				int[] blankExts = new int[2];
				blankExts[0] = srcExtents[0];
				blankExts[1] = srcExtents[1];


				int imageDim = srcExtents[2];

				for (int j = 0; j < imageDim; j++) {

					ModelImage image = imageVec.get(j);
					
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

					AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(resultImage, blankImage, thresholds, fillValue, outputType, regionFlag, isInverse);
					thresholdAlgo.run();

					
					resultImage.calcMinMax();
					AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(resultImage);
					VOIExtractionAlgo.run();

					VOIVector v = resultImage.getVOIs();
					if (v.size() == 0)
						continue;

					// for femur part seg
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
					// maxMaskImage = null;
					// maskBuffer_ced = null;
					maskBuffer = null;
					// blankImage.disposeLocal(false);
					blankImage = null;
					// blankImage_ced.disposeLocal(false);
					// blankImage_ced = null;
					
					// mathAlgo.finalize();
					// mathAlgo = null;
					
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
				// for patella part only, femur don't need this
				// smoothVOI30(srcImage, srcImage);
				try {

					System.err.println("directory = " + directory);

					FileVOI fileVOI = new FileVOI("voi_" + key + ".xml",directory, srcImage);
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
		
	}
	
	public void smoothVOI30(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage,
					v.VOIAt(0), 30, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}
	
	/**
	 * Read testing images and the HNN predcited probability maps. 
	 */
	public void readFile() {

		try {
			
			int index;
		
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
				if (image != null) {
					
					keyImagesOrientation.put(key, image);
					keyImagesOrientation.get(key).setImageName(key);
					
				}
				keyImageIO = null;
			}
	        
			keys = maskImageTable.keySet();
	        for(String key : keys ){
	        	  
					Vector<String> stringVec = maskImageTable.get(key);
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
					System.err.println("axial key = " + key);
					maskTable.put(key, imageVec);
        	   
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
