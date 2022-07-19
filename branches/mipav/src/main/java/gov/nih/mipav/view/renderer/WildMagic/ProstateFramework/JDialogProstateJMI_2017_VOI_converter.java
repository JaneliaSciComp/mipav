package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.file.FileIO;
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
 * For the JMI paper, this class converts VOI contours back to binary image masks. 
 * Those binary image masks are used to compare the segmentation results with Ground
 * Truth results with "Segmentation Evaluation" software.   
 * https://github.com/Visceral-Project/EvaluateSegmentation
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateJMI_2017_VOI_converter extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	private JPanel imageSelectionPanel;

	/** image vector to hold the actual images. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();

	/** voi vector to hold the actual vois. */
	private Vector<VOI[]> keyImageVOIs = new Vector<VOI[]>();

	private Vector<String> keyImageVector = new Vector<String>();
	private Vector<String> keyImageVOIVector = new Vector<String>();


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
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateJMI_2017_VOI_converter(Frame theParentFrame) {
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
			
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	
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
		
		File fileDir_1 = new File("/scratch/aam_test/fold4/");
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
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector.add(dir.toString());
		}
       
		if (dirName.substring(begin, end).startsWith("result_voi")
				&& dirName.substring(begin, end).endsWith(".xml")  
				/*&&  !dirName.substring(begin, end).startsWith("voi_") */ ) {
		    System.err.println(dir.toString());
			keyImageVOIVector.add(dir.toString());
		}
	 
	}

	public void sortKeyImage_1() {
		int i;
		int len = keyImageVector.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector.get(i);
			// System.err.println("imageName = " + imageName);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+5, end));
			index = Integer.valueOf(imageName.substring(start+5, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}
		
		keyImageVector.clear();
		for (i = 0; i <= 49; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				// System.err.println("i = " + i + " " + imageName);
				keyImageVector.add(imageName);
			}
		}

		len = keyImageVOIVector.size();
		for (i = 0; i < len; i++) {
			voiName = keyImageVOIVector.get(i);
			start = voiName.lastIndexOf(File.separator) + 1;
			end = voiName.lastIndexOf(".");
			// System.err.println(voiName.substring(start+3, end));
			// index = Integer.valueOf(voiName.substring(start+3, end));
			// index = Integer.valueOf(voiName.substring(start+8, end));
			index = Integer.valueOf(voiName.substring(start+10, end));
			System.err.println("index = " + index);
			imageVOITable.put(index, voiName);
		}


		keyImageVOIVector.clear();
		for (i = 0; i <= 49; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVector.add(voiName);
				// System.err.println("i = " + i + " " + voiName);
			}
		}
        
		
		// test for printing
		/*
		i = 0;
		for (i = 0; i <= 49; i++) {
			System.err.println(i + " = " + keyImageVector.get(i));
			// System.err.println(i + " = " + keyImageVOIVector.get(i));
			i++;
		}
		*/ 
		/*
		System.err.println("VOI:");
		i = 0;
		for (String entry : keyImageVOIVector) {
			System.err.println(i + " = " + entry);
			i++;
		}
		*/ 
        
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

		dispose();
		System.gc();
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
		
		int size = keyImages.size();
		// Set<Integer> keys = maskTable.keySet();
		// for ( Integer i : keys ) {
        for(int i = 0; i < size; i++ ){
			
		    ModelImage srcImage = keyImages.get(i);
		 
		    String directory = saveImageDirectory + File.separator;
			
			File dirFile = new File(directory);
			if (!dirFile.isDirectory()) {
				dirFile.mkdir();
			}
		

			ModelImage gtMaskImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "hed");
			gtMaskImage = srcImage.generateUnsignedByteImage(1, false, false);
		    gtMaskImage.saveImage(directory, "hed_voi_" + i + ".nii" , FileUtility.NIFTI, false);	
			
		}
		
	}
	

	/**
	 * load image files and voi files
	 */
	public void loadFiles() {
		// readFiles();
		readFile();
		System.err.println("finish image I/O");

	}

		
	public void readFile() {
		int index;
		try {
			/*
			Set<String> keys = imageNameHashtable.keySet();
			
	        for(String key: keys){
	            Vector<String> imageDicomSet = imageNameHashtable.get(key);
	            
	            int lastIndex = key.indexOf("-");
                int len = key.length();
                String numberString = key.substring(lastIndex+1, len);
                int number = Integer.parseInt(numberString);
                System.err.println("number = " + number);
	            
	            FileIO fileIO = new FileIO();
	            String imageFullName = imageDicomSet.get(0);
	            int index = imageFullName.lastIndexOf(File.separator);
	            
                String fileName = imageFullName.substring(index+1, imageFullName.length());
                String directory = imageFullName.substring(0, index+1);
                
                boolean multiFile = true;
                ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
                imageHashtable.put(number, image);
	        }
	        */ 
			
			int start = 0;
			// int end = keyImageVector.size();
			int end = 50; 
			
			int currentIndex = 0;
			
		
			for (int imageIndex = start; imageIndex < end; imageIndex++) {
				// read key image
				String dir = keyImageVector.get(imageIndex);
				
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector.get(imageIndex);
				System.err.println("voiDir = " + voiDir);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory, keyImages.get(currentIndex));
				System.err.println("fileDirectory = " + directory + " fileName = " + fileName);
				System.err.println("voiDirectory = " + voiDirectory + "  voiFileName = " + voiFileName);
				keyImageVOIs.add(currentIndex, fileVOI.readVOI(false));

				keyImages.get(currentIndex).registerVOI(keyImageVOIs.get(currentIndex)[0]);
				
				// new ViewJFrameImage(keyImages.get(currentIndex));
				
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