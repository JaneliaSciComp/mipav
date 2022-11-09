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
 * This class converts 3D prostate surface into nii binary mask images. 
 * Those binary mask images will be used by "EvaluateSegmentation" software for performance measure. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateISBIfinalSurfaceConvertNII extends JDialogBase implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;
	
	Hashtable<String, Hashtable<String, ModelImage>> origImageTable = new Hashtable<String, Hashtable<String, ModelImage>>();
	
	Hashtable<String, Hashtable<String, String>> origImageTableName = new Hashtable<String, Hashtable<String, String>>();
	Hashtable<String, Hashtable<String, String>> origVOITableName = new Hashtable<String, Hashtable<String, String>>();
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateISBIfinalSurfaceConvertNII(Frame theParentFrame) {
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
	 * Let user specify the saved 2D slices atlas, record the save directory.
	 */
	private void recordSaveImageDir() {
		String saveImageName;
		saveImageChooser.setDialogTitle("Open Saved Images Directory");
		saveImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = saveImageChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			saveImageName = saveImageChooser.getSelectedFile().getName();

			saveImageDirectory = String.valueOf(saveImageChooser.getCurrentDirectory()) + File.separatorChar + saveImageName + File.separatorChar;
			textFieldSaveImage.setText(saveImageDirectory);

		} else {
			return;
		}
	}

	/**
	 * Read 3D images atlas directory.
	 */
	private void readKeyImageDir() {
		
		File fileDir_1 = new File("/scratch/gitrepo/ProstateSeg/ISBI2017/3D_data_final");
		traverse_Layer(fileDir_1);
	
	}

	private void traverse_Layer(File dir) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
			    origImageTableName.put(children[i], new Hashtable<String, String>());
			    traverse_firstLayer(dir, children[i]);
			}
		}
	}

	private void traverse_firstLayer(File firstDir, String child) {
		File firstLayer = new File(firstDir, child);
		traverse_secondLayer(firstLayer, child);
	}

	

	private void traverse_secondLayer(File firstLayer, String hashID) {

		String[] children = firstLayer.list();

		for (int i = 0; i < children.length; i++) {

			// traverse_thirdLayer(new File(firstLayer, children[i]), hashID);
			if ( origImageTableName.get(hashID) == null ) {
				origImageTableName.put(hashID, new Hashtable<String, String>());
			}
			
			if ( origVOITableName.get(hashID) == null ) {
				origVOITableName.put(hashID, new Hashtable<String, String>());
			}
			
			if (children[i].equals("imageAxial.xml")) {
				origImageTableName.get(hashID).put("axial", firstLayer.getAbsolutePath() + File.separator + children[i]);
			} else if (children[i].equals("imageSagittal.xml")) {
				origImageTableName.get(hashID).put("sagittal", firstLayer.getAbsolutePath() + File.separator + children[i]);
			} else if (children[i].equals("imageCoronal.xml")) {
				origImageTableName.get(hashID).put("coronal", firstLayer.getAbsolutePath() + File.separator + children[i]);
			} 
			 
			if (children[i].equals("voiAxial.xml")) {
				origVOITableName.get(hashID).put("axial", firstLayer.getAbsolutePath() + File.separator + children[i]);
			} else if (children[i].equals("voiSagittal.xml")) {
				origVOITableName.get(hashID).put("sagittal", firstLayer.getAbsolutePath() + File.separator + children[i]);
			} else if (children[i].equals("voiCoronal.xml")) {
				origVOITableName.get(hashID).put("coronal", firstLayer.getAbsolutePath() + File.separator + children[i]);
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

		readImages();
		generateContours();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	
	private void generateContours() {

		
		Set<String> hashKeys = origImageTable.keySet();
		
		// String orientation = "axial";
	    // String orientation = "sagittal";
	    String orientation = "coronal";
		
	    for ( String hashKey : hashKeys ) {
	    	
	    	Hashtable<String, ModelImage> imageHash = origImageTable.get(hashKey);
	    	ModelImage srcImage = imageHash.get(orientation);
	    	
	    	String folderName = null;
	    	
	    	int index = hashKey.lastIndexOf("_");
	    	String id = hashKey.substring(0, index);
	    	
	    	String subDir = saveImageDirectory + File.separator + folderName + File.separator + orientation + File.separator;
			
	    	File subDirFile = new File(subDir);
			if (!subDirFile.isDirectory())
				subDirFile.mkdir();
			subDir += id + File.separator;
			subDirFile = new File(subDir);
			if (!subDirFile.isDirectory())
				subDirFile.mkdir();
	
			smoothVOI30(srcImage, srcImage);
			ModelImage gtMaskImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "seg3D");
			gtMaskImage = srcImage.generateUnsignedByteImage(1, false, false);
		    gtMaskImage.saveImage(subDir, "seg3D_" + id + ".nii" , FileUtility.NIFTI, false);	
		
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

	public void readImages() {

		
		Set<String> keys = origImageTableName.keySet();
		try {
		for (String hashID : keys) {

			Hashtable<String, String> imageHashtable = origImageTableName.get(hashID);
			Hashtable<String, String>  voiHashtable = origVOITableName.get(hashID);
			
		    if ( origImageTable.get(hashID) == null ) {
		    	origImageTable.put(hashID, new Hashtable<String, ModelImage>());
		    }
		    
		    
		    FileIO fileIO = new FileIO();
			String imageFullName = imageHashtable.get("axial");
			int index = imageFullName.lastIndexOf(File.separator);
			String fileName = imageFullName.substring(index + 1, imageFullName.length());
			String directory = imageFullName.substring(0, index + 1);
			System.err.println("filename = " + fileName);
			System.err.println("directory = " + directory);
			ModelImage imageAxial = fileIO.readImage(fileName, directory);
		
		    
			imageFullName = imageHashtable.get("sagittal");
			index = imageFullName.lastIndexOf(File.separator);
			fileName = imageFullName.substring(index + 1, imageFullName.length());
			directory = imageFullName.substring(0, index + 1);
			System.err.println("filename = " + fileName);
			System.err.println("directory = " + directory);
			ModelImage imageSagittal = fileIO.readImage(fileName, directory);
			
			imageFullName = imageHashtable.get("coronal");
			index = imageFullName.lastIndexOf(File.separator);
			fileName = imageFullName.substring(index + 1, imageFullName.length());
			directory = imageFullName.substring(0, index + 1);
			System.err.println("filename = " + fileName);
			System.err.println("directory = " + directory);
			ModelImage imageCoronal = fileIO.readImage(fileName, directory);
			
            
			String voiDir = voiHashtable.get("axial");
			index = voiDir.lastIndexOf(File.separator);
			String voiDirectory = new String(voiDir.substring(0, index + 1));
			String voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));
			FileVOI fileVOIAxial = new FileVOI(voiFileName, voiDirectory, imageAxial);
			imageAxial.registerVOI(fileVOIAxial.readVOI(false)[0]);
			// new ViewJFrameImage(imageAxial);
			origImageTable.get(hashID).put("axial", imageAxial);
			
			voiDir = voiHashtable.get("sagittal");
			index = voiDir.lastIndexOf(File.separator);
		    voiDirectory = new String(voiDir.substring(0, index + 1));
			voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));
			FileVOI fileVOISagittal = new FileVOI(voiFileName, voiDirectory, imageSagittal);
			imageSagittal.registerVOI(fileVOISagittal.readVOI(false)[0]);
			// new ViewJFrameImage(imageSagittal);
			origImageTable.get(hashID).put("sagittal", imageSagittal);
			
			
			voiDir = voiHashtable.get("coronal");
			index = voiDir.lastIndexOf(File.separator);
		    voiDirectory = new String(voiDir.substring(0, index + 1));
			voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));
			FileVOI fileVOICoronal = new FileVOI(voiFileName, voiDirectory, imageCoronal);
			imageCoronal.registerVOI(fileVOICoronal.readVOI(false)[0]);
			// new ViewJFrameImage(imageCoronal);
			origImageTable.get(hashID).put("coronal", imageCoronal);
            
		}	
	
		} catch ( Exception e ) {
			e.printStackTrace();
		}
	}


	/**
	 * Smooth VOIs to 60 points.
	 * 
	 * @param maskImage
	 * @param resultImage
	 */
	public void smoothVOI60(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 60, false);
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
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
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