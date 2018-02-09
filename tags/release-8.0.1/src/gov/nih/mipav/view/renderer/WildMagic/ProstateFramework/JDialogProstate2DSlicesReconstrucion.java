package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;


/**
 * The class reconstructs the 3D surface from the axial, coronal, sagittal VOI contours.   
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesReconstrucion extends JDialogBase implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

		/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	private Vector<String> keyImageVector5 = new Vector<String>();
	private Vector<String> keyImageVOIVector5 = new Vector<String>();

	
	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<String, Hashtable<String, Vector<String>>> imageNameHashtable = new Hashtable<String, Hashtable<String, Vector<String>>>();

	Hashtable<String, Hashtable<String, String>> voiNameHashtable = new Hashtable<String, Hashtable<String, String>>();

	Hashtable<String, Hashtable<String, ModelImage>> imageHashtable = new Hashtable<String, Hashtable<String, ModelImage>>();
	Hashtable<String, Vector<VOI>> voiHashtable = new Hashtable<String, Vector<VOI>>();

	Hashtable<String, Vector<ModelImage>> srcImageTable = new Hashtable<String, Vector<ModelImage>>();
	Hashtable<String, Vector<VOI>> srcVOITable = new Hashtable<String, Vector<VOI>>();
	
	Hashtable<String, Hashtable<String, ModelImage>> origImageTable = new Hashtable<String, Hashtable<String, ModelImage>>();
	
	Hashtable<String, Hashtable<String, String>> origImageTableName = new Hashtable<String, Hashtable<String, String>>();
	Hashtable<String, Hashtable<String, String>> origVOITableName = new Hashtable<String, Hashtable<String, String>>();
	
	Hashtable<String, Hashtable<String, Vector<String>>> imageNameHashtableExtra = new Hashtable<String, Hashtable<String, Vector<String>>>();
	Hashtable<String, Hashtable<String, ModelImage>> imageHashtableExtra = new Hashtable<String, Hashtable<String, ModelImage>>();
	
	Hashtable<String, Vector<String>> imageTable = new Hashtable<String, Vector<String>>();
	Hashtable<String, Vector<String>> newImageTable = new Hashtable<String, Vector<String>>();
	Hashtable<String, Vector<ModelImage>> maskTable = new Hashtable<String, Vector<ModelImage>>();
	Hashtable<String, Vector<String>> maskImageTable = new Hashtable<String, Vector<String>>();
	
	Hashtable<String, Integer> dicomTable = new Hashtable<String, Integer>();
	Hashtable<String, Integer> voiTable = new Hashtable<String, Integer>();

	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesReconstrucion(Frame theParentFrame) {
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
		
		File fileDir_1 = new File("/scratch/ISBI2017/dataset");
		traverse_Layer(fileDir_1);
	
		File fileDir_5_test = new File("/scratch/ISBI2017/result/fold4/");
		traverse_folder_5(fileDir_5_test);
		
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
	
		private void traverse_folder_5(File dir) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_5(new File(dir, children[i]), children[i]);
			}
		}
	}
	
	private void traverse_folder_5(File dir, String orientation) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder_5(new File(dir, children[i]), orientation, children[i]);
			}
		}
	}
	
	private void processDir_folder_5(File dir, String orientation, String hashID) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder_last(new File(dir, children[i]), orientation, hashID);
			}
		}
	}
	
	private void processDir_folder_last(File dir, String orientation, String hashID) {
		String dirName = dir.toString();
		
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("voi") && 
				dirName.substring(begin, end).endsWith(".xml") ) {
			
			System.err.println("file = " + dir.toString());
			
			if (imageTable.get(hashID) == null) {
				imageTable.put(hashID, new Vector<String>());
			}
			imageTable.get(hashID).add(dir.toString());
		}
	}


	public void sortKeyImage_5() {
		int i;
		int len = keyImageVOIVector5.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector5.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+5, end));
			index = Integer.valueOf(imageName.substring(start + 5, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}

		keyImageVector5.clear();
		for (i = 0; i <= 49; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector5.add(imageName);
			}
		}

		for (i = 0; i < len; i++) {
			voiName = keyImageVOIVector5.get(i);
			start = voiName.lastIndexOf(File.separator) + 1;
			end = voiName.lastIndexOf(".");
			// System.err.println(voiName.substring(start+3, end));
			index = Integer.valueOf(voiName.substring(start + 3, end));
			// System.err.println("index = " + index);
			imageVOITable.put(index, voiName);
		}

		keyImageVOIVector5.clear();
		for (i = 0; i <= 49; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVector5.add(voiName);
			}
		}

		// test for printing
		i = 0;
		for (String entry : keyImageVector5) {
			System.err.println(i + " = " + entry);
			i++;
		}
		System.err.println("VOI:");
		i = 0;
		for (String entry : keyImageVOIVector5) {
			System.err.println(i + " = " + entry);
			i++;
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

		reconstructSurface();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	private void reconstructSurface() {
		
		Set<String> hashKeys = origImageTableName.keySet();
		
	    for ( String hashID : hashKeys ) {
	    	
    	    Hashtable<String, String> imageHashtable = origImageTableName.get(hashID);
			String imageFullName = imageHashtable.get("axial");
			int index = imageFullName.lastIndexOf(File.separator);
			String fileName = imageFullName.substring(index + 1, imageFullName.length());
			String directory = imageFullName.substring(0, index);
	    	
			int dashIndex = imageFullName.lastIndexOf("_");
			index = directory.lastIndexOf(File.separator);
			String imageTableHashID =  imageFullName.substring(index+1, dashIndex);
			
	    	Vector<String> imageVector = imageTable.get(imageTableHashID);
	    	
	    	if ( imageVector != null ) {
	    		System.err.println("imageTableHashID = " + imageTableHashID);
	    		System.err.println("filename = " + fileName);
				System.err.println("directory = " + directory);
	    		
		    	String xmlFile1 = imageVector.get(0);
		    	String xmlFile2 = imageVector.get(1);
		    	String xmlFile3 = imageVector.get(2);
		    	String plyFile = directory + File.separator + "input.ply";
		    	
		    	JDialogSaveMergedVOIs merge = new JDialogSaveMergedVOIs(parentFrame, xmlFile1, xmlFile2, xmlFile3, plyFile);
		    	merge.saveFile();
		    	
		    	JDialogSurfaceReconstruction rec = new JDialogSurfaceReconstruction(parentFrame, false);
		    	rec.setFileInput(directory, "input.ply");
		    	rec.setFileOutput(directory, "output.ply");
		    	rec.processAlgorithm();
		    	
		    	
	    	}
		    
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