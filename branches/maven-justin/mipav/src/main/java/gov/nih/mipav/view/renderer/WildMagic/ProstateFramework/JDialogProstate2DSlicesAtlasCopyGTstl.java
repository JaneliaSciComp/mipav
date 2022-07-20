package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import java.net.URLDecoder;
import org.apache.commons.io.*;

/**
 * This class copy ground truth stl file to destination directory.   
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesAtlasCopyGTstl extends JDialogBase implements AlgorithmInterface {

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
	
	private JTextField textFieldSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<String, Hashtable<String, Vector<String>>> imageNameHashtable = new Hashtable<String, Hashtable<String, Vector<String>>>();
	Hashtable<String, Hashtable<String, String>> voiNameHashtable = new Hashtable<String, Hashtable<String, String>>();

	Hashtable<String, Vector<ModelImage>> srcImageTable = new Hashtable<String, Vector<ModelImage>>();
	Hashtable<String, Hashtable<String, String>> origImageTableName = new Hashtable<String, Hashtable<String, String>>();
	Hashtable<String, Hashtable<String, String>> origVOITableName = new Hashtable<String, Hashtable<String, String>>();
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesAtlasCopyGTstl(Frame theParentFrame) {
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
		
		// source directory
		File fileDir_1 = new File("/scratch/data/Nov2015_July2016");
		// File fileDir_1 = new File("/scratch/ISBI2017/newData");
		traverse_Layer(fileDir_1);
	
		// destination directory
		File fileDir_2 = new File("/scratch/ISBI2017/dataset");
		traverse_Layer_DS(fileDir_2);
		
	}


	private void traverse_Layer_DS(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
			    origImageTableName.put(children[i], new Hashtable<String, String>());
			    traverse_firstLayer_DS(dir, children[i]);
				System.err.println();
			}
			
		}
	}

	private void traverse_firstLayer_DS(File firstDir, String child) {
		File firstLayer = new File(firstDir, child);
		traverse_secondLayer_DS(firstLayer, child);
	}
	
	private void traverse_secondLayer_DS(File firstLayer, String hashID) {

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
	
	private void traverse_Layer(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
		
			for (int i = 0; i < children.length; i++) {
			    System.err.println("current index = " + i);
				imageNameHashtable.put(children[i], new Hashtable<String, Vector<String>>());
				voiNameHashtable.put(children[i], new Hashtable<String, String>());
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
			
			if (children[i].toLowerCase().equals("dicom")) {
				traverse_scanLayer(new File(firstLayer, children[i]), hashID);
			} else if (children[i].toLowerCase().equals("mold")) {
				traverse_voiLayer(new File(firstLayer, children[i]), hashID);
			} else {
				
			}
			 
		}
		
	}
	

	private void traverse_scanLayer(File secondLayer, String hashID) {

		String[] children = secondLayer.list();

		for (int i = 0; i < children.length; i++) {
			
			String decodedPath = MipavUtil.decodeStr(secondLayer + File.separator + children[i]);
			File file = new File(decodedPath);
			if (file.isDirectory()) {
				imageNameHashtable.get(hashID).put(children[i], new Vector<String>());
				traverse_T2Layer(new File(decodedPath), hashID, children[i]);
			}
		}

	}

	private void traverse_T2Layer(File T2Layer, String hashID, String orientationLabel) {
		String[] children = T2Layer.list();
		System.err.println("children[" + 0 + "] = " + children[0]);
		if ((children[0].startsWith("I") && children[0].substring(1, children[0].length()).matches("^?\\d+$")) || children[0].endsWith("dcm")) {

			System.err.println(hashID + "-->" + orientationLabel + ":");
			for (int i = 0; i < children.length; i++) {
				imageNameHashtable.get(hashID).get(orientationLabel).add(T2Layer + File.separator + children[i]);
			}
			System.err.println("imageNameHashtable.get(hashID).get(orientationLabel).size() = " + imageNameHashtable.get(hashID).get(orientationLabel).size());

		} else {

			for (int i = 0; i < children.length; i++) {
				String decodedPath = MipavUtil.decodeStr(T2Layer + File.separator + children[i]);
				File file = new File(decodedPath);
				if (file.isDirectory()) {
					traverse_T2Layer_deeper(new File(decodedPath), hashID, children[i]);
				}
			}

		}
		System.err.println();
	}

	private void traverse_T2Layer_deeper(File T2Layer, String hashID, String orientationLabel) {
		String[] children = T2Layer.list();
		if (children[0].startsWith("I") && children[0].substring(1, children[0].length()).matches("^?\\d+$")) {
			System.err.println(hashID + "-->" + orientationLabel + ":");
			imageNameHashtable.get(hashID).put(orientationLabel, new Vector<String>());

			for (int i = 0; i < children.length; i++) {
				imageNameHashtable.get(hashID).get(orientationLabel).add(T2Layer.getAbsolutePath() + File.separator + children[i]);
				// System.err.println("imageNameHashtable.get(hashID).get(orientationLabel).size() = " + imageNameHashtable.get(hashID).get(orientationLabel));
			}
			System.err.println("imageNameHashtable.get(hashID).get(orientationLabel).size() = " + imageNameHashtable.get(hashID).get(orientationLabel).size());
		}
	}

	private void traverse_voiLayer(File secondLayer, String hashID) {

		String[] children = secondLayer.list();
		
		for (int i = 0; i < children.length; i++) {

			String voiString = secondLayer + File.separator + children[i];
			int index = voiString.lastIndexOf(File.separator);
			String voiName = voiString.substring(index + 1, voiString.length());

			if (voiName.endsWith("ax_fin.stl")) {
				voiNameHashtable.get(hashID).put("axial", voiString);
				System.err.println(voiString);
			} else if (voiName.endsWith("ax_cor.stl") || voiName.endsWith("ax_sag_cor.stl")) {
				voiNameHashtable.get(hashID).put("coronal", voiString);
				System.err.println(voiString);
			} else if (voiName.endsWith("ax_cor_sag.stl") || voiName.endsWith("ax_sag.stl")) {
				voiNameHashtable.get(hashID).put("sagittal", voiString);
				System.err.println(voiString);
			}

		}
		System.err.println();

	}



	private void processDir_folder_5(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("image") && dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector5.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi") && dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVOIVector5.add(dir.toString());
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

	
	public void callAlgorithm() {
		
		long startTime = System.currentTimeMillis();

		copyFile();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}



	

	public void copyFile() {

		srcImageTable.put("axial", new Vector<ModelImage>());
		srcImageTable.put("sagittal", new Vector<ModelImage>());
		srcImageTable.put("coronal", new Vector<ModelImage>());
		
		try {
		
			Set<String> keys = imageNameHashtable.keySet();
			
			for (String hashID : keys) {

				Hashtable<String, Vector<String>> imageDicomSet = imageNameHashtable.get(hashID);
				Hashtable<String, String>  voiSet = voiNameHashtable.get(hashID);
				
				Hashtable<String, String> imageHashtable = origImageTableName.get(hashID);
				if ( imageHashtable == null ) continue;
				String imageFullNameDS = imageHashtable.get("axial");
				int index_DS = imageFullNameDS.lastIndexOf(File.separator);
				String fileName_DS = imageFullNameDS.substring(index_DS + 1, imageFullNameDS.length());
				String directory_DS = imageFullNameDS.substring(0, index_DS + 1);
				System.err.println("filename_DS = " + fileName_DS);
				System.err.println("directory_DS = " + directory_DS);
				
				Set<String> orientationKeys = imageDicomSet.keySet();
				
				for ( String orientation : orientationKeys ) {
					
					    Vector<String> imageOrientationSet = imageDicomSet.get(orientation);
					    System.err.println("imageOrientationSet.size() = " + imageOrientationSet.size());
					    
					    
					    if ( imageOrientationSet.size() > 0 ) {
							FileIO fileIO = new FileIO();
							String voiFileName = null;
							String imageFullName = imageOrientationSet.get(0);
							int index = imageFullName.lastIndexOf(File.separator);
			
							String fileName = imageFullName.substring(index + 1, imageFullName.length());
							String directory = imageFullName.substring(0, index + 1);
							System.err.println("filename = " + fileName);
							System.err.println("directory = " + directory);
			
							boolean multiFile = true;
							ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
							image.setImageName(hashID + "_" + orientation);
							
							int[] extents = image.getExtents();
							
								int imageOrientation = image.getImageOrientation();
								
								if ( imageOrientation == FileInfoBase.AXIAL ) {
									voiFileName = voiSet.get("axial");
								    System.err.println("voiFileName = " + voiFileName);
								    FileUtils.copyFileToDirectory(new File(voiFileName), 
								    		new File(directory_DS + File.separator));
							
								}
				
					   } else {
					      	System.err.println("Error");
					     	System.err.println(hashID + " ---->>" + orientation );
					   }
				    
				} 
				
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
		imageSelectionPanel.setLayout(new GridLayout(1, 3));
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

		

	}

}