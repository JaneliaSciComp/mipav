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
 * The class generates the png file list for test cases. 
 * 
 * Actually, no longer needed, since the HED test routine doesn't need the test case png file list. 
 * -------------------------------------------------------
 * For backup purpose only, will delete after SVN check-in
 * -------------------------------------------------------
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesPngTextFileConverterTest extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;


	private JPanel imageSelectionPanel;

	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();

	/** saved 2D slices atlas dir. */
	private JTextField textFieldSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<Integer, Vector<String>> imageTable = new Hashtable<Integer, Vector<String>>();
	Hashtable<Integer, Vector<String>> newImageTable = new Hashtable<Integer, Vector<String>>();
	
	Hashtable<Integer, Vector<String>> voiTable = new Hashtable<Integer, Vector<String>>();
	Hashtable<Integer, Vector<String>> newVOITable = new Hashtable<Integer, Vector<String>>();
	
	Hashtable<Integer, Vector<ModelImage>> maskTable = new Hashtable<Integer, Vector<ModelImage>>();
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesPngTextFileConverterTest(Frame theParentFrame) {
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
			sortImageTable();
			sortVOITable();
			writePngPair();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			
		} else if (command.equals("ChooseKeyImageDir")) {
			initImageTable();
			readKeyImageDir();
			// sortImageTable();
			// sortVOITable();
			// writePngPair();
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	private void sortImageTable() {
		for (int i = 0; i < 50; i++ ) {
			Vector<String> stringVec = imageTable.get(i);
			Vector<String> newStringVect = new Vector<String>();
			Hashtable<Integer, String> hash = new Hashtable<Integer, String>(); 
			for ( int j = 0; j < stringVec.size(); j++ ) {
				String fullName = stringVec.get(j);
				int stopIndex = fullName.lastIndexOf(".");
				int startIndex = fullName.lastIndexOf("_");
				String numString = fullName.substring(startIndex+1, stopIndex);
				System.err.println(numString);
				int num = Integer.valueOf(numString);
				System.err.println("num = " + num);
				hash.put(num, fullName);
			}
			
			for ( int j = 0; j < 50; j++ ) {
				String name = hash.get(j);
				if ( name != null ) {
					newStringVect.add(name);
				}
			}
			newImageTable.put(i, newStringVect);
		}
	}
	
	private void sortVOITable() {
		for (int i = 0; i < 50; i++ ) {
			Vector<String> stringVec = voiTable.get(i);
			Vector<String> newStringVect = new Vector<String>();
			Hashtable<Integer, String> hash = new Hashtable<Integer, String>(); 
			for ( int j = 0; j < stringVec.size(); j++ ) {
				String fullName = stringVec.get(j);
				int stopIndex = fullName.lastIndexOf(".");
				int startIndex = fullName.lastIndexOf("_");
				String numString = fullName.substring(startIndex+1, stopIndex);
				// System.err.println(numString);
				int num = Integer.valueOf(numString);
				// System.err.println("num = " + num);
				hash.put(num, fullName);
			}
			
			for ( int j = 0; j < 50; j++ ) {
				String name = hash.get(j);
				if ( name != null ) {
					newStringVect.add(name);
				}
			}
			newVOITable.put(i, newStringVect);
		}
	}
	private void initImageTable() {
		for ( int i = 0; i < 50; i++ ) {
			imageTable.put(i, new Vector<String>());
			newImageTable.put(i, new Vector<String>());
			voiTable.put(i, new Vector<String>());
			newVOITable.put(i, new Vector<String>());
			// maskTable.put(i, new Vector<ModelImage>());
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
		
		
		// File fileDir_1 = new File("/scratch/aam_test/fold1");
		// traverse_folder_1(fileDir_1); 
	    // File fileDir_2 = new File("/scratch/aam_test/fold2");
	    // traverse_folder_2(fileDir_2);
		// File fileDir_3 = new File("/scratch/aam_test/fold3");
		// traverse_folder_3(fileDir_3);
		// File fileDir_4 = new File("/scratch/aam_test/fold4");
		// traverse_folder_4(fileDir_4);
		// File fileDir_5 = new File("/scratch/aam_test/fold5");
		// traverse_folder_5(fileDir_5);
	    
	    // File fileDir_5 = new File("/data/ruida/prostateHED/fold5Test/test");
	    File fileDir_5 = new File("/data/ruida/JMI_2017/test/fold5/test");
	    traverse_folder_5(fileDir_5);
	}
	
	private void writePngPair() {
		try {
			// PrintWriter fileWriter = new PrintWriter(new FileWriter("/data/ruida/prostateHED/fold5Test/test_mask_img-label.lst"));
			PrintWriter fileWriter = new PrintWriter(new FileWriter("/data/ruida/JMI_2017/test/fold5/test_mask_img-label.lst"));
			int i;
            String imageName, voiName;
			
			for (i = 0; i < 50; i++ ) {
				Vector<String> imageNameVector = newImageTable.get(i);
				Vector<String> voiNameVector = newVOITable.get(i);
				
				for ( int j = 0; j < imageNameVector.size(); j++ ) {
					imageName = imageNameVector.get(j);
					voiName = voiNameVector.get(j);
					fileWriter.write(imageName + " " + voiName + "\n");
					
				}
			}
			
			fileWriter.close();
			
		} catch (IOException e) {
			
		}
		
	}
	
	private void traverse_folder_5(File dir) {
		// processDir_folder_5(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_5(new File(dir, children[i]), i);
			}
		}
	}
	
	private void traverse_folder_5(File dir, int index) {
		// processDir_folder_5(dir);
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			System.err.println("children.length = " + children.length);
			for (int i = 0; i < children.length; i++) {
				processDir_folder_5(new File(dir, children[i]), index);
			}
			
			
		}

	}

	private void processDir_folder_5(File dir, int index) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
	
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".png")
				) {
			System.err.println(dir.toString());
			String fullName = dir.toString();
			int idxEnd = fullName.lastIndexOf("/");
			String subName = fullName.substring(0, idxEnd);
			int idxStart = subName.lastIndexOf("/");
			String numString = fullName.substring(idxStart+1, idxEnd);
			int hashIndex = Integer.valueOf(numString);
			System.err.println(numString);
		    imageTable.get(hashIndex).add(dir.toString());
		}
		
		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".png")
				) {
			System.err.println(dir.toString());
			String fullName = dir.toString();
			int idxEnd = fullName.lastIndexOf("/");
			String subName = fullName.substring(0, idxEnd);
			int idxStart = subName.lastIndexOf("/");
			String numString = fullName.substring(idxStart+1, idxEnd);
			int hashIndex = Integer.valueOf(numString);
			System.err.println(numString);
		    voiTable.get(hashIndex).add(dir.toString());
		}
		
	}
	


	public void sortKeyImage() {
		int i;
		int len = keyImageVOIVector1.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector1.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+5, end));
			index = Integer.valueOf(imageName.substring(start+6, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}
		
		keyImageVector1.clear();
		for (i = 0; i < len; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector1.add(imageName);
			}
		}

		
		for (i = 0; i < len; i++) {
			voiName = keyImageVOIVector1.get(i);
			start = voiName.lastIndexOf(File.separator) + 1;
			end = voiName.lastIndexOf(".");
			// System.err.println(voiName.substring(start+3, end));
			index = Integer.valueOf(voiName.substring(start+4, end));
			// System.err.println("index = " + index);
			imageVOITable.put(index, voiName);
		}


		keyImageVOIVector1.clear();
		for (i = 0; i < len; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVector1.add(voiName);
			}
		}

		// test for printing
		/*
		i = 0;
		for (String entry : keyImageVector1) {
			System.err.println(i + " = " + entry);
			i++;
		}
		System.err.println("VOI:");
		i = 0;
		for (String entry : keyImageVOIVector1) {
			System.err.println(i + " = " + entry);
			i++;
		}
		*/
        
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

		dispose();
		System.gc();
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

		/*
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
		*/
	}

}