package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;


/**
 * The class converts the 2D-volumetric approach axial, sagittal and coronal MRI and CED png slices 
 * into a file list.  HED uses the list to train HED deep learning model. 
 * 
 * Training case only. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesPngTextFileConverterCentralGland extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	private JPanel imageSelectionPanel;

	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();

	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesPngTextFileConverterCentralGland(Frame theParentFrame) {
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
			readKeyImageDir();
			sortKeyImage();
			writePngPair();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			
		} else if (command.equals("ChooseKeyImageDir")) {
			// readKeyImageDir();
			// sortKeyImage();
			// writePngPair();
		} else if (command.equals("ChooseSaveImageDir")) {
			
		}
	}

	/**
	 * Read 3D images atlas directory.
	 */
	private void readKeyImageDir() {
		
	   
		File fileDir = new File("/scratch/prostateXdata/trainFold/121_146/coronal");
	    traverse_folder(fileDir);
	}
	
	private void writePngPair() {
		try {
			
			PrintWriter fileWriter = new PrintWriter(new FileWriter("/scratch/prostateXdata/trainFold/121_146/coronal.lst"));
			
			int len = keyImageVOIVector1.size();
			int i;
            String imageName, voiName;
            
			for (i = 0; i < len; i++) {
				imageName = keyImageVector1.get(i);
				voiName = keyImageVOIVector1.get(i);
			    fileWriter.write(imageName + " " + voiName + "\n");
			}
			
			fileWriter.close();
			System.err.println("text write finish");
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
	private void traverse_folder(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder(File dir) {
		String dirName = dir.toString();
		
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".png")) {
			
			keyImageVector1.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".png")) {
			
			keyImageVOIVector1.add(dir.toString());
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
			index = Integer.valueOf(imageName.substring(start+6, end));
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
			index = Integer.valueOf(voiName.substring(start+4, end));
			imageVOITable.put(index, voiName);
		}


		keyImageVOIVector1.clear();
		for (i = 0; i < len; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVector1.add(voiName);
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
	 * Initial panel
	 */
	public void init() {

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		mainPanel.add(imageSelectionPanel, BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}


}