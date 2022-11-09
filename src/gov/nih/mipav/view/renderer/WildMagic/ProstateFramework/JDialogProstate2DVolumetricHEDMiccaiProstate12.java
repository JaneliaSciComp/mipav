package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

/**
 * For the Miccai promise 12 data, this class simply read the png files and generates
 * the png file list for HED to train the deep learning model.   
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DVolumetricHEDMiccaiProstate12 extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;


	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();


	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DVolumetricHEDMiccaiProstate12(Frame theParentFrame) {
		super(theParentFrame, false);
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
			
		} else if (command.equals("ChooseSaveImageDir")) {
			
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
	 * Read 3D images atlas directory.
	 */
	private void readKeyImageDir() {
		
		File fileDir = new File("/data/ruida/miccai/CED/axial");
	    traverse_folder(fileDir);
	}
	
	private void writePngPair() {
		try {
			PrintWriter fileWriter = new PrintWriter(new FileWriter("/data/ruida/miccai/CED/axial.lst"));
			int len = keyImageVOIVector1.size();
			int i;
            String imageName, voiName;
            
			for (i = 0; i < len; i++) {
				imageName = keyImageVector1.get(i);
				voiName = keyImageVOIVector1.get(i);
			    fileWriter.write(imageName + " " + voiName + "\n");
			}
			
			fileWriter.close();
			
		} catch (IOException e) {
			
		}
		
	}
	
	private void traverse_folder(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				// traverse_folder_1(new File(dir, children[i]));
				processDir_folder(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".png")) {
			// System.err.println(dir.toString());
			keyImageVector1.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".png")) {
			// System.err.println(dir.toString());
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

        
	}

	
	/**
	 * Initial panel
	 */
	public void init() {

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}


}