package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import gov.nih.mipav.view.dialogs.*;

/**
 * This class simply converts the MRI and CED slices png files into a list file. 
 * The HED training model will take the list as input to train the deep learning model. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesPngTextFileConverterMICCAI extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;
	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();


	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesPngTextFileConverterMICCAI(Frame theParentFrame) {
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
		
	    /*
		File fileDir_80 = new File("/scratch/ProstateNewData/");
		traverse_80(fileDir_80);
		File fileDir_100 = new File("/scratch/LFF100backup/");
		traverse_100(fileDir_100);
		File fileDir_50 = new File("/scratch/50_test_cases");
		traverse_100(fileDir_50);
	    File fileDir_60 = new File("/scratch/60_prostate_sets");
		traverse_100(fileDir_60);
		*/
		
		// File fileDir_1 = new File("/scratch/aam_test/fold1");
		// traverse_folder_1(fileDir_1); 
	    // File fileDir_2 = new File("/scratch/aam_test/fold2");
	    // traverse_folder_2(fileDir_2);
		// File fileDir_3 = new File("/scratch/aam_test/fold3");
		// traverse_folder_3(fileDir_3);
		// File fileDir_4 = new File("/scratch/aam_test/fold4");
		// traverse_folder_4(fileDir_4);
		// File fileDir = new File("/data/ruida/MICAI2012/trainPNG");
		// File fileDir = new File("/data/ruida/MICAI2012/train_CED_TRANS");
		// File fileDir = new File("/data/ruida/MICCAI_2012_cg/train");
		// File fileDir = new File("/scratch/ISBI2017/train/fold4/sagittal");
		// File fileDir = new File("/data/ruida/train/fold2/sagittal");
		File fileDir = new File("/data/ruida/knees/trainFold/coronal/fold9");
	    traverse_folder(fileDir);
	}
	
	private void writePngPair() {
		try {
			// PrintWriter fileWriter = new PrintWriter(new FileWriter("/scratch/ISBI2017/train/fold4/sagittal.lst"));
			// PrintWriter fileWriter = new PrintWriter(new FileWriter("/data/ruida/train/fold2/sagittal.lst"));
			PrintWriter fileWriter = new PrintWriter(new FileWriter("/data/ruida/knees/trainFold/coronal/coronal_fold9.lst"));
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
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}


}