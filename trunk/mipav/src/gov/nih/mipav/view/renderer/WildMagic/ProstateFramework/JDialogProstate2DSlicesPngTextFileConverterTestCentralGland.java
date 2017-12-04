package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

/**
 * The class converts the MRI and CED png slices into a file list.  HED uses the list to train HED deep learning model. 
 * 
 * --------------------------------------------------------------------------------------------
 * Testing case only. Don't need it.  For check-in purpose only.   Will remove after check-in
 * --------------------------------------------------------------------------------------------
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesPngTextFileConverterTestCentralGland extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;


	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();

	Hashtable<String, Vector<String>> imageTable = new Hashtable<String, Vector<String>>();
	Hashtable<String, Vector<String>> newImageTable = new Hashtable<String, Vector<String>>();
	
	Hashtable<String, Vector<String>> voiTable = new Hashtable<String, Vector<String>>();
	Hashtable<String, Vector<String>> newVOITable = new Hashtable<String, Vector<String>>();
	
	Hashtable<String, Vector<ModelImage>> maskTable = new Hashtable<String, Vector<ModelImage>>();
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesPngTextFileConverterTestCentralGland(Frame theParentFrame) {
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
	 * Dialog local actionPerformed handler.
	 */
	public void actionPerformed(ActionEvent event) {

		String command = event.getActionCommand();
		if (command.equals("OK")) {
			readKeyImageDir();
			sortImageTable();
			sortVOITable();
			writePngPair();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			
		} else if (command.equals("ChooseKeyImageDir")) {
			// readKeyImageDir();
			// sortImageTable();
			// sortVOITable();
			// writePngPair();
		} else if (command.equals("ChooseSaveImageDir")) {
			
		}
	}

	private void sortImageTable() {
		Set<String> keys = imageTable.keySet();
		
		for (String key : keys ) {
			Vector<String> stringVec = imageTable.get(key);
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
			newImageTable.put(key, newStringVect);
		}
	}
	
	private void sortVOITable() {
		Set<String> keys = voiTable.keySet();
		
		for (String key : keys) {
			Vector<String> stringVec = voiTable.get(key);
			Vector<String> newStringVect = new Vector<String>();
			Hashtable<Integer, String> hash = new Hashtable<Integer, String>(); 
			for ( int j = 0; j < stringVec.size(); j++ ) {
				String fullName = stringVec.get(j);
				int stopIndex = fullName.lastIndexOf(".");
				int startIndex = fullName.lastIndexOf("_");
				String numString = fullName.substring(startIndex+1, stopIndex);
				
				int num = Integer.valueOf(numString);
				hash.put(num, fullName);
			}
			
			for ( int j = 0; j < 50; j++ ) {
				String name = hash.get(j);
				if ( name != null ) {
					newStringVect.add(name);
				}
			}
			
			newVOITable.put(key, newStringVect);
		}
	}
	
	
	private void readKeyImageDir() {
	    File fileDir_5 = new File("/data/ruida/cg_HED_CED_scale_boundary/fold5Test/test");
	    traverse_folder_5(fileDir_5);
	}
	
	private void writePngPair() {
		try {
			
			PrintWriter fileWriter = new PrintWriter(new FileWriter("/data/ruida/cg_HED_CED_scale_boundary/fold5Test/test_mask_img-label.lst"));
			
            String imageName, voiName;
            
            Set<String> keys = newImageTable.keySet();
            for(String key: keys){
				Vector<String> imageNameVector = newImageTable.get(key);
				Vector<String> voiNameVector = newVOITable.get(key);
				
				for ( int j = 0; j < imageNameVector.size(); j++ ) {
					imageName = imageNameVector.get(j);
					voiName = voiNameVector.get(j);
					fileWriter.write(imageName + " " + voiName + "\n");
					
				}
			}
			
			fileWriter.close();
			System.err.println("finish text write");
		} catch (IOException e) {
			
		}
		
	}
	
	private void traverse_folder_5(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_5(new File(dir, children[i]), i);
			}
		}
	}
	
	private void traverse_folder_5(File dir, int index) {
		
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
			// int hashIndex = Integer.valueOf(numString);
			System.err.println(numString);
			if ( imageTable.get(numString) == null ) {
				imageTable.put(numString, new Vector<String>());
				imageTable.get(numString).add(dir.toString());
			} else {
				imageTable.get(numString).add(dir.toString());
			}
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
			// int hashIndex = Integer.valueOf(numString);
			System.err.println(numString);
			if ( voiTable.get(numString) == null ) {
				voiTable.put(numString, new Vector<String>());
				voiTable.get(numString).add(dir.toString());
			} else {
				voiTable.get(numString).add(dir.toString());
			}
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