package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.URLDecoder;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import ar.com.hjg.pngj.*;
import ar.com.hjg.pngj.chunks.PngChunkTextVar;

/**
 * This class convert the 3D images to 2D slices based atlas. Users specify the
 * 3D prostate images dir, and output 2D slices based dir, the algorithm auto
 * convert each 3D image to 2D slices with corresponding VOIs. Each saved VOIs
 * is 2-contour based VOI, which is used to build the Active Appearance Model
 * (AAM).
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DHEDmapCg extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/**
	 * bounding box for crop the image. Currently set from 0 to 512, the orginal
	 * image slice size.
	 */
	private int boxYmin, boxYmax;
	private int boxXmin, boxXmax;

	/** X cropped region bounds. */
	private int[] xBounds = new int[2];

	/** Y cropped region bounds. */
	private int[] yBounds = new int[2];

	/** Z cropped region bound. */
	private int[] zBounds = new int[2];

	/** crop margin algorithm. */
	private AlgorithmAddMargins cropAlgo;

	/** result cropped image. */
	private ModelImage cropImage = null;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** key images variables. */
	private JFileChooser keyImageChooser = new JFileChooser();
	private String keyImageDirectory;

	// axis region
	private JComboBox axisList;
	private JLabel labelAxis;

	private static int Axial = 0;
	private static int Saggital = 1;
	private static int Coronal = 2;
	private int axis = Axial;

	

	/** image vector to hold the actual images. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();

	/** voi vector to hold the actual vois. */
	private Vector<VOI[]> keyImageVOIs = new Vector<VOI[]>();

	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();

	private Vector<String> keyImageVector2 = new Vector<String>();
	private Vector<String> keyImageVOIVector2 = new Vector<String>();

	private Vector<String> keyImageVector3 = new Vector<String>();
	private Vector<String> keyImageVOIVector3 = new Vector<String>();

	private Vector<String> keyImageVector4 = new Vector<String>();
	private Vector<String> keyImageVOIVector4 = new Vector<String>();

	private Vector<String> keyImageVector5 = new Vector<String>();
	private Vector<String> keyImageVOIVector5 = new Vector<String>();

	
	/** cropped key image vector. */
	private Vector<ModelImage> cropKeyImages = new Vector<ModelImage>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<String, Vector<String>> imageTable = new Hashtable<String, Vector<String>>();
	Hashtable<String, Vector<String>> newImageTable = new Hashtable<String, Vector<String>>();
	Hashtable<String, Vector<ModelImage>> maskTable = new Hashtable<String, Vector<ModelImage>>();
	
	Hashtable<String, Vector<String>> maskImageTable = new Hashtable<String, Vector<String>>();
	

	Hashtable<String, Vector<String>> imageNameHashtable = new Hashtable<String, Vector<String>>(); 
	Hashtable<String, Vector<String>> voiNameHashtable = new Hashtable<String, Vector<String>>(); 
	
	Hashtable<String, ModelImage> imageHashtable = new Hashtable<String, ModelImage>(); 
	Hashtable<String, VOI> voiHashtable = new Hashtable<String, VOI>(); 
	
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DHEDmapCg(Frame theParentFrame) {
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
			// MipavUtil.showHelp("Haral1001");
		} else if (command.equals("ChooseKeyImageDir")) {
			readKeyImageDir();
			
			// sortKeyImage_5();
			sortImageTable();
			
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
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
				int stopIndex = fullName.lastIndexOf("-");
				String subName = fullName.substring(0, stopIndex-1);
				stopIndex = subName.lastIndexOf("-");
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
			maskImageTable.put(key, newStringVect);
		}
	}
	/*
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
			
			newVOITable.put(key, newStringVect);
		}
	}
	*/
	
	private void printImageTable() {
		for (int i = 0; i < 50; i++ ) {
			Vector<String> stringVec = newImageTable.get(i);
			System.err.println("index = " + i + ":");
			for ( int j = 0; j < stringVec.size(); j++ ) {
				System.err.println(stringVec.get(j));
			}
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
		
		File fileDir_1 = new File("/scratch/data/Data/Prostate");
		traverse_Layer(fileDir_1); 
		File fileDir_5_test = new File("/data/ruida/cgHED/fold1Test/test_cg_mask_iter_15000");
	    traverse_folder_5(fileDir_5_test);
	}
	
	private void traverse_Layer(File dir) {
		// processDir_folder_1(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			// for (int i = 0; i < 1;  i++) {
		    for (int i = 0; i < children.length; i++) {
				System.err.println(dir + File.separator+ children[i]);
				imageNameHashtable.put(children[i], new Vector<String>());
				voiNameHashtable.put(children[i], new Vector<String>());
				traverse_firstLayer(dir, children[i]);
				// traverse_folder_1(new File(dir, children[i]));
			}
		}
	}
 
    private void traverse_firstLayer(File firstDir, String child) {
    	File firstLayer = new File(firstDir, child);
    	traverse_secondLayer(firstLayer, child);
    }
    
    private void traverse_secondLayer(File firstLayer, String hashID) {
    	String[] children = firstLayer.list();
    	for ( int i = 0; i < children.length; i++ ) {
    		traverse_thirdLayer(new File(firstLayer, children[i]), hashID);
    		if ( children[i].equals("Scans")) {
    			traverse_scanLayer(new File(firstLayer, children[i]), hashID);
    		}
    		
    		if ( children[i].equals("Annotations")) {
    			traverse_voiLayer(new File(firstLayer, children[i]), hashID);
    		}
    		
    	}
    }
    
    private void traverse_thirdLayer(File secondLayer, String hashID) {
    	String[] children = secondLayer.list();
    	for ( int i = 0; i < children.length; i++ ) {
    		
    		if ( children[i].equals("Scans")) {
    			traverse_scanLayer(new File(secondLayer, children[i]), hashID);
    		}
    		
    		if ( children[i].equals("Annotations")) {
    			traverse_voiLayer(new File(secondLayer, children[i]), hashID);
    		}
    		
    	}
    }
    
    private void traverse_scanLayer(File secondLayer, String hashID) {
    	String[] children = secondLayer.list();
    	for ( int i = 0; i < children.length; i++ ) {
    			// System.err.println("image: " + secondLayer + File.separator + children[i]);
    			String decodedPath = MipavUtil.decodeStr(secondLayer + File.separator + children[i]);
    			System.err.println("decodedPath = " + decodedPath);
    		    File file = new File(decodedPath);
    		    if ( file.isDirectory() ) {
    		    	traverse_T2Layer(new File(decodedPath), hashID);
    		    } else {
    		    	file.delete();
    		    }
    	}
    }
    
    private void traverse_T2Layer(File T2Layer, String hashID) {
    	System.out.println(T2Layer.exists());
    	// System.err.println("test: " + T2Layer.toString());
    	
    	String[] children = T2Layer.list();
    	int smallIndex = Integer.MAX_VALUE;
    	String smallIndexString = null;
    	for ( int i = 0; i < children.length; i++ ) {
    		int currentIndex = Integer.valueOf(children[i]);
    		if ( currentIndex < smallIndex) {
    			smallIndex = currentIndex;
    			smallIndexString = children[i];
    		}
    	}
    	
    	// System.err.println("t2Layer: " + T2Layer + File.separator + smallIndexString);
    	traverse_DicomLayer(new File(T2Layer + File.separator + smallIndexString), hashID);
    
    }
    
    private void traverse_DicomLayer(File lastLayer, String hashID) {
    	String[] children = lastLayer.list();
    	for ( int i = 0; i < children.length; i++ ) {
    		imageNameHashtable.get(hashID).add(lastLayer + File.separator + children[i]);
    	    // System.err.println(lastLayer + File.separator + children[i]);
    	}
    	
    }
    
    private void traverse_voiLayer(File secondLayer, String hashID) {
    	String[] children = secondLayer.list();
    	
    	int count = 0;
    	
    	for ( int i = 0; i < children.length; i++ ) {
    		
    		String voiString = secondLayer + File.separator + children[i];
    		int index = voiString.lastIndexOf(File.separator);
    		String voiName = voiString.substring(index+1, voiString.length());
    		/*
    		if ( voiName.equals("cg.voi") || voiName.equals("wp.voi")) {
    			System.err.println("voi: " + secondLayer + File.separator + children[i]);
    			voiNameHashtable.get(hashID).add(voiString);
    		}
    		*/
    		String decodedVOI = MipavUtil.decodeStr(voiName);
    		
    		if ( !decodedVOI.contains("manuel") && !decodedVOI.contains("manual") && 
    				decodedVOI.startsWith("cg") && decodedVOI.endsWith("voi")) {
    			System.err.println("voi: " + secondLayer + File.separator + children[i]);
    			voiNameHashtable.get(hashID).add(MipavUtil.decodeStr(voiString));
    			count++;
    		}
    		if ( !decodedVOI.contains("manuel") && !decodedVOI.contains("manual") && 
    				decodedVOI.startsWith("wp") && decodedVOI.endsWith("voi")) {
    			// System.err.println("voi: " + secondLayer + File.separator + children[i]);
    			// voiNameHashtable.get(hashID).add(MipavUtil.decodeStr(voiString));
    			count++;
    		}

    	
    	}
    	
    	if ( count < 2 ) {
			// System.err.println("voi: " + secondLayer + File.separator);
		}
		
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
			keyImageVector1.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVOIVector1.add(dir.toString());
		}
	}

	public void sortKeyImage_1() {
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
			index = Integer.valueOf(imageName.substring(start+5, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}
		
		keyImageVector1.clear();
		for (i = 0; i <= 49; i++) {
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
			index = Integer.valueOf(voiName.substring(start+3, end));
			// System.err.println("index = " + index);
			imageVOITable.put(index, voiName);
		}


		keyImageVOIVector1.clear();
		for (i = 0; i <= 49; i++) {
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


	private void traverse_folder_2(File dir) {
		processDir_folder_2(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_2(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder_2(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector2.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVOIVector2.add(dir.toString());
		}
	}

	public void sortKeyImage_2() {
		int i;
		int len = keyImageVOIVector2.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector2.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+5, end));
			index = Integer.valueOf(imageName.substring(start+5, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}
		
		keyImageVector2.clear();
		for (i = 0; i <= 49; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector2.add(imageName);
			}
		}

		
		for (i = 0; i < len; i++) {
			voiName = keyImageVOIVector2.get(i);
			start = voiName.lastIndexOf(File.separator) + 1;
			end = voiName.lastIndexOf(".");
			// System.err.println(voiName.substring(start+3, end));
			index = Integer.valueOf(voiName.substring(start+3, end));
			// System.err.println("index = " + index);
			imageVOITable.put(index, voiName);
		}


		keyImageVOIVector2.clear();
		for (i = 0; i <= 49; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVector2.add(voiName);
			}
		}

		// test for printing
		/*
		i = 0;
		for (String entry : keyImageVector2) {
			System.err.println(i + " = " + entry);
			i++;
		}
		System.err.println("VOI:");
		i = 0;
		for (String entry : keyImageVOIVector2) {
			System.err.println(i + " = " + entry);
			i++;
		}
		*/

	}

	private void traverse_folder_3(File dir) {
		processDir_folder_3(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_3(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder_3(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector3.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVOIVector3.add(dir.toString());
		}
	}

	public void sortKeyImage_3() {
		int i;
		int len = keyImageVOIVector3.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector3.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+5, end));
			index = Integer.valueOf(imageName.substring(start+5, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}
		
		keyImageVector3.clear();
		for (i = 0; i <= 49; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector3.add(imageName);
			}
		}

		
		for (i = 0; i < len; i++) {
			voiName = keyImageVOIVector3.get(i);
			start = voiName.lastIndexOf(File.separator) + 1;
			end = voiName.lastIndexOf(".");
			// System.err.println(voiName.substring(start+3, end));
			index = Integer.valueOf(voiName.substring(start+3, end));
			// System.err.println("index = " + index);
			imageVOITable.put(index, voiName);
		}


		keyImageVOIVector3.clear();
		for (i = 0; i <= 49; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVector3.add(voiName);
			}
		}

		// test for printing
		/*
		i = 0;
		for (String entry : keyImageVector3) {
			System.err.println(i + " = " + entry);
			i++;
		}
		System.err.println("VOI:");
		i = 0;
		for (String entry : keyImageVOIVector3) {
			System.err.println(i + " = " + entry);
			i++;
		}
		*/
		

	}


	private void traverse_folder_4(File dir) {
		processDir_folder_4(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_4(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder_4(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector4.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVOIVector4.add(dir.toString());
		}
	}
	
	private void traverse_folder_5(File dir,boolean test) {
		processDir_folder_5(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_5(new File(dir, children[i]), test);
			}
		}

	}

	private void processDir_folder_5(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector5.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVOIVector5.add(dir.toString());
		}
	}

	public void sortKeyImage_4() {
		int i;
		int len = keyImageVOIVector4.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector4.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+5, end));
			index = Integer.valueOf(imageName.substring(start+5, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}
		
		keyImageVector4.clear();
		for (i = 0; i <= 49; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector4.add(imageName);
			}
		}

		
		for (i = 0; i < len; i++) {
			voiName = keyImageVOIVector4.get(i);
			start = voiName.lastIndexOf(File.separator) + 1;
			end = voiName.lastIndexOf(".");
			// System.err.println(voiName.substring(start+3, end));
			index = Integer.valueOf(voiName.substring(start+3, end));
			// System.err.println("index = " + index);
			imageVOITable.put(index, voiName);
		}


		keyImageVOIVector4.clear();
		for (i = 0; i <= 49; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVector4.add(voiName);
			}
		}

		// test for printing
		/*
		i = 0;
		for (String entry : keyImageVector4) {
			System.err.println(i + " = " + entry);
			i++;
		}
		System.err.println("VOI:");
		i = 0;
		for (String entry : keyImageVOIVector4) {
			System.err.println(i + " = " + entry);
			i++;
		}
		*/

	}

	
	private void traverse_folder_5_old(File dir) {
		processDir_folder_5(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_5(new File(dir, children[i]), i);
			}
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
				&& dirName.contains("dsn5")) {
			// System.err.println("file = " + dir.toString());
			String fullName = dir.toString();
			int idxEnd = fullName.lastIndexOf("/");
			String subName = fullName.substring(0, idxEnd);
			int idxStart = subName.lastIndexOf("/");
			String numString = fullName.substring(idxStart+1, idxEnd);
			// int hashIndex = Integer.valueOf(numString);
			// System.err.println(numString);
			if ( imageTable.get(numString) == null ) {
				imageTable.put(numString, new Vector<String>());
			}
		    imageTable.get(numString).add(dir.toString());
		}
	}

	public void sortKeyImage_5() {
		int i;
		int len = keyImageVector5.size();
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
			index = Integer.valueOf(imageName.substring(start+5, end));
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
			index = Integer.valueOf(voiName.substring(start+3, end));
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
		/*
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

		if (algorithm instanceof AlgorithmAddMargins) {

			if ((cropAlgo.isCompleted() == true) && (cropImage != null)) {
				/*
				 * try { new ViewJFrameImage(cropImage, null, new Dimension(610,
				 * 200)); } catch (OutOfMemoryError error) { MipavUtil
				 * .displayError("Out of memory: unable to open new frame"); }
				 */
				// cropAlgo.finalize();
				// cropAlgo = null;

			}
		}

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

		//cropKeyImages();

		System.err.println("saveImage");
		generateContours();
		// saveImages();

		// saveTestedImages();
		disposeLocal();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	private void generateContours() {
		Set<String> keys = maskTable.keySet();
        for(String key : keys){
		
			Vector<ModelImage> imageVec = maskTable.get(key);
			Vector<String> stringVec = newImageTable.get(key);
			Vector<String> voiStringVec = voiNameHashtable.get(key);
			ModelImage srcImage = imageHashtable.get(key);
			// String dir = stringVec.get(0);
			String dir = voiStringVec.get(0);
			int index = dir.lastIndexOf(File.separator);
			String directory = new String(dir.substring(0, index + 1));
			String fileName = new String(dir.substring(index + 1, dir.length()));
			System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
			VOI voiNewFinal = new VOI((short)0, "voi_reuslt_" + key);
			int[] extents = new int[2];
			for (int j = 0; j < imageVec.size(); j++) {
				ModelImage image = imageVec.get(j);
				extents = image.getExtents();
				String name = "binaryMask_" + image.getImageName();
				// new ViewJFrameImage(image);
				ModelImage resultImage = new ModelImage(ModelImage.BOOLEAN, extents, name);
				float[] thresholds = new float[2];
				thresholds[0] = 240;
				thresholds[1] = 255;
				float fillValue = 0f;
				boolean isInverse = false;
				boolean regionFlag = true;
				int outputType = 1;

				AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(resultImage, image, thresholds, fillValue, outputType, regionFlag, isInverse);
				thresholdAlgo.run();
                
				boolean wholeImage = true;

				
				AlgorithmMorphology2D idObjectsAlgo2D;
				int method = AlgorithmMorphology2D.ID_OBJECTS;

				idObjectsAlgo2D = new AlgorithmMorphology2D(resultImage, 0, 0, method, 0, 0, 0, 0, wholeImage);
				idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
				idObjectsAlgo2D.run();
				idObjectsAlgo2D.finalize();
				idObjectsAlgo2D = null;
				
				resultImage.calcMinMax();
				final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(resultImage);
				VOIExtractionAlgo.run();
				
				// new ViewJFrameImage(resultImage);
				VOIVector v = resultImage.getVOIs();
				if ( v.size() == 0 ) continue;
				
				if (resultImage.getVOIs() != null  ) {
					
					int maxVOIs = 0;
					int voiIndex = 0;
					int contourIndex = 0;
					for ( int k = 0; k < resultImage.getVOIs().size(); k++ ) {
						VOIBaseVector current_va = resultImage.getVOIs().VOIAt(k).getCurves();
						if (current_va != null && current_va.size() > 0) {
							for ( int r = 0; r < current_va.size(); r++ ) {
								VOIBase temp_v = current_va.get(r);
								if ( temp_v.size() > maxVOIs ) {
									maxVOIs = temp_v.size();
									contourIndex = r;
									voiIndex = k;
								}
							}
						}	
					}
					VOIBaseVector current_va = resultImage.getVOIs().VOIAt(voiIndex).getCurves();
						VOIBase current_v = current_va.get(contourIndex);
						VOIBase vTemp = (VOIBase) current_v.clone();
						int nPtsCurrent = current_v.size();

					    // if ( nPtsCurrent != 30 ) continue;

						float[] xPts = new float[nPtsCurrent];
						float[] yPts = new float[nPtsCurrent];
						float[] zPts = new float[nPtsCurrent];

						current_v.exportArrays(xPts, yPts, zPts);

						for (int k = 0; k < nPtsCurrent; k++) {
							zPts[k] = j;
						}

						vTemp.importArrays(xPts, yPts, zPts, nPtsCurrent);
						voiNewFinal.importCurve(vTemp);
						// vTemp = null;
					// }
				}
			
			}  // end for j loop
			
			voiNewFinal.update();
			// save VOI contours
			
			
			
			int[] newExtents = new int[3];		
			newExtents[0] = extents[0];
			newExtents[1] = extents[1];
			newExtents[2] = imageVec.size()-1;
			
			
			srcImage.registerVOI(voiNewFinal);
			new ViewJFrameImage(srcImage);
			smoothVOI30(srcImage, srcImage);
			try {
				FileVOI fileVOI = new FileVOI("result_voi_" + key + ".voi", directory, srcImage);
				fileVOI.writeVOI(voiNewFinal, true);
			} catch (IOException e ) {
				e.printStackTrace();
			}
			
		}
		
	}
	
	public void saveTestedImages() {
		int index;

		System.err.println("keyImageVector.size() = " + keyImageVector1.size());
		
		try {
			// read key images and VOIs
			// for (int i = 0; i < keyImageVector.size(); i++) {
			// int start = 233;
			// int end = keyImageVector.size();
			
			int start = 151;
			int end = 200;
			
			// end = 200;
			int currentIndex = 0;
			for (int imageIndex = start; imageIndex < end; imageIndex++) {
				// read key image
				
				ModelImage targetImageSlice = keyImages.get(imageIndex);
				VOI voiNew = targetImageSlice.getVOIs().elementAt(0);
				// 1) save image
				String sliceDir = saveImageDirectory;
				File sliceDirFile = new File(sliceDir);
				if (!sliceDirFile.isDirectory())
					sliceDirFile.mkdir();
				sliceDir += File.separator;
				File dir = new File(sliceDir);
				if (!dir.isDirectory()) {
					dir.mkdir();
				}
				String imgName = "image" + currentIndex + ".xml";
				// String imageFileToSave = sliceDir +
				// File.separator + imgName;
				// targetImageSlice.saveImage(directory,
				// fileName,
				// fileType, isActive, bDisplayProgress)
				targetImageSlice.saveImage(sliceDir, imgName, FileUtility.XML, false);
				// 2) save VOI
				FileVOI fileVOI = new FileVOI("voi" + currentIndex + ".xml", sliceDir, targetImageSlice);
				fileVOI.writeVOI(voiNew, true);

				currentIndex++;
			}
		
		} catch (IOException e ) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Save the 2D slices and VOIs to user specified dir.
	 */
	public void saveImages() {

            int index = 0;		
		for (int i = 0; i < keyImages.size(); i++) {
			try {

				// if ( i == count ) continue;

				ModelImage cropKeyImage = keyImages.get(i);

				int xDim = cropKeyImage.getExtents()[0];
				int yDim = cropKeyImage.getExtents()[1];
				int zDim = cropKeyImage.getExtents()[2];

				int size_3D = xDim * yDim * zDim;
				float[] imageBuffer = new float[size_3D];

				try {
					cropKeyImage.exportData(0, imageBuffer.length, imageBuffer);
				} catch (Exception e) {
					e.printStackTrace();
				}

				float percentile_left = 0.1f;
				float percentile_right = 0.1f;

				int minIndex = (int) (size_3D * percentile_left);
				int maxIndex = (int) (size_3D * (1.0 - percentile_right));

				Arrays.sort(imageBuffer);
				
				float min = imageBuffer[minIndex];
				float max = imageBuffer[maxIndex];

				int size = xDim * yDim;

				int[] newExtents = new int[2];
				newExtents[0] = xDim;
				newExtents[1] = yDim;

				VOIVector targetImageVOI = cropKeyImage.getVOIs();

				// Vector<ModelImage> ceImageVector = new
				// Vector<ModelImage>();
				for (int j = 3; j <= 20; j++) {

					try {

						System.err.println(" image number = " + (0 + i) + "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
						float[] targetBuffer = new float[size];
						cropKeyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// find the intersection of the lower bound with the
						// VOI.
						Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);

						if (vArray[j].size() > 0) {
							VOIBase v = vArray[j].get(0);
							VOIBase vTemp = (VOIBase) v.clone();
							int nPts = vTemp.size();

							// zero out the z dimension VOI
							float[] xPts = new float[nPts];
							float[] yPts = new float[nPts];
							float[] zPts = new float[nPts];
							float[] zPtsZero = new float[nPts];

							vTemp.exportArrays(xPts, yPts, zPts);

							vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

							VOI voiNew = new VOI((short) 0, "blank");
							voiNew.importCurve(vTemp);

							targetImageSlice.registerVOI(voiNew);
							smoothVOI60(targetImageSlice, targetImageSlice);
							voiNew = targetImageSlice.getVOIs().elementAt(0);

							// 1) save image
							String sliceDir = saveImageDirectory + File.separator + i + File.separator;
							// String sliceDir = saveImageDirectory;
							File sliceDirFile = new File(sliceDir);
							if (!sliceDirFile.isDirectory())
								sliceDirFile.mkdir();

							System.err.println("index = " + index);

							// String imgName = "image_" + index + ".png";
							String imgName = "image_" + j + ".png";
							// targetImageSlice.saveImage(sliceDir, imgName,
							// FileUtility.JIMI, false);
							savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);

							ModelImage maskImage = null;
							maskImage = targetImageSlice.generateBinaryImage(false, false);
							// String maskName = "voi_" + index + ".png";
							String maskName = "voi_" + j + ".png";
							// maskImage.saveImage(sliceDir, maskName,
							// FileUtility.JIMI, false);
							// new ViewJFrameImage(maskImage);
							// if ( true ) break;
							savePNGfile(sliceDir, maskName, maskImage, min, max, true);

							targetBuffer = null;
							targetImageSlice.disposeLocal();
							targetImageSlice = null;

							maskImage.disposeLocal();
							maskImage = null;

							// new ViewJFrameImage(targetImageSlice);
							// new ViewJFrameImage(maskImage);
							// 2) save VOI
							// FileVOI fileVOI = new FileVOI("voi" + i + "_" + j
							// + ".xml", sliceDir, targetImageSlice);
							// fileVOI.writeVOI(voiNew, true);

							index++;

							vTemp = null;
							xPts = null;
							zPts = null;
							zPtsZero = null;

						}

					} catch (IOException e) {

					}
				}

				// if ( true ) break;
				// cropKeyImagesCE.add(ceImageVector);
				// new ViewJFrameImage(coherenceEnhancingDiffusionImage);

			} catch (OutOfMemoryError x) {
				MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			}
		}
		
	}

	private void savePNGfile(String dirName, String fileName, ModelImage srcImage, float minIntensity, float maxIntensity, 
			boolean isMask) {
		File file = null;
		boolean alpha = false;
		boolean gray = true;
		boolean indexed = false;
		try {

			ImageInfo imi = new ImageInfo(512, 512, 8, alpha, gray, indexed);
			ImageLineByte line = new ImageLineByte(imi);
			int yMin = 0, yMax = 512;
			int xMin = 0, xMax = 512;
			int x, y;
			file = new File(dirName + File.separator + fileName);
			if (!file.exists()) {
				file.createNewFile();
			}

			// OutputStream os = new FileOutputStream(savedImageDir +
			// File.separator + imgName);
			OutputStream os = new FileOutputStream(file);

			PngWriter pngw = new PngWriter(os, imi);

			// System.err.println("xMin = " + xMin + "  xMax = " + xMax);

			for (int j = yMin; j < yMax; j++) {

				y = j - yMin;

				for (int i = xMin; i < xMax; i++) {

					x = i - xMin;
					
					
                    if ( isMask == false ) {
                    	float intensity = srcImage.getFloat(i, j);
    					float r = 0;
						if (intensity >= minIntensity && intensity <= maxIntensity) {
							r = (float) ((intensity - minIntensity) * 255d / (maxIntensity - minIntensity));
						} else if (intensity > maxIntensity)
							r = 255;
						else if (intensity < minIntensity)
							r = 0;
						
						 ImageLineHelper.setPixelGray8(line, x, (int) r);
                    } else {
                    	
                    	short intensity = srcImage.getShort(i, j);
                    	
    					if ( intensity == 1 ) {
    						// System.err.println("intensity = " + intensity);
							ImageLineHelper.setPixelGray8(line, x, (int)255 );
						} else { 
							ImageLineHelper.setPixelGray8(line, x, (int)0 );
						}
					
                    	
						 
					}
				    // ImageLineHelper.setPixelGray8(line, x, (int) r);
				    
				}
				// System.err.println();
				pngw.writeRow(line, y);
			}
			pngw.end();
			pngw.close();
			pngw = null;
			os.close();
			os = null;
			line = null;
			imi = null;
			// System.err.println("testing array");

		} catch (Exception e) {
			System.err.println("image find wrong : " + file.getAbsolutePath());
			e.printStackTrace();

			System.exit(0);
		}
	}
	
	/**
	 * Re-oriented the starting and ending VOIs.
	 * 
	 * @param xPts
	 *            voi x coordinate
	 * @param yPts
	 *            voi y coordinate
	 * @param xDim
	 *            image x dimension
	 * @param yDim
	 *            image y dimension
	 */
	private void rotateToStartingPoint_yMid(float xPts[], float yPts[],
			int xDim, int yDim) {
		float yMid = yDim / 2f;
		float xMid = xDim / 2f;
		// 1. find the starting point
		// min point distance to (0, yMid) is the ideal starting point
		int startIndex = 0;
		int len = yPts.length;
		float yDist = -1;
		float xDist = -1;
		float minYDist = 1000;
		for (int i = 0; i < len; i++) {
			yDist = (float) Math.abs(yPts[i] - yMid);
			xDist = (float) Math.abs(xPts[i] - 0);
			if (yDist < minYDist && xDist < xMid) {
				minYDist = yDist;
				startIndex = i;
			}
		}
		// 2. rotate the x, y arrary according to the starting point
		float[] x = new float[len];
		float[] y = new float[len];
		int index = 0;
		for (int i = startIndex; i < len; i++) {
			x[index] = xPts[i];
			y[index] = yPts[i];
			index++;
		}
		for (int i = 0; i < startIndex; i++) {
			x[index] = xPts[i];
			y[index] = yPts[i];
			index++;
		}
		// copy back
		for (int i = 0; i < len; i++) {
			xPts[i] = x[i];
			yPts[i] = y[i];
		}

	}

	// debugger to test. 
	private void rotateToStartingPoint_leftMost(float xPts[], float yPts[],
			int xDim, int yDim) {
		// float yMid = yDim / 2f;
		// float xMid = xDim / 2f;
		// 1. find the starting point
		// min point distance to (0, yMid) is the ideal starting point
		int startIndex = 0;
		int len = yPts.length;
		// float yDist = -1;
		float xDist = -1;
		// float minYDist = 1000;
		float minXDist = 1000;
		for (int i = 0; i < len; i++) {
			// yDist = (float)Math.abs(yPts[i] - yMid);
			xDist = (float) Math.abs(xPts[i] - 0);
			if (xDist < minXDist) {
				minXDist = xDist;
				startIndex = i;
			}
		}
		// 2. rotate the x, y arrary according to the starting point
		float[] x = new float[len];
		float[] y = new float[len];
		int index = 0;
		for (int i = startIndex; i < len; i++) {
			x[index] = xPts[i];
			y[index] = yPts[i];
			index++;
		}
		for (int i = 0; i < startIndex; i++) {
			x[index] = xPts[i];
			y[index] = yPts[i];
			index++;
		}
		// copy back
		for (int i = 0; i < len; i++) {
			xPts[i] = x[i];
			yPts[i] = y[i];
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
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage,
					v.VOIAt(0), 60, false);
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
	 * Convert VOI from one contour to two contours
	 * 
	 * @param srcContour
	 *            single VOI contour
	 * @param targetImageSlice
	 *            target 2D image slice
	 */
	private void generateBoundaryContours(VOIBase srcContour,
			ModelImage targetImageSlice) {
		int sidePointsForTangent = 1;
		int innerDistance = 4;
		int outerDistance = 4;
		boolean doInner = true;
		boolean doOuter = true;
		int index;
		int i;
		int j;
		int k;
		int m;
		if (srcContour == null || (srcContour.size() == 0)) {
			return;
		}
		int slice = (int) srcContour.elementAt(0).Z;

		int nPoints = srcContour.size();
		Vector3f point;
		float xPoints[] = new float[nPoints + 2 * sidePointsForTangent];
		float yPoints[] = new float[nPoints + 2 * sidePointsForTangent];
		Vector<Vector3f> innerV = new Vector<Vector3f>();
		Vector<Vector3f> outerV = new Vector<Vector3f>();
		float tangentX;
		float tangentY;
		float xCenteredPoints[] = new float[2 * sidePointsForTangent + 1];
		float yCenteredPoints[] = new float[2 * sidePointsForTangent + 1];
		double xSqSum;
		double ySqSum;
		double xySum;
		double var;
		double x1t;
		double x2t;
		double y1t;
		double y2t;
		double slope;
		double d1;
		double d2;
		double xDist;
		double yDist;
		for (i = 0; i < nPoints; i++) {
			point = (srcContour.get(i));
			xPoints[i + sidePointsForTangent] = point.X;
			yPoints[i + sidePointsForTangent] = point.Y;
		}
		for (i = sidePointsForTangent - 1, j = 0; i >= 0; i--, j++) {
			xPoints[i] = xPoints[nPoints - 1 - j];
			yPoints[i] = yPoints[nPoints - 1 - j];
		}
		for (i = nPoints, j = 0; i <= nPoints + sidePointsForTangent - 1; i++, j++) {
			xPoints[i] = xPoints[j];
			yPoints[i] = yPoints[j];
		}
		for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent
				+ nPoints - 1; i++, j++) {
			if (sidePointsForTangent == 1) {
				tangentX = (xPoints[i + 1] - xPoints[i - 1]) / 2.0f;
				tangentY = (yPoints[i + 1] - yPoints[i - 1]) / 2.0f;
				if (tangentY == 0.0f) {
					slope = Double.POSITIVE_INFINITY;
				} else {
					slope = -tangentX / tangentY;
				}
			} // if (sidePointsForTangent == 1)
			else { // sidePointsForTangent > 1
					// Center all points for tangent point touching curve at (0,
					// 0)
					// That is, use an x axis and a y axis going thru the
					// tangent point
				for (k = 0, m = i - sidePointsForTangent; m <= i
						+ sidePointsForTangent; m++, k++) {
					xCenteredPoints[k] = xPoints[m] - xPoints[i];
					yCenteredPoints[k] = yPoints[m] - yPoints[i];
				}
				xSqSum = 0.0;
				ySqSum = 0.0;
				xySum = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					xSqSum += xCenteredPoints[k] * xCenteredPoints[k];
					ySqSum += yCenteredPoints[k] * yCenteredPoints[k];
					xySum += xCenteredPoints[k] * yCenteredPoints[k];
				}
				if (xySum != 0.0) {
					var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum
							+ xSqSum * xSqSum + 4.0 * xySum * xySum);
					x1t = 0.5 * ((-ySqSum + xSqSum + var) / xySum);
					x2t = 0.5 * ((-ySqSum + xSqSum - var) / xySum);
					y1t = 1.0;
					y2t = 1.0;
				} else {
					// If all points are symmetric to either this new x axis or
					// this new y axis, then
					// their product sum is 0 and the tangentX, tangentY must be
					// 1,0 or 0,1
					x1t = 1.0;
					x2t = 0.0;
					y1t = 0.0;
					y2t = 1.0;
				}
				// x1t, y1t and x2t, y2t are perpindicular. To find the
				// solution, calculate the sum of
				// distances from the curve points to the line for the 2 cases
				// The shortest distance is the correct solution
				// Distance from AX + BY + C = 0 to P1 is
				// abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
				// Here A = slope, B = -1, and C = 0.
				d1 = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					if (x1t == 0.0) {
						// Infinite slope thru (0,0)
						d1 += Math.abs(yCenteredPoints[k]);
					} else if (y1t == 0.0) {
						// Zero slope thru (0, 0)
						d1 += Math.abs(xCenteredPoints[k]);
					} else {
						slope = y1t / x1t;
						d1 += Math
								.abs((slope * xCenteredPoints[k] - yCenteredPoints[k])
										/ Math.sqrt(slope * slope + 1));
					}
				}
				d2 = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					if (x2t == 0.0) {
						// Infinite slope thru (0,0)
						d2 += Math.abs(yCenteredPoints[k]);
					} else if (y2t == 0.0) {
						// Zero slope thru (0, 0)
						d2 += Math.abs(xCenteredPoints[k]);
					} else {
						slope = y2t / x2t;
						d2 += Math
								.abs((slope * xCenteredPoints[k] - yCenteredPoints[k])
										/ Math.sqrt(slope * slope + 1));
					}
				}
				if (d1 < d2) {
					tangentX = (float) x1t;
					tangentY = (float) y1t;
				} else {
					tangentX = (float) x2t;
					tangentY = (float) y2t;
				}
				if (tangentY == 0.0f) {
					slope = Double.POSITIVE_INFINITY;

				} else {
					slope = -tangentX / tangentY;
				}
			} // else sidePointsForTangent > 1
			if (doInner) {
				if (Double.isInfinite(slope)) {
					if (srcContour.contains(xPoints[i], yPoints[i]
							+ innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i]
								+ innerDistance, slice);
						innerV.add(point);
					} else if (srcContour.contains(xPoints[i], yPoints[i]
							- innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i]
								- innerDistance, slice);
						innerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = innerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (srcContour.contains((float) (xPoints[i] + xDist),
							(float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist),
								(float) (yPoints[i] + yDist), slice);
						innerV.add(point);
					} else if (srcContour.contains(
							(float) (xPoints[i] - xDist),
							(float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist),
								(float) (yPoints[i] - yDist), slice);
						innerV.add(point);
					}
				}
			} // if (doInner)
			if (doOuter) {
				if (Double.isInfinite(slope)) {
					if (!srcContour.contains(xPoints[i], yPoints[i]
							+ outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i]
								+ outerDistance, slice);
						outerV.add(point);
					} else if (!srcContour.contains(xPoints[i], yPoints[i]
							- outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i]
								- outerDistance, slice);
						outerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = outerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (!srcContour.contains((float) (xPoints[i] + xDist),
							(float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist),
								(float) (yPoints[i] + yDist), slice);
						outerV.add(point);
					} else if (!srcContour.contains(
							(float) (xPoints[i] - xDist),
							(float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist),
								(float) (yPoints[i] - yDist), slice);
						outerV.add(point);
					}
				}
			} // if (doOuter)
		} // for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent +
			// nPoints - 1; i++, j++)

		short sID = (short) (targetImageSlice.getVOIs().getUniqueID());
		String kName = srcContour.getClass().getName();
		index = kName.lastIndexOf('.') + 1;
		kName = kName.substring(index);
		VOI resultVOI = new VOI(sID, kName + "_" + sID, srcContour.getType(),
				-1);

		if (doInner) {
			Vector3f pt[] = new Vector3f[innerV.size()];
			for (i = 0; i < innerV.size(); i++) {
				pt[i] = innerV.elementAt(i);
			}
			resultVOI.importCurve(pt);
		}
		if (doOuter) {
			Vector3f pt[] = new Vector3f[outerV.size()];
			for (i = 0; i < outerV.size(); i++) {
				pt[i] = outerV.elementAt(i);
			}
			resultVOI.importCurve(pt);
		}

		targetImageSlice.getVOIs().removeElementAt(0);

		targetImageSlice.registerVOI(resultVOI);

	}

	/**
	 * Crop key images.
	 */
	public void cropKeyImages() {

		int zDim;

		// Crop key images. VOIs
		for (int i = 0; i < keyImages.size(); i++) {

			ModelImage image = keyImages.get(i);

			int[] extents = (int[]) image.getFileInfo(0).getExtents();
			zDim = extents[2] - 1;

			// manually set the crop image starting point and ending point
			boxYmin = 0;
			boxYmax = 512 - 1;

			boxXmin = 0;
			boxXmax = 512 - 1;

			xBounds[0] = boxXmin;
			xBounds[1] = boxXmax;

			yBounds[0] = boxYmin;
			yBounds[1] = boxYmax;

			zBounds[0] = 0;
			zBounds[1] = zDim;

			int borderSize = 0;
			try {
				int[] destExtents = null;
				if (image.getNDims() == 3) {

					if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 3D
																	// image
						// to 2D image
						destExtents = new int[2];
						destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
								+ (2 * borderSize);
						destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
								+ (2 * borderSize);
					} else {
						destExtents = new int[3];
						destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
								+ (2 * borderSize);
						destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
								+ (2 * borderSize);
						destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
					}
				} else {
					return;
				}

				System.err.println("destExtents[0] = " + destExtents[0]
						+ "  destExtents[1] = " + destExtents[1]);

				// create crop images
				cropKeyImages.add(i,
						new ModelImage(image.getType(), destExtents,
								makeImageName(image.getImageName(), "_crop")));

				int[] xCrop = new int[] { 0, 0 };
				int[] yCrop = new int[] { 0, 0 };
				int[] zCrop = new int[] { 0, 0 };
				if (destExtents.length > 0) {
					xCrop[0] = -1 * (xBounds[0] - borderSize);
					xCrop[1] = -1 * (xBounds[1] - destExtents[0] - 1);
				}
				if (destExtents.length > 1) {
					yCrop[0] = -1 * (yBounds[0] - borderSize);
					yCrop[1] = -1 * (yBounds[1] - destExtents[1] - 1);
				}
				if (destExtents.length > 2) {
					zCrop[0] = -1 * (zBounds[0]);
					zCrop[1] = -1 * (zBounds[1] - destExtents[2] - 1);
				} else // 3D to 2D
				{
					zCrop[0] = -1 * (zBounds[0]);
					zCrop[1] = -1 * (zBounds[1] + 1);
				}

				System.err.println("xCrop[0] = " + xCrop[0] + "   xCrop[1] = "
						+ xCrop[1]);
				System.err.println("yCrop[0] = " + yCrop[0] + "   yCrop[1] = "
						+ yCrop[1]);

				cropAlgo = new AlgorithmAddMargins(image, cropKeyImages.get(i),
						xCrop, yCrop, zCrop);

				cropAlgo.addListener(this);

				// Hide the dialog since the algorithm is about to run.
				setVisible(false);

				cropAlgo.run();

			} catch (OutOfMemoryError e) {
				MipavUtil
						.displayError("Dialog Crop: unable to allocate enough memory");

				return;
			}
		} // end for loop

		// crop target image

	}

	/**
	 * load image files and voi files
	 */
	public void loadFiles() {
		// readFiles();
		readFile();
		System.err.println("finish image I/O");

	}

	/**
	 * Debugger to test dir deletion.
	 * 
	 * @param dir
	 * @return
	 */
	public static boolean deleteDir(File dir) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				boolean success = deleteDir(new File(dir, children[i]));
				if (!success) {
					return false;
				}
			}
		}
		return dir.delete();
	}

	
	
	public void readFile() {

		try {
			  
			Set<String> keys = imageNameHashtable.keySet();
	        for(String key: keys){
	            Vector<String> imageDicomSet = imageNameHashtable.get(key);
	            
	            FileIO fileIO = new FileIO();
	            String imageFullName = imageDicomSet.get(0);
	            int index = imageFullName.lastIndexOf(File.separator);
	            
                String fileName = imageFullName.substring(index+1, imageFullName.length());
                String directory = imageFullName.substring(0, index+1);
                // System.err.println("filename = " + fileName);
                // System.err.println("directory = " + directory);
                
                
                boolean multiFile = true;
                ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
                image.setImageName(key);
                imageHashtable.put(key, image);
                
                /*
                Vector<String> voiSet = voiNameHashtable.get(key);
                VOI[] voi = null;
                for ( int k = 0; k < voiSet.size(); k++ ) {
	                String voiFullName = voiSet.get(k);
	                if ( voiFullName.contains("cg")) {
		                index = voiFullName.lastIndexOf(File.separator);
		                String voiName = voiFullName.substring(index+1, voiFullName.length());
		                String voiDirectory = voiFullName.substring(0, index+1);
		                FileVOI fileVOI = null;
						fileVOI = new FileVOI(voiName, voiDirectory, image);
						voi = fileVOI.readVOI(false);
			            // image.registerVOI(voi[0]);
	                }
                }
                voiHashtable.put(key, voi[0]);
                */ 
	            // new ViewJFrameImage(image);
	            // pause();
	        }


			
		   
			Set<String> maskKeys = maskImageTable.keySet();
	        for(String maskKey: maskKeys){
				Vector<String> stringVec = maskImageTable.get(maskKey);
				System.err.println("index = " + maskKey + ":");
				Vector<ModelImage> imageVec = new Vector<ModelImage>();
				
				for ( int j = 0; j < stringVec.size(); j++ ) {
					String dir = stringVec.get(j);
					int index = dir.lastIndexOf(File.separator);
					String directory = new String(dir.substring(0, index + 1));
					String fileName = new String(dir.substring(index + 1, dir.length()));
					System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
					FileIO keyImageIO = new FileIO();
				    imageVec.add(keyImageIO.readImage(fileName, directory));
				}
				
				maskTable.put(maskKey, imageVec);
				
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
		imageSelectionPanel.setLayout(new GridLayout(3, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Auto Train"));

		// axis label
		String[] axisStrings = { "Axial", "Saggital", "Coronal" };

		axisList = new JComboBox(axisStrings);
		axisList.setSelectedIndex(0);
		axisList.setActionCommand("SetAxis");
		axisList.addActionListener(this);

		labelAxis = new JLabel("Axis: ");
		labelAxis.setFont(serif12);
		labelAxis.setForeground(Color.black);

		imageSelectionPanel.add(labelAxis, gbc);

		gbc.gridx = 1;
		imageSelectionPanel.add(axisList, gbc);

		gbc.gridx = 2;
		JLabel emptyLabel = new JLabel("");
		imageSelectionPanel.add(emptyLabel, gbc);

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