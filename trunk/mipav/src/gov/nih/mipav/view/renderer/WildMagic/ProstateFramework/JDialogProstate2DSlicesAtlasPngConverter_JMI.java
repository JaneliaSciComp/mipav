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
public class JDialogProstate2DSlicesAtlasPngConverter_JMI extends JDialogBase
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

	Hashtable<Integer, ModelImage> imageHashtableWp_ext = new Hashtable<Integer, ModelImage>();
	Hashtable<Integer, ModelImage> imageHashtableCg_ext = new Hashtable<Integer, ModelImage>();
	Hashtable<Integer, ModelImage> cedTable_ext = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> voiTable_ext = new Hashtable<Integer, ModelImage>();
	
	Hashtable<Integer, ModelImage> scaleImageTable_ext = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> scaleIntensityTable_ext = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> scaleCedTable_ext = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> scaleVoiTable_ext = new Hashtable<Integer, ModelImage>();
	
	
	Hashtable<Integer, ModelImage> cropTable_ext = new Hashtable<Integer, ModelImage>();
	Hashtable<Integer, ModelImage> cropVoiTable_ext = new Hashtable<Integer, ModelImage>();
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesAtlasPngConverter_JMI(Frame theParentFrame) {
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
			// sortKeyImage_1();
		    sortKeyImage_2();
		    sortKeyImage_3();
		    sortKeyImage_4();
			sortKeyImage_5();
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
		/*
		String keyImageName;
		keyImageChooser.setDialogTitle("Open Key Images Directory");
		keyImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = keyImageChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			keyImageName = keyImageChooser.getSelectedFile().getName();

			keyImageDirectory = String.valueOf(keyImageChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ keyImageName
					+ File.separatorChar;

			textFieldKeyImage.setText(keyImageDirectory);

			File fileDir = new File(keyImageDirectory);
			System.err.println("check = " + keyImageDirectory);
			traverse(fileDir);
		} else {
			return;
		}
        */
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
	    File fileDir_2 = new File("/scratch/aam_test/fold2");
	    traverse_folder_2(fileDir_2);
		File fileDir_3 = new File("/scratch/aam_test/fold3");
		traverse_folder_3(fileDir_3);
		File fileDir_4 = new File("/scratch/aam_test/fold4");
		traverse_folder_4(fileDir_4);
		File fileDir_5 = new File("/scratch/aam_test/fold5");
	    traverse_folder_5(fileDir_5);
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
			System.err.println(dir.toString());
			keyImageVector4.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			System.err.println(dir.toString());
			keyImageVOIVector4.add(dir.toString());
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

	private void traverse_folder_5(File dir) {
		processDir_folder_5(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_5(new File(dir, children[i]));
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
		// saveImages();
		
		runMask_ext();
		runCrop_ext();
		runScaleIntensity_ext();
		runCED_ext();
		
		disposeMemory();
		System.gc();
		System.err.println("saveImage");
		
		crossValidationTrain();
		// saveTestedImages();
		// disposeLocal();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}
	
	

	public void crossValidationTrain() {

		int dataSize = scaleIntensityTable_ext.size();
		
		System.err.println("table size = "+ dataSize);
		
		int[] index = new int[1];
	
		index[0] = 0;
       
        // cropVoiTable_ext
		// cedTable_ext
		// scaleIntensityTable_ext
		
        int len = keyImages.size();
		
		for (int i = 0; i < len; i++) {
        
	    	System.err.println("ruida index = " + i );
        	saveImages_ext(i, index, "train", scaleIntensityTable_ext); 
        	saveImages_ext(i, index, "train", cedTable_ext); 
        }
		
        
	}
	
	public void saveImages_ext(int i, int []index, String folderName, Hashtable<Integer, ModelImage> whichTable) {
		try {

			// if ( i == count ) continue;
			
			ModelImage keyImage = whichTable.get(i);
			// ModelImage cedImage = scaleCedTable.get(key);
			ModelImage keyImageVOI = cropVoiTable_ext.get(i);
			// new ViewJFrameImage(keyImageVOI);
		
			int xDim = keyImage.getExtents()[0];
			int yDim = keyImage.getExtents()[1];
			int zDim = keyImage.getExtents()[2];

			System.err.println("zim = " + zDim);
			
			int size_3D = xDim * yDim * zDim;
			float[] imageBuffer = new float[size_3D];

			try {
				keyImage.exportData(0, imageBuffer.length, imageBuffer);
			} catch (Exception e) {
				e.printStackTrace();
			}

			float percentile_left = 0.01f;
			float percentile_right = 0.01f;

			int minIndex = (int) (size_3D * percentile_left);
			int maxIndex = (int) (size_3D * (1.0 - percentile_right));

			Arrays.sort(imageBuffer);

			float min = imageBuffer[minIndex];
			float max = imageBuffer[maxIndex];

			int size = xDim * yDim;

			int[] newExtents = new int[2];
			newExtents[0] = xDim;
			newExtents[1] = yDim;

			VOIVector targetImageVOI = keyImage.getVOIs();

			// Vector<ModelImage> ceImageVector = new
			// Vector<ModelImage>();
			
			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator;
			File sliceDirFile = new File(sliceDir);
			if (!sliceDirFile.isDirectory())
				sliceDirFile.mkdir();
			
			for (int j = 0; j < zDim; j++) {

				try {

					System.err.println(" image number = " + index[0] + "   slice number = " + j);

					ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
					float[] targetBuffer = new float[size];
					keyImage.exportData(j * size, size, targetBuffer);
					targetImageSlice.importData(0, targetBuffer, true);

					ModelImage maskImage = new ModelImage(ModelStorageBase.SHORT, newExtents, "voi" + j);
					int[] voiBuffer = new int[size];
					keyImageVOI.exportData(j * size, size, voiBuffer);
					maskImage.importData(0, voiBuffer, true);
					
						System.err.println("index = " + index[0]);

						String imgName = "image_" + index[0] + ".png";
						// String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						
						String maskName = "voi_" + index[0] + ".png";
						// String maskName = "voi_" + j + ".png";
						// maskImage.saveImage(sliceDir, maskName,
						// FileUtility.JIMI, false);
						// new ViewJFrameImage(maskImage);
						// if ( true ) break;
						savePNGfile(sliceDir, maskName, maskImage, min, max, xDim, yDim, true);

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

						index[0]++;

					
					 

				} catch (IOException e) {
					e.printStackTrace();
				}
			}

			// if ( true ) break;
			// cropKeyImagesCE.add(ceImageVector);
			// new ViewJFrameImage(coherenceEnhancingDiffusionImage);
			imageBuffer = null;
		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			x.printStackTrace();
		}
	}

	private void savePNGfile(String dirName, String fileName, ModelImage srcImage, float minIntensity, float maxIntensity, 
			int xDim, int yDim, boolean isMask) {
		File file = null;
		boolean alpha = false;
		boolean gray = true;
		boolean indexed = false;
		try {

			ImageInfo imi = new ImageInfo(xDim, yDim, 8, alpha, gray, indexed);
			ImageLineByte line = new ImageLineByte(imi);
			int yMin = 0, yMax = xDim;
			int xMin = 0, xMax = yDim;
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
	
	public void disposeMemory() {
		int len = keyImages.size();
		
		for (int i = 0; i < len; i++) {

			ModelImage wpImage = keyImages.get(i);
			wpImage.disposeLocal();
			wpImage = null;
			
			
			ModelImage voiImage = voiTable_ext.get(i);
			voiImage.disposeLocal();
			voiImage = null;
			
		}
		imageHashtableWp_ext = null;
		voiTable_ext = null;
	 
	}
	
	public void runCED_ext() {
		
		int len = scaleIntensityTable_ext.size();
		for (int i = 0; i < len; i++) {
			 ModelImage keyImage = scaleIntensityTable_ext.get(i);
			ModelImage cedImage = calculateCoherenceEnhancingDiffusion(keyImage);
			// new ViewJFrameImage(cedImage);
			cedTable_ext.put(i, cedImage);
		}
	}
	
	public void runMask_ext() {
		
		int len = keyImages.size();
		
		for (int i = 0; i < len; i++) {

			ModelImage wpImage = keyImages.get(i);

			ModelImage wpMaskImage = null;

			wpMaskImage = wpImage.generateBinaryImage(false, false);

			voiTable_ext.put(i, wpMaskImage);

		}

	}

	public void runCrop_ext() {
		
		int len = keyImages.size();
		for (int i = 0; i < len; i++) {
			
			ModelImage keyImage = keyImages.get(i);
			ModelImage cropImage = cropImage(keyImage);
			cropTable_ext.put(i, cropImage);
			// new ViewJFrameImage(cropImage);
			
			ModelImage voiImage = voiTable_ext.get(i);
			// new ViewJFrameImage(voiImage);
			ModelImage cropVoiImage = cropImage_ext(voiImage);
			// new ViewJFrameImage(cropVoiImage);
			cropVoiTable_ext.put(i, cropVoiImage);
			
		}
		
	}
	
	public void runScaleIntensity_ext() {
		
		int len = keyImages.size();
		
		for (int i = 0; i < len; i++) {
			ModelImage keyImage = cropTable_ext.get(i);
			ModelImage scaleIntenImage = scaleIntensity(keyImage);
		    // new ViewJFrameImage(scaleIntenImage);
			scaleIntensityTable_ext.put(i, scaleIntenImage);		
		}
	
	}
	
	private ModelImage cropImage(ModelImage image) {

		int[] destExtents = new int[3];
		int[] xBounds = new int[2];
		int[] yBounds = new int[2];
		int[] zBounds = new int[2];
		
		int[] extents = image.getExtents();
		int cropPixels = (int)Math.ceil(((double)extents[0] * 0.25d));
		// int cropPixels = 128;
		
		xBounds[0] = cropPixels;
		xBounds[1] = cropPixels;
		yBounds[0] = cropPixels;
		yBounds[1] = cropPixels;
		zBounds[0] = 0;
		zBounds[1] = 0;
		
        destExtents[0] = Math.abs(image.getExtents()[0] - xBounds[1] - xBounds[0]);
        destExtents[1] = Math.abs(image.getExtents()[1] - yBounds[1] - yBounds[0]);
        destExtents[2] = Math.abs(image.getExtents()[2] - zBounds[1] - zBounds[0]);

        ModelImage resultImage = new ModelImage(image.getType(), destExtents, makeImageName(image.getImageName(), "_crop"));
		
        xBounds[0] *= -1;
        xBounds[1] *= -1;
        yBounds[0] *= -1;
        yBounds[1] *= -1;
        zBounds[0] *= -1;
        zBounds[1] *= -1;
        
        AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(image, resultImage, xBounds, yBounds, zBounds);
        
        cropAlgo.run();
        
        cropAlgo = null;
        return resultImage;
		
	}
 
	private ModelImage cropImage_ext(ModelImage image) {

		int[] destExtents = new int[3];
		int[] xBounds = new int[2];
		int[] yBounds = new int[2];
		int[] zBounds = new int[2];
		
		int[] extents = image.getExtents();
		int cropPixels = (int)Math.ceil(((double)extents[0] * 0.25d));
		// int cropPixels = 128;
		
		xBounds[0] = cropPixels;
		xBounds[1] = cropPixels;
		yBounds[0] = cropPixels;
		yBounds[1] = cropPixels;
		zBounds[0] = 0;
		zBounds[1] = 0;
		
        destExtents[0] = Math.abs(image.getExtents()[0] - xBounds[1] - xBounds[0]);
        destExtents[1] = Math.abs(image.getExtents()[1] - yBounds[1] - yBounds[0]);
        destExtents[2] = Math.abs(image.getExtents()[2] - zBounds[1] - zBounds[0]);

        ModelImage resultImage = new ModelImage(ModelStorageBase.SHORT, destExtents, makeImageName(image.getImageName(), "_crop"));
		
        xBounds[0] *= -1;
        xBounds[1] *= -1;
        yBounds[0] *= -1;
        yBounds[1] *= -1;
        zBounds[0] *= -1;
        zBounds[1] *= -1;
        
        AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(image, resultImage, xBounds, yBounds, zBounds);
        
        cropAlgo.run();
        cropAlgo = null;
        // new ViewJFrameImage(resultImage);
        return resultImage;
		
	}
	
	public ModelImage scaleIntensity(ModelImage image) {

		int[] destExtents = new int[3];
		
		destExtents[0] = image.getExtents()[0];
		destExtents[1] = image.getExtents()[1];
		destExtents[2] = image.getExtents()[2];
		int dataType = image.getType();
		
		// ModelImage resultImage = new ModelImage(dataType, destExtents, "scale_inten");
		ModelImage resultImage = (ModelImage)image.clone();
		
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int zDim = image.getExtents()[2];

		int size_3D = xDim * yDim * zDim;
		float[] imageBuffer = new float[size_3D];
		float[] origBuffer = new float[size_3D];

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);
			image.exportData(0, imageBuffer.length, origBuffer);
		} catch (Exception e) {
			e.printStackTrace();
		}
		

        float percentile_10 = 0.05f;
		float percentile_75 = 0.75f;
		float percentile_25 = 0.25f;
        float percentile_50 = 0.50f;
        float percentile_90 = 0.95f;
		
        int index_50 = (int) (size_3D * percentile_50);
		int index_75 = (int) (size_3D * percentile_75);
		int index_25 = (int) (size_3D * percentile_25);
		int index_10 = (int) (size_3D * percentile_10);
		int index_90 = (int) (size_3D * percentile_90);
		
		Arrays.sort(imageBuffer);

		float inten_75 = imageBuffer[index_75];
		float inten_25 = imageBuffer[index_25];
        float median = imageBuffer[index_50];
        float min = imageBuffer[index_10];
        float max = imageBuffer[index_90];
		
        float contrast = median + 2f * ( inten_75 - inten_25);
        
        for ( int i = 0; i < size_3D; i++ ) {
        	
        	if (origBuffer[i] <=  min) {
        		origBuffer[i] = min;
        	}
        	
        	if (origBuffer[i] >= max) {
        		origBuffer[i] = max;
        	}
        	
        	origBuffer[i] = 1000f * ( origBuffer[i] / contrast);
        }
        
        try {
        	resultImage.importData(0, origBuffer, true);
        } catch (IOException e) {
        	e.printStackTrace();
        }

        return resultImage;
	}
	
	private ModelImage calculateCoherenceEnhancingDiffusion(ModelImage inImage) {

		int numIterations;
		float diffusitivityDenom;
		float derivativeScale;
		float gaussianScale;
		boolean do25D;
		boolean entireImage;

		derivativeScale = 0.5f;
		diffusitivityDenom = 0.001f;
		gaussianScale = 2.0f;
		numIterations = 20;
		do25D = true;
		entireImage = true;
		ModelImage coherenceEnhancingDiffusionImage;

		try {
			coherenceEnhancingDiffusionImage = (ModelImage) inImage.clone();
			int type = coherenceEnhancingDiffusionImage.getType();
			// coherenceEnhancingDiffusionImage.setType(ModelStorageBase.FLOAT);
			// coherenceEnhancingDiffusionImage.reallocate(ModelStorageBase.FLOAT);
			coherenceEnhancingDiffusionImage.setType(type);
			coherenceEnhancingDiffusionImage.reallocate(type);

			AlgorithmCoherenceEnhancingDiffusion coherenceEnhancingDiffusionAlgo = new AlgorithmCoherenceEnhancingDiffusion(
					coherenceEnhancingDiffusionImage, inImage, numIterations,
					diffusitivityDenom, derivativeScale, gaussianScale, do25D,
					entireImage);

			coherenceEnhancingDiffusionAlgo.addListener(this);

			coherenceEnhancingDiffusionAlgo.run();

			coherenceEnhancingDiffusionAlgo.setCompleted(true);
			coherenceEnhancingDiffusionAlgo.finalize();
			coherenceEnhancingDiffusionAlgo = null;

		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog GaborFilter: unable to allocate enough memory");

			return null;
		}

		return coherenceEnhancingDiffusionImage;
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

				float percentile_left = 0.05f;
				float percentile_right = 0.05f;

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
				for (int j = 0; j <= zDim; j++) {

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
							// String sliceDir = saveImageDirectory + File.separator + i + File.separator;
							String sliceDir = saveImageDirectory;
							File sliceDirFile = new File(sliceDir);
							if (!sliceDirFile.isDirectory())
								sliceDirFile.mkdir();

							System.err.println("index = " + index);

							String imgName = "image_" + index + ".png";
							// String imgName = "image_" + j + ".png";
							// targetImageSlice.saveImage(sliceDir, imgName,
							// FileUtility.JIMI, false);
							savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);

							ModelImage maskImage = null;
							maskImage = targetImageSlice.generateBinaryImage(false, false);
							String maskName = "voi_" + index + ".png";
							// String maskName = "voi_" + j + ".png";
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

						} else {
							// 1) save image
							// String sliceDir = saveImageDirectory + File.separator + i + File.separator;
							String sliceDir = saveImageDirectory;
							File sliceDirFile = new File(sliceDir);
							if (!sliceDirFile.isDirectory())
								sliceDirFile.mkdir();

							System.err.println("index = " + index);

							String imgName = "image_" + index + ".png";
							// String imgName = "image_" + j + ".png";
							// targetImageSlice.saveImage(sliceDir, imgName,
							// FileUtility.JIMI, false);
							savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);

							ModelImage maskImage = null;
							// maskImage = targetImageSlice.generateBinaryImage(false, false);
							// maskImage = new ModelImage(imageA.getType(), destExtents, imageA.getImageName());
							maskImage = new ModelImage(ModelStorageBase.SHORT, newExtents, "mask" + j);
							String maskName = "voi_" + index + ".png";
							// String maskName = "voi_" + j + ".png";
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

		int index;

		// System.err.println("keyImageVector.size() = " + keyImageVector.size());
		
		try {
			// read key images and VOIs
			// for (int i = 0; i < keyImageVector.size(); i++) {
			int start = 0;
			// int end = keyImageVector.size();
			int end = 50; 
			
			int currentIndex = 0;
			
			/*
			for (int imageIndex = start; imageIndex < end; imageIndex++) {
				// read key image
				String dir = keyImageVector1.get(imageIndex);
				
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector1.get(imageIndex);
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
		    */
			
		   
			for (int imageIndex = start; imageIndex < end; imageIndex++) {
				// read key image
				String dir = keyImageVector2.get(imageIndex);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector2.get(imageIndex);
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
		 
			
			int size = keyImageVector3.size();
			for (int imageIndex = start; imageIndex < size; imageIndex++) {
				// read key image
				String dir = keyImageVector3.get(imageIndex);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector3.get(imageIndex);
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
			 
		   
			for (int imageIndex = start; imageIndex < end; imageIndex++) {
				// read key image
				String dir = keyImageVector4.get(imageIndex);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector4.get(imageIndex);
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
		   
			
			
			for (int imageIndex = start; imageIndex < end; imageIndex++) {
				// read key image
				String dir = keyImageVector5.get(imageIndex);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector5.get(imageIndex);
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
		     
			
			/*
			start = 201;
			end = keyImageVector.size();
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
            */
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