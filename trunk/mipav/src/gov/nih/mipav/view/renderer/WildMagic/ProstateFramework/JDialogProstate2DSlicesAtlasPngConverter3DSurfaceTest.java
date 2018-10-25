package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.vecmath.Point3f;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import ar.com.hjg.pngj.*;
import ar.com.hjg.pngj.chunks.PngChunkTextVar;

import java.net.URLDecoder;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.apache.commons.io.*;
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
public class JDialogProstate2DSlicesAtlasPngConverter3DSurfaceTest extends JDialogBase implements AlgorithmInterface {

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
	
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesAtlasPngConverter3DSurfaceTest(Frame theParentFrame) {
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
		 * for (i = 0; i < keyImageVector.size(); i++) { String temp =
		 * keyImageVector.get(i); temp = null; } keyImageVector = null;
		 */

		for (i = 0; i < keyImages.size(); i++) {
			ModelImage temp = keyImages.get(i);
			temp.disposeLocal();
		}
		keyImages = null;

		/*
		 * for (i = 0; i < keyImageVOIVector.size(); i++) { String temp =
		 * keyImageVOIVector.get(i); temp = null; } keyImageVOIVector = null;
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
			// printTable();
			// sortKeyImage_1();
			// sortKeyImage_2();
			// sortKeyImage_3();
			// sortKeyImage_4();
			// sortKeyImage_5();
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
	
	}


	private void traverse_Layer_extra(File dir) {
		// processDir_folder_1(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			// for (int i = 0; i < 1; i++) {
			for (int i = 0; i < children.length; i++) {
			    
				// System.err.println("current index = " + i);
				// nSystem.err.println(dir + File.separator + children[i]);
			
				traverse_patientID(new File(dir + File.separator + children[i]), dir + File.separator + children[i]);
				/*
				imageNameHashtable.put(children[i], new Hashtable<String, Vector<String>>());
				voiNameHashtable.put(children[i], new Hashtable<String, String>());
				traverse_firstLayer(dir, children[i]);
				// traverse_folder_1(new File(dir, children[i]));
				System.err.println();
				*/ 
			}
			
		}
	}

	private void traverse_patientID(File dir, String patientID ) {
		if ( dir.isDirectory() ) {
			String[] children = dir.list();
			for ( int i = 0; i < children.length; i++ ) {
				System.err.println(dir + File.separator + children[i]);
				traverse_date(new File(dir + File.separator + children[i]), patientID, children[i]);
			}
		}
	}
	
	private void traverse_date(File dir, String patientID, String date) {
		String hashID = patientID + File.separator + date;
		imageNameHashtableExtra.put(hashID, new Hashtable<String, Vector<String>>());
		traverse_orientation(dir, hashID);
	}
	
	private void traverse_orientation(File dir, String hashID) {
		if ( dir.isDirectory() ) {
			String[] children = dir.list();
			for ( int i = 0; i < children.length; i++ ) {
				
		        String decodeChildName = MipavUtil.decodeStr(children[i]);
		        if ( decodeChildName.toLowerCase().contains("ax")) {
		        	imageNameHashtableExtra.get(hashID).put("axial", new Vector<String>());
		        	traverse_serial(new File(dir + File.separator + decodeChildName), hashID, "axial");
		        } else if ( decodeChildName.toLowerCase().contains("sag")) {
		        	imageNameHashtableExtra.get(hashID).put("sagittal", new Vector<String>());
		        	traverse_serial(new File(dir + File.separator + decodeChildName), hashID, "sagittal");
		        } else if ( decodeChildName.toLowerCase().contains("cor")) {
		        	imageNameHashtableExtra.get(hashID).put("coronal", new Vector<String>());
		        	traverse_serial(new File(dir + File.separator + decodeChildName), hashID, "coronal");
		        }
			}
		}
	}
	
	private void traverse_serial(File dir, String hashID, String orientation) {
		Hashtable<String, Vector<Integer>> serialTable = new Hashtable<String, Vector<Integer>>();
		serialTable.put(orientation, new Vector<Integer>());
		if ( dir.isDirectory() ) {
			String[] children = dir.list(); 
			int min = Integer.MAX_VALUE;
			int serialNum;
			for ( int i = 0; i < children.length; i++ ) {
				// String decodedPathChild = MipavUtil.decodeStr(dir + File.separator + children[i]+ File.separator);	
				// System.err.println(decodedPathChild);
				// imageNameHashtableExtra.get(hashID).get(orientation).add(decodedPathChild);
				serialNum = Integer.valueOf(children[i]);
				if ( serialNum < min ) {
					min = serialNum;
				}
				// serialTable.get(orientation).add();
				
			}
			
			String decodedPathChild = MipavUtil.decodeStr(dir + File.separator + min+ File.separator);	
			traverse_dicom(new File(decodedPathChild), hashID, orientation );
			System.err.println();
			// System.err.println(decodedPathChild);
		    // imageNameHashtableExtra.get(hashID).get(orientation).add(decodedPathChild);
			
		}
	}
	
	private void traverse_dicom(File dir, String hashID, String orientation ) {
		if ( dir.isDirectory() ) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++ ) {
				String decodedPathChild = MipavUtil.decodeStr(dir + File.separator + children[i]);	
				System.err.println(decodedPathChild);
				imageNameHashtableExtra.get(hashID).get(orientation).add(decodedPathChild);
			}
		}
	}
	
	
	private void traverse_Layer(File dir) {
		// processDir_folder_1(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			// for (int i = 0; i < 1; i++) {
			for (int i = 0; i < children.length; i++) {
			    // System.err.println("current index = " + i);
				// System.err.println(dir + File.separator + children[i]);
			    // Hashtable<String, Hashtable<String, ModelImage>>
			    origImageTableName.put(children[i], new Hashtable<String, String>());
				
			    traverse_firstLayer(dir, children[i]);
				System.err.println();
			}
			/*
			 * System.err.println("check voi table"); Set<String> keys =
			 * dicomTable.keySet(); System.err.println("dicom table size = " +
			 * keys.size()); for ( String key : keys ) { if ( voiTable.get(key)
			 * == null ) { // System.err.println(key); } }
			 * System.err.println(" check dicom table"); Set<String> keyset =
			 * voiTable.keySet(); System.err.println("voi table size = " +
			 * keyset.size()); for ( String key : keyset ) { if (
			 * dicomTable.get(key) == null ) { // System.err.println(key); } }
			 */
		}
	}

	private void traverse_firstLayer(File firstDir, String child) {
		File firstLayer = new File(firstDir, child);
		traverse_secondLayer(firstLayer, child);
		// traverse_DicomLayer(firstLayer, child);
		// traverse_VoiLayer(firstLayer, child);

	}

	Hashtable<String, Integer> dicomTable = new Hashtable<String, Integer>();
	Hashtable<String, Integer> voiTable = new Hashtable<String, Integer>();

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
	
	void deleteDirectory(File file) {
		File[] contents = file.listFiles();
		if ( contents != null ) {
			for ( File f : contents ) {
				deleteDir(f);
			}
		}
		file.delete();
	}

	private void traverse_special(File layer, String hashID, String orientationLabel) {
		String[] children = layer.list();
		String decodedPathChild = MipavUtil.decodeStr(layer + File.separator + children[0]+ File.separator);
		File layerDeeper = new File(decodedPathChild);
		
		if ( imageNameHashtable.get(hashID) == null ) {
			imageNameHashtable.put(hashID, new Hashtable<String, Vector<String>>());
		}
		
		String[] files = layerDeeper.list();
		imageNameHashtable.get(hashID).put(orientationLabel, new Vector<String>());
		for ( int i = 0; i < files.length; i++ ) {
			String decodedPath = MipavUtil.decodeStr(layerDeeper + File.separator + files[i]);
			imageNameHashtable.get(hashID).get(orientationLabel).add(decodedPath);
			// System.err.println(decodedPath);
		}
		
		/*
		if (children[0].startsWith("I") && children[0].substring(1, children[0].length()).matches("^?\\d+$")) {
			System.err.println(hashID + "-->" + orientationLabel + ":");
			imageNameHashtable.get(hashID).put(orientationLabel, new Vector<String>());

			for (int i = 0; i < children.length; i++) {
				imageNameHashtable.get(hashID).get(orientationLabel).add(layer.getAbsolutePath() + File.separator + children[i]);
				// System.err.println(T2Layer.getAbsolutePath() + File.separator + children[i]);
			}
		}
		*/ 
	}
	
	private void traverse_DicomLayer(File firstLayer, String hashID) {

		String[] children = firstLayer.list();

		for (int i = 0; i < children.length; i++) {

			// traverse_thirdLayer(new File(firstLayer, children[i]), hashID);

			if (children[i].toLowerCase().equals("dicom")) {
				// traverse_scanLayer(new File(firstLayer, children[i]),
				// hashID);
				dicomTable.put(hashID, 1);
			}

		}
	}

	private void traverse_VoiLayer(File firstLayer, String hashID) {

		String[] children = firstLayer.list();

		for (int i = 0; i < children.length; i++) {

			if (children[i].toLowerCase().equals("mold")) {
				// traverse_voiLayer(new File(firstLayer, children[i]), hashID);
				voiTable.put(hashID, 1);
			}

		}
	}

	private void traverse_scanLayer(File secondLayer, String hashID) {

		String[] children = secondLayer.list();

		for (int i = 0; i < children.length; i++) {
			// System.err.println("image: " + secondLayer + File.separator +
			// children[i]);
			String decodedPath = MipavUtil.decodeStr(secondLayer + File.separator + children[i]);
			// System.err.println("decodedPath = " + decodedPath);
			File file = new File(decodedPath);
			if (file.isDirectory()) {
				imageNameHashtable.get(hashID).put(children[i], new Vector<String>());
				traverse_T2Layer(new File(decodedPath), hashID, children[i]);
			}
		}

		System.err.println();
	}

	private void traverse_T2Layer(File T2Layer, String hashID, String orientationLabel) {
		String[] children = T2Layer.list();
		System.err.println("children[" + 0 + "] = " + children[0]);
		if ((children[0].startsWith("I") && children[0].substring(1, children[0].length()).matches("^?\\d+$")) || children[0].endsWith("dcm")) {

			System.err.println(hashID + "-->" + orientationLabel + ":");
			for (int i = 0; i < children.length; i++) {
				imageNameHashtable.get(hashID).get(orientationLabel).add(T2Layer + File.separator + children[i]);
				// System.err.println(T2Layer + File.separator + children[i]);
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
		// System.err.println("hashID = " + hashID);
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
			/*
			 * if ( voiName.equals("cg.voi") || voiName.equals("wp.voi")) {
			 * System.err.println("voi: " + secondLayer + File.separator +
			 * children[i]); voiNameHashtable.get(hashID).add(voiString); }
			 */
			// System.err.println(voiString);
			/*
			 * String decodedVOI = MipavUtil.decodeStr(voiName);
			 * 
			 * if ( !decodedVOI.contains("manuel") &&
			 * !decodedVOI.contains("manual") && decodedVOI.startsWith("cg") &&
			 * decodedVOI.endsWith("voi")) { // System.err.println("voi: " +
			 * secondLayer + File.separator + children[i]);
			 * voiNameHashtable.get(hashID).add(MipavUtil.decodeStr(voiString));
			 * count++; } if ( !decodedVOI.contains("manuel") &&
			 * !decodedVOI.contains("manual") && decodedVOI.startsWith("wp") &&
			 * decodedVOI.endsWith("voi")) { // System.err.println("voi: " +
			 * secondLayer + File.separator + children[i]); //
			 * voiNameHashtable.get(hashID).add(MipavUtil.decodeStr(voiString));
			 * count++; }
			 */

		}
		System.err.println();

	}

	private void processDir_folder_1(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("image") && dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector1.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi") && dirName.substring(begin, end).endsWith(".xml")) {
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
			index = Integer.valueOf(imageName.substring(start + 5, end));
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
			index = Integer.valueOf(voiName.substring(start + 3, end));
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
		 * i = 0; for (String entry : keyImageVector1) { System.err.println(i +
		 * " = " + entry); i++; } System.err.println("VOI:"); i = 0; for (String
		 * entry : keyImageVOIVector1) { System.err.println(i + " = " + entry);
		 * i++; }
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

		if (dirName.substring(begin, end).startsWith("image") && dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector2.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi") && dirName.substring(begin, end).endsWith(".xml")) {
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
			index = Integer.valueOf(imageName.substring(start + 5, end));
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
			index = Integer.valueOf(voiName.substring(start + 3, end));
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
		 * i = 0; for (String entry : keyImageVector2) { System.err.println(i +
		 * " = " + entry); i++; } System.err.println("VOI:"); i = 0; for (String
		 * entry : keyImageVOIVector2) { System.err.println(i + " = " + entry);
		 * i++; }
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

		if (dirName.substring(begin, end).startsWith("image") && dirName.substring(begin, end).endsWith(".xml")) {
			// System.err.println(dir.toString());
			keyImageVector3.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi") && dirName.substring(begin, end).endsWith(".xml")) {
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
			index = Integer.valueOf(imageName.substring(start + 5, end));
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
			index = Integer.valueOf(voiName.substring(start + 3, end));
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
		 * i = 0; for (String entry : keyImageVector3) { System.err.println(i +
		 * " = " + entry); i++; } System.err.println("VOI:"); i = 0; for (String
		 * entry : keyImageVOIVector3) { System.err.println(i + " = " + entry);
		 * i++; }
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

		if (dirName.substring(begin, end).startsWith("image") && dirName.substring(begin, end).endsWith(".xml")) {
			System.err.println(dir.toString());
			keyImageVector4.add(dir.toString());
		}

		if (dirName.substring(begin, end).startsWith("voi") && dirName.substring(begin, end).endsWith(".xml")) {
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
			index = Integer.valueOf(imageName.substring(start + 5, end));
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
			index = Integer.valueOf(voiName.substring(start + 3, end));
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
		 * i = 0; for (String entry : keyImageVector4) { System.err.println(i +
		 * " = " + entry); i++; } System.err.println("VOI:"); i = 0; for (String
		 * entry : keyImageVOIVector4) { System.err.println(i + " = " + entry);
		 * i++; }
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

		// loadFiles();
		readImages();
		// readImagesExra();               // now
		// cropKeyImages();

		System.err.println("saveImage");
		// saveImages();
		// saveGroundTruth();
		// mad!
		crossValidationTrain();
		// ruida
		// crossValidationTest();
		// crossValidationTestExtra();       // now

		// saveTestedImages();
		// disposeLocal();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

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

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void crossValidationTestExtra() {
	    
		Set<String> hashKeys = imageHashtableExtra.keySet();
		
		int[] indexAxial = new int[1];
		indexAxial[0] = 0;
		
		int[] indexSagittal = new int[1];
		indexSagittal[0] = 0;
		
		int[] indexCoronal = new int[1];
		indexCoronal[0] = 0;
		
	    for ( String hashID : hashKeys ) {
	    	
	    	Hashtable<String, ModelImage> imageHash = imageHashtableExtra.get(hashID);
	    	ModelImage axialImage = imageHash.get("axial");
	    	ModelImage sagittalImage = imageHash.get("sagittal");
	    	ModelImage coronalImage = imageHash.get("coronal");
	    	
	    	int idx = hashID.lastIndexOf(File.separator);
	    	String upperDir = hashID.substring(0, idx);
	    	String lowerDir = hashID.substring(idx+1, hashID.length());
	    	idx = upperDir.lastIndexOf(File.separator);
	    	upperDir = upperDir.substring(idx+1, upperDir.length());
		    
	    	
	    	String subDirAxial = saveImageDirectory + File.separator + "axial" + File.separator;
			File subDirFileAxial = new File(subDirAxial);
			if (!subDirFileAxial.isDirectory())
				subDirFileAxial.mkdir();
			// saveImagesTest(axialImage, indexAxial, "axial" + File.separator);
	    	saveImagesTest(axialImage, upperDir, "axial" + File.separator);
			// new ViewJFrameImage(axialImage);
	    	// indexAxial[0]++;
	    	
	    	String subDirSagittal = saveImageDirectory + File.separator + "sagittal" + File.separator;
			File subDirFileSagittal = new File(subDirSagittal);
			if (!subDirFileSagittal.isDirectory())
				subDirFileSagittal.mkdir();
	    	// saveImagesTest(sagittalImage, indexSagittal, "sagittal" + File.separator);
	    	saveImagesTest(sagittalImage, upperDir, "sagittal" + File.separator);
	    	// new ViewJFrameImage(sagittalImage);
	    	// indexSagittal[0]++;
	    	
	    	String subDirCoronal = saveImageDirectory + File.separator + "coronal" + File.separator;
			File subDirFileCoronal = new File(subDirCoronal);
			if (!subDirFileCoronal.isDirectory())
				subDirFileCoronal.mkdir();
	    	// saveImagesTest(coronalImage, indexCoronal, "coronal" + File.separator);
	    	saveImagesTest(coronalImage, upperDir, "coronal" + File.separator);
	    	// new ViewJFrameImage(coronalImage);
	    	// indexCoronal[0]++;
	    }
	   
		
		
	}
	public void crossValidationTrain() {

	    
		Set<String> hashKeys = origImageTable.keySet();
		
		int count = 0;
		String folderName = null;
		
		 int[] indexAxial = new int[1];
		 indexAxial[0] = 0;
		 
		 int[] indexCoronal = new int[1];
		 indexCoronal[0] = 0;
		 
		 int[] indexSagittal = new int[1];
		 indexSagittal[0] = 0;
		 
		
	    for ( String hashKey : hashKeys ) {
	    	
	    	/*
	    	folderName = "fold1";
	    	if ( count >= 0 && count <= 25) {
	    		count++;
	    		continue;
	    	}
	    	*/
	    	
	    
	    	
	    	folderName = "fold2";
	    	if ( count >= 26 && count <= 50 ) {
	    		count++;
	    		continue;
	    	} 
	    	 
	    
	    	/*
	    	folderName = "fold3";
	    	if ( count >= 51 && count <= 75 ) {
	    		count++;
	    		continue;
	    	}
	        */
	    	 
	        /*
	    	folderName = "fold4";
	    	if ( count >= 76 && count <= 106 ) {
	    		count++;
	    		continue;
	    	}
	    	*/ 
	    	
	    	int index = hashKey.lastIndexOf("_");
	    	String id = hashKey.substring(0, index);
	    	
	    	Hashtable<String, ModelImage> imageHash = origImageTable.get(hashKey);
	    	ModelImage axialImage = imageHash.get("axial");
	    	ModelImage sagittalImage = imageHash.get("sagittal");
	    	ModelImage coronalImage = imageHash.get("coronal");
	    	
	    	String subDirAxial = saveImageDirectory + File.separator + folderName + File.separator + "axial" + File.separator;
			File subDirFileAxial = new File(subDirAxial);
			if (!subDirFileAxial.isDirectory())
				subDirFileAxial.mkdir();
	    	// saveImagesTest(axialImage, id, folderName + File.separator + "axial" + File.separator);
	    	saveImages(axialImage, indexAxial,  folderName + File.separator, "axial");
	    	System.err.println("indexAxial = " + indexAxial[0]);
	    	
	    	
	    	String subDirSagittal = saveImageDirectory + File.separator + folderName + File.separator + "sagittal" + File.separator;
			File subDirFileSagittal = new File(subDirSagittal);
			if (!subDirFileSagittal.isDirectory())
				subDirFileSagittal.mkdir();
	    	// saveImagesTest(sagittalImage, id, folderName + File.separator + "sagittal" + File.separator);
	    	saveImages(sagittalImage, indexSagittal,  folderName + File.separator, "sagittal");
	    	System.err.println("indexSagittal = " + indexSagittal[0]);
	    	
	    	
	    	String subDirCoronal = saveImageDirectory + File.separator + folderName + File.separator + "coronal" + File.separator;
			File subDirFileCoronal = new File(subDirCoronal);
			if (!subDirFileCoronal.isDirectory())
				subDirFileCoronal.mkdir();
	    	// saveImagesTest(coronalImage, id, folderName + File.separator + "coronal" + File.separator);
	    	saveImages(coronalImage, indexCoronal,  folderName + File.separator, "coronal");
	    	System.err.println("indexCoronal = " + indexCoronal[0]);
	    	
	    	count++;
	    }
	   
		
		
	
        /*
	    Set<String> orientationKeys = srcImageTable.keySet();
	    int[] index = new int[1];
	    index[0] = 0;
	    for ( String orientation : orientationKeys ) {
	    	Vector<ModelImage> images = srcImageTable.get(orientation);
	    	if ( orientation.equals("axial") ) {
	    		index = new int[1];
	    	    index[0] = 0;
	    		int len = images.size();
	    		for ( int i = 0; i < len; i++ ) {
	    			ModelImage image = images.get(i);
	    			saveImages(image, index, "foldTest" + File.separator + "train", orientation);
	    		}
	    	} else if ( orientation.equals("sagittal") ) {
	    		index = new int[1];
	    	    index[0] = 0;
	    		int len = images.size();
	    		for ( int i = 0; i < len; i++ ) {
	    			ModelImage image = images.get(i);
	    			saveImages(image, index, "foldTest" + File.separator + "train", orientation);
	    		}
	    	} else if ( orientation.equals("coronal") ) {
	    		index = new int[1];
	    	    index[0] = 0;
	    		int len = images.size();
	    		for ( int i = 0; i < len; i++ ) {
	    			ModelImage image = images.get(i);
	    			saveImages(image, index, "foldTest" + File.separator + "train", orientation);
	    		}
	    	}
	    			
	    }
		*/ 
	}
	public void crossValidationTest() {
	    
		Set<String> hashKeys = origImageTable.keySet();
		
		int count = 0;
		String folderName = null;
		
		 int[] indexAxial = new int[1];
		 indexAxial[0] = 0;
		 
		 int[] indexCoronal = new int[1];
		 indexCoronal[0] = 0;
		 
		 int[] indexSagittal = new int[1];
		 indexSagittal[0] = 0;
		 
		
	    for ( String hashKey : hashKeys ) {
	    	
	    	if ( count >= 0 && count <= 25) {
	    		folderName = "fold1";
	    	} else if ( count >= 26 && count <= 50 ) {
	    		folderName = "fold2";
	    	} else if ( count >= 51 && count <= 75 ) {
	    		folderName = "fold3";
	    	} else if ( count >= 76 && count <= 106 ) {
	    		folderName = "fold4";
	    	}
	    
	    	
	    	int index = hashKey.lastIndexOf("_");
	    	String id = hashKey.substring(0, index);
	    	
	    	Hashtable<String, ModelImage> imageHash = origImageTable.get(hashKey);
	    	ModelImage axialImage = imageHash.get("axial");
	    	ModelImage sagittalImage = imageHash.get("sagittal");
	    	ModelImage coronalImage = imageHash.get("coronal");
	    	
	    	String subDirAxial = saveImageDirectory + File.separator + folderName + File.separator + "axial" + File.separator;
			File subDirFileAxial = new File(subDirAxial);
			if (!subDirFileAxial.isDirectory())
				subDirFileAxial.mkdir();
	        saveImagesTest(axialImage, id, folderName + File.separator + "axial" + File.separator);
	    	// saveImages(axialImage, indexAxial,  folderName + File.separator, "axial");
	    	
	    	
	    	String subDirSagittal = saveImageDirectory + File.separator + folderName + File.separator + "sagittal" + File.separator;
			File subDirFileSagittal = new File(subDirSagittal);
			if (!subDirFileSagittal.isDirectory())
				subDirFileSagittal.mkdir();
	    	saveImagesTest(sagittalImage, id, folderName + File.separator + "sagittal" + File.separator);
	    	// saveImages(sagittalImage, indexSagittal,  folderName + File.separator, "sagittal");
	    	
	    	String subDirCoronal = saveImageDirectory + File.separator + folderName + File.separator + "coronal" + File.separator;
			File subDirFileCoronal = new File(subDirCoronal);
			if (!subDirFileCoronal.isDirectory())
				subDirFileCoronal.mkdir();
	    	saveImagesTest(coronalImage, id, folderName + File.separator + "coronal" + File.separator);
	    	// saveImages(coronalImage, indexCoronal,  folderName + File.separator, "coronal");
	    	
	    	count++;
	    }
	   
		
		
	}
	
	private void printTable() {
		
		Set<String> keys = origImageTableName.keySet();
		
		for (String hashID : keys) {

			Hashtable<String, String> imageHashtable = origImageTableName.get(hashID);
			Hashtable<String, String>  voiHashtable = origVOITableName.get(hashID);
            System.err.println(imageHashtable.get("axial"));
            System.err.println(imageHashtable.get("sagittal"));
            System.err.println(imageHashtable.get("coronal"));
            
            System.err.println(voiHashtable.get("axial"));
            System.err.println(voiHashtable.get("sagittal"));
            System.err.println(voiHashtable.get("coronal"));
            
            System.err.println();
		}	
		
			
	}
	
	public void readImagesExra() {

		String imageFullName;
		boolean multiFile = true;
		int count = 0;
		Set<String> keys = imageNameHashtableExtra.keySet();
		try {
		for (String hashID : keys) {
  
			imageHashtableExtra.put(hashID, new Hashtable<String, ModelImage>());
			Hashtable<String, Vector<String>> imageHashtable = imageNameHashtableExtra.get(hashID);
			// Hashtable<String, String>  voiHashtable = origVOITableName.get(hashID);
			
		    // if ( origImageTable.get(hashID) == null ) {
		    //	origImageTable.put(hashID, new Hashtable<String, ModelImage>());
		    // }
			
			Vector<String> axialNames = imageHashtable.get("axial");
		    
		    FileIO fileIO = new FileIO();
			 imageFullName = axialNames.get(0);
			int index = imageFullName.lastIndexOf(File.separator);
			String fileName = imageFullName.substring(index + 1, imageFullName.length());
			String directory = imageFullName.substring(0, index + 1);
			System.err.println("filename = " + fileName);
			System.err.println("directory = " + directory);
			ModelImage imageAxial = fileIO.readImage(fileName, directory, multiFile, null);
			// new ViewJFrameImage(imageAxial);
			imageHashtableExtra.get(hashID).put("axial", imageAxial);
		
		    
			Vector<String> sagittalNames = imageHashtable.get("sagittal");
			imageFullName = sagittalNames.get(0);
			index = imageFullName.lastIndexOf(File.separator);
			fileName = imageFullName.substring(index + 1, imageFullName.length());
			directory = imageFullName.substring(0, index + 1);
			System.err.println("filename = " + fileName);
			System.err.println("directory = " + directory);
			ModelImage imageSagittal = fileIO.readImage(fileName, directory, multiFile, null);
			// new ViewJFrameImage(imageSagittal);
			imageHashtableExtra.get(hashID).put("sagittal", imageSagittal);
			
			Vector<String> coronalNames = imageHashtable.get("coronal");
			imageFullName = coronalNames.get(0);
			index = imageFullName.lastIndexOf(File.separator);
			fileName = imageFullName.substring(index + 1, imageFullName.length());
			directory = imageFullName.substring(0, index + 1);
			System.err.println("filename = " + fileName);
			System.err.println("directory = " + directory);
			ModelImage imageCoronal = fileIO.readImage(fileName, directory, multiFile, null);
			// new ViewJFrameImage(imageCoronal);
			imageHashtableExtra.get(hashID).put("coronal", imageCoronal);
			
			count++;
			if ( count == 50 ) break;
            /*
			String voiDir = voiHashtable.get("axial");
			index = voiDir.lastIndexOf(File.separator);
			String voiDirectory = new String(voiDir.substring(0, index + 1));
			String voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));
			FileVOI fileVOIAxial = new FileVOI(voiFileName, voiDirectory, imageAxial);
			imageAxial.registerVOI(fileVOIAxial.readVOI(false)[0]);
			origImageTable.get(hashID).put("axial", imageAxial);
			
			voiDir = voiHashtable.get("sagittal");
			index = voiDir.lastIndexOf(File.separator);
		    voiDirectory = new String(voiDir.substring(0, index + 1));
			voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));
			FileVOI fileVOISagittal = new FileVOI(voiFileName, voiDirectory, imageSagittal);
			imageSagittal.registerVOI(fileVOISagittal.readVOI(false)[0]);
			origImageTable.get(hashID).put("sagittal", imageSagittal);
			
			
			voiDir = voiHashtable.get("coronal");
			index = voiDir.lastIndexOf(File.separator);
		    voiDirectory = new String(voiDir.substring(0, index + 1));
			voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));
			FileVOI fileVOICoronal = new FileVOI(voiFileName, voiDirectory, imageCoronal);
			imageCoronal.registerVOI(fileVOICoronal.readVOI(false)[0]);
			origImageTable.get(hashID).put("coronal", imageCoronal);
            */ 
		}	
	
		} catch ( Exception e ) {
			e.printStackTrace();
		}
		/*
		Set<String> hashKeys = origImageTable.keySet();
	    for ( String hashKey : hashKeys ) {
	    	
	    	Hashtable<String, ModelImage> imageHash = origImageTable.get(hashKey);
	    	ModelImage axialImage = imageHash.get("axial");
	    	ModelImage sagittalImage = imageHash.get("sagittal");
	    	ModelImage coronalImage = imageHash.get("coronal");
	    	
	    	try {
			    String subDir = saveImageDirectory + File.separator + hashKey + File.separator;
				File subDirFile = new File(subDir);
				if (!subDirFile.isDirectory())
					subDirFile.mkdir();
				
				VOI voiAxial = axialImage.getVOIs().elementAt(0);
				axialImage.saveImage(subDir, "imageAxial.xml", FileUtility.XML, false);
				FileVOI fileVOIAxial = new FileVOI("voiAxial.xml", subDir, axialImage);
				fileVOIAxial.writeVOI(voiAxial, true);
				
				VOI voiSagittal = sagittalImage.getVOIs().elementAt(0);
				sagittalImage.saveImage(subDir, "imageSagittal.xml", FileUtility.XML, false);
				FileVOI fileVOISagittal = new FileVOI("voiSagittal.xml", subDir, sagittalImage);
				fileVOISagittal.writeVOI(voiSagittal, true);
				
				VOI voiCoronal = coronalImage.getVOIs().elementAt(0);
				coronalImage.saveImage(subDir, "imageCoronal.xml", FileUtility.XML, false);
				FileVOI fileVOICoronal = new FileVOI("voiCoronal.xml", subDir, coronalImage);
				fileVOICoronal.writeVOI(voiCoronal, true);
				
				
			} catch (Exception e) {
				e.printStackTrace();
			}
	    }
	    */ 
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
			origImageTable.get(hashID).put("axial", imageAxial);
			
			voiDir = voiHashtable.get("sagittal");
			index = voiDir.lastIndexOf(File.separator);
		    voiDirectory = new String(voiDir.substring(0, index + 1));
			voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));
			FileVOI fileVOISagittal = new FileVOI(voiFileName, voiDirectory, imageSagittal);
			imageSagittal.registerVOI(fileVOISagittal.readVOI(false)[0]);
			origImageTable.get(hashID).put("sagittal", imageSagittal);
			
			
			voiDir = voiHashtable.get("coronal");
			index = voiDir.lastIndexOf(File.separator);
		    voiDirectory = new String(voiDir.substring(0, index + 1));
			voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));
			FileVOI fileVOICoronal = new FileVOI(voiFileName, voiDirectory, imageCoronal);
			imageCoronal.registerVOI(fileVOICoronal.readVOI(false)[0]);
			origImageTable.get(hashID).put("coronal", imageCoronal);
            
		}	
	
		} catch ( Exception e ) {
			e.printStackTrace();
		}
		/*
		Set<String> hashKeys = origImageTable.keySet();
	    for ( String hashKey : hashKeys ) {
	    	
	    	Hashtable<String, ModelImage> imageHash = origImageTable.get(hashKey);
	    	ModelImage axialImage = imageHash.get("axial");
	    	ModelImage sagittalImage = imageHash.get("sagittal");
	    	ModelImage coronalImage = imageHash.get("coronal");
	    	
	    	try {
			    String subDir = saveImageDirectory + File.separator + hashKey + File.separator;
				File subDirFile = new File(subDir);
				if (!subDirFile.isDirectory())
					subDirFile.mkdir();
				
				VOI voiAxial = axialImage.getVOIs().elementAt(0);
				axialImage.saveImage(subDir, "imageAxial.xml", FileUtility.XML, false);
				FileVOI fileVOIAxial = new FileVOI("voiAxial.xml", subDir, axialImage);
				fileVOIAxial.writeVOI(voiAxial, true);
				
				VOI voiSagittal = sagittalImage.getVOIs().elementAt(0);
				sagittalImage.saveImage(subDir, "imageSagittal.xml", FileUtility.XML, false);
				FileVOI fileVOISagittal = new FileVOI("voiSagittal.xml", subDir, sagittalImage);
				fileVOISagittal.writeVOI(voiSagittal, true);
				
				VOI voiCoronal = coronalImage.getVOIs().elementAt(0);
				coronalImage.saveImage(subDir, "imageCoronal.xml", FileUtility.XML, false);
				FileVOI fileVOICoronal = new FileVOI("voiCoronal.xml", subDir, coronalImage);
				fileVOICoronal.writeVOI(voiCoronal, true);
				
				
			} catch (Exception e) {
				e.printStackTrace();
			}
	    }
	    */ 
	}

	

	public void saveImages(ModelImage image, int[] index, String folderName, String orientation) {
		try {

			// if ( i == count ) continue;

			// ModelImage cropKeyImage = imageHashtable.get(key).get(0);

			int xDim = image.getExtents()[0];
			int yDim = image.getExtents()[1];
			int zDim = image.getExtents()[2];

			int size_3D = xDim * yDim * zDim;
			float[] imageBuffer = new float[size_3D];

			try {
				image.exportData(0, imageBuffer.length, imageBuffer);
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

			VOIVector targetImageVOI = image.getVOIs();

			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator;
			File sliceDirFile = new File(sliceDir);
			if (!sliceDirFile.isDirectory())
				sliceDirFile.mkdir();
			
			sliceDir += orientation + File.separator;
			File axisDir = new File(sliceDir);
			if ( !axisDir.isDirectory() )
				axisDir.mkdir();

			for (int j = 0; j <= zDim; j++) {

				try {

					// System.err.println(" image number = " + key + "   slice number = " + j);

					ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
					float[] targetBuffer = new float[size];
					image.exportData(j * size, size, targetBuffer);
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
						// String sliceDir = saveImageDirectory +
						// File.separator + i + File.separator;

						// System.err.println("index = " + index[0]);

						String imgName = "image_" + index[0] + ".png";
						// String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						ModelImage maskImage = null;
						maskImage = targetImageSlice.generateBinaryImage(false, false);
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

						vTemp = null;
						xPts = null;
						zPts = null;
						zPtsZero = null;

					} else {
						// 1) save image
						// String sliceDir = saveImageDirectory +
						// File.separator + i + File.separator;

						// System.err.println("index = " + index[0]);

						String imgName = "image_" + index[0] + ".png";
						// String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						ModelImage maskImage = null;
						// maskImage =
						// targetImageSlice.generateBinaryImage(false,
						// false);
						// maskImage = new ModelImage(imageA.getType(),
						// destExtents, imageA.getImageName());
						maskImage = new ModelImage(ModelStorageBase.SHORT, newExtents, "mask" + j);
						String maskName = "voi_" + index[0] + ".png";
						// String maskName = "voi_" + j + ".png";
						// maskImage.saveImage(sliceDir, maskName,
						// FileUtility.JIMI, false);
						// new ViewJFrameImage(maskImage);
						// if ( true ) break;
						savePNGfile(sliceDir, maskName, maskImage, min, max, xDim, yDim,true);

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

	public void saveImagesTest(ModelImage cropKeyImage, String pid, String folderName) {
		try {

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


			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator + pid + File.separator;

			// String sliceDir = saveImageDirectory +
			// File.separator + i + File.separator;

			File sliceDirFile = new File(sliceDir);
			if (!sliceDirFile.isDirectory())
				sliceDirFile.mkdir();

			for (int j = 0; j <= zDim; j++) {

				try {

					System.err.println(" image number = " + pid + "   slice number = " + j);

					ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
					float[] targetBuffer = new float[size];
					cropKeyImage.exportData(j * size, size, targetBuffer);
					targetImageSlice.importData(0, targetBuffer, true);

					// find the intersection of the lower bound with the
					// VOI.
					// Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);

					// if (vArray[j].size() > 0) {
						

						// 1) save image
						// String sliceDir = saveImageDirectory +
						// File.separator + i + File.separator;

						System.err.println("index = " + pid);

						// String imgName = "image_" + index[0] + ".png";
						String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						
						targetBuffer = null;
						targetImageSlice.disposeLocal();
						targetImageSlice = null;

						

					

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
	private void rotateToStartingPoint_yMid(float xPts[], float yPts[], int xDim, int yDim) {
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
	private void rotateToStartingPoint_leftMost(float xPts[], float yPts[], int xDim, int yDim) {
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
	 * Convert VOI from one contour to two contours
	 * 
	 * @param srcContour
	 *            single VOI contour
	 * @param targetImageSlice
	 *            target 2D image slice
	 */
	private void generateBoundaryContours(VOIBase srcContour, ModelImage targetImageSlice) {
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
		for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent + nPoints - 1; i++, j++) {
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
				for (k = 0, m = i - sidePointsForTangent; m <= i + sidePointsForTangent; m++, k++) {
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
					var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
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
						d1 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k]) / Math.sqrt(slope * slope + 1));
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
						d2 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k]) / Math.sqrt(slope * slope + 1));
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
					if (srcContour.contains(xPoints[i], yPoints[i] + innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] + innerDistance, slice);
						innerV.add(point);
					} else if (srcContour.contains(xPoints[i], yPoints[i] - innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] - innerDistance, slice);
						innerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = innerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (srcContour.contains((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist), slice);
						innerV.add(point);
					} else if (srcContour.contains((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist), slice);
						innerV.add(point);
					}
				}
			} // if (doInner)
			if (doOuter) {
				if (Double.isInfinite(slope)) {
					if (!srcContour.contains(xPoints[i], yPoints[i] + outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] + outerDistance, slice);
						outerV.add(point);
					} else if (!srcContour.contains(xPoints[i], yPoints[i] - outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] - outerDistance, slice);
						outerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = outerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (!srcContour.contains((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist), slice);
						outerV.add(point);
					} else if (!srcContour.contains((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist), slice);
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
		VOI resultVOI = new VOI(sID, kName + "_" + sID, srcContour.getType(), -1);

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
						destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
						destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
					} else {
						destExtents = new int[3];
						destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
						destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
						destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
					}
				} else {
					return;
				}

				System.err.println("destExtents[0] = " + destExtents[0] + "  destExtents[1] = " + destExtents[1]);

				// create crop images
				cropKeyImages.add(i, new ModelImage(image.getType(), destExtents, makeImageName(image.getImageName(), "_crop")));

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

				System.err.println("xCrop[0] = " + xCrop[0] + "   xCrop[1] = " + xCrop[1]);
				System.err.println("yCrop[0] = " + yCrop[0] + "   yCrop[1] = " + yCrop[1]);

				cropAlgo = new AlgorithmAddMargins(image, cropKeyImages.get(i), xCrop, yCrop, zCrop);

				cropAlgo.addListener(this);

				// Hide the dialog since the algorithm is about to run.
				setVisible(false);

				cropAlgo.run();

			} catch (OutOfMemoryError e) {
				MipavUtil.displayError("Dialog Crop: unable to allocate enough memory");

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

		// int index;

		// System.err.println("keyImageVector.size() = " +
		// keyImageVector.size());
		srcImageTable.put("axial", new Vector<ModelImage>());
		srcImageTable.put("sagittal", new Vector<ModelImage>());
		srcImageTable.put("coronal", new Vector<ModelImage>());
		
		try {
			// imageNameHashtable.get(hashID).get(orientationLabel).add(T2Layer
			// + File.separator + children[i]);
			// ruida
			Set<String> keys = imageNameHashtable.keySet();
			
			for (String hashID : keys) {

				Hashtable<String, Vector<String>> imageDicomSet = imageNameHashtable.get(hashID);
				Hashtable<String, String>  voiSet = voiNameHashtable.get(hashID);
				
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
							if ( extents[0] >= 400 ) {
								// new ViewJFrameImage(image);
								int imageOrientation = image.getImageOrientation();
								
								if ( imageOrientation == FileInfoBase.AXIAL ) {
									voiFileName = voiSet.get("axial");
								} else if ( imageOrientation == FileInfoBase.SAGITTAL ) {
									voiFileName = voiSet.get("sagittal");
								} else if ( imageOrientation == FileInfoBase.CORONAL ) {
									voiFileName = voiSet.get("coronal");
								}
								
								// loadSTLBinaryMesh(new File(voiFileName), image, imageOrientation);
								
							    loadSTLBinaryMesh(new File(voiFileName), image, imageOrientation, hashID);
								
							}
						
						    // pause();
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
	
	 private void loadSTLBinaryMesh(File file, ModelImage kImage, int imageOrientation) {

	        try {
	        	FileInputStream data;    
	        	data = new FileInputStream(file);
	            readSTLBinary(data, kImage, imageOrientation);
	         
	        } catch (FileNotFoundException e) {
	            System.err.println("ERROR: Can't find file " + file);
	           
	        } catch (IOException e) {
	           
	        }
	    }
	 
	 private void loadSTLBinaryMesh(File file, ModelImage kImage, int imageOrientation, String hashID) {

	        try {
	        	FileInputStream data;    
	        	data = new FileInputStream(file);
	            readSTLBinary(data, kImage, imageOrientation, hashID);
	         
	        } catch (FileNotFoundException e) {
	            System.err.println("ERROR: Can't find file " + file);
	           
	        } catch (IOException e) {
	           
	        }
	    }

	  private void readSTLBinary(FileInputStream data, ModelImage kImage, int imageOrientation) throws IOException {
			ByteBuffer dataBuffer; // For reading in the correct endian
			byte[] Info = new byte[80]; // Header data
			byte[] Array_number = new byte[4]; // Holds the number of faces
			byte[] Temp_Info; // Intermediate array

			float zMin = 999;  
			float zMax = 0;
			Vector3f temp = new Vector3f();
			Vector3f normal = new Vector3f();
			Vector3f side1 = new Vector3f();
			Vector3f side2 = new Vector3f();
			Vector3f surfaceNormal = new Vector3f();
			Vector<Vector3f> vertexArray = new Vector<Vector3f>();
			VertexBuffer kVBuffer;
			int[] aiConnect;

			TriMesh kMesh = null;
			float x, y, z;
			Vector3f vertex1, vertex2, vertex3;
			int index = 0;
			Integer searchIndex;
			Vector<Integer> connectivity = new Vector<Integer>();
			HashMap<String, Integer> vertexHashtable = new HashMap<String, Integer>();

			float _res[] = kImage.getFileInfo(0).getResolutions();
			float[] _startLocation = kImage.getFileInfo()[0].getOrigin();
			int[] _direction = MipavCoordinateSystems.getModelDirections(kImage);

			Vector3f ptIn = new Vector3f();
			Vector3f ptOut = new Vector3f();

			int Number_faces; // First info (after the header) on the file

			System.out.println("Machine's endian: " + ByteOrder.nativeOrder());

			// It's a local file

			// First 80 bytes aren't important
			if (80 != data.read(Info)) { // File is incorrect
				System.out.println("Format Error: 80 bytes expected");
				return;
			}

			data.read(Array_number); // We get the 4 bytes
			dataBuffer = ByteBuffer.wrap(Array_number); // ByteBuffer for
														// reading correctly the
														// int
			dataBuffer.order(ByteOrder.nativeOrder()); // Set the right order
			Number_faces = dataBuffer.getInt();

			Temp_Info = new byte[50 * Number_faces]; // Each face has 50 bytes
														// of data

			data.read(Temp_Info); // We get the rest of the file

			dataBuffer = ByteBuffer.wrap(Temp_Info); // Now we have all the data
														// in this ByteBuffer
			dataBuffer.order(ByteOrder.nativeOrder());

			// System.out.println("Number of faces= " + Number_faces);

			// We can create that array directly as we know how big it's going
			// to be

			for (int i = 0; i < Number_faces; i++) {

				// readFacetB(dataBuffer,i);
				Point3f vertex = new Point3f();

				// System.out.println("Reading face number " + i);

				// Read the Normal
				normal = new Vector3f();
				normal.X = dataBuffer.getFloat();
				normal.Y = dataBuffer.getFloat();
				normal.Z = dataBuffer.getFloat();

				// System.out.println("Normal: X=" + normal.X + " Y=" + normal.Y + " Z=" + normal.Z);

				// Read vertex1
				vertex1 = new Vector3f();
				vertex1.X = dataBuffer.getFloat();
				vertex1.Y = dataBuffer.getFloat();
				vertex1.Z = dataBuffer.getFloat();

				// System.out.println("Vertex 1: X=" + vertex1.X + " Y=" + vertex1.Y + " Z=" + vertex1.Z);

				x = vertex1.X;
				y = vertex1.Y;
				z = vertex1.Z;

				ptIn.X = x;
				ptIn.Y = y;
				ptIn.Z = z;

				MipavCoordinateSystems.scannerToFile(ptIn, ptOut, kImage);
               
				// x = (ptOut.X * _res[0] * _direction[0]) + _startLocation[0];
				// y = (ptOut.Y * _res[1] * _direction[1]) + _startLocation[1];
				// z = (ptOut.Z * _res[2] * _direction[2]) + _startLocation[2];
				x = ptOut.X;
				y = ptOut.Y;
				z = ptOut.Z;
				
				
				searchIndex = vertexHashtable.get((x + " " + y + " " + z));
				if (searchIndex == null) { // not found
					vertexHashtable.put((x + " " + y + " " + z), new Integer(index));
					connectivity.add(new Integer(index));
					vertexArray.add(new Vector3f(x, y, z));
					if ( z > zMax ) {
						zMax = z;
					}
					if ( z < zMin ) {
						zMin = z;
					}
					index++;
				} else {
					connectivity.add(searchIndex);
				}

				// Read vertex2
				vertex2 = new Vector3f();
				vertex2.X = dataBuffer.getFloat();
				vertex2.Y = dataBuffer.getFloat();
				vertex2.Z = dataBuffer.getFloat();
				// System.out.println("Vertex 2: X=" + vertex2.X + " Y=" + vertex2.Y + " Z=" + vertex2.Z);

				x = vertex2.X;
				y = vertex2.Y;
				z = vertex2.Z;

				ptIn.X = x;
				ptIn.Y = y;
				ptIn.Z = z;

				MipavCoordinateSystems.scannerToFile(ptIn, ptOut, kImage);

				// x = (ptOut.X * _res[0] * _direction[0]) + _startLocation[0];
				// y = (ptOut.Y * _res[1] * _direction[1]) + _startLocation[1];
				// z = (ptOut.Z * _res[2] * _direction[2]) + _startLocation[2];
                x = ptOut.X;
                y = ptOut.Y;
                z = ptOut.Z;
				
				searchIndex = vertexHashtable.get((x + " " + y + " " + z));
				if (searchIndex == null) { // not found
					vertexHashtable.put((x + " " + y + " " + z), new Integer(index));
					connectivity.add(new Integer(index));
					vertexArray.add(new Vector3f(x, y, z));
					if ( z > zMax ) {
						zMax = z;
					}
					if ( z < zMin ) {
						zMin = z;
					}
					index++;
				} else {
					connectivity.add(searchIndex);
				}

				// Read vertex3
				vertex3 = new Vector3f();
				vertex3.X = dataBuffer.getFloat();
				vertex3.Y = dataBuffer.getFloat();
				vertex3.Z = dataBuffer.getFloat();
				// System.out.println("Vertex 3: X=" + vertex3.X + " Y=" + vertex3.Y + " Z=" + vertex3.Z);

				x = vertex3.X;
				y = vertex3.Y;
				z = vertex3.Z;

				ptIn.X = x;
				ptIn.Y = y;
				ptIn.Z = z;

				MipavCoordinateSystems.scannerToFile(ptIn, ptOut, kImage);

				// x = (ptOut.X * _res[0] * _direction[0]) + _startLocation[0];
				// y = (ptOut.Y * _res[1] * _direction[1]) + _startLocation[1];
				// z = (ptOut.Z * _res[2] * _direction[2]) + _startLocation[2];
				x = ptOut.X;
				y = ptOut.Y;
				z = ptOut.Z;
				
				searchIndex = vertexHashtable.get((x + " " + y + " " + z));
				if (searchIndex == null) { // not found
					vertexHashtable.put((x + " " + y + " " + z), new Integer(index));
					connectivity.add(new Integer(index));
					vertexArray.add(new Vector3f(x, y, z));
					if ( z > zMax ) {
						zMax = z;
					}
					if ( z < zMin ) {
						zMin = z;
					}
					index++;
				} else {
					connectivity.add(searchIndex);
				}
				// After each facet there are 2 bytes without information
				// In the last iteration we dont have to skip those bytes..
				if (i != Number_faces - 1) {
					dataBuffer.get();
					dataBuffer.get();
				}

			} // End for

			int vertexCount = vertexArray.size();
			VOIVector voiVectorNew = new VOIVector();
			VOI voiNew = new VOI((short)0, "ImageVOI");
			voiVectorNew.add(voiNew);
			
			int startIndex = Math.round(zMin+1);
			int endIndex = Math.round(zMax-1);
			for ( int j = startIndex; j < endIndex; j++ ) {
				
				// 1. filter out closed points
				Vector<Vector3f> ptsArray = new Vector<Vector3f>();
				float centerX = 0; 
				float centerY = 0;
				int i;
				for (i = 0; i < vertexCount; i++) {
					Vector3f pos = vertexArray.elementAt(i);
					if ( Math.abs(pos.Z - j) < 0.1 ) {
						ptsArray.add(new Vector3f(pos.X, pos.Y, j));
						centerX += pos.X;
						centerY += pos.Y;
					}
				}
				
				// 2. compute center;
				int numPts = ptsArray.size();
				
				if ( numPts <= 20 ) continue;
				
				centerX = centerX / numPts;
				centerY = centerY / numPts;
				Vector3f center = new Vector3f(centerX, centerY, j);
				
				Hashtable<Float, Vector3f> ptsTable = new Hashtable<Float, Vector3f>();
				
				// 3.  compute polar coordinate
				for ( i = 0; i < numPts; i++ ) {
					Vector3f loc = ptsArray.get(i);
					Vector2f in = new Vector2f(loc.X, loc.Y);
					Vector2f out = new Vector2f(0, 0);
					MipavCoordinateSystems.CartesianToPolar2D(in, out, center);
					ptsTable.put(out.Y, new Vector3f(loc.X, loc.Y, j));
					
				}
				
				ArrayList<Float> ptsList = Collections.list(ptsTable.keys());
				Collections.sort(ptsList);
				Iterator<Float> it = ptsList.iterator();
				Vector3f[] ptsResult = new Vector3f[numPts];
				int idx = 0;
				while ( it.hasNext() ) {
					float key = it.next();
					ptsResult[idx] = ptsTable.get(key);
					idx++;
				}
				
				VOIBase vTemp = new VOIContour(true);
				vTemp.importPoints(ptsResult);
				voiNew.importCurve(vTemp);
				
			}  // int j = startIndex; j <= endIndex; j++ 

			kImage.addVOIs(voiVectorNew);
			smoothVOI60(kImage, kImage);
			// new ViewJFrameImage(kImage);
			
		
			if ( imageOrientation == FileInfoBase.AXIAL ) {
				srcImageTable.get("axial").add(kImage);
			} else if ( imageOrientation == FileInfoBase.SAGITTAL ) {
				srcImageTable.get("sagittal").add(kImage);
			} else if ( imageOrientation == FileInfoBase.CORONAL ) {
				srcImageTable.get("coronal").add(kImage);
			}

		}// End of readBinaryFile 
	  
	  private void readSTLBinary(FileInputStream data, ModelImage kImage, int imageOrientation, String hashID) throws IOException {
			ByteBuffer dataBuffer; // For reading in the correct endian
			byte[] Info = new byte[80]; // Header data
			byte[] Array_number = new byte[4]; // Holds the number of faces
			byte[] Temp_Info; // Intermediate array

			float zMin = 999;  
			float zMax = 0;
			Vector3f temp = new Vector3f();
			Vector3f normal = new Vector3f();
			Vector3f side1 = new Vector3f();
			Vector3f side2 = new Vector3f();
			Vector3f surfaceNormal = new Vector3f();
			Vector<Vector3f> vertexArray = new Vector<Vector3f>();
			VertexBuffer kVBuffer;
			int[] aiConnect;

			TriMesh kMesh = null;
			float x, y, z;
			Vector3f vertex1, vertex2, vertex3;
			int index = 0;
			Integer searchIndex;
			Vector<Integer> connectivity = new Vector<Integer>();
			HashMap<String, Integer> vertexHashtable = new HashMap<String, Integer>();

			float _res[] = kImage.getFileInfo(0).getResolutions();
			float[] _startLocation = kImage.getFileInfo()[0].getOrigin();
			int[] _direction = MipavCoordinateSystems.getModelDirections(kImage);

			Vector3f ptIn = new Vector3f();
			Vector3f ptOut = new Vector3f();

			int Number_faces; // First info (after the header) on the file

			System.out.println("Machine's endian: " + ByteOrder.nativeOrder());

			// It's a local file

			// First 80 bytes aren't important
			if (80 != data.read(Info)) { // File is incorrect
				System.out.println("Format Error: 80 bytes expected");
				return;
			}

			data.read(Array_number); // We get the 4 bytes
			dataBuffer = ByteBuffer.wrap(Array_number); // ByteBuffer for
														// reading correctly the
														// int
			dataBuffer.order(ByteOrder.nativeOrder()); // Set the right order
			Number_faces = dataBuffer.getInt();

			Temp_Info = new byte[50 * Number_faces]; // Each face has 50 bytes
														// of data

			data.read(Temp_Info); // We get the rest of the file

			dataBuffer = ByteBuffer.wrap(Temp_Info); // Now we have all the data
														// in this ByteBuffer
			dataBuffer.order(ByteOrder.nativeOrder());

			// System.out.println("Number of faces= " + Number_faces);

			// We can create that array directly as we know how big it's going
			// to be

			for (int i = 0; i < Number_faces; i++) {

				// readFacetB(dataBuffer,i);
				Point3f vertex = new Point3f();

				// System.out.println("Reading face number " + i);

				// Read the Normal
				normal = new Vector3f();
				normal.X = dataBuffer.getFloat();
				normal.Y = dataBuffer.getFloat();
				normal.Z = dataBuffer.getFloat();

				// System.out.println("Normal: X=" + normal.X + " Y=" + normal.Y + " Z=" + normal.Z);

				// Read vertex1
				vertex1 = new Vector3f();
				vertex1.X = dataBuffer.getFloat();
				vertex1.Y = dataBuffer.getFloat();
				vertex1.Z = dataBuffer.getFloat();

				// System.out.println("Vertex 1: X=" + vertex1.X + " Y=" + vertex1.Y + " Z=" + vertex1.Z);

				x = vertex1.X;
				y = vertex1.Y;
				z = vertex1.Z;

				ptIn.X = x;
				ptIn.Y = y;
				ptIn.Z = z;

				MipavCoordinateSystems.scannerToFile(ptIn, ptOut, kImage);
             
				// x = (ptOut.X * _res[0] * _direction[0]) + _startLocation[0];
				// y = (ptOut.Y * _res[1] * _direction[1]) + _startLocation[1];
				// z = (ptOut.Z * _res[2] * _direction[2]) + _startLocation[2];
				x = ptOut.X;
				y = ptOut.Y;
				z = ptOut.Z;
				
				
				searchIndex = vertexHashtable.get((x + " " + y + " " + z));
				if (searchIndex == null) { // not found
					vertexHashtable.put((x + " " + y + " " + z), new Integer(index));
					connectivity.add(new Integer(index));
					vertexArray.add(new Vector3f(x, y, z));
					if ( z > zMax ) {
						zMax = z;
					}
					if ( z < zMin ) {
						zMin = z;
					}
					index++;
				} else {
					connectivity.add(searchIndex);
				}

				// Read vertex2
				vertex2 = new Vector3f();
				vertex2.X = dataBuffer.getFloat();
				vertex2.Y = dataBuffer.getFloat();
				vertex2.Z = dataBuffer.getFloat();
				// System.out.println("Vertex 2: X=" + vertex2.X + " Y=" + vertex2.Y + " Z=" + vertex2.Z);

				x = vertex2.X;
				y = vertex2.Y;
				z = vertex2.Z;

				ptIn.X = x;
				ptIn.Y = y;
				ptIn.Z = z;

				MipavCoordinateSystems.scannerToFile(ptIn, ptOut, kImage);

				// x = (ptOut.X * _res[0] * _direction[0]) + _startLocation[0];
				// y = (ptOut.Y * _res[1] * _direction[1]) + _startLocation[1];
				// z = (ptOut.Z * _res[2] * _direction[2]) + _startLocation[2];
              x = ptOut.X;
              y = ptOut.Y;
              z = ptOut.Z;
				
				searchIndex = vertexHashtable.get((x + " " + y + " " + z));
				if (searchIndex == null) { // not found
					vertexHashtable.put((x + " " + y + " " + z), new Integer(index));
					connectivity.add(new Integer(index));
					vertexArray.add(new Vector3f(x, y, z));
					if ( z > zMax ) {
						zMax = z;
					}
					if ( z < zMin ) {
						zMin = z;
					}
					index++;
				} else {
					connectivity.add(searchIndex);
				}

				// Read vertex3
				vertex3 = new Vector3f();
				vertex3.X = dataBuffer.getFloat();
				vertex3.Y = dataBuffer.getFloat();
				vertex3.Z = dataBuffer.getFloat();
				// System.out.println("Vertex 3: X=" + vertex3.X + " Y=" + vertex3.Y + " Z=" + vertex3.Z);

				x = vertex3.X;
				y = vertex3.Y;
				z = vertex3.Z;

				ptIn.X = x;
				ptIn.Y = y;
				ptIn.Z = z;

				MipavCoordinateSystems.scannerToFile(ptIn, ptOut, kImage);

				// x = (ptOut.X * _res[0] * _direction[0]) + _startLocation[0];
				// y = (ptOut.Y * _res[1] * _direction[1]) + _startLocation[1];
				// z = (ptOut.Z * _res[2] * _direction[2]) + _startLocation[2];
				x = ptOut.X;
				y = ptOut.Y;
				z = ptOut.Z;
				
				searchIndex = vertexHashtable.get((x + " " + y + " " + z));
				if (searchIndex == null) { // not found
					vertexHashtable.put((x + " " + y + " " + z), new Integer(index));
					connectivity.add(new Integer(index));
					vertexArray.add(new Vector3f(x, y, z));
					if ( z > zMax ) {
						zMax = z;
					}
					if ( z < zMin ) {
						zMin = z;
					}
					index++;
				} else {
					connectivity.add(searchIndex);
				}
				// After each facet there are 2 bytes without information
				// In the last iteration we dont have to skip those bytes..
				if (i != Number_faces - 1) {
					dataBuffer.get();
					dataBuffer.get();
				}

			} // End for

			int vertexCount = vertexArray.size();
			VOIVector voiVectorNew = new VOIVector();
			VOI voiNew = new VOI((short)0, "ImageVOI");
			voiVectorNew.add(voiNew);
			
			int startIndex = Math.round(zMin+1);
			int endIndex = Math.round(zMax-1);
			for ( int j = startIndex; j < endIndex; j++ ) {
				
				// 1. filter out closed points
				Vector<Vector3f> ptsArray = new Vector<Vector3f>();
				float centerX = 0; 
				float centerY = 0;
				int i;
				for (i = 0; i < vertexCount; i++) {
					Vector3f pos = vertexArray.elementAt(i);
					if ( Math.abs(pos.Z - j) < 0.1 ) {
						ptsArray.add(new Vector3f(pos.X, pos.Y, j));
						centerX += pos.X;
						centerY += pos.Y;
					}
				}
				
				// 2. compute center;
				int numPts = ptsArray.size();
				
				if ( numPts <= 20 ) continue;
				
				centerX = centerX / numPts;
				centerY = centerY / numPts;
				Vector3f center = new Vector3f(centerX, centerY, j);
				
				Hashtable<Float, Vector3f> ptsTable = new Hashtable<Float, Vector3f>();
				
				// 3.  compute polar coordinate
				for ( i = 0; i < numPts; i++ ) {
					Vector3f loc = ptsArray.get(i);
					Vector2f in = new Vector2f(loc.X, loc.Y);
					Vector2f out = new Vector2f(0, 0);
					MipavCoordinateSystems.CartesianToPolar2D(in, out, center);
					ptsTable.put(out.Y, new Vector3f(loc.X, loc.Y, j));
					
				}
				
				ArrayList<Float> ptsList = Collections.list(ptsTable.keys());
				Collections.sort(ptsList);
				Iterator<Float> it = ptsList.iterator();
				Vector3f[] ptsResult = new Vector3f[numPts];
				int idx = 0;
				while ( it.hasNext() ) {
					float key = it.next();
					ptsResult[idx] = ptsTable.get(key);
					idx++;
				}
				
				VOIBase vTemp = new VOIContour(true);
				vTemp.importPoints(ptsResult);
				voiNew.importCurve(vTemp);
				
			}  // int j = startIndex; j <= endIndex; j++ 

			kImage.addVOIs(voiVectorNew);
			smoothVOI60(kImage, kImage);
			// new ViewJFrameImage(kImage);
		
			if ( origImageTable.get(hashID) == null ) {
				origImageTable.put(hashID, new Hashtable<String, ModelImage>());
			}
			
			if ( imageOrientation == FileInfoBase.AXIAL ) {
				origImageTable.get(hashID).put("axial", kImage);
			} else if ( imageOrientation == FileInfoBase.SAGITTAL ) {
				origImageTable.get(hashID).put("sagittal", kImage);
			} else if ( imageOrientation == FileInfoBase.CORONAL ) {
				origImageTable.get(hashID).put("coronal", kImage);
			}
			

		}// End of readBinaryFile  

	public static void pause() {
		System.err.println("enter to continue: ");
		try {
			for (int av = System.in.available(); av > 0; av--) {
				System.in.read();
			}
			System.in.read();
		} catch (IOException e) {
			System.err.println("keyboard failed: " + e);
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