package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
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
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import ar.com.hjg.pngj.*;

import java.net.URLDecoder;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * For ISBI 2017 paper:
 * Data given: Dr. Choyke's group gave us the 3D prostate MRI data ( axial, sagittal, coronal images with corresponding 
 * 3D stl prostate surfaces.  No, ground truth VOIs were given. 
 * 
 * This class reads the images and stl surfaces, converts 3D surface to VOI contours.   
 * And save axial, sagittal, coronal VOIs with corresponding image in .xml file format.  
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesAtlasPngConverter3DSurface extends JDialogBase implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	
	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;


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
	
	Hashtable<String, Integer> dicomTable = new Hashtable<String, Integer>();
	Hashtable<String, Integer> voiTable = new Hashtable<String, Integer>();

	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesAtlasPngConverter3DSurface(Frame theParentFrame) {
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
		
		File fileDir_1 = new File("/scratch/ISBI2017/newData");
		traverse_Layer(fileDir_1);

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
			} 
		}
		
	}


	private void traverse_scanLayer(File secondLayer, String hashID) {

		String[] children = secondLayer.list();

		for (int i = 0; i < children.length; i++) {
			String decodedPath = URLDecoder.decode(secondLayer + File.separator + children[i]);
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
				String decodedPath = URLDecoder.decode(T2Layer + File.separator + children[i]);
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

		loadFiles();


		System.err.println("saveImage");
		saveGroundTruth();
		// mad!
		// crossValidationTrain();
		// crossValidationTest();

		// saveTestedImages();
		// disposeLocal();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	
	public void saveGroundTruth() {

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
	}

	public void crossValidationTrain() {


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
		/*
		Set<String> keys = imageHashtable.keySet();
		int dataSize = keys.size();

		int folder1StartIndex = 0;
		int folder1EndIndex = (int) (dataSize * 0.2f);
		System.err.println("folder1StartIndex = " + folder1StartIndex + "   folder1EndIndex = " + folder1EndIndex);

		int folder2StartIndex = folder1EndIndex + 1;
		int folder2EndIndex = (int) (dataSize * 0.4f);
		System.err.println("folder2StartIndex = " + folder2StartIndex + " folder2EndIndex = " + folder2EndIndex);

		int folder3StartIndex = folder2EndIndex + 1;
		int folder3EndIndex = (int) (dataSize * 0.6f);
		System.err.println("folder3StartIndex = " + folder3StartIndex + " folder3EndIndex = " + folder3EndIndex);

		int folder4StartIndex = folder3EndIndex + 1;
		int folder4EndIndex = (int) (dataSize * 0.8f);
		System.err.println("folder4StartIndex = " + folder4StartIndex + " folder4EndIndex = " + folder4EndIndex);

		int folder5StartIndex = folder4EndIndex + 1;
		int folder5EndIndex = dataSize;
		System.err.println("folder5StartIndex = " + folder5StartIndex + " folder5EndIndex = " + folder5EndIndex);

		int[] index = new int[1];

		// Save Folder 1
		int folder1count = 0;
		index[0] = 0;
		for (String key : keys) {

			if (folder1count <= folder1EndIndex) {
			}

			if (folder1count > folder1EndIndex) {
				saveImages(key, index, "fold1Test" + File.separator + "train");
			}

			folder1count++;
		}

		int folder2count = 0;
		index = new int[1];
		index[0] = 0;
		for (String key : keys) {

			if (folder2count < folder2StartIndex) {
				saveImages(key, index, "fold2Test" + File.separator + "train");
			}

			if (folder2count >= folder2StartIndex && folder2count <= folder2EndIndex) {
			}

			if (folder2count > folder2EndIndex) {
				saveImages(key, index, "fold2Test" + File.separator + "train");
			}

			folder2count++;
		}

		int folder3count = 0;
		index = new int[1];
		index[0] = 0;
		for (String key : keys) {

			if (folder3count < folder3StartIndex) {
				saveImages(key, index, "fold3Test" + File.separator + "train");
			}

			if (folder3count >= folder3StartIndex && folder3count <= folder3EndIndex) {
			}

			if (folder3count > folder3EndIndex) {
				saveImages(key, index, "fold3Test" + File.separator + "train");
			}

			folder3count++;
		}

		int folder4count = 0;
		index = new int[1];
		index[0] = 0;
		for (String key : keys) {

			if (folder4count < folder4StartIndex) {
				saveImages(key, index, "fold4Test" + File.separator + "train");
			}

			if (folder4count >= folder4StartIndex && folder4count <= folder4EndIndex) {
			}

			if (folder4count > folder4EndIndex) {
				saveImages(key, index, "fold4Test" + File.separator + "train");
			}
			folder4count++;
		}

		int folder5count = 0;
		index = new int[1];
		index[0] = 0;
		for (String key : keys) {
			if (folder5count < folder5StartIndex) {
				saveImages(key, index, "fold5Test" + File.separator + "train");
				folder5count++;
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

						System.err.println("index = " + index[0]);

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

						System.err.println("index = " + index[0]);

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

	public void saveImagesTest(String key, int[] index, String folderName) {
		try {

			// if ( i == count ) continue;

			ModelImage cropKeyImage = imageHashtable.get(key).get(0);

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

			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator + key + File.separator;

			// String sliceDir = saveImageDirectory +
			// File.separator + i + File.separator;

			File sliceDirFile = new File(sliceDir);
			if (!sliceDirFile.isDirectory())
				sliceDirFile.mkdir();

			for (int j = 0; j <= zDim; j++) {

				try {

					System.err.println(" image number = " + key + "   slice number = " + j);

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
						// String sliceDir = saveImageDirectory +
						// File.separator + i + File.separator;

						System.err.println("index = " + index[0]);

						// String imgName = "image_" + index[0] + ".png";
						String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						ModelImage maskImage = null;
						maskImage = targetImageSlice.generateBinaryImage(false, false);
						// String maskName = "voi_" + index[0] + ".png";
						String maskName = "voi_" + j + ".png";
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

						System.err.println("index = " + index[0]);

						// String imgName = "image_" + index[0] + ".png";
						String imgName = "image_" + j + ".png";
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
						// String maskName = "voi_" + index[0] + ".png";
						String maskName = "voi_" + j + ".png";
						// maskImage.saveImage(sliceDir, maskName,
						// FileUtility.JIMI, false);
						// new ViewJFrameImage(maskImage);
						// if ( true ) break;
						savePNGfile(sliceDir, maskName, maskImage, min, max,xDim, yDim, true);

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
	 * load image files and voi files
	 */
	public void loadFiles() {
		readFile();
		System.err.println("finish image I/O");

	}


	public void readFile() {

		srcImageTable.put("axial", new Vector<ModelImage>());
		srcImageTable.put("sagittal", new Vector<ModelImage>());
		srcImageTable.put("coronal", new Vector<ModelImage>());
		
		try {
			
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
							
								int imageOrientation = image.getImageOrientation();
								
								if ( imageOrientation == FileInfoBase.AXIAL ) {
									voiFileName = voiSet.get("axial");
								} else if ( imageOrientation == FileInfoBase.SAGITTAL ) {
									voiFileName = voiSet.get("sagittal");
								} else if ( imageOrientation == FileInfoBase.CORONAL ) {
									voiFileName = voiSet.get("coronal");
								}
									
							    loadSTLBinaryMesh(new File(voiFileName), image, imageOrientation, hashID);
							
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
	  
	  
	  /**
	   * This method read the STL surface file, convert the 3D surface into VOI contours.    
	   * @param data
	   * @param kImage
	   * @param imageOrientation
	   * @param hashID
	   * @throws IOException
	   */
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