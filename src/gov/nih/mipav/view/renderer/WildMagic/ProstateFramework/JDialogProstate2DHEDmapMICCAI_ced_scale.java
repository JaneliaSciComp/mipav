package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * This class generates the VOIs from HED prediction maps.  
 * 
 * Only takes MRI transformed images
 * into concern, no CED involved.   The transformed images are isotropic in x, y direction.  
 * 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DHEDmapMICCAI_ced_scale extends JDialogBase implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	private Vector<String> keyImageVector1 = new Vector<String>();

	private Vector<String> keyImageVector5 = new Vector<String>();
	private Vector<String> keyImageVOIVector5 = new Vector<String>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<Integer, Vector<String>> imageTable = new Hashtable<Integer, Vector<String>>();
	Hashtable<Integer, Vector<String>> newImageTable = new Hashtable<Integer, Vector<String>>();
	Hashtable<Integer, Vector<ModelImage>> maskTable = new Hashtable<Integer, Vector<ModelImage>>();

	Hashtable<Integer, Vector<String>> maskImageTable = new Hashtable<Integer, Vector<String>>();


	Hashtable<Integer, ModelImage> imageHashtable = new Hashtable<Integer, ModelImage>();
	Hashtable<String, VOI> voiHashtable = new Hashtable<String, VOI>();

	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DHEDmapMICCAI_ced_scale(Frame theParentFrame) {
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
			sortKeyImage_1();
			sortImageTable();
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	private void sortImageTable() {
		
		int size = imageTable.size();
		for (int i = 0; i < size; i++) {
			Vector<String> stringVec = imageTable.get(i);
			Vector<String> newStringVect = new Vector<String>();
			Hashtable<Integer, String> hash = new Hashtable<Integer, String>();
			for (int j = 0; j < stringVec.size(); j++) {
				String fullName = stringVec.get(j);
				int stopIndex = fullName.lastIndexOf("-");
				String subName = fullName.substring(0, stopIndex - 1);
				stopIndex = subName.lastIndexOf("-");
				int startIndex = fullName.lastIndexOf("_");
				String numString = fullName.substring(startIndex + 1, stopIndex);
				int num = Integer.valueOf(numString);
				hash.put(num, fullName);
			}

			for (int j = 0; j < 1000; j++) {
				String name = hash.get(j);
				if (name != null) {
					newStringVect.add(name);
					System.err.println("name = " + name);
				}
			}
			maskImageTable.put(i, newStringVect);
		}
		
		System.err.println(" finish inti read dir");
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
		File fileDir_1 = new File("/data/ruida/MICAI2012/test");
		traverse_folder_1(fileDir_1);
		File fileDir_5_test = new File("/data/ruida/MICCAI_2012_cg/test_prostate_mask_cg_ced_trans_iter_55000");
		traverse_folder_5(fileDir_5_test);
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
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("Case") && dirName.substring(begin, end).endsWith(".mhd") && !dirName.contains("segmentation")) {
			keyImageVector1.add(dir.toString());
		}

	}

	public void sortKeyImage_1() {
		int i;
		int len = keyImageVector1.size();
		String imageName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector1.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			index = Integer.parseInt(imageName.substring(start + 4, end));
			imageNameTable.put(index, imageName);
		}

		keyImageVector1.clear();
		for (i = 0; i < len; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector1.add(imageName);
			}
		}

	}


	private void processDir_folder_5(File dir) {
		String dirName = dir.toString();
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("image") && dirName.substring(begin, end).endsWith(".xml")) {
			
			keyImageVector5.add(dir.toString());
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
		processDir_folder_5(dir);

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder_5(new File(dir, children[i]), index);
			}
		}

	}

	
	
	private void processDir_folder_5(File dir, int index) {
		String dirName = dir.toString();
		
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("image") && 
				dirName.substring(begin, end).endsWith(".png") && 
				dirName.contains("dsn5")) {
			String fullName = dir.toString();
			int idxEnd = fullName.lastIndexOf("/");
			String subName = fullName.substring(0, idxEnd);
			int idxStart = subName.lastIndexOf("/");
			String numString = fullName.substring(idxStart + 1, idxEnd);
			int hashIndex = Integer.valueOf(numString);
			if (imageTable.get(hashIndex) == null) {
				imageTable.put(hashIndex, new Vector<String>());
			}
			imageTable.get(hashIndex).add(dir.toString());
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
			index = Integer.valueOf(imageName.substring(start + 5, end));
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
			index = Integer.valueOf(voiName.substring(start + 3, end));
			imageVOITable.put(index, voiName);
		}

		keyImageVOIVector5.clear();
		for (i = 0; i <= 49; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVector5.add(voiName);
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
	 * Driver function to read image and VOIs, and convert each 3D image to 2D
	 * slices.
	 */
	public void callAlgorithm() {
		long startTime = System.currentTimeMillis();

		loadFiles();

		System.err.println("saveImage");

		generateContours();

		disposeLocal();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	public ModelImage calculateTransform(ModelImage image, int scaledDimX, int scaledDimY) {
		ModelImage resultImage;
		TransMatrix xfrm;
		int interp;
		float oXres, oYres, oZres, cXres, cYres, cZres;
		int oXdim, oYdim, oZdim, cXdim, cYdim, cZdim;
		int[] units;
		boolean doVOI, doClip, doPad, preserveFOV, doUpdateOrigin, doInvMat;
		boolean doRotateCenter;
		float fillValue = 0.0f;
		boolean isSATransform = false;
		Vector3f center = null;

		float[] dims = new float[2];
		float[] resols = new float[2];
		float factor, fov;
		float userValue;
		int constantFOV = 1;

		factor = 1.f;

		dims[0] = image.getFileInfo()[0].getExtents()[0];
		dims[1] = image.getFileInfo()[0].getExtents()[1];

		resols[0] = image.getFileInfo()[0].getResolutions()[0];
		resols[1] = image.getFileInfo()[0].getResolutions()[1];

		doVOI = false;
		doClip = true;
		doPad = false;
		doRotateCenter = false;
		center = null;

		fillValue = 0.0f;
		doUpdateOrigin = true;
		isSATransform = false;

		interp = 1;
		xfrm = new TransMatrix(3);
		xfrm.identity();

		units = new int[2];
		units[0] = units[1] = Unit.MILLIMETERS.getLegacyNum();

		oXdim = scaledDimX;
		oYdim = scaledDimY;

		userValue = oXdim;
		factor = (userValue - constantFOV) / (dims[0] - constantFOV);
		fov = (dims[0] - constantFOV) * resols[0];
		oXres = fov / (userValue - constantFOV);

		dims[1] = (dims[1] = constantFOV) * factor + constantFOV;
		oYres = resols[1] / factor;

		System.err.println("oXres = " + oXres + " oYres = " + oYres);

		AlgorithmTransform algoTrans = new AlgorithmTransform(image, xfrm, interp, oXres, oYres, oXdim, oYdim, units, doVOI, doClip, doPad, doRotateCenter, center);

		algoTrans.setFillValue(fillValue);
		algoTrans.setUpdateOriginFlag(doUpdateOrigin);
		algoTrans.setUseScannerAnatomical(isSATransform);
		algoTrans.run();

		resultImage = algoTrans.getTransformedImage();
		resultImage.calcMinMax();

		algoTrans.disposeLocal();
		algoTrans = null;
		return resultImage;
	}

	private void generateContours() {

		int size = maskTable.size();
		for (int i = 0; i < size; i++) {

			Vector<ModelImage> imageVec = maskTable.get(i);
			
			ModelImage srcImage = imageHashtable.get(i);
			String dir = keyImageVector1.get(i);
			int index = dir.lastIndexOf(File.separator);
			String directory = new String(dir.substring(0, index + 1));
			String fileName = new String(dir.substring(index + 1, dir.length()));
			System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
			VOI voiNewFinal = new VOI((short) 0, "voi_result_" + i);
			int[] extents = new int[2];
			int[] srcExtents = srcImage.getExtents();
			int imageSize = srcExtents[0] * srcExtents[1];
			
			int[] newExtent = new int[2];
			newExtent[0] = srcExtents[0];
			newExtent[1] = srcExtents[1];
			
			int imageDim = srcExtents[2];
			for (int j = 0; j < imageDim; j++) {

				ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtent, "target" + j);
				float[] targetBuffer = new float[imageSize];
				try {
					srcImage.exportData(j * imageSize, imageSize, targetBuffer);
					targetImageSlice.importData(0, targetBuffer, true);
					// new ViewJFrameImage(targetImageSlice);
				} catch ( IOException e ) {
					e.printStackTrace();
				}
				ModelImage gradMagImage = (ModelImage)targetImageSlice.clone();
				float[] sigmas = new float[2];
				sigmas[0] = 2.0f;
				sigmas[1] = 2.0f;
				AlgorithmGradientMagnitude  gradientMagAlgo = new AlgorithmGradientMagnitude(gradMagImage, targetImageSlice, sigmas, true, false);
				gradientMagAlgo.run();
				// new ViewJFrameImage(gradMagImage);
				gradientMagAlgo = null;
				
				
				/***********************************************************************************/ 
				
				ModelImage image = imageVec.get(j);
				ModelImage cedImage = imageVec.get(j + imageDim);

				ModelImage imageScaled = calculateTransform(image, srcExtents[0], srcExtents[1]);
				ModelImage cedImageScaled = calculateTransform(cedImage, srcExtents[0], srcExtents[1]);
			
				ModelImage minMaskImage = new ModelImage(imageScaled.getType(), imageScaled.getExtents(), makeImageName(imageScaled.getImageName(), "_calc"));
				int opType = 8;
				int clipMode = 0;
				String adOpString = null;
				AlgorithmImageCalculator mathAlgo = new AlgorithmImageCalculator(minMaskImage, imageScaled, cedImageScaled, opType, clipMode, true, adOpString);
				mathAlgo.run();
                
				// new ViewJFrameImage(minMaskImage);
				// new ViewJFrameImage(gradMagImage);
				
				curvePair curve = optimizeContours(minMaskImage, gradMagImage);
				
				if (curve != null) {

					VOIBase current_v = curve.getCurve();
					VOIBase vTemp = (VOIBase)current_v.clone();
					int nPtsCurrent = current_v.size();

					float[] xPts = new float[nPtsCurrent];
					float[] yPts = new float[nPtsCurrent];
					float[] zPts = new float[nPtsCurrent];

					current_v.exportArrays(xPts, yPts, zPts);

					for (int k = 0; k < nPtsCurrent; k++) {
						zPts[k] = j;
					}

					vTemp.importArrays(xPts, yPts, zPts, nPtsCurrent);
					voiNewFinal.importCurve(vTemp);

				}
                
			} // end for j loop
           
			voiNewFinal.update();
			// save VOI contours

			int[] newExtents = new int[3];
			newExtents[0] = extents[0];
			newExtents[1] = extents[1];
			newExtents[2] = imageVec.size();

			srcImage.registerVOI(voiNewFinal);
			new ViewJFrameImage(srcImage);
			smoothVOI30(srcImage, srcImage);

			ModelImage maskImage = new ModelImage(ModelStorageBase.UBYTE, newExtents, "mask" + i);
			maskImage = srcImage.generateUnsignedByteImage(1, false, false);

			try {
				FileVOI fileVOI = new FileVOI("case_" + i + "_cg_segmentation.voi", directory, srcImage);
				fileVOI.writeVOI(voiNewFinal, true);

				maskImage.saveImage(directory, "case_" + i + "_cg_segmentation.mhd", FileUtility.METAIMAGE, false);

			} catch (IOException e) {
				e.printStackTrace();
			}
           
		}

	}

	public curvePair optimizeContours(ModelImage maskImage, ModelImage gradMagImage ) {
		float maxScore = 0;
		int curveNum = 0;
		curvePair curve1 = getScore(maskImage, gradMagImage, 255, 255);
		curvePair curve2 = getScore(maskImage, gradMagImage, 200, 255);
		curvePair curve3 = getScore(maskImage, gradMagImage, 100, 255);
		curvePair curve4 = getScore(maskImage, gradMagImage, 30, 255);
		
		if ( curve1 != null ) {
			if ( curve1.getScore() > maxScore ) { 
				maxScore = curve1.getScore();
				curveNum = 1;
			}
		}
		
		if ( curve2 != null ) {
			if ( curve2.getScore() > maxScore ) { 
				maxScore = curve2.getScore();
				curveNum = 2;
			}
		}
		
		if ( curve3 != null ) {
			if ( curve3.getScore() > maxScore ) { 
				maxScore = curve3.getScore();
				curveNum = 3;
			}
		}
		
		if ( curve4 != null ) {
			if ( curve4.getScore() > maxScore ) { 
				maxScore = curve4.getScore();
				curveNum = 4;
			}
		}
		
		if ( curveNum == 1 ) {
			return curve1;
		} else if ( curveNum == 2 ) {
			return curve2;
		} else if ( curveNum == 3 ) {
			return curve3;
		} else if ( curveNum == 4 ) {
			return curve4;
		} else {
			return null;
		}
		
	}
	
	public curvePair getScore(ModelImage maskImage, ModelImage gradMagImage, float minThreshold, float maxThreshold) {
        
		int[] extents = maskImage.getExtents();
		String name = "binaryMask_";
		float score = 0;
		
		ModelImage resultImage = new ModelImage(ModelImage.BOOLEAN, extents, name);
		float[] thresholds = new float[2];
		thresholds[0] = minThreshold;
		thresholds[1] = maxThreshold;
		float fillValue = 0f;
		boolean isInverse = false;
		boolean regionFlag = true;
		int outputType = 1;

		AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(resultImage, maskImage, thresholds, fillValue, outputType, regionFlag, isInverse);
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
		if (v.size() == 0)
			return null;

		if (resultImage.getVOIs() != null) {

			int maxVOIs = 0;
			int voiIndex = 0;
			int contourIndex = 0;
			for (int k = 0; k < resultImage.getVOIs().size(); k++) {
				VOIBaseVector current_va = resultImage.getVOIs().VOIAt(k).getCurves();
				if (current_va != null && current_va.size() > 0) {
					for (int r = 0; r < current_va.size(); r++) {
						VOIBase temp_v = current_va.get(r);
						if (temp_v.size() > maxVOIs) {
							maxVOIs = temp_v.size();
							contourIndex = r;
							voiIndex = k;
						}
					}
				}
			}
			
			VOIBaseVector current_va = resultImage.getVOIs().VOIAt(voiIndex).getCurves();
			VOIBase current_v = current_va.get(contourIndex);
			int nPtsCurrent = current_v.size();

			float[] xPts = new float[nPtsCurrent];
			float[] yPts = new float[nPtsCurrent];
			float[] zPts = new float[nPtsCurrent];

			current_v.exportArrays(xPts, yPts, zPts);

           		
			for (int k = 0; k < nPtsCurrent; k++) {
				score += gradMagImage.getDouble((int)xPts[k], (int)yPts[k]);
			}

			return new curvePair(current_v, score);
		}
		
		return null;
	}
	

	public void smoothVOI30(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 30, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
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

		try {
			int size = keyImageVector1.size();

			for (int i = 0; i < size; i++) {

				FileIO fileIO = new FileIO();
				String imageFullName = keyImageVector1.get(i);
				int index = imageFullName.lastIndexOf(File.separator);

				String fileName = imageFullName.substring(index + 1, imageFullName.length());
				String directory = imageFullName.substring(0, index + 1);
			
				boolean multiFile = true;
				ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
				image.setImageName("case" + i + ".mhd");
				imageHashtable.put(i, image);

			}

			int len = maskImageTable.size();
			for (int i = 0; i < len; i++) {
				Vector<String> stringVec = maskImageTable.get(i);
				Vector<ModelImage> imageVec = new Vector<ModelImage>();

				for (int j = 0; j < stringVec.size(); j++) {
					String dir = stringVec.get(j);
					int index = dir.lastIndexOf(File.separator);
					String directory = new String(dir.substring(0, index + 1));
					String fileName = new String(dir.substring(index + 1, dir.length()));
					FileIO keyImageIO = new FileIO();
					imageVec.add(keyImageIO.readImage(fileName, directory));
				}

				maskTable.put(i, imageVec);

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
	
	class curvePair {
		private VOIBase curve;
		private float score;
		
		public curvePair(VOIBase _curve, float _score) {
			curve = _curve;
			score = _score;
		}
		
		public VOIBase getCurve() {
			return curve;
		}
		
		public float getScore() {
			return score;
		}
		
	}

}