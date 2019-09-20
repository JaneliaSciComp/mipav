package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
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

/**
 * This class generates the VOIs from HED prediction maps. 
 * The maps are from HED generated MRI energy maps(no CED maps involved). 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DHEDmapSPIE_2017 extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	private Vector<String> keyImageVectorOrigImg = new Vector<String>();
	private Vector<String> keyImageVOIVectorOrigImg = new Vector<String>();
	
	private Vector<String> keyImageVectorMap = new Vector<String>();
	private Vector<String> keyImageVOIVectorMap = new Vector<String>();

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
	public JDialogProstate2DHEDmapSPIE_2017(Frame theParentFrame) {
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
			sortKeyImage_OrigImg();
			sortImageTable();
			
			
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	private void sortImageTable() {
		
		int size = imageTable.size();
		for (int i = 0; i < size; i++  ) {
			Vector<String> stringVec = imageTable.get(i);
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
			maskImageTable.put(i, newStringVect);
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
		File fileDir_orig_img = new File("/data/ruida/MICAI2012/test");
		traverse_folder_org_img(fileDir_orig_img); 
		File fileDir_map = new File("/data/ruida/MICAI2012/test_prostate_mask_iter_69000");
	    traverse_folder_map(fileDir_map);
	}
	

	
	private void traverse_folder_org_img(File dir) {
		processDir_folder_org_img(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_org_img(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder_org_img(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("Case")
				&& dirName.substring(begin, end).endsWith(".mhd") 
				&& !dirName.contains("segmentation")) {
			// System.err.println(dir.toString());
			keyImageVectorOrigImg.add(dir.toString());
		}
      
		
	}
	
	

	public void sortKeyImage_OrigImg() {
		int i;
		int len = keyImageVectorOrigImg.size();
		String imageName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVectorOrigImg.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+4, end));
			// index = Integer.valueOf(imageName.substring(start+5, end));
			index = Integer.parseInt(imageName.substring(start + 4, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}

		keyImageVectorOrigImg.clear();
		for (i = 0; i < len; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVectorOrigImg.add(imageName);
			}
		}

	}
 
	private void traverse_folder_map(File dir) {

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_map(new File(dir, children[i]), i);
			}
		}
	}
	
	private void traverse_folder_map(File dir, int index) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder_map(new File(dir, children[i]), index);
			}
		}

	}

	private void processDir_folder_map(File dir, int index) {
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
			int hashIndex = Integer.valueOf(numString);
			// System.err.println(numString);
			if ( imageTable.get(hashIndex) == null ) {
				imageTable.put(hashIndex, new Vector<String>());
			}
		    imageTable.get(hashIndex).add(dir.toString());
		}
	}

	public void sortKeyImage_Map() {
		int i;
		int len = keyImageVectorMap.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVectorMap.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+5, end));
			index = Integer.valueOf(imageName.substring(start+5, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}
		
		keyImageVectorMap.clear();
		for (i = 0; i <= 49; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVectorMap.add(imageName);
			}
		}

		
		for (i = 0; i < len; i++) {
			voiName = keyImageVOIVectorMap.get(i);
			start = voiName.lastIndexOf(File.separator) + 1;
			end = voiName.lastIndexOf(".");
			// System.err.println(voiName.substring(start+3, end));
			index = Integer.valueOf(voiName.substring(start+3, end));
			// System.err.println("index = " + index);
			imageVOITable.put(index, voiName);
		}


		keyImageVOIVectorMap.clear();
		for (i = 0; i <= 49; i++) {
			voiName = imageVOITable.get(i);
			if (voiName != null) {
				keyImageVOIVectorMap.add(voiName);
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
		int len = keyImageVOIVectorOrigImg.size();
		for (int i = 0; i < len; i++) {
			System.err.println(keyImageVectorOrigImg.get(i));
			System.err.println(keyImageVOIVectorOrigImg.get(i));
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

	private void generateContours() {
		
		int size = maskTable.size();
        for(int i = 0; i < size; i++ ){
		
			Vector<ModelImage> imageVec = maskTable.get(i);
			
			ModelImage srcImage = imageHashtable.get(i);
		    String dir = keyImageVectorOrigImg.get(i);
			int index = dir.lastIndexOf(File.separator);
			String directory = new String(dir.substring(0, index + 1));
			String fileName = new String(dir.substring(index + 1, dir.length()));
			System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
			VOI voiNewFinal = new VOI((short)0, "voi_reuslt_" + i);
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
			newExtents[2] = imageVec.size();
			
			
			srcImage.registerVOI(voiNewFinal);
			new ViewJFrameImage(srcImage);
			smoothVOI30(srcImage, srcImage);
			
			// maskImage = targetImageSlice.generateBinaryImage(false, false);
			// maskImage = new ModelImage(imageA.getType(), destExtents, imageA.getImageName());
			ModelImage maskImage = new ModelImage(ModelStorageBase.UBYTE, newExtents, "mask" + i);
			maskImage = srcImage.generateUnsignedByteImage(1, false, false);
			
			try {
				FileVOI fileVOI = new FileVOI("case_" + i + "_segmetation.voi", directory, srcImage);
				fileVOI.writeVOI(voiNewFinal, true);
				
				maskImage.saveImage(directory, "case" + i + "_segmentation.mhd" , FileUtility.METAIMAGE, false);
				
			} catch (IOException e ) {
				e.printStackTrace();
			}
			
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
	 * load image files and voi files
	 */
	public void loadFiles() {
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
			int size = keyImageVectorOrigImg.size();
			
			for(int i = 0; i < size; i++ ){
	            
	            FileIO fileIO = new FileIO();
	            String imageFullName = keyImageVectorOrigImg.get(i);
	            int index = imageFullName.lastIndexOf(File.separator);
	            
                String fileName = imageFullName.substring(index+1, imageFullName.length());
                String directory = imageFullName.substring(0, index+1);
                // System.err.println("filename = " + fileName);
                // System.err.println("directory = " + directory);
                
                
                boolean multiFile = true;
                ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
                image.setImageName("case" + i + ".mhd");
                imageHashtable.put(i, image);
             
	        }

			
            int len = maskImageTable.size();
			for(int i = 0; i < len; i++ ){
				Vector<String> stringVec = maskImageTable.get(i);
				Vector<ModelImage> imageVec = new Vector<ModelImage>();
				
				for ( int j = 0; j < stringVec.size(); j++ ) {
					String dir = stringVec.get(j);
					int index = dir.lastIndexOf(File.separator);
					String directory = new String(dir.substring(0, index + 1));
					String fileName = new String(dir.substring(index + 1, dir.length()));
					// System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
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