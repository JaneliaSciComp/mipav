package gov.nih.mipav.view.renderer.WildMagic.Knees;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * This class converts the original knees MRI images into isotropic images.
 * 
 * 1) Convert the VOIs into binary image masks. 
 * 2) Transform the binary mask images to original image resolution.
 * 3) Then, save the binary mask into .nii file format for comparison. 
 * 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogSKI10_femur_eval_pre extends JDialogBase implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** image vector to hold the actual images. */
	private Hashtable<String, ModelImage> keyImages = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> maskImages = new Hashtable<String, ModelImage>();

	private Hashtable<String, ModelImage> segKeyImages = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> segMaskImages = new Hashtable<String, ModelImage>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	private AlgorithmTransform algoTrans;


	Hashtable<String, ModelImage> cedTableAxial = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTableSagittal = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTableCoronal = new Hashtable<String, ModelImage>();

	Hashtable<String, String> origTable = new Hashtable<String, String>();
	Hashtable<String, String> nameTable = new Hashtable<String, String>();
	Hashtable<String, String> nameVOITable = new Hashtable<String, String>();

	Hashtable<String, String> seg_nameTable = new Hashtable<String, String>();
	Hashtable<String, String> seg_nameVOITable = new Hashtable<String, String>();
	Hashtable<String, String> nameVOI_GT_Table = new Hashtable<String, String>();

	Hashtable<String, ModelImage> keyImagesTransform = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> keyImageMasksTransform = new Hashtable<String, ModelImage>();

	Hashtable<String, String> imageNameHashtable = new Hashtable<String, String>();
	Hashtable<String, String> voiNameHashtable = new Hashtable<String, String>();

	Hashtable<String, String> nameTableImages = new Hashtable<String, String>();
	Hashtable<String, String> nameTableImagesMask = new Hashtable<String, String>();
	
	private Hashtable<String, ModelImage> keyImagesOrientation = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> keyImagesOrientationMask = new Hashtable<String, ModelImage>();

	Hashtable<String, ModelImage> maskThresAxial = new Hashtable<String, ModelImage>();
	
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogSKI10_femur_eval_pre(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
		setVisible(true);

	}

	/**
	 * dispose memory
	 */
	public void disposeLocal() {

		int i;

		for (i = 0; i < keyImages.size(); i++) {
			ModelImage temp = keyImages.get(i);
			temp.disposeLocal();
		}

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

			saveImageDirectory = String.valueOf(saveImageChooser.getCurrentDirectory()) + File.separatorChar
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

		File fileDir = new File("/backup/Knee_2010_challenge/trainData");
		traverse_image_folder(fileDir, null);
		
	    File fileResultDir = new File("/backup/Knee_2010_challenge/voi");
	    traverse_voi_folder(fileResultDir);
	}

	
	private void traverse_image_folder(File dir, String hashID) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				read_image_name(new File(dir, children[i]));
			}
		}

	}

	private void read_image_name(File dir) {

		String dirName = dir.toString();

		String lowerCaseName;
		String fileName;
		String hashID;

		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		lowerCaseName = dirName.toLowerCase();
		fileName = lowerCaseName.substring(begin, end);
		begin = fileName.lastIndexOf("-") + 1;
		end = fileName.lastIndexOf(".");

		hashID = fileName.substring(begin, end);

		
		if (fileName.startsWith("image") && fileName.endsWith("mhd")) {
			System.err.println("hashID = " + hashID + "  dirName = " + dirName);
			nameTableImages.put(hashID, dirName);
		}

		if (fileName.startsWith("labels") && fileName.endsWith("mhd")) {
			System.err.println("hashID = " + hashID + "  dirName = " + dirName);
			nameTableImagesMask.put(hashID, dirName);
		}

	}
	

	
	private void traverse_voi_folder(File dir) {

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_fold(new File(dir, children[i]));
			}

		}
	}
	
	private void traverse_fold(File dir) {
		if ( dir.isDirectory() ) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				if ( children[i].equals("axial")) {
				 traverse_id(new File(dir, children[i]));
				}
			}
		}
	}
	
	private void traverse_id(File dir ) {
		if ( dir.isDirectory() ) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				 traverse_file(new File(dir, children[i]), children[i]);
			}
		}
	}
	
	private void traverse_file(File dir, String id) {
		String dirName;
		String fileName;
		String hashID;
		int index;
		
		if ( dir.isDirectory() ) {
			String[] children = dir.list();
			hashID = null;
			String voiName = null;
			String imageName = null;
			for ( int i = 0; i < children.length; i++ ) {
				// System.err.println("dir = " + firstLayer.toString());
				fileName = children[i];
				// System.err.println("fileName = " + fileName);
				
				if ( !fileName.startsWith("voi") && fileName.endsWith("xml")) { 
					imageName = dir.toString() + File.separator + fileName; 
					index = fileName.indexOf(".");
				    hashID = fileName.substring(0, index); 
				} else if ( fileName.startsWith("voi") && fileName.endsWith("xml")) { 
					voiName = dir.toString() + File.separator + fileName; }
			}
			
			if (hashID != null) {
				System.err.println("hashID = " + hashID);
				System.err.println("imageName = " + imageName);
				System.err.println("voiName = " + voiName);
				System.err.println();
				seg_nameTable.put(hashID, imageName);
				seg_nameVOITable.put(hashID, voiName);

			}
		}
	}


	/**
	 * This method is required if the AlgorithmPerformed interface is implemented.
	 * It is called by the algorithms when it has completed or failed to to
	 * complete, so that the dialog can be display the result image and/or clean up.
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

		threshold_GT();
		generateGTContours();
		
		convertVOItoMask();
		transform();
		generateSegContours();
		

		System.err.println("saveImage");
    
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");
		// System.gc();

	}
	
	public void threshold_GT() {

		Set<String> keys = keyImagesOrientationMask.keySet();

		for (String key : keys) {
			
				System.err.println("key = " + key);
				ModelImage maskImage = keyImagesOrientationMask.get(key);
				ModelImage thresImage = calculateThreshold(maskImage);
				maskThresAxial.put(key, thresImage);
			
		}

	}
	
	private ModelImage calculateThreshold(ModelImage srcImage) {
		
		float[] thresholds = new float[2];
    	thresholds[0] = 1.0f;
    	thresholds[1] = 1.0f;
    	ModelImage thresholdImage = new ModelImage(ModelImage.UBYTE, srcImage.getExtents(), srcImage.getImageName()+ "threshold");
    	(thresholdImage.getFileInfo(0)).setModality(FileInfoBase.OTHER);
    	// fillValue not used
    	float fillValue = 0.0f;
    	int outputType = AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE;
    	boolean wholeImage = true;
    	boolean isInverse = false;
    	AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(thresholdImage, srcImage, thresholds, fillValue, outputType,
                wholeImage, isInverse);
    	thresholdAlgo.run();
    	FileInfoBase[] fileInfo = thresholdImage.getFileInfo();
        fileInfo[0].setModality(FileInfoBase.OTHER);
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setEndianess(srcImage.getFileInfo()[0].getEndianess());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(thresholdImage.getExtents());
        fileInfo[0].setMax(thresholdImage.getMax());
        fileInfo[0].setMin(thresholdImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        fileInfo[0].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
        fileInfo[0].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
        thresholdAlgo.finalize();
        thresholdAlgo = null;
		
		return thresholdImage;
	}
	 

	private void generateGTContours() {

		Set<String> keys = maskThresAxial.keySet();

		for (String key : keys) {

			ModelImage srcImage = maskThresAxial.get(key);

			String directory = saveImageDirectory + File.separator;

			File dirFile = new File(directory);
			if (!dirFile.isDirectory()) {
				dirFile.mkdir();
			}

			AlgorithmChangeType changeZ = new AlgorithmChangeType(srcImage, ModelImage.UBYTE, srcImage.getMin(),
					srcImage.getMax(), srcImage.getMin(), srcImage.getMax(), false);
			changeZ.run();

			srcImage.saveImage(directory, key + ".nii", FileUtility.NIFTI, false);
			
		}

	}

	private void generateSegContours() {

		Set<String> keys = keyImageMasksTransform.keySet();

		for (String key : keys) {

			ModelImage srcImage = keyImageMasksTransform.get(key);

			String directory = saveImageDirectory + File.separator;

			File dirFile = new File(directory);
			if (!dirFile.isDirectory()) {
				dirFile.mkdir();
			}

			AlgorithmChangeType changeZ = new AlgorithmChangeType(srcImage, ModelImage.UBYTE, srcImage.getMin(),
					srcImage.getMax(), srcImage.getMin(), srcImage.getMax(), false);
			changeZ.run();

			srcImage.saveImage(directory, key + ".nii", FileUtility.NIFTI, false);

		}

	}

	public void transform() {

		Set<String> keys = segMaskImages.keySet();

		System.err.println("Transform");

		for (String key : keys) {

			System.err.println("Transform key = " + key);

			ModelImage keyImage = maskThresAxial.get(key);
			ModelImage keyImageMask = segMaskImages.get(key);

			ModelImage transKeyImageMask = calculateTransform(keyImageMask, keyImage);
			keyImageMasksTransform.put(key, transKeyImageMask);

		}

	}

	public void convertVOItoMask() {

		Set<String> keys = segKeyImages.keySet();

		System.err.println("convertVOItoMask");

		for (String key : keys) {
			
			ModelImage targetImage = segKeyImages.get(key);

			System.err.println("key = " + key);

			ModelImage maskImage = null;
			maskImage = targetImage.generateBinaryImage();
			maskImage.getMatrixHolder().replaceMatrices(targetImage.getMatrixHolder().getMatrices());
			maskImage.getFileInfo(0).setOrigin(targetImage.getFileInfo(0).getOrigin());
			
			segMaskImages.put(key, maskImage);

		}

	}

	public ModelImage calculateTransform(ModelImage keyImage, ModelImage srcImage) {

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


		doVOI = false;
		doClip = false;
		doPad = false;
		doRotateCenter = false;
		center = null;

		fillValue = 0.0f;
		doUpdateOrigin = true;
		isSATransform = false;

		interp = AlgorithmTransform.NEAREST_NEIGHBOR;
		xfrm = new TransMatrix(3);
		xfrm.identity();

		units = new int[3];
		units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

		oXres = srcImage.getFileInfo()[0].getResolutions()[0];
		oYres = srcImage.getFileInfo()[0].getResolutions()[1];
		oZres = srcImage.getFileInfo()[0].getResolutions()[2];

		oXdim = srcImage.getFileInfo()[0].getExtents()[0];
		oYdim = srcImage.getFileInfo()[0].getExtents()[1];
		oZdim = srcImage.getFileInfo()[0].getExtents()[2];

		System.err.println("oXdim = " + oXdim + " oYdim = " + oYdim);

		algoTrans = new AlgorithmTransform(keyImage, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim, units,
				doVOI, doClip, doPad, doRotateCenter, center);
		algoTrans.setFillValue(fillValue);
		algoTrans.setUpdateOriginFlag(doUpdateOrigin);
		algoTrans.setUseScannerAnatomical(isSATransform);
		algoTrans.run();

		resultImage = algoTrans.getTransformedImage();
		resultImage.setImageOrientation(FileInfoBase.AXIAL);
		FileInfoBase fileInfo[] = resultImage.getFileInfo();

		for (int i = 0; i < fileInfo.length; i++) {
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_R2L_TYPE, 0);
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_A2P_TYPE, 1);
			fileInfo[i].setAxisOrientation(FileInfoBase.ORI_I2S_TYPE, 2);
		}
		resultImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
		resultImage.calcMinMax();

		algoTrans.disposeLocal();
		algoTrans = null;
		return resultImage;
	}

	/**
	 * load image files and voi files
	 */
	public void loadFiles() {
		readGTFile();
		readSegFile();
		System.err.println("finish image I/O");
	}

	public void readGTFile() {

		int index;
		
		try {
			
			Set<String> keys = nameTableImages.keySet();
            
			for (String key : keys) {
				System.err.println("axial key = " + key);
				String dir = nameTableImages.get(key);
				if (dir == null)
					continue;
				// System.err.println("dir = " + dir);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					
					// new ViewJFrameImage(image);
					keyImagesOrientation.put(key, image);
					keyImagesOrientation.get(key).setImageName(key);
					
				}
				keyImageIO = null;
			}
			
			
			keys = nameTableImagesMask.keySet();

			for (String key : keys) {
				System.err.println("axial mask key = " + key);
				String dir = nameTableImagesMask.get(key);
				if (dir == null)
					continue;
				// System.err.println("dir = " + dir);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					// new ViewJFrameImage(image);
					keyImagesOrientationMask.put(key, image);
					keyImagesOrientationMask.get(key).setImageName(key);
					
				}
				keyImageIO.dispose();
				keyImageIO = null;
			}
		

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void readSegFile() {

		int index;
		try {
			// read key images and VOIs

			Set<String> keys = nameTable.keySet();
        
			for (String key : keys) {
				System.err.println("key = " + key);
				String dir = nameTable.get(key);
				if (dir == null)
					continue;

				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {
					
					System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
					keyImages.put(key, image);
					keyImages.get(key).setImageName(key);


				}
				keyImageIO = null;
			}
			
			// read seg images, ruida
			
			Set<String> segKeys = seg_nameTable.keySet();

			for (String key : segKeys) {
				System.err.println("key = " + key);
				String dir = seg_nameTable.get(key);
				
				String voiName = seg_nameVOITable.get(key);
				
				if (dir == null)
					continue;

				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));

				FileIO keyImageIO = new FileIO();
				keyImageIO.setQuiet(true);
				ModelImage image = keyImageIO.readImage(fileName, directory);
				if (image != null) {

					System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
			    	segKeyImages.put(key, image);
					segKeyImages.get(key).setImageName(key);
				}
				
				
				keyImageIO = null;
			}
		
		
			Set<String> voiKeys = seg_nameVOITable.keySet();
			VOI[] voi = null;
			for (String key : voiKeys) {
				System.err.println("key = " + key);
				String dir = seg_nameVOITable.get(key);

				index = dir.lastIndexOf(File.separator);
				String voiName = dir.substring(index + 1, dir.length());
				String voiDirectory = dir.substring(0, index + 1);

				if (segKeyImages.get(key) != null) {
					FileVOI fileVOI = null;
					fileVOI = new FileVOI(voiName, voiDirectory, segKeyImages.get(key));
					voi = fileVOI.readVOI(false);

					if (voi[0] != null) {
						System.err.println("voiDirectory = " + voiDirectory + "   " + "voiName = " + voiName);
						segKeyImages.get(key).registerVOI(voi[0]);
						// new ViewJFrameImage(segKeyImages.get(key));
					}

					fileVOI = null;

				}

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