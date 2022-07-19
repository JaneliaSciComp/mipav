package gov.nih.mipav.view.renderer.WildMagic.Knees;

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
 * For knees project, reconstruct the 3D surface from VOIs.  
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogKnees_3DReconstrucion extends JDialogBase implements AlgorithmInterface {

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
	Hashtable<String, Hashtable<String, Vector<String>>> voiNameHashtable = new Hashtable<String, Hashtable<String, Vector<String>>>();
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogKnees_3DReconstrucion(Frame theParentFrame) {
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

		// File fileDir = new File("/scratch/knees/result_VOI/");
		// File fileDir = new File("/home/ruida/temp/smooth_VOI/");
		// File fileDir = new File("/home/ruida/temp/result_VOI/");
		File fileDir = new File("/home/ruida/temp/try_voi/");
		traverse_folder(fileDir);
		
	}


	private void traverse_folder(File dir) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				imageNameHashtable.put(children[i], new Hashtable<String, Vector<String>>());
				voiNameHashtable.put(children[i], new Hashtable<String, Vector<String>>());
				traverse_folder(new File(dir, children[i]), children[i]);
			}
		}
	}
	
	private void traverse_folder(File dir, String orientation) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				imageNameHashtable.get(orientation).put(children[i], new Vector<String>());
				voiNameHashtable.get(orientation).put(children[i], new Vector<String>());
				processDir_folder(new File(dir, children[i]), orientation, children[i]);
			}
		}
	}
	
	private void processDir_folder(File dir, String orientation, String hashID) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				processDir_folder_last(new File(dir, children[i]), orientation, hashID);
			}
		}
	}
	
	private void processDir_folder_last(File dir, String orientation, String hashID) {
		
		String dirName = dir.toString();
		
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("voi") && 
				dirName.substring(begin, end).endsWith(".xml") ) {
			
			System.err.println("voi file = " + dir.toString());
			
			voiNameHashtable.get(orientation).get(hashID).add(dir.toString());
		}
		
		if (!dirName.substring(begin, end).startsWith("voi") && 
				dirName.substring(begin, end).endsWith(".xml") ) {
			
			System.err.println("image file = " + dir.toString());
			
			imageNameHashtable.get(orientation).get(hashID).add(dir.toString());
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


		System.err.println("saveImage");
		
		reconstructSurface();
		// smoothVOI();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}
	
	private void smoothVOI() {

		
		Hashtable<String, Vector<String>> imageSetAxial = imageNameHashtable.get("axial");
		Hashtable<String, Vector<String>> imageSetCoronal = imageNameHashtable.get("coronal");
		Hashtable<String, Vector<String>> imageSetSagittal = imageNameHashtable.get("sagittal");
		
		Hashtable<String, Vector<String>>  voiSetAxial = voiNameHashtable.get("axial");
		Hashtable<String, Vector<String>>  voiSetCoronal = voiNameHashtable.get("coronal");
		Hashtable<String, Vector<String>>  voiSetSagittal = voiNameHashtable.get("sagittal");
		
		Set<String> hashIDKeys = voiSetAxial.keySet();
			
		String saveDir = saveImageDirectory + File.separator;
		
		for ( String hashID : hashIDKeys ) {
		
				Vector<String> imageNameVecAxial = imageSetAxial.get(hashID);
				Vector<String> imageNameVecCoronal = imageSetCoronal.get(hashID);
				Vector<String> imageNameVecSagittal = imageSetSagittal.get(hashID);
	
				String imgFile1 = imageNameVecAxial.get(0);
				String imgFile2 = imageNameVecCoronal.get(0);
				String imgFile3 = imageNameVecSagittal.get(0);
				
			    Vector<String> xmlSetAxial = voiSetAxial.get(hashID);
			    Vector<String> xmlSetCoronal = voiSetCoronal.get(hashID);
			    Vector<String> xmlSetSagittal = voiSetSagittal.get(hashID);
			    
			    String xmlFile1 = xmlSetAxial.get(0);
		    	String xmlFile2 = xmlSetCoronal.get(0);
		    	String xmlFile3 = xmlSetSagittal.get(0);
		    	
		    	
		    	System.err.println("xmlFile1 = " + xmlFile1);
		    	System.err.println("xmlFile2 = " + xmlFile2);
		    	System.err.println("xmlFile3 = " + xmlFile3);
		    	
		    	try {
		    	
			    	// 1) read axial orientation
			    	// get axial image 
			    	int index = imgFile1.lastIndexOf(File.separator);
					String fileName = imgFile1.substring(index + 1, imgFile1.length());
					String directory = imgFile1.substring(0, index+1);
					FileIO keyImageIO = new FileIO();
					keyImageIO.setQuiet(true);
				    ModelImage imageAxial = keyImageIO.readImage(fileName, directory);
					
				    // get axial VOI 
				    index = xmlFile1.lastIndexOf(File.separator);
					String voiDirectory = new String(xmlFile1.substring(0, index + 1));
					String voiFileName = new String(xmlFile1.substring(index + 1, xmlFile1.length()));
					FileVOI fileVOI = null;
					fileVOI = new FileVOI(voiFileName, voiDirectory, imageAxial);
				    imageAxial.registerVOI(fileVOI.readVOI(false)[0]);
				    
				    smoothVOI(imageAxial, imageAxial, 30);
				    
				    // save VOI and image, axial
				    directory = saveDir + "axial" + File.separator + hashID + File.separator;  
				    File sliceDirFile = new File(directory);
					if (!sliceDirFile.isDirectory())
						sliceDirFile.mkdir();
				    fileVOI = new FileVOI(voiFileName, directory, imageAxial);  
				    fileVOI.writeVOI(imageAxial.getVOIs().VOIAt(0), true);
				    // imageAxial.saveImage(directory, fileName, FileUtility.XML, false);
				    imageAxial.saveImage(directory, fileName, FileUtility.XML, false, false, false);
				 
				    
				    // 2) read coronal orientation
			    	// get coronal image 
			    	index = imgFile2.lastIndexOf(File.separator);
					fileName = imgFile2.substring(index + 1, imgFile2.length());
					directory = imgFile2.substring(0, index+1);
					keyImageIO = new FileIO();
					keyImageIO.setQuiet(true);
				    ModelImage imageCoronal = keyImageIO.readImage(fileName, directory);
					
				    // get coronal VOI 
				    index = xmlFile2.lastIndexOf(File.separator);
					voiDirectory = new String(xmlFile2.substring(0, index + 1));
					voiFileName = new String(xmlFile2.substring(index + 1, xmlFile2.length()));
					fileVOI = null;
					fileVOI = new FileVOI(voiFileName, voiDirectory, imageCoronal);
				    imageCoronal.registerVOI(fileVOI.readVOI(false)[0]);
				    
				    smoothVOI(imageCoronal, imageCoronal, 45);
				    
				    // save VOI and image, coronal
				    directory = saveDir + "coronal" + File.separator + hashID + File.separator;  
				    sliceDirFile = new File(directory);
					if (!sliceDirFile.isDirectory())
						sliceDirFile.mkdir();
				    fileVOI = new FileVOI(voiFileName, directory, imageCoronal);  
				    fileVOI.writeVOI(imageCoronal.getVOIs().VOIAt(0), true);
				    // imageCoronal.saveImage(directory, fileName, FileUtility.XML, false);
				    imageCoronal.saveImage(directory, fileName, FileUtility.XML, false, false, false);
				    
				    // 3) read sagittal orientation
			    	// get sagittal image 
			    	index = imgFile3.lastIndexOf(File.separator);
					fileName = imgFile3.substring(index + 1, imgFile3.length());
					directory = imgFile3.substring(0, index+1);
					keyImageIO = new FileIO();
					keyImageIO.setQuiet(true);
				    ModelImage imageSagittal = keyImageIO.readImage(fileName, directory);
					
				    // get sagittal VOI 
				    index = xmlFile3.lastIndexOf(File.separator);
					voiDirectory = new String(xmlFile3.substring(0, index + 1));
					voiFileName = new String(xmlFile3.substring(index + 1, xmlFile3.length()));
					fileVOI = null;
					fileVOI = new FileVOI(voiFileName, voiDirectory, imageSagittal);
				    imageSagittal.registerVOI(fileVOI.readVOI(false)[0]);
			    
				    smoothVOI(imageSagittal, imageSagittal, 45);
				    
				    
				    // save VOI and image, coronal
				    directory = saveDir + "sagittal" + File.separator + hashID + File.separator;  
				    sliceDirFile = new File(directory);
					if (!sliceDirFile.isDirectory())
						sliceDirFile.mkdir();
				    fileVOI = new FileVOI(voiFileName, directory, imageSagittal);  
				    fileVOI.writeVOI(imageSagittal.getVOIs().VOIAt(0), true);
				    // imageSagittal.saveImage(directory, fileName, FileUtility.XML, false);
				    imageSagittal.saveImage(directory, fileName, FileUtility.XML, false, false, false);
				    
				    // new ViewJFrameImage(imageAxial);
				    // new ViewJFrameImage(imageCoronal);
				    // new ViewJFrameImage(imageSagittal);
				  
				    
		    	} catch (Exception e) {
					e.printStackTrace();
				}
			    
		
	  }
	}
	
	public void smoothVOI(ModelImage maskImage, ModelImage resultImage, int numPoints) {

		
		VOIVector v = maskImage.getVOIs();
		int voiSize = v.size();
		if (v.size() == 0)
			return;

		
		for ( int i = 0; i < voiSize; i++ ) {
			v.VOIAt(i).setActive(true);
			v.VOIAt(i).setAllActive(true);
	
			// new ViewJFrameImage(maskImage);
			try {
				AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(i), numPoints, false);
				smoothAlgo.addListener(this);
				smoothAlgo.run();
	
				VOIVector resultVOIs = resultImage.getVOIs();
				VOI resultVOI = smoothAlgo.getResultVOI();
				resultVOIs.VOIAt(i).removeCurves();
				resultVOIs.VOIAt(i).setCurves(resultVOI.getCurves());
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
	}

	
	private void reconstructSurface() {

		Hashtable<String, Vector<String>> imageSet = imageNameHashtable.get("sagittal");
		Hashtable<String, Vector<String>>  voiSetAxial = voiNameHashtable.get("axial");
		Hashtable<String, Vector<String>>  voiSetCoronal = voiNameHashtable.get("coronal");
		Hashtable<String, Vector<String>>  voiSetSagittal = voiNameHashtable.get("sagittal");
		

		Set<String> hashIDKeys = voiSetAxial.keySet();
			
		for ( String hashID : hashIDKeys ) {
			
				Vector<String> imageNameVec = imageSet.get(hashID);
			
			    Vector<String> xmlSetAxial = voiSetAxial.get(hashID);
			    Vector<String> xmlSetCoronal = voiSetCoronal.get(hashID);
			    Vector<String> xmlSetSagittal = voiSetSagittal.get(hashID);
			    
			    String xmlFile1 = xmlSetAxial.get(0);
		    	String xmlFile2 = xmlSetCoronal.get(0);
		    	String xmlFile3 = xmlSetSagittal.get(0);
		    	
		    	System.err.println("xmlFile1 = " + xmlFile1);
		    	System.err.println("xmlFile2 = " + xmlFile2);
		    	System.err.println("xmlFile3 = " + xmlFile3);
		    	
		    	String imageFullName = imageNameVec.get(0);
				int index = imageFullName.lastIndexOf(File.separator);
				String fileName = imageFullName.substring(index + 1, imageFullName.length());
				String directory = imageFullName.substring(0, index);
		    	
		    	String plyFile = directory + File.separator + "input.ply";
		    	
		    	System.err.println("plyFile = " + plyFile);
		    	
		    	JDialogSaveMergedVOIs merge = new JDialogSaveMergedVOIs(parentFrame, xmlFile1, xmlFile2, xmlFile3, plyFile);
		    	merge.saveFile();
		    	/*
		    	JDialogSurfaceReconstruction rec = new JDialogSurfaceReconstruction(parentFrame, false);
		    	rec.setFileInput(directory, "input.ply");
		    	rec.setFileOutput(directory, "output.ply");
		    	rec.processAlgorithm();
		    	*/ 
		    	// break;
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