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

public class JDialogGenerateEndingSlices extends JDialogBase implements
		AlgorithmInterface {

	/** DOCUMENT ME! */
	private ModelImage image; // source image
	/** The main user interface. */
	private ViewUserInterface UI;

	private JPanel svmOptionsPanel;
	private JPanel trainGroupPanel;

	private JTextField  trainGroupTextfield;
	
	private JRadioButton radioButtonSVMBinary;
	private JRadioButton radioButtonSVMMulticlass;
	private ButtonGroup group;

	private int boxYmin, boxYmax;
	private int boxXmin, boxXmax;

	/** DOCUMENT ME! */
	private int[] xBounds = new int[2];

	/** DOCUMENT ME! */
	private int[] yBounds = new int[2];

	/** DOCUMENT ME! */
	private int[] zBounds = new int[2];

	/** DOCUMENT ME! */
	private AlgorithmAddMargins cropAlgo;

	/** DOCUMENT ME! */
	private ModelImage cropImage = null; // result image

	
	// key image directory
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	// kay images variables
	private JFileChooser keyImageChooser = new JFileChooser();
	private String keyImageName;
	private String keyImageDirectory;

	
	// axis region
	private JComboBox axisList;
	private JLabel labelAxis;

	private static int Axial = 0;
	private static int Saggital = 1;
	private static int Coronal = 2;
	private int axis = Axial;
	
	private Vector<String> keyImageVector = new Vector<String>();
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();

	private Vector<String> keyImageVOIVector = new Vector<String>();
	private Vector<VOI[]> keyImageVOIs = new Vector<VOI[]>();

	private Vector<ModelImage> cropKeyImages = new Vector<ModelImage>();
	
    public static final String modelDirName = "model";
    public static final String featureDirName = "featureCE";  // "featureCE"

    private Vector<Vector<String>> modelList = new Vector<Vector<String>>();  

    private JLabel labelSaveImage;
    private JTextField textFieldSaveImage;
    private JButton buttonSaveImage;
	
    private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageName;
	private String saveImageDirectory;
    
	public JDialogGenerateEndingSlices(Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		image = im;
		init();
		setVisible(true);

	}
	
	public void disposeLocal() {
		int i;
		for ( i = 0; i < keyImageVector.size(); i++ ) {
		 String temp = keyImageVector.get(i);
		 temp =null;
		}
		keyImageVector = null;
		
		
		for ( i = 0; i < keyImages.size(); i++ ) {
			 ModelImage temp = keyImages.get(i);
			 temp.disposeLocal();
		}
		keyImages = null;
			
		for ( i = 0; i < keyImageVOIVector.size(); i++ ) {
			 String temp = keyImageVOIVector.get(i);
			 temp =null;
		}
		keyImageVOIVector = null;
			
		
		for ( i = 0; i < keyImageVOIs.size(); i++ ) {
			 VOI[] temp = keyImageVOIs.get(i);
			 temp = null;
		}
		keyImageVOIs = null;
			
		for ( i = 0; i < cropKeyImages.size(); i++ ) {
			 ModelImage temp = cropKeyImages.get(i);
			 temp.disposeLocal();
		}
		cropKeyImages = null;
		
	}

	
	
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();
		String command = event.getActionCommand();
		if (command.equals("OK")) {
			callAlgorithm();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			// MipavUtil.showHelp("Haral1001");
		} else if (command.equals("ChooseKeyImageDir")) {	
			readKeyImageDir();
			// sortKeyImage();
		} else if (command.equals("ChooseSaveImageDir")) {
            recordSaveImageDir();
		}
	}

	
	private void recordSaveImageDir() {
		String saveImageName;
		saveImageChooser.setDialogTitle("Open Saved Images Directory");
		saveImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = saveImageChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			saveImageName = saveImageChooser.getSelectedFile().getName();

			saveImageDirectory = String.valueOf(saveImageChooser.getCurrentDirectory())
					+ File.separatorChar
					+ saveImageName
					+ File.separatorChar;
			textFieldSaveImage.setText(saveImageDirectory);
			

		} else {
			return;
		}
	}
			
		
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
			// UI.setDefaultDirectory(directory);
			textFieldKeyImage.setText(keyImageDirectory);

			File fileDir = new File(keyImageDirectory);
			// System.err.println("check = " + keyImageDirectory);
			traverse(fileDir);
		} else {
			return;
		}
		*/
		File fileDir_80 = new File("/scratch/ProstateNewData/");
		traverse_80(fileDir_80);
		File fileDir_100 = new File("/scratch/LFF100backup/");
		traverse_100(fileDir_100);
		File fileDir_50 = new File("/scratch/50_test_cases");
		traverse_100(fileDir_50);
	    File fileDir_60 = new File("/scratch/60_prostate_sets");
		traverse_100(fileDir_60);
	}
	
	private void traverse_80(File dir) {
		processDir_80(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_80(new File(dir, children[i]));
			}
		}

	}

	private void processDir_80(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			System.err.println(dir.toString());
			keyImageVector.add(dir.toString());

		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")) {
			System.err.println(dir.toString());
			keyImageVOIVector.add(dir.toString());
		}

	}

	private void traverse_100(File dir) {
		processDir_100(dir);

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_100(new File(dir, children[i]));
			}
		}

	}

	private void processDir_100(File dir) {
		String dirName = dir.toString();
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		
		String axisString = "";
		if ( axis == Axial ) {
			axisString = "ax";
		} else if ( axis == Saggital ) {
			axisString = "sag";
		} else if ( axis == Coronal ) {
			axisString = "cor";
		}
		
		if (dirName.substring(begin, end).startsWith("img")
				&& dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains(axisString)) {
			System.err.println(dir.toString());
			keyImageVector.add(dir.toString());

		}

		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains(axisString)) {
			System.err.println(dir.toString());
			keyImageVOIVector.add(dir.toString());
		}

	}

	
	public void sortKeyImage() {
		// keyImageVector;
		// keyImageVOIVector
		int i;
		int len = keyImageVector.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
	    int index;
		
		for ( i = 0; i < 288; i++) {
			imageName = keyImageVector.get(i);
			voiName = keyImageVOIVector.get(i);
			start = imageName.lastIndexOf("_");
			end = imageName.lastIndexOf(".");
			index = Integer.valueOf(imageName.substring(start+1, end));
			imageNameTable.put(index, imageName);
			imageVOITable.put(index, voiName);
		}
	
		keyImageVector.clear();
		keyImageVOIVector.clear();
		for ( i = 0; i < 288; i++ ) {
			imageName = imageNameTable.get(i+1);
			voiName = imageVOITable.get(i+1);
			if ( imageName != null ) {
				keyImageVector.add(imageName);
				keyImageVOIVector.add(voiName);
			}
		}
		
		// test for printing
		i = 0;
		for ( String entry : keyImageVector) {
			System.err.println(i + " = " + entry);
		    i++;
		}
		System.err.println("VOI:");
		i = 0;
		for ( String entry : keyImageVOIVector) {
			System.err.println(i + " = " + entry);
		    i++;
		}
	
	}

	private void traverse(File dir) {
		processDir(dir);

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse(new File(dir, children[i]));
			}
		}

	}

	private void processDir(File dir) {
		String dirName = dir.toString();
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
	
		String axisString = "";
		if (axis == Axial) {
			axisString = "ax";
		} else if (axis == Saggital) {
			axisString = "sag";
		} else if (axis == Coronal) {
			axisString = "cor";
		}
		if (dirName.substring(begin, end).startsWith("img")
				&& dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains(axisString)) {
			keyImageVector.add(dir.toString());
		}
		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains(axisString)) {
			keyImageVOIVector.add(dir.toString());
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
				try {
					new ViewJFrameImage(cropImage, null,
							new Dimension(610, 200));
				} catch (OutOfMemoryError error) {
					MipavUtil
							.displayError("Out of memory: unable to open new frame");
				}
               */
				// cropAlgo.finalize();
				// cropAlgo = null;

			}

		} else if (algorithm instanceof AlgorithmProstateFeaturesSaveAutoTrain) {
			// image.clearMask();

			
		}

		dispose();
		System.gc();
	}
	
	

	public void callAlgorithm() {
		long startTime = System.currentTimeMillis();
		
		loadFiles();
		
		cropKeyImages();
		
		saveImages();
		
		disposeLocal();
	   
	    
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");
		
		System.gc();
        
	}
	
	public void saveImages() {
		
		 for ( int i = 0; i < cropKeyImages.size(); i++ ) {
				try {
					ModelImage cropKeyImage = cropKeyImages.get(i); 
					
					int xDim = cropKeyImage.getExtents()[0];
					int yDim = cropKeyImage.getExtents()[1];
					int zDim = cropKeyImage.getExtents()[2];
					int size = xDim * yDim;
					
					int[] newExtents = new int[2];
					newExtents[0] = xDim;
					newExtents[1] = yDim;
					
					VOIVector targetImageVOI = cropKeyImage.getVOIs();
					// new ViewJFrameImage(cropKeyImage);
					
					int j = 12;
					int startSlice, endSlice;
                    int []x = new int[2];
                    int []y = new int[2];
                    int []z = new int[2];
                    
						try {
						
							System.err.println(" image number = " + i + "   slice number = " + j);
							
							ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
							float[] targetBuffer = new float[size];
							cropKeyImage.exportData(j * size, size, targetBuffer);
							targetImageSlice.importData(0, targetBuffer, true);							

							// find the intersection of the lower bound with the VOI.
							Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);
							targetImageVOI.VOIAt(0).getBounds(x, y, z);
							startSlice = z[0];
							endSlice = z[1];
							System.err.println("startSlice = " + startSlice + " endSlice = " + endSlice);
						
							if ( vArray[j].size() > 0 ) {
								
					            // **************************
						        // 1) save image
						         //  saveImageDirectory;
					            
						        String sliceDir = saveImageDirectory + "slice" + j +  File.separator;
						        File dir = new File(sliceDir);
								if ( !dir.isDirectory() ) {
								        dir.mkdir();
								}
						        String imgName = "image" + i + ".xml";
						        // String imageFileToSave = sliceDir + File.separator + imgName;
						       //  targetImageSlice.saveImage(directory, fileName, fileType, isActive, bDisplayProgress)
						        targetImageSlice.saveImage(sliceDir, imgName,  FileUtility.XML, false);
						        // 2) save VOI
						        // FileVOI fileVOI = new FileVOI("voi" + i + ".xml", sliceDir, targetImageSlice);
			                    // fileVOI.writeVOI(voiNew, true);
			                    // 3) save ending slices numbers
						        Formatter output = new Formatter(sliceDir + "end" + i + ".txt");
						        output.format("%d %d\n", startSlice, endSlice);
						        output.close();
			                    
							}
							
						
						} catch ( IOException e ) {
							
						}
				

					// cropKeyImagesCE.add(ceImageVector);
					// new ViewJFrameImage(coherenceEnhancingDiffusionImage);
					
								
				} catch (OutOfMemoryError x) {
		          MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
		      }
			 }
	}
	
		
	public void cropKeyImages() {

	
		int zDim;
	
		
		// Crop key images. VOIs
		for (int i = 0; i < keyImages.size();  i++) {

			
			ModelImage image = keyImages.get(i);

			int[] extents = (int[]) image.getFileInfo(0).getExtents();
			zDim = extents[2] - 1;

			// manually set the crop image starting point and ending point
			
			boxYmin = 124;
			boxYmax = 380-1;

			boxXmin = 124;
			boxXmax = 380-1;
           
			/*
			boxYmin = 0;
			boxYmax = 512-1;

			boxXmin = 0;
			boxXmax = 512-1;
			*/
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
				
				System.err.println("destExtents[0] = " + destExtents[0] + "  destExtents[1] = " +  destExtents[1]);

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

				System.err.println("xCrop[0] = " + xCrop[0] + "   xCrop[1] = " + xCrop[1]);
				System.err.println("yCrop[0] = " + yCrop[0] + "   yCrop[1] = " + yCrop[1]);
				
				cropAlgo = new AlgorithmAddMargins(image,
						cropKeyImages.get(i), xCrop, yCrop, zCrop);

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
			readFiles();
			// readManualModel();
			System.err.println("finish image I/O");

		}

		public void removeModelDir() {

			for ( int i = 0; i < keyImageVector.size(); i++ ) {
				
				String dir = keyImageVector.get(i);
				// System.err.println(dir);
				int index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1, dir.length()));
                System.err.println(directory + modelDirName);
				File f = new File(directory + modelDirName);
				if ( f.isDirectory() ) {
					deleteDir(f);
				}
				
				
			}
		}
		
		public static boolean deleteDir(File dir) {
		    if (dir.isDirectory()) {
		        String[] children = dir.list();
		        for (int i=0; i<children.length; i++) {
		            boolean success = deleteDir(new File(dir, children[i]));
		            if (!success) {
		                return false;
		            }
		        }
		    }

		    // The directory is now empty so delete it
		    return dir.delete();
		}
		
		public void readFiles() {
			FileIO targetImageIO = null;

			int start, end, index;

			try {
				// read key images and VOIs
				for (int i = 0; i < keyImageVector.size(); i++) {

					// read key image
					String dir = keyImageVector.get(i);
					index = dir.lastIndexOf(File.separator);
					String directory = new String(dir.substring(0, index + 1));
					String fileName = new String(dir.substring(index + 1, dir.length()));

					
				    System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
					FileIO keyImageIO = new FileIO();
					keyImages.add(i, keyImageIO.readImage(fileName, directory));
					
					// read corresponding VOI
					String voiDir = keyImageVOIVector.get(i);
					System.err.println("voiDir = " + voiDir);
					index = voiDir.lastIndexOf(File.separator);
					String voiDirectory = new String(voiDir.substring(0, index + 1));
					String voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));

					FileVOI fileVOI = null;
					fileVOI = new FileVOI(voiFileName, voiDirectory, keyImages.get(i));
					System.err.println("fileDirectory = " + directory + " fileName = " + fileName);
					System.err.println("voiDirectory = " + voiDirectory + "  voiFileName = " + voiFileName);
					keyImageVOIs.add(i, fileVOI.readVOI(false));

					keyImages.get(i).registerVOI(keyImageVOIs.get(i)[0]);
	               
					
					
				}
				 
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
			

	public void init() {

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

        buildKeyImagePanel();		
	
		mainPanel.add(imageSelectionPanel,  BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}
	
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

	public void buildTrainGroupPanel() {
		trainGroupPanel = new JPanel();
		trainGroupPanel.setBorder(buildTitledBorder("Training Groups"));
		trainGroupPanel.setLayout(new GridBagLayout());
		
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		
		JLabel trainGroupLabel = new JLabel("Groups :");
		trainGroupTextfield = new JTextField(20);
		// trainGroupTextfield.addActionListener(this);
		
		gbc.gridx = 0;
		gbc.gridy = 0;
	    trainGroupPanel.add(trainGroupLabel, gbc);
		gbc.gridx = 1;
		gbc.gridy = 0;
		trainGroupPanel.add(trainGroupTextfield, gbc);
		
	}
	
	public void buildSVMoptionsPanel() {
		svmOptionsPanel = new JPanel();
		svmOptionsPanel.setBorder(buildTitledBorder("SVM Options"));
		svmOptionsPanel.setLayout(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;

		radioButtonSVMBinary = new JRadioButton("SVM Binary Class", true);
		radioButtonSVMBinary.addActionListener(this);
		radioButtonSVMMulticlass = new JRadioButton("SVM Multi Class", false);
		radioButtonSVMMulticlass.addActionListener(this);

		group = new ButtonGroup();
		group.add(radioButtonSVMBinary);
		group.add(radioButtonSVMMulticlass);

		gbc.gridx = 0;
		gbc.gridy = 0;
		svmOptionsPanel.add(radioButtonSVMBinary, gbc);
		gbc.gridx = 1;
		gbc.gridy = 0;
		svmOptionsPanel.add(radioButtonSVMMulticlass, gbc);
	}

}