package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
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
import ar.com.hjg.pngj.*;

import java.net.URLDecoder;

/**
 * This is the trial-and-error test, which applying the NIH data for training, Prostate promise 12 data for testing. 
 * Failed test.  
 * 
 *  -----------------------------------------------------------------------------------------
 *  For backup purpose only.  Will delete after SVN check-in. 
 *  -----------------------------------------------------------------------------------------
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesAtlasPngConverterCentralGland_miccai extends JDialogBase
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


	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** image vector to hold the actual images. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();

	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();

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

	private Hashtable<String, Vector<String>> imageNameHashtable = new Hashtable<String, Vector<String>>(); 
	private Hashtable<String, Vector<String>> voiNameHashtable = new Hashtable<String, Vector<String>>(); 
	
	private Hashtable<String, ModelImage> imageHashtable = new Hashtable<String, ModelImage>(); 
	private Hashtable<String, VOI> voiHashtable = new Hashtable<String, VOI>(); 
	
	private Hashtable<String, ModelImage> cedHashtable = new Hashtable<String, ModelImage>(); 
	
	
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesAtlasPngConverterCentralGland_miccai(Frame theParentFrame) {
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
	 
	}
	
	private void traverse_Layer(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
		    for (int i = 0; i < children.length; i++) {
				System.err.println(dir + File.separator+ children[i]);
				imageNameHashtable.put(children[i], new Vector<String>());
				voiNameHashtable.put(children[i], new Vector<String>());
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
    			String decodedPath = URLDecoder.decode(secondLayer + File.separator + children[i]);
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
    	}
    	
    }
    
    private void traverse_voiLayer(File secondLayer, String hashID) {
    	String[] children = secondLayer.list();
    	
    	int count = 0;
    	
    	for ( int i = 0; i < children.length; i++ ) {
    		
    		String voiString = secondLayer + File.separator + children[i];
    		int index = voiString.lastIndexOf(File.separator);
    		String voiName = voiString.substring(index+1, voiString.length());
    		
    		String decodedVOI = URLDecoder.decode(voiName);
    		
    		if ( !decodedVOI.contains("manuel") && !decodedVOI.contains("manual") && 
    				decodedVOI.startsWith("cg") && decodedVOI.endsWith("voi")) {
    			// System.err.println("voi: " + secondLayer + File.separator + children[i]);
    			voiNameHashtable.get(hashID).add(URLDecoder.decode(voiString));
    			count++;
    		}
    		if ( !decodedVOI.contains("manuel") && !decodedVOI.contains("manual") && 
    				decodedVOI.startsWith("wp") && decodedVOI.endsWith("voi")) {
    			// System.err.println("voi: " + secondLayer + File.separator + children[i]);
    			// voiNameHashtable.get(hashID).add(URLDecoder.decode(voiString));
    			count++;
    		}

    	
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

	}

	/**
	 * Driver function to read image and VOIs, and convert each 3D image to 2D
	 * slices.
	 */
	public void callAlgorithm() {
		
		long startTime = System.currentTimeMillis();

		loadFiles();

		runCED();
	
		addVOIsToCED();
		
		System.err.println("saveImage");
		
        train();
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

	}

	public void runCED() {
		Set<String> keys = imageHashtable.keySet();
		
		for(String key: keys){
	    	try {
	    		ModelImage keyImage = imageHashtable.get(key);
	    		cedHashtable.put(key, calculateCoherenceEnhancingDiffusion(keyImage));
			} catch ( OutOfMemoryError e ) {
				e.printStackTrace();
			}
	    }
		
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
		numIterations = 50;
		do25D = true;
		entireImage = true;
		ModelImage coherenceEnhancingDiffusionImage;

		try {
			
			coherenceEnhancingDiffusionImage = new ModelImage(ModelStorageBase.FLOAT, inImage.getExtents(), "target");
	
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
        // new ViewJFrameImage(coherenceEnhancingDiffusionImage);
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
	
	public void crossValidationTest() {


		Set<String> keys = imageHashtable.keySet();
		int dataSize = keys.size();
		
		int folder1StartIndex = 0;
		int folder1EndIndex = (int)(dataSize * 0.2f);
		
		int folder2StartIndex = folder1EndIndex + 1;
		int folder2EndIndex = (int)(dataSize * 0.4f);
		
		int folder3StartIndex = folder2EndIndex + 1;
		int folder3EndIndex = (int)(dataSize * 0.6f);
		
		int folder4StartIndex = folder3EndIndex + 1;
		int folder4EndIndex = (int)(dataSize * 0.8f);
		
		int folder5StartIndex = folder4EndIndex + 1;
		int folder5EndIndex = dataSize;
		
		int folder1count = 0;
		// Save Folder 1
		int[] index = new int[1];
		index[0] = 0;
        for(String key: keys){
        	if ( folder1count >= folder1StartIndex && folder1count<= folder1EndIndex ) {
        		saveImagesTest(key, index, "fold1Test" + File.separator + "test"); 
        	}
        	folder1count++;
        }
		
        int folder2count = 0;
        index = new int[1];
		index[0] = 0;
        for(String key: keys){
        	if ( folder2count >= folder2StartIndex && folder2count<= folder2EndIndex ) {
        		saveImagesTest(key, index, "fold2Test" + File.separator + "test"); 
        	}
        	folder2count++;
        }
		
        int folder3count = 0;
        index = new int[1];
		index[0] = 0;
        for(String key: keys){
        	if ( folder3count >= folder3StartIndex && folder3count<= folder3EndIndex ) {
        		saveImagesTest(key, index, "fold3Test" + File.separator + "test"); 
        	}
        	folder3count++;
        }
        
        int folder4count = 0;
        index = new int[1];
 		index[0] = 0;
	     for(String key: keys){
	     	if ( folder4count >= folder4StartIndex && folder4count<= folder4EndIndex ) {
	     		saveImagesTest(key, index, "fold4Test" + File.separator + "test"); 
	     	}
	     	folder4count++;
	     }
	    
	    int folder5count = 0;
	    index = new int[1];
 		index[0] = 0;
	     for(String key: keys){
	     	if ( folder5count >= folder5StartIndex && folder5count<= folder5EndIndex ) {
	     		saveImagesTest(key, index, "fold5Test" + File.separator + "test"); 
	     	}
	     	folder5count++;
	     }	
	}
	
	public void train() {

		Set<String> keys = imageHashtable.keySet();
		/*
		int dataSize = keys.size();
		
		int folder1StartIndex = 0;
		int folder1EndIndex = (int)(dataSize * 0.2f);
		System.err.println("folder1StartIndex = " + folder1StartIndex + "   folder1EndIndex = " + folder1EndIndex);
		
		
		int folder2StartIndex = folder1EndIndex + 1;
		int folder2EndIndex = (int)(dataSize * 0.4f);
		System.err.println("folder2StartIndex = " + folder2StartIndex + " folder2EndIndex = " + folder2EndIndex);
		
		int folder3StartIndex = folder2EndIndex + 1;
		int folder3EndIndex = (int)(dataSize * 0.6f);
		System.err.println("folder3StartIndex = " + folder3StartIndex + " folder3EndIndex = " + folder3EndIndex);
		
		int folder4StartIndex = folder3EndIndex + 1;
		int folder4EndIndex = (int)(dataSize * 0.8f);
		System.err.println("folder4StartIndex = " + folder4StartIndex + " folder4EndIndex = " + folder4EndIndex);
		
		
		int folder5StartIndex = folder4EndIndex + 1;
		int folder5EndIndex = dataSize;
		System.err.println("folder5StartIndex = " + folder5StartIndex + " folder5EndIndex = " + folder5EndIndex);
		
		
		int[] index = new int[1];
	    */ 
		int[] index = new int[1];
		for(String key: keys){
	        saveImages(key, index, "train"); 	
	    }
		
		for(String key: keys){
	        saveImagesCED(key, index, "train"); 	
	    }
		
		/*
		// Save Folder 1
		int folder1count = 0;
		index[0] = 0;
        for(String key: keys){
        	
        	if ( folder1count<= folder1EndIndex ) {
        	}
        	
        	if ( folder1count > folder1EndIndex ) {
        		saveImages(key, index, "fold1Test" + File.separator + "train"); 
        	}
        	
        	folder1count++;
        }
	 
        
        int folder2count = 0;
        index = new int[1];
		index[0] = 0;
        for(String key: keys){
        	
        	if ( folder2count < folder2StartIndex ) {
        		saveImages(key, index, "fold2Test" + File.separator + "train"); 
        	}
        	
        	if ( folder2count >= folder2StartIndex && folder2count<= folder2EndIndex ) {
        	}
        	
        	if ( folder2count > folder2EndIndex ) {
        		saveImages(key, index, "fold2Test" + File.separator + "train"); 
        	}
        	
        	folder2count++;
        }
		
        
        int folder3count = 0;
        index = new int[1];
		index[0] = 0;
        for(String key: keys){
        	
        	if ( folder3count < folder3StartIndex ) {
        		saveImages(key, index, "fold3Test" + File.separator + "train"); 
        	}
        	
        	if ( folder3count >= folder3StartIndex && folder3count<= folder3EndIndex ) {
        	}
        	
        	if ( folder3count > folder3EndIndex ) {
        		saveImages(key, index, "fold3Test" + File.separator + "train"); 
        	}
        	
        	folder3count++;
        }
        
        
        int folder4count = 0;
        index = new int[1];
 		index[0] = 0;
	    for(String key: keys){
	    	
	    	if ( folder4count < folder4StartIndex ) {
	    		saveImages(key, index, "fold4Test" + File.separator + "train"); 
	    	}
	    	
	     	if ( folder4count >= folder4StartIndex && folder4count<= folder4EndIndex ) {
	     	}
	     	
	     	if ( folder4count > folder4EndIndex ) { 
	     		saveImages(key, index, "fold4Test" + File.separator + "train"); 
	     	}
	     	folder4count++;
	     }
	     
	    
	    int folder5count = 0;
	    index = new int[1];
 		index[0] = 0;
	     for(String key: keys){
	     	if ( folder5count < folder5StartIndex ) {
	     		saveImages(key, index, "fold5Test" + File.separator + "train"); 
	     		folder5count++;
	     	}
	     }
        
	    */ 

	}
	
	
	public void saveImages(String key, int []index, String folderName ) {
		try {

			// if ( i == count ) continue;

			ModelImage cropKeyImage = imageHashtable.get(key);

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
			
			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator;
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

						String imgName = "image_" + index[0] + ".png";
						// String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						
						// scale to 256x256
						ModelImage imageScaled = calculateTransform(targetImageSlice, 256, 256);
						min = (int)imageScaled.getMin();
						max = (int)imageScaled.getMax();
						savePNGfile(sliceDir, imgName, imageScaled, min, max, 256, 256, false);

						ModelImage maskImage = null;
						maskImage = targetImageSlice.generateBinaryImage(false, false);
						String maskName = "voi_" + index[0] + ".png";
						// String maskName = "voi_" + j + ".png";
						// maskImage.saveImage(sliceDir, maskName,
						// FileUtility.JIMI, false);
						// new ViewJFrameImage(maskImage);
						// if ( true ) break;
						
						// scale to 256x256
						ModelImage imageMaskScaled = calculateTransform(maskImage, 256, 256);
						min = (int)imageMaskScaled.getMin();
						max = (int)imageMaskScaled.getMax();
						savePNGfile(sliceDir, maskName, imageMaskScaled, min, max, 256, 256, true);

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
						
						// scale to 256x256
						ModelImage imageScaled = calculateTransform(targetImageSlice, 256, 256);
						min = (int)imageScaled.getMin();
						max = (int)imageScaled.getMax();
						savePNGfile(sliceDir, imgName, imageScaled, min, max, 256, 256, false);

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
						
						ModelImage imageMaskScaled = calculateTransform(maskImage, 256, 256);
						min = (int)imageMaskScaled.getMin();
						max = (int)imageMaskScaled.getMax();
						savePNGfile(sliceDir, maskName, imageMaskScaled, min, max, 256, 256, true);

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

		    

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
		}
	}
	
	public void saveImagesCED(String key, int []index, String folderName ) {
		try {

			ModelImage cropKeyImage = cedHashtable.get(key);

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
			
			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator;
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

						String imgName = "image_" + index[0] + ".png";
						// String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						
						// scale to 256x256
						ModelImage imageScaled = calculateTransform(targetImageSlice, 256, 256);
						min = (int)imageScaled.getMin();
						max = (int)imageScaled.getMax();
						savePNGfile(sliceDir, imgName, imageScaled, min, max, 256, 256, false);
						// savePNGfile(sliceDir, imgName, imageScaled, min, max, false);

						ModelImage maskImage = null;
						maskImage = targetImageSlice.generateBinaryImage(false, false);
						String maskName = "voi_" + index[0] + ".png";
						// String maskName = "voi_" + j + ".png";
						// maskImage.saveImage(sliceDir, maskName,
						// FileUtility.JIMI, false);
						// new ViewJFrameImage(maskImage);
						// if ( true ) break;
						ModelImage imageMaskScaled = calculateTransform(maskImage, 256, 256);
						min = (int)imageMaskScaled.getMin();
						max = (int)imageMaskScaled.getMax();
						savePNGfile(sliceDir, maskName, imageMaskScaled, min, max, 256, 256, true);
						// savePNGfile(sliceDir, maskName, maskImage, min, max, true);

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
						
						// scale to 256x256
						ModelImage imageScaled = calculateTransform(targetImageSlice, 256, 256);
						min = (int)imageScaled.getMin();
						max = (int)imageScaled.getMax();
						savePNGfile(sliceDir, imgName, imageScaled, min, max,  256, 256, false);
						// savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);

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
						ModelImage imageMaskScaled = calculateTransform(maskImage, 256, 256);
						min = (int)imageMaskScaled.getMin();
						max = (int)imageMaskScaled.getMax();
						savePNGfile(sliceDir, maskName, imageMaskScaled, min, max,  256, 256, true);
						// savePNGfile(sliceDir, maskName, maskImage, min, max, true);

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

		    

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
		}
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
	
	public void saveImagesTest(String key, int []index, String folderName ) {
		try {

			// if ( i == count ) continue;

			ModelImage cropKeyImage = imageHashtable.get(key);

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
			
			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator
					+ key + File.separator;
			
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
			
			
			Set<String> keys = imageNameHashtable.keySet();
	        for(String key: keys){
	            Vector<String> imageDicomSet = imageNameHashtable.get(key);
	            
	            FileIO fileIO = new FileIO();
	            String imageFullName = imageDicomSet.get(0);
	            int index = imageFullName.lastIndexOf(File.separator);
	            
                String fileName = imageFullName.substring(index+1, imageFullName.length());
                String directory = imageFullName.substring(0, index+1);
               
                
                boolean multiFile = true;
                ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
                image.setImageName(key);
                imageHashtable.put(key, image);
                
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
			            image.registerVOI(voi[0]);
	                }
                }
                voiHashtable.put(key, voi[0]);
	           
	        }

	        
	        
	        
		
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void addVOIsToCED() {

		try {
			Set<String> keys = cedHashtable.keySet();
	        for(String key: keys){
	            ModelImage image = cedHashtable.get(key);
	            VOI voi = voiHashtable.get(key);
                image.registerVOI(voi);
	        }
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	
	public static void pause() {
		System.err.println("enter to continue: ");
		try {
			for ( int av = System.in.available(); av > 0; av-- ) {
				System.in.read();
			}
			System.in.read();	
		} catch ( IOException e ) {
			System.err.println("keyboard failed: " + e );
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