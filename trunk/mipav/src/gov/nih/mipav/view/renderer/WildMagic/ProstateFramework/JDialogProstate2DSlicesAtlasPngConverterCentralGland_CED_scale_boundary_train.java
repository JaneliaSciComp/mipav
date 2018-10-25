package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.file.FileIO;
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
 *
 * This class is the trial-and-error approach for HED deep learning experiment. 
 * This experiment applied to the boundary only, not the binary image masks. 
 * The first try is to use the MRI image and CED image together without intensity normalization. 
 * The data we used is the prostate NIH data (cg and wp).  Training case only.
 * 
 * I didn't get a chance to finish the experiment.  
 * 
 * Basic steps:
 * 1) generate boundary mask
 * 2) run CED against MRI images
 * 3) isotropic transform
 * NO intensity scale 
 * 4) extract png slices
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstate2DSlicesAtlasPngConverterCentralGland_CED_scale_boundary_train extends JDialogBase
		implements AlgorithmInterface {

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

	Hashtable<String, Vector<String>> imageNameHashtable = new Hashtable<String, Vector<String>>(); 
	Hashtable<String, Vector<String>> voiNameHashtable = new Hashtable<String, Vector<String>>(); 
	
	Hashtable<String, ModelImage> imageHashtable = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTable = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> voiTable = new Hashtable<String, ModelImage>();
	
	Hashtable<String, ModelImage> scaleImageTable = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> scaleCedTable = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> scaleVoiTable = new Hashtable<String, ModelImage>();
	
	Hashtable<String, VOI> voiHashtable = new Hashtable<String, VOI>(); 
	
	private AlgorithmTransform algoTrans;
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstate2DSlicesAtlasPngConverterCentralGland_CED_scale_boundary_train(Frame theParentFrame) {
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
		
		File fileDir_1 = new File("/scratch/backup/ProstateCg");
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
    		
    		if ( !decodedVOI.contains("manuel") && 
    				decodedVOI.startsWith("cg") && decodedVOI.endsWith("voi")) {
    			// System.err.println("voi: " + secondLayer + File.separator + children[i]);
    			voiNameHashtable.get(hashID).add(MipavUtil.decodeStr(voiString));
    			count++;
    		}
    		if ( !decodedVOI.contains("manuel") && 
    				decodedVOI.startsWith("wp") && decodedVOI.endsWith("voi")) {
    			// System.err.println("voi: " + secondLayer + File.separator + children[i]);
    			// voiNameHashtable.get(hashID).add(MipavUtil.decodeStr(voiString));
    			count++;
    		}
    		
    		if ( !decodedVOI.contains("manuel") && 
    				decodedVOI.startsWith("TZ") && decodedVOI.endsWith("voi")) {
    			System.err.println("voi: " + secondLayer + File.separator + children[i]);
    			voiNameHashtable.get(hashID).add(MipavUtil.decodeStr(voiString));
    			count++;
    		}

    	
    	}
    	
    	if ( count < 2 ) {
			// System.err.println("voi: " + secondLayer + File.separator);
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
	
		runMask();
        
		runCED();
        runTransform();
		
        
		System.err.println("saveImage");
		crossValidationTrain();
         
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}
	
	public void runMask() {
		
		Set<String> keys = imageHashtable.keySet();
		
		for ( String key : keys ) {
			
			ModelImage keyImage = imageHashtable.get(key);
			ModelImage maskImage = null;
			maskImage = keyImage.generateBinaryImage(false, false);
		
			// Erode algorithm
		    ModelImage erodeImage = (ModelImage)maskImage.clone();
		    erodeImage.setImageName("erode");
		    int kernelErode = AlgorithmMorphology25D.CONNECTED4;
		    float kernelSizeErode = 0.0f;
		    int iters = 2;
		    // new ViewJFrameImage(erodeImage);
		    AlgorithmMorphology25D erodeAlgo25D = new AlgorithmMorphology25D(erodeImage, kernelErode, kernelSizeErode,
                                                   AlgorithmMorphology25D.ERODE, 0, iters, 0, 0, true);
		
		    erodeAlgo25D.run();
		    // new ViewJFrameImage(erodeImage);
		    
		    // dilate algorithm
		    ModelImage dilateImage = (ModelImage)maskImage.clone();
            dilateImage.setImageName("dilate");
            float kernelSizeDilate = 0.0f;
            int kernelDilate = AlgorithmMorphology25D.CONNECTED4;
            // Make algorithm
            AlgorithmMorphology25D dilateAlgo25D = new AlgorithmMorphology25D(dilateImage, kernelDilate, kernelSizeDilate,
                                                       AlgorithmMorphology25D.DILATE, iters, 0, 0, 0,
                                                       true);
            dilateAlgo25D.run();
            // new ViewJFrameImage(dilateImage);
            // diff
            ModelImage diffImage = new ModelImage(maskImage.getType(), maskImage.getExtents(),
                    makeImageName(maskImage.getImageName(), "_calc"));

            // Make algorithm
            int  opType = AlgorithmImageCalculator.DIFFERENCE;
            
            AlgorithmImageCalculator mathAlgo = new AlgorithmImageCalculator(diffImage, dilateImage, erodeImage, opType, AlgorithmImageCalculator.CLIP, true,
                               null);
            mathAlgo.run();
            // new ViewJFrameImage(diffImage);
            
			voiTable.put(key, diffImage);
			
			// erodeImage = null;
			// dilateImage = null;
			
		}
		
	}

	public void runCED() {
		Set<String> keys = imageHashtable.keySet();
		for (String key : keys) {
			ModelImage keyImage = imageHashtable.get(key);
			cedTable.put(key, calculateCoherenceEnhancingDiffusion(keyImage));
		}
	}
	
	public void runTransform() {
		
		Set<String> keys = imageHashtable.keySet();
		
		
		for (String key : keys) {
		
			// System.err.println("transform key = " + key);
			
			ModelImage keyImage = imageHashtable.get(key);
            scaleImageTable.put(key, calculateTransform(keyImage));
            
            ModelImage cedImage = cedTable.get(key);
			scaleCedTable.put(key, calculateTransform(cedImage));
			
			ModelImage voiImage = voiTable.get(key);
			scaleVoiTable.put(key, calculateTransform(voiImage));
		
			
		}
	
	}
	
	public ModelImage calculateTransform(ModelImage image) {
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
		
		float[] dims = new float[3];
        float[] resols = new float[3];
        float factor, fov;
        float userValue;
        int constantFOV = 1;
        
        factor = 1.f;
        
        dims[0] = image.getFileInfo()[0].getExtents()[0];
        dims[1] = image.getFileInfo()[0].getExtents()[1];
        dims[2] = image.getFileInfo()[0].getExtents()[2];   
        resols[0] = image.getFileInfo()[0].getResolutions()[0];
        resols[1] = image.getFileInfo()[0].getResolutions()[1];
        resols[2] = image.getFileInfo()[0].getResolutions()[2];
		
        doVOI = false;
        doClip = true;
        doPad = false;
        doRotateCenter = false;
        center = null;
        
        fillValue = 0.0f;
        doUpdateOrigin = true;
        isSATransform = false;
        
        interp = 0;
        xfrm = new TransMatrix(3);
        xfrm.identity();
        
        units = new int[3];
        units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();
        
        oXdim = 256;
        oYdim = 256;
        oZdim = (int)dims[2];
        
        oZres = resols[2];
        
        userValue = oXdim;
        factor = (userValue - constantFOV ) / ( dims[0] - constantFOV );
        fov = (dims[0] - constantFOV ) * resols[0];
        oXres = fov / ( userValue - constantFOV );
       
        dims[1] = ( dims[1] = constantFOV ) * factor + constantFOV;
        oYres = resols[1] / factor;
        
        System.err.println("oXres = " + oXres + " oYres = " + oYres);
        
		algoTrans = new AlgorithmTransform(image, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim, units,
                doVOI, doClip, doPad, doRotateCenter, center);
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
	
	public void crossValidationTrain() {

		Set<String> keys = scaleImageTable.keySet();
		int dataSize = keys.size();
		
		System.err.println("table size = "+ dataSize);
		
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
	    
		// Save Folder 1
		int folder1count = 0;
		index[0] = 0;
        for(String key: keys){
        	
        	// saveImages(key, index, "fold1Test" + File.separator + "train", scaleImageTable); 
    		// saveImages(key, index, "fold1Test" + File.separator + "train", scaleCedTable); 
        	
    	
        	if ( folder1count<= folder1EndIndex ) {
        		// folder1count++;
        		// continue;
        	}
        	
        	if ( folder1count > folder1EndIndex ) {
        		saveImages(key, index, "fold1Test" + File.separator + "train", scaleImageTable); 
        		saveImages(key, index, "fold1Test" + File.separator + "train", scaleCedTable); 
        	}
            
    		
        	folder1count++;
        	System.err.println("folder1count = " + folder1count + " index = " + index[0]);
        }
	 
       
        int folder2count = 0;
        index = new int[1];
		index[0] = 0;
        for(String key: keys){
        	
        	if ( folder2count < folder2StartIndex ) {
        		saveImages(key, index, "fold2Test" + File.separator + "train", scaleImageTable); 
        		saveImages(key, index, "fold2Test" + File.separator + "train", scaleCedTable); 
        	}
        	
        	if ( folder2count >= folder2StartIndex && folder2count<= folder2EndIndex ) {
        	}
        	
        	if ( folder2count > folder2EndIndex ) {
        		saveImages(key, index, "fold2Test" + File.separator + "train", scaleImageTable); 
        		saveImages(key, index, "fold2Test" + File.separator + "train", scaleCedTable);
        	}
        	
        	folder2count++;
        }
		
        
        int folder3count = 0;
        index = new int[1];
		index[0] = 0;
        for(String key: keys){
        	
        	if ( folder3count < folder3StartIndex ) {
        		saveImages(key, index, "fold3Test" + File.separator + "train", scaleImageTable);
        		saveImages(key, index, "fold3Test" + File.separator + "train", scaleCedTable);
        	}
        	
        	if ( folder3count >= folder3StartIndex && folder3count<= folder3EndIndex ) {
        	}
        	
        	if ( folder3count > folder3EndIndex ) {
        		saveImages(key, index, "fold3Test" + File.separator + "train", scaleImageTable);
        		saveImages(key, index, "fold3Test" + File.separator + "train", scaleCedTable);
        	}
        	
        	folder3count++;
        }
        
        
        int folder4count = 0;
        index = new int[1];
 		index[0] = 0;
	    for(String key: keys){
	    	
	    	if ( folder4count < folder4StartIndex ) {
	    		saveImages(key, index, "fold4Test" + File.separator + "train", scaleImageTable); 
	    		saveImages(key, index, "fold4Test" + File.separator + "train", scaleCedTable); 
	    	}
	    	
	     	if ( folder4count >= folder4StartIndex && folder4count<= folder4EndIndex ) {
	     	}
	     	
	     	if ( folder4count > folder4EndIndex ) { 
	     		saveImages(key, index, "fold4Test" + File.separator + "train", scaleImageTable);
	     		saveImages(key, index, "fold4Test" + File.separator + "train", scaleCedTable);
	     	}
	     	folder4count++;
	     }
	     
	    
	    int folder5count = 0;
	    index = new int[1];
 		index[0] = 0;
	     for(String key: keys){
	     	if ( folder5count < folder5StartIndex ) {
	     		saveImages(key, index, "fold5Test" + File.separator + "train", scaleImageTable); 
	     		saveImages(key, index, "fold5Test" + File.separator + "train", scaleCedTable); 
	     		folder5count++;
	     	}
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

			OutputStream os = new FileOutputStream(file);

			PngWriter pngw = new PngWriter(os, imi);

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
    						ImageLineHelper.setPixelGray8(line, x, (int)255 );
						} else { 
							ImageLineHelper.setPixelGray8(line, x, (int)0 );
						}
					
                    	
						 
					}
				    
				}
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
	
	
	public void saveImages(String key, int []index, String folderName, Hashtable<String, ModelImage> whichTable) {
		try {

			ModelImage keyImage = whichTable.get(key);
			ModelImage keyImageVOI = scaleVoiTable.get(key);
			
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
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

						
						String maskName = "voi_" + index[0] + ".png";
						savePNGfile(sliceDir, maskName, maskImage, min, max, xDim, yDim, true);

						targetBuffer = null;
						targetImageSlice.disposeLocal();
						targetImageSlice = null;

						maskImage.disposeLocal();
						maskImage = null;

						
						index[0]++;

					
					 

				} catch (IOException e) {
					e.printStackTrace();
				}
			}

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			x.printStackTrace();
		}
	}
	
	public void saveImagesTest(String key, int []index, String folderName ) {
		try {

			ModelImage keyImage = scaleImageTable.get(key);
			ModelImage keyImageVOI = scaleVoiTable.get(key);

			int xDim = keyImage.getExtents()[0];
			int yDim = keyImage.getExtents()[1];
			int zDim = keyImage.getExtents()[2];

			int size_3D = xDim * yDim * zDim;
			float[] imageBuffer = new float[size_3D];

			try {
			  keyImage.exportData(0, imageBuffer.length, imageBuffer);
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

			VOIVector targetImageVOI = keyImage.getVOIs();

			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator
					+ key + File.separator;
			
			
			File sliceDirFile = new File(sliceDir);
			if (!sliceDirFile.isDirectory())
				sliceDirFile.mkdir();
			
			for (int j = 0; j <= zDim; j++) {

				try {

					System.err.println(" image number = " + key + "   slice number = " + j);

					ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
					float[] targetBuffer = new float[size];
					keyImage.exportData(j * size, size, targetBuffer);
					targetImageSlice.importData(0, targetBuffer, true);

					ModelImage maskImage = new ModelImage(ModelStorageBase.SHORT, newExtents, "voi" + j);
					int[] voiBuffer = new int[size];
					keyImageVOI.exportData(j * size, size, voiBuffer);
					maskImage.importData(0, voiBuffer, true);
					
					// find the intersection of the lower bound with the
					// VOI.
					Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);

					if (vArray[j].size() > 0) {
			
						System.err.println("index = " + index[0]);
						String imgName = "image_" + j + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);
						
						String maskName = "voi_" + j + ".png";
						savePNGfile(sliceDir, maskName, maskImage, min, max, true);

						targetBuffer = null;
						targetImageSlice.disposeLocal();
						targetImageSlice = null;

						maskImage.disposeLocal();
						maskImage = null;

						index[0]++;


					} else {

						System.err.println("index = " + index[0]);

						String imgName = "image_" + j + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);
						maskImage = new ModelImage(ModelStorageBase.SHORT, newExtents, "mask" + j);
						String maskName = "voi_" + j + ".png";
						savePNGfile(sliceDir, maskName, maskImage, min, max, true);

						targetBuffer = null;
						targetImageSlice.disposeLocal();
						targetImageSlice = null;

						maskImage.disposeLocal();
						maskImage = null;
						index[0]++;
					}

				} catch (IOException e) {

				}
			}


		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
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

			OutputStream os = new FileOutputStream(file);

			PngWriter pngw = new PngWriter(os, imi);


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
	 * load image files and voi files
	 */
	public void loadFiles() {
		readFile();
		System.err.println("finish image I/O");

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
                
                
                Vector<String> voiSet = voiNameHashtable.get(key);
                VOI[] voi = null;
                for ( int k = 0; k < voiSet.size(); k++ ) {
	                String voiFullName = voiSet.get(k);
	                if ( voiFullName.contains("cg") || voiFullName.contains("TZ")) {
		                index = voiFullName.lastIndexOf(File.separator);
		                String voiName = voiFullName.substring(index+1, voiFullName.length());
		                String voiDirectory = voiFullName.substring(0, index+1);
		               
		                FileVOI fileVOI = null;
						fileVOI = new FileVOI(voiName, voiDirectory, image);
						voi = fileVOI.readVOI(false);
			            
		                if ( voi[0] != null ) {
		                  image.registerVOI(voi[0]);  
		                  imageHashtable.put(key, image);	
		                  voiHashtable.put(key, voi[0]);
			              break;
			              
		                } else {
		                	  System.err.println("filename = " + fileName);
		                      System.err.println("directory = " + directory);
		                      
		                	 System.err.println("voiDirectory = " + voiDirectory);
				             System.err.println("voiName = " + voiName);
		                }
			            
	                }
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