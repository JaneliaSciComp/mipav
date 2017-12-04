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


/**
 * 
 * The MICCAI ProstateX challenge is the first try on MRI image alone. 
 * We extract MRI image slice alone to train HED model.  In general, HED with MRI image alone
 * performs worse than the MRI+CED.   
 * 
 * ------------------------------------------------------------------
 * for backup only, will remove later
 * ------------------------------------------------------------------
 * 
 * steps:
 * 1) From MRI image, generates the binary mask images. 
 * 2) Normalize the MRI image slices.   
 * 3) save MRI slice with corresponding binary masks in png format. 
 * 
 * 
 */
public class JDialogProstateSPIEcancerChallenge_noCED extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;


	/** image vector to hold the actual images. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();

	private Vector<String> keyImageVector1 = new Vector<String>();
	private Vector<String> keyImageVOIVector1 = new Vector<String>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<String, Vector<String>> imageNameHashtable = new Hashtable<String, Vector<String>>(); 
	Hashtable<String, Vector<String>> voiNameHashtable = new Hashtable<String, Vector<String>>(); 
	
	Hashtable<Integer, ModelImage> imageHashtable = new Hashtable<Integer, ModelImage>();
	Hashtable<Integer, ModelImage> cedTable = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> cropTable = new Hashtable<Integer, ModelImage>(); 

	Hashtable<Integer, ModelImage> scaleImageTable = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> scaleIntensityTable = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> transformTable = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> scaleCedTable = new Hashtable<Integer, ModelImage>(); 
	Hashtable<Integer, ModelImage> scaleVoiTable = new Hashtable<Integer, ModelImage>();
		
	private AlgorithmTransform algoTrans;
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateSPIEcancerChallenge_noCED(Frame theParentFrame) {
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
		
		File fileDir_1 = new File("/scratch/backup/SPIEChallenge/Training/Sorted/");
		traverse_Layer(fileDir_1); 
		
	}
	
	private void traverse_Layer(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
		
			for (int i = 0; i < children.length; i++) {
				System.err.println(dir + File.separator+ children[i]);
				imageNameHashtable.put(children[i], new Vector<String>());
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
    		
    		if ( children[i].equals("t2_tse_tra") || children[i].contains("t2_tse_tra_")) {
    			traverse_scanLayer(new File(firstLayer, children[i]), hashID);
    		}
    		
    	}
    }
    
    private void traverse_scanLayer(File secondLayer, String hashID) {
    	String[] children = secondLayer.list();
    	int smallIndex = Integer.MAX_VALUE;
    	String smallIndexString = null;
    	for ( int i = 0; i < children.length; i++ ) {
    		int currentIndex = Integer.valueOf(children[i]);
    		if ( currentIndex < smallIndex) {
    			smallIndex = currentIndex;
    			smallIndexString = children[i];
    		}
    	}
    	traverse_DicomLayer(new File(secondLayer + File.separator + smallIndexString), hashID);
     
    }
    
    
    private void traverse_DicomLayer(File lastLayer, String hashID) {
    	String[] children = lastLayer.list();
    	for ( int i = 0; i < children.length; i++ ) {
    		imageNameHashtable.get(hashID).add(lastLayer + File.separator + children[i]);
    		System.err.println(lastLayer + File.separator + children[i]);
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
	
		runTransform();
		runCrop();
        runScaleIntensity();
        
        
		System.err.println("saveImage");
		
		crossValidationTrain();
		crossValidationTest();
	     
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}
	
	public void runCrop() {
		
		Set<Integer> keys = transformTable.keySet();
		for (Integer key : keys) {
			ModelImage keyImage = transformTable.get(key);
			ModelImage cropImage = cropImage(keyImage);
			cropTable.put(key, cropImage);
		}
		
	}
	
	private ModelImage cropImage(ModelImage image) {

		int[] destExtents = new int[3];
		int[] xBounds = new int[2];
		int[] yBounds = new int[2];
		int[] zBounds = new int[2];
		
		int[] extents = image.getExtents();
		int cropPixels = (int)Math.ceil(((double)extents[0] * 0.25d));
		
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
        
        return resultImage;
		
	}
	
	public void runCED() {
		
		Set<Integer> keys = imageHashtable.keySet();
		for (Integer key : keys) {
			 ModelImage keyImage = imageHashtable.get(key);
			ModelImage scaleIntenImage = scaleIntensity(keyImage);
			ModelImage cedImage = calculateCoherenceEnhancingDiffusion(scaleIntenImage);
			cedTable.put(key, cedImage);
		}
		
	}
	
	
	
	public void runTransform() {
		
		Set<Integer> keys = imageHashtable.keySet();
		
		for (int key : keys) {
			ModelImage keyImage = imageHashtable.get(key);
			ModelImage transImage = calculateTransform_resol_fix(keyImage);
			transformTable.put(key, transImage);	
		
		}
	
	}
	
	public ModelImage calculateTransform_dim_fixed(ModelImage image) {
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
        
        oXdim = 512;
        oYdim = 512;
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
	
	
	public void runScaleIntensity() {
		
		Set<Integer> keys = cropTable.keySet();
		
		for (int key : keys) {
		
			ModelImage keyImage = cropTable.get(key);
			ModelImage scaleIntenImage = scaleIntensity(keyImage);
			scaleIntensityTable.put(key, scaleIntenImage);
			
		}
	
	}
	
	public ModelImage histoEqual(ModelImage image) {
		ModelImage resultImage = (ModelImage) image.clone();
		   // Make algorithm
		int heightDivisions = 1;
		int widthDivisions = 1;
		
		AlgorithmAHE aheAlgo = new AlgorithmAHE(resultImage, image, heightDivisions, widthDivisions);
        aheAlgo.setRGBChannelFilter(false, false, false);                        
        aheAlgo.setContrastLimited(true);
        aheAlgo.setClipLevel(75);
        aheAlgo.run();
        
        return resultImage;

	}
	
	public ModelImage scaleIntensity(ModelImage image) {

		int[] destExtents = new int[3];
		
		destExtents[0] = image.getExtents()[0];
		destExtents[1] = image.getExtents()[1];
		destExtents[2] = image.getExtents()[2];
		
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
		

        float percentile_10 = 0.10f;
		float percentile_75 = 0.75f;
		float percentile_25 = 0.25f;
        float percentile_50 = 0.50f;
        float percentile_90 = 0.90f;
		
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
	
	public ModelImage calculateTransform_resol_fix(ModelImage image) {
		
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
        
        /*
        oXdim = 710;
        oYdim = 710;
        oZdim = (int)dims[2];
        
        oZres = resols[2];
        
        userValue = oXdim;
        factor = (userValue - constantFOV ) / ( dims[0] - constantFOV );
        fov = (dims[0] - constantFOV ) * resols[0];
        oXres = fov / ( userValue - constantFOV );
       
        dims[1] = ( dims[1] = constantFOV ) * factor + constantFOV;
        oYres = resols[1] / factor;
        */ 
        
        float iXdim = dims[0];
        float iYdim = dims[1];
        
        float iXres = resols[0];
        float iYres = resols[1];
        
        oXres = 0.3515625f;
        oYres = 0.3515625f;
        oZres =  resols[2];
        
        float fovX = iXres * (iXdim - constantFOV);
        float fovY = iYres * (iYdim - constantFOV);
        oXdim = Math.round(fovX / oXres) + constantFOV;
        oYdim = Math.round(fovY / oYres) + constantFOV;
        oZdim = (int)dims[2];
        
        System.err.println("oXdim = " + oXdim + " oYdim = " + oYdim);
        
		algoTrans = new AlgorithmTransform(image, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim, units,
                doVOI, doClip, doPad, doRotateCenter, center);
        algoTrans.setFillValue(fillValue);
        algoTrans.setUpdateOriginFlag(doUpdateOrigin);
        algoTrans.setUseScannerAnatomical(isSATransform);
        algoTrans.run();
        
        resultImage = algoTrans.getTransformedImage();
        resultImage.calcMinMax();
        
        // new ViewJFrameImage(resultImage);
        
        
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

		System.err.println("keyImageVector.size() = " + keyImageVector1.size());
		
		try {
			
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

		Set<Integer> keys = scaleIntensityTable.keySet();
		int dataSize = keys.size();
		
		System.err.println("table size = "+ dataSize);
		
		/*
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
		*/ 
		// Save Folder 1
		int[] index = new int[1];
		index[0] = 0;
        for(int key: keys){
        	saveImagesTest(key, index, "train"); 
        }
		
        /*
        int folder2count = 0;
        index = new int[1];
		index[0] = 0;
        for(int key: keys){
        	if ( folder2count >= folder2StartIndex && folder2count<= folder2EndIndex ) {
        		saveImagesTest(key, index, "fold2Test" + File.separator + "test"); 
        	}
        	folder2count++;
        }
		
        int folder3count = 0;
        index = new int[1];
		index[0] = 0;
        for(int key: keys){
        	if ( folder3count >= folder3StartIndex && folder3count<= folder3EndIndex ) {
        		saveImagesTest(key, index, "fold3Test" + File.separator + "test");
        	}
        	folder3count++;
        }
        
        int folder4count = 0;
        index = new int[1];
 		index[0] = 0;
	     for(int key: keys){
	     	if ( folder4count >= folder4StartIndex && folder4count<= folder4EndIndex ) {
	     		saveImagesTest(key, index, "fold4Test" + File.separator + "test"); 
	     	}
	     	folder4count++;
	     }
	    
	    int folder5count = 0;
	    index = new int[1];
 		index[0] = 0;
	     for(int key: keys){
	     	if ( folder5count >= folder5StartIndex && folder5count<= folder5EndIndex ) {
	     		saveImagesTest(key, index, "fold5Test" + File.separator + "test"); 
	     	}
	     	folder5count++;
	     }	
	     */ 
	}
	
	public void crossValidationTrain() {

		Set<Integer> keys = scaleImageTable.keySet();
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
        for(int key: keys){
        	
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
        for(int key: keys){
        	
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
        for(int key: keys){
        	
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
	    for(int key: keys){
	    	
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
	     for(int key: keys){
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
	
	
	public void saveImages(int key, int []index, String folderName, Hashtable<Integer, ModelImage> whichTable) {
		try {

			// if ( i == count ) continue;

			ModelImage keyImage = whichTable.get(key);
			// ModelImage cedImage = scaleCedTable.get(key);
			ModelImage keyImageVOI = scaleVoiTable.get(key);
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

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			x.printStackTrace();
		}
	}
	
	public void saveImagesTest(int key, int []index, String folderName) {
		
		try {

			// if ( i == count ) continue;

			ModelImage keyImage = scaleIntensityTable.get(key);
			
			// ModelImage keyImageVOI = scaleVoiTable.get(key);

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

			
			File dirFile = new File(saveImageDirectory + File.separator + folderName);
			if ( !dirFile.isDirectory() ) {
				dirFile.mkdir();
			}
			
			String sliceDir = saveImageDirectory + File.separator + folderName + File.separator
					+ key + File.separator;
			
			System.err.println("sliceDir = " + sliceDir);
			// String sliceDir = saveImageDirectory +
			// File.separator + i + File.separator;
			
			File sliceDirFile = new File(sliceDir);
			if (!sliceDirFile.isDirectory())
				sliceDirFile.mkdir();
			
			for (int j = 0; j < zDim; j++) {

				try {

					System.err.println(" image number = " + key + "   slice number = " + j);

					ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
					float[] targetBuffer = new float[size];
					keyImage.exportData(j * size, size, targetBuffer);
					targetImageSlice.importData(0, targetBuffer, true);
					
					String imgName = "image_" + j + ".png";
					
					// savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);
					savePNGfile(sliceDir, imgName, targetImageSlice, min, max, xDim, yDim, false);

					targetImageSlice.disposeLocal();
					targetImageSlice = null;
					
					/*
					ModelImage maskImage = new ModelImage(ModelStorageBase.SHORT, newExtents, "voi" + j);
					int[] voiBuffer = new int[size];
					keyImageVOI.exportData(j * size, size, voiBuffer);
					maskImage.importData(0, voiBuffer, true);
					
					String maskName = "voi_" + j + ".png";
					savePNGfile(sliceDir, maskName, maskImage, min, max, xDim, yDim, true);
					
					targetBuffer = null;
					targetImageSlice.disposeLocal();
					targetImageSlice = null;
					
					voiBuffer = null;
					maskImage.disposeLocal();
					maskImage = null;

					index[0]++;
                    */ 
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

		// System.err.println("keyImageVector.size() = " + keyImageVector.size());
		
		try {
			
			
			Set<String> keys = imageNameHashtable.keySet();
			int count = 0;
	        for(String key: keys){
	            Vector<String> imageDicomSet = imageNameHashtable.get(key);
	            
	            int lastIndex = key.indexOf("-");
                int len = key.length();
                String numberString = key.substring(lastIndex+1, len);
                int number = Integer.parseInt(numberString);
                System.err.println("number = " + number);
	            
	            FileIO fileIO = new FileIO();
	            String imageFullName = imageDicomSet.get(0);
	            int index = imageFullName.lastIndexOf(File.separator);
	            
                String fileName = imageFullName.substring(index+1, imageFullName.length());
                String directory = imageFullName.substring(0, index+1);
                
                boolean multiFile = true;
                ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
                // image.setImageName(key);
                
                
                imageHashtable.put(number, image);
                // new ViewJFrameImage(image);
                count++;
                /*
                Vector<String> voiSet = voiNameHashtable.get(key);
                VOI[] voi = null;
                for ( int k = 0; k < voiSet.size(); k++ ) {
	                String voiFullName = voiSet.get(k);
	                if ( voiFullName.contains("cg") || voiFullName.contains("TZ")) {
		                index = voiFullName.lastIndexOf(File.separator);
		                String voiName = voiFullName.substring(index+1, voiFullName.length());
		                String voiDirectory = voiFullName.substring(0, index+1);
		                // System.err.println("voiDirectory = " + voiDirectory);
		                // System.err.println("voiName = " + voiName);
		                FileVOI fileVOI = null;
						fileVOI = new FileVOI(voiName, voiDirectory, image);
						voi = fileVOI.readVOI(false);
			            
		                if ( voi[0] != null ) {
		                  image.registerVOI(voi[0]);  
		                  imageHashtable.put(key, image);	
		                  // voiHashtable.put(key, voi[0]);
			              // new ViewJFrameImage(image);
		                  break;
			              
		                } else {
		                	  System.err.println("filename = " + fileName);
		                      System.err.println("directory = " + directory);
		                      
		                	 System.err.println("voiDirectory = " + voiDirectory);
				             System.err.println("voiName = " + voiName);
		                }
			            
	                }
                }
                */ 
               // Thread.sleep(1000);
	            // pause();
	        }

	        System.err.println("count = " + count);
	        
	        
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