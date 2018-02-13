package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;

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
 * The MICCAI ProstateX challenge is rolling over to prostate segmentation, which includes the 
 * whole prostate(wp) and central gland(cg). This is the first attempt to try the wp and cg segmentation. 
 * Combining the CED and MRI slices to train one HED deep learning model.  This version is the one helps 
 * us to win MICCAI ProstateX 2017 challenge for the third place.   I implemented this version a week before
 * the challenge submission deadline.  The training date includes NIH data + ProstateX training data. 
 * 
 * For the challenge, we apply this class to central gland segmentation for the challenge testing data. 
 * 
 * steps:
 * 1) From MRI image, generates the binary mask images. 
 * 2) Normalize the MRI image slices within range [0, 1000].
 * 3) Generate the CED slices
 * 3) save MRI and CED slices with corresponding binary masks in png format. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateSPIEcancerChallengeNIH_train_ced_ext extends JDialogBase
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
	Hashtable<String, Vector<String>> voiNameHashtableWp = new Hashtable<String, Vector<String>>(); 
	Hashtable<String, Vector<String>> voiNameHashtableCg = new Hashtable<String, Vector<String>>(); 
	
	Hashtable<String, Vector<String>> imageNameHashtable_ext = new Hashtable<String, Vector<String>>(); 
	Hashtable<String, Vector<String>> voiNameHashtableWp_ext = new Hashtable<String, Vector<String>>(); 
	Hashtable<String, Vector<String>> voiNameHashtableCg_ext = new Hashtable<String, Vector<String>>(); 
	
	Hashtable<String, ModelImage> imageHashtableWp_ext = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> imageHashtableCg_ext = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTable_ext = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> voiTable_ext = new Hashtable<String, ModelImage>();
	
	Hashtable<String, ModelImage> imageHashtableWp = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> imageHashtableCg = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cedTable = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> voiTable = new Hashtable<String, ModelImage>();
	
	Hashtable<String, ModelImage> scaleImageTable = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> scaleCedTable = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> scaleVoiTable = new Hashtable<String, ModelImage>();
	
	Hashtable<String, ModelImage> scaleImageTable_ext = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> scaleIntensityTable_ext = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> scaleCedTable_ext = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> scaleVoiTable_ext = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> transformTable_ext = new Hashtable<String, ModelImage>(); 
	
	
	Hashtable<String, VOI> voiHashtable = new Hashtable<String, VOI>(); 
	
	Hashtable<String, ModelImage> cropTable_ext = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cropVoiTable_ext = new Hashtable<String, ModelImage>();
	
	
	private AlgorithmTransform algoTrans;
	
	Hashtable<String, ModelImage> scaleIntensityTable = new Hashtable<String, ModelImage>(); 
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateSPIEcancerChallengeNIH_train_ced_ext(Frame theParentFrame) {
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
				
		File fileDir_2 = new File("/scratch/backup/SPIEChallenge/ProcessedNoERC/SortedNormalized/");
		traverse_Layer_noCoil(fileDir_2); 
		
		File fileDir = new File("/scratch/backup/SPIEChallenge/NetherlandTrainDataPng/cg_ced_150_voi_modified/");
		traverse_layer_modified(fileDir);
		
	}
	
	private void traverse_layer_modified(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			
		    for (int i = 0; i < children.length; i++) {
		    	
		    	if ( imageNameHashtable_ext.get(children[i]) == null ) {
		    		imageNameHashtable_ext.put(children[i], new Vector<String>());
		    		voiNameHashtableWp_ext.put(children[i], new Vector<String>());
		    		voiNameHashtableCg_ext.put(children[i], new Vector<String>());
		    	}
		    	File firstLayer = new File(dir, children[i]);
		    	traverse_secondLayer_noCoil_ext(firstLayer, children[i]);
				
			}
		}
		
	}
	
	private void traverse_Layer_noCoil(File dir) {
		
		if (dir.isDirectory()) {
			
			String[] children = dir.list();
			
			for (int i = 0; i < children.length; i++) {
		    	
		    	if ( imageNameHashtable.get(children[i]) == null ) {
		    		imageNameHashtable.put(children[i], new Vector<String>());
		    		voiNameHashtableWp.put(children[i], new Vector<String>());
		    		voiNameHashtableCg.put(children[i], new Vector<String>());
		    	}
				traverse_firstLayer_noCoil(dir, children[i]);
			}
		}
	}
	
	private void traverse_firstLayer_noCoil(File firstDir, String child) {
	    File firstLayer = new File(firstDir, child);
	    traverse_secondLayer_noCoil(firstLayer, child);
	}
 
	private void traverse_secondLayer_noCoil_ext(File firstLayer, String hashID) {
    	
	    String[] children = firstLayer.list();
    	
    	for ( int i = 0; i < children.length; i++ ) {
    		
    		System.err.println(firstLayer + File.separator+ children[i]);
    		if ( children[i].startsWith("image") && children[i].endsWith(".xml")) {
    			String imgString = firstLayer + File.separator+ children[i];
    			imageNameHashtable_ext.get(hashID).add(URLDecoder.decode(imgString));
    		}
    		
    		if ( children[i].startsWith("voi") && children[i].endsWith(".voi")) {
    			String voiString = firstLayer + File.separator+ children[i];
    			voiNameHashtableWp_ext.get(hashID).add(URLDecoder.decode(voiString));
    		}
    		
    	}
}

	
	private void traverse_secondLayer_noCoil(File firstLayer, String hashID) {
	    	
		    String[] children = firstLayer.list();
	    	
	    	for ( int i = 0; i < children.length; i++ ) {
	    		
	    		if ( children[i].equals("wp.voi")) {
	    			String voiString = firstLayer + File.separator+ children[i];
	    			voiNameHashtableWp.get(hashID).add(URLDecoder.decode(voiString));
	    		}
	    		
	    		if ( children[i].equals("cg.voi")) {
	    			String voiString = firstLayer + File.separator+ children[i];
	    			voiNameHashtableCg.get(hashID).add(URLDecoder.decode(voiString));
	    		}
	    		
	    		if ( children[i].equals("T2WI")) {
	    			File lastLayer = new File(firstLayer + File.separator+ children[i]);
	    			traverse_DicomLayer(lastLayer, hashID);
	    		
	    		}
	    		
	    	}
    }
	
	
    
    private void traverse_DicomLayer(File lastLayer, String hashID) {
    	String[] children = lastLayer.list();
    	System.err.println("hashID = " + hashID);
    	for ( int i = 0; i < children.length; i++ ) {
    		imageNameHashtable.get(hashID).add(lastLayer + File.separator + children[i]);
    		System.err.println(lastLayer + File.separator + children[i]);
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
		
	    // ProstateX data 
		runMask_ext();
		runTransform_ext();
		runCrop_ext();
		runScaleIntensity_ext();
		runCED_ext();
	
		// NIH data
		runMask();
		runScaleIntensity();
        runCED();
	  
        disposeMemory();
        System.gc();
		System.err.println("saveImage");
		
        train();
         
         
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		

	}
	
	
	public void disposeMemory() {
		Set<String> keys = imageHashtableWp_ext.keySet();
		for (String key : keys) {
			
			ModelImage wpImage = imageHashtableWp_ext.get(key);
			wpImage.disposeLocal();
			wpImage = null;
			
			ModelImage trImage = transformTable_ext.get(key);
			trImage.disposeLocal();
			trImage = null;
			
			
			ModelImage voiImage = voiTable_ext.get(key);
			voiImage.disposeLocal();
			voiImage = null;
			
		}
		imageHashtableWp_ext = null;
		transformTable_ext = null;
		voiTable_ext = null;
		
		
		Set<String> keysOrig = imageHashtableCg.keySet();
		for (String keyOrig : keysOrig ) {
			ModelImage wpImage = imageHashtableCg.get(keyOrig);
			// wpImage.disposeLocal();
			wpImage = null;
		}
		
		imageHashtableCg = null;
	 
	}
	
	public void runTransform_ext() {
		
		Set<String> keys = imageHashtableWp_ext.keySet();
		
		for (String key : keys) {
			ModelImage keyImage = imageHashtableWp_ext.get(key);
			ModelImage transImage = calculateTransform_resol_fix(keyImage);
			transformTable_ext.put(key, transImage);	
			
			ModelImage maskImage = voiTable_ext.get(key);
			ModelImage transMaskImage = calculateTransform_resol_fix(maskImage);
			scaleVoiTable_ext.put(key, transMaskImage);
	
		}
	
	}
	
	public void runCrop_ext() {
		
		Set<String> keys = transformTable_ext.keySet();
		for (String key : keys) {
			ModelImage keyImage = transformTable_ext.get(key);
			ModelImage cropImage = cropImage(keyImage);
			cropTable_ext.put(key, cropImage);
			
			ModelImage voiImage = scaleVoiTable_ext.get(key);
			ModelImage cropVoiImage = cropImage_ext(voiImage);
			cropVoiTable_ext.put(key, cropVoiImage);
			
		}
		
	}
	
	public void runScaleIntensity_ext() {
		
		Set<String> keys = cropTable_ext.keySet();
		
		for (String key : keys) {
			ModelImage keyImage = cropTable_ext.get(key);
			ModelImage scaleIntenImage = scaleIntensity(keyImage);
			scaleIntensityTable_ext.put(key, scaleIntenImage);
				
		}
	
	}
	
	public void runCED_ext() {
		
		Set<String> keys = scaleIntensityTable_ext.keySet();
		for (String key : keys) {
			 ModelImage keyImage = scaleIntensityTable_ext.get(key);
			ModelImage cedImage = calculateCoherenceEnhancingDiffusion(keyImage);
			cedTable_ext.put(key, cedImage);
		}
	}
	
	public void runScaleIntensity() {


		Set<String> keys = imageHashtableCg.keySet();

		for (String key : keys) {

			ModelImage keyImage = imageHashtableCg.get(key);
			scaleIntensityTable.put(key, keyImage);
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
		

        float percentile_10 = 0.05f;
		float percentile_75 = 0.75f;
		float percentile_25 = 0.25f;
        float percentile_50 = 0.50f;
        float percentile_90 = 0.95f;
		
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
		
	public void runMask() {
	
		Set<String> keys = imageHashtableCg.keySet();
	
		for (String key : keys) {

			ModelImage cgImage = imageHashtableCg.get(key);
			ModelImage cgMaskImage = null;
			cgMaskImage = cgImage.generateBinaryImage(false, false);
            voiTable.put(key, cgMaskImage);

		}
	
	}

	public void runMask_ext() {
		
		Set<String> keys = imageHashtableWp_ext.keySet();
		
		for (String key : keys) {

			ModelImage wpImage = imageHashtableWp_ext.get(key);
			ModelImage wpMaskImage = null;
			wpMaskImage = wpImage.generateBinaryImage(false, false);
			voiTable_ext.put(key, wpMaskImage);

		}

	}

	
	public void runCED() {

		Set<String> keys = scaleIntensityTable.keySet();
		for (String key : keys) {
			ModelImage keyImage = scaleIntensityTable.get(key);
			ModelImage cedImage = calculateCoherenceEnhancingDiffusion(keyImage);
			cedTable.put(key, cedImage);
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
		numIterations = 20;
		do25D = true;
		entireImage = true;
		ModelImage coherenceEnhancingDiffusionImage;

		try {
			coherenceEnhancingDiffusionImage = (ModelImage) inImage.clone();
			int type = coherenceEnhancingDiffusionImage.getType();
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


	
	
	public void train() {

		Set<String> keys = scaleIntensityTable.keySet();
		int dataSize = keys.size();
		
		System.err.println("table size = "+ dataSize);
		
		int[] index = new int[1];
	
		index[0] = 0;
        for(String key: keys){
        	saveImages(key, index, "train", scaleIntensityTable); 
        	saveImages(key, index, "train", cedTable); 
        }
	 
		

	    keys = scaleIntensityTable_ext.keySet();
		
        for(String key: keys){
        	System.err.println("ruida key = " + key );
        	saveImages_ext(key, index, "train", scaleIntensityTable_ext); 
        	saveImages_ext(key, index, "train", cedTable_ext); 
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
			

		} catch (Exception e) {
			System.err.println("image find wrong : " + file.getAbsolutePath());
			e.printStackTrace();

			System.exit(0);
		}
	}
	
	
	public void saveImages(String key, int []index, String folderName, Hashtable<String, ModelImage> whichTable) {
		try {

			
			ModelImage keyImage = whichTable.get(key);
			
			ModelImage keyImageVOI = voiTable.get(key);
			
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

			
			imageBuffer = null;
			

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			x.printStackTrace();
		}
	}
	
	public void saveImages_ext(String key, int []index, String folderName, Hashtable<String, ModelImage> whichTable) {
		try {

			ModelImage keyImage = whichTable.get(key);
			ModelImage keyImageVOI = cropVoiTable_ext.get(key);
			
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

			imageBuffer = null;
		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			x.printStackTrace();
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
	        	
	        	System.err.println("key = " + key);
	        	
	            Vector<String> imageDicomSet = imageNameHashtable.get(key);
	            
	            FileIO fileIO = new FileIO();
	            String imageFullName = imageDicomSet.get(0);
	            int index = imageFullName.lastIndexOf(File.separator);
	            
                String fileName = imageFullName.substring(index+1, imageFullName.length());
                String directory = imageFullName.substring(0, index+1);
                
                boolean multiFile = true;
                
                // ModelImage imageWp = fileIO.readImage(fileName, directory, multiFile, null);
                // imageWp.setImageName(key);
                
                ModelImage imageCg = fileIO.readImage(fileName, directory, multiFile, null);
                imageCg.setImageName(key);
                
                // Vector<String> voiSetWp = voiNameHashtableWp.get(key);
                Vector<String> voiSetCg = voiNameHashtableCg.get(key);
                // int sizeWp = voiSetWp.size();
                int sizeCg = voiSetCg.size();
                // int sizeMin = Math.min(sizeWp, sizeCg);
                int sizeMin = sizeCg; 
                
                for ( int k = 0; k < sizeMin; k++ ) {
                	
	                // String voiFullNameWp = voiSetWp.get(k);
	                String voiFullNameCg = voiSetCg.get(k);
	                
	                // if ( voiFullName.contains("cg") || voiFullName.contains("TZ")) {
	                if ( /* voiFullNameWp.contains("wp") && */  voiFullNameCg.contains("cg")   ) {
	                // if ( voiFullNameWp.contains("wp") && voiFullNameCg.contains("cg") ) {
	                	/*
		                index = voiFullNameWp.lastIndexOf(File.separator);
		                String voiNameWp = voiFullNameWp.substring(index+1, voiFullNameWp.length());
		                String voiDirectoryWp = voiFullNameWp.substring(0, index+1);
		                
		                FileVOI fileVOIwp = null;
					    fileVOIwp = new FileVOI(voiNameWp, voiDirectoryWp, imageWp);
					    VOI[] voiWp = null;
						voiWp = fileVOIwp.readVOI(false);
		                */   
					   
						index = voiFullNameCg.lastIndexOf(File.separator);
		                String voiNameCg = voiFullNameCg.substring(index+1, voiFullNameCg.length());
		                String voiDirectoryCg = voiFullNameCg.substring(0, index+1);
						
						FileVOI fileVOICg = null;
						fileVOICg = new FileVOI(voiNameCg, voiDirectoryCg, imageCg);
						VOI[] voiCg = null;
						voiCg = fileVOICg.readVOI(false);
			             
						
					    /*
		                if ( voiWp[0] != null ) {
		                  imageWp.registerVOI(voiWp[0]);  
		                  imageHashtableWp.put(key, imageWp);	
		                  // new ViewJFrameImage(imageWp);
		                } 
		                */   
			            
		               
		                if ( voiCg[0] != null ) {
		                  imageCg.registerVOI(voiCg[0]);  
		                  imageHashtableCg.put(key, imageCg);
		                  // new ViewJFrameImage(imageCg);
			            }
			            
			            
		                
	                }
                }
               // Thread.sleep(1000);
	            // pause();
	        }

			
			// read modified table
			keys = imageNameHashtable_ext.keySet();
	        
			for(String key: keys){
	        	
	        	System.err.println("key = " + key);
	        	
	            Vector<String> imageDicomSet = imageNameHashtable_ext.get(key);
	            
	            FileIO fileIO = new FileIO();
	            String imageFullName = imageDicomSet.get(0);
	            int index = imageFullName.lastIndexOf(File.separator);
	            
                String fileName = imageFullName.substring(index+1, imageFullName.length());
                String directory = imageFullName.substring(0, index+1);
                
                // boolean multiFile = true;
                // ModelImage imageWp = fileIO.readImage(fileName, directory, multiFile, null);
                // imageWp.setImageName(key);
                
                ModelImage imageWp = fileIO.readImage(fileName, directory);
                imageWp.setImageName(key);
                
                Vector<String> voiSetWp = voiNameHashtableWp_ext.get(key);
                // Vector<String> voiSetCg = voiNameHashtableCg.get(key);
                int sizeWp = voiSetWp.size();
                // int sizeCg = voiSetCg.size();
                // int sizeMin = Math.min(sizeWp, sizeCg);
                int sizeMin = sizeWp; 
                
                for ( int k = 0; k < sizeMin; k++ ) {
                	
	                String voiFullNameWp = voiSetWp.get(k);
	                // String voiFullNameCg = voiSetCg.get(k);
	                
	                // if ( voiFullName.contains("cg") || voiFullName.contains("TZ")) {
	                if ( voiFullNameWp.endsWith("voi")  /* && voiFullNameCg.contains("cg") */  ) {
	                // if ( voiFullNameWp.contains("wp") && voiFullNameCg.contains("cg") ) {
	                	
		                index = voiFullNameWp.lastIndexOf(File.separator);
		                String voiNameWp = voiFullNameWp.substring(index+1, voiFullNameWp.length());
		                String voiDirectoryWp = voiFullNameWp.substring(0, index+1);
		                
		                FileVOI fileVOIwp = null;
					    fileVOIwp = new FileVOI(voiNameWp, voiDirectoryWp, imageWp);
					    VOI[] voiWp = null;
						voiWp = fileVOIwp.readVOI(false);
		                  
					    /*
						index = voiFullNameCg.lastIndexOf(File.separator);
		                String voiNameCg = voiFullNameCg.substring(index+1, voiFullNameCg.length());
		                String voiDirectoryCg = voiFullNameCg.substring(0, index+1);
						
						FileVOI fileVOICg = null;
						fileVOICg = new FileVOI(voiNameCg, voiDirectoryCg, imageCg);
						VOI[] voiCg = null;
						voiCg = fileVOICg.readVOI(false);
			            */ 
					   
		                if ( voiWp[0] != null ) {
		                  imageWp.registerVOI(voiWp[0]);  
		                  imageHashtableWp_ext.put(key, imageWp);	
		                  // new ViewJFrameImage(imageWp);
		                } 
		                 
			            /*
		                if ( voiCg[0] != null ) {
		                  imageCg.registerVOI(voiCg[0]);  
		                  imageHashtableCg.put(key, imageCg);
		                  // new ViewJFrameImage(imageCg);
			            }
			            */ 
			            
		                
	                }
                }
               // Thread.sleep(1000);
	            // pause();
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
		
	 private ModelImage cropImage(ModelImage image) {

			int[] destExtents = new int[3];
			int[] xBounds = new int[2];
			int[] yBounds = new int[2];
			int[] zBounds = new int[2];
			
			int[] extents = image.getExtents();
			int cropPixels = (int)Math.ceil(((double)extents[0] * 0.25d));
			// int cropPixels = 128;
			
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
	        
	        cropAlgo = null;
	        return resultImage;
			
		}
	 
	 private ModelImage cropImage_ext(ModelImage image) {

			int[] destExtents = new int[3];
			int[] xBounds = new int[2];
			int[] yBounds = new int[2];
			int[] zBounds = new int[2];
			
			int[] extents = image.getExtents();
			int cropPixels = (int)Math.ceil(((double)extents[0] * 0.25d));
			// int cropPixels = 128;
			
			xBounds[0] = cropPixels;
			xBounds[1] = cropPixels;
			yBounds[0] = cropPixels;
			yBounds[1] = cropPixels;
			zBounds[0] = 0;
			zBounds[1] = 0;
			
	        destExtents[0] = Math.abs(image.getExtents()[0] - xBounds[1] - xBounds[0]);
	        destExtents[1] = Math.abs(image.getExtents()[1] - yBounds[1] - yBounds[0]);
	        destExtents[2] = Math.abs(image.getExtents()[2] - zBounds[1] - zBounds[0]);

	        ModelImage resultImage = new ModelImage(ModelStorageBase.SHORT, destExtents, makeImageName(image.getImageName(), "_crop"));
			
	        xBounds[0] *= -1;
	        xBounds[1] *= -1;
	        yBounds[0] *= -1;
	        yBounds[1] *= -1;
	        zBounds[0] *= -1;
	        zBounds[1] *= -1;
	        
	        AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(image, resultImage, xBounds, yBounds, zBounds);
	        
	        cropAlgo.run();
	        cropAlgo = null;
	        
	        return resultImage;
			
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

				// if ( true ) break;
				// cropKeyImagesCE.add(ceImageVector);
				// new ViewJFrameImage(coherenceEnhancingDiffusionImage);

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

	     
}