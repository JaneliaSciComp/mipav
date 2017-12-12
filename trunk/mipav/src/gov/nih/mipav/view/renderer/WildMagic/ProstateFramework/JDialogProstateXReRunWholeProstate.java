package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
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
 * For NIH prostate data, we apply wp and cg segmentation using HED deep learning model. 
 * 
 * Data: given NIH prostate data, axial images with corresponing VOIs for Cg and Wp. 
 * 
 * Steps:
 * 1. read image file and VOIs
 * 2. From VOIs, generate the binary mask images. 
 * 3. Transform into isotropic image (upsampling) with resolution: 0.35m x 0.35m x 0.35m. 
 * 4. Conversion: converts axial image to axial an coronal using JDialogReoriented. 
 * 5. Generate the CED images from MR images in three orientation: axial, sagittal, coronal. 
 * 6. Each orientation, save 3D MR images, CED images with corresponding binary mask images. 
 * 
 * This is the 2D-volumetric segmentation approach.   
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateXReRunWholeProstate extends JDialogBase
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
	Hashtable<String, ModelImage> transformTable = new Hashtable<String, ModelImage>(); 
	
	
	Hashtable<String, VOI> voiHashtable = new Hashtable<String, VOI>(); 
	
	Hashtable<String, ModelImage> cropTable_ext = new Hashtable<String, ModelImage>();
	Hashtable<String, ModelImage> cropVoiTable_ext = new Hashtable<String, ModelImage>();
	
	
	private AlgorithmTransform algoTrans;
	
	Hashtable<String, ModelImage> scaleIntensityTable = new Hashtable<String, ModelImage>(); 
	
	private Hashtable<String, ModelImage> axialList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> axialMaskList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> sagittalList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> sagittalMaskList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> coronalList = new Hashtable<String, ModelImage>();
	private Hashtable<String, ModelImage> coronalMaskList = new Hashtable<String, ModelImage>();
	
	Hashtable<String, ModelImage> cedTableAxial = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> cedTableSagittal = new Hashtable<String, ModelImage>(); 
	Hashtable<String, ModelImage> cedTableCoronal = new Hashtable<String, ModelImage>(); 
	
	private int axial_index = 0;
	private int sagittal_index = 0;
	private int coronal_index = 0;
	
	private int startIndex = 121;
	private int endIndex = 146;
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateXReRunWholeProstate(Frame theParentFrame) {
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
		
		File fileDir_2 = new File("/scratch/prostateXdata/SortedNormalized/");
		traverse_Layer_noCoil(fileDir_2); 
		 
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
 

	
	private void traverse_secondLayer_noCoil(File firstLayer, String hashID) {
	    	
		    String[] children = firstLayer.list();
	    	
	    	for ( int i = 0; i < children.length; i++ ) {
	    		
	    		if ( children[i].equals("cg.voi")) {
	    			String voiString = firstLayer + File.separator+ children[i];
	    			System.err.println("voiString = " + voiString);
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
		
	
		runMask();
		runTransform();
		
		conversion();
		runCED();
	    
		System.err.println("saveImage");
		
		saveOrthogonalCEDImage();
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		

	}
	
	public void saveOrthogonalCEDImage() {

		System.err.println("saveOrthogonalCEDImage");

		String sliceDir = saveImageDirectory;
		File sliceDirFile = new File(sliceDir);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();

		Set<String> keys = transformTable.keySet();
		
		int index = 0;
		
		for (String key : keys ) { 

		        if ( index >= startIndex && index <= endIndex ) {	
					ModelImage axialImage = axialList.get(key);
					ModelImage cedAxialImage = cedTableAxial.get(key);
					ModelImage axialMaskImage = axialMaskList.get(key);
					
					String axialDir = sliceDir + File.separator + "axial" + File.separator;
					String imgName = "image_ced_" + key + ".xml";
					cedAxialImage.saveImage(axialDir, imgName, FileUtility.XML, false);
					imgName = "image_" + key + ".xml";
					axialImage.saveImage(axialDir, imgName, FileUtility.XML, false);
					imgName = "image_mask_" + key + ".xml";
					axialMaskImage.saveImage(axialDir, imgName, FileUtility.XML, false);
					
					ModelImage coronalImage = coronalList.get(key);
					ModelImage cedCoronalImage = cedTableCoronal.get(key);
					ModelImage coronalMaskImage = coronalMaskList.get(key);
					String coronalDir = sliceDir + File.separator + "coronal" 	+ File.separator;
					imgName = "image_ced_" + key + ".xml";
					cedCoronalImage.saveImage(coronalDir, imgName, FileUtility.XML, false);
					imgName = "image_" + key + ".xml";
					coronalImage.saveImage(coronalDir, imgName, FileUtility.XML, false);
					imgName = "image_mask_" + key + ".xml";
					coronalMaskImage.saveImage(coronalDir, imgName, FileUtility.XML, false);
	
					ModelImage sagittalImage = sagittalList.get(key);
					ModelImage cedSagittalImage = cedTableSagittal.get(key);
					ModelImage sagittalMaskImage = sagittalMaskList.get(key);
					String sagittalDir = sliceDir + File.separator + "sagittal" + File.separator;
					imgName = "image_ced_" + key + ".xml";
					cedSagittalImage.saveImage(sagittalDir, imgName, FileUtility.XML, false);
					imgName = "image_" + key + ".xml";
					sagittalImage.saveImage(sagittalDir, imgName, FileUtility.XML, false);
					imgName = "image_mask_" + key + ".xml";
					sagittalMaskImage.saveImage(sagittalDir, imgName, FileUtility.XML, false);
		        }
		        index++;
				
		}
	}
	
	 public void saveHED2DsliceCED() {
			
		 
		    int size = axialList.size();
		 
			for ( int i = 0; i < size ; i++ ) {
				
				if ( i >= startIndex && i <= endIndex ) {
			
					ModelImage axialImage = axialList.get(i);
					ModelImage axialImageMask = axialMaskList.get(i);
					// new ViewJFrameImage(axialImage);
				    // new ViewJFrameImage(axialImageMask);
					saveImage(axialImage, axialImageMask, "axial");
					
				
					ModelImage sagittalImage = sagittalList.get(i);
					ModelImage sagittalImageMask = sagittalMaskList.get(i);
					// new ViewJFrameImage(sagittalImage);
					// new ViewJFrameImage(sagittalImageMask);
					saveImage(sagittalImage, sagittalImageMask, "sagittal");
					
					
					ModelImage coronalImage = coronalList.get(i);
					ModelImage coronalImageMask = coronalMaskList.get(i);
					// new ViewJFrameImage(coronalImage);
					// new ViewJFrameImage(coronalImageMask);
					saveImage(coronalImage, coronalImageMask, "coronal");
					
				} 
			}
			
		
			size = cedTableAxial.size();
			for ( int i = 0; i < size; i++ ) {
				
				if ( i >= startIndex && i <= endIndex ) {
					
					ModelImage axialImage = cedTableAxial.get(i);
					ModelImage axialImageMask = axialMaskList.get(i);
					// new ViewJFrameImage(axialImage);
				    // new ViewJFrameImage(axialImageMask);
					saveImage(axialImage, axialImageMask, "axial");
					
					ModelImage sagittalImage = cedTableSagittal.get(i);
					ModelImage sagittalImageMask = sagittalMaskList.get(i);
					// new ViewJFrameImage(sagittalImage);
					// new ViewJFrameImage(sagittalImageMask);
					saveImage(sagittalImage, sagittalImageMask, "sagittal");
					
					ModelImage coronalImage = cedTableCoronal.get(i);
					ModelImage coronalImageMask = coronalMaskList.get(i);
					// new ViewJFrameImage(coronalImage);
					// new ViewJFrameImage(coronalImageMask);
					saveImage(coronalImage, coronalImageMask, "coronal");
				}
			}
		 
		}
		
	 
		
	    public void saveImage(ModelImage image, ModelImage imageMask,  String orientation) {
			
			int xDim = image.getExtents()[0];
			int yDim = image.getExtents()[1];
			int zDim = image.getExtents()[2];
	  
			float[] res = image.getResolutions(0);
			float x_res = res[0];
			float y_res = res[1];
			// float z_res = res[2];
			
			float[] newRes = new float[2];
			newRes[0] = x_res;
			newRes[1] = y_res;
			
			int size_3D = xDim * yDim * zDim;

			int[] newExtents = new int[2];
			newExtents[0] = xDim;
			newExtents[1] = yDim;
		
			int size = xDim * yDim;
			

			float[] imageBuffer = new float[size_3D];

			try {
				image.exportData(0, imageBuffer.length, imageBuffer);
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

			imageBuffer = null;
			
			for (int j = 0; j < zDim; j++) {
				
				try {

					ModelImage imageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "image" + j);
					imageSlice.getFileInfo(0).setResolutions(newRes);
					imageSlice.setResolutions(newRes);
					float[] imgBuffer = new float[size];
					image.exportData(j * size, size, imgBuffer);
					imageSlice.importData(0, imgBuffer, true);
					
					// new ViewJFrameImage(imageSlice);
				    
					
					ModelImage maskSlice = new ModelImage(ModelStorageBase.SHORT, newExtents, "mask" + j);
					maskSlice.setResolutions(newRes);
					maskSlice.getFileInfo(0).setResolutions(newRes);
					short[] maskBuffer = new short[size];
					imageMask.exportData(j * size, size, maskBuffer);
					maskSlice.importData(0, maskBuffer, true);
					
					// new ViewJFrameImage(maskSlice);
					
					
					String sliceDir = saveImageDirectory +  orientation + File.separator;
					File sliceDirFile = new File(sliceDir);
					if (!sliceDirFile.isDirectory())
						sliceDirFile.mkdir();
					
					String imgName = null;
					String maskName = null;
					
					if ( orientation.equals("axial")) {
						imgName = "image_" + axial_index + ".png";
						maskName = "voi_" + axial_index + ".png";
					} else if ( orientation.equals("coronal") ) {
						imgName = "image_" + coronal_index + ".png";
						maskName = "voi_" + coronal_index + ".png";
					} else if ( orientation.equals("sagittal")) {
						imgName = "image_" + sagittal_index + ".png";
						maskName = "voi_" + sagittal_index + ".png";
					}
				
					if ( imgName != null && maskName != null ) {
						
						savePNGfile(sliceDir, imgName, imageSlice, min, max, xDim, yDim, false);
						savePNGfile(sliceDir, maskName, maskSlice, min, max, xDim, yDim, true);
						System.err.println("sliceDir = " + sliceDir);
						System.err.println("imgName = " + imgName);
						// imageSlice.saveImage(sliceDir, imgName, FileUtility.JIMI, false);
						// maskSlice.saveImage(sliceDir, maskName, FileUtility.JIMI, false);
						
						if ( orientation.equals("axial")) {
							axial_index++;
						} else if ( orientation.equals("coronal") ) {
							coronal_index++;
						} else if ( orientation.equals("sagittal") ) {
							sagittal_index++;
						}
					}
					 

					imgBuffer = null;
					imageSlice.disposeLocal();
					imageSlice = null;

					maskBuffer = null;
					maskSlice.disposeLocal();
					maskSlice = null;
					
					// imgName = null;
					// maskName = null;
					// sliceDir = null;
					// break;
					
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			newExtents = null;
		}
		
	 
	public void disposeMemory() {
		
	}

	
	public void runMask() {
		
		Set<String> keys = imageHashtableCg.keySet();
		for (String key : keys) {
			    System.err.println("binary mask key = " + key);
				ModelImage cgImage = imageHashtableCg.get(key);
				ModelImage cgMaskImage = null;
				cgMaskImage = cgImage.generateBinaryImage();
				cgMaskImage.getMatrixHolder().replaceMatrices(cgImage.getMatrixHolder().getMatrices());
				cgMaskImage.getFileInfo(0).setOrigin(cgImage.getFileInfo(0).getOrigin());
				voiTable.put(key, cgMaskImage);
		}
	}

	public void runTransform() {
		
		Set<String> keys = imageHashtableCg.keySet();
		for (String key : keys) {
			    System.err.println("tranform key = " + key);
				ModelImage keyImage = imageHashtableCg.get(key);
				ModelImage transImage = calculateTransform(keyImage);
				transformTable.put(key, transImage);	
				// new ViewJFrameImage(transImage);
				
				ModelImage maskImage = voiTable.get(key);
				ModelImage transMaskImage = calculateTransform(maskImage);
				scaleVoiTable.put(key, transMaskImage);
			// new ViewJFrameImage(transMaskImage);
		}
		
	    System.err.println("finish transform");
	}
	

	public void conversion() {

		Set<String> keys = transformTable.keySet();
		
		String fileList = saveImageDirectory;
		File sliceDirFile = new File(fileList);
		if (!sliceDirFile.isDirectory())
			sliceDirFile.mkdir();
		
		int imageIndex = 0;
		
		for (String key : keys ) {

			if ( imageIndex >= startIndex && imageIndex <= endIndex ) {
				
				System.err.println("conversion imageIndex = " + imageIndex);
				
				ModelImage keyImage = transformTable.get(key);
				ModelImage keyImageMask = scaleVoiTable.get(key);
				
				// axial orientation
		        axialList.put(key, keyImage);
		        axialMaskList.put(key, keyImageMask);
		        
		        // sagittal orientation
	 			ImageReorientation sagittal_orient = new ImageReorientation(keyImage, ImageReorientation.SAGITTAL_INDEX);
	 			sagittal_orient.preformOrientation();
	 			sagittalList.put(key, sagittal_orient.getResultImage());
	 			
	 			ImageReorientation sagittal_orient_mask = new ImageReorientation(keyImageMask, ImageReorientation.SAGITTAL_INDEX);
	 			sagittal_orient_mask.preformOrientation();
	 			sagittalMaskList.put(key, sagittal_orient_mask.getResultImage());
	 		
	 			// coronal orientation
	 			ImageReorientation coronal_orient = new ImageReorientation(keyImage, ImageReorientation.CORONAL_INDEX);
	 			coronal_orient.preformOrientation();
	 			coronalList.put(key, coronal_orient.getResultImage());
	 			
	 			ImageReorientation coronal_orient_mask = new ImageReorientation(keyImageMask, ImageReorientation.CORONAL_INDEX);
	 			coronal_orient_mask.preformOrientation();
	 			coronalMaskList.put(key, coronal_orient_mask.getResultImage());
				
		        
//				// sagittal orientation 
//				JDialogReorient sag_orient = new JDialogReorient(keyImage);
//				sag_orient.setVisible(false);
//				sag_orient.set_sagittal_orientation();
//				sag_orient.doRun();
//				sagittalList.put(key, sag_orient.getResultImage());
//			    
//				JDialogReorient sag_orient_mask = new JDialogReorient(keyImageMask);
//				sag_orient_mask.setVisible(false);
//				sag_orient_mask.set_sagittal_orientation();
//				sag_orient_mask.doRun();
//				sagittalMaskList.put(key, sag_orient_mask.getResultImage());
//			
//				
//				// coronal orientation
//				JDialogReorient coronal_orient = new JDialogReorient(keyImage);
//				coronal_orient.setVisible(false);
//				coronal_orient.set_coronal_orientation();
//				coronal_orient.doRun();
//				coronalList.put(key, coronal_orient.getResultImage());
//				
//				JDialogReorient coronal_orient_mask = new JDialogReorient(keyImageMask);
//				coronal_orient_mask.setVisible(false);
//				coronal_orient_mask.set_coronal_orientation();
//				coronal_orient_mask.doRun();
//				coronalMaskList.put(key, coronal_orient_mask.getResultImage());
				
				System.err.println("imageIndex = " + imageIndex);
			}
			
			imageIndex++;
		}

		System.err.println("text write finish");

	}
	
	
	public void runCED() {
		
		Set<String> keys = transformTable.keySet();
		
		int index = 0;
		
		for (String key : keys ) {
			
	    	if ( index >= startIndex && index <= endIndex) {
	    		
				ModelImage axialImage = axialList.get(key);
				ModelImage cedAxial = calculateCoherenceEnhancingDiffusion(axialImage);
				cedTableAxial.put(key, cedAxial);
				
				ModelImage sagittalImage = sagittalList.get(key);
				ModelImage cedSagittal = calculateCoherenceEnhancingDiffusion(sagittalImage);
				cedTableSagittal.put(key, cedSagittal);
				
				ModelImage coronalImage = coronalList.get(key);
				ModelImage cedCoronal = calculateCoherenceEnhancingDiffusion(coronalImage);
				cedTableCoronal.put(key, cedCoronal);
				
	    	}
	    	index++;
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

	
	
	public void crossValidationTrain() {

		Set<String> keys = scaleIntensityTable.keySet();
		int dataSize = keys.size();
		
		System.err.println("table size = "+ dataSize);
		
		int[] index = new int[1];
	
		index[0] = 0;
        for(String key: keys){
        	saveImages(key, index, "train", scaleIntensityTable); 
        	saveImages(key, index, "train", cedTable); 
        }
		
        
	}
	
	public void saveImages(String key, int []index, String folderName, Hashtable<String, ModelImage> whichTable) {
		
		try {

			ModelImage keyImage = whichTable.get(key);
			// ModelImage cedImage = scaleCedTable.get(key);
			ModelImage keyImageVOI = voiTable.get(key);
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

	
	public void saveImagesTest(String key, int []index, String folderName ) {
		try {


			ModelImage keyImage = scaleImageTable.get(key);
			// ModelImage cedImage = scaleCedTable.get(key);
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
						
						// 1) save image
						// String sliceDir = saveImageDirectory +
						// File.separator + i + File.separator;
						

						System.err.println("index = " + index[0]);

						// String imgName = "image_" + index[0] + ".png";
						String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);

						
						String maskName = "voi_" + j + ".png";
						// maskImage.saveImage(sliceDir, maskName,
						// FileUtility.JIMI, false);
						// new ViewJFrameImage(maskImage);
						// if ( true ) break;
						savePNGfile(sliceDir, maskName, maskImage, min, max, true);

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


					} else {
						// 1) save image
						// String sliceDir = saveImageDirectory +
						// File.separator + i + File.separator;

						System.err.println("index = " + index[0]);

						// String imgName = "image_" + index[0] + ".png";
						String imgName = "image_" + j + ".png";
						// targetImageSlice.saveImage(sliceDir, imgName,
						// FileUtility.JIMI, false);
						savePNGfile(sliceDir, imgName, targetImageSlice, min, max, false);

						// ModelImage maskImage = null;
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
						savePNGfile(sliceDir, maskName, maskImage, min, max, true);

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
	        	
	        	System.err.println("readFile key = " + key);
	        	
	            Vector<String> imageDicomSet = imageNameHashtable.get(key);
	            
	            FileIO fileIO = new FileIO();
	            String imageFullName = imageDicomSet.get(0);
	            int index = imageFullName.lastIndexOf(File.separator);
	            
                String fileName = imageFullName.substring(index+1, imageFullName.length());
                String directory = imageFullName.substring(0, index+1);
                
                boolean multiFile = true;

                ModelImage imageCg = fileIO.readImage(fileName, directory, multiFile, null);
                fileIO.setQuiet(true);
                imageCg.setImageName(key);
                
                /*
                ModelImage imageChangeType = (ModelImage)imageCg.clone();
                if ( imageCg != null ) {
                	 AlgorithmChangeType algoChange = new AlgorithmChangeType(imageChangeType, ModelImage.FLOAT, (float) imageCg.getMin(),
                             (float) imageCg.getMax(), (float) imageCg.getMin(), (float) imageCg.getMax(), false);
                	 algoChange.run();
                     imageChangeType.setImageName(key);
                     algoChange = null;
                }
                */ 
                
                // Vector<String> voiSetWp = voiNameHashtableWp.get(key);
                Vector<String> voiSetCg = voiNameHashtableCg.get(key);
                // int sizeWp = voiSetWp.size();
                int sizeCg = voiSetCg.size();
                // int sizeMin = Math.min(sizeWp, sizeCg);
                // int sizeMin = sizeWp;
                int sizeMin = sizeCg;
                
                for ( int k = 0; k < sizeMin; k++ ) {
                	
	                // String voiFullNameWp = voiSetWp.get(k);
	                String voiFullNameCg = voiSetCg.get(k);
	                
	                // if ( voiFullName.contains("cg") || voiFullName.contains("TZ")) {
	                if ( /* voiFullNameWp.contains("wp") &&  */  voiFullNameCg.contains("cg")   ) {
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
		               
			            }
			             
			            
			            
		                
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
	

	 public ModelImage calculateTransform(ModelImage keyImage) {

			ModelImage resultImage;
			TransMatrix xfrm;
			int interp;
			float oXres, oYres, oZres, cXres, cYres, cZres;
			int oXdim, oYdim, oZdim, cXdim, cYdim, cZdim;
			int[] units;
			boolean doVOI, doClip, doPad, preserveFOV, doUpdateOrigin, doInvMat;
			boolean doRotateCenter;
			float fillValue = 0.0f;
			boolean isSATransform = true;
			Vector3f center = null;

			float[] dims = new float[3];
			float[] resols = new float[3];

			dims[0] = keyImage.getFileInfo()[0].getExtents()[0];
			dims[1] = keyImage.getFileInfo()[0].getExtents()[1];
			dims[2] = keyImage.getFileInfo()[0].getExtents()[2];
			resols[0] = keyImage.getFileInfo()[0].getResolutions()[0];
			resols[1] = keyImage.getFileInfo()[0].getResolutions()[1];
			resols[2] = keyImage.getFileInfo()[0].getResolutions()[2];

			doVOI = false;
			doClip = false;
			doPad = false;
			doRotateCenter = false;
			center = null;

			fillValue = 0.0f;
			doUpdateOrigin = true;
			isSATransform = true;

			interp = 0;
			xfrm = new TransMatrix(3);
			xfrm.identity();

			units = new int[3];
			units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

			float iXdim = dims[0];
			float iYdim = dims[1];
			float iZdim = dims[2];

			float iXres = resols[0];
			float iYres = resols[1];
			float iZres = resols[2];

			oXres = 0.3515625f;
		    oYres = 0.3515625f;
		    oZres = 0.3515625f;


			int constantFOV = 1;

			float fovX = iXres * (iXdim - constantFOV);
			float fovY = iYres * (iYdim - constantFOV);
			float fovZ = iZres * (iZdim - constantFOV);
			oXdim = Math.round(fovX / oXres) + constantFOV;
			oYdim = Math.round(fovY / oYres) + constantFOV;
			oZdim = Math.round(fovZ / oZres) + constantFOV;

			System.err.println("oXdim = " + oXdim + " oYdim = " + oYdim);

			algoTrans = new AlgorithmTransform(keyImage, xfrm, interp, oXres,
					oYres, oZres, oXdim, oYdim, oZdim, units, doVOI, doClip, doPad,
					doRotateCenter, center);
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
	        resultImage.getMatrixHolder().replaceMatrices(keyImage.getMatrixHolder().getMatrices());
			 
			resultImage.calcMinMax();

			// algoTrans.disposeLocal();
			algoTrans = null;
			return resultImage;
		}
	
}