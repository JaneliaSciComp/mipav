package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
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

/**
 * For MICCAI ProstateX 2017 challenge testing cases, 
 * this file generates the VOI contours from HED predicted MRI+CED energy maps.   
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateSPIEcancerChallenge_HEDmap_mri_ced extends JDialogBase
		implements AlgorithmInterface {

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
	Hashtable<String, Vector<String>> imageNameHashtable = new Hashtable<String, Vector<String>>(); 
	
	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateSPIEcancerChallenge_HEDmap_mri_ced(Frame theParentFrame) {
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
			sortImageTable();
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	private void sortImageTable() {
		Set<Integer> keys = imageTable.keySet();
		
		for ( Integer i : keys ) {
		
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
			
			for ( int j = 0; j < 1000; j++ ) {
				String name = hash.get(j);
				if ( name != null ) {
					newStringVect.add(name);
					System.err.println("name = " + name);
				}
			}
			maskImageTable.put(i, newStringVect);
		}
		
		System.err.println(" finish inti read dir");
	}
	
	public static void pause() {
		try {
			// eat any pending characters
			for (int av = System.in.available(); av > 0; av--) {
				System.in.read();
			}
			System.in.read();// wait for user to hit Enter, discard result
		} catch (IOException e) {
			System.err.println("keyboard failed: " + e);
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
		
	    // read original testing images 
		File fileDir_1 = new File("/scratch/backup/SPIEChallenge/Testing/Sorted/");
		traverse_Layer(fileDir_1); 
		
		// read HED predicted MRI+CED energy maps
		File fileDir_5_test = new File("/scratch/backup/SPIEChallenge/NetherlandTrainDataPng/spie_2017_final_wp_train_mask/");
	    traverse_folder_5(fileDir_5_test);
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
	    	int maxIndex = Integer.MIN_VALUE;
	    	String maxIndexString = null;
	    	for ( int i = 0; i < children.length; i++ ) {
	    		int currentIndex = Integer.valueOf(children[i]);
	    		if ( currentIndex > maxIndex) {
	    			maxIndex = currentIndex;
	    			maxIndexString = children[i];
	    		}
	    	}
	    	// System.err.println("t2Layer: " + secondLayer + File.separator + smallIndexString);
	    	traverse_DicomLayer(new File(secondLayer + File.separator + maxIndexString), hashID);
	    	
	    }
	    

	    private void traverse_DicomLayer(File lastLayer, String hashID) {
	    	String[] children = lastLayer.list();
	    	for ( int i = 0; i < children.length; i++ ) {
	    		imageNameHashtable.get(hashID).add(lastLayer + File.separator + children[i]);
	    		System.err.println(lastLayer + File.separator + children[i]);
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
				index = Integer.parseInt(imageName.substring(start+4, end));
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
 
	
	private void traverse_folder_5(File dir) {
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_5(new File(dir, children[i]), i);
			}
		}
	}
	
	private void traverse_folder_5(File dir, int index) {
		
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
		
		if (dirName.substring(begin, end).startsWith("image")
				&& dirName.substring(begin, end).endsWith(".png")
				&& dirName.contains("fuse")) {
			System.err.println("file = " + dir.toString());
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
	    
        
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	private void generateContours() {
		
		Set<Integer> keys = maskTable.keySet();
		for ( Integer i : keys ) {
        
			Vector<ModelImage> imageVec = maskTable.get(i);
			
		    ModelImage srcImage = imageHashtable.get(i);
		    
		    float[] srcResol = srcImage.getResolutions(0);
		    ModelImage transformImage = calculateTransform_resol_fix_3D(srcImage, 0.3515625f);
		    
		    int[] transExt = transformImage.getExtents();
		    float[] transRes = transformImage.getResolutions(0);
		    int[] units = transformImage.getUnitsOfMeasure();
		    		
			int cropPixels = (int) Math.ceil(((double) transExt[0] * 0.25d));

			String leadingZero = String.format("%04d", i);
			String subDir = "ProstateX-" + leadingZero;
			String directory = saveImageDirectory + File.separator + subDir + File.separator;
           
			File dirFile = new File(directory);
			if (!dirFile.isDirectory()) {
				dirFile.mkdir();
			}
			
			VOI voiNewFinal = new VOI((short)0, "voi_result_" + i);
			int[] extents = new int[2];
			int[] srcExtents = srcImage.getExtents();
			
			int[] blankExts = new int[2];
			blankExts[0] = transExt[0];
			blankExts[1] = transExt[1];
			
			// ***********************************************
			int imageDim = srcExtents[2];
			
			for (int j = 0; j < imageDim; j++) {
				
				ModelImage image = imageVec.get(j);
				ModelImage cedImage = imageVec.get(j + imageDim);
				
				ModelImage blankImage = new ModelImage(image.getType(), blankExts, "_blank");
				int[] imageExt = image.getExtents();
				int imageSize = imageExt[0]*imageExt[1];
				int[] maskBuffer = new int[imageSize];
				int blankImageSize = blankExts[0]*blankExts[1];
				int[] blankBuffer = new int[blankImageSize];
				
				try {
					image.exportData(0, imageSize, maskBuffer);
					for ( int yIndex = 0; yIndex <= (imageExt[1]-1); yIndex++ ) {
						for (int xIndex = 0; xIndex <= (imageExt[0]-1); xIndex++ ) {
							blankBuffer[(yIndex+cropPixels)*blankExts[0] + (xIndex+cropPixels)] =
									maskBuffer[ yIndex * imageExt[0] + xIndex];
						}
					}
					blankImage.importData(0, blankBuffer, true);
					FileInfoBase fileInfo = blankImage.getFileInfo()[0];
					fileInfo.setResolutions(transRes);
					fileInfo.setUnitsOfMeasure(units);
				} catch (IOException e) {
					e.printStackTrace();
				}
				
				ModelImage blankImage_ced = new ModelImage(image.getType(), blankExts, "_blank_ced");
				int[] imageExt_ced = cedImage.getExtents();
				int imageSize_ced = imageExt_ced[0] *imageExt_ced[1];
				int[] maskBuffer_ced = new int[imageSize_ced];
				int blankImageSize_ced = blankExts[0]*blankExts[1];
				int[] blankBuffer_ced = new int[blankImageSize_ced];
				
				try {
					cedImage.exportData(0, imageSize_ced, maskBuffer_ced);
					for ( int yIndex = 0; yIndex <= (imageExt_ced[1]-1); yIndex++ ) {
						for (int xIndex = 0; xIndex <= (imageExt_ced[0]-1); xIndex++ ) {
							blankBuffer_ced[(yIndex+cropPixels)*blankExts[0] + (xIndex+cropPixels)] =
									maskBuffer_ced[ yIndex * imageExt_ced[0] + xIndex];
						}
					}
					blankImage_ced.importData(0, blankBuffer_ced, true);
					FileInfoBase fileInfo_ced = blankImage_ced.getFileInfo()[0];
					fileInfo_ced.setResolutions(transRes);
					fileInfo_ced.setUnitsOfMeasure(units);
				} catch (IOException e) {
					e.printStackTrace();
				}
				
				ModelImage transformedMaskImage = calculateTransform_resol_fix_2D(blankImage, srcResol[0]);
				ModelImage transformedMaskImage_ced = calculateTransform_resol_fix_2D(blankImage_ced, srcResol[0]);
				
				ModelImage maxMaskImage = new ModelImage(transformedMaskImage.getType(), transformedMaskImage.getExtents(), "_calc");
				int opType = AlgorithmImageCalculator.MAXIMUM;
				int clipMode = 0;
				String adOpString = null;
				AlgorithmImageCalculator mathAlgo = new AlgorithmImageCalculator(maxMaskImage, transformedMaskImage, transformedMaskImage_ced, opType, clipMode, true, adOpString);
				mathAlgo.run();
				
				extents = transformedMaskImage.getExtents();
				String name = "binaryMask_" + transformedMaskImage.getImageName();
				// new ViewJFrameImage(image);
				ModelImage resultImage = new ModelImage(ModelImage.BOOLEAN, extents, name);
				float[] thresholds = new float[2];
				thresholds[0] = 200;
				thresholds[1] = 255;
				float fillValue = 0f;
				boolean isInverse = false;
				boolean regionFlag = true;
				int outputType = 1;

				AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(resultImage, maxMaskImage, thresholds, fillValue, outputType, regionFlag, isInverse);
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
			// new ViewJFrameImage(srcImage);
			smoothVOI30(srcImage, srcImage);
			
			
			try {
				
				System.err.println("directory = " + directory);
				
				FileVOI fileVOI = new FileVOI("voi_" + i + ".voi", directory, srcImage);
				fileVOI.writeVOI(voiNewFinal, true);
			
				// maskImage.saveImage(directory, "case_" + i + "_segmentation.mhd" , FileUtility.METAIMAGE, false);
				srcImage.saveImage(directory, "image_" + i + ".xml" , FileUtility.XML, false);
	
				srcImage.disposeLocal();
				srcImage = null;
				
			} catch (IOException e ) {
				e.printStackTrace();
			}
			
		}
		
	}
	
	public ModelImage calculateTransform_resol_fix_2D(ModelImage image, float resolution) {
		
		ModelImage resultImage;
		TransMatrix xfrm;
		int interp;
		float oXres, oYres, cXres, cYres;
		int oXdim, oYdim, cXdim, cYdim;
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
        // dims[2] = image.getFileInfo()[0].getExtents()[2];   
        resols[0] = image.getFileInfo()[0].getResolutions()[0];
        resols[1] = image.getFileInfo()[0].getResolutions()[1];
        // resols[2] = image.getFileInfo()[0].getResolutions()[2];
		
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
        
        float iXdim = dims[0];
        float iYdim = dims[1];
        
        float iXres = resols[0];
        float iYres = resols[1];
        
        oXres = resolution;
        oYres = resolution;
        // oZres =  resols[2];
        
        float fovX = iXres * (iXdim - constantFOV);
        float fovY = iYres * (iYdim - constantFOV);
        oXdim = Math.round(fovX / oXres) + constantFOV;
        oYdim = Math.round(fovY / oYres) + constantFOV;
        // oZdim = (int)dims[2];
        
        System.err.println("oXdim = " + oXdim + " oYdim = " + oYdim);
        
        AlgorithmTransform algoTrans = new AlgorithmTransform(image, xfrm, interp, oXres, oYres, oXdim, oYdim, units,
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
	
	
	public ModelImage calculateTransform_resol_fix_3D(ModelImage image, float resolution) {
		
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
        
        oXres = resolution;
        oYres = resolution;
        oZres =  resols[2];
        
        float fovX = iXres * (iXdim - constantFOV);
        float fovY = iYres * (iYdim - constantFOV);
        oXdim = Math.round(fovX / oXres) + constantFOV;
        oYdim = Math.round(fovY / oYres) + constantFOV;
        oZdim = (int)dims[2];
        
        System.err.println("oXdim = " + oXdim + " oYdim = " + oYdim);
        
        AlgorithmTransform algoTrans = new AlgorithmTransform(image, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim, units,
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

	
	
	public void readFile() {

		try {
			Set<String> keys = imageNameHashtable.keySet();
			
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
                imageHashtable.put(number, image);
	        }

			
	        Set<Integer> maskKeys = maskImageTable.keySet();
			for ( Integer i : maskKeys ) {
	       
				Vector<String> stringVec = maskImageTable.get(i);
				Vector<ModelImage> imageVec = new Vector<ModelImage>();
				
				for ( int j = 0; j < stringVec.size(); j++ ) {
					String dir = stringVec.get(j);
					int index = dir.lastIndexOf(File.separator);
					String directory = new String(dir.substring(0, index + 1));
					String fileName = new String(dir.substring(index + 1, dir.length()));
				    System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
					FileIO keyImageIO = new FileIO();
				    imageVec.add(keyImageIO.readImage(fileName, directory));
				}
				System.err.println("i = " + i);
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