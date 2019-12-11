import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmAnisotropicDiffusion;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogFuzzyCMeans;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;

import java.awt.Color;
import java.io.*;
import java.util.*;

import javax.swing.JTextField;

import org.apache.commons.csv.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmStrokeSegmentationPWI extends AlgorithmBase {
    private ModelImage dwiImage;
    
    private ModelImage adcImage;
    
    private ModelImage pwiImage;
    private ModelImage TmaxRegImage;
    
    private int pwiThreshold = 6;
    
    private int pwiCoreSegThreshold = 2;
    
    private ModelImage adcVolForThresholding;
    private ModelImage filteredADCImg = null;
    
    private ModelImage skullMaskImg = null;
    
    private ModelImage pwiArtifactMaskImg = null;
    
    private ModelImage pwiVentricleMaskImg = null;
    
    private int adcThreshold;
    
    private boolean doFilter;
    
    private boolean doSymmetryRemoval;
    
    private int maxSymmetryRemovalSlice = 10;
    
    private int maxSymmetryGrowthSlice = 10;
    
    private boolean doCerebellumSkip;
    
    private int cerebellumSkipSliceMax;
    
//    private boolean doCerebellumSkipAggressive;
//    
//    private int cerebellumSkipAggressiveSliceMax;
    
    private boolean doSkullRemoval;
    
    private int skullRemovalMaskThreshold = 70;
    
    private int ventricalRemovalMaskThreshold = 95;
    
    private int threshCloseIter;
    
    private float threshCloseSize;
    
    private VOI coreVOI;
    
    private boolean doSelectAdditionalObj = false;
    
    private float additionalObjectMinimumRatio = 0.7f;
    
    private int additionalObjectSearchSize = 1000;
    
    private int additionalObjectMaxDistance = 4;
    
    private boolean requireMinCoreSize;
    private float minCoreSizeCC;
    
    private FileIO fileIO;
    
    private int minAdcObjectSize = 10;
    private int maxAdcObjectSize = 100000;
    
    public static final String outputLabel = "CoreSeg";
    
    private String outputBasename;
    
    private static final String voiExtension = ".xml";
    
    private String coreOutputDir;
    
    private float lightboxOpacity = 0.5f;
    
    private Color coreLightboxColor = Color.RED;
    
    private Color pwiLightboxColor = Color.BLUE;
    
//    private File threshLightboxFile;
//    private File coreLightboxFile;
    
    private Hashtable<File, Vector<MaskObject>> lightboxObjectTable = new Hashtable<File, Vector<MaskObject>>();
    
    private Vector<File> lightboxFileList = new Vector<File>();

    // core size threshold for using distance-based selection 
    private int coreSizeDistSelectionThreshold = 5;
    
    // weights for selection of core object when no objects are above 5cc in size
    private float coreSelectionDistWeight = 1f;
    private float coreSelectionSizeWeight = 1 - coreSelectionDistWeight;
    
    boolean havePWI = true;
    
    boolean doPwiMultiThreading = true;
    boolean doPwiCalculateCorrelation = false;
    boolean doPwiCalculateCBFCBVMTT = false;
    boolean doPwiSaveOutputs = false;
    
    private boolean doArtifactCleanupWithMean = true;
    private float artifactMeanThreshold = 0.5f;
    private float artifactCloseSize = 6f;
    private int artifactCloseIter = 1;
    
    private boolean doPerfusionSymmetryRemoval = true;
    private int minPerfusionObjectSize = 100;
    
    /**
     * Constructor.
     *
     * @param  dwi  DWI image
     * @param  adc  ADC image
     */
    public PlugInAlgorithmStrokeSegmentationPWI(ModelImage dwi, ModelImage adc, ModelImage pwi, int threshold, boolean anisoFilter, boolean cerebellumSkip,
    		int cerebellumSkipMax, boolean symmetryRemoval, int symmetryMax, boolean removeSkull, int closeIter, float closeSize, boolean doAdditionalObj,
    		int additionalObjPct, boolean reqMinCore, float minCoreSize, String outputDir, boolean pwiMultiThreading, boolean pwiCalculateCorrelation,
    		boolean pwiCalculateCBFCBVMTT, boolean pwiSaveOutputs, boolean doArtifactCleanup, float artMean, float artCloseSize, int artCloseIter,
    		boolean doPerfSymmetry, int minPerfSize) {
        super();
        
        dwiImage = dwi;
        adcImage = adc;
        pwiImage = pwi;
        
        if (pwiImage == null) {
        	havePWI = false;
        }
        
        adcThreshold = threshold;
        doFilter = anisoFilter;
        doSymmetryRemoval = symmetryRemoval;
        maxSymmetryRemovalSlice = symmetryMax;
        maxSymmetryGrowthSlice = symmetryMax;
        doCerebellumSkip = cerebellumSkip;
        cerebellumSkipSliceMax = cerebellumSkipMax;
//        doCerebellumSkipAggressive = cerebellumSkipAggressive;
//        cerebellumSkipAggressiveSliceMax = cerebellumSkipAggressiveMax;
        doSkullRemoval = removeSkull;
        threshCloseIter = closeIter;
        threshCloseSize = closeSize;
        
        doSelectAdditionalObj = doAdditionalObj;
        additionalObjectMinimumRatio = 1f - (float)(additionalObjPct / 100f);
        
        requireMinCoreSize = reqMinCore;
        minCoreSizeCC = minCoreSize;
        
        coreOutputDir = outputDir;
        
        doPwiMultiThreading = pwiMultiThreading;
        doPwiCalculateCorrelation = pwiCalculateCorrelation;
        doPwiCalculateCBFCBVMTT = pwiCalculateCorrelation;
        doPwiSaveOutputs = pwiSaveOutputs;
        
        doArtifactCleanupWithMean = doArtifactCleanup;
        artifactMeanThreshold = artMean;
        artifactCloseSize = artCloseSize;
        artifactCloseIter = artCloseIter;
        
        doPerfusionSymmetryRemoval = doPerfSymmetry;
        minPerfusionObjectSize = minPerfSize;
        
        outputBasename = new File(coreOutputDir).getName() + "_" + outputLabel;
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        ModelImage segImg = null;
        ModelImage dwiLightbox = null;
        
        try {
            fireProgressStateChanged("Segmenting image ...");
            fireProgressStateChanged(5);
            
            // DWI -> fuzzy c means 4 class
            segImg = fuzzyCMeans(dwiImage, 4, 4);
            
            saveImageFile(segImg, coreOutputDir, outputBasename + "_seg_dwi", FileUtility.XML);
            
            final int[] extents = segImg.getExtents();
            final int sliceLength = extents[0] * extents[1];
            final int volLength = sliceLength * extents[2];
            final FileInfoBase fInfo = (FileInfoBase) segImg.getFileInfo(0).clone();
            
    //        // add in lesion segmentation from ADC
    //        ModelImage adcSegImg = fuzzyCMeans(adcImage, 3, 1);
    //        
    //        saveImageFile(adcSegImg, coreOutputDir, outputBasename + "_seg_adc", FileUtility.XML);
    //        
    //        for (int i = 0; i < volLength; i++) {
    //            if (adcSegImg.getUByte(i) != 0) {
    //                segImg.set(i, 1);
    //            }
    //        }
    //        
    //        saveImageFile(segImg, coreOutputDir, outputBasename + "_seg_both", FileUtility.XML);
            
            short[] dwiSegBuffer = new short[volLength];
            try {
                segImg.exportData(0, volLength, dwiSegBuffer);
            } catch (IOException error) {
            	dwiSegBuffer = null;
                displayError("Error on segmentation export: " + segImg.getImageName());
                setCompleted(false);
                return;
            }
            
            // skull artifact masking
            if (doSkullRemoval) {
                skullMaskImg = new ModelImage(ModelStorageBase.BOOLEAN, dwiImage.getExtents(), "dwi_skull_mask");
                for (int i = 0; i < skullMaskImg.getExtents()[2]; i++) {
                    skullMaskImg.setFileInfo(fInfo, i);
                }
                
                for (int i = 0; i < volLength; i++) {
                    if (dwiImage.getInt(i) > skullRemovalMaskThreshold) {
                        skullMaskImg.set(i, 1);
                    } else {
                        skullMaskImg.set(i, 0);
                    }
                }
                
                // get largest object after ventrical threshold (+some dilation) and save it for later pwi masking
                pwiVentricleMaskImg = new ModelImage(ModelStorageBase.BOOLEAN, dwiImage.getExtents(), "pwi_brain_mask");
                for (int i = 0; i < pwiVentricleMaskImg.getExtents()[2]; i++) {
                    pwiVentricleMaskImg.setFileInfo(fInfo, i);
                }
                
                for (int i = 0; i < volLength; i++) {
                    if (dwiImage.getInt(i) > ventricalRemovalMaskThreshold) {
                        pwiVentricleMaskImg.set(i, 1);
                    } else {
                        pwiVentricleMaskImg.set(i, 0);
                    }
                }
                
                // open - try to disconnect any artifacts from the brain mask
                open(pwiVentricleMaskImg);
                
                // close
                //close(skullMaskImg, 2, 2f, false);
                
                short[] maskBuffer = new short[volLength];
                short[] processBuffer = new short[volLength];
                try {
                    pwiVentricleMaskImg.exportData(0, volLength, maskBuffer);
                } catch (IOException error) {
                    if (segImg != null) {
                        segImg.disposeLocal();
                        segImg = null;
                    }
                    
                    maskBuffer = null;
                    displayError("Error on brain mask export: " + pwiVentricleMaskImg.getImageName());
                    setCompleted(false);
                    return;
                }
                
                MaskObject[] objects = findObjects(pwiVentricleMaskImg, maskBuffer, processBuffer, 100, 10000000);
                
                if (objects.length > 0) {
                    MaskObject largest = objects[objects.length - 1];
                    for (int i = 0; i < processBuffer.length; i++) {
                        if (processBuffer[i] == largest.id) {
                            maskBuffer[i] = 1;
                        } else {
                            maskBuffer[i] = 0;
                        }
                    }
                }
                
                try {
                    pwiVentricleMaskImg.importData(0, maskBuffer, true);
                } catch (IOException error) {
                    if (segImg != null) {
                        segImg.disposeLocal();
                        segImg = null;
                    }
                    
                    if (pwiVentricleMaskImg != null) {
                        pwiVentricleMaskImg.disposeLocal();
                        pwiVentricleMaskImg = null;
                    }
                    
                    maskBuffer = null;
                    displayError("Error on pwi brain mask importData");
                    setCompleted(false);
                    return;
                }
                
                //dilate(pwiBrainMaskImg, 0.5f);
                
                saveImageFile(pwiVentricleMaskImg, coreOutputDir, outputBasename + "_pwi_ventricle_mask", FileUtility.XML);
                
                // open - try to disconnect any artifacts from the brain mask
                open(skullMaskImg);
                
                // close
                close(skullMaskImg, 2, 2f, false);
                
                // fill holes
                fillHoles(skullMaskImg);
                
                // select only largest object
                try {
                    skullMaskImg.exportData(0, volLength, maskBuffer);
                } catch (IOException error) {
                    if (segImg != null) {
                        segImg.disposeLocal();
                        segImg = null;
                    }
                    
                    maskBuffer = null;
                    displayError("Error on brain mask export: " + skullMaskImg.getImageName());
                    setCompleted(false);
                    return;
                }
                
                objects = findObjects(skullMaskImg, maskBuffer, processBuffer, 100, 10000000);
                
                if (objects.length > 0) {
                    MaskObject largest = objects[objects.length - 1];
                    for (int i = 0; i < processBuffer.length; i++) {
                        if (processBuffer[i] == largest.id) {
                            maskBuffer[i] = 1;
                        } else {
                            maskBuffer[i] = 0;
                        }
                    }
                }
                
                try {
                    skullMaskImg.importData(0, maskBuffer, true);
                } catch (IOException error) {
                    if (segImg != null) {
                        segImg.disposeLocal();
                        segImg = null;
                    }
                    
                    if (skullMaskImg != null) {
                        skullMaskImg.disposeLocal();
                        skullMaskImg = null;
                    }
                    
                    maskBuffer = null;
                    displayError("Error on brain mask importData");
                    setCompleted(false);
                    return;
                }
                
                // dilate object slightly
                dilate(skullMaskImg, 1);
                
                saveImageFile(skullMaskImg, coreOutputDir, outputBasename + "_brain_mask", FileUtility.XML);
            }
            
            // optionally filter ADC before thresholding, etc
            adcVolForThresholding = adcImage;
            if (doFilter) {
                fireProgressStateChanged("Filtering ADC...");
                
                filteredADCImg = filterImage(adcImage);
                adcVolForThresholding = filteredADCImg;
            }
            
            // get pixels from ADC within mask with intensity < 620
            fireProgressStateChanged("Thresholding ADC ...");
            fireProgressStateChanged(20);
            
            short[] coreSegBuffer = new short[volLength];
            for (int i = 0; i < volLength; i++) {
                if (dwiSegBuffer[i] != 0 && (skullMaskImg != null && skullMaskImg.getBoolean(i) == true)) {
                    if (isADCFractional()) {
                        float adcFracThreshold = adcThreshold / 1000.0f;
                        if (adcVolForThresholding.getFloat(i) < adcFracThreshold) {
                        	coreSegBuffer[i] = 1;
                        } else {
                        	coreSegBuffer[i] = 0;
                        }
                    } else {
                        if (adcVolForThresholding.getInt(i) < adcThreshold) {
                        	coreSegBuffer[i] = 1;
                        } else {
                        	coreSegBuffer[i] = 0;
                        }
                    }
                } else {
                	coreSegBuffer[i] = 0;
                }
            }
            
            try {
                segImg.importData(0, coreSegBuffer, true);
            } catch (IOException error) {
                if (segImg != null) {
                    segImg.disposeLocal();
                    segImg = null;
                }
                
                coreSegBuffer = null;
                displayError("Error on adc threshold importData: " + adcImage.getImageName());
                setCompleted(false);
                return;
            }
            
            saveImageFile(segImg, coreOutputDir, outputBasename + "_ADC_thresh", FileUtility.XML);
            
    //        // combine threshold with ADC and save lightbox
    //        ModelImage adcLightbox = generateLightbox(adcImage, segImg, lightboxOpacity);
    //
    //        threshLightboxFile = saveImageFile(adcLightbox, coreOutputDir, outputBasename + "_ADC_thresh_lightbox", FileUtility.PNG);
    //        
    //        adcLightbox.disposeLocal();
            
            // select largest object
            fireProgressStateChanged("Finding core lesion ...");
            fireProgressStateChanged(30);
            
            // do first two results with selection only based on core size
            File lightboxPass1 = processThresholdedImg(segImg, adcImage, coreSegBuffer, extents, 1, doCerebellumSkip, cerebellumSkipSliceMax, false, doSymmetryRemoval);
//            File lightboxPass1 = processThresholdedImg(segImg, coreSegBuffer, extents, 1, false, 0, false);
            
            if (lightboxPass1 != null) {
                lightboxFileList.add(lightboxPass1);
            }
            
            File lightboxPass2 = null;
            
            if (havePWI) {
            
	            fireProgressStateChanged("Calculating perfusion Tmax ...");
	            fireProgressStateChanged(35);
	            
	            // calculate Tmax, register to adc, generate lightbox of perfusion area
	            File lightboxPWI = processPwi(pwiImage);
	            
	            if (lightboxPWI != null) {
	                lightboxFileList.add(lightboxPWI);
	            }
	            
	            fireProgressStateChanged("Finding Tmax-based core lesion ...");
	            fireProgressStateChanged(85);
	            
	            // core pass done with a segImg that combines Tmax > 2 and DWI as search area
	            ModelImage pwiSegImg = getTmaxSeg(TmaxRegImage, pwiVentricleMaskImg, pwiArtifactMaskImg, pwiCoreSegThreshold);
	            for (int i = 0; i < volLength; i++) {
	                if ((dwiSegBuffer[i] != 0 || pwiSegImg.getInt(i) != 0)
	                        && (pwiVentricleMaskImg != null && pwiVentricleMaskImg.getBoolean(i) == true)
	                        && (pwiArtifactMaskImg != null && pwiArtifactMaskImg.getBoolean(i) == true)) {
	                    if (isADCFractional()) {
	                        float adcFracThreshold = adcThreshold / 1000.0f;
	                        if (adcVolForThresholding.getFloat(i) < adcFracThreshold) {
	                        	coreSegBuffer[i] = 1;
	                        } else {
	                        	coreSegBuffer[i] = 0;
	                        }
	                    } else {
	                        if (adcVolForThresholding.getInt(i) < adcThreshold) {
	                        	coreSegBuffer[i] = 1;
	                        } else {
	                        	coreSegBuffer[i] = 0;
	                        }
	                    }
	                } else {
	                	coreSegBuffer[i] = 0;
	                }
	            }
	            
	            pwiSegImg.disposeLocal();
	            
	            try {
	                segImg.importData(0, coreSegBuffer, true);
	            } catch (IOException error) {
	                if (segImg != null) {
	                    segImg.disposeLocal();
	                    segImg = null;
	                }
	                
	                coreSegBuffer = null;
	                displayError("Error on adc threshold importData: " + adcImage.getImageName());
	                setCompleted(false);
	                return;
	            }
	            
	            saveImageFile(segImg, coreOutputDir, outputBasename + "_ADC_thresh_pwi", FileUtility.XML);
            }
            
            // generate lightbox of DWI volume with custom transfer function
            dwiLightbox = generateLightbox(dwiImage, null, coreLightboxColor, lightboxOpacity, false);
            
            File lightboxDWI = saveImageFile(dwiLightbox, coreOutputDir, outputBasename + "_DWI_lightbox", FileUtility.PNG, true);
            
            if (lightboxDWI != null) {
                lightboxFileList.add(lightboxDWI);
                Vector<MaskObject> maskList = new Vector<MaskObject>();
                MaskObject obj = new MaskObject(0, (short) 0, 0);
                obj.setDwiNoObject();
                maskList.add(obj);
                lightboxObjectTable.put(lightboxDWI, maskList);
            }
            
            dwiLightbox.disposeLocal();
            dwiLightbox = null;
            
            // do this now to have it come after the DWI in the reports
            if (havePWI) {
                lightboxPass2 = processThresholdedImg(segImg, adcImage, coreSegBuffer, extents, 2, doCerebellumSkip, cerebellumSkipSliceMax, false, doSymmetryRemoval);
                
                if (lightboxPass2 != null) {
                    lightboxFileList.add(lightboxPass2);
                }
            }
            
    //        // distance-based selection (if small core)
    //        File lightboxPass3 = processThresholdedImg(segImg, segBuffer, extents, 3, doCerebellumSkipAggressive, cerebellumSkipAggressiveSliceMax, true);
    //        
    //        if (lightboxPass3 != null) {
    //            lightboxFileList.add(lightboxPass3);
    //        }
            
            // commented out because masks seem just as useful to users
            
    //        // output core object to VOI on disk
    //        fireProgressStateChanged("Saving core VOI ...");
    //        fireProgressStateChanged(90);
    //        
    //        coreVOI = maskToVOI(segImg);
    //        if (!saveVOI(segImg, coreVOI, coreOutputDir, outputBasename + "_VOI")) {
    //            if (segImg != null) {
    //                segImg.disposeLocal();
    //            }
    //        
    //            // problem saving voi
    //            displayError("Error saving core VOI");
    //            setCompleted(false);
    //            return;
    //        }
            
            // save core stats to tab-delmited file
            String pwiImageName;
            if (pwiImage != null) {
            	pwiImageName = pwiImage.getImageFileName();
            } else {
            	pwiImageName = "No PWI included in processing";
            }
            if (!saveCoreStats(coreOutputDir, dwiImage.getImageFileName(), adcImage.getImageFileName(), pwiImageName, adcImage.getResolutions(0))) {
                if (segImg != null) {
                    segImg.disposeLocal();
                    segImg = null;
                }
                
                setCompleted(false);
                return;
            }
            
            if (skullMaskImg != null) {
                skullMaskImg.disposeLocal();
                skullMaskImg = null;
            }
            
            if (pwiArtifactMaskImg != null) {
                pwiArtifactMaskImg.disposeLocal();
                pwiArtifactMaskImg = null;
            }
            
            if (pwiVentricleMaskImg != null) {
                pwiVentricleMaskImg.disposeLocal();
                pwiVentricleMaskImg = null;
            }
            
            if (segImg != null) {
                segImg.disposeLocal();
            }
            
            if (filteredADCImg != null) {
                filteredADCImg.disposeLocal();
            }
            
            if (TmaxRegImage != null) {
            	TmaxRegImage.disposeLocal();
            }
            
            setCompleted(true);
        } catch (Exception e) {
            e.printStackTrace();
            
            if (segImg != null) {
                segImg.disposeLocal();
                segImg = null;
            }
            
            if (skullMaskImg != null) {
                skullMaskImg.disposeLocal();
                skullMaskImg = null;
            }
            
            if (pwiArtifactMaskImg != null) {
                pwiArtifactMaskImg.disposeLocal();
                pwiArtifactMaskImg = null;
            }
            
            if (pwiVentricleMaskImg != null) {
                pwiVentricleMaskImg.disposeLocal();
                pwiVentricleMaskImg = null;
            }
            
            if (dwiLightbox != null) {
                dwiLightbox.disposeLocal();
                dwiLightbox = null;
            }
            
            if (filteredADCImg != null) {
                filteredADCImg.disposeLocal();
                filteredADCImg = null;
            }
            
            if (TmaxRegImage != null) {
            	TmaxRegImage.disposeLocal();
            }
            
            setCompleted(false);
            return;
        }
    }
    
    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        dwiImage = null;
        adcImage = null;
        pwiImage = null;
        super.finalize();
    }
    
    public VOI getCoreVOI() {
        return coreVOI;
    }
    
    private File saveImageFile(final ModelImage img, final String dir, final String fileBasename, int fileType) {
        return saveImageFile(img, dir, fileBasename, fileType, false);
    }
    
    private File saveImageFile(final ModelImage img, final String dir, final String fileBasename, int fileType, boolean useDwiLUT) {
        if (fileIO == null) {
            fileIO = new FileIO();
            fileIO.setQuiet(true);
        }
        
        FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileDirectory(dir);

        if (img.getNDims() == 3) {
            opts.setBeginSlice(0);
            opts.setEndSlice(img.getExtents()[2] - 1);
        } else if (img.getNDims() == 4) {
            opts.setBeginSlice(0);
            opts.setEndSlice(img.getExtents()[2] - 1);
            opts.setBeginTime(0);
            opts.setEndTime(img.getExtents()[3] - 1);
        }

        opts.setFileType(fileType);
        final String ext = FileTypeTable.getFileTypeInfo(fileType).getDefaultExtension();
        opts.setFileName(fileBasename + ext);

        opts.setOptionsSet(true);
        opts.setMultiFile(false);
        
        if (useDwiLUT) {
            int dwiMaxLUT = 150;
            
            ModelLUT dwiLUT = new ModelLUT();
            TransferFunction tf = new TransferFunction();
            float[] tfX = new float[] {0, dwiMaxLUT, 255};
            float[] tfY = new float[] {255, 0, 0};
            tf.importArrays(tfX, tfY, tfX.length);
            dwiLUT.setTransferFunction(tf); 
            int[] indexedLUT = dwiLUT.exportIndexedLUT();
            
            ModelImage newRGB = new ModelImage(ModelImage.ARGB, img.getExtents(), img.getImageName() + "_rgb");
            
            for (int i = 1; i < img.getSize(); i += 4) {
                int indexA = (int) (tf.getRemappedValue(img.getInt(i), 255) + 0.5f);   
                int remappedA = indexedLUT[indexA];
                int red = (int)((remappedA & 0x00ff0000) >> 16);
                int green = (int)((remappedA & 0x0000ff00) >> 8);
                int blue = (int)(remappedA & 0x000000ff);
//                int pixValue = 0xff000000 | (red << 16) | (green << 8) | (blue);
                newRGB.setC((i - 1) / 4, 0, 255.0f);
                newRGB.setC((i - 1) / 4, 1, red);
                newRGB.setC((i - 1) / 4, 2, green);
                newRGB.setC((i - 1) / 4, 3, blue);
            }

//            ViewJFrameImage frame = new ViewJFrameImage(newRGB);
//            frame.setActiveImage(ViewJFrameImage.IMAGE_A);
            
            fileIO.writeImage(newRGB, opts, false, false);
        } else {
            fileIO.writeImage(img, opts, false, false);
        }
        
        return new File(dir + File.separator + fileBasename + ext);
    }
    
    private File processThresholdedImg(ModelImage segImg, ModelImage lightboxBgImg, short[] segBuffer, int[] extents, int passNum, boolean doSkip, int skipSliceMax, boolean useDistanceSelection, boolean doSymmRemoval) {
        File lightboxFile = null;
        
        final int sliceLength = extents[0] * extents[1];
        final int volLength = sliceLength * extents[2];
        
        short[] threshBuffer = Arrays.copyOf(segBuffer, segBuffer.length);
        ModelImage threshImg = (ModelImage) segImg.clone();
        
        short[] removedBuffer = null;
        if (doSkip) {
            if (removedBuffer == null) {
                removedBuffer = new short[volLength];
            }
            
            for (int iZ = 0; iZ < skipSliceMax && iZ < extents[2]; iZ++) {
                for (int iY = 0; iY < extents[1]; iY++) {
                    for (int iX = 0; iX < extents[0]; iX++) {
                        int index = (iZ * sliceLength) + (iY * extents[0]) + iX;
                        
                        if (threshBuffer[index] > 0) {
                            threshBuffer[index] = 0;
                            
                            removedBuffer[index] = 1;
                        }
                    }
                }
            }
        }
        
        if (doSymmRemoval) {
            if (removedBuffer == null) {
                removedBuffer = new short[volLength];
            }
            
            // if values are mirrored across the the l->r of the image, cancel them out.  core values should only be on one side, while the cerebelum will have values on both sides
            for (int iZ = 0; iZ < maxSymmetryRemovalSlice && iZ < extents[2]; iZ++) {
                for (int iY = 0; iY < extents[1]; iY++) {
                    for (int iX = 0; iX < extents[0] / 2; iX++) {
                        int index = (iZ * sliceLength) + (iY * extents[0]) + iX;
                        int mirroredIndex = (iZ * sliceLength) + (iY * extents[0]) + (extents[0] - iX - 1);
                        
                        if (threshBuffer[index] > 0 && threshBuffer[mirroredIndex] > 0) {
//                            threshBuffer[index] = 0;
//                            threshBuffer[mirroredIndex] = 0;
                            
                            removedBuffer[index] = 1;
                            removedBuffer[mirroredIndex] = 1;
                        }
                    }
                }
            }
            
            try {
                threshImg.importData(0, removedBuffer, true);
            } catch (IOException error) {
                if (threshImg != null) {
                    threshImg.disposeLocal();
                }
                
                threshBuffer = null;
                displayError("Error on mirrored removed data importData: " + adcImage.getImageName());
                setCompleted(false);
                return null;
            }
            
            saveImageFile(threshImg, coreOutputDir, outputBasename + "_ADC_thresh_removed_init_pass" + passNum, FileUtility.XML);
        }
        
        try {
            threshImg.importData(0, threshBuffer, true);
        } catch (IOException error) {
            if (threshImg != null) {
                threshImg.disposeLocal();
            }
            
            threshBuffer = null;
            displayError("Error on cleaned segmentation importData: " + adcImage.getImageName());
            setCompleted(false);
            return null;
        }
        
        // perform closing on threshold mask
        close(threshImg, threshCloseIter, threshCloseSize, true);
        
        try {
            threshImg.exportData(0, volLength, threshBuffer);
        } catch (IOException error) {
            if (threshImg != null) {
                threshImg.disposeLocal();
            }
            
            threshBuffer = null;
            displayError("Error on closed ADC threshold export: " + adcImage.getImageName());
            setCompleted(false);
            return null;
        }
        
        saveImageFile(threshImg, coreOutputDir, outputBasename + "_ADC_thresh_close_pass" + passNum, FileUtility.XML);
        
        Vector<MaskObject> selectedObjectList = new Vector<MaskObject>();
        
        short[] objectBuffer = chooseCoreObjects(threshImg, threshBuffer, removedBuffer, passNum, useDistanceSelection, selectedObjectList);
        
        // get pixels from ADC within closed object mask with intensity < 620, again - this time always from original ADC
        for (int i = 0; i < volLength; i++) {
            if (objectBuffer[i] != 0) {
                if (isADCFractional()) {
                    float adcFracThreshold = adcThreshold / 1000.0f;
                    if (adcImage.getFloat(i) < adcFracThreshold) {
                        objectBuffer[i] = 1;
                    } else {
                        objectBuffer[i] = 0;
                    }
                } else {
                    if (adcImage.getInt(i) < adcThreshold) {
                        objectBuffer[i] = 1;
                    } else {
                        objectBuffer[i] = 0;
                    }
                }
            } else {
                objectBuffer[i] = 0;
            }
        }
        
        try {
            threshImg.importData(0, objectBuffer, true);
        } catch (IOException error) {
            if (threshImg != null) {
                threshImg.disposeLocal();
            }
            
            threshBuffer = null;
            objectBuffer = null;
            displayError("Error on adc threshold importData: " + adcImage.getImageName());
            setCompleted(false);
            return null;
        }
        
        saveImageFile(threshImg, coreOutputDir, outputBasename + "_ADC_thresh_only_largest_pass" + passNum, FileUtility.XML);
        
        // combine core mask with ADC and save lightbox
        ModelImage coreLightbox = generateLightbox(lightboxBgImg, threshImg, coreLightboxColor, lightboxOpacity, false);
        
        lightboxFile = saveImageFile(coreLightbox, coreOutputDir, outputBasename + "_ADC_core_lightbox_pass" + passNum, FileUtility.PNG);
        
        if (lightboxFile != null) {
            lightboxObjectTable.put(lightboxFile, selectedObjectList);
        }
        
        coreLightbox.disposeLocal();
        
        if (threshImg != null) {
            threshImg.disposeLocal();
        }
        
        return lightboxFile;
    }
    
    public short[] chooseCoreObjects(final ModelImage img, final short[] imgBuffer, final short[] removedBuffer, int passNum, boolean useDistanceSelection, Vector<MaskObject> selectedObjectList) {
        short[] processBuffer = new short[imgBuffer.length];
        MaskObject[] sortedObjects = findObjects(img, imgBuffer, processBuffer, minAdcObjectSize, maxAdcObjectSize);
        
        FileInfoBase fileInfo1;
        ModelImage segImg = new ModelImage(ModelStorageBase.UBYTE, img.getExtents(), "find_objects");
        fileInfo1 = (FileInfoBase) img.getFileInfo()[0].clone();
        fileInfo1.setResolutions(img.getResolutions(0).clone());
        fileInfo1.setUnitsOfMeasure(img.getUnitsOfMeasure().clone());
        fileInfo1.setAxisOrientation(img.getFileInfo(0).getAxisOrientation().clone());
        fileInfo1.setImageOrientation(img.getFileInfo(0).getImageOrientation());
        fileInfo1.setOrigin(img.getFileInfo(0).getOrigin().clone());
        fileInfo1.setSliceThickness(img.getFileInfo(0).getSliceThickness());
        for (int i = 0; i < img.getExtents()[2]; i++) {
            segImg.setFileInfo(fileInfo1, i);
        }
        
        // if we removed some cerebellum due to symmetry, remove attached objects
        if (removedBuffer != null) {
            Vector<Short> removedMaskIdList = new Vector<Short>();
            for (int i = 0; i < removedBuffer.length; i++) {
                if (removedBuffer[i] != 0 && !removedMaskIdList.contains(processBuffer[i]) && processBuffer[i] != 0) {
                    removedMaskIdList.add(processBuffer[i]);
                }
            }
            
            int maxIndex = Math.min(processBuffer.length, maxSymmetryGrowthSlice * img.getExtents()[0] * img.getExtents()[1]);
            for (int i = 0; i < maxIndex; i++) {
                if (removedMaskIdList.contains(processBuffer[i])) {
                    processBuffer[i] = 0;
                    removedBuffer[i] = 1;
                }
            }
            
            try {
                segImg.importData(0, removedBuffer, true);
            } catch (IOException error) {
                if (segImg != null) {
                    segImg.disposeLocal();
                }
                
                segImg = null;
                displayError("Error on mirrored removed data importData: " + adcImage.getImageName());
                setCompleted(false);
                return null;
            }
            
            saveImageFile(segImg, coreOutputDir, outputBasename + "_ADC_thresh_removed_pass" + passNum, FileUtility.XML);
        }
        
        try {
            segImg.importData(0, processBuffer, true);
        } catch (IOException error) {
            processBuffer = null;
            displayError("Error on importData: " + segImg.getImageName());
            setCompleted(false);
            return null;
        }
        
        saveImageFile(segImg, coreOutputDir, outputBasename + "_find_objects_pass" + passNum, FileUtility.XML);
        
        // last object should be the largest
        if (sortedObjects.length > 0) {
            // check the number of core pixels in the top 5 objects and select the one with the most
            int numObjectsToCheckCore = sortedObjects.length;
            int[] coreSizeList = new int[numObjectsToCheckCore];
            
            for (int i = 0; i < processBuffer.length; i++) {
                for (int objNum = 0; objNum < numObjectsToCheckCore; objNum++) {
                    if (processBuffer[i] == sortedObjects[sortedObjects.length - 1 - objNum].id) {
                        if (isADCFractional()) {
                            float adcFracThreshold = adcThreshold / 1000.0f;
                            if (adcImage.getFloat(i) < adcFracThreshold) {
                                coreSizeList[objNum]++;
                            }
                        } else {
                            if (adcImage.getInt(i) < adcThreshold) {
                                coreSizeList[objNum]++;
                            }
                        }
                    }
                }
            }
            
            // min core size
            
            boolean allCoreSmallFlag = true;
            double resFactorCC = getResolutionFactorCC(adcImage);
            for (int i = 0; i < coreSizeList.length; i++) {
                int coreSize = coreSizeList[i];
                
                sortedObjects[sortedObjects.length - 1 - i].setCoreSize(coreSize);
                
                if ((coreSize * resFactorCC) > coreSizeDistSelectionThreshold) {
                    allCoreSmallFlag = false;
                    System.err.println("Found large core object");
                }
            }
            
            if (allCoreSmallFlag && useDistanceSelection) {
//                    if (!prevBrainDistMapFlag) {
//                        distanceMapFG(skullMaskImg);
//                        prevBrainDistMapFlag = true;
//                    }
                
                float[] coreDistList = new float[numObjectsToCheckCore];
                for (int i = 0; i < numObjectsToCheckCore; i++) {
                    coreDistList[i] = Float.MAX_VALUE;
                }
                int[] extents = img.getExtents();
                int sliceSize = extents[0] * extents[1];
                Vector3f centerPt = new Vector3f(extents[0]/2, extents[1]/2, extents[2]/2);
                for (int x = 0; x < extents[0]; x++) {
                    for (int y = 0; y < extents[1]; y++) {
                        for (int z = 0; z < extents[2]; z++) {
                            for (int objNum = 0; objNum < numObjectsToCheckCore; objNum++) {
                                if (processBuffer[x + (y * extents[0]) + (z * sliceSize)] == sortedObjects[sortedObjects.length - 1 - objNum].id) {
                                    Vector3f curPt = new Vector3f(x, y, z);
                                    double dist = MipavMath.distance(centerPt, curPt, adcImage.getResolutions(0));
                                    if (dist < coreDistList[objNum]) {
                                        coreDistList[objNum] = (float) dist;
                                    }
                                }
                            }
                        }
                    }
                }
                
//                    float[] coreDistList = new float[numObjectsToCheckCore];
                for (int objNum = 0; objNum < numObjectsToCheckCore; objNum++) {
                    System.err.println("Object distance: " + sortedObjects[sortedObjects.length - 1 - objNum].id + "\t" + coreDistList[objNum]);
//                        coreDistList[objNum] = skullMaskImg.getFloat(sortedObjects[sortedObjects.length -1 - objNum].index);
                }
                
                int selectedObjectIndex = 0;
                MaskObject selectedObject = sortedObjects[sortedObjects.length - 1];
                System.err.println("Object core: " + sortedObjects[sortedObjects.length - 1].id + "\t" + sortedObjects[sortedObjects.length - 1].size + "\t" + coreSizeList[0] + "\t" + coreDistList[0]);
                for (int objNum = 1; objNum < numObjectsToCheckCore; objNum++) {
                    System.err.println("Object core: " + sortedObjects[sortedObjects.length - 1 - objNum].id + "\t" + sortedObjects[sortedObjects.length - 1 - objNum].size + "\t" + coreSizeList[objNum] + "\t" + coreDistList[objNum]);
                    if ((coreSizeList[objNum] * coreSelectionSizeWeight - coreDistList[objNum] * coreSelectionDistWeight) > (coreSizeList[selectedObjectIndex] * coreSelectionSizeWeight - coreDistList[selectedObjectIndex] * coreSelectionDistWeight)) {
                        selectedObjectIndex = objNum;
                        selectedObject = sortedObjects[sortedObjects.length - 1 - objNum];
                    }
                }
                
                selectedObjectList.add(selectedObject);
                System.err.println("Selected object: " + selectedObject.id + "\t" + selectedObject.size + "\t" + coreDistList[selectedObjectIndex]);
            } else {
                int selectedObjectIndex = 0;
                MaskObject selectedObject = sortedObjects[sortedObjects.length - 1];
                System.err.println("Object core: " + sortedObjects[sortedObjects.length - 1].id + "\t" + sortedObjects[sortedObjects.length - 1].size + "\t" + coreSizeList[0]);
                for (int objNum = 1; objNum < numObjectsToCheckCore; objNum++) {
                    System.err.println("Object core: " + sortedObjects[sortedObjects.length - 1 - objNum].id + "\t" + sortedObjects[sortedObjects.length - 1 - objNum].size + "\t" + coreSizeList[objNum]);
                    if (coreSizeList[objNum] > coreSizeList[selectedObjectIndex]) {
                        selectedObjectIndex = objNum;
                        selectedObject = sortedObjects[sortedObjects.length - 1 - objNum];
                    }
                }
                
                selectedObjectList.add(selectedObject);
                System.err.println("Selected object: " + selectedObject.id + "\t" + selectedObject.size);
            }
            
            if (requireMinCoreSize) {
                Vector<Integer> remObjIndexList = new Vector<Integer>();
                for (int i = 0; i < selectedObjectList.size(); i++) {
                    if ((selectedObjectList.get(i).coreSize * resFactorCC) < minCoreSizeCC) {
                        remObjIndexList.add(i);
                    }
                }
                
                for (Integer objIndex : remObjIndexList) {
                    System.err.println("Removed small object: " + selectedObjectList.get(objIndex).id + "\t" + selectedObjectList.get(objIndex).size + "\t" + (selectedObjectList.get(objIndex).coreSize * resFactorCC));
                    selectedObjectList.remove(objIndex.intValue());
                }
            }
        } else {
            System.err.println("No qualifying object found in volume.");
        }
        
        // disabled selection of additional objects based on size/distance for now
        
        // disabled size limit - just search based on distance
//        if (selectedObjectList.get(0).size <= additionalObjectSearchSize) {
//            // find distance between largest object and the rest of the image
//            short largestObjectID = sortedObjects[sortedObjects.length - 1].id;
//            for (int i = 0; i < processBuffer.length; i++) {
//                if (segImg.getShort(i) == largestObjectID) {
//                    segImg.set(i, 1);
//                } else {
//                    segImg.set(i, 0);
//                }
//            }
//            distanceMap(segImg);
//            
//            // TODO add more objects if distance of object center of mass is below threshold
//            int nextObjectIndex = sortedObjects.length - 2;
//            MaskObject nextObject;
//            while (nextObjectIndex >= 0) {
//                nextObject = sortedObjects[nextObjectIndex];
//                float distance = segImg.getFloat(nextObject.index);
//                
//                System.err.println(nextObject.id + "\t" + nextObject.index + "\t" + segImg.getFloat(nextObject.index) + "\t" + nextObject.size);
//                
//                if (distance <= additionalObjectMaxDistance) {
//                    selectedObjectList.add(nextObject);
//                }
//                
//                nextObjectIndex--;
//            }
//        }
        
        // also keep 2nd (maybe 3rd) largest object as well, if first one is small and the second one is >= 50% of its size
        
        // if the largest object is small, see if the next one is close in size
        if (doSelectAdditionalObj) {
//        if (selectedObjectList.get(0).size <= additionalObjectSearchSize) {
            MaskObject primaryObject = selectedObjectList.get(0);
            int objectMinSize = (int) (primaryObject.coreSize * additionalObjectMinimumRatio);
            int nextObjectIndex = sortedObjects.length - 1;
            MaskObject nextObject;
            while (nextObjectIndex >= 0) {
                nextObject = sortedObjects[nextObjectIndex];
                if (nextObject.id != primaryObject.id && nextObject.coreSize >= objectMinSize) {
                    System.err.println("Added obj: " + nextObject.id + "\t" + nextObject.size + "\t" + nextObject.coreSize);
                    selectedObjectList.add(nextObject);
                }
                nextObjectIndex--;
            }
        }
//        }
        
        // set the mask value for any object that isn't the largest to 0
        for (int i = 0; i < processBuffer.length; i++) {
            boolean found = false;
            for (MaskObject obj : selectedObjectList) {
                if (processBuffer[i] == obj.id) {
                    found = true;
                    break;
                }
            }
            
            if (!found) {
                processBuffer[i] = 0;
            }
        }
        
        if (segImg != null) {
            segImg.disposeLocal(false);
            segImg = null;
        }
        
        return processBuffer;
    }
    
    public MaskObject[] findObjects(final ModelImage img, final short[] imgBuffer, short[] processBuffer, int minSize, int maxSize) {
        final int imageLength = imgBuffer.length;
        
        Vector<MaskObject> objects = new Vector<MaskObject>();
        
        short floodValue = 1;
        int count = 0;
        
        deleteObjects(img, objects, imgBuffer, processBuffer, minSize, maxSize, true);

        objects.removeAllElements();
        
        for (int i = 0; (i < imageLength) && !threadStopped; i++) {
            if (imgBuffer[i] > 0) {
                count = floodFill(imgBuffer, img.getExtents(), processBuffer, i, floodValue, imgBuffer[i]);
                objects.addElement(new MaskObject(i, floodValue, count));
                floodValue++;
            }
        }
        
        MaskObject[] sortedObjects = objects.toArray(new MaskObject[0]);
        Arrays.sort(sortedObjects, new MaskObjectSizeComparator());
        
        return sortedObjects;
    }
    
    private void deleteObjects(final ModelImage img, final Vector<MaskObject> objects, short[] imgBuffer, short[] processBuffer, final int min, final int max, final boolean returnFlag) {
        int i, pix;
        int count;
        final int xDim = img.getExtents()[0];
        final int yDim = img.getExtents()[1];
        final int zDim = img.getExtents()[2];
        final int volumeLength = xDim * yDim * zDim;
        short floodValue = 1;
        short[] tmpBuffer;

        objects.removeAllElements();

        for (pix = 0; (pix < volumeLength) && !threadStopped; pix++) {
            if (imgBuffer[pix] > 0) {
                count = floodFill(imgBuffer, img.getExtents(), processBuffer, pix, floodValue, imgBuffer[pix]);
                objects.addElement(new MaskObject(pix, floodValue, count));
                floodValue++;
            }
        }

        tmpBuffer = imgBuffer;
        imgBuffer = processBuffer;
        processBuffer = tmpBuffer;

        for (i = 0; i < objects.size(); i++) {
            if ( (objects.elementAt(i).size < min) || (objects.elementAt(i).size > max)) {
                floodFill(imgBuffer, img.getExtents(), processBuffer, objects.elementAt(i).index, (short) 0,
                        objects.elementAt(i).id);
                objects.removeElementAt(i);
                i--;
            }
        }

        // relabel objects in order
        for (i = 0; i < objects.size(); i++) {
                floodFill(imgBuffer, img.getExtents(), processBuffer, objects.elementAt(i).index, (short) (i + 1),
                        objects.elementAt(i).id);
        }
    }
    
    /**
     * 3D flood fill that forms a short mask.
     * 
     * @param imgBuffer buffer of image data being processed
     * @param extents extents of image data being processed (3D)
     * @param idBuffer buffer to store flooding results
     * @param stIndex starting index indicating the starting location of the flood fill
     * @param floodValue the value to flood the area with
     * @param objValue DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private int floodFill(final short[] imgBuffer, final int[] extents, final short[] idBuffer, final int stIndex, final short floodValue, final short objValue) {
        final int xDim = extents[0];
        final int yDim = extents[1];
        final int zDim = extents[2];
        final int sliceSize = xDim * yDim;
        int x, y, z;
        int indexZ, indexY;
        int pixCount = 0;

        Point3D pt;
        Point3D tempPt;
        final Point3D seed3DPt = new Point3D( (stIndex % sliceSize) % xDim, (stIndex % sliceSize) / xDim,
                (stIndex / sliceSize));
        final Stack<Point3D> stack = new Stack<Point3D>();

        if (imgBuffer[ (seed3DPt.z * sliceSize) + (seed3DPt.y * xDim) + seed3DPt.x] > 0) {
            stack.push(seed3DPt);
            imgBuffer[ (seed3DPt.z * sliceSize) + (seed3DPt.y * xDim) + seed3DPt.x] = 0;

            while ( !stack.empty()) {
                pt = (Point3D) stack.pop();
                x = pt.x;
                y = pt.y;
                z = pt.z;

                indexZ = z * sliceSize;
                indexY = y * xDim;
                idBuffer[indexZ + indexY + x] = floodValue;
                pixCount++;

                if ( (x + 1) < xDim) {

                    if (imgBuffer[indexZ + indexY + x + 1] == objValue) {
                        tempPt = new Point3D(x + 1, y, z);
                        stack.push(tempPt);
                        imgBuffer[indexZ + indexY + tempPt.x] = 0;
                    }
                }

                if ( (x - 1) >= 0) {

                    if (imgBuffer[indexZ + indexY + x - 1] == objValue) {
                        tempPt = new Point3D(x - 1, y, z);
                        stack.push(tempPt);
                        imgBuffer[indexZ + indexY + tempPt.x] = 0;
                    }
                }

                if ( (y + 1) < yDim) {

                    if (imgBuffer[indexZ + ( (y + 1) * xDim) + x] == objValue) {
                        tempPt = new Point3D(x, y + 1, z);
                        stack.push(tempPt);
                        imgBuffer[indexZ + (tempPt.y * xDim) + x] = 0;
                    }
                }

                if ( (y - 1) >= 0) {

                    if (imgBuffer[indexZ + ( (y - 1) * xDim) + x] == objValue) {
                        tempPt = new Point3D(x, y - 1, z);
                        stack.push(tempPt);
                        imgBuffer[indexZ + (tempPt.y * xDim) + x] = 0;
                    }
                }

                if ( (z + 1) < zDim) {

                    if (imgBuffer[ ( (z + 1) * sliceSize) + indexY + x] == objValue) {
                        tempPt = new Point3D(x, y, z + 1);
                        stack.push(tempPt);
                        imgBuffer[ (tempPt.z * sliceSize) + indexY + x] = 0;
                    }
                }

                if ( (z - 1) >= 0) {

                    if (imgBuffer[ ( (z - 1) * sliceSize) + indexY + x] == objValue) {
                        tempPt = new Point3D(x, y, z - 1);
                        stack.push(tempPt);
                        imgBuffer[ (tempPt.z * sliceSize) + indexY + x] = 0;
                    }
                }
            }
        }

        return pixCount;
    }
    
    /**
     * Simple class to temporarily store the object's size, ID and seed index value. This class is used by the
     * identifyObjects and deleteObjects methods of AlgorithmMorphology3D class.
     */
    public class MaskObject {

        /** DOCUMENT ME! */
        public short id = 0;

        /** DOCUMENT ME! */
        public int index = 0;

        /** DOCUMENT ME! */
        public int size = 0;
        
        public int coreSize = 0;
        
        public int perfusionSize = 0;
        
        public boolean isDwi = false;

        /**
         * Creates a new intObject object.
         * 
         * @param idx seed index. Index is the location in the image
         * @param objectID the flood seed having a value >= 0.
         * @param objectSize the number of voxels in the object
         */
        public MaskObject(final int idx, final short objectID, final int objectSize) {
            index = idx;
            id = objectID;
            size = objectSize;
        }
        
        public void setDwiNoObject() {
        	id = -1;
        	index = -1;
        	size = -1;
        	coreSize = -1;
        	perfusionSize = -1;
        	isDwi = true;
        }
        
        public void setCoreSize(final int core) {
            coreSize = core;
        }
        
        public void setPerfusionSize(final int size) {
        	perfusionSize = size;
        }
    }
    
    private class MaskObjectSizeComparator implements Comparator<MaskObject> {
        @Override
        public int compare(final MaskObject o1, final MaskObject o2) {
            if (o1.size > o2.size) {
                return 1;
            } else if (o1.size < o2.size) {
                return -1;
            } else {
                return 0;
            }
        }
    }
    
    private VOI maskToVOI(ModelImage img) {
        final AlgorithmVOIExtraction voiAlgo = new AlgorithmVOIExtraction(img);
        voiAlgo.run();
        return voiAlgo.getAddedVOI();
    }
    
    private boolean saveVOI(final ModelImage img, final VOI voi, final String outputDir, final String voiBasename) {
        final boolean saveAllContours = true;
        final boolean overwriteVOI = true;

        FileVOI fileVOI;
        String extension = voiExtension;

        try {
            fileVOI = new FileVOI(voiBasename + extension, outputDir, img);
            fileVOI.writeXML(voi, saveAllContours, overwriteVOI);
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing VOI" + error);
            return false;
        }
        
        return true;
    }
    
    private boolean saveCoreStats(final String outputDir, final String dwiFile, final String adcFile, final String pwiFile, final float[] imgResol) {
        double voxelSize = imgResol[0] * imgResol[1] * imgResol[2];
        
        final String statsFile = outputDir + File.separator + outputBasename + "_stats.table";
        
        final String volUnitsStr = adcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();
        
        CSVPrinter csvPrinter = null;
        BufferedWriter bw = null;
        FileWriter fw = null;
        try {
            fw = new FileWriter(statsFile);
            bw = new BufferedWriter(fw);
            
            csvPrinter = new CSVPrinter(bw, CSVFormat.TDF.withHeader("Base Dir", "DWI File", "ADC File", "PWI File",
            		"Core Voxel Count (DWI+ADC)", "Core Volume (DWI+ADC) " + volUnitsStr,
            		"Core Voxel Count (DWI+TMax+ADC)", "Core Volume (DWI+TMax+ADC) " + volUnitsStr,
            		"Tmax > " + pwiThreshold + " Voxel Count", "Tmax > " + pwiThreshold + " Volume",
            		"Perfusion mismatch (Perfusion - DWI+ADC)", "Perfusion ratio (DWI+ADC/DWI+ADC)",
            		"Perfusion mismatch (Perfusion - DWI+Tmax+ADC)", "Perfusion ratio (DWI+Tmax+ADC/Perfusion)"));
            
            double perfusionSize = 0;
            double dwiCore = 0;
            double pwiCore = 0;
            
            // pass 1 = DWI/ADC ; pass 2 = perfusion ; pass 3 = DWI+Tmax/ADC
            for (int i = 0; i < getLightboxFileList().size(); i++) {
                int passNum = i + 1;
                
                int coreSize = getCoreSize(getLightboxFileList().get(i));
                int perfSize = getPerfusionSize(getLightboxFileList().get(i));
                
                if (perfSize > 0) {
                	perfusionSize = perfSize;
                } else if (coreSize > 0) {
                    if (passNum == 1) {
                    	dwiCore = coreSize;
                    } else if (passNum == 3) {
                    	pwiCore = coreSize;
                    } else {
                    	System.err.println("Unexpected core pass value: " + passNum + " " + coreSize);
                    }
                }
            }
            
            double dwiCoreVol = dwiCore * voxelSize;
            double pwiCoreVol = pwiCore * voxelSize;
            double perfusionVol = perfusionSize * voxelSize;
            
            double perfusionMismatchDwi = perfusionSize - dwiCore;
            double perfusionRatioDwi = dwiCore / perfusionSize;
            double perfusionMismatchPwi = perfusionSize - pwiCore;
            double perfusionRatioPwi = pwiCore / perfusionSize;
            
            System.err.println("CoreDWI/CorePWI/Perfusion Sizes:\t" + dwiCore + "\t" + pwiCore + "\t" + perfusionSize);
            csvPrinter.printRecord(outputDir, dwiFile, adcFile, pwiFile, dwiCore, dwiCoreVol, pwiCore, pwiCoreVol, perfusionSize, perfusionVol, perfusionMismatchDwi, perfusionRatioDwi, perfusionMismatchPwi, perfusionRatioPwi);
        } catch (final IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("Error writing core stats file: " + statsFile);
            return false;
        } finally {
            try {
                if (csvPrinter != null) {
                    csvPrinter.flush();
                    csvPrinter.close();
                }
                if (bw != null) {
                    bw.close();
                }
                if (fw != null) {
                    fw.close();
                }
            } catch (final IOException e) {
                // do nothing
            }
        }
        
        return true;
    }
    
    private ModelImage generateLightbox(final ModelImage imgA, final ModelImage mask, Color maskColor, float blendingOpacity, boolean isTmax) {
        ModelImage lightbox = null;
        
        ModelImage newRGB;
        if (imgA.isColorImage()) {
            newRGB = (ModelImage) imgA.clone();
        } else {
            if (!isTmax) {
            	newRGB = new ModelImage(ModelImage.ARGB, imgA.getExtents(), imgA.getImageName());
            	final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(imgA, imgA, imgA, newRGB, true, true, 255.0f, true);
            	mathAlgo.run();
            } else {
            	float tmaxWin = 10;
	            float tmaxLev = 1;
	            
	            ModelLUT tmaxLUT = new ModelLUT();
	            TransferFunction tf = new TransferFunction();
	            float[] tfX = new float[] {0, 0, 6, (int)imgA.getMax()};
	            float[] tfY = new float[] {255, 153, 0, 0};
//	            float[] tfX = new float[4];
//	            float[] tfY = new float[4];
//	            JDialogWinLevel.calcWinLevTransferFunction(imgA, tmaxWin, tmaxLev, tfX, tfY);
	            tf.importArrays(tfX, tfY, tfX.length);
	            tmaxLUT.setTransferFunction(tf);
	            
	            ModelImage newImg = (ModelImage) imgA.clone();
	            
	            for (int i = 0; i < imgA.getSize(); i++) {
	                int indexA = (int) (tf.getRemappedValue(imgA.getInt(i), (int)imgA.getMax()) + 0.5f);
	                newImg.set(i, indexA);
	            }
	            
	            newImg.calcMinMax();
	            
	            //AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(newImg, ModelImage.UBYTE, newImg.getMin(), newImg.getMax(), 0, 255, false);
	            //changeTypeAlgo.run();
	            
	            //saveImageFile(newImg, coreOutputDir, outputBasename + "_PWI_Tmax_reg_rgb_remap", FileUtility.XML);
            	
            	newRGB = new ModelImage(ModelImage.ARGB, newImg.getExtents(), newImg.getImageName());
            	final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(newImg, newImg, newImg, newRGB, true, true, 255.0f, true, true);
            	mathAlgo.run();
            	
            	//saveImageFile(newRGB, coreOutputDir, outputBasename + "_PWI_Tmax_reg_rgb", FileUtility.XML);
            	
            	newImg.disposeLocal();
	            
//	            ViewJFrameImage frame = new ViewJFrameImage(newRGB);
//	            frame.setActiveImage(ViewJFrameImage.IMAGE_A);
            }
        }
        
        if (mask != null) {
            // set any values to fully red if set in the mask
            for (int i = 0; i < mask.getDataSize(); i++) {
                if (mask.getBoolean(i) == true) {
                    // TODO set value with opacity blending
                    
                    newRGB.setC(i, 1, maskColor.getRed());
                    newRGB.setC(i, 2, maskColor.getGreen());
                    newRGB.setC(i, 3, maskColor.getBlue());
                }
            }
        }
        
        // TODO change based on x/y dim size
        final int zoomPercent = 125;
        
        // TODO change based on slice num
        final int columns = 8;
        final int rows = 5;
        
        final int rBorderVal = 0;
        final int gBorderVal = 0;
        final int bBorderVal = 0;
        final int borderThick = 0;
        
        int startSlice = 0;
        int endSlice = imgA.getExtents()[2] - 1;
        LightboxGenerator lightGen;

        try {
            lightGen = new LightboxGenerator(newRGB, startSlice, endSlice, zoomPercent, rows, columns, rBorderVal, gBorderVal, bBorderVal, false, borderThick);
            lightGen.run();
            lightbox = lightGen.getImage();
            
            lightbox.calcMinMax();
        } catch (final Exception e) {
            e.printStackTrace();
        }
        
        newRGB.disposeLocal();
        
        return lightbox;
    }
    
    public Vector<File> getLightboxFileList() {
        return lightboxFileList;
    }
    
    public Hashtable<File, Double> getCoreObjectSizeTable() {
        Hashtable<File, Double> sizeTable = new Hashtable<File, Double>();
        for (File file : lightboxObjectTable.keySet()) {
            sizeTable.put(file, Double.valueOf(getCoreSize(file)));
        }
        
        return sizeTable;
    }
    
    public Hashtable<File, Double> getPerfusionObjectSizeTable() {
        Hashtable<File, Double> sizeTable = new Hashtable<File, Double>();
        for (File file : lightboxObjectTable.keySet()) {
            sizeTable.put(file, Double.valueOf(getPerfusionSize(file)));
        }
        
        return sizeTable;
    }
    
    public int getCoreSize(File lightboxFile) {
        int totalSize = 0;
        for (MaskObject obj : lightboxObjectTable.get(lightboxFile)) {
            totalSize += obj.coreSize;
        }
        
        return totalSize;
    }
    
    public int getPerfusionSize(File lightboxFile) {
    	int totalSize = 0;
    	for (MaskObject obj : lightboxObjectTable.get(lightboxFile)) {
            totalSize += obj.perfusionSize;
        }
        
        return totalSize;
    }
    
    private void fillHoles(ModelImage img) {
        int kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        boolean wholeImg = true;

        try {
            // No need to make new image space because the user has choosen to replace the source image
            // Make the algorithm class
            AlgorithmMorphology25D idObjectsAlgo25D = new AlgorithmMorphology25D(img, kernel, 0, AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0, wholeImg);

            //idObjectsAlgo25D.addListener(this);

            idObjectsAlgo25D.run();
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Fill objects: unable to allocate enough memory");

            return;
        }
    }
    
    private void open(ModelImage img) {
        int kernel = AlgorithmMorphology3D.CONNECTED6;
        float kernelSize = 1f; // mm
        int itersD = 2;
        int itersE = 2;
        boolean wholeImg = true;

        try {

            // Make algorithm
            AlgorithmMorphology3D closeAlgo3D = new AlgorithmMorphology3D(img, kernel, kernelSize, AlgorithmMorphology3D.OPEN, itersD, itersE, 0, 0, wholeImg);

            // closeAlgo3D.addListener(this);

            closeAlgo3D.run();
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Open: unable to allocate enough memory");

            return;
        }
    }
    
    private void open25D(ModelImage img) {
        int kernel = AlgorithmMorphology25D.CONNECTED4;
        float kernelSize = 1f; // mm
        int itersD = 2;
        int itersE = 2;
        boolean wholeImg = true;

        try {

            // Make algorithm
            AlgorithmMorphology25D closeAlgo25D = new AlgorithmMorphology25D(img, kernel, kernelSize, AlgorithmMorphology25D.OPEN, itersD, itersE, 0, 0, wholeImg);

            // closeAlgo25D.addListener(this);

            closeAlgo25D.run();
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Open: unable to allocate enough memory");

            return;
        }
    }
    
    private void close(ModelImage img, int iters, float size, boolean do25D) {
        // allow for closing to be skipped
        if (iters == 0) {
            return;
        }
        
        float kernelSize = size; // mm
        int itersD = iters;
        int itersE = iters;
        boolean wholeImg = true;
        
        try {
            if (do25D) {
                int kernel = AlgorithmMorphology25D.SIZED_CIRCLE;
                
                // Make algorithm
                AlgorithmMorphology25D closeAlgo = new AlgorithmMorphology25D(img, kernel, kernelSize, AlgorithmMorphology25D.CLOSE,
                                                        itersD, itersE, 0, 0, wholeImg);
                
                //closeAlgo.addListener(this);

                closeAlgo.run();
            } else {
                int kernel = AlgorithmMorphology3D.SIZED_SPHERE;
                
                // Make algorithm
                AlgorithmMorphology3D closeAlgo = new AlgorithmMorphology3D(img, kernel, kernelSize, AlgorithmMorphology3D.CLOSE,
                                                        itersD, itersE, 0, 0, wholeImg);
                
                //closeAlgo.addListener(this);

                closeAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Close: unable to allocate enough memory");

            return;
        }
    }
    
    private void dilate(ModelImage img, float kernelMM) {
        int kernel = AlgorithmMorphology3D.CONNECTED6;
        float kernelSize = kernelMM; // mm
        int itersD = 1;
        int itersE = 1;
        boolean wholeImg = true;

        try {

            // Make algorithm
            AlgorithmMorphology3D closeAlgo3D = new AlgorithmMorphology3D(img, kernel, kernelSize, AlgorithmMorphology3D.DILATE, itersD, itersE, 0, 0, wholeImg);

            // closeAlgo3D.addListener(this);

            closeAlgo3D.run();
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dilate: unable to allocate enough memory");

            return;
        }
    }
    
    private void distanceMapBG(ModelImage img) {
        int kernel = AlgorithmMorphology3D.CONNECTED6;
        boolean wholeImg = true;
        
        try {
            // Make algorithm
            AlgorithmMorphology3D distanceMapAlgo3D = new AlgorithmMorphology3D(img, kernel, 0,
                                                          AlgorithmMorphology3D.BG_DISTANCE_MAP, 0, 0, 0, 0,
                                                          wholeImg);

            distanceMapAlgo3D.run();
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Distance map: unable to allocate enough memory");
            return;
        }
    }
    
    private void distanceMapFG(ModelImage img) {
        int kernel = AlgorithmMorphology3D.CONNECTED6;
        boolean wholeImg = true;
        
        try {
            // Make algorithm
            AlgorithmMorphology3D distanceMapAlgo3D = new AlgorithmMorphology3D(img, kernel, 0,
                                                          AlgorithmMorphology3D.DISTANCE_MAP, 0, 0, 0, 0,
                                                          wholeImg);

            distanceMapAlgo3D.run();
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Distance map: unable to allocate enough memory");
            return;
        }
    }
    
    private ModelImage fuzzyCMeans(ModelImage srcImg, int numClasses, int segClass) {
        ModelImage segImg;
        
        final int nClasses = numClasses;
        final int nPyramid = 4;
        final int oneJacobiIter = 1;
        final int twoJacobiIter = 2;
        final float q = 2.0f;
        final float oneSmooth = 2e4f;
        final float twoSmooth = 2e5f;
        final boolean outputGainField = false;
        final int segmentation = AlgorithmFuzzyCMeans.HARD_ONLY;
        final boolean cropBackground = false;
        final float threshold = 0.0f;
        final int maxIter = 200;
        final float endTolerance = 0.01f;
        final boolean wholeImage = true;
        
        ModelImage[] segArray = new ModelImage[1];
        FileInfoBase fileInfo1;
        segArray[0] = new ModelImage(ModelStorageBase.UBYTE, srcImg.getExtents(), "_hardSeg");
        fileInfo1 = segArray[0].getFileInfo()[0];
        fileInfo1.setResolutions(srcImg.getResolutions(0));
        fileInfo1.setUnitsOfMeasure(srcImg.getUnitsOfMeasure());
        fileInfo1.setAxisOrientation(srcImg.getFileInfo(0).getAxisOrientation());
        fileInfo1.setImageOrientation(srcImg.getFileInfo(0).getImageOrientation());
        fileInfo1.setOrigin(srcImg.getFileInfo(0).getOrigin());
        fileInfo1.setSliceThickness(srcImg.getFileInfo(0).getSliceThickness());
        for (int i = 0; i < srcImg.getExtents()[2]; i++) {
            segArray[0].setFileInfo(fileInfo1, i);
        }

        AlgorithmFuzzyCMeans fcmAlgo = new AlgorithmFuzzyCMeans(segArray, srcImg, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q, oneSmooth,
                                           twoSmooth, outputGainField, segmentation, cropBackground, threshold, maxIter,
                                           endTolerance, wholeImage);
        
        //final float[] centroids = getCentroid(srcImg, nClasses);
        final float[] centroids = JDialogFuzzyCMeans.getDefaultCentroids(srcImg, nClasses, wholeImage, null, cropBackground, threshold);
        fcmAlgo.setCentroids(centroids);
        fcmAlgo.run();
        fcmAlgo.finalize();
        fcmAlgo = null;
        
        segImg = segArray[0];
        
        fireProgressStateChanged(10);
        
        // extract class 4 as mask
        fireProgressStateChanged("Extracting segmentation mask ...");
        fireProgressStateChanged(15);
        
        final int[] extents = srcImg.getExtents();
        final int sliceLength = extents[0] * extents[1];
        final int volLength = sliceLength * extents[2];
        
        // clear all values not in the selected class (lesion)
        for (int i = 0; i < volLength; i++) {
            if (segImg.getInt(i) != segClass) {
                segImg.set(i, 0);
            } else {
                segImg.set(i, 1);
            }
        }
        
        return segImg;
    }
    
    private ModelImage filterImage(final ModelImage srcImg) {
        int[] destExtents = new int[3];
        destExtents[0] = srcImg.getExtents()[0];
        destExtents[1] = srcImg.getExtents()[1];
        destExtents[2] = srcImg.getExtents()[2];
        
        float sigmaVal = 1.0f;
        float normZ = sigmaVal * (srcImg.getResolutions(0)[0] / srcImg.getResolutions(0)[2]);
        int iters = 10;
        int diffusionK = 15;
        boolean is25D = false;
        
        
        ModelImage resImg = new ModelImage(ModelImage.FLOAT, destExtents, srcImg.getImageName() + "_filtered");

        // Make algorithm
        AlgorithmAnisotropicDiffusion diffusionAlgo = new AlgorithmAnisotropicDiffusion(resImg, srcImg, new float[] {sigmaVal, sigmaVal, normZ}, iters, diffusionK, true, is25D);

        diffusionAlgo.run();
        
        return resImg;
    }
    
    public static final double getResolutionFactorCC(ModelImage img) {
        float[] resol = img.getResolutions(0);
        int[] units = img.getUnitsOfMeasure();
        
        Unit unit = Unit.getUnitFromLegacyNum(units[0]);
        double[] resolCC = new double[resol.length];
        for (int i = 0; i < resol.length; i++) {
            resolCC[i] = unit.convertTo(resol[i], Unit.CENTIMETERS);
        }
        
        return resolCC[0] * resolCC[1] * resolCC[2];
    }
    
    private boolean isADCFractional() {
        return (adcImage.getType() == ModelStorageBase.FLOAT);
    }
    
    private File processPwi(ModelImage pwiImg) {
        long tspStartTime = System.currentTimeMillis();
        
        boolean calculateMaskingThreshold = true;
        int masking_threshold = 600;
        double TSP_threshold = 0.8;
        int TSP_iter = 4;
        double Psvd = 0.1;
        boolean autoAIFCalculation = true;
        boolean calculateBounds = false;
        
        int search = PlugInAlgorithmTSPAnalysis.ELSUNC_2D_SEARCH;
        
        PlugInAlgorithmTSPAnalysis tspAlgo = new PlugInAlgorithmTSPAnalysis(pwiImg, calculateMaskingThreshold, masking_threshold,
                TSP_threshold, TSP_iter, Psvd, autoAIFCalculation, doPwiMultiThreading, search, doPwiCalculateCorrelation, doPwiCalculateCBFCBVMTT, calculateBounds);
        
        if (doPwiSaveOutputs) {
        	tspAlgo.setOutputFilePath(coreOutputDir);
        } else {
        	tspAlgo.setOutputFilePath(null);
        }
        tspAlgo.setOutputPrefix(new File(coreOutputDir).getName() + "_" + outputLabel + "_PWI_");
        
        linkProgressToAlgorithm(tspAlgo);
        tspAlgo.setProgressValues(generateProgressValues(35, 70));
        
        tspAlgo.setRunningInSeparateThread(false);
        
        tspAlgo.run();
        
        ModelImage TmaxUnregImage = tspAlgo.getTmaxImage();
        
        saveImageFile(TmaxUnregImage, coreOutputDir, outputBasename + "_PWI_Tmax", FileUtility.XML);
        
        System.err.println("PWI TSP Algo time elapsed: " + (System.currentTimeMillis() - tspStartTime) / 1000.0f);
        
        long regStartTime = System.currentTimeMillis();
        
        // register PWI seg to ADC since it is lower res
        ModelImage firstPwiVol = getFirstVolume(pwiImg);
        TransMatrix transform = getRegistrationTransform(firstPwiVol, adcImage, TmaxUnregImage);
        TmaxRegImage = transformImage(TmaxUnregImage, adcImage, transform);
        saveImageFile(TmaxRegImage, coreOutputDir, outputBasename + "_PWI_Tmax_reg", FileUtility.XML);
        
        System.err.println("PWI seg registration time elapsed: " + (System.currentTimeMillis() - regStartTime) / 1000.0f);

        ModelImage unregMaskImg = null;
        if (doArtifactCleanupWithMean) {
            unregMaskImg = getPwiArtifactMask(firstPwiVol);
            pwiArtifactMaskImg = transformImage(unregMaskImg, adcImage, transform);
            saveImageFile(pwiArtifactMaskImg, coreOutputDir, outputBasename + "_pwi_brain_mask", FileUtility.XML);
        }
        
        firstPwiVol.disposeLocal();
        
        ModelImage pwiSegImg = getTmaxSeg(TmaxRegImage, pwiVentricleMaskImg, pwiArtifactMaskImg, pwiThreshold);
        cleanupMask(pwiSegImg, doPerfusionSymmetryRemoval, minPerfusionObjectSize);
        
        // combine perfusion mask with Tmax and save lightbox
        ModelImage perfLightbox = generateLightbox(TmaxRegImage, pwiSegImg, pwiLightboxColor, lightboxOpacity, true);
        
        File lightboxFile = saveImageFile(perfLightbox, coreOutputDir, outputBasename + "_Tmax_perfusion_lightbox", FileUtility.PNG, false);
        
        if (lightboxFile != null) {
        	int size = 0;
        	for (int i = 0; i < pwiSegImg.getDataSize(); i++) {
        		if (pwiSegImg.getInt(i) != 0) {
        			size++;
        		}
        	}
        	
        	Vector<MaskObject> maskList = new Vector<MaskObject>();
            MaskObject obj = new MaskObject(0, (short) 0, size);
            obj.setPerfusionSize(size);
            maskList.add(obj);
            
            lightboxObjectTable.put(lightboxFile, maskList);
        }
        
        perfLightbox.disposeLocal();
        
        if (TmaxUnregImage != null) {
        	TmaxUnregImage.disposeLocal();
        }
        
        if (unregMaskImg != null) {
            unregMaskImg.disposeLocal();
        }
        
        if (pwiSegImg != null) {
        	pwiSegImg.disposeLocal();
        }

        return lightboxFile;
    }
    
    private ModelImage getTmaxSeg(ModelImage tmaxImg, ModelImage ventricleImg, ModelImage artifactImg, int thresh) {
    	final int[] extents = tmaxImg.getExtents();
        final int sliceLength = extents[0] * extents[1];
        final int volLength = sliceLength * extents[2];
        final FileInfoBase fInfo = (FileInfoBase) tmaxImg.getFileInfo(0).clone();
        
        ModelImage pwiSegImg = new ModelImage(ModelImage.UBYTE, tmaxImg.getExtents(), tmaxImg.getImageName() + "_seg_" + thresh + "s");
        for (int i = 0; i < pwiSegImg.getExtents()[2]; i++) {
            pwiSegImg.setFileInfo(fInfo, i);
        }
        
        for (int i = 0; i < volLength; i++) {
            if ((ventricleImg != null && ventricleImg.getBoolean(i) == true) && (artifactImg != null && artifactImg.getBoolean(i) == true)) {
                if (tmaxImg.getInt(i) > thresh) {
                    pwiSegImg.set(i, 1);
                } else {
                    pwiSegImg.set(i, 0);
                }
            }
        }
        
        saveImageFile(pwiSegImg, coreOutputDir, outputBasename + "_PWI_Tmax_thresh_" + thresh + "s", FileUtility.XML);
        
        return pwiSegImg;
    }
    
    private ModelImage getFirstVolume(ModelImage multiVolImg) {
        int extents3Dorg[] = multiVolImg.getExtents();
        int sliceLength = extents3Dorg[0] * extents3Dorg[1];
        int xDim = extents3Dorg[0];
        int yDim = extents3Dorg[1];
        int zDim;
        int tDim;
        int units3D[] = new int[3];
        int extents3D[] = new int[3];
        float resolutions3D[] = new float[3];
        
        String tDimString;
        
        String delZString;
        float delZ;
        
        extents3D[0] = xDim;
        extents3D[1] = yDim;
        
        FileInfoDicom dicomInfo = (FileInfoDicom) multiVolImg.getFileInfo(0);
        FileDicomTagTable tagTable = dicomInfo.getTagTable();
        FileInfoDicom dicomInfoLast = (FileInfoDicom) multiVolImg.getFileInfo(extents3Dorg[2]-1);
        FileDicomTagTable tagTableLast = dicomInfoLast.getTagTable();

        //System.out.println("TE = " + TE);
        if (tagTable.getValue("0020,0105") != null) {
            // Number of temporal positions
            FileDicomTag tag = tagTable.get(new FileDicomKey("0020,0105"));
            tDimString = (String)tag.getValue(false);
        }
        else if (tagTableLast.getValue("0020,0012") != null) {
            FileDicomTag tag = tagTableLast.get(new FileDicomKey("0020,0012"));
            tDimString = (String)tag.getValue(false); 
        }
        else {
            MipavUtil.displayError("Tags (0020,0012) and (0020,0105) are both null");
            setCompleted(false);
            return multiVolImg;
        }
        tDim = Integer.valueOf(tDimString.trim()).intValue();
        if ((tDim <= 2) || (tDim > 500)) {
            MipavUtil.displayError("Exiting with an impossible time dimension value of " + tDim);
            setCompleted(false);
            return multiVolImg;
        }
        zDim = extents3Dorg[2]/tDim;
        if ((zDim <= 2) || (zDim > 500)) {
            MipavUtil.displayError("Exiting with an impossible z dimension value of " + zDim);
            setCompleted(false);
            return multiVolImg;
        }
        extents3D[2] = zDim;
        for (int i = 0; i < 2; i++) {
            resolutions3D[i] = multiVolImg.getResolutions(0)[i];
            //System.out.println("resolutions["+i+"] = " + resolutions[i]);
        }
        
        if (tagTable.getValue("0018,0088") != null) {
            // Spacing between slices in millimeters
            FileDicomTag tag = tagTable.get(new FileDicomKey("0018,0088"));
            delZString = (String)tag.getValue(false);
        }
        else {
            MipavUtil.displayError("Tag (0018,0088) for Spacing between slices is null");
            setCompleted(false);
            return multiVolImg;
        }
        delZ = Float.valueOf(delZString.trim()).floatValue();
        //System.out.println("delZ = " + delZ);
        resolutions3D[2] = delZ;
        //System.out.println("zDim = " + zDim + " tDim = " + tDim);

        for (int i = 0; i < 3; i++) {
            units3D[i] = Unit.MILLIMETERS.getLegacyNum();
        }
        
        short[] sliceBuffer = new short[sliceLength];
        
        ModelImage firstVol = new ModelImage(multiVolImg.getType(), extents3D, multiVolImg.getImageName() + "_firstVol");
        firstVol.setResolutions(resolutions3D);
        // TODO units? others?
        
        int zDest = 0; // start counting the slices of the destination image at the first slice.

        int multiVolZDim = extents3Dorg[2];
        
        String acquisNumFirst = (String)((FileInfoDicom) multiVolImg.getFileInfo(0)).getTagTable().get("0020,0012").getValue(false);
        String temporalPosNumFirst = null;
        if (((FileInfoDicom) multiVolImg.getFileInfo(0)).getTagTable().get("0020,0100") != null) {
            temporalPosNumFirst = (String)((FileInfoDicom) multiVolImg.getFileInfo(0)).getTagTable().get("0020,0100").getValue(false);
        }
        
        for (int zSrc = 0; (zSrc < multiVolZDim) && !threadStopped; zSrc++) {
        	String acquisNum = (String)((FileInfoDicom) multiVolImg.getFileInfo(zSrc)).getTagTable().get("0020,0012").getValue(false);
        	String temporalPosNum = null;
        	if (((FileInfoDicom) multiVolImg.getFileInfo(zSrc)).getTagTable().get("0020,0100") != null) {
        	    temporalPosNum = (String)((FileInfoDicom) multiVolImg.getFileInfo(zSrc)).getTagTable().get("0020,0100").getValue(false);
        	}
        	
        	// prefer temporal position indicator, but fall-back to acquision number since not all PWI vols have 0020,0100
        	if (temporalPosNum != null) {
        	    if (temporalPosNum.trim().equals(temporalPosNumFirst.trim())) {
    	            try {
    	                // try copying the zSrc slice out of srcImage, making it the zDest in destImage
    	                multiVolImg.exportSliceXY(zSrc, sliceBuffer);
    	                firstVol.importData(zDest * sliceLength, sliceBuffer, false);
    	                zDest++;
    	            } catch (IOException e) {
    	                MipavUtil.displayError("IOException on PWI first vol export/import");
    	                setCompleted(false);
    	                return multiVolImg;
    	            }
        	    }
        	} else {
        		if (acquisNum != null && acquisNum.trim().equals(acquisNumFirst.trim())) {
    	            try {
    	                // try copying the zSrc slice out of srcImage, making it the zDest in destImage
    	                multiVolImg.exportSliceXY(zSrc, sliceBuffer);
    	                firstVol.importData(zDest * sliceLength, sliceBuffer, false);
    	                zDest++;
    	            } catch (IOException e) {
    	                MipavUtil.displayError("IOException on PWI first vol export/import");
    	                setCompleted(false);
    	                return multiVolImg;
    	            }
            	}
        	}
        	
        	
        }

        saveImageFile(firstVol, coreOutputDir, outputBasename + "_PWI_firstVol", FileUtility.XML);
        
        return firstVol;
    }
    
    private ModelImage transformImage(ModelImage imgToTransform, ModelImage refImg, TransMatrix transformation) {
        final int xdimA = refImg.getExtents()[0];
        final int ydimA = refImg.getExtents()[1];
        final int zdimA = refImg.getExtents()[2];
        final float xresA = refImg.getFileInfo(0).getResolutions()[0];
        final float yresA = refImg.getFileInfo(0).getResolutions()[1];
        final float zresA = refImg.getFileInfo(0).getResolutions()[2];
        
        float fillValue = 0.0f;
        boolean pad = false;

        AlgorithmTransform transform = new AlgorithmTransform(imgToTransform, transformation, AlgorithmTransform.TRILINEAR, xresA, yresA, zresA, xdimA,
                ydimA, zdimA, true, false, pad);

        transform.setUpdateOriginFlag(true);
        transform.setFillValue(fillValue);
        transform.run();
        ModelImage registeredImg = transform.getTransformedImage();
        
        transform.finalize();
        
        registeredImg.calcMinMax();
        
        return registeredImg;
    }
    
    private TransMatrix getRegistrationTransform(ModelImage movingImg, ModelImage refImg, ModelImage imgToTransform) {
        int costFunc = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
        
        int dof = 6;
        //int dof = 9;
        //int dof = 12;

        int searchAngle = 45;
        int coarseAngle = 15;
        int fineAngle = 6;

        int interp = AlgorithmTransform.TRILINEAR;

        boolean maxOfMin = true;
        
        boolean doSubsample = true;
        
        boolean doMultiThread = true;
        
        boolean fastMode = false;

        int baseNumIter = 2;
        
        int numMinima = 3;
        
        AlgorithmRegOAR3D regAlgo = new AlgorithmRegOAR3D(refImg, movingImg, costFunc, dof, interp, -searchAngle,
                searchAngle, coarseAngle, fineAngle, -searchAngle,
                searchAngle, coarseAngle, fineAngle, -searchAngle,
                searchAngle, coarseAngle, fineAngle, maxOfMin, doSubsample,
                doMultiThread, fastMode, baseNumIter, numMinima);

        linkProgressToAlgorithm(regAlgo);
        regAlgo.setProgressValues(generateProgressValues(70, 85));
        
        regAlgo.setRunningInSeparateThread(false);
        regAlgo.run();
        
        //saveImageFile(movingImg, coreOutputDir, outputBasename + "_PWI_firstVol_reg", FileUtility.XML);
        
        final TransMatrix finalMatrix = regAlgo.getTransform();
        //System.err.println(finalMatrix);
        
        regAlgo.finalize();
        
        return finalMatrix;
    }
    
    // get artifact mask from first PWI vol: voxels w/ vals > 50% of mean value of vol
    private ModelImage getPwiArtifactMask(ModelImage firstPwiImg) {
        ModelImage maskImg = new ModelImage(ModelStorageBase.BOOLEAN, firstPwiImg.getExtents(), "pwi_artifact_mask");
        
        final int[] extents = firstPwiImg.getExtents();
        final int sliceLength = extents[0] * extents[1];
        final int volLength = sliceLength * extents[2];
        final FileInfoBase fInfo = (FileInfoBase) firstPwiImg.getFileInfo(0).clone();
        
        for (int i = 0; i < maskImg.getExtents()[2]; i++) {
            maskImg.setFileInfo(fInfo, i);
        }
        
        // calculate mean within the DWI-based ventricle mask - the whole volume was too low
        double meanVal = 0;
        long total = 0;
        int voxelCount = 0;
        for (int i = 0; i < volLength; i++) {
            if (pwiVentricleMaskImg.getBoolean(i) == true) {
                total += firstPwiImg.getInt(i);
                voxelCount++;
            }
        }
        meanVal = total / voxelCount;
        
        double thresholdIntensity = meanVal * artifactMeanThreshold;

        for (int i = 0; i < volLength; i++) {
            if (firstPwiImg.getInt(i) >= thresholdIntensity) {
                maskImg.set(i, 1);
            } else {
                maskImg.set(i, 0);
            }
        }
        
        saveImageFile(maskImg, coreOutputDir, outputBasename + "_pwi_brain_mask", FileUtility.XML);

        // open - try to disconnect any artifacts from the brain mask
        open(maskImg);

        // close
//        close(maskImg, artifactCloseIter, artifactCloseSize, false);
        
     // fill holes
        fillHoles(maskImg);

        short[] maskBuffer = new short[volLength];
        short[] processBuffer = new short[volLength];
        try {
            maskImg.exportData(0, volLength, maskBuffer);
        } catch (IOException error) {
            if (maskImg != null) {
                maskImg.disposeLocal();
                maskImg = null;
            }

            maskBuffer = null;
            displayError("Error on brain mask export: " + firstPwiImg.getImageName());
            setCompleted(false);
            return null;
        }

        MaskObject[] objects = findObjects(maskImg, maskBuffer, processBuffer, 100, 10000000);

        if (objects.length > 0) {
            MaskObject largest = objects[objects.length - 1];
            for (int i = 0; i < processBuffer.length; i++) {
                if (processBuffer[i] == largest.id) {
                    maskBuffer[i] = 1;
                } else {
                    maskBuffer[i] = 0;
                }
            }
        }

        try {
            maskImg.importData(0, maskBuffer, true);
        } catch (IOException error) {
            if (maskImg != null) {
                maskImg.disposeLocal();
                maskImg = null;
            }

            maskBuffer = null;
            displayError("Error on pwi brain mask importData");
            setCompleted(false);
            return null;
        }

        // dilate(maskImg, 0.5f);

        //saveImageFile(maskImg, coreOutputDir, outputBasename + "_pwi_brain_mask", FileUtility.XML);
        
        return maskImg;
    }
    
    private void cleanupMask(ModelImage maskImg, boolean doSymmetryRemoval, int minObjSize) {
        final int[] extents = maskImg.getExtents();
        final int sliceLength = extents[0] * extents[1];
        final int volLength = sliceLength * extents[2];
        
        short[] removedBuffer = null;
        short[] maskBuffer = null;
        
        // TODO cleanup mask by removing voxels that are symmetric across the image center
        if (doSymmetryRemoval) {
            if (removedBuffer == null) {
                removedBuffer = new short[volLength];
            }
            
            if (maskBuffer == null) {
                maskBuffer = new short[volLength];
                
                try {
                    maskImg.exportData(0, volLength, maskBuffer);
                } catch (IOException error) {
                    maskBuffer = null;
                    displayError("Error on mask export for cleanup: " + maskImg.getImageName());
                    setCompleted(false);
                }
            }
            
            // if values are mirrored across the the l->r of the image, cancel them out.  core values should only be on one side, while the cerebelum will have values on both sides
            for (int iZ = 0; iZ < extents[2]; iZ++) {
                for (int iY = 0; iY < extents[1]; iY++) {
                    for (int iX = 0; iX < extents[0] / 2; iX++) {
                        int index = (iZ * sliceLength) + (iY * extents[0]) + iX;
                        int mirroredIndex = (iZ * sliceLength) + (iY * extents[0]) + (extents[0] - iX - 1);
                        
                        if (maskBuffer[index] > 0 && maskBuffer[mirroredIndex] > 0) {
//                            threshBuffer[index] = 0;
//                            threshBuffer[mirroredIndex] = 0;
                            
                            removedBuffer[index] = 1;
                            removedBuffer[mirroredIndex] = 1;
                        }
                    }
                }
            }
        }
        
        // remove objects below a certain size
        if (minObjSize > 0) {
            if (removedBuffer == null) {
                removedBuffer = new short[volLength];
            }
            
            if (maskBuffer == null) {
                maskBuffer = new short[volLength];
                
                try {
                    maskImg.exportData(0, volLength, maskBuffer);
                } catch (IOException error) {
                    maskBuffer = null;
                    displayError("Error on mask export for cleanup: " + maskImg.getImageName());
                    setCompleted(false);
                }
            }
            
            short[] processBuffer = new short[volLength];
    
            MaskObject[] objects = findObjects(maskImg, maskBuffer, processBuffer, minObjSize, 10000000);
    
            if (objects.length > 0) {
                for (int i = 0; i < processBuffer.length; i++) {
                    for (int curObjNum = 0; curObjNum < objects.length; curObjNum++) {
                        if (processBuffer[i] == objects[curObjNum].id) {
                            maskBuffer[i] = 1;
                            removedBuffer[i] = 0;
                        } else {
                            maskBuffer[i] = 0;
                            removedBuffer[i] = 1;
                        }
                    }
                }
            }
            
            try {
                maskImg.importData(0, maskBuffer, true);
            } catch (IOException error) {
                maskBuffer = null;
                displayError("Error on mask cleanup importData");
                setCompleted(false);
            }
        }

        if (removedBuffer != null) {
            try {
                maskImg.importData(0, removedBuffer, true);
            } catch (IOException error) {
                removedBuffer = null;
                displayError("Error on mask cleanup importData");
                setCompleted(false);
            }
            
            saveImageFile(maskImg, coreOutputDir, outputBasename + "_mask_cleanup_removed", FileUtility.XML);
            removedBuffer = null;
            
            if (maskBuffer != null) {
                try {
                    maskImg.importData(0, maskBuffer, true);
                } catch (IOException error) {
                    maskBuffer = null;
                    displayError("Error on mask cleanup importData");
                    setCompleted(false);
                }
            }
        }
        
        saveImageFile(maskImg, coreOutputDir, outputBasename + "_mask_cleaned", FileUtility.XML);
    }
}
