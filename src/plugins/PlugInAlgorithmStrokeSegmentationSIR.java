import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmAnisotropicDiffusion;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
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

import javax.swing.JLabel;
import javax.swing.JTextField;

import org.apache.commons.csv.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmStrokeSegmentationSIR extends AlgorithmBase {
    private ModelImage dwiImage;
    
    private ModelImage adcImage;
    
    private ModelImage flairImage;
    
    private ModelImage coreImage;
    
    private boolean haveFlair = true;
    
    private float contraAdcMax = 900;
    
    private TransMatrix dwiToFlairTransform;
    
    // temporarily disabling PWI processing
//    private ModelImage pwiImage;
//    
//    private ModelImage TmaxRegImage;
//    private ModelImage TmaxUnregImage;
//    
//    private TransMatrix tmaxToAdcTransform;
//    private TransMatrix adcToTmaxTransform;
    
//    private int pwiCoreSegThreshold = 2;

    private ModelImage adcVolForThresholding;
    private ModelImage filteredADCImg = null;
    
    private ModelImage skullMaskImg = null;
    
    // temporarily disabling PWI processing
//    private ModelImage pwiArtifactMaskImg = null;
//    
//    private ModelImage pwiVentricleMaskImg = null;
    
    private ModelImage unregSkullMaskImg = null;
    
    private ModelImage unregVentrImg = null;
    
    private ModelImage dwiFindObjImg = null;
    
    private MaskObject[] dwiSortedObjects = null;
    
    private int dwiMinObjSizeSIR = 1;
    
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
    
    // temporarily disabling PWI processing
//    private float ventricleRemovalMeanThreshold = 0.4f;
    
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
    
    private float sirLightboxOpacity = 0.3f;
    
    private Color coreLightboxColor = Color.RED;
    
    private Color flippedCoreLightboxColor = Color.GREEN;
    
    private static final int[] pwiThreshList = new int[] {4, 6, 8, 10};
    
//    private File threshLightboxFile;
//    private File coreLightboxFile;
    
    private Hashtable<File, Vector<MaskObject>> lightboxObjectTable = new Hashtable<File, Vector<MaskObject>>();
    
    private Vector<File> lightboxFileList = new Vector<File>();

    // core size threshold for using distance-based selection 
    private int coreSizeDistSelectionThreshold = 5;
    
    // weights for selection of core object when no objects are above 5cc in size
    private float coreSelectionDistWeight = 1f;
    private float coreSelectionSizeWeight = 1 - coreSelectionDistWeight;
    
    // temporarily disabling PWI processing
//    private PlugInAlgorithmTSPAnalysis tspAlgo = null;
    
//    private boolean havePWI = true;
//    
//    private boolean doPwiMultiThreading = true;
//    private boolean doPwiCalculateCorrelation = false;
//    private boolean doPwiCalculateCBFCBVMTT = false;
//    private boolean doPwiSaveOutputs = false;
//    
//    private boolean doPwiSpatialSmoothing = false;
//    private float pwiSigmax = 5.0f;
//    private float pwiSigmay = 5.0f;
//    
//    private boolean doArtifactCleanupWithMean = true;
//    private float artifactMeanThreshold = 0.5f;
//    private float artifactCloseSize = 6f;
//    private int artifactCloseIter = 1;
    
    // temporarily disabling PWI processing
//    private boolean doPerfusionSymmetryRemoval = true;
//    private int minPerfusionObjectSize = 100;
    
    private int adcBrainTissueTheshold = 900;
    
    /**
     * Constructor.
     *
     * @param  dwi  DWI image
     * @param  adc  ADC image
     */
    public PlugInAlgorithmStrokeSegmentationSIR(ModelImage dwi, ModelImage adc, ModelImage flair, int threshold, boolean anisoFilter, boolean cerebellumSkip,
    		int cerebellumSkipMax, boolean symmetryRemoval, int symmetryMax, boolean removeSkull, int closeIter, float closeSize, boolean doAdditionalObj,
    		int additionalObjPct, boolean reqMinCore, float minCoreSize, String outputDir) {
        super();
        
        dwiImage = dwi;
        adcImage = adc;
        
        // temporarily disabling PWI processing
//        pwiImage = pwi;
//        
//        if (pwiImage == null) {
//        	havePWI = false;
//        }
        
        flairImage = flair;
        
        if (flairImage == null) {
            haveFlair = false;
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
        
        // temporarily disabling PWI processing
//        doPwiMultiThreading = pwiMultiThreading;
//        doPwiCalculateCorrelation = pwiCalculateCorrelation;
//        doPwiCalculateCBFCBVMTT = pwiCalculateCorrelation;
//        doPwiSaveOutputs = pwiSaveOutputs;
//        
//        doPwiSpatialSmoothing = spatialSmoothing;
//        pwiSigmax = sigmax;
//        pwiSigmay = sigmay;
//        
//        doArtifactCleanupWithMean = doArtifactCleanup;
//        artifactMeanThreshold = artMean;
//        artifactCloseSize = artCloseSize;
//        artifactCloseIter = artCloseIter;
        
        // temporarily disabling PWI processing
//        doPerfusionSymmetryRemoval = doPerfSymmetry;
//        minPerfusionObjectSize = minPerfSize;
//        
//        ventricleRemovalMeanThreshold = ventThresh;
        
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
                
                // open - try to disconnect any artifacts from the brain mask
                open(skullMaskImg);
                
                // close
                close(skullMaskImg, 2, 2f, false);
                
                // fill holes
                fillHoles(skullMaskImg);
                
                short[] maskBuffer = new short[volLength];
                short[] processBuffer = new short[volLength];
                
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
                
                MaskObject[] objects = findObjects(skullMaskImg, maskBuffer, processBuffer, 100, 10000000);
                
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
            
            // temporarily disabling PWI processing
//            File lightboxPass2 = null;
//            
//            if (havePWI) {
//            
//	            fireProgressStateChanged("Calculating perfusion Tmax ...");
//	            fireProgressStateChanged(35);
//	            
//	            // calculate Tmax, register to adc, generate lightbox of perfusion area
//	            File lightboxPWI = processPwi(pwiImage);
//	            
//	            if (lightboxPWI != null) {
//	                lightboxFileList.add(lightboxPWI);
//	            }
//	            
//	            fireProgressStateChanged("Finding Tmax-based core lesion ...");
//	            fireProgressStateChanged(85);
//	            
//	            // core pass done with a segImg that combines Tmax > 2 and DWI as search area
//	            //ModelImage pwiSegImg = getTmaxSeg(TmaxRegImage, pwiVentricleMaskImg, pwiArtifactMaskImg, pwiCoreSegThreshold);
//	            ModelImage unregPwiSegImg = getTmaxSeg(TmaxUnregImage, unregVentrImg, unregSkullMaskImg, pwiCoreSegThreshold);
//	            ModelImage pwiSegImg = transformImage(unregPwiSegImg, adcImage, tmaxToAdcTransform);
//	            for (int i = 0; i < volLength; i++) {
//	                if ((dwiSegBuffer[i] != 0 || pwiSegImg.getInt(i) != 0)
//	                        && (pwiVentricleMaskImg != null && pwiVentricleMaskImg.getBoolean(i) == true)
//	                        && (skullMaskImg != null && skullMaskImg.getBoolean(i) == true)) {
//	                        /*&& (pwiArtifactMaskImg != null && pwiArtifactMaskImg.getBoolean(i) == true)) {*/
//	                    if (isADCFractional()) {
//	                        float adcFracThreshold = adcThreshold / 1000.0f;
//	                        if (adcVolForThresholding.getFloat(i) < adcFracThreshold) {
//	                        	coreSegBuffer[i] = 1;
//	                        } else {
//	                        	coreSegBuffer[i] = 0;
//	                        }
//	                    } else {
//	                        if (adcVolForThresholding.getInt(i) < adcThreshold) {
//	                        	coreSegBuffer[i] = 1;
//	                        } else {
//	                        	coreSegBuffer[i] = 0;
//	                        }
//	                    }
//	                } else {
//	                	coreSegBuffer[i] = 0;
//	                }
//	            }
//	            
//	            pwiSegImg.disposeLocal();
//	            unregPwiSegImg.disposeLocal();
//	            
//	            try {
//	                segImg.importData(0, coreSegBuffer, true);
//	            } catch (IOException error) {
//	                if (segImg != null) {
//	                    segImg.disposeLocal();
//	                    segImg = null;
//	                }
//	                
//	                coreSegBuffer = null;
//	                displayError("Error on adc threshold importData: " + adcImage.getImageName());
//	                setCompleted(false);
//	                return;
//	            }
//	            
//	            saveImageFile(segImg, coreOutputDir, outputBasename + "_ADC_thresh_pwi", FileUtility.XML);
//            }
            
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
            
            // SIR processing
            if (haveFlair) {
                processFlair();
            }
            
            // temporarily disabling PWI processing
//            // do this now to have it come after the DWI in the reports
//            if (havePWI) {
//                lightboxPass2 = processThresholdedImg(segImg, adcImage, coreSegBuffer, extents, 2, doCerebellumSkip, cerebellumSkipSliceMax, false, doSymmetryRemoval);
//                
//                if (lightboxPass2 != null) {
//                    lightboxFileList.add(lightboxPass2);
//                }
//                
//                File aifFile = tspAlgo.getAifFile();
//                File sliceAifFile = tspAlgo.getSliceAifFile();
//                if (aifFile != null) {
//                    lightboxFileList.add(aifFile);
//                    Vector<MaskObject> maskList = new Vector<MaskObject>();
//                    MaskObject obj = new MaskObject(0, (short) 0, 0);
//                    obj.setDwiNoObject();
//                    maskList.add(obj);
//                    lightboxObjectTable.put(aifFile, maskList);
//                }
//                if (sliceAifFile != null) {
//                    lightboxFileList.add(sliceAifFile);
//                    Vector<MaskObject> maskList = new Vector<MaskObject>();
//                    MaskObject obj = new MaskObject(0, (short) 0, 0);
//                    obj.setDwiNoObject();
//                    maskList.add(obj);
//                    lightboxObjectTable.put(sliceAifFile, maskList);
//                }
//            }
            
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
            
            // temporarily disabling PWI processing
//            // save core stats to tab-delmited file
//            String pwiImageName;
//            if (pwiImage != null) {
//            	pwiImageName = pwiImage.getImageFileName();
//            } else {
//            	pwiImageName = "No PWI included in processing";
//            }
            
            // save core stats to tab-delmited file
            String flairImageName;
            if (flairImage != null) {
                flairImageName = flairImage.getImageFileName();
            } else {
                flairImageName = "No FLAIR image included in processing";
            }
            if (!saveCoreStats(coreOutputDir, dwiImage.getImageFileName(), adcImage.getImageFileName(), flairImageName, adcImage)) {
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
            
            if (unregSkullMaskImg != null) {
                unregSkullMaskImg.disposeLocal();
                unregSkullMaskImg = null;
            }
            
            // temporarily disabling PWI processing
//            if (pwiArtifactMaskImg != null) {
//                pwiArtifactMaskImg.disposeLocal();
//                pwiArtifactMaskImg = null;
//            }
//            
//            if (pwiVentricleMaskImg != null) {
//                pwiVentricleMaskImg.disposeLocal();
//                pwiVentricleMaskImg = null;
//            }
            
            if (unregVentrImg != null) {
                unregVentrImg.disposeLocal();
                unregVentrImg = null;
            }
            
            if (segImg != null) {
                segImg.disposeLocal();
            }
            
            if (filteredADCImg != null) {
                filteredADCImg.disposeLocal();
            }
            
            // temporarily disabling PWI processing
//            if (TmaxRegImage != null) {
//            	TmaxRegImage.disposeLocal();
//            }
//            
//            if (TmaxUnregImage != null) {
//                TmaxUnregImage.disposeLocal();
//            }
            
            if (dwiFindObjImg != null) {
                dwiFindObjImg.disposeLocal(false);
                dwiFindObjImg = null;
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
            
            if (unregSkullMaskImg != null) {
                unregSkullMaskImg.disposeLocal();
                unregSkullMaskImg = null;
            }
            
            // temporarily disabling PWI processing
//            if (pwiArtifactMaskImg != null) {
//                pwiArtifactMaskImg.disposeLocal();
//                pwiArtifactMaskImg = null;
//            }
//            
//            if (pwiVentricleMaskImg != null) {
//                pwiVentricleMaskImg.disposeLocal();
//                pwiVentricleMaskImg = null;
//            }
            
            if (unregVentrImg != null) {
                unregVentrImg.disposeLocal();
                unregVentrImg = null;
            }
            
            if (dwiLightbox != null) {
                dwiLightbox.disposeLocal();
                dwiLightbox = null;
            }
            
            if (filteredADCImg != null) {
                filteredADCImg.disposeLocal();
                filteredADCImg = null;
            }
            
            // temporarily disabling PWI processing
//            if (TmaxRegImage != null) {
//            	TmaxRegImage.disposeLocal();
//            }
//            
//            if (TmaxUnregImage != null) {
//                TmaxUnregImage.disposeLocal();
//            }
            
            if (dwiFindObjImg != null) {
                dwiFindObjImg.disposeLocal(false);
                dwiFindObjImg = null;
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
        flairImage = null;
        
        // temporarily disabling PWI processing
//        pwiImage = null;
//        if (tspAlgo != null) {
//            tspAlgo.finalize();
//        }
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
        coreImage = (ModelImage) segImg.clone();
        
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
                coreImage.importData(0, removedBuffer, true);
            } catch (IOException error) {
                if (coreImage != null) {
                    coreImage.disposeLocal();
                }
                
                threshBuffer = null;
                displayError("Error on mirrored removed data importData: " + adcImage.getImageName());
                setCompleted(false);
                return null;
            }
            
            saveImageFile(coreImage, coreOutputDir, outputBasename + "_ADC_thresh_removed_init_pass" + passNum, FileUtility.XML);
        }
        
        try {
            coreImage.importData(0, threshBuffer, true);
        } catch (IOException error) {
            if (coreImage != null) {
                coreImage.disposeLocal();
            }
            
            threshBuffer = null;
            displayError("Error on cleaned segmentation importData: " + adcImage.getImageName());
            setCompleted(false);
            return null;
        }
        
        // perform closing on threshold mask
        close(coreImage, threshCloseIter, threshCloseSize, true);
        
        try {
            coreImage.exportData(0, volLength, threshBuffer);
        } catch (IOException error) {
            if (coreImage != null) {
                coreImage.disposeLocal();
            }
            
            threshBuffer = null;
            displayError("Error on closed ADC threshold export: " + adcImage.getImageName());
            setCompleted(false);
            return null;
        }
        
        saveImageFile(coreImage, coreOutputDir, outputBasename + "_ADC_thresh_close_pass" + passNum, FileUtility.XML);
        
        Vector<MaskObject> selectedObjectList = new Vector<MaskObject>();
        
        short[] objectBuffer = chooseCoreObjects(coreImage, threshBuffer, removedBuffer, passNum, useDistanceSelection, selectedObjectList);
        
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
            coreImage.importData(0, objectBuffer, true);
        } catch (IOException error) {
            if (coreImage != null) {
                coreImage.disposeLocal();
            }
            
            threshBuffer = null;
            objectBuffer = null;
            displayError("Error on adc threshold importData: " + adcImage.getImageName());
            setCompleted(false);
            return null;
        }
        
        saveImageFile(coreImage, coreOutputDir, outputBasename + "_ADC_thresh_only_largest_pass" + passNum, FileUtility.XML);
        
        // combine core mask with ADC and save lightbox
        ModelImage coreLightbox = generateLightbox(lightboxBgImg, coreImage, coreLightboxColor, lightboxOpacity, false);
        
        lightboxFile = saveImageFile(coreLightbox, coreOutputDir, outputBasename + "_ADC_core_lightbox_pass" + passNum, FileUtility.PNG);
        
        if (lightboxFile != null) {
            lightboxObjectTable.put(lightboxFile, selectedObjectList);
        }
        
        coreLightbox.disposeLocal();
        
//        if (threshImg != null) {
//            threshImg.disposeLocal();
//        }
        
        return lightboxFile;
    }
    
    public short[] chooseCoreObjects(final ModelImage img, final short[] imgBuffer, final short[] removedBuffer, int passNum, boolean useDistanceSelection, Vector<MaskObject> selectedObjectList) {
        short[] processBuffer = new short[imgBuffer.length];
        dwiSortedObjects = findObjects(img, imgBuffer, processBuffer, minAdcObjectSize, maxAdcObjectSize);
        
        FileInfoBase fileInfo1;
        dwiFindObjImg = new ModelImage(ModelStorageBase.UBYTE, img.getExtents(), "find_objects");
        fileInfo1 = (FileInfoBase) img.getFileInfo()[0].clone();
        fileInfo1.setResolutions(img.getResolutions(0).clone());
        fileInfo1.setUnitsOfMeasure(img.getUnitsOfMeasure().clone());
        fileInfo1.setAxisOrientation(img.getFileInfo(0).getAxisOrientation().clone());
        fileInfo1.setImageOrientation(img.getFileInfo(0).getImageOrientation());
        fileInfo1.setOrigin(img.getFileInfo(0).getOrigin().clone());
        fileInfo1.setSliceThickness(img.getFileInfo(0).getSliceThickness());
        for (int i = 0; i < img.getExtents()[2]; i++) {
            dwiFindObjImg.setFileInfo(fileInfo1, i);
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
                dwiFindObjImg.importData(0, removedBuffer, true);
            } catch (IOException error) {
                if (dwiFindObjImg != null) {
                    dwiFindObjImg.disposeLocal();
                }
                
                dwiFindObjImg = null;
                displayError("Error on mirrored removed data importData: " + adcImage.getImageName());
                setCompleted(false);
                return null;
            }
            
            saveImageFile(dwiFindObjImg, coreOutputDir, outputBasename + "_ADC_thresh_removed_pass" + passNum, FileUtility.XML);
        }
        
        try {
            dwiFindObjImg.importData(0, processBuffer, true);
        } catch (IOException error) {
            processBuffer = null;
            displayError("Error on importData: " + dwiFindObjImg.getImageName());
            setCompleted(false);
            return null;
        }
        
        saveImageFile(dwiFindObjImg, coreOutputDir, outputBasename + "_find_objects_pass" + passNum, FileUtility.XML);
        
        // last object should be the largest
        if (dwiSortedObjects.length > 0) {
            // check the number of core pixels in the top 5 objects and select the one with the most
            int numObjectsToCheckCore = dwiSortedObjects.length;
            int[] coreSizeList = new int[numObjectsToCheckCore];
            
            for (int i = 0; i < processBuffer.length; i++) {
                for (int objNum = 0; objNum < numObjectsToCheckCore; objNum++) {
                    if (processBuffer[i] == dwiSortedObjects[dwiSortedObjects.length - 1 - objNum].id) {
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
                
                dwiSortedObjects[dwiSortedObjects.length - 1 - i].setCoreSize(coreSize);
                
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
                                if (processBuffer[x + (y * extents[0]) + (z * sliceSize)] == dwiSortedObjects[dwiSortedObjects.length - 1 - objNum].id) {
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
                    System.err.println("Object distance: " + dwiSortedObjects[dwiSortedObjects.length - 1 - objNum].id + "\t" + coreDistList[objNum]);
//                        coreDistList[objNum] = skullMaskImg.getFloat(sortedObjects[sortedObjects.length -1 - objNum].index);
                }
                
                int selectedObjectIndex = 0;
                MaskObject selectedObject = dwiSortedObjects[dwiSortedObjects.length - 1];
                System.err.println("Object core: " + dwiSortedObjects[dwiSortedObjects.length - 1].id + "\t" + dwiSortedObjects[dwiSortedObjects.length - 1].size + "\t" + coreSizeList[0] + "\t" + coreDistList[0]);
                for (int objNum = 1; objNum < numObjectsToCheckCore; objNum++) {
                    System.err.println("Object core: " + dwiSortedObjects[dwiSortedObjects.length - 1 - objNum].id + "\t" + dwiSortedObjects[dwiSortedObjects.length - 1 - objNum].size + "\t" + coreSizeList[objNum] + "\t" + coreDistList[objNum]);
                    if ((coreSizeList[objNum] * coreSelectionSizeWeight - coreDistList[objNum] * coreSelectionDistWeight) > (coreSizeList[selectedObjectIndex] * coreSelectionSizeWeight - coreDistList[selectedObjectIndex] * coreSelectionDistWeight)) {
                        selectedObjectIndex = objNum;
                        selectedObject = dwiSortedObjects[dwiSortedObjects.length - 1 - objNum];
                    }
                }
                
                selectedObjectList.add(selectedObject);
                System.err.println("Selected object: " + selectedObject.id + "\t" + selectedObject.size + "\t" + coreDistList[selectedObjectIndex]);
            } else {
                int selectedObjectIndex = 0;
                MaskObject selectedObject = dwiSortedObjects[dwiSortedObjects.length - 1];
                System.err.println("Object core: " + dwiSortedObjects[dwiSortedObjects.length - 1].id + "\t" + dwiSortedObjects[dwiSortedObjects.length - 1].size + "\t" + coreSizeList[0]);
                for (int objNum = 1; objNum < numObjectsToCheckCore; objNum++) {
                    System.err.println("Object core: " + dwiSortedObjects[dwiSortedObjects.length - 1 - objNum].id + "\t" + dwiSortedObjects[dwiSortedObjects.length - 1 - objNum].size + "\t" + coreSizeList[objNum]);
                    if (coreSizeList[objNum] > coreSizeList[selectedObjectIndex]) {
                        selectedObjectIndex = objNum;
                        selectedObject = dwiSortedObjects[dwiSortedObjects.length - 1 - objNum];
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
            int nextObjectIndex = dwiSortedObjects.length - 1;
            MaskObject nextObject;
            while (nextObjectIndex >= 0) {
                nextObject = dwiSortedObjects[nextObjectIndex];
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
        
        public Hashtable<Integer, Integer> tmaxSizeMap = new Hashtable<Integer, Integer>();
        
        public double SIRValue = 0;
        
        public boolean isDwi = false;
        
        public boolean isPerfusion = false;
        
        public boolean isSIR = false;

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
        	isDwi = true;
        	isPerfusion = false;
        	isSIR = false;
        }
        
        public void setSIRNoObject() {
            id = -1;
            index = -1;
            size = -1;
            coreSize = -1;
            isDwi = false;
            isPerfusion = false;
            isSIR = true;
        }
        
        public void setCoreSize(final int core) {
            coreSize = core;
        }
        
        public void setPerfusionSize(final int tmaxVal, final int size) {
            isPerfusion = true;
        	tmaxSizeMap.put(tmaxVal, size);
        }
        
        public void setSIRValue(final double sir) {
            SIRValue = sir;
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
    
    // TODO remove PWI, add SIR
    private boolean saveCoreStats(final String outputDir, final String dwiFile, final String adcFile, final String flairFile, final ModelImage img) {
        //double voxelSize = imgResol[0] * imgResol[1] * imgResol[2];
        double ccFactor = getResolutionFactorCC(img);
        
        final String statsFile = outputDir + File.separator + outputBasename + "_stats.table";
        
        //final String volUnitsStr = adcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();
        
        final String volUnitsStr = "CC";
        
        CSVPrinter csvPrinter = null;
        BufferedWriter bw = null;
        FileWriter fw = null;
        try {
            fw = new FileWriter(statsFile);
            bw = new BufferedWriter(fw);
            
//            csvPrinter = new CSVPrinter(bw, CSVFormat.TDF.withHeader("Base Dir", "DWI File", "ADC File", "PWI File",
//            		"Core Volume " + volUnitsStr,
//            		"Core Volume using Tmax " + volUnitsStr,
//            		"Tmax > " + pwiThreshList[0] + "s Volume",
//            		"Tmax > " + pwiThreshList[1] + "s Volume",
//            		"Tmax > " + pwiThreshList[2] + "s Volume",
//            		"Tmax > " + pwiThreshList[3] + "s Volume",
//            		"Hypoperfusion Index (Tmax > 10s / Tmax > 6s)",
//            		"Mismatch (Tmax > 6s - core) " + volUnitsStr, "Mismatch Ratio (Tmax > 6s / core)",
//            		"Mismatch (Tmax > 6s - core using Tmax) " + volUnitsStr, "Mismatch Ratio (Tmax > 6s / core using Tmax)"));
            
            csvPrinter = new CSVPrinter(bw, CSVFormat.TDF.withHeader("Base Dir", "DWI File", "ADC File", "FLAIR File",
                    "Core Volume " + volUnitsStr,
                    "SIR"));
            
//            double[] perfusionSize = new double[pwiThreshList.length];
            double dwiCore = 0;
//            double pwiCore = 0;
            double sirValue = 0;
            
            // pass 1 = DWI/ADC ; pass 2 = perfusion ; pass 3 = DWI+Tmax/ADC
            for (int i = 0; i < getLightboxFileList().size(); i++) {
                int passNum = i + 1;
                
                int coreSize = getCoreSize(getLightboxFileList().get(i));

                if (isSIRLightbox(getLightboxFileList().get(i))) {
                    //System.err.println("file:\t" + getLightboxFileList().get(i));
//                    for (int perfIndex = pwiThreshList.length - 1; perfIndex >= 0; perfIndex--) {
//                        int perfSize = getPerfusionSize(getLightboxFileList().get(i), pwiThreshList[perfIndex]);
//                        
//                        // add in previous higher thresholds, if we're not at the highest threshold already
//                        if (perfIndex + 1 < pwiThreshList.length) {
//                            perfSize += perfusionSize[perfIndex + 1];
//                        }
//                        
//                        if (perfSize > 0) {
//                            perfusionSize[perfIndex] = perfSize;
//                            //System.err.println("perfSize " + perfIndex + " " + pwiThreshList[perfIndex] + ":\t" + perfSize);
//                        }
//                    }
                    
                    for (MaskObject obj : lightboxObjectTable.get(getLightboxFileList().get(i))) {
                        if (obj.SIRValue != 0) {
                            sirValue = obj.SIRValue;
                            break;
                        }
                    }
                } else {
                    if (coreSize > 0) {
                        if (passNum == 1) {
                        	dwiCore = coreSize;
                        } else {
                        	System.err.println("Unexpected core pass value: " + passNum + " " + coreSize);
                        }
                    }
                }
            }
            
            double dwiCoreVol = dwiCore * ccFactor;
            
            System.err.println("CoreDWI/SIR:\t" + dwiCore + "\t" + sirValue);
            csvPrinter.printRecord(outputDir, dwiFile, adcFile, flairFile, dwiCoreVol, sirValue);
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
    
    // temporarily disabling PWI processing
//    private ModelImage generateLightbox(final ModelImage imgA, final ModelImage mask, float blendingOpacity, boolean isTmax, TransMatrix transform) {
//        ModelImage lightbox = null;
//        
//        ModelImage newRGB;
//        if (imgA.isColorImage()) {
//            newRGB = (ModelImage) imgA.clone();
//        } else {
//            if (!isTmax) {
//                newRGB = new ModelImage(ModelImage.ARGB, imgA.getExtents(), imgA.getImageName());
//                final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(imgA, imgA, imgA, newRGB, true, true, 255.0f, true);
//                mathAlgo.run();
//            } else {
//                float tmaxWin = 10;
//                float tmaxLev = 1;
//                
//                ModelLUT tmaxLUT = new ModelLUT();
//                TransferFunction tf = new TransferFunction();
//                float[] tfX = new float[] {0, 0, 6, (int)imgA.getMax()};
//                float[] tfY = new float[] {255, 153, 0, 0};
////              float[] tfX = new float[4];
////              float[] tfY = new float[4];
////              JDialogWinLevel.calcWinLevTransferFunction(imgA, tmaxWin, tmaxLev, tfX, tfY);
//                tf.importArrays(tfX, tfY, tfX.length);
//                tmaxLUT.setTransferFunction(tf);
//                
//                ModelImage newImg = (ModelImage) imgA.clone();
//                
//                for (int i = 0; i < imgA.getSize(); i++) {
//                    int indexA = (int) (tf.getRemappedValue(imgA.getInt(i), (int)imgA.getMax()) + 0.5f);
//                    newImg.set(i, indexA);
//                }
//                
//                newImg.calcMinMax();
//                
//                //AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(newImg, ModelImage.UBYTE, newImg.getMin(), newImg.getMax(), 0, 255, false);
//                //changeTypeAlgo.run();
//                
//                //saveImageFile(newImg, coreOutputDir, outputBasename + "_PWI_Tmax_reg_rgb_remap", FileUtility.XML);
//                
//                newRGB = new ModelImage(ModelImage.ARGB, newImg.getExtents(), newImg.getImageName());
//                final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(newImg, newImg, newImg, newRGB, true, true, 255.0f, true, true);
//                mathAlgo.run();
//                
//                //saveImageFile(newRGB, coreOutputDir, outputBasename + "_PWI_Tmax_reg_rgb", FileUtility.XML);
//                
//                newImg.disposeLocal();
//                
////              ViewJFrameImage frame = new ViewJFrameImage(newRGB);
////              frame.setActiveImage(ViewJFrameImage.IMAGE_A);
//            }
//        }
//        
//        if (mask != null) {
//            // set any values to fully red if set in the mask
//            for (int i = 0; i < mask.getDataSize(); i++) {
//                int val = mask.getInt(i);
//                
//                if (val > 0 && mapTmaxColor(val) != null) {
//                    Color c = mapTmaxColor(val);
//                    
//                    // TODO set value with opacity blending
//                    newRGB.setC(i, 1, c.getRed());
//                    newRGB.setC(i, 2, c.getGreen());
//                    newRGB.setC(i, 3, c.getBlue());
//                }
//            }
//        }
//        
//        saveImageFile(newRGB, coreOutputDir, outputBasename + "_pwi_rgb_lightbox_unreg", FileUtility.XML);
//        
//        ModelImage lightboxReg = transformImage(newRGB, adcImage, transform);
//        saveImageFile(lightboxReg, coreOutputDir, outputBasename + "_pwi_rgb_lightbox_reg", FileUtility.XML);
//        
//        // TODO change based on x/y dim size
//        final int zoomPercent = 125;
//        
//        final int columns = 5;
//        int rows;
//        if (newRGB.getExtents()[2] % 5 == 0) {
//            rows = newRGB.getExtents()[2] / 5;
//        } else {
//            rows = (newRGB.getExtents()[2] / 5) + 1;
//        }
//        
//        final int rBorderVal = 0;
//        final int gBorderVal = 0;
//        final int bBorderVal = 0;
//        final int borderThick = 0;
//        
//        int startSlice = 0;
//        int endSlice = imgA.getExtents()[2] - 1;
//        LightboxGenerator lightGen;
//
//        try {
//            lightGen = new LightboxGenerator(newRGB, startSlice, endSlice, zoomPercent, rows, columns, rBorderVal, gBorderVal, bBorderVal, false, borderThick);
//            lightGen.run();
//            lightbox = lightGen.getImage();
//            
//            lightbox.calcMinMax();
//        } catch (final Exception e) {
//            e.printStackTrace();
//        }
//        
//        newRGB.disposeLocal();
//        lightboxReg.disposeLocal();
//        
//        return lightbox;
//    }
    
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
        final int columns = 5;
        int rows;
        if (newRGB.getExtents()[2] % 5 == 0) {
            rows = newRGB.getExtents()[2] / 5;
        } else {
            rows = (newRGB.getExtents()[2] / 5) + 1;
        }
        
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
    
    private ModelImage generateLightbox(final ModelImage imgA, final ModelImage mask1, Color mask1Color, final ModelImage mask2, Color mask2Color, float blendingOpacity, boolean isTmax) {
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
//              float[] tfX = new float[4];
//              float[] tfY = new float[4];
//              JDialogWinLevel.calcWinLevTransferFunction(imgA, tmaxWin, tmaxLev, tfX, tfY);
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
                
//              ViewJFrameImage frame = new ViewJFrameImage(newRGB);
//              frame.setActiveImage(ViewJFrameImage.IMAGE_A);
            }
        }
        
        if (mask1 != null) {
            // set any values to fully red if set in the mask
            for (int i = 0; i < mask1.getDataSize(); i++) {
                if (mask1.getBoolean(i) == true) {
                    // TODO set value with opacity blending

                    newRGB.setC(i, 1, (newRGB.getC(i, 1).intValue() * (1 - blendingOpacity)) + (mask1Color.getRed() * blendingOpacity));
                    newRGB.setC(i, 2, (newRGB.getC(i, 2).intValue() * (1 - blendingOpacity)) + (mask1Color.getGreen() * blendingOpacity));
                    newRGB.setC(i, 3, (newRGB.getC(i, 3).intValue() * (1 - blendingOpacity)) + (mask1Color.getBlue() * blendingOpacity));
                }
            }
        }
        
        if (mask2 != null) {
            // set any values to fully red if set in the mask
            for (int i = 0; i < mask2.getDataSize(); i++) {
                if (mask2.getBoolean(i) == true) {
                    // TODO set value with opacity blending
                    
                    newRGB.setC(i, 1, (newRGB.getC(i, 1).intValue() * (1 - blendingOpacity)) + (mask2Color.getRed() * blendingOpacity));
                    newRGB.setC(i, 2, (newRGB.getC(i, 2).intValue() * (1 - blendingOpacity)) + (mask2Color.getGreen() * blendingOpacity));
                    newRGB.setC(i, 3, (newRGB.getC(i, 3).intValue() * (1 - blendingOpacity)) + (mask2Color.getBlue() * blendingOpacity));
                }
            }
        }
        
        // TODO change based on x/y dim size
        final int zoomPercent = 125;
        
        // TODO change based on slice num
        final int columns = 5;
        int rows;
        if (newRGB.getExtents()[2] % 5 == 0) {
            rows = newRGB.getExtents()[2] / 5;
        } else {
            rows = (newRGB.getExtents()[2] / 5) + 1;
        }
        
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
    
    public Hashtable<Integer, Hashtable<File, Double>> getTmaxTables() {
        Hashtable<Integer, Hashtable<File, Double>> tmaxTable = new Hashtable<Integer, Hashtable<File, Double>>();
        for (int thresh : pwiThreshList) {
            tmaxTable.put(thresh, getPerfusionObjectSizeTable(thresh));
        }
        return tmaxTable;
    }
    
    public Hashtable<File, Double> getPerfusionObjectSizeTable(final int tmax) {
        Hashtable<File, Double> sizeTable = new Hashtable<File, Double>();
        for (File file : lightboxObjectTable.keySet()) {
            sizeTable.put(file, Double.valueOf(getPerfusionSize(file, tmax)));
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
    
    public int getPerfusionSize(File lightboxFile, int tmax) {
    	int totalSize = 0;
    	for (MaskObject obj : lightboxObjectTable.get(lightboxFile)) {
    	    if (obj != null && obj.tmaxSizeMap != null && obj.tmaxSizeMap.get(tmax) != null) {
    	        totalSize += obj.tmaxSizeMap.get(tmax);
    	    }
        }
        
        return totalSize;
    }
    
    public boolean isPerfusionLightbox(File lightboxFile) {
        for (MaskObject obj : lightboxObjectTable.get(lightboxFile)) {
            if (obj.isPerfusion) {
                return true;
            }
        }
        
        return false;
    }
    
    public boolean isSIRLightbox(File lightboxFile) {
        for (MaskObject obj : lightboxObjectTable.get(lightboxFile)) {
            if (obj.isSIR) {
                return true;
            }
        }
        
        return false;
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
    
    // temporarily disabling PWI processing
//    private File processPwi(ModelImage pwiImg) {
//        long tspStartTime = System.currentTimeMillis();
//        
//        boolean calculateMaskingThreshold = true;
//        int masking_threshold = 600;
//        double TSP_threshold = 0.8;
//        int TSP_iter = 4;
//        double Psvd = 0.1;
//        boolean autoAIFCalculation = true;
//        boolean calculateBounds = false;
//        
//        int search = PlugInAlgorithmTSPAnalysis.ELSUNC_2D_SEARCH;
//        
//        tspAlgo = new PlugInAlgorithmTSPAnalysis(pwiImg, doPwiSpatialSmoothing, pwiSigmax, pwiSigmay, calculateMaskingThreshold, masking_threshold,
//                TSP_threshold, TSP_iter, Psvd, autoAIFCalculation, doPwiMultiThreading, search, doPwiCalculateCorrelation, doPwiCalculateCBFCBVMTT, calculateBounds);
//        
//        tspAlgo.setSaveAllOutputs(doPwiSaveOutputs);
//    	tspAlgo.setOutputFilePath(coreOutputDir);
//        
//        tspAlgo.setOutputPrefix(new File(coreOutputDir).getName() + "_" + outputLabel + "_PWI_");
//        
//        linkProgressToAlgorithm(tspAlgo);
//        tspAlgo.setProgressValues(generateProgressValues(35, 70));
//        
//        tspAlgo.setRunningInSeparateThread(false);
//        
//        tspAlgo.run();
//        
//        TmaxUnregImage = tspAlgo.getTmaxImage();
//        
//        saveImageFile(TmaxUnregImage, coreOutputDir, outputBasename + "_PWI_Tmax", FileUtility.XML);
//        
//        System.err.println("PWI TSP Algo time elapsed: " + (System.currentTimeMillis() - tspStartTime) / 1000.0f);
//        
//        long regStartTime = System.currentTimeMillis();
//        
//        // register PWI seg to ADC since it is lower res
//        ModelImage firstPwiVol = getFirstVolume(pwiImg);
//        
//        tmaxToAdcTransform = getRegistrationTransform(firstPwiVol, adcImage, TmaxUnregImage);
//        tmaxToAdcTransform.saveMatrix(this.coreOutputDir + File.separator + outputBasename + "_matrix.mat");
//        adcToTmaxTransform = tmaxToAdcTransform.clone();
//        adcToTmaxTransform.Inverse();
//        
//        //TmaxRegImage = transformImage(TmaxUnregImage, adcImage, transform);
//        //saveImageFile(TmaxRegImage, coreOutputDir, outputBasename + "_PWI_Tmax_reg", FileUtility.XML);
//        
//        System.err.println("PWI seg registration time elapsed: " + (System.currentTimeMillis() - regStartTime) / 1000.0f);
//
//        if (doArtifactCleanupWithMean) {
//            //unregMaskImg = getPwiArtifactMask(firstPwiVol);
//            //pwiArtifactMaskImg = transformImage(unregMaskImg, adcImage, transform);
//            //saveImageFile(pwiArtifactMaskImg, coreOutputDir, outputBasename + "_pwi_brain_mask", FileUtility.XML);
//            
//            //pwiVentricleMaskImg = getPwiVentricleMask(pwiArtifactMaskImg);
//            pwiVentricleMaskImg = getPwiVentricleMask(skullMaskImg);
//            unregVentrImg = transformImage(pwiVentricleMaskImg, TmaxUnregImage, adcToTmaxTransform);
//            saveImageFile(unregVentrImg, coreOutputDir, outputBasename + "_vent_unreg", FileUtility.XML);
//        }
//        
//        firstPwiVol.disposeLocal();
//        
//        unregSkullMaskImg = transformImage(skullMaskImg, TmaxUnregImage, adcToTmaxTransform);
//        saveImageFile(unregSkullMaskImg, coreOutputDir, outputBasename + "_skull_unreg", FileUtility.XML);
//        
//        //ModelImage pwiSegImg = getTmaxSeg(TmaxRegImage, pwiVentricleMaskImg, pwiArtifactMaskImg, pwiThreshold);
//        //ModelImage pwiSegImg = getTmaxSeg(TmaxRegImage, pwiVentricleMaskImg, skullMaskImg, pwiThreshList);
//        ModelImage pwiSegImg = getTmaxSeg(TmaxUnregImage, unregVentrImg, unregSkullMaskImg, pwiThreshList);
//        
//        saveImageFile(TmaxUnregImage, coreOutputDir, outputBasename + "_pwi_seg_multi_unreg", FileUtility.XML);
////        TmaxRegImage = transformImage(TmaxUnregImage, adcImage, transform);
////        saveImageFile(TmaxRegImage, coreOutputDir, outputBasename + "_pwi_seg_multi_reg", FileUtility.XML);
//        
//        cleanupMask(pwiSegImg, doPerfusionSymmetryRemoval, minPerfusionObjectSize);
//        
//        // combine perfusion mask with Tmax and save lightbox
//        ModelImage perfLightbox = generateLightbox(TmaxUnregImage, pwiSegImg, lightboxOpacity, true, tmaxToAdcTransform);
//        
//        File lightboxFile = saveImageFile(perfLightbox, coreOutputDir, outputBasename + "_Tmax_perfusion_lightbox", FileUtility.PNG, false);
//        
//        if (lightboxFile != null) {
//            int[] tmaxSizes = new int[pwiThreshList.length];
//        	int totalSize = 0;
//        	for (int i = 0; i < pwiSegImg.getDataSize(); i++) {
//        	    for (int threshIndex = 0; threshIndex < pwiThreshList.length; threshIndex++) {
//        	        int thresh = pwiThreshList[threshIndex];
//        	        if (pwiSegImg.getInt(i) == thresh) {
//        	            tmaxSizes[threshIndex]++;
//        	            totalSize++;
//        	        }
//        	    }
//        	}
//        	
//        	Vector<MaskObject> maskList = new Vector<MaskObject>();
//            MaskObject obj = new MaskObject(0, (short) 0, totalSize);
//            for (int threshIndex = 0; threshIndex < pwiThreshList.length; threshIndex++) {
//                obj.setPerfusionSize(pwiThreshList[threshIndex], tmaxSizes[threshIndex]);
//            }
//            maskList.add(obj);
//            
//            lightboxObjectTable.put(lightboxFile, maskList);
//        }
//        
//        perfLightbox.disposeLocal();
//        
//        if (pwiSegImg != null) {
//        	pwiSegImg.disposeLocal();
//        }
//
//        return lightboxFile;
//    }
//    
//    private ModelImage getTmaxSeg(ModelImage tmaxImg, ModelImage ventricleImg, ModelImage artifactImg, int thresh) {
//    	final int[] extents = tmaxImg.getExtents();
//        final int sliceLength = extents[0] * extents[1];
//        final int volLength = sliceLength * extents[2];
//        final FileInfoBase fInfo = (FileInfoBase) tmaxImg.getFileInfo(0).clone();
//        
//        ModelImage pwiSegImg = new ModelImage(ModelImage.UBYTE, tmaxImg.getExtents(), tmaxImg.getImageName() + "_seg_" + thresh + "s");
//        for (int i = 0; i < pwiSegImg.getExtents()[2]; i++) {
//            pwiSegImg.setFileInfo(fInfo, i);
//        }
//        
//        for (int i = 0; i < volLength; i++) {
//            if ((ventricleImg != null && ventricleImg.getBoolean(i) == true) && (artifactImg != null && artifactImg.getBoolean(i) == true)) {
//                if (tmaxImg.getInt(i) > thresh) {
//                    pwiSegImg.set(i, 1);
//                } else {
//                    pwiSegImg.set(i, 0);
//                }
//            }
//        }
//        
//        saveImageFile(pwiSegImg, coreOutputDir, outputBasename + "_PWI_Tmax_thresh_" + thresh + "s", FileUtility.XML);
//        
//        return pwiSegImg;
//    }
//    
//    private ModelImage getTmaxSeg(ModelImage tmaxImg, ModelImage ventricleImg, ModelImage artifactImg, int[] threshList) {
//        final int[] extents = tmaxImg.getExtents();
//        final int sliceLength = extents[0] * extents[1];
//        final int volLength = sliceLength * extents[2];
//        final FileInfoBase fInfo = (FileInfoBase) tmaxImg.getFileInfo(0).clone();
//        
//        ModelImage pwiSegImg = new ModelImage(ModelImage.UBYTE, tmaxImg.getExtents(), tmaxImg.getImageName() + "_seg_multi_threshold");
//        for (int i = 0; i < pwiSegImg.getExtents()[2]; i++) {
//            pwiSegImg.setFileInfo(fInfo, i);
//        }
//        
//        Arrays.sort(threshList);
//        
//        for (int i = 0; i < volLength; i++) {
//            if ((ventricleImg != null && ventricleImg.getBoolean(i) == true) && (artifactImg != null && artifactImg.getBoolean(i) == true)) {
//                for (int thresh : threshList) {
//                    if (tmaxImg.getInt(i) > thresh) {
//                        pwiSegImg.set(i, thresh);
//                    }
//                }
//            }
//        }
//        
//        saveImageFile(pwiSegImg, coreOutputDir, outputBasename + "_PWI_Tmax_thresh_multi", FileUtility.XML);
//        
//        return pwiSegImg;
//    }
//    
//    private ModelImage getFirstVolume(ModelImage multiVolImg) {
//        int extents3Dorg[] = multiVolImg.getExtents();
//        int sliceLength = extents3Dorg[0] * extents3Dorg[1];
//        int xDim = extents3Dorg[0];
//        int yDim = extents3Dorg[1];
//        int zDim;
//        int tDim;
//        int units3D[] = new int[3];
//        int extents3D[] = new int[3];
//        float resolutions3D[] = new float[3];
//        
//        String tDimString;
//        
//        String delZString;
//        float delZ;
//        
//        extents3D[0] = xDim;
//        extents3D[1] = yDim;
//        
//        FileInfoDicom dicomInfo = (FileInfoDicom) multiVolImg.getFileInfo(0);
//        FileDicomTagTable tagTable = dicomInfo.getTagTable();
//        FileInfoDicom dicomInfoLast = (FileInfoDicom) multiVolImg.getFileInfo(extents3Dorg[2]-1);
//        FileDicomTagTable tagTableLast = dicomInfoLast.getTagTable();
//
//        //System.out.println("TE = " + TE);
//        if (tagTable.getValue("0020,0105") != null) {
//            // Number of temporal positions
//            FileDicomTag tag = tagTable.get(new FileDicomKey("0020,0105"));
//            tDimString = (String)tag.getValue(false);
//        }
//        else if (tagTableLast.getValue("0020,0012") != null) {
//            FileDicomTag tag = tagTableLast.get(new FileDicomKey("0020,0012"));
//            tDimString = (String)tag.getValue(false); 
//        }
//        else {
//            MipavUtil.displayError("Tags (0020,0012) and (0020,0105) are both null");
//            setCompleted(false);
//            return multiVolImg;
//        }
//        tDim = Integer.valueOf(tDimString.trim()).intValue();
//        if ((tDim <= 2) || (tDim > 500)) {
//            MipavUtil.displayError("Exiting with an impossible time dimension value of " + tDim);
//            setCompleted(false);
//            return multiVolImg;
//        }
//        zDim = extents3Dorg[2]/tDim;
//        if ((zDim <= 2) || (zDim > 500)) {
//            MipavUtil.displayError("Exiting with an impossible z dimension value of " + zDim);
//            setCompleted(false);
//            return multiVolImg;
//        }
//        extents3D[2] = zDim;
//        for (int i = 0; i < 2; i++) {
//            resolutions3D[i] = multiVolImg.getResolutions(0)[i];
//            //System.out.println("resolutions["+i+"] = " + resolutions[i]);
//        }
//        
//        if (tagTable.getValue("0018,0088") != null) {
//            // Spacing between slices in millimeters
//            FileDicomTag tag = tagTable.get(new FileDicomKey("0018,0088"));
//            delZString = (String)tag.getValue(false);
//        }
//        else {
//            MipavUtil.displayError("Tag (0018,0088) for Spacing between slices is null");
//            setCompleted(false);
//            return multiVolImg;
//        }
//        delZ = Float.valueOf(delZString.trim()).floatValue();
//        //System.out.println("delZ = " + delZ);
//        resolutions3D[2] = delZ;
//        //System.out.println("zDim = " + zDim + " tDim = " + tDim);
//
//        for (int i = 0; i < 3; i++) {
//            units3D[i] = Unit.MILLIMETERS.getLegacyNum();
//        }
//        
//        short[] sliceBuffer = new short[sliceLength];
//        
//        ModelImage firstVol = new ModelImage(multiVolImg.getType(), extents3D, multiVolImg.getImageName() + "_firstVol");
//        firstVol.setResolutions(resolutions3D);
//        // TODO units? others?
//        
//        int zDest = 0; // start counting the slices of the destination image at the first slice.
//
//        int multiVolZDim = extents3Dorg[2];
//        
//        String acquisNumFirst = (String)((FileInfoDicom) multiVolImg.getFileInfo(0)).getTagTable().get("0020,0012").getValue(false);
//        String temporalPosNumFirst = null;
//        if (((FileInfoDicom) multiVolImg.getFileInfo(0)).getTagTable().get("0020,0100") != null) {
//            temporalPosNumFirst = (String)((FileInfoDicom) multiVolImg.getFileInfo(0)).getTagTable().get("0020,0100").getValue(false);
//        }
//        
//        for (int zSrc = 0; (zSrc < multiVolZDim) && !threadStopped; zSrc++) {
//        	String acquisNum = (String)((FileInfoDicom) multiVolImg.getFileInfo(zSrc)).getTagTable().get("0020,0012").getValue(false);
//        	String temporalPosNum = null;
//        	if (((FileInfoDicom) multiVolImg.getFileInfo(zSrc)).getTagTable().get("0020,0100") != null) {
//        	    temporalPosNum = (String)((FileInfoDicom) multiVolImg.getFileInfo(zSrc)).getTagTable().get("0020,0100").getValue(false);
//        	}
//        	
//        	// prefer temporal position indicator, but fall-back to acquision number since not all PWI vols have 0020,0100
//        	if (temporalPosNum != null) {
//        	    if (temporalPosNum.trim().equals(temporalPosNumFirst.trim())) {
//    	            try {
//    	                // try copying the zSrc slice out of srcImage, making it the zDest in destImage
//    	                multiVolImg.exportSliceXY(zSrc, sliceBuffer);
//    	                firstVol.importData(zDest * sliceLength, sliceBuffer, false);
//    	                zDest++;
//    	            } catch (IOException e) {
//    	                MipavUtil.displayError("IOException on PWI first vol export/import");
//    	                setCompleted(false);
//    	                return multiVolImg;
//    	            }
//        	    }
//        	} else {
//        		if (acquisNum != null && acquisNum.trim().equals(acquisNumFirst.trim())) {
//    	            try {
//    	                // try copying the zSrc slice out of srcImage, making it the zDest in destImage
//    	                multiVolImg.exportSliceXY(zSrc, sliceBuffer);
//    	                firstVol.importData(zDest * sliceLength, sliceBuffer, false);
//    	                zDest++;
//    	            } catch (IOException e) {
//    	                MipavUtil.displayError("IOException on PWI first vol export/import");
//    	                setCompleted(false);
//    	                return multiVolImg;
//    	            }
//            	}
//        	}
//        	
//        	
//        }
//
//        saveImageFile(firstVol, coreOutputDir, outputBasename + "_PWI_firstVol", FileUtility.XML);
//        
//        return firstVol;
//    }
    
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
    
    private TransMatrix getRegistrationTransform(ModelImage movingImg, ModelImage refImg) {
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
    
    // temporarily disabling PWI processing
//    // get artifact mask from first PWI vol: voxels w/ vals > 50% of mean value of vol
//    private ModelImage getPwiArtifactMask(ModelImage firstPwiImg) {
//        ModelImage maskImg = new ModelImage(ModelStorageBase.BOOLEAN, firstPwiImg.getExtents(), "pwi_artifact_mask");
//        
//        final int[] extents = firstPwiImg.getExtents();
//        final int sliceLength = extents[0] * extents[1];
//        final int volLength = sliceLength * extents[2];
//        final FileInfoBase fInfo = (FileInfoBase) firstPwiImg.getFileInfo(0).clone();
//        
//        for (int i = 0; i < maskImg.getExtents()[2]; i++) {
//            maskImg.setFileInfo(fInfo, i);
//        }
//        
//        // calculate mean within the DWI-based skull mask - the whole volume was too low
//        double meanVal = 0;
//        long total = 0;
//        int voxelCount = 0;
//        for (int i = 0; i < volLength; i++) {
//            if (skullMaskImg.getBoolean(i) == true) {
//                total += firstPwiImg.getInt(i);
//                voxelCount++;
//            }
//        }
//        meanVal = total / voxelCount;
//        
//        double thresholdIntensity = meanVal * artifactMeanThreshold;
//
//        for (int i = 0; i < volLength; i++) {
//            if (firstPwiImg.getInt(i) >= thresholdIntensity) {
//                maskImg.set(i, 1);
//            } else {
//                maskImg.set(i, 0);
//            }
//        }
//        
//        saveImageFile(maskImg, coreOutputDir, outputBasename + "_pwi_brain_mask", FileUtility.XML);
//
//        // open - try to disconnect any artifacts from the brain mask
//        open(maskImg);
//
//        // close
////        close(maskImg, artifactCloseIter, artifactCloseSize, false);
//        
//     // fill holes
//        fillHoles(maskImg);
//
//        short[] maskBuffer = new short[volLength];
//        short[] processBuffer = new short[volLength];
//        try {
//            maskImg.exportData(0, volLength, maskBuffer);
//        } catch (IOException error) {
//            if (maskImg != null) {
//                maskImg.disposeLocal();
//                maskImg = null;
//            }
//
//            maskBuffer = null;
//            displayError("Error on brain mask export: " + firstPwiImg.getImageName());
//            setCompleted(false);
//            return null;
//        }
//
//        MaskObject[] objects = findObjects(maskImg, maskBuffer, processBuffer, 100, 10000000);
//
//        if (objects.length > 0) {
//            MaskObject largest = objects[objects.length - 1];
//            for (int i = 0; i < processBuffer.length; i++) {
//                if (processBuffer[i] == largest.id) {
//                    maskBuffer[i] = 1;
//                } else {
//                    maskBuffer[i] = 0;
//                }
//            }
//        }
//
//        try {
//            maskImg.importData(0, maskBuffer, true);
//        } catch (IOException error) {
//            if (maskImg != null) {
//                maskImg.disposeLocal();
//                maskImg = null;
//            }
//
//            maskBuffer = null;
//            displayError("Error on pwi brain mask importData");
//            setCompleted(false);
//            return null;
//        }
//
//        // dilate(maskImg, 0.5f);
//
//        //saveImageFile(maskImg, coreOutputDir, outputBasename + "_pwi_brain_mask", FileUtility.XML);
//        
//        return maskImg;
//    }
//    
//    private ModelImage getPwiVentricleMask(ModelImage fullBrainMask) {
//        // get largest object after ventricle threshold (+some dilation) and save it for later pwi masking
//        ModelImage ventMaskImg = new ModelImage(ModelStorageBase.BOOLEAN, fullBrainMask.getExtents(), "pwi_ventricle_mask");
//        
//        final int[] extents = fullBrainMask.getExtents();
//        final int sliceLength = extents[0] * extents[1];
//        final int volLength = sliceLength * extents[2];
//        final FileInfoBase fInfo = (FileInfoBase) fullBrainMask.getFileInfo(0).clone();
//        
//        for (int i = 0; i < ventMaskImg.getExtents()[2]; i++) {
//            ventMaskImg.setFileInfo(fInfo, i);
//        }
//        
//        // TODO
//        // calculate mean within the brain mask
//        double meanVal = 0;
//        long total = 0;
//        int voxelCount = 0;
//        for (int i = 0; i < volLength; i++) {
//            if (fullBrainMask.getBoolean(i) == true) {
//                total += dwiImage.getInt(i);
//                voxelCount++;
//            }
//        }
//        meanVal = total / voxelCount;
//        
//        double thresholdIntensity = meanVal * ventricleRemovalMeanThreshold;
//        
//        System.err.println("Ventricle threshold: mean = " + meanVal + " threshold = " + thresholdIntensity);
//        
//        for (int i = 0; i < volLength; i++) {
//            //if (fullBrainMask.getBoolean(i) == true && dwiImage.getInt(i) > ventricleRemovalMaskThreshold) {
//            if (fullBrainMask.getBoolean(i) == true && dwiImage.getInt(i) > thresholdIntensity) {
//                ventMaskImg.set(i, 1);
//            } else {
//                ventMaskImg.set(i, 0);
//            }
//        }
//        
//        // open - try to disconnect any artifacts from the brain mask
//        open(ventMaskImg);
//        
//        // close
//        //close(ventMaskImg, 2, 2f, false);
//        
//        short[] maskBuffer = new short[volLength];
//        short[] processBuffer = new short[volLength];
//        try {
//            ventMaskImg.exportData(0, volLength, maskBuffer);
//        } catch (IOException error) {
//            if (ventMaskImg != null) {
//                ventMaskImg.disposeLocal();
//                ventMaskImg = null;
//            }
//            
//            maskBuffer = null;
//            displayError("Error on ventricle mask export: " + dwiImage.getImageName());
//            setCompleted(false);
//            return fullBrainMask;
//        }
//        
//        MaskObject[] objects = findObjects(ventMaskImg, maskBuffer, processBuffer, 100, 10000000);
//        
//        if (objects.length > 0) {
//            MaskObject largest = objects[objects.length - 1];
//            for (int i = 0; i < processBuffer.length; i++) {
//                if (processBuffer[i] == largest.id) {
//                    maskBuffer[i] = 1;
//                } else {
//                    maskBuffer[i] = 0;
//                }
//            }
//        }
//        
//        try {
//            ventMaskImg.importData(0, maskBuffer, true);
//        } catch (IOException error) {
//            if (ventMaskImg != null) {
//                ventMaskImg.disposeLocal();
//                ventMaskImg = null;
//            }
//            
//            maskBuffer = null;
//            displayError("Error on pwi brain mask importData");
//            setCompleted(false);
//            return fullBrainMask;
//        }
//        
//        // TODO fill holes of ventMaskImg to get its outer boundary
//        ModelImage boundaryImg = (ModelImage) ventMaskImg.clone("pwi_ventricle_outer");
//        saveImageFile(boundaryImg, coreOutputDir, outputBasename + "_pwi_ventricle_mask_pre", FileUtility.XML);
//        close(boundaryImg, 2, 2f, false);
//        saveImageFile(boundaryImg, coreOutputDir, outputBasename + "_pwi_ventricle_mask_closed", FileUtility.XML);
//        fillHoles(boundaryImg);
//        saveImageFile(boundaryImg, coreOutputDir, outputBasename + "_pwi_ventricle_mask_filled", FileUtility.XML);
//        
//        // TODO find values included in the full brain mask, but not in the ventricle outer boundary (which we assume will be smaller)
//        for (int i = 0; i < volLength; i++) {
//            if (fullBrainMask.getBoolean(i) == true && boundaryImg.getBoolean(i) == false) {
//                ventMaskImg.set(i, 1);
//                boundaryImg.set(i, 1);
//            } else {
//                boundaryImg.set(i, 0);
//            }
//        }
//
//        saveImageFile(boundaryImg, coreOutputDir, outputBasename + "_pwi_ventricle_mask_add_bound", FileUtility.XML);
//        
//        //dilate(pwiBrainMaskImg, 0.5f);
//        
//        saveImageFile(ventMaskImg, coreOutputDir, outputBasename + "_pwi_ventricle_mask", FileUtility.XML);
//        
//        return ventMaskImg;
//    }
//    
//    private void cleanupMask(ModelImage maskImg, boolean doSymmetryRemoval, int minObjSize) {
//        final int[] extents = maskImg.getExtents();
//        final int sliceLength = extents[0] * extents[1];
//        final int volLength = sliceLength * extents[2];
//        
//        short[] removedBuffer = null;
//        short[] maskBuffer = null;
//        
//        // TODO cleanup mask by removing voxels that are symmetric across the image center
//        if (doSymmetryRemoval) {
//            if (removedBuffer == null) {
//                removedBuffer = new short[volLength];
//            }
//            
//            if (maskBuffer == null) {
//                maskBuffer = new short[volLength];
//                
//                try {
//                    maskImg.exportData(0, volLength, maskBuffer);
//                } catch (IOException error) {
//                    maskBuffer = null;
//                    displayError("Error on mask export for cleanup: " + maskImg.getImageName());
//                    setCompleted(false);
//                }
//            }
//            
//            // if values are mirrored across the the l->r of the image, cancel them out.  core values should only be on one side, while the cerebelum will have values on both sides
//            for (int iZ = 0; iZ < extents[2]; iZ++) {
//                for (int iY = 0; iY < extents[1]; iY++) {
//                    for (int iX = 0; iX < extents[0] / 2; iX++) {
//                        int index = (iZ * sliceLength) + (iY * extents[0]) + iX;
//                        int mirroredIndex = (iZ * sliceLength) + (iY * extents[0]) + (extents[0] - iX - 1);
//                        
//                        if (maskBuffer[index] > 0 && maskBuffer[mirroredIndex] > 0) {
//                            maskBuffer[index] = 0;
//                            maskBuffer[mirroredIndex] = 0;
//                            
//                            removedBuffer[index] = 1;
//                            removedBuffer[mirroredIndex] = 1;
//                        }
//                    }
//                }
//            }
//        }
//        
//        // remove objects below a certain size
//        if (minObjSize > 0) {
//            if (removedBuffer == null) {
//                removedBuffer = new short[volLength];
//            }
//            
//            if (maskBuffer == null) {
//                maskBuffer = new short[volLength];
//                
//                try {
//                    maskImg.exportData(0, volLength, maskBuffer);
//                } catch (IOException error) {
//                    maskBuffer = null;
//                    displayError("Error on mask export for cleanup: " + maskImg.getImageName());
//                    setCompleted(false);
//                }
//            }
//            
//            short[] finalBuffer = new short[volLength];
//            for (int i = 0; i < maskBuffer.length; i++) {
//                finalBuffer[i] = maskBuffer[i];
//            }
//            
//            short[] processBuffer = new short[volLength];
//    
//            MaskObject[] objects = findObjects(maskImg, maskBuffer, processBuffer, minObjSize, 10000000);
//    
//            if (objects.length > 0) {
//                for (int i = 0; i < processBuffer.length; i++) {
//                    boolean found = false;
//                    for (int curObjNum = 0; curObjNum < objects.length; curObjNum++) {
//                        if (processBuffer[i] == objects[curObjNum].id) {
//                            found = true;
//                            break;
//                        }
//                    }
//                    
//                    if (found) {
//                        maskBuffer[i] = 1;
//                        removedBuffer[i] = 0;
//                    } else {
//                        maskBuffer[i] = 0;
//                        removedBuffer[i] = 1;
//                    }
//                }
//            }
//            
//            for (int i = 0; i < maskBuffer.length; i++) {
//                if (maskBuffer[i] == 0) {
//                    finalBuffer[i] = 0;
//                }
//            }
//            maskBuffer = null;
//            maskBuffer = finalBuffer;
//            
//            try {
//                maskImg.importData(0, finalBuffer, true);
//            } catch (IOException error) {
//                maskBuffer = null;
//                displayError("Error on mask cleanup importData");
//                setCompleted(false);
//            }
//        }
//
//        if (removedBuffer != null) {
//            try {
//                maskImg.importData(0, removedBuffer, true);
//            } catch (IOException error) {
//                removedBuffer = null;
//                displayError("Error on mask cleanup importData");
//                setCompleted(false);
//            }
//            
//            saveImageFile(maskImg, coreOutputDir, outputBasename + "_mask_cleanup_removed", FileUtility.XML);
//            removedBuffer = null;
//            
//            if (maskBuffer != null) {
//                try {
//                    maskImg.importData(0, maskBuffer, true);
//                } catch (IOException error) {
//                    maskBuffer = null;
//                    displayError("Error on mask cleanup importData");
//                    setCompleted(false);
//                }
//            }
//        }
//        
//        saveImageFile(maskImg, coreOutputDir, outputBasename + "_mask_cleaned", FileUtility.XML);
//    }
//    
//    private static final Color mapTmaxColor(final int tmaxVal) {
//        if (tmaxVal == pwiThreshList[0]) {
//            return Color.BLUE;
//        } else if (tmaxVal == pwiThreshList[1]) {
//            return Color.GREEN;
//        } else if (tmaxVal == pwiThreshList[2]) {
//            return Color.YELLOW;
//        } else if (tmaxVal == pwiThreshList[3]) {
//                return Color.RED;
//        } else {
//            return null;
//        }
//    }
    
    private void processFlair() {
        // register dwi to flair
        dwiToFlairTransform = getRegistrationTransform(dwiImage, flairImage);
        ModelImage dwiReg = transformImage(dwiImage, flairImage, dwiToFlairTransform);
        saveImageFile(dwiReg, coreOutputDir, outputBasename + "_dwi_reg", FileUtility.XML);
        dwiReg.disposeLocal();
        
        // use transformation to also transform adc
        ModelImage adcReg = transformImage(adcImage, flairImage, dwiToFlairTransform);
        saveImageFile(adcReg, coreOutputDir, outputBasename + "_adc_reg", FileUtility.XML);
        
        ModelImage sirRegionReg;
        
        double resFactorCC = getResolutionFactorCC(dwiFindObjImg);
        
        System.out.println("SIR: " + dwiSortedObjects[dwiSortedObjects.length - 1].coreSize + "\t" + dwiSortedObjects[dwiSortedObjects.length - 1].size + "\t" + resFactorCC * dwiSortedObjects[dwiSortedObjects.length - 1].size);
        
        // if some core was found, use that for SIR. If no/small core, check largest object from DWI findObjects img and write to disk - if > 1 CC, use for SIR. Otherwise, no SIR.
        if (dwiSortedObjects.length > 0 && dwiSortedObjects[dwiSortedObjects.length - 1].coreSize > 0) {
            // use transformation to also transform core mask
            sirRegionReg = transformImage(coreImage, flairImage, dwiToFlairTransform);
        } else if (dwiSortedObjects.length > 0 && dwiSortedObjects[dwiSortedObjects.length - 1].coreSize <= 0 && (resFactorCC * dwiSortedObjects[dwiSortedObjects.length - 1].size) > dwiMinObjSizeSIR) {
            // use largest dwi object
            sirRegionReg = transformImage(dwiFindObjImg, flairImage, dwiToFlairTransform);
            
            final int[] extents = sirRegionReg.getExtents();
            final int sliceLength = extents[0] * extents[1];
            final int volLength = sliceLength * extents[2];
            
            int voxelCount = 0;
            for (int i = 0; i < volLength; i++) {
                if (sirRegionReg.getInt(i) == dwiSortedObjects[dwiSortedObjects.length - 1].id) {
                    voxelCount++;
                } else {
                    sirRegionReg.set(i, 0);
                }
            }
        } else {
            // no SIR calc
            adcReg.disposeLocal();
            return;
        }
        
        saveImageFile(sirRegionReg, coreOutputDir, outputBasename + "_sir_mask_reg", FileUtility.XML);

        final int[] extents = sirRegionReg.getExtents();
        final int sliceLength = extents[0] * extents[1];
        final int volLength = sliceLength * extents[2];
        
        // get average intensity of flair voxels inside core mask
        double flairCoreMeanVal = 0;
        long total = 0;
        int voxelCount = 0;
        for (int i = 0; i < volLength; i++) {
            if (sirRegionReg.getInt(i) > 0) {
                total += flairImage.getInt(i);
                voxelCount++;
            }
        }
        
        if (voxelCount > 0) {
            flairCoreMeanVal = total / voxelCount;
            
            // flip core mask across y axis
            ModelImage coreFlipImage = flipImageAcrossY(sirRegionReg);
            
            // get average intensity of flair voxels inside flipped mask
            double flairFlippedCoreMeanVal = 0;
            total = 0;
            voxelCount = 0;
            for (int i = 0; i < volLength; i++) {
                // remove flipped core mask voxels where the adc value is > 900 (outside brain and/or is csf)
                if (coreFlipImage.getInt(i) > 0 && adcReg.getInt(i) > adcBrainTissueTheshold) {
                    coreFlipImage.set(i, 0);
                } else if (coreFlipImage.getInt(i) > 0) {
                    total += flairImage.getInt(i);
                    voxelCount++;
                }
            }
            flairFlippedCoreMeanVal = total / voxelCount;
            
            saveImageFile(coreFlipImage, coreOutputDir, outputBasename + "_sir_mask_reg_flip", FileUtility.XML);
                    
            // SIR value = core avg. flair intensity / flipped core avg. flair intensity
            double sirValue = flairCoreMeanVal / flairFlippedCoreMeanVal;
            
            // generate lightbox with outline of core area in red (?) and flipped core in green (?)
            ModelImage sirLightbox = generateLightbox(flairImage, sirRegionReg, coreLightboxColor, coreFlipImage, flippedCoreLightboxColor, sirLightboxOpacity, false);
            
            File sirLightboxFile = saveImageFile(sirLightbox, coreOutputDir, outputBasename + "_SIR_lightbox", FileUtility.PNG, false);
            
            // attach SIR value for later stats output
            if (sirLightboxFile != null) {
                lightboxFileList.add(sirLightboxFile);
                Vector<MaskObject> maskList = new Vector<MaskObject>();
                MaskObject obj = new MaskObject(0, (short) 0, 0);
                obj.setSIRNoObject();
                obj.setSIRValue(sirValue);
                maskList.add(obj);
                lightboxObjectTable.put(sirLightboxFile, maskList);
            }
            
            sirLightbox.disposeLocal();
            coreFlipImage.disposeLocal();
            
            // divide flair image values by flipped core avg intensity and save image as SIRmap
            ModelImage sirMapImage = new ModelImage(ModelImage.FLOAT, flairImage.getExtents(), flairImage.getImageName() + "_SIRmap");
            sirMapImage.copyFileTypeInfo(flairImage);
            for (int i = 0; i < volLength; i++) {
                sirMapImage.set(i, flairImage.getDouble(i) / flairFlippedCoreMeanVal);
            }
            saveImageFile(sirMapImage, coreOutputDir, outputBasename + "_SIRmap", FileUtility.XML);
            sirMapImage.disposeLocal();
        }
        
        adcReg.disposeLocal();
        sirRegionReg.disposeLocal();
    }
    
    private ModelImage flipImageAcrossY(final ModelImage img) {
        ModelImage flipImage = (ModelImage) img.clone(img.getImageName() + "_flip");

        AlgorithmFlip flipAlgo = new AlgorithmFlip(flipImage, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE, true);
        flipAlgo.setRunningInSeparateThread(false);

        flipAlgo.run();
        
        return flipImage;
    }
}
