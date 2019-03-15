import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogFuzzyCMeans;

import java.io.*;
import java.util.*;

import org.apache.commons.csv.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmStrokeSegmentation extends AlgorithmBase {
    private ModelImage dwiImage;
    
    private ModelImage adcImage;
    
    private int adcThreshold;
    
    private boolean doSymmetryRemoval;
    
    private int maxSymmetryRemovalSlice = 10;
    
    private int maxSymmetryGrowthSlice = 10;
    
    private boolean doCerebellumSkip;
    
    private int cerebellumSkipSliceMax;
    
    private boolean doCerebellumSkipAggressive;
    
    private int cerebellumSkipAggressiveSliceMax;
    
    private boolean doSkullRemoval;
    
    private int skullRemovalMaskThreshold = 70;
    
    private int threshCloseIter;
    
    private float threshCloseSize;
    
    private VOI coreVOI;
    
    private float additionalObjectMinimumRatio = 0.8f;
    
    private int additionalObjectSearchSize = 1000;
    
    private int additionalObjectMaxDistance = 4;
    
    private FileIO fileIO;
    
    private int minAdcObjectSize = 10;
    private int maxAdcObjectSize = 100000;
    
    public static final String outputLabel = "CoreSeg";
    
    private String outputBasename;
    
    private static final String voiExtension = ".xml";
    
    private String coreOutputDir;
    
    private float lightboxOpacity = 0.5f;
    
//    private File threshLightboxFile;
//    private File coreLightboxFile;
    
    private Hashtable<File, Vector<MaskObject>> selectedObjectTable = new Hashtable<File, Vector<MaskObject>>();
    
    private Vector<File> lightboxFileList = new Vector<File>();

    // core size threshold for using distance-based selection 
    private int coreSizeDistSelectionThreshold = 5;
    
    // weights for selection of core object when no objects are above 5cc in size
    private float coreSelectionDistWeight = 1f;
    private float coreSelectionSizeWeight = 1 - coreSelectionDistWeight;
    
    /**
     * Constructor.
     *
     * @param  dwi  DWI image
     * @param  adc  ADC image
     */
    public PlugInAlgorithmStrokeSegmentation(ModelImage dwi, ModelImage adc, int threshold, boolean symmetryRemoval, boolean cerebellumSkip, int cerebellumSkipMax, boolean cerebellumSkipAggressive, int cerebellumSkipAggressiveMax, boolean removeSkull, int closeIter, float closeSize, String outputDir) {
        super();
        
        dwiImage = dwi;
        adcImage = adc;
        adcThreshold = threshold;
        doSymmetryRemoval = symmetryRemoval;
        doCerebellumSkip = cerebellumSkip;
        cerebellumSkipSliceMax = cerebellumSkipMax;
        doCerebellumSkipAggressive = cerebellumSkipAggressive;
        cerebellumSkipAggressiveSliceMax = cerebellumSkipAggressiveMax;
        doSkullRemoval = removeSkull;
        threshCloseIter = closeIter;
        threshCloseSize = closeSize;
        coreOutputDir = outputDir;
        
        outputBasename = new File(coreOutputDir).getName() + "_" + outputLabel;
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        ModelImage segImg = null;
        ModelImage maskImg = null;
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
            
            short[] segBuffer = new short[volLength];
            try {
                segImg.exportData(0, volLength, segBuffer);
            } catch (IOException error) {
                segBuffer = null;
                displayError("Error on segmentation export: " + segImg.getImageName());
                setCompleted(false);
                return;
            }
            
            // skull artifact masking
            if (doSkullRemoval) {
                maskImg = new ModelImage(ModelStorageBase.BOOLEAN, dwiImage.getExtents(), "dwi_skull_mask");
                for (int i = 0; i < maskImg.getExtents()[2]; i++) {
                    maskImg.setFileInfo(fInfo, i);
                }
                
                for (int i = 0; i < volLength; i++) {
                    if (dwiImage.getInt(i) > skullRemovalMaskThreshold) {
                        maskImg.set(i, 1);
                    } else {
                        maskImg.set(i, 0);
                    }
                }
                
                // open - try to disconnect any artifacts from the brain mask
                open(maskImg);
                
                // close
                close(maskImg, 2, 2f, false);
                
                // fill holes
                fillHoles(maskImg);
                
                // select only largest object
                short[] maskBuffer = new short[volLength];
                short[] processBuffer = new short[volLength];
                try {
                    maskImg.exportData(0, volLength, maskBuffer);
                } catch (IOException error) {
                    if (segImg != null) {
                        segImg.disposeLocal();
                        segImg = null;
                    }
                    
                    maskBuffer = null;
                    displayError("Error on brain mask export: " + maskImg.getImageName());
                    setCompleted(false);
                    return;
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
                    if (segImg != null) {
                        segImg.disposeLocal();
                        segImg = null;
                    }
                    
                    if (maskImg != null) {
                        maskImg.disposeLocal();
                        maskImg = null;
                    }
                    
                    maskBuffer = null;
                    displayError("Error on brain mask importData");
                    setCompleted(false);
                    return;
                }
                
                // dilate object slightly
                dilate(maskImg);
                
                saveImageFile(maskImg, coreOutputDir, outputBasename + "_brain_mask", FileUtility.XML);
            }
            
            // get pixels from ADC within mask with intensity < 620
            fireProgressStateChanged("Thresholding ADC ...");
            fireProgressStateChanged(60);
            
            for (int i = 0; i < volLength; i++) {
                if (segBuffer[i] != 0 && (maskImg != null && maskImg.getBoolean(i) == true)) {
                    if (adcImage.getInt(i) < adcThreshold) {
                        segBuffer[i] = 1;
                    } else {
                        segBuffer[i] = 0;
                    }
                } else {
                    segBuffer[i] = 0;
                }
            }
            
            if (maskImg != null) {
                maskImg.disposeLocal();
                maskImg = null;
            }
            
            try {
                segImg.importData(0, segBuffer, true);
            } catch (IOException error) {
                if (segImg != null) {
                    segImg.disposeLocal();
                    segImg = null;
                }
                
                segBuffer = null;
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
            fireProgressStateChanged(70);
            
            // do first two results with selection only based on core size
    //        File lightboxPass1 = processThresholdedImg(segImg, segBuffer, extents, 1, doCerebellumSkip, cerebellumSkipSliceMax, false);
            File lightboxPass1 = processThresholdedImg(segImg, segBuffer, extents, 1, false, cerebellumSkipSliceMax, false);
            
            if (lightboxPass1 != null) {
                lightboxFileList.add(lightboxPass1);
            }
            
    //        File lightboxPass2 = processThresholdedImg(segImg, segBuffer, extents, 2, doCerebellumSkipAggressive, cerebellumSkipAggressiveSliceMax, false);
    //        File lightboxPass2 = processThresholdedImg(segImg, segBuffer, extents, 2, false, cerebellumSkipAggressiveSliceMax, false);
    //        
    //        if (lightboxPass2 != null) {
    //            lightboxFileList.add(lightboxPass2);
    //        }
            
            // generate lightbox of DWI volume with custom transfer function
            dwiLightbox = generateLightbox(dwiImage, null, lightboxOpacity);
            
            File lightboxDWI = saveImageFile(dwiLightbox, coreOutputDir, outputBasename + "_DWI_lightbox", FileUtility.PNG, true);
            
            if (lightboxDWI != null) {
                lightboxFileList.add(lightboxDWI);
                selectedObjectTable.put(lightboxDWI, new Vector<MaskObject>());
            }
            
            dwiLightbox.disposeLocal();
            dwiLightbox = null;
            
    //        // distance-based selection (if small core)
    //        File lightboxPass3 = processThresholdedImg(segImg, segBuffer, extents, 3, doCerebellumSkipAggressive, cerebellumSkipAggressiveSliceMax, true);
    //        
    //        if (lightboxPass3 != null) {
    //            lightboxFileList.add(lightboxPass3);
    //        }
            
            // commented out because masks seem just as useful to users
            
    //        // output core object to VOI on disk
    //        fireProgressStateChanged("Saving core VOI ...");
    //        fireProgressStateChanged(80);
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
            if (!saveCoreStats(coreOutputDir, dwiImage.getImageFileName(), adcImage.getImageFileName(), adcImage.getResolutions(0))) {
                if (segImg != null) {
                    segImg.disposeLocal();
                    segImg = null;
                }
                
                setCompleted(false);
                return;
            }
            
            if (segImg != null) {
                segImg.disposeLocal();
            }
            
            setCompleted(true);
        } catch (Exception e) {
            e.printStackTrace();
            
            if (segImg != null) {
                segImg.disposeLocal();
                segImg = null;
            }
            
            if (maskImg != null) {
                maskImg.disposeLocal();
                maskImg = null;
            }
            
            if (dwiLightbox != null) {
                dwiLightbox.disposeLocal();
                dwiLightbox = null;
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
        
        // TODO set lut/rgb transfer func
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
    
    private File processThresholdedImg(ModelImage segImg, short[] segBuffer, int[] extents, int passNum, boolean doSkip, int skipSliceMax, boolean useDistanceSelection) {
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
        
        if (doSymmetryRemoval) {
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
        
        // get pixels from ADC within closed object mask with intensity < 620, again
        for (int i = 0; i < volLength; i++) {
            if (objectBuffer[i] != 0) {
                if (adcImage.getInt(i) < adcThreshold) {
                    objectBuffer[i] = 1;
                } else {
                    objectBuffer[i] = 0;
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
        ModelImage coreLightbox = generateLightbox(adcImage, threshImg, lightboxOpacity);
        
        lightboxFile = saveImageFile(coreLightbox, coreOutputDir, outputBasename + "_ADC_core_lightbox_pass" + passNum, FileUtility.PNG);
        
        if (lightboxFile != null) {
            selectedObjectTable.put(lightboxFile, selectedObjectList);
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
                        if (adcImage.getInt(i) < adcThreshold) {
                            coreSizeList[objNum]++;
                        }
                    }
                }
            }
            
            boolean allCoreSmallFlag = true;
            double resFactorCC = getResolutionFactorCC(adcImage);
            for (int coreSize : coreSizeList) {
                if ((coreSize * resFactorCC) > coreSizeDistSelectionThreshold) {
                    allCoreSmallFlag = false;
                }
            }
            
            if (allCoreSmallFlag && useDistanceSelection) {
//                    if (!prevBrainDistMapFlag) {
//                        distanceMapFG(maskImg);
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
//                        coreDistList[objNum] = maskImg.getFloat(sortedObjects[sortedObjects.length -1 - objNum].index);
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
        } else {
            System.err.println("No qualifying object found in volume.");
        }
        
        // TODO disabled selection of additional objects based on size/distance for now
        
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
//        if (selectedObjectList.get(0).size <= additionalObjectSearchSize) {
            int objectMinSize = (int) (selectedObjectList.get(0).size * additionalObjectMinimumRatio);
            int nextObjectIndex = sortedObjects.length - 2;
            MaskObject nextObject;
            while (nextObjectIndex >= 0 && (nextObject = sortedObjects[nextObjectIndex]).size >= objectMinSize) {
                System.err.println(nextObject.id + "\t" + nextObject.size);
                selectedObjectList.add(nextObject);
                nextObjectIndex--;
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
    
    private boolean saveCoreStats(final String outputDir, final String dwiFile, final String adcFile, final float[] imgResol) {
        double voxelSize = imgResol[0] * imgResol[1] * imgResol[2];
        
        final String statsFile = outputDir + File.separator + outputBasename + "_stats.table";
        
        final String volUnitsStr = adcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();
        
        CSVPrinter csvPrinter = null;
        BufferedWriter bw = null;
        FileWriter fw = null;
        try {
            fw = new FileWriter(statsFile);
            bw = new BufferedWriter(fw);
            
            csvPrinter = new CSVPrinter(bw, CSVFormat.TDF.withHeader("Base Dir", "DWI File", "ADC File", "Pass number", "Core Voxel Count", "Core Volume " + volUnitsStr));
            
            for (int i = 0; i < getLightboxFileList().size(); i++) {
                int passNum = i + 1;
                
                int coreSize = getCoreSize(getLightboxFileList().get(i));
                if (coreSize > 0) {
                    double coreVol = coreSize * voxelSize;
                    System.err.println("Core Size (pass " + passNum + "):\t" + coreSize + "\t" + coreVol);
                    
                    csvPrinter.printRecord(outputDir, dwiFile, adcFile, passNum, coreSize, coreVol);
                }
            }
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
    
    private ModelImage generateLightbox(final ModelImage imgA, final ModelImage mask, float blendingOpacity) {
        ModelImage lightbox = null;
        
        ModelImage newRGB;
        if (imgA.isColorImage()) {
            newRGB = (ModelImage) imgA.clone();
        } else {
            newRGB = new ModelImage(ModelImage.ARGB, imgA.getExtents(), imgA.getImageName());
            final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(imgA, imgA, imgA, newRGB, true, true, 255.0f, true);
            mathAlgo.run();
        }
        
        if (mask != null) {
            // set any values to fully red if set in the mask
            for (int i = 0; i < mask.getDataSize(); i++) {
                if (mask.getBoolean(i) == true) {
                    // TODO set value with opacity blending
                    
                    newRGB.setC(i, 1, 255.0f);
                    newRGB.setC(i, 2, 0f);
                    newRGB.setC(i, 3, 0f);
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
        for (File file : selectedObjectTable.keySet()) {
            sizeTable.put(file, Double.valueOf(getCoreSize(file)));
        }
        
        return sizeTable;
    }
    
    public int getCoreSize(File lightboxFile) {
        int totalSize = 0;
        for (MaskObject obj : selectedObjectTable.get(lightboxFile)) {
            totalSize += obj.size;
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
    
    private void dilate(ModelImage img) {
        int kernel = AlgorithmMorphology3D.CONNECTED6;
        float kernelSize = 1f; // mm
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
        
        fireProgressStateChanged(40);
        
        // extract class 4 as mask
        fireProgressStateChanged("Extracting segmentation mask ...");
        fireProgressStateChanged(45);
        
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
}
