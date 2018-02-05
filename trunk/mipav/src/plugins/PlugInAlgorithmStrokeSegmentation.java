import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogFuzzyCMeans;

import java.io.*;
import java.nio.file.*;
import java.util.Stack;
import java.util.Vector;

import org.apache.commons.csv.*;


public class PlugInAlgorithmStrokeSegmentation extends AlgorithmBase {
    private ModelImage dwiImage;
    
    private ModelImage dwiSeg;
    
    private ModelImage adcImage;
    
    private int adcThreshold;
    
    private VOI coreVOI;
    
    private MaskObject largestObject;
    
    private FileIO fileIO;
    
    private int minAdcObjectSize = 10;
    private int maxAdcObjectSize = 100000;
    
    private static final String outputBasename = "CoreSeg";
    
    private static final String voiExtension = ".xml";
    
    private String coreOutputDir;
    
    /**
     * Constructor.
     *
     * @param  dwi  DWI image
     * @param  adc  ADC image
     */
    public PlugInAlgorithmStrokeSegmentation(ModelImage dwi, ModelImage adc, int threshold, String outputDir) {
        super();
        
        dwiImage = dwi;
        adcImage = adc;
        adcThreshold = threshold;
        coreOutputDir = outputDir;
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	// DWI -> fuzzy c means 4 class
        fireProgressStateChanged("Segmenting DWI image ...");
        fireProgressStateChanged(5);

        final int nClasses = 4;
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
        
        ModelImage[] dwiSegArray = new ModelImage[1];
        FileInfoBase fileInfo1;
        dwiSegArray[0] = new ModelImage(ModelStorageBase.UBYTE, dwiImage.getExtents(), "dwi_hardSeg");
        fileInfo1 = dwiSegArray[0].getFileInfo()[0];
        fileInfo1.setResolutions(dwiImage.getResolutions(0));
        fileInfo1.setUnitsOfMeasure(dwiImage.getUnitsOfMeasure());
        dwiSegArray[0].setFileInfo(fileInfo1, 0);

        AlgorithmFuzzyCMeans fcmAlgo = new AlgorithmFuzzyCMeans(dwiSegArray, dwiImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q, oneSmooth,
                                           twoSmooth, outputGainField, segmentation, cropBackground, threshold, maxIter,
                                           endTolerance, wholeImage);
        
        //final float[] centroids = getCentroid(dwiImage, nClasses);
        final float[] centroids = JDialogFuzzyCMeans.getDefaultCentroids(dwiImage, nClasses, wholeImage, null, cropBackground, threshold);
        fcmAlgo.setCentroids(centroids);
        fcmAlgo.run();
        fcmAlgo.finalize();
        fcmAlgo = null;
        
        dwiSeg = dwiSegArray[0];
        
        fireProgressStateChanged(40);
        
        // extract class 4 as mask
        fireProgressStateChanged("Extracting DWI mask ...");
        fireProgressStateChanged(45);
        
        final int volLength = dwiImage.getExtents()[0] * dwiImage.getExtents()[1] * dwiImage.getExtents()[2];
        short[] dwiSegBuffer = new short[volLength];
        try {
            dwiSeg.exportData(0, volLength, dwiSegBuffer);
        } catch (IOException error) {
            dwiSegBuffer = null;
            displayError("Error on dwi segmentation export: " + dwiImage.getImageName());
            setCompleted(false);
            return;
        }
        
        // clear all values not in class 4 (lesion/gray)
        for (int i = 0; i < volLength; i++) {
            if (dwiSegBuffer[i] != 4) {
                dwiSegBuffer[i] = 0;
            }
        }
        
        try {
            dwiSeg.importData(0, dwiSegBuffer, true);
        } catch (IOException error) {
            dwiSegBuffer = null;
            displayError("Error on dwi segementation importData: " + dwiImage.getImageName());
            setCompleted(false);
            return;
        }
        
        // output mask to disk
        fireProgressStateChanged("Saving DWI mask ...");
        fireProgressStateChanged(50);

        saveImageFile(dwiSeg, coreOutputDir, outputBasename + "_DWI_seg");
        
        // get pixels from ADC within mask with intensity < 620
        fireProgressStateChanged("Thresholding ADC ...");
        fireProgressStateChanged(60);
        
        for (int i = 0; i < volLength; i++) {
            if (dwiSegBuffer[i] != 0) {
                if (adcImage.getInt(i) < adcThreshold) {
                    dwiSegBuffer[i] = 1;
                } else {
                    dwiSegBuffer[i] = 0;
                }
            }
        }
        
        try {
            dwiSeg.importData(0, dwiSegBuffer, true);
        } catch (IOException error) {
            dwiSegBuffer = null;
            displayError("Error on adc threshold importData: " + dwiImage.getImageName());
            setCompleted(false);
            return;
        }
        
        saveImageFile(dwiSeg, coreOutputDir, outputBasename + "_ADC_thresh");
        
        // select largest object
        // TODO: non-symmetric across midline?
        fireProgressStateChanged("Finding core lesion ...");
        fireProgressStateChanged(70);
        
        short[] objectBuffer = keepOnlyLargestObject(dwiSeg, dwiSegBuffer);
        
        try {
            dwiSeg.importData(0, objectBuffer, true);
        } catch (IOException error) {
            dwiSegBuffer = null;
            objectBuffer = null;
            displayError("Error on adc threshold importData: " + dwiImage.getImageName());
            setCompleted(false);
            return;
        }
        
        saveImageFile(dwiSeg, coreOutputDir, outputBasename + "_ADC_thresh_only_largest");
        
        // output core object to VOI on disk
        fireProgressStateChanged("Saving core VOI ...");
        fireProgressStateChanged(80);
        
        coreVOI = maskToVOI(dwiSeg);
        if (!saveVOI(dwiSeg, coreVOI, coreOutputDir, outputBasename + "_VOI")) {
            // problem saving voi
            displayError("Error saving core VOI");
            setCompleted(false);
            return;
        }
        
        // save core stats to tab-delmited file
        if (!saveCoreStats(coreOutputDir, dwiImage.getImageFileName(), adcImage.getImageFileName(), outputBasename + "_VOI" + voiExtension, largestObject.size, adcImage.getResolutions(0))) {
            setCompleted(false);
            return;
        }
        
        setCompleted(true);
    }
    
    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        dwiImage = null;
        adcImage = null;
        if (dwiSeg != null) {
            dwiSeg.disposeLocal();
            dwiSeg = null;
        }
        super.finalize();
    }
    
    public VOI getCoreVOI() {
        return coreVOI;
    }
    
    private void saveImageFile(final ModelImage img, final String dir, final String fileBasename) {
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

        opts.setFileType(FileUtility.XML);
        opts.setFileName(fileBasename + ".xml");

        opts.setOptionsSet(true);
        opts.setMultiFile(false);

        fileIO.writeImage(img, opts, false, false);
    }
    
    public short[] keepOnlyLargestObject(final ModelImage img, final short[] imgBuffer) {
        final int imageLength = imgBuffer.length;
        short[] processBuffer = new short[imgBuffer.length];
        
        Vector<MaskObject> objects = new Vector<MaskObject>();
        
        final int xDim = img.getExtents()[0];
        final int yDim = img.getExtents()[1];
        final int zDim = img.getExtents()[2];
        short floodValue = 1;
        int count = 0;
        
        deleteObjects(img, objects, imgBuffer, processBuffer, minAdcObjectSize, maxAdcObjectSize, true);

        objects.removeAllElements();
        
        for (int i = 0; (i < imageLength) && !threadStopped; i++) {
            if (imgBuffer[i] > 0) {
                count = floodFill(imgBuffer, img.getExtents(), processBuffer, i, floodValue, imgBuffer[i]);
                objects.addElement(new MaskObject(i, floodValue, count));
                floodValue++;
            }
        }

        String mStr;
        float vol;

        mStr = img.getFileInfo(0).getVolumeUnitsOfMeasureStr();
        ViewUserInterface.getReference().getMessageFrame().getData().append(
                " Object \t# of pixels\tVolume(" + mStr + ")\n");
        
        // default to no size
        largestObject = new MaskObject(0, (short) 0, 0);

        for (int i = 0; i < objects.size(); i++) {
            final MaskObject curObj = objects.elementAt(i);
            
            if (curObj.size > largestObject.size) {
                largestObject = curObj;
            }
            
            vol = (curObj.size * img.getFileInfo(0).getResolutions()[0]
                    * img.getFileInfo(0).getResolutions()[1] * img.getFileInfo(0).getResolutions()[2]);

            // UI.setDataText(
            ViewUserInterface.getReference().getMessageFrame().getData().append(
                    "    " + (i + 1) + "\t" + + curObj.size + "\t" + vol + "\n");
        }
        
        // set the mask value for any object that isn't the largest to 0
        for (int i = 0; i < processBuffer.length; i++) {
            if (processBuffer[i] != largestObject.id) {
                processBuffer[i] = 0;
            }
        }
        
        return processBuffer;
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
    private class MaskObject {

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
    
    private boolean saveCoreStats(final String outputDir, final String dwiFile, final String adcFile, final String voiFile, final long numVoxels, final float[] imgResol) {
        double voxelSize = imgResol[0] * imgResol[1] * imgResol[2];
        double coreVol = largestObject.size * voxelSize;
        System.err.println("Largest Object:\t" + largestObject.size + "\t" + coreVol);
        
        final String statsFile = outputDir + File.separator + outputBasename + "_stats.table";
        
        final String volUnitsStr = adcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();
        
        CSVPrinter csvPrinter = null;
        BufferedWriter writer = null;
        try {
            writer = Files.newBufferedWriter(Paths.get(statsFile));
            
            csvPrinter = new CSVPrinter(writer, CSVFormat.TDF.withHeader("Base Dir", "DWI File", "ADC File", "Core VOI", "Core Voxel Count", "Core Volume " + volUnitsStr));
            csvPrinter.printRecord(outputDir, dwiFile, adcFile, voiFile, numVoxels, coreVol);
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
                if (writer != null) {
                    writer.close();
                }
            } catch (final IOException e) {
                // do nothing
            }
        }
        
        return true;
    }
}
