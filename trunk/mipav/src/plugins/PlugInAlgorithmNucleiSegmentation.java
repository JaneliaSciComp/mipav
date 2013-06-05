import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * PlugInAlgorithmNucleiSegmentation is used to identify nuclei.
 * @version  May 9, 2013
 * @author   William Gandler
 * @see      AlgorithmBase
 */
public class PlugInAlgorithmNucleiSegmentation extends AlgorithmBase {
  //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;
    
  //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Minimum number of pixels in a nucleus */
    private int minSize = 100;
    
    /** Maximum number of pixels in a nucleus */
    private int maxSize = 1000000;
    
    /** The list of files to try to process with the algorithm. */
    private Vector<File> inputFiles;
    
    /** List of result images. */
    private static final Vector<ModelImage> resultImages = new Vector<ModelImage>();
    
    /**
     * 
     * @param srcImg
     * @param minSize
     * @param maxSize
     */
    public PlugInAlgorithmNucleiSegmentation(Vector<File> inputFiles, int minSize, int maxSize) {
        super(null, null);
        this.minSize = minSize;
        this.maxSize = maxSize;
        this.inputFiles = inputFiles;
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        inputFiles = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        int length;
        int xDim;
        int yDim;
        float xRes;
        float yRes;
        int xUnits;
        int yUnits;
        float buffer[];
        AlgorithmFuzzyCMeans fcmAlgo;
        ModelImage grayImage;
        FileInfoBase fileInfo;
        int nClasses;
        int nPyramid;
        int oneJacobiIter;
        int twoJacobiIter;
        float q;
        float oneSmooth;
        float twoSmooth;
        boolean outputGainField;
        int segmentation;
        boolean cropBackground;
        float threshold;
        int maxIter;
        float endTolerance;
        boolean wholeImage;
        float[] centroids;
        float min;
        float max;
        byte byteBuffer[];
        int i;
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        AlgorithmMorphology2D idObjectsAlgo2D;
        int numObjects;
        byte[] IDArray;
        boolean[] removeID;
        int id;
        int j;
        int x;
        int y;
        boolean allRemoved;
        int numRemoved;
        AlgorithmVOIExtraction algoVOIExtraction;
        VOIVector VOIs;
        VOIVector VOI2s;
        int nVOIs;
        ViewUserInterface UI = ViewUserInterface.getReference();
        AlgorithmMorphology2D fillHolesAlgo2D;
        double[] angleAxislsq = new double[1];
        double[] eccentricitylsq = new double[1];
        double[] majorAxislsq = new double[1];
        double[] minorAxislsq = new double[1];
        double[] xCenterlsq = new double[1];
        double[] yCenterlsq = new double[1];
        double[][] xyproj = null;
        double[] residualSumOfSquares = new double[1];
        Vector<Vector3f> xy = null;
        double stdDevEllipse;
        int numVOIsDeleted;
        
        int progressPerImg = 100 / inputFiles.size();
    	int curProgress = 0;
        
        for (File inFile : inputFiles) {
        	fireProgressStateChanged("Opening image " + inFile.getName() + " ...");
        	srcImage = openFile(inFile);

	        if (srcImage == null) {
	            System.err.println("Source Image is null - skipping: " + inFile.getName());
	            continue;
	        }
	        
	        xDim = srcImage.getExtents()[0];
	        yDim = srcImage.getExtents()[1];
	        xRes = srcImage.getFileInfo(0).getResolutions()[0];
	        yRes = srcImage.getFileInfo(0).getResolutions()[1];
	        xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
	        yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
	        length = xDim * yDim;
	        
	        try {
	            buffer = new float[length];
	            srcImage.exportData(0, length, buffer); // export blue data
	        } catch (IOException error) {
	            buffer = null;
	            //errorCleanUp("Algorithm NucleiSegmentation reports: source image locked", true);
	            displayError("Algorithm NucleiSegmentation reports: source image locked: " + inFile.getName());
	            continue;
	        } catch (OutOfMemoryError e) {
	            buffer = null;
	            //errorCleanUp("Algorithm NucleiSegmentation reports: out of memory", true);
	            displayError("Algorithm NucleiSegmentation reports: out of memory: " + inFile.getName());
	            continue;
	        }

	        //fireProgressStateChanged("Processing image ...");
	        //fireProgressStateChanged("Creating  image");
	        grayImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_gray");
	        fileInfo = grayImage.getFileInfo()[0];
	        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
	        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
	        grayImage.setFileInfo(fileInfo, 0);
	
	        try {
	            grayImage.importData(0, buffer, true);
	        } catch (IOException error) {
	            buffer = null;
	            //errorCleanUp("Error on grayImage.importData", true);
	            displayError("Error on grayImage.importData: " + inFile.getName());
	            continue;
	        }
	
	        // Segment into 2 values
	        fireProgressStateChanged("Segmenting image " + inFile.getName() + " ...");
	        curProgress += progressPerImg / 5;
        	fireProgressStateChanged(curProgress);
	        //fireProgressStateChanged("Performing FuzzyCMeans Segmentation on image");
	        //fireProgressStateChanged(2);
	
	        nClasses = 2;
	        nPyramid = 4;
	        oneJacobiIter = 1;
	        twoJacobiIter = 2;
	        q = 2.0f;
	        oneSmooth = 2e4f;
	        twoSmooth = 2e5f;
	        outputGainField = false;
	        segmentation = HARD_ONLY;
	        cropBackground = false;
	        threshold = 0.0f;
	        maxIter = 200;
	        endTolerance = 0.01f;
	        wholeImage = true;
	
	        // grayImage enters ModelStorageBase.FLOAT and returns ModelStorageBase.UBYTE
	        fcmAlgo = new AlgorithmFuzzyCMeans(grayImage, nClasses, nPyramid, oneJacobiIter, twoJacobiIter, q, oneSmooth,
	                                           twoSmooth, outputGainField, segmentation, cropBackground, threshold, maxIter,
	                                           endTolerance, wholeImage);
	        centroids = new float[2];
	        min = (float) grayImage.getMin();
	        max = (float) grayImage.getMax();
	        centroids[0] = min + ((max - min) / 3.0f);
	        centroids[1] = min + (2.0f * (max - min) / 3.0f);
	        fcmAlgo.setCentroids(centroids);
	        fcmAlgo.run();
	        fcmAlgo.finalize();
	        fcmAlgo = null;
	
	        // Now convert the min = 1 and max = 2 to min = 0 and and max = 1
	        //fireProgressStateChanged("Setting segmented image values to 0 and 1");
	        //fireProgressStateChanged(6);
	
	        byteBuffer = new byte[length];
	        try {
	            grayImage.exportData(0, length, byteBuffer);
	        } catch (IOException error) {
	            byteBuffer = null;
	            //errorCleanUp("Error on grayImage.exportData", true);
	            displayError("Error on grayImage.exportData: " + inFile.getName());
	            continue;
	        }
	        
	        for (i = 0; i < length; i++) {
	            byteBuffer[i]--;
	        }
	        
	        try {
	            grayImage.importData(0, byteBuffer, true);
	        }
	        catch (IOException error) {
	            byteBuffer = null;
	            //errorCleanUp("Error on grayImage.importData", true);
	            displayError("Error on grayImage.importData: " + inFile.getName());
	            continue;
	        }
	        
	        // Remove all inappropriate holes in nuclei
	        fireProgressStateChanged("Cleaning up nuclei segmentations for " + inFile.getName() + " ...");
	        curProgress += progressPerImg / 5;
        	fireProgressStateChanged(curProgress);
	        
	        //fireProgressStateChanged("Removing holes from nuclei");
	        //fireProgressStateChanged(7);
	
	        fillHolesAlgo2D = new AlgorithmMorphology2D(grayImage, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0,
	                                                    wholeImage);
	        fillHolesAlgo2D.run();
	        fillHolesAlgo2D.finalize();
	        fillHolesAlgo2D = null;
	        
	        // Filling holes set top, right, left, and bottom boundaries to zero
	        // Restore original boundary values
	        // This will allow objects touching boundaries to be removed
	        IDArray = new byte[length];
	        for (i = 0; i < length; i++) {
	            IDArray[i] = byteBuffer[i];
	        }
	        
	        try {
	            grayImage.exportData(0, length, byteBuffer);
	        } catch (IOException error) {
	            byteBuffer = null;
	            //errorCleanUp("Error on grayImage.exportData", true);
	            displayError("Error on grayImage.exportData: " + inFile.getName());
	            continue;
	        }
	        
	        for (i = 0; i < xDim; i++) {
	            byteBuffer[i] = IDArray[i];
	        }
	
	        // bottom boundary
	        for (i = (yDim - 1) * xDim; i < length; i++) {
	            byteBuffer[i] = IDArray[i];
	        }
	
	        // left boundary
	        for (i = 0; i < length; i = i + xDim) {
	            byteBuffer[i] = IDArray[i];
	        }
	
	        // right boundary
	        for (i = xDim; i < length; i = i + xDim) {
	            byteBuffer[i - 1] = IDArray[i-1];
	        }
	        
	        try {
	            grayImage.importData(0, byteBuffer, true);
	        }
	        catch (IOException error) {
	            byteBuffer = null;
	            //errorCleanUp("Error on grayImage.importData", true);
	            displayError("Error on grayImage.importData: " + inFile.getName());
	            continue;
	        }
	        
	        //fireProgressStateChanged("IDing objects in segmented image");
	        //fireProgressStateChanged(15);
	        
	        try {
	            grayImage.importData(0, byteBuffer, true);
	        }
	        catch (IOException error) {
	            byteBuffer = null;
	            //errorCleanUp("Error on grayImage.importData", true);
	            displayError("Error on grayImage.importData: " + inFile.getName());
	            continue;
	        }
	
	        //fireProgressStateChanged("IDing objects in segmented image");
	        //fireProgressStateChanged(15);
	        
	        numPruningPixels = 0;
	        edgingType = 0;   
	        
	        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
	        circleDiameter = 0.0f;
	        method = AlgorithmMorphology2D.ID_OBJECTS;
	        itersDilation = 0;
	        itersErosion = 0;
	        idObjectsAlgo2D = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
	                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
	        idObjectsAlgo2D.setMinMax(minSize, maxSize);
	        idObjectsAlgo2D.run();
	        idObjectsAlgo2D.finalize();
	        idObjectsAlgo2D = null;
	
	        grayImage.calcMinMax();
	        numObjects = (int) grayImage.getMax();
	        Preferences.debug("numObjects = " + numObjects + "\n", Preferences.DEBUG_ALGORITHM);
	
	        try {
	            grayImage.exportData(0, length, IDArray);
	        } catch (IOException error) {
	            byteBuffer = null;
	            IDArray = null;
	            //errorCleanUp("Error on grayImage.exportData", true);
	            displayError("Error on grayImage.exportData: " + inFile.getName());
	            continue;
	        }

	        fireProgressStateChanged("Removing nuclei touching edges from " + inFile.getName() + " ...");
	        curProgress += progressPerImg / 5;
        	fireProgressStateChanged(curProgress);
	        
	        //fireProgressStateChanged("Removing objects touching edges");
	        //fireProgressStateChanged(22);
	
	        removeID = new boolean[numObjects];
	        for (id = 1; id <= numObjects; id++) {
	
	            for (j = 0, y = 0; y < yDim; y++, j += xDim) {
	
	                for (x = 0; x < xDim; x++) {
	                    i = x + j;
	
	                    if ((IDArray[i] == id) && ((x == 0) || (x == (xDim - 1)) || (y == 0) || (y == (yDim - 1)))) {
	                        removeID[id-1] = true;
	                    }
	                } // for (x = 0; x < xDim; x++)
	            } // for (j = 0, y = 0; y < yDim; y++, j += xDim)
	        } // for (id = 1; id <= numObjects; id++)
	        
	        allRemoved = true;
	        numRemoved = 0;
	        for (id = 1; id <= numObjects; id++) {
	            if (!removeID[id-1]) {
	                allRemoved = false;
	            }
	            else {
	                numRemoved++;
	            }
	        } // for (id = 1; id <= numObjects; id++)
	        
	        if (allRemoved) {
	            Preferences.debug("All objects touch edges so don't remove any\n");
	            for (id = 1; id <= numObjects; id++) {
	                removeID[id-1] = false;   
	            }
	        }
	        else {
	            UI.setDataText("Removing " + numRemoved + " objects of " + numObjects + " for touching edges\n");
	            Preferences.debug("Removing " + numRemoved + " objects of " + numObjects + " for touching edges\n");    
	        }

	        for (id = numObjects; id >= 1; id--) {    
	            if (removeID[id-1]) {
	    
	                for (i = 0; i < length; i++) {
	
	                    if (IDArray[i] == (byte) id) {
	                        IDArray[i] = (byte) 0;
	                    } 
	                    else if (IDArray[i] > id) {
	                        IDArray[i]--;
	                    }
	                }
	
	                numObjects--;
	            } // if (removeID[id-1])
	        } // for (id = numObjects; id >= 1; id--) 
	        
	        try {
	            grayImage.importData(0, IDArray, true);
	        } catch (IOException error) {
	            byteBuffer = null;
	            //errorCleanUp("Error on grayImage.importData", true);
	            displayError("Error on grayImage.importData: " + inFile.getName());
	            continue;
	        }
	        
	        fireProgressStateChanged("Extracting VOIs from segmentation of " + inFile.getName() + " ...");
	        curProgress += progressPerImg / 5;
        	fireProgressStateChanged(curProgress);
	        
	        //fireProgressStateChanged("Extracting VOIs from segmented image");
	        //fireProgressStateChanged(70);
	        
	        algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
	        //algoVOIExtraction.setColorTable(colorTable);
	        //algoVOIExtraction.setNameTable(nameTable);
	        algoVOIExtraction.run();
	        algoVOIExtraction.finalize();
	        algoVOIExtraction = null;
	        
	        VOIs = grayImage.getVOIs();
	        nVOIs = VOIs.size();
	        Preferences.debug("nVOIS before removal for excessive deviation from ellipse shape = " + nVOIs + "\n", Preferences.DEBUG_ALGORITHM);
	        
	        // Want to remove VOIs that surround 2 nuclei instead of 1, that have an improper fitting and only
	        // cover part of the nucleus instead of all of it, and that are folded over.
	        // Proper nuclei have shapes that are roughly ellipses.  Calculate the residual sum of squares of
	        // the difference between the VOI contour and the best fitting ellipse.  From the residual sum of
	        // squares calculate the standard deviation = sqrt(residualSumOfSquares/n-1).
	        // In the first 16 samples the largest standard deviation observed for a good nucleus was 2.65
	        // So eliminate all nuclei showing standard deviation >= 3.0.
	        VOI2s = new VOIVector();
	        numVOIsDeleted = 0;
	        for (i = 0, j = 0; i < nVOIs; i++) {
	            xRes = 1.0f;
	            yRes = 1.0f;
	            ((VOIContour) (VOIs.VOIAt(i).getCurves().elementAt(0))).secondOrderAttributeslsq(xRes, yRes, xUnits, yUnits,
                        angleAxislsq, eccentricitylsq, majorAxislsq,
                        minorAxislsq, xCenterlsq, yCenterlsq);
	            xy = VOIs.VOIAt(i).getCurves().elementAt(0);
	            xyproj = new double[xy.size()][2];
	            ((VOIContour) (VOIs.VOIAt(i).getCurves().elementAt(0))).residuals_ellipse(residualSumOfSquares, xyproj,
	                           xy, xCenterlsq[0], yCenterlsq[0], majorAxislsq[0], minorAxislsq[0], angleAxislsq[0]);
	            stdDevEllipse = Math.sqrt(residualSumOfSquares[0]/(xy.size() - 1));
	            Preferences.debug("VOI " + i + " standard deviation from ellipse = " + stdDevEllipse + "\n",
	                              Preferences.DEBUG_ALGORITHM);
	            if (stdDevEllipse < 3.0) {
	                VOI2s.add(j, VOIs.VOIAt(i));
	                VOI2s.VOIAt(j).setName(String.valueOf(j));
	                j++;
	            }
	            else{
	                numVOIsDeleted++;
	            }
	        }
	        
	        nVOIs = nVOIs - numVOIsDeleted;
	        VOIs.clear();
	        VOIs = null;
	
	        srcImage.setVOIs(VOI2s);
	        
	        VOIs = srcImage.getVOIs();
	        for (i = 0; i < nVOIs; i++) {
	            VOIs.VOIAt(i).setFixed(true);
	            VOIs.VOIAt(i).setDisplayMode(VOI.CONTOUR);
	            ((VOIContour)(VOIs.VOIAt(i).getCurves().elementAt(0))).setDoGeometricCenterLabel(true);
	        }
	        
	        curProgress += progressPerImg / 5;
        	fireProgressStateChanged(curProgress);
        	
        	// cleanup the temp image
        	grayImage.disposeLocal(false);
        }
        
        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }
    
    /**
     * Try to open an image file.
     * @param file The file to try to open.
     * @return The ModelImage of the specified file, or null if there was an error.
     */
    private static final ModelImage openFile(File file) {
    	ModelImage img = null;
    	
    	System.err.println("Trying to open file:\t" + file.getName());
    	
    	FileIO io = new FileIO();
    	try {
    		img = io.readImage(file.getAbsolutePath());
    		
			resultImages.add(img);
    	} catch (Exception e) {
    		System.err.println("Failed to open file:\t" + file.getName());
    		e.printStackTrace();
    		img = null;
    	}
    	
    	return img;
    }
    
    public Vector<ModelImage> getResultImages() {
    	return resultImages;
    }
}