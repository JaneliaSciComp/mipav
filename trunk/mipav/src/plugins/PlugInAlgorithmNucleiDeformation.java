import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;

import java.io.*;
import java.util.Vector;


/**
*
* @version  May 9, 2013
* @author   William Gandler
* @see      AlgorithmBase
* PlugInAlgorithmNucleiDeformation is used to identify nuclei and output statistics for each nucleus 
*/

public class PlugInAlgorithmNucleiDeformation extends AlgorithmBase {
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
    private File[] inputFiles;
    
    /** Whether to show the images processed in frames, with the VOI segmentations. */
    private boolean showResultImages;
    
    /** List of result images. */
    private static final Vector<ModelImage> resultImages = new Vector<ModelImage>();
    
    private static final String[] statsToCalculate = new String[] {VOIStatisticalProperties.quantityDescription, VOIStatisticalProperties.areaDescription, 
		VOIStatisticalProperties.perimeterDescription, VOIStatisticalProperties.circularityDescription,
		VOIStatisticalProperties.solidityDescription, VOIStatisticalProperties.minIntensity, VOIStatisticalProperties.maxIntensity,
		VOIStatisticalProperties.avgIntensity, VOIStatisticalProperties.deviationDescription,
        VOIStatisticalProperties.eccentricityDescription, VOIStatisticalProperties.meanCurvatureDescription,
        VOIStatisticalProperties.stdDevCurvatureDescription, VOIStatisticalProperties.meanNegativeCurvatureDescription,
        VOIStatisticalProperties.numberOfIndentationsDescription};

    private static final boolean[] checkList = new boolean[VOIStatisticalProperties.numberOfStatistics];

    static {
    	String[] statDescr = VOIStatisticalProperties.statisticDescription;
		for (int i = 0; i < statDescr.length; i++) {
			checkList[i] = false;
			for (int j = 0; j < statsToCalculate.length; j++) {
				if (statDescr[i].equals(statsToCalculate[j])) {
					checkList[i] = true;
					break;
				}
			}
		} 
    }
    
    /**
     * 
     * @param srcImg
     * @param minSize
     * @param maxSize
     */
    public PlugInAlgorithmNucleiDeformation(File[] inputFiles, int minSize, int maxSize, boolean showResultImages) {
        super(null, null);
        this.minSize = minSize;
        this.maxSize = maxSize;
        this.inputFiles = inputFiles;
        this.showResultImages = showResultImages;
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
        int nVOIs;
        ViewUserInterface UI = ViewUserInterface.getReference();
        AlgorithmMorphology2D fillHolesAlgo2D;
        
        for (File inFile : inputFiles) {
        	srcImage = openFile(inFile, showResultImages);

	        if (srcImage == null) {
	            System.err.println("Source Image is null - skipping: " + inFile.getName());
	            continue;
	        }
	        
	        xDim = srcImage.getExtents()[0];
	        yDim = srcImage.getExtents()[1];
	        length = xDim * yDim;
	        
	        try {
	            buffer = new float[length];
	            srcImage.exportData(0, length, buffer); // export blue data
	        } catch (IOException error) {
	            buffer = null;
	            //errorCleanUp("Algorithm NucleiDeformation reports: source image locked", true);
	            displayError("Algorithm NucleiDeformation reports: source image locked: " + inFile.getName());
	            continue;
	        } catch (OutOfMemoryError e) {
	            buffer = null;
	            //errorCleanUp("Algorithm NucleiDeformation reports: out of memory", true);
	            displayError("Algorithm NucleiDeformation reports: out of memory: " + inFile.getName());
	            continue;
	        }
	
	        fireProgressStateChanged("Processing image ...");
	
	        fireProgressStateChanged("Creating  image");
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
	        fireProgressStateChanged("Performing FuzzyCMeans Segmentation on image");
	        
	        fireProgressStateChanged(2);
	
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
	        fireProgressStateChanged("Setting segmented image values to 0 and 1");
	        fireProgressStateChanged(6);
	
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
	        fireProgressStateChanged("Removing holes from nuclei");
	        fireProgressStateChanged(7);
	
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
	        
	        fireProgressStateChanged("IDing objects in segmented image");
	        fireProgressStateChanged(15);
	        
	        try {
	            grayImage.importData(0, byteBuffer, true);
	        }
	        catch (IOException error) {
	            byteBuffer = null;
	            //errorCleanUp("Error on grayImage.importData", true);
	            displayError("Error on grayImage.importData: " + inFile.getName());
	            continue;
	        }
	
	        fireProgressStateChanged("IDing objects in segmented image");
	        fireProgressStateChanged(15);
	        
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
	
	        fireProgressStateChanged("Removing objects touching edges");
	        fireProgressStateChanged(22);
	
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
	
	        fireProgressStateChanged("Extracting VOIs from segmented image");
	        fireProgressStateChanged(70);
	        algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
	        //algoVOIExtraction.setColorTable(colorTable);
	        //algoVOIExtraction.setNameTable(nameTable);
	        algoVOIExtraction.run();
	        algoVOIExtraction.finalize();
	        algoVOIExtraction = null;
	        
	        VOIs = grayImage.getVOIs();
	        nVOIs = VOIs.size();
	        Preferences.debug("nVOIS = " + nVOIs + "\n", Preferences.DEBUG_ALGORITHM);
	
	        srcImage.setVOIs(VOIs);
	        
	        // save all VOIs to disk for this image
	        saveAllVOIs(srcImage);
	        
	        // run statistics generator on VOIs in image and output to file on disk
	        outputStatistics(srcImage);
	        
	        VOIs = srcImage.getVOIs();
	        for (i = 0; i < nVOIs; i++) {
	            VOIs.VOIAt(i).setFixed(true);
	            VOIs.VOIAt(i).setDisplayMode(VOI.CONTOUR);
	            ((VOIContour)(VOIs.VOIAt(i).getCurves().elementAt(0))).setDoGeometricCenterLabel(true);
	        }
	        
	        // cleanup current image
	        if (!showResultImages) {
	        	srcImage.disposeLocal();
	        	srcImage = null;
	        }
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
    private static final ModelImage openFile(File file, boolean showResultImages) {
    	ModelImage img = null;
    	
    	if (file.isDirectory()) {
    		System.err.println("Skipping directory:\t" + file.getName());
    		return null;
    	} else if (file.getName().startsWith(".")) {
    		System.err.println("Skipping file that starts with .:\t" + file.getName());
    		return null;
    	} else {
    		System.err.println("Trying to open file:\t" + file.getName());
    	}
    	
    	FileIO io = new FileIO();
    	try {
    		img = io.readImage(file.getAbsolutePath());
    		
    		if (showResultImages) {
    			new ViewJFrameImage(img);
    			resultImages.add(img);
    		}
    	} catch (Exception e) {
    		System.err.println("Failed to open file:\t" + file.getName());
    		e.printStackTrace();
    		img = null;
    	}
    	
    	return img;
    }
    
    /**
     * This method saves all VOIs for the active image to the default VOI directory for that image.
     * @param img The image for which to save VOIs to disk.
     */
    private static final void saveAllVOIs(ModelImage img) {
        String fileDir;
        String tmpImageName;
        String imageName;
        String voiDir;
        fileDir = img.getFileInfo(0).getFileDirectory();

        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        if (img.isDicomImage()) {
            tmpImageName = img.getFileInfo(0).getFileName();

            final int index = tmpImageName.lastIndexOf(".");

            if (index > 0) {
                tmpImageName = tmpImageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = tmpImageName.length();

            for (int i = tmpImageName.length() - 1; i >= 0; i--) {
                final char myChar = tmpImageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                tmpImageName = new String("DICOM");
            } else {
                tmpImageName = tmpImageName.substring(0, newIndex);
            }
        } else {
            tmpImageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = tmpImageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        voiDir = new String(fileDir + File.separator + "defaultVOIs_" + imageName + File.separator);

        try {
        	ViewVOIVector VOIs = img.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, img);
                    fileVOI.writeXML(VOIs.VOIAt(i), true, true);
                } else {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, img);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), true);
                }
            }
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }
    }
    
    private void outputStatistics(ModelImage img) {
    	// TODO: run statistics generator on VOIs in image and output to file on disk
    	AlgorithmVOIProps calculator = new AlgorithmVOIProps(img, AlgorithmVOIProps.PROCESS_PER_CONTOUR, RangeType.NO_RANGE, img.getVOIs());
    	calculator.setSelectedStatistics(checkList);
    	calculator.setShowTotals(false);
    	calculator.run();
    	
    	Vector<VOIStatisticalProperties> statsList = new Vector<VOIStatisticalProperties>(img.getVOIs().size());
    	for (VOI voi : img.getVOIs()) {
    		statsList.add(calculator.getVOIProperties(voi));
    	}
    	
    	String statsOutputFile = img.getFileInfo(0).getFileDirectory() + File.separator + img.getImageName() + ".table";
    	Vector<String> columnHeaders = getColumnHeaders(img);
    	writeStatisticFile(statsOutputFile, columnHeaders, statsList, img.getVOIs());
    }
    
    /**
     * Writes out the statistics file based on the current logModel
     */
    private void writeStatisticFile(String tableDestination, Vector<String> columnHeaders, Vector<VOIStatisticalProperties> statsList, VOIVector VOIs) {
    	final boolean noisyProcess = true;
    	final boolean overwrite = true;
    	final boolean append = false;
    	StringBuffer logFileText = null;
        FileWriter statFW;
        final File statFile = new File(tableDestination);

        try {

            if (statFile.exists()) {

                if (overwrite) {
                    statFile.delete();
                }
            }
        } catch (final SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \"" + statFile.getName()
                        + "\"; \n" + "is destination directory still writable?  " + "Table file not written.");
            }

            Preferences.debug("security violation incurred while creating \"" + statFile.getName() + "\";\n");

            return;
        }

        try {

            if ( !statFile.createNewFile()) { /* there was an error here! */
            }
        } catch (final IOException io) {
            Preferences.debug("IOexception error in creating statFile!  threw "
                    + "exception rather than createNewFile == false;\n" + io);
            io.printStackTrace();
            Preferences.debug("IO exception while writing VOIStatistic's \"" + statFile.getAbsolutePath()
                    + "\"\n");

            return;
        } catch (final SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \""
                        + statFile.getAbsolutePath() + "\"; \n" + "is destination directory still writable?  "
                        + "Table file not written.");
            }

            Preferences.debug("security violation incurred while creating \"" + statFile.getAbsolutePath()
                    + "\";\n");

            return;
        }

        try {

            if (overwrite) {
                statFW = new FileWriter(statFile.getAbsolutePath(), false);
            } else if (append) {
                statFW = new FileWriter(statFile.getAbsolutePath(), true);           
            } else { // WRITE
                statFW = new FileWriter(statFile.getAbsolutePath());
            }
        
            logFileText = writeStatsToString(columnHeaders, statsList, VOIs);
            statFW.write(logFileText.toString());
            statFW.flush();
            statFW.close();
        } catch (final IOException ioe) {

            if (noisyProcess) {
                MipavUtil.displayError("error writing the logging to \"" + statFile.getAbsolutePath() + "\""); // figure
                // out
                // where
                // to
                // store
                // somewhere
                // else?
            }
        } finally {
        	if (logFileText != null) {
        		logFileText.delete(0, logFileText.length() - 1); // empty out the buffer
        	}
        }
    }
    
    /**
     * Converts the current logModel into either a tab-delimited text file or an XML file.
     * 
     * @return
     */
    @SuppressWarnings("unchecked")
    protected StringBuffer writeStatsToString(Vector<String> columnHeaders, Vector<VOIStatisticalProperties> statsList, VOIVector VOIs) {
        StringBuffer total = new StringBuffer();
        String newLine = System.getProperty("line.separator");
        //get column names
        for(int i=0; i<columnHeaders.size(); i++) {
            total.append(columnHeaders.get(i)).append("\t");
        }
        total.append(newLine);
        
        //get total data
        Vector<Vector<String>> column = getStatsData(statsList, VOIs);
        Vector<String> row;
        String cellEntry;
        for(int i=0; i<column.size(); i++) {
            row = column.get(i);
            for(int j=0; j<row.size(); j++) {
                if(row.get(j) == null || row.get(j).toString().length() == 0) {
                    cellEntry = " ";
                } else {
                    cellEntry = row.get(j).toString();
                }
                total.append(cellEntry).append("\t");
            }
            total.append(newLine);
        }
        
        return total;
    }
    
    /**
     * Writes the column titles of selected statistics calculations to the logModel.
     */
    private static final Vector<String> getColumnHeaders(ModelImage img) {
        Vector<String> logModelCol = new Vector<String>();
        logModelCol.add("Name");
        
        // assume that always PER_CONTOUR
        logModelCol.add("Contour");
        
        int totalCount = 0;
        String str;
        
        int xUnits = img.getFileInfo(0).getUnitsOfMeasure()[0];
        int yUnits = img.getFileInfo(0).getUnitsOfMeasure()[1];
        int zUnits = Unit.UNKNOWN_MEASURE.getLegacyNum();
        
        for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {
    
            //add statistic to column list if selected by user
            if (checkList[i]) {
                if ( (VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits)
                        && (xUnits == zUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = img.getFileInfo(0).getVolumeUnitsOfMeasureStr().trim();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Area") != -1)
                        && (xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = img.getFileInfo(0).getAreaUnitsOfMeasureStr().trim();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1)
                        && (xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = (Unit.getUnitFromLegacyNum(xUnits)).getAbbrev();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (degrees)");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1)
                        && (xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = (Unit.getUnitFromLegacyNum(xUnits)).getAbbrev();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1)
                        && (xUnits == yUnits) && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = (Unit.getUnitFromLegacyNum(xUnits)).getAbbrev();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else {
                    logModelCol.add(VOIStatisticList.statisticDescription[i]);
                }
    
                // total count used for total # of data elemets, need to add 3 if color
                // image and intensity related (R,G,B)
                totalCount++;
    
                if (img.isColorImage() && (VOIStatisticList.statisticDescription[i].indexOf("Intensity") != -1)) {
                    totalCount += 2;
                }
            }
        }

        return logModelCol;
    }
    
    private Vector<Vector<String>> getStatsData(Vector<VOIStatisticalProperties> statsList, VOIVector VOIs) {
    	Vector<Vector<String>> data = new Vector<Vector<String>>();
    	int voiIndex = 0;
    	for (VOIStatisticalProperties prop : statsList) {
    		VOIBaseVector contours = VOIs.get(voiIndex).getCurves();
    		for (VOIBase voi : contours) {
	    		Vector<String> row = new Vector<String>();
	    		String contourLabel = voi.getLabel();
	    		row.add(VOIs.get(voiIndex).getName().replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
	    		row.add(contourLabel.replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
		    	for (int i = 0; i < checkList.length; i++) {
		        	if (checkList[i]) {
		        		row.add(prop.getProperty(VOIStatisticList.statisticDescription[i] + contourLabel).replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
		        	}
		        }
		    	data.add(row);
    		}
	    	voiIndex++;
    	}
    	
    	return data;
    	
    	// end = contours[slice].elementAt(num).getLabel()
    	//VOIBaseVector contours = ((VOI) list.getElementAt(i)).getCurves();
        //updateDialogRow((VOI) list.getElementAt(i), new Vector[]{contours}, properties, list, i, rowData, totalData);
    }
    
    public Vector<ModelImage> getResultImages() {
    	return resultImages;
    }
}