import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * PlugInAlgorithmNucleiStatistics is used to identify nuclei and output statistics for each nucleus
 * @version  May 9, 2013
 * @author   William Gandler
 * @see      AlgorithmBase 
 */
public class PlugInAlgorithmNucleiStatistics extends AlgorithmBase {
  //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** The list of files to try to process with the algorithm. */
    private Vector<File> inputFiles;
    
    /** A count of the number of images processed. */
    private int numProcessedImages = 0;
    
    /** The names of the all stats columns, saved from the first image processed. */
    private Vector<String> allStatsColumnNames = new Vector<String>();
    
    /** Vector of the stats for all the images. */
    private Vector<Vector<String>> allStatsColumns = new Vector<Vector<String>>();
    
    /** Whether to force the resolution of all images to 1um. */
    private boolean forceResolution = true;
    
    private static final String[] statsToCalculate = new String[] {VOIStatisticalProperties.quantityDescription, VOIStatisticalProperties.areaDescription, 
		VOIStatisticalProperties.perimeterDescription, VOIStatisticalProperties.circularityDescription,
		VOIStatisticalProperties.solidityDescription, VOIStatisticalProperties.eccentricityDescription, VOIStatisticalProperties.meanCurvatureDescription,
        VOIStatisticalProperties.stdDevCurvatureDescription, VOIStatisticalProperties.meanNegativeCurvatureDescription,
        VOIStatisticalProperties.numberOfIndentationsCurvatureDescription/*, VOIStatisticalProperties.numberOfIndentationsHullDescription*/};

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
    public PlugInAlgorithmNucleiStatistics(Vector<File> inputFiles, boolean forceResolution) {
        super(null, null);
        this.inputFiles = inputFiles;
        this.forceResolution = forceResolution;
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
    	int progressPerImg = 100 / inputFiles.size();
    	int curProgress = 0;
    	
    	Vector<byte[]> gCurvature = new Vector<byte[]>();
        Vector<Double> gMeanNegativeCurvature = new Vector<Double>();
    	for (File inFile : inputFiles) {
        	fireProgressStateChanged("Opening image " + inFile.getName() + " ...");
        	srcImage = openFile(inFile);

	        if (srcImage == null) {
	            System.err.println("Source Image is null - skipping: " + inFile.getName());
	            continue;
	        }
	        
	        fireProgressStateChanged("Loading nuclei segmentations for " + inFile.getName() + " ...");
	        curProgress += progressPerImg / 3;
        	fireProgressStateChanged(curProgress);
	        loadAllVOIs(srcImage, false);

	        // run statistics generator on VOIs in image and output to file on disk
	        fireProgressStateChanged("Calculating statistics for " + inFile.getName() + " ...");
	        curProgress += progressPerImg / 3;
        	fireProgressStateChanged(curProgress);
	        outputStatistics(srcImage, gCurvature, gMeanNegativeCurvature);
	        
	        curProgress += progressPerImg / 3;
        	fireProgressStateChanged(curProgress);
	        
	        numProcessedImages++;
	        
	        // cleanup current image
        	srcImage.disposeLocal();
        	srcImage = null;
        }
        
        outputAllStatsFile();
        outputCurvatureFile(gCurvature, gMeanNegativeCurvature);
        
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
    private ModelImage openFile(File file) {
    	ModelImage img = null;
    	
		System.err.println("Trying to open file:\t" + file.getName());
    	
    	FileIO io = new FileIO();
    	try {
    		img = io.readImage(file.getAbsolutePath());
    	} catch (Exception e) {
    		System.err.println("Failed to open file:\t" + file.getName());
    		e.printStackTrace();
    		img = null;
    	}
    	
    	if (forceResolution) {
    		FileInfoBase[] fInfos = img.getFileInfo();
    		for (int i = 0; i < fInfos.length; i++) {
    			for (int j = 0; j < img.getNDims(); j++) {
    				fInfos[i].setUnitsOfMeasure(Unit.MICROMETERS, j);
    				fInfos[i].setResolutions(1.0f, j);
    			}
    		}
    	}
    	
    	return img;
    }
    
    /**
     * This method loads all VOIs to the active image from the default VOI directory for that image.
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    private static void loadAllVOIs(ModelImage img, boolean quietMode) {
        String fileDir = img.getFileInfo(0).getFileDirectory();
        String imageName;

        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        if (img.isDicomImage()) {
            imageName = img.getFileInfo(0).getFileName();

            final int index = imageName.lastIndexOf(".");

            if (index > 0) {
                imageName = imageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = imageName.length();

            for (int i = imageName.length() - 1; i >= 0; i--) {
                final char myChar = imageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                imageName = new String("DICOM");
            } else {
                imageName = imageName.substring(0, newIndex);
            }
        } else {
            imageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = imageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        String voiDir = new String(fileDir + File.separator + "defaultVOIs_" + imageName + File.separator);
        
        int i, j;
        VOI[] VOIs;
        FileVOI fileVOI;

        try {

            // if voiDir does not exist, then return
            // if voiDir exists, then get list of voi's from directory (*.voi)
            final File voiFileDir = new File(voiDir);
            final Vector<String> filenames = new Vector<String>();
            final Vector<Boolean> isLabel = new Vector<Boolean>();

            if (voiFileDir.exists() && voiFileDir.isDirectory()) {

                // get list of files
                final File[] files = voiFileDir.listFiles();

                for (final File element : files) {

                    if (element.getName().endsWith(".voi") || element.getName().endsWith(".xml")) {
                        filenames.add(element.getName());
                        isLabel.add(false);
                    } else if (element.getName().endsWith(".lbl")) {
                        filenames.add(element.getName());
                        isLabel.add(true);
                    }
                }
            } else { // voiFileDir either doesn't exist, or isn't a directory

                if ( !quietMode) {
                    MipavUtil.displayError("No VOIs are found in directory: " + voiDir);
                }

                return;
            }

            // open each voi array, then register voi array to this image
            for (i = 0; i < filenames.size(); i++) {

                fileVOI = new FileVOI( (filenames.elementAt(i)), voiDir, img);

                VOIs = fileVOI.readVOI(isLabel.get(i));

                //if(m_kCurrentVOIGroup != null) {
                //    advanceVOIUID();
                //}
                for (j = 0; j < VOIs.length; j++) {
                    if(VOIs[j].getColor() == null) {
                        //VOIs[j].setColor(toolbarBuilder.getVOIColorButton().getBackground());
                    }
                    img.registerVOI(VOIs[j]);
                    //VOIs[j].addVOIListener(this);
                    //advanceVOIUID();
                }
            }

            // when everything's done, notify the image listeners
            img.notifyImageDisplayListeners();

        } catch (final Exception error) {

            if ( !quietMode) {
                MipavUtil.displayError("Error loading all VOIs from " + voiDir + ": " + error);
            }
        }
    }
    
    private void outputStatistics(ModelImage img, Vector<byte[]> gCurvature, Vector<Double> gMeanNegativeCurvature) {
        boolean smoothCurvature = true;
        double negativeHysteresisFraction = 0.25;
        double positiveHysteresisFraction = 0.25;
        int consecutiveNegativeNeeded = 2;
        double negativeCurvatureNeeded = -2.5E-2;
    	AlgorithmVOIProps calculator = new AlgorithmVOIProps(img, AlgorithmVOIProps.PROCESS_PER_CONTOUR, RangeType.NO_RANGE, img.getVOIs());
    	calculator.setSmoothCurvature(smoothCurvature);
    	calculator.setNegativeHysteresisFraction(negativeHysteresisFraction);
    	calculator.setPositiveHysteresisFraction(positiveHysteresisFraction);
    	calculator.setConsecutiveNegativeNeeded(consecutiveNegativeNeeded);
    	calculator.setNegativeCurvatureNeeded(negativeCurvatureNeeded);
    	calculator.setSelectedStatistics(checkList);
    	calculator.setShowTotals(false);
    	calculator.run();
    	
    	Vector<VOIStatisticalProperties> statsList = new Vector<VOIStatisticalProperties>(img.getVOIs().size());
    	for (VOI voi : img.getVOIs()) {
    		statsList.add(calculator.getVOIProperties(voi));
    	}
    	
    	File statsDir = new File(img.getFileInfo(0).getFileDirectory() + File.separator + "statistics" + File.separator);
    	if (!statsDir.exists()) { 
    		statsDir.mkdirs();
    	}
    	
    	String statsOutputFile = statsDir.getAbsolutePath() + File.separator + img.getImageName() + ".table";
    	Vector<String> columnHeaders = getColumnHeaders(img);
    	if (allStatsColumnNames.size() == 0) {
    		allStatsColumnNames.addAll(columnHeaders);
    	}
    	
    	Vector<Vector<String>> columns = getStatsData(statsList, img.getVOIs(), img, gCurvature, gMeanNegativeCurvature);
    	allStatsColumns.addAll(columns);
    	
    	writeStatisticFile(statsOutputFile, columnHeaders, columns);
    }
    
    private void outputAllStatsFile() {
    	File statsDir = new File(inputFiles.elementAt(0).getParent() + File.separator + "statistics" + File.separator);
    	if (!statsDir.exists()) { 
    		statsDir.mkdirs();
    	}
    	
    	String statsOutputFile = statsDir.getAbsolutePath() + File.separator + "all_statistics" + ".table";
    	
    	writeStatisticFile(statsOutputFile, allStatsColumnNames, allStatsColumns);
    }
    
    private void outputCurvatureFile(Vector<byte[]> gCurvature, Vector<Double> gMeanNegativeCurvature) {
        File statsDir = new File(inputFiles.elementAt(0).getParent() + File.separator + "statistics" + File.separator);
        if (!statsDir.exists()) { 
            statsDir.mkdirs();
        }
        
        // Sort nuclei in order of increasing mean negative curvature
        ArrayList<negativeMeanCurvatureIndexItem> curvIndexList = new ArrayList<negativeMeanCurvatureIndexItem>();
        int nCurves = gMeanNegativeCurvature.size();
        for (int i = 0; i < nCurves; i++) {
            curvIndexList.add(new negativeMeanCurvatureIndexItem(gMeanNegativeCurvature.get(i), i));
        }
        Collections.sort(curvIndexList, new negativeMeanCurvatureIndexComparator());
        Vector<byte[]> gCurvature2 = new Vector<byte[]>();
        for (int i = 0; i < nCurves; i++) {
            int j = curvIndexList.get(i).getIndex();
            gCurvature2.add(gCurvature.get(j));
        }
        
        int xDim = nCurves;
        int yDim = 801;
        int sliceSize = xDim * yDim;
        byte buffer[] = new byte[sliceSize];
        for (int x = 0; x < xDim; x++) {
            byte curv[] = gCurvature2.get(x);
            for (int y = 0; y < yDim; y++) {
                buffer[x + xDim * y] = curv[y];
            }
        }
        int extents[] = new int[2];
        extents[0] = xDim;
        extents[1] = yDim;
        String imageName = "all_curvatures";
        ModelImage curvatureImage = new ModelImage(ModelStorageBase.UBYTE, extents, imageName);
        curvatureImage.setImageName(imageName + ".tif");
        curvatureImage.getFileInfo()[0].setFileDirectory(statsDir.getAbsolutePath() + File.separator);
        try {
            curvatureImage.importData(0, buffer, true);
        } catch (IOException error) {
            MipavUtil.displayError("IOException " + error + " on curvatureImage.importData(0, buffer, true)");
            return;
        }
        // Indexed color LUT is saved with image
        curvatureImage.getFileInfo()[0].setPhotometric((short) 3);
        int[] dimExtentsLUT = new int[2];
        dimExtentsLUT[0] = 4;
        dimExtentsLUT[1] = 256;
        int colorsUsed = 256;

        ModelLUT LUT = new ModelLUT(ModelLUT.SPECTRUM, colorsUsed, dimExtentsLUT);
        FileTiff imageFile = null;
        try {
            imageFile = new FileTiff(imageName + ".tif", statsDir.getAbsolutePath() + File.separator);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on new FileTiff");
            return;
        }
        FileWriteOptions options = new FileWriteOptions(imageName + ".tif", statsDir.getAbsolutePath() + File.separator, true);
        try {
            imageFile.writeImage(curvatureImage, LUT, options);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on imageFile.writeImage");
            return;
        }
    }
    
    /**
     * Writes out the statistics file based on the current logModel
     */
    private void writeStatisticFile(String tableDestination, Vector<String> columnHeaders, Vector<Vector<String>> columns) {
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
            
            logFileText = writeStatsToString(columnHeaders, columns);
            addSummaryLines(logFileText, columns.size());

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
    protected StringBuffer writeStatsToString(Vector<String> columnHeaders, Vector<Vector<String>> column) {
        StringBuffer total = new StringBuffer();
        String newLine = System.getProperty("line.separator");
        //get column names
        for(int i=0; i<columnHeaders.size(); i++) {
            total.append(columnHeaders.get(i)).append("\t");
        }
        total.append(newLine);
        
        //get total data
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
    
    private void addSummaryLines(StringBuffer output, int numLines) {
    	String newLine = System.getProperty("line.separator");
    	output.append("Summary").append("\t"); // file
    	output.append(" ").append("\t"); // VOI name
    	output.append("=AVERAGE(C2:C" + (numLines + 1) + ")").append("\t");
    	output.append("=AVERAGE(D2:D" + (numLines + 1) + ")").append("\t");
    	output.append("=AVERAGE(E2:E" + (numLines + 1) + ")").append("\t");
    	output.append("=AVERAGE(F2:F" + (numLines + 1) + ")").append("\t");
    	output.append("=AVERAGE(G2:G" + (numLines + 1) + ")").append("\t");
    	output.append("=AVERAGE(H2:H" + (numLines + 1) + ")").append("\t");
    	output.append("=AVERAGE(I2:I" + (numLines + 1) + ")").append("\t");
    	output.append("=AVERAGE(J2:J" + (numLines + 1) + ")").append("\t");
    	output.append("=AVERAGE(K2:K" + (numLines + 1) + ")").append("\t");
    	output.append("=SUM(L2:L" + (numLines + 1) + ")").append("\t");
    	output.append(newLine);
    }
    
    /**
     * Writes the column titles of selected statistics calculations to the logModel.
     */
    private static final Vector<String> getColumnHeaders(ModelImage img) {
        Vector<String> logModelCol = new Vector<String>();
        logModelCol.add("Image file name");
        logModelCol.add("Name");
        
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
    
                // total count used for total # of data elements, need to add 3 if color
                // image and intensity related (R,G,B)
                totalCount++;
    
                if (img.isColorImage() && (VOIStatisticList.statisticDescription[i].indexOf("Intensity") != -1)) {
                    totalCount += 2;
                }
            }
        }

        return logModelCol;
    }
    
    private Vector<Vector<String>> getStatsData(Vector<VOIStatisticalProperties> statsList, VOIVector VOIs, ModelImage img,
                                                Vector<byte[]> gCurvature, Vector<Double> gMeanNegativeCurvature) {
        //                          minCurvature             maxCurvature
        // HGADFN167_LAC_40X_1      -0.3075                   0.2463
        // HGADFN167_LAC_40X_2      -0.08464                  0.1443
        // HGADFN167_LAC_40X_3      -0.4589                   0.4927
        // HGADFN167_LAC_40X_4      -1.6375                   1.774
        // HGADFN167_LAC_40X_5      -0.1258                   0.1614
        // HGADFN167_LAC_40X_6      -1.196                    0.3777
        // HGADFN167_LAC_40X_7      -0.1428                   0.1725
        // HGADFN167_LAC_40X_8      -0.5064                   5.987
        // HGFDFN168_LAC_40X_1      -0.0923                   0.2574
        // HGFDFN168_LAC_40X_2      -0.2630                   0.1806
        // HGFDFN168_LAC_40X_3      -0.1153                   0.1691
        // HGFDFN168_LAC_40X_4      -0.09794                  0.1551
        // HGFDFN168_LAC_40X_5      -0.1876                   0.2318
        // HGFDFN168_LAC_40X_6      -0.2035                   0.2853
        // HGFDFN168_LAC_40X_7      -0.09728                  0.2410
        // HGFDFN168_LAC_40X_8      -0.5568                   0.3128

    	Vector<Vector<String>> data = new Vector<Vector<String>>();
    	int voiIndex = 0;
    	for (VOIStatisticalProperties prop : statsList) {
    		VOIBaseVector contours = VOIs.get(voiIndex).getCurves();
    		for (VOIBase voi : contours) {
    		    // The first curvature displayed will be farthest from the geometric center of the nucleus
    		    Vector3f gCenter = voi.getGeometricCenter();
    		    Vector<Vector3f> positions = new Vector<Vector3f>();
                Vector<Float> curvature = new Vector<Float>();
                double meanCurvature[] = new double[1];
                double stdDevCurvature[] = new double[1];
                double meanNegativeCurvature[] = new double[1];
                int numberOfIndentations[] = new int[1];
                double totalLength[] = new double[1];
                boolean smoothCurvature = true;
                double negativeHysteresisFraction = 0.25;
                double positiveHysteresisFraction = 0.25;
                int consecutiveNegativeNeeded = 2;
                double negativeCurvatureNeeded = -2.5E-2;
                voi.findPositionAndCurvature(positions, curvature, smoothCurvature, meanCurvature,
                                             stdDevCurvature, meanNegativeCurvature, negativeHysteresisFraction,
                                             positiveHysteresisFraction, numberOfIndentations, consecutiveNegativeNeeded,
                                             negativeCurvatureNeeded, totalLength);
                double maxDistSquared = -Double.MAX_VALUE;
                int nPoints = positions.size();
                int startingIndex = -1;
                double minCurvature = Double.MAX_VALUE;
                double maxCurvature = -Double.MAX_VALUE;
                for (int i = 0; i < nPoints; i++) {
                    if (curvature.get(i) > maxCurvature) {
                        maxCurvature = curvature.get(i);
                    }
                    if (curvature.get(i) < minCurvature) {
                        minCurvature = curvature.get(i);
                    }
                    double distX = positions.get(i).X - gCenter.X;
                    double distY = positions.get(i).Y - gCenter.Y;
                    double distSquared = distX*distX + distY*distY;
                    if (distSquared > maxDistSquared) {
                        maxDistSquared = distSquared;
                        startingIndex = i;
                    }
                }
                double initDistance[] = new double[nPoints];
                float initCurvature[] = new float[nPoints];
                int index = 0;
                float startingDistance = positions.get(startingIndex).Z;
                for (int i = startingIndex; i < nPoints; i++) {
                    initDistance[index] = 800.0*((positions.get(i).Z - startingDistance)/totalLength[0]);
                    initCurvature[index++] = curvature.get(i);
                }
                double previousDistance = 800.0*(positions.get(nPoints-1).Z - startingDistance)/totalLength[0];
                for (int i = 0; i < startingIndex; i++) {
                    initDistance[index] = previousDistance + (800.0*(positions.get(i).Z/totalLength[0]));
                    initCurvature[index++] = curvature.get(i);    
                }
                double interpCurvature[] = new double[801];
                double lowerDistance;
                double upperDistance;
                // initDistance[0] = 0.0
                interpCurvature[0] = initCurvature[0];
                interpCurvature[800] = initCurvature[0];
                index = 1;
                for (int i = 1; i <= 799; i++) {
                   while ((i > initDistance[index]) && (index < nPoints-1)) {
                       index++;
                   }
                   if (i >= initDistance[nPoints-1]) {
                       lowerDistance = i - initDistance[nPoints-1];
                       upperDistance = 800.0 - i;
                       interpCurvature[i] = (upperDistance*initCurvature[nPoints-1] + lowerDistance*initCurvature[0])/
                                             (lowerDistance + upperDistance);
                   }
                   else {
                       lowerDistance = i - initDistance[index-1];
                       upperDistance = initDistance[index] - i;
                       interpCurvature[i] = (upperDistance*initCurvature[index-1] + lowerDistance*initCurvature[index])/
                                                                 (lowerDistance+upperDistance);
                   }
                } // for (int i = 1; i <= 799; i++)
                byte scaledCurvature[] = new byte[801];
                double minCurv = -0.5;
                double maxCurv = 0.5;
                double sCurv;
                for (int i = 0; i <= 800; i++) {
                    sCurv = 255.0*(interpCurvature[i] - minCurv)/(maxCurv - minCurv);
                    scaledCurvature[i] = (byte)Math.round(Math.min(255.0, Math.max(0.0, sCurv)));
                }
                gCurvature.add(scaledCurvature);
                gMeanNegativeCurvature.add(meanNegativeCurvature[0]);
                
	    		Vector<String> row = new Vector<String>();
	    		String contourLabel = voi.getLabel();
	    		row.add(img.getImageFileName().replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
	    		row.add(VOIs.get(voiIndex).getName().replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
	    		//row.add(contourLabel.replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
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
    
    public int getNumProcessedImages() {
    	return numProcessedImages;
    }
    
    private class negativeMeanCurvatureIndexComparator implements Comparator<negativeMeanCurvatureIndexItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final negativeMeanCurvatureIndexItem o1, final negativeMeanCurvatureIndexItem o2) {
            final double a = o1.getNegativeMeanCurvature();
            final double b = o2.getNegativeMeanCurvature();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }
    
    private class negativeMeanCurvatureIndexItem {

        /** DOCUMENT ME! */
        private final double negativeMeanCurvature;

        /** DOCUMENT ME! */
        private final int index;

        /**
         * Creates a new negativeMeanCurvatureIndexItem object.
         * 
         * @param negativeMeanCurvature
         * @param index
         */
        public negativeMeanCurvatureIndexItem(final double negativeMeanCurvature, final int index) {
            this.negativeMeanCurvature = negativeMeanCurvature;
            this.index = index;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getNegativeMeanCurvature() {
            return negativeMeanCurvature;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getIndex() {
            return index;
        }

    }
}