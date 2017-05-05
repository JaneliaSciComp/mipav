import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmVOIProps;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * PlugInAlgorithmNucleiStatistics is used to identify nuclei and output statistics for each nucleus
 * 
 * @version May 9, 2013
 * @author William Gandler
 * @see AlgorithmBase
 */
public class PlugInAlgorithmNucleiStatistics extends AlgorithmBase {
    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The list of files to try to process with the algorithm. */
    private Vector<File> inputFiles;

    /** A count of the number of images processed. */
    private int numProcessedImages = 0;

    /** The names of the all stats columns, saved from the first image processed. */
    private final Vector<String> allStatsColumnNames = new Vector<String>();

    /** Vector of the stats for all the images. */
    private final Vector<Vector<String>> allStatsColumns = new Vector<Vector<String>>();

    /** Whether to force the resolution of all images to a new resolution. */
    private boolean forceResolutionFlag = true;

    /** The new resolution (in um) to set for the images if the force resolution flag is set. */
    private final double forceResolutionValue;

    private static final String[] statsToCalculate = new String[] {VOIStatisticalProperties.quantityDescription, VOIStatisticalProperties.areaDescription,
            VOIStatisticalProperties.perimeterDescription, VOIStatisticalProperties.circularityDescription, VOIStatisticalProperties.solidityDescription,
            VOIStatisticalProperties.eccentricityDescription, VOIStatisticalProperties.meanCurvatureDescription,
            VOIStatisticalProperties.stdDevCurvatureDescription, VOIStatisticalProperties.meanNegativeCurvatureDescription,
            VOIStatisticalProperties.numberOfIndentationsCurvatureDescription/*
                                                                              * , VOIStatisticalProperties.
                                                                              * numberOfIndentationsHullDescription
                                                                              */};

    private static final boolean[] checkList = new boolean[VOIStatisticalProperties.numberOfStatistics];

    static {
        final String[] statDescr = VOIStatisticalProperties.statisticDescription;
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
    public PlugInAlgorithmNucleiStatistics(final Vector<File> inputFiles, final boolean forceResolutionFlag, final double forceResolutionValue) {
        super(null, null);
        this.inputFiles = inputFiles;
        this.forceResolutionFlag = forceResolutionFlag;
        this.forceResolutionValue = forceResolutionValue;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    @Override
    public void finalize() {
        inputFiles = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    @Override
    public void runAlgorithm() {
        final int progressPerImg = 100 / inputFiles.size();
        int curProgress = 0;

        final Vector<byte[]> gCurvature = new Vector<byte[]>();
        final Vector<Double> gMeanNegativeCurvature = new Vector<Double>();
        for (final File inFile : inputFiles) {
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
     * 
     * @param file The file to try to open.
     * @return The ModelImage of the specified file, or null if there was an error.
     */
    private ModelImage openFile(final File file) {
        ModelImage img = null;
        ModelImage grayImg = null;

        System.err.println("Trying to open file:\t" + file.getName());

        final FileIO io = new FileIO();
        try {
            img = io.readImage(file.getAbsolutePath());
        } catch (final Exception e) {
            System.err.println("Failed to open file:\t" + file.getName());
            e.printStackTrace();
            img = null;
        }

        if (forceResolutionFlag) {
            final FileInfoBase[] fInfos = img.getFileInfo();
            for (int i = 0; i < fInfos.length; i++) {
                for (int j = 0; j < img.getNDims(); j++) {
                    fInfos[i].setUnitsOfMeasure(Unit.MICROMETERS, j);
                    fInfos[i].setResolutions((float) forceResolutionValue, j);
                }
            }
        }

        if ( !img.isColorImage()) {
            return img;
        } else {
            grayImg = createGrayImage(img);
            return grayImg;
        }
    }

    /**
     * create a gray image from the input color image
     */
    private ModelImage createGrayImage(ModelImage img) {
        ModelImage grayImg;
        final String originalName = img.getImageName();
        img.setImageName(originalName + "tmp");

        if (img.getType() == ModelStorageBase.ARGB) {
            grayImg = new ModelImage(ModelStorageBase.UBYTE, img.getExtents(), originalName);
        } else if (img.getType() == ModelStorageBase.ARGB_USHORT) {
            grayImg = new ModelImage(ModelStorageBase.USHORT, img.getExtents(), originalName);
        } else if (img.getType() == ModelStorageBase.ARGB_FLOAT) {
            grayImg = new ModelImage(ModelStorageBase.FLOAT, img.getExtents(), originalName);
        } else {
            // default to standard rgb
            grayImg = new ModelImage(ModelStorageBase.UBYTE, img.getExtents(), originalName);
        }

        for (int n = 0; n < img.getFileInfo().length; n++) {
            final FileInfoBase fInfoBase = (FileInfoBase) (img.getFileInfo(n).clone());
            fInfoBase.setDataType(grayImg.getType());
            grayImg.setFileInfo(fInfoBase, n);
        }

        // Make algorithm
        float redValue = 0.0f;
        float greenValue = 0.0f;
        float blueValue = 0.0f;
        final double redMin = img.getMinR();
        final double redMax = img.getMaxR();
        final double greenMin = img.getMinG();
        final double greenMax = img.getMaxG();
        final double blueMin = img.getMinB();
        final double blueMax = img.getMaxB();
        if (redMin != redMax) {
            redValue = 1.0f;
        } else if (greenMin != greenMax) {
            greenValue = 1.0f;
        } else if (blueMin != blueMax) {
            blueValue = 1.0f;
        }
        final AlgorithmRGBtoGray RGBAlgo = new AlgorithmRGBtoGray(grayImg, img, redValue, greenValue, blueValue, false, 0f, false, true, 0f, 0f, 0.0f, 0f,
                0.0f, 0f);

        RGBAlgo.run();

        img.disposeLocal();
        img = null;

        return grayImg;
    }

    /**
     * This method loads all VOIs to the active image from the default VOI directory for that image.
     * 
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    private static void loadAllVOIs(final ModelImage img, final boolean quietMode) {
        final String fileDir = img.getFileInfo(0).getFileDirectory();
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

        final String voiDir = new String(fileDir + File.separator + "defaultVOIs_" + imageName + File.separator);

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

                // if(m_kCurrentVOIGroup != null) {
                // advanceVOIUID();
                // }
                for (j = 0; j < VOIs.length; j++) {
                    if (VOIs[j].getColor() == null) {
                        // VOIs[j].setColor(toolbarBuilder.getVOIColorButton().getBackground());
                    }
                    img.registerVOI(VOIs[j]);
                    // VOIs[j].addVOIListener(this);
                    // advanceVOIUID();
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

    private void outputStatistics(final ModelImage img, final Vector<byte[]> gCurvature, final Vector<Double> gMeanNegativeCurvature) {
        final boolean smoothCurvature = true;
        final double negativeHysteresisFraction = 0.25;
        final double positiveHysteresisFraction = 0.25;
        final int consecutiveNegativeNeeded = 2;
        final double negativeCurvatureNeeded = -2.5E-2;
        final AlgorithmVOIProps calculator = new AlgorithmVOIProps(img, AlgorithmVOIProps.PROCESS_PER_CONTOUR, RangeType.NO_RANGE, img.getVOIs());
        calculator.setSmoothCurvature(smoothCurvature);
        calculator.setNegativeHysteresisFraction(negativeHysteresisFraction);
        calculator.setPositiveHysteresisFraction(positiveHysteresisFraction);
        calculator.setConsecutiveNegativeNeeded(consecutiveNegativeNeeded);
        calculator.setNegativeCurvatureNeeded(negativeCurvatureNeeded);
        calculator.setSelectedStatistics(checkList);
        calculator.setShowTotals(false);
        calculator.run();

        final Vector<VOIStatisticalProperties> statsList = new Vector<VOIStatisticalProperties>(img.getVOIs().size());
        for (final VOI voi : img.getVOIs()) {
            statsList.add(calculator.getVOIProperties(voi));
        }

        final File statsDir = new File(img.getFileInfo(0).getFileDirectory() + File.separator + "statistics" + File.separator);
        if ( !statsDir.exists()) {
            statsDir.mkdirs();
        }

        final String statsOutputFile = statsDir.getAbsolutePath() + File.separator + img.getImageName() + ".table";
        final Vector<String> columnHeaders = getColumnHeaders(img);
        if (allStatsColumnNames.size() == 0) {
            allStatsColumnNames.addAll(columnHeaders);
        }

        final Vector<Vector<String>> columns = getStatsData(statsList, img.getVOIs(), img, gCurvature, gMeanNegativeCurvature);
        allStatsColumns.addAll(columns);

        writeStatisticFile(statsOutputFile, columnHeaders, columns);
    }

    private void outputAllStatsFile() {
        final File statsDir = new File(inputFiles.elementAt(0).getParent() + File.separator + "statistics" + File.separator);
        if ( !statsDir.exists()) {
            statsDir.mkdirs();
        }

        final String statsOutputFile = statsDir.getAbsolutePath() + File.separator + "all_statistics" + ".table";

        writeStatisticFile(statsOutputFile, allStatsColumnNames, allStatsColumns);
    }

    private void outputCurvatureFile(final Vector<byte[]> gCurvature, final Vector<Double> gMeanNegativeCurvature) {
        final File statsDir = new File(inputFiles.elementAt(0).getParent() + File.separator + "statistics" + File.separator);
        if ( !statsDir.exists()) {
            statsDir.mkdirs();
        }

        // Sort nuclei in order of increasing mean negative curvature
        final ArrayList<negativeMeanCurvatureIndexItem> curvIndexList = new ArrayList<negativeMeanCurvatureIndexItem>();
        final int nCurves = gMeanNegativeCurvature.size();
        for (int i = 0; i < nCurves; i++) {
            curvIndexList.add(new negativeMeanCurvatureIndexItem(gMeanNegativeCurvature.get(i), i));
        }
        Collections.sort(curvIndexList, new negativeMeanCurvatureIndexComparator());
        final Vector<byte[]> gCurvature2 = new Vector<byte[]>();
        for (int i = 0; i < nCurves; i++) {
            final int j = curvIndexList.get(i).getIndex();
            gCurvature2.add(gCurvature.get(j));
        }

        final int xDim = nCurves;
        final int yDim = 801;
        final int sliceSize = xDim * yDim;
        final byte buffer[] = new byte[sliceSize];
        for (int x = 0; x < xDim; x++) {
            final byte curv[] = gCurvature2.get(x);
            for (int y = 0; y < yDim; y++) {
                buffer[x + xDim * y] = curv[y];
            }
        }
        final int extents[] = new int[2];
        extents[0] = xDim;
        extents[1] = yDim;
        final String imageName = "all_curvatures";
        final ModelImage curvatureImage = new ModelImage(ModelStorageBase.UBYTE, extents, imageName);
        curvatureImage.setImageName(imageName + ".tif");
        curvatureImage.getFileInfo()[0].setFileDirectory(statsDir.getAbsolutePath() + File.separator);
        try {
            curvatureImage.importData(0, buffer, true);
        } catch (final IOException error) {
            MipavUtil.displayError("IOException " + error + " on curvatureImage.importData(0, buffer, true)");
            return;
        }
        // Indexed color LUT is saved with image
        curvatureImage.getFileInfo()[0].setPhotometric((short) 3);
        final int[] dimExtentsLUT = new int[2];
        dimExtentsLUT[0] = 4;
        dimExtentsLUT[1] = 256;
        final int colorsUsed = 256;

        final ModelLUT LUT = new ModelLUT(ModelLUT.SPECTRUM, colorsUsed, dimExtentsLUT);
        FileTiff imageFile = null;
        try {
            imageFile = new FileTiff(imageName + ".tif", statsDir.getAbsolutePath() + File.separator);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException " + e + " on new FileTiff");
            return;
        }
        final FileWriteOptions options = new FileWriteOptions(imageName + ".tif", statsDir.getAbsolutePath() + File.separator, true);
        try {
            imageFile.writeImage(curvatureImage, LUT, options);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException " + e + " on imageFile.writeImage");
            return;
        }
    }

    /**
     * Writes out the statistics file based on the current logModel
     */
    private void writeStatisticFile(final String tableDestination, final Vector<String> columnHeaders, final Vector<Vector<String>> columns) {
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
                MipavUtil.displayError("security violation incurred while creating \"" + statFile.getName() + "\"; \n"
                        + "is destination directory still writable?  " + "Table file not written.");
            }

            Preferences.debug("security violation incurred while creating \"" + statFile.getName() + "\";\n");

            return;
        }

        try {

            if ( !statFile.createNewFile()) { /* there was an error here! */
            }
        } catch (final IOException io) {
            Preferences.debug("IOexception error in creating statFile!  threw " + "exception rather than createNewFile == false;\n" + io);
            io.printStackTrace();
            Preferences.debug("IO exception while writing VOIStatistic's \"" + statFile.getAbsolutePath() + "\"\n");

            return;
        } catch (final SecurityException se) {

            if (noisyProcess) {
                MipavUtil.displayError("security violation incurred while creating \"" + statFile.getAbsolutePath() + "\"; \n"
                        + "is destination directory still writable?  " + "Table file not written.");
            }

            Preferences.debug("security violation incurred while creating \"" + statFile.getAbsolutePath() + "\";\n");

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
    protected StringBuffer writeStatsToString(final Vector<String> columnHeaders, final Vector<Vector<String>> column) {
        final StringBuffer total = new StringBuffer();
        final String newLine = System.getProperty("line.separator");
        // get column names
        for (int i = 0; i < columnHeaders.size(); i++) {
            total.append(columnHeaders.get(i)).append("\t");
        }
        total.append(newLine);

        // get total data
        Vector<String> row;
        String cellEntry;
        for (int i = 0; i < column.size(); i++) {
            row = column.get(i);
            for (int j = 0; j < row.size(); j++) {
                if (row.get(j) == null || row.get(j).toString().length() == 0) {
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

    private void addSummaryLines(final StringBuffer output, final int numLines) {
        final String newLine = System.getProperty("line.separator");
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
    private static final Vector<String> getColumnHeaders(final ModelImage img) {
        final Vector<String> logModelCol = new Vector<String>();
        logModelCol.add("Image file name");
        logModelCol.add("Name");

        int totalCount = 0;
        String str;

        final int xUnits = img.getFileInfo(0).getUnitsOfMeasure()[0];
        final int yUnits = img.getFileInfo(0).getUnitsOfMeasure()[1];
        final int zUnits = Unit.UNKNOWN_MEASURE.getLegacyNum();

        for (int i = 0; i < VOIStatisticList.numberOfStatistics; i++) {

            // add statistic to column list if selected by user
            if (checkList[i]) {
                if ( (VOIStatisticList.statisticDescription[i].indexOf("Volume") != -1) && (xUnits == yUnits) && (xUnits == zUnits)
                        && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = img.getFileInfo(0).getVolumeUnitsOfMeasureStr().trim();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Area") != -1) && (xUnits == yUnits)
                        && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = img.getFileInfo(0).getAreaUnitsOfMeasureStr().trim();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Perimeter") != -1) && (xUnits == yUnits)
                        && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = (Unit.getUnitFromLegacyNum(xUnits)).getAbbrev();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if (VOIStatisticList.statisticDescription[i].indexOf("Principal Axis") != -1) {
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (degrees)");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Major axis length") != -1) && (xUnits == yUnits)
                        && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
                    str = (Unit.getUnitFromLegacyNum(xUnits)).getAbbrev();
                    logModelCol.add(VOIStatisticList.statisticDescription[i] + " (" + str + ")");
                } else if ( (VOIStatisticList.statisticDescription[i].indexOf("Minor axis length") != -1) && (xUnits == yUnits)
                        && (xUnits != Unit.UNKNOWN_MEASURE.getLegacyNum())) {
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

    private Vector<Vector<String>> getStatsData(final Vector<VOIStatisticalProperties> statsList, final VOIVector VOIs, final ModelImage img,
            final Vector<byte[]> gCurvature, final Vector<Double> gMeanNegativeCurvature) {
        // minCurvature maxCurvature
        // HGADFN167_LAC_40X_1 -0.3075 0.2463
        // HGADFN167_LAC_40X_2 -0.08464 0.1443
        // HGADFN167_LAC_40X_3 -0.4589 0.4927
        // HGADFN167_LAC_40X_4 -1.6375 1.774
        // HGADFN167_LAC_40X_5 -0.1258 0.1614
        // HGADFN167_LAC_40X_6 -1.196 0.3777
        // HGADFN167_LAC_40X_7 -0.1428 0.1725
        // HGADFN167_LAC_40X_8 -0.5064 5.987
        // HGFDFN168_LAC_40X_1 -0.0923 0.2574
        // HGFDFN168_LAC_40X_2 -0.2630 0.1806
        // HGFDFN168_LAC_40X_3 -0.1153 0.1691
        // HGFDFN168_LAC_40X_4 -0.09794 0.1551
        // HGFDFN168_LAC_40X_5 -0.1876 0.2318
        // HGFDFN168_LAC_40X_6 -0.2035 0.2853
        // HGFDFN168_LAC_40X_7 -0.09728 0.2410
        // HGFDFN168_LAC_40X_8 -0.5568 0.3128

        final Vector<Vector<String>> data = new Vector<Vector<String>>();
        int voiIndex = 0;
        for (final VOIStatisticalProperties prop : statsList) {
            final VOIBaseVector contours = VOIs.get(voiIndex).getCurves();
            for (final VOIBase voi : contours) {
                // The first curvature displayed will be farthest from the geometric center of the nucleus
                final Vector3f gCenter = voi.getGeometricCenter();
                final Vector<Vector3f> positions = new Vector<Vector3f>();
                final Vector<Float> curvature = new Vector<Float>();
                final double meanCurvature[] = new double[1];
                final double stdDevCurvature[] = new double[1];
                final double meanNegativeCurvature[] = new double[1];
                final int numberOfIndentations[] = new int[1];
                final double totalLength[] = new double[1];
                final boolean smoothCurvature = true;
                final double negativeHysteresisFraction = 0.25;
                final double positiveHysteresisFraction = 0.25;
                final int consecutiveNegativeNeeded = 2;
                final double negativeCurvatureNeeded = -2.5E-2;
                voi.findPositionAndCurvature(positions, curvature, smoothCurvature, meanCurvature, stdDevCurvature, meanNegativeCurvature,
                        negativeHysteresisFraction, positiveHysteresisFraction, numberOfIndentations, consecutiveNegativeNeeded, negativeCurvatureNeeded,
                        totalLength);
                double maxDistSquared = -Double.MAX_VALUE;
                final int nPoints = positions.size();
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
                    final double distX = positions.get(i).X - gCenter.X;
                    final double distY = positions.get(i).Y - gCenter.Y;
                    final double distSquared = distX * distX + distY * distY;
                    if (distSquared > maxDistSquared) {
                        maxDistSquared = distSquared;
                        startingIndex = i;
                    }
                }
                final double initDistance[] = new double[nPoints];
                final float initCurvature[] = new float[nPoints];
                int index = 0;
                final float startingDistance = positions.get(startingIndex).Z;
                for (int i = startingIndex; i < nPoints; i++) {
                    initDistance[index] = 800.0 * ( (positions.get(i).Z - startingDistance) / totalLength[0]);
                    initCurvature[index++] = curvature.get(i);
                }
                final double previousDistance = 800.0 * (positions.get(nPoints - 1).Z - startingDistance) / totalLength[0];
                for (int i = 0; i < startingIndex; i++) {
                    initDistance[index] = previousDistance + (800.0 * (positions.get(i).Z / totalLength[0]));
                    initCurvature[index++] = curvature.get(i);
                }
                final double interpCurvature[] = new double[801];
                double lowerDistance;
                double upperDistance;
                // initDistance[0] = 0.0
                interpCurvature[0] = initCurvature[0];
                interpCurvature[800] = initCurvature[0];
                index = 1;
                for (int i = 1; i <= 799; i++) {
                    while ( (i > initDistance[index]) && (index < nPoints - 1)) {
                        index++;
                    }
                    if (i >= initDistance[nPoints - 1]) {
                        lowerDistance = i - initDistance[nPoints - 1];
                        upperDistance = 800.0 - i;
                        interpCurvature[i] = (upperDistance * initCurvature[nPoints - 1] + lowerDistance * initCurvature[0]) / (lowerDistance + upperDistance);
                    } else {
                        lowerDistance = i - initDistance[index - 1];
                        upperDistance = initDistance[index] - i;
                        interpCurvature[i] = (upperDistance * initCurvature[index - 1] + lowerDistance * initCurvature[index])
                                / (lowerDistance + upperDistance);
                    }
                } // for (int i = 1; i <= 799; i++)
                final byte scaledCurvature[] = new byte[801];
                final double minCurv = -0.5;
                final double maxCurv = 0.5;
                double sCurv;
                for (int i = 0; i <= 800; i++) {
                    sCurv = 255.0 * (interpCurvature[i] - minCurv) / (maxCurv - minCurv);
                    scaledCurvature[i] = (byte) Math.round(Math.min(255.0, Math.max(0.0, sCurv)));
                }
                gCurvature.add(scaledCurvature);
                gMeanNegativeCurvature.add(meanNegativeCurvature[0]);

                final Vector<String> row = new Vector<String>();
                final String contourLabel = voi.getLabel();
                row.add(img.getImageFileName().replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
                row.add(VOIs.get(voiIndex).getName().replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
                // row.add(contourLabel.replaceAll("[\\t+]", ", ").replaceAll("[\\n\\r+]", ":"));
                for (int i = 0; i < checkList.length; i++) {
                    if (checkList[i]) {
                        row.add(prop.getProperty(VOIStatisticList.statisticDescription[i] + contourLabel).replaceAll("[\\t+]", ", ")
                                .replaceAll("[\\n\\r+]", ":"));
                    }
                }
                data.add(row);
            }
            voiIndex++;
        }

        return data;

        // end = contours[slice].elementAt(num).getLabel()
        // VOIBaseVector contours = ((VOI) list.getElementAt(i)).getCurves();
        // updateDialogRow((VOI) list.getElementAt(i), new Vector[]{contours}, properties, list, i, rowData, totalData);
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
        @Override
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
