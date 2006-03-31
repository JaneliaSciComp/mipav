package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   an Interfile image is stored on disk.
*
*/

public class FileInfoInterfile extends FileInfoBase {

    private String imagingModality = null;
    private String originatingSystem = null;
    private String keysVersion = null;
    private String keysDate = null;
    private String conversionProgram = null;
    private String programAuthor = null;
    private String programVersion = null;
    private String programDate = null;
    private boolean generalData = false;
    private String originalInstitution = null;
    private String contactPerson = null;
    private String dataDescription = null;
    private String patientName = null;
    private String patientID = null;
    private String patientDOB = null;
    private String patientSex = null;
    private String studyID = null;
    private String examType = null;
    private String dataCompression = null;
    private String dataEncode = null;
    private boolean generalImageData = false;
    private String interfileDataType = null;
    private String totalImageNumber = null;
    private boolean haveTomographic = false;
    private boolean haveStaticStudy = false;
    private boolean haveGated = false;
    private String studyDate = null;
    private String studyTime = null;
    private String isotopeNumber = null;
    private int    isoNumber = 1;
    private int    iNumber;
    private String[] isotopeName = null;
    private String[] betaHalflife = null;
    private String[] gammaHalflife = null;
    private String[] branchingFactor = null;
    private String radiopharmaceutical = null;
    private String energyWindowsNumber = null;
    private int    energyNumber = 0;
    private String[] energyWindowName = null;
    private String[] lowerLevel = null;
    private String[] upperLevel = null;
    private int      i;
    private int      wNumber;
    private String floodCorrected = null;
    private String decayCorrected = null;
    private boolean PETStudyGeneral = false;
    private String scannerQuantificationFactor = null;
    private String quantificationUnits = null;
    private String PETDataType = null;
    private String startHorizontalBedPosition = null;
    private String timeFrames = null;
    private int    timeFrameNumber = 0;
    private int    tFrame;
    private boolean dynamicStudyGeneral = false;
    private boolean spectStudyGeneral = false;
    private boolean staticStudyGeneral = false;
    private boolean gatedStudyGeneral = false;
    private String detectorHeadNumber = null;
    private String imagesPerEWindow = null;
    private int    numberImagesPerEWindow;
    private int    numberImages; // numberImagesPerEWindow * energyNumber
    private String[] matrixSize1 = null;
    private int      matrixSize1Index = 0;
    private String[] matrixSize2 = null;
    private int      matrixSize2Index = 0;
    private String[] numberFormat = null;
    private int      numberFormatIndex = 0;
    private String[] bytesPerPixel = null;
    private int      bytesPerPixelIndex = 0;
    private String[] scalingFactor1 = null;
    private int      scalingFactor1Index = 0;
    private String[] scalingFactor2 = null;
    private int      scalingFactor2Index = 0;
    private String[] imageStartTime = null;
    private int      imageStartTimeIndex = 0;
    private String[] label = null;
    private int      labelIndex = 0;
    private String[] maximumPixelCount = null;
    private int      maximumPixelCountIndex = 0;
    private String[] totalCounts = null;
    private int      totalCountsIndex = 0;
    private String processStatus = null;
    private String dimensionNumber = null;
    private boolean haveReconstructed = false;
    private String projectionNumber = null;
    private String rotationExtent = null;
    private String projectionTime = null;
    private String studyDuration = null;
    private String patientOrientation = null;
    private String patientRotation = null;
    private boolean PETStudyImageData = false;
    private boolean spectStudyReconstructedData = false;
    private String reconstructionMethod = null;
    private String sliceNumber = null;
    private String referenceFrameNumber = null;
    private String sliceThickness = null;
    private String centerCenter = null;
    private String filterName = null;
    private String zAxisFilter = null;
    private String appliedCorrections = null;
    private String attenuationCorrection = null;
    private String scatterCorrected = null;
    private String scatterCorrectionMethod = null;
    private String obliqueReconstruction = null;
    private boolean imageDataDescription = false;
    private String indexNestingLevel = null;
    private String[] imageDuration = null;
    private int    imageDurationIndex = 0;
    private String[] imageRelativeStartTime = null;
    private int    imageRelativeStartTimeIndex = 0;
    private boolean endianess;
    private String frameGroupNumber = null;
    private int    fGroupNumber = 1;
    private String[] frameGroupImages = null;
    private int frameGroupImagesIndex = 0;
    private String[] pauseBetweenImages = null;
    private int pauseBetweenImagesIndex = 0;
    private String[] pauseBetweenFrameGroups = null;
    private int pauseBetweenFrameGroupsIndex = 0;
    private String[] maximumPixelCountInGroup = null;
    private int maximumPixelCountInGroupIndex = 0;
    private String elapsedStudyDuration = null;
    private String observedCardiacCycles = null;
    private String timeWindows = null;
    private int timeWindowsNumber = 1;
    private String[] timeWindowImages = null;
    private int timeWindowImagesIndex = 0;
    private String[] framingMethod = null;
    private int framingMethodIndex = 0;
    private String[] timeWindowLowerLimit = null;
    private int timeWindowLowerLimitIndex = 0;
    private String[] timeWindowUpperLimit = null;
    private int timeWindowUpperLimitIndex = 0;
    private String[] RRCycles = null;
    private int RRCyclesIndex = 0;
    private String[] acquiredCardiacCycles = null;
    private int acquiredCardiacCyclesIndex = 0;
    private String[] acquiredStudyDuration = null;
    private int acquiredStudyDurationIndex = 0;
    private String[] RRHistogram = null;
    private int RRHistogramIndex = 0;

    /**
    *  FileInfoInterfile     - file info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoInterfile(String name, String directory, int format) {
        super(name, directory, format);
    }

    public String getImagingModality() {
        return imagingModality;
    }

    public void setImagingModality(String imagingModality) {
        this.imagingModality = imagingModality;
    }

    public String getOriginatingSystem() {
        return originatingSystem;
    }

    public void setOriginatingSystem(String originatingSystem) {
        this.originatingSystem = originatingSystem;
    }

    public String getKeysVersion() {
        return keysVersion;
    }

    public void setKeysVersion(String keysVersion) {
        this.keysVersion = keysVersion;
    }

    public String getKeysDate() {
        return keysDate;
    }

    public void setKeysDate(String keysDate) {
        this.keysDate = keysDate;
    }

    public String getConversionProgram() {
        return conversionProgram;
    }

    public void setConversionProgram(String conversionProgram) {
        this.conversionProgram = conversionProgram;
    }

    public String getProgramAuthor() {
        return programAuthor;
    }

    public void setProgramAuthor(String programAuthor) {
        this.programAuthor = programAuthor;
    }

    public String getProgramVersion() {
        return programVersion;
    }

    public void setProgramVersion(String programVersion) {
        this.programVersion = programVersion;
    }

    public String getProgramDate() {
        return programDate;
    }

    public void setProgramDate(String programDate) {
        this.programDate = programDate;
    }

    public boolean getGeneralData() {
        return generalData;
    }

    public void setGeneralData(boolean generalData) {
        this.generalData = generalData;
    }

    public String getOriginalInstitution() {
        return originalInstitution;
    }

    public void setOriginalInstitution(String originalInstitution) {
        this.originalInstitution = originalInstitution;
    }

    public String getContactPerson() {
        return contactPerson;
    }

    public void setContactPerson(String contactPerson) {
        this.contactPerson = contactPerson;
    }

    public String getDataDescription() {
        return dataDescription;
    }

    public void setDataDescription(String dataDescription) {
        this.dataDescription = dataDescription;
    }

    public String getPatientName() {
        return patientName;
    }

    public void setPatientName(String patientName) {
        this.patientName = patientName;
    }

    public String getPatientID() {
        return patientID;
    }

    public void setPatientID(String patientID) {
        this.patientID = patientID;
    }

    public String getPatientDOB() {
        return patientDOB;
    }

    public void setPatientDOB(String patientDOB) {
        this.patientDOB = patientDOB;
    }

    public String getPatientSex() {
        return patientSex;
    }

    public void setPatientSex(String patientSex) {
        this.patientSex = patientSex;
    }

    public String getStudyID() {
        return studyID;
    }

    public void setStudyID(String studyID) {
        this.studyID = studyID;
    }

    public String getExamType() {
        return examType;
    }

    public void setExamType(String examType) {
        this.examType = examType;
    }

    public String getDataCompression() {
        return dataCompression;
    }

    public void setDataCompression(String dataCompression) {
        this.dataCompression = dataCompression;
    }

    public String getDataEncode() {
        return dataEncode;
    }

    public void setDataEncode(String dataEncode) {
        this.dataEncode = dataEncode;
    }

    public boolean generalImageData() {
        return generalImageData;
    }

    public void setGeneralImageData(boolean generalImageData) {
        this.generalImageData = generalImageData;
    }

    public String getInterfileDataType() {
        return interfileDataType;
    }

    public void setInterfileDataType(String interfileDataType) {
        this.interfileDataType = interfileDataType;
        if (interfileDataType.equalsIgnoreCase("TOMOGRAPHIC")) {
            haveTomographic = true;
        }
        if ((interfileDataType.equalsIgnoreCase("STATIC")) ||
            (interfileDataType.equalsIgnoreCase("ROI"))) {
            haveStaticStudy = true;
        }
        if (interfileDataType.equalsIgnoreCase("GATED")) {
            haveGated = true;
        }
    }

    public String getTotalImageNumber() {
        return totalImageNumber;
    }

    public void setTotalImageNumber(String totalImageNumber) {
        this.totalImageNumber = totalImageNumber;
    }

    public String getStudyDate() {
        return studyDate;
    }

    public void setStudyDate(String studyDate) {
        this.studyDate = studyDate;
    }

    public String getStudyTime() {
        return studyTime;
    }

    public void setStudyTime(String studyTime) {
        this.studyTime = studyTime;
    }

    public String getIsotopeNumber() {
        return isotopeNumber;
    }

    public void setIsotopeNumber(String isotopeNumber) {
        this.isotopeNumber = isotopeNumber;
        isoNumber = Integer.valueOf(isotopeNumber).intValue();
        isotopeName = new String[iNumber];
        betaHalflife = new String[iNumber];
        gammaHalflife = new String[iNumber];
        branchingFactor = new String[iNumber];

        for (i = 0; i < isoNumber; i++) {
            isotopeName[i] = null;
            betaHalflife[i] = null;
            gammaHalflife[i] = null;
            branchingFactor[i] = null;
        }
    }

    public String[] getIsotopeName() {
        return isotopeName;
    }

    public void setIsotopeName(String currentIsotopeNumber, String currentIsotopeName) {
        iNumber = Integer.valueOf(currentIsotopeNumber).intValue();
        if (isotopeName == null) {
            isotopeName = new String[1];
        }
        isotopeName[iNumber-1] = currentIsotopeName;
    }

    public String[] getBetaHalflife() {
        return betaHalflife;
    }

    public void setBetaHalflife(String currentIsotopeNumber, String currentBetaHalflife) {
        iNumber = Integer.valueOf(currentIsotopeNumber).intValue();
        if (betaHalflife == null) {
            betaHalflife = new String[1];
        }
        betaHalflife[iNumber-1] = currentBetaHalflife;
    }

    public String[] getGammaHalflife() {
        return gammaHalflife;
    }

    public void setGammaHalflife(String currentIsotopeNumber, String currentGammaHalflife) {
        iNumber = Integer.valueOf(currentIsotopeNumber).intValue();
        if (gammaHalflife == null) {
            gammaHalflife = new String[1];
        }
        gammaHalflife[iNumber-1] = currentGammaHalflife;
    }

    public String[] getBranchingFactor() {
        return branchingFactor;
    }


    public void setBranchingFactor(String currentIsotopeNumber, String currentBranchingFactor) {
        iNumber = Integer.valueOf(currentIsotopeNumber).intValue();
        if (branchingFactor == null) {
            branchingFactor = new String[1];
        }
        branchingFactor[iNumber-1] = currentBranchingFactor;
    }

    public String getRadiopharmaceutical() {
        return radiopharmaceutical;
    }

    public void setRadiopharmaceutical(String radiopharmaceutical) {
        this.radiopharmaceutical = radiopharmaceutical;
    }

    public String getEnergyWindowsNumber() {
        return energyWindowsNumber;
    }

    public void setEnergyWindowsNumber(String energyWindowsNumber) {
        this.energyWindowsNumber = energyWindowsNumber;
        energyNumber = Integer.valueOf(energyWindowsNumber).intValue();
        energyWindowName = new String[energyNumber];
        lowerLevel = new String[energyNumber];
        upperLevel = new String[energyNumber];

        for (i = 0; i < energyNumber; i++) {
            energyWindowName[i] = null;
            lowerLevel[i] = null;
            upperLevel[i] = null;
        }
    }

    public String[] getEnergyWindow() {
        return energyWindowName;
    }

    public void setEnergyWindow(String currentWindowNumber, String currentEnergyWindowName) {
        wNumber = Integer.valueOf(currentWindowNumber).intValue();
        energyWindowName[wNumber-1] = currentEnergyWindowName;
    }

    public String[] getEnergyWindowLowerLevel() {
        return lowerLevel;
    }

    public void setEnergyWindowLowerLevel(String currentWindowNumber,
                                          String currentLowerLevel) {
        wNumber = Integer.valueOf(currentWindowNumber).intValue();
        lowerLevel[wNumber-1] = currentLowerLevel;
    }

    public String[] getEnergyWindowUpperLevel() {
        return upperLevel;
    }

    public void setEnergyWindowUpperLevel(String currentWindowNumber,
                                          String currentUpperLevel) {
        wNumber = Integer.valueOf(currentWindowNumber).intValue();
        upperLevel[wNumber-1] = currentUpperLevel;
    }

    public String getFloodCorrected() {
        return floodCorrected;
    }

    public void setFloodCorrected(String floodCorrected) {
        this.floodCorrected = floodCorrected;
    }

    public String getDecayCorrected() {
        return decayCorrected;
    }

    public void setDecayCorrected(String decayCorrected) {
        this.decayCorrected = decayCorrected;
    }

    public boolean getPETStudyGeneral() {
        return PETStudyGeneral;
    }

    public void setPETStudyGeneral(boolean PETStudyGeneral) {
        this.PETStudyGeneral = PETStudyGeneral;
    }

    public String getScannerQuantificationFactor() {
        return scannerQuantificationFactor;
    }

    public void setScannerQuantificationFactor(String scannerQuantificationFactor) {
        this.scannerQuantificationFactor = scannerQuantificationFactor;
    }

    public String getQuantificationUnits() {
        return quantificationUnits;
    }

    public void setQuantificationUnits(String quantificationUnits) {
        this.quantificationUnits = quantificationUnits;
    }

    public String getPETDataType() {
        return PETDataType;
    }

    public void setPETDataType(String PETDataType) {
        this.PETDataType = PETDataType;
    }

    public boolean getDynamicStudyGeneral() {
        return dynamicStudyGeneral;
    }

    public void setDynamicStudyGeneral(boolean dynamicStudyGeneral) {
        this.dynamicStudyGeneral = dynamicStudyGeneral;
    }

    public boolean getSpectStudyGeneral() {
        return spectStudyGeneral;
    }

    public void setSpectStudyGeneral(boolean spectStudyGeneral) {
        this.spectStudyGeneral = spectStudyGeneral;
    }

    public boolean getStaticStudyGeneral() {
        return staticStudyGeneral;
    }

    public void setStaticStudyGeneral(boolean staticStudyGeneral) {
        this.staticStudyGeneral = staticStudyGeneral;
    }

    public boolean getGatedStudyGeneral() {
        return gatedStudyGeneral;
    }

    public void setGatedStudyGeneral(boolean gatedStudyGeneral) {
        this.gatedStudyGeneral = gatedStudyGeneral;
    }

    public String getDetectorHeadNumber() {
        return detectorHeadNumber;
    }

    public void setDetectorHeadNumber(String detectorHeadNumber) {
        this.detectorHeadNumber = detectorHeadNumber;
    }

    public String getImagesPerEWindow() {
        return imagesPerEWindow;
    }

    public void setImagesPerEWindow(String imagesPerEWindow) {
        this.imagesPerEWindow = imagesPerEWindow;
        numberImagesPerEWindow = Integer.valueOf(imagesPerEWindow).intValue();
        numberImages = numberImagesPerEWindow * energyNumber;
        if (haveStaticStudy) {
            matrixSize1 = new String[numberImagesPerEWindow];
            matrixSize2 = new String[numberImagesPerEWindow];
            numberFormat = new String[numberImagesPerEWindow];
            bytesPerPixel = new String[numberImagesPerEWindow];
            scalingFactor1 = new String[numberImagesPerEWindow];
            scalingFactor2 = new String[numberImagesPerEWindow];
            imageDuration = new String[numberImagesPerEWindow];
            imageStartTime = new String[numberImagesPerEWindow];
            label = new String[numberImagesPerEWindow];
            maximumPixelCount = new String[numberImagesPerEWindow];
            totalCounts = new String[numberImagesPerEWindow];
            for (i = 0; i < numberImagesPerEWindow; i++) {
                matrixSize1[i] = null;
                matrixSize2[i] = null;
                numberFormat[i] = null;
                bytesPerPixel[i] = null;
                scalingFactor1[i] = null;
                scalingFactor2[i] = null;
                imageDuration[i] = null;
                imageStartTime[i] = null;
                label[i] = null;
                maximumPixelCount[i] = null;
                totalCounts[i] = null;
            }
        }
    }

    public String getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(String processStatus) {
        this.processStatus = processStatus;
        if (processStatus.equalsIgnoreCase("RECONSTRUCTED")) {
            haveReconstructed = true;
        }
    }

    public String getDimensionNumber() {
        return dimensionNumber;
    }

    public void setDimensionNumber(String dimensionNumber) {
        this.dimensionNumber = dimensionNumber;
    }

    public String getStartHorizontalBedPosition() {
        return startHorizontalBedPosition;
    }

    public void setStartHorizontalBedPosition(String startHorizontalBedPosition) {
        this.startHorizontalBedPosition = startHorizontalBedPosition;
    }

    public String getTimeFrames() {
        return timeFrames;
    }

    public void setTimeFrames(String timeFrames) {
        this.timeFrames = timeFrames;
        timeFrameNumber = Integer.valueOf(timeFrames).intValue();
        imageDuration = new String[timeFrameNumber];
        imageRelativeStartTime = new String[timeFrameNumber];
        for (i = 0; i < timeFrameNumber; i++) {
            imageDuration[i] = null;
            imageRelativeStartTime[i] = null;
        }
    }

    public String getProjectionNumber() {
        return projectionNumber;
    }

    public void setProjectionNumber(String projectionNumber) {
        this.projectionNumber = projectionNumber;
    }

    public String getRotationExtent() {
        return rotationExtent;
    }

    public void setRotationExtent(String rotationExtent) {
        this.rotationExtent = rotationExtent;
    }

    public String getProjectionTime() {
        return projectionTime;
    }

    public void setProjectionTime(String projectionTime) {
        this.projectionTime = projectionTime;
    }

    public String getStudyDuration() {
        return studyDuration;
    }

    public void setStudyDuration(String studyDuration) {
        this.studyDuration = studyDuration;
    }

    public String[] getMaximumPixelCount() {
        return maximumPixelCount;
    }

    public void setMaximumPixelCount(String maximumPixelCt) {
        if (maximumPixelCount == null) {
            maximumPixelCount = new String[1];
        }
        maximumPixelCount[maximumPixelCountIndex++] = maximumPixelCt;
    }

    public String[] getMatrixSize1() {
        return matrixSize1;
    }

    public void setMatrixSize1(String matrixSz1) {
        if (matrixSize1 == null) {
            matrixSize1 = new String[1];
        }
        matrixSize1[matrixSize1Index++] = matrixSz1;
    }

    public String[] getMatrixSize2() {
        return matrixSize2;
    }
    public void setMatrixSize2(String matrixSz2) {
        if (matrixSize2 == null) {
            matrixSize2 = new String[1];
        }
        matrixSize2[matrixSize2Index++] = matrixSz2;
    }

    public String[] getNumberFormat() {
        return numberFormat;
    }

    public void setNumberFormat(String nFormat) {
        if (numberFormat == null) {
            numberFormat = new String[1];
        }
        numberFormat[numberFormatIndex++] = nFormat;
    }

    public String[] getBytesPerPixel() {
        return bytesPerPixel;
    }

    public void setBytesPerPixel(String bpp) {
        if (bytesPerPixel == null) {
            bytesPerPixel = new String[1];
        }
        bytesPerPixel[bytesPerPixelIndex++] = bpp;
    }

    public String[] getScalingFactor1() {
        return scalingFactor1;
    }

    public void setScalingFactor1(String s1) {
        if (scalingFactor1 == null) {
            scalingFactor1 = new String[1];
        }
        scalingFactor1[scalingFactor1Index++] = s1;
    }

    public String[] getScalingFactor2() {
        return scalingFactor2;
    }

    public void setScalingFactor2(String s2) {
        if (scalingFactor2 == null) {
            scalingFactor2 = new String[1];
        }
        scalingFactor2[scalingFactor2Index++] = s2;
    }

    public String[] getImageStartTime() {
        return imageStartTime;
    }

    public void setImageStartTime(String sTime) {
        if (imageStartTime == null) {
            imageStartTime = new String[1];
        }
        imageStartTime[imageStartTimeIndex++] = sTime;
    }

    public String[] getLabel() {
        return label;
    }

    public void setLabel(String lab) {
        if (label == null) {
            label = new String[1];
        }
        label[labelIndex++] = lab;
    }

    public String[] getTotalCounts() {
        return totalCounts;
    }

    public void setTotalCounts(String tCounts) {
        if (totalCounts == null) {
            totalCounts = new String[1];
        }
        totalCounts[totalCountsIndex++] = tCounts;
    }

    public String getPatientOrientation() {
        return patientOrientation;
    }

    public void setPatientOrientation(String patientOrientation) {
        this.patientOrientation = patientOrientation;
    }

    public String getPatientRotation() {
        return patientRotation;
    }

    public void setPatientRotation(String patientRotation) {
        this.patientRotation = patientRotation;
    }

    public boolean getPETStudyImageData() {
        return PETStudyImageData;
    }

    public void setPETStudyImageData(boolean PETStudyImageData) {
        this.PETStudyImageData = PETStudyImageData;
    }

    public boolean getSpectStudyReconstructedData() {
        return spectStudyReconstructedData;
    }

    public void setSpectStudyReconstructedData(boolean spectStudyReconstructedData) {
        this.spectStudyReconstructedData = spectStudyReconstructedData;
    }

    public String getReconstructionMethod() {
        return reconstructionMethod;
    }

    public void setReconstructionMethod(String reconstructionMethod){
        this.reconstructionMethod = reconstructionMethod;
    }

    public String getSliceNumber() {
        return sliceNumber;
    }

    public void setSliceNumber(String sliceNumber) {
        this.sliceNumber = sliceNumber;
    }

    public String getReferenceFrameNumber() {
        return referenceFrameNumber;
    }

    public void setReferenceFrameNumber(String referenceFrameNumber) {
        this.referenceFrameNumber = referenceFrameNumber;
    }

    public String getSliceThickness() {
        return sliceThickness;
    }

    public void setSliceThickness(String sliceThickness) {
        this.sliceThickness = sliceThickness;
    }

    public String getCenterCenter() {
        return centerCenter;
    }

    public void setCenterCenter(String centerCenter) {
        this.centerCenter = centerCenter;
    }

    public String getFilterName() {
        return filterName;
    }

    public void setFilterName(String filterName) {
        this.filterName = filterName;
    }

    public String getZAxisFilter() {
        return zAxisFilter;
    }

    public void setZAxisFilter(String zAxisFilter) {
        this.zAxisFilter = zAxisFilter;
    }

    public String getAppliedCorrections() {
        return appliedCorrections;
    }

    public void setAppliedCorrections(String appliedCorrections) {
        this.appliedCorrections = appliedCorrections;
    }

    public String getAttenuationCorrection() {
        return attenuationCorrection;
    }

    public void setAttenuationCorrection(String attenuationCorrection) {
        this.attenuationCorrection = attenuationCorrection;
    }

    public String getScatterCorrected() {
        return scatterCorrected;
    }

    public void setScatterCorrected(String scatterCorrected) {
        this.scatterCorrected = scatterCorrected;
    }

    public String getScatterCorrectionMethod() {
        return scatterCorrectionMethod;
    }

    public void setScatterCorrectionMethod(String scatterCorrectionMethod) {
        this.scatterCorrectionMethod = scatterCorrectionMethod;
    }

    public String getObliqueReconstruction() {
        return obliqueReconstruction;
    }

    public void setObliqueReconstruction(String obliqueReconstruction) {
        this.obliqueReconstruction = obliqueReconstruction;
    }

    public boolean getImageDataDescription() {
        return imageDataDescription;
    }

    public void setImageDataDescription(boolean imageDataDescription) {
        this.imageDataDescription = imageDataDescription;
    }

    public String getIndexNestingLevel() {
        return indexNestingLevel;
    }

    public void setIndexNestingLevel(String indexNestingLevel) {
        this.indexNestingLevel = indexNestingLevel;
    }

    public String[] getImageDuration() {
        return imageDuration;
    }

    public void setImageDuration(String currentTimeFrame, String currentImageDuration) {
        tFrame = Integer.valueOf(currentTimeFrame).intValue();
        imageDuration[tFrame-1] = currentImageDuration;
    }

    public void setImageDuration(String currentImageDuration) {
        imageDuration[imageDurationIndex++] = currentImageDuration;
    }

    public String[] getImageRelativeStartTime() {
        return imageRelativeStartTime;
    }

    public void setImageRelativeStartTime(String currentTimeFrame,
                                          String currentImageRelativeStartTime) {
        tFrame = Integer.valueOf(currentTimeFrame).intValue();
        imageRelativeStartTime[tFrame-1] = currentImageRelativeStartTime;
    }

    public void setImageRelativeStartTime(String currentImageRelativeStartTime) {
        imageRelativeStartTime[imageRelativeStartTimeIndex++] = currentImageRelativeStartTime;
    }

    public String getFrameGroupNumber() {
        return frameGroupNumber;
    }

    public void setFrameGroupNumber(String frameGroupNumber) {
        this.frameGroupNumber = frameGroupNumber;
        fGroupNumber = Integer.valueOf(frameGroupNumber).intValue();
        matrixSize1 = new String[fGroupNumber];
        matrixSize2 = new String[fGroupNumber];
        numberFormat = new String[fGroupNumber];
        bytesPerPixel = new String[fGroupNumber];
        scalingFactor1 = new String[fGroupNumber];
        scalingFactor2 = new String[fGroupNumber];
        frameGroupImages = new String[fGroupNumber];
        imageDuration = new String[fGroupNumber];
        pauseBetweenImages = new String[fGroupNumber];
        pauseBetweenFrameGroups = new String[fGroupNumber];
        maximumPixelCountInGroup = new String[fGroupNumber];
        for (i = 0; i < fGroupNumber; i++) {
            matrixSize1[i] = null;
            matrixSize2[i] = null;
            numberFormat[i] = null;
            bytesPerPixel[i] = null;
            scalingFactor1[i] = null;
            scalingFactor2[i] = null;
            frameGroupImages[i] = null;
            imageDuration[i] = null;
            pauseBetweenImages[i] = null;
            pauseBetweenFrameGroups[i] = null;
            maximumPixelCountInGroup[i] = null;
        }
    }

    public String[] getFrameGroupImages() {
        return frameGroupImages;
    }

    public void setFrameGroupImages(String fGroupImages) {
        if (frameGroupImages == null) {
            frameGroupImages = new String[1];
        }
        frameGroupImages[frameGroupImagesIndex++] = fGroupImages;
    }

    public String[] getPauseBetweenImages() {
        return pauseBetweenImages;
    }

    public void setPauseBetweenImages(String pBetweenImages) {
        if (pauseBetweenImages == null) {
            pauseBetweenImages = new String[1];
        }
        pauseBetweenImages[pauseBetweenImagesIndex++] = pBetweenImages;
    }

    public String[] getPauseBetweenFrameGroups() {
        return pauseBetweenFrameGroups;
    }

    public void setPauseBetweenFrameGroups(String pBetweenFrameGroups) {
        if (pauseBetweenFrameGroups == null) {
            pauseBetweenFrameGroups = new String[1];
        }
        pauseBetweenFrameGroups[pauseBetweenFrameGroupsIndex++] = pBetweenFrameGroups;
    }

    public String[] getMaximumPixelCountInGroup() {
        return maximumPixelCountInGroup;
    }

    public void setMaximumPixelCountInGroup (String mPixelCountInGroup) {
        if (maximumPixelCountInGroup == null) {
            maximumPixelCountInGroup = new String[1];
        }
        maximumPixelCountInGroup[maximumPixelCountInGroupIndex++] =
        mPixelCountInGroup;
    }

    public String getElapsedStudyDuration() {
        return elapsedStudyDuration;
    }

    public void setElapsedStudyDuration(String elapsedStudyDuration) {
        this.elapsedStudyDuration = elapsedStudyDuration;
    }

    public String getObservedCardiacCycles() {
        return observedCardiacCycles;
    }

    public void setObservedCardiacCycles(String observedCardiacCycles) {
        this.observedCardiacCycles = observedCardiacCycles;
    }

    public String getTimeWindows() {
        return timeWindows;
    }

    public void setTimeWindows(String timeWindows) {
        this.timeWindows = timeWindows;
        timeWindowsNumber = Integer.valueOf(timeWindows).intValue();
        timeWindowImages = new String[timeWindowsNumber];
        framingMethod = new String[timeWindowsNumber];
        timeWindowLowerLimit = new String[timeWindowsNumber];
        timeWindowUpperLimit = new String[timeWindowsNumber];
        RRCycles = new String[timeWindowsNumber];
        acquiredCardiacCycles = new String[timeWindowsNumber];
        acquiredStudyDuration = new String[timeWindowsNumber];
        RRHistogram = new String[timeWindowsNumber];
        for (i = 0; i < timeWindowsNumber; i++) {
            timeWindowImages[i] = null;
            framingMethod[i] = null;
            timeWindowLowerLimit[i] = null;
            timeWindowUpperLimit[i] = null;
            RRCycles[i] = null;
            acquiredCardiacCycles[i] = null;
            acquiredStudyDuration[i] = null;
            RRHistogram[i] = null;
        }
        if (haveGated) {
            imageDuration = new String[timeWindowsNumber];
            for (i = 0; i < timeWindowsNumber; i++) {
                imageDuration[i] = null;
            }
        } // if (haveGated)
    }

    public String[] getTimeWindowImages() {
        return timeWindowImages;
    }

    public void setTimeWindowImages(String tWindowImages) {
        if (timeWindowImages == null) {
            timeWindowImages = new String[1];
        }
        timeWindowImages[timeWindowImagesIndex++] = tWindowImages;
    }

    public String[] getFramingMethod() {
        return framingMethod;
    }

    public void setFramingMethod(String fMethod) {
        if (framingMethod == null) {
            framingMethod = new String[1];
        }
        framingMethod[framingMethodIndex++] = fMethod;
    }

    public String[] getTimeWindowLowerLimit() {
        return timeWindowLowerLimit;
    }

    public void setTimeWindowLowerLimit(String lLimit) {
        if (timeWindowLowerLimit == null) {
            timeWindowLowerLimit = new String[1];
        }
        timeWindowLowerLimit[timeWindowLowerLimitIndex++] = lLimit;
    }

    public String[] getTimeWindowUpperLimit() {
        return timeWindowUpperLimit;
    }

    public void setTimeWindowUpperLimit(String uLimit) {
        if (timeWindowUpperLimit == null) {
            timeWindowUpperLimit = new String[1];
        }
        timeWindowUpperLimit[timeWindowUpperLimitIndex++] = uLimit;
    }

    public String[] getRRCycles() {
        return RRCycles;
    }

    public void setRRCycles(String rC) {
        if (RRCycles == null) {
            RRCycles = new String[1];
        }
        RRCycles[RRCyclesIndex++] = rC;
    }

    public String[] getAcquiredCardiacCycles() {
        return acquiredCardiacCycles;
    }

    public void setAcquiredCardiacCycles(String aCardiacCycles) {
        if (acquiredCardiacCycles == null) {
            acquiredCardiacCycles = new String[1];
        }
        acquiredCardiacCycles[acquiredCardiacCyclesIndex++] = aCardiacCycles;
    }

    public String[] getAcquiredStudyDuration() {
        return acquiredStudyDuration;
    }

    public void setAcquiredStudyDuration(String aStudyDuration) {
        if (acquiredStudyDuration == null) {
            acquiredStudyDuration = new String[1];
        }
        acquiredStudyDuration[acquiredStudyDurationIndex++] = aStudyDuration;
    }

    public String[] getRRHistogram() {
        return RRHistogram;
    }

    public void setRRHistogram(String RRH) {
        if (RRHistogram == null) {
            RRHistogram = new String[1];
        }
        RRHistogram[RRHistogramIndex++] = RRH;
    }


    /**
    *  Displays the file information
    *  @param dlog    dialog box that is written to
    *  @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        if (imagingModality != null) {
            dialog.append("Imaging modality = " + imagingModality + "\n");
        }

        if (originatingSystem != null) {
            dialog.append("Originating system = " + originatingSystem + "\n");
        }

        if (keysVersion != null) {
            dialog.append("Version of keys = " + keysVersion + "\n");
        }

        if (keysDate != null) {
            dialog.append("Date of keys = " + keysDate + "\n");
        }

        if (conversionProgram != null) {
            dialog.append("Conversion program = " + conversionProgram + "\n");
        }

        if (programAuthor != null) {
            dialog.append("Program author = " + programAuthor + "\n");
        }

        if (programVersion != null) {
            dialog.append("Program version = " + programVersion + "\n");
        }

        if (programDate != null) {
            dialog.append("Program date = " + programDate + "\n");
        }

        if (generalData) {
            dialog.append("GENERAL DATA\n");
        }

        if (originalInstitution != null) {
            dialog.append("Original institution = " + originalInstitution + "\n");
        }

        if (contactPerson != null) {
            dialog.append("Contact person = " + contactPerson + "\n");
        }

        if (dataDescription != null) {
            dialog.append("Data description = " + dataDescription + "\n");
        }

        if (patientName != null) {
            dialog.append("Patient name = " + patientName + "\n");
        }

        if (patientID != null) {
            dialog.append("Patient ID = " + patientID + "\n");
        }

        if (patientDOB != null) {
            dialog.append("Patient DOB = " + patientDOB + "\n");
        }

        if (patientSex != null) {
            dialog.append("Patient sex = " + patientSex + "\n");
        }

        if (studyID != null) {
            dialog.append("Study ID = " + studyID + "\n");
        }

        if (examType != null) {
            dialog.append("Exam Type = " + examType + "\n");
        }

        if (dataCompression != null) {
            dialog.append("Data compression = " + dataCompression + "\n");
        }

        if (dataEncode != null) {
            dialog.append("Data encode = " + dataEncode + "\n");
        }

        if (generalImageData) {
            dialog.append("GENERAL IMAGE DATA\n");
        }

        if (interfileDataType != null) {
            dialog.append("Type of data = " + interfileDataType + "\n");
        }

        if (totalImageNumber != null) {
            dialog.append("Total number of images = " + totalImageNumber + "\n");
        }

        if (studyDate != null) {
            dialog.append("Study date = " + studyDate + "\n");
        }

        if (studyTime != null) {
            dialog.append("Study time = " + studyTime + "\n");
        }

        if (isotopeNumber != null) {
            dialog.append("Number of isotopes = " + isotopeNumber + "\n");
        }



        if (energyWindowsNumber != null) {
            dialog.append("Number of energy windows = " + energyWindowsNumber + "\n");
        }

        for (i = 0; i < isoNumber; i++) {
            if (isoNumber == 1) {
                if (isotopeName != null) {
                    dialog.append("Isotope name = " + isotopeName[0] + "\n");
                }
                if (betaHalflife != null) {
                    dialog.append("Isotope beta halflife (sec) = " + betaHalflife[0] + "\n");
                }
                if (gammaHalflife != null) {
                    dialog.append("Isotope gamma halflife (sec) = " + gammaHalflife[0] + "\n");
                }
                if (branchingFactor != null) {
                    dialog.append("Isotope branching factor = " + branchingFactor[0] + "\n");
                }
            } // if (iNumber == 1)
            else {
                if (isotopeName[i] != null) {
                    dialog.append("Isotope name [" + (i+1) + "] = " + isotopeName[i] + "\n");
                }
                if (betaHalflife[i] != null) {
                    dialog.append("Isotope beta halflife (sec) [" + (i+1) + "] = " + betaHalflife[i] + "\n");
                }
                if (gammaHalflife[i] != null) {
                    dialog.append("Isotope gamma halflife (sec) [" + (i+1) + "] = " + gammaHalflife[i] + "\n");
                }
                if (branchingFactor[i] != null) {
                    dialog.append("Isotope branching factor [" + (i+1) + "] = " +
                                   branchingFactor[i] + "\n");
                }
            }
        } // for (i = 0; i < iNumber; i++)

        if (radiopharmaceutical != null) {
            dialog.append("Radiopharmaceutical = " + radiopharmaceutical + "\n");
        }

        for (i = 0; i < energyNumber; i++) {
            if (energyWindowName[i] != null) {
                dialog.append("Energy window[" + (i+1) + "] = " +
                               energyWindowName[i] + "\n");
            }
            if (lowerLevel[i] != null) {
                dialog.append("Energy window lower level[" + (i+1) + "] = " +
                               lowerLevel[i] + "\n");
            }
            if (upperLevel[i] != null) {
                dialog.append("Energy window upper level[" + (i+1) + "] = " +
                               upperLevel[i] + "\n");
            }
        } // for (i = 0; i < energyNumber; i++)

        if (floodCorrected != null) {
            dialog.append("floodCorrected = " + floodCorrected + "\n");
        }

        if (decayCorrected != null) {
            dialog.append("decayCorrected = " + decayCorrected + "\n");
        }

        if (PETStudyGeneral) {
            dialog.append("PET STUDY (General)\n");
        }

        if (scannerQuantificationFactor != null) {
            dialog.append("Scanner quantification factor = " + scannerQuantificationFactor + "\n");
        }

        if (quantificationUnits != null) {
            dialog.append("Quantification units = " + quantificationUnits + "\n");
        }

        if (PETDataType != null) {
            dialog.append("PET data type = " + PETDataType + "\n");
        }

        if (spectStudyGeneral) {
            dialog.append("SPECT STUDY (General)\n");
        }

        if (staticStudyGeneral) {
            dialog.append("STATIC STUDY (General) \n");
        }

        if (gatedStudyGeneral) {
            dialog.append("GATED STUDY (General) \n");
            if (elapsedStudyDuration != null) {
                dialog.append("Study duration (elapsed) sec = " +
                               elapsedStudyDuration + "\n");
            }
            if (observedCardiacCycles != null) {
                dialog.append("Number of cardiac cycles (observed) := " +
                               observedCardiacCycles + "\n");
            }
            dialog.append("number of time windows := " + timeWindowsNumber + "\n");
            for (i = 0; i < timeWindowsNumber; i++) {
                dialog.append("Gated Study (each time window) := \n");
                dialog.append("Time window number := " + (i+1) + "\n");
                if (timeWindowImages != null) {
                    if (timeWindowImages[i] != null) {
                        dialog.append("Number of images in time window := " +
                                       timeWindowImages[i] + "\n");
                    }
                }
                if (imageDuration != null) {
                    if (imageDuration[i] != null) {
                        dialog.append("Image duration (sec) := " + imageDuration[i] + "\n");
                    }
                }
                if (framingMethod != null) {
                    if (framingMethod[i] != null) {
                        dialog.append("Framing method := " + framingMethod[i] + "\n");
                    }
                }
                if (timeWindowLowerLimit != null) {
                    if (timeWindowLowerLimit[i] != null) {
                        dialog.append("Time window lower limit (sec) := " +
                                       timeWindowLowerLimit[i] + "\n");
                    }
                }
                if (timeWindowUpperLimit != null) {
                    if (timeWindowUpperLimit[i] != null) {
                        dialog.append("Time window upper limit (sec) := " +
                                       timeWindowUpperLimit[i] + "\n");
                    }
                }
                if (RRCycles != null) {
                    if (RRCycles[i] != null) {
                        dialog.append("% R-R cycles acquired this window := " +
                                       RRCycles[i] + "\n");
                    }
                }
                if (acquiredCardiacCycles != null) {
                    if (acquiredCardiacCycles[i] != null) {
                        dialog.append("number of cardiac cycles (acquired) := " +
                                       acquiredCardiacCycles[i] + "\n");
                    }
                }
                if (acquiredStudyDuration != null) {
                    if (acquiredStudyDuration[i] != null) {
                        dialog.append("study time (acquired) sec := " +
                        acquiredStudyDuration[i] + "\n");
                    }
                }
                if (maximumPixelCount != null) {
                    if (maximumPixelCount[i] != null) {
                        dialog.append("Maximum pixel count := " + maximumPixelCount[i] + "\n");
                    }
                }
                if (RRHistogram != null) {
                    if (RRHistogram[i] != null) {
                        dialog.append("R-R histogram := " + RRHistogram[i] + "\n");
                    }
                }
            } // for (i = 0; i < timeWindowsNumber; i++)
            return;
        } // if (gatedStudyGeneral)

        if (dynamicStudyGeneral) {
            dialog.append("DYNAMIC STUDY (General) \n");
            dialog.append("Number of frame groups := " + frameGroupNumber + "\n");
            for (i = 0; i < fGroupNumber; i++) {
                dialog.append("Dynamic Study (each frame group) :=\n");
                dialog.append("Frame group number := " + (fGroupNumber + 1) + "\n");
                if (matrixSize1 != null) {
                    if (matrixSize1[i] != null) {
                        dialog.append("Matrix size [1] := " + matrixSize1[i] + "\n");
                    }
                }
                if (matrixSize2 != null) {
                    if (matrixSize2[i] != null) {
                        dialog.append("Matrix size [2] := " + matrixSize2[i] + "\n");
                    }
                }
                if (numberFormat != null) {
                    if (numberFormat[i] != null) {
                        dialog.append("Number format := " + numberFormat[i] + "\n");
                    }
                }
                if (bytesPerPixel != null) {
                    if (bytesPerPixel[i] != null) {
                        dialog.append("Number of bytes per pixel := " +
                                       bytesPerPixel[i] + "\n");
                    }
                }
                if (scalingFactor1 != null) {
                    if (scalingFactor1[i] != null) {
                        dialog.append("Scaling factor (mm/pixel) [1] := " +
                                      scalingFactor1[i] + "\n");
                    }
                }
                if (scalingFactor2 != null) {
                    if (scalingFactor2[i] != null) {
                        dialog.append("Scaling factor (mm/pixel) [2] := " +
                                      scalingFactor2[i] + "\n");
                    }
                }
                if (frameGroupImages != null) {
                    if (frameGroupImages[i] != null) {
                        dialog.append("Number of images this frame group := " +
                                       frameGroupImages[i] + "\n");
                    }
                }
                if (imageDuration != null) {
                    if (imageDuration[i] != null) {
                        dialog.append("Image duration (sec) := " +
                                       imageDuration[i] + "\n");
                    }
                }
                if (pauseBetweenImages != null) {
                    if (pauseBetweenImages[i] != null) {
                        dialog.append("Pause between images (sec) := " +
                                       pauseBetweenImages[i] + "\n");
                    }
                }
                if (pauseBetweenFrameGroups != null) {
                    if (pauseBetweenFrameGroups[i] != null) {
                        dialog.append("Pause between frame groups (sec) := " +
                                       pauseBetweenFrameGroups[i] + "\n");
                    }
                }
                if (maximumPixelCountInGroup != null) {
                    if (maximumPixelCountInGroup[i] != null) {
                        dialog.append("Maximum pixel count in group := " +
                                       maximumPixelCountInGroup[i] + "\n");
                    }
                }
            }
            return;
        } // if (dynamicStudyGeneral)

        if (detectorHeadNumber != null) {
            dialog.append("Number of detector heads = " + detectorHeadNumber + "\n");
        }

        if (imagesPerEWindow != null) {
            dialog.append("Number of images/energy window = " + imagesPerEWindow + "\n");
        }


        if (haveStaticStudy) {
            for (i = 0; i < numberImagesPerEWindow; i++) {
                dialog.append("Static Study (each frame) :=\n");
                dialog.append("Image Number := " + (i+1) + "\n");
                if (matrixSize1 != null) {
                    if (matrixSize1[i] != null) {
                        dialog.append("Matrix size [1] := " + matrixSize1[i] + "\n");
                    }
                }
                if (matrixSize2 != null) {
                    if (matrixSize2[i] != null) {
                        dialog.append("Matrix size [2] := " + matrixSize2[i] + "\n");
                    }
                }
                if (numberFormat != null) {
                    if (numberFormat[i] != null) {
                        dialog.append("Number format := " + numberFormat[i] + "\n");
                    }
                }
                if (bytesPerPixel != null) {
                    if (bytesPerPixel[i] != null) {
                        dialog.append("Number of bytes per pixel := " +
                                       bytesPerPixel[i] + "\n");
                    }
                }
                if (scalingFactor1 != null) {
                    if (scalingFactor1[i] != null) {
                        dialog.append("Scaling factor (mm/pixel) [1] := " +
                                      scalingFactor1[i] + "\n");
                    }
                }
                if (scalingFactor2 != null) {
                    if (scalingFactor2[i] != null) {
                        dialog.append("Scaling factor (mm/pixel) [2] := " +
                                      scalingFactor2[i] + "\n");
                    }
                }
                if (imageDuration != null) {
                    if (imageDuration[i] != null) {
                        dialog.append("Image duration (sec) := " +
                                       imageDuration[i] + "\n");
                    }
                }
                if (imageStartTime != null) {
                    if (imageStartTime[i] != null) {
                        dialog.append("Image start time := " +
                                       imageStartTime[i] + "\n");
                    }
                }
                if (label != null) {
                    if (label[i] != null) {
                        dialog.append("Label := " + label[i] + "\n");
                    }
                }
                if (maximumPixelCount != null) {
                    if (maximumPixelCount[i] != null) {
                        dialog.append("Maximum pixel count := " +
                                       maximumPixelCount[i] + "\n");
                    }
                }
                if (totalCounts != null) {
                    if (totalCounts[i] != null) {
                        dialog.append("Total counts := " + totalCounts[i] + "\n");
                    }
                }
            }
            return;
        } // if (haveStaticStudy)

        if (processStatus != null) {
            dialog.append("Process status = " + processStatus + "\n");
        }

        // This refers to the number of dimensions with distances.  It does
        // not include the time dimension.  It is confusing so do not display.
        //if (dimensionNumber != null) {
            //dialog.append("Number of dimensions = " + dimensionNumber + "\n");
        //}

        if (startHorizontalBedPosition != null) {
            dialog.append("Start horizontal bed position (mm) = " + startHorizontalBedPosition + "\n");
        }

        if (timeFrames != null) {
            dialog.append("Time frames = " + timeFrames + "\n");
        }

        if (projectionNumber != null) {
            dialog.append("Number of projections = " + projectionNumber + "\n");
        }

        if (rotationExtent != null) {
            dialog.append("Extent of rotation = " + rotationExtent + "\n");
        }

        if (projectionTime != null) {
            dialog.append("Time per projection (sec) = " + projectionTime + "\n");
        }

        if (studyDuration != null) {
            dialog.append("Study duration (sec) = " + studyDuration + "\n");
        }

        if (maximumPixelCount != null) {
            if (maximumPixelCount[0] != null) {
                if (haveTomographic) {
                    dialog.append("Maximum pixel count for scaling with this head and this energy window = " +
                               maximumPixelCount[0] + "\n");
                }
            }
        } // if (maximumPixelCount != null)

        if (patientOrientation != null) {
            dialog.append("Patient orientation = " + patientOrientation + "\n");
        }

        if (patientRotation != null) {
            dialog.append("Patient rotation = " + patientRotation + "\n");
        }

        if (PETStudyImageData) {
            dialog.append("PET STUDY (Image Data)\n");
        }

        if (spectStudyReconstructedData) {
            dialog.append("SPECT STUDY (reconstructed data)\n");
        }

        if (reconstructionMethod != null) {
            dialog.append("Method of reconstruction = " + reconstructionMethod + "\n");
        }

        if (sliceNumber != null) {
            if ((haveTomographic) && (haveReconstructed)) {
                dialog.append("Number of slices for this head and this energy window = " +
                               sliceNumber + "\n");
            }
        } // if (sliceNumber != null)

        if (referenceFrameNumber != null) {
            dialog.append("Reference frame number = " + referenceFrameNumber + "\n");
        }

        if (sliceThickness != null) {
            dialog.append("Slice thickness (pixels) = " + sliceThickness + "\n");
        }

        if (centerCenter != null) {
            dialog.append("Center-center slice separation (pixels) = " + centerCenter + "\n");
        }

        if (filterName != null) {
            dialog.append("Filer name = " + filterName + "\n");
        }

        if (zAxisFilter != null) {
            dialog.append("Z-axis filter = " + zAxisFilter + "\n");
        }

        if (appliedCorrections != null) {
            dialog.append("Applied corrections = " + appliedCorrections + "\n");
        }

        if (attenuationCorrection != null) {
            dialog.append("Method of attenuation correction = " + attenuationCorrection + "\n");
        }

        if (scatterCorrected != null) {
            dialog.append("Scatter corrected = " + scatterCorrected + "\n");
        }

        if (scatterCorrectionMethod != null) {
            dialog.append("Method of scatter correction = " + scatterCorrectionMethod + "\n");
        }

        if (obliqueReconstruction != null) {
            dialog.append("Oblique reconstruction = " + obliqueReconstruction + "\n");
        }

        if (imageDataDescription) {
            dialog.append("IMAGE DATA DESCRIPTION\n");
        }

        if (indexNestingLevel != null) {
            dialog.append("Index nesting level = " + indexNestingLevel + "\n");
        }

        for (i = 0; i < timeFrameNumber; i++) {
            if (timeFrameNumber == 1) {
                if (imageDuration[0] != null) {
                    dialog.append("Image duration (sec) = " + imageDuration[0] + "\n");
                }
                if (imageRelativeStartTime[0] != null) {
                    dialog.append("Image relative start time (sec) = " +
                                   imageRelativeStartTime[0] + "\n");
                }
            }
            else {
                if (imageDuration[i] != null) {
                    dialog.append("Image duration (sec) [" + (i+1) + "] = " +
                                   imageDuration[i] + "\n");
                }
                if (imageRelativeStartTime[i] != null) {
                    dialog.append("Image relative start time (sec) [" + (i+1) + "] = " +
                                   imageRelativeStartTime[i] + "\n");
                }
            }
        } // for (i = 0; i < timeFrameNumber; i++)
    }
}
