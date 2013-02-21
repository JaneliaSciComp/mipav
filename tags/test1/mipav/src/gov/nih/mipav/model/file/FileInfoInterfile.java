package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how an Interfile image is stored on disk.
 */

public class FileInfoInterfile extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1860672784494846816L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String[] acquiredCardiacCycles = null;

    /** DOCUMENT ME! */
    private int acquiredCardiacCyclesIndex = 0;

    /** DOCUMENT ME! */
    private String[] acquiredStudyDuration = null;

    /** DOCUMENT ME! */
    private int acquiredStudyDurationIndex = 0;

    /** DOCUMENT ME! */
    private String appliedCorrections = null;

    /** DOCUMENT ME! */
    private String attenuationCorrection = null;

    /** DOCUMENT ME! */
    private String[] betaHalflife = null;

    /** DOCUMENT ME! */
    private String[] branchingFactor = null;

    /** DOCUMENT ME! */
    private String[] bytesPerPixel = null;

    /** DOCUMENT ME! */
    private int bytesPerPixelIndex = 0;

    /** DOCUMENT ME! */
    private String centerCenter = null;

    /** DOCUMENT ME! */
    private String contactPerson = null;

    /** DOCUMENT ME! */
    private String conversionProgram = null;

    /** DOCUMENT ME! */
    private String dataCompression = null;

    /** DOCUMENT ME! */
    private String dataDescription = null;

    /** DOCUMENT ME! */
    private String dataEncode = null;

    /** DOCUMENT ME! */
    private String decayCorrected = null;

    /** DOCUMENT ME! */
    private String detectorHeadNumber = null;

    /** DOCUMENT ME! */
    private String dimensionNumber = null;

    /** DOCUMENT ME! */
    private boolean dynamicStudyGeneral = false;

    /** DOCUMENT ME! */
    private String elapsedStudyDuration = null;

    /** DOCUMENT ME! */
    private int energyNumber = 0;

    /** DOCUMENT ME! */
    private String[] energyWindowName = null;

    /** DOCUMENT ME! */
    private String energyWindowsNumber = null;

    /** DOCUMENT ME! */
    private String examType = null;

    /** DOCUMENT ME! */
    private int fGroupNumber = 1;

    /** DOCUMENT ME! */
    private String filterName = null;

    /** DOCUMENT ME! */
    private String floodCorrected = null;

    /** DOCUMENT ME! */
    private String[] frameGroupImages = null;

    /** DOCUMENT ME! */
    private int frameGroupImagesIndex = 0;

    /** DOCUMENT ME! */
    private String frameGroupNumber = null;

    /** DOCUMENT ME! */
    private String[] framingMethod = null;

    /** DOCUMENT ME! */
    private int framingMethodIndex = 0;

    /** DOCUMENT ME! */
    private String[] gammaHalflife = null;

    /** DOCUMENT ME! */
    private boolean gatedStudyGeneral = false;

    /** DOCUMENT ME! */
    private boolean generalData = false;

    /** DOCUMENT ME! */
    private boolean generalImageData = false;

    /** DOCUMENT ME! */
    private boolean haveGated = false;

    /** DOCUMENT ME! */
    private boolean haveReconstructed = false;

    /** DOCUMENT ME! */
    private boolean haveStaticStudy = false;

    /** DOCUMENT ME! */
    private boolean haveTomographic = false;

    /** DOCUMENT ME! */
    private int i;

    /** DOCUMENT ME! */
    private boolean imageDataDescription = false;

    /** DOCUMENT ME! */
    private String[] imageDuration = null;

    /** DOCUMENT ME! */
    private int imageDurationIndex = 0;

    /** DOCUMENT ME! */
    private String[] imageRelativeStartTime = null;

    /** DOCUMENT ME! */
    private int imageRelativeStartTimeIndex = 0;

    /** DOCUMENT ME! */
    private String imagesPerEWindow = null;

    /** DOCUMENT ME! */
    private String[] imageStartTime = null;

    /** DOCUMENT ME! */
    private int imageStartTimeIndex = 0;

    /** DOCUMENT ME! */
    private String imagingModality = null;

    /** DOCUMENT ME! */
    private String indexNestingLevel = null;

    /** DOCUMENT ME! */
    private String interfileDataType = null;

    /** DOCUMENT ME! */
    private int iNumber;

    /** DOCUMENT ME! */
    private int isoNumber = 1;

    /** DOCUMENT ME! */
    private String[] isotopeName = null;

    /** DOCUMENT ME! */
    private String isotopeNumber = null;

    /** DOCUMENT ME! */
    private String keysDate = null;

    /** DOCUMENT ME! */
    private String keysVersion = null;

    /** DOCUMENT ME! */
    private String[] label = null;

    /** DOCUMENT ME! */
    private int labelIndex = 0;

    /** DOCUMENT ME! */
    private String[] lowerLevel = null;

    /** DOCUMENT ME! */
    private String[] matrixSize1 = null;

    /** DOCUMENT ME! */
    private int matrixSize1Index = 0;

    /** DOCUMENT ME! */
    private String[] matrixSize2 = null;

    /** DOCUMENT ME! */
    private int matrixSize2Index = 0;

    /** DOCUMENT ME! */
    private String[] maximumPixelCount = null;

    /** DOCUMENT ME! */
    private int maximumPixelCountIndex = 0;

    /** DOCUMENT ME! */
    private String[] maximumPixelCountInGroup = null;

    /** DOCUMENT ME! */
    private int maximumPixelCountInGroupIndex = 0;

    /** DOCUMENT ME! */
    private String[] numberFormat = null;

    /** DOCUMENT ME! */
    private int numberFormatIndex = 0;

    @SuppressWarnings("unused")
    private int numberImages; // numberImagesPerEWindow * energyNumber

    /** DOCUMENT ME! */
    private int numberImagesPerEWindow;

    /** DOCUMENT ME! */
    private String obliqueReconstruction = null;

    /** DOCUMENT ME! */
    private String observedCardiacCycles = null;

    /** DOCUMENT ME! */
    private String originalInstitution = null;

    /** DOCUMENT ME! */
    private String originatingSystem = null;

    /** DOCUMENT ME! */
    private String patientDOB = null;

    /** DOCUMENT ME! */
    private String patientID = null;

    /** DOCUMENT ME! */
    private String patientName = null;

    /** DOCUMENT ME! */
    private String patientOrientation = null;

    /** DOCUMENT ME! */
    private String patientRotation = null;

    /** DOCUMENT ME! */
    private String patientSex = null;

    /** DOCUMENT ME! */
    private String[] pauseBetweenFrameGroups = null;

    /** DOCUMENT ME! */
    private int pauseBetweenFrameGroupsIndex = 0;

    /** DOCUMENT ME! */
    private String[] pauseBetweenImages = null;

    /** DOCUMENT ME! */
    private int pauseBetweenImagesIndex = 0;

    /** DOCUMENT ME! */
    private String PETDataType = null;

    /** DOCUMENT ME! */
    private boolean PETStudyGeneral = false;

    /** DOCUMENT ME! */
    private boolean PETStudyImageData = false;

    /** DOCUMENT ME! */
    private String processStatus = null;

    /** DOCUMENT ME! */
    private String programAuthor = null;

    /** DOCUMENT ME! */
    private String programDate = null;

    /** DOCUMENT ME! */
    private String programVersion = null;

    /** DOCUMENT ME! */
    private String projectionNumber = null;

    /** DOCUMENT ME! */
    private String projectionTime = null;

    /** DOCUMENT ME! */
    private String quantificationUnits = null;

    /** DOCUMENT ME! */
    private String radiopharmaceutical = null;

    /** DOCUMENT ME! */
    private String reconstructionMethod = null;

    /** DOCUMENT ME! */
    private String referenceFrameNumber = null;

    /** DOCUMENT ME! */
    private String rotationExtent = null;

    /** DOCUMENT ME! */
    private String[] RRCycles = null;

    /** DOCUMENT ME! */
    private int RRCyclesIndex = 0;

    /** DOCUMENT ME! */
    private String[] RRHistogram = null;

    /** DOCUMENT ME! */
    private int RRHistogramIndex = 0;

    /** DOCUMENT ME! */
    private String[] scalingFactor1 = null;

    /** DOCUMENT ME! */
    private int scalingFactor1Index = 0;

    /** DOCUMENT ME! */
    private String[] scalingFactor2 = null;

    /** DOCUMENT ME! */
    private int scalingFactor2Index = 0;

    /** DOCUMENT ME! */
    private String scannerQuantificationFactor = null;

    /** DOCUMENT ME! */
    private String scatterCorrected = null;

    /** DOCUMENT ME! */
    private String scatterCorrectionMethod = null;

    /** DOCUMENT ME! */
    private String sliceNumber = null;

    /** DOCUMENT ME! */
    private boolean spectStudyGeneral = false;

    /** DOCUMENT ME! */
    private boolean spectStudyReconstructedData = false;

    /** DOCUMENT ME! */
    private String startHorizontalBedPosition = null;

    /** DOCUMENT ME! */
    private boolean staticStudyGeneral = false;

    /** DOCUMENT ME! */
    private String studyDate = null;

    /** DOCUMENT ME! */
    private String studyDuration = null;

    /** DOCUMENT ME! */
    private String studyID = null;

    /** DOCUMENT ME! */
    private String studyTime = null;

    /** DOCUMENT ME! */
    private int tFrame;

    /** DOCUMENT ME! */
    private int timeFrameNumber = 0;

    /** DOCUMENT ME! */
    private String timeFrames = null;

    /** DOCUMENT ME! */
    private String[] timeWindowImages = null;

    /** DOCUMENT ME! */
    private int timeWindowImagesIndex = 0;

    /** DOCUMENT ME! */
    private String[] timeWindowLowerLimit = null;

    /** DOCUMENT ME! */
    private int timeWindowLowerLimitIndex = 0;

    /** DOCUMENT ME! */
    private String timeWindows = null;

    /** DOCUMENT ME! */
    private int timeWindowsNumber = 1;

    /** DOCUMENT ME! */
    private String[] timeWindowUpperLimit = null;

    /** DOCUMENT ME! */
    private int timeWindowUpperLimitIndex = 0;

    /** DOCUMENT ME! */
    private String[] totalCounts = null;

    /** DOCUMENT ME! */
    private int totalCountsIndex = 0;

    /** DOCUMENT ME! */
    private String totalImageNumber = null;

    /** DOCUMENT ME! */
    private String[] upperLevel = null;

    /** DOCUMENT ME! */
    private int wNumber;

    /** DOCUMENT ME! */
    private String zAxisFilter = null;
    
    private String organ = null;
    
    private String gatedFrameMode[] = null;
    
    private int gatedFrameModeIndex = 0;
    
    private String windowA = null;
    
    private String windowB = null;
    
    private String windowC = null;
    
    private String sliceOrientation = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoInterfile - file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoInterfile(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
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
        
        if (organ != null) {
            dialog.append("Organ = " + organ + "\n");
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
        
        if (windowA != null) {
            dialog.append("Window A = " + windowA + "\n");
        }
        
        if (windowB != null) {
            dialog.append("Window B = " + windowB + "\n");
        }
        
        if (windowC != null) {
            dialog.append("Window C = " + windowC + "\n");
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
                    dialog.append("Isotope name [" + (i + 1) + "] = " + isotopeName[i] + "\n");
                }

                if (betaHalflife[i] != null) {
                    dialog.append("Isotope beta halflife (sec) [" + (i + 1) + "] = " + betaHalflife[i] + "\n");
                }

                if (gammaHalflife[i] != null) {
                    dialog.append("Isotope gamma halflife (sec) [" + (i + 1) + "] = " + gammaHalflife[i] + "\n");
                }

                if (branchingFactor[i] != null) {
                    dialog.append("Isotope branching factor [" + (i + 1) + "] = " + branchingFactor[i] + "\n");
                }
            }
        } // for (i = 0; i < iNumber; i++)

        if (radiopharmaceutical != null) {
            dialog.append("Radiopharmaceutical = " + radiopharmaceutical + "\n");
        }

        for (i = 0; i < energyNumber; i++) {

            if (energyWindowName[i] != null) {
                dialog.append("Energy window[" + (i + 1) + "] = " + energyWindowName[i] + "\n");
            }

            if (lowerLevel[i] != null) {
                dialog.append("Energy window lower level[" + (i + 1) + "] = " + lowerLevel[i] + "\n");
            }

            if (upperLevel[i] != null) {
                dialog.append("Energy window upper level[" + (i + 1) + "] = " + upperLevel[i] + "\n");
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
                dialog.append("Study duration (elapsed) sec = " + elapsedStudyDuration + "\n");
            }

            if (observedCardiacCycles != null) {
                dialog.append("Number of cardiac cycles (observed) := " + observedCardiacCycles + "\n");
            }

            dialog.append("number of time windows := " + timeWindowsNumber + "\n");

            for (i = 0; i < timeWindowsNumber; i++) {
                dialog.append("Gated Study (each time window) := \n");
                dialog.append("Time window number := " + (i + 1) + "\n");

                if (timeWindowImages != null) {

                    if (timeWindowImages[i] != null) {
                        dialog.append("Number of images in time window := " + timeWindowImages[i] + "\n");
                    }
                }

                if (imageDuration != null) {

                    if (imageDuration[i] != null) {
                        dialog.append("Image duration (sec) := " + imageDuration[i] + "\n");
                    }
                }
                
                if (gatedFrameMode != null) {
                    
                    if (gatedFrameMode[i] != null) {
                        dialog.append("Gated frame mode := " + gatedFrameMode[i] + "\n");
                    }
                }

                if (framingMethod != null) {

                    if (framingMethod[i] != null) {
                        dialog.append("Framing method := " + framingMethod[i] + "\n");
                    }
                }

                if (timeWindowLowerLimit != null) {

                    if (timeWindowLowerLimit[i] != null) {
                        dialog.append("Time window lower limit (sec) := " + timeWindowLowerLimit[i] + "\n");
                    }
                }

                if (timeWindowUpperLimit != null) {

                    if (timeWindowUpperLimit[i] != null) {
                        dialog.append("Time window upper limit (sec) := " + timeWindowUpperLimit[i] + "\n");
                    }
                }

                if (RRCycles != null) {

                    if (RRCycles[i] != null) {
                        dialog.append("% R-R cycles acquired this window := " + RRCycles[i] + "\n");
                    }
                }

                if (acquiredCardiacCycles != null) {

                    if (acquiredCardiacCycles[i] != null) {
                        dialog.append("number of cardiac cycles (acquired) := " + acquiredCardiacCycles[i] + "\n");
                    }
                }

                if (acquiredStudyDuration != null) {

                    if (acquiredStudyDuration[i] != null) {
                        dialog.append("study time (acquired) sec := " + acquiredStudyDuration[i] + "\n");
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
                dialog.append("Frame group number := " + (i + 1) + "\n");

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
                        dialog.append("Number of bytes per pixel := " + bytesPerPixel[i] + "\n");
                    }
                }

                if (scalingFactor1 != null) {

                    if (scalingFactor1[i] != null) {
                        dialog.append("Scaling factor (mm/pixel) [1] := " + scalingFactor1[i] + "\n");
                    }
                }

                if (scalingFactor2 != null) {

                    if (scalingFactor2[i] != null) {
                        dialog.append("Scaling factor (mm/pixel) [2] := " + scalingFactor2[i] + "\n");
                    }
                }

                if (frameGroupImages != null) {

                    if (frameGroupImages[i] != null) {
                        dialog.append("Number of images this frame group := " + frameGroupImages[i] + "\n");
                    }
                }

                if (imageDuration != null) {

                    if (imageDuration[i] != null) {
                        dialog.append("Image duration (sec) := " + imageDuration[i] + "\n");
                    }
                }

                if (pauseBetweenImages != null) {

                    if (pauseBetweenImages[i] != null) {
                        dialog.append("Pause between images (sec) := " + pauseBetweenImages[i] + "\n");
                    }
                }

                if (pauseBetweenFrameGroups != null) {

                    if (pauseBetweenFrameGroups[i] != null) {
                        dialog.append("Pause between frame groups (sec) := " + pauseBetweenFrameGroups[i] + "\n");
                    }
                }

                if (maximumPixelCountInGroup != null) {

                    if (maximumPixelCountInGroup[i] != null) {
                        dialog.append("Maximum pixel count in group := " + maximumPixelCountInGroup[i] + "\n");
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
                dialog.append("Image Number := " + (i + 1) + "\n");

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
                        dialog.append("Number of bytes per pixel := " + bytesPerPixel[i] + "\n");
                    }
                }

                if (scalingFactor1 != null) {

                    if (scalingFactor1[i] != null) {
                        dialog.append("Scaling factor (mm/pixel) [1] := " + scalingFactor1[i] + "\n");
                    }
                }

                if (scalingFactor2 != null) {

                    if (scalingFactor2[i] != null) {
                        dialog.append("Scaling factor (mm/pixel) [2] := " + scalingFactor2[i] + "\n");
                    }
                }

                if (imageDuration != null) {

                    if (imageDuration[i] != null) {
                        dialog.append("Image duration (sec) := " + imageDuration[i] + "\n");
                    }
                }

                if (imageStartTime != null) {

                    if (imageStartTime[i] != null) {
                        dialog.append("Image start time := " + imageStartTime[i] + "\n");
                    }
                }

                if (label != null) {

                    if (label[i] != null) {
                        dialog.append("Label := " + label[i] + "\n");
                    }
                }

                if (maximumPixelCount != null) {

                    if (maximumPixelCount[i] != null) {
                        dialog.append("Maximum pixel count := " + maximumPixelCount[i] + "\n");
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
        // if (dimensionNumber != null) {
        // dialog.append("Number of dimensions = " + dimensionNumber + "\n");
        // }

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
                dialog.append("Number of slices for this head and this energy window = " + sliceNumber + "\n");
            }
        } // if (sliceNumber != null)
        
        if (sliceOrientation != null) {
            dialog.append("Slice orientation = " + sliceOrientation + "\n");
        }

        if (referenceFrameNumber != null) {
            dialog.append("Reference frame number = " + referenceFrameNumber + "\n");
        }

        if (getSliceThickness() != 0) {
            dialog.append("Slice thickness (pixels) = " + getSliceThickness() + "\n");
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
                    dialog.append("Image relative start time (sec) = " + imageRelativeStartTime[0] + "\n");
                }
            } else {

                if (imageDuration[i] != null) {
                    dialog.append("Image duration (sec) [" + (i + 1) + "] = " + imageDuration[i] + "\n");
                }

                if (imageRelativeStartTime[i] != null) {
                    dialog.append("Image relative start time (sec) [" + (i + 1) + "] = " + imageRelativeStartTime[i] +
                                  "\n");
                }
            }
        } // for (i = 0; i < timeFrameNumber; i++)
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean generalImageData() {
        return generalImageData;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getAcquiredCardiacCycles() {
        return acquiredCardiacCycles;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getAcquiredStudyDuration() {
        return acquiredStudyDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getAppliedCorrections() {
        return appliedCorrections;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getAttenuationCorrection() {
        return attenuationCorrection;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getBetaHalflife() {
        return betaHalflife;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getBranchingFactor() {
        return branchingFactor;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getBytesPerPixel() {
        return bytesPerPixel;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getCenterCenter() {
        return centerCenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getContactPerson() {
        return contactPerson;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getConversionProgram() {
        return conversionProgram;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDataCompression() {
        return dataCompression;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDataDescription() {
        return dataDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDataEncode() {
        return dataEncode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDecayCorrected() {
        return decayCorrected;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDetectorHeadNumber() {
        return detectorHeadNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDimensionNumber() {
        return dimensionNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getDynamicStudyGeneral() {
        return dynamicStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getElapsedStudyDuration() {
        return elapsedStudyDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getEnergyWindow() {
        return energyWindowName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getEnergyWindowLowerLevel() {
        return lowerLevel;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getEnergyWindowsNumber() {
        return energyWindowsNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getEnergyWindowUpperLevel() {
        return upperLevel;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getExamType() {
        return examType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getFilterName() {
        return filterName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getFloodCorrected() {
        return floodCorrected;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getFrameGroupImages() {
        return frameGroupImages;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getFrameGroupNumber() {
        return frameGroupNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getFramingMethod() {
        return framingMethod;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getGammaHalflife() {
        return gammaHalflife;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getGatedStudyGeneral() {
        return gatedStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getGeneralData() {
        return generalData;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getImageDataDescription() {
        return imageDataDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getImageDuration() {
        return imageDuration;
    }
    
    public String[] getGatedFrameMode() {
        return gatedFrameMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getImageRelativeStartTime() {
        return imageRelativeStartTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getImagesPerEWindow() {
        return imagesPerEWindow;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getImageStartTime() {
        return imageStartTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getImagingModality() {
        return imagingModality;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getIndexNestingLevel() {
        return indexNestingLevel;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getInterfileDataType() {
        return interfileDataType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getIsotopeName() {
        return isotopeName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getIsotopeNumber() {
        return isotopeNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getKeysDate() {
        return keysDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getKeysVersion() {
        return keysVersion;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getLabel() {
        return label;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getMatrixSize1() {
        return matrixSize1;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getMatrixSize2() {
        return matrixSize2;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getMaximumPixelCount() {
        return maximumPixelCount;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getMaximumPixelCountInGroup() {
        return maximumPixelCountInGroup;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getNumberFormat() {
        return numberFormat;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getObliqueReconstruction() {
        return obliqueReconstruction;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getObservedCardiacCycles() {
        return observedCardiacCycles;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getOriginalInstitution() {
        return originalInstitution;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getOriginatingSystem() {
        return originatingSystem;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPatientDOB() {
        return patientDOB;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPatientID() {
        return patientID;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPatientName() {
        return patientName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPatientOrientation() {
        return patientOrientation;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPatientRotation() {
        return patientRotation;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPatientSex() {
        return patientSex;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getPauseBetweenFrameGroups() {
        return pauseBetweenFrameGroups;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getPauseBetweenImages() {
        return pauseBetweenImages;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPETDataType() {
        return PETDataType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getPETStudyGeneral() {
        return PETStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getPETStudyImageData() {
        return PETStudyImageData;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getProcessStatus() {
        return processStatus;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getProgramAuthor() {
        return programAuthor;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getProgramDate() {
        return programDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getProgramVersion() {
        return programVersion;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getProjectionNumber() {
        return projectionNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getProjectionTime() {
        return projectionTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getQuantificationUnits() {
        return quantificationUnits;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getRadiopharmaceutical() {
        return radiopharmaceutical;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getReconstructionMethod() {
        return reconstructionMethod;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getReferenceFrameNumber() {
        return referenceFrameNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getRotationExtent() {
        return rotationExtent;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getRRCycles() {
        return RRCycles;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getRRHistogram() {
        return RRHistogram;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getScalingFactor1() {
        return scalingFactor1;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getScalingFactor2() {
        return scalingFactor2;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getScannerQuantificationFactor() {
        return scannerQuantificationFactor;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getScatterCorrected() {
        return scatterCorrected;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getScatterCorrectionMethod() {
        return scatterCorrectionMethod;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getSliceNumber() {
        return sliceNumber;
    }
    
    public String getSliceOrientation() {
        return sliceOrientation;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getSpectStudyGeneral() {
        return spectStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getSpectStudyReconstructedData() {
        return spectStudyReconstructedData;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getStartHorizontalBedPosition() {
        return startHorizontalBedPosition;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getStaticStudyGeneral() {
        return staticStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getStudyDate() {
        return studyDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getStudyDuration() {
        return studyDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getStudyID() {
        return studyID;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getStudyTime() {
        return studyTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getTimeFrames() {
        return timeFrames;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getTimeWindowImages() {
        return timeWindowImages;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getTimeWindowLowerLimit() {
        return timeWindowLowerLimit;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getTimeWindows() {
        return timeWindows;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getTimeWindowUpperLimit() {
        return timeWindowUpperLimit;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String[] getTotalCounts() {
        return totalCounts;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getTotalImageNumber() {
        return totalImageNumber;
    }
    
    public String getWindowA() {
        return windowA;
    }
    
    public String getWindowB() {
        return windowB;
    }
    
    public String getWindowC() {
        return windowC;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getZAxisFilter() {
        return zAxisFilter;
    }
    
    public void setOrgan(String organ) {
        this.organ = organ;
    }
    
    public String getOrgan() {
        return organ;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  aCardiacCycles  DOCUMENT ME!
     */
    public void setAcquiredCardiacCycles(String aCardiacCycles) {

        if (acquiredCardiacCycles == null) {
            acquiredCardiacCycles = new String[1];
        }

        acquiredCardiacCycles[acquiredCardiacCyclesIndex++] = aCardiacCycles;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  aStudyDuration  DOCUMENT ME!
     */
    public void setAcquiredStudyDuration(String aStudyDuration) {

        if (acquiredStudyDuration == null) {
            acquiredStudyDuration = new String[1];
        }

        acquiredStudyDuration[acquiredStudyDurationIndex++] = aStudyDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  appliedCorrections  DOCUMENT ME!
     */
    public void setAppliedCorrections(String appliedCorrections) {
        this.appliedCorrections = appliedCorrections;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  attenuationCorrection  DOCUMENT ME!
     */
    public void setAttenuationCorrection(String attenuationCorrection) {
        this.attenuationCorrection = attenuationCorrection;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentIsotopeNumber  DOCUMENT ME!
     * @param  currentBetaHalflife   DOCUMENT ME!
     */
    public void setBetaHalflife(String currentIsotopeNumber, String currentBetaHalflife) {
        iNumber = Integer.valueOf(currentIsotopeNumber).intValue();

        if (betaHalflife == null) {
            betaHalflife = new String[1];
        }

        betaHalflife[iNumber - 1] = currentBetaHalflife;
    }


    /**
     * DOCUMENT ME!
     *
     * @param  currentIsotopeNumber    DOCUMENT ME!
     * @param  currentBranchingFactor  DOCUMENT ME!
     */
    public void setBranchingFactor(String currentIsotopeNumber, String currentBranchingFactor) {
        iNumber = Integer.valueOf(currentIsotopeNumber).intValue();

        if (branchingFactor == null) {
            branchingFactor = new String[1];
        }

        branchingFactor[iNumber - 1] = currentBranchingFactor;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bpp  DOCUMENT ME!
     */
    public void setBytesPerPixel(String bpp) {

        if (bytesPerPixel == null) {
            bytesPerPixel = new String[1];
        }

        bytesPerPixel[bytesPerPixelIndex++] = bpp;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  centerCenter  DOCUMENT ME!
     */
    public void setCenterCenter(String centerCenter) {
        this.centerCenter = centerCenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  contactPerson  DOCUMENT ME!
     */
    public void setContactPerson(String contactPerson) {
        this.contactPerson = contactPerson;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  conversionProgram  DOCUMENT ME!
     */
    public void setConversionProgram(String conversionProgram) {
        this.conversionProgram = conversionProgram;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dataCompression  DOCUMENT ME!
     */
    public void setDataCompression(String dataCompression) {
        this.dataCompression = dataCompression;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dataDescription  DOCUMENT ME!
     */
    public void setDataDescription(String dataDescription) {
        this.dataDescription = dataDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dataEncode  DOCUMENT ME!
     */
    public void setDataEncode(String dataEncode) {
        this.dataEncode = dataEncode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  decayCorrected  DOCUMENT ME!
     */
    public void setDecayCorrected(String decayCorrected) {
        this.decayCorrected = decayCorrected;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  detectorHeadNumber  DOCUMENT ME!
     */
    public void setDetectorHeadNumber(String detectorHeadNumber) {
        this.detectorHeadNumber = detectorHeadNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dimensionNumber  DOCUMENT ME!
     */
    public void setDimensionNumber(String dimensionNumber) {
        this.dimensionNumber = dimensionNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dynamicStudyGeneral  DOCUMENT ME!
     */
    public void setDynamicStudyGeneral(boolean dynamicStudyGeneral) {
        this.dynamicStudyGeneral = dynamicStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  elapsedStudyDuration  DOCUMENT ME!
     */
    public void setElapsedStudyDuration(String elapsedStudyDuration) {
        this.elapsedStudyDuration = elapsedStudyDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentWindowNumber      DOCUMENT ME!
     * @param  currentEnergyWindowName  DOCUMENT ME!
     */
    public void setEnergyWindow(String currentWindowNumber, String currentEnergyWindowName) {
        wNumber = Integer.valueOf(currentWindowNumber).intValue();
        energyWindowName[wNumber - 1] = currentEnergyWindowName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentWindowNumber  DOCUMENT ME!
     * @param  currentLowerLevel    DOCUMENT ME!
     */
    public void setEnergyWindowLowerLevel(String currentWindowNumber, String currentLowerLevel) {
        wNumber = Integer.valueOf(currentWindowNumber).intValue();
        lowerLevel[wNumber - 1] = currentLowerLevel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  energyWindowsNumber  DOCUMENT ME!
     */
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

    /**
     * DOCUMENT ME!
     *
     * @param  currentWindowNumber  DOCUMENT ME!
     * @param  currentUpperLevel    DOCUMENT ME!
     */
    public void setEnergyWindowUpperLevel(String currentWindowNumber, String currentUpperLevel) {
        wNumber = Integer.valueOf(currentWindowNumber).intValue();
        upperLevel[wNumber - 1] = currentUpperLevel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  examType  DOCUMENT ME!
     */
    public void setExamType(String examType) {
        this.examType = examType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  filterName  DOCUMENT ME!
     */
    public void setFilterName(String filterName) {
        this.filterName = filterName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  floodCorrected  DOCUMENT ME!
     */
    public void setFloodCorrected(String floodCorrected) {
        this.floodCorrected = floodCorrected;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fGroupImages  DOCUMENT ME!
     */
    public void setFrameGroupImages(String fGroupImages) {

        if (frameGroupImages == null) {
            frameGroupImages = new String[1];
        }

        frameGroupImages[frameGroupImagesIndex++] = fGroupImages;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  frameGroupNumber  DOCUMENT ME!
     */
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
        gatedFrameMode = new String[fGroupNumber];
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
            gatedFrameMode[i] = null;
            pauseBetweenImages[i] = null;
            pauseBetweenFrameGroups[i] = null;
            maximumPixelCountInGroup[i] = null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fMethod  DOCUMENT ME!
     */
    public void setFramingMethod(String fMethod) {

        if (framingMethod == null) {
            framingMethod = new String[1];
        }

        framingMethod[framingMethodIndex++] = fMethod;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentIsotopeNumber  DOCUMENT ME!
     * @param  currentGammaHalflife  DOCUMENT ME!
     */
    public void setGammaHalflife(String currentIsotopeNumber, String currentGammaHalflife) {
        iNumber = Integer.valueOf(currentIsotopeNumber).intValue();

        if (gammaHalflife == null) {
            gammaHalflife = new String[1];
        }

        gammaHalflife[iNumber - 1] = currentGammaHalflife;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  gatedStudyGeneral  DOCUMENT ME!
     */
    public void setGatedStudyGeneral(boolean gatedStudyGeneral) {
        this.gatedStudyGeneral = gatedStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  generalData  DOCUMENT ME!
     */
    public void setGeneralData(boolean generalData) {
        this.generalData = generalData;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  generalImageData  DOCUMENT ME!
     */
    public void setGeneralImageData(boolean generalImageData) {
        this.generalImageData = generalImageData;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageDataDescription  DOCUMENT ME!
     */
    public void setImageDataDescription(boolean imageDataDescription) {
        this.imageDataDescription = imageDataDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentImageDuration  DOCUMENT ME!
     */
    public void setImageDuration(String currentImageDuration) {
        if (imageDuration == null) {
            imageDuration = new String[1];
        }
        else if (imageDurationIndex > imageDuration.length - 1) {
            String oldImageDuration[] = new String[imageDuration.length];
            for (i = 0; i < imageDuration.length; i++) {
                oldImageDuration[i] = imageDuration[i];
            }
            imageDuration = new String[imageDurationIndex];
            for (i = 0; i < oldImageDuration.length; i++) {
                imageDuration[i] = oldImageDuration[i];
            }
        }
        imageDuration[imageDurationIndex++] = currentImageDuration;
    }
    
    public void setGatedFrameMode(String currentGatedFrameMode) {
        if (gatedFrameMode == null) {
            gatedFrameMode = new String[1];
        }
        else if (gatedFrameModeIndex > gatedFrameMode.length - 1) {
            String oldGatedFrameMode[] = new String[gatedFrameMode.length];
            for (i = 0; i < gatedFrameMode.length; i++) {
                oldGatedFrameMode[i] = gatedFrameMode[i];
            }
            gatedFrameMode = new String[gatedFrameModeIndex];
            for (i = 0; i < oldGatedFrameMode.length; i++) {
                gatedFrameMode[i] = oldGatedFrameMode[i];
            }
        }
        gatedFrameMode[gatedFrameModeIndex++] = currentGatedFrameMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentTimeFrame      DOCUMENT ME!
     * @param  currentImageDuration  DOCUMENT ME!
     */
    public void setImageDuration(String currentTimeFrame, String currentImageDuration) {
        tFrame = Integer.valueOf(currentTimeFrame).intValue();
        if (imageDuration == null) {
            imageDuration = new String[1];
        }
        else if ((tFrame-1) > imageDuration.length - 1) {
            String oldImageDuration[] = new String[imageDuration.length];
            for (i = 0; i < imageDuration.length; i++) {
                oldImageDuration[i] = imageDuration[i];
            }
            imageDuration = new String[imageDurationIndex];
            for (i = 0; i < oldImageDuration.length; i++) {
                imageDuration[i] = oldImageDuration[i];
            }
        }
        imageDuration[tFrame - 1] = currentImageDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentImageRelativeStartTime  DOCUMENT ME!
     */
    public void setImageRelativeStartTime(String currentImageRelativeStartTime) {
        imageRelativeStartTime[imageRelativeStartTimeIndex++] = currentImageRelativeStartTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentTimeFrame               DOCUMENT ME!
     * @param  currentImageRelativeStartTime  DOCUMENT ME!
     */
    public void setImageRelativeStartTime(String currentTimeFrame, String currentImageRelativeStartTime) {
        tFrame = Integer.valueOf(currentTimeFrame).intValue();
        imageRelativeStartTime[tFrame - 1] = currentImageRelativeStartTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imagesPerEWindow  DOCUMENT ME!
     */
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
    
    /**
     * DOCUMENT ME!
     *
     * @param  imagesPerEWindow  DOCUMENT ME!
     */
    public void setImagesPerEWindowBrief(String imagesPerEWindow) {
        this.imagesPerEWindow = imagesPerEWindow;
        numberImagesPerEWindow = Integer.valueOf(imagesPerEWindow).intValue();
        numberImages = numberImagesPerEWindow * energyNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sTime  DOCUMENT ME!
     */
    public void setImageStartTime(String sTime) {

        if (imageStartTime == null) {
            imageStartTime = new String[1];
        }

        imageStartTime[imageStartTimeIndex++] = sTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imagingModality  DOCUMENT ME!
     */
    public void setImagingModality(String imagingModality) {
        this.imagingModality = imagingModality;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  indexNestingLevel  DOCUMENT ME!
     */
    public void setIndexNestingLevel(String indexNestingLevel) {
        this.indexNestingLevel = indexNestingLevel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  interfileDataType  DOCUMENT ME!
     */
    public void setInterfileDataType(String interfileDataType) {
        this.interfileDataType = interfileDataType;

        if (interfileDataType.equalsIgnoreCase("TOMOGRAPHIC")) {
            haveTomographic = true;
        }

        if ((interfileDataType.equalsIgnoreCase("STATIC")) || (interfileDataType.equalsIgnoreCase("ROI"))) {
            haveStaticStudy = true;
        }

        if (interfileDataType.equalsIgnoreCase("GATED")) {
            haveGated = true;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  currentIsotopeNumber  DOCUMENT ME!
     * @param  currentIsotopeName    DOCUMENT ME!
     */
    public void setIsotopeName(String currentIsotopeNumber, String currentIsotopeName) {
        iNumber = Integer.valueOf(currentIsotopeNumber).intValue();

        if (isotopeName == null) {
            isotopeName = new String[1];
        }

        isotopeName[iNumber - 1] = currentIsotopeName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  isotopeNumber  DOCUMENT ME!
     */
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

    /**
     * DOCUMENT ME!
     *
     * @param  keysDate  DOCUMENT ME!
     */
    public void setKeysDate(String keysDate) {
        this.keysDate = keysDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  keysVersion  DOCUMENT ME!
     */
    public void setKeysVersion(String keysVersion) {
        this.keysVersion = keysVersion;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  lab  DOCUMENT ME!
     */
    public void setLabel(String lab) {

        if (label == null) {
            label = new String[1];
        }

        label[labelIndex++] = lab;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  matrixSz1  DOCUMENT ME!
     */
    public void setMatrixSize1(String matrixSz1) {

        if (matrixSize1 == null) {
            matrixSize1 = new String[1];
        }

        matrixSize1[matrixSize1Index++] = matrixSz1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  matrixSz2  DOCUMENT ME!
     */
    public void setMatrixSize2(String matrixSz2) {

        if (matrixSize2 == null) {
            matrixSize2 = new String[1];
        }

        matrixSize2[matrixSize2Index++] = matrixSz2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  maximumPixelCt  DOCUMENT ME!
     */
    public void setMaximumPixelCount(String maximumPixelCt) {

        if (maximumPixelCount == null) {
            maximumPixelCount = new String[1];
        }

        maximumPixelCount[maximumPixelCountIndex++] = maximumPixelCt;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mPixelCountInGroup  DOCUMENT ME!
     */
    public void setMaximumPixelCountInGroup(String mPixelCountInGroup) {

        if (maximumPixelCountInGroup == null) {
            maximumPixelCountInGroup = new String[1];
        }

        maximumPixelCountInGroup[maximumPixelCountInGroupIndex++] = mPixelCountInGroup;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nFormat  DOCUMENT ME!
     */
    public void setNumberFormat(String nFormat) {

        if (numberFormat == null) {
            numberFormat = new String[1];
        }

        numberFormat[numberFormatIndex++] = nFormat;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  obliqueReconstruction  DOCUMENT ME!
     */
    public void setObliqueReconstruction(String obliqueReconstruction) {
        this.obliqueReconstruction = obliqueReconstruction;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  observedCardiacCycles  DOCUMENT ME!
     */
    public void setObservedCardiacCycles(String observedCardiacCycles) {
        this.observedCardiacCycles = observedCardiacCycles;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  originalInstitution  DOCUMENT ME!
     */
    public void setOriginalInstitution(String originalInstitution) {
        this.originalInstitution = originalInstitution;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  originatingSystem  DOCUMENT ME!
     */
    public void setOriginatingSystem(String originatingSystem) {
        this.originatingSystem = originatingSystem;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientDOB  DOCUMENT ME!
     */
    public void setPatientDOB(String patientDOB) {
        this.patientDOB = patientDOB;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientID  DOCUMENT ME!
     */
    public void setPatientID(String patientID) {
        this.patientID = patientID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientName  DOCUMENT ME!
     */
    public void setPatientName(String patientName) {
        this.patientName = patientName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientOrientation  DOCUMENT ME!
     */
    public void setPatientOrientation(String patientOrientation) {
        this.patientOrientation = patientOrientation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientRotation  DOCUMENT ME!
     */
    public void setPatientRotation(String patientRotation) {
        this.patientRotation = patientRotation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientSex  DOCUMENT ME!
     */
    public void setPatientSex(String patientSex) {
        this.patientSex = patientSex;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pBetweenFrameGroups  DOCUMENT ME!
     */
    public void setPauseBetweenFrameGroups(String pBetweenFrameGroups) {

        if (pauseBetweenFrameGroups == null) {
            pauseBetweenFrameGroups = new String[1];
        }

        pauseBetweenFrameGroups[pauseBetweenFrameGroupsIndex++] = pBetweenFrameGroups;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pBetweenImages  DOCUMENT ME!
     */
    public void setPauseBetweenImages(String pBetweenImages) {

        if (pauseBetweenImages == null) {
            pauseBetweenImages = new String[1];
        }

        pauseBetweenImages[pauseBetweenImagesIndex++] = pBetweenImages;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  PETDataType  DOCUMENT ME!
     */
    public void setPETDataType(String PETDataType) {
        this.PETDataType = PETDataType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  PETStudyGeneral  DOCUMENT ME!
     */
    public void setPETStudyGeneral(boolean PETStudyGeneral) {
        this.PETStudyGeneral = PETStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  PETStudyImageData  DOCUMENT ME!
     */
    public void setPETStudyImageData(boolean PETStudyImageData) {
        this.PETStudyImageData = PETStudyImageData;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  processStatus  DOCUMENT ME!
     */
    public void setProcessStatus(String processStatus) {
        this.processStatus = processStatus;

        if (processStatus.equalsIgnoreCase("RECONSTRUCTED")) {
            haveReconstructed = true;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  programAuthor  DOCUMENT ME!
     */
    public void setProgramAuthor(String programAuthor) {
        this.programAuthor = programAuthor;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  programDate  DOCUMENT ME!
     */
    public void setProgramDate(String programDate) {
        this.programDate = programDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  programVersion  DOCUMENT ME!
     */
    public void setProgramVersion(String programVersion) {
        this.programVersion = programVersion;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  projectionNumber  DOCUMENT ME!
     */
    public void setProjectionNumber(String projectionNumber) {
        this.projectionNumber = projectionNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  projectionTime  DOCUMENT ME!
     */
    public void setProjectionTime(String projectionTime) {
        this.projectionTime = projectionTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  quantificationUnits  DOCUMENT ME!
     */
    public void setQuantificationUnits(String quantificationUnits) {
        this.quantificationUnits = quantificationUnits;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  radiopharmaceutical  DOCUMENT ME!
     */
    public void setRadiopharmaceutical(String radiopharmaceutical) {
        this.radiopharmaceutical = radiopharmaceutical;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  reconstructionMethod  DOCUMENT ME!
     */
    public void setReconstructionMethod(String reconstructionMethod) {
        this.reconstructionMethod = reconstructionMethod;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  referenceFrameNumber  DOCUMENT ME!
     */
    public void setReferenceFrameNumber(String referenceFrameNumber) {
        this.referenceFrameNumber = referenceFrameNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rotationExtent  DOCUMENT ME!
     */
    public void setRotationExtent(String rotationExtent) {
        this.rotationExtent = rotationExtent;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rC  DOCUMENT ME!
     */
    public void setRRCycles(String rC) {

        if (RRCycles == null) {
            RRCycles = new String[1];
        }

        RRCycles[RRCyclesIndex++] = rC;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RRH  DOCUMENT ME!
     */
    public void setRRHistogram(String RRH) {

        if (RRHistogram == null) {
            RRHistogram = new String[1];
        }

        RRHistogram[RRHistogramIndex++] = RRH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  s1  DOCUMENT ME!
     */
    public void setScalingFactor1(String s1) {

        if (scalingFactor1 == null) {
            scalingFactor1 = new String[1];
        }

        scalingFactor1[scalingFactor1Index++] = s1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  s2  DOCUMENT ME!
     */
    public void setScalingFactor2(String s2) {

        if (scalingFactor2 == null) {
            scalingFactor2 = new String[1];
        }

        scalingFactor2[scalingFactor2Index++] = s2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scannerQuantificationFactor  DOCUMENT ME!
     */
    public void setScannerQuantificationFactor(String scannerQuantificationFactor) {
        this.scannerQuantificationFactor = scannerQuantificationFactor;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scatterCorrected  DOCUMENT ME!
     */
    public void setScatterCorrected(String scatterCorrected) {
        this.scatterCorrected = scatterCorrected;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scatterCorrectionMethod  DOCUMENT ME!
     */
    public void setScatterCorrectionMethod(String scatterCorrectionMethod) {
        this.scatterCorrectionMethod = scatterCorrectionMethod;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sliceNumber  DOCUMENT ME!
     */
    public void setSliceNumber(String sliceNumber) {
        this.sliceNumber = sliceNumber;
    }
    
    public void setSliceOrientation(String sliceOrientation) {
        this.sliceOrientation = sliceOrientation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  spectStudyGeneral  DOCUMENT ME!
     */
    public void setSpectStudyGeneral(boolean spectStudyGeneral) {
        this.spectStudyGeneral = spectStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  spectStudyReconstructedData  DOCUMENT ME!
     */
    public void setSpectStudyReconstructedData(boolean spectStudyReconstructedData) {
        this.spectStudyReconstructedData = spectStudyReconstructedData;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  startHorizontalBedPosition  DOCUMENT ME!
     */
    public void setStartHorizontalBedPosition(String startHorizontalBedPosition) {
        this.startHorizontalBedPosition = startHorizontalBedPosition;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  staticStudyGeneral  DOCUMENT ME!
     */
    public void setStaticStudyGeneral(boolean staticStudyGeneral) {
        this.staticStudyGeneral = staticStudyGeneral;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyDate  DOCUMENT ME!
     */
    public void setStudyDate(String studyDate) {
        this.studyDate = studyDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyDuration  DOCUMENT ME!
     */
    public void setStudyDuration(String studyDuration) {
        this.studyDuration = studyDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyID  DOCUMENT ME!
     */
    public void setStudyID(String studyID) {
        this.studyID = studyID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyTime  DOCUMENT ME!
     */
    public void setStudyTime(String studyTime) {
        this.studyTime = studyTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  timeFrames  DOCUMENT ME!
     */
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

    /**
     * DOCUMENT ME!
     *
     * @param  tWindowImages  DOCUMENT ME!
     */
    public void setTimeWindowImages(String tWindowImages) {

        if (timeWindowImages == null) {
            timeWindowImages = new String[1];
        }

        timeWindowImages[timeWindowImagesIndex++] = tWindowImages;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  lLimit  DOCUMENT ME!
     */
    public void setTimeWindowLowerLimit(String lLimit) {

        if (timeWindowLowerLimit == null) {
            timeWindowLowerLimit = new String[1];
        }

        timeWindowLowerLimit[timeWindowLowerLimitIndex++] = lLimit;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  timeWindows  DOCUMENT ME!
     */
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

    /**
     * DOCUMENT ME!
     *
     * @param  uLimit  DOCUMENT ME!
     */
    public void setTimeWindowUpperLimit(String uLimit) {

        if (timeWindowUpperLimit == null) {
            timeWindowUpperLimit = new String[1];
        }

        timeWindowUpperLimit[timeWindowUpperLimitIndex++] = uLimit;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tCounts  DOCUMENT ME!
     */
    public void setTotalCounts(String tCounts) {

        if (totalCounts == null) {
            totalCounts = new String[1];
        }

        totalCounts[totalCountsIndex++] = tCounts;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  totalImageNumber  DOCUMENT ME!
     */
    public void setTotalImageNumber(String totalImageNumber) {
        this.totalImageNumber = totalImageNumber;
    }
    
    public void setWindowA(String windowA) {
        this.windowA = windowA;
    }
    
    public void setWindowB(String windowB) {
        this.windowB = windowB;
    }
    
    public void setWindowC(String windowC) {
        this.windowC = windowC;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  zAxisFilter  DOCUMENT ME!
     */
    public void setZAxisFilter(String zAxisFilter) {
        this.zAxisFilter = zAxisFilter;
    }
}
