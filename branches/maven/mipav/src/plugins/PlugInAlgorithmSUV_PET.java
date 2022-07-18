import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * This plug in prints out the standardized uptake values for VOIs in 3D PET images. The SUVs are printed out for each
 * VOI slice and for the VOI total. Calculation of SUVbw and SUVbsa are taken from equations in "Standardized uptake
 * values of FDG: body surface area correction is preferable to body weight correction", CK Kim, NC Gupta, B.
 * Chandramouli, and A. Alavi, Journal of Nuclear Medicine, Vol. 35, Issue 1, pp. 164-167, 1994. SUVbw = (PET image
 * pixels) * (weight in grams)/ (injected dose). SUVbsa = (PET image pixels) * (BSA in m2) * (10000cm2/m2)/ (injected
 * dose). (BSA in m2) = [(weight in kg)^0.425 * (height in cm)^0.725 * 0.007184]. Calculation of SUVlbm is taken from
 * Reevaluation of the Standardized Uptake Value for FDG: Variations with Body Weight and Methods for Correction",
 * Yoshifumi Sugawara, Kenneth R. Zasadny, Alex W. Neuhoff, and Richard L. Wahl, Radiology, November, 1999, pp. 521-525.
 * SUVlbm = (PET image pixels) * (LBM in kg) * (1000g/kg) / (injected dose). For males: LBM in kg = 1.10 * (weight in
 * kg) - 120 * [(weight in kg)/(height in cm)]^2. For females: LBM in kg = 1.07 * (weight in kg) - 148 * [(weight in
 * kg)/(height in cm)]^2. PET image pixels and injected dose are decay corrected to the start of scan. PET image pixels
 * are in units of activity/volume, and should be in Becquerels/milliliter for dicom images. Images converted to SUVbw
 * will be displayed in units of g/ml. Images converted to SUVbsa will be displayed in units of cm^2/ml. Images
 * converted to SUVlbm will be displayed in units of g/ml.
 *
 * <p>The radionuclide total dose is administered at the injection time or radiopharmaceutical start time which comes
 * before the acquisition start time. So if the decay correction tag is START which means that the image pixels have all
 * been decay corrected to the acquisition start time, then the radionuclide total dose must be decay corrected from the
 * radiopharmaceutical start time to the acqusition start time using the radionuclide half life. The exponential time
 * constant is equal to (half life)/ln(2). If the decay correction tag is ADMIN, then the image pixels have all been
 * decay corrected to the radiopharmaceutical start time, so the radionuclide total dose need no longer be decay
 * corrected.</p>
 *
 * @version  May 9, 2005
 * @author   William Gandler working according to information provided by Marcelo Mamede
 * @see      AlgorithmBase
 */
public class PlugInAlgorithmSUV_PET extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** 3 types of decay correction. */
    private static final int NONE = 1;

    /** DOCUMENT ME! */
    private static final int START = 2;

    /** DOCUMENT ME! */
    //private static final int ADMIN = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float acqTime;

    /** DOCUMENT ME! */
    private String acqTimeStr;

    /** DOCUMENT ME! */
    private int decayCorrection;

    /** DOCUMENT ME! */
    private long dose;

    /** DOCUMENT ME! */
    private long halfLife;

    /** DOCUMENT ME! */
    private float height;

    /** DOCUMENT ME! */
    private boolean male;

    /** DOCUMENT ME! */
    private String radStartStr;

    /** DOCUMENT ME! */
    private float radStartTime;

    /** DOCUMENT ME! */
    private float weight;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Setup the algorithm.
     *
     * @param  srcImg           Source image model.
     * @param  male             whether the patient is a male
     * @param  height           height
     * @param  weight           weight
     * @param  dose             dosage given
     * @param  decayCorrection  decay correction
     * @param  radStartStr      radiation start string
     * @param  radStartTime     radiation start time
     * @param  acqTimeStr       acquisition time string
     * @param  acqTime          acquisition time
     * @param  halfLife         halflife
     */
    public PlugInAlgorithmSUV_PET(ModelImage srcImg, boolean male, float height, float weight, long dose,
                                  int decayCorrection, String radStartStr, float radStartTime, String acqTimeStr,
                                  float acqTime, long halfLife) {
        super(null, srcImg);
        this.male = male;
        this.height = height;
        this.weight = weight;
        this.dose = dose;
        this.decayCorrection = decayCorrection;
        this.radStartStr = radStartStr;
        this.radStartTime = radStartTime;
        this.acqTimeStr = acqTimeStr;
        this.acqTime = acqTime;
        this.halfLife = halfLife;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            calc3D();
        }
    } // end runAlgorithm()

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("null")
    private void calc2D() {
        ViewUserInterface UI = ViewUserInterface.getReference();
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int i;
        int x, y;
        int offset;
        float sum;
        float value;
        float minimum;
        float mean;
        float maximum;
        float voiNumber;
        FileInfoBase[] fileInfo;
        String patientName;
        String patientID;
        String studyDate;
        String studyTime;
        String studyDescription;
        float weightDivHeight;
        float lbm;
        float lbmMul = 1.0f;
        float bsa;
        float bsaMul;
        float bwMul;
        float SUVbw;
        float SUVbsa;
        float SUVlbm = 1.0f;
        String fileDirectory = srcImage.getFileInfo(0).getFileDirectory();

        // Default names if no dicom information is available
        String fileName = "Data.txt";
        String voiName = "defaultVOIs_s" + File.separator;
        File file;
        RandomAccessFile raFile;
        String dataString = null;
        float[] buffer;
        String unitsStr = null;
        float decaySeconds;

        // Time constant for radionuclide decay = (half life)/ln(2)
        double tau;
        ViewVOIVector VOIs = null;
        int nVOIs;
        fireProgressStateChanged("Measuring SUVs ...");

        fileInfo = srcImage.getFileInfo();

        if (fileInfo[0] instanceof FileInfoDicom) {

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0010,0010") != null) {
                    patientName = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0010,0010"));
                    UI.setDataText("Patient Name = " + patientName + "\n");
                    dataString += "Patient Name = " + patientName + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Patient Name = \n");
                Preferences.debug("Tag (0010,0010) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0010,0020") != null) {
                    patientID = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0010,0020"));
                    UI.setDataText("Patient ID = " + patientID + "\n");
                    dataString += "Patient ID = " + patientID + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Patient ID = \n");
                Preferences.debug("Tag (0010,0020) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,0020") != null) {
                    studyDate = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,0020"));
                    UI.setDataText("Study Date = " + studyDate + "\n");
                    dataString += "Study Date = " + studyDate + "\n";
                    UI.setDataText("Study Date = " + studyDate + "\n");
                    dataString += "Study Date = " + studyDate + "\n";
                    fileName = "Data";
                    voiName = "defaultVOIs_";

                    for (i = 0; i < studyDate.length(); i++) {

                        if ((studyDate.charAt(i) != '/') && (studyDate.charAt(i) != '\0')) {
                            fileName += studyDate.charAt(i);
                            voiName += studyDate.charAt(i);
                        }
                    }
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Date = \n");
                Preferences.debug("Tag (0008,0020) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,0030") != null) {
                    studyTime = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,0030"));
                    UI.setDataText("Study Time = " + studyTime + "\n");
                    dataString += "Study Time = " + studyTime + "\n";

                    for (i = 0; i < studyTime.length(); i++) {

                        if ((studyTime.charAt(i) != ':') && (studyTime.charAt(i) != '.') &&
                                (studyTime.charAt(i) != '\0')) {
                            fileName += studyTime.charAt(i);
                            voiName += studyTime.charAt(i);
                        }
                    }

                    fileName = fileName.trim();
                    voiName = voiName.trim();
                    fileName = fileName + ".txt";
                    voiName = voiName + File.separator;
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Time = \n");
                Preferences.debug("Tag (0008,0030) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,1030") != null) {
                    studyDescription = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,1030"));
                    UI.setDataText("Study Description = " + studyDescription + "\n");
                    dataString += "Study Description = " + studyDescription + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Description = \n");
                Preferences.debug("Tag (0008,1030) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0054,1001") != null) {
                    unitsStr = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0054,1001"));
                    unitsStr = unitsStr.trim();

                    if (unitsStr.equalsIgnoreCase("BQML")) {
                        unitsStr = "Becquerels/millimeter";
                    } else {

                        if (unitsStr.equalsIgnoreCase("CNTS")) {
                            unitsStr = "counts";
                        } else if (unitsStr.equalsIgnoreCase("NONE")) {
                            unitsStr = "unitless";
                        } else if (unitsStr.equalsIgnoreCase("CM2")) {
                            unitsStr = "centimeter**2";
                        } else if (unitsStr.equalsIgnoreCase("PCNT")) {
                            unitsStr = "percent";
                        } else if (unitsStr.equalsIgnoreCase("CPS")) {
                            unitsStr = "counts/second";
                        } else if (unitsStr.equalsIgnoreCase("MGMINML")) {
                            unitsStr = "milligram/minute/millimeter";
                        } else if (unitsStr.equalsIgnoreCase("UMOLMINML")) {
                            unitsStr = "micromole/minute/millimeter";
                        } else if (unitsStr.equalsIgnoreCase("MLMING")) {
                            unitsStr = "milliliter/minute/gram";
                        } else if (unitsStr.equalsIgnoreCase("MLG")) {
                            unitsStr = "milliliter/gram";
                        } else if (unitsStr.equalsIgnoreCase("1CM")) {
                            unitsStr = "1/centimeter";
                        } else if (unitsStr.equalsIgnoreCase("UMOLML")) {
                            unitsStr = "micromole/milliliter";
                        } else if (unitsStr.equalsIgnoreCase("PROPCNTS")) {
                            unitsStr = "proportional to counts";
                        } else if (unitsStr.equalsIgnoreCase("PROPCPS")) {
                            unitsStr = "proportional to counts/sec";
                        } else if (unitsStr.equalsIgnoreCase("MLMINML")) {
                            unitsStr = "milliliter/minute/milliliter";
                        } else if (unitsStr.equalsIgnoreCase("MLML")) {
                            unitsStr = "milliliter/milliliter";
                        } else if (unitsStr.equalsIgnoreCase("GML")) {
                            unitsStr = "grams/milliliter";
                        } else if (unitsStr.equalsIgnoreCase("STDDEV")) {
                            unitsStr = "standard deviations";
                        }

                        displayError("Units = " + unitsStr + " instead of expected Becquerels/milliliter");
                    }

                    UI.setDataText("Pixel value units = " + unitsStr + "\n");
                    dataString += "Pixel value units = " + unitsStr + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Units = \n");
                Preferences.debug("Tag (0054,1001) was not found.\n");
            }
        } // if ( fileInfo[0] instanceof FileInfoDicom )

        if (decayCorrection == NONE) {
            UI.setDataText("No decay correction\n");
            dataString += "No decay correction\n";
        } else if (decayCorrection == START) {
            UI.setDataText("Decay corrected to acquisition start time\n");
            dataString += "Decay corrected to acquisiiton start time\n";
        } else {
            UI.setDataText("Decay corrected to radiopharmaceutical start time\n");
            dataString += "Decay corrected to radiopharmaceutical start time\n";
        }

        UI.setDataText("Radiopharmaceutical Start Time = " + radStartStr + "\n");
        dataString += "Radiopharmaceutical Start Time = " + radStartStr + "\n";
        UI.setDataText("Acqusition Start Time = " + acqTimeStr + "\n");
        dataString += "Acquisition Start Time = " + acqTimeStr + "\n";
        UI.setDataText("Radionuclide half life = " + halfLife + " seconds\n");
        dataString += "Radionuclide half life = " + halfLife + " seconds\n";

        if (male) {
            UI.setDataText("Sex = Male\n");
            dataString += "Sex = Male\n";
        } else {
            UI.setDataText("Sex = Female\n");
            dataString += "Sex = Female\n";
        }

        UI.setDataText("Height = " + height + " meters\n");
        dataString += "Height = " + height + " meters\n";

        // Convert height to centimeters
        height = 100.0f * height;
        UI.setDataText("Weight = " + weight + " kg\n");
        dataString += "Weight = " + weight + " kg\n";
        UI.setDataText("Radionuclide total dose = " + dose + " becquerel\n");
        dataString += "Radionuclide total dose = " + dose + " becquerel\n";

        if (decayCorrection == START) {
            decaySeconds = acqTime - radStartTime;

            // Allow for transition to next day
            if (decaySeconds < 0.0f) {
                decaySeconds = decaySeconds + (24 * 3600);
            }

            tau = halfLife / Math.log(2.0);
            dose = (long) (dose * Math.exp(-decaySeconds / tau));
        } // if (decayCorrection == START)

        weightDivHeight = weight / height;

        if (male) {
            lbm = (1.10f * weight) - (120.0f * weightDivHeight * weightDivHeight);
        } else {
            lbm = (1.07f * weight) - (148.0f * weightDivHeight * weightDivHeight);
        }

        lbmMul = lbm * 1000.0f / dose;
        bsa = (float) (0.007184 * Math.pow(weight, 0.425) * Math.pow(height, 0.725));
        bsaMul = 10000.0f * bsa / dose;
        bwMul = 1000.0f * weight / dose;

        try {
            buffer = new float[sliceSize];
        } catch (OutOfMemoryError err) {
            MipavUtil.displayError("Cannot allocate buffer array");
            buffer = null;

            setCompleted(false);

            return;
        }

        try {
            srcImage.exportData(0, sliceSize, buffer);
        } catch (IOException e) {
            MipavUtil.displayError("Error on srcImage.exportData(0,sliceSize,buffer)");

            setCompleted(false);

            return;
        }

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                sum = 0.0f;
                voiNumber = 0;
                minimum = Float.MAX_VALUE;
                maximum = -Float.MAX_VALUE;
                fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
                fireProgressStateChanged(100 * (i + 1) / nVOIs);
                UI.setDataText("VOI ID = " + VOIs.VOIAt(i).getID() + "\n");
                dataString += "VOI ID = " + VOIs.VOIAt(i).getID() + "\n";
                mask = VOIs.VOIAt(i).createBinaryMask(xDim, yDim, 0);

                for (y = 0; y < yDim; y++) {
                    offset = y * xDim;

                    for (x = 0; x < xDim; x++) {

                        if (mask.get(offset + x)) {
                            voiNumber++;
                            value = buffer[offset + x];
                            sum += value;

                            if (value < minimum) {
                                minimum = value;
                            }

                            if (value > maximum) {
                                maximum = value;
                            }
                        }
                    } // for (x = 0; x < xDim; x++)
                } // for (y = 0; y < yDim; y+

                mean = sum / voiNumber;
                SUVbw = minimum * bwMul;
                UI.setDataText(" Minimum SUVbw = " + SUVbw + " g/ml\t");
                dataString += " Minimum SUVbw = " + SUVbw + " g/ml\t";
                SUVbsa = minimum * bsaMul;
                UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                SUVlbm = minimum * lbmMul;
                UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                SUVbw = mean * bwMul;
                UI.setDataText(" Mean SUVbw = " + SUVbw + " g/ml\t");
                dataString += " Mean SUVbw = " + SUVbw + " g/ml\t";
                SUVbsa = mean * bsaMul;
                UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                SUVlbm = mean * lbmMul;
                UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                SUVbw = maximum * bwMul;
                UI.setDataText(" Maximum SUVbw = " + SUVbw + " g/ml\t");
                dataString += " Maximum SUVbw = " + SUVbw + " g/ml\t";
                SUVbsa = maximum * bsaMul;
                UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                SUVlbm = maximum * lbmMul;
                UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                SUVbw = sum * bwMul;
                UI.setDataText(" Sum SUVbw = " + SUVbw + " g/ml\t");
                dataString += " Sum SUVbw = " + SUVbw + " g/ml\t";
                SUVbsa = sum * bsaMul;
                UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                SUVlbm = sum * lbmMul;
                UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        if (threadStopped) {
            finalize();

            return;
        }

        file = new File(fileDirectory + fileName);

        try {
            raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            raFile.write(dataString.getBytes());
            raFile.close();
        } catch (FileNotFoundException e) {
            MipavUtil.displayError("FileNotFoundException " + e);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e);
        }

        ViewUserInterface.getReference().getFrameContainingImage(srcImage).saveAllVOIsTo(fileDirectory +
                                                                                         File.separator + voiName);
        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("null")
    private void calc3D() {
        int i;
        int x, y, z;
        int offset;
        float[] buffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int zOffset;
        int volSize = sliceSize * zDim;
        FileInfoBase[] fileInfo;
        String patientName;
        String patientID;
        String studyDate;
        String studyTime;
        String studyDescription;
        float weightDivHeight;
        float bsa;
        float bsaMul;
        float bwMul;
        float lbm;
        float lbmMul = 1.0f;
        String fileDirectory = srcImage.getFileInfo(0).getFileDirectory();

        // Default names if no dicom information is available
        String fileName = "Data.txt";
        String voiName = "defaultVOIs_s" + File.separator;
        File file;
        RandomAccessFile raFile;
        String dataString = null;
        float sliceSum;
        float totalSum;
        int voiSliceNumber;
        int voiTotalNumber;
        float sliceMinimum;
        float totalMinimum;
        float sliceMean;
        float totalMean;
        float sliceMaximum;
        float totalMaximum;
        float SUVbw;
        float SUVbsa;
        float SUVlbm;
        float value;
        String unitsStr = null;
        float decaySeconds;

        // Time constant for radionuclide decay = (half life)/ln(2)
        double tau;
        ViewVOIVector VOIs = null;
        int nVOIs;
        Vector<VOIBase>[] contours;
        int nContours;
        BitSet mask;
        ViewUserInterface UI = ViewUserInterface.getReference();
        fireProgressStateChanged("Measuring SUVs ...");

        fileInfo = srcImage.getFileInfo();

        if (fileInfo[0] instanceof FileInfoDicom) {

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0010,0010") != null) {
                    patientName = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0010,0010"));
                    UI.setDataText("Patient Name = " + patientName + "\n");
                    dataString += "Patient Name = " + patientName + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Patient Name = \n");
                Preferences.debug("Tag (0010,0010) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0010,0020") != null) {
                    patientID = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0010,0020"));
                    UI.setDataText("Patient ID = " + patientID + "\n");
                    dataString += "Patient ID = " + patientID + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Patient ID = \n");
                Preferences.debug("Tag (0010,0020) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,0020") != null) {
                    studyDate = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,0020"));
                    UI.setDataText("Study Date = " + studyDate + "\n");
                    dataString += "Study Date = " + studyDate + "\n";
                    fileName = "Data";
                    voiName = "defaultVOIs_";

                    for (i = 0; i < studyDate.length(); i++) {

                        if ((studyDate.charAt(i) != '/') && (studyDate.charAt(i) != '\0')) {
                            fileName += studyDate.charAt(i);
                            voiName += studyDate.charAt(i);
                        }
                    }
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Date = \n");
                Preferences.debug("Tag (0008,0020) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,0030") != null) {
                    studyTime = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,0030"));
                    UI.setDataText("Study Time = " + studyTime + "\n");
                    dataString += "Study Time = " + studyTime + "\n";
                    fileName = fileName + "_";
                    voiName = voiName + "_";

                    for (i = 0; i < studyTime.length(); i++) {

                        if ((studyTime.charAt(i) != ':') && (studyTime.charAt(i) != '.') &&
                                (studyTime.charAt(i) != '\0')) {
                            fileName += studyTime.charAt(i);
                            voiName += studyTime.charAt(i);
                        }
                    }

                    voiName = voiName.trim();
                    fileName = fileName.trim();
                    fileName = fileName + ".txt";
                    voiName = voiName + File.separator;
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Time = \n");
                Preferences.debug("Tag (0008,0030) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0008,1030") != null) {
                    studyDescription = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0008,1030"));
                    UI.setDataText("Study Description = " + studyDescription + "\n");
                    dataString += "Study Description = " + studyDescription + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Study Description = \n");
                Preferences.debug("Tag (0008,1030) was not found.\n");
            }

            try {

                if (((FileInfoDicom) (fileInfo[0])).getTagTable().getValue("0054,1001") != null) {
                    unitsStr = (String) (((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0054,1001"));
                    unitsStr = unitsStr.trim();

                    if (unitsStr.equalsIgnoreCase("BQML")) {
                        unitsStr = "Becquerels/millimeter";
                    } else {

                        if (unitsStr.equalsIgnoreCase("CNTS")) {
                            unitsStr = "counts";
                        } else if (unitsStr.equalsIgnoreCase("NONE")) {
                            unitsStr = "unitless";
                        } else if (unitsStr.equalsIgnoreCase("CM2")) {
                            unitsStr = "centimeter**2";
                        } else if (unitsStr.equalsIgnoreCase("PCNT")) {
                            unitsStr = "percent";
                        } else if (unitsStr.equalsIgnoreCase("CPS")) {
                            unitsStr = "counts/second";
                        } else if (unitsStr.equalsIgnoreCase("MGMINML")) {
                            unitsStr = "milligram/minute/millimeter";
                        } else if (unitsStr.equalsIgnoreCase("UMOLMINML")) {
                            unitsStr = "micromole/minute/millimeter";
                        } else if (unitsStr.equalsIgnoreCase("MLMING")) {
                            unitsStr = "milliliter/minute/gram";
                        } else if (unitsStr.equalsIgnoreCase("MLG")) {
                            unitsStr = "milliliter/gram";
                        } else if (unitsStr.equalsIgnoreCase("1CM")) {
                            unitsStr = "1/centimeter";
                        } else if (unitsStr.equalsIgnoreCase("UMOLML")) {
                            unitsStr = "micromole/milliliter";
                        } else if (unitsStr.equalsIgnoreCase("PROPCNTS")) {
                            unitsStr = "proportional to counts";
                        } else if (unitsStr.equalsIgnoreCase("PROPCPS")) {
                            unitsStr = "proportional to counts/sec";
                        } else if (unitsStr.equalsIgnoreCase("MLMINML")) {
                            unitsStr = "milliliter/minute/milliliter";
                        } else if (unitsStr.equalsIgnoreCase("MLML")) {
                            unitsStr = "milliliter/milliliter";
                        } else if (unitsStr.equalsIgnoreCase("GML")) {
                            unitsStr = "grams/milliliter";
                        } else if (unitsStr.equalsIgnoreCase("STDDEV")) {
                            unitsStr = "standard deviations";
                        }

                        displayError("Units = " + unitsStr + " instead of expected Becquerels/milliliter");
                    }

                    UI.setDataText("Pixel value units = " + unitsStr + "\n");
                    dataString += "Pixel value units = " + unitsStr + "\n";
                }
            } catch (NullPointerException noTag) {
                UI.setDataText("Units = \n");
                Preferences.debug("Tag (0054,1001) was not found.\n");
            }
        } // if ( fileInfo[0] instanceof FileInfoDicom )

        if (decayCorrection == NONE) {
            UI.setDataText("No decay correction\n");
            dataString += "No decay correction\n";
        } else if (decayCorrection == START) {
            UI.setDataText("Decay corrected to acquisition start time\n");
            dataString += "Decay corrected to acquisiiton start time\n";
        } else {
            UI.setDataText("Decay corrected to radiopharmaceutical start time\n");
            dataString += "Decay corrected to radiopharmaceutical start time\n";
        }

        UI.setDataText("Radiopharmaceutical Start Time = " + radStartStr + "\n");
        dataString += "Radiopharmaceutical Start Time = " + radStartStr + "\n";
        UI.setDataText("Acqusition Start Time = " + acqTimeStr + "\n");
        dataString += "Acquisition Start Time = " + acqTimeStr + "\n";
        UI.setDataText("Radionuclide half life = " + halfLife + " seconds\n");
        dataString += "Radionuclide half life = " + halfLife + " seconds\n";

        if (male) {
            UI.setDataText("Sex = Male\n");
            dataString += "Sex = Male\n";
        } else {
            UI.setDataText("Sex = Female\n");
            dataString += "Sex = Female\n";
        }

        UI.setDataText("Height = " + height + " meters\n");
        dataString += "Height = " + height + " meters\n";

        // Convert height to centimeters
        height = 100.0f * height;
        UI.setDataText("Weight = " + weight + " kg\n");
        dataString += "Weight = " + weight + " kg\n";
        UI.setDataText("Radionuclide total dose = " + dose + " becquerel\n");
        dataString += "Radionuclide total dose = " + dose + " becquerel\n";

        if (decayCorrection == START) {
            decaySeconds = acqTime - radStartTime;

            // Allow for transition to next day
            if (decaySeconds < 0.0f) {
                decaySeconds = decaySeconds + (24 * 3600);
            }

            tau = halfLife / Math.log(2.0);
            dose = (long) (dose * Math.exp(-decaySeconds / tau));
        } // if (decayCorrection == START)

        weightDivHeight = weight / height;

        if (male) {
            lbm = (1.10f * weight) - (120.0f * weightDivHeight * weightDivHeight);
        } else {
            lbm = (1.07f * weight) - (148.0f * weightDivHeight * weightDivHeight);
        }

        lbmMul = lbm * 1000.0f / dose;
        bsa = (float) (0.007184 * Math.pow(weight, 0.425) * Math.pow(height, 0.725));
        bsaMul = 10000.0f * bsa / dose;
        bwMul = 1000.0f * weight / dose;

        try {
            buffer = new float[volSize];
        } catch (OutOfMemoryError err) {
            MipavUtil.displayError("Cannot allocate buffer array");
            buffer = null;

            setCompleted(false);

            return;
        }

        try {
            srcImage.exportData(0, volSize, buffer);
        } catch (IOException e) {
            MipavUtil.displayError("Error on srcImage.exportData(0,volSize,buffer)");

            setCompleted(false);

            return;
        }

        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                voiTotalNumber = 0;
                totalMinimum = Float.MAX_VALUE;
                totalMaximum = -Float.MAX_VALUE;
                totalSum = 0.0f;
                fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);
                fireProgressStateChanged(100 * (i + 1) / nVOIs);
                UI.setDataText("VOI ID = " + VOIs.VOIAt(i).getID() + "\n");
                dataString += "VOI ID = " + VOIs.VOIAt(i).getID() + "\n";
                contours = VOIs.VOIAt(i).getSortedCurves(zDim);

                for (z = 0; z < zDim; z++) {
                    voiSliceNumber = 0;
                    sliceMinimum = Float.MAX_VALUE;
                    sliceMaximum = -Float.MAX_VALUE;
                    sliceSum = 0.0f;
                    zOffset = z * sliceSize;
                    nContours = contours[z].size();

                    if (nContours >= 1) {
                        mask = VOIs.VOIAt(i).createBinaryMask(xDim, yDim, z);

                        for (y = 0; y < yDim; y++) {
                            offset = y * xDim;

                            for (x = 0; x < xDim; x++) {

                                if (mask.get(offset + x)) {
                                    voiSliceNumber++;
                                    value = buffer[zOffset + offset + x];
                                    sliceSum += value;

                                    if (value < sliceMinimum) {
                                        sliceMinimum = value;
                                    }

                                    if (value > sliceMaximum) {
                                        sliceMaximum = value;
                                    }
                                }
                            } // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)

                        sliceMean = sliceSum / voiSliceNumber;
                        voiTotalNumber += voiSliceNumber;
                        totalSum += sliceSum;

                        if (sliceMinimum < totalMinimum) {
                            totalMinimum = sliceMinimum;
                        }

                        if (sliceMaximum > totalMaximum) {
                            totalMaximum = sliceMaximum;
                        }

                        UI.setDataText(" Slice = " + (z + 1) + " minimums\t");
                        dataString += " Slice = " + (z + 1) + " minimums\t";
                        SUVbw = sliceMinimum * bwMul;
                        UI.setDataText(" SUVbw = " + SUVbw + " g/ml\t");
                        dataString += " SUVbw = " + SUVbw + " g/ml\t";
                        SUVbsa = sliceMinimum * bsaMul;
                        UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                        dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                        SUVlbm = sliceMinimum * lbmMul;
                        UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                        dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                        UI.setDataText(" Slice = " + (z + 1) + " means\t");
                        dataString += " Slice = " + (z + 1) + " means\t";
                        SUVbw = sliceMean * bwMul;
                        UI.setDataText(" SUVbw = " + SUVbw + " g/ml\t");
                        dataString += " SUVbw = " + SUVbw + " g/ml\t";
                        SUVbsa = sliceMean * bsaMul;
                        UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                        dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                        SUVlbm = sliceMean * lbmMul;
                        UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                        dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                        UI.setDataText(" Slice = " + (z + 1) + " maximums\t");
                        dataString += " Slice = " + (z + 1) + " maximums\t";
                        SUVbw = sliceMaximum * bwMul;
                        UI.setDataText(" SUVbw = " + SUVbw + " g/ml\t");
                        dataString += " SUVbw = " + SUVbw + " g/ml\t";
                        SUVbsa = sliceMaximum * bsaMul;
                        UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                        dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                        SUVlbm = sliceMaximum * lbmMul;
                        UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                        dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                        UI.setDataText(" Slice = " + (z + 1) + " Sums\t");
                        dataString += " Slice = " + (z + 1) + " Sums\t";
                        SUVbw = sliceSum * bwMul;
                        UI.setDataText(" SUVbw = " + SUVbw + " g/ml\t");
                        dataString += " SUVbw = " + SUVbw + " g/ml\t";
                        SUVbsa = sliceSum * bsaMul;
                        UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                        dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                        SUVlbm = sliceSum * lbmMul;
                        UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                        dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                    } // if (nContours >= 1)
                } // for (z = 0; z < nSlices; z++)

                totalMean = totalSum / voiTotalNumber;
                UI.setDataText("Total Minimums " + "\t");
                dataString += "Total Minimums " + "\t";
                SUVbw = totalMinimum * bwMul;
                UI.setDataText(" SUVbw = " + SUVbw + " g/ml\t");
                dataString += " SUVbw = " + SUVbw + " g/ml\t";
                SUVbsa = totalMinimum * bsaMul;
                UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                SUVlbm = totalMinimum * lbmMul;
                UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                UI.setDataText("Total Means " + "\t\t");
                dataString += "Total Means " + "\t\t";
                SUVbw = totalMean * bwMul;
                UI.setDataText(" SUVbw = " + SUVbw + " g/ml\t");
                dataString += " SUVbw = " + SUVbw + " g/ml\t";
                SUVbsa = totalMean * bsaMul;
                UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                SUVlbm = totalMean * lbmMul;
                UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                UI.setDataText("Total Maximums " + "\t");
                dataString += "Total Maximums " + "\t";
                SUVbw = totalMaximum * bwMul;
                UI.setDataText(" SUVbw = " + SUVbw + " g/ml\t");
                dataString += " SUVbw = " + SUVbw + " g/ml\t";
                SUVbsa = totalMaximum * bsaMul;
                UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                SUVlbm = totalMaximum * lbmMul;
                UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n");
                dataString += " SUVlbm = " + SUVlbm + " g/ml\n";
                UI.setDataText("Total Sums " + "\t\t");
                dataString += "Total Sums " + "\t\t";
                SUVbw = totalSum * bwMul;
                UI.setDataText(" SUVbw = " + SUVbw + " g/ml\t");
                dataString += " SUVbw = " + SUVbw + " g/ml\t";
                SUVbsa = totalSum * bsaMul;
                UI.setDataText(" SUVbsa = " + SUVbsa + " cm^2/ml\t");
                dataString += " SUVbsa = " + SUVbsa + " cm^2/ml\t";
                SUVlbm = totalSum * lbmMul;
                UI.setDataText(" SUVlbm = " + SUVlbm + " g/ml\n\n");
                dataString += " SUVlbm = " + SUVlbm + " g/ml\n\n";
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        if (threadStopped) {
            finalize();

            return;
        }

        file = new File(fileDirectory + fileName);

        try {
            raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            raFile.write(dataString.getBytes());
            raFile.close();
        } catch (FileNotFoundException e) {
            MipavUtil.displayError("FileNotFoundException " + e);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e);
        }

        ViewUserInterface.getReference().getFrameContainingImage(srcImage).saveAllVOIsTo(fileDirectory +
                                                                                         File.separator + voiName);
        setCompleted(true);
    }
}
