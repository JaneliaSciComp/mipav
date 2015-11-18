package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a Siemens MagnetomVision image is stored on disk.
 */

public class FileInfoMagnetomVision extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7248419259071769004L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int AcquisitionDateDD;

    /** DOCUMENT ME! */
    private int AcquisitionDateMM;

    /** DOCUMENT ME! */
    private int AcquisitionDateYYYY;

    /** DOCUMENT ME! */
    private int AcquisitionTimeHH;

    /** DOCUMENT ME! */
    private int AcquisitionTimeMM;

    /** DOCUMENT ME! */
    private int AcquisitionTimeSS;

    /** DOCUMENT ME! */
    private String Annotation = null;

    /** DOCUMENT ME! */
    private int CalibrationDateDD;

    /** DOCUMENT ME! */
    private int CalibrationDateMM;

    /** DOCUMENT ME! */
    private int CalibrationDateYYYY;

    /** DOCUMENT ME! */
    private int CalibrationTimeHH;

    /** DOCUMENT ME! */
    private int CalibrationTimeMM;

    /** DOCUMENT ME! */
    private int CalibrationTimeSS;

    /** DOCUMENT ME! */
    private double CenterPointX;

    /** DOCUMENT ME! */
    private double CenterPointY;

    /** DOCUMENT ME! */
    private double CenterPointZ;

    /** DOCUMENT ME! */
    private double ColumnVectorX;

    /** DOCUMENT ME! */
    private double ColumnVectorY;

    /** DOCUMENT ME! */
    private double ColumnVectorZ;

    /** DOCUMENT ME! */
    private int DisplayMatrixSize;

    /** DOCUMENT ME! */
    private double DistanceFromIsocenter;

    /** DOCUMENT ME! */
    private int DOBDD;

    /** DOCUMENT ME! */
    private int DOBMM;

    /** DOCUMENT ME! */
    private int DOBYYYY;

    /** DOCUMENT ME! */
    private double EchoTime;

    /** DOCUMENT ME! */
    private double FlipAngle;

    /** DOCUMENT ME! */
    private double FOVColumn;

    /** DOCUMENT ME! */
    private double FOVRow;

    /** DOCUMENT ME! */
    private double FrequencyMHz;

    /** DOCUMENT ME! */
    private int ImageDateDD;

    /** DOCUMENT ME! */
    private int ImageDateMM;

    /** DOCUMENT ME! */
    private int ImageDateYYYY;

    /** DOCUMENT ME! */
    private String ImagedNucleus = null;

    /** DOCUMENT ME! */
    private int ImageTimeHH;

    /** DOCUMENT ME! */
    private int ImageTimeMM;

    /** DOCUMENT ME! */
    private int ImageTimeSS;

    /** DOCUMENT ME! */
    private String InstitutionName = null;

    /** DOCUMENT ME! */
    private int LastMoveDateDD;

    /** DOCUMENT ME! */
    private int LastMoveDateMM;

    /** DOCUMENT ME! */
    private int LastMoveDateYYYY;

    /** DOCUMENT ME! */
    private int LastMoveTimeHH;

    /** DOCUMENT ME! */
    private int LastMoveTimeMM;

    /** DOCUMENT ME! */
    private int LastMoveTimeSS;

    /** DOCUMENT ME! */
    private double MagneticFieldStrength;

    /** DOCUMENT ME! */
    private String Manufacturer = null; // should be SIEMENS

    /** DOCUMENT ME! */
    private String ModelName = null; // should be MAGNETOM VISION

    /** DOCUMENT ME! */
    private double NormalVectorX;

    /** DOCUMENT ME! */
    private double NormalVectorY;

    /** DOCUMENT ME! */
    private double NormalVectorZ;

    /** DOCUMENT ME! */
    private String OrientationSet1Back = null;

    /** DOCUMENT ME! */
    private String OrientationSet1Left = null;

    /** DOCUMENT ME! */
    private String OrientationSet1Top = null;

    /** DOCUMENT ME! */
    private String OrientationSet2Down = null;

    /** DOCUMENT ME! */
    private String OrientationSet2Front = null;

    /** DOCUMENT ME! */
    private String OrientationSet2Right = null;

    /** DOCUMENT ME! */
    private String PatientAge = null;

    /** DOCUMENT ME! */
    private String PatientAgeUnits = null; // ('Y' =years)

    /** DOCUMENT ME! */
    private String PatientID = null;

    /** DOCUMENT ME! */
    private String PatientName = null;

    /** DOCUMENT ME! */
    private String ReceivingCoil = null;

    /** DOCUMENT ME! */
    private int RegistrationDateDD;

    /** DOCUMENT ME! */
    private int RegistrationDateMM;

    /** DOCUMENT ME! */
    private int RegistrationDateYYYY;

    /** DOCUMENT ME! */
    private int RegistrationTimeHH;

    /** DOCUMENT ME! */
    private int RegistrationTimeMM;

    /** DOCUMENT ME! */
    private int RegistrationTimeSS;

    /** DOCUMENT ME! */
    private double RepetitionTime;

    /** DOCUMENT ME! */
    private double RowVectorX;

    /** DOCUMENT ME! */
    private double RowVectorY;

    /** DOCUMENT ME! */
    private double RowVectorZ;

    /** DOCUMENT ME! */
    private String SequenceAuthor = null;

    /** DOCUMENT ME! */
    private String SequenceName = null;

    /** DOCUMENT ME! */
    private String SequencePrgName = null;

    /** DOCUMENT ME! */
    private String SequenceType = null;

    /** DOCUMENT ME! */
    private String SequenceWkcName = null;

    /** DOCUMENT ME! */
    private int SiemensStudyDateDD;

    /** DOCUMENT ME! */
    private int SiemensStudyDateMM;

    /** DOCUMENT ME! */
    private int SiemensStudyDateYYYY;

    /** DOCUMENT ME! */
    private int SiemensStudyTimeHH;

    /** DOCUMENT ME! */
    private int SiemensStudyTimeMM;

    /** DOCUMENT ME! */
    private int SiemensStudyTimeSS;

    /** DOCUMENT ME! */
    private String Station = null;

    /** DOCUMENT ME! */
    private String TextAcquisitionMatrixFreq = null;

    /** DOCUMENT ME! */
    private String TextAcquisitionMatrixFreqO = null; // ('o' = o, ' ' = blank)

    /** DOCUMENT ME! */
    private String TextAcquisitionMatrixFreqS = null; // ('s' = s, ' ' = blank)

    /** DOCUMENT ME! */
    private String TextAcquisitionMatrixPhase = null;

    /** DOCUMENT ME! */
    private String TextAcquisitionMatrixPhaseAxis = null; // ('h' = horizotnal, 'v' = vertical)

    /** DOCUMENT ME! */
    private String TextAcquisitionTimeMM = null;

    /** DOCUMENT ME! */
    private String TextAcquisitionTimeSS = null;

    /** DOCUMENT ME! */
    private String TextAngle = null;

    /** DOCUMENT ME! */
    private String TextAngleFlag1 = null;

    /** DOCUMENT ME! */
    private String TextAngleFlag2 = null;

    /** DOCUMENT ME! */
    private String TextAngleFlag3 = null;

    /** DOCUMENT ME! */
    private String TextAnnotation = null;

    /** DOCUMENT ME! */
    private String TextDateDD = null;

    /** DOCUMENT ME! */
    private String TextDateMM = null;

    /** DOCUMENT ME! */
    private String TextDateYYYY = null;

    /** DOCUMENT ME! */
    private String TextDOBDD = null;

    /** DOCUMENT ME! */
    private String TextDOBMM = null;

    /** DOCUMENT ME! */
    private String TextDOBYYYY = null;

    /** DOCUMENT ME! */
    private String TextEchoNumber = null;

    /** DOCUMENT ME! */
    private String TextEchoTime = null;

    /** DOCUMENT ME! */
    private String TextFlipAngle = null;

    /** DOCUMENT ME! */
    private String TextFOVH = null;

    /** DOCUMENT ME! */
    private String TextFOVV = null;

    /** DOCUMENT ME! */
    private String TextImageNumber = null;

    /** DOCUMENT ME! */
    private String TextImageNumber2 = null;

    /** DOCUMENT ME! */
    private String TextModelName = null;

    /** DOCUMENT ME! */
    private String TextOrganization = null;

    /** DOCUMENT ME! */
    private String TextPatientAge = null;

    /** DOCUMENT ME! */
    private String TextPatientAgeUnits = null; // ('Y' = years)

    /** DOCUMENT ME! */
    private String TextPatientID = null;

    /** DOCUMENT ME! */
    private String TextPatientName = null;

    /** DOCUMENT ME! */
    private String TextPatientPosition = null;

    /** DOCUMENT ME! */
    private String TextPatientSex = null;

    /** DOCUMENT ME! */
    private String TextRepetitionTime = null;

    /** DOCUMENT ME! */
    private String TextScanNumberA = null;

    /** DOCUMENT ME! */
    private String TextScanNumberB = null;

    /** DOCUMENT ME! */
    private String TextScanStartTimeHH = null;

    /** DOCUMENT ME! */
    private String TextScanStartTimeMM = null;

    /** DOCUMENT ME! */
    private String TextScanStartTimeSS = null;

    /** DOCUMENT ME! */
    private String TextSequence = null;

    /** DOCUMENT ME! */
    private String TextSlicePosition = null;

    /** DOCUMENT ME! */
    private String TextSliceThickness = null;

    /** DOCUMENT ME! */
    private String TextStation = null;

    /** DOCUMENT ME! */
    private String TextStudyImageNumber3 = null;

    /** DOCUMENT ME! */
    private String TextStudyNumber = null;

    /** DOCUMENT ME! */
    private String TextStudyNumber2 = null;

    /** DOCUMENT ME! */
    private String TextTablePosition = null;

    /** DOCUMENT ME! */
    private String TextTimeHH = null;

    /** DOCUMENT ME! */
    private String TextTimeMM = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoMagnetomVision - file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoMagnetomVision(String name, String directory, int format) {
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

        dialog.append("Siemens Study Date Year: \t" + SiemensStudyDateYYYY + "\n");
        dialog.append("Siemens Study Date Month: \t" + SiemensStudyDateMM + "\n");
        dialog.append("Siemens Study Date Day: \t" + SiemensStudyDateDD + "\n");
        dialog.append("Siemens Study Time Hour: \t" + SiemensStudyTimeHH + "\n");
        dialog.append("Siemens Study Time Minute: \t" + SiemensStudyTimeMM + "\n");
        dialog.append("Siemens Study Time Second: \t" + SiemensStudyTimeSS + "\n");
        dialog.append("Acquisition Date Year: \t\t" + AcquisitionDateYYYY + "\n");
        dialog.append("Acquisition Date Month:\t\t" + AcquisitionDateMM + "\n");
        dialog.append("Acquisition Date Day: \t\t" + AcquisitionDateDD + "\n");
        dialog.append("Acquisition Time Hour: \t\t" + AcquisitionTimeHH + "\n");
        dialog.append("Acquisition Time Minute: \t" + AcquisitionTimeMM + "\n");
        dialog.append("Acquisition Time Second: \t" + AcquisitionTimeSS + "\n");
        dialog.append("Image Date Year: \t\t" + ImageDateYYYY + "\n");
        dialog.append("Image Date Month:\t\t" + ImageDateMM + "\n");
        dialog.append("Image Date Day: \t\t" + ImageDateDD + "\n");
        dialog.append("Image Time Hour: \t\t" + ImageTimeHH + "\n");
        dialog.append("Image Time Minute: \t\t" + ImageTimeMM + "\n");
        dialog.append("Image Time Second: \t\t" + ImageTimeSS + "\n");

        if (Manufacturer != null) {
            dialog.append("Manufacturer: \t\t" + Manufacturer + "\n");
        }

        if (InstitutionName != null) {
            dialog.append("Institution Name: \t\t" + InstitutionName + "\n");
        }

        if (Annotation != null) {
            dialog.append("Annotation: \t\t" + Annotation + "\n");
        }

        if (ModelName != null) {
            dialog.append("Model Name: \t\t" + ModelName + "\n");
        }

        dialog.append("Last Move Date Year: \t" + LastMoveDateYYYY + "\n");
        dialog.append("Last Move Date Month: \t" + LastMoveDateMM + "\n");
        dialog.append("Last Move Date Day: \t" + LastMoveDateDD + "\n");
        dialog.append("Last Move Time Hour: \t" + LastMoveTimeHH + "\n");
        dialog.append("Last Move Time Minute: \t" + LastMoveTimeMM + "\n");
        dialog.append("Last Move Time Second: \t" + LastMoveTimeSS + "\n");

        if (PatientName != null) {
            dialog.append("Patient Name: " + PatientName + "\n");
        }

        if (PatientID != null) {
            dialog.append("Patient ID: " + PatientID + "\n");
        }

        dialog.append("Date of Birth Year: " + DOBYYYY + "\n");
        dialog.append("Date of Birth Month: " + DOBMM + "\n");
        dialog.append("Date of Birth Day: " + DOBDD + "\n");

        if (PatientAge != null) {
            dialog.append("Patient Age: " + PatientAge + "\n");
        }

        if (PatientAgeUnits != null) {
            dialog.append("Patient Age Units: " + PatientAgeUnits + "\n");
        }

        dialog.append("Registration Date Year: " + RegistrationDateYYYY + "\n");
        dialog.append("Registration Date Month: " + RegistrationDateMM + "\n");
        dialog.append("Registration Date Day: " + RegistrationDateDD + "\n");
        dialog.append("Registration Time Hour: " + RegistrationTimeHH + "\n");
        dialog.append("Registration Time Minute: " + RegistrationTimeMM + "\n");
        dialog.append("Registration Time Second: " + RegistrationTimeSS + "\n");
        dialog.append("Slice Thickness: " + getSliceThickness() + "\n");
        dialog.append("Repetition Time: " + RepetitionTime + "\n");
        dialog.append("Echo Time: " + EchoTime + "\n");
        dialog.append("Frequency MHz: " + FrequencyMHz + "\n");

        if (Station != null) {
            dialog.append("Station = " + Station + "\n");
        }

        dialog.append("Calibration Date Year: " + CalibrationDateYYYY + "\n");
        dialog.append("Calibration Date Month: " + CalibrationDateMM + "\n");
        dialog.append("Calibration Date Day: " + CalibrationDateDD + "\n");
        dialog.append("Calibration Time Hour: " + CalibrationTimeHH + "\n");
        dialog.append("Calibration Time Minute: " + CalibrationTimeMM + "\n");
        dialog.append("Calibration Time Second: " + CalibrationTimeSS + "\n");

        if (ReceivingCoil != null) {
            dialog.append("Receiving Coil: " + ReceivingCoil + "\n");
        }

        if (ImagedNucleus != null) {
            dialog.append("Imaged Nucleus: " + ImagedNucleus + "\n");
        }

        dialog.append("Flip Angle: " + FlipAngle + "\n");
        dialog.append("Magnetic Field Strength: " + MagneticFieldStrength + "\n");
        dialog.append("Display Matrix Size: " + DisplayMatrixSize + "\n");

        if (SequencePrgName != null) {
            dialog.append("SequencePrgName: " + SequencePrgName + "\n");
        }

        if (SequenceWkcName != null) {
            dialog.append("SequenceWkcName: " + SequenceWkcName + "\n");
        }

        if (SequenceAuthor != null) {
            dialog.append("Sequence Author: " + SequenceAuthor + "\n");
        }

        if (SequenceType != null) {
            dialog.append("Sequence Type: " + SequenceType + "\n");
        }

        dialog.append("FOV Row: " + FOVRow + "\n");
        dialog.append("FOV Column: " + FOVColumn + "\n");
        dialog.append("Center Point X: " + CenterPointX + "\n");
        dialog.append("Center Point Y: " + CenterPointY + "\n");
        dialog.append("Center Point Z: " + CenterPointZ + "\n");
        dialog.append("Normal Vector X: " + NormalVectorX + "\n");
        dialog.append("Normal Vector Y: " + NormalVectorY + "\n");
        dialog.append("Normal Vector Z: " + NormalVectorZ + "\n");
        dialog.append("Distance From Isocenter: " + DistanceFromIsocenter + "\n");
        dialog.append("Row Vector X: " + RowVectorX + "\n");
        dialog.append("Row Vector Y: " + RowVectorY + "\n");
        dialog.append("Row Vector Z: " + RowVectorZ + "\n");
        dialog.append("Column Vector X: " + ColumnVectorX + "\n");
        dialog.append("Column Vector Y: " + ColumnVectorY + "\n");
        dialog.append("Column Vector Z: " + ColumnVectorZ + "\n");

        if (OrientationSet1Top != null) {
            dialog.append("OrientationSet1Top: " + OrientationSet1Top + "\n");
        }

        if (OrientationSet1Left != null) {
            dialog.append("OrientationSet1Left: " + OrientationSet1Left + "\n");
        }

        if (OrientationSet1Back != null) {
            dialog.append("OrientationSet1Back: " + OrientationSet1Back + "\n");
        }

        if (OrientationSet2Down != null) {
            dialog.append("OrientationSet2Down: " + OrientationSet2Down + "\n");
        }

        if (OrientationSet2Right != null) {
            dialog.append("OrientationSet2Right: " + OrientationSet2Right + "\n");
        }

        if (OrientationSet2Front != null) {
            dialog.append("OrientationSet2Front: " + OrientationSet2Front + "\n");
        }

        if (SequenceName != null) {
            dialog.append("Sequence Name: " + SequenceName + "\n");
        }

        if (TextPatientID != null) {
            dialog.append("Text Patient ID: " + TextPatientID + "\n");
        }

        if (TextPatientSex != null) {
            dialog.append("Text Patient Sex: " + TextPatientSex + "\n");
        }

        if (TextPatientAge != null) {
            dialog.append("Text Patient Age: " + TextPatientAge + "\n");
        }

        if (TextPatientAgeUnits != null) {
            dialog.append("Text Patient Age Units: " + TextPatientAgeUnits + "\n");
        }

        if (TextPatientPosition != null) {
            dialog.append("Text Patient Position: " + TextPatientPosition + "\n");
        }

        if (TextImageNumber != null) {
            dialog.append("Text Image Number: " + TextImageNumber + "\n");
        }

        if (TextDateYYYY != null) {
            dialog.append("Text Date Year: " + TextDateYYYY + "\n");
        }

        if (TextDateMM != null) {
            dialog.append("Text Date Month: " + TextDateMM + "\n");
        }

        if (TextDateDD != null) {
            dialog.append("Text Date Day: " + TextDateDD + "\n");
        }

        if (TextTimeHH != null) {
            dialog.append("Text Time Hour: " + TextTimeHH + "\n");
        }

        if (TextTimeMM != null) {
            dialog.append("Text Time Minute: " + TextTimeMM + "\n");
        }

        if (TextAcquisitionTimeMM != null) {
            dialog.append("Text Acquisition Time Minute: " + TextAcquisitionTimeMM + "\n");
        }

        if (TextAcquisitionTimeSS != null) {
            dialog.append("Text Acquisition Time Second: " + TextAcquisitionTimeSS + "\n");
        }

        if (TextAnnotation != null) {
            dialog.append("Text Annotation: " + TextAnnotation + "\n");
        }

        if (TextOrganization != null) {
            dialog.append("Text Organization: " + TextOrganization + "\n");
        }

        if (TextStation != null) {
            dialog.append("Text Station: " + TextStation + "\n");
        }

        if (TextAcquisitionMatrixPhase != null) {
            dialog.append("Text Acquisition Matrix Phase: " + TextAcquisitionMatrixPhase + "\n");
        }

        if (TextAcquisitionMatrixPhaseAxis != null) {
            dialog.append("Text Acquisition Matrix Phase Axis: " + TextAcquisitionMatrixPhaseAxis + "\n");
        }

        if (TextAcquisitionMatrixFreq != null) {
            dialog.append("Text Acquisition Matrix Freq: " + TextAcquisitionMatrixFreq + "\n");
        }

        if (TextAcquisitionMatrixFreqO != null) {
            dialog.append("Text Acquisition Matrix FreqO: " + TextAcquisitionMatrixFreqO + "\n");
        }

        if (TextAcquisitionMatrixFreqS != null) {
            dialog.append("Text Acquisition Matrix FreqS: " + TextAcquisitionMatrixFreqS + "\n");
        }

        if (TextSequence != null) {
            dialog.append("Text Sequence: " + TextSequence + "\n");
        }

        if (TextFlipAngle != null) {
            dialog.append("Text Flip Angle: " + TextFlipAngle + "\n");
        }

        if (TextScanNumberA != null) {
            dialog.append("Text Scan Number A: " + TextScanNumberA + "\n");
        }

        if (TextScanNumberB != null) {
            dialog.append("Text Scan Number B: " + TextScanNumberB + "\n");
        }

        if (TextRepetitionTime != null) {
            dialog.append("Text Repetition Time: " + TextRepetitionTime + "\n");
        }

        if (TextEchoTime != null) {
            dialog.append("Text Echo Time: " + TextEchoTime + "\n");
        }

        if (TextEchoNumber != null) {
            dialog.append("Text Echo Number: " + TextEchoNumber + "\n");
        }

        if (TextSliceThickness != null) {
            dialog.append("Text Slice Thickness: " + TextSliceThickness + "\n");
        }

        if (TextSlicePosition != null) {
            dialog.append("Text Slice Position: " + TextSlicePosition + "\n");
        }

        if (TextAngleFlag1 != null) {
            dialog.append("Text Angle Flag 1: " + TextAngleFlag1 + "\n");
        }

        if (TextAngleFlag2 != null) {
            dialog.append("Text Angle Flag 2: " + TextAngleFlag2 + "\n");
        }

        if (TextAngleFlag3 != null) {
            dialog.append("Text Angle Flag 3: " + TextAngleFlag3 + "\n");
        }

        if (TextAngle != null) {
            dialog.append("Text Angle: " + TextAngle + "\n");
        }

        if (TextFOVH != null) {
            dialog.append("Text FOV H: " + TextFOVH + "\n");
        }

        if (TextFOVV != null) {
            dialog.append("Text FOV V: " + TextFOVV + "\n");
        }

        if (TextTablePosition != null) {
            dialog.append("Text Table Position: " + TextTablePosition + "\n");
        }

        if (TextStudyNumber != null) {
            dialog.append("Text Study Number: " + TextStudyNumber + "\n");
        }

        if (TextDOBYYYY != null) {
            dialog.append("Text DOB Year: " + TextDOBYYYY + "\n");
        }

        if (TextDOBMM != null) {
            dialog.append("Text DOB Month: " + TextDOBMM + "\n");
        }

        if (TextDOBDD != null) {
            dialog.append("Text DOB Day: " + TextDOBDD + "\n");
        }

        if (TextStudyNumber2 != null) {
            dialog.append("Text Study Number 2: " + TextStudyNumber2 + "\n");
        }

        if (TextImageNumber2 != null) {
            dialog.append("Text Image Number 2: " + TextImageNumber2 + "\n");
        }

        if (TextStudyImageNumber3 != null) {
            dialog.append("Text Study Image Number 3: " + TextStudyImageNumber3 + "\n");
        }

        if (TextModelName != null) {
            dialog.append("Text Model Name: " + TextModelName + "\n");
        }

        if (TextPatientName != null) {
            dialog.append("Text Patient Name: " + TextPatientName + "\n");
        }

        if (TextScanStartTimeHH != null) {
            dialog.append("Text Scan Start Time Hour: " + TextScanStartTimeHH + "\n");
        }

        if (TextScanStartTimeMM != null) {
            dialog.append("Text Scan Start Time Minute: " + TextScanStartTimeMM + "\n");
        }

        if (TextScanStartTimeSS != null) {
            dialog.append("Text Scan Start Time Second: " + TextScanStartTimeSS + "\n");
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double getCenterPointX() {
        return CenterPointX;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double getCenterPointY() {
        return CenterPointY;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double getCenterPointZ() {
        return CenterPointZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPatientName() {
        return PatientName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getTextImageNumber() {
        return Integer.parseInt(TextImageNumber);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  AcquisitionDateDD  DOCUMENT ME!
     */
    public void setAcquisitionDateDD(int AcquisitionDateDD) {
        this.AcquisitionDateDD = AcquisitionDateDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  AcquisitionDateMM  DOCUMENT ME!
     */
    public void setAcquisitionDateMM(int AcquisitionDateMM) {
        this.AcquisitionDateMM = AcquisitionDateMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  AcquisitionTimeHH  DOCUMENT ME!
     */
    public void setAcquisitionTimeHH(int AcquisitionTimeHH) {
        this.AcquisitionTimeHH = AcquisitionTimeHH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  AcquisitionTimeMM  DOCUMENT ME!
     */
    public void setAcquisitionTimeMM(int AcquisitionTimeMM) {
        this.AcquisitionTimeMM = AcquisitionTimeMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  AcquisitionTimeSS  DOCUMENT ME!
     */
    public void setAcquisitionTimeSS(int AcquisitionTimeSS) {
        this.AcquisitionTimeSS = AcquisitionTimeSS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  AcquisitionDateYYYY  DOCUMENT ME!
     */
    public void setAcqusitionDateYYYY(int AcquisitionDateYYYY) {
        this.AcquisitionDateYYYY = AcquisitionDateYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  Annotation  DOCUMENT ME!
     */
    public void setAnnotation(String Annotation) {
        this.Annotation = Annotation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CalibrationDateDD  DOCUMENT ME!
     */
    public void setCalibrationDateDD(int CalibrationDateDD) {
        this.CalibrationDateDD = CalibrationDateDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CalibrationDateMM  DOCUMENT ME!
     */
    public void setCalibrationDateMM(int CalibrationDateMM) {
        this.CalibrationDateMM = CalibrationDateMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CalibrationDateYYYY  DOCUMENT ME!
     */
    public void setCalibrationDateYYYY(int CalibrationDateYYYY) {
        this.CalibrationDateYYYY = CalibrationDateYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CalibrationTimeHH  DOCUMENT ME!
     */
    public void setCalibrationTimeHH(int CalibrationTimeHH) {
        this.CalibrationTimeHH = CalibrationTimeHH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CalibrationTimeMM  DOCUMENT ME!
     */
    public void setCalibrationTimeMM(int CalibrationTimeMM) {
        this.CalibrationTimeMM = CalibrationTimeMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CalibrationTimeSS  DOCUMENT ME!
     */
    public void setCalibrationTimeSS(int CalibrationTimeSS) {
        this.CalibrationTimeSS = CalibrationTimeSS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CenterPointX  DOCUMENT ME!
     */
    public void setCenterPointX(double CenterPointX) {
        this.CenterPointX = CenterPointX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CenterPointY  DOCUMENT ME!
     */
    public void setCenterPointY(double CenterPointY) {
        this.CenterPointY = CenterPointY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  CenterPointZ  DOCUMENT ME!
     */
    public void setCenterPointZ(double CenterPointZ) {
        this.CenterPointZ = CenterPointZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ColumnVectorX  DOCUMENT ME!
     */
    public void setColumnVectorX(double ColumnVectorX) {
        this.ColumnVectorX = ColumnVectorX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ColumnVectorY  DOCUMENT ME!
     */
    public void setColumnVectorY(double ColumnVectorY) {
        this.ColumnVectorY = ColumnVectorY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ColumnVectorZ  DOCUMENT ME!
     */
    public void setColumnVectorZ(double ColumnVectorZ) {
        this.ColumnVectorZ = ColumnVectorZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  DisplayMatrixSize  DOCUMENT ME!
     */
    public void setDisplayMatrixSize(int DisplayMatrixSize) {
        this.DisplayMatrixSize = DisplayMatrixSize;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  DistanceFromIsocenter  DOCUMENT ME!
     */
    public void setDistanceFromIsocenter(double DistanceFromIsocenter) {
        this.DistanceFromIsocenter = DistanceFromIsocenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  DOBDD  DOCUMENT ME!
     */
    public void setDOBDD(int DOBDD) {
        this.DOBDD = DOBDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  DOBMM  DOCUMENT ME!
     */
    public void setDOBMM(int DOBMM) {
        this.DOBMM = DOBMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  DOBYYYY  DOCUMENT ME!
     */
    public void setDOBYYYY(int DOBYYYY) {
        this.DOBYYYY = DOBYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  EchoTime  DOCUMENT ME!
     */
    public void setEchoTime(double EchoTime) {
        this.EchoTime = EchoTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  FlipAngle  DOCUMENT ME!
     */
    public void setFlipAngle(double FlipAngle) {
        this.FlipAngle = FlipAngle;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  FOVColumn  DOCUMENT ME!
     */
    public void setFOVCOlumn(double FOVColumn) {
        this.FOVColumn = FOVColumn;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  FOVRow  DOCUMENT ME!
     */
    public void setFOVRow(double FOVRow) {
        this.FOVRow = FOVRow;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  FrequencyMHz  DOCUMENT ME!
     */
    public void setFrequencyMHz(double FrequencyMHz) {
        this.FrequencyMHz = FrequencyMHz;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ImageDateDD  DOCUMENT ME!
     */
    public void setImageDateDD(int ImageDateDD) {
        this.ImageDateDD = ImageDateDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ImageDateMM  DOCUMENT ME!
     */
    public void setImageDateMM(int ImageDateMM) {
        this.ImageDateMM = ImageDateMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ImageDateYYYY  DOCUMENT ME!
     */
    public void setImageDateYYYY(int ImageDateYYYY) {
        this.ImageDateYYYY = ImageDateYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ImagedNucleus  DOCUMENT ME!
     */
    public void setImagedNucleus(String ImagedNucleus) {
        this.ImagedNucleus = ImagedNucleus;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ImageTimeHH  DOCUMENT ME!
     */
    public void setImageTimeHH(int ImageTimeHH) {
        this.ImageTimeHH = ImageTimeHH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ImageTimeMM  DOCUMENT ME!
     */
    public void setImageTimeMM(int ImageTimeMM) {
        this.ImageTimeMM = ImageTimeMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ImageTimeSS  DOCUMENT ME!
     */
    public void setImageTimeSS(int ImageTimeSS) {
        this.ImageTimeSS = ImageTimeSS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  InstitutionName  DOCUMENT ME!
     */
    public void setInstitutionName(String InstitutionName) {
        this.InstitutionName = InstitutionName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  LastMoveDateDD  DOCUMENT ME!
     */
    public void setLastMoveDateDD(int LastMoveDateDD) {
        this.LastMoveDateDD = LastMoveDateDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  LastMoveDateMM  DOCUMENT ME!
     */
    public void setLastMoveDateMM(int LastMoveDateMM) {
        this.LastMoveDateMM = LastMoveDateMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  LastMoveDateYYYY  DOCUMENT ME!
     */
    public void setLastMoveDateYYYY(int LastMoveDateYYYY) {
        this.LastMoveDateYYYY = LastMoveDateYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  LastMoveTimeHH  DOCUMENT ME!
     */
    public void setLastMoveTimeHH(int LastMoveTimeHH) {
        this.LastMoveTimeHH = LastMoveTimeHH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  LastMoveTimeMM  DOCUMENT ME!
     */
    public void setLastMoveTimeMM(int LastMoveTimeMM) {
        this.LastMoveTimeMM = LastMoveTimeMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  LastMoveTimeSS  DOCUMENT ME!
     */
    public void setLastMoveTimeSS(int LastMoveTimeSS) {
        this.LastMoveTimeSS = LastMoveTimeSS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  MagneticFieldStrength  DOCUMENT ME!
     */
    public void setMagneticFieldStrength(double MagneticFieldStrength) {
        this.MagneticFieldStrength = MagneticFieldStrength;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  Manufacturer  DOCUMENT ME!
     */
    public void setManufacturer(String Manufacturer) {
        this.Manufacturer = Manufacturer;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ModelName  DOCUMENT ME!
     */
    public void setModelName(String ModelName) {
        this.ModelName = ModelName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  NormalVectorX  DOCUMENT ME!
     */
    public void setNormalVectorX(double NormalVectorX) {
        this.NormalVectorX = NormalVectorX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  NormalVectorY  DOCUMENT ME!
     */
    public void setNormalVectorY(double NormalVectorY) {
        this.NormalVectorY = NormalVectorY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  NormalVectorZ  DOCUMENT ME!
     */
    public void setNormalVectorZ(double NormalVectorZ) {
        this.NormalVectorZ = NormalVectorZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  OrientationSet1Back  DOCUMENT ME!
     */
    public void setOrientationSet1Back(String OrientationSet1Back) {
        this.OrientationSet1Back = OrientationSet1Back;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  OrientationSet1Left  DOCUMENT ME!
     */
    public void setOrientationSet1Left(String OrientationSet1Left) {
        this.OrientationSet1Left = OrientationSet1Left;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  OrientationSet1Top  DOCUMENT ME!
     */
    public void setOrientationSet1Top(String OrientationSet1Top) {
        this.OrientationSet1Top = OrientationSet1Top;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  OrientationSet2Down  DOCUMENT ME!
     */
    public void setOrientationSet2Down(String OrientationSet2Down) {
        this.OrientationSet2Down = OrientationSet2Down;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  OrientationSet2Front  DOCUMENT ME!
     */
    public void setOrientationSet2Front(String OrientationSet2Front) {
        this.OrientationSet2Front = OrientationSet2Front;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  OrientationSet2Right  DOCUMENT ME!
     */
    public void setOrientationSet2Right(String OrientationSet2Right) {
        this.OrientationSet2Right = OrientationSet2Right;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  PatientAge  DOCUMENT ME!
     */
    public void setPatientAge(String PatientAge) {
        this.PatientAge = PatientAge;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  PatientAgeUnits  DOCUMENT ME!
     */
    public void setPatientAgeUnits(String PatientAgeUnits) {
        this.PatientAgeUnits = PatientAgeUnits;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  PatientID  DOCUMENT ME!
     */
    public void setPatientID(String PatientID) {
        this.PatientID = PatientID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  PatientName  DOCUMENT ME!
     */
    public void setPatientName(String PatientName) {
        this.PatientName = PatientName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ReceivingCoil  DOCUMENT ME!
     */
    public void setReceivingCoil(String ReceivingCoil) {
        this.ReceivingCoil = ReceivingCoil;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RegistrationDateDD  DOCUMENT ME!
     */
    public void setRegistrationDateDD(int RegistrationDateDD) {
        this.RegistrationDateDD = RegistrationDateDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RegistrationDateMM  DOCUMENT ME!
     */
    public void setRegistrationDateMM(int RegistrationDateMM) {
        this.RegistrationDateMM = RegistrationDateMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RegistrationDateYYYY  DOCUMENT ME!
     */
    public void setRegistrationDateYYYY(int RegistrationDateYYYY) {
        this.RegistrationDateYYYY = RegistrationDateYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RegistrationTimeHH  DOCUMENT ME!
     */
    public void setRegistrationTimeHH(int RegistrationTimeHH) {
        this.RegistrationTimeHH = RegistrationTimeHH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RegistrationTimeMM  DOCUMENT ME!
     */
    public void setRegistrationTimeMM(int RegistrationTimeMM) {
        this.RegistrationTimeMM = RegistrationTimeMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RegistrationTimeSS  DOCUMENT ME!
     */
    public void setRegistrationTimeSS(int RegistrationTimeSS) {
        this.RegistrationTimeSS = RegistrationTimeSS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RepetitionTime  DOCUMENT ME!
     */
    public void setRepetitionTime(double RepetitionTime) {
        this.RepetitionTime = RepetitionTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RowVectorX  DOCUMENT ME!
     */
    public void setRowVectorX(double RowVectorX) {
        this.RowVectorX = RowVectorX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RowVectorY  DOCUMENT ME!
     */
    public void setRowVectorY(double RowVectorY) {
        this.RowVectorY = RowVectorY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RowVectorZ  DOCUMENT ME!
     */
    public void setRowVectorZ(double RowVectorZ) {
        this.RowVectorZ = RowVectorZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SequenceAuthor  DOCUMENT ME!
     */
    public void setSequenceAuthor(String SequenceAuthor) {
        this.SequenceAuthor = SequenceAuthor;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SequenceName  DOCUMENT ME!
     */
    public void setSequenceName(String SequenceName) {
        this.SequenceName = SequenceName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SequencePrgName  DOCUMENT ME!
     */
    public void setSequencePrgName(String SequencePrgName) {
        this.SequencePrgName = SequencePrgName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SequenceType  DOCUMENT ME!
     */
    public void setSequenceType(String SequenceType) {
        this.SequenceType = SequenceType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SequenceWkcName  DOCUMENT ME!
     */
    public void setSequenceWkcName(String SequenceWkcName) {
        this.SequenceWkcName = SequenceWkcName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SiemensStudyDateDD  DOCUMENT ME!
     */
    public void setSiemensStudyDateDD(int SiemensStudyDateDD) {
        this.SiemensStudyDateDD = SiemensStudyDateDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SiemensStudyDateMM  DOCUMENT ME!
     */
    public void setSiemensStudyDateMM(int SiemensStudyDateMM) {
        this.SiemensStudyDateMM = SiemensStudyDateMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SiemensStudyDateYYYY  DOCUMENT ME!
     */
    public void setSiemensStudyDateYYYY(int SiemensStudyDateYYYY) {
        this.SiemensStudyDateYYYY = SiemensStudyDateYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SiemensStudyTimeHH  DOCUMENT ME!
     */
    public void setSiemensStudyTimeHH(int SiemensStudyTimeHH) {
        this.SiemensStudyTimeHH = SiemensStudyTimeHH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SiemensStudyTimeMM  DOCUMENT ME!
     */
    public void setSiemensStudyTimeMM(int SiemensStudyTimeMM) {
        this.SiemensStudyTimeMM = SiemensStudyTimeMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SiemensStudyTimeSS  DOCUMENT ME!
     */
    public void setSiemensStudyTimeSS(int SiemensStudyTimeSS) {
        this.SiemensStudyTimeSS = SiemensStudyTimeSS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  Station  DOCUMENT ME!
     */
    public void setStation(String Station) {
        this.Station = Station;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAcquisitionMatrixFreq  DOCUMENT ME!
     */
    public void setTextAcquisitionMatrixFreq(String TextAcquisitionMatrixFreq) {
        this.TextAcquisitionMatrixFreq = TextAcquisitionMatrixFreq;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAcquisitionMatrixFreqO  DOCUMENT ME!
     */
    public void setTextAcquisitionMatrixFreqO(String TextAcquisitionMatrixFreqO) {
        this.TextAcquisitionMatrixFreqO = TextAcquisitionMatrixFreqO;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAcquisitionMatrixFreqS  DOCUMENT ME!
     */
    public void setTextAcquisitionMatrixFreqS(String TextAcquisitionMatrixFreqS) {
        this.TextAcquisitionMatrixFreqS = TextAcquisitionMatrixFreqS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAcquisitionMatrixPhase  DOCUMENT ME!
     */
    public void setTextAcquisitionMatrixPhase(String TextAcquisitionMatrixPhase) {
        this.TextAcquisitionMatrixPhase = TextAcquisitionMatrixPhase;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAcquisitionMatrixPhaseAxis  DOCUMENT ME!
     */
    public void setTextAcquisitionMatrixPhaseAxis(String TextAcquisitionMatrixPhaseAxis) {
        this.TextAcquisitionMatrixPhaseAxis = TextAcquisitionMatrixPhaseAxis;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAcquisitionTimeMM  DOCUMENT ME!
     */
    public void setTextAcquisitionTimeMM(String TextAcquisitionTimeMM) {
        this.TextAcquisitionTimeMM = TextAcquisitionTimeMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAcquisitionTimeSS  DOCUMENT ME!
     */
    public void setTextAcquisitionTimeSS(String TextAcquisitionTimeSS) {
        this.TextAcquisitionTimeSS = TextAcquisitionTimeSS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAngle  DOCUMENT ME!
     */
    public void setTextAngle(String TextAngle) {
        this.TextAngle = TextAngle;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAngleFlag1  DOCUMENT ME!
     */
    public void setTextAngleFlag1(String TextAngleFlag1) {
        this.TextAngleFlag1 = TextAngleFlag1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAngleFlag2  DOCUMENT ME!
     */
    public void setTextAngleFlag2(String TextAngleFlag2) {
        this.TextAngleFlag2 = TextAngleFlag2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAngleFlag3  DOCUMENT ME!
     */
    public void setTextAngleFlag3(String TextAngleFlag3) {
        this.TextAngleFlag3 = TextAngleFlag3;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextAnnotation  DOCUMENT ME!
     */
    public void setTextAnnotation(String TextAnnotation) {
        this.TextAnnotation = TextAnnotation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextDateDD  DOCUMENT ME!
     */
    public void setTextDateDD(String TextDateDD) {
        this.TextDateDD = TextDateDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextDateMM  DOCUMENT ME!
     */
    public void setTextDateMM(String TextDateMM) {
        this.TextDateMM = TextDateMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextDateYYYY  DOCUMENT ME!
     */
    public void setTextDateYYYY(String TextDateYYYY) {
        this.TextDateYYYY = TextDateYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextDOBDD  DOCUMENT ME!
     */
    public void setTextDOBDD(String TextDOBDD) {
        this.TextDOBDD = TextDOBDD;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextDOBMM  DOCUMENT ME!
     */
    public void setTextDOBMM(String TextDOBMM) {
        this.TextDOBMM = TextDOBMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextDOBYYYY  DOCUMENT ME!
     */
    public void setTextDOBYYYY(String TextDOBYYYY) {
        this.TextDOBYYYY = TextDOBYYYY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextEchoNumber  DOCUMENT ME!
     */
    public void setTextEchoNumber(String TextEchoNumber) {
        this.TextEchoNumber = TextEchoNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextEchoTime  DOCUMENT ME!
     */
    public void setTextEchoTime(String TextEchoTime) {
        this.TextEchoTime = TextEchoTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextFlipAngle  DOCUMENT ME!
     */
    public void setTextFlipAngle(String TextFlipAngle) {
        this.TextFlipAngle = TextFlipAngle;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextFOVH  DOCUMENT ME!
     */
    public void setTextFOVH(String TextFOVH) {
        this.TextFOVH = TextFOVH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextFOVV  DOCUMENT ME!
     */
    public void setTextFOVV(String TextFOVV) {
        this.TextFOVV = TextFOVV;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextImageNumber  DOCUMENT ME!
     */
    public void setTextImageNumber(String TextImageNumber) {
        this.TextImageNumber = TextImageNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextImageNumber2  DOCUMENT ME!
     */
    public void setTextImageNumber2(String TextImageNumber2) {
        this.TextImageNumber2 = TextImageNumber2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextModelName  DOCUMENT ME!
     */
    public void setTextModelName(String TextModelName) {
        this.TextModelName = TextModelName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextOrganization  DOCUMENT ME!
     */
    public void setTextOrganization(String TextOrganization) {
        this.TextOrganization = TextOrganization;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextPatientAge  DOCUMENT ME!
     */
    public void setTextPatientAge(String TextPatientAge) {
        this.TextPatientAge = TextPatientAge;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextPatientAgeUnits  DOCUMENT ME!
     */
    public void setTextPatientAgeUnits(String TextPatientAgeUnits) {
        this.TextPatientAgeUnits = TextPatientAgeUnits;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextPatientID  DOCUMENT ME!
     */
    public void setTextPatientID(String TextPatientID) {
        this.TextPatientID = TextPatientID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextPatientName  DOCUMENT ME!
     */
    public void setTextPatientName(String TextPatientName) {
        this.TextPatientName = TextPatientName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextPatientPosition  DOCUMENT ME!
     */
    public void setTextPatientPosition(String TextPatientPosition) {
        this.TextPatientPosition = TextPatientPosition;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextPatientSex  DOCUMENT ME!
     */
    public void setTextPatientSex(String TextPatientSex) {
        this.TextPatientSex = TextPatientSex;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextRepetitionTime  DOCUMENT ME!
     */
    public void setTextRepetitionTime(String TextRepetitionTime) {
        this.TextRepetitionTime = TextRepetitionTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextScanNumberA  DOCUMENT ME!
     */
    public void setTextScanNumberA(String TextScanNumberA) {
        this.TextScanNumberA = TextScanNumberA;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextScanNumberB  DOCUMENT ME!
     */
    public void setTextScanNumberB(String TextScanNumberB) {
        this.TextScanNumberB = TextScanNumberB;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextScanStartTimeHH  DOCUMENT ME!
     */
    public void setTextScanStartTimeHH(String TextScanStartTimeHH) {
        this.TextScanStartTimeHH = TextScanStartTimeHH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextScanStartTimeMM  DOCUMENT ME!
     */
    public void setTextScanStartTimeMM(String TextScanStartTimeMM) {
        this.TextScanStartTimeMM = TextScanStartTimeMM;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextScanStartTimeSS  DOCUMENT ME!
     */
    public void setTextScanStartTimeSS(String TextScanStartTimeSS) {
        this.TextScanStartTimeSS = TextScanStartTimeSS;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextSequence  DOCUMENT ME!
     */
    public void setTextSequence(String TextSequence) {
        this.TextSequence = TextSequence;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextSlicePosition  DOCUMENT ME!
     */
    public void setTextSlicePosition(String TextSlicePosition) {
        this.TextSlicePosition = TextSlicePosition;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextSliceThickness  DOCUMENT ME!
     */
    public void setTextSliceThickness(String TextSliceThickness) {
        this.TextSliceThickness = TextSliceThickness;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextStation  DOCUMENT ME!
     */
    public void setTextStation(String TextStation) {
        this.TextStation = TextStation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextStudyImageNumber3  DOCUMENT ME!
     */
    public void setTextStudyImageNumber3(String TextStudyImageNumber3) {
        this.TextStudyImageNumber3 = TextStudyImageNumber3;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextStudyNumber  DOCUMENT ME!
     */
    public void setTextStudyNumber(String TextStudyNumber) {
        this.TextStudyNumber = TextStudyNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextStudyNumber2  DOCUMENT ME!
     */
    public void setTextStudyNumber2(String TextStudyNumber2) {
        this.TextStudyNumber2 = TextStudyNumber2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextTablePosition  DOCUMENT ME!
     */
    public void setTextTablePosition(String TextTablePosition) {
        this.TextTablePosition = TextTablePosition;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextTimeHH  DOCUMENT ME!
     */
    public void setTextTimeHH(String TextTimeHH) {
        this.TextTimeHH = TextTimeHH;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TextTimeMM  DOCUMENT ME!
     */
    public void setTextTimeMM(String TextTimeMM) {
        this.TextTimeMM = TextTimeMM;
    }

    /**
     * Returns the image name which should be used for the image this file info is attached to.
     * 
     * @return  The name to give to this file info's image.
     */
    public String getImageNameFromInfo() {
        return TextSequence.trim() + "_" + TextScanNumberA.trim() + "_" + TextScanNumberB.trim() + "_" + TextImageNumber.trim();
    }
}
