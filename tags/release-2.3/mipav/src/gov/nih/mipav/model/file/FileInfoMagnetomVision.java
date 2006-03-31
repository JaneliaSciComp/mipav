package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   a Siemens MagnetomVision image is stored on disk.
*
*/

public class FileInfoMagnetomVision extends FileInfoBase {

    private int SiemensStudyDateYYYY;
    private int SiemensStudyDateMM;
    private int SiemensStudyDateDD;
    private int AcquisitionDateYYYY;
    private int AcquisitionDateMM;
    private int AcquisitionDateDD;
    private int ImageDateYYYY;
    private int ImageDateMM;
    private int ImageDateDD;
    private int SiemensStudyTimeHH;
    private int SiemensStudyTimeMM;
    private int SiemensStudyTimeSS;
    private int AcquisitionTimeHH;
    private int AcquisitionTimeMM;
    private int AcquisitionTimeSS;
    private int ImageTimeHH;
    private int ImageTimeMM;
    private int ImageTimeSS;
    private String Manufacturer = null; // should be SIEMENS
    private String InstitutionName = null;
    private String Annotation = null;
    private String ModelName = null; // should be MAGNETOM VISION
    private int LastMoveDateYYYY;
    private int LastMoveDateMM;
    private int LastMoveDateDD;
    private int LastMoveTimeHH;
    private int LastMoveTimeMM;
    private int LastMoveTimeSS;
    private String PatientName = null;
    private String PatientID = null;
    private int DOBYYYY;
    private int DOBMM;
    private int DOBDD;
    private String PatientAge = null;
    private String PatientAgeUnits = null; // ('Y' =years)
    private int RegistrationDateYYYY;
    private int RegistrationDateMM;
    private int RegistrationDateDD;
    private int RegistrationTimeHH;
    private int RegistrationTimeMM;
    private int RegistrationTimeSS;
    private double SliceThickness;
    private double RepetitionTime;
    private double EchoTime;
    private double FrequencyMHz;
    private String Station = null;
    private int CalibrationDateYYYY;
    private int CalibrationDateMM;
    private int CalibrationDateDD;
    private int CalibrationTimeHH;
    private int CalibrationTimeMM;
    private int CalibrationTimeSS;
    private String ReceivingCoil = null;
    private String ImagedNucleus = null;
    private double FlipAngle;
    private double MagneticFieldStrength;
    private int DisplayMatrixSize;
    private String SequencePrgName = null;
    private String SequenceWkcName = null;
    private String SequenceAuthor = null;
    private String SequenceType = null;
    private double FOVRow;
    private double FOVColumn;
    private double CenterPointX;
    private double CenterPointY;
    private double CenterPointZ;
    private double NormalVectorX;
    private double NormalVectorY;
    private double NormalVectorZ;
    private double DistanceFromIsocenter;
    private double RowVectorX;
    private double RowVectorY;
    private double RowVectorZ;
    private double ColumnVectorX;
    private double ColumnVectorY;
    private double ColumnVectorZ;
    private String OrientationSet1Top = null;
    private String OrientationSet1Left = null;
    private String OrientationSet1Back = null;
    private String OrientationSet2Down = null;
    private String OrientationSet2Right = null;
    private String OrientationSet2Front = null;
    private String SequenceName = null;

    private String TextPatientID = null;
    private String TextPatientSex = null;
    private String TextPatientAge = null;
    private String TextPatientAgeUnits = null;  // ('Y' = years)
    private String TextPatientPosition = null;
    private String TextImageNumber = null;
    private String TextDateDD = null;
    private String TextDateMM = null;
    private String TextDateYYYY = null;
    private String TextTimeHH = null;
    private String TextTimeMM = null;
    private String TextAcquisitionTimeMM = null;
    private String TextAcquisitionTimeSS = null;
    private String TextAnnotation = null;
    private String TextOrganization = null;
    private String TextStation = null;
    private String TextAcquisitionMatrixPhase = null;
    private String TextAcquisitionMatrixPhaseAxis = null; // ('h' = horizotnal, 'v' = vertical)
    private String TextAcquisitionMatrixFreq = null;
    private String TextAcquisitionMatrixFreqO = null; // ('o' = o, ' ' = blank)
    private String TextAcquisitionMatrixFreqS = null; // ('s' = s, ' ' = blank)
    private String TextSequence = null;
    private String TextFlipAngle = null;
    private String TextScanNumberA = null;
    private String TextScanNumberB = null;
    private String TextRepetitionTime = null;
    private String TextEchoTime = null;
    private String TextEchoNumber = null;
    private String TextSliceThickness = null;
    private String TextSlicePosition = null;
    private String TextAngleFlag1 = null;
    private String TextAngleFlag2 = null;
    private String TextAngleFlag3 = null;
    private String TextAngle = null;
    private String TextFOVH = null;
    private String TextFOVV = null;
    private String TextTablePosition = null;
    private String TextStudyNumber = null;
    private String TextDOBDD = null;
    private String TextDOBMM = null;
    private String TextDOBYYYY = null;
    private String TextStudyNumber2 = null;
    private String TextImageNumber2 = null;
    private String TextStudyImageNumber3 = null;
    private String TextModelName = null;
    private String TextPatientName = null;
    private String TextScanStartTimeHH = null;
    private String TextScanStartTimeMM = null;
    private String TextScanStartTimeSS = null;

    /**
    *  FileInfoMagnetomVision     - file info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoMagnetomVision(String name, String directory, int format) {
        super(name, directory, format);
    }

    public void setSiemensStudyDateYYYY(int SiemensStudyDateYYYY) {
        this.SiemensStudyDateYYYY = SiemensStudyDateYYYY;
    }

    public void setSiemensStudyDateMM(int SiemensStudyDateMM) {
        this.SiemensStudyDateMM = SiemensStudyDateMM;
    }

    public void setSiemensStudyDateDD(int SiemensStudyDateDD) {
        this.SiemensStudyDateDD = SiemensStudyDateDD;
    }

    public void setAcqusitionDateYYYY(int AcquisitionDateYYYY) {
        this.AcquisitionDateYYYY = AcquisitionDateYYYY;
    }

    public void setAcquisitionDateMM(int AcquisitionDateMM) {
        this.AcquisitionDateMM = AcquisitionDateMM;
    }

    public void setAcquisitionDateDD(int AcquisitionDateDD) {
        this.AcquisitionDateDD = AcquisitionDateDD;
    }

    public void setImageDateYYYY(int ImageDateYYYY) {
        this.ImageDateYYYY = ImageDateYYYY;
    }

    public void setImageDateMM(int ImageDateMM) {
        this.ImageDateMM = ImageDateMM;
    }

    public void setImageDateDD(int ImageDateDD) {
        this.ImageDateDD = ImageDateDD;
    }

    public void setSiemensStudyTimeHH(int SiemensStudyTimeHH) {
        this.SiemensStudyTimeHH = SiemensStudyTimeHH;
    }

    public void setSiemensStudyTimeMM(int SiemensStudyTimeMM) {
        this.SiemensStudyTimeMM = SiemensStudyTimeMM;
    }

    public void setSiemensStudyTimeSS(int SiemensStudyTimeSS) {
        this.SiemensStudyTimeSS = SiemensStudyTimeSS;
    }

    public void setAcquisitionTimeHH(int AcquisitionTimeHH) {
        this.AcquisitionTimeHH = AcquisitionTimeHH;
    }

    public void setAcquisitionTimeMM(int AcquisitionTimeMM) {
        this.AcquisitionTimeMM = AcquisitionTimeMM;
    }

    public void setAcquisitionTimeSS(int AcquisitionTimeSS) {
        this.AcquisitionTimeSS = AcquisitionTimeSS;
    }

    public void setImageTimeHH(int ImageTimeHH) {
        this.ImageTimeHH = ImageTimeHH;
    }

    public void setImageTimeMM(int ImageTimeMM) {
        this.ImageTimeMM = ImageTimeMM;
    }

    public void setImageTimeSS(int ImageTimeSS) {
        this.ImageTimeSS = ImageTimeSS;
    }

    public void setManufacturer(String Manufacturer) {
        this.Manufacturer = Manufacturer;
    }

    public void setInstitutionName(String InstitutionName) {
        this.InstitutionName = InstitutionName;
    }

    public void setAnnotation(String Annotation) {
        this.Annotation = Annotation;
    }

    public void setModelName(String ModelName) {
        this.ModelName = ModelName;
    }

    public void setLastMoveDateYYYY(int LastMoveDateYYYY) {
        this.LastMoveDateYYYY = LastMoveDateYYYY;
    }

    public void setLastMoveDateMM(int LastMoveDateMM) {
        this.LastMoveDateMM = LastMoveDateMM;
    }

    public void setLastMoveDateDD(int LastMoveDateDD) {
        this.LastMoveDateDD = LastMoveDateDD;
    }

    public void setLastMoveTimeHH(int LastMoveTimeHH) {
        this.LastMoveTimeHH = LastMoveTimeHH;
    }

    public void setLastMoveTimeMM(int LastMoveTimeMM) {
        this.LastMoveTimeMM = LastMoveTimeMM;
    }

    public void setLastMoveTimeSS(int LastMoveTimeSS) {
        this.LastMoveTimeSS = LastMoveTimeSS;
    }

    public void setPatientName(String PatientName) {
        this.PatientName = PatientName;
    }

    public String getPatientName() {
        return PatientName;
    }

    public void setPatientID(String PatientID) {
        this.PatientID = PatientID;
    }

    public void setDOBYYYY(int DOBYYYY) {
        this.DOBYYYY = DOBYYYY;
    }

    public void setDOBMM(int DOBMM) {
        this.DOBMM = DOBMM;
    }

    public void setDOBDD(int DOBDD) {
        this.DOBDD = DOBDD;
    }

    public void setPatientAge(String PatientAge) {
        this.PatientAge = PatientAge;
    }

    public void setPatientAgeUnits(String PatientAgeUnits) {
        this.PatientAgeUnits = PatientAgeUnits;
    }

    public void setRegistrationDateYYYY(int RegistrationDateYYYY) {
        this.RegistrationDateYYYY = RegistrationDateYYYY;
    }

    public void setRegistrationDateMM(int RegistrationDateMM) {
        this.RegistrationDateMM = RegistrationDateMM;
    }

    public void setRegistrationDateDD(int RegistrationDateDD) {
        this.RegistrationDateDD = RegistrationDateDD;
    }

    public void setRegistrationTimeHH(int RegistrationTimeHH) {
        this.RegistrationTimeHH = RegistrationTimeHH;
    }

    public void setRegistrationTimeMM(int RegistrationTimeMM) {
        this.RegistrationTimeMM = RegistrationTimeMM;
    }

    public void setRegistrationTimeSS(int RegistrationTimeSS) {
        this.RegistrationTimeSS = RegistrationTimeSS;
    }

    public void setSliceThickness(double SliceThickness) {
        this.SliceThickness = SliceThickness;
    }

    public double getSliceThickness() {
        return SliceThickness;
    }

    public void setRepetitionTime(double RepetitionTime) {
        this.RepetitionTime = RepetitionTime;
    }

    public void setEchoTime(double EchoTime) {
        this.EchoTime = EchoTime;
    }

    public void setFrequencyMHz(double FrequencyMHz) {
        this.FrequencyMHz = FrequencyMHz;
    }

    public void setStation(String Station) {
        this.Station = Station;
    }

    public void setCalibrationDateYYYY(int CalibrationDateYYYY) {
        this.CalibrationDateYYYY = CalibrationDateYYYY;
    }

    public void setCalibrationDateMM(int CalibrationDateMM) {
        this.CalibrationDateMM = CalibrationDateMM;
    }

    public void setCalibrationDateDD(int CalibrationDateDD) {
        this.CalibrationDateDD = CalibrationDateDD;
    }

    public void setCalibrationTimeHH(int CalibrationTimeHH) {
        this.CalibrationTimeHH = CalibrationTimeHH;
    }

    public void setCalibrationTimeMM(int CalibrationTimeMM) {
        this.CalibrationTimeMM = CalibrationTimeMM;
    }

    public void setCalibrationTimeSS(int CalibrationTimeSS) {
        this.CalibrationTimeSS = CalibrationTimeSS;
    }

    public void setReceivingCoil(String ReceivingCoil) {
        this.ReceivingCoil = ReceivingCoil;
    }

    public void setImagedNucleus(String ImagedNucleus) {
        this.ImagedNucleus = ImagedNucleus;
    }

    public void setFlipAngle(double FlipAngle) {
        this.FlipAngle = FlipAngle;
    }

    public void setMagneticFieldStrength(double MagneticFieldStrength) {
        this.MagneticFieldStrength = MagneticFieldStrength;
    }

    public void setDisplayMatrixSize(int DisplayMatrixSize) {
        this.DisplayMatrixSize = DisplayMatrixSize;
    }

    public void setSequencePrgName(String SequencePrgName) {
        this.SequencePrgName = SequencePrgName;
    }

    public void setSequenceWkcName(String SequenceWkcName) {
        this.SequenceWkcName = SequenceWkcName;
    }

    public void setSequenceAuthor(String SequenceAuthor) {
        this.SequenceAuthor = SequenceAuthor;
    }

    public void setSequenceType(String SequenceType) {
        this.SequenceType = SequenceType;
    }

    public void setFOVRow(double FOVRow) {
        this.FOVRow = FOVRow;
    }

    public void setFOVCOlumn(double FOVColumn) {
        this.FOVColumn = FOVColumn;
    }

    public void setCenterPointX(double CenterPointX) {
        this.CenterPointX = CenterPointX;
    }

    public double getCenterPointX() {
        return CenterPointX;
    }

    public void setCenterPointY(double CenterPointY) {
        this.CenterPointY = CenterPointY;
    }

    public double getCenterPointY() {
        return CenterPointY;
    }

    public void setCenterPointZ(double CenterPointZ) {
        this.CenterPointZ = CenterPointZ;
    }

    public double getCenterPointZ() {
        return CenterPointZ;
    }

    public void setNormalVectorX(double NormalVectorX) {
        this.NormalVectorX = NormalVectorX;
    }

    public void setNormalVectorY(double NormalVectorY) {
        this.NormalVectorY = NormalVectorY;
    }

    public void setNormalVectorZ(double NormalVectorZ) {
        this.NormalVectorZ = NormalVectorZ;
    }

    public void setDistanceFromIsocenter(double DistanceFromIsocenter) {
        this.DistanceFromIsocenter = DistanceFromIsocenter;
    }

    public void setRowVectorX(double RowVectorX) {
        this.RowVectorX = RowVectorX;
    }

    public void setRowVectorY(double RowVectorY) {
        this.RowVectorY = RowVectorY;
    }

    public void setRowVectorZ(double RowVectorZ) {
        this.RowVectorZ = RowVectorZ;
    }

    public void setColumnVectorX(double ColumnVectorX) {
        this.ColumnVectorX = ColumnVectorX;
    }

    public void setColumnVectorY(double ColumnVectorY) {
        this.ColumnVectorY = ColumnVectorY;
    }

    public void setColumnVectorZ(double ColumnVectorZ) {
        this.ColumnVectorZ = ColumnVectorZ;
    }

    public void setOrientationSet1Top(String OrientationSet1Top) {
        this.OrientationSet1Top = OrientationSet1Top;
    }

    public void setOrientationSet1Left(String OrientationSet1Left) {
        this.OrientationSet1Left = OrientationSet1Left;
    }

    public void setOrientationSet1Back(String OrientationSet1Back) {
        this.OrientationSet1Back = OrientationSet1Back;
    }

    public void setOrientationSet2Down(String OrientationSet2Down) {
        this.OrientationSet2Down = OrientationSet2Down;
    }

    public void setOrientationSet2Right(String OrientationSet2Right) {
        this.OrientationSet2Right = OrientationSet2Right;
    }

    public void setOrientationSet2Front(String OrientationSet2Front) {
        this.OrientationSet2Front = OrientationSet2Front;
    }

    public void setSequenceName(String SequenceName) {
        this.SequenceName = SequenceName;
    }

    public void setTextPatientID(String TextPatientID) {
        this.TextPatientID = TextPatientID;
    }

    public void setTextPatientSex(String TextPatientSex) {
        this.TextPatientSex = TextPatientSex;
    }

    public void setTextPatientAge(String TextPatientAge) {
        this.TextPatientAge = TextPatientAge;
    }

    public void setTextPatientAgeUnits(String TextPatientAgeUnits) {
        this.TextPatientAgeUnits = TextPatientAgeUnits;
    }

    public void setTextPatientPosition(String TextPatientPosition) {
        this.TextPatientPosition = TextPatientPosition;
    }

    public void setTextImageNumber(String TextImageNumber) {
        this.TextImageNumber = TextImageNumber;
    }

    public int getTextImageNumber() {
        return Integer.parseInt(TextImageNumber);
    }

    public void setTextDateDD(String TextDateDD) {
        this.TextDateDD = TextDateDD;
    }

    public void setTextDateMM(String TextDateMM) {
        this.TextDateMM = TextDateMM;
    }

    public void setTextDateYYYY(String TextDateYYYY) {
        this.TextDateYYYY = TextDateYYYY;
    }

    public void setTextTimeHH(String TextTimeHH) {
        this.TextTimeHH = TextTimeHH;
    }

    public void setTextTimeMM(String TextTimeMM) {
        this.TextTimeMM = TextTimeMM;
    }

    public void setTextAcquisitionTimeMM(String TextAcquisitionTimeMM) {
        this.TextAcquisitionTimeMM = TextAcquisitionTimeMM;
    }

    public void setTextAcquisitionTimeSS(String TextAcquisitionTimeSS) {
        this.TextAcquisitionTimeSS = TextAcquisitionTimeSS;
    }

    public void setTextAnnotation(String TextAnnotation) {
        this.TextAnnotation = TextAnnotation;
    }

    public void setTextOrganization(String TextOrganization) {
        this.TextOrganization = TextOrganization;
    }

    public void setTextStation(String TextStation) {
        this.TextStation = TextStation;
    }

    public void setTextAcquisitionMatrixPhase(String TextAcquisitionMatrixPhase) {
        this.TextAcquisitionMatrixPhase = TextAcquisitionMatrixPhase;
    }

    public void setTextAcquisitionMatrixPhaseAxis(String TextAcquisitionMatrixPhaseAxis) {
        this.TextAcquisitionMatrixPhaseAxis = TextAcquisitionMatrixPhaseAxis;
    }

    public void setTextAcquisitionMatrixFreq(String TextAcquisitionMatrixFreq) {
        this.TextAcquisitionMatrixFreq = TextAcquisitionMatrixFreq;
    }

    public void setTextAcquisitionMatrixFreqO(String TextAcquisitionMatrixFreqO) {
        this.TextAcquisitionMatrixFreqO = TextAcquisitionMatrixFreqO;
    }

    public void setTextAcquisitionMatrixFreqS(String TextAcquisitionMatrixFreqS) {
        this.TextAcquisitionMatrixFreqS = TextAcquisitionMatrixFreqS;
    }

    public void setTextSequence(String TextSequence) {
        this.TextSequence = TextSequence;
    }

    public void setTextFlipAngle(String TextFlipAngle) {
        this.TextFlipAngle = TextFlipAngle;
    }

    public void setTextScanNumberA(String TextScanNumberA) {
        this.TextScanNumberA = TextScanNumberA;
    }

    public void setTextScanNumberB(String TextScanNumberB) {
        this.TextScanNumberB = TextScanNumberB;
    }

    public void setTextRepetitionTime(String TextRepetitionTime) {
        this.TextRepetitionTime = TextRepetitionTime;
    }

    public void setTextEchoTime(String TextEchoTime) {
        this.TextEchoTime = TextEchoTime;
    }

    public void setTextEchoNumber(String TextEchoNumber) {
        this.TextEchoNumber = TextEchoNumber;
    }

    public void setTextSliceThickness(String TextSliceThickness) {
        this.TextSliceThickness = TextSliceThickness;
    }

    public void setTextSlicePosition(String TextSlicePosition) {
        this.TextSlicePosition = TextSlicePosition;
    }

    public void setTextAngleFlag1(String TextAngleFlag1) {
        this.TextAngleFlag1 = TextAngleFlag1;
    }

    public void setTextAngleFlag2(String TextAngleFlag2) {
        this.TextAngleFlag2 = TextAngleFlag2;
    }

    public void setTextAngleFlag3(String TextAngleFlag3) {
        this.TextAngleFlag3 = TextAngleFlag3;
    }

    public void setTextAngle(String TextAngle) {
        this.TextAngle = TextAngle;
    }

    public void setTextFOVH(String TextFOVH) {
        this.TextFOVH = TextFOVH;
    }

    public void setTextFOVV(String TextFOVV) {
        this.TextFOVV = TextFOVV;
    }

    public void setTextTablePosition(String TextTablePosition) {
        this.TextTablePosition = TextTablePosition;
    }

    public void setTextStudyNumber(String TextStudyNumber) {
        this.TextStudyNumber = TextStudyNumber;
    }

    public void setTextDOBDD(String TextDOBDD) {
        this.TextDOBDD = TextDOBDD;
    }

    public void setTextDOBMM(String TextDOBMM) {
        this.TextDOBMM = TextDOBMM;
    }

    public void setTextDOBYYYY(String TextDOBYYYY) {
        this.TextDOBYYYY = TextDOBYYYY;
    }

    public void setTextStudyNumber2(String TextStudyNumber2) {
        this.TextStudyNumber2 = TextStudyNumber2;
    }

    public void setTextImageNumber2(String TextImageNumber2) {
        this.TextImageNumber2 = TextImageNumber2;
    }

    public void setTextStudyImageNumber3(String TextStudyImageNumber3) {
        this.TextStudyImageNumber3 = TextStudyImageNumber3;
    }

    public void setTextModelName(String TextModelName) {
        this.TextModelName = TextModelName;
    }

    public void setTextPatientName(String TextPatientName) {
        this.TextPatientName = TextPatientName;
    }

    public void setTextScanStartTimeHH(String TextScanStartTimeHH) {
        this.TextScanStartTimeHH = TextScanStartTimeHH;
    }

    public void setTextScanStartTimeMM(String TextScanStartTimeMM) {
        this.TextScanStartTimeMM = TextScanStartTimeMM;
    }

    public void setTextScanStartTimeSS(String TextScanStartTimeSS) {
        this.TextScanStartTimeSS = TextScanStartTimeSS;
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

        dialog.append("Siemens Study Date Year: \t"   + SiemensStudyDateYYYY  + "\n");
        dialog.append("Siemens Study Date Month: \t"  + SiemensStudyDateMM    + "\n");
        dialog.append("Siemens Study Date Day: \t"    + SiemensStudyDateDD    + "\n");
        dialog.append("Siemens Study Time Hour: \t"   + SiemensStudyTimeHH    + "\n");
        dialog.append("Siemens Study Time Minute: \t" + SiemensStudyTimeMM    + "\n");
        dialog.append("Siemens Study Time Second: \t" + SiemensStudyTimeSS    + "\n");
        dialog.append("Acquisition Date Year: \t\t"   + AcquisitionDateYYYY   + "\n");
        dialog.append("Acquisition Date Month:\t\t"   + AcquisitionDateMM     + "\n");
        dialog.append("Acquisition Date Day: \t\t"    + AcquisitionDateDD     + "\n");
        dialog.append("Acquisition Time Hour: \t\t"   + AcquisitionTimeHH     + "\n");
        dialog.append("Acquisition Time Minute: \t"   + AcquisitionTimeMM     + "\n");
        dialog.append("Acquisition Time Second: \t"   + AcquisitionTimeSS     + "\n");
        dialog.append("Image Date Year: \t\t"         + ImageDateYYYY         + "\n");
        dialog.append("Image Date Month:\t\t"         + ImageDateMM           + "\n");
        dialog.append("Image Date Day: \t\t"          + ImageDateDD           + "\n");
        dialog.append("Image Time Hour: \t\t"         + ImageTimeHH           + "\n");
        dialog.append("Image Time Minute: \t\t"       + ImageTimeMM           + "\n");
        dialog.append("Image Time Second: \t\t"       + ImageTimeSS           + "\n");

        if (Manufacturer != null) {
            dialog.append("Manufacturer: \t\t"        + Manufacturer + "\n");
        }
        if (InstitutionName != null) {
            dialog.append("Institution Name: \t\t"    + InstitutionName + "\n");
        }
        if (Annotation != null) {
            dialog.append("Annotation: \t\t"          + Annotation + "\n");
        }
        if (ModelName != null) {
            dialog.append("Model Name: \t\t"          + ModelName + "\n");
        }
        dialog.append("Last Move Date Year: \t"       + LastMoveDateYYYY + "\n");
        dialog.append("Last Move Date Month: \t"      + LastMoveDateMM + "\n");
        dialog.append("Last Move Date Day: \t"        + LastMoveDateDD + "\n");
        dialog.append("Last Move Time Hour: \t"       + LastMoveTimeHH + "\n");
        dialog.append("Last Move Time Minute: \t"     + LastMoveTimeMM + "\n");
        dialog.append("Last Move Time Second: \t"     + LastMoveTimeSS + "\n");
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
        dialog.append("Slice Thickness: " + SliceThickness + "\n");
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

}
