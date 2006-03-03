package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.io.*;

import java.awt.Dialog.*;


/**
*   Class that reads Siemens Magnetom Vision files.
*/
public class FileMagnetomVision extends FileBase {

    private String                  fileName;
    private String                  fileDir;
    private File                    fileHeader;
    private FileInfoMagnetomVision  fileInfo;
    private float                   imgResols[] = new float[] {1.0f,1.0f,1.0f,1.0f,1.0f};
    private int                     imgExtents[]= new int[2];
    private int                     orient[]    = new int[3];

    /**
    *   Magnetom Vision reader/writer constructor
    *   @param fileName         file name
    *   @param fileDir          file directory
    *   @exception IOException  if there is an error making the file
    */
    public FileMagnetomVision(String fileName, String fileDir) throws IOException {
        this.fileName   = fileName;
        this.fileDir    = fileDir;
    }

    /**
    *  Accessor that sets the file name and allocates new FileInfo, File and RandomAccess file objects.
    *  @param fName             File name.
    *  @exception IOException   if there is an error constructing the files.
    */
    public void setFileName(String fName) throws IOException {
        fileName    = fName;
        try {
            if (raFile != null) {
                raFile.close();
            }
            fileHeader  = new File(fileDir + fileName);
            raFile      = new RandomAccessFile(fileHeader, "r");
            fileInfo    = new FileInfoMagnetomVision(fileName, fileDir, FileBase.MAGNETOM_VISION);
        }
        catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileGEGenesis.setFileName.");
            throw new IOException();
        }
        fileInfo.setEndianess(BIG_ENDIAN);
        fileInfo.setFileName(fileName);
    }


    /**
    *   Returns null.
    *   @return null.
    */
    public ModelLUT getModelLUT(){ return null;}


    /**
    *   Reads the Magnetom file header and data
    *   @return Returns an object containing the file info.
    *   @exception IOException if there is an error reading the file
    */
    public FileInfoMagnetomVision readHeader() throws IOException {
        // The data types are Sun, hence the byte order is big-endian and all the floats
        // seem to be doubles.  The uncompressed image data starts at offset 6144.
        String  s;
        int     temp;
        float   temp2;

        try {

            fileInfo.setSiemensStudyDateYYYY(raFile.readInt());     // location 0
            fileInfo.setSiemensStudyDateMM(raFile.readInt());       // location 4
            fileInfo.setSiemensStudyDateDD(raFile.readInt());       // location 8
            fileInfo.setAcqusitionDateYYYY(raFile.readInt());       // location 12
            fileInfo.setAcquisitionDateMM(raFile.readInt());        // location 16
            fileInfo.setAcquisitionDateDD(raFile.readInt());        // location 20
            fileInfo.setImageDateYYYY(raFile.readInt());            // location 24
            fileInfo.setImageDateMM(raFile.readInt());              // location 28
            fileInfo.setImageDateDD(raFile.readInt());              // location 32
            fileInfo.setSiemensStudyTimeHH(raFile.readInt());       // location 36
            fileInfo.setSiemensStudyTimeMM(raFile.readInt());       // location 40
            fileInfo.setSiemensStudyTimeSS(raFile.readInt());       // location 44
            raFile.seek(52L);
            fileInfo.setAcquisitionTimeHH(raFile.readInt());        // location 52
            fileInfo.setAcquisitionTimeMM(raFile.readInt());        // location 56
            fileInfo.setAcquisitionTimeSS(raFile.readInt());        // location 60
            raFile.seek(68L);
            fileInfo.setImageTimeHH(raFile.readInt());              // location 68
            fileInfo.setImageTimeMM(raFile.readInt());              // location 72
            fileInfo.setImageTimeSS(raFile.readInt());              // location 76
            raFile.seek(96L);
            s = getString(7);
            fileInfo.setManufacturer(s); // Should be SIEMENS
            raFile.seek(105L);
            s = getString(25);
            fileInfo.setInstitutionName(s);
            raFile.seek(186L);
            s = getString(4);
            fileInfo.setAnnotation(s.trim());
            raFile.seek(281L);
            s = getString(15);
            fileInfo.setModelName(s); // should be MAGNETOM VISION
            raFile.seek(412L);
            fileInfo.setLastMoveDateYYYY(raFile.readInt());         // location 412
            fileInfo.setLastMoveDateMM(raFile.readInt());           // location 416
            fileInfo.setLastMoveDateDD(raFile.readInt());           // location 420
            fileInfo.setLastMoveTimeHH(raFile.readInt());           // location 424
            fileInfo.setLastMoveTimeMM(raFile.readInt());           // location 428
            fileInfo.setLastMoveTimeSS(raFile.readInt());           // location 432
            raFile.seek(768L);
            s = getString(25);
            fileInfo.setPatientName(s.trim());
            raFile.seek(795L);
            s = getString(12);
            fileInfo.setPatientID(s.trim());
            raFile.seek(808L);
            fileInfo.setDOBYYYY(raFile.readInt());                  // location 808
            fileInfo.setDOBMM(raFile.readInt());                    // location 812
            fileInfo.setDOBDD(raFile.readInt());                    // location 816
            raFile.seek(851L);
            s = getString(3);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                MipavUtil.displayWarning("Instead of integer PatientAge had " + s);
            }
            fileInfo.setPatientAge(s.trim());
            raFile.seek(854L);
            s = getString(1);
            fileInfo.setPatientAgeUnits(s); // 'Y' = years
            raFile.seek(1052L);
            fileInfo.setRegistrationDateYYYY(raFile.readInt());     // location 1052
            fileInfo.setRegistrationDateMM(raFile.readInt());       // location 1056
            fileInfo.setRegistrationDateDD(raFile.readInt());       // location 1060
            fileInfo.setRegistrationTimeHH(raFile.readInt());       // location 1064
            fileInfo.setRegistrationTimeMM(raFile.readInt());       // location 1068
            fileInfo.setRegistrationTimeSS(raFile.readInt());       // location 1072
            raFile.seek(1544L);
            fileInfo.setSliceThickness(raFile.readDouble());
            raFile.seek(1560L);
            fileInfo.setRepetitionTime(raFile.readDouble());
            fileInfo.setEchoTime(raFile.readDouble());              // location 1568
            raFile.seek(1592L);
            fileInfo.setFrequencyMHz(raFile.readDouble());
            raFile.seek(1639L);
            s = getString(5);
            fileInfo.setStation(s);
            raFile.seek(1712L);
            fileInfo.setCalibrationDateYYYY(raFile.readInt());      // location 1712
            fileInfo.setCalibrationDateMM(raFile.readInt());        // location 1716
            fileInfo.setCalibrationDateDD(raFile.readInt());        // location 1720
            fileInfo.setCalibrationTimeHH(raFile.readInt());        // location 1724
            fileInfo.setCalibrationTimeMM(raFile.readInt());        // location 1728
            fileInfo.setCalibrationTimeSS(raFile.readInt());        // location 1732
            raFile.seek(1767L);
            s = getString(16);
            fileInfo.setReceivingCoil(s);
            raFile.seek(1828L);
            s = getString(4);
            fileInfo.setImagedNucleus(s.trim());
            raFile.seek(2112L);
            fileInfo.setFlipAngle(raFile.readDouble());
            raFile.seek(2560L);
            fileInfo.setMagneticFieldStrength(raFile.readDouble());
            raFile.seek(2864L);
            fileInfo.setDisplayMatrixSize(raFile.readInt());
            raFile.seek(2944L);
            s = getString(65);
            fileInfo.setSequencePrgName(s);
            raFile.seek(3009L);
            s = getString(65);
            fileInfo.setSequenceWkcName(s);
            raFile.seek(3074L);
            s = getString(9);
            fileInfo.setSequenceAuthor(s.trim());
            raFile.seek(3083L);
            s = getString(8);
            fileInfo.setSequenceType(s);
            raFile.seek(3744L);
            fileInfo.setFOVRow(raFile.readDouble());
            fileInfo.setFOVCOlumn(raFile.readDouble());     // location 3752
            raFile.seek(3768L);
            fileInfo.setCenterPointX(raFile.readDouble());
            fileInfo.setCenterPointY(raFile.readDouble());  // location 3776
            fileInfo.setCenterPointZ(raFile.readDouble());  // location 3784
            fileInfo.setNormalVectorX(raFile.readDouble()); // location 3792
            fileInfo.setNormalVectorY(raFile.readDouble()); // location 3800
            fileInfo.setNormalVectorZ(raFile.readDouble()); // location 3808
            fileInfo.setDistanceFromIsocenter(raFile.readDouble()); // location 3816
            raFile.seek(3832L);
            fileInfo.setRowVectorX(raFile.readDouble());
            fileInfo.setRowVectorY(raFile.readDouble());    // location 3840
            fileInfo.setRowVectorZ(raFile.readDouble());    // location 3848
            fileInfo.setColumnVectorX(raFile.readDouble()); // location 3856
            fileInfo.setColumnVectorY(raFile.readDouble()); // location 3864
            fileInfo.setColumnVectorZ(raFile.readDouble()); // location 3872


            s = getString(3); // location 3880
            s = s.trim();
            if (s.substring(0,1).equals("R")) {
                orient[1] = FileInfoBase.ORI_R2L_TYPE;
            }
            else if (s.substring(0,1).equals("L")) {
                orient[1] = FileInfoBase.ORI_L2R_TYPE;
            }
            else if (s.substring(0,1).equals("A")) {
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
            }
            else if (s.substring(0,1).equals("P")) {
                orient[1] = FileInfoBase.ORI_P2A_TYPE;
            }
            else if (s.substring(0,1).equals("F")) { //F for feet for inferior
                orient[1] = FileInfoBase.ORI_I2S_TYPE;
            }
            else if (s.substring(0,1).equals("H")) { // H for head for superior
                orient[1] = FileInfoBase.ORI_S2I_TYPE;
            }
            else {
                orient[1] = FileInfoBase.ORI_UNKNOWN_TYPE;
            }
            fileInfo.setOrientationSet1Top(s);


            raFile.seek(3884L);
            s = getString(3);
            s = s.trim();
            if (s.substring(0,1).equals("R")) {
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
            }
            else if (s.substring(0,1).equals("L")) {
                orient[0] = FileInfoBase.ORI_L2R_TYPE;
            }
            else if (s.substring(0,1).equals("A")) {
                orient[0] = FileInfoBase.ORI_A2P_TYPE;
            }
            else if (s.substring(0,1).equals("P")) {
                orient[0] = FileInfoBase.ORI_P2A_TYPE;
            }
            else if (s.substring(0,1).equals("F")) { // F for feet for inferior
                orient[0] = FileInfoBase.ORI_I2S_TYPE;
            }
            else if (s.substring(0,1).equals("H")) { // H for head for superior
                orient[0] = FileInfoBase.ORI_S2I_TYPE;
            }
            else {
                orient[0] = FileInfoBase.ORI_UNKNOWN_TYPE;
            }
            fileInfo.setOrientationSet1Left(s);


            raFile.seek(3888L);
            s = getString(3);
            fileInfo.setOrientationSet1Back(s);
            raFile.seek(3892L);
            s = getString(3);
            fileInfo.setOrientationSet2Down(s);
            raFile.seek(3896L);
            s = getString(3);
            fileInfo.setOrientationSet2Right(s);


            raFile.seek(3900L);
            s = getString(3);
            s = s.trim();
            if (s.substring(0,1).equals("L")) {
                orient[2] = FileInfoBase.ORI_R2L_TYPE;
            }
            else if (s.substring(0,1).equals("R")) {
                orient[2] = FileInfoBase.ORI_L2R_TYPE;
            }
            else if (s.substring(0,1).equals("P")) {
                orient[2] = FileInfoBase.ORI_A2P_TYPE;
            }
            else if (s.substring(0,1).equals("A")) {
                orient[2] = FileInfoBase.ORI_P2A_TYPE;
            }
            else if (s.substring(0,1).equals("H")) { // H for head for superior
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
            }
            else if (s.substring(0,1).equals("F")) { // F for feet for inferior
                orient[2] = FileInfoBase.ORI_S2I_TYPE;
            }
            else {
                orient[2] = FileInfoBase.ORI_UNKNOWN_TYPE;
            }
            fileInfo.setOrientationSet2Front(s);
            fileInfo.setAxisOrientation(orient);


            raFile.seek(3904L);
            s = getString(32);
            fileInfo.setSequenceName(s.trim());

            raFile.seek(5000L);
            double pixelSize = raFile.readDouble();
            imgResols[0] = (float)pixelSize;
            pixelSize     = raFile.readDouble(); // location 5008
            imgResols[1] = (float)pixelSize;
            imgResols[2] = (float)fileInfo.getSliceThickness();

            fileInfo.setResolutions(imgResols);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS,0);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS,1);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS,2);

            raFile.seek(5504L);
            s = getString(12);
            fileInfo.setTextPatientID(s.trim());
            raFile.seek(5517L);
            s = getString(1);
            fileInfo.setTextPatientSex(s);

            raFile.seek(5518L);
            s = getString(3);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                //MipavUtil.displayWarning("Instead of integer TextPatientAge has " + s);
                Preferences.debug("Instead of integer TextPatientAge has " + s + "\n");
            }

            fileInfo.setTextPatientAge(s);
            raFile.seek(5521L);
            s = getString(1);
            fileInfo.setTextPatientAgeUnits(s); // 'Y' = years
            raFile.seek(5529L);
            s = getString(7);
            fileInfo.setTextPatientPosition(s.trim());
            raFile.seek(5541L);
            s = getString(5);
            if (!s.equals("IMAGE")) {
                Preferences.debug("Instead of IMAGE location 5541 has " + s + "\n");
            }
            raFile.seek(5546L);
            s = getString(3);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextImageNumber has " + s + "\n");
            }
            fileInfo.setTextImageNumber(s.trim());
            raFile.seek(5559L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextDateDD has " + s + "\n");
            }
            fileInfo.setTextDateDD(s);
            raFile.seek(5562L);
            s = getString(3);
            fileInfo.setTextDateMM(s);
            raFile.seek(5566L);
            s = getString(4);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextDateYYYY has " + s + "\n");
            }
            fileInfo.setTextDateYYYY(s);
            raFile.seek(5571L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextTimeHH has " + s + "\n");
            }
            fileInfo.setTextTimeHH(s);
            raFile.seek(5574L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextTimeMM has " + s + "\n");
            }
            fileInfo.setTextTimeMM(s);
            raFile.seek(5577L);
            s = getString(2);
            if (!s.equals("TA")) {
                Preferences.debug("Instead of TA location 5577 has " + s + "\n");
            }

            raFile.seek(5583L);
            s = getString(2);
            try {
               temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextAcquisitionTimeMM has " + s + "\n");
            }
            fileInfo.setTextAcquisitionTimeMM(s.trim());

            raFile.seek(5586L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextAcquisitionTimeSS has " + s + "\n");
            }
            fileInfo.setTextAcquisitionTimeSS(s.trim());

            raFile.seek(5601L);
            s = getString(4);
            fileInfo.setTextAnnotation(s.trim());
            raFile.seek(5655L);
            s = getString(25);
            fileInfo.setTextOrganization(s.trim());
            raFile.seek(5682L);
            s = getString(5);
            fileInfo.setTextStation(s);

            raFile.seek(5695L);
            s = getString(3);
            try {
                temp2 = Float.parseFloat(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of float TextAcquisitionMatrixPhase has " + s + "\n");
            }
            fileInfo.setTextAcquisitionMatrixPhase(s.trim());

            raFile.seek(5698L);
            s = getString(1);
            fileInfo.setTextAcquisitionMatrixPhaseAxis(s); // 'h' = horizontal, ' ' = vertical

            raFile.seek(5700L);
            s = getString(3);
            try {
                temp2 = Float.parseFloat(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of float TextAcquisitionMatrixFreq has " + s + "\n");
            }
            fileInfo.setTextAcquisitionMatrixFreq(s.trim());

            raFile.seek(5703L);
            s = getString(1);
            fileInfo.setTextAcquisitionMatrixFreqO(s); // 'o' = o, ' ' = blank
            raFile.seek(5704L);
            s = getString(1);
            fileInfo.setTextAcquisitionMatrixFreqS(s); // 's' = s, ' ' = blank
            raFile.seek(5706L);
            s = getString(8);
            fileInfo.setTextSequence(s);
            raFile.seek(5714L);
            s = getString(3);

            try {
                temp2 = Float.parseFloat(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of float TextFlipAngle has " + s + "\n");
            }
            fileInfo.setTextFlipAngle(s.trim());

            raFile.seek(5718L);
            s = getString(4);
            if (!s.equals("SCAN")) {
                Preferences.debug("Instead of SCAN location 5718 has " + s + "\n");
            }
            raFile.seek(5723L);
            s = getString(3);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextScanNumberA has " + s + "\n");
            }
            fileInfo.setTextScanNumberA(s.trim());

            raFile.seek(5726L);
            s = getString(3);
            fileInfo.setTextScanNumberB(s.trim());

            raFile.seek(5730L);
            s = getString(2);
            if (!s.equals("TR")) {
                Preferences.debug("Instead of TR location 5730 has " + s + "\n");
            }
            raFile.seek(5734L);
            s = getString(7);
            try {
                temp2 = Float.parseFloat(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of float TextRepetitionTime has " + s + "\n");
            }
            fileInfo.setTextRepetitionTime(s.trim());

            raFile.seek(5742L);
            s = getString(2);
            if (!s.equals("TE")) {
                Preferences.debug("Instead of TE location 5742 has " + s + "\n");
            }

            raFile.seek(5746L);
            s = getString(5);
            try {
                temp2 = Float.parseFloat(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of float TextEchoTime has " + s + "\n");
            }
            fileInfo.setTextEchoTime(s.trim());

            raFile.seek(5752);
            s = getString(1);
            try {
                temp = Integer.parseInt(s);
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextEchoNumber has " + s + "\n");
            }
            fileInfo.setTextEchoNumber(s.trim());

            raFile.seek(5790L);
            s = getString(2);
            if (!s.equals("SL")) {
                Preferences.debug("Instead of SL location 5790 has " + s + "\n");
            }

            raFile.seek(5794L);
            s = getString(7);
            try {
                temp2 = Float.parseFloat(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of float TextSliceThickness has " + s + "\n");
            }

            fileInfo.setTextSliceThickness(s.trim());
            raFile.seek(5802L);
            s = getString(2);
            if (!s.equals("SP")) {
                Preferences.debug("Instead of SP location 5802 has " + s + "\n");
            }

            raFile.seek(5806L);
            s = getString(7);
            try {
                temp2 = Float.parseFloat(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of float TextSlicePosition has " + s + "\n");
            }
            fileInfo.setTextSlicePosition(s.trim());

            float[] locations = new float[3];
            locations[0] = -imgResols[0]*imgExtents[0]/2;
            locations[1] = -imgResols[1]*imgExtents[1]/2;

            raFile.seek(5814L);
            s = getString(3);
            if (s.equals("Sag")) {
                fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                locations[0] += fileInfo.getCenterPointY();
                locations[1] += fileInfo.getCenterPointZ();
                locations[2]  = (float)fileInfo.getCenterPointX();
            }
            else if (s.equals("Cor")) {
                fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                locations[0] += fileInfo.getCenterPointX();
                locations[1] += fileInfo.getCenterPointZ();
                locations[2]  = (float)fileInfo.getCenterPointY();
            }
            else if (s.equals("Tra")) {
                fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                locations[0] += fileInfo.getCenterPointX();
                locations[1] += fileInfo.getCenterPointY();
                locations[2]  = (float)fileInfo.getCenterPointZ();
            }
            else {
                fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
                locations[0] = 0;
                locations[1] = 0;
                locations[2] = 0;
            }
            fileInfo.setOrigin(locations);
            fileInfo.setTextAngleFlag1(s); // 'Sag' = sagittal, 'Cor' = coronal, 'Tra' = axial
            raFile.seek(5817L);
            s = getString(1);
            fileInfo.setTextAngleFlag2(s); // '>' = gt, '<' = lt
            raFile.seek(5818L);
            s = getString(3);
            fileInfo.setTextAngleFlag3(s); // 'Sag' = sagittal, 'Cor' = coronal
            raFile.seek(5821L);
            s = getString(4);
            fileInfo.setTextAngle(s.trim());

            raFile.seek(5838L);
            s = getString(3); // Look for TextFOVFLAG, FoV, the field of view flag
            if (!s.equals("FoV")) {
                Preferences.debug("Instead of FoV location 5838 has " + s + "\n");
            }
            raFile.seek(5842L); // Look for TextFOVH, the horizontal distance of the field of view
            s = getString(3);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextFOVH has " + s);
            }
            fileInfo.setTextFOVH(s);

            raFile.seek(5846L); // Look for TextFOVV, the vertical distance of the field of view
            s = getString(3);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextFOVV has " + s + "\n");
            }
            fileInfo.setTextFOVV(s);
            raFile.seek(5874L);
            s = getString(2);
            if (!s.equals("TP")) {
                Preferences.debug("Instead of TP location 5874 has " + s + "\n");
            }
            raFile.seek(5878L);
            s = getString(7);
            try {
                temp2 = Float.parseFloat(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of float TextTablePosition has " + s + "\n");
            }
            fileInfo.setTextTablePosition(s.trim());
            raFile.seek(5938L);
            s = getString(5);
            if (!s.equals("STUDY")) {
                Preferences.debug("Instead of STUDY location 5938 has " + s + "\n");
            }
            raFile.seek(5943L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextStudyNumber has " + s + "\n");
            }
            fileInfo.setTextStudyNumber(s.trim());
            raFile.seek(5956L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextDOBDD has " + s + "\n");
            }
            fileInfo.setTextDOBDD(s.trim());
            raFile.seek(5959L);
            s = getString(3);
            fileInfo.setTextDOBMM(s);
            raFile.seek(5963L);
            s = getString(4);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextDOBYYYY has " + s + "\n");
            }
            fileInfo.setTextDOBYYYY(s.trim());
            raFile.seek(5992L);
            s = getString(3);
            if (!s.equals("STU")) {
                Preferences.debug("Instead of STU location 5992 has " + s + "\n");
            }
            raFile.seek(5996L);
            s = getString(3);
            if (!s.equals("IMA")) {
                Preferences.debug("Instead of IMA location 5996 has " + s + "\n");
            }
            raFile.seek(5999L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextStudyNumber2 has " + s + "\n");
            }
            fileInfo.setTextStudyNumber2(s.trim());
            raFile.seek(6002L);
            s = getString(2);
            fileInfo.setTextImageNumber2(s.trim());
            raFile.seek(6013L);
            s = getString(5);
            fileInfo.setTextStudyImageNumber3(s.trim());
            raFile.seek(6031L);
            s = getString(15);
            fileInfo.setTextModelName(s.trim());
            raFile.seek(6058L);
            s = getString(25);
            fileInfo.setTextPatientName(s.trim());
            raFile.seek(6085L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextScanStartTimeHH has " + s + "\n");
            }
            fileInfo.setTextScanStartTimeHH(s.trim());
            raFile.seek(6088L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextScanStartTimeMM has " + s + "\n");
            }
            fileInfo.setTextScanStartTimeMM(s.trim());
            raFile.seek(6091L);
            s = getString(2);
            try {
                temp = Integer.parseInt(s.trim());
            }
            catch(NumberFormatException e) {
                Preferences.debug("Instead of integer TextScanStartTimeSS has " + s + "\n");
            }
            fileInfo.setTextScanStartTimeSS(s.trim());

            imgExtents[0] = (int)Math.round(Math.sqrt(((raFile.length() - 6144)/2)));
            imgExtents[1] = imgExtents[0];

            fileInfo.setDataType(ModelStorageBase.SHORT);
            fileInfo.setExtents(imgExtents);
        }
        catch (Exception e) {
            System.gc();
            throw new IOException();
        }
        return fileInfo;
    }


    /**
    *   Reads a slice of data at a time and stores the results in the buffer
    *   @param buffer           Buffer where the image is stored
    *   @exception IOException  if there is an error reading the file
    */
    public void readImage(short buffer[]) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2;
        byte [] byteBuffer;
        byteBuffer =  new byte[2*buffer.length];
        nBytes = 2*buffer.length;
        raFile.seek(6144L); // location of start of uncompressed data

        raFile.read(byteBuffer, 0, nBytes);
        for (j = 0; j < nBytes; j+=2, i++ ) {
            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j+1);
            buffer[i] = (short)((b1 << 8) + b2);
        } // for (j = 0; j < nBytes; j+=2, i++ )
    }

    /**
    *   Closes random access file.
    */
    public void close() throws IOException {
        raFile.close();
    }

}
