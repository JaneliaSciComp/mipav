package gov.nih.mipav.model.file;


import java.util.ArrayList;
import java.util.HashMap;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogFileInfo;
import gov.nih.mipav.view.dialogs.JDialogEditor;
import gov.nih.mipav.view.dialogs.JDialogText;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelImage;


/**
 * Created by IntelliJ IDEA. User: bennett Date: Mar 16, 2007 Time: 9:18:41 AM To change this template use File |
 * Settings | File Templates.
 * Updated September 16, 2011 by Beth Tyrie
 */
public class FileInfoPARREC extends FileInfoBase {

    /** vol parameters **/
    private static HashMap<String, String> VolParameters;

    /** slice specific info **/
    private String sliceInfo;

    /** par/rec version **/
    private String version;

    /** par/rec date **/
    private String date;

    /** par/rec examName **/
    private String examName;

    /** par/rec protocolName **/
    private String protocolName;

    /** par/rec patientPosition **/
    private String patientPosition;

    /** par/rec PreparationDirection **/
    private String foldover;

    /** par/rec slice Angulation **/
    private double[] sliceAng;

    /** Off-centre translation **/
    private double[] offCentre;

    /** par/rec Bvalues **/
    private String stringBvalueArray[];

    /** par/rec gradients **/
    private String stringGradientArray[];

    /** Slice Orientation Value (sag/cor/tra) **/
    private int firstOrientValue;

    /** numSlices **/
    private int numSlices;

    /** numSlices **/
    private int numVolumes;

    private ArrayList<String> generalInfoList = new ArrayList<String>();

    private ArrayList<String> imageInfoList = new ArrayList<String>();

    // default constructor
    public FileInfoPARREC(String name, String directory, int format) {
        super(name, directory, format);
    }

    public void displayDTIInfo_JDialogText(JDialogText dlg) {
        String ori0, ori1, ori2;
        
        ori0 = FileInfoBase.axisOrientationStr[getAxisOrientation(0)];
        ori1 = FileInfoBase.axisOrientationStr[getAxisOrientation(1)];
        ori2 = FileInfoBase.axisOrientationStr[getAxisOrientation(2)];
        
        if(sliceAng != null) {
            dlg.append(ori0+" angulation: "+ Double.toString(sliceAng[0])+ "\n");
            dlg.append(ori1+" angulation: "+ Double.toString(sliceAng[1])+ "\n");
            dlg.append(ori2+" angulation: "+ Double.toString(sliceAng[2])+ "\n");
        }
        
        if(offCentre != null) {
            dlg.append(ori0+" off centre: "+ Double.toString(offCentre[0])+ "\n");
            dlg.append(ori1+" off centre: "+ Double.toString(offCentre[1])+ "\n");
            dlg.append(ori2+" off centre: "+ Double.toString(offCentre[2])+ "\n");
        }
        
        dlg.append("PAR/REC Version: " + getVersion() + "\n");
        dlg.append("Date: " + getDate() + "\n");
        dlg.append("Exam Name: " + getExamName() + "\n");
        dlg.append("Protocol Name: " + getProtocolName() + "\n");
    }

    public void displayDTIInfo_JDialogFileInfo(JDialogFileInfo dlg) {

    }

    // required by FileInfoBase
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogFileInfo dialog;
        try {
            dialog = (JDialogFileInfo) dlog;
        } catch (Exception e) {
            displayPrimaryInfo((JDialogText) dlog, matrix);
            displayDTIInfo_JDialogText((JDialogText) dlog);
            return;
        }

        int[] extents;
        int i;
        int[] editorChoice = new int[1];
        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo(this); // setup layout in the dialog

        extents = super.getExtents();

        for (i = 0; i < extents.length; i++) {
            dialog.appendPrimaryData("Dimension " + i, Integer.toString(extents[i]));
        }

        dialog.appendPrimaryData("Type", ModelStorageBase.getBufferTypeStr(getDataType()));

        if (ModelImage.isColorImage(getDataType())) {
            dialog.appendPrimaryData("Min red", Double.toString(getMinR()));
            dialog.appendPrimaryData("Max red", Double.toString(getMaxR()));
            dialog.appendPrimaryData("Min green", Double.toString(getMinG()));
            dialog.appendPrimaryData("Max green", Double.toString(getMaxG()));
            dialog.appendPrimaryData("Min blue", Double.toString(getMinB()));
            dialog.appendPrimaryData("Max blue", Double.toString(getMaxB()));

        } else {
            dialog.appendPrimaryData("Min", Double.toString(getMin()));
            dialog.appendPrimaryData("Max", Double.toString(getMax()));
        }

        dialog.appendPrimaryData("Modality", FileInfoBase.getModalityStr(getModality()));

        dialog.appendPrimaryData("Version", getVersion());

        dialog.appendPrimaryData("Orientation", getImageOrientationStr(getImageOrientation()));

        float[] resolutions; // = new float[5];
        resolutions = getResolutions();

        int[] measure; // = new int[5];
        measure = getUnitsOfMeasure();

        for (i = 0; i < extents.length; i++) {

            if (resolutions[i] > 0.0) {
                String pixelRes = "Pixel resolution " + i;
                dialog.appendPrimaryData(pixelRes,
                        Float.toString(resolutions[i]) + " " + (Unit.getUnitFromLegacyNum(measure[i])).toString());
            } // end of if (resolutions[i] > 0.0)
        } // for (i=0; i < 5; i++)

        if (getEndianess() == FileBase.LITTLE_ENDIAN) {
            dialog.appendPrimaryData("Endianess", "Little Endian");
        } else {
            dialog.appendPrimaryData("Endianess", "Big Endian");
        }
        
        
        displayDTIInfo_JDialogFileInfo(dialog);
    }

    /** getter for vol parameters **/
    public HashMap<String, String> getVolParameters() {
        return VolParameters;
    }

    public void setVolParameters(HashMap<String, String> volParameters) {
        VolParameters = volParameters;
    }

    public String getSliceInfo() {
        return sliceInfo;
    }

    public void setSliceInfo(String sliceInfo) {
        this.sliceInfo = sliceInfo;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getExamName() {
        return examName;
    }

    public void setExamName(String examName) {
        this.examName = examName;
    }

    public String getProtocolName() {
        return protocolName;
    }

    public void setProtocolName(String protocolName) {
        this.protocolName = protocolName;
    }

    public String getPatientPosition() {
        return patientPosition;
    }

    public void setPatientPosition(String patientPosition) {
        this.patientPosition = patientPosition;
    }

    public String getPreparationDirection() {
        return foldover;
    }

    public void setPreparationDirection(String foldover) {
        this.foldover = foldover;
    }

    public void setSliceAngulation(double[] sliceAng) {
        this.sliceAng = sliceAng;
    }

    public double[] getSliceAngulation() {
        return sliceAng;
    }

    public void setOffCentre(double[] offCentre) {
        this.offCentre = offCentre;
    }

    public double[] getOffCentre() {
        return offCentre;
    }

    public int getNumSlices() {
        return numSlices;
    }

    public void setNumSlices(int numSlices) {
        this.numSlices = numSlices;
    }

    public int getSliceOrient() {
        return firstOrientValue;
    }

    public void setSliceOrient(int firstOrientValue) {
        this.firstOrientValue = firstOrientValue;
    }

    public int getNumVolumes() {
        return numVolumes;
    }

    public void setNumVolumes(int numVolumes) {
        this.numVolumes = numVolumes;
    }  

    public ArrayList<String> getGeneralInfoList() {
        return generalInfoList;
    }

    public void setGeneralInfoList(String info) {
        generalInfoList.add(info);
    }

    public ArrayList<String> getImageInfoList() {
        return imageInfoList;
    }

    public void setImageInfoList(String info) {
        imageInfoList.add(info);
    }

    /*
     * public String getDate(){ return date; }
     */
    /*
     * public void setDate(String date) { this.date = date; }
     */

}
