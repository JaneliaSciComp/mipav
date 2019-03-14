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
    
    private String time;
    
    private String patientName = null;

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
    
    private String repetitionTime = null;

    /** Slice Orientation Value (sag/cor/tra) **/
    private int firstOrientValue;

    /** numSlices **/
    private int numSlices;

    /** numSlices **/
    private int numVolumes;

    private ArrayList<String> generalInfoList = new ArrayList<String>();

    private ArrayList<String> imageInfoList = new ArrayList<String>();
    
    private int originalDataType = ModelStorageBase.FLOAT;
    
    private float scaleSlope = 1.0f;
    
    private int echoNumber[] = null;
    
    private int dynamicScanNumber[] = null;
    
    private int cardiacPhaseNumber[] = null;
    
    private int imageTypeMR[] = null;
    
    private int scanningSequence[] = null;
    
    private int scanPercentage[] = null;
    
    private int windowCenter[]= null;
    
    private int windowWidth[] = null;
    
    private float imageAngulation[][] = null;
    
    private float imageOffcentre[][] = null;
    
    private int fmriStatusIndication[] = null;
    
    private int imageTypeEDES[] = null;
    
    private float echoTime[] = null;
    
    private float dynamicScanBeginTime[] = null;
    
    private float triggerTime[] = null;
    
    private int numberOfAverages[] = null;
    
    private float flipAngle[] = null;
    
    private int cardiacFrequency[] = null;
    
    private int minimumRRInterval[] = null;
    
    private int maximumRRInterval[] = null;
    
    private int turboFactor[] = null;
    
    private float inversionDelay[] = null;
    
    private float diffusionBFactor[] = null;
    
    private int diffusionBValueNumber[] = null;
    
    private int gradientOrientationNumber[] = null;
    
    private String contrastType[] = null;
    
    private String diffusionAnisotropyType[] = null;
    
    private float diffusion[][] = null;
    
    private int labelType[] = null;
    
    private String contrastBolusAgent[] = null;
    
    private float contrastBolusRoute[] = null;
    
    private String contrastBolusVolume[] = null;
    
    private String contrastBolusStartTime[] = null;
    
    private float contrastBolusTotalDose[] = null;
    
    private String contrastBolusIngredient[] = null;
    
    private float contrastBolusIngredientConcentration[] = null;

    // default constructor
    public FileInfoPARREC(String name, String directory, int format) {
        super(name, directory, format);
    }

    public void displayDTIInfo_JDialogText(JDialogText dlg, int index) {
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
        if (patientName != null) {
            dlg.append("Patient Name: " + patientName + "\n");
        }
        dlg.append("Date: " + getDate() + "\n");
        dlg.append("Time: " + getTime() + "\n");
        dlg.append("Exam Name: " + getExamName() + "\n");
        dlg.append("Protocol Name: " + getProtocolName() + "\n");
        if (repetitionTime != null) {
        	dlg.append("Repetition time (ms): " + getRepetitionTime() + "\n");
        }
        dlg.append("\nValues for slice["+index+"]:\n");
        if (echoNumber != null) {
        	dlg.append("Echo number: " + echoNumber[index] + "\n");
        }
        if (dynamicScanNumber != null) {
        	dlg.append("Dynamic scan number: " + dynamicScanNumber[index] + "\n");
        }
        if (cardiacPhaseNumber != null) {
        	dlg.append("Cardiac phase number: " + cardiacPhaseNumber[index] + "\n");
        }
        if (imageTypeMR != null) {
        	dlg.append("Image type MR: " + imageTypeMR[index] + "\n");
        }
        if (scanningSequence != null) {
        	dlg.append("Scanning sequence: " + scanningSequence[index] + "\n");
        }
        if (scanPercentage != null) {
        	dlg.append("Scan percentage: " + scanPercentage[index] + "\n");
        }
        if (windowCenter != null) {
        	dlg.append("Window center: " + windowCenter[index] + "\n");
        }
        if (windowWidth != null) {
        	dlg.append("Window width: " + windowWidth[index] + "\n");
        }
        if (imageAngulation != null) {
        	dlg.append("Image " + ori0 + " angulation (degrees): " + imageAngulation[index][0] + "\n");
        	dlg.append("Image " + ori1 + " angulation (degrees): " + imageAngulation[index][1] + "\n");
        	dlg.append("Image " + ori2 + " angulation (degrees): " + imageAngulation[index][2] + "\n");
        }
        if (imageOffcentre != null) {
        	dlg.append("Image " + ori0 + " off center (mm): " + imageOffcentre[index][0] + "\n");
        	dlg.append("Image " + ori1 + " off center (mm): " + imageOffcentre[index][1] + "\n");
        	dlg.append("Image " + ori2 + " off center (mm): " + imageOffcentre[index][2] + "\n");
        }
        if (fmriStatusIndication != null) {
        	dlg.append("fmri status indication: " + fmriStatusIndication[index] + "\n");
        }
        if (imageTypeEDES != null) {
        	dlg.append("Image type ed es (end diast/end syst): " + imageTypeEDES[index] + "\n");
        }
        if (echoTime != null) {
        	dlg.append("Echo time (ms): " + echoTime[index] + "\n");
        }
        if (dynamicScanBeginTime != null) {
        	dlg.append("Dynamic scan begin time: " + dynamicScanBeginTime[index] + "\n");
        }
        if (triggerTime != null) {
        	dlg.append("Trigger time (ms): " + triggerTime[index] + "\n");
        }
        if (diffusionBFactor != null) {
        	dlg.append("Diffusion B factor: " + diffusionBFactor[index] + "\n");
        }
        if (numberOfAverages != null) {
            dlg.append("Number of averages: " + numberOfAverages[index] + "\n");	
        }
        if (flipAngle != null) {
        	dlg.append("Flip angle (degrees): " + flipAngle[index] + "\n");
        }
        if (cardiacFrequency != null) {
        	dlg.append("Cardiac frequency (bpm): " + cardiacFrequency[index] + "\n");
        }
        if (minimumRRInterval != null) {
        	dlg.append("Minimum RR interval (ms): " + minimumRRInterval[index] + "\n");
        }
        if (maximumRRInterval != null) {
        	dlg.append("Maximum RR interval (ms): " + maximumRRInterval[index] + "\n");
        }
        if (turboFactor != null) {
        	dlg.append("TURBO factor <0=no turbo>: " + turboFactor[index] + "\n");
        }
        if (inversionDelay != null) {
        	dlg.append("Inversion delay (ms): " + inversionDelay[index] + "\n");
        }
        if (diffusionBValueNumber != null) {
        	dlg.append("Diffsuion B value number: " + diffusionBValueNumber[index] + "\n");
        }
        if (gradientOrientationNumber != null) {
        	dlg.append("Gradient orientation number: " + gradientOrientationNumber[index] + "\n");
        }
        if (contrastType != null) {
        	dlg.append("Contrast type: " + contrastType[index] + "\n");
        }
        if (diffusionAnisotropyType != null) {
        	dlg.append("Diffusion anisotropy type: " + diffusionAnisotropyType[index] + "\n");
        }
        if (diffusion != null) {
        	dlg.append("Diffusion AP: " + diffusion[index][0] + "\n");
        	dlg.append("Diffusion FH: " + diffusion[index][1] + "\n");
        	dlg.append("Diffusion RL: " + diffusion[index][2] + "\n");
        }
        if (labelType != null) {
        	dlg.append("Label type (ASL): " + labelType[index] + "\n");
        }
        
        if (contrastBolusAgent != null) {
        	dlg.append("Contrast Bolus Agent: " + contrastBolusAgent[index] + "\n");
        }
        
        if (contrastBolusRoute != null) {
        	dlg.append("Contrast Bolus Route: " + contrastBolusRoute[index] + "\n");
        }
        
        if (contrastBolusVolume != null) {
        	dlg.append("Contrast Bolus Volume: " + contrastBolusVolume[index] + "\n");
        }
        
        if (contrastBolusStartTime != null) {
        	dlg.append("Contrast Bolus Start Time: " + contrastBolusStartTime[index] + "\n");
        }
        
        if (contrastBolusTotalDose != null) {
        	dlg.append("Contrast Bolus Total Dose: " + contrastBolusTotalDose[index] + "\n");
        }
        
        if (contrastBolusIngredient != null) {
        	dlg.append("Contrast Bolus Ingredient: " + contrastBolusIngredient[index] + "\n");
        }
        
        if (contrastBolusIngredientConcentration != null) {
        	dlg.append("Contrast Bolus Ingredient Concentration: " + contrastBolusIngredientConcentration[index] + "\n");
        }
    }

    public void displayDTIInfo_JDialogFileInfo(JDialogFileInfo dlg) {

    }

    // required by FileInfoBase 
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
    	
    }
    
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix, int index) {
        JDialogFileInfo dialog;
        try {
            dialog = (JDialogFileInfo) dlog;
        } catch (Exception e) {
            displayPrimaryInfo((JDialogText) dlog, matrix);
            displayDTIInfo_JDialogText((JDialogText) dlog, index);
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
    
    public String getTime() {
    	return time;
    }
    
    public void setTime(String time) {
    	this.time = time;
    }
    
    public void setPatientName(String patientName) {
        this.patientName = patientName;
    }
    
    public String getPatientName() {
    	return patientName;
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
    
    public void setOriginalDataType(int originalDataType) {
        this.originalDataType = originalDataType;
    }
    
    public int getOriginalDataType() {
        return originalDataType;
    }
    
    public void setScaleSlope(float scaleSlope) {
        this.scaleSlope = scaleSlope;
    }
    
    public float getScaleSlope() {
        return scaleSlope;
    }
    
    public void setRepetitionTime(String repetitionTime) {
    	this.repetitionTime = repetitionTime;
    }
    
    public String getRepetitionTime() {
    	return repetitionTime;
    }
    
    public void setEchoNumber(int echoNumber[]) {
    	this.echoNumber = echoNumber;
    }
    
    public int[] getEchoNumber() {
    	return echoNumber;
    }
    
    public void setDynamicScanNumber(int dynamicScanNumber[]) {
    	this.dynamicScanNumber = dynamicScanNumber;
    }
    
    public int[] getDynamicScanNumber() {
    	return dynamicScanNumber;
    }
    
    public void setCardiacPhaseNumber(int cardiacPhaseNumber[]) {
    	this.cardiacPhaseNumber = cardiacPhaseNumber;
    }
    
    public int[] getCardiacPhaseNumber() {
    	return cardiacPhaseNumber;
    }
    
    public void setImageTypeMR(int imageTypeMR[]) {
    	this.imageTypeMR = imageTypeMR;
    }
    
    public int[] getImageTypeMR() {
    	return imageTypeMR;
    }
    
    public void setScanningSequence(int scanningSequence[]) {
    	this.scanningSequence = scanningSequence;
    }
    
    public int[] getScanningSequence() {
    	return scanningSequence;
    }
    
    public void setScanPercentage(int scanPercentage[]) {
    	this.scanPercentage = scanPercentage;
    }
    
    public int[] getScanPercentage() {
    	return scanPercentage;
    }
    
    public void setWindowCenter(int windowCenter[]) {
    	this.windowCenter = windowCenter;
    }
    
    public int[] getWindowCenter() {
    	return windowCenter;
    }
    
    public void setWindowWidth(int windowWidth[]) {
    	this.windowWidth = windowWidth;
    }
    
    public int[] getWindowWidth() {
    	return windowWidth;
    }
    
    public void setImageAngulation(float imageAngulation[][]) {
    	this.imageAngulation = imageAngulation;
    }
    
    public float[][] getImageAngulation() {
    	return imageAngulation;
    }
    
    public void setImageOffcentre(float imageOffcentre[][]) {
    	this.imageOffcentre = imageOffcentre;
    }
    
    public float[][] getImageOffcentre() {
    	return imageOffcentre;
    }
    
    public void setFmriStatusIndication(int fmriStatusIndication[]) {
    	this.fmriStatusIndication = fmriStatusIndication;
    }
    
    public int[] getFmriStatusIndication() {
    	return fmriStatusIndication;
    }
    
    public void setImageTypeEDES(int imageTypeEDES[]) {
    	this.imageTypeEDES = imageTypeEDES;
    }
    
    public int[] imageTypeEDES() {
    	return imageTypeEDES;
    }

    public void setEchoTime(float echoTime[]) {
    	this.echoTime = echoTime;
    }
    
    public float[] getEchoTime() {
    	return echoTime;
    }
    
    public void setDynamicScanBeginTime(float dynamicScanBeginTime[]) {
    	this.dynamicScanBeginTime = dynamicScanBeginTime;
    }
    
    public float[] getDynamicScanBeginTime() {
    	return dynamicScanBeginTime;
    }
    
    public void setTriggerTime(float triggerTime[]) {
    	this.triggerTime = triggerTime;
    }
    
    public float[] getTriggerTime() {
    	return triggerTime;
    }
    
    public void setNumberOfAverages(int numberOfAverages[]) {
    	this.numberOfAverages = numberOfAverages;
    }
    
    public int[] getNumberOfAverages() {
    	return numberOfAverages;
    }
    
    public void setFlipAngle(float flipAngle[]) {
    	this.flipAngle = flipAngle;
    }
    
    public float[] getFlipAngle() {
    	return flipAngle;
    }
    
    public void setCardiacFrequency(int cardiacFrequency[]) {
    	this.cardiacFrequency = cardiacFrequency;
    }
    
    public int[] getCardiacFrequency() {
    	return cardiacFrequency;
    }
    
    public void setMinimumRRInterval(int minimumRRInterval[]) {
    	this.minimumRRInterval = minimumRRInterval;
    }
    
    public int[] getMinimumRRInterval() {
    	return minimumRRInterval;
    }
    
    public void setMaximumRRInterval(int maximumRRInterval[]) {
    	this.maximumRRInterval = maximumRRInterval;
    }
    
    public int[] getMaximumRRInterval() {
    	return maximumRRInterval;
    }
    
    public void setTurboFactor(int turboFactor[]) {
    	this.turboFactor = turboFactor;
    }
    
    public int[] getTurobFactor() {
    	return turboFactor;
    }
    
    public void setInversionDelay(float inversionDelay[]) {
    	this.inversionDelay = inversionDelay;
    }
    
    public float[] getInversionDelay() {
    	return inversionDelay;
    }
    
    public void setDiffusionBFactor(float diffusionBFactor[]) {
    	this.diffusionBFactor = diffusionBFactor;
    }
    
    public float[] getDiffusionBFactor() {
    	return diffusionBFactor;
    }
    
    public void setDiffusionBValueNumber(int diffusionBValueNumber[]) {
    	this.diffusionBValueNumber = diffusionBValueNumber;
    }
    
    public int[] getDiffusionBValueNumber() {
    	return diffusionBValueNumber;
    }
    
    public void setGradientOrientationNumber(int gradientOrientationNumber[]) {
    	this.gradientOrientationNumber = gradientOrientationNumber;
    }
    
    public int[] getGradientOrientationNumber() {
    	return gradientOrientationNumber;
    }

    public void setContrastType(String contrastType[]) {
    	this.contrastType = contrastType;
    }
    
    public String[] getContrastType() {
    	return contrastType;
    }
    
    public void setDiffusionAnisotropyType(String diffusionAnisotropyType[]) {
    	this.diffusionAnisotropyType = diffusionAnisotropyType;
    }
    
    public String[] getDiffusionAnisotropyType() {
    	return diffusionAnisotropyType;
    }
    
    public void setDiffusion(float diffusion[][]) {
    	this.diffusion = diffusion;
    }
    
    public float[][] getDiffusion() {
    	return diffusion;
    }
    
    public void setLabelType(int labelType[]) {
    	this.labelType = labelType;
    }
    
    public int[] getLabelType() {
    	return labelType;
    }
    
    public void setContrastBolusAgent(String contrastBolusAgent[]) {
    	this.contrastBolusAgent = contrastBolusAgent;
    }
    
    public String[] getContrastBolusAgent() {
    	return contrastBolusAgent;
    }
    
    public void setContrastBolusRoute(float contrastBolusRoute[]) {
    	this.contrastBolusRoute = contrastBolusRoute;
    }
    
    public float[] getContrastBolusRoute() {
    	return contrastBolusRoute;
    }
    
    public void setContrastBolusVolume(String contrastBolusVolume[]) {
    	this.contrastBolusVolume = contrastBolusVolume;
    }
    
    public String[] getContrastBolusVolume() {
    	return contrastBolusVolume;
    }
    
    public void setContrastBolusStartTime(String contrastBolusStartTime[]) {
    	this.contrastBolusStartTime = contrastBolusStartTime;
    }
    
    public String[] getContrastBolusStartTime() {
    	return contrastBolusStartTime;
    }
    
    public void setContrastBolusTotalDose(float contrastBolusTotalDose[]) {
    	this.contrastBolusTotalDose = contrastBolusTotalDose;
    }
    
    public float[] getContrastBolusTotalDose() {
    	return contrastBolusTotalDose;
    }
    
    public void setContrastBolusIngredient(String contrastBolusIngredient[]) {
    	this.contrastBolusIngredient = contrastBolusIngredient;
    }
    
    public String[] getContrastBolusIngredient() {
    	return contrastBolusIngredient;
    }
    
    public void setContrastBolusIngredientConcentration(float contrastBolusIngredientConcentration[]) {
    	this.contrastBolusIngredientConcentration = contrastBolusIngredientConcentration;
    }
    
    public float[] getContrastBolusIngredientConcentration() {
    	return contrastBolusIngredientConcentration;
    }
}
