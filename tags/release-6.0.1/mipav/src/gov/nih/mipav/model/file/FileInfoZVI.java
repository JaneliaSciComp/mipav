package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * File info storage container.
 */
public class FileInfoZVI extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    // A different blackValue for every z slice and channel
    double blackValue0 = Double.NaN;
    double blackValue1 = Double.NaN;
    double blackValue2 = Double.NaN;
    double blackValue3 = Double.NaN;
    // A different whiteValue for every z slice and channel
    double whiteValue0 = Double.NaN;
    double whiteValue1 = Double.NaN;
    double whiteValue2 = Double.NaN;
    double whiteValue3 = Double.NaN;
    double gammaValue = Double.NaN;
    // In one image acquisitionBitDepth = 12 while validBitsPerPixel = 16
    int acquisitionBitDepth = 0;
    int validBitsPerPixel = 0;
    double cameraFrameScalingFactor = Double.NaN;
    double cameraLiveScalingFactor = Double.NaN;
    double cameraFramePixelDistance = Double.NaN;
    String channelName0 = null;
    String channelName1 = null;
    String channelName2 = null;
    String channelName3 = null;
    int channelNameChannel0;
    int channelNameChannel1;
    int channelNameChannel2;
    int channelNameChannel3;
    // Exposure time in milliseconds
    // A different exposure time seen for each channel
    double exposureTime0 = Double.NaN;
    double exposureTime1 = Double.NaN;
    double exposureTime2 = Double.NaN;
    double exposureTime3 = Double.NaN;
    int exposureTimeChannel0;
    int exposureTimeChannel1;
    int exposureTimeChannel2;
    int exposureTimeChannel3;
    int apotomeProcessingMode = Integer.MIN_VALUE;
    // A different apotome grid position is seen for each channel
    int apotomeGridPosition0 = Integer.MIN_VALUE;
    int apotomeGridPosition1 = Integer.MIN_VALUE;
    int apotomeGridPosition2 = Integer.MIN_VALUE;
    int apotomeGridPosition3 = Integer.MIN_VALUE;
    int apotomeGridPositionChannel0;
    int apotomeGridPositionChannel1;
    int apotomeGridPositionChannel2;
    int apotomeGridPositionChannel3;
    int apotomeFullPhaseShift = Integer.MIN_VALUE;
    double apotomeFilterStrength = Double.NaN;
    int apotomeCamFilterHarmonics = Integer.MIN_VALUE;
    double apotomeGratingPeriod = Double.NaN;
    int apotomeCamNormalize = Integer.MIN_VALUE;
    int apotomeAveragingCount = Integer.MIN_VALUE;
    int microscopeType = Integer.MIN_VALUE;
    int microscopeIllumination = Integer.MIN_VALUE;
    double focusDepth = Double.NaN;
    double cameraAdapterMagnification = Double.NaN;
    int microscopePort = Integer.MIN_VALUE;
    double ocularTotalMagnification = Double.NaN;
    int objectiveTurretPosition = Integer.MIN_VALUE;
    double objectiveMagnification = Double.NaN;
    double objectiveNA = Double.NaN;
    int objectiveContrastMethod = Integer.MIN_VALUE;
    String objectiveImmersionType = null;
    double objectiveWorkingDistance = Double.NaN;
    // One focusPosition for every z slice
    double focusPosition = Double.NaN;
    double relFocusPosition1 = Double.NaN;
    double relFocusPosition2 = Double.NaN;
    String focusCalibrated = null;
    String lightManagerEnabled = null;
    int lightManagerMode = Integer.MIN_VALUE;
    String parfocalCorrection = null;
    String dazzleProtection = null;
    int contrastManagerMode = Integer.MIN_VALUE;
    // One reflectorPosition for every channel
    int reflectorPosition0 = Integer.MIN_VALUE;
    int reflectorPosition1 = Integer.MIN_VALUE;
    int reflectorPosition2 = Integer.MIN_VALUE;
    int reflectorPosition3 = Integer.MIN_VALUE;
    int reflectorPositionChannel0;
    int reflectorPositionChannel1;
    int reflectorPositionChannel2;
    int reflectorPositionChannel3;
    double reflectorMagnification = Double.NaN;
    int transmittedLightShutter = Integer.MIN_VALUE;
    int reflectedLightShutter = Integer.MIN_VALUE;
    double transmittedLightHalogenLampVoltage = Double.NaN;
    int transmittedLightHalogenLampMode = Integer.MIN_VALUE;
    // One multichannel color for every channel
    int multichannelColor0 = Integer.MIN_VALUE;
    int multichannelColor1 = Integer.MIN_VALUE;
    int multichannelColor2 = Integer.MIN_VALUE;
    int multichannelColor3 = Integer.MIN_VALUE;
    int multichannelColorChannel0;
    int multichannelColorChannel1;
    int multichannelColorChannel2;
    int multichannelColorChannel3;
    double multichannelWeight = Double.NaN;
    double stagePositionX = Double.NaN;
    double stagePositionY = Double.NaN;
    double originalStagePositionX = Double.NaN;
    double originalStagePositionY = Double.NaN;
    double scaleWidth = Double.NaN;
    double scaleHeight = Double.NaN;
    int cameraFrameStartLeft = Integer.MIN_VALUE;
    int cameraFrameStartTop = Integer.MIN_VALUE;
    int cameraFrameWidth = Integer.MIN_VALUE;
    int cameraFrameHeight = Integer.MIN_VALUE;
    int cameraFrameImageOrientation = Integer.MIN_VALUE;
    int cameraBinning = Integer.MIN_VALUE;
    int axioCamSelector = Integer.MIN_VALUE;
    int axioCamType = Integer.MIN_VALUE;
    int axioCamResolution = Integer.MIN_VALUE;
    int axioCamColorModel = Integer.MIN_VALUE;
    int axioCamMicroScanning = Integer.MIN_VALUE;
    int amplificationIndex = Integer.MIN_VALUE;
    int axioCamBlackReference = Integer.MIN_VALUE;
    int cameraShadingCorrection = Integer.MIN_VALUE;
    int axioCamEnhanceColor = Integer.MIN_VALUE;
    int axioCamNIRMode = Integer.MIN_VALUE;
    int axioCamShutterControl = Integer.MIN_VALUE;
    int axioCamShutterSignal = Integer.MIN_VALUE;
    int axioCamDelayTime = Integer.MIN_VALUE;
    double microscopeMagnification = Double.NaN;
    double reflectedLightHalogenLampVoltage = Double.NaN;
    int reflectedLightHalogenLampMode = Integer.MIN_VALUE;
    int apotomeAutoShutterUsed = Integer.MIN_VALUE;
    int objectType = Integer.MIN_VALUE;
    double imageMemoryUsage = Double.NaN;
    int excitationWavelength0 = Integer.MIN_VALUE;
    int excitationWavelength1 = Integer.MIN_VALUE;
    int excitationWavelength2 = Integer.MIN_VALUE;
    int excitationWavelength3 = Integer.MIN_VALUE;
    int excitationWavelengthChannel0;
    int excitationWavelengthChannel1;
    int excitationWavelengthChannel2;
    int excitationWavelengthChannel3;
    int emissionWavelength0 = Integer.MIN_VALUE;
    int emissionWavelength1 = Integer.MIN_VALUE;
    int emissionWavelength2 = Integer.MIN_VALUE;
    int emissionWavelength3 = Integer.MIN_VALUE;
    int emissionWavelengthChannel0;
    int emissionWavelengthChannel1;
    int emissionWavelengthChannel2;
    int emissionWavelengthChannel3;
    String cameraImageAcquisitionTime0 = null;
    String cameraImageAcquisitionTime1 = null;
    String cameraImageAcquisitionTime2 = null;
    String cameraImageAcquisitionTime3 = null;
    String imageRelativeTime0 = null;
    String imageRelativeTime1 = null;
    String imageRelativeTime2 = null;
    String imageRelativeTime3 = null;
    String fileDate = null;
    int fileSize = Integer.MIN_VALUE;
    int channel0 = Integer.MIN_VALUE;
    int channel1 = Integer.MIN_VALUE;
    int channel2 = Integer.MIN_VALUE;
    int channel3 = Integer.MIN_VALUE;
    int axioCamAnalogGainEnable = Integer.MIN_VALUE;
    int cameraShutterLiveEnable = Integer.MIN_VALUE;
    double axioCamSaturation = Double.NaN;
    int condenserContrast = Integer.MIN_VALUE;
    double condenserNumericalAperture = Double.NaN;
    String stageCalibrated = null;
    double optovar = Double.NaN;
    int cameraBitDepth = Integer.MIN_VALUE;
    int externalShutter1 = Integer.MIN_VALUE;
    String documentType = null;
    String IDTagFileName = null;
    String userID = null;
    String userName = null;
    String userCity = null;
    String userAddress = null;
    String userPhone = null;
    String userFax = null;
    String userCountry = null;
    String userCompany = null;
    String userCompanyLogo = null;
    String deviceScalingName = null;
    String camera = null;
    String microscopeName = null;
    String objectiveID = null;
    String objectiveName = null;
    String reflectorID = null;
    String reflector = null;
    String reference = null;
    String fileLink = null;
    String fileID;
    String lastModifiedBy = null;
    String title = null;
    String apotomeGridName = null;
    String apotomeFilterName = null;
    String apotomeCamPhaseAngles = null;
    String apotomeCamGenericCameraName = null;
    String documentSubtype =  null;
    String comments = null;
        /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoZVI(String name, String directory, int format) {
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
        
        if (acquisitionBitDepth > 0) {
            dialog.append("Acquisition bit depth = " + acquisitionBitDepth + "\n");
        }
        
        if (validBitsPerPixel > 0) {
            dialog.append("Valid bits per pixel in raw image data = " + validBitsPerPixel + "\n");
        }
        
        if (!Double.isNaN(stagePositionX)) {
            dialog.append("Stage position X = " + stagePositionX + "\n");
        }
        
        if (!Double.isNaN(stagePositionY)) {
            dialog.append("Stage position Y = " + stagePositionY + "\n");
        }
        
        if (!Double.isNaN(originalStagePositionX)) {
            dialog.append("Original stage position X = " + originalStagePositionX + "\n");
        }
        
        if (!Double.isNaN(originalStagePositionY)) {
            dialog.append("Original stage position Y = " + originalStagePositionY + "\n");
        }
        
        if (channelName0 != null) {
            dialog.append("Channel " + channelNameChannel0 + " channel name = " + channelName0 + "\n");
        }
        
        if (channelName1 != null) {
            dialog.append("Channel " + channelNameChannel1 + " channel name = " + channelName1 + "\n");
        }
        
        if (channelName2 != null) {
            dialog.append("Channel " + channelNameChannel2 + " channel name = " + channelName2 + "\n");
        }
        
        if (channelName3 != null) {
            dialog.append("Channel " + channelNameChannel3 + " channel name = " + channelName3 + "\n");
        }
        
        if (excitationWavelength0 != Integer.MIN_VALUE) {
            dialog.append("Channel " + excitationWavelengthChannel0 + " excitation wavelength = " + excitationWavelength0 + "\n");
        }
        
        if (excitationWavelength1 != Integer.MIN_VALUE) {
            dialog.append("Channel " + excitationWavelengthChannel1 + " excitation wavelength = " + excitationWavelength1 + "\n");
        }
        
        if (excitationWavelength2 != Integer.MIN_VALUE) {
            dialog.append("Channel " + excitationWavelengthChannel2 + " excitation wavelength = " + excitationWavelength2 + "\n");
        }
        
        if (excitationWavelength3 != Integer.MIN_VALUE) {
            dialog.append("Channel " + excitationWavelengthChannel3 + " excitation wavelength = " + excitationWavelength3 + "\n");
        }
        
        if (emissionWavelength0 != Integer.MIN_VALUE) {
            dialog.append("Channel " + emissionWavelengthChannel0 + " emission wavelength = " + emissionWavelength0 + "\n");
        }
        
        if (emissionWavelength1 != Integer.MIN_VALUE) {
            dialog.append("Channel " + emissionWavelengthChannel1 + " emission wavelength = " + emissionWavelength1 + "\n");
        }
        
        if (emissionWavelength2 != Integer.MIN_VALUE) {
            dialog.append("Channel " + emissionWavelengthChannel2 + " emission wavelength = " + emissionWavelength2 + "\n");
        }
        
        if (emissionWavelength3 != Integer.MIN_VALUE) {
            dialog.append("Channel " + emissionWavelengthChannel3 + " emission wavelength = " + emissionWavelength3 + "\n");
        }
        
        if (!Double.isNaN(blackValue0)) {
            dialog.append("Channel " + channel0 + " black value = " + blackValue0 + "\n");
        }
        
        if (!Double.isNaN(blackValue1)) {
            dialog.append("Channel " + channel1 + " black value = " + blackValue1 + "\n");
        }
        
        if (!Double.isNaN(blackValue2)) {
            dialog.append("Channel " + channel2 + " black value = " + blackValue2 + "\n");
        }
        
        if (!Double.isNaN(blackValue3)) {
            dialog.append("Channel " + channel3 + " black value = " + blackValue3 + "\n");
        }
        
        if (!Double.isNaN(whiteValue0)) {
            dialog.append("Channel " + channel0 + " white value = " + whiteValue0 + "\n");
        }
        
        if (!Double.isNaN(whiteValue1)) {
            dialog.append("Channel " + channel1 + " white value = " + whiteValue1 + "\n");
        }
        
        if (!Double.isNaN(whiteValue2)) {
            dialog.append("Channel " + channel2 + " white value = " + whiteValue2 + "\n");
        }
        
        if (!Double.isNaN(whiteValue3)) {
            dialog.append("Channel " + channel3 + " white value = " + whiteValue3 + "\n");
        }
        
        if (!Double.isNaN(gammaValue)) {
            dialog.append("Gamma value = " + gammaValue + "\n");
        }
        
        if (camera != null) {
            dialog.append("Camera = " + camera + "\n");
        }
        
        if (cameraBitDepth != Integer.MIN_VALUE) {
            dialog.append("Camera bit depth = " + cameraBitDepth + "\n");
        }
        
        if (cameraFrameStartLeft != Integer.MIN_VALUE) {
            dialog.append("Camera frame start left = " + cameraFrameStartLeft + "\n");
        }
        
        if (cameraFrameStartTop != Integer.MIN_VALUE) {
            dialog.append("Camera frame start top = " + cameraFrameStartTop + "\n");
        }
        
        if (cameraFrameWidth != Integer.MIN_VALUE) {
            dialog.append("Camera frame width = " + cameraFrameWidth + "\n");
        }
        
        if (cameraFrameHeight != Integer.MIN_VALUE) {
            dialog.append("Camera frame height = " + cameraFrameHeight + "\n");
        }
        
        if (cameraFrameImageOrientation != Integer.MIN_VALUE) {
            dialog.append("Camera frame image orientation = " + cameraFrameImageOrientation + "\n");
        }
        
        if (cameraBinning != Integer.MIN_VALUE) {
            dialog.append("Camera binning = " + cameraBinning + "\n");
        }
        
        if (deviceScalingName != null) {
            dialog.append("Device scaling name = " + deviceScalingName + "\n");
        }
        
        if (!Double.isNaN(cameraFrameScalingFactor)) {
            dialog.append("Camera frame scaling factor = " + cameraFrameScalingFactor + "\n");
        }
        
        if (!Double.isNaN(cameraLiveScalingFactor)) {
            dialog.append("Camera live scaling factor = " + cameraLiveScalingFactor + "\n");
        }
        
        if (!Double.isNaN(cameraFramePixelDistance)) {
            dialog.append("Camera frame pixel distance = " + cameraFramePixelDistance + "\n");
        }
        
        if (!Double.isNaN(exposureTime0)) {
            if (exposureTimeChannel0 != Integer.MIN_VALUE) {
                dialog.append("Channel " + exposureTimeChannel0 + " exposure time = " + exposureTime0 + " milliseconds\n");
            }
            else {
                dialog.append("Exposure time = " + exposureTime0 + " milliseconds\n");    
            }
        }
        
        if (!Double.isNaN(exposureTime1)) {
            dialog.append("Channel " + exposureTimeChannel1 + " exposure time = " + exposureTime1 + " milliseconds\n");
        }
        
        if (!Double.isNaN(exposureTime2)) {
            dialog.append("Channel " + exposureTimeChannel2 + " exposure time = " + exposureTime2 + " milliseconds\n");
        }
        
        if (!Double.isNaN(exposureTime3)) {
            dialog.append("Channel " + exposureTimeChannel3 + " exposure time = " + exposureTime3 + " milliseconds\n");
        }
        
        if (apotomeProcessingMode != Integer.MIN_VALUE) {
            dialog.append("Apotome processing mode = " + apotomeProcessingMode + "\n");
        }
        
        if (apotomeGridName != null) {
            dialog.append("Apotome grid name = " + apotomeGridName + "\n");
        }
        
        if (apotomeGridPosition0 != Integer.MIN_VALUE) {
            dialog.append("Channel " + apotomeGridPositionChannel0 + " apotome grid position = " + apotomeGridPosition0 + "\n");
        }
        
        if (apotomeGridPosition1 != Integer.MIN_VALUE) {
            dialog.append("Channel " + apotomeGridPositionChannel1 + " apotome grid position = " + apotomeGridPosition1 + "\n");
        }
        
        if (apotomeGridPosition2 != Integer.MIN_VALUE) {
            dialog.append("Channel " + apotomeGridPositionChannel2 + " apotome grid position = " + apotomeGridPosition2 + "\n");
        }
        
        if (apotomeGridPosition3 != Integer.MIN_VALUE) {
            dialog.append("Channel " + apotomeGridPositionChannel3 + " apotome grid position = " + apotomeGridPosition3 + "\n");
        }
        
        if (apotomeFullPhaseShift != Integer.MIN_VALUE) {
            dialog.append("Apotome full phase shift = " + apotomeFullPhaseShift + "\n");
        }
        
        if (apotomeFilterName != null) {
            dialog.append("Apotome filter name = " + apotomeFilterName + "\n");
        }
        
        if (!Double.isNaN(apotomeFilterStrength)) {
            dialog.append("Apotome filter strength = " + apotomeFilterStrength + "\n");
        }
        
        if (apotomeCamFilterHarmonics != Integer.MIN_VALUE) {
            dialog.append("Apotome cam filter harmonics = " + apotomeCamFilterHarmonics + "\n");
        }
        
        if (!Double.isNaN(apotomeGratingPeriod)) {
            dialog.append("Apotome grating period = " + apotomeGratingPeriod + "\n");    
        }
        
        if (apotomeCamGenericCameraName !=  null) {
            dialog.append("Apotome cam generic camera name = " + apotomeCamGenericCameraName + "\n");
        }
        
        if (apotomeCamPhaseAngles != null) {
            dialog.append("Apotome cam phase angles = " + apotomeCamPhaseAngles + "\n");
        }
        
        if (apotomeCamNormalize != Integer.MIN_VALUE) {
            dialog.append("Apotome cam normalize = " + apotomeCamNormalize + "\n");
        }
        
        if (apotomeAveragingCount != Integer.MIN_VALUE) {
            dialog.append("Apotome averaging count = " + apotomeAveragingCount + "\n");
        }
        
        if (apotomeAutoShutterUsed != Integer.MIN_VALUE) {
            dialog.append("Apotome auto shutter used = " + apotomeAutoShutterUsed + "\n");
        }
        
        if (microscopeName != null) {
            dialog.append("Microscope name = " + microscopeName + "\n");
        }
        
        if (microscopeType != Integer.MIN_VALUE) {
            dialog.append("Microscope type = " + microscopeType + "\n");
        }
        
        if (microscopeIllumination != Integer.MIN_VALUE) {
            dialog.append("Microscope illumination = " + microscopeIllumination + "\n");
        }
        
        if (!Double.isNaN(microscopeMagnification)) {
            dialog.append("Microscope magnification = " + microscopeMagnification + "\n");
        }
        
        if (!Double.isNaN(focusDepth)) {
            dialog.append("Focus depth = " + focusDepth + "\n");
        }
        
        if (!Double.isNaN(cameraAdapterMagnification)) {
            dialog.append("Camera adapter magnification = " + cameraAdapterMagnification + "\n");
        }
        
        if (microscopePort != Integer.MIN_VALUE) {
            dialog.append("Microscope port = " + microscopePort + "\n");
        }
        
        if (objectiveName != null) {
            dialog.append("Objective name = " + objectiveName + "\n");
        }
        
        if (objectiveID != null) {
            dialog.append("Objective ID = " + objectiveID + "\n");
        }
        
        if (!Double.isNaN(ocularTotalMagnification)) {
            dialog.append("Ocular total magnification = " + ocularTotalMagnification + "\n");
        }
        
        if (objectiveTurretPosition != Integer.MIN_VALUE) {
            dialog.append("Objective turret position = " + objectiveTurretPosition + "\n");
        }
        
        if (!Double.isNaN(objectiveMagnification)) {
            dialog.append("Objective magnification = " + objectiveMagnification + "\n");
        }
        
        if (!Double.isNaN(objectiveNA)) {
            dialog.append("Objective numerical aperture = " + objectiveNA + "\n");
        }
        
        if (objectiveContrastMethod != Integer.MIN_VALUE) {
            dialog.append("Objective contrast method = " + objectiveContrastMethod + "\n");
        }
        
        if (objectiveImmersionType != null) {
            dialog.append("Objective immersion type = " + objectiveImmersionType + "\n");
        }
        
        if (!Double.isNaN(objectiveWorkingDistance)) {
            dialog.append("Objective working distance = " + objectiveWorkingDistance + "\n");
        }
        
        if (!Double.isNaN(focusPosition)) {
            dialog.append("Focus position = " + focusPosition + "\n");
        }
        
        if (!Double.isNaN(relFocusPosition1)) {
            dialog.append("Rel focus position 1 = " + relFocusPosition1 + "\n");
        }
        
        if (!Double.isNaN(relFocusPosition2)) {
            dialog.append("Rel focus position 2 = " + relFocusPosition2 + "\n");
        }
        
        if (focusCalibrated != null) {
            dialog.append(focusCalibrated + "\n");
        }
        
        if (lightManagerEnabled != null) {
            dialog.append(lightManagerEnabled + "\n");
        }
        
        if (lightManagerMode != Integer.MIN_VALUE) {
            dialog.append("Light manager mode = " + lightManagerMode + "\n");
        }
        
        if (parfocalCorrection != null) {
            dialog.append(parfocalCorrection + "\n");
        }
        
        if (dazzleProtection != null) {
            dialog.append(dazzleProtection + "\n");
        }
        
        if (contrastManagerMode != Integer.MIN_VALUE) {
            dialog.append("Contrast manager mode = " + contrastManagerMode + "\n");   
        }
        
        if (reflector != null) {
            dialog.append("Reflector = " + reflector + "\n");
        }
        
        if (reflectorID != null) {
            dialog.append("Reflector ID = " + reflectorID + "\n");
        }
        
        if (reflectorPosition0 != Integer.MIN_VALUE) {
            if (reflectorPositionChannel0 != Integer.MIN_VALUE) {
                dialog.append("Channel " + reflectorPositionChannel0 + " reflector position = " + reflectorPosition0 + "\n");
            }
            else {
                dialog.append("Reflector position = " + reflectorPosition0 + "\n");    
            }
        }
        
        if (reflectorPosition1 != Integer.MIN_VALUE) {
            dialog.append("Channel " + reflectorPositionChannel1 + " reflector position = " + reflectorPosition1 + "\n");
        }
        
        if (reflectorPosition2 != Integer.MIN_VALUE) {
            dialog.append("Channel " + reflectorPositionChannel2 + " reflector position = " + reflectorPosition2 + "\n");
        }
        
        if (reflectorPosition3 != Integer.MIN_VALUE) {
            dialog.append("Channel " + reflectorPositionChannel3 + " reflector position = " + reflectorPosition3 + "\n");
        }
        
        if (!Double.isNaN(reflectorMagnification)) {
            dialog.append("Reflector magnification = " + reflectorMagnification + "\n");
        }
        
        if (transmittedLightShutter != Integer.MIN_VALUE) {
            dialog.append("Transmitted light shutter = " + transmittedLightShutter + "\n");
        }
        
        if (reflectedLightShutter != Integer.MIN_VALUE) {
            dialog.append("Reflected light shutter = " + reflectedLightShutter + "\n");
        }
        
        if (!Double.isNaN(reflectedLightHalogenLampVoltage)) {
            dialog.append("Reflected light halogen lamp voltage = " + reflectedLightHalogenLampVoltage + "\n");    
        }
        
        if (reflectedLightHalogenLampMode != Integer.MIN_VALUE) {
            dialog.append("Reflected light halogen lamp mode = " + reflectedLightHalogenLampMode + "\n");
        }
        
        if (!Double.isNaN(transmittedLightHalogenLampVoltage)) {
            dialog.append("Transmitted light halogen lamp voltage = " + transmittedLightHalogenLampVoltage + "\n");    
        }
        
        if (transmittedLightHalogenLampMode != Integer.MIN_VALUE) {
            dialog.append("Transmitted light halogen lamp mode = " + transmittedLightHalogenLampMode + "\n");
        }
        
        if (multichannelColor0 != Integer.MIN_VALUE) {
            dialog.append("Channel " + multichannelColorChannel0 + " multichannel color = " + multichannelColor0 + "\n");
        }
        
        if (multichannelColor1 != Integer.MIN_VALUE) {
            dialog.append("Channel " + multichannelColorChannel1 + " multichannel color = " + multichannelColor1 + "\n");
        }
        
        if (multichannelColor2 != Integer.MIN_VALUE) {
            dialog.append("Channel " + multichannelColorChannel2 + " multichannel color = " + multichannelColor2 + "\n");
        }
        
        if (multichannelColor3 != Integer.MIN_VALUE) {
            dialog.append("Channel " + multichannelColorChannel3 + " multichannel color = " + multichannelColor3 + "\n");
        }
        
        if (!Double.isNaN(multichannelWeight)) {
            dialog.append("Multichannel weight = " + multichannelWeight + "\n");
        }
        
        if (!Double.isNaN(scaleWidth)) {
            dialog.append("Scale width = " + scaleWidth + "\n");
        }
        
        if (!Double.isNaN(scaleHeight)) {
            dialog.append("Scale height = " + scaleHeight + "\n");
        }
        
        if (axioCamSelector != Integer.MIN_VALUE) {
            dialog.append("Axio cam selector = " + axioCamSelector + "\n");
        }
        
        if (axioCamType != Integer.MIN_VALUE) {
            dialog.append("Axio cam type = " + axioCamType + "\n");
        }
        
        if (axioCamResolution != Integer.MIN_VALUE) {
            dialog.append("Axio cam resolution = " + axioCamResolution + "\n");
        }
        
        if (axioCamColorModel != Integer.MIN_VALUE) {
            dialog.append("Axio cam color model = " + axioCamColorModel + "\n");
        }
        
        if (axioCamMicroScanning != Integer.MIN_VALUE) {
            dialog.append("Axio cam micro scanning = " + axioCamMicroScanning + "\n");
        }
        
        if (amplificationIndex != Integer.MIN_VALUE) {
            dialog.append("Amplification index = " + amplificationIndex + "\n");
        }
        
        if (axioCamBlackReference != Integer.MIN_VALUE) {
            dialog.append("Axio cam black reference = " + axioCamBlackReference + "\n");
        }
        
        if (cameraShadingCorrection != Integer.MIN_VALUE) {
            dialog.append("Camera shading correction = " + cameraShadingCorrection + "\n");
        }
        
        if (axioCamEnhanceColor != Integer.MIN_VALUE) {
            dialog.append("Axio cam enhance color = " + axioCamEnhanceColor + "\n");
        }
        
        if (axioCamNIRMode != Integer.MIN_VALUE) {
            dialog.append("Axio cam NIR mode = " + axioCamNIRMode + "\n");
        }
        
        if (axioCamAnalogGainEnable != Integer.MIN_VALUE) {
            dialog.append("Axio cam analog gain enable = " + axioCamAnalogGainEnable + "\n");
        }
        
        if (axioCamShutterControl != Integer.MIN_VALUE) {
            dialog.append("Axio cam shutter control = " + axioCamShutterControl + "\n");
        }
        
        if (axioCamShutterSignal != Integer.MIN_VALUE) {
            dialog.append("Axio cam shutter signal = " + axioCamShutterSignal + "\n");
        }
        
        if (axioCamDelayTime != Integer.MIN_VALUE) {
            dialog.append("Axio cam delay time = " + axioCamDelayTime + "\n");
        }
        
        if (cameraShutterLiveEnable != Integer.MIN_VALUE) {
            dialog.append("Camera shutter live enable = " + cameraShutterLiveEnable + "\n");
        }
        
        if (externalShutter1 != Integer.MIN_VALUE) {
            dialog.append("External shutter 1 = " + externalShutter1 + "\n");
        }
        
        if (!Double.isNaN(axioCamSaturation)) {
            dialog.append("Axio cam saturation = " + axioCamSaturation + "\n");
        }
        
        if (condenserContrast != Integer.MIN_VALUE) {
            dialog.append("Condenser contrast = " + condenserContrast + "\n");
        }
        
        if (!Double.isNaN(condenserNumericalAperture)) {
            dialog.append("Condenser numerical aperture = " + condenserNumericalAperture + "\n");
        }
        
        if (stageCalibrated != null) {
            dialog.append(stageCalibrated + "\n");
        }
        
        if (!Double.isNaN(optovar)) {
            dialog.append("Optovar = " + optovar + "\n");
        }
        
        if (objectType != Integer.MIN_VALUE) {
            dialog.append("Object type = " + objectType + "\n");
        }
        
        if (!Double.isNaN(imageMemoryUsage)) {
            dialog.append("Image memory usage (RAM) = " + imageMemoryUsage + "\n");
        }
        
        if (cameraImageAcquisitionTime0 != null) {
            dialog.append("Channel " + channel0 + " camera image acquisition time = " + cameraImageAcquisitionTime0 + "\n");
        }
        
        if (cameraImageAcquisitionTime1 != null) {
            dialog.append("Channel " + channel1 + " camera image acquisition time = " + cameraImageAcquisitionTime1 + "\n");
        }
        
        if (cameraImageAcquisitionTime2 != null) {
            dialog.append("Channel " + channel2 + " camera image acquisition time = " + cameraImageAcquisitionTime2 + "\n");
        }
        
        if (cameraImageAcquisitionTime3 != null) {
            dialog.append("Channel " + channel3 + " camera image acquisition time = " + cameraImageAcquisitionTime3 + "\n");
        }
        
        if (imageRelativeTime0 != null) {
            dialog.append("Channel " + channel0 + " image relative time = " + imageRelativeTime0 + "\n");
        }
        
        if (imageRelativeTime1 != null) {
            dialog.append("Channel " + channel1 + " image relative time = " + imageRelativeTime1 + "\n");
        }
        
        if (imageRelativeTime2 != null) {
            dialog.append("Channel " + channel2 + " image relative time = " + imageRelativeTime2 + "\n");
        }
        
        if (imageRelativeTime3 != null) {
            dialog.append("Channel " + channel3 + " image relative time = " + imageRelativeTime3 + "\n");
        }
        
        if (title != null) {
            dialog.append("Title = " + title + "\n");
        }
        
        if (IDTagFileName != null) {
            dialog.append("File name = " + IDTagFileName + "\n");
        }
        
        if (comments != null) {
            dialog.append("Comments = " + comments + "\n");
        }
        
        if (fileDate != null) {
            dialog.append("File date = " + fileDate + "\n");
        }
        
        if (fileSize != Integer.MIN_VALUE) {
            dialog.append("File size = " + fileSize + "\n");
        }
        
        if (fileLink != null) {
            dialog.append("File link = " + fileLink + "\n");
        }
        
        if (fileID != null) {
            dialog.append("File ID = " + fileID + "\n");
        }
        
        if (reference != null) {
            dialog.append("Reference = " + reference + "\n");
        }
        
        if (lastModifiedBy != null) {
            dialog.append("Last modified by = " + lastModifiedBy + "\n");
        }
        
        if (documentType != null) {
            dialog.append("Document type = " + documentType + "\n");
        }
        
        if (documentSubtype != null) {
            dialog.append("Document subtype = " + documentSubtype + "\n");
        }
        
        if (userID != null) {
            dialog.append("User ID = " + userID + "\n");
        }
        
        if (userName != null) {
            dialog.append("User name = " + userName + "\n");
        }
        
        if (userCity != null) {
            dialog.append("User city = " + userCity + "\n");
        }
        
        if (userAddress != null) {
            dialog.append("User address = " + userAddress + "\n");
        }
        
        if (userPhone != null) {
            dialog.append("User phone = " + userPhone + "\n");
        }
        
        if (userFax != null) {
            dialog.append("User fax = " + userFax + "\n");
        }
        
        if (userCountry != null) {
            dialog.append("User country = " + userCountry + "\n");
        }
        
        if (userCompany != null) {
            dialog.append("User company = " + userCompany + "\n");
        }
        
        if (userCompanyLogo != null) {
            dialog.append("User company logo = " + userCompanyLogo + "\n");
        }
    }
    
    public void setAcquisitionBitDepth(int acquisitionBitDepth) {
        this.acquisitionBitDepth = acquisitionBitDepth;
    }
    
    public void setValidBitsPerPixel(int validBitsPerPixel) {
        this.validBitsPerPixel = validBitsPerPixel;
    }
    
    public void setBlackValue0(double blackValue0) {
        this.blackValue0 = blackValue0;
    }
    
    public void setBlackValue1(double blackValue1) {
        this.blackValue1 = blackValue1;
    }
    
    public void setBlackValue2(double blackValue2) {
        this.blackValue2 = blackValue2;
    }
    
    public void setBlackValue3(double blackValue3) {
        this.blackValue3 = blackValue3;
    }
    
    public void setWhiteValue0(double whiteValue0) {
        this.whiteValue0 = whiteValue0;
    }
    
    public void setWhiteValue1(double whiteValue1) {
        this.whiteValue1 = whiteValue1;
    }
    
    public void setWhiteValue2(double whiteValue2) {
        this.whiteValue2 = whiteValue2;
    }
    
    public void setWhiteValue3(double whiteValue3) {
        this.whiteValue3 = whiteValue3;
    }
    
    public void setGammaValue(double gammaValue) {
        this.gammaValue = gammaValue;
    }
    
    public void setCameraFrameScalingFactor(double cameraFrameScalingFactor) {
        this.cameraFrameScalingFactor = cameraFrameScalingFactor;
    }
    
    public void setCameraLiveScalingFactor(double cameraLiveScalingFactor) {
        this.cameraLiveScalingFactor = cameraLiveScalingFactor;
    }
    
    public void setCameraFramePixelDistance(double cameraFramePixelDistance) {
        this.cameraFramePixelDistance = cameraFramePixelDistance;
    }
    
    public void setExposureTime0(int exposureTimeChannel0, double exposureTime0) {
        this.exposureTimeChannel0 = exposureTimeChannel0;
        this.exposureTime0 = exposureTime0;
    }
    
    public void setExposureTime1(int exposureTimeChannel1, double exposureTime1) {
        this.exposureTimeChannel1 = exposureTimeChannel1;
        this.exposureTime1 = exposureTime1;
    }
    
    public void setExposureTime2(int exposureTimeChannel2, double exposureTime2) {
        this.exposureTimeChannel2 = exposureTimeChannel2;
        this.exposureTime2 = exposureTime2;
    }
    
    public void setExposureTime3(int exposureTimeChannel3, double exposureTime3) {
        this.exposureTimeChannel3 = exposureTimeChannel3;
        this.exposureTime3 = exposureTime3;
    }
    
    public void setApotomeProcessingMode(int apotomeProcessingMode) {
        this.apotomeProcessingMode = apotomeProcessingMode;
    }
    
    public void setApotomeGridPosition0(int apotomeGridPositionChannel0, int apotomeGridPosition0) {
        this.apotomeGridPositionChannel0 = apotomeGridPositionChannel0;
        this.apotomeGridPosition0 = apotomeGridPosition0;    
    }
    
    public void setApotomeGridPosition1(int apotomeGridPositionChannel1, int apotomeGridPosition1) {
        this.apotomeGridPositionChannel1 = apotomeGridPositionChannel1;
        this.apotomeGridPosition1 = apotomeGridPosition1;    
    }
    
    public void setApotomeGridPosition2(int apotomeGridPositionChannel2, int apotomeGridPosition2) {
        this.apotomeGridPositionChannel2 = apotomeGridPositionChannel2;
        this.apotomeGridPosition2 = apotomeGridPosition2;    
    }
    
    public void setApotomeGridPosition3(int apotomeGridPositionChannel3, int apotomeGridPosition3) {
        this.apotomeGridPositionChannel3 = apotomeGridPositionChannel3;
        this.apotomeGridPosition3 = apotomeGridPosition3;    
    }
    
    public void setApotomeFullPhaseShift(int apotomeFullPhaseShift) {
        this.apotomeFullPhaseShift = apotomeFullPhaseShift;
    }
    
    public void setApotomeFilterStrength(double apotomeFilterStrength) {
        this.apotomeFilterStrength = apotomeFilterStrength;
    }
    
    public void setApotomeCamFilterHarmonics(int apotomeCamFilterHarmonics) {
        this.apotomeCamFilterHarmonics = apotomeCamFilterHarmonics;
    }
    
    public void setApotomeGratingPeriod(double apotomeGratingPeriod) {
        this.apotomeGratingPeriod = apotomeGratingPeriod;
    }
    
    public void setApotomeCamNormalize(int apotomeCamNormalize) {
        this.apotomeCamNormalize = apotomeCamNormalize;
    }
    
    public void setApotomeAveragingCount(int apotomeAveragingCount) {
        this.apotomeAveragingCount = apotomeAveragingCount;
    }
    
    public void setMicroscopeType(int microscopeType) {
        this.microscopeType = microscopeType;
    }
    
    public void setMicroscopeIllumination(int microscopeIllumination) {
        this.microscopeIllumination = microscopeIllumination;
    }
    
    public void setFocusDepth(double focusDepth) {
        this.focusDepth = focusDepth;
    }
    
    public void setCameraAdapterMagnification(double cameraAdapterMagnification) {
        this.cameraAdapterMagnification = cameraAdapterMagnification;
    }
    
    public void setMicroscopePort(int microscopePort) {
        this.microscopePort = microscopePort;
    }
    
    public void setOcularTotalMagnification(double ocularTotalMagnification) {
        this.ocularTotalMagnification = ocularTotalMagnification;
    }
    
    public void setObjectiveTurretPosition(int objectiveTurretPosition) {
        this.objectiveTurretPosition = objectiveTurretPosition;
    }
    
    public void setObjectiveMagnification(double objectiveMagnification) {
        this.objectiveMagnification = objectiveMagnification;
    }
    
    public void setObjectiveNA(double objectiveNA) {
        this.objectiveNA = objectiveNA;
    }
    
    public void setObjectiveContrastMethod(int objectiveContrastMethod) {
        this.objectiveContrastMethod = objectiveContrastMethod;
    }
    
    public void setObjectiveImmersionType(String objectiveImmersionType) {
        this.objectiveImmersionType = objectiveImmersionType;
    }
    
    public void setObjectiveWorkingDistance(double objectiveWorkingDistance) {
        this.objectiveWorkingDistance = objectiveWorkingDistance;
    }
    
    public void setFocusPosition(double focusPosition) {
        this.focusPosition = focusPosition;
    }
    
    public void setFocusCalibrated(String focusCalibrated) {
        this.focusCalibrated = focusCalibrated;
    }
    
    public void setLightManagerEnabled(String lightManagerEnabled) {
        this.lightManagerEnabled = lightManagerEnabled;
    }
    
    public void setLightManagerMode(int lightManagerMode) {
        this.lightManagerMode = lightManagerMode;
    }
    
    public void setParfocalCorrection(String parfocalCorrection) {
        this.parfocalCorrection = parfocalCorrection;
    }
    
    public void setDazzleProtection(String dazzleProtection) {
        this.dazzleProtection = dazzleProtection;
    }
    
    public void setContrastManagerMode(int contrastManagerMode) {
        this.contrastManagerMode = contrastManagerMode;
    }
    
    public void setReflectorPosition0(int reflectorPositionChannel0, int reflectorPosition0) {
        this.reflectorPositionChannel0 = reflectorPositionChannel0;
        this.reflectorPosition0 = reflectorPosition0;
    }
    
    public void setReflectorPosition1(int reflectorPositionChannel1, int reflectorPosition1) {
        this.reflectorPositionChannel1 = reflectorPositionChannel1;
        this.reflectorPosition1 = reflectorPosition1;
    }
    
    public void setReflectorPosition2(int reflectorPositionChannel2, int reflectorPosition2) {
        this.reflectorPositionChannel2 = reflectorPositionChannel2;
        this.reflectorPosition2 = reflectorPosition2;
    }
    
    public void setReflectorPosition3(int reflectorPositionChannel3, int reflectorPosition3) {
        this.reflectorPositionChannel3 = reflectorPositionChannel3;
        this.reflectorPosition3 = reflectorPosition3;
    }
    
    public void setReflectorMagnification(double reflectorMagnification) {
        this.reflectorMagnification = reflectorMagnification;
    }
    
    public void setTransmittedLightShutter(int transmittedLightShutter) {
        this.transmittedLightShutter = transmittedLightShutter;
    }
    
    public void setReflectedLightShutter(int reflectedLightShutter) {
        this.reflectedLightShutter = reflectedLightShutter;
    }
    
    public void setTransmittedLightHalogenLampVoltage(double transmittedLightHalogenLampVoltage) {
        this.transmittedLightHalogenLampVoltage = transmittedLightHalogenLampVoltage;
    }
    
    public void setTransmittedLightHalogenLampMode(int transmittedLightHalogenLampMode) {
        this.transmittedLightHalogenLampMode = transmittedLightHalogenLampMode;
    }
    
    public void setMultichannelColor0(int multichannelColorChannel0, int multichannelColor0) {
        this.multichannelColorChannel0 = multichannelColorChannel0;
        this.multichannelColor0 = multichannelColor0;
    }
    
    public void setMultichannelColor1(int multichannelColorChannel1, int multichannelColor1) {
        this.multichannelColorChannel1 = multichannelColorChannel1;
        this.multichannelColor1 = multichannelColor1;
    }
    
    public void setMultichannelColor2(int multichannelColorChannel2, int multichannelColor2) {
        this.multichannelColorChannel2 = multichannelColorChannel2;
        this.multichannelColor2 = multichannelColor2;
    }
    
    public void setMultichannelColor3(int multichannelColorChannel3, int multichannelColor3) {
        this.multichannelColorChannel3 = multichannelColorChannel3;
        this.multichannelColor3 = multichannelColor3;
    }
    
    public void setMultichannelWeight(double multichannelWeight) {
        this.multichannelWeight = multichannelWeight;
    }
    
    public void setStagePositionX(double stagePositionX) {
        this.stagePositionX = stagePositionX;
    }
    
    public void setStagePositionY(double stagePositionY) {
        this.stagePositionY = stagePositionY;
    }
    
    public void setOriginalStagePositionX(double originalStagePositionX) {
        this.originalStagePositionX = originalStagePositionX;
    }
    
    public void setOriginalStagePositionY(double originalStagePositionY) {
        this.originalStagePositionY = originalStagePositionY;
    }
    
    public void setScaleWidth(double scaleWidth) {
        this.scaleWidth = scaleWidth;
    }
    
    public void setScaleHeight(double scaleHeight) {
        this.scaleHeight = scaleHeight;
    }
    
    public void setCameraFrameStartLeft(int cameraFrameStartLeft) {
        this.cameraFrameStartLeft = cameraFrameStartLeft;
    }
    
    public void setCameraFrameStartTop(int cameraFrameStartTop) {
        this.cameraFrameStartTop = cameraFrameStartTop;    
    }
    
    public void setCameraFrameWidth(int cameraFrameWidth) {
        this.cameraFrameWidth = cameraFrameWidth;
    }
    
    public void setCameraFrameHeight(int cameraFrameHeight) {
        this.cameraFrameHeight = cameraFrameHeight;
    }
    
    public void setCameraFrameImageOrientation(int cameraFrameImageOrientation) {
        this.cameraFrameImageOrientation = cameraFrameImageOrientation;
    }
    
    public void setCameraBinning(int cameraBinning) {
        this.cameraBinning = cameraBinning;
    }
    
    public void setAxioCamSelector(int axioCamSelector) {
        this.axioCamSelector = axioCamSelector;
    }
    
    public void setAxioCamType(int axioCamType) {
        this.axioCamType = axioCamType;
    }
    
    public void setAxioCamResolution(int axioCamResolution) {
        this.axioCamResolution = axioCamResolution;
    }
    
    public void setAxioCamColorModel(int axioCamColorModel) {
        this.axioCamColorModel = axioCamColorModel;
    }
    
    public void setAxioCamMicroScanning(int axioCamMicroScanning) {
        this.axioCamMicroScanning = axioCamMicroScanning;
    }
    
    public void setAmplificationIndex(int amplificationIndex) {
        this.amplificationIndex = amplificationIndex;
    }
    
    public void setAxioCamBlackReference(int axioCamBlackReference) {
        this.axioCamBlackReference = axioCamBlackReference;
    }
    
    public void setCameraShadingCorrection(int cameraShadingCorrection) {
        this.cameraShadingCorrection = cameraShadingCorrection;
    }
    
    public void setAxioCamEnhanceColor(int axioCamEnhanceColor) {
        this.axioCamEnhanceColor = axioCamEnhanceColor;
    }
    
    public void setAxioCamNIRMode(int axioCamNIRMode) {
        this.axioCamNIRMode = axioCamNIRMode;
    }
    
    public void setAxioCamShutterControl(int axioCamShutterControl) {
        this.axioCamShutterControl = axioCamShutterControl;
    }
    
    public void setAxioCamShutterSignal(int axioCamShutterSignal) {
        this.axioCamShutterSignal = axioCamShutterSignal;
    }
    
    public void setAxioCamDelayTime(int axioCamDelayTime) {
        this.axioCamDelayTime = axioCamDelayTime;
    }
    
    public void setMicroscopeMagnification(double microscopeMagnification) {
        this.microscopeMagnification = microscopeMagnification;
    }
    
    public void setReflectedLightHalogenLampVoltage(double reflectedLightHalogenLampVoltage) {
        this.reflectedLightHalogenLampVoltage = reflectedLightHalogenLampVoltage;
    }
    
    public void setReflectedLightHalogenLampMode(int reflectedLightHalogenLampMode) {
        this.reflectedLightHalogenLampMode = reflectedLightHalogenLampMode;
    }
    
    public void setApotomeAutoShutterUsed(int apotomeAutoShutterUsed) {
        this.apotomeAutoShutterUsed = apotomeAutoShutterUsed;
    }
    
    public void setRelFocusPosition1(double relFocusPosition1) {
        this.relFocusPosition1 = relFocusPosition1;
    }
    
    public void setRelFocusPosition2(double relFocusPosition2) {
        this.relFocusPosition2 = relFocusPosition2;
    }
    
    public void setObjectType(int objectType) {
        this.objectType = objectType;
    }
    
    public void setImageMemoryUsage(double imageMemoryUsage) {
        this.imageMemoryUsage = imageMemoryUsage;
    }
    
    public void setChannelName0(int channelNameChannel0, String channelName0) {
        this.channelNameChannel0 = channelNameChannel0;
        this.channelName0 = channelName0;
    }
    
    public void setChannelName1(int channelNameChannel1, String channelName1) {
        this.channelNameChannel1 = channelNameChannel1;
        this.channelName1 = channelName1;
    }
    
    public void setChannelName2(int channelNameChannel2, String channelName2) {
        this.channelNameChannel2 = channelNameChannel2;
        this.channelName2 = channelName2;
    }
    
    public void setChannelName3(int channelNameChannel3, String channelName3) {
        this.channelNameChannel3 = channelNameChannel3;
        this.channelName3 = channelName3;
    }
    
    public void setExcitationWavelength0(int excitationWavelengthChannel0, int excitationWavelength0) {
        this.excitationWavelengthChannel0 = excitationWavelengthChannel0;
        this.excitationWavelength0 = excitationWavelength0;
    }
    
    public void setExcitationWavelength1(int excitationWavelengthChannel1, int excitationWavelength1) {
        this.excitationWavelengthChannel1 = excitationWavelengthChannel1;
        this.excitationWavelength1 = excitationWavelength1;
    }
    
    public void setExcitationWavelength2(int excitationWavelengthChannel2, int excitationWavelength2) {
        this.excitationWavelengthChannel2 = excitationWavelengthChannel2;
        this.excitationWavelength2 = excitationWavelength2;
    }
    
    public void setExcitationWavelength3(int excitationWavelengthChannel3, int excitationWavelength3) {
        this.excitationWavelengthChannel3 = excitationWavelengthChannel3;
        this.excitationWavelength3 = excitationWavelength3;
    }
    
    public void setEmissionWavelength0(int emissionWavelengthChannel0, int emissionWavelength0) {
        this.emissionWavelengthChannel0 = emissionWavelengthChannel0;
        this.emissionWavelength0 = emissionWavelength0;
    }
    
    public void setEmissionWavelength1(int emissionWavelengthChannel1, int emissionWavelength1) {
        this.emissionWavelengthChannel1 = emissionWavelengthChannel1;
        this.emissionWavelength1 = emissionWavelength1;
    }
    
    public void setEmissionWavelength2(int emissionWavelengthChannel2, int emissionWavelength2) {
        this.emissionWavelengthChannel2 = emissionWavelengthChannel2;
        this.emissionWavelength2 = emissionWavelength2;
    }
    
    public void setEmissionWavelength3(int emissionWavelengthChannel3, int emissionWavelength3) {
        this.emissionWavelengthChannel3 = emissionWavelengthChannel3;
        this.emissionWavelength3 = emissionWavelength3;
    }
    
    public void setCameraImageAcquisitionTime0(String cameraImageAcquisitionTime0) {
        this.cameraImageAcquisitionTime0 = cameraImageAcquisitionTime0;
    }
    
    public void setCameraImageAcquisitionTime1(String cameraImageAcquisitionTime1) {
        this.cameraImageAcquisitionTime1 = cameraImageAcquisitionTime1;
    }
    
    public void setCameraImageAcquisitionTime2(String cameraImageAcquisitionTime2) {
        this.cameraImageAcquisitionTime2 = cameraImageAcquisitionTime2;
    }
    
    public void setCameraImageAcquisitionTime3(String cameraImageAcquisitionTime3) {
        this.cameraImageAcquisitionTime3 = cameraImageAcquisitionTime3;
    }
    
    public void setImageRelativeTime0(String imageRelativeTime0) {
        this.imageRelativeTime0 = imageRelativeTime0;
    }
    
    public void setImageRelativeTime1(String imageRelativeTime1) {
        this.imageRelativeTime1 = imageRelativeTime1;
    }
    
    public void setImageRelativeTime2(String imageRelativeTime2) {
        this.imageRelativeTime2 = imageRelativeTime2;
    }
    
    public void setImageRelativeTime3(String imageRelativeTime3) {
        this.imageRelativeTime3 = imageRelativeTime3;
    }
    
    public void setFileDate(String fileDate) {
        this.fileDate = fileDate;
    }
    
    public void setFileSize(int fileSize) {
        this.fileSize = fileSize;
    }
    
    public void setChannel0(int channel0) {
        this.channel0 = channel0;
    }
    
    public void setChannel1(int channel1) {
        this.channel1 = channel1;
    }
    
    public void setChannel2(int channel2) {
        this.channel2 = channel2;
    }
    
    public void setChannel3(int channel3) {
        this.channel3 = channel3;
    }
    
    public void setAxioCamAnalogGainEnable(int axioCamAnalogGainEnable) {
        this.axioCamAnalogGainEnable = axioCamAnalogGainEnable;
    }
    
    public void setCameraShutterLiveEnable(int cameraShutterLiveEnable) {
        this.cameraShutterLiveEnable = cameraShutterLiveEnable;
    }
    
    public void setAxioCamSaturation(double axioCamSaturation) {
        this.axioCamSaturation = axioCamSaturation;
    }
    
    public void setCondenserContrast(int condenserContrast) {
        this.condenserContrast = condenserContrast;
    }
    
    public void setCondenserNumericalAperture(double condenserNumericalAperture) {
        this.condenserNumericalAperture = condenserNumericalAperture;
    }
    
    public void setStageCalibrated(String stageCalibrated) {
        this.stageCalibrated = stageCalibrated;
    }
    
    public void setOptovar(double optovar) {
        this.optovar = optovar;
    }
    
    public void setCameraBitDepth(int cameraBitDepth) {
        this.cameraBitDepth = cameraBitDepth;
    }
    
    public void setExternalShutter1(int externalShutter1) {
        this.externalShutter1 = externalShutter1;
    }
    
    public void setDocumentType(String documentType) {
        this.documentType = documentType;
    }
    
    public void setFilename(String IDTagFileName) {
        this.IDTagFileName = IDTagFileName;
    }
    
    public void setUserID(String userID) {
        this.userID = userID;
    }
    
    public void setUserName(String userName) {
        this.userName = userName;
    }
    
    public void setUserCity(String userCity) {
        this.userCity = userCity;
    }
    
    public void setUserAddress(String userAddress) {
        this.userAddress = userAddress;
    }
    
    public void setUserPhone(String userPhone) {
        this.userPhone = userPhone;
    }
    
    public void setUserFax(String userFax) {
        this.userFax = userFax;
    }
    
    public void setUserCountry(String userCountry) {
        this.userCountry = userCountry;
    }
    
    public void setUserCompany(String userCompany) {
        this.userCompany = userCompany;
    }
    
    public void setUserCompanyLogo(String userCompanyLogo) {
        this.userCompanyLogo = userCompanyLogo;
    }
    
    public void setDeviceScalingName(String deviceScalingName) {
        this.deviceScalingName = deviceScalingName;
    }
    
    public void setCamera(String camera) {
        this.camera = camera;
    }
    
    public void setMicroscopeName(String microscopeName) {
        this.microscopeName = microscopeName;
    }
    
    public void setObjectiveID(String objectiveID) {
        this.objectiveID = objectiveID;
    }
    
    public void setObjectiveName(String objectiveName) {
        this.objectiveName = objectiveName;
    }
    
    public void setReflectorID(String reflectorID) {
        this.reflectorID = reflectorID;
    }
    
    public void setReflector(String reflector) {
        this.reflector = reflector;
    }
    
    public void setReference(String reference) {
        this.reference = reference;
    }
    
    public void setFileLink(String fileLink) {
        this.fileLink = fileLink;
    }
    
    public void setFileID(String fileID) {
        this.fileID = fileID;
    }
    
    public void setLastModifiedBy(String lastModifiedBy) {
        this.lastModifiedBy = lastModifiedBy;
    }
    
    public void setTitle(String title) {
        this.title = title;
    }
    
    public void setApotomeGridName(String apotomeGridName) {
        this.apotomeGridName = apotomeGridName;
    }
    
    public void setApotomeFilterName(String apotomeFilterName) {
        this.apotomeFilterName = apotomeFilterName;
    }
    
    public void setApotomeCamPhaseAngles(String apotomeCamPhaseAngles) {
        this.apotomeCamPhaseAngles = apotomeCamPhaseAngles;
    }
    
    public void setApotomeCamGenericCameraName(String apotomeCamGenericCameraName) {
        this.apotomeCamGenericCameraName = apotomeCamGenericCameraName;
    }
    
    public void setDocumentSubtype(String documentSubtype) {
        this.documentSubtype = documentSubtype;
    }
    
    public void setComments(String comments) {
        this.comments = comments;
    }

}
