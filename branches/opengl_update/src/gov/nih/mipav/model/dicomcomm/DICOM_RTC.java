package gov.nih.mipav.model.dicomcomm;


// ***********************************************************
// WARNING: This file is generated automatically.
// Any edits will be lost upon regeneration.
// Please modify the generator instead.
// See CreateDICOMFiles
// ***********************************************************

// please see the copyright notice in the LICENSE.txt file

import java.util.*;


/**
 * DOCUMENT ME!
 */
class RTCEntry {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int dd_type;

    /** DOCUMENT ME! */
    String description;

    /** DOCUMENT ME! */
    int element;

    /** DOCUMENT ME! */
    int group;

    /** DOCUMENT ME! */
    int typeCode;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Loads the inputted variabels straight into the class.
     *
     * @param  group        DOCUMENT ME!
     * @param  element      DOCUMENT ME!
     * @param  typeCode     DOCUMENT ME!
     * @param  description  DOCUMENT ME!
     * @param  dd_type      DOCUMENT ME!
     */
    RTCEntry(int group, int element, int typeCode, String description, int dd_type) {
        this.group = group;
        this.element = element;
        this.typeCode = typeCode;
        this.description = description;
        this.dd_type = dd_type;
    }
}


/**
 * RTC - Run Time Class (although it is actually statically defined here) This allows access to information in the DICOM
 * data dictionary. Each group, element pair is accessed via a unique static integer variable defined here. This allows
 * for extremely fast access to this table in a readable source code fashion. All integer hash indices are easily
 * readable representations of group-element pairs. For example, (0010,0010) is represented by RTC.DD_PatientName.
 * (0010,0020) is RTC.DD_PatientID, etc. If there is a group-element pair you require use of which is not hashed as an
 * integer here, use the <A HREF="#unknownDDType">unknownDDType()</A> method to hash it on the fly and receive a new
 * integer hash index. *
 *
 * <p>Once an integer hash index is available, the following methods may be called: <A HREF="#getGroup">getGroup()</A>,
 * <A HREF="#getElement">getElement()</A>, <A HREF="#getTypeCode">getTypeCode()</A>, and <A HREF="#getTypeCodeName">
 * getTypeCodeName()</A>, and <A HREF="#getDescription">getDescription()</A></p>
 *
 * <p>See the main() routine in the source for usage example</p>
 */
public class DICOM_RTC {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int TYPE_UNKNOWN = 0;

    /** DOCUMENT ME! */
    public static final int TYPE_CS = 1;

    /** DOCUMENT ME! */
    public static final int TYPE_DS = 2;

    /** DOCUMENT ME! */
    public static final int TYPE_SQ = 3;

    /** DOCUMENT ME! */
    public static final int TYPE_US = 4;

    /** DOCUMENT ME! */
    public static final int TYPE_LO = 5;

    /** DOCUMENT ME! */
    public static final int TYPE_IS = 6;

    /** DOCUMENT ME! */
    public static final int TYPE_LT = 7;

    /** DOCUMENT ME! */
    public static final int TYPE_TM = 8;

    /** DOCUMENT ME! */
    public static final int TYPE_SH = 9;

    /** DOCUMENT ME! */
    public static final int TYPE_DA = 10;

    /** DOCUMENT ME! */
    public static final int TYPE_PN = 11;

    /** DOCUMENT ME! */
    public static final int TYPE_ST = 12;

    /** DOCUMENT ME! */
    public static final int TYPE_AE = 13;

    /** DOCUMENT ME! */
    public static final int TYPE_AT = 14;

    /** DOCUMENT ME! */
    public static final int TYPE_OB = 15;

    /** DOCUMENT ME! */
    public static final int TYPE_UL = 16;

    /** DOCUMENT ME! */
    public static final int TYPE_UI = 17;

    /** DOCUMENT ME! */
    public static final int TYPE_SS = 18;

    /** DOCUMENT ME! */
    public static final int TYPE_OW = 19;

    /** DOCUMENT ME! */
    public static final int TYPE_UT = 20;

    /** DOCUMENT ME! */
    public static final int TYPE_FL = 21;

    /** DOCUMENT ME! */
    public static final int TYPE_AS = 22;

    /** DOCUMENT ME! */
    public static final int TYPE_DT = 23;

    /** DOCUMENT ME! */
    public static final int TYPE_NONE = 24;

    /** DOCUMENT ME! */
    public static final int TYPE_FD = 25;

    /** DOCUMENT ME! */
    public static final int TYPE_SL = 26;

    /** DOCUMENT ME! */
    public static final int DD_UNDEFINED = 0;

    /** DOCUMENT ME! */
    public static final int DD_SmoothingType = 1;

    /** DOCUMENT ME! */
    public static final int DD_GantryRotationDirection = 2;

    /** DOCUMENT ME! */
    public static final int DD_GantryAngle = 3;

    /** DOCUMENT ME! */
    public static final int DD_LeafJawPositions = 4;

    /** DOCUMENT ME! */
    public static final int DD_MACParametersSequence = 5;

    /** DOCUMENT ME! */
    public static final int DD_MACIDNumber = 6;

    /** DOCUMENT ME! */
    public static final int DD_BeamLimitingDevicePositionSequence = 7;

    /** DOCUMENT ME! */
    public static final int DD_RequestedProcedureCodeSequence = 8;

    /** DOCUMENT ME! */
    public static final int DD_WedgePosition = 9;

    /** DOCUMENT ME! */
    public static final int DD_RequestedProcedureDescription = 10;

    /** DOCUMENT ME! */
    public static final int DD_XrayTubeCurrentInuA = 11;

    /** DOCUMENT ME! */
    public static final int DD_WedgePositionSequence = 12;

    /** DOCUMENT ME! */
    public static final int DD_ExposureTimeInuS = 13;

    /** DOCUMENT ME! */
    public static final int DD_DoseRateSet = 14;

    /** DOCUMENT ME! */
    public static final int DD_NominalBeamEnergy = 15;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfTemporalPositions = 16;

    /** DOCUMENT ME! */
    public static final int DD_ControlPointIndex = 17;

    /** DOCUMENT ME! */
    public static final int DD_ControlPointSequence = 18;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfControlPoints = 19;

    /** DOCUMENT ME! */
    public static final int DD_PhototimerSetting = 20;

    /** DOCUMENT ME! */
    public static final int DD_ExposureStatus = 21;

    /** DOCUMENT ME! */
    public static final int DD_TemporalPositionIdentifier = 22;

    /** DOCUMENT ME! */
    public static final int DD_ExposureControlModeDescription = 23;

    /** DOCUMENT ME! */
    public static final int DD_ExposureControlMode = 24;

    /** DOCUMENT ME! */
    public static final int DD_CoordinateStepValue = 25;

    /** DOCUMENT ME! */
    public static final int DD_FinalCumulativeMetersetWeight = 26;

    /** DOCUMENT ME! */
    public static final int DD_CoordinateStartValue = 27;

    /** DOCUMENT ME! */
    public static final int DD_CumulativeDoseReferenceCoefficient = 28;

    /** DOCUMENT ME! */
    public static final int DD_MessageIDBeingRespondedTo = 29;

    /** DOCUMENT ME! */
    public static final int DD_CurveDataDescriptor = 30;

    /** DOCUMENT ME! */
    public static final int DD_ApplicatorDescription = 31;

    /** DOCUMENT ME! */
    public static final int DD_StudyComponentStatusID = 32;

    /** DOCUMENT ME! */
    public static final int DD_ApplicatorType = 33;

    /** DOCUMENT ME! */
    public static final int DD_StudyCompletionTime = 34;

    /** DOCUMENT ME! */
    public static final int DD_ApplicatorID = 35;

    /** DOCUMENT ME! */
    public static final int DD_StudyCompletionDate = 36;

    /** DOCUMENT ME! */
    public static final int DD_ApplicatorSequence = 37;

    /** DOCUMENT ME! */
    public static final int DD_BlockData = 38;

    /** DOCUMENT ME! */
    public static final int DD_BlockNumberOfPoints = 39;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedPrintJobSequencePull = 40;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedCalculatedDoseReferenceNumber = 41;

    /** DOCUMENT ME! */
    public static final int DD_BlockTransmission = 42;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedCalculatedDoseReferenceSequence = 43;

    /** DOCUMENT ME! */
    public static final int DD_BlockThickness = 44;

    /** DOCUMENT ME! */
    public static final int DD_FilterThicknessMaximum = 45;

    /** DOCUMENT ME! */
    public static final int DD_FilterThicknessMinimum = 46;

    /** DOCUMENT ME! */
    public static final int DD_FilterMaterial = 47;

    /** DOCUMENT ME! */
    public static final int DD_PatientPrimaryLanguageModifierCodeSequence = 48;

    /** DOCUMENT ME! */
    public static final int DD_PatientPrimaryLanguageCodeSequence = 49;

    /** DOCUMENT ME! */
    public static final int DD_CurveRange = 50;

    /** DOCUMENT ME! */
    public static final int DD_MagnificationType = 51;

    /** DOCUMENT ME! */
    public static final int DD_MaximumCoordinateValue = 52;

    /** DOCUMENT ME! */
    public static final int DD_MinimumCoordinateValue = 53;

    /** DOCUMENT ME! */
    public static final int DD_DataValueRepresentation = 54;

    /** DOCUMENT ME! */
    public static final int DD_RelativeXrayExposure = 55;

    /** DOCUMENT ME! */
    public static final int DD_ExposuresOnPlate = 56;

    /** DOCUMENT ME! */
    public static final int DD_CassetteSize = 57;

    /** DOCUMENT ME! */
    public static final int DD_MessageID = 58;

    /** DOCUMENT ME! */
    public static final int DD_CassetteOrientation = 59;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionDeviceProcessingCode = 60;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionDeviceProcessingDescription = 61;

    /** DOCUMENT ME! */
    public static final int DD_GridFocalDistance = 62;

    /** DOCUMENT ME! */
    public static final int DD_SpecimenAccessionNumber = 63;

    /** DOCUMENT ME! */
    public static final int DD_BurnedInAnnotation = 64;

    /** DOCUMENT ME! */
    public static final int DD_QualityControlImage = 65;

    /** DOCUMENT ME! */
    public static final int DD_StudyArrivalTime = 66;

    /** DOCUMENT ME! */
    public static final int DD_StudyArrivalDate = 67;

    /** DOCUMENT ME! */
    public static final int DD_ImageBoxPresentationLUTFlag = 68;

    /** DOCUMENT ME! */
    public static final int DD_GridPeriod = 69;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedMeasuredDoseReferenceNumber = 70;

    /** DOCUMENT ME! */
    public static final int DD_GridAspectRatio = 71;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedMeasuredDoseReferenceSequence = 72;

    /** DOCUMENT ME! */
    public static final int DD_GridPitch = 73;

    /** DOCUMENT ME! */
    public static final int DD_DefaultPrinterResolutionID = 74;

    /** DOCUMENT ME! */
    public static final int DD_GridThickness = 75;

    /** DOCUMENT ME! */
    public static final int DD_PresentationLUTFlag = 76;

    /** DOCUMENT ME! */
    public static final int DD_GridSpacingMaterial = 77;

    /** DOCUMENT ME! */
    public static final int DD_PrinterResolutionID = 78;

    /** DOCUMENT ME! */
    public static final int DD_GridAbsorbingMaterial = 79;

    /** DOCUMENT ME! */
    public static final int DD_ImageOverlayFlag = 80;

    /** DOCUMENT ME! */
    public static final int DD_FilmSizeID = 81;

    /** DOCUMENT ME! */
    public static final int DD_AnnotationFlag = 82;

    /** DOCUMENT ME! */
    public static final int DD_CollationFlag = 83;

    /** DOCUMENT ME! */
    public static final int DD_ColorImagePrintingFlag = 84;

    /** DOCUMENT ME! */
    public static final int DD_MaximumMemoryAllocation = 85;

    /** DOCUMENT ME! */
    public static final int DD_CommandField = 86;

    /** DOCUMENT ME! */
    public static final int DD_MemoryAllocation = 87;

    /** DOCUMENT ME! */
    public static final int DD_EndMeterset = 88;

    /** DOCUMENT ME! */
    public static final int DD_RequestingService = 89;

    /** DOCUMENT ME! */
    public static final int DD_RequestingPhysician = 90;

    /** DOCUMENT ME! */
    public static final int DD_StartMeterset = 91;

    /** DOCUMENT ME! */
    public static final int DD_ReasonForStudy = 92;

    /** DOCUMENT ME! */
    public static final int DD_CalculatedDoseReferenceDoseValue = 93;

    /** DOCUMENT ME! */
    public static final int DD_CalculatedDoseReferenceDescription = 94;

    /** DOCUMENT ME! */
    public static final int DD_CalculatedDoseReferenceNumber = 95;

    /** DOCUMENT ME! */
    public static final int DD_CalculatedDoseReferenceSequence = 96;

    /** DOCUMENT ME! */
    public static final int DD_FieldOfViewHorizontalFlip = 97;

    /** DOCUMENT ME! */
    public static final int DD_FieldOfViewRotation = 98;

    /** DOCUMENT ME! */
    public static final int DD_FieldOfViewOrigin = 99;

    /** DOCUMENT ME! */
    public static final int DD_FilmOrientation = 100;

    /** DOCUMENT ME! */
    public static final int DD_BasicColorImageSequence = 101;

    /** DOCUMENT ME! */
    public static final int DD_FilmSessionLabel = 102;

    /** DOCUMENT ME! */
    public static final int DD_BasicGrayscaleImageSequence = 103;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStudyLocationAETitle = 104;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStudyLocation = 105;

    /** DOCUMENT ME! */
    public static final int DD_CurveReferencedOverlayGroup = 106;

    /** DOCUMENT ME! */
    public static final int DD_OverrideReason = 107;

    /** DOCUMENT ME! */
    public static final int DD_MeasuredDoseReferenceNumber = 108;

    /** DOCUMENT ME! */
    public static final int DD_DetectorActiveOrigin = 109;

    /** DOCUMENT ME! */
    public static final int DD_OverrideParameterPointer = 110;

    /** DOCUMENT ME! */
    public static final int DD_DetectorActiveDimensions = 111;

    /** DOCUMENT ME! */
    public static final int DD_OverrideSequence = 112;

    /** DOCUMENT ME! */
    public static final int DD_DetectorActiveShape = 113;

    /** DOCUMENT ME! */
    public static final int DD_EntranceDoseInmGy = 114;

    /** DOCUMENT ME! */
    public static final int DD_DetectorElementSpacing = 115;

    /** DOCUMENT ME! */
    public static final int DD_DetectorElementPhysicalSize = 116;

    /** DOCUMENT ME! */
    public static final int DD_AnnotationDisplayFormatID = 117;

    /** DOCUMENT ME! */
    public static final int DD_FilmDestination = 118;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFractionsDelivered = 119;

    /** DOCUMENT ME! */
    public static final int DD_DetectorBinning = 120;

    /** DOCUMENT ME! */
    public static final int DD_CertifiedTimestamp = 121;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStudyStopTime = 122;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStudyStopDate = 123;

    /** DOCUMENT ME! */
    public static final int DD_CurveReferencedOverlaySequence = 124;

    /** DOCUMENT ME! */
    public static final int DD_MostRecentTreatmentDate = 125;

    /** DOCUMENT ME! */
    public static final int DD_FirstTreatmentDate = 126;

    /** DOCUMENT ME! */
    public static final int DD_CumulativeDoseToDoseReference = 127;

    /** DOCUMENT ME! */
    public static final int DD_DetectorActivationOffsetFromExposure = 128;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentSummaryCalculatedDoseReferenceSequence = 129;

    /** DOCUMENT ME! */
    public static final int DD_DetectorActiveTime = 130;

    /** DOCUMENT ME! */
    public static final int DD_DetectorTimeSinceLastExposure = 131;

    /** DOCUMENT ME! */
    public static final int DD_ExposuresOnDetectorSinceManufactured = 132;

    /** DOCUMENT ME! */
    public static final int DD_ExposuresOnDetectorSinceLastCalibration = 133;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedPresentationLUTSequence = 134;

    /** DOCUMENT ME! */
    public static final int DD_MediumType = 135;

    /** DOCUMENT ME! */
    public static final int DD_TimeOfLastDetectorCalibration = 136;

    /** DOCUMENT ME! */
    public static final int DD_CertifiedTimestampType = 137;

    /** DOCUMENT ME! */
    public static final int DD_VerticesOfPolygonalCollimator = 138;

    /** DOCUMENT ME! */
    public static final int DD_DateOfLastDetectorCalibration = 139;

    /** DOCUMENT ME! */
    public static final int DD_DetectorID = 140;

    /** DOCUMENT ME! */
    public static final int DD_DoseRateDelivered = 141;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStudyStartTime = 142;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStudyStartDate = 143;

    /** DOCUMENT ME! */
    public static final int DD_DeliveredMeterset = 144;

    /** DOCUMENT ME! */
    public static final int DD_DetectorMode = 145;

    /** DOCUMENT ME! */
    public static final int DD_SpecifiedMeterset = 146;

    /** DOCUMENT ME! */
    public static final int DD_DetectorDescription = 147;

    /** DOCUMENT ME! */
    public static final int DD_ControlPointDeliverySequence = 148;

    /** DOCUMENT ME! */
    public static final int DD_ROIStandardDeviation = 149;

    /** DOCUMENT ME! */
    public static final int DD_DetectorConfiguration = 150;

    /** DOCUMENT ME! */
    public static final int DD_ROIMean = 151;

    /** DOCUMENT ME! */
    public static final int DD_DetectorType = 152;

    /** DOCUMENT ME! */
    public static final int DD_ROIArea = 153;

    /** DOCUMENT ME! */
    public static final int DD_PrinterPixelSpacing = 154;

    /** DOCUMENT ME! */
    public static final int DD_DetectorTemperature = 155;

    /** DOCUMENT ME! */
    public static final int DD_DetectorConditionsNominalFlag = 156;

    /** DOCUMENT ME! */
    public static final int DD_ImageDisplayFormat = 157;

    /** DOCUMENT ME! */
    public static final int DD_DeliveredTreatmentTime = 158;

    /** DOCUMENT ME! */
    public static final int DD_PrintPriority = 159;

    /** DOCUMENT ME! */
    public static final int DD_SpecifiedTreatmentTime = 160;

    /** DOCUMENT ME! */
    public static final int DD_RadiusOfCircularCollimator = 161;

    /** DOCUMENT ME! */
    public static final int DD_CenterOfCircularCollimator = 162;

    /** DOCUMENT ME! */
    public static final int DD_PrinterConfigurationSequence = 163;

    /** DOCUMENT ME! */
    public static final int DD_DeliveredSecondaryMeterset = 164;

    /** DOCUMENT ME! */
    public static final int DD_DeliveredPrimaryMeterset = 165;

    /** DOCUMENT ME! */
    public static final int DD_SpecifiedSecondaryMeterset = 166;

    /** DOCUMENT ME! */
    public static final int DD_SpecifiedPrimaryMeterset = 167;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedTreatmentRecordSequence = 168;

    /** DOCUMENT ME! */
    public static final int DD_FilmBoxGroupLength = 169;

    /** DOCUMENT ME! */
    public static final int DD_CollimatorLowerHorizontalEdge = 170;

    /** DOCUMENT ME! */
    public static final int DD_CollimatorUpperHorizontalEdge = 171;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentVerificationStatus = 172;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentTerminationCode = 173;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfCopies = 174;

    /** DOCUMENT ME! */
    public static final int DD_CollimatorRightVerticalEdge = 175;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentTerminationStatus = 176;

    /** DOCUMENT ME! */
    public static final int DD_CollimatorLeftVerticalEdge = 177;

    /** DOCUMENT ME! */
    public static final int DD_CollimatorShape = 178;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentControlPointTime = 179;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentControlPointDate = 180;

    /** DOCUMENT ME! */
    public static final int DD_CurrentFractionNumber = 181;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfReferences = 182;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentSessionBeamSequence = 183;

    /** DOCUMENT ME! */
    public static final int DD_FilmSessionGroupLength = 184;

    /** DOCUMENT ME! */
    public static final int DD_MeasuredDoseValue = 185;

    /** DOCUMENT ME! */
    public static final int DD_MeasuredDoseType = 186;

    /** DOCUMENT ME! */
    public static final int DD_MeasuredDoseDescription = 187;

    /** DOCUMENT ME! */
    public static final int DD_MeasuredDoseReferenceSequence = 188;

    /** DOCUMENT ME! */
    public static final int DD_BlockName = 189;

    /** DOCUMENT ME! */
    public static final int DD_BlockNumber = 190;

    /** DOCUMENT ME! */
    public static final int DD_BlockDivergence = 191;

    /** DOCUMENT ME! */
    public static final int DD_BlockType = 192;

    /** DOCUMENT ME! */
    public static final int DD_SourceToBlockTrayDistance = 193;

    /** DOCUMENT ME! */
    public static final int DD_BlockTrayID = 194;

    /** DOCUMENT ME! */
    public static final int DD_BlockSequence = 195;

    /** DOCUMENT ME! */
    public static final int DD_TotalBlockTrayFactor = 196;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfBlocks = 197;

    /** DOCUMENT ME! */
    public static final int DD_AngularViewVector = 198;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorType = 199;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfBoli = 200;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorThicknessData = 201;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorTransmissionData = 202;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorPosition = 203;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorPixelSpacing = 204;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorColumns = 205;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorRows = 206;

    /** DOCUMENT ME! */
    public static final int DD_SourceToCompensatorTrayDistance = 207;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorID = 208;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorNumber = 209;

    /** DOCUMENT ME! */
    public static final int DD_CompensatorSequence = 210;

    /** DOCUMENT ME! */
    public static final int DD_TotalCompensatorTrayFactor = 211;

    /** DOCUMENT ME! */
    public static final int DD_MaterialID = 212;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfCompensators = 213;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfSlices = 214;

    /** DOCUMENT ME! */
    public static final int DD_SliceVector = 215;

    /** DOCUMENT ME! */
    public static final int DD_SourceToWedgeTrayDistance = 216;

    /** DOCUMENT ME! */
    public static final int DD_WedgeOrientation = 217;

    /** DOCUMENT ME! */
    public static final int DD_WedgeFactor = 218;

    /** DOCUMENT ME! */
    public static final int DD_WedgeAngle = 219;

    /** DOCUMENT ME! */
    public static final int DD_WedgeID = 220;

    /** DOCUMENT ME! */
    public static final int DD_WedgeType = 221;

    /** DOCUMENT ME! */
    public static final int DD_WedgeNumber = 222;

    /** DOCUMENT ME! */
    public static final int DD_TimeSlotTime = 223;

    /** DOCUMENT ME! */
    public static final int DD_WedgeSequence = 224;

    /** DOCUMENT ME! */
    public static final int DD_TimeSlotInformationSequence = 225;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfWedges = 226;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfTimeSlots = 227;

    /** DOCUMENT ME! */
    public static final int DD_TimeSlotVector = 228;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentDeliveryType = 229;

    /** DOCUMENT ME! */
    public static final int DD_ImagingDeviceSpecificAcquisitionParameters = 230;

    /** DOCUMENT ME! */
    public static final int DD_PlannedVerificationImageSequence = 231;

    /** DOCUMENT ME! */
    public static final int DD_ReferenceImageNumber = 232;

    /** DOCUMENT ME! */
    public static final int DD_HighDoseTechniqueType = 233;

    /** DOCUMENT ME! */
    public static final int DD_RadiationType = 234;

    /** DOCUMENT ME! */
    public static final int DD_BeamType = 235;

    /** DOCUMENT ME! */
    public static final int DD_BeamDescription = 236;

    /** DOCUMENT ME! */
    public static final int DD_BeamName = 237;

    /** DOCUMENT ME! */
    public static final int DD_DataInformationSequence = 238;

    /** DOCUMENT ME! */
    public static final int DD_GatedInformationSequence = 239;

    /** DOCUMENT ME! */
    public static final int DD_BeamNumber = 240;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfRRIntervals = 241;

    /** DOCUMENT ME! */
    public static final int DD_RRIntervalVector = 242;

    /** DOCUMENT ME! */
    public static final int DD_LeafPositionBoundaries = 243;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfLeafJawPairs = 244;

    /** DOCUMENT ME! */
    public static final int DD_DistributionAddress = 245;

    /** DOCUMENT ME! */
    public static final int DD_SourceToBeamLimitingDeviceDistance = 246;

    /** DOCUMENT ME! */
    public static final int DD_RTBeamLimitingDeviceType = 247;

    /** DOCUMENT ME! */
    public static final int DD_BeamLimitingDeviceSequence = 248;

    /** DOCUMENT ME! */
    public static final int DD_SourceAxisDistance = 249;

    /** DOCUMENT ME! */
    public static final int DD_PrimaryDosimeterUnit = 250;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentMachineName = 251;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFramesInRotation = 252;

    /** DOCUMENT ME! */
    public static final int DD_RotationInformationSequence = 253;

    /** DOCUMENT ME! */
    public static final int DD_BeamSequence = 254;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfRotations = 255;

    /** DOCUMENT ME! */
    public static final int DD_RotationVector = 256;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationAuthor = 257;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationText = 258;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationTranscriber = 259;

    /** DOCUMENT ME! */
    public static final int DD_Priority = 260;

    /** DOCUMENT ME! */
    public static final int DD_DistributionName = 261;

    /** DOCUMENT ME! */
    public static final int DD_IconImageSequence = 262;

    /** DOCUMENT ME! */
    public static final int DD_ResultsDistributionListSequence = 263;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationDiagnosisCodeSequence = 264;

    /** DOCUMENT ME! */
    public static final int DD_ProjectionEponymousNameCodeSequence = 265;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationDiagnosisDescription = 266;

    /** DOCUMENT ME! */
    public static final int DD_PhysicianApprovingInterpretation = 267;

    /** DOCUMENT ME! */
    public static final int DD_BrachyApplicationSetupDose = 268;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationApprovalTime = 269;

    /** DOCUMENT ME! */
    public static final int DD_ViewPosition = 270;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationApprovalDate = 271;

    /** DOCUMENT ME! */
    public static final int DD_BrachyApplicationSetupDoseSpecificationPoint = 272;

    /** DOCUMENT ME! */
    public static final int DD_PatientPosition = 273;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationApproverSequence = 274;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfBrachyApplicationSetups = 275;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationTranscriptionTime = 276;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationTranscriptionDate = 277;

    /** DOCUMENT ME! */
    public static final int DD_PauseBetweenFrames = 278;

    /** DOCUMENT ME! */
    public static final int DD_PhaseDelay = 279;

    /** DOCUMENT ME! */
    public static final int DD_ReferenceToRecordedSound = 280;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationRecorder = 281;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFramesInPhase = 282;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationRecordedTime = 283;

    /** DOCUMENT ME! */
    public static final int DD_PhaseInformationSequence = 284;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationRecordedDate = 285;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfPhases = 286;

    /** DOCUMENT ME! */
    public static final int DD_PhaseVector = 287;

    /** DOCUMENT ME! */
    public static final int DD_RecommendedViewingMode = 288;

    /** DOCUMENT ME! */
    public static final int DD_DetectorInformationSequence = 289;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfDetectors = 290;

    /** DOCUMENT ME! */
    public static final int DD_DetectorVector = 291;

    /** DOCUMENT ME! */
    public static final int DD_FillerOrderNumberOfImagingServiceRequest = 292;

    /** DOCUMENT ME! */
    public static final int DD_PlacerOrderNumberOfImagingServiceRequest = 293;

    /** DOCUMENT ME! */
    public static final int DD_TriggerWindow = 294;

    /** DOCUMENT ME! */
    public static final int DD_OrderCallbackPhoneNumber = 295;

    /** DOCUMENT ME! */
    public static final int DD_CardiacNumberOfImages = 296;

    /** DOCUMENT ME! */
    public static final int DD_ScatterCorrectionMethod = 297;

    /** DOCUMENT ME! */
    public static final int DD_DetectorLinesOfResponseUsed = 298;

    /** DOCUMENT ME! */
    public static final int DD_ReconstructionMethod = 299;

    /** DOCUMENT ME! */
    public static final int DD_DecayCorrection = 300;

    /** DOCUMENT ME! */
    public static final int DD_AttenuationCorrectionMethod = 301;

    /** DOCUMENT ME! */
    public static final int DD_RandomsCorrectionMethod = 302;

    /** DOCUMENT ME! */
    public static final int DD_EnergyWindowName = 303;

    /** DOCUMENT ME! */
    public static final int DD_ResidualSyringeCounts = 304;

    /** DOCUMENT ME! */
    public static final int DD_RadiopharmaceuticalInformationSequence = 305;

    /** DOCUMENT ME! */
    public static final int DD_EnergyWindowUpperLimit = 306;

    /** DOCUMENT ME! */
    public static final int DD_EnergyWindowLowerLimit = 307;

    /** DOCUMENT ME! */
    public static final int DD_EnergyWindowRangeSequence = 308;

    /** DOCUMENT ME! */
    public static final int DD_EnergyWindowInformationSequence = 309;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfEnergyWindows = 310;

    /** DOCUMENT ME! */
    public static final int DD_EnergyWindowVector = 311;

    /** DOCUMENT ME! */
    public static final int DD_OrderEntererLocation = 312;

    /** DOCUMENT ME! */
    public static final int DD_OrderEnteredBy = 313;

    /** DOCUMENT ME! */
    public static final int DD_HeartRate = 314;

    /** DOCUMENT ME! */
    public static final int DD_IssueTimeOfImagingServiceRequest = 315;

    /** DOCUMENT ME! */
    public static final int DD_IssueDateOfImagingServiceRequest = 316;

    /** DOCUMENT ME! */
    public static final int DD_SkipBeats = 317;

    /** DOCUMENT ME! */
    public static final int DD_PrivateInformation = 318;

    /** DOCUMENT ME! */
    public static final int DD_PVCRejection = 319;

    /** DOCUMENT ME! */
    public static final int DD_IntervalsRejected = 320;

    /** DOCUMENT ME! */
    public static final int DD_PrivateInformationCreatorUID = 321;

    /** DOCUMENT ME! */
    public static final int DD_ReasonForImagingServiceRequest = 322;

    /** DOCUMENT ME! */
    public static final int DD_IntervalsAcquired = 323;

    /** DOCUMENT ME! */
    public static final int DD_HighRRValue = 324;

    /** DOCUMENT ME! */
    public static final int DD_DecimateCropResult = 325;

    /** DOCUMENT ME! */
    public static final int DD_LowRRValue = 326;

    /** DOCUMENT ME! */
    public static final int DD_BeatRejectionFlag = 327;

    /** DOCUMENT ME! */
    public static final int DD_RequestedImageSizeFlag = 328;

    /** DOCUMENT ME! */
    public static final int DD_ManufacturerModelName = 329;

    /** DOCUMENT ME! */
    public static final int DD_SpecialNeeds = 330;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFilms = 331;

    /** DOCUMENT ME! */
    public static final int DD_NuclearAcquisitionGroupLength = 332;

    /** DOCUMENT ME! */
    public static final int DD_RadiopharmaceuticalSpecificActivity = 333;

    /** DOCUMENT ME! */
    public static final int DD_RadionuclidePositronFraction = 334;

    /** DOCUMENT ME! */
    public static final int DD_RadionuclideHalfLife = 335;

    /** DOCUMENT ME! */
    public static final int DD_RadionuclideTotalDose = 336;

    /** DOCUMENT ME! */
    public static final int DD_RadiopharmaceuticalStopTime = 337;

    /** DOCUMENT ME! */
    public static final int DD_RadiopharmaceuticalStartTime = 338;

    /** DOCUMENT ME! */
    public static final int DD_RadiopharmaceuticalVolume = 339;

    /** DOCUMENT ME! */
    public static final int DD_RadiopharmaceuticalRoute = 340;

    /** DOCUMENT ME! */
    public static final int DD_AdmittingDiagnosesCodeSequence = 341;

    /** DOCUMENT ME! */
    public static final int DD_AdmittingDiagnosesDescription = 342;

    /** DOCUMENT ME! */
    public static final int DD_DischargeDiagnosisCodeSequence = 343;

    /** DOCUMENT ME! */
    public static final int DD_DischargeDiagnosisDescription = 344;

    /** DOCUMENT ME! */
    public static final int DD_OwnerID = 345;

    /** DOCUMENT ME! */
    public static final int DD_WindowCenterWidthExplanation = 346;

    /** DOCUMENT ME! */
    public static final int DD_RescaleType = 347;

    /** DOCUMENT ME! */
    public static final int DD_RescaleSlope = 348;

    /** DOCUMENT ME! */
    public static final int DD_RescaleIntercept = 349;

    /** DOCUMENT ME! */
    public static final int DD_FrameDelay = 350;

    /** DOCUMENT ME! */
    public static final int DD_WindowWidth = 351;

    /** DOCUMENT ME! */
    public static final int DD_FrameTimeVector = 352;

    /** DOCUMENT ME! */
    public static final int DD_WindowCenter = 353;

    /** DOCUMENT ME! */
    public static final int DD_FramingType = 354;

    /** DOCUMENT ME! */
    public static final int DD_FrameTime = 355;

    /** DOCUMENT ME! */
    public static final int DD_NominalInterval = 356;

    /** DOCUMENT ME! */
    public static final int DD_TriggerSourceOrType = 357;

    /** DOCUMENT ME! */
    public static final int DD_TriggerTime = 358;

    /** DOCUMENT ME! */
    public static final int DD_OperatorName = 359;

    /** DOCUMENT ME! */
    public static final int DD_DischargeTime = 360;

    /** DOCUMENT ME! */
    public static final int DD_DischargeDate = 361;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledPatientInstitutionResidence = 362;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledDischargeTime = 363;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledDischargeDate = 364;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledAdmissionTime = 365;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledAdmissionDate = 366;

    /** DOCUMENT ME! */
    public static final int DD_PixelIntensityRelationshipSign = 367;

    /** DOCUMENT ME! */
    public static final int DD_PixelIntensityRelationship = 368;

    /** DOCUMENT ME! */
    public static final int DD_SpatialResolution = 369;

    /** DOCUMENT ME! */
    public static final int DD_PhysicianReadingStudy = 370;

    /** DOCUMENT ME! */
    public static final int DD_AdmittingTime = 371;

    /** DOCUMENT ME! */
    public static final int DD_AdmittingDate = 372;

    /** DOCUMENT ME! */
    public static final int DD_DestinationAE = 373;

    /** DOCUMENT ME! */
    public static final int DD_RecommendedDisplayFrameRate = 374;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusIngredientConcentration = 375;

    /** DOCUMENT ME! */
    public static final int DD_StopTrim = 376;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusIngredient = 377;

    /** DOCUMENT ME! */
    public static final int DD_StartTrim = 378;

    /** DOCUMENT ME! */
    public static final int DD_ContrastFlowDuration = 379;

    /** DOCUMENT ME! */
    public static final int DD_ContrastFlowRate = 380;

    /** DOCUMENT ME! */
    public static final int DD_SyringeCounts = 381;

    /** DOCUMENT ME! */
    public static final int DD_CurveData = 382;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusTotalDose = 383;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusStopTime = 384;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusStartTime = 385;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusVolume = 386;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusRoute = 387;

    /** DOCUMENT ME! */
    public static final int DD_PerformingPhysicianName = 388;

    /** DOCUMENT ME! */
    public static final int DD_RouteOfAdmissions = 389;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfViewsInStage = 390;

    /** DOCUMENT ME! */
    public static final int DD_IssuerOfAdmissionID = 391;

    /** DOCUMENT ME! */
    public static final int DD_AdmissionID = 392;

    /** DOCUMENT ME! */
    public static final int DD_LossyImageCompressionRatio = 393;

    /** DOCUMENT ME! */
    public static final int DD_LossyImageCompression = 394;

    /** DOCUMENT ME! */
    public static final int DD_EventTimerName = 395;

    /** DOCUMENT ME! */
    public static final int DD_EventElapsedTime = 396;

    /** DOCUMENT ME! */
    public static final int DD_PhysicianOfRecord = 397;

    /** DOCUMENT ME! */
    public static final int DD_ProtocolName = 398;

    /** DOCUMENT ME! */
    public static final int DD_VisitStatusID = 399;

    /** DOCUMENT ME! */
    public static final int DD_InstitutionalDepartmentName = 400;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedPatientAliasSequence = 401;

    /** DOCUMENT ME! */
    public static final int DD_DeadTimeCorrectionFlag = 402;

    /** DOCUMENT ME! */
    public static final int DD_CountsIncluded = 403;

    /** DOCUMENT ME! */
    public static final int DD_BeamMeterset = 404;

    /** DOCUMENT ME! */
    public static final int DD_VisitGroupLength = 405;

    /** DOCUMENT ME! */
    public static final int DD_BeamDose = 406;

    /** DOCUMENT ME! */
    public static final int DD_SeriesDescription = 407;

    /** DOCUMENT ME! */
    public static final int DD_BeamDoseSpecificationPoint = 408;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfEventTimers = 409;

    /** DOCUMENT ME! */
    public static final int DD_ViewNumber = 410;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfBeams = 411;

    /** DOCUMENT ME! */
    public static final int DD_ViewName = 412;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfStages = 413;

    /** DOCUMENT ME! */
    public static final int DD_StageNumber = 414;

    /** DOCUMENT ME! */
    public static final int DD_StageName = 415;

    /** DOCUMENT ME! */
    public static final int DD_DigitalImageFormatAcquired = 416;

    /** DOCUMENT ME! */
    public static final int DD_VideoImageFormatAcquired = 417;

    /** DOCUMENT ME! */
    public static final int DD_FractionPattern = 418;

    /** DOCUMENT ME! */
    public static final int DD_SoftwareVersion = 419;

    /** DOCUMENT ME! */
    public static final int DD_RepeatFractionCycleLength = 420;

    /** DOCUMENT ME! */
    public static final int DD_ProcedureCodeSequence = 421;

    /** DOCUMENT ME! */
    public static final int DD_StudyDescription = 422;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFractionPatternDigitsPerDay = 423;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFractionsPlanned = 424;

    /** DOCUMENT ME! */
    public static final int DD_EnergyWindowNumber = 425;

    /** DOCUMENT ME! */
    public static final int DD_HardcopyDeviceManufacturerModelName = 426;

    /** DOCUMENT ME! */
    public static final int DD_HardcopyDeviceSoftwareVersion = 427;

    /** DOCUMENT ME! */
    public static final int DD_CalibrationDataSequence = 428;

    /** DOCUMENT ME! */
    public static final int DD_FractionGroupNumber = 429;

    /** DOCUMENT ME! */
    public static final int DD_RadiopharmaceuticalCodeSequence = 430;

    /** DOCUMENT ME! */
    public static final int DD_FractionGroupSequence = 431;

    /** DOCUMENT ME! */
    public static final int DD_ImageLaterality = 432;

    /** DOCUMENT ME! */
    public static final int DD_AdministrationRouteCodeSequence = 433;

    /** DOCUMENT ME! */
    public static final int DD_Laterality = 434;

    /** DOCUMENT ME! */
    public static final int DD_RadionuclideCodeSequence = 435;

    /** DOCUMENT ME! */
    public static final int DD_SecondaryCaptureDeviceSoftwareVersion = 436;

    /** DOCUMENT ME! */
    public static final int DD_SourceImageSequence = 437;

    /** DOCUMENT ME! */
    public static final int DD_SecondaryCaptureDeviceManufacturerModelName = 438;

    /** DOCUMENT ME! */
    public static final int DD_DerivationDescription = 439;

    /** DOCUMENT ME! */
    public static final int DD_HardcopyDeviceManufacturer = 440;

    /** DOCUMENT ME! */
    public static final int DD_SecondaryCaptureDeviceManufacturer = 441;

    /** DOCUMENT ME! */
    public static final int DD_TimeOfSecondaryCapture = 442;

    /** DOCUMENT ME! */
    public static final int DD_DateOfSecondaryCapture = 443;

    /** DOCUMENT ME! */
    public static final int DD_HardcopyCreationDeviceID = 444;

    /** DOCUMENT ME! */
    public static final int DD_SecondaryCaptureDeviceID = 445;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedControlPointIndex = 446;

    /** DOCUMENT ME! */
    public static final int DD_FrameOfReferenceUID = 447;

    /** DOCUMENT ME! */
    public static final int DD_PartialViewDescription = 448;

    /** DOCUMENT ME! */
    public static final int DD_PlateID = 449;

    /** DOCUMENT ME! */
    public static final int DD_PartialView = 450;

    /** DOCUMENT ME! */
    public static final int DD_DeviceSerialNumber = 451;

    /** DOCUMENT ME! */
    public static final int DD_StationName = 452;

    /** DOCUMENT ME! */
    public static final int DD_RTPlanRelationship = 453;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedBlockNumber = 454;

    /** DOCUMENT ME! */
    public static final int DD_TableTopLateralPositionTolerance = 455;

    /** DOCUMENT ME! */
    public static final int DD_TableTopLongitudinalPositionTolerance = 456;

    /** DOCUMENT ME! */
    public static final int DD_TableTopVerticalPositionTolerance = 457;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepSequence = 458;

    /** DOCUMENT ME! */
    public static final int DD_PatientInsurancePlanCodeSequence = 459;

    /** DOCUMENT ME! */
    public static final int DD_TableTopEccentricAngleTolerance = 460;

    /** DOCUMENT ME! */
    public static final int DD_PatientSupportAngleTolerance = 461;

    /** DOCUMENT ME! */
    public static final int DD_BeamLimitingDevicePositionTolerance = 462;

    /** DOCUMENT ME! */
    public static final int DD_BeamLimitingDeviceToleranceSequence = 463;

    /** DOCUMENT ME! */
    public static final int DD_BeamLimitingDeviceAngleTolerance = 464;

    /** DOCUMENT ME! */
    public static final int DD_ImageOrientationPatient = 465;

    /** DOCUMENT ME! */
    public static final int DD_GantryAngleTolerance = 466;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedCompensatorNumber = 467;

    /** DOCUMENT ME! */
    public static final int DD_ToleranceTableLabel = 468;

    /** DOCUMENT ME! */
    public static final int DD_ToleranceTableNumber = 469;

    /** DOCUMENT ME! */
    public static final int DD_ToleranceTableSequence = 470;

    /** DOCUMENT ME! */
    public static final int DD_ImagePositionPatient = 471;

    /** DOCUMENT ME! */
    public static final int DD_PatientSex = 472;

    /** DOCUMENT ME! */
    public static final int DD_AxisLabels = 473;

    /** DOCUMENT ME! */
    public static final int DD_PurposeOfReferenceCodeSequence = 474;

    /** DOCUMENT ME! */
    public static final int DD_LUTNumber = 475;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedWedgeNumber = 476;

    /** DOCUMENT ME! */
    public static final int DD_CurveNumber = 477;

    /** DOCUMENT ME! */
    public static final int DD_OverlayNumber = 478;

    /** DOCUMENT ME! */
    public static final int DD_PatientOrientation = 479;

    /** DOCUMENT ME! */
    public static final int DD_PatientBirthTime = 480;

    /** DOCUMENT ME! */
    public static final int DD_PatientBirthDate = 481;

    /** DOCUMENT ME! */
    public static final int DD_OrganAtRiskOverdoseVolumeFraction = 482;

    /** DOCUMENT ME! */
    public static final int DD_OrganAtRiskMaximumDose = 483;

    /** DOCUMENT ME! */
    public static final int DD_AxisUnits = 484;

    /** DOCUMENT ME! */
    public static final int DD_OrganAtRiskLimitDose = 485;

    /** DOCUMENT ME! */
    public static final int DD_OrganAtRiskFullVolumeDose = 486;

    /** DOCUMENT ME! */
    public static final int DD_ConceptCodeSequence = 487;

    /** DOCUMENT ME! */
    public static final int DD_TargetUnderdoseVolumeFraction = 488;

    /** DOCUMENT ME! */
    public static final int DD_TextValue = 489;

    /** DOCUMENT ME! */
    public static final int DD_TargetMaximumDose = 490;

    /** DOCUMENT ME! */
    public static final int DD_ItemNumber = 491;

    /** DOCUMENT ME! */
    public static final int DD_TargetPrescriptionDose = 492;

    /** DOCUMENT ME! */
    public static final int DD_TargetMinimumDose = 493;

    /** DOCUMENT ME! */
    public static final int DD_CurrentPatientLocation = 494;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedBolusSequence = 495;

    /** DOCUMENT ME! */
    public static final int DD_DeliveryMaximumDose = 496;

    /** DOCUMENT ME! */
    public static final int DD_DeliveryWarningDose = 497;

    /** DOCUMENT ME! */
    public static final int DD_ConstraintWeight = 498;

    /** DOCUMENT ME! */
    public static final int DD_InstanceNumber = 499;

    /** DOCUMENT ME! */
    public static final int DD_DoseReferenceType = 500;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionNumber = 501;

    /** DOCUMENT ME! */
    public static final int DD_SeriesNumber = 502;

    /** DOCUMENT ME! */
    public static final int DD_StudyID = 503;

    /** DOCUMENT ME! */
    public static final int DD_IssuerOfPatientID = 504;

    /** DOCUMENT ME! */
    public static final int DD_PatientID = 505;

    /** DOCUMENT ME! */
    public static final int DD_CurveDescription = 506;

    /** DOCUMENT ME! */
    public static final int DD_SeriesInstanceUID = 507;

    /** DOCUMENT ME! */
    public static final int DD_TypeOfData = 508;

    /** DOCUMENT ME! */
    public static final int DD_RequestedResolutionID = 509;

    /** DOCUMENT ME! */
    public static final int DD_StudyInstanceUID = 510;

    /** DOCUMENT ME! */
    public static final int DD_NominalPriorDose = 511;

    /** DOCUMENT ME! */
    public static final int DD_DoseReferencePointCoordinates = 512;

    /** DOCUMENT ME! */
    public static final int DD_DoseReferenceDescription = 513;

    /** DOCUMENT ME! */
    public static final int DD_NominalBeamEnergyUnit = 514;

    /** DOCUMENT ME! */
    public static final int DD_DirectoryRecordSequence = 515;

    /** DOCUMENT ME! */
    public static final int DD_DoseReferenceStructureType = 516;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedToleranceTableNumber = 517;

    /** DOCUMENT ME! */
    public static final int DD_DoseReferenceNumber = 518;

    /** DOCUMENT ME! */
    public static final int DD_DoseReferenceSequence = 519;

    /** DOCUMENT ME! */
    public static final int DD_RelationshipGroupLength = 520;

    /** DOCUMENT ME! */
    public static final int DD_PatientName = 521;

    /** DOCUMENT ME! */
    public static final int DD_dBdt = 522;

    /** DOCUMENT ME! */
    public static final int DD_SAR = 523;

    /** DOCUMENT ME! */
    public static final int DD_PrescriptionDescription = 524;

    /** DOCUMENT ME! */
    public static final int DD_ImplantPresent = 525;

    /** DOCUMENT ME! */
    public static final int DD_VariableFlipAngleFlag = 526;

    /** DOCUMENT ME! */
    public static final int DD_FlipAngle = 527;

    /** DOCUMENT ME! */
    public static final int DD_RTPlanGeometry = 528;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfPoints = 529;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentSites = 530;

    /** DOCUMENT ME! */
    public static final int DD_RequestedDecimateCropBehavior = 531;

    /** DOCUMENT ME! */
    public static final int DD_InPlanePhaseEncodingDirection = 532;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentIntent = 533;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionMatrix = 534;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentProtocols = 535;

    /** DOCUMENT ME! */
    public static final int DD_RTPlanTime = 536;

    /** DOCUMENT ME! */
    public static final int DD_FileSetConsistencyFlag = 537;

    /** DOCUMENT ME! */
    public static final int DD_RTPlanDate = 538;

    /** DOCUMENT ME! */
    public static final int DD_RTPlanDescription = 539;

    /** DOCUMENT ME! */
    public static final int DD_RTPlanName = 540;

    /** DOCUMENT ME! */
    public static final int DD_RTPlanLabel = 541;

    /** DOCUMENT ME! */
    public static final int DD_PatientGroupLength = 542;

    /** DOCUMENT ME! */
    public static final int DD_CurveDimensions = 543;

    /** DOCUMENT ME! */
    public static final int DD_TextString = 544;

    /** DOCUMENT ME! */
    public static final int DD_CurveGroupLength = 545;

    /** DOCUMENT ME! */
    public static final int DD_RequestedImageSize = 546;

    /** DOCUMENT ME! */
    public static final int DD_ScanLength = 547;

    /** DOCUMENT ME! */
    public static final int DD_WholeBodyTechnique = 548;

    /** DOCUMENT ME! */
    public static final int DD_ScanVelocity = 549;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFrameNumbers = 550;

    /** DOCUMENT ME! */
    public static final int DD_RootDirectoryLastRecord = 551;

    /** DOCUMENT ME! */
    public static final int DD_RootDirectoryFirstRecord = 552;

    /** DOCUMENT ME! */
    public static final int DD_CommentsOnScheduledProcedureStep = 553;

    /** DOCUMENT ME! */
    public static final int DD_AnnotationPosition = 554;

    /** DOCUMENT ME! */
    public static final int DD_RequestedSOPClassUID = 555;

    /** DOCUMENT ME! */
    public static final int DD_AffectedSOPClassUID = 556;

    /** DOCUMENT ME! */
    public static final int DD_GroupLength = 557;

    /** DOCUMENT ME! */
    public static final int DD_Polarity = 558;

    /** DOCUMENT ME! */
    public static final int DD_UID = 559;

    /** DOCUMENT ME! */
    public static final int DD_PersonName = 560;

    /** DOCUMENT ME! */
    public static final int DD_Time = 561;

    /** DOCUMENT ME! */
    public static final int DD_Date = 562;

    /** DOCUMENT ME! */
    public static final int DD_AnnotationGroupLength = 563;

    /** DOCUMENT ME! */
    public static final int DD_MaskOperationExplanation = 564;

    /** DOCUMENT ME! */
    public static final int DD_ImageBoxPosition = 565;

    /** DOCUMENT ME! */
    public static final int DD_TimezoneOffsetFromUTC = 566;

    /** DOCUMENT ME! */
    public static final int DD_ImageBoxGroupLength = 567;

    /** DOCUMENT ME! */
    public static final int DD_CurveLabel = 568;

    /** DOCUMENT ME! */
    public static final int DD_VerticesOfPolygonalShutter = 569;

    /** DOCUMENT ME! */
    public static final int DD_OverlayBlue = 570;

    /** DOCUMENT ME! */
    public static final int DD_OverlayGreen = 571;

    /** DOCUMENT ME! */
    public static final int DD_OverlayRed = 572;

    /** DOCUMENT ME! */
    public static final int DD_OverlayGray = 573;

    /** DOCUMENT ME! */
    public static final int DD_RadiusOfCircularShutter = 574;

    /** DOCUMENT ME! */
    public static final int DD_CenterOfCircularShutter = 575;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedTransferSyntaxUIDInFile = 576;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedSOPInstanceUIDInFile = 577;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedSOPClassUIDInFile = 578;

    /** DOCUMENT ME! */
    public static final int DD_ShutterLowerHorizontalEdge = 579;

    /** DOCUMENT ME! */
    public static final int DD_ShutterUpperHorizontalEdge = 580;

    /** DOCUMENT ME! */
    public static final int DD_OverlayBitPosition = 581;

    /** DOCUMENT ME! */
    public static final int DD_ShutterRightVerticalEdge = 582;

    /** DOCUMENT ME! */
    public static final int DD_OverlayBitsAllocated = 583;

    /** DOCUMENT ME! */
    public static final int DD_ShutterLeftVerticalEdge = 584;

    /** DOCUMENT ME! */
    public static final int DD_PatientReligiousPreference = 585;

    /** DOCUMENT ME! */
    public static final int DD_ShutterShape = 586;

    /** DOCUMENT ME! */
    public static final int DD_MRDRDirectoryRecordOffset = 587;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFileID = 588;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedDoseSequence = 589;

    /** DOCUMENT ME! */
    public static final int DD_LastMenstrualDate = 590;

    /** DOCUMENT ME! */
    public static final int DD_DepthOfScanField = 591;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedPatientSetupNumber = 592;

    /** DOCUMENT ME! */
    public static final int DD_TIDOffset = 593;

    /** DOCUMENT ME! */
    public static final int DD_PregnancyStatus = 594;

    /** DOCUMENT ME! */
    public static final int DD_StorageMediaFileSetUID = 595;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedStructureSetSequence = 596;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedInterpretationSequence = 597;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentTime = 598;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentDate = 599;

    /** DOCUMENT ME! */
    public static final int DD_MaskSubPixelShift = 600;

    /** DOCUMENT ME! */
    public static final int DD_ContrastFrameAveraging = 601;

    /** DOCUMENT ME! */
    public static final int DD_MaskFrameNumbers = 602;

    /** DOCUMENT ME! */
    public static final int DD_AdditionalPatientHistory = 603;

    /** DOCUMENT ME! */
    public static final int DD_StorageMediaFileSetID = 604;

    /** DOCUMENT ME! */
    public static final int DD_BrachyReferencedDoseReferenceSequence = 605;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedDoseReferenceNumber = 606;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedDoseReferenceSequence = 607;

    /** DOCUMENT ME! */
    public static final int DD_ResultsIDIssuer = 608;

    /** DOCUMENT ME! */
    public static final int DD_ResultsID = 609;

    /** DOCUMENT ME! */
    public static final int DD_FractionStatusSummarySequence = 610;

    /** DOCUMENT ME! */
    public static final int DD_OverlayLabel = 611;

    /** DOCUMENT ME! */
    public static final int DD_VisitComments = 612;

    /** DOCUMENT ME! */
    public static final int DD_ApplicableFrameRange = 613;

    /** DOCUMENT ME! */
    public static final int DD_MaskOperation = 614;

    /** DOCUMENT ME! */
    public static final int DD_MaskSubtractionSequence = 615;

    /** DOCUMENT ME! */
    public static final int DD_SmokingStatus = 616;

    /** DOCUMENT ME! */
    public static final int DD_SoftTissueSurfaceThermalIndex = 617;

    /** DOCUMENT ME! */
    public static final int DD_SoftTissueFocusThermalIndex = 618;

    /** DOCUMENT ME! */
    public static final int DD_SoftTissueThermalIndex = 619;

    /** DOCUMENT ME! */
    public static final int DD_CranialThermalIndex = 620;

    /** DOCUMENT ME! */
    public static final int DD_BoneThermalIndex = 621;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedReferenceImageSequence = 622;

    /** DOCUMENT ME! */
    public static final int DD_MechanicalIndex = 623;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedVerificationImageSequence = 624;

    /** DOCUMENT ME! */
    public static final int DD_PostprocessingFunction = 625;

    /** DOCUMENT ME! */
    public static final int DD_ProcessingFunction = 626;

    /** DOCUMENT ME! */
    public static final int DD_BeamStopperPosition = 627;

    /** DOCUMENT ME! */
    public static final int DD_BiplaneAcquisitionSequence = 628;

    /** DOCUMENT ME! */
    public static final int DD_ContentSequence = 629;

    /** DOCUMENT ME! */
    public static final int DD_FocusDepth = 630;

    /** DOCUMENT ME! */
    public static final int DD_FractionGroupType = 631;

    /** DOCUMENT ME! */
    public static final int DD_TransducerData = 632;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFractionNumber = 633;

    /** DOCUMENT ME! */
    public static final int DD_FractionGroupSummarySequence = 634;

    /** DOCUMENT ME! */
    public static final int DD_MoveDestination = 635;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFractionGroupNumber = 636;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFractionGroupSequence = 637;

    /** DOCUMENT ME! */
    public static final int DD_OutputPower = 638;

    /** DOCUMENT ME! */
    public static final int DD_PrintQueueID = 639;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentStatusComment = 640;

    /** DOCUMENT ME! */
    public static final int DD_ResultsGroupLength = 641;

    /** DOCUMENT ME! */
    public static final int DD_CurrentTreatmentStatus = 642;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedSourceNumber = 643;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedBrachyApplicationSetupNumber = 644;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedBrachyApplicationSetupSequence = 645;

    /** DOCUMENT ME! */
    public static final int DD_EndCumulativeMetersetWeight = 646;

    /** DOCUMENT ME! */
    public static final int DD_StartCumulativeMetersetWeight = 647;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedReferenceImageNumber = 648;

    /** DOCUMENT ME! */
    public static final int DD_SlideIdentifier = 649;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedBeamNumber = 650;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedBeamSequence = 651;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedRTPlanSequence = 652;

    /** DOCUMENT ME! */
    public static final int DD_ConfidentialityConstraintOnPatientDataDescription = 653;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicationEntityTitle = 654;

    /** DOCUMENT ME! */
    public static final int DD_ImplementationVersionName = 655;

    /** DOCUMENT ME! */
    public static final int DD_ImplementationClassUID = 656;

    /** DOCUMENT ME! */
    public static final int DD_TransferSyntaxUID = 657;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedBasicAnnotationBoxSequence = 658;

    /** DOCUMENT ME! */
    public static final int DD_ReprojectionMethod = 659;

    /** DOCUMENT ME! */
    public static final int DD_CountsSource = 660;

    /** DOCUMENT ME! */
    public static final int DD_Units = 661;

    /** DOCUMENT ME! */
    public static final int DD_SeriesType = 662;

    /** DOCUMENT ME! */
    public static final int DD_CumulativeTimeWeight = 663;

    /** DOCUMENT ME! */
    public static final int DD_ControlPoint3DPosition = 664;

    /** DOCUMENT ME! */
    public static final int DD_ControlPointRelativePosition = 665;

    /** DOCUMENT ME! */
    public static final int DD_BrachyControlPointSequence = 666;

    /** DOCUMENT ME! */
    public static final int DD_MediaStorageSOPInstanceUID = 667;

    /** DOCUMENT ME! */
    public static final int DD_MediaStorageSOPClassUID = 668;

    /** DOCUMENT ME! */
    public static final int DD_FileMetaInformationVersion = 669;

    /** DOCUMENT ME! */
    public static final int DD_FileMetaInformationGroupLength = 670;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedImageBoxSequence = 671;

    /** DOCUMENT ME! */
    public static final int DD_FinalCumulativeTimeWeight = 672;

    /** DOCUMENT ME! */
    public static final int DD_Originator = 673;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFilmSessionSequence = 674;

    /** DOCUMENT ME! */
    public static final int DD_ChannelShieldNominalTransmission = 675;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedStoredPrintSequence = 676;

    /** DOCUMENT ME! */
    public static final int DD_ImageComments = 677;

    /** DOCUMENT ME! */
    public static final int DD_ChannelShieldNominalThickness = 678;

    /** DOCUMENT ME! */
    public static final int DD_ChannelShieldName = 679;

    /** DOCUMENT ME! */
    public static final int DD_ChannelShieldID = 680;

    /** DOCUMENT ME! */
    public static final int DD_ChannelShieldNumber = 681;

    /** DOCUMENT ME! */
    public static final int DD_ChannelShieldSequence = 682;

    /** DOCUMENT ME! */
    public static final int DD_ErrorID = 683;

    /** DOCUMENT ME! */
    public static final int DD_ErrorComment = 684;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFilmBoxSequence = 685;

    /** DOCUMENT ME! */
    public static final int DD_OffendingElement = 686;

    /** DOCUMENT ME! */
    public static final int DD_Status = 687;

    /** DOCUMENT ME! */
    public static final int DD_ImageIndex = 688;

    /** DOCUMENT ME! */
    public static final int DD_PatientComments = 689;

    /** DOCUMENT ME! */
    public static final int DD_CreationTime = 690;

    /** DOCUMENT ME! */
    public static final int DD_TransferTubeLength = 691;

    /** DOCUMENT ME! */
    public static final int DD_TransferTubeNumber = 692;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorStepSize = 693;

    /** DOCUMENT ME! */
    public static final int DD_Occupation = 694;

    /** DOCUMENT ME! */
    public static final int DD_DeadTimeFactor = 695;

    /** DOCUMENT ME! */
    public static final int DD_ScatterFractionFactor = 696;

    /** DOCUMENT ME! */
    public static final int DD_PrinterName = 697;

    /** DOCUMENT ME! */
    public static final int DD_DoseCalibrationFactor = 698;

    /** DOCUMENT ME! */
    public static final int DD_DecayFactor = 699;

    /** DOCUMENT ME! */
    public static final int DD_SliceSensitivityFactor = 700;

    /** DOCUMENT ME! */
    public static final int DD_CreationDate = 701;

    /** DOCUMENT ME! */
    public static final int DD_MedicalRecordLocator = 702;

    /** DOCUMENT ME! */
    public static final int DD_Impressions = 703;

    /** DOCUMENT ME! */
    public static final int DD_RotationOfScannedFilm = 704;

    /** DOCUMENT ME! */
    public static final int DD_PrinterStatusInfo = 705;

    /** DOCUMENT ME! */
    public static final int DD_SecondaryCountsAccumulated = 706;

    /** DOCUMENT ME! */
    public static final int DD_OtherStudyNumbers = 707;

    /** DOCUMENT ME! */
    public static final int DD_PrimaryPromptsCountsAccumulated = 708;

    /** DOCUMENT ME! */
    public static final int DD_ExecutionStatusInfo = 709;

    /** DOCUMENT ME! */
    public static final int DD_BranchOfService = 710;

    /** DOCUMENT ME! */
    public static final int DD_MilitaryRank = 711;

    /** DOCUMENT ME! */
    public static final int DD_ViewModifierCodeSequence = 712;

    /** DOCUMENT ME! */
    public static final int DD_DigitizingDeviceTransportDirection = 713;

    /** DOCUMENT ME! */
    public static final int DD_ViewCodeSequence = 714;

    /** DOCUMENT ME! */
    public static final int DD_OverlayBackgroundDensity = 715;

    /** DOCUMENT ME! */
    public static final int DD_OverlayForegroundDensity = 716;

    /** DOCUMENT ME! */
    public static final int DD_EthnicGroup = 717;

    /** DOCUMENT ME! */
    public static final int DD_LesionNumber = 718;

    /** DOCUMENT ME! */
    public static final int DD_IVUSPullbackStopFrameNumber = 719;

    /** DOCUMENT ME! */
    public static final int DD_IVUSPullbackStartFrameNumber = 720;

    /** DOCUMENT ME! */
    public static final int DD_PrinterStatus = 721;

    /** DOCUMENT ME! */
    public static final int DD_IVUSGatedRate = 722;

    /** DOCUMENT ME! */
    public static final int DD_IVUSPullbackRate = 723;

    /** DOCUMENT ME! */
    public static final int DD_IVUSAcquisition = 724;

    /** DOCUMENT ME! */
    public static final int DD_FrameReferenceTime = 725;

    /** DOCUMENT ME! */
    public static final int DD_ExecutionStatus = 726;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfTriggersInPhase = 727;

    /** DOCUMENT ME! */
    public static final int DD_NominalScannedPixelSpacing = 728;

    /** DOCUMENT ME! */
    public static final int DD_TriggerVector = 729;

    /** DOCUMENT ME! */
    public static final int DD_MagnifyToNumberOfColumns = 730;

    /** DOCUMENT ME! */
    public static final int DD_OverlayOrImageMagnification = 731;

    /** DOCUMENT ME! */
    public static final int DD_PatientTelephoneNumber = 732;

    /** DOCUMENT ME! */
    public static final int DD_OverlaySmoothingType = 733;

    /** DOCUMENT ME! */
    public static final int DD_RegionOfResidence = 734;

    /** DOCUMENT ME! */
    public static final int DD_CountryOfResidence = 735;

    /** DOCUMENT ME! */
    public static final int DD_PrinterGroupLength = 736;

    /** DOCUMENT ME! */
    public static final int DD_PrintJobID = 737;

    /** DOCUMENT ME! */
    public static final int DD_DisplayWindowLabelVector = 738;

    /** DOCUMENT ME! */
    public static final int DD_PatientMotherBirthName = 739;

    /** DOCUMENT ME! */
    public static final int DD_SliceLocationVector = 740;

    /** DOCUMENT ME! */
    public static final int DD_FrameSecondaryAngleVector = 741;

    /** DOCUMENT ME! */
    public static final int DD_FramePrimaryAngleVector = 742;

    /** DOCUMENT ME! */
    public static final int DD_FrameLabelVector = 743;

    /** DOCUMENT ME! */
    public static final int DD_TypeOfDetectorMotion = 744;

    /** DOCUMENT ME! */
    public static final int DD_PageNumberVector = 745;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepStatus = 746;

    /** DOCUMENT ME! */
    public static final int DD_StartAngle = 747;

    /** DOCUMENT ME! */
    public static final int DD_OverlayMagnificationType = 748;

    /** DOCUMENT ME! */
    public static final int DD_SliceLocation = 749;

    /** DOCUMENT ME! */
    public static final int DD_PositionReferenceIndicator = 750;

    /** DOCUMENT ME! */
    public static final int DD_PrintJobGroupLength = 751;

    /** DOCUMENT ME! */
    public static final int DD_PreMedication = 752;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepLocation = 753;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStationName = 754;

    /** DOCUMENT ME! */
    public static final int DD_PhosphorType = 755;

    /** DOCUMENT ME! */
    public static final int DD_PlateType = 756;

    /** DOCUMENT ME! */
    public static final int DD_StageCodeSequence = 757;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepID = 758;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProtocolCodeSequence = 759;

    /** DOCUMENT ME! */
    public static final int DD_PatientAddress = 760;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepDescription = 761;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledPerformingPhysicianName = 762;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepEndTime = 763;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepEndDate = 764;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepStartTime = 765;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepStartDate = 766;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStationAETitle = 767;

    /** DOCUMENT ME! */
    public static final int DD_DiaphragmPosition = 768;

    /** DOCUMENT ME! */
    public static final int DD_MetersetExposure = 769;

    /** DOCUMENT ME! */
    public static final int DD_ExposureSequence = 770;

    /** DOCUMENT ME! */
    public static final int DD_TransmitCoilName = 771;

    /** DOCUMENT ME! */
    public static final int DD_ReceiveCoilName = 772;

    /** DOCUMENT ME! */
    public static final int DD_PatientWeight = 773;

    /** DOCUMENT ME! */
    public static final int DD_FractionNumber = 774;

    /** DOCUMENT ME! */
    public static final int DD_SourceToReferenceObjectDistance = 775;

    /** DOCUMENT ME! */
    public static final int DD_RTImageSID = 776;

    /** DOCUMENT ME! */
    public static final int DD_RadiationMachineSSD = 777;

    /** DOCUMENT ME! */
    public static final int DD_RadiationMachineSAD = 778;

    /** DOCUMENT ME! */
    public static final int DD_PresentationLUTShape = 779;

    /** DOCUMENT ME! */
    public static final int DD_OverlayData = 780;

    /** DOCUMENT ME! */
    public static final int DD_RadiationMachineName = 781;

    /** DOCUMENT ME! */
    public static final int DD_PreferredPlaybackSequencing = 782;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorWallNominalTransmission = 783;

    /** DOCUMENT ME! */
    public static final int DD_CountRate = 784;

    /** DOCUMENT ME! */
    public static final int DD_ActualFrameDuration = 785;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorWallNominalThickness = 786;

    /** DOCUMENT ME! */
    public static final int DD_ContrastAllergies = 787;

    /** DOCUMENT ME! */
    public static final int DD_FileSetCharacterSet = 788;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorManufacturer = 789;

    /** DOCUMENT ME! */
    public static final int DD_FileSetDescriptorFileID = 790;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorLength = 791;

    /** DOCUMENT ME! */
    public static final int DD_PatientSize = 792;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorName = 793;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorType = 794;

    /** DOCUMENT ME! */
    public static final int DD_MoveOriginatorMessageID = 795;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorID = 796;

    /** DOCUMENT ME! */
    public static final int DD_MoveOriginatorApplicationEntityTitle = 797;

    /** DOCUMENT ME! */
    public static final int DD_SourceApplicatorNumber = 798;

    /** DOCUMENT ME! */
    public static final int DD_PerformedSeriesSequence = 799;

    /** DOCUMENT ME! */
    public static final int DD_SegmentedBluePaletteColorLookupTableData = 800;

    /** DOCUMENT ME! */
    public static final int DD_SegmentedGreenPaletteColorLookupTableData = 801;

    /** DOCUMENT ME! */
    public static final int DD_RTImagePosition = 802;

    /** DOCUMENT ME! */
    public static final int DD_SegmentedRedPaletteColorLookupTableData = 803;

    /** DOCUMENT ME! */
    public static final int DD_PresentationLUTSequence = 804;

    /** DOCUMENT ME! */
    public static final int DD_ImagePlanePixelSpacing = 805;

    /** DOCUMENT ME! */
    public static final int DD_RTImageOrientation = 806;

    /** DOCUMENT ME! */
    public static final int DD_PulseRepetitionInterval = 807;

    /** DOCUMENT ME! */
    public static final int DD_OverlayPixelDataSequence = 808;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfPulses = 809;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionsInStudy = 810;

    /** DOCUMENT ME! */
    public static final int DD_XRayImageReceptorAngle = 811;

    /** DOCUMENT ME! */
    public static final int DD_ImagesInAcquisition = 812;

    /** DOCUMENT ME! */
    public static final int DD_RTImagePlane = 813;

    /** DOCUMENT ME! */
    public static final int DD_SeriesInStudy = 814;

    /** DOCUMENT ME! */
    public static final int DD_ReportedValuesOrigin = 815;

    /** DOCUMENT ME! */
    public static final int DD_SourceMovementType = 816;

    /** DOCUMENT ME! */
    public static final int DD_FileSetID = 817;

    /** DOCUMENT ME! */
    public static final int DD_ChannelTotalTime = 818;

    /** DOCUMENT ME! */
    public static final int DD_PatientAge = 819;

    /** DOCUMENT ME! */
    public static final int DD_ChannelLength = 820;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfWarningSuboperations = 821;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFailedSuboperations = 822;

    /** DOCUMENT ME! */
    public static final int DD_ChannelNumber = 823;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfCompletedSuboperations = 824;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfRemainingSuboperations = 825;

    /** DOCUMENT ME! */
    public static final int DD_ChannelSequence = 826;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedProcedureStepSequence = 827;

    /** DOCUMENT ME! */
    public static final int DD_RTImageDescription = 828;

    /** DOCUMENT ME! */
    public static final int DD_RTImageName = 829;

    /** DOCUMENT ME! */
    public static final int DD_RTImageLabel = 830;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedOverlayPlaneGroups = 831;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedOverlayPlaneSequence = 832;

    /** DOCUMENT ME! */
    public static final int DD_PixelPaddingValue = 833;

    /** DOCUMENT ME! */
    public static final int DD_PatientBirthName = 834;

    /** DOCUMENT ME! */
    public static final int DD_OtherPatientName = 835;

    /** DOCUMENT ME! */
    public static final int DD_OtherPatientID = 836;

    /** DOCUMENT ME! */
    public static final int DD_BillingSuppliesAndDevicesSequence = 837;

    /** DOCUMENT ME! */
    public static final int DD_FilmConsumptionSequence = 838;

    /** DOCUMENT ME! */
    public static final int DD_BillingProcedureStepSequence = 839;

    /** DOCUMENT ME! */
    public static final int DD_BluePaletteColorLookupTableData = 840;

    /** DOCUMENT ME! */
    public static final int DD_GreenPaletteColorLookupTableData = 841;

    /** DOCUMENT ME! */
    public static final int DD_RedPaletteColorLookupTableData = 842;

    /** DOCUMENT ME! */
    public static final int DD_BrachyAccessoryDeviceNominalTransmission = 843;

    /** DOCUMENT ME! */
    public static final int DD_OverlayBoxGroupLength = 844;

    /** DOCUMENT ME! */
    public static final int DD_ConvolutionKernel = 845;

    /** DOCUMENT ME! */
    public static final int DD_BrachyAccessoryDeviceNominalThickness = 846;

    /** DOCUMENT ME! */
    public static final int DD_LargestPixelValueInPlane = 847;

    /** DOCUMENT ME! */
    public static final int DD_ConceptNameCodeSequence = 848;

    /** DOCUMENT ME! */
    public static final int DD_SmallestPixelValueInPlane = 849;

    /** DOCUMENT ME! */
    public static final int DD_ActionTypeID = 850;

    /** DOCUMENT ME! */
    public static final int DD_RequestedProcedureComments = 851;

    /** DOCUMENT ME! */
    public static final int DD_BrachyAccessoryDeviceName = 852;

    /** DOCUMENT ME! */
    public static final int DD_AttributeIdentifierList = 853;

    /** DOCUMENT ME! */
    public static final int DD_OrganExposed = 854;

    /** DOCUMENT ME! */
    public static final int DD_BrachyAccessoryDeviceType = 855;

    /** DOCUMENT ME! */
    public static final int DD_BrachyAccessoryDeviceID = 856;

    /** DOCUMENT ME! */
    public static final int DD_EventTypeID = 857;

    /** DOCUMENT ME! */
    public static final int DD_OrganDose = 858;

    /** DOCUMENT ME! */
    public static final int DD_BrachyAccessoryDeviceNumber = 859;

    /** DOCUMENT ME! */
    public static final int DD_RequestedSOPInstanceUID = 860;

    /** DOCUMENT ME! */
    public static final int DD_AffectedSOPInstanceUID = 861;

    /** DOCUMENT ME! */
    public static final int DD_HalfValueLayer = 862;

    /** DOCUMENT ME! */
    public static final int DD_BrachyAccessoryDeviceSequence = 863;

    /** DOCUMENT ME! */
    public static final int DD_XRayOutput = 864;

    /** DOCUMENT ME! */
    public static final int DD_CommentsOnRadiationDose = 865;

    /** DOCUMENT ME! */
    public static final int DD_LargestPixelValueInSeries = 866;

    /** DOCUMENT ME! */
    public static final int DD_SmallestPixelValueInSeries = 867;

    /** DOCUMENT ME! */
    public static final int DD_TimeOfLastCalibration = 868;

    /** DOCUMENT ME! */
    public static final int DD_LargestImagePixelValue = 869;

    /** DOCUMENT ME! */
    public static final int DD_DateOfLastCalibration = 870;

    /** DOCUMENT ME! */
    public static final int DD_SmallestImagePixelValue = 871;

    /** DOCUMENT ME! */
    public static final int DD_ExposureDoseSequence = 872;

    /** DOCUMENT ME! */
    public static final int DD_ReviewerName = 873;

    /** DOCUMENT ME! */
    public static final int DD_PixelRepresentation = 874;

    /** DOCUMENT ME! */
    public static final int DD_HighBit = 875;

    /** DOCUMENT ME! */
    public static final int DD_ReviewTime = 876;

    /** DOCUMENT ME! */
    public static final int DD_BitsStored = 877;

    /** DOCUMENT ME! */
    public static final int DD_ReviewDate = 878;

    /** DOCUMENT ME! */
    public static final int DD_BitsAllocated = 879;

    /** DOCUMENT ME! */
    public static final int DD_OriginalImageSequence = 880;

    /** DOCUMENT ME! */
    public static final int DD_ApprovalStatus = 881;

    /** DOCUMENT ME! */
    public static final int DD_DistanceSourceToSupport = 882;

    /** DOCUMENT ME! */
    public static final int DD_DistanceSourceToEntrance = 883;

    /** DOCUMENT ME! */
    public static final int DD_TotalReferenceAirKerma = 884;

    /** DOCUMENT ME! */
    public static final int DD_ExposedArea = 885;

    /** DOCUMENT ME! */
    public static final int DD_EntranceDose = 886;

    /** DOCUMENT ME! */
    public static final int DD_TotalNumberOfExposures = 887;

    /** DOCUMENT ME! */
    public static final int DD_TotalTimeOfFlouroscopy = 888;

    /** DOCUMENT ME! */
    public static final int DD_PlaneOrigin = 889;

    /** DOCUMENT ME! */
    public static final int DD_ImageFrameOrigin = 890;

    /** DOCUMENT ME! */
    public static final int DD_OverlayOrigin = 891;

    /** DOCUMENT ME! */
    public static final int DD_TemplateName = 892;

    /** DOCUMENT ME! */
    public static final int DD_TemplateType = 893;

    /** DOCUMENT ME! */
    public static final int DD_TemplateNumber = 894;

    /** DOCUMENT ME! */
    public static final int DD_FileSetGroupLength = 895;

    /** DOCUMENT ME! */
    public static final int DD_OverlaySubtype = 896;

    /** DOCUMENT ME! */
    public static final int DD_OverlayType = 897;

    /** DOCUMENT ME! */
    public static final int DD_Signature = 898;

    /** DOCUMENT ME! */
    public static final int DD_ProposedStudySequence = 899;

    /** DOCUMENT ME! */
    public static final int DD_ApplicationSetupManufacturer = 900;

    /** DOCUMENT ME! */
    public static final int DD_ApplicationSetupName = 901;

    /** DOCUMENT ME! */
    public static final int DD_CodeMeaning = 902;

    /** DOCUMENT ME! */
    public static final int DD_ApplicationSetupNumber = 903;

    /** DOCUMENT ME! */
    public static final int DD_CodingSchemeDesignator = 904;

    /** DOCUMENT ME! */
    public static final int DD_ApplicationSetupType = 905;

    /** DOCUMENT ME! */
    public static final int DD_CodeValue = 906;

    /** DOCUMENT ME! */
    public static final int DD_ApplicationSetupSequence = 907;

    /** DOCUMENT ME! */
    public static final int DD_AirKermaRateReferenceTime = 908;

    /** DOCUMENT ME! */
    public static final int DD_AirKermaRateReferenceDate = 909;

    /** DOCUMENT ME! */
    public static final int DD_CertificateOfSigner = 910;

    /** DOCUMENT ME! */
    public static final int DD_ResultsComments = 911;

    /** DOCUMENT ME! */
    public static final int DD_ReferenceAirKermaRate = 912;

    /** DOCUMENT ME! */
    public static final int DD_DetectorSecondaryAngle = 913;

    /** DOCUMENT ME! */
    public static final int DD_DetectorPrimaryAngle = 914;

    /** DOCUMENT ME! */
    public static final int DD_CertificateType = 915;

    /** DOCUMENT ME! */
    public static final int DD_SourceIsotopeHalfLife = 916;

    /** DOCUMENT ME! */
    public static final int DD_PrivateRecordUID = 917;

    /** DOCUMENT ME! */
    public static final int DD_SourceIsotopeName = 918;

    /** DOCUMENT ME! */
    public static final int DD_DirectoryRecordType = 919;

    /** DOCUMENT ME! */
    public static final int DD_SourceEncapsulationNominalTransmission = 920;

    /** DOCUMENT ME! */
    public static final int DD_PatientState = 921;

    /** DOCUMENT ME! */
    public static final int DD_SourceEncapsulationNominalThickness = 922;

    /** DOCUMENT ME! */
    public static final int DD_OverlayDescription = 923;

    /** DOCUMENT ME! */
    public static final int DD_DigitalSignatureDateTime = 924;

    /** DOCUMENT ME! */
    public static final int DD_ActiveSourceLength = 925;

    /** DOCUMENT ME! */
    public static final int DD_PositionerSecondaryAngleIncrement = 926;

    /** DOCUMENT ME! */
    public static final int DD_PositionerPrimaryAngleIncrement = 927;

    /** DOCUMENT ME! */
    public static final int DD_DigitalSignatureUID = 928;

    /** DOCUMENT ME! */
    public static final int DD_ActiveSourceDiameter = 929;

    /** DOCUMENT ME! */
    public static final int DD_SourceManufacturer = 930;

    /** DOCUMENT ME! */
    public static final int DD_LowerLevelDirectoryOffset = 931;

    /** DOCUMENT ME! */
    public static final int DD_SourceType = 932;

    /** DOCUMENT ME! */
    public static final int DD_OverlayDescriptorBlue = 933;

    /** DOCUMENT ME! */
    public static final int DD_SourceNumber = 934;

    /** DOCUMENT ME! */
    public static final int DD_OverlayDescriptorGreen = 935;

    /** DOCUMENT ME! */
    public static final int DD_OverlayDescriptorRed = 936;

    /** DOCUMENT ME! */
    public static final int DD_SourceSequence = 937;

    /** DOCUMENT ME! */
    public static final int DD_OverlayDescriptorGray = 938;

    /** DOCUMENT ME! */
    public static final int DD_SynchronizationFrameOfReference = 939;

    /** DOCUMENT ME! */
    public static final int DD_SequenceDelimitationItem = 940;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFramesInOverlay = 941;

    /** DOCUMENT ME! */
    public static final int DD_OverlayPlanes = 942;

    /** DOCUMENT ME! */
    public static final int DD_OverlayColumns = 943;

    /** DOCUMENT ME! */
    public static final int DD_OverlayRows = 944;

    /** DOCUMENT ME! */
    public static final int DD_PositionerSecondaryAngle = 945;

    /** DOCUMENT ME! */
    public static final int DD_PositionerPrimaryAngle = 946;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentMachineSequence = 947;

    /** DOCUMENT ME! */
    public static final int DD_RecordInUseFlag = 948;

    /** DOCUMENT ME! */
    public static final int DD_BrachyTreatmentType = 949;

    /** DOCUMENT ME! */
    public static final int DD_TableOfParameterValues = 950;

    /** DOCUMENT ME! */
    public static final int DD_BrachyTreatmentTechnique = 951;

    /** DOCUMENT ME! */
    public static final int DD_PositionerType = 952;

    /** DOCUMENT ME! */
    public static final int DD_ReflectedAmbientLight = 953;

    /** DOCUMENT ME! */
    public static final int DD_OverlayGroupLength = 954;

    /** DOCUMENT ME! */
    public static final int DD_PositionerMotion = 955;

    /** DOCUMENT ME! */
    public static final int DD_Illumination = 956;

    /** DOCUMENT ME! */
    public static final int DD_PixelComponentDataType = 957;

    /** DOCUMENT ME! */
    public static final int DD_NextDirectoryRecordOffset = 958;

    /** DOCUMENT ME! */
    public static final int DD_PixelComponentPhysicalUnits = 959;

    /** DOCUMENT ME! */
    public static final int DD_PixelComponentRangeStop = 960;

    /** DOCUMENT ME! */
    public static final int DD_DVHMeanDose = 961;

    /** DOCUMENT ME! */
    public static final int DD_MaximumCollatedFilms = 962;

    /** DOCUMENT ME! */
    public static final int DD_DVHMaximumDose = 963;

    /** DOCUMENT ME! */
    public static final int DD_ConfigurationInformationDescription = 964;

    /** DOCUMENT ME! */
    public static final int DD_DVHMinimumDose = 965;

    /** DOCUMENT ME! */
    public static final int DD_ConfigurationInformation = 966;

    /** DOCUMENT ME! */
    public static final int DD_TableOfPixelValues = 967;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfTableEntries = 968;

    /** DOCUMENT ME! */
    public static final int DD_RWavePointer = 969;

    /** DOCUMENT ME! */
    public static final int DD_TableOfYBreakPoints = 970;

    /** DOCUMENT ME! */
    public static final int DD_TableOfXBreakPoints = 971;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfTableBreakPoints = 972;

    /** DOCUMENT ME! */
    public static final int DD_TMLinePositionY0 = 973;

    /** DOCUMENT ME! */
    public static final int DD_TMLinePositionX0 = 974;

    /** DOCUMENT ME! */
    public static final int DD_DopplerSampleVolumeYPosition = 975;

    /** DOCUMENT ME! */
    public static final int DD_DVHROIContributionType = 976;

    /** DOCUMENT ME! */
    public static final int DD_DVHReferencedROISequence = 977;

    /** DOCUMENT ME! */
    public static final int DD_Trim = 978;

    /** DOCUMENT ME! */
    public static final int DD_PixelComponentRangeStart = 979;

    /** DOCUMENT ME! */
    public static final int DD_PixelComponentMask = 980;

    /** DOCUMENT ME! */
    public static final int DD_MaskPointer = 981;

    /** DOCUMENT ME! */
    public static final int DD_PixelComponentOrganization = 982;

    /** DOCUMENT ME! */
    public static final int DD_TMLinePositionY1 = 983;

    /** DOCUMENT ME! */
    public static final int DD_TMLinePositionX1 = 984;

    /** DOCUMENT ME! */
    public static final int DD_SafePositionReturnTime = 985;

    /** DOCUMENT ME! */
    public static final int DD_SafePositionReturnDate = 986;

    /** DOCUMENT ME! */
    public static final int DD_PhysicalDeltaY = 987;

    /** DOCUMENT ME! */
    public static final int DD_SafePositionExitTime = 988;

    /** DOCUMENT ME! */
    public static final int DD_PhysicalDeltaX = 989;

    /** DOCUMENT ME! */
    public static final int DD_SafePositionExitDate = 990;

    /** DOCUMENT ME! */
    public static final int DD_DVHData = 991;

    /** DOCUMENT ME! */
    public static final int DD_ReferencePixelPhysicalValueY = 992;

    /** DOCUMENT ME! */
    public static final int DD_StudyReadTime = 993;

    /** DOCUMENT ME! */
    public static final int DD_BrachyControlPointDeliveredSequence = 994;

    /** DOCUMENT ME! */
    public static final int DD_StudyReadDate = 995;

    /** DOCUMENT ME! */
    public static final int DD_DVHNumberOfBins = 996;

    /** DOCUMENT ME! */
    public static final int DD_StudyVerifiedTime = 997;

    /** DOCUMENT ME! */
    public static final int DD_StudyVerifiedDate = 998;

    /** DOCUMENT ME! */
    public static final int DD_DVHVolumeUnits = 999;

    /** DOCUMENT ME! */
    public static final int DD_DVHDoseScaling = 1000;

    /** DOCUMENT ME! */
    public static final int DD_DVHSequence = 1001;

    /** DOCUMENT ME! */
    public static final int DD_NumericValue = 1002;

    /** DOCUMENT ME! */
    public static final int DD_MaxDensity = 1003;

    /** DOCUMENT ME! */
    public static final int DD_DopplerSampleVolumeXPosition = 1004;

    /** DOCUMENT ME! */
    public static final int DD_FrameOfInterestDescription = 1005;

    /** DOCUMENT ME! */
    public static final int DD_SteeringAngle = 1006;

    /** DOCUMENT ME! */
    public static final int DD_FrameNumbersOfInterest = 1007;

    /** DOCUMENT ME! */
    public static final int DD_DopplerCorrectionAngle = 1008;

    /** DOCUMENT ME! */
    public static final int DD_PulseRepetitionFrequency = 1009;

    /** DOCUMENT ME! */
    public static final int DD_TransducerType = 1010;

    /** DOCUMENT ME! */
    public static final int DD_TransducerFrequency = 1011;

    /** DOCUMENT ME! */
    public static final int DD_RegionLocationMaxY1 = 1012;

    /** DOCUMENT ME! */
    public static final int DD_RegionLocationMaxX1 = 1013;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedChannelShieldNumber = 1014;

    /** DOCUMENT ME! */
    public static final int DD_RegionLocationMinY0 = 1015;

    /** DOCUMENT ME! */
    public static final int DD_HistogramData = 1016;

    /** DOCUMENT ME! */
    public static final int DD_RecordedChannelShieldSequence = 1017;

    /** DOCUMENT ME! */
    public static final int DD_DVHNormalizationDoseValue = 1018;

    /** DOCUMENT ME! */
    public static final int DD_DVHNormalizationPoint = 1019;

    /** DOCUMENT ME! */
    public static final int DD_MinDensity = 1020;

    /** DOCUMENT ME! */
    public static final int DD_ReferencePixelPhysicalValueX = 1021;

    /** DOCUMENT ME! */
    public static final int DD_PhysicalUnitsYDirection = 1022;

    /** DOCUMENT ME! */
    public static final int DD_RepresentativeFrameNumber = 1023;

    /** DOCUMENT ME! */
    public static final int DD_PhysicalUnitsXDirection = 1024;

    /** DOCUMENT ME! */
    public static final int DD_ReferencePixelY0 = 1025;

    /** DOCUMENT ME! */
    public static final int DD_StudyPriorityID = 1026;

    /** DOCUMENT ME! */
    public static final int DD_ReferencePixelX0 = 1027;

    /** DOCUMENT ME! */
    public static final int DD_StudyStatusID = 1028;

    /** DOCUMENT ME! */
    public static final int DD_FrameOfReferenceTransformationComment = 1029;

    /** DOCUMENT ME! */
    public static final int DD_FrameOfReferenceTransformationMatrix = 1030;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedSourceApplicatorNumber = 1031;

    /** DOCUMENT ME! */
    public static final int DD_HistogramExplanation = 1032;

    /** DOCUMENT ME! */
    public static final int DD_FrameOfReferenceTransformationType = 1033;

    /** DOCUMENT ME! */
    public static final int DD_RecordedSourceApplicatorSequence = 1034;

    /** DOCUMENT ME! */
    public static final int DD_RelatedFrameOfReferenceUID = 1035;

    /** DOCUMENT ME! */
    public static final int DD_StudyIDIssuer = 1036;

    /** DOCUMENT ME! */
    public static final int DD_FrameOfReferenceRelationshipSequence = 1037;

    /** DOCUMENT ME! */
    public static final int DD_EmptyImageDensity = 1038;

    /** DOCUMENT ME! */
    public static final int DD_RegionLocationMinX0 = 1039;

    /** DOCUMENT ME! */
    public static final int DD_RegionFlags = 1040;

    /** DOCUMENT ME! */
    public static final int DD_HumanPerformersName = 1041;

    /** DOCUMENT ME! */
    public static final int DD_DeliveredPulseRepetitionInterval = 1042;

    /** DOCUMENT ME! */
    public static final int DD_HumanPerformersOrganization = 1043;

    /** DOCUMENT ME! */
    public static final int DD_RegionDataType = 1044;

    /** DOCUMENT ME! */
    public static final int DD_ActualHumanPerformersSequence = 1045;

    /** DOCUMENT ME! */
    public static final int DD_SpecifiedPulseRepetitionInterval = 1046;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledHumanPerformersSequence = 1047;

    /** DOCUMENT ME! */
    public static final int DD_OutputInformationSequence = 1048;

    /** DOCUMENT ME! */
    public static final int DD_RegionSpatialFormat = 1049;

    /** DOCUMENT ME! */
    public static final int DD_NonDICOMOutputCodeSequence = 1050;

    /** DOCUMENT ME! */
    public static final int DD_SequenceOfUltrasoundRegions = 1051;

    /** DOCUMENT ME! */
    public static final int DD_RequestedSubsequentWorkitemCodeSequence = 1052;

    /** DOCUMENT ME! */
    public static final int DD_PerformedStationGeographicLocationCodeSequence = 1053;

    /** DOCUMENT ME! */
    public static final int DD_PresentationLUTContentSequence = 1054;

    /** DOCUMENT ME! */
    public static final int DD_HistogramBinWidth = 1055;

    /** DOCUMENT ME! */
    public static final int DD_DeliveredNumberOfPulses = 1056;

    /** DOCUMENT ME! */
    public static final int DD_HistogramLastBinValue = 1057;

    /** DOCUMENT ME! */
    public static final int DD_SpecifiedNumberOfPulses = 1058;

    /** DOCUMENT ME! */
    public static final int DD_HistogramFirstBinValue = 1059;

    /** DOCUMENT ME! */
    public static final int DD_DeliveredChannelTotalTime = 1060;

    /** DOCUMENT ME! */
    public static final int DD_HistogramNumberOfBins = 1061;

    /** DOCUMENT ME! */
    public static final int DD_SpecifiedChannelTotalTime = 1062;

    /** DOCUMENT ME! */
    public static final int DD_HistogramSequence = 1063;

    /** DOCUMENT ME! */
    public static final int DD_ROIPhysicalPropertyValue = 1064;

    /** DOCUMENT ME! */
    public static final int DD_RecordedChannelSequence = 1065;

    /** DOCUMENT ME! */
    public static final int DD_ROIPhysicalProperty = 1066;

    /** DOCUMENT ME! */
    public static final int DD_ROIPhysicalPropertiesSequence = 1067;

    /** DOCUMENT ME! */
    public static final int DD_StudyGroupLength = 1068;

    /** DOCUMENT ME! */
    public static final int DD_BorderDensity = 1069;

    /** DOCUMENT ME! */
    public static final int DD_PerformedStationClassCodeSequence = 1070;

    /** DOCUMENT ME! */
    public static final int DD_PerformedStationNameCodeSequence = 1071;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStationGeographicLocationCodeSequence = 1072;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStationClassCodeSequence = 1073;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStationNameCodeSequence = 1074;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedGeneralPurposeScheduledProcedureStepTransactionUID = 1075;

    /** DOCUMENT ME! */
    public static final int DD_DigitalSignaturesSequence = 1076;

    /** DOCUMENT ME! */
    public static final int DD_RelevantInformationSequence = 1077;

    /** DOCUMENT ME! */
    public static final int DD_InputInformationSequence = 1078;

    /** DOCUMENT ME! */
    public static final int DD_Sensitivity = 1079;

    /** DOCUMENT ME! */
    public static final int DD_InputAvailabilityFlag = 1080;

    /** DOCUMENT ME! */
    public static final int DD_ROIInterpreter = 1081;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedBrachyAccessoryDeviceNumber = 1082;

    /** DOCUMENT ME! */
    public static final int DD_RTROIInterpretedType = 1083;

    /** DOCUMENT ME! */
    public static final int DD_RecordedBrachyAccessoryDeviceSequence = 1084;

    /** DOCUMENT ME! */
    public static final int DD_RelatedRTROIObservationsSequence = 1085;

    /** DOCUMENT ME! */
    public static final int DD_DoseValue = 1086;

    /** DOCUMENT ME! */
    public static final int DD_RTDoseROISequence = 1087;

    /** DOCUMENT ME! */
    public static final int DD_PerformedWorkitemCodeSequence = 1088;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledWorkitemCodeSequence = 1089;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedGeneralPurposeScheduledProcedureStepSequence = 1090;

    /** DOCUMENT ME! */
    public static final int DD_ResultingGeneralPurposePerformedProcedureStepsSequence = 1091;

    /** DOCUMENT ME! */
    public static final int DD_DoseGridScaling = 1092;

    /** DOCUMENT ME! */
    public static final int DD_ExpectedCompletionDateAndTime = 1093;

    /** DOCUMENT ME! */
    public static final int DD_GridFrameOffsetVector = 1094;

    /** DOCUMENT ME! */
    public static final int DD_ImageOverlayBoxContentSequence = 1095;

    /** DOCUMENT ME! */
    public static final int DD_DoseSummationType = 1096;

    /** DOCUMENT ME! */
    public static final int DD_StorageGroupLength = 1097;

    /** DOCUMENT ME! */
    public static final int DD_ApplicationSetupCheck = 1098;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedPrintJobSequenceQueue = 1099;

    /** DOCUMENT ME! */
    public static final int DD_NormalizationPoint = 1100;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentSessionApplicationSetupSequence = 1101;

    /** DOCUMENT ME! */
    public static final int DD_DoseComment = 1102;

    /** DOCUMENT ME! */
    public static final int DD_DoseType = 1103;

    /** DOCUMENT ME! */
    public static final int DD_DoseUnits = 1104;

    /** DOCUMENT ME! */
    public static final int DD_DVHType = 1105;

    /** DOCUMENT ME! */
    public static final int DD_HumanPerformerCodeSequence = 1106;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcessingApplicationsCodeSequence = 1107;

    /** DOCUMENT ME! */
    public static final int DD_MultipleCopiesFlag = 1108;

    /** DOCUMENT ME! */
    public static final int DD_CompressionForce = 1109;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcedureStepStartDateAndTime = 1110;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledProcessingApplicationsCodeSequence = 1111;

    /** DOCUMENT ME! */
    public static final int DD_BodyPartThickness = 1112;

    /** DOCUMENT ME! */
    public static final int DD_GeneralPurposeScheduledProcedureStepPriority = 1113;

    /** DOCUMENT ME! */
    public static final int DD_GeneralPurposePerformedProcedureStepStatus = 1114;

    /** DOCUMENT ME! */
    public static final int DD_GeneralPurposeScheduledProcedureStepStatus = 1115;

    /** DOCUMENT ME! */
    public static final int DD_AnnotationContentSequence = 1116;

    /** DOCUMENT ME! */
    public static final int DD_SourceSerialNumber = 1117;

    /** DOCUMENT ME! */
    public static final int DD_RecordedSourceSequence = 1118;

    /** DOCUMENT ME! */
    public static final int DD_ImageBoxContentSequence = 1119;

    /** DOCUMENT ME! */
    public static final int DD_PrintJobDescriptionSequence = 1120;

    /** DOCUMENT ME! */
    public static final int DD_FilmBoxContentSequence = 1121;

    /** DOCUMENT ME! */
    public static final int DD_TableTopLateralSetupDisplacement = 1122;

    /** DOCUMENT ME! */
    public static final int DD_TableTopLongitudinalSetupDisplacement = 1123;

    /** DOCUMENT ME! */
    public static final int DD_TableTopVerticalSetupDisplacement = 1124;

    /** DOCUMENT ME! */
    public static final int DD_SetupReferenceDescription = 1125;

    /** DOCUMENT ME! */
    public static final int DD_EndMessageSet = 1126;

    /** DOCUMENT ME! */
    public static final int DD_SetupDeviceParameter = 1127;

    /** DOCUMENT ME! */
    public static final int DD_SetupDeviceDescription = 1128;

    /** DOCUMENT ME! */
    public static final int DD_PrinterCharacteristicsSequence = 1129;

    /** DOCUMENT ME! */
    public static final int DD_MessageSetID = 1130;

    /** DOCUMENT ME! */
    public static final int DD_PrintManagementCapabilitiesSequence = 1131;

    /** DOCUMENT ME! */
    public static final int DD_SetupDeviceLabel = 1132;

    /** DOCUMENT ME! */
    public static final int DD_SetupDeviceType = 1133;

    /** DOCUMENT ME! */
    public static final int DD_SetupDeviceSequence = 1134;

    /** DOCUMENT ME! */
    public static final int DD_ImageTranslationVector = 1135;

    /** DOCUMENT ME! */
    public static final int DD_SetupTechniqueDescription = 1136;

    /** DOCUMENT ME! */
    public static final int DD_ImageTransformationMatrix = 1137;

    /** DOCUMENT ME! */
    public static final int DD_SetupTechnique = 1138;

    /** DOCUMENT ME! */
    public static final int DD_DataSetType = 1139;

    /** DOCUMENT ME! */
    public static final int DD_ShieldingDevicePosition = 1140;

    /** DOCUMENT ME! */
    public static final int DD_QueueStatus = 1141;

    /** DOCUMENT ME! */
    public static final int DD_ShieldingDeviceDescription = 1142;

    /** DOCUMENT ME! */
    public static final int DD_ShieldingDeviceLabel = 1143;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationStatusID = 1144;

    /** DOCUMENT ME! */
    public static final int DD_ShieldingDeviceType = 1145;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationTypeID = 1146;

    /** DOCUMENT ME! */
    public static final int DD_ShieldingDeviceSequence = 1147;

    /** DOCUMENT ME! */
    public static final int DD_VOILUTSequence = 1148;

    /** DOCUMENT ME! */
    public static final int DD_SecondaryCountsType = 1149;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationIDIssuer = 1150;

    /** DOCUMENT ME! */
    public static final int DD_PaletteColorLookupTableUID = 1151;

    /** DOCUMENT ME! */
    public static final int DD_InterpretationID = 1152;

    /** DOCUMENT ME! */
    public static final int DD_DeviceDescription = 1153;

    /** DOCUMENT ME! */
    public static final int DD_LUTData = 1154;

    /** DOCUMENT ME! */
    public static final int DD_ModalityLUTType = 1155;

    /** DOCUMENT ME! */
    public static final int DD_LUTExplanation = 1156;

    /** DOCUMENT ME! */
    public static final int DD_LUTDescriptor = 1157;

    /** DOCUMENT ME! */
    public static final int DD_ModalityLUTSequence = 1158;

    /** DOCUMENT ME! */
    public static final int DD_CoincidenceWindowWidth = 1159;

    /** DOCUMENT ME! */
    public static final int DD_ROIObservationDescription = 1160;

    /** DOCUMENT ME! */
    public static final int DD_RTROIIdentificationCodeSequence = 1161;

    /** DOCUMENT ME! */
    public static final int DD_ROIObservationLabel = 1162;

    /** DOCUMENT ME! */
    public static final int DD_InterMarkerDistance = 1163;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedROINumber = 1164;

    /** DOCUMENT ME! */
    public static final int DD_DeviceVolume = 1165;

    /** DOCUMENT ME! */
    public static final int DD_DeviceDiameterUnits = 1166;

    /** DOCUMENT ME! */
    public static final int DD_ObservationNumber = 1167;

    /** DOCUMENT ME! */
    public static final int DD_DeviceDiameter = 1168;

    /** DOCUMENT ME! */
    public static final int DD_RTROIObservationsSequence = 1169;

    /** DOCUMENT ME! */
    public static final int DD_DeviceLength = 1170;

    /** DOCUMENT ME! */
    public static final int DD_DeviceSequence = 1171;

    /** DOCUMENT ME! */
    public static final int DD_AnodeTargetMaterial = 1172;

    /** DOCUMENT ME! */
    public static final int DD_FocalSpot = 1173;

    /** DOCUMENT ME! */
    public static final int DD_DetectorElementSize = 1174;

    /** DOCUMENT ME! */
    public static final int DD_TransverseMash = 1175;

    /** DOCUMENT ME! */
    public static final int DD_AxialMash = 1176;

    /** DOCUMENT ME! */
    public static final int DD_MeasurementUnitsCodeSequence = 1177;

    /** DOCUMENT ME! */
    public static final int DD_AxialAcceptance = 1178;

    /** DOCUMENT ME! */
    public static final int DD_BillingItemSequence = 1179;

    /** DOCUMENT ME! */
    public static final int DD_MeasuringUnitsSequence = 1180;

    /** DOCUMENT ME! */
    public static final int DD_Quantity = 1181;

    /** DOCUMENT ME! */
    public static final int DD_QuantitySequence = 1182;

    /** DOCUMENT ME! */
    public static final int DD_CalibrationImage = 1183;

    /** DOCUMENT ME! */
    public static final int DD_CalibrationGroupLength = 1184;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedSOPSequence = 1185;

    /** DOCUMENT ME! */
    public static final int DD_YFocusCenter = 1186;

    /** DOCUMENT ME! */
    public static final int DD_FailedSOPSequence = 1187;

    /** DOCUMENT ME! */
    public static final int DD_XFocusCenter = 1188;

    /** DOCUMENT ME! */
    public static final int DD_FailureReason = 1189;

    /** DOCUMENT ME! */
    public static final int DD_FocalDistance = 1190;

    /** DOCUMENT ME! */
    public static final int DD_CollimatorType = 1191;

    /** DOCUMENT ME! */
    public static final int DD_TransactionUID = 1192;

    /** DOCUMENT ME! */
    public static final int DD_CollimatorGridName = 1193;

    /** DOCUMENT ME! */
    public static final int DD_PixelBandwidth = 1194;

    /** DOCUMENT ME! */
    public static final int DD_PercentPhaseFieldOfView = 1195;

    /** DOCUMENT ME! */
    public static final int DD_NamesOfIntendedRecipientsOfResults = 1196;

    /** DOCUMENT ME! */
    public static final int DD_PercentSampling = 1197;

    /** DOCUMENT ME! */
    public static final int DD_EchoTrainLength = 1198;

    /** DOCUMENT ME! */
    public static final int DD_DataCollectionDiameter = 1199;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfTimeSlices = 1200;

    /** DOCUMENT ME! */
    public static final int DD_TimeSliceVector = 1201;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureStepDiscontinuationReasonCodeSequence = 1202;

    /** DOCUMENT ME! */
    public static final int DD_CommentsOnPerformedProcedureStep = 1203;

    /** DOCUMENT ME! */
    public static final int DD_ReportingPriority = 1204;

    /** DOCUMENT ME! */
    public static final int DD_ConfidentialityCode = 1205;

    /** DOCUMENT ME! */
    public static final int DD_GeneratorPower = 1206;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfPhaseEncodingSteps = 1207;

    /** DOCUMENT ME! */
    public static final int DD_RequestedProcedureLocation = 1208;

    /** DOCUMENT ME! */
    public static final int DD_SpacingBetweenSlices = 1209;

    /** DOCUMENT ME! */
    public static final int DD_PatientTransportArrangements = 1210;

    /** DOCUMENT ME! */
    public static final int DD_MagneticFieldStrength = 1211;

    /** DOCUMENT ME! */
    public static final int DD_RequestedProcedurePriority = 1212;

    /** DOCUMENT ME! */
    public static final int DD_EchoNumber = 1213;

    /** DOCUMENT ME! */
    public static final int DD_ReasonForRequestedProcedure = 1214;

    /** DOCUMENT ME! */
    public static final int DD_ImagedNucleus = 1215;

    /** DOCUMENT ME! */
    public static final int DD_RequestedProcedureID = 1216;

    /** DOCUMENT ME! */
    public static final int DD_ImagingFrequency = 1217;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfAverages = 1218;

    /** DOCUMENT ME! */
    public static final int DD_InversionTime = 1219;

    /** DOCUMENT ME! */
    public static final int DD_ImageAreaDoseProduct = 1220;

    /** DOCUMENT ME! */
    public static final int DD_EchoTime = 1221;

    /** DOCUMENT ME! */
    public static final int DD_RepetitionTime = 1222;

    /** DOCUMENT ME! */
    public static final int DD_ReferringPhysicianTelephoneNumber = 1223;

    /** DOCUMENT ME! */
    public static final int DD_RadiationMode = 1224;

    /** DOCUMENT ME! */
    public static final int DD_ReferringPhysicianAddress = 1225;

    /** DOCUMENT ME! */
    public static final int DD_RequestAttributesSequence = 1226;

    /** DOCUMENT ME! */
    public static final int DD_ReferringPhysicianName = 1227;

    /** DOCUMENT ME! */
    public static final int DD_TreatmentSummaryMeasuredDoseReferenceSequence = 1228;

    /** DOCUMENT ME! */
    public static final int DD_ContourData = 1229;

    /** DOCUMENT ME! */
    public static final int DD_ItemDelimitationItem = 1230;

    /** DOCUMENT ME! */
    public static final int DD_ScheduledStepAttributesSequence = 1231;

    /** DOCUMENT ME! */
    public static final int DD_Grid = 1232;

    /** DOCUMENT ME! */
    public static final int DD_ImagerPixelSpacing = 1233;

    /** DOCUMENT ME! */
    public static final int DD_IntensifierSize = 1234;

    /** DOCUMENT ME! */
    public static final int DD_TypeOfFilters = 1235;

    /** DOCUMENT ME! */
    public static final int DD_FilterType = 1236;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionTerminationConditionData = 1237;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionStartConditionData = 1238;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionStartCondition = 1239;

    /** DOCUMENT ME! */
    public static final int DD_EffectiveSeriesDuration = 1240;

    /** DOCUMENT ME! */
    public static final int DD_Item = 1241;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionTerminationCondition = 1242;

    /** DOCUMENT ME! */
    public static final int DD_AttachedContours = 1243;

    /** DOCUMENT ME! */
    public static final int DD_CountsAccumulated = 1244;

    /** DOCUMENT ME! */
    public static final int DD_ContourNumber = 1245;

    /** DOCUMENT ME! */
    public static final int DD_InstitutionCodeSequence = 1246;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfContourPoints = 1247;

    /** DOCUMENT ME! */
    public static final int DD_InstitutionAddress = 1248;

    /** DOCUMENT ME! */
    public static final int DD_ContourOffsetVector = 1249;

    /** DOCUMENT ME! */
    public static final int DD_OtherSmoothingTypesAvailable = 1250;

    /** DOCUMENT ME! */
    public static final int DD_InstitutionName = 1251;

    /** DOCUMENT ME! */
    public static final int DD_ContourSlabThickness = 1252;

    /** DOCUMENT ME! */
    public static final int DD_DefaultSmoothingType = 1253;

    /** DOCUMENT ME! */
    public static final int DD_RecordedBlockSequence = 1254;

    /** DOCUMENT ME! */
    public static final int DD_OtherMagnificationTypesAvailable = 1255;

    /** DOCUMENT ME! */
    public static final int DD_ContourGeometricType = 1256;

    /** DOCUMENT ME! */
    public static final int DD_DefaultMagnificationType = 1257;

    /** DOCUMENT ME! */
    public static final int DD_ContourSequence = 1258;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProtocolCodeSequence = 1259;

    /** DOCUMENT ME! */
    public static final int DD_RectificationType = 1260;

    /** DOCUMENT ME! */
    public static final int DD_RadiationSetting = 1261;

    /** DOCUMENT ME! */
    public static final int DD_AveragePulseWidth = 1262;

    /** DOCUMENT ME! */
    public static final int DD_ExposureInuAs = 1263;

    /** DOCUMENT ME! */
    public static final int DD_Exposure = 1264;

    /** DOCUMENT ME! */
    public static final int DD_XrayTubeCurrent = 1265;

    /** DOCUMENT ME! */
    public static final int DD_ExposureTime = 1266;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFrameNumber = 1267;

    /** DOCUMENT ME! */
    public static final int DD_CorrectedImage = 1268;

    /** DOCUMENT ME! */
    public static final int DD_ROIContourSequence = 1269;

    /** DOCUMENT ME! */
    public static final int DD_KVP = 1270;

    /** DOCUMENT ME! */
    public static final int DD_ROIGenerationDescription = 1271;

    /** DOCUMENT ME! */
    public static final int DD_ROIGenerationAlgorithm = 1272;

    /** DOCUMENT ME! */
    public static final int DD_AudioComments = 1273;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureTypeDescription = 1274;

    /** DOCUMENT ME! */
    public static final int DD_Manufacturer = 1275;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureStepDescription = 1276;

    /** DOCUMENT ME! */
    public static final int DD_RTROIRelationship = 1277;

    /** DOCUMENT ME! */
    public static final int DD_RecordedCompensatorSequence = 1278;

    /** DOCUMENT ME! */
    public static final int DD_AudioSampleData = 1279;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureStepID = 1280;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureStepStatus = 1281;

    /** DOCUMENT ME! */
    public static final int DD_SOPClassesSupported = 1282;

    /** DOCUMENT ME! */
    public static final int DD_TransducerOrientationModifierSequence = 1283;

    /** DOCUMENT ME! */
    public static final int DD_TotalTime = 1284;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureStepEndTime = 1285;

    /** DOCUMENT ME! */
    public static final int DD_RTRelatedROISequence = 1286;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureStepEndDate = 1287;

    /** DOCUMENT ME! */
    public static final int DD_TransducerOrientationSequence = 1288;

    /** DOCUMENT ME! */
    public static final int DD_SupportedImageDisplayFormatsSequence = 1289;

    /** DOCUMENT ME! */
    public static final int DD_FieldOfViewDimensions = 1290;

    /** DOCUMENT ME! */
    public static final int DD_TransducerPositionModifierSequence = 1291;

    /** DOCUMENT ME! */
    public static final int DD_FieldOfViewShape = 1292;

    /** DOCUMENT ME! */
    public static final int DD_TransducerPositionSequence = 1293;

    /** DOCUMENT ME! */
    public static final int DD_OtherMediaAvailableSequence = 1294;

    /** DOCUMENT ME! */
    public static final int DD_CenterOfRotationOffset = 1295;

    /** DOCUMENT ME! */
    public static final int DD_AngularStep = 1296;

    /** DOCUMENT ME! */
    public static final int DD_MediaInstalledSequence = 1297;

    /** DOCUMENT ME! */
    public static final int DD_ScanArc = 1298;

    /** DOCUMENT ME! */
    public static final int DD_PrintingBitDepth = 1299;

    /** DOCUMENT ME! */
    public static final int DD_RadialPosition = 1300;

    /** DOCUMENT ME! */
    public static final int DD_MemoryBitDepth = 1301;

    /** DOCUMENT ME! */
    public static final int DD_AngularPosition = 1302;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedSOPInstanceUID = 1303;

    /** DOCUMENT ME! */
    public static final int DD_RotationDirection = 1304;

    /** DOCUMENT ME! */
    public static final int DD_ROIVolume = 1305;

    /** DOCUMENT ME! */
    public static final int DD_ROIDisplayColor = 1306;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedSOPClassUID = 1307;

    /** DOCUMENT ME! */
    public static final int DD_PresentationIntentType = 1308;

    /** DOCUMENT ME! */
    public static final int DD_FixationDevicePosition = 1309;

    /** DOCUMENT ME! */
    public static final int DD_SliceThickness = 1310;

    /** DOCUMENT ME! */
    public static final int DD_FixationDeviceDescription = 1311;

    /** DOCUMENT ME! */
    public static final int DD_ConversionType = 1312;

    /** DOCUMENT ME! */
    public static final int DD_ROIDescription = 1313;

    /** DOCUMENT ME! */
    public static final int DD_FixationDeviceLabel = 1314;

    /** DOCUMENT ME! */
    public static final int DD_ROIName = 1315;

    /** DOCUMENT ME! */
    public static final int DD_TableType = 1316;

    /** DOCUMENT ME! */
    public static final int DD_ModalitiesInStudy = 1317;

    /** DOCUMENT ME! */
    public static final int DD_FixationDeviceType = 1318;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureStepStartTime = 1319;

    /** DOCUMENT ME! */
    public static final int DD_Modality = 1320;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFrameOfReferenceUID = 1321;

    /** DOCUMENT ME! */
    public static final int DD_PerformedProcedureStepStartDate = 1322;

    /** DOCUMENT ME! */
    public static final int DD_RecordedWedgeSequence = 1323;

    /** DOCUMENT ME! */
    public static final int DD_FixationDeviceSequence = 1324;

    /** DOCUMENT ME! */
    public static final int DD_PerformedLocation = 1325;

    /** DOCUMENT ME! */
    public static final int DD_ROINumber = 1326;

    /** DOCUMENT ME! */
    public static final int DD_PerformedStationName = 1327;

    /** DOCUMENT ME! */
    public static final int DD_PerformedStationAETitle = 1328;

    /** DOCUMENT ME! */
    public static final int DD_StructureSetROISequence = 1329;

    /** DOCUMENT ME! */
    public static final int DD_TableAngle = 1330;

    /** DOCUMENT ME! */
    public static final int DD_TableLongitudinalIncrement = 1331;

    /** DOCUMENT ME! */
    public static final int DD_PrimaryAnatomicStructureModifierSequence = 1332;

    /** DOCUMENT ME! */
    public static final int DD_TableLateralIncrement = 1333;

    /** DOCUMENT ME! */
    public static final int DD_TableVerticalIncrement = 1334;

    /** DOCUMENT ME! */
    public static final int DD_TableMotion = 1335;

    /** DOCUMENT ME! */
    public static final int DD_DataSetTrailingPadding = 1336;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfTomosynthesisSourceImages = 1337;

    /** DOCUMENT ME! */
    public static final int DD_SampleRate = 1338;

    /** DOCUMENT ME! */
    public static final int DD_TableTraverse = 1339;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedCurveSequence = 1340;

    /** DOCUMENT ME! */
    public static final int DD_TableHeight = 1341;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfSamples = 1342;

    /** DOCUMENT ME! */
    public static final int DD_MedicalAlerts = 1343;

    /** DOCUMENT ME! */
    public static final int DD_TomoClass = 1344;

    /** DOCUMENT ME! */
    public static final int DD_TomoType = 1345;

    /** DOCUMENT ME! */
    public static final int DD_PixelAspectRatio = 1346;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfChannels = 1347;

    /** DOCUMENT ME! */
    public static final int DD_ZoomCenter = 1348;

    /** DOCUMENT ME! */
    public static final int DD_AudioSampleFormat = 1349;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedImageSequence = 1350;

    /** DOCUMENT ME! */
    public static final int DD_ZoomFactor = 1351;

    /** DOCUMENT ME! */
    public static final int DD_SpecimenTypeCodeSequence = 1352;

    /** DOCUMENT ME! */
    public static final int DD_PixelSpacing = 1353;

    /** DOCUMENT ME! */
    public static final int DD_AudioType = 1354;

    /** DOCUMENT ME! */
    public static final int DD_FailedSOPInstanceUIDList = 1355;

    /** DOCUMENT ME! */
    public static final int DD_InstanceAvailability = 1356;

    /** DOCUMENT ME! */
    public static final int DD_CineRate = 1357;

    /** DOCUMENT ME! */
    public static final int DD_RetrieveAETitle = 1358;

    /** DOCUMENT ME! */
    public static final int DD_PatientAdditionalPosition = 1359;

    /** DOCUMENT ME! */
    public static final int DD_QueryRetrieveLevel = 1360;

    /** DOCUMENT ME! */
    public static final int DD_ContourImageSequence = 1361;

    /** DOCUMENT ME! */
    public static final int DD_PatientSetupNumber = 1362;

    /** DOCUMENT ME! */
    public static final int DD_AccessionNumber = 1363;

    /** DOCUMENT ME! */
    public static final int DD_AnatomicStructureSpaceOrRegionSequence = 1364;

    /** DOCUMENT ME! */
    public static final int DD_RTReferencedSeriesSequence = 1365;

    /** DOCUMENT ME! */
    public static final int DD_PatientGantryRelationshipCodeSequence = 1366;

    /** DOCUMENT ME! */
    public static final int DD_PrimaryAnatomicStructureSequence = 1367;

    /** DOCUMENT ME! */
    public static final int DD_BeamLimitingDeviceLeafPairsSequence = 1368;

    /** DOCUMENT ME! */
    public static final int DD_PatientSetupSequence = 1369;

    /** DOCUMENT ME! */
    public static final int DD_RTReferencedStudySequence = 1370;

    /** DOCUMENT ME! */
    public static final int DD_PatientOrientationModifierCodeSequence = 1371;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedFrameOfReferenceSequence = 1372;

    /** DOCUMENT ME! */
    public static final int DD_PatientOrientationCodeSequence = 1373;

    /** DOCUMENT ME! */
    public static final int DD_AdditionalDrugSequence = 1374;

    /** DOCUMENT ME! */
    public static final int DD_AnatomicRegionModifierSequence = 1375;

    /** DOCUMENT ME! */
    public static final int DD_PixelData = 1376;

    /** DOCUMENT ME! */
    public static final int DD_ImagingServiceRequestComments = 1377;

    /** DOCUMENT ME! */
    public static final int DD_GantryDetectorSlew = 1378;

    /** DOCUMENT ME! */
    public static final int DD_GantryDetectorTilt = 1379;

    /** DOCUMENT ME! */
    public static final int DD_TherapyDescription = 1380;

    /** DOCUMENT ME! */
    public static final int DD_TomoTime = 1381;

    /** DOCUMENT ME! */
    public static final int DD_InterventionStatus = 1382;

    /** DOCUMENT ME! */
    public static final int DD_TherapyType = 1383;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedOverlaySequence = 1384;

    /** DOCUMENT ME! */
    public static final int DD_InterventionTherapySequence = 1385;

    /** DOCUMENT ME! */
    public static final int DD_InterventionDrugStartTime = 1386;

    /** DOCUMENT ME! */
    public static final int DD_InterventionDrugName = 1387;

    /** DOCUMENT ME! */
    public static final int DD_Radiopharmaceutical = 1388;

    /** DOCUMENT ME! */
    public static final int DD_StructureSetTime = 1389;

    /** DOCUMENT ME! */
    public static final int DD_StructureSetDate = 1390;

    /** DOCUMENT ME! */
    public static final int DD_StructureSetDescription = 1391;

    /** DOCUMENT ME! */
    public static final int DD_StructureSetName = 1392;

    /** DOCUMENT ME! */
    public static final int DD_AnatomicRegionSequence = 1393;

    /** DOCUMENT ME! */
    public static final int DD_StructureSetLabel = 1394;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedNonImageCompositeSOPInstanceSequence = 1395;

    /** DOCUMENT ME! */
    public static final int DD_ImageID = 1396;

    /** DOCUMENT ME! */
    public static final int DD_BluePaletteColorLookupTableDescriptor = 1397;

    /** DOCUMENT ME! */
    public static final int DD_GreenPaletteColorLookupTableDescriptor = 1398;

    /** DOCUMENT ME! */
    public static final int DD_RedPaletteColorLookupTableDescriptor = 1399;

    /** DOCUMENT ME! */
    public static final int DD_PixelDataGroupLength = 1400;

    /** DOCUMENT ME! */
    public static final int DD_EstimatedRadiographicMagnificationFactor = 1401;

    /** DOCUMENT ME! */
    public static final int DD_DistanceSourceToPatient = 1402;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedVisitSequence = 1403;

    /** DOCUMENT ME! */
    public static final int DD_DistanceSourceToDetector = 1404;

    /** DOCUMENT ME! */
    public static final int DD_InterventionDrugCodeSequence = 1405;

    /** DOCUMENT ME! */
    public static final int DD_TomoAngle = 1406;

    /** DOCUMENT ME! */
    public static final int DD_UltrasoundColorDataPresent = 1407;

    /** DOCUMENT ME! */
    public static final int DD_InterventionDrugDose = 1408;

    /** DOCUMENT ME! */
    public static final int DD_InterventionDrugStopTime = 1409;

    /** DOCUMENT ME! */
    public static final int DD_Planes = 1410;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedPatientSequence = 1411;

    /** DOCUMENT ME! */
    public static final int DD_InterventionDrugInformationSequence = 1412;

    /** DOCUMENT ME! */
    public static final int DD_Columns = 1413;

    /** DOCUMENT ME! */
    public static final int DD_AngioFlag = 1414;

    /** DOCUMENT ME! */
    public static final int DD_Rows = 1415;

    /** DOCUMENT ME! */
    public static final int DD_SequenceName = 1416;

    /** DOCUMENT ME! */
    public static final int DD_MRAcquisitionType = 1417;

    /** DOCUMENT ME! */
    public static final int DD_ScanOptions = 1418;

    /** DOCUMENT ME! */
    public static final int DD_SequenceVariant = 1419;

    /** DOCUMENT ME! */
    public static final int DD_CurveTime = 1420;

    /** DOCUMENT ME! */
    public static final int DD_ScanningSequence = 1421;

    /** DOCUMENT ME! */
    public static final int DD_TopicKeyWords = 1422;

    /** DOCUMENT ME! */
    public static final int DD_OverlayTime = 1423;

    /** DOCUMENT ME! */
    public static final int DD_ContentTime = 1424;

    /** DOCUMENT ME! */
    public static final int DD_TopicAuthor = 1425;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionTime = 1426;

    /** DOCUMENT ME! */
    public static final int DD_SeriesTime = 1427;

    /** DOCUMENT ME! */
    public static final int DD_StudyTime = 1428;

    /** DOCUMENT ME! */
    public static final int DD_FrameIncrementPointer = 1429;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfFrames = 1430;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedSeriesSequence = 1431;

    /** DOCUMENT ME! */
    public static final int DD_ReconstructionDiameter = 1432;

    /** DOCUMENT ME! */
    public static final int DD_PlanarConfiguration = 1433;

    /** DOCUMENT ME! */
    public static final int DD_TomoLayerHeight = 1434;

    /** DOCUMENT ME! */
    public static final int DD_PhotometricInterpretation = 1435;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedPerformedProcedureStepSequence = 1436;

    /** DOCUMENT ME! */
    public static final int DD_SamplesPerPixel = 1437;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedStudySequence = 1438;

    /** DOCUMENT ME! */
    public static final int DD_StudyComments = 1439;

    /** DOCUMENT ME! */
    public static final int DD_BodyPartExamined = 1440;

    /** DOCUMENT ME! */
    public static final int DD_ImagePresentationGroupLength = 1441;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusAdministrationRouteSequence = 1442;

    /** DOCUMENT ME! */
    public static final int DD_TopicSubject = 1443;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusAgentSequence = 1444;

    /** DOCUMENT ME! */
    public static final int DD_TopicTitle = 1445;

    /** DOCUMENT ME! */
    public static final int DD_CurveDate = 1446;

    /** DOCUMENT ME! */
    public static final int DD_ContrastBolusAgent = 1447;

    /** DOCUMENT ME! */
    public static final int DD_OverlayDate = 1448;

    /** DOCUMENT ME! */
    public static final int DD_ContentDate = 1449;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionDate = 1450;

    /** DOCUMENT ME! */
    public static final int DD_SeriesDate = 1451;

    /** DOCUMENT ME! */
    public static final int DD_StudyDate = 1452;

    /** DOCUMENT ME! */
    public static final int DD_ColumnAngulation = 1453;

    /** DOCUMENT ME! */
    public static final int DD_ReferencedResultsSequence = 1454;

    /** DOCUMENT ME! */
    public static final int DD_SOPInstanceUID = 1455;

    /** DOCUMENT ME! */
    public static final int DD_SOPClassUID = 1456;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionGroupLength = 1457;

    /** DOCUMENT ME! */
    public static final int DD_InstanceCreatorUID = 1458;

    /** DOCUMENT ME! */
    public static final int DD_InstanceCreationTime = 1459;

    /** DOCUMENT ME! */
    public static final int DD_InstanceCreationDate = 1460;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionContextDescription = 1461;

    /** DOCUMENT ME! */
    public static final int DD_AcquisitionContextSequence = 1462;

    /** DOCUMENT ME! */
    public static final int DD_SpecimenIdentifier = 1463;

    /** DOCUMENT ME! */
    public static final int DD_SpecimenSequence = 1464;

    /** DOCUMENT ME! */
    public static final int DD_DataElementsSigned = 1465;

    /** DOCUMENT ME! */
    public static final int DD_ImageType = 1466;

    /** DOCUMENT ME! */
    public static final int DD_SpecificCharacterSet = 1467;

    /** DOCUMENT ME! */
    public static final int DD_CumulativeMetersetWeight = 1468;

    /** DOCUMENT ME! */
    public static final int DD_IdentifyingGroupLength = 1469;

    /** DOCUMENT ME! */
    public static final int DD_SourceToSurfaceDistance = 1470;

    /** DOCUMENT ME! */
    public static final int DD_SurfaceEntryPoint = 1471;

    /** DOCUMENT ME! */
    public static final int DD_IsocenterPosition = 1472;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfSeriesRelatedInstances = 1473;

    /** DOCUMENT ME! */
    public static final int DD_MACAlgorithm = 1474;

    /** DOCUMENT ME! */
    public static final int DD_TableTopLateralPosition = 1475;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfStudyRelatedInstances = 1476;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfStudyRelatedSeries = 1477;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfPatientRelatedInstances = 1478;

    /** DOCUMENT ME! */
    public static final int DD_MACCalculationTransferSyntaxUID = 1479;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfPatientRelatedSeries = 1480;

    /** DOCUMENT ME! */
    public static final int DD_TableTopLongitudinalPosition = 1481;

    /** DOCUMENT ME! */
    public static final int DD_NumberOfPatientRelatedStudies = 1482;

    /** DOCUMENT ME! */
    public static final int DD_TableTopVerticalPosition = 1483;

    /** DOCUMENT ME! */
    public static final int DD_RequestedContrastAgent = 1484;

    /** DOCUMENT ME! */
    public static final int DD_TableTopEccentricRotationDirection = 1485;

    /** DOCUMENT ME! */
    public static final int DD_TableTopEccentricAngle = 1486;

    /** DOCUMENT ME! */
    public static final int DD_TableTopEccentricAxisDistance = 1487;

    /** DOCUMENT ME! */
    public static final int DD_PatientInstitutionResidence = 1488;

    /** DOCUMENT ME! */
    public static final int DD_PatientSupportRotationDirection = 1489;

    /** DOCUMENT ME! */
    public static final int DD_PatientSupportAngle = 1490;

    /** DOCUMENT ME! */
    public static final int DD_BeamLimitingDeviceRotationDirection = 1491;

    /** DOCUMENT ME! */
    public static final int DD_BeamLimitingDeviceAngle = 1492;

    /** DOCUMENT ME! */
    public static final int DD_TemporalResolution = 1493;

    /** DOCUMENT ME! */
    public static final int DD_LASTSEARCHED = 1494;

    /** DOCUMENT ME! */
    private static Hashtable ddTypeIndexes = new Hashtable();

    /** DOCUMENT ME! */
    private static Hashtable typeCodeNames = new Hashtable();

    /** DOCUMENT ME! */
    private static boolean typeCodeNamesFilled = false;

    /** array of RTC Entries:. */
    private static RTCEntry[] rtcList = {
        new RTCEntry(0x0300a, 0x011f, TYPE_CS, "Gantry Rotation Direction", DD_GantryRotationDirection),
        new RTCEntry(0x0300a, 0x011e, TYPE_DS, "Gantry Angle", DD_GantryAngle),
        new RTCEntry(0x0300a, 0x011c, TYPE_DS, "Leaf Jaw Positions", DD_LeafJawPositions),
        new RTCEntry(0x04ffe, 0x01, TYPE_SQ, "MAC Parameters Sequence", DD_MACParametersSequence),
        new RTCEntry(0x0400, 0x05, TYPE_US, "MAC ID Number", DD_MACIDNumber),
        new RTCEntry(0x0300a, 0x011a, TYPE_SQ, "Beam Limiting Device Position Sequence",
                     DD_BeamLimitingDevicePositionSequence),
        new RTCEntry(0x032, 0x01064, TYPE_SQ, "Requested Procedure Code Sequence", DD_RequestedProcedureCodeSequence),
        new RTCEntry(0x0300a, 0x0118, TYPE_CS, "Wedge Position", DD_WedgePosition),
        new RTCEntry(0x032, 0x01060, TYPE_LO, "Requested Procedure Description", DD_RequestedProcedureDescription),
        new RTCEntry(0x018, 0x08151, TYPE_DS, "X-ray Tube Current in uA", DD_XrayTubeCurrentInuA),
        new RTCEntry(0x0300a, 0x0116, TYPE_SQ, "Wedge Position Sequence", DD_WedgePositionSequence),
        new RTCEntry(0x018, 0x08150, TYPE_DS, "Exposure Time In uS", DD_ExposureTimeInuS),
        new RTCEntry(0x0300a, 0x0115, TYPE_DS, "Dose Rate Set", DD_DoseRateSet),
        new RTCEntry(0x0300a, 0x0114, TYPE_DS, "Nominal Beam Energy", DD_NominalBeamEnergy),
        new RTCEntry(0x020, 0x0105, TYPE_IS, "Number of Temporal Positions", DD_NumberOfTemporalPositions),
        new RTCEntry(0x0300a, 0x0112, TYPE_IS, "Control Point Index", DD_ControlPointIndex),
        new RTCEntry(0x0300a, 0x0111, TYPE_SQ, "Control Point Sequence", DD_ControlPointSequence),
        new RTCEntry(0x0300a, 0x0110, TYPE_IS, "Number of Control Points", DD_NumberOfControlPoints),
        new RTCEntry(0x018, 0x07065, TYPE_DS, "Phototimer Setting", DD_PhototimerSetting),
        new RTCEntry(0x018, 0x07064, TYPE_CS, "Exposure Status", DD_ExposureStatus),
        new RTCEntry(0x020, 0x0100, TYPE_IS, "Temporal Position Identifier", DD_TemporalPositionIdentifier),
        new RTCEntry(0x018, 0x07062, TYPE_LT, "Exposure Control Mode Description", DD_ExposureControlModeDescription),
        new RTCEntry(0x018, 0x07060, TYPE_CS, "Exposure Control Mode", DD_ExposureControlMode),
        new RTCEntry(0x05000, 0x0114, TYPE_US, "Coordinate Step Value", DD_CoordinateStepValue),
        new RTCEntry(0x0300a, 0x010e, TYPE_DS, "Final Cumulative Meterset Weight", DD_FinalCumulativeMetersetWeight),
        new RTCEntry(0x05000, 0x0112, TYPE_US, "Coordinate Start Value", DD_CoordinateStartValue),
        new RTCEntry(0x0300a, 0x010c, TYPE_DS, "Cumulative Dose Reference Coefficient",
                     DD_CumulativeDoseReferenceCoefficient),
        new RTCEntry(0x00, 0x0120, TYPE_US, "Message ID Being Responded To", DD_MessageIDBeingRespondedTo),
        new RTCEntry(0x05000, 0x0110, TYPE_US, "Curve Data Descriptor", DD_CurveDataDescriptor),
        new RTCEntry(0x0300a, 0x010a, TYPE_LO, "Applicator Description", DD_ApplicatorDescription),
        new RTCEntry(0x032, 0x01055, TYPE_CS, "Study Component Status ID", DD_StudyComponentStatusID),
        new RTCEntry(0x0300a, 0x0109, TYPE_CS, "Applicator Type", DD_ApplicatorType),
        new RTCEntry(0x032, 0x01051, TYPE_TM, "Study Completion Time", DD_StudyCompletionTime),
        new RTCEntry(0x0300a, 0x0108, TYPE_SH, "Applicator ID", DD_ApplicatorID),
        new RTCEntry(0x032, 0x01050, TYPE_DA, "Study Completion Date", DD_StudyCompletionDate),
        new RTCEntry(0x0300a, 0x0107, TYPE_SQ, "Applicator Sequence", DD_ApplicatorSequence),
        new RTCEntry(0x0300a, 0x0106, TYPE_DS, "Block Data", DD_BlockData),
        new RTCEntry(0x0300a, 0x0104, TYPE_IS, "Block Number of Points", DD_BlockNumberOfPoints),
        new RTCEntry(0x02100, 0x0500, TYPE_SQ, "Referenced Print Job Sequence (in Pull Request N-Action)",
                     DD_ReferencedPrintJobSequencePull),
        new RTCEntry(0x03008, 0x092, TYPE_IS, "Referenced Calculated Dose Reference Number",
                     DD_ReferencedCalculatedDoseReferenceNumber),
        new RTCEntry(0x0300a, 0x0102, TYPE_DS, "Block Transmission", DD_BlockTransmission),
        new RTCEntry(0x03008, 0x090, TYPE_SQ, "Referenced Calculated Dose Reference Sequence",
                     DD_ReferencedCalculatedDoseReferenceSequence),
        new RTCEntry(0x0300a, 0x0100, TYPE_DS, "Block Thickness", DD_BlockThickness),
        new RTCEntry(0x018, 0x07054, TYPE_DS, "Filter Thickness Maximum", DD_FilterThicknessMaximum),
        new RTCEntry(0x018, 0x07052, TYPE_DS, "Filter Thickness Minimum", DD_FilterThicknessMinimum),
        new RTCEntry(0x018, 0x07050, TYPE_LT, "Filter Material", DD_FilterMaterial),
        new RTCEntry(0x010, 0x0102, TYPE_SQ, "Patient's Primary Language Modifier Code Sequence",
                     DD_PatientPrimaryLanguageModifierCodeSequence),
        new RTCEntry(0x010, 0x0101, TYPE_SQ, "Patient's Primary Language Code Sequence",
                     DD_PatientPrimaryLanguageCodeSequence),
        new RTCEntry(0x05000, 0x0106, TYPE_SH, "Curve Range", DD_CurveRange),
        new RTCEntry(0x02010, 0x060, TYPE_CS, "Magnification Type", DD_MagnificationType),
        new RTCEntry(0x05000, 0x0105, TYPE_US, "Maximum Coordinate Value", DD_MaximumCoordinateValue),
        new RTCEntry(0x05000, 0x0104, TYPE_US, "Minimum Coordinate Value", DD_MinimumCoordinateValue),
        new RTCEntry(0x05000, 0x0103, TYPE_US, "Data Value Representation", DD_DataValueRepresentation),
        new RTCEntry(0x018, 0x01405, TYPE_IS, "Relative X-ray Exposure", DD_RelativeXrayExposure),
        new RTCEntry(0x018, 0x01404, TYPE_US, "Exposures on Plate", DD_ExposuresOnPlate),
        new RTCEntry(0x018, 0x01403, TYPE_CS, "Cassette Size", DD_CassetteSize),
        new RTCEntry(0x00, 0x0110, TYPE_US, "Message ID", DD_MessageID),
        new RTCEntry(0x018, 0x01402, TYPE_CS, "Cassette Orientation", DD_CassetteOrientation),
        new RTCEntry(0x018, 0x01401, TYPE_LO, "Acquisition Device Processing Code", DD_AcquisitionDeviceProcessingCode),
        new RTCEntry(0x018, 0x01400, TYPE_LO, "Acquisition Device Processing Description",
                     DD_AcquisitionDeviceProcessingDescription),
        new RTCEntry(0x018, 0x0704c, TYPE_DS, "Grid Focal Distance", DD_GridFocalDistance),
        new RTCEntry(0x040, 0x050a, TYPE_LO, "Specimen Accession Number", DD_SpecimenAccessionNumber),
        new RTCEntry(0x028, 0x0301, TYPE_CS, "Burned In Annotation", DD_BurnedInAnnotation),
        new RTCEntry(0x028, 0x0300, TYPE_CS, "Quality Control Image", DD_QualityControlImage),
        new RTCEntry(0x032, 0x01041, TYPE_TM, "Study Arrival Time", DD_StudyArrivalTime),
        new RTCEntry(0x032, 0x01040, TYPE_DA, "Study Arrival Date", DD_StudyArrivalDate),
        new RTCEntry(0x02000, 0x06a, TYPE_CS, "Image Box Presentation LUT Flag", DD_ImageBoxPresentationLUTFlag),
        new RTCEntry(0x018, 0x07048, TYPE_DS, "Grid Period", DD_GridPeriod),
        new RTCEntry(0x03008, 0x082, TYPE_IS, "Referenced Measured Dose Reference Number",
                     DD_ReferencedMeasuredDoseReferenceNumber),
        new RTCEntry(0x018, 0x07046, TYPE_IS, "Grid Aspect Ratio", DD_GridAspectRatio),
        new RTCEntry(0x03008, 0x080, TYPE_SQ, "Referenced Measured Dose Reference Sequence",
                     DD_ReferencedMeasuredDoseReferenceSequence),
        new RTCEntry(0x018, 0x07044, TYPE_DS, "Grid Pitch", DD_GridPitch),
        new RTCEntry(0x02010, 0x054, TYPE_CS, "Default Printer Resolution ID", DD_DefaultPrinterResolutionID),
        new RTCEntry(0x018, 0x07042, TYPE_DS, "Grid Thickness", DD_GridThickness),
        new RTCEntry(0x02000, 0x069, TYPE_CS, "Presentation LUT Flag", DD_PresentationLUTFlag),
        new RTCEntry(0x018, 0x07041, TYPE_LT, "Grid Spacing Material", DD_GridSpacingMaterial),
        new RTCEntry(0x02010, 0x052, TYPE_CS, "Printer Resolution ID", DD_PrinterResolutionID),
        new RTCEntry(0x018, 0x07040, TYPE_LT, "Grid Absorbing Material", DD_GridAbsorbingMaterial),
        new RTCEntry(0x02000, 0x067, TYPE_CS, "Image Overlay Flag", DD_ImageOverlayFlag),
        new RTCEntry(0x02010, 0x050, TYPE_CS, "Film Size ID", DD_FilmSizeID),
        new RTCEntry(0x02000, 0x065, TYPE_CS, "Annotation Flag", DD_AnnotationFlag),
        new RTCEntry(0x02000, 0x063, TYPE_CS, "Collation Flag", DD_CollationFlag),
        new RTCEntry(0x02000, 0x062, TYPE_CS, "Color Image Printing Flag", DD_ColorImagePrintingFlag),
        new RTCEntry(0x02000, 0x061, TYPE_IS, "Maximum Memory Allocation", DD_MaximumMemoryAllocation),
        new RTCEntry(0x00, 0x0100, TYPE_US, "Command Field", DD_CommandField),
        new RTCEntry(0x02000, 0x060, TYPE_IS, "Memory Allocation", DD_MemoryAllocation),
        new RTCEntry(0x03008, 0x07a, TYPE_DS, "End Meterset", DD_EndMeterset),
        new RTCEntry(0x032, 0x01033, TYPE_LO, "Requesting Service", DD_RequestingService),
        new RTCEntry(0x032, 0x01032, TYPE_PN, "Requesting Physician", DD_RequestingPhysician),
        new RTCEntry(0x03008, 0x078, TYPE_DS, "Start Meterset", DD_StartMeterset),
        new RTCEntry(0x032, 0x01030, TYPE_LO, "Reason for Study", DD_ReasonForStudy),
        new RTCEntry(0x03008, 0x076, TYPE_DS, "Calculated Dose Reference Dose Value",
                     DD_CalculatedDoseReferenceDoseValue),
        new RTCEntry(0x03008, 0x074, TYPE_ST, "Calculated Dose Reference Description",
                     DD_CalculatedDoseReferenceDescription),
        new RTCEntry(0x03008, 0x072, TYPE_IS, "Calculated Dose Reference Number", DD_CalculatedDoseReferenceNumber),
        new RTCEntry(0x03008, 0x070, TYPE_SQ, "Calculated Dose Reference Sequence", DD_CalculatedDoseReferenceSequence),
        new RTCEntry(0x018, 0x07034, TYPE_CS, "Field of View Horizontal Flip", DD_FieldOfViewHorizontalFlip),
        new RTCEntry(0x018, 0x07032, TYPE_DS, "Field of View Rotation", DD_FieldOfViewRotation),
        new RTCEntry(0x018, 0x07030, TYPE_DS, "Field of View Origin", DD_FieldOfViewOrigin),
        new RTCEntry(0x02010, 0x040, TYPE_CS, "Film Orientation", DD_FilmOrientation),
        new RTCEntry(0x02020, 0x0111, TYPE_SQ, "Basic Color Image Sequence", DD_BasicColorImageSequence),
        new RTCEntry(0x02000, 0x050, TYPE_LO, "Film Session Label", DD_FilmSessionLabel),
        new RTCEntry(0x02020, 0x0110, TYPE_SQ, "Basic Grayscale Image Sequence", DD_BasicGrayscaleImageSequence),
        new RTCEntry(0x032, 0x01021, TYPE_AE, "Scheduled Study Location AE Title(s)", DD_ScheduledStudyLocationAETitle),
        new RTCEntry(0x032, 0x01020, TYPE_LO, "Scheduled Study Location", DD_ScheduledStudyLocation),
        new RTCEntry(0x05000, 0x02610, TYPE_US, "CurveReferenced Overlay Group", DD_CurveReferencedOverlayGroup),
        new RTCEntry(0x03008, 0x066, TYPE_ST, "Override Reason", DD_OverrideReason),
        new RTCEntry(0x03008, 0x064, TYPE_IS, "Measured Dose Reference Number", DD_MeasuredDoseReferenceNumber),
        new RTCEntry(0x018, 0x07028, TYPE_DS, "Detector Active Origin", DD_DetectorActiveOrigin),
        new RTCEntry(0x03008, 0x062, TYPE_AT, "Override Parameter Pointer", DD_OverrideParameterPointer),
        new RTCEntry(0x018, 0x07026, TYPE_DS, "Detector Active Dimensions", DD_DetectorActiveDimensions),
        new RTCEntry(0x03008, 0x060, TYPE_SQ, "Override Sequence", DD_OverrideSequence),
        new RTCEntry(0x018, 0x07024, TYPE_CS, "Detector Active Shape", DD_DetectorActiveShape),
        new RTCEntry(0x040, 0x08302, TYPE_DS, "Entrance Dose in mGy", DD_EntranceDoseInmGy),
        new RTCEntry(0x018, 0x07022, TYPE_DS, "Detector Element Spacing", DD_DetectorElementSpacing),
        new RTCEntry(0x018, 0x07020, TYPE_DS, "Detector Element Physical Size", DD_DetectorElementPhysicalSize),
        new RTCEntry(0x02010, 0x030, TYPE_CS, "Annotation Display Format ID", DD_AnnotationDisplayFormatID),
        new RTCEntry(0x02000, 0x040, TYPE_CS, "Film Destination", DD_FilmDestination),
        new RTCEntry(0x03008, 0x05a, TYPE_IS, "Number of Fractions Delivered", DD_NumberOfFractionsDelivered),
        new RTCEntry(0x018, 0x0701a, TYPE_DS, "Detector Binning", DD_DetectorBinning),
        new RTCEntry(0x0400, 0x0310, TYPE_OB, "CertifiedTimestamp", DD_CertifiedTimestamp),
        new RTCEntry(0x032, 0x01011, TYPE_TM, "Scheduled Study Stop Time", DD_ScheduledStudyStopTime),
        new RTCEntry(0x032, 0x01010, TYPE_DA, "Scheduled Study Stop Date", DD_ScheduledStudyStopDate),
        new RTCEntry(0x05000, 0x02600, TYPE_SQ, "CurveReferenced Overlay Sequence", DD_CurveReferencedOverlaySequence),
        new RTCEntry(0x03008, 0x056, TYPE_DA, "Most Recent Treatment Date", DD_MostRecentTreatmentDate),
        new RTCEntry(0x03008, 0x054, TYPE_DA, "First Treatment Date", DD_FirstTreatmentDate),
        new RTCEntry(0x03008, 0x052, TYPE_DS, "Cumulative Dose to Dose Reference", DD_CumulativeDoseToDoseReference),
        new RTCEntry(0x018, 0x07016, TYPE_DS, "Detector Activation Offset From Exposure",
                     DD_DetectorActivationOffsetFromExposure),
        new RTCEntry(0x03008, 0x050, TYPE_SQ, "Treatment Summary Calculated Dose Reference Sequence",
                     DD_TreatmentSummaryCalculatedDoseReferenceSequence),
        new RTCEntry(0x018, 0x07014, TYPE_DS, "Detector Active Time", DD_DetectorActiveTime),
        new RTCEntry(0x018, 0x07012, TYPE_DS, "Detector Time Since Last Exposure", DD_DetectorTimeSinceLastExposure),
        new RTCEntry(0x018, 0x07011, TYPE_IS, "Exposures on Detector Since Manufactured",
                     DD_ExposuresOnDetectorSinceManufactured),
        new RTCEntry(0x018, 0x07010, TYPE_IS, "Exposures on Detector Since Last Calibration",
                     DD_ExposuresOnDetectorSinceLastCalibration),
        new RTCEntry(0x02050, 0x0500, TYPE_SQ, "Referenced Presentation LUT Sequence",
                     DD_ReferencedPresentationLUTSequence),
        new RTCEntry(0x02000, 0x030, TYPE_CS, "Medium Type", DD_MediumType),
        new RTCEntry(0x018, 0x0700e, TYPE_TM, "Time of Last Detector Calibration", DD_TimeOfLastDetectorCalibration),
        new RTCEntry(0x0400, 0x0305, TYPE_CS, "CertifiedTimestampType", DD_CertifiedTimestampType),
        new RTCEntry(0x018, 0x01720, TYPE_IS, "Vertices of Polygonal Collimator", DD_VerticesOfPolygonalCollimator),
        new RTCEntry(0x018, 0x0700c, TYPE_DA, "Date of Last Detector Calibration ", DD_DateOfLastDetectorCalibration),
        new RTCEntry(0x018, 0x0700a, TYPE_SH, "Detector ID", DD_DetectorID),
        new RTCEntry(0x03008, 0x048, TYPE_DS, "Dose Rate Delivered", DD_DoseRateDelivered),
        new RTCEntry(0x032, 0x01001, TYPE_TM, "Scheduled Study Start Time", DD_ScheduledStudyStartTime),
        new RTCEntry(0x032, 0x01000, TYPE_DA, "Scheduled Study Start Date", DD_ScheduledStudyStartDate),
        new RTCEntry(0x03008, 0x044, TYPE_DS, "Delivered Meterset", DD_DeliveredMeterset),
        new RTCEntry(0x018, 0x07008, TYPE_LT, "Detector Mode", DD_DetectorMode),
        new RTCEntry(0x03008, 0x042, TYPE_DS, "Specified Meterset", DD_SpecifiedMeterset),
        new RTCEntry(0x018, 0x07006, TYPE_LT, "Detector Description", DD_DetectorDescription),
        new RTCEntry(0x03008, 0x040, TYPE_SQ, "Control Point Delivery Sequence", DD_ControlPointDeliverySequence),
        new RTCEntry(0x06000, 0x01303, TYPE_DS, "ROI Standard Deviation", DD_ROIStandardDeviation),
        new RTCEntry(0x018, 0x07005, TYPE_CS, "Detector Configuration", DD_DetectorConfiguration),
        new RTCEntry(0x06000, 0x01302, TYPE_DS, "ROI Mean", DD_ROIMean),
        new RTCEntry(0x018, 0x07004, TYPE_CS, "Detector Type", DD_DetectorType),
        new RTCEntry(0x06000, 0x01301, TYPE_IS, "ROI Area", DD_ROIArea),
        new RTCEntry(0x02010, 0x0376, TYPE_DS, "Printer Pixel Spacing", DD_PrinterPixelSpacing),
        new RTCEntry(0x018, 0x07001, TYPE_DS, "Detector Temperature", DD_DetectorTemperature),
        new RTCEntry(0x018, 0x07000, TYPE_CS, "Detector Conditions Nominal Flag", DD_DetectorConditionsNominalFlag),
        new RTCEntry(0x02010, 0x010, TYPE_ST, "Image Display Format", DD_ImageDisplayFormat),
        new RTCEntry(0x03008, 0x03b, TYPE_DS, "Delivered Treatment Time", DD_DeliveredTreatmentTime),
        new RTCEntry(0x02000, 0x020, TYPE_CS, "Print Priority", DD_PrintPriority),
        new RTCEntry(0x03008, 0x03a, TYPE_DS, "Specified Treatment  Time", DD_SpecifiedTreatmentTime),
        new RTCEntry(0x018, 0x01712, TYPE_IS, "Radius of Circular Collimator", DD_RadiusOfCircularCollimator),
        new RTCEntry(0x018, 0x01710, TYPE_IS, "Center of Circular Collimator", DD_CenterOfCircularCollimator),
        new RTCEntry(0x02000, 0x01e, TYPE_SQ, "Printer Configuration Sequence", DD_PrinterConfigurationSequence),
        new RTCEntry(0x03008, 0x037, TYPE_DS, "Delivered Secondary Meterset", DD_DeliveredSecondaryMeterset),
        new RTCEntry(0x03008, 0x036, TYPE_DS, "Delivered Primary Meterset", DD_DeliveredPrimaryMeterset),
        new RTCEntry(0x03008, 0x033, TYPE_DS, "Specified Secondary Meterset", DD_SpecifiedSecondaryMeterset),
        new RTCEntry(0x03008, 0x032, TYPE_DS, "Specified Primary Meterset", DD_SpecifiedPrimaryMeterset),
        new RTCEntry(0x03008, 0x030, TYPE_SQ, "Referenced Treatment Record Sequence",
                     DD_ReferencedTreatmentRecordSequence),
        new RTCEntry(0x02010, 0x00, TYPE_UL, "Film Box Group Length", DD_FilmBoxGroupLength),
        new RTCEntry(0x018, 0x01708, TYPE_IS, "Collimator Lower Horizontal Edge", DD_CollimatorLowerHorizontalEdge),
        new RTCEntry(0x018, 0x01706, TYPE_IS, "Collimator Upper Horizontal Edge", DD_CollimatorUpperHorizontalEdge),
        new RTCEntry(0x03008, 0x02c, TYPE_CS, "Treatment Verification Status", DD_TreatmentVerificationStatus),
        new RTCEntry(0x03008, 0x02b, TYPE_SH, "Treatment Termination Code", DD_TreatmentTerminationCode),
        new RTCEntry(0x02000, 0x010, TYPE_IS, "Number of Copies", DD_NumberOfCopies),
        new RTCEntry(0x018, 0x01704, TYPE_IS, "Collimator Right Vertical Edge", DD_CollimatorRightVerticalEdge),
        new RTCEntry(0x03008, 0x02a, TYPE_CS, "Treatment Termination Status", DD_TreatmentTerminationStatus),
        new RTCEntry(0x018, 0x01702, TYPE_IS, "Collimator Left Vertical Edge", DD_CollimatorLeftVerticalEdge),
        new RTCEntry(0x018, 0x01700, TYPE_CS, "Collimator Shape", DD_CollimatorShape),
        new RTCEntry(0x03008, 0x025, TYPE_TM, "Treatment Control Point Time", DD_TreatmentControlPointTime),
        new RTCEntry(0x03008, 0x024, TYPE_DA, "Treatment Control Point Date", DD_TreatmentControlPointDate),
        new RTCEntry(0x03008, 0x022, TYPE_IS, "Current Fraction  Number", DD_CurrentFractionNumber),
        new RTCEntry(0x04, 0x01600, TYPE_UL, "Number of References", DD_NumberOfReferences),
        new RTCEntry(0x03008, 0x020, TYPE_SQ, "Treatment Session Beam Sequence", DD_TreatmentSessionBeamSequence),
        new RTCEntry(0x02000, 0x00, TYPE_UL, "Film Session Group Length", DD_FilmSessionGroupLength),
        new RTCEntry(0x03008, 0x016, TYPE_DS, "Measured Dose Value", DD_MeasuredDoseValue),
        new RTCEntry(0x03008, 0x014, TYPE_CS, "Measured Dose Type", DD_MeasuredDoseType),
        new RTCEntry(0x03008, 0x012, TYPE_ST, "Measured Dose Description", DD_MeasuredDoseDescription),
        new RTCEntry(0x03008, 0x010, TYPE_SQ, "Measured Dose Reference Sequence", DD_MeasuredDoseReferenceSequence),
        new RTCEntry(0x0300a, 0x0fe, TYPE_LO, "Block Name", DD_BlockName),
        new RTCEntry(0x0300a, 0x0fc, TYPE_IS, "Block Number", DD_BlockNumber),
        new RTCEntry(0x0300a, 0x0fa, TYPE_CS, "Block Divergence", DD_BlockDivergence),
        new RTCEntry(0x0300a, 0x0f8, TYPE_CS, "Block Type", DD_BlockType),
        new RTCEntry(0x0300a, 0x0f6, TYPE_DS, "Source to Block Tray Distance", DD_SourceToBlockTrayDistance),
        new RTCEntry(0x0300a, 0x0f5, TYPE_SH, "Block Tray ID", DD_BlockTrayID),
        new RTCEntry(0x0300a, 0x0f4, TYPE_SQ, "Block Sequence", DD_BlockSequence),
        new RTCEntry(0x0300a, 0x0f2, TYPE_DS, "Total Block Tray Factor", DD_TotalBlockTrayFactor),
        new RTCEntry(0x0300a, 0x0f0, TYPE_IS, "Number of Blocks", DD_NumberOfBlocks),
        new RTCEntry(0x054, 0x090, TYPE_US, "Angular View Vector", DD_AngularViewVector),
        new RTCEntry(0x0300a, 0x0ee, TYPE_CS, "CompensatorType", DD_CompensatorType),
        new RTCEntry(0x0300a, 0x0ed, TYPE_IS, "Number of Boli", DD_NumberOfBoli),
        new RTCEntry(0x0300a, 0x0ec, TYPE_DS, "Compensator Thickness Data", DD_CompensatorThicknessData),
        new RTCEntry(0x0300a, 0x0eb, TYPE_DS, "Compensator Transmission Data", DD_CompensatorTransmissionData),
        new RTCEntry(0x0300a, 0x0ea, TYPE_DS, "Compensator Position", DD_CompensatorPosition),
        new RTCEntry(0x0300a, 0x0e9, TYPE_DS, "Compensator Pixel Spacing", DD_CompensatorPixelSpacing),
        new RTCEntry(0x0300a, 0x0e8, TYPE_IS, "Compensator Columns", DD_CompensatorColumns),
        new RTCEntry(0x0300a, 0x0e7, TYPE_IS, "Compensator Rows", DD_CompensatorRows),
        new RTCEntry(0x0300a, 0x0e6, TYPE_DS, "Source to Compensator Tray Distance",
                     DD_SourceToCompensatorTrayDistance),
        new RTCEntry(0x0300a, 0x0e5, TYPE_SH, "Compensator ID", DD_CompensatorID),
        new RTCEntry(0x0300a, 0x0e4, TYPE_IS, "Compensator Number", DD_CompensatorNumber),
        new RTCEntry(0x0300a, 0x0e3, TYPE_SQ, "Compensator Sequence", DD_CompensatorSequence),
        new RTCEntry(0x0300a, 0x0e2, TYPE_DS, "Total Compensator Tray Factor", DD_TotalCompensatorTrayFactor),
        new RTCEntry(0x0300a, 0x0e1, TYPE_SH, "Material ID", DD_MaterialID),
        new RTCEntry(0x0300a, 0x0e0, TYPE_IS, "Number of Compensators", DD_NumberOfCompensators),
        new RTCEntry(0x054, 0x081, TYPE_US, "Number of Slices", DD_NumberOfSlices),
        new RTCEntry(0x054, 0x080, TYPE_US, "Slice Vector", DD_SliceVector),
        new RTCEntry(0x0300a, 0x0da, TYPE_DS, "Source to Wedge Tray Distance", DD_SourceToWedgeTrayDistance),
        new RTCEntry(0x0300a, 0x0d8, TYPE_DS, "Wedge Orientation", DD_WedgeOrientation),
        new RTCEntry(0x0300a, 0x0d6, TYPE_DS, "Wedge Factor", DD_WedgeFactor),
        new RTCEntry(0x0300a, 0x0d5, TYPE_IS, "Wedge Angle", DD_WedgeAngle),
        new RTCEntry(0x0300a, 0x0d4, TYPE_SH, "Wedge ID", DD_WedgeID),
        new RTCEntry(0x0300a, 0x0d3, TYPE_CS, "Wedge Type", DD_WedgeType),
        new RTCEntry(0x0300a, 0x0d2, TYPE_IS, "Wedge Number", DD_WedgeNumber),
        new RTCEntry(0x054, 0x073, TYPE_DS, "Time Slot Time", DD_TimeSlotTime),
        new RTCEntry(0x0300a, 0x0d1, TYPE_SQ, "Wedge Sequence", DD_WedgeSequence),
        new RTCEntry(0x054, 0x072, TYPE_SQ, "Time Slot Information Sequence", DD_TimeSlotInformationSequence),
        new RTCEntry(0x0300a, 0x0d0, TYPE_IS, "Number of Wedges", DD_NumberOfWedges),
        new RTCEntry(0x054, 0x071, TYPE_US, "Number of Time Slots", DD_NumberOfTimeSlots),
        new RTCEntry(0x054, 0x070, TYPE_US, "Time Slot Vector", DD_TimeSlotVector),
        new RTCEntry(0x0300a, 0x0ce, TYPE_CS, "Treatment Delivery Type", DD_TreatmentDeliveryType),
        new RTCEntry(0x0300a, 0x0cc, TYPE_LO, "Imaging Device Specific Acquisition Parameters",
                     DD_ImagingDeviceSpecificAcquisitionParameters),
        new RTCEntry(0x0300a, 0x0ca, TYPE_SQ, "Planned Verification Image Sequence",
                     DD_PlannedVerificationImageSequence),
        new RTCEntry(0x0300a, 0x0c8, TYPE_IS, "Reference Image Number", DD_ReferenceImageNumber),
        new RTCEntry(0x0300a, 0x0c7, TYPE_CS, "High-Dose Technique Type", DD_HighDoseTechniqueType),
        new RTCEntry(0x0300a, 0x0c6, TYPE_CS, "Radiation Type", DD_RadiationType),
        new RTCEntry(0x0300a, 0x0c4, TYPE_CS, "Beam Type", DD_BeamType),
        new RTCEntry(0x0300a, 0x0c3, TYPE_ST, "Beam Description", DD_BeamDescription),
        new RTCEntry(0x0300a, 0x0c2, TYPE_LO, "Beam Name", DD_BeamName),
        new RTCEntry(0x054, 0x063, TYPE_SQ, "Data Information Sequence", DD_DataInformationSequence),
        new RTCEntry(0x054, 0x062, TYPE_SQ, "Gated Information Sequence", DD_GatedInformationSequence),
        new RTCEntry(0x0300a, 0x0c0, TYPE_IS, "Beam Number", DD_BeamNumber),
        new RTCEntry(0x054, 0x061, TYPE_US, "Number of R-R Intervals", DD_NumberOfRRIntervals),
        new RTCEntry(0x054, 0x060, TYPE_US, "R-R Interval Vector", DD_RRIntervalVector),
        new RTCEntry(0x0300a, 0x0be, TYPE_DS, "Leaf Position Boundaries", DD_LeafPositionBoundaries),
        new RTCEntry(0x0300a, 0x0bc, TYPE_IS, "Number of Leaf/Jaw Pairs", DD_NumberOfLeafJawPairs),
        new RTCEntry(0x04008, 0x011a, TYPE_LO, "Distribution Address", DD_DistributionAddress),
        new RTCEntry(0x0300a, 0x0ba, TYPE_DS, "Source to Beam Limiting Device Distance",
                     DD_SourceToBeamLimitingDeviceDistance),
        new RTCEntry(0x0300a, 0x0b8, TYPE_CS, "RT Beam Limiting Device Type", DD_RTBeamLimitingDeviceType),
        new RTCEntry(0x0300a, 0x0b6, TYPE_SQ, "Beam Limiting Device Sequence", DD_BeamLimitingDeviceSequence),
        new RTCEntry(0x0300a, 0x0b4, TYPE_DS, "Source-Axis Distance", DD_SourceAxisDistance),
        new RTCEntry(0x0300a, 0x0b3, TYPE_CS, "Primary Dosimeter Unit", DD_PrimaryDosimeterUnit),
        new RTCEntry(0x0300a, 0x0b2, TYPE_SH, "Treatment Machine Name ", DD_TreatmentMachineName),
        new RTCEntry(0x054, 0x053, TYPE_US, "Number of Frames In Rotation", DD_NumberOfFramesInRotation),
        new RTCEntry(0x054, 0x052, TYPE_SQ, "Rotation Information Sequence", DD_RotationInformationSequence),
        new RTCEntry(0x0300a, 0x0b0, TYPE_SQ, "Beam Sequence", DD_BeamSequence),
        new RTCEntry(0x054, 0x051, TYPE_US, "Number of Rotations", DD_NumberOfRotations),
        new RTCEntry(0x054, 0x050, TYPE_US, "Rotation Vector", DD_RotationVector),
        new RTCEntry(0x04008, 0x010c, TYPE_PN, "Interpretation Author", DD_InterpretationAuthor),
        new RTCEntry(0x04008, 0x010b, TYPE_ST, "Interpretation Text", DD_InterpretationText),
        new RTCEntry(0x04008, 0x010a, TYPE_PN, "Interpretation Transcriber", DD_InterpretationTranscriber),
        new RTCEntry(0x00, 0x0700, TYPE_US, "Priority", DD_Priority),
        new RTCEntry(0x04008, 0x0119, TYPE_PN, "Distribution Name", DD_DistributionName),
        new RTCEntry(0x088, 0x0200, TYPE_SQ, "Icon Image Sequence", DD_IconImageSequence),
        new RTCEntry(0x04008, 0x0118, TYPE_SQ, "Results Distribution List Sequence",
                     DD_ResultsDistributionListSequence),
        new RTCEntry(0x04008, 0x0117, TYPE_SQ, "Interpretation Diagnosis Code Sequence",
                     DD_InterpretationDiagnosisCodeSequence),
        new RTCEntry(0x018, 0x05104, TYPE_SQ, "Projection Eponymous Name Code Sequence",
                     DD_ProjectionEponymousNameCodeSequence),
        new RTCEntry(0x04008, 0x0115, TYPE_LT, "Interpretation Diagnosis Description",
                     DD_InterpretationDiagnosisDescription),
        new RTCEntry(0x04008, 0x0114, TYPE_PN, "Physician Approving Interpretation",
                     DD_PhysicianApprovingInterpretation),
        new RTCEntry(0x0300a, 0x0a4, TYPE_DS, "Brachy Application Setup Dose", DD_BrachyApplicationSetupDose),
        new RTCEntry(0x04008, 0x0113, TYPE_TM, "Interpretation Approval Time", DD_InterpretationApprovalTime),
        new RTCEntry(0x018, 0x05101, TYPE_CS, "View Position", DD_ViewPosition),
        new RTCEntry(0x04008, 0x0112, TYPE_DA, "Interpretation Approval Date", DD_InterpretationApprovalDate),
        new RTCEntry(0x0300a, 0x0a2, TYPE_DS, "Brachy Application Setup Dose Specification Point",
                     DD_BrachyApplicationSetupDoseSpecificationPoint),
        new RTCEntry(0x018, 0x05100, TYPE_CS, "Patient Position", DD_PatientPosition),
        new RTCEntry(0x04008, 0x0111, TYPE_SQ, "Interpretation Approver Sequence", DD_InterpretationApproverSequence),
        new RTCEntry(0x0300a, 0x0a0, TYPE_IS, "Number of Brachy Application Setups",
                     DD_NumberOfBrachyApplicationSetups),
        new RTCEntry(0x04008, 0x0109, TYPE_TM, "Interpretation Transcription Time", DD_InterpretationTranscriptionTime),
        new RTCEntry(0x04008, 0x0108, TYPE_DA, "Interpretation Transcription Date", DD_InterpretationTranscriptionDate),
        new RTCEntry(0x054, 0x038, TYPE_IS, "Pause Between Frames", DD_PauseBetweenFrames),
        new RTCEntry(0x054, 0x036, TYPE_IS, "Phase Delay", DD_PhaseDelay),
        new RTCEntry(0x04008, 0x0103, TYPE_LO, "Reference to Recorded Sound", DD_ReferenceToRecordedSound),
        new RTCEntry(0x04008, 0x0102, TYPE_PN, "Interpretation Recorder", DD_InterpretationRecorder),
        new RTCEntry(0x054, 0x033, TYPE_US, "Number of Frames In Phase", DD_NumberOfFramesInPhase),
        new RTCEntry(0x04008, 0x0101, TYPE_TM, "Interpretation Recorded Time", DD_InterpretationRecordedTime),
        new RTCEntry(0x054, 0x032, TYPE_SQ, "Phase Information Sequence", DD_PhaseInformationSequence),
        new RTCEntry(0x04008, 0x0100, TYPE_DA, "Interpretation Recorded Date", DD_InterpretationRecordedDate),
        new RTCEntry(0x054, 0x031, TYPE_US, "Number of Phases", DD_NumberOfPhases),
        new RTCEntry(0x054, 0x030, TYPE_US, "Phase Vector", DD_PhaseVector),
        new RTCEntry(0x028, 0x01090, TYPE_CS, "Recommended Viewing Mode", DD_RecommendedViewingMode),
        new RTCEntry(0x054, 0x022, TYPE_SQ, "Detector Information Sequence", DD_DetectorInformationSequence),
        new RTCEntry(0x054, 0x021, TYPE_US, "Number of Detectors", DD_NumberOfDetectors),
        new RTCEntry(0x054, 0x020, TYPE_US, "Detector Vector", DD_DetectorVector),
        new RTCEntry(0x040, 0x02017, TYPE_LO, "Filler Order Number of Imaging Service Request",
                     DD_FillerOrderNumberOfImagingServiceRequest),
        new RTCEntry(0x040, 0x02016, TYPE_LO, "Placer Order Number of Imaging Service Request",
                     DD_PlacerOrderNumberOfImagingServiceRequest),
        new RTCEntry(0x018, 0x01094, TYPE_IS, "Trigger Window", DD_TriggerWindow),
        new RTCEntry(0x040, 0x02010, TYPE_SH, "Order Callback Phone Number", DD_OrderCallbackPhoneNumber),
        new RTCEntry(0x018, 0x01090, TYPE_IS, "Cardiac Number of Images", DD_CardiacNumberOfImages),
        new RTCEntry(0x054, 0x01105, TYPE_LO, "Scatter Correction Method", DD_ScatterCorrectionMethod),
        new RTCEntry(0x054, 0x01104, TYPE_LO, "Detector Lines of Response Used", DD_DetectorLinesOfResponseUsed),
        new RTCEntry(0x054, 0x01103, TYPE_LO, "Reconstruction Method", DD_ReconstructionMethod),
        new RTCEntry(0x054, 0x01102, TYPE_CS, "Decay Correction", DD_DecayCorrection),
        new RTCEntry(0x054, 0x01101, TYPE_LO, "Attenuation Correction Method", DD_AttenuationCorrectionMethod),
        new RTCEntry(0x054, 0x01100, TYPE_CS, "Randoms Correction Method", DD_RandomsCorrectionMethod),
        new RTCEntry(0x054, 0x018, TYPE_SH, "Energy Window Name", DD_EnergyWindowName),
        new RTCEntry(0x054, 0x017, TYPE_IS, "Residual Syringe Counts", DD_ResidualSyringeCounts),
        new RTCEntry(0x054, 0x016, TYPE_SQ, "Radiopharmaceutical Information Sequence",
                     DD_RadiopharmaceuticalInformationSequence),
        new RTCEntry(0x054, 0x015, TYPE_DS, "Energy Window Upper Limit", DD_EnergyWindowUpperLimit),
        new RTCEntry(0x054, 0x014, TYPE_DS, "Energy Window Lower Limit", DD_EnergyWindowLowerLimit),
        new RTCEntry(0x054, 0x013, TYPE_SQ, "Energy Window Range Sequence", DD_EnergyWindowRangeSequence),
        new RTCEntry(0x054, 0x012, TYPE_SQ, "Energy Window Information Sequence", DD_EnergyWindowInformationSequence),
        new RTCEntry(0x054, 0x011, TYPE_US, "Number of Energy Windows", DD_NumberOfEnergyWindows),
        new RTCEntry(0x054, 0x010, TYPE_US, "Energy Window Vector", DD_EnergyWindowVector),
        new RTCEntry(0x040, 0x02009, TYPE_SH, "Order Enterer Location", DD_OrderEntererLocation),
        new RTCEntry(0x040, 0x02008, TYPE_PN, "Order Entered By", DD_OrderEnteredBy),
        new RTCEntry(0x018, 0x01088, TYPE_IS, "Heart Rate", DD_HeartRate),
        new RTCEntry(0x040, 0x02005, TYPE_TM, "Issue Time of Imaging Service Request",
                     DD_IssueTimeOfImagingServiceRequest),
        new RTCEntry(0x040, 0x02004, TYPE_DA, "Issue Date of Imaging Service Request",
                     DD_IssueDateOfImagingServiceRequest),
        new RTCEntry(0x018, 0x01086, TYPE_IS, "Skip Beats", DD_SkipBeats),
        new RTCEntry(0x02, 0x0102, TYPE_OB, "Private Information", DD_PrivateInformation),
        new RTCEntry(0x018, 0x01085, TYPE_LO, "PVC Rejection", DD_PVCRejection),
        new RTCEntry(0x018, 0x01084, TYPE_IS, "Intervals Rejected", DD_IntervalsRejected),
        new RTCEntry(0x02, 0x0100, TYPE_UI, "Private Information Creator UID", DD_PrivateInformationCreatorUID),
        new RTCEntry(0x040, 0x02001, TYPE_LO, "Reason For Imaging Service Request", DD_ReasonForImagingServiceRequest),
        new RTCEntry(0x018, 0x01083, TYPE_IS, "Intervals Acquired", DD_IntervalsAcquired),
        new RTCEntry(0x018, 0x01082, TYPE_IS, "High R-R Value", DD_HighRRValue),
        new RTCEntry(0x02020, 0x0a2, TYPE_CS, "Decimate/Crop Result", DD_DecimateCropResult),
        new RTCEntry(0x018, 0x01081, TYPE_IS, "Low R-R Value", DD_LowRRValue),
        new RTCEntry(0x018, 0x01080, TYPE_CS, "Beat Rejection Flag", DD_BeatRejectionFlag),
        new RTCEntry(0x02020, 0x0a0, TYPE_CS, "Requested Image Size Flag", DD_RequestedImageSizeFlag),
        new RTCEntry(0x08, 0x01090, TYPE_LO, "Manufacturer's Model Name", DD_ManufacturerModelName),
        new RTCEntry(0x038, 0x050, TYPE_LO, "Special Needs", DD_SpecialNeeds),
        new RTCEntry(0x02100, 0x0170, TYPE_IS, "Number Of Films", DD_NumberOfFilms),
        new RTCEntry(0x054, 0x00, TYPE_UL, "Nuclear Acquisition Group Length", DD_NuclearAcquisitionGroupLength),
        new RTCEntry(0x018, 0x01077, TYPE_DS, "Radiopharmaceutical Specific Activity",
                     DD_RadiopharmaceuticalSpecificActivity),
        new RTCEntry(0x018, 0x01076, TYPE_DS, "Radionuclide Positron Fraction", DD_RadionuclidePositronFraction),
        new RTCEntry(0x018, 0x01075, TYPE_DS, "Radionuclide Half Life", DD_RadionuclideHalfLife),
        new RTCEntry(0x018, 0x01074, TYPE_DS, "Radionuclide Total Dose", DD_RadionuclideTotalDose),
        new RTCEntry(0x018, 0x01073, TYPE_TM, "Radiopharmaceutical Stop Time", DD_RadiopharmaceuticalStopTime),
        new RTCEntry(0x018, 0x01072, TYPE_TM, "Radiopharmaceutical Start Time", DD_RadiopharmaceuticalStartTime),
        new RTCEntry(0x018, 0x01071, TYPE_DS, "Radiopharmaceutical Volume", DD_RadiopharmaceuticalVolume),
        new RTCEntry(0x018, 0x01070, TYPE_LO, "Radiopharmaceutical Route", DD_RadiopharmaceuticalRoute),
        new RTCEntry(0x08, 0x01084, TYPE_SQ, "Admitting Diagnoses Code Sequence", DD_AdmittingDiagnosesCodeSequence),
        new RTCEntry(0x08, 0x01080, TYPE_LO, "Admitting Diagnoses Description", DD_AdmittingDiagnosesDescription),
        new RTCEntry(0x038, 0x044, TYPE_SQ, "Discharge Diagnosis Code Sequence", DD_DischargeDiagnosisCodeSequence),
        new RTCEntry(0x038, 0x040, TYPE_LO, "Discharge Diagnosis Description", DD_DischargeDiagnosisDescription),
        new RTCEntry(0x02100, 0x0160, TYPE_SH, "OwnerID", DD_OwnerID),
        new RTCEntry(0x028, 0x01055, TYPE_LO, "Window Center & Width Explanation", DD_WindowCenterWidthExplanation),
        new RTCEntry(0x028, 0x01054, TYPE_LO, "Rescale Type", DD_RescaleType),
        new RTCEntry(0x028, 0x01053, TYPE_DS, "Rescale Slope", DD_RescaleSlope),
        new RTCEntry(0x028, 0x01052, TYPE_DS, "Rescale Intercept", DD_RescaleIntercept),
        new RTCEntry(0x018, 0x01066, TYPE_DS, "Frame Delay", DD_FrameDelay),
        new RTCEntry(0x028, 0x01051, TYPE_DS, "Window Width", DD_WindowWidth),
        new RTCEntry(0x018, 0x01065, TYPE_DS, "Frame Time Vector", DD_FrameTimeVector),
        new RTCEntry(0x028, 0x01050, TYPE_DS, "Window Center", DD_WindowCenter),
        new RTCEntry(0x018, 0x01064, TYPE_LO, "Framing Type", DD_FramingType),
        new RTCEntry(0x018, 0x01063, TYPE_DS, "Frame Time", DD_FrameTime),
        new RTCEntry(0x018, 0x01062, TYPE_IS, "Nominal Interval", DD_NominalInterval),
        new RTCEntry(0x018, 0x01061, TYPE_LO, "Trigger Source or Type", DD_TriggerSourceOrType),
        new RTCEntry(0x018, 0x01060, TYPE_DS, "Trigger Time", DD_TriggerTime),
        new RTCEntry(0x08, 0x01070, TYPE_PN, "Operator's Name", DD_OperatorName),
        new RTCEntry(0x038, 0x032, TYPE_TM, "Discharge Time", DD_DischargeTime),
        new RTCEntry(0x038, 0x030, TYPE_DA, "Discharge Date", DD_DischargeDate),
        new RTCEntry(0x038, 0x01e, TYPE_LO, "Scheduled Patient Institution Residence",
                     DD_ScheduledPatientInstitutionResidence),
        new RTCEntry(0x038, 0x01d, TYPE_TM, "Scheduled Discharge Time", DD_ScheduledDischargeTime),
        new RTCEntry(0x038, 0x01c, TYPE_DA, "Scheduled Discharge Date", DD_ScheduledDischargeDate),
        new RTCEntry(0x038, 0x01b, TYPE_TM, "Scheduled Admission Time", DD_ScheduledAdmissionTime),
        new RTCEntry(0x038, 0x01a, TYPE_DA, "Scheduled Admission Date", DD_ScheduledAdmissionDate),
        new RTCEntry(0x028, 0x01041, TYPE_SS, "Pixel Intensity Relationship Sign", DD_PixelIntensityRelationshipSign),
        new RTCEntry(0x028, 0x01040, TYPE_CS, "Pixel Intensity Relationship", DD_PixelIntensityRelationship),
        new RTCEntry(0x018, 0x01050, TYPE_DS, "Spatial Resolution", DD_SpatialResolution),
        new RTCEntry(0x08, 0x01060, TYPE_PN, "Name of Physician(s) Reading Study", DD_PhysicianReadingStudy),
        new RTCEntry(0x038, 0x021, TYPE_TM, "Admitting Time", DD_AdmittingTime),
        new RTCEntry(0x038, 0x020, TYPE_DA, "Admitting Date", DD_AdmittingDate),
        new RTCEntry(0x02100, 0x0140, TYPE_AE, "Destination AE", DD_DestinationAE),
        new RTCEntry(0x08, 0x02144, TYPE_IS, "Recommended Display Frame Rate", DD_RecommendedDisplayFrameRate),
        new RTCEntry(0x018, 0x01049, TYPE_DS, "Contrast/Bolus Ingredient Concentration",
                     DD_ContrastBolusIngredientConcentration),
        new RTCEntry(0x08, 0x02143, TYPE_IS, "Stop Trim", DD_StopTrim),
        new RTCEntry(0x018, 0x01048, TYPE_CS, "Contrast/Bolus Ingredient", DD_ContrastBolusIngredient),
        new RTCEntry(0x08, 0x02142, TYPE_IS, "Start Trim", DD_StartTrim),
        new RTCEntry(0x018, 0x01047, TYPE_DS, "Contrast Flow Duration", DD_ContrastFlowDuration),
        new RTCEntry(0x018, 0x01046, TYPE_DS, "Contrast Flow Rate", DD_ContrastFlowRate),
        new RTCEntry(0x018, 0x01045, TYPE_IS, "Syringe Counts", DD_SyringeCounts),
        new RTCEntry(0x05000, 0x03000, TYPE_OW, "Curve Data", DD_CurveData),
        new RTCEntry(0x018, 0x01044, TYPE_DS, "Contrast/Bolus Total Dose", DD_ContrastBolusTotalDose),
        new RTCEntry(0x018, 0x01043, TYPE_TM, "Contrast/Bolus Stop Time", DD_ContrastBolusStopTime),
        new RTCEntry(0x018, 0x01042, TYPE_TM, "Contrast/Bolus Start Time", DD_ContrastBolusStartTime),
        new RTCEntry(0x018, 0x01041, TYPE_DS, "Contrast/Bolus Volume", DD_ContrastBolusVolume),
        new RTCEntry(0x018, 0x01040, TYPE_LO, "Contrast/Bolus Route", DD_ContrastBolusRoute),
        new RTCEntry(0x08, 0x01050, TYPE_PN, "Performing Physician's Name", DD_PerformingPhysicianName),
        new RTCEntry(0x038, 0x016, TYPE_LO, "Route of Admissions", DD_RouteOfAdmissions),
        new RTCEntry(0x08, 0x0212a, TYPE_IS, "Number of Views in Stage", DD_NumberOfViewsInStage),
        new RTCEntry(0x038, 0x011, TYPE_LO, "Issuer of Admission ID", DD_IssuerOfAdmissionID),
        new RTCEntry(0x038, 0x010, TYPE_LO, "Admission ID", DD_AdmissionID),
        new RTCEntry(0x028, 0x02112, TYPE_DS, "Lossy Image Compression Ratio", DD_LossyImageCompressionRatio),
        new RTCEntry(0x028, 0x02110, TYPE_CS, "Lossy Image Compression", DD_LossyImageCompression),
        new RTCEntry(0x08, 0x02132, TYPE_LO, "Event Timer Name(s)", DD_EventTimerName),
        new RTCEntry(0x08, 0x02130, TYPE_DS, "Event Elapsed Time(s)", DD_EventElapsedTime),
        new RTCEntry(0x08, 0x01048, TYPE_PN, "Physician of Record", DD_PhysicianOfRecord),
        new RTCEntry(0x018, 0x01030, TYPE_LO, "Protocol Name", DD_ProtocolName),
        new RTCEntry(0x038, 0x08, TYPE_CS, "Visit Status ID", DD_VisitStatusID),
        new RTCEntry(0x08, 0x01040, TYPE_LO, "Institutional Department Name", DD_InstitutionalDepartmentName),
        new RTCEntry(0x038, 0x04, TYPE_SQ, "Referenced Patient Alias Sequence", DD_ReferencedPatientAliasSequence),
        new RTCEntry(0x054, 0x01401, TYPE_CS, "Dead Time Correction Flag", DD_DeadTimeCorrectionFlag),
        new RTCEntry(0x054, 0x01400, TYPE_CS, "Counts Included", DD_CountsIncluded),
        new RTCEntry(0x0300a, 0x086, TYPE_DS, "Beam Meterset", DD_BeamMeterset),
        new RTCEntry(0x038, 0x00, TYPE_UL, "Visit Group Length", DD_VisitGroupLength),
        new RTCEntry(0x0300a, 0x084, TYPE_DS, "Beam Dose", DD_BeamDose),
        new RTCEntry(0x08, 0x0103e, TYPE_LO, "Series Description", DD_SeriesDescription),
        new RTCEntry(0x0300a, 0x082, TYPE_DS, "Beam Dose Specification Point", DD_BeamDoseSpecificationPoint),
        new RTCEntry(0x08, 0x02129, TYPE_IS, "Number of Event Timers", DD_NumberOfEventTimers),
        new RTCEntry(0x08, 0x02128, TYPE_IS, "View Number", DD_ViewNumber),
        new RTCEntry(0x0300a, 0x080, TYPE_IS, "Number of Beams", DD_NumberOfBeams),
        new RTCEntry(0x08, 0x02127, TYPE_SH, "View Name", DD_ViewName),
        new RTCEntry(0x08, 0x02124, TYPE_IS, "Number of Stages", DD_NumberOfStages),
        new RTCEntry(0x08, 0x02122, TYPE_IS, "Stage Number", DD_StageNumber),
        new RTCEntry(0x08, 0x02120, TYPE_SH, "Stage Name", DD_StageName),
        new RTCEntry(0x018, 0x01023, TYPE_LO, "Digital Image Format Acquired", DD_DigitalImageFormatAcquired),
        new RTCEntry(0x018, 0x01022, TYPE_SH, "Video Image Format Acquired", DD_VideoImageFormatAcquired),
        new RTCEntry(0x0300a, 0x07b, TYPE_LT, "Fraction Pattern", DD_FractionPattern),
        new RTCEntry(0x018, 0x01020, TYPE_LO, "Software Version(s)", DD_SoftwareVersion),
        new RTCEntry(0x0300a, 0x07a, TYPE_IS, "Repeat Fraction Cycle Length", DD_RepeatFractionCycleLength),
        new RTCEntry(0x08, 0x01032, TYPE_SQ, "Procedure Code Sequence", DD_ProcedureCodeSequence),
        new RTCEntry(0x08, 0x01030, TYPE_LO, "Study Description", DD_StudyDescription),
        new RTCEntry(0x0300a, 0x079, TYPE_IS, "Number of Fraction Pattern Digits Per Day",
                     DD_NumberOfFractionPatternDigitsPerDay),
        new RTCEntry(0x0300a, 0x078, TYPE_IS, "Number of Fractions Planned", DD_NumberOfFractionsPlanned),
        new RTCEntry(0x054, 0x0308, TYPE_US, "Energy Window Number", DD_EnergyWindowNumber),
        new RTCEntry(0x018, 0x0101b, TYPE_LO, "Hardcopy Device Manufacturer's Model Name",
                     DD_HardcopyDeviceManufacturerModelName),
        new RTCEntry(0x018, 0x0101a, TYPE_LO, "Hardcopy Device Software Version", DD_HardcopyDeviceSoftwareVersion),
        new RTCEntry(0x054, 0x0306, TYPE_SQ, "Calibration Data Sequence", DD_CalibrationDataSequence),
        new RTCEntry(0x0300a, 0x071, TYPE_IS, "Fraction Group Number", DD_FractionGroupNumber),
        new RTCEntry(0x054, 0x0304, TYPE_SQ, "Radiopharmaceutical Code Sequence", DD_RadiopharmaceuticalCodeSequence),
        new RTCEntry(0x0300a, 0x070, TYPE_SQ, "Fraction Group Sequence", DD_FractionGroupSequence),
        new RTCEntry(0x020, 0x062, TYPE_CS, "Image Laterality", DD_ImageLaterality),
        new RTCEntry(0x054, 0x0302, TYPE_SQ, "Administration Route Code Sequence", DD_AdministrationRouteCodeSequence),
        new RTCEntry(0x020, 0x060, TYPE_CS, "Laterality", DD_Laterality),
        new RTCEntry(0x054, 0x0300, TYPE_SQ, "Radionuclide Code Sequence", DD_RadionuclideCodeSequence),
        new RTCEntry(0x018, 0x01019, TYPE_LO, "Secondary Capture Device Software Version(s)",
                     DD_SecondaryCaptureDeviceSoftwareVersion),
        new RTCEntry(0x08, 0x02112, TYPE_SQ, "Source Image Sequence", DD_SourceImageSequence),
        new RTCEntry(0x018, 0x01018, TYPE_LO, "Secondary Capture Device Manufacturer's Model Name",
                     DD_SecondaryCaptureDeviceManufacturerModelName),
        new RTCEntry(0x08, 0x02111, TYPE_ST, "Derivation Description", DD_DerivationDescription),
        new RTCEntry(0x018, 0x01017, TYPE_LO, "Hardcopy Device Manufacturer", DD_HardcopyDeviceManufacturer),
        new RTCEntry(0x018, 0x01016, TYPE_LO, "Secondary Capture Device Manufacturer",
                     DD_SecondaryCaptureDeviceManufacturer),
        new RTCEntry(0x018, 0x01014, TYPE_TM, "Time of Secondary Capture", DD_TimeOfSecondaryCapture),
        new RTCEntry(0x018, 0x01012, TYPE_DA, "Date of Secondary Capture", DD_DateOfSecondaryCapture),
        new RTCEntry(0x018, 0x01011, TYPE_LO, "Hardcopy Creation Device ID", DD_HardcopyCreationDeviceID),
        new RTCEntry(0x018, 0x01010, TYPE_LO, "Secondary Capture Device ID", DD_SecondaryCaptureDeviceID),
        new RTCEntry(0x0300c, 0x0f0, TYPE_IS, "Referenced Control Point Index", DD_ReferencedControlPointIndex),
        new RTCEntry(0x020, 0x052, TYPE_UI, "Frame of Reference UID", DD_FrameOfReferenceUID),
        new RTCEntry(0x028, 0x01351, TYPE_ST, "Partial View Description", DD_PartialViewDescription),
        new RTCEntry(0x018, 0x01004, TYPE_LO, "Plate ID", DD_PlateID),
        new RTCEntry(0x028, 0x01350, TYPE_CS, "Partial View", DD_PartialView),
        new RTCEntry(0x018, 0x01000, TYPE_LO, "Device Serial Number", DD_DeviceSerialNumber),
        new RTCEntry(0x08, 0x01010, TYPE_SH, "Station Name", DD_StationName),
        new RTCEntry(0x0300a, 0x055, TYPE_CS, "RT Plan Relationship", DD_RTPlanRelationship),
        new RTCEntry(0x0300c, 0x0e0, TYPE_IS, "Referenced Block Number", DD_ReferencedBlockNumber),
        new RTCEntry(0x0300a, 0x053, TYPE_DS, "Table Top Lateral Position Tolerance",
                     DD_TableTopLateralPositionTolerance),
        new RTCEntry(0x0300a, 0x052, TYPE_DS, "Table Top Longitudinal Position Tolerance",
                     DD_TableTopLongitudinalPositionTolerance),
        new RTCEntry(0x0300a, 0x051, TYPE_DS, "Table Top Vertical Position Tolerance",
                     DD_TableTopVerticalPositionTolerance),
        new RTCEntry(0x040, 0x0100, TYPE_SQ, "Scheduled Procedure Step Sequence", DD_ScheduledProcedureStepSequence),
        new RTCEntry(0x010, 0x050, TYPE_SQ, "Patient's Insurance Plan Code Sequence",
                     DD_PatientInsurancePlanCodeSequence),
        new RTCEntry(0x0300a, 0x04e, TYPE_DS, "Table Top Eccentric Angle Tolerance",
                     DD_TableTopEccentricAngleTolerance),
        new RTCEntry(0x0300a, 0x04c, TYPE_DS, "Patient Support Angle Tolerance", DD_PatientSupportAngleTolerance),
        new RTCEntry(0x0300a, 0x04a, TYPE_DS, "Beam Limiting Device Position Tolerance",
                     DD_BeamLimitingDevicePositionTolerance),
        new RTCEntry(0x0300a, 0x048, TYPE_SQ, "Beam Limiting Device Tolerance Sequence",
                     DD_BeamLimitingDeviceToleranceSequence),
        new RTCEntry(0x0300a, 0x046, TYPE_DS, "Beam Limiting Device Angle Tolerance",
                     DD_BeamLimitingDeviceAngleTolerance),
        new RTCEntry(0x020, 0x037, TYPE_DS, "Image Orientation (Patient)", DD_ImageOrientationPatient),
        new RTCEntry(0x0300a, 0x044, TYPE_DS, "Gantry Angle Tolerance", DD_GantryAngleTolerance),
        new RTCEntry(0x0300c, 0x0d0, TYPE_IS, "Referenced Compensator Number", DD_ReferencedCompensatorNumber),
        new RTCEntry(0x0300a, 0x043, TYPE_SH, "Tolerance Table Label", DD_ToleranceTableLabel),
        new RTCEntry(0x0300a, 0x042, TYPE_IS, "Tolerance Table Number", DD_ToleranceTableNumber),
        new RTCEntry(0x0300a, 0x040, TYPE_SQ, "Tolerance Table Sequence", DD_ToleranceTableSequence),
        new RTCEntry(0x020, 0x032, TYPE_DS, "Image Position (Patient)", DD_ImagePositionPatient),
        new RTCEntry(0x010, 0x040, TYPE_CS, "Patient's Sex", DD_PatientSex),
        new RTCEntry(0x05000, 0x040, TYPE_SH, "Axis Labels", DD_AxisLabels),
        new RTCEntry(0x040, 0x0a170, TYPE_SQ, "Purpose of Reference Code Sequence", DD_PurposeOfReferenceCodeSequence),
        new RTCEntry(0x020, 0x026, TYPE_IS, "LUT Number", DD_LUTNumber),
        new RTCEntry(0x0300c, 0x0c0, TYPE_IS, "Referenced Wedge Number", DD_ReferencedWedgeNumber),
        new RTCEntry(0x020, 0x024, TYPE_IS, "Curve Number", DD_CurveNumber),
        new RTCEntry(0x020, 0x022, TYPE_IS, "Overlay Number", DD_OverlayNumber),
        new RTCEntry(0x020, 0x020, TYPE_CS, "Patient Orientation", DD_PatientOrientation),
        new RTCEntry(0x010, 0x032, TYPE_TM, "Patient's Birth Time", DD_PatientBirthTime),
        new RTCEntry(0x010, 0x030, TYPE_DA, "Patient's Birth Date", DD_PatientBirthDate),
        new RTCEntry(0x0300a, 0x02d, TYPE_DS, "Organ at Risk Overdose Volume Fraction",
                     DD_OrganAtRiskOverdoseVolumeFraction),
        new RTCEntry(0x0300a, 0x02c, TYPE_DS, "Organ at Risk Maximum Dose", DD_OrganAtRiskMaximumDose),
        new RTCEntry(0x05000, 0x030, TYPE_SH, "Axis Units", DD_AxisUnits),
        new RTCEntry(0x0300a, 0x02b, TYPE_DS, "Organ at Risk Limit Dose", DD_OrganAtRiskLimitDose),
        new RTCEntry(0x0300a, 0x02a, TYPE_DS, "Organ at Risk Full-volume Dose", DD_OrganAtRiskFullVolumeDose),
        new RTCEntry(0x040, 0x0a168, TYPE_SQ, "Concept Code Sequence", DD_ConceptCodeSequence),
        new RTCEntry(0x0300a, 0x028, TYPE_DS, "Target Underdose Volume Fraction", DD_TargetUnderdoseVolumeFraction),
        new RTCEntry(0x040, 0x0a160, TYPE_UT, "Text Value", DD_TextValue),
        new RTCEntry(0x0300a, 0x027, TYPE_DS, "Target Maximum Dose", DD_TargetMaximumDose),
        new RTCEntry(0x020, 0x019, TYPE_IS, "Item Number", DD_ItemNumber),
        new RTCEntry(0x0300a, 0x026, TYPE_DS, "Target Prescription Dose", DD_TargetPrescriptionDose),
        new RTCEntry(0x0300a, 0x025, TYPE_DS, "Target Minimum Dose", DD_TargetMinimumDose),
        new RTCEntry(0x038, 0x0300, TYPE_LO, "Current Patient Location", DD_CurrentPatientLocation),
        new RTCEntry(0x0300c, 0x0b0, TYPE_SQ, "Referenced Bolus Sequence", DD_ReferencedBolusSequence),
        new RTCEntry(0x0300a, 0x023, TYPE_DS, "Delivery Maximum Dose", DD_DeliveryMaximumDose),
        new RTCEntry(0x0300a, 0x022, TYPE_DS, "Delivery Warning Dose", DD_DeliveryWarningDose),
        new RTCEntry(0x0300a, 0x021, TYPE_DS, "Constraint Weight", DD_ConstraintWeight),
        new RTCEntry(0x020, 0x013, TYPE_IS, "Instance (formerly Image) Number", DD_InstanceNumber),
        new RTCEntry(0x0300a, 0x020, TYPE_CS, "Dose Reference Type", DD_DoseReferenceType),
        new RTCEntry(0x020, 0x012, TYPE_IS, "Acquisition Number", DD_AcquisitionNumber),
        new RTCEntry(0x020, 0x011, TYPE_IS, "Series Number", DD_SeriesNumber),
        new RTCEntry(0x020, 0x010, TYPE_SH, "Study ID", DD_StudyID),
        new RTCEntry(0x010, 0x021, TYPE_LO, "Issuer of Patient's ID", DD_IssuerOfPatientID),
        new RTCEntry(0x010, 0x020, TYPE_LO, "Patient's ID", DD_PatientID),
        new RTCEntry(0x05000, 0x022, TYPE_LO, "Curve Description", DD_CurveDescription),
        new RTCEntry(0x020, 0x0e, TYPE_UI, "Series Instance UID", DD_SeriesInstanceUID),
        new RTCEntry(0x05000, 0x020, TYPE_CS, "Type of Data", DD_TypeOfData),
        new RTCEntry(0x02020, 0x050, TYPE_CS, "Requested Resolution ID", DD_RequestedResolutionID),
        new RTCEntry(0x020, 0x0d, TYPE_UI, "Study Instance UID", DD_StudyInstanceUID),
        new RTCEntry(0x0300a, 0x01a, TYPE_DS, "Nominal Prior Dose", DD_NominalPriorDose),
        new RTCEntry(0x0300a, 0x018, TYPE_DS, "Dose Reference Point Coordinates", DD_DoseReferencePointCoordinates),
        new RTCEntry(0x0300a, 0x016, TYPE_LO, "Dose ReferenceDescription", DD_DoseReferenceDescription),
        new RTCEntry(0x0300a, 0x015, TYPE_CS, "Nominal Beam Energy Unit", DD_NominalBeamEnergyUnit),
        new RTCEntry(0x04, 0x01220, TYPE_SQ, "Directory Record Sequence", DD_DirectoryRecordSequence),
        new RTCEntry(0x0300a, 0x014, TYPE_CS, "Dose Reference Structure Type", DD_DoseReferenceStructureType),
        new RTCEntry(0x0300c, 0x0a0, TYPE_IS, "Referenced Tolerance Table Number", DD_ReferencedToleranceTableNumber),
        new RTCEntry(0x0300a, 0x012, TYPE_IS, "Dose ReferenceNumber", DD_DoseReferenceNumber),
        new RTCEntry(0x0300a, 0x010, TYPE_SQ, "Dose ReferenceSequence", DD_DoseReferenceSequence),
        new RTCEntry(0x020, 0x00, TYPE_UL, "Relationship Group Length", DD_RelationshipGroupLength),
        new RTCEntry(0x010, 0x010, TYPE_PN, "Patient's Name", DD_PatientName),
        new RTCEntry(0x018, 0x01318, TYPE_DS, "dB/dt", DD_dBdt), new RTCEntry(0x018, 0x01316, TYPE_DS, "SAR", DD_SAR),
        new RTCEntry(0x0300a, 0x0e, TYPE_ST, "Prescription Description", DD_PrescriptionDescription),
        new RTCEntry(0x028, 0x01300, TYPE_CS, "Implant Present", DD_ImplantPresent),
        new RTCEntry(0x018, 0x01315, TYPE_CS, "Variable Flip Angle Flag", DD_VariableFlipAngleFlag),
        new RTCEntry(0x018, 0x01314, TYPE_DS, "Flip Angle", DD_FlipAngle),
        new RTCEntry(0x0300a, 0x0c, TYPE_CS, "RT Plan Geometry", DD_RTPlanGeometry),
        new RTCEntry(0x05000, 0x010, TYPE_US, "Number of Points", DD_NumberOfPoints),
        new RTCEntry(0x0300a, 0x0b, TYPE_LO, "Treatment Sites", DD_TreatmentSites),
        new RTCEntry(0x02020, 0x040, TYPE_CS, "Requested Decimate/Crop Behavior", DD_RequestedDecimateCropBehavior),
        new RTCEntry(0x018, 0x01312, TYPE_CS, "In-plane Phase Encoding Direction", DD_InPlanePhaseEncodingDirection),
        new RTCEntry(0x0300a, 0x0a, TYPE_CS, "Treatment Intent", DD_TreatmentIntent),
        new RTCEntry(0x018, 0x01310, TYPE_US, "Acquisition Matrix", DD_AcquisitionMatrix),
        new RTCEntry(0x0300a, 0x09, TYPE_LO, "Treatment Protocols", DD_TreatmentProtocols),
        new RTCEntry(0x0300a, 0x07, TYPE_TM, "RT Plan Time", DD_RTPlanTime),
        new RTCEntry(0x04, 0x01212, TYPE_US, "File Set Consistency Flag", DD_FileSetConsistencyFlag),
        new RTCEntry(0x0300a, 0x06, TYPE_DA, "RT Plan Date", DD_RTPlanDate),
        new RTCEntry(0x0300a, 0x04, TYPE_ST, "RT Plan Description", DD_RTPlanDescription),
        new RTCEntry(0x0300a, 0x03, TYPE_LO, "RT Plan Name", DD_RTPlanName),
        new RTCEntry(0x0300a, 0x02, TYPE_SH, "RT Plan Label", DD_RTPlanLabel),
        new RTCEntry(0x010, 0x00, TYPE_UL, "Patient Group Length", DD_PatientGroupLength),
        new RTCEntry(0x05000, 0x05, TYPE_US, "Curve Dimensions", DD_CurveDimensions),
        new RTCEntry(0x02030, 0x020, TYPE_LO, "Text String", DD_TextString),
        new RTCEntry(0x05000, 0x00, TYPE_UL, "Curve Group Length", DD_CurveGroupLength),
        new RTCEntry(0x02020, 0x030, TYPE_DS, "Requested Image Size", DD_RequestedImageSize),
        new RTCEntry(0x018, 0x01302, TYPE_IS, "Scan Length", DD_ScanLength),
        new RTCEntry(0x018, 0x01301, TYPE_CS, "Whole Body Technique", DD_WholeBodyTechnique),
        new RTCEntry(0x018, 0x01300, TYPE_DS, "Scan Velocity", DD_ScanVelocity),
        new RTCEntry(0x040, 0x0a136, TYPE_US, "Referenced Frame Numbers", DD_ReferencedFrameNumbers),
        new RTCEntry(0x04, 0x01202, TYPE_UL, "Root Directory Entity Last Directory Record Offset",
                     DD_RootDirectoryLastRecord),
        new RTCEntry(0x04, 0x01200, TYPE_UL, "Root Directory Entity First Directory Record Offset",
                     DD_RootDirectoryFirstRecord),
        new RTCEntry(0x040, 0x0400, TYPE_LT, "Comments On Scheduled Procedure Step",
                     DD_CommentsOnScheduledProcedureStep),
        new RTCEntry(0x02030, 0x010, TYPE_US, "Annotation Position", DD_AnnotationPosition),
        new RTCEntry(0x00, 0x03, TYPE_UI, "Requested SOP Class UID", DD_RequestedSOPClassUID),
        new RTCEntry(0x00, 0x02, TYPE_UI, "Affected SOP Class UID", DD_AffectedSOPClassUID),
        new RTCEntry(0x00, 0x00, TYPE_UL, "Group Length", DD_GroupLength),
        new RTCEntry(0x02020, 0x020, TYPE_CS, "Polarity", DD_Polarity),
        new RTCEntry(0x040, 0x0a124, TYPE_UI, "UID", DD_UID),
        new RTCEntry(0x040, 0x0a123, TYPE_PN, "Person Name", DD_PersonName),
        new RTCEntry(0x040, 0x0a122, TYPE_TM, "Time", DD_Time), new RTCEntry(0x040, 0x0a121, TYPE_DA, "Date", DD_Date),
        new RTCEntry(0x02030, 0x00, TYPE_UL, "Annotation Group Length", DD_AnnotationGroupLength),
        new RTCEntry(0x028, 0x06190, TYPE_ST, "Mask Operation Explanation", DD_MaskOperationExplanation),
        new RTCEntry(0x02020, 0x010, TYPE_US, "Image Box Position", DD_ImageBoxPosition),
        new RTCEntry(0x08, 0x0201, TYPE_SH, "Timezone Offset From UTC", DD_TimezoneOffsetFromUTC),
        new RTCEntry(0x02020, 0x00, TYPE_UL, "Image Box Group Length", DD_ImageBoxGroupLength),
        new RTCEntry(0x05000, 0x02500, TYPE_LO, "Curve Label", DD_CurveLabel),
        new RTCEntry(0x018, 0x01620, TYPE_IS, "Vertices of Polygonal Shutter", DD_VerticesOfPolygonalShutter),
        new RTCEntry(0x06000, 0x01203, TYPE_US, "Overlays - Blue", DD_OverlayBlue),
        new RTCEntry(0x06000, 0x01202, TYPE_US, "Overlays - Green", DD_OverlayGreen),
        new RTCEntry(0x06000, 0x01201, TYPE_US, "Overlays - Red", DD_OverlayRed),
        new RTCEntry(0x06000, 0x01200, TYPE_US, "Overlays - Gray", DD_OverlayGray),
        new RTCEntry(0x018, 0x01612, TYPE_IS, "Radius of Circular Shutter", DD_RadiusOfCircularShutter),
        new RTCEntry(0x018, 0x01610, TYPE_IS, "Center of Circular Shutter", DD_CenterOfCircularShutter),
        new RTCEntry(0x04, 0x01512, TYPE_UI, "Referenced Transfer Syntax UID In File",
                     DD_ReferencedTransferSyntaxUIDInFile),
        new RTCEntry(0x04, 0x01511, TYPE_UI, "Referenced SOP Instance UID In File", DD_ReferencedSOPInstanceUIDInFile),
        new RTCEntry(0x04, 0x01510, TYPE_UI, "Referenced SOP Class UID In File", DD_ReferencedSOPClassUIDInFile),
        new RTCEntry(0x018, 0x01608, TYPE_IS, "Shutter Lower Horizontal Edge", DD_ShutterLowerHorizontalEdge),
        new RTCEntry(0x018, 0x01606, TYPE_IS, "Shutter Upper Horizontal Edge", DD_ShutterUpperHorizontalEdge),
        new RTCEntry(0x06000, 0x0102, TYPE_US, "Overlay Bit Position", DD_OverlayBitPosition),
        new RTCEntry(0x018, 0x01604, TYPE_IS, "Shutter Right Vertical Edge", DD_ShutterRightVerticalEdge),
        new RTCEntry(0x06000, 0x0100, TYPE_US, "Overlay Bits Allocated", DD_OverlayBitsAllocated),
        new RTCEntry(0x018, 0x01602, TYPE_IS, "Shutter Left Vertical Edge", DD_ShutterLeftVerticalEdge),
        new RTCEntry(0x010, 0x021f0, TYPE_LO, "Patient's Religious Preference", DD_PatientReligiousPreference),
        new RTCEntry(0x018, 0x01600, TYPE_CS, "Shutter Shape", DD_ShutterShape),
        new RTCEntry(0x04, 0x01504, TYPE_UL, "MRDR Directory Record Offset", DD_MRDRDirectoryRecordOffset),
        new RTCEntry(0x04, 0x01500, TYPE_CS, "Referenced File ID", DD_ReferencedFileID),
        new RTCEntry(0x0300c, 0x080, TYPE_SQ, "Referenced Dose Sequence", DD_ReferencedDoseSequence),
        new RTCEntry(0x010, 0x021d0, TYPE_DA, "Last Menstrual Date", DD_LastMenstrualDate),
        new RTCEntry(0x018, 0x05050, TYPE_IS, "Depth of Scan Field", DD_DepthOfScanField),
        new RTCEntry(0x0300c, 0x06a, TYPE_IS, "Referenced Patient Setup Number", DD_ReferencedPatientSetupNumber),
        new RTCEntry(0x028, 0x06120, TYPE_SS, "TID Offset", DD_TIDOffset),
        new RTCEntry(0x010, 0x021c0, TYPE_US, "Pregnancy Status", DD_PregnancyStatus),
        new RTCEntry(0x088, 0x0140, TYPE_UI, "Storage Media FileSet UID", DD_StorageMediaFileSetUID),
        new RTCEntry(0x0300c, 0x060, TYPE_SQ, "Referenced Structure Set Sequence", DD_ReferencedStructureSetSequence),
        new RTCEntry(0x04008, 0x050, TYPE_SQ, "Referenced Interpretation Sequence",
                     DD_ReferencedInterpretationSequence),
        new RTCEntry(0x03008, 0x0251, TYPE_TM, "Treatment Time", DD_TreatmentTime),
        new RTCEntry(0x03008, 0x0250, TYPE_DA, "Treatment Date", DD_TreatmentDate),
        new RTCEntry(0x028, 0x06114, TYPE_FL, "Mask Sub-Pixel Shift", DD_MaskSubPixelShift),
        new RTCEntry(0x028, 0x06112, TYPE_US, "Contrast Frame Averaging", DD_ContrastFrameAveraging),
        new RTCEntry(0x028, 0x06110, TYPE_US, "Mask Frame Numbers", DD_MaskFrameNumbers),
        new RTCEntry(0x010, 0x021b0, TYPE_LT, "Additional Patient History", DD_AdditionalPatientHistory),
        new RTCEntry(0x088, 0x0130, TYPE_SH, "Storage Media FileSet ID", DD_StorageMediaFileSetID),
        new RTCEntry(0x0300c, 0x055, TYPE_SQ, "Brachy Referenced Dose Reference Sequence",
                     DD_BrachyReferencedDoseReferenceSequence),
        new RTCEntry(0x0300c, 0x051, TYPE_IS, "Referenced Dose Reference Number", DD_ReferencedDoseReferenceNumber),
        new RTCEntry(0x0300c, 0x050, TYPE_SQ, "Referenced Dose Reference Sequence", DD_ReferencedDoseReferenceSequence),
        new RTCEntry(0x04008, 0x042, TYPE_LO, "Results ID Issuer", DD_ResultsIDIssuer),
        new RTCEntry(0x04008, 0x040, TYPE_SH, "Results ID", DD_ResultsID),
        new RTCEntry(0x03008, 0x0240, TYPE_SQ, "Fraction Status Summary Sequence", DD_FractionStatusSummarySequence),
        new RTCEntry(0x06000, 0x01500, TYPE_LO, "Overlay Label", DD_OverlayLabel),
        new RTCEntry(0x038, 0x04000, TYPE_LT, "Visit Comments", DD_VisitComments),
        new RTCEntry(0x028, 0x06102, TYPE_US, "Applicable Frame Range", DD_ApplicableFrameRange),
        new RTCEntry(0x028, 0x06101, TYPE_CS, "Mask Operation", DD_MaskOperation),
        new RTCEntry(0x028, 0x06100, TYPE_SQ, "Mask Subtraction Sequence", DD_MaskSubtractionSequence),
        new RTCEntry(0x010, 0x021a0, TYPE_CS, "Smoking Status", DD_SmokingStatus),
        new RTCEntry(0x018, 0x05029, TYPE_DS, "Soft Tissue-Surface Thermal Index", DD_SoftTissueSurfaceThermalIndex),
        new RTCEntry(0x018, 0x05028, TYPE_DS, "Soft Tissue-Focus Thermal Index", DD_SoftTissueFocusThermalIndex),
        new RTCEntry(0x018, 0x05027, TYPE_DS, "Soft Tissue Thermal Index", DD_SoftTissueThermalIndex),
        new RTCEntry(0x018, 0x05026, TYPE_DS, "Cranial Thermal Index", DD_CranialThermalIndex),
        new RTCEntry(0x018, 0x05024, TYPE_DS, "Bone Thermal Index", DD_BoneThermalIndex),
        new RTCEntry(0x0300c, 0x042, TYPE_SQ, "Referenced Reference Image Sequence",
                     DD_ReferencedReferenceImageSequence),
        new RTCEntry(0x018, 0x05022, TYPE_DS, "Mechanical Index", DD_MechanicalIndex),
        new RTCEntry(0x0300c, 0x040, TYPE_SQ, "Referenced Verification Image Sequence",
                     DD_ReferencedVerificationImageSequence),
        new RTCEntry(0x018, 0x05021, TYPE_LO, "Postprocessing Function", DD_PostprocessingFunction),
        new RTCEntry(0x018, 0x05020, TYPE_LO, "Processing Function", DD_ProcessingFunction),
        new RTCEntry(0x03008, 0x0230, TYPE_CS, "Beam Stopper Position", DD_BeamStopperPosition),
        new RTCEntry(0x028, 0x05000, TYPE_SQ, "Biplane Acquisition Sequence", DD_BiplaneAcquisitionSequence),
        new RTCEntry(0x040, 0x0a730, TYPE_SQ, "Content Sequence", DD_ContentSequence),
        new RTCEntry(0x018, 0x05012, TYPE_DS, "Focus Depth", DD_FocusDepth),
        new RTCEntry(0x03008, 0x0224, TYPE_CS, "Fraction Group Type", DD_FractionGroupType),
        new RTCEntry(0x018, 0x05010, TYPE_LO, "Transducer Data", DD_TransducerData),
        new RTCEntry(0x03008, 0x0223, TYPE_IS, "Referenced Fraction Number", DD_ReferencedFractionNumber),
        new RTCEntry(0x03008, 0x0220, TYPE_SQ, "Fraction Group Summary Sequence", DD_FractionGroupSummarySequence),
        new RTCEntry(0x00, 0x0600, TYPE_AE, "Move Destination", DD_MoveDestination),
        new RTCEntry(0x0300c, 0x022, TYPE_IS, "Referenced Fraction Group Number", DD_ReferencedFractionGroupNumber),
        new RTCEntry(0x0300c, 0x020, TYPE_SQ, "Referenced Fraction Group Sequence", DD_ReferencedFractionGroupSequence),
        new RTCEntry(0x018, 0x05000, TYPE_SH, "Output Power", DD_OutputPower),
        new RTCEntry(0x02110, 0x099, TYPE_SH, "Print Queue ID", DD_PrintQueueID),
        new RTCEntry(0x03008, 0x0202, TYPE_ST, "Treatment Status Comment", DD_TreatmentStatusComment),
        new RTCEntry(0x04008, 0x00, TYPE_UL, "Results Group Length", DD_ResultsGroupLength),
        new RTCEntry(0x03008, 0x0200, TYPE_CS, "Current Treatment  Status", DD_CurrentTreatmentStatus),
        new RTCEntry(0x0300c, 0x0e, TYPE_IS, "Referenced Source Number", DD_ReferencedSourceNumber),
        new RTCEntry(0x0300c, 0x0c, TYPE_IS, "Referenced Brachy Application Setup Number",
                     DD_ReferencedBrachyApplicationSetupNumber),
        new RTCEntry(0x0300c, 0x0a, TYPE_SQ, "Referenced Brachy Application Setup Sequence",
                     DD_ReferencedBrachyApplicationSetupSequence),
        new RTCEntry(0x0300c, 0x09, TYPE_DS, "End Cumulative Meterset Weight", DD_EndCumulativeMetersetWeight),
        new RTCEntry(0x0300c, 0x08, TYPE_DS, "Start Cumulative Meterset Weight", DD_StartCumulativeMetersetWeight),
        new RTCEntry(0x0300c, 0x07, TYPE_IS, "Referenced Reference Image Number", DD_ReferencedReferenceImageNumber),
        new RTCEntry(0x040, 0x06fa, TYPE_LO, "Slide Identifier", DD_SlideIdentifier),
        new RTCEntry(0x0300c, 0x06, TYPE_IS, "Referenced Beam Number", DD_ReferencedBeamNumber),
        new RTCEntry(0x0300c, 0x04, TYPE_SQ, "Referenced Beam Sequence", DD_ReferencedBeamSequence),
        new RTCEntry(0x0300c, 0x02, TYPE_SQ, "Referenced RT Plan Sequence", DD_ReferencedRTPlanSequence),
        new RTCEntry(0x040, 0x03001, TYPE_LO, "Confidentiality Constraint On Patient Data Description",
                     DD_ConfidentialityConstraintOnPatientDataDescription),
        new RTCEntry(0x02, 0x016, TYPE_AE, "Source Application Entity Title", DD_SourceApplicationEntityTitle),
        new RTCEntry(0x02, 0x013, TYPE_SH, "Implementation Version Name", DD_ImplementationVersionName),
        new RTCEntry(0x02, 0x012, TYPE_UI, "Implementation Class UID", DD_ImplementationClassUID),
        new RTCEntry(0x02, 0x010, TYPE_UI, "Transfer Syntax UID", DD_TransferSyntaxUID),
        new RTCEntry(0x02010, 0x0520, TYPE_SQ, "Referenced Basic Annotation Box Sequence",
                     DD_ReferencedBasicAnnotationBoxSequence),
        new RTCEntry(0x054, 0x01004, TYPE_CS, "Reprojection Method", DD_ReprojectionMethod),
        new RTCEntry(0x054, 0x01002, TYPE_CS, "Counts Source", DD_CountsSource),
        new RTCEntry(0x054, 0x01001, TYPE_CS, "Units", DD_Units),
        new RTCEntry(0x054, 0x01000, TYPE_CS, "Series Type", DD_SeriesType),
        new RTCEntry(0x0300a, 0x02d6, TYPE_DS, "Cumulative Time Weight", DD_CumulativeTimeWeight),
        new RTCEntry(0x0300a, 0x02d4, TYPE_DS, "Control Point 3D Position", DD_ControlPoint3DPosition),
        new RTCEntry(0x0300a, 0x02d2, TYPE_DS, "Control Point Relative Position", DD_ControlPointRelativePosition),
        new RTCEntry(0x0300a, 0x02d0, TYPE_SQ, "Brachy Control Point Sequence", DD_BrachyControlPointSequence),
        new RTCEntry(0x02, 0x03, TYPE_UI, "Media Storage SOP Instance UID", DD_MediaStorageSOPInstanceUID),
        new RTCEntry(0x02, 0x02, TYPE_UI, "Media Storage SOP Class UID", DD_MediaStorageSOPClassUID),
        new RTCEntry(0x02, 0x01, TYPE_OB, "File Meta Information Version", DD_FileMetaInformationVersion),
        new RTCEntry(0x02, 0x00, TYPE_UL, "File Meta Information Group Length", DD_FileMetaInformationGroupLength),
        new RTCEntry(0x02010, 0x0510, TYPE_SQ, "Referenced Image Box Sequence", DD_ReferencedImageBoxSequence),
        new RTCEntry(0x0300a, 0x02c8, TYPE_DS, "Final Cumulative Time Weight", DD_FinalCumulativeTimeWeight),
        new RTCEntry(0x02100, 0x070, TYPE_AE, "Originator", DD_Originator),
        new RTCEntry(0x02010, 0x0500, TYPE_SQ, "Referenced Film Session Sequence", DD_ReferencedFilmSessionSequence),
        new RTCEntry(0x0300a, 0x02ba, TYPE_DS, "Channel Shield Nominal Transmission",
                     DD_ChannelShieldNominalTransmission),
        new RTCEntry(0x02000, 0x0510, TYPE_SQ, "Referenced Stored Print Sequence", DD_ReferencedStoredPrintSequence),
        new RTCEntry(0x020, 0x04000, TYPE_LT, "Image Comments", DD_ImageComments),
        new RTCEntry(0x0300a, 0x02b8, TYPE_DS, "Channel Shield Nominal Thickness", DD_ChannelShieldNominalThickness),
        new RTCEntry(0x0300a, 0x02b4, TYPE_LO, "Channel Shield Name", DD_ChannelShieldName),
        new RTCEntry(0x0300a, 0x02b3, TYPE_SH, "Channel Shield ID", DD_ChannelShieldID),
        new RTCEntry(0x0300a, 0x02b2, TYPE_IS, "Channel Shield Number", DD_ChannelShieldNumber),
        new RTCEntry(0x0300a, 0x02b0, TYPE_SQ, "Channel Shield Sequence", DD_ChannelShieldSequence),
        new RTCEntry(0x00, 0x0903, TYPE_US, "Error ID", DD_ErrorID),
        new RTCEntry(0x00, 0x0902, TYPE_LO, "Error Comment", DD_ErrorComment),
        new RTCEntry(0x02000, 0x0500, TYPE_SQ, "Referenced Film Box Sequence", DD_ReferencedFilmBoxSequence),
        new RTCEntry(0x00, 0x0901, TYPE_AT, "Offending Element", DD_OffendingElement),
        new RTCEntry(0x00, 0x0900, TYPE_US, "Status", DD_Status),
        new RTCEntry(0x054, 0x01330, TYPE_US, "Image Index", DD_ImageIndex),
        new RTCEntry(0x010, 0x04000, TYPE_LT, "Patient Comments", DD_PatientComments),
        new RTCEntry(0x02100, 0x050, TYPE_TM, "Creation Time", DD_CreationTime),
        new RTCEntry(0x0300a, 0x02a4, TYPE_DS, "Transfer Tube Length", DD_TransferTubeLength),
        new RTCEntry(0x0300a, 0x02a2, TYPE_IS, "Transfer Tube Number", DD_TransferTubeNumber),
        new RTCEntry(0x0300a, 0x02a0, TYPE_DS, "Source Applicator Step Size", DD_SourceApplicatorStepSize),
        new RTCEntry(0x010, 0x02180, TYPE_SH, "Occupation", DD_Occupation),
        new RTCEntry(0x054, 0x01324, TYPE_DS, "Dead Time Factor", DD_DeadTimeFactor),
        new RTCEntry(0x054, 0x01323, TYPE_DS, "Scatter Fraction Factor", DD_ScatterFractionFactor),
        new RTCEntry(0x02110, 0x030, TYPE_LO, "Printer Name", DD_PrinterName),
        new RTCEntry(0x054, 0x01322, TYPE_DS, "Dose Calibration Factor", DD_DoseCalibrationFactor),
        new RTCEntry(0x054, 0x01321, TYPE_DS, "Decay Factor", DD_DecayFactor),
        new RTCEntry(0x054, 0x01320, TYPE_DS, "Slice Sensitivity Factor", DD_SliceSensitivityFactor),
        new RTCEntry(0x02100, 0x040, TYPE_DA, "Creation Date", DD_CreationDate),
        new RTCEntry(0x010, 0x01090, TYPE_LO, "Medical Record Locator", DD_MedicalRecordLocator),
        new RTCEntry(0x04008, 0x0300, TYPE_ST, "Impressions", DD_Impressions),
        new RTCEntry(0x018, 0x02030, TYPE_DS, "Rotation of Scanned Film", DD_RotationOfScannedFilm),
        new RTCEntry(0x02110, 0x020, TYPE_CS, "Printer Status Info", DD_PrinterStatusInfo),
        new RTCEntry(0x054, 0x01311, TYPE_IS, "Secondary Counts Accumulated", DD_SecondaryCountsAccumulated),
        new RTCEntry(0x020, 0x01070, TYPE_IS, "Other Study Numbers", DD_OtherStudyNumbers),
        new RTCEntry(0x054, 0x01310, TYPE_IS, "Primary Prompts Counts Accumulated", DD_PrimaryPromptsCountsAccumulated),
        new RTCEntry(0x02100, 0x030, TYPE_CS, "Execution Status Info", DD_ExecutionStatusInfo),
        new RTCEntry(0x010, 0x01081, TYPE_LO, "Branch of Service", DD_BranchOfService),
        new RTCEntry(0x010, 0x01080, TYPE_LO, "Military Rank", DD_MilitaryRank),
        new RTCEntry(0x054, 0x0222, TYPE_SQ, "View Modifier Code Sequence", DD_ViewModifierCodeSequence),
        new RTCEntry(0x018, 0x02020, TYPE_CS, "Digitizing Device Transport Direction",
                     DD_DigitizingDeviceTransportDirection),
        new RTCEntry(0x054, 0x0220, TYPE_SQ, "View Code Sequence", DD_ViewCodeSequence),
        new RTCEntry(0x02040, 0x082, TYPE_CS, "Overlay Background Density", DD_OverlayBackgroundDensity),
        new RTCEntry(0x02040, 0x080, TYPE_CS, "Overlay Foreground Density", DD_OverlayForegroundDensity),
        new RTCEntry(0x010, 0x02160, TYPE_SH, "Ethnic Group", DD_EthnicGroup),
        new RTCEntry(0x018, 0x03105, TYPE_IS, "Lesion Number", DD_LesionNumber),
        new RTCEntry(0x018, 0x03104, TYPE_IS, "IVUS Pullback Stop Frame Number", DD_IVUSPullbackStopFrameNumber),
        new RTCEntry(0x018, 0x03103, TYPE_IS, "IVUS Pullback Start Frame Number", DD_IVUSPullbackStartFrameNumber),
        new RTCEntry(0x02110, 0x010, TYPE_CS, "Printer Status", DD_PrinterStatus),
        new RTCEntry(0x018, 0x03102, TYPE_DS, "IVUS Gated Rate", DD_IVUSGatedRate),
        new RTCEntry(0x018, 0x03101, TYPE_DS, "IVUS Pullback Rate", DD_IVUSPullbackRate),
        new RTCEntry(0x018, 0x03100, TYPE_CS, "IVUS Acquisition", DD_IVUSAcquisition),
        new RTCEntry(0x054, 0x01300, TYPE_DS, "Frame Reference Time", DD_FrameReferenceTime),
        new RTCEntry(0x02100, 0x020, TYPE_CS, "Execution Status", DD_ExecutionStatus),
        new RTCEntry(0x054, 0x0211, TYPE_US, "Number of Triggers in Phase", DD_NumberOfTriggersInPhase),
        new RTCEntry(0x018, 0x02010, TYPE_DS, "Nominal Scanned Pixel Spacing", DD_NominalScannedPixelSpacing),
        new RTCEntry(0x054, 0x0210, TYPE_IS, "Trigger Vector", DD_TriggerVector),
        new RTCEntry(0x02040, 0x074, TYPE_US, "Magnify to Number of Columns", DD_MagnifyToNumberOfColumns),
        new RTCEntry(0x02040, 0x072, TYPE_CS, "Overlay Or Image Magnification", DD_OverlayOrImageMagnification),
        new RTCEntry(0x010, 0x02154, TYPE_SH, "Patient's Telephone Numbers", DD_PatientTelephoneNumber),
        new RTCEntry(0x02040, 0x070, TYPE_CS, "Overlay Smoothing Type", DD_OverlaySmoothingType),
        new RTCEntry(0x010, 0x02152, TYPE_LO, "Region of Residence", DD_RegionOfResidence),
        new RTCEntry(0x010, 0x02150, TYPE_LO, "Country of Residence", DD_CountryOfResidence),
        new RTCEntry(0x02110, 0x00, TYPE_UL, "Printer Group Length", DD_PrinterGroupLength),
        new RTCEntry(0x02100, 0x010, TYPE_SH, "Print Job ID", DD_PrintJobID),
        new RTCEntry(0x018, 0x02006, TYPE_SH, "Display Window Label Vector", DD_DisplayWindowLabelVector),
        new RTCEntry(0x010, 0x01060, TYPE_PN, "Patient's Mother's Birth Name", DD_PatientMotherBirthName),
        new RTCEntry(0x018, 0x02005, TYPE_DS, "Slice Location Vector", DD_SliceLocationVector),
        new RTCEntry(0x018, 0x02004, TYPE_DS, "Frame Secondary Angle Vector", DD_FrameSecondaryAngleVector),
        new RTCEntry(0x018, 0x02003, TYPE_DS, "Frame Primary Angle Vector", DD_FramePrimaryAngleVector),
        new RTCEntry(0x018, 0x02002, TYPE_SH, "Frame Label Vector", DD_FrameLabelVector),
        new RTCEntry(0x054, 0x0202, TYPE_CS, "Type of Detector Motion", DD_TypeOfDetectorMotion),
        new RTCEntry(0x018, 0x02001, TYPE_IS, "Page Number Vector", DD_PageNumberVector),
        new RTCEntry(0x040, 0x020, TYPE_CS, "Scheduled Procedure Step Status", DD_ScheduledProcedureStepStatus),
        new RTCEntry(0x054, 0x0200, TYPE_DS, "Start Angle", DD_StartAngle),
        new RTCEntry(0x02040, 0x060, TYPE_CS, "Overlay Magnification Type", DD_OverlayMagnificationType),
        new RTCEntry(0x020, 0x01041, TYPE_DS, "Slice Location", DD_SliceLocation),
        new RTCEntry(0x020, 0x01040, TYPE_LO, "Position Reference Indicator", DD_PositionReferenceIndicator),
        new RTCEntry(0x02100, 0x00, TYPE_UL, "Print Job Group Length", DD_PrintJobGroupLength),
        new RTCEntry(0x040, 0x012, TYPE_LO, "Pre-Medication", DD_PreMedication),
        new RTCEntry(0x040, 0x011, TYPE_SH, "Scheduled Procedure Step Location", DD_ScheduledProcedureStepLocation),
        new RTCEntry(0x040, 0x010, TYPE_SH, "Scheduled Station Name", DD_ScheduledStationName),
        new RTCEntry(0x018, 0x01261, TYPE_LO, "Phosphor Type", DD_PhosphorType),
        new RTCEntry(0x018, 0x01260, TYPE_SH, "Plate Type", DD_PlateType),
        new RTCEntry(0x040, 0x0a, TYPE_SQ, "Stage Code Sequence", DD_StageCodeSequence),
        new RTCEntry(0x040, 0x09, TYPE_SH, "Scheduled Procedure Step ID", DD_ScheduledProcedureStepID),
        new RTCEntry(0x040, 0x08, TYPE_SQ, "Scheduled Protocol Code Sequence", DD_ScheduledProtocolCodeSequence),
        new RTCEntry(0x010, 0x01040, TYPE_LO, "Patient's Address", DD_PatientAddress),
        new RTCEntry(0x040, 0x07, TYPE_LO, "Scheduled Procedure Step Description",
                     DD_ScheduledProcedureStepDescription),
        new RTCEntry(0x040, 0x06, TYPE_PN, "Scheduled Performing Physician Name", DD_ScheduledPerformingPhysicianName),
        new RTCEntry(0x040, 0x05, TYPE_TM, "Scheduled Procedure Step End Time", DD_ScheduledProcedureStepEndTime),
        new RTCEntry(0x040, 0x04, TYPE_DA, "Scheduled Procedure Step End Date", DD_ScheduledProcedureStepEndDate),
        new RTCEntry(0x040, 0x03, TYPE_TM, "Scheduled Procedure Step Start Time", DD_ScheduledProcedureStepStartTime),
        new RTCEntry(0x040, 0x02, TYPE_DA, "Scheduled Procedure Step Start Date", DD_ScheduledProcedureStepStartDate),
        new RTCEntry(0x040, 0x01, TYPE_AE, "Scheduled Station AE Title", DD_ScheduledStationAETitle),
        new RTCEntry(0x03002, 0x034, TYPE_DS, "Diaphragm Position", DD_DiaphragmPosition),
        new RTCEntry(0x03002, 0x032, TYPE_DS, "Meterset Exposure", DD_MetersetExposure),
        new RTCEntry(0x03002, 0x030, TYPE_SQ, "Exposure Sequence", DD_ExposureSequence),
        new RTCEntry(0x018, 0x01251, TYPE_SH, "Transmit Coil Name", DD_TransmitCoilName),
        new RTCEntry(0x018, 0x01250, TYPE_SH, "Receive Coil Name", DD_ReceiveCoilName),
        new RTCEntry(0x010, 0x01030, TYPE_DS, "Patient's Weight", DD_PatientWeight),
        new RTCEntry(0x03002, 0x029, TYPE_IS, "Fraction Number", DD_FractionNumber),
        new RTCEntry(0x03002, 0x028, TYPE_DS, "Source to Reference Object Distance",
                     DD_SourceToReferenceObjectDistance),
        new RTCEntry(0x03002, 0x026, TYPE_DS, "RT Image SID", DD_RTImageSID),
        new RTCEntry(0x03002, 0x024, TYPE_DS, "Radiation Machine SSD", DD_RadiationMachineSSD),
        new RTCEntry(0x03002, 0x022, TYPE_DS, "Radiation Machine SAD", DD_RadiationMachineSAD),
        new RTCEntry(0x02050, 0x020, TYPE_CS, "Presentation LUT Shape", DD_PresentationLUTShape),
        new RTCEntry(0x06000, 0x03000, TYPE_OW, "Overlay Data", DD_OverlayData),
        new RTCEntry(0x03002, 0x020, TYPE_SH, "Radiation Machine Name", DD_RadiationMachineName),
        new RTCEntry(0x018, 0x01244, TYPE_US, "Preferred Playback Sequencing", DD_PreferredPlaybackSequencing),
        new RTCEntry(0x0300a, 0x029e, TYPE_DS, "Source Applicator Wall Nominal Transmission",
                     DD_SourceApplicatorWallNominalTransmission),
        new RTCEntry(0x018, 0x01243, TYPE_IS, "Count Rate", DD_CountRate),
        new RTCEntry(0x018, 0x01242, TYPE_IS, "Actual Frame Duration", DD_ActualFrameDuration),
        new RTCEntry(0x0300a, 0x029c, TYPE_DS, "Source Applicator Wall Nominal Thickness",
                     DD_SourceApplicatorWallNominalThickness),
        new RTCEntry(0x010, 0x02110, TYPE_LO, "Contrast Allergies", DD_ContrastAllergies),
        new RTCEntry(0x04, 0x01142, TYPE_CS, "File Set Descriptor File Specific Character Set", DD_FileSetCharacterSet),
        new RTCEntry(0x0300a, 0x0298, TYPE_LO, "Source Applicator Manufacturer", DD_SourceApplicatorManufacturer),
        new RTCEntry(0x04, 0x01141, TYPE_CS, "File Set Descriptor File ID", DD_FileSetDescriptorFileID),
        new RTCEntry(0x0300a, 0x0296, TYPE_DS, "Source Applicator Length", DD_SourceApplicatorLength),
        new RTCEntry(0x010, 0x01020, TYPE_DS, "Patient's Size", DD_PatientSize),
        new RTCEntry(0x0300a, 0x0294, TYPE_LO, "Source Applicator Name", DD_SourceApplicatorName),
        new RTCEntry(0x0300a, 0x0292, TYPE_CS, "Source Applicator Type", DD_SourceApplicatorType),
        new RTCEntry(0x00, 0x01031, TYPE_US, "Move Originator Message ID", DD_MoveOriginatorMessageID),
        new RTCEntry(0x0300a, 0x0291, TYPE_SH, "Source Applicator ID", DD_SourceApplicatorID),
        new RTCEntry(0x00, 0x01030, TYPE_AE, "Move Originator Application Entity Title",
                     DD_MoveOriginatorApplicationEntityTitle),
        new RTCEntry(0x0300a, 0x0290, TYPE_IS, "Source Applicator Number", DD_SourceApplicatorNumber),
        new RTCEntry(0x040, 0x0340, TYPE_SQ, "Performed Series Sequence", DD_PerformedSeriesSequence),
        new RTCEntry(0x028, 0x01223, TYPE_OW, "Segmented Blue Palette Color Lookup Table Data",
                     DD_SegmentedBluePaletteColorLookupTableData),
        new RTCEntry(0x028, 0x01222, TYPE_OW, "Segmented Green Palette Color Lookup Table Data",
                     DD_SegmentedGreenPaletteColorLookupTableData),
        new RTCEntry(0x03002, 0x012, TYPE_DS, "RT Image Position", DD_RTImagePosition),
        new RTCEntry(0x028, 0x01221, TYPE_OW, "Segmented Red Palette Color Lookup Table Data",
                     DD_SegmentedRedPaletteColorLookupTableData),
        new RTCEntry(0x02050, 0x010, TYPE_SQ, "Presentation LUT Sequence", DD_PresentationLUTSequence),
        new RTCEntry(0x03002, 0x011, TYPE_DS, "Image Plane Pixel Spacing", DD_ImagePlanePixelSpacing),
        new RTCEntry(0x03002, 0x010, TYPE_DS, "RTImageOrientation", DD_RTImageOrientation),
        new RTCEntry(0x0300a, 0x028c, TYPE_DS, "Pulse Repetition Interval", DD_PulseRepetitionInterval),
        new RTCEntry(0x02040, 0x020, TYPE_SQ, "Overlay Pixel Data Sequence", DD_OverlayPixelDataSequence),
        new RTCEntry(0x0300a, 0x028a, TYPE_IS, "Number of Pulses", DD_NumberOfPulses),
        new RTCEntry(0x020, 0x01004, TYPE_IS, "Acquisitions in Study", DD_AcquisitionsInStudy),
        new RTCEntry(0x03002, 0x0e, TYPE_DS, "X-Ray Image Receptor Angle", DD_XRayImageReceptorAngle),
        new RTCEntry(0x020, 0x01002, TYPE_IS, "Images in Acquisition", DD_ImagesInAcquisition),
        new RTCEntry(0x03002, 0x0c, TYPE_CS, "RT Image Plane", DD_RTImagePlane),
        new RTCEntry(0x020, 0x01000, TYPE_IS, "Series in Study", DD_SeriesInStudy),
        new RTCEntry(0x03002, 0x0a, TYPE_CS, "Reported Values Origin", DD_ReportedValuesOrigin),
        new RTCEntry(0x0300a, 0x0288, TYPE_CS, "Source Movement Type", DD_SourceMovementType),
        new RTCEntry(0x04, 0x01130, TYPE_CS, "File Set ID", DD_FileSetID),
        new RTCEntry(0x0300a, 0x0286, TYPE_DS, "Channel Total Time", DD_ChannelTotalTime),
        new RTCEntry(0x010, 0x01010, TYPE_AS, "Patient's Age", DD_PatientAge),
        new RTCEntry(0x0300a, 0x0284, TYPE_DS, "Channel Length", DD_ChannelLength),
        new RTCEntry(0x00, 0x01023, TYPE_US, "Number of Warning Suboperations", DD_NumberOfWarningSuboperations),
        new RTCEntry(0x00, 0x01022, TYPE_US, "Number of Failed Suboperations", DD_NumberOfFailedSuboperations),
        new RTCEntry(0x0300a, 0x0282, TYPE_IS, "Channel Number", DD_ChannelNumber),
        new RTCEntry(0x00, 0x01021, TYPE_US, "Number of Completed Suboperations", DD_NumberOfCompletedSuboperations),
        new RTCEntry(0x00, 0x01020, TYPE_US, "Number of Remaining Suboperations", DD_NumberOfRemainingSuboperations),
        new RTCEntry(0x0300a, 0x0280, TYPE_SQ, "Channel Sequence", DD_ChannelSequence),
        new RTCEntry(0x040, 0x0330, TYPE_SQ, "Referenced Procedure Step Sequence", DD_ReferencedProcedureStepSequence),
        new RTCEntry(0x03002, 0x04, TYPE_ST, "RT Image Description", DD_RTImageDescription),
        new RTCEntry(0x03002, 0x03, TYPE_LO, "RT Image Name", DD_RTImageName),
        new RTCEntry(0x03002, 0x02, TYPE_SH, "RT Image Label", DD_RTImageLabel),
        new RTCEntry(0x02040, 0x011, TYPE_US, "Referenced Overlay Plane Groups", DD_ReferencedOverlayPlaneGroups),
        new RTCEntry(0x02040, 0x010, TYPE_SQ, "Referenced Overlay Plane Sequence", DD_ReferencedOverlayPlaneSequence),
        new RTCEntry(0x028, 0x0120, TYPE_US, "Pixel Padding Value", DD_PixelPaddingValue),
        new RTCEntry(0x010, 0x01005, TYPE_PN, "Patient's Birth Name", DD_PatientBirthName),
        new RTCEntry(0x010, 0x01001, TYPE_PN, "Other Patient's Names", DD_OtherPatientName),
        new RTCEntry(0x010, 0x01000, TYPE_LO, "Other Patient's ID's", DD_OtherPatientID),
        new RTCEntry(0x040, 0x0324, TYPE_SQ, "Billing Supplies And Devices Sequence",
                     DD_BillingSuppliesAndDevicesSequence),
        new RTCEntry(0x040, 0x0321, TYPE_SQ, "Film Consumption Sequence", DD_FilmConsumptionSequence),
        new RTCEntry(0x040, 0x0320, TYPE_SQ, "Billing Procedure Step Sequence", DD_BillingProcedureStepSequence),
        new RTCEntry(0x028, 0x01203, TYPE_OW, "Blue Palette Color Lookup Table Data",
                     DD_BluePaletteColorLookupTableData),
        new RTCEntry(0x028, 0x01202, TYPE_OW, "Green Palette Color Lookup Table Data",
                     DD_GreenPaletteColorLookupTableData),
        new RTCEntry(0x028, 0x01201, TYPE_OW, "Red Palette Color Lookup Table Data", DD_RedPaletteColorLookupTableData),
        new RTCEntry(0x0300a, 0x026c, TYPE_DS, "Brachy Accessory Device Nominal Transmission",
                     DD_BrachyAccessoryDeviceNominalTransmission),
        new RTCEntry(0x02040, 0x00, TYPE_UL, "Overlay Box Group Length", DD_OverlayBoxGroupLength),
        new RTCEntry(0x018, 0x01210, TYPE_SH, "Convolution Kernel", DD_ConvolutionKernel),
        new RTCEntry(0x0300a, 0x026a, TYPE_DS, "Brachy Accessory Device Nominal Thickness",
                     DD_BrachyAccessoryDeviceNominalThickness),
        new RTCEntry(0x028, 0x0111, TYPE_US, "Largest Pixel Value in Plane", DD_LargestPixelValueInPlane),
        new RTCEntry(0x040, 0x0a043, TYPE_SQ, "Concept Name Code Sequence", DD_ConceptNameCodeSequence),
        new RTCEntry(0x028, 0x0110, TYPE_US, "Smallest Pixel Value in Plane", DD_SmallestPixelValueInPlane),
        new RTCEntry(0x00, 0x01008, TYPE_US, "Action Type ID", DD_ActionTypeID),
        new RTCEntry(0x040, 0x01400, TYPE_LT, "Requested Procedure Comments", DD_RequestedProcedureComments),
        new RTCEntry(0x0300a, 0x0266, TYPE_LO, "Brachy Accessory Device Name", DD_BrachyAccessoryDeviceName),
        new RTCEntry(0x00, 0x01005, TYPE_AT, "Attribute Identifier List", DD_AttributeIdentifierList),
        new RTCEntry(0x040, 0x0318, TYPE_CS, "Organ Exposed", DD_OrganExposed),
        new RTCEntry(0x0300a, 0x0264, TYPE_CS, "Brachy Accessory Device Type", DD_BrachyAccessoryDeviceType),
        new RTCEntry(0x0300a, 0x0263, TYPE_SH, "Brachy Accessory Device ID", DD_BrachyAccessoryDeviceID),
        new RTCEntry(0x00, 0x01002, TYPE_US, "Event Type ID", DD_EventTypeID),
        new RTCEntry(0x040, 0x0316, TYPE_DS, "Organ Dose", DD_OrganDose),
        new RTCEntry(0x0300a, 0x0262, TYPE_IS, "Brachy Accessory Device Number", DD_BrachyAccessoryDeviceNumber),
        new RTCEntry(0x00, 0x01001, TYPE_UI, "Requested SOP Instance UID", DD_RequestedSOPInstanceUID),
        new RTCEntry(0x00, 0x01000, TYPE_UI, "Affected SOP Instance UID", DD_AffectedSOPInstanceUID),
        new RTCEntry(0x040, 0x0314, TYPE_DS, "Half Value Layer", DD_HalfValueLayer),
        new RTCEntry(0x0300a, 0x0260, TYPE_SQ, "Brachy Accessory Device Sequence", DD_BrachyAccessoryDeviceSequence),
        new RTCEntry(0x040, 0x0312, TYPE_DS, "X-Ray Output", DD_XRayOutput),
        new RTCEntry(0x040, 0x0310, TYPE_ST, "Comments On Radiation Dose", DD_CommentsOnRadiationDose),
        new RTCEntry(0x028, 0x0109, TYPE_US, "Largest Pixel Value in Series", DD_LargestPixelValueInSeries),
        new RTCEntry(0x028, 0x0108, TYPE_US, "Smallest Pixel Value in Series", DD_SmallestPixelValueInSeries),
        new RTCEntry(0x018, 0x01201, TYPE_TM, "Time of Last Calibration", DD_TimeOfLastCalibration),
        new RTCEntry(0x028, 0x0107, TYPE_US, "Largest Image Pixel Value", DD_LargestImagePixelValue),
        new RTCEntry(0x018, 0x01200, TYPE_DA, "Date of Last Calibration", DD_DateOfLastCalibration),
        new RTCEntry(0x028, 0x0106, TYPE_US, "Smallest Image Pixel Value", DD_SmallestImagePixelValue),
        new RTCEntry(0x040, 0x030e, TYPE_SQ, "Exposure Dose Sequence", DD_ExposureDoseSequence),
        new RTCEntry(0x0300e, 0x08, TYPE_PN, "Reviewer Name", DD_ReviewerName),
        new RTCEntry(0x028, 0x0103, TYPE_US, "Pixel Representation", DD_PixelRepresentation),
        new RTCEntry(0x028, 0x0102, TYPE_US, "High Bit", DD_HighBit),
        new RTCEntry(0x0300e, 0x05, TYPE_TM, "Review Time", DD_ReviewTime),
        new RTCEntry(0x028, 0x0101, TYPE_US, "Bits Stored", DD_BitsStored),
        new RTCEntry(0x0300e, 0x04, TYPE_DA, "Review Date", DD_ReviewDate),
        new RTCEntry(0x028, 0x0100, TYPE_US, "Bits Allocated", DD_BitsAllocated),
        new RTCEntry(0x02130, 0x0c0, TYPE_SQ, "Original Image Sequence", DD_OriginalImageSequence),
        new RTCEntry(0x0300e, 0x02, TYPE_CS, "Approval Status", DD_ApprovalStatus),
        new RTCEntry(0x040, 0x0307, TYPE_DS, "Distance Source to Support", DD_DistanceSourceToSupport),
        new RTCEntry(0x040, 0x0306, TYPE_DS, "Distance Source to Entrance", DD_DistanceSourceToEntrance),
        new RTCEntry(0x0300a, 0x0250, TYPE_DS, "Total Reference Air Kerma", DD_TotalReferenceAirKerma),
        new RTCEntry(0x040, 0x0303, TYPE_US, "Exposed Area", DD_ExposedArea),
        new RTCEntry(0x040, 0x0302, TYPE_US, "Entrance Dose", DD_EntranceDose),
        new RTCEntry(0x040, 0x0301, TYPE_US, "Total Number Of Exposures", DD_TotalNumberOfExposures),
        new RTCEntry(0x040, 0x0300, TYPE_US, "Total Time of Flouroscopy", DD_TotalTimeOfFlouroscopy),
        new RTCEntry(0x06000, 0x052, TYPE_US, "Plane Origin", DD_PlaneOrigin),
        new RTCEntry(0x06000, 0x051, TYPE_US, "Image Frame Origin", DD_ImageFrameOrigin),
        new RTCEntry(0x06000, 0x050, TYPE_SS, "Overlay Origin", DD_OverlayOrigin),
        new RTCEntry(0x0300a, 0x0244, TYPE_LO, "Template Name", DD_TemplateName),
        new RTCEntry(0x0300a, 0x0242, TYPE_SH, "Template Type", DD_TemplateType),
        new RTCEntry(0x0300a, 0x0240, TYPE_IS, "Template Number", DD_TemplateNumber),
        new RTCEntry(0x04, 0x00, TYPE_UL, "File Set Group Length", DD_FileSetGroupLength),
        new RTCEntry(0x06000, 0x045, TYPE_CS, "Overlay Subtype", DD_OverlaySubtype),
        new RTCEntry(0x06000, 0x040, TYPE_CS, "Overlay Type", DD_OverlayType),
        new RTCEntry(0x0400, 0x0120, TYPE_OB, "Signature", DD_Signature),
        new RTCEntry(0x02130, 0x0a0, TYPE_SQ, "Proposed Study Sequence", DD_ProposedStudySequence),
        new RTCEntry(0x0300a, 0x0238, TYPE_LO, "Application Setup Manufacturer", DD_ApplicationSetupManufacturer),
        new RTCEntry(0x0300a, 0x0236, TYPE_LO, "Application Setup Name", DD_ApplicationSetupName),
        new RTCEntry(0x08, 0x0104, TYPE_LO, "Code Meaning", DD_CodeMeaning),
        new RTCEntry(0x0300a, 0x0234, TYPE_IS, "Application Setup Number", DD_ApplicationSetupNumber),
        new RTCEntry(0x08, 0x0102, TYPE_SH, "Coding Scheme Designator", DD_CodingSchemeDesignator),
        new RTCEntry(0x0300a, 0x0232, TYPE_CS, "Application Setup Type", DD_ApplicationSetupType),
        new RTCEntry(0x08, 0x0100, TYPE_SH, "Code Value", DD_CodeValue),
        new RTCEntry(0x0300a, 0x0230, TYPE_SQ, "Application Setup Sequence", DD_ApplicationSetupSequence),
        new RTCEntry(0x0300a, 0x022e, TYPE_TM, "Air Kerma Rate Reference Time", DD_AirKermaRateReferenceTime),
        new RTCEntry(0x0300a, 0x022c, TYPE_DA, "Air Kerma Rate Reference Date", DD_AirKermaRateReferenceDate),
        new RTCEntry(0x0400, 0x0115, TYPE_OB, "Certificate of Signer", DD_CertificateOfSigner),
        new RTCEntry(0x04008, 0x04000, TYPE_ST, "Results Comments", DD_ResultsComments),
        new RTCEntry(0x0300a, 0x022a, TYPE_DS, "Reference Air Kerma Rate", DD_ReferenceAirKermaRate),
        new RTCEntry(0x018, 0x01531, TYPE_DS, "Detector Secondary Angle", DD_DetectorSecondaryAngle),
        new RTCEntry(0x018, 0x01530, TYPE_DS, "Detector Primary Angle", DD_DetectorPrimaryAngle),
        new RTCEntry(0x0400, 0x0110, TYPE_CS, "Certificate Type", DD_CertificateType),
        new RTCEntry(0x0300a, 0x0228, TYPE_DS, "Source Isotope Half Life", DD_SourceIsotopeHalfLife),
        new RTCEntry(0x04, 0x01432, TYPE_UI, "Private Record UID", DD_PrivateRecordUID),
        new RTCEntry(0x0300a, 0x0226, TYPE_LO, "Source IsotopeName", DD_SourceIsotopeName),
        new RTCEntry(0x04, 0x01430, TYPE_CS, "Directory Record Type", DD_DirectoryRecordType),
        new RTCEntry(0x0300a, 0x0224, TYPE_DS, "Source Encapsulation Nominal Transmission",
                     DD_SourceEncapsulationNominalTransmission),
        new RTCEntry(0x038, 0x0500, TYPE_LO, "Patient State", DD_PatientState),
        new RTCEntry(0x0300a, 0x0222, TYPE_DS, "Source Encapsulation Nominal Thickness",
                     DD_SourceEncapsulationNominalThickness),
        new RTCEntry(0x06000, 0x022, TYPE_LO, "Overlay Description", DD_OverlayDescription),
        new RTCEntry(0x0400, 0x0105, TYPE_DT, "Digital Signature DateTime", DD_DigitalSignatureDateTime),
        new RTCEntry(0x0300a, 0x021a, TYPE_DS, "Active Source Length", DD_ActiveSourceLength),
        new RTCEntry(0x018, 0x01521, TYPE_DS, "Positioner Secondary Angle Increment",
                     DD_PositionerSecondaryAngleIncrement),
        new RTCEntry(0x018, 0x01520, TYPE_DS, "Positioner Primary Angle Increment", DD_PositionerPrimaryAngleIncrement),
        new RTCEntry(0x0400, 0x0100, TYPE_UI, "Digital Signature UID", DD_DigitalSignatureUID),
        new RTCEntry(0x0300a, 0x0218, TYPE_DS, "Active Source Diameter", DD_ActiveSourceDiameter),
        new RTCEntry(0x0300a, 0x0216, TYPE_LO, "Source Manufacturer", DD_SourceManufacturer),
        new RTCEntry(0x04, 0x01420, TYPE_UL, "Referenced Lower Level Directory Entity Offset",
                     DD_LowerLevelDirectoryOffset),
        new RTCEntry(0x0300a, 0x0214, TYPE_CS, "Source Type", DD_SourceType),
        new RTCEntry(0x06000, 0x01103, TYPE_US, "Overlay Descriptor - Blue", DD_OverlayDescriptorBlue),
        new RTCEntry(0x0300a, 0x0212, TYPE_IS, "Source Number", DD_SourceNumber),
        new RTCEntry(0x06000, 0x01102, TYPE_US, "Overlay Descriptor - Green", DD_OverlayDescriptorGreen),
        new RTCEntry(0x06000, 0x01101, TYPE_US, "Overlay Descriptor - Red", DD_OverlayDescriptorRed),
        new RTCEntry(0x0300a, 0x0210, TYPE_SQ, "Source Sequence", DD_SourceSequence),
        new RTCEntry(0x06000, 0x01100, TYPE_US, "Overlay Descriptor - Gray", DD_OverlayDescriptorGray),
        new RTCEntry(0x020, 0x0200, TYPE_UI, "Synchronization Frame of Reference", DD_SynchronizationFrameOfReference),
        new RTCEntry(0x0fffe, 0x0e0dd, TYPE_NONE, "Sequence Delimitation Item", DD_SequenceDelimitationItem),
        new RTCEntry(0x06000, 0x015, TYPE_IS, "Number of Frames in Overlay", DD_NumberOfFramesInOverlay),
        new RTCEntry(0x06000, 0x012, TYPE_US, "Overlay Planes", DD_OverlayPlanes),
        new RTCEntry(0x06000, 0x011, TYPE_US, "Overlay Columns", DD_OverlayColumns),
        new RTCEntry(0x06000, 0x010, TYPE_US, "Overlay Rows", DD_OverlayRows),
        new RTCEntry(0x018, 0x01511, TYPE_DS, "Positioner Secondary Angle", DD_PositionerSecondaryAngle),
        new RTCEntry(0x018, 0x01510, TYPE_DS, "Positioner Primary Angle", DD_PositionerPrimaryAngle),
        new RTCEntry(0x0300a, 0x0206, TYPE_SQ, "Treatment Machine Sequence", DD_TreatmentMachineSequence),
        new RTCEntry(0x04, 0x01410, TYPE_US, "Record In Use Flag", DD_RecordInUseFlag),
        new RTCEntry(0x0300a, 0x0202, TYPE_CS, "Brachy Treatment Type", DD_BrachyTreatmentType),
        new RTCEntry(0x018, 0x0605a, TYPE_FL, "Table of Parameter Values", DD_TableOfParameterValues),
        new RTCEntry(0x0300a, 0x0200, TYPE_CS, "Brachy Treatment Technique", DD_BrachyTreatmentTechnique),
        new RTCEntry(0x018, 0x01508, TYPE_CS, "Positioner Type", DD_PositionerType),
        new RTCEntry(0x02010, 0x0160, TYPE_US, "Reflected Ambient Light", DD_ReflectedAmbientLight),
        new RTCEntry(0x06000, 0x00, TYPE_UL, "Overlay Group Length", DD_OverlayGroupLength),
        new RTCEntry(0x018, 0x01500, TYPE_CS, "Positioner Motion", DD_PositionerMotion),
        new RTCEntry(0x02010, 0x015e, TYPE_US, "Illumination", DD_Illumination),
        new RTCEntry(0x018, 0x0604e, TYPE_US, "Pixel Component Data Type", DD_PixelComponentDataType),
        new RTCEntry(0x04, 0x01400, TYPE_UL, "Next Directory Record Offset", DD_NextDirectoryRecordOffset),
        new RTCEntry(0x018, 0x0604c, TYPE_US, "Pixel Component Physical Units", DD_PixelComponentPhysicalUnits),
        new RTCEntry(0x018, 0x0604a, TYPE_UL, "Pixel Component Range Stop", DD_PixelComponentRangeStop),
        new RTCEntry(0x03004, 0x074, TYPE_DS, "DVH Mean Dose", DD_DVHMeanDose),
        new RTCEntry(0x02010, 0x0154, TYPE_IS, "Maximum Collated Films", DD_MaximumCollatedFilms),
        new RTCEntry(0x03004, 0x072, TYPE_DS, "DVH Maximum Dose", DD_DVHMaximumDose),
        new RTCEntry(0x02010, 0x0152, TYPE_LT, "Configuration Information Description",
                     DD_ConfigurationInformationDescription),
        new RTCEntry(0x03004, 0x070, TYPE_DS, "DVH Minimum Dose", DD_DVHMinimumDose),
        new RTCEntry(0x02010, 0x0150, TYPE_ST, "Configuration Information", DD_ConfigurationInformation),
        new RTCEntry(0x018, 0x06058, TYPE_UL, "Table of Pixel Values", DD_TableOfPixelValues),
        new RTCEntry(0x018, 0x06056, TYPE_UL, "Number of Table Entries", DD_NumberOfTableEntries),
        new RTCEntry(0x028, 0x06040, TYPE_US, "R Wave Pointer", DD_RWavePointer),
        new RTCEntry(0x018, 0x06054, TYPE_FD, "Table of Y Break Points", DD_TableOfYBreakPoints),
        new RTCEntry(0x018, 0x06052, TYPE_UL, "Table of X Break Points", DD_TableOfXBreakPoints),
        new RTCEntry(0x018, 0x06050, TYPE_UL, "Number of Table Break Points", DD_NumberOfTableBreakPoints),
        new RTCEntry(0x018, 0x0603e, TYPE_UL, "TM-Line Position Y0", DD_TMLinePositionY0),
        new RTCEntry(0x018, 0x0603c, TYPE_UL, "TM-Line Position X0", DD_TMLinePositionX0),
        new RTCEntry(0x018, 0x0603a, TYPE_UL, "Doppler Sample Volume Y Position", DD_DopplerSampleVolumeYPosition),
        new RTCEntry(0x03004, 0x062, TYPE_CS, "DVH ROI Contribution Type", DD_DVHROIContributionType),
        new RTCEntry(0x03004, 0x060, TYPE_SQ, "DVH Referenced ROI Sequence", DD_DVHReferencedROISequence),
        new RTCEntry(0x02010, 0x0140, TYPE_CS, "Trim", DD_Trim),
        new RTCEntry(0x018, 0x06048, TYPE_UL, "Pixel Component Range Start", DD_PixelComponentRangeStart),
        new RTCEntry(0x018, 0x06046, TYPE_UL, "Pixel Component Mask", DD_PixelComponentMask),
        new RTCEntry(0x028, 0x06030, TYPE_US, "Mask Pointer", DD_MaskPointer),
        new RTCEntry(0x018, 0x06044, TYPE_US, "Pixel Component Organization", DD_PixelComponentOrganization),
        new RTCEntry(0x018, 0x06042, TYPE_UL, "TM-Line Position Y1", DD_TMLinePositionY1),
        new RTCEntry(0x018, 0x06040, TYPE_UL, "TM-Line Position X1", DD_TMLinePositionX1),
        new RTCEntry(0x03008, 0x0168, TYPE_TM, "Safe Position Return Time", DD_SafePositionReturnTime),
        new RTCEntry(0x03008, 0x0166, TYPE_DA, "Safe Position Return  Date", DD_SafePositionReturnDate),
        new RTCEntry(0x018, 0x0602e, TYPE_FD, "Physical Delta Y", DD_PhysicalDeltaY),
        new RTCEntry(0x03008, 0x0164, TYPE_TM, "Safe Position Exit Time", DD_SafePositionExitTime),
        new RTCEntry(0x018, 0x0602c, TYPE_FD, "Physical Delta X", DD_PhysicalDeltaX),
        new RTCEntry(0x03008, 0x0162, TYPE_DA, "Safe Position Exit Date", DD_SafePositionExitDate),
        new RTCEntry(0x03004, 0x058, TYPE_DS, "DVH Data", DD_DVHData),
        new RTCEntry(0x018, 0x0602a, TYPE_FD, "Reference Pixel Physical Value Y", DD_ReferencePixelPhysicalValueY),
        new RTCEntry(0x032, 0x035, TYPE_TM, "Study Read Time", DD_StudyReadTime),
        new RTCEntry(0x03008, 0x0160, TYPE_SQ, "Brachy Control Point Delivered Sequence",
                     DD_BrachyControlPointDeliveredSequence),
        new RTCEntry(0x032, 0x034, TYPE_DA, "Study Read Date", DD_StudyReadDate),
        new RTCEntry(0x03004, 0x056, TYPE_IS, "DVH Number of Bins", DD_DVHNumberOfBins),
        new RTCEntry(0x032, 0x033, TYPE_TM, "Study Verified Time", DD_StudyVerifiedTime),
        new RTCEntry(0x032, 0x032, TYPE_DA, "Study Verified Date", DD_StudyVerifiedDate),
        new RTCEntry(0x03004, 0x054, TYPE_CS, "DVH Volume Units", DD_DVHVolumeUnits),
        new RTCEntry(0x03004, 0x052, TYPE_DS, "DVH Dose Scaling", DD_DVHDoseScaling),
        new RTCEntry(0x03004, 0x050, TYPE_SQ, "DVH Sequence", DD_DVHSequence),
        new RTCEntry(0x040, 0x0a30a, TYPE_DS, "Numeric Value", DD_NumericValue),
        new RTCEntry(0x02010, 0x0130, TYPE_US, "Max Density", DD_MaxDensity),
        new RTCEntry(0x018, 0x06038, TYPE_UL, "Doppler Sample Volume X Position", DD_DopplerSampleVolumeXPosition),
        new RTCEntry(0x028, 0x06022, TYPE_LO, "Frame of Interest Description", DD_FrameOfInterestDescription),
        new RTCEntry(0x018, 0x06036, TYPE_FD, "Steering Angle", DD_SteeringAngle),
        new RTCEntry(0x028, 0x06020, TYPE_US, "Frame Numbers of Interest", DD_FrameNumbersOfInterest),
        new RTCEntry(0x018, 0x06034, TYPE_FD, "Doppler Correction Angle", DD_DopplerCorrectionAngle),
        new RTCEntry(0x018, 0x06032, TYPE_UL, "Pulse Repetition Frequency", DD_PulseRepetitionFrequency),
        new RTCEntry(0x018, 0x06031, TYPE_CS, "Transducer Type", DD_TransducerType),
        new RTCEntry(0x018, 0x06030, TYPE_UL, "Transducer Frequency", DD_TransducerFrequency),
        new RTCEntry(0x018, 0x0601e, TYPE_UL, "Region Location Max Y1", DD_RegionLocationMaxY1),
        new RTCEntry(0x018, 0x0601c, TYPE_UL, "Region Location Max X1", DD_RegionLocationMaxX1),
        new RTCEntry(0x03008, 0x0152, TYPE_IS, "Referenced Channel Shield Number", DD_ReferencedChannelShieldNumber),
        new RTCEntry(0x018, 0x0601a, TYPE_UL, "Region Location Min Y0", DD_RegionLocationMinY0),
        new RTCEntry(0x060, 0x03020, TYPE_UL, "Histogram Data", DD_HistogramData),
        new RTCEntry(0x03008, 0x0150, TYPE_SQ, "Recorded Channel Shield Sequence", DD_RecordedChannelShieldSequence),
        new RTCEntry(0x03004, 0x042, TYPE_DS, "DVH Normalization Dose Value", DD_DVHNormalizationDoseValue),
        new RTCEntry(0x03004, 0x040, TYPE_DS, "DVH Normalization Point", DD_DVHNormalizationPoint),
        new RTCEntry(0x02010, 0x0120, TYPE_US, "Min Density", DD_MinDensity),
        new RTCEntry(0x018, 0x06028, TYPE_FD, "Reference Pixel Physical Value X", DD_ReferencePixelPhysicalValueX),
        new RTCEntry(0x018, 0x06026, TYPE_US, "Physical Units Y Direction", DD_PhysicalUnitsYDirection),
        new RTCEntry(0x028, 0x06010, TYPE_US, "Representative Frame Number", DD_RepresentativeFrameNumber),
        new RTCEntry(0x018, 0x06024, TYPE_US, "Physical Units X Direction", DD_PhysicalUnitsXDirection),
        new RTCEntry(0x018, 0x06022, TYPE_SL, "Reference Pixel Y0", DD_ReferencePixelY0),
        new RTCEntry(0x032, 0x0c, TYPE_CS, "Study Priority ID", DD_StudyPriorityID),
        new RTCEntry(0x018, 0x06020, TYPE_SL, "Reference Pixel X0", DD_ReferencePixelX0),
        new RTCEntry(0x032, 0x0a, TYPE_CS, "Study Status ID", DD_StudyStatusID),
        new RTCEntry(0x03006, 0x0c8, TYPE_LO, "Frame of Reference Transformation Comment",
                     DD_FrameOfReferenceTransformationComment),
        new RTCEntry(0x03006, 0x0c6, TYPE_DS, "Frame of Reference Transformation Matrix",
                     DD_FrameOfReferenceTransformationMatrix),
        new RTCEntry(0x03008, 0x0142, TYPE_IS, "Referenced Source Applicator Number",
                     DD_ReferencedSourceApplicatorNumber),
        new RTCEntry(0x060, 0x03010, TYPE_LO, "Histogram Explanation", DD_HistogramExplanation),
        new RTCEntry(0x03006, 0x0c4, TYPE_CS, "Frame of Reference Transformation Type",
                     DD_FrameOfReferenceTransformationType),
        new RTCEntry(0x03008, 0x0140, TYPE_SQ, "Recorded Source Applicator Sequence",
                     DD_RecordedSourceApplicatorSequence),
        new RTCEntry(0x03006, 0x0c2, TYPE_UI, "Related Frame of Reference UID", DD_RelatedFrameOfReferenceUID),
        new RTCEntry(0x032, 0x012, TYPE_LO, "Study ID Issuer", DD_StudyIDIssuer),
        new RTCEntry(0x03006, 0x0c0, TYPE_SQ, "Frame of Reference Relationship Sequence",
                     DD_FrameOfReferenceRelationshipSequence),
        new RTCEntry(0x02010, 0x0110, TYPE_CS, "Empty Image Density", DD_EmptyImageDensity),
        new RTCEntry(0x018, 0x06018, TYPE_UL, "Region Location Min X0", DD_RegionLocationMinX0),
        new RTCEntry(0x018, 0x06016, TYPE_UL, "Region Flags", DD_RegionFlags),
        new RTCEntry(0x040, 0x04037, TYPE_PN, "Human Performer's Name", DD_HumanPerformersName),
        new RTCEntry(0x03008, 0x013c, TYPE_DS, "Delivered Pulse Repetition Interval",
                     DD_DeliveredPulseRepetitionInterval),
        new RTCEntry(0x040, 0x04036, TYPE_LO, "Human Performer's Organization", DD_HumanPerformersOrganization),
        new RTCEntry(0x018, 0x06014, TYPE_US, "Region Data Type", DD_RegionDataType),
        new RTCEntry(0x040, 0x04035, TYPE_SQ, "Actual Human Performers Sequence", DD_ActualHumanPerformersSequence),
        new RTCEntry(0x03008, 0x013a, TYPE_DS, "Specified Pulse Repetition Interval",
                     DD_SpecifiedPulseRepetitionInterval),
        new RTCEntry(0x040, 0x04034, TYPE_SQ, "Scheduled Human Performers Sequence",
                     DD_ScheduledHumanPerformersSequence),
        new RTCEntry(0x040, 0x04033, TYPE_SQ, "Output Information Sequence", DD_OutputInformationSequence),
        new RTCEntry(0x018, 0x06012, TYPE_US, "Region Spatial Format", DD_RegionSpatialFormat),
        new RTCEntry(0x040, 0x04032, TYPE_SQ, "Non-DICOM Output Code Sequence", DD_NonDICOMOutputCodeSequence),
        new RTCEntry(0x018, 0x06011, TYPE_SQ, "Sequence of Ultrasound Regions", DD_SequenceOfUltrasoundRegions),
        new RTCEntry(0x040, 0x04031, TYPE_SQ, "Requested Subsequent Workitem Code Sequence",
                     DD_RequestedSubsequentWorkitemCodeSequence),
        new RTCEntry(0x040, 0x04030, TYPE_SQ, "Performed Station Geographic Location Code Sequence",
                     DD_PerformedStationGeographicLocationCodeSequence),
        new RTCEntry(0x02130, 0x080, TYPE_SQ, "Presentation LUT Content Sequence", DD_PresentationLUTContentSequence),
        new RTCEntry(0x060, 0x03008, TYPE_US, "Histogram Bin Width", DD_HistogramBinWidth),
        new RTCEntry(0x03008, 0x0138, TYPE_IS, "Delivered Number of Pulses", DD_DeliveredNumberOfPulses),
        new RTCEntry(0x060, 0x03006, TYPE_US, "Histogram Last Bin Value", DD_HistogramLastBinValue),
        new RTCEntry(0x03008, 0x0136, TYPE_IS, "Specified Number of Pulses", DD_SpecifiedNumberOfPulses),
        new RTCEntry(0x060, 0x03004, TYPE_US, "Histogram First Bin Value", DD_HistogramFirstBinValue),
        new RTCEntry(0x03008, 0x0134, TYPE_DS, "Delivered Channel Total Time", DD_DeliveredChannelTotalTime),
        new RTCEntry(0x060, 0x03002, TYPE_US, "Histogram Number of Bins", DD_HistogramNumberOfBins),
        new RTCEntry(0x03008, 0x0132, TYPE_DS, "Specified Channel Total Time", DD_SpecifiedChannelTotalTime),
        new RTCEntry(0x060, 0x03000, TYPE_SQ, "Histogram Sequence", DD_HistogramSequence),
        new RTCEntry(0x03006, 0x0b4, TYPE_DS, "ROI Physical Property Value", DD_ROIPhysicalPropertyValue),
        new RTCEntry(0x03008, 0x0130, TYPE_SQ, "Recorded Channel Sequence", DD_RecordedChannelSequence),
        new RTCEntry(0x03006, 0x0b2, TYPE_CS, "ROI Physical Property", DD_ROIPhysicalProperty),
        new RTCEntry(0x03006, 0x0b0, TYPE_SQ, "ROI Physical Properties Sequence", DD_ROIPhysicalPropertiesSequence),
        new RTCEntry(0x032, 0x00, TYPE_UL, "Study Group Length", DD_StudyGroupLength),
        new RTCEntry(0x02010, 0x0100, TYPE_CS, "Border Density", DD_BorderDensity),
        new RTCEntry(0x040, 0x04029, TYPE_SQ, "Performed Station Class Code Sequence",
                     DD_PerformedStationClassCodeSequence),
        new RTCEntry(0x040, 0x04028, TYPE_SQ, "Performed Station Name Code Sequence",
                     DD_PerformedStationNameCodeSequence),
        new RTCEntry(0x040, 0x04027, TYPE_SQ, "Scheduled Station Geographic Location Code Sequence",
                     DD_ScheduledStationGeographicLocationCodeSequence),
        new RTCEntry(0x040, 0x04026, TYPE_SQ, "Scheduled Station Class Code Sequence",
                     DD_ScheduledStationClassCodeSequence),
        new RTCEntry(0x040, 0x04025, TYPE_SQ, "Scheduled Station Name Code Sequence",
                     DD_ScheduledStationNameCodeSequence),
        new RTCEntry(0x040, 0x04023, TYPE_UI, "Referenced General Purpose Scheduled Procedure Step Transaction UID",
                     DD_ReferencedGeneralPurposeScheduledProcedureStepTransactionUID),
        new RTCEntry(0x0fffa, 0x0fffa, TYPE_SQ, "Digital Signatures Sequence", DD_DigitalSignaturesSequence),
        new RTCEntry(0x040, 0x04022, TYPE_SQ, "Relevant Information Sequence", DD_RelevantInformationSequence),
        new RTCEntry(0x040, 0x04021, TYPE_SQ, "Input Information Sequence", DD_InputInformationSequence),
        new RTCEntry(0x018, 0x06000, TYPE_DS, "Sensitivity", DD_Sensitivity),
        new RTCEntry(0x040, 0x04020, TYPE_CS, "Input Availability Flag", DD_InputAvailabilityFlag),
        new RTCEntry(0x03006, 0x0a6, TYPE_PN, "ROI Interpreter", DD_ROIInterpreter),
        new RTCEntry(0x03008, 0x0122, TYPE_IS, "Referenced Brachy Accessory Device Number",
                     DD_ReferencedBrachyAccessoryDeviceNumber),
        new RTCEntry(0x03006, 0x0a4, TYPE_CS, "RT ROI Interpreted Type", DD_RTROIInterpretedType),
        new RTCEntry(0x03008, 0x0120, TYPE_SQ, "Recorded Brachy Accessory Device Sequence",
                     DD_RecordedBrachyAccessoryDeviceSequence),
        new RTCEntry(0x03006, 0x0a0, TYPE_SQ, "Related RT ROI Observations Sequence",
                     DD_RelatedRTROIObservationsSequence),
        new RTCEntry(0x03004, 0x012, TYPE_DS, "Dose Value", DD_DoseValue),
        new RTCEntry(0x03004, 0x010, TYPE_SQ, "RT Dose ROI Sequence", DD_RTDoseROISequence),
        new RTCEntry(0x040, 0x04019, TYPE_SQ, "Performed Workitem Code Sequence", DD_PerformedWorkitemCodeSequence),
        new RTCEntry(0x040, 0x04018, TYPE_SQ, "Scheduled Workitem Code Sequence", DD_ScheduledWorkitemCodeSequence),
        new RTCEntry(0x040, 0x04016, TYPE_SQ, "Referenced General Purpose Scheduled Procedure Step Sequence",
                     DD_ReferencedGeneralPurposeScheduledProcedureStepSequence),
        new RTCEntry(0x040, 0x04015, TYPE_SQ, "Resulting General Purpose Performed Procedure Steps Sequence",
                     DD_ResultingGeneralPurposePerformedProcedureStepsSequence),
        new RTCEntry(0x03004, 0x0e, TYPE_DS, "Dose Grid Scaling", DD_DoseGridScaling),
        new RTCEntry(0x040, 0x04011, TYPE_DT, "Expected Completion Date and Time", DD_ExpectedCompletionDateAndTime),
        new RTCEntry(0x03004, 0x0c, TYPE_DS, "GridFrame Offset Vector", DD_GridFrameOffsetVector),
        new RTCEntry(0x02130, 0x060, TYPE_SQ, "Image Overlay Box Content Sequence", DD_ImageOverlayBoxContentSequence),
        new RTCEntry(0x03004, 0x0a, TYPE_CS, "Dose Summation Type", DD_DoseSummationType),
        new RTCEntry(0x088, 0x00, TYPE_UL, "Storage Group Length", DD_StorageGroupLength),
        new RTCEntry(0x03008, 0x0116, TYPE_CS, "Application Setup  Check", DD_ApplicationSetupCheck),
        new RTCEntry(0x02120, 0x070, TYPE_SQ, "Referenced Print Job Sequence (in Queue Mx Module)",
                     DD_ReferencedPrintJobSequenceQueue),
        new RTCEntry(0x03004, 0x08, TYPE_DS, "Normalization Point", DD_NormalizationPoint),
        new RTCEntry(0x03008, 0x0110, TYPE_SQ, "Treatment Session Application Setup Sequence",
                     DD_TreatmentSessionApplicationSetupSequence),
        new RTCEntry(0x03004, 0x06, TYPE_LO, "Dose Comment", DD_DoseComment),
        new RTCEntry(0x03004, 0x04, TYPE_CS, "Dose Type", DD_DoseType),
        new RTCEntry(0x03004, 0x02, TYPE_CS, "Dose Units", DD_DoseUnits),
        new RTCEntry(0x03004, 0x01, TYPE_CS, "DVH Type", DD_DVHType),
        new RTCEntry(0x040, 0x04009, TYPE_SQ, "Human Performer Code Sequence", DD_HumanPerformerCodeSequence),
        new RTCEntry(0x040, 0x04007, TYPE_SQ, "Performed Processing Applications Code Sequence",
                     DD_PerformedProcessingApplicationsCodeSequence),
        new RTCEntry(0x040, 0x04006, TYPE_CS, "Multiple Copies Flag", DD_MultipleCopiesFlag),
        new RTCEntry(0x018, 0x011a2, TYPE_DS, "Compression Force", DD_CompressionForce),
        new RTCEntry(0x040, 0x04005, TYPE_DT, "Scheduled Procedure Step Start Date and Time",
                     DD_ScheduledProcedureStepStartDateAndTime),
        new RTCEntry(0x040, 0x04004, TYPE_SQ, "Scheduled Processing Applications Code Sequence",
                     DD_ScheduledProcessingApplicationsCodeSequence),
        new RTCEntry(0x018, 0x011a0, TYPE_DS, "Body Part Thickness", DD_BodyPartThickness),
        new RTCEntry(0x040, 0x04003, TYPE_CS, "General Purpose Scheduled Procedure Step Priority",
                     DD_GeneralPurposeScheduledProcedureStepPriority),
        new RTCEntry(0x040, 0x04002, TYPE_CS, "General Purpose Performed Procedure Step Status",
                     DD_GeneralPurposePerformedProcedureStepStatus),
        new RTCEntry(0x040, 0x04001, TYPE_CS, "General Purpose Scheduled Procedure Step Status",
                     DD_GeneralPurposeScheduledProcedureStepStatus),
        new RTCEntry(0x02130, 0x050, TYPE_SQ, "Annotation Content Sequence", DD_AnnotationContentSequence),
        new RTCEntry(0x03008, 0x0105, TYPE_LO, "Source Serial Number", DD_SourceSerialNumber),
        new RTCEntry(0x03008, 0x0100, TYPE_SQ, "Recorded Source Sequence", DD_RecordedSourceSequence),
        new RTCEntry(0x02130, 0x040, TYPE_SQ, "Image Box Content Sequence", DD_ImageBoxContentSequence),
        new RTCEntry(0x02120, 0x050, TYPE_SQ, "Print Job Description Sequence", DD_PrintJobDescriptionSequence),
        new RTCEntry(0x02130, 0x030, TYPE_SQ, "Film Box Content Sequence", DD_FilmBoxContentSequence),
        new RTCEntry(0x0300a, 0x01d6, TYPE_DS, "Table Top Lateral Setup Displacement",
                     DD_TableTopLateralSetupDisplacement),
        new RTCEntry(0x0300a, 0x01d4, TYPE_DS, "Table Top Longitudinal Setup Displacement",
                     DD_TableTopLongitudinalSetupDisplacement),
        new RTCEntry(0x0300a, 0x01d2, TYPE_DS, "Table Top Vertical Setup Displacement",
                     DD_TableTopVerticalSetupDisplacement),
        new RTCEntry(0x0300a, 0x01d0, TYPE_ST, "Setup ReferenceDescription", DD_SetupReferenceDescription),
        new RTCEntry(0x00, 0x05020, TYPE_SH, "End Message Set", DD_EndMessageSet),
        new RTCEntry(0x0300a, 0x01bc, TYPE_DS, "Setup Device Parameter", DD_SetupDeviceParameter),
        new RTCEntry(0x0300a, 0x01ba, TYPE_ST, "Setup Device Description", DD_SetupDeviceDescription),
        new RTCEntry(0x02130, 0x015, TYPE_SQ, "Printer Characteristics Sequence", DD_PrinterCharacteristicsSequence),
        new RTCEntry(0x00, 0x05010, TYPE_SH, "Message Set ID", DD_MessageSetID),
        new RTCEntry(0x02130, 0x010, TYPE_SQ, "Print Management Capabilities Sequence",
                     DD_PrintManagementCapabilitiesSequence),
        new RTCEntry(0x0300a, 0x01b8, TYPE_SH, "Setup Device Label", DD_SetupDeviceLabel),
        new RTCEntry(0x0300a, 0x01b6, TYPE_CS, "Setup Device Type", DD_SetupDeviceType),
        new RTCEntry(0x0300a, 0x01b4, TYPE_SQ, "Setup Device Sequence", DD_SetupDeviceSequence),
        new RTCEntry(0x018, 0x05212, TYPE_DS, "Image Translation Vector", DD_ImageTranslationVector),
        new RTCEntry(0x0300a, 0x01b2, TYPE_ST, "Setup TechniqueDescription", DD_SetupTechniqueDescription),
        new RTCEntry(0x018, 0x05210, TYPE_DS, "Image Transformation Matrix", DD_ImageTransformationMatrix),
        new RTCEntry(0x0300a, 0x01b0, TYPE_CS, "Setup Technique", DD_SetupTechnique),
        new RTCEntry(0x00, 0x0800, TYPE_US, "Data Set Type", DD_DataSetType),
        new RTCEntry(0x0300a, 0x01a8, TYPE_SH, "Shielding Device Position", DD_ShieldingDevicePosition),
        new RTCEntry(0x02120, 0x010, TYPE_CS, "Queue Status", DD_QueueStatus),
        new RTCEntry(0x0300a, 0x01a6, TYPE_ST, "Shielding Device Description", DD_ShieldingDeviceDescription),
        new RTCEntry(0x0300a, 0x01a4, TYPE_SH, "Shielding Device Label", DD_ShieldingDeviceLabel),
        new RTCEntry(0x04008, 0x0212, TYPE_CS, "Interpretation Status ID", DD_InterpretationStatusID),
        new RTCEntry(0x0300a, 0x01a2, TYPE_CS, "Shielding Device Type", DD_ShieldingDeviceType),
        new RTCEntry(0x04008, 0x0210, TYPE_CS, "Interpretation Type ID", DD_InterpretationTypeID),
        new RTCEntry(0x0300a, 0x01a0, TYPE_SQ, "Shielding Device Sequence", DD_ShieldingDeviceSequence),
        new RTCEntry(0x028, 0x03010, TYPE_SQ, "VOI LUT Sequence", DD_VOILUTSequence),
        new RTCEntry(0x054, 0x01220, TYPE_CS, "Secondary Counts Type", DD_SecondaryCountsType),
        new RTCEntry(0x04008, 0x0202, TYPE_LO, "Interpretation ID Issuer", DD_InterpretationIDIssuer),
        new RTCEntry(0x028, 0x01199, TYPE_UI, "Palette Color Lookup Table UID", DD_PaletteColorLookupTableUID),
        new RTCEntry(0x04008, 0x0200, TYPE_SH, "Interpretation ID", DD_InterpretationID),
        new RTCEntry(0x050, 0x020, TYPE_LO, "Device Description", DD_DeviceDescription),
        new RTCEntry(0x028, 0x03006, TYPE_US, "LUT Data", DD_LUTData),
        new RTCEntry(0x028, 0x03004, TYPE_LO, "Modality LUT Type", DD_ModalityLUTType),
        new RTCEntry(0x028, 0x03003, TYPE_LO, "LUT Explanation", DD_LUTExplanation),
        new RTCEntry(0x028, 0x03002, TYPE_US, "LUT Descriptor", DD_LUTDescriptor),
        new RTCEntry(0x028, 0x03000, TYPE_SQ, "Modality LUT Sequence", DD_ModalityLUTSequence),
        new RTCEntry(0x054, 0x01210, TYPE_DS, "Coincidence Window Width", DD_CoincidenceWindowWidth),
        new RTCEntry(0x03006, 0x088, TYPE_ST, "ROI Observation Description", DD_ROIObservationDescription),
        new RTCEntry(0x03006, 0x086, TYPE_SQ, "RT ROI Identification Code Sequence",
                     DD_RTROIIdentificationCodeSequence),
        new RTCEntry(0x03006, 0x085, TYPE_SH, "ROI Observation Label", DD_ROIObservationLabel),
        new RTCEntry(0x050, 0x019, TYPE_DS, "Inter Marker Distance", DD_InterMarkerDistance),
        new RTCEntry(0x03006, 0x084, TYPE_IS, "Referenced ROI Number", DD_ReferencedROINumber),
        new RTCEntry(0x050, 0x018, TYPE_DS, "Device Volume", DD_DeviceVolume),
        new RTCEntry(0x050, 0x017, TYPE_CS, "Device Diameter Units", DD_DeviceDiameterUnits),
        new RTCEntry(0x03006, 0x082, TYPE_IS, "Observation Number", DD_ObservationNumber),
        new RTCEntry(0x050, 0x016, TYPE_DS, "Device Diameter", DD_DeviceDiameter),
        new RTCEntry(0x03006, 0x080, TYPE_SQ, "RT ROI Observations Sequence", DD_RTROIObservationsSequence),
        new RTCEntry(0x050, 0x014, TYPE_DS, "Device Length", DD_DeviceLength),
        new RTCEntry(0x050, 0x010, TYPE_SQ, "Device Sequence", DD_DeviceSequence),
        new RTCEntry(0x018, 0x01191, TYPE_CS, "Anode Target Material", DD_AnodeTargetMaterial),
        new RTCEntry(0x018, 0x01190, TYPE_DS, "Focal Spot(s)", DD_FocalSpot),
        new RTCEntry(0x054, 0x01203, TYPE_DS, "Detector Element Size", DD_DetectorElementSize),
        new RTCEntry(0x054, 0x01202, TYPE_IS, "Transverse Mash", DD_TransverseMash),
        new RTCEntry(0x054, 0x01201, TYPE_IS, "Axial Mash", DD_AxialMash),
        new RTCEntry(0x040, 0x08ea, TYPE_SQ, "Measurement Units Code Sequence", DD_MeasurementUnitsCodeSequence),
        new RTCEntry(0x054, 0x01200, TYPE_DS, "Axial Acceptance", DD_AxialAcceptance),
        new RTCEntry(0x040, 0x0296, TYPE_SQ, "Billing Item Sequence", DD_BillingItemSequence),
        new RTCEntry(0x040, 0x0295, TYPE_SQ, "Measuring Units Sequence", DD_MeasuringUnitsSequence),
        new RTCEntry(0x040, 0x0294, TYPE_DS, "Quantity", DD_Quantity),
        new RTCEntry(0x040, 0x0293, TYPE_SQ, "Quantity Sequence", DD_QuantitySequence),
        new RTCEntry(0x050, 0x04, TYPE_CS, "Calibration Image", DD_CalibrationImage),
        new RTCEntry(0x050, 0x00, TYPE_UL, "Calibration Group Length", DD_CalibrationGroupLength),
        new RTCEntry(0x08, 0x01199, TYPE_SQ, "Referenced SOP Sequence", DD_ReferencedSOPSequence),
        new RTCEntry(0x018, 0x01184, TYPE_DS, "Y Focus Center", DD_YFocusCenter),
        new RTCEntry(0x08, 0x01198, TYPE_SQ, "Failed SOP Sequence", DD_FailedSOPSequence),
        new RTCEntry(0x018, 0x01183, TYPE_DS, "X Focus Center", DD_XFocusCenter),
        new RTCEntry(0x08, 0x01197, TYPE_US, "Failure Reason", DD_FailureReason),
        new RTCEntry(0x018, 0x01182, TYPE_IS, "Focal Distance", DD_FocalDistance),
        new RTCEntry(0x018, 0x01181, TYPE_CS, "Collimator Type", DD_CollimatorType),
        new RTCEntry(0x08, 0x01195, TYPE_UI, "Transaction UID", DD_TransactionUID),
        new RTCEntry(0x018, 0x01180, TYPE_SH, "Collimator/Grid Name", DD_CollimatorGridName),
        new RTCEntry(0x018, 0x095, TYPE_DS, "Pixel Bandwidth", DD_PixelBandwidth),
        new RTCEntry(0x018, 0x094, TYPE_DS, "Percent Phase Field of View", DD_PercentPhaseFieldOfView),
        new RTCEntry(0x040, 0x01010, TYPE_PN, "Names of Intended Recipients of Results",
                     DD_NamesOfIntendedRecipientsOfResults),
        new RTCEntry(0x018, 0x093, TYPE_DS, "Percent Sampling", DD_PercentSampling),
        new RTCEntry(0x018, 0x091, TYPE_IS, "Echo Train Length", DD_EchoTrainLength),
        new RTCEntry(0x018, 0x090, TYPE_DS, "Data Collection Diameter", DD_DataCollectionDiameter),
        new RTCEntry(0x054, 0x0101, TYPE_US, "Number Of Time Slices", DD_NumberOfTimeSlices),
        new RTCEntry(0x054, 0x0100, TYPE_US, "Time Slice Vector", DD_TimeSliceVector),
        new RTCEntry(0x040, 0x0281, TYPE_SQ, "Performed Procedure Step Discontinuation Reason Code Sequence",
                     DD_PerformedProcedureStepDiscontinuationReasonCodeSequence),
        new RTCEntry(0x040, 0x0280, TYPE_ST, "Comments on the Performed Procedure Step",
                     DD_CommentsOnPerformedProcedureStep),
        new RTCEntry(0x040, 0x01009, TYPE_SH, "Reporting Priority", DD_ReportingPriority),
        new RTCEntry(0x040, 0x01008, TYPE_LO, "Confidentiality Code", DD_ConfidentialityCode),
        new RTCEntry(0x018, 0x01170, TYPE_IS, "Generator Power", DD_GeneratorPower),
        new RTCEntry(0x018, 0x089, TYPE_IS, "Number of Phase Encoding Steps", DD_NumberOfPhaseEncodingSteps),
        new RTCEntry(0x040, 0x01005, TYPE_LO, "Requested Procedure Location", DD_RequestedProcedureLocation),
        new RTCEntry(0x018, 0x088, TYPE_DS, "Spacing Between Slices", DD_SpacingBetweenSlices),
        new RTCEntry(0x040, 0x01004, TYPE_LO, "Patient Transport Arrangements", DD_PatientTransportArrangements),
        new RTCEntry(0x018, 0x087, TYPE_DS, "Magnetic Field Strength", DD_MagneticFieldStrength),
        new RTCEntry(0x040, 0x01003, TYPE_SH, "Requested Procedure Priority", DD_RequestedProcedurePriority),
        new RTCEntry(0x018, 0x086, TYPE_IS, "Echo Number(s)", DD_EchoNumber),
        new RTCEntry(0x040, 0x01002, TYPE_LO, "Reason For Requested Procedure", DD_ReasonForRequestedProcedure),
        new RTCEntry(0x018, 0x085, TYPE_SH, "Imaged Nucleus", DD_ImagedNucleus),
        new RTCEntry(0x040, 0x01001, TYPE_SH, "Requested Procedure ID", DD_RequestedProcedureID),
        new RTCEntry(0x018, 0x084, TYPE_DS, "Imaging Frequency", DD_ImagingFrequency),
        new RTCEntry(0x018, 0x083, TYPE_DS, "Number of Averages", DD_NumberOfAverages),
        new RTCEntry(0x018, 0x082, TYPE_DS, "Inversion Time", DD_InversionTime),
        new RTCEntry(0x018, 0x0115e, TYPE_DS, "Image Area Dose Product", DD_ImageAreaDoseProduct),
        new RTCEntry(0x018, 0x081, TYPE_DS, "Echo Time", DD_EchoTime),
        new RTCEntry(0x018, 0x080, TYPE_DS, "Repetition Time", DD_RepetitionTime),
        new RTCEntry(0x08, 0x094, TYPE_SH, "Referring Physician's Telephone Numbers",
                     DD_ReferringPhysicianTelephoneNumber),
        new RTCEntry(0x018, 0x0115a, TYPE_CS, "Radiation Mode", DD_RadiationMode),
        new RTCEntry(0x08, 0x092, TYPE_ST, "Referring Physician's Address", DD_ReferringPhysicianAddress),
        new RTCEntry(0x040, 0x0275, TYPE_SQ, "Request Attributes Sequence", DD_RequestAttributesSequence),
        new RTCEntry(0x08, 0x090, TYPE_PN, "Referring Physician's Name", DD_ReferringPhysicianName),
        new RTCEntry(0x03008, 0x0e0, TYPE_SQ, "Treatment Summary Measured Dose Reference Sequence ",
                     DD_TreatmentSummaryMeasuredDoseReferenceSequence),
        new RTCEntry(0x03006, 0x050, TYPE_DS, "Contour Data", DD_ContourData),
        new RTCEntry(0x0fffe, 0x0e00d, TYPE_NONE, "Item Delimitation Item", DD_ItemDelimitationItem),
        new RTCEntry(0x040, 0x0270, TYPE_SQ, "Scheduled Step Attributes Sequence", DD_ScheduledStepAttributesSequence),
        new RTCEntry(0x018, 0x01166, TYPE_CS, "Grid", DD_Grid),
        new RTCEntry(0x018, 0x01164, TYPE_DS, "Imager Pixel Spacing", DD_ImagerPixelSpacing),
        new RTCEntry(0x018, 0x01162, TYPE_DS, "Intensifier Size", DD_IntensifierSize),
        new RTCEntry(0x018, 0x01161, TYPE_LO, "Type Of Filters", DD_TypeOfFilters),
        new RTCEntry(0x018, 0x01160, TYPE_SH, "Filter Type", DD_FilterType),
        new RTCEntry(0x018, 0x075, TYPE_IS, "Acquisition Termination Condition Data",
                     DD_AcquisitionTerminationConditionData),
        new RTCEntry(0x018, 0x074, TYPE_IS, "Acquisition Start Condition Data", DD_AcquisitionStartConditionData),
        new RTCEntry(0x018, 0x073, TYPE_CS, "Acquisition Start Condition", DD_AcquisitionStartCondition),
        new RTCEntry(0x018, 0x072, TYPE_DS, "Effective Series Duration", DD_EffectiveSeriesDuration),
        new RTCEntry(0x0fffe, 0x0e000, TYPE_NONE, "Item", DD_Item),
        new RTCEntry(0x018, 0x071, TYPE_CS, "Acquisition Termination Condition", DD_AcquisitionTerminationCondition),
        new RTCEntry(0x03006, 0x049, TYPE_IS, "Attached Contours", DD_AttachedContours),
        new RTCEntry(0x018, 0x070, TYPE_IS, "Counts Accumulated", DD_CountsAccumulated),
        new RTCEntry(0x03006, 0x048, TYPE_IS, "Contour Number", DD_ContourNumber),
        new RTCEntry(0x08, 0x082, TYPE_SQ, "Institution Code Sequence", DD_InstitutionCodeSequence),
        new RTCEntry(0x03006, 0x046, TYPE_IS, "Number of Contour Points", DD_NumberOfContourPoints),
        new RTCEntry(0x08, 0x081, TYPE_ST, "Institution Address", DD_InstitutionAddress),
        new RTCEntry(0x03006, 0x045, TYPE_DS, "Contour Offset Vector", DD_ContourOffsetVector),
        new RTCEntry(0x02010, 0x0a9, TYPE_CS, "Other Smoothing Types Available", DD_OtherSmoothingTypesAvailable),
        new RTCEntry(0x08, 0x080, TYPE_LO, "Institution Name", DD_InstitutionName),
        new RTCEntry(0x03006, 0x044, TYPE_DS, "Contour SlabT hickness", DD_ContourSlabThickness),
        new RTCEntry(0x02010, 0x0a8, TYPE_CS, "Default Smoothing Type", DD_DefaultSmoothingType),
        new RTCEntry(0x03008, 0x0d0, TYPE_SQ, "Recorded Block Sequence", DD_RecordedBlockSequence),
        new RTCEntry(0x02010, 0x0a7, TYPE_CS, "Other Magnification Types Available",
                     DD_OtherMagnificationTypesAvailable),
        new RTCEntry(0x03006, 0x042, TYPE_CS, "Contour Geometric Type", DD_ContourGeometricType),
        new RTCEntry(0x02010, 0x0a6, TYPE_CS, "Default Magnification Type", DD_DefaultMagnificationType),
        new RTCEntry(0x03006, 0x040, TYPE_SQ, "Contour Sequence", DD_ContourSequence),
        new RTCEntry(0x040, 0x0260, TYPE_SQ, "Performed Protocol Code Sequence", DD_PerformedProtocolCodeSequence),
        new RTCEntry(0x018, 0x01156, TYPE_CS, "Rectification Type", DD_RectificationType),
        new RTCEntry(0x018, 0x01155, TYPE_CS, "Radiation Setting", DD_RadiationSetting),
        new RTCEntry(0x018, 0x01154, TYPE_DS, "Average Pulse Width", DD_AveragePulseWidth),
        new RTCEntry(0x018, 0x01153, TYPE_IS, "Exposure in uAs", DD_ExposureInuAs),
        new RTCEntry(0x018, 0x01152, TYPE_IS, "Exposure", DD_Exposure),
        new RTCEntry(0x018, 0x01151, TYPE_IS, "X-ray Tube Current", DD_XrayTubeCurrent),
        new RTCEntry(0x018, 0x01150, TYPE_IS, "Exposure Time", DD_ExposureTime),
        new RTCEntry(0x08, 0x01160, TYPE_IS, "Referenced Frame Number", DD_ReferencedFrameNumber),
        new RTCEntry(0x028, 0x051, TYPE_CS, "Corrected Image", DD_CorrectedImage),
        new RTCEntry(0x03006, 0x039, TYPE_SQ, "ROI Contour Sequence", DD_ROIContourSequence),
        new RTCEntry(0x018, 0x060, TYPE_DS, "KVP", DD_KVP),
        new RTCEntry(0x03006, 0x038, TYPE_LO, "ROI Generation Description", DD_ROIGenerationDescription),
        new RTCEntry(0x03006, 0x036, TYPE_CS, "ROI Generation Algorithm", DD_ROIGenerationAlgorithm),
        new RTCEntry(0x05000, 0x0200e, TYPE_LT, "Audio Comments", DD_AudioComments),
        new RTCEntry(0x040, 0x0255, TYPE_LO, "Performed Procedure Type Description",
                     DD_PerformedProcedureTypeDescription),
        new RTCEntry(0x08, 0x070, TYPE_LO, "Manufacturer", DD_Manufacturer),
        new RTCEntry(0x040, 0x0254, TYPE_LO, "Performed Procedure Step Description",
                     DD_PerformedProcedureStepDescription),
        new RTCEntry(0x03006, 0x033, TYPE_CS, "RT ROI Relationship", DD_RTROIRelationship),
        new RTCEntry(0x03008, 0x0c0, TYPE_SQ, "Recorded Compensator Sequence", DD_RecordedCompensatorSequence),
        new RTCEntry(0x05000, 0x0200c, TYPE_OW, "Audio Sample Data", DD_AudioSampleData),
        new RTCEntry(0x040, 0x0253, TYPE_SH, "Performed Procedure Step ID", DD_PerformedProcedureStepID),
        new RTCEntry(0x040, 0x0252, TYPE_CS, "Performed Procedure Step Status", DD_PerformedProcedureStepStatus),
        new RTCEntry(0x08, 0x0115a, TYPE_UI, "SOP Classes Supported", DD_SOPClassesSupported),
        new RTCEntry(0x08, 0x02246, TYPE_SQ, "Transducer Orientation Modifier Sequence",
                     DD_TransducerOrientationModifierSequence),
        new RTCEntry(0x05000, 0x0200a, TYPE_UL, "Total Time", DD_TotalTime),
        new RTCEntry(0x040, 0x0251, TYPE_TM, "Performed Procedure Step End Time", DD_PerformedProcedureStepEndTime),
        new RTCEntry(0x03006, 0x030, TYPE_SQ, "RT Related ROI Sequence", DD_RTRelatedROISequence),
        new RTCEntry(0x040, 0x0250, TYPE_DA, "Performed Procedure Step End Date", DD_PerformedProcedureStepEndDate),
        new RTCEntry(0x08, 0x02244, TYPE_SQ, "Transducer Orientation Sequence", DD_TransducerOrientationSequence),
        new RTCEntry(0x02000, 0x0a8, TYPE_SQ, "Supported Image Display Formats Sequence",
                     DD_SupportedImageDisplayFormatsSequence),
        new RTCEntry(0x018, 0x01149, TYPE_IS, "Field of View Dimension(s)", DD_FieldOfViewDimensions),
        new RTCEntry(0x08, 0x02242, TYPE_SQ, "Transducer Position Modifier Sequence",
                     DD_TransducerPositionModifierSequence),
        new RTCEntry(0x018, 0x01147, TYPE_CS, "Field of View Shape", DD_FieldOfViewShape),
        new RTCEntry(0x08, 0x02240, TYPE_SQ, "Transducer Position Sequence", DD_TransducerPositionSequence),
        new RTCEntry(0x02000, 0x0a4, TYPE_SQ, "Other Media Available Sequence", DD_OtherMediaAvailableSequence),
        new RTCEntry(0x018, 0x01145, TYPE_DS, "Center of Rotation Offset", DD_CenterOfRotationOffset),
        new RTCEntry(0x018, 0x01144, TYPE_DS, "Angular Step", DD_AngularStep),
        new RTCEntry(0x02000, 0x0a2, TYPE_SQ, "Media Installed Sequence", DD_MediaInstalledSequence),
        new RTCEntry(0x018, 0x01143, TYPE_DS, "Scan Arc", DD_ScanArc),
        new RTCEntry(0x02000, 0x0a1, TYPE_US, "Printing Bit Depth", DD_PrintingBitDepth),
        new RTCEntry(0x018, 0x01142, TYPE_DS, "Radial Position", DD_RadialPosition),
        new RTCEntry(0x02000, 0x0a0, TYPE_US, "Memory Bit Depth", DD_MemoryBitDepth),
        new RTCEntry(0x018, 0x01141, TYPE_DS, "Angular Position", DD_AngularPosition),
        new RTCEntry(0x08, 0x01155, TYPE_UI, "Referenced SOP Instance UID", DD_ReferencedSOPInstanceUID),
        new RTCEntry(0x018, 0x01140, TYPE_CS, "Rotation Direction", DD_RotationDirection),
        new RTCEntry(0x03006, 0x02c, TYPE_DS, "ROI Volume", DD_ROIVolume),
        new RTCEntry(0x03006, 0x02a, TYPE_IS, "ROI Display Color", DD_ROIDisplayColor),
        new RTCEntry(0x08, 0x01150, TYPE_UI, "Referenced SOP Class UID", DD_ReferencedSOPClassUID),
        new RTCEntry(0x08, 0x068, TYPE_CS, "Presentation Intent Type", DD_PresentationIntentType),
        new RTCEntry(0x0300a, 0x0198, TYPE_SH, "Fixation Device Position", DD_FixationDevicePosition),
        new RTCEntry(0x018, 0x050, TYPE_DS, "Slice Thickness", DD_SliceThickness),
        new RTCEntry(0x0300a, 0x0196, TYPE_ST, "Fixation Device Description", DD_FixationDeviceDescription),
        new RTCEntry(0x08, 0x064, TYPE_CS, "Conversion Type", DD_ConversionType),
        new RTCEntry(0x03006, 0x028, TYPE_ST, "ROI Description", DD_ROIDescription),
        new RTCEntry(0x0300a, 0x0194, TYPE_SH, "Fixation Device Label", DD_FixationDeviceLabel),
        new RTCEntry(0x03006, 0x026, TYPE_LO, "ROI Name", DD_ROIName),
        new RTCEntry(0x018, 0x0113a, TYPE_CS, "Table Type", DD_TableType),
        new RTCEntry(0x08, 0x061, TYPE_CS, "Modalities In Study", DD_ModalitiesInStudy),
        new RTCEntry(0x0300a, 0x0192, TYPE_CS, "Fixation Device Type", DD_FixationDeviceType),
        new RTCEntry(0x040, 0x0245, TYPE_TM, "Performed Procedure Step Start Time", DD_PerformedProcedureStepStartTime),
        new RTCEntry(0x08, 0x060, TYPE_CS, "Modality", DD_Modality),
        new RTCEntry(0x03006, 0x024, TYPE_UI, "Referenced Frame of Reference UID", DD_ReferencedFrameOfReferenceUID),
        new RTCEntry(0x040, 0x0244, TYPE_DA, "Performed Procedure Step Start Date", DD_PerformedProcedureStepStartDate),
        new RTCEntry(0x03008, 0x0b0, TYPE_SQ, "Recorded Wedge Sequence", DD_RecordedWedgeSequence),
        new RTCEntry(0x0300a, 0x0190, TYPE_SQ, "Fixation Device Sequence", DD_FixationDeviceSequence),
        new RTCEntry(0x040, 0x0243, TYPE_SH, "Performed Location", DD_PerformedLocation),
        new RTCEntry(0x03006, 0x022, TYPE_IS, "ROI Number", DD_ROINumber),
        new RTCEntry(0x040, 0x0242, TYPE_SH, "Performed Station Name", DD_PerformedStationName),
        new RTCEntry(0x040, 0x0241, TYPE_AE, "Performed Station AE Title", DD_PerformedStationAETitle),
        new RTCEntry(0x03006, 0x020, TYPE_SQ, "Structure Set ROI Sequence", DD_StructureSetROISequence),
        new RTCEntry(0x018, 0x01138, TYPE_DS, "Table Angle", DD_TableAngle),
        new RTCEntry(0x018, 0x01137, TYPE_DS, "Table Longitudinal Increment", DD_TableLongitudinalIncrement),
        new RTCEntry(0x08, 0x02230, TYPE_SQ, "Primary Anatomic Structure Modifier Sequence",
                     DD_PrimaryAnatomicStructureModifierSequence),
        new RTCEntry(0x018, 0x01136, TYPE_DS, "Table Lateral Increment", DD_TableLateralIncrement),
        new RTCEntry(0x018, 0x01135, TYPE_DS, "Table Vertical Increment", DD_TableVerticalIncrement),
        new RTCEntry(0x018, 0x01134, TYPE_CS, "Table Motion", DD_TableMotion),
        new RTCEntry(0x0fffc, 0x0fffc, TYPE_OB, "Data Set Trailing Padding", DD_DataSetTrailingPadding),
        new RTCEntry(0x018, 0x01495, TYPE_IS, "Number of Tomosynthesis Source Images",
                     DD_NumberOfTomosynthesisSourceImages),
        new RTCEntry(0x05000, 0x02008, TYPE_UL, "Sample Rate", DD_SampleRate),
        new RTCEntry(0x018, 0x01131, TYPE_DS, "Table Traverse", DD_TableTraverse),
        new RTCEntry(0x08, 0x01145, TYPE_SQ, "Referenced Curve Sequence", DD_ReferencedCurveSequence),
        new RTCEntry(0x018, 0x01130, TYPE_DS, "Table Height", DD_TableHeight),
        new RTCEntry(0x05000, 0x02006, TYPE_UL, "Number of Samples", DD_NumberOfSamples),
        new RTCEntry(0x010, 0x02000, TYPE_LO, "Medical Alerts", DD_MedicalAlerts),
        new RTCEntry(0x018, 0x01491, TYPE_CS, "Tomo Class", DD_TomoClass),
        new RTCEntry(0x018, 0x01490, TYPE_CS, "Tomo Type", DD_TomoType),
        new RTCEntry(0x028, 0x034, TYPE_IS, "Pixel Aspect Ratio", DD_PixelAspectRatio),
        new RTCEntry(0x05000, 0x02004, TYPE_US, "Number of Channels", DD_NumberOfChannels),
        new RTCEntry(0x028, 0x032, TYPE_DS, "Zoom Center", DD_ZoomCenter),
        new RTCEntry(0x05000, 0x02002, TYPE_US, "Audio Sample Format", DD_AudioSampleFormat),
        new RTCEntry(0x08, 0x01140, TYPE_SQ, "Referenced Image Sequence", DD_ReferencedImageSequence),
        new RTCEntry(0x028, 0x031, TYPE_DS, "Zoom Factor", DD_ZoomFactor),
        new RTCEntry(0x040, 0x059a, TYPE_SQ, "Specimen Type Code Sequence", DD_SpecimenTypeCodeSequence),
        new RTCEntry(0x028, 0x030, TYPE_DS, "Pixel Spacing", DD_PixelSpacing),
        new RTCEntry(0x05000, 0x02000, TYPE_US, "Audio Type", DD_AudioType),
        new RTCEntry(0x08, 0x058, TYPE_UI, "Failed SOP Instance UID List", DD_FailedSOPInstanceUIDList),
        new RTCEntry(0x08, 0x056, TYPE_CS, "Instance Availability", DD_InstanceAvailability),
        new RTCEntry(0x018, 0x040, TYPE_IS, "Cine Rate", DD_CineRate),
        new RTCEntry(0x08, 0x054, TYPE_AE, "Retrieve AE Title", DD_RetrieveAETitle),
        new RTCEntry(0x0300a, 0x0184, TYPE_LO, "Patient Additional Position", DD_PatientAdditionalPosition),
        new RTCEntry(0x08, 0x052, TYPE_CS, "Query/Retrieve Level", DD_QueryRetrieveLevel),
        new RTCEntry(0x03006, 0x016, TYPE_SQ, "Contour Image Sequence", DD_ContourImageSequence),
        new RTCEntry(0x0300a, 0x0182, TYPE_IS, "Patient Setup Number", DD_PatientSetupNumber),
        new RTCEntry(0x08, 0x050, TYPE_SH, "Accession Number", DD_AccessionNumber),
        new RTCEntry(0x08, 0x02229, TYPE_SQ, "Anatomic Structure, Space or Region Sequence",
                     DD_AnatomicStructureSpaceOrRegionSequence),
        new RTCEntry(0x03006, 0x014, TYPE_SQ, "RT Referenced Series Sequence", DD_RTReferencedSeriesSequence),
        new RTCEntry(0x054, 0x0414, TYPE_SQ, "Patient Gantry Relationship Code Sequence",
                     DD_PatientGantryRelationshipCodeSequence),
        new RTCEntry(0x08, 0x02228, TYPE_SQ, "Primary Anatomic Structure Sequence",
                     DD_PrimaryAnatomicStructureSequence),
        new RTCEntry(0x03008, 0x0a0, TYPE_SQ, "Beam Limiting Device Leaf Pairs Sequence",
                     DD_BeamLimitingDeviceLeafPairsSequence),
        new RTCEntry(0x0300a, 0x0180, TYPE_SQ, "Patient Setup Sequence", DD_PatientSetupSequence),
        new RTCEntry(0x03006, 0x012, TYPE_SQ, "RT Referenced Study Sequence", DD_RTReferencedStudySequence),
        new RTCEntry(0x054, 0x0412, TYPE_SQ, "Patient Orientation Modifier Code Sequence",
                     DD_PatientOrientationModifierCodeSequence),
        new RTCEntry(0x03006, 0x010, TYPE_SQ, "Referenced Frame of Reference Sequence",
                     DD_ReferencedFrameOfReferenceSequence),
        new RTCEntry(0x054, 0x0410, TYPE_SQ, "Patient Orientation Code Sequence", DD_PatientOrientationCodeSequence),
        new RTCEntry(0x018, 0x02a, TYPE_SQ, "Additional Drug Sequence", DD_AdditionalDrugSequence),
        new RTCEntry(0x08, 0x02220, TYPE_SQ, "Anatomic Region Modifier Sequence", DD_AnatomicRegionModifierSequence),
        new RTCEntry(0x07fe0, 0x010, TYPE_OW, "Pixel Data", DD_PixelData),
        new RTCEntry(0x040, 0x02400, TYPE_LT, "Imaging Service Request Comments", DD_ImagingServiceRequestComments),
        new RTCEntry(0x018, 0x01121, TYPE_DS, "Gantry/Detector Slew", DD_GantryDetectorSlew),
        new RTCEntry(0x018, 0x01120, TYPE_DS, "Gantry/Detector Tilt", DD_GantryDetectorTilt),
        new RTCEntry(0x018, 0x039, TYPE_CS, "Therapy Description", DD_TherapyDescription),
        new RTCEntry(0x018, 0x01480, TYPE_DS, "Tomo Time", DD_TomoTime),
        new RTCEntry(0x018, 0x038, TYPE_CS, "Intervention Status", DD_InterventionStatus),
        new RTCEntry(0x018, 0x037, TYPE_CS, "Therapy Type", DD_TherapyType),
        new RTCEntry(0x08, 0x01130, TYPE_SQ, "Referenced Overlay Sequence", DD_ReferencedOverlaySequence),
        new RTCEntry(0x018, 0x036, TYPE_SQ, "Intervention Therapy Sequence", DD_InterventionTherapySequence),
        new RTCEntry(0x018, 0x035, TYPE_TM, "Intervention Drug Start Time", DD_InterventionDrugStartTime),
        new RTCEntry(0x018, 0x034, TYPE_LO, "Intervention Drug Name", DD_InterventionDrugName),
        new RTCEntry(0x018, 0x031, TYPE_LO, "Radiopharmaceutical", DD_Radiopharmaceutical),
        new RTCEntry(0x03006, 0x09, TYPE_TM, "Structure Set Time", DD_StructureSetTime),
        new RTCEntry(0x03006, 0x08, TYPE_DA, "Structure Set Date", DD_StructureSetDate),
        new RTCEntry(0x03006, 0x06, TYPE_ST, "Structure Set Description", DD_StructureSetDescription),
        new RTCEntry(0x03006, 0x04, TYPE_LO, "Structure Set Name", DD_StructureSetName),
        new RTCEntry(0x08, 0x02218, TYPE_SQ, "Anatomic Region Sequence", DD_AnatomicRegionSequence),
        new RTCEntry(0x03006, 0x02, TYPE_SH, "Structure Set Label", DD_StructureSetLabel),
        new RTCEntry(0x040, 0x0220, TYPE_SQ, "Referenced Non-Image Composite SOP Instance Sequence",
                     DD_ReferencedNonImageCompositeSOPInstanceSequence),
        new RTCEntry(0x054, 0x0400, TYPE_SH, "Image ID", DD_ImageID),
        new RTCEntry(0x028, 0x01103, TYPE_US, "Blue Palette Color Lookup Table Descriptor",
                     DD_BluePaletteColorLookupTableDescriptor),
        new RTCEntry(0x028, 0x01102, TYPE_US, "Green Palette Color Lookup Table Descriptor",
                     DD_GreenPaletteColorLookupTableDescriptor),
        new RTCEntry(0x028, 0x01101, TYPE_US, "Red Palette Color Lookup Table Descriptor",
                     DD_RedPaletteColorLookupTableDescriptor),
        new RTCEntry(0x07fe0, 0x00, TYPE_UL, "Pixel Data Group Length", DD_PixelDataGroupLength),
        new RTCEntry(0x018, 0x01114, TYPE_DS, "Estimated Radiographic Magnification Factor",
                     DD_EstimatedRadiographicMagnificationFactor),
        new RTCEntry(0x018, 0x01111, TYPE_DS, "Distance Source to Patient", DD_DistanceSourceToPatient),
        new RTCEntry(0x08, 0x01125, TYPE_SQ, "Referenced Visit Sequence", DD_ReferencedVisitSequence),
        new RTCEntry(0x018, 0x01110, TYPE_DS, "Distance Source to Detector", DD_DistanceSourceToDetector),
        new RTCEntry(0x018, 0x029, TYPE_SQ, "Intervention Drug Code Sequence", DD_InterventionDrugCodeSequence),
        new RTCEntry(0x018, 0x01470, TYPE_DS, "Tomo Angle", DD_TomoAngle),
        new RTCEntry(0x028, 0x014, TYPE_US, "Ultrasound Color Data Present", DD_UltrasoundColorDataPresent),
        new RTCEntry(0x018, 0x028, TYPE_DS, "Intervention Drug Dose", DD_InterventionDrugDose),
        new RTCEntry(0x018, 0x027, TYPE_TM, "Intervention Drug Stop Time", DD_InterventionDrugStopTime),
        new RTCEntry(0x028, 0x012, TYPE_US, "Planes", DD_Planes),
        new RTCEntry(0x08, 0x01120, TYPE_SQ, "Referenced Patient Sequence", DD_ReferencedPatientSequence),
        new RTCEntry(0x018, 0x026, TYPE_SQ, "Intervention Drug Information Sequence",
                     DD_InterventionDrugInformationSequence),
        new RTCEntry(0x028, 0x011, TYPE_US, "Columns", DD_Columns),
        new RTCEntry(0x018, 0x025, TYPE_CS, "Angio Flag", DD_AngioFlag),
        new RTCEntry(0x028, 0x010, TYPE_US, "Rows", DD_Rows),
        new RTCEntry(0x018, 0x024, TYPE_SH, "Sequence Name", DD_SequenceName),
        new RTCEntry(0x018, 0x023, TYPE_CS, "MR Acquisition Type", DD_MRAcquisitionType),
        new RTCEntry(0x018, 0x022, TYPE_CS, "Scan Options", DD_ScanOptions),
        new RTCEntry(0x018, 0x021, TYPE_CS, "Sequence Variant", DD_SequenceVariant),
        new RTCEntry(0x08, 0x035, TYPE_TM, "Curve Time", DD_CurveTime),
        new RTCEntry(0x018, 0x020, TYPE_CS, "Scanning Sequence", DD_ScanningSequence),
        new RTCEntry(0x088, 0x0912, TYPE_LO, "Topic Key Words", DD_TopicKeyWords),
        new RTCEntry(0x08, 0x034, TYPE_TM, "Overlay Time", DD_OverlayTime),
        new RTCEntry(0x08, 0x033, TYPE_TM, "Content (formerly Image) Time", DD_ContentTime),
        new RTCEntry(0x088, 0x0910, TYPE_LO, "Topic Author", DD_TopicAuthor),
        new RTCEntry(0x08, 0x032, TYPE_TM, "Acquisition Time", DD_AcquisitionTime),
        new RTCEntry(0x08, 0x031, TYPE_TM, "Series Time", DD_SeriesTime),
        new RTCEntry(0x08, 0x030, TYPE_TM, "Study Time", DD_StudyTime),
        new RTCEntry(0x028, 0x09, TYPE_AT, "Frame Increment Pointer", DD_FrameIncrementPointer),
        new RTCEntry(0x028, 0x08, TYPE_IS, "Number of Frames", DD_NumberOfFrames),
        new RTCEntry(0x08, 0x01115, TYPE_SQ, "Referenced Series Sequence", DD_ReferencedSeriesSequence),
        new RTCEntry(0x018, 0x01100, TYPE_DS, "Reconstruction Diameter", DD_ReconstructionDiameter),
        new RTCEntry(0x028, 0x06, TYPE_US, "Planar Configuration", DD_PlanarConfiguration),
        new RTCEntry(0x018, 0x01460, TYPE_DS, "Tomo Layer Height", DD_TomoLayerHeight),
        new RTCEntry(0x028, 0x04, TYPE_CS, "Photometric Interpretation", DD_PhotometricInterpretation),
        new RTCEntry(0x08, 0x01111, TYPE_SQ, "Referenced Performed Procedure Step Sequence",
                     DD_ReferencedPerformedProcedureStepSequence),
        new RTCEntry(0x028, 0x02, TYPE_US, "Samples per Pixel", DD_SamplesPerPixel),
        new RTCEntry(0x08, 0x01110, TYPE_SQ, "Referenced Study Sequence", DD_ReferencedStudySequence),
        new RTCEntry(0x032, 0x04000, TYPE_LT, "Study Comments", DD_StudyComments),
        new RTCEntry(0x018, 0x015, TYPE_CS, "Body Part Examined", DD_BodyPartExamined),
        new RTCEntry(0x028, 0x00, TYPE_UL, "Image Presentation Group Length", DD_ImagePresentationGroupLength),
        new RTCEntry(0x018, 0x014, TYPE_SQ, "Contrast/Bolus Administration Route Sequence",
                     DD_ContrastBolusAdministrationRouteSequence),
        new RTCEntry(0x088, 0x0906, TYPE_ST, "Topic Subject", DD_TopicSubject),
        new RTCEntry(0x018, 0x012, TYPE_SQ, "Contrast/Bolus Agent Sequence", DD_ContrastBolusAgentSequence),
        new RTCEntry(0x088, 0x0904, TYPE_LO, "Topic Title", DD_TopicTitle),
        new RTCEntry(0x08, 0x025, TYPE_DA, "Curve Date", DD_CurveDate),
        new RTCEntry(0x018, 0x010, TYPE_LO, "Contrast/Bolus Agent", DD_ContrastBolusAgent),
        new RTCEntry(0x08, 0x024, TYPE_DA, "Overlay Date", DD_OverlayDate),
        new RTCEntry(0x08, 0x023, TYPE_DA, "Content (formerly Image) Date", DD_ContentDate),
        new RTCEntry(0x08, 0x022, TYPE_DA, "Acquisition Date", DD_AcquisitionDate),
        new RTCEntry(0x08, 0x021, TYPE_DA, "Series Date", DD_SeriesDate),
        new RTCEntry(0x08, 0x020, TYPE_DA, "Study Date", DD_StudyDate),
        new RTCEntry(0x018, 0x01450, TYPE_DS, "Column Angulation", DD_ColumnAngulation),
        new RTCEntry(0x08, 0x01100, TYPE_SQ, "Referenced Results Sequence", DD_ReferencedResultsSequence),
        new RTCEntry(0x08, 0x018, TYPE_UI, "SOP Instance UID", DD_SOPInstanceUID),
        new RTCEntry(0x08, 0x016, TYPE_UI, "SOP Class UID", DD_SOPClassUID),
        new RTCEntry(0x018, 0x00, TYPE_UL, "Acquisition Group Length", DD_AcquisitionGroupLength),
        new RTCEntry(0x08, 0x014, TYPE_UI, "Instance Creator UID", DD_InstanceCreatorUID),
        new RTCEntry(0x08, 0x013, TYPE_TM, "Instance Creation Time", DD_InstanceCreationTime),
        new RTCEntry(0x08, 0x012, TYPE_DA, "Instance Creation Date", DD_InstanceCreationDate),
        new RTCEntry(0x040, 0x0556, TYPE_ST, "Acquisition Context Description", DD_AcquisitionContextDescription),
        new RTCEntry(0x040, 0x0555, TYPE_SQ, "Acquisition Context Sequence", DD_AcquisitionContextSequence),
        new RTCEntry(0x040, 0x0551, TYPE_LO, "Specimen Identifier", DD_SpecimenIdentifier),
        new RTCEntry(0x040, 0x0550, TYPE_SQ, "Specimen Sequence", DD_SpecimenSequence),
        new RTCEntry(0x0400, 0x020, TYPE_AT, "Data Elements Signed", DD_DataElementsSigned),
        new RTCEntry(0x08, 0x08, TYPE_CS, "Image Type", DD_ImageType),
        new RTCEntry(0x08, 0x05, TYPE_CS, "Specific Character Set", DD_SpecificCharacterSet),
        new RTCEntry(0x0300a, 0x0134, TYPE_DS, "Cumulative Meterset Weight", DD_CumulativeMetersetWeight),
        new RTCEntry(0x08, 0x00, TYPE_UL, "Identifying Group Length", DD_IdentifyingGroupLength),
        new RTCEntry(0x0300a, 0x0130, TYPE_DS, "Source to Surface Distance", DD_SourceToSurfaceDistance),
        new RTCEntry(0x0300a, 0x012e, TYPE_DS, "Surface Entry Point", DD_SurfaceEntryPoint),
        new RTCEntry(0x0300a, 0x012c, TYPE_DS, "Isocenter Position", DD_IsocenterPosition),
        new RTCEntry(0x020, 0x01209, TYPE_IS, "Number of Series Related Instances", DD_NumberOfSeriesRelatedInstances),
        new RTCEntry(0x0400, 0x015, TYPE_CS, "MAC Algorithm", DD_MACAlgorithm),
        new RTCEntry(0x0300a, 0x012a, TYPE_DS, "Table Top Lateral Position", DD_TableTopLateralPosition),
        new RTCEntry(0x020, 0x01208, TYPE_IS, "Number of Study Related Instances", DD_NumberOfStudyRelatedInstances),
        new RTCEntry(0x020, 0x01206, TYPE_IS, "Number of Study Related Series", DD_NumberOfStudyRelatedSeries),
        new RTCEntry(0x020, 0x01204, TYPE_IS, "Number of Patient Related Instances",
                     DD_NumberOfPatientRelatedInstances),
        new RTCEntry(0x0400, 0x010, TYPE_UI, "MAC Calculation Transfer Syntax UID", DD_MACCalculationTransferSyntaxUID),
        new RTCEntry(0x020, 0x01202, TYPE_IS, "Number of Patient Related Series", DD_NumberOfPatientRelatedSeries),
        new RTCEntry(0x0300a, 0x0129, TYPE_DS, "Table Top Longitudinal Position", DD_TableTopLongitudinalPosition),
        new RTCEntry(0x020, 0x01200, TYPE_IS, "Number of Patient Related Studies", DD_NumberOfPatientRelatedStudies),
        new RTCEntry(0x0300a, 0x0128, TYPE_DS, "Table Top Vertical Position", DD_TableTopVerticalPosition),
        new RTCEntry(0x032, 0x01070, TYPE_LO, "Requested Contrast Agent", DD_RequestedContrastAgent),
        new RTCEntry(0x0300a, 0x0126, TYPE_CS, "Table Top Eccentric Rotation Direction",
                     DD_TableTopEccentricRotationDirection),
        new RTCEntry(0x0300a, 0x0125, TYPE_DS, "Table Top Eccentric Angle", DD_TableTopEccentricAngle),
        new RTCEntry(0x0300a, 0x0124, TYPE_DS, "Table Top Eccentric Axis Distance", DD_TableTopEccentricAxisDistance),
        new RTCEntry(0x038, 0x0400, TYPE_LO, "Patient's Institution Residence", DD_PatientInstitutionResidence),
        new RTCEntry(0x0300a, 0x0123, TYPE_CS, "Patient Support Rotation Direction",
                     DD_PatientSupportRotationDirection),
        new RTCEntry(0x0300a, 0x0122, TYPE_DS, "Patient Support Angle", DD_PatientSupportAngle),
        new RTCEntry(0x0300a, 0x0121, TYPE_CS, "Beam Limiting Device Rotation Direction",
                     DD_BeamLimitingDeviceRotationDirection),
        new RTCEntry(0x0300a, 0x0120, TYPE_DS, "Beam Limiting Device Angle", DD_BeamLimitingDeviceAngle),
        new RTCEntry(0x020, 0x0110, TYPE_DS, "Temporal Resolution", DD_TemporalResolution)
    }; // End RTCEntry[]

    /** DOCUMENT ME! */
    static int lastSearchGroup = 0;

    /** DOCUMENT ME! */
    static int lastSearchElement = 0;

    /** DOCUMENT ME! */
    static int lastSearchIndex = 0;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets the description of a given integer hash index.
     *
     * <p>for example: getElement( RTC.DD_PatientID ) returns "Patient's ID"</p>
     *
     * @param   dd_type  the integer hash index to use (see huge list of DD_XXX constants)
     *
     * @return  the description string
     */
    public static final String getDescription(int dd_type) {
        int i;

        return (((i = getIndex(dd_type)) >= 0) ? rtcList[i].description : "Unknown");
    }

    /**
     * Gets the element number of a given integer hash index.
     *
     * <p>for example: getElement( RTC.DD_PatientID ) returns 0x0020</p>
     *
     * @param   dd_type  the integer hash index to use (see huge list of DD_XXX constants)
     *
     * @return  the element number
     */
    public static final int getElement(int dd_type) {
        int i;

        return (((i = getIndex(dd_type)) >= 0) ? rtcList[i].element : -1);
    }

    /**
     * Gets the group number of a given integer hash index.
     *
     * <p>for example: getGroup( RTC.DD_PatientID ) returns 0x0010</p>
     *
     * @param   dd_type  the integer hash index to use (see huge list of DD_XXX constants)
     *
     * @return  the group number
     */
    public static final int getGroup(int dd_type) {
        int i;

        return (((i = getIndex(dd_type)) >= 0) ? rtcList[i].group : -1);
    }

    /**
     * Gets the type code of a given integer hash index.
     *
     * <p>for example: getTypeCode( RTC.DD_PatientID ) returns RTC.TYPE_LO</p>
     *
     * @param   dd_type  the integer hash index to use (see huge list of DD_XXX constants)
     *
     * @return  the type code (see list of TYPE_XXX constants)
     */
    public static final int getTypeCode(int dd_type) {
        int i;

        return (((i = getIndex(dd_type)) >= 0) ? rtcList[i].typeCode : TYPE_UNKNOWN);
    }

    /**
     * gets the type code name of a given integer hash index for example: getTypeCodeName( RTC.DD_PatientID ) returns
     * "LO"
     *
     * @param   dd_type  the integer hash index to use (see huge list of DD_XXX constants)
     *
     * @return  the type code name string
     */
    public static String getTypeCodeName(int dd_type) {
        String s = null;
        int i = getIndex(dd_type);

        if (i >= 0) {

            if (!typeCodeNamesFilled) {
                typeCodeNames.put(new Integer(TYPE_CS), "CS");
                typeCodeNames.put(new Integer(TYPE_DS), "DS");
                typeCodeNames.put(new Integer(TYPE_SQ), "SQ");
                typeCodeNames.put(new Integer(TYPE_US), "US");
                typeCodeNames.put(new Integer(TYPE_LO), "LO");
                typeCodeNames.put(new Integer(TYPE_IS), "IS");
                typeCodeNames.put(new Integer(TYPE_LT), "LT");
                typeCodeNames.put(new Integer(TYPE_TM), "TM");
                typeCodeNames.put(new Integer(TYPE_SH), "SH");
                typeCodeNames.put(new Integer(TYPE_DA), "DA");
                typeCodeNames.put(new Integer(TYPE_PN), "PN");
                typeCodeNames.put(new Integer(TYPE_ST), "ST");
                typeCodeNames.put(new Integer(TYPE_AE), "AE");
                typeCodeNames.put(new Integer(TYPE_AT), "AT");
                typeCodeNames.put(new Integer(TYPE_OB), "OB");
                typeCodeNames.put(new Integer(TYPE_UL), "UL");
                typeCodeNames.put(new Integer(TYPE_UI), "UI");
                typeCodeNames.put(new Integer(TYPE_SS), "SS");
                typeCodeNames.put(new Integer(TYPE_OW), "OW");
                typeCodeNames.put(new Integer(TYPE_UT), "UT");
                typeCodeNames.put(new Integer(TYPE_FL), "FL");
                typeCodeNames.put(new Integer(TYPE_AS), "AS");
                typeCodeNames.put(new Integer(TYPE_DT), "DT");
                typeCodeNames.put(new Integer(TYPE_NONE), "NONE");
                typeCodeNames.put(new Integer(TYPE_FD), "FD");
                typeCodeNames.put(new Integer(TYPE_SL), "SL");
                typeCodeNamesFilled = true;
            }

            s = (String) typeCodeNames.get(new Integer(rtcList[i].typeCode));
        }

        if (s == null) {
            s = "??";
        }

        return s;
    }

    /**
     * hash a group, element pair that is not already present in our list.
     *
     * @param   g  DOCUMENT ME!
     * @param   e  DOCUMENT ME!
     *
     * @return  a dd_type integer hash index for use with the other methods here.
     */
    public static int unknownDDType(int g, int e) {

        if (e == 0x0000) {
            return (DD_GroupLength); // handled special
        }

        if ((g == lastSearchGroup) && (e == lastSearchElement)) {

            // we just looked up this one... no need to repeat...
            return (DD_LASTSEARCHED);
        } else {

            // do an exhaustive search...
            for (int i = 0; i < rtcList.length; i++) {

                if (g == rtcList[i].group) {

                    if (e == rtcList[i].element) {
                        lastSearchGroup = g;
                        lastSearchElement = e;
                        lastSearchIndex = i;

                        int dd_type = rtcList[i].dd_type;

                        if (dd_type == DD_UNDEFINED) {
                            dd_type = DD_LASTSEARCHED;
                        }

                        return (dd_type);
                    }
                }
            }
        }

        return (DD_UNDEFINED);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  g        DOCUMENT ME!
     * @param  e        DOCUMENT ME!
     * @param  dd_type  DOCUMENT ME!
     */
    private static void addDDTypeHash(int g, int e, int dd_type) {

        if (unknownDDType(g, e) == DD_LASTSEARCHED) {
            ddTypeIndexes.put(new Integer(dd_type), new Integer(lastSearchIndex));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   dd_type  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  RuntimeException  DOCUMENT ME!
     */
    private static int getIndex(int dd_type) {
        Integer index;

        // did the caller just do a search?
        if (dd_type == DD_LASTSEARCHED) {
            return (lastSearchIndex);
        }

        // shouldn't be this...
        if (dd_type == DD_UNDEFINED) {
            return (-1);
        }

        // is it in hash?
        index = (Integer) ddTypeIndexes.get(new Integer(dd_type));

        if (index != null) {
            return (index.intValue());
        }

        // no? well, then put it there...
        for (int i = 0; i < rtcList.length; i++) {

            if (rtcList[i].dd_type == dd_type) {
                addDDTypeHash(rtcList[i].group, rtcList[i].element, dd_type);

                return (i);
            }
        }

        // user gave us an unknown dd_type ?!?
        throw new RuntimeException("RTC data dictionary hash failure         (passed type " + dd_type + ")");
    }
} // end RTC
