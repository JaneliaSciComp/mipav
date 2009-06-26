package gov.nih.mipav.model.file.xcede;

import java.util.Vector;
import java.util.Enumeration;

import javax.swing.tree.TreeNode;

/**
 * This <code>XCEDEElement</code> represents the general element of the XCEDE xml file.
 * Right now I only think Project, Subject, Visit, Study and Series to be nodes, and
 * others to be leaves.
 * @author Hailong Wang, Ph.D
 * @version 1.0, 04/24/2006
 *
 */
public class XCEDEElement extends Element implements TreeNode{
    /**
     * XCEDE top level element constants.
     */
    public static final String XCEDE_ROOT_ELEMENT_PROJECTLEVEL = "projectlevel";
    
    public static final String XCEDE_ROOT_ELEMENT_SUBJECTLEVEL = "subjectlevel";
    
    public static final String XCEDE_ROOT_ELEMENT_VISITLEVEL = "visitlevel";
    
    public static final String XCEDE_ROOT_ELEMENT_STUDYLEVEL = "studylevel";
    
    public static final String XCEDE_ROOT_ELEMENT_SERIESLEVEL = "serieslevel";
    
    /**
     * XCEDE non top level element constants.
     */
    public static final String XCEDE_ELEMENT_PROJECT = "project";
    
    public static final String XCEDE_ELEMENT_SUBJECT = "subject";
    
    public static final String XCEDE_ELEMENT_VISIT = "visit";
    
    public static final String XCEDE_ELEMENT_STUDY = "study";
    
    public static final String XCEDE_ELEMENT_SERIES = "series";
    
    public static final String XCEDE_ELEMENT_ID = "id";
    
    public static final String XCEDE_ELEMENT_DESCRIPTION = "description";
    
    public static final String XCEDE_ELEMENT_FUNDING = "funding";
   
    public static final String XCEDE_ELEMENT_BIRTHDATE = "birthDate";
    
    public static final String XCEDE_ELEMENT_DEATHDATE = "deathDate";
    
    public static final String XCEDE_ELEMENT_NAME = "name";
    
    public static final String XCEDE_ELEMENT_SEX = "sex";
    
    public static final String XCEDE_ELEMENT_SPECIES = "species"; 
    
    public static final String XCEDE_ELEMENT_EXTENDEDDESCRIPTOR = "extendedDescriptor";
    
    public static final String XCEDE_ELEMENT_LOCATION = "location";
    
    public static final String XCEDE_ELEMENT_SUBJECTVAR = "subjectVar";
    
    public static final String XCEDE_ELEMENT_SCANNER = "scanner";
    
    public static final String XCEDE_ELEMENT_EXPPROTOCOL = "expProtocol";
    
    public static final String XCEDE_ELEMENT_ACQPROTOCOL = "acqProtocol";
    
    public static final String XCEDE_ELEMENT_DATAREC = "datarec";

    public static final String XCEDE_ELEMENT_AGE = "age";
    public static final String XCEDE_ELEMENT_AGE_VALUE = "value";
    
    public static final String XCEDE_ATTR_AGETYPE = "agetype";
    
    public static final String XCEDE_ATTR_UNITS = "units";
    
    public static final String XCEDE_ELEMENT_ASSESSMENT = "assessment";
    
    public static final String XCEDE_ELEMENT_ANNOTATION = "annotation";
    
    public static final String XCEDE_ELEMENT_MANUFACTURER = "manufacturer";
    
    public static final String XCEDE_ELEMENT_MODEL = "model";
    
    public static final String XCEDE_ELEMENT_ADDITIONALEQUIPMENT = "additionalEquipment";
    
    public static final String XCEDE_ELEMENT_FILENAME = "filename";
    
    public static final String XCEDE_ELEMENT_FILEOFFSET = "fileoffset";
    
    public static final String XCEDE_ELEMENT_FILERECORDSIZE = "filerecordsize";
    
    public static final String XCEDE_ATTR_TYPE = "type";
    
    public static final String XCEDE_ATTR_SUBTYPE = "subtype";
    
    public static final String XCEDE_ELEMENT_PROVENANCEID = "provenanceID";
    
    public static final String XCEDE_ELEMENT_RASORIGIN = "rasorigin";
    
    public static final String XCEDE_ELEMENT_DIMENSION = "dimension";
    
    public static final String XCEDE_ELEMENT_BYTEORDER = "byteorder";
    
    public static final String XCEDE_ELEMENT_ELEMENTTYPE = "elementtype";
    
    public static final String XCEDE_ELEMENT_DATARECFRAG = "datarecFrag";

    public static final String XCEDE_ELEMENT_SIZE = "size";
    
    public static final String XCEDE_ELEMENT_ORIGIN = "origin";
    
    public static final String XCEDE_ELEMENT_SPACING = "spacing";
    
    public static final String XCEDE_ELEMENT_GAP = "gap";
    
    public static final String XCEDE_ELEMENT_DATAPOINTS = "datapoints";
    
    public static final String XCEDE_ELEMENT_DIRECTION = "direction";
    
    public static final String XCEDE_ELEMENT_UNITS = "units";
    
    public static final String XCEDE_ATTR_OUTPUTSELECT = "outputselect";
    
    public static final String XCEDE_ELEMENT_PROVENANCE = "provenance";
    
    public static final String XCEDE_ELEMENT_STATISTIC = "statistic";

    public static final String XCEDE_ELEMENT_PROCESSSTEP = "processStep";

    public static final String XCEDE_ELEMENT_PROGRAMNAME = "programName";
   
    public static final String XCEDE_ELEMENT_PROGRAMARGUMENT = "programArgument";
    
    public static final String XCEDE_ELEMENT_VERSION = "version";
    
    public static final String XCEDE_ELEMENT_TIMESTAMP = "timeStamp";
    
    public static final String XCEDE_ELEMENT_CVS = "cvs";
    
    public static final String XCEDE_ELEMENT_USER = "user";
    
    public static final String XCEDE_ELEMENT_MACHINE = "machine";
    
    public static final String XCEDE_ELEMENT_PLATFORM = "platform";
    
    public static final String XCEDE_ELEMENT_PLATFORMVERSION = "platformVersion";
    
    public static final String XCEDE_ELEMENT_COMPILERNAME = "compilerName";
    
    public static final String XCEDE_ELEMENT_COMPILERVERSION = "compilerVersion";
    public static final String XCEDE_ELEMENT_LIBRARIES = "libraries";
    public static final String XCEDE_ELEMENT_LIBNAME = "libName";
    
    public static final String XCEDE_ELEMENT_LIBVERSION = "libVersion";
    
    public static final String XCEDE_ELEMENT_SOURCEDATA = "sourceData";
    
    public static final String XCEDE_ELEMENT_PROCESS = "process";

    public static final String XCEDE_ELEMENT_VALUE = "value";
    
    public static final String XCEDE_ELEMENT_PUNCORRECTED = "pUncorrected";
    
    public static final String XCEDE_ELEMENT_PCORRECTED = "pCorrected";
    public static final String XCEDE_ELEMENT_PCORRECTED_VALUE = "value";
    public static final String XCEDE_ATTR_CORRECTIONTYPE = "correctionType";
    
    public static final String XCEDE_ATTR_LOCATIONFOCI = "locationFoci";

    public static final String XCEDE_ELEMENT_NUMCLUSTERS = "numClusters";
    
    public static final String XCEDE_ELEMENT_P = "p";
    
    public static final String XCEDE_ELEMENT_EXTENTS = "extents";

    public static final String XCEDE_ELEMENT_LABEL = "label";
    
    public static final String XCEDE_ELEMENT_X = "x";
    
    public static final String XCEDE_ELEMENT_Y = "y";
    
    public static final String XCEDE_ELEMENT_Z = "z";
    
    public static final String XCEDE_ELEMENT_THRESH = "thresh";

    public static final String XCEDE_ELEMENT_BRODMAN = "brodman";
    
    public static final String XCEDE_ELEMENT_LATERIZATION = "laterization";
    
    public static final String XCEDE_ATTR_ATLAS = "atlas";

    public static final String XCEDE_ELEMENT_FWHM = "fwhm";
    
    public static final String XCEDE_ATTR_FWHMUNITS = "fwhmUnits";

    public static final String XCEDE_ELEMENT_S = "s";
    
    public static final String XCEDE_ELEMENT_SEARCHUNITS = "searchUnits";

    public static final String XCEDE_ELEMENT_VOX = "vox";
    
    public static final String XCEDE_ATTR_VOXELUNITS = "voxelUnits";

    public static final String XCEDE_ELEMENT_EXPVOXPERCLUSTER = "expVoxPerCluster";
    
    public static final String XCEDE_ELEMENT_EXPNUMCLUSTERS = "expNumClusters";
    
    public static final String XCEDE_ELEMENT_DF = "df";
    
    public static final String XCEDE_ELEMENT_FWHMSMOOTHNESS = "fwhmSmoothness";
    
    public static final String XCEDE_ELEMENT_VOXELSIZE = "voxelSize";
    
    public static final String XCEDE_ELEMENT_ACTIVATIONPARAMS = "activationParams";
    
    public static final String XCEDE_ELEMENT_CLUSTERS = "clusters";
    
    public static final String XCEDE_ELEMENT_VOXEL = "voxel";

    public static final String XCEDE_ELEMENT_ITEM = "item";

    public static final String XCEDE_ELEMENT_TAXONOMICCLASS = "taxonomicClass";
    
    public static final String XCEDE_ELEMENT_ACQPARAM= "acqParam";

    public static final String XCEDE_ELEMENT_EVENTS = "events";

    public static final String XCEDE_ELEMENT_EVENTPARAMS = "eventParams";
    
    public static final String XCEDE_ELEMENT_EVENT = "event";

    public static final String XCEDE_ELEMENT_FIRSTMRITIME = "firstmritime";

    public static final String XCEDE_ELEMENT_ONSET = "onset";
    
    public static final String XCEDE_ELEMENT_DURATION = "duration";
    
    public static final String XCEDE_ATTR_NAME = "name";

    public static final String XCEDE_ELEMENT_TEXT = "text";
    
    public static final String XCEDE_ELEMENT_ANNOTATOR = "annotator";
    
    public static final String XCEDE_ATTR_HREF = "href";
    
    public static final String XCEDE_ATTR_DESCRIPTION = "description";

    public static final String XCEDE_ATTR_DIRECTION = "direction";
    
    public static final String XCEDE_ELEMENT_ASSESSMENTVALUE = "assessmentValue";
    
    public static final String XCEDE_ELEMENT_SUMMARYNAME = "summaryName";
    
    public static final String XCEDE_ELEMENT_SUMMARYVALUE = "summaryValue";
    
    public static final String XCEDE_ELEMENT_NORMALIZEDVALUE = "normalizedValue";
    
    public static final String XCEDE_ELEMENT_SCHEME = "scheme";
    
    public static final String XCEDE_ELEMENT_FULLPATH = "fullPath";
    
    public static final String XCEDE_ELEMENT_ACTUAL_VALUE = "actualValue";
    
    public static final String XCEDE_ELEMENT_DATACLASSIFICATION = "dataClassification";

    public static final String XCEDE_ATTR_NITS = "units";

    public static final String XCEDE_ATTR_ORIGINALVALUE = "originalvalue";
    
    public static final String XCEDE_ATTR_ORIGINALUNITS = "originalunits";
    
    public static final String XCEDE_ELEMENT_RT = "RT";
    
    public static final String XCEDE_ELEMENT_COMMONNAME = "commonName";
    
    public static final String XCEDE_ELEMENT_LATINNAME = "latinName";
    
    public static final String XCEDE_ELEMENT_STRAIN = "strain";
    
    /***********************************************
     ***** XCEDE Schema Hierarchical Structure *****
     ****************************************/
    /**
     * XCEDE element constants for the project element.
     */
    public static final String XCEDE_ELEMENT_PROJECT_ID = "id";
    public static final String XCEDE_ELEMENT_PROJECT_DESCRIPTION = "description";
    public static final String XCEDE_ELEMENT_PROJECT_FUNDING = "funding";
    public static final String XCEDE_ELEMENT_PROJECT_SUBJECT = "subject";
   
    /**
     * XCEDE element constants for the subject element.
     */
    public static final String XCEDE_ELEMENT_SUBJECT_ID = "id";
    public static final String XCEDE_ELEMENT_SUBJECT_BIRTHDATE = "birthDate";
    public static final String XCEDE_ELEMENT_SUBJECT_DEATHDATE = "deathDate";
    public static final String XCEDE_ELEMENT_SUBJECT_NAME = "name";
    public static final String XCEDE_ELEMENT_SUBJECT_SEX = "sex";
    public static final String XCEDE_ELEMENT_SUBJECT_SPECIES = "species"; 
    public static final String XCEDE_ELEMENT_SUBJECT_EXTENDEDDESCRIPTOR = "extendedDescriptor";
    
    /**
     * XCEDE element constants for the visit element.
     */
    public static final String XCEDE_ELEMENT_VISIT_ID = "id";
    public static final String XCEDE_ELEMENT_VISIT_LOCATION = "location";
    public static final String XCEDE_ELEMENT_VISIT_SUBJECTVAR = "subjectVar";
    
    /**
     * XCEDE element constants for the study element.
     */
    public static final String XCEDE_ELEMENT_STUDY_ID = "id";
    
    /**
     * XCEDE element constants for the series element.
     */
    public static final String XCEDE_ELEMENT_SERIES_ID = "id";
    public static final String XCEDE_ELEMENT_SERIES_SCANNER = "scanner";
    public static final String XCEDE_ELEMENT_SERIES_EXPPROTOCOL = "expProtocol";
    public static final String XCEDE_ELEMENT_SERIES_ACQPROTOCOL = "acqProtocol";
    public static final String XCEDE_ELEMENT_SERIES_DATAREC = "datarec";

    /**
     * XCEDE helper element constants. 
     */
    /** XCEDE element and attribute constants for the subjectVar element. */
    public static final String XCEDE_ELEMENT_SUBJECTVAR_AGE = "age";
    public static final String XCEDE_ATTR_SUBJECTVAR_AGE_TYPE = "agetype";
    public static final String XCEDE_ATTR_SUBJECTVAR_UNITS = "units";
    public static final String XCEDE_ELEMENT_SUBJECTVAR_ASSESSMENT = "assessment";
    public static final String XCEDE_ELEMENT_SUBJECTVAR_ANNOTATION = "annotation";
    
    /** XCEDE element constants for the scanner element. */
    public static final String XCEDE_ELEMENT_SACNNER_MANUFACTURER = "manufacturer";
    public static final String XCEDE_ELEMENT_SCANNER_MODEL = "model";
    public static final String XCEDE_ELEMENT_SCANNER_ADDITIONALEQUIPMENT = "additionalEquipment";
    public static final String XCEDE_ELEMENT_SCANNER_ANNOTATION = "annotation";
    
    /** XCEDE element constants for the datarecFrag element. */
    public static final String XCEDE_ELEMENT_DATARECFRAG_FILENAME = "filename";
    public static final String XCEDE_ELEMENT_DATARECFRAG_FILEOFFSET = "fileOffset";
    public static final String XCEDE_ELEMENT_DATARECFRAG_FILERECORDSIZE = "filerecordsize";
    
    /**
     * XCEDE element and attribute constants for the datarec element.
     */
    public static final String XCEDE_ATTR_DATAREC_TYPE = "type";
    public static final String XCEDE_ATTR_DATAREC_SUBTYPE = "subtype";
    public static final String XCEDE_ELEMENT_DATAREC_PROVENANCEID = "provenanceID";
    public static final String XCEDE_ELEMENT_DATAREC_RASORIGIN = "rasorigin";
    public static final String XCEDE_ELEMENT_DATAREC_DIMENSION = "dimension";
    public static final String XCEDE_ELEMENT_DATAREC_BYTE_ORDER = "byteorder";
    public static final String XCEDE_ELEMENT_DATAREC_ELEMENT_TYPE = "elementtype";
    public static final String XCEDE_ELEMENT_DATAREC_DATARECFRAG = "datarecFrag";
    public static final String XCEDE_ELEMENT_DATAREC_DESCRIPTION = "description";

    
    /**
     * XCEDE element and attribute constants for the dimension element.
     */
    public static final String XCEDE_ELEMENT_DIMENSION_SIZE = "size";
    public static final String XCEDE_ELEMENT_DIMENSION_ORIGIN = "origin";
    public static final String XCEDE_ELEMENT_DIMENSION_SPACING = "spacing";
    public static final String XCEDE_ELEMENT_DIMENSION_GAP = "gap";
    public static final String XCEDE_ELEMENT_DIMENSION_DATA_POINTS = "datapoints";
    public static final String XCEDE_ELEMENT_DIMENSION_DIRECTION = "direction";
    public static final String XCEDE_ELEMENT_DIMENSION_UNITS = "units";
    public static final String XCEDE_ATTR_DIMENSION_TYPE = "type";
    public static final String XCEDE_ATTR_DIMENSION_OUTPUT_SELECT = "outputselect";
    
    /**
     * XCEDE common element constants.
     */
    public static final String XCEDE_ELEMENT_COMMON_PROVENANCE = "provenance";
    public static final String XCEDE_ELEMENT_COMMON_STATISTIC = "statistic";
    public static final String XCEDE_ELEMENT_COMMON_ANNOTATION = "annotation";

    /**
     * XCEDE element constants for the provenance element.
     */
    public static final String XCEDE_ELEMENT_PROVENANCE_PROCESSSTEP = "processStep";

    /**
     * XCEDE element constants for the processStep element.
     */
    public static final String XCEDE_ELEMENT_PROCESSSTEP_PROGRAMNAME = "programName";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_PROGRAMARGUMENT = "programArgument";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_VERSION = "version";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_TIMESTAMP = "timeStamp";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_CVS = "cvs";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_USER = "user";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_MACHINE = "machine";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_PLATFORM = "platform";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_PLATFORMVERSION = "platformVersion";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_COMPILERNAME = "compilerName";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_COMPILERVERSION = "compilerVersion";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_LIBNAME = "libName";
    public static final String XCEDE_ELEMENT_PROCESSSTEP_LIBVERSION = "libVersion";
    
    /**
     * XCEDE element constants for the statistic element.
     */
    public static final String XCEDE_ELEMENT_STATISTIC_SOURCEDATA = "sourceData";
    public static final String XCEDE_ELEMENT_STATISTIC_PROCESS = "process";
    public static final String XCEDE_ELEMENT_STATISTIC_PROVENANCE_ID = "provenanceID";
    public static final String XCEDE_ELEMENT_STATISTIC_DESCRIPTION = "description";
    public static final String XCEDE_ELEMENT_STATISTIC_ANNOTATION = "annotation";

    /**
     * XCEDE element and attribute constants for the thresh element.
     */
    public static final String XCEDE_ELEMENT_THRESH_VALUE = "value";
    public static final String XCEDE_ELEMENT_THRESH_PUNCORRECTED = "pUncorrected";
    public static final String XCEDE_ELEMENT_THRESH_PCORRECTED = "pCorrected";
    public static final String XCEDE_ATTR_THRESH_TYPE = "threshType";

    /**
     * XCEDE element and attribute constants for the extent element.
     */
    public static final String XCEDE_ELEMENT_EXTENT_LOCATION = "location";
    public static final String XCEDE_ELEMENT_EXTENT_SIZE = "size";
    public static final String XCEDE_ELEMENT_EXTENT_PUNCORRECTED = "pUncorrected";
    public static final String XCEDE_ELEMENT_EXTENT_PCORRECTED = "pCorrected";
    public static final String XCEDE_ATTR_EXTENT_LOCATIONFOCI = "locationFoci";

    /**
     * XCEDE element constants for the setLevel element.
     */
    public static final String XCEDE_ELEMENT_SETLEVEL_NUMCLUSTERS = "numClusters";
    public static final String XCEDE_ELEMENT_SETLEVEL_P = "p";
    public static final String XCEDE_ELEMENT_SETLEVEL_EXTENTS = "extents";

    /**
     * XCEDE element constants for the voxelLevel element.
     */
    public static final String XCEDE_ELEMENT_VOXEL_LEVEL_LOCATION = "location";

    /**
     * XCEDE element and attribute constants for the location element.
     */
    public static final String XCEDE_ELEMENT_LOCATION_LABEL = "label";
    public static final String XCEDE_ELEMENT_LOCATION_X = "x";
    public static final String XCEDE_ELEMENT_LOCATION_Y = "y";
    public static final String XCEDE_ELEMENT_LOCATION_Z = "z";
    public static final String XCEDE_ELEMENT_LOCATION_THRESH = "thresh";
    public static final String XCEDE_ATTR_LOCATION_UNITS = "units";

    /**
     * XCEDE element and attribute constants for the label element.
     */
    public static final String XCEDE_ELEMENT_LABEL_VALUE = "value";
    public static final String XCEDE_ELEMENT_LABEL_BRODMAN = "brodman";
    public static final String XCEDE_ELEMENT_LABEL_LATERIZATION = "laterization";
    public static final String XCEDE_ATTR_LABEL_ATLAS = "atlas";

    /**
     * XCEDE element and attribute constants for the fwhm element.
     */
    public static final String XCEDE_ELEMENT_FWHM_FWHM = "fwhm";
    public static final String XCEDE_ATTR_FWHM_FWHMUNITS = "fwhmUnits";

    /**
     * XCEDE element and attribute constants for the searchVol element.
     */
    public static final String XCEDE_ELEMENT_SEARCHVOL_S = "s";
    public static final String XCEDE_ELEMENT_SEARCHVOL_SEARCHUNITS = "searchUnits";

    /**
     * XCEDE element and attribute constants for the vox element.
     */
    public static final String XCEDE_ELEMENT_VOX_VOX = "vox";
    public static final String XCEDE_ATTR_VOX_VOXELUNITS = "voxelUnits";

    /**
     * XCEDE element constants for the activationParams element.
     */
    public static final String XCEDE_ELEMENT_ACTIVATIONPARAMS_THRESH = "thresh";
    public static final String XCEDE_ELEMENT_ACTIVATIONPARAMS_EXPVOXPERCLUSTER = "expVoxPerCluster";
    public static final String XCEDE_ELEMENT_ACTIVATIONPARAMS_EXPNUMCLUSTERS = "expNumClusters";
    public static final String XCEDE_ELEMENT_ACTIVATIONPARAMS_DF = "df";
    public static final String XCEDE_ELEMENT_ACTIVATIONPARAMS_FWHMSMOOTHNESS = "fwhmSmoothness";
    public static final String XCEDE_ELEMENT_ACTIVATIONPARAMS_VOXELSIZE = "voxelSize";
    
    /**
     * XCEDE element constants for the activation element.
     */
    public static final String XCEDE_ELEMENT_ACTIVATION_STATISTIC = "statistic";
    public static final String XCEDE_ELEMENT_ACTIVATION_ACTIVATIONPARAMS = "activationParams";
    public static final String XCEDE_ELEMENT_ACTIVATION_CLUSTERS = "clusters";
    public static final String XCEDE_ELEMENT_ACTIVATION_VOXEL = "voxel";

    /**
     * XCEDE element constants for the simpleStatisticList element.
     */
    public static final String XCEDE_ELEMENT_SIMPLESTATISTICLIST_STATISTIC = "statistic";
    public static final String XCEDE_ELEMENT_SIMPLESTATISTICLIST_ITEM = "item";

    /**
     * XCEDE element constants for the acqProtocol element.
     */
    public static final String XCEDE_ELEMENT_ACQPROTOCOL_ID = "id";
    public static final String XCEDE_ELEMENT_ACQPROTOCOL_TAXONOMICCLASS = "taxonomicClass";
    public static final String XCEDE_ELEMENT_ACQPROTOCOL_NAME = "name";
    public static final String XCEDE_ELEMENT_ACQPROTOCOL_ACQPARAM= "acqParam";
    public static final String XCEDE_ELEMENT_ACQPROTOCOL_ANNOTATION = "annotation";

    /**
     * XCEDE element constants for the protocolDescriptor attribute.
     */
    public static final String XCEDE_ATTR_PROTOCOLDESCRIPTOR_DESCRIPTION = "description";

    /**
     * XCEDE element constants for the expProtocol element.
     */
    public static final String XCEDE_ELEMENT_EXPPROTOCOL_ID = "id";
    public static final String XCEDE_ELEMENT_EXPPROTOCOL_TAXONOMICCLASS = "taxonomicClass";
    public static final String XCEDE_ELEMENT_EXPPROTOCOL_NAME = "name";
    public static final String XCEDE_ELEMENT_EXPPROTOCOL_EVENTS = "events";
    public static final String XCEDE_ELEMENT_EXPPROTOCOL_ANNOTATION = "annotation";

    /**
     * XCEDE element constants for the events element.
     */
    public static final String XCEDE_ELEMENT_EVENTS_PARAMS = "eventParams";
    public static final String XCEDE_ELEMENT_EVENTS_EVENT = "event";
    public static final String XCEDE_ELEMENT_EVENTS_DESCRIPTION = "description";
    public static final String XCEDE_ELEMENT_EVENTS_ANNOTATION = "annotation";

    /**
     * XCEDE element constants for the eventValue element.
     */
    public static final String XCEDE_ELEMENT_EVENTVALUE_NAME = "name";
    public static final String XCEDE_ELEMENT_EVENTVALUE_TYPE = "type";

    /**
     * XCEDE element constants for the eventParams element.
     */
    public static final String XCEDE_ELEMENT_EVENTPARAMS_FIRSTMRITIME = "firstmritime";
    public static final String XCEDE_ELEMENT_EVENTPARAMS_VALUE = "value";

    /**
     * XCEDE element and attribute constants for the event element.
     */
    public static final String XCEDE_ELEMENT_EVENT_ONSET = "onset";
    public static final String XCEDE_ELEMENT_EVENT_DURATION = "duration";
    public static final String XCEDE_ELEMENT_EVENT_VALUE = "value";
    public static final String XCEDE_ELEMENT_EVENT_ANNOTATION = "annotation";
    public static final String XCEDE_ATTR_EVENT_TYPE = "type";
    public static final String XCEDE_ATTR_EVENT_UNITS = "units";
    public static final String XCEDE_ATTR_EVENT_NAME = "name";

    /**
     * XCEDE element constants for the annotation element.
     */
    public static final String XCEDE_ELEMENT_ANNOTATION_TEXT = "text";
    public static final String XCEDE_ELEMENT_ANNOTATION_ANNOTATOR = "annotator";
    public static final String XCEDE_ELEMENT_ANNOTATION_TIMESTAMP = "timeStamp";
    
    /**
     * XCEDE attribute constants for the splice element.
     */
    public static final String XCEDE_ATTR_SPLICE_TYPE = "type";
    public static final String XCEDE_ATTR_SPLICE_HREF = "href";
    public static final String XCEDE_ATTR_SPLICE_DESCRIPTION = "description";

    /**
     * XCEDE attribute constants for the spliceUp element.
     */
    public static final String XCEDE_ATTR_SPLICEUP_DIRECTION = "direction";

    /**
     * XCEDE attribute constants for the spliceDown element.
     */
    public static final String XCEDE_ATTR_SPLICEDOWN_DIRECTION = "direction";
    
    /**
     * XCEDE element constants for the extendedDescriptor element.
     */
    public static final String XCEDE_ELEMENT_EXTENDEDDESCRIPTOR_NAME = "name";
    public static final String XCEDE_ELEMENT_EXTENDEDDESCRIPTOR_DESCRIPTION = "description";
    public static final String XCEDE_ELEMENT_EXTENDEDDESCRIPTOR_VALUE = "value";
    public static final String XCEDE_ELEMENT_EXTENDEDDESCRIPTOR_ANNOTATION = "annotation";
    
    /**
     * XCEDE element constants for the assessment element.
     */
    public static final String XCEDE_ELEMENT_ASSESSMENT_NAME = "name";
    public static final String XCEDE_ELEMENT_ASSESSMENT_TAXONOMICCLASS = "taxonomicClass";
    public static final String XCEDE_ELEMENT_ASSESSMENT_DESCRIPTION = "description";
    public static final String XCEDE_ELEMENT_ASSESSMENT_ASSESSMENTVALUE = "assessmentValue";

    /**
     * XCEDE element constants for the assessmentValue element.
     */
    public static final String XCEDE_ELEMENT_ASSESSMENTVALUE_SUMMARYNAME = "summaryName";
    public static final String XCEDE_ELEMENT_ASSESSMENTVALUE_SUMMARYVALUE = "summaryValue";
    public static final String XCEDE_ELEMENT_ASSESSMENTVALUE_NORMALIZEDVALUE = "normalizedValue";
    public static final String XCEDE_ELEMENT_ASSESSMENTVALUE_DESCRIPTION = "description";
    public static final String XCEDE_ELEMENT_ASSESSMENTVALUE_ANNOTATION = "annotation";

    /**
     * XCEDE element constants for the taxonomicClass element.
     */
    public static final String XCEDE_ELEMENT_TAXONOMICCLASS_SCHEME = "scheme";
    public static final String XCEDE_ELEMENT_TAXONOMICCLASS_ID = "ID";
    public static final String XCEDE_ELEMENT_TAXONOMICCLASS_FULLPATH = "fullPath";

    /**
     * XCEDE element constants for the value element.
     */
    public static final String XCEDE_ELEMENT_VALUE_ACTUAL_VALUE = "actualValue";
    public static final String XCEDE_ELEMENT_VALUE_DATACLASSIFICATION = "dataClassification";
    public static final String XCEDE_ELEMENT_VALUE_UNITS = "units";

    /**
     * XCEDE attribute constants for the compactValue element.
     */
    public static final String XCEDE_ATTR_COMPACTVALUE_NAME = "name";
    public static final String XCEDE_ATTR_COMPACTVALUE_TYPE = "type";
    public static final String XCEDE_ATTR_COMPACTVALUE_UNITS = "units";

    /**
     * XCEDE attribute constants for the convertedCompactValue element.
     */
    public static final String XCEDE_ATTR_CONVERTEDCOMPACTVALUE_ORIGINALVALUE = "originalvalue";
    public static final String XCEDE_ATTR_CONVERTEDCOMPACTVALUE_ORIGINALUNITS = "originalunits";
    
    /**
     * Enumerations for the units attribute of the age element.
     */
    public static final String XCEDE_AGE_UNITS_HOURS = "hours"; 
    public static final String XCEDE_AGE_UNITS_DAYS = "days"; 
    public static final String XCEDE_AGE_UNITS_WEEKS = "weeks"; 
    public static final String XCEDE_AGE_UNITS_YEARS = "years";
    
    /**
     * Enumerations for the age type attribute of the age element.
     */
    public static final String XCEDE_AGE_TYPE_GESTATIONAL = "gestaional"; 
    public static final String XCEDE_AGE_TYPE_POSTNATAL = "postnatal"; 
    public static final String XCEDE_AGE_TYPE_PORTMORTEM = "portmortem"; 
    
    /**
     * Enumerations for the byte order.
     */
    public static final String XCEDE_BYTEORDER_MSBFIRST = "msbfirst";
    public static final String XCEDE_BYTEORDER_LSBFIRST = "lsbfirst";
    
    /**
     * Enumberations for the the element type.
     */
    public static final String XCEDE_ELEMENTTYPE_INT8 = "int8";
    public static final String XCEDE_ELEMENTTYPE_UINT8 = "uint8";
    public static final String XCEDE_ELEMENTTYPE_INT16 = "int16";
    public static final String XCEDE_ELEMENTTYPE_UINT16 = "uint16";
    public static final String XCEDE_ELEMENTTYPE_INT32 = "int32";
    public static final String XCEDE_ELEMENTTYPE_UINT32 = "uint32";
    public static final String XCEDE_ELEMENTTYPE_INT64 = "int64";
    public static final String XCEDE_ELEMENTTYPE_UINT64 = "uint64";
    public static final String XCEDE_ELEMENTTYPE_FLOAT32 = "float32";
    public static final String XCEDE_ELEMENTTYPE_FLOAT64 = "float64";
    public static final String XCEDE_ELEMENTTYPE_ASCII = "ascii";
    
    
    /**
     * Enumerations for the lateralization.
     */
    public static final String XCEDE_LATERALIZATION_LEFT = "left";
    public static final String XCEDE_LATERALIZATION_RIGHT = "right";
    public static final String XCEDE_LATERALIZATION_BILATERAL = "bilateral";
    
    /**
     * Enumerations for the event value type attribute.
     */
    public static final String XCEDE_EVENTVALUE_TYPE_NUMBER = "number";
    public static final String XCEDE_EVENTVALUE_TYPE_STRING = "string";
    
    /**
     * Enumerations for the value types element.
     */
    public static final String XCEDE_VALUETYPES_FLOAT = "float";
    public static final String XCEDE_VALUETYPES_BOOLEAN = "boolean";
    public static final String XCEDE_VALUETYPES_VARCHAR = "varchar";
    public static final String XCEDE_VALUETYPES_INTEGER = "integer";
    public static final String XCEDE_VALUETYPES_URI = "URI";

    /**
     * Enumerations for the sex element.
     */
    public static final String XCEDE_SEX_MALE = "male";
    public static final String XCEDE_SEX_FEMALE = "female";
    public static final String XCEDE_SEX_OTHER = "other";
    
    public static final String XCEDE_DATAREC_IMAGEFRAME = "imageFrame";
    /**
     * Constructor.
     */
    public XCEDEElement() {
        super();
    }
    
    public XCEDEElement(XCEDEElement parent){
        super(parent);
    }
    
    public XCEDEElement(String key){
        super(key);
    }
    
    public XCEDEElement(String key, XCEDEElement parent){
        super(key, parent);
    }

    protected void finalize() throws Throwable{
        System.out.println("Entering the XCEDEElement's finalize() ... ");
        super.finalize();
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the root elment.
     * @return true if this <code>XCEDEElement</code> is the root elment.
     */
    public boolean isRootElement(){
        if(getParent() == null){
            return true;
        }else{
            return false;
        }
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the project elment.
     * @return true if this <code>XCEDEElement</code> is the project elment.
     */
    public boolean isProjectLevelElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ROOT_ELEMENT_PROJECTLEVEL)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the subject elment.
     * @return true if this <code>XCEDEElement</code> is the subject elment.
     */
    public boolean isSubjectLevelElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ROOT_ELEMENT_SUBJECTLEVEL)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the visit elment.
     * @return true if this <code>XCEDEElement</code> is the visit elment.
     */
    public boolean isVisitLevelElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ROOT_ELEMENT_VISITLEVEL)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the study elment.
     * @return true if this <code>XCEDEElement</code> is the study elment.
     */
    public boolean isStudyLevelElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ROOT_ELEMENT_STUDYLEVEL)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the series elment.
     * @return true if this <code>XCEDEElement</code> is the series elment.
     */
    public boolean isSeriesLevelElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ROOT_ELEMENT_SERIESLEVEL)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the project elment.
     * @return true if this <code>XCEDEElement</code> is the project elment.
     */
    public boolean isProjectElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ELEMENT_PROJECT)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the subject elment.
     * @return true if this <code>XCEDEElement</code> is the subject elment.
     */
    public boolean isSubjectElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ELEMENT_SUBJECT)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the visit elment.
     * @return true if this <code>XCEDEElement</code> is the visit elment.
     */
    public boolean isVisitElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ELEMENT_VISIT)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the study elment.
     * @return true if this <code>XCEDEElement</code> is the study elment.
     */
    public boolean isStudyElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ELEMENT_STUDY)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the series elment.
     * @return true if this <code>XCEDEElement</code> is the series elment.
     */
    public boolean isSeriesElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ELEMENT_SERIES)){
            return true;
        }
        return false;
    }
    
    /**
     * Returns true if this <code>XCEDEElement</code> is the datarec elment.
     * @return true if this <code>XCEDEElement</code> is the datarec elment.
     */
    public boolean isDatarecElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ELEMENT_DATAREC)){
            return true;
        }
        return false;
    }
  
    public boolean isDatarecFragElement(){
        String level = getLevel();
        if(level == null){
            return false;
        }
        if(level.equals(XCEDE_ELEMENT_DATARECFRAG)){
            return true;
        }
        return false;
    }
    /********************************************************
     ***** The implementation of the TreeNode interface *****
     ********************************************************
     */
    /**
     * @see TreeNode.
     */
    public Enumeration children(){
        Vector children = getChildren();
        if(children != null){
            return children.elements();
        }
        return null;
    }
    
    /**
     * Basically 
     * @return
     */
    private Vector getChildren(){
        if(isProjectLevelElement()){
            if(get(XCEDEElement.XCEDE_ELEMENT_SUBJECT) != null){
                return getChildList(XCEDEElement.XCEDE_ELEMENT_SUBJECT);
            }
            return null;
        }
        if(isSubjectLevelElement()){
            if(get(XCEDEElement.XCEDE_ELEMENT_VISIT) != null){
                return getChildList(XCEDEElement.XCEDE_ELEMENT_VISIT);
            }
            return null;
        }
        if(isVisitLevelElement()){
            if(get(XCEDEElement.XCEDE_ELEMENT_STUDY) != null){
                return getChildList(XCEDEElement.XCEDE_ELEMENT_STUDY);
            }
            return null;
        }
        if(isStudyLevelElement()){
            if(get(XCEDEElement.XCEDE_ELEMENT_SERIES) != null){
                return getChildList(XCEDEElement.XCEDE_ELEMENT_SERIES);
            }
            return null;
        }
        if(isSeriesLevelElement()){
            if(get(XCEDEElement.XCEDE_ELEMENT_DATAREC) != null){
                return getChildList(XCEDEElement.XCEDE_ELEMENT_DATAREC);
            }
            return null;
        }
        if(isDatarecElement()){
            if(get(XCEDEElement.XCEDE_ELEMENT_DATARECFRAG) != null){
                return getChildList(XCEDEElement.XCEDE_ELEMENT_DATARECFRAG);
            }
            return null;
        }
        return null;
    }
    /**
     * @see TreeNode.
     */
    public boolean getAllowsChildren(){
        return true;
    }
    
    /**
     * @see TreeNode.
     */
    public TreeNode getChildAt(int childIndex){
        Vector children = getChildren();
        if(children != null){
            return (XCEDEElement)children.get(childIndex);
        }
        return null;
    }
    
    /**
     * @see TreeNode.
     */
    public int getChildCount(){
        Vector children = getChildren();
        if(children != null){
            return children.size();
        }
        return 0;
    }
    
    /**
     * @see TreeNode.
     */
    public int getIndex(TreeNode node){
        Vector children = getChildren();
        if(children != null){
            return children.indexOf(node);
        }
        return -1;
    }
    
    /**
     * @see TreeNode.
     */
    public TreeNode getParent(){
        return (XCEDEElement)getParentElement();
    }
    
    /**
     * @see TreeNode.
     */
    public boolean isLeaf(){
        if(isProjectLevelElement() && get(XCEDEElement.XCEDE_ELEMENT_SUBJECT) != null){
            return false;
        }
        if(isSubjectLevelElement() && get(XCEDEElement.XCEDE_ELEMENT_VISIT) != null){
            return false;
        }
        if(isVisitLevelElement() && get(XCEDEElement.XCEDE_ELEMENT_STUDY) != null){
            return false;
        }
        if(isStudyLevelElement() && get(XCEDEElement.XCEDE_ELEMENT_SERIES) != null){
            return false;
        }
        if(isSeriesLevelElement() && get(XCEDEElement.XCEDE_ELEMENT_DATAREC) != null){
            return false;
        }
        if(isDatarecElement() && get(XCEDEElement.XCEDE_ELEMENT_DATARECFRAG) != null){
            return false;
        }
        return true;
    }
    
    /**
     * A helper function to get the datarec element contained in the rootElement.
     * @param rootElement an element.
     * @return
     */
    public static XCEDEElement getDatarecElement(XCEDEElement element){
        return null;
    }
    
    public boolean isChild(String elementName){
        if(isProjectLevelElement()){
            if(elementName.equals(XCEDEElement.XCEDE_ELEMENT_SUBJECT)){
                return true;
            }
        }else if(isSubjectLevelElement()){
            if(elementName.equals(XCEDEElement.XCEDE_ELEMENT_VISIT)){
                return true;
            }
        }else if(isVisitLevelElement()){
            if(elementName.equals(XCEDEElement.XCEDE_ELEMENT_STUDY)){
                return true;
            }
        }else if(isStudyLevelElement()){
            if(elementName.equals(XCEDEElement.XCEDE_ELEMENT_SERIES)){
                return true;
            }
        }else if(isSeriesLevelElement()){
            if(elementName.equals(XCEDEElement.XCEDE_ELEMENT_DATAREC)){
                return true;
            }
        }else if(isDatarecElement()){
            if(elementName.equals(XCEDEElement.XCEDE_ELEMENT_DATARECFRAG)){
                return true;
            }
        }
        return false;
    }
    /**
     * Returns the number of attributes which need to be displayed in the information panel.
     * @return the number of attributes which need to be displayed in the information panel.
     */
    public int getAttrCount(){
        int attrCount = getKeyCount();
        /**
         * Removes the level, parent and subject attributes.
         */
        attrCount -= 3;
        return attrCount;
    }
    
    public String toString(){
        StringBuffer  sb = new StringBuffer("");
        /**
         * This element corresponds to the projectlevel_up_s
         */
        if(isProjectElement()){
            if(get(XCEDEElement.XCEDE_ELEMENT_ID) != null){
                sb.append("id: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_ID));
                sb.append("\n");
            }
            if(get(XCEDEElement.XCEDE_ELEMENT_DESCRIPTION) != null){
                sb.append("description: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_DESCRIPTION));
                sb.append("\n");
            }
            if(get(XCEDEElement.XCEDE_ELEMENT_FUNDING) != null){
                sb.append("funding: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_FUNDING));
                sb.append("\n");
            }
        }else if(isSubjectElement()){
            if(get(XCEDEElement.XCEDE_ELEMENT_ID) != null){
                sb.append("id: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_ID));
                sb.append("\n");
            }
            if(get(XCEDEElement.XCEDE_ELEMENT_BIRTHDATE) != null){
                sb.append("birthdate: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_BIRTHDATE));
                sb.append("\n");
            }
            if(get(XCEDEElement.XCEDE_ELEMENT_DEATHDATE) != null){
                sb.append("deathdate: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_DEATHDATE));
                sb.append("\n");
            }
            if(get(XCEDEElement.XCEDE_ELEMENT_NAME) != null){
                sb.append("name: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_NAME));
                sb.append("\n");
            }
            if(get(XCEDEElement.XCEDE_ELEMENT_SEX) != null){
                sb.append("sex: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_SEX));
                sb.append("\n");
            }
            if(get(XCEDEElement.XCEDE_ELEMENT_SPECIES) != null){
                sb.append("species: ");
                sb.append(get(XCEDEElement.XCEDE_ELEMENT_SPECIES).toString());
                sb.append("\n");
            }
            Vector extendedDescriptors = (Vector)get(XCEDEElement.XCEDE_ELEMENT_EXTENDEDDESCRIPTOR);
            if(extendedDescriptors != null){
                for(int i = 0; i < extendedDescriptors.size(); i++){
                    sb.append("extendedDescriptor: ");
                    sb.append(((XCEDEElement)extendedDescriptors.get(i)).toString());
                    sb.append("\n");
                }
            }
            
        }else if(isVisitElement()){
           if(get(XCEDEElement.XCEDE_ELEMENT_ID) != null){
               sb.append("id: ");
               sb.append(get(XCEDEElement.XCEDE_ELEMENT_ID).toString());
               sb.append("\n");
           }else if(get(XCEDEElement.XCEDE_ELEMENT_LOCATION) != null){
               sb.append("location: ");
               sb.append(get(XCEDEElement.XCEDE_ELEMENT_LOCATION).toString());
               sb.append("\n");
           }else if(get(XCEDEElement.XCEDE_ELEMENT_SUBJECTVAR) != null){
               sb.append("subject: ");
               sb.append(get(XCEDEElement.XCEDE_ELEMENT_SUBJECTVAR).toString());
               sb.append("\n");               
           }
        }else if(isStudyElement()){
            
        }else{
            String level = (String)get(Element.LEVEL);
            if(level.equals(XCEDEElement.XCEDE_ELEMENT_DIMENSION)){
                if(get(XCEDEElement.XCEDE_ATTR_TYPE) != null){
                    sb.append("type: ");
                    sb.append(get(XCEDEElement.XCEDE_ATTR_TYPE));
                    sb.append("\n");
                }
                if(get(XCEDEElement.XCEDE_ELEMENT_SIZE) != null){
                    sb.append("size: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_SIZE).toString());
                    sb.append("\n");
                }
                if(get(XCEDEElement.XCEDE_ELEMENT_ORIGIN) != null){
                    sb.append("origin: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_ORIGIN).toString());
                    sb.append("\n");
                }
                if(get(XCEDEElement.XCEDE_ELEMENT_SPACING) != null){
                    sb.append("spacing: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_SPACING).toString());
                    sb.append("\n");
                }
                if(get(XCEDEElement.XCEDE_ELEMENT_GAP) != null){
                    sb.append("gap: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_GAP).toString());
                    sb.append("\n");
                }
                if(get(XCEDEElement.XCEDE_ELEMENT_UNITS) != null){
                    sb.append("units: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_UNITS).toString());
                    sb.append("\n");
                }
                if(get(XCEDEElement.XCEDE_ELEMENT_DIRECTION) != null){
                    sb.append("direction: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_DIRECTION).toString());
                    sb.append("\n");
                }
                if(get(XCEDEElement.XCEDE_ELEMENT_DATAPOINTS) != null){
                    sb.append("data points: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_DATAPOINTS).toString());
                    sb.append("\n");
                }
                if(get(XCEDEElement.XCEDE_ATTR_OUTPUTSELECT) != null){
                    sb.append("outputselect: ");
                    sb.append(get(XCEDEElement.XCEDE_ATTR_OUTPUTSELECT));
                    sb.append("\n");
                }
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_EXPPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_SCANNER)){
                if(get(XCEDEElement.XCEDE_ELEMENT_MANUFACTURER) != null){
                    sb.append("manufacturer: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_MANUFACTURER));
                    sb.append("\n");
                }else if(get(XCEDEElement.XCEDE_ELEMENT_MODEL) != null){
                    sb.append("manufacturer: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_MODEL));
                    sb.append("\n");
                }else if(get(XCEDEElement.XCEDE_ELEMENT_ADDITIONALEQUIPMENT) != null){
                    sb.append("manufacturer: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_ADDITIONALEQUIPMENT));
                    sb.append("\n");
                }else if(get(XCEDEElement.XCEDE_ELEMENT_ANNOTATION) != null){
                    sb.append("annotation: ");
                    sb.append(get(XCEDEElement.XCEDE_ELEMENT_ANNOTATION));
                    sb.append("\n");
                }
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_SUBJECTVAR)){
                if(get(XCEDEElement.XCEDE_ELEMENT_AGE) != null){
                    XCEDEElement ageElement = (XCEDEElement)get(XCEDEElement.XCEDE_ELEMENT_AGE);
                    if(ageElement.get(XCEDEElement.XCEDE_ELEMENT_AGE_VALUE) != null){
                        sb.append("age: ");
                        sb.append(ageElement.get(XCEDEElement.XCEDE_ELEMENT_AGE_VALUE));
                        sb.append("\n");
                    }
                }
                if(get(XCEDEElement.XCEDE_ELEMENT_ASSESSMENT) != null){
                    sb.append("age: ");
                    sb.append(((XCEDEElement)get(XCEDEElement.XCEDE_ELEMENT_AGE)).get(XCEDEElement.XCEDE_ELEMENT_AGE_VALUE));
                    sb.append("\n");
                }
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }else if(level.equals(XCEDEElement.XCEDE_ELEMENT_ACQPROTOCOL)){
                
            }
        }
        return sb.toString();
    }
}
