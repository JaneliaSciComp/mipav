package gov.nih.mipav.model.file;


/**
 * Inherits from FileXML, reads SurfaceRef.XML files based on the "surfaceref.xsd" file. Defines specific variables for
 * reading and writing surfaceref.xml files:
 */
public abstract class FileSurfaceGiftiXML extends FileXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    /** array of strings representing the tags under <CoordinateSystemTransformMatrix> in the Gifti xml schema. */
    protected static final String[] CoordinateSystemTransformMatrixStr = { "DataSpace", "TransformedSpace", "MatrixData"};

    /** array of strings representing the tags under <Data> in the Gifti xml schema. */
    protected static final String[] DataStr = { "true"};   // ???????
    
    /** array of strings representing the tags under <DataArray> in the Gifti xml schema. */
    protected static final String[] DataArrayStr = { "MetaData", "CoordinateSystemTransformMatrix", "Data" };

    // DataArray Attributes
    protected static final String[] DataArrayAttributes = {"Dimensionality", "Dim0", "Dim1", "Dim2", "Dim3", "Dim4", "Dim5", 
    	"Intent", "ArrayIndexingOrder", "DataType", "Endian", "ExternalFileName", "ExternalFileOffset",
    	"Encoding", };
    
    // Intent values 
    protected static final String[] IntentValues = { "NIFTI_INTENT_NONE", "NIFTI_INTENT_CORREL","NIFTI_INTENT_TTEST",
    	"NIFTI_INTENT_FTEST","NIFTI_INTENT_ZSCORE","NIFTI_INTENT_CHISQ","NIFTI_INTENT_BETA","NIFTI_INTENT_BINOM",
    	"NIFTI_INTENT_GAMMA","NIFTI_INTENT_POISSON","NIFTI_INTENT_NORMAL","NIFTI_INTENT_FTEST_NONC","NIFTI_INTENT_CHISQ_NONC",
    	"NIFTI_INTENT_LOGISTIC","NIFTI_INTENT_LAPLACE","NIFTI_INTENT_UNIFORM","NIFTI_INTENT_TTEST_NONC",
    	"NIFTI_INTENT_WEIBULL","NIFTI_INTENT_CHI","NIFTI_INTENT_INVGAUSS","NIFTI_INTENT_EXTVAL","NIFTI_INTENT_PVAL",
    	"NIFTI_INTENT_LOGPVAL","NIFTI_INTENT_LOG10PVAL","NIFTI_INTENT_ESTIMATE","NIFTI_INTENT_LABEL",
    	"NIFTI_INTENT_NEURONAME","NIFTI_INTENT_GENMATRIX","NIFTI_INTENT_SYMMATRIX","NIFTI_INTENT_DISPVECT",
    	"NIFTI_INTENT_VECTOR","NIFTI_INTENT_POINTSET","NIFTI_INTENT_TRIANGLE","NIFTI_INTENT_QUATERNION",
    	"NIFTI_INTENT_DIMLESS","NIFTI_INTENT_TIME_SERIES","NIFTI_INTENT_RGB_VECTOR","NIFTI_INTENT_RGBA_VECTOR",
    	"NIFTI_INTENT_NODE_INDEX_LIST","NIFTI_INTENT_SHAPE","NIFTI_INTENT_UNKNOWN"};
    
    // ArrayIndexingOrder values
    protected static final String[] ArrayIndexingOrderValues = {"RowMajorOrder","ColumnMajorOrder"};
    
    // DataType Values
    protected static final String[] DataTypeValues = {"NIFTI_TYPE_UINT8", "NIFTI_TYPE_INT32", "NIFTI_TYPE_FLOAT32"};
    
    // Endian Values 
    protected static final String[] EndianValues = {"BigEndian", "LittleEndian"};
    
    // Encoding values
    protected static final String[] EncodingTypes = {"Base64Binary", "GZipBase64Binary", "ExternalFileBinary"};
    
    // DataSpace value 
    protected static final String[] DataSpaceValues = {"true"};
   
    /** array of strings representing the tags under <GIFTI> in the Gifti xml schema. */
    protected static final String[] GiftiStr = { "MetaData", "LabelTable", "DataArray" }; 
    
    // Gifti attributes
    protected static final String[] GiftiAttributes = { "NumberOfDataArrays", "Version"};
    
    /** array of strings representing the tags under <Label> in the Gifti xml schema. */
    protected static final String[] LabelStr = {"Index"};
    
    /** array of strings representing the tags under <LabelTable> in the Gifti xml schema. */
    protected static final String[] LabelTableStr = { "Label" };
    
    /** array of strings representing the tags under <MatrixData> in the Gifti xml schema. */
    protected static final String[] MatrixDataStr = {"true"};  // ???????????????
    
    /** array of strings representing the tags under <MD> in the Gifti xml schema. */
    protected static final String[] MDStr = { "Name", "Value" };
    
    /** array of strings representing the tags under <MetaData> in the Gifti xml schema. */
    protected static final String[] MetaDataStr = { "MD" };
    
    /** array of strings representing the tags under <Name> in the Gifti xml schema. */
    protected static final String[] NameStr = { "true" };
    
    /** array of strings representing the tags under <TransformedSpace> in the Gifti xml schema. */
    protected static final String[] TransformedSpaceStr = { "true" };
    
    /** array of strings representing the tags under <Value> in the Gifti xml schema. */
    protected static final String[] ValueStr = { "true" };
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileSurfaceGiftiXML(String fName, String fDir) {
        super(fName, fDir);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares class for cleanup.
     */
    public void finalize() {
        super.finalize();
    }
}
