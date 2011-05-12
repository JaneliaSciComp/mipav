package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.model.structures.*;


/**
 * A script action which changes the image's resolutions
 */
public class ActionChangeTransformInfo extends ActionImageProcessorBase {

    /**
     * The label to use for the parameter indicating the transform matrix
     */
    private static final String TRANSFORM_MATRIX = "transform_matrix";
    
    /**
     * The label to use for the parameter indicating the transform ID
     */
    private static final String TRANSFORM_ID = "transform_id";
    
    private TransMatrix transMatrix = null;
    
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionChangeTransformInfo() {
        super();
    }
    
    /**
     * Main constructor with parameters for changing the transform information
     * @param image
     */
    public ActionChangeTransformInfo(ModelImage image, TransMatrix tMat) {
        super(image);
        this.transMatrix = tMat;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
    	//double [][] matrix = transMatrix.getMatrix();
    	
    	int len = transMatrix.getDim();
    	float [] tMat = new float [len * len];
    	
    	transMatrix.GetData(tMat);
    	
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(createInputImageParameter(isScript));
            parameters.put(ParameterFactory.newParameter(TRANSFORM_MATRIX, tMat));
            parameters.put(ParameterFactory.newParameter(TRANSFORM_ID, transMatrix.getTransformID()));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered while recording " + getActionName() + " script action:\n" + pe);
            return;
        }
        
        if (isScript) {
            ScriptRecorder.getReference().addLine(getActionName(), parameters);
        } else {
        	ProvenanceRecorder.getReference().addLine(getActionName(), parameters);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
        
        float [] tMat =parameters.getList(TRANSFORM_MATRIX).getAsFloatArray();
        int transformID = parameters.getInt(TRANSFORM_ID);
        
        int nDims = inputImage.getNDims();
        
        transMatrix = new TransMatrix(nDims+1, transformID);
        
        
        int index = 0;
        
        for (int i = 0; i < (nDims+1); i++) {
        	for (int j = 0; j < (nDims+1); j++, index++) {
        		transMatrix.Set(i, j, tMat[index]);
        	}
        }
       inputImage.getMatrixHolder().addMatrix(transMatrix);
    }
}
