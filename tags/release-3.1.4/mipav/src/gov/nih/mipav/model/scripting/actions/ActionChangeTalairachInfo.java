package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.Point3Df;
import gov.nih.mipav.model.structures.TalairachTransformInfo;
import gov.nih.mipav.view.MipavUtil;


/**
 * A script action which changes the image's talairach information
 */
public class ActionChangeTalairachInfo extends ActionImageProcessorBase {

   
    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionChangeTalairachInfo() {
        super();
    }
    
    /**
     * Main constructor with parameters for changing the talairach information
     * @param image
     */
    public ActionChangeTalairachInfo(ModelImage image) {
        super(image);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
    	TalairachTransformInfo tInfo = recordingInputImage.getTalairachTransformInfo();

    	Point3Df tempPoint;
    	
        try {
        	ParameterTable parameters = new ParameterTable();
        	parameters.put(createInputImageParameter());
        	
        	parameters.put(ParameterFactory.newParameter("is_acpc", tInfo.isAcpc()));
        	tempPoint = tInfo.getOrigAC();
        	 
        	 parameters.put(ParameterFactory.newParameter("orig_ac", new float[]{tempPoint.x, tempPoint.y, tempPoint.z}));
        	         	 
        	 tempPoint = tInfo.getOrigPC();
        	 parameters.put(ParameterFactory.newParameter("orig_pc", new float[]{tempPoint.x, tempPoint.y, tempPoint.z}));
        	
        	 parameters.put(ParameterFactory.newParameter("orig_dim", tInfo.getOrigDim()));
           
        	 parameters.put(ParameterFactory.newParameter("orig_res", tInfo.getOrigRes()));
        	 
        	 tempPoint = tInfo.getAcpcPC();
        	 parameters.put(ParameterFactory.newParameter("acpc_pc", new float[]{tempPoint.x, tempPoint.y, tempPoint.z}));
        	 
        	 parameters.put(ParameterFactory.newParameter("acpc_res", tInfo.getAcpcRes()));
        	 
        	 float [][] origOrient = tInfo.getOrigOrient();
        	 int len = origOrient.length;
        	 int index = 0;
        	 float [] oOrient = new float[len * len];
        	 for (int i = 0; i < len; i++) {
        		 for (int j = 0; j < len; j++, index++) {
        			 oOrient[index] = origOrient[i][j];
        		 }
        	 }
        	 
        	 parameters.put(ParameterFactory.newParameter("orig_orient", oOrient));
        	 parameters.put(ParameterFactory.newParameter("is_tlrc", tInfo.isTlrc()));
           
        	 if (tInfo.isTlrc()) {
        		 
        		 tempPoint = tInfo.getAcpcMin();
        		 parameters.put(ParameterFactory.newParameter("acpc_min", new float[]{tempPoint.x, tempPoint.y, tempPoint.z}));
        		 
        		 tempPoint = tInfo.getAcpcMax();
        		 parameters.put(ParameterFactory.newParameter("acpc_max", new float[]{tempPoint.x, tempPoint.y, tempPoint.z}));
        		 
        		 parameters.put(ParameterFactory.newParameter("tlrc_res", tInfo.getTlrcRes()));
        		 
        		 
        		 
        	 }

             ScriptRecorder.getReference().addLine(getActionName(), parameters);
           
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered while recording " + getActionName() + " script action:\n" + pe);
            return;
        }
        
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
        
        TalairachTransformInfo tInfo = inputImage.getTalairachTransformInfo();
        if (tInfo == null) {
        	tInfo = new TalairachTransformInfo();
        }
        
        tInfo.isAcpc(parameters.getBoolean("is_acpc"));
        float [] origAC = parameters.getList("orig_ac").getAsFloatArray();
        tInfo.setOrigAC(new Point3Df(origAC[0], origAC[1], origAC[2]));
        
        float [] origPC = parameters.getList("orig_pc").getAsFloatArray();
        tInfo.setOrigPC(new Point3Df(origPC[0], origPC[1], origPC[2]));
        
        tInfo.setOrigDim(parameters.getList("orig_dim").getAsIntArray());
        tInfo.setOrigRes(parameters.getList("orig_res").getAsFloatArray());
        
        float [] acpcPC = parameters.getList("acpc_pc").getAsFloatArray();
        tInfo.setAcpcPC(new Point3Df(acpcPC[0], acpcPC[1], acpcPC[2]));
        
        tInfo.setAcpcRes(parameters.getFloat("acpc_res"));
        
        float [] oOrient = parameters.getList("orig_orient").getAsFloatArray();
        
        float [][] origOrient = new float[3][3];
        int index = 0;	
        for (int i = 0; i < 3; i++) {
        	for (int j = 0; j < 3; j++, index++) {
        		origOrient[i][j] = oOrient[index];
        	}
        }
        tInfo.setOrigOrient(origOrient);
        
        tInfo.isTlrc(parameters.getBoolean("is_tlrc"));
        
        if (tInfo.isTlrc()) {
        	
        	float [] acpcMin = parameters.getList("acpc_min").getAsFloatArray();
        	tInfo.setAcpcMin(new Point3Df(acpcMin[0], acpcMin[1], acpcMin[2]));
        	
        	float [] acpcMax = parameters.getList("acpc_max").getAsFloatArray();
        	tInfo.setAcpcMax(new Point3Df(acpcMax[0], acpcMax[1], acpcMax[2]));
        	
        	tInfo.setTlrcRes(parameters.getList("tlrc_res").getAsFloatArray());
        }
        inputImage.setTalairachTransformInfo(tInfo);
    }
}
