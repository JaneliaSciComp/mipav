package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * A script action which converts all paint within an image to some type of mask image (short, binary, ubyte).
 */
public class ActionPaintToMask implements ScriptableActionInterface {

    /**
     * The label to use for the input image parameter.
     */
    private static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * The label to use for the mask output image data type parameter.
     */
    private static final String MASK_DATA_TYPE = "mask_data_type";
    
    /**
     * Indicates that a binary mask should be generated from an image's paint.
     */
    public static final String MASK_BINARY = "binary";
    
    /**
     * Indicates that a short mask should be generated from an image's paint.
     */
    public static final String MASK_SHORT = "short";
    
    /**
     * Indicates that a unsigned byte mask should be generated from an image's paint.
     */
    public static final String MASK_UBYTE = "ubyte";
    
    /**
     * The image which had its paint extracted to a mask (which should now be recorded).
     */
    private ModelImage recordingInputImage;
    
    /**
     * The type of mask extracted from the paint (which should now be recorded).
     */
    private String recordingMaskType;
    
    /**
     * Constructor for the dynamic instantiation and execution of the PaintToMask script action.
     */
    public ActionPaintToMask() {}
    
    /**
     * Constructor used to record the PaintToMask script action line.
     * @param inputImage  The image whose paint was extracted to a mask image.
     * @param maskType    The type of the extracted mask image.
     */
    public ActionPaintToMask(ModelImage inputImage, String maskType) {
        recordingInputImage = inputImage;
        recordingMaskType = maskType;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(ParameterFactory.newImage(INPUT_IMAGE_LABEL, recordingInputImage.getImageName()));
            parameters.put(ParameterFactory.newString(MASK_DATA_TYPE, recordingMaskType));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating parameters while recording PaintToMask script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("PaintToMask", parameters);
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        try {
            String maskType = parameters.getString(MASK_DATA_TYPE);
            ModelImage inputImage = parameters.getImage(INPUT_IMAGE_LABEL);
            String maskImageName;
            
            if (maskType.equals(MASK_SHORT)) {
                maskImageName = inputImage.getParentFrame().getComponentImage().commitPaintToMask();
            } else {
                MipavUtil.displayError("Unrecognized mask data type: " + maskType);
                return;
            }
            
            ScriptRunner.getReference().getImageTable().storeImageName(maskImageName);
        } catch (OutOfMemoryError oome) {
            MipavUtil.displayError("Out of memory: unable to create new mask image from paint.");
            return;
        }
    }
    
    /**
     * Changes the image whose paint to mask extraction should be recorded in the script.
     * @param inputImage  The image whose paint was extracted.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
    
    /**
     * Changes the data type of the paint extraction we want to record.
     * @param maskType  The type of mask extracted from an image's paint.
     */
    public void setMaskType(String maskType) {
        recordingMaskType = maskType;
    }
}
