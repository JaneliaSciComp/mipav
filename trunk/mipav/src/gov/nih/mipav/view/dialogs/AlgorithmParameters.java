package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.components.*;


/**
 * <p>This class standardizes the parameter names given to many common parameters used in algorithms. It also provides
 * helper methods to store/retrieve those common parameters.</p>
 *
 * @see  gov.nih.mipav.view.dialogs.JDialogGaussianBlur
 */
public class AlgorithmParameters {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * The base of parameter labels used for an algorithm's input image(s). Append a '_1' to get the first image, for
     * example.
     *
     * @see  #getInputImageLabel(int)
     */
    public static final String INPUT_IMAGE_LABEL_BASE = "input_image";

    /** Label used for the parameter indicating whether an algorithm should output a new image. */
    public static final String DO_OUTPUT_NEW_IMAGE = "do_output_new_image";

    /**
     * Label used for the parameter indicating whether the whole image should be processed (<code>true</code>), or just
     * areas within VOIs (<code>false</code>).
     */
    public static final String DO_PROCESS_WHOLE_IMAGE = "do_process_whole_image";

    /** Label used for the parameter indicating whether separable convolution kernels should be used in an algorithm. */
    public static final String DO_PROCESS_SEPARABLE = "do_separable_convolution";

    /**
     * Label used for the parameter indicating whether an algorithm should process a 3D image as a set of 2D slices
     * instead of a 3D volume.
     */
    public static final String DO_PROCESS_3D_AS_25D = "do_process_in_2.5D";

    /** Label used for the parameter indicating the unnormalized/uncorrected gaussian standard deviation to be used. */
    public static final String SIGMAS = "gauss_std_dev";

    /**
     * Label used for the parameter indicating whether to correct the gaussian's standard deviation Z dimension using
     * the ratio of the X and Z resolutions.
     */
    public static final String SIGMA_DO_Z_RES_CORRECTION = "gauss_do_z_resolution_correction";

    /**
     * Label used for the parameter indicating whether to process the each channel of an RGB image (ignored for
     * grayscale images).
     */
    public static final String DO_PROCESS_RGB = "do_process_r_g_b_channel";

    /** Label used for the parameter indicating the number of iterations to perform in a script action. */
    public static final String NUM_ITERATIONS = "num_iterations";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Counter used to track the number of input images stored (aka used) by an algorithm. This is only used while
     * recording parameters from an algorithm's GUI.
     */
    protected int currentInputImageLabelNumber = 1;

    /**
     * The list of parameters selected by the user in an algorithm's dialog, or the list of parameters to use to setup
     * the dialog GUI. This depends on whether this object is being used to store parameters or to make use of
     * previously stored parameters.
     */
    protected ParameterTable params;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmParameters object to be used to record the current parameters entered into the algorithm's
     * GUI by the user.
     */
    public AlgorithmParameters() {
        params = new ParameterTable();
    }

    /**
     * Creates a new AlgorithmParameters object to be used to set up the algorithm's GUI from stored parameters.
     *
     * @param  parsedParams  stored parameters which should be used to set up the algorithm's variables/GUI.
     */
    public AlgorithmParameters(ParameterTable parsedParams) {
        params = parsedParams;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the label which should be used for an input image parameter.
     *
     * @param   inputImageNumber  Number indicating which input image this is.
     *
     * @return  The label/name which should be used for this input image in the parameter table.
     */
    public static final String getInputImageLabel(int inputImageNumber) {
        return INPUT_IMAGE_LABEL_BASE + "_" + inputImageNumber;
    }

    /**
     * Store an image in the script recorder image variable table. Used to store input/output images while recording a
     * script. Should not be used while running a script.
     *
     * @param   image  The image to store in the variable table.
     *
     * @return  The image placeholder variable assigned to the image by the variable table.
     */
    public static final String storeImageInRecorder(ModelImage image) {
        return ScriptRecorder.getReference().storeImage(image.getImageName());
    }

    /**
     * Store an image in the script runner image variable table. Used to store input/output images while running a
     * script. Should not be used while recording a script.
     *
     * @param   image  The image to store in the variable table.
     *
     * @return  The image placeholder variable assigned to the image by the variable table.
     */
    public static final String storeImageInRunner(ModelImage image) {
        return ScriptRunner.getReference().storeImage(image.getImageName());
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean doOutputNewImage() {
        return params.getBoolean(DO_OUTPUT_NEW_IMAGE);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean doProcess3DAs25D() {
        return params.getBoolean(DO_PROCESS_3D_AS_25D);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean[] doProcessRGB() {
        return params.getList(DO_PROCESS_RGB).getAsBooleanArray();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean doProcessSeparable() {
        return params.getBoolean(DO_PROCESS_SEPARABLE);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean doProcessWholeImage() {
        return params.getBoolean(DO_PROCESS_WHOLE_IMAGE);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean doSigmaResolutionCorrection() {
        return params.getBoolean(SIGMA_DO_Z_RES_CORRECTION);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getNumIterations() {
        return params.getInt(NUM_ITERATIONS);
    }

    /**
     * Retrieves the list of parameters to either be used to run the script or store settings for future executions of
     * the algorithm in a script.
     *
     * @return  The table of parameters for this algorithm.
     */
    public ParameterTable getParams() {
        return params;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getUnnormalizedSigmas() {
        return params.getList(SIGMAS).getAsFloatArray();
    }

    /**
     * Returns an image with a particular parameter name.
     *
     * @param   imageParamLabel  The parameter label of the image to retrieve.
     *
     * @return  The requested image.
     */
    public ModelImage retrieveImage(String imageParamLabel) {
        return params.getImage(imageParamLabel);
    }

    /**
     * Returns the first input image for the algorithm.
     *
     * @return  The first input image.
     *
     * @see     #getInputImageLabel(int)
     */
    public ModelImage retrieveInputImage() {
        return retrieveImage(getInputImageLabel(1));
    }

    /**
     * Returns one of the algorithm's input images.
     *
     * @param   inputImageNumber  A number indicating which input image should be retrieved.
     *
     * @return  The <code>inputImageNumber</code>-th input image.
     */
    public ModelImage retrieveInputImage(int inputImageNumber) {
        return retrieveImage(getInputImageLabel(inputImageNumber));
    }

    /**
     * Sets the color channel GUI panel, based on the script action's parameters. This is a helper function used to
     * handle parameters common to many algorithms.
     *
     * @param  colorChannelPanel  The color channel panel to set the values of.
     */
    public void setColorOptionsGUI(JPanelColorChannels colorChannelPanel) {
        boolean[] rgb = params.getList(DO_PROCESS_RGB).getAsBooleanArray();
        colorChannelPanel.setRedProcessingRequested(rgb[0]);
        colorChannelPanel.setGreenProcessingRequested(rgb[1]);
        colorChannelPanel.setBlueProcessingRequested(rgb[2]);
    }

    /**
     * Sets up the algorithm output options panel components (whole image vs. VOI and new image vs. replace), based on
     * the script action's parameters. This is a helper function used to handle parameters common to many algorithms.
     *
     * @param  outputOptionsPanel  The algorithm output options panel to set the values of.
     */
    public void setOutputOptionsGUI(JPanelAlgorithmOutputOptions outputOptionsPanel) {
        outputOptionsPanel.setOutputNewImage(doOutputNewImage());
        outputOptionsPanel.setProcessWholeImage(doProcessWholeImage());
    }

    /**
     * Sets up the GUI containing the sigmas used in an algorithm (usually Gaussian sigmas), based on the script
     * action's parameters. This is a helper function used to handle parameters common to many algorithms.
     *
     * @param  sigmaPanel  The sigmas panel to set up based on the parameters.
     */
    public void setSigmasGUI(JPanelSigmas sigmaPanel) {
        float[] sigmas = params.getList(SIGMAS).getAsFloatArray();
        sigmaPanel.setSigmaX(sigmas[0]);
        sigmaPanel.setSigmaY(sigmas[1]);
        sigmaPanel.setSigmaZ(sigmas[2]);
        sigmaPanel.enableResolutionCorrection(params.getBoolean(SIGMA_DO_Z_RES_CORRECTION));
    }

    /**
     * Stores the values from the color channel panel in the parameter table. This function is used when recording a
     * script. This is a helper function used to handle parameters common to many algorithms.
     *
     * @param   colorChannelPanel  The color channel panel to extract the options from.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    public void storeColorOptions(JPanelColorChannels colorChannelPanel) throws ParserException {
        storeColorOptions(colorChannelPanel.isRedProcessingRequested(), colorChannelPanel.isGreenProcessingRequested(),
                          colorChannelPanel.isBlueProcessingRequested());
    }

    /**
     * Stores the values of color channel processing options in the parameter table. This function is used when
     * recording a script. This is a helper function used to handle parameters common to many algorithms.
     *
     * @param   processRed    Whether the red channel of a color image should be processed.
     * @param   processGreen  Whether the green channel of a color image should be processed.
     * @param   processBlue   Whether the blue channel of a color image should be processed.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    public void storeColorOptions(boolean processRed, boolean processGreen, boolean processBlue)
            throws ParserException {
        boolean[] rgb = new boolean[] { processRed, processGreen, processBlue };
        params.put(ParameterFactory.newParameter(DO_PROCESS_RGB, rgb));
    }

    /**
     * Stores an input image in the list of parameters for the algorithm.
     *
     * @param   image       The image to store.
     * @param   paramLabel  The label to give to the new image parameter.
     *
     * @return  The image placeholder variable assigned to the image by the variable table.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    public String storeImage(ModelImage image, String paramLabel) throws ParserException {
        boolean isExternalImage = !isImageStoredInRecorder(image);

        String var = storeImageInRecorder(image);
        params.put(ParameterFactory.newImage(paramLabel, var, isExternalImage));

        return var;
    }

    /**
     * Stores an input image in the list of parameters for the algorithm. This image is stored with a new parameter
     * label incremented with each new call to this method.
     *
     * @param   inputImage  The input image to store.
     *
     * @return  The image placeholder variable assigned to the image by the variable table.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    public String storeInputImage(ModelImage inputImage) throws ParserException {
        String var = storeImage(inputImage, getInputImageLabel(currentInputImageLabelNumber));

        currentInputImageLabelNumber++;

        return var;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   numIters  DOCUMENT ME!
     *
     * @throws  ParserException  DOCUMENT ME!
     */
    public void storeNumIterations(int numIters) throws ParserException {
        params.put(ParameterFactory.newParameter(NUM_ITERATIONS, numIters));
    }

    /**
     * Stores information about the output of images for this algorithm in the parameter table. It first stores whether
     * a new output image should be generated by the algorithm, and then (if a new image should be generated) stores the
     * new output image in the variable table.
     *
     * @param   outputImage  The result image generated by the algorithm (may be <code>null</code> if <code>
     *                       isNewImage</code> is <code>false</code>.
     * @param   isNewImage   Whether the algorithm should output a new image when it is executed.
     *
     * @return  The new image placeholder variable assigned to the result image (or <code>null</code> if no output image
     *          should be generated).
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    public String storeOutputImageParams(ModelImage outputImage, boolean isNewImage) throws ParserException {
        params.put(ParameterFactory.newBoolean(DO_OUTPUT_NEW_IMAGE, isNewImage));

        if (isNewImage) {
            return storeImageInRecorder(outputImage);
        }

        return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   doProcess3DAs25D  DOCUMENT ME!
     *
     * @throws  ParserException  DOCUMENT ME!
     */
    public void storeProcess3DAs25D(boolean doProcess3DAs25D) throws ParserException {
        params.put(ParameterFactory.newParameter(DO_PROCESS_3D_AS_25D, doProcess3DAs25D));
    }

    /**
     * Stores options for how an algorithm should process images in the parameter table. This function is used when
     * recording a script. This is a helper function used to handle parameters common to many algorithms.
     *
     * @param   doWholeImage  Whether to process the whole image or just VOI regions.
     * @param   do25D         Whether to process 3D images in 2.5D mode (slice by slice).
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    public void storeProcessingOptions(boolean doWholeImage, boolean do25D) throws ParserException {
        storeProcessWholeImage(doWholeImage);
        storeProcess3DAs25D(do25D);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   doProcessSep  DOCUMENT ME!
     *
     * @throws  ParserException  DOCUMENT ME!
     */
    public void storeProcessSeparable(boolean doProcessSep) throws ParserException {
        params.put(ParameterFactory.newParameter(DO_PROCESS_SEPARABLE, doProcessSep));
    }

    /**
     * Stores whether the whole image or just VOI regions should be processed in the parameter table. This function is
     * used when recording a script. This is a helper function used to handle parameters common to many algorithms.
     *
     * @param   doWholeImage  Whether to process the whole image or just VOI regions.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameter.
     */
    public void storeProcessWholeImage(boolean doWholeImage) throws ParserException {
        params.put(ParameterFactory.newBoolean(DO_PROCESS_WHOLE_IMAGE, doWholeImage));
    }

    /**
     * Stores an algorithm's sigmas in the parameter table. This function is used when recording a script. This is a
     * helper function used to handle parameters common to many algorithms.
     *
     * @param   sigmaPanel  The sigma GUI panel to extract the parameters from.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    public void storeSigmas(JPanelSigmas sigmaPanel) throws ParserException {
        storeSigmas(sigmaPanel.getUnnormalized3DSigmas(), sigmaPanel.isResolutionCorrectionEnabled());
    }

    /**
     * Stores an algorithm's sigmas in the parameter table. This function is used when recording a script. This is a
     * helper function used to handle parameters common to many algorithms.
     *
     * @param   sigmas                The unnormalized sigmas (must be a 3D array).
     * @param   isZCorrectionEnabled  Whether adjustment of the Z sigma should be performed based on the ratio of x
     *                                resolution to z resolution.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    public void storeSigmas(float[] sigmas, boolean isZCorrectionEnabled) throws ParserException {
        params.put(ParameterFactory.newParameter(SIGMAS, sigmas));
        params.put(ParameterFactory.newBoolean(SIGMA_DO_Z_RES_CORRECTION, isZCorrectionEnabled));
    }

    /**
     * DOCUMENT ME!
     *
     * @param   resultImage  DOCUMENT ME!
     *
     * @throws  ParserException  DOCUMENT ME!
     */
    public void tryToStoreResultImageInRunner(ModelImage resultImage) throws ParserException {

        if (params.containsParameter(DO_OUTPUT_NEW_IMAGE) && doOutputNewImage()) {

            if (resultImage == null) {
                throw new ParserException("", 0,
                                          "A script action wants to register a result image, but none was found after executing the action.");
            }

            storeImageInRunner(resultImage);
        }
    }

    /**
     * Returns whether an image has been registered in the script recorder. If it has not been used, it must be
     * specified externally when this script is run later.
     *
     * @param   image  The image to look for in the recorder's image table.
     *
     * @return  <code>True</code> if the image has been stored in the recorder's image table, <code>false</code>
     *          otherwise.
     */
    protected static final boolean isImageStoredInRecorder(ModelImage image) {
        return ScriptRecorder.getReference().getImageTable().isImageStored(image.getImageName());
    }
}
