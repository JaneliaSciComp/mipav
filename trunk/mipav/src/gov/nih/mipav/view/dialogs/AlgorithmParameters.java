package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.components.*;


/**
 * <p>
 * This class standardizes the parameter names given to many common parameters used in algorithms. It also provides
 * helper methods to store/retrieve those common parameters.
 * </p>
 * 
 * @see gov.nih.mipav.view.dialogs.JDialogGaussianBlur
 */
public class AlgorithmParameters {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /**
     * The base of parameter labels used for an algorithm's input image(s). Append a '_1' to get the first image, for
     * example.
     * 
     * @see #getInputImageLabel(int)
     */
    public static final String INPUT_IMAGE_LABEL_BASE = "input_image";

    /** Label used for the parameter indicating whether an algorithm should output a new image. */
    public static final String DO_OUTPUT_NEW_IMAGE = "do_output_new_image";

    /**
     * Label used for the parameter indicating whether the whole image should be processed (<code>true</code>), or
     * just areas within VOIs (<code>false</code>).
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

    /** Label used for the parameter indicating the WaterShed ITK parameters to be used. */
    public static final String WATERSHED = "watershed_itk";

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

    /** Label used for itk filters */
    public static final String ITK_FILTER_NAME = "itk_filter_name";

    /** Label used in ActionDiscovery system for main result image of an algorithm. */
    public static final String RESULT_IMAGE = "result_image";
    
    /** Label used for boolean param indicating whether the algorithm should try to use OpenCL. */
    public static final String USE_OPENCL = "use_opencl";

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

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

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

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
     * @param parsedParams stored parameters which should be used to set up the algorithm's variables/GUI.
     */
    public AlgorithmParameters(final ParameterTable parsedParams) {
        params = parsedParams;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns the label which should be used for an input image parameter.
     * 
     * @param inputImageNumber Number indicating which input image this is.
     * 
     * @return The label/name which should be used for this input image in the parameter table.
     */
    public static String getInputImageLabel(final int inputImageNumber) {
        return AlgorithmParameters.INPUT_IMAGE_LABEL_BASE + "_" + inputImageNumber;
    }

    /**
     * Store an image in the script recorder image variable table. Used to store input/output images while recording a
     * script. Should not be used while running a script.
     * 
     * @param image The image to store in the variable table.
     * 
     * @return The image placeholder variable assigned to the image by the variable table.
     */
    public String storeImageInRecorder(final ModelImage image) {
        return ScriptRecorder.getReference().storeImage(image.getImageName());
    }

    /**
     * Store an image in the script runner image variable table. Used to store input/output images while running a
     * script. Should not be used while recording a script.
     * 
     * @param image The image to store in the variable table.
     * 
     * @return The image placeholder variable assigned to the image by the variable table.
     */
    public static String storeImageInRunner(final ModelImage image) {
        return ScriptRunner.getReference().storeImage(image.getImageName());
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean doOutputNewImage() {
        return params.getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean doProcess3DAs25D() {
        return params.getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean[] doProcessRGB() {
        return params.getList(AlgorithmParameters.DO_PROCESS_RGB).getAsBooleanArray();
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean doProcessSeparable() {
        return params.getBoolean(AlgorithmParameters.DO_PROCESS_SEPARABLE);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean doProcessWholeImage() {
        return params.getBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean doSigmaResolutionCorrection() {
        return params.getBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getNumIterations() {
        return params.getInt(AlgorithmParameters.NUM_ITERATIONS);
    }

    /**
     * Retrieve the Itk filter's name.
     * 
     * @return name The filter's base name.
     * 
     */
    public String getItkFilterName() {
        return params.getString(AlgorithmParameters.ITK_FILTER_NAME);
    }

    /**
     * Retrieves the list of parameters to either be used to run the script or store settings for future executions of
     * the algorithm in a script.
     * 
     * @return The table of parameters for this algorithm.
     */
    public ParameterTable getParams() {
        return params;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public float[] getUnnormalizedSigmas() {
        return params.getList(AlgorithmParameters.SIGMAS).getAsFloatArray();
    }

    /**
     * Returns an image with a particular parameter name.
     * 
     * @param imageParamLabel The parameter label of the image to retrieve.
     * 
     * @return The requested image.
     */
    public ModelImage retrieveImage(final String imageParamLabel) {
        return params.getImage(imageParamLabel);
    }

    /**
     * Returns the first input image for the algorithm.
     * 
     * @return The first input image.
     * 
     * @see #getInputImageLabel(int)
     */
    public ModelImage retrieveInputImage() {
        return retrieveImage(AlgorithmParameters.getInputImageLabel(1));
    }

    /**
     * Returns one of the algorithm's input images.
     * 
     * @param inputImageNumber A number indicating which input image should be retrieved.
     * 
     * @return The <code>inputImageNumber</code>-th input image.
     */
    public ModelImage retrieveInputImage(final int inputImageNumber) {
        return retrieveImage(AlgorithmParameters.getInputImageLabel(inputImageNumber));
    }

    /**
     * Sets the color channel GUI panel, based on the script action's parameters. This is a helper function used to
     * handle parameters common to many algorithms.
     * 
     * @param colorChannelPanel The color channel panel to set the values of.
     */
    public void setColorOptionsGUI(final JPanelColorChannels colorChannelPanel) {
        final boolean[] rgb = params.getList(AlgorithmParameters.DO_PROCESS_RGB).getAsBooleanArray();
        colorChannelPanel.setRedProcessingRequested(rgb[0]);
        colorChannelPanel.setGreenProcessingRequested(rgb[1]);
        colorChannelPanel.setBlueProcessingRequested(rgb[2]);
    }

    /**
     * Sets up the algorithm output options panel components (whole image vs. VOI and new image vs. replace), based on
     * the script action's parameters. This is a helper function used to handle parameters common to many algorithms.
     * 
     * @param outputOptionsPanel The algorithm output options panel to set the values of.
     */
    public void setOutputOptionsGUI(final JPanelAlgorithmOutputOptions outputOptionsPanel) {
        outputOptionsPanel.setOutputNewImage(doOutputNewImage());
        outputOptionsPanel.setProcessWholeImage(doProcessWholeImage());
    }

    /**
     * Sets up the GUI containing the sigmas used in an algorithm (usually Gaussian sigmas), based on the script
     * action's parameters. This is a helper function used to handle parameters common to many algorithms.
     * 
     * @param sigmaPanel The sigmas panel to set up based on the parameters.
     */
    public void setSigmasGUI(final JPanelSigmas sigmaPanel) {
        final float[] sigmas = params.getList(AlgorithmParameters.SIGMAS).getAsFloatArray();
        sigmaPanel.setSigmaX(sigmas[0]);
        sigmaPanel.setSigmaY(sigmas[1]);
        sigmaPanel.setSigmaZ(sigmas[2]);
        sigmaPanel.enableResolutionCorrection(params.getBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION));
    }

    /**
     * Sets up the GUI containing the sigmas used in an algorithm WaterShedITK, based on the script action's parameters.
     * This is a helper function used to handle parameters common to many algorithms.
     * 
     * @param waterShedPanel The sigmas panel to set up based on the parameters.
     */
    public void setSigmasGUI(final JPanelWaterShedITK waterShedPanel) {
        final float[] waterShedParams = params.getList(AlgorithmParameters.WATERSHED).getAsFloatArray();
        waterShedPanel.setConductance(waterShedParams[0]);
        waterShedPanel.setIterations(waterShedParams[1]);
        waterShedPanel.setThreshold(waterShedParams[2]);
        waterShedPanel.setLevel(waterShedParams[3]);
        waterShedPanel.setTimeStep(waterShedParams[4]);
    }

    /**
     * Call set method for Itk filter based on values in params table.
     * 
     * @param itkPanel panel to set up.
     */
    public void setItkFilterParamGUI(final JPanelItkFilterParams itkPanel) {
        itkPanel.runFromScript(this);
    }

    /**
     * Stores the values from the color channel panel in the parameter table. This function is used when recording a
     * script. This is a helper function used to handle parameters common to many algorithms.
     * 
     * @param colorChannelPanel The color channel panel to extract the options from.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeColorOptions(final JPanelColorChannels colorChannelPanel) throws ParserException {
        storeColorOptions(colorChannelPanel.isRedProcessingRequested(), colorChannelPanel.isGreenProcessingRequested(),
                colorChannelPanel.isBlueProcessingRequested());
    }

    /**
     * Stores the values of color channel processing options in the parameter table. This function is used when
     * recording a script. This is a helper function used to handle parameters common to many algorithms.
     * 
     * @param processRed Whether the red channel of a color image should be processed.
     * @param processGreen Whether the green channel of a color image should be processed.
     * @param processBlue Whether the blue channel of a color image should be processed.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeColorOptions(final boolean processRed, final boolean processGreen, final boolean processBlue)
            throws ParserException {
        final boolean[] rgb = new boolean[] {processRed, processGreen, processBlue};
        params.put(ParameterFactory.newParameter(AlgorithmParameters.DO_PROCESS_RGB, rgb));
    }

    /**
     * Stores an input image in the list of parameters for the algorithm.
     * 
     * @param image The image to store.
     * @param paramLabel The label to give to the new image parameter.
     * 
     * @return The image placeholder variable assigned to the image by the variable table.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public String storeImage(final ModelImage image, final String paramLabel) throws ParserException {
        final boolean isExternalImage = !AlgorithmParameters.isImageStoredInRecorder(image);

        final String var = storeImageInRecorder(image);
        params.put(ParameterFactory.newImage(paramLabel, var, isExternalImage));

        return var;
    }

    /**
     * Stores an input image in the list of parameters for the algorithm. This image is stored with a new parameter
     * label incremented with each new call to this method.
     * 
     * @param inputImage The input image to store.
     * 
     * @return The image placeholder variable assigned to the image by the variable table.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public String storeInputImage(final ModelImage inputImage) throws ParserException {
        final String var = storeImage(inputImage, AlgorithmParameters.getInputImageLabel(currentInputImageLabelNumber));

        currentInputImageLabelNumber++;

        return var;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param numIters DOCUMENT ME!
     * 
     * @throws ParserException DOCUMENT ME!
     */
    public void storeNumIterations(final int numIters) throws ParserException {
        params.put(ParameterFactory.newParameter(AlgorithmParameters.NUM_ITERATIONS, numIters));
    }

    /**
     * Stores information about the output of images for this algorithm in the parameter table. It first stores whether
     * a new output image should be generated by the algorithm, and then (if a new image should be generated) stores the
     * new output image in the variable table.
     * 
     * @param outputImage The result image generated by the algorithm (may be <code>null</code> if <code>
     *                       isNewImage</code>
     *            is <code>false</code>.
     * @param isNewImage Whether the algorithm should output a new image when it is executed.
     * 
     * @return The new image placeholder variable assigned to the result image (or <code>null</code> if no output
     *         image should be generated).
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public String storeOutputImageParams(final ModelImage outputImage, final boolean isNewImage) throws ParserException {
        params.put(ParameterFactory.newBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, isNewImage));

        if (isNewImage) {
            return storeImageInRecorder(outputImage);
        }

        return null;
    }

    /**
     * @see storeProcessingOptions
     * 
     * @param doProcess3DAs25D Whether to process 3D images in 2.5D mode (slice by slice).
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeProcess3DAs25D(final boolean doProcess3DAs25D) throws ParserException {
        params.put(ParameterFactory.newParameter(AlgorithmParameters.DO_PROCESS_3D_AS_25D, doProcess3DAs25D));
    }

    /**
     * Stores options for how an algorithm should process images in the parameter table. This function is used when
     * recording a script. This is a helper function used to handle parameters common to many algorithms.
     * 
     * @param doWholeImage Whether to process the whole image or just VOI regions.
     * @param do25D Whether to process 3D images in 2.5D mode (slice by slice).
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeProcessingOptions(final boolean doWholeImage, final boolean do25D) throws ParserException {
        storeProcessWholeImage(doWholeImage);
        storeProcess3DAs25D(do25D);
    }

    /**
     * Stores options for an algorithm using a kernel that can process dimensions separately.
     * 
     * @param doProcessSep Whether to use a separable filter kernel.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeProcessSeparable(final boolean doProcessSep) throws ParserException {
        params.put(ParameterFactory.newParameter(AlgorithmParameters.DO_PROCESS_SEPARABLE, doProcessSep));
    }

    /**
     * Stores whether the whole image or just VOI regions should be processed in the parameter table. This function is
     * used when recording a script. This is a helper function used to handle parameters common to many algorithms.
     * 
     * @see storeProcessingOptions
     * 
     * @param doWholeImage Whether to process the whole image or just VOI regions.
     * 
     * @throws ParserException If there is a problem creating one of the new parameter.
     */
    public void storeProcessWholeImage(final boolean doWholeImage) throws ParserException {
        params.put(ParameterFactory.newBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, doWholeImage));
    }

    /**
     * Stores an algorithm's sigmas in the parameter table. This function is used when recording a script. This is a
     * helper function used to handle parameters common to many algorithms.
     * 
     * @param sigmaPanel The sigma GUI panel to extract the parameters from.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeSigmas(final JPanelSigmas sigmaPanel) throws ParserException {
        storeSigmas(sigmaPanel.getUnnormalized3DSigmas(), sigmaPanel.isResolutionCorrectionEnabled());
    }

    /**
     * Stores an algorithm's parameters in the parameter table. This function is used when recording a script. This is
     * for the WaterShed algorithm.
     * 
     * @param waterShedPanel The WaterShed ITK panel to extract the parameters from.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeWaterShed(final JPanelWaterShedITK waterShedPanel) throws ParserException {
        params.put(ParameterFactory.newParameter(AlgorithmParameters.WATERSHED, waterShedPanel.getParameters()));
    }

    /**
     * Stores an algorithm's sigmas in the parameter table. This function is used when recording a script. This is a
     * helper function used to handle parameters common to many algorithms.
     * 
     * @param sigmas The unnormalized sigmas (must be a 3D array).
     * @param isZCorrectionEnabled Whether adjustment of the Z sigma should be performed based on the ratio of x
     *            resolution to z resolution.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeSigmas(final float[] sigmas, final boolean isZCorrectionEnabled) throws ParserException {
        params.put(ParameterFactory.newParameter(AlgorithmParameters.SIGMAS, sigmas));
        params.put(ParameterFactory.newBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, isZCorrectionEnabled));
    }

    /**
     * Stores an algorithm's sigmas in the parameter table. This function is used when recording a script. This is a for
     * the WaterShed algorithm.
     * 
     * @param sigmas The unnormalized sigmas (must be a 3D array).
     * @param isZCorrectionEnabled Whether adjustment of the Z sigma should be performed based on the ratio of x
     *            resolution to z resolution.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeWaterShed(final float[] sigmas, final boolean isZCorrectionEnabled) throws ParserException {
        params.put(ParameterFactory.newParameter(AlgorithmParameters.SIGMAS, sigmas));
        params.put(ParameterFactory.newBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, isZCorrectionEnabled));
    }

    /**
     * Stores an Itk filter's name.
     * 
     * @param name The filter's base name.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeItkFilterName(final String name) throws ParserException {
        params.put(ParameterFactory.newParameter(AlgorithmParameters.ITK_FILTER_NAME, name));
    }

    /**
     * Stores an algorithm's parameters in the parameter table. This function is used when recording a script. This is
     * for ITK filter algorithms.
     * 
     * @param itkFilterPanel The panel to extract the parameters from.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    public void storeItkMethods(final JPanelItkFilterParams itkFilterPanel) throws ParserException {
        itkFilterPanel.putScriptParams(this);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param resultImage DOCUMENT ME!
     * 
     * @throws ParserException DOCUMENT ME!
     */
    public void tryToStoreResultImageInRunner(final ModelImage resultImage) throws ParserException {

        if (params.containsParameter(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE) && doOutputNewImage()) {

            if (resultImage == null) {
                throw new ParserException("", 0,
                        "A script action wants to register a result image, but none was found after executing the action.");
            }

            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * Returns whether an image has been registered in the script recorder. If it has not been used, it must be
     * specified externally when this script is run later.
     * 
     * @param image The image to look for in the recorder's image table.
     * 
     * @return <code>True</code> if the image has been stored in the recorder's image table, <code>false</code>
     *         otherwise.
     */
    protected static boolean isImageStoredInRecorder(final ModelImage image) {
        return ScriptRecorder.getReference().getImageTable().isImageStored(image.getImageName());
    }
}
