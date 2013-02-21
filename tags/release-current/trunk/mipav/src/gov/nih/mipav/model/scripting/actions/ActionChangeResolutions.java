package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * A script action which changes the image's resolutions.
 */
public class ActionChangeResolutions extends ActionImageProcessorBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The label to use for the parameter indicating the new resolutions. */
    private static final String IMAGE_RESOLUTIONS = "image_resolutions";

    /** The label to use for the parameter indicating whether to change all slices. */
    private static final String RESOLUTIONS_ALL = "resolutions_all";

    /**
     * The label to use for the parameter indicating the index of the slice from which to set/retrieve the resolutions.
     */
    private static final String SLICE_INDEX = "slice_index";

    /** The label to use for the parameter indicating the slice thickness. */
    private static final String SLICE_THICKNESS = "slice_thickness";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Whether or not to change all fileinfos/slices. */
    private boolean changeAll;

    /** index of the slice from which to retrieve the resolutions. */
    private int sliceIndex;

    /** the slice thickness. */
    private float sliceThickness = 0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for the dynamic instantiation and execution of the script action.
     */
    public ActionChangeResolutions() {
        super();
    }

    /**
     * Main constructor with parameters for changing the resolution/slice thickness.
     *
     * @param  image            DOCUMENT ME!
     * @param  change_all       change for all slices
     * @param  slice_index      index of slice from which to get resolution change
     * @param  slice_thickness  the slice thickness
     */
    public ActionChangeResolutions(ModelImage image, boolean change_all, int slice_index, float slice_thickness) {
        super(image);
        this.changeAll = change_all;
        this.sliceIndex = slice_index;
        this.sliceThickness = slice_thickness;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {

        ParameterTable parameters = new ParameterTable();

        try {
            parameters.put(createInputImageParameter(isScript));
            parameters.put(ParameterFactory.newParameter(IMAGE_RESOLUTIONS,
                                                         recordingInputImage.getResolutions(sliceIndex)));
            parameters.put(ParameterFactory.newParameter(SLICE_INDEX, sliceIndex));
            parameters.put(ParameterFactory.newParameter(RESOLUTIONS_ALL, changeAll));
            parameters.put(ParameterFactory.newParameter(SLICE_THICKNESS, sliceThickness));
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
        boolean isDicom = inputImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM;

        float[] resolutions = parameters.getList(IMAGE_RESOLUTIONS).getAsFloatArray();
        boolean changeAll = parameters.getBoolean(RESOLUTIONS_ALL);
        int sliceIndex = parameters.getInt(SLICE_INDEX);
        float sliceThickness = parameters.getFloat(SLICE_THICKNESS);

        int nDims = inputImage.getNDims();

        String dicomString = null;

        if (isDicom) {
            dicomString = String.valueOf(resolutions[1]) + "\\" + String.valueOf(resolutions[0]);
        }

        int zDim = 1;

        if (nDims > 2) {
            zDim = inputImage.getExtents()[2];
        }

        if (nDims == 4) {
            zDim *= inputImage.getExtents()[3];
        }

        if (inputImage.getExtents().length == resolutions.length) {

            if (changeAll) {

                for (int i = 0; i < zDim; i++) {
                    inputImage.getFileInfo()[i].setResolutions(resolutions);
                    inputImage.getFileInfo()[i].setSliceThickness(sliceThickness);

                    if (isDicom) {
                        ((FileInfoDicom) (inputImage.getFileInfo()[i])).getTagTable().setValue("0028,0030", dicomString,
                                                                                               dicomString.length());
                    }
                }
            } else {

                if (((nDims > 2) && (sliceIndex < inputImage.getExtents()[2])) || (sliceIndex == 0)) {
                    inputImage.getFileInfo()[sliceIndex].setResolutions(resolutions);
                    inputImage.getFileInfo()[sliceIndex].setSliceThickness(sliceThickness);

                    if (isDicom) {
                        ((FileInfoDicom) (inputImage.getFileInfo()[sliceIndex])).getTagTable().setValue("0028,0030",
                                                                                                        dicomString,
                                                                                                        dicomString.length());
                    }
                }
            }
        }
    }
}
