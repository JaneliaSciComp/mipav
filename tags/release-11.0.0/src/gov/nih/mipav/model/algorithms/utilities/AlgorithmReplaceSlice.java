package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * DOCUMENT ME!
 *
 * @author   not attributable
 * @version  1.0
 */
public class AlgorithmReplaceSlice extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage sliceImage;

    /** DOCUMENT ME! */
    private int sliceNum;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new AlgorithmReplaceSlice object.
     *
     * @param  srcImage    DOCUMENT ME!
     * @param  sliceImage  DOCUMENT ME!
     * @param  sliceNum    DOCUMENT ME!
     */
    public AlgorithmReplaceSlice(ModelImage srcImage, ModelImage sliceImage, int sliceNum) {
        super(null, srcImage);

        this.sliceImage = sliceImage;
        this.sliceNum = sliceNum;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Actually runs the algorithm.
     *
     * @todo  Implement this gov.nih.mipav.model.algorithms.AlgorithmBase method
     */
    public void runAlgorithm() {

        int sliceSize = sliceImage.getExtents()[0] * sliceImage.getExtents()[1];

        if (sliceImage.isColorImage()) {
            sliceSize *= 4;
        }
        else if (sliceImage.isComplexImage()) {
        	sliceSize *= 2;
        }

        float[] sliceData = new float[sliceSize];

        try {
            sliceImage.exportData(0, sliceSize, sliceData);
        } catch (Exception ex) {
            MipavUtil.displayError("Error exporting data from slice");

            return;
        }

        try {
            srcImage.importData((sliceNum * sliceSize), sliceData, true);
        } catch (Exception ex) {
            MipavUtil.displayError("Error replacing slice in source image");

            return;
        }

        setCompleted(true);

        return;
    }

}
