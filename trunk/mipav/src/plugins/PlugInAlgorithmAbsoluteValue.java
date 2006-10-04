
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

public class PlugInAlgorithmAbsoluteValue extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private ModelImage srcImage = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  srcImage  the source image
     */
    public PlugInAlgorithmAbsoluteValue(ModelImage srcImage) {
        super(null, srcImage);
        this.srcImage = srcImage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        imgBuffer = null;
    }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Run method for algorithm.
     */
    public void runAlgorithm() {

        int i, j;
        int z = 1;
        int colorFactor = 1;

        if (srcImage.isColorImage()) {
            colorFactor = 4;
        }

        imgBuffer = new float[srcImage.getExtents()[0] * srcImage.getExtents()[1] * colorFactor];

        if (srcImage.getNDims() == 3) {
            z = srcImage.getExtents()[2];
        }

        for (i = 0; i < z; i++) {

            try {
                srcImage.exportData((i * imgBuffer.length), imgBuffer.length, imgBuffer);

                for (j = 0; j < imgBuffer.length; j++) {
                    imgBuffer[j] = Math.abs(imgBuffer[j]);
                }

                srcImage.importData((i * imgBuffer.length), imgBuffer, false);

              
                fireProgressStateChanged(Math.round((float) ((i + 1.0) / z) * 100));
                
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmAbsoluteValue");
            }
        }

        srcImage.calcMinMax();
        srcImage.notifyImageDisplayListeners();

        setCompleted(true);
    }


}
