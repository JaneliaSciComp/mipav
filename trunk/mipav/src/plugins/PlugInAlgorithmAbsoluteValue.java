
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.io.*;


/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

public class PlugInAlgorithmAbsoluteValue extends AlgorithmBase {

    private ModelImage srcImage = null;
    private float [] imgBuffer = null;


    /**
     * Default constructor
     * @param srcImage the source image
     */
    public PlugInAlgorithmAbsoluteValue(ModelImage srcImage) {
        super(null, srcImage);
        this.srcImage = srcImage;
    }

    /**
     * Run method for algorithm
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

        buildProgressBar(srcImage.getImageName(), "Calculating absolute intensities ...", 0, 100);

        initProgressBar();




        for (i = 0; i < z; i++) {

            try {
                srcImage.exportData( (i * imgBuffer.length), imgBuffer.length, imgBuffer);
                for (j = 0; j < imgBuffer.length; j++) {
                    imgBuffer[j] = Math.abs(imgBuffer[j]);
                }
                srcImage.importData((i * imgBuffer.length), imgBuffer, false);
                if (isProgressBarVisible())
                    progressBar.updateValue(Math.round((float)((i+1.0)/z) * 100), activeImage);
            }
            catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmAbsoluteValue");
            }
        }
        srcImage.calcMinMax();
        srcImage.notifyImageDisplayListeners();

        disposeProgressBar();

        setCompleted(true);
    }

    public void finalize() {
        disposeLocal();
        super.finalize();
    }
    public void disposeLocal() {
        imgBuffer = null;
    }


}
