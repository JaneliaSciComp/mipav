import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.AlgorithmFuzzyCMeans;


/**
 * Process an image with the following algorithms:<br>
 * ISN, IHN3 correction, Fuzzy C-Means.
 */
public class PlugInAlgorithmStartProcessing extends AlgorithmBase {
    private ModelImage imageA = null; // srcImage --> imageA

    private ModelImage resultImage = null;

    private ModelImage[] resultImageFUZZY = null;

    private ModelImage fieldImg = null;

    /**
     * Default constructor
     * @param resultImage the image to put the processed image into
     * @param imageA the source image
     */
    public PlugInAlgorithmStartProcessing(ModelImage resultImage, ModelImage imageA) {
        super(resultImage, imageA);
        this.imageA = imageA;
        this.resultImage = resultImage;
    }

    /**
     * Run method for algorithm
     */
    public void runAlgorithm() {
        process();
    }

    /**
     * Run method for algorithm
     */
    public void process() {
        // intensity normalization
        PlugInAlgorithmISN algISN = null;
        algISN = new PlugInAlgorithmISN(resultImage, imageA);
        algISN.run();
        /***************************************************************************************************************
         * **separate thighs ????
         */
        // N3 correction
        AlgorithmIHN3Correction n3Correct = null;
        n3Correct = new AlgorithmIHN3Correction(resultImage, fieldImg, imageA, 100f, 150, 0.0001f, 33.33f, 4f, 0.2f,
                0.01f, false, false, false);
        n3Correct.run();
        /*
         * n3Correct = new AlgorithmIHN3Correction(destImg,fieldImg,srcImg,float _threshold, int _maxIters, float
         * _endTol, float _fieldDistance,float _shrink, float _kernelfwhm,float _noise, boolean maskFlag, boolean
         * _autoThreshold, boolean useScript)
         */
        // fuzzy segment into bkgrd, muscle, fat
        AlgorithmFuzzyCMeans fuzzyC = null;
        fuzzyC = new AlgorithmFuzzyCMeans(resultImageFUZZY, imageA, 3, 4, 1, 2, 2f, 20000f, 200000f, false, 2, false,
                0f, 200, 0.01f, true);
        fuzzyC.run();
        // int segmentation = 2, for HARD segmentation.
        // ???? pyramidLevels, jacobiIters1,2, smooth1,smooth2, outputGainField, ????? --- used default: 4, 1, 2, 20000,
        // 200000.
        /*
         * public AlgorithmFuzzyCMeans( ModelImage[] destImg, ModelImage srcImg, int _nClass, int _pyramidLevels, int
         * _jacobiIters1, int _jacobiIters2, float _q, float _smooth1, float _smooth2, boolean _outputGainField, int
         * _segmentation, boolean _cropBackground, float _threshold, int _max_iter, float _tolerance, boolean
         * _wholeImage )
         */
        // clean image
        /*
         * clean boundaries AlgorithmCleanBnds cleanBnds = null; cleanBnds = new AlgorithmCleanBnds(destImg, srcImg);
         * cleanBnds.run();
         */
    }

    /**
     * Clean up the algorithm's memory.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Clean up the algorithm's memory.
     */
    public void disposeLocal() {
        imageA = null;
    }
}
// RESULT SO FAR SHOULD BE (W/O CLEAN IMAGE) resultImage, resultImageTEMP
