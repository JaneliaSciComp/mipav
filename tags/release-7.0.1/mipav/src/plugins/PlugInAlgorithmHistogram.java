import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

import java.text.DecimalFormat;


/**
 * Calculates the histogram for an image and outputs the histogram to a text file.
 *
 * @version  0.1 November 17, 2008
 * @author   William Gandler
 */
public class PlugInAlgorithmHistogram extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    

    /** If true the histogram should be calculated for the entire image. */
    private boolean entireImage = true;

    /** Reference to the image. */
    private ModelImage image;

    /**
     * Indicates which channel of the RGB image the histogram should be calculated. 1 = Red channel 2 = Green channel 3
     * = Blue channel
     */
    private int RGBOffset;
    
    // If true, histogram covers from userMin to userMax instead of image.getMin() to image.getMax()
    private boolean userLimits = false;
    
    private float userMin;
    
    private float userMax;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructs the histogram calculation object for an image.
     *
     * @param  image        ModelImage the image
     * @param  maskFlag     Flag that indicates that the histogram will be calculated for the whole image if equal to
     *                      true
     * @param userLimits    If true, histogram goes from userMin to userMax instead of image.getMin() to image.getMax()
     * @param userMin
     * @param usermax
     */
    public PlugInAlgorithmHistogram(ModelImage image, boolean maskFlag,
                              boolean userLimits, float userMin, float userMax) {
        this.image = image;
        this.entireImage = maskFlag;
        this.userLimits = userLimits;
        this.userMin = userMin;
        this.userMax = userMax;

        if (entireImage == false) {
            mask = image.generateVOIMask();
        }
    }
    
    
    /**
     * Constructs the histogram calculation object for an RGB image.
     *
     * @param  image        ModelImage the image
     * @param  RGBOffset    correct offset for RED = 1 , GREEN = 2, or BLUE = 3 component to be exported
     * @param  maskFlag     Flag that indicates that the histogram will be calculated for the whole image if equal to
     *                      true
     * * @param userLimits    If true, histogram goes from userMin to userMax instead of image.getMin() to image.getMax()
     * @param userMin
     * @param usermax
     */
    public PlugInAlgorithmHistogram(ModelImage image, int RGBOffset, boolean maskFlag,
                              boolean userLimits, float userMin, float userMax) {
        this.image = image;
        this.RGBOffset = RGBOffset;
        this.entireImage = maskFlag;
        this.userLimits = userLimits;
        this.userMin = userMin;
        this.userMax = userMax;

        if (entireImage == false) {
            mask = image.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables.
     */
    public void disposeLocal() {

        

        if (image != null) {
            image = null;
        }

        
    }

    /**
     * Sets a flag to indicate if the histogram is to be calculated for the entire image or just the VOI.
     *
     * @param  flag  true = histogram of entire image, false = histogram of VOI
     */
    public void entireImage(boolean flag) {
        entireImage = flag;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    
    

    /**
     * Calculates the histogram and outputs the histogram information to a text file.
     *
     * 
     */
    public void runAlgorithm() {
        double sum = 0.0;
        double sumSq = 0.0;
        int cnt = 0;
        double mean;
        double stdDev;
        float sort[];
        int index = 0;
        double median;
        int countInMaximumPeak = 0;
        float mode;
        int numberMaximumPeaks = 0;
        int runLength;
        DecimalFormat df;
        double imageMax, imageMin;
        float[] imgBuffer;
        int z, zStop;
        int length;
        int i;
        File writeFile;
        RandomAccessFile raFile;
        String lineString;

        if (image == null) {
            displayError("Source Image is null");

            return;
        }

        fireProgressStateChanged("Histogram", "Calculating histogram data...");
        
        df = new DecimalFormat();
        df.setMinimumFractionDigits(4);
        df.setMaximumFractionDigits(4);
        
        writeFile = new File(image.getFileInfo(0).getFileDirectory() +
                image.getFileInfo(0).getFileName() + "_hist.txt");
        try {
            raFile = new RandomAccessFile(writeFile, "rw");
        }
        catch(FileNotFoundException e) {
            MipavUtil.displayError("File not found exception " + e);
            setCompleted(false);
            return;
        }
        
        try {
            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
        }
        catch(IOException e) {
            MipavUtil.displayError("IO exception on raFile.setLength(0) " + e);
            setCompleted(false);
            return;
        }

        switch (image.getType()) {


            case ModelStorageBase.ARGB:
            case ModelStorageBase.ARGB_USHORT:
            case ModelStorageBase.ARGB_FLOAT:
                if (RGBOffset == 1) {
                    imageMin = image.getMinR();
                    imageMax = image.getMaxR();
                } else if (RGBOffset == 2) {
                    imageMin = image.getMinG();
                    imageMax = image.getMaxG();
                } else {
                    imageMin = image.getMinB();
                    imageMax = image.getMaxB();
                }

                break;

            default:
                imageMin = (double) image.getMin();
                imageMax = (double) image.getMax();
                break;
        }
        
        if (userLimits) {
            imageMin = userMin;
            imageMax = userMax;
        }

        

        length = image.getSliceSize();
        imgBuffer = new float[length];
        


        if (image.getNDims() > 2) {
            zStop = image.getExtents()[2];
        } else {
            zStop = 1;
        }

        try {
            image.setLock(ModelStorageBase.W_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Histogram: Image locked", false);

            return;
        }

        for (z = 0; z < zStop; z++) {

            try {

                if (image.getType() == ModelStorageBase.COMPLEX) {
                    image.exportMagData(2 * z * length, length, imgBuffer);
                } else if (image.isColorImage()) {
                    image.exportRGBDataNoLock(RGBOffset, 4 * z * length, length, imgBuffer);
                } else {
                    image.exportDataNoLock(z * length, length, imgBuffer);
                }
            } catch (IOException error) {
                errorCleanUp("Plug In Algorithm Histogram: image bounds exceeded", false);
                image.releaseLock();

                return;
            }

            if ((image.getType() == ModelStorageBase.COMPLEX) && (image.getLogMagDisplay() == true)) {

                for (i = 0; i < length; i++) {
                    imgBuffer[i] = (float) (0.4342944819 * java.lang.Math.log(1 + imgBuffer[i]));
                }
            }

            // This is the part that actually calculates the histogram
            // Calculate for the entire image
            for (i = 0; i < length; i++) {

                if (((entireImage == false) && mask.get(i + (length * z))) || (entireImage == true)) {
                    if ((imgBuffer[i] >= imageMin) && (imgBuffer[i] <= imageMax)) {
                        sum += imgBuffer[i];
                        sumSq += imgBuffer[i]*imgBuffer[i];
                        cnt++;
                    }
                }
            }
            
            fireProgressStateChanged(Math.round((float) (z + 1) / zStop * 100));
        }
        
        sort = new float[cnt];
        for (z = 0; z < zStop; z++) {

            try {

                if (image.getType() == ModelStorageBase.COMPLEX) {
                    image.exportMagData(2 * z * length, length, imgBuffer);
                } else if (image.isColorImage()) {
                    image.exportRGBDataNoLock(RGBOffset, 4 * z * length, length, imgBuffer);
                } else {
                    image.exportDataNoLock(z * length, length, imgBuffer);
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Histogram: image bounds exceeded", false);
                image.releaseLock();

                return;
            }

            if ((image.getType() == ModelStorageBase.COMPLEX) && (image.getLogMagDisplay() == true)) {

                for (i = 0; i < length; i++) {
                    imgBuffer[i] = (float) (0.4342944819 * java.lang.Math.log(1 + imgBuffer[i]));
                }
            }

            
            for (i = 0; i < length; i++) {

                if (((entireImage == false) && mask.get(i + (length * z))) || (entireImage == true)) {
                    if ((imgBuffer[i] >= imageMin) && (imgBuffer[i] <= imageMax)) {
                        sort[index++] = imgBuffer[i];
                        
                    }
                }
            }
        }

        image.releaseLock();
        
        try {
            if (image.isColorImage()) {
                if (RGBOffset == 1) {
                    lineString = new String(image.getImageName() + " red\n");
                    raFile.write(lineString.getBytes());
                }
                else if (RGBOffset == 2) {
                    lineString = new String(image.getImageName() + " green\n");
                    raFile.write(lineString.getBytes());
                }
                else {
                    lineString = new String(image.getImageName() + " blue\n");
                    raFile.write(lineString.getBytes());
                }
            }
            else {
                lineString = new String(image.getImageName() + "\n");
                raFile.write(lineString.getBytes());
            }
            mean = sum/cnt;
            lineString = new String("Mean = " + df.format(mean) + "\n");
            raFile.write(lineString.getBytes());
            stdDev = Math.sqrt((sumSq - (sum*mean))/(cnt - 1));
            lineString = new String("Standard deviation = " + df.format(stdDev) + "\n");
            raFile.write(lineString.getBytes());
            Arrays.sort(sort);
            if (cnt%2 == 1) {
                median = sort[cnt/2] ;   
            }
            else {
                median = (sort[cnt/2] + sort[(cnt/2) - 1])/2.0;
            }
            lineString = new String("Median = " + median + "\n");
            raFile.write(lineString.getBytes());
            mode = sort[0];
            runLength = 1;
            countInMaximumPeak = 1;
            numberMaximumPeaks = 1;
            for (i = 1; i < cnt; i++) {
                if (sort[i] == sort[i-1]) {
                    runLength++;
                    if (runLength == countInMaximumPeak) {
                        numberMaximumPeaks++;
                    }
                    else if (runLength > countInMaximumPeak) {
                        mode = sort[i];
                        countInMaximumPeak = runLength;
                        numberMaximumPeaks = 1;
                    }
                } // if (sort[i] == sort[i-1]
                else {
                    runLength = 1;
                    if (countInMaximumPeak == 1) {
                        numberMaximumPeaks++;
                    }
                }
            }
            if (numberMaximumPeaks == 1) {
                if (image.getNDims() == 2) {
                    if (countInMaximumPeak == 1) {
                        lineString = new String("Mode = " + mode + " with a count of " + countInMaximumPeak + " pixel\n");
                    }
                    else {
                        lineString = new String("Mode = " + mode + " with a count of " + countInMaximumPeak + " pixels\n");    
                    }
                }
                else {
                    if (countInMaximumPeak == 1) {
                        lineString = new String("Mode = " + mode + " with a count of " + countInMaximumPeak + " voxel\n");
                    }
                    else {
                        lineString = new String("Mode = " + mode + " with a count of " + countInMaximumPeak + " voxels\n");    
                    }    
                }
            }
            else {
                lineString = new String("No obvious mode. " + numberMaximumPeaks + " values have counts = " + countInMaximumPeak + "\n");
            }
            raFile.write(lineString.getBytes());
            runLength = 1;
            for (i = 1; i < cnt; i++) {
                if (sort[i] == sort[i-1]) {
                    runLength++;
                    if (i == (cnt - 1)) {
                        lineString = new String(String.valueOf(sort[i]) + "     " + String.valueOf(runLength) + "\n");
                        raFile.write(lineString.getBytes());
                    }
                } // if (sort[i] == sort[i-1]
                else {
                    lineString = new String(String.valueOf(sort[i-1]) + "     " + String.valueOf(runLength)+ "\n");
                    raFile.write(lineString.getBytes());
                    runLength = 1;
                    if (i == (cnt - 1)) {
                        lineString = new String(String.valueOf(sort[i]) + "     " + String.valueOf(runLength) + "\n");
                        raFile.write(lineString.getBytes());    
                    }
                }
            }
        }
        catch (IOException e) {
            MipavUtil.displayError("IO exception on raFile.write " + e);
            setCompleted(false);
            return;
        }

        try {
            raFile.close();
        }
        catch(IOException e) {
            MipavUtil.displayError("IO exception on raFile.close " + e);
            setCompleted(false);
            return;    
        }

        // in this algorithm, we must clear the paint mask, since it was generated during generateVOIMask in the
        // constructor In the future, we might consider using a separate paint mask so that the image's paint mask can
        // remain even after this algorithm is run
        if (mask != null) {
            mask.clear();
        }

        Vector<ViewImageUpdateInterface> imageFrameVector = image.getImageFrameVector();

        for (i = 0; i < imageFrameVector.size(); i++) {
            Object object = imageFrameVector.elementAt(i);

            if (object instanceof ViewJFrameImage) {
                ViewJFrameImage imageFrame = (ViewJFrameImage) object;

                imageFrame.updateImages(true);
            }
        }

        
        setCompleted(true);
        imgBuffer = null;
        System.gc();
    }
    
   

    

}
