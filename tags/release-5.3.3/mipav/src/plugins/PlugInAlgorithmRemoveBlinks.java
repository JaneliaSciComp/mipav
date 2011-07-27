import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.ViewJFrameMessage;
import java.io.*;
import gov.nih.mipav.view.ViewUserInterface;

public class PlugInAlgorithmRemoveBlinks
    extends AlgorithmBase {

    private double[] buffer = null;
    private double[] averages = null;
    private double[] sliceMax = null;
    private boolean[] removedSlices = null;

    private double thresholdDevsNorm = .6; // # of std deviations to allow for slices with a normal max
    private double thresholdMax;
    private double percentage = 80;

    private boolean calcInPlace = false;

    private boolean doRemove = true;

    private double stripPercent = .1;
    
    /**
     *   @param destImg   image model where result image is to stored
     *   @param srcImage  source image model
     */
    public PlugInAlgorithmRemoveBlinks(ModelImage srcImg, boolean doRemove) {
        this.srcImage = srcImg;
        this.doRemove = doRemove;
    }

    /**
     *   Prepares this class for destruction
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     *   Starts the program
     */
    public void runAlgorithm() {
        
        removeBlinks();
    }

    public boolean [] getBlinks() {
        return this.removedSlices;
    }

    private void removeBlinks() {

        int xDim, yDim;
        int numSlices;

        int length;
        int i, j;

        int stripHeight = 0;
        
        int sliceSize = srcImage.getSliceSize();
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        numSlices = srcImage.getExtents()[2];

        thresholdMax = ( (double) (percentage) / 100.0) * srcImage.getMax();

        float totalAverage = 0;

        try {
        	stripHeight = (int)(yDim * stripPercent);
            length = xDim * stripHeight;
            buffer = new double[sliceSize];
            averages = new double[numSlices];
            sliceMax = new double[numSlices];
            removedSlices = new boolean[numSlices];
            fireProgressStateChanged("Locating blinks ...");
        }
        catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Blink Removal reports: Out of memory when creating image buffer");
            setCompleted(false);
            return;
        }

        System.err.println("new version");


        double mod = 100.0 / numSlices;
        int counter = 0;


        // export each slice into buffer and calculate the intensities
        
        for (i = 0; i < numSlices; i++) {
            try {
                srcImage.exportSliceXY(i, buffer);
                sliceMax[i] = buffer[0];
                
                //calc the top strip (0 to 10percent)
                for (j = 0; j < length; j++) {
                    averages[i] += buffer[j];
                    if (buffer[j] > sliceMax[i]) {
                        sliceMax[i] = buffer[j];
                    }
                }
                //calc the bottom strip (90 to 100 percent)
                for (j = sliceSize - length; j < sliceSize; j++) {
                	averages[i] += buffer[j];
                    if (buffer[j] > sliceMax[i]) {
                        sliceMax[i] = buffer[j];
                    }
                }
                
                //divide by length * 2 because we're doing 2 strips
                averages[i] /= (length * 2);
                fireProgressStateChanged((int)((i+1) * mod));
            }
            catch (IOException ex) {
                System.err.println(ex.toString());
            }

            counter += length;
            totalAverage += averages[i];

        }

        //get the total average
        totalAverage /= numSlices;

        //find the sum of squares
        double sumOfSquares = 0;
        double deviation = 0;
        for (i = 0; i < numSlices; i++) {
            deviation = averages[i] - totalAverage;
            sumOfSquares += (deviation * deviation);
            counter++;

        }

        double standardDeviation = Math.sqrt(sumOfSquares / (numSlices - 1));

        int numNewSlices = 0;
        int numRemovedSlices = 0;

        boolean hitThreshold = false;
        for (i = 0; i < numSlices; i++) {
            counter++;
            //threshold has not been hit
            if (!hitThreshold) {
                // average is still below threshold
                if (averages[i] < totalAverage * .75) {
                    // if the slice max intensity is less than 50.. discard
                    if (sliceMax[i] < thresholdMax) {
                        removedSlices[i] = true;
                        numRemovedSlices++;
                    }
                    else {
                        removedSlices[i] = false;
                    }
                }
                //average is above threshold
                else {
                    hitThreshold = true;
                    removedSlices[i] = false;
                }
            }
            //threshold has been hit... will now use std deviation
            else {
                deviation = totalAverage - averages[i];

                //System.err.println((i + 1) + "Standard deviation (average): " + (deviation - standardDeviation));
                if ( (deviation - standardDeviation) > thresholdDevsNorm) {
                    removedSlices[i] = true;
                    numRemovedSlices++;

                    //System.err.println("Removing slice: " + (i + 1) + "  Std devs away: " + (deviation - standardDeviation));

                }
                else {
                    removedSlices[i] = false;
                }
            }
        }
        numNewSlices = numSlices - numRemovedSlices;

        System.err.println("Found " + numRemovedSlices + " blinks in " + srcImage.getImageFileName());

        ViewUserInterface.getReference().getMessageFrame().append("\tfound " + numRemovedSlices + " blinks in " + srcImage.getImageFileName() + "\n",
            ViewJFrameMessage.DATA);
        for (j = 0; j < removedSlices.length; j++) {
            if (removedSlices[j]) {
                ViewUserInterface.getReference().getMessageFrame().append("\t" + (j + 1) + ": blink detected\n", ViewJFrameMessage.DATA);
            }
        }

        

        if (doRemove) {
            //determine the extents for the new movie
            System.err.println("Removing " + numRemovedSlices + " slices.  Result image has " + numNewSlices + " slices");

            if (numRemovedSlices > 0 && numNewSlices > 0) {
                if (!calcInPlace) {
                    int[] extents = new int[3];
                    extents[0] = srcImage.getExtents()[0];
                    extents[1] = srcImage.getExtents()[1];
                    extents[2] = numNewSlices;
                    String name = JDialogBase.makeImageName(srcImage.getImageName(),
                            "_no_blinks");
                    destImage = new ModelImage(srcImage.getType(), extents, name);
                }

                AlgorithmRemoveSlices algoRS = new AlgorithmRemoveSlices(srcImage,
                        destImage, removedSlices);
               // algoRS.setActiveImage(activeImage);
                algoRS.run();
                if (algoRS.isCompleted()) {
                    setCompleted(true);
                    return;
                } else {
                    setCompleted(false);
                    return;
                }
            } else {
                if (numRemovedSlices == 0) {
                    System.err.println("Algorithm will remove no slices");

                } else {
                    System.err.println("Algorithm would remove all slices: aborting");
                }

                //because this can be used in a script, will just clone the image as the result
                String name = JDialogBase.makeImageName(srcImage.getImageName(),
                                                        "_unchanged");
                destImage = new ModelImage(srcImage.getType(), srcImage.getExtents(),
                                           name);
            }
        }
        setCompleted(true);
        notifyListeners(this);
    }

    public ModelImage getResultImage() {
        return destImage;
    }

}
