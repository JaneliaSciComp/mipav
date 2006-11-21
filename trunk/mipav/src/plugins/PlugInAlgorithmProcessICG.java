import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR25D2;
import gov.nih.mipav.view.*;

public class PlugInAlgorithmProcessICG
        extends AlgorithmBase {

    private double distortionThreshold = .8;
    private boolean checkDistortion = true;
    private int registrationFrame;


    /**
     *   @param destImg   image model where result image is to stored
     *   @param srcImage  source image model
     */
    public PlugInAlgorithmProcessICG(ModelImage srcImg,
                                     int regFrame,
                                     double distortionThresh,
                                     boolean doDistortion) {
        this.srcImage = srcImg;
        this.registrationFrame = regFrame;
        this.distortionThreshold = distortionThresh;
        this.checkDistortion = doDistortion;
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
     *   Constructs a string of the contruction parameters and out puts the
     *   string to the messsage frame if the logging procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("ProcessICG()\n");
    }

    /**
     *   Starts the program
     */
    public void runAlgorithm() {
        constructLog();
        processICG();
    }

    private void processICG() {

        //lock the srcImage
        int refImageNum = registrationFrame;

        int i = 0;

        boolean foundBlinks = false;

        //System.err.println("Number of slices at beginning: " + srcImage.getExtents()[2]);

        ViewUserInterface.getReference().getMessageFrame().append("Process ICG on image: " + srcImage.getImageName() + "\n",
            ViewJFrameMessage.DATA);

        //detect the blinks...don't remove yet
        PlugInAlgorithmRemoveBlinks algoBlinks = new PlugInAlgorithmRemoveBlinks(srcImage, false);
        algoBlinks.runAlgorithm();

        boolean [] detectedBlinks = algoBlinks.getBlinks();

        //make sure we're removing blinks

        int numBlinks = 0;

        for (i = 0; i < detectedBlinks.length; i++) {
            if (detectedBlinks[i]) {

                numBlinks++;
                foundBlinks = true;
                if ( (i+1) < registrationFrame) {
                    refImageNum--;
                }
            }
        }

        //only if we've found blinks do we want to do the next step
        if (foundBlinks) {
            //System.err.println("\n\nRemoving blinks");

            AlgorithmRemoveSlices algoSlice = new AlgorithmRemoveSlices(srcImage, null, detectedBlinks);
            algoSlice.runAlgorithm();
            srcImage.calcMinMax();
            ViewUserInterface.getReference().getMessageFrame().append("\tRemoved " + numBlinks + " blinks from image" + "\n",
                        ViewJFrameMessage.DATA);
        }



        //make zero-based
        //System.err.println("Using reference frame (1-indexed): " + refImageNum);
        ViewUserInterface.getReference().getMessageFrame().append("\tUsing reference frame (1-indexed): " + refImageNum + "\n",
            ViewJFrameMessage.DATA);

        refImageNum--;


        //run registration.....oar2.5d
        AlgorithmRegOAR25D2 reg25 = new AlgorithmRegOAR25D2( srcImage, 2, 3, 1, 1, false, refImageNum,
                -3, 3, 3, 2, false, true, false, 3, 2, 6 );
        reg25.runAlgorithm();

        double [] sliceCosts = reg25.getCosts();



        if (checkDistortion) {
            ViewUserInterface.getReference().getMessageFrame().append("\tChecking for distortion" + "\n",
            ViewJFrameMessage.DATA);
            boolean doRemoveDistortion = false;
            //System.err.println("\n\nChecking costs for distortion after registration");
            boolean[] distortedSlices = computeDistortedSlices(sliceCosts,
                    refImageNum);

            int numDistorted = 0;
            for (i = 0; i < distortedSlices.length; i++) {
                if (distortedSlices[i]) {
                    doRemoveDistortion = true;
                    numDistorted++;
                }
            }



            System.err.println("Distortion found: " + doRemoveDistortion +
                               " num distorted: " + numDistorted);

            if (doRemoveDistortion) {

                ViewUserInterface.getReference().getMessageFrame().append("\tDistortion found: " + doRemoveDistortion +
                    " num distorted: " + numDistorted + "\n", ViewJFrameMessage.DATA);

                // System.err.println("cloning the registered for comparison");
                // new ViewJFrameImage((ModelImage)srcImage.clone());

                //System.err.println("found distortion, removing slices");
                AlgorithmRemoveSlices algoSlice = new AlgorithmRemoveSlices(srcImage, null, distortedSlices);
                algoSlice.runAlgorithm();
                //System.err.println("Number of slices after distortion removal: " + srcImage.getExtents()[2]);


                //combine distorted boolean with the original blinks (and replace slices)

                if (foundBlinks) {
                    int distCounter = 0;
                    for (i = 0; i < detectedBlinks.length; i++) {
                        if (detectedBlinks[i]) {
                            //do nothing, this didn't affect the distorted (registered) array
                        } else {
                            //blink wasn't here originally, checked distorted
                            if (distortedSlices[distCounter]) {
                                detectedBlinks[i] = true;
                            }
                            //increment counter into distorted slices
                            distCounter++;
                        }
                    }

                    ViewUserInterface.getReference().getMessageFrame().append("\tReplacing blinks and distorted slices" + "\n",
                            ViewJFrameMessage.DATA);

                   // System.err.println("Replacing blinks and distorted slices");
                    AlgorithmReplaceRemovedSlices algoReplace = new
                            AlgorithmReplaceRemovedSlices(srcImage, detectedBlinks, false, false, false);
                    algoReplace.runAlgorithm();
                 //   System.err.println("Number of slices after replacement: " +
                 //                      srcImage.getExtents()[2]);

                } else {
                    ViewUserInterface.getReference().getMessageFrame().append("\tReplacing distorted slices only" + "\n",
                            ViewJFrameMessage.DATA);
                    //System.err.println("Replacing distorted slices only");
                    AlgorithmReplaceRemovedSlices algoReplace = new
                            AlgorithmReplaceRemovedSlices(srcImage, distortedSlices, false, false, false);
                    algoReplace.runAlgorithm();
                 //   System.err.println("Number of slices after replacement: " +
                 //                      srcImage.getExtents()[2]);
                }

            }
        }
        else if (foundBlinks) {
            ViewUserInterface.getReference().getMessageFrame().append("\tReplacing blinks only" + "\n",
                            ViewJFrameMessage.DATA);
          //  System.err.println("Replacing blinks only");
            AlgorithmReplaceRemovedSlices algoReplace = new AlgorithmReplaceRemovedSlices(srcImage, detectedBlinks, false, false, false);
            algoReplace.runAlgorithm();
            System.err.println("Number of slices after replacement: " + srcImage.getExtents()[2]);
        }

       // srcImage.calcMinMax();

        ModelImage destImage = new ModelImage(ModelImage.FLOAT, new int[] { srcImage.getExtents()[0], srcImage.getExtents()[1]},
                                              srcImage.getImageName() + "_sum");


        AlgorithmImageMath algoMath = new AlgorithmImageMath(destImage, srcImage, AlgorithmImageMath.SUM, 0, 0, 0, true);
        algoMath.runAlgorithm();
        destImage.calcMinMax();
        new ViewJFrameImage(destImage);

        //System.err.println("Number of slices after replacing frames " + srcImage.getExtents()[2]);
        setCompleted(true);
    }

    private boolean [] computeDistortedSlices( double [] costs, int refImageNum) {

        int i = 0;
        int length = costs.length;

        boolean [] distortedSlices = new boolean[length];
        for (i = 0; i < length; i++) {
            distortedSlices[i] = false;
        }

        double totalAverage = 0;

        for (i = 0; i < length; i++) {
            if (i != refImageNum) {
                totalAverage += costs[i];
            }
        }
        totalAverage /= (length - 1);

        //find the sum of squares
        double sumOfSquares = 0;
        double deviation = 0;
        for (i = 0; i < length; i++) {
            if (i != refImageNum) {
                deviation = costs[i] - totalAverage;
                sumOfSquares += (deviation * deviation);
            }
        }

        double standardDeviation = Math.sqrt(sumOfSquares / (length - 2));

        ViewUserInterface.getReference().getMessageFrame().append("\tStandard deviation of costs for registration: "
               + standardDeviation + "\n", ViewJFrameMessage.DATA);

       // System.err.println("Average: " + totalAverage + " , Standard Dev: " + standardDeviation + "\n");

        for (i = 0; i < length; i++) {
            if (i != refImageNum) {
                deviation = costs[i] - totalAverage;
           //     System.err.println(i + " distortion test: " + costs[i] + " , deviation: " +
         //                          (deviation / standardDeviation));
                if (deviation / standardDeviation >= distortionThreshold) {
            //        System.err.println("\t " + i + " slice marked for removal/replacement");
                    distortedSlices[i] = true;
                    ViewUserInterface.getReference().getMessageFrame().append("\t\tdistortion frame: " + (i + 1)
                            + ": " + (deviation/standardDeviation) + "\n",
                            ViewJFrameMessage.DATA);
                }
            }
        }

        return distortedSlices;
    }



    public ModelImage getResultImage() {
        return destImage;
    }

}
