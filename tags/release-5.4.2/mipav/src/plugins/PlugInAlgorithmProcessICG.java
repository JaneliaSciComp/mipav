import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR25D2;
import gov.nih.mipav.view.*;

import java.util.*;

public class PlugInAlgorithmProcessICG
        extends AlgorithmBase {

    private double distortionThreshold = .8;
    private boolean checkDistortion = true;
    private boolean checkBlinks = true;
    
    private boolean replaceBlinks = true;
    private boolean replaceDistortion = true;
    private int registrationFrame;
    private int desiredOutputFrames = 0;
    

    /**
     *   @param destImg   image model where result image is to stored
     *   @param srcImage  source image model
     */
    public PlugInAlgorithmProcessICG(ModelImage srcImg,
                                     int regFrame,
                                     double distortionThresh,
                                     boolean doBlinks,
                                     boolean doReplaceBlinks,
                                     boolean doDistortion,
                                     boolean doReplaceDistortion,
                                     int desiredNumberFrames) {
        this.srcImage = srcImg;
        this.registrationFrame = regFrame;
        this.distortionThreshold = distortionThresh;
        this.checkBlinks = doBlinks;
        this.replaceBlinks = doReplaceBlinks;
        
        this.checkDistortion = doDistortion;
        this.replaceDistortion = doReplaceDistortion;
        this.desiredOutputFrames = desiredNumberFrames;
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
        
        processICG();
    }

    private void processICG() {
    	fireProgressStateChanged(0);
System.err.println("starting process icg");
        //lock the srcImage
        int refImageNum = registrationFrame;

        int i = 0;

        boolean foundBlinks = false;

        //System.err.println("Number of slices at beginning: " + srcImage.getExtents()[2]);

        ViewUserInterface.getReference().getMessageFrame().append("Process ICG on image: " + srcImage.getImageName() + "\n",
            ViewJFrameMessage.DATA);

        

        boolean [] detectedBlinks = null;
        int numBlinks = 0;
        

        System.err.println("Check blinks is: " + checkBlinks);
        
        if (checkBlinks) {
        	fireProgressStateChanged("Checking for blinks...");
        	//        	detect the blinks...don't remove yet
            PlugInAlgorithmRemoveBlinks algoBlinks = new PlugInAlgorithmRemoveBlinks(srcImage, false);
            algoBlinks.runAlgorithm();
            
            detectedBlinks = algoBlinks.getBlinks();
            
            

            for (i = 0; i < detectedBlinks.length; i++) {
                if (detectedBlinks[i]) {

                    numBlinks++;
                    foundBlinks = true;
                    if ( (i+1) < registrationFrame) {
                        refImageNum--;
                    }
                }
            }
        }
        fireProgressStateChanged(5);

        //only if we've found blinks do we want to do the next step
        if (foundBlinks) {
            System.err.println("Removing blinks");

            fireProgressStateChanged("Removing blinks...");
            AlgorithmRemoveSlices algoSlice = new AlgorithmRemoveSlices(srcImage, null, detectedBlinks);
            algoSlice.runAlgorithm();
            srcImage.calcMinMax();
            ViewUserInterface.getReference().getMessageFrame().append("\tRemoved " + numBlinks + " blinks from image" + "\n",
                        ViewJFrameMessage.DATA);
            fireProgressStateChanged(10);
        }



        //make zero-based
        //System.err.println("Using reference frame (1-indexed): " + refImageNum);
        ViewUserInterface.getReference().getMessageFrame().append("\tUsing reference frame (1-indexed): " + refImageNum + "\n",
            ViewJFrameMessage.DATA);
        refImageNum--;

        System.err.println("running registration");
        //run registration.....oar2.5d
        AlgorithmRegOAR25D2 reg25 = new AlgorithmRegOAR25D2( srcImage, 2, 3, 1, 1, false, refImageNum,
                -3, 3, 3, 2, false, true, false, 2, 6 );
        linkProgressToAlgorithm(reg25);
        reg25.setProgressValues(10, 90);
        reg25.runAlgorithm();

        double [] sliceCosts = reg25.getCosts();

        //registration algorithm is no longer needed, freeing memory
        reg25.disposeLocal();
        reg25.finalize();

        if (checkDistortion) {
        	fireProgressStateChanged("Checking for distortion...");
            ViewUserInterface.getReference().getMessageFrame().append("\tChecking for distortion" + "\n",
            ViewJFrameMessage.DATA);
            boolean foundDistortion = false;
            //System.err.println("\n\nChecking costs for distortion after registration");
            boolean[] distortedSlices = computeDistortedSlices(sliceCosts,
                    refImageNum, desiredOutputFrames);

            int numDistorted = 0;
            for (i = 0; i < distortedSlices.length; i++) {
                if (distortedSlices[i]) {
                    foundDistortion = true;
                    numDistorted++;
                }
            }



            System.err.println("Distortion found: " + foundDistortion +
                               " num distorted: " + numDistorted);

            if (foundDistortion) {
                ViewUserInterface.getReference().getMessageFrame().append("\tDistortion found: " + foundDistortion +
                    " num distorted: " + numDistorted + "\n", ViewJFrameMessage.DATA);


                System.err.println("found distortion, removing slices");
                fireProgressStateChanged("Removing distortion...");
                AlgorithmRemoveSlices algoSlice = new AlgorithmRemoveSlices(srcImage, null, distortedSlices);
                algoSlice.runAlgorithm();
                fireProgressStateChanged(95);
                //System.err.println("Number of slices after distortion removal: " + srcImage.getExtents()[2]);


                //combine distorted boolean with the original blinks (and replace slices)
                if (foundBlinks && replaceBlinks && replaceDistortion) {
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
                    fireProgressStateChanged("Replacing blinks and distortion...");
                    algoReplace.runAlgorithm();
                 //   System.err.println("Number of slices after replacement: " +
                 //                      srcImage.getExtents()[2]);

                } else if (replaceDistortion) {
                    ViewUserInterface.getReference().getMessageFrame().append("\tReplacing distorted slices only" + "\n",
                            ViewJFrameMessage.DATA);
                    //System.err.println("Replacing distorted slices only");
                    fireProgressStateChanged("Replacing distortion...");
                    AlgorithmReplaceRemovedSlices algoReplace = new
                            AlgorithmReplaceRemovedSlices(srcImage, distortedSlices, false, false, false);
                    algoReplace.runAlgorithm();
                 //   System.err.println("Number of slices after replacement: " +
                 //                      srcImage.getExtents()[2]);
                }

            }
        }
        else if (foundBlinks && replaceBlinks) {
            ViewUserInterface.getReference().getMessageFrame().append("\tReplacing blinks only" + "\n",
                            ViewJFrameMessage.DATA);
          //  System.err.println("Replacing blinks only");
            AlgorithmReplaceRemovedSlices algoReplace = new AlgorithmReplaceRemovedSlices(srcImage, detectedBlinks, false, false, false);
            algoReplace.runAlgorithm();
            System.err.println("Number of slices after replacement: " + srcImage.getExtents()[2]);
        }

       // srcImage.calcMinMax();

        destImage = new ModelImage(ModelImage.FLOAT, new int[] { srcImage.getExtents()[0], srcImage.getExtents()[1]},
                                              srcImage.getImageName() + "_sum");


        AlgorithmImageMath algoMath = new AlgorithmImageMath(destImage, srcImage, AlgorithmImageMath.SUM, 0.0, 0.0, 0.0, 0, true);
        algoMath.runAlgorithm();
        destImage.calcMinMax();
        new ViewJFrameImage(destImage);

        //System.err.println("Number of slices after replacing frames " + srcImage.getExtents()[2]);
        setCompleted(true);
    }

    private boolean [] computeDistortedSlices( double [] costs, int refImageNum, int numDesired) {

    	boolean doTargetNum = false;
    	if (numDesired != 0) {
    		doTargetNum = true;
    	}
        int i = 0;
        int length = costs.length;

        boolean [] distortedSlices = new boolean[length];
        

        
        if (doTargetNum) {
        	for (i = 0; i < length; i++) {
                distortedSlices[i] = true;
            }
        	
        	Vector<SliceCost> vec = new Vector<SliceCost>();
        	for (i = 0; i < costs.length; i++) {
        		vec.add(new SliceCost(i, costs[i]));
        	}
        	
        	Collections.sort(vec);
        	
        	SliceCost tempCost;
        	for (i = 0; i < numDesired; i++) {
        		tempCost = (SliceCost)vec.elementAt(i);
        		distortedSlices[tempCost.frame] = false;
        		System.err.println(i + " cost: " + tempCost.cost + ", frame: " + tempCost.frame );
        	}
        	
        } else {
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
        	
        }
        
        
        

        return distortedSlices;
    }

    private class SliceCost implements Comparable<Object> {
    	public int frame;
    	public double cost;
    	
    	public SliceCost(int frame, double cost) {
    		this.frame = frame;
    		this.cost = cost;
    	}
    	
    	public int compareTo(Object other) throws ClassCastException {
    	    if (!(other instanceof SliceCost))
    	      throw new ClassCastException("whoops");
    	    
    	    double otherCost = ((SliceCost) other).cost;
    	    
    	    if (otherCost > cost) {
    	    	return -1;
    	    } else if (otherCost < cost) {
    	    	return 1;
    	    } else {
    	    	return 0;
    	    }    	    
    	  }
    	
    }


    public ModelImage getResultImage() {
        return destImage;
    }

}
