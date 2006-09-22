package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;

import javax.vecmath.*;


/**
 * Anonymize an image of a patient's head by removing the face.
 *
 * <p>This version of of the de-facer uses BET to find the brain, the brain's location is then used to calculate a
 * plane. This plane is then used to remove the patient's face from the image.</p>
 *
 * @author  mccreedy
 */
public class AlgorithmFaceAnonymizerBET extends AlgorithmBase {
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------
    
    /** DOCUMENT ME! */
    public static final int FACING_RIGHT = 1;

    /** DOCUMENT ME! */
    public static final int FACING_LEFT = 2;

    /** DOCUMENT ME! */
    public static final int FACING_DOWN = 3;

    /** DOCUMENT ME! */
    public static final int FACING_UP = 4;

    /** DOCUMENT ME! */
    public static final int FACING_INTO_SCREEN = 5;

    /** DOCUMENT ME! */
    public static final int FACING_OUT_OF_SCREEN = 6;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int currentProgressBarValue = 0;

    /** DOCUMENT ME! */
    private boolean estimateWithSphereBET = false;

    /** DOCUMENT ME! */
    private int faceOrientation;

    /** DOCUMENT ME! */
    private float imageInfluenceBET = 0.1f;

    /** DOCUMENT ME! */
    private int mmToDelete;

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private float stiffnessBET = 0.15f;

    /** DOCUMENT ME! */
    private float verticalDeletionLimit;

    /** DOCUMENT ME! */
    private int volumeSize;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int zDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct the face anonymizer, but do not run it yet.
     *
     * @param  srcImg            The image to de-face
     * @param  faceDirection     the orientation of the patient's face, as determined by the dialog
     * @param  extraMMsToDelete  the number of millimeters to try to delete from the non-brain portions of the head
     * @param  verticalLimit     the limit to place on the deletion of the face in the from the 'bottom' of the face.
     *                           Should be between 0 and 1, with 0 = no deletion and 1 = full deletion (until the brain
     *                           is hit).
     */
    public AlgorithmFaceAnonymizerBET(ModelImage srcImg, int faceDirection, int extraMMsToDelete, float verticalLimit) {
        srcImage = srcImg;
        faceOrientation = faceDirection;
        mmToDelete = extraMMsToDelete;
        verticalDeletionLimit = verticalLimit;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        volumeSize = sliceSize * zDim;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory used by the algorithm.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Run the de-facing algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("The source image is null");
            finalize();

            return;
        }

        if (srcImage.getNDims() != 3) {
            displayError("The source image must be 3D");
            finalize();

            return;
        }

        constructLog();

        try {
            anonymizeFace();
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Face anonymizer BET: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Changes the BET algorithm parameters from their defaults (which we choose).
     *
     * @param  estimateWithSphere  whether to estimate the brain with a sphere initially
     * @param  imageInfluence      the image influence ratio
     * @param  stiffness           the mesh stiffness
     */
    public void setBETParameters(boolean estimateWithSphere, float imageInfluence, float stiffness) {
        estimateWithSphereBET = estimateWithSphere;
        imageInfluenceBET = imageInfluence;
        stiffnessBET = stiffness;
    }

    /**
     * Executes the de-facing algorithm.
     */
    private void anonymizeFace() {
        int curPosition, closestPosition;

        fireProgressStateChanged(0, null, "Anonymizing face ...");
        

        // See if the image orientation is known
        int betOrientation;

        if (faceOrientation == FACING_RIGHT) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
            curPosition = Integer.MIN_VALUE;
            closestPosition = Integer.MIN_VALUE;
        } else if (faceOrientation == FACING_LEFT) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
            curPosition = Integer.MAX_VALUE;
            closestPosition = Integer.MAX_VALUE;
        } else if (faceOrientation == FACING_DOWN) {
            betOrientation = AlgorithmBrainExtractor.AXIAL;
            curPosition = Integer.MIN_VALUE;
            closestPosition = Integer.MIN_VALUE;
        } else if (faceOrientation == FACING_UP) {
            betOrientation = AlgorithmBrainExtractor.AXIAL;
            curPosition = Integer.MAX_VALUE;
            closestPosition = Integer.MAX_VALUE;
        } else if (faceOrientation == FACING_INTO_SCREEN) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
            curPosition = Integer.MIN_VALUE;
            closestPosition = Integer.MIN_VALUE;
        } else if (faceOrientation == FACING_OUT_OF_SCREEN) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
            curPosition = Integer.MAX_VALUE;
            closestPosition = Integer.MAX_VALUE;
        } else {
            displayError("No face orientation given.");
            finalize();

            return;
        }

        fireProgressStateChanged(3);

        boolean showInitialEstimation = false;
        Point3f initialCenter = JDialogExtractBrain.computeCenter(srcImage, betOrientation, estimateWithSphereBET);
        int iterations = 500;
        int maxDepth = 5;
        boolean extractBrainToPaint = true;
        AlgorithmBrainExtractor bet = new AlgorithmBrainExtractor(srcImage, betOrientation, showInitialEstimation,
                                                                  estimateWithSphereBET, initialCenter);
        bet.setIterations(iterations);
        bet.setMaxDepth(maxDepth);
        bet.setImageRatio(imageInfluenceBET);
        bet.setStiffness(stiffnessBET);
        bet.setExtractPaint(extractBrainToPaint);
        bet.setSaveBrainMesh(false);
        bet.setRunningInSeparateThread(isRunningInSeparateThread());
        linkProgressToAlgorithm(bet);
        bet.setProgressValues(generateProgressValues(3, 57));
        bet.run();

        int updateInterval = srcImage.getSize() / 19;

        // calc plane from BET extraction
        BitSet brainMask = srcImage.getMask();

        int pBar = 0;
        
        // go through each slice, find the closest point in the direction the face should be pointing
        for (int i = 0; i < srcImage.getSize(); i++) {

            if ((i % updateInterval) == 0) {
            	pBar++;
                fireProgressStateChanged(57 + pBar);
            }

            if (brainMask.get(i)) {
                curPosition = getNormalPosition(i);

                if (faceOrientation == FACING_RIGHT) {

                    if (curPosition > closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == FACING_LEFT) {

                    if (curPosition < closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == FACING_DOWN) {

                    if (curPosition > closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == FACING_UP) {

                    if (curPosition < closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == FACING_INTO_SCREEN) {

                    if (curPosition > closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == FACING_OUT_OF_SCREEN) {

                    if (curPosition < closestPosition) {
                        closestPosition = curPosition;
                    }
                }
            }
        }

        if (closestPosition == Integer.MAX_VALUE) {
            displayError("Unable to find the front of the segmented brain.");
            finalize();

            return;
        }

        // bump out point slightly
        float percentBump = 0.025f;

        if (faceOrientation == FACING_RIGHT) {
            closestPosition += MipavMath.round(percentBump * srcImage.getExtents()[0]);
        } else if (faceOrientation == FACING_LEFT) {
            closestPosition -= MipavMath.round(percentBump * srcImage.getExtents()[0]);
        } else if (faceOrientation == FACING_DOWN) {
            closestPosition += MipavMath.round(percentBump * srcImage.getExtents()[1]);
        } else if (faceOrientation == FACING_UP) {
            closestPosition -= MipavMath.round(percentBump * srcImage.getExtents()[1]);
        } else if (faceOrientation == FACING_INTO_SCREEN) {
            closestPosition += MipavMath.round(percentBump * srcImage.getExtents()[2]);
        } else if (faceOrientation == FACING_OUT_OF_SCREEN) {
            closestPosition -= MipavMath.round(percentBump * srcImage.getExtents()[2]);
        }

        int removalIndex;
        int removalStep;
        int removalStart;
        int removalEnd;
        int removalPosition;

        boolean foundBrain;
        boolean removedFace = false;
        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float zRes = srcImage.getFileInfo(0).getResolutions()[2];

        int faceLevel;

        updateInterval = srcImage.getSize() / 19;

        //progress should currently be at 76
        
        pBar = 0;
        for (int i = 0; i < srcImage.getSize(); i++) {

            if ((i % updateInterval) == 0) {
            	pBar++;
                fireProgressStateChanged(76 + pBar);
            }

            if (faceOrientation == FACING_RIGHT) {
                faceLevel = (i % sliceSize) / xDim;

                // in sagittal images, the face should be in the lower third of each slice (>
                // yDim/verticalDeletionLimit)
                if (faceLevel < (yDim * (1 - verticalDeletionLimit))) {
                    continue;
                }
            } else if (faceOrientation == FACING_LEFT) {
                faceLevel = (i % sliceSize) / xDim;

                // in sagittal images, the face should be in the lower third of each slice (>
                // yDim/verticalDeletionLimit)
                if (faceLevel < (yDim * (1 - verticalDeletionLimit))) {
                    continue;
                }
            } else if (faceOrientation == FACING_DOWN) {
                faceLevel = i / sliceSize;

                // in axial images, the face should be in the first third of the slices (< zDim/verticalDeletionLimit)
                if (faceLevel > (zDim * verticalDeletionLimit)) {
                    continue;
                }
            } else if (faceOrientation == FACING_UP) {
                faceLevel = i / sliceSize;

                // in axial images, the face should be in the first third of the slices (< zDim/verticalDeletionLimit)
                if (faceLevel > (zDim * verticalDeletionLimit)) {
                    continue;
                }
            } else if (faceOrientation == FACING_INTO_SCREEN) {
                faceLevel = (i % sliceSize) / xDim;

                // in coronal images, the face should be in the lower portion of each slice (>
                // yDim/verticalDeletionLimit)
                if (faceLevel < (yDim * (1 - verticalDeletionLimit))) {
                    continue;
                }
            } else if (faceOrientation == FACING_OUT_OF_SCREEN) {
                faceLevel = (i % sliceSize) / xDim;

                // in coronal images, the face should be in the lower third of each slice (> yDim/verticalDeletionLimit)
                if (faceLevel < (yDim * (1 - verticalDeletionLimit))) {
                    continue;
                }
            } else {

                // should never happen..
                faceLevel = 0;

                MipavUtil.displayError("Error in face orientation specification: " + faceOrientation);

                srcImage.clearMask();
                bet.finalize();
                bet = null;
                finalize();

                return;
            }

            removalPosition = getNormalPosition(i);

            // ignore points not on the "face plane"
            if (removalPosition != closestPosition) {
                continue;
            }

            if (faceOrientation == FACING_RIGHT) {
                removalIndex = i - (int) (mmToDelete / xRes);
                removalStep = 1;
                removalStart = removalIndex;
                removalEnd = i + (xDim - getNormalPosition(i));
            } else if (faceOrientation == FACING_LEFT) {
                removalIndex = i + (int) (mmToDelete / xRes);
                removalStep = 1;
                removalStart = i - getNormalPosition(i);
                removalEnd = removalIndex;
            } else if (faceOrientation == FACING_DOWN) {
                removalIndex = i - (int) ((int) (mmToDelete / yRes) * xDim);
                removalStep = xDim;
                removalStart = removalIndex;
                removalEnd = i + (sliceSize - (xDim * getNormalPosition(i)));
            } else if (faceOrientation == FACING_UP) {
                removalIndex = i + (int) ((int) (mmToDelete / yRes) * xDim);
                removalStep = xDim;
                removalStart = i - (xDim * getNormalPosition(i));
                removalEnd = removalIndex;
            } else if (faceOrientation == FACING_INTO_SCREEN) {
                removalIndex = i - (int) ((int) (mmToDelete / zRes) * sliceSize);
                removalStep = sliceSize;
                removalStart = removalIndex;
                removalEnd = i + (volumeSize - (sliceSize * getNormalPosition(i)));
            } else if (faceOrientation == FACING_OUT_OF_SCREEN) {
                removalIndex = i + (int) ((int) (mmToDelete / zRes) * sliceSize);
                removalStep = sliceSize;
                removalStart = i - (sliceSize * getNormalPosition(i));
                removalEnd = removalIndex;
            } else {

                // should never happen..
                removalIndex = 0;
                removalStep = 0;
                removalStart = 0;
                removalEnd = 0;

                MipavUtil.displayError("Error in face orientation specification: " + faceOrientation);

                srcImage.clearMask();
                bet.finalize();
                bet = null;
                finalize();

                return;
            }

            foundBrain = false;

            for (int j = removalStart; j < removalEnd; j += removalStep) {

                if (brainMask.get(j)) {
                    foundBrain = true;
                }
            }

            if (!foundBrain) {

                for (int j = removalStart; j < removalEnd; j += removalStep) {

                    if (j < srcImage.getSize()) {
                        srcImage.set(j, 0);
                    }
                }

                removedFace = true;
            }
        }

        if (!removedFace) {
            MipavUtil.displayError("The face could not be located and removed, possibly because the brain segmentation was too large.");
        }

        srcImage.clearMask();

        bet.finalize();
        bet = null;

        fireProgressStateChanged(100);
        

        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the message frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("AnonymizeFaceBET(" + ")\n");
    }

    /**
     * Get the position in the dimension normal to the face (in slice) from an index, based on the face orientation.
     *
     * @param   index  the image index
     *
     * @return  the position of the index in the dimension normal to the face
     */
    private int getNormalPosition(int index) {

        if (faceOrientation == FACING_RIGHT) {
            return index % xDim;
        } else if (faceOrientation == FACING_LEFT) {
            return index % xDim;
        } else if (faceOrientation == FACING_DOWN) {
            return (index % sliceSize) / xDim;
        } else if (faceOrientation == FACING_UP) {
            return (index % sliceSize) / xDim;
        } else if (faceOrientation == FACING_INTO_SCREEN) {
            return index / sliceSize;
        } else if (faceOrientation == FACING_OUT_OF_SCREEN) {
            return index / sliceSize;
        } else {
            return Integer.MAX_VALUE;
        }
    }
}
