package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogExtractBrain;
import gov.nih.mipav.view.dialogs.JDialogFaceAnonymizer;
import java.util.BitSet;
import javax.vecmath.*;


/**
 * Anonymize an image of a patient's head by removing the face.
 * 
 * This version of of the de-facer uses BET to find the brain, the brain's location is then used to calculate a plane.
 * This plane is then used to remove the patient's face from the image.
 * 
 * @author mccreedy
 */
public class AlgorithmFaceAnonymizerBET extends AlgorithmBase {
    private int faceOrientation;
    
    private int mmToDelete;

    private int volumeSize;
    
    private int sliceSize;

    private int xDim;
    private int yDim;
    private int zDim;
    
    private boolean estimateWithSphereBET = false;
    private float imageInfluenceBET = 0.1f;
    private float stiffnessBET = 0.15f;
    
    private int currentProgressBarValue = 0;

    /**
     * Construct the face anonymizer, but do not run it yet.
     * @param srcImg The image to de-face
     * @param faceDirection the orientation of the patient's face, as determined by the dialog
     * @param extraMMsToDelete the number of millimeters to try to delete from the non-brain portions of the head
     */
    public AlgorithmFaceAnonymizerBET(ModelImage srcImg, int faceDirection, int extraMMsToDelete) {
        srcImage = srcImg;
        faceOrientation = faceDirection;
        mmToDelete = extraMMsToDelete;
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        volumeSize = sliceSize * zDim;
    }

    /**
     * Clean up memory used by the algorithm.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the message frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("AnonymizeFaceBET(" + ")\n");
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
     * Executes the de-facing algorithm.
     */
    private void anonymizeFace() {
        int curPosition, closestPosition;
        
        buildProgressBar( srcImage.getImageName(), "Anonymize face ...", 0, 100 );
        initProgressBar();
        
        // See if the image orientation is known
        int betOrientation;
        if (faceOrientation == JDialogFaceAnonymizer.FACING_RIGHT) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
            curPosition = Integer.MIN_VALUE;
            closestPosition = Integer.MIN_VALUE;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_LEFT) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
            curPosition = Integer.MAX_VALUE;
            closestPosition = Integer.MAX_VALUE;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_DOWN) {
            betOrientation = AlgorithmBrainExtractor.AXIAL;
            curPosition = Integer.MIN_VALUE;
            closestPosition = Integer.MIN_VALUE;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_UP) {
            betOrientation = AlgorithmBrainExtractor.AXIAL;
            curPosition = Integer.MAX_VALUE;
            closestPosition = Integer.MAX_VALUE;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_INTO_SCREEN) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
            curPosition = Integer.MIN_VALUE;
            closestPosition = Integer.MIN_VALUE;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_OUT_OF_SCREEN) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
            curPosition = Integer.MAX_VALUE;
            closestPosition = Integer.MAX_VALUE;
        } else {
            displayError("No face orientation given.");
            finalize();
            return;
        }
        
        incrementProgressBar(3);
        
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
        bet.setActiveImage(isActiveImage());
        if ( !ViewUserInterface.getReference().isAppFrameVisible()) {
            bet.setProgressBarVisible(false);
        }
        bet.run();
        
        incrementProgressBar(57);
        
        int updateInterval = srcImage.getSize() / 19;
        
        // calc plane from BET extraction
        BitSet brainMask = srcImage.getMask();
        // go through each slice, find the closest point in the direction the face should be pointing
        for (int i = 0; i < srcImage.getSize(); i++ ) {
            if (i % updateInterval == 0) {
                incrementProgressBar(1);
            }
            
            if (brainMask.get(i)) {
                curPosition = getNormalPosition(i);
                if (faceOrientation == JDialogFaceAnonymizer.FACING_RIGHT) {
                    if (curPosition > closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == JDialogFaceAnonymizer.FACING_LEFT) {
                    if (curPosition < closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == JDialogFaceAnonymizer.FACING_DOWN) {
                    if (curPosition > closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == JDialogFaceAnonymizer.FACING_UP) {
                    if (curPosition < closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == JDialogFaceAnonymizer.FACING_INTO_SCREEN) {
                    if (curPosition > closestPosition) {
                        closestPosition = curPosition;
                    }
                } else if (faceOrientation == JDialogFaceAnonymizer.FACING_OUT_OF_SCREEN) {
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
        if (faceOrientation == JDialogFaceAnonymizer.FACING_RIGHT) {
            closestPosition += MipavMath.round(percentBump * srcImage.getExtents()[0]);
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_LEFT) {
            closestPosition -= MipavMath.round(percentBump * srcImage.getExtents()[0]);
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_DOWN) {
            closestPosition += MipavMath.round(percentBump * srcImage.getExtents()[1]);
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_UP) {
            closestPosition -= MipavMath.round(percentBump * srcImage.getExtents()[1]);
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_INTO_SCREEN) {
            closestPosition += MipavMath.round(percentBump * srcImage.getExtents()[2]);
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_OUT_OF_SCREEN) {
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
        
        for (int i = 0; i < srcImage.getSize(); i++ ) {
            if (i % updateInterval == 0) {
                incrementProgressBar(1);
            }
            
            if (faceOrientation == JDialogFaceAnonymizer.FACING_RIGHT) {
                faceLevel = (i % sliceSize) / xDim;
                
                // in sagittal images, the face should be in the lower third of each slice (> yDim/3)
                if (faceLevel < yDim * .66) {
                    continue;
                }
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_LEFT) {
                faceLevel = (i % sliceSize) / xDim;
                
                // in sagittal images, the face should be in the lower third of each slice (> yDim/3)
                if (faceLevel < yDim * .66) {
                    continue;
                }
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_DOWN) {
                faceLevel = i / sliceSize;
                
                // in axial images, the face should be in the first third of the slices (< zDim/3)
                if (faceLevel > zDim * .33) {
                    continue;
                }
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_UP) {
                faceLevel = i / sliceSize;
                
                // in axial images, the face should be in the first third of the slices (< zDim/3)
                if (faceLevel > zDim * .33) {
                    continue;
                }
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_INTO_SCREEN) {
                faceLevel = (i % sliceSize) / xDim;
                
                // in coronal images, the face should be in the lower third of each slice (> yDim/3)
                if (faceLevel < yDim * .66) {
                    continue;
                }
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_OUT_OF_SCREEN) {
                faceLevel = (i % sliceSize) / xDim;
                
                // in coronal images, the face should be in the lower third of each slice (> yDim/3)
                if (faceLevel < yDim * .66) {
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
            
            if (faceOrientation == JDialogFaceAnonymizer.FACING_RIGHT) {
                removalIndex = i - (int) (mmToDelete / xRes);
                removalStep = 1;
                removalStart = removalIndex;
                removalEnd = i + (xDim - getNormalPosition(i));
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_LEFT) {
                removalIndex = i + (int) (mmToDelete / xRes);
                removalStep = 1;
                removalStart = i - getNormalPosition(i);
                removalEnd = removalIndex;
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_DOWN) {
                removalIndex = i - (int) ((int)(mmToDelete / yRes) * xDim);
                removalStep = xDim;
                removalStart = removalIndex;
                removalEnd = i + (sliceSize - (xDim * getNormalPosition(i)));
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_UP) {
                removalIndex = i + (int) ((int)(mmToDelete / yRes) * xDim);
                removalStep = xDim;
                removalStart = i - (xDim * getNormalPosition(i));
                removalEnd = removalIndex;
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_INTO_SCREEN) {
                removalIndex = i - (int) ((int)(mmToDelete / zRes) * sliceSize);
                removalStep = sliceSize;
                removalStart = removalIndex;
                removalEnd = i + (volumeSize - (sliceSize * getNormalPosition(i)));
            } else if (faceOrientation == JDialogFaceAnonymizer.FACING_OUT_OF_SCREEN) {
                removalIndex = i + (int) ((int)(mmToDelete / zRes) * sliceSize);
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
            for (int j = removalStart; j < removalEnd; j += removalStep ) {
                if (brainMask.get(j)) {
                    foundBrain = true;
                }
            }
            
            if (!foundBrain) {
                for (int j = removalStart; j < removalEnd; j += removalStep ) {
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
        
        progressBar.dispose();
        
        setCompleted(true);
    }

    /**
     * Get the position in the dimension normal to the face (in slice) from an index, based on the face orientation.
     * @param index the image index
     * @return the position of the index in the dimension normal to the face
     */
    private final int getNormalPosition(int index) {
        if (faceOrientation == JDialogFaceAnonymizer.FACING_RIGHT) {
            return index % xDim;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_LEFT) {
            return index % xDim;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_DOWN) {
            return (index % sliceSize) / xDim;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_UP) {
            return (index % sliceSize) / xDim;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_INTO_SCREEN) {
            return index / sliceSize;
        } else if (faceOrientation == JDialogFaceAnonymizer.FACING_OUT_OF_SCREEN) {
            return index / sliceSize;
        } else {
            return Integer.MAX_VALUE;
        }
    }
    
    /**
     * Changes the BET algorithm parameters from their defaults (which we choose).
     * @param estimateWithSphere whether to estimate the brain with a sphere initially
     * @param imageInfluence the image influence ratio
     * @param stiffness the mesh stiffness
     */
    public void setBETParameters(boolean estimateWithSphere, float imageInfluence, float stiffness) {
        estimateWithSphereBET = estimateWithSphere;
        imageInfluenceBET = imageInfluence;
        stiffnessBET = stiffness;
    }
    
    /**
     * Increment the progress bar by a value.
     * @param inc the amount to increment the progress bar by
     */
    private final void incrementProgressBar(int inc) {
        currentProgressBarValue += inc;
        progressBar.updateValue(currentProgressBarValue, activeImage);
    }
}
