package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJProgressBar;

public class AlgorithmRegionsFromPartialBorders extends AlgorithmBase {
	
	private double threshold;
	
	private boolean darkObjectOnWhiteBackground;
	
	private double maximumDistance;
	
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmRegionsFromPartialBorders object.
     *
     * @param  srcImg  source image model
     */
    public AlgorithmRegionsFromPartialBorders(ModelImage destImg, ModelImage srcImg, double threshold,
    		boolean darkObjectOnWhiteBackground, double maximumDistance) {
        super(destImg, srcImg);
        this.threshold = threshold;
        this.darkObjectOnWhiteBackground = darkObjectOnWhiteBackground;
        this.maximumDistance = maximumDistance;
    }
    
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int length;
    	double imgBuffer[];
    	int x;
    	int y;
    	int z;
    	int d;
    	// 0, 45, 90, 135, 180, 225,270, 315 degrees, y, x
    	short mask[][][] = new short[8][3][3];
    	double edgeMagnitude[];
    	double suppressedMagnitude[];
    	int edgeDirection[];
    	double magnitude;
    	int direction;
    	double currentMagnitude;
    	int xd;
    	int yd;
    	int xe;
    	int ye;
    	boolean edgeFound;
    	int i;
    	short output[];
    	
    	// 0 degrees
    	mask[0][0][0] = 1;
    	mask[0][0][1] = 1;
    	mask[0][0][2] = 1;
    	mask[0][1][0] = 0;
    	mask[0][1][1] = 0;
    	mask[0][1][2] = 0;
    	mask[0][2][0] = -1;
    	mask[0][2][1] = -1;
    	mask[0][2][2] = -1;
    	
    	// 45 degrees
    	mask[1][0][0] = 1;
    	mask[1][0][1] = 1;
    	mask[1][0][2] = 0;
    	mask[1][1][0] = 1;
    	mask[1][1][1] = 0;
    	mask[1][1][2] = -1;
    	mask[1][2][0] = 0;
        mask[1][2][1] = -1;
        mask[1][2][2] = -1;
        
        // 90 degrees
        mask[2][0][0] = 1;
        mask[2][0][1] = 0;
        mask[2][0][2] = -1;
        mask[2][1][0] = 1;
        mask[2][1][1] = 0;
        mask[2][1][2] = -1;
        mask[2][2][0] = 1;
        mask[2][2][1] = 0;
        mask[2][2][2] = -1;
        
        // 135 degrees
        mask[3][0][0] = 0;
        mask[3][0][1] = -1;
        mask[3][0][2] = -1;
        mask[3][1][0] = 1;
        mask[3][1][1] = 0;
        mask[3][1][2] = -1;
        mask[3][2][0] = 1;
        mask[3][2][1] = 1;
        mask[3][2][2] = 0;
        
        // 180 degrees
        mask[4][0][0] = -1;
        mask[4][0][1] = -1;
        mask[4][0][2] = -1;
        mask[4][1][0] = 0;
        mask[4][1][1] = 0;
        mask[4][1][2] = 0;
        mask[4][2][0] = 1;
        mask[4][2][1] = 1;
        mask[4][2][2] = 1;
        
        // 225 degrees
        mask[5][0][0] = -1;
        mask[5][0][1] = -1;
        mask[5][0][2] = 0;
        mask[5][1][0] = -1;
        mask[5][1][1] = 0;
        mask[5][1][2] = 1;
        mask[5][2][0] = 0;
        mask[5][2][1] = 1;
        mask[5][2][2] = 1;
        
        // 270 degrees
        mask[6][0][0] = -1;
        mask[6][0][1] = 0;
        mask[6][0][2] = 1;
        mask[6][1][0] = -1;
        mask[6][1][1] = 0;
        mask[6][1][2] = 1;
        mask[6][2][0] = -1;
        mask[6][2][1] = 0;
        mask[6][2][2] = 1;
        
        // 315 degrees
        mask[7][0][0] = 0;
        mask[7][0][1] = 1;
        mask[7][0][2] = 1;
        mask[7][1][0] = -1;
        mask[7][1][1] = 0;
        mask[7][1][2] = 1;
        mask[7][2][0] = -1;
        mask[7][2][1] = -1;
        mask[7][2][2] = 0;
        
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
    	
        if (srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }
        
        try {
            imgBuffer = new double[length];
            edgeMagnitude = new double[length];
            suppressedMagnitude = new double[length];
            edgeDirection = new int[length];
            output = new short[length];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm VOIExtraction: Out of memory creating buffers");
            setCompleted(false);

            return;
        }
        
        try {
            srcImage.setLock(ModelStorageBase.W_LOCKED);
        } catch (IOException error) {
            displayError("Algorithm VOI Extraction: Image locked");
            setCompleted(false);
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

            return;
        }



        for (z = 0; z < zDim; z++) {

            try {
                srcImage.exportDataNoLock(z * length, length, imgBuffer);
            } catch (IOException error) {
                displayError("Algorithm VOI Extraction: image bounds exceeded");
                setCompleted(false);
                
                srcImage.releaseLock();

                return;
            }
            
            for (y = 1; y < yDim-1; y++) {
            	for (x = 1; x < xDim-1; x++) {
            		magnitude = 0.0;
            		direction = 0;
            		for (d = 0; d <= 7; d++) {
            			currentMagnitude = 0.0;
            		    for (yd = y - 1; yd <= y + 1; yd++) {
            		    	for (xd = x - 1; xd <= x + 1; xd++) {
            		    	    currentMagnitude += imgBuffer[xd + xDim * yd] * mask[d][yd-y-1][xd-x-1];	
            		    	}
            		    } // for (yd = y - 1; yd <= y + 1; yd++)
            		    if (Math.abs(currentMagnitude) > magnitude) {
            		    	magnitude = Math.abs(currentMagnitude);
            		    	direction = d;
            		    }
            		} // for (d = 0; d <= 7; d++)
            		if (magnitude >= threshold) {
            		    edgeMagnitude[x + y * xDim] = magnitude;
            		}
            		else {
            			edgeMagnitude[x + y * xDim] = 0.0;
            		}
            		edgeDirection[x + y * xDim] = direction;
            	} // for (x = 1; x < xDim-1; x++)
            } // for (y = 1; y < yDim-1; y++)
            
            for (y = 1; y < yDim-1; y++) {
                for (x = 1; x < xDim -1; x++) {
                    switch(edgeDirection[x + y * xDim]) {
                    case 0:
                    case 4:
                    	if ((edgeMagnitude[x + (y+1)*xDim] > edgeMagnitude[x + y * xDim]) ||
                    		(edgeMagnitude[x + (y-1)*xDim] > edgeMagnitude[x + y * xDim])) {
                    		suppressedMagnitude[x + y * xDim] = 0.0;
                    	}
                    	else {
                    		suppressedMagnitude[x + y * xDim] = edgeMagnitude[x + y * xDim];
                    	}
                    	break;
                    case 1:
                    case 5:
                    	if ((edgeMagnitude[x+1 + (y-1)*xDim] > edgeMagnitude[x + y * xDim]) ||
                    		(edgeMagnitude[x-1 + (y+1)*xDim] > edgeMagnitude[x + y * xDim])) {
                    		suppressedMagnitude[x + y * xDim] = 0.0;
                    	}
                    	else {
                    		suppressedMagnitude[x + y * xDim] = edgeMagnitude[x + y * xDim];
                    	}
                    	break;
                    case 2:
                    case 6:
                    	if ((edgeMagnitude[x-1 + y * xDim] > edgeMagnitude[x + y * xDim]) ||
                    		(edgeMagnitude[x+1 + y * xDim] > edgeMagnitude[x + y * xDim])) {
                    		suppressedMagnitude[x + y * xDim] = 0.0;
                    	}
                    	else {
                    		suppressedMagnitude[x + y * xDim] = edgeMagnitude[x + y * xDim];
                    	}
                    	break;
                    case 3:
                    case 7:
                    	if ((edgeMagnitude[x-1 + (y-1) * xDim] > edgeMagnitude[x + y * xDim]) ||
                    		(edgeMagnitude[x+1 + (y+1) * xDim] > edgeMagnitude[x + y * xDim])) {
                    		suppressedMagnitude[x + y * xDim] = 0.0;
                    	}
                    	else {
                    		suppressedMagnitude[x + y * xDim] = edgeMagnitude[x + y * xDim];
                    	}
                    }
                }
            } // for (y = 1; y < yDim-1; y++)
            
            for (i = 0; i < length; i++) {
            	output[i]= 0;
            }
            
            if (darkObjectOnWhiteBackground) {
            	for (y = 1; y < yDim-1; y++) {
            		for (x = 1; x < xDim-1; x++) {
            			edgeFound = false;
            			switch(edgeDirection[x + y * xDim]) {
            			case 0:
            				if (suppressedMagnitude[x + (y+1)*xDim] >= suppressedMagnitude[x + (y-1)*xDim]) {
            					for (yd = 1; yd <= maximumDistance && (y - yd) >= 1; yd++) {
            						if (suppressedMagnitude[x + (y-yd) * xDim] > 0) {
            							edgeFound = true;
            					        break;
            						}
            					}
            					if (edgeFound && (edgeDirection[x + (y-yd) * xDim] >= 3) && (edgeDirection[x + (y-yd)*xDim] <= 5)) {
            					    for (ye = 1; ye < yd; ye++) {
            					    	output[x + (y - ye)*xDim]++;
            					    }
            					}
            				} // if (suppressedMagnitude[x + (y+1)*xDim] >= suppressedMagnitude[x + (y-1)*xDim]) 
            				else {
            					for (yd = 1; yd <= maximumDistance && (y + yd) < yDim-1; yd++) {
            						if (suppressedMagnitude[x + (y+yd) * xDim] > 0) {
            							edgeFound = true;
            					        break;
            						}
            					}
            					if (edgeFound && (edgeDirection[x + (y+yd) * xDim] >= 3) && (edgeDirection[x + (y+yd)*xDim] <= 5)) {
            					    for (ye = 1; ye < yd; ye++) {
            					    	output[x + (y + ye)*xDim]++;
            					    }
            					}	
            				}
            				break;
            			case 1:
            				if (suppressedMagnitude[x+1 + (y-1) * xDim] >= suppressedMagnitude[x-1 + (y+1) * xDim]) {
            					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x - xd) >= 1 &&
            							(y + yd) < yDim-1; xd++, yd++) {
            						if (suppressedMagnitude[x-xd + (y+yd) * xDim] > 0) {
            							edgeFound = true;
            							break;
            						}
            					}
            					if (edgeFound && (edgeDirection[x-xd + (y+yd) * xDim] >= 4) && (edgeDirection[x-xd + (y+yd) * xDim] <= 6)) {
            						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
            							output[x-xe + (y+ye) * xDim]++;
            						}
            					}
            				}
            				else {
            					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x + xd) < xDim-1 &&
            							(y - yd) >= 1; xd++, yd++) {
            						if (suppressedMagnitude[x+xd + (y-yd) * xDim] > 0) {
            							edgeFound = true;
            							break;
            						}
            					}
            					if (edgeFound && (edgeDirection[x+xd + (y-yd) * xDim] >= 4) && (edgeDirection[x+xd + (y-yd) * xDim] <= 6)) {
            						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
            							output[x+xe + (y-ye) * xDim]++;
            						}
            					}	
            				}
            				break;
            			case 2:
            				if (suppressedMagnitude[x+1 + y * xDim] >= suppressedMagnitude[x-1 + y * xDim]) {
            					for (xd = 1; xd <= maximumDistance && (x - xd) >= 1; xd++) {
            						if (suppressedMagnitude[x-xd + y * xDim] > 0) {
            							edgeFound = true;
            						    break;
            						}
            					}
            				}
            				break;
            			}
            		} // for (x = 1; x < xDim - 1; x++)
            	} // for (y = 1; y < yDim-1; y++)
            } // if (darkObjectOnWhiteBackground)
        } // for (z = 0; z < zDim; z++)
    }

}