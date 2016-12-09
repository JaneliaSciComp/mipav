package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

/**
 * 
 * @author ilb
 * References:
 * 1.) "Texture Primitive Extraction Using an Edge-Based Approach" by Tsai-Hong Hong, Charles R. Dyer, and
 * Azriel Rosenfeld, IEEE Transactions on Systems, Man, and Cybernetics, Vol. SMC-10, No. 10, October, 1980,
 * pp. 659-675.
 * 2.) Image Processing, Analysis, and Machine Vision 4th edition International Edition by Milan Sonka,
 * Vaclav Hlavac, and Roger Boyle, Section 6.2.8 Region construction from borders, pp. 218-220.
 */

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
    	int tDim;
    	int nDims;
    	int length;
    	double imgBuffer[];
    	int x;
    	int y;
    	int z;
    	int t;
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
    	byte primitive[];
    	byte primitive2[];
    	double sum = 0.0;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
    	
    	fireProgressStateChanged(0, srcImage.getImageName(), "Regions From Partial Borders ...");

    	
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
    	
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        nDims = srcImage.getNDims();
    	
        if (nDims > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        
        try {
            imgBuffer = new double[length];
            edgeMagnitude = new double[length];
            suppressedMagnitude = new double[length];
            edgeDirection = new int[length];
            output = new short[length];
            primitive = new byte[length];
            primitive2 = new byte[length];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm VOIExtraction: Out of memory creating buffers");
            setCompleted(false);

            return;
        }

        for (t = 0; t < tDim; t++) {
        for (z = 0; z < zDim; z++) {

            try {
                srcImage.exportData((z + t*zDim)*length, length, imgBuffer);
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
            		    for (yd = -1; yd <= 1; yd++) {
            		    	for (xd = -1; xd <= 1; xd++) {
            		    	    currentMagnitude += imgBuffer[x+xd + xDim * (y+yd)] * mask[d][yd+1][xd+1];	
            		    	}
            		    } // for (yd = -1; yd <= 1; yd++)
            		    if (currentMagnitude > magnitude) {
            		    	magnitude = currentMagnitude;
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
            			if (suppressedMagnitude[x + y * xDim] > 0.0) {
            			edgeFound = false;
            			switch(edgeDirection[x + y * xDim]) {
            			case 0:
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
            				break;
            			case 1:
        					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x + xd) < xDim-1 &&
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
            				break;
            			case 2:
        					for (xd = 1; xd <= maximumDistance && (x + xd) < xDim-1; xd++) {
        						if (suppressedMagnitude[x+xd + y * xDim] > 0) {
        							edgeFound = true;
        						    break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x+xd + y * xDim] >=5) && (edgeDirection[x+xd + y * xDim] <= 7)) {
        						for (xe = 1; xe < xd; xe++) {
        							output[x+xe + y * xDim]++;
        						}
        					}	
            				break;
            			case 3:
        					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x + xd) < xDim-1 &&
        							(y - yd) >= 1; xd++, yd++) {
        						if (suppressedMagnitude[x+xd + (y-yd) * xDim] > 0) {
        							edgeFound = true;
        							break;
        						}
        					}
        					if (edgeFound && ((edgeDirection[x+xd + (y-yd) * xDim] == 6) || (edgeDirection[x+xd + (y-yd) * xDim] == 7) ||
        							(edgeDirection[x+xd + (y-yd) * xDim] == 0))) {
        						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
        							output[x+xe + (y-ye) * xDim]++;
        						}
        					}
            			    break;	
            			case 4:
        					for (yd = 1; yd <= maximumDistance && (y - yd) >= 1; yd++) {
        						if (suppressedMagnitude[x + (y-yd) * xDim] > 0) {
        							edgeFound = true;
        					        break;
        						}
        					}
        					if (edgeFound && ((edgeDirection[x + (y-yd) * xDim] == 7) || (edgeDirection[x + (y-yd)*xDim] == 0) ||
        							(edgeDirection[x + (y-yd)*xDim] == 1))) {
        					    for (ye = 1; ye < yd; ye++) {
        					    	output[x + (y - ye)*xDim]++;
        					    }
        					}
            				break;
            			case 5:
        					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x - xd) >= 1 &&
        							(y - yd) >= 1; xd++, yd++) {
        						if (suppressedMagnitude[x-xd + (y-yd) * xDim] > 0) {
        							edgeFound = true;
        							break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x-xd + (y-yd) * xDim] >= 0) && (edgeDirection[x-xd + (y-yd) * xDim] <= 2)) {
        						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
        							output[x-xe + (y-ye) * xDim]++;
        						}
        					}
            				break;
            			case 6:
        					for (xd = 1; xd <= maximumDistance && (x - xd) >= 1; xd++) {
        						if (suppressedMagnitude[x-xd + y * xDim] > 0) {
        							edgeFound = true;
        						    break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x-xd + y * xDim] >=1) && (edgeDirection[x-xd + y * xDim] <= 3)) {
        						for (xe = 1; xe < xd; xe++) {
        							output[x-xe + y * xDim]++;
        						}
        					}
            				break;
            			case 7:
        					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x - xd) >= 1 &&
        							(y + yd) < yDim-1; xd++, yd++) {
        						if (suppressedMagnitude[x-xd + (y+yd) * xDim] > 0) {
        							edgeFound = true;
        							break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x-xd + (y+yd) * xDim] >= 2) && (edgeDirection[x-xd + (y+yd) * xDim] <= 4)) {
        						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
        							output[x-xe + (y+ye) * xDim]++;
        						}
        					}
            			} // switch(edgeDirection[x + y * xDim])
            			} // if (suppressedMagnitude[x + y * xDim] > 0.0)
            		} // for (x = 1; x < xDim - 1; x++)
            	} // for (y = 1; y < yDim-1; y++)
            } // if (darkObjectOnWhiteBackground)
            else { // whiteObjectOnDarkBackground
            	for (y = 1; y < yDim-1; y++) {
            		for (x = 1; x < xDim-1; x++) {
            			if (suppressedMagnitude[x + y * xDim] > 0.0){
            			edgeFound = false;
            			switch(edgeDirection[x + y * xDim]) {
            			case 0:
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
            				break;
            			case 1:
        					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x - xd) >= 1 &&
        							(y - yd) >= 1; xd++, yd++) {
        						if (suppressedMagnitude[x-xd + (y-yd) * xDim] > 0) {
        							edgeFound = true;
        							break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x-xd + (y-yd) * xDim] >= 4) && (edgeDirection[x-xd + (y-yd) * xDim] <= 6)) {
        						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
        							output[x-xe + (y-ye) * xDim]++;
        						}
        					}
            				break;
            			case 2:
        					for (xd = 1; xd <= maximumDistance && (x - xd) >= 1; xd++) {
        						if (suppressedMagnitude[x-xd + y * xDim] > 0) {
        							edgeFound = true;
        						    break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x-xd + y * xDim] >=5) && (edgeDirection[x-xd + y * xDim] <= 7)) {
        						for (xe = 1; xe < xd; xe++) {
        							output[x-xe + y * xDim]++;
        						}
        					}
            				break;
            			case 3:	
        					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x - xd) >= 1 &&
        							(y + yd) < yDim-1; xd++, yd++) {
        						if (suppressedMagnitude[x-xd + (y+yd) * xDim] > 0) {
        							edgeFound = true;
        							break;
        						}
        					}
        					if (edgeFound && ((edgeDirection[x-xd + (y+yd) * xDim] == 6) || (edgeDirection[x-xd + (y+yd) * xDim] == 7) ||
        							(edgeDirection[x-xd + (y+yd) * xDim] == 0))) {
        						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
        							output[x-xe + (y+ye) * xDim]++;
        						}
        					}
            			    break;	
            			case 4:
        					for (yd = 1; yd <= maximumDistance && (y + yd) < yDim-1; yd++) {
        						if (suppressedMagnitude[x + (y+yd) * xDim] > 0) {
        							edgeFound = true;
        					        break;
        						}
        					}
        					if (edgeFound && ((edgeDirection[x + (y+yd) * xDim] == 7) || (edgeDirection[x + (y+yd)*xDim] == 0) ||
        							(edgeDirection[x + (y+yd)*xDim] == 1))) {
        					    for (ye = 1; ye < yd; ye++) {
        					    	output[x + (y + ye)*xDim]++;
        					    }
        					}	
            				break;
            			case 5:
        					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x + xd) < xDim-1 &&
        							(y + yd) < yDim-1; xd++, yd++) {
        						if (suppressedMagnitude[x+xd + (y+yd) * xDim] > 0) {
        							edgeFound = true;
        							break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x+xd + (y+yd) * xDim] >= 0) && (edgeDirection[x+xd + (y+yd) * xDim] <= 2)) {
        						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
        							output[x+xe + (y+ye) * xDim]++;
        						}
        					}
            				break;
            			case 6:
        					for (xd = 1; xd <= maximumDistance && (x + xd) < xDim-1; xd++) {
        						if (suppressedMagnitude[x+xd + y * xDim] > 0) {
        							edgeFound = true;
        						    break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x+xd + y * xDim] >=1) && (edgeDirection[x+xd + y * xDim] <= 3)) {
        						for (xe = 1; xe < xd; xe++) {
        							output[x+xe + y * xDim]++;
        						}
        					}	
            				break;
            			case 7:
        					for (xd = 1, yd = 1; Math.sqrt(xd*xd + yd*yd) <= maximumDistance && (x + xd) < xDim-1 &&
        							(y - yd) >= 1; xd++, yd++) {
        						if (suppressedMagnitude[x+xd + (y-yd) * xDim] > 0) {
        							edgeFound = true;
        							break;
        						}
        					}
        					if (edgeFound && (edgeDirection[x+xd + (y-yd) * xDim] >= 2) && (edgeDirection[x+xd + (y-yd) * xDim] <= 4)) {
        						for (xe = 1, ye = 1; xe < xd; xe++, ye++) {
        							output[x+xe + (y-ye) * xDim]++;
        						}
        					}
            			} // switch(edgeDirection[x + y * xDim])
            			} // if (suppressedMagnitude[x + y * xDim] > 0.0)
            		} // for (x = 1; x < xDim - 1; x++)
            	} // for (y = 1; y < yDim-1; y++)	
            } // whiteObjectOnDarkBackground
            
            for (i = 0; i < length; i++) {
            	primitive[i] = 0;
            	primitive2[i] = 0;
            }
            
            for (y = 1; y < yDim-1; y++) {
            	for (x = 1; x < xDim-1; x++) {
            		sum = 0.0;
            	    for (yd = -1; yd <= 1; yd++) {
            	    	for (xd = -1; xd <= 1; xd++) {
            	    		switch(output[x+xd + (y+yd)*xDim]) {
            	    		case 0:
            	    			break;
            	    		case 1:
            	    			sum += 0.1;
            	    			break;
            	    		case 2:
            	    			sum += 0.2;
            	    			break;
            	    		case 3:
            	    			sum += 0.5;
            	    			break;
            	    		default:
            	    			sum += 1.0;
            	    		}
            	    	} // for (xd = -1; xd <= 1; xd++)
            	    } // for (yd = -1; yd <= 1; yd++)
            	    if (sum >= 1.0) {
            	    	primitive[x + y * xDim] = 1;
            	    }
            	} // for (x = 1; x < xDim-1; x++)
            } // for (y = 1; y < yDim-1; y++)
            
            
            for (y = 1; y < yDim-1; y ++) {
            	for (x = 1; x < xDim-1; x++) {
            	    if (primitive[x + y * xDim] == 1) {
            	    	primitive2[x + y * xDim] = 1;
            	    }
            	    else {
            	    	loop: for (yd = -1; yd <= 1; yd++) {
            	    		for (xd = -1; xd <= 1; xd++) {
            	    			if ((imgBuffer[x + y * xDim] == imgBuffer[x+xd + (y+yd) * xDim]) && 
            	    					(primitive[x + xd + (y+yd) * xDim] == 1)) {
            	    				primitive2[x + y * xDim] = 1;
            	    				break loop;
            	    			}
            	    		}
            	    	}
            	    }
            	}
            } // for (y = 1; y < yDim-1; y++)
            
            if (darkObjectOnWhiteBackground) {
                for (i = 0; i < length; i++) {
                	primitive2[i] = (byte)(1 - primitive2[i]);
                }
            }
            
            
			try {
			    destImage.importData((z + t*zDim)*length, primitive2, false);
			}
			catch(IOException e) {
				MipavUtil.displayError("IOException " + e + " on destImage.importData");
				setCompleted(false);
				return;
			}
    		
        } // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        destImage.calcMinMax();
        
        setCompleted(true);
        return;
    }

}