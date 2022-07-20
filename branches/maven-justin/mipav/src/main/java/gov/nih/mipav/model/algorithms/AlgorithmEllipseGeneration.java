package gov.nih.mipav.model.algorithms;



import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import java.io.*;
import java.util.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * This module draws uniformly randomly positioned ellipses with a specified semi-major axis, semi-minor axis,
 * and angle phi between the x-axis and the major axis of the ellipse.
 
 */
public class AlgorithmEllipseGeneration extends AlgorithmBase {
    
    public static final int RANDOM = 1;
    
    // The first initialRandomEllipses are generated at random.  The remaining ellipses are only accepted if the 
    // nearest neighbor distance is <= maximumNearestNeighborDistance.
    public static final int AGGREGATED = 2;
    
    // Regular patterns can arise from inhibition or repulsion mechanisms which constrain objects to remain a
    // certain distance from each other.  The first ellipses is generated at random.  All other ellipses are only
    // accepted if the nearest neighbor distance is >= minimumNearestNeighborDistance and 
    // <= maximumNearestNeighborDistance.
    public static final int REGULAR = 3;
    
    // Very small and large distances between neighboring objects are allowed, but not intermediate distances.
    // Such constrained patterns are found in nature due to growth patterns.
    // The first ellipse is generated at random.  The remaining ellipses are only accepted if the nearest neighbor
    // distance is less than the lowestForbiddenNNDistance or greater than the highestForbiddenNNDistance.  For
    // each objected rejected from being in the forbidden intermediate range, the new replacement generated ellipse
    // must have a value greater than the highestForbiddenNNDistance and less than or equal to the 
    // highestRegenerationNNDistance.  The pattern obtained is not distinguishable from randomness for NN distances
    // less than the lowestForbiddenNNDistance and greater than the highestRegenerationNNDistance, but is regular
    // in the range from the lowestForbiddenNNDistance to the highestRegenerationNNDistnace with a peak of 
    // significance at an NN Distance just above the hgihestForbiddenNNDistance.
    public static final int CONSTRAINED = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Ellipse semi-major axis
    private int semiMajorAxis;
    
    // Ellipse semi-minor axis
    private int semiMinorAxis;
    
    // Angle between the x-axis and the major axis of the ellipse in radians
    private double phi;
    
    // number of ellipses to be drawn
    private int numEllipses;
    
    // Used in AGGREGATED.  initialRandomEllipses are drawn randomly.  The rest are drawn with nearestNeighborDistance
    // less than or equal to maximumNearestNeighborDistance
    private int initialRandomEllipses;
    
    // RANDOM, AGGREGATED, or REGULAR.
    private int pattern;
    
    // Used in REGULAR
    private double minimumNearestNeighborDistance;
    
    // Used in AGGREGATED and REGULAR
    private double maximumNearestNeighborDistance;
    
    private double lowestForbiddenNNDistance;
    
    private double highestForbiddenNNDistance;
    
    private double highestRegenerationNNDistance;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmEllipseGeneration - default constructor.
     */
    public AlgorithmEllipseGeneration() { }

    /**
     * AlgorithmEllipseGeneration.
     *
     * @param  srcImg   Blank source image in which circles will be drawn
     * @param  semiMajorAxis Ellipse semi-major axis
     * @param  semiMajorAxis Ellipse semi-minor axis
     * @param  phi Angle between the x-axis and the major axis of the ellipse in radians
     * @param  numEllipses Number of ellipses to be drawn
     * @param  pattern RANDOM, AGGREGATED, or REGULAR
     * @param  initialRandomEllipses Used in AGGREGATED.  initialRandomEllipses are drawn randomly.  The rest
     *         are drawn with nearestNeighborDistance less than or equal ot maximumNearestNeighborDistance.
     * @param  minimumNearestNeighborDistance Used in REGULAR
     * @param  maximumNearestNeighborDistance Used in AGGREGATED and REGULAR
     * @param  lowestForbiddenNNDistance Used in CONSTRAINED
     * @param  highestForbiddenNNDistance Used in CONSTRAINED
     * @param  highestRegeneerationNNDistance Used in CONSTRAINED
     */
    public AlgorithmEllipseGeneration(ModelImage srcImage, int semiMajorAxis, int semiMinorAxis, double phi,
    		int numEllipses, int pattern, int initialRandomEllipses, double minimumNearestNeighborDistance, 
    		double maximumNearestNeighborDistance, double lowestForbiddenNNDistance, double highestForbiddenNNDistance,
    		double highestRegenerationNNDistance) {
        super(null, srcImage);
        this.semiMajorAxis = semiMajorAxis;
        this.semiMinorAxis = semiMinorAxis;
        this.phi = phi;
        this.numEllipses = numEllipses;
        this.pattern = pattern;
        this.initialRandomEllipses = initialRandomEllipses;
        this.minimumNearestNeighborDistance = minimumNearestNeighborDistance;
        this.maximumNearestNeighborDistance = maximumNearestNeighborDistance;
        this.lowestForbiddenNNDistance = lowestForbiddenNNDistance;
        this.highestForbiddenNNDistance = highestForbiddenNNDistance;
        this.highestRegenerationNNDistance = highestRegenerationNNDistance;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int xDim;
        int yDim;
        byte mask[];
        int x;
        int y;
        int yDistSquared;
        int xDistSquared;
        int xMaskDim;
        int yMaskDim;
        int distSquared;
        int lowestDistSquared;
        int i;
        int j;
        int attempts;
        boolean found;
        int buffer[];
        int length;
        int xCenter = semiMajorAxis;
        int yCenter = semiMajorAxis;
        /** Reference to the random number generator. */
        RandomNumberGen randomGen;
        int ellipsesDrawn;
        int ellipseXCenter[] = new int[numEllipses];
        int ellipseYCenter[] = new int[numEllipses];
        int maskBytesSet;
        int numRandomEllipses;
        double minimumNNDistanceSquared;
        double maximumNNDistanceSquared;
        double lowestForbiddenSquared;
        double highestForbiddenSquared;
        double highestRegenerationSquared;
        boolean intermediateRejected;
        int theta;
        double angle;
        VOI boundaryVOI; 
        VOIBaseVector boundaryCurves;
        VOIBase boundaryBase;
        Vector<Vector3f> boundaryV = new Vector<Vector3f>();
        int xLow = Integer.MAX_VALUE;
        int xHigh = Integer.MIN_VALUE;
        int yLow = Integer.MAX_VALUE;
        int yHigh = Integer.MIN_VALUE;
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Ellipse generation ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        buffer = new int[length];
        // Create a mask for setting ellipses
        xMaskDim = 2 * semiMajorAxis + 1;
        yMaskDim = xMaskDim;
        mask = new byte[xMaskDim * yMaskDim];
        maskBytesSet = 0;
        boundaryVOI = new VOI((short)0, "boundaryVOI", VOI.CONTOUR, -1 );
        for (theta = 0; theta < 3600; theta++) {
        	angle = theta * Math.PI/1800;
            x = (int)Math.round(semiMajorAxis + semiMajorAxis*Math.cos(angle)*Math.cos(phi) - semiMinorAxis*Math.sin(angle)*Math.sin(phi));	
            y = (int)Math.round(semiMajorAxis + semiMajorAxis*Math.cos(angle)*Math.sin(phi) + semiMinorAxis*Math.sin(angle)*Math.cos(phi));
            if (mask[x + y * xMaskDim] == 0) {
                mask[x + y * xMaskDim] = 1;
                boundaryV.add(new Vector3f(x, y, 0.0f));
                maskBytesSet++;
            }
        }
        Vector3f pt[] = new Vector3f[boundaryV.size()];
    	for (i = 0; i < boundaryV.size(); i++) {
    		pt[i] = boundaryV.elementAt(i);
    	}
    	boundaryVOI.importCurve(pt);
    	boundaryCurves = boundaryVOI.getCurves();
    	boundaryBase = boundaryCurves.elementAt(0);
    	for (y = 0; y < yMaskDim; y++) {
    		for (x = 0; x < xMaskDim; x++) {
    			if (boundaryBase.contains(x, y)) {
    				if (mask[x + y * xMaskDim] == 0) {
    					mask[x + y * xMaskDim] = 1;
    					maskBytesSet++;	
    				}	    
    			}
    		}
    	}
    	for (y = 0; y < yMaskDim; y++) {
    		for (x = 0; x < xMaskDim; x++) {
    			if (mask[x + y * xMaskDim] == 1) {
    				if (x < xLow) {
    					xLow = x;
    				}
    				if (x > xHigh) {
    					xHigh = x;
    				}
    				if (y < yLow) {
    					yLow = y;
    				}
    				if (y > yHigh) {
    				    yHigh = y;	
    				}
    			}
    		}
    	}
        
        minimumNNDistanceSquared = minimumNearestNeighborDistance * minimumNearestNeighborDistance;
        maximumNNDistanceSquared = maximumNearestNeighborDistance * maximumNearestNeighborDistance;
        lowestForbiddenSquared = lowestForbiddenNNDistance * lowestForbiddenNNDistance;
        highestForbiddenSquared = highestForbiddenNNDistance * highestForbiddenNNDistance;
        highestRegenerationSquared = highestRegenerationNNDistance * highestRegenerationNNDistance;
        
        randomGen = new RandomNumberGen();
        switch(pattern) {
            case RANDOM:
                numRandomEllipses = numEllipses;
                break;
            case AGGREGATED:
                numRandomEllipses = initialRandomEllipses;
                break;
            case REGULAR:
            case CONSTRAINED:
                numRandomEllipses = 1;
                break;
            default:
                numRandomEllipses = numEllipses;
        }
        for (i = 1; i <= numRandomEllipses; i++) {
        found = false;
        attempts = 0;
            while ((!found) && (attempts <= 100)) {
                found = true;
                xCenter = randomGen.genUniformRandomNum(semiMajorAxis - xLow, xDim - (xHigh - semiMajorAxis) - 1);
                yCenter = randomGen.genUniformRandomNum(semiMajorAxis - yLow, yDim - (yHigh - semiMajorAxis) - 1);
                yloop:
                for (y = 0; y < yMaskDim; y++) {
                    for (x = 0; x < xMaskDim; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            if (buffer[(xCenter + x - semiMajorAxis) + xDim*(yCenter + y - semiMajorAxis)] != 0) {
                                found = false;
                                attempts++;
                                break yloop;
                            }
                        }
                    }
                } // for (y = 0; y <= 2*radius; y++)
            } // while ((!found) && (attempts <= 100)
            if (!found) {
                break;
            }
            ellipseXCenter[i-1] = xCenter;
            ellipseYCenter[i-1] = yCenter;
            for (y = 0; y < yMaskDim; y++) {
                for (x = 0; x < xMaskDim; x++) {
                    if (mask[x + y * xMaskDim] == 1) {
                        buffer[(xCenter + x - semiMajorAxis) + xDim*(yCenter + y - semiMajorAxis)] =  i;
                    }
                }
            }
        } // for (i = 1; i <= numRandomEllipses; i++)
        ellipsesDrawn = i-1;
        if (ellipsesDrawn == 1) {
            Preferences.debug("1 random ellipse drawn.  1 random ellipse requested.\n", Preferences.DEBUG_ALGORITHM);
            System.out.println("1 random ellipse drawn.  1 random ellipse requested.");    
        }
        else {
            Preferences.debug(ellipsesDrawn + " random ellipses drawn.  " + numEllipses + " random ellipses requested.\n", 
            		Preferences.DEBUG_ALGORITHM);
            System.out.println(ellipsesDrawn + " random ellipses drawn.  " + numEllipses + " random ellipses requested.");
        }
        
        if ((pattern == AGGREGATED) && (ellipsesDrawn == initialRandomEllipses)) {
            for (i = initialRandomEllipses+1; i <= numEllipses; i++) {
                found = false;
                attempts = 0;
                    while ((!found) && (attempts <= 10000)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(semiMajorAxis - xLow, xDim - (xHigh - semiMajorAxis) - 1);
                        yCenter = randomGen.genUniformRandomNum(semiMajorAxis - yLow, yDim - (yHigh - semiMajorAxis) - 1);
                        attemptloop:
                        {
                            for (y = 0; y < yMaskDim; y++) {
                                for (x = 0; x < yMaskDim; x++) {
                                    if (mask[x + y * xMaskDim] == 1) {
                                        if (buffer[(xCenter + x - semiMajorAxis) + xDim*(yCenter + y - semiMajorAxis)] != 0) {
                                            found = false;
                                            attempts++;
                                            break attemptloop;
                                        }
                                    }
                                }
                            } // for (y = 0; y < yMaskDim; y++)
                            for (j = 0; j < i-1; j++) {         
                                xDistSquared = ellipseXCenter[j] - xCenter;
                                xDistSquared = xDistSquared * xDistSquared;
                                yDistSquared = ellipseYCenter[j] - yCenter;
                                yDistSquared = yDistSquared * yDistSquared;
                                distSquared = xDistSquared + yDistSquared;
                                if (distSquared <= maximumNNDistanceSquared) {
                                    break attemptloop;
                                }  
                            }
                            found = false;
                            attempts++;
                        } // attemptloop
                    } // while ((!found) && (attempts <= 1000)
                    if (!found) {
                        break;
                    }
                    ellipseXCenter[i-1] = xCenter;
                    ellipseYCenter[i-1] = yCenter;
                    for (y = 0; y < yMaskDim; y++) {
                        for (x = 0; x < xMaskDim; x++) {
                            if (mask[x + y * xMaskDim] == 1) {
                                buffer[(xCenter + x - semiMajorAxis) + xDim*(yCenter + y - semiMajorAxis)] =  i;
                            }
                        }
                    }
                } // for (i = initialRandomEllipses+1; i <= numEllipses; i++)
                ellipsesDrawn = i-1; 
                Preferences.debug(ellipsesDrawn + " ellipses drawn.  " + numEllipses + " ellipses requested.\n", 
                		Preferences.DEBUG_ALGORITHM);
                System.out.println(ellipsesDrawn + " ellipses drawn.  " + numEllipses + " ellipses requested.");
        } // if ((pattern == AGGREGATED) && (ellipsesDrawn == initialRandomEllipses))
        
        if (pattern == REGULAR) {
            for (i = 2; i <= numEllipses; i++) {
                found = false;
                attempts = 0;
                wloop:
                    while ((!found) && (attempts <= 10000)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(semiMajorAxis - xLow, xDim - (xHigh - semiMajorAxis) - 1);
                        yCenter = randomGen.genUniformRandomNum(semiMajorAxis - yLow, yDim - (yHigh - semiMajorAxis) - 1);
                        for (y = 0; y < yMaskDim; y++) {
                            for (x = 0; x < xMaskDim; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - semiMajorAxis) + xDim*(yCenter + y - semiMajorAxis)] != 0) {
                                        found = false;
                                        attempts++;
                                        continue wloop;
                                    }
                                }
                            }
                        } // for (y = 0; y < semiMajorAxis; y++)
                        lowestDistSquared = Integer.MAX_VALUE;
                        for (j = 0; j < i-1; j++) {         
                            xDistSquared = ellipseXCenter[j] - xCenter;
                            xDistSquared = xDistSquared * xDistSquared;
                            yDistSquared = ellipseYCenter[j] - yCenter;
                            yDistSquared = yDistSquared * yDistSquared;
                            distSquared = xDistSquared + yDistSquared;
                            if (distSquared < lowestDistSquared) {
                                lowestDistSquared = distSquared;
                            }  
                        }
                        if ((lowestDistSquared < minimumNNDistanceSquared) || 
                            (lowestDistSquared > maximumNNDistanceSquared)) {
                            found = false;
                            attempts++;
                        }  
                    } // while ((!found) && (attempts <= 1000)
                    if (!found) {
                        break;
                    }
                    ellipseXCenter[i-1] = xCenter;
                    ellipseYCenter[i-1] = yCenter;
                    for (y = 0; y < yMaskDim; y++) {
                        for (x = 0; x < xMaskDim; x++) {
                            if (mask[x + y * xMaskDim] == 1) {
                                buffer[(xCenter + x - semiMajorAxis) + xDim*(yCenter + y - semiMajorAxis)] =  i;
                            }
                        }
                    }
                } // for (i = 2; i <= numEllipses; i++)
                ellipsesDrawn = i-1; 
                Preferences.debug(ellipsesDrawn + " ellipses drawn.  " + numEllipses + " ellipses requested.\n", 
                		Preferences.DEBUG_ALGORITHM);
                System.out.println(ellipsesDrawn + " ellipses drawn.  " + numEllipses + " ellipses requested.");    
        } // if (pattern == REGULAR)
        
        if (pattern == CONSTRAINED) {
            for (i = 2; i <= numEllipses; i++) {
                found = false;
                attempts = 0;
                intermediateRejected = false;
                wl:
                    while ((!found) && (attempts <= 10000)) {
                        found = true;
                        xCenter = randomGen.genUniformRandomNum(semiMajorAxis - xLow, xDim - (xHigh - semiMajorAxis) - 1);
                        yCenter = randomGen.genUniformRandomNum(semiMajorAxis - yLow, yDim - (yHigh - semiMajorAxis) - 1);
                        for (y = 0; y < semiMajorAxis; y++) {
                            for (x = 0; x < semiMajorAxis; x++) {
                                if (mask[x + y * xMaskDim] == 1) {
                                    if (buffer[(xCenter + x - semiMajorAxis) + xDim*(yCenter + y - semiMajorAxis)] != 0) {
                                        found = false;
                                        attempts++;
                                        continue wl;
                                    }
                                }
                            }
                        } // for (y = 0; y <= 2*radius; y++)
                        lowestDistSquared = Integer.MAX_VALUE;
                        for (j = 0; j < i-1; j++) {         
                            xDistSquared = ellipseXCenter[j] - xCenter;
                            xDistSquared = xDistSquared * xDistSquared;
                            yDistSquared = ellipseYCenter[j] - yCenter;
                            yDistSquared = yDistSquared * yDistSquared;
                            distSquared = xDistSquared + yDistSquared;
                            if (distSquared < lowestDistSquared) {
                                lowestDistSquared = distSquared;
                            }  
                        }
                        if ((!intermediateRejected) && (lowestDistSquared >= lowestForbiddenSquared) && 
                            (lowestDistSquared <= highestForbiddenSquared)) {
                            found = false;
                            intermediateRejected = true;
                            attempts++;
                        } 
                        else if (intermediateRejected && ((lowestDistSquared <= highestForbiddenSquared) ||
                                (lowestDistSquared > highestRegenerationSquared))) {
                            found = false;
                            attempts++;
                        }
                    } // while ((!found) && (attempts <= 1000)
                    if (!found) {
                        break;
                    }
                    ellipseXCenter[i-1] = xCenter;
                    ellipseYCenter[i-1] = yCenter;
                    for (y = 0; y < yMaskDim; y++) {
                        for (x = 0; x < xMaskDim; x++) {
                            if (mask[x + y * xMaskDim] == 1) {
                                buffer[(xCenter + x - semiMajorAxis) + xDim*(yCenter + y - semiMajorAxis)] =  i;
                            }
                        }
                    }
                } // for (i = 2; i <= numEllipses; i++)
                ellipsesDrawn = i-1; 
                Preferences.debug(ellipsesDrawn + " ellipses drawn.  " + numEllipses + " ellipses requested.\n", 
                		Preferences.DEBUG_ALGORITHM);
                System.out.println(ellipsesDrawn + " ellipses drawn.  " + numEllipses + " ellipses requested.");     
        } // if (pattern == CONSTRAINED)
        
       for (i = 0; i < buffer.length; i++) {
           if (buffer[i] > 0) {
               buffer[i] = 1;
           }
       }
       try {
           srcImage.importData(0, buffer, true);
       }
       catch(IOException e) {
           MipavUtil.displayError("IO exception on srcImage.importData(0, buffer, true)");
           setCompleted(false);
           return;
       }
       
       setCompleted(true);
       return;
    }
    
    
}
