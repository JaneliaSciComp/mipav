package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.io.*;
import java.util.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmVOIExtraction extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private VOI addedVOI;

    /** DOCUMENT ME! */
    private Color[] colorTable = null;

    /** DOCUMENT ME! */
    private short[] expImgBuffer;

    /** DOCUMENT ME! */
    private short[] grayScaleArray = new short[10000];

    /** DOCUMENT ME! */
    private short[] imgBuffer;

    /** DOCUMENT ME! */
    private BitSet mask = null;

    /** DOCUMENT ME! */
    private BitSet mask2 = null;

    /** DOCUMENT ME! */
    private BitSet maskAll = null;

    /** DOCUMENT ME! */
    private short[][] maskList = null;

    /** DOCUMENT ME! */
    private String[] nameTable = null;

    /** DOCUMENT ME! */
    private short[][] nextMaskList = null;

    /** DOCUMENT ME! */
    private short[] objBuffer;

    /** DOCUMENT ME! */
    private BitSet outMask = null;

    /** DOCUMENT ME! */
    private int[] VOIIndexArray = new int[10000];

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int xDimE;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int yDimE;
    
    private BitSet maskExpanded = null;
    
    // connected = 4 or connected = 8
    private int connected = 8;
    
    private int numVOIAdded = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmVOIExtraction object.
     *
     * @param  srcImg  source image model
     */
    public AlgorithmVOIExtraction(ModelImage srcImg) {
        super(null, srcImg);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        int i;

        imgBuffer = null;
        objBuffer = null;
        expImgBuffer = null;
        mask = null;
        outMask = null;
        mask2 = null;
        maskAll = null;
        maskExpanded = null;

        if (maskList != null) {

            for (i = 0; i < maskList.length; i++) {
                maskList[i] = null;
            }
        }

        maskList = null;

        if (nextMaskList != null) {

            for (i = 0; i < nextMaskList.length; i++) {
                nextMaskList[i] = null;
            }
        }

        nextMaskList = null;
        grayScaleArray = null;
        VOIIndexArray = null;
        addedVOI = null;
        colorTable = null;
        if (nameTable != null) {
        	for (i = 0; i < nameTable.length; i++) {
        		nameTable[i] = null;
        	}
        }
        nameTable = null;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        System.gc();
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        calcInPlace();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  colorTable  DOCUMENT ME!
     */
    public void setColorTable(Color[] colorTable) {
        this.colorTable = colorTable;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nameTable  DOCUMENT ME!
     */
    public void setNameTable(String[] nameTable) {
        this.nameTable = nameTable;
    }
    
    /**
     * From text description in 4th edition Image Processing, Analysis, and Machine Vision International Edition
    * by Milan Sonka, Vaclav Hlavac, and Roger Boyle, Algorithm 6.8 Extended Boundary Tracing, pp. 195-196. 
    */
   @SuppressWarnings("unused")
    private void extendedBoundaryTracing() {
    	ViewVOIVector VOIs = null;
    	int length;
    	int paddedLength;
    	int paddedXDim;
    	int paddedYDim;
    	int zDim;
    	int x = 0;
    	int y = 0;
    	int z;
    	int i, j;
    	int numBoundaryPixels = 0;
    	int boundaryBuffer[];
    	boolean found;
    	int index = 0;
    	int dir = 0;
    	short voiID;
    	int xArrInt[];
    	int yArrInt[];
    	int zArrInt[];
    	int xinc;
    	int yinc;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }

        if ((srcImage.getType() != ModelImage.BOOLEAN) && (srcImage.getType() != ModelImage.UBYTE) &&
                (srcImage.getType() != ModelImage.BYTE) && (srcImage.getType() != ModelImage.USHORT) &&
                (srcImage.getType() != ModelImage.SHORT)) {
            displayError("Source Image must be Boolean, UByte, Byte, UShort, or Short");
            setCompleted(false);

            return;
        }

        VOIs = srcImage.getVOIs();

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        paddedXDim = xDim + 2;
        paddedYDim = yDim + 2;
        paddedLength = paddedXDim * paddedYDim;
        
        if (srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }
        
        try {
            imgBuffer = new short[length];
            objBuffer = new short[paddedLength];
            boundaryBuffer = new int[length];
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
            
            
            for (short obj = 1; obj < (srcImage.getMax() + 1); obj++) {
            	
            	for (int m = 0; m < imgBuffer.length; m++) {
                    x = m % xDim;
                    y = m / xDim;
                    if (imgBuffer[m] == obj) {
                        objBuffer[(x+1) + (y+1)*paddedXDim] = obj;
                    } else {
                        objBuffer[(x+1) + (y+1)*paddedXDim] = 0;
                    }
                    
                } // for (int m = 0; m < imgBuffer.length; m++)
                
                found = false;
                xinc = 1;
                yinc = 1;
                for (y = 1; y <= yDim && !found; y+= yinc) {
                	for (x = 1; x <= xDim && !found; x+= xinc) {
                	    index = x + y * paddedXDim;
                	    if (objBuffer[index] == obj) {
                	    	found = true;
                	    	boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                	    	dir = 3;
                	    	y = y + 1;
                	    	index = index + paddedYDim;
                	    	boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                	    	xinc = 0;
                	    	yinc = 0;
                	    }
                	} // for (x = 1; x <= xDim; x++)
                } // for (y = 1; y <= yDim; y++)
                if (found) {
                    while ((numBoundaryPixels < 4) || (boundaryBuffer[0] != boundaryBuffer[numBoundaryPixels-2]) ||
                    		(boundaryBuffer[1] != boundaryBuffer[numBoundaryPixels-1])) {
                    	if (dir == 0) { // right
                    		if ((objBuffer[index - 1 - paddedYDim] == obj) && (objBuffer[index - paddedYDim] != obj) &&
                    		    (objBuffer[index - 1] != obj)) {
                    			dir = 1;
                    			y = y - 1;
                    			index = index - paddedYDim;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else if ((objBuffer[index - 1 - paddedYDim] == obj) && (objBuffer[index - paddedYDim] == obj)
                    				&& (objBuffer[index - 1] != obj) && (objBuffer[index] != obj)) {
                    			dir = 0;
                    			x = x + 1;
                    			index = index + 1;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else if ((objBuffer[index - 1 - paddedYDim] == obj) && (objBuffer[index - paddedYDim] == obj)
                    				&& (objBuffer[index - 1] != obj) && (objBuffer[index] == obj)) {
                    			dir = 3;
                    			y = y + 1;
                    			index = index + paddedYDim;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else {
                    			MipavUtil.displayError("No boundary found for dir == 0 at x = " + x + " y = " + y);
                    			setCompleted(false);
                    			return;
                    		}
                    	} // if (dir == 0) 
                    	else if (dir == 1) { // up
                    		if ((objBuffer[index - 1 - paddedYDim] == obj) && (objBuffer[index - paddedYDim] != obj)
                    				&& (objBuffer[index - 1] == obj) && (objBuffer[index] != obj)) {
                    			dir = 1;
                    			y = y - 1;
                    			index = index - paddedYDim;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else if ((objBuffer[index - 1 - paddedYDim] != obj)
                    				&& (objBuffer[index - 1] == obj) && (objBuffer[index] != obj)) {
                    			dir = 2;
                    			x = x - 1;
                    			index = index - 1;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else if ((objBuffer[index - 1 - paddedYDim] == obj) && (objBuffer[index - paddedYDim] == obj)
                    				&& (objBuffer[index - 1] == obj) && (objBuffer[index] != obj)) {
                    			dir = 0;
                    			x = x + 1;
                    			index = index + 1;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else {
                    			MipavUtil.displayError("No boundary found for dir == 1 at x = " + x + " y = " + y);
                    			setCompleted(false);
                    			return;
                    		}
                    	} // else if (dir == 1)
                    	else if (dir == 2) { // left)
                    		if ((objBuffer[index - 1 - paddedYDim] != obj) && (objBuffer[index - paddedYDim] != obj)
                    				&& (objBuffer[index - 1] == obj) && (objBuffer[index] == obj)) {	
                    			dir = 2;
                    			x = x - 1;
                    			index = index - 1;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else if ((objBuffer[index - 1 - paddedYDim] == obj) && (objBuffer[index - paddedYDim] != obj)
                    				&& (objBuffer[index - 1] == obj) && (objBuffer[index] == obj)) {
                    			dir = 1;
                    			y = y - 1;
                    			index = index - paddedYDim;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else if ((objBuffer[index - paddedYDim] != obj)
                    				&& (objBuffer[index - 1] != obj) && (objBuffer[index] == obj)) {
                    			dir = 3;
                    			y = y + 1;
                    			index = index + paddedYDim;
                    			boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    		}
                    		else {
                    			MipavUtil.displayError("No boundary found for dir == 2 at x = " + x + " y = " + y);
                    			setCompleted(false);
                    			return;
                    		}		
                    	} // else if (dir == 2)
                    	else { // dir == 3 // down
                    		if ((objBuffer[index - 1 - paddedYDim] != obj) && (objBuffer[index - paddedYDim] == obj)
                    				&& (objBuffer[index - 1] != obj) && (objBuffer[index] == obj)) {	
                    			dir = 3;
                    			y = y + 1;
                    			index = index + paddedYDim;
                    			boundaryBuffer[numBoundaryPixels++] = x - 1 + (y-1)*xDim;
                    		}
                    		else if ((objBuffer[index - 1 - paddedYDim] != obj) && (objBuffer[index - paddedYDim] == obj)
                    				&& (objBuffer[index - 1] == obj) && (objBuffer[index] == obj)) {
                    			dir = 2;
                    			x = x - 1;
                    			index = index - 1;
                    			boundaryBuffer[numBoundaryPixels++] = x - 1 + (y-1)*xDim;
                    		}
                    		else if ((objBuffer[index - 1 - paddedYDim] != obj) && (objBuffer[index - paddedYDim] == obj)
                    			     && (objBuffer[index] != obj)) {
                    			dir = 0;
                    			x = x + 1;
                    			index = index + 1;
                    			boundaryBuffer[numBoundaryPixels++] = x - 1 + (y-1)*xDim;
                    		}
                    		else {
                    			MipavUtil.displayError("No boundary found for dir == 3 at x = " + x + " y = " + y);
                    			setCompleted(false);
                    			return;
                    		}		
                    	} // else dir == 3
                    } // while ((numBoundaryPixels < 4) || (boundaryBuffer[0] != boundaryBuffer[numBoundaryPixels-2]) ||
                    // Zero is the same as numBoundaryPixels-2 and one is the same as numBoundaryPixels-1
                
                numBoundaryPixels = numBoundaryPixels-2;
                
                voiID = (short)(obj - 1);
                addedVOI = new VOI(voiID, "VOI" + voiID, VOI.CONTOUR, -1.0f);
                xArrInt = new int[numBoundaryPixels];
                yArrInt = new int[numBoundaryPixels];
                zArrInt = new int[numBoundaryPixels];
                for (i = 0; i < numBoundaryPixels; i++) {
                	xArrInt[i] = boundaryBuffer[i] % xDim;
                	yArrInt[i] = boundaryBuffer[i] / xDim;
                	zArrInt[i] = z;
                }
                addedVOI.importCurve(xArrInt, yArrInt, zArrInt);
                ((VOIContour) (addedVOI.getCurves().lastElement()))
                .trimPoints(Preferences.getTrimMask(), Preferences.getTrimAdjacient());
                if (colorTable != null) {
                    addedVOI.setColor(colorTable[obj - 1]);
                }

               

                VOIs.add(addedVOI);
                
                if (nameTable != null) {
                    addedVOI.setName(nameTable[obj - 1]);
                }
                } // if (found)
            } // for (short obj = 1; obj < (srcImage.getMax() + 1); obj++)
            
        } // for (z = 0; z < zDim; z++)
        
        if (nameTable != null) {

            // Reorder the vois to match the order in nameTable
            VOIVector VOIs2 = new VOIVector();
            int nVOI = VOIs.size();
            String name;

            for (i = 0; i < nVOI; i++) {
                name = nameTable[i];
                found = false;

                for (j = 0; (j < nVOI) && !found; j++) {

                    if (VOIs.VOIAt(j).getName().equals(name)) {
                        found = true;
                        VOIs2.addElement(VOIs.VOIAt(j));
                    }
                }
            }

            srcImage.setVOIs(VOIs2);

        } // if (nameTable != null)

        srcImage.releaseLock();

        setCompleted(true);

    }
    
    /**
      * From text description in 4th edition Image Processing, Analysis, and Machine Vision International Edition
     * by Milan Sonka, Vaclav Hlavac, and Roger Boyle, Algorithm 6.7 Outer Boundary Tracing, p. 193. 
     */
    @SuppressWarnings("unused")
	private void outerBoundaryTracing() {
    	ViewVOIVector VOIs = null;
    	int length;
    	int paddedLength;
    	int paddedXDim;
    	int paddedYDim;
    	int zDim;
    	int x = 0;
    	int y = 0;
    	int outX;
    	int outY;
    	int outIndex;
    	int z;
    	int i, j;
    	int numBoundaryPixels = 0;
    	int numOuterPixels = 0;
    	int boundaryBuffer[];
    	int outerBuffer[];
    	boolean found;
    	int index = 0;
    	int dir;
    	int numDir;
    	short voiID;
    	int xArrInt[];
    	int yArrInt[];
    	int zArrInt[];
    	int xinc;
    	int yinc;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }

        if ((srcImage.getType() != ModelImage.BOOLEAN) && (srcImage.getType() != ModelImage.UBYTE) &&
                (srcImage.getType() != ModelImage.BYTE) && (srcImage.getType() != ModelImage.USHORT) &&
                (srcImage.getType() != ModelImage.SHORT)) {
            displayError("Source Image must be Boolean, UByte, Byte, UShort, or Short");
            setCompleted(false);

            return;
        }

        VOIs = srcImage.getVOIs();

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        paddedXDim = xDim + 2;
        paddedYDim = yDim + 2;
        paddedLength = paddedXDim * paddedYDim;
        
        if (srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }
        
        try {
            imgBuffer = new short[length];
            objBuffer = new short[paddedLength];
            boundaryBuffer = new int[length];
            outerBuffer = new int[length];
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
            
            
            for (short obj = 1; obj < (srcImage.getMax() + 1); obj++) {
            	
            	for (int m = 0; m < imgBuffer.length; m++) {
                    x = m % xDim;
                    y = m / xDim;
                    if (imgBuffer[m] == obj) {
                        objBuffer[(x+1) + (y+1)*paddedXDim] = obj;
                    } else {
                        objBuffer[(x+1) + (y+1)*paddedXDim] = 0;
                    }
                    
                } // for (int m = 0; m < imgBuffer.length; m++)
                
                found = false;
                xinc = 1;
                yinc = 1;
                for (y = 1; y <= yDim && !found; y+= yinc) {
                	for (x = 1; x <= xDim && !found; x+= xinc) {
                	    index = x + y * paddedXDim;
                	    if (objBuffer[index] == obj) {
                	    	found = true;
                	    	boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                	    	xinc = 0;
                	    	yinc = 0;
                	    }
                	} // for (x = 1; x <= xDim; x++)
                } // for (y = 1; y <= yDim; y++)
                
                if (found) {
                    dir = 3;
                    while ((numBoundaryPixels < 4) || (boundaryBuffer[0] != boundaryBuffer[numBoundaryPixels-2]) ||
                    		(boundaryBuffer[1] != boundaryBuffer[numBoundaryPixels-1])) {
                    	found = false;
                    	dir = (dir + 3) % 4;
                    	numDir = 1;
                    	while (!found) {
                    		if (numDir >= 5) {
                    			MipavUtil.displayError("No boundary found in any of 4 directions at x = " + x + " y = " + y);
                    			setCompleted(false);
                    			return;
                    		}
                    		if (dir == 0) {
                    			if ((x < paddedXDim-1) && (objBuffer[index+1] == obj)) {
                    			    found = true;
                    			    x = x+1;
                    			    index = index + 1;
                    			    boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				if (x < paddedXDim - 1) {
                    					outX = x + 1;
                    					outY = y;
                    					outIndex = index + 1;
                    					if ((outX >= 1) && (outX <= xDim) && (outY >= 1) && (outY <= yDim) &&
                    					    ((numOuterPixels == 0) || (outerBuffer[numOuterPixels-1] != outX-1 + (outY-1)*xDim))) {
                    					    outerBuffer[numOuterPixels++] = outX-1 + (outY-1)*xDim;
                    					}
                    				}
                    				dir = 1;
                    				numDir++;
                    			}
                    		} // if (dir == 0)
                    		else if (dir == 1) {
                    			if ((y >= 1) && (objBuffer[index - paddedYDim] == obj)) {
                    				found = true;
                    				y = y - 1;
                    				index = index - paddedYDim;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				if (y >= 1) {
                    					outX = x;
                    					outY = y - 1;
                    					outIndex = index -paddedYDim;
                    					if ((outX >= 1) && (outX <= xDim) && (outY >= 1) && (outY <= yDim) &&
                    							((numOuterPixels == 0) || (outerBuffer[numOuterPixels-1] != outX-1 + (outY-1)*xDim))) {
                    					    outerBuffer[numOuterPixels++] = outX-1 + (outY-1)*xDim;
                    					}
                    				}
                    				dir = 2;
                    				numDir++;
                    			}
                    		} // else if (dir == 1)
                    		else if (dir == 2) {
                    			if ((x >= 1) && (objBuffer[index - 1] == obj)) {
                    				found = true;
                    				x = x - 1;
                    				index = index - 1;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				if (x >= 1) {
                    					outX = x - 1;
                    					outY = y;
                    					outIndex = index - 1;
                    					if ((outX >= 1) && (outX <= xDim) && (outY >= 1) && (outY <= yDim) &&
                    							((numOuterPixels == 0) || (outerBuffer[numOuterPixels-1] != outX-1 + (outY-1)*xDim))) {
                    					    outerBuffer[numOuterPixels++] = outX-1 + (outY-1)*xDim;
                    					}
                    				}
                    				dir = 3;
                    				numDir++;
                    			}
                    		} // else if (dir == 2)
                    		else if (dir == 3) {
                    			if ((y < paddedYDim - 1) && (objBuffer[index + paddedYDim] == obj)) {
                    				found = true;
                    				y = y + 1;
                    				index = index + paddedYDim;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				if (y < paddedYDim - 1) {
                    					outX = x;
                    					outY = y + 1;
                    					outIndex = index + paddedYDim;
                    					if ((outX >= 1) && (outX <= xDim) && (outY >= 1) && (outY <= yDim) &&
                    							((numOuterPixels == 0) || (outerBuffer[numOuterPixels-1] != outX-1 + (outY-1)*xDim))) {
                        					    outerBuffer[numOuterPixels++] = outX-1 + (outY-1)*xDim;
                        					}
                    				}
                    				dir = 0;
                    				numDir++;
                    			}
                    		} // else if (dir == 3)
                    	} // while (!found)
                    } // while ((numBoundaryPixels < 4) || (boundaryBuffer[0] != boundaryBuffer[numBoundaryPixels-2]) ||
                    // Zero is the same as numBoundaryPixels-2 and one is the same as numBoundaryPixels-1
                
                numBoundaryPixels = numBoundaryPixels-2;
                
                voiID = (short)(obj - 1);
                addedVOI = new VOI(voiID, "VOI" + voiID, VOI.CONTOUR, -1.0f);
                xArrInt = new int[numOuterPixels];
                yArrInt = new int[numOuterPixels];
                zArrInt = new int[numOuterPixels];
                for (i = 0; i < numOuterPixels; i++) {
                	xArrInt[i] = outerBuffer[i] % xDim;
                	yArrInt[i] = outerBuffer[i] / xDim;
                	zArrInt[i] = z;
                }
                addedVOI.importCurve(xArrInt, yArrInt, zArrInt);
                ((VOIContour) (addedVOI.getCurves().lastElement()))
                .trimPoints(Preferences.getTrimMask(), Preferences.getTrimAdjacient());
                if (colorTable != null) {
                    addedVOI.setColor(colorTable[obj - 1]);
                }

               

                VOIs.add(addedVOI);
                
                if (nameTable != null) {
                    addedVOI.setName(nameTable[obj - 1]);
                }
                } // if (found)
            } // for (short obj = 1; obj < (srcImage.getMax() + 1); obj++)
        } // for (z = 0; z < zDim; z++)
        
        if (nameTable != null) {

            // Reorder the vois to match the order in nameTable
            VOIVector VOIs2 = new VOIVector();
            int nVOI = VOIs.size();
            String name;

            for (i = 0; i < nVOI; i++) {
                name = nameTable[i];
                found = false;

                for (j = 0; (j < nVOI) && !found; j++) {

                    if (VOIs.VOIAt(j).getName().equals(name)) {
                        found = true;
                        VOIs2.addElement(VOIs.VOIAt(j));
                    }
                }
            }

            srcImage.setVOIs(VOIs2);

        } // if (nameTable != null)

        srcImage.releaseLock();

        setCompleted(true);

    }
    
    /**
     * From text description in 4th edition Image Processing, Analysis, and Machine Vision International Edition
     * by Milan Sonka, Vaclav Hlavac, and Roger Boyle, Algorithm 6.6 Inner Boundary Tracing, pp. 191-192.
     * "Algorithm 6.6 works for all regions larger than one pixel.  It is able to find region borders but does
     * not find borders of region holes, which may be located by identifying border elements unallocated thus
     * far to borders.  The search for border elements always starts after a traced border is closed, and the
     * search for "unused" border elements can continue in the same way as the search for the first border element.
     * Note that if objects are of unit width, more conditions must be added."
     * 
     * Can be used with either connected = 4 or connected = 8.  Much less memory than calcInPlace.
     */
    @SuppressWarnings("unused")
	private void calcInPlace2() {
    	ViewVOIVector VOIs = null;
    	int length;
    	int paddedLength;
    	int paddedXDim;
    	int paddedYDim;
    	int zDim;
    	int x = 0;
    	int y = 0;
    	int z;
    	int i, j;
    	int numBoundaryPixels = 0;
    	int boundaryBuffer[];
    	boolean found;
    	int index = 0;
    	int dir;
    	int numDir;
    	short voiID;
    	int xArrInt[];
    	int yArrInt[];
    	int zArrInt[];
    	int xinc;
    	int yinc;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }

        if ((srcImage.getType() != ModelImage.BOOLEAN) && (srcImage.getType() != ModelImage.UBYTE) &&
                (srcImage.getType() != ModelImage.BYTE) && (srcImage.getType() != ModelImage.USHORT) &&
                (srcImage.getType() != ModelImage.SHORT)) {
            displayError("Source Image must be Boolean, UByte, Byte, UShort, or Short");
            setCompleted(false);

            return;
        }

        VOIs = srcImage.getVOIs();

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        paddedXDim = xDim + 2;
        paddedYDim = yDim + 2;
        paddedLength = paddedXDim * paddedYDim;
        
        if (srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }
        
        try {
            imgBuffer = new short[length];
            objBuffer = new short[paddedLength];
            boundaryBuffer = new int[length];
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
            
            
            for (short obj = 1; obj < (srcImage.getMax() + 1); obj++) {

                for (int m = 0; m < imgBuffer.length; m++) {
                    x = m % xDim;
                    y = m / xDim;
                    if (imgBuffer[m] == obj) {
                        objBuffer[(x+1) + (y+1)*paddedXDim] = obj;
                    } else {
                        objBuffer[(x+1) + (y+1)*paddedXDim] = 0;
                    }
                    
                } // for (int m = 0; m < imgBuffer.length; m++)
                
                found = false;
                xinc = 1;
                yinc = 1;
                for (y = 1; y <= yDim && !found; y += yinc) {
                	for (x = 1; x <= xDim && !found; x += xinc) {
                	    index = x + y * paddedXDim;
                	    if (objBuffer[index] == obj) {
                	    	found = true;
                	    	boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                	    	xinc = 0;
                	    	yinc = 0;
                	    }
                	} // for (x = 1; x <= xDim; x+= xinc)
                } // for (y = 1; y <= yDim; y+= yinc)
                
                if (found) {
                if (connected == 4) {
                    dir = 3;
                    while ((numBoundaryPixels < 4) || (boundaryBuffer[0] != boundaryBuffer[numBoundaryPixels-2]) ||
                    		(boundaryBuffer[1] != boundaryBuffer[numBoundaryPixels-1])) {
                    	found = false;
                    	dir = (dir + 3) % 4;
                    	numDir = 1;
                    	while (!found) {
                    		if (numDir >= 5) {
                    			MipavUtil.displayError("No boundary found in any of 4 directions at x = " + x + " y = " + y);
                    			setCompleted(false);
                    			return;
                    		}
                    		if (dir == 0) {
                    			if ((x < paddedXDim - 1) && (objBuffer[index+1] == obj)) {
                    			    found = true;
                    			    x = x+1;
                    			    index = index + 1;
                    			    boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 1;
                    				numDir++;
                    			}
                    		} // if (dir == 0)
                    		else if (dir == 1) {
                    			if ((y >= 1) && (objBuffer[index - paddedYDim] == obj)) {
                    				found = true;
                    				y = y - 1;
                    				index = index - paddedYDim;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 2;
                    				numDir++;
                    			}
                    		} // else if (dir == 1)
                    		else if (dir == 2) {
                    			if ((x >= 1) && (objBuffer[index - 1] == obj)) {
                    				found = true;
                    				x = x - 1;
                    				index = index - 1;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 3;
                    				numDir++;
                    			}
                    		} // else if (dir == 2)
                    		else if (dir == 3) {
                    			if ((y < paddedYDim - 1) && (objBuffer[index + paddedYDim] == obj)) {
                    				found = true;
                    				y = y + 1;
                    				index = index + paddedYDim;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 0;
                    				numDir++;
                    			}
                    		} // else if (dir == 3)
                    	} // while (!found)
                    } // while ((numBoundaryPixels < 4) || (boundaryBuffer[0] != boundaryBuffer[numBoundaryPixels-2]) ||
                    // Zero is the same as numBoundaryPixels-2 and one is the same as numBoundaryPixels-1
                } // if (connected == 4)
                else if (connected == 8) {
                	dir = 7;
                    while ((numBoundaryPixels < 4) || (boundaryBuffer[0] != boundaryBuffer[numBoundaryPixels-2]) ||
                    		(boundaryBuffer[1] != boundaryBuffer[numBoundaryPixels-1])) {
                    	found = false;	
                    	if ((dir % 2) == 0) { // dir is even
                    		dir = (dir + 7) % 8;
                    	}
                    	else {
                    		dir = (dir + 6) % 8;
                    	}
                    	numDir = 1;
                    	while (!found) {
                    		if (numDir >= 9) {
                    			MipavUtil.displayError("No boundary found in any of 8 directions at x = " + x + " y = " + y);
                    			setCompleted(false);
                    			return;
                    		}
                    		if (dir == 0) {
                    			if ((x < paddedXDim-1) && (objBuffer[index+1] == obj)) {
                    			    found = true;
                    			    x = x+1;
                    			    index = index + 1;
                    			    boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 1;
                    				numDir++;
                    			}
                    		} // if (dir == 0)
                    		else if (dir == 1) {
                    			if ((x < paddedXDim - 1) && (y >= 1) && (objBuffer[index - paddedYDim + 1] == obj)) {
                    				found = true;
                    				x = x + 1;
                    				y = y - 1;
                    				index = index - paddedYDim + 1;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 2;
                    				numDir++;
                    			}
                    		} // else if (dir == 1)
                    		else if (dir == 2) {
                    			if ((y >= 1) && (objBuffer[index - paddedYDim] == obj)) {
                    				found = true;
                    				y = y - 1;
                    				index = index - paddedYDim;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 3;
                    				numDir++;
                    			}
                    		} // else if (dir == 2)
                    		else if (dir == 3) {
                    			if ((x >= 1) && (y >= 1) && (objBuffer[index - paddedYDim - 1] == obj)) {
                    				found = true;
                    				x = x - 1;
                    				y = y - 1;
                    				index = index - paddedYDim - 1;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 4;
                    				numDir++;
                    			}
                    		} // else if (dir == 3)
                    		else if (dir == 4) {
                    			if ((x >= 1) && (objBuffer[index - 1] == obj)) {
                    				found = true;
                    				x = x - 1;
                    				index = index - 1;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 5;
                    				numDir++;
                    			}
                    		} // else if (dir == 4)
                    		else if (dir == 5) {
                    			if ((x >= 1) && (y < paddedYDim - 1) && (objBuffer[index + paddedYDim - 1] == obj)) {
                    				found = true;
                    				x = x - 1;
                    				y = y + 1;
                    				index = index + paddedYDim - 1;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 6;
                    				numDir++;
                    			}
                    		} // else if (dir == 5)
                    		else if (dir == 6) {
                    			if ((y < paddedYDim - 1) && (objBuffer[index + paddedYDim] == obj)) {
                    				found = true;
                    				y = y + 1;
                    				index = index + paddedYDim;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 7;
                    				numDir++;
                    			}
                    		} // else if (dir == 6)
                    		else if (dir == 7) {
                    			if ((x < paddedXDim - 1) && (y < paddedYDim - 1) && (objBuffer[index + paddedYDim + 1] == obj)) {
                    				found = true;
                    				x = x + 1;
                    				y = y + 1;
                    				index = index + paddedYDim + 1;
                    				boundaryBuffer[numBoundaryPixels++] = x-1 + (y-1)*xDim;
                    			}
                    			else {
                    				dir = 0;
                    				numDir++;
                    			}
                    		} // else if (dir == 7)
                    	} // while (!found)
                    } // while ((numBoundaryPixels < 4) || (boundaryBuffer[0] != boundaryBuffer[numBoundaryPixels-2]) ||
                } // else if (connected == 8)
                numBoundaryPixels = numBoundaryPixels-2;
                
                voiID = (short)(obj - 1);
                addedVOI = new VOI(voiID, "VOI" + voiID, VOI.CONTOUR, -1.0f);
                xArrInt = new int[numBoundaryPixels];
                yArrInt = new int[numBoundaryPixels];
                zArrInt = new int[numBoundaryPixels];
                for (i = 0; i < numBoundaryPixels; i++) {
                	xArrInt[i] = boundaryBuffer[i] % xDim;
                	yArrInt[i] = boundaryBuffer[i] / xDim;
                	zArrInt[i] = z;
                }
                addedVOI.importCurve(xArrInt, yArrInt, zArrInt);
                ((VOIContour) (addedVOI.getCurves().lastElement()))
                .trimPoints(Preferences.getTrimMask(), Preferences.getTrimAdjacient());
                if (colorTable != null) {
                    addedVOI.setColor(colorTable[obj - 1]);
                }

               

                VOIs.add(addedVOI);
                
                if (nameTable != null) {
                    addedVOI.setName(nameTable[obj - 1]);
                }
                } // if (found)
            } // for (short obj = 1; obj < (srcImage.getMax() + 1); obj++)
            
        } // for (z = 0; z < zDim; z++)
        
        if (nameTable != null) {

            // Reorder the vois to match the order in nameTable
            VOIVector VOIs2 = new VOIVector();
            int nVOI = VOIs.size();
            String name;

            for (i = 0; i < nVOI; i++) {
                name = nameTable[i];
                found = false;

                for (j = 0; (j < nVOI) && !found; j++) {

                    if (VOIs.VOIAt(j).getName().equals(name)) {
                        found = true;
                        VOIs2.addElement(VOIs.VOIAt(j));
                    }
                }
            }

            srcImage.setVOIs(VOIs2);

        } // if (nameTable != null)

        srcImage.releaseLock();

        setCompleted(true);

    }

    /**
     * Calculates the VOI extraction.
     */
    private void calcInPlace() {
        int x, y, z, zDim;
        int i, j;
        int scanPos;
        int smallLength, smallPos, smallX, smallY;
        short grayScaleNumber = 0;
        int nCurves;
        Polygon contourPolygon;
        boolean newGrayScale;
        ViewVOIVector VOIs = null;
        int xpos;
        int ypos;
        int index;

        if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }

        if ((srcImage.getType() != ModelImage.BOOLEAN) && (srcImage.getType() != ModelImage.UBYTE) &&
                (srcImage.getType() != ModelImage.BYTE) && (srcImage.getType() != ModelImage.USHORT) &&
                (srcImage.getType() != ModelImage.SHORT)) {
            displayError("Source Image must be Boolean, UByte, Byte, UShort, or Short");
            setCompleted(false);

            return;
        }

        VOIs = srcImage.getVOIs();

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        smallLength = xDim * yDim;

        // Must form 4 copies of every xy pixel or algorithm will not properly handle
        // one pixel wide passages into the object
        xDimE = 2 * xDim;
        yDimE = 2 * yDim;

        int length = xDimE * yDimE;
        int expandedLength = (xDimE + 2)*(yDimE + 2);

        if (srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }

        try {
            imgBuffer = new short[smallLength];
            expImgBuffer = new short[length];
            mask = new BitSet(smallLength);
            objBuffer = new short[smallLength];
            outMask = new BitSet(smallLength);
            mask2 = new BitSet(length);
            maskAll = new BitSet(length);
            maskExpanded = new BitSet(expandedLength);
            maskList = new short[smallLength][2];
            nextMaskList = new short[smallLength][2];
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
                srcImage.exportDataNoLock(z * xDim * yDim, xDim * yDim, imgBuffer);
            } catch (IOException error) {
                displayError("Algorithm VOI Extraction: image bounds exceeded");
                setCompleted(false);
                
                srcImage.releaseLock();

                return;
            }

            for (short obj = 1; obj < (srcImage.getMax() + 1); obj++) {
                
                for (int m = 0; m < imgBuffer.length; m++) {

                    if (imgBuffer[m] == obj) {
                        objBuffer[m] = obj;
                    } else {
                        objBuffer[m] = 0;
                    }
                }

                mask.clear();
                outMask.clear();
                mask2.clear();
                maskAll.clear();
                maskExpanded.clear();

                short val = 0;
                int xTmpIdx;
                int yTmpIdx;

                for (y = 0; (y < yDim) && !threadStopped; y++) {

                    for (x = 0; (x < xDim) && !threadStopped; x++) {
                        val = objBuffer[x + (y * xDim)];
                        xTmpIdx = 2 * x;
                        yTmpIdx = 4 * y * xDim;
                        expImgBuffer[xTmpIdx + yTmpIdx] = val;
                        expImgBuffer[xTmpIdx + 1 + yTmpIdx] = val;
                        expImgBuffer[xTmpIdx + yTmpIdx + (2 * xDim)] = val;
                        expImgBuffer[xTmpIdx + 1 + yTmpIdx + (2 * xDim)] = val;
                    }
                }

                for (y = 0; (y < yDimE) && !threadStopped; y++) {

                    for (x = 0; (x < xDimE) && !threadStopped; x++) {
                        scanPos = x + (y * xDimE);
                        smallX = x / 2;
                        smallY = y / 2;
                        smallPos = smallX + (smallY * xDim);

                        if ((!mask.get(smallPos)) && (objBuffer[smallPos] == obj)) {
                            short voiID = (short) (objBuffer[smallPos] - 1);

                            // have found a new contour
                            // Since genContour works on finding a four-connected boundary using
                            // Papert's turtle algorithm, the routine to grow the region must
                            // also use four connected neighbors.
                            // Set mask of all points in 2D region
                            setRegionMaskI(smallX, smallY, objBuffer[smallPos]);

                            // Expand mask points so as to draw boundary properly.
                            // A pixel point only refers to left upper corner
                            setRegionMaskAll();
                            
                            for (ypos = 0; ypos < yDimE; ypos++) {
                                for (xpos = 0; xpos < xDimE; xpos++) {
                                    index = xpos + ypos * xDimE;
                                    if (maskAll.get(index)) {
                                        if ((xpos == xDimE - 1) && (ypos == yDimE - 1)) {
                                            maskExpanded.set(xpos + 1 + ypos * (xDimE + 2));  
                                            maskExpanded.set(xpos + 2 + ypos * (xDimE + 2));
                                            maskExpanded.set(xpos + (ypos+1)*(xDimE + 2));
                                            maskExpanded.set(xpos + (ypos+2)*(xDimE + 2));
                                            maskExpanded.set(xpos + 1 + (ypos+1) * (xDimE + 2));
                                            maskExpanded.set(xpos + 2 + (ypos+1) * (xDimE + 2));    
                                            maskExpanded.set(xpos + 1 + (ypos+2) * (xDimE + 2));    
                                            maskExpanded.set(xpos + 2 + (ypos+2) * (xDimE + 2)); 
                                            maskExpanded.set(xpos + ypos * (xDimE + 2));
                                        }
                                        else if (xpos == xDimE - 1) {
                                            maskExpanded.set(xpos + 1 + ypos * (xDimE + 2));  
                                            maskExpanded.set(xpos + 2 + ypos * (xDimE + 2)); 
                                            maskExpanded.set(xpos + ypos * (xDimE + 2));
                                        }
                                        else if (ypos == yDimE - 1) {
                                            maskExpanded.set(xpos + (ypos+1)*(xDimE + 2));
                                            maskExpanded.set(xpos + (ypos+2)*(xDimE + 2));
                                            maskExpanded.set(xpos + ypos * (xDimE + 2));
                                        }
                                        else {
                                            maskExpanded.set(xpos + ypos * (xDimE + 2));
                                        }
                                        
                                    }
                                }
                            }

                            // Return polygon of contour
                            Point startPt = new Point(x, y);

                            contourPolygon = AlgorithmMorphology2D.genContour(xDimE+2, yDimE+2, startPt, maskExpanded);
                            maskAll.clear();
                            maskExpanded.clear();
                            newGrayScale = true;

                            for (i = 0; ((newGrayScale) && (i < grayScaleNumber)); i++) {

                                if (grayScaleArray[i] == expImgBuffer[scanPos]) {
                                    newGrayScale = false;

                                    // add the polygon to an existing VOI
                                    VOIs.VOIAt(VOIIndexArray[i]).importPolygon(contourPolygon, z);
                                    ((VOIContour) (VOIs.VOIAt(VOIIndexArray[i]).getCurves().lastElement()))
                                        .trimPoints(Preferences.getTrimMask(), Preferences.getTrimAdjacient());
                                }
                            }

                            if (newGrayScale) {
                                grayScaleArray[grayScaleNumber] = expImgBuffer[scanPos];
                                addedVOI = new VOI(voiID, "VOI" + voiID, VOI.CONTOUR, -1.0f);

                                if (colorTable != null) {
                                    addedVOI.setColor(colorTable[expImgBuffer[scanPos] - 1]);
                                }

                               

                                VOIs.add(addedVOI);
                                numVOIAdded++;
                                VOIIndexArray[grayScaleNumber] = VOIs.size() - 1;
                                VOIs.VOIAt(VOIIndexArray[grayScaleNumber]).importPolygon(contourPolygon, z);
                                ((VOIContour) (VOIs.VOIAt(VOIIndexArray[grayScaleNumber]).getCurves().lastElement()))
                                    .trimPoints(Preferences.getTrimMask(), Preferences.getTrimAdjacient());
                                grayScaleNumber++;

                                if (nameTable != null) {
                                    addedVOI.setName(nameTable[expImgBuffer[scanPos] - 1]);
                                }
                                
                                if (grayScaleNumber >= 10000) {
                                    displayError("Algorithm VOI Extraction: Impossibly high >= 10000 gray scales detected");
                                    setCompleted(false);
                                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

                                    return;

                                } // end of if (grayScaleNumber >= 1000)
                            } // end of if (newGrayScale)
                            contourPolygon.reset();
                        } // end of if ((!mask.get(smallPos)) && (objBuffer[smallPos] == obj))
                    } // end of for (x = 0; x < xDimE; x++)
                } // end of for (y = 0; y < yDimE; y++)

                // Find all zero points 4 connected by other zero points to a zero point on the boundary
                outMask.clear();
                maskAll.clear();
                maskExpanded.clear();

                smallY = 0;

                for (smallX = 0; smallX < xDim; smallX++) {
                    smallPos = smallX + (smallY * xDim);

                    if ((!outMask.get(smallPos)) && (objBuffer[smallPos] == 0)) {
                        setRegionOutMaskI(smallX, smallY, (short) 0);
                    }
                }

                smallY = yDim - 1;

                for (smallX = 0; smallX < xDim; smallX++) {
                    smallPos = smallX + (smallY * xDim);

                    if ((!outMask.get(smallPos)) && (objBuffer[smallPos] == 0)) {
                        setRegionOutMaskI(smallX, smallY, (short) 0);
                    }
                }

                smallX = 0;

                for (smallY = 1; smallY < (yDim - 1); smallY++) {
                    smallPos = smallX + (smallY * xDim);

                    if ((!outMask.get(smallPos)) && (objBuffer[smallPos] == 0)) {
                        setRegionOutMaskI(smallX, smallY, (short) 0);
                    }
                }

                smallX = xDim - 1;

                for (smallY = 1; smallY < (yDim - 1); smallY++) {
                    smallPos = smallX + (smallY * xDim);

                    if ((!outMask.get(smallPos)) && (objBuffer[smallPos] == 0)) {
                        setRegionOutMaskI(smallX, smallY, (short) 0);
                    }
                }

                // Will find holes (holes = background) interior to objects
                short voiID;

                // = (short) Math.min(31000, srcImage.getMax());
                for (y = 0; (y < yDimE) && !threadStopped; y++) {

                    for (x = 0; (x < xDimE) && !threadStopped; x++) {
                        scanPos = x + (y * xDimE);
                        smallX = x / 2;
                        smallY = y / 2;
                        smallPos = smallX + (smallY * xDim);

                        if ((!outMask.get(smallPos)) && (!mask.get(smallPos)) && (objBuffer[smallPos] == 0)) {

                            // System.out.println("This is = " + voiID);
                            // have found a new contour
                            // Since genContour works on finding a four-connected boundary using
                            // Papert's turtle algorithm, the routine to grow the region must
                            // also use four connected neighbors.
                            // Set mask of all points in 2D region
                            setRegionMaskI(smallX, smallY, (short) 0);

                            // Expand mask points so as to draw boundary properly.
                            // A pixel point only refers to left upper corner
                            setRegionMaskAll();
                            
                            for (ypos = 0; ypos < yDimE; ypos++) {
                                for (xpos = 0; xpos < xDimE; xpos++) {
                                    index = xpos + ypos * xDimE;
                                    if (maskAll.get(index)) {
                                        if ((xpos == xDimE - 1) && (ypos == yDimE - 1)) {
                                            maskExpanded.set(xpos + 1 + ypos * (xDimE + 2));  
                                            maskExpanded.set(xpos + 2 + ypos * (xDimE + 2));
                                            maskExpanded.set(xpos + (ypos+1)*(xDimE + 2));
                                            maskExpanded.set(xpos + (ypos+2)*(xDimE + 2));
                                            maskExpanded.set(xpos + 1 + (ypos+1) * (xDimE + 2));
                                            maskExpanded.set(xpos + 2 + (ypos+1) * (xDimE + 2));    
                                            maskExpanded.set(xpos + 1 + (ypos+2) * (xDimE + 2));    
                                            maskExpanded.set(xpos + 2 + (ypos+2) * (xDimE + 2)); 
                                            maskExpanded.set(xpos + ypos * (xDimE + 2));
                                        }
                                        else if (xpos == xDimE - 1) {
                                            maskExpanded.set(xpos + 1 + ypos * (xDimE + 2));  
                                            maskExpanded.set(xpos + 2 + ypos * (xDimE + 2)); 
                                            maskExpanded.set(xpos + ypos * (xDimE + 2));
                                        }
                                        else if (ypos == yDimE - 1) {
                                            maskExpanded.set(xpos + (ypos+1)*(xDimE + 2));
                                            maskExpanded.set(xpos + (ypos+2)*(xDimE + 2));
                                            maskExpanded.set(xpos + ypos * (xDimE + 2));
                                        }
                                        else {
                                            maskExpanded.set(xpos + ypos * (xDimE + 2));
                                        }
                                        
                                    }
                                }
                            }

                            // Return polygon of contour
                            Point startPt = new Point(x, y);

                            contourPolygon = AlgorithmMorphology2D.genContour(xDimE+2, yDimE+2, startPt, maskExpanded);

                            maskAll.clear();
                            maskExpanded.clear();
                            newGrayScale = true;
                            // If inside a curve of an existing VOI, make part of this VOI

                            for (i = 0; ((newGrayScale) && (i < grayScaleNumber)); i++) {
                                voiID = (short) (VOIs.VOIAt(VOIIndexArray[i]).getID() + 1);
                                nCurves = VOIs.VOIAt(VOIIndexArray[i]).getCurves().size();

                                for (j = 0; j < nCurves; j++) {

                                    if (((VOIContour) (VOIs.VOIAt(VOIIndexArray[i]).getCurves().elementAt(j)))
                                            .contains(x / 2, y / 2) && (voiID == obj)) {
                                        newGrayScale = false;

                                        // add the polygon to an existing VOI
                                        VOIs.VOIAt(VOIIndexArray[i]).importPolygon(contourPolygon, z);
                                        ((VOIContour) (VOIs.VOIAt(VOIIndexArray[i]).getCurves().lastElement()))
                                            .trimPoints(Preferences.getTrimMask(), Preferences.getTrimAdjacient());
                                    }
                                }
                            }
                            contourPolygon.reset();
                        } // end of if ((!outMask.get(smallPos)) && (!mask.get(smallPos)) && (objBuffer[smallPos] == 0))
                    } // end of for (x = 0; x < xDimE; x++)
                } // end of for (y = 0; y < yDimE; y++)

                 
            } // for (short obj = 1; obj < (srcImage.getMax() + 1); obj++)
            fireProgressStateChanged(Math.round((z + 1) * 100.0f / zDim));
        } // end of for (z = 0; z < zDim; z++)

        if (threadStopped) {
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
            setCompleted(false);

            return;
        }
/*
        for (i = 0; i < grayScaleNumber; i++) {
            curves = VOIs.VOIAt(VOIIndexArray[i]).getCurvesTemp();

            for (z = 0; z < zDim; z++) {
                nCurves = curves[z].size();

                for (j = 0; j < nCurves; j++) { // Remove only collinear points with parameter = 0.10

                    // ((VOIContour)(curves[z].elementAt(j))).trimPoints(0.00);
                }
            } // end of for (z = 0; z < zDim; z++)
        } // end of for (i = 0; i < grayScaleNumber; i++)
*/
        if (nameTable != null) {

            // Reorder the vois to match the order in nameTable
            VOIVector VOIs2 = new VOIVector();
            int nVOI = VOIs.size();
            String name;
            boolean found;

            for (i = 0; i < nVOI; i++) {
                name = nameTable[i];
                found = false;

                for (j = 0; (j < nVOI) && !found; j++) {

                    if (VOIs.VOIAt(j).getName().equals(name)) {
                        found = true;
                        VOIs2.addElement(VOIs.VOIAt(j));
                    }
                }
            }

            srcImage.setVOIs(VOIs2);

        } // if (nameTable != null)

        srcImage.releaseLock();

        setCompleted(true);
    }
    
   

    /**
     * Sets the mask of all points in the 4 connected region with a gray scale value.
     */
    private void setRegionMaskAll() {
        int xt, yt, testPos;
        int neighbors;
        boolean north;
        boolean south;
        boolean east;
        boolean west;

        for (yt = 0; yt < yDimE; yt++) {

            for (xt = 0; xt < xDimE; xt++) {
                testPos = xt + (yt * xDimE);
                north = false;
                south = false;
                east = false;
                west = false;

                if (mask2.get(testPos)) {
                    neighbors = 0;

                    if (((xt + 1) < xDimE) && (mask2.get(xt + 1 + (xDimE * yt)))) {
                        east = true;
                        neighbors++;
                    }

                    if (((xt - 1) > 0) && (mask2.get(xt - 1 + (xDimE * yt)))) {
                        west = true;
                        neighbors++;
                    }

                    if (((yt + 1) < yDimE) && (mask2.get(xt + (xDimE * (yt + 1))))) {
                        south = true;
                        neighbors++;
                    }

                    if (((yt - 1) > 0) && (mask2.get(xt + (xDimE * (yt - 1))))) {
                        north = true;
                        neighbors++;
                    }

                    if (neighbors == 2) {

                        if (north && east) {

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else if (north && south) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else if (north && west) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else if (east && south) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }
                        } else if (east && west) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else { // south and west neighbors

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        }
                    } // end of else if (neighbors == 2)
                    else if (neighbors == 3) {

                        if (!north) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }
                        } else if (!east) {

                            if ((xt + 1) < xDimE) {
                                maskAll.set(xt + 1 + (xDimE * yt));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else if (!south) {

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }

                            if (((xt + 1) < xDimE) && ((yt + 1) < yDimE)) {
                                maskAll.set(xt + 1 + (xDimE * (yt + 1)));
                            }
                        } else { // no west neighbor

                            if ((yt + 1) < yDimE) {
                                maskAll.set(xt + (xDimE * (yt + 1)));
                            }
                        }
                    } // end of else if (neighbors == 3)
                } // end of if (mask2.set(testPos)
            } // end of for (x = 0; x < xDimE; x++)
        } // end of for (y = 0; y < yDimE; y++)

        mask2.clear();
    }


    /**
     * Sets the mask of all points in the 4 connected region with a gray scale value of objectValue Uses iteration.
     *
     * @param  xStart       the x coordinate of the starting point
     * @param  yStart       the y coordinate of the starting point
     * @param  objectValue  the gray scale value of the object
     */

    private void setRegionMaskI(int xStart, int yStart, short objectValue) {
        int i, xt, yt;
        int maskNumber;
        int nextMaskNumber;
        int yTmpIdx = 4 * xDim * yStart;
        int xTmpIdx = 2 * xDim;
        int xTmpIdx2 = 2 * xStart;

        mask.set(xStart + (xDim * yStart));
        mask2.set(xTmpIdx2 + yTmpIdx);
        mask2.set(xTmpIdx2 + 1 + yTmpIdx);
        mask2.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
        mask2.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
        maskAll.set(xTmpIdx2 + yTmpIdx);
        maskAll.set(xTmpIdx2 + 1 + yTmpIdx);
        maskAll.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
        maskAll.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
        maskList[0][0] = (short) xStart;
        maskList[0][1] = (short) yStart;

        maskNumber = 1;

        while (maskNumber > 0) {
            nextMaskNumber = 0;

            for (i = 0; i < maskNumber; i++) {
                xt = maskList[i][0];
                yt = maskList[i][1];

                if (((xt + 1) < xDim) && (!mask.get(xt + 1 + (xDim * yt))) &&
                        (objBuffer[xt + 1 + (xDim * yt)] == objectValue)) {
                    nextMaskList[nextMaskNumber][0] = (short) (xt + 1);
                    nextMaskList[nextMaskNumber++][1] = (short) yt;
                    yTmpIdx = 4 * xDim * yt;
                    xTmpIdx2 = 2 * (xt + 1);

                    mask.set((xt + 1) + (xDim * yt));
                    mask2.set(xTmpIdx2 + yTmpIdx);
                    mask2.set(xTmpIdx2 + 1 + yTmpIdx);
                    mask2.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
                    mask2.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
                    maskAll.set(xTmpIdx2 + yTmpIdx);
                    maskAll.set(xTmpIdx2 + 1 + yTmpIdx);
                    maskAll.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
                    maskAll.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
                }

                if (((xt - 1) > 0) && (!mask.get(xt - 1 + (xDim * yt))) &&
                        (objBuffer[xt - 1 + (xDim * yt)] == objectValue)) {
                    nextMaskList[nextMaskNumber][0] = (short) (xt - 1);
                    nextMaskList[nextMaskNumber++][1] = (short) yt;
                    yTmpIdx = 4 * xDim * yt;
                    xTmpIdx2 = 2 * (xt - 1);

                    mask.set((xt - 1) + (xDim * yt));
                    mask2.set(xTmpIdx2 + yTmpIdx);
                    mask2.set(xTmpIdx2 + 1 + yTmpIdx);
                    mask2.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
                    mask2.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
                    maskAll.set(xTmpIdx2 + yTmpIdx);
                    maskAll.set(xTmpIdx2 + 1 + yTmpIdx);
                    maskAll.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
                    maskAll.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
                }

                if (((yt + 1) < yDim) && (!mask.get(xt + (xDim * (yt + 1)))) &&
                        (objBuffer[xt + (xDim * (yt + 1))] == objectValue)) {
                    nextMaskList[nextMaskNumber][0] = (short) xt;
                    nextMaskList[nextMaskNumber++][1] = (short) (yt + 1);
                    yTmpIdx = 4 * xDim * (yt + 1);
                    xTmpIdx2 = 2 * xt;

                    mask.set(xt + (xDim * (yt + 1)));
                    mask2.set(xTmpIdx2 + yTmpIdx);
                    mask2.set(xTmpIdx2 + 1 + yTmpIdx);
                    mask2.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
                    mask2.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
                    maskAll.set(xTmpIdx2 + yTmpIdx);
                    maskAll.set(xTmpIdx2 + 1 + yTmpIdx);
                    maskAll.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
                    maskAll.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
                }

                if (((yt - 1) > 0) && (!mask.get(xt + (xDim * (yt - 1)))) &&
                        (objBuffer[xt + (xDim * (yt - 1))] == objectValue)) {
                    nextMaskList[nextMaskNumber][0] = (short) xt;
                    nextMaskList[nextMaskNumber++][1] = (short) (yt - 1);
                    yTmpIdx = 4 * xDim * (yt - 1);
                    xTmpIdx2 = 2 * xt;

                    mask.set(xt + (xDim * (yt - 1)));
                    mask2.set(xTmpIdx2 + yTmpIdx);
                    mask2.set(xTmpIdx2 + 1 + yTmpIdx);
                    mask2.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
                    mask2.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
                    maskAll.set(xTmpIdx2 + yTmpIdx);
                    maskAll.set(xTmpIdx2 + 1 + yTmpIdx);
                    maskAll.set(xTmpIdx2 + yTmpIdx + xTmpIdx);
                    maskAll.set(xTmpIdx2 + 1 + yTmpIdx + xTmpIdx);
                }
            } // for (i = 0; i < maskNumber; i++)

            for (i = 0; i < nextMaskNumber; i++) {
                maskList[i][0] = nextMaskList[i][0];
                maskList[i][1] = nextMaskList[i][1];
            } // for (i = 0; i < nextMaskNumber; i++)

            maskNumber = nextMaskNumber;
        } // while (maskNumber > 0)
    }

    /**
     * Sets the mask of all points in the 4 connected region with a gray scale value of objectValue Uses iteration.
     *
     * @param  xStart       the x coordinate of the starting point
     * @param  yStart       the y coordinate of the starting point
     * @param  objectValue  the gray scale value of the object
     */

    private void setRegionOutMaskI(int xStart, int yStart, short objectValue) {
        int i, xt, yt;
        int maskNumber;
        int nextMaskNumber;

        outMask.set(xStart + (xDim * yStart));
        maskList[0][0] = (short) xStart;
        maskList[0][1] = (short) yStart;
        maskNumber = 1;

        while (maskNumber > 0) {
            nextMaskNumber = 0;

            for (i = 0; i < maskNumber; i++) {
                xt = maskList[i][0];
                yt = maskList[i][1];

                if (((xt + 1) < xDim) && (!outMask.get(xt + 1 + (xDim * yt))) &&
                        (objBuffer[xt + 1 + (xDim * yt)] == objectValue)) {
                    nextMaskList[nextMaskNumber][0] = (short) (xt + 1);
                    nextMaskList[nextMaskNumber++][1] = (short) yt;
                    outMask.set((xt + 1) + (xDim * yt));
                }

                if (((xt - 1) > 0) && (!outMask.get(xt - 1 + (xDim * yt))) &&
                        (objBuffer[xt - 1 + (xDim * yt)] == objectValue)) {
                    nextMaskList[nextMaskNumber][0] = (short) (xt - 1);
                    nextMaskList[nextMaskNumber++][1] = (short) yt;
                    outMask.set((xt - 1) + (xDim * yt));
                }

                if (((yt + 1) < yDim) && (!outMask.get(xt + (xDim * (yt + 1)))) &&
                        (objBuffer[xt + (xDim * (yt + 1))] == objectValue)) {
                    nextMaskList[nextMaskNumber][0] = (short) xt;
                    nextMaskList[nextMaskNumber++][1] = (short) (yt + 1);
                    outMask.set(xt + (xDim * (yt + 1)));
                }

                if (((yt - 1) > 0) && (!outMask.get(xt + (xDim * (yt - 1)))) &&
                        (objBuffer[xt + (xDim * (yt - 1))] == objectValue)) {
                    nextMaskList[nextMaskNumber][0] = (short) xt;
                    nextMaskList[nextMaskNumber++][1] = (short) (yt - 1);
                    outMask.set(xt + (xDim * (yt - 1)));
                }
            } // for (i = 0; i < maskNumber; i++)

            for (i = 0; i < nextMaskNumber; i++) {
                maskList[i][0] = nextMaskList[i][0];
                maskList[i][1] = nextMaskList[i][1];
            } // for (i = 0; i < nextMaskNumber; i++)

            maskNumber = nextMaskNumber;
        } // while (maskNumber > 0)
    }
    
    /**
     * Retrieve the VOI generated by the mask to voi extraction.
     * @return The generated VOI object.
     */
    public VOI getAddedVOI() {
        return addedVOI;
    }
    
    public int getNumVOIAdded() {
    	return numVOIAdded;
    }
}
