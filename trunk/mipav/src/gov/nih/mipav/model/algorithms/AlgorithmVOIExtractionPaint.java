package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmVOIExtractionPaint extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private VOI addedVOI;

    /** xDim2*yDim2. */
    private int length;

    /** DOCUMENT ME! */
    private BitSet mask = null;

    /** DOCUMENT ME! */
    private BitSet mask2 = null;

    /** DOCUMENT ME! */
    private BitSet maskAll = null;

    /** DOCUMENT ME! */
    private BitSet maskE = null;

    /** DOCUMENT ME! */
    private int[][] maskList = null;

    /** DOCUMENT ME! */
    private BitSet maskN = null;

    /** DOCUMENT ME! */
    private BitSet maskS = null;

    /** DOCUMENT ME! */
    private BitSet maskW = null;

    /** DOCUMENT ME! */
    private int neighbors;

    /** DOCUMENT ME! */
    private int[][] nextMaskList = null;

    /** DOCUMENT ME! */
    private BitSet outMask = null;

    /** xDim * yDim. */
    private int smallLength;

    /** DOCUMENT ME! */
    private int smallPos;

    /** DOCUMENT ME! */
    private int smallPos3D;

    /** DOCUMENT ME! */
    private int smallX;

    /** DOCUMENT ME! */
    private int smallY;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private BitSet srcPaintMask;

    /** x + y*xDim. */
    private int testPos;

    /** DOCUMENT ME! */
    private short voiIDnum = 0;

    /** current x position. */
    private int x;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int xDim2;

    /** DOCUMENT ME! */
    private int xDim4;

    /** DOCUMENT ME! */
    private int xt, yt;

    /** current y position. */
    private int y;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int yDim2;

    /** current z slice. */
    private int z;

    /** extent of z dimension in the in the source image. */
    private int zDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for finding the borders of the paint on various slices of an image.
     *
     * @param  sourceImage  the image that has been painted
     */
    public AlgorithmVOIExtractionPaint(ModelImage sourceImage) {
        super(null, null);

        srcImage = sourceImage;

        xDim = sourceImage.getExtents().length > 0 ? sourceImage.getExtents()[0] : 1;
        yDim = sourceImage.getExtents().length > 1 ? sourceImage.getExtents()[1] : 1;
        zDim = sourceImage.getExtents().length > 2 ? sourceImage.getExtents()[2] : 1;

        xDim2 = 2 * xDim;
        yDim2 = 2 * xDim;
        xDim4 = 4 * xDim;

        smallLength = xDim * yDim;

        // Must form 4 copies of every xy pixel or algorithm will not properly handle
        // one pixel wide passages into the object
        length = xDim2 * yDim2;

        try {
            mask = new BitSet(smallLength);
            outMask = new BitSet(smallLength);
            maskN = new BitSet(length);
            maskE = new BitSet(length);
            maskS = new BitSet(length);
            maskW = new BitSet(length);
            mask2 = new BitSet(length);
            maskAll = new BitSet(length);
            maskList = new int[length][2];
            nextMaskList = new int[length][2];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Algorithm VOIExtractionPaint: Out of memory");
        }
    }

    /**
     * Construct the extractor for paint extraction to a VOI.
     *
     * @param  sourceImage  the image that has been painted
     * @param  srcPaint     the paint bitmap
     * @param  xDimen       the x dimension of the image
     * @param  yDimen       the y dimension of the image
     * @param  zDimen       the z dimension of the image
     * @param  voiID        the VOI number to give to the generated VOI
     */
    public AlgorithmVOIExtractionPaint(ModelImage sourceImage, BitSet srcPaint, int xDimen, int yDimen, int zDimen,
                                       short voiID) {
        super(null, null);

        srcPaintMask = srcPaint;
        srcImage = sourceImage;
        voiIDnum = voiID;

        xDim = xDimen;
        yDim = yDimen;
        zDim = zDimen;

        xDim2 = 2 * xDim;
        yDim2 = 2 * yDim;
        xDim4 = 4 * xDim;

        smallLength = xDim * yDim;

        // Must form 4 copies of every xy pixel or algorithm will not properly handle
        // one pixel wide passages into the object
        length = xDim2 * yDim2;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        int i;

        mask = null;
        outMask = null;
        mask2 = null;
        maskAll = null;
        maskN = null;
        maskE = null;
        maskS = null;
        maskW = null;

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

        super.finalize();
    }

    /**
     * Finds a set of lines which surround the paint in the image.
     *
     * @param   slice  the slice to find the paint borders on
     *
     * @return  DOCUMENT ME!
     */
    public Vector<Polygon> findPaintBorder(int slice) {
        Vector<Polygon> paintBorders = new Vector<Polygon>();

        int offset;

        if (slice < 0) {
            return paintBorders;
        }

        mask.clear();
        outMask.clear();
        maskN.clear();
        maskE.clear();
        maskS.clear();
        maskW.clear();
        mask2.clear();
        maskAll.clear();

        z = slice;
        offset = z * xDim * yDim;

        for (y = 0; y < yDim2; y++) {

            for (x = 0; x < xDim2; x++) {
                smallX = x / 2;
                smallY = y / 2;
                smallPos = smallX + (smallY * xDim);
                smallPos3D = smallX + (smallY * xDim) + offset;

                if ((!mask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == true)) {

                    // have found a new contour
                    // Since genContour works on finding a four-connected boundary using
                    // Papert's turtle algorithm, the routine to grow the region must
                    // also use four connected neighbors.
                    // Set mask of all points in 2D region

                    setRegionMaskI(smallX, smallY, offset);

                    // Expand mask points so as to draw boundary properly.
                    // A pixel point only refers to left upper corner
                    setRegionMaskAll();

                    // Return polygon of contour
                    Point startPt = new Point(x, y);
                    paintBorders.add(AlgorithmMorphology2D.genContour(xDim2, yDim2, startPt, maskAll));

                    maskAll.clear();
                }
            } // end of for (x = 0; x < xDim2; x++)
        } // end of for (y = 0; y < yDim2; y++)

        // Find all zero points 4 connected by other zero points to a zero
        // point on the boundary
        maskN.clear();
        maskE.clear();
        maskS.clear();
        maskW.clear();
        maskAll.clear();

        smallY = 0;

        for (smallX = 0; smallX < xDim; smallX++) {
            smallPos = smallX + (smallY * xDim);
            smallPos3D = smallX + (smallY * xDim) + offset;

            if ((!outMask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                setRegionOutMaskI(smallX, smallY, offset);
            }
        } // for (smallX = 0; smallX < xDim; smallX++)

        smallY = yDim - 1;

        for (smallX = 0; smallX < xDim; smallX++) {
            smallPos = smallX + (smallY * xDim);
            smallPos3D = smallX + (smallY * xDim) + offset;

            if ((!outMask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                setRegionOutMaskI(smallX, smallY, offset);
            }
        } // for (smallX = 0; smallX < xDim; smallX++)

        smallX = 0;

        for (smallY = 1; smallY < (yDim - 1); smallY++) {
            smallPos = smallX + (smallY * xDim);
            smallPos3D = smallX + (smallY * xDim) + offset;

            if ((!outMask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                setRegionOutMaskI(smallX, smallY, offset);
            }
        } // for (smallY = 1; smallY < yDim - 1; smallY++)

        smallX = xDim - 1;

        for (smallY = 1; smallY < (yDim - 1); smallY++) {
            smallPos = smallX + (smallY * xDim);
            smallPos3D = smallX + (smallY * xDim) + offset;

            if ((!outMask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                setRegionOutMaskI(smallX, smallY, (short) 0);
            }
        } // for (smallY = 1; smallY < yDim - 1; smallY++)

        // Now find all zero points not 4 connected thru other
        // zero points to a boundary zero point

        for (y = 0; y < yDim2; y++) {

            for (x = 0; x < xDim2; x++) {
                smallX = x / 2;
                smallY = y / 2;
                smallPos = smallX + (smallY * xDim);
                smallPos3D = smallX + (smallY * xDim) + offset;

                if ((!outMask.get(smallPos)) && (!mask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                    // have found a new contour
                    // Since genContour works on finding a four-connected boundary using
                    // Papert's turtle algorithm, the routine to grow the region must
                    // also use four connected neighbors.
                    // Set mask of all points in 2D region

                    setRegionMaskIFalse(smallX, smallY, offset);

                    // Expand mask points so as to draw boundary properly.
                    // A pixel point only refers to left upper corner
                    setRegionMaskAll();

                    paintBorders.add(AlgorithmMorphology2D.genContour(xDim2, yDim2, new Point(x, y), maskAll));
                    maskAll.clear();
                }
            } // for (x = 0; x < xDim2 && !threadStopped; x++)
        } // for ( y = 0; y < yDim2 && !threadStopped; y++ )

        return paintBorders;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        calcInPlace();
        finalize();
    }

    /**
     * Changes the paint mask that we look for borders in.
     *
     * @param  mask  the new mask to search through
     */
    public void setPaintMask(BitSet mask) {
        srcPaintMask = mask;
    }

    /**
     * Calculates the VOI extraction.
     */
    private void calcInPlace() {
        int i;
        Polygon contourPolygon;
        int offset;

        if (srcPaintMask == null) {
            displayError("Source paint mask is null");
            setCompleted(false);

            return;
        }

        try {
            mask = new BitSet(smallLength);
            outMask = new BitSet(smallLength);
            maskN = new BitSet(length);
            maskE = new BitSet(length);
            maskS = new BitSet(length);
            maskW = new BitSet(length);
            mask2 = new BitSet(length);
            maskAll = new BitSet(length);
            maskList = new int[length][2];
            nextMaskList = new int[length][2];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm VOIExtractionPaint: Out of memory");
            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Extracting VOI", "VOIExtraction ...");
        

        boolean regVOI = true;

        if (voiIDnum == srcImage.getVOIs().size()) {
            addedVOI = new VOI(voiIDnum, "VOI" + voiIDnum, VOI.CONTOUR, -1.0f);
        } else {
            regVOI = false;

            ViewVOIVector VOIs = srcImage.getVOIs();
            int nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if ((VOIs.VOIAt(i).getID() == voiIDnum) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {
                    addedVOI = VOIs.VOIAt(i);
                }
            }
        }

        for (z = 0; z < zDim; z++) {
            mask.clear();
            outMask.clear();
            maskN.clear();
            maskE.clear();
            maskS.clear();
            maskW.clear();
            mask2.clear();
            maskAll.clear();

            offset = z * xDim * yDim;

            for (y = 0; (y < yDim2) && !threadStopped; y++) {

                for (x = 0; (x < xDim2) && !threadStopped; x++) {
                    smallX = x / 2;
                    smallY = y / 2;
                    smallPos = smallX + (smallY * xDim);
                    smallPos3D = smallX + (smallY * xDim) + offset;

                    if ((!mask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == true)) {

                        // have found a new contour
                        // Since genContour works on finding a four-connected boundary using
                        // Papert's turtle algorithm, the routine to grow the region must
                        // also use four connected neighbors.
                        // Set mask of all points in 2D region

                        setRegionMaskI(smallX, smallY, offset);

                        // Expand mask points so as to draw boundary properly.
                        // A pixel point only refers to left upper corner
                        setRegionMaskAll();

                        // Return polygon of contour
                        Point startPt = new Point(x, y);

                        contourPolygon = AlgorithmMorphology2D.genContour(xDim2, yDim2, startPt, maskAll);
                        maskAll.clear();

                        // add the polygon to an existing VOI
                        addedVOI.importPolygon(contourPolygon, z);
                        ((VOIContour) (addedVOI.getCurves().lastElement())).trimPoints(Preferences.getTrimMask(),
                                                                                          Preferences.getTrimAdjacient());

                    }
                } // end of for (x = 0; x < xDim2; x++)
            } // end of for (y = 0; y < yDim2; y++)

            // Find all zero points 4 connected by other zero points to a zero
            // point on the boundary
            maskN.clear();
            maskE.clear();
            maskS.clear();
            maskW.clear();
            maskAll.clear();

            smallY = 0;

            for (smallX = 0; smallX < xDim; smallX++) {
                smallPos = smallX + (smallY * xDim);
                smallPos3D = smallX + (smallY * xDim) + offset;

                if ((!outMask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                    setRegionOutMaskI(smallX, smallY, offset);
                }
            } // for (smallX = 0; smallX < xDim; smallX++)

            smallY = yDim - 1;

            for (smallX = 0; smallX < xDim; smallX++) {
                smallPos = smallX + (smallY * xDim);
                smallPos3D = smallX + (smallY * xDim) + offset;

                if ((!outMask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                    setRegionOutMaskI(smallX, smallY, offset);
                }
            } // for (smallX = 0; smallX < xDim; smallX++)

            smallX = 0;

            for (smallY = 1; smallY < (yDim - 1); smallY++) {
                smallPos = smallX + (smallY * xDim);
                smallPos3D = smallX + (smallY * xDim) + offset;

                if ((!outMask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                    setRegionOutMaskI(smallX, smallY, offset);
                }
            } // for (smallY = 1; smallY < yDim - 1; smallY++)

            smallX = xDim - 1;

            for (smallY = 1; smallY < (yDim - 1); smallY++) {
                smallPos = smallX + (smallY * xDim);
                smallPos3D = smallX + (smallY * xDim) + offset;

                if ((!outMask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                    setRegionOutMaskI(smallX, smallY, (short) 0);
                }
            } // for (smallY = 1; smallY < yDim - 1; smallY++)

            // Now find all zero points not 4 connected thru other
            // zero points to a boundary zero point

            for (y = 0; (y < yDim2) && !threadStopped; y++) {

                for (x = 0; (x < xDim2) && !threadStopped; x++) {
                    smallX = x / 2;
                    smallY = y / 2;
                    smallPos = smallX + (smallY * xDim);
                    smallPos3D = smallX + (smallY * xDim) + offset;

                    if ((!outMask.get(smallPos)) && (!mask.get(smallPos)) && (srcPaintMask.get(smallPos3D) == false)) {
                        // have found a new contour
                        // Since genContour works on finding a four-connected boundary using
                        // Papert's turtle algorithm, the routine to grow the region must
                        // also use four connected neighbors.
                        // Set mask of all points in 2D region

                        setRegionMaskIFalse(smallX, smallY, offset);

                        // Expand mask points so as to draw boundary properly.
                        // A pixel point only refers to left upper corner
                        setRegionMaskAll();

                        // Return polygon of contour
                        Point startPt = new Point(x, y);

                        contourPolygon = AlgorithmMorphology2D.genContour(xDim2, yDim2, startPt, maskAll);
                        maskAll.clear();

                        // add the polygon to an existing VOI
                        addedVOI.importPolygon(contourPolygon, z);
                        ((VOIContour) (addedVOI.getCurves().lastElement())).trimPoints(Preferences.getTrimMask(),
                                                                                          Preferences.getTrimAdjacient());
                    }
                } // for (x = 0; x < xDim2 && !threadStopped; x++)
            } // for ( y = 0; y < yDim2 && !threadStopped; y++ )

            fireProgressStateChanged(Math.round((z + 1) * 100.0f / zDim));
            
        } // end of for (z = 0; z < zDim; z++)

        if (threadStopped) {
            
            setCompleted(false);

            return;
        }

        if (regVOI == true) {
            srcImage.registerVOI(addedVOI);
        }

        setCompleted(true);
    }

    /**
     * Sets the mask of all points in the 4 connected region with a gray scale value.
     */
    private void setRegionMaskAll() {

        for (yt = 0; yt < yDim2; yt++) {

            for (xt = 0; xt < xDim2; xt++) {
                testPos = xt + (yt * xDim2);

                if (mask2.get(testPos)) {
                    neighbors = 0;

                    if (((xt + 1) < xDim2) && (mask2.get(xt + 1 + (xDim2 * yt)))) {
                        maskE.set(testPos);
                        neighbors++;
                    }

                    if (((xt - 1) > 0) && (mask2.get(xt - 1 + (xDim2 * yt)))) {
                        maskW.set(testPos);
                        neighbors++;
                    }

                    if (((yt + 1) < yDim2) && (mask2.get(xt + (xDim2 * (yt + 1))))) {
                        maskS.set(testPos);
                        neighbors++;
                    }

                    if (((yt - 1) > 0) && (mask2.get(xt + (xDim2 * (yt - 1))))) {
                        maskN.set(testPos);
                        neighbors++;
                    }

                    if (neighbors == 2) {

                        if ((maskN.get(testPos)) && (maskE.get(testPos))) {

                            if ((yt + 1) < yDim2) {
                                maskAll.set(xt + (xDim2 * (yt + 1)));
                            }

                            if (((xt + 1) < xDim2) && ((yt + 1) < yDim2)) {
                                maskAll.set(xt + 1 + (xDim2 * (yt + 1)));
                            }
                        } else if ((maskN.get(testPos)) && (maskS.get(testPos))) {

                            if ((xt + 1) < xDim2) {
                                maskAll.set(xt + 1 + (xDim2 * yt));
                            }

                            if ((yt + 1) < yDim2) {
                                maskAll.set(xt + (xDim2 * (yt + 1)));
                            }

                            if (((xt + 1) < xDim2) && ((yt + 1) < yDim2)) {
                                maskAll.set(xt + 1 + (xDim2 * (yt + 1)));
                            }
                        } else if ((maskN.get(testPos)) && (maskW.get(testPos))) {

                            if ((xt + 1) < xDim2) {
                                maskAll.set(xt + 1 + (xDim2 * yt));
                            }

                            if ((yt + 1) < yDim2) {
                                maskAll.set(xt + (xDim2 * (yt + 1)));
                            }

                            if (((xt + 1) < xDim2) && ((yt + 1) < yDim2)) {
                                maskAll.set(xt + 1 + (xDim2 * (yt + 1)));
                            }
                        } else if ((maskE.get(testPos)) && (maskS.get(testPos))) {

                            if ((xt + 1) < xDim2) {
                                maskAll.set(xt + 1 + (xDim2 * yt));
                            }

                            if ((yt + 1) < yDim2) {
                                maskAll.set(xt + (xDim2 * (yt + 1)));
                            }
                        } else if ((maskE.get(testPos)) && (maskW.get(testPos))) {

                            if ((xt + 1) < xDim2) {
                                maskAll.set(xt + 1 + (xDim2 * yt));
                            }

                            if ((yt + 1) < yDim2) {
                                maskAll.set(xt + (xDim2 * (yt + 1)));
                            }

                            if (((xt + 1) < xDim2) && ((yt + 1) < yDim2)) {
                                maskAll.set(xt + 1 + (xDim2 * (yt + 1)));
                            }
                        } else { // south and west neighbors

                            if ((xt + 1) < xDim2) {
                                maskAll.set(xt + 1 + (xDim2 * yt));
                            }

                            if (((xt + 1) < xDim2) && ((yt + 1) < yDim2)) {
                                maskAll.set(xt + 1 + (xDim2 * (yt + 1)));
                            }
                        }
                    } // end of else if (neighbors == 2)
                    else if (neighbors == 3) {

                        if (!maskN.get(testPos)) {

                            if ((xt + 1) < xDim2) {
                                maskAll.set(xt + 1 + (xDim2 * yt));
                            }
                        } else if (!maskE.get(testPos)) {

                            if ((xt + 1) < xDim2) {
                                maskAll.set(xt + 1 + (xDim2 * yt));
                            }

                            if (((xt + 1) < xDim2) && ((yt + 1) < yDim2)) {
                                maskAll.set(xt + 1 + (xDim2 * (yt + 1)));
                            }
                        } else if (!maskS.get(testPos)) {

                            if ((yt + 1) < yDim2) {
                                maskAll.set(xt + (xDim2 * (yt + 1)));
                            }

                            if (((xt + 1) < xDim2) && ((yt + 1) < yDim2)) {
                                maskAll.set(xt + 1 + (xDim2 * (yt + 1)));
                            }
                        } else { // no west neighbor

                            if ((yt + 1) < yDim2) {
                                maskAll.set(xt + (xDim2 * (yt + 1)));
                            }
                        }
                    } // end of else if (neighbors == 3)
                } // end of if (mask2.set(testPos)
            } // end of for (x = 0; x < xDim2; x++)
        } // end of for (y = 0; y < yDim2; y++)

        mask2.clear();
    }

    /**
     * Sets the mask of all points in the 4 connected region with a gray scale value of objectValue Uses recursion.
     *
     * @param  xStart  the x coordinate of the starting point
     * @param  yStart  the y coordinate of the starting point
     * @param  offset  offset into z plane of srcPaintMask
     */
    /*private void setRegionMaskR( int xStart, int yStart, int offset ) {
     *  mask.set( xStart + xDim * yStart ); mask2.set( 2 * xStart + xDim4 * yStart ); mask2.set( 2 * xStart + 1 + xDim4
     * yStart ); mask2.set( 2 * xStart + xDim4 * yStart + xDim2 ); mask2.set( 2 * xStart + 1 + xDim4 * yStart + xDim2 );
     * maskAll.set( 2 * xStart + xDim4 * yStart ); maskAll.set( 2 * xStart + 1 + xDim4 * yStart ); maskAll.set( 2 *
     * xStart + xDim4 * yStart + xDim2 ); maskAll.set( 2 * xStart + 1 + xDim4 * yStart + xDim2 );
     *
     * if ( ( ( xStart + 1 ) < xDim ) && ( !mask.get( xStart + 1 + xDim * yStart ) )         && ( srcPaintMask.get( offset
     * + xStart + 1 + xDim * yStart ) == true ) ) {     setRegionMaskR( xStart + 1, yStart, offset ); }
     *
     * if ( ( ( xStart - 1 ) > 0 ) && ( !mask.get( xStart - 1 + xDim * yStart ) )         && ( srcPaintMask.get( offset +
     * xStart - 1 + xDim * yStart ) == true ) ) {     setRegionMaskR( xStart - 1, yStart, offset ); }
     *
     * if ( ( ( yStart + 1 ) < yDim ) && ( !mask.get( xStart + xDim * ( yStart + 1 ) ) )         && ( srcPaintMask.get(
     * offset + xStart + xDim * ( yStart + 1 ) ) == true ) ) {     setRegionMaskR( xStart, yStart + 1, offset ); }
     *
     * if ( ( ( yStart - 1 ) > 0 ) && ( !mask.get( xStart + xDim * ( yStart - 1 ) ) )         && ( srcPaintMask.get(
     * offset + xStart + xDim * ( yStart - 1 ) ) == true ) ) {     setRegionMaskR( xStart, yStart - 1, offset ); }}*/

    /**
     * Sets the mask of all points in the 4 connected region with a gray scale value of objectValue Uses iteration.
     *
     * @param  xStart  the x coordinate of the starting point
     * @param  yStart  the y coordinate of the starting point
     * @param  offset  offset into z plane of srcPaintMask
     */
    private void setRegionMaskI(int xStart, int yStart, int offset) {
        int i, xt, yt;
        int maskNumber;
        int nextMaskNumber;

        int tempXT;
        int tempYT;

        int xStart2 = 2 * xStart;
        int yOffset4 = 2 * yStart * xDim2;

        mask.set(xStart + (xDim * yStart));
        mask2.set(xStart2 + yOffset4);
        mask2.set(xStart2 + 1 + yOffset4);
        mask2.set(xStart2 + yOffset4 + xDim2);
        mask2.set(xStart2 + 1 + yOffset4 + xDim2);
        maskAll.set(xStart2 + yOffset4);
        maskAll.set(xStart2 + 1 + yOffset4);
        maskAll.set(xStart2 + yOffset4 + xDim2);
        maskAll.set(xStart2 + 1 + yOffset4 + xDim2);

        maskList[0][0] = xStart;
        maskList[0][1] = yStart;
        maskNumber = 1;

        while (maskNumber > 0) {
            nextMaskNumber = 0;

            for (i = 0; i < maskNumber; i++) {
                xt = maskList[i][0];
                yt = maskList[i][1];

                if (((xt + 1) < xDim) && (!mask.get(xt + 1 + (xDim * yt))) &&
                        (srcPaintMask.get(offset + xt + 1 + (xDim * yt)) == true)) {
                    nextMaskList[nextMaskNumber][0] = xt + 1;
                    nextMaskList[nextMaskNumber++][1] = yt;

                    tempXT = 2 * (xt + 1);
                    tempYT = xDim4 * yt;

                    mask.set((xt + 1) + (xDim * yt));
                    mask2.set(tempXT + tempYT);
                    mask2.set(tempXT + 1 + tempYT);
                    mask2.set(tempXT + tempYT + xDim2);
                    mask2.set(tempXT + 1 + tempYT + xDim2);
                    maskAll.set(tempXT + tempYT);
                    maskAll.set(tempXT + 1 + tempYT);
                    maskAll.set(tempXT + tempYT + xDim2);
                    maskAll.set(tempXT + 1 + tempYT + xDim2);
                }

                if (((xt - 1) > 0) && (!mask.get(xt - 1 + (xDim * yt))) &&
                        (srcPaintMask.get(offset + xt - 1 + (xDim * yt)) == true)) {
                    nextMaskList[nextMaskNumber][0] = xt - 1;
                    nextMaskList[nextMaskNumber++][1] = yt;

                    tempXT = 2 * (xt - 1);
                    tempYT = xDim4 * yt;

                    mask.set((xt - 1) + (xDim * yt));
                    mask2.set(tempXT + tempYT);
                    mask2.set(tempXT + 1 + tempYT);
                    mask2.set(tempXT + tempYT + xDim2);
                    mask2.set(tempXT + 1 + tempYT + xDim2);
                    maskAll.set(tempXT + tempYT);
                    maskAll.set(tempXT + 1 + tempYT);
                    maskAll.set(tempXT + tempYT + xDim2);
                    maskAll.set(tempXT + 1 + tempYT + xDim2);
                }

                if (((yt + 1) < yDim) && (!mask.get(xt + (xDim * (yt + 1)))) &&
                        (srcPaintMask.get(offset + xt + (xDim * (yt + 1))) == true)) {
                    nextMaskList[nextMaskNumber][0] = xt;
                    nextMaskList[nextMaskNumber++][1] = yt + 1;

                    tempXT = 2 * xt;
                    tempYT = xDim4 * (yt + 1);

                    mask.set(xt + (xDim * (yt + 1)));
                    mask2.set(tempXT + tempYT);
                    mask2.set(tempXT + 1 + tempYT);
                    mask2.set(tempXT + tempYT + xDim2);
                    mask2.set(tempXT + 1 + tempYT + xDim2);
                    maskAll.set(tempXT + tempYT);
                    maskAll.set(tempXT + 1 + tempYT);
                    maskAll.set(tempXT + tempYT + xDim2);
                    maskAll.set(tempXT + 1 + tempYT + xDim2);
                }

                if (((yt - 1) > 0) && (!mask.get(xt + (xDim * (yt - 1)))) &&
                        (srcPaintMask.get(offset + xt + (xDim * (yt - 1))) == true)) {
                    nextMaskList[nextMaskNumber][0] = xt;
                    nextMaskList[nextMaskNumber++][1] = yt - 1;

                    tempXT = 2 * xt;
                    tempYT = xDim4 * (yt - 1);

                    mask.set(xt + (xDim * (yt - 1)));
                    mask2.set(tempXT + tempYT);
                    mask2.set(tempXT + 1 + tempYT);
                    mask2.set(tempXT + tempYT + xDim2);
                    mask2.set(tempXT + 1 + tempYT + xDim2);
                    maskAll.set(tempXT + tempYT);
                    maskAll.set(tempXT + 1 + tempYT);
                    maskAll.set(tempXT + tempYT + xDim2);
                    maskAll.set(tempXT + 1 + tempYT + xDim2);
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
     * Sets the mask of all points in the 4 connected region with a gray scale value of objectValue. Uses iteration.
     *
     * @param  xStart  the x coordinate of the starting point
     * @param  yStart  the y coordinate of the starting point
     * @param  offset  offset into z plane of srcPaintMask
     */
    private void setRegionMaskIFalse(int xStart, int yStart, int offset) {
        int i, xt, yt;
        int maskNumber;
        int nextMaskNumber;

        int tempXT;
        int tempYT;

        int xStart2 = 2 * xStart;
        int yOffset4 = 2 * yStart * xDim2;

        mask.set(xStart + (xDim * yStart));
        mask2.set(xStart2 + yOffset4);
        mask2.set(xStart2 + 1 + yOffset4);
        mask2.set(xStart2 + yOffset4 + xDim2);
        mask2.set(xStart2 + 1 + yOffset4 + xDim2);
        maskAll.set(xStart2 + yOffset4);
        maskAll.set(xStart2 + 1 + yOffset4);
        maskAll.set(xStart2 + yOffset4 + xDim2);
        maskAll.set(xStart2 + 1 + yOffset4 + xDim2);

        maskList[0][0] = xStart;
        maskList[0][1] = yStart;
        maskNumber = 1;

        while (maskNumber > 0) {
            nextMaskNumber = 0;

            for (i = 0; i < maskNumber; i++) {
                xt = maskList[i][0];
                yt = maskList[i][1];

                if (((xt + 1) < xDim) && (!mask.get(xt + 1 + (xDim * yt))) &&
                        (srcPaintMask.get(offset + xt + 1 + (xDim * yt)) == false)) {
                    nextMaskList[nextMaskNumber][0] = xt + 1;
                    nextMaskList[nextMaskNumber++][1] = yt;

                    tempXT = 2 * (xt + 1);
                    tempYT = xDim4 * yt;

                    mask.set((xt + 1) + (xDim * yt));
                    mask2.set(tempXT + tempYT);
                    mask2.set(tempXT + 1 + tempYT);
                    mask2.set(tempXT + tempYT + xDim2);
                    mask2.set(tempXT + 1 + tempYT + xDim2);
                    maskAll.set(tempXT + tempYT);
                    maskAll.set(tempXT + 1 + tempYT);
                    maskAll.set(tempXT + tempYT + xDim2);
                    maskAll.set(tempXT + 1 + tempYT + xDim2);
                }

                if (((xt - 1) > 0) && (!mask.get(xt - 1 + (xDim * yt))) &&
                        (srcPaintMask.get(offset + xt - 1 + (xDim * yt)) == false)) {
                    nextMaskList[nextMaskNumber][0] = xt - 1;
                    nextMaskList[nextMaskNumber++][1] = yt;

                    tempXT = 2 * (xt - 1);
                    tempYT = xDim4 * yt;

                    mask.set((xt - 1) + (xDim * yt));
                    mask2.set(tempXT + tempYT);
                    mask2.set(tempXT + 1 + tempYT);
                    mask2.set(tempXT + tempYT + xDim2);
                    mask2.set(tempXT + 1 + tempYT + xDim2);
                    maskAll.set(tempXT + tempYT);
                    maskAll.set(tempXT + 1 + tempYT);
                    maskAll.set(tempXT + tempYT + xDim2);
                    maskAll.set(tempXT + 1 + tempYT + xDim2);
                }

                if (((yt + 1) < yDim) && (!mask.get(xt + (xDim * (yt + 1)))) &&
                        (srcPaintMask.get(offset + xt + (xDim * (yt + 1))) == false)) {
                    nextMaskList[nextMaskNumber][0] = xt;
                    nextMaskList[nextMaskNumber++][1] = yt + 1;

                    tempXT = 2 * xt;
                    tempYT = xDim4 * (yt + 1);

                    mask.set(xt + (xDim * (yt + 1)));
                    mask2.set(tempXT + tempYT);
                    mask2.set(tempXT + 1 + tempYT);
                    mask2.set(tempXT + tempYT + xDim2);
                    mask2.set(tempXT + 1 + tempYT + xDim2);
                    maskAll.set(tempXT + tempYT);
                    maskAll.set(tempXT + 1 + tempYT);
                    maskAll.set(tempXT + tempYT + xDim2);
                    maskAll.set(tempXT + 1 + tempYT + xDim2);
                }

                if (((yt - 1) > 0) && (!mask.get(xt + (xDim * (yt - 1)))) &&
                        (srcPaintMask.get(offset + xt + (xDim * (yt - 1))) == false)) {
                    nextMaskList[nextMaskNumber][0] = xt;
                    nextMaskList[nextMaskNumber++][1] = yt - 1;

                    tempXT = 2 * xt;
                    tempYT = xDim4 * (yt - 1);

                    mask.set(xt + (xDim * (yt - 1)));
                    mask2.set(tempXT + tempYT);
                    mask2.set(tempXT + 1 + tempYT);
                    mask2.set(tempXT + tempYT + xDim2);
                    mask2.set(tempXT + 1 + tempYT + xDim2);
                    maskAll.set(tempXT + tempYT);
                    maskAll.set(tempXT + 1 + tempYT);
                    maskAll.set(tempXT + tempYT + xDim2);
                    maskAll.set(tempXT + 1 + tempYT + xDim2);
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
     * Sets the mask of all points in the 4 connected region with a srcPaintMask equal to false Uses iteration.
     *
     * @param  xStart  the x coordinate of the starting point
     * @param  yStart  the y coordinate of the starting point
     * @param  offset  offset into z plane of srcPaintMask
     */
    private void setRegionOutMaskI(int xStart, int yStart, int offset) {
        int i, xt, yt;
        int maskNumber;
        int nextMaskNumber;

        outMask.set(xStart + (xDim * yStart));
        maskList[0][0] = xStart;
        maskList[0][1] = yStart;
        maskNumber = 1;

        while (maskNumber > 0) {
            nextMaskNumber = 0;

            for (i = 0; i < maskNumber; i++) {
                xt = maskList[i][0];
                yt = maskList[i][1];

                if (((xt + 1) < xDim) && (!outMask.get(xt + 1 + (xDim * yt))) &&
                        (srcPaintMask.get(offset + xt + 1 + (xDim * yt)) == false)) {
                    nextMaskList[nextMaskNumber][0] = xt + 1;
                    nextMaskList[nextMaskNumber++][1] = yt;
                    outMask.set((xt + 1) + (xDim * yt));
                }

                if (((xt - 1) > 0) && (!outMask.get(xt - 1 + (xDim * yt))) &&
                        (srcPaintMask.get(offset + xt - 1 + (xDim * yt)) == false)) {
                    nextMaskList[nextMaskNumber][0] = xt - 1;
                    nextMaskList[nextMaskNumber++][1] = yt;
                    outMask.set((xt - 1) + (xDim * yt));
                }

                if (((yt + 1) < yDim) && (!outMask.get(xt + (xDim * (yt + 1)))) &&
                        (srcPaintMask.get(offset + xt + (xDim * (yt + 1))) == false)) {
                    nextMaskList[nextMaskNumber][0] = xt;
                    nextMaskList[nextMaskNumber++][1] = yt + 1;
                    outMask.set(xt + (xDim * (yt + 1)));
                }

                if (((yt - 1) > 0) && (!outMask.get(xt + (xDim * (yt - 1)))) &&
                        (srcPaintMask.get(offset + xt + (xDim * (yt - 1))) == false)) {
                    nextMaskList[nextMaskNumber][0] = xt;
                    nextMaskList[nextMaskNumber++][1] = yt - 1;
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
}
