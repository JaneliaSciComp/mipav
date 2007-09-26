package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.MipavMath;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

public class JDialogVOISplitter extends JDialogBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4074062695729029005L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int currentSlice = 0;

    /** DOCUMENT ME! */
    private int dimX = 0;

    /** DOCUMENT ME! */
    private int dimY = 0;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int infarctionVOIIndex = -1;

    /** DOCUMENT ME! */
    private int innerVOIIndex = -1;

    /** DOCUMENT ME! */
    private int numSections = 0;

    /** DOCUMENT ME! */
    private JCheckBox allSlicesBox;
    
    private Point3Df startPt;
    private Point3Df endPt;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

  
    public JDialogVOISplitter(Frame theParentFrame, ModelImage im, Point3Df sPt, Point3Df ePt) {
        super(theParentFrame, true);
        image = im;

        this.startPt = sPt;
        this.endPt = ePt;
        
        dimX = image.getExtents()[0];
        dimY = image.getExtents()[1];

        if (image.getNDims() == 3) {
            currentSlice = ((ViewJFrameImage) parentFrame).getViewableSlice();
        }
        
        init();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {

        /**
         * @todo  Implement this java.awt.event.ActionListener abstract method
         */

        String command = e.getActionCommand();

        if (command.equals("Split")) {

        	createMasks();
        	splitVOI();

            setVisible(false);
            dispose();
        }
    }

    private void createMasks() {
    	
    }
    
    private void splitVOI() {
    	boolean doAllSlices = allSlicesBox.isSelected();
    	    	
    	int nVOI = image.getVOIs().size();
    	
    	VOI currentVOI = null;
    	
    	int zDim = 1;
    	if (image.getNDims() > 2) {
    		zDim = image.getExtents()[2];
    	}
    	
    	Vector[] curves = null;
    	VOIContour currentContour = null;
    	int size = 0;
    	int numPoints = 0;
    	
    	Point3Df firstPt = null;
    	Point3Df secondPt = null;
    	
    	boolean foundIntersection = false;
    	Point3Df tempPt = new Point3Df();
    	
    	Point3Df firstIntersectionPt = null;
    	Point3Df secondIntersectionPt = null;
    	int firstIndex = 0;
    	int secondIndex = 0;
    	
    	VOI firstVOI = null;
    	VOI secondVOI = null;
    	
    	int colorID = 0;

        if (nVOI > 0) {
            colorID = ((VOI) (image.getVOIs().lastElement())).getID() + 1;
        }

        firstVOI = new VOI((short) colorID, "firstVOI", zDim, VOI.CONTOUR, -1.0f);
        colorID++;
        secondVOI = new VOI((short) colorID, "secondVOI", zDim, VOI.CONTOUR, -1.0f);
       
        int startSlice = 0;
        int endSlice = 0;

		if (doAllSlices) {
			startSlice = 0;
			endSlice = zDim;
		} else {
			startSlice = currentSlice;
			endSlice = startSlice + 1;
		}
		
		System.err.println("Splitting");
				
		
    	for (int voiIndex = nVOI - 1; voiIndex >= 0; voiIndex--) {
    		currentVOI = image.getVOIs().VOIAt(voiIndex);
    		if (currentVOI.isActive() &&
    				currentVOI.getCurveType() == VOI.CONTOUR) {
    			
    			curves = currentVOI.getCurves();
    			
    			
    				for (int slice = startSlice; slice < endSlice; slice++) {
    					size = curves[slice].size();
    					for (int voiBaseIndex = size - 1; voiBaseIndex >= 0; voiBaseIndex--) {
    						currentContour = (VOIContour)curves[slice].elementAt(voiBaseIndex);
    						if (currentContour.isClosed()) {
    							numPoints = currentContour.size();
    							for (int ptIndex = 0; ptIndex < numPoints - 1 && (secondIntersectionPt == null); ptIndex++) {
    								firstPt = currentContour.elementAt(ptIndex);
    								secondPt = currentContour.elementAt(ptIndex + 1);
    								
    								foundIntersection = intersects(firstPt, secondPt, startPt, endPt, tempPt);
    								
    								//check for intersection, assign to either 1st or 2nd pt
    								if (foundIntersection) {
    									//System.err.println("Found intersection: " + tempPt);
    									if (firstIntersectionPt == null) {
    										firstIntersectionPt = new Point3Df(MipavMath.round(tempPt.x), MipavMath.round(tempPt.y), tempPt.z);
    										firstIndex = ptIndex;
    									} else if (secondIntersectionPt == null) {
    										secondIntersectionPt = new Point3Df(MipavMath.round(tempPt.x), MipavMath.round(tempPt.y), tempPt.z);
    										secondIndex = ptIndex;
    									} else {
    										MipavUtil.displayError("VOI Splitter does not support more than 2 intersection points: exiting");
    										return;
    									}
    								}
    								
    							}
    							
    							if (firstIntersectionPt == null) {
    								//no intersection found
    								
    							} else if (secondIntersectionPt == null) {
    								//need to check last two segments
    								
    								firstPt = currentContour.elementAt(numPoints - 1);
    								secondPt = currentContour.elementAt(0);
    								
    								foundIntersection = intersects(firstPt, secondPt, startPt, endPt, tempPt);
    								
    								if (foundIntersection) {
    									secondIntersectionPt = new Point3Df(MipavMath.round(tempPt.x), MipavMath.round(tempPt.y), tempPt.z);
    									secondIndex = numPoints - 1;
    								}
    							}
    							
    							//if both points were found (no more, no less than 2)
    							if (firstIntersectionPt != null &&
    									secondIntersectionPt != null) {
    								
    								VOIContour firstContour = new VOIContour("firstContour", true);
    								VOIContour secondContour = new VOIContour("secondContour", true);
    								
    								//add the second intersection point 1st
    								firstContour.addElement(new Point3Df(secondIntersectionPt.x, secondIntersectionPt.y, secondIntersectionPt.z));
    									
    								//check if there are points from second index to 0-index, add those first
    								for (int ptIndex = secondIndex + 1; ptIndex < numPoints; ptIndex++) {
    									firstContour.addElement(currentContour.elementAt(ptIndex));
    								}
    									
    								//add points from 0-index to first index 
    								for (int ptIndex = 0; ptIndex < firstIndex + 1; ptIndex++) {
    									firstContour.addElement(currentContour.elementAt(ptIndex));
    								}
    									
    								//add the first intersection point
    								firstContour.addElement(new Point3Df(firstIntersectionPt.x, firstIntersectionPt.y, firstIntersectionPt.z));
    								
    								//repeat for second contour
 
    								//add the first intersection point
    								secondContour.addElement(new Point3Df(firstIntersectionPt.x, firstIntersectionPt.y, firstIntersectionPt.z));
    									
    								//add all points between the first and second intersection points
    								for (int ptIndex = firstIndex + 1; ptIndex < secondIndex + 1 && ptIndex < numPoints; ptIndex++) {
    									secondContour.addElement(currentContour.elementAt(ptIndex));
    								}
    									
    								//add the second intersection point
    								secondContour.addElement(new Point3Df(secondIntersectionPt.x, secondIntersectionPt.y, secondIntersectionPt.z));
    									
    									
    								//determine which contour goes where
    									
    								firstVOI.importCurve(firstContour, slice);    									
    									
    								secondVOI.importCurve(secondContour, slice);
    								
    								firstIntersectionPt = null;
    								secondIntersectionPt = null;
    						
    								curves[slice].remove(voiBaseIndex);
    								currentContour = null;
    								
    								if (image.getVOIs().VOIAt(voiIndex).isEmpty()) {
    									image.getVOIs().remove(voiIndex);
    								}
    								
    							} else {
    								//intersection NOT found...but must still add contour to either 1st or 2nd VOI
    								
    							}
    								
    						}
    					}
    				}
    			//image.getVOIs().remove(voiIndex);
    		}
    			
    	}
    	
    	image.getVOIs().addVOI(firstVOI);
    	image.getVOIs().addVOI(secondVOI);
    	image.notifyImageDisplayListeners();
    	image.getParentFrame().getComponentImage().setCursorMode(ViewJComponentBase.DEFAULT);
    	
    }
    
    /**
     * Determine if there are THREE VOIs, Two closed (one completely inside the other) and a third that is unclosed (the
     * infarction).
     *
     * @return  boolean
     */
    private boolean checkVOIs() {
        System.err.println("checking VOIs...");

        BitSet mask = new BitSet(image.getExtents()[0] * image.getExtents()[1]);
        System.err.println("Number of VOIs:" + image.getVOIs().size());

        if ((image.getVOIs() != null) && (image.getVOIs().size() == 3)) {

            System.err.println("Got 3 VOI contours");

            int outerGuess = -1;
            int innerGuess = -1;

            // search for the infarction VOI (only un-closed contour)
            if (image.getVOIs().VOIAt(0).getCurveType() == VOI.POLYLINE) {
                infarctionVOIIndex = 0;
                outerGuess = 1;
                innerGuess = 2;
            } else if (image.getVOIs().VOIAt(1).getCurveType() == VOI.POLYLINE) {
                infarctionVOIIndex = 1;
                outerGuess = 0;
                innerGuess = 2;
            } else if (image.getVOIs().VOIAt(2).getCurveType() == VOI.POLYLINE) {
                infarctionVOIIndex = 2;
                outerGuess = 0;
                innerGuess = 1;
            } else {
                System.err.println("Had a problem trying to parse the VOIs");
            }

            System.err.println("Infarction index: " + infarctionVOIIndex);

            // try binary masking each VOI to see if one is completely inside the other


            // first try: get the binary mask for the first VOI, then test it on the second
            image.getVOIs().VOIAt(outerGuess).createBinaryMask(mask, image.getExtents()[0], image.getExtents()[1]);

            if (image.getVOIs().VOIAt(innerGuess).isBinaryMaskContained(mask, image.getExtents()[0],
                                                                            image.getExtents()[1])) {
               // outerVOIIndex = outerGuess;
                innerVOIIndex = innerGuess;

                // System.err.println("Outer index: " + outerVOIIndex + ", inner index: " + innerVOIIndex);
                return true;
            }
            // VOI at index 1 was not inside VOI at index 0, check for the reverse
            else {
                int temp = outerGuess;
                outerGuess = innerGuess;
                innerGuess = temp;

                mask.clear();

                image.getVOIs().VOIAt(outerGuess).createBinaryMask(mask, image.getExtents()[0], image.getExtents()[1]);

                if (image.getVOIs().VOIAt(innerGuess).isBinaryMaskContained(mask, image.getExtents()[0],
                                                                                image.getExtents()[1])) {
                  //  outerVOIIndex = outerGuess;
                    innerVOIIndex = innerGuess;

                    // System.err.println("Outer index: " + outerVOIIndex + ", inner index: " + innerVOIIndex);
                    return true;
                } else {
                    return false;
                }
            }
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void createSections() {
        // get the center of mass, then try to find intersections...

        //numSections = Integer.parseInt(numSectionsField.getText());

        double angleInc = 0;

        Point3Df centerPt = image.getVOIs().VOIAt(0).getCenterOfMass();

        float[] x;
        float[] y;
        float[] z;

        int[] xPtsInner;
        int[] yPtsInner;
        int[] xPtsOuter;
        int[] yPtsOuter;

        int cX = (int) centerPt.x;
        int cY = (int) centerPt.y;
        int x2 = 0, y2 = 0;

        int dimX = image.getExtents()[0];
        int dimY = image.getExtents()[1];

        xPtsInner = new int[numSections];
        yPtsInner = new int[numSections];
        xPtsOuter = new int[numSections];
        yPtsOuter = new int[numSections];

        x = new float[2];
        y = new float[2];
        z = new float[2];

        angleInc = 2.0 * Math.PI / numSections;

        // System.err.println("angle increment is (deg): " + ((angleInc * 180) / Math.PI));

        double angle = 0;

        Point3Df secondPt = new Point3Df();

        VOIContour outerContour = (VOIContour)
                                      image.getVOIs().VOIAt(0).getCurves()[currentSlice].elementAt(0);
        VOIContour innerContour = (VOIContour)
                                      image.getVOIs().VOIAt(innerVOIIndex).getCurves()[currentSlice].elementAt(0);
        VOIContour infarctionContour = (VOIContour)
                                           image.getVOIs().VOIAt(infarctionVOIIndex).getCurves()[currentSlice].elementAt(0);

        int numPtsOuter = outerContour.size();
        int numPtsInner = innerContour.size();

        // create an array of Vectors to store the infarction section intersections
        Vector[] infSecIntersects = new Vector[numSections];

        // create another array of Vectors to store the infarction intersections
        // with the inner contour
        Vector infInnerIntersects = new Vector();

        boolean foundIntersection = false;

        Point3Df intersectionPt = new Point3Df();


        int numPtsInfarction = infarctionContour.size();

        for (int i = 0; i < numSections; i++, angle += angleInc) {

            // find a second point to use
            VOICardiology.getSecondPoint(angle, centerPt, secondPt, dimX, dimY);

            // search for an intersection with a segment created by
            // two adjacent points in the INNER VOIContour

            foundIntersection = false;

            // try looking at segment joining last and first pt in inner contour
            foundIntersection = VOICardiology.intersects(centerPt, secondPt,
                                                         (Point3Df) innerContour.elementAt(numPtsInner - 1),
                                                         (Point3Df) innerContour.elementAt(0), intersectionPt);

            for (int j = 0; !foundIntersection && (j < (numPtsInner - 1)); j++) {

                foundIntersection = VOICardiology.intersects(centerPt, secondPt, (Point3Df) innerContour.elementAt(j),
                                                             (Point3Df) innerContour.elementAt(j + 1), intersectionPt);
            }

            if (!foundIntersection) {
                System.err.println("Could not find inner intersection for point " + (i + 1) + " for inner contour");
            } else {
                x[0] = intersectionPt.x;
                y[0] = intersectionPt.y;
                z[0] = 0;

                xPtsInner[i] = (int) intersectionPt.x;
                yPtsInner[i] = (int) intersectionPt.y;
                // innerContour.insertElement( (int)intersectionPt.x, (int)intersectionPt.y, 0 );
            }

            // do the same for the outer contour

            // try looking at segment joining last and first pt in inner contour
            foundIntersection = VOICardiology.intersects(centerPt, secondPt,
                                                         (Point3Df) outerContour.elementAt(numPtsOuter - 1),
                                                         (Point3Df) outerContour.elementAt(0), intersectionPt);

            for (int j = 0; !foundIntersection && (j < (numPtsOuter - 1)); j++) {

                foundIntersection = VOICardiology.intersects(centerPt, secondPt, (Point3Df) outerContour.elementAt(j),
                                                             (Point3Df) outerContour.elementAt(j + 1), intersectionPt);
            }

            if (!foundIntersection) {
                System.err.println("Could not find outer intersection for point " + (i + 1) + " for inner contour");
            } else {
                x[1] = intersectionPt.x;
                y[1] = intersectionPt.y;
                z[1] = 0;

                xPtsOuter[i] = (int) intersectionPt.x;
                yPtsOuter[i] = (int) intersectionPt.y;
                // outerContour.insertElement( (int)intersectionPt.x, (int)intersectionPt.y, 0 );
            }

            // now look for any section divider intersections along the infarction contour
            int sectionCounter = 0;
            foundIntersection = false;

            // for (int j = 0; j < numPtsInfarction - 1 && !foundIntersection; j++) {
            for (int j = 0; j < (numPtsInfarction - 1); j++) {
                foundIntersection = VOICardiology.intersects(centerPt, secondPt,
                                                             (Point3Df) infarctionContour.elementAt(j),
                                                             (Point3Df) infarctionContour.elementAt(j + 1),
                                                             intersectionPt);

                // if an intersection is found, mark it but continue looking through all points
                // of the infarction contour for more intersections along the same section divider
                if (foundIntersection) {

                    if (infSecIntersects[i] == null) {
                        infSecIntersects[i] = new Vector();
                    }

                    infSecIntersects[i].add(new Point3Df(intersectionPt.x, intersectionPt.y, 0));

                    // System.err.println("found intersection along section " + i + " and added point to vector");
                    sectionCounter++;
                }
            }
        }

        Vector currentVec = null;
        Point3Df currentPt = null;

        // add the new section intersection points to the infarction contour
        for (int k = 0; k < numSections; k++) {
            currentVec = infSecIntersects[k];

            if (currentVec != null) {

                for (int j = 0; j < currentVec.size(); j++) {

                    currentPt = (Point3Df) currentVec.elementAt(j);
                    numPtsInfarction = infarctionContour.size();

                    if (infarctionContour.nearLine((int) currentPt.x, (int) currentPt.y)) {
                        infarctionContour.insertElement((int) currentPt.x, (int) currentPt.y, 0);
                    }
                }
            }
        }

        // now look for any infarction intersections with the inner contour
        for (int k = 0; k < (numPtsInfarction - 1); k++) {

            // check first between first & last pt of inner contour
            foundIntersection = VOICardiology.intersects((Point3Df) innerContour.elementAt(numPtsInner - 1),
                                                         (Point3Df) innerContour.elementAt(0),
                                                         (Point3Df) infarctionContour.elementAt(k),
                                                         (Point3Df) infarctionContour.elementAt(k + 1), intersectionPt);

            for (int j = 0; !foundIntersection && (j < (numPtsInner - 1)); j++) {
                foundIntersection = VOICardiology.intersects((Point3Df) innerContour.elementAt(j),
                                                             (Point3Df) innerContour.elementAt(j + 1),
                                                             (Point3Df) infarctionContour.elementAt(k),
                                                             (Point3Df) infarctionContour.elementAt(k + 1),
                                                             intersectionPt);
            }

            if (foundIntersection) {
                infInnerIntersects.add(new Point3Df(intersectionPt.x, intersectionPt.y, 0));
            }

        }

        // add these new intersection points to the infarction contour
        // AND to the inner contour (mirrored points in both contours)
        for (int i = 0; i < infInnerIntersects.size(); i++) {
            currentPt = (Point3Df) infInnerIntersects.elementAt(i);

            if (infarctionContour.nearLine((int) currentPt.x, (int) currentPt.y)) {
                infarctionContour.insertElement((int) currentPt.x, (int) currentPt.y, 0);
            }

            if (innerContour.nearLine((int) currentPt.x, (int) currentPt.y)) {
                innerContour.insertElement((int) currentPt.x, (int) currentPt.y, 0);
            }
        }

        // temporary stuff:  add all the new pts to the contour
        for (int i = 0; i < numSections; i++) {

            if (innerContour.nearLine(xPtsInner[i], yPtsInner[i])) {
                innerContour.insertElement(xPtsInner[i], yPtsInner[i], 0);

            }

            if (outerContour.nearLine(xPtsOuter[i], yPtsOuter[i])) {
                outerContour.insertElement(xPtsOuter[i], yPtsOuter[i], 0);
            }
        }

        // start importing points into the VOICardiology
        // ....begin with the inner contour

        VOI newVOI = new VOI((short) 8, "cardio.voi", 1, VOI.CARDIOLOGY, -1.0f);
        VOICardiology cardioVOI = new VOICardiology(numSections, centerPt);

        currentPt = null;

        boolean isIntersection = false;

        numPtsInner = innerContour.size();

        // find the first intersection point
        int firstPt;

        for (firstPt = 0; firstPt < numPtsInner; firstPt++) {
            currentPt = (Point3Df) innerContour.elementAt(firstPt);

            if (((int) currentPt.x == xPtsInner[0]) && ((int) currentPt.y == yPtsInner[0])) {
                cardioVOI.addPoint(currentPt, VOICardiology.INNER, 0, true, false);

                break;
            }
        }

        // last found intersection point (speeds up looking for each intersection point)
        int startPt = 1;

        for (int i = firstPt; i < numPtsInner; i++) {
            currentPt = (Point3Df) innerContour.elementAt(i);

            // see if it is one of the intersection points (already found pt 1)
            for (int j = startPt; j < numSections; j++) {

                if (((int) currentPt.x == xPtsInner[j]) && ((int) currentPt.y == yPtsInner[j])) {
                    isIntersection = true;
                    cardioVOI.addPoint(currentPt, VOICardiology.INNER, j, true, false);
                    startPt++;
                }
            }

            // otherwise it is a normal curve pt or an intersection between the inner and infarction
            if (!isIntersection) {

                // look through infarction-inner intersection vector
                for (int m = 0; m < infInnerIntersects.size(); m++) {

                    if (((int) currentPt.x == (int) ((Point3Df) infInnerIntersects.elementAt(m)).x) &&
                            ((int) currentPt.y == (int) ((Point3Df) infInnerIntersects.elementAt(m)).y)) {
                        cardioVOI.addPoint(currentPt, VOICardiology.INNER, (startPt - 1), false, true);
                        isIntersection = true;
                        // System.err.println("found intersection pt for INNER and adding as a BOTH pt");
                    }
                }

                // must be a normal inner curve point (doesn't intersection section or infarction)
                if (!isIntersection) {
                    cardioVOI.addPoint(currentPt, VOICardiology.INNER, (startPt - 1), false, false);
                }
            }

            isIntersection = false;
        }

        // now add the first half of the pts from the inner contour
        if (firstPt > 0) {

            for (int i = 0; i < firstPt; i++) {
                currentPt = (Point3Df) innerContour.elementAt(i);

                // see if it is one of the intersection points (already found points before startPt)
                for (int j = startPt; j < numSections; j++) {

                    if (((int) currentPt.x == xPtsInner[j]) && ((int) currentPt.y == yPtsInner[j])) {
                        isIntersection = true;
                        cardioVOI.addPoint(currentPt, VOICardiology.INNER, j, true, false);
                        startPt++;
                    }
                }

                if (!isIntersection) {

                    // look through infarction-inner intersection vector
                    for (int m = 0; m < infInnerIntersects.size(); m++) {

                        if (((int) currentPt.x == (int) ((Point3Df) infInnerIntersects.elementAt(m)).x) &&
                                ((int) currentPt.y == (int) ((Point3Df) infInnerIntersects.elementAt(m)).y)) {
                            cardioVOI.addPoint(currentPt, VOICardiology.INNER, (startPt - 1), false, true);
                            isIntersection = true;
                        }
                    }

                    // must be a normal inner curve point (doesn't intersection section or infarction)
                    if (!isIntersection) {
                        cardioVOI.addPoint(currentPt, VOICardiology.INNER, (startPt - 1), false, false);
                    }
                }

                isIntersection = false;
            }
        }

        // repeat the same process for the outer contour

        numPtsOuter = outerContour.size();

        for (firstPt = 0; firstPt < numPtsOuter; firstPt++) {
            currentPt = (Point3Df) outerContour.elementAt(firstPt);

            if (((int) currentPt.x == xPtsOuter[0]) && ((int) currentPt.y == yPtsOuter[0])) {
                cardioVOI.addPoint(currentPt, VOICardiology.OUTER, 0, true, false);

                break;
            }
        }
        // System.err.println("first pt outer at index: " + firstPt);

        // last found intersection point (speeds up looking for each intersection point)
        startPt = 1;

        for (int i = firstPt; i < numPtsOuter; i++) {
            currentPt = (Point3Df) outerContour.elementAt(i);

            // see if it is one of the intersection points (already found pt 1)
            for (int j = startPt; j < numSections; j++) {

                if (((int) currentPt.x == xPtsOuter[j]) && ((int) currentPt.y == yPtsOuter[j])) {
                    isIntersection = true;
                    cardioVOI.addPoint(currentPt, VOICardiology.OUTER, j, true, false);
                    startPt++;
                }
            }

            // otherwise it is a normal curve pt
            if (!isIntersection) {
                cardioVOI.addPoint(currentPt, VOICardiology.OUTER, (startPt - 1), false, false);
            }

            isIntersection = false;
        }

        // now add the first half of the pts from the outer contour
        if (firstPt > 0) {

            for (int i = 0; i < firstPt; i++) {
                currentPt = (Point3Df) outerContour.elementAt(i);

                // see if it is one of the intersection points (already found points before startPt)
                for (int j = startPt; j < numSections; j++) {

                    if (((int) currentPt.x == xPtsOuter[j]) && ((int) currentPt.y == yPtsOuter[j])) {
                        isIntersection = true;
                        cardioVOI.addPoint(currentPt, VOICardiology.OUTER, j, true, false);
                        startPt++;
                    }
                }

                // otherwise it is a normal curve pt
                if (!isIntersection) {
                    cardioVOI.addPoint(currentPt, VOICardiology.OUTER, (startPt - 1), false, false);
                }

                isIntersection = false;
            }
        }

        // finally add the infarction points

        int numInfarctions = 0; // number of distinct infarction areas outside the inner contour
        boolean insideInner = true; // tells if the infarction is currently INSIDE the inner contour
        numPtsInfarction = infarctionContour.size();

        int section = -1;

        Vector currentVector = null;

        int infarctionCounter = 0;

        for (int i = 0; i < numPtsInfarction; i++) {
            isIntersection = false;
            currentPt = (Point3Df) infarctionContour.elementAt(i);

            // find in which section the infarction point should be placed
            section = cardioVOI.getSection((int) currentPt.x, (int) currentPt.y);

            if (section < 0) {
                System.err.println("error finding section for infarction point");
            }

            // look through infarction-inner intersection vector (shared point)
            for (int m = 0; m < infInnerIntersects.size(); m++) {

                if (((int) currentPt.x == (int) ((Point3Df) infInnerIntersects.elementAt(m)).x) &&
                        ((int) currentPt.y == (int) ((Point3Df) infInnerIntersects.elementAt(m)).y)) {
                    cardioVOI.addPoint(currentPt, VOICardiology.INFARCTION, section, false, true);
                    isIntersection = true;
                    insideInner = !insideInner;
                    infarctionCounter++;

                    break;
                }
            }

            if (!isIntersection && !insideInner) {

                for (int m = 0; m < numSections; m++) {
                    currentVector = infSecIntersects[m];

                    if (currentVector != null) {

                        for (int k = 0; k < currentVector.size(); k++) {

                            if (((int) currentPt.x == (int) ((Point3Df) currentVector.elementAt(k)).x) &&
                                    ((int) currentPt.y == (int) ((Point3Df) currentVector.elementAt(k)).y)) {
                                cardioVOI.addPoint(currentPt, VOICardiology.INFARCTION, m, true, false);
                                isIntersection = true;
                                infarctionCounter++;
                            }
                        }
                    }
                }
            }

            // must be a normal inner curve point (doesn't intersection section or infarction)
            if (!isIntersection && !insideInner) {
                numInfarctions++;
                cardioVOI.addPoint(currentPt, VOICardiology.INFARCTION, section, false, false);
                infarctionCounter++;
            }
        }

        cardioVOI.setNumInfarctions(numInfarctions);

        newVOI.importCurve(cardioVOI);


        ModelImage newImage = null;

        if (image.getNDims() == 3) {
            newImage = new ModelImage(image.getType(), new int[] { dimX, dimY }, image.getImageName() + "_CardioVOI");

            int sliceSize = dimX * dimY;
            float[] buffer = new float[sliceSize];

            int currentSlice = ((ViewJFrameImage) parentFrame).getViewableSlice();

            try {
                image.exportData((currentSlice * sliceSize), sliceSize, buffer);
                newImage.importData(0, buffer, true);
            } catch (Exception ex) {
                System.err.println("something messed up");
            }

            FileInfoBase[] fBase = new FileInfoBase[1];

            fBase[0] = (FileInfoBase) image.getFileInfo()[currentSlice].clone();
            newImage.setFileInfo(fBase);
        } else {
            newImage = (ModelImage) image.clone();
        }

        newImage.resetVOIs();
        newImage.registerVOI(newVOI);

        new ViewJFrameCardiology(newImage, null, new Dimension(100, 100));

    }

    /**
     * DOCUMENT ME!
     */
    private void init() {
        setTitle("Split VOI");

        JPanel mainPanel = new JPanel();

        allSlicesBox = new JCheckBox("Split all slices: ", true);

        JButton splitButton = new JButton("Split");
        splitButton.addActionListener(this);
        splitButton.setActionCommand("Split");

        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;

        mainPanel.add(allSlicesBox, gbc);


        gbc.gridx = 2;
        mainPanel.add(splitButton, gbc);

        getContentPane().add(mainPanel);
        pack();
    }

    public static boolean intersects(Point3Df p1, Point3Df p2, Point3Df p3, Point3Df p4, Point3Df intersection) {
        double denom = 0f;
        double uAnum = 0f;
        double uBnum = 0f;
        double uA = 0f;
        double uB = 0f;
        denom = ((p4.y - p3.y) * (p2.x - p1.x)) - ((p4.x - p3.x) * (p2.y - p1.y));
        uAnum = ((p4.x - p3.x) * (p1.y - p3.y)) - ((p4.y - p3.y) * (p1.x - p3.x));
        uBnum = ((p2.x - p1.x) * (p1.y - p3.y)) - ((p2.y - p1.y) * (p1.x - p3.x));

        if (denom == 0) {

            // System.err.println("Denom is 0");
            return false;
        }

        uA = uAnum / denom;
        uB = uBnum / denom;

        if ((uA >= 0) && (uA <= 1) && (uB >= 0) && (uB <= 1)) {
            intersection.x = p1.x + (float) (uA * (p2.x - p1.x));
            intersection.y = p1.y + (float) (uA * (p2.y - p1.y));

            // System.err.println("found intersection to be: " + intersection.x + "," + intersection.y + "\n\n");
            return true;
        } else {
            return false;
        }
    }
    
}
