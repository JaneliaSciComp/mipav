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
    
    private float slope;
    private float b;
    private float tol = 1.0f;
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

        	calcSlopeAndB();
        	splitVOI();

            setVisible(false);
            dispose();
        }
    }

    private void calcSlopeAndB() {
    	if (endPt.x - startPt.x == 0) {
    		slope = 1;
    	} else {
    		slope = (endPt.y - startPt.y) / (endPt.x - startPt.x);
    	}
    	b = endPt.y - (slope * endPt.x);    	
    }
    
    private int checkSide(Point3Df testPoint) {
    	
    	//System.err.println("checkSide(" + testPoint + ")");
    	
    	float dif = testPoint.y - ((slope * testPoint.x) + b);
    	//System.err.println("checkSide() dif is: " + dif);
    	if (dif > tol) {
    		return 1;
    	} else if (dif <= tol && dif >= 0) {
    		return 0;
    	} else {
    		return -1;
    	}
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
		
		int currentSide = 0;
		int currentSideB = 0;
		
    	for (int voiIndex = nVOI - 1; voiIndex >= 0; voiIndex--) {
    		currentVOI = image.getVOIs().VOIAt(voiIndex);
    		if (currentVOI.getCurveType() == VOI.CONTOUR) {
    			
    			curves = currentVOI.getCurves();
    			
    			
    				for (int slice = startSlice; slice < endSlice; slice++) {
    					size = curves[slice].size();
    					for (int voiBaseIndex = size - 1; voiBaseIndex >= 0; voiBaseIndex--) {
    						currentContour = (VOIContour)curves[slice].elementAt(voiBaseIndex);
    						if (currentContour.isClosed()) {
    						//	System.err.println("found closed contour VOIAt(" + voiIndex + ").curves[" +
    						//			slice + "].elementAt(" + voiBaseIndex + ")");
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
    							
    							
    							if (firstIntersectionPt != null &&
    									secondIntersectionPt == null) {
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
    								
    								//System.err.println("Found two intersection points: " + firstIntersectionPt + "\n\t" + secondIntersectionPt);
    								
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
    									
    								currentSide = 0;
    								for (int j = 1; j < firstContour.size(); j++) {
    									
    									currentSide = checkSide(firstContour.elementAt(j));
    									if (currentSide != 0) {
    										break;
    									}
    								}
    								
    								currentSideB = 0;
    								for (int j = 1; j < secondContour.size(); j++) {
    									
    									currentSideB = checkSide(secondContour.elementAt(j));
    									if (currentSideB != 0) {
    										break;
    									}
    								}
    								
    								//System.err.println("current side for first contour: " + currentSide );
    								//System.err.println("current side for second contour: " + currentSideB + "\n\n");
    								
    								if (currentSide == 1) {
    									firstVOI.importCurve(firstContour, slice);
    									secondVOI.importCurve(secondContour, slice);
    								} else {
    									firstVOI.importCurve(secondContour, slice);
    									secondVOI.importCurve(firstContour, slice);
    								}
    								
    								firstIntersectionPt = null;
    								secondIntersectionPt = null;
    						
    								curves[slice].remove(voiBaseIndex);
    								currentContour = null;
    								
    								if (image.getVOIs().VOIAt(voiIndex).isEmpty()) {
    									System.err.println("VOIAt(" + voiIndex + ") now empty: removing");
    									image.getVOIs().remove(voiIndex);
    								}
    								
    							} else {
    							//	System.err.println("Did not find two intersection points, adding contour ");
    								
    								//intersection NOT found...but must still add contour to either 1st or 2nd VOI
    								currentSide = checkSide(currentContour.elementAt(0));
    									
    						//		System.err.println("current side: " + currentSide);
    								if (currentSide == 1) {
    									//System.err.println("Did not find two intersection points, adding contour to firstVOI" + "\n\n");
    									firstVOI.importCurve(currentContour, slice);
    									
    								} else {
    									//System.err.println("Did not find two intersection points, adding contour to secondVOI" + "\n\n");
    									secondVOI.importCurve(currentContour, slice);
    								}
    								
    								if (image.getVOIs().VOIAt(voiIndex).isEmpty()) {
    									//System.err.println("VOIAt(" + voiIndex + ") now empty: removing");
    									image.getVOIs().remove(voiIndex);
    								}
    								
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
