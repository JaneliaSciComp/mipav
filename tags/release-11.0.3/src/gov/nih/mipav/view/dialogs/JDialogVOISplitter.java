package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.util.MipavMath;

import java.awt.*;
import java.awt.event.*;
import java.io.FileNotFoundException;

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
    private ModelImage image;

    /** DOCUMENT ME! */
    private JRadioButton allSlicesRadio;
    
    private JRadioButton onlyActiveRadio;
    
    private ButtonGroup radioGroup;
    
    private Vector3f startPt;
    private Vector3f endPt;
    
    private float slope;
    private float b;
    private float tol = 1.0f;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    
    public JDialogVOISplitter(Frame theParentFrame, ModelImage im, Vector3f sPt, Vector3f ePt) {
        super(theParentFrame, true);
        image = im;

        this.startPt = sPt;
        this.endPt = ePt;

        if (image.getNDims() == 3) {
            currentSlice = ((ViewJFrameImage) parentFrame).getViewableSlice();
        }
        
        init();
        setVisible(true);
    }

    
    public JDialogVOISplitter(ModelImage im) {
        super(true);        
        image = im;
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
            setVisible(false);
            if ( startPt != null && endPt != null )
            {
                calcSlopeAndB();
                splitVOI();
                dispose();
            }
        } else {
            super.actionPerformed(e);
        }
    }
    
    public boolean getAllSlices()
    {
        return allSlicesRadio.isSelected();
    }
    
    public boolean getOnlyActive()
    {
        return onlyActiveRadio.isSelected();
    }

    private void calcSlopeAndB() {
    	if (endPt.X - startPt.X == 0) {
    		slope = 1;
    	} else {
    		slope = (endPt.Y - startPt.Y) / (endPt.X - startPt.X);
    	}
    	b = endPt.Y - (slope * endPt.X);    	
    }
    
    private int checkSide(Vector3f testPoint) {
    	
    	//System.err.println("checkSide(" + testPoint + ")");
    	
    	float dif = testPoint.Y - ((slope * testPoint.X) + b);
    	//System.err.println("checkSide() dif is: " + dif);
    	if (dif > tol) {
    		return 1;
    	} else if (dif <= tol && dif >= 0) {
    		return 0;
    	} else {
    		return -1;
    	}
    }
    
    /**
     * Splits the VOI
     *
     */
    private void splitVOI() {
    	boolean doAllSlices = allSlicesRadio.isSelected();
    	    	
    	boolean onlyActive = onlyActiveRadio.isSelected();
    	
    	int nVOI = image.getVOIs().size();
    	
    	VOI currentVOI = null;
    	
    	int zDim = 1;
    	if (image.getNDims() > 2) {
    		zDim = image.getExtents()[2];
    	}
    	
    	Vector<VOIBase>[] curves = null;
    	VOIContour currentContour = null;
    	int size = 0;
    	int numPoints = 0;
    	
    	Vector3f firstPt = null;
    	Vector3f secondPt = null;
    	
    	boolean foundIntersection = false;
    	Vector3f tempPt = new Vector3f();
    	
    	Vector3f firstIntersectionPt = null;
    	Vector3f secondIntersectionPt = null;
    	int firstIndex = 0;
    	int secondIndex = 0;
    	
    	VOI firstVOI = null;
    	VOI secondVOI = null;
    	
    	int colorID = 0;

        if (nVOI > 0) {
            colorID = ((VOI) (image.getVOIs().lastElement())).getID() + 1;
        }

        firstVOI = new VOI((short) colorID, "firstVOI", VOI.CONTOUR, -1.0f);
        colorID++;
        secondVOI = new VOI((short) colorID, "secondVOI", VOI.CONTOUR, -1.0f);
       
        int startSlice = 0;
        int endSlice = 0;

		if (doAllSlices) {
			startSlice = 0;
			endSlice = zDim;
		} else {
			startSlice = currentSlice;
			endSlice = startSlice + 1;
		}
		
		int currentSide = 0;
		int currentSideB = 0;
		
		boolean isClosed = false;
		
    	for (int voiIndex = nVOI - 1; voiIndex >= 0; voiIndex--) {
    		currentVOI = image.getVOIs().VOIAt(voiIndex);
    		//System.err.println(voiIndex + ", " + currentVOI.getCurveType());
    		if (currentVOI.getCurveType() == VOI.CONTOUR && (!onlyActive || currentVOI.isActive())) {
    			
    			curves = currentVOI.getSortedCurves( VOIBase.ZPLANE, zDim );
    			
    			
    				for (int slice = startSlice; slice < endSlice; slice++) {
    					size = curves[slice].size();
    					for (int voiBaseIndex = size - 1; voiBaseIndex >= 0; voiBaseIndex--) {
    						currentContour = (VOIContour)curves[slice].elementAt(voiBaseIndex);
    						if (!onlyActive || currentContour.isActive() ) {
    							
    							isClosed = currentContour.isClosed();
    						//	System.err.println("checking contour.  isClosed: " + isClosed);
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
    										firstIntersectionPt = new Vector3f(MipavMath.round(tempPt.X), MipavMath.round(tempPt.Y), tempPt.Z);
    										firstIndex = ptIndex;
    									} else if (secondIntersectionPt == null) {
    										secondIntersectionPt = new Vector3f(MipavMath.round(tempPt.X), MipavMath.round(tempPt.Y), tempPt.Z);
    										secondIndex = ptIndex;
    									} else {
    										MipavUtil.displayError("VOI Splitter does not support more than 2 intersection points: exiting");
    										return;
    									}
    								}
    								
    							}
    							
    							//only check the last to first point segment if contour is closed (and 2nd intersection pt null)
    							if (isClosed && firstIntersectionPt != null &&
    									secondIntersectionPt == null) {
    								//need to check last two segments
    								
    								firstPt = currentContour.elementAt(numPoints - 1);
    								secondPt = currentContour.elementAt(0);
    								
    								foundIntersection = intersects(firstPt, secondPt, startPt, endPt, tempPt);
    								
    								if (foundIntersection) {
    									secondIntersectionPt = new Vector3f(MipavMath.round(tempPt.X), MipavMath.round(tempPt.Y), tempPt.Z);
    									secondIndex = numPoints - 1;
    								}
    							}
    							
    							//if both points were found (no more, no less than 2)
    							if (firstIntersectionPt != null &&
    									secondIntersectionPt != null) {
    								
    								//System.err.println("Found two intersection points: " + firstIntersectionPt + "\n\t" + secondIntersectionPt);
    								
    								//VOIContour firstContour = new VOIContour("firstContour", isClosed);
    								//VOIContour secondContour = new VOIContour("secondContour", isClosed);
                                    VOIContour firstContour = new VOIContour(isClosed);
                                    VOIContour secondContour = new VOIContour(isClosed);
    								
    								//add the second intersection point 1st
    								firstContour.addElement(new Vector3f(secondIntersectionPt.X, secondIntersectionPt.Y, secondIntersectionPt.Z));
    									
    								//check if there are points from second index to 0-index, add those first
    								for (int ptIndex = secondIndex + 1; ptIndex < numPoints; ptIndex++) {
    									firstContour.addElement(currentContour.elementAt(ptIndex));
    								}
    									
    								//add points from 0-index to first index 
    								for (int ptIndex = 0; ptIndex < firstIndex + 1; ptIndex++) {
    									firstContour.addElement(currentContour.elementAt(ptIndex));
    								}
    									
    								//add the first intersection point
    								firstContour.addElement(new Vector3f(firstIntersectionPt.X, firstIntersectionPt.Y, firstIntersectionPt.Z));
    								
    								//repeat for second contour
 
    								//add the first intersection point
    								secondContour.addElement(new Vector3f(firstIntersectionPt.X, firstIntersectionPt.Y, firstIntersectionPt.Z));
    									
    								//add all points between the first and second intersection points
    								for (int ptIndex = firstIndex + 1; ptIndex < secondIndex + 1 && ptIndex < numPoints; ptIndex++) {
    									secondContour.addElement(currentContour.elementAt(ptIndex));
    								}
    									
    								//add the second intersection point
    								secondContour.addElement(new Vector3f(secondIntersectionPt.X, secondIntersectionPt.Y, secondIntersectionPt.Z));
    									
    									
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
    									firstVOI.importCurve(firstContour);
    									secondVOI.importCurve(secondContour);
    								} else {
    									firstVOI.importCurve(secondContour);
    									secondVOI.importCurve(firstContour);
    								}
    								
    								firstIntersectionPt = null;
    								secondIntersectionPt = null;
    						
    								curves[slice].remove(voiBaseIndex);
    								currentContour = null;
    								
    								if (image.getVOIs().VOIAt(voiIndex).isEmpty()) {
    									image.getVOIs().remove(voiIndex);
    								}
    								
    							} else {
    							//	System.err.println("Did not find two intersection points, adding contour ");
    								
    								//intersection NOT found...but must still add contour to either 1st or 2nd VOI
    								currentSide = checkSide(currentContour.elementAt(0));
    									
    						//		System.err.println("current side: " + currentSide);
    								if (currentSide == 1) {
    									//System.err.println("Did not find two intersection points, adding contour to firstVOI" + "\n\n");
    									firstVOI.importCurve(currentContour);
    									
    								} else {
    									//System.err.println("Did not find two intersection points, adding contour to secondVOI" + "\n\n");
    									secondVOI.importCurve(currentContour);
    								}
    								
    								curves[slice].remove(voiBaseIndex);
    								
    								if (image.getVOIs().VOIAt(voiIndex).isEmpty()) {
    									image.getVOIs().remove(voiIndex);
    								}
    								
    							}
    								
    						}
    					}
    				}
    			//image.getVOIs().remove(voiIndex);
    		}
    			
    	}
    	
    	for (int i = image.getVOIs().size() - 1; i >= 0; i--) {
    		if (image.getVOIs().VOIAt(i).isEmpty()) {
    			image.getVOIs().remove(i);
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
        try {
        	setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        }catch(FileNotFoundException e) {
        	 Preferences.debug("Exception ocurred while getting <" + e.getMessage()
                     + ">.  Check that this file is available.\n");
             System.err.println("Exception ocurred while getting <" + e.getMessage()
                     + ">.  Check that this file is available.\n");
        }

        JPanel mainPanel = new JPanel();

        allSlicesRadio = new JRadioButton("Split all VOI(s) in all slices", true);
        
        onlyActiveRadio = new JRadioButton("Only split active VOI(s)/contour(s)", false);
        
        radioGroup = new ButtonGroup();
        radioGroup.add(allSlicesRadio);
        radioGroup.add(onlyActiveRadio);
        
        JButton splitButton = new JButton("Split");
        splitButton.addActionListener(this);
        splitButton.setActionCommand("Split");

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(splitButton);
        
        mainPanel.setLayout(new GridBagLayout());
        mainPanel.setBorder(this.buildTitledBorder("Options"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;

        if (image.getNDims() >2 ) {
        	mainPanel.add(allSlicesRadio, gbc);
        	gbc.gridy++;
        }
        
        mainPanel.add(onlyActiveRadio, gbc);

       

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

    public static boolean intersects(Vector3f p1, Vector3f p2, Vector3f p3, Vector3f p4, Vector3f intersection) {
        double denom = 0f;
        double uAnum = 0f;
        double uBnum = 0f;
        double uA = 0f;
        double uB = 0f;
        denom = ((p4.Y - p3.Y) * (p2.X - p1.X)) - ((p4.X - p3.X) * (p2.Y - p1.Y));
        uAnum = ((p4.X - p3.X) * (p1.Y - p3.Y)) - ((p4.Y - p3.Y) * (p1.X - p3.X));
        uBnum = ((p2.X - p1.X) * (p1.Y - p3.Y)) - ((p2.Y - p1.Y) * (p1.X - p3.X));

        if (denom == 0) {

            // System.err.println("Denom is 0");
            return false;
        }

        uA = uAnum / denom;
        uB = uBnum / denom;

        if ((uA >= 0) && (uA <= 1) && (uB >= 0) && (uB <= 1)) {
            intersection.X = p1.X + (float) (uA * (p2.X - p1.X));
            intersection.Y = p1.Y + (float) (uA * (p2.Y - p1.Y));

            // System.err.println("found intersection to be: " + intersection.X + "," + intersection.Y + "\n\n");
            return true;
        } else {
            return false;
        }
    }
    
}
