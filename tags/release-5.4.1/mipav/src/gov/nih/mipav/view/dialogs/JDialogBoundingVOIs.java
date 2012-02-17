package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class JDialogBoundingVOIs extends JDialogBase {

    /**  */
    //private static final long serialVersionUID;
    

    /** DOCUMENT ME! */
    private int groupNum;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;

    /** DOCUMENT ME! */
    private VOI srcVOI;

    /** DOCUMENT ME! */
    private Color voiColor;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs = null;
    
    private JCheckBox innerCurveCheckBox;
    
    private boolean doInner = true;
    
    private JLabel innerDistanceLabel;
    
    private JTextField innerDistanceText;
    
    private float innerDistance = 2.0f;
    
    private JCheckBox outerCurveCheckBox;
    
    private boolean doOuter = true;
    
    private JLabel outerDistanceLabel;
    
    private JTextField outerDistanceText;
    
    private float outerDistance = 2.0f;
    
    private float maxDistance;
    
    /** Maximum number of points to take from each side of a point on a curve in determining a tangent */
    private int sidePointsForTangent;
    
    private JTextField sideText;
    
    private JLabel sideLabel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image.
     */
    public JDialogBoundingVOIs(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        VOIs = im.getVOIs();

        int nVOI;

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (groupNum = 0; groupNum < nVOI; groupNum++) {

            if ((VOIs.VOIAt(groupNum).isActive() == true) && (VOIs.VOIAt(groupNum).getCurveType() == VOI.CONTOUR)) {
                break;
            }
        }

        if (groupNum == nVOI) {
            MipavUtil.displayError("VOI must be selected");
            dispose();

            return;
        }

        voiColor = VOIs.VOIAt(groupNum).getColor();
        srcVOI = VOIs.VOIAt(groupNum);
        image = im;
        init();
    }
    

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image.
     */
    public JDialogBoundingVOIs(Frame theParentFrame, ModelImage im, boolean separateThread) {
    	this(theParentFrame, im);
        setSeparateThread(separateThread);   	
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;

        if (source == OKButton) {

            removeOriginal = removeOriginalCheckBox.isSelected();
            
            maxDistance = (float)(Math.min(image.getExtents()[0], image.getExtents()[1])/2.0);
            
            doInner = innerCurveCheckBox.isSelected();
            
            if (doInner) {

	            tmpStr = innerDistanceText.getText();
	
	            if (testParameter(tmpStr, 1, maxDistance)) {
	                innerDistance = Float.valueOf(tmpStr).floatValue();
	            } else {
	                innerDistanceText.requestFocus();
	                innerDistanceText.selectAll();
	
	                return;
	            }
            } // if (doInner)

            doOuter = outerCurveCheckBox.isSelected();
            
            if ((!doInner) && (!doOuter)) {
            	MipavUtil.displayError("Must select inner or outer or both check boxes");
            	return;
            }
            
            if (doOuter) {

	            tmpStr = outerDistanceText.getText();
	
	            if (testParameter(tmpStr, 1, maxDistance)) {
	                outerDistance = Float.valueOf(tmpStr).floatValue();
	            } else {
	                outerDistanceText.requestFocus();
	                outerDistanceText.selectAll();
	
	                return;
	            }
            } // if (doOuter)
            
            tmpStr = sideText.getText();
            if (testParameter(tmpStr, 1, 10)) {
            	sidePointsForTangent = Integer.valueOf(tmpStr).intValue();
            }
            else {
            	sideText.requestFocus();
            	sideText.selectAll();
            	return;
            }

            VOIBaseVector curves = srcVOI.getCurves();
            VOIBase srcContour = null;
            for ( int i = 0; i < curves.size(); i++ )
            {
                if ( curves.elementAt(i).isActive() )
                {
                    srcContour = curves.elementAt(i);
                    generateBoundaryContours( srcContour );
                }
            }

            // Update frame
            ((ViewJFrameBase) parentFrame).updateImages(true);
            if ( voiManager != null )
            {
                voiManager.algorithmPerformed();
            }
            dispose();
        } else if (source == innerCurveCheckBox) {
        	if (innerCurveCheckBox.isSelected()) {
        		innerDistanceLabel.setEnabled(true);
        		innerDistanceText.setEnabled(true);
        	}
        	else {
        		innerDistanceLabel.setEnabled(false);
        		innerDistanceText.setEnabled(false);
        	}
        } else if (source == outerCurveCheckBox) {
        	if (outerCurveCheckBox.isSelected()) {
        		outerDistanceLabel.setEnabled(true);
        		outerDistanceText.setEnabled(true);
        	}
        	else {
        		outerDistanceLabel.setEnabled(false);
        		outerDistanceText.setEnabled(false);
        	}
        } else if (source == cancelButton) {
            dispose();
        }
        else if (source == helpButton) {
            //MipavUtil.showHelp("");
        }
    }

    public void focusLost(FocusEvent event) {
        super.focusLost(event);
    }

    /**
     * @param  event  Event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        
    }
    
    private void generateBoundaryContours ( VOIBase srcContour )
    {
    	
        int index;
        int i;
        int j;
        int k;
        int m;
        if ( srcContour == null || (srcContour.size() == 0) )
        {
            return;
        }
        int slice = (int)srcContour.elementAt(0).Z;
        
        int nPoints = srcContour.size();
        Vector3f point;
        float xPoints[] = new float[nPoints + 2*sidePointsForTangent];
        float yPoints[] = new float[nPoints + 2*sidePointsForTangent];
        Vector<Vector3f> innerV = new Vector<Vector3f>();
        Vector<Vector3f> outerV = new Vector<Vector3f>();
        float tangentX;
        float tangentY;
        float xCenteredPoints[] = new float[2*sidePointsForTangent+1];
        float yCenteredPoints[] = new float[2*sidePointsForTangent+1];
        double xSqSum;
        double ySqSum;
        double xySum;
        double var;
        double x1t;
        double x2t;
        double y1t;
        double y2t;
        double slope;
        double d1;
        double d2;
        double xDist;
        double yDist;
        for (i = 0; i < nPoints; i++) {
        	point = (srcContour.get(i));
            xPoints[i + sidePointsForTangent] = point.X;
            yPoints[i + sidePointsForTangent] = point.Y;	
        }
        for (i = sidePointsForTangent - 1, j = 0; i >= 0; i--, j++) {
        	xPoints[i] = xPoints[nPoints - 1 - j];
        	yPoints[i] = yPoints[nPoints - 1 - j];
        }
        for (i = nPoints, j = 0; i <= nPoints + sidePointsForTangent - 1; i++, j++) {
        	xPoints[i] = xPoints[j];
        	yPoints[i] = yPoints[j];
        }
        for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent + nPoints - 1; i++, j++) {
        	if (sidePointsForTangent == 1) {
                tangentX = (xPoints[i+1] - xPoints[i-1])/2.0f;
                tangentY = (yPoints[i+1] - yPoints[i-1])/2.0f;
                if (tangentY == 0.0f) {
                    slope = Double.POSITIVE_INFINITY;
                }
                else {
                    slope = -tangentX/tangentY;
                }
            } // if (sidePointsForTangent == 1)
        	else { // sidePointsForTangent > 1
        		// Center all points for tangent point touching curve at (0, 0)
                // That is, use an x axis and a y axis going thru the tangent point
        		for (k = 0, m = i - sidePointsForTangent; m <= i + sidePointsForTangent; m++, k++) {
                    xCenteredPoints[k] = xPoints[m] - xPoints[i];
                    yCenteredPoints[k] = yPoints[m] - yPoints[i];
                }
        		xSqSum = 0.0;
                ySqSum = 0.0;
                xySum = 0.0;
                for (k = 0; k < xCenteredPoints.length; k++) {
                    xSqSum += xCenteredPoints[k]*xCenteredPoints[k];
                    ySqSum += yCenteredPoints[k]*yCenteredPoints[k];
                    xySum += xCenteredPoints[k]*yCenteredPoints[k];
                }
                if (xySum != 0.0) {
                    var = Math.sqrt(ySqSum*ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
                    x1t = 0.5 * ((-ySqSum + xSqSum + var)/xySum);
                    x2t = 0.5 * ((-ySqSum + xSqSum - var)/xySum);
                    y1t = 1.0;
                    y2t = 1.0;
                }
                else {
                    // If all points are symmetric to either this new x axis or this new y axis, then
                    // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                    x1t = 1.0;
                    x2t = 0.0;
                    y1t = 0.0;
                    y2t = 1.0;
                }
                // x1t, y1t and x2t, y2t are perpindicular.  To find the solution, calculate the sum of
                // distances from the curve points to the line for the 2 cases
                // The shortest distance is the correct solution
                // Distance from AX + BY + C = 0 to P1 is 
                // abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
                // Here A = slope, B = -1, and C = 0.
                d1 = 0.0;
                for (k = 0; k < xCenteredPoints.length; k++) {
                    if (x1t == 0.0) {
                        // Infinite slope thru (0,0)
                        d1 += Math.abs(yCenteredPoints[k]);
                    }
                    else if (y1t == 0.0) {
                        // Zero slope thru (0, 0)
                        d1 += Math.abs(xCenteredPoints[k]);
                    }
                    else {
                        slope = y1t/x1t;
                        d1 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k])/Math.sqrt(slope*slope + 1));
                    }
                }
                d2 = 0.0;
                for (k = 0; k < xCenteredPoints.length; k++) {
                    if (x2t == 0.0) {
                        // Infinite slope thru (0,0)
                        d2 += Math.abs(yCenteredPoints[k]);
                    }
                    else if (y2t == 0.0) {
                        // Zero slope thru (0, 0)
                        d2 += Math.abs(xCenteredPoints[k]);
                    }
                    else {
                        slope = y2t/x2t;
                        d2 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k])/Math.sqrt(slope*slope + 1));
                    }
                }
                if (d1 < d2) {
                    tangentX = (float)x1t;
                    tangentY = (float)y1t;
                }
                else {
                    tangentX = (float)x2t;
                    tangentY = (float)y2t;
                }
                if (tangentY == 0.0f) {
                    slope = Double.POSITIVE_INFINITY;
                    
                }
                else {
                    slope = -tangentX/tangentY;
                }    
        	} // else sidePointsForTangent > 1
        	if (doInner) {
        		if (Double.isInfinite(slope)) {
        			if (srcContour.contains(xPoints[i], yPoints[i] + innerDistance)) {
        		         point = new Vector3f(xPoints[i], yPoints[i] + innerDistance, slice);
        		         innerV.add(point);
        			}
        			else if (srcContour.contains(xPoints[i], yPoints[i] - innerDistance)) {
        				point = new Vector3f(xPoints[i], yPoints[i] - innerDistance, slice);
       		            innerV.add(point);	
        			}
        		} // if (Double.isInfinite(slope))
        		else {
        		    xDist = innerDistance/Math.sqrt(1.0 + slope*slope);
        		    yDist = xDist*slope;
        		    if (srcContour.contains((float)(xPoints[i] + xDist), (float)(yPoints[i] + yDist))) {
       		           point = new Vector3f((float)(xPoints[i] + xDist), (float)(yPoints[i] + yDist), slice);
       		           innerV.add(point);
       			    }
       			    else if (srcContour.contains((float)(xPoints[i] - xDist), (float)(yPoints[i] - yDist))) {
       				    point = new Vector3f((float)(xPoints[i] - xDist), (float)(yPoints[i] - yDist), slice);
      		            innerV.add(point);	
       			    }
        		}
        	} // if (doInner)
        	if (doOuter) {
        		if (Double.isInfinite(slope)) {
        			if (!srcContour.contains(xPoints[i], yPoints[i] + outerDistance)) {
        		         point = new Vector3f(xPoints[i], yPoints[i] + outerDistance, slice);
        		         outerV.add(point);
        			}
        			else if (!srcContour.contains(xPoints[i], yPoints[i] - outerDistance)) {
        				point = new Vector3f(xPoints[i], yPoints[i] - outerDistance, slice);
       		            outerV.add(point);	
        			}
        		} // if (Double.isInfinite(slope))
        		else {
        		    xDist = outerDistance/Math.sqrt(1.0 + slope*slope);
        		    yDist = xDist*slope;
        		    if (!srcContour.contains((float)(xPoints[i] + xDist), (float)(yPoints[i] + yDist))) {
       		           point = new Vector3f((float)(xPoints[i] + xDist), (float)(yPoints[i] + yDist), slice);
       		           outerV.add(point);
       			    }
       			    else if (!srcContour.contains((float)(xPoints[i] - xDist), (float)(yPoints[i] - yDist))) {
       				    point = new Vector3f((float)(xPoints[i] - xDist), (float)(yPoints[i] - yDist), slice);
      		            outerV.add(point);	
       			    }
        		}
        	} // if (doOuter)
        } // for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent + nPoints - 1; i++, j++)
        
        short sID = (short)(image.getVOIs().getUniqueID());
        String kName = srcContour.getClass().getName();
        index = kName.lastIndexOf('.') + 1;
        kName = kName.substring(index);
        VOI resultVOI = new VOI(sID, kName + "_" + sID, srcContour.getType(), -1 );
        if (doInner) {
        	Vector3f pt[] = new Vector3f[innerV.size()];
        	for (i = 0; i < innerV.size(); i++) {
        		pt[i] = innerV.elementAt(i);
        	}
        	resultVOI.importCurve(pt);
        }
        if (doOuter) {
        	Vector3f pt[] = new Vector3f[outerV.size()];
        	for (i = 0; i < outerV.size(); i++) {
        		pt[i] = outerV.elementAt(i);
        	}
        	resultVOI.importCurve(pt);
        }
       
        if (removeOriginal) {
            image.getVOIs().removeElementAt(groupNum);
        }
        image.registerVOI( resultVOI );
        
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Bounding VOIs");
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel curvePanel = new JPanel(new GridBagLayout());
        curvePanel.setForeground(Color.black);
        curvePanel.setBorder(buildTitledBorder("Curve specifications"));
        
        removeOriginalCheckBox = new JCheckBox("Remove original selected VOI", false);
        removeOriginalCheckBox.setFont(serif12);
        removeOriginalCheckBox.setForeground(Color.black);
        curvePanel.add(removeOriginalCheckBox, gbc);
        
        innerCurveCheckBox = new JCheckBox("Inner curve", true);
        innerCurveCheckBox.setFont(serif12);
        innerCurveCheckBox.setForeground(Color.black);
        innerCurveCheckBox.addActionListener(this);
        gbc.gridy = 1;
        curvePanel.add(innerCurveCheckBox, gbc);
        
        innerDistanceLabel = new JLabel("Enter distance from VOI in pixels ");
        innerDistanceLabel.setForeground(Color.black);
        innerDistanceLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        curvePanel.add(innerDistanceLabel, gbc);
        
        innerDistanceText = new JTextField(10);
        innerDistanceText.setText("2.0");
        innerDistanceText.setFont(serif12);
        gbc.gridx = 1;
        curvePanel.add(innerDistanceText, gbc);
        
        outerCurveCheckBox = new JCheckBox("Outer curve", true);
        outerCurveCheckBox.setFont(serif12);
        outerCurveCheckBox.setForeground(Color.black);
        outerCurveCheckBox.addActionListener(this);
        gbc.gridx = 0;
        gbc.gridy = 3;
        curvePanel.add(outerCurveCheckBox, gbc);
        
        outerDistanceLabel = new JLabel("Enter distance from VOI in pixels ");
        outerDistanceLabel.setForeground(Color.black);
        outerDistanceLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 4;
        curvePanel.add(outerDistanceLabel, gbc);
        
        outerDistanceText = new JTextField(10);
        outerDistanceText.setText("2.0");
        outerDistanceText.setFont(serif12);
        gbc.gridx = 1;
        curvePanel.add(outerDistanceText, gbc);

        sideLabel = new JLabel("Curve points on each side for tangent ");
        sideLabel.setForeground(Color.black);
        sideLabel.setFont(serif12);
        sideLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 5;
        curvePanel.add(sideLabel, gbc);

        sideText = new JTextField(10);
        sideText.setText("1");
        sideText.setFont(serif12);
        sideText.setEnabled(true);
        gbc.gridx = 1;
        curvePanel.add(sideText, gbc);


        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));

        
        mainPanel.add(curvePanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }
}
