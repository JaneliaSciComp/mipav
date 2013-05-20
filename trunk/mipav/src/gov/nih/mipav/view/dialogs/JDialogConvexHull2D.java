package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm for smoothing VOIs. The user can choose whether or not to remove
 * the original selected voi and whether or not to apply trimming to remove nearly collinear points. The user can choose
 * the number of interpolation points. In 2D all selected curves of a selected voi are smoothed. In 3D all selected
 * curves in all slices of a selected voi are smoothed. The original zslice information is only used so that the default
 * number of interpolated points comes from a contour in zslice. If the original voi is not removed, the new voi has a
 * new color. If the original voi is removed, the new voi has the same color.
 */
public class JDialogConvexHull2D extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int defaultPts;
    
    private int elementNum;

    /** DOCUMENT ME! */
    private int groupNum;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int nPts;

    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;

    /** DOCUMENT ME! */
    private AlgorithmBSmooth smoothAlgo;

    /** DOCUMENT ME! */
    private JTextField textInterpNPts;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private boolean trim;

    /** DOCUMENT ME! */
    private JCheckBox trimCheckBox;

    /** DOCUMENT ME! */
    private Color voiColor;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;
    
    private VOIManagerInterface voiManager;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogConvexHull2D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        int nVOI;
        Vector<VOIBase> contours;
        int nContours;

        image = im;
        VOIs = image.getVOIs();

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
        contours = VOIs.VOIAt(groupNum).getCurves();
        nContours = contours.size();

        for (elementNum = 0; elementNum < nContours; elementNum++) {

            if (((VOIContour) (contours.elementAt(elementNum))).isActive()) {
                break;
            }
        }

        if (elementNum == nContours) {

            // Don't think this should happen under normal operations
            dispose();

            return;
        }

        init();
    }
    
    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogConvexHull2D(Frame theParentFrame, VOIManagerInterface voiManager, ModelImage im) {
    	this(theParentFrame,im);
    	this.voiManager = voiManager;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        int i;
        int element;
        Vector<VOIBase> contours;
        int nContours;


        if (source == OKButton) {

            removeOriginal = removeOriginalCheckBox.isSelected();
            
            VOI resultVOI = new VOI(VOIs.VOIAt(groupNum));
            for (i = 0; i < resultVOI.getCurves().size(); i++) {
                ((VOIContour)resultVOI.getCurves().elementAt(i)).convexHull();
            }

            if (removeOriginal) {
                resultVOI.setColor(voiColor);
                resultVOI.setAllActive(true);
                contours = VOIs.VOIAt(groupNum).getCurves();

                int resultIndex = 0;
                nContours = contours.size();
                for (element = nContours - 1; element >= 0; element--) {

                    if (((VOIContour) (contours.elementAt(element))).isActive()) {
                        VOIs.VOIAt(groupNum).removeCurve(contours.elementAt(element));

                        VOIs.VOIAt(groupNum).importCurve((VOIContour) resultVOI.getCurves().elementAt(resultIndex++));
                    }
                }

            } else {
                image.registerVOI(resultVOI);
                //System.err.println("would have registered the new one here");
            }
            
            // Update frame
            if ( voiManager != null )
            {
                voiManager.updateDisplay();
            }
            dispose();
        } else if (source == cancelButton) {
            dispose();
        }
        else if (source == helpButton) {
            //MipavUtil.showHelp("");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * algorithmPerformed - this method is required if the AlgorithmPerformed interface is implemented. It is called by
     * the algorithms when it has completed or failed to to complete, so that the dialog can be display the result image
     * and/or clean up.
     *
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Convex hull 2D VOI");

        JPanel imageVOIPanel = new JPanel(new GridLayout(2, 1));
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("VOI Options"));

        removeOriginalCheckBox = new JCheckBox("Replace Original Contour");
        removeOriginalCheckBox.setFont(serif12);
        removeOriginalCheckBox.setForeground(Color.black);
        removeOriginalCheckBox.setSelected(false);
        imageVOIPanel.add(removeOriginalCheckBox);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(imageVOIPanel, gbc);
        gbc.gridy = 1;

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

}
