package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user search for best boundary between 2 contours
 */
@SuppressWarnings("serial")
public class JDialogDualContourSearch extends JDialogBase implements AlgorithmInterface, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton outerButton;

    /** DOCUMENT ME! */
    private int outerIndex = -1;
    
    private JTextField textContourPoints;
    
    private int contourPoints;
    
    private JTextField textLinePoints;
    
    private int linePoints;
    
    private JTextField textRegularization;
    
    private double regularization;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private JRadioButton innerButton;

    /** DOCUMENT ME! */
    private int innerIndex = -1;
    
    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;


    /** DOCUMENT ME! */
    private AlgorithmDualContourSearch dualAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ButtonGroup VOIGroup;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;
    
    private String[] titles;
    
    private double sigmas[] = new double[]{2.0, 2.0};
    
    private JTextField textGaussX;
    
    private JTextField textGaussY;
    
    private JTextField textContract;
    
    private JTextField textExpand;
    
    private int pixelsContract = 5;
    
    private int pixelsExpand = 5;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogDualContourSearch object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogDualContourSearch(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
        componentImage = ((ViewJFrameImage) parentFrame).getComponentImage();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogDualContourSearch(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
        	componentImage.getVOIHandler().setPresetHue(-1.0f);
            dispose();
        } else if ((source == innerButton) || (source == outerButton)) {

            if (innerButton.isSelected()) {
            	componentImage.getVOIHandler().newVOI(0.0f); // red
            } else if (outerButton.isSelected()) {
            	componentImage.getVOIHandler().newVOI(2.0f / 3.0f); // blue
            } 
        } else {
            super.actionPerformed(event);
        }
    }


    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {


        if (dualAlgo.isCompleted() == true) {

                 
                 if (removeOriginal) {
                     image.getVOIs().removeElementAt(Math.max(innerIndex, outerIndex));
                     image.getVOIs().removeElementAt(Math.min(innerIndex, outerIndex));
                 }

                 // Update frame
                 if ( voiManager != null )
                 {
                     voiManager.algorithmPerformed();
                 }

           } // if(dualAlgo.isCompleted() == true)

             // These next lines set the titles in all frames where the source image is displayed to
             // image name so as to indicate that the image is now unlocked!
             // The image frames are enabled and then registed to the userinterface.
             Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

             for (int i = 0; i < imageFrames.size(); i++) {
             	if ( imageFrames.elementAt(i) instanceof ViewJFrameBase )
             	{
             		((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
             		((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);
             		((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame) (imageFrames.elementAt(i)));
             	}
             }

        dispose();
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {

        componentImage.getVOIHandler().setPresetHue(-1.0f);
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

        try {
        	componentImage.getVOIHandler().setPresetHue(-1.0f);

            

            dualAlgo = new AlgorithmDualContourSearch(image, innerIndex, outerIndex, contourPoints, linePoints,
            		regularization, sigmas, pixelsContract, pixelsExpand);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            dualAlgo.addListener(this);

            createProgressBar(image.getImageName(), dualAlgo);

            // Hide dialog
            setVisible(false);
            
            // These next lines set the titles in all frames where the source image is displayed to
            // "locked - " image name so as to indicate that the image is now read/write locked!
            // The image frames are disabled and then unregisted from the userinterface until the
            // algorithm has completed.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
            titles = new String[imageFrames.size()];

            for (int i = 0; i < imageFrames.size(); i++) {
                titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                ((ViewJFrameBase) parentFrame).getUserInterface().unregisterFrame((Frame)
                                                                                  (imageFrames.elementAt(i)));
            }

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (dualAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                dualAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog DualContourSearch: unable to allocate enough memory");

            return;
        }
    }

    


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JPanel VOIPanel;

        setForeground(Color.black);
        setTitle("Dual Contour Search");

        

        VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("Select inner and outer contours"));

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;

        VOIGroup = new ButtonGroup();
        
        outerButton = new JRadioButton("Add required outer contour", true);
        outerButton.setForeground(Color.blue);
        outerButton.setFont(serif12);
        outerButton.addActionListener(this);
        VOIGroup.add(outerButton);
        VOIPanel.add(outerButton, gbc4);
        componentImage.getVOIHandler().newVOI(2.0f/3.0f); // blue

        innerButton = new JRadioButton("Add required inner contour", false);
        innerButton.setForeground(Color.red);
        innerButton.setFont(serif12);
        innerButton.addActionListener(this);
        VOIGroup.add(innerButton);
        gbc4.gridy = 1;
        VOIPanel.add(innerButton, gbc4);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;


        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel labelContourPoints = new JLabel("Points around contours:");
        labelContourPoints.setForeground(Color.black);
        labelContourPoints.setFont(serif12);
        paramPanel.add(labelContourPoints, gbc);

        textContourPoints = new JTextField();
        textContourPoints.setText("64");
        textContourPoints.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textContourPoints, gbc);
        
        JLabel labelLinePoints = new JLabel("Points along lines between contours:");
        labelLinePoints.setForeground(Color.black);
        labelLinePoints.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelLinePoints, gbc);

        textLinePoints = new JTextField();
        textLinePoints.setText("40");
        textLinePoints.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textLinePoints, gbc);
        
        JLabel labelRegularization = new JLabel("Regularization parameter (0.0-1.0):");
        labelRegularization.setForeground(Color.black);
        labelRegularization.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelRegularization, gbc);
        
        textRegularization = new JTextField();
        textRegularization.setText("0.5");
        textRegularization.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textRegularization, gbc);
        
        JLabel labelGaussX = new JLabel(" X dimension gaussian (0.5 - 5.0) ");
        labelGaussX.setForeground(Color.black);
        labelGaussX.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(labelGaussX, gbc);

        textGaussX = new JTextField();
        textGaussX.setText("2.0");
        textGaussX.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textGaussX, gbc);

        JLabel labelGaussY = new JLabel(" Y dimension gaussian (0.5 - 5.0) ");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(labelGaussY, gbc);

        textGaussY = new JTextField();
        textGaussY.setText("2.0");
        textGaussY.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textGaussY, gbc);
        
        removeOriginalCheckBox = new JCheckBox("Remove Original Contours");
        removeOriginalCheckBox.setFont(serif12);
        removeOriginalCheckBox.setForeground(Color.black);
        removeOriginalCheckBox.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramPanel.add(removeOriginalCheckBox, gbc);
        
        if (image.getNDims() > 2) {
            JLabel labelContract = new JLabel("Number of pixels to contract for next slice ");
            labelContract.setForeground(Color.black);
            labelContract.setFont(serif12);
            gbc.gridx = 0;
            gbc.gridy = 6;
            paramPanel.add(labelContract, gbc);
            
            textContract = new JTextField();
            textContract.setText("5");
            textContract.setFont(serif12);
            gbc.gridx = 1;
            paramPanel.add(textContract, gbc);
            
            JLabel labelExpand = new JLabel("Number of pixels to expand for next slice ");
            labelExpand.setForeground(Color.black);
            labelExpand.setFont(serif12);
            gbc.gridx = 0;
            gbc.gridy = 7;
            paramPanel.add(labelExpand, gbc);
            
            textExpand = new JTextField();
            textExpand.setText("5");
            textExpand.setFont(serif12);
            gbc.gridx = 1;
            paramPanel.add(textExpand, gbc);
        } // if (image.getNDims() > 2)

        getContentPane().add(VOIPanel, BorderLayout.NORTH);
        getContentPane().add(paramPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i;
        int nVOIs;
        float[] hsb;
        float hue;
        String tmpStr;
        
        VOIs = image.getVOIs();

        nVOIs = VOIs.size();

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {

                    if (innerIndex == -1) {
                        innerIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 inner VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if (outerIndex == -1) {
                        outerIndex = i;
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 outer VOI");

                        return false;
                    }
                }  else {
                    MipavUtil.displayError("Contour VOI with illegal hue = " + hue + " is present");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)

        if (innerIndex == -1) {
            MipavUtil.displayError("Must specify an inner VOI");

            return false;
        }

        if (outerIndex == -1) {
        	 MipavUtil.displayError("Must specify an outer VOI");

             return false;	
        }

        

        if (VOIs.VOIAt(innerIndex).getCurves().size() == 0) {
            MipavUtil.displayError("Must have an inner VOI present");

            return false;
        }


        if (VOIs.VOIAt(outerIndex).getCurves().size() == 0) {
            MipavUtil.displayError("Must have an outer VOI present");

            return false;
        }

        tmpStr = textContourPoints.getText();

        if (testParameter(tmpStr, 4, 10000)) {
            contourPoints = Integer.valueOf(tmpStr).intValue();
        } else {
            textContourPoints.requestFocus();
            textContourPoints.selectAll();

            return false;
        }
        
        tmpStr = textLinePoints.getText();

        if (testParameter(tmpStr, 4, 10000)) {
            linePoints = Integer.valueOf(tmpStr).intValue();
        } else {
            textLinePoints.requestFocus();
            textLinePoints.selectAll();

            return false;
        }
        
        tmpStr = textRegularization.getText();
        
        if (testParameter(tmpStr, 0.0, 1.0)) {
        	regularization = Double.valueOf(tmpStr).doubleValue();
        } else {
        	textRegularization.requestFocus();
        	textRegularization.selectAll();
        	
        	return false;
        }
        
        tmpStr = textGaussX.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            sigmas[0] = Double.valueOf(tmpStr).doubleValue();
        } else {
            textGaussX.requestFocus();
            textGaussX.selectAll();

            return false;
        }

        tmpStr = textGaussY.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            sigmas[1] = Double.valueOf(tmpStr).doubleValue();
        } else {
            textGaussY.requestFocus();
            textGaussY.selectAll();

            return false;
        }
        
        removeOriginal = removeOriginalCheckBox.isSelected();
        
        if (image.getNDims() > 2) {
        	tmpStr = textContract.getText();
        	if (testParameter(tmpStr, 0, 100)) {
        		pixelsContract = Integer.valueOf(tmpStr).intValue();
        	}
        	else {
        		textContract.requestFocus();
        		textContract.selectAll();
        		
        		return false;
        	}
        	
        	tmpStr = textExpand.getText();
        	if (testParameter(tmpStr, 0, 100)) {
        		pixelsExpand = Integer.valueOf(tmpStr).intValue();
        	}
        	else {
        		textExpand.requestFocus();
        		textExpand.selectAll();
        		
        		return false;
        	}
        } // if (image.getNDims() > 2)
        

        return true;
    }

}
