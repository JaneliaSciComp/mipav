package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create Hough transform with p, q, r1, r2, and theta output for ellipse detection in
 * binary image, where p is the x coordinate of the ellipse center, q is the y coordinate of the
 * ellipse center, r1 is the semimajor axis (or major radius), r2 is the semiminor axis (or
 * minor radius), and theta is the angle of the major axis with the x axis.
 */
public class JDialogHoughEllipse extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmHoughEllipse hAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** Minimum percentage of the perimiter of a found ellipse that must be covered by points for it to be valid. */
    private double minCoverage;

    /** DOCUMENT ME! */
    private JTextField minCoverageText;

    /** Maximum number of points to take from each side of a point on a curve in determining a tangent */
    private int sidePointsForTangent;
    
    private JTextField sideText;
    
    private JTextField pixelWidthText;
    
    /** For xCenter, yCenter, r1, and r2 must bin width <= maxPixelBinWidth */
    private double maxPixelBinWidth;
    
    private JTextField degreesWidthText;
    
    /** For theta must have bin width <= maxDegreesBinWidth */
    private double maxDegreesBinWidth;
    
    /** number of ellipses to be found */
    private int numEllipses;
    
    private JTextField numEllipsesText;
    
    /** Smallest allowable distance between 2 of 3 picked points */
    private double minPointDistance;
    
    private JTextField minPointText;
    
    /** Largest allowable distance between 2 of 3 picked points */
    private double maxPointDistance;
    
    private JTextField maxPointText;
    
    /** Number of point triplets required before each ellipse find is performed */
    private int pointSetsRequired;
    
    private JTextField pointSetsText;
    
    /** Number of counts required to find an ellipse */
    private int countThreshold;
    
    private JTextField countText;
    
    /** Maximum percent by which perimiter pixels can deviate from the ellipse equation */
    private double ellipseRangeTolerance;
    
    private JTextField toleranceText;
    
    private JTextField maxCyclesText;
    
    /** Maximum number of pointSetsRequired triplet point acqusitions that is allowed to occur */
    private int maxEllipseFindCycles;
    
    private JTextField maxBufferText;
    
    /** The maximum Hough transform size in megabytes - default is currently 256 */
    private int maxBufferSize;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughEllipse object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogHoughEllipse(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogHoughEllipse(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("HoughEllipse003");
        } else if (command.equals("Cancel")) {
            dispose();
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


        if (algorithm instanceof AlgorithmHoughEllipse) {
            Preferences.debug("Hough Ellipse: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((hAlgo.isCompleted() == true) && (resultImage != null)) {


                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            // insertScriptLine(algorithm);
        } // if (algorithm instanceof AlgorithmHoughEllipse)

        if (hAlgo != null) {
            hAlgo.finalize();
            hAlgo = null;
        }

        dispose();
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        
    }


    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    private void callAlgorithm() {

        try {
            String name = makeImageName(image.getImageName(), "_hough_ellipse");
            resultImage = new ModelImage(image.getType(), image.getExtents(), name);
            resultImage.setImageName(name);

            // Make algorithm
            hAlgo = new AlgorithmHoughEllipse(resultImage, image, minCoverage, sidePointsForTangent, maxPixelBinWidth,
                                              maxDegreesBinWidth, minPointDistance,
                                              maxPointDistance, pointSetsRequired, countThreshold,
                                              ellipseRangeTolerance, numEllipses, 
                                              maxEllipseFindCycles, maxBufferSize);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            hAlgo.addListener(this);
            
            createProgressBar(image.getImageName(), hAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (hAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                hAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Hough Ellipse: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel mainLabel;
        JLabel mainLabel2;
        JLabel minCoverageLabel;
        JLabel sideLabel;
        JLabel pixelDiffLabel;
        JLabel degreesWidthLabel;
        JLabel minPointLabel;
        JLabel maxPointLabel;
        JLabel pointSetsLabel;
        JLabel countLabel;
        JLabel toleranceLabel;
        JLabel numEllipsesLabel;
        JLabel maxCyclesLabel;
        JLabel maxBufferLabel;
        setForeground(Color.black);
        setTitle("Hough transform for ellipse detection");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Hough transform parameters"));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mainLabel = new JLabel("((y-q)*sin(theta) + (x-p)*cos(theta))^2/r1^2 + ");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        paramPanel.add(mainLabel, gbc);
        
        mainLabel2 = new JLabel("((y-q)*cos(theta) - (x-p)*sin(theta))^2/r2^2 = 1 ");
        mainLabel2.setForeground(Color.black);
        mainLabel2.setFont(serif12);
        mainLabel2.setEnabled(true);
        gbc.gridy = 1;
        paramPanel.add(mainLabel2, gbc);

        minCoverageLabel = new JLabel("Minimum percentage of ellipse perimiter with points ");
        minCoverageLabel.setForeground(Color.black);
        minCoverageLabel.setFont(serif12);
        minCoverageLabel.setEnabled(true);
        gbc.gridy = 2;
        paramPanel.add(minCoverageLabel, gbc);

        minCoverageText = new JTextField(10);
        minCoverageText.setText("30.0");
        minCoverageText.setFont(serif12);
        minCoverageText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(minCoverageText, gbc);

        sideLabel = new JLabel("Maximum curve points on each side for tangent ");
        sideLabel.setForeground(Color.black);
        sideLabel.setFont(serif12);
        sideLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(sideLabel, gbc);

        sideText = new JTextField(10);
        sideText.setText("3");
        sideText.setFont(serif12);
        sideText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(sideText, gbc);
        
        pixelDiffLabel = new JLabel("Desired maximum pixel difference for p, q, r1, or r2 value ");
        pixelDiffLabel.setForeground(Color.black);
        pixelDiffLabel.setFont(serif12);
        pixelDiffLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(pixelDiffLabel, gbc);
        
        pixelWidthText = new JTextField(10);
        pixelWidthText.setText("2.0");
        pixelWidthText.setFont(serif12);
        pixelWidthText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(pixelWidthText, gbc);
        
        degreesWidthLabel = new JLabel("Desired maximum degrees difference for theta value ");
        degreesWidthLabel.setForeground(Color.black);
        degreesWidthLabel.setFont(serif12);
        degreesWidthLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramPanel.add(degreesWidthLabel, gbc);
        
        degreesWidthText = new JTextField(10);
        degreesWidthText.setText("3.0");
        degreesWidthText.setFont(serif12);
        degreesWidthText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(degreesWidthText, gbc);
        
        minPointLabel = new JLabel("Minimum distance between 2 of 3 picked points ");
        minPointLabel.setForeground(Color.black);
        minPointLabel.setFont(serif12);
        minPointLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 6;
        paramPanel.add(minPointLabel, gbc);
        
        minPointText = new JTextField(10);
        minPointText.setText("3.0");
        minPointText.setFont(serif12);
        minPointText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(minPointText, gbc);
        
        maxPointLabel = new JLabel("Maximum distance between 2 of 3 picked points ");
        maxPointLabel.setForeground(Color.black);
        maxPointLabel.setFont(serif12);
        maxPointLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 7;
        paramPanel.add(maxPointLabel, gbc);
        
        maxPointText = new JTextField(10);
        maxPointText.setText("100.0");
        maxPointText.setFont(serif12);
        maxPointText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(maxPointText, gbc);
        
        pointSetsLabel = new JLabel("Point triplets required per ellipse find ");
        pointSetsLabel.setForeground(Color.black);
        pointSetsLabel.setFont(serif12);
        pointSetsLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 8;
        paramPanel.add(pointSetsLabel, gbc);
        
        pointSetsText = new JTextField(10);
        pointSetsText.setText("1000");
        pointSetsText.setFont(serif12);
        pointSetsText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(pointSetsText, gbc);
        
        countLabel = new JLabel("Counts in a p,q,r1,r2,theta bin required for an ellipse find ");
        countLabel.setForeground(Color.black);
        countLabel.setFont(serif12);
        countLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 9;
        paramPanel.add(countLabel, gbc);
        
        countText = new JTextField(10);
        countText.setText("2");
        countText.setFont(serif12);
        countText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(countText, gbc);
        
        toleranceLabel = new JLabel("Maximum percent deviation for perimiter pixels ");
        toleranceLabel.setForeground(Color.black);
        toleranceLabel.setFont(serif12);
        toleranceLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 10;
        paramPanel.add(toleranceLabel, gbc);
        
        toleranceText = new JTextField(10);
        toleranceText.setText("15.0");
        toleranceText.setFont(serif12);
        toleranceText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(toleranceText, gbc);
        
        numEllipsesLabel = new JLabel("Number of ellipses ");
        numEllipsesLabel.setForeground(Color.black);
        numEllipsesLabel.setFont(serif12);
        numEllipsesLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 11;
        paramPanel.add(numEllipsesLabel, gbc);
        
        numEllipsesText = new JTextField(5);
        numEllipsesText.setText("1");
        numEllipsesText.setFont(serif12);
        numEllipsesText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(numEllipsesText, gbc);
        
        maxCyclesLabel = new JLabel("Maximum number of ellipse find cycles ");
        maxCyclesLabel.setForeground(Color.black);
        maxCyclesLabel.setFont(serif12);
        maxCyclesLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 12;
        paramPanel.add(maxCyclesLabel, gbc);
        
        maxCyclesText = new JTextField(10);
        maxCyclesText.setText("80");
        maxCyclesText.setFont(serif12);
        maxCyclesText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(maxCyclesText, gbc);
        
        maxBufferLabel = new JLabel("Maximum Hough transform in megabytes ");
        maxBufferLabel.setForeground(Color.black);
        maxBufferLabel.setFont(serif12);
        maxBufferLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 13;
        paramPanel.add(maxBufferLabel, gbc);
        
        maxBufferText = new JTextField(10);
        maxBufferText.setText("256");
        maxBufferText.setFont(serif12);
        maxBufferText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(maxBufferText, gbc);

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

        if (!testParameter(minCoverageText.getText(), 0.0, 100.0)) {
            minCoverageText.requestFocus();
            minCoverageText.selectAll();

            return false;
        } else {
            minCoverage = Double.valueOf(minCoverageText.getText()).doubleValue();
        }

        if (!testParameter(sideText.getText(), 1, 10)) {
            sideText.requestFocus();
            sideText.selectAll();

            return false;
        } else {
            sidePointsForTangent = Integer.valueOf(sideText.getText()).intValue();
        }
        
        if (!testParameter(pixelWidthText.getText(), 0.1, 20.0)) {
            pixelWidthText.requestFocus();
            pixelWidthText.selectAll();

            return false;
        } else {
            maxPixelBinWidth = Double.valueOf(pixelWidthText.getText()).doubleValue();
        }
        
        if (!testParameter(degreesWidthText.getText(), 0.1, 20.0)) {
            degreesWidthText.requestFocus();
            degreesWidthText.selectAll();

            return false;
        } else {
            maxDegreesBinWidth = Double.valueOf(degreesWidthText.getText()).doubleValue();
        }
        
        if (!testParameter(minPointText.getText(), 1.0, Math.max(image.getExtents()[0],image.getExtents()[1])/2.0)) {
            minPointText.requestFocus();
            minPointText.selectAll();

            return false;
        } else {
            minPointDistance = Double.valueOf(minPointText.getText()).doubleValue();
        }
        
        if (!testParameter(maxPointText.getText(), minPointDistance, Math.max(image.getExtents()[0],image.getExtents()[1]))) {
            maxPointText.requestFocus();
            maxPointText.selectAll();

            return false;
        } else {
            maxPointDistance = Double.valueOf(maxPointText.getText()).doubleValue();
        }
        
        if (!testParameter(pointSetsText.getText(), 10, 1000)) {
            pointSetsText.requestFocus();
            pointSetsText.selectAll();

            return false;
        } else {
            pointSetsRequired = Integer.valueOf(pointSetsText.getText()).intValue();
        }
        
        if (!testParameter(countText.getText(), 1, 1000)) {
            countText.requestFocus();
            countText.selectAll();

            return false;
        } else {
            countThreshold = Integer.valueOf(countText.getText()).intValue();
        }
        
        if (!testParameter(toleranceText.getText(), 1.0, 20.0)) {
            toleranceText.requestFocus();
            toleranceText.selectAll();

            return false;
        } else {
            ellipseRangeTolerance = Double.valueOf(toleranceText.getText()).doubleValue();
        }
        
        if (!testParameter(numEllipsesText.getText(), 1, 100)) {
            numEllipsesText.requestFocus();
            numEllipsesText.selectAll();

            return false;
        } else {
            numEllipses = Integer.valueOf(numEllipsesText.getText()).intValue();
        }
        
        if (!testParameter(maxCyclesText.getText(), 1, 1000000)) {
            maxCyclesText.requestFocus();
            maxCyclesText.selectAll();

            return false;
        } else {
            maxEllipseFindCycles = Integer.valueOf(maxCyclesText.getText()).intValue();
        }
        
        if (!testParameter(maxBufferText.getText(), 1, 10000)) {
            maxBufferText.requestFocus();
            maxBufferText.selectAll();

            return false;
        } else {
            maxBufferSize = Integer.valueOf(maxBufferText.getText()).intValue();
        }

        return true;
    }
}
