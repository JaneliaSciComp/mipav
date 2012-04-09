package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create an image with randomly spaced, aggregated, regular or uniform, or constrained circles of the same size
 */
public class JDialogCircleGeneration extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmCircleGeneration cAlgo = null;

    private int extents[];

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int xDim = 2048;

    /** DOCUMENT ME! */
    private JTextField xDimText;

    /** DOCUMENT ME! */
    private int yDim = 2048;
    
    private JTextField yDimText;
    
    private int radius = 5;

    /** DOCUMENT ME! */
    private JTextField radiusText;
    
    private int numCircles = 200;
    
    private JTextField numCirclesText;
    
    private ButtonGroup patternGroup;
    
    private JRadioButton randomButton;
    
    private JRadioButton aggregatedButton;
    
    private JRadioButton aggregatedCirclesDifferentSizesButton;
    
    private JRadioButton aggregatedEllipseButton;
    
    private JRadioButton aggregatedEllipseRandomOrientationButton;
    
    private JRadioButton aggregatedEllipseRandomOrientationDifferentSizes;
    
    private JRadioButton regularButton;
    
    private JRadioButton constrainedButton;
    
    private int pattern = AlgorithmCircleGeneration.RANDOM;
    
    private JLabel initialCirclesLabel;
    
    private JTextField initialCirclesText;
    
    private int initialRandomCircles = 20;
    
    private JLabel minimumDistanceLabel;
    
    private JTextField minimumDistanceText;
    
    private double minimumNearestNeighborDistance = 12.0;
    
    private JLabel maximumDistanceLabel;
    
    private JTextField maximumDistanceText;
    
    private double maximumNearestNeighborDistance = 15.0;
    
    private JLabel semiMajorAxisLabel;
    
    private JTextField semiMajorAxisText;
    
    private double semiMajorAxis = 	15.0;
    
    private JLabel semiMinorAxisLabel;
    
    private JTextField semiMinorAxisText;
    
    private JLabel phiLabel;
    
    private JTextField phiText;
    
    private double phi = 0.0;
    
    private double semiMinorAxis = 	10.0;
    
    private JLabel lowestForbiddenLabel;
    
    private JTextField lowestForbiddenText;
    
    private double lowestForbiddenNNDistance = 15.0;
    
    private JLabel highestForbiddenLabel;
    
    private JTextField highestForbiddenText;
    
    private double highestForbiddenNNDistance = 20.0;
    
    private JLabel highestRegenerationLabel;
    
    private JTextField highestRegenerationText;
    
    private double highestRegenerationNNDistance = 25.0;
    
    private JLabel ratioLabel;
    
    private JTextField ratioText;
    
    private double smallestToLargestAxisRatio = 0.1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogCircleGeneration(Frame theParentFrame) {
        super(theParentFrame, false);
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
            dispose();
        } else if ((source == randomButton) || (source == aggregatedButton) || (source == aggregatedEllipseButton) ||
        		   (source == aggregatedCirclesDifferentSizesButton) ||
        		   (source == aggregatedEllipseRandomOrientationButton) || 
        		   (source == aggregatedEllipseRandomOrientationDifferentSizes) ||
        		   (source == regularButton) || (source == constrainedButton)) {
            if (randomButton.isSelected()) {
                initialCirclesLabel.setEnabled(false);
                initialCirclesText.setEnabled(false);
                minimumDistanceLabel.setEnabled(false);
                minimumDistanceText.setEnabled(false);
                maximumDistanceLabel.setEnabled(false);
                maximumDistanceText.setEnabled(false);
                lowestForbiddenLabel.setEnabled(false);
                lowestForbiddenText.setEnabled(false);
                highestForbiddenLabel.setEnabled(false);
                highestForbiddenText.setEnabled(false);
                highestRegenerationLabel.setEnabled(false);
                highestRegenerationText.setEnabled(false);
                semiMajorAxisLabel.setEnabled(false);
                semiMajorAxisText.setEnabled(false);
                semiMinorAxisLabel.setEnabled(false);
                semiMinorAxisText.setEnabled(false);
                phiLabel.setEnabled(false);
                phiText.setEnabled(false);
                ratioLabel.setEnabled(false);
                ratioText.setEnabled(false);
            }
            else if (aggregatedButton.isSelected()) {
                initialCirclesLabel.setEnabled(true);
                initialCirclesText.setEnabled(true);
                minimumDistanceLabel.setEnabled(false);
                minimumDistanceText.setEnabled(false);
                maximumDistanceLabel.setEnabled(true);
                maximumDistanceText.setEnabled(true);
                lowestForbiddenLabel.setEnabled(false);
                lowestForbiddenText.setEnabled(false);
                highestForbiddenLabel.setEnabled(false);
                highestForbiddenText.setEnabled(false);
                highestRegenerationLabel.setEnabled(false);
                highestRegenerationText.setEnabled(false);
                semiMajorAxisLabel.setEnabled(false);
                semiMajorAxisText.setEnabled(false);
                semiMinorAxisLabel.setEnabled(false);
                semiMinorAxisText.setEnabled(false);
                phiLabel.setEnabled(false);
                phiText.setEnabled(false);
                ratioLabel.setEnabled(false);
                ratioText.setEnabled(false);
            }
            else if (aggregatedCirclesDifferentSizesButton.isSelected()) {
                initialCirclesLabel.setEnabled(true);
                initialCirclesText.setEnabled(true);
                minimumDistanceLabel.setEnabled(false);
                minimumDistanceText.setEnabled(false);
                maximumDistanceLabel.setEnabled(true);
                maximumDistanceText.setEnabled(true);
                lowestForbiddenLabel.setEnabled(false);
                lowestForbiddenText.setEnabled(false);
                highestForbiddenLabel.setEnabled(false);
                highestForbiddenText.setEnabled(false);
                highestRegenerationLabel.setEnabled(false);
                highestRegenerationText.setEnabled(false);
                semiMajorAxisLabel.setEnabled(false);
                semiMajorAxisText.setEnabled(false);
                semiMinorAxisLabel.setEnabled(false);
                semiMinorAxisText.setEnabled(false);
                phiLabel.setEnabled(false);
                phiText.setEnabled(false);
                ratioLabel.setEnabled(true);
                ratioText.setEnabled(true);
            }
            else if (aggregatedEllipseButton.isSelected()) {
            	initialCirclesLabel.setEnabled(true);
                initialCirclesText.setEnabled(true);
                minimumDistanceLabel.setEnabled(false);
                minimumDistanceText.setEnabled(false);
                maximumDistanceLabel.setEnabled(false);
                maximumDistanceText.setEnabled(false);
                lowestForbiddenLabel.setEnabled(false);
                lowestForbiddenText.setEnabled(false);
                highestForbiddenLabel.setEnabled(false);
                highestForbiddenText.setEnabled(false);
                highestRegenerationLabel.setEnabled(false);
                highestRegenerationText.setEnabled(false);
                semiMajorAxisLabel.setEnabled(true);
                semiMajorAxisText.setEnabled(true);
                semiMinorAxisLabel.setEnabled(true);
                semiMinorAxisText.setEnabled(true);
                phiLabel.setEnabled(true);
                phiText.setEnabled(true);	
                ratioLabel.setEnabled(false);
                ratioText.setEnabled(false);
            }
            else if (aggregatedEllipseRandomOrientationButton.isSelected()) {
            	initialCirclesLabel.setEnabled(true);
                initialCirclesText.setEnabled(true);
                minimumDistanceLabel.setEnabled(false);
                minimumDistanceText.setEnabled(false);
                maximumDistanceLabel.setEnabled(false);
                maximumDistanceText.setEnabled(false);
                lowestForbiddenLabel.setEnabled(false);
                lowestForbiddenText.setEnabled(false);
                highestForbiddenLabel.setEnabled(false);
                highestForbiddenText.setEnabled(false);
                highestRegenerationLabel.setEnabled(false);
                highestRegenerationText.setEnabled(false);
                semiMajorAxisLabel.setEnabled(true);
                semiMajorAxisText.setEnabled(true);
                semiMinorAxisLabel.setEnabled(true);
                semiMinorAxisText.setEnabled(true);
                phiLabel.setEnabled(false);
                phiText.setEnabled(false);	
                ratioLabel.setEnabled(false);
                ratioText.setEnabled(false);
            }
            else if (aggregatedEllipseRandomOrientationDifferentSizes.isSelected()) {
            	initialCirclesLabel.setEnabled(true);
                initialCirclesText.setEnabled(true);
                minimumDistanceLabel.setEnabled(false);
                minimumDistanceText.setEnabled(false);
                maximumDistanceLabel.setEnabled(false);
                maximumDistanceText.setEnabled(false);
                lowestForbiddenLabel.setEnabled(false);
                lowestForbiddenText.setEnabled(false);
                highestForbiddenLabel.setEnabled(false);
                highestForbiddenText.setEnabled(false);
                highestRegenerationLabel.setEnabled(false);
                highestRegenerationText.setEnabled(false);
                semiMajorAxisLabel.setEnabled(true);
                semiMajorAxisText.setEnabled(true);
                semiMinorAxisLabel.setEnabled(true);
                semiMinorAxisText.setEnabled(true);
                phiLabel.setEnabled(false);
                phiText.setEnabled(false);	
                ratioLabel.setEnabled(true);
                ratioText.setEnabled(true);
            }
            else if (regularButton.isSelected()){
                initialCirclesLabel.setEnabled(false);
                initialCirclesText.setEnabled(false);
                minimumDistanceLabel.setEnabled(true);
                minimumDistanceText.setEnabled(true);
                minimumDistanceText.setText("120.0");
                maximumDistanceLabel.setEnabled(true);
                maximumDistanceText.setEnabled(true);
                maximumDistanceText.setText("140.0");
                lowestForbiddenLabel.setEnabled(false);
                lowestForbiddenText.setEnabled(false);
                highestForbiddenLabel.setEnabled(false);
                highestForbiddenText.setEnabled(false);
                highestRegenerationLabel.setEnabled(false);
                highestRegenerationText.setEnabled(false);
                semiMajorAxisLabel.setEnabled(false);
                semiMajorAxisText.setEnabled(false);
                semiMinorAxisLabel.setEnabled(false);
                semiMinorAxisText.setEnabled(false);
                phiLabel.setEnabled(false);
                phiText.setEnabled(false);
                ratioLabel.setEnabled(false);
                ratioText.setEnabled(false);
            }
            else {
                initialCirclesLabel.setEnabled(false);
                initialCirclesText.setEnabled(false);
                minimumDistanceLabel.setEnabled(false);
                minimumDistanceText.setEnabled(false);
                maximumDistanceLabel.setEnabled(false);
                maximumDistanceText.setEnabled(false);
                lowestForbiddenLabel.setEnabled(true);
                lowestForbiddenText.setEnabled(true);
                highestForbiddenLabel.setEnabled(true);
                highestForbiddenText.setEnabled(true);
                highestRegenerationLabel.setEnabled(true);
                highestRegenerationText.setEnabled(true); 
                semiMajorAxisLabel.setEnabled(false);
                semiMajorAxisText.setEnabled(false);
                semiMinorAxisLabel.setEnabled(false);
                semiMinorAxisText.setEnabled(false);
                phiLabel.setEnabled(false);
                phiText.setEnabled(false);
                ratioLabel.setEnabled(false);
                ratioText.setEnabled(false);
            }
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


        if (algorithm instanceof AlgorithmCircleGeneration) {
            Preferences.debug("Circle Generation: " + algorithm.getElapsedTime());

            if ((cAlgo.isCompleted() == true) && (resultImage != null)) {


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
        } // if (algorithm instanceof AlgorithmCircleGeneration)

        if (cAlgo != null) {
            cAlgo.finalize();
            cAlgo = null;
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
            String name = "Generated_circles";
            extents = new int[2];
            extents[0] = xDim;
            extents[1] = yDim;
            resultImage = new ModelImage(ModelStorageBase.BYTE, extents, name);
            resultImage.setImageName(name);

            // Make algorithm
            cAlgo = new AlgorithmCircleGeneration(resultImage, radius, numCircles, pattern, initialRandomCircles,
                        minimumNearestNeighborDistance, maximumNearestNeighborDistance, lowestForbiddenNNDistance,
                        highestForbiddenNNDistance, highestRegenerationNNDistance, semiMajorAxis, semiMinorAxis,
                        phi, smallestToLargestAxisRatio);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            cAlgo.addListener(this);
            
            createProgressBar(name, cAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (cAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                cAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Circle Generation: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel xDimLabel;
        JLabel yDimLabel;
        JLabel radiusLabel;
        JLabel numCirclesLabel;
        
        setForeground(Color.black);
        setTitle("Circle generation");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Circle parameters"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;

        xDimLabel = new JLabel("Image x dimension ");
        xDimLabel.setForeground(Color.black);
        xDimLabel.setFont(serif12);
        xDimLabel.setEnabled(true);
        gbc6.gridy = 0;
        paramPanel.add(xDimLabel, gbc6);

        xDimText = new JTextField(10);
        xDimText.setText(String.valueOf(xDim));
        xDimText.setFont(serif12);
        xDimText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(xDimText, gbc6);

        yDimLabel = new JLabel("Image y dimension ");
        yDimLabel.setForeground(Color.black);
        yDimLabel.setFont(serif12);
        yDimLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 1;
        paramPanel.add(yDimLabel, gbc6);

        yDimText = new JTextField(10);
        yDimText.setText(String.valueOf(yDim));
        yDimText.setFont(serif12);
        yDimText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(yDimText, gbc6);
        
        radiusLabel = new JLabel("Circle radius ");
        radiusLabel.setForeground(Color.black);
        radiusLabel.setFont(serif12);
        radiusLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 2;
        paramPanel.add(radiusLabel, gbc6);

        radiusText = new JTextField(10);
        radiusText.setText(String.valueOf(radius));
        radiusText.setFont(serif12);
        radiusText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(radiusText, gbc6);
        
        numCirclesLabel = new JLabel("Number of circles ");
        numCirclesLabel.setForeground(Color.black);
        numCirclesLabel.setFont(serif12);
        numCirclesLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 3;
        paramPanel.add(numCirclesLabel, gbc6);
        
        numCirclesText = new JTextField(3);
        numCirclesText.setText(String.valueOf(numCircles));
        numCirclesText.setFont(serif12);
        numCirclesText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(numCirclesText, gbc6);
        
        patternGroup = new ButtonGroup();
        randomButton = new JRadioButton("Random pattern", true);
        randomButton.setFont(serif12);
        randomButton.setForeground(Color.black);
        randomButton.addActionListener(this);
        patternGroup.add(randomButton);
        gbc6.gridx = 0;
        gbc6.gridy = 4;
        paramPanel.add(randomButton, gbc6);
        
        aggregatedButton = new JRadioButton("Aggregated pattern", false);
        aggregatedButton.setFont(serif12);
        aggregatedButton.setForeground(Color.black);
        aggregatedButton.addActionListener(this);
        patternGroup.add(aggregatedButton);
        gbc6.gridx = 0;
        gbc6.gridy = 5;
        paramPanel.add(aggregatedButton, gbc6);
        
        aggregatedCirclesDifferentSizesButton = new JRadioButton("Aggregated circles different sizes pattern", false);
        aggregatedCirclesDifferentSizesButton.setFont(serif12);
        aggregatedCirclesDifferentSizesButton.setForeground(Color.black);
        aggregatedCirclesDifferentSizesButton.addActionListener(this);
        patternGroup.add(aggregatedCirclesDifferentSizesButton);
        gbc6.gridx = 0;
        gbc6.gridy = 6;
        paramPanel.add(aggregatedCirclesDifferentSizesButton, gbc6);
        
        aggregatedEllipseButton = new JRadioButton("Aggregated ellipse pattern", false);
        aggregatedEllipseButton.setFont(serif12);
        aggregatedEllipseButton.setForeground(Color.black);
        aggregatedEllipseButton.addActionListener(this);
        patternGroup.add(aggregatedEllipseButton);
        gbc6.gridx = 0;
        gbc6.gridy = 7;
        paramPanel.add(aggregatedEllipseButton, gbc6);
        
        aggregatedEllipseRandomOrientationButton = new JRadioButton("Aggregated ellipse random orientation pattern", false);
        aggregatedEllipseRandomOrientationButton.setFont(serif12);
        aggregatedEllipseRandomOrientationButton.setForeground(Color.black);
        aggregatedEllipseRandomOrientationButton.addActionListener(this);
        patternGroup.add(aggregatedEllipseRandomOrientationButton);
        gbc6.gridx = 0;
        gbc6.gridy = 8;
        paramPanel.add(aggregatedEllipseRandomOrientationButton, gbc6);
        
        aggregatedEllipseRandomOrientationDifferentSizes = 
        	new JRadioButton("Aggregated ellipse random orientation different sizes pattern", false);
        aggregatedEllipseRandomOrientationDifferentSizes.setFont(serif12);
        aggregatedEllipseRandomOrientationDifferentSizes.setForeground(Color.black);
        aggregatedEllipseRandomOrientationDifferentSizes.addActionListener(this);
        patternGroup.add(aggregatedEllipseRandomOrientationDifferentSizes);
        gbc6.gridx = 0;
        gbc6.gridy = 9;
        paramPanel.add(aggregatedEllipseRandomOrientationDifferentSizes, gbc6);
        
        regularButton = new JRadioButton("Regular pattern", false);
        regularButton.setFont(serif12);
        regularButton.setForeground(Color.black);
        regularButton.addActionListener(this);
        patternGroup.add(regularButton);
        gbc6.gridx = 0;
        gbc6.gridy = 10;
        paramPanel.add(regularButton, gbc6);
        
        constrainedButton = new JRadioButton("Constrained pattern", false);
        constrainedButton.setFont(serif12);
        constrainedButton.setForeground(Color.black);
        constrainedButton.addActionListener(this);
        patternGroup.add(constrainedButton);
        gbc6.gridx = 0;
        gbc6.gridy = 11;
        paramPanel.add(constrainedButton, gbc6);
        
        initialCirclesLabel = new JLabel("Initial circles randomly generated ");
        initialCirclesLabel.setForeground(Color.black);
        initialCirclesLabel.setFont(serif12);
        initialCirclesLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 12;
        paramPanel.add(initialCirclesLabel, gbc6);
        
        initialCirclesText = new JTextField(10);
        initialCirclesText.setText(String.valueOf(initialRandomCircles));
        initialCirclesText.setFont(serif12);
        initialCirclesText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(initialCirclesText, gbc6);
        
        minimumDistanceLabel = new JLabel("Minimum nearest neighbor distance ");
        minimumDistanceLabel.setForeground(Color.black);
        minimumDistanceLabel.setFont(serif12);
        minimumDistanceLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 13;
        paramPanel.add(minimumDistanceLabel, gbc6);
        
        minimumDistanceText = new JTextField(10);
        minimumDistanceText.setText(String.valueOf(minimumNearestNeighborDistance));
        minimumDistanceText.setFont(serif12);
        minimumDistanceText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(minimumDistanceText, gbc6);
        
        maximumDistanceLabel = new JLabel("Maximum nearest neighbor distance ");
        maximumDistanceLabel.setForeground(Color.black);
        maximumDistanceLabel.setFont(serif12);
        maximumDistanceLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 14;
        paramPanel.add(maximumDistanceLabel, gbc6);
        
        maximumDistanceText = new JTextField(10);
        maximumDistanceText.setText(String.valueOf(maximumNearestNeighborDistance));
        maximumDistanceText.setFont(serif12);
        maximumDistanceText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(maximumDistanceText, gbc6);
        
        ratioLabel = new JLabel("Smallest to largest axis ratio");
        ratioLabel.setForeground(Color.black);
        ratioLabel.setFont(serif12);
        ratioLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 15;
        paramPanel.add(ratioLabel, gbc6);
        
        ratioText = new JTextField(10);
        ratioText.setText(String.valueOf(smallestToLargestAxisRatio));
        ratioText.setFont(serif12);
        ratioText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(ratioText, gbc6);
        
        semiMajorAxisLabel = new JLabel("Maximum semi-major axis distance ");
        semiMajorAxisLabel.setForeground(Color.black);
        semiMajorAxisLabel.setFont(serif12);
        semiMajorAxisLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 16;
        paramPanel.add(semiMajorAxisLabel, gbc6);
        
        semiMajorAxisText = new JTextField(10);
        semiMajorAxisText.setText(String.valueOf(semiMajorAxis));
        semiMajorAxisText.setFont(serif12);
        semiMajorAxisText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(semiMajorAxisText, gbc6);
        
        semiMinorAxisLabel = new JLabel("Maximum semi-minor axis distance ");
        semiMinorAxisLabel.setForeground(Color.black);
        semiMinorAxisLabel.setFont(serif12);
        semiMinorAxisLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 17;
        paramPanel.add(semiMinorAxisLabel, gbc6);
        
        semiMinorAxisText = new JTextField(10);
        semiMinorAxisText.setText(String.valueOf(semiMinorAxis));
        semiMinorAxisText.setFont(serif12);
        semiMinorAxisText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(semiMinorAxisText, gbc6);
        
        phiLabel = new JLabel("Angle semi-major axis makes with x-axis in degrees ");
        phiLabel.setForeground(Color.black);
        phiLabel.setFont(serif12);
        phiLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 18;
        paramPanel.add(phiLabel, gbc6);
        
        phiText = new JTextField(10);
        phiText.setText(String.valueOf(phi));
        phiText.setFont(serif12);
        phiText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(phiText, gbc6);
        
        lowestForbiddenLabel = new JLabel("Lowest forbidden intermediate nearest neighbor distance ");
        lowestForbiddenLabel.setForeground(Color.black);
        lowestForbiddenLabel.setFont(serif12);
        lowestForbiddenLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 19;
        paramPanel.add(lowestForbiddenLabel, gbc6);
        
        lowestForbiddenText = new JTextField(10);
        lowestForbiddenText.setText(String.valueOf(lowestForbiddenNNDistance));
        lowestForbiddenText.setFont(serif12);
        lowestForbiddenText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(lowestForbiddenText, gbc6);
        
        highestForbiddenLabel = new JLabel("Highest forbidden intermediate nearest neighbor distance ");
        highestForbiddenLabel.setForeground(Color.black);
        highestForbiddenLabel.setFont(serif12);
        highestForbiddenLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 20;
        paramPanel.add(highestForbiddenLabel, gbc6);
        
        highestForbiddenText = new JTextField(10);
        highestForbiddenText.setText(String.valueOf(highestForbiddenNNDistance));
        highestForbiddenText.setFont(serif12);
        highestForbiddenText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(highestForbiddenText, gbc6);
        
        highestRegenerationLabel = new JLabel("Highest regeneration nearest neighbor distance ");
        highestRegenerationLabel.setForeground(Color.black);
        highestRegenerationLabel.setFont(serif12);
        highestRegenerationLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 21;
        paramPanel.add(highestRegenerationLabel, gbc6);
        
        highestRegenerationText = new JTextField(10);
        highestRegenerationText.setText(String.valueOf(highestRegenerationNNDistance));
        highestRegenerationText.setFont(serif12);
        highestRegenerationText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(highestRegenerationText, gbc6);

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

        if (!testParameter(xDimText.getText(), 5, 1000000)) {
            xDimText.requestFocus();
            xDimText.selectAll();

            return false;
        } else {
            xDim = Integer.valueOf(xDimText.getText()).intValue();
        }

        if (!testParameter(yDimText.getText(), 5, 1000000)) {
            yDimText.requestFocus();
            yDimText.selectAll();

            return false;
        } else {
            yDim = Integer.valueOf(yDimText.getText()).intValue();
        }
        
        if (!testParameter(radiusText.getText(), 0, 1000000)) {
            radiusText.requestFocus();
            radiusText.selectAll();

            return false;
        } else {
            radius = Integer.valueOf(radiusText.getText()).intValue();
        }
        
        if (!testParameter(numCirclesText.getText(), 1, 100000)) {
            numCirclesText.requestFocus();
            numCirclesText.selectAll();

            return false;
        } else {
            numCircles = Integer.valueOf(numCirclesText.getText()).intValue();
        }
        
        if (randomButton.isSelected()) {
            pattern = AlgorithmCircleGeneration.RANDOM;
        }
        else if (aggregatedButton.isSelected()) {
            pattern = AlgorithmCircleGeneration.AGGREGATED;
            initialRandomCircles = Integer.valueOf(initialCirclesText.getText()).intValue();
            if (initialRandomCircles < 1) {
                MipavUtil.displayError("The number of initial random circles must be at least 1");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            if (initialRandomCircles >= numCircles) {
                MipavUtil.displayError("The number of initial random circles must be less than the number of circles");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            
            maximumNearestNeighborDistance = Double.valueOf(maximumDistanceText.getText()).doubleValue();
            if (maximumNearestNeighborDistance < 2.0 * radius) {
                MipavUtil.displayError("The maximum nearest neighbor distance must be at least 2.0 * radius");
                maximumDistanceText.requestFocus();
                maximumDistanceText.selectAll();
                return false;
            }
        }
        else if (aggregatedCirclesDifferentSizesButton.isSelected()) {
            pattern = AlgorithmCircleGeneration.AGGREGATED_CIRCLES_DIFFERENT_SIZES;
            initialRandomCircles = Integer.valueOf(initialCirclesText.getText()).intValue();
            if (initialRandomCircles < 1) {
                MipavUtil.displayError("The number of initial random circles must be at least 1");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            if (initialRandomCircles >= numCircles) {
                MipavUtil.displayError("The number of initial random circles must be less than the number of circles");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            
            maximumNearestNeighborDistance = Double.valueOf(maximumDistanceText.getText()).doubleValue();
            if (maximumNearestNeighborDistance < 2.0 * radius) {
                MipavUtil.displayError("The maximum nearest neighbor distance must be at least 2.0 * radius");
                maximumDistanceText.requestFocus();
                maximumDistanceText.selectAll();
                return false;
            }
            
            smallestToLargestAxisRatio = Double.valueOf(ratioText.getText()).doubleValue();
            if (smallestToLargestAxisRatio > 1.0) {
            	MipavUtil.displayError("The smallest to largest axis ratio cannot exceed 1.0");
            	ratioText.requestFocus();
            	ratioText.selectAll();
            	return false;
            }
        }
        else if (aggregatedEllipseButton.isSelected()) {
        	pattern = AlgorithmCircleGeneration.AGGREGATED_ELLIPSE;
            initialRandomCircles = Integer.valueOf(initialCirclesText.getText()).intValue();
            if (initialRandomCircles < 1) {
                MipavUtil.displayError("The number of initial random circles must be at least 1");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            if (initialRandomCircles >= numCircles) {
                MipavUtil.displayError("The number of initial random circles must be less than the number of circles");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            
            if (!testParameter(semiMajorAxisText.getText(), 0, 1000000)) {
                semiMajorAxisText.requestFocus();
                semiMajorAxisText.selectAll();

                return false;
            } else {
                semiMajorAxis = Double.valueOf(semiMajorAxisText.getText()).doubleValue();
                if (semiMajorAxis < 2.0 * radius) {
                    MipavUtil.displayError("The semi-major axis distance must be a least 2.0 * radius");
                    semiMajorAxisText.requestFocus();
                    semiMajorAxisText.selectAll();
                    return false;
                }
            }
            
            if (!testParameter(semiMinorAxisText.getText(), 0, 1000000)) {
                semiMinorAxisText.requestFocus();
                semiMinorAxisText.selectAll();

                return false;
            } else {
                semiMinorAxis = Double.valueOf(semiMinorAxisText.getText()).doubleValue();
                if (semiMinorAxis > semiMajorAxis) {
                	semiMinorAxisText.requestFocus();
                    semiMinorAxisText.selectAll();	
                    MipavUtil.displayError("The semi-minor axis cannot be greater than the semi-major axis");
                    return false;
                }
                if (semiMinorAxis < 2.0 * radius) {
                    MipavUtil.displayError("The semi-minor axis distance must be a least 2.0 * radius");
                    semiMinorAxisText.requestFocus();
                    semiMinorAxisText.selectAll();
                    return false;
                }
            }
            
            if (!testParameter(phiText.getText(), -Double.MAX_VALUE, Double.MAX_VALUE)) {
            	phiText.requestFocus();
            	phiText.selectAll();
            	return false;
            }
            else {
            	phi = Double.valueOf(phiText.getText()).doubleValue();
            	phi = phi * Math.PI/180.0;
            }
        }
        else if (aggregatedEllipseRandomOrientationButton.isSelected()) {
        	pattern = AlgorithmCircleGeneration.AGGREGATED_ELLIPSE_RANDOM_ORIENTATION;
            initialRandomCircles = Integer.valueOf(initialCirclesText.getText()).intValue();
            if (initialRandomCircles < 1) {
                MipavUtil.displayError("The number of initial random circles must be at least 1");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            if (initialRandomCircles >= numCircles) {
                MipavUtil.displayError("The number of initial random circles must be less than the number of circles");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            
            if (!testParameter(semiMajorAxisText.getText(), 0, 1000000)) {
                semiMajorAxisText.requestFocus();
                semiMajorAxisText.selectAll();

                return false;
            } else {
                semiMajorAxis = Double.valueOf(semiMajorAxisText.getText()).doubleValue();
                if (semiMajorAxis < 2.0 * radius) {
                    MipavUtil.displayError("The semi-major axis distance must be a least 2.0 * radius");
                    semiMajorAxisText.requestFocus();
                    semiMajorAxisText.selectAll();
                    return false;
                }
            }
            
            if (!testParameter(semiMinorAxisText.getText(), 0, 1000000)) {
                semiMinorAxisText.requestFocus();
                semiMinorAxisText.selectAll();

                return false;
            } else {
                semiMinorAxis = Double.valueOf(semiMinorAxisText.getText()).doubleValue();
                if (semiMinorAxis > semiMajorAxis) {
                	semiMinorAxisText.requestFocus();
                    semiMinorAxisText.selectAll();	
                    MipavUtil.displayError("The semi-minor axis cannot be greater than the semi-major axis");
                    return false;
                }
                if (semiMinorAxis < 2.0 * radius) {
                    MipavUtil.displayError("The semi-minor axis distance must be a least 2.0 * radius");
                    semiMinorAxisText.requestFocus();
                    semiMinorAxisText.selectAll();
                    return false;
                }
            }
            
        }
        else if (aggregatedEllipseRandomOrientationDifferentSizes.isSelected()) {
        	pattern = AlgorithmCircleGeneration.AGGREGATED_ELLIPSE_RANDOM_ORIENTATION_DIFFERENT_SIZES;
            initialRandomCircles = Integer.valueOf(initialCirclesText.getText()).intValue();
            if (initialRandomCircles < 1) {
                MipavUtil.displayError("The number of initial random circles must be at least 1");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            if (initialRandomCircles >= numCircles) {
                MipavUtil.displayError("The number of initial random circles must be less than the number of circles");
                initialCirclesText.requestFocus();
                initialCirclesText.selectAll();
                return false;
            }
            
            if (!testParameter(semiMajorAxisText.getText(), 0, 1000000)) {
                semiMajorAxisText.requestFocus();
                semiMajorAxisText.selectAll();

                return false;
            } else {
                semiMajorAxis = Double.valueOf(semiMajorAxisText.getText()).doubleValue();
                if (semiMajorAxis < 2.0 * radius) {
                    MipavUtil.displayError("The semi-major axis distance must be a least 2.0 * radius");
                    semiMajorAxisText.requestFocus();
                    semiMajorAxisText.selectAll();
                    return false;
                }
            }
            
            if (!testParameter(semiMinorAxisText.getText(), 0, 1000000)) {
                semiMinorAxisText.requestFocus();
                semiMinorAxisText.selectAll();

                return false;
            } else {
                semiMinorAxis = Double.valueOf(semiMinorAxisText.getText()).doubleValue();
                if (semiMinorAxis > semiMajorAxis) {
                	semiMinorAxisText.requestFocus();
                    semiMinorAxisText.selectAll();	
                    MipavUtil.displayError("The semi-minor axis cannot be greater than the semi-major axis");
                    return false;
                }
                if (semiMinorAxis < 2.0 * radius) {
                    MipavUtil.displayError("The semi-minor axis distance must be a least 2.0 * radius");
                    semiMinorAxisText.requestFocus();
                    semiMinorAxisText.selectAll();
                    return false;
                }
            }
            
            smallestToLargestAxisRatio = Double.valueOf(ratioText.getText()).doubleValue();
            if (smallestToLargestAxisRatio > 1.0) {
            	MipavUtil.displayError("The smallest to largest axis ratio cannot exceed 1.0");
            	ratioText.requestFocus();
            	ratioText.selectAll();
            	return false;
            }
            
        }
        else if (regularButton.isSelected()){
            pattern = AlgorithmCircleGeneration.REGULAR;
            minimumNearestNeighborDistance = Double.valueOf(minimumDistanceText.getText()).doubleValue();
            if (minimumNearestNeighborDistance < 2.0 * radius) {
                MipavUtil.displayError("The minimum nearest neighbor distance must be at least 2.0 * radius");
                minimumDistanceText.requestFocus();
                minimumDistanceText.selectAll();
                return false;
            }
            
            maximumNearestNeighborDistance = Double.valueOf(maximumDistanceText.getText()).doubleValue();
            if (maximumNearestNeighborDistance <= minimumNearestNeighborDistance) {
                MipavUtil.displayError(
                "The maximum nearest neighbor distance must be greater than the minimum nearest neighbor distance");
                maximumDistanceText.requestFocus();
                maximumDistanceText.selectAll();
                return false;
            }
        }
        else {
            pattern = AlgorithmCircleGeneration.CONSTRAINED;
            lowestForbiddenNNDistance = Double.valueOf(lowestForbiddenText.getText()).doubleValue();
            if (lowestForbiddenNNDistance <= 2.0 * radius) {
                MipavUtil.displayError(
                "The lowest forbidden intermediate nearest neighbor distance must be greater than 2.0 * radius");
                lowestForbiddenText.requestFocus();
                lowestForbiddenText.selectAll();
                return false;
            }
            
            highestForbiddenNNDistance = Double.valueOf(highestForbiddenText.getText()).doubleValue();
            if (highestForbiddenNNDistance <= lowestForbiddenNNDistance) {
                MipavUtil.displayError(
                "The highest forbidden intermediate nearest neighbor distance must be greater than the lowest forbidden distance");
                highestForbiddenText.requestFocus();
                highestForbiddenText.selectAll();
                return false;
            }
            
            highestRegenerationNNDistance = Double.valueOf(highestRegenerationText.getText()).doubleValue();
            if (highestRegenerationNNDistance <= highestForbiddenNNDistance) {
                MipavUtil.displayError(
                "The highest regeneration nearest neighbor distance must be greater than the highest forbidden distance");
                highestRegenerationText.requestFocus();
                highestRegenerationText.selectAll();
                return false;
            }
        }

        return true;
    }
}
