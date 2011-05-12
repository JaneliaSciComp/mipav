package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create an image with randomly spaced, aggregated, regular or uniform, or constrained spheres of the same size
 */
public class JDialogSphereGeneration extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmSphereGeneration sAlgo = null;

    private int extents[];

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int xDim = 512;

    /** DOCUMENT ME! */
    private JTextField xDimText;

    /** DOCUMENT ME! */
    private int yDim = 512;
    
    private JTextField yDimText;
    
    private int zDim = 512;
    
    private JTextField zDimText;
    
    private ButtonGroup radiusGroup;
    
    private JRadioButton constantButton;
    
    private JRadioButton uniformButton;
    
    private JLabel radiusLabel;
    
    private int minRadius = 5;

    /** DOCUMENT ME! */
    private JTextField radiusText;
    
    private JLabel maxRadiusLabel;
    
    private int maxRadius = 10;
    
    private JTextField maxRadiusText;
    
    private int radiusDistribution = AlgorithmSphereGeneration.CONSTANT_RADIUS;
    
    private int numSpheres = 200;
    
    private JTextField numSpheresText;
    
    private ButtonGroup patternGroup;
    
    private JRadioButton randomButton;
    
    private JRadioButton aggregatedButton;
    
    private JRadioButton regularButton;
    
    private JRadioButton constrainedButton;
    
    private int pattern = AlgorithmCircleGeneration.RANDOM;
    
    private JLabel initialSpheresLabel;
    
    private JTextField initialSpheresText;
    
    private int initialRandomSpheres = 20;
    
    private JLabel minimumDistanceLabel;
    
    private JTextField minimumDistanceText;
    
    private double minimumNearestNeighborDistance = 12.0;
    
    private JLabel maximumDistanceLabel;
    
    private JTextField maximumDistanceText;
    
    private double maximumNearestNeighborDistance = 15.0;
    
    private JLabel lowestForbiddenLabel;
    
    private JTextField lowestForbiddenText;
    
    private double lowestForbiddenNNDistance = 15.0;
    
    private JLabel highestForbiddenLabel;
    
    private JTextField highestForbiddenText;
    
    private double highestForbiddenNNDistance = 20.0;
    
    private JLabel highestRegenerationLabel;
    
    private JTextField highestRegenerationText;
    
    private double highestRegenerationNNDistance = 25.0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogSphereGeneration(Frame theParentFrame) {
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
        } else if ((source == randomButton) || (source == aggregatedButton) || (source == regularButton) ||
                   (source == constrainedButton)) {
            if (randomButton.isSelected()) {
                initialSpheresLabel.setEnabled(false);
                initialSpheresText.setEnabled(false);
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
            }
            else if (aggregatedButton.isSelected()) {
                initialSpheresLabel.setEnabled(true);
                initialSpheresText.setEnabled(true);
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
            }
            else if (regularButton.isSelected()){
                initialSpheresLabel.setEnabled(false);
                initialSpheresText.setEnabled(false);
                minimumDistanceLabel.setEnabled(true);
                minimumDistanceText.setEnabled(true);
                minimumDistanceText.setText("55.0");
                maximumDistanceLabel.setEnabled(true);
                maximumDistanceText.setEnabled(true);
                maximumDistanceText.setText("70.0");
                lowestForbiddenLabel.setEnabled(false);
                lowestForbiddenText.setEnabled(false);
                highestForbiddenLabel.setEnabled(false);
                highestForbiddenText.setEnabled(false);
                highestRegenerationLabel.setEnabled(false);
                highestRegenerationText.setEnabled(false);
            }
            else {
                initialSpheresLabel.setEnabled(false);
                initialSpheresText.setEnabled(false);
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
            }
        }
        else if ((constantButton.isSelected()) || uniformButton.isSelected()) {
            if (constantButton.isSelected()) {
                radiusLabel.setText("Sphere radius ");
                maxRadiusLabel.setEnabled(false);
                maxRadiusText.setEnabled(false);
            }
            else {
                radiusLabel.setText("Minimum sphere radius ");
                maxRadiusLabel.setEnabled(true);
                maxRadiusText.setEnabled(true);
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


        if (algorithm instanceof AlgorithmSphereGeneration) {
            Preferences.debug("Sphere Generation: " + algorithm.getElapsedTime());

            if ((sAlgo.isCompleted() == true) && (resultImage != null)) {


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
        } // if (algorithm instanceof AlgorithmSphereGeneration)

        if (sAlgo != null) {
            sAlgo.finalize();
            sAlgo = null;
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
            String name = "Generated_spheres";
            extents = new int[3];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
            resultImage = new ModelImage(ModelStorageBase.BYTE, extents, name);
            resultImage.setImageName(name);

            // Make algorithm
            sAlgo = new AlgorithmSphereGeneration(resultImage, minRadius, maxRadius, numSpheres, pattern, 
                        radiusDistribution, initialRandomSpheres,
                        minimumNearestNeighborDistance, maximumNearestNeighborDistance, lowestForbiddenNNDistance,
                        highestForbiddenNNDistance, highestRegenerationNNDistance);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            sAlgo.addListener(this);
            
            createProgressBar(name, sAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (sAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                sAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Sphere Generation: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel xDimLabel;
        JLabel yDimLabel;
        JLabel zDimLabel;
        JLabel numCirclesLabel;
        
        setForeground(Color.black);
        setTitle("Sphere generation");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Sphere parameters"));

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
        
        zDimLabel = new JLabel("Image z dimension ");
        zDimLabel.setForeground(Color.black);
        zDimLabel.setFont(serif12);
        zDimLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 2;
        paramPanel.add(zDimLabel, gbc6);

        zDimText = new JTextField(10);
        zDimText.setText(String.valueOf(zDim));
        zDimText.setFont(serif12);
        zDimText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(zDimText, gbc6);
        
        radiusGroup = new ButtonGroup();
        constantButton = new JRadioButton("Constant radius", true);
        constantButton.setFont(serif12);
        constantButton.setForeground(Color.black);
        constantButton.addActionListener(this);
        radiusGroup.add(constantButton);
        gbc6.gridx = 0;
        gbc6.gridy = 3;
        paramPanel.add(constantButton, gbc6);
        
        uniformButton = new JRadioButton("Uniform radius distribution", false);
        uniformButton.setFont(serif12);
        uniformButton.setForeground(Color.black);
        uniformButton.addActionListener(this);
        radiusGroup.add(uniformButton);
        gbc6.gridx = 0;
        gbc6.gridy = 4;
        paramPanel.add(uniformButton, gbc6);
        
        radiusLabel = new JLabel("Sphere radius ");
        radiusLabel.setForeground(Color.black);
        radiusLabel.setFont(serif12);
        radiusLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 5;
        paramPanel.add(radiusLabel, gbc6);

        radiusText = new JTextField(10);
        radiusText.setText(String.valueOf(minRadius));
        radiusText.setFont(serif12);
        radiusText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(radiusText, gbc6);
        
        maxRadiusLabel = new JLabel("Maximum sphere radius ");
        maxRadiusLabel.setForeground(Color.black);
        maxRadiusLabel.setFont(serif12);
        maxRadiusLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 6;
        paramPanel.add(maxRadiusLabel, gbc6);

        maxRadiusText = new JTextField(10);
        maxRadiusText.setText(String.valueOf(maxRadius));
        maxRadiusText.setFont(serif12);
        maxRadiusText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(maxRadiusText, gbc6);
        
        numCirclesLabel = new JLabel("Number of spheres ");
        numCirclesLabel.setForeground(Color.black);
        numCirclesLabel.setFont(serif12);
        numCirclesLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 7;
        paramPanel.add(numCirclesLabel, gbc6);
        
        numSpheresText = new JTextField(3);
        numSpheresText.setText(String.valueOf(numSpheres));
        numSpheresText.setFont(serif12);
        numSpheresText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(numSpheresText, gbc6);
        
        patternGroup = new ButtonGroup();
        randomButton = new JRadioButton("Random pattern", true);
        randomButton.setFont(serif12);
        randomButton.setForeground(Color.black);
        randomButton.addActionListener(this);
        patternGroup.add(randomButton);
        gbc6.gridx = 0;
        gbc6.gridy = 8;
        paramPanel.add(randomButton, gbc6);
        
        aggregatedButton = new JRadioButton("Aggregated pattern", false);
        aggregatedButton.setFont(serif12);
        aggregatedButton.setForeground(Color.black);
        aggregatedButton.addActionListener(this);
        patternGroup.add(aggregatedButton);
        gbc6.gridx = 0;
        gbc6.gridy = 9;
        paramPanel.add(aggregatedButton, gbc6);
        
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
        
        initialSpheresLabel = new JLabel("Initial spheres randomly generated ");
        initialSpheresLabel.setForeground(Color.black);
        initialSpheresLabel.setFont(serif12);
        initialSpheresLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 12;
        paramPanel.add(initialSpheresLabel, gbc6);
        
        initialSpheresText = new JTextField(10);
        initialSpheresText.setText(String.valueOf(initialRandomSpheres));
        initialSpheresText.setFont(serif12);
        initialSpheresText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(initialSpheresText, gbc6);
        
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
        
        lowestForbiddenLabel = new JLabel("Lowest forbidden intermediate nearest neighbor distance ");
        lowestForbiddenLabel.setForeground(Color.black);
        lowestForbiddenLabel.setFont(serif12);
        lowestForbiddenLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 15;
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
        gbc6.gridy = 16;
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
        gbc6.gridy = 17;
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
        
        if (!testParameter(zDimText.getText(), 5, 1000000)) {
            zDimText.requestFocus();
            zDimText.selectAll();

            return false;
        } else {
            zDim = Integer.valueOf(zDimText.getText()).intValue();
        }
        
        if (constantButton.isSelected()) {
            radiusDistribution = AlgorithmSphereGeneration.CONSTANT_RADIUS;
        }
        else {
            radiusDistribution = AlgorithmSphereGeneration.UNIFORM_RADIUS;
        }
        
        if (!testParameter(radiusText.getText(), 0, 1000000)) {
            radiusText.requestFocus();
            radiusText.selectAll();

            return false;
        } else {
            minRadius = Integer.valueOf(radiusText.getText()).intValue();
        }
        
        if (radiusDistribution == AlgorithmSphereGeneration.CONSTANT_RADIUS) {
            maxRadius = minRadius;
        }
        else {
            if (!testParameter(maxRadiusText.getText(), minRadius, 1000000)) {
                maxRadiusText.requestFocus();
                maxRadiusText.selectAll();

                return false;
            } else {
                maxRadius = Integer.valueOf(maxRadiusText.getText()).intValue();
            }    
        }
        
        if (!testParameter(numSpheresText.getText(), 1, 100000)) {
            numSpheresText.requestFocus();
            numSpheresText.selectAll();

            return false;
        } else {
            numSpheres = Integer.valueOf(numSpheresText.getText()).intValue();
        }
        
        if (randomButton.isSelected()) {
            pattern = AlgorithmSphereGeneration.RANDOM;
        }
        else if (aggregatedButton.isSelected()) {
            pattern = AlgorithmSphereGeneration.AGGREGATED;
            initialRandomSpheres = Integer.valueOf(initialSpheresText.getText()).intValue();
            if (initialRandomSpheres < 1) {
                MipavUtil.displayError("The number of initial random spheres must be at least 1");
                initialSpheresText.requestFocus();
                initialSpheresText.selectAll();
                return false;
            }
            if (initialRandomSpheres >= numSpheres) {
                MipavUtil.displayError("The number of initial random spheres must be less than the number of spheres");
                initialSpheresText.requestFocus();
                initialSpheresText.selectAll();
                return false;
            }
            
            maximumNearestNeighborDistance = Double.valueOf(maximumDistanceText.getText()).doubleValue();
            if (maximumNearestNeighborDistance < 2.0 * minRadius) {
                MipavUtil.displayError("The maximum nearest neighbor distance must be at least 2.0 * radius");
                maximumDistanceText.requestFocus();
                maximumDistanceText.selectAll();
                return false;
            }
        }
        else if (regularButton.isSelected()){
            pattern = AlgorithmSphereGeneration.REGULAR;
            minimumNearestNeighborDistance = Double.valueOf(minimumDistanceText.getText()).doubleValue();
            if (minimumNearestNeighborDistance < 2.0 * minRadius) {
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
            pattern = AlgorithmSphereGeneration.CONSTRAINED;
            lowestForbiddenNNDistance = Double.valueOf(lowestForbiddenText.getText()).doubleValue();
            if (lowestForbiddenNNDistance <= 2.0 * minRadius) {
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
