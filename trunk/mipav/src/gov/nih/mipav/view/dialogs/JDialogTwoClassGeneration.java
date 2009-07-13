package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create an image with two classes of circles which can be 1.) repelled or segregated, 2.) independent or 
 * fixedPoissonSame, or 3.) attracted or segregated.
 */
public class JDialogTwoClassGeneration extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmTwoClassGeneration tcAlgo = null;

    private int extents[];

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int dim = 512;

    /** DOCUMENT ME! */
    private JTextField dimText;
    
    private int radius = 0;

    /** DOCUMENT ME! */
    private JTextField radiusText;
    
    private int numParents = 5;
    
    private JLabel numParentsLabel;
    
    private JTextField numParentsText;
    
    private ButtonGroup processGroup;
    
    private JRadioButton fixedPoissonSameButton;
    
    private JRadioButton fixedPoissonDifferentButton;
    
    private JRadioButton randomPoissonSameButton;
    
    private JRadioButton randomPoissonDifferentButton;
    
    private JRadioButton MaternSameButton;
    
    private JRadioButton MaternDifferentButton;
    
    private JRadioButton inhomogeneousPoissonButton;
    
    private JRadioButton segregationButton;
    
    private JRadioButton associationButton;
    
    private int process = AlgorithmTwoClassGeneration.FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS;
    
    private JLabel numOffspring1Label;
    
    private JTextField numOffspring1Text;
    
    private int numOffspring1 = 50;
    
    private JLabel numOffspring2Label;
    
    private JTextField numOffspring2Text;
    
    private int numOffspring2 = 50;
    
    private JLabel normalizedStdDevLabel;
    
    private JTextField normalizedStdDevText;
    
    private double normalizedStdDev = 0.10;
    
    private JLabel parentPoissonNormalizedMeanLabel;
    
    private JTextField parentPoissonNormalizedMeanText;
    
    private double parentPoissonNormalizedMean = 5.0;
    
    private ButtonGroup inhomogeneousGroup;
    
    private JRadioButton sqrtXPlusYButton;
    
    private JRadioButton sqrtXTimesYButton;
    
    private JRadioButton absXMinusYButton;
    
    private JLabel normalizedDiscRadiusLabel;
    
    private JTextField normalizedDiscRadiusText;
    
    private double normalizedDiscRadius = 0.10;
    
    private int inhomogeneous = AlgorithmTwoClassGeneration.SQRT_X_PLUS_Y;
    
    private JLabel numPoints1Label;
    
    private JTextField numPoints1Text;
    
    private int numPoints1 = 100;
    
    private JLabel numPoints2Label;
    
    private JTextField numPoints2Text;
    
    private int numPoints2 = 100;
    
    private JLabel segregationLabel;
    
    private JTextField segregationText;
    
    private double segregation = 0.25;
    
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogTwoClassGeneration(Frame theParentFrame) {
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
        } else if ((source == fixedPoissonSameButton) || (source == fixedPoissonDifferentButton) ||
                   (source == randomPoissonSameButton) || (source == randomPoissonDifferentButton) ||
                   (source == MaternSameButton) || (source == MaternDifferentButton) ||
                   (source == inhomogeneousPoissonButton) || (source == segregationButton) ||
                   (source == associationButton)) {
            if (fixedPoissonSameButton.isSelected() || fixedPoissonDifferentButton.isSelected() || 
                randomPoissonSameButton.isSelected() || randomPoissonDifferentButton.isSelected()) {
                numParentsLabel.setEnabled(true);
                numParentsText.setEnabled(true);
                numOffspring1Label.setEnabled(true);
                numOffspring1Text.setEnabled(true);
                numOffspring2Label.setEnabled(true);
                numOffspring2Text.setEnabled(true);
                normalizedStdDevLabel.setEnabled(true);
                normalizedStdDevText.setEnabled(true);
                parentPoissonNormalizedMeanLabel.setEnabled(false);
                parentPoissonNormalizedMeanText.setEnabled(false);
                normalizedDiscRadiusLabel.setEnabled(false);
                normalizedDiscRadiusText.setEnabled(false);
                numPoints1Label.setEnabled(false);
                numPoints1Text.setEnabled(false);
                numPoints2Label.setEnabled(false);
                numPoints2Text.setEnabled(false);
                sqrtXPlusYButton.setEnabled(false);
                sqrtXTimesYButton.setEnabled(false);
                absXMinusYButton.setEnabled(false);
                segregationLabel.setEnabled(false);
                segregationText.setEnabled(false);
            }
            else if (MaternSameButton.isSelected() || MaternDifferentButton.isSelected()) {
                numParentsLabel.setEnabled(false);
                numParentsText.setEnabled(false);
                numOffspring1Label.setEnabled(true);
                numOffspring1Text.setEnabled(true);
                numOffspring2Label.setEnabled(true);
                numOffspring2Text.setEnabled(true);
                normalizedStdDevLabel.setEnabled(false);
                normalizedStdDevText.setEnabled(false);
                parentPoissonNormalizedMeanLabel.setEnabled(true);
                parentPoissonNormalizedMeanText.setEnabled(true);
                normalizedDiscRadiusLabel.setEnabled(true);
                normalizedDiscRadiusText.setEnabled(true);
                numPoints1Label.setEnabled(false);
                numPoints1Text.setEnabled(false);
                numPoints2Label.setEnabled(false);
                numPoints2Text.setEnabled(false);
                sqrtXPlusYButton.setEnabled(false);
                sqrtXTimesYButton.setEnabled(false);
                absXMinusYButton.setEnabled(false);
                segregationLabel.setEnabled(false);
                segregationText.setEnabled(false);
            }
            else if (inhomogeneousPoissonButton.isSelected()){
                numParentsLabel.setEnabled(false);
                numParentsText.setEnabled(false); 
                numOffspring1Label.setEnabled(false);
                numOffspring1Text.setEnabled(false);
                numOffspring2Label.setEnabled(false);
                numOffspring2Text.setEnabled(false);
                normalizedStdDevLabel.setEnabled(false);
                normalizedStdDevText.setEnabled(false);
                parentPoissonNormalizedMeanLabel.setEnabled(false);
                parentPoissonNormalizedMeanText.setEnabled(false);
                normalizedDiscRadiusLabel.setEnabled(false);
                normalizedDiscRadiusText.setEnabled(false);
                numPoints1Label.setEnabled(true);
                numPoints1Text.setEnabled(true);
                numPoints2Label.setEnabled(true);
                numPoints2Text.setEnabled(true);
                sqrtXPlusYButton.setEnabled(true);
                sqrtXTimesYButton.setEnabled(true);
                absXMinusYButton.setEnabled(true);
                segregationLabel.setEnabled(false);
                segregationText.setEnabled(false);
            }
            else if (segregationButton.isSelected()) {
                numParentsLabel.setEnabled(false);
                numParentsText.setEnabled(false); 
                numOffspring1Label.setEnabled(true);
                numOffspring1Text.setEnabled(true);
                numOffspring2Label.setEnabled(true);
                numOffspring2Text.setEnabled(true);
                normalizedStdDevLabel.setEnabled(false);
                normalizedStdDevText.setEnabled(false);
                parentPoissonNormalizedMeanLabel.setEnabled(false);
                parentPoissonNormalizedMeanText.setEnabled(false);
                normalizedDiscRadiusLabel.setEnabled(false);
                normalizedDiscRadiusText.setEnabled(false);
                numPoints1Label.setEnabled(false);
                numPoints1Text.setEnabled(false);
                numPoints2Label.setEnabled(false);
                numPoints2Text.setEnabled(false);
                sqrtXPlusYButton.setEnabled(false);
                sqrtXTimesYButton.setEnabled(false);
                absXMinusYButton.setEnabled(false);
                segregationLabel.setEnabled(true);
                segregationText.setEnabled(true);    
            }
            else if (associationButton.isSelected()) {
                numParentsLabel.setEnabled(false);
                numParentsText.setEnabled(false); 
                numOffspring1Label.setEnabled(true);
                numOffspring1Text.setEnabled(true);
                numOffspring2Label.setEnabled(true);
                numOffspring2Text.setEnabled(true);
                normalizedStdDevLabel.setEnabled(false);
                normalizedStdDevText.setEnabled(false);
                parentPoissonNormalizedMeanLabel.setEnabled(false);
                parentPoissonNormalizedMeanText.setEnabled(false);
                normalizedDiscRadiusLabel.setEnabled(true);
                normalizedDiscRadiusText.setEnabled(true);
                numPoints1Label.setEnabled(false);
                numPoints1Text.setEnabled(false);
                numPoints2Label.setEnabled(false);
                numPoints2Text.setEnabled(false);
                sqrtXPlusYButton.setEnabled(false);
                sqrtXTimesYButton.setEnabled(false);
                absXMinusYButton.setEnabled(false);
                segregationLabel.setEnabled(false);
                segregationText.setEnabled(false);        
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


        if (algorithm instanceof AlgorithmTwoClassGeneration) {
            Preferences.debug("Two Class Generation: " + algorithm.getElapsedTime());

            if ((tcAlgo.isCompleted() == true) && (resultImage != null)) {


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
        } // if (algorithm instanceof AlgorithmTwoClassGeneration)

        if (tcAlgo != null) {
            tcAlgo.finalize();
            tcAlgo = null;
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
        Object source = event.getSource();
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
            String name = "Two_classes";
            extents = new int[2];
            extents[0] = dim;
            extents[1] = dim;
            resultImage = new ModelImage(ModelStorageBase.BYTE, extents, name);
            resultImage.setImageName(name);

            // Make algorithm
            tcAlgo = new AlgorithmTwoClassGeneration(resultImage, radius, process, numParents, numOffspring1,
                        numOffspring2, normalizedStdDev, parentPoissonNormalizedMean,
                        normalizedDiscRadius, numPoints1, numPoints2, inhomogeneous, segregation);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            tcAlgo.addListener(this);
            
            createProgressBar(name, tcAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (tcAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                tcAlgo.run();
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
        JLabel dimLabel;
        JLabel radiusLabel;
        
        setForeground(Color.black);
        setTitle("Two class segregation/association generation");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = gbc6.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;

        dimLabel = new JLabel("Image x dimension = image y dimension ");
        dimLabel.setForeground(Color.black);
        dimLabel.setFont(serif12);
        dimLabel.setEnabled(true);
        gbc6.gridy = 0;
        paramPanel.add(dimLabel, gbc6);

        dimText = new JTextField(10);
        dimText.setText(String.valueOf(dim));
        dimText.setFont(serif12);
        dimText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(dimText, gbc6);
        
        radiusLabel = new JLabel("Circle radius ");
        radiusLabel.setForeground(Color.black);
        radiusLabel.setFont(serif12);
        radiusLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 1;
        paramPanel.add(radiusLabel, gbc6);

        radiusText = new JTextField(10);
        radiusText.setText(String.valueOf(radius));
        radiusText.setFont(serif12);
        radiusText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(radiusText, gbc6);
        
        processGroup = new ButtonGroup();
        fixedPoissonSameButton = new JRadioButton("Fixed offspring allocation Poisson same parents process", true);
        fixedPoissonSameButton.setFont(serif12);
        fixedPoissonSameButton.setForeground(Color.black);
        fixedPoissonSameButton.addActionListener(this);
        processGroup.add(fixedPoissonSameButton);
        gbc6.gridx = 0;
        gbc6.gridy = 2;
        paramPanel.add(fixedPoissonSameButton, gbc6);
        
        fixedPoissonDifferentButton = new JRadioButton("Fixed offspring allocation Poisson different parents process", false);
        fixedPoissonDifferentButton.setFont(serif12);
        fixedPoissonDifferentButton.setForeground(Color.black);
        fixedPoissonDifferentButton.addActionListener(this);
        processGroup.add(fixedPoissonDifferentButton);
        gbc6.gridx = 0;
        gbc6.gridy = 3;
        paramPanel.add(fixedPoissonDifferentButton, gbc6);
        
        randomPoissonSameButton = new JRadioButton("Random offspring allocation Poisson same parents process", false);
        randomPoissonSameButton.setFont(serif12);
        randomPoissonSameButton.setForeground(Color.black);
        randomPoissonSameButton.addActionListener(this);
        processGroup.add(randomPoissonSameButton);
        gbc6.gridx = 0;
        gbc6.gridy = 4;
        paramPanel.add(randomPoissonSameButton, gbc6);
        
        randomPoissonDifferentButton = new JRadioButton("Random offspring allocation Poisson different parents process", false);
        randomPoissonDifferentButton.setFont(serif12);
        randomPoissonDifferentButton.setForeground(Color.black);
        randomPoissonDifferentButton.addActionListener(this);
        processGroup.add(randomPoissonDifferentButton);
        gbc6.gridx = 0;
        gbc6.gridy = 5;
        paramPanel.add(randomPoissonDifferentButton, gbc6);
        
        MaternSameButton = new JRadioButton("Matern same parents process", false);
        MaternSameButton.setFont(serif12);
        MaternSameButton.setForeground(Color.black);
        MaternSameButton.addActionListener(this);
        processGroup.add(MaternSameButton);
        gbc6.gridx = 0;
        gbc6.gridy = 6;
        paramPanel.add(MaternSameButton, gbc6);
        
        MaternDifferentButton = new JRadioButton("Matern different parents process", false);
        MaternDifferentButton.setFont(serif12);
        MaternDifferentButton.setForeground(Color.black);
        MaternDifferentButton.addActionListener(this);
        processGroup.add(MaternDifferentButton);
        gbc6.gridx = 0;
        gbc6.gridy = 7;
        paramPanel.add(MaternDifferentButton, gbc6);
        
        inhomogeneousPoissonButton = new JRadioButton("Inhomogeneous Poisson process", false);
        inhomogeneousPoissonButton.setFont(serif12);
        inhomogeneousPoissonButton.setForeground(Color.black);
        inhomogeneousPoissonButton.addActionListener(this);
        processGroup.add(inhomogeneousPoissonButton);
        gbc6.gridx = 0;
        gbc6.gridy = 8;
        paramPanel.add(inhomogeneousPoissonButton, gbc6);
        
        segregationButton = new JRadioButton("Segregation alternative", false);
        segregationButton.setFont(serif12);
        segregationButton.setForeground(Color.black);
        segregationButton.addActionListener(this);
        processGroup.add(segregationButton);
        gbc6.gridx = 0;
        gbc6.gridy = 9;
        paramPanel.add(segregationButton, gbc6);
        
        associationButton = new JRadioButton("Association alternative", false);
        associationButton.setFont(serif12);
        associationButton.setForeground(Color.black);
        associationButton.addActionListener(this);
        processGroup.add(associationButton);
        gbc6.gridx = 0;
        gbc6.gridy = 10;
        paramPanel.add(associationButton, gbc6);
        
        numParentsLabel = new JLabel("Number of parents ");
        numParentsLabel.setForeground(Color.black);
        numParentsLabel.setFont(serif12);
        numParentsLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy =11;
        paramPanel.add(numParentsLabel, gbc6);
        
        numParentsText = new JTextField(3);
        numParentsText.setText(String.valueOf(numParents));
        numParentsText.setFont(serif12);
        numParentsText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(numParentsText, gbc6);
        
        numOffspring1Label = new JLabel("Number of Type 1 offspring ");
        numOffspring1Label.setForeground(Color.black);
        numOffspring1Label.setFont(serif12);
        numOffspring1Label.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 12;
        paramPanel.add(numOffspring1Label, gbc6);
        
        numOffspring1Text = new JTextField(10);
        numOffspring1Text.setText(String.valueOf(numOffspring1));
        numOffspring1Text.setFont(serif12);
        numOffspring1Text.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(numOffspring1Text, gbc6);
        
        numOffspring2Label = new JLabel("Number of Type 2 offspring ");
        numOffspring2Label.setForeground(Color.black);
        numOffspring2Label.setFont(serif12);
        numOffspring2Label.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 13;
        paramPanel.add(numOffspring2Label, gbc6);
        
        numOffspring2Text = new JTextField(10);
        numOffspring2Text.setText(String.valueOf(numOffspring2));
        numOffspring2Text.setFont(serif12);
        numOffspring2Text.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(numOffspring2Text, gbc6);
        
        normalizedStdDevLabel = new JLabel("Normalized standard deviation ");
        normalizedStdDevLabel.setForeground(Color.black);
        normalizedStdDevLabel.setFont(serif12);
        normalizedStdDevLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 14;
        paramPanel.add(normalizedStdDevLabel, gbc6);
        
        normalizedStdDevText = new JTextField(10);
        normalizedStdDevText.setText(String.valueOf(normalizedStdDev));
        normalizedStdDevText.setFont(serif12);
        normalizedStdDevText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(normalizedStdDevText, gbc6);
        
        parentPoissonNormalizedMeanLabel = new JLabel("Parent Poisson normalized mean ");
        parentPoissonNormalizedMeanLabel.setForeground(Color.black);
        parentPoissonNormalizedMeanLabel.setFont(serif12);
        parentPoissonNormalizedMeanLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 15;
        paramPanel.add(parentPoissonNormalizedMeanLabel, gbc6);
        
        parentPoissonNormalizedMeanText = new JTextField(10);
        parentPoissonNormalizedMeanText.setText(String.valueOf(parentPoissonNormalizedMean));
        parentPoissonNormalizedMeanText.setFont(serif12);
        parentPoissonNormalizedMeanText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(parentPoissonNormalizedMeanText, gbc6);
        
        normalizedDiscRadiusLabel = new JLabel("Normalized disc radius ");
        normalizedDiscRadiusLabel.setForeground(Color.black);
        normalizedDiscRadiusLabel.setFont(serif12);
        normalizedDiscRadiusLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 16;
        paramPanel.add(normalizedDiscRadiusLabel, gbc6);
        
        normalizedDiscRadiusText = new JTextField(10);
        normalizedDiscRadiusText.setText(String.valueOf(normalizedDiscRadius));
        normalizedDiscRadiusText.setFont(serif12);
        normalizedDiscRadiusText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(normalizedDiscRadiusText, gbc6);
        
        numPoints1Label = new JLabel("Number of type 1 objects before deletion ");
        numPoints1Label.setForeground(Color.black);
        numPoints1Label.setFont(serif12);
        numPoints1Label.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 17;
        paramPanel.add(numPoints1Label, gbc6);
        
        numPoints1Text = new JTextField(10);
        numPoints1Text.setText(String.valueOf(numPoints1));
        numPoints1Text.setFont(serif12);
        numPoints1Text.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(numPoints1Text, gbc6);
        
        numPoints2Label = new JLabel("Number of type 2 objects before deletion ");
        numPoints2Label.setForeground(Color.black);
        numPoints2Label.setFont(serif12);
        numPoints2Label.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 18;
        paramPanel.add(numPoints2Label, gbc6);
        
        numPoints2Text = new JTextField(10);
        numPoints2Text.setText(String.valueOf(numPoints2));
        numPoints2Text.setFont(serif12);
        numPoints2Text.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(numPoints2Text, gbc6);
        
        inhomogeneousGroup = new ButtonGroup();
        sqrtXPlusYButton = new JRadioButton("lambda2(x,y) = n2*sqrt(x + y)", true);
        sqrtXPlusYButton.setFont(serif12);
        sqrtXPlusYButton.setForeground(Color.black);
        sqrtXPlusYButton.setEnabled(false);
        inhomogeneousGroup.add(sqrtXPlusYButton);
        gbc6.gridx = 0;
        gbc6.gridy = 19;
        paramPanel.add(sqrtXPlusYButton, gbc6);
        
        sqrtXTimesYButton = new JRadioButton("lambda2(x,y) = n2*sqrt(x * y)", false);
        sqrtXTimesYButton.setFont(serif12);
        sqrtXTimesYButton.setForeground(Color.black);
        sqrtXTimesYButton.setEnabled(false);
        inhomogeneousGroup.add(sqrtXTimesYButton);
        gbc6.gridx = 0;
        gbc6.gridy = 20;
        paramPanel.add(sqrtXTimesYButton, gbc6);
        
        absXMinusYButton = new JRadioButton("lambda2(x,y) = n2*|x - y|", true);
        absXMinusYButton.setFont(serif12);
        absXMinusYButton.setForeground(Color.black);
        absXMinusYButton.setEnabled(false);
        inhomogeneousGroup.add(absXMinusYButton);
        gbc6.gridx = 0;
        gbc6.gridy = 21;
        paramPanel.add(absXMinusYButton, gbc6);
        
        segregationLabel = new JLabel("Segregation parameter (0.0 - 1.0)");
        segregationLabel.setForeground(Color.black);
        segregationLabel.setFont(serif12);
        segregationLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy = 22;
        paramPanel.add(segregationLabel, gbc6);
        
        segregationText = new JTextField(10);
        segregationText.setText(String.valueOf(segregation));
        segregationText.setFont(serif12);
        segregationText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(segregationText, gbc6);

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

        if (!testParameter(dimText.getText(), 5, 1000000)) {
            dimText.requestFocus();
            dimText.selectAll();

            return false;
        } else {
            dim = Integer.valueOf(dimText.getText()).intValue();
        }
        
        if (!testParameter(radiusText.getText(), 0, 1000000)) {
            radiusText.requestFocus();
            radiusText.selectAll();

            return false;
        } else {
            radius = Integer.valueOf(radiusText.getText()).intValue();
        }
        
        
        
        if (fixedPoissonSameButton.isSelected() || fixedPoissonDifferentButton.isSelected() ||
            randomPoissonSameButton.isSelected() || randomPoissonDifferentButton.isSelected()) {
            if (!testParameter(numParentsText.getText(), 1, 100000)) {
                numParentsText.requestFocus();
                numParentsText.selectAll();

                return false;
            } else {
                numParents = Integer.valueOf(numParentsText.getText()).intValue();
            }
            
            if (!testParameter(numOffspring1Text.getText(), 1, 100000)) {
                numOffspring1Text.requestFocus();
                numOffspring1Text.selectAll();
                return false;
            }
            else {
                numOffspring1 = Integer.valueOf(numOffspring1Text.getText()).intValue();
            }
            if (!testParameter(numOffspring2Text.getText(), 1, 100000)) {
                numOffspring2Text.requestFocus();
                numOffspring2Text.selectAll();
                return false;
            }
            else {
                numOffspring2 = Integer.valueOf(numOffspring2Text.getText()).intValue();
            }
            
            if (!testParameter(normalizedStdDevText.getText(), 0.01, 1.0)) {
                MipavUtil.displayError("Normalized standard deviation must be between 0.01 and 1.0");
                normalizedStdDevText.requestFocus();
                normalizedStdDevText.selectAll();
                return false;
            }
            else {
                normalizedStdDev = Double.valueOf(normalizedStdDevText.getText()).doubleValue();
            }
            
            if (fixedPoissonSameButton.isSelected()){
                if ((numOffspring1 < numParents) || ((numOffspring1 % numParents) != 0)) {
                    MipavUtil.displayError("Offspring 1 must be a positive integer multiple of the number of parents");
                    numOffspring1Text.requestFocus();
                    numOffspring1Text.selectAll();
                    return false;
                }
                
                if ((numOffspring2 < numParents) || ((numOffspring2 % numParents) != 0)) {
                    MipavUtil.displayError("Offspring 2 must be a positive integer multiple of the number of parents");
                    numOffspring2Text.requestFocus();
                    numOffspring2Text.selectAll();
                    return false;
                }
                process = AlgorithmTwoClassGeneration.FIXED_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS;  
            }
            else if (fixedPoissonDifferentButton.isSelected()) {
                if ((numOffspring1 < numParents) || ((numOffspring1 % numParents) != 0)) {
                    MipavUtil.displayError("Offspring 1 must be a positive integer multiple of the number of parents");
                    numOffspring1Text.requestFocus();
                    numOffspring1Text.selectAll();
                    return false;
                }
                
                if ((numOffspring2 < numParents) || ((numOffspring2 % numParents) != 0)) {
                    MipavUtil.displayError("Offspring 2 must be a positive integer multiple of the number of parents");
                    numOffspring2Text.requestFocus();
                    numOffspring2Text.selectAll();
                    return false;
                }
                process = AlgorithmTwoClassGeneration.FIXED_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS;
            }
            else if (randomPoissonSameButton.isSelected()){
                process = AlgorithmTwoClassGeneration.RANDOM_OFFSPRING_ALLOCATION_POISSON_SAME_PARENTS;
            }
            else {
                process = AlgorithmTwoClassGeneration.RANDOM_OFFSPRING_ALLOCATION_POISSON_DIFFERENT_PARENTS;
                
            }
        }
        else if (MaternSameButton.isSelected() || MaternDifferentButton.isSelected()) {
            
            if (!testParameter(parentPoissonNormalizedMeanText.getText(), 1.0, 50.0)) {
                MipavUtil.displayError("Parent Poisson normalized mean must be between 1.0 and 50.0");
                parentPoissonNormalizedMeanText.requestFocus();
                parentPoissonNormalizedMeanText.selectAll();
                return false;
            }
            else {
                parentPoissonNormalizedMean = Double.valueOf(parentPoissonNormalizedMeanText.getText()).doubleValue();
            }
            
            if (!testParameter(numOffspring1Text.getText(), 1, 100000)) {
                numOffspring1Text.requestFocus();
                numOffspring1Text.selectAll();
                return false;
            }
            else {
                numOffspring1 = Integer.valueOf(numOffspring1Text.getText()).intValue();
            }
            
            if (!testParameter(numOffspring2Text.getText(), 1, 100000)) {
                numOffspring2Text.requestFocus();
                numOffspring2Text.selectAll();
                return false;
            }
            else {
                numOffspring2 = Integer.valueOf(numOffspring2Text.getText()).intValue();
            }
            
            if (!testParameter(normalizedDiscRadiusText.getText(), 0.01, 1.0)) {
                MipavUtil.displayError("Normalized disc radius must be between 0.01 and 1.0");
                normalizedDiscRadiusText.requestFocus();
                normalizedDiscRadiusText.selectAll();
                return false;
            }
            else {
                normalizedDiscRadius = Double.valueOf(normalizedDiscRadiusText.getText()).doubleValue();
            }
            
            if (MaternSameButton.isSelected()) {
                process = AlgorithmTwoClassGeneration.MATERN_SAME_PARENTS;
            }
            else {
                process = AlgorithmTwoClassGeneration.MATERN_DIFFERENT_PARENTS;
            }
        }
        else if (inhomogeneousPoissonButton.isSelected()){
            process = AlgorithmTwoClassGeneration.INHOMOGENEOUS_POISSON;
            
            if (!testParameter(numPoints1Text.getText(), 10, 10000)) {
                 MipavUtil.displayError("Number of type 1 objects before deletion must be between 10 and 10000");
                 numPoints1Text.requestFocus();
                 numPoints1Text.selectAll();
                 return false;
            }
            else {
                numPoints1 = Integer.valueOf(numPoints1Text.getText()).intValue();
            }
            
            if (!testParameter(numPoints2Text.getText(), 10, 10000)) {
                MipavUtil.displayError("Number of type 2 objects before deletion must be between 10 and 10000");
                numPoints2Text.requestFocus();
                numPoints2Text.selectAll();
                return false;
           }
           else {
               numPoints2 = Integer.valueOf(numPoints2Text.getText()).intValue();
           }
            
            if (sqrtXPlusYButton.isSelected()) {
                inhomogeneous = AlgorithmTwoClassGeneration.SQRT_X_PLUS_Y;
            }
            else if (sqrtXTimesYButton.isSelected()) {
                inhomogeneous = AlgorithmTwoClassGeneration.SQRT_X_TIMES_Y;
            }
            else {
                inhomogeneous = AlgorithmTwoClassGeneration.ABS_X_MINUS_Y;
            }
        }
        else if (segregationButton.isSelected()) {
            process = AlgorithmTwoClassGeneration.SEGREGATION_ALTERNATIVE;
            
            if (!testParameter(numOffspring1Text.getText(), 1, 100000)) {
                numOffspring1Text.requestFocus();
                numOffspring1Text.selectAll();
                return false;
            }
            else {
                numOffspring1 = Integer.valueOf(numOffspring1Text.getText()).intValue();
            }
            if (!testParameter(numOffspring2Text.getText(), 1, 100000)) {
                numOffspring2Text.requestFocus();
                numOffspring2Text.selectAll();
                return false;
            }
            else {
                numOffspring2 = Integer.valueOf(numOffspring2Text.getText()).intValue();
            }
            
            if (!testParameter(segregationText.getText(), 0.0, 1.0)) {
                segregationText.requestFocus();
                segregationText.selectAll();
                return false;
            }
            else {
                segregation = Double.valueOf(segregationText.getText()).doubleValue();
            }
        }
        else if (associationButton.isSelected()) {
            process = AlgorithmTwoClassGeneration.ASSOCIATION_ALTERNATIVE;
            
            if (!testParameter(numOffspring1Text.getText(), 1, 100000)) {
                numOffspring1Text.requestFocus();
                numOffspring1Text.selectAll();
                return false;
            }
            else {
                numOffspring1 = Integer.valueOf(numOffspring1Text.getText()).intValue();
            }
            if (!testParameter(numOffspring2Text.getText(), 1, 100000)) {
                numOffspring2Text.requestFocus();
                numOffspring2Text.selectAll();
                return false;
            }
            else {
                numOffspring2 = Integer.valueOf(numOffspring2Text.getText()).intValue();
            }
            
            if (!testParameter(normalizedDiscRadiusText.getText(), 0.01, 1.0)) {
                MipavUtil.displayError("Normalized disc radius must be between 0.01 and 1.0");
                normalizedDiscRadiusText.requestFocus();
                normalizedDiscRadiusText.selectAll();
                return false;
            }
            else {
                normalizedDiscRadius = Double.valueOf(normalizedDiscRadiusText.getText()).doubleValue();
            }
        }

        return true;
    }
}
