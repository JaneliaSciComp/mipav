package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
public class JDialogXcosmEM extends JDialogBase implements AlgorithmInterface,
    ScriptableInterface {

    private ModelImage originalImage;
    private ModelImage psfImage = null;
    private ModelImage resultImage = null; // result image
    private ViewUserInterface UI = null;

    private JLabel labelImage;
    private JComboBox imageComboBox;

    private JTextField textWindowLowerLimit;
    private int windowLowerLimit = 0;

    private JRadioButton radioEstimateDecay;
    private boolean estimateDecay = false;

    private JTextField textDecayConstant;
    private float decayConstant = 1.0f;

    private JTextField textPercentIterationsOriginalSize;
    int percentIterationsOriginalSize = 100;
    private JTextField textPercentIterationsHalfSize;
    int percentIterationsHalfSize = 0;
    private JTextField textPercentIterationsQuaterSize;
    int percentIterationsQuaterSize = 0;

    private JTextField textTotalNumberIterations;
    private int totalNumberIterations = 100;
    private JTextField textBackupNumberIterations;
    private int backupNumberIterations = 50;

    private JRadioButton radioIntensity, radioRoughness;
    private boolean penaltyIntensity = true;
    private JTextField textPenaltyValue;
    private float penaltyValue = 0.0f;

    private JRadioButton radioEntireImage, radioVOIRegion;
    private boolean entireImage = true;

    private long start;
    private long end;

    AlgorithmXcosmEM xcosmEMAlgo;

    /**
     *  Creates new dialog.
     *  @param theParentFrame    Parent frame
     *  @param im                Source image
     */
    public JDialogXcosmEM(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);

        originalImage = im;
        UI = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    private void init() {
        setForeground(Color.black);
        setTitle("Xcosm Expectation Maximum Restoration");
        JPanel parameterPanel = new JPanel(new GridBagLayout());

        parameterPanel.setForeground(Color.black);
        parameterPanel.setBorder(buildTitledBorder("Parameters"));

        // imageComboBox
        String matchName = originalImage.getImageName();
        labelImage = new JLabel("Restoration of [" + matchName + "] with PSF:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        imageComboBox = buildComboBox(originalImage);
        imageComboBox.addItemListener(this);

        UI = originalImage.getUserInterface();
        String selectedName = (String) imageComboBox.getSelectedItem();
        if (selectedName == null) {
          MipavUtil.displayError("No Point Spread Function Image");
          return;
        }
        psfImage = UI.getRegisteredImageByName(selectedName);

        // Window lower limit in Z
        JLabel labelWindowLowerLimit = new JLabel("Window lower limit in Z ");
        labelWindowLowerLimit.setFont(serif12);

        textWindowLowerLimit = new JTextField();
        textWindowLowerLimit.setColumns(5);
        textWindowLowerLimit.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textWindowLowerLimit.setHorizontalAlignment(JTextField.RIGHT);
        textWindowLowerLimit.setText( Integer.toString( windowLowerLimit ) );
        textWindowLowerLimit.setFont(serif12);

        // Estimate Decay
        radioEstimateDecay = new JRadioButton("Estimate Decay", estimateDecay);
        radioEstimateDecay.setFont(MipavUtil.font12);
        radioEstimateDecay.addActionListener(this);

        // Decay Constant
        JLabel labelDecayConstant = new JLabel("Decay Constant ");
        labelDecayConstant.setFont(serif12);

        textDecayConstant = new JTextField();
        textDecayConstant.setColumns(5);
        textDecayConstant.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textDecayConstant.setHorizontalAlignment(JTextField.RIGHT);
        textDecayConstant.setText( Float.toString( decayConstant ) );
        textDecayConstant.setFont(serif12);

        // Percent Iterations at Original Size
        JLabel labelPercentIterationsOriginalSize = new JLabel("Original Size Iteration Percentage");
        labelPercentIterationsOriginalSize.setFont(serif12);

        textPercentIterationsOriginalSize= new JTextField();
        textPercentIterationsOriginalSize.setColumns(5);
        textPercentIterationsOriginalSize.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textPercentIterationsOriginalSize.setHorizontalAlignment(JTextField.RIGHT);
        textPercentIterationsOriginalSize.setText( Integer.toString( percentIterationsOriginalSize ) );
        textPercentIterationsOriginalSize.setFont(serif12);

        // Percent Iterations at oneHalf Size
        JLabel labelPercentIterationsHalfSize = new JLabel("Half Size Iteration Percentage");
        labelPercentIterationsHalfSize.setFont(serif12);

        textPercentIterationsHalfSize = new JTextField();
        textPercentIterationsHalfSize.setColumns(5);
        textPercentIterationsHalfSize.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textPercentIterationsHalfSize.setHorizontalAlignment(JTextField.RIGHT);
        textPercentIterationsHalfSize.setText( Integer.toString( percentIterationsHalfSize ) );
        textPercentIterationsHalfSize.setFont(serif12);

        // Percent Iterations at Quater Size
        JLabel labelPercentIterationsQuaterSize = new JLabel("Quater Size Iteration Percentage");
        labelPercentIterationsQuaterSize.setFont(serif12);

        textPercentIterationsQuaterSize = new JTextField();
        textPercentIterationsQuaterSize.setColumns(5);
        textPercentIterationsQuaterSize.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textPercentIterationsQuaterSize.setHorizontalAlignment(JTextField.RIGHT);
        textPercentIterationsQuaterSize.setText( Integer.toString( percentIterationsQuaterSize ) );
        textPercentIterationsQuaterSize.setFont(serif12);


        // Number of Iterations
        JLabel labelTotalNumberIterations = new JLabel("Number of Iterations");
        labelTotalNumberIterations.setFont(serif12);

        textTotalNumberIterations = new JTextField();
        textTotalNumberIterations.setColumns(5);
        textTotalNumberIterations.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textTotalNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
        textTotalNumberIterations.setText( Integer.toString ( totalNumberIterations ) );
        textTotalNumberIterations.setFont(serif12);


        // Backup Number of Iterations
        JLabel labelBackupNumberIterations = new JLabel("Backup Number of Iterations");
        labelBackupNumberIterations.setFont(serif12);

        textBackupNumberIterations = new JTextField();
        textBackupNumberIterations.setColumns(5);
        textBackupNumberIterations.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textBackupNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
        textBackupNumberIterations.setText( Integer.toString ( backupNumberIterations ) );
        textBackupNumberIterations.setFont(serif12);


        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = gbc.WEST;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(3, 3, 3, 3);

        gbc.gridy = 0;
        gbc.gridx = 0;
        parameterPanel.add(labelImage, gbc);
        gbc.gridx = 1;
        parameterPanel.add(imageComboBox, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelWindowLowerLimit, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textWindowLowerLimit, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(radioEstimateDecay, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelDecayConstant, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textDecayConstant, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelPercentIterationsOriginalSize, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textPercentIterationsOriginalSize, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelPercentIterationsHalfSize, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textPercentIterationsHalfSize, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelPercentIterationsQuaterSize, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textPercentIterationsQuaterSize, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelTotalNumberIterations, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textTotalNumberIterations, gbc);


        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelBackupNumberIterations, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textBackupNumberIterations, gbc);




        ButtonGroup penaltyGroup = new ButtonGroup();

        radioIntensity = new JRadioButton("Intensity Penalty", penaltyIntensity);
        radioIntensity.setFont(MipavUtil.font12);
//        radioIntensity.setActionCommand("EntireImage");
        radioIntensity.addActionListener(this);
        penaltyGroup.add(radioIntensity);

        radioRoughness = new JRadioButton("Roughness Penalty", !penaltyIntensity);
        radioRoughness.setFont(MipavUtil.font12);
//        radioRoughness.setActionCommand("VOIRegion");
        radioRoughness.addActionListener(this);
        penaltyGroup.add(radioRoughness);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(radioIntensity, gbc);
        gbc.gridx = 1;
        parameterPanel.add(radioRoughness, gbc);


        // Penalty Value
        JLabel labelPenaltyValue = new JLabel("Penalty Value");
        labelPenaltyValue.setFont(serif12);

        textPenaltyValue = new JTextField();
        textPenaltyValue.setColumns(5);
        textPenaltyValue.setMaximumSize(labelWindowLowerLimit.getPreferredSize());
        textPenaltyValue.setHorizontalAlignment(JTextField.RIGHT);
        textPenaltyValue.setText( Float.toString ( penaltyValue ) );
        textPenaltyValue.setFont(serif12);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelPenaltyValue, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textPenaltyValue, gbc);




        ButtonGroup regionGroup = new ButtonGroup();

        radioEntireImage = new JRadioButton("Entire image", entireImage);
        radioEntireImage.setFont(MipavUtil.font12);
        radioEntireImage.setActionCommand("EntireImage");
        radioEntireImage.addActionListener(this);
        regionGroup.add(radioEntireImage);

        radioVOIRegion = new JRadioButton("VOI regions", !entireImage);
        radioVOIRegion.setFont(MipavUtil.font12);
        radioVOIRegion.setActionCommand("VOIRegion");
        radioVOIRegion.addActionListener(this);
        regionGroup.add(radioVOIRegion);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(radioEntireImage, gbc);
        gbc.gridx = 1;
        parameterPanel.add(radioVOIRegion, gbc);

        gbc.gridx = 0;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.fill = gbc.HORIZONTAL;

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridy = 0;
        mainPanel.add(parameterPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);

    } // end init()





    /**
     *	Builds a list of images.  Returns combobox.
     *   List must be all color or all black and white.
     *	@return	Newly created combo box.
     */
    private JComboBox buildComboBox(ModelImage image) {
      ViewUserInterface UI;
      ModelImage nextImage;
      boolean doAdd;
      int i;

      JComboBox comboBox = new JComboBox();
      comboBox.setFont(serif12);
      comboBox.setBackground(Color.white);

      UI = image.getUserInterface();
      Enumeration names = UI.getRegisteredImageNames();

      while (names.hasMoreElements()) {
        String name = (String) names.nextElement();
        if (!name.equals(image.getImageName())) {
          nextImage = UI.getRegisteredImageByName(name);
          if (UI.getFrameContainingImage(nextImage) != null) {
            if ( (image.isColorImage() == nextImage.isColorImage()) &&
                (image.getNDims() == nextImage.getNDims())) {
              doAdd = true;
              for (i = 0; i < image.getNDims(); i++) {
                if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                  doAdd = false;
                }
              }
              if (doAdd) {
                comboBox.addItem(name);
              }
            }
          }
        }
      }
      return comboBox;
    }



    /**
     *  Closes dialog box when the OK button is pressed and calls the algorithm.
     *  @param event       Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10086");
        } // end if()-else
    } // end actionPerformed(...)


    private boolean setVariables() {
        String tmpStr;

        UI = originalImage.getUserInterface();
        String selectedName = (String) imageComboBox.getSelectedItem();
        psfImage = UI.getRegisteredImageByName(selectedName);
        if (psfImage == null) {
          return false;
        }


        tmpStr = textWindowLowerLimit.getText();
        windowLowerLimit = Integer.parseInt(tmpStr);

        estimateDecay = radioEstimateDecay.isSelected();

        tmpStr = textDecayConstant.getText();
        decayConstant = Float.parseFloat(tmpStr);

        tmpStr = textPercentIterationsOriginalSize.getText();
        percentIterationsOriginalSize = Integer.parseInt(tmpStr);

        tmpStr = textPercentIterationsHalfSize.getText();
        percentIterationsHalfSize = Integer.parseInt(tmpStr);

        tmpStr = textPercentIterationsQuaterSize.getText();
        percentIterationsQuaterSize = Integer.parseInt(tmpStr);

        tmpStr = textTotalNumberIterations.getText();
        totalNumberIterations = Integer.parseInt(tmpStr);

        tmpStr = textBackupNumberIterations.getText();
        backupNumberIterations = Integer.parseInt(tmpStr);

        penaltyIntensity = radioIntensity.isSelected();

        tmpStr = textPenaltyValue.getText();
        penaltyValue = Float.parseFloat(tmpStr);

        entireImage = radioEntireImage.isSelected();

        return true;
    } // end setVariables()


    /**
     *	Once all the necessary variables are set, call the mean
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {
        start = System.currentTimeMillis();

        String name;
        name = makeImageName(originalImage.getImageName(), "_em");

        try {
            if (originalImage.isColorImage()) {
                resultImage = new ModelImage(originalImage.getType(),
                                             originalImage.getExtents(), name, UI);
            } else {
                resultImage = new ModelImage(ModelStorageBase.FLOAT,
                                             originalImage.getExtents(), name, UI);
            }

            xcosmEMAlgo = new AlgorithmXcosmEM(resultImage, originalImage, psfImage,
                          windowLowerLimit, estimateDecay, decayConstant,
                          percentIterationsOriginalSize,
                          percentIterationsHalfSize,
                          percentIterationsQuaterSize,
                          totalNumberIterations, backupNumberIterations,
                          penaltyIntensity, penaltyValue);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            xcosmEMAlgo.addListener(this);

            if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (xcosmEMAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError(
                        "A thread is already running on this object");
                }
            } else {
                xcosmEMAlgo.setActiveImage(isActiveImage);
                if (!UI.isAppFrameVisible()) {
                    xcosmEMAlgo.setProgressBarVisible(false);
                }
                xcosmEMAlgo.run();
            } // end if (runInSeparateThread)

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError(
                "JDialogXcosmEM: unable to allocate enough memory");
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }
            return;
        } // end try()=catch()

        dispose();
    } // end callAlgorithm()


    public void algorithmPerformed(AlgorithmBase algorithm) {
        ViewJFrameImage imageFrame = null;

        if ((algorithm instanceof AlgorithmXcosmEM
             && xcosmEMAlgo.isCompleted() == true)
            && resultImage != null) {

            updateFileInfo(originalImage, resultImage);
            resultImage.clearMask();
            try {
                imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError(
                    "Out of memory: unable to open new frame");
            }

            insertScriptLine(algorithm);

            xcosmEMAlgo.finalize();
            xcosmEMAlgo = null;
        }
    } // end algorithmPerformed(...)


    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws
        IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage(srcImageKey);

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
//        callAlgorithm();
        if (!srcImageKey.equals(destImageKey)) {
//            parser.putVariable( destImageKey, getResultImage().getImageName() );
        }
    }


    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {
        if (algo.isCompleted()) {
//            if ( userInterface.isScriptRecording() ) {
//                //check to see if the match image is already in the ImgTable
//                if ( userInterface.getScriptDialog().getImgTableVar( srcImage.getImageName() ) == null ) {
//                    if ( userInterface.getScriptDialog().getActiveImgTableVar( srcImage.getImageName() ) == null ) {
//                        userInterface.getScriptDialog().putActiveVar( srcImage.getImageName() );
//                    }
//                }

//                userInterface.getScriptDialog().append(
//                        "CoherenceEnhancingDiffusion "
//                                + userInterface.getScriptDialog().getVar( srcImage.getImageName() ) + " " );
            //if (displayLoc == NEW) {
//                userInterface.getScriptDialog().putVar( resultImage.getImageName() );
//                userInterface.getScriptDialog().append(
//                        userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + numIterations + " "
//                        + diffusitivityDenom + " " + derivativeScale + " " + gaussianScale + " " + do25D + " "
//                        + entireImage + "\n" );
            //}
            /*else {
             userInterface.getScriptDialog().append(userInterface.
             getScriptDialog().getVar(image.getImageName()) + " " +
             + numIterations + " " + diffusitivityDenom + " " + derivativeScale +
             " " + gaussianScale +
             " " + do25D + "\n");
             }*/
//            }
        }
    }


} // end JDialogXcosmEM
