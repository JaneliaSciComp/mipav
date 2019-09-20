package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmHMRF_EM;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class JDialogHMRF_EM extends JDialogScriptableBase implements AlgorithmInterface {
    
    private final static int EQUAL_WEIGHTS = 1;
    
    private final static int GRAPHIC_WEIGHTS = 2;
    
    /** Source image. */
    private ModelImage image;
    
    /** result image **/
    private ModelImage resultImage;
    
    private AlgorithmHMRF_EM alg;
    
    private JRadioButton equalButton;
    
    private JRadioButton graphicsButton;
    
    private int colorWeighting = EQUAL_WEIGHTS;
    
    private JTextField textClusters;
    
    private int numberClusters;
    
    private JTextField textGauss;
    
    private float gaussianSigma = 3.0f;
    
    private JTextField textMaxEMIterations;
    
    private int maxEMIterations = 10;
    
    private JTextField textMaxMAPIterations;
    
    private int maxMAPIterations = 10;
    
    private JCheckBox showMAPplotCheckBox;
    
    private boolean showMAPplot = false;
    
    String fileNameBase = null;
    
    private JLabel resultsFileNameLabel;
    
    private JTextField resultsFileNameText;
    
    private String resultsFileName;

    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogHMRF_EM() { }


    /**
     * Creates a new JDialogHMRF_EM object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogHMRF_EM(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * init
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        
        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
        setForeground(Color.black);
        setTitle("Hidden Markov Random Field Segmentation");
        
        if (image.isColorImage()) {
            JPanel optionsPanel = new JPanel(new GridLayout(2, 1));
            optionsPanel.setForeground(Color.black);
            optionsPanel.setBorder(buildTitledBorder("Weighting methods"));
            mainPanel.add(optionsPanel, gbc);
            gbc.gridy++;

            ButtonGroup weightGroup = new ButtonGroup();
            equalButton = new JRadioButton("Equal weights", true); // default
            equalButton.setFont(serif12);
            equalButton.addActionListener(this);
            weightGroup.add(equalButton);
            optionsPanel.add(equalButton, BorderLayout.NORTH);

            graphicsButton = new JRadioButton("Computer graphics", false);
            graphicsButton.setFont(serif12);
            graphicsButton.addActionListener(this);
            weightGroup.add(graphicsButton);
            optionsPanel.add(graphicsButton, BorderLayout.CENTER);    
        } // if (image.isColorImage())
        
        JLabel clustersLabel = new JLabel("Choose the number of clusters");
        clustersLabel.setForeground(Color.black);
        clustersLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        mainPanel.add(clustersLabel, gbc);
        
        textClusters = new JTextField(10);
        textClusters.setText("3");
        textClusters.setForeground(Color.black);
        textClusters.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textClusters, gbc);
        gbc.gridy++;
        
        JLabel labelGauss = new JLabel("Gaussian std. dev. ");
        labelGauss.setForeground(Color.black);
        labelGauss.setFont(serif12);
        gbc.gridx = 0;
        mainPanel.add(labelGauss, gbc);
        
        textGauss = new JTextField();
        textGauss.setText("3.0");
        textGauss.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textGauss, gbc);
        gbc.gridy++;
        
        JLabel maxEMIterationsLabel = new JLabel("Choose the maximum number of EM iterations");
        maxEMIterationsLabel.setForeground(Color.black);
        maxEMIterationsLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        mainPanel.add(maxEMIterationsLabel, gbc);
        
        textMaxEMIterations = new JTextField(10);
        textMaxEMIterations.setText("10");
        textMaxEMIterations.setForeground(Color.black);
        textMaxEMIterations.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxEMIterations, gbc);
        gbc.gridy++;
        
        JLabel maxMAPIterationsLabel = new JLabel("Choose the maximum number of MAP iterations");
        maxMAPIterationsLabel.setForeground(Color.black);
        maxMAPIterationsLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        mainPanel.add(maxMAPIterationsLabel, gbc);
        
        textMaxMAPIterations = new JTextField(10);
        textMaxMAPIterations.setText("10");
        textMaxMAPIterations.setForeground(Color.black);
        textMaxMAPIterations.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxMAPIterations, gbc);
        gbc.gridy++;
        
        showMAPplotCheckBox = new JCheckBox("Show a plot of every MAP run");
        showMAPplotCheckBox.setFont(serif12);
        showMAPplotCheckBox.setForeground(Color.black);
        showMAPplotCheckBox.setSelected(false);
        gbc.gridx = 0;
        mainPanel.add(showMAPplotCheckBox, gbc);
        gbc.gridy++;
        
        resultsFileNameLabel = new JLabel("Results file name:");
        resultsFileNameLabel.setForeground(Color.black);
        resultsFileNameLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        mainPanel.add(resultsFileNameLabel, gbc);
        String imageName = image.getImageFileName();
        int i = imageName.indexOf(".");
        if (i > 0) {
            fileNameBase = imageName.substring(0,i);
        }
        else {
            fileNameBase = new String(imageName);
        }
        resultsFileName = image.getImageDirectory() + fileNameBase + "_HMRF_EM.txt";
        resultsFileNameText = new JTextField(40);
        resultsFileNameText.setText(resultsFileName);
        resultsFileNameText.setForeground(Color.black);
        resultsFileNameText.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(resultsFileNameText, gbc);
        gbc.gridy++;
    
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }
    
    private boolean setVariables() {
        String tmpStr;
        
        if (image.isColorImage()) {
            if (equalButton.isSelected()) {
                colorWeighting = EQUAL_WEIGHTS;
            }
            else {
                colorWeighting = GRAPHIC_WEIGHTS;
            }
        }
        
        tmpStr = textClusters.getText();
        
        numberClusters = Integer.valueOf(tmpStr).intValue();
        if (numberClusters < 2) {
            MipavUtil.displayError("Must have at least 2 clusters");
            textClusters.requestFocus();
            textClusters.selectAll();
            return false;
        }
        
        tmpStr = textGauss.getText();

        if (testParameter(tmpStr, 0.01, 100.0)) {
            gaussianSigma = Float.valueOf(tmpStr).floatValue();
        } else {
            textGauss.requestFocus();
            textGauss.selectAll();

            return false;
        }
        
        tmpStr = textMaxEMIterations.getText();

        if (testParameter(tmpStr, 1.0, 1000.0)) {
            maxEMIterations = Integer.valueOf(tmpStr).intValue();
        } else {
            textMaxEMIterations.requestFocus();
            textMaxEMIterations.selectAll();

            return false;
        }
        
        tmpStr = textMaxMAPIterations.getText();

        if (testParameter(tmpStr, 1.0, 1000.0)) {
            maxMAPIterations = Integer.valueOf(tmpStr).intValue();
        } else {
            textMaxMAPIterations.requestFocus();
            textMaxMAPIterations.selectAll();

            return false;
        }
        
        resultsFileName = resultsFileNameText.getText();
        
        return true;
        
    }
    
    public void actionPerformed(ActionEvent event) {
        
        String command = event.getActionCommand();
         if (command.equals("OK")) {
             if (setVariables()) {
                 callAlgorithm();
             }
         } 
         else if (command.equals("Cancel")) {
             if (image != null) {
                 image.disposeLocal();
                 image = null;
             }
             dispose();
         } else if (command.equals("Help")) {
                //MipavUtil.showHelp("");
         }
    }
    
    protected void callAlgorithm() {
    
        resultImage = new ModelImage(ModelStorageBase.BYTE, image.getExtents(),
                fileNameBase +  "_HMRF_EM"); 
        resultImage.getFileInfo()[0].setResolutions(image.getFileInfo()[0].getResolutions());
        
        try {
            
            alg = new AlgorithmHMRF_EM(resultImage, image, colorWeighting, numberClusters, gaussianSigma,
                    maxEMIterations, maxMAPIterations, showMAPplot, resultsFileName);
            
            
            //This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            alg.addListener(this);
            
            createProgressBar(image.getImageName(), alg);
            
            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                alg.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog HMRF_EM: unable to allocate enough memory");

            return;
        }
    }
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmHMRF_EM) {
            image.clearMask();

            if ((alg.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }
        
        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        alg.finalize();
        alg = null;
        dispose();
    }
    
    /**
     * 
     * @param colorWeighting
     */
    public void setColorWeighting(int colorWeighting) {
        this.colorWeighting = colorWeighting;
    }
    
    /**
     * 
     * @param numberClusters
     */
    public void setNumberClusters(int numberClusters) {
        this.numberClusters = numberClusters;
    }
    
    /**
     * 
     * @param gaussianSigma
     */
    public void setGaussianSigma(float gaussianSigma) {
        this.gaussianSigma = gaussianSigma;
    }
    
    /**
     * 
     * @param maxEMIterations
     */
    public void setMaxEMIterations(int maxEMIterations) {
        this.maxEMIterations = maxEMIterations;
    }
    
    /**
     * 
     * @param maxMAPIterations
     */
    public void setMaxMAPIterations(int maxMAPIterations) {
        this.maxMAPIterations = maxMAPIterations;
    }
    
    /**
     * 
     * @param showMAPplot
     */
    public void setShowMAPplot(boolean showMAPplot) {
        this.showMAPplot = showMAPplot;
    }
    
    /**
     * set GUI from params
     */
    protected void setGUIFromParams(){
        image = scriptParameters.retrieveInputImage();
        setColorWeighting(scriptParameters.getParams().getInt("color_weighting"));
        setNumberClusters(scriptParameters.getParams().getInt("number_clusters"));
        setGaussianSigma(scriptParameters.getParams().getFloat("gaussian_sigma"));
        setMaxEMIterations(scriptParameters.getParams().getInt("max_em_iterations"));
        setMaxMAPIterations(scriptParameters.getParams().getInt("max_map_iterations"));
        setShowMAPplot(scriptParameters.getParams().getBoolean("show_map_plot"));
    }
    
    /**
     * store params from gui
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("color_weighting", colorWeighting));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_clusters", numberClusters));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gaussian_sigma", gaussianSigma));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_em_iterations", maxEMIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_map_iterations", maxMAPIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("show_map_plot", showMAPplot));
    }

}