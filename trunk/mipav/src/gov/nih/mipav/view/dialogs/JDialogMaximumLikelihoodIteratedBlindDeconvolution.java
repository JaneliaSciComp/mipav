package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog that launches the Maximum likelihood iterated blind deconvolution
 * algorithm. The dialog allows the user to set four parameters to the
 * algorithm:
 *
 * numberIterations: the number of times to iterate in the blind deconvolution algorithm
 * objectiveNumericalAperature: a property of the lense used to image the sample
 * wavelength: the wavelength of the reflected (for brightfield) or fluorescence light
 * refractiveIndex: the index of refraction of the sample.
 *
 * See AlgorithmMaximumLikelihoodIteratedBlindDeconvolution.java for algorithm details. 
 */
public class JDialogMaximumLikelihoodIteratedBlindDeconvolution
    extends JDialogBase
    implements AlgorithmInterface,
               ScriptableInterface
{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7390265931793649147L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The MaximumLikelihood Blind Deconvolution algorithm: */
    AlgorithmMaximumLikelihoodIteratedBlindDeconvolution ibdAlgor;

    /** The number of iterations for calculating the deconvolution: */
    private int numberIterations = 10;
    /** Display the deconvolved image every X images to show progress: */
    private int showProgress = 5;

    /** The following are physical properties of the imaging lense and
     * sample: */
    /** The object numerical aperature, a property of the lense used to image
     * the sample: */
    private float objectiveNumericalAperature = 1.4f;
    /** The reflected or fluroescence wavelength of the sample: */
    private float wavelength = 520f;
    /** The index of refraction of the sample: */
    private float refractiveIndex = 1.518f;

    /** When true the lense NA, wavelength, and refractive index are used in
     * the deconvolution process*/
    private boolean useMicroscopeSettings = true;

    /** The original data image to be reconstructed: */
    private ModelImage originalImage;

    /** User-Interface components: */
    /** User-Interface for entering the number of deconvolution iterations: */
    private JTextField textNumberIterations;
    /** User-Interface for entering the number of intermediate images to show: */
    private JTextField textShowProgress;
    /** User-Interface for entering the objective numerical aperature: */
    private JTextField textObjectiveNumericalAperature;
    /** User-Interface for entering the sample wavelength: */
    private JTextField textWavelength;
    /** User-Interface for entering the index of refraction: */
    private JTextField textRefractiveIndex;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new JDialogMaximumLikelihoodIteratedBlindDeconvolution object.
     *
     * @param  theParentFrame  The frame that launched this dialog
     * @param  im              The source image to be reconstructed
     */
    public JDialogMaximumLikelihoodIteratedBlindDeconvolution(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);

        originalImage = im;
        init();
    } // end JDialogMaximumLikelihoodIteratedBlindDeconvolution(...)

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK"))
        {
            if (setVariables())
            {
                callAlgorithm();
            }
        }
        else if (command.equals("Cancel"))
        {
            dispose();
        }
        else if (command.equals("Help"))
        {
            MipavUtil.showHelp("10086");
        } 
        else if (command.equals("MicroscopeOptions") )
        {
            useMicroscopeSettings = ((JCheckBox)event.getSource()).isSelected();
            textObjectiveNumericalAperature.setEnabled( useMicroscopeSettings );
            textWavelength.setEnabled( useMicroscopeSettings );
            textRefractiveIndex.setEnabled( useMicroscopeSettings );
        }// end if()-else
    } // end actionPerformed(...)


    /**
     * Called when the AlgorithmMaximumLikelihoodIteratedBlindDeconvolution
     * has finished processing the source image. If the algorithm is
     * completed, the resulting reconstructed image is displayed.
     *
     * @param  algorithm  The algorithm that has finished processing.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if ((algorithm
             instanceof AlgorithmMaximumLikelihoodIteratedBlindDeconvolution) &&
            (ibdAlgor.isCompleted() == true) )
        {

            ModelImage resultImage =
                ((AlgorithmMaximumLikelihoodIteratedBlindDeconvolution)algorithm).getReconstructedImage();
            
            updateFileInfo(originalImage, resultImage);
            resultImage.clearMask();

            /* Display the result: */
            try {
                new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            insertScriptLine(algorithm);

            ibdAlgor.finalize();
            ibdAlgor = null;
        }
    } // end algorithmPerformed(...)


    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) { }
    }


    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
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

        try { }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }
        
        setSeparateThread(false);

        if (!srcImageKey.equals(destImageKey)) { }
    }


    /**
     * Once all the necessary variables are set, call the mean algorithm based
     * on what type of image this is and whether or not there is a separate
     * destination image.
     */
    private void callAlgorithm() {
        String name;
        name = makeImageName(originalImage.getImageName(), "_ibd");

        try {
            ibdAlgor =
                new AlgorithmMaximumLikelihoodIteratedBlindDeconvolution(originalImage,
                                                                         numberIterations,
                                                                         showProgress,
                                                                         objectiveNumericalAperature,
                                                                         wavelength,
                                                                         refractiveIndex,
                                                                         useMicroscopeSettings
                                                                         );
            
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            ibdAlgor.addListener(this);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (ibdAlgor.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                ibdAlgor.setProgressBarVisible(false);
                ibdAlgor.run();
            } // end if (runInSeperateThread) {} else {}

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog MaximumLikelihoodIteratedBlindDeconvolution: unable to allocate enough memory");
            return;
        }

        dispose();
    } // end callAlgorithm()


    /**
     * Initializes the user-interface components of the dialog:
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Maximum Likelihood Iterated Blind Deconvolution");

        JPanel parameterPanel = new JPanel(new GridBagLayout());

        parameterPanel.setForeground(Color.black);
        parameterPanel.setBorder(buildTitledBorder("Parameters"));

        // Maximum number of iterations
        JLabel labelNumberIterations = new JLabel("Number of Iterations ");
        labelNumberIterations.setFont(serif12);

        textNumberIterations = new JTextField();
        textNumberIterations.setColumns(5);
        textNumberIterations.setMaximumSize(labelNumberIterations.getPreferredSize());
        textNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
        textNumberIterations.setText(Integer.toString(numberIterations));
        textNumberIterations.setFont(serif12);

        // Show deconvolved image, every X images:
        JLabel labelShowProgress = new JLabel("Display intermediate images every: ");
        labelShowProgress.setFont(serif12);

        textShowProgress = new JTextField();
        textShowProgress.setColumns(5);
        textShowProgress.setMaximumSize(labelShowProgress.getPreferredSize());
        textShowProgress.setHorizontalAlignment(JTextField.RIGHT);
        textShowProgress.setText(Integer.toString(showProgress));
        textShowProgress.setFont(serif12);

        // Objective numerical aperature
        JLabel labelObjectiveNumericalAperature =
            new JLabel("Objective Numerical Aperature ");
        labelObjectiveNumericalAperature.setFont(serif12);

        textObjectiveNumericalAperature = new JTextField();
        textObjectiveNumericalAperature.setColumns(5);
        textObjectiveNumericalAperature.setMaximumSize(labelObjectiveNumericalAperature.getPreferredSize());
        textObjectiveNumericalAperature.setHorizontalAlignment(JTextField.RIGHT);
        textObjectiveNumericalAperature.setText(Float.toString(objectiveNumericalAperature));
        textObjectiveNumericalAperature.setFont(serif12);

        // Wavelength
        JLabel labelWavelength =
            new JLabel("Wavelength (nm)");
        labelWavelength.setFont(serif12);

        textWavelength = new JTextField();
        textWavelength.setColumns(5);
        textWavelength.setMaximumSize(labelWavelength.getPreferredSize());
        textWavelength.setHorizontalAlignment(JTextField.RIGHT);
        textWavelength.setText(Float.toString(wavelength));
        textWavelength.setFont(serif12);

        // Refractive Index
        JLabel labelRefractiveIndex =
            new JLabel("Refractive Index ");
        labelRefractiveIndex.setFont(serif12);

        textRefractiveIndex = new JTextField();
        textRefractiveIndex.setColumns(5);
        textRefractiveIndex.setMaximumSize(labelRefractiveIndex.getPreferredSize());
        textRefractiveIndex.setHorizontalAlignment(JTextField.RIGHT);
        textRefractiveIndex.setText(Float.toString(refractiveIndex));
        textRefractiveIndex.setFont(serif12);

        JCheckBox checkAdvancedOptions = new JCheckBox( "Use microscope settings" );
        checkAdvancedOptions.setEnabled( true );
        checkAdvancedOptions.setSelected( true );
        checkAdvancedOptions.addActionListener( this );
        checkAdvancedOptions.setActionCommand( "MicroscopeOptions" );

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = gbc.WEST;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(3, 3, 3, 3);

        gbc.gridy = 0;
        gbc.gridx = 0;
        parameterPanel.add(labelNumberIterations, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textNumberIterations, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelShowProgress, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textShowProgress, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelObjectiveNumericalAperature, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textObjectiveNumericalAperature, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelWavelength, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textWavelength, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(labelRefractiveIndex, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textRefractiveIndex, gbc);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(checkAdvancedOptions, gbc);

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
    }


    /**
     * Gets the user-determined parameters for the algorithm from the
     * user-interface components.
     *
     * @return  true
     */
    private boolean setVariables() {
        String tmpStr;
        tmpStr = textNumberIterations.getText();
        numberIterations = Integer.parseInt(tmpStr);

        tmpStr = textShowProgress.getText();
        showProgress = Integer.parseInt(tmpStr);

        tmpStr = textObjectiveNumericalAperature.getText();
        objectiveNumericalAperature = Float.parseFloat(tmpStr);

        tmpStr = textWavelength.getText();
        wavelength = Float.parseFloat(tmpStr);

        tmpStr = textRefractiveIndex.getText();
        refractiveIndex = Float.parseFloat(tmpStr);

        return true;
    } // end setVariables()


} // end class JDialogMaximumLikelihoodIteratedBlindDeconvolution
