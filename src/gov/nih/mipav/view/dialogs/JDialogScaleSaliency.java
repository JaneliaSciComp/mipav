package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterDouble;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 *
 */
public class JDialogScaleSaliency extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields -----------------------------------------------------------------------------------------------\
    
    private static final int NORMAL_MODE = 1;
	
	private static final int PARZEN_WINDOW_MODE = 2;
	
	private static final int ANTI_ALIASED_SAMPLING_MODE = 3;
	
	private int mode = NORMAL_MODE;
	
	private ButtonGroup modeGroup;
	
	private JRadioButton normalButton;
	
	private JRadioButton parzenButton;
	
	private boolean scalarImage = true;
	
	private JRadioButton aaButton;

    /** DOCUMENT ME! */
    private AlgorithmScaleSaliency sAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int maximumRadius;

    /** DOCUMENT ME! */
    private JTextField maximumText;

    /** DOCUMENT ME! */
    private double sigma = 1.0;
    
    private JLabel sigmaLabel;
    
    private JTextField sigmanbinsText;
    
    JTextField minimumText;
    
    private int minimumRadius;
    
    JLabel nbinsLabel;
    
    private int nbins = 16;

    /** DOCUMENT ME! */
    private JTextField nbinsText;
    
    // Threshold on inter-scale saliency values (set between 0 and 2)
    // Setting wt high forces the selection of features that are more scale localized or isotropic.
    private double wt = 0.5;
    
    private JTextField interScaleSaliencyText;
    
    // Fraction of saliency maximum value used as threshold
    private double yt = 0.5;
    
    private JTextField saliencyText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogScaleSaliency object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogScaleSaliency(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
        scalarImage = (!image.isColorImage()) && (!image.isComplexImage());
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogScaleSaliency(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        scalarImage = (!image.isColorImage()) && (!image.isComplexImage());
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
            //MipavUtil.showWebHelp("Scale_Saliency");
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == normalButton) || (source == parzenButton) || (source == aaButton)) {
        	if (normalButton.isSelected()) {
        		sigmaLabel.setEnabled(false);
        		sigmanbinsText.setEnabled(false);
        		nbinsLabel.setEnabled(true);
        		nbinsText.setEnabled(true);
        		nbinsText.setText(String.valueOf(16));
        	}
        	else if (parzenButton.isSelected()) {
        		sigmaLabel.setEnabled(true);
        		sigmanbinsText.setEnabled(true);
        		nbinsLabel.setEnabled(false);
        		nbinsText.setEnabled(false);
        		nbinsText.setText(String.valueOf(256));
        	}
        	else {
        		sigmaLabel.setEnabled(false);
        		sigmanbinsText.setEnabled(false);
        		nbinsLabel.setEnabled(true);
        		nbinsText.setEnabled(true);
        		nbinsText.setText(String.valueOf(16));
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


        if (algorithm instanceof AlgorithmScaleSaliency) {
            Preferences.debug("Scale Saliency: " + algorithm.getElapsedTime());
            image.clearMask();

            

            // insertScriptLine(algorithm);
        } // if (algorithm instanceof AlgorithmScaleSaliency)

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
    protected void callAlgorithm() {

        try {
            // Make algorithm
            sAlgo = new AlgorithmScaleSaliency(image, minimumRadius,
            		    maximumRadius, mode, nbins, sigma, wt, yt);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            sAlgo.addListener(this);
            
            createProgressBar(image.getImageName(), sAlgo);

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
            MipavUtil.displayError("Dialog Scale Saliency: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel mainLabel;
        JLabel minimumLabel;
        JLabel maximumLabel;
        JLabel interScaleSaliencyLabel;
        JLabel saliencyLabel;
        setForeground(Color.black);
        setTitle("Scale Saliency");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Scale Saliency parameters"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;
        
        mainLabel = new JLabel("Circles salient features");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        paramPanel.add(mainLabel, gbc6);
        
        minimumLabel = new JLabel("Minimum circular radius");
        minimumLabel.setForeground(Color.black);
        minimumLabel.setFont(serif12);
        minimumLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(minimumLabel, gbc6);

        minimumText = new JTextField(10);
        minimumText.setText(String.valueOf(3));
        minimumText.setFont(serif12);
        minimumText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(minimumText, gbc6);

        maximumLabel = new JLabel("Maximum circular radius ");
        maximumLabel.setForeground(Color.black);
        maximumLabel.setFont(serif12);
        maximumLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(maximumLabel, gbc6);

        maximumText = new JTextField(10);
        maximumText.setText(String.valueOf(33));
        maximumText.setFont(serif12);
        maximumText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(maximumText, gbc6);
        
        modeGroup = new ButtonGroup();
        normalButton = new JRadioButton("Neither Parzen window nor Anti-aliased sampling", true);
        normalButton.setFont(serif12);
        normalButton.setForeground(Color.black);
        normalButton.addActionListener(this);
        modeGroup.add(normalButton);
        if (!scalarImage) {
        	normalButton.setEnabled(false);
        }
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(normalButton, gbc6);
        
        parzenButton = new JRadioButton("Parzen window", false);
        parzenButton.setFont(serif12);
        parzenButton.setForeground(Color.black);
        parzenButton.addActionListener(this);
        modeGroup.add(parzenButton);
        if (!scalarImage) {
        	parzenButton.setEnabled(false);
        }
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(parzenButton, gbc6);

        sigmaLabel = new JLabel("Parzen window sigma ");
        sigmaLabel.setForeground(Color.black);
        sigmaLabel.setFont(serif12);
        sigmaLabel.setEnabled(false);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(sigmaLabel, gbc6);

        sigmanbinsText = new JTextField(10);
        sigmanbinsText.setText(String.valueOf(1.0));
        sigmanbinsText.setFont(serif12);
        sigmanbinsText.setEnabled(false);
        gbc6.gridx = 1;
        paramPanel.add(sigmanbinsText, gbc6);
        
        aaButton = new JRadioButton("Anti-aliased sampling", false);
        aaButton.setFont(serif12);
        aaButton.setForeground(Color.black);
        aaButton.addActionListener(this);
        modeGroup.add(aaButton);
        if (!scalarImage) {
        	aaButton.setEnabled(false);
        }
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(aaButton, gbc6);
        
        nbinsLabel = new JLabel("Histogram bins ");
        nbinsLabel.setForeground(Color.black);
        nbinsLabel.setFont(serif12);
        nbinsLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(nbinsLabel, gbc6);

        nbinsText = new JTextField(10);
        nbinsText.setText(String.valueOf(16));
        nbinsText.setFont(serif12);
        nbinsText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(nbinsText, gbc6);
        
        interScaleSaliencyLabel = new JLabel("Inter-scale saliency threshold (0.0-2.0) ");
        interScaleSaliencyLabel.setForeground(Color.black);
        interScaleSaliencyLabel.setFont(serif12);
        interScaleSaliencyLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(interScaleSaliencyLabel, gbc6);
        
        interScaleSaliencyText = new JTextField(3);
        interScaleSaliencyText.setText("0.5");
        interScaleSaliencyText.setFont(serif12);
        interScaleSaliencyText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(interScaleSaliencyText, gbc6);
        
        saliencyLabel = new JLabel("Fraction of saliency maximum value used as threshold ");
        saliencyLabel.setForeground(Color.black);
        saliencyLabel.setFont(serif12);
        saliencyLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(saliencyLabel, gbc6);
        
        saliencyText = new JTextField(3);
        saliencyText.setText("0.5");
        saliencyText.setFont(serif12);
        saliencyText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(saliencyText, gbc6);

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
    	
    	if (!testParameter(minimumText.getText(), 1.0, 200.0)) {
            minimumText.requestFocus();
            minimumText.selectAll();

            return false;
        } else {
            minimumRadius = Integer.valueOf(minimumText.getText()).intValue();
        }


        if (!testParameter(maximumText.getText(), minimumRadius+1, 200.0)) {
            maximumText.requestFocus();
            maximumText.selectAll();

            return false;
        } else {
            maximumRadius = Integer.valueOf(maximumText.getText()).intValue();
        }
        
        if (normalButton.isSelected()) {
            mode = NORMAL_MODE;	
        }
        else if (parzenButton.isSelected()) {
        	mode = PARZEN_WINDOW_MODE;
        }
        else {
        	mode = ANTI_ALIASED_SAMPLING_MODE;
        }

        if (mode == PARZEN_WINDOW_MODE) {
	        if (!testParameter(sigmanbinsText.getText(), 0.1, 20.0)) {
	            sigmanbinsText.requestFocus();
	            sigmanbinsText.selectAll();
	
	            return false;
	        } else {
	            sigma = Double.valueOf(sigmanbinsText.getText()).doubleValue();
	        }
        } // if (mode == PARZEN_WINDOW_MODE)
        
        
        if (mode != PARZEN_WINDOW_MODE) {
	        if (!testParameter(nbinsText.getText(), 2, 1024)) {
	            nbinsText.requestFocus();
	            nbinsText.selectAll();
	
	            return false;
	        } else {
	            nbins = Integer.valueOf(nbinsText.getText()).intValue();
	        }
        } // if (mode != PARZEN_WINDOW_MODE)
        else {
        	nbins = 256;
        }
        
        if (!testParameter(interScaleSaliencyText.getText(), 0.0, 2.0)) {
            interScaleSaliencyText.requestFocus();
            interScaleSaliencyText.selectAll();

            return false;
        } else {
            wt = Double.valueOf(interScaleSaliencyText.getText()).doubleValue();
        }
        
        if (!testParameter(saliencyText.getText(), 0.0, 5.0)) {
            saliencyText.requestFocus();
            saliencyText.selectAll();

            return false;
        } else {
            yt = Double.valueOf(saliencyText.getText()).doubleValue();
        }

        return true;
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        minimumRadius = scriptParameters.getParams().getInt("minimum");
        maximumRadius = scriptParameters.getParams().getInt("maximum");
        mode = scriptParameters.getParams().getInt("mod");
        nbins = scriptParameters.getParams().getInt("nb"); 
        sigma = scriptParameters.getParams().getDouble("sig");
        wt = scriptParameters.getParams().getDouble("w_t");
        yt = scriptParameters.getParams().getDouble("y_t");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("minimum", minimumRadius));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maximum", maximumRadius));
        scriptParameters.getParams().put(ParameterFactory.newParameter("mod", mode));
        scriptParameters.getParams().put(ParameterFactory.newParameter("nb", nbins));
        scriptParameters.getParams().put(ParameterFactory.newParameter("sig", sigma));
        scriptParameters.getParams().put(ParameterFactory.newParameter("w_t", wt));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y_t", yt));
    }
    
    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt("minimum", 3));
            table.put(new ParameterInt("maximm", 33));
            table.put(new ParameterInt("mod", 1));
            table.put(new ParameterInt("nb", 16));
            table.put(new ParameterDouble("sig", 1.0));
            table.put(new ParameterDouble("w_t", 0.5));
            table.put(new ParameterDouble("y_t", 0.5));
            } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }
    
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Scale Saliency");
            }

            public String getDescription() {
                return new String("Applies Scale Saliency.");
            }

            public String getDescriptionLong() {
                return new String("Applies Scales Saliency to circle salient features.");
            }

            public String getShortLabel() {
                return new String("ScaleSaliency");
            }

            public String getLabel() {
                return new String("Scale Saliency");
            }

            public String getName() {
                return new String("Scale Saliency");
            }
        };
    }
}

