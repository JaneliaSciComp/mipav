import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.ArrayList;

import javax.swing.*;

/**
 * @version  June 4, 2007
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 */
public class PlugInDialogT2Mapping extends JDialogScriptableBase implements AlgorithmInterface
{
    
    public enum FittingType {
        //TODO: Find compatible classes in NLEngine
        BiExp(null, "Bi-exponential"),
        BiExpFixedT2(null, "Bi-exp w/ fixed T2"),
        BiExpSweep(null, "Bi-exp w/ sweep"),
        L1(null, "L1"),
        MonoExp(null, "Mono-exponential"),
        NNLS(null, "NNLS"), 
        MultiExp(null, "Multi-exponential");
        
        private String name;
        
        FittingType(Class c, String name) {
            this.name = name;
            
;       }
        
        /* (non-Javadoc)
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return name;
        }
    }
    
    
    // Noise Estimators.

    public enum NoiseEstimation {
        /** No noise estimation. */
        NE_NONE("None"),
        /** Estimate the noise across the rows */
        NE_ACROSS_ROW("Across Row"),
        /** Estimate the noise across the columns. */
        NE_ACROSS_COLUMN("Across Column"),
        /** Estimate the noise in the image (gradientMethod). */
        NE_ACROSS_IMAGE("Across Slice");
        
        private String name;

        NoiseEstimation(String name) {
            this.name = name;
        }

        /* (non-Javadoc)
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return name;
        }
    }
    
                                                             
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    ArrayList<JTextField> txtField = new ArrayList<JTextField>();
    
    
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image
    
    /** DOCUMENT ME! */
    private AlgorithmBase genericAlgo = null;

	private JCheckBox chi2CheckBox;

	private JCheckBox myelinSliceCheckBox;

	private JTextField teRangeField;

	private JCheckBox useFlipEstimateCheckBox;

	private JComboBox noiseEstimateComboBox;

	private JComboBox fittingTypeComboBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogT2Mapping() 
    {
    	
    }

	/**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogT2Mapping(Frame theParentFrame, ModelImage im)
    {
        super(theParentFrame, false);
        image = im;
        init();
    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event)
    {
        String command = event.getActionCommand();

        if (command.equals("OK"))
        {
            //if (setVariables()) {
                callAlgorithm();
            //}
                
                
                
        } else if (command.equals("Script")) {
            callAlgorithm();
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
    public void algorithmPerformed(AlgorithmBase algorithm)
    {

        if (algorithm instanceof AlgorithmBase)
        {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((genericAlgo.isCompleted() == true) && (resultImage != null)) 
            {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                try 
                {
                    new ViewJFrameImage(resultImage);
                } 
                catch (OutOfMemoryError error) 
                {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } 
            else if (resultImage != null) {
                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            if (algorithm != null) 
            {
                algorithm.finalize();
                algorithm = null;
            }

            dispose();
        }

    } // end AlgorithmPerformed()

    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() 
    {
        try 
        {
        	double[] timeVals = new double[txtField.size()];
        	for(int i = 0; i<timeVals.length; i++)
        	{
        		timeVals[i]= Double.parseDouble(txtField.get(i).getText());
        		
        	}
        	
            String name = makeImageName(image.getImageName(), "_T2 fit");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            genericAlgo = new PlugInAlgorithmT2Mapping(resultImage, image, timeVals);
            //genericAlgo = new PlugInAlgorithmNewGeneric2(resultImage, image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            genericAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", genericAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) 
            {
                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (genericAlgo.startMethod(Thread.MIN_PRIORITY) == false)
                {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } 
            else 
            {
                genericAlgo.run();
            }
        } 
        catch (OutOfMemoryError x)
        {
            if (resultImage != null)
            {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }
            MipavUtil.displayError("Kidney segmentation: unable to allocate enough memory");
            return;
        }

    } // end callAlgorithm()

    protected void setGUIFromParams() 
    {
    // TODO Auto-generated method stub, no params yet
    }

    protected void storeParamsFromGUI() throws ParserException
    {
    // TODO Auto-generated method stub, no params yet
    }
   
    private void init() 
    {
    	GuiBuilder builder = new GuiBuilder(this);
    	
    	setForeground(Color.black);
        setTitle("T2 Mapping");

        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(MipavUtil.buildTitledBorder("Display parameters"));
        //TODO must be changed when not run from Eclipse
        chi2CheckBox = builder.buildCheckBox("Display chi^2 image", false);
        mainPanel.add(chi2CheckBox.getParent(), gbc);
        gbc.gridy++;
        
        myelinSliceCheckBox = builder.buildCheckBox("Display myelin slice image", true);
        mainPanel.add(myelinSliceCheckBox.getParent(), gbc);
        gbc.gridy++;

        getContentPane().add(mainPanel, BorderLayout.NORTH);
        
        JPanel mainPanel2 = new JPanel(new GridBagLayout());

        mainPanel2 = new JPanel();
        mainPanel2.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel2.setLayout(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        GridBagConstraints gbcParam = new GridBagConstraints();
        gbcParam.gridwidth = 1;
        gbcParam.gridheight = 1;
        gbcParam.anchor = GridBagConstraints.WEST;
        gbcParam.weightx = 1;
        gbcParam.insets = new Insets(3, 3, 3, 3);
        gbcParam.gridx = 0;
        gbcParam.gridy = 0;
        gbcParam.fill = GridBagConstraints.HORIZONTAL;
        
        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(MipavUtil.buildTitledBorder("Fitting parameters"));
        noiseEstimateComboBox = builder.buildComboBox("Noise estimation method: ", NoiseEstimation.values(), 1);
        paramPanel.add(noiseEstimateComboBox.getParent(), gbcParam);
        gbcParam.gridy++;
        fittingTypeComboBox = builder.buildComboBox("Fitting type: ", FittingType.values());
        paramPanel.add(fittingTypeComboBox.getParent(), gbcParam);
        gbcParam.gridy++;
        useFlipEstimateCheckBox = builder.buildCheckBox("Use estimated flip angle", false);
        paramPanel.add(useFlipEstimateCheckBox.getParent(), gbcParam);
        mainPanel2.add(paramPanel, gbc2);
        
        
        JPanel tePanel = new JPanel(new GridBagLayout());
        tePanel.setBorder(MipavUtil.buildTitledBorder("Acquisition parameters"));
        gbcParam = new GridBagConstraints();
        gbcParam.gridwidth = 1;
        
        gbcParam.gridheight = 1;
        gbcParam.anchor = GridBagConstraints.WEST;
        gbcParam.weightx = 1;
        gbcParam.insets = new Insets(3, 3, 3, 3);
        gbcParam.gridx = 0;
        gbcParam.gridy = 0;
        gbcParam.fill = GridBagConstraints.HORIZONTAL;
        teRangeField = builder.buildField("Time range: ", "80 - 160");
        tePanel.add(teRangeField.getParent(), gbcParam);
        gbcParam.gridy++;
        
        int numTimeSlices = image.getExtents()[3];
        int base = 80;
        for (int i = 0; i < numTimeSlices; i++) 
        {
        	txtField.add(builder.buildIntegerField("Time "+i, base+i*20));
        	tePanel.add(txtField.get(i).getParent(), gbcParam);
        	gbcParam.gridy++;
        }
        
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        mainPanel2.add(tePanel, gbc2);
        gbc2.gridy++;
        

        mainDialogPanel.add(mainPanel2, BorderLayout.CENTER);
        mainDialogPanel.add(builder.buildOKCancelPanel(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel, BorderLayout.CENTER);

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();
        
    } // end init()   
}
