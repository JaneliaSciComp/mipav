package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to call AlgorithmBoxCount.
 */
public class JDialogBoxCount extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface  {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmBoxCount boxCountAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image
    
    private boolean entireImage = true;
    
    private JRadioButton radioEntireImage;
    
    private JRadioButton radioVOIRegion;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogBoxCount() { }

    /**
     * Constructs new transform dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogBoxCount(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
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
            //MipavUtil.showWebHelp("");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmBoxCount) {
            image.clearMask();

            
        } // if ( algorithm instanceof AlgorithmBoxCOunt)

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(boxCountAlgo.isCompleted());

        boxCountAlgo.finalize();
        boxCountAlgo = null;
        dispose();
    }

    

    /**
     * Calls the algorithm with the set variables.
     */
    protected void callAlgorithm() {

        try {

            
            boxCountAlgo     = new AlgorithmBoxCount(image, entireImage);

            // This is very important. Adding this object as a listener allows
            // the algorithm to notify this object when it has completed of failed.
            // See algorithm performed event. This is made possible by implementing
            boxCountAlgo.addListener(this);

            createProgressBar(image.getImageName(), boxCountAlgo);

            // Start the thread as a low priority because we wish to still have
            // user interface work fast

            if (isRunInSeparateThread()) {

                if (boxCountAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                boxCountAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog AlgorithmBoxCount: unable to allocate enough memory");

            return;
        }
    }

   
    protected void doPostAlgorithmActions() {

        
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        entireImage = scriptParameters.doProcessWholeImage();

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeProcessWholeImage(entireImage);
    }

    /**
     * Initializes the dialog box to a certain size and adds the components.
     */
    private void init() {
        setTitle("Box Counting");
        JPanel outputPanel = new JPanel();
        // panel gets a grid layout
        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0);
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)
        outputPanel.setLayout(gbl);

        outputPanel.setForeground(Color.black);
        outputPanel.setBorder(buildTitledBorder("Process")); // set the border ... "Parameters"
        ButtonGroup regionGroup = new ButtonGroup();

        radioEntireImage = new JRadioButton("Entire image", entireImage);
        radioEntireImage.setFont(MipavUtil.font12);
        regionGroup.add(radioEntireImage);
        gbc.gridx = 0;
        gbc.gridy = 0;
        outputPanel.add(radioEntireImage, gbc);

        radioVOIRegion = new JRadioButton("VOI regions", !entireImage);
        radioVOIRegion.setFont(MipavUtil.font12);
        regionGroup.add(radioVOIRegion);
        gbc.gridy = 1;
        outputPanel.add(radioVOIRegion, gbc);

        getContentPane().add(outputPanel, BorderLayout.CENTER); // put the setupBox into the dialog

        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Sets the variables needed to run the algorithm.
     *
     * @return  Flag indicating successful set of the variables.
     */
    private boolean setVariables() {

        entireImage = radioEntireImage.isSelected();
        setVisible(false);

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms");
            }

            public String getDescription() {
                return new String("Box Counting.");
            }

            public String getDescriptionLong() {
                return new String("Calculates Fractal Dimension with Box Counting.");
            }

            public String getShortLabel() {
                return new String("Box Counting");
            }

            public String getLabel() {
                return new String("Box Counting");
            }

            public String getName() {
                return new String("Box Counting");
            }
        };
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
        	} catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();
        return table;
    }


    


    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

}
