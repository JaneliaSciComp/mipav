package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogLLE extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private int embeddedDimensions = 2; // Number of embedded dimensions
    

    /** DOCUMENT ME! */
    private JTextField textEmbeddedDimensions;
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private AlgorithmLLE lleAlgo;

    /** DOCUMENT ME! */
    private ModelImage destImage = null;
    
    private int numberOfNeighbors = 4;
    
    
    private JTextField textNeighbors;
    
    private double tol = 1.0E-4; // regualrizer
    
    private JTextField textTol;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogLLE() { }

    /**
     * Creates new dialog for entering parameters for locally linear embedding.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogLLE(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
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

        if (algorithm instanceof AlgorithmLLE) {
            image.clearMask();

            if (lleAlgo.isCompleted()) {

                if (destImage != null) {
                    try {
                        new ViewJFrameImage(destImage, null, new Dimension(610, 220));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: Unable to open LLE image frame");
                    }
                } // if (destImage != null)
                        
            }
        }
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        lleAlgo.finalize();
        lleAlgo = null;
        dispose();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }

    /**
     * Accessor that sets the number of embedded dimensions.
     *
     * @param  embeddedDimensions  DOCUMENT ME!
     */
    public void setEmbeddedDimensions(int embeddedDimensions) {
        this.embeddedDimensions = embeddedDimensions;
    }
    
    public void setNumberOfNieghbors(int numberOfNeighbors) {
        this.numberOfNeighbors = numberOfNeighbors;
    }
    
    public void setTol(double tol) {
        this.tol = tol;
    }

    /**
     * Once all the necessary variables are set, call the locally linear embedding algorithm
     */
    protected void callAlgorithm() {

            try {
                // Make algorithm
                lleAlgo = new AlgorithmLLE(null, image, embeddedDimensions, numberOfNeighbors, tol);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                lleAlgo.addListener(this);

                createProgressBar(image.getImageName(), lleAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (lleAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    lleAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog LLE: unable to allocate enough memory");
                return;
            }
           
       
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        embeddedDimensions = scriptParameters.getParams().getInt("embedded_dimensions");
        numberOfNeighbors = scriptParameters.getParams().getInt("number_of_neighbors");
        tol = scriptParameters.getParams().getDouble("regularizer");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("embedded_dimensions", embeddedDimensions));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_neighbors", numberOfNeighbors));
        scriptParameters.getParams().put(ParameterFactory.newParameter("regularizer", tol));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Locally Linear Embedding");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel dimensionLabel = new JLabel("Number of embedded dimensions (usually 2)");
        dimensionLabel.setForeground(Color.black);
        dimensionLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(dimensionLabel, gbc);

        textEmbeddedDimensions = new JTextField(10);
        textEmbeddedDimensions.setFont(serif12);
        textEmbeddedDimensions.setText("2");
        gbc.gridx = 1;
        paramPanel.add(textEmbeddedDimensions, gbc);
        
        JLabel neighborsLabel = new JLabel("Number of neighbors:");
        neighborsLabel.setForeground(Color.black);
        neighborsLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(neighborsLabel, gbc);
        
        textNeighbors = new JTextField(10);
        textNeighbors.setFont(serif12);
        textNeighbors.setText("4");
        gbc.gridx = 1;
        paramPanel.add(textNeighbors, gbc);
        
        JLabel tolLabel = new JLabel("Regularizer:");
        tolLabel.setForeground(Color.black);
        tolLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(tolLabel, gbc);
        
        textTol = new JTextField(10);
        textTol.setFont(serif12);
        textTol.setText("1.0E-4");
        gbc.gridx = 1;
        paramPanel.add(textTol, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        mainPanel.add(paramPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        String tmpStr;

        tmpStr = textEmbeddedDimensions.getText();
        
        if (testParameter(tmpStr, 1, 100)) {
            embeddedDimensions = Integer.valueOf(tmpStr).intValue();
        }
        else {
            textEmbeddedDimensions.requestFocus();
            textEmbeddedDimensions.selectAll();
            
            return false;
        }
        
        
        tmpStr = textNeighbors.getText();
        if (testParameter(tmpStr, 1, 100)) {
            numberOfNeighbors = Integer.valueOf(tmpStr).intValue();
        } else {
            textNeighbors.requestFocus();
            textNeighbors.selectAll();
            
            return false;
        }
            
        tmpStr = textTol.getText();
        if (testParameter(tmpStr, Double.MIN_VALUE, 1.0)) {
            tol = Double.valueOf(tmpStr).doubleValue();
        } else {
            textTol.requestFocus();
            textTol.selectAll();
            
            return false;
        }

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
                return new String("Algorithms(locally linear embedding)");
            }

            public String getDescription() {
                return new String("Locally linear embedding.");
            }

            public String getDescriptionLong() {
                return new String("Locally linear embedding.");
            }

            public String getShortLabel() {
                return new String("LLE");
            }

            public String getLabel() {
                return new String("Locally linear embedding");
            }

            public String getName() {
                return new String("Locally linear embedding");
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
            //table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterInt("embedded_dimensions", 2));
            table.put(new ParameterInt("number_of_neighbors", 4));
            table.put(new ParameterDouble("regularizer", tol));
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

       /* try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }
*/
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
