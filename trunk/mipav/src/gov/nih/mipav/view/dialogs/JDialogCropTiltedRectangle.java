package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmCropTilted;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Creates the dialog to crop pixels around the 4 selected tilted rectangle points.
 *
 * <p>User selects:</p>
 *
 * <ol>
 *   <li>Upper left x,y</li>
 *   <li>Upper right x,y</li>
 *   <li>Bottom right x,y</li>
 *   <li>Bottom left x,y</li>
 * </ol>
 *
 * <p>A new image will be created.</p>
 */
public class JDialogCropTiltedRectangle extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ----------------------------------------------------------------------------------------------

    // or if the source image is to be replaced

   

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private int x1;
    private int x2;
    private int x3;
    private int x4;
    
    private JTextField x1Input;
    private JTextField x2Input;
    private JTextField x3Input;
    private JTextField x4Input;
    
    private int y1;
    private int y2;
    private int y3;
    private int y4;
    
    private JTextField y1Input;
    private JTextField y2Input;
    private JTextField y3Input;
    private JTextField y4Input;
    
    private AlgorithmCropTilted cropAlgo;
    
   
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCropTiltedRectangle() { }

    /**
     * Constructor that makes dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCropTiltedRectangle(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        		//MipavUtil.showHelp("");
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
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        
        
        if (algorithm instanceof AlgorithmCropTilted) {
        	
        	if (cropAlgo.isCompleted()) {
        		resultImage = cropAlgo.getResultImage();
        		if (resultImage != null) {
        			 try {
                         new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                     } catch (OutOfMemoryError error) {
                         MipavUtil.displayError("Out of memory: unable to open new frame");
                     }

                     insertScriptLine();	
        		}
        	}

            
        // save the completion status for later
        
        setComplete(algorithm.isCompleted());

        cropAlgo.finalize();
        cropAlgo = null;
        dispose();
        }
    }
    
    /**
     * 
     * @param x1
     */
    public void setX1(int x1) {
        this.x1 = x1;
    }
    
    /**
     * 
     * @param y1
     */
    public void setY1(int y1) {
        this.y1 = y1;
    }
    
    /**
     * 
     * @param x2
     */
    public void setX2(int x2) {
        this.x2 = x2;
    }
    
    /**
     * 
     * @param y2
     */
    public void setY2(int y2) {
        this.y2 = y2;
    }
    
    /**
     * 
     * @param x3
     */
    public void setX3(int x3) {
        this.x3 = x3;
    }
    
    /**
     * 
     * @param y3
     */
    public void setY3(int y3) {
        this.y3 = y3;
    }
    
    /**
     * 
     * @param x4
     */
    public void setX4(int x4) {
        this.x4 = x4;
    }
    
    /**
     * 
     * @param y4
     */
    public void setY4(int y4) {
        this.y4 = y4;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
    	try {
                cropAlgo = new AlgorithmCropTilted(image, x1, y1, x2, y2,
                		x3, y3, x4, y4);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                cropAlgo.addListener(this);

                createProgressBar(image.getImageName(), cropAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (cropAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    cropAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog CropTitled: unable to allocate enough memory");

                return;
            }
       
    }

    /**
     * When one of the text inputs has been left blank, trying to convert them to ints results in throwing a null
     * pointer exception. This method determines which one of the JTextFields threw the null pointer Exception.
     *
     * @return  The text field that returned null.
     */
    protected JTextField determineNull() {
        String t;

        try {
            t = x1Input.getText();
            if (t.equals("")) {
                return x1Input;
            }
            
            t = y1Input.getText();
            if (t.equals("")) {
                return y1Input;
            }
            
            t = x2Input.getText();
            if (t.equals("")) {
                return x2Input;
            }
            
            t = y2Input.getText();
            if (t.equals("")) {
                return y2Input;
            }
            
            t = x3Input.getText();
            if (t.equals("")) {
                return x3Input;
            }
            
            t = y3Input.getText();
            if (t.equals("")) {
                return y3Input;
            }
            
            t = x4Input.getText();
            if (t.equals("")) {
                return x4Input;
            }
            
            t = y4Input.getText();
            if (t.equals("")) {
                return y4Input;
            }
            
            return x1Input;
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogCropTiltedRectangle reports: Unknown Error");

            return x1Input; // gotta have some thing returned
        }
    }
    
    /**
     * Accessor that returns the image after adding image margins.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        setX1(scriptParameters.getParams().getInt("x1_point"));
        setY1(scriptParameters.getParams().getInt("y1_point"));
        setX2(scriptParameters.getParams().getInt("x2_point"));
        setY2(scriptParameters.getParams().getInt("y2_point"));
        setX3(scriptParameters.getParams().getInt("x3_point"));
        setY3(scriptParameters.getParams().getInt("y3_point"));
        setX4(scriptParameters.getParams().getInt("x4_point"));
        setY4(scriptParameters.getParams().getInt("y4_point"));
        
        if (x1 < 0) {
            throw new ParameterException("x1_point", "Cannot have x1 < 0");
        }
        
        if (x1 >= image.getExtents()[0]) {
            throw new ParameterException("x1_point", "Cannot have x1 >= image.getExtents()[0]");
        }
        
        if (y1 < 0) {
            throw new ParameterException("y1_point", "Cannot have y1 < 0");
        }
        
        if (y1 >= image.getExtents()[1]) {
            throw new ParameterException("y1_point", "Cannot have y1 >= image.getExtents()[1]");
        }
        
        if (x2 < 0) {
            throw new ParameterException("x2_point", "Cannot have x2 < 0");
        }
        
        if (x2 >= image.getExtents()[0]) {
            throw new ParameterException("x2_point", "Cannot have x2 >= image.getExtents()[0]");
        }
        
        if (y2 < 0) {
            throw new ParameterException("y2_point", "Cannot have y2 < 0");
        }
        
        if (y2 >= image.getExtents()[1]) {
            throw new ParameterException("y2_point", "Cannot have y2 >= image.getExtents()[1]");
        }
        
        if (x3 < 0) {
            throw new ParameterException("x3_point", "Cannot have x3 < 0");
        }
        
        if (x3 >= image.getExtents()[0]) {
            throw new ParameterException("x3_point", "Cannot have x3 >= image.getExtents()[0]");
        }
        
        if (y3 < 0) {
            throw new ParameterException("y3_point", "Cannot have y3 < 0");
        }
        
        if (y3 >= image.getExtents()[1]) {
            throw new ParameterException("y3_point", "Cannot have y3 >= image.getExtents()[1]");
        }
        
        if (x4 < 0) {
            throw new ParameterException("x4_point", "Cannot have x4 < 0");
        }
        
        if (x4 >= image.getExtents()[0]) {
            throw new ParameterException("x4_point", "Cannot have x4 >= image.getExtents()[0]");
        }
        
        if (y4 < 0) {
            throw new ParameterException("y4_point", "Cannot have y4 < 0");
        }
        
        if (y4 >= image.getExtents()[1]) {
            throw new ParameterException("y4_point", "Cannot have y4 >= image.getExtents()[1]");
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("x1_point", x1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y1_point", y1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x2_point", x2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y2_point", y2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x3_point", x3));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y3_point", y3));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x4_point", x4));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y4_point", y4));
    }

    /**
     * Initializes the GUI components and places them in the dialog.
     */
    private void init() {
        setTitle("Crop Pixels Around Tilted Rectangle");
        setSize(350, 230);
        setForeground(Color.black);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        JPanel optionPanel = new JPanel();

        // make border
        optionPanel.setBorder(buildTitledBorder("Pixels Retained Around Tilted Rectangle"));
        contentBox.add(optionPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        optionPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        // make content, place into layout
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x1Label = new JLabel("Upper left X point:");
        x1Label.setFont(serif12);
        x1Label.setForeground(Color.black);
        x1Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x1Label, gbc);
        optionPanel.add(x1Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        x1Input = new JTextField("0", 4);
        x1Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(x1Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x1Input, gbc);
        optionPanel.add(x1Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y1Label = new JLabel("Upper left Y point:");
        y1Label.setFont(serif12);
        y1Label.setForeground(Color.black);
        y1Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y1Label, gbc);
        optionPanel.add(y1Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        y1Input = new JTextField("0", 4);
        y1Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(y1Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y1Input, gbc);
        optionPanel.add(y1Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x2Label = new JLabel("Upper right X point:");
        x2Label.setFont(serif12);
        x2Label.setForeground(Color.black);
        x2Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x2Label, gbc);
        optionPanel.add(x2Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        x2Input = new JTextField("0", 4);
        x2Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(x2Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x2Input, gbc);
        optionPanel.add(x2Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y2Label = new JLabel("Upper right Y point:");
        y2Label.setFont(serif12);
        y2Label.setForeground(Color.black);
        y2Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y2Label, gbc);
        optionPanel.add(y2Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        y2Input = new JTextField("0", 4);
        y2Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(y2Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y2Input, gbc);
        optionPanel.add(y2Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x3Label = new JLabel("Lower right X point:");
        x3Label.setFont(serif12);
        x3Label.setForeground(Color.black);
        x3Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x3Label, gbc);
        optionPanel.add(x3Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        x3Input = new JTextField("0", 4);
        x3Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(x3Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x3Input, gbc);
        optionPanel.add(x3Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y3Label = new JLabel("Lower right Y point:");
        y3Label.setFont(serif12);
        y3Label.setForeground(Color.black);
        y3Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y3Label, gbc);
        optionPanel.add(y3Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        y3Input = new JTextField("0", 4);
        y3Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(y3Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y3Input, gbc);
        optionPanel.add(y3Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x4Label = new JLabel("Lower left X point:");
        x4Label.setFont(serif12);
        x4Label.setForeground(Color.black);
        x4Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x4Label, gbc);
        optionPanel.add(x4Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        x4Input = new JTextField("0", 4);
        x4Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(x4Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x4Input, gbc);
        optionPanel.add(x4Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y4Label = new JLabel("Lower left Y point:");
        y4Label.setFont(serif12);
        y4Label.setForeground(Color.black);
        y4Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y4Label, gbc);
        optionPanel.add(y4Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        y4Input = new JTextField("0", 4);
        y4Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(y4Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y4Input, gbc);
        optionPanel.add(y4Input);

     
        
        contentBox.add(buildButtons());

        mainDialogPanel.add(contentBox);
        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        try {
            x1 = Integer.parseInt(x1Input.getText());
            y1 = Integer.parseInt(y1Input.getText());
            x2 = Integer.parseInt(x2Input.getText());
            y2 = Integer.parseInt(y2Input.getText());
            x3 = Integer.parseInt(x3Input.getText());
            y3 = Integer.parseInt(y3Input.getText());
            x4 = Integer.parseInt(x4Input.getText());
            y4 = Integer.parseInt(y4Input.getText());
        } catch (NumberFormatException nfe) {

            // an empty text-field.  decide which one is empty, then alert the user to correct
            JTextField t = determineNull();
            MipavUtil.displayError("Improper number!");
            t.requestFocus();
            t.selectAll();

            return false;
        }
        
        if (x1 < 0) {
            MipavUtil.displayError("Cannot have X1 point < 0");
            x1Input.requestFocus();
            x1Input.selectAll();

            return false;
        }

        if (x1 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X1 point >= image.getExtents()[0]");
            x1Input.requestFocus();
            x1Input.selectAll();

            return false;
        }
        
        if (y1 < 0) {
            MipavUtil.displayError("Cannot have Y1 point < 0");
            y1Input.requestFocus();
            y1Input.selectAll();

            return false;
        }

        if (y1 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y1 point >= image.getExtents()[1]");
            y1Input.requestFocus();
            y1Input.selectAll();

            return false;
        }
        
        if (x2 < 0) {
            MipavUtil.displayError("Cannot have X2 point < 0");
            x2Input.requestFocus();
            x2Input.selectAll();

            return false;
        }

        if (x2 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X2 point >= image.getExtents()[0]");
            x2Input.requestFocus();
            x2Input.selectAll();

            return false;
        }
        
        if (y2 < 0) {
            MipavUtil.displayError("Cannot have Y2 point < 0");
            y2Input.requestFocus();
            y2Input.selectAll();

            return false;
        }

        if (y2 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y2 point >= image.getExtents()[1]");
            y2Input.requestFocus();
            y2Input.selectAll();

            return false;
        }
        
        if (x3 < 0) {
            MipavUtil.displayError("Cannot have X3 point < 0");
            x3Input.requestFocus();
            x3Input.selectAll();

            return false;
        }

        if (x3 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X3 point >= image.getExtents()[0]");
            x3Input.requestFocus();
            x3Input.selectAll();

            return false;
        }
        
        if (y3 < 0) {
            MipavUtil.displayError("Cannot have Y3 point < 0");
            y3Input.requestFocus();
            y3Input.selectAll();

            return false;
        }

        if (y3 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y3 point >= image.getExtents()[1]");
            y3Input.requestFocus();
            y3Input.selectAll();

            return false;
        }
        
        if (x4 < 0) {
            MipavUtil.displayError("Cannot have X4 point < 0");
            x4Input.requestFocus();
            x4Input.selectAll();

            return false;
        }

        if (x4 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X4 point >= image.getExtents()[0]");
            x4Input.requestFocus();
            x4Input.selectAll();

            return false;
        }
        
        if (y4 < 0) {
            MipavUtil.displayError("Cannot have Y4 point < 0");
            y4Input.requestFocus();
            y4Input.selectAll();

            return false;
        }

        if (y4 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y4 point >= image.getExtents()[1]");
            y4Input.requestFocus();
            y4Input.selectAll();

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
                return new String("Utilities.Crop");
            }

            public String getDescription() {
                return new String("Crops image around a tilted rectangle.");
            }

            public String getDescriptionLong() {
                return new String("Crops image around a tilted rectangle.");
            }

            public String getShortLabel() {
                return new String("CropAroundTiltedRectangle");
            }

            public String getLabel() {
                return new String("Crop around Tilted Rectangle");
            }

            public String getName() {
                return new String("Crop around Tilted Rectangle");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterInt("x1_point", 0));
            table.put(new ParameterInt("y1_point", 0));
            table.put(new ParameterInt("x2_point", 0));
            table.put(new ParameterInt("y2_point", 0));
            table.put(new ParameterInt("x3_point", 0));
            table.put(new ParameterInt("y3_point", 0));
            table.put(new ParameterInt("x4_point", 0));
            table.put(new ParameterInt("y4_point", 0));
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

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
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
