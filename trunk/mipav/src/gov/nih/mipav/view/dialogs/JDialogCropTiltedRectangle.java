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
    
    private int x1Point;
    private int x2Point;
    private int x3Point;
    private int x4Point;
    
    private JTextField x1PointInput;
    private JTextField x2PointInput;
    private JTextField x3PointInput;
    private JTextField x4PointInput;
    
    private int y1Point;
    private int y2Point;
    private int y3Point;
    private int y4Point;
    
    private JTextField y1PointInput;
    private JTextField y2PointInput;
    private JTextField y3PointInput;
    private JTextField y4PointInput;
    
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
     * @param x1Point
     */
    public void setX1Point(int x1Point) {
        this.x1Point = x1Point;
    }
    
    /**
     * 
     * @param y1Point
     */
    public void setY1Point(int y1Point) {
        this.y1Point = y1Point;
    }
    
    /**
     * 
     * @param x2Point
     */
    public void setX2Point(int x2Point) {
        this.x2Point = x2Point;
    }
    
    /**
     * 
     * @param y2Point
     */
    public void setY2Point(int y2Point) {
        this.y2Point = y2Point;
    }
    
    /**
     * 
     * @param x13Point
     */
    public void setX3Point(int x3Point) {
        this.x3Point = x3Point;
    }
    
    /**
     * 
     * @param y3Point
     */
    public void setY3Point(int y3Point) {
        this.y3Point = y3Point;
    }
    
    /**
     * 
     * @param x4Point
     */
    public void setX4Point(int x4Point) {
        this.x4Point = x4Point;
    }
    
    /**
     * 
     * @param y4Point
     */
    public void setY4Point(int y4Point) {
        this.y4Point = y4Point;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
    	try {
                cropAlgo = new AlgorithmCropTilted(image, x1Point, y1Point, x2Point, y2Point,
                		x3Point, y3Point, x4Point, y4Point);

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
            t = x1PointInput.getText();
            if (t.equals("")) {
                return x1PointInput;
            }
            
            t = y1PointInput.getText();
            if (t.equals("")) {
                return y1PointInput;
            }
            
            t = x2PointInput.getText();
            if (t.equals("")) {
                return x2PointInput;
            }
            
            t = y2PointInput.getText();
            if (t.equals("")) {
                return y2PointInput;
            }
            
            t = x3PointInput.getText();
            if (t.equals("")) {
                return x3PointInput;
            }
            
            t = y3PointInput.getText();
            if (t.equals("")) {
                return y3PointInput;
            }
            
            t = x4PointInput.getText();
            if (t.equals("")) {
                return x4PointInput;
            }
            
            t = y4PointInput.getText();
            if (t.equals("")) {
                return y4PointInput;
            }
            
            return x1PointInput;
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogCropTiltedRectangle reports: Unknown Error");

            return x1PointInput; // gotta have some thing returned
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
        setX1Point(scriptParameters.getParams().getInt("x1_point"));
        setY1Point(scriptParameters.getParams().getInt("y1_point"));
        setX1Point(scriptParameters.getParams().getInt("x2_point"));
        setY1Point(scriptParameters.getParams().getInt("y2_point"));
        setX1Point(scriptParameters.getParams().getInt("x3_point"));
        setY1Point(scriptParameters.getParams().getInt("y3_point"));
        setX1Point(scriptParameters.getParams().getInt("x4_point"));
        setY1Point(scriptParameters.getParams().getInt("y4_point"));
        
        if (x1Point < 0) {
            throw new ParameterException("x1_point", "Cannot have x1Point < 0");
        }
        
        if (x1Point >= image.getExtents()[0]) {
            throw new ParameterException("x1_point", "Cannot have x1Point >= image.getExtents()[0]");
        }
        
        if (y1Point < 0) {
            throw new ParameterException("y1_point", "Cannot have y1Point < 0");
        }
        
        if (y1Point >= image.getExtents()[1]) {
            throw new ParameterException("y1_point", "Cannot have y1Point >= image.getExtents()[1]");
        }
        
        if (x2Point < 0) {
            throw new ParameterException("x2_point", "Cannot have x2Point < 0");
        }
        
        if (x2Point >= image.getExtents()[0]) {
            throw new ParameterException("x2_point", "Cannot have x2Point >= image.getExtents()[0]");
        }
        
        if (y2Point < 0) {
            throw new ParameterException("y2_point", "Cannot have y2Point < 0");
        }
        
        if (y2Point >= image.getExtents()[1]) {
            throw new ParameterException("y2_point", "Cannot have y2Point >= image.getExtents()[1]");
        }
        
        if (x3Point < 0) {
            throw new ParameterException("x3_point", "Cannot have x3Point < 0");
        }
        
        if (x3Point >= image.getExtents()[0]) {
            throw new ParameterException("x3_point", "Cannot have x3Point >= image.getExtents()[0]");
        }
        
        if (y3Point < 0) {
            throw new ParameterException("y3_point", "Cannot have y3Point < 0");
        }
        
        if (y3Point >= image.getExtents()[1]) {
            throw new ParameterException("y3_point", "Cannot have y3Point >= image.getExtents()[1]");
        }
        
        if (x4Point < 0) {
            throw new ParameterException("x4_point", "Cannot have x4Point < 0");
        }
        
        if (x4Point >= image.getExtents()[0]) {
            throw new ParameterException("x4_point", "Cannot have x4Point >= image.getExtents()[0]");
        }
        
        if (y4Point < 0) {
            throw new ParameterException("y4_point", "Cannot have y4Point < 0");
        }
        
        if (y4Point >= image.getExtents()[1]) {
            throw new ParameterException("y4_point", "Cannot have y4Point >= image.getExtents()[1]");
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("x1_point", x1Point));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y1_point", y1Point));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x2_point", x2Point));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y2_point", y2Point));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x3_point", x3Point));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y3_point", y3Point));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x4_point", x4Point));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y4_point", y4Point));
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
        
        JLabel x1PointLabel = new JLabel("Upper left X point:");
        x1PointLabel.setFont(serif12);
        x1PointLabel.setForeground(Color.black);
        x1PointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x1PointLabel, gbc);
        optionPanel.add(x1PointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        x1PointInput = new JTextField("0", 4);
        x1PointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(x1PointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x1PointInput, gbc);
        optionPanel.add(x1PointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y1PointLabel = new JLabel("Upper left Y point:");
        y1PointLabel.setFont(serif12);
        y1PointLabel.setForeground(Color.black);
        y1PointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y1PointLabel, gbc);
        optionPanel.add(y1PointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        y1PointInput = new JTextField("0", 4);
        y1PointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(y1PointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y1PointInput, gbc);
        optionPanel.add(y1PointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x2PointLabel = new JLabel("Upper right X point:");
        x2PointLabel.setFont(serif12);
        x2PointLabel.setForeground(Color.black);
        x2PointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x2PointLabel, gbc);
        optionPanel.add(x2PointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        x2PointInput = new JTextField("0", 4);
        x2PointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(x2PointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x2PointInput, gbc);
        optionPanel.add(x2PointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y2PointLabel = new JLabel("Upper right Y point:");
        y2PointLabel.setFont(serif12);
        y2PointLabel.setForeground(Color.black);
        y2PointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y2PointLabel, gbc);
        optionPanel.add(y2PointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        y2PointInput = new JTextField("0", 4);
        y2PointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(y2PointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y2PointInput, gbc);
        optionPanel.add(y2PointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x3PointLabel = new JLabel("Lower right X point:");
        x3PointLabel.setFont(serif12);
        x3PointLabel.setForeground(Color.black);
        x3PointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x3PointLabel, gbc);
        optionPanel.add(x3PointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        x3PointInput = new JTextField("0", 4);
        x3PointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(x3PointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x3PointInput, gbc);
        optionPanel.add(x3PointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y3PointLabel = new JLabel("Lower right Y point:");
        y3PointLabel.setFont(serif12);
        y3PointLabel.setForeground(Color.black);
        y3PointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y3PointLabel, gbc);
        optionPanel.add(y3PointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        y3PointInput = new JTextField("0", 4);
        y3PointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(y3PointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y3PointInput, gbc);
        optionPanel.add(y3PointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x4PointLabel = new JLabel("Lower left X point:");
        x4PointLabel.setFont(serif12);
        x4PointLabel.setForeground(Color.black);
        x4PointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x4PointLabel, gbc);
        optionPanel.add(x4PointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        x4PointInput = new JTextField("0", 4);
        x4PointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(x4PointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x4PointInput, gbc);
        optionPanel.add(x4PointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y4PointLabel = new JLabel("Lower left Y point:");
        y4PointLabel.setFont(serif12);
        y4PointLabel.setForeground(Color.black);
        y4PointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y4PointLabel, gbc);
        optionPanel.add(y4PointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        y4PointInput = new JTextField("0", 4);
        y4PointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(y4PointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y4PointInput, gbc);
        optionPanel.add(y4PointInput);

     
        
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
            x1Point = Integer.parseInt(x1PointInput.getText());
            y1Point = Integer.parseInt(y1PointInput.getText());
            x2Point = Integer.parseInt(x2PointInput.getText());
            y2Point = Integer.parseInt(y2PointInput.getText());
            x3Point = Integer.parseInt(x3PointInput.getText());
            y3Point = Integer.parseInt(y3PointInput.getText());
            x4Point = Integer.parseInt(x4PointInput.getText());
            y4Point = Integer.parseInt(y4PointInput.getText());
        } catch (NumberFormatException nfe) {

            // an empty text-field.  decide which one is empty, then alert the user to correct
            JTextField t = determineNull();
            MipavUtil.displayError("Improper number!");
            t.requestFocus();
            t.selectAll();

            return false;
        }
        
        if (x1Point < 0) {
            MipavUtil.displayError("Cannot have X1 point < 0");
            x1PointInput.requestFocus();
            x1PointInput.selectAll();

            return false;
        }

        if (x1Point >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X1 point >= image.getExtents()[0]");
            x1PointInput.requestFocus();
            x1PointInput.selectAll();

            return false;
        }
        
        if (y1Point < 0) {
            MipavUtil.displayError("Cannot have Y1 point < 0");
            y1PointInput.requestFocus();
            y1PointInput.selectAll();

            return false;
        }

        if (y1Point >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y1 point >= image.getExtents()[1]");
            y1PointInput.requestFocus();
            y1PointInput.selectAll();

            return false;
        }
        
        if (x2Point < 0) {
            MipavUtil.displayError("Cannot have X2 point < 0");
            x2PointInput.requestFocus();
            x2PointInput.selectAll();

            return false;
        }

        if (x2Point >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X2 point >= image.getExtents()[0]");
            x2PointInput.requestFocus();
            x2PointInput.selectAll();

            return false;
        }
        
        if (y2Point < 0) {
            MipavUtil.displayError("Cannot have Y2 point < 0");
            y2PointInput.requestFocus();
            y2PointInput.selectAll();

            return false;
        }

        if (y2Point >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y2 point >= image.getExtents()[1]");
            y2PointInput.requestFocus();
            y2PointInput.selectAll();

            return false;
        }
        
        if (x3Point < 0) {
            MipavUtil.displayError("Cannot have X3 point < 0");
            x3PointInput.requestFocus();
            x3PointInput.selectAll();

            return false;
        }

        if (x3Point >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X3 point >= image.getExtents()[0]");
            x3PointInput.requestFocus();
            x3PointInput.selectAll();

            return false;
        }
        
        if (y3Point < 0) {
            MipavUtil.displayError("Cannot have Y3 point < 0");
            y3PointInput.requestFocus();
            y3PointInput.selectAll();

            return false;
        }

        if (y3Point >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y3 point >= image.getExtents()[1]");
            y3PointInput.requestFocus();
            y3PointInput.selectAll();

            return false;
        }
        
        if (x4Point < 0) {
            MipavUtil.displayError("Cannot have X4 point < 0");
            x4PointInput.requestFocus();
            x4PointInput.selectAll();

            return false;
        }

        if (x4Point >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X4 point >= image.getExtents()[0]");
            x4PointInput.requestFocus();
            x4PointInput.selectAll();

            return false;
        }
        
        if (y4Point < 0) {
            MipavUtil.displayError("Cannot have Y4 point < 0");
            y4PointInput.requestFocus();
            y4PointInput.selectAll();

            return false;
        }

        if (y4Point >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y4 point >= image.getExtents()[1]");
            y4PointInput.requestFocus();
            y4PointInput.selectAll();

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
