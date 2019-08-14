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
 * Creates the dialog to crop pixels around the 8 selected tilted cuboid points.
 *
 * <p>User selects:</p>
 *
 * <ol>
 *   <li>Front upper left x,y</li>
 *   <li>Front upper right x,y</li>
 *   <li>Front bottom right x,y</li>
 *   <li>Front bottom left x,y</li>
 *   <li>Back upper left x,y</li>
 *   <li>Back upper right x,y</li>
 *   <li>Back bottom right x,y</li>
 *   <li>Back bottom left x,y</li>
 * </ol>
 *
 * <p>A new image will be created.</p>
 */
public class JDialogCropTiltedCuboid extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

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
    private int x5;
    private int x6;
    private int x7;
    private int x8;
    
    private JTextField x1Input;
    private JTextField x2Input;
    private JTextField x3Input;
    private JTextField x4Input;
    private JTextField x5Input;
    private JTextField x6Input;
    private JTextField x7Input;
    private JTextField x8Input;
    
    private int y1;
    private int y2;
    private int y3;
    private int y4;
    private int y5;
    private int y6;
    private int y7;
    private int y8;
    
    private JTextField y1Input;
    private JTextField y2Input;
    private JTextField y3Input;
    private JTextField y4Input;
    private JTextField y5Input;
    private JTextField y6Input;
    private JTextField y7Input;
    private JTextField y8Input;
    
    private int z1;
    private int z2;
    private int z3;
    private int z4;
    private int z5;
    private int z6;
    private int z7;
    private int z8;
    
    private JTextField z1Input;
    private JTextField z2Input;
    private JTextField z3Input;
    private JTextField z4Input;
    private JTextField z5Input;
    private JTextField z6Input;
    private JTextField z7Input;
    private JTextField z8Input;
    
    private AlgorithmCropTilted cropAlgo;
    
   
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCropTiltedCuboid() { }

    /**
     * Constructor that makes dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCropTiltedCuboid(Frame theParentFrame, ModelImage im) {
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
     * @param z1
     */
    public void setZ1(int z1) {
        this.z1 = z1;
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
     * @param z2
     */
    public void setZ2(int z2) {
        this.z2 = z2;
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
     * @param z3
     */
    public void setZ3(int z3) {
        this.z3 = z3;
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
     * 
     * @param z4
     */
    public void setZ4(int z4) {
        this.z4 = z4;
    }
    
    /**
     * 
     * @param x5
     */
    public void setX5(int x5) {
        this.x5 = x5;
    }
    
    /**
     * 
     * @param y5
     */
    public void setY5(int y5) {
        this.y5 = y5;
    }
    
    /**
     * 
     * @param z5
     */
    public void setZ5(int z5) {
        this.z5 = z5;
    }
    
    /**
     * 
     * @param x6
     */
    public void setX6(int x6) {
        this.x6 = x6;
    }
    
    /**
     * 
     * @param y6
     */
    public void setY6(int y6) {
        this.y6 = y6;
    }
    
    /**
     * 
     * @param z6
     */
    public void setZ6(int z6) {
        this.z6 = z6;
    }
    
    /**
     * 
     * @param x7
     */
    public void setX7(int x7) {
        this.x7 = x7;
    }
    
    /**
     * 
     * @param y7
     */
    public void setY7(int y7) {
        this.y7 = y7;
    }
    
    /**
     * 
     * @param z7
     */
    public void setZ7(int z7) {
        this.z7 = z7;
    }
    
    /**
     * 
     * @param x8
     */
    public void setX8(int x8) {
        this.x8 = x8;
    }
    
    /**
     * 
     * @param y8
     */
    public void setY8(int y8) {
        this.y8 = y8;
    }
    
    /**
     * 
     * @param z8
     */
    public void setZ8(int z8) {
        this.z8 = z8;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
    	try {
                cropAlgo = new AlgorithmCropTilted(image, x1, y1, z1, x2, y2, z2,
                		x3, y3, z3, x4, y4, z4, x5, y5, z5, x6, y6, z6,
                		x7, y7, z7, x8, y8, z8);

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
            
            t = z1Input.getText();
            if (t.equals("")) {
                return z1Input;
            }
            
            t = x2Input.getText();
            if (t.equals("")) {
                return x2Input;
            }
            
            t = y2Input.getText();
            if (t.equals("")) {
                return y2Input;
            }
            
            t = z2Input.getText();
            if (t.equals("")) {
                return z2Input;
            }
            
            t = x3Input.getText();
            if (t.equals("")) {
                return x3Input;
            }
            
            t = y3Input.getText();
            if (t.equals("")) {
                return y3Input;
            }
            
            t = z3Input.getText();
            if (t.equals("")) {
                return z3Input;
            }
            
            t = x4Input.getText();
            if (t.equals("")) {
                return x4Input;
            }
            
            t = y4Input.getText();
            if (t.equals("")) {
                return y4Input;
            }
            
            t = z4Input.getText();
            if (t.equals("")) {
                return z4Input;
            }
            
            t = x5Input.getText();
            if (t.equals("")) {
                return x5Input;
            }
            
            t = y5Input.getText();
            if (t.equals("")) {
                return y5Input;
            }
            
            t = z5Input.getText();
            if (t.equals("")) {
                return z5Input;
            }
            
            t = x6Input.getText();
            if (t.equals("")) {
                return x6Input;
            }
            
            t = y6Input.getText();
            if (t.equals("")) {
                return y6Input;
            }
            
            t = z6Input.getText();
            if (t.equals("")) {
                return z6Input;
            }
            
            t = x7Input.getText();
            if (t.equals("")) {
                return x7Input;
            }
            
            t = y7Input.getText();
            if (t.equals("")) {
                return y7Input;
            }
            
            t = z7Input.getText();
            if (t.equals("")) {
                return z7Input;
            }
            
            t = x8Input.getText();
            if (t.equals("")) {
                return x8Input;
            }
            
            t = y8Input.getText();
            if (t.equals("")) {
                return y8Input;
            }
            
            t = z8Input.getText();
            if (t.equals("")) {
                return z8Input;
            }
            
            return x1Input;
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogCropTiltedCuboid reports: Unknown Error");

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
        setZ1(scriptParameters.getParams().getInt("z1_point"));
        setX2(scriptParameters.getParams().getInt("x2_point"));
        setY2(scriptParameters.getParams().getInt("y2_point"));
        setZ2(scriptParameters.getParams().getInt("z2_point"));
        setX3(scriptParameters.getParams().getInt("x3_point"));
        setY3(scriptParameters.getParams().getInt("y3_point"));
        setZ3(scriptParameters.getParams().getInt("z3_point"));
        setX4(scriptParameters.getParams().getInt("x4_point"));
        setY4(scriptParameters.getParams().getInt("y4_point"));
        setZ4(scriptParameters.getParams().getInt("z4_point"));
        setX5(scriptParameters.getParams().getInt("x5_point"));
        setY5(scriptParameters.getParams().getInt("y5_point"));
        setZ5(scriptParameters.getParams().getInt("z5_point"));
        setX6(scriptParameters.getParams().getInt("x6_point"));
        setY6(scriptParameters.getParams().getInt("y6_point"));
        setZ6(scriptParameters.getParams().getInt("z6_point"));
        setX7(scriptParameters.getParams().getInt("x7_point"));
        setY7(scriptParameters.getParams().getInt("y7_point"));
        setZ7(scriptParameters.getParams().getInt("z7_point"));
        setX8(scriptParameters.getParams().getInt("x8_point"));
        setY8(scriptParameters.getParams().getInt("y8_point"));
        setZ8(scriptParameters.getParams().getInt("z8_point"));
        
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
        
        if (z1 < 0) {
            throw new ParameterException("z1_point", "Cannot have z1 < 0");
        }
        
        if (z1 >= image.getExtents()[2]) {
            throw new ParameterException("z1_point", "Cannot have z1 >= image.getExtents()[2]");
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
        
        if (z2 < 0) {
            throw new ParameterException("z2_point", "Cannot have z2 < 0");
        }
        
        if (z2 >= image.getExtents()[2]) {
            throw new ParameterException("z2_point", "Cannot have z2 >= image.getExtents()[2]");
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
        
        if (z3 < 0) {
            throw new ParameterException("z3_point", "Cannot have z3 < 0");
        }
        
        if (z3 >= image.getExtents()[2]) {
            throw new ParameterException("z3_point", "Cannot have z3 >= image.getExtents()[2]");
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
        
        if (z4 < 0) {
            throw new ParameterException("z4_point", "Cannot have z4 < 0");
        }
        
        if (z4 >= image.getExtents()[2]) {
            throw new ParameterException("z4_point", "Cannot have z4 >= image.getExtents()[2]");
        }
        
        if (x5 < 0) {
            throw new ParameterException("x5_point", "Cannot have x5 < 0");
        }
        
        if (x5 >= image.getExtents()[0]) {
            throw new ParameterException("x5_point", "Cannot have x5 >= image.getExtents()[0]");
        }
        
        if (y5 < 0) {
            throw new ParameterException("y5_point", "Cannot have y5 < 0");
        }
        
        if (y5 >= image.getExtents()[1]) {
            throw new ParameterException("y5_point", "Cannot have y5 >= image.getExtents()[1]");
        }
        
        if (z5 < 0) {
            throw new ParameterException("z5_point", "Cannot have z5 < 0");
        }
        
        if (z5 >= image.getExtents()[2]) {
            throw new ParameterException("z5_point", "Cannot have z5 >= image.getExtents()[2]");
        }
        
        if (x6 < 0) {
            throw new ParameterException("x6_point", "Cannot have x6 < 0");
        }
        
        if (x6 >= image.getExtents()[0]) {
            throw new ParameterException("x6_point", "Cannot have x6 >= image.getExtents()[0]");
        }
        
        if (y6 < 0) {
            throw new ParameterException("y6_point", "Cannot have y6 < 0");
        }
        
        if (y6 >= image.getExtents()[1]) {
            throw new ParameterException("y6_point", "Cannot have y6 >= image.getExtents()[1]");
        }
        
        if (z6 < 0) {
            throw new ParameterException("z6_point", "Cannot have z6 < 0");
        }
        
        if (z6 >= image.getExtents()[2]) {
            throw new ParameterException("z6_point", "Cannot have z6 >= image.getExtents()[2]");
        }
        
        if (x7 < 0) {
            throw new ParameterException("x7_point", "Cannot have x7 < 0");
        }
        
        if (x7 >= image.getExtents()[0]) {
            throw new ParameterException("x7_point", "Cannot have x7 >= image.getExtents()[0]");
        }
        
        if (y7 < 0) {
            throw new ParameterException("y7_point", "Cannot have y7 < 0");
        }
        
        if (y7 >= image.getExtents()[1]) {
            throw new ParameterException("y7_point", "Cannot have y7 >= image.getExtents()[1]");
        }
        
        if (z7 < 0) {
            throw new ParameterException("z7_point", "Cannot have z7 < 0");
        }
        
        if (z7 >= image.getExtents()[2]) {
            throw new ParameterException("z7_point", "Cannot have z7 >= image.getExtents()[2]");
        }
        
        if (x8 < 0) {
            throw new ParameterException("x8_point", "Cannot have x8 < 0");
        }
        
        if (x8 >= image.getExtents()[0]) {
            throw new ParameterException("x8_point", "Cannot have x8 >= image.getExtents()[0]");
        }
        
        if (y8 < 0) {
            throw new ParameterException("y8_point", "Cannot have y8 < 0");
        }
        
        if (y8 >= image.getExtents()[1]) {
            throw new ParameterException("y8_point", "Cannot have y8 >= image.getExtents()[1]");
        }
        
        if (z8 < 0) {
            throw new ParameterException("z8_point", "Cannot have z8 < 0");
        }
        
        if (z8 >= image.getExtents()[2]) {
            throw new ParameterException("z8_point", "Cannot have z8 >= image.getExtents()[2]");
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("x1_point", x1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y1_point", y1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z1_point", z1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x2_point", x2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y2_point", y2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z2_point", z2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x3_point", x3));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y3_point", y3));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z3_point", z3));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x4_point", x4));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y4_point", y4));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z4_point", z4));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x5_point", x5));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y5_point", y5));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z5_point", z5));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x6_point", x6));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y6_point", y6));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z6_point", z6));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x7_point", x7));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y7_point", y7));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z7_point", z7));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x8_point", x8));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y8_point", y8));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z8_point", z8));
    }

    /**
     * Initializes the GUI components and places them in the dialog.
     */
    private void init() {
        setTitle("Crop Pixels Around Tilted Cuboid");
        setSize(350, 650);
        setForeground(Color.black);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        JPanel optionPanel = new JPanel();

        // make border
        optionPanel.setBorder(buildTitledBorder("Pixels Retained Around Tilted Cuboid"));
        contentBox.add(optionPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        optionPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        // make content, place into layout
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x1Label = new JLabel("Front upper left X point:");
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

        JLabel y1Label = new JLabel("Front upper left Y point:");
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
        
        JLabel z1Label = new JLabel("Front upper left Z point:");
        z1Label.setFont(serif12);
        z1Label.setForeground(Color.black);
        z1Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(z1Label, gbc);
        optionPanel.add(z1Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        z1Input = new JTextField("0", 4);
        z1Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(z1Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(z1Input, gbc);
        optionPanel.add(z1Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x2Label = new JLabel("Front upper right X point:");
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

        JLabel y2Label = new JLabel("Front upper right Y point:");
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
        
        JLabel z2Label = new JLabel("Front upper right Z point:");
        z2Label.setFont(serif12);
        z2Label.setForeground(Color.black);
        z2Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(z2Label, gbc);
        optionPanel.add(z2Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        z2Input = new JTextField("0", 4);
        z2Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(z2Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(z2Input, gbc);
        optionPanel.add(z2Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x3Label = new JLabel("Front lower right X point:");
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

        JLabel y3Label = new JLabel("Front lower right Y point:");
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
        
        JLabel z3Label = new JLabel("Front lower right Z point:");
        z3Label.setFont(serif12);
        z3Label.setForeground(Color.black);
        z3Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(z3Label, gbc);
        optionPanel.add(z3Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        z3Input = new JTextField("0", 4);
        z3Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(z3Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(z3Input, gbc);
        optionPanel.add(z3Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x4Label = new JLabel("Front lower left X point:");
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

        JLabel y4Label = new JLabel("Front lower left Y point:");
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

        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel z4Label = new JLabel("Front lower left Z point:");
        z4Label.setFont(serif12);
        z4Label.setForeground(Color.black);
        z4Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(z4Label, gbc);
        optionPanel.add(z4Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        z4Input = new JTextField("0", 4);
        z4Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(z4Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(z4Input, gbc);
        optionPanel.add(z4Input);

        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x5Label = new JLabel("Back upper left X point:");
        x5Label.setFont(serif12);
        x5Label.setForeground(Color.black);
        x5Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x5Label, gbc);
        optionPanel.add(x5Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        x5Input = new JTextField("0", 4);
        x5Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(x5Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x5Input, gbc);
        optionPanel.add(x5Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y5Label = new JLabel("Back upper left Y point:");
        y5Label.setFont(serif12);
        y5Label.setForeground(Color.black);
        y5Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y5Label, gbc);
        optionPanel.add(y5Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        y5Input = new JTextField("0", 4);
        y5Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(y5Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y5Input, gbc);
        optionPanel.add(y5Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel z5Label = new JLabel("Back upper left Z point:");
        z5Label.setFont(serif12);
        z5Label.setForeground(Color.black);
        z5Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(z5Label, gbc);
        optionPanel.add(z5Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        z5Input = new JTextField("0", 4);
        z5Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(z5Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(z5Input, gbc);
        optionPanel.add(z5Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x6Label = new JLabel("Back upper right X point:");
        x6Label.setFont(serif12);
        x6Label.setForeground(Color.black);
        x6Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x6Label, gbc);
        optionPanel.add(x6Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        x6Input = new JTextField("0", 4);
        x6Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(x6Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x6Input, gbc);
        optionPanel.add(x6Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y6Label = new JLabel("Back upper right Y point:");
        y6Label.setFont(serif12);
        y6Label.setForeground(Color.black);
        y6Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y6Label, gbc);
        optionPanel.add(y6Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        y6Input = new JTextField("0", 4);
        y6Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(y6Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y6Input, gbc);
        optionPanel.add(y6Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel z6Label = new JLabel("Back upper right Z point:");
        z6Label.setFont(serif12);
        z6Label.setForeground(Color.black);
        z6Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(z6Label, gbc);
        optionPanel.add(z6Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        z6Input = new JTextField("0", 4);
        z6Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(z6Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(z6Input, gbc);
        optionPanel.add(z6Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x7Label = new JLabel("Back lower right X point:");
        x7Label.setFont(serif12);
        x7Label.setForeground(Color.black);
        x7Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x7Label, gbc);
        optionPanel.add(x7Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        x7Input = new JTextField("0", 4);
        x7Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(x7Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x7Input, gbc);
        optionPanel.add(x7Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y7Label = new JLabel("Back lower right Y point:");
        y7Label.setFont(serif12);
        y7Label.setForeground(Color.black);
        y7Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y7Label, gbc);
        optionPanel.add(y7Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        y7Input = new JTextField("0", 4);
        y7Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(y7Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y7Input, gbc);
        optionPanel.add(y7Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel z7Label = new JLabel("Back lower right Z point:");
        z7Label.setFont(serif12);
        z7Label.setForeground(Color.black);
        z7Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(z7Label, gbc);
        optionPanel.add(z7Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        z7Input = new JTextField("0", 4);
        z7Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(z7Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(z7Input, gbc);
        optionPanel.add(z7Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel x8Label = new JLabel("Back lower left X point:");
        x8Label.setFont(serif12);
        x8Label.setForeground(Color.black);
        x8Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(x8Label, gbc);
        optionPanel.add(x8Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        x8Input = new JTextField("0", 4);
        x8Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(x8Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(x8Input, gbc);
        optionPanel.add(x8Input);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel y8Label = new JLabel("Back lower left Y point:");
        y8Label.setFont(serif12);
        y8Label.setForeground(Color.black);
        y8Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(y8Label, gbc);
        optionPanel.add(y8Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        y8Input = new JTextField("0", 4);
        y8Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(y8Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(y8Input, gbc);
        optionPanel.add(y8Input);

        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel z8Label = new JLabel("Back lower left Z point:");
        z8Label.setFont(serif12);
        z8Label.setForeground(Color.black);
        z8Label.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(z8Label, gbc);
        optionPanel.add(z8Label);
        optionPanel.add(Box.createHorizontalStrut(10));
        z8Input = new JTextField("0", 4);
        z8Input.addActionListener(this);
        MipavUtil.makeNumericsOnly(z8Input, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(z8Input, gbc);
        optionPanel.add(z8Input);
        
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
            z1 = Integer.parseInt(z1Input.getText());
            x2 = Integer.parseInt(x2Input.getText());
            y2 = Integer.parseInt(y2Input.getText());
            z2 = Integer.parseInt(z2Input.getText());
            x3 = Integer.parseInt(x3Input.getText());
            y3 = Integer.parseInt(y3Input.getText());
            z3 = Integer.parseInt(z3Input.getText());
            x4 = Integer.parseInt(x4Input.getText());
            y4 = Integer.parseInt(y4Input.getText());
            z4 = Integer.parseInt(z4Input.getText());
            x5 = Integer.parseInt(x5Input.getText());
            y5 = Integer.parseInt(y5Input.getText());
            z5 = Integer.parseInt(z5Input.getText());
            x6 = Integer.parseInt(x6Input.getText());
            y6 = Integer.parseInt(y6Input.getText());
            z6 = Integer.parseInt(z6Input.getText());
            x7 = Integer.parseInt(x7Input.getText());
            y7 = Integer.parseInt(y7Input.getText());
            z7 = Integer.parseInt(z7Input.getText());
            x8 = Integer.parseInt(x8Input.getText());
            y8 = Integer.parseInt(y8Input.getText());
            z8 = Integer.parseInt(z8Input.getText());
        } catch (NumberFormatException nfe) {

            // an empty text-field.  decide which one is empty, then alert the user to correct
            JTextField t = determineNull();
            MipavUtil.displayError("Improper number!");
            t.requestFocus();
            t.selectAll();

            return false;
        }
        
        if (x1 < 0) {
            MipavUtil.displayError("Cannot have X1 < 0");
            x1Input.requestFocus();
            x1Input.selectAll();

            return false;
        }

        if (x1 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X1 >= image.getExtents()[0]");
            x1Input.requestFocus();
            x1Input.selectAll();

            return false;
        }
        
        if (y1 < 0) {
            MipavUtil.displayError("Cannot have Y1 < 0");
            y1Input.requestFocus();
            y1Input.selectAll();

            return false;
        }

        if (y1 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y1 >= image.getExtents()[1]");
            y1Input.requestFocus();
            y1Input.selectAll();

            return false;
        }
        
        if (z1 < 0) {
            MipavUtil.displayError("Cannot have Z1 < 0");
            z1Input.requestFocus();
            z1Input.selectAll();

            return false;
        }

        if (z1 >= image.getExtents()[2]) {
            MipavUtil.displayError("Cannot have Z1 >= image.getExtents()[2]");
            z1Input.requestFocus();
            z1Input.selectAll();

            return false;
        }
        
        if (x2 < 0) {
            MipavUtil.displayError("Cannot have X2 < 0");
            x2Input.requestFocus();
            x2Input.selectAll();

            return false;
        }

        if (x2 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X2 >= image.getExtents()[0]");
            x2Input.requestFocus();
            x2Input.selectAll();

            return false;
        }
        
        if (y2 < 0) {
            MipavUtil.displayError("Cannot have Y2 < 0");
            y2Input.requestFocus();
            y2Input.selectAll();

            return false;
        }

        if (y2 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y2 >= image.getExtents()[1]");
            y2Input.requestFocus();
            y2Input.selectAll();

            return false;
        }
        
        if (z2 < 0) {
            MipavUtil.displayError("Cannot have Z2 < 0");
            z2Input.requestFocus();
            z2Input.selectAll();

            return false;
        }

        if (z2 >= image.getExtents()[2]) {
            MipavUtil.displayError("Cannot have Z2 >= image.getExtents()[2]");
            z2Input.requestFocus();
            z2Input.selectAll();

            return false;
        }
        
        if (x3 < 0) {
            MipavUtil.displayError("Cannot have X3 < 0");
            x3Input.requestFocus();
            x3Input.selectAll();

            return false;
        }

        if (x3 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X3 >= image.getExtents()[0]");
            x3Input.requestFocus();
            x3Input.selectAll();

            return false;
        }
        
        if (y3 < 0) {
            MipavUtil.displayError("Cannot have Y3 < 0");
            y3Input.requestFocus();
            y3Input.selectAll();

            return false;
        }

        if (y3 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y3 >= image.getExtents()[1]");
            y3Input.requestFocus();
            y3Input.selectAll();

            return false;
        }
        
        if (z3 < 0) {
            MipavUtil.displayError("Cannot have Z3 < 0");
            z3Input.requestFocus();
            z3Input.selectAll();

            return false;
        }

        if (z3 >= image.getExtents()[2]) {
            MipavUtil.displayError("Cannot have Z3 >= image.getExtents()[2]");
            z3Input.requestFocus();
            z3Input.selectAll();

            return false;
        }
        
        if (x4 < 0) {
            MipavUtil.displayError("Cannot have X4 < 0");
            x4Input.requestFocus();
            x4Input.selectAll();

            return false;
        }

        if (x4 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X4 >= image.getExtents()[0]");
            x4Input.requestFocus();
            x4Input.selectAll();

            return false;
        }
        
        if (y4 < 0) {
            MipavUtil.displayError("Cannot have Y4 < 0");
            y4Input.requestFocus();
            y4Input.selectAll();

            return false;
        }

        if (y4 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y4 >= image.getExtents()[1]");
            y4Input.requestFocus();
            y4Input.selectAll();

            return false;
        }
        
        if (z4 < 0) {
            MipavUtil.displayError("Cannot have Z4 < 0");
            z4Input.requestFocus();
            z4Input.selectAll();

            return false;
        }

        if (z4 >= image.getExtents()[2]) {
            MipavUtil.displayError("Cannot have Z4 >= image.getExtents()[2]");
            z4Input.requestFocus();
            z4Input.selectAll();

            return false;
        }
        
        if (x5 < 0) {
            MipavUtil.displayError("Cannot have X5 < 0");
            x5Input.requestFocus();
            x5Input.selectAll();

            return false;
        }

        if (x5 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X5 >= image.getExtents()[0]");
            x5Input.requestFocus();
            x5Input.selectAll();

            return false;
        }
        
        if (y5 < 0) {
            MipavUtil.displayError("Cannot have Y5 < 0");
            y5Input.requestFocus();
            y5Input.selectAll();

            return false;
        }

        if (y5 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y5 >= image.getExtents()[1]");
            y5Input.requestFocus();
            y5Input.selectAll();

            return false;
        }
        
        if (z5 < 0) {
            MipavUtil.displayError("Cannot have Z5 < 0");
            z5Input.requestFocus();
            z5Input.selectAll();

            return false;
        }

        if (z5 >= image.getExtents()[2]) {
            MipavUtil.displayError("Cannot have Z5 >= image.getExtents()[2]");
            z5Input.requestFocus();
            z5Input.selectAll();

            return false;
        }
        
        if (x6 < 0) {
            MipavUtil.displayError("Cannot have X6 < 0");
            x6Input.requestFocus();
            x6Input.selectAll();

            return false;
        }

        if (x6 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X6 >= image.getExtents()[0]");
            x6Input.requestFocus();
            x6Input.selectAll();

            return false;
        }
        
        if (y6 < 0) {
            MipavUtil.displayError("Cannot have Y6 < 0");
            y6Input.requestFocus();
            y6Input.selectAll();

            return false;
        }

        if (y6 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y6 >= image.getExtents()[1]");
            y6Input.requestFocus();
            y6Input.selectAll();

            return false;
        }
        
        if (z6 < 0) {
            MipavUtil.displayError("Cannot have Z6 < 0");
            z6Input.requestFocus();
            z6Input.selectAll();

            return false;
        }

        if (z6 >= image.getExtents()[2]) {
            MipavUtil.displayError("Cannot have Z6 >= image.getExtents()[2]");
            z6Input.requestFocus();
            z6Input.selectAll();

            return false;
        }
        
        if (x7 < 0) {
            MipavUtil.displayError("Cannot have X7 < 0");
            x7Input.requestFocus();
            x7Input.selectAll();

            return false;
        }

        if (x7 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X7 >= image.getExtents()[0]");
            x7Input.requestFocus();
            x7Input.selectAll();

            return false;
        }
        
        if (y7 < 0) {
            MipavUtil.displayError("Cannot have Y7 < 0");
            y7Input.requestFocus();
            y7Input.selectAll();

            return false;
        }

        if (y7 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y7 >= image.getExtents()[1]");
            y7Input.requestFocus();
            y7Input.selectAll();

            return false;
        }
        
        if (z7 < 0) {
            MipavUtil.displayError("Cannot have Z7 < 0");
            z7Input.requestFocus();
            z7Input.selectAll();

            return false;
        }

        if (z7 >= image.getExtents()[2]) {
            MipavUtil.displayError("Cannot have Z7 >= image.getExtents()[2]");
            z7Input.requestFocus();
            z7Input.selectAll();

            return false;
        }
        
        if (x8 < 0) {
            MipavUtil.displayError("Cannot have X8 < 0");
            x8Input.requestFocus();
            x8Input.selectAll();

            return false;
        }

        if (x8 >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X8 >= image.getExtents()[0]");
            x8Input.requestFocus();
            x8Input.selectAll();

            return false;
        }
        
        if (y8 < 0) {
            MipavUtil.displayError("Cannot have Y8 < 0");
            y8Input.requestFocus();
            y8Input.selectAll();

            return false;
        }

        if (y8 >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y8 >= image.getExtents()[1]");
            y8Input.requestFocus();
            y8Input.selectAll();

            return false;
        }
        
        if (z8 < 0) {
            MipavUtil.displayError("Cannot have Z8 < 0");
            z8Input.requestFocus();
            z8Input.selectAll();

            return false;
        }

        if (z8 >= image.getExtents()[2]) {
            MipavUtil.displayError("Cannot have Z8 >= image.getExtents()[2]");
            z8Input.requestFocus();
            z8Input.selectAll();

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
                return new String("Crops image around a tilted cuboid.");
            }

            public String getDescriptionLong() {
                return new String("Crops image around a tilted cuboid.");
            }

            public String getShortLabel() {
                return new String("CropAroundTiltedCuboid");
            }

            public String getLabel() {
                return new String("Crop around Tilted Cuboid");
            }

            public String getName() {
                return new String("Crop around Tilted Cuboid");
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
            table.put(new ParameterInt("x1_point", 0));
            table.put(new ParameterInt("y1_point", 0));
            table.put(new ParameterInt("z1_point", 0));
            table.put(new ParameterInt("x2_point", 0));
            table.put(new ParameterInt("y2_point", 0));
            table.put(new ParameterInt("z2_point", 0));
            table.put(new ParameterInt("x3_point", 0));
            table.put(new ParameterInt("y3_point", 0));
            table.put(new ParameterInt("z3_point", 0));
            table.put(new ParameterInt("x4_point", 0));
            table.put(new ParameterInt("y4_point", 0));
            table.put(new ParameterInt("z4_point", 0));
            table.put(new ParameterInt("x5_point", 0));
            table.put(new ParameterInt("y5_point", 0));
            table.put(new ParameterInt("z5_point", 0));
            table.put(new ParameterInt("x6_point", 0));
            table.put(new ParameterInt("y6_point", 0));
            table.put(new ParameterInt("z6_point", 0));
            table.put(new ParameterInt("x7_point", 0));
            table.put(new ParameterInt("y7_point", 0));
            table.put(new ParameterInt("z7_point", 0));
            table.put(new ParameterInt("x8_point", 0));
            table.put(new ParameterInt("y8_point", 0));
            table.put(new ParameterInt("z8_point", 0));
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
