package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Creates the dialog to add margins around the image.
 *
 * <p>User selects:</p>
 *
 * <ol>
 *   <li>Pixels on each side</li>
 *   <li>Pixels on top and bottom</li>
 *   <li>Slices at the front of the image</li>
 *   <li>Slices at the back of the image</li>
 * </ol>
 *
 * <p>The user chooses a value for the margins, with the image minimum the default. If Color, values are chosen for red,
 * green, and blue. A new image or replacement of the old image may be selected.</p>
 */
public class JDialogAddMargins extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8624253753510650511L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int back;

    /** DOCUMENT ME! */
    private JTextField backInput;

    /** DOCUMENT ME! */
    private double blueValue = 0.0;

    /** DOCUMENT ME! */
    private JTextField bottomInput;

    /** DOCUMENT ME! */
    private int bottomSide;

    /** DOCUMENT ME! */
    private int colorFactor;

    /** DOCUMENT ME! */
    private JTextField defaultBlueInput;

    /** DOCUMENT ME! */
    private JTextField defaultGreenInput;

    /** DOCUMENT ME! */
    private JTextField defaultRedInput;

    /** DOCUMENT ME! */
    private double defaultValue = 0.0;

    /** DOCUMENT ME! */
    private JTextField defaultValueInput;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private int front;

    /** DOCUMENT ME! */
    private JTextField frontInput;

    /** DOCUMENT ME! */
    private double greenValue = 0.0;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private AlgorithmAddMargins imageMarginsAlgo;

    /** DOCUMENT ME! */
    private int leftSide;

    /** DOCUMENT ME! */
    private JTextField leftSideInput;

    /** DOCUMENT ME! */
    private ButtonGroup loopingGroup;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton noBuffer;

    /** DOCUMENT ME! */
    private double redValue = 0.0;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int rightSide;

    /** DOCUMENT ME! */
    private JTextField rightSideInput;

    /** DOCUMENT ME! */
    private JTextField topInput;

    /** DOCUMENT ME! */
    private int topSide;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton usingBuffer;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAddMargins() { }

    /**
     * Constructor that makes dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogAddMargins(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;

        if (image.isColorImage() == false) {
            colorFactor = 1;
        } else {
            colorFactor = 4;
        }

        userInterface = ViewUserInterface.getReference();
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
            MipavUtil.showHelp("10062");
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
        if (algorithm instanceof AlgorithmAddMargins) {

            if ((imageMarginsAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // put the new image into a new frame
                    new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } // if (imageMarginsAlgo.isCompleted() == true && resultImage != null)
            else if ((imageMarginsAlgo.isCompleted() == true) && (resultImage == null)) {

                image = imageMarginsAlgo.getSrcImage();

                try {
                    new ViewJFrameImage(image, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        imageMarginsAlgo.finalize();
        imageMarginsAlgo = null;
        dispose();
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
     * Accessor that sets the back value.
     *
     * @param  x  Value to set back value to.
     */
    public void setBack(int x) {
        back = x;
    }

    /**
     * Accessor that sets the blue value.
     *
     * @param  x  Value to set blue value to.
     */
    public void setBlue(double x) {
        blueValue = x;
    }

    /**
     * Accessor that sets the bottom side value.
     *
     * @param  x  Value to set bottom side value to.
     */
    public void setBottom(int x) {
        bottomSide = x;
    }

    /**
     * Accessor that sets the default value.
     *
     * @param  x  Value to set default value to.
     */
    public void setDefault(double x) {
        defaultValue = x;
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * Accessor that sets the front value.
     *
     * @param  x  Value to set front value to.
     */
    public void setFront(int x) {
        front = x;
    }

    /**
     * Accessor that sets the green value.
     *
     * @param  x  Value to set green value to.
     */
    public void setGreen(double x) {
        greenValue = x;
    }

    /**
     * Accessor that sets the left side value.
     *
     * @param  x  Value to set left side value to.
     */
    public void setLeft(int x) {
        leftSide = x;
    }

    /**
     * Accessor that sets the red value.
     *
     * @param  x  Value to set red value to.
     */
    public void setRed(double x) {
        redValue = x;
    }

    /**
     * Accessor that sets the right side value.
     *
     * @param  x  Value to set right side value to.
     */
    public void setRight(int x) {
        rightSide = x;
    }

    /**
     * Accessor that sets the top side value.
     *
     * @param  x  Value to set top side value to.
     */
    public void setTop(int x) {
        topSide = x;
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
            t = rightSideInput.getText();

            if (t.equals("")) {
                return rightSideInput;
            }

            t = leftSideInput.getText();

            if (t.equals("")) {
                return leftSideInput;
            }

            t = topInput.getText();

            if (t.equals("")) {
                return topInput;
            }

            t = bottomInput.getText();

            if (t.equals("")) {
                return bottomInput;
            }

            t = frontInput.getText();

            if (t.equals("")) {
                return frontInput;
            }

            t = backInput.getText();

            if (t.equals("")) {
                return backInput;
            }

            return defaultValueInput;
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogAddMargins reports: Unknown Error");

            return rightSideInput; // gotta have some thing returned
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("left_side", leftSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("right_side", rightSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("top_side", topSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bottom_side", bottomSide));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("front", front));
        scriptParameters.getParams().put(ParameterFactory.newParameter("back", back));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("margin_value", defaultValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("margin_value_rgb", new double[] {redValue, greenValue, blueValue}));
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams(){
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        
        if (image.isColorImage() == false) {
            colorFactor = 1;
        } else {
            colorFactor = 4;
        }
        
        rightSide = scriptParameters.getParams().getInt("right_side");
        leftSide = scriptParameters.getParams().getInt("left_side");
        topSide = scriptParameters.getParams().getInt("top_side");
        bottomSide = scriptParameters.getParams().getInt("bottom_side");
        
        front = scriptParameters.getParams().getInt("front");
        back = scriptParameters.getParams().getInt("back");
        
        defaultValue = scriptParameters.getParams().getDouble("margin_value");
        
        double[] rgb = scriptParameters.getParams().getList("margin_value_rgb").getAsDoubleArray();
        redValue = rgb[0];
        greenValue = rgb[1];
        blueValue = rgb[2];
    }
    
    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }
    
    /**
     * Once all the necessary variables are set, call the Image Margins algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        if (displayLoc == NEW) {
            int[] destExtents = null; // length along an axis of the destination image

            if (image.getNDims() == 2) {

                try {
                    destExtents = new int[2];
                    destExtents[0] = image.getExtents()[0] + rightSide + leftSide;
                    destExtents[1] = image.getExtents()[1] + topSide + bottomSide;

                    resultImage = new ModelImage(image.getType(), destExtents, image.getImageName());
                    resultImage.setMatrix(image.getMatrix());

                    // resultImage.
                    if (colorFactor == 1) {
                        imageMarginsAlgo = new AlgorithmAddMargins(image, resultImage, defaultValue, leftSide,
                                                                   rightSide, topSide);
                    } else {
                        imageMarginsAlgo = new AlgorithmAddMargins(image, resultImage, redValue, greenValue, blueValue,
                                                                   leftSide, rightSide, topSide);
                    }

                    // when using the local-buffer method of the algorithm.  false is default
                    // imageMarginsAlgo.performCopiesWithBuffers(usingBuffer.isSelected());
                    imageMarginsAlgo.performCopiesWithBuffers(false);

                    // Listen to the algorithm so we get notified when it is succeeded or failed.
                    // See algorithm performed event.  caused by implementing AlgorithmedPerformed interface
                    imageMarginsAlgo.addListener(this);
                    createProgressBar(image.getImageName(), imageMarginsAlgo);
                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (imageMarginsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("AddMargins reports: A thread is already running on this object [addMarginsAlgo]");
                        }
                    } else {
                      
                        imageMarginsAlgo.run();
                    }
                } catch (OutOfMemoryError oome) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    MipavUtil.displayError("AddMargins reports: unable to allocate enough memory");

                    return;
                }
            } else if ((image.getNDims() == 3) || (image.getNDims() == 4)) {

                try {

                    if (image.getNDims() == 3) {
                        destExtents = new int[3];
                        destExtents[0] = image.getExtents()[0] + rightSide + leftSide;
                        destExtents[1] = image.getExtents()[1] + topSide + bottomSide;
                        destExtents[2] = image.getExtents()[2] + front + back;
                    } else {
                        destExtents = new int[4];
                        destExtents[0] = image.getExtents()[0] + rightSide + leftSide;
                        destExtents[1] = image.getExtents()[1] + topSide + bottomSide;
                        destExtents[2] = image.getExtents()[2] + front + back;
                        destExtents[3] = image.getExtents()[3];
                    }

                    resultImage = new ModelImage(image.getType(), destExtents, image.getImageName());
                    resultImage.setMatrix(image.getMatrix());

                    // preload this image with the minimum of the source image
                    // resultImage.
                    if (colorFactor == 1) {
                        imageMarginsAlgo = new AlgorithmAddMargins(image, resultImage, defaultValue, leftSide,
                                                                   rightSide, topSide, front, back);
                    } else {
                        imageMarginsAlgo = new AlgorithmAddMargins(image, resultImage, redValue, greenValue, blueValue,
                                                                   leftSide, rightSide, topSide, front, back);
                    }

                    // when using the local-buffer method of the algorithm,  false is default
                    // imageMarginsAlgo.performCopiesWithBuffers(usingBuffer.isSelected());
                    imageMarginsAlgo.performCopiesWithBuffers(false);

                    // Listen to the algorithm so we get notified when it is succeeded or failed.
                    // See algorithm performed event.  caused by implementing AlgorithmedPerformed interface
                    imageMarginsAlgo.addListener(this);
                    createProgressBar(image.getImageName(), imageMarginsAlgo);
                    
                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (imageMarginsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("AddMargins reports: A thread is already running on this object [addMarginsAlgo]");
                        }
                    } else {
                       
                        imageMarginsAlgo.run();
                    }
                } catch (OutOfMemoryError oome) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    MipavUtil.displayError("AddMargins reports: unable to allocate enough memory");

                    return;
                }
            }
        } // if (displayLoc == NEW)
        else { // displayLoc == REPLACE

            if (image.getNDims() == 2) {

                try {

                    if (colorFactor == 1) {
                        imageMarginsAlgo = new AlgorithmAddMargins(image, defaultValue, leftSide, rightSide, topSide,
                                                                   bottomSide);
                    } else {
                        imageMarginsAlgo = new AlgorithmAddMargins(image, redValue, greenValue, blueValue, leftSide,
                                                                   rightSide, topSide, bottomSide);
                    }

                    imageMarginsAlgo.addListener(this);
                    createProgressBar(image.getImageName(), imageMarginsAlgo);
                    
                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    /*Vector imageFrames = image.getImageFrameVector();
                     *
                     * titles = new String[imageFrames.size()]; for ( int i = 0; i < imageFrames.size(); i++ ) { titles[i]
                     * = ( (Frame) ( imageFrames.elementAt( i ) ) ).getTitle(); ( (Frame) ( imageFrames.elementAt( i ) )
                     * ).setTitle( "Locked: " + titles[i] ); ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled(
                     * false ); userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) ); }*/

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (imageMarginsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                    
                        imageMarginsAlgo.run();
                    }
                } catch (OutOfMemoryError oome) {
                    MipavUtil.displayError("AddMargins reports: unable to allocate enough memory");

                    return;
                }
            } // if (image.getNDims() == 2)
            else if ((image.getNDims() == 3) || (image.getNDims() == 4)) {

                try {

                    if (colorFactor == 1) {
                        imageMarginsAlgo = new AlgorithmAddMargins(image, defaultValue, leftSide, rightSide, topSide,
                                                                   bottomSide, front, back);
                    } else {
                        imageMarginsAlgo = new AlgorithmAddMargins(image, redValue, greenValue, blueValue, leftSide,
                                                                   rightSide, topSide, bottomSide, front, back);
                    }

                    imageMarginsAlgo.addListener(this);
                    createProgressBar(image.getImageName(), imageMarginsAlgo);
                    
                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    /*Vector imageFrames = image.getImageFrameVector();
                     *
                     * titles = new String[imageFrames.size()]; for ( int i = 0; i < imageFrames.size(); i++ ) { titles[i]
                     * = ( (Frame) ( imageFrames.elementAt( i ) ) ).getTitle(); ( (Frame) ( imageFrames.elementAt( i ) )
                     * ).setTitle( "Locked: " + titles[i] ); ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled(
                     * false ); userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) ); }*/

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (imageMarginsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                      
                        imageMarginsAlgo.run();
                    }
                } catch (OutOfMemoryError oome) {
                    MipavUtil.displayError("AddMargins reports: unable to allocate enough memory");

                    return;
                }
            } // else if ((image.getNDims == 3) || (image.getNDims() == 4))

        } // else displayLoc == REPLACE

    }

    /**
     * Initializes the GUI components and places them in the dialog.
     */
    private void init() {
        setTitle("Add Image Border");
        setSize(350, 230);
        setForeground(Color.black);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        JPanel optionPanel = new JPanel();

        // make border
        optionPanel.setBorder(buildTitledBorder("Margins Around Image"));
        contentBox.add(optionPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        optionPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        // make content, place into layout

        // left
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel leftSideLabel = new JLabel("Pixels on the left side:");
        leftSideLabel.setFont(serif12);
        leftSideLabel.setForeground(Color.black);
        leftSideLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(leftSideLabel, gbc);
        optionPanel.add(leftSideLabel);
        optionPanel.add(Box.createHorizontalStrut(10));

        leftSideInput = new JTextField("0", 4);
        leftSideInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(leftSideInput, false);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(leftSideInput, gbc);
        optionPanel.add(leftSideInput);

        // right
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel rightSideLabel = new JLabel("Pixels on the right side:");
        rightSideLabel.setFont(serif12);
        rightSideLabel.setForeground(Color.black);
        rightSideLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(rightSideLabel, gbc);
        optionPanel.add(rightSideLabel);
        optionPanel.add(Box.createHorizontalStrut(10));

        rightSideInput = new JTextField("0", 4);
        rightSideInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(rightSideInput, false);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(rightSideInput, gbc);
        optionPanel.add(rightSideInput);

        // top
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel topLabel = new JLabel("Pixels on top:");
        topLabel.setFont(serif12);
        topLabel.setForeground(Color.black);
        topLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(topLabel, gbc);
        optionPanel.add(topLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        topInput = new JTextField("0", 4);
        topInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(topInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(topInput, gbc);
        optionPanel.add(topInput);

        // bottom
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel bottomLabel = new JLabel("Pixels on bottom:");
        bottomLabel.setFont(serif12);
        bottomLabel.setForeground(Color.black);
        bottomLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(bottomLabel, gbc);
        optionPanel.add(bottomLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        bottomInput = new JTextField("0", 4);
        bottomInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(bottomInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(bottomInput, gbc);
        optionPanel.add(bottomInput);

        // front
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel frontLabel = new JLabel("Slices at the front of image:");
        frontLabel.setFont(serif12);
        frontLabel.setForeground(Color.black);
        frontLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(frontLabel, gbc);
        optionPanel.add(frontLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        frontInput = new JTextField("0", 4);
        frontInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(frontInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(frontInput, gbc);
        optionPanel.add(frontInput);

        // back
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel backLabel = new JLabel("Slices at the back of image:");
        backLabel.setFont(serif12);
        backLabel.setForeground(Color.black);
        backLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(backLabel, gbc);
        optionPanel.add(backLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        backInput = new JTextField("0", 4);
        backInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(backInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(backInput, gbc);
        optionPanel.add(backInput);

        // default margin value
        JPanel defaultValuePanel = new JPanel();
        defaultValuePanel.setBorder(buildTitledBorder("Select Margin Value"));

        if (image.isColorImage() == false) {

            // set layout
            gbl = new GridBagLayout();
            gbc = new GridBagConstraints();
            defaultValuePanel.setLayout(gbl);
            gbc.anchor = GridBagConstraints.NORTHWEST;

            // make content, place into layout
            JLabel defaultLabel = new JLabel("Value for margins (image minimum is default)");
            defaultLabel.setFont(serif12);
            defaultLabel.setForeground(Color.black);
            defaultLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(defaultLabel, gbc);
            defaultValuePanel.add(defaultLabel);
            defaultValuePanel.add(Box.createHorizontalStrut(10));
            defaultValueInput = new JTextField(Double.toString(image.getMin()), 8);
            defaultValueInput.addActionListener(this);
            MipavUtil.makeNumericsOnly(defaultValueInput, true);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(defaultValueInput, gbc);
            defaultValuePanel.add(defaultValueInput);
            contentBox.add(defaultValuePanel);
        } else { // color image
            gbl = new GridBagLayout();
            gbc = new GridBagConstraints();
            defaultValuePanel.setLayout(gbl);
            gbc.anchor = GridBagConstraints.NORTHWEST;

            // make content, place into layout
            JLabel redLabel = new JLabel("Red margin value (image minimum is default)");
            redLabel.setFont(serif12);
            redLabel.setForeground(Color.black);
            redLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(redLabel, gbc);
            defaultValuePanel.add(redLabel);
            defaultValuePanel.add(Box.createHorizontalStrut(10));
            defaultRedInput = new JTextField(Double.toString(image.getMinR()), 8);
            defaultRedInput.addActionListener(this);
            MipavUtil.makeNumericsOnly(defaultRedInput, true);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(defaultRedInput, gbc);
            defaultValuePanel.add(defaultRedInput);

            JLabel greenLabel = new JLabel("Green margin value (image minimum is default)");
            greenLabel.setFont(serif12);
            greenLabel.setForeground(Color.black);
            greenLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(greenLabel, gbc);
            defaultValuePanel.add(greenLabel);
            defaultValuePanel.add(Box.createHorizontalStrut(10));
            defaultGreenInput = new JTextField(Double.toString(image.getMinG()), 8);
            defaultGreenInput.addActionListener(this);
            MipavUtil.makeNumericsOnly(defaultGreenInput, true);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(defaultGreenInput, gbc);
            defaultValuePanel.add(defaultGreenInput);

            JLabel blueLabel = new JLabel("Blue margin value (image minimum is default)");
            blueLabel.setFont(serif12);
            blueLabel.setForeground(Color.black);
            blueLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(blueLabel, gbc);
            defaultValuePanel.add(blueLabel);
            defaultValuePanel.add(Box.createHorizontalStrut(10));
            defaultBlueInput = new JTextField(Double.toString(image.getMinB()), 8);
            defaultBlueInput.addActionListener(this);
            MipavUtil.makeNumericsOnly(defaultBlueInput, true);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(defaultBlueInput, gbc);
            defaultValuePanel.add(defaultBlueInput);

            contentBox.add(defaultValuePanel);
        }

        // image destination select
        JPanel destPanel = new JPanel(); // panel carries no content but box & border
        destPanel.setBorder(buildTitledBorder("Select Destination"));

        Box destinationBox = new Box(BoxLayout.Y_AXIS);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New Image", true);
        newImage.setFont(serif12);
        newImage.addActionListener(this);
        destinationGroup.add(newImage);
        destinationBox.add(newImage);
        newImage.setEnabled(true);

        replaceImage = new JRadioButton("Replace Image", false);
        replaceImage.setFont(serif12);
        replaceImage.addActionListener(this);
        destinationGroup.add(replaceImage);
        destinationBox.add(replaceImage);
        replaceImage.setEnabled(true);
        destPanel.add(destinationBox);
        contentBox.add(destPanel);

        // test speed panel (choice 1: algo has personal buff, imported into img,
        // choice 2: algo calcs where to insert a row.  methinks fewer loops)
        /*
         * JPanel loopingTestPanel = new JPanel(); loopingTestPanel.setBorder(buildTitledBorder("Looping test")); Box
         * loopingTestBox = new Box(BoxLayout.Y_AXIS); loopingGroup = new ButtonGroup(); noBuffer = new JRadioButton("No
         * Buffer", true); noBuffer.setFont(serif12); noBuffer.addActionListener(this); loopingGroup.add(noBuffer);
         * loopingTestBox.add(noBuffer); usingBuffer = new JRadioButton("Uses Buffer", false);
         * usingBuffer.setFont(serif12); usingBuffer.addActionListener(this); loopingGroup.add(usingBuffer);
         * loopingTestBox.add(usingBuffer); loopingTestPanel.add(loopingTestBox); contentBox.add(loopingTestPanel);
         */
        // end looping test display

        /*
         * JPanel OKCancelPanel = new JPanel(new FlowLayout()); OKButton = buildOKButton(); OKCancelPanel.add(OKButton);
         *
         * cancelButton = buildCancelButton(); OKCancelPanel.add(cancelButton); contentBox.add(OKCancelPanel);
         */
        contentBox.add(buildButtons());

        // if this is a 2D image, turn off slice margins
        if (image.getNDims() == 2) {
            frontLabel.setEnabled(false);
            frontInput.setEnabled(false);
            backLabel.setEnabled(false);
            backInput.setEnabled(false);
        }

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
            rightSide = Integer.parseInt(rightSideInput.getText()); // in pixels
            leftSide = Integer.parseInt(leftSideInput.getText()); // in pixels
            topSide = Integer.parseInt(topInput.getText()); // in pixels
            bottomSide = Integer.parseInt(bottomInput.getText()); // in pixels
            front = Integer.parseInt(frontInput.getText()); // in slices
            back = Integer.parseInt(backInput.getText()); // in slices

            if (colorFactor == 1) {
                defaultValue = Double.parseDouble(defaultValueInput.getText());
            } else {
                redValue = Double.parseDouble(defaultRedInput.getText());
                greenValue = Double.parseDouble(defaultGreenInput.getText());
                blueValue = Double.parseDouble(defaultBlueInput.getText());
            }
        } catch (NumberFormatException nfe) {

            // an empty text-field.  decide which one is empty, then alert the user to correct
            JTextField t = determineNull();
            MipavUtil.displayError("Improper number!");
            t.requestFocus();
            t.selectAll();

            return false;
        }

        if (newImage.isSelected()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
        }

        return true;
    }
}
