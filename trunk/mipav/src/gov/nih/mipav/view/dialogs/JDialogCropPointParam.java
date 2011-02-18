package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Creates the dialog to crop pixels around the selected point.
 *
 * <p>User selects:</p>
 *
 * <ol>
 *   <li>Point around which pixels are retained
 *   <li>Pixels retained on each side</li>
 *   <li>Pixels retained on top and bottom</li>
 *   <li>Slices retained at the front and back</li>
 * </ol>
 *
 * <p>A new image or replacement of the old image may be selected.</p>
 */
public class JDialogCropPointParam extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int back;

    /** DOCUMENT ME! */
    private JTextField backInput;

    /** DOCUMENT ME! */
    private JTextField bottomInput;

    /** DOCUMENT ME! */
    private int bottomSide;

    /** DOCUMENT ME! */
    private AlgorithmAddMargins cropAlgo;

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
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int leftSide;

    /** DOCUMENT ME! */
    private JTextField leftSideInput;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int rightSide;

    /** DOCUMENT ME! */
    private JTextField rightSideInput;
    
    private int xPoint;
    
    private JTextField xPointInput;
    
    private int yPoint;
    
    private JTextField yPointInput;
    
    private int zPoint;
    
    private JTextField zPointInput;

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private JTextField topInput;

    /** DOCUMENT ME! */
    private int topSide;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private int[] xBounds = new int[2];

    /** DOCUMENT ME! */
    private int[] yBounds = new int[2];

    /** DOCUMENT ME! */
    private int[] zBounds = new int[2];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCropPointParam() { }

    /**
     * Constructor that makes dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCropPointParam(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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
        		//MipavUtil.showHelp("");
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

            if ((cropAlgo.isCompleted() == true) && (resultImage != null)) { // in StoreInDest; "new Image"

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                insertScriptLine();

            } else if ((cropAlgo.isCompleted() == true) && (resultImage == null)) {

                image = cropAlgo.getSrcImage();

                try {
                    new ViewJFrameImage(image, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                insertScriptLine();

            } else if (cropAlgo.isCompleted() == false) {

                // algorithm failed but result image still has garbage
                if (resultImage != null) {
                    resultImage.disposeLocal(); // clean up memory
                }

                resultImage = null;

                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }
            }
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        cropAlgo.finalize();
        cropAlgo = null;
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
     * Accessor that sets the bottom side value.
     *
     * @param  x  Value to set bottom side value to.
     */
    public void setBottom(int x) {
        bottomSide = x;
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
     * Accessor that sets the left side value.
     *
     * @param  x  Value to set left side value to.
     */
    public void setLeft(int x) {
        leftSide = x;
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
     * 
     * @param xPoint
     */
    public void setXPoint(int xPoint) {
        this.xPoint = xPoint;
    }
    
    /**
     * 
     * @param yPoint
     */
    public void setYPoint(int yPoint) {
        this.yPoint = yPoint;
    }
    
    /**
     * 
     * @param zPoint
     */
    public void setZPoint(int zPoint) {
        this.zPoint = zPoint;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

        if (displayLoc == NEW) {

            try {
                int[] destExtents = null;

                if (image.getNDims() == 2) {
                    destExtents = new int[2];
                    destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1;
                    destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1;
                } else if (image.getNDims() == 3) {
                    destExtents = new int[3];
                    destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1;
                    destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1;
                    destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);

                } else if (image.getNDims() == 4) {
                    destExtents = new int[4];
                    destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1;
                    destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1;
                    destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
                    destExtents[3] = image.getExtents()[3];
                } else {
                    return;
                }

                // Make result image
                resultImage = new ModelImage(image.getType(), destExtents,
                        makeImageName(image.getImageName(), "_crop"));
                if (image.getNDims() >= 2) {
                    xBounds[0] *= -1;
                    xBounds[1] = resultImage.getExtents()[0] - image.getExtents()[0];
                    yBounds[0] *= -1;
                    yBounds[1] = resultImage.getExtents()[1] - image.getExtents()[1];
                }
                if (image.getNDims() >= 3) {
                    zBounds[0] *= -1;
                    zBounds[1] = resultImage.getExtents()[2] - image.getExtents()[2];
                }
                cropAlgo = new AlgorithmAddMargins(image, resultImage, xBounds, yBounds, zBounds);

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
                MipavUtil.displayError("Dialog CropPointParam: unable to allocate enough memory");

                return;
            }
        } // if (displayLoc == NEW)
        else { // displayLoc == REPLACE

            try {
                if (image.getNDims() >= 2) {
                    xBounds[0] *= -1;
                    xBounds[1] = resultImage.getExtents()[0] - image.getExtents()[0];
                    yBounds[0] *= -1;
                    yBounds[1] = resultImage.getExtents()[1] - image.getExtents()[1];
                }
                if (image.getNDims() >= 3) {
                    zBounds[0] *= -1;
                    zBounds[1] = resultImage.getExtents()[2] - image.getExtents()[2];
                }
                cropAlgo = new AlgorithmAddMargins(image, xBounds, yBounds, zBounds);
                cropAlgo.addListener(this);

                createProgressBar(image.getImageName(), cropAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                /*Vector imageFrames = image.getImageFrameVector();
                 *
                 * titles = new String[imageFrames.size()]; for ( int i = 0; i < imageFrames.size(); i++ ) { titles[i] = (
                 * (Frame) ( imageFrames.elementAt( i ) ) ).getTitle(); ( (Frame) ( imageFrames.elementAt( i ) )
                 * ).setTitle( "Locked: " + titles[i] ); ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                 * userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );}*/

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (cropAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    cropAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog CropPointParam: unable to allocate enough memory");

                return;
            }

        } // else displayLoc == REPLACE
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
            t = xPointInput.getText();
            if (t.equals("")) {
                return xPointInput;
            }
            
            t = yPointInput.getText();
            if (t.equals("")) {
                return yPointInput;
            }
            
            t = zPointInput.getText();
            if (t.equals("")) {
                return zPointInput;
            }
            
            
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

            return rightSideInput;
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogCropPointParam reports: Unknown Error");

            return rightSideInput; // gotta have some thing returned
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setXPoint(scriptParameters.getParams().getInt("x_point"));
        setYPoint(scriptParameters.getParams().getInt("y_point"));
        setZPoint(scriptParameters.getParams().getInt("z_point"));
        setRight(scriptParameters.getParams().getInt("right_side"));
        setLeft(scriptParameters.getParams().getInt("left_side"));
        setTop(scriptParameters.getParams().getInt("top_side"));
        setBottom(scriptParameters.getParams().getInt("bottom_side"));
        setFront(scriptParameters.getParams().getInt("front"));
        setBack(scriptParameters.getParams().getInt("back"));
        
        if (xPoint < 0) {
            throw new ParameterException("x_point", "Cannot have xPoint < 0");
        }
        
        if (xPoint >= image.getExtents()[0]) {
            throw new ParameterException("x_point", "Cannot have xPoint >= image.getExtents()[0]");
        }
        
        if (yPoint < 0) {
            throw new ParameterException("y_point", "Cannot have yPoint < 0");
        }
        
        if (yPoint >= image.getExtents()[1]) {
            throw new ParameterException("y_point", "Cannot have yPoint >= image.getExtents()[1]");
        }
        
        if (image.getNDims() >= 3) {
            if (zPoint < 0) {
                throw new ParameterException("z_point", "Cannot have zPoint < 0");
            }
            
            if (zPoint >= image.getExtents()[2]) {
                throw new ParameterException("z_point", "Cannot have zPoint >= image.getExtents()[2]");
            }    
        } // if (image.getNDims() >= 3)

        if (rightSide < 0) {
            throw new ParameterException("right_side", "Cannot have rightSide < 0");
        }

        if ((rightSide + xPoint) >= image.getExtents()[0]) {
            throw new ParameterException("right_side", "Cannot have (rightSide + xPoint) >= image.getExtents()[0]");
        }

        if (leftSide < 0) {
            throw new ParameterException("left_side", "Cannot have leftSide < 0");
        }

        if (leftSide >= (xPoint + 1)) {
            throw new ParameterException("left_side", "Cannot have leftSide >= (xPoint + 1)");
        }

        if (topSide < 0) {
            throw new ParameterException("top_side", "Cannot have topSide < 0");
        }

        if (topSide >= (yPoint+1)) {
            throw new ParameterException("top_side", "Cannot have topSide >= (yPoint + 1)");
        }

        if (bottomSide < 0) {
            throw new ParameterException("bottom_side", "Cannot have bottomSide < 0");
        }

        if ((bottomSide + yPoint) >= image.getExtents()[1]) {
            throw new ParameterException("bottom_side", "Cannot have (bottomSide + yPoint) >= image.getExtents()[1]");
        }


        if (image.getNDims() >= 3) {

            if (front < 0) {
                throw new ParameterException("front", "Cannot have front < 0");
            }

            if (front >= (zPoint + 1)) {
                throw new ParameterException("front", "Cannot have front >= (zPoint + 1)");
            }

            if (back < 0) {
                throw new ParameterException("back", "Cannot have back < 0");
            }

            if ((back + zPoint) >= image.getExtents()[2]) {
                throw new ParameterException("back", "Cannot have (back + zPoint) >= image.getExtents()[2]");
            }

        } // if (image.getNDims() >= 3)


        xBounds[0] = xPoint - leftSide;
        xBounds[1] = xPoint + rightSide;
        yBounds[0] = yPoint - topSide;
        yBounds[1] = yPoint + bottomSide;

        if (image.getNDims() >= 3) {
            zBounds[0] = zPoint - front;
            zBounds[1] = zPoint + back;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("x_point", xPoint));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y_point", yPoint));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z_point", zPoint));
        scriptParameters.getParams().put(ParameterFactory.newParameter("right_side", rightSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("left_side", leftSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("top_side", topSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bottom_side", bottomSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("front", front));
        scriptParameters.getParams().put(ParameterFactory.newParameter("back", back));
    }

    /**
     * Initializes the GUI components and places them in the dialog.
     */
    private void init() {
        setTitle("Crop Pixels Around Selected Point");
        setSize(350, 230);
        setForeground(Color.black);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        JPanel optionPanel = new JPanel();

        // make border
        optionPanel.setBorder(buildTitledBorder("Pixels Retained Around Selected Point"));
        contentBox.add(optionPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        optionPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        // make content, place into layout
        
        optionPanel.add(Box.createHorizontalStrut(10));
        
        JLabel xPointLabel = new JLabel("X point:");
        xPointLabel.setFont(serif12);
        xPointLabel.setForeground(Color.black);
        xPointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(xPointLabel, gbc);
        optionPanel.add(xPointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        xPointInput = new JTextField("0", 4);
        xPointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(xPointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(xPointInput, gbc);
        optionPanel.add(xPointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel yPointLabel = new JLabel("Y point:");
        yPointLabel.setFont(serif12);
        yPointLabel.setForeground(Color.black);
        yPointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(yPointLabel, gbc);
        optionPanel.add(yPointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        yPointInput = new JTextField("0", 4);
        yPointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(yPointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(yPointInput, gbc);
        optionPanel.add(yPointInput);
        
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel zPointLabel = new JLabel("Z point:");
        zPointLabel.setFont(serif12);
        zPointLabel.setForeground(Color.black);
        zPointLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2; // GridBagConstraints.RELATIVE;
        gbl.setConstraints(zPointLabel, gbc);
        optionPanel.add(zPointLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        zPointInput = new JTextField("0", 4);
        zPointInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(zPointInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(zPointInput, gbc);
        optionPanel.add(zPointInput);

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

        JLabel frontLabel = new JLabel("Slices at the front:");
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

        JLabel backLabel = new JLabel("Slices at the back:");
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
            zPointLabel.setEnabled(false);
            zPointInput.setEnabled(false);
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
            xPoint = Integer.parseInt(xPointInput.getText());
            yPoint = Integer.parseInt(yPointInput.getText());
            zPoint = Integer.parseInt(zPointInput.getText());
            rightSide = Integer.parseInt(rightSideInput.getText()); // in pixels
            leftSide = Integer.parseInt(leftSideInput.getText()); // in pixels
            topSide = Integer.parseInt(topInput.getText()); // in pixels
            bottomSide = Integer.parseInt(bottomInput.getText()); // in pixels
            front = Integer.parseInt(frontInput.getText()); // in slices
            back = Integer.parseInt(backInput.getText()); // in slices
        } catch (NumberFormatException nfe) {

            // an empty text-field.  decide which one is empty, then alert the user to correct
            JTextField t = determineNull();
            MipavUtil.displayError("Improper number!");
            t.requestFocus();
            t.selectAll();

            return false;
        }
        
        if (xPoint < 0) {
            MipavUtil.displayError("Cannot have X point < 0");
            xPointInput.requestFocus();
            xPointInput.selectAll();

            return false;
        }

        if (xPoint >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have X point >= image.getExtents()[0]");
            xPointInput.requestFocus();
            xPointInput.selectAll();

            return false;
        }
        
        if (yPoint < 0) {
            MipavUtil.displayError("Cannot have Y point < 0");
            yPointInput.requestFocus();
            yPointInput.selectAll();

            return false;
        }

        if (yPoint >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have Y point >= image.getExtents()[1]");
            yPointInput.requestFocus();
            yPointInput.selectAll();

            return false;
        }
        
        if (image.getNDims() >= 3) {
            if (zPoint < 0) {
                MipavUtil.displayError("Cannot have Z point < 0");
                zPointInput.requestFocus();
                zPointInput.selectAll();
    
                return false;
            }
    
            if (zPoint >= image.getExtents()[2]) {
                MipavUtil.displayError("Cannot have Z point >= image.getExtents()[2]");
                zPointInput.requestFocus();
                zPointInput.selectAll();
    
                return false;
            }
        } // if (image.getNDims() >= 3)

        if (rightSide < 0) {
            MipavUtil.displayError("Cannot have rightSide < 0");
            rightSideInput.requestFocus();
            rightSideInput.selectAll();

            return false;
        }

        if ((rightSide + xPoint) >= image.getExtents()[0]) {
            MipavUtil.displayError("Cannot have (rightSide + xPoint) >= image.getExtents()[0]");
            rightSideInput.requestFocus();
            rightSideInput.selectAll();

            return false;
        }

        if (leftSide < 0) {
            MipavUtil.displayError("Cannot have leftSide < 0");
            leftSideInput.requestFocus();
            leftSideInput.selectAll();

            return false;
        }

        if (leftSide >= (xPoint + 1)) {
            MipavUtil.displayError("Cannot have leftSide >= (xPoint + 1)");
            leftSideInput.requestFocus();
            leftSideInput.selectAll();

            return false;
        }

        if (topSide < 0) {
            MipavUtil.displayError("Cannot have topSide < 0");
            topInput.requestFocus();
            topInput.selectAll();

            return false;
        }

        if (topSide >= (yPoint+1)) {
            MipavUtil.displayError("Cannot have topSide >= (yPoint+1)");
            topInput.requestFocus();
            topInput.selectAll();

            return false;
        }

        if (bottomSide < 0) {
            MipavUtil.displayError("Cannot have bottomSide < 0");
            bottomInput.requestFocus();
            bottomInput.selectAll();

            return false;
        }

        if ((bottomSide + yPoint) >= image.getExtents()[1]) {
            MipavUtil.displayError("Cannot have (bottomSide + yPoint) >= image.getExtents()[1]");
            bottomInput.requestFocus();
            bottomInput.selectAll();

            return false;
        }


        if (image.getNDims() >= 3) {

            if (front < 0) {
                MipavUtil.displayError("Cannot have front < 0");
                frontInput.requestFocus();
                frontInput.selectAll();

                return false;
            }

            if (front >= (zPoint + 1)) {
                MipavUtil.displayError("Cannot have front >= (zPoint + 1)");
                frontInput.requestFocus();
                frontInput.selectAll();

                return false;
            }

            if (back < 0) {
                MipavUtil.displayError("Cannot have back < 0");
                backInput.requestFocus();
                backInput.selectAll();

                return false;
            }

            if ((back + zPoint) >= image.getExtents()[2]) {
                MipavUtil.displayError("Cannot have (back + zPoint) >= image.getExtents()[2]");
                backInput.requestFocus();
                backInput.selectAll();

                return false;
            }

        } // if (image.getNDims() >= 3)

        xBounds[0] = xPoint - leftSide;
        xBounds[1] = xPoint + rightSide;
        yBounds[0] = yPoint - topSide;
        yBounds[1] = yPoint + bottomSide;

        if (image.getNDims() >= 3) {
            zBounds[0] = zPoint - front;
            zBounds[1] = zPoint + back;
        }

        if (newImage.isSelected()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
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
                return new String("Crops image around a certain point.");
            }

            public String getDescriptionLong() {
                return new String("Crops image around a certain point.");
            }

            public String getShortLabel() {
                return new String("CropAroundPoint");
            }

            public String getLabel() {
                return new String("Crop around Point");
            }

            public String getName() {
                return new String("Crop around Point");
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
            table.put(new ParameterInt("x_point", 0));
            table.put(new ParameterInt("y_point", 0));
            table.put(new ParameterInt("z_point", 0));
            table.put(new ParameterInt("right_side", 0));
            table.put(new ParameterInt("left_side", 0));
            table.put(new ParameterInt("top_side", 0));            
            table.put(new ParameterInt("bottom_side", 0));
            table.put(new ParameterInt("front", 0));
            table.put(new ParameterInt("back", 0));
            
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
