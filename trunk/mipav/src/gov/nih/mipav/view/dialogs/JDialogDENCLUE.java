package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. It
 * should be noted that the algorithms are executed in their own thread.
 */
public class JDialogDENCLUE extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private ButtonGroup influenceGroup;
    private JRadioButton gaussianButton;
    private boolean isGaussian;
    private JRadioButton squareButton;
    private JLabel distanceLabel;
    private JTextField distanceText;
    private float distance = 1.0f;
    private JLabel thresholdLabel;
    private JTextField thresholdText;
    private float threshold = 1.0f;
    private ButtonGroup clusterGroup;
    private JRadioButton arbitraryButton;
    private JRadioButton centerButton;
    private boolean isArbitrary;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private AlgorithmDENCLUE denAlgo = null;

    
    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogDENCLUE() { }

    /**
     * Creates a new JDialogDENCLUE object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDENCLUE(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogDENCLUE(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;

        image = im;
        parentFrame = image.getParentFrame();
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
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        }
        else if ((source == gaussianButton) || (source == squareButton)) {
            if (gaussianButton.isSelected()) {
                distanceLabel.setText("Standard deviation");    
            }
            else {
                distanceLabel.setText("Influence distance");    
            }
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

        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmDENCLUE) {
            image.clearMask();

            if ((denAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
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

        insertScriptLine(algorithm);

        denAlgo.finalize();
        denAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("DENCLUE " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                userInterface.getScriptDialog().putVar(resultImage.getImageName());
                userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                       " " + isGaussian + " " + distance + " " + threshold +
                                                       " " + isArbitrary + "\n");
                
            }
        }
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

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
            setIsGaussian(parser.getNextBoolean());
            setDistance(parser.getNextFloat());
            setThreshold(parser.getNextInteger());
            setIsArbitrary(parser.getNextBoolean());
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();

        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    /**
     * Accessor that sets whether the influence function is gaussian or square wave
     *
     * @param  isGaussian
     */
    public void setIsGaussian(boolean isGaussian) {
        this.isGaussian = isGaussian;
    }

    /**
     * Accessor that sets the distance
     * @param distance
     */
    public void setDistance(float distance) {
        this.distance = distance;
    }

    /**
     * Accessor that sets the threshold
     * @param threshold
     */
    public void setThreshold(float threshold) {
        this.threshold = threshold;
    }

    /**
     * Accessor that sets the wheteher the cluster shape is arbitrary or center defined.
     *
     * @param  isArbitrary
     */
    public void setIsArbitrary(boolean isArbitrary) {
        this.isArbitrary = isArbitrary;
    }

    
    /**
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_DENCLUE");

        

        int[] destExtents = new int[2];
        destExtents[0] = image.getExtents()[0]; // X dim
        destExtents[1] = image.getExtents()[1]; // Y dim


        try {

            // Make result image of float type
            resultImage     = new ModelImage(ModelStorageBase.FLOAT, destExtents, name, userInterface);

            // Make algorithm
            denAlgo = new AlgorithmDENCLUE(resultImage, image, isGaussian, distance,
                                        threshold, isArbitrary);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            denAlgo.addListener(this);
            setVisible(false); // Hide dialog

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (denAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                denAlgo.setActiveImage(isActiveImage);

                if (!userInterface.isAppFrameVisible()) {
                    denAlgo.setProgressBarVisible(false);
                }

                denAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog DENCLUE: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }
            
        
    }

    

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Density based clustering"); 

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)
        

        JPanel paramPanel = new JPanel();
        paramPanel.setLayout(gbl);
        gbc.anchor = GridBagConstraints.WEST;
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Clustering parameters")); // set the border ... "Colour channel
                                                                             // Selection"

        influenceGroup = new ButtonGroup();
        gaussianButton = new JRadioButton("Gaussian influence function", true);
        gaussianButton.setFont(serif12);
        gaussianButton.setForeground(Color.black);
        gaussianButton.addActionListener(this);
        influenceGroup.add(gaussianButton);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        paramPanel.add(gaussianButton, gbc);
        
        squareButton = new JRadioButton("Square wave influence function", false);
        squareButton.setFont(serif12);
        squareButton.setForeground(Color.black);
        squareButton.addActionListener(this);
        influenceGroup.add(squareButton);
        gbc.gridy = 1;
        paramPanel.add(squareButton, gbc);
        
        distanceLabel = new JLabel("Standard deviation");
        distanceLabel.setForeground(Color.black);
        distanceLabel.setFont(serif12);
        gbc.gridy = 2;
        paramPanel.add(distanceLabel, gbc);
        
        distanceText = new JTextField(10);
        distanceText.setText("1.0");
        distanceText.setForeground(Color.BLACK);
        distanceText.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(distanceText, gbc);
        
        thresholdLabel = new JLabel("Threshold");
        thresholdLabel.setForeground(Color.black);
        thresholdLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(thresholdLabel, gbc);
        
        thresholdText = new JTextField(10);
        thresholdText.setText("1.0");
        thresholdText.setForeground(Color.BLACK);
        thresholdText.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(thresholdText, gbc);
        
        clusterGroup = new ButtonGroup();
        arbitraryButton = new JRadioButton("Arbitrary shape cluster", true);
        arbitraryButton.setFont(serif12);
        arbitraryButton.setForeground(Color.black);
        clusterGroup.add(arbitraryButton);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(arbitraryButton, gbc);
        
        centerButton = new JRadioButton("Center defined cluster", false);
        centerButton.setFont(serif12);
        centerButton.setForeground(Color.black);
        clusterGroup.add(centerButton);
        gbc.gridy = 5;
        paramPanel.add(centerButton, gbc);
        
        
        getContentPane().add(paramPanel, BorderLayout.CENTER); // put the setupBox into the dialog

        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        isGaussian = gaussianButton.isSelected();
        
        tmpStr = distanceText.getText();
        distance = Float.parseFloat(tmpStr);
        if (distance <= 0) {
            MipavUtil.displayError("Distance must be greater than zero");
            distanceText.requestFocus();
            distanceText.selectAll();
            return false;
        }
        
        tmpStr = thresholdText.getText();
        threshold = Float.parseFloat(tmpStr);
        
        isArbitrary = arbitraryButton.isSelected();

        return true;
    }
}
