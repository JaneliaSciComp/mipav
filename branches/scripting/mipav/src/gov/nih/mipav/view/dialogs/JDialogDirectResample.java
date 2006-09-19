package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubsample;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;
import gov.nih.mipav.view.components.JPanelColorChannels;
import gov.nih.mipav.view.components.JPanelSigmas;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to ask user to resample the images or not.
 *
 * @author  Ruida Cheng
 */
public class JDialogDirectResample extends JDialogScriptableBase implements AlgorithmInterface{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2885627426314170051L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Number of available dimension. */
    int dim;

    /** Boolean flag to enable volume render button. */
    boolean enableVolRender = true;

    /** Original dimensions extents value array. */
    int[] extents;

    /** Original X, Y, Z dimension extents values. */
    JTextField extXInput, extYInput, extZInput;

    /** Power of 2 X, Y, Z dimension extents values. */
    JTextField extXOutput, extYOutput, extZOutput;

    /** Boolean flag to do resample images. */
    boolean forceResample = false;

    /** Model images A and B. */
    ModelImage image, imageB;

    /** Left panel and right panels corresponding to original and expected extents. */
    JPanel leftPanel, rightPanel;

    /** Resample resolutioin corresponding to Power of 2. */
    float[] newRes = new float[3];

    /** Boolean flag to indicate the original image is in Power of 2. */
    boolean originalVolPowerOfTwo = true;

    /** Original resolutioin arrray. */
    float[] res;

    /** Temp Model image. */
    ModelImage resultImage = null;
    
    ModelImage resultImageB = null;

    /** Parent ui. */
    ViewUserInterface userInterface;

    /** Resampled dimension value in Power of 2. */
    int[] volExtents = new int[3];

    /** Volume size X*Y*Z. */
    int volSize = 1;

    ViewJFrameImage resampledImageFrame = null;
    
    /** The algorithm */
    AlgorithmTransform algoTransform;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates the dialog, using the input parameters to place it on the screen.
     *
     * @param  _imageA               Model image A.
     * @param  _imageB               Model image B.
     * @param  _userInterface        Parent ui.
     * @param  _componentImage       Dicom converted to Java image.
     * @param  _resampledImageFrame  Parent image frame ViewJFrameImage.
     */
    public JDialogDirectResample(ModelImage _imageA, ModelImage _imageB, ViewUserInterface _userInterface) {
        super(_userInterface.getMainFrame(), false);
        this.image = _imageA;
        this.imageB = _imageB;
        this.userInterface = _userInterface;
        extents = image.getExtents();
        res = image.getFileInfo(0).getResolutions();
        this.dim = extents.length;

        for (int i = 0; i < extents.length; i++) {
            volExtents[i] = dimPowerOfTwo(extents[i]);
            volSize *= volExtents[i];

            if (volExtents[i] != extents[i]) {
                originalVolPowerOfTwo = false;
            }

            newRes[i] = (res[i] * (extents[i])) / (volExtents[i]);
        }

        init();
    }

    /**
     * Empty Contructor for script running
     *
     */
    public JDialogDirectResample() { }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * On "OK", sets the name variable to the text entered. On "Cancel" disposes of this dialog and sets cancel flag.
     *
     * @param  event  Event that triggered this method.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Resample")) {
            volExtents[0] = dimPowerOfTwo(Integer.parseInt(extXOutput.getText()));
            newRes[0] = (float) ((extents[0]) * res[0]) / (float) (volExtents[0]);
            volExtents[1] = dimPowerOfTwo(Integer.parseInt(extYOutput.getText()));
            newRes[1] = (float) ((extents[1]) * res[1]) / (float) (volExtents[1]);

            if (dim >= 3) {
                volExtents[2] = dimPowerOfTwo(Integer.parseInt(extZOutput.getText()));
                newRes[2] = (float) ((extents[2]) * res[2]) / (float) (volExtents[2]);
            }

            if (dim >= 3) {

                if ((extents[0] == volExtents[0]) && (extents[1] == volExtents[1]) && (extents[2] == volExtents[2])) {
                    forceResample = false;
                } else {
                    forceResample = true;
                }
            } else {

                if ((extents[0] == volExtents[0]) && (extents[1] == volExtents[1])) {
                    forceResample = false;
                } else {
                    forceResample = true;
                }
            }

            callAlgorithm();
            dispose();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("xChanged")) {
            int x = Integer.parseInt(extXOutput.getText());
            x = dimPowerOfTwo(x);
            extXOutput.setText(Integer.toString(x));
        } else if (command.equals("yChanged")) {
            int y = Integer.parseInt(extYOutput.getText());
            y = dimPowerOfTwo(y);
            extYOutput.setText(Integer.toString(y));
        } else if (command.equals("zChanged")) {
            int z = Integer.parseInt(extZOutput.getText());
            z = dimPowerOfTwo(z);
            extZOutput.setText(Integer.toString(z));
        }
    }

    /**
     * Dispose memory.
     *
     * @param  flag  DOCUMENT ME!
     */
    public void dispose(boolean flag) {

        /*
         * if (componentImage != null) { componentImage.setBuffers(null, null, null, null, null);
         * componentImage.setImageA(null); componentImage.setImageB(null); componentImage.dispose(false);
         * componentImage.disposeLocal(); componentImage = null; }
         *
         * if (resampledImageFrame != null) { resampledImageFrame.dispose(); resampledImageFrame = null; }
         *
         * if (resImageA != null) { resImageA.disposeLocal(); resImageA = null; }
         *
         * if (resImageB != null) { resImageB.disposeLocal(); resImageA = null; }
         *
         * if (resampledImage != null) { resampledImage.disposeLocal(); resampledImage = null; }
         */
        extents = null;
        res = null;
        volExtents = null;
        newRes = null;

        super.dispose();
    }


    /**
     * Resample images to power of 2.
     */
    public void callAlgorithm() {

        if (forceResample && resultImage == null) {
            // resample image
            if (dim >= 3) {
                algoTransform = new AlgorithmTransform(image, new TransMatrix(4), AlgorithmTransform.TRILINEAR,
                                                      newRes[0], newRes[1], newRes[2], volExtents[0], volExtents[1],
                                                      volExtents[2], false, true, false);
            } else {
                algoTransform = new AlgorithmTransform(image, new TransMatrix(4), AlgorithmTransform.BILINEAR,
                                                      newRes[0], newRes[1], volExtents[0], volExtents[1], false, true,
                                                      false);
            }
            algoTransform.addListener(this);

            createProgressBar(image.getImageName(), algoTransform);
            
            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoTransform.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
               
                algoTransform.run();
            }
            return;
        }

        // resample imageB
        if ((imageB != null) && forceResample) {
            // Resample image into volume that is a power of two !
            Preferences.debug("ViewJFrameSurfaceRenderer.buildTexture: Volume resampled.");

            if (dim >= 3) {
                algoTransform = new AlgorithmTransform(imageB, new TransMatrix(4), AlgorithmTransform.TRILINEAR,
                                                      newRes[0], newRes[1], newRes[2], volExtents[0], volExtents[1],
                                                      volExtents[2], false, true, false);
            } else {
                algoTransform = new AlgorithmTransform(imageB, new TransMatrix(4),

                                                      // AlgorithmTransform.CUBIC_LAGRANGIAN,
                                                      AlgorithmTransform.BILINEAR, newRes[0], newRes[1], volExtents[0],
                                                      volExtents[1], false, true, false);

            }
            algoTransform.addListener(this);

            createProgressBar(image.getImageName(), algoTransform);
            
            if (isRunInSeparateThread()) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoTransform.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoTransform.run();
            }
        }
    }

    /**
     * Algorithm notifies dialog of status
     */
    public void algorithmPerformed(AlgorithmBase algo) {
        if (algo instanceof AlgorithmTransform) {
            if (algoTransform.isCompleted()) {

                
                if (resultImage == null) {
                    resultImage = algoTransform.getTransformedImage();
                    resultImage.calcMinMax();

                    
                    algoTransform.disposeLocal();
                    algoTransform = null;
                    
                    if (imageB != null) {
                        callAlgorithm();
                        return;
                    } else {
                        resampledImageFrame = new ViewJFrameImage(resultImage, null, new Dimension(200, 200));
                        insertScriptLine();
                    }
                    return;
                } else if (imageB != null) {
                    resultImageB = algoTransform.getTransformedImage();
                    resultImageB.calcMinMax();
                    
                    algoTransform.disposeLocal();
                    algoTransform = null;
                    
                    resampledImageFrame = new ViewJFrameImage(resultImage, null, new Dimension(200, 200));
                    resampledImageFrame.setImageB(resultImageB);
                    resampledImageFrame.setControls();
                    resampledImageFrame.setTitle();
                    insertScriptLine();
                }   
            }
        }
    }
    
    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
        if (resultImageB != null) {
            AlgorithmParameters.storeImageInRunner(resultImageB);
        }
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        
        image = scriptParameters.retrieveInputImage();
       
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        imageB = ((ViewJFrameImage)parentFrame).getImageB(); // can be null      
        
        extents = image.getExtents();
        res = image.getFileInfo(0).getResolutions();
        dim = extents.length;

        for (int i = 0; i < extents.length; i++) {
            volExtents[i] = dimPowerOfTwo(extents[i]);
            volSize *= volExtents[i];

            newRes[i] = (res[i] * (extents[i])) / (volExtents[i]);
        }

        this.originalVolPowerOfTwo = false;  //force this to act regardless (script expects a result image)
        forceResample = true;
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     * 
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        
        scriptParameters.storeOutputImageParams(resultImage, true);
        if (resultImageB != null) {
            scriptParameters.storeOutputImageParams(resultImageB, true);
        }
        
    }
    
    /**
     * Build the resample dialog.
     */
    public void init() {
        setTitle("Resample Dialog");

        Box mainBox = new Box(BoxLayout.Y_AXIS);

        /*
         * JPanel msgPanel = new JPanel(); msgPanel.setLayout(new BorderLayout());
         * msgPanel.setBorder(buildTitledBorder("")); msgPanel.add(new JLabel("Do you want to resample the images?"),
         * BorderLayout.NORTH); msgPanel.add(new JLabel("You can modify the extents to Power of 2."),
         * BorderLayout.CENTER);
         */
        JPanel endPanel = new JPanel();
        endPanel.setLayout(new BorderLayout());
        // endPanel.add(new JLabel("Selecting Resample will resample the extents to Power of 2."), BorderLayout.NORTH);
        // endPanel.add(new JLabel("Cancel will exit the resample dialog."), BorderLayout.CENTER);

        // msgPanel.add(endPanel, BorderLayout.SOUTH);

        // mainBox.add(msgPanel);
        mainBox.add(endPanel);

        Box contentBox = new Box(BoxLayout.X_AXIS);
        JPanel leftPanel = new JPanel();
        JPanel rightPanel = new JPanel();

        // make border
        leftPanel.setBorder(buildTitledBorder("Original Extents"));
        contentBox.add(leftPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        leftPanel.setLayout(gbl);

        // extent X
        leftPanel.add(Box.createHorizontalStrut(10));

        JLabel extXLabel = new JLabel("extent X:");
        extXLabel.setFont(serif12);
        extXLabel.setForeground(Color.black);
        extXLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extXLabel, gbc);
        leftPanel.add(extXLabel);
        leftPanel.add(Box.createHorizontalStrut(10));

        extXInput = new JTextField(Integer.toString(extents[0]), 3);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extXInput, gbc);
        extXInput.setEnabled(false);
        leftPanel.add(extXInput);

        // extent Y
        leftPanel.add(Box.createHorizontalStrut(10));

        JLabel extYLabel = new JLabel("extent Y:");
        extYLabel.setFont(serif12);
        extYLabel.setForeground(Color.black);
        extYLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extYLabel, gbc);
        leftPanel.add(extYLabel);
        leftPanel.add(Box.createHorizontalStrut(10));

        extYInput = new JTextField(Integer.toString(extents[1]), 3);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extYInput, gbc);
        extYInput.setEnabled(false);
        leftPanel.add(extYInput);

        if (dim >= 3) {

            // extent Z
            leftPanel.add(Box.createHorizontalStrut(10));

            JLabel extZLabel = new JLabel("extent Z:");
            extZLabel.setFont(serif12);
            extZLabel.setForeground(Color.black);
            extZLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(extZLabel, gbc);
            leftPanel.add(extZLabel);
            leftPanel.add(Box.createHorizontalStrut(10));

            extZInput = new JTextField(Integer.toString(extents[2]), 3);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(extZInput, gbc);
            extZInput.setEnabled(false);
            leftPanel.add(extZInput);
        }
        // make border

        rightPanel.setBorder(buildTitledBorder("Expected Extents"));
        contentBox.add(rightPanel);

        // set layout
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        rightPanel.setLayout(gbl);

        // extent X expected
        rightPanel.add(Box.createHorizontalStrut(10));

        JLabel extXNewLabel = new JLabel("extent X:");
        extXNewLabel.setFont(serif12);
        extXNewLabel.setForeground(Color.black);
        extXNewLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extXNewLabel, gbc);
        rightPanel.add(extXNewLabel);
        rightPanel.add(Box.createHorizontalStrut(10));

        extXOutput = new JTextField(Integer.toString(volExtents[0]), 3);
        extXOutput.addActionListener(this);
        extXOutput.setActionCommand("xChanged");
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extXOutput, gbc);
        MipavUtil.makeNumericsOnly(extXOutput, false);
        rightPanel.add(extXOutput);

        // extent Y expected
        rightPanel.add(Box.createHorizontalStrut(10));

        JLabel extYNewLabel = new JLabel("extent Y:");
        extYNewLabel.setFont(serif12);
        extYNewLabel.setForeground(Color.black);
        extYNewLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extYNewLabel, gbc);
        rightPanel.add(extYNewLabel);
        rightPanel.add(Box.createHorizontalStrut(10));

        extYOutput = new JTextField(Integer.toString(volExtents[1]), 3);
        extYOutput.addActionListener(this);
        extYOutput.setActionCommand("yChanged");
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extYOutput, gbc);
        MipavUtil.makeNumericsOnly(extYOutput, false);
        rightPanel.add(extYOutput);

        if (dim >= 3) {

            // extent Z expected
            rightPanel.add(Box.createHorizontalStrut(10));

            JLabel extZNewLabel = new JLabel("extent Z:");
            extZNewLabel.setFont(serif12);
            extZNewLabel.setForeground(Color.black);
            extZNewLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(extZNewLabel, gbc);
            rightPanel.add(extZNewLabel);
            rightPanel.add(Box.createHorizontalStrut(10));

            extZOutput = new JTextField(Integer.toString(volExtents[2]), 3);
            extZOutput.addActionListener(this);
            extZOutput.setActionCommand("zChanged");
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(extZOutput, gbc);
            MipavUtil.makeNumericsOnly(extZOutput, false);
            rightPanel.add(extZOutput);
        }

        mainBox.add(contentBox);

        JPanel OKCancelPanel = new JPanel(new FlowLayout());

        // OKButton = buildOKButton();
        OKButton = buildResampleButton();
        OKCancelPanel.add(OKButton);

        cancelButton = buildCancelButton();
        OKCancelPanel.add(cancelButton);
        mainBox.add(OKCancelPanel);

        getContentPane().add(mainBox);

        pack();
        setVisible(true);

    }

    /**
     * DOCUMENT ME!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        dispose(true);
        super.finalize();
    }

    /**
     * Builds the OK button. Sets it internally as well return the just-built button.
     *
     * @return  DOCUMENT ME!
     */
    private JButton buildResampleButton() {
        OKButton = new JButton("Resample");
        OKButton.addActionListener(this);
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);

        return OKButton;
    }

    /**
     * Calculate the dimension value to power of 2.
     *
     * @param   dim  dimension value
     *
     * @return  value dimension value in power of 2.
     */
    private int dimPowerOfTwo(int dim) {

        // 128^3 x 4 is 8MB
        // 256^3 x 4 is 64MB
        if (dim <= 16) {
            return 16;
        } else if (dim <= 32) {
            return 32;
        } else if (dim <= 64) {

            if (dim > 40) {
                return 64;
            } else {
                return 32;
            }
        } else if (dim <= 128) {

            if (dim > 80) {
                return 128;
            } else {
                return 64;
            }
        } else if (dim <= 256) {

            if (dim > 160) {
                return 256;
            } else {
                return 128;
            }
        } else if (dim <= 512) {

            if (dim > 448) {
                return 512;
            } else {
                return 256;
            }
        } else {
            return 512;
        }
    }

}
