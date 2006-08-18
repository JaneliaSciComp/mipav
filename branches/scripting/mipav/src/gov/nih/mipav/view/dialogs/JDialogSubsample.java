package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog for subsampling a 2D or 3D or 4D image by 2, 4, or 8. With 4D images only the first 3 dimensions are
 * subsampled.
 *
 * @author   Sir Benjamin Link
 * @version  1.0
 */
public class JDialogSubsample extends JDialogScriptableBase implements AlgorithmInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3213495943646123969L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmSubsample algoSub = null;

    /** DOCUMENT ME! */
    private JRadioButton by2Button = null; // subsample by 2

    /** DOCUMENT ME! */
    private JRadioButton by4Button = null; // subsample by 4

    /** DOCUMENT ME! */
    private JRadioButton by8Button = null; // subsample by 8

    /** DOCUMENT ME! */
    private int denom = 2; // denominator for subsampling

    /** DOCUMENT ME! */
    private boolean doVOI = false;

    /** DOCUMENT ME! */
    private ModelImage image = null; // sourceImage

    /** DOCUMENT ME! */
    private boolean lockZ = false;

    /** DOCUMENT ME! */
    private JCheckBox lockZBox = null;

    /** DOCUMENT ME! */
    private int[] newExtents = null;

    /** DOCUMENT ME! */
    private float oXres, oYres, oZres;

    /** DOCUMENT ME! */
    private boolean processIndep = false; // use 2.5D?

    /** DOCUMENT ME! */
    private JCheckBox processIndepBox = null; // for processing 3D as 2.5D (slices remain unchanged)

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result Image

    /** DOCUMENT ME! */
    private float[] sigmas = null;

    /** DOCUMENT ME! */
    private float Sx, Sy, Sz;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JCheckBox voiCheckBox = null;

    /** DOCUMENT ME! */
    private TransMatrix xfrm = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSubsample() { }

    /**
     * Constructor for creating the dialog via a parent frame.
     *
     * @param  theParentFrame  the parent frame
     * @param  sourceImage     the source image
     */
    public JDialogSubsample(Frame theParentFrame, ModelImage sourceImage) {
        super(theParentFrame, false);
        this.image = sourceImage;
        this.userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Method for catching actions (button/script).
     *
     * @param  e  the action event
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10067");
        }
    }

    /**
     * Method for catching end of algorithm events.
     *
     * @param  algo  the algorithm that is caught
     */
    public void algorithmPerformed(AlgorithmBase algo) {

        if (algo instanceof AlgorithmSubsample) {

            if (algoSub.isCompleted()) {

                try {
                    new ViewJFrameImage(resultImage, null, userInterface.getNewFrameLocation());
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Subsample reports: out of memory; " + "unable to open a new frame");
                }

                insertScriptLine();
            }

            algoSub.finalize();
            algoSub = null;
        }
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
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), true);
        
        scriptParameters.getParams().put(ParameterFactory.newParameter(AlgorithmParameters.DO_PROCESS_3D_AS_25D, processIndep));
        scriptParameters.getParams().put(ParameterFactory.newParameter("subsample_factor", denom));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_transform_vois", doVOI));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_not_change_zdim", lockZ));
    }

    /**
     * Respond to checkbox item events.
     *
     * @param  event  item event
     */
    public void itemStateChanged(ItemEvent event) {

        if (event.getSource() == processIndepBox) {
            lockZBox.setSelected(processIndepBox.isSelected());
            lockZBox.setEnabled(!processIndepBox.isSelected());

            if (processIndepBox.isSelected()) {

                if (voiCheckBox != null) {
                    voiCheckBox.setEnabled(false);
                    voiCheckBox.setSelected(false);
                }
            } else {

                if (voiCheckBox != null) {
                    voiCheckBox.setEnabled(true);
                }
            }
        }
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        setProcessIndep(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        denom = scriptParameters.getParams().getInt("subsample_factor");
        setDoVOI(scriptParameters.getParams().getBoolean("do_transform_vois"));
        lockZ = scriptParameters.getParams().getBoolean("do_not_change_zdim");

        if (image.getNDims() == 2) {
            newExtents = new int[2];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;

            sigmas = new float[2];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
        } else if (image.getNDims() == 3) {
            newExtents = new int[3];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;

            if (lockZ) {
                newExtents[2] = image.getExtents()[2];
            } else {
                newExtents[2] = image.getExtents()[2] / denom;
            }

            sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * (image.getFileInfo(0).getResolutions()[0] / image.getFileInfo(0).getResolutions()[2]);
        } else if (image.getNDims() == 4) {
            newExtents = new int[4];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;
            newExtents[2] = image.getExtents()[2] / denom;
            newExtents[3] = image.getExtents()[3];

            sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * (image.getFileInfo(0).getResolutions()[0] / image.getFileInfo(0).getResolutions()[2]);
        }

        if (doVOI) {
            oXres = image.getFileInfo(0).getResolutions()[0] * denom;
            oYres = image.getFileInfo(0).getResolutions()[1] * denom;
            Sx = ((float) (newExtents[0]) * oXres) /
                     ((float) (image.getExtents()[0]) * image.getFileInfo(0).getResolutions()[0]);
            Sy = ((float) (newExtents[1]) * oYres) /
                     ((float) (image.getExtents()[1]) * image.getFileInfo(0).getResolutions()[1]);

            if (processIndep || (image.getNDims() == 2)) {
                xfrm = new TransMatrix(3);
                xfrm.identity();
                xfrm.setZoom(Sx, Sy);
            } else {

                if (lockZ) {
                    oZres = image.getFileInfo(0).getResolutions()[2];
                    Sz = 1.0f;
                } else {
                    oZres = image.getFileInfo(0).getResolutions()[2] * denom;
                    Sz = ((float) (newExtents[2]) * oZres) /
                             ((float) (image.getExtents()[2]) * image.getFileInfo(0).getResolutions()[2]);
                }

                xfrm = new TransMatrix(4);
                xfrm.identity();
                xfrm.setZoom(Sx, Sy, Sz);
            }
        } // if (doVOI)
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * Accessor that tells whether VOIs are transformed.
     *
     * @param  doVOI  boolean
     */
    public void setDoVOI(boolean doVOI) {
        this.doVOI = doVOI;
    }

    /**
     * Accessor that sets whether slices are processed independently.
     *
     * @param  processIndep  DOCUMENT ME!
     */
    public void setProcessIndep(boolean processIndep) {
        this.processIndep = processIndep;
    }

    /**
     * Method for calling the Subsample algorithm.
     */
    protected void callAlgorithm() {
        setVisible(false);

        // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
        resultImage = new ModelImage(image.getType(), newExtents, image.getImageName() + "_subsample_" + denom,
                                     userInterface);

        algoSub = new AlgorithmSubsample(image, resultImage, newExtents, sigmas, processIndep, doVOI, xfrm);

        algoSub.addListener(this);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (algoSub.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            if (!userInterface.isAppFrameVisible()) {
                algoSub.setProgressBarVisible(false);
            }

            algoSub.run();
        }
    }

    /**
     * Sets up the dialog window and makes it visible.
     */
    private void init() {

        setTitle("Subsample");

        ButtonGroup sampleSizeGroup = null;

        JPanel sampleSizePanel = new JPanel();

        sampleSizePanel.setLayout(new BoxLayout(sampleSizePanel, BoxLayout.Y_AXIS));
        sampleSizePanel.setBorder(buildTitledBorder("New Size"));
        sampleSizePanel.setForeground(Color.black);

        sampleSizeGroup = new ButtonGroup();

        by2Button = new JRadioButton("Subsample by 2", true);
        by2Button.setFont(serif12);
        sampleSizeGroup.add(by2Button);
        sampleSizePanel.add(by2Button);

        by4Button = new JRadioButton("Subsample by 4", false);
        by4Button.setFont(serif12);
        sampleSizeGroup.add(by4Button);
        sampleSizePanel.add(by4Button);

        by8Button = new JRadioButton("Subsample by 8", false);
        by8Button.setFont(serif12);
        sampleSizeGroup.add(by8Button);
        sampleSizePanel.add(by8Button);

        if ((image.getVOIs() != null) && (!image.getVOIs().isEmpty()) && (image.getNDims() <= 3)) {
            voiCheckBox = new JCheckBox("Transform VOIs");
            voiCheckBox.setFont(serif12);
            voiCheckBox.setSelected(false);
            sampleSizePanel.add(voiCheckBox);
        }

        if (image.getNDims() == 3) {
            processIndepBox = new JCheckBox("Process each slice independently (2.5D)", false);
            processIndepBox.setFont(serif12);
            processIndepBox.addItemListener(this);
            sampleSizePanel.add(processIndepBox);

            lockZBox = new JCheckBox("Leave Z dimension unchanged", false);
            lockZBox.setFont(serif12);
            lockZBox.addItemListener(this);
            sampleSizePanel.add(lockZBox);
        }

        JPanel buttonPanel = new JPanel(new FlowLayout());

        // Make & set the OK (remove) and Cancel buttons--place outside the border
        /*
         * buildOKButton(); OKButton.setText("OK"); OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
         * buttonPanel.add(OKButton);
         *
         * buildCancelButton(); cancelButton.setPreferredSize(MipavUtil.defaultButtonSize); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        JPanel panel = new JPanel(new BorderLayout());

        panel.add(sampleSizePanel); // put the main panel into the center of the dialog
        panel.add(buttonPanel, BorderLayout.SOUTH);
        panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(panel);
        pack();
        setResizable(false);
        setVisible(true);
    }

    /**
     * Sets the variables based on the user's dialog input.
     *
     * @return  was everything ok (unnecessary because there is no possible illegal user input)
     */
    private boolean setVariables() {

        if (by4Button.isSelected()) {
            denom = 4;
        } else if (by8Button.isSelected()) {
            denom = 8;
        }

        if (processIndepBox != null) {
            processIndep = processIndepBox.isSelected();
        }

        if (lockZBox != null) {
            lockZ = lockZBox.isSelected();
        }

        if (image.getNDims() == 2) {
            newExtents = new int[2];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;

            sigmas = new float[2];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
        } else if (image.getNDims() == 3) {
            newExtents = new int[3];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;

            if (lockZ) {
                newExtents[2] = image.getExtents()[2];
            } else {
                newExtents[2] = image.getExtents()[2] / denom;
            }

            sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * (image.getFileInfo(0).getResolutions()[0] / image.getFileInfo(0).getResolutions()[2]);
        } else if (image.getNDims() == 4) {
            newExtents = new int[4];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;
            newExtents[2] = image.getExtents()[2] / denom;
            newExtents[3] = image.getExtents()[3];
            sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * (image.getFileInfo(0).getResolutions()[0] / image.getFileInfo(0).getResolutions()[2]);
        }

        if (voiCheckBox != null) {
            doVOI = voiCheckBox.isSelected();
        } // if (voiCheckBox != null)

        if (doVOI) {
            oXres = image.getFileInfo(0).getResolutions()[0] * (float) (image.getExtents()[0]) /
                        (float) (newExtents[0]);
            oYres = image.getFileInfo(0).getResolutions()[1] * (float) (image.getExtents()[1]) /
                        (float) (newExtents[1]);

            // Sx = ( (float) (newExtents[0]) * oXres) /
            // ( (float) (image.getExtents()[0]) * image.getFileInfo(0).getResolutions()[0]);
            Sx = 1.0f;

            // Sy = ( (float) (newExtents[1]) * oYres) /
            // ( (float) (image.getExtents()[1]) * image.getFileInfo(0).getResolutions()[1]);
            Sy = 1.0f;

            if (processIndep || (image.getNDims() == 2)) {
                xfrm = new TransMatrix(3);
                xfrm.identity();
                xfrm.setZoom(Sx, Sy);
            } else {

                if (lockZ) {
                    oZres = image.getFileInfo(0).getResolutions()[2];
                    Sz = 1.0f;
                } else {
                    oZres = image.getFileInfo(0).getResolutions()[2] * (float) (image.getExtents()[2]) /
                                (float) (newExtents[2]);

                    // Sz = ( (float) (newExtents[2]) * oZres) /
                    // ( (float) (image.getExtents()[2]) * image.getFileInfo(0).getResolutions()[2]);
                    Sz = 1.0f;
                }

                xfrm = new TransMatrix(4);
                xfrm.identity();
                xfrm.setZoom(Sx, Sy, Sz);
            }
        } // if (doVOI)

        return true;
    }
}
