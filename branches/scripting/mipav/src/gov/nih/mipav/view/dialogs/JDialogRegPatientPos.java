package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call algorithmRegPatientPos.
 *
 * @version  0.1 May 19, 1999
 * @author   Delia McGarry
 */
public class JDialogRegPatientPos extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7190471812525279048L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private boolean doMatch;

    /** DOCUMENT ME! */
    private ModelImage imageA;

    /** DOCUMENT ME! */
    private ModelImage imageB; // register imageB to imageA

    /** DOCUMENT ME! */
    private JCheckBox matchOrigBox;

    /** DOCUMENT ME! */
    private AlgorithmRegPatientPos RegPatPos = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegPatientPos() { }

    /**
     * Creates new dialog for registration.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogRegPatientPos(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);
        imageB = im;
        UI = imageB.getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogRegPatientPos(ViewUserInterface UI, ModelImage im) {
        super();
        this.UI = UI;
        imageB = im;
        parentFrame = im.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (source == OKButton) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == cancelButton) {
            dispose();
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

        if (algorithm instanceof AlgorithmRegPatientPos) {

            if (RegPatPos.isCompleted() == true) {
                resultImage = RegPatPos.getResultImage();

                if (resultImage != null) {
                    String name = JDialogBase.makeImageName(imageB.getImageName(), "_aligned");
                    resultImage.setImageName(name);
                    resultImage.calcMinMax();

                    // The algorithm has completed and produced a new image to be displayed.
                    try {
                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                }
            } else if (RegPatPos.getResultImage() != null) {

                // algorithm failed but result image still has garbage
                RegPatPos.getResultImage().disposeLocal(); // clean up memory
            }

            insertScriptLine(algorithm);
        }

        if (RegPatPos != null) {
            RegPatPos.finalize();
            RegPatPos = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the result image.
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

            if (UI.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(imageB.getImageName()) == null) {

                    if (UI.getScriptDialog().getActiveImgTableVar(imageB.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(imageB.getImageName());
                    }
                }

                // check to see if the match image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(imageA.getImageName()) == null) {

                    if (UI.getScriptDialog().getActiveImgTableVar(imageA.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(imageA.getImageName());
                    }
                }

                UI.getScriptDialog().append("RegPatientPos " + UI.getScriptDialog().getVar(imageB.getImageName()) +
                                            " " + UI.getScriptDialog().getVar(imageA.getImageName()) + " ");
                UI.getScriptDialog().putVar(resultImage.getImageName());
                UI.getScriptDialog().append(UI.getScriptDialog().getVar(resultImage.getImageName()) + " " + doMatch +
                                            "\n");
            }
        }
    }

    /**
     * For scripting, dialog won't show but will set up the variables.
     *
     * @param   parser  image to register
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
    /*public JDialogRegPatientPos(ModelImage imA, ModelImage imB, boolean match) {
     *  super(); imageA = imA; imageB = imB; doMatch = match; if (imageA.getNDims() != 3 ) {
     * MipavUtil.displayError("This algorithm only works for 3D datasets.");     return; }}*/

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String imageAKey = null;
        String imageBKey = null;
        String destImageKey = null;

        try {
            imageBKey = parser.getNextString();
            imageAKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage imB = parser.getImage(imageBKey);
        ModelImage imA = parser.getImage(imageAKey);

        imageA = imA;
        imageB = imB;
        UI = imageB.getUserInterface();
        parentFrame = imageB.getParentFrame();

        if (imageB.getNDims() != 3) {
            MipavUtil.displayError("This algorithm only works for 3D datasets.");

            return;
        }

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
            setMatchFlag(parser.getNextBoolean());
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setSeparateThread(false);
        callAlgorithm();
        parser.putVariable(destImageKey, getResultImage().getImageName());
    }

    /**
     * Accessor to set imageA.
     *
     * @param  imageA  DOCUMENT ME!
     */
    public void setImageA(ModelImage imageA) {
        this.imageA = imageA;
    }

    /**
     * Accessor that sets the matchOrigin flag.
     *
     * @param  flag  <code>true</code> indicates that origins should be matched.
     */
    public void setMatchFlag(boolean flag) {
        doMatch = flag;
    }

    /**
     * Calls the algorithm with the set-up parameters.
     */
    protected void callAlgorithm() {

        try {
            RegPatPos = new AlgorithmRegPatientPos(imageA, imageB, doMatch);
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Register PatientPos: unable to allocate enough memory");

            return;
        }

        // Hide dialog
        setVisible(false);

        // Start the thread as a low priority because we wish to still have user interface work fast.
        RegPatPos.addListener(this);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (RegPatPos.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            if (!UI.isAppFrameVisible()) {
                RegPatPos.setProgressBarVisible(false);
            }

            RegPatPos.run();
        }

    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Register Patient Position");

        String matchName = imageB.getImageName();

        JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        comboBoxImage = buildImageComboBox(imageB);
        matchOrigBox = new JCheckBox("Match image origins");

        JPanel imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        imagePanel.add(labelImage);
        imagePanel.add(comboBoxImage);
        imagePanel.setBorder(buildTitledBorder("Options"));

        JPanel optionPanel = new JPanel();
        optionPanel.setLayout(new BoxLayout(optionPanel, BoxLayout.Y_AXIS));
        optionPanel.add(matchOrigBox);
        matchOrigBox.setSelected(true);
        matchOrigBox.setEnabled(true);
        matchOrigBox.addItemListener(this);

        buildOKButton();
        buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(optionPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
     *
     * @return  <code>true</code> if the variables are properly set, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        // assign imageA to image selected in comboBox
        String selectedName = (String) comboBoxImage.getSelectedItem();
        imageA = UI.getRegisteredImageByName(selectedName);

        if (imageA.getNDims() != 3) {
            MipavUtil.displayError("This algorithm only works for 3D datasets.");

            return false;
        }

        doMatch = matchOrigBox.isSelected();

        return true;
    }

}
