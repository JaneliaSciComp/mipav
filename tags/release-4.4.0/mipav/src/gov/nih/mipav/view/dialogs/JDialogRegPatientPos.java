package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

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
public class JDialogRegPatientPos extends JDialogScriptableBase implements AlgorithmInterface {

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
        super(theParentFrame, false);
        imageB = im;
        UI = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

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

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
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

        createProgressBar(imageA.getImageName(), RegPatPos);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (RegPatPos.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            RegPatPos.run();
        }

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
        imageB = scriptParameters.retrieveInputImage();
        UI = ViewUserInterface.getReference();
        parentFrame = imageB.getParentFrame();

        if (imageB.getNDims() != 3) {
            MipavUtil.displayError("This algorithm only works for 3D datasets.");

            return;
        }

        imageA = scriptParameters.retrieveImage("reference_image");

        setMatchFlag(scriptParameters.getParams().getBoolean("do_match_origins"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(imageB);
        scriptParameters.storeImage(imageA, "reference_image");
        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_match_origins", doMatch));
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
        
        for (int i = 0; i < 3; i++) {
            if (imageA.getAxisOrientation()[i] == FileInfoBase.ORI_UNKNOWN_TYPE) {
                MipavUtil.displayError(imageA.getImageName() + " has an unknown orientation for axis = " + i);
                return false;
            }
            if (imageB.getAxisOrientation()[i] == FileInfoBase.ORI_UNKNOWN_TYPE) {
                MipavUtil.displayError(imageB.getImageName() + " has an unknown orientation for axis = " + i);
                return false;
            }
        }

        doMatch = matchOrigBox.isSelected();

        return true;
    }

}
