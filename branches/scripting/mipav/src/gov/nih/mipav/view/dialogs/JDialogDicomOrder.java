package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to set the image to Dicom order.** replaces image
 *
 * @version  1.0 July 17, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogDicomOrder extends JDialogBase implements AlgorithmInterface/*, ScriptableInterface*/ {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5868649690570274188L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxOrientX, comboBoxOrientY, comboBoxOrientZ;

    /** DOCUMENT ME! */
    private AlgorithmDicomOrder dicomOrderAlgo;

    /** DOCUMENT ME! */
    private boolean doClose = true;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private ViewJFrameImage imageFrame = null;

    /** DOCUMENT ME! */
    private int[] orient;

    /** DOCUMENT ME! */
    private JPanel orientPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogDicomOrder() { }

    /**
     * Constructor.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogDicomOrder(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        userInterface = ViewUserInterface.getReference();
        setForeground(Color.black);
        image = im;
        orient = image.getFileInfo()[0].getAxisOrientation();

        if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            constructDialog();
        }
    }

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present. This constructor is used by the script parser because it doesn't have the parent frame.
     *
     * @param  ui      User interface.
     * @param  im      Source image.
     * @param  orient  axes orientations
     */
    public JDialogDicomOrder(ViewUserInterface ui, ModelImage im, int[] orient) {
        super(false);

        parentFrame = im.getParentFrame();
        setForeground(Color.black);
        image = im;
        this.userInterface = ui;
        this.orient = orient;
        doClose = false;
        userInterface.regFrame(image.getParentFrame());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        boolean success;
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (source == OKButton) {
            success = getOrient();

            if (success) {
                callAlgorithm();
            }
        } else if (source == cancelButton) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10072");
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

        if (algorithm instanceof AlgorithmDicomOrder) {

            if (algorithm.isCompleted() == true) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                resultImage = dicomOrderAlgo.returnImage();

                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                Point pt;

                if (parentFrame != null) {
                    pt = ((ViewJFrameBase) parentFrame).getLocation();
                } else {
                    pt = new Point(Toolkit.getDefaultToolkit().getScreenSize().width / 2,
                                   Toolkit.getDefaultToolkit().getScreenSize().height / 2);
                }

                imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(pt.x, pt.y));

                //insertScriptLine(algorithm);

                if (doClose && (parentFrame != null)) {
                    ((ViewJFrameBase) parentFrame).close();
                }

                // Not so sure about this.
                if (image.getLightBoxFrame() != null) {
                    try {
                        pt = image.getLightBoxFrame().getLocation();
                        image.getLightBoxFrame().close();
                        new ViewJFrameLightBox(imageFrame, "LightBox", resultImage,
                                                               imageFrame.getComponentImage().getLUTa(),
                                                               imageFrame.getComponentImage().getImageB(),
                                                               imageFrame.getComponentImage().getLUTb(),
                                                               imageFrame.getComponentImage().getResolutionX(),
                                                               imageFrame.getComponentImage().getResolutionY(),
                                                               new Dimension(pt.x, pt.y), imageFrame.getControls());
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                }

            } else {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            }
        }

        dicomOrderAlgo.finalize();
        dicomOrderAlgo = null;
        dispose();
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        if ((orient[0] == FileInfoBase.ORI_R2L_TYPE) && (orient[1] == FileInfoBase.ORI_A2P_TYPE) &&
                (orient[2] == FileInfoBase.ORI_I2S_TYPE)) {
            MipavUtil.displayWarning("Image is already dicom ordered");

            return;
        }

        try {
            System.gc();

            // Make algorithm
            dicomOrderAlgo = new AlgorithmDicomOrder(image, orient);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            dicomOrderAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            // These next lines set the titles in all frames where the source image is displayed to
            // "locked - " image name so as to indicate that the image is now read/write locked!
            // The image frames are disabled and then unregisted from the userinterface until the
            // algorithm has completed.

            Vector imageFrames = image.getImageFrameVector();
            titles = new String[imageFrames.size()];

            for (int i = 0; i < imageFrames.size(); i++) {
                titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
            }

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (dicomOrderAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    dicomOrderAlgo.setProgressBarVisible(false);
                }

                dicomOrderAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog DicomOrder: unable to allocate enough memory");

            return;
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void constructDialog() {
        setTitle("Dicom Ordering");

        JLabel labelOrientX = new JLabel("X axis:");
        labelOrientX.setForeground(Color.black);
        labelOrientX.setFont(serif12);

        comboBoxOrientX = new JComboBox();
        comboBoxOrientX.setFont(serif12);
        comboBoxOrientX.setBackground(Color.white);

        comboBoxOrientX.addItem("Right to Left");
        comboBoxOrientX.addItem("Left to right");
        comboBoxOrientX.addItem("Anterior to posterior");
        comboBoxOrientX.addItem("Posterior to anterior");
        comboBoxOrientX.addItem("Inferior to superior");
        comboBoxOrientX.addItem("Superior to inferior");

        JLabel labelOrientY = new JLabel("Y axis:");
        labelOrientY.setForeground(Color.black);
        labelOrientY.setFont(serif12);

        comboBoxOrientY = new JComboBox();
        comboBoxOrientY.setFont(serif12);
        comboBoxOrientY.setBackground(Color.white);

        comboBoxOrientY.addItem("Right to Left");
        comboBoxOrientY.addItem("Left to right");
        comboBoxOrientY.addItem("Anterior to posterior");
        comboBoxOrientY.addItem("Posterior to anterior");
        comboBoxOrientY.addItem("Inferior to superior");
        comboBoxOrientY.addItem("Superior to inferior");

        JLabel labelOrientZ = new JLabel("Z axis:");
        labelOrientZ.setForeground(Color.black);
        labelOrientZ.setFont(serif12);

        comboBoxOrientZ = new JComboBox();
        comboBoxOrientZ.setFont(serif12);
        comboBoxOrientZ.setBackground(Color.white);

        comboBoxOrientZ.addItem("Right to Left");
        comboBoxOrientZ.addItem("Left to right");
        comboBoxOrientZ.addItem("Anterior to posterior");
        comboBoxOrientZ.addItem("Posterior to anterior");
        comboBoxOrientZ.addItem("Inferior to superior");
        comboBoxOrientZ.addItem("Superior to inferior");

        orientPanel = new JPanel(new GridBagLayout());
        orientPanel.setForeground(Color.black);
        orientPanel.setBorder(buildTitledBorder("Describe input orientation"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        orientPanel.add(labelOrientX, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        orientPanel.add(comboBoxOrientX, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        orientPanel.add(labelOrientY, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        orientPanel.add(comboBoxOrientY, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        orientPanel.add(labelOrientZ, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        orientPanel.add(comboBoxOrientZ, gbc);

        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buildCancelButton(); buttonPanel.add(OKButton); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());
        getContentPane().add(orientPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
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
     *
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("DicomOrder " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");
                userInterface.getScriptDialog().putVar(resultImage.getImageName());
                userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                       " " + orient[0] + " " + orient[1] + " " + orient[2] + "\n");
            }
        }
    }*/

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     *
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        setModal(false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        setForeground(Color.black);
        doClose = false;
        userInterface.regFrame(image.getParentFrame());

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
            orient[0] = parser.getNextInteger();
            orient[1] = parser.getNextInteger();
            orient[2] = parser.getNextInteger();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setSeparateThread(false);
        callAlgorithm();

        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }*/

    /**
     * Sets up the axis orientations based on what the user entered in the combo boxes.
     *
     * @return  <code>true</code> if the selections make sense (that is, are consistent with each other); otherwise
     *          <code>false</code>.
     */
    private boolean getOrient() {
        boolean success = true;
        int i;

        /* Obtain the input axes orientations */
        i = comboBoxOrientX.getSelectedIndex();

        switch (i) {

            case 0:
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 1:
                orient[0] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 2:
                orient[0] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case 3:
                orient[0] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 4:
                orient[0] = FileInfoBase.ORI_I2S_TYPE;
                break;

            case 5:
                orient[0] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        i = comboBoxOrientY.getSelectedIndex();

        switch (i) {

            case 0:
                orient[1] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 1:
                orient[1] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 2:
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case 3:
                orient[1] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 4:
                orient[1] = FileInfoBase.ORI_I2S_TYPE;
                break;

            case 5:
                orient[1] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        i = comboBoxOrientZ.getSelectedIndex();

        switch (i) {

            case 0:
                orient[2] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 1:
                orient[2] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 2:
                orient[2] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case 3:
                orient[2] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 4:
                orient[2] = FileInfoBase.ORI_I2S_TYPE;
                break;

            case 5:
                orient[2] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        // check that all directions are accounted for - one patient x-axis, one patient y-axis, and one patient z-axis
        if (!((orient[0] == FileInfoBase.ORI_R2L_TYPE) || (orient[0] == FileInfoBase.ORI_L2R_TYPE) ||
                  (orient[1] == FileInfoBase.ORI_R2L_TYPE) || (orient[1] == FileInfoBase.ORI_L2R_TYPE) ||
                  (orient[2] == FileInfoBase.ORI_R2L_TYPE) || (orient[2] == FileInfoBase.ORI_L2R_TYPE))) {
            success = false;
        }

        if (!((orient[0] == FileInfoBase.ORI_P2A_TYPE) || (orient[0] == FileInfoBase.ORI_A2P_TYPE) ||
                  (orient[1] == FileInfoBase.ORI_P2A_TYPE) || (orient[1] == FileInfoBase.ORI_A2P_TYPE) ||
                  (orient[2] == FileInfoBase.ORI_P2A_TYPE) || (orient[2] == FileInfoBase.ORI_A2P_TYPE))) {
            success = false;
        }

        if (!((orient[0] == FileInfoBase.ORI_I2S_TYPE) || (orient[0] == FileInfoBase.ORI_S2I_TYPE) ||
                  (orient[1] == FileInfoBase.ORI_I2S_TYPE) || (orient[1] == FileInfoBase.ORI_S2I_TYPE) ||
                  (orient[2] == FileInfoBase.ORI_I2S_TYPE) || (orient[2] == FileInfoBase.ORI_S2I_TYPE))) {
            success = false;
        }

        if (!success) {
            MipavUtil.displayError("Illegal selections for axes orientations");
        }

        return success;

    }

}
