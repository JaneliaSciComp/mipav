package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 January 13, 2006
 * @author   William Gandler
 */
public class JDialogSkeletonize3D extends JDialogBase implements AlgorithmInterface, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4112815251500514340L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton chooserButton;

    /** DOCUMENT ME! */
    private int distCharges = 0;

    /** DOCUMENT ME! */
    private ButtonGroup fieldGroup;

    /** DOCUMENT ME! */
    private int fieldStrength = 5;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private JRadioButton lineButton;

    /** DOCUMENT ME! */
    private JRadioButton loadButton;

    /** DOCUMENT ME! */
    private float minObjectValue;

    /** DOCUMENT ME! */
    private JRadioButton noLoadSaveButton;

    /** DOCUMENT ME! */
    private ButtonGroup outputGroup;

    /** DOCUMENT ME! */
    private boolean outputPoints = true;

    /** DOCUMENT ME! */
    private float perHDPoints = 0.3f;

    /** DOCUMENT ME! */
    private JRadioButton pointButton;

    /** DOCUMENT ME! */
    private JButton removeButton;

    /** DOCUMENT ME! */
    private JRadioButton saveButton;

    /** DOCUMENT ME! */
    private boolean saveVF = true;

    /** DOCUMENT ME! */
    private AlgorithmSkeletonize3D skeletonize3DAlgo;

    /** DOCUMENT ME! */
    private int skelNumPoints;

    /** DOCUMENT ME! */
    private int skelNumSegments;

    /** DOCUMENT ME! */
    private double[][] skPoints;

    /** DOCUMENT ME! */
    private int[][] skSegments;

    /** DOCUMENT ME! */
    private boolean sliceHoleFilling = true;

    /** DOCUMENT ME! */
    private JCheckBox sliceHoleFillingCheckBox;

    /** DOCUMENT ME! */
    private JTextField textDistCharges;

    /** DOCUMENT ME! */
    private JTextField textDivergencePoints;

    /** DOCUMENT ME! */
    private JTextField textFieldStrength;

    /** DOCUMENT ME! */
    private JTextField textMinObjectValue;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private ModelImage[] vfImage = null;

    /** DOCUMENT ME! */
    private DefaultListModel vfModel;

    /** DOCUMENT ME! */
    private String vfName;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present. This constructor is used by the script parser because it doesn't have the parent frame.
     *
     * @param  im  Source image.
     */
    public JDialogSkeletonize3D(ModelImage im) {
        super();
        setForeground(Color.black);
        image = im;
        this.userInterface = ViewUserInterface.getReference();

        init();
    }

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogSkeletonize3D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        setForeground(Color.black);
        image = im;
        userInterface = ViewUserInterface.getReference();


        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Presently only the script function calls this method. When the script sends this dialog the action command, this
     * method calls run.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("Choose")) {
            vfImage = openVF();

            if (!checkImage(vfImage[0])) {
                return;
            }

            if (!checkImage(vfImage[1])) {
                return;
            }

            if (!checkImage(vfImage[2])) {
                return;
            }

            vfName = vfImage[0].getImageName();
            vfModel.addElement(vfName);
            removeButton.setEnabled(true);
            chooserButton.setEnabled(false);
        } // if (command.equals("Choose"))
        else if (command.equals("Remove")) {
            vfModel.removeElement(vfName);
            vfImage[0].disposeLocal();
            vfImage[1].disposeLocal();
            vfImage[2].disposeLocal();
            vfImage[0] = null;
            vfImage[1] = null;
            vfImage[2] = null;
            vfImage = null;
            removeButton.setEnabled(false);
            chooserButton.setEnabled(true);
        } // else if ((command.equals("Remove"))
        else if ((source == loadButton) || (source == saveButton) || (source == noLoadSaveButton)) {

            if (loadButton.isSelected()) {

                if (vfImage == null) {
                    chooserButton.setEnabled(true);
                    removeButton.setEnabled(false);
                } else {
                    chooserButton.setEnabled(false);
                    removeButton.setEnabled(true);
                }
            } else {
                chooserButton.setEnabled(false);
                removeButton.setEnabled(false);
            }
        } else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }

    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmSkeletonize3D) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
            }

            image.calcMinMax();
            image.notifyImageDisplayListeners(null, true);
        }
    }


    /**
     * Sets the remove index based on the selected index in the list.
     *
     * @param  evt  Event that caused this method to fire.
     */
    public void valueChanged(ListSelectionEvent evt) { }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {
        double[] xvf = null;
        double[] yvf = null;
        double[] zvf = null;
        int length;

        try {

            if (vfImage != null) {
                length = image.getSliceSize() * image.getExtents()[2];

                if (vfImage[0] != null) {
                    xvf = new double[length];

                    try {
                        vfImage[0].exportData(0, length, xvf);
                    } catch (IOException error) {
                        MipavUtil.displayError("Error on vfImage[0].expportData");

                        return;
                    }

                    vfImage[0].disposeLocal();
                    vfImage[0] = null;
                }

                if (vfImage[1] != null) {
                    yvf = new double[length];

                    try {
                        vfImage[1].exportData(0, length, yvf);
                    } catch (IOException error) {
                        MipavUtil.displayError("Error on vfImage[1].expportData");

                        return;
                    }

                    vfImage[1].disposeLocal();
                    vfImage[1] = null;
                }

                if (vfImage[2] != null) {
                    zvf = new double[length];

                    try {
                        vfImage[2].exportData(0, length, zvf);
                    } catch (IOException error) {
                        MipavUtil.displayError("Error on vfImage[2].exportData");

                        return;
                    }

                    vfImage[2].disposeLocal();
                    vfImage[2] = null;
                }

                vfImage = null;
            } // if (gvfImage != null)

            System.gc();

            // Make algorithm

            skeletonize3DAlgo = new AlgorithmSkeletonize3D(image, minObjectValue, sliceHoleFilling, saveVF, distCharges,
                                                           fieldStrength, perHDPoints, xvf, yvf, zvf, skPoints,
                                                           skSegments, outputPoints);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            skeletonize3DAlgo.addListener(this);

            createProgressBar(image.getImageName(), skeletonize3DAlgo);

            // Hide dialog
            setVisible(false);

            // These next lines set the titles in all frames where the source image is displayed to
            // "locked - " image name so as to indicate that the image is now read/write locked!
            // The image frames are disabled and then unregisted from the userinterface until the
            // algorithm has completed.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
            titles = new String[imageFrames.size()];

            for (int i = 0; i < imageFrames.size(); i++) {
                titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
            }

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (skeletonize3DAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                skeletonize3DAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Skeletonize 3D : unable to allocate enough memory");

            return;
        }
    }

    /**
     * Checks the dimensionality of the new image vs. the original source image. All new images should be of the same
     * dimensions.
     *
     * @param   testImage  DOCUMENT ME!
     *
     * @return  Flag indicating if the image checks out.
     */
    private boolean checkImage(ModelImage testImage) {

        if (testImage == null) {
            return false;
        }

        if (image.getNDims() != testImage.getNDims()) {
            MipavUtil.displayError("Error! " + image.getImageName() + " is " + image.getNDims() + "D, while " +
                                   testImage.getImageName() + " is " + testImage.getNDims() + "D");

            return false;
        }

        for (int i = 0; i < image.getNDims(); i++) {

            if ((testImage != null) && (image.getExtents()[i] != testImage.getExtents()[i])) {
                MipavUtil.displayError("Error! For dimension = " + i + " " + image.getImageName() + " has length = " +
                                       image.getExtents()[i] + " while " + testImage.getImageName() + " has length = " +
                                       testImage.getExtents()[i]);

                return false;
            }
        }

        return true;

    }

    /**
     * Makes the GUI elements of the dialog.
     */
    private void init() {
        setTitle("Skeletonize 3D");
        getContentPane().setLayout(new BorderLayout());

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 3;
        gbc.weightx = 1;

        JLabel labelMinObjectValue = new JLabel("Minimum value for outer pixel to be object");
        labelMinObjectValue.setForeground(Color.black);
        labelMinObjectValue.setFont(serif12);
        paramPanel.add(labelMinObjectValue, gbc);

        minObjectValue = (float) (image.getMin() + 1.0);
        textMinObjectValue = new JTextField(10);
        textMinObjectValue.setText(String.valueOf(minObjectValue));
        textMinObjectValue.setFont(serif12);
        textMinObjectValue.setForeground(Color.black);
        gbc.gridwidth = 1;
        gbc.gridx = 3;
        paramPanel.add(textMinObjectValue, gbc);

        sliceHoleFillingCheckBox = new JCheckBox("Slice by slice hole filling");
        sliceHoleFillingCheckBox.setFont(serif12);
        sliceHoleFillingCheckBox.setForeground(Color.black);
        sliceHoleFillingCheckBox.setSelected(true);
        gbc.gridwidth = 4;
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(sliceHoleFillingCheckBox, gbc);


        JLabel labelDistCharges = new JLabel("Distance of electrical charges from object boundary (-40 to 40)  ");
        labelDistCharges.setForeground(Color.black);
        labelDistCharges.setFont(serif12);
        gbc.gridwidth = 3;
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelDistCharges, gbc);

        textDistCharges = new JTextField(5);
        textDistCharges.setText("0");
        textDistCharges.setFont(serif12);
        textDistCharges.setForeground(Color.black);
        gbc.gridwidth = 1;
        gbc.gridx = 3;
        paramPanel.add(textDistCharges, gbc);

        JLabel labelFieldStrength = new JLabel("Field strength (4-9)");
        labelFieldStrength.setForeground(Color.black);
        labelFieldStrength.setFont(serif12);
        gbc.gridwidth = 3;
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(labelFieldStrength, gbc);

        textFieldStrength = new JTextField(5);
        textFieldStrength.setText("5");
        textFieldStrength.setFont(serif12);
        textFieldStrength.setForeground(Color.black);
        gbc.gridwidth = 1;
        gbc.gridx = 3;
        paramPanel.add(textFieldStrength, gbc);

        JLabel labelDivergencePoints = new JLabel("Fraction of divergence points to use (0.0-1.0)");
        labelDivergencePoints.setForeground(Color.black);
        labelDivergencePoints.setFont(serif12);
        gbc.gridwidth = 3;
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(labelDivergencePoints, gbc);

        textDivergencePoints = new JTextField(7);
        textDivergencePoints.setText("0.3");
        textDivergencePoints.setFont(serif12);
        textDivergencePoints.setForeground(Color.black);
        gbc.gridwidth = 1;
        gbc.gridx = 3;
        paramPanel.add(textDivergencePoints, gbc);

        fieldGroup = new ButtonGroup();
        saveButton = new JRadioButton("Save the vector field to files", true);
        saveButton.setFont(serif12);
        saveButton.setForeground(Color.black);
        saveButton.addActionListener(this);
        fieldGroup.add(saveButton);
        gbc.gridwidth = 4;
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramPanel.add(saveButton, gbc);

        loadButton = new JRadioButton("Load the vector field from files", false);
        loadButton.setFont(serif12);
        loadButton.setForeground(Color.black);
        loadButton.addActionListener(this);
        fieldGroup.add(loadButton);
        gbc.gridx = 0;
        gbc.gridy = 6;
        paramPanel.add(loadButton, gbc);

        JPanel vfPanel = new JPanel(new BorderLayout());
        vfPanel.setBorder(buildTitledBorder("Open XVF file"));

        vfModel = new DefaultListModel();

        JList vfList = new JList(vfModel);
        vfList.setVisibleRowCount(1);
        vfList.setPreferredSize(new Dimension(300, 30));
        vfList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        vfList.addListSelectionListener(this);
        vfPanel.add(vfList);

        JPanel chooserPanel = new JPanel();
        chooserButton = new JButton("Load");
        chooserButton.setPreferredSize(MipavUtil.defaultButtonSize);
        chooserButton.setFont(serif12B);
        chooserButton.setEnabled(false);
        chooserPanel.add(chooserButton);
        chooserButton.addActionListener(this);
        chooserButton.setActionCommand("Choose");

        removeButton = new JButton("Remove");
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeButton.setFont(serif12B);
        removeButton.setEnabled(false);
        chooserPanel.add(removeButton);
        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");

        vfPanel.add(chooserPanel, BorderLayout.SOUTH);
        gbc.gridy = 7;
        paramPanel.add(vfPanel, gbc);

        noLoadSaveButton = new JRadioButton("Don't load or save the vector field", false);
        noLoadSaveButton.setFont(serif12);
        noLoadSaveButton.setForeground(Color.black);
        noLoadSaveButton.addActionListener(this);
        fieldGroup.add(noLoadSaveButton);
        gbc.gridx = 0;
        gbc.gridy = 8;
        paramPanel.add(noLoadSaveButton, gbc);

        outputGroup = new ButtonGroup();
        pointButton = new JRadioButton("Output all skeleton points", true);
        pointButton.setFont(serif12);
        pointButton.setForeground(Color.black);
        outputGroup.add(pointButton);
        gbc.gridy = 9;
        paramPanel.add(pointButton, gbc);

        lineButton = new JRadioButton("Output only segment end points", false);
        lineButton.setFont(serif12);
        lineButton.setForeground(Color.black);
        outputGroup.add(lineButton);
        gbc.gridy = 10;
        paramPanel.add(lineButton, gbc);

        getContentPane().add(paramPanel);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);
    }


    /**
     * Open an image based on the suffix of the file.
     *
     * @return  The image.
     */
    private ModelImage[] openVF() {
        JFileChooser chooser = null;
        FileIO fileIO = null;
        boolean multiFile = false;
        String fileName;
        String directory;
        String fileSKF;
        RandomAccessFile raFile;
        int i, j;

        try {

            chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

            chooser.setDialogTitle("Open Image");

            int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return null;
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        try {
            fileIO = new FileIO();
            i = fileName.lastIndexOf('.');

            // Files are _xvf.xml, _yvf.xml, _zvf.xml, .skf
            String fileNameBase = fileName.substring(0, i - 4);
            String fileXVF = fileNameBase + "_xvf.xml";
            String fileYVF = fileNameBase + "_yvf.xml";
            String fileZVF = fileNameBase + "_zvf.xml";
            fileSKF = fileNameBase + ".skf";
            vfImage = new ModelImage[3];
            vfImage[0] = fileIO.readImage(fileXVF, directory, multiFile, null);
            vfImage[1] = fileIO.readImage(fileYVF, directory, multiFile, null);
            vfImage[2] = fileIO.readImage(fileZVF, directory, multiFile, null);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        try {
            File file = new File(directory + fileSKF);
            raFile = new RandomAccessFile(file, "r");
            skelNumPoints = raFile.readInt();
            skPoints = new double[skelNumPoints][3];

            for (i = 0; i < skelNumPoints; i++) {

                for (j = 0; j < 3; j++) {
                    skPoints[i][j] = raFile.readDouble();
                }
            } // for (i = 0; i < skelNumPoints; i++)

            skelNumSegments = raFile.readInt();
            skSegments = new int[skelNumSegments][4];

            for (i = 0; i < skelNumSegments; i++) {

                for (j = 0; j < 4; j++) {
                    skSegments[i][j] = raFile.readInt();
                }
            } // for (i = 0; i < skelNumSegments; i++)

            raFile.close();

        } catch (IOException error) {
            MipavUtil.displayError(fileSKF + " read error");
        }


        return vfImage;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int i;

        if (!loadButton.isSelected()) {

            if (vfImage != null) {

                if (vfImage[0] != null) {
                    vfImage[0].disposeLocal();
                    vfImage[0] = null;
                }

                if (vfImage[1] != null) {
                    vfImage[1].disposeLocal();
                    vfImage[1] = null;
                }

                if (vfImage[2] != null) {
                    vfImage[2].disposeLocal();
                    vfImage[2] = null;
                }

                vfImage = null;
            } // if (vfImage != null)

            skelNumPoints = 0;

            if (skPoints != null) {

                for (i = 0; i < skPoints.length; i++) {
                    skPoints[i] = null;
                }

                skPoints = null;
            }

            skelNumSegments = 0;

            if (skSegments != null) {

                for (i = 0; i < skSegments.length; i++) {
                    skSegments[i] = null;
                }

                skSegments = null;
            }
        } // if (!loadButton.isSelected())

        tmpStr = textMinObjectValue.getText();
        minObjectValue = Float.parseFloat(tmpStr);

        if (minObjectValue <= image.getMin()) {
            MipavUtil.displayError("Minimum object value must exceed image min of " + image.getMin());
            textMinObjectValue.requestFocus();
            textMinObjectValue.selectAll();

            return false;
        } else if (minObjectValue > image.getMax()) {
            MipavUtil.displayError("Minimum object value must not exceed image max of " + image.getMax());
            textMinObjectValue.requestFocus();
            textMinObjectValue.selectAll();

            return false;
        }

        sliceHoleFilling = sliceHoleFillingCheckBox.isSelected();

        if (!loadButton.isSelected()) {

            // If loading the save vector fields and the level 1 skeleton, then the
            // distCharges and fieldStrength parameters are not used
            tmpStr = textDistCharges.getText();
            distCharges = Integer.parseInt(tmpStr);

            if (distCharges < -40) {
                MipavUtil.displayError("Distance from charges must be at least -40");
                textDistCharges.requestFocus();
                textDistCharges.selectAll();

                return false;
            } else if (distCharges > 40) {
                MipavUtil.displayError("Distance from charges must not exceed 40");
                textDistCharges.requestFocus();
                textDistCharges.selectAll();

                return false;
            }

            tmpStr = textFieldStrength.getText();
            fieldStrength = Integer.parseInt(tmpStr);

            if (fieldStrength < 4) {
                MipavUtil.displayError("Field strength must be at least 4");
                textFieldStrength.requestFocus();
                textFieldStrength.selectAll();

                return false;
            } else if (fieldStrength > 9) {
                MipavUtil.displayError("Field strength must not exceed 9");
                textFieldStrength.requestFocus();
                textFieldStrength.selectAll();

                return false;
            }
        } // if (!loadButton.isSelected())

        tmpStr = textDivergencePoints.getText();
        perHDPoints = Float.parseFloat(tmpStr);

        if (perHDPoints < 0.0f) {
            MipavUtil.displayError("Fraction of divergence points must be at least 0.0");
            textDivergencePoints.requestFocus();
            textDivergencePoints.selectAll();

            return false;
        } else if (perHDPoints > 1.0f) {
            MipavUtil.displayError("Fraction of divergence points must not exceed 1.0");
            textDivergencePoints.requestFocus();
            textDivergencePoints.selectAll();

            return false;
        }


        if (saveButton.isSelected()) {
            saveVF = true;
        } else {
            saveVF = false;
        }

        if (pointButton.isSelected()) {
            outputPoints = true;
        } else {
            outputPoints = false;
        }

        return true;
    }

}
