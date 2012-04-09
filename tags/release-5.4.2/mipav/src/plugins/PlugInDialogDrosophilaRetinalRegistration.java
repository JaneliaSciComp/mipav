import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.*;
import java.util.StringTokenizer;

import javax.swing.*;
import javax.swing.border.*;

import WildMagic.LibFoundation.Curves.BSplineBasisf;


/**
 * This plugin was done for Dr Chi-hon Lee of NICHD
 * 
 * The main research problem Dr Lee had was that the z-resolution is poor compared to x and y when acquiring datsets
 * using confocal microscopy
 * 
 * The solution was to acquire 2 orthogonal datsets and combine them into a result image
 * 
 * In order to achieve this, one dataset needs to be registered to the other datset to obtain transformation files. This
 * part is done prior to the plugin by a user
 * 
 * This plugin builds a result image by transforming back to both images using the transformation files and applying a
 * certain combination of the pixels to put in the result image
 * 
 * The result image is of 512x512x512 size
 * 
 * @author pandyan
 */
public class PlugInDialogDrosophilaRetinalRegistration extends JDialogBase implements AlgorithmInterface {

    /** grid bag constraints * */
    private GridBagConstraints gbc;

    /** main panel * */
    private JPanel mainPanel, optionsPanel, processPanel, interpPanel, rescalePanel, averagingOptionsPanel, mirroredImgPanel;

    /** images * */
    private ModelImage imageX, imageXRegistered, imageY;

    /** transform files * */
    private File transform1File, transform2File, transform3File;

    /** current directory * */
    private String currDir = null;

    /** transform matrices * */
    private TransMatrix matrixGreen, matrixAffine;

    /** textfields * */
    private JTextField imageXFilePathTextField, imageXRegisteredFilePathTextField, imageYFilePathTextField,
            transform1FilePathTextField, transform2FilePathTextField, transform3FilePathTextField;

    /** browse button * */
    private JButton imageXBrowseButton, imageXRegisteredBrowseButton, imageYBrowseButton, transform1BrowseButton,
            transform2BrowseButton, transform3BrowseButton;

    /** boolean indicating average or closest z * */
    // private boolean doAverage = true;
    /** button group * */
    private ButtonGroup processGroup, interpGroup, rescaleGroup, ignoreBGGroup, mirroredImgGroup;

    /** radio buttons * */
    private JRadioButton doAverageRadio, doClosestZRadio, doSqRtRadio, doTrilinearRadio, doBsplineRadio,
            doRescaleRadio, noRescaleRadio, ignoreBGRadio, includeBGRadio, createMirroredImgRadio, doNotCreateMirroredImgRadio, doWeightingRadio;

    /** 2.5 d */
    private boolean have25D = false;

    /** num dims */
    private int nDims;

    /** resolutions */
    private float[] resolutions;

    /** extents */
    private int[] imageYExtents;

    /** extents */
    private int destMinExtent;

    /** num slices */
    private int numberSlices;

    /** spline degree */
    private int splineDegree;

    /** number control points */
    private int numControlPoints;

    /** control matrix */
    private float[][] controlMat;

    /** control matrix */
    private float[][][] controlMat25D;

    /** vjf to draw vois on * */
    private ViewJFrameImage vjfX, vjfY;

    /** output text area * */
    private JTextArea outputTextArea;

    /** scroll pane * */
    private JScrollPane scrollPane;

    /** alg * */
    private PlugInAlgorithmDrosophilaRetinalRegistration alg;

    /**
     * constructor
     */
    public PlugInDialogDrosophilaRetinalRegistration() {

    }

    /**
     * constructor
     * 
     * @param modal
     */
    public PlugInDialogDrosophilaRetinalRegistration(final boolean modal) {
        super(modal);
        init();
    }

    
    /**
     * init
     * 
     */
    public void init() {
        setForeground(Color.black);
        setTitle("Drosophila Retinal Registration v2.9");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();

        final JLabel imageXLabel = new JLabel("Image H");
        imageXFilePathTextField = new JTextField(55);
        imageXFilePathTextField.setEditable(false);
        imageXFilePathTextField.setBackground(Color.white);
        imageXBrowseButton = new JButton("Browse");
        imageXBrowseButton.addActionListener(this);
        imageXBrowseButton.setActionCommand("imageXBrowse");

        final JLabel imageXRegisteredLabel = new JLabel("Image H - Registered");
        imageXRegisteredFilePathTextField = new JTextField(55);
        imageXRegisteredFilePathTextField.setEditable(false);
        imageXRegisteredFilePathTextField.setBackground(Color.white);
        imageXRegisteredBrowseButton = new JButton("Browse");
        imageXRegisteredBrowseButton.addActionListener(this);
        imageXRegisteredBrowseButton.setActionCommand("imageXRegisteredBrowse");

        final JLabel imageYLabel = new JLabel("Image F");
        imageYFilePathTextField = new JTextField(55);
        imageYFilePathTextField.setEditable(false);
        imageYFilePathTextField.setBackground(Color.white);
        imageYBrowseButton = new JButton("Browse");
        imageYBrowseButton.addActionListener(this);
        imageYBrowseButton.setActionCommand("imageYBrowse");

        final JLabel transform1Label = new JLabel("Transformation 1 - Green (optional)");
        transform1FilePathTextField = new JTextField(55);
        transform1FilePathTextField.setEditable(false);
        transform1FilePathTextField.setBackground(Color.white);
        transform1BrowseButton = new JButton("Browse");
        transform1BrowseButton.addActionListener(this);
        transform1BrowseButton.setActionCommand("transform1Browse");

        final JLabel transform2Label = new JLabel("Transformation 2 - Affine");
        transform2FilePathTextField = new JTextField(55);
        transform2FilePathTextField.setEditable(false);
        transform2FilePathTextField.setBackground(Color.white);
        transform2BrowseButton = new JButton("Browse");
        transform2BrowseButton.addActionListener(this);
        transform2BrowseButton.setActionCommand("transform2Browse");

        final JLabel transform3Label = new JLabel("Transformation 3 - Nonlinear (optional)");
        transform3FilePathTextField = new JTextField(55);
        transform3FilePathTextField.setEditable(false);
        transform3FilePathTextField.setBackground(Color.white);
        transform3BrowseButton = new JButton("Browse");
        transform3BrowseButton.addActionListener(this);
        transform3BrowseButton.setActionCommand("transform3Browse");

        processGroup = new ButtonGroup();
        doAverageRadio = new JRadioButton("Average");
        doAverageRadio.addActionListener(this);
        doAverageRadio.setActionCommand("average");
        doSqRtRadio = new JRadioButton("SqrRt(Intensity-H x Intensity-F)");
        doSqRtRadio.addActionListener(this);
        doSqRtRadio.setActionCommand("sqrRt");
        doClosestZRadio = new JRadioButton("Closest Z");
        doClosestZRadio.addActionListener(this);
        doClosestZRadio.setActionCommand("closestZ");
        doAverageRadio.setSelected(true);
        doWeightingRadio = new JRadioButton("Weighted");
        doWeightingRadio.addActionListener(this);
        doWeightingRadio.setActionCommand("weighted");
        
        processGroup.add(doAverageRadio);
        processGroup.add(doSqRtRadio);
        processGroup.add(doWeightingRadio);
        // processGroup.add(doClosestZRadio);

        interpGroup = new ButtonGroup();
        doTrilinearRadio = new JRadioButton("Trilinear");
        doBsplineRadio = new JRadioButton("B Spline");
        doTrilinearRadio.setSelected(true);
        interpGroup.add(doTrilinearRadio);
        interpGroup.add(doBsplineRadio);

        rescaleGroup = new ButtonGroup();
        doRescaleRadio = new JRadioButton("Rescale");
        noRescaleRadio = new JRadioButton("No Rescale");
        doRescaleRadio.setSelected(true);
        rescaleGroup.add(doRescaleRadio);
        rescaleGroup.add(noRescaleRadio);
        
        mirroredImgGroup = new ButtonGroup();
        doNotCreateMirroredImgRadio = new JRadioButton("Do not create additional mirrored image");
        doNotCreateMirroredImgRadio.setSelected(true);
        createMirroredImgRadio = new JRadioButton("Create additional mirrored image");
        mirroredImgGroup.add(doNotCreateMirroredImgRadio);
        mirroredImgGroup.add(createMirroredImgRadio);

        ignoreBGGroup = new ButtonGroup();
        ignoreBGRadio = new JRadioButton("Ignore background pixels");
        includeBGRadio = new JRadioButton("Include background pixels");
        ignoreBGRadio.setSelected(true);
        ignoreBGGroup.add(ignoreBGRadio);
        ignoreBGGroup.add(includeBGRadio);

        outputTextArea = new JTextArea();
        outputTextArea.setRows(15);
        outputTextArea.setEditable(false);
        outputTextArea.setBackground(Color.lightGray);
        outputTextArea.setBorder(new LineBorder(Color.black));
        outputTextArea.setForeground(Color.black);
        scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15, 5, 5, 15);

        optionsPanel = new JPanel();

        processPanel = new JPanel(new GridBagLayout());
        interpPanel = new JPanel(new GridBagLayout());
        rescalePanel = new JPanel(new GridBagLayout());
        mirroredImgPanel  = new JPanel(new GridBagLayout());
        averagingOptionsPanel = new JPanel(new GridBagLayout());
        processPanel.setBorder(new TitledBorder("Process using"));
        interpPanel.setBorder(new TitledBorder("Interpolation"));
        rescalePanel.setBorder(new TitledBorder("Rescale H to F"));
        averagingOptionsPanel.setBorder(new TitledBorder("Averaging Options"));
        mirroredImgPanel.setBorder(new TitledBorder("Mirrored Image"));
         
        processPanel.add(doAverageRadio, gbc);
        gbc.gridy = 1;
        processPanel.add(doSqRtRadio, gbc);
        gbc.gridy = 2;
        processPanel.add(doWeightingRadio, gbc);
        gbc.gridy = 0;
        interpPanel.add(doTrilinearRadio, gbc);
        gbc.gridy = 1;
        interpPanel.add(doBsplineRadio, gbc);
        gbc.gridy = 0;
        rescalePanel.add(doRescaleRadio, gbc);
        gbc.gridy = 1;
        rescalePanel.add(noRescaleRadio, gbc);
        gbc.gridy = 0;
        averagingOptionsPanel.add(ignoreBGRadio, gbc);
        gbc.gridy = 1;
        averagingOptionsPanel.add(includeBGRadio, gbc);
        gbc.gridy = 0;
        mirroredImgPanel.add(doNotCreateMirroredImgRadio, gbc);
        gbc.gridy = 1;
        mirroredImgPanel.add(createMirroredImgRadio, gbc);

        optionsPanel.add(processPanel);
        optionsPanel.add(interpPanel);
        optionsPanel.add(rescalePanel);
        optionsPanel.add(averagingOptionsPanel);
        optionsPanel.add(mirroredImgPanel);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15, 5, 5, 15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(imageXLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(imageXFilePathTextField, gbc);
        gbc.gridx = 2;
        mainPanel.add(imageXBrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(imageXRegisteredLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(imageXRegisteredFilePathTextField, gbc);
        gbc.gridx = 2;
        mainPanel.add(imageXRegisteredBrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(imageYLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(imageYFilePathTextField, gbc);
        gbc.gridx = 2;
        mainPanel.add(imageYBrowseButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(transform1Label, gbc);
        gbc.gridx = 1;
        mainPanel.add(transform1FilePathTextField, gbc);
        gbc.gridx = 2;
        mainPanel.add(transform1BrowseButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(transform2Label, gbc);
        gbc.gridx = 1;
        mainPanel.add(transform2FilePathTextField, gbc);
        gbc.gridx = 2;
        mainPanel.add(transform2BrowseButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        mainPanel.add(transform3Label, gbc);
        gbc.gridx = 1;
        mainPanel.add(transform3FilePathTextField, gbc);
        gbc.gridx = 2;
        mainPanel.add(transform3BrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(optionsPanel, gbc);

        gbc.gridy = 7;
        mainPanel.add(scrollPane, gbc);
        final JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        setResizable(false);
        pack();
        setVisible(true);

    }

    /**
     * action performed
     */
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();
        if (command.equalsIgnoreCase("average")) {
            ignoreBGRadio.setEnabled(true);
            includeBGRadio.setEnabled(true);
        } else if (command.equalsIgnoreCase("sqrRt")) {
            ignoreBGRadio.setEnabled(true);
            includeBGRadio.setEnabled(true);
        }  else if (command.equalsIgnoreCase("weighted")) {
            ignoreBGRadio.setEnabled(true);
            includeBGRadio.setEnabled(true);
        }else if (command.equalsIgnoreCase("closestZ")) {
            ignoreBGRadio.setEnabled(false);
            includeBGRadio.setEnabled(false);
        } else if (command.equalsIgnoreCase("imageXBrowse")) {
            final JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose image");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                final FileIO fileIO = new FileIO();
                imageX = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                        + File.separator, true, null);

                if (doRescaleRadio.isSelected()) {
                    vjfX = new ViewJFrameImage(imageX);
                }
                imageXFilePathTextField.setText(currDir);
            }
        } else if (command.equalsIgnoreCase("imageXRegisteredBrowse")) {
            final JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose image");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                final FileIO fileIO = new FileIO();
                imageXRegistered = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                        + File.separator, true, null);
                imageXRegisteredFilePathTextField.setText(currDir);
            }
        } else if (command.equalsIgnoreCase("imageYBrowse")) {
            final JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose image");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                final FileIO fileIO = new FileIO();
                imageY = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                        + File.separator, true, null);
                imageYExtents = imageY.getExtents();

                if (doRescaleRadio.isSelected()) {
                    vjfY = new ViewJFrameImage(imageY);
                }
                imageYFilePathTextField.setText(currDir);
            }
        } else if (command.equalsIgnoreCase("transform1Browse")) {

            final JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose transformation 1");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                // parentDir = chooser.getSelectedFile().getParent();
                transform1File = new File(currDir);
                readTransform1(transform1File);
            }
        } else if (command.equalsIgnoreCase("transform2Browse")) {
            final JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose transformation 2");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                transform2File = new File(currDir);
                readTransform2(transform2File);
            }
        } else if (command.equalsIgnoreCase("transform3Browse")) {
            final JFileChooser chooser = new JFileChooser();
            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose transformation 3");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                transform3File = new File(currDir);
                if ( !transform3File.getName().endsWith(".nlt")) {
                    MipavUtil.displayError("A valid .nlt file is required");
                    return;
                }
                boolean success = readNLTFile(transform3File);
                if ( !success) {
                    MipavUtil.displayError("Error reading nlt file");
                    return;
                }
                transform3FilePathTextField.setText(currDir);
            }
        } else if (command.equalsIgnoreCase("cancel")) {
            if (imageX != null) {
                imageX.disposeLocal();
                imageX = null;
            }
            if (imageY != null) {
                imageY.disposeLocal();
                imageY = null;
            }
            if (vjfX != null) {
                vjfX.close();
            }
            if (vjfY != null) {
                vjfY.close();
            }
            dispose();
        } else if (command.equalsIgnoreCase("ok")) {
            if ( !validatingVars()) {
                return;
            } else {
                callAlgorithm();
            }

        }
    }

    /*******************************************************************************************************************
     * calls algorithm
     */
    protected void callAlgorithm() {
    	boolean createMirroredImg = createMirroredImgRadio.isSelected();
        alg = new PlugInAlgorithmDrosophilaRetinalRegistration(imageX, imageXRegistered, imageY, vjfX, vjfY,
                outputTextArea, matrixGreen, matrixAffine, doTrilinearRadio, doAverageRadio, doRescaleRadio,
                ignoreBGRadio, doSqRtRadio, transform3FilePathTextField, numControlPoints, splineDegree, controlMat, createMirroredImg);

        alg.addListener(this);
        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still
            // have user interface work fast.
            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            alg.run();
        }
    }

    /**
     * validate vars
     * 
     * @return success
     */
    private boolean validatingVars() {
        boolean valid = true;
        if (imageXFilePathTextField.getText().equals("")) {
            MipavUtil.displayError("Image H must be entered");
            valid = false;
            return valid;
        }
        if (imageXRegisteredFilePathTextField.getText().equals("")) {
            MipavUtil.displayError("Image H-Registered must be entered");
            valid = false;
            return valid;
        }
        if (imageYFilePathTextField.getText().equals("")) {
            MipavUtil.displayError("Image F must be entered");
            valid = false;
            return valid;
        }
        if (transform2FilePathTextField.getText().equals("")) {
            MipavUtil.displayError("Transformation 2-Affine must be entered");
            valid = false;
            return valid;
        }
        /*if (transform2FilePathTextField.getText().equals("")) {
            MipavUtil.displayError("Transformation 2-Affine must be entered");
            valid = false;
            return valid;
        }*/

        return valid;
    }

    /**
     * reads the non-linear transform file
     * 
     * @param nltFile success
     * @return
     */
    private boolean readNLTFile(final File nltFile) {
        RandomAccessFile in;
        String str = null;
        StringTokenizer stoken = null;
        int i, j, k;
        int srcMinExtent;
        int iNumControlPointsMax;
        try {
            in = new RandomAccessFile(nltFile, "r");

            // read number of dimensions
            do {
                str = in.readLine().trim();
            } while (str.substring(0, 1).equals("#"));

            final float fDims = Float.valueOf(str).floatValue();

            if (2.5f == fDims) {
                nDims = 3;
                have25D = true;
            } else {
                nDims = (int) fDims;
                have25D = false;
            }

            if (imageY.getNDims() != nDims) {
                MipavUtil.displayError("");
                in.close();

                return false;
            }

            // read resolutions for output image
            do {
                str = in.readLine().trim();
            } while (str.substring(0, 1).equals("#"));

            stoken = new StringTokenizer(str);
            resolutions = new float[nDims];
            srcMinExtent = Integer.MAX_VALUE;

            for (i = 0; i < nDims; i++) {
                resolutions[i] = Float.valueOf(stoken.nextToken()).floatValue();

                if ( (imageY.getExtents()[i] < srcMinExtent) && ( ( !have25D) || (i < 2))) {
                    srcMinExtent = imageY.getExtents()[i];
                }
            }

            // If 2D/3D, read dimensions for target image
            if ( !have25D) {

                do {
                    str = in.readLine().trim();
                } while (str.substring(0, 1).equals("#"));

                stoken = new StringTokenizer(str);
                imageYExtents = new int[nDims];
                destMinExtent = Integer.MAX_VALUE;

                for (i = 0; i < nDims; i++) {
                    imageYExtents[i] = Integer.valueOf(stoken.nextToken()).intValue();

                    if (imageYExtents[i] < destMinExtent) {
                        destMinExtent = imageYExtents[i];
                    }
                }
            } else {
                numberSlices = imageY.getExtents()[2];
            }

            // read B-spline degree
            do {
                str = in.readLine().trim();
            } while (str.substring(0, 1).equals("#"));

            stoken = new StringTokenizer(str);
            splineDegree = Integer.valueOf(stoken.nextToken()).intValue();

            if ( (splineDegree < 1) || (splineDegree > 4)) {
                MipavUtil.displayError("Error! Spline degree has an illegal value = " + splineDegree);
                in.close();

                return false;
            }

            // read number of control points
            do {
                str = in.readLine().trim();
            } while (str.substring(0, 1).equals("#"));

            stoken = new StringTokenizer(str);
            numControlPoints = Integer.valueOf(stoken.nextToken()).intValue();

            final int iNumControlPointsMin = BSplineBasisf.GetMinNumControlPoints(splineDegree);

            if (have25D) {
                iNumControlPointsMax = srcMinExtent / 2;
            } else {
                iNumControlPointsMax = destMinExtent / 2;
            }

            if (numControlPoints < iNumControlPointsMin) {
                MipavUtil.displayError("Error! The parameter file specifies " + numControlPoints
                        + " control points, but " + iNumControlPointsMin + " are required");
                in.close();

                return false;
            }

            if (numControlPoints > iNumControlPointsMax) {
                MipavUtil.displayError("Error! The parameter file specifies " + numControlPoints
                        + " control points, but no more than " + iNumControlPointsMax + " are allowed");
                in.close();

                return false;
            }

            if ( !have25D) {
                final int allDimControlPoints = (nDims == 2) ? (numControlPoints * numControlPoints)
                        : (numControlPoints * numControlPoints * numControlPoints);

                controlMat = new float[allDimControlPoints][nDims];

                for (i = 0; i < allDimControlPoints; i++) {

                    do {
                        str = in.readLine().trim();
                    } while (str.substring(0, 1).equals("#"));

                    stoken = new StringTokenizer(str);

                    for (j = 0; j < nDims; j++) {
                        controlMat[i][j] = Float.valueOf(stoken.nextToken()).floatValue();
                    }
                } // for (i = 0; i < allDimControlPoints; i++)
            } // if (!have25D)
            else { // have25D

                final int allDimControlPoints = numControlPoints * numControlPoints;
                controlMat25D = new float[numberSlices][allDimControlPoints][2];

                for (k = 0; k < numberSlices; k++) {

                    for (i = 0; i < allDimControlPoints; i++) {

                        do {
                            str = in.readLine().trim();
                        } while (str.substring(0, 1).equals("#"));

                        stoken = new StringTokenizer(str);
                        controlMat25D[k][i][0] = Float.valueOf(stoken.nextToken()).floatValue();
                        controlMat25D[k][i][1] = Float.valueOf(stoken.nextToken()).floatValue();
                    } // for (i = 0; i < allDimControlPoints; i++)
                } // for (k = 0; k < numberSlices; k++)
            } // else have25D

            in.close();

            return true;
        } catch (final IOException e) {
            MipavUtil.displayError("Read Error reading nlt file : " + e.getMessage());

            return false;
        }
    }

    /**
     * reads the transform file
     * 
     * @param transformFile
     */
    private void readTransform1(final File transform1File) {
        try {
            final RandomAccessFile raFile = new RandomAccessFile(transform1File, "r");
            String[] arr;
            raFile.readLine(); // skip over num columns since we know it is 4
            raFile.readLine(); // skip over num rows since we know it is 4
            final double[][] doubleArr = new double[4][4];
            final String line1 = raFile.readLine().trim();
            arr = line1.split("\\s+");
            if (arr.length == 4) {
                doubleArr[0][0] = Double.valueOf(arr[0]).doubleValue();
                doubleArr[0][1] = Double.valueOf(arr[1]).doubleValue();
                doubleArr[0][2] = Double.valueOf(arr[2]).doubleValue();
                doubleArr[0][3] = Double.valueOf(arr[3]).doubleValue();
            }
            final String line2 = raFile.readLine().trim();
            arr = line2.split("\\s+");
            if (arr.length == 4) {
                doubleArr[1][0] = Double.valueOf(arr[0]).doubleValue();
                doubleArr[1][1] = Double.valueOf(arr[1]).doubleValue();
                doubleArr[1][2] = Double.valueOf(arr[2]).doubleValue();
                doubleArr[1][3] = Double.valueOf(arr[3]).doubleValue();
            }
            final String line3 = raFile.readLine().trim();
            arr = line3.split("\\s+");
            if (arr.length == 4) {
                doubleArr[2][0] = Double.valueOf(arr[0]).doubleValue();
                doubleArr[2][1] = Double.valueOf(arr[1]).doubleValue();
                doubleArr[2][2] = Double.valueOf(arr[2]).doubleValue();
                doubleArr[2][3] = Double.valueOf(arr[3]).doubleValue();
            }
            final String line4 = raFile.readLine().trim();
            arr = line4.split("\\s+");
            if (arr.length == 4) {
                doubleArr[3][0] = Double.valueOf(arr[0]).doubleValue();
                doubleArr[3][1] = Double.valueOf(arr[1]).doubleValue();
                doubleArr[3][2] = Double.valueOf(arr[2]).doubleValue();
                doubleArr[3][3] = Double.valueOf(arr[3]).doubleValue();
            }
            raFile.close();

            matrixGreen = new TransMatrix(4);
            matrixGreen.setMatrix(doubleArr);
            transform1FilePathTextField.setText(currDir);
        } catch (final Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * reads the transform file
     * 
     * @param transformFile
     */
    private void readTransform2(final File transform2File) {
        try {
            final RandomAccessFile raFile = new RandomAccessFile(transform2File, "r");
            String[] arr;
            raFile.readLine(); // skip over num columns since we know it is 4
            raFile.readLine(); // skip over num rows since we know it is 4
            final double[][] doubleArr = new double[4][4];
            final String line1 = raFile.readLine().trim();
            arr = line1.split("\\s+");
            if (arr.length == 4) {
                doubleArr[0][0] = Double.valueOf(arr[0]).doubleValue();
                doubleArr[0][1] = Double.valueOf(arr[1]).doubleValue();
                doubleArr[0][2] = Double.valueOf(arr[2]).doubleValue();
                doubleArr[0][3] = Double.valueOf(arr[3]).doubleValue();
            }
            final String line2 = raFile.readLine().trim();
            arr = line2.split("\\s+");
            if (arr.length == 4) {
                doubleArr[1][0] = Double.valueOf(arr[0]).doubleValue();
                doubleArr[1][1] = Double.valueOf(arr[1]).doubleValue();
                doubleArr[1][2] = Double.valueOf(arr[2]).doubleValue();
                doubleArr[1][3] = Double.valueOf(arr[3]).doubleValue();
            }
            final String line3 = raFile.readLine().trim();
            arr = line3.split("\\s+");
            if (arr.length == 4) {
                doubleArr[2][0] = Double.valueOf(arr[0]).doubleValue();
                doubleArr[2][1] = Double.valueOf(arr[1]).doubleValue();
                doubleArr[2][2] = Double.valueOf(arr[2]).doubleValue();
                doubleArr[2][3] = Double.valueOf(arr[3]).doubleValue();
            }
            final String line4 = raFile.readLine().trim();
            arr = line4.split("\\s+");
            if (arr.length == 4) {
                doubleArr[3][0] = Double.valueOf(arr[0]).doubleValue();
                doubleArr[3][1] = Double.valueOf(arr[1]).doubleValue();
                doubleArr[3][2] = Double.valueOf(arr[2]).doubleValue();
                doubleArr[3][3] = Double.valueOf(arr[3]).doubleValue();
            }
            raFile.close();
            matrixAffine = new TransMatrix(4);
            matrixAffine.setMatrix(doubleArr);
            transform2FilePathTextField.setText(currDir);
        } catch (final Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * algorithm performed
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (alg.isCompleted()) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            OKButton.setEnabled(false);
            cancelButton.setText("Close");

            outputTextArea.append("Finished" + "\n");
        }

    }

}
