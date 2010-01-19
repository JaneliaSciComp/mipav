package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input for 3 parameter dynamic (contrast) enhanced MRI model or DEMRI model
 */
public class JDialogDEMRI3 extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private JLabel labelParamsToFit1;
    
    private JLabel labelParamsToFit2;
    
    private JLabel labelParamsToFit3;
    
    private JLabel labelParamsToFit4;

    /** DOCUMENT ME! */
    private JLabel labelContrastRelaxivityRate;
    
    private JTextField textContrastRelaxivityRate;
    
    // contrast relaxivity rate
    double r1;
    
    double r1Min = 0.0;
    
    double r1Max = 1000.0;
    
    private JLabel labelBloodIntrinsicRelaxivityRate;
    
    private JLabel labelBloodIntrinsicRelaxivityRate2;
    
    private JTextField textBloodIntrinsicRelaxivityRate;
    
    // blood intrinsic relaxivity rate
    double rib;
    
    double ribMin = 0.001;
    
    double ribMax = 10000.0;
    
    private ButtonGroup tissueGroup;
    
    private JRadioButton constantTissueRadioButton;
    
    private JRadioButton fileTissueRadioButton;
    
    private JLabel labelTissueIntrinsicRelaxivityRate;
    
    private JLabel labelTissueIntrinsicRelaxivityRate2;
    
    private JTextField textTissueIntrinsicRelaxivityRate;
    
    // tissue intrinsic relaxivity rate
    double rit;
    
    double ritMin = 0.001;
    
    double ritMax = 10000.0;
    
    private JButton buttonTissueFile;
    
    private JTextField textTissueFile;
    
    // non-uniform tissue intrinsic relaxivity map
    double r1i[] = null;
    
    private String directoryTissue;
    
    private String fileNameTissue;
    
    private ModelImage tissueImage = null;
    
    private JLabel labelFlipAngle;
    
    private JTextField textFlipAngle;
    
    // flip angle in degrees
    double theta;
    
    double thetaMin = 0.0;
    
    double thetaMax = 90.0;
    
    private JLabel labelTimeBetweenShots;
    
    private JTextField textTimeBetweenShots;
    
    // time between shots in seconds
    double tr;
    
    double trMin = 0.0;
    
    double trMax = 10000.0;
    
    private JLabel labelTimeBetweenFrames;
    
    private JTextField textTimeBetweenFrames;
    
    // time between frames (volumes) in seconds
    double tf;
    
    double tfMin = 0.1;
    
    double tfMax = 30.0;
    
    private ButtonGroup rateGroup;
    
    private JRadioButton secondButton;
    
    private JRadioButton minuteButton;
    
    boolean perMin = false;
    
    private JButton buttonMpFile;
    
    private JTextField textMpFile;
    
    private String directoryMp;
    
    private String fileNameMp;
    
    private File fileMp;
    
    // Contents of Mp(t) file
    private double mcp[] = null;
    
    private int nx;
    
    private int ny;
    
    private int mp_len;
    
    private JLabel labelNFirst;
    
    private JTextField textNFirst;
    
    private int nFirst;
    
    private int nFirstMin = 0;
    
    private int nFirstMax = 1000;
    
    private ButtonGroup secondParamGroup;
    
    private JRadioButton kepButton;
    
    private JRadioButton veButton;
    
    private boolean useVe = false;

    /** DOCUMENT ME! */
    private int backgroundIndex = -1;

    /** DOCUMENT ME! */
    private int cost;


    /** DOCUMENT ME! */
    private boolean createRegImage = false;


    /** DOCUMENT ME! */
    private int donorIndex = -1;


    /** DOCUMENT ME! */
    private AlgorithmDEMRI3 demri3Algo = null;

    /** DOCUMENT ME! */
    private ModelImage image; // prebleached image

    /** DOCUMENT ME! */
    private ModelImage postImage; // postbleached image

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private int signalIndex = -1;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogDEMRI3 object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogDEMRI3(ModelImage image) {
        super();
        this.UI = ViewUserInterface.getReference();
        this.image = image;
        parentFrame = image.getParentFrame();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogDEMRI3(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        UI = ViewUserInterface.getReference();
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("TissueFile")) {

                try {
                    JFileChooser chooser = new JFileChooser();

                    if (UI.getDefaultDirectory() != null) {
                        File file = new File(UI.getDefaultDirectory());

                        if (file != null) {
                            chooser.setCurrentDirectory(file);
                        } else {
                            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                        }
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }

                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                    chooser.setDialogTitle("Open tissue intrinsic relaxivity rate file");
                    directoryTissue = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                    int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        fileNameTissue = chooser.getSelectedFile().getName();
                        directoryTissue = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                        UI.setDefaultDirectory(directoryTissue);
                    } else {
                        fileNameTissue = null;

                        return;
                    }

                    if (fileNameTissue != null) {
                        textTissueFile.setText(fileNameTissue);
                    }
                } catch (OutOfMemoryError e) {
                    MipavUtil.displayError("Out of memory in JDialogDEMRI3.");

                    return;
                }
        } else if (command.equals("MpFile")) {

            try {
                JFileChooser chooser = new JFileChooser();

                if (UI.getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                chooser.setDialogTitle("Open 1D Mp(t) data file");
                directoryMp = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameMp = chooser.getSelectedFile().getName();
                    directoryMp = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    UI.setDefaultDirectory(directoryMp);
                } else {
                    fileNameMp = null;

                    return;
                }

                if (fileNameMp != null) {
                    textMpFile.setText(fileNameMp);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogDEMRI3.");

                return;
            }
    }   else if (command.equals("Cancel")) {
            dispose();
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


        if (demri3Algo.isCompleted() == true) { }

        dispose();
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if ((source == constantTissueRadioButton) || (source == fileTissueRadioButton)) {
            buttonTissueFile.setEnabled(fileTissueRadioButton.isSelected());
            textTissueFile.setEnabled(fileTissueRadioButton.isSelected());
            labelTissueIntrinsicRelaxivityRate.setEnabled(constantTissueRadioButton.isSelected());
            labelTissueIntrinsicRelaxivityRate2.setEnabled(constantTissueRadioButton.isSelected());
            textTissueIntrinsicRelaxivityRate.setEnabled(constantTissueRadioButton.isSelected());
        }


    }

    

    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {

        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

        try {

            // Make algorithm

            demri3Algo = new AlgorithmDEMRI3(r1, rib, rit, r1i, theta, tr, tf, perMin, mcp, mp_len, nFirst, useVe);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            demri3Algo.addListener(this);

            createProgressBar(image.getImageName(), demri3Algo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (demri3Algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                demri3Algo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog DEMRI3: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildComboBox(ModelImage image) {
        ViewUserInterface UI;
        ModelImage nextImage;
        boolean doAdd;
        int i;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(nextImage) != null) {

                    if ((image.isColorImage() == nextImage.isColorImage()) && (nextImage.getNDims() == 2)) {
                        doAdd = true;

                        for (i = 0; i < image.getNDims(); i++) {

                            if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                                doAdd = false;
                            }
                        }

                        if (doAdd) {
                            comboBox.addItem(name);
                        }
                    }
                }
            }
        }

        return comboBox;
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        

        setForeground(Color.black);
        setTitle("3 - parameter DEMRI model");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
        
        labelParamsToFit1 = new JLabel("3 model parameters to fit:");
        labelParamsToFit1.setForeground(Color.black);
        labelParamsToFit1.setFont(serif12);
        mainPanel.add(labelParamsToFit1, gbc);
        
        labelParamsToFit2 = new JLabel("K_trans (plasma Gd -> tissue Gd) in [0, 0.05]");
        labelParamsToFit2.setForeground(Color.black);
        labelParamsToFit2.setFont(serif12);
        gbc.gridy = 1;
        mainPanel.add(labelParamsToFit2, gbc);
        
        labelParamsToFit3 = new JLabel("k_ep in [0, 0.05] or ve");
        labelParamsToFit3.setForeground(Color.black);
        labelParamsToFit3.setFont(serif12);
        gbc.gridy = 2;
        mainPanel.add(labelParamsToFit3, gbc);
        
        labelParamsToFit4 = new JLabel("f_vp in [0, 0.99]");
        labelParamsToFit4.setForeground(Color.black);
        labelParamsToFit4.setFont(serif12);
        gbc.gridy = 3;
        mainPanel.add(labelParamsToFit4, gbc);
        
        labelContrastRelaxivityRate = new JLabel("Contrast relaxivity rate in 1/(mMol*sec) (0.0 - 1000.0)");
        labelContrastRelaxivityRate.setForeground(Color.black);
        labelContrastRelaxivityRate.setFont(serif12);
        gbc.gridy = 4;
        mainPanel.add(labelContrastRelaxivityRate, gbc);
        
        textContrastRelaxivityRate = new JTextField(10);
        textContrastRelaxivityRate.setText("4.8");
        textContrastRelaxivityRate.setForeground(Color.black);
        textContrastRelaxivityRate.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textContrastRelaxivityRate, gbc);
        
        labelBloodIntrinsicRelaxivityRate = new JLabel("Blood intrinsic relaxivity rate in 1/(mMol * sec)");
        labelBloodIntrinsicRelaxivityRate.setForeground(Color.black);
        labelBloodIntrinsicRelaxivityRate.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 5;
        mainPanel.add(labelBloodIntrinsicRelaxivityRate, gbc);
        
        textBloodIntrinsicRelaxivityRate = new JTextField(10);
        textBloodIntrinsicRelaxivityRate.setText("1.5");
        textBloodIntrinsicRelaxivityRate.setForeground(Color.black);
        textBloodIntrinsicRelaxivityRate.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridheight = 2;
        mainPanel.add(textBloodIntrinsicRelaxivityRate, gbc);
        
        labelBloodIntrinsicRelaxivityRate2 = new JLabel("specified as reciprocal, in seconds (0.001 - 10000.0)");
        labelBloodIntrinsicRelaxivityRate2.setForeground(Color.black);
        labelBloodIntrinsicRelaxivityRate2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridheight = 1;
        mainPanel.add(labelBloodIntrinsicRelaxivityRate2, gbc);
        
        tissueGroup = new ButtonGroup();
        constantTissueRadioButton = new JRadioButton("Constant tissue intrinsic relaxivity rate", true);
        constantTissueRadioButton.setFont(serif12);
        constantTissueRadioButton.setForeground(Color.black);
        constantTissueRadioButton.addActionListener(this);
        tissueGroup.add(constantTissueRadioButton);
        gbc.gridx = 0;
        gbc.gridy = 7;
        mainPanel.add(constantTissueRadioButton, gbc);
        
        fileTissueRadioButton = new JRadioButton("File specified tissue intrinsic relaxivity rate", false);
        fileTissueRadioButton.setFont(serif12);
        fileTissueRadioButton.setForeground(Color.black);
        fileTissueRadioButton.addActionListener(this);
        tissueGroup.add(fileTissueRadioButton);
        gbc.gridx = 0;
        gbc.gridy = 8;
        mainPanel.add(fileTissueRadioButton, gbc);
        
        labelTissueIntrinsicRelaxivityRate = new JLabel("Tissue intrinsic relaxivity rate in 1/(mMol * sec)");
        labelTissueIntrinsicRelaxivityRate.setForeground(Color.black);
        labelTissueIntrinsicRelaxivityRate.setFont(serif12);
        labelTissueIntrinsicRelaxivityRate.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 9;
        mainPanel.add(labelTissueIntrinsicRelaxivityRate, gbc);
        
        textTissueIntrinsicRelaxivityRate = new JTextField(10);
        textTissueIntrinsicRelaxivityRate.setText("1.1");
        textTissueIntrinsicRelaxivityRate.setForeground(Color.black);
        textTissueIntrinsicRelaxivityRate.setFont(serif12);
        textTissueIntrinsicRelaxivityRate.setEnabled(true);
        gbc.gridx = 1;
        gbc.gridheight = 2;
        mainPanel.add(textTissueIntrinsicRelaxivityRate, gbc);
        
        labelTissueIntrinsicRelaxivityRate2 = new JLabel("specified as reciprocal, in seconds (0.001 - 10000.0)");
        labelTissueIntrinsicRelaxivityRate2.setForeground(Color.black);
        labelTissueIntrinsicRelaxivityRate2.setFont(serif12);
        labelTissueIntrinsicRelaxivityRate2.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridheight = 1;
        mainPanel.add(labelTissueIntrinsicRelaxivityRate2, gbc);
        
        buttonTissueFile = new JButton("Choose tissue file");
        buttonTissueFile.setForeground(Color.black);
        buttonTissueFile.setFont(serif12B);
        buttonTissueFile.setEnabled(false);
        buttonTissueFile.addActionListener(this);
        buttonTissueFile.setActionCommand("TissueFile");
        buttonTissueFile.setPreferredSize(new Dimension(145, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridy = 11;
        mainPanel.add(buttonTissueFile, gbc);

        textTissueFile = new JTextField();
        textTissueFile.setFont(serif12);
        textTissueFile.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textTissueFile, gbc);
        
        labelFlipAngle = new JLabel("Flip angle in degrees (0.0 - 90.0)");
        labelFlipAngle.setForeground(Color.black);
        labelFlipAngle.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 12;
        mainPanel.add(labelFlipAngle, gbc);
        
        textFlipAngle = new JTextField(10);
        textFlipAngle.setText("30.0");
        textFlipAngle.setForeground(Color.black);
        textFlipAngle.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textFlipAngle, gbc);
        
        labelTimeBetweenShots = new JLabel("Time between shots in seconds (0.0 - 10000.0)");
        labelTimeBetweenShots.setForeground(Color.black);
        labelTimeBetweenShots.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 13;
        mainPanel.add(labelTimeBetweenShots, gbc);
        
        textTimeBetweenShots = new JTextField(10);
        textTimeBetweenShots.setText("0.008");
        textTimeBetweenShots.setForeground(Color.black);
        textTimeBetweenShots.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textTimeBetweenShots, gbc);
        
        labelTimeBetweenFrames = new JLabel("Time between frames (volumes) in seconds (0.1 - 30.0)");
        labelTimeBetweenFrames.setForeground(Color.black);
        labelTimeBetweenFrames.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 14;
        mainPanel.add(labelTimeBetweenFrames, gbc);
        
        textTimeBetweenFrames = new JTextField(10);
        textTimeBetweenFrames.setText("20.0");
        textTimeBetweenFrames.setForeground(Color.black);
        textTimeBetweenFrames.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textTimeBetweenFrames, gbc);
        
        rateGroup = new ButtonGroup();
        secondButton = new JRadioButton("K_trans and k_ep are per second", true);
        secondButton.setFont(serif12);
        secondButton.setForeground(Color.black);
        rateGroup.add(secondButton);
        gbc.gridx = 0;
        gbc.gridy = 15;
        mainPanel.add(secondButton, gbc);
        
        minuteButton = new JRadioButton("K_trans and k_ep are per minute", false);
        minuteButton.setFont(serif12);
        minuteButton.setForeground(Color.black);
        rateGroup.add(minuteButton);
        gbc.gridx = 0;
        gbc.gridy = 16;
        mainPanel.add(minuteButton, gbc);
        
        buttonMpFile = new JButton("Choose 1D Mp(t) data file");
        buttonMpFile.setForeground(Color.black);
        buttonMpFile.setFont(serif12B);
        buttonMpFile.addActionListener(this);
        buttonMpFile.setActionCommand("MpFile");
        buttonMpFile.setPreferredSize(new Dimension(145, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridy = 17;
        mainPanel.add(buttonMpFile, gbc);

        textMpFile = new JTextField();
        textMpFile.setFont(serif12);
        textMpFile.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textMpFile, gbc);
        
        labelNFirst = new JLabel("nfirst injection TR index of input dataset (0 - 1000)");
        labelNFirst.setForeground(Color.black);
        labelNFirst.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 18;
        mainPanel.add(labelNFirst, gbc);
        
        textNFirst = new JTextField(10);
        textNFirst.setText("5");
        textNFirst.setForeground(Color.black);
        textNFirst.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textNFirst, gbc);
        
        secondParamGroup = new ButtonGroup();
        kepButton = new JRadioButton("Second parameter is back-transfer rate (k_ep)", true);
        kepButton.setFont(serif12);
        kepButton.setForeground(Color.black);
        secondParamGroup.add(kepButton);
        gbc.gridx = 0;
        gbc.gridy = 19;
        mainPanel.add(kepButton, gbc);
        
        veButton = new JRadioButton("Second parameter is external celluar volume fraction (ve)", false);
        veButton.setFont(serif12);
        veButton.setForeground(Color.black);
        secondParamGroup.add(veButton);
        gbc.gridx = 0;
        gbc.gridy = 20;
        mainPanel.add(veButton, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        BufferedReader br = null;
        
        tmpStr = textContrastRelaxivityRate.getText();
        r1 = Double.parseDouble(tmpStr);
        
        if (r1 < r1Min) {
            MipavUtil.displayError("Contrast relaxivity rate must be at least " + Double.toString(r1Min));
            textContrastRelaxivityRate.requestFocus();
            textContrastRelaxivityRate.selectAll();
            return false;
        }
        
        if (r1 > r1Max) {
            MipavUtil.displayError("Contrast relaxivity rate must not exceed " + Double.toString(r1Max));
            textContrastRelaxivityRate.requestFocus();
            textContrastRelaxivityRate.selectAll();
            return false;
        }
        
        tmpStr = textBloodIntrinsicRelaxivityRate.getText();
        rib = Double.parseDouble(tmpStr);
        
        if (rib < ribMin) {
            MipavUtil.displayError("Blood intrinsic relaxivity rate must be at least " + Double.toString(ribMin));
            textBloodIntrinsicRelaxivityRate.requestFocus();
            textBloodIntrinsicRelaxivityRate.selectAll();
            return false;
        }
        
        if (rib > ribMax) {
            MipavUtil.displayError("Blood intrinsic relaxivity rate must not exceed " + Double.toString(ribMax));
            textBloodIntrinsicRelaxivityRate.requestFocus();
            textBloodIntrinsicRelaxivityRate.selectAll(); 
            return false;
        }
        rib = 1.0/rib;
        
        if (constantTissueRadioButton.isSelected()) {
            tmpStr = textTissueIntrinsicRelaxivityRate.getText();
            rit = Double.parseDouble(tmpStr);
            
            if (rit < ritMin) {
                MipavUtil.displayError("Tissue intrinsic relaxivity rate must be at least " + Double.toString(ritMin));
                textTissueIntrinsicRelaxivityRate.requestFocus();
                textTissueIntrinsicRelaxivityRate.selectAll();
                return false;
            }
            
            if (rit > ritMax) {
                MipavUtil.displayError("Tissue intrinsic relaxivity rate must not exceed " + Double.toString(ritMax));
                textTissueIntrinsicRelaxivityRate.requestFocus();
                textTissueIntrinsicRelaxivityRate.selectAll(); 
                return false;
            }
            rit = 1.0/rit;    
        } // (constantTissueRadioButton.isSelected())
        
        if (fileTissueRadioButton.isSelected()) {
            fileNameTissue = textTissueFile.getText();  
            try {
                FileIO fileIO = new FileIO();
                tissueImage = fileIO.readImage(fileNameTissue, directoryTissue, false, null);

                if (tissueImage == null) {
                    MipavUtil.displayError("Tissue image is not valid.");

                    return false;
                } else if (tissueImage.getNDims() != image.getNDims()) {
                    MipavUtil.displayError("Dimensions of tissue image must match the source image.");

                    return false;
                }

                for (int i = 0; i < tissueImage.getNDims(); i++) {

                    if (image.getExtents()[i] != tissueImage.getExtents()[i]) {
                        MipavUtil.displayError("Dimensions of tissue image must match the source image.");

                        return false;
                    }
                }
                int length = tissueImage.getExtents()[0];
                for (int i = 1; i < tissueImage.getNDims(); i++) {
                    length *= tissueImage.getExtents()[i];
                }
                r1i = new double[length];
                try {
                    tissueImage.exportData(0, length, r1i);
                }
                catch(IOException e) {
                    MipavUtil.displayError("IOException on tissueImage.exportData(0, length, r1i)");
                    return false;
                }
                tissueImage.disposeLocal();
                tissueImage = null;

            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogDEMRI3");

                return false;
            }
        } // if (fileTissueRadioButton.isSelected())

        tmpStr = textFlipAngle.getText();
        theta = Double.parseDouble(tmpStr);
        
        if (theta < thetaMin) {
            MipavUtil.displayError("Flip angle must be at least " + Double.toString(thetaMin));
            textFlipAngle.requestFocus();
            textFlipAngle.selectAll();
            return false;
        }
        
        if (theta > thetaMax) {
            MipavUtil.displayError("Flip angle must not exceed " + Double.toString(thetaMax));
            textFlipAngle.requestFocus();
            textFlipAngle.selectAll();  
            return false;
        }
        
        tmpStr = textTimeBetweenShots.getText();
        tr = Double.parseDouble(tmpStr);
        
        if (tr < trMin) {
            MipavUtil.displayError("Time between shots must be at least " + Double.toString(trMin));
            textTimeBetweenShots.requestFocus();
            textTimeBetweenShots.selectAll();
            return false;
        }
        
        if (tr > trMax) {
            MipavUtil.displayError("Time between shots must not exceed " + Double.toString(trMax));
            textTimeBetweenShots.requestFocus();
            textTimeBetweenShots.selectAll(); 
            return false;
        }
        
        tmpStr = textTimeBetweenFrames.getText();
        tf = Double.parseDouble(tmpStr);
        
        if (tf < tfMin) {
            MipavUtil.displayError("Time between frames (volumes) must be at least " + Double.toString(tfMin));
            textTimeBetweenFrames.requestFocus();
            textTimeBetweenFrames.selectAll();
            return false;
        }
        
        if (tf > tfMax) {
            MipavUtil.displayError("Time between frames (volumes) must not exceed " + Double.toString(tfMax));
            textTimeBetweenFrames.requestFocus();
            textTimeBetweenFrames.selectAll(); 
            return false;
        }
        
        if (secondButton.isSelected()) {
            perMin = false;
        }
        else {
            perMin = true;
        }
        
        fileNameMp = textMpFile.getText();
        fileMp = new File(directoryMp + fileNameMp);
        
        try {
            br = new BufferedReader(new InputStreamReader(new FileInputStream(fileMp)));
        }
        catch (FileNotFoundException e) {
            MipavUtil.displayError((directoryMp + fileNameMp) + " was not found");
            return false;
        }
        
        // Port of mri_read_1D_stdin
        // Read lines until first character is not blank and not #
        int ii = 0;
        String line = null;
        do {
            try {
                // Contains the contents of the line not including line termination characters
                line = br.readLine();  
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException on br.readLine");
                return false;
            }
            // have reached end of stream
            if (line == null) {
                MipavUtil.displayError("Have reached end of stream on br.readLine");
                return false;
            }
            for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++);
        } while ((ii == line.length()) || (line.charAt(ii) == '#'));

        int start = 0;
        int end = 0;
        double val[] = new double[10000];
        int nval = 0;
        while (true) {
            for (; (Character.isSpaceChar(line.charAt(start))); start++);
            end = start;
            for (; ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
                           (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
                           (line.charAt(end) == '+') || (line.charAt(end) == '-')); end++);
            if (start == end) {
                break;
            }
            val[nval++] = Double.valueOf(line.substring(start, end)).doubleValue();
            if (nval == 10000) {
                break;
            }
            start = end;
            if (line.charAt(start) == ',') {
                start++;
            }
            if (start == line.length()) {
                break;
            }
        } // while (true)
        if (nval < 1) {
            MipavUtil.displayError("No double values found in " + fileNameMp);
            return false;
        }
        
        nx = nval;
        ny = 1;
        double far[] = new double[nx];
        for (ii = 0; ii < nx; ii++) {
            far[ii] = val[ii];
        }
        double far2[] = null;
        
        while (true) {
            try {
                // Contains the contents of the line not including line termination characters
                line = br.readLine();  
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException on br.readLine");
                return false;
            }
            // have reached end of stream
            if (line == null) {
                // done
                break;
            }
            for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++); 
            if ((ii == line.length()) || (line.charAt(ii) == '#')) {
                continue;
            }
            
            // Set input buffer to zero
            for (ii = 0; ii < nx; ii++) {
                val[ii] = 0.0;
            }
            nval = 0;
            start = 0;
            while (true) {
                for (; (Character.isSpaceChar(line.charAt(start))); start++);
                end = start;
                for (; ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
                               (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
                               (line.charAt(end) == '+') || (line.charAt(end) == '-')); end++);
                if (start == end) {
                    break;
                }
                val[nval++] = Double.valueOf(line.substring(start, end)).doubleValue();
                if (nval == nx) {
                    break;
                }
                start = end;
                if (line.charAt(start) == ',') {
                    start++;
                }
                if (start == line.length()) {
                    break;
                }
            } // while (true)
            far2 = new double[far.length];
            for (ii = 0; ii < far.length; ii++) {
                far2[ii] = far[ii];
            }
            far = new double[(ny+1)*nx];
            for (ii = 0; ii < far2.length; ii++) {
                far[ii] = far2[ii];
            }
            for (ii = 0; ii < nx; ii++) {
                far[ny*nx + ii] = val[ii];    
            }
            ny++;
        } // while (true)
        
        int jj;
        mcp = new double[far.length];
        int temp;
        if (ny > 1) {
            // more than one row ==> transpose (the usual case)
            for (jj = 0; jj < ny; jj++) {
                for (ii = 0; ii < nx; ii++) {
                    mcp[jj + ii * ny] = far[ii + jj * nx];
                }
            }
            temp = nx;
            nx = ny;
            ny = temp;
        } // if (ny > 1)
        else {
            for (ii = 0; ii < far.length; ii++) {
                mcp[ii] = far[ii];
            }
        }
        
        mp_len = nx;
        Preferences.debug("mcp array is : \n");
        for (ii = 0; ii < mp_len; ii++) {
            Preferences.debug("mcp[" + ii + "] = " + mcp[ii] + "\n");
        }
        Preferences.debug("\n");
        
        tmpStr = textNFirst.getText();
        nFirst = Integer.parseInt(tmpStr);
        
        if (nFirst < nFirstMin) {
            MipavUtil.displayError("nfirst must be >= " + nFirstMin);
            textNFirst.requestFocus();
            textNFirst.selectAll();
            return false;
        }
        if (nFirst > nFirstMax) {
            MipavUtil.displayError("nfirst must not exceed " + nFirstMax);
            textNFirst.requestFocus();
            textNFirst.selectAll();
            return false;
        }
        
        if (kepButton.isSelected()) {
            useVe = false;
        }
        else {
            useVe = true;
        }
        return true;
    }

}
