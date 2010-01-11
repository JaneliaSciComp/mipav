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
import java.io.RandomAccessFile;
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
    
    private RandomAccessFile raFile;

    /** DOCUMENT ME! */
    private int backgroundIndex = -1;

    /** DOCUMENT ME! */
    private int cost;


    /** DOCUMENT ME! */
    private boolean createRegImage = false;


    /** DOCUMENT ME! */
    private int donorIndex = -1;


    /** DOCUMENT ME! */
    private AlgorithmFRETAcceptorPhotobleach fretAlgo = null;

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


        if (fretAlgo.isCompleted() == true) { }

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

            fretAlgo = new AlgorithmFRETAcceptorPhotobleach(image, postImage, useRed, useGreen, useBlue, donorIndex,
                                                            backgroundIndex, signalIndex, register, cost,
                                                            createRegImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            fretAlgo.addListener(this);

            createProgressBar(image.getImageName(), fretAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (fretAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                fretAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog FRET: unable to allocate enough memory");

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
        
        labelContrastRelaxivityRate = new JLabel("Contrast relaxivity rate in 1/(mMol*sec) (0.0 - 1000.0)");
        labelContrastRelaxivityRate.setForeground(Color.black);
        labelContrastRelaxivityRate.setFont(serif12);
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
        gbc.gridy = 1;
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
        gbc.gridy = 2;
        gbc.gridheight = 1;
        mainPanel.add(labelBloodIntrinsicRelaxivityRate2, gbc);
        
        tissueGroup = new ButtonGroup();
        constantTissueRadioButton = new JRadioButton("Constant tissue intrinsic relaxivity rate", true);
        constantTissueRadioButton.setFont(serif12);
        constantTissueRadioButton.setForeground(Color.black);
        constantTissueRadioButton.addActionListener(this);
        tissueGroup.add(constantTissueRadioButton);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(constantTissueRadioButton, gbc);
        
        fileTissueRadioButton = new JRadioButton("File specified tissue intrinsic relaxivity rate", false);
        fileTissueRadioButton.setFont(serif12);
        fileTissueRadioButton.setForeground(Color.black);
        fileTissueRadioButton.addActionListener(this);
        tissueGroup.add(fileTissueRadioButton);
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(fileTissueRadioButton, gbc);
        
        labelTissueIntrinsicRelaxivityRate = new JLabel("Tissue intrinsic relaxivity rate in 1/(mMol * sec)");
        labelTissueIntrinsicRelaxivityRate.setForeground(Color.black);
        labelTissueIntrinsicRelaxivityRate.setFont(serif12);
        labelTissueIntrinsicRelaxivityRate.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 5;
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
        gbc.gridy = 6;
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
        gbc.gridy = 7;
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
        gbc.gridy = 8;
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
        gbc.gridy = 9;
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
        gbc.gridy = 10;
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
        gbc.gridy = 11;
        mainPanel.add(secondButton, gbc);
        
        minuteButton = new JRadioButton("K_trans and k_ep are per minute", false);
        minuteButton.setFont(serif12);
        minuteButton.setForeground(Color.black);
        rateGroup.add(minuteButton);
        gbc.gridx = 0;
        gbc.gridy = 12;
        mainPanel.add(minuteButton, gbc);
        
        buttonMpFile = new JButton("Choose 1D Mp(t) data file");
        buttonMpFile.setForeground(Color.black);
        buttonMpFile.setFont(serif12B);
        buttonMpFile.addActionListener(this);
        buttonMpFile.setActionCommand("MpFile");
        buttonMpFile.setPreferredSize(new Dimension(145, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridy = 13;
        mainPanel.add(buttonMpFile, gbc);

        textMpFile = new JTextField();
        textMpFile.setFont(serif12);
        textMpFile.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textMpFile, gbc);


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

        if (fileMp.exists() == false) {
            MipavUtil.displayError(fileNameMp + " does not exist");
            return false;
        }
        
        try {
            raFile = new RandomAccessFile(fileMp, "r");
        }
        catch(FileNotFoundException e) {
            MipavUtil.displayError("File not found exception on fileMp");
            return false;
        }

        return true;
    }

}
