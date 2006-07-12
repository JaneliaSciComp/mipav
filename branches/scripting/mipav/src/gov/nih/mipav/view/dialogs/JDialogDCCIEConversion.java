package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;

import gov.nih.mipav.view.*;

import com.sun.media.codec.video.vcm.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.media.*;
import javax.media.format.*;

import javax.swing.*;


/**
 * <p>Title: JDialogDCCIEConversion</p>
 *
 * <p>Description: Dialog for recursively traversing a directory and converting all 3D DICOM images to compressed AVI
 * (using java or native codecs)</p>
 *
 * <p>Copyright: Copyright (c) 2003</p>
 *
 * <p>Company:</p>
 *
 * @author   Sir Benjamin Link
 * @version  1.0
 */

public class JDialogDCCIEConversion extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8180947616779037172L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JTextField aviField = null;

    /** DOCUMENT ME! */
    private JPanel aviPanel = null;

    /** DOCUMENT ME! */
    private JButton browseButton = null;

    /** DOCUMENT ME! */
    private JButton browseOutputButton = null;

    /** DOCUMENT ME! */
    private int compression = 0; // the compression type

    /** DOCUMENT ME! */
    private JComboBox compressionBox = null;

    /** DOCUMENT ME! */
    private JTextField dirField = null;

    /** DOCUMENT ME! */
    private JComboBox inputBox = null;

    /** DOCUMENT ME! */
    private JPanel inputPanel = null;

    /** DOCUMENT ME! */
    private JRadioButton mjpegButton = null;

    /** DOCUMENT ME! */
    private JRadioButton mp4Button = null;

    /** DOCUMENT ME! */
    private JTextField qualityField = null;

    /** DOCUMENT ME! */
    private JLabel qualityLabel = null;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for running the DICOM to AVI algorithm.
     *
     * @param  ui  User Interface
     */
    public JDialogDCCIEConversion(ViewUserInterface ui) {
        super(false);
        this.userInterface = ui;
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
                setVisible(false);
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Browse")) {
            ViewDirectoryChooser chooser = new ViewDirectoryChooser(this);
            String dir = chooser.getImageDirectory();

            if (dir != null) {

                if (e.getSource() == browseButton) {
                    userInterface.setDefaultDirectory(dir);
                    dirField.setText(dir + File.separator);
                    dir += "_AVI" + File.separator;
                    aviField.setText(dir);
                } else {
                    aviField.setText(dir + File.separator);
                }
            }
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("10067");
        }
    }

    /**
     * Method for catching end of algorithm events.
     *
     * @param  algo  the algorithm that is caught
     */
    public void algorithmPerformed(AlgorithmBase algo) {

        if (algo instanceof AlgorithmDICOMtoAVI) {

            if (algo.isCompleted()) {
                System.err.println("algo completed");
            }
        }
    }

    /**
     * If the M-JPEG option is selected, enable the compression quality field, otherwise turn it off.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source.equals(compressionBox)) {

            if (((String) compressionBox.getSelectedItem()).equals("M-JPEG")) {
                qualityField.setEnabled(true);
                qualityLabel.setEnabled(true);
            } else {
                qualityField.setEnabled(false);
                qualityLabel.setEnabled(false);
            }
        } else if (source.equals(inputBox)) {

            if (((String) inputBox.getSelectedItem()).equals("DICOM")) {
                inputPanel.setBorder(buildTitledBorder("DICOM Input"));
            } else if (((String) inputBox.getSelectedItem()).equals("AVI")) {
                inputPanel.setBorder(buildTitledBorder("AVI Input"));
            }
        }
    }

    /**
     * Method for calling the Dicom to AVI algorithm.
     */
    protected void callAlgorithm() {
        setVisible(false);

        Preferences.debug("Using DICOM input directory: " + dirField.getText() + "\n");
        Preferences.debug("Using AVI output directory: " + aviField.getText() + "\n");
        Preferences.debug("Using " + (String) compressionBox.getSelectedItem() + " compression" + "\n");

        AlgorithmDCCIEConversion algoConvert = new AlgorithmDCCIEConversion(userInterface, dirField.getText(),
                                                                            aviField.getText(), compression,
                                                                            ((String) inputBox.getSelectedItem())
                                                                                .equals("DICOM"));

        /**
         * Only set quality for MJPEG compression
         */
        if (compression == AlgorithmTranscode.TRANSCODE_MJPG) {
            algoConvert.setQuality(Float.parseFloat(qualityField.getText()));
        }

        algoConvert.addListener(this);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (algoConvert.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            if (!userInterface.isAppFrameVisible()) {
                algoConvert.setProgressBarVisible(false);
            }

            algoConvert.run();
        }

    }

    /**
     * Sets up the dialog window and makes it visible.
     */
    private void init() {

        setTitle("DCCIE Image Conversion");

        inputPanel = new JPanel();
        inputPanel.setBorder(buildTitledBorder("AVI Input"));
        inputPanel.setForeground(Color.black);

        dirField = new JTextField(30);
        dirField.setEditable(false);
        dirField.setText(userInterface.getDefaultDirectory());

        browseButton = new JButton("Browse");
        browseButton.addActionListener(this);
        browseButton.setActionCommand("Browse");

        inputBox = new JComboBox();
        inputBox.addItem("AVI");
        inputBox.addItem("DICOM");
        inputBox.addItemListener(this);

        inputPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 4;
        gbc.anchor = gbc.WEST;
        gbc.fill = gbc.HORIZONTAL;
        gbc.weightx = 1.0;
        gbc.insets = new Insets(0, 5, 0, 10);
        inputPanel.add(dirField, gbc);

        gbc.gridx = 4;
        gbc.gridwidth = 1;
        gbc.fill = gbc.NONE;
        gbc.weightx = 0.0;
        gbc.insets = new Insets(0, 0, 0, 5);
        inputPanel.add(browseButton, gbc);

        gbc.gridx = 5;
        gbc.fill = gbc.NONE;
        inputPanel.add(inputBox, gbc);

        aviPanel = new JPanel();
        aviPanel.setBorder(buildTitledBorder("AVI Output"));
        aviPanel.setForeground(Color.black);

        aviPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();

        aviField = new JTextField(30);
        aviField.setEditable(true);

        String outputString = new String(userInterface.getDefaultDirectory());

        if (outputString.endsWith(File.separator)) {
            outputString = outputString.substring(0, outputString.length() - 1);
        }

        outputString += "_AVI" + File.separator;
        aviField.setText(outputString);

        browseOutputButton = new JButton("Browse");
        browseOutputButton.addActionListener(this);
        browseOutputButton.setActionCommand("Browse");

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 4;
        gbc2.anchor = gbc2.WEST;
        gbc2.insets = new Insets(0, 5, 0, 10);
        gbc2.fill = gbc2.HORIZONTAL;
        gbc2.weightx = 1.0;
        aviPanel.add(aviField, gbc2);

        gbc2.gridx = 4;
        gbc2.gridwidth = 1;
        gbc2.fill = gbc2.NONE;
        gbc2.weightx = 0.0;
        gbc2.insets = new Insets(0, 0, 0, 5);
        aviPanel.add(browseOutputButton, gbc2);

        compressionBox = new JComboBox();

        /**
         * Look for native formats (only found if JMF is installed)
         */
        Format[] formats = null;

        try {
            formats = new NativeEncoder().getSupportedOutputFormats(new RGBFormat());
        } catch (UnsatisfiedLinkError ex) {
            System.err.println("JMF library not installed");
        } catch (NoClassDefFoundError ex) {
            System.err.println("JMF library not installed");
        }

        compressionBox.addItem("24 bit uncompressed RGB");
        compressionBox.addItem("8 bit RLE with LUT");
        compressionBox.addItem("M-JPEG");

        if (formats != null) {

            for (int i = 0; i < formats.length; i++) {
                Preferences.debug("JDialogDICOMtoAVI: Encoding options found on computer: " + formats[i].getEncoding() +
                                  "\n");

                if (formats[i].getEncoding().equals("IV32")) {
                    compressionBox.addItem("IR32");
                } else if (formats[i].getEncoding().equals("IV41")) {
                    compressionBox.addItem("IR41");
                } else if (formats[i].getEncoding().equals("IV50")) {
                    compressionBox.addItem("Indeo Video 5");
                    compressionBox.setSelectedItem("Indeo Video 5");
                } else if (formats[i].getEncoding().equals("MPG4")) {
                    compressionBox.addItem("MS-MPEG4 V1");
                } else if (formats[i].getEncoding().equals("MP42")) {
                    compressionBox.addItem("MS-MPEG4 V2");
                } else if (formats[i].getEncoding().equals("DIVX")) {
                    compressionBox.addItem("DivX");
                } else if (formats[i].getEncoding().equals("DX50")) {
                    compressionBox.addItem("DivX 5.0");
                }
            }
        }

        compressionBox.addItemListener(this);

        gbc2.gridx = 0;
        gbc2.gridy = 1;
        gbc2.anchor = gbc2.WEST;
        aviPanel.add(compressionBox, gbc2);

        qualityLabel = new JLabel("M-JPEG Quality (0.01 - 1.00):");
        qualityLabel.setFont(serif12);
        qualityLabel.setEnabled(true);
        gbc2.gridwidth = 1;
        gbc2.gridx = 2;
        aviPanel.add(qualityLabel, gbc2);

        qualityField = new JTextField(3);
        qualityField.setText(".8");

        if (!((String) compressionBox.getSelectedItem()).equals("M-JPEG")) {
            qualityField.setEnabled(false);
            qualityLabel.setEnabled(false);
        }

        MipavUtil.makeNumericsOnly(qualityField, true);

        gbc2.gridx = 3;
        aviPanel.add(qualityField, gbc2);

        JPanel buttonPanel = new JPanel(new FlowLayout());
        buttonPanel.add(buildButtons());

        JPanel twoPanels = new JPanel(new BorderLayout());
        twoPanels.add(inputPanel, BorderLayout.NORTH);
        twoPanels.add(aviPanel, BorderLayout.SOUTH);

        JPanel panel = new JPanel(new BorderLayout());
        panel.add(twoPanels);
        panel.add(buttonPanel, BorderLayout.SOUTH);
        panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(panel);
        pack();
        setVisible(true);
    }


    /**
     * Set up the variables before calling the algorithm. If there is no destination directory or if there is no valid
     * float value for the compression quality (for M-JPEG only), will return false so that the user may correctly set
     * up the algorithm
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {

        if ((aviField.getText() == null) || aviField.getText().equals("")) {
            MipavUtil.displayWarning("Please enter destination directory.");

            return false;
        }

        String compressionStr = (String) compressionBox.getSelectedItem();

        if (compressionStr.equals("IR32")) {
            compression = AlgorithmTranscode.TRANSCODE_IV32;
        } else if (compressionStr.equals("IR41")) {
            compression = AlgorithmTranscode.TRANSCODE_IV41;
        } else if (compressionStr.equals("Indeo Video 5")) {
            compression = AlgorithmTranscode.TRANSCODE_IV50;
        } else if (compressionStr.equals("MS-MPEG4 V1")) {
            compression = AlgorithmTranscode.TRANSCODE_MPG4;
        } else if (compressionStr.equals("MS-MPEG4 V2")) {
            compression = AlgorithmTranscode.TRANSCODE_MP42;
        } else if (compressionStr.equals("M-JPEG")) {
            compression = AlgorithmTranscode.TRANSCODE_MJPG;

            String temp = qualityField.getText();

            try {
                float testFloat = Float.parseFloat(temp);

                if ((testFloat > 1) || (testFloat < .01)) {
                    MipavUtil.displayWarning("Enter a valid value for compression quality (0.01 - 1.00).");

                    return false;
                }
            } catch (Exception ex) {
                MipavUtil.displayWarning("Please enter float value for compression quality.");

                return false;
            }
        } else if (compressionStr.equals("DivX")) {
            compression = AlgorithmTranscode.TRANSCODE_DIVX;
        } else if (compressionStr.equals("DivX 5.0")) {
            compression = AlgorithmTranscode.TRANSCODE_DX50;
        } else if (compressionStr.equals("24 bit uncompressed RGB")) {
            compression = 0;
        } else if (compressionStr.equals("8 bit RLE with LUT")) {
            compression = 1;
        }

        return true;
    }

}
