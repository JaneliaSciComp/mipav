package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import javax.swing.*;


public class JDialogDTIFiberTracking extends JDialogBase implements WindowListener {

    private JTextField tensorImageTextField, outputDirTextField;

    /** current directory * */
    private String currDir = null;

    private ModelImage tensorImage, eigenVectorImage, anisotropyImage, eigenValueImage, rgbImage, traceImage, raImage,
            vrImage, adcImage;

    private JCheckBox negXCheckBox;

    private JCheckBox negYCheckBox;

    private JCheckBox negZCheckBox;

    private JTextField faMinThresholdTextField;

    private JTextField faMaxThresholdTextField;

    private JTextField maxAngleTextField;

    public JDialogDTIFiberTracking() {
        super(ViewUserInterface.getReference().getMainFrame(), false);
        init();
    }

    private void init() {
        setForeground(Color.black);
        setTitle("Fiber Tracking / Statistics");

        final GridBagConstraints gbc = new GridBagConstraints();

        final JPanel mainPanel = new JPanel(new GridBagLayout());

        final JLabel tensorImageLabel = new JLabel("Tensor Image");
        final JLabel outputDirLabel = new JLabel("Output Dir");

        tensorImageTextField = new JTextField(20);
        tensorImageTextField.setEditable(false);
        tensorImageTextField.setBackground(Color.white);

        outputDirTextField = new JTextField(20);
        outputDirTextField.setEditable(false);
        outputDirTextField.setBackground(Color.white);

        final JButton tensorBrowseButton = new JButton("Browse");
        tensorBrowseButton.addActionListener(this);
        tensorBrowseButton.setActionCommand("tensorBrowse");

        final JButton outputDirBrowseButton = new JButton("Browse");
        outputDirBrowseButton.addActionListener(this);
        outputDirBrowseButton.setActionCommand("outputDirBrowse");

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15, 5, 5, 15);
        gbc.gridwidth = 1;

        mainPanel.add(tensorImageLabel, gbc);

        gbc.gridx = 1;
        mainPanel.add(tensorImageTextField, gbc);

        gbc.gridx = 2;
        mainPanel.add(tensorBrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;

        mainPanel.add(outputDirLabel, gbc);

        gbc.gridx = 1;
        mainPanel.add(outputDirTextField, gbc);

        gbc.gridx = 2;
        mainPanel.add(outputDirBrowseButton, gbc);

        negXCheckBox = new JCheckBox("+/- x");
        negXCheckBox.setSelected(false);
        negXCheckBox.addActionListener(this);
        negXCheckBox.setActionCommand("NegX");
        negXCheckBox.setEnabled(true);

        negYCheckBox = new JCheckBox("+/- y");
        negYCheckBox.setSelected(false);
        negYCheckBox.addActionListener(this);
        negYCheckBox.setActionCommand("NegY");
        negYCheckBox.setEnabled(true);

        negZCheckBox = new JCheckBox("+/- z");
        negZCheckBox.setSelected(true);
        negZCheckBox.addActionListener(this);
        negZCheckBox.setActionCommand("NegZ");
        negZCheckBox.setEnabled(true);

        final JPanel kVectorPanel = new JPanel();
        kVectorPanel.setLayout(new BoxLayout(kVectorPanel, BoxLayout.X_AXIS));
        kVectorPanel.add(negXCheckBox);
        kVectorPanel.add(negYCheckBox);
        kVectorPanel.add(negZCheckBox);

        faMinThresholdTextField = new JTextField("0.0", 4);
        faMinThresholdTextField.setActionCommand("FAMINChanged");
        faMinThresholdTextField.addActionListener(this);
        faMaxThresholdTextField = new JTextField("1.0", 4);
        faMaxThresholdTextField.setActionCommand("FAMAXChanged");
        faMaxThresholdTextField.addActionListener(this);
        maxAngleTextField = new JTextField("45", 4);
        maxAngleTextField.setActionCommand("MaxAngleChanged");
        maxAngleTextField.addActionListener(this);
        final JPanel kTrackPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        kTrackPanel.add(new JLabel("FA Threshold Min (0.0-1.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add(faMinThresholdTextField, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        kTrackPanel.add(new JLabel("FA Threshold Max (0.0-1.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add(faMaxThresholdTextField, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        kTrackPanel.add(new JLabel("Maximum Angle (0.0-180.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add(maxAngleTextField, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        final JPanel kTractOPtionsPanel = new JPanel();
        kTractOPtionsPanel.setLayout(new GridBagLayout());
        kTractOPtionsPanel.add(kVectorPanel, gbc);
        gbc.gridy = 1;
        kTractOPtionsPanel.add(kTrackPanel, gbc);
        kTractOPtionsPanel.setBorder(buildTitledBorder("Fiber Track Recontruction Options"));

        gbc.gridy = 2;
        gbc.gridx = 0;
        gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(kTractOPtionsPanel, gbc);

        final JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        pack();
        setMinimumSize(getSize());
        // setResizable(false);
        setVisible(true);

    }

    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();

        if (command.equalsIgnoreCase("tensorBrowse")) {
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
                fileIO.setQuiet(true);
                tensorImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                        + File.separator, true, null);
                if (tensorImage.getNDims() != 4) {
                    MipavUtil.displayError("Tensor Image must be a 4D image");
                    tensorImage.disposeLocal();
                    tensorImage = null;
                    return;

                }

                tensorImageTextField.setText(currDir);
            }
        } else if (command.equals("outputDirBrowse")) {
            final JFileChooser chooser = new JFileChooser();

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose dir");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                outputDirTextField.setText(currDir);
            }
        } else if (command.equals("ok")) {
            boolean success = validateData();
            if ( !success) {
                return;
            }

            setCursor(new Cursor(Cursor.WAIT_CURSOR));

            calcEigenVectorImage();

            createRGBImage();

            trackFibers();

            MipavUtil.displayInfo("Fiber file and statistics saved under  " + outputDirTextField.getText());

            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

            cleanup();

        } else if (command.equals("cancel")) {
            cleanup();
            dispose();
        }

    }

    /** Calls AlgorithmDTI2EGFA to create eigen vector and functional anisotropy images. */
    private void calcEigenVectorImage() {
        final int[] extents = tensorImage.getExtents();
        float[] res = tensorImage.getFileInfo(0).getResolutions();
        final float[] saveRes = new float[] {res[0], res[1], res[2], res[3]};

        /*
         * if ( m_bUseXRes ) { saveRes[0] = m_fResX; } if ( m_bUseYRes ) { saveRes[1] = m_fResY; } if ( m_bUseZRes ) {
         * saveRes[2] = m_fResZ; }
         */

        final float[] newRes = new float[extents.length];
        final int[] volExtents = new int[extents.length];
        boolean originalVolPowerOfTwo = true;
        int volSize = 1;
        for (int i = 0; i < extents.length; i++) {
            volExtents[i] = MipavMath.dimPowerOfTwo(extents[i]);
            volSize *= volExtents[i];

            if ( (i < 3) && volExtents[i] != extents[i]) {
                originalVolPowerOfTwo = false;
            }
            newRes[i] = (res[i] * (extents[i])) / (volExtents[i]);
            saveRes[i] = (saveRes[i] * (extents[i])) / (volExtents[i]);
        }

        if ( !originalVolPowerOfTwo) {
            AlgorithmTransform transformFunct = new AlgorithmTransform(tensorImage, new TransMatrix(4),
                    AlgorithmTransform.TRILINEAR, newRes[0], newRes[1], newRes[2], volExtents[0], volExtents[1],
                    volExtents[2], false, true, false);
            transformFunct.setRunningInSeparateThread(false);
            transformFunct.run();

            if (transformFunct.isCompleted() == false) {
                transformFunct.finalize();
                transformFunct = null;
            }

            final ModelImage kDTIImageScaled = transformFunct.getTransformedImage();
            kDTIImageScaled.calcMinMax();

            if (transformFunct != null) {
                transformFunct.disposeLocal();
            }
            transformFunct = null;

            tensorImage.disposeLocal();
            tensorImage = null;
            tensorImage = kDTIImageScaled;

            res = tensorImage.getFileInfo(0).getResolutions();
        }

        AlgorithmDTI2EGFA kAlgorithm = new AlgorithmDTI2EGFA(tensorImage);
        kAlgorithm.run();
        eigenVectorImage = kAlgorithm.getEigenImage();
        anisotropyImage = kAlgorithm.getFAImage();
        traceImage = kAlgorithm.getTraceImage();
        traceImage.saveImage(outputDirTextField.getText() + File.separator, "TraceImage.xml", FileUtility.XML, false);
        raImage = kAlgorithm.getRAImage();
        raImage.saveImage(outputDirTextField.getText() + File.separator, "RAImage.xml", FileUtility.XML, false);
        vrImage = kAlgorithm.getVRImage();
        vrImage
                .saveImage(outputDirTextField.getText() + File.separator, "VolumeRatioImage.xml", FileUtility.XML,
                        false);
        adcImage = kAlgorithm.getADCImage();
        adcImage.saveImage(outputDirTextField.getText() + File.separator, "ADCImage.xml", FileUtility.XML, false);
        eigenValueImage = kAlgorithm.getEigenValueImage();
        eigenValueImage.saveImage(outputDirTextField.getText() + File.separator, "EigenValueImage.xml",
                FileUtility.XML, false);
        // tensorImage.saveImage(outputDirTextField.getText(), "DTIImage.xml", FileUtility.XML, true);
        eigenVectorImage.saveImage(outputDirTextField.getText() + File.separator, "EigenVectorImage.xml",
                FileUtility.XML, true);
        anisotropyImage.saveImage(outputDirTextField.getText() + File.separator, "AnisotropyImage.xml",
                FileUtility.XML, true);
        kAlgorithm.disposeLocal();
        kAlgorithm = null;

        // The resolutions should be reset after the FiberTracts are calculated (See JDialogDTIInput.java)
        /*
         * if ( m_bUseXRes || m_bUseYRes || m_bUseZRes ) { for ( int i = 0; i < tensorImage.getFileInfo().length; i++ ) {
         * tensorImage.getFileInfo(i).setResolutions(saveRes); tensorImage.getFileInfo(i).setSliceThickness(saveRes[2]); } }
         */
    }

    public void createRGBImage() {
        // gamma factor
        final float gamma = 1.8f;

        // create the dest extents of the dec image...the 4th dim will only have 3 as the value
        int[] destExtents = new int[4];
        destExtents[0] = eigenVectorImage.getExtents()[0];
        destExtents[1] = eigenVectorImage.getExtents()[1];
        destExtents[2] = eigenVectorImage.getExtents()[2];
        destExtents[3] = 3;

        ModelImage decImage = new ModelImage(ModelStorageBase.FLOAT, destExtents, ModelImage.makeImageName(
                eigenVectorImage.getImageName(), "_DEC"));

        // buffer
        float[] buffer;

        // determine length of dec image
        final int length = eigenVectorImage.getExtents()[0] * eigenVectorImage.getExtents()[1]
                * eigenVectorImage.getExtents()[2] * 3;
        buffer = new float[length];

        // export eigvecSrcImage into buffer based on length
        try {
            eigenVectorImage.exportData(0, length, buffer);
        } catch (final IOException error) {
            System.out.println("IO exception");
            // return null;
        }

        // lets first do absolute value for each value in the buffer
        for (int i = 0; i < buffer.length; i++) {
            buffer[i] = Math.abs(buffer[i]);
        }

        // import resultBuffer into decImage
        try {
            decImage.importData(0, buffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");

            // return null;
        }

        // extract dec image into channel images
        destExtents = new int[3];
        destExtents[0] = decImage.getExtents()[0];
        destExtents[1] = decImage.getExtents()[1];
        destExtents[2] = decImage.getExtents()[2];
        final ModelImage[] channelImages = new ModelImage[decImage.getExtents()[3]];
        for (int i = 0; i < decImage.getExtents()[3]; i++) {
            final int num = i + 1;
            final String resultString = ModelImage.makeImageName(decImage.getImageName(), "_Vol=" + num);
            channelImages[i] = new ModelImage(decImage.getType(), destExtents, resultString);
            final AlgorithmSubset subsetAlgo = new AlgorithmSubset(decImage, channelImages[i],
                    AlgorithmSubset.REMOVE_T, i);
            subsetAlgo.setRunningInSeparateThread(false);
            subsetAlgo.run();
        }

        decImage.disposeLocal();
        decImage = null;

        // set up result image
        rgbImage = new ModelImage(ModelStorageBase.ARGB_FLOAT, channelImages[0].getExtents(), ModelImage.makeImageName(
                eigenVectorImage.getImageName(), "_ColorDisplay"));

        // cocatenate channel images into an RGB image
        final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(channelImages[0], channelImages[1],
                channelImages[2], rgbImage, false, true, 255.0f, false);
        mathAlgo.setRunningInSeparateThread(false);
        mathAlgo.run();

        channelImages[0].disposeLocal();
        channelImages[0] = null;
        channelImages[1].disposeLocal();
        channelImages[1] = null;
        channelImages[2].disposeLocal();
        channelImages[2] = null;

        // copy core file info over
        final FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[rgbImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(rgbImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(eigenVectorImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(eigenVectorImage.getFileInfo()[0].getUnitsOfMeasure());
            // fileInfoBases[i].setResolutions(eigenVectorImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setResolutions(tensorImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(rgbImage.getExtents());
            fileInfoBases[i].setImageOrientation(tensorImage.getFileInfo()[0].getImageOrientation());
            fileInfoBases[i].setAxisOrientation(tensorImage.getFileInfo()[0].getAxisOrientation());
            fileInfoBases[i].setOrigin(eigenVectorImage.getFileInfo()[0].getOrigin());
            fileInfoBases[i].setPixelPadValue(eigenVectorImage.getFileInfo()[0].getPixelPadValue());
            fileInfoBases[i].setPhotometric(eigenVectorImage.getFileInfo()[0].getPhotometric());
            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);
            fileInfoBases[i].setFileDirectory(eigenVectorImage.getFileInfo()[0].getFileDirectory());
        }

        rgbImage.setFileInfo(fileInfoBases);

        // now we need to weight the result image by anisotopy

        float[] rgbBuffer;
        // determine length of dec image
        final int rgbBuffLength = rgbImage.getExtents()[0] * rgbImage.getExtents()[1] * rgbImage.getExtents()[2] * 4;
        rgbBuffer = new float[rgbBuffLength];

        // export eigvecSrcImage into buffer based on length
        try {
            rgbImage.exportData(0, rgbBuffLength, rgbBuffer);
        } catch (final IOException error) {
            System.out.println("IO exception");
            // return null;
        }

        float[] anisotropyBuffer;
        final int anisLength = anisotropyImage.getExtents()[0] * anisotropyImage.getExtents()[1]
                * anisotropyImage.getExtents()[2];
        anisotropyBuffer = new float[anisLength];
        try {
            anisotropyImage.exportData(0, anisLength, anisotropyBuffer);
        } catch (final IOException error) {
            System.out.println("IO exception");
            // return null;
        }

        // take r,g,and b and weight by anisotropy and gamma...and rescale to 0-255
        for (int i = 0, j = 0; i < rgbBuffer.length; i = i + 4, j++) {
            rgbBuffer[i + 1] = rgbBuffer[i + 1] * anisotropyBuffer[j];
            rgbBuffer[i + 1] = (float) Math.pow(rgbBuffer[i + 1], (1 / gamma));
            rgbBuffer[i + 1] = rgbBuffer[i + 1] * 255;

            rgbBuffer[i + 2] = rgbBuffer[i + 2] * anisotropyBuffer[j];
            rgbBuffer[i + 2] = (float) Math.pow(rgbBuffer[i + 2], (1 / gamma));
            rgbBuffer[i + 2] = rgbBuffer[i + 2] * 255;

            rgbBuffer[i + 3] = rgbBuffer[i + 3] * anisotropyBuffer[j];
            rgbBuffer[i + 3] = (float) Math.pow(rgbBuffer[i + 3], (1 / gamma));
            rgbBuffer[i + 3] = rgbBuffer[i + 3] * 255;

        }

        try {
            rgbImage.importData(0, rgbBuffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");

            // return null;
        }

        rgbImage.calcMinMax();
        rgbImage.saveImage(outputDirTextField.getText() + File.separator, "colorMapImage.xml", FileUtility.XML, false);
        // new ViewJFrameImage(eigenVectorImage);
        // new ViewJFrameImage(anisotropyImage);
        // new ViewJFrameImage(rgbImage);

        // return rgbImage;
    }

    private void trackFibers() {
        final float fFAMin = Float.valueOf(faMinThresholdTextField.getText()).floatValue();
        final float fFAMax = Float.valueOf(faMaxThresholdTextField.getText()).floatValue();
        final float fMaxAngle = Float.valueOf(maxAngleTextField.getText()).floatValue();
        AlgorithmDTITract kTractAlgorithm = new AlgorithmDTITract(tensorImage, anisotropyImage, eigenVectorImage,
                eigenValueImage, outputDirTextField.getText() + File.separator + "DTIImage.xml_tract", negXCheckBox
                        .isSelected(), negYCheckBox.isSelected(), negZCheckBox.isSelected(), fFAMin, fFAMax, fMaxAngle);
        kTractAlgorithm.run();
        kTractAlgorithm.disposeLocal();
        kTractAlgorithm = null;
    }

    public void windowClosing(final WindowEvent event) {
        super.windowClosing(event);
        cleanup();

    }

    private boolean validateData() {
        final boolean success = true;

        final String outputDirString = outputDirTextField.getText().trim();
        final String tensorImageString = tensorImageTextField.getText().trim();

        if (outputDirString.equals("") || tensorImageString.equals("")) {
            MipavUtil.displayError("Tensor Image and Output Dir are required parameters");
            return false;
        }

        try {
            final float fFAMin = Float.valueOf(faMinThresholdTextField.getText()).floatValue();
            final float fFAMax = Float.valueOf(faMaxThresholdTextField.getText()).floatValue();
            final float fMaxAngle = Float.valueOf(maxAngleTextField.getText()).floatValue();
            if (fFAMin < 0 || fFAMin > 1) {
                MipavUtil.displayError("FA Threshold Min is not in acceptable range");
                return false;
            }
            if (fFAMax < 0 || fFAMax > 1) {
                MipavUtil.displayError("FA Threshold Max is not in acceptable range");
                return false;
            }
            if (fMaxAngle < 0 || fMaxAngle > 180) {
                MipavUtil.displayError("Maximum Angle is not in acceptable range");
                return false;
            }
        } catch (final NumberFormatException e) {
            MipavUtil.displayError("One or more values enteres is not valid");
            return false;
        }

        return success;
    }

    private void cleanup() {
        if (tensorImage != null) {
            tensorImage.disposeLocal();
            tensorImage = null;
        }
        if (eigenVectorImage != null) {
            eigenVectorImage.disposeLocal();
            eigenVectorImage = null;
        }
        if (anisotropyImage != null) {
            anisotropyImage.disposeLocal();
            anisotropyImage = null;
        }
        if (eigenValueImage != null) {
            eigenValueImage.disposeLocal();
            eigenValueImage = null;
        }
        if (rgbImage != null) {
            rgbImage.disposeLocal();
            rgbImage = null;
        }
        if (traceImage != null) {
            traceImage.disposeLocal();
            traceImage = null;
        }
        if (raImage != null) {
            raImage.disposeLocal();
            raImage = null;
        }
        if (vrImage != null) {
            vrImage.disposeLocal();
            vrImage = null;
        }
        if (adcImage != null) {
            adcImage.disposeLocal();
            adcImage = null;
        }
    }

}
