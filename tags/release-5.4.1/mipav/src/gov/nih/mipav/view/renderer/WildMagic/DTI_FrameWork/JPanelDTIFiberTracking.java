package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTI2EGFA;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTITract;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

public class JPanelDTIFiberTracking extends JPanel implements ActionListener {
    
	public static final String TraceImageName = "TraceImage";
	public static final String RAImageName = "RAImage";
	public static final String VRImageName = "VolumeRatioImage";
	public static final String ADCImageName = "ADCImage";
	public static final String EigenValueImageName = "EigenValueImage";
	public static final String EigenVectorImageName = "EigenVectorImage";
	public static final String FAImageName = "FAImage";
	public static final String ColorMapImageName = "ColorMapImage";
	public static final String TrackFileName = "TractData.bin";

    public static void createFrame()
    {
    	JDialog dialog = new JDialog( ViewUserInterface.getReference().getMainFrame(), "Fiber tracking / Statistics" );
    	dialog.add( new JPanelDTIFiberTracking(dialog, null) );
    	dialog.pack();
    	dialog.setVisible(true);
    }
    
	/** Parent dialog, when this panel is created as a stand-alone dialog: * */
    private JDialog parentFrame;
    /** Parent DTI pipeline framework */
    private DTIPipeline pipeline;
    
    private JTextField tensorImageTextField, outputDirTextField;

    /** current directory * */
    private String currDir = null;

    private ModelImage tensorImage, eigenVectorImage, FAImage, eigenValueImage, rgbImage, traceImage, raImage,
            vrImage, adcImage;

    private JCheckBox negXCheckBox;

    private JCheckBox negYCheckBox;

    private JCheckBox negZCheckBox;

    private JTextField faMinThresholdTextField;

    private JTextField faMaxThresholdTextField;


    private JTextField maxAngleTextField;

    /**
     * Constructs the Fiber Tracking input panel:
     */
    public JPanelDTIFiberTracking(DTIPipeline pipeline) {
        super();
        this.pipeline = pipeline;
        init(false);
    }
    
    /**
     * Constructs the Fiber Tracking input panel:
     */
    public JPanelDTIFiberTracking(JDialog parent, DTIPipeline pipeline) {
        super();
        this.parentFrame = parent;
        this.pipeline = pipeline;
        init(true);
    }
    
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();

        if ( command.equals("Cancel" ) && (parentFrame != null) )
        {
        	parentFrame.dispose();
        }
        else if ( command.equals("OK" ) && (tensorImage != null) && (outputDirTextField.getText() != null) )
        {
        	createDerivedImages();
        	parentFrame.dispose();
        }
        else if (command.equalsIgnoreCase("tensorBrowse")) {
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
                outputDirTextField.setText(tensorImage.getImageDirectory());
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
        }
        if ( (pipeline != null) && (tensorImage != null) && (outputDirTextField.getText() != null) )
        {
        	pipeline.nextButton.setEnabled(true);
        }
    }
    
    /**
     * Creates the images derived from the tensor image. The following images are generated:
     * eigen vector image with eigen vectors
     * eigen value image with eigen values
     * functional anisotropy image
     * trace image
     * ra image
     * volume ratio image
     * adc image
     * rgb color image displaying the eigen vectors weighted by the functional anisotropy as RGB.
     */
    public void createDerivedImages()
    {
        boolean success = validateData();
        if ( !success) {
            return;
        }

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        calcEigenVectorImage();

        createRGBImage();

        trackFibers();

        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }
    
    /**
     * Creates a color map image by combing the three eigen values into an RGB image and multiplying by the functional anisotropy. 
     */
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
        final int anisLength = FAImage.getExtents()[0] * FAImage.getExtents()[1]
                * FAImage.getExtents()[2];
        anisotropyBuffer = new float[anisLength];
        try {
            FAImage.exportData(0, anisLength, anisotropyBuffer);
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
        rgbImage.setImageName( ColorMapImageName );
		ModelImage.saveImage( rgbImage, rgbImage.getImageName() + ".xml", outputDirTextField.getText() );
        // new ViewJFrameImage(eigenVectorImage);
        // new ViewJFrameImage(FAImage);
        // new ViewJFrameImage(rgbImage);

        // return rgbImage;
    }
    public ModelImage getColorMapImage()
    {
    	return rgbImage;
    }
    public ModelImage getEigenValueImage()
    {
    	return eigenValueImage;
    }
    
    public ModelImage getEigenVectorImage()
    {
    	return eigenVectorImage;
    }
    
    public ModelImage getFAImage()
    {
    	return FAImage;
    }

    /**
     * Returns the output directory for the derived image calculations.
     * @return output directory for the derived image calculations.
     */
    public String getOutputDirectory()
    {
    	return outputDirTextField.getText();
    }
    
    /**
     * Return the tensor image.
     * @return tensor image.
     */
    public ModelImage getTensorImage()
    {
    	return tensorImage;
    }

    public void setInputImage( ModelImage image )
    {
    	tensorImage = image;
    	tensorImageTextField.setText( image.getImageName() );
    	outputDirTextField.setText( image.getImageDirectory() );
    }

    public void windowClosing(final WindowEvent event) {
        cleanup();

    }
    
    
    private TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                                Color.black);
    }

    /** Calls AlgorithmDTI2EGFA to create an Apparent Diffusion Coefficient Image, 
     * Functional Anisotropy Image, Color Image, Eigen Value Image, Eigen Vector Image, Relative Anisotropy Image,
     * Trace Image, and Volume Ratio Image. */
     private void calcEigenVectorImage() {

        AlgorithmDTI2EGFA kAlgorithm = new AlgorithmDTI2EGFA(tensorImage);
        kAlgorithm.run();
        
        traceImage = kAlgorithm.getTraceImage();
        traceImage.setImageName( TraceImageName );
		ModelImage.saveImage( traceImage, traceImage.getImageName() + ".xml", outputDirTextField.getText() );
		traceImage.disposeLocal();
		
        raImage = kAlgorithm.getRAImage();
        raImage.setImageName( RAImageName );
		ModelImage.saveImage( raImage, raImage.getImageName() + ".xml", outputDirTextField.getText() );
		raImage.disposeLocal();		
		
        vrImage = kAlgorithm.getVRImage();
        vrImage.setImageName( VRImageName );
		ModelImage.saveImage( vrImage, vrImage.getImageName() + ".xml", outputDirTextField.getText() );
		vrImage.disposeLocal();
		
        adcImage = kAlgorithm.getADCImage();
        adcImage.setImageName( ADCImageName );
		ModelImage.saveImage( adcImage, adcImage.getImageName() + ".xml", outputDirTextField.getText() );
		adcImage.disposeLocal();
		
		// The remaining images the calling function must close:
        eigenValueImage = kAlgorithm.getEigenValueImage();
        eigenValueImage.setImageName( EigenValueImageName );
		ModelImage.saveImage( eigenValueImage, eigenValueImage.getImageName() + ".xml", outputDirTextField.getText() );

        eigenVectorImage = kAlgorithm.getEigenVectorImage();
        eigenVectorImage.setImageName( EigenVectorImageName );
		ModelImage.saveImage( eigenVectorImage, eigenVectorImage.getImageName() + ".xml", outputDirTextField.getText() );

        FAImage = kAlgorithm.getFAImage();
        FAImage.setImageName( FAImageName );
		ModelImage.saveImage( FAImage, FAImage.getImageName() + ".xml", outputDirTextField.getText() );
        kAlgorithm.disposeLocal();
        kAlgorithm = null;
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
        if (FAImage != null) {
            FAImage.disposeLocal();
            FAImage = null;
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

    private void init(boolean bStandAlone) {
        setForeground(Color.black);


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

        if ( bStandAlone )
        {
        	JPanel OKCancelPanel = new JPanel();
        	JButton OKButton = new JButton("OK");
        	OKButton.setActionCommand("OK");
            OKButton.addActionListener(this);
        	OKCancelPanel.add(OKButton, BorderLayout.WEST);
        	JButton cancelButton = new JButton("Cancel");
        	cancelButton.setActionCommand("Cancel");
            cancelButton.addActionListener(this);
        	OKCancelPanel.add(cancelButton, BorderLayout.EAST);
            gbc.gridy = 3;
            gbc.gridx = 0;
            gbc.gridwidth = 3;
            mainPanel.add(OKCancelPanel, gbc);
        	//getContentPane().add(mainPanel, BorderLayout.CENTER);
        	//getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        }
        this.add(mainPanel);
        setVisible(true);

    }

    /**
     * Calls AlgorithmDTITRACT to calculate fiber bundle tracts 
     */
    private void trackFibers() {
        final float fFAMin = Float.valueOf(faMinThresholdTextField.getText()).floatValue();
        final float fFAMax = Float.valueOf(faMaxThresholdTextField.getText()).floatValue();
        final float fMaxAngle = Float.valueOf(maxAngleTextField.getText()).floatValue();
        AlgorithmDTITract kTractAlgorithm = new AlgorithmDTITract(tensorImage, FAImage, eigenVectorImage,
                eigenValueImage, outputDirTextField.getText() + File.separator + TrackFileName, negXCheckBox
                        .isSelected(), negYCheckBox.isSelected(), negZCheckBox.isSelected(), fFAMin, fFAMax, fMaxAngle);
        kTractAlgorithm.run();
        kTractAlgorithm.disposeLocal();
        kTractAlgorithm = null;
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

}


