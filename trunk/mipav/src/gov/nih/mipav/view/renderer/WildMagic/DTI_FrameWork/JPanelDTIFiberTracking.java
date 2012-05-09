package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTI2EGFA;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTIColorDisplay;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTITract;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.DialogDTIColorDisplay;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
    
	private static final long serialVersionUID = 3303854496014433147L;
	
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
    	JDialog dialog = new JDialog( ViewUserInterface.getReference().getMainFrame(), "Tensor Statistics" );
    	dialog.add( new JPanelDTIFiberTracking(dialog, null) );
    	dialog.pack();
    	dialog.setVisible(true);
    }

	/** main panel * */
	private JPanel mainPanel;
    
	/** Parent dialog, when this panel is created as a stand-alone dialog: * */
    private JDialog parentFrame;
    
    /** Parent DTI pipeline framework */
    private DTIPipeline pipeline;
    
    private JTextField textDTIimage, outputDirTextField;

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
    
    private JTextField minTractLengthTextField;


    JCheckBox createADC = new JCheckBox( "Create ADC Image" );
    JCheckBox displayADC = new JCheckBox( "Display ADC Image" );

    JCheckBox createColor = new JCheckBox( "Create Color Image" );
    JCheckBox displayColor = new JCheckBox( "Display Color Image" );

    JCheckBox createEValue = new JCheckBox( "Create Eigen Value Image" );
    JCheckBox displayEValue = new JCheckBox( "Display Eigen Value Image" );

    JCheckBox createEVector = new JCheckBox( "Create Eigen Vector Image" );
    JCheckBox displayEVector = new JCheckBox( "Display Eigen Vector Image" );

    JCheckBox createFA = new JCheckBox( "Create FA Image" );
    JCheckBox displayFA = new JCheckBox( "Display FA Image" );

    JCheckBox createRA = new JCheckBox( "Create RA Image" );
    JCheckBox displayRA = new JCheckBox( "Display RA Image" );

    JCheckBox createTrace = new JCheckBox( "Create Trace Image" );
    JCheckBox displayTrace = new JCheckBox( "Display Trace Image" );

    JCheckBox createVR = new JCheckBox( "Create VR Image" );
    JCheckBox displayVR = new JCheckBox( "Display VR Image" );

    private Font serif12;

    /**
     * Constructs the Fiber Tracking input panel:
     */
    public JPanelDTIFiberTracking(DTIPipeline pipeline) {
        super(new GridBagLayout());
        this.pipeline = pipeline;
        init(false);
    }
    
    /**
     * Constructs the Fiber Tracking input panel:
     */
    public JPanelDTIFiberTracking(JDialog parent, DTIPipeline pipeline) {
        super(new GridBagLayout());
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
        	if ( createDerivedImages() )
        	{
        		parentFrame.dispose();
        	}
        }
        else if (command.equalsIgnoreCase("browseDTIFile")) {    		
    		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
    		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
    		chooser.setDialogTitle("Choose image file");
    		final int returnValue = chooser.showOpenDialog(this);
    		if (returnValue == JFileChooser.APPROVE_OPTION) {
    			final FileIO fileIO = new FileIO();

    			tensorImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
    					+ File.separator);
    			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
    			
                if (tensorImage.getNDims() != 4) {
                    MipavUtil.displayError("Tensor Image must be a 4D image");
                    
                    createADC.setEnabled(false);
                    createColor.setEnabled(false);
                    createEValue.setEnabled(false);
                    createEVector.setEnabled(false);
                    createFA.setEnabled(false);
                    createRA.setEnabled(false);
                    createTrace.setEnabled(false);
                    createVR.setEnabled(false);
                }
                else
                {
                	textDTIimage.setText(tensorImage.getImageDirectory());
                	outputDirTextField.setText(tensorImage.getImageDirectory());

                	createADC.setEnabled(true);
                	createColor.setEnabled(true); createColor.setSelected(true);
                	createEValue.setEnabled(true); createEValue.setSelected(true);
                	createEVector.setEnabled(true); createEVector.setSelected(true);
                	createFA.setEnabled(true); createFA.setSelected(true);
                	createRA.setEnabled(true);
                	createTrace.setEnabled(true);
                	createVR.setEnabled(true);
                }
            }
        } else if (command.equals("browseOutput")) {
            final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));

            if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setDialogTitle("Choose dir");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
                outputDirTextField.setText(currDir);
            }
        }
        if ( (pipeline != null) && (tensorImage != null) && (outputDirTextField.getText() != null) )
        {
        	pipeline.nextButton.setEnabled(true);
        }
        

        displayADC.setEnabled(createADC.isSelected());
        displayColor.setEnabled(createColor.isSelected());
        displayEValue.setEnabled(createEValue.isSelected());
        displayEVector.setEnabled(createEVector.isSelected());
        displayFA.setEnabled(createFA.isSelected());
        displayRA.setEnabled(createRA.isSelected());
        displayTrace.setEnabled(createTrace.isSelected());
        displayVR.setEnabled(createVR.isSelected());
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
    public boolean createDerivedImages()
    {
        boolean success = validateData();
        if ( !success) {
            return false;
        }

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        

        if ( createADC.isSelected() || createColor.isSelected() || createEValue.isSelected() ||
        		createEVector.isSelected() || createFA.isSelected() || createRA.isSelected() ||
        		createTrace.isSelected() || createVR.isSelected() )
        {
        	calcEigenVectorImage();      
        }

        //trackFibers();

        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        return true;
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
    	textDTIimage.setText( image.getImageName() );
    	outputDirTextField.setText( image.getImageDirectory() );

    	createADC.setEnabled(true);
    	createColor.setEnabled(true); createColor.setSelected(true);
    	createEValue.setEnabled(true); createEValue.setSelected(true);
    	createEVector.setEnabled(true); createEVector.setSelected(true);
    	createFA.setEnabled(true); createFA.setSelected(true);
    	createRA.setEnabled(true);
    	createTrace.setEnabled(true);
    	createVR.setEnabled(true);

        displayADC.setEnabled(createADC.isSelected());
        displayColor.setEnabled(createColor.isSelected());
        displayEValue.setEnabled(createEValue.isSelected());
        displayEVector.setEnabled(createEVector.isSelected());
        displayFA.setEnabled(createFA.isSelected());
        displayRA.setEnabled(createRA.isSelected());
        displayTrace.setEnabled(createTrace.isSelected());
        displayVR.setEnabled(createVR.isSelected());
    }

    public void windowClosing(final WindowEvent event) {
        cleanup();

    }
	private void buildDTILoadPanel() {

		final JPanel DTIloadPanel = new JPanel();
		DTIloadPanel.setLayout(new GridBagLayout());
		DTIloadPanel.setBorder(JInterfaceBase.buildTitledBorder("Upload Tensor Image"));

		final GridBagConstraints gbc = new GridBagConstraints();

		/*gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.CENTER;
		gbc.anchor = GridBagConstraints.WEST;*/

		JButton openDTIimageButton = new JButton("Browse");
		openDTIimageButton.setToolTipText("Browse Diffusion Tensor image file");
		openDTIimageButton.addActionListener(this);
		openDTIimageButton.setActionCommand("browseDTIFile");
		openDTIimageButton.setEnabled(true);

		textDTIimage = new JTextField();
		textDTIimage.setPreferredSize(new Dimension(275, 21));
		textDTIimage.setEditable(true);
		textDTIimage.setBackground(Color.white);
		textDTIimage.setFont(MipavUtil.font12);

		JLabel dtiFileLabel = new JLabel("Tensor Image: ");
		dtiFileLabel.setFont(serif12);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(dtiFileLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.15;
        gbc.fill = GridBagConstraints.HORIZONTAL;
		DTIloadPanel.add(textDTIimage, gbc);

        gbc.gridx = 2;
        gbc.gridy = 1;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
		DTIloadPanel.add(openDTIimageButton, gbc);

		mainPanel.add(DTIloadPanel);
	}
	
	
	/*private void buildDTIOutputPanel() {

		final JPanel DTIloadPanel = new JPanel();
		DTIloadPanel.setLayout(new GridBagLayout());
		DTIloadPanel.setBorder(JInterfaceBase.buildTitledBorder(""));

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.CENTER;
		gbc.anchor = GridBagConstraints.WEST;

		JButton openDTIOutputButton = new JButton("Browse");
		openDTIOutputButton.setToolTipText("Browse diffusion tensor output directory");
		openDTIOutputButton.addActionListener(this);
		openDTIOutputButton.setActionCommand("browseOutput");
		openDTIOutputButton.setEnabled(true);

		outputDirTextField = new JTextField();
		outputDirTextField.setPreferredSize(new Dimension(275, 21));
		outputDirTextField.setEditable(true);
		outputDirTextField.setBackground(Color.white);
		outputDirTextField.setFont(MipavUtil.font12);

		JLabel dtiOutputLabel = new JLabel("Output Directory: ");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(dtiOutputLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(outputDirTextField, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openDTIOutputButton, gbc);

		mainPanel.add(DTIloadPanel);
	}*/
    
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
        if ( traceImage != null )
        {
        	if ( createTrace.isSelected() )
        	{        	
        		traceImage.setImageName( TraceImageName );
        		ModelImage.saveImage( traceImage, traceImage.getImageName() + ".xml", outputDirTextField.getText() );
        		if ( displayTrace.isSelected() )
        		{
        			traceImage.calcMinMax();
        			new ViewJFrameImage(traceImage);
        		}
        	}
        	if ( !createTrace.isSelected() || !displayTrace.isSelected() )
        	{
        		traceImage.disposeLocal();
        	}
        }

        raImage = kAlgorithm.getRAImage();
        if ( raImage != null )
        {
        	if ( createRA.isSelected() )
        	{
        		raImage.setImageName( RAImageName );
        		ModelImage.saveImage( raImage, raImage.getImageName() + ".xml", outputDirTextField.getText() );
        		if ( displayRA.isSelected() )
        		{
        			raImage.calcMinMax();
        			new ViewJFrameImage(raImage);
        		}
        	}
        	if ( !createRA.isSelected() || !displayRA.isSelected() )
        	{
        		raImage.disposeLocal();
        	}
        }

        vrImage = kAlgorithm.getVRImage();
        if ( vrImage != null )
        {
        	if ( createVR.isSelected() )
        	{
        		vrImage.setImageName( VRImageName );
        		ModelImage.saveImage( vrImage, vrImage.getImageName() + ".xml", outputDirTextField.getText() );
        		if ( displayVR.isSelected() )
        		{
        			vrImage.calcMinMax();
        			new ViewJFrameImage(vrImage);
        		}
        	}
        	if ( !createVR.isSelected() || !displayVR.isSelected() )
        	{
        		vrImage.disposeLocal();
        	}
        }

        adcImage = kAlgorithm.getADCImage();
        if ( adcImage != null )
        {
        	if ( createADC.isSelected() )
        	{
        		adcImage.setImageName( ADCImageName );
        		ModelImage.saveImage( adcImage, adcImage.getImageName() + ".xml", outputDirTextField.getText() );
        		if ( displayADC.isSelected() )
        		{
        			adcImage.calcMinMax();
        			new ViewJFrameImage(adcImage);
        		}
        	}
        	if ( !createADC.isSelected() || !displayADC.isSelected() )
        	{
        		adcImage.disposeLocal();
        	}
        }

        

        // The remaining images the calling function must close:
        if ( createEValue.isSelected() )
        {
        	eigenValueImage = kAlgorithm.getEigenValueImage();
        	eigenValueImage.setImageName( EigenValueImageName );
        	ModelImage.saveImage( eigenValueImage, eigenValueImage.getImageName() + ".xml", outputDirTextField.getText() );
        	if ( displayEValue.isSelected() )
        	{
        		eigenValueImage.calcMinMax();
        		new ViewJFrameImage(eigenValueImage);
        	}
        }

        if ( createEVector.isSelected() )
        {
        	eigenVectorImage = kAlgorithm.getEigenVectorImage();
        	eigenVectorImage.setImageName( EigenVectorImageName );
        	ModelImage.saveImage( eigenVectorImage, eigenVectorImage.getImageName() + ".xml", outputDirTextField.getText() );
        	if ( displayEVector.isSelected() )
        	{
        		eigenVectorImage.calcMinMax();
        		new ViewJFrameImage( eigenVectorImage );
        	}
        }

        if ( createFA.isSelected() )
        {
        	FAImage = kAlgorithm.getFAImage();
        	FAImage.setImageName( FAImageName );
        	ModelImage.saveImage( FAImage, FAImage.getImageName() + ".xml", outputDirTextField.getText() );
        	if ( displayFA.isSelected() )
        	{
        		FAImage.calcMinMax();
        		new ViewJFrameImage( FAImage );
        	}
        }

        if ( createColor.isSelected() )
        {
        	rgbImage = kAlgorithm.getColorImage();
        	rgbImage.setImageName( ColorMapImageName );
        	ModelImage.saveImage( rgbImage, rgbImage.getImageName() + ".xml", outputDirTextField.getText() );
        	if ( displayColor.isSelected() )
        	{
        		rgbImage.calcMinMax();
        		new ViewJFrameImage( rgbImage );
        	}
        }
/*
        if ( (eigenVectorImage != null) && (FAImage != null) )
        {
        	int[] dimExtentsLUT;
        	dimExtentsLUT = new int[2];
        	dimExtentsLUT[0] = 4;
        	dimExtentsLUT[1] = 256;
        	ModelLUT m_kLUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
        	m_kLUTa.resetTransferLine(0.0f, (int) Math.round(eigenVectorImage.getMin()), 255.0f, (int) Math
        			.round(eigenVectorImage.getMax()));

        	new DialogDTIColorDisplay((ModelImage)eigenVectorImage.clone(), (ModelImage)FAImage.clone(), m_kLUTa, false);
        }
        */
        kAlgorithm.disposeLocal();
        kAlgorithm = null;
    }
    
    private void cleanup() {
        if ((tensorImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(tensorImage) == null)) {
            tensorImage.disposeLocal();
        }
        tensorImage = null;
        
        
        if ((eigenVectorImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(eigenVectorImage) == null)) {
            eigenVectorImage.disposeLocal();
        }
        eigenVectorImage = null;
        
        
        if ((FAImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(FAImage) == null)) {
            FAImage.disposeLocal();
        }
        FAImage = null;
        
        
        if ((eigenValueImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(eigenValueImage) == null)) {
            eigenValueImage.disposeLocal();
        }
        eigenValueImage = null;
        
        
        if ((rgbImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(rgbImage) == null)) {
            rgbImage.disposeLocal();
        }
        rgbImage = null;
        
        
        if ((traceImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(traceImage) == null)) {
            traceImage.disposeLocal();
        }
        traceImage = null;
        
        
        if ((raImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(raImage) == null)) {
            raImage.disposeLocal();
        }
        raImage = null;
        
        
        if ((vrImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(vrImage) == null)) {
            vrImage.disposeLocal();
        }
        vrImage = null;
        
        
        if ((adcImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(tensorImage) == null)) {
            adcImage.disposeLocal();
        }
        adcImage = null;
    }

    private void init(boolean bStandAlone) {
		mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

		buildDTILoadPanel();
		//buildDTIOutputPanel();
		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.fill = GridBagConstraints.HORIZONTAL;
		gbc2.weightx = 1;
		gbc2.weighty = 1;
		gbc2.gridx = 0;
		gbc2.gridy = 0;
		gbc2.anchor = GridBagConstraints.NORTHWEST;
		this.add(mainPanel, gbc2);
		
		JPanel imageOutputPanel = new JPanel(new GridBagLayout());
		imageOutputPanel.setBorder(JInterfaceBase.buildTitledBorder("Output Options"));
		final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(createADC, gbc);
        gbc.gridx = 1;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        imageOutputPanel.add(displayADC, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(createColor, gbc);
        gbc.gridx = 1;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        imageOutputPanel.add(displayColor, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(createEValue, gbc);
        gbc.gridx = 1;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        imageOutputPanel.add(displayEValue, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(createEVector, gbc);
        gbc.gridx = 1;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        imageOutputPanel.add(displayEVector, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(createFA, gbc);
        gbc.gridx = 1;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        imageOutputPanel.add(displayFA, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(createRA, gbc);
        gbc.gridx = 1;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        imageOutputPanel.add(displayRA, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(createTrace, gbc);
        gbc.gridx = 1;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        imageOutputPanel.add(displayTrace, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(createVR, gbc);
        gbc.gridx = 1;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        imageOutputPanel.add(displayVR, gbc);
        
        JButton openDTIOutputButton = new JButton("Browse");
        openDTIOutputButton.setToolTipText("Browse diffusion tensor output directory");
        openDTIOutputButton.addActionListener(this);
        openDTIOutputButton.setActionCommand("browseOutput");
        openDTIOutputButton.setEnabled(true);

        outputDirTextField = new JTextField();
        outputDirTextField.setPreferredSize(new Dimension(275, 21));
        outputDirTextField.setEditable(true);
        outputDirTextField.setBackground(Color.white);
        outputDirTextField.setFont(MipavUtil.font12);

        JLabel dtiOutputLabel = new JLabel("Output Directory: ");
        dtiOutputLabel.setFont(serif12);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        imageOutputPanel.add(dtiOutputLabel, gbc);

        gbc.gridx = 1;
        gbc.weightx = 0.10;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        imageOutputPanel.add(outputDirTextField, gbc);

        gbc.gridx = 2;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
        imageOutputPanel.add(openDTIOutputButton, gbc);
        



        createADC.setFont(serif12);
        createADC.setEnabled(false); createADC.addActionListener(this);
        displayADC.setFont(serif12);
        displayADC.setEnabled(false);

        createColor.setFont(serif12);
        createColor.setEnabled(false); createColor.addActionListener(this);
        displayColor.setFont(serif12);
        displayColor.setEnabled(false);

        createEValue.setFont(serif12);
        createEValue.setEnabled(false); createEValue.addActionListener(this);
        displayEValue.setFont(serif12);
        displayEValue.setEnabled(false);

        createEVector.setFont(serif12);
        createEVector.setEnabled(false); createEVector.addActionListener(this);
        displayEVector.setFont(serif12);
        displayEVector.setEnabled(false);

        createFA.setFont(serif12);
        createFA.setEnabled(false); createFA.addActionListener(this);
        displayFA.setFont(serif12);
        displayFA.setEnabled(false);

        createRA.setFont(serif12);
        createRA.setEnabled(false); createRA.addActionListener(this);
        displayRA.setFont(serif12);
        displayRA.setEnabled(false);

        createTrace.setFont(serif12);
        createTrace.setEnabled(false); createTrace.addActionListener(this);
        displayTrace.setFont(serif12);
        displayTrace.setEnabled(false);

        createVR.setFont(serif12);
        createVR.setEnabled(false); createVR.addActionListener(this);
        displayVR.setFont(serif12);
        displayVR.setEnabled(false);

        mainPanel.add( imageOutputPanel );
        
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
        
        
        setVisible(true);
    }


    private boolean validateData() {
        final boolean success = true;

        final String outputDirString = outputDirTextField.getText().trim();
        final String tensorImageString = textDTIimage.getText().trim();

        if (outputDirString.equals("") || tensorImageString.equals("")) {
            MipavUtil.displayError("Tensor Image and Output Dir are required parameters");
            return false;
        }
        return success;
    }

}


