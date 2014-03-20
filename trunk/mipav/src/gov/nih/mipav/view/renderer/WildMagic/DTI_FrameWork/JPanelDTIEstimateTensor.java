package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.GMatrixd;

public class JPanelDTIEstimateTensor extends JPanel implements AlgorithmInterface, ActionListener, ItemListener {

	private static final long serialVersionUID = -6738722246165232246L;
	
	/** Different DTI algorithms: */
	public final static int DEFAULT = 0;
	public final static int LLMSE = 1;
	public final static int LINEAR = 2;
	public final static int NON_LINEAR = 3;
	public final static int RESTORE = 4;
	public final static int WEIGHTED_LINEAR = 5;
	
	/** menu for selecting dti algorithm: */
	private JComboBox comboBoxDTI_Algorithm;
	/** main panel */
	private JPanel mainPanel;

	/** Mask image for the tensor calculation */
	private ModelImage maskImage = null;
	/** output directory */
	private JTextField outputDirTextField;
	/** parent dialog */
	private DTIPipeline pipeline;
	/** Diffusion tensor image: */
	private ModelImage tensorImage = null;
	/** mask image location: */
	private JTextField textMaskimage;
	/** DWI .list location: */
	private JTextField textListFile;
	private JPanelEPIDistortionCorrection EPIpanel;
	
	/** Check boxes enable the user to save and display output images: */
	private JCheckBox displayExit = new JCheckBox( "Display Exit Code Image" );
	private JCheckBox displayIntensity = new JCheckBox( "Display Intensity Image" );
	private JCheckBox displayTensor = new JCheckBox( "Display Tensor Image" );
	private JCheckBox saveExit = new JCheckBox( "Save Exit Code Image" );
	private JCheckBox saveIntensity = new JCheckBox( "Save Intensity Image" );
	private JCheckBox saveTensor = new JCheckBox( "Save Tensor Image" );

    private Font serif12;

    private JButton calcTensor;
    
    public JPanelDTIEstimateTensor(DTIPipeline pipeline) {
		super(new GridBagLayout());
		this.pipeline = pipeline;
		init();
	}

	public void actionPerformed(ActionEvent event) {
		final String command = event.getActionCommand();
		if ( command.equals("browseMaskFile")){
			loadMaskImage();
		} else if (command.equals("browseOutput")) {
            final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));

            chooser.setDialogTitle("Choose dir");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                outputDirTextField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);
                System.err.println( outputDirTextField.getText() );
            }
        } else if (command.equalsIgnoreCase("DWIListBrowse")) {
            loadDWIListFile();
        }//TODO:
        else if (command.equalsIgnoreCase("calcTensor")) { 
            calcTensor(pipeline.currentImage);
        }

		displayExit.setEnabled( saveExit.isSelected() );
		displayIntensity.setEnabled( saveIntensity.isSelected() );
		displayTensor.setEnabled( saveTensor.isSelected() );

	}

	public void algorithmPerformed(final AlgorithmBase algorithm) {
		if ( algorithm instanceof AlgorithmDWI2DTI && algorithm.isCompleted() )
		{
			tensorImage = ((AlgorithmDWI2DTI)algorithm).getDTI();
			finishTensorPanel();
			// delete intermediate images:
			((AlgorithmDWI2DTI)algorithm).deleteImages();
		}
	}

	public void calcTensor( ModelImage dwiImage )
	{
		switch ( comboBoxDTI_Algorithm.getSelectedIndex() ) {
		case DEFAULT: 
			AlgorithmDWI2DTI calcDTI;
			if ( (m_aakDWIList != null) && (m_kBMatrix != null) )
			{
				calcDTI = new AlgorithmDWI2DTI(maskImage, false, m_iSlices,
                        m_iDimX, m_iDimY, m_iBOrig, m_iWeights, m_fMeanNoise, m_aakDWIList, m_aiMatrixEntries, m_kBMatrix,
                        m_kRawFormat);
			}
			else
			{
				calcDTI = new AlgorithmDWI2DTI( dwiImage, maskImage );
			}
			calcDTI.addListener(this);
			calcDTI.run();
			break;
		case LLMSE:
			if ( (m_kBMatrix != null) && (m_kDWIImage != null) )
			{
				tensorImage = EstimateTensorLLMSE.estimate( m_kDWIImage, maskImage, m_kBMatrix, true );
			}
			else
			{
				tensorImage = EstimateTensorLLMSE.estimate( dwiImage, maskImage, true );
			}
			finishTensorPanel();
			break;
		case LINEAR:
		case NON_LINEAR:
		case RESTORE:
		case WEIGHTED_LINEAR:
			tensorImage = EstimateTensorLLMSE.estimateCamino( dwiImage, maskImage, comboBoxDTI_Algorithm.getSelectedIndex(),
					saveExit.isSelected(), displayExit.isSelected(), saveIntensity.isSelected(), displayIntensity.isSelected(),
					outputDirTextField.getText() );
			finishTensorPanel();
			break;
		}
	}
	
	
	
	
	public ModelImage getMaskImage()
	{
		return maskImage;
	}
	
	
	public void itemStateChanged(ItemEvent event) 
	{
		if ( event.getSource() == comboBoxDTI_Algorithm )
		{
			switch ( comboBoxDTI_Algorithm.getSelectedIndex() )
			{

			case DEFAULT: 
			case LLMSE:
				saveExit.setEnabled(false); saveExit.setSelected(false);
				displayExit.setEnabled(false); displayExit.setSelected(false); 
				
				saveIntensity.setEnabled(false); saveIntensity.setSelected(false);
				displayIntensity.setEnabled(false); displayIntensity.setSelected(false); 
				break;
			case LINEAR:
			case NON_LINEAR:
			case RESTORE:
			case WEIGHTED_LINEAR:
				saveExit.setEnabled(true);
				displayExit.setEnabled(true); 
				
				saveIntensity.setEnabled(true);
				displayIntensity.setEnabled(true);
				break;
			}
		}
	}

	/**
	 * Set the input diffusion-weighted image (4D image series).
	 * @param image
	 */
	public void setImage( ModelImage image )
	{
		tensorImage = image;
		outputDirTextField.setText( tensorImage.getImageDirectory() );
		DTIParameters dtiparams = image.getDTIParameters();
		if ( dtiparams == null )
		{
			return;
		}
		if ( (dtiparams.getGradients() == null) || (dtiparams.getbValues() == null) )
		{
			if ( comboBoxDTI_Algorithm.getItemAt(5) != null )
			{
				comboBoxDTI_Algorithm.removeItemAt(5);
			}
			if ( comboBoxDTI_Algorithm.getItemAt(4) != null )
			{
				comboBoxDTI_Algorithm.removeItemAt(4);
			}
			if ( comboBoxDTI_Algorithm.getItemAt(3) != null )
			{
				comboBoxDTI_Algorithm.removeItemAt(3);
			}
			if ( comboBoxDTI_Algorithm.getItemAt(2) != null )
			{
				comboBoxDTI_Algorithm.removeItemAt(2);
			}
		}
	}


	private void buildTensorEstPanel() {
	    
	    final GridBagConstraints gbc = new GridBagConstraints();
	    
	    JPanel tensorEstPanel = new JPanel(new GridBagLayout());
        tensorEstPanel.setBorder(JInterfaceBase.buildTitledBorder("Choose Tensor Estimation Algorithm"));       
        JLabel labelDTIEst = new JLabel("DTI Algorithm:");
        labelDTIEst.setFont(serif12);
        
        comboBoxDTI_Algorithm = new JComboBox();
        comboBoxDTI_Algorithm.setEnabled(true);
        comboBoxDTI_Algorithm.setFont(MipavUtil.font12);
        comboBoxDTI_Algorithm.setBackground(Color.white);
        comboBoxDTI_Algorithm.setToolTipText("Select DTI Algorithm");
        comboBoxDTI_Algorithm.addItem("Weighted, noise-reduction");
        comboBoxDTI_Algorithm.addItem("LLMSE");
        comboBoxDTI_Algorithm.addItem("CAMINO: Linear");
        comboBoxDTI_Algorithm.addItem("CAMINO: Non-Linear");
        comboBoxDTI_Algorithm.addItem("CAMINO: Restore");
        comboBoxDTI_Algorithm.addItem("CAMINO: Weighted Linear");
        comboBoxDTI_Algorithm.setSelectedIndex(0);
        comboBoxDTI_Algorithm.addItemListener(this);
        comboBoxDTI_Algorithm.setActionCommand("comboBoxDTI");
        comboBoxDTI_Algorithm.addActionListener(this);

        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        tensorEstPanel.add(labelDTIEst, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = .15;
        gbc.fill = GridBagConstraints.REMAINDER;
        tensorEstPanel.add(comboBoxDTI_Algorithm, gbc);

		mainPanel.add(tensorEstPanel);
	}

	/*
	private void buildListLoadPanel() {

		final JPanel ListloadPanel = new JPanel();
		ListloadPanel.setLayout(new GridBagLayout());
		ListloadPanel.setBorder(JInterfaceBase.buildTitledBorder(""));

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.CENTER;
		gbc.anchor = GridBagConstraints.WEST;

		JButton openListFileButton = new JButton("Browse");
		openListFileButton.setToolTipText("Browse .list file");
		openListFileButton.addActionListener(this);
		openListFileButton.setActionCommand("DWIListBrowse");
		openListFileButton.setEnabled(true);

		textListFile = new JTextField();
		textListFile.setPreferredSize(new Dimension(275, 21));
		textListFile.setEditable(true);
		textListFile.setBackground(Color.white);
		textListFile.setFont(MipavUtil.font12);

		JLabel listLabel = new JLabel("DWI .list file: ");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		ListloadPanel.add(listLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		ListloadPanel.add(textListFile, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		ListloadPanel.add(openListFileButton, gbc);

		mainPanel.add(ListloadPanel);
	}
	*/

	private void buildMaskLoadPanel() {

		
        final GridBagConstraints gbc = new GridBagConstraints();
        
        final JPanel DTIloadPanel = new JPanel(new GridBagLayout());
        DTIloadPanel.setBorder(JInterfaceBase.buildTitledBorder("Upload Mask Image"));            
        JLabel maskLabel = new JLabel("Mask Image (optional): ");
        maskLabel.setFont(serif12);
        maskLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 2, 0, 2);
        DTIloadPanel.add(maskLabel,gbc);
        
        textMaskimage = new JTextField();
        textMaskimage.setPreferredSize(new Dimension(100, 21));
        textMaskimage.setEnabled(true);
        textMaskimage.setBackground(Color.white);
        textMaskimage.setFont(MipavUtil.font12);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.15;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        DTIloadPanel.add(textMaskimage,gbc);
        
        
        JButton openMaskImageButton = new JButton("Browse");
        openMaskImageButton.setToolTipText("Browse mask image file");
        openMaskImageButton.addActionListener(this);
        openMaskImageButton.setActionCommand("browseMaskFile");
        openMaskImageButton.setEnabled(true);
        gbc.gridx = 2;
        gbc.gridy = 1;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
        DTIloadPanel.add(openMaskImageButton,gbc);
        
        mainPanel.add(DTIloadPanel);
	}
	
	private void finishTensorPanel()
	{
		tensorImage.setImageDirectory( outputDirTextField.getText() );
		if ( saveTensor.isSelected() )
		{
			// save the tensor image
			ModelImage.saveImage( tensorImage, tensorImage.getImageName() + ".xml", outputDirTextField.getText() );
		}
		if ( displayTensor.isSelected() )
		{
			new ViewJFrameImage( tensorImage );
		}
		pipeline.finishTensorPanel(tensorImage);
		if ( maskImage != null )
		{
			maskImage.disposeLocal();
			maskImage = null;
		}
	}


	private void init() {
		mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

		buildMaskLoadPanel();
		buildTensorEstPanel();
		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.fill = GridBagConstraints.HORIZONTAL;
		gbc2.weightx = 1;
		gbc2.weighty = 1;
		gbc2.gridx = 0;
		gbc2.gridy = 0;
		gbc2.anchor = GridBagConstraints.NORTHWEST;
		this.add(mainPanel,gbc2);
		
		final GridBagConstraints gbc = new GridBagConstraints();

        final JPanel DTIOutputPanel = new JPanel();
        DTIOutputPanel.setLayout(new GridBagLayout());
        DTIOutputPanel.setBorder(JInterfaceBase.buildTitledBorder("Output Options"));
		
	    gbc.fill = GridBagConstraints.NONE;
	    gbc.anchor = GridBagConstraints.WEST;

	    saveTensor.setFont(serif12);
		saveTensor.setEnabled(true);
		saveTensor.setSelected(true);
		saveTensor.addActionListener(this);
		gbc.gridx = 0;
		gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        DTIOutputPanel.add( saveTensor, gbc );
		gbc.gridx = 1;
		gbc.weightx = .15;
		gbc.fill = GridBagConstraints.REMAINDER;
		displayTensor.setFont(serif12);
		displayTensor.setEnabled(true);
		DTIOutputPanel.add( displayTensor, gbc );

		saveExit.setFont(serif12);
		saveExit.setEnabled(false);
		saveExit.addActionListener(this);
		gbc.gridx = 0;
		gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        DTIOutputPanel.add( saveExit, gbc );
	    gbc.gridx = 1;
	    gbc.weightx = .15;
	    gbc.fill = GridBagConstraints.REMAINDER;
	    displayExit.setFont(serif12);
		displayExit.setEnabled(false);
		DTIOutputPanel.add( displayExit, gbc );

		saveIntensity.setFont(serif12);
		saveIntensity.setEnabled(false);
		saveIntensity.addActionListener(this);
		gbc.gridx = 0;
		gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        DTIOutputPanel.add( saveIntensity, gbc );
		gbc.gridx = 1;
	    gbc.weightx = .15;
	    gbc.fill = GridBagConstraints.REMAINDER;
	    displayIntensity.setFont(serif12);
		displayIntensity.setEnabled(false);
		DTIOutputPanel.add( displayIntensity, gbc );
		
        JLabel dtiOutputLabel = new JLabel("Tensor Estimation Output Directory: ");
        dtiOutputLabel.setFont(serif12);
        
        
        outputDirTextField = new JTextField();
        outputDirTextField.setPreferredSize(new Dimension(80, 21));
        outputDirTextField.setEditable(true);
        outputDirTextField.setBackground(Color.white);
        outputDirTextField.setFont(MipavUtil.font12);
        
        
        JButton openDTIOutputButton = new JButton("Browse");
        openDTIOutputButton.setToolTipText("Browse diffusion tensor output directory");
        openDTIOutputButton.addActionListener(this);
        openDTIOutputButton.setActionCommand("browseOutput");
        openDTIOutputButton.setEnabled(true);
        
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        DTIOutputPanel.add(dtiOutputLabel, gbc);

        gbc.gridx = 1;
        gbc.weightx = 0.10;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        DTIOutputPanel.add(outputDirTextField, gbc);

        gbc.gridx = 2;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
        DTIOutputPanel.add(openDTIOutputButton, gbc);
		
		mainPanel.add(DTIOutputPanel);
		
		
		//TODO:
		calcTensor = new JButton("Calculate Tensor");
        calcTensor.addActionListener(this);
        calcTensor.setActionCommand("calcTensor");
        calcTensor.setEnabled(false);
        
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridBagLayout());
        buttonPanel.setBorder(JInterfaceBase.buildTitledBorder(""));
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.CENTER;
        gbc.anchor = GridBagConstraints.CENTER;
        buttonPanel.add(calcTensor, gbc);
        //mainPanel.add(buttonPanel);

	}
	
	

	private void loadMaskImage() {
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
		chooser.setDialogTitle("Choose Diffusion Tensor Color image file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			final FileIO fileIO = new FileIO();

			maskImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
					+ File.separator);
			textMaskimage.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}
	}
	

    private int m_iDimX, m_iDimY, m_iSlices, m_iWeights;
    private String m_kRawFormat;
    private float m_fResX, m_fResY, m_fResZ, m_fMeanNoise;
    private boolean m_bUseXRes, m_bUseYRes, m_bUseZRes;
    ModelImage m_kDWIImage;
	
    /**
     * Launches the JFileChooser for the user to select the Diffusion Weighted Images .path file. Loads the .path file.
     */
    public void loadDWIListFile() {

        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
        chooser.setDialogTitle("Choose Diffusion Weighted Images  .list file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final File kFile = new File(chooser.getSelectedFile().getAbsolutePath());
            if ( !kFile.exists() || !kFile.canRead()) {
                return;
            }
            final int iLength = (int) kFile.length();
            if (iLength <= 0) {
                return;
            }
            textListFile.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());

            String kParentDir = chooser.getCurrentDirectory().toString();
            final File kListFile = new File(chooser.getSelectedFile().getAbsolutePath());
            String pathFilename = null;
            String pathFileAbsPath = null;

            String bMatrixFilename = null;
            String bMatrixFileAbsPath = null;
            
            try {
                BufferedReader kReader = new BufferedReader(new FileReader(kListFile));
                String lineString = null;
                while ( (lineString = kReader.readLine()) != null) {
                    if (lineString.startsWith("<original_columns>")) {
                        final String columnsStr = lineString.substring(lineString.indexOf("<original_columns>") + 18,
                                lineString.indexOf("</original_columns>")).trim();
                        m_iDimX = Integer.parseInt(columnsStr);
                    } else if (lineString.startsWith("<original_rows>")) {
                        final String rowsStr = lineString.substring(lineString.indexOf("<original_rows>") + 15,
                                lineString.indexOf("</original_rows>")).trim();
                        m_iDimY = Integer.parseInt(rowsStr);
                    } else if (lineString.startsWith("<slice>")) {
                        final String sliceStr = lineString.substring(lineString.indexOf("<slice>") + 7,
                                lineString.indexOf("</slice>")).trim();
                        m_iSlices = Integer.parseInt(sliceStr);
                    } else if (lineString.startsWith("<nim>")) {
                        final String nimStr = lineString.substring(lineString.indexOf("<nim>") + 5,
                                lineString.indexOf("</nim>")).trim();
                        m_iWeights = Integer.parseInt(nimStr);
                    } else if (lineString.startsWith("<rawimageformat>")) {
                        m_kRawFormat = lineString.substring(lineString.indexOf("<rawimageformat>") + 16,
                                lineString.indexOf("</rawimageformat>")).trim();
                    } else if (lineString.startsWith("<raw_image_path_filename>")) {
                        pathFilename = lineString.substring(lineString.indexOf("<raw_image_path_filename>") + 25,
                                lineString.indexOf("</raw_image_path_filename>")).trim();
                        pathFileAbsPath = kParentDir + File.separator + pathFilename;
                        // studyName = pathFilename.substring(0, pathFilename.indexOf(".path"));
                    } else if (lineString.startsWith("<bmatrixfile>")) {
                        bMatrixFilename = lineString.substring(lineString.indexOf("<bmatrixfile>") + 13,
                                lineString.indexOf("</bmatrixfile>")).trim();
                        bMatrixFileAbsPath = kParentDir + File.separator + bMatrixFilename;
                        // studyName = pathFilename.substring(0, pathFilename.indexOf(".path"));
                    } else if (lineString.startsWith("<x_field_of_view>")) {
                        final String xFOVStr = lineString.substring(lineString.indexOf("<x_field_of_view>") + 17,
                                lineString.indexOf("</x_field_of_view>")).trim();
                        final float xFOV = Float.parseFloat(xFOVStr);
                        m_fResX = xFOV;
                        m_bUseXRes = true;
                    } else if (lineString.startsWith("<y_field_of_view>")) {
                        final String yFOVStr = lineString.substring(lineString.indexOf("<y_field_of_view>") + 17,
                                lineString.indexOf("</y_field_of_view>")).trim();
                        final float yFOV = Float.parseFloat(yFOVStr);
                        m_fResY = yFOV;
                        m_bUseYRes = true;
                    } else if (lineString.startsWith("<slice_thickness>")) {
                        final String zResStr = lineString.substring(lineString.indexOf("<slice_thickness>") + 17,
                                lineString.indexOf("</slice_thickness>")).trim();
                        m_fResZ = Float.parseFloat(zResStr);
                        m_bUseZRes = true;
                    } else if (lineString.startsWith("<noise_mean_ori>")) {
                        final String noiseStr = lineString.substring(lineString.indexOf("<noise_mean_ori>") + 16,
                                lineString.indexOf("</noise_mean_ori>")).trim();
                        m_fMeanNoise = Float.parseFloat(noiseStr);
                    }
                }
                kReader.close();
                kReader = null;
            } catch (final Exception e) {
                e.printStackTrace();
            }

            if ( (pathFilename != null) && (bMatrixFileAbsPath != null) ) {
                loadPathFile(pathFileAbsPath, kParentDir);
                loadBMatrixFile(bMatrixFileAbsPath);
                

                AlgorithmDWI2DTI calcDTI = new AlgorithmDWI2DTI(maskImage, false, m_iSlices,
                        m_iDimX, m_iDimY, m_iBOrig, m_iWeights, m_fMeanNoise, m_aakDWIList, m_aiMatrixEntries, m_kBMatrix,
                        m_kRawFormat);
                m_kDWIImage = calcDTI.getDWI();
				new ViewJFrameImage( m_kDWIImage );
            }
            m_fResX /= m_iDimX;
            m_fResY /= m_iDimY;
        }
    }
    //TODO:
    public void enableCalcButton()
    {
           calcTensor.setEnabled(true);
    }

    private String[][] m_aakDWIList;
    
    /**
     * Loads the .path file.
     * 
     * @param kFileName path file name.
     * @param kPathName, parent directory.
     */
    public void loadPathFile(final String kFileName, final String kPathName) {
        final File kFile = new File(kFileName);
        if ( !kFile.exists() || !kFile.canRead()) {
            return;
        }
        final int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return;
        }
        m_aakDWIList = new String[m_iSlices][m_iWeights];
        try {
            final BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;
            for (int i = 0; i < m_iSlices; i++) {
                for (int j = 0; j < m_iWeights; j++) {
                    str = in.readLine();
                    // m_aakDWIList[i][j] = new String(kPathName + File.separator + str);
                    m_aakDWIList[i][j] = new String(str);
                }
            }
            in.close();
        } catch (final IOException e) {}
    }
    
    private GMatrixd m_kBMatrix;
    private int[] m_aiMatrixEntries;
    private int m_iBOrig;
    
    /**
     * Loads the BMatrix file.
     * 
     * @param kFileName, name of BMatrix file.
     */
    private void loadBMatrixFile(final String kFileName) {
        final File kFile = new File(kFileName);
        if ( !kFile.exists() || !kFile.canRead()) {
            return;
        }
        final int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return;
        }

        try {
            final BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;

            m_kBMatrix = new GMatrixd(m_iWeights, 6 + 1);

            final String[] kBMatrixString = new String[m_iWeights];
            int nb = 0;

            m_aiMatrixEntries = new int[m_iWeights];
            for (int iRow = 0; iRow < m_iWeights; iRow++) {
                str = in.readLine();

                boolean gotit = false;
                for (int j = 0; j < nb; j++) {
                    if (str.equals(kBMatrixString[j])) {
                        gotit = true;
                        m_aiMatrixEntries[iRow] = j;
                        break;
                    }
                }
                if ( !gotit) {
                    kBMatrixString[nb] = str;
                    m_aiMatrixEntries[iRow] = nb;
                    nb = nb + 1;
                }

                final java.util.StringTokenizer st = new java.util.StringTokenizer(str);
                for (int iCol = 0; iCol < 6; iCol++) {
                    final double dValue = Double.valueOf(st.nextToken()).doubleValue();
                    m_kBMatrix.Set(iRow, iCol, dValue);
                }
                m_kBMatrix.Set(iRow, 6, 1.0);
            }
            in.close();

            m_iBOrig = nb;

        } catch (final IOException e) {}
    }


}
