package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

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
	
	/** Check boxes enable the user to save and display output images: */
	private JCheckBox displayExit = new JCheckBox( "Display Exit Code Image" );
	private JCheckBox displayIntensity = new JCheckBox( "Display Intensity Image" );
	private JCheckBox displayTensor = new JCheckBox( "Display Tensor Image" );
	private JCheckBox saveExit = new JCheckBox( "Save Exit Code Image" );
	private JCheckBox saveIntensity = new JCheckBox( "Save Intensity Image" );
	private JCheckBox saveTensor = new JCheckBox( "Save Tensor Image" );
    
    public JPanelDTIEstimateTensor(DTIPipeline pipeline) {
		super(new GridBagLayout());
		this.pipeline = pipeline;
		init();
	}

	public void actionPerformed(ActionEvent event) {
		final String command = event.getActionCommand();
		if ( command.equals("browseMaskFile")){
			loadMaskImage();
		}
		else if (command.equals("browseOutput")) {
            final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));

            chooser.setDialogTitle("Choose dir");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                outputDirTextField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);
                System.err.println( outputDirTextField.getText() );
            }
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
			AlgorithmDWI2DTI calcDTI = new AlgorithmDWI2DTI( dwiImage, maskImage );
			calcDTI.addListener(this);
			calcDTI.run();
			break;
		case LLMSE:
			tensorImage = EstimateTensorLLMSE.estimate( dwiImage, maskImage, true );
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
    
	
	
	
	

	public void setImage( ModelImage image )
	{
		tensorImage = image;
		outputDirTextField.setText( tensorImage.getImageDirectory() );
        System.err.println( outputDirTextField.getText() );
	}



	private void buildDTIOutputPanel() {

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
	}

	private void buildMaskLoadPanel() {

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

		JButton openMaskimageButton = new JButton("Browse");
		openMaskimageButton.setToolTipText("Browse mask image file");
		openMaskimageButton.addActionListener(this);
		openMaskimageButton.setActionCommand("browseMaskFile");
		openMaskimageButton.setEnabled(true);

		textMaskimage = new JTextField();
		textMaskimage.setPreferredSize(new Dimension(275, 21));
		textMaskimage.setEditable(true);
		textMaskimage.setBackground(Color.white);
		textMaskimage.setFont(MipavUtil.font12);

		JLabel maskLabel = new JLabel("Mask Image: ");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(maskLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(textMaskimage, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openMaskimageButton, gbc);

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
		buildDTIOutputPanel();
		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.fill = GridBagConstraints.NONE;
		gbc2.weightx = 1;
		gbc2.weighty = 1;
		gbc2.gridx = 0;
		gbc2.gridy = 0;
		gbc2.anchor = GridBagConstraints.NORTHWEST;
		this.add(mainPanel, gbc2);
		
		
		JPanel tensorEstPanel = new JPanel(new GridBagLayout());
		tensorEstPanel.setBorder(JInterfaceBase.buildTitledBorder("Choose Tensor Estimation Algorithm"));       
		JLabel labelDTIEst = new JLabel("DTI Algorithm:");
		
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

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;

		gbc.gridx = 0;
		gbc.gridy = 0;
		tensorEstPanel.add(labelDTIEst, gbc);
		gbc.gridx = 1;
		tensorEstPanel.add(comboBoxDTI_Algorithm, gbc);

		saveTensor.setEnabled(true);
		saveTensor.setSelected(true);
		saveTensor.addActionListener(this);
		gbc.gridx = 0;
		gbc.gridy++;
		tensorEstPanel.add( saveTensor, gbc );
		gbc.gridx = 1;
		displayExit.setEnabled(true);
		tensorEstPanel.add( displayTensor, gbc );

		saveExit.setEnabled(false);
		saveExit.addActionListener(this);
		gbc.gridx = 0;
		gbc.gridy++;
		tensorEstPanel.add( saveExit, gbc );
		gbc.gridx = 1;
		displayExit.setEnabled(false);
		tensorEstPanel.add( displayExit, gbc );

		saveIntensity.setEnabled(false);
		saveIntensity.addActionListener(this);
		gbc.gridx = 0;
		gbc.gridy++;
		tensorEstPanel.add( saveIntensity, gbc );
		gbc.gridx = 1;
		displayIntensity.setEnabled(false);
		tensorEstPanel.add( displayIntensity, gbc );
		
		mainPanel.add(tensorEstPanel);

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

}
