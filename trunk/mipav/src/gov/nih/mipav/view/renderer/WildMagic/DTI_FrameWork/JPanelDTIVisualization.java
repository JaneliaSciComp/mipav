package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

public class JPanelDTIVisualization extends JPanel implements ActionListener {

	private static final long serialVersionUID = 8011148684175711251L;

	/** Creates as stand-alone dialog with the interface: */
	public static void createFrame()
	{
		JDialog dialog = new JDialog( ViewUserInterface.getReference().getMainFrame(), "Init DTI Visualization" );
		dialog.add( new JPanelDTIVisualization(dialog, true) );
		dialog.pack();
		dialog.setVisible(true);
	}

	/** Button enabled when all required images are loaded. Pressing 'Load' launches the Volume DTI Renderer. */
	private JButton loadButton;

	/** Anisotropy image * */
	private ModelImage m_kT2Image;

	/** Anisotropy image * */
	private ModelImage m_kAnisotropyImage;

	/** result image * */
	private ModelImage m_kDTIColorImage;

	/** Diffusion Tensor image. */
	private ModelImage m_kDTIImage = null;

	/** EigenValue image * */
	private ModelImage m_kEigenValueImage;

	/** Eigenvector image * */
	private ModelImage m_kEigenVectorImage;

	/** Tract input file. */
	private File m_kTractFile = null;

	/** Fiber bundle tract file input path name text box. */
	private JTextField m_kTractPath;

	/** main panel * */
	private JPanel mainPanel;

	/** Parent dialog, when this panel is created as a stand-alone dialog: * */
	private final JDialog parentFrame;

	/** Text boxes for the tensor, color image, eigen vector, eigen value, and functional anisotropy files: */
	private JTextField textDTIimage, textDTIColorImage, textEVimage, textEValueImage, textFAimage, textT2image;

	/**
	 * Creates the DTI Visualization panel inside the parent dialog.
	 * @param parent Dialog containing this panel.
	 */
	public JPanelDTIVisualization(JDialog parent, boolean bStandAlone ) {
		super(new GridBagLayout());
		parentFrame = parent;
		init(bStandAlone);
	}

	public void actionPerformed(final ActionEvent event) {
		final String command = event.getActionCommand();

		if (command.equals("tractLoad")) {
			loadTractFile();
		} else if (command.equals("browseDTIFile")) {
			loadDTIFile();
		} else if (command.equals("browseDTIColorFile")) {
			loadDTIColorFile();
		} else if (command.equals("browseEVFile")) {
			loadEVFile();
		} else if (command.equals("browseFAFile")) {
			loadFAFile();
		} else if (command.equals("browseT2File")) {
			loadT2File();
		} else if (command.equals("browseEValueFile")) {
			loadEValueFile();
		} else if (command.equalsIgnoreCase("compute")) {       
			if ( m_kDTIColorImage == null )
			{
				final FileIO fileIO = new FileIO();
				m_kDTIColorImage = fileIO.readImage(textDTIColorImage.getText());

				if ( m_kT2Image != null )
				{
					if ( !VolumeImage.checkImage( m_kT2Image, m_kDTIColorImage ) )
					{
						MipavUtil.displayError( "T2 image must match extents, resolutions, and units as " + m_kDTIColorImage.getImageName() );
						m_kT2Image.disposeLocal(false);
						m_kT2Image = null;
						return;
					}
				}
			}
			if ( m_kEigenVectorImage == null )
			{
				final FileIO fileIO = new FileIO();
				m_kEigenVectorImage = fileIO.readImage(textEVimage.getText());
			}
			if ( m_kEigenValueImage == null )
			{
				final FileIO fileIO = new FileIO();
				m_kEigenValueImage = fileIO.readImage(textEValueImage.getText());
			}
			if ( m_kAnisotropyImage == null )
			{
				final FileIO fileIO = new FileIO();
				m_kAnisotropyImage = fileIO.readImage(textFAimage.getText());
			}

			new VolumeTriPlanarInterfaceDTI( m_kDTIColorImage, m_kT2Image, m_kDTIImage, 
						m_kEigenVectorImage, m_kEigenValueImage, m_kAnisotropyImage );

			if ( parentFrame != null )
			{
				parentFrame.setVisible(false);
				parentFrame.dispose();
			}
			return;
		} else if ( command.equals("cancel") ) {
			disposeLocal(true);
			if ( parentFrame != null )
			{
				parentFrame.setVisible(false);
				parentFrame.dispose();
			}
			return;
		}
		enableLoad();
	}

	/**
	 * Dispose memory.
	 */
	public void disposeLocal(boolean bDispose) {

		if ( (m_kAnisotropyImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kAnisotropyImage) == null)  )
		{
			if ( bDispose ) {
				m_kAnisotropyImage.disposeLocal();
			}
			m_kAnisotropyImage = null;
		}
		if ( (m_kDTIColorImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kDTIColorImage) == null) )
		{
			if ( bDispose ) {
				m_kDTIColorImage.disposeLocal();
			}
			m_kDTIColorImage = null;
		}
		if ( (m_kDTIImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kDTIImage) == null)  )
		{
			if ( bDispose ) {
				m_kDTIImage.disposeLocal();
			}
			m_kDTIImage = null;
		}            
		if ( (m_kEigenValueImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kEigenValueImage) == null) )
		{
			if ( bDispose ) {
				m_kEigenValueImage.disposeLocal();
			}
			m_kEigenValueImage = null;
		}
		if ( (m_kEigenVectorImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kEigenVectorImage) == null) )
		{
			if ( bDispose ) {
				m_kEigenVectorImage.disposeLocal();
			}
			m_kEigenVectorImage = null;
		}
	}

	public void enableLoad()
	{
		if ( !textDTIimage.getText().isEmpty() && !textDTIColorImage.getText().isEmpty() && 
				!textFAimage.getText().isEmpty() && !textEVimage.getText().isEmpty() &&
				!textEValueImage.getText().isEmpty() /* &&  !m_kTractPath.getText().isEmpty() */ )
		{
			loadButton.setEnabled(true);
		}
	}

	/**
	 * Set the DTI Color image
	 * @param DTI Color Image
	 */
	public void setDTIColorImage( ModelImage dtiColorImage )
	{
		m_kDTIColorImage = dtiColorImage;
		if ( dtiColorImage != null )
		{
			textDTIColorImage.setText(m_kDTIColorImage.getImageDirectory() + File.separator + m_kDTIColorImage.getImageName() );
		}
	}

	/**
	 * Set the DTI image
	 * @param DTI image
	 */
	public void setDTIImage( ModelImage dtiImage )
	{
		m_kDTIImage = dtiImage;
		if ( dtiImage != null )
		{
			textDTIimage.setText(m_kDTIImage.getImageDirectory() + File.separator + m_kDTIImage.getImageFileName());
		}
	}

	/**
	 * Set the eigen value image
	 * @param eigen value image
	 */
	public void setEValueImage( ModelImage evImage )
	{
		m_kEigenValueImage = evImage;
		if ( evImage != null )
		{
			textEValueImage.setText(m_kEigenValueImage.getImageDirectory() + File.separator + m_kEigenValueImage.getImageFileName());
		}
	}

	/**
	 * Set the eigen vector image
	 * @param eigen vector image
	 */
	public void setEVImage( ModelImage evImage )
	{
		m_kEigenVectorImage = evImage;
		if ( evImage != null )
		{
			textEVimage.setText(m_kEigenVectorImage.getImageDirectory() + File.separator + m_kEigenVectorImage.getImageFileName());
		}
	}

	/**
	 * Set the functional anisotropy image
	 * @param eigen vector image
	 */
	public void setFAImage( ModelImage faImage )
	{
		m_kAnisotropyImage = faImage;
		if ( faImage != null )
		{
			textFAimage.setText(m_kAnisotropyImage.getImageDirectory() + File.separator + m_kAnisotropyImage.getImageFileName());
		}
	}

	public void setTractFile( String tractFileName )
	{
		//m_kTractPath.setText(tractFileName);
	}

	private void buildDTIColorLoadPanel() {

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

		JButton openDTIColorImageButton = new JButton("Browse");
		openDTIColorImageButton.setToolTipText("Browse Diffusion Tensor color image file");
		openDTIColorImageButton.addActionListener(this);
		openDTIColorImageButton.setActionCommand("browseDTIColorFile");
		openDTIColorImageButton.setEnabled(true);

		textDTIColorImage = new JTextField();
		textDTIColorImage.setPreferredSize(new Dimension(275, 21));
		textDTIColorImage.setEditable(true);
		textDTIColorImage.setBackground(Color.white);
		textDTIColorImage.setFont(MipavUtil.font12);

		JLabel dtiColorFileLabel = new JLabel("Color Image: ");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(dtiColorFileLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(textDTIColorImage, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openDTIColorImageButton, gbc);

		mainPanel.add(DTIloadPanel);
	}

	private void buildDTILoadPanel() {

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

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(dtiFileLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(textDTIimage, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openDTIimageButton, gbc);

		mainPanel.add(DTIloadPanel);
	}

	private void buildEValueLoadPanel() {

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

		JButton openEValueImageButton = new JButton("Browse");
		openEValueImageButton.setToolTipText("Browse EigenValue image file");
		openEValueImageButton.addActionListener(this);
		openEValueImageButton.setActionCommand("browseEValueFile");
		openEValueImageButton.setEnabled(true);

		textEValueImage = new JTextField();
		textEValueImage.setPreferredSize(new Dimension(275, 21));
		textEValueImage.setEditable(true);
		textEValueImage.setBackground(Color.white);
		textEValueImage.setFont(MipavUtil.font12);

		JLabel dtiEValueFileLabel = new JLabel("EValue Image : ");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(dtiEValueFileLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(textEValueImage, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openEValueImageButton, gbc);

		mainPanel.add(DTIloadPanel);
	}

	private void buildEVLoadPanel() {

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

		JButton openEVimageButton = new JButton("Browse");
		openEVimageButton.setToolTipText("Browse EigenVector image file");
		openEVimageButton.addActionListener(this);
		openEVimageButton.setActionCommand("browseEVFile");
		openEVimageButton.setEnabled(true);

		textEVimage = new JTextField();
		textEVimage.setPreferredSize(new Dimension(275, 21));
		textEVimage.setEditable(true);
		textEVimage.setBackground(Color.white);
		textEVimage.setFont(MipavUtil.font12);

		JLabel dtiEVFileLabel = new JLabel("EVector Image: ");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(dtiEVFileLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(textEVimage, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openEVimageButton, gbc);

		mainPanel.add(DTIloadPanel);
	}

	private void buildFALoadPanel() {

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

		JButton openFAimageButton = new JButton("Browse");
		openFAimageButton.setToolTipText("Browse Functinal Anisotropy image file");
		openFAimageButton.addActionListener(this);
		openFAimageButton.setActionCommand("browseFAFile");
		openFAimageButton.setEnabled(true);

		textFAimage = new JTextField();
		textFAimage.setPreferredSize(new Dimension(275, 21));
		textFAimage.setEditable(true);
		textFAimage.setBackground(Color.white);
		textFAimage.setFont(MipavUtil.font12);

		JLabel dtiFAFileLabel = new JLabel("FA Image: ");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(dtiFAFileLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(textFAimage, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openFAimageButton, gbc);
		mainPanel.add(DTIloadPanel);
	}

	private void buildT2LoadPanel() {

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

		JButton openT2imageButton = new JButton("Browse");
		openT2imageButton.setToolTipText("Browse T2 image file");
		openT2imageButton.addActionListener(this);
		openT2imageButton.setActionCommand("browseT2File");
		openT2imageButton.setEnabled(true);

		textT2image = new JTextField();
		textT2image.setPreferredSize(new Dimension(275, 21));
		textT2image.setEditable(true);
		textT2image.setBackground(Color.white);
		textT2image.setFont(MipavUtil.font12);

		JLabel dtiFAFileLabel = new JLabel("T2 Image: ");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(dtiFAFileLabel, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(textT2image, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openT2imageButton, gbc);
		mainPanel.add(DTIloadPanel);
	}

	private void init(boolean bStandAlone) {

		mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

		buildDTILoadPanel();
		buildDTIColorLoadPanel();
		buildEVLoadPanel();
		buildFALoadPanel();
		buildEValueLoadPanel();
		buildT2LoadPanel();
		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.fill = GridBagConstraints.NONE;
		gbc2.weightx = 1;
		gbc2.weighty = 1;
		gbc2.gridx = 0;
		gbc2.gridy = 0;
		gbc2.anchor = GridBagConstraints.NORTHWEST;
		this.add(mainPanel, gbc2);

		// build button panel
		final GridBagLayout kGBL = new GridBagLayout();
		final JPanel buttonPanel = new JPanel(kGBL);
		final GridBagConstraints gbc = new GridBagConstraints();

		loadButton = new JButton("Load");
		loadButton.setToolTipText("Load");
		loadButton.addActionListener(this);
		loadButton.setActionCommand("compute");
		loadButton.setVisible(true);
		loadButton.setEnabled(false);
		gbc.gridx = 0;
		gbc.gridy = 0;
		buttonPanel.add(loadButton, gbc);

		if ( bStandAlone )
		{
			JButton cancelButton = new JButton("Cancel");
			cancelButton.setToolTipText("Cancel");
			cancelButton.addActionListener(this);
			cancelButton.setActionCommand("cancel");
			cancelButton.setVisible(true);
			cancelButton.setEnabled(true);
			gbc.gridx++;
			gbc.gridy = 0;
			buttonPanel.add(cancelButton, gbc);
		}
		mainPanel.add(buttonPanel);

	}

	private void loadDTIColorFile() {
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
		chooser.setDialogTitle("Choose Diffusion Tensor Color image file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			final FileIO fileIO = new FileIO();

			m_kDTIColorImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
					+ File.separator);
			
			if ( m_kDTIColorImage != null && m_kT2Image != null )
			{
				if ( !VolumeImage.checkImage( m_kT2Image, m_kDTIColorImage ) )
				{
					MipavUtil.displayError( "Color Map image must match extents, resolutions, and units as " + m_kT2Image.getImageName() );
					m_kDTIColorImage.disposeLocal(false);
					m_kDTIColorImage = null;
					return;
				}
			}

			textDTIColorImage.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}
	}

	/**
	 * Launches the JFileChooser for the user to select the Diffusion Tensor Image. Loads the tensor data.
	 */
	private void loadDTIFile() {
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
		chooser.setDialogTitle("Choose Diffusion Tensor file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			final FileIO fileIO = new FileIO();

			m_kDTIImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
					+ File.separator);

			textDTIimage.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());


			String dir = chooser.getSelectedFile().getParent() + File.separator;
			File test = new File( dir + JPanelDTIFiberTracking.ColorMapImageName + ".xml" );
			if ( test != null && test.exists() )
			{
				textDTIColorImage.setText(test.getAbsolutePath());
			}
			test = new File( dir + JPanelDTIFiberTracking.FAImageName + ".xml");
			if ( test != null && test.exists() )
			{
				textFAimage.setText(test.getAbsolutePath());
			}
			test = new File( dir + JPanelDTIFiberTracking.EigenVectorImageName + ".xml");
			if ( test != null && test.exists() )
			{
				textEVimage.setText(test.getAbsolutePath());
			}
			test = new File( dir + JPanelDTIFiberTracking.EigenValueImageName + ".xml");
			if ( test != null && test.exists() )
			{
				textEValueImage.setText(test.getAbsolutePath());
			}
			/*
            test = new File( dir + JPanelDTIFiberTracking.TrackFileName);
            if ( test != null && test.exists() )
            {
            	m_kTractPath.setText(test.getAbsolutePath());
            }
			 */
		}
	}

	private void loadEValueFile() {
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
		chooser.setDialogTitle("Choose EigenVector image file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			final FileIO fileIO = new FileIO();

			m_kEigenValueImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
					+ File.separator);
			
			textEValueImage.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}
	}

	private void loadEVFile() {
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
		chooser.setDialogTitle("Choose EigenVector image file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			final FileIO fileIO = new FileIO();

			m_kEigenVectorImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
					+ File.separator);

			textEVimage.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}
	}

	private void loadFAFile() {
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
		chooser.setDialogTitle("Choose Functional Anisotropy image file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			final FileIO fileIO = new FileIO();

			m_kAnisotropyImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
					+ File.separator);

			textFAimage.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}
	}

	private void loadT2File() {
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
		chooser.setDialogTitle("Choose T2 image file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			final FileIO fileIO = new FileIO();

			m_kT2Image = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
					+ File.separator);

			if ( m_kDTIColorImage != null && m_kT2Image != null )
			{
				if ( !VolumeImage.checkImage( m_kT2Image, m_kDTIColorImage ) )
				{
					MipavUtil.displayError( "T2 image must match extents, resolutions, and units as " + m_kDTIColorImage.getImageName() );
					m_kT2Image.disposeLocal(false);
					m_kT2Image = null;
					return;
				}
			}
			
			textT2image.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}
	}

	/**
	 * Launches the JFileChooser for the user to select the tract file. Stores the File for the tract file but does not
	 * read the file.
	 */
	private void loadTractFile() {
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
		chooser.setDialogTitle("Choose Diffusion Tensor Tract file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			m_kTractFile = new File(chooser.getSelectedFile().getAbsolutePath());
			if ( !m_kTractFile.exists() || !m_kTractFile.canRead()) {
				m_kTractFile = null;
				return;
			}
			final int iLength = (int) m_kTractFile.length();
			if (iLength <= 0) {
				m_kTractFile = null;
				return;
			}
			m_kTractPath.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}
	}

}


