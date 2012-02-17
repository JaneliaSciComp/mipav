package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.BorderLayout;
import java.awt.Color;
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

public class JPanelDTIVisualization extends JPanel implements ActionListener {
    
    private static final long serialVersionUID = 8011148684175711251L;

    public static void createFrame()
    {
    	JDialog dialog = new JDialog( ViewUserInterface.getReference().getMainFrame(), "Init DTI Visualization" );
    	dialog.add( new JPanelDTIVisualization(dialog) );
    	dialog.pack();
    	dialog.setVisible(true);
    }

    private JButton openDTIimageButton, openDTIColorImageButton, openEVimageButton, openEValueImageButton,
            openFAimageButton;

    private JTextField textDTIimage, textDTIColorImage, textEVimage, textEValueImage, textFAimage;

    private JLabel dtiFileLabel, dtiColorFileLabel, dtiEVFileLabel, dtiEValueFileLabel, dtiFAFileLabel;
        
    private JButton computeButton;

    /** main panel * */
    private JPanel mainPanel;

    /** parent directory for the DTI output images. */
    private String m_kParentDir = null;

    /** Diffusion Tensor image. */
    private ModelImage m_kDTIImage = null;

    /** Eigenvector image * */
    private ModelImage m_kEigenVectorImage;

    /** EigenValue image * */
    private ModelImage m_kEigenValueImage;

    /** Anisotropy image * */
    private ModelImage m_kAnisotropyImage;

    /** Parent dialog, when this panel is created as a stand-alone dialog: * */
    private final JDialog parentFrame;

    /** result image * */
    private ModelImage m_kDTIColorImage;

    /** Tract input file. */
    private File m_kTractFile = null;

    /** For TRACTS dialog: number of tracts to display. */
    private JTextField m_kTractsLimit;

    /** For TRACTS dialog: minimum tract length to display. */
    private JTextField m_kTractsMin;

    /** For TRACTS dialog: maximum tract length to display. */
    private JTextField m_kTractsMax;
    
    /** Fiber bundle tract file input path name text box. */
    private JTextField m_kTractPath;
    
    public JPanelDTIVisualization(JDialog parent) {
        super();
        parentFrame = parent;
        init();
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
        } else if (command.equals("browseEValueFile")) {
            loadEValueFile();
        } else if (command.equalsIgnoreCase("compute")) {       
        	if ( m_kDTIColorImage == null )
        	{
                final FileIO fileIO = new FileIO();
                m_kDTIColorImage = fileIO.readImage(textDTIColorImage.getText());
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
        	
        	
        	VolumeTriPlanarInterfaceDTI dtiViz = new VolumeTriPlanarInterfaceDTI( m_kDTIColorImage, m_kDTIImage, 
        			m_kEigenVectorImage, m_kEigenValueImage, m_kAnisotropyImage );

            m_kTractFile = new File(m_kTractPath.getText());
        	dtiViz.getParamPanel().setTractParams(m_kTractFile, m_kTractsLimit, m_kTractsMin, m_kTractsMax,
                    m_kTractPath, m_kDTIImage);

        	if ( parentFrame != null )
        	{
        		parentFrame.setVisible(false);
        		parentFrame.dispose();
        	}
        }
        if ( !textDTIimage.getText().isEmpty() && !textDTIColorImage.getText().isEmpty() && 
        		!textFAimage.getText().isEmpty() && !textEVimage.getText().isEmpty() &&
        		!textEValueImage.getText().isEmpty() &&  !m_kTractPath.getText().isEmpty() )
        {
        	computeButton.setEnabled(true);
        }
    }

    public void buildDTIColorLoadPanel() {

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

        openDTIColorImageButton = new JButton("Browse");
        openDTIColorImageButton.setToolTipText("Browse Diffusion Tensor color image file");
        openDTIColorImageButton.addActionListener(this);
        openDTIColorImageButton.setActionCommand("browseDTIColorFile");
        openDTIColorImageButton.setEnabled(true);

        textDTIColorImage = new JTextField();
        textDTIColorImage.setPreferredSize(new Dimension(275, 21));
        textDTIColorImage.setEditable(true);
        textDTIColorImage.setBackground(Color.white);
        textDTIColorImage.setFont(MipavUtil.font12);

        dtiColorFileLabel = new JLabel("Color Image: ");
        dtiColorFileLabel.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 0, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;

        DTIloadPanel.add(dtiColorFileLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(textDTIColorImage, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 10, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(openDTIColorImageButton, gbc);

        mainPanel.add(DTIloadPanel);

    }

    public void buildDTILoadPanel() {

        final JPanel DTIloadPanel = new JPanel();
        DTIloadPanel.setLayout(new GridBagLayout());
        DTIloadPanel.setBorder(JInterfaceBase.buildTitledBorder(""));

        final GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.CENTER;
        gbc.anchor = GridBagConstraints.WEST;

        openDTIimageButton = new JButton("Browse");
        openDTIimageButton.setToolTipText("Browse Diffusion Tensor image file");
        openDTIimageButton.addActionListener(this);
        openDTIimageButton.setActionCommand("browseDTIFile");
        openDTIimageButton.setEnabled(true);

        textDTIimage = new JTextField();
        textDTIimage.setPreferredSize(new Dimension(275, 21));
        textDTIimage.setEditable(true);
        textDTIimage.setBackground(Color.white);
        textDTIimage.setFont(MipavUtil.font12);

        dtiFileLabel = new JLabel("Tensor Image: ");
        dtiFileLabel.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 0, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;

        DTIloadPanel.add(dtiFileLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(textDTIimage, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 10, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(openDTIimageButton, gbc);

        mainPanel.add(DTIloadPanel);

    }

   /* public void setDTIimage() {
        parentFrame.setDTIimage(m_kDTIImage);
    }

    public void setEVimage() {
        parentFrame.setEVimage(m_kEigenVectorImage);
    }

    public void setEValueimage() {
        parentFrame.setEValueimage(m_kEigenValueImage);
    }

    public void setFAimage() {
        parentFrame.setFAimage(m_kAnisotropyImage);
    }

    public void setParentDir() {
        parentFrame.setParentDir(m_kParentDir);
    }

    public void setDTIColorImage() {
        parentFrame.setDTIColorImage(m_kDTIColorImage);
    }*/

    public void buildEValueLoadPanel() {

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

        openEValueImageButton = new JButton("Browse");
        openEValueImageButton.setToolTipText("Browse EigenValue image file");
        openEValueImageButton.addActionListener(this);
        openEValueImageButton.setActionCommand("browseEValueFile");
        openEValueImageButton.setEnabled(true);

        textEValueImage = new JTextField();
        textEValueImage.setPreferredSize(new Dimension(275, 21));
        textEValueImage.setEditable(true);
        textEValueImage.setBackground(Color.white);
        textEValueImage.setFont(MipavUtil.font12);

        dtiEValueFileLabel = new JLabel("EValue Image : ");
        dtiEValueFileLabel.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 0, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;

        DTIloadPanel.add(dtiEValueFileLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(textEValueImage, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 10, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(openEValueImageButton, gbc);

        mainPanel.add(DTIloadPanel);

    }
    
    public void buildEVLoadPanel() {

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

        openEVimageButton = new JButton("Browse");
        openEVimageButton.setToolTipText("Browse EigenVector image file");
        openEVimageButton.addActionListener(this);
        openEVimageButton.setActionCommand("browseEVFile");
        openEVimageButton.setEnabled(true);

        textEVimage = new JTextField();
        textEVimage.setPreferredSize(new Dimension(275, 21));
        textEVimage.setEditable(true);
        textEVimage.setBackground(Color.white);
        textEVimage.setFont(MipavUtil.font12);

        dtiEVFileLabel = new JLabel("EVector Image: ");
        dtiEVFileLabel.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 0, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;

        DTIloadPanel.add(dtiEVFileLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(textEVimage, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 10, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(openEVimageButton, gbc);

        mainPanel.add(DTIloadPanel);

    }

    public void buildFALoadPanel() {

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

        openFAimageButton = new JButton("Browse");
        openFAimageButton.setToolTipText("Browse Functinal Anisotropy image file");
        openFAimageButton.addActionListener(this);
        openFAimageButton.setActionCommand("browseFAFile");
        openFAimageButton.setEnabled(true);

        textFAimage = new JTextField();
        textFAimage.setPreferredSize(new Dimension(275, 21));
        textFAimage.setEditable(true);
        textFAimage.setBackground(Color.white);
        textFAimage.setFont(MipavUtil.font12);

        dtiFAFileLabel = new JLabel("FA Image: ");
        dtiFAFileLabel.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 0, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;

        DTIloadPanel.add(dtiFAFileLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(textFAimage, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 10, 10, 0);
        gbc.fill = GridBagConstraints.CENTER;
        DTIloadPanel.add(openFAimageButton, gbc);

        mainPanel.add(DTIloadPanel);

    }



    /**
     * Dispose memory.
     */
    public void disposeLocal() {

    }

    
    public void enableLoad()
    {
        if ( !textDTIimage.getText().isEmpty() && !textDTIColorImage.getText().isEmpty() && 
        		!textFAimage.getText().isEmpty() && !textEVimage.getText().isEmpty() &&
        		!textEValueImage.getText().isEmpty() &&  !m_kTractPath.getText().isEmpty() )
        {
        	computeButton.setEnabled(true);
        }
    }

    public void init() {
        
        mainPanel = new JPanel();

        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        buildDTILoadPanel();
        buildDTIColorLoadPanel();
        buildEVLoadPanel();
        buildFALoadPanel();
        buildEValueLoadPanel();
        buildLoadTractPanel();
        this.add(mainPanel);

        // build button panel
        final GridBagLayout kGBL = new GridBagLayout();
        final JPanel buttonPanel = new JPanel(kGBL);
        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;

        computeButton = new JButton("Load");
        computeButton.setToolTipText("Load");
        computeButton.addActionListener(this);
        computeButton.setActionCommand("compute");
        computeButton.setVisible(true);
        computeButton.setEnabled(false);
        computeButton.setPreferredSize(new Dimension(90, 30));

        buttonPanel.add(computeButton, gbc);

        mainPanel.add(buttonPanel);

    }

    public void loadDTIColorFile() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Diffusion Tensor Color image file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();

            m_kDTIColorImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);

            m_kDTIColorImage = resampleImage(m_kDTIColorImage);

            m_kParentDir = chooser.getCurrentDirectory().getPath();

            textDTIColorImage.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }

    /**
     * Launches the JFileChooser for the user to select the Diffusion Tensor Image. Loads the tensor data.
     */
    public void loadDTIFile() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Diffusion Tensor file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();

            m_kDTIImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);

            m_kDTIImage = resampleImage(m_kDTIImage);

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
            test = new File( dir + JPanelDTIFiberTracking.TrackFileName);
            if ( test != null && test.exists() )
            {
            	m_kTractPath.setText(test.getAbsolutePath());
            }
        }
    }

    public void loadEValueFile() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose EigenVector image file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();

            m_kEigenValueImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);
            m_kEigenValueImage = resampleImage(m_kEigenValueImage);

            textEValueImage.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }

    public void loadEVFile() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose EigenVector image file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();

            m_kEigenVectorImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);

            m_kEigenVectorImage = resampleImage(m_kEigenVectorImage);

            textEVimage.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }

    public void loadFAFile() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose EigenVector image file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();

            m_kAnisotropyImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);
            m_kAnisotropyImage = resampleImage(m_kAnisotropyImage);

            textFAimage.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }

    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     * 
     * @param panelWidth DOCUMENT ME!
     * @param frameHeight DOCUMENT ME!
     */
    public void resizePanel(final int panelWidth, final int frameHeight) {
        mainPanel.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        mainPanel.setSize(new Dimension(panelWidth, frameHeight - 40));
        mainPanel.revalidate();
    }
    
    /**
     * Set the DTI Color image
     * @param DTI Color Image
     */
    public void setDTIColorImage( ModelImage dtiColorImage )
    {
    	m_kDTIColorImage = dtiColorImage;
    	textDTIColorImage.setText(m_kDTIColorImage.getImageDirectory() + File.separator + m_kDTIColorImage.getImageName() );
    }

    /**
     * Set the DTI image
     * @param DTI image
     */
    public void setDTIImage( ModelImage dtiImage )
    {
    	m_kDTIImage = dtiImage;
        textDTIimage.setText(m_kDTIImage.getImageDirectory() + File.separator + m_kDTIImage.getImageFileName());
    }

    /**
     * Set the eigen value image
     * @param eigen value image
     */
    public void setEValueImage( ModelImage evImage )
    {
    	m_kEigenValueImage = evImage;
    	textEValueImage.setText(m_kEigenValueImage.getImageDirectory() + File.separator + m_kEigenValueImage.getImageFileName());
    }

    /**
     * Set the eigen vector image
     * @param eigen vector image
     */
    public void setEVImage( ModelImage evImage )
    {
    	m_kEigenVectorImage = evImage;
    	textEVimage.setText(m_kEigenVectorImage.getImageDirectory() + File.separator + m_kEigenVectorImage.getImageFileName());
    }

    /**
     * Set the functional anisotropy image
     * @param eigen vector image
     */
    public void setFAImage( ModelImage faImage )
    {
    	m_kAnisotropyImage = faImage;
    	textFAimage.setText(m_kAnisotropyImage.getImageDirectory() + File.separator + m_kAnisotropyImage.getImageFileName());
    }

    public void setTractFile( String tractFileName )
    {
        m_kTractPath.setText(tractFileName);
    }

    public void setTractParams() {
        //parentFrame.setTractParams(m_kTractFile, m_kTractsLimit, m_kTractsMin, m_kTractsMax, m_kTractPath);
    }

    private void buildLoadTractPanel() {
        final JPanel tractLoadPanel = new JPanel(new BorderLayout());
        tractLoadPanel.setLayout(new GridBagLayout());
        tractLoadPanel.setBorder(JInterfaceBase.buildTitledBorder(""));

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 0, 10, 0);

        final JPanel kParamsPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        final JLabel kNumberTractsLimit = new JLabel("Maximum number of tracts to display:");
        kParamsPanel.add(kNumberTractsLimit, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kTractsLimit = new JTextField("100", 5);
        m_kTractsLimit.setBackground(Color.white);
        kParamsPanel.add(m_kTractsLimit, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        final JLabel m_kTractsMinLength = new JLabel("Minimum tract length:");
        kParamsPanel.add(m_kTractsMinLength, gbc);
        gbc.gridx++;
        m_kTractsMin = new JTextField("50", 5);
        m_kTractsMin.setBackground(Color.white);
        kParamsPanel.add(m_kTractsMin, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        final JLabel m_kTractsMaxLength = new JLabel("Maximum tract length:");
        kParamsPanel.add(m_kTractsMaxLength, gbc);
        gbc.gridx++;
        m_kTractsMax = new JTextField("100", 5);
        m_kTractsMax.setBackground(Color.white);
        kParamsPanel.add(m_kTractsMax, gbc);

        final JPanel filesPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.insets = new Insets(0, 0, 10, 0);
        gbc.fill = GridBagConstraints.NONE;

        final JLabel kTractLabel = new JLabel(" DTI tract file: ");
        filesPanel.add(kTractLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        m_kTractPath = new JTextField();
        m_kTractPath.setPreferredSize(new Dimension(275, 21));
        m_kTractPath.setEditable(true);
        m_kTractPath.setBackground(Color.white);
        filesPanel.add(m_kTractPath, gbc);

        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(0, 10, 10, 0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        final JButton kTractLoadButton = new JButton("Browse");
        kTractLoadButton.addActionListener(this);
        kTractLoadButton.setActionCommand("tractLoad");

        filesPanel.add(kTractLoadButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;

        tractLoadPanel.add(kParamsPanel, gbc);
        gbc.gridy = 5;
        tractLoadPanel.add(filesPanel, gbc);

        mainPanel.add(tractLoadPanel);

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
            // System.err.println("ruida: " + m_kTractFile.getName());
            m_kTractPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }

    /**
     * Resample 4D image to power of 2
     * 
     * @param srcImage source image need to be resampled
     */
    private ModelImage resampleImage(ModelImage srcImage) {
    	/*
        final int[] extents = srcImage.getExtents();
        final float[] res = srcImage.getFileInfo(0).getResolutions();
        float[] saveRes;

        if (extents.length == 3) {
            saveRes = new float[] {res[0], res[1], res[2]};
        } else {
            saveRes = new float[] {res[0], res[1], res[2], res[3]};
        }

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
            AlgorithmTransform transformFunct = new AlgorithmTransform(srcImage, new TransMatrix(4),
                    AlgorithmTransform.TRILINEAR, newRes[0], newRes[1], newRes[2], volExtents[0], volExtents[1],
                    volExtents[2], false, true, false);
            transformFunct.setRunningInSeparateThread(false);
            transformFunct.run();

            if (transformFunct.isCompleted() == false) {
                transformFunct.finalize();
                transformFunct = null;
            }

            srcImage = transformFunct.getTransformedImage();
            srcImage.calcMinMax();

        }
        */
        return srcImage;

    }

}


