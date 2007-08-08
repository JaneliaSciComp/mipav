import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowEvent;
import java.awt.image.PixelGrabber;
import java.io.File;
import java.io.IOException;
import java.util.Calendar;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.JViewport;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewControlsImage;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJComponentDTIImage;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

/**
 * @author pandyan
 * 
 * This is the main dialog for the DTI Color Display
 * 
 * References: This algorithm was developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group and
 * Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL)
 * Division of Cumputational Bioscience (DCB)
 * Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInDialogDTIColorDisplay extends ViewJFrameBase
		implements AlgorithmInterface, ChangeListener, ItemListener, MouseListener, MouseWheelListener {
	
	/** dialog title and version **/
	private String title = " DTI Color Display  v1.1      ";
	
	/** panels **/
	private JPanel mainPanel,topPanel, filesPanel, okPanel, bottomPanel, colorPanel, colorWheelPanel, resultImagePanel, optionsPanel, colorWheelChoicesPanel, heuristicParametersPanel, anisotropyMaxPanel, anisotropyMinPanel, gammaPanel, stevensBetaPanel, satBluePanel, dimGreenPanel, colorRangePanel, satVsThetaPanel, resultPanel, resultImageSliderPanel, tempPanel, toolbarPanel, adjustExpPanel, truncMultPanel;
	
	/** labels **/
	private JLabel eigenvectorLabel, anisotropyLabel, minAnisotropyMaxLabel, maxAnisotropyMaxLabel, minAnisotropyMinLabel, maxAnisotropyMinLabel, minGammaLabel, maxGammaLabel, minStevensBetaLabel, maxStevensBetaLabel, minSatBlueLabel, maxSatBlueLabel, minDimGreenLabel, maxDimGreenLabel, minColorRangeLabel, maxColorRangeLabel, minSatVsThetaLabel, maxSatVsThetaLabel, minResultImageSlicesLabel, maxResultImageSlicesLabel, magLabel, refLabel, minAdjustExpLabel, maxAdjustExpLabel;
	
	/** scroll pane for result image **/
    private JScrollPane resultScrollPanel;
    
	/** textfields **/
	private JTextField eigenvectorPath,anisotropyPath, anisotropyMaxTextField, anisotropyMinTextField, gammaTextField, stevensBetaTextField, satBlueTextField, dimGreenTextField, colorRangeTextField, satVsThetaTextField, adjustExpTextField;
	
	/** buttons **/
	private JButton eigenvectorBrowseButton,anisotropyBrowseButton, okButton, magButton, unMagButton, zoomOneButton, captureImageButton;
	
	/** current directory  **/
    //private String currDir = null;
    
    /** eigenvector src image **/
    private ModelImage eigvecSrcImage;
    
    /** anisotropy src image **/
    private ModelImage anisotropyImage;
    
    /** names of eigenvector and anisotropy files**/
    private String eigvecFilename, anisotropyFilename;
    
    /** result image **/
    private ModelImage resultImage;
    
    /** LUT of input image **/
    private ModelLUT LUTa;
    
    /** Color Wheels **/
    private ColorWheel currentColorWheel,absValColorWheel,noSymmColorWheel, rotationalSymmColorWheel, mirrorSymmColorWheel; 
    
    /** handle to the algorithm **/
    private PlugInAlgorithmDTIColorDisplay alg;
    
    /** GridBagLayout **/
    private GridBagLayout gbl;
    
    /** GridbagConstraints **/
    private GridBagConstraints gbc;
    
    /** various sliders in dialog **/
    public JSlider anisotropyMaxSlider, anisotropyMinSlider, gammaSlider, stevensBetaSlider, satBlueSlider, dimGreenSlider, colorRangeSlider, satVsThetaSlider, resultImageSlider, adjustExpSlider;
    
    /** titled border for certain components **/
    private TitledBorder titledBorder;
    
    /** color wheel choices combo box **/
    private JComboBox colorWheelComboBox;
    
    /** anisotropy max **/
    private float anisotropyMax = 0.7f;
    
    /** anisotropy min **/
    private float anisotropyMin = 0.0f;
    
    /** adjust exp **/
    private float adjustExp = 0.5f;
    
    /** gamma correction **/
    private float gamma = 1.8f;
    
    /** steven's beta **/
    private float stevensBeta = 0.4f;
    
    /** blue saturation **/
    private float pB = 0.35f;
    
    /** green saturation **/
    private float pG = 0.8f;
    
    /** color range **/
    private float pC = 0.7f;
    
    /** saturation vs theta **/    
    private float pS = 0.5f;
    
    /** ViewJComponentDTIImage **/
    private ViewJComponentDTIImage componentImage;
    
    /** Buffer used to store image intensities the presently viewed slice of image A. */
    protected float[] imageBufferA;
    
    /** Storage of the image voxel resolutions. One resolution value per dimension. */
    protected float[] resols;
    
    /** Integer buffer (4 bytes that stores the concatenated Alpha (1 byte), Red (1 byte), Green ( 1 byte ), Blue (1 byte ) data. The ARGB values are generated by using the imageA intensities as a index into a LUT. */
    protected int[] pixBuffer;
    
    /** Storage of the resolution units of measure. For example, mm, cm, inches ... */
    protected int[] units;
    
    /** current z slice **/
    private int zSlice;
    
    /** Storage for correction parameters where datasets have non isotropic values. */
    protected float widthResFactor;
    
    /** Storage for correction parameters where datasets have non isotropic values. */
    protected float heightResFactor;
    
    /** Image time sequence number of image that is displayed. Zero indexed based. */
    protected int tSlice;
    
    /** Number of slices in a 3D dataset. */
    protected int nImage;

    /** Number of time sequences in a 4D dataset. */
    protected int nTImage;
    
    /** type of color wheel 	ABSVAL, NOSYMM, ROTATIONALSYMM, MIRRORSYMM  **/
    private String type;
    
    /** ViewToolBarBuilder **/
    private ViewToolBarBuilder toolbarBuilder;
    
    /** current zoom for result image **/
    private float zoom = 1.0f;
    
    /** num slices for result image **/
    private int numSlices;
    
    /** rectangle for screen capture**/
    private Rectangle currentRectangle;
    
    /** radio group for truncate/multiply **/
    private ButtonGroup truncMultRadioGroup;
    
    /** radio buttons for truncate/multiply **/
    private JRadioButton truncRadio, multRadio;
    
    /** boolean for truncate/multiply **/
    private boolean isMultiply = true;
    
    
	/**
	 * Constructor
	 */
	public PlugInDialogDTIColorDisplay(boolean modal) {
		super(null,null);
		init();
	}
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        setTitle(title);
        
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        
        //topPanel
        topPanel = new JPanel(gbl);
        filesPanel = new JPanel(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,5,5);
        eigenvectorLabel = new JLabel(" eigenvector file: ");
        filesPanel.add(eigenvectorLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
		eigenvectorPath = new JTextField(35);
		eigenvectorPath.setEditable(false);
		eigenvectorPath.setBackground(Color.white);
		filesPanel.add(eigenvectorPath, gbc);
		gbc.gridx = 2;
		gbc.gridy = 0;
		eigenvectorBrowseButton = new JButton("Browse");
		eigenvectorBrowseButton.addActionListener(this);
		eigenvectorBrowseButton.setActionCommand("eigenvectorBrowse");
		filesPanel.add(eigenvectorBrowseButton, gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		anisotropyLabel = new JLabel(" anisotropy file: ");
        filesPanel.add(anisotropyLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        anisotropyPath = new JTextField(35);
        anisotropyPath.setEditable(false);
        anisotropyPath.setBackground(Color.white);
		filesPanel.add(anisotropyPath, gbc);
		gbc.gridx = 2;
		gbc.gridy = 1;
		anisotropyBrowseButton = new JButton("Browse");
		anisotropyBrowseButton.addActionListener(this);
		anisotropyBrowseButton.setActionCommand("anisotropyBrowse");
		filesPanel.add(anisotropyBrowseButton, gbc);
		okPanel = new JPanel();
		okButton = new JButton("OK");
		okButton.addActionListener(this);
		okButton.setActionCommand("ok");
		okPanel.add(okButton);
		gbc.insets = new Insets(0,0,0,0);
		gbc.gridx = 0;
        gbc.gridy = 0;
        topPanel.add(filesPanel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        topPanel.add(okPanel, gbc);
		 
		//color panel
		colorPanel = new JPanel(gbl);
		colorWheelPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		colorWheelPanel.setBorder(titledBorder);
		colorWheelPanel.setPreferredSize(new Dimension(400, 400));
		colorWheelPanel.setMinimumSize(new Dimension(400,400));
		absValColorWheel = new ColorWheel("ABSVAL",150);
		currentColorWheel = absValColorWheel;
		gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,0,0);
        gbc.anchor = GridBagConstraints.CENTER;
		colorWheelPanel.add(currentColorWheel,gbc);
		colorWheelChoicesPanel = new JPanel(gbl);
		colorWheelChoicesPanel.setForeground(Color.black);
		titledBorder = new TitledBorder(new EtchedBorder(), " Color Wheel ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		colorWheelChoicesPanel.setBorder(titledBorder);	
		colorWheelComboBox = new JComboBox();
		colorWheelComboBox.addItem(" Absolute Value ");
		colorWheelComboBox.addItem(" No Symmetry ");
		colorWheelComboBox.addItem(" Rotational Symmetry ");
		colorWheelComboBox.addItem(" Mirror Symmetry ");
		colorWheelComboBox.addItemListener(this);
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(0,0,10,0);
		colorWheelChoicesPanel.add(colorWheelComboBox,gbc);
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(0,0,0,0);
		colorPanel.add(colorWheelPanel,gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		colorPanel.add(colorWheelChoicesPanel,gbc);

		//options panel
        gbc.anchor = GridBagConstraints.NORTHWEST;
		optionsPanel = new JPanel(gbl);
		
		//heuristic parameters panel
		heuristicParametersPanel = new JPanel(gbl);
		heuristicParametersPanel.setForeground(Color.black);
		titledBorder = new TitledBorder(new EtchedBorder(), " Heuristic Parameters ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		heuristicParametersPanel.setBorder(titledBorder);
		//anisotropy max
		anisotropyMaxPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " Anisotropy Max ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		anisotropyMaxPanel.setBorder(titledBorder);
		anisotropyMaxSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 700);
		anisotropyMaxSlider.setMajorTickSpacing(100);
		anisotropyMaxSlider.setPaintTicks(true);
		anisotropyMaxSlider.addChangeListener(this);
		anisotropyMaxSlider.addMouseListener(this);
		anisotropyMaxSlider.addMouseWheelListener(this);
		maxAnisotropyMaxLabel = new JLabel("1.0");
		maxAnisotropyMaxLabel.setForeground(Color.black);
		maxAnisotropyMaxLabel.setFont(MipavUtil.font12);
		minAnisotropyMaxLabel = new JLabel("0.0");
		minAnisotropyMaxLabel.setForeground(Color.black);
		minAnisotropyMaxLabel.setFont(MipavUtil.font12);
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		anisotropyMaxPanel.add(anisotropyMaxSlider,gbc);
		anisotropyMaxTextField = new JTextField(8);
		anisotropyMaxTextField.setEditable(false);
		anisotropyMaxTextField.setBackground(Color.white);
		anisotropyMaxTextField.setText(String.valueOf((float) (anisotropyMaxSlider.getValue() / 1000.000000f)));
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		anisotropyMaxPanel.add(anisotropyMaxTextField,gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		anisotropyMaxPanel.add(minAnisotropyMaxLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		anisotropyMaxPanel.add(maxAnisotropyMaxLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(anisotropyMaxPanel, gbc);
		//anisotropy min
		anisotropyMinPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " Anisotropy Min ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		anisotropyMinPanel.setBorder(titledBorder);
		anisotropyMinSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 0);
		anisotropyMinSlider.setMajorTickSpacing(100);
		anisotropyMinSlider.setPaintTicks(true);
		anisotropyMinSlider.addChangeListener(this);
		anisotropyMinSlider.addMouseListener(this);
		anisotropyMinSlider.addMouseWheelListener(this);
		maxAnisotropyMinLabel = new JLabel("1.0");
		maxAnisotropyMinLabel.setForeground(Color.black);
		maxAnisotropyMinLabel.setFont(MipavUtil.font12);
		minAnisotropyMinLabel = new JLabel("0.0");
		minAnisotropyMinLabel.setForeground(Color.black);
		minAnisotropyMinLabel.setFont(MipavUtil.font12);
        gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;;
		anisotropyMinPanel.add(anisotropyMinSlider,gbc);
		anisotropyMinTextField = new JTextField(8);
		anisotropyMinTextField.setEditable(false);
		anisotropyMinTextField.setBackground(Color.white);
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		anisotropyMinPanel.add(anisotropyMinTextField,gbc);
		anisotropyMinTextField.setText(String.valueOf((float) (anisotropyMinSlider.getValue() / 1000.000000f)));
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		anisotropyMinPanel.add(minAnisotropyMinLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		anisotropyMinPanel.add(maxAnisotropyMinLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(anisotropyMinPanel, gbc);
		//gamma
		gammaPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " Gamma Correction ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		gammaPanel.setBorder(titledBorder);
		gammaSlider = new JSlider(JSlider.HORIZONTAL, 100, 400, 180);
		gammaSlider.setMajorTickSpacing(50);
		gammaSlider.setPaintTicks(true);
		gammaSlider.addChangeListener(this);
		gammaSlider.addMouseListener(this);
		gammaSlider.addMouseWheelListener(this);
		maxGammaLabel = new JLabel("4");
		maxGammaLabel.setForeground(Color.black);
		maxGammaLabel.setFont(MipavUtil.font12);
		minGammaLabel = new JLabel("1");
		minGammaLabel.setForeground(Color.black);
		minGammaLabel.setFont(MipavUtil.font12);
        gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;;
		gammaPanel.add(gammaSlider,gbc);
		gammaTextField = new JTextField(8);
		gammaTextField.setEditable(false);
		gammaTextField.setBackground(Color.white);
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gammaPanel.add(gammaTextField,gbc);
		gammaTextField.setText(String.valueOf((float) (gammaSlider.getValue() / 100.000000f)));
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		gammaPanel.add(minGammaLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		gammaPanel.add(maxGammaLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(gammaPanel, gbc);
		//blue sat
		satBluePanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " pB Sat. Blue ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		satBluePanel.setBorder(titledBorder);
		satBlueSlider = new JSlider(JSlider.HORIZONTAL, 0, 500, 350);
		satBlueSlider.setMajorTickSpacing(50);
		satBlueSlider.setPaintTicks(true);
		satBlueSlider.addChangeListener(this);
		satBlueSlider.addMouseListener(this);
		satBlueSlider.addMouseWheelListener(this);
		maxSatBlueLabel = new JLabel("0.5");
		maxSatBlueLabel.setForeground(Color.black);
		maxSatBlueLabel.setFont(MipavUtil.font12);
		minSatBlueLabel = new JLabel("0.0");
		minSatBlueLabel.setForeground(Color.black);
		minSatBlueLabel.setFont(MipavUtil.font12);
        gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;;
		satBluePanel.add(satBlueSlider,gbc);
		satBlueTextField = new JTextField(8);
		satBlueTextField.setEditable(false);
		satBlueTextField.setBackground(Color.white);
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		satBluePanel.add(satBlueTextField,gbc);
		satBlueTextField.setText(String.valueOf((float) (satBlueSlider.getValue() / 1000.000000f)));
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		satBluePanel.add(minSatBlueLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		satBluePanel.add(maxSatBlueLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(satBluePanel, gbc);
		//green dim
		dimGreenPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " pG Dim Green ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		dimGreenPanel.setBorder(titledBorder);
		dimGreenSlider = new JSlider(JSlider.HORIZONTAL, 0, 300, 80);
		dimGreenSlider.setMajorTickSpacing(50);
		dimGreenSlider.setPaintTicks(true);
		dimGreenSlider.addChangeListener(this);
		dimGreenSlider.addMouseListener(this);
		dimGreenSlider.addMouseWheelListener(this);
		maxDimGreenLabel = new JLabel("3.0");
		maxDimGreenLabel.setForeground(Color.black);
		maxDimGreenLabel.setFont(MipavUtil.font12);
		minDimGreenLabel = new JLabel("0.0");
		minDimGreenLabel.setForeground(Color.black);
		minDimGreenLabel.setFont(MipavUtil.font12);
        gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;;
		dimGreenPanel.add(dimGreenSlider,gbc);
		dimGreenTextField = new JTextField(8);
		dimGreenTextField.setEditable(false);
		dimGreenTextField.setBackground(Color.white);
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		dimGreenPanel.add(dimGreenTextField,gbc);
		dimGreenTextField.setText(String.valueOf((float) (dimGreenSlider.getValue() / 100.000000f)));
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		dimGreenPanel.add(minDimGreenLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		dimGreenPanel.add(maxDimGreenLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(dimGreenPanel, gbc);
		//color range
		colorRangePanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " pC Color Range ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		colorRangePanel.setBorder(titledBorder);
		colorRangeSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 700);
		colorRangeSlider.setMajorTickSpacing(100);
		colorRangeSlider.setPaintTicks(true);
		colorRangeSlider.addChangeListener(this);
		colorRangeSlider.addMouseListener(this);
		colorRangeSlider.addMouseWheelListener(this);
		maxColorRangeLabel = new JLabel("1.0");
		maxColorRangeLabel.setForeground(Color.black);
		maxColorRangeLabel.setFont(MipavUtil.font12);
		minColorRangeLabel = new JLabel("0.0");
		minColorRangeLabel.setForeground(Color.black);
		minColorRangeLabel.setFont(MipavUtil.font12);
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		colorRangePanel.add(colorRangeSlider,gbc);
		colorRangeTextField = new JTextField(8);
		colorRangeTextField.setEditable(false);
		colorRangeTextField.setBackground(Color.white);
		colorRangeTextField.setText(String.valueOf((float) (colorRangeSlider.getValue() / 1000.000000f)));
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		colorRangePanel.add(colorRangeTextField,gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		colorRangePanel.add(minColorRangeLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		colorRangePanel.add(maxColorRangeLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(colorRangePanel, gbc);
		//sat vs theta
		satVsThetaPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " pS Sat vs Theta ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		satVsThetaPanel.setBorder(titledBorder);
		satVsThetaSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 500);
		satVsThetaSlider.setMajorTickSpacing(100);
		satVsThetaSlider.setPaintTicks(true);
		satVsThetaSlider.addChangeListener(this);
		satVsThetaSlider.addMouseListener(this);
		satVsThetaSlider.setEnabled(false);
		satVsThetaSlider.addMouseWheelListener(this);
		maxSatVsThetaLabel = new JLabel("1.0");
		maxSatVsThetaLabel.setForeground(Color.black);
		maxSatVsThetaLabel.setFont(MipavUtil.font12);
		minSatVsThetaLabel = new JLabel("0.0");
		minSatVsThetaLabel.setForeground(Color.black);
		minSatVsThetaLabel.setFont(MipavUtil.font12);
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		satVsThetaPanel.add(satVsThetaSlider,gbc);
		satVsThetaTextField = new JTextField(8);
		satVsThetaTextField.setEditable(false);
		satVsThetaTextField.setBackground(Color.lightGray);
		satVsThetaTextField.setText(String.valueOf((float) (satVsThetaSlider.getValue() / 1000.000000f)));
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		satVsThetaPanel.add(satVsThetaTextField,gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		satVsThetaPanel.add(minSatVsThetaLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		satVsThetaPanel.add(maxSatVsThetaLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 6;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(satVsThetaPanel, gbc);
		//adjustExp
		adjustExpPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " Adjust Exp ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		adjustExpPanel.setBorder(titledBorder);
		adjustExpSlider = new JSlider(JSlider.HORIZONTAL, 50, 100, 50);
		adjustExpSlider.setMajorTickSpacing(10);
		adjustExpSlider.setPaintTicks(true);
		adjustExpSlider.addChangeListener(this);
		adjustExpSlider.addMouseListener(this);
		adjustExpSlider.addMouseWheelListener(this);
		maxAdjustExpLabel = new JLabel("1.0");
		maxAdjustExpLabel.setForeground(Color.black);
		maxAdjustExpLabel.setFont(MipavUtil.font12);
		minAdjustExpLabel = new JLabel("0.5");
		minAdjustExpLabel.setForeground(Color.black);
		minAdjustExpLabel.setFont(MipavUtil.font12);
        gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;;
		adjustExpPanel.add(adjustExpSlider,gbc);
		adjustExpTextField = new JTextField(8);
		adjustExpTextField.setEditable(false);
		adjustExpTextField.setBackground(Color.white);
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		adjustExpPanel.add(adjustExpTextField,gbc);
		adjustExpTextField.setText(String.valueOf((float) (adjustExpSlider.getValue() / 100.000000f)));
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		adjustExpPanel.add(minAdjustExpLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		adjustExpPanel.add(maxAdjustExpLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 7;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(adjustExpPanel, gbc);
		//steven's beta
		stevensBetaPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " Stevens Beta ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		stevensBetaPanel.setBorder(titledBorder);
		stevensBetaSlider = new JSlider(JSlider.HORIZONTAL, 30, 60, 40);
		stevensBetaSlider.setMajorTickSpacing(3);
		stevensBetaSlider.setPaintTicks(true);
		stevensBetaSlider.addChangeListener(this);
		stevensBetaSlider.addMouseListener(this);
		stevensBetaSlider.addMouseWheelListener(this);
		maxStevensBetaLabel = new JLabel("0.6");
		maxStevensBetaLabel.setForeground(Color.black);
		maxStevensBetaLabel.setFont(MipavUtil.font12);
		minStevensBetaLabel = new JLabel("0.3");
		minStevensBetaLabel.setForeground(Color.black);
		minStevensBetaLabel.setFont(MipavUtil.font12);
        gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 3;
		gbc.weightx = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;;
		stevensBetaPanel.add(stevensBetaSlider,gbc);
		stevensBetaTextField = new JTextField(8);
		stevensBetaTextField.setEditable(false);
		stevensBetaTextField.setBackground(Color.white);
		gbc.gridx = 3;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		stevensBetaPanel.add(stevensBetaTextField,gbc);
		stevensBetaTextField.setText(String.valueOf((float) (stevensBetaSlider.getValue() / 100.000000f)));
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.NONE;
		stevensBetaPanel.add(minStevensBetaLabel,gbc);
		gbc.gridx = 2;
		gbc.anchor = GridBagConstraints.EAST;
		gbc.weightx = 0;
		stevensBetaPanel.add(maxStevensBetaLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 8;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(stevensBetaPanel, gbc);
		//truncate-multiply
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(0,30,0,30);
		truncMultPanel = new JPanel(gbl);
		titledBorder = new TitledBorder(new EtchedBorder(), " Truncate/Multiply ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		truncMultPanel.setBorder(titledBorder);
		truncMultRadioGroup = new ButtonGroup();
		truncRadio = new JRadioButton("Truncate");
		truncRadio.setSelected(false);
		truncRadio.addActionListener(this);
		truncRadio.setActionCommand("truncRadio");
		truncMultRadioGroup.add(truncRadio);
		gbc.gridx = 0;
		gbc.gridy = 0;
		truncMultPanel.add(truncRadio, gbc);
		multRadio = new JRadioButton("Multiply");
		multRadio.setSelected(true);
		multRadio.addActionListener(this);
		multRadio.setActionCommand("multRadio");
		truncMultRadioGroup.add(multRadio);
		gbc.gridx = 1;
		gbc.gridy = 0;
		truncMultPanel.add(multRadio, gbc);
		gbc.insets = new Insets(0,0,0,0);
		gbc.gridx = 0;
		gbc.gridy = 9;
		gbc.gridwidth = 1;
		heuristicParametersPanel.add(truncMultPanel, gbc);
		gbc.insets = new Insets(0,0,0,0);
		gbc.fill = GridBagConstraints.NONE;
		gbc.gridx = 0;
		gbc.gridy = 1;
		optionsPanel.add(heuristicParametersPanel,gbc);
		
		//temp panel
		tempPanel = new JPanel(gbl);
		tempPanel.setPreferredSize(new Dimension(400, 400));
		tempPanel.setMinimumSize(new Dimension(400,400));
		tempPanel.setForeground(Color.black);
		titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
		tempPanel.setBorder(titledBorder);
		
		//reference label
		refLabel = new JLabel();
		refLabel.setText("<html>Developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group and Dr. Lin-Ching Chang D.Sc.,  Dr. Carlo Pierpaoli MD Ph.D.,  and Lindsay Walker MS from the NIH/NICHD/LIMB/STBB group </html>");

		//bottom panel
        gbc.anchor = GridBagConstraints.NORTH;
        bottomPanel = new JPanel(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;
        bottomPanel.add(colorPanel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        bottomPanel.add(optionsPanel, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        bottomPanel.add(tempPanel, gbc);
        
        //main panel
        mainPanel = new JPanel(gbl);
		gbc.gridx = 0;
        gbc.gridy = 0;
		mainPanel.add(topPanel,gbc);
		gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(bottomPanel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.insets = new Insets(15,5,10,0);
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(refLabel,gbc);
        getContentPane().add(mainPanel);
        
        pack();
        setResizable(false);
        setVisible(true);
        
	}
	
	/**
	 * call algorithm
	 *
	 */
	protected void callAlgorithm() {
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		alg = new PlugInAlgorithmDTIColorDisplay(eigvecSrcImage);
		alg.addListener(this);
		alg.run();
	}


	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			
			//grab the eigenvector and anisotropy file names and clear the textfields
			eigvecFilename = eigvecSrcImage.getImageName();
			anisotropyFilename = anisotropyImage.getImageName();
			eigenvectorPath.setText("");
			anisotropyPath.setText("");
			
			//get result Model Image
			resultImage = alg.getResultImage();
			imageBufferA = initImageBuffer(resultImage.getExtents(), true);
	        pixBuffer = initPixelBuffer(resultImage.getExtents());
	        resols = initResolutions(resultImage);
	        units = initUnits(resultImage);
	        float[] factor = initResFactor(resols, units);
	        widthResFactor = factor[0];
	        heightResFactor = factor[1];
	        zSlice = (resultImage.getExtents()[2] - 1) / 2;
	        zSlice = zSlice - 1;
	        
	        //get data from anisotropy file to send into ViewJComponentEditImage constructor
	        float[] anisotropyBuffer;
	        int length = anisotropyImage.getExtents()[0] * anisotropyImage.getExtents()[1] * anisotropyImage.getExtents()[2];
	        anisotropyBuffer = new float[length];
	        try {
	        	anisotropyImage.exportData(0, length, anisotropyBuffer);
	        }
	        catch (IOException error) {
	        	System.out.println("IO exception");
	            return;
	        }
	        
	        //create component image
			componentImage = new ViewJComponentDTIImage(this,resultImage,null,imageBufferA,pixBuffer,1,resultImage.getExtents(),false,FileInfoBase.UNKNOWN_ORIENT,anisotropyBuffer);
			componentImage.addMouseWheelListener(this);
			componentImage.setBuffers(imageBufferA, null, pixBuffer, null);
			if (resols[1] >= resols[0]) {
	            componentImage.setResolutions(1, heightResFactor);
	        } else {
	            componentImage.setResolutions(widthResFactor, 1);
	        }
			if (getRGBTA() == null) {
                setRGBTA(initRGB(resultImage));
            }
			initExtentsVariables(resultImage);
			componentImage.setSlice(zSlice);
			componentImage.setZoom(zoom, zoom);
			numSlices = resultImage.getExtents()[2];
			
			
			//determine which color wheel is selected...then call show based on type
			if(colorWheelComboBox.getSelectedIndex() == 0) {
				type = "ABSVAL";
			}
			else if(colorWheelComboBox.getSelectedIndex() == 1) {
				type = "NOSYMM";
			}
			else if(colorWheelComboBox.getSelectedIndex() == 2) {
				type = "ROTATIONALSYMM";
			}
			else if(colorWheelComboBox.getSelectedIndex() == 3) {
				type = "MIRRORSYMM";
			}
			
			//call show
			componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			
			gbc = new GridBagConstraints();
			
			//result panel, resultImage panel, resultScroll panel
			resultPanel = new JPanel(gbl);
			resultImagePanel = new JPanel(gbl);
			resultScrollPanel = new JScrollPane(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);		
			titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
			resultImagePanel.setMinimumSize(new Dimension(400,400));
			resultScrollPanel.setBorder(titledBorder);
			resultScrollPanel.setPreferredSize(new Dimension(400, 400));
			resultScrollPanel.setMinimumSize(new Dimension(400,400));
			resultScrollPanel.addMouseWheelListener(this);
			gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.insets = new Insets(0,0,0,0);
			gbc.anchor = GridBagConstraints.CENTER;
			resultImagePanel.add(componentImage,gbc);
			resultScrollPanel.setViewportView(resultImagePanel);
			//resultImageSlider panel
			resultImageSliderPanel = new JPanel(gbl);
			titledBorder = new TitledBorder(new EtchedBorder(), " Image slice ", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
			resultImageSliderPanel.setBorder(titledBorder);
			resultImageSlider = new JSlider(JSlider.HORIZONTAL, 1, nImage, zSlice + 1);
			resultImageSlider.setMajorTickSpacing(10);
			resultImageSlider.setPaintTicks(true);
			resultImageSlider.addChangeListener(this);
			maxResultImageSlicesLabel = new JLabel(Integer.toString(nImage));
			maxResultImageSlicesLabel.setForeground(Color.black);
			maxResultImageSlicesLabel.setFont(MipavUtil.font12);
			minResultImageSlicesLabel = new JLabel("1");
			minResultImageSlicesLabel.setForeground(Color.black);
			minResultImageSlicesLabel.setFont(MipavUtil.font12);
			gbc.gridx = 0;
			gbc.gridy = 0;
			gbc.gridwidth = 2;
			gbc.weightx = 1;
			gbc.gridheight = 1;
			gbc.fill = GridBagConstraints.HORIZONTAL;
			resultImageSliderPanel.add(resultImageSlider,gbc);
			gbc.gridx = 0;
			gbc.gridy = 1;
			gbc.gridwidth = 1;
			gbc.weightx = 0;
			gbc.anchor = GridBagConstraints.WEST;
			gbc.fill = GridBagConstraints.NONE;
			resultImageSliderPanel.add(minResultImageSlicesLabel,gbc);
			gbc.gridx = 2;
			gbc.anchor = GridBagConstraints.EAST;
			gbc.weightx = 0;
			resultImageSliderPanel.add(maxResultImageSlicesLabel, gbc);
			//toolbar panel
			toolbarPanel = new JPanel(new GridBagLayout());
			titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
			toolbarPanel.setBorder(titledBorder);
			toolbarBuilder = new ViewToolBarBuilder(this);
			magButton = toolbarBuilder.buildButton("MagImage", "Magnify Image", "zoomin");
			magButton.addMouseListener(this);
			unMagButton = toolbarBuilder.buildButton("UnMagImage", "Un-Mag Image", "zoomout");
			unMagButton.addMouseListener(this);
			zoomOneButton = toolbarBuilder.buildButton("ZoomOne", "Magnify Image 1.0x", "zoom1");
			zoomOneButton.addMouseListener(this);
			captureImageButton = toolbarBuilder.buildButton("CaptureImage", "Capture image slices to new frame", "camera");
			captureImageButton.addMouseListener(this);
			magLabel = new JLabel("M:"+zoom);
			setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice+1) + "/" + numSlices + "    M:"+zoom);
			
			gbc.anchor = GridBagConstraints.CENTER;
			gbc.insets = new Insets(0,5,0,5);
			gbc.gridx = 0;
	        gbc.gridy = 0;
			toolbarPanel.add(magButton,gbc);
			gbc.gridx = 1;
	        gbc.gridy = 0;
			toolbarPanel.add(unMagButton,gbc);
			gbc.gridx = 2;
	        gbc.gridy = 0;
			toolbarPanel.add(zoomOneButton,gbc);
			gbc.gridx = 3;
	        gbc.gridy = 0;
			toolbarPanel.add(captureImageButton,gbc);
			gbc.gridx = 0;
	        gbc.gridy = 1;
	        gbc.gridwidth = 4;
	        gbc.insets = new Insets(10,0,0,0);
			toolbarPanel.add(magLabel,gbc);

			gbc.gridwidth = 1;
			gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.insets = new Insets(0,0,0,0);
			gbc.anchor = GridBagConstraints.CENTER;
			resultPanel.add(resultScrollPanel,gbc);
			gbc.gridx = 0;
	        gbc.gridy = 1;
	        gbc.insets = new Insets(0,0,0,0);
			gbc.anchor = GridBagConstraints.CENTER;
			gbc.fill = GridBagConstraints.HORIZONTAL;
			resultPanel.add(resultImageSliderPanel,gbc);
			gbc.gridx = 0;
	        gbc.gridy = 2;
	        gbc.insets = new Insets(0,0,0,0);
			gbc.anchor = GridBagConstraints.WEST;
			resultPanel.add(toolbarPanel,gbc);
			
			gbc.anchor = GridBagConstraints.NORTH;
			gbc.gridx = 2;
		    gbc.gridy = 0;
		    bottomPanel.add(resultPanel, gbc);
			bottomPanel.validate();
			bottomPanel.repaint();
			bottomPanel.remove(tempPanel);

			alg = null;
			
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		
		finalize();

	}

	
	/**
	 *  action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("eigenvectorBrowse")) {
			JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
			chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
	        chooser.setDialogTitle("Choose eigenvector file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
	        	FileIO fileIO = new FileIO();
	        	eigvecSrcImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator);
	        	if(eigvecSrcImage.getNDims() != 4) {
	        		MipavUtil.displayError("Eigenvector file does not have correct dimensions");
	        		if(eigvecSrcImage != null) {
	        			eigvecSrcImage.disposeLocal();
	        		}
	        		eigvecSrcImage = null;
					return;
	        	}
	        	if(eigvecSrcImage.getExtents()[3] != 9) {
					MipavUtil.displayError("Eigenvector file does not have correct dimensions");
					if(eigvecSrcImage != null) {
						eigvecSrcImage.disposeLocal();
					}
					eigvecSrcImage = null;
					return;
	        	}
	        	eigenvectorPath.setText(chooser.getSelectedFile().getAbsolutePath());
	        	Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
	        	int[] dimExtentsLUT;
	            dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            LUTa.resetTransferLine(0.0f, (int) Math.round(eigvecSrcImage.getMin()), 255.0f, (int) Math.round(eigvecSrcImage.getMax()));
	            int[] extents;
	            extents = new int[4];
	            extents[0] = Math.round(eigvecSrcImage.getExtents()[0]);
	            extents[1] = Math.round(eigvecSrcImage.getExtents()[1]);
	            extents[2] = Math.round(eigvecSrcImage.getExtents()[2]);
	            extents[3] = Math.round(eigvecSrcImage.getExtents()[3]);   
	        }
		}
		else if(command.equalsIgnoreCase("anisotropyBrowse")) {
			JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
			chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
	        chooser.setDialogTitle("Choose anisotropy file");
	        int returnValue = chooser.showOpenDialog(this);
	        if (returnValue == JFileChooser.APPROVE_OPTION) {
	        	FileIO fileIO = new FileIO();
	        	anisotropyImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator);
	        	if(anisotropyImage.getNDims() > 3) {
					MipavUtil.displayError("anisotropy file does not have correct dimensions");
					if(anisotropyImage != null) {
						anisotropyImage.disposeLocal();
					}
					anisotropyImage = null;
					return;
	        	}
	        	anisotropyPath.setText(chooser.getSelectedFile().getAbsolutePath());
	        	Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
	        }
		}
		else if(command.equalsIgnoreCase("ok")) {
			if(eigvecSrcImage == null || anisotropyImage == null) {
				MipavUtil.displayError("Both eigenvector and anisotropy files are needed");
				return;
			}
			//this is if user hits ok for a new set of eigenvector and anisotropy files
			if(bottomPanel.getComponent(2) == resultPanel) {
				tempPanel = new JPanel(gbl);
				tempPanel.setPreferredSize(new Dimension(400, 400));
				tempPanel.setMinimumSize(new Dimension(400,400));
				tempPanel.setForeground(Color.black);
				titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
				tempPanel.setBorder(titledBorder);
				bottomPanel.add(tempPanel, 2);
				bottomPanel.remove(resultPanel);
			}
			callAlgorithm();
		}
		else if (command.equals("MagImage")) {
			if(!unMagButton.isEnabled()) {
				unMagButton.setEnabled(true);
			}
            float newZoom;
            if ((Preferences.is(Preferences.PREF_ZOOM_LINEAR))) {
            	if(componentImage.getZoomX() < 1.0f) {
            		newZoom = 2.0f * componentImage.getZoomX();
            	}else {
            		newZoom = componentImage.getZoomX() + 1.0f;
            	}
            }
            else {
            	newZoom = 2.0f * componentImage.getZoomX();
            }
            zoom = newZoom;
            if(zoom != 1) {
            	captureImageButton.setEnabled(false);
            }
            else {
            	captureImageButton.setEnabled(true);
            }
            componentImage.setZoom(newZoom, newZoom);
            validate();
            componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
            
            magLabel.setText("M:"+zoom);
			setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice+1) + "/" + numSlices + "    M:"+zoom);
            if(componentImage.getZoomX() >= 32) {
				magButton.setEnabled(false);
			}
		}
		else if (command.equals("UnMagImage")) {
			if(!magButton.isEnabled()) {
				magButton.setEnabled(true);
			}
            float newZoom;
            if ((Preferences.is(Preferences.PREF_ZOOM_LINEAR)) && (componentImage.getZoomX() > 1.0f)) {
                // linear zoom is prevented if getZoomX() <= 1.0
                newZoom = componentImage.getZoomX() - 1.0f;
            } else {
                newZoom = 0.5f * componentImage.getZoomX();
            }
            zoom = newZoom;
            if(zoom != 1) {
            	captureImageButton.setEnabled(false);
            }
            else {
            	captureImageButton.setEnabled(true);
            }
            componentImage.setZoom(newZoom, newZoom);
            validate();
            componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
            magLabel.setText("M:"+zoom);
			setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice+1) + "/" + numSlices + "    M:"+zoom);
            if(componentImage.getZoomX() <= 0.125) {
				unMagButton.setEnabled(false);
			}
        }
		else if (command.equals("ZoomOne")) {
			if(!unMagButton.isEnabled()) {
				unMagButton.setEnabled(true);
			}
			if(!magButton.isEnabled()) {
				magButton.setEnabled(true);
			}
			float newZoom = 1;
			zoom = newZoom;
			captureImageButton.setEnabled(true);
            componentImage.setZoom(newZoom, newZoom);
			validate();
            componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
            magLabel.setText("M:"+zoom);
			setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice+1) + "/" + numSlices + "    M:"+zoom);

		}
		else if(command.equals("CaptureImage")) {
			writeImage();
		}
		else if(command.equals("truncRadio")) {
			isMultiply = false;
			if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
		}
		else if(command.equals("multRadio")) {
			isMultiply = true;
			if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
		}
		
	}





	/**
	 * state changed
	 */
	public void stateChanged(ChangeEvent e) {
		Object source = e.getSource();

        if (source == anisotropyMaxSlider) {
        	anisotropyMaxTextField.setText(String.valueOf(anisotropyMaxSlider.getValue() / 1000.000000f));
        	anisotropyMax = Float.valueOf(anisotropyMaxTextField.getText());
        	if(componentImage != null) {
        		if(anisotropyMax > anisotropyMin) {
        			componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
        		}
			}
        }
        else if (source == anisotropyMinSlider) {
        	anisotropyMinTextField.setText(String.valueOf(anisotropyMinSlider.getValue() / 1000.000000f));
        	anisotropyMin = Float.valueOf(anisotropyMinTextField.getText());
        	if(componentImage != null) {
        		if(anisotropyMin < anisotropyMax) {
        			componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
        		}
			}
        }
        else if (source == adjustExpSlider) {
        	adjustExpTextField.setText(String.valueOf(adjustExpSlider.getValue() / 100.000000f));
        	adjustExp = Float.valueOf(adjustExpTextField.getText());
        	if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
        }
        else if (source == gammaSlider) {
        	gammaTextField.setText(String.valueOf(gammaSlider.getValue() / 100.000000f));
        	gamma = Float.valueOf(gammaTextField.getText());
        	if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
        }
        else if (source == stevensBetaSlider) {
        	stevensBetaTextField.setText(String.valueOf(stevensBetaSlider.getValue() / 100.000000f));
        	stevensBeta = Float.valueOf(stevensBetaTextField.getText());
        	if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
        }
        else if (source == satBlueSlider) {
        	satBlueTextField.setText(String.valueOf(satBlueSlider.getValue() / 1000.000000f));
        	pB = Float.valueOf(satBlueTextField.getText());
        	if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
        }
        else if (source == dimGreenSlider) {
        	dimGreenTextField.setText(String.valueOf(dimGreenSlider.getValue() / 100.000000f));
        	pG = Float.valueOf(dimGreenTextField.getText());
        	if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
        }
        else if (source == colorRangeSlider) {
        	colorRangeTextField.setText(String.valueOf(colorRangeSlider.getValue() / 1000.000000f));
        	pC = Float.valueOf(colorRangeTextField.getText());
        	if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
        }
        else if (source == satVsThetaSlider) {
        	satVsThetaTextField.setText(String.valueOf(satVsThetaSlider.getValue() / 1000.000000f));
        	pS = Float.valueOf(satVsThetaTextField.getText());
        	if(componentImage != null) {
				componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
			}
        }
        else if(source == resultImageSlider) {
        	zSlice = resultImageSlider.getValue() - 1;
        	componentImage.setSlice(zSlice);
			componentImage.show(tSlice, zSlice, true);
			setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice+1) + "/" + numSlices + "    M:"+zoom);
        }
	}



	/**
	 * item state changed
	 */
	public void itemStateChanged(ItemEvent event) {
		Object source = event.getSource();
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,0,0);
		gbc.anchor = GridBagConstraints.CENTER;
		if (source == colorWheelComboBox) {
			int index = colorWheelComboBox.getSelectedIndex();
			if(index == 0) {
				if(currentColorWheel != absValColorWheel) {
					absValColorWheel = new ColorWheel("ABSVAL",150,pB,pC,pS,pG,stevensBeta,gamma);
					colorWheelPanel.add(absValColorWheel,gbc);
					colorWheelPanel.validate();
					colorWheelPanel.repaint();
					colorWheelPanel.remove(currentColorWheel);
					currentColorWheel = absValColorWheel;
					if(componentImage != null) {
						type = "ABSVAL";
						componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
					}
				}
				satVsThetaSlider.setEnabled(false);
				satVsThetaTextField.setBackground(Color.lightGray);
			}
			else if(index == 1) {
				if(currentColorWheel != noSymmColorWheel) {
					noSymmColorWheel = new ColorWheel("NOSYMM",150,pB,pC,pS,pG,stevensBeta,gamma);
					colorWheelPanel.add(noSymmColorWheel,gbc);
					colorWheelPanel.validate();
					colorWheelPanel.repaint();
					colorWheelPanel.remove(currentColorWheel);
					currentColorWheel = noSymmColorWheel;
					if(componentImage != null) {
						type = "NOSYMM";
						componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
					}
				}
				satVsThetaSlider.setEnabled(true);
				satVsThetaTextField.setBackground(Color.white);
			}
			else if(index == 2) {
				if(currentColorWheel != rotationalSymmColorWheel) {
					rotationalSymmColorWheel = new ColorWheel("ROTATIONALSYMM",150,pB,pC,pS,pG,stevensBeta,gamma);
					colorWheelPanel.add(rotationalSymmColorWheel,gbc);
					colorWheelPanel.validate();
					colorWheelPanel.repaint();
					colorWheelPanel.remove(currentColorWheel);
					currentColorWheel = rotationalSymmColorWheel;
					if(componentImage != null) {
						type = "ROTATIONALSYMM";
						componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
					}
				}
				satVsThetaSlider.setEnabled(true);
				satVsThetaTextField.setBackground(Color.white);
			}
			else if(index == 3) {
				if(currentColorWheel != mirrorSymmColorWheel) {
					mirrorSymmColorWheel = new ColorWheel("MIRRORSYMM",150,pB,pC,pS,pG,stevensBeta,gamma);
					colorWheelPanel.add(mirrorSymmColorWheel,gbc);
					colorWheelPanel.validate();
					colorWheelPanel.repaint();
					colorWheelPanel.remove(currentColorWheel);
					currentColorWheel = mirrorSymmColorWheel;
					if(componentImage != null) {
						type = "MIRRORSYMM";
						componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax, stevensBeta, adjustExp, isMultiply);
					}
				}
				satVsThetaSlider.setEnabled(true);
				satVsThetaTextField.setBackground(Color.white);
			}
		}
		
	}



	
	/**
	 * update current color wheel
	 *
	 */
	public void updateCurrentColorWheel() {
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,0,0);
		gbc.anchor = GridBagConstraints.CENTER;
		if(currentColorWheel == absValColorWheel) {
			absValColorWheel = new ColorWheel("ABSVAL",150,pB,pC,pS,pG,stevensBeta,gamma);
			colorWheelPanel.add(absValColorWheel,gbc);
			colorWheelPanel.validate();
			colorWheelPanel.repaint();
			colorWheelPanel.remove(currentColorWheel);
			currentColorWheel = absValColorWheel;
		}
		else if(currentColorWheel == noSymmColorWheel) {
			noSymmColorWheel = new ColorWheel("NOSYMM",150,pB,pC,pS,pG,stevensBeta,gamma);
			colorWheelPanel.add(noSymmColorWheel,gbc);
			colorWheelPanel.validate();
			colorWheelPanel.repaint();
			colorWheelPanel.remove(currentColorWheel);
			currentColorWheel = noSymmColorWheel;
		}
		else if(currentColorWheel == rotationalSymmColorWheel) {
			rotationalSymmColorWheel = new ColorWheel("ROTATIONALSYMM",150,pB,pC,pS,pG,stevensBeta,gamma);
			colorWheelPanel.add(rotationalSymmColorWheel,gbc);
			colorWheelPanel.validate();
			colorWheelPanel.repaint();
			colorWheelPanel.remove(currentColorWheel);
			currentColorWheel = rotationalSymmColorWheel;
		}
		else if(currentColorWheel == mirrorSymmColorWheel) {
			mirrorSymmColorWheel = new ColorWheel("MIRRORSYMM",150,pB,pC,pS,pG,stevensBeta,gamma);
			colorWheelPanel.add(mirrorSymmColorWheel,gbc);
			colorWheelPanel.validate();
			colorWheelPanel.repaint();
			colorWheelPanel.remove(currentColorWheel);
			currentColorWheel = mirrorSymmColorWheel;
		}

	}
	
	
	
	
	/**
     * Create the intensity buffer for an image.
     *
     * @param   extents  the extents of the image
     * @param   isColor  whether the image is in color
     *
     * @return  a buffer which is big enough to contain the image intensity data
     */
    protected static float[] initImageBuffer(int[] extents, boolean isColor) {
        int bufferFactor = 1;

        if (isColor) {
            bufferFactor = 4;
        }

        return new float[bufferFactor * extents[0] * extents[1]];
    }
    
    
    
    
    /**
     * Create the pixel buffer for an image.
     *
     * @param   extents  the extents of the image
     *
     * @return  a buffer which is big enough to contain the image pixel data
     */
    protected static int[] initPixelBuffer(int[] extents) {
        return new int[extents[0] * extents[1]];
    }
    
    
    
    
    /**
     * init resolutions
     * @param img
     * @return
     */
    protected static float[] initResolutions(ModelImage img) {
        float[] res = img.getFileInfo(0).getResolutions();

        for (int r = 0; r < img.getNDims(); r++) {

            if (res[r] < 0) {
                res[r] = Math.abs(res[r]);
            } else if (res[r] == 0) {
                res[r] = 1.0f;
            }
        }

        return res;
    }
    
    
    /**
     * init units
     * @param img
     * @return
     */
    protected static int[] initUnits(ModelImage img) {
        return img.getFileInfo(0).getUnitsOfMeasure();
    }
    
    
    
    /**
     * Get the resolution correction needed for non-isotropic images.
     *
     * @param   imgResols  the image resolution
     * @param   imgUnits   the image units of measure
     *
     * @return  the resolution correction factor in the x (the first element) and y (the second element) dimensions
     */
    protected static float[] initResFactor(float[] imgResols, int[] imgUnits) {
        float[] resFactor = new float[2];

        resFactor[0] = 1.0f;
        resFactor[1] = 1.0f;

        if ((imgResols[1] >= imgResols[0]) && (imgResols[1] < (20.0f * imgResols[0])) && (imgUnits[0] == imgUnits[1])) {
            resFactor[1] = imgResols[1] / imgResols[0];
        } else if ((imgResols[0] > imgResols[1]) && (imgResols[0] < (20.0f * imgResols[1])) &&
                       (imgUnits[0] == imgUnits[1])) {
            resFactor[0] = imgResols[0] / imgResols[1];
        }

        return resFactor;
    }
    
    
    
    
    /**
     * Creates and initializes the ModelRGB for an image.
     *
     * @param   img  the image to create a ModelRGB for
     *
     * @return  a ModelRGB for the image <code>img</code> (null if NOT a color image)
     *
     * @throws  OutOfMemoryError  if enough memory cannot be allocated for this method
     */
    public static ModelRGB initRGB(ModelImage img) throws OutOfMemoryError {
        ModelRGB newRGB = null;

        if (img.isColorImage()) {
            float[] x = new float[4];
            float[] y = new float[4];
            Dimension dim = new Dimension(256, 256);

            // Set ModelRGB min max values;
            x[0] = 0;
            y[0] = dim.height - 1;

            x[1] = 255 * 0.333f;
            y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);

            x[2] = 255 * 0.667f;
            y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);

            x[3] = 255;
            y[3] = 0;

            int[] RGBExtents = new int[2];
            RGBExtents[0] = 4;
            RGBExtents[1] = 256;
            newRGB = new ModelRGB(RGBExtents);
            newRGB.getRedFunction().importArrays(x, y, 4);
            newRGB.getGreenFunction().importArrays(x, y, 4);
            newRGB.getBlueFunction().importArrays(x, y, 4);
            newRGB.makeRGB(-1);
        }

        return newRGB;
    }
    
    
   
    
    
    
    /**
     * Initializes the variables based on the image extents. (i.e. number of slices, number of time slices, the initial
     * z-slice, etc.
     *
     * @param  img  the image to set the extent variables for
     */
    public void initExtentsVariables(ModelImage img) {
        int[] slices = null;
        int[] numImages = null;

        slices = initSlicePositions(img);
        numImages = initNumSlices(img);

        zSlice = slices[0];
        tSlice = slices[1];

        nImage = numImages[0];
        nTImage = numImages[1];
    }
    
    /**
     * Get the initial time and volume slice positions.
     *
     * @param   img  the image to get the slice positions of
     *
     * @return  an array containing the slice in the volume (in the first element) and the time slice (in the second
     *          element)
     */
    protected static int[] initSlicePositions(ModelImage img) {
        int[] slices = new int[2];

        if (img.getNDims() == 4) {
            slices[0] = (img.getExtents()[2] - 1) / 2;
            slices[1] = 0;
        } else if (img.getNDims() == 3) {
            slices[0] = (img.getExtents()[2] - 1) / 2;
            slices[1] = 0;
        } else {
            slices[0] = 0;
            slices[1] = 0;
        }

        return slices;
    }
    
    /**
     * Get the total number of time slices and volume slices.
     *
     * @param   img  the image to get the slices of
     *
     * @return  an array containing the number of volume slices (in the first element) and the number of time slices in
     *          the image (in the second element)
     */
    protected static int[] initNumSlices(ModelImage img) {
        int[] numImages = new int[2];

        if (img.getNDims() == 4) {
            numImages[0] = img.getExtents()[2];
            numImages[1] = img.getExtents()[3];
        } else if (img.getNDims() == 3) {
            numImages[0] = img.getExtents()[2];
            numImages[1] = 0;
        } else {
            numImages[0] = 1;
            numImages[1] = 0;
        }

        return numImages;
    }
    

    
    
    /**
	 * 
	 */
	public void finalize() {
		if(eigvecSrcImage != null) {
			eigvecSrcImage.disposeLocal();
		}
		if(anisotropyImage != null) {
			anisotropyImage.disposeLocal();
		}
		
		eigvecSrcImage = null;
		anisotropyImage = null;
	}
	
	
	
	

	/**
	 * mouse clicked
	 */
	public void mouseClicked(MouseEvent event) {
		if (event.getButton() == MouseEvent.BUTTON3) {
            if (event.getSource() instanceof JButton) {
                JButton btnSource = (JButton) event.getSource();

                if (btnSource.getActionCommand().equals("MagImage") ||
                        btnSource.getActionCommand().equals("UnMagImage")) {
                    handleZoomPopupMenu(btnSource, event);
                }
            }
            
		}
		
	}



	/**
	 * mouse entered
	 */
	public void mouseEntered(MouseEvent arg0) {
		
	}



	/**
	 * mouse exited
	 */
	public void mouseExited(MouseEvent arg0) {

	}



	/**
	 * mouse pressed
	 */
	public void mousePressed(MouseEvent arg0) {
		
	}



	/**
	 * mouse released
	 */
	public void mouseReleased(MouseEvent e) {
		Object source = e.getSource();

        if (source == anisotropyMaxSlider) {
        	if(anisotropyMaxSlider.getValue() < anisotropyMinSlider.getValue()) {
        		anisotropyMaxSlider.setValue(anisotropyMinSlider.getValue() + 1);
        	}
        	updateCurrentColorWheel();
        }
        else if (source == anisotropyMinSlider) {
        	if(anisotropyMinSlider.getValue() > anisotropyMaxSlider.getValue()) {
        		anisotropyMinSlider.setValue(anisotropyMaxSlider.getValue() - 1);
        	}
        	updateCurrentColorWheel();
        }
        else if (source == gammaSlider) {
        	updateCurrentColorWheel();
        }
        //else if (source == stevensBetaSlider) {
        //	updateCurrentColorWheel();
        //}
        else if (source == satBlueSlider) {
        	updateCurrentColorWheel();
        }
        else if (source == dimGreenSlider) {
        	updateCurrentColorWheel();
        }
        else if (source == colorRangeSlider) {
        	updateCurrentColorWheel();
        }
        else if (source == satVsThetaSlider) {
        	updateCurrentColorWheel();
        }
		
	}
	
	
	
	
	
	 /**
     * Gets the RGB LUT table for ARGB image A.
     *
     * @return  RGBT the new RGB LUT to be applied to the image
     */
    public ModelRGB getRGBTA() {
        return (componentImage.getRGBTA());
    }
    
    
    /**
     * Sets the RGB LUT table for ARGB image A.
     *
     * @param  RGBT  the new RGB LUT to be applied to the image
     */
    public void setRGBTA(ModelRGB RGBT) {

        if (componentImage != null) {
            componentImage.setRGBTA(RGBT);
        }
    }
    
	/**
	 * get controls
	 */
	public ViewControlsImage getControls() {
		// TODO Auto-generated method stub
		return null;
	}




	/**
	 * get image a
	 */
	public ModelImage getImageA() {
		// TODO Auto-generated method stub
		return null;
	}




	/**
	 * get image b
	 */
	public ModelImage getImageB() {
		// TODO Auto-generated method stub
		return null;
	}




	/**
	 * remove controls
	 */
	public void removeControls() {
		// TODO Auto-generated method stub
		
	}




	/**
	 * set active image
	 */
	public void setActiveImage(int active) {
		// TODO Auto-generated method stub
		
	}




	/**
	 * set alpha blend
	 */
	public void setAlphaBlend(int value) {
		// TODO Auto-generated method stub
		
	}




	/**
	 * set controls
	 */
	public void setControls() {
		// TODO Auto-generated method stub
		
	}




	/**
	 * set enabled
	 */
	public void setEnabled(boolean flag) {
		// TODO Auto-generated method stub
		
	}




	/**
	 * set image b
	 */
	public void setImageB(ModelImage imageB) {
		// TODO Auto-generated method stub
		
	}




	/**
	 * set paint bitmap switch
	 */
	public void setPaintBitmapSwitch(boolean flag) {
		// TODO Auto-generated method stub
		
	}


	/**
	 * set rgbtb
	 */
	public void setRGBTB(ModelRGB RGBT) {
		// TODO Auto-generated method stub
		
	}




	/**
	 * set title
	 */
	public void setTitle() {
		// TODO Auto-generated method stub
		
	}




	/**
	 * update image extents
	 */
	public boolean updateImageExtents() {
		// TODO Auto-generated method stub
		return false;
	}



	/**
	 * set slice
	 */
	public void setSlice(int slice) {
		// TODO Auto-generated method stub
		
	}



	/**
	 * set time slice
	 */
	public void setTimeSlice(int tSlice) {
		// TODO Auto-generated method stub
		
	}



	/**
	 * update images
	 */
	public boolean updateImages() {
		return false;
	}



	/**
	 * update images
	 */
	public boolean updateImages(boolean flag) {
		// TODO Auto-generated method stub
		return false;
	}


	
	/**
	 * update images
	 */
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
		// TODO Auto-generated method stub
		return false;
	}
	
	
	
	/**
     * Scrolls through all z slices of a 3d/4d image and captures them into a new ARGB ModelImage, then puts the
     * ModelImage in a ViewJFrameImage.
     *
     * @return  
     */
    private boolean writeImage() {
        int[] pixels;
        int bufferSize, xDim, yDim;
        short[] buffer = null;
        int[] extents = new int[3];
        ModelImage screenCaptureImage = null;
        Robot robot = null;
        Image imagePix = null;

        /**
         * Create a Robot to capture the screen at the given location and dimension (the region of interest is the
         * ViewJFrameImage)
         */
        try {
            robot = new Robot();

            Point p = new Point();
            p.x = 0;
            p.y = 0;
            SwingUtilities.convertPointToScreen(p, componentImage);

            Dimension d = new Dimension();
            d.width = componentImage.getWidth();
            d.height = componentImage.getHeight();
            currentRectangle = new Rectangle(p, d);


            extents[0] = currentRectangle.width; // RGB
            extents[1] = currentRectangle.height;
            extents[2] = numSlices;
            pixels = new int[extents[0] * extents[1]];
            bufferSize = 4 * extents[0] * extents[1];
            screenCaptureImage = new ModelImage(ModelStorageBase.ARGB, extents, resultImage.getImageName() + "_" + type);
            screenCaptureImage.getFileInfo()[0].setFileDirectory(resultImage.getFileInfo(0).getFileDirectory());
            buffer = new short[bufferSize];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("JDialogScreenCapture: unable to allocate enough memory for RGB image");

            return false;
        } catch (AWTException aex) {
            MipavUtil.displayError("Platform doesn't support screen capture.");

            return false;
        }


        // Turn off image slice #'s
        componentImage.setShowSliceNumber(false);
        componentImage.useHighlight(false);

        /**
         * Scroll through each slice, grabbing the screen with the robot and exporting pixels into a buffer for ARGB
         */
        for (int slice = 0; slice < numSlices; slice++) {
            resultImageSlider.setValue(slice+1);

            try {
                imagePix = robot.createScreenCapture(currentRectangle);

                PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, extents[0], extents[1], pixels, 0, extents[0]);
                pgTest.grabPixels();
            } catch (InterruptedException e) {
                Preferences.debug("Interrupted waiting for pixels!");

                return false;
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("ViewFrameImage: unable to allocate enough memory for RGB image");

                return false;
            }

            int i, k;

            for (i = 0, k = 0; i < (extents[0] * extents[1]); i++, k += 4) {
                buffer[k] = (short) (255); // alpha
                buffer[k + 1] = (short) ((pixels[i] >> 16) & 0xFF); // Red
                buffer[k + 2] = (short) ((pixels[i] >> 8) & 0xFF); // Green
                buffer[k + 3] = (short) (pixels[i] & 0xFF); // Blue
            }

            /**
             * Import the ARGB buffer into the model image
             */
            try {
            	screenCaptureImage.importData((buffer.length * slice), buffer, false);
            } catch (IOException error) {
                MipavUtil.displayError("JDialogScreenCapture: Problems grabbing image!");
            }

            screenCaptureImage.getFileInfo()[0].setPhotometric((short) 2); // Indicates RGB tiff file format


        }

        componentImage.setShowSliceNumber(true);

        FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[resultImage.getExtents()[2]];
        String psetDesc = "Color Display Parameters";
        String name;
        String value;
        for (int i=0;i<fileInfoBases.length;i++) {
       	 	fileInfoBases[i] = new FileInfoImageXML(screenCaptureImage.getImageName(), null, FileUtility.XML);
       	 	fileInfoBases[i].createPSet(psetDesc);
       	 	name = "Eigenvector File";
       	 	value = eigvecFilename;
       	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
       	 	name = "Anisotropy File";
    	 	value = anisotropyFilename;
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Color Wheel";
    	 	value = type;
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Anisotropy Max";
    	 	value = String.valueOf(anisotropyMax);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Anisotropy Min";
    	 	value = String.valueOf(anisotropyMin);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Gamma Correction";
    	 	value = String.valueOf(gamma);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Blue Saturation";
    	 	value = String.valueOf(pB);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Green Adjustment";
    	 	value = String.valueOf(pG);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Color Range";
    	 	value = String.valueOf(pC);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Saturation vs Theta";
    	 	value = String.valueOf(pS);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Adjust Exponent";
    	 	value = String.valueOf(adjustExp);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Stevens Beta";
    	 	value = String.valueOf(stevensBeta);
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
    	 	name = "Truncate / Multiply";
    	 	if(isMultiply) {
    	 		value = "Multiply";
    	 	}
    	 	else {
    	 		value = "Truncate";
    	 	}
    	 	fileInfoBases[i].getPSet(psetDesc).addParameter(name);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
    	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
       	 	fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("string");
       	 	
       	 	fileInfoBases[i].setFileDirectory(resultImage.getFileInfo()[0].getFileDirectory());
        }
        FileInfoBase.copyCoreInfo(resultImage.getFileInfo(), fileInfoBases);
        
        screenCaptureImage.setFileInfo(fileInfoBases);
        screenCaptureImage.calcMinMax();
        
        
        new ViewJFrameImage(screenCaptureImage, null, new Dimension(610, 200));
        
        resultImageSlider.setValue(numSlices/2);

        return true;
    }
	
	
    /** 
	 * window closing
	 */
	public void windowClosing(WindowEvent event) {
		if(resultImage != null) {
			resultImage.disposeLocal();
		}
		super.windowClosing(event);
		if(alg != null) {
			alg.setThreadStopped(true);
		}

		dispose();
	}

	/**
	 * mouse wheel moved
	 * @param arg0
	 */
	public void mouseWheelMoved(MouseWheelEvent event) {
		int wheelRotation = event.getWheelRotation();
		Object source = event.getSource();
		if(source == componentImage || source == resultScrollPanel) {	
			if (wheelRotation < 0) {
				if(zSlice != numSlices-1) {
					zSlice = zSlice + 1;
					resultImageSlider.setValue(zSlice + 1);
					setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice+1) + "/" + numSlices + "    M:"+zoom);
				}
			}else {
				if(zSlice != 0) {
					zSlice = zSlice -1;
					resultImageSlider.setValue(zSlice + 1);
					setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice+1) + "/" + numSlices + "    M:"+zoom);
				}
			}
		}
		else if(source == anisotropyMaxSlider) {
			if (wheelRotation < 0) {
				if(anisotropyMaxSlider.getValue() < anisotropyMaxSlider.getMaximum()) {
					anisotropyMaxSlider.setValue(anisotropyMaxSlider.getValue() + 1);
					updateCurrentColorWheel();
				}
			}
			else {
				if(anisotropyMaxSlider.getValue() > anisotropyMaxSlider.getMinimum()) {
					if(anisotropyMaxSlider.getValue() - 1 > anisotropyMinSlider.getValue()) {
						anisotropyMaxSlider.setValue(anisotropyMaxSlider.getValue() - 1);
						updateCurrentColorWheel();
					}
				}
			}
		}
		else if(source == anisotropyMinSlider) {
			if (wheelRotation < 0) {
				if(anisotropyMinSlider.getValue() < anisotropyMinSlider.getMaximum()) {
					if(anisotropyMinSlider.getValue() + 1 < anisotropyMaxSlider.getValue()) {
						anisotropyMinSlider.setValue(anisotropyMinSlider.getValue() + 1);
						updateCurrentColorWheel();
					}
				}
			}
			else {
				if(anisotropyMinSlider.getValue() > anisotropyMinSlider.getMinimum()) {
					anisotropyMinSlider.setValue(anisotropyMinSlider.getValue() - 1);
					updateCurrentColorWheel();
				}
			}
		}
		else if(source == gammaSlider) {
			if (wheelRotation < 0) {
				if(gammaSlider.getValue() < gammaSlider.getMaximum()) {
					gammaSlider.setValue(gammaSlider.getValue() + 1);
					updateCurrentColorWheel();
				}
			}
			else {
				if(gammaSlider.getValue() > gammaSlider.getMinimum()) {
					gammaSlider.setValue(gammaSlider.getValue() - 1);
					updateCurrentColorWheel();
				}
			}
		}
		else if(source == satBlueSlider) {
			if (wheelRotation < 0) {
				if(satBlueSlider.getValue() < satBlueSlider.getMaximum()) {
					satBlueSlider.setValue(satBlueSlider.getValue() + 1);
					updateCurrentColorWheel();
				}
			}
			else {
				if(satBlueSlider.getValue() > satBlueSlider.getMinimum()) {
					satBlueSlider.setValue(satBlueSlider.getValue() - 1);
					updateCurrentColorWheel();
				}
			}
		}
		else if(source == dimGreenSlider) {
			if (wheelRotation < 0) {
				if(dimGreenSlider.getValue() < dimGreenSlider.getMaximum()) {
					dimGreenSlider.setValue(dimGreenSlider.getValue() + 1);
					updateCurrentColorWheel();
				}
			}
			else {
				if(dimGreenSlider.getValue() > dimGreenSlider.getMinimum()) {
					dimGreenSlider.setValue(dimGreenSlider.getValue() - 1);
					updateCurrentColorWheel();
				}
			}
		}
		else if(source == colorRangeSlider) {
			if (wheelRotation < 0) {
				if(colorRangeSlider.getValue() < colorRangeSlider.getMaximum()) {
					colorRangeSlider.setValue(colorRangeSlider.getValue() + 1);
					updateCurrentColorWheel();
				}
			}
			else {
				if(colorRangeSlider.getValue() > colorRangeSlider.getMinimum()) {
					colorRangeSlider.setValue(colorRangeSlider.getValue() - 1);
					updateCurrentColorWheel();
				}
			}
		}
		else if(source == satVsThetaSlider) {
			if (wheelRotation < 0) {
				if(satVsThetaSlider.getValue() < satVsThetaSlider.getMaximum()) {
					satVsThetaSlider.setValue(satVsThetaSlider.getValue() + 1);
					updateCurrentColorWheel();
				}
			}
			else {
				if(satVsThetaSlider.getValue() > satVsThetaSlider.getMinimum()) {
					satVsThetaSlider.setValue(satVsThetaSlider.getValue() - 1);
					updateCurrentColorWheel();
				}
			}
		}
		else if(source == adjustExpSlider) {
			if (wheelRotation < 0) {
				if(adjustExpSlider.getValue() < adjustExpSlider.getMaximum()) {
					adjustExpSlider.setValue(adjustExpSlider.getValue() + 1);
					updateCurrentColorWheel();
				}
			}
			else {
				if(adjustExpSlider.getValue() > adjustExpSlider.getMinimum()) {
					adjustExpSlider.setValue(adjustExpSlider.getValue() - 1);
					updateCurrentColorWheel();
				}
			}
		}
		else if(source == stevensBetaSlider) {
			if (wheelRotation < 0) {
				if(stevensBetaSlider.getValue() < stevensBetaSlider.getMaximum()) {
					stevensBetaSlider.setValue(stevensBetaSlider.getValue() + 1);
					updateCurrentColorWheel();
				}
			}
			else {
				if(stevensBetaSlider.getValue() > stevensBetaSlider.getMinimum()) {
					stevensBetaSlider.setValue(stevensBetaSlider.getValue() - 1);
					updateCurrentColorWheel();
				}
			}
		}

	}
	
	
	

}
