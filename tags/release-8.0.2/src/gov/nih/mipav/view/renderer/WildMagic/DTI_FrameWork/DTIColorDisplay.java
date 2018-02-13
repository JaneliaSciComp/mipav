package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.PixelGrabber;
import java.io.*;
import java.lang.reflect.Method;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * @author pandyan
 * 
 * This is the main dialog for the DTI Color Display
 * 
 * References: Developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group, Lin-Ching Chang D.Sc., Carlo
 * Pierpaoli MD Ph.D., and Lindsay Walker MS from the the NIH/NICHD/LIMB/STBB group and Olga Vogt from the
 * NIH/CIT/DCB/ISL/BIRSS group:
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL) Biomedical Imaging Research Services Section (BIRSS) Imaging
 * Sciences Laboratory (ISL) Division of Cumputational Bioscience (DCB) Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB) Laboratory of Integrative and Medical Biophysics (LIMB) National
 * Institute of Child Health & Humann Development National Institutes of Health
 * 
 * 
 * Publication Reference:
 * 
 * S. Pajevic and C. Pierpaoli, "Color Schemes to Represent the Orientation of Anisotropic Tissues from Diffusion Tensor
 * Data: Application to White Matter Fiber Tract Mapping in the Human Brain," Magnetic Resonance in Medicine, vol. 42,
 * no. 3, pp. 526-540, 1999
 * 
 */
public class DTIColorDisplay extends ViewJFrameBase implements AlgorithmInterface, ChangeListener,
        ItemListener, MouseListener, MouseWheelListener, KeyListener, FocusListener {

    /** dialog title and version * */
    private String title = " DTI Color Display  v1.5      ";

    /** panels * */
    private JPanel mainPanel, topPanel, filesPanel, okPanel, bottomPanel, refPanel, colorPanel, colorWheelPanel,
            resultImagePanel, optionsPanel, colorWheelChoicesPanel, heuristicParametersPanel, anisotropyMaxPanel,
            anisotropyMinPanel, gammaPanel, stevensBetaPanel, satBluePanel, dimGreenPanel, colorRangePanel,
            satVsThetaPanel, resultPanel, resultImageSliderPanel, tempPanel, toolbarPanel, adjustExpPanel,
            truncMultPanel, restoreDefaultsPanel, saveLoadPanel;

    /** labels * */
    private JLabel eigenvectorLabel, anisotropyLabel, minAnisotropyMaxLabel, maxAnisotropyMaxLabel,
            minAnisotropyMinLabel, maxAnisotropyMinLabel, minGammaLabel, maxGammaLabel, minStevensBetaLabel,
            maxStevensBetaLabel, minSatBlueLabel, maxSatBlueLabel, minDimGreenLabel, maxDimGreenLabel,
            minColorRangeLabel, maxColorRangeLabel, minSatVsThetaLabel, maxSatVsThetaLabel, minResultImageSlicesLabel,
            maxResultImageSlicesLabel, currentResultImageSlicesLabel, magLabel, refLabel1, refLabel2, refLabel3,
            minAdjustExpLabel, maxAdjustExpLabel;

    /** mins and maxes for heuristic parameters* */
    private float minAnisotropyMax, maxAnisotropyMax, minAnisotropyMin, maxAnisotropyMin, minGamma, maxGamma,
            minStevensBeta, maxStevensBeta, minSatBlue, maxSatBlue, minDimGreen, maxDimGreen, minColorRange,
            maxColorRange, minSatVsTheta, maxSatVsTheta, minAdjustExp, maxAdjustExp;

    /** scroll pane for result image * */
    private JScrollPane resultScrollPanel;

    /** textfields * */
    private JTextField eigenvectorPath, anisotropyPath, anisotropyMaxTextField, anisotropyMinTextField, gammaTextField,
            stevensBetaTextField, satBlueTextField, dimGreenTextField, colorRangeTextField, satVsThetaTextField,
            adjustExpTextField;

    /** buttons * */
    private JButton eigenvectorBrowseButton, anisotropyBrowseButton, okButton, magButton, unMagButton, zoomOneButton,
            captureImageButton, restoreDefaultsButton, saveButton, loadButton;

    /** eigenvector src image * */
    private ModelImage eigvecSrcImage;

    /** anisotropy src image * */
    private ModelImage anisotropyImage;

    /** names of eigenvector and anisotropy files* */
    private String eigvecFilename, anisotropyFilename;

    /** result image * */
    private ModelImage resultImage;

    /** LUT of input image * */
    private ModelLUT LUTa;

    /** Color Wheels * */
    private ColorWheel colorWheel;

    /** handle to the algorithm * */
    private AlgorithmDTIColorDisplay alg;

    /** GridBagLayout * */
    private GridBagLayout gbl;

    /** GridbagConstraints * */
    private GridBagConstraints gbc;

    /** various sliders in dialog * */
    public JSlider anisotropyMaxSlider, anisotropyMinSlider, gammaSlider, stevensBetaSlider, satBlueSlider,
            dimGreenSlider, colorRangeSlider, satVsThetaSlider, resultImageSlider, adjustExpSlider;

    /** titled border for certain components * */
    private TitledBorder titledBorder;

    /** color wheel choices combo box * */
    private JComboBox colorWheelComboBox;

    /** anisotropy max * */
    private float anisotropyMax = 0.7f;

    /** anisotropy min * */
    private float anisotropyMin = 0.0f;

    /** adjust exp * */
    private float adjustExp = 0.5f;

    /** gamma correction * */
    private float gamma = 1.8f;

    /** steven's beta * */
    private float stevensBeta = 0.4f;

    /** blue saturation * */
    private float pB = 0.35f;

    /** green saturation * */
    private float pG = 0.8f;

    /** color range * */
    private float pC = 0.7f;

    /** saturation vs theta * */
    private float pS = 0.5f;

    /** ViewJComponentDTIImage * */
    private ViewJComponentDTIImage componentImage;

    /** Buffer used to store image intensities the presently viewed slice of image A. */
    protected float[] imageBufferA;

    /** Storage of the image voxel resolutions. One resolution value per dimension. */
    protected float[] resols;

    /**
     * Integer buffer (4 bytes that stores the concatenated Alpha (1 byte), Red (1 byte), Green ( 1 byte ), Blue (1 byte )
     * data. The ARGB values are generated by using the imageA intensities as a index into a LUT.
     */
    protected int[] pixBuffer;

    /** Storage of the resolution units of measure. For example, mm, cm, inches ... */
    protected int[] units;

    /** current z slice * */
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

    /** type of color wheel ABSVAL, NOSYMM, ROTATIONALSYMM, MIRRORSYMM * */
    private String type;

    /** ViewToolBarBuilder * */
    private ViewToolBarBuilder toolbarBuilder;

    /** current zoom for result image * */
    private float zoom = 1.0f;

    /** num slices for result image * */
    private int numSlices;

    /** rectangle for screen capture* */
    private Rectangle currentRectangle;

    /** radio group for truncate/multiply * */
    private ButtonGroup truncMultRadioGroup;

    /** radio buttons for truncate/multiply * */
    private JRadioButton truncRadio, multRadio;

    /** boolean for truncate/multiply * */
    private boolean isMultiply = true;

    /** flag needed when performing restore defaults * */
    private boolean flag = false;

    /** Buffered Reader for loading params * */
    private BufferedReader in;

    /** Buffered Writer for saving params * */
    private BufferedWriter out;

    /** String indicating current color wheel type * */
    private String currentColorWheelType;

    /**
     * Constructor
     */
    public DTIColorDisplay(boolean modal) {
        super(null, null);
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

        // topPanel
        topPanel = new JPanel(gbl);
        filesPanel = new JPanel(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 5, 5);
        eigenvectorLabel = new JLabel("EVector Image: ");
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
        anisotropyLabel = new JLabel("FA Image: ");
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
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        topPanel.add(filesPanel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        topPanel.add(okPanel, gbc);

        // color panel
        colorPanel = new JPanel(gbl);
        colorWheelPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        colorWheelPanel.setBorder(titledBorder);
        colorWheelPanel.setPreferredSize(new Dimension(500, 500));
        colorWheelPanel.setMinimumSize(new Dimension(500, 500));
        colorWheel = new ColorWheel("ABSVAL", 200);
        currentColorWheelType = "ABSVAL";
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.anchor = GridBagConstraints.CENTER;
        colorWheelPanel.add(colorWheel, gbc);
        colorWheelChoicesPanel = new JPanel(gbl);
        colorWheelChoicesPanel.setForeground(Color.black);
        titledBorder = new TitledBorder(new EtchedBorder(), " Color Wheel ", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
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
        gbc.insets = new Insets(0, 0, 10, 0);
        colorWheelChoicesPanel.add(colorWheelComboBox, gbc);
        // reference panel
        refPanel = new JPanel(gbl);
        refLabel1 = new JLabel();
        Font f = new Font("Helvetica", Font.PLAIN, 11);
        refLabel1.setFont(f);
        refLabel1
                .setText("<html>Software developed in concert with Dr. Sinisa Pajevic Ph.D. from the NIH/CIT/DCB/MSCL group and <br>Dr. Lin-Ching Chang D.Sc.,  Dr. Carlo Pierpaoli MD Ph.D.,  and Lindsay Walker M.S. <br>from the NIH/NICHD/LIMB/STBB group</html>");
        refLabel2 = new JLabel();
        refLabel2.setFont(f);
        refLabel2
                .setText("<html><br>Color Schemes produced according to: S. Pajevic and C. Pierpaoli, Color Schemes to Represent the <br>Orientation of Anisotropic Tissues from Diffusion Tensor Data: Application to White Matter Fiber <br>Tract Mapping in the Human Brain, Magnetic Resonance in Medicine, vol. 42, no. 3, pp. 526-540, 1999</html>");
        refLabel3 = new JLabel();
        refLabel3.setFont(f);
        refLabel3.setText("<html><a href=' '>Download Paper</a></html>");
        refLabel3.addMouseListener(this);

        gbc.gridx = 0;
        gbc.gridy = 0;
        refPanel.add(refLabel1, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        refPanel.add(refLabel2, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        refPanel.add(refLabel3, gbc);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 0, 0);
        colorPanel.add(colorWheelPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        colorPanel.add(colorWheelChoicesPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.fill = GridBagConstraints.NONE;
        colorPanel.add(refPanel, gbc);

        // options panel
        gbc.anchor = GridBagConstraints.NORTHWEST;
        optionsPanel = new JPanel(gbl);
        // heuristic parameters panel
        heuristicParametersPanel = new JPanel(gbl);
        heuristicParametersPanel.setForeground(Color.black);
        titledBorder = new TitledBorder(new EtchedBorder(), " Heuristic Parameters ", TitledBorder.LEFT,
                TitledBorder.CENTER, MipavUtil.font12B, Color.black);
        heuristicParametersPanel.setBorder(titledBorder);
        // anisotropy max
        anisotropyMaxPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " Anisotropy Max ", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        anisotropyMaxPanel.setBorder(titledBorder);
        anisotropyMaxSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 700);
        anisotropyMaxSlider.setMajorTickSpacing(100);
        anisotropyMaxSlider.setPaintTicks(true);
        anisotropyMaxSlider.addChangeListener(this);
        anisotropyMaxSlider.addMouseListener(this);
        maxAnisotropyMaxLabel = new JLabel("1.0");
        maxAnisotropyMaxLabel.setForeground(Color.black);
        maxAnisotropyMaxLabel.setFont(MipavUtil.font12);
        try {
            maxAnisotropyMax = Float.parseFloat(maxAnisotropyMaxLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minAnisotropyMaxLabel = new JLabel("0.0");
        minAnisotropyMaxLabel.setForeground(Color.black);
        minAnisotropyMaxLabel.setFont(MipavUtil.font12);
        try {
            minAnisotropyMax = Float.parseFloat(minAnisotropyMaxLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        anisotropyMaxPanel.add(anisotropyMaxSlider, gbc);
        anisotropyMaxTextField = new JTextField(8);
        anisotropyMaxTextField.addKeyListener(this);
        anisotropyMaxTextField.addFocusListener(this);
        anisotropyMaxTextField.setBackground(Color.white);
        anisotropyMaxTextField.setText(String.valueOf((float) (anisotropyMaxSlider.getValue() / 1000.000000f)));
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        anisotropyMaxPanel.add(anisotropyMaxTextField, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        anisotropyMaxPanel.add(minAnisotropyMaxLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        anisotropyMaxPanel.add(maxAnisotropyMaxLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(anisotropyMaxPanel, gbc);
        // anisotropy min
        anisotropyMinPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " Anisotropy Min ", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        anisotropyMinPanel.setBorder(titledBorder);
        anisotropyMinSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 0);
        anisotropyMinSlider.setMajorTickSpacing(100);
        anisotropyMinSlider.setPaintTicks(true);
        anisotropyMinSlider.addChangeListener(this);
        anisotropyMinSlider.addMouseListener(this);
        maxAnisotropyMinLabel = new JLabel("1.0");
        maxAnisotropyMinLabel.setForeground(Color.black);
        maxAnisotropyMinLabel.setFont(MipavUtil.font12);
        try {
            maxAnisotropyMin = Float.parseFloat(maxAnisotropyMinLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minAnisotropyMinLabel = new JLabel("0.0");
        minAnisotropyMinLabel.setForeground(Color.black);
        minAnisotropyMinLabel.setFont(MipavUtil.font12);
        try {
            minAnisotropyMin = Float.parseFloat(minAnisotropyMinLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        ;
        anisotropyMinPanel.add(anisotropyMinSlider, gbc);
        anisotropyMinTextField = new JTextField(8);
        anisotropyMinTextField.addKeyListener(this);
        anisotropyMinTextField.addFocusListener(this);
        anisotropyMinTextField.setBackground(Color.white);
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        anisotropyMinPanel.add(anisotropyMinTextField, gbc);
        anisotropyMinTextField.setText(String.valueOf((float) (anisotropyMinSlider.getValue() / 1000.000000f)));
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        anisotropyMinPanel.add(minAnisotropyMinLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        anisotropyMinPanel.add(maxAnisotropyMinLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(anisotropyMinPanel, gbc);
        // gamma
        gammaPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " Gamma Correction ", TitledBorder.LEFT,
                TitledBorder.CENTER, MipavUtil.font12B, Color.black);
        gammaPanel.setBorder(titledBorder);
        gammaSlider = new JSlider(JSlider.HORIZONTAL, 100, 400, 180);
        gammaSlider.setMajorTickSpacing(50);
        gammaSlider.setPaintTicks(true);
        gammaSlider.addChangeListener(this);
        gammaSlider.addMouseListener(this);
        maxGammaLabel = new JLabel("4");
        maxGammaLabel.setForeground(Color.black);
        maxGammaLabel.setFont(MipavUtil.font12);
        try {
            maxGamma = Float.parseFloat(maxGammaLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minGammaLabel = new JLabel("1");
        minGammaLabel.setForeground(Color.black);
        minGammaLabel.setFont(MipavUtil.font12);
        try {
            minGamma = Float.parseFloat(minGammaLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        ;
        gammaPanel.add(gammaSlider, gbc);
        gammaTextField = new JTextField(8);
        gammaTextField.addKeyListener(this);
        gammaTextField.addFocusListener(this);
        gammaTextField.setBackground(Color.white);
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gammaPanel.add(gammaTextField, gbc);
        gammaTextField.setText(String.valueOf((float) (gammaSlider.getValue() / 100.000000f)));
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        gammaPanel.add(minGammaLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        gammaPanel.add(maxGammaLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(gammaPanel, gbc);
        // blue sat
        satBluePanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " pB Sat. Blue ", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        satBluePanel.setBorder(titledBorder);
        satBlueSlider = new JSlider(JSlider.HORIZONTAL, 0, 500, 350);
        satBlueSlider.setMajorTickSpacing(50);
        satBlueSlider.setPaintTicks(true);
        satBlueSlider.addChangeListener(this);
        satBlueSlider.addMouseListener(this);
        maxSatBlueLabel = new JLabel("0.5");
        maxSatBlueLabel.setForeground(Color.black);
        maxSatBlueLabel.setFont(MipavUtil.font12);
        try {
            maxSatBlue = Float.parseFloat(maxSatBlueLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minSatBlueLabel = new JLabel("0.0");
        minSatBlueLabel.setForeground(Color.black);
        minSatBlueLabel.setFont(MipavUtil.font12);
        try {
            minSatBlue = Float.parseFloat(minSatBlueLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        ;
        satBluePanel.add(satBlueSlider, gbc);
        satBlueTextField = new JTextField(8);
        satBlueTextField.addKeyListener(this);
        satBlueTextField.addFocusListener(this);
        satBlueTextField.setBackground(Color.white);
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        satBluePanel.add(satBlueTextField, gbc);
        satBlueTextField.setText(String.valueOf((float) (satBlueSlider.getValue() / 1000.000000f)));
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        satBluePanel.add(minSatBlueLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        satBluePanel.add(maxSatBlueLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(satBluePanel, gbc);
        // green dim
        dimGreenPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " pG Dim Green ", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        dimGreenPanel.setBorder(titledBorder);
        dimGreenSlider = new JSlider(JSlider.HORIZONTAL, 0, 300, 80);
        dimGreenSlider.setMajorTickSpacing(50);
        dimGreenSlider.setPaintTicks(true);
        dimGreenSlider.addChangeListener(this);
        dimGreenSlider.addMouseListener(this);
        maxDimGreenLabel = new JLabel("3.0");
        maxDimGreenLabel.setForeground(Color.black);
        maxDimGreenLabel.setFont(MipavUtil.font12);
        try {
            maxDimGreen = Float.parseFloat(maxDimGreenLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minDimGreenLabel = new JLabel("0.0");
        minDimGreenLabel.setForeground(Color.black);
        minDimGreenLabel.setFont(MipavUtil.font12);
        try {
            minDimGreen = Float.parseFloat(minDimGreenLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        ;
        dimGreenPanel.add(dimGreenSlider, gbc);
        dimGreenTextField = new JTextField(8);
        dimGreenTextField.addKeyListener(this);
        dimGreenTextField.addFocusListener(this);
        dimGreenTextField.setBackground(Color.white);
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        dimGreenPanel.add(dimGreenTextField, gbc);
        dimGreenTextField.setText(String.valueOf((float) (dimGreenSlider.getValue() / 100.000000f)));
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        dimGreenPanel.add(minDimGreenLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        dimGreenPanel.add(maxDimGreenLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(dimGreenPanel, gbc);
        // color range
        colorRangePanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " pC Color Range ", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        colorRangePanel.setBorder(titledBorder);
        colorRangeSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 700);
        colorRangeSlider.setMajorTickSpacing(100);
        colorRangeSlider.setPaintTicks(true);
        colorRangeSlider.addChangeListener(this);
        colorRangeSlider.addMouseListener(this);
        maxColorRangeLabel = new JLabel("1.0");
        maxColorRangeLabel.setForeground(Color.black);
        maxColorRangeLabel.setFont(MipavUtil.font12);
        try {
            maxColorRange = Float.parseFloat(maxColorRangeLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minColorRangeLabel = new JLabel("0.0");
        minColorRangeLabel.setForeground(Color.black);
        minColorRangeLabel.setFont(MipavUtil.font12);
        try {
            minColorRange = Float.parseFloat(minColorRangeLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        colorRangePanel.add(colorRangeSlider, gbc);
        colorRangeTextField = new JTextField(8);
        colorRangeTextField.addKeyListener(this);
        colorRangeTextField.addFocusListener(this);
        colorRangeTextField.setBackground(Color.white);
        colorRangeTextField.setText(String.valueOf((float) (colorRangeSlider.getValue() / 1000.000000f)));
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        colorRangePanel.add(colorRangeTextField, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        colorRangePanel.add(minColorRangeLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        colorRangePanel.add(maxColorRangeLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(colorRangePanel, gbc);
        // sat vs theta
        satVsThetaPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " pS Sat vs Theta ", TitledBorder.LEFT,
                TitledBorder.CENTER, MipavUtil.font12B, Color.black);
        satVsThetaPanel.setBorder(titledBorder);
        satVsThetaSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 500);
        satVsThetaSlider.setMajorTickSpacing(100);
        satVsThetaSlider.setPaintTicks(true);
        satVsThetaSlider.addChangeListener(this);
        satVsThetaSlider.addMouseListener(this);
        satVsThetaSlider.setEnabled(false);
        maxSatVsThetaLabel = new JLabel("1.0");
        maxSatVsThetaLabel.setForeground(Color.black);
        maxSatVsThetaLabel.setFont(MipavUtil.font12);
        try {
            maxSatVsTheta = Float.parseFloat(maxSatVsThetaLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minSatVsThetaLabel = new JLabel("0.0");
        minSatVsThetaLabel.setForeground(Color.black);
        minSatVsThetaLabel.setFont(MipavUtil.font12);
        try {
            minSatVsTheta = Float.parseFloat(minSatVsThetaLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        satVsThetaPanel.add(satVsThetaSlider, gbc);
        satVsThetaTextField = new JTextField(8);
        satVsThetaTextField.addKeyListener(this);
        satVsThetaTextField.addFocusListener(this);
        satVsThetaTextField.setDisabledTextColor(Color.lightGray);
        satVsThetaTextField.setEnabled(false);
        satVsThetaTextField.setEditable(false);
        satVsThetaTextField.setBackground(Color.lightGray);
        satVsThetaTextField.setText(String.valueOf((float) (satVsThetaSlider.getValue() / 1000.000000f)));
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        satVsThetaPanel.add(satVsThetaTextField, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        satVsThetaPanel.add(minSatVsThetaLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        satVsThetaPanel.add(maxSatVsThetaLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(satVsThetaPanel, gbc);
        // adjustExp
        adjustExpPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " Adjust Exp ", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        adjustExpPanel.setBorder(titledBorder);
        adjustExpSlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 50);
        adjustExpSlider.setMajorTickSpacing(10);
        adjustExpSlider.setPaintTicks(true);
        adjustExpSlider.addChangeListener(this);
        adjustExpSlider.addMouseListener(this);
        maxAdjustExpLabel = new JLabel("1.0");
        maxAdjustExpLabel.setForeground(Color.black);
        maxAdjustExpLabel.setFont(MipavUtil.font12);
        try {
            maxAdjustExp = Float.parseFloat(maxAdjustExpLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minAdjustExpLabel = new JLabel("0.0");
        minAdjustExpLabel.setForeground(Color.black);
        minAdjustExpLabel.setFont(MipavUtil.font12);
        try {
            minAdjustExp = Float.parseFloat(minAdjustExpLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        ;
        adjustExpPanel.add(adjustExpSlider, gbc);
        adjustExpTextField = new JTextField(8);
        adjustExpTextField.addKeyListener(this);
        adjustExpTextField.addFocusListener(this);
        adjustExpTextField.setBackground(Color.white);
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        adjustExpPanel.add(adjustExpTextField, gbc);
        adjustExpTextField.setText(String.valueOf((float) (adjustExpSlider.getValue() / 100.000000f)));
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        adjustExpPanel.add(minAdjustExpLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        adjustExpPanel.add(maxAdjustExpLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(adjustExpPanel, gbc);
        // steven's beta
        stevensBetaPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " Stevens Beta ", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        stevensBetaPanel.setBorder(titledBorder);
        stevensBetaSlider = new JSlider(JSlider.HORIZONTAL, 30, 60, 40);
        stevensBetaSlider.setMajorTickSpacing(3);
        stevensBetaSlider.setPaintTicks(true);
        stevensBetaSlider.addChangeListener(this);
        stevensBetaSlider.addMouseListener(this);
        maxStevensBetaLabel = new JLabel("0.6");
        maxStevensBetaLabel.setForeground(Color.black);
        maxStevensBetaLabel.setFont(MipavUtil.font12);
        try {
            maxStevensBeta = Float.parseFloat(maxStevensBetaLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        minStevensBetaLabel = new JLabel("0.3");
        minStevensBetaLabel.setForeground(Color.black);
        minStevensBetaLabel.setFont(MipavUtil.font12);
        try {
            minStevensBeta = Float.parseFloat(minStevensBetaLabel.getText());
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return;
        }
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        ;
        stevensBetaPanel.add(stevensBetaSlider, gbc);
        stevensBetaTextField = new JTextField(8);
        stevensBetaTextField.addKeyListener(this);
        stevensBetaTextField.addFocusListener(this);
        stevensBetaTextField.setBackground(Color.white);
        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        stevensBetaPanel.add(stevensBetaTextField, gbc);
        stevensBetaTextField.setText(String.valueOf((float) (stevensBetaSlider.getValue() / 100.000000f)));
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        stevensBetaPanel.add(minStevensBetaLabel, gbc);
        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        stevensBetaPanel.add(maxStevensBetaLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(stevensBetaPanel, gbc);
        // truncate-multiply
        truncMultPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " Truncate / Multiply ", TitledBorder.LEFT,
                TitledBorder.CENTER, MipavUtil.font12B, Color.black);
        truncMultPanel.setBorder(titledBorder);
        truncMultRadioGroup = new ButtonGroup();
        truncRadio = new JRadioButton("Truncate");
        truncRadio.setSelected(false);
        truncRadio.addActionListener(this);
        truncRadio.setActionCommand("truncRadio");
        truncMultRadioGroup.add(truncRadio);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 30, 0, 30);
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
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 9;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(truncMultPanel, gbc);
        // save load params
        saveLoadPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " Save Params / Load Params ", TitledBorder.LEFT,
                TitledBorder.CENTER, MipavUtil.font12B, Color.black);
        saveLoadPanel.setBorder(titledBorder);
        saveButton = new JButton("Save");
        saveButton.addActionListener(this);
        saveButton.setActionCommand("save");
        saveButton.setToolTipText("Save heuristic parameter values to file");
        loadButton = new JButton("Load");
        loadButton.addActionListener(this);
        loadButton.setActionCommand("load");
        loadButton.setToolTipText("Load heuristic parameter values from file");
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 30, 5, 30);
        gbc.gridx = 0;
        gbc.gridy = 0;
        saveLoadPanel.add(saveButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        saveLoadPanel.add(loadButton, gbc);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(saveLoadPanel, gbc);
        // restore defaults
        restoreDefaultsPanel = new JPanel(gbl);
        titledBorder = new TitledBorder(new EtchedBorder(), " Restore Default Params ", TitledBorder.LEFT,
                TitledBorder.CENTER, MipavUtil.font12B, Color.black);
        restoreDefaultsPanel.setBorder(titledBorder);
        restoreDefaultsButton = new JButton("Restore");
        restoreDefaultsButton.addActionListener(this);
        restoreDefaultsButton.setActionCommand("restoreDefaults");
        restoreDefaultsButton.setToolTipText("Restore default heuristic parameter values");
        gbc.insets = new Insets(0, 0, 5, 0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        restoreDefaultsPanel.add(restoreDefaultsButton, gbc);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        heuristicParametersPanel.add(restoreDefaultsPanel, gbc);
        // add heuristic panel to options panel
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 1;
        optionsPanel.add(heuristicParametersPanel, gbc);

        // temp panel
        tempPanel = new JPanel(gbl);
        tempPanel.setPreferredSize(new Dimension(500, 500));
        tempPanel.setMinimumSize(new Dimension(500, 500));
        tempPanel.setForeground(Color.black);
        titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER,
                MipavUtil.font12B, Color.black);
        tempPanel.setBorder(titledBorder);

        // bottom panel
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

        // main panel
        mainPanel = new JPanel(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(topPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(bottomPanel, gbc);
        // gbc.gridx = 0;
        // gbc.gridy = 2;
        // gbc.insets = new Insets(15,5,10,0);
        // gbc.anchor = GridBagConstraints.CENTER;
        // mainPanel.add(refPanel,gbc);
        getContentPane().add(mainPanel);

        pack();
        setResizable(false);
        setVisible(true);

    }

    /**
     * action performed
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        if (command.equalsIgnoreCase("eigenvectorBrowse")) {
            loadEigenVectorFile();
        } else if (command.equalsIgnoreCase("anisotropyBrowse")) {
            loadAnisotropyFile();
        } else if (command.equalsIgnoreCase("ok")) {
            if (eigvecSrcImage == null || anisotropyImage == null) {
                MipavUtil.displayError("Both eigenvector and anisotropy files are needed");
                return;
            }
            // this is if user hits ok for a new set of eigenvector and anisotropy files
            if (bottomPanel.getComponent(2) == resultPanel) {
                zoom = 1.0f;
                tempPanel = new JPanel(gbl);
                tempPanel.setPreferredSize(new Dimension(500, 500));
                tempPanel.setMinimumSize(new Dimension(500, 500));
                tempPanel.setForeground(Color.black);
                titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER,
                        MipavUtil.font12B, Color.black);
                tempPanel.setBorder(titledBorder);
                bottomPanel.add(tempPanel, 2);
                bottomPanel.remove(resultPanel);
            }
            if (resultImage != null) {
                resultImage.disposeLocal();
                resultImage = null;
            }
            callAlgorithm();
        } else if (command.equals("MagImage")) {
            magImage();
        } else if (command.equals("UnMagImage")) {
            unMagImage();
        } else if (command.equals("ZoomOne")) {
            zoomOne();

        } else if (command.equals("CaptureImage")) {
            captureImage();
        } else if (command.equals("truncRadio")) {
            isMultiply = false;
            if (componentImage != null) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
        } else if (command.equals("multRadio")) {
            isMultiply = true;
            if (componentImage != null) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
        } else if (command.equals("restoreDefaults")) {
            restoreDefaults();
        } else if (command.equals("save")) {
            saveParams();
            if (out != null) {
                try {
                    out.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        } else if (command.equals("load")) {
            loadParams();
            if (in != null) {
                try {
                    in.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }
    }

    /**
     * algorithm performed
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (alg.isCompleted()) {

            // grab the eigenvector and anisotropy file names and clear the textfields
            eigvecFilename = eigvecSrcImage.getImageName();
            anisotropyFilename = anisotropyImage.getImageName();
            eigenvectorPath.setText("");
            anisotropyPath.setText("");

            // get result Model Image
            resultImage = alg.getResultImage();
            imageBufferA = initImageBuffer(resultImage.getExtents(), true);
            pixBuffer = initPixelBuffer(resultImage.getExtents());
            resols = initResolutions(resultImage);
            units = initUnits(resultImage);
            float[] factor = initResFactor(resols, units);
            widthResFactor = factor[0];
            heightResFactor = factor[1];
            zSlice = (resultImage.getExtents()[2] - 1) / 2;
            // zSlice = zSlice - 1;

            // get data from anisotropy file to send into ViewJComponentEditImage constructor
            float[] anisotropyBuffer;
            int length = anisotropyImage.getExtents()[0] * anisotropyImage.getExtents()[1]
                    * anisotropyImage.getExtents()[2];
            anisotropyBuffer = new float[length];
            try {
                anisotropyImage.exportData(0, length, anisotropyBuffer);
            } catch (IOException error) {
                System.out.println("IO exception");
                return;
            }

            // create component image
            componentImage = new ViewJComponentDTIImage(this, resultImage, null, imageBufferA, pixBuffer, 1,
                    resultImage.getExtents(), false, FileInfoBase.UNKNOWN_ORIENT, anisotropyBuffer);
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

            // determine which color wheel is selected...then call show based on type
            if (colorWheelComboBox.getSelectedIndex() == 0) {
                type = "ABSVAL";
            } else if (colorWheelComboBox.getSelectedIndex() == 1) {
                type = "NOSYMM";
            } else if (colorWheelComboBox.getSelectedIndex() == 2) {
                type = "ROTATIONALSYMM";
            } else if (colorWheelComboBox.getSelectedIndex() == 3) {
                type = "MIRRORSYMM";
            }

            // call show
            componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                    stevensBeta, adjustExp, isMultiply);

            gbc = new GridBagConstraints();

            // result panel, resultImage panel, resultScroll panel
            resultPanel = new JPanel(gbl);
            resultImagePanel = new JPanel(gbl);
            resultScrollPanel = new JScrollPane(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER,
                    MipavUtil.font12B, Color.black);
            resultImagePanel.setMinimumSize(new Dimension(500, 500));
            resultScrollPanel.setBorder(titledBorder);
            resultScrollPanel.setPreferredSize(new Dimension(500, 500));
            resultScrollPanel.setMinimumSize(new Dimension(500, 500));
            resultScrollPanel.addMouseWheelListener(this);
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.insets = new Insets(0, 0, 0, 0);
            gbc.anchor = GridBagConstraints.CENTER;
            resultImagePanel.add(componentImage, gbc);
            resultScrollPanel.setViewportView(resultImagePanel);
            // resultImageSlider panel
            resultImageSliderPanel = new JPanel(gbl);
            titledBorder = new TitledBorder(new EtchedBorder(), " Image slice ", TitledBorder.LEFT,
                    TitledBorder.CENTER, MipavUtil.font12B, Color.black);
            resultImageSliderPanel.setBorder(titledBorder);
            resultImageSliderPanel.addMouseWheelListener(this);
            resultImageSlider = new JSlider(JSlider.HORIZONTAL, 0, nImage - 1, zSlice);
            resultImageSlider.setMajorTickSpacing(10);
            resultImageSlider.setPaintTicks(true);
            resultImageSlider.addChangeListener(this);
            resultImageSlider.addMouseWheelListener(this);
            maxResultImageSlicesLabel = new JLabel(Integer.toString(nImage - 1));
            maxResultImageSlicesLabel.setForeground(Color.black);
            maxResultImageSlicesLabel.setFont(MipavUtil.font12);
            minResultImageSlicesLabel = new JLabel("0");
            minResultImageSlicesLabel.setForeground(Color.black);
            minResultImageSlicesLabel.setFont(MipavUtil.font12);
            currentResultImageSlicesLabel = new JLabel( (zSlice) + "/" + (numSlices - 1));
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridwidth = 2;
            gbc.weightx = 1;
            gbc.gridheight = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            resultImageSliderPanel.add(resultImageSlider, gbc);
            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.gridwidth = 1;
            gbc.weightx = 0;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.NONE;
            resultImageSliderPanel.add(minResultImageSlicesLabel, gbc);
            gbc.gridx = 2;
            gbc.anchor = GridBagConstraints.EAST;
            gbc.weightx = 0;
            resultImageSliderPanel.add(maxResultImageSlicesLabel, gbc);
            gbc.gridx = 1;
            gbc.gridy = 1;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = 0;
            gbc.insets = new Insets(10, 0, 10, 0);
            resultImageSliderPanel.add(currentResultImageSlicesLabel, gbc);

            // toolbar panel
            toolbarPanel = new JPanel(new GridBagLayout());
            titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER,
                    MipavUtil.font12B, Color.black);
            toolbarPanel.setBorder(titledBorder);
            toolbarBuilder = new ViewToolBarBuilder(this);
            magButton = toolbarBuilder.buildButton("MagImage", "Magnify Image", "zoomin");
            magButton.addMouseListener(this);
            unMagButton = toolbarBuilder.buildButton("UnMagImage", "Un-Mag Image", "zoomout");
            unMagButton.addMouseListener(this);
            zoomOneButton = toolbarBuilder.buildButton("ZoomOne", "Magnify Image 1.0x", "zoom1");
            zoomOneButton.addMouseListener(this);
            captureImageButton = toolbarBuilder.buildButton("CaptureImage", "Capture image slices to new frame",
                    "camera");
            captureImageButton.addMouseListener(this);
            magLabel = new JLabel("M:" + zoom);
            setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice) + "/" + (numSlices - 1)
                    + "    M:" + zoom);

            gbc.anchor = GridBagConstraints.CENTER;
            gbc.insets = new Insets(0, 5, 0, 5);
            gbc.gridx = 0;
            gbc.gridy = 0;
            toolbarPanel.add(magButton, gbc);
            gbc.gridx = 1;
            gbc.gridy = 0;
            toolbarPanel.add(unMagButton, gbc);
            gbc.gridx = 2;
            gbc.gridy = 0;
            toolbarPanel.add(zoomOneButton, gbc);
            gbc.gridx = 3;
            gbc.gridy = 0;
            toolbarPanel.add(captureImageButton, gbc);
            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.gridwidth = 4;
            gbc.insets = new Insets(10, 0, 10, 0);
            toolbarPanel.add(magLabel, gbc);

            gbc.gridwidth = 1;
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.insets = new Insets(0, 0, 0, 0);
            gbc.anchor = GridBagConstraints.CENTER;
            resultPanel.add(resultScrollPanel, gbc);
            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.insets = new Insets(0, 0, 0, 0);
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            resultPanel.add(resultImageSliderPanel, gbc);
            gbc.gridx = 0;
            gbc.gridy = 2;
            gbc.insets = new Insets(0, 0, 0, 0);
            gbc.anchor = GridBagConstraints.WEST;
            resultPanel.add(toolbarPanel, gbc);

            gbc.anchor = GridBagConstraints.NORTH;
            gbc.gridx = 2;
            gbc.gridy = 0;
            bottomPanel.add(resultPanel, gbc);
            bottomPanel.validate();
            bottomPanel.repaint();
            bottomPanel.remove(tempPanel);

            alg = null;

            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

            // this is test code for creating an rgb image based on eigenvector and anisotropy image
            // ModelImage testImage = createRGBImage();
            // new ViewJFrameImage(testImage);

        }

        finalize();

    }

    /**
     * test code for creating rgb image based on eigvev and anisot
     */
    public ModelImage createRGBImage() {
        // gamma factor
        float gamma = 1.8f;

        // create the dest extents of the dec image...the 4th dim will only have 3 as the value
        int[] destExtents = new int[4];
        destExtents[0] = eigvecSrcImage.getExtents()[0];
        destExtents[1] = eigvecSrcImage.getExtents()[1];
        destExtents[2] = eigvecSrcImage.getExtents()[2];
        destExtents[3] = 3;

        ModelImage decImage = new ModelImage(ModelStorageBase.FLOAT, destExtents, eigvecSrcImage.getImageName()
                + "_DEC");

        // buffer
        float[] buffer;

        // determine length of dec image
        int length = eigvecSrcImage.getExtents()[0] * eigvecSrcImage.getExtents()[1] * eigvecSrcImage.getExtents()[2]
                * 3;
        buffer = new float[length];

        // export eigvecSrcImage into buffer based on length
        try {
            eigvecSrcImage.exportData(0, length, buffer);
        } catch (IOException error) {
            System.out.println("IO exception");
            return null;
        }

        // lets first do absolute value for each value in the buffer
        for (int i = 0; i < buffer.length; i++) {
            buffer[i] = Math.abs(buffer[i]);
        }

        // import resultBuffer into decImage
        try {
            decImage.importData(0, buffer, true);
        } catch (IOException error) {
            System.out.println("IO exception");

            return null;
        }

        // extract dec image into channel images
        destExtents = new int[3];
        destExtents[0] = decImage.getExtents()[0];
        destExtents[1] = decImage.getExtents()[1];
        destExtents[2] = decImage.getExtents()[2];
        ModelImage[] channelImages = new ModelImage[decImage.getExtents()[3]];
        for (int i = 0; i < decImage.getExtents()[3]; i++) {
            int num = i + 1;
            String resultString = decImage.getImageName() + "_Vol=" + num;
            channelImages[i] = new ModelImage(decImage.getType(), destExtents, resultString);
            AlgorithmSubset subsetAlgo = new AlgorithmSubset(decImage, channelImages[i], AlgorithmSubset.REMOVE_T, i);
            subsetAlgo.setRunningInSeparateThread(false);
            subsetAlgo.run();
        }

        decImage.disposeLocal();
        decImage = null;

        // set up result image
        resultImage = new ModelImage(ModelImage.ARGB_FLOAT, channelImages[0].getExtents(), eigvecSrcImage
                .getImageName()
                + "_ColorDisplay");

        // cocatenate channel images into an RGB image
        AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(channelImages[0], channelImages[1], channelImages[2],
                resultImage, false, true, 255.0f, false);
        mathAlgo.setRunningInSeparateThread(false);
        mathAlgo.run();

        channelImages[0].disposeLocal();
        channelImages[0] = null;
        channelImages[1].disposeLocal();
        channelImages[1] = null;
        channelImages[2].disposeLocal();
        channelImages[2] = null;

        // copy core file info over
        FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[resultImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(resultImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(eigvecSrcImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(eigvecSrcImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(eigvecSrcImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(resultImage.getExtents());
            fileInfoBases[i].setImageOrientation(eigvecSrcImage.getFileInfo()[0].getImageOrientation());
            fileInfoBases[i].setAxisOrientation(eigvecSrcImage.getFileInfo()[0].getAxisOrientation());
            fileInfoBases[i].setOrigin(eigvecSrcImage.getFileInfo()[0].getOrigin());
            fileInfoBases[i].setPixelPadValue(eigvecSrcImage.getFileInfo()[0].getPixelPadValue());
            fileInfoBases[i].setPhotometric(eigvecSrcImage.getFileInfo()[0].getPhotometric());
            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);
            fileInfoBases[i].setFileDirectory(eigvecSrcImage.getFileInfo()[0].getFileDirectory());
        }

        resultImage.setFileInfo(fileInfoBases);

        // now we need to weight the result image by anisotopy

        float[] rgbBuffer;
        // determine length of dec image
        int rgbBuffLength = resultImage.getExtents()[0] * resultImage.getExtents()[1] * resultImage.getExtents()[2] * 4;
        rgbBuffer = new float[rgbBuffLength];

        // export eigvecSrcImage into buffer based on length
        try {
            resultImage.exportData(0, rgbBuffLength, rgbBuffer);
        } catch (IOException error) {
            System.out.println("IO exception");
            return null;
        }

        float[] anisotropyBuffer;
        int anisLength = anisotropyImage.getExtents()[0] * anisotropyImage.getExtents()[1]
                * anisotropyImage.getExtents()[2];
        anisotropyBuffer = new float[anisLength];
        try {
            anisotropyImage.exportData(0, anisLength, anisotropyBuffer);
        } catch (IOException error) {
            System.out.println("IO exception");
            return null;
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
            resultImage.importData(0, rgbBuffer, true);
        } catch (IOException error) {
            System.out.println("IO exception");

            return null;
        }

        resultImage.calcMinMax();
        return resultImage;
    }

    /**
     * call algorithm
     */
    protected void callAlgorithm() {
        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        alg = new AlgorithmDTIColorDisplay(eigvecSrcImage);
        alg.addListener(this);
        alg.run();
    }

    /**
     * state changed
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == anisotropyMaxSlider) {
            anisotropyMaxTextField.setText(String.valueOf(anisotropyMaxSlider.getValue() / 1000.000000f));
            anisotropyMax = Float.valueOf(anisotropyMaxTextField.getText());
            if (componentImage != null && !flag) {
                if (anisotropyMax > anisotropyMin) {
                    componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin,
                            anisotropyMax, stevensBeta, adjustExp, isMultiply);
                }
            }
        } else if (source == anisotropyMinSlider) {
            anisotropyMinTextField.setText(String.valueOf(anisotropyMinSlider.getValue() / 1000.000000f));
            anisotropyMin = Float.valueOf(anisotropyMinTextField.getText());
            if (componentImage != null && !flag) {
                if (anisotropyMin < anisotropyMax) {
                    componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin,
                            anisotropyMax, stevensBeta, adjustExp, isMultiply);
                }
            }
        } else if (source == gammaSlider) {
            gammaTextField.setText(String.valueOf(gammaSlider.getValue() / 100.000000f));
            gamma = Float.valueOf(gammaTextField.getText());
            if (componentImage != null && !flag) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
            updateCurrentColorWheel();
        } else if (source == satBlueSlider) {
            satBlueTextField.setText(String.valueOf(satBlueSlider.getValue() / 1000.000000f));
            pB = Float.valueOf(satBlueTextField.getText());
            if (componentImage != null && !flag) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
            updateCurrentColorWheel();
        } else if (source == dimGreenSlider) {
            dimGreenTextField.setText(String.valueOf(dimGreenSlider.getValue() / 100.000000f));
            pG = Float.valueOf(dimGreenTextField.getText());
            if (componentImage != null && !flag) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
            updateCurrentColorWheel();
        } else if (source == colorRangeSlider) {
            colorRangeTextField.setText(String.valueOf(colorRangeSlider.getValue() / 1000.000000f));
            pC = Float.valueOf(colorRangeTextField.getText());
            if (componentImage != null && !flag) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
            updateCurrentColorWheel();
        } else if (source == satVsThetaSlider) {
            satVsThetaTextField.setText(String.valueOf(satVsThetaSlider.getValue() / 1000.000000f));
            pS = Float.valueOf(satVsThetaTextField.getText());
            if (componentImage != null && !flag) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
            updateCurrentColorWheel();
        } else if (source == adjustExpSlider) {
            adjustExpTextField.setText(String.valueOf(adjustExpSlider.getValue() / 100.000000f));
            adjustExp = Float.valueOf(adjustExpTextField.getText());
            if (componentImage != null && !flag) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
        } else if (source == stevensBetaSlider) {
            stevensBetaTextField.setText(String.valueOf(stevensBetaSlider.getValue() / 100.000000f));
            stevensBeta = Float.valueOf(stevensBetaTextField.getText());
            if (componentImage != null && !flag) {
                componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                        stevensBeta, adjustExp, isMultiply);
            }
            updateCurrentColorWheel();
        } else if (source == resultImageSlider) {
            zSlice = resultImageSlider.getValue();
            componentImage.setSlice(zSlice);
            componentImage.show(tSlice, zSlice, true);
            currentResultImageSlicesLabel.setText( (zSlice) + "/" + (numSlices - 1));
            setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice) + "/" + (numSlices - 1)
                    + "    M:" + zoom);
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
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.anchor = GridBagConstraints.CENTER;
        if (source == colorWheelComboBox) {
            int index = colorWheelComboBox.getSelectedIndex();
            if (index == 0) {
                if (currentColorWheelType != "ABSVAL") {
                    colorWheel.setType("ABSVAL");
                    colorWheel.setPB(pB);
                    colorWheel.setPC(pC);
                    colorWheel.setPS(pS);
                    colorWheel.setPG(pG);
                    colorWheel.setStevensBeta(stevensBeta);
                    colorWheel.setGamma(gamma);
                    colorWheel.repaint();
                    currentColorWheelType = "ABSVAL";
                    if (componentImage != null) {
                        type = "ABSVAL";
                        componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin,
                                anisotropyMax, stevensBeta, adjustExp, isMultiply);
                    }
                }
                satVsThetaSlider.setEnabled(false);
                satVsThetaTextField.setEditable(false);
                satVsThetaTextField.setEnabled(false);
                satVsThetaTextField.setBackground(Color.lightGray);
            } else if (index == 1) {
                if (currentColorWheelType != "NOSYMM") {
                    colorWheel.setType("NOSYMM");
                    colorWheel.setPB(pB);
                    colorWheel.setPC(pC);
                    colorWheel.setPS(pS);
                    colorWheel.setPG(pG);
                    colorWheel.setStevensBeta(stevensBeta);
                    colorWheel.setGamma(gamma);
                    colorWheel.repaint();
                    currentColorWheelType = "NOSYMM";
                    if (componentImage != null) {
                        type = "NOSYMM";
                        componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin,
                                anisotropyMax, stevensBeta, adjustExp, isMultiply);
                    }
                }
                satVsThetaSlider.setEnabled(true);
                satVsThetaTextField.setEditable(true);
                satVsThetaTextField.setEnabled(true);
                satVsThetaTextField.setBackground(Color.white);
            } else if (index == 2) {
                if (currentColorWheelType != "ROTATIONALSYMM") {
                    colorWheel.setType("ROTATIONALSYMM");
                    colorWheel.setPB(pB);
                    colorWheel.setPC(pC);
                    colorWheel.setPS(pS);
                    colorWheel.setPG(pG);
                    colorWheel.setStevensBeta(stevensBeta);
                    colorWheel.setGamma(gamma);
                    colorWheel.repaint();
                    currentColorWheelType = "ROTATIONALSYMM";
                    if (componentImage != null) {
                        type = "ROTATIONALSYMM";
                        componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin,
                                anisotropyMax, stevensBeta, adjustExp, isMultiply);
                    }
                }
                satVsThetaSlider.setEnabled(true);
                satVsThetaTextField.setEditable(true);
                satVsThetaTextField.setEnabled(true);
                satVsThetaTextField.setBackground(Color.white);
            } else if (index == 3) {
                if (currentColorWheelType != "MIRRORSYMM") {
                    colorWheel.setType("MIRRORSYMM");
                    colorWheel.setPB(pB);
                    colorWheel.setPC(pC);
                    colorWheel.setPS(pS);
                    colorWheel.setPG(pG);
                    colorWheel.setStevensBeta(stevensBeta);
                    colorWheel.setGamma(gamma);
                    colorWheel.repaint();
                    currentColorWheelType = "MIRRORSYMM";
                    if (componentImage != null) {
                        type = "MIRRORSYMM";
                        componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin,
                                anisotropyMax, stevensBeta, adjustExp, isMultiply);
                    }
                }
                satVsThetaSlider.setEnabled(true);
                satVsThetaTextField.setEditable(true);
                satVsThetaTextField.setEnabled(true);
                satVsThetaTextField.setBackground(Color.white);
            }
        }

    }

    /**
     * update current color wheel
     */
    public void updateCurrentColorWheel() {
        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.anchor = GridBagConstraints.CENTER;
        if (currentColorWheelType == "ABSVAL") {
            colorWheel.setType("ABSVAL");
            colorWheel.setPB(pB);
            colorWheel.setPC(pC);
            colorWheel.setPS(pS);
            colorWheel.setPG(pG);
            colorWheel.setStevensBeta(stevensBeta);
            colorWheel.setGamma(gamma);
            colorWheel.repaint();
            currentColorWheelType = "ABSVAL";
        } else if (currentColorWheelType == "NOSYMM") {
            colorWheel.setType("NOSYMM");
            colorWheel.setPB(pB);
            colorWheel.setPC(pC);
            colorWheel.setPS(pS);
            colorWheel.setPG(pG);
            colorWheel.setStevensBeta(stevensBeta);
            colorWheel.setGamma(gamma);
            colorWheel.repaint();
            currentColorWheelType = "NOSYMM";
        } else if (currentColorWheelType == "ROTATIONALSYMM") {
            colorWheel.setType("ROTATIONALSYMM");
            colorWheel.setPB(pB);
            colorWheel.setPC(pC);
            colorWheel.setPS(pS);
            colorWheel.setPG(pG);
            colorWheel.setStevensBeta(stevensBeta);
            colorWheel.setGamma(gamma);
            colorWheel.repaint();
            currentColorWheelType = "ROTATIONALSYMM";
        } else if (currentColorWheelType == "MIRRORSYMM") {
            colorWheel.setType("MIRRORSYMM");
            colorWheel.setPB(pB);
            colorWheel.setPC(pC);
            colorWheel.setPS(pS);
            colorWheel.setPG(pG);
            colorWheel.setStevensBeta(stevensBeta);
            colorWheel.setGamma(gamma);
            colorWheel.repaint();
            currentColorWheelType = "MIRRORSYMM";
        }

    }

    /**
     * mouse clicked
     */
    public void mouseClicked(MouseEvent event) {
        Object source = event.getSource();

        if (source == magButton || source == unMagButton) {
            if (event.getButton() == MouseEvent.BUTTON3) {
                if (event.getSource() instanceof JButton) {
                    JButton btnSource = (JButton) event.getSource();
                    if (btnSource.getActionCommand().equals("MagImage")
                            || btnSource.getActionCommand().equals("UnMagImage")) {
                        handleZoomPopupMenu(btnSource, event);
                    }
                }
            }
        }

        if (source == refLabel3) {
            if (event.getButton() == MouseEvent.BUTTON1) {
                //openURL("http://eclipse.nichd.nih.gov/nichd/stbb/MRM42.pdf");
                //Link updated 4/5/2012
                openURL("http://mscl.cit.nih.gov/mscl_publications/pierpaoli_99.pdf");
            }
        }

    }

    /**
     * mouse entered
     */
    public void mouseEntered(MouseEvent event) {
        Object source = event.getSource();

        if (source == refLabel3) {
            setCursor(new Cursor(Cursor.HAND_CURSOR));
        }

    }

    /**
     * mouse exited
     */
    public void mouseExited(MouseEvent event) {
        Object source = event.getSource();

        if (source == refLabel3) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }

    }

    /**
     * mouse pressed
     */
    public void mousePressed(MouseEvent event) {

    }

    /**
     * mouse released
     */
    public void mouseReleased(MouseEvent e) {
        Object source = e.getSource();

        if (source == anisotropyMaxSlider) {
            if (anisotropyMaxSlider.getValue() <= anisotropyMinSlider.getValue()) {
                anisotropyMaxSlider.setValue(anisotropyMinSlider.getValue() + 1);
            }
        } else if (source == anisotropyMinSlider) {
            if (anisotropyMinSlider.getValue() >= anisotropyMaxSlider.getValue()) {
                anisotropyMinSlider.setValue(anisotropyMaxSlider.getValue() - 1);
            }
        } else if (source == gammaSlider) {
            // updateCurrentColorWheel();
        } else if (source == stevensBetaSlider) {
            // updateCurrentColorWheel();
        } else if (source == satBlueSlider) {
            // updateCurrentColorWheel();
        } else if (source == dimGreenSlider) {
            // updateCurrentColorWheel();
        } else if (source == colorRangeSlider) {
            // updateCurrentColorWheel();
        } else if (source == satVsThetaSlider) {
            // updateCurrentColorWheel();
        }

    }

    /**
     * browses and loads eigen vector file
     */
    public void loadEigenVectorFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose eigenvector file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            FileIO fileIO = new FileIO();
            if (eigvecSrcImage != null) {
                eigvecSrcImage.disposeLocal();
                eigvecSrcImage = null;
            }
            eigvecSrcImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);
            if (eigvecSrcImage.getNDims() != 4) {
                MipavUtil.displayError("Eigenvector file does not have correct dimensions");
                if (eigvecSrcImage != null) {
                    eigvecSrcImage.disposeLocal();
                }
                eigenvectorPath.setText("");
                eigvecSrcImage = null;
                return;
            }
            if (eigvecSrcImage.getExtents()[3] != 9) {
                MipavUtil.displayError("Eigenvector file does not have correct dimensions");
                if (eigvecSrcImage != null) {
                    eigvecSrcImage.disposeLocal();
                }
                eigenvectorPath.setText("");
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
            LUTa.resetTransferLine(0.0f, (int) Math.round(eigvecSrcImage.getMin()), 255.0f, (int) Math
                    .round(eigvecSrcImage.getMax()));
            int[] extents;
            extents = new int[4];
            extents[0] = Math.round(eigvecSrcImage.getExtents()[0]);
            extents[1] = Math.round(eigvecSrcImage.getExtents()[1]);
            extents[2] = Math.round(eigvecSrcImage.getExtents()[2]);
            extents[3] = Math.round(eigvecSrcImage.getExtents()[3]);
        }
    }

    /**
     * browses and loads anisotropy file
     */
    public void loadAnisotropyFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose anisotropy file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            FileIO fileIO = new FileIO();
            if (anisotropyImage != null) {
                anisotropyImage.disposeLocal();
                anisotropyImage = null;
            }
            anisotropyImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);
            if (anisotropyImage.getNDims() > 3) {
                MipavUtil.displayError("anisotropy file does not have correct dimensions");
                if (anisotropyImage != null) {
                    anisotropyImage.disposeLocal();
                }
                anisotropyPath.setText("");
                anisotropyImage = null;
                return;
            }
            anisotropyPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }

    /**
     * magnifies image
     */
    public void magImage() {
        if ( !unMagButton.isEnabled()) {
            unMagButton.setEnabled(true);
        }
        float newZoom;
        if ( (Preferences.is(Preferences.PREF_ZOOM_LINEAR))) {
            if (componentImage.getZoomX() < 1.0f) {
                newZoom = 2.0f * componentImage.getZoomX();
            } else {
                newZoom = componentImage.getZoomX() + 1.0f;
            }
        } else {
            newZoom = 2.0f * componentImage.getZoomX();
        }
        zoom = newZoom;
        if (zoom != 1) {
            captureImageButton.setEnabled(false);
        } else {
            captureImageButton.setEnabled(true);
        }
        componentImage.setZoom(newZoom, newZoom);
        validate();
        componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                stevensBeta, adjustExp, isMultiply);

        magLabel.setText("M:" + zoom);
        setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice) + "/" + (numSlices - 1)
                + "    M:" + zoom);
        if (componentImage.getZoomX() >= 32) {
            magButton.setEnabled(false);
        }
    }

    /**
     * un-magnifies image
     */
    public void unMagImage() {
        if ( !magButton.isEnabled()) {
            magButton.setEnabled(true);
        }
        float newZoom;
        if ( (Preferences.is(Preferences.PREF_ZOOM_LINEAR)) && (componentImage.getZoomX() > 1.0f)) {
            // linear zoom is prevented if getZoomX() <= 1.0
            newZoom = componentImage.getZoomX() - 1.0f;
        } else {
            newZoom = 0.5f * componentImage.getZoomX();
        }
        zoom = newZoom;
        if (zoom != 1) {
            captureImageButton.setEnabled(false);
        } else {
            captureImageButton.setEnabled(true);
        }
        componentImage.setZoom(newZoom, newZoom);
        validate();
        componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                stevensBeta, adjustExp, isMultiply);
        magLabel.setText("M:" + zoom);
        setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice) + "/" + (numSlices - 1)
                + "    M:" + zoom);
        if (componentImage.getZoomX() <= 0.125) {
            unMagButton.setEnabled(false);
        }
    }

    /**
     * sets image zoom to 1
     */
    public void zoomOne() {
        if ( !unMagButton.isEnabled()) {
            unMagButton.setEnabled(true);
        }
        if ( !magButton.isEnabled()) {
            magButton.setEnabled(true);
        }
        float newZoom = 1;
        zoom = newZoom;
        captureImageButton.setEnabled(true);
        componentImage.setZoom(newZoom, newZoom);
        validate();
        componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                stevensBeta, adjustExp, isMultiply);
        magLabel.setText("M:" + zoom);
        setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice) + "/" + (numSlices - 1)
                + "    M:" + zoom);
    }

    /**
     * restore default parameters
     */
    public void restoreDefaults() {
        flag = true;
        anisotropyMaxSlider.setValue(700);
        anisotropyMax = 0.7f;
        anisotropyMinSlider.setValue(0);
        anisotropyMin = 0.0f;
        gammaSlider.setValue(180);
        gamma = 1.8f;
        satBlueSlider.setValue(350);
        pB = 0.35f;
        dimGreenSlider.setValue(80);
        pG = 0.8f;
        colorRangeSlider.setValue(700);
        pC = 0.7f;
        satVsThetaSlider.setValue(500);
        pS = 0.5f;
        adjustExpSlider.setValue(50);
        adjustExp = 0.5f;
        stevensBetaSlider.setValue(40);
        stevensBeta = 0.4f;
        multRadio.setSelected(true);
        isMultiply = true;
        if (componentImage != null) {
            componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin, anisotropyMax,
                    stevensBeta, adjustExp, isMultiply);
        }
        updateCurrentColorWheel();
        flag = false;
    }

    /**
     * save heuristic parameters
     */
    public void saveParams() {
        String fileName = "", directory = "";
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.setDialogTitle("Save");
        int returnValue = chooser.showSaveDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = chooser.getCurrentDirectory().toString() + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            try {
                out = new BufferedWriter(new FileWriter(directory + fileName));
                out.write("anisotropyMax:" + anisotropyMax);
                out.newLine();
                out.write("anisotropyMin:" + anisotropyMin);
                out.newLine();
                out.write("gamma:" + gamma);
                out.newLine();
                out.write("satBlue:" + pB);
                out.newLine();
                out.write("dimGreen:" + pG);
                out.newLine();
                out.write("colorRange:" + pC);
                out.newLine();
                out.write("satVsTheta:" + pS);
                out.newLine();
                out.write("adjustExp:" + adjustExp);
                out.newLine();
                out.write("stevensBeta:" + stevensBeta);

            } catch (Exception e) {
                e.printStackTrace();
                MipavUtil.displayError("Error writing params to file");
                return;
            }
        } else {
            return;
        }
    }

    /**
     * loads heuristic parameter values
     */
    public void loadParams() {
        String fileName = "", directory = "";
        String line;
        String[] splits;
        float num;
        float[] vals = new float[9];
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.setDialogTitle("Choose eigenvector file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = chooser.getCurrentDirectory().toString() + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            try {
                in = new BufferedReader(new FileReader(directory + fileName));
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("anisotropyMax")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[0] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("anisotropyMin")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[1] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("gamma")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[2] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("satBlue")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[3] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("dimGreen")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[4] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("colorRange")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[5] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("satVsTheta")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[6] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("adjustExp")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[7] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                line = in.readLine();
                if (line != null) {
                    splits = line.split(":");
                    if (splits[0].equals("stevensBeta")) {
                        try {
                            num = Float.parseFloat(splits[1]);
                        } catch (NumberFormatException e) {
                            e.printStackTrace();
                            MipavUtil.displayError("Error reading params from file");
                            return;
                        }
                        vals[8] = num;
                    } else {
                        MipavUtil.displayError("Error reading params from file");
                        return;
                    }
                } else {
                    MipavUtil.displayError("Error reading params from file");
                    return;
                }
                // this means we read the file successfully...so now lets set the vars, set the sliders, update color
                // wheel and image
                flag = true;
                anisotropyMaxSlider.setValue((int) (vals[0] * 1000));
                anisotropyMax = vals[0];
                anisotropyMinSlider.setValue((int) (vals[1] * 1000));
                anisotropyMin = vals[1];
                gammaSlider.setValue((int) (vals[2] * 100));
                gamma = vals[2];
                satBlueSlider.setValue((int) (vals[3] * 1000));
                pB = vals[3];
                dimGreenSlider.setValue((int) (vals[4] * 100));
                pG = vals[4];
                colorRangeSlider.setValue((int) (vals[5] * 1000));
                pC = vals[5];
                satVsThetaSlider.setValue((int) (vals[6] * 1000));
                pS = vals[6];
                adjustExpSlider.setValue((int) (vals[7] * 100));
                adjustExp = vals[7];
                stevensBetaSlider.setValue((int) (vals[8] * 100));
                stevensBeta = vals[8];
                if (componentImage != null) {
                    componentImage.show(tSlice, zSlice, true, type, pS, pB, pC, pG, gamma, anisotropyMin,
                            anisotropyMax, stevensBeta, adjustExp, isMultiply);
                }
                updateCurrentColorWheel();
                flag = false;

            } catch (Exception e) {
                e.printStackTrace();
                MipavUtil.displayError("Error reading params from file");
                return;
            }
        } else {
            return;
        }
    }

    /**
     * Scrolls through all z slices of a 3d/4d image and captures them into a new ARGB ModelImage, then puts the
     * ModelImage in a ViewJFrameImage.
     * 
     * @return
     */
    private boolean captureImage() {
        int[] pixels;
        int bufferSize;
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
            resultImageSlider.setValue(slice);

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
                buffer[k + 1] = (short) ( (pixels[i] >> 16) & 0xFF); // Red
                buffer[k + 2] = (short) ( (pixels[i] >> 8) & 0xFF); // Green
                buffer[k + 3] = (short) (pixels[i] & 0xFF); // Blue
            }

            /**
             * Import the ARGB buffer into the model image
             */
            try {
                screenCaptureImage.importData( (buffer.length * slice), buffer, false);
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
        for (int i = 0; i < fileInfoBases.length; i++) {
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
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Anisotropy Min";
            value = String.valueOf(anisotropyMin);
            fileInfoBases[i].getPSet(psetDesc).addParameter(name);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Gamma Correction";
            value = String.valueOf(gamma);
            fileInfoBases[i].getPSet(psetDesc).addParameter(name);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Blue Saturation";
            value = String.valueOf(pB);
            fileInfoBases[i].getPSet(psetDesc).addParameter(name);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Green Adjustment";
            value = String.valueOf(pG);
            fileInfoBases[i].getPSet(psetDesc).addParameter(name);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Color Range";
            value = String.valueOf(pC);
            fileInfoBases[i].getPSet(psetDesc).addParameter(name);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Saturation vs Theta";
            value = String.valueOf(pS);
            fileInfoBases[i].getPSet(psetDesc).addParameter(name);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Adjust Exponent";
            value = String.valueOf(adjustExp);
            fileInfoBases[i].getPSet(psetDesc).addParameter(name);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Stevens Beta";
            value = String.valueOf(stevensBeta);
            fileInfoBases[i].getPSet(psetDesc).addParameter(name);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValue(value);
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setDescription("");
            fileInfoBases[i].getPSet(psetDesc).getParameter(name).setValueType("float");
            name = "Truncate / Multiply";
            if (isMultiply) {
                value = "Multiply";
            } else {
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

        resultImageSlider.setValue( (numSlices - 1) / 2);

        return true;
    }

    /**
     * window closing
     */
    public void windowClosing(WindowEvent event) {
        finalize();
        if (resultImage != null) {
            resultImage.disposeLocal();
        }
        super.windowClosing(event);
        if (alg != null) {
            alg.setThreadStopped(true);
        }

        dispose();
    }

    /**
     * mouse wheel moved
     * 
     * @param event
     */
    public void mouseWheelMoved(MouseWheelEvent event) {
        int wheelRotation = event.getWheelRotation();
        Object source = event.getSource();
        if (source == componentImage || source == resultScrollPanel || source == resultImageSlider
                || source == resultImageSliderPanel) {
            if (wheelRotation < 0) {
                if (zSlice != numSlices - 1) {
                    zSlice = zSlice + 1;
                    resultImageSlider.setValue(zSlice);
                    currentResultImageSlicesLabel.setText( (zSlice) + "/" + (numSlices - 1));
                    setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice) + "/"
                            + (numSlices - 1) + "    M:" + zoom);
                }
            } else {
                if (zSlice != 0) {
                    zSlice = zSlice - 1;
                    resultImageSlider.setValue(zSlice);
                    currentResultImageSlicesLabel.setText( (zSlice) + "/" + (numSlices - 1));
                    setTitle(title + eigvecFilename + " , " + anisotropyFilename + "    " + (zSlice) + "/"
                            + (numSlices - 1) + "    M:" + zoom);
                }
            }
        }
    }

    /**
     * key typed
     */
    public void keyTyped(KeyEvent event) {
        Object source = event.getSource();

        if (event.getKeyChar() == KeyEvent.VK_ENTER) {
            if (source == anisotropyMaxTextField) {
                String numString = anisotropyMaxTextField.getText();
                float num = validateCurrentNum(numString, minAnisotropyMax, maxAnisotropyMax);
                if (num != -1) {
                    if ( ((int) (num * 1000)) <= anisotropyMinSlider.getValue()) {
                        num = (anisotropyMinSlider.getValue() + 1) / 1000.000000f;
                    }
                    if (num == anisotropyMaxSlider.getValue() / 1000.000000f) {
                        anisotropyMaxTextField.setText(String.valueOf(anisotropyMaxSlider.getValue() / 1000.000000f));
                    }
                    if ( ((int) (num * 1000)) != anisotropyMaxSlider.getValue()) {
                        anisotropyMax = num;
                        // updateCurrentColorWheel();
                        anisotropyMaxSlider.setValue((int) (num * 1000));
                    }
                } else {
                    anisotropyMaxTextField.setText(String.valueOf(anisotropyMaxSlider.getValue() / 1000.000000f));
                }
            } else if (source == anisotropyMinTextField) {
                String numString = anisotropyMinTextField.getText();
                float num = validateCurrentNum(numString, minAnisotropyMin, maxAnisotropyMin);
                if (num != -1) {
                    if ( ((int) (num * 1000)) >= anisotropyMaxSlider.getValue()) {
                        num = (anisotropyMaxSlider.getValue() - 1) / 1000.000000f;
                    }
                    if (num == anisotropyMinSlider.getValue() / 1000.000000f) {
                        anisotropyMinTextField.setText(String.valueOf(anisotropyMinSlider.getValue() / 1000.000000f));
                    }
                    if ( ((int) (num * 1000)) != anisotropyMinSlider.getValue()) {
                        anisotropyMin = num;
                        // updateCurrentColorWheel();
                        anisotropyMinSlider.setValue((int) (num * 1000));
                    }
                } else {
                    anisotropyMinTextField.setText(String.valueOf(anisotropyMinSlider.getValue() / 1000.000000f));
                }
            } else if (source == gammaTextField) {
                String numString = gammaTextField.getText();
                float num = validateCurrentNum(numString, minGamma, maxGamma);
                if (num != -1) {
                    if ( ((int) (num * 100)) != gammaSlider.getValue()) {
                        gamma = num;
                        updateCurrentColorWheel();
                        gammaSlider.setValue((int) (num * 100));
                    }
                } else {
                    gammaTextField.setText(String.valueOf(gammaSlider.getValue() / 100.000000f));
                }
            } else if (source == satBlueTextField) {
                String numString = satBlueTextField.getText();
                float num = validateCurrentNum(numString, minSatBlue, maxSatBlue);
                if (num != -1) {
                    if ( ((int) (num * 1000)) != satBlueSlider.getValue()) {
                        pB = num;
                        updateCurrentColorWheel();
                        satBlueSlider.setValue((int) (num * 1000));
                    }
                } else {
                    satBlueTextField.setText(String.valueOf(satBlueSlider.getValue() / 1000.000000f));
                }
            } else if (source == dimGreenTextField) {
                String numString = dimGreenTextField.getText();
                float num = validateCurrentNum(numString, minDimGreen, maxDimGreen);
                if (num != -1) {
                    if ( ((int) (num * 100)) != dimGreenSlider.getValue()) {
                        pG = num;
                        updateCurrentColorWheel();
                        dimGreenSlider.setValue((int) (num * 100));
                    }
                } else {
                    dimGreenTextField.setText(String.valueOf(dimGreenSlider.getValue() / 100.000000f));
                }
            } else if (source == colorRangeTextField) {
                String numString = colorRangeTextField.getText();
                float num = validateCurrentNum(numString, minColorRange, maxColorRange);
                if (num != -1) {
                    if ( ((int) (num * 1000)) != colorRangeSlider.getValue()) {
                        pC = num;
                        updateCurrentColorWheel();
                        colorRangeSlider.setValue((int) (num * 1000));
                    }
                } else {
                    colorRangeTextField.setText(String.valueOf(colorRangeSlider.getValue() / 1000.000000f));
                }
            } else if (source == satVsThetaTextField) {
                String numString = satVsThetaTextField.getText();
                float num = validateCurrentNum(numString, minSatVsTheta, maxSatVsTheta);
                if (num != -1) {
                    if ( ((int) (num * 1000)) != satVsThetaSlider.getValue()) {
                        pS = num;
                        updateCurrentColorWheel();
                        satVsThetaSlider.setValue((int) (num * 1000));
                    }
                } else {
                    satVsThetaTextField.setText(String.valueOf(satVsThetaSlider.getValue() / 1000.000000f));
                }
            } else if (source == adjustExpTextField) {
                String numString = adjustExpTextField.getText();
                float num = validateCurrentNum(numString, minAdjustExp, maxAdjustExp);
                if (num != -1) {
                    if ( ((int) (num * 100)) != adjustExpSlider.getValue()) {
                        adjustExp = num;
                        updateCurrentColorWheel();
                        adjustExpSlider.setValue((int) (num * 100));
                    }
                } else {
                    adjustExpTextField.setText(String.valueOf(adjustExpSlider.getValue() / 100.000000f));
                }
            } else if (source == stevensBetaTextField) {
                String numString = stevensBetaTextField.getText();
                float num = validateCurrentNum(numString, minStevensBeta, maxStevensBeta);
                if (num != -1) {
                    if ( ((int) (num * 100)) != stevensBetaSlider.getValue()) {
                        stevensBeta = num;
                        updateCurrentColorWheel();
                        stevensBetaSlider.setValue((int) (num * 100));
                    }
                } else {
                    stevensBetaTextField.setText(String.valueOf(stevensBetaSlider.getValue() / 100.000000f));
                }
            }
        }
    }

    /**
     * focus lost
     */
    public void focusLost(FocusEvent event) {
        Object source = event.getSource();

        if (source == anisotropyMaxTextField) {
            String numString = anisotropyMaxTextField.getText();
            float num = validateCurrentNum(numString, minAnisotropyMax, maxAnisotropyMax);
            if (num != -1) {
                if ( ((int) (num * 1000)) <= anisotropyMinSlider.getValue()) {
                    num = (anisotropyMinSlider.getValue() + 1) / 1000.000000f;
                }
                if (num == anisotropyMaxSlider.getValue() / 1000.000000f) {
                    anisotropyMaxTextField.setText(String.valueOf(anisotropyMaxSlider.getValue() / 1000.000000f));
                }
                if ( ((int) (num * 1000)) != anisotropyMaxSlider.getValue()) {
                    anisotropyMax = num;
                    // updateCurrentColorWheel();
                    anisotropyMaxSlider.setValue((int) (num * 1000));
                }
            } else {
                anisotropyMaxTextField.setText(String.valueOf(anisotropyMaxSlider.getValue() / 1000.000000f));
            }
        } else if (source == anisotropyMinTextField) {
            String numString = anisotropyMinTextField.getText();
            float num = validateCurrentNum(numString, minAnisotropyMin, maxAnisotropyMin);
            if (num != -1) {
                if ( ((int) (num * 1000)) >= anisotropyMaxSlider.getValue()) {
                    num = (anisotropyMaxSlider.getValue() - 1) / 1000.000000f;
                }
                if (num == anisotropyMinSlider.getValue() / 1000.000000f) {
                    anisotropyMinTextField.setText(String.valueOf(anisotropyMinSlider.getValue() / 1000.000000f));
                }
                if ( ((int) (num * 1000)) != anisotropyMinSlider.getValue()) {
                    anisotropyMin = num;
                    // updateCurrentColorWheel();
                    anisotropyMinSlider.setValue((int) (num * 1000));
                }
            } else {
                anisotropyMinTextField.setText(String.valueOf(anisotropyMinSlider.getValue() / 1000.000000f));
            }
        } else if (source == gammaTextField) {
            String numString = gammaTextField.getText();
            float num = validateCurrentNum(numString, minGamma, maxGamma);
            if (num != -1) {
                if ( ((int) (num * 100)) != gammaSlider.getValue()) {
                    gamma = num;
                    updateCurrentColorWheel();
                    gammaSlider.setValue((int) (num * 100));
                }
            } else {
                gammaTextField.setText(String.valueOf(gammaSlider.getValue() / 100.000000f));
            }
        } else if (source == satBlueTextField) {
            String numString = satBlueTextField.getText();
            float num = validateCurrentNum(numString, minSatBlue, maxSatBlue);
            if (num != -1) {
                if ( ((int) (num * 1000)) != satBlueSlider.getValue()) {
                    pB = num;
                    updateCurrentColorWheel();
                    satBlueSlider.setValue((int) (num * 1000));
                }
            } else {
                satBlueTextField.setText(String.valueOf(satBlueSlider.getValue() / 1000.000000f));
            }
        } else if (source == dimGreenTextField) {
            String numString = dimGreenTextField.getText();
            float num = validateCurrentNum(numString, minDimGreen, maxDimGreen);
            if (num != -1) {
                if ( ((int) (num * 100)) != dimGreenSlider.getValue()) {
                    pG = num;
                    updateCurrentColorWheel();
                    dimGreenSlider.setValue((int) (num * 100));
                }
            } else {
                dimGreenTextField.setText(String.valueOf(dimGreenSlider.getValue() / 100.000000f));
            }
        } else if (source == colorRangeTextField) {
            String numString = colorRangeTextField.getText();
            float num = validateCurrentNum(numString, minColorRange, maxColorRange);
            if (num != -1) {
                if ( ((int) (num * 1000)) != colorRangeSlider.getValue()) {
                    pC = num;
                    updateCurrentColorWheel();
                    colorRangeSlider.setValue((int) (num * 1000));
                }
            } else {
                colorRangeTextField.setText(String.valueOf(colorRangeSlider.getValue() / 1000.000000f));
            }
        } else if (source == satVsThetaTextField) {
            String numString = satVsThetaTextField.getText();
            float num = validateCurrentNum(numString, minSatVsTheta, maxSatVsTheta);
            if (num != -1) {
                if ( ((int) (num * 1000)) != satVsThetaSlider.getValue()) {
                    pS = num;
                    updateCurrentColorWheel();
                    satVsThetaSlider.setValue((int) (num * 1000));
                }
            } else {
                satVsThetaTextField.setText(String.valueOf(satVsThetaSlider.getValue() / 1000.000000f));
            }
        } else if (source == adjustExpTextField) {
            String numString = adjustExpTextField.getText();
            float num = validateCurrentNum(numString, minAdjustExp, maxAdjustExp);
            if (num != -1) {
                if ( ((int) (num * 100)) != adjustExpSlider.getValue()) {
                    adjustExp = num;
                    updateCurrentColorWheel();
                    adjustExpSlider.setValue((int) (num * 100));
                }
            } else {
                adjustExpTextField.setText(String.valueOf(adjustExpSlider.getValue() / 100.000000f));
            }
        } else if (source == stevensBetaTextField) {
            String numString = stevensBetaTextField.getText();
            float num = validateCurrentNum(numString, minStevensBeta, maxStevensBeta);
            if (num != -1) {
                if ( ((int) (num * 100)) != stevensBetaSlider.getValue()) {
                    stevensBeta = num;
                    updateCurrentColorWheel();
                    stevensBetaSlider.setValue((int) (num * 100));
                }
            } else {
                stevensBetaTextField.setText(String.valueOf(stevensBetaSlider.getValue() / 100.000000f));
            }
        }

    }

    /**
     * validate current number
     * 
     * @param numString
     * @param min
     * @param max
     * @return
     */
    public float validateCurrentNum(String numString, float min, float max) {
        float num;

        try {
            num = Float.parseFloat(numString);
        } catch (NumberFormatException e) {
            return -1;
        }
        if (num >= min && num <= max) {
            return num;
        } else {
            return -1;
        }
    }

    /**
     * Create the intensity buffer for an image.
     * 
     * @param extents the extents of the image
     * @param isColor whether the image is in color
     * @return a buffer which is big enough to contain the image intensity data
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
     * @param extents the extents of the image
     * @return a buffer which is big enough to contain the image pixel data
     */
    protected static int[] initPixelBuffer(int[] extents) {
        return new int[extents[0] * extents[1]];
    }

    /**
     * init resolutions
     * 
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
     * 
     * @param img
     * @return
     */
    protected static int[] initUnits(ModelImage img) {
        return img.getFileInfo(0).getUnitsOfMeasure();
    }

    /**
     * Get the resolution correction needed for non-isotropic images.
     * 
     * @param imgResols the image resolution
     * @param imgUnits the image units of measure
     * @return the resolution correction factor in the x (the first element) and y (the second element) dimensions
     */
    protected static float[] initResFactor(float[] imgResols, int[] imgUnits) {
        float[] resFactor = new float[2];

        resFactor[0] = 1.0f;
        resFactor[1] = 1.0f;

        if ( (imgResols[1] >= imgResols[0]) && (imgResols[1] < (20.0f * imgResols[0])) && (imgUnits[0] == imgUnits[1])) {
            resFactor[1] = imgResols[1] / imgResols[0];
        } else if ( (imgResols[0] > imgResols[1]) && (imgResols[0] < (20.0f * imgResols[1]))
                && (imgUnits[0] == imgUnits[1])) {
            resFactor[0] = imgResols[0] / imgResols[1];
        }

        return resFactor;
    }

    /**
     * Creates and initializes the ModelRGB for an image.
     * 
     * @param img the image to create a ModelRGB for
     * @return a ModelRGB for the image <code>img</code> (null if NOT a color image)
     * @throws OutOfMemoryError if enough memory cannot be allocated for this method
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
            y[1] = (dim.height - 1) - ( (dim.height - 1) / 3.0f);

            x[2] = 255 * 0.667f;
            y[2] = (dim.height - 1) - ( (dim.height - 1) * 0.67f);

            x[3] = 255;
            y[3] = 0;

            int[] RGBExtents = new int[2];
            RGBExtents[0] = 4;
            RGBExtents[1] = 256;
            newRGB = new ModelRGB(RGBExtents);
            newRGB.getRedFunction().importArrays(x, y, 4);
            newRGB.getGreenFunction().importArrays(x, y, 4);
            newRGB.getBlueFunction().importArrays(x, y, 4);
            newRGB.makeRGB( -1);
        }

        return newRGB;
    }

    /**
     * Initializes the variables based on the image extents. (i.e. number of slices, number of time slices, the initial
     * z-slice, etc.
     * 
     * @param img the image to set the extent variables for
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
     * @param img the image to get the slice positions of
     * @return an array containing the slice in the volume (in the first element) and the time slice (in the second
     *         element)
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
     * @param img the image to get the slices of
     * @return an array containing the number of volume slices (in the first element) and the number of time slices in
     *         the image (in the second element)
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
     * finalize
     */
    public void finalize() {
        if (eigvecSrcImage != null) {
            eigvecSrcImage.disposeLocal();
        }
        if (anisotropyImage != null) {
            anisotropyImage.disposeLocal();
        }

        eigvecSrcImage = null;
        anisotropyImage = null;
    }

    /**
     * Gets the RGB LUT table for ARGB image A.
     * 
     * @return RGBT the new RGB LUT to be applied to the image
     */
    public ModelRGB getRGBTA() {
        return (componentImage.getRGBTA());
    }

    /**
     * Sets the RGB LUT table for ARGB image A.
     * 
     * @param RGBT the new RGB LUT to be applied to the image
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
     * focus gained
     */
    public void focusGained(FocusEvent event) {

    }

    /**
     * key pressed
     */
    public void keyPressed(KeyEvent event) {
    // TODO Auto-generated method stub

    }

    /**
     * key released
     */
    public void keyReleased(KeyEvent event) {
    // TODO Auto-generated method stub

    }

    /**
     * Launches browser...code obtained from: Bare Bones Browser Launch by Dem Pilafian Web Page Copyright (c) 2007
     * Center Key Software Source Code and Javadoc are Public Domain http://www.centerkey.com/java/browser
     * 
     * @param url
     */
    public void openURL(String url) {

        String osName = System.getProperty("os.name");
        try {
            if (osName.startsWith("Mac OS")) {
                Class fileMgr = Class.forName("com.apple.eio.FileManager");
                Method openURL = fileMgr.getDeclaredMethod("openURL", new Class[] {String.class});
                openURL.invoke(null, new Object[] {url});
            } else if (osName.startsWith("Windows")) {
                Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url);
            } else { // assume Unix or Linux
                String[] browsers = {"firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape"};
                String browser = null;
                for (int count = 0; count < browsers.length && browser == null; count++) {
                    if (Runtime.getRuntime().exec(new String[] {"which", browsers[count]}).waitFor() == 0) {
                        browser = browsers[count];
                    }
                }
                if (browser == null) {
                    System.out.println("Can not find web browser");
                } else {
                    Runtime.getRuntime().exec(new String[] {browser, url});
                }
            }
        } catch (Exception e) {
            System.out.println("Can not find web browser");
        }
    }

}
