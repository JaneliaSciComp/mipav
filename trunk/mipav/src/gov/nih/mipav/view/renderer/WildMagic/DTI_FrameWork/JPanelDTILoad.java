package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTI2EGFA;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTIColorDisplay;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTITract;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import WildMagic.LibFoundation.Mathematics.GMatrixf;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogDirectResample;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.*;
import java.awt.event.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import javax.swing.*;

public class JPanelDTILoad extends JInterfaceBase implements AlgorithmInterface {
    /**
     * 
     */
    private static final long serialVersionUID = 8011148684175711251L;

    /** Box layout for control panel. */
    private Box contentBox;

    private JButton openDTIimageButton;

    private JTextField textDTIimage;

    private JButton openDTIimageListFile,openDTIMaskImage;

    private JTextField textDTIimageListFile,textDTIListFile,textDTIMaskImage;

    private JButton openDWIimageBT;

    private JCheckBox reconstructTracts;

    private JButton computeButton;

    /** X-dimensions for Diffusion Weighted Images. */
    private int m_iDimX = 0;

    /** Y-dimensions for Diffusion Weighted Images. */
    private int m_iDimY = 0;

    /** parent directory for the DTI output images. */
    private String m_kParentDir = null;

    /** Diffusion Tensor image. */
    private ModelImage m_kDTIImage = null;

    /** Slice thickness read from .list file */
    private float m_fResX = 1f, m_fResY = 1f, m_fResZ = 1f;
    /** Set to true if the slice resolution is read from the .list file: (xRes) */
    private boolean m_bUseXRes = false;
    /** Set to true if the slice resolution is read from the .list file: (yRes) */
    private boolean m_bUseYRes = false;
    /** Set to true if the slice resolution is read from the .list file: (zRes) */
    private boolean m_bUseZRes = false;

    /** Mean noise vale read from the .list file */
    private float m_fMeanNoise = 0f;

    /** raw image format read from the .list file: */
    private String m_kRawFormat;

    /** Number of slices in the Diffusion Weighted Images series. */
    private int m_iSlices = 0;

    /** Number of weights in the Diffusion Weighted Images series. */
    private int m_iWeights = 0;

    /** General matrix storing BMatrix values. */
    private GMatrixf m_kBMatrix = null;

    /**
     * List of file names for the Diffusion Weighted Images, from the .path
     * file.
     */
    private String[][] m_aakDWIList = null;

    /** keeps track of unique entries in the BMatrix */
    private int[] m_aiMatrixEntries;

    /** Number of different BMatrix rows: */
    private int m_iBOrig = 0;

    /** Eigenvector image **/
    private ModelImage m_kEigenVectorImage;

    /** EigenValue image **/
    private ModelImage m_kEigenValueImage;

    /** Anisotropy image **/
    private ModelImage m_kAnisotropyImage;

    private VolumeTriPlanarInterfaceDTI parentFrame;

    /** handle to the algorithm **/
    private AlgorithmDTIColorDisplay alg;

    /** result image **/
    private ModelImage resultImage;

    /** mask image **/
    private ModelImage m_kDWIMaskImage;

    private GridBagConstraints mainPanelGBC;

    private ButtonGroup loadDataRadioGroup;

    private JRadioButton listRadio, dtiRadio;

    private JLabel dtiFileLabel,maskImageLabel,listFileLabel;

    private JCheckBox tractCheckBox;
    private JCheckBox m_kNegX;
    private JCheckBox m_kNegY;
    private JCheckBox m_kNegZ;
    private JTextField m_kFAMinThreshold;
    private JTextField m_kFAMaxThreshold;
    private JTextField m_kMaxAngle;

    private AlgorithmDWI2DTI kAlgorithm;




    public JPanelDTILoad(VolumeTriPlanarInterfaceDTI _parentFrame) {
        parentFrame = _parentFrame;
        mainPanel = new JPanel();
        init();
    }

    
    /**
     * Dispose memory.
     */
    public void disposeLocal() {
    	if ( resultImage != null ) {
    		resultImage.disposeLocal();
    		resultImage = null;
    	}
    }

    public void init() {
        mainPanelGBC = new GridBagConstraints();
        mainPanel.setLayout(new GridBagLayout());
        loadDataRadioGroup = new ButtonGroup();

        buildDWIListLoadPanel();

        buildDTILoadPanel();

        buildOptionsPanel();

        computeButton = new JButton("Compute");
        computeButton.setToolTipText("Compute");
        computeButton.addActionListener(this);
        computeButton.setActionCommand("compute");
        computeButton.setVisible(true);
        computeButton.setEnabled(false);

        mainPanelGBC.gridx = 0;
        mainPanelGBC.gridy = 4;
        mainPanelGBC.insets = new Insets(20,0,0,0);
        mainPanelGBC.fill = GridBagConstraints.NONE;
        mainPanel.add(computeButton,mainPanelGBC);



    }

    public void setDTIimage() {
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
        parentFrame.setDTIColorImage(resultImage);
    }

    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();
        if (command.equalsIgnoreCase("browseDTIFile")) {
            //openDTIimageListFile.setEnabled(false);
            //textDTIimageListFile.setEnabled(false);
            loadDTIFile();
        } else if (command.equalsIgnoreCase("browseListFile")) {
            //openDTIimageButton.setEnabled(false);
            //textDTIimage.setEnabled(false);
            loadDWIListFile();
        } else if(command.equalsIgnoreCase("browseMaskImage")) {
            loadDWIMaskFile();
        }
        else if ( command.equals( "FAMINChanged" ) )
        {
            if (!JDialogBase.testParameter(m_kFAMinThreshold.getText(), 0.0f, 1.0f))
            {
                m_kFAMinThreshold.requestFocus();
                m_kFAMinThreshold.selectAll();
            }
        }

        else if ( command.equals( "FAMAXChanged" ) )
        {
            if (!JDialogBase.testParameter(m_kFAMaxThreshold.getText(), 0.0f, 1.0f))
            {
                m_kFAMaxThreshold.requestFocus();
                m_kFAMaxThreshold.selectAll();
            }
        }

        else if ( command.equals( "MaxAngleChanged" ) )
        {
            if (!JDialogBase.testParameter(m_kMaxAngle.getText(), 0.0f, 180f))
            {
                m_kMaxAngle.requestFocus();
                m_kMaxAngle.selectAll();
            }
        }
        else if ( command.equalsIgnoreCase("compute")) {
            if(dtiRadio.isSelected()) {
                processDTI();
                setParentDir();
                setDTIimage();
                setEVimage();
                setEValueimage();
                setFAimage();
                setDTIColorImage();
                float fFAMin = Float.valueOf(m_kFAMinThreshold.getText()).floatValue();
                float fFAMax = Float.valueOf(m_kFAMaxThreshold.getText()).floatValue();
                float fMaxAngle = Float.valueOf(m_kMaxAngle.getText()).floatValue();
                //if tract reconstruction checkbox is selectd, do the fiber calculation
                if(tractCheckBox.isSelected()) {
                    AlgorithmDTITract kTractAlgorithm = new AlgorithmDTITract(
                            parentFrame.getDTIimage(), parentFrame.getFAimage(), parentFrame.getEVimage(), 
                            parentFrame.getEValueimage(),
                            parentFrame.getParentDir() + "DTIImage.xml_tract",
                            m_kNegX.isSelected(), m_kNegY.isSelected(), m_kNegZ.isSelected(),
                            fFAMin, fFAMax, fMaxAngle );
                    kTractAlgorithm.run();
                    kTractAlgorithm.disposeLocal();
                    kTractAlgorithm = null;
                }
                parentFrame.setDTIParamsActive();
            }else {
                if ( m_kBMatrix == null || m_aakDWIList == null )
                {
                    MipavUtil.displayError("Both BMatrix and .path files are needed.");
                    return;
                }
                processDWI();
            }


        }
        else if(command.equals("dtiRadio")) {
            openDTIimageButton.setEnabled(true);
            textDTIimage.setBackground(Color.white);
            dtiFileLabel.setEnabled(true);

            openDTIimageListFile.setEnabled(false);
            openDTIMaskImage.setEnabled(false);
            textDTIListFile.setBackground(Color.lightGray);
            textDTIMaskImage.setBackground(Color.lightGray);
            maskImageLabel.setEnabled(false);
            listFileLabel.setEnabled(false);

        }
        else if(command.equals("listRadio")) {
            openDTIimageListFile.setEnabled(true);
            openDTIMaskImage.setEnabled(true);
            textDTIListFile.setBackground(Color.white);
            textDTIMaskImage.setBackground(Color.white);
            maskImageLabel.setEnabled(true);
            listFileLabel.setEnabled(true);

            openDTIimageButton.setEnabled(false);
            textDTIimage.setBackground(Color.lightGray);
            dtiFileLabel.setEnabled(false);
        }

    }


    public void algorithmPerformed(AlgorithmBase algorithm) {
        //this is for the DWItoDTI algorithm
        if(kAlgorithm.isCompleted()) {
            m_kDTIImage = ((AlgorithmDWI2DTI)kAlgorithm).getDTIImage();
            ((AlgorithmDWI2DTI)kAlgorithm).disposeLocal();
            processDTI();
            setParentDir();
            setDTIimage();
            setEVimage();
            setEValueimage();
            setFAimage();
            setDTIColorImage();
            //if tract reconstruction checkbox is selectd, do the fiber calculation
            if(tractCheckBox.isSelected()) {
                AlgorithmDTITract kTractAlgorithm = new AlgorithmDTITract(
                        parentFrame.getDTIimage(), parentFrame.getEVimage(), 
                        parentFrame.getEValueimage(),
                        parentFrame.getParentDir() + "DTIImage.xml_tract",
                        m_kNegX.isSelected(), m_kNegY.isSelected(), m_kNegZ.isSelected() );
                kTractAlgorithm.run();
                kTractAlgorithm.disposeLocal();
                kTractAlgorithm = null;
                parentFrame.setDTIParamsActive();
            }
        }

    }

    public void buildOptionsPanel() {
        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        tractCheckBox = new JCheckBox("Tract Reconstruction");
        tractCheckBox.setSelected(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        //gbc.insets = new Insets(0,0,10,0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionsPanel.add(tractCheckBox, gbc);


        m_kNegX = new JCheckBox("+/- x");
        m_kNegX.setSelected(false);
        m_kNegX.addActionListener(this);
        m_kNegX.setActionCommand("NegX");
        m_kNegX.setEnabled(true);

        m_kNegY = new JCheckBox("+/- y");
        m_kNegY.setSelected(false);
        m_kNegY.addActionListener(this);
        m_kNegY.setActionCommand("NegY");
        m_kNegY.setEnabled(true);

        m_kNegZ = new JCheckBox("+/- z");
        m_kNegZ.setSelected(true);
        m_kNegZ.addActionListener(this);
        m_kNegZ.setActionCommand("NegZ");
        m_kNegZ.setEnabled(true);


        JPanel kVectorPanel = new JPanel();
        kVectorPanel.setLayout(new BoxLayout(kVectorPanel, BoxLayout.X_AXIS));
        kVectorPanel.add(m_kNegX);
        kVectorPanel.add(m_kNegY);
        kVectorPanel.add(m_kNegZ);

        m_kFAMinThreshold = new JTextField("0.0", 4);
        m_kFAMinThreshold.setActionCommand("FAMINChanged");
        m_kFAMinThreshold.addActionListener(this);
        m_kFAMaxThreshold = new JTextField("1.0", 4);
        m_kFAMaxThreshold.setActionCommand("FAMAXChanged");
        m_kFAMaxThreshold.addActionListener(this);
        m_kMaxAngle = new JTextField("45", 4);
        m_kMaxAngle.setActionCommand("MaxAngleChanged");
        m_kMaxAngle.addActionListener(this);
        JPanel kTrackPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        kTrackPanel.add(new JLabel( "FA Threshold Min (0.0-1.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add( m_kFAMinThreshold, gbc );
        gbc.gridx = 0;
        gbc.gridy++;
        kTrackPanel.add(new JLabel( "FA Threshold Max (0.0-1.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add( m_kFAMaxThreshold, gbc );
        gbc.gridx = 0;
        gbc.gridy++;
        kTrackPanel.add(new JLabel( "Maximum Angle (0.0-180.0):"), gbc);
        gbc.gridx = 2;
        kTrackPanel.add( m_kMaxAngle, gbc );
        

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        JPanel kTractOPtionsPanel = new JPanel();
        kTractOPtionsPanel.setLayout(new GridBagLayout());
        kTractOPtionsPanel.add(optionsPanel,gbc);
        gbc.gridy = 1;
        kTractOPtionsPanel.add(kVectorPanel,gbc);
        gbc.gridy = 2;
        kTractOPtionsPanel.add(kTrackPanel,gbc);
        kTractOPtionsPanel.setBorder(buildTitledBorder("Fiber Track Recontruction Options"));
        
        
        mainPanelGBC.gridx = 0;
        mainPanelGBC.gridy = 2;
        mainPanelGBC.insets = new Insets(10,0,0,0);
        mainPanelGBC.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(kTractOPtionsPanel,mainPanelGBC);
    }



    public void buildDTILoadPanel() {

        JPanel DTIloadPanel = new JPanel();
        DTIloadPanel.setLayout(new GridBagLayout());
        DTIloadPanel.setBorder(buildTitledBorder(""));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        openDTIimageButton = new JButton("Browse");
        openDTIimageButton.setToolTipText("Browse Diffusion Tensor file");
        openDTIimageButton.addActionListener(this);
        openDTIimageButton.setActionCommand("browseDTIFile");
        openDTIimageButton.setEnabled(false);

        textDTIimage = new JTextField();
        textDTIimage.setPreferredSize(new Dimension(275, 21));
        textDTIimage.setEditable(false);
        textDTIimage.setFont(MipavUtil.font12);
        textDTIimage.setBackground(Color.lightGray);

        dtiFileLabel = new JLabel("DTI File: ");
        dtiFileLabel.setEnabled(false);

        dtiRadio = new JRadioButton("Load DTI File");
        dtiRadio.setSelected(false);
        dtiRadio.addActionListener(this);
        dtiRadio.setActionCommand("dtiRadio");
        loadDataRadioGroup.add(dtiRadio);




        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.insets = new Insets(0,0,10,0);
        gbc.fill = GridBagConstraints.NONE;
        DTIloadPanel.add(dtiRadio, gbc);



        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        DTIloadPanel.add(dtiFileLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        DTIloadPanel.add(textDTIimage, gbc);
        gbc.gridx = 2;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.insets = new Insets(0,10,10,0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        DTIloadPanel.add(openDTIimageButton, gbc);


        mainPanelGBC.gridx = 0;
        mainPanelGBC.gridy = 1;
        mainPanel.add(DTIloadPanel,mainPanelGBC);


    }

    public void buildDWIListLoadPanel() {

        JPanel DWIlistPanel = new JPanel();
        DWIlistPanel.setLayout(new GridBagLayout());
        DWIlistPanel.setBorder(buildTitledBorder(""));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;


        listFileLabel = new JLabel("List File: ");

        openDTIimageListFile = new JButton("Browse");
        openDTIimageListFile.setToolTipText("Browse list file");
        openDTIimageListFile.addActionListener(this);
        openDTIimageListFile.setActionCommand("browseListFile");

        textDTIListFile = new JTextField();
        textDTIListFile.setPreferredSize(new Dimension(275, 21));
        textDTIListFile.setEditable(false);
        textDTIListFile.setFont(MipavUtil.font12);
        textDTIListFile.setBackground(Color.white);

        listRadio = new JRadioButton("Load List File");
        listRadio.setSelected(true);
        listRadio.addActionListener(this);
        listRadio.setActionCommand("listRadio");
        loadDataRadioGroup.add(listRadio);


        maskImageLabel = new JLabel("Mask Image: ");

        openDTIMaskImage = new JButton("Browse");
        openDTIMaskImage.setToolTipText("Browse mask image");
        openDTIMaskImage.addActionListener(this);
        openDTIMaskImage.setActionCommand("browseMaskImage");

        textDTIMaskImage = new JTextField();
        textDTIMaskImage.setPreferredSize(new Dimension(275, 21));
        textDTIMaskImage.setEditable(false);
        textDTIMaskImage.setFont(MipavUtil.font12);
        textDTIMaskImage.setBackground(Color.white);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.insets = new Insets(0,0,10,0);
        gbc.fill = GridBagConstraints.NONE;
        DWIlistPanel.add(listRadio, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        DWIlistPanel.add(listFileLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        DWIlistPanel.add(textDTIListFile, gbc);
        gbc.gridx = 2;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.insets = new Insets(0,10,10,0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        DWIlistPanel.add(openDTIimageListFile, gbc);


        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.insets = new Insets(0,0,10,0);
        gbc.fill = GridBagConstraints.NONE;
        DWIlistPanel.add(maskImageLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        DWIlistPanel.add(textDTIMaskImage, gbc);
        gbc.gridx = 2;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.insets = new Insets(0,10,10,0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        DWIlistPanel.add(openDTIMaskImage, gbc);

        mainPanelGBC.gridx = 0;
        mainPanelGBC.gridy = 0;
        mainPanel.add(DWIlistPanel,mainPanelGBC);


    }





    public void builDWILoadPanel() {
        openDWIimageBT = new JButton("Open DWI image");
        openDWIimageBT.setToolTipText("Open Diffusion Weighted Image");
        openDWIimageBT.addActionListener(this);
        openDWIimageBT.setActionCommand("openDWIimage");

    }

    /**
     * Launches the JFileChooser for the user to select the Diffusion Tensor
     * Image. Loads the tensor data.
     */
    public void loadDTIFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences
                .getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(
                ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Diffusion Tensor file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            FileIO fileIO = new FileIO();
            m_kDTIImage = fileIO.readImage(chooser.getSelectedFile().getName(),
                    chooser.getCurrentDirectory() + File.separator);
            m_kParentDir = chooser.getCurrentDirectory().getParent();

            if (m_kDTIImage.getNDims() != 4) {
                MipavUtil
                .displayError("Diffusion Tensor file does not have correct dimensions");
                if (m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                textDTIimage.setText("");
                m_kDTIImage = null;
                return;
            }
            if (m_kDTIImage.getExtents()[3] != 6) {
                MipavUtil
                .displayError("Diffusion Tensor does not have correct dimensions");
                if (m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                textDTIimage.setText("");
                m_kDTIImage = null;
                return;
            }
            textDTIimage.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser
                    .getCurrentDirectory().toString());
            computeButton.setEnabled(true);
        }
    }


    /**
     * Launches the JFileChooser for the user to select the Diffusion Weighted Images .path file. Loads the .path file.
     */
    public void loadDWIMaskFile()
    {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Mask Image");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
            FileIO fileIO = new FileIO();
            if( m_kDWIMaskImage != null )
            {
                m_kDWIMaskImage.disposeLocal();
                m_kDWIMaskImage = null;
            }
            m_kDWIMaskImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator);
            textDTIMaskImage.setText(chooser.getSelectedFile().getAbsolutePath());
        }
    }


    /**
     * Launches the JFileChooser for the user to select the Diffusion Weighted
     * Images .path file. Loads the .path file.
     */
    public void loadDWIListFile() {

        JFileChooser chooser = new JFileChooser(new File(Preferences
                .getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(
                ViewImageFileFilter.ALL));
        chooser.setDialogTitle("Choose Diffusion Weighted Images  .list file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            File kFile = new File(chooser.getSelectedFile().getAbsolutePath());
            if (!kFile.exists() || !kFile.canRead()) {
                return;
            }
            int iLength = (int) kFile.length();
            if (iLength <= 0) {
                return;
            }
            textDTIListFile.setText(chooser.getSelectedFile()
                    .getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser
                    .getCurrentDirectory().toString());

            m_kParentDir = chooser.getCurrentDirectory().toString();
            File kListFile = new File(chooser.getSelectedFile()
                    .getAbsolutePath());
            String pathFilename = null;
            String pathFileAbsPath = null;

            String bMatrixFilename = null;
            String bMatrixFileAbsPath = null;
            try {
                BufferedReader kReader = new BufferedReader(new FileReader(
                        kListFile));
                String lineString = null;
                while ((lineString = kReader.readLine()) != null) {
                    if (lineString.startsWith("<original_columns>")) {
                        String columnsStr = lineString.substring(
                                lineString.indexOf("<original_columns>") + 18,
                                lineString.indexOf("</original_columns>"))
                                .trim();
                        m_iDimX = Integer.parseInt(columnsStr);
                    } else if (lineString.startsWith("<original_rows>")) {
                        String rowsStr = lineString.substring(
                                lineString.indexOf("<original_rows>") + 15,
                                lineString.indexOf("</original_rows>")).trim();
                        m_iDimY = Integer.parseInt(rowsStr);
                    } else if (lineString.startsWith("<slice>")) {
                        String sliceStr = lineString.substring(
                                lineString.indexOf("<slice>") + 7,
                                lineString.indexOf("</slice>")).trim();
                        m_iSlices = Integer.parseInt(sliceStr);
                    } else if (lineString.startsWith("<nim>")) {
                        String nimStr = lineString.substring(
                                lineString.indexOf("<nim>") + 5,
                                lineString.indexOf("</nim>")).trim();
                        m_iWeights = Integer.parseInt(nimStr);
                    } else if (lineString.startsWith("<rawimageformat>")) {
                        m_kRawFormat = lineString.substring(
                                lineString.indexOf("<rawimageformat>") + 16,
                                lineString.indexOf("</rawimageformat>")).trim();
                    } else if (lineString
                            .startsWith("<raw_image_path_filename>")) {
                        pathFilename = lineString
                        .substring(
                                lineString
                                .indexOf("<raw_image_path_filename>") + 25,
                                lineString
                                .indexOf("</raw_image_path_filename>"))
                                .trim();
                        pathFileAbsPath = m_kParentDir + File.separator
                        + pathFilename;
                        // studyName = pathFilename.substring(0,
                        // pathFilename.indexOf(".path"));
                    } else if (lineString.startsWith("<bmatrixfile>")) {
                        bMatrixFilename = lineString.substring(
                                lineString.indexOf("<bmatrixfile>") + 13,
                                lineString.indexOf("</bmatrixfile>")).trim();
                        bMatrixFileAbsPath = m_kParentDir + File.separator
                        + bMatrixFilename;
                        // studyName = pathFilename.substring(0,
                        // pathFilename.indexOf(".path"));
                    } else if (lineString.startsWith("<x_field_of_view>")) {
                        String xFOVStr = lineString.substring(
                                lineString.indexOf("<x_field_of_view>") + 17,
                                lineString.indexOf("</x_field_of_view>"))
                                .trim();
                        float xFOV = Float.parseFloat(xFOVStr);
                        m_fResX = xFOV;
                        m_bUseXRes = true;
                    } else if (lineString.startsWith("<y_field_of_view>")) {
                        String yFOVStr = lineString.substring(
                                lineString.indexOf("<y_field_of_view>") + 17,
                                lineString.indexOf("</y_field_of_view>"))
                                .trim();
                        float yFOV = Float.parseFloat(yFOVStr);
                        m_fResY = yFOV;
                        m_bUseYRes = true;
                    } else if (lineString.startsWith("<slice_thickness>")) {
                        String zResStr = lineString.substring(
                                lineString.indexOf("<slice_thickness>") + 17,
                                lineString.indexOf("</slice_thickness>"))
                                .trim();
                        m_fResZ = Float.parseFloat(zResStr);
                        m_bUseZRes = true;
                    } else if (lineString.startsWith("<noise_mean_ori>")) {
                        String noiseStr = lineString.substring(
                                lineString.indexOf("<noise_mean_ori>") + 16,
                                lineString.indexOf("</noise_mean_ori>")).trim();
                        m_fMeanNoise = Float.parseFloat(noiseStr);
                    }
                }
                kReader.close();
                kReader = null;
            } catch (Exception e) {
                e.printStackTrace();
            }

            if (pathFilename != null) {
                loadPathFile(pathFileAbsPath, m_kParentDir);
            }
            if (bMatrixFileAbsPath != null) {
                loadBMatrixFile(bMatrixFileAbsPath);
            }
            m_fResX /= (float) m_iDimX;
            m_fResY /= (float) m_iDimY;
        }
    }

    /**
     * Loads the BMatrix file.
     * 
     * @param kFileName
     *            name of BMatrix file.
     */
    private void loadBMatrixFile(String kFileName) {
        File kFile = new File(kFileName);
        if (!kFile.exists() || !kFile.canRead()) {
            return;
        }
        int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return;
        }

        try {
            BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;

            m_kBMatrix = new GMatrixf(m_iWeights, 6 + 1);

            String[] kBMatrixString = new String[m_iWeights];
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
                if (!gotit) {
                    kBMatrixString[nb] = str;
                    m_aiMatrixEntries[iRow] = nb;
                    nb = nb + 1;
                }

                java.util.StringTokenizer st = new java.util.StringTokenizer(
                        str);
                for (int iCol = 0; iCol < 6; iCol++) {
                    float fValue = Float.valueOf(st.nextToken()).floatValue();
                    m_kBMatrix.Set(iRow, iCol, fValue);
                }
                m_kBMatrix.Set(iRow, 6, 1f);
            }
            in.close();

            m_iBOrig = nb;

        } catch (IOException e) {
        }
    }

    /**
     * Loads the .path file.
     * 
     * @param kFileName
     *            path file name.
     * @param kPathName
     *            parent directory.
     */
    public void loadPathFile(String kFileName, String kPathName) {
        File kFile = new File(kFileName);
        if (!kFile.exists() || !kFile.canRead()) {
            return;
        }
        int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return;
        }
        m_aakDWIList = new String[m_iSlices][m_iWeights];
        try {
            BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;
            for (int i = 0; i < m_iSlices; i++) {
                for (int j = 0; j < m_iWeights; j++) {
                    str = in.readLine();
                    m_aakDWIList[i][j] = new String(kPathName + File.separator
                            + str);
                }
            }
            in.close();
        } catch (IOException e) {
        }
    }

    /** Processes the Diffusion Tensor Image. Creates the eigen vector
     * and functional anisotropy images. Launched the
     * DialogDTIColorDisplay. */
    private void processDTI()
    {
        if ( m_kDTIImage == null )
        {
            MipavUtil.displayError("DTI file must be set to create eigen vector data.");
            return;
        }
        // set up parent directory before calling calcEigenVectorImage:
        m_kParentDir = m_kParentDir.concat( File.separator + "DTIOutput" + File.separator);

        File kDir = new File( m_kParentDir );
        if ( !kDir.exists() )
        {
            try {
                kDir.mkdir();
            } catch (SecurityException e) {}
        }

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        calcEigenVectorImage();

        createRGBImage();

        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

    /** Calls AlgorithmDWI2DTI to create the diffusion tensor image. */
    private void processDWI()
    {
        if ( m_kBMatrix == null )
        {
            MipavUtil.displayError("BMatrix file must be set to create tensor data.");
            return;
        }
        if ( m_aakDWIList == null )
        {
            MipavUtil.displayError("Path file must be set to create tensor data.");
            return;
        }

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        kAlgorithm = new AlgorithmDWI2DTI( m_kDWIMaskImage, false,
                m_iSlices, m_iDimX, m_iDimY,
                m_iBOrig, m_iWeights,
                m_fMeanNoise, m_aakDWIList,
                m_aiMatrixEntries, m_kBMatrix, m_kRawFormat);
        kAlgorithm.addListener(this);
        kAlgorithm.run();

    }



    /**
     * test code for creating rgb image based on eigvev and anisot
     */
    public void createRGBImage() {
        //gamma factor
        float gamma = 1.8f;

        //create the dest extents of the dec image...the 4th dim will only have 3 as the value
        int[] destExtents = new int[4];
        destExtents[0] = m_kEigenVectorImage.getExtents()[0];
        destExtents[1] = m_kEigenVectorImage.getExtents()[1];
        destExtents[2] = m_kEigenVectorImage.getExtents()[2];
        destExtents[3] = 3;

        ModelImage decImage = new ModelImage(ModelStorageBase.FLOAT, destExtents, 
                ModelImage.makeImageName(m_kEigenVectorImage.getImageName(), "_DEC" ));

        //buffer
        float[] buffer;

        //determine length of dec image
        int length = m_kEigenVectorImage.getExtents()[0] * m_kEigenVectorImage.getExtents()[1] * m_kEigenVectorImage.getExtents()[2] * 3;
        buffer = new float[length];

        //export eigvecSrcImage into buffer based on length
        try {
            m_kEigenVectorImage.exportData(0, length, buffer);
        }
        catch (IOException error) {
            System.out.println("IO exception");
            // return null;
        }

        //lets first do absolute value for each value in the buffer
        for(int i=0;i<buffer.length;i++) {
            buffer[i] = Math.abs(buffer[i]);
        }

        //import resultBuffer into decImage
        try {
            decImage.importData(0, buffer, true);
        }
        catch (IOException error) {
            System.out.println("IO exception");

            // return null;
        }

        //extract dec image into channel images
        destExtents = new int[3];
        destExtents[0] = decImage.getExtents()[0];
        destExtents[1] = decImage.getExtents()[1];
        destExtents[2] = decImage.getExtents()[2];
        ModelImage[] channelImages = new ModelImage[decImage.getExtents()[3]];
        for(int i=0;i<decImage.getExtents()[3];i++) {
            int num = i + 1;
            String resultString = ModelImage.makeImageName( decImage.getImageName(), "_Vol=" + num);
            channelImages[i] = new ModelImage(decImage.getType(), destExtents, resultString);
            AlgorithmSubset subsetAlgo = new AlgorithmSubset(decImage, channelImages[i], AlgorithmSubset.REMOVE_T, i);
            subsetAlgo.setRunningInSeparateThread(false);
            subsetAlgo.run();
        }

        decImage.disposeLocal();
        decImage = null;

        //set up result image
        resultImage = new ModelImage(ModelImage.ARGB_FLOAT, channelImages[0].getExtents(),
                ModelImage.makeImageName( m_kEigenVectorImage.getImageName(), "_ColorDisplay") );


        //cocatenate channel images into an RGB image
        AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(channelImages[0], channelImages[1], channelImages[2], resultImage, false, false);
        mathAlgo.setRunningInSeparateThread(false);
        mathAlgo.run();


        channelImages[0].disposeLocal();
        channelImages[0] = null;
        channelImages[1].disposeLocal();
        channelImages[1] = null;
        channelImages[2].disposeLocal();
        channelImages[2] = null;

        //copy core file info over
        FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[resultImage.getExtents()[2]];
        for (int i=0;i<fileInfoBases.length;i++) {
            fileInfoBases[i] = new FileInfoImageXML(resultImage.getImageName(), null, FileUtility.XML);	
            fileInfoBases[i].setEndianess(m_kEigenVectorImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(m_kEigenVectorImage.getFileInfo()[0].getUnitsOfMeasure());
            //fileInfoBases[i].setResolutions(m_kEigenVectorImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setResolutions(m_kDTIImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(resultImage.getExtents());
            fileInfoBases[i].setImageOrientation(m_kDTIImage.getFileInfo()[0].getImageOrientation());
            fileInfoBases[i].setAxisOrientation(m_kDTIImage.getFileInfo()[0].getAxisOrientation());
            fileInfoBases[i].setOrigin(m_kEigenVectorImage.getFileInfo()[0].getOrigin());
            fileInfoBases[i].setPixelPadValue(m_kEigenVectorImage.getFileInfo()[0].getPixelPadValue());
            fileInfoBases[i].setPhotometric(m_kEigenVectorImage.getFileInfo()[0].getPhotometric());
            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);
            fileInfoBases[i].setFileDirectory(m_kEigenVectorImage.getFileInfo()[0].getFileDirectory());
        }

        resultImage.setFileInfo(fileInfoBases);


        //now we need to weight the result image by anisotopy

        float[] rgbBuffer;
        //determine length of dec image
        int rgbBuffLength = resultImage.getExtents()[0] * resultImage.getExtents()[1] * resultImage.getExtents()[2] * 4;
        rgbBuffer = new float[rgbBuffLength];

        //export eigvecSrcImage into buffer based on length
        try {
            resultImage.exportData(0, rgbBuffLength, rgbBuffer);
        }
        catch (IOException error) {
            System.out.println("IO exception");
            // return null;
        }


        float[] anisotropyBuffer;
        int anisLength = m_kAnisotropyImage.getExtents()[0] * m_kAnisotropyImage.getExtents()[1] * m_kAnisotropyImage.getExtents()[2];
        anisotropyBuffer = new float[anisLength];
        try {
            m_kAnisotropyImage.exportData(0, anisLength, anisotropyBuffer);
        }
        catch (IOException error) {
            System.out.println("IO exception");
            // return null;
        }

        //take r,g,and b and weight by anisotropy and gamma...and rescale to 0-255
        for(int i=0,j=0;i<rgbBuffer.length;i=i+4,j++) {
            rgbBuffer[i+1] = rgbBuffer[i+1] * anisotropyBuffer[j];
            rgbBuffer[i+1] = (float)Math.pow(rgbBuffer[i+1],(1/gamma));
            rgbBuffer[i+1] = rgbBuffer[i+1] * 255;

            rgbBuffer[i+2] = rgbBuffer[i+2] * anisotropyBuffer[j];
            rgbBuffer[i+2] = (float)Math.pow(rgbBuffer[i+2],(1/gamma));
            rgbBuffer[i+2] = rgbBuffer[i+2] * 255;

            rgbBuffer[i+3] = rgbBuffer[i+3] * anisotropyBuffer[j];
            rgbBuffer[i+3] = (float)Math.pow(rgbBuffer[i+3],(1/gamma));
            rgbBuffer[i+3] = rgbBuffer[i+3] * 255;

        }


        try {
            resultImage.importData(0, rgbBuffer, true);
        }
        catch (IOException error) {
            System.out.println("IO exception");

            // return null;
        }

        resultImage.calcMinMax();
        // new ViewJFrameImage(m_kEigenVectorImage);
        // new ViewJFrameImage(m_kAnisotropyImage);
        // new ViewJFrameImage(resultImage);


        // return resultImage;
    }



    /** Calls AlgorithmDTI2EGFA to create eigen vector and functional anisotropy images. */
    private void calcEigenVectorImage()
    {
        int[] extents = m_kDTIImage.getExtents();
        float[] res = m_kDTIImage.getFileInfo(0).getResolutions();
        float[] saveRes = new float[]{res[0], res[1], res[2], res[3]};
        if ( m_bUseXRes )
        {
            saveRes[0] = m_fResX;
        }
        if ( m_bUseYRes )
        {
            saveRes[1] = m_fResY;
        }
        if ( m_bUseZRes )
        {
            saveRes[2] = m_fResZ;
        }

        float[] newRes = new float[extents.length];
        int[] volExtents = new int[extents.length];
        boolean originalVolPowerOfTwo = true;
        int volSize = 1;
        for (int i = 0; i < extents.length; i++) {
            volExtents[i] = JDialogDirectResample.dimPowerOfTwo(extents[i]);
            volSize *= volExtents[i];

            if ((i < 3) && volExtents[i] != extents[i]) {
                originalVolPowerOfTwo = false;
            }
            newRes[i] = (res[i] * (extents[i])) / (volExtents[i]);
            saveRes[i] = (saveRes[i] * (extents[i])) / (volExtents[i]);
        }

        if ( !originalVolPowerOfTwo )
        {
            AlgorithmTransform transformFunct = new AlgorithmTransform(m_kDTIImage, new TransMatrix(4),
                    AlgorithmTransform.TRILINEAR,
                    newRes[0], newRes[1], newRes[2],
                    volExtents[0], volExtents[1], volExtents[2],
                    false, true, false);
            transformFunct.setRunningInSeparateThread(false);
            transformFunct.run();

            if (transformFunct.isCompleted() == false) {
                transformFunct.finalize();
                transformFunct = null;
            }

            ModelImage kDTIImageScaled = transformFunct.getTransformedImage();
            kDTIImageScaled.calcMinMax();

            if (transformFunct != null) {
                transformFunct.disposeLocal();
            }
            transformFunct = null;

            m_kDTIImage.disposeLocal();
            m_kDTIImage = null;
            m_kDTIImage = kDTIImageScaled;

            res = m_kDTIImage.getFileInfo(0).getResolutions();
        }

        AlgorithmDTI2EGFA kAlgorithm = new AlgorithmDTI2EGFA(m_kDTIImage);
        kAlgorithm.run();
        m_kEigenVectorImage = kAlgorithm.getEigenImage();
        m_kAnisotropyImage = kAlgorithm.getFAImage();
        kAlgorithm.getTraceImage().saveImage( m_kParentDir, "TraceImage.xml", FileUtility.XML, false );
        kAlgorithm.getRAImage().saveImage( m_kParentDir, "RAImage.xml", FileUtility.XML, false );
        kAlgorithm.getVRImage().saveImage( m_kParentDir, "VolumeRatioImage.xml", FileUtility.XML, false );
        kAlgorithm.getADCImage().saveImage( m_kParentDir, "ADCImage.xml", FileUtility.XML, false );
        m_kEigenValueImage = kAlgorithm.getEigenValueImage();
        m_kEigenValueImage.saveImage( m_kParentDir, "EigenValueImage.xml", FileUtility.XML, false );
        m_kDTIImage.saveImage(m_kParentDir, "DTIImage.xml", FileUtility.XML, true);
        m_kEigenVectorImage.saveImage(m_kParentDir, "EigenVectorImage.xml", FileUtility.XML, true);
        m_kAnisotropyImage.saveImage(m_kParentDir, "AnisotropyImage.xml", FileUtility.XML, true);
        kAlgorithm.disposeLocal();
        kAlgorithm = null;


        // The resolutions should be reset after the FiberTracts are calculated (See JDialogDTIInput.java)
        if ( m_bUseXRes || m_bUseYRes || m_bUseZRes )
        {
            for ( int i = 0; i < m_kDTIImage.getFileInfo().length; i++ )
            {
                m_kDTIImage.getFileInfo(i).setResolutions(saveRes);
                m_kDTIImage.getFileInfo(i).setSliceThickness(saveRes[2]);
            }
        }
    }    


}