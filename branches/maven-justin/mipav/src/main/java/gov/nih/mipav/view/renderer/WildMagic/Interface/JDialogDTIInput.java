package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTI2EGFA;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDTITract;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.MipavMath;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.dialogs.DialogDTIColorDisplay;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import WildMagic.LibFoundation.Mathematics.GMatrixd;


/**
 * Dialog for specifying Diffusion Tensor Images.
 * 
 * Diffusion Tensor images may be loaded in one of the following ways, specified by the dialog types: DWI, DTI, EG_FA,
 * or TRACTS.
 * 
 * DWI: The DWI dialog loads the raw Diffusion Weighted Images. Calculates the tensor image from the set of weighed
 * images. The dialog requires the user to specify the raw image dimensions, and the number of weighted image sets. The
 * user must also specify the mean noise value, the name of the B-Matrix file, and the .path file which specifies the
 * locations of the weighted images in the directory structure. Once the tensor image is calculated, the eigen-vector
 * and functional anisotropy images are calculated and passed to the DialogDTIColorDisplay dialog. The user also has the
 * option of creating the fiber-tract bundles from the tensor image.
 * 
 * DTI: The DTI dialog loads a previously-calculated Diffusion Tensor image and calculates the eigen-vector image and
 * the functional anisotropy image. The DialogDTIColorDisplay dialog is then launched. The user specifies the tensor
 * image and has the option of creating the fiber-tract bundles from the tensor image.
 * 
 * EG_FA: The EG_FA dialog loads previously calculated eigen-vector and functional anisotropy images and launches the
 * DialogDTIColorDisplay dialog
 * 
 * TRACTS: The tracts dialog is launched from the ViewJFrameViewWM class. It enables the user to add the fiber-tract
 * visualization to the GPU Volume display. The user can limit the number of tracts, the minimum and maximum tract
 * lengths, or specify that tracts that pass through a VOI are loaded.
 */
public class JDialogDTIInput extends JInterfaceBase implements ActionListener, ListSelectionListener,
        AlgorithmInterface, ChangeListener {

    private static final long serialVersionUID = 9207590841799033846L;

    /** Types of dialogs: */
    /** Diffusion Weighted Images dialog: */
    public static final int DWI = 0;

    /** Diffusion Tensor Image dialog: */
    public static final int DTI = 1;

    /** EigenVector and Functional Anisotropy dialog: */
    public static final int EG_FA = 2;

    /** Eigenvector image * */
    private ModelImage m_kEigenVectorImage;

    /** EigenValue image * */
    private ModelImage m_kEigenValueImage;

    /** Anisotropy image * */
    private ModelImage m_kAnisotropyImage;

    /** Diffusion Tensor image. */
    private ModelImage m_kDTIImage = null;

    /** Mask image for calculating the DTI image. */
    private ModelImage m_kDWIMaskImage = null;

    /** LUT of input image * */
    private ModelLUT m_kLUTa;

    /** EigenVector file input path name text box. */
    private JTextField m_kEigenVectorPath;

    /** Anisotropy file input path name text box. */
    private JTextField m_kAnisotropyPath;

    /** Diffusion Tensor file input path name text box. */
    private JTextField m_kDTIPath;

    /** Diffusion Weighted Images .list file input path name text box. */
    private JTextField m_kDWIPath;

    /** Diffusion Weighted Images Mask file input path name text box. */
    private JTextField m_kDWIMaskPath;

    /** General matrix storing BMatrix values. */
    private GMatrixd m_kBMatrix = null;

    /** List of file names for the Diffusion Weighted Images, from the .path file. */
    private String[][] m_aakDWIList = null;

    /** Number of slices in the Diffusion Weighted Images series. */
    private int m_iSlices = 0;

    /** Number of weights in the Diffusion Weighted Images series. */
    private int m_iWeights = 0;

    /** X-dimensions for Diffusion Weighted Images. */
    private int m_iDimX = 0;

    /** Y-dimensions for Diffusion Weighted Images. */
    private int m_iDimY = 0;

    /** Dialog type. */
    private final int m_iType;

    /** Checkbox for tract reconstruction. */
    private JCheckBox m_kReconstructTracts;

    private JCheckBox m_kOpenB0 = null;

    /** Number of different BMatrix rows: */
    private int m_iBOrig = 0;

    /** keeps track of unique entries in the BMatrix */
    private int[] m_aiMatrixEntries;

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

    /** parent directory for the DTI output images. */
    private String m_kParentDir = null;
    

    /**
     * Create a new JDialogDTIInput of one of the four types:
     * 
     * @param iType, type of Diffusion Tensor Input dialog to create.
     */
    public JDialogDTIInput(final int iType) {
        super();
        init(iType);
        m_iType = iType;
    }

    /**
     * ActionListener event.
     * 
     * @param kAction, ActionEvent
     */
    public void actionPerformed(final ActionEvent kAction) {
        final Object kSource = kAction.getSource();
        final String kCommand = kAction.getActionCommand();

        if (kSource == cancelButton) {
            disposeLocal();
        }

        if (kCommand.equalsIgnoreCase("eigenvectorBrowse")) {
            loadEigenVectorFile();
        } else if (kCommand.equalsIgnoreCase("anisotropyBrowse")) {
            loadAnisotropyFile();
        } else if (kCommand.equalsIgnoreCase("DTIBrowse")) {
            loadDTIFile();
        } else if (kCommand.equalsIgnoreCase("DWIListBrowse")) {
            loadDWIListFile();
        } else if (kCommand.equalsIgnoreCase("DWIMaskBrowse")) {
            loadDWIMaskFile();
        } else if (kCommand.equalsIgnoreCase("ok")) {
            if (m_iType == JDialogDTIInput.DWI) {
                if (m_kBMatrix == null || m_aakDWIList == null) {
                    MipavUtil.displayError("Both BMatrix and .path files are needed.");
                    return;
                }
                processDWI();
            } else if (m_iType == JDialogDTIInput.DTI) {
                if (m_kDTIImage == null) {
                    MipavUtil.displayError("Diffusion Tensor Image needed");
                    return;
                }
                processDTI();
            } else if (m_iType == JDialogDTIInput.EG_FA) {
                if (m_kEigenVectorImage == null || m_kAnisotropyImage == null) {
                    MipavUtil.displayError("Both eigenvector and anisotropy files are needed");
                    return;
                }
                new DialogDTIColorDisplay(m_kEigenVectorImage, m_kAnisotropyImage, m_kLUTa, false);
                disposeLocal();
            } 
        }
    }

    /**
     * Called when AlgorithmDWI2DTI is done creating the DTI image.
     * 
     * @param kAlgorithm, algorithm that is finished.
     */
    public void algorithmPerformed(final AlgorithmBase kAlgorithm) {
        if (kAlgorithm instanceof AlgorithmDWI2DTI) {
            if (kAlgorithm.isCompleted()) {
                m_kDTIImage = ((AlgorithmDWI2DTI) kAlgorithm).getDTI();
                ((AlgorithmDWI2DTI) kAlgorithm).disposeLocal();
                setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                processDTI();
            } else if (m_kOpenB0.isSelected()) {
                disposeLocal();
            }
        }
    }

    /** Clean up local memory. */
    public void disposeLocal() {
        m_kEigenVectorImage = null;
        m_kAnisotropyImage = null;
        m_kLUTa = null;

        if (m_kDTIImage != null) {
            m_kDTIImage.disposeLocal();
            m_kDTIImage = null;
        }
        if (m_kDWIMaskImage != null) {
            m_kDWIMaskImage.disposeLocal();
            m_kDWIMaskImage = null;
        }
        m_kBMatrix = null;
        m_aakDWIList = null;
        m_aiMatrixEntries = null;
        m_kRawFormat = null;
        m_kParentDir = null;
        setVisible(false);
    }


    @Override
	public void stateChanged(ChangeEvent arg0) {}

    @Override
	public void valueChanged(ListSelectionEvent arg0) {}

    /** Calls AlgorithmDTI2EGFA to create eigen vector and functional anisotropy images. */
    private void calcEigenVectorImage() {
        AlgorithmDTI2EGFA kAlgorithm = new AlgorithmDTI2EGFA(m_kDTIImage);
        kAlgorithm.run();
        m_kEigenVectorImage = kAlgorithm.getEigenVectorImage();
        m_kAnisotropyImage = kAlgorithm.getFAImage();
        kAlgorithm.getTraceImage().saveImage(m_kParentDir, "TraceImage.xml", FileUtility.XML, false);
        kAlgorithm.getRAImage().saveImage(m_kParentDir, "RAImage.xml", FileUtility.XML, false);
        kAlgorithm.getVRImage().saveImage(m_kParentDir, "VRImage.xml", FileUtility.XML, false);
        kAlgorithm.getADCImage().saveImage(m_kParentDir, "ADCImage.xml", FileUtility.XML, false);
        m_kEigenValueImage = kAlgorithm.getEigenValueImage();
        m_kEigenValueImage.saveImage(m_kParentDir, "EVImage.xml", FileUtility.XML, false);
        kAlgorithm.disposeLocal();
        kAlgorithm = null;

        if ( (m_kReconstructTracts != null) && m_kReconstructTracts.isSelected()) {
            AlgorithmDTITract kTractAlgorithm = new AlgorithmDTITract(m_kDTIImage, m_kEigenVectorImage,
                    m_kEigenValueImage, m_kParentDir + "DTIImage.xml_tract", false, false, false);
            kTractAlgorithm.run();
            kTractAlgorithm.disposeLocal();
            kTractAlgorithm = null;
        }
    }

    /**
     * Creates the user-interface for the Diffusion Tensor Image dialog.
     * 
     * @return JPanel containing the user-interface for the Diffusion Tensor Image dialog.
     */
    private JPanel createDTIPanel() {
        final GridBagLayout kGBL = new GridBagLayout();
        final GridBagConstraints gbc = new GridBagConstraints();
        final JPanel kDTIPanel = new JPanel(kGBL);
        final JPanel kDTIFilesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 5, 5);
        final JLabel kDTILabel = new JLabel("  Diffusion Tensor Image: ");
        kDTIFilesPanel.add(kDTILabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kDTIPath = new JTextField(35);
        m_kDTIPath.setEditable(false);
        m_kDTIPath.setBackground(Color.white);
        kDTIFilesPanel.add(m_kDTIPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        final JButton kDTIBrowseButton = new JButton("Browse");
        kDTIBrowseButton.addActionListener(this);
        kDTIBrowseButton.setActionCommand("DTIBrowse");
        kDTIFilesPanel.add(kDTIBrowseButton, gbc);

        m_kReconstructTracts = new JCheckBox("Tract Reconstruction");
        m_kReconstructTracts.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy++;
        kDTIFilesPanel.add(m_kReconstructTracts, gbc);

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        kDTIPanel.add(kDTIFilesPanel, gbc);
        return kDTIPanel;
    }

    /**
     * Creates the user-interface for the Diffusion Tensor Image dialog.
     * 
     * @return JPanel containing the user-interface for the Diffusion Tensor Image dialog.
     */
    private JPanel createDWIPanel() {
        final GridBagLayout kGBL = new GridBagLayout();
        final GridBagConstraints gbc = new GridBagConstraints();
        final JPanel kDWIPanel = new JPanel(kGBL);
        final JPanel kDWIFilesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 5, 5);
        final JLabel kDWILabel = new JLabel("  Diffusion Weighted Image (.list): ");
        kDWIFilesPanel.add(kDWILabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kDWIPath = new JTextField(35);
        m_kDWIPath.setEditable(false);
        m_kDWIPath.setBackground(Color.white);
        kDWIFilesPanel.add(m_kDWIPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        final JButton kDWIBrowseButton = new JButton("Browse");
        kDWIBrowseButton.addActionListener(this);
        kDWIBrowseButton.setActionCommand("DWIListBrowse");
        kDWIFilesPanel.add(kDWIBrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        final JLabel kDWIMaskLabel = new JLabel("  Mask Image: ");
        kDWIFilesPanel.add(kDWIMaskLabel, gbc);
        gbc.gridx = 1;
        m_kDWIMaskPath = new JTextField(35);
        m_kDWIMaskPath.setEditable(false);
        m_kDWIMaskPath.setBackground(Color.white);
        kDWIFilesPanel.add(m_kDWIMaskPath, gbc);
        gbc.gridx = 2;
        final JButton kDWIMaskBrowseButton = new JButton("Browse");
        kDWIMaskBrowseButton.addActionListener(this);
        kDWIMaskBrowseButton.setActionCommand("DWIMaskBrowse");
        kDWIFilesPanel.add(kDWIMaskBrowseButton, gbc);

        m_kOpenB0 = new JCheckBox("Open B0 image only");
        m_kOpenB0.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy++;
        kDWIFilesPanel.add(m_kOpenB0, gbc);

        m_kReconstructTracts = new JCheckBox("Tract Reconstruction");
        m_kReconstructTracts.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy++;
        kDWIFilesPanel.add(m_kReconstructTracts, gbc);

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        kDWIPanel.add(kDWIFilesPanel, gbc);
        return kDWIPanel;
    }

    /**
     * Creates the user-interface for the EigenVector FA dialog.
     * 
     * @return JPanel containing the user-interface for the EigenVector FA dialog.
     */
    private JPanel createEigenPanel() {
        final GridBagLayout kGBL = new GridBagLayout();
        final GridBagConstraints gbc = new GridBagConstraints();
        final JPanel kEigenPanel = new JPanel(kGBL);
        final JPanel filesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 5, 5);
        final JLabel eigenvectorLabel = new JLabel(" eigenvector file: ");
        filesPanel.add(eigenvectorLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kEigenVectorPath = new JTextField(35);
        m_kEigenVectorPath.setEditable(false);
        m_kEigenVectorPath.setBackground(Color.white);
        filesPanel.add(m_kEigenVectorPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        final JButton eigenvectorBrowseButton = new JButton("Browse");
        eigenvectorBrowseButton.addActionListener(this);
        eigenvectorBrowseButton.setActionCommand("eigenvectorBrowse");
        filesPanel.add(eigenvectorBrowseButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        final JLabel anisotropyLabel = new JLabel(" anisotropy file: ");
        filesPanel.add(anisotropyLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        m_kAnisotropyPath = new JTextField(35);
        m_kAnisotropyPath.setEditable(false);
        m_kAnisotropyPath.setBackground(Color.white);
        filesPanel.add(m_kAnisotropyPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 1;
        final JButton anisotropyBrowseButton = new JButton("Browse");
        anisotropyBrowseButton.addActionListener(this);
        anisotropyBrowseButton.setActionCommand("anisotropyBrowse");
        filesPanel.add(anisotropyBrowseButton, gbc);

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        kEigenPanel.add(filesPanel, gbc);
        return kEigenPanel;
    }

    /**
     * The JDialogDTIInput interface.
     * 
     * @param iType, type of Diffusion Tensor Input dialog to create.
     */
    private void init(final int iType) {
        setForeground(Color.black);
        setTitle("Select Diffusion Tensor Input");

        final JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        final GridBagConstraints gbcMain = new GridBagConstraints();
        gbcMain.gridx = 0;
        gbcMain.gridy = 0;
        gbcMain.fill = GridBagConstraints.BOTH;
        gbcMain.weightx = 1;
        gbcMain.weighty = 1;

        mainPanel = new JPanel(new GridBagLayout());
        if (iType == JDialogDTIInput.DWI) {
            mainPanel.add(createDWIPanel(), gbcMain);
        } else if (iType == JDialogDTIInput.DTI) {
            mainPanel.add(createDTIPanel(), gbcMain);
        } else if (iType == JDialogDTIInput.EG_FA) {
            mainPanel.add(createEigenPanel(), gbcMain);
        } 

        gbcMain.gridy++;
        gbcMain.weightx = 0;
        gbcMain.weighty = 0;
        mainPanel.add(buttonPanel, gbcMain);
        getContentPane().add(mainPanel);
        pack();
        setVisible(true);
    }

    /**
     * Launches the JFileChooser for the user to select the functional anisotropy file. Loads the anisotropy data.
     */
    private void loadAnisotropyFile() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose anisotropy file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();
            if (m_kAnisotropyImage != null) {
                m_kAnisotropyImage.disposeLocal();
                m_kAnisotropyImage = null;
            }
            m_kAnisotropyImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);
            if (m_kAnisotropyImage.getNDims() > 3) {
                MipavUtil.displayError("anisotropy file does not have correct dimensions");
                if (m_kAnisotropyImage != null) {
                    m_kAnisotropyImage.disposeLocal();
                }
                m_kAnisotropyPath.setText("");
                m_kAnisotropyImage = null;
                return;
            }
            m_kAnisotropyPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }

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
            m_kParentDir = chooser.getCurrentDirectory().getParent();
            if (m_kDTIImage.getNDims() != 4) {
                MipavUtil.displayError("Diffusion Tensor file does not have correct dimensions");
                if (m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                m_kDTIPath.setText("");
                m_kDTIImage = null;
                return;
            }
            if (m_kDTIImage.getExtents()[3] != 6) {
                MipavUtil.displayError("Diffusion Tensor does not have correct dimensions");
                if (m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                m_kDTIPath.setText("");
                m_kDTIImage = null;
                return;
            }
            m_kDTIPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }

    /**
     * Launches the JFileChooser for the user to select the Diffusion Weighted Images .path file. Loads the .path file.
     */
    private void loadDWIListFile() {

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
            m_kDWIPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());

            m_kParentDir = chooser.getCurrentDirectory().toString();
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
                        pathFileAbsPath = m_kParentDir + File.separator + pathFilename;
                        // studyName = pathFilename.substring(0, pathFilename.indexOf(".path"));
                    } else if (lineString.startsWith("<bmatrixfile>")) {
                        bMatrixFilename = lineString.substring(lineString.indexOf("<bmatrixfile>") + 13,
                                lineString.indexOf("</bmatrixfile>")).trim();
                        bMatrixFileAbsPath = m_kParentDir + File.separator + bMatrixFilename;
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

            if (pathFilename != null) {
                loadPathFile(pathFileAbsPath, m_kParentDir);
            }
            if (bMatrixFileAbsPath != null) {
                loadBMatrixFile(bMatrixFileAbsPath);
            }
            m_fResX /= m_iDimX;
            m_fResY /= m_iDimY;
        }
    }

    /**
     * Launches the JFileChooser for the user to select the Diffusion Weighted Images .path file. Loads the .path file.
     */
    private void loadDWIMaskFile() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Mask Image");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();
            if (m_kDWIMaskImage != null) {
                m_kDWIMaskImage.disposeLocal();
                m_kDWIMaskImage = null;
            }
            m_kDWIMaskImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);
            m_kDWIMaskPath.setText(chooser.getSelectedFile().getAbsolutePath());
        }
    }

    /**
     * Launches the JFileChooser for the user to select the Eigen Vector file.. Loads the eigen vector data.
     */
    private void loadEigenVectorFile() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose eigenvector file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();
            if (m_kEigenVectorImage != null) {
                m_kEigenVectorImage.disposeLocal();
                m_kEigenVectorImage = null;
            }
            m_kEigenVectorImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);
            if (m_kEigenVectorImage.getNDims() != 4) {
                MipavUtil.displayError("Eigenvector file does not have correct dimensions");
                if (m_kEigenVectorImage != null) {
                    m_kEigenVectorImage.disposeLocal();
                }
                m_kEigenVectorPath.setText("");
                m_kEigenVectorImage = null;
                return;
            }
            if (m_kEigenVectorImage.getExtents()[3] != 9) {
                MipavUtil.displayError("Eigenvector file does not have correct dimensions");
                if (m_kEigenVectorImage != null) {
                    m_kEigenVectorImage.disposeLocal();
                }
                m_kEigenVectorPath.setText("");
                m_kEigenVectorImage = null;
                return;
            }
            m_kEigenVectorPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            int[] dimExtentsLUT;
            dimExtentsLUT = new int[2];
            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;
            m_kLUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
            m_kLUTa.resetTransferLine(0.0f, (int) Math.round(m_kEigenVectorImage.getMin()), 255.0f, (int) Math
                    .round(m_kEigenVectorImage.getMax()));
            int[] extents;
            extents = new int[4];
            extents[0] = Math.round(m_kEigenVectorImage.getExtents()[0]);
            extents[1] = Math.round(m_kEigenVectorImage.getExtents()[1]);
            extents[2] = Math.round(m_kEigenVectorImage.getExtents()[2]);
            extents[3] = Math.round(m_kEigenVectorImage.getExtents()[3]);
        }
    }

    /**
     * Loads the .path file.
     * 
     * @param kFileName path file name.
     * @param kPathName, parent directory.
     */
   private void loadPathFile(final String kFileName, final String kPathName) {
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

	/**
     * Processes the Diffusion Tensor Image. Creates the eigen vector and functional anisotropy images. Launched the
     * DialogDTIColorDisplay.
     */
    private void processDTI() {
        if (m_kDTIImage == null) {
            MipavUtil.displayError("DTI file must be set to create eigen vector data.");
            return;
        }
        // set up parent directory before calling calcEigenVectorImage:
        m_kParentDir = m_kParentDir.concat(File.separator + "DTIOutput" + File.separator);
        final File kDir = new File(m_kParentDir);
        if ( !kDir.exists()) {
            try {
                kDir.mkdir();
            } catch (final SecurityException e) {}
        }

        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        calcEigenVectorImage();
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

        if (m_iType == JDialogDTIInput.DWI) {
            m_kDTIImage.saveImage(m_kParentDir, "DTIImage.xml", FileUtility.XML, false);
        }
        m_kEigenVectorImage.saveImage(m_kParentDir, "EigenVectorImage.xml", FileUtility.XML, false);
        m_kAnisotropyImage.saveImage(m_kParentDir, "AnisotropyImage.xml", FileUtility.XML, false);

        final DialogDTIColorDisplay kColorDisplay = new DialogDTIColorDisplay(m_kEigenVectorImage, m_kAnisotropyImage,
                m_kLUTa, false);

        kColorDisplay.setScreenImageResolutions(m_kDTIImage.getFileInfo(0).getResolutions(), m_kDTIImage.getFileInfo(0)
                .getSliceThickness());

        disposeLocal();
    }

	/** Calls AlgorithmDWI2DTI to create the diffusion tensor image. */
    private void processDWI() {
        if (m_kBMatrix == null) {
            MipavUtil.displayError("BMatrix file must be set to create tensor data.");
            return;
        }
        if (m_aakDWIList == null) {
            MipavUtil.displayError("Path file must be set to create tensor data.");
            return;
        }

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        final AlgorithmDWI2DTI kAlgorithm = new AlgorithmDWI2DTI(m_kDWIMaskImage, m_kOpenB0.isSelected(), m_iSlices,
                m_iDimX, m_iDimY, m_iBOrig, m_iWeights, m_fMeanNoise, m_aakDWIList, m_aiMatrixEntries, m_kBMatrix,
                m_kRawFormat);
        kAlgorithm.addListener(this);
        kAlgorithm.run();

    }

};
