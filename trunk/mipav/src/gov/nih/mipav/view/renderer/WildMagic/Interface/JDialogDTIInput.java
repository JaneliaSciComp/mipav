package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.DialogDTIColorDisplay;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Vector;

import javax.swing.*;
import javax.swing.event.*;

import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.SceneGraph.*;


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

    /** Fiber Bundle tracts dialog: */
    public static final int TRACTS_DIALOG = 3;

    /** Fiber Bundle tracts dialog: */
    public static final int TRACTS_PANEL = 4;

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

    /** Fiber bundle tract file input path name text box. */
    private JTextField m_kTractPath;

    /** General matrix storing BMatrix values. */
    private GMatrixf m_kBMatrix = null;

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

    /** Image displayed in the GPUVolumeRender and SurfaceRender */
    private ModelImage m_kImage;

    /** Checkbox for tract reconstruction. */
    private JCheckBox m_kReconstructTracts;

    private JCheckBox m_kOpenB0 = null;

    /** For TRACTS dialog: number of tracts to display. */
    private JTextField m_kTractsLimit;

    /** For TRACTS dialog: minimum tract length to display. */
    private JTextField m_kTractsMin;

    /** For TRACTS dialog: maximum tract length to display. */
    private JTextField m_kTractsMax;

    /** Tract input file. */
    private File m_kTractFile = null;

    /** When selected, only tracts that intersect the VOI are displayed. */
    private JCheckBox m_kUseVOICheck = null;

    /** Number of different BMatrix rows: */
    private int m_iBOrig = 0;

    /** keeps track of unique entries in the BMatrix */
    private int[] m_aiMatrixEntries;

    /** The list box in the dialog for fiber bundle tracts. */
    private JList m_kTractList;

    /** Color button for changing the color of the fiber bundles. */
    private JButton m_kColorButton;

    /** Color button detault color: */
    private Color m_kColorButtonDefault;

    /** Checkbox for turning on/off volume color for the polylines. */
    private JCheckBox m_kUseVolumeColor;

    /** Checkbox for switching between polylines and ellipsoids. */
    private JCheckBox m_kUseEllipsoids;

    /** Checkbox for displaying all tensors as ellipsoids. */
    private JCheckBox m_kAllEllipsoids;

    /** Keeps track of the groups of polylines loaded. */
    private Vector<Integer> m_kBundleList = new Vector<Integer>();

    /** Number of currently loaded fiber bundle groups. */
    private int m_iBundleCount = 0;

    /** DTI panel parent of load tract dialog */
    private JDialogDTIInput m_kParentDialog;

    /** User-control over the number of ellipsoids displayed in GPUVolumeRender */
    private JSlider m_kDisplaySlider;

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
    
    private boolean m_bDTIImageSet = false;

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
     * Create a new JDialogDTIInput of one of the four types:
     * 
     * @param iType, type of Diffusion Tensor Input dialog to create.
     * @param kDisplay, reference to the GPUVolumeRender display for loading fiber bundle tracts.
     * @param kDialog, JPanelSurface display displaying fiber bundle line arrays.
     * @param kImage, ModelImage displayed in GPUVolumeRender
     */
    public JDialogDTIInput(final int iType, final VolumeTriPlanarInterface kVolumeViewer, final ModelImage kImage) {
        super(kVolumeViewer);
        init(iType);
        m_iType = iType;
        m_kImage = kImage;
    }

    /**
     * Create a new JDialogDTIInput of one of the four types:
     * 
     * @param iType, type of Diffusion Tensor Input dialog to create. loading fiber bundle tracts.
     * @param kParent, parent DTI panel
     */
    public JDialogDTIInput(final int iType, final JDialogDTIInput kParent) {
        super();
        m_iType = iType;
        m_kParentDialog = kParent;
        init(iType);
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
        m_kImage = null;
        m_aiMatrixEntries = null;
        m_kBundleList = null;
        m_kParentDialog = null;
        m_kRawFormat = null;
        m_kParentDir = null;
        setVisible(false);
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
        } else if (iType == JDialogDTIInput.TRACTS_DIALOG) {
            mainPanel.add(createTractDialog(), gbcMain);
        } else if (iType == JDialogDTIInput.TRACTS_PANEL) {
            mainPanel.add(createTractPanel(), gbcMain);
        }

        if (iType != JDialogDTIInput.TRACTS_PANEL) {
            gbcMain.gridy++;
            gbcMain.weightx = 0;
            gbcMain.weighty = 0;
            mainPanel.add(buttonPanel, gbcMain);
            getContentPane().add(mainPanel);
            pack();
            setVisible(true);
        }
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
        } else if (kCommand.equalsIgnoreCase("tractBrowse")) {
            loadTractFile();
        } else if (kCommand.equals("ChangeColor")) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick fiber bundle color", new OkColorListener(
                    m_kColorButton), new CancelListener());
        } else if (kCommand.equals("VolumeColor")) {
            if (m_kUseVolumeColor.isSelected()) {
                setButtonColor(null, null);
            } else {
                setButtonColor(m_kColorButton, m_kColorButton.getBackground());
            }
        } else if (kCommand.equals("UseEllipsoids")) {
            if (rayBasedRenderWM != null) {
                rayBasedRenderWM.setDisplayEllipsoids(m_kUseEllipsoids.isSelected());
            }
        } else if (kCommand.equals("AllEllipsoids")) {
            if (rayBasedRenderWM != null) {
                rayBasedRenderWM.setDisplayAllGlyphs(m_kAllEllipsoids.isSelected());
            }
        } else if (kCommand.equals("Add")) {
            new JDialogDTIInput(JDialogDTIInput.TRACTS_DIALOG, this);
        } else if (kCommand.equals("Remove")) {
            removePolyline();
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
            } else if (m_iType == JDialogDTIInput.TRACTS_DIALOG) {
                processTractFile();
                disposeLocal();
            }
        }
    }

    /**
     * @param e Event that triggered this function.
     */
    public void stateChanged(final ChangeEvent e) {
        final Object source = e.getSource();

        if (source == m_kDisplaySlider) {
            if (rayBasedRenderWM != null) {
                rayBasedRenderWM.setEllipseMod(m_kDisplaySlider.getValue());
            }
        }
    }

    /**
     * @param kEvent The list selection event.
     */
    public void valueChanged(final ListSelectionEvent kEvent) {

        if (rayBasedRenderWM == null) {
            return;
        }
        if ( ((JList) kEvent.getSource()).getMinSelectionIndex() == -1) {
            return;
        }
        if ( ((JList) kEvent.getSource()) != m_kTractList) {
            return;
        }
        final int[] aiSelected = m_kTractList.getSelectedIndices();
        if (aiSelected.length > 1) {
            return;
        }

        final DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        final int iHeaderLength = (new String("FiberBundle")).length();
        for (final int element : aiSelected) {
            final String kName = ((String) (kList.elementAt(element)));
            final int iLength = kName.length();
            final int iGroup = (new Integer(kName.substring(iHeaderLength, iLength))).intValue();
            ColorRGB kColor = null;
            if (rayBasedRenderWM != null) {
                kColor = rayBasedRenderWM.getPolylineColor(iGroup);
            }
            if (kColor != null) {
                m_kColorButton.setBackground(new Color(kColor.R, kColor.G, kColor.B));
                m_kUseVolumeColor.setSelected(false);
            } else {
                m_kColorButton.setBackground(m_kColorButtonDefault);
                m_kUseVolumeColor.setSelected(true);
            }

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

            m_kBMatrix = new GMatrixf(m_iWeights, 6 + 1);

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
                    final float fValue = Float.valueOf(st.nextToken()).floatValue();
                    m_kBMatrix.Set(iRow, iCol, fValue);
                }
                m_kBMatrix.Set(iRow, 6, 1f);
            }
            in.close();

            m_iBOrig = nb;

        } catch (final IOException e) {}
    }

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
    public void loadDWIMaskFile() {
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

    /** Calls AlgorithmDTI2EGFA to create eigen vector and functional anisotropy images. */
    private void calcEigenVectorImage() {
        final int[] extents = m_kDTIImage.getExtents();
        final float[] res = m_kDTIImage.getFileInfo(0).getResolutions();
        final float[] saveRes = new float[] {res[0], res[1], res[2], res[3]};
        if (m_iType == JDialogDTIInput.DWI) {
            if (m_bUseXRes) {
                saveRes[0] = m_fResX;
            }
            if (m_bUseYRes) {
                saveRes[1] = m_fResY;
            }
            if (m_bUseZRes) {
                saveRes[2] = m_fResZ;
            }
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
            AlgorithmTransform transformFunct = new AlgorithmTransform(m_kDTIImage, new TransMatrix(4),
                    AlgorithmTransform.TRILINEAR, newRes[0], newRes[1], newRes[2], volExtents[0], volExtents[1],
                    volExtents[2], false, true, false);
            transformFunct.setRunningInSeparateThread(false);
            transformFunct.run();

            if (transformFunct.isCompleted() == false) {
                transformFunct.finalize();
                transformFunct = null;
            }

            final ModelImage kDTIImageScaled = transformFunct.getTransformedImage();
            kDTIImageScaled.calcMinMax();

            transformFunct.disposeLocal();
            transformFunct = null;

            m_kDTIImage.disposeLocal();
            m_kDTIImage = null;
            m_kDTIImage = kDTIImageScaled;

        }
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
        if (m_iType == JDialogDTIInput.DWI) {
            for (int i = 0; i < m_kDTIImage.getFileInfo().length; i++) {
                m_kDTIImage.getFileInfo(i).setResolutions(saveRes);
                m_kDTIImage.getFileInfo(i).setSliceThickness(saveRes[2]);
            }
        }
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
     * Launches the JFileChooser for the user to select the Eigen Vector file.. Loads the eigen vector data.
     */
    public void loadEigenVectorFile() {
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
     * Launches the JFileChooser for the user to select the functional anisotropy file. Loads the anisotropy data.
     */
    public void loadAnisotropyFile() {
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
     * Creates the user-interface for the Fiber Bundle Tract panel.
     * 
     * @return JPanel containing the user-interface for the Fiber Bundle Tract panel.
     */
    private JPanel createTractPanel() {
        // Scroll panel that hold the control panel layout in order to use JScrollPane
        final JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        final JScrollPane scroller = new JScrollPane(mainScrollPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        final JPanel kTractPanel = new JPanel(new BorderLayout());

        final JPanel buttonPanel = new JPanel();

        // buttons for add/remove of surfaces from list
        final JButton addButton = new JButton("Add");

        addButton.addActionListener(this);
        addButton.setActionCommand("Add");
        addButton.setFont(MipavUtil.font12B);
        addButton.setPreferredSize(MipavUtil.defaultButtonSize);

        final JButton removeButton = new JButton("Remove");

        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");
        removeButton.setFont(MipavUtil.font12B);
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);

        buttonPanel.add(addButton);
        buttonPanel.add(removeButton);

        // list panel for surface filenames
        m_kTractList = new JList(new DefaultListModel());
        m_kTractList.addListSelectionListener(this);
        m_kTractList.setPrototypeCellValue("aaaaaaaaaaaaaaaa.aaa    ");

        final JScrollPane kScrollPane = new JScrollPane(m_kTractList);
        final JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        final JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(buttonPanel, BorderLayout.SOUTH);
        listPanel.setBorder(JInterfaceBase.buildTitledBorder("Fiber Bundle list"));

        final JPanel paintTexturePanel = new JPanel();
        paintTexturePanel.setLayout(new FlowLayout(FlowLayout.LEFT));

        m_kColorButton = new JButton("   ");
        m_kColorButton.setToolTipText("Change fiber bundle color");
        m_kColorButtonDefault = m_kColorButton.getBackground();
        m_kColorButton.addActionListener(this);
        m_kColorButton.setActionCommand("ChangeColor");

        final JLabel kColorLabel = new JLabel("Fiber Bundle color");
        kColorLabel.setFont(MipavUtil.font12B);
        kColorLabel.setForeground(Color.black);

        m_kUseVolumeColor = new JCheckBox("Use volume color");
        m_kUseVolumeColor.addActionListener(this);
        m_kUseVolumeColor.setActionCommand("VolumeColor");
        m_kUseVolumeColor.setSelected(true);

        m_kUseEllipsoids = new JCheckBox("Use Ellipsoids");
        m_kUseEllipsoids.addActionListener(this);
        m_kUseEllipsoids.setActionCommand("UseEllipsoids");
        m_kUseEllipsoids.setSelected(false);

        m_kAllEllipsoids = new JCheckBox("Display All Ellipsoids");
        m_kAllEllipsoids.addActionListener(this);
        m_kAllEllipsoids.setActionCommand("AllEllipsoids");
        m_kAllEllipsoids.setSelected(false);

        m_kDisplaySlider = new JSlider(1, 500, 450);
        m_kDisplaySlider.setEnabled(true);
        m_kDisplaySlider.setMinorTickSpacing(10);
        m_kDisplaySlider.setPaintTicks(true);
        m_kDisplaySlider.addChangeListener(this);
        m_kDisplaySlider.setVisible(true);

        final JLabel kSliderLabel = new JLabel("Display ellipsoids every X step: ");
        kSliderLabel.setFont(MipavUtil.font12B);
        kSliderLabel.setForeground(Color.black);

        final JPanel colorPanel = new JPanel();
        colorPanel.setLayout(new BorderLayout());
        colorPanel.add(m_kColorButton, BorderLayout.WEST);
        colorPanel.add(kColorLabel, BorderLayout.CENTER);
        colorPanel.add(m_kUseVolumeColor, BorderLayout.EAST);
        colorPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        colorPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        final JPanel ellipsePanel = new JPanel();
        ellipsePanel.setLayout(new BorderLayout());
        ellipsePanel.add(m_kUseEllipsoids, BorderLayout.WEST);
        ellipsePanel.add(m_kAllEllipsoids, BorderLayout.CENTER);
        ellipsePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        ellipsePanel.setAlignmentY(Component.TOP_ALIGNMENT);

        final JPanel sliderPanel = new JPanel();
        sliderPanel.setLayout(new BorderLayout());
        sliderPanel.add(kSliderLabel, BorderLayout.WEST);
        sliderPanel.add(m_kDisplaySlider, BorderLayout.CENTER);
        sliderPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        sliderPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        final JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
        optionsPanel.add(colorPanel);
        optionsPanel.add(ellipsePanel);
        optionsPanel.add(sliderPanel);
        optionsPanel.setBorder(JInterfaceBase.buildTitledBorder("Fiber bundle options"));

        final JPanel rightPanel = new JPanel();
        rightPanel.setLayout(new BorderLayout());
        rightPanel.add(optionsPanel, BorderLayout.NORTH);

        final Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(listPanel);
        contentBox.add(rightPanel);

        mainScrollPanel.add(contentBox, BorderLayout.NORTH);

        kTractPanel.add(scroller, BorderLayout.CENTER);

        return kTractPanel;
    }

    /**
     * Creates the user-interface for the Fiber Bundle Tract dialog.
     * 
     * @return JPanel containing the user-interface for the Fiber Bundle Tract dialog.
     */
    private JPanel createTractDialog() {
        final GridBagLayout kGBL = new GridBagLayout();
        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;

        final JPanel kTractPanel = new JPanel(kGBL);

        final JPanel kParamsPanel = new JPanel(kGBL);
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

        gbc.gridx = 0;
        gbc.gridy++;
        final JLabel kUseVOI = new JLabel("Use VOI:");
        kParamsPanel.add(kUseVOI, gbc);
        gbc.gridx++;
        m_kUseVOICheck = new JCheckBox("use voi");
        kParamsPanel.add(m_kUseVOICheck, gbc);

        final JPanel filesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        final JLabel kTractLabel = new JLabel(" DTI tract file: " );
        filesPanel.add(kTractLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        m_kTractPath = new JTextField(35);
        m_kTractPath.setEditable(true);
        m_kTractPath.setBackground(Color.white);
        if ( m_kParentDialog != null && m_kParentDialog.getDTIImage() != null )
        {
        	m_kTractPath.setText(m_kParentDialog.getDTIImage().getImageFileName());
        }
        filesPanel.add(m_kTractPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 0;
        final JButton kTractBrowseButton = new JButton("Browse");
        kTractBrowseButton.addActionListener(this);
        kTractBrowseButton.setActionCommand("tractBrowse");
        filesPanel.add(kTractBrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        kTractPanel.add(kParamsPanel, gbc);
        gbc.gridy++;
        kTractPanel.add(filesPanel, gbc);

        return kTractPanel;
    }

    /**
     * Reads a single fiber bundle tract from disk.
     * 
     * @param kFileReader, FileInputStream.
     * @return Vector<Integer> fiber bundle tract -- list of voxel indices in order in which they appear in the tract.
     */
    private Vector<Integer> inputTract(final FileInputStream kFileReader) {
        int iVQuantity = 0;
        int iBufferSize = 4;

        byte[] racBuffer = new byte[iBufferSize];
        try {
            kFileReader.read(racBuffer, 0, iBufferSize);
        } catch (final IOException e1) {}
        ByteArrayInputStream acBufferIn = new ByteArrayInputStream(racBuffer);
        DataInputStream acDataIn = new DataInputStream(acBufferIn);
        try {
            iVQuantity = acDataIn.readInt();
        } catch (final IOException e) {
            e.printStackTrace();
        }
        acBufferIn = null;
        acDataIn = null;
        racBuffer = null;

        iBufferSize = 4 * iVQuantity;
        racBuffer = new byte[iBufferSize];
        try {
            kFileReader.read(racBuffer, 0, iBufferSize);
        } catch (final IOException e1) {}
        acBufferIn = new ByteArrayInputStream(racBuffer);
        acDataIn = new DataInputStream(acBufferIn);

        final Vector<Integer> kTract = new Vector<Integer>();
        for (int i = 0; i < iVQuantity; i++) {
            try {
                kTract.add(acDataIn.readInt());
            } catch (final IOException e) {
                e.printStackTrace();
            }
        }
        acBufferIn = null;
        acDataIn = null;

        return kTract;
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
            String kDTIName = new String(chooser.getSelectedFile().getName());
            final String kTract = new String("_tract");
            kDTIName = kDTIName.substring(0, kDTIName.length() - kTract.length());
            if (m_kParentDialog.getDTIImage() == null) {
                final FileIO fileIO = new FileIO();
                m_kDTIImage = fileIO.readImage(kDTIName, chooser.getCurrentDirectory() + File.separator);
                if (m_kDTIImage.getNDims() != 4) {
                    MipavUtil.displayError("Diffusion Tensor file does not have correct dimensions");
                    if (m_kDTIImage != null) {
                        m_kDTIImage.disposeLocal();
                    }
                    m_kDTIImage = null;
                }
                if (m_kDTIImage.getExtents()[3] != 6) {
                    MipavUtil.displayError("Diffusion Tensor does not have correct dimensions");
                    if (m_kDTIImage != null) {
                        m_kDTIImage.disposeLocal();
                    }
                    m_kDTIImage = null;
                }
            }

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

    /**
     * process the tract file. Uses the File stored from the loadTractFile fn. Loads fiber bundle tracts, filters them
     * with the user-defined display parameters, and passes them to the GPUVolumeRender for display.
     */
    private void processTractFile() {
        if (m_kTractFile == null) {
            MipavUtil.displayError("Tract file must be set.");
            return;
        }

        try {
            m_kParentDialog.updateTractCount();

            boolean bTractsAdded = false;

            final int iNumTractsLimit = (new Integer(m_kTractsLimit.getText())).intValue();
            final int iTractMinLength = (new Integer(m_kTractsMin.getText())).intValue();
            final int iTractMaxLength = (new Integer(m_kTractsMax.getText())).intValue();

            int iNumTracts = 0;

            ModelImage kVOIImage = null;
            if (m_kUseVOICheck.isSelected()) {
                kVOIImage = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage()
                        .generateBinaryImage(false, true);
            }

            int iDimX = 0, iDimY = 0, iDimZ = 0;
            boolean negX = false, negY = false, negZ = false;
            final FileInputStream kFileReader = new FileInputStream(m_kTractFile);
            final int iBufferSize = 3 * 4 + 3;

            byte[] racBuffer = new byte[iBufferSize];
            kFileReader.read(racBuffer, 0, iBufferSize);
            ByteArrayInputStream acBufferIn = new ByteArrayInputStream(racBuffer);
            DataInputStream acDataIn = new DataInputStream(acBufferIn);
            try {
                iDimX = acDataIn.readInt();
                iDimY = acDataIn.readInt();
                iDimZ = acDataIn.readInt();
                negX = acDataIn.readBoolean();
                negY = acDataIn.readBoolean();
                negZ = acDataIn.readBoolean();
            } catch (final IOException e) {
                e.printStackTrace();
            }
            acBufferIn = null;
            acDataIn = null;
            racBuffer = null;
            
            if ( !m_bDTIImageSet && m_kParentDialog.getDTIImage() == null )
            {
                m_bDTIImageSet = true;
                m_kParentDialog.setDTIImage(m_kDTIImage, negX, negY, negZ );
            }

            final int iLength = (int) m_kTractFile.length();
            int iBufferNext = iBufferSize;
            while (iBufferNext < iLength) {
                if (iNumTracts >= iNumTractsLimit) {
                    break;
                }

                final Vector<Integer> kTract = inputTract(kFileReader);
                iBufferNext += kTract.size() * 4 + 4;
                final int iVQuantity = kTract.size();
                if (contains(kVOIImage, kTract)) {
                    if ( (iVQuantity > iTractMinLength) && (iVQuantity < iTractMaxLength)) {
                        if (iNumTracts < iNumTractsLimit) {
                            iNumTracts++;
                            bTractsAdded = true;
                            m_kParentDialog.addTract(kTract, iVQuantity, 
                            		iDimX, iDimY, iDimZ);
                        }
                    }
                }
            }
            if (bTractsAdded) {
                m_kParentDialog.addTract();
            }
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Determines if the input tract is contained within the ModelImage representing user-selected VOIs.
     * 
     * @param kVOIImage user-selected VOI image.
     * @param kTract, list of voxels in the current fiber bundle tract.
     * @return true if the tract passes through the VOI or if the VOIImage is null, false otherwise.
     */
    private boolean contains(final ModelImage kVOIImage, final Vector<Integer> kTract) {
        if (kVOIImage == null) {
            return true;
        }
        for (int i = 0; i < kTract.size(); i++) {
            final int iIndex = kTract.get(i);
            if (kVOIImage.getBoolean(iIndex)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Pass the DTI image to the GPUVolumeRender.
     * 
     * @param kDTIImage, new DTI image.
     */
    protected void setDTIImage(final ModelImage kDTIImage, boolean bNegX, boolean bNegY, boolean bNegZ) {
        m_kDTIImage = kDTIImage;
        if (rayBasedRenderWM != null) {
            rayBasedRenderWM.setDTIImage(m_kDTIImage, m_kEigenVectorImage, m_kEigenValueImage);
            rayBasedRenderWM.setEllipseMod(m_kDisplaySlider.getValue());
        }
    }

    /**
     * Returns the DTI image.
     * 
     * @return the DTI image.
     */
    protected ModelImage getDTIImage() {
        return m_kDTIImage;
    }

    /**
     * Adds a fiber bundle tract to the GPUVolumeRender and JPanelSurface.
     * 
     * @param kTract, list of voxels in the fiber bundle.
     * @param iVQuantity, number of voxels in the fiber bundle.
     * @param iDimX, the x-dimensions of the DTI image used to create the tract.
     * @param iDimY, the y-dimensions of the DTI image used to create the tract.
     * @param iDimZ, the z-dimensions of the DTI image used to create the tract.
     */
    protected void addTract(final Vector<Integer> kTract, final int iVQuantity, 
    		final int iDimX, final int iDimY, final int iDimZ) {
    	
        final Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0, 3);
        kAttr.SetCChannels(1, 3);
        final VertexBuffer pkVBuffer = new VertexBuffer(kAttr, iVQuantity);

        float fR = 0, fG = 0, fB = 0;
        for (int i = 0; i < iVQuantity; i++) {
            int iIndex = kTract.get(i);

            int iX = iIndex % iDimX;
            iIndex -= iX;
            iIndex /= iDimX;

            int iY = iIndex % iDimY;
            iIndex -= iY;
            iIndex /= iDimY;

            int iZ = iIndex;

            iIndex = kTract.get(i);
            ColorRGB kColor1;
            if (m_kImage.isColorImage()) {
                fR = m_kImage.getFloat(iIndex * 4 + 1) / 255.0f;
                fG = m_kImage.getFloat(iIndex * 4 + 2) / 255.0f;
                fB = m_kImage.getFloat(iIndex * 4 + 3) / 255.0f;
                kColor1 = new ColorRGB(fR, fG, fB);
            } else {
                fR = m_kImage.getFloat(iIndex);
                kColor1 = new ColorRGB(fR, fR, fR);
            }

            final float fX = (float) (iX) / (float) (iDimX);
            final float fY = (float) (iY) / (float) (iDimY);
            final float fZ = (float) (iZ) / (float) (iDimZ);

            pkVBuffer.SetPosition3(i, iX, iY, iZ);
            pkVBuffer.SetColor3(0, i, new ColorRGB(fX, fY, fZ));
            pkVBuffer.SetColor3(1, i, kColor1);
        }
        final boolean bClosed = false;
        final boolean bContiguous = true;
        // apply B-spline filter to smooth the track
        addPolyline(new Polyline(smoothTrack(pkVBuffer, kTract, iVQuantity, iDimX, iDimY, iDimZ), bClosed, bContiguous));
    }

    /**
     * Smooth the fiber tracks with B-spline interpolation
     * 
     * @param pkVBuffer, fiber track vertex coordinates as the control points.
     * @param kTract, fiber track index list.
     * @param iVQuantity, number of voxels in the fiber bundle.
     * @return B-spline interpolated fiber track
     */
    private VertexBuffer smoothTrack(final VertexBuffer pkVBuffer, final Vector<Integer> kTract, final int iVQuantity,
            final int iDimX, final int iDimY, final int iDimZ) {
        float fX_0, fY_0, fZ_0;
        float fX_1, fY_1, fZ_1;
        float fX_2, fY_2, fZ_2;
        float fX_3, fY_3, fZ_3;

        // curve sub-division number, default to 10.
        final int curveSubD = 1;
        float u, u_2, u_3;

        final Attributes attr = new Attributes();
        attr.SetPChannels(3);
        attr.SetCChannels(0, 3);
        attr.SetCChannels(1, 3);
        final VertexBuffer bsplineVBuffer = new VertexBuffer(attr, (iVQuantity - 3) * curveSubD);

        int index = 0;

        float fR = 0, fG = 0, fB = 0;

        float pos_x, pos_y, pos_z;

        for (int i = 0; i < iVQuantity - 3; i++) {
            for (int j = 0; j < curveSubD; j++) {

                ColorRGB resultUnit0, resultUnit1;

                u = (float) j / curveSubD;
                u_2 = u * u;
                u_3 = u_2 * u;

                fX_0 = pkVBuffer.GetPosition3fX(i);
                fY_0 = pkVBuffer.GetPosition3fY(i);
                fZ_0 = pkVBuffer.GetPosition3fZ(i);

                fX_1 = pkVBuffer.GetPosition3fX(i + 1);
                fY_1 = pkVBuffer.GetPosition3fY(i + 1);
                fZ_1 = pkVBuffer.GetPosition3fZ(i + 1);

                fX_2 = pkVBuffer.GetPosition3fX(i + 2);
                fY_2 = pkVBuffer.GetPosition3fY(i + 2);
                fZ_2 = pkVBuffer.GetPosition3fZ(i + 2);

                fX_3 = pkVBuffer.GetPosition3fX(i + 3);
                fY_3 = pkVBuffer.GetPosition3fY(i + 3);
                fZ_3 = pkVBuffer.GetPosition3fZ(i + 3);

                pos_x = B_SPLINE(u, u_2, u_3, fX_0, fX_1, fX_2, fX_3);
                pos_y = B_SPLINE(u, u_2, u_3, fY_0, fY_1, fY_2, fY_3);
                pos_z = B_SPLINE(u, u_2, u_3, fZ_0, fZ_1, fZ_2, fZ_3);

                int iIndex = kTract.get(i);

                final int iX = iIndex % iDimX;
                iIndex -= iX;
                iIndex /= iDimX;

                final int iY = iIndex % iDimY;
                iIndex -= iY;
                iIndex /= iDimY;

                final int iZ = iIndex;

                final float fX = (float) (iX) / (float) (iDimX);
                final float fY = (float) (iY) / (float) (iDimY);
                final float fZ = (float) (iZ) / (float) (iDimZ);

                resultUnit0 = new ColorRGB(fX, fY, fZ);

                iIndex = kTract.get(i);

                if (m_kImage.isColorImage()) {
                    fR = m_kImage.getFloat(iIndex * 4 + 1) / 255.0f;
                    fG = m_kImage.getFloat(iIndex * 4 + 2) / 255.0f;
                    fB = m_kImage.getFloat(iIndex * 4 + 3) / 255.0f;
                    resultUnit1 = new ColorRGB(fR, fG, fB);
                } else {
                    fR = m_kImage.getFloat(iIndex);
                    resultUnit1 = new ColorRGB(fR, fR, fR);
                }

                bsplineVBuffer.SetPosition3(index, pos_x, pos_y, pos_z);
                bsplineVBuffer.SetColor3(0, index, resultUnit0);
                bsplineVBuffer.SetColor3(1, index, resultUnit1);

                index++;

            }
        }

        return bsplineVBuffer;

    }

    /**
     * B-spline computation.
     * 
     * @param u u parameter
     * @param u_2 u^2 parameter
     * @param u_3 u^3 parameter
     * @param cntrl0 1st control point coordinate
     * @param cntrl1 2nd control point coordinate
     * @param cntrl2 3rd control point coordinate
     * @param cntrl3 4th control point coordinate
     * @return interpolated position
     */
    private float B_SPLINE(final float u, final float u_2, final float u_3, final float cntrl0, final float cntrl1,
            final float cntrl2, final float cntrl3) {

        return ( ( ( -1 * u_3 + 3 * u_2 - 3 * u + 1) * (cntrl0) + (3 * u_3 - 6 * u_2 + 0 * u + 4) * (cntrl1)
                + ( -3 * u_3 + 3 * u_2 + 3 * u + 1) * (cntrl2) + (1 * u_3 + 0 * u_2 + 0 * u + 0) * (cntrl3)) / 6f);
    }

    /**
     * Add a polyline to the GPUVolumeRender.
     * 
     * @param kLine, the Polyline to add.
     */
    protected void addPolyline(final Polyline kLine) {
        if (rayBasedRenderWM != null) {
            rayBasedRenderWM.addPolyline(kLine, m_iBundleCount);
        }
    }

    /** Updates the number of fiber bundle tract groups. */
    protected void updateTractCount() {
        m_iBundleCount = getMinUnused(m_kBundleList);
    }

    /** Updates the tract list user-interface. */
    protected void addTract() {
        m_kBundleList.add(new Integer(m_iBundleCount));

        final DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        final int iSize = kList.getSize();
        kList.add(iSize, new String("FiberBundle" + m_iBundleCount));
        m_kTractList.setSelectedIndex(iSize);
    }

    /**
     * Gets a new fiber bundle index.
     * 
     * @param kBundleList, list of fiber bundles.
     */
    private int getMinUnused(final Vector<Integer> kBundleList) {
        int iMin = 0;
        if (kBundleList.size() == 0) {
            return iMin;
        }
        boolean bFound = false;
        for (int i = 0; i < kBundleList.size(); i++) {
            iMin = i;
            bFound = false;
            for (int j = 0; j < kBundleList.size(); j++) {
                if (iMin == kBundleList.get(j).intValue()) {
                    bFound = true;
                }
            }
            if ( !bFound) {
                return iMin;
            }
        }
        iMin++;
        return iMin;
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#setButtonColor(javax.swing.JButton,
     *      java.awt.Color)
     */
    public void setButtonColor(final JButton _button, final Color _color) {
        super.setButtonColor(_button, _color);
        final int[] aiSelected = m_kTractList.getSelectedIndices();
        final DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        final int iHeaderLength = (new String("FiberBundle")).length();

        for (final int element : aiSelected) {
            if (rayBasedRenderWM != null) {
                final String kName = ((String) (kList.elementAt(element)));
                final int iLength = kName.length();
                final int iGroup = (new Integer(kName.substring(iHeaderLength, iLength))).intValue();
                if (_color == null) {
                    rayBasedRenderWM.setPolylineColor(iGroup, null);
                }

                else if ( !m_kUseVolumeColor.isSelected()) {
                    rayBasedRenderWM.setPolylineColor(iGroup, new ColorRGB(_color.getRed() / 255.0f,
                            _color.getGreen() / 255.0f, _color.getBlue() / 255.0f));
                }
            }
        }
    }

    /** Removes the fiber bundle from the GPUVolumeRender and JPanelSurface. */
    private void removePolyline() {
        final int[] aiSelected = m_kTractList.getSelectedIndices();

        final DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        final int iHeaderLength = (new String("FiberBundle")).length();

        for (final int element : aiSelected) {
            if (rayBasedRenderWM != null) {
                final String kName = ((String) (kList.elementAt(element)));
                final int iLength = kName.length();
                final int iGroup = (new Integer(kName.substring(iHeaderLength, iLength))).intValue();
                rayBasedRenderWM.removePolyline(iGroup);
                m_kBundleList.remove(new Integer(iGroup));
            }
            kList.remove(element);
        }
        if (kList.size() == 0) {
            if (m_kDTIImage != null) {
                m_kDTIImage.disposeLocal();
            }
            m_kDTIImage = null;
        } else {
            m_kTractList.setSelectedIndex(kList.size());
        }
    }

};
