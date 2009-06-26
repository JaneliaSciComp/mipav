package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.flythruview.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * DOCUMENT ME!
 */
public class JPanelBrainSurfaceFlattener extends JPanelRendererJ3D implements ViewImageUpdateInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2749305357977770854L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean m_bFileLoaded = false;

    /** DOCUMENT ME! */
    private boolean m_bFirstSurface = true;

    /** DOCUMENT ME! */
    private int m_iGridY = 0;

    /** DOCUMENT ME! */
    private JButton m_kButtonLoadImage = new JButton();

    /** DOCUMENT ME! */
    private JButton m_kButtonLoadSurface = new JButton();

    /** DOCUMENT ME! */
    private Point3f m_kCenter = null;

    /** DOCUMENT ME! */
    private JRadioButton m_kDisablePick = new JRadioButton();

    /** DOCUMENT ME! */
    private ButtonGroup m_kDisplayButtonGroup = new ButtonGroup();

    /** DOCUMENT ME! */
    private JRadioButton m_kDisplayPlane = new JRadioButton();

    /** DOCUMENT ME! */
    private JRadioButton m_kDisplaySphere = new JRadioButton();

    /** DOCUMENT ME! */
    private File m_kFile;

    /** DOCUMENT ME! */
    private ModelImage m_kImage = null;

    /** DOCUMENT ME! */
    private String m_kImageDir;

    /** DOCUMENT ME! */
    private String m_kImageFile;

    /** DOCUMENT ME! */
    private JLabel m_kLabelFileName = new JLabel();

    /** DOCUMENT ME! */
    private JLabel m_kLabelFileNameImage = new JLabel();

    /** DOCUMENT ME! */
    private JCheckBox m_kLatLonLines = new JCheckBox();

    /** DOCUMENT ME! */
    private ModelLUT m_kLUTa = null;

    /** DOCUMENT ME! */
    private ModelImage m_kLUTImageA = null;

    /** DOCUMENT ME! */
    private JTextField m_kNumLatText = new JTextField("9", 3);

    /** DOCUMENT ME! */
    private JTextField m_kNumLonText = new JTextField("13", 3);

    /** DOCUMENT ME! */
    private JPanelHistoLUT m_kPanelBrainsurfaceFlattenerLUT = null;

    /** DOCUMENT ME! */
    private ViewJFrameVolumeView m_kParentFrame;

    /** DOCUMENT ME! */
    private ButtonGroup m_kPickButtonGroup = new ButtonGroup();

    /** DOCUMENT ME! */
    private JRadioButton m_kPickCorrespondence = new JRadioButton();

    /** DOCUMENT ME! */
    private JRadioButton m_kPickPuncture = new JRadioButton();

    /** DOCUMENT ME! */
    private String m_kSurfaceDir;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kTriangleMesh = null;

    /** DOCUMENT ME! */
    private MjCorticalAnalysis m_kView;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create the control-panel for the brainsurfaceFlattener interface:
     *
     * @param  kView         the engine behind the brainsurfaceFlattener and one of the parent frames affected by the
     *                       interface
     * @param  kImage        the ModelImage data
     * @param  kParentFrame  the parent frame for the panel, contains the surfaceRenderer, where the brain model is
     *                       displayed
     */
    public JPanelBrainSurfaceFlattener(MjCorticalAnalysis kView, ModelImage kImage, ViewJFrameVolumeView kParentFrame) {
        super(kView);
        m_kImage = kImage;
        m_kParentFrame = kParentFrame;
        m_kView = kView;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets up the variables needed for running the algorithm, and
     * calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        /* Load the ModelImage file, if changed and the surface file: */
        if (source == OKButton) {
            loadingImage();
            loadingSurface();
            setVisible(false);
        } else if (source == cancelButton) {
            setVisible(false);
        }

        /* Launch the file chooser and set the new filename for the ModelImage
         * data: */
        if (command.equals("Select Surface ...")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(true);
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            if (JFileChooser.APPROVE_OPTION != chooser.showOpenDialog(null)) {
                return;
            }

            m_kFile = chooser.getSelectedFile();
            m_kSurfaceDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            m_kLabelFileName.setText(m_kFile.getName());
            chooser.setVisible(false);
        }
        /* Launch the file chooser and set the new filename for the .sur
         * file: */
        else if (command.equals("Select Image ...")) {
            JFileChooser chooser = new JFileChooser();

            chooser.setMultiSelectionEnabled(true);
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            if (JFileChooser.APPROVE_OPTION != chooser.showOpenDialog(null)) {
                return;
            }

            m_kImageFile = chooser.getSelectedFile().getName();
            m_kImageDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            m_kLabelFileNameImage.setText(m_kImageFile);
            chooser.setVisible(false);
        }
        /* If no file is loaded, then the remaining interface options are not
         * defined: */
        else if (!m_bFileLoaded) {
            return;
        }
        /* Toggle the latitude/longitude lines On/Off: */
        else if (command.equals("LatLonLines")) {
            m_kView.toggleLatLonLines();
        }
        /* Toggle the enable picking On/Off: */
        else if (source == m_kPickCorrespondence) {

            if (m_kView.isPuncturePickEnabled()) {
                m_kView.togglePickPuncture();
            }

            m_kView.togglePickCorrespondence();
        } else if (source == m_kPickPuncture) {

            if (m_kView.isCorrespondencePickEnabled()) {
                m_kView.togglePickCorrespondence();
            }

            m_kView.togglePickPuncture();
        } else if (source == m_kDisablePick) {

            if (m_kView.isCorrespondencePickEnabled()) {
                m_kView.togglePickCorrespondence();
            }

            if (m_kView.isPuncturePickEnabled()) {
                m_kView.togglePickPuncture();
            }
        }
        /* Remove all picked points from the scene: */
        else if (command.equals("RemovePoints")) {
            m_kView.removePoints();
        }
        /* Remove all picked points from the scene: */
        else if (command.equals("RecalculateConformal")) {

            if (m_bFileLoaded) {
                m_kParentFrame.removeBranch(m_kView.getMeshLines(), false);
                m_kView.calculateConformal();

                int iNumLat = Integer.parseInt(m_kNumLatText.getText());
                int iNumLon = Integer.parseInt(m_kNumLonText.getText());
                m_kView.setupLatLon(iNumLat, iNumLon);
                m_kParentFrame.addBranch(m_kView.getMeshLines(), null, null);
            }
        }

        /* Display the Plane view of the brain in the right-hand panel: */
        else if (source == m_kDisplayPlane) {
            m_kView.displayPlane();
        }
        /* Display the Sphere view of the brain in the right-hand panel: */
        else if (source == m_kDisplaySphere) {
            m_kView.displaySphere();
        }
        /* Perform the inflation step in the brainsurface flattener */
        else if (command.equals("Inflation")) {
            boolean bResult = m_kView.inflation();
        }
        /* The number of latitude/longitude lines has changed, update the
         * meshes: */
        else if (command.equals("UpdateMesh")) {
            m_kParentFrame.removeBranch(m_kView.getMesh(), true);
            m_kParentFrame.addBranch(m_kView.getMesh(), m_kView.getTMesh(), m_kCenter);
        } else if (command.equals("NumLatChanged") || command.equals("NumLonChanged")) {
            m_kLatLonLines.setSelected(true);

            m_kParentFrame.removeBranch(m_kView.getMeshLines(), false);

            int iNumLat = Integer.parseInt(m_kNumLatText.getText());
            int iNumLon = Integer.parseInt(m_kNumLonText.getText());
            m_kView.setupLatLon(iNumLat, iNumLon);

            m_kParentFrame.addBranch(m_kView.getMeshLines(), null, null);
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        m_kLabelFileName = null;
        m_kButtonLoadSurface = null;
        m_kLabelFileNameImage = null;
        m_kButtonLoadImage = null;
        m_kLatLonLines = null;
        m_kPickCorrespondence = null;
        m_kPickButtonGroup = null;
        m_kDisplayPlane = null;
        m_kDisplaySphere = null;
        m_kDisplayButtonGroup = null;
        m_kNumLatText = null;
        m_kNumLonText = null;

        if (m_kTriangleMesh != null) {
            m_kTriangleMesh = null;
        }

        if (m_kImage != null) {
            m_kImage = null;
        }

        if (m_kLUTImageA != null) {
            m_kLUTImageA = null;
        }

        if (m_kLUTa != null) {
            m_kLUTa = null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getImageA() {
        return m_kLUTImageA;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getImageB() {
        return null;
    }


    /**
     * Return the main control panel.
     *
     * @return  JPanel the main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * setSlice.
     *
     * @param  slice  int
     */
    public void setSlice(int slice) { }

    /**
     * setTimeSlice.
     *
     * @param  tSlice  int
     */
    public void setTimeSlice(int tSlice) { }

    /**
     * updateImageExtents.
     *
     * @return  boolean
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * updateImages.
     *
     * @return  boolean
     */
    public boolean updateImages() {
        return false;
    }

    /**
     * updateImages.
     *
     * @param   flag  boolean
     *
     * @return  boolean
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /**
     * updateImages.
     *
     * @param   LUTa        ModelLUT
     * @param   LUTb        ModelLUT
     * @param   flag        boolean
     * @param   interpMode  int
     *
     * @return  boolean
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {

        if (LUTa != null) {
            m_kLUTa = LUTa;
            m_kView.setLUTCurvature(m_kLUTa);
            m_kView.displayCurvatureColors();
        }

        return true;
    }

    /**
     * Delete all local member variables:
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Creates a LUT for the surface, where the curvature values are used in the look-up table instead of the ModelImage
     * values:
     */
    private void createLUTFromSurface() {

        /* Create ModelImage from curvature values: */
        float fMin = m_kView.getMinCurvature();
        float fMax = m_kView.getMaxCurvature();

        int[] iExtents = { 256, 256 };
        m_kLUTImageA = new ModelImage(ModelStorageBase.FLOAT, iExtents, "temp");
        m_kLUTImageA.addImageDisplayListener(this);

        for (int i = 0; i < 256; i++) {

            for (int j = 0; j < 256; j++) {
                m_kLUTImageA.set(i, j, fMin + ((float) i / 255.0f * (fMax - fMin)));
            }
        }

        m_kLUTImageA.calcMinMax();

        /* Create LUT */
        int[] dimExtentsLUT = { 4, 256 };
        m_kLUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
        m_kLUTa.resetTransferLine(fMin, fMin, fMax, fMax);

        /* Create LUT panel: */
        m_kPanelBrainsurfaceFlattenerLUT = new JPanelHistoLUT(m_kLUTImageA, null, m_kLUTa, null, true);

        JPanel brainsurfaceFlattenerLUTPanel = new JPanel();
        brainsurfaceFlattenerLUTPanel.add(m_kPanelBrainsurfaceFlattenerLUT.getMainPanel());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        mainPanel.add(brainsurfaceFlattenerLUTPanel, gbc);

        mainPanel.updateUI();
        m_kView.setLUTCurvature(m_kLUTa);
    }

    /**
     * Resets the Mesh Display, when the file is reloaded:
     */
    private void displayCorticalAnalysis() {
        m_kView.setup(m_kTriangleMesh);

        int iNumLat = Integer.parseInt(m_kNumLatText.getText());
        int iNumLon = Integer.parseInt(m_kNumLonText.getText());
        m_kView.setupLatLon(iNumLat, iNumLon);

        /* If this is fht first surface loaded, create and display the
         * LUT: */
        if (m_bFirstSurface == true) {
            m_bFirstSurface = false;
            createLUTFromSurface();
        } else {

            /* Otherwise, reset the LUT to grayscale:
             * m_kLUTa.makeGrayTransferFunctions(); m_kLUTa.makeLUT( 256 ); m_kPanelBrainsurfaceFlattenerLUT.setLUTA(
             * m_kLUTa ); m_kPanelBrainsurfaceFlattenerLUT.selectLUTa( );mainPanel.updateUI(); */
            m_kView.setLUTCurvature(m_kLUTa);
        }

        /* Display mesh in the surfaceRender: */
        m_kParentFrame.addBranch(m_kView.getMesh(), m_kView.getTMesh(), m_kCenter);
        m_kParentFrame.addBranch(m_kView.getMeshLines(), null, null);

        m_bFileLoaded = true;

        /* Reset the defaults: */
        /* Display plane in right-hand view: */
        m_kView.displayPlane();
        m_kDisplaySphere.setSelected(false);
        m_kDisplayPlane.setSelected(true);

        /* Display the latitude/longitude lines: */
        m_kLatLonLines.setSelected(true);

        /* Reset the color map to display curvature: */
        m_kView.displayCurvatureColors();
    }

    /**
     * Initialize the user-interface, buttons and ActionCommands.
     */
    private void init() {

        /* Button: Load Surface */
        m_kButtonLoadImage.setText("Select Image ...");
        m_kButtonLoadImage.setActionCommand("Select Image ...");
        m_kButtonLoadImage.addActionListener(this);


        /* Label: name of loaded surface file */
        m_kLabelFileNameImage.setPreferredSize(new Dimension(130, 21));
        m_kLabelFileNameImage.setBorder(BorderFactory.createLoweredBevelBorder());
        m_kLabelFileNameImage.setText(m_kImage.getImageName());
        m_kImageFile = m_kImage.getImageName();


        /* Button: Load Surface */
        m_kButtonLoadSurface.setText("Select Surface ...");
        m_kButtonLoadSurface.setActionCommand("Select Surface ...");
        m_kButtonLoadSurface.addActionListener(this);


        /* Label: name of loaded surface file */
        m_kLabelFileName.setPreferredSize(new Dimension(130, 21));
        m_kLabelFileName.setBorder(BorderFactory.createLoweredBevelBorder());
        m_kLabelFileName.setText("<no surface loaded>");

        JPanel kSurfacePanel = new JPanel();
        kSurfacePanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        kSurfacePanel.add(m_kButtonLoadImage, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        kSurfacePanel.add(m_kLabelFileNameImage, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        kSurfacePanel.add(m_kButtonLoadSurface, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        kSurfacePanel.add(m_kLabelFileName, gbc);

        Box kFileContentBox = new Box(BoxLayout.Y_AXIS);
        kFileContentBox.setBorder(buildTitledBorder("File control box"));

        JPanel kPanelButton = new JPanel();
        buildOKButton();
        OKButton.setText("Load");
        kPanelButton.add(OKButton);

        kFileContentBox.add(kSurfacePanel);
        kFileContentBox.add(kPanelButton);


        /* Latitude/Longitude panel layout: */
        Box kLatLonContentBox = new Box(BoxLayout.Y_AXIS);
        kLatLonContentBox.setBorder(buildTitledBorder("Latitude/Longitude control box"));

        int iGridY = 0;
        JPanel kLatLonPanel = new JPanel();
        kLatLonPanel.setLayout(new GridBagLayout());

        /* Display latitude/longitude lines toggle and label: */
        gbc.gridx = 0;
        gbc.gridy = iGridY;
        m_kLatLonLines.setSelected(true);
        m_kLatLonLines.addActionListener(this);
        m_kLatLonLines.setActionCommand("LatLonLines");
        kLatLonPanel.add(m_kLatLonLines, gbc);

        JLabel kLatLonLabel = new JLabel("Show Latitude/Longitude lines");
        gbc.gridx = 1;
        gbc.gridy = iGridY++;
        kLatLonPanel.add(kLatLonLabel, gbc);


        /* Number of latitude lines label and textField : */
        JLabel kNumLatLabel = new JLabel();
        kNumLatLabel.setText("Number of Latitude Lines");
        gbc.gridx = 1;
        gbc.gridy = iGridY;
        kLatLonPanel.add(kNumLatLabel, gbc);
        m_kNumLatText.addActionListener(this);
        m_kNumLatText.setActionCommand("NumLatChanged");
        gbc.gridx = 2;
        gbc.gridy = iGridY++;
        kLatLonPanel.add(m_kNumLatText, gbc);


        /* Number of longitude lines button and label: */
        JLabel kNumLonLabel = new JLabel();
        kNumLonLabel.setText("Number of Longitude Lines");
        gbc.gridx = 1;
        gbc.gridy = iGridY;
        kLatLonPanel.add(kNumLonLabel, gbc);
        m_kNumLonText.addActionListener(this);
        m_kNumLonText.setActionCommand("NumLonChanged");
        gbc.gridx = 2;
        gbc.gridy = iGridY++;
        kLatLonPanel.add(m_kNumLonText, gbc);

        kLatLonContentBox.add(kLatLonPanel);

        /* Plane/Sphere panel layout: */
        Box kPlaneSphereContentBox = new Box(BoxLayout.Y_AXIS);
        kPlaneSphereContentBox.setBorder(buildTitledBorder("Plane/Sphere display control box"));

        iGridY = 0;

        JPanel kPlaneSpherePanel = new JPanel();
        kPlaneSpherePanel.setLayout(new GridBagLayout());

        /* Display Plane / Display Sphere radio buttons*/
        gbc.gridx = 0;
        gbc.gridy = iGridY;
        m_kDisplayPlane.setSelected(true);
        m_kDisplayPlane.addActionListener(this);
        m_kDisplayPlane.setActionCommand("DisplayPlane");
        kPlaneSpherePanel.add(m_kDisplayPlane, gbc);
        m_kDisplayButtonGroup.add(m_kDisplayPlane);

        JLabel kPlaneLabel = new JLabel("Display Plane");
        gbc.gridx = 1;
        gbc.gridy = iGridY;
        kPlaneSpherePanel.add(kPlaneLabel, gbc);

        gbc.gridx = 2;
        gbc.gridy = iGridY;
        m_kDisplaySphere.setSelected(false);
        m_kDisplaySphere.addActionListener(this);
        m_kDisplaySphere.setActionCommand("DisplaySphere");
        kPlaneSpherePanel.add(m_kDisplaySphere, gbc);
        m_kDisplayButtonGroup.add(m_kDisplaySphere);

        JLabel kSphereLabel = new JLabel("Display Sphere");
        gbc.gridx = 3;
        gbc.gridy = iGridY++;
        kPlaneSpherePanel.add(kSphereLabel, gbc);

        kPlaneSphereContentBox.add(kPlaneSpherePanel);


        /* Picking panel layout: */
        Box kPickContentBox = new Box(BoxLayout.Y_AXIS);
        kPickContentBox.setBorder(buildTitledBorder("Picking control box"));

        iGridY = 0;

        JPanel kPickPanel = new JPanel();
        kPickPanel.setLayout(new GridBagLayout());

        /* Enable Picking of Correspondence Points radio button and label:*/
        gbc.gridx = 0;
        gbc.gridy = iGridY;
        m_kPickCorrespondence.setSelected(false);
        m_kPickCorrespondence.addActionListener(this);
        m_kPickCorrespondence.setActionCommand("PickCorrespondence");
        m_kPickButtonGroup.add(m_kPickCorrespondence);
        kPickPanel.add(m_kPickCorrespondence, gbc);

        JLabel kPickCorrespondenceLabel = new JLabel("Pick Correspondence Points");
        gbc.gridx = 1;
        gbc.gridy = iGridY;
        kPickPanel.add(kPickCorrespondenceLabel, gbc);

        /* Remove points button: */
        JButton kButtonRemovePoints = new JButton();
        kButtonRemovePoints.setText("Remove all points");
        kButtonRemovePoints.setActionCommand("RemovePoints");
        kButtonRemovePoints.addActionListener(this);
        gbc.gridx = 2;
        gbc.gridy = iGridY++;
        kPickPanel.add(kButtonRemovePoints, gbc);


        /* Enable Picking of Puncture Points radio button and label:*/
        gbc.gridx = 0;
        gbc.gridy = iGridY;
        m_kPickPuncture.setSelected(false);
        m_kPickPuncture.addActionListener(this);
        m_kPickPuncture.setActionCommand("PickPuncture");
        m_kPickButtonGroup.add(m_kPickPuncture);
        kPickPanel.add(m_kPickPuncture, gbc);

        JLabel kPickPunctureLabel = new JLabel("Pick Puncture Triangle");
        gbc.gridx = 1;
        gbc.gridy = iGridY;
        kPickPanel.add(kPickPunctureLabel, gbc);

        /* Recalculate conformal mapping button: */
        JButton kButtonConformal = new JButton();
        kButtonConformal.setText("Recalculate Conformal Mapping");
        kButtonConformal.setActionCommand("RecalculateConformal");
        kButtonConformal.addActionListener(this);
        gbc.gridx = 2;
        gbc.gridy = iGridY++;
        kPickPanel.add(kButtonConformal, gbc);

        /* Disable Picking radio button and label:*/
        gbc.gridx = 0;
        gbc.gridy = iGridY;
        m_kDisablePick.setSelected(true);
        m_kDisablePick.addActionListener(this);
        m_kDisablePick.setActionCommand("DisablePick");
        m_kPickButtonGroup.add(m_kDisablePick);
        kPickPanel.add(m_kDisablePick, gbc);

        JLabel kDisablePickLabel = new JLabel("Disable Mouse Picking");
        gbc.gridx = 1;
        gbc.gridy = iGridY++;
        kPickPanel.add(kDisablePickLabel, gbc);

        kPickContentBox.add(kPickPanel);


        /* Panel layout: */
        Box kContentBox = new Box(BoxLayout.Y_AXIS);
        kContentBox.setBorder(buildTitledBorder("Surface Inflation and Color"));

        iGridY = 0;

        JPanel kPanel = new JPanel();
        kPanel.setLayout(new GridBagLayout());

        /* Inflation button: */
        JButton kButtonInflation = new JButton();
        kButtonInflation.setText("Inflate");
        kButtonInflation.setActionCommand("Inflation");
        kButtonInflation.addActionListener(this);
        gbc.gridx = 1;
        gbc.gridy = iGridY;
        kPanel.add(kButtonInflation, gbc);

        /* Update the mesh in the 2D planar views button: */
        JButton kButtonUpdateMesh = new JButton();
        kButtonUpdateMesh.setText("Update Mesh in 2D-Planar Views");
        kButtonUpdateMesh.setActionCommand("UpdateMesh");
        kButtonUpdateMesh.addActionListener(this);
        gbc.gridx = 2;
        gbc.gridy = iGridY++;
        kPanel.add(kButtonUpdateMesh, gbc);


        kContentBox.add(kPanel);

        /* Create and add the mainPanel: */
        mainPanel = new JPanel();
        mainPanel.setLayout(new GridBagLayout());
        m_iGridY = 0;
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        mainPanel.add(kFileContentBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        mainPanel.add(kLatLonContentBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        mainPanel.add(kPlaneSphereContentBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        mainPanel.add(kPickContentBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        mainPanel.add(kContentBox, gbc);
    }

    /**
     * Load a new ModelImage for the brainsurface flattener scene:
     */
    private void loadingImage() {

        /* If the ModelImage m_kImage is not defined (null) or the name
         * doesn't equal the name set in m_kImageFile, then load the new
         * ModelImage: */
        if ((m_kImage == null) || ((m_kImage != null) && (!m_kImageFile.equals(m_kImage.getImageName())))) {
            FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);
            m_kImage = fileIO.readImage(m_kImageFile, m_kImageDir, false, null);
        }
    }

    /**
     * Load a new surface file for the brainsurface flattener scene:
     */
    private void loadingSurface() {

        /* If no file has been specified, return: */
        if (m_kFile == null) {
            return;
        }

        try {

            if (m_bFileLoaded) {
                m_kParentFrame.removeBranch(m_kView.getMesh(), true);
                m_kParentFrame.removeBranch(m_kView.getMeshLines(), false);
            }

            /* Load the selected surface file and extract the mesh. */
            SurfaceLoaderSUR kSurfaceLoader = new SurfaceLoaderSUR(m_kFile.getPath());

            m_kTriangleMesh = kSurfaceLoader.getTriangleMesh();

            int[] direction = ModelTriangleMesh.getDirection();
            float[] startLocation = ModelTriangleMesh.getStartLocation();
            Point3f[] akVertex = m_kTriangleMesh.getVertexCopy();
            int[] extents = m_kImage.getExtents();
            int xDim = extents[0];
            int yDim = extents[1];
            int zDim = extents[2];
            float[] resols = m_kImage.getFileInfo()[0].getResolutions();
            float xBox = (xDim - 1) * resols[0];
            float yBox = (yDim - 1) * resols[1];
            float zBox = (zDim - 1) * resols[2];
            float maxBox = Math.max(xBox, Math.max(yBox, zBox));
            float xSum = 0f, ySum = 0f, zSum = 0f;

            int iVQuantity = akVertex.length;

            /* Transform vertices into same space as the tri-planar view: */
            for (int j = 0; j < iVQuantity; j++) {
                akVertex[j].x = ((2.0f * (akVertex[j].x - startLocation[0]) / direction[0]) -
                                 ((xDim - 1) * resols[0])) / maxBox;
                akVertex[j].y = ((2.0f * (akVertex[j].y - startLocation[1]) / direction[1]) -
                                 ((yDim - 1) * resols[1])) / maxBox;
                akVertex[j].z = ((2.0f * (akVertex[j].z - startLocation[2]) / direction[2]) -
                                 ((zDim - 1) * resols[2])) / maxBox;

                xSum += akVertex[j].x;
                ySum += akVertex[j].y;
                zSum += akVertex[j].z;
            }

            m_kCenter = new Point3f(xSum / (float) iVQuantity, ySum / (float) iVQuantity, zSum / (float) iVQuantity);

            m_kTriangleMesh.setVerticies(akVertex);

            /* Display the mesh: */
            displayCorticalAnalysis();
        } catch (IOException kException) {
            System.out.println(kException.getMessage());
            kException.printStackTrace();

            return;
        } finally {
            ViewJFrameVolumeView.getRendererProgressBar().setValue(100);
            ViewJFrameVolumeView.getRendererProgressBar().update(ViewJFrameVolumeView.getRendererProgressBar().getGraphics());
        }
    }
}
