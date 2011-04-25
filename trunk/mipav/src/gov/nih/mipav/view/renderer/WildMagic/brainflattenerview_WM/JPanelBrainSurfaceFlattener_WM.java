package gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.renderer.JPanelHistoLUT;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

public class JPanelBrainSurfaceFlattener_WM extends JPanel implements ActionListener, ViewImageUpdateInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2749305357977770854L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Builds a titled border with the given title, an etched border, and the
     * proper font and color.  Changed to public static member so that it can
     * be used for other JPanels not inherited from this base class.
     * @param   title  Title of the border
     *
     * @return  The titled border.
     */
    public static TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title,
                                TitledBorder.LEFT, TitledBorder.CENTER,
                                MipavUtil.font12B, Color.black);
    }

    /** The main control. */
    protected JPanel mainPanel = null;

    /** Fonts, same as <code>MipavUtil.font12</code> and
     * <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;

    /**
     * OK button is used on most dialogs. Defining it in the base allows
     * default actions if the user presses return and the button is in focus.
     */
    protected JButton OKButton;

    /**
     * Cancel button is used on most dialogs. Defining it in the base allows
     * default actions if the user presses return and the button is in focus.
     */
    protected JButton cancelButton;

    /** Set to true when a surface has been loaded */
    private boolean m_bFileLoaded = false;

    /** Causes the surface LUT to be calculated for the first time. */
    private boolean m_bFirstSurface = true;

    /** Keeps track of the user-interface position in the main panel. */
    private int m_iGridY = 0;

    /** Enable/disable correspondence picking */
    private JRadioButton m_kDisablePick = new JRadioButton();

    /** ButtonGroup for the plane/surface display selection. */
    private ButtonGroup m_kDisplayButtonGroup = new ButtonGroup();

    /** Toggle displaying the plane on/off */
    private JRadioButton m_kDisplayPlane = new JRadioButton();

    /** Toggle displaying the sphere on/off */
    private JRadioButton m_kDisplaySphere = new JRadioButton();

    /** Toggle display of latitude/longitude lines on/off. */
    private JCheckBox m_kLatLonLines = new JCheckBox();

    /** Color LUT based on Volume Data */
    private ModelLUT m_kLUTa = null;

    /** Volume data for LUT */
    private ModelImage m_kLUTImageA = null;

    /** Text box for entering the number of latitude lines. */
    private JTextField m_kNumLatText = new JTextField("9", 3);

    /** Text box for entering the number of longitude lines. */
    private JTextField m_kNumLonText = new JTextField("13", 3);

    /** LUT Panel based on surface curvature. */
    private JPanelHistoLUT m_kPanelBrainsurfaceFlattenerLUT = null;

    /** Picking button group -- switch between picking correspondence points
     * and the puncture triangle. */
    private ButtonGroup m_kPickButtonGroup = new ButtonGroup();
    /** Correspondence point picking. */
    private JRadioButton m_kPickCorrespondence = new JRadioButton();
    /** Puncture triangle picking. */
    private JRadioButton m_kPickPuncture = new JRadioButton();
    
    /** Brain Surface TriMesh */
    private TriMesh m_kTriangleMesh = null;
    
    /** Brain Surface Renderer. */
    private CorticalAnalysisRender m_kView;
    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;
    
    /** Panel container. */
    private JPanel m_kInsidePanel;

    /** Parent user-interface frame. */
    private VolumeTriPlanarInterface m_kVolumeViewer;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Create the control-panel for the brainsurfaceFlattener interface:
     *
     * @param kView the engine behind the brainsurfaceFlattener and one of the
     * parent frames affected by the interface
     * @param kParentFrame the parent frame for the panel, contains the
     * surfaceRenderer, where the brain model is displayed
     */
    public JPanelBrainSurfaceFlattener_WM(CorticalAnalysisRender kView,
                                          VolumeTriPlanarInterface kParent)
    {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;
        m_kVolumeViewer = kParent;
        m_kView = kView;
        init();
    }


    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
	public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (command.equals("LatLonLines")) {
            m_kView.toggleLatLonLines( m_kLatLonLines.isSelected() );
            m_kVolumeViewer.toggleNode( m_kView.getMeshLines(), m_kLatLonLines.isSelected() );
        }
        /* Toggle the enable picking On/Off: */
        else if (source == m_kPickCorrespondence) {

            if (m_kView.isPuncturePickEnabled()) {
                m_kView.togglePickPuncture();
            }

            m_kView.togglePickCorrespondence();
            m_kVolumeViewer.PickCorrespondence( m_kPickCorrespondence.isSelected() );
        } else if (source == m_kPickPuncture) {

            if (m_kView.isCorrespondencePickEnabled()) {
                m_kView.togglePickCorrespondence();
            }

            m_kView.togglePickPuncture();
            
            m_kVolumeViewer.PickCorrespondence( m_kPickPuncture.isSelected() );
        } else if (source == m_kDisablePick) {

            if (m_kView.isCorrespondencePickEnabled()) {
                m_kView.togglePickCorrespondence();
            }

            if (m_kView.isPuncturePickEnabled()) {
                m_kView.togglePickPuncture();
            }
            m_kVolumeViewer.PickCorrespondence( false );
        }
        /* Remove all picked points from the scene: */
        else if (command.equals("RemovePoints")) {
            m_kView.removePoints();
        }
        /* Remove all picked points from the scene: */
        else if (command.equals("RecalculateConformal")) {

            if (m_bFileLoaded) {
                m_kView.removePoints();
                m_kView.calculateConformal();

                int iNumLat = Integer.parseInt(m_kNumLatText.getText());
                int iNumLon = Integer.parseInt(m_kNumLonText.getText());
                m_kView.setupLatLon(iNumLat, iNumLon);
                if ( m_kDisplayPlane.isSelected() )
                {
                    m_kView.displayPlane();
                }
                else
                {
                    m_kView.displaySphere();
                }
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
            m_kView.inflation();
        }
        /* The number of latitude/longitude lines has changed, update the
         * meshes: */
        else if (command.equals("NumLatChanged") || command.equals("NumLonChanged")) {
            m_kLatLonLines.setSelected(true);

            //m_kParentFrame.removeBranch(m_kView.getMeshLines(), false);

            int iNumLat = Integer.parseInt(m_kNumLatText.getText());
            int iNumLon = Integer.parseInt(m_kNumLonText.getText());
            m_kView.setupLatLon(iNumLat, iNumLon);

            //m_kParentFrame.addBranch(m_kView.getMeshLines(), null, null);
        }
    }

    /**
     * @param kMesh
     * @return Scene-graph node containing the TriMesh lat/lon lines.
     */
    public Node displayCorticalAnalysis( TriMesh kMesh, Vector3f kCenter ) {
        m_kTriangleMesh = kMesh;
        if ( !m_kView.setup(m_kTriangleMesh, kCenter) )
        {
        	return null;
        }

        int iNumLat = Integer.parseInt(m_kNumLatText.getText());
        int iNumLon = Integer.parseInt(m_kNumLonText.getText());
        m_kView.setupLatLon(iNumLat, iNumLon);

        /* If this is the first surface loaded, create and display the
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
        m_bFileLoaded = true;

        /* Display the latitude/longitude lines: */
        m_kLatLonLines.setSelected(true);

        /* Reset the color map to display curvature: */
        m_kView.displayCurvatureColors();

        return m_kView.getMeshLines();
    }

    /**
     * Memory cleanup.
     */
    public void disposeLocal() {
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

        if (m_kLUTImageA != null) {
            m_kLUTImageA = null;
        }

        if (m_kLUTa != null) {
            m_kLUTa = null;
        }
    }


    /**
     * Return ModelImage A.
     * @return  ModelImage A.
     */
    public ModelImage getImageA() {
        return m_kLUTImageA;
    }

    /**
     * Return ModelImage B.
     * @return ModelImage B.
     */
    public ModelImage getImageB() {
        return null;
    }

    /**
     * Return the main control panel.
     * @return  JPanel the main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Resizing the control panel.
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        frameHeight = frameHeight - (40 * 2);
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight));
        scroller.setSize(new Dimension(panelWidth, frameHeight));
        scroller.revalidate();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#setSlice(int)
     */
    @Override
	public void setSlice(int slice) { }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#setTimeSlice(int)
     */
    @Override
	public void setTimeSlice(int tSlice) { }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImageExtents()
     */
    @Override
	public boolean updateImageExtents() {
        return false;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages()
     */
    @Override
	public boolean updateImages() {
        return false;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(boolean)
     */
    @Override
	public boolean updateImages(boolean flag) {
        return false;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(gov.nih.mipav.model.structures.ModelLUT, gov.nih.mipav.model.structures.ModelLUT, boolean, int)
     */
    @Override
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {

        if (LUTa != null) {
            m_kLUTa = LUTa;
            m_kView.setLUTCurvature(m_kLUTa);
            m_kView.displayCurvatureColors();
        }

        return true;
    }
    
    /**
     * Creates a LUT for the surface, where the curvature values are used in
     * the look-up table instead of the ModelImage values:
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
                m_kLUTImageA.set(i, j, fMin + (i / 255.0f * (fMax - fMin)));
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
        m_kInsidePanel.add(brainsurfaceFlattenerLUTPanel, gbc);

        mainPanel.updateUI();
        m_kView.setLUTCurvature(m_kLUTa);
    }
    
    /**
     * Initialize the user-interface, buttons and ActionCommands.
     */
    private void init() {

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;

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
        kContentBox.add(kPanel);
        
        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());
        scroller = new JScrollPane(mainScrollPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        m_kInsidePanel = new JPanel();
        m_kInsidePanel.setLayout(new GridBagLayout());
        m_iGridY = 0;
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        m_kInsidePanel.add(kLatLonContentBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        m_kInsidePanel.add(kPlaneSphereContentBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        m_kInsidePanel.add(kPickContentBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = m_iGridY++;
        m_kInsidePanel.add(kContentBox, gbc);
        
        mainScrollPanel.add(m_kInsidePanel);
        
        /* Create and add the mainPanel: */
        mainPanel = new JPanel();
        mainPanel.add(scroller, BorderLayout.CENTER);
    }
    
    /**
     * Builds the OK button. Sets it internally as well return the just-built button.
     * @return  OK button.
     */
    protected JButton buildOKButton() {
        OKButton = new JButton("OK");
        OKButton.addActionListener(this);

        // OKButton.setToolTipText("Accept values and perform action.");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);

        return OKButton;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#finalize()
     */
    @Override
	protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }
}
