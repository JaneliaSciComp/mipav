package gov.nih.mipav.view.renderer.WildMagic;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;


public class JPanelSurface_WM
        implements ActionListener, ListSelectionListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4600563188022683359L;

    /** The colors for the first six surfaces are fixed.
    private static Color3f[] fixedColor = {
        new Color3f(0.0f, 0.0f, 0.5f), // blue
        new Color3f(0.0f, 0.5f, 0.0f), // green
        new Color3f(0.5f, 0.0f, 0.0f), // red
        new Color3f(0.0f, 0.5f, 0.5f), // cyan
        new Color3f(0.5f, 0.0f, 0.5f), // violet
        new Color3f(0.5f, 0.5f, 0.0f) // yellow
    };
 */
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The area label. */
    private JLabel areaLabel;

    /** Displays the area of triangle. */
    private JTextField areaText;

    /** The color button, which calls a color chooser. */
    private JButton colorButton;

    /** Color chooser for when the user wants to change the color of the surface. */
    private ViewJColorChooser colorChooser;

    /** The color button label. */
    private JLabel colorLabel;

    // Polygon mode selection box:
    /** The polygon mode combo box label. */
    private JLabel comboLabel;

    /** Current surface index being highlighted. */
    private int currentIndex;

    /** Decimate button. */
    private JButton decimateButton;

    // Level of detail labels/slider:
    /** The level of detail slider label. */
    private JLabel detailLabel;

    /** Level of detail slider. */
    private JSlider detailSlider;

    /** The labels below the detail slider. */
    private JLabel[] detailSliderLabels;


    // Picking Behavior:
    /** flag indicates arbitrary clpping bounding frame being picked. */
    private boolean findArbitraryClipping = false;

    /** flag indicates the probe being picked. */
    private boolean findProbe = false;

    /** Save surface button. */
    private JButton levelSButton, levelVButton, levelWButton, levelXMLButton;

    // Paint tool-bar (containted in the SurfacePaint class)

    /** The material options button, which launches the material editor window. */
    private JButton m_kAdvancedMaterialOptionsButton;

    /** Stereo render button, launches the JStereoWindow for viewing the ModelTriangleMesh in stereo:. */
    private JButton m_kStereoButton;

    /** Opens SurfaceTexture dialog:. */
    private JButton m_kSurfaceTextureButton;

    /** ModelImage max dimension: (Extents * resolutions). */
    private float maxBox;

    // Opacity labels/slider
    /** The opacity slider label. */
    private JLabel opacityLabel;

    /** Opacity slider, not enabled yet. */
    private JSlider opacitySlider;

    /** The labels below the opacity slider. */
    private JLabel[] opacitySliderLabels;

    /** The combo box for the polygon mode to display. */
    private JComboBox polygonModeCB;

    // Interface Components in lay-out order:
    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    // For Mouse Recording:
    /** Indicator for the opacity slider moves. */
    private boolean setSurfaceOpacityFlag;

    // Top tool-bar surface smooth, decimate, and save surface options:
    /** Smooth button. */
    private JButton smooth1Button, smooth2Button, smooth3Button;


    /** The directory where a surface file was last loaded/saved. Defaults to MIPAV default directory. */
    private String surfaceDirectoryName;

    // Surface list:
    /** The list box in the dialog for surfaces. */
    private JList surfaceList;

    /** Surface volume opacity. */
    private int surfaceOpacitySlice;

    // Rendering/Picking options check boxes:
    /** Check Box for surface pickable. */
    private JCheckBox surfacePickableCB;


    /** Counter for surface opacity slider moves. */
    private int surfaceSliderCount;

    /** Check Box for surface transparency. */
    private JCheckBox surfaceTransparencyCB;

    /** A list of the surfaces. The elements are of type SurfaceAttributes. */
    private Vector surfaceVector;

    // Mesh statistics: Number of triangles, volume, surface area:
    /** The number of triangles label. */
    private JLabel triangleLabel;

    /** Displays the number of triangles. */
    private JTextField triangleText;

    /** The volume label. */
    private JLabel volumeLabel;

    /** Displays the volume of triangle. */
    private JTextField volumeText;

    private VolumeViewer m_kVolumeViewer = null;
    /** Fonts, same as <code>MipavUtil.font12</code> and <code>MipavUtil.font12B.</code> */
    protected Font serif12, serif12B;

    /** The main control. */
    protected JPanel mainPanel = null;

    private Vector<TriMesh> m_kMeshes = new Vector<TriMesh>();



    //~ Constructors ---------------------------------------------------------------------------------------------------
    public JPanelSurface_WM( VolumeViewer kParent )
    {
        m_kVolumeViewer = kParent;

        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;
        surfaceVector = new Vector();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * static function returns the next default surface color, based on the current number of surfaces displayed. If the
     * number of surfaces is less than the fixedColor.length then fixedColor is the source of the surface color,
     * otherwise a random color is generated.
     *
     * @param   index  the number of the new surface
     *
     * @return  Color4f, the default surface color for the new surface.

    public static Color4f getNewSurfaceColor(int index) {
        Color4f surfaceColor = new Color4f();
        surfaceColor.w = 1.0f;

        if (index < fixedColor.length) {

            // Use the fixed colors for the first six surfaces.
            surfaceColor.x = fixedColor[index].x;
            surfaceColor.y = fixedColor[index].y;
            surfaceColor.z = fixedColor[index].z;
        } else {
            Random randomGen = new Random();

            // Use randomly generated colors for the seventh and
            // later surfaces.
            surfaceColor.x = 0.5f * (1.0f + randomGen.nextFloat());
            surfaceColor.y = 0.5f * (1.0f + randomGen.nextFloat());
            surfaceColor.z = 0.5f * (1.0f + randomGen.nextFloat());
        }

        return surfaceColor;
    }
     */
    /**
     * The override necessary to be an ActionListener. This callback is executed whenever the Add or Remove buttons are
     * clicked, or when the color button or light button is clicked, or when the combo box changes. If the Add button is
     * clicked, a file dialog is launched to allow the user to select new surface meshes to load from disk. If the
     * Remove button is clicked, the currently selected surfaces in the list box are removed from the scene graph.
     *
     * @param  event  The action event.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Add")) {
            addSurface();
        } 
        else if (command.equals("Remove")) {
            removeSurface();
        }

        else if (command.equals("ChangeColor")) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick surface color", new OkColorListener(),
                                                 new CancelListener());
        } /* 
        else if (command.equals("Stereo")) {
            displayStereo(getSelectedSurfaces(surfaceList.getSelectedIndices()));
        } else if (command.equals("ImageAsTexture")) {
            displayImageAsTexture(getSelectedSurfaces(surfaceList.getSelectedIndices()));
        } */ 
        else if (command.equals("AdvancedMaterialOptions")) {
            displayAdvancedMaterialOptions(surfaceList.getSelectedIndices());
        } 
        /*
        else if (command.equals("SurfaceTexture")) {
            parentScene.getParentFrame().actionPerformed(new ActionEvent(m_kSurfaceTextureButton, 0, "SurfaceTexture"));
        } else if (command.equals("ChangeLight")) {
            m_kLightsControl.setVisible(true);
        } else if (command.equals("SurfacePickable")) {
            setPickable(getSelectedSurfaces(surfaceList.getSelectedIndices()));
        } else if (command.equals("backface")) {
            setBackface(getSelectedSurfaces(surfaceList.getSelectedIndices()));
        } */ 
        else if (command.equals("transparency")) {
            setTransparency(surfaceList.getSelectedIndices());
        }
        /*
        else if (command.equals("Clipping") && ((SurfaceRender) parentScene).getDisplayMode3D()) {
            setClipping(getSelectedSurfaces(surfaceList.getSelectedIndices()));
            */
        else if (command.equals("ChangePolyMode")) {
            changePolyMode(polygonIndexToMode(polygonModeCB.getSelectedIndex()));
        }
            /*
        } else if (command.equals("LevelXML") || command.equals("LevelW") || command.equals("LevelS") ||
                       command.equals("LevelV")) {
            FileSurface.saveSurfaces(parentScene.getImageA(), getSelectedSurfaces(surfaceList.getSelectedIndices()),
                                     command);
        } else if (command.equals("Smooth")) {
            smoothSurface(getSelectedSurfaces(surfaceList.getSelectedIndices()), JDialogSmoothMesh.SMOOTH1);
        } else if (command.equals("Smooth2")) {
            smoothSurface(getSelectedSurfaces(surfaceList.getSelectedIndices()), JDialogSmoothMesh.SMOOTH2);
        } else if (command.equals("Smooth3")) {
            smoothSurface(getSelectedSurfaces(surfaceList.getSelectedIndices()), JDialogSmoothMesh.SMOOTH3);
        } else if (command.equals("Decimate")) {
            decimate(getSelectedSurfaces(surfaceList.getSelectedIndices()));
        }
        */
    }


    /**
     * Return the main surface panel.
     *
     * @return  the surface panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }


    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        frameHeight = frameHeight - (40 * 2);
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight));
        scroller.setSize(new Dimension(panelWidth, frameHeight));
        scroller.revalidate();
    }


    /**
     */
    public void stateChanged(ChangeEvent event)
    {
        if (event.getSource() == opacitySlider)
        {
            setTransparency(surfaceList.getSelectedIndices());
        }
    }



    /**
     */
    public void valueChanged(ListSelectionEvent kEvent)
    {
        if ( m_kVolumeViewer != null )
        {
            int[] aiSelected = surfaceList.getSelectedIndices();
            if ( aiSelected.length == 1 )
            {
                DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
                for (int i = 0; i < aiSelected.length; i++) {
                    triangleText.setText(String.valueOf( m_kMeshes.get(aiSelected[i]).GetTriangleQuantity() ) );
                    volumeText.setText(String.valueOf( m_kVolumeViewer.getVolume( (String)kList.elementAt(aiSelected[i]) ) ) );
                    areaText.setText(String.valueOf( m_kVolumeViewer.getSurfaceArea( (String)kList.elementAt(aiSelected[i]) ) ) );              
                }
            }
        }
    }

    /**
     * Build the toolbar.
     */
    private void buildToolBar() {

        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);

        JToolBar toolBar = new JToolBar();
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setFloatable(false);

        smooth1Button = toolbarBuilder.buildButton("Smooth", "Smooth level 1", "sm1");
        smooth2Button = toolbarBuilder.buildButton("Smooth2", "Smooth level 2", "sm2");
        smooth3Button = toolbarBuilder.buildButton("Smooth3", "Smooth level 3", "sm3");
        decimateButton = toolbarBuilder.buildButton("Decimate", "Decimate the surface", "decimate");
        levelSButton = toolbarBuilder.buildButton("LevelS", "Save single level (.sur)", "levelssave");
        levelVButton = toolbarBuilder.buildButton("LevelV", "Save single level (.wrl)", "levelvsave");
        levelWButton = toolbarBuilder.buildButton("LevelW", "Save multi objects (.wrl)", "levelwsave");
        levelXMLButton = toolbarBuilder.buildButton("LevelXML", "Save xml surface (.xml)", "savexml");

        toolBar.add(smooth1Button);
        toolBar.add(smooth2Button);
        toolBar.add(smooth3Button);
        toolBar.add(decimateButton);
        toolBar.add(levelSButton);
        toolBar.add(levelVButton);
        toolBar.add(levelWButton);
        toolBar.add(levelXMLButton);

        JPanel toolBarPanel = new JPanel();
        toolBarPanel.setLayout(new BorderLayout());
        toolBarPanel.add(toolBar, BorderLayout.WEST);
        //toolBarPanel.add(mSurfacePaint.getToolBar(), BorderLayout.SOUTH);

        mainPanel.add(toolBarPanel, BorderLayout.NORTH);
    }

    /**
     * Creates a label in the proper font and color.
     *
     * @param   title  The title of the label.
     *
     * @return  The new label.
     */
    private JLabel createLabel(String title) {
        JLabel label = new JLabel(title);

        label.setFont(serif12);
        label.setForeground(Color.black);

        return label;
    }


    /**
     * This is called when the user chooses a new color for the surface. It changes the color of the surface.
     *
     * @param  color  Color to change surface to.
     */
    private void getColorChange(Color color)
    {
        if ( m_kVolumeViewer != null )
        {
            int[] aiSelected = surfaceList.getSelectedIndices();
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_kVolumeViewer.setColor( (String)kList.elementAt(aiSelected[i]),
                        new ColorRGB( color.getRed()/255.0f, 
                                color.getGreen()/255.0f,
                                color.getBlue()/255.0f ));
            }
        }
    }


    /**
     * Returns an array of SurfaceAttributes based on which surfaces are selected by the user in the surfaceList
     * combo-box. Only surfaces are selected, VOI points are ignored.
     *
     * @param   aiSelected  the list of selected indices in the surfaceList
     *
     * @return  an array of SurfaceAttributes that contains the corresponding list of surfaces from the surfaceVector.
    private SurfaceAttributes[] getSelectedSurfaces(int[] aiSelected) {

        if (aiSelected.length == 0) {
            return null;
        }

        SurfaceAttributes[] selectedSurfaces = new SurfaceAttributes[aiSelected.length];

        for (int i = 0; i < aiSelected.length; i++) {
            selectedSurfaces[i] = (SurfaceAttributes) surfaceVector.get(aiSelected[i]);
        }

        return selectedSurfaces;
    }
     */

    /**
     * Initializes the GUI components.
     */
    private void init() {
        // setSize(400, 256);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());
        buildToolBar();

        // Layout
        //
        // +-----------------------------------+
        // |                       color button|
        // |   surfaceList         opacity     |
        // |                       shininess   |
        // |                       triangles   |
        // |                       LOD slider  |
        // |   add  remove         light button|
        // +-----------------------------------+

        JPanel buttonPanel = new JPanel();

        // buttons for add/remove of surfaces from list
        JButton addButton = new JButton("Add");

        addButton.addActionListener(this);
        addButton.setActionCommand("Add");
        addButton.setFont(serif12B);
        addButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton removeButton = new JButton("Remove");

        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");
        removeButton.setFont(serif12B);
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);

        buttonPanel.add(addButton);
        buttonPanel.add(removeButton);

        // list panel for surface filenames
        surfaceList = new JList(new DefaultListModel());
        surfaceList.addListSelectionListener(this);
        surfaceList.setPrototypeCellValue("aaaaaaaaaaaaaaaa.aaa    ");

        JScrollPane kScrollPane = new JScrollPane(surfaceList);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(buttonPanel, BorderLayout.SOUTH);
        listPanel.setBorder(buildTitledBorder("Surface list"));

        JPanel paintTexturePanel = new JPanel();
        paintTexturePanel.setLayout(new FlowLayout(FlowLayout.LEFT));


        /* Creates the surface paint button, which launches the surface
         * dialog: */
        m_kSurfaceTextureButton = new JButton("Surface Texture");
        m_kSurfaceTextureButton.addActionListener(this);
        m_kSurfaceTextureButton.setActionCommand("SurfaceTexture");
        m_kSurfaceTextureButton.setSelected(false);
        m_kSurfaceTextureButton.setEnabled(false);
        paintTexturePanel.add(m_kSurfaceTextureButton);

        colorButton = new JButton("   ");
        colorButton.setToolTipText("Change surface color");
        colorButton.addActionListener(this);
        colorButton.setActionCommand("ChangeColor");

        colorLabel = new JLabel("Surface color");
        colorLabel.setFont(serif12B);
        colorLabel.setForeground(Color.black);

        JPanel colorPanel = new JPanel();
        colorPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        colorPanel.add(colorButton);
        colorPanel.add(colorLabel);

        /* Creates the advanced material options button, which launches the
         * material editor dialog: */
        m_kAdvancedMaterialOptionsButton = new JButton("Advanced Options");
        m_kAdvancedMaterialOptionsButton.setToolTipText("Change surface material properties");
        m_kAdvancedMaterialOptionsButton.addActionListener(this);
        m_kAdvancedMaterialOptionsButton.setActionCommand("AdvancedMaterialOptions");
        m_kAdvancedMaterialOptionsButton.setEnabled(false);
        colorPanel.add(m_kAdvancedMaterialOptionsButton);

        m_kStereoButton = new JButton("Stereo");
        m_kStereoButton.setToolTipText("Display stereo pair");
        m_kStereoButton.addActionListener(this);
        m_kStereoButton.setActionCommand("Stereo");
        m_kStereoButton.setEnabled(false);
        colorPanel.add(m_kStereoButton);

        // Slider for changing opacity; not currently enabled.
        opacityLabel = new JLabel("Opacity");
        opacityLabel.setFont(serif12B);
        opacityLabel.setForeground(Color.black);

        opacitySliderLabels = new JLabel[3];
        detailSliderLabels = new JLabel[3];
        opacitySliderLabels[0] = createLabel("0");
        opacitySliderLabels[1] = createLabel("50");
        opacitySliderLabels[2] = createLabel("100");
        detailSliderLabels[0] = createLabel("0");
        detailSliderLabels[1] = createLabel("50");
        detailSliderLabels[2] = createLabel("100");

        Hashtable labels = new Hashtable();

        labels.put(new Integer(0), opacitySliderLabels[0]);
        labels.put(new Integer(50), opacitySliderLabels[1]);
        labels.put(new Integer(100), opacitySliderLabels[2]);

        opacitySlider = new JSlider(0, 100, 100);
        opacitySlider.setFont(serif12);
        opacitySlider.setMinorTickSpacing(10);
        opacitySlider.setPaintTicks(true);
        opacitySlider.addChangeListener(this);
        opacitySlider.setLabelTable(labels);
        opacitySlider.setPaintLabels(true);
        opacitySlider.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacityLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacitySlider.setEnabled(true);
        opacityLabel.setEnabled(true);
        surfaceOpacitySlice = opacitySlider.getValue();
        opacitySliderLabels[0].setEnabled(true);
        opacitySliderLabels[1].setEnabled(true);
        opacitySliderLabels[2].setEnabled(true);

        triangleLabel = new JLabel("Number of triangles");
        triangleLabel.setFont(serif12B);
        triangleLabel.setForeground(Color.black);
        triangleLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        triangleText = new JTextField(10);
        triangleText.setEditable(false);
        triangleText.setBorder(new EmptyBorder(triangleText.getInsets()));
        triangleText.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel trianglePanel = new JPanel();

        trianglePanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        trianglePanel.add(triangleLabel);
        trianglePanel.add(triangleText);
        trianglePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        volumeLabel = new JLabel("Volume of mesh");
        volumeLabel.setFont(serif12B);
        volumeLabel.setForeground(Color.black);
        volumeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        volumeText = new JTextField(10);
        volumeText.setEditable(false);
        volumeText.setBorder(new EmptyBorder(volumeText.getInsets()));
        volumeText.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel volumePanel = new JPanel();

        volumePanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        volumePanel.add(volumeLabel);
        volumePanel.add(volumeText);
        volumePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        areaLabel = new JLabel("Surface area");
        areaLabel.setFont(serif12B);
        areaLabel.setForeground(Color.black);
        areaLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        areaText = new JTextField(10);
        areaText.setEditable(false);
        areaText.setBorder(new EmptyBorder(areaText.getInsets()));
        areaText.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel areaPanel = new JPanel();

        areaPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        areaPanel.add(areaLabel);
        areaPanel.add(areaText);
        areaPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Slider for changing level of detail.  Range is [0,100] with initial
        // value 100.
        detailLabel = new JLabel("Level of Detail");
        detailLabel.setFont(serif12B);
        detailLabel.setForeground(Color.black);

        Hashtable labels2 = new Hashtable();

        labels2.put(new Integer(0), detailSliderLabels[0]);
        labels2.put(new Integer(50), detailSliderLabels[1]);
        labels2.put(new Integer(100), detailSliderLabels[2]);

        detailSlider = new JSlider(0, 100, 100);
        detailSlider.setFont(serif12);
        detailSlider.setMinorTickSpacing(10);
        detailSlider.setPaintTicks(true);
        detailSlider.addChangeListener(this);
        detailSlider.setLabelTable(labels2);
        detailSlider.setPaintLabels(true);
        detailSlider.setAlignmentX(Component.LEFT_ALIGNMENT);
        detailLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        detailSlider.setEnabled(false);
        detailLabel.setEnabled(false);
        detailSliderLabels[0].setEnabled(false);
        detailSliderLabels[1].setEnabled(false);
        detailSliderLabels[2].setEnabled(false);

        JPanel sliderPanel = new JPanel();

        sliderPanel.setLayout(new BoxLayout(sliderPanel, BoxLayout.Y_AXIS));
        sliderPanel.add(opacityLabel);
        sliderPanel.add(opacitySlider);
        sliderPanel.add(trianglePanel);
        sliderPanel.add(volumePanel);
        sliderPanel.add(areaPanel);
        sliderPanel.add(detailLabel);
        sliderPanel.add(detailSlider);

        comboLabel = new JLabel("Polygon mode:");
        comboLabel.setFont(serif12B);
        comboLabel.setForeground(Color.black);

        polygonModeCB = new JComboBox(new String[] { "Fill", "Line", "Point" });
        polygonModeCB.addActionListener(this);
        polygonModeCB.setActionCommand("ChangePolyMode");
        polygonModeCB.setAlignmentX(Component.LEFT_ALIGNMENT);
        polygonModeCB.setFont(serif12);
        polygonModeCB.setBackground(Color.white);

        surfacePickableCB = new JCheckBox("Surface Pickable", true);
        surfacePickableCB.addActionListener(this);
        surfacePickableCB.setActionCommand("SurfacePickable");
        surfacePickableCB.setFont(serif12B);
        surfacePickableCB.setSelected(false);
/*
        surfaceClipCB = new JCheckBox("Surface Clipping", true);
        surfaceClipCB.addActionListener(this);
        surfaceClipCB.setActionCommand("Clipping");
        surfaceClipCB.setFont(serif12B);
        surfaceClipCB.setSelected(false);
        surfaceClipCB.setEnabled(false);

        surfaceBackFaceCB = new JCheckBox("Backface Culling", true);
        surfaceBackFaceCB.addActionListener(this);
        surfaceBackFaceCB.setActionCommand("backface");
        surfaceBackFaceCB.setFont(serif12B);
        surfaceBackFaceCB.setSelected(true);
*/
        surfaceTransparencyCB = new JCheckBox("Transparency", true);
        surfaceTransparencyCB.addActionListener(this);
        surfaceTransparencyCB.setActionCommand("transparency");
        surfaceTransparencyCB.setFont(serif12B);
        surfaceTransparencyCB.setSelected(true);


        JPanel cbSurfacePanel = new JPanel();
        cbSurfacePanel.setLayout(new BoxLayout(cbSurfacePanel, BoxLayout.Y_AXIS));
        cbSurfacePanel.add(surfacePickableCB);
        //cbSurfacePanel.add(surfaceClipCB);
        //cbSurfacePanel.add(surfaceBackFaceCB);
        cbSurfacePanel.add(surfaceTransparencyCB);

        JPanel cbPanel = new JPanel();
        cbPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        cbPanel.add(comboLabel);
        cbPanel.add(polygonModeCB);
        cbPanel.add(cbSurfacePanel);

        paintTexturePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        colorPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        sliderPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        cbPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        colorPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        sliderPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        cbPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
        optionsPanel.add(colorPanel);
        optionsPanel.add(paintTexturePanel);
        optionsPanel.add(sliderPanel);
        optionsPanel.add(cbPanel);
        optionsPanel.setBorder(buildTitledBorder("Surface options"));

        JPanel rightPanel = new JPanel();
        rightPanel.setLayout(new BorderLayout());
        rightPanel.add(optionsPanel, BorderLayout.NORTH);

        // distinguish between the swing Box and the j3d Box
        javax.swing.Box contentBox = new javax.swing.Box(BoxLayout.Y_AXIS);

        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(listPanel);
        contentBox.add(rightPanel);

        mainScrollPanel.add(contentBox, BorderLayout.NORTH);

        mainPanel.add(scroller, BorderLayout.CENTER);

        // no surfaces yet, so the elements shouldn't be enabled
        setElementsEnabled(false);

    }


    /**
     * Add surface to the volume image. Calls the FileSurface.openSurfaces function to open a file dialog so the user
     * can choose the surfaces to add.
     */
    public void addSurface() {
        //SurfaceAttributes[] surface = FileSurface.openSurfaces(parentScene.getImageA(), surfaceVector.size());
        TriMesh[] akSurfaces = FileSurface_WM.openSurfaces(m_kVolumeViewer.getImageA(), surfaceVector.size());
        if ( akSurfaces != null )
        {
            m_kVolumeViewer.addSurface(akSurfaces);

            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            int iSize = kList.getSize();
            for ( int i = 0; i < akSurfaces.length; i++ )
            {
                kList.add( iSize + i, akSurfaces[i].GetName() );
                m_kMeshes.add(akSurfaces[i]);
            }
            surfaceList.setSelectedIndex(iSize);
            setElementsEnabled(true);
            //addSurfaces(surface, false);
        }
    }


    /**
     * Adding the surface with specific directory and file name. Called from the ViewJFrameVolumeView class from the
     * JPanelEndoscopy loadingSurface function.
     *
     * @param  dir          directory name
     * @param  surfaceFile  file name
     */
    public void addSurfaces(String dir, File surfaceFile) {
        this.readSurface(dir, surfaceFile, 0.5f);
    }

    /**
     * Adding the surface with specific directory, file name, and surfaceOpacity.
     *
     * @param  dir             directory name
     * @param  surfaceFile     file name
     * @param  surfaceOpacity  opacity
     */
    private void readSurface(String dir, File surfaceFile, float surfaceOpacity) {
        /*
        ViewUserInterface.getReference().setDefaultDirectory(dir);
        surfaceDirectoryName = dir;

        Color4f surfaceColor = getNewSurfaceColor(surfaceVector.size());
        SurfaceAttributes[] surfaces = new SurfaceAttributes[1];
        surfaces[0] = FileSurface.readSurface(parentScene.getImageA(), surfaceFile, surfaceColor);
        addSurfaces(surfaces, false);
        */
    }

    /**
     * The action taken when the Remove button is clicked in the list box. All selected surfaces in the list box are
     * removed from the scene graph.
     *
     * @param  surfaces  the selected surfaces (SurfaceAttributes[]) to be removed.
    private void removeSurfaces(SurfaceAttributes[] surfaces) {
    }
     */

    /**
     * Sets the surface options GUI panel to enabled or disabled. If there are 0 or multiple surfaces selected, all the
     * options should be disabled.
     *
     * @param  flag  Enable or disable.
     */
    private void setElementsEnabled(boolean flag) {
        levelSButton.setEnabled(flag);
        levelVButton.setEnabled(flag);
        levelWButton.setEnabled(flag);
        decimateButton.setEnabled(flag);
        colorButton.setEnabled(flag);
        smooth1Button.setEnabled(flag);
        smooth2Button.setEnabled(flag);
        smooth3Button.setEnabled(flag);
        levelXMLButton.setEnabled(flag);

        colorLabel.setEnabled(flag);
        m_kAdvancedMaterialOptionsButton.setEnabled(flag);
        m_kSurfaceTextureButton.setEnabled(flag);
        m_kStereoButton.setEnabled(flag);
        //detailLabel.setEnabled(flag);
        //detailSlider.setEnabled(flag);
        opacityLabel.setEnabled(flag);
        opacitySlider.setEnabled(flag);
        triangleLabel.setEnabled(flag);
        triangleText.setEnabled(flag);
        volumeLabel.setEnabled(flag);
        volumeText.setEnabled(flag);
        areaLabel.setEnabled(flag);
        areaText.setEnabled(flag);

        //for (int i = 0; i < detailSliderLabels.length; i++) {
        //    detailSliderLabels[i].setEnabled(flag);
        //}

        for (int i = 0; i < opacitySliderLabels.length; i++) {
            opacitySliderLabels[i].setEnabled(flag);
        }

        polygonModeCB.setEnabled(flag);
        comboLabel.setEnabled(flag);
        surfacePickableCB.setEnabled(flag);
        //surfaceClipCB.setEnabled(flag);
        //surfaceBackFaceCB.setEnabled(flag);
        surfaceTransparencyCB.setEnabled(flag);

//         mSurfacePaint.setEnabled(flag);

//         if (((SurfaceRender) parentScene).getSurfaceTexture() != null) {
//             ((SurfaceRender) parentScene).getSurfaceTexture().setEnabled(flag);
//         }
    }


    /**
     * Called when surfaces are added or removed from the surfaceVector SurfaceAttributes list. Updates the surfaceList
     * combo-box displayed in the user-interface.
     *
     * @param  selected  array of names that are currently selected.
     */
    private void updateSurfaceNameList(int[] selected) {
        /*
        Vector surfaceNames = new Vector();

        for (Enumeration en = surfaceVector.elements(); en.hasMoreElements();) {
            surfaceNames.addElement(((SurfaceAttributes) en.nextElement()).getName());
        }

        surfaceList.setListData(surfaceNames);
        surfaceList.setSelectedIndices(selected);
        */
    }

    /**
     * Changes the polygon mode of the selected surface by detaching it, calling the appropriate method, and reattaching
     * it.
     *
     * @param  mode  The new polygon mode to set.
     */
    private void changePolyMode(WireframeState.FillMode mode) {
        int[] aiSelected = surfaceList.getSelectedIndices();

        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_kVolumeViewer.setPolygonMode( (String)kList.elementAt(aiSelected[i]), mode);
            }
        }
    }

    /**
     */
    private void removeSurface()
    {
        int[] aiSelected = surfaceList.getSelectedIndices();

        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        if ( m_kVolumeViewer != null )
        {
            for (int i = 0; i < aiSelected.length; i++) {
                m_kVolumeViewer.removeSurface( (String)kList.elementAt(aiSelected[i]) );
            }
        }
        for (int i = 0; i < aiSelected.length; i++) {
            kList.remove(aiSelected[i]);
            m_kMeshes.remove(aiSelected[i]);
        }
    }

    /**
     * Convert from the polygon mode combo-box list index to the PolygonAttributes.POLYGON_LINE,
     * PolygonAttributes.POLYGON_POINT, and PolygonAttributes.POLYGON_FILL values:
     *
     * @param   index  the index of the selected polygon mode in the polygonModeCB combo box.
     *
     * @return  the corresponding PolygonAttributes defined value.
     */
    private WireframeState.FillMode polygonIndexToMode(int index) {
        switch (index) {

            case 1:
                return WireframeState.FillMode.FM_LINE;

            case 2:
                return WireframeState.FillMode.FM_POINT;

            case 0:
            default:
                return WireframeState.FillMode.FM_FILL;
        }
    }

    /**
     * Turns Transparency on/off for the selected surfaces.
     *
     * @param  surfaces  the list of selected surfaces (SurfaceAttributes)
     */
    private void setTransparency(int[] aiSelected) {

        if ( m_kVolumeViewer != null )
        {
            if ( surfaceTransparencyCB.isSelected() )
            {
                float opacity = opacitySlider.getValue() / 100.0f;
                DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
                for (int i = 0; i < aiSelected.length; i++) {
                    m_kVolumeViewer.setTransparency( (String)kList.elementAt(aiSelected[i]), opacity);
                }
            }
        }
    }

    /**
     */
    private void displayAdvancedMaterialOptions(int[] aiSelected) {
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                new JFrameSurfaceMaterialProperties_WM(this, i, m_kVolumeViewer.GetLights(),
                        m_kVolumeViewer.getMaterial( (String)kList.elementAt(aiSelected[i]) ),
                        m_kVolumeViewer.GetAnimator() );
            }
        }
    }

    /**
     * Called from the JPanelSurfaceMAterialProperties.java dialog when the dialog is used to change the material
     * properties of a surface. The surface is determined by the index iIndex. The color button is set to the Material
     * diffuse color.
     *
     * @param  kMaterial  Material reference
     * @param  iIndex     int material index
     */
    public void setMaterial(MaterialState kMaterial, int iIndex) {
        colorButton.setBackground( new Color(kMaterial.Diffuse.R(), kMaterial.Diffuse.G(), kMaterial.Diffuse.B()) );
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            m_kVolumeViewer.setMaterial( (String)kList.elementAt(iIndex), kMaterial);
        }
    }


    /**
     * Builds a titled border with the given title, an etched border, and the
     * proper font and color.  Changed to public static member so that it can
     * be used for other JPanels not inherited from this base class.
     * @param   title  Title of the border
     *
     * @return  The titled border.
     */
    public static TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                                Color.black);
    }
    
    /**
     * Cancel the color dialog, change nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Do nothing.
         *
         * @param  e  action event
         */
        public void actionPerformed(ActionEvent e) { }
    }


    /**
     * Pick up the selected color and call method to change the surface color.
     */
    class OkColorListener implements ActionListener {

        /**
         * Sets the button color to the chosen color and changes the color of the surface.
         *
         * @param  e  Event that triggered this method.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();

            colorButton.setBackground(color);
            getColorChange(color);
        }
    }
}
