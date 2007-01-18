package gov.nih.mipav.view.renderer.surfaceview;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;

import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * A Java3D-based dialog for surfaces represented as triangle meshes. The
 * surfaces are displayed in the 3D viewer that already contains a 3D
 * image. The mouse behavior is the same for the image as it is for the
 * surfaces, so that when the user zooms or rotates, it looks like the
 * surfaces are part of the tri-image.
 *
 * @author  David Eberly
 * @author  Neva Cherniavsky
 */
public class JPanelSurface extends JPanelRendererBase
    implements ListSelectionListener,
               MouseListener,
               MouseMotionListener,
               ChangeListener
{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4600563188022683359L;

    /** The colors for the first six surfaces are fixed. */
    private static Color3f[] fixedColor = {
        new Color3f(0.0f, 0.0f, 0.5f), // blue
        new Color3f(0.0f, 0.5f, 0.0f), // green
        new Color3f(0.5f, 0.0f, 0.0f), // red
        new Color3f(0.0f, 0.5f, 0.5f), // cyan
        new Color3f(0.5f, 0.0f, 0.5f), // violet
        new Color3f(0.5f, 0.5f, 0.0f) // yellow

    };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Record the current active index of light bulb, which is being picked. */
    private int activeLightBulbIndex;

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

    /** The polygon mode combo box label. */
    private JLabel comboLabel;

    /** Current surface index being highlighted. */
    private int currentIndex;

    /** Decimate button. */
    private JButton decimateButton;

    /** The level of detail slider label. */
    private JLabel detailLabel;

    /** Level of detail slider. */
    private JSlider detailSlider;

    /** The labels below the detail slider. */
    private JLabel[] detailSliderLabels;

    /** flag indicates arbitrary clpping bounding frame being picked. */
    private boolean findArbitraryClipping = false;

    /** flag indicates the probe being picked. */
    private boolean findProbe = false;

    /**
     * If only a single surface is selected in the list box, this member stores the index into the list for the selected
     * item. If no items are selected or if multiple items are selected, the value is -1.
     */
    private int iSelect = -1;

    /** Save surface button. */
    private JButton levelSButton, levelVButton, levelWButton, levelXMLButton;

    /**
     * An array of eight lights that can be used for illuminating the surfaces in the scene. The lights at indices 0 and
     * 7 are initially enabled; the other lights are initially disabled. The default positions for the lights are at the
     * 8 corners of the cube [0,1]^3. Light 0 is at (0,0,0) and light 7 is at (1,1,1), thereby providing suitable
     * lighting for the visible surfaces in the scene. Other lights can be enabled as desired through the light
     * attribute dialog.
     */
    private Light[] lightArray;

    /** The branch groups to which the lights are attached. */
    private BranchGroup[] lightArrayBG;

    /** The structure for the light bulbs. */
    private ViewJComponentLightBulbs lightBulbs;

    /** Surface list panel. */
    private JPanel listPanel;

    /**
     * The index of the lights in JPanelLights is different from the order maintained here. This array maps from our
     * index to JPanelLights' index.
     */
    private final int[] m_aiMapIndexToJPanelLightsIndex;

    /**
     * The index of the lights in JPanelLights is different from the order maintained here. This array maps from
     * JPanelLights' index to our index.
     */
    private final int[] m_aiMapJPanelLightsIndexToIndex;

    /** The description of the lights so they can be duplicated in the "Advanced Material Properties" dialog: */
    private GeneralLight[] m_akLights;

    /** The material options button, which launches the material editor window. */
    private JButton m_kAdvancedMaterialOptionsButton;

    /** Opens TexturePaint dialog: */
    private JButton m_kTexturePaintButton;

    /** Opens SurfaceTexture dialog: */
    private JButton m_kSurfaceTextureButton;
    
    /** For drawing the geodesic lines on the triangle mesh:. */
    private BranchGroup m_kGeodesicGroup = null;

    /** Light dialog for when the user clicks the light button. */
    private JPanelLights m_kLightsControl;

    /** For drawing the flythru path in the triangle mesh:. */
    private TransformGroup m_kPathPositionTG = null;

    /** Stereo render button, launches the JStereoWindow for viewing the ModelTriangleMesh in stereo:. */
    private JButton m_kStereoButton;

    /** DOCUMENT ME! */
    private ModelTriangleMesh[] m_kSurface = null;

    /** Scroll panel that holding the all the control components. */
    private JPanel mainScrollPanel;

    /** The largest of xBox, yBox, and zBox. */
    private float maxBox;

    /** The opacity slider label. */
    private JLabel opacityLabel;

    /** Opacity slider, not enabled yet. */
    private JSlider opacitySlider;

    /** The labels below the opacity slider. */
    private JLabel[] opacitySliderLabels;

    /** The parent object that holds necessary the structures to which the surfaces will be added and modified. */
    private SurfaceRender parentScene;

    // TODO: intensity/attenuation vars not used right now, alexandra may use them in the future though
    /**
     * Java3D does not expose an intensity member for lights, but it does support attenuation for point lights. The
     * linear and quadratic attenuation coefficients are set to zero. The constant attenuation coefficient is then the
     * inverse of the light intensity. To avoid divisions by numbers close to zero, the intensity is clamped to a
     * minimum and maximum intensity.
     */
    // private static float minIntensity = 0.001f;

    /**
     * Java3D does not expose an intensity member for lights, but it does support attenuation for point lights. The
     * linear and quadratic attenuation coefficients are set to zero. The constant attenuation coefficient is then the
     * inverse of the light intensity. To avoid divisions by numbers close to zero, the intensity is clamped to a
     * minimum and maximum intensity.
     */
    // private static float maxIntensity = 100.0f;

    /**
     * Java3D does not expose an intensity member for lights, but it does support attenuation for point lights. The
     * linear and quadratic attenuation coefficients are set to zero. The constant attenuation coefficient is then the
     * inverse of the light intensity. To avoid divisions by numbers close to zero, the intensity is clamped to a
     * minimum and maximum intensity.
     */
    // private float fMinCAttenuate = 1.0f / maxIntensity;

    /**
     * Java3D does not expose an intensity member for lights, but it does support attenuation for point lights. The
     * linear and quadratic attenuation coefficients are set to zero. The constant attenuation coefficient is then the
     * inverse of the light intensity. To avoid divisions by numbers close to zero, the intensity is clamped to a
     * minimum and maximum intensity.
     */
    // private float fMaxCAttenuate = 1.0f / minIntensity;

    /**
     * The PickCanvas object that ties together the canvas and the surfaces branch group to allow pick operations on the
     * scene.
     */
    private PickCanvas pickCanvas;

    /** The combo box for the polygon mode to display. */
    private JComboBox polygonModeCB;

    /** Control panel on the right. */
    private JPanel rightPanel;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    /** Indicator for the opacity slider moves. */
    private boolean setSurfaceOpacityFlag;

    /** Smooth button. */
    private JButton smooth1Button, smooth2Button, smooth3Button;

    /** Sphere showing where the user clicked in the surface. */
    private Sphere sphere;

    /** Branch group parent of the sphere. */
    private BranchGroup sphereBranch;

    /** Flag indicating if the sphere is currently showing. */
    private boolean sphereShowing = false;

    /** Transform group parent of the sphere. */
    private TransformGroup sphereTransform;

    /** Static light hehavior branch group. */
    private BranchGroup staticLightBehaviorBG;

    /** The branch group that holds the static light bulb. */
    private BranchGroup staticLightBG;

    /** The structure for the static light bulb. */
    private ViewJComponentLightBulbs staticLightBulb;

    /** The transform group that holds the static light branch group. */
    private TransformGroup staticLightTG;

    /** Static light translate behavior. */
    private MouseTranslate staticLightTranslate;

    /** Static light zoom behavior. */
    private MouseZoom staticLightZoom;

    /** Surface color. */
    private Color surColor;

    /** Check Box for surface back face culling. */
    private JCheckBox surfaceBackFaceCB;

    /** Check Box for surface clpping of the volume render. */
    private JCheckBox surfaceClipCB;

    /** The directory where a surface file was last loaded/saved. Defaults to MIPAV default directory. */
    private String surfaceDirectoryName;

    /** The list box in the dialog for surfaces. */
    private JList surfaceList;

    /** Surface mask. */
    private SurfaceMask mSurfaceMask = new SurfaceMask();

    /** Surface opacity changes event queue. */
    private MouseEventVector surfaceOpacityEvents;

    /** Surface volume opacity. */
    private int surfaceOpacitySlice;

    /** Check Box for surface pickable. */
    private JCheckBox surfacePickableCB;

    /** The branch group that holds all the surfaces together. */
    private BranchGroup surfaceRootBG;

    /** The transform group to which the model lights are attached. */
    private TransformGroup surfaceRootTG;

    /** Counter for surface opacity slider moves. */
    private int surfaceSliderCount;

    /** A list of the surfaces. The elements are of type SurfaceAttributes. */
    private Vector surfaceVector;

    /** Surface button toolbar. */
    private JToolBar toolBar;

    /** The number of triangles label. */
    private JLabel triangleLabel;

    /** Displays the number of triangles. */
    private JTextField triangleText;

    /** The volume label. */
    private JLabel volumeLabel;

    /** Displays the volume of triangle. */
    private JTextField volumeText;

    /** 3D Texture used to display the ModelImage data as a texture mapped
     * onto the ModelTriangleMesh */
    private Texture3D mTexture = null;

    /** Paint interface/algorithm for allowing the user to interactively paint
     * the vertices of the ModelTriangleMesh */
    private SurfacePaint mSurfacePaint = null;



    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates the Java3D scene graph for the application. The surfaces are all children of the BranchGroup
     * surfaceRootBG, which is in turn a child of the TransformGroup in ViewJFrameTriImage3D. That transform group
     * controls how both the surfaces and the three image planes are displayed; by making the surfaces a child of the
     * transform, the surfaces and image planes move together. The BranchGroup created here is used for the PickCanvas,
     * so that the pick tool ignores the image planes when the mouse is clicked and just looks for the surfaces.
     *
     * <p>This constructor also adds lights to the scene. Because of the texture maps on the image planes, lights don't
     * make a difference in how the image planes are displayed. Therefore, the lights are only necessary for the
     * surfaces.</p>
     *
     * @param  parent       SurfaceRender reference
     * @param  canvas       Canvas3D reference
     * @param  surfaceRoot  TransformGroup root to add surfaces
     * @param  xBox         red box x dimension
     * @param  yBox         red box y dimension
     * @param  zBox         red box z dimension
     */
    public JPanelSurface(SurfaceRender parent, Canvas3D canvas, TransformGroup surfaceRoot,
                         float xBox, float yBox, float zBox)
    {

        super(parent);
        parentScene = parent;
        maxBox = Math.max(xBox, Math.max(yBox, zBox));
        surfaceRootTG = surfaceRoot;

        surfaceRootBG = new BranchGroup();
        surfaceRootBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        surfaceRootBG.setCapability(Group.ALLOW_CHILDREN_READ);
        surfaceRootBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        surfaceRootBG.setCapability(Node.ALLOW_PICKABLE_READ);
        surfaceRootBG.setCapability(Node.ALLOW_PICKABLE_WRITE);
        surfaceRootBG.setCapability(BranchGroup.ALLOW_DETACH);

        staticLightBG = new BranchGroup();
        staticLightBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        staticLightBG.setCapability(Group.ALLOW_CHILDREN_READ);
        staticLightBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        staticLightBG.setCapability(BranchGroup.ALLOW_DETACH);

        staticLightBehaviorBG = new BranchGroup();
        staticLightBehaviorBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        staticLightBehaviorBG.setCapability(Group.ALLOW_CHILDREN_READ);
        staticLightBehaviorBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        staticLightBehaviorBG.setCapability(BranchGroup.ALLOW_DETACH);

        staticLightTG = new TransformGroup();
        staticLightTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        staticLightTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        staticLightTG.setCapability(Group.ALLOW_CHILDREN_READ);
        staticLightTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        staticLightTG.setCapability(BranchGroup.ALLOW_DETACH);
        staticLightTG.addChild(staticLightBG);

        // picking support for linking to the surface attribute window
        pickCanvas = new PickCanvas(canvas, parent.getBranchGroup());

        pickCanvas.setMode(PickTool.GEOMETRY_INTERSECT_INFO);
        pickCanvas.setTolerance(2.0f);

        m_kLightsControl = new JPanelLights(this, parentScene);

        m_aiMapJPanelLightsIndexToIndex = new int[m_kLightsControl.getNumLights()];
        m_aiMapIndexToJPanelLightsIndex = new int[10];
        lightArray = new Light[10];
        lightArrayBG = new BranchGroup[10];

        m_akLights = new GeneralLight[10];

        for (int i = 0; i < m_aiMapJPanelLightsIndexToIndex.length; i++) {
            m_aiMapJPanelLightsIndexToIndex[i] = -1;
        }

        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_MODEL_X0Y0Z0] = 0;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_MODEL_X1Y0Z0] = 1;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_MODEL_X0Y1Z0] = 2;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_MODEL_X1Y1Z0] = 3;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_MODEL_X0Y0Z1] = 4;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_MODEL_X1Y0Z1] = 5;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_MODEL_X0Y1Z1] = 6;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_MODEL_X1Y1Z1] = 7;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_AMBIENT] = 8;
        m_aiMapJPanelLightsIndexToIndex[JPanelLights.LIGHT_INDEX_STATIC] = 9;

        for (int i = 0; i < 10; i++) {

            for (int j = 0; j < m_aiMapJPanelLightsIndexToIndex.length; j++) {

                if (i == m_aiMapJPanelLightsIndexToIndex[j]) {
                    m_aiMapIndexToJPanelLightsIndex[i] = j;

                    break;
                }
            }
        }

        lightBulbs = new ViewJComponentLightBulbs(8, pickCanvas);
        staticLightBulb = new ViewJComponentLightBulbs(1, pickCanvas);

        for (int i = 0; i < 8; i++) {
            setLightAttributes(m_aiMapIndexToJPanelLightsIndex[i]);
        }

        setLightAttributes(m_aiMapIndexToJPanelLightsIndex[9]);

        parent.getSceneRootTG().addChild(surfaceRootBG);
        parent.getBranchGroup().addChild(staticLightTG);

        staticLightTranslate = new MouseTranslate(staticLightTG);
        staticLightTG.addChild(staticLightBehaviorBG);
        staticLightBehaviorBG.addChild(staticLightTranslate);
        staticLightTranslate.setSchedulingBounds(parentScene.bounds);
        staticLightTranslate.setFactor(0.0045);

        staticLightZoom = new MouseZoom(staticLightTG);
        staticLightBehaviorBG.addChild(staticLightZoom);
        staticLightZoom.setSchedulingBounds(parentScene.bounds);
        staticLightZoom.setFactor(0.001);

        // Set up the lights for the objects
        setupLights(surfaceRoot);
        canvas.addMouseListener(this);
        canvas.addMouseMotionListener(this);

        surfaceVector = new Vector();

        Appearance app = new Appearance();
        Material mat = new Material(new Color3f(Color.black), new Color3f(Color.white), new Color3f(Color.black),
                                    new Color3f(Color.black), 80f);

        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);
        app.setCapability(Appearance.ALLOW_MATERIAL_READ);
        app.setCapability(Appearance.ALLOW_MATERIAL_WRITE);
        app.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
        app.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);
        app.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ);
        app.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);

        TransparencyAttributes tap = new TransparencyAttributes();
        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_READ);
        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        app.setTransparencyAttributes(tap);

        PolygonAttributes pAttr = new PolygonAttributes();
        pAttr.setCullFace(PolygonAttributes.CULL_NONE);
        pAttr.setPolygonMode(PolygonAttributes.POLYGON_LINE);
        app.setPolygonAttributes(pAttr);

        sphere = new Sphere(0.1f, Sphere.ENABLE_APPEARANCE_MODIFY, 8, app);
        sphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        sphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        sphere.getShape().setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        sphere.getShape().setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        sphere.getShape().setCapability(Geometry.ALLOW_INTERSECT);
        sphere.getShape().setPickable(false);
        sphere.getShape().setAppearanceOverrideEnable(true);
        sphereTransform = new TransformGroup();
        sphereTransform.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        sphereTransform.setCapability(Group.ALLOW_CHILDREN_READ);
        sphereTransform.setCapability(Group.ALLOW_CHILDREN_WRITE);
        sphereTransform.addChild(sphere);
        sphereBranch = new BranchGroup();
        sphereBranch.setCapability(Group.ALLOW_CHILDREN_READ);
        sphereBranch.setCapability(Group.ALLOW_CHILDREN_WRITE);
        sphereBranch.setCapability(BranchGroup.ALLOW_DETACH);
        sphereBranch.addChild(sphereTransform);

        mSurfacePaint = new SurfacePaint( this, parentScene );
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
        } else if (command.equals("Remove")) {
            removeSurface();
        } else if (command.equals("Transform")) {
            transformSurface();
        } else if (command.equals("ChangeColor")) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick surface color", new OkColorListener(),
                                                 new CancelListener());
        } else if (command.equals("Stereo")) {
            displayStereo( getSelectedSurfaces( surfaceList.getSelectedIndices() ) );
        } else if (command.equals("ImageAsTexture")) {
            if ( ((SurfaceRender)parentScene).getSurfaceTexture() != null )
            {
                displayImageAsTexture( getSelectedSurfaces( surfaceList.getSelectedIndices() ),
                                       ((SurfaceRender)parentScene).getSurfaceTexture().getEnabled() );
            }
        } else if (command.equals("AdvancedMaterialOptions")) {
            displayAdvancedMaterialOptions( getSelectedSurfaces( surfaceList.getSelectedIndices() ) );
        } else if (command.equals("TexturePaint")) {
            if ( m_kTexturePaintButton.isSelected() )
            {
                parentScene.getParentFrame().actionPerformed( new ActionEvent( m_kSurfaceTextureButton, 0, "SurfaceTexture" ) );
            }
            mSurfacePaint.setPickCanvas( pickCanvas );
            mSurfacePaint.setPaintMode( SurfacePaint.TEXTURE );
        } else if (command.equals("SurfaceTexture")) {
            parentScene.getParentFrame().actionPerformed( new ActionEvent( m_kSurfaceTextureButton, 0, "SurfaceTexture" ) );
        } else if (command.equals("ChangeLight")) {

            if (m_kLightsControl == null) {
                m_kLightsControl = new JPanelLights(this, parentScene);
            }

            m_kLightsControl.setVisible(true);
        } else if (command.equals("SurfacePickable")) {
            setPickable( getSelectedSurfaces( surfaceList.getSelectedIndices() ) );
        } else if (command.equals("backface")) {
            setBackface( getSelectedSurfaces( surfaceList.getSelectedIndices() ) );
        } else if (command.equals("Clipping") && ((SurfaceRender) parentScene).getDisplayMode3D()) {
            setClipping( getSelectedSurfaces( surfaceList.getSelectedIndices() ) );
        } else if (command.equals("ChangePolyMode")) {
            changePolyMode( polygonIndexToMode( polygonModeCB.getSelectedIndex() ) );
        } else if (command.equals("LevelXML") ||
                   command.equals("LevelW") ||
                   command.equals("LevelS") ||
                   command.equals("LevelV") )
        {
            FileSurface fileSurface = new FileSurface();
            fileSurface.saveSurfaces( parentScene.getImageA(),
                                      getSelectedSurfaces( surfaceList.getSelectedIndices() ),
                                      command );
        }
        else if (command.equals("saveSurface")) {

            // save surface
            int selected = surfaceList.getSelectedIndex();
            ModelTriangleMesh[] meshes = ((SurfaceAttributes)surfaceVector.get(selected)).getMesh();
            Color4f color = ((SurfaceAttributes)surfaceVector.get(selected)).getColor();
            String fName = ((SurfaceAttributes) surfaceVector.get(selected)).getName();

            FileSurface fileSurface = new FileSurface();
            fileSurface.saveSingleMesh(fName, parentScene.getImageA(), meshes, command.equals("LevelS"), color);

            // smooth surface
            actionPerformed(new ActionEvent(this, 1, "Smooth3"));
        } else if (command.equals("Smooth")) {
            smoothSurface( getSelectedSurfaces( surfaceList.getSelectedIndices() ), JDialogSmoothMesh.SMOOTH1 );
        } else if (command.equals("Smooth2")) {
            smoothSurface( getSelectedSurfaces( surfaceList.getSelectedIndices() ), JDialogSmoothMesh.SMOOTH2 );
        } else if (command.equals("Smooth3")) {
            smoothSurface( getSelectedSurfaces( surfaceList.getSelectedIndices() ), JDialogSmoothMesh.SMOOTH3 );
        } else if (command.equals("Decimate")) {
            decimate( getSelectedSurfaces( surfaceList.getSelectedIndices() ) );
        }

    }

    /** Returns an array of SurfaceAttributes based on which surfaces are
     * selected by the user in the surfaceList combo-box. Only surfaces are
     * selected, VOI points are ignored.
     * @param aiSelected, the list of selected indices in the surfaceList
     * @return an array of SurfaceAttributes that contains the corresponding
     * list of surfaces from the surfaceVector.
     */
    private SurfaceAttributes[] getSelectedSurfaces( int[] aiSelected )
    {
        int surfaceCount = 0;
        for (int i = 0; i < aiSelected.length; i++) {
            int iIndex = aiSelected[i];
            if (!((SurfaceAttributes) surfaceVector.get(iIndex)).getIsVOIPt())
            {
                surfaceCount++;
            }
        }
        if ( surfaceCount == 0 )
        {
            return null;
        }

        SurfaceAttributes[] selectedSurfaces = new SurfaceAttributes[ surfaceCount ];
        surfaceCount = 0;
        for (int i = 0; i < aiSelected.length; i++) {
            int iIndex = aiSelected[i];
            if (!((SurfaceAttributes) surfaceVector.get(iIndex)).getIsVOIPt()) {
                selectedSurfaces[ surfaceCount++ ] = (SurfaceAttributes) surfaceVector.get(iIndex);
            }
        }
        return selectedSurfaces;
    }

    /**
     * Returns a list of all the surfaces loaded in the scene graph.
     * @return a list of all the surfaces loaded in the scene graph.
     */
    private SurfaceAttributes[] getAllSurfaces( )
    {
        int surfaceCount = 0;
        for (int i = 0; i < surfaceVector.size(); i++) {
            if (!((SurfaceAttributes) surfaceVector.get(i)).getIsVOIPt())
            {
                surfaceCount++;
            }
        }
        if ( surfaceCount == 0 )
        {
            return null;
        }

        SurfaceAttributes[] selectedSurfaces = new SurfaceAttributes[ surfaceCount ];
        surfaceCount = 0;
        for (int i = 0; i < surfaceVector.size(); i++) {
            if (!((SurfaceAttributes) surfaceVector.get(i)).getIsVOIPt()) {
                selectedSurfaces[ surfaceCount++ ] = (SurfaceAttributes) surfaceVector.get(i);
            }
        }
        return selectedSurfaces;
    }

    /**
     * Displays the selected surfaces with the ModelImage as a 3D texture map
     * on the surface triangle mesh.
     * @param surfaces, the list of selected surfaces (SurfaceAttributes)
     * @param asImage, when true displays the ModelImage as a texture, when false, disables the 3D texture.
     */
    private void displayImageAsTexture(  SurfaceAttributes[] surfaces, boolean asImage )
    {
        if ( surfaces != null )
        {
            for ( int i = 0; i < surfaces.length; i++ )
            {
                if ( !asImage )
                {
                    colorButton.setBackground( surfaces[i].getColor().get() );
                }
                Shape3D[] shapes = surfaces[i].getShape();
                for ( int j = 0; j < shapes.length; j++ )
                {
                    TextureUnitState[] textureUnitState = shapes[j].getAppearance().getTextureUnitState();
                    if ( (textureUnitState != null) && (textureUnitState.length > 0) )
                    {
                        textureUnitState[0].getTexture().setImage(0, ((SurfaceRender)parentScene).getSurfaceTexture().getSurfaceTextureImage()) ;
                        textureUnitState[0].getTexture().setEnable( asImage );
                    }
                }
            }
        }
    }

    /**
     * Turns picking on/off for the selected surfaces.
     * @param surfaces, the list of selected surfaces (SurfaceAttributes)
     */
    private void setPickable( SurfaceAttributes[] surfaces )
    {
        if ( surfaces != null )
        {
            for ( int i = 0; i < surfaces.length; i++ )
            {
                BranchGroup root = surfaces[i].getBranch();
                root.setPickable(surfacePickableCB.isSelected());
            }
        }
    }

    /**
     * Turns BackFace Culling on/off for the selected surfaces.
     * @param surfaces, the list of selected surfaces (SurfaceAttributes)
     */
    private void setBackface( SurfaceAttributes[] surfaces )
    {
        if ( surfaces != null )
        {
            for ( int i = 0; i < surfaces.length; i++ )
            {
                Shape3D[] shapes = surfaces[i].getShape();
                for ( int j = 0; j < shapes.length; j++ )
                {
                    if (surfaceBackFaceCB.isSelected()) {
                        shapes[j].getAppearance().getPolygonAttributes().setCullFace(PolygonAttributes.CULL_BACK);
                    } else {
                        shapes[j].getAppearance().getPolygonAttributes().setCullFace(PolygonAttributes.CULL_NONE);
                    }
                }
            }
        }
    }

    /**
     * Turns Clipping on/off for the selected surfaces.
     * @param surfaces, the list of selected surfaces (SurfaceAttributes)
     */
    private void setClipping( SurfaceAttributes[] surfaces )
    {
        if ( surfaces != null )
        {
            for ( int i = 0; i < surfaces.length; i++ )
            {
                BranchGroup root = surfaces[i].getBranch();

                if (root != null) {
                    root.setAlternateCollisionTarget(surfaceClipCB.isSelected());
                    if (surfaceClipCB.isSelected()) {
                        parentScene.getClipDialog().addToModelClip(root);
                    } else {
                        parentScene.getClipDialog().removeFromModelClip(root);
                    }
                }
            }
        }
    }

    /**
     * For each file in the selected list, launch the stereo viewer:
     * @param surfaces, the list of selected surfaces (SurfaceAttributes)
     */
    private void displayStereo( SurfaceAttributes[] surfaces )
    {
        if ( surfaces != null )
        {
            for (int i = 0; i < surfaces.length; i++) {
                /* Use the current view transform: */
                Transform3D kTransform = new Transform3D();
                parentScene.getSceneRootTG().getTransform(kTransform);

                /* Pass in a new copy of the triangle mesh: */
                ModelTriangleMesh[] kMesh = surfaces[i].getMesh();
                for ( int j = 0; j < kMesh.length; j++ )
                {
                    new JStereoWindow( surfaces[i], kTransform );
                }
            }
        }
    }

    /**
     * For each file in the selected list, launch the AdvancedMaterialOptions dialog:
     * @param surfaces, the list of selected surfaces (SurfaceAttributes)
     */
    private void displayAdvancedMaterialOptions( SurfaceAttributes[] surfaces )
    {
        if ( surfaces != null )
        {
            for (int i = 0; i < surfaces.length; i++) {
                Material material = surfaces[i].getMaterial();
                float fOpacity = surfaces[i].getOpacity();

                /* Launch the material editor: */
                new JFrameSurfaceMaterialProperties(this, i, m_akLights, fOpacity, material);
            }
        }
    }

    /**
     * Smoothes the selected surfaces. One dialog per group of selected
     * surfaces is displayed (not a different dialog per-serface).
     *
     * @param surfaces, the list of selected surfaces (SurfaceAttributes)
     * @param smoothType, the level of smoothing JDialogSmoothMesh.SMOOTH1,
     * JDialogSmoothMesh.SMOOTH2, or JDialogSmoothMesh.SMOOTH3
     */
    private void smoothSurface( SurfaceAttributes[] surfaces, int smoothType )
    {
        if ( surfaces == null )
        {
            MipavUtil.displayError("Select a surface to smooth.");
            return;
        }

        JDialogSmoothMesh dialog = new JDialogSmoothMesh(((SurfaceRender) renderBase).getParentFrame(), true, smoothType);
        if (dialog.isCancelled()) {
            return;
        }
        for ( int i = 0; i < surfaces.length; i++ )
        {
            ModelTriangleMesh[] meshes = surfaces[i].getMesh();
            BranchGroup root = surfaces[i].getBranch();
            surfaceRootBG.removeChild(root);

            ModelClodMesh clod = null;
            int numTriangles = 0;
            float volume = 0;
            float area = 0;

            for (int j = 0; j < meshes.length; j++) {
                if ( surfaces[i].getIsClodMesh() == true )
                {
                    clod = (ModelClodMesh) meshes[j].getGenerator();
                    clod.setLOD(clod.getMaximumLOD());
                    meshes[j] = clod.getMesh();
                }

                if ( smoothType == JDialogSmoothMesh.SMOOTH1 )
                {
                    meshes[j].smoothMesh( dialog.getIterations(), dialog.getAlpha(), dialog.getVolumeLimit(), dialog.getVolumePercent(), true);
                }
                else if ( smoothType == JDialogSmoothMesh.SMOOTH2 )
                {
                    meshes[j].smoothTwo(dialog.getIterations(), dialog.getStiffness(),
                                        dialog.getVolumeLimit(), dialog.getVolumePercent(), true);
                }
                else
                {
                    meshes[j].smoothThree(dialog.getIterations(), dialog.getLambda(), dialog.getMu(), true);
                }

                meshes[j].computeNormals();

                if ( surfaces[i].getIsClodMesh() == true )
                {
                    clod.setVerticies(meshes[j].getVertexCopy());
                }

                numTriangles += meshes[j].getIndexCount();
                volume += meshes[j].volume();
                area += meshes[j].area();
            }

            surfaces[i].setMesh( meshes );
            surfaces[i].setVolume( volume );
            surfaces[i].setArea( area );
            surfaces[i].setNumberTriangles( numTriangles / 3 );

            createSurface( surfaces[i], false );
            mSurfaceMask.maskInsideVoxels( surfaceVector.size(),
                                           parentScene.getImageA(),
                                           surfaces[i].getMesh(), false, false, false,
                                           surfaces[i].getOpacity(), surfaces[i].getColor());
            surfaces[i].setMask( mSurfaceMask.getVolumeMask() );

            if (((SurfaceRender) parentScene).getDisplayMode3D()) {
                surfaceClipCB.setSelected(false);
            }
            if ( surfaces[i].getIsClodMesh() == true )
            {
                detailSlider.setValue( surfaces[i].getLevelDetail() );
                stateChanged(new ChangeEvent(detailSlider));
            }

            triangleText.setText(String.valueOf(numTriangles / 3));

            // One length across the extracted surface changes from -1 to 1
            // while one length across the actual volume changes by ((dim-1)*res)max
            volumeText.setText(String.valueOf(volume * maxBox * maxBox * maxBox / 8.0f));
            areaText.setText(String.valueOf(area * maxBox * maxBox));
        }
    }


    /**
     * Adding the surface with specific directory and file name.
     *
     * @param   dir   directory name
     * @param   file  file name
     */
    public void addSurfaces(String dir, File surfaceFile)
    {
        addSurfaces( dir, surfaceFile, 0.5f );
    }

    /**
     * Adding the surface with specific directory, file name, and surfaceOpacity.
     *
     * @param   dir   directory name
     * @param   file  file name
     * @param   surfaceOpacity opacity
     */
    public void addSurfaces(String dir, File surfaceFile, float surfaceOpacity ) {

        ViewUserInterface.getReference().setDefaultDirectory(dir);
        surfaceDirectoryName = dir;

        String surfaceName = surfaceFile.getName();
        int index = surfaceVector.size();
        Color4f surfaceColor = getNewSurfaceColor( index );

        FileSurface fileSurface = new FileSurface();
        SurfaceAttributes[] surfaces = new SurfaceAttributes[1];
        surfaces[0] = fileSurface.addSurface( parentScene.getImageA(), surfaceFile, surfaceColor, surfaceName, surfaceOpacity, -1, true);
        addSurfaces( surfaces, false );
    }

    /**
     * Add any attached surfaces the current image has in its file info (if
     * the file info is in the xml format).
     */
    public void addAttachedSurfaces() {

        // get any surfaces attached to the image we're working with
        FileInfoBase fInfo = parentScene.getImageA().getFileInfo(0);

        if ((fInfo != null) && (fInfo instanceof FileInfoImageXML)) {
            FileInfoImageXML fileInfo = (FileInfoImageXML) fInfo;
            Enumeration e = fileInfo.getSurfaceKeys();
            Vector addedNames = new Vector();

            while (e.hasMoreElements()) {
                String surfaceFileName = (String) e.nextElement();
                File surfaceFile = new File(surfaceFileName);

                if ((surfaceFile == null) || !surfaceFile.exists()) {
                    String originalSurfaceFileName = surfaceFileName;

                    surfaceFileName = parentScene.getImageA().getUserInterface().getDefaultDirectory() +
                                      File.separator + surfaceFile.getName();
                    Preferences.debug("Failed loading surface: " + originalSurfaceFileName);
                    Preferences.debug("Trying to auto-load surface: " + surfaceFileName);
                    surfaceFile = new File(surfaceFileName);

                    if ((surfaceFile == null) || !surfaceFile.exists()) {
                        Preferences.debug("Failed loading surface: " + surfaceFileName);

                        continue;
                    }

                    // replace the missing surface since we were able to find it
                    fileInfo.addSurface(surfaceFileName);
                    fileInfo.getSurface(surfaceFileName).setOpacity(fileInfo.getSurface(originalSurfaceFileName).getOpacity());
                    fileInfo.getSurface(surfaceFileName).setDisplay(fileInfo.getSurface(originalSurfaceFileName).getDisplay());
                    fileInfo.removeSurface(originalSurfaceFileName);
                }

                if (!addedNames.contains(surfaceFile.getName())) {
                    addedNames.add( surfaceFile.getName() );
                    addSurfaces( surfaceFile.getParent(), surfaceFile, fileInfo.getSurface(surfaceFileName).getOpacity() );
                } else {
                    MipavUtil.displayError("Cannot add two surfaces with the same name.");
                }
            }
        }
    }

    /**
     * static function returns the next default surface color, based on the
     * current number of surfaces displayed. If the number of surfaces is less
     * than the fixedColor.length then fixedColor is the source of the surface
     * color, otherwise a random color is generated.
     * @param index, the number of the new surface
     * @return Color4f, the default surface color for the new surface.
     */
    public static Color4f getNewSurfaceColor( int index )
    {
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


    /**
     * Adds a BranchGroup to the display.
     *
     * @param  kBranch      BranchGroup branch group
     * @param  kMesh        ModelTriangleMesh surface mesh
     * @param  kMeshCenter  Point3f center of mass
     */
    public void addBranch(BranchGroup kBranch, ModelTriangleMesh kMesh, Point3f kMeshCenter) {
        surfaceRootBG.addChild(kBranch);

        /* If a new mesh is added to the surRenderer, then pass the mesh
         * information to the PlaneRender objects: */
        if (kMesh != null) {
            ModelTriangleMesh[] kSurface = new ModelTriangleMesh[1];
            kSurface[0] = kMesh;
            surColor = Color.RED;


            /* Tell the surfaceRenderer to add the triangle mesh surface: */
            ((SurfaceRender) renderBase).updateData();

            int numTriangles = kMesh.getIndexCount();
            float volume = kMesh.volume();
            float area = kMesh.area();
            SurfaceAttributes surface = new SurfaceAttributes( kSurface, "", "branch", numTriangles, volume, area, kMesh.center());
            surface.setBranch( kBranch );

            /** mask the surface added. */
            mSurfaceMask.maskInsideVoxels( surfaceVector.size(), parentScene.getImageA(),
                                           surface.getMesh(), true, false, false, 1.0f, surface.getColor() );
            surface.setMask( mSurfaceMask.getVolumeMask() );

            surfaceVector.add(surface);
            triangleText.setText(String.valueOf(numTriangles));
            volumeText.setText(String.valueOf(volume));
            areaText.setText(String.valueOf(area));

            /* Add to the surfaceList: */
            int iNameIndex = 0;
            int iIndex = 0;
            Vector surfaceNames = new Vector();

            for (Enumeration en = surfaceVector.elements(); en.hasMoreElements();) {
                String kElementName = ((SurfaceAttributes) en.nextElement()).getName();

                surfaceNames.addElement(kElementName);

                if (kElementName.equals("branch")) {
                    iNameIndex = iIndex;
                }

                iIndex++;
            }

            surfaceList.setListData(surfaceNames);
            surfaceList.setSelectedIndex(iNameIndex);
        }

    }

    /**
     * Adds the flythru path shape and current view position to the display.
     *
     * @param  kPathShape  the flythru path geometry
     * @param  kPosition   the current viewpoint.
     */
    public void addFlightPath(Shape3D kPathShape, Point3f kPosition) {
        Shape3D kSphere = new Sphere(.01f).getShape();

        kSphere.getAppearance().getMaterial().setDiffuseColor(new Color3f(1, 0, 0));
        kSphere.setPickable(false);

        Transform3D kViewTransform = new Transform3D();

        kViewTransform.set(new Vector3f(kPosition));
        m_kPathPositionTG = new TransformGroup();
        m_kPathPositionTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        m_kPathPositionTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        m_kPathPositionTG.setTransform(kViewTransform);
        m_kPathPositionTG.addChild(kSphere.cloneTree());

        TransformGroup kPathTransformGroup = new TransformGroup(new Transform3D());

        kPathTransformGroup.addChild(kPathShape);

        BranchGroup kPathBranchGroup = new BranchGroup();

        kPathBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kPathBranchGroup.addChild(kPathTransformGroup);
        kPathBranchGroup.addChild(m_kPathPositionTG);
        kPathBranchGroup.compile();

        parentScene.getSceneRootTG().addChild(kPathBranchGroup);
    }

    /**
     * Called when a mesh is changed by the Geodesic class, when a mesh is cut along the geodesic curve it may be
     * divided into two or more meshes. This function allows the Geodesic class to add meshes to the scene renderer. It
     * follows the pattern of adding a surface through the surface dialog.
     *
     * @param  kOld   ModelTriangleMesh old mesh
     * @param  kNew   ModelTriangleMesh new mesh
     * @param  kName  String mesh name
     */
    public void addMesh(ModelTriangleMesh kOld, ModelTriangleMesh kNew, String kName) {

        SurfaceAttributes[] surface = containsMesh( kOld );
        if (surface == null)
        {
            return;
        }

        /* Determine the surface color: */
        Color4f surfaceColor = getNewSurfaceColor( surfaceVector.size() );
        /* setup the mesh array: */
        ModelTriangleMesh[] akComponent = new ModelTriangleMesh[1];
        akComponent[0] = kNew;
        /* Create a new SurfaceAttributes: */
        int iNumTriangles = kNew.getIndexCount() / 3;
        SurfaceAttributes[] surfaces = new SurfaceAttributes[1];
        surfaces[0] = new SurfaceAttributes( akComponent, "", kName, iNumTriangles, kNew.volume(), kNew.area(), kNew.center() );
        surfaces[0].setColor( surfaceColor );
        /* Add the new surface: */
        addSurfaces( surfaces, true );
    }

    /**
     * containsMesh returns the SurfaceAttributes[] containing the input
     * ModelTriangleMesh paramter, where the SurfaceAttributes[] array is of
     * length 1. If the input mesh is not in the scene graph, then null is returned.
     * @param kOld, the ModelTriangleMesh that is being searched for in the scene graph
     * @return SurfaceAttributes[1], an array of 1 containing the
     * SurfaceAttributes describing the input mesh, null if the mesh is not
     * found.
     */
    private SurfaceAttributes[] containsMesh( ModelTriangleMesh kOld )
    {
        for (int i = 0; i < surfaceList.getModel().getSize(); i++) {
            BranchGroup kSurfaceBG = ((SurfaceAttributes) surfaceVector.get(i)).getBranch();

            for (Enumeration e = kSurfaceBG.getAllChildren(); e.hasMoreElements();) {
                Object kObj = e.nextElement();

                if (kObj instanceof Shape3D) {
                    Shape3D kShape = (Shape3D) kObj;

                    if (kShape.getGeometry() instanceof ModelTriangleMesh) {
                        ModelTriangleMesh kChildMesh = (ModelTriangleMesh) (kShape.getGeometry());

                        if (kChildMesh == kOld) {
                            SurfaceAttributes[] surfaces = new SurfaceAttributes[1];
                            surfaces[0] = ((SurfaceAttributes) surfaceVector.get(i));
                            return surfaces;
                        }
                    }
                }
            }
        }
        return null;
    }


    /**
     * Add surface to the volume image. Calls the FileSurface.openSurfaces
     * function to open a file dialog so the user can choose the surfaces to
     * add.
     */
    public void addSurface() {

        FileSurface fileSurface = new FileSurface();
        SurfaceAttributes[] surface = fileSurface.openSurfaces( parentScene.getImageA(), surfaceVector.size() );
        addSurfaces( surface, false );
    }

    /**
     * Adds an array of surfaces described by their SurfaceAttributes to the scene graph.
     * @param surfaces, the new surfaces (SurfaceAttributes[]) to add to the scene graph.
     * @param pickable, when true turn picking on for the new surfaces, when
     * false disable picking.
     */
    public void addSurfaces( SurfaceAttributes[] surfaces, boolean pickable )
    {
        if ( surfaces == null )
        {
            return;
        }
        int[] selected = new int[ surfaces.length ];
        for ( int i = 0; i < surfaces.length; i++ )
        {
            if (  surfaces[i] != null )
            {
                createSurface( surfaces[i], pickable );
                mSurfaceMask.maskInsideVoxels( surfaceVector.size(),
                                               parentScene.getImageA(),
                                               surfaces[i].getMesh(), false, false, false,
                                               surfaces[i].getOpacity(), surfaces[i].getColor());
                surfaces[i].setMask( mSurfaceMask.getVolumeMask() );

                surfaceVector.add( surfaces[i] );
                if (surfaceVector.size() == 1 )
                {
                    setElementsEnabled(true);
                }
                selected[i] = surfaceVector.indexOf( surfaces[i] );

                // add the surface to the image's file info if it is XML (so it can be saved)
                if (parentScene.getParentFrame().getImageOriginal().getFileInfo()[0] instanceof FileInfoImageXML) {
                    for (int s = 0; s < parentScene.getParentFrame().getImageOriginal().getExtents()[2]; s++) {
                        ((FileInfoImageXML) parentScene.getParentFrame().getImageOriginal().getFileInfo()[s]).addSurface(surfaceDirectoryName +
                                                                                                                         surfaces[i].getName());
                    }
                }
            }
        }
        /* Update the surfaceList combo box list of names: */
        updateSurfaceNameList( selected );

        /* Tell the surfaceRenderer to add the triangle mesh
         * surface: */
        ((SurfaceRender) renderBase).updateData();
        ((SurfaceRender) renderBase).getGeodesicPanel().setEnabled(true);

        if (((SurfaceRender) renderBase).getProbeDialog() != null) {
            ((SurfaceRender) renderBase).getProbeDialog().updateTargetList();
        }
    }


    /**
     * Dispose memory.
     */
    public void dispose() {
        parentScene = null;
        surfaceVector = null;
        levelSButton = null;
        levelVButton = null;
        levelWButton = null;
        decimateButton = null;
        polygonModeCB = null;
        surfaceList = null;
        colorButton = null;
        colorLabel = null;
        m_kAdvancedMaterialOptionsButton = null;
        m_kTexturePaintButton = null;
        m_kSurfaceTextureButton = null;
        m_kStereoButton = null;
        opacityLabel = null;
        triangleLabel = null;
        volumeLabel = null;
        areaText = null;
        detailLabel = null;
        comboLabel = null;
        detailSliderLabels = null;
        opacitySliderLabels = null;
        triangleText = null;
        volumeText = null;
        detailSlider = null;
        opacitySlider = null;
        colorChooser = null;
        m_kLightsControl = null;
        surfacePickableCB = null;
        surfaceClipCB = null;
        surfaceBackFaceCB = null;
        listPanel = null;
        rightPanel = null;
        lightArray = null;
        pickCanvas = null;
        surfaceRootBG = null;
        staticLightBG = null;
        staticLightTG = null;
        lightBulbs = null;
        staticLightBulb = null;
        sphere = null;
        sphereTransform = null;
        sphereBranch = null;
        staticLightBehaviorBG = null;
        staticLightTranslate = null;
        staticLightZoom = null;
        surfaceOpacityEvents = null;
        surfaceDirectoryName = null;
    }

    /**
     * Locate the surface that corresponds to a picking operation in the viewer window. This member is called via the
     * mouse listener callback. If a surface is picked, the list box is updated to show that the surface is selected and
     * the corresponding information is set for the color button, the detail slider, and the number of triangles.
     *
     * @param   kEvent  The mouse event generated by the picking operation.
     *
     * @return  The picked surface, or null if no surface is located at the pick location.
     */
    public PickResult doPick(MouseEvent kEvent) {
        PickResult kResult;

        try {
            pickCanvas.setShapeLocation(kEvent);
            kResult = pickCanvas.pickClosest();
        } catch (NullPointerException e) {
            return null;
        }

        if (kResult != null) {
            return kResult;
        } else {
            return null;
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener. The surface attribute dialog was added as a listener to the
     * 3D canvas on which the scene is drawn. Whenever a picking operation occurs on the canvas, this member function is
     * called. The pick operation returns the Shape3D object (null, if no object is situated at the pick location). The
     * root of the subtree for the picked surface is looked up within the surfaces list. If found, the list box, color
     * button, detail slider, triangle text, and polygon mode are updated to display the selected surface.<br>
     * <br>
     * <b>Note</b>. The use of Shape3D by the picker and BranchGroup by the surfaces list appears to be more complicated
     * than necessary, and instead both should use Shape3D or BranchGroup. In fact it is necessary to structure the code
     * as it currently is. The picker needs to locate actual geometry, so Shape3D objects must enable themselves to be
     * picked with ray-triangle intersection testing. If the surfaces in the list were set up to be Shape3D instead of
     * BranchGroup, then all add/remove operations would have to be based on Shape3D, not on BranchGroups. These
     * operations are not allowed in compiled scene graphs.<br>
     * <br>
     * <b>Note.</b> The iteration over the surface list checking for the BranchGroup whose child is the picked surface
     * is also necessary. Simpler would be to query the Shape3D object for its parent. Such a query is also not
     * supported in a compiled scene graph.
     *
     * @param  kEvent  The mouse event.
     */
    public void findPickedObject(MouseEvent kEvent) {

        if (!kEvent.isShiftDown() && parentScene.getParentFrame().isGeodesicEnable() ) {
            return;
        }
        if ( mSurfacePaint.getEnabled() || m_kTexturePaintButton.isSelected() )
        {
            return;
        }

        // locate the surface that was picked
        PickResult pickedObject;
        int iPickedLight = -1;

        sphereBranch.detach();
        sphereShowing = false;

        try {
            pickedObject = doPick(kEvent);
        } catch (CapabilityNotSetException error) {
            return;
        }

        if (pickedObject == null) {
            sphereBranch.detach();
            sphereShowing = false;

            return;
        }

        /* Pass the picked point to the brainsurfaceFlattenerRenderer: */
        if (kEvent.isControlDown() && parentScene.getParentFrame().isBrainSurfaceFlattenerPickEnabled()) {

            /* Pick the first intersection since we executed a pick
             * closest. */
            PickIntersection kPick = pickedObject.getIntersection(0);

            /* Get the coordinates of the picked point on the mesh. */
            Point3f kPickPoint = new Point3f(kPick.getPointCoordinates());

            /* Get the triangle indices of the triangle that that
             * pickPoint falls in:  */
            int[] aiIndex = kPick.getPrimitiveCoordinateIndices();

            parentScene.getParentFrame().drawBrainSurfaceFlattenerPoint(kPickPoint, aiIndex, 2);
        }

        Shape3D pickedShape = (Shape3D) pickedObject.getObject();
        int iIndex;

        findArbitraryClipping = false;

        // Find ArbitraryClipping plane
        if ((parentScene.getClipDialog() != null) && ((SurfaceRender) renderBase).getDisplayMode3D()) {
            findArbitraryClipping = parentScene.getClipDialog().isArbitraryClipping(pickedShape);

            if (findArbitraryClipping) {
                return;
            }
        }

        // Find picked surface
        for (iIndex = 0; iIndex < surfaceVector.size(); iIndex++) {
            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).getBranch();
            boolean bRootFound = false;

            for (int i = 0; i < root.numChildren(); i++) {

                if (root.getChild(i) == pickedShape) {
                    bRootFound = true;

                    break;
                }
            }

            if (bRootFound) {
                break;
            }
        }

        // Find picked light bulb
        boolean foundLightBulb = false;

        if (iIndex >= surfaceVector.size()) {
            iPickedLight = lightBulbs.getPickedLight(pickedShape);

            if (iPickedLight >= 0) {
                foundLightBulb = true;
            } else {
                iPickedLight = staticLightBulb.getPickedLight(pickedShape);

                if (iPickedLight >= 0) {
                    iPickedLight = 9;
                    foundLightBulb = true;
                }
            }
        }

        findProbe = false;

        /** find probe */
        if (((SurfaceRender) renderBase).getProbeDialog() != null) {
            findProbe = ((SurfaceRender) renderBase).getProbeDialog().getProbeBase().findProbe(pickedShape);

            if (findProbe) {
                return;
            }
        }

        // pick burn sphere

        if (((SurfaceRender) renderBase).getProbeDialog() != null) {
            BranchGroup burnRoot = ((SurfaceRender) renderBase).getProbeDialog().getBurnRootParentBG();

            int findIndex = ((SurfaceRender) renderBase).getProbeDialog().getBurnBase().findBurnPoint(pickedShape);

            if (findIndex != -1) {
                ((SurfaceRender) renderBase).getProbeDialog().updateBurnList(findIndex);

                PickIntersection pi = pickedObject.getClosestIntersection(pickCanvas.getStartPosition());
                Point3d point = pi.getClosestVertexCoordinates();
                Transform3D t = new Transform3D();

                // Largest dimension goes from -1 to 1
                t.set(0.1, new Vector3d(point.x, point.y, point.z));
                sphereTransform.setTransform(t);

                if (sphereShowing == false) {
                    ((BranchGroup) ((BranchGroup) (burnRoot.getChild(findIndex))).getChild(0)).addChild(sphereBranch);
                    sphereShowing = true;
                }

                return;
            }

        }

        // select the surface file name in the list box
        if (iIndex < surfaceVector.size()) {
            iSelect = iIndex;
            surfaceList.setSelectedIndex(iSelect);
            surfaceList.ensureIndexIsVisible(iSelect);

            PickIntersection pi = pickedObject.getClosestIntersection(pickCanvas.getStartPosition());
            Point3d point = pi.getClosestVertexCoordinates();

            // compute C0, C1, C2, and max{C0, C1, C2}
            // these are the physical space voxel dimensions
            float fC0 = parentScene.getImageA().getExtents()[0] *
                            parentScene.getImageA().getFileInfo(0).getResolutions()[0];
            float fC1 = parentScene.getImageA().getExtents()[1] *
                            parentScene.getImageA().getFileInfo(0).getResolutions()[1];
            float fC2 = parentScene.getImageA().getExtents()[2] *
                            parentScene.getImageA().getFileInfo(0).getResolutions()[2];
            float fMax;

            // fMax is the largest of the three physical space dimensions
            fMax = (fC0 < fC1) ? fC1 : fC0;
            fMax = (fMax < fC2) ? fC2 : fMax;

            // want y to be 0 at top of image and extents[1] - 1 at bottom of image.
            // point.y = parentScene.getImageA().getExtents()[1] - point.y - 1;

            Transform3D t = new Transform3D();

            // Largest dimension goes from -1 to 1
            t.set(0.1, new Vector3d(point.x, point.y, point.z));
            sphereTransform.setTransform(t);

            if (sphereShowing == false) {
                surfaceRootBG.addChild(sphereBranch);
                sphereShowing = true;
            }

            // map into *physical* space
            // max{C0,C1,C2}*xk + Ck
            // yk =    ---------------------
            // 2
            point.x = ((point.x * fMax) + fC0) / 2;
            point.y = ((point.y * fMax) + fC1) / 2;
            point.z = ((point.z * fMax) + fC2) / 2;

            // now want to get into image space, so divide by resolutions
            point.x = point.x / parentScene.getImageA().getFileInfo(0).getResolutions()[0];
            point.y = point.y / parentScene.getImageA().getFileInfo(0).getResolutions()[1];
            point.z = point.z / parentScene.getImageA().getFileInfo(0).getResolutions()[2];
        }

        if (foundLightBulb) {
            m_kLightsControl.setSelectedIndex(m_aiMapIndexToJPanelLightsIndex[iPickedLight]);
            activeLightBulbIndex = iPickedLight;
        }
    }

    /**
     * Return access to the Group data member m_kGeodesicGroup so Geodesic object can draw on the triangle mesh.
     *
     * @return  BranchGroup root of the geodesic group reference.
     */
    public BranchGroup getGeodesicGroup() {

        if (m_kGeodesicGroup == null) {
            m_kGeodesicGroup = new BranchGroup();
            m_kGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            m_kGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
            m_kGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_READ);
            parentScene.getSceneRootTG().addChild(m_kGeodesicGroup);
        }

        return m_kGeodesicGroup;
    }

    /**
     * The only caller of this method is the light attribute dialog, an object from class JDialogSurfaceLight. The
     * dialog calls this to initialize its color button.
     *
     * @param   i  The light index (must be in [0..8]).
     *
     * @return  The color of light i.
     */
    public Color3f getLightColor(int i) {

        if ((0 <= i) && (i < lightArray.length)) {
            Color3f color = new Color3f();

            lightArray[i].getColor(color);

            return color;
        } else {
            return null;
        }
    }

    /**
     * The number of lights supported by the viewer (currently 9). The only caller of this method is the light attribute
     * dialog, an object from class JDialogSurfaceLight. The dialog uses the count to create a list of lights in the
     * scene. If the number of supported lights ever changes, the dialog code does not have to change.
     *
     * @return  The number of supported lights in the scene.
     */
    public int getLightCount() {
        return lightArray.length;
    }

    /**
     * Return the surface light control dialog box.
     *
     * @return  lights Surface light dialog box
     */
    public JPanelLights getLightDialog() {
        return m_kLightsControl;
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
     * Get the surface material reference.
     *
     * @param   iIndex  material index.
     *
     * @return  Material material reference.
     */
    public Material getMaterial(int iIndex) {

        if (!((SurfaceAttributes) surfaceVector.get(iIndex)).getIsVOIPt()) {
            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).getBranch();
            Shape3D shape = (Shape3D) root.getChild(0);

            /* Set the new surface material values: */
            Appearance kAppearance = shape.getAppearance();

            return kAppearance.getMaterial();
        }

        return null;
    }

    /**
     * Get the current surface opacity slider.
     *
     * @return  opacitySlider Surface opacity slider
     */
    public JSlider getOpacitySlider() {
        return opacitySlider;
    }

    /**
     * Get the pick canvas.
     *
     * @return  pickCanvas pick canvas.
     */
    public PickCanvas getPickCanvas() {
        return pickCanvas;
    }

    /**
     * Return the triangle mesh m_kSurface.
     *
     * @return  the surface mesh
     */
    public ModelTriangleMesh[] getSurface() {
        return m_kSurface;
    }

    /**
     * Get the back face culling check box.
     *
     * @return  surfaceBackFaceCB surface back face culling check box.
     */
    public JCheckBox getSurfaceBackFaceCB() {
        return surfaceBackFaceCB;
    }

    /**
     * Get the surface clip check box.
     *
     * @return  surfaceClipCB surface clip check box.
     */
    public JCheckBox getSurfaceClipCB() {
        return surfaceClipCB;
    }

    /**
     * Get the surface color.
     *
     * @return  Color surface color
     */
    public Color getSurfaceColor() {
        return surColor;
    }

    /**
     * Get the reference to the surface list. This method is called by the probePanel.
     *
     * @return  surfaceList reference to surfaceList
     */
    public JList getSurfaceList() {
        return surfaceList;
    }

    /**
     * Get the current surface opacity value.
     *
     * @return  surfaceOpaictySlice Surface opacity slider value
     */
    public int getSurfaceOpacity() {
        return surfaceOpacitySlice;
    }


    /**
     * Return the surface vector reference.
     *
     * @return  surfaceVector surface vector
     */
    public Vector getSurfaceVector() {
        return surfaceVector;
    }

    /**
     * If probe being picked or not.
     *
     * @return  boolean probe picked or not
     */
    public boolean isProbePicked() {
        return findProbe;
    }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  The mouse event.
     */
    public void mouseClicked(MouseEvent kEvent) { // updateSurface(kEvent);
    }

    /**
     * One of the overrides necessary to be a MouseMotionListener. This member only exists to satisfy the conditions of
     * being a MouseMotionListener. It does nothing when invoked.
     *
     * @param  kEvent  The mouse event.
     */
    public void mouseDragged(MouseEvent kEvent) { }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  The mouse event.
     */
    public void mouseEntered(MouseEvent kEvent) { /* stub */
    }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  The mouse event.
     */
    public void mouseExited(MouseEvent kEvent) { /* stub */
    }

    /**
     * One of the overrides necessary to be a MouseMotionListener. This member only exists to satisfy the conditions of
     * being a MouseMotionListener. It does nothing when invoked.
     *
     * @param  kEvent  The mouse event.
     */
    public void mouseMoved(MouseEvent kEvent) { }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  The mouse event.
     */
    public synchronized void mousePressed(MouseEvent kEvent) { /* stub */

        Object source = kEvent.getSource();
        JPanelMouse myMouseDialog = ((SurfaceRender) parentScene).getMouseDialog();

        if (myMouseDialog.isRecording() && (source == opacitySlider)) {
            Transform3D t3D = new Transform3D();

            parentScene.getSceneRootTG().getTransform(t3D);
            surfaceOpacityEvents = new MouseEventVector("surfaceOpacitySlider" + surfaceSliderCount, t3D,
                                                        myMouseDialog.first, parentScene.getSceneState(),
                                                        parentScene.getMouseMode());
            setSurfaceOpacityFlag = true;
            myMouseDialog.events.add(surfaceOpacityEvents);
            currentIndex = myMouseDialog.events.indexOf(surfaceOpacityEvents);
        }

        findPickedObject(kEvent);

        if (findArbitraryClipping) {
            ((SurfaceRender) renderBase).enableObjBehavior(false);
            parentScene.getClipDialog().enableClipArbiBehavior(true);
        } else if (isProbePicked()) {

            if (((SurfaceRender) renderBase).getProbeDialog() != null) {
                ((SurfaceRender) renderBase).getProbeDialog().setProbeGreenColor(true);
                ((SurfaceRender) renderBase).enableObjBehavior(false);
                ((SurfaceRender) renderBase).getProbeDialog().enableProbeBehavior(true);
                enableStaticLightBehavior(false);
            }
        } else if (isStaticPicked() && isStaticLightEnabled()) {
            enableStaticLightBehavior(true);
            ((SurfaceRender) parentScene).enableObjBehavior(false);
        } else {
            enableStaticLightBehavior(false);
            ((SurfaceRender) parentScene).enableObjBehavior(true);

            if (((SurfaceRender) parentScene).getProbeDialog() != null) {
                ((SurfaceRender) parentScene).getProbeDialog().enableProbeBehavior(false);
            }
        }

        if (!isProbePicked() && (((SurfaceRender) renderBase).getDisplayDialog() != null)) {
            float coarseVal = ((SurfaceRender) renderBase).getDisplayDialog().getCoarseVal();

            ((SurfaceRender) renderBase).setSliceSpacingCoarse(coarseVal);
            ((SurfaceRender) renderBase).useSliceSpacingCoarse();
        }

        // mouse press in Canvas3D, remove the probing path, which indicates the tissues detected along the path.
        ((SurfaceRender) parentScene).getProbeDialog().removeProbingPath();

    }

    /**
     * One of the overrides necessary to be a MouseListener. This member only exists to satisfy the conditions of being
     * a MouseListener. It does nothing when invoked.
     *
     * @param  kEvent  The mouse event.
     */
    public synchronized void mouseReleased(MouseEvent kEvent) { /* stub */

        float fineVal = 0f;
        Object source = kEvent.getSource();

        // System.out.println("JPanelSurface mouseReleased");
        if (isStaticPicked() && isStaticLightEnabled()) {
            activeLightBulbIndex = -1;
        }


        if (findArbitraryClipping) {
            ((SurfaceRender) renderBase).enableObjBehavior(true);
            parentScene.getClipDialog().enableClipArbiBehavior(false);
            findArbitraryClipping = false;
        }

        if (!isProbePicked() && (((SurfaceRender) renderBase).getDisplayDialog() != null)) {

            // coarseVal = ( (SurfaceRender) renderBase ).getDisplayDialog().getCoarseVal();
            // ( (SurfaceRender) renderBase ).setSliceSpacingCoarse( coarseVal );
            fineVal = ((SurfaceRender) renderBase).getDisplayDialog().getFineVal();
            ((SurfaceRender) renderBase).setSliceSpacingFine(fineVal);
            ((SurfaceRender) renderBase).useSliceSpacingFine();
        }

        /* The probe was occaisionally not being un-selected when the mouse
         * button was released, so the probe is always un-selected on
         * mouseRelease */
        if (((SurfaceRender) renderBase).getProbeDialog() != null) {

            /* If the mouse release is the left mouse button, then the
             * textures need to be updated one last time at the super-sampled
             * high resolution: */
            if (isProbePicked() && (kEvent.getButton() == MouseEvent.BUTTON1)) {
                ((SurfaceRender) renderBase).updateProbe(true, true, false);
            }

            /* De-select the probe: */
            findProbe = false;
            ((SurfaceRender) renderBase).getProbeDialog().setProbeGreenColor(false);
            ((SurfaceRender) renderBase).enableObjBehavior(true);
            ((SurfaceRender) renderBase).getProbeDialog().enableProbeBehavior(false);
            enableStaticLightBehavior(false);
        }

        JPanelMouse myMouseDialog = ((SurfaceRender) parentScene).getMouseDialog();

        if (myMouseDialog.isRecording() && (source == opacitySlider)) {
            surfaceSliderCount++;
        }

        // mouse release in Canvas3D, detecting tissues along the probe path.
        ((SurfaceRender) parentScene).getProbeDialog().detectTissue();
        ((SurfaceRender) parentScene).updateRaycastRender();
    }

    /**
     * Removes a BranchGroup to the display.
     *
     * @param  kBranch      BranchGroup surface branch group reference.
     * @param  bRemoveMesh  boolean flag to remove the surface mesh or not
     */
    public void removeBranch(BranchGroup kBranch, boolean bRemoveMesh) {
        surfaceRootBG.removeChild(kBranch);

        for ( int i = 0; i < surfaceVector.size();  i++ )
        {
            String kElementName = ((SurfaceAttributes) surfaceVector.get(i)).getName();
            if ( kElementName.equals( "branch" ) )
            {
                surfaceVector.removeElementAt(i);
                parentScene.getImageA().removeSurfaceMask(i);
                break;
            }
        }

        ((SurfaceRender) renderBase).updateData();
    }


    /**
     * ReplaceMesh is used by the Geodesic when a mesh is cut along either an open or closed geodesic curve, the
     * original mesh is changed, but not deleted and no new mesh is added. The orginal mesh, kOld, is replced by the new
     * mesh, kNew
     *
     * @param  kOld  ModelTriangleMesh old mesh reference
     * @param  kNew  ModelTriangleMesh new mesh reference
     */
    public void replaceMesh(ModelTriangleMesh kOld, ModelTriangleMesh kNew) {

        SurfaceAttributes[] surfaces = containsMesh( kOld );
        if ( surfaces == null )
        {
            return;
        }

        boolean found = false;
        for ( int i = 0; i < surfaceVector.size() && !found; i++ )
        {
            ModelTriangleMesh[] meshes = ((SurfaceAttributes)surfaceVector.get( i )).getMesh();
            for ( int j = 0; j < meshes.length && !found; j++ )
            {
                if ( meshes[j] == kOld )
                {
                    Shape3D[] shapes = ((SurfaceAttributes)surfaceVector.get( i )).getShape();
                    shapes[j].removeGeometry( kOld );
                    shapes[j].addGeometry( kNew );
                    found = true;
                }
            }
        }

        /* Remove from the scene graph, adding the new mesh, kNew, in it's
         * place: */
        for (int iChild = 0; iChild < surfaceRootBG.numChildren(); iChild++) {
            if (surfaceRootBG.getChild(iChild) instanceof BranchGroup) {
                BranchGroup kBG = (BranchGroup) (surfaceRootBG.getChild(iChild));
                if (kBG.getChild(0) instanceof Shape3D) {
                    Shape3D kShapeMesh = (Shape3D) (kBG.getChild(0));
                    if (kShapeMesh.getGeometry() instanceof ModelTriangleMesh) {
                        ModelTriangleMesh kChildMesh = (ModelTriangleMesh) (kShapeMesh.getGeometry());
                        if (kChildMesh == kOld) {
                            kShapeMesh.removeGeometry(kOld);
                            kShapeMesh.addGeometry(kNew);
                            break;
                        }
                    }
                }
            }
        }
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
     * Update the properties of the specified light bulb.
     *
     * @param  iIndex  int Index of the bulb in the array as stored in JPanelLights.
     */
    public void setLightAttributes(int iIndex) {
        GeneralLight kGeneralLight = m_kLightsControl.getGeneralLight(iIndex);
        int i = m_aiMapJPanelLightsIndexToIndex[iIndex];

        // Check for an undefined light.
        if (-1 == i) {
            return;
        }

        if ((0 <= i) && (i < (lightArray.length - 2))) {

            if (null != lightArray[i]) {
                lightArrayBG[i].detach();
                lightArray[i] = kGeneralLight.createJava3dLight();
                lightArray[i].setInfluencingBounds(parentScene.bounds);
                lightArrayBG[i] = new BranchGroup();
                lightArrayBG[i].setCapability(BranchGroup.ALLOW_DETACH);
                lightArrayBG[i].addChild(lightArray[i]);
                surfaceRootTG.addChild(lightArrayBG[i]);
            }

            lightBulbs.setLightAttributes(i, kGeneralLight);
        } else if (i == (lightArray.length - 1)) {

            if (null != lightArray[i]) {
                lightArrayBG[i].detach();
                lightArray[i] = kGeneralLight.createJava3dLight();
                lightArray[i].setInfluencingBounds(parentScene.bounds);
                lightArrayBG[i] = new BranchGroup();
                lightArrayBG[i].setCapability(BranchGroup.ALLOW_DETACH);
                lightArrayBG[i].addChild(lightArray[i]);
                staticLightBG.addChild(lightArrayBG[i]);
            }

            staticLightBulb.setLightAttributes(0, kGeneralLight);
        } else {

            if (null != lightArray[i]) {
                lightArrayBG[i].detach();
                lightArray[i] = kGeneralLight.createJava3dLight();
                lightArray[i].setInfluencingBounds(parentScene.bounds);
                lightArrayBG[i] = new BranchGroup();
                lightArrayBG[i].setCapability(BranchGroup.ALLOW_DETACH);
                lightArrayBG[i].addChild(lightArray[i]);
                surfaceRootTG.addChild(lightArrayBG[i]);
            }
        }
    }

    /**
     * Sets the light bulbs to visible or invisible.
     *
     * @param  flag  <code>true</code> means visible.
     */
    public void setLightBulbsVisible(boolean flag) {

        if (flag == true) { // will add it to surfaceRoot
            surfaceRootBG.addChild(lightBulbs.getRoot());
            staticLightBG.addChild(staticLightBulb.getRoot());
        } else {
            lightBulbs.getRoot().detach();
            staticLightBulb.getRoot().detach();
        }
    }

    /**
     * Set the surface level of detail attribute.
     * @param levelDetail level detail
     * @param iIndex int surface index
     */
    public void setLevelDetail(int levelDetail, int iIndex) {
      ( (SurfaceAttributes) surfaceVector.get(iIndex)).setLevelDetail( levelDetail );
    }


    /**
     * Called from the JPanelSurfaceMAterialProperties.java dialog when the dialog is used to change the material
     * properties of a surface. The surface is determined by the index iIndex. The color button is set to the Material
     * diffuse color.
     *
     * @param  kMaterial  Material reference
     * @param  iIndex     int material index
     */
    public void setMaterial(Material kMaterial, int iIndex) {

        if (!((SurfaceAttributes) surfaceVector.get(iIndex)).getIsVOIPt()) {
            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).getBranch();
            Shape3D shape = (Shape3D) root.getChild(0);
            shape.getAppearance().setMaterial( kMaterial );
            Color3f kDiffuse = new Color3f();
            kMaterial.getDiffuseColor(kDiffuse);
            Color4f kColor = new Color4f(kDiffuse.get());
            colorButton.setBackground(kDiffuse.get());
            ((SurfaceAttributes) surfaceVector.get(iIndex)).setMaterial( kMaterial );
            kDiffuse = null;
        }
    }

    /**
     * Updates the position of the flythru view point along the flythru path.
     *
     * @param  kPosition  Point3f
     */
    public void setPathPosition(Point3f kPosition) {

        if (m_kPathPositionTG != null) {
            Transform3D kViewTransform = new Transform3D();

            kViewTransform.set(new Vector3f(kPosition));
            m_kPathPositionTG.setTransform(kViewTransform);
        }
    }

    /**
     * The override necessary to be a ChangeListener for a JSlider. When a change occurs, the user has requested that
     * the level of detail be changed for the currently active surface. The slider range is [0,100] and is treated as a
     * percent of maximum level of detail. A change has no effect on ModelTriangleMesh objects, but it does change the
     * level of detail for an ModelClodMesh object. The number of triangles is also updated.
     *
     * @param  event  The change event.
     */
    public void stateChanged(ChangeEvent event) {

        if (event.getSource() == detailSlider) {

            if (detailSlider.getValueIsAdjusting()) {
                // Too many LOD changes occur if you always get the slider
                // value.  Wait until the user stops dragging the slider.

                // Maybe not. Comment out the next line to have the surface update quickly.
                // If the CLOD mesh holds a surface over time one could view the surface evolution !!!!
                return;
            }

            ModelImage imageA = parentScene.getImageA();
            int[] extents = imageA.getExtents();
            float[] resols = imageA.getFileInfo()[0].getResolutions();
            float xBox = extents[0] * resols[0];
            float yBox = extents[1] * resols[1];
            float zBox = extents[2] * resols[2];
            float maxBox = Math.max(xBox, Math.max(yBox, zBox));

            // value in [0,100], corresponds to percent of maximum LOD
            int iValue = detailSlider.getValue();

            // construct the lists of items whose LODs need to be changed
            int[] aiSelected = surfaceList.getSelectedIndices();
            for (int i = 0; i < aiSelected.length; i++) {

                if (((SurfaceAttributes) surfaceVector.get(aiSelected[i])).getIsVOIPt() == false) {
                    int numTriangles = 0;
                    float volume = 0;
                    float area = 0;
                    BranchGroup root = ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).getBranch();

                    ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).setLevelDetail( iValue );

                    for (int j = 0; j < root.numChildren(); j++) {
                        Shape3D shape = (Shape3D) root.getChild(j);
                        ModelTriangleMesh kMesh = (ModelTriangleMesh) shape.getGeometry(0);

                        if (kMesh.getGenerator() != null) {

                            // clod mesh surface was selected for LOD changes
                            ModelClodMesh kClod = (ModelClodMesh) kMesh.getGenerator();
                            int iLOD = (int) (iValue * kClod.getMaximumLOD() / 100.0f);

                            kClod.setLOD(iLOD);
                            numTriangles += kClod.getMesh().getIndexCount();
                            volume += kClod.getMesh().volume();
                            area += kClod.getMesh().area();
                            // update the Shape3D parent

                            shape.insertGeometry(kClod.getMesh(), 0);
                            shape.removeGeometry(1);
                        } else {
                            numTriangles += kMesh.getIndexCount();
                            volume += kMesh.volume();
                            area += kMesh.area();
                        }
                    }

                    numTriangles /= 3;
                    triangleText.setText("" + numTriangles);

                    // One length across the extracted surface changes from -1 to 1
                    // while one length across the actual volume changes by ((dim-1)*res)max
                    volumeText.setText("" + (volume * maxBox * maxBox * maxBox / 8.0f));
                    areaText.setText(String.valueOf(area * maxBox * maxBox));
                    ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).setNumberTriangles( numTriangles );
                    ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).setVolume( volume );
                    ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).setArea( area );
                }
            }
        }
        else if (event.getSource() == opacitySlider) {
             // change the opacity for the selected items
            JPanelMouse myMouseDialog = ((SurfaceRender) parentScene).getMouseDialog();

             surfaceOpacitySlice = opacitySlider.getValue();

            if (myMouseDialog.isRecording() && setSurfaceOpacityFlag) {
                surfaceOpacityEvents.setName("surfaceOpacitySlider" + currentIndex);
                myMouseDialog.listModel.addElement("surfaceOpacitySlider" + currentIndex);
                setSurfaceOpacityFlag = false;
            }
            if (myMouseDialog.isRecording()) {
                surfaceOpacityEvents.add(event, parentScene.getSceneState());
            }

            int[] aiSelected = surfaceList.getSelectedIndices();
            SurfaceAttributes[] surfaces = getSelectedSurfaces( aiSelected );
            int iValue = opacitySlider.getValue();
            for (int i = 0; i < surfaces.length; i++) {
                surfaces[i].setOpacity(1 - (iValue / 100.0f));
                Color4f newColor = new Color4f( surfaces[i].getColor().x,
                                                surfaces[i].getColor().y,
                                                surfaces[i].getColor().z,
                                                1 - surfaces[i].getOpacity() );
                /* Change the mask color: */
                parentScene.getImageA().addSurfaceMask( aiSelected[i], null, null, newColor );
                /* update the surface renderer: */
                ((SurfaceRender) renderBase).updateData();

                BranchGroup root = surfaces[i].getBranch();
                Shape3D shape = (Shape3D) root.getChild(0);
                Appearance appearance = shape.getAppearance();
                appearance.getTransparencyAttributes().setTransparency(1 - (iValue / 100.0f)); // 0 = Opaque
            }
        }
    }

    /**
     * Toggle between wireframe and filled polygon mode.
     */
    public void toggleWireframe() {
        int iMode = polygonIndexToMode( polygonModeCB.getSelectedIndex() );

        if (iMode == PolygonAttributes.POLYGON_LINE) {
            iMode = PolygonAttributes.POLYGON_FILL;
        } else {
            iMode = PolygonAttributes.POLYGON_LINE;
        }

        polygonModeCB.setSelectedIndex(iMode);
        surfaceList.setSelectionInterval(0, surfaceList.getModel().getSize() - 1);
        changePolyMode(iMode);
    }

    /**
     * The override necessary to be a ListSelectionListener. This callback is executed whenever the user selects a new
     * item (or items) in the list box. If a single item is selected, then the selection index is remembered in <code>
     * iSelect</code> and the color button, detail slider, and polygon mode are initialized with the appropriate values
     * corresponding to the selected surface. If multiple items are selected, then the selection index is -1 and the
     * color button is set to the background color and disabled. The slider and polygon mode are set to the values found
     * in the minimum selected surface but are still enabled. The dialog does not support changing the color for
     * multiple surfaces at one time but does support changing level of detail and polygon mode for multiple surfaces at
     * one time (to the same value).
     *
     * @param  kEvent  The list selection event.
     */
    public void valueChanged(ListSelectionEvent kEvent) {

        // Let getColorChange know that only the selection is changing,
        // not the material colors for the selected surface.
        iSelect = -1;

        try {
            JList kList = (JList) kEvent.getSource();
            int[] indices = kList.getSelectedIndices();

            int index = kList.getMinSelectionIndex();
            SurfaceAttributes attributes;

            if (index != -1) {
                BranchGroup root = ((SurfaceAttributes) surfaceVector.get(index)).getBranch();

                surfacePickableCB.setSelected(root.getPickable());
                surfaceBackFaceCB.setSelected(root.getCollidable());

                if (((SurfaceRender) parentScene).getDisplayMode3D()) {
                    surfaceClipCB.setSelected(root.getAlternateCollisionTarget());
                }
            }

            if (indices.length > 0) {
                attributes = (SurfaceAttributes) surfaceVector.get(index);

                if (indices.length == 1) {
                    iSelect = kList.getMinSelectionIndex();

                    // a single surface was selected, set edit text to its color
                    Color4f color = attributes.getColor();

                    colorButton.setBackground(color.get());
                    setElementsEnabled(true);
                } else {

                    // multiple surfaces were selected (not all colors the same)
                    colorButton.setBackground(getBackground());
                    colorButton.setEnabled(false);
                    colorLabel.setEnabled(false);

                }

                if ( polygonIndexToMode( polygonModeCB.getSelectedIndex() ) !=
                     ((SurfaceAttributes) surfaceVector.get(index)).getPolygonMode())
                {
                    int mode = 0;

                    switch (((SurfaceAttributes) surfaceVector.get(index)).getPolygonMode()) {

                    case PolygonAttributes.POLYGON_FILL:
                        mode = 0;
                        break;

                    case PolygonAttributes.POLYGON_LINE:
                        mode = 1;
                        break;

                    case PolygonAttributes.POLYGON_POINT:
                        mode = 2;
                        break;
                    }

                    polygonModeCB.setSelectedIndex(mode);
                }

                triangleText.setText("" + attributes.getNumberTriangles());
                volumeText.setText("" + attributes.getVolume());
                areaText.setText("" + attributes.getArea());
                detailSlider.setValue(attributes.getLevelDetail());
                opacitySlider.setValue((int) ( (1 - attributes.getOpacity()) * 100));

                boolean enable = true;

                for (int i = 0; i < indices.length; i++) {

                    if (!((SurfaceAttributes) surfaceVector.get(indices[i])).getIsClodMesh()) {
                        enable = false;
                    }
                }

                detailSlider.setEnabled(enable);
                detailLabel.setEnabled(enable);

                for (int i = 0; i < detailSliderLabels.length; i++) {
                    detailSliderLabels[i].setEnabled(enable);
                }

                decimateButton.setEnabled(!enable);
            }
        } catch (ArrayIndexOutOfBoundsException e) {
            return;
        }
    }

    /**
     * Overrides method in JDialogBase so dialog isn't disposed, just hidden.
     *
     * @param  event  Event that triggered this method.
     */
    public void windowClosing(WindowEvent event) { }

    /**
     * Build the toolbar.
     */
    private void buildToolBar() {

        Border etchedBorder = BorderFactory.createEtchedBorder();
        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);
        ;

        toolBar = new JToolBar();
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setBorder(etchedBorder);

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
        toolBarPanel.setLayout( new BorderLayout() );
        toolBarPanel.add( toolBar, BorderLayout.WEST );
        toolBarPanel.add( mSurfacePaint.getToolBar(), BorderLayout.SOUTH );

        mainPanel.add(toolBarPanel, BorderLayout.NORTH);
    }

    /** Convert from the polygon mode combo-box list index to the
     * PolygonAttributes.POLYGON_LINE, PolygonAttributes.POLYGON_POINT, and
     * PolygonAttributes.POLYGON_FILL values:
     * @param index, the index of the selected polygon mode in the polygonModeCB combo box.
     * @return the corresponding PolygonAttributes defined value.
     */
    private int polygonIndexToMode( int index )
    {
        switch ( index )
        {
        case 1:
            return (PolygonAttributes.POLYGON_LINE);
        case 2:
            return (PolygonAttributes.POLYGON_POINT);
        case 0:
        default:
            return (PolygonAttributes.POLYGON_FILL);
        }
    }

    /**
     * Changes the polygon mode of the selected surface by detaching it,
     * calling the appropriate method, and reattaching it.
     *
     * @param  mode  The new polygon mode to set.
     */
    private void changePolyMode(int mode) {
        int[] indices;
        BranchGroup surface;

        indices = surfaceList.getSelectedIndices();

        for (int i = 0; i < indices.length; i++) {
            surface = ((SurfaceAttributes) surfaceVector.get(indices[i])).getBranch();
            ((SurfaceAttributes) surfaceVector.get(indices[i])).setPolygonMode( mode );

            Object obj;
            for (Enumeration e = surface.getAllChildren(); e.hasMoreElements();) {
                obj = e.nextElement();
                try {
                    Shape3D shape = (Shape3D) obj;
                    shape.getAppearance().getPolygonAttributes().setPolygonMode(mode);
                } catch (ClassCastException error) { /* if (obj instanceof BranchGroup) {
                                                      * Object obj2; // won't let me read these branch groups. for
                                                      * (Enumeration e2 = ((BranchGroup)obj).getAllChildren();
                                                      * e2.hasMoreElements();) { obj2 = e2.nextElement(); if (obj2
                                                      * instanceof BranchGroup) { Object obj3; for (Enumeration e3 =
                                                      * ((BranchGroup)obj2).getAllChildren(); e3.hasMoreElements();) {
                                                      * obj3 = e3.nextElement(); } } }}*/
                }
            }
        }
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
     * Creates a surface in the scene graph from an array of triangle meshes.
     *
     * @param   meshes  Triangle meshes that make up surface
     * @param   color   Color of surface
     * @param   mode    The polygon drawing mode
     *
     * @return  Parent node of surface.
     */
    private void createSurface(SurfaceAttributes surface, boolean pickable )
    {
        if ( mTexture == null )
        {
            createVolumeTexture();
        }

        ModelTriangleMesh[] meshes = surface.getMesh();
        int mode = surface.getPolygonMode();
        float opacity = surface.getOpacity();

        // This Appearance Contains: 
        // ColoringAttributes, PolygonAttributes, RenderingAttributes, TransparencyAttributes,
        // Material, Texture, TextureAttributes, TextureUnitState
        Appearance appearance = new Appearance();
        appearance.setCapability(Appearance.ALLOW_COLORING_ATTRIBUTES_READ);
        appearance.setCapability(Appearance.ALLOW_COLORING_ATTRIBUTES_WRITE);
        appearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);
        appearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
        appearance.setCapability(Appearance.ALLOW_RENDERING_ATTRIBUTES_READ);
        appearance.setCapability(Appearance.ALLOW_RENDERING_ATTRIBUTES_WRITE);
        appearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ);
        appearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);
        appearance.setCapability(Appearance.ALLOW_MATERIAL_READ);
        appearance.setCapability(Appearance.ALLOW_MATERIAL_WRITE);
        appearance.setCapability( Appearance.ALLOW_TEXTURE_UNIT_STATE_READ );
        appearance.setCapability( Appearance.ALLOW_TEXTURE_UNIT_STATE_WRITE );

        // Coloring Attributes: for per-vertex color, set color interpolation to NICEST:
        ColoringAttributes colorA = new ColoringAttributes( surface.getColor3(), ColoringAttributes.NICEST );
        appearance.setColoringAttributes( colorA );

        // Transparency Attributes:
        TransparencyAttributes tap = new TransparencyAttributes();
        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_READ);
        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        tap.setTransparencyMode(TransparencyAttributes.BLENDED);
        tap.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA);
        tap.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA);
        System.err.println( "Surface opacity " + surface.getOpacity() );
        tap.setTransparency(0);
        appearance.setTransparencyAttributes(tap);

        // Material
        Material material = surface.getMaterial();
        appearance.setMaterial(material);

        // Texture Attributes:
        TextureAttributes kTextureAttr = new TextureAttributes();
        kTextureAttr.setTextureMode(TextureAttributes.REPLACE);
        //  Texture Unit State:
        TextureUnitState[] textureState = new TextureUnitState[1];
        textureState[0] = new TextureUnitState( mTexture, kTextureAttr, null );
        textureState[0].setCapability( TextureUnitState.ALLOW_STATE_WRITE );
        appearance.setTextureUnitState( textureState );

        // Polygon Attributes:
        // No back-face culling.  Supports double-sided meshes which can
        // regularly occur for level surfaces (open surfaces).
        PolygonAttributes kPAttr = new PolygonAttributes();
        kPAttr.setCapability(PolygonAttributes.ALLOW_CULL_FACE_WRITE);
        kPAttr.setCapability(PolygonAttributes.ALLOW_CULL_FACE_READ);
        kPAttr.setCapability(PolygonAttributes.ALLOW_MODE_READ);
        kPAttr.setCapability(PolygonAttributes.ALLOW_MODE_WRITE);
        kPAttr.setCullFace(PolygonAttributes.CULL_BACK);
        /* PolygonOffsetFactor is set so lines can be drawn ontop of the mesh
         * without coplanar problems. */
        kPAttr.setPolygonOffsetFactor(1.0f);
        kPAttr.setPolygonMode(mode);
        appearance.setPolygonAttributes(kPAttr);

        // Rendering Attributes:
        RenderingAttributes kRenderingAttr = new RenderingAttributes();
        kRenderingAttr.setIgnoreVertexColors( false );
        kRenderingAttr.setCapability( RenderingAttributes.ALLOW_IGNORE_VERTEX_COLORS_READ );
        kRenderingAttr.setCapability( RenderingAttributes.ALLOW_IGNORE_VERTEX_COLORS_WRITE );
        appearance.setRenderingAttributes( kRenderingAttr );

        // Add surface to scene graph:
        // Give the meshes a branch group parent.  This type of parent is
        // necessary when working with a compiled scene graph to allow
        // attaching/detaching nodes.
        BranchGroup root = new BranchGroup();
        root.setCapability(BranchGroup.ALLOW_DETACH);
        root.setCapability(Group.ALLOW_CHILDREN_READ);
        root.setCapability(Group.ALLOW_CHILDREN_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_READ);

        // For culling and picking purposes, it is better to have a separate
        // Shape3D parent for each mesh rather than a single Shape3D parent
        // for all meshes.
        Shape3D[] akSurfaceShape = new Shape3D[meshes.length];
        for (int i = 0; i < meshes.length; i++) {
            akSurfaceShape[i] = new Shape3D(meshes[i], appearance);

            akSurfaceShape[i].setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            akSurfaceShape[i].setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            akSurfaceShape[i].setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
            akSurfaceShape[i].setCapability(Shape3D.ALLOW_GEOMETRY_READ);
            akSurfaceShape[i].setCapability(Geometry.ALLOW_INTERSECT);

            try {

                // pickCanvas.setCapabilities( shape, PickTool.INTERSECT_FULL );
                PickCanvas.setCapabilities(akSurfaceShape[i], PickCanvas.INTERSECT_FULL);
            } catch (RestrictedAccessException error) { }

            root.addChild(akSurfaceShape[i]);
        }
        surface.setShape( akSurfaceShape );

        m_kSurface = new ModelTriangleMesh[2];
        m_kSurface = meshes;

        root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        root.setCapability(Group.ALLOW_CHILDREN_READ);
        root.setCapability(Group.ALLOW_CHILDREN_WRITE);
        root.setCapability(BranchGroup.ALLOW_DETACH);
        root.setCapability(Node.ALLOW_PICKABLE_READ);
        root.setCapability(Node.ALLOW_PICKABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_READ);
        root.setPickable( pickable );
        surfacePickableCB.setSelected( pickable );

        // Allow Java to optimize subtree.  Attach to the scene graph.
        root.compile();
        surfaceRootBG.addChild(root);
        surface.setBranch( root );
    }

    /**
     * Creates a 3D Texture object for the ModelImage displayed in the
     * SurfaceRender object. The Texture3D object is used for texture-mapping
     * on any or all ModelTriangleMesh objects displayed in the SurfaceRender.
     */
    private void createVolumeTexture()
    {

        ModelImage imageA = ((SurfaceRender)parentScene).getImageA();
        int[] localExtents = imageA.getExtents();

        mTexture = new Texture3D(Texture.BASE_LEVEL, Texture.RGBA, localExtents[0], localExtents[1], localExtents[2]);
        mTexture.setEnable(false);
        mTexture.setMinFilter(Texture.BASE_LEVEL_LINEAR);
        mTexture.setMagFilter(Texture.NICEST);

        // m_kTexture.setAnisotropicFilterMode(Texture.ANISOTROPIC_SINGLE_VALUE);
        // m_kTexture.setAnisotropicFilterDegree(5);
        mTexture.setBoundaryModeS(Texture.CLAMP_TO_BOUNDARY);
        mTexture.setBoundaryModeT(Texture.CLAMP_TO_BOUNDARY);
        mTexture.setBoundaryModeR(Texture.CLAMP_TO_BOUNDARY);
        mTexture.setBoundaryColor(0.0f, 0.0f, 0.0f, 0.0f);
        mTexture.setImage(0, ((SurfaceRender)parentScene).getSurfaceTexture().getSurfaceTextureImage()) ;
        mTexture.setCapability(Texture3D.ALLOW_IMAGE_WRITE);
        mTexture.setCapability(Texture3D.ALLOW_IMAGE_READ);
        mTexture.setCapability(Texture3D.ALLOW_ENABLE_WRITE);
    }


    /**
     * Called from SurfacePaint. Used to paint the ModelImage texture with the
     * paint can function. 
     * @param paintMask, the paint mask to add to the texture.
     */
    public void updateVolumeTexture( BitSet paintMask, Color4f kColor )
    {
        JPanelSurfaceTexture surfaceTexturePanel = ((SurfaceRender)parentScene).getSurfaceTexture();
        surfaceTexturePanel.updateSurfaceTextureImage( 0, paintMask, kColor );
    }

    /** 
     * Returns the ModelImage used to generate the Texture3D.
     * @return the JPanelSurfaceTexture ModelImage data source for the Texture3D object.
     */
    public ModelImage getTextureImage()
    {
        return ((SurfaceRender)parentScene).getSurfaceTexture().getTextureImage();
    }

    /** 
     * Returns the texture status, either TEXTURE, VERTEX_COLOR, or NONE
     * @return the texture status, either TEXTURE, VERTEX_COLOR, or NONE
     */
    public int getTextureStatus()
    {
        return ((SurfaceRender)parentScene).getSurfaceTexture().getTextureStatus();
    }

    /**
     * Restores the vertex colors for the surfaces.
     */
    public void restoreVertexColors()
    {
        JPanelSurfaceTexture surfaceTexturePanel = ((SurfaceRender)parentScene).getSurfaceTexture();
        if ( surfaceTexturePanel.getTextureStatus() != JPanelSurfaceTexture.NONE )
        {
            surfaceTexturePanel.updateSurfaceTextureImage( 0, null, null );
        }
        else
        {
            SurfaceAttributes[] surfaces = getAllSurfaces();
            for ( int i = 0; i < surfaces.length; i++ )
            {
                surfaces[i].restoreVertexColors();
            }
        }
    }

    /** 
     * Generates new texture coordinates for all surfaces, based on the new
     * ModelImage texture-source. If bVertexColors is true, the ModelImage is
     * displayed as per-vertex colors.
     * @param kImage, the new ModelImage used to texture-map the
     * ModelTriangleMesh polygons.
     * @param bVertexColors, when true display the ModelImage as per-vertex
     * colors, when false, display the ModelImage as a 3D Texture.
     */
    public void generateNewTextureCoords( ModelImage kImage, boolean bVertexColors, boolean bUseImageMask ) 
    {
        SurfaceAttributes[] surfaces = getAllSurfaces();
        for ( int i = 0; i < surfaces.length; i++ )
        {
            mSurfaceMask.maskInsideVoxels( 0,
                                           kImage,
                                           surfaces[i].getMesh(), false, bVertexColors, bUseImageMask,
                                           surfaces[i].getOpacity(), surfaces[i].getColor());
            surfaces[i].setMask( mSurfaceMask.getVolumeMask() );
        }
    }

    /**
     * Enables/Disables the SurfacePaint Paint Can function.
     * @param bEnable when true the Paint Can function is enabled, when false it is disabled.
     */
    public void enableSurfacePaintCan( boolean bEnable )
    {
        mSurfacePaint.enableSurfacePaintCan( bEnable );
    }

    /**
     * Enables/Disables the SurfacePaint per-vertex functions.
     * @param bEnable when true the SurfacePaint per-vertex functions (PaintBrush, Dropper, Eraser, BrushSize) are
     * enabled, when false they are disabled.
     */
    public void enableSurfacePaint( boolean bEnable )
    {
        mSurfacePaint.enableSurfacePaint( bEnable );
    }


    /**
     * Returns the SurfaceMask object.
     * @return mSurfaceMask.
     */
    public SurfaceMask getSurfaceMask()
    {
        return mSurfaceMask;
    }


    /**
     * Decimate the surface.
     */
    private void decimate( SurfaceAttributes[] surfaces )
    {
        for ( int i = 0; i < surfaces.length; i++ )
        {
            ModelTriangleMesh[] meshes = surfaces[i].getMesh();
            BranchGroup root = surfaces[i].getBranch();
            surfaceRootBG.removeChild(root);
            //root.detach();
            //             BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).getBranch();
            //             ModelTriangleMesh[] meshes = new ModelTriangleMesh[root.numChildren()];
            
            //             for (int j = 0; j < root.numChildren(); j++) {
            //                 Shape3D shape = (Shape3D) root.getChild(j);
            
            //                 meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
            //             }
            
            int iVMaxQuantity = 0, iTMaxQuantity = 0;
            
            for (int j = 0; j < meshes.length; j++) {
                int iVQuantity = meshes[j].getVertexCount();
                
                if (iVQuantity > iVMaxQuantity) {
                    iVMaxQuantity = iVQuantity;
                }
                
                int iTQuantity = meshes[j].getIndexCount() / 3;
                
                if (iTQuantity > iTMaxQuantity) {
                    iTMaxQuantity = iTQuantity;
                }
            }

            ViewJProgressBar progressBar = new ViewJProgressBar("Decimating surface", "Decimating surface",
                                                                0, 100, false, null, null);
            
            progressBar.updateValue(0, true);
            progressBar.setLocation(200, 200);
            progressBar.setVisible(true);
            
            ModelClodMesh[] akClod = new ModelClodMesh[meshes.length];
            ModelSurfaceDecimator kDecimator = new ModelSurfaceDecimator(iVMaxQuantity, iTMaxQuantity);
            
            for (int j = 0; j < meshes.length; j++) {
                Point3f[] akVertex = meshes[j].getVertexCopy();
                int[] aiConnect = meshes[j].getIndexCopy();
                kDecimator.decimate(akVertex, aiConnect, progressBar, j * 100 / meshes.length, meshes.length);
                akClod[j] = new ModelClodMesh(akVertex, aiConnect, kDecimator.getRecords());
                akClod[j].setLOD(akClod[j].getMaximumLOD());
                meshes[j] = akClod[j].getMesh();
            }
            
            progressBar.updateValue(100, true);
            
            surfaces[i].setMesh( meshes );
            surfaces[i].restoreVertexColors();
            createSurface( surfaces[i], false );
            mSurfaceMask.maskInsideVoxels( surfaceVector.size(),
                                           parentScene.getImageA(),
                                           surfaces[i].getMesh(), false, false, false,
                                           surfaces[i].getOpacity(), surfaces[i].getColor());
            surfaces[i].setMask( mSurfaceMask.getVolumeMask() );
            
            if (((SurfaceRender) parentScene).getDisplayMode3D()) {
                surfaceClipCB.setSelected(false);
            }
            surfaces[i].setIsClodMesh( true );
            
            decimateButton.setEnabled(false);
            detailSlider.setEnabled(true);
            detailLabel.setEnabled(true);
            
            for (int j = 0; j < detailSliderLabels.length; j++) {
                detailSliderLabels[j].setEnabled(true);
            }
            
            progressBar.dispose();
        }
    }

    /**
     * Enable static light behavior.
     *
     * @param  flag  true enable and false disable.
     */
    private void enableStaticLightBehavior(boolean flag) {
        staticLightTranslate.setEnable(flag);
        staticLightZoom.setEnable(flag);
    }

    /**
     * This is called when the user chooses a new color for the surface. It changes the color of the surface.
     *
     * @param  color  Color to change surface to.
     */
    private void getColorChange(Color color) {

        // change the material colors for the selected items
        int[] aiSelected = surfaceList.getSelectedIndices();

        Color3f black = new Color3f(Color.black);

        for (int i = 0; i < aiSelected.length; i++) {
            int iIndex = aiSelected[i];

            /** when surface color changed, record it here. */
            surColor = color;

            Color4f newColor = new Color4f(color);

            ((SurfaceAttributes) surfaceVector.get(iIndex)).setColor( newColor );
            /* Change the mask color: */
            parentScene.getImageA().addSurfaceMask( iIndex, null, null, newColor );
            /* update the surface renderer: */
            ((SurfaceRender) renderBase).updateData();

            if (((SurfaceAttributes) surfaceVector.get(iIndex)).getIsVOIPt()) {
                BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).getBranch();

                for (int k = 0; k < root.numChildren(); k++) {
                    Shape3D shape = (Shape3D)
                                        (((Sphere) (((TransformGroup) (root.getChild(k))).getChild(0))).getShape());
                    Appearance appearance = shape.getAppearance();
                    Material mat = new Material(black, new Color3f(color), black, black, 80f);

                    mat.setDiffuseColor(newColor.x, newColor.y, newColor.z);
                    mat.setSpecularColor(newColor.x, newColor.y, newColor.z);
                    mat.setAmbientColor(newColor.x, newColor.y, newColor.z);
                    mat.setEmissiveColor(newColor.x, newColor.y, newColor.z);
                    mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
                    appearance.setMaterial(mat);
                }
            } 
        }
    }

    /**
     * Initializes the GUI components.
     */
    private void init() {
        // setSize(400, 256);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());
        buildToolBar();
        iSelect = -1;

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

        JButton transButton = new JButton("Transform");

        transButton.addActionListener(this);
        transButton.setActionCommand("Transform");
        transButton.setFont(serif12B);
        transButton.setPreferredSize(MipavUtil.defaultButtonSize);

        buttonPanel.add(addButton);
        buttonPanel.add(removeButton);

        // list panel for surface filenames
        surfaceList = new JList();
        surfaceList.addListSelectionListener(this);
        surfaceList.setPrototypeCellValue("aaaaaaaaaaaaaaaa.aaa    ");

        JScrollPane kScrollPane = new JScrollPane(surfaceList);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(buttonPanel, BorderLayout.SOUTH);
        listPanel.setBorder(buildTitledBorder("Surface list"));

        JPanel paintTexturePanel = new JPanel();
        paintTexturePanel.setLayout(new FlowLayout(FlowLayout.LEFT));

        /* Creates the surface paint button, which launches the surface
         * dialog: */
        m_kTexturePaintButton = new JButton("Texture Paint Dialog");
        m_kTexturePaintButton.addActionListener(this);
        m_kTexturePaintButton.setActionCommand("TexturePaint");
        m_kTexturePaintButton.setSelected(false);
        m_kTexturePaintButton.setEnabled(false);
        //paintTexturePanel.add(m_kTexturePaintButton);


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

        opacitySlider = new JSlider(0, 100, 50);
        opacitySlider.setFont(serif12);
        opacitySlider.setMinorTickSpacing(10);
        opacitySlider.setPaintTicks(true);
        opacitySlider.addChangeListener(this);
        opacitySlider.addMouseListener(this);
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
        detailSlider.setEnabled(true);
        detailLabel.setEnabled(true);
        detailSliderLabels[0].setEnabled(true);
        detailSliderLabels[1].setEnabled(true);
        detailSliderLabels[2].setEnabled(true);

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

        JPanel cbSurfacePanel = new JPanel();

        cbSurfacePanel.setLayout(new BoxLayout(cbSurfacePanel, BoxLayout.Y_AXIS));
        cbSurfacePanel.add(surfacePickableCB);
        cbSurfacePanel.add(surfaceClipCB);
        cbSurfacePanel.add(surfaceBackFaceCB);

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

        rightPanel = new JPanel();
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
     * Check static light is enable or not.
     *
     * @return  staticLightEnable <code>true</code> means enable, <code>false</code> means disable.
     */
    private boolean isStaticLightEnabled() {
        return m_kLightsControl.getGeneralLight(JPanelLights.LIGHT_INDEX_STATIC).isEnabled();
    }

    /**
     * Indicates whether the static light bulb being picked by the mouse.
     *
     * @return  whether the static light bulb is being picked
     */
    private boolean isStaticPicked() {

        if (activeLightBulbIndex == 8) {
            return true;
        } else {
            return false;
        }
    }


    /**
     * Remove the surface from the volume render.
     */
    public void removeSurface() {
        SurfaceAttributes[] surfaces = getSelectedSurfaces( surfaceList.getSelectedIndices() );
        removeSurfaces( surfaces );

        if (((SurfaceRender) renderBase).getProbeDialog() != null) {
            ((SurfaceRender) renderBase).getProbeDialog().updateTargetList();
        }

        if (surfaceVector.size() <= 0) {
            ((SurfaceRender) renderBase).getGeodesicPanel().setEnabled(false);
        }
        ((SurfaceRender) renderBase).updateData();
    }

    /**
     * Remove the specified surface subtree from the scene graph.
     *
     * @param  root  The root of the subtree to remove.
     */
    public void removeSurface(BranchGroup root) {

        // remove the surface from the scene (if it exists)
        for (int i = 0; i < surfaceRootBG.numChildren(); i++) {

            if (surfaceRootBG.getChild(i) == root) {
                surfaceRootBG.removeChild(i);
            }
        }

        System.gc();
    }

    /**
     * The action taken when the Remove button is clicked in the list box. All
     * selected surfaces in the list box are removed from the scene graph.
     * @param surfaces[] the selected surfaces (SurfaceAttributes[]) to be removed.
     */
    private void removeSurfaces( SurfaceAttributes[] surfaces ) {

        // remove the items
        for ( int i = 0; i < surfaces.length; i++) {
            removeSurface( surfaces[i].getBranch() );

            // remove the surface from the image's file info if it is XML (so
            // that it won't be saved with it)
            if (parentScene.getParentFrame().getImageOriginal().getFileInfo()[0] instanceof FileInfoImageXML) {
                for (int s = 0; s < parentScene.getParentFrame().getImageOriginal().getExtents()[2]; s++) {
                    ((FileInfoImageXML) parentScene.getParentFrame().getImageOriginal().getFileInfo()[s]).removeSurface(surfaces[i].getFullPath());
                }
            }

            int removeIndex = surfaceVector.indexOf( surfaces[i] );
            surfaceVector.removeElementAt(removeIndex);
            parentScene.getImageA().removeSurfaceMask(removeIndex);
        }
        int[] selected = new int[1];
        selected[0] = iSelect >= surfaceVector.size() ? surfaceVector.size() - 1 : iSelect;
        updateSurfaceNameList( selected );

        if ( surfaceVector.size() == 0 )
        {
            // clear out the color values if the list is empty
            iSelect = -1;
            setElementsEnabled(false);
            colorButton.setBackground(getBackground());
            triangleText.setText("");
            volumeText.setText("");
            areaText.setText("");
        }
    }

    /**
     * Called when surfaces are added or removed from the surfaceVector
     * SurfaceAttributes list. Updates the surfaceList combo-box displayed in
     * the user-interface.
     * @param selected, array of names that are currently selected.
     */
    private void updateSurfaceNameList( int[] selected )
    {
        Vector surfaceNames = new Vector();
        for (Enumeration en = surfaceVector.elements(); en.hasMoreElements();) {
            surfaceNames.addElement(((SurfaceAttributes) en.nextElement()).getName());
        }
        surfaceList.setListData(surfaceNames);
        surfaceList.setSelectedIndices(selected);
    }


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
        m_kTexturePaintButton.setEnabled(flag);
        m_kSurfaceTextureButton.setEnabled(flag);
        m_kStereoButton.setEnabled(flag);
        detailLabel.setEnabled(flag);
        detailSlider.setEnabled(flag);
        opacityLabel.setEnabled(flag);
        opacitySlider.setEnabled(flag);
        triangleLabel.setEnabled(flag);
        triangleText.setEnabled(flag);
        volumeLabel.setEnabled(flag);
        volumeText.setEnabled(flag);
        areaLabel.setEnabled(flag);
        areaText.setEnabled(flag);

        for (int i = 0; i < detailSliderLabels.length; i++) {
            detailSliderLabels[i].setEnabled(flag);
        }

        for (int i = 0; i < opacitySliderLabels.length; i++) {
            opacitySliderLabels[i].setEnabled(flag);
        }

        polygonModeCB.setEnabled(flag);
        comboLabel.setEnabled(flag);
        surfacePickableCB.setEnabled(flag);
        surfaceClipCB.setEnabled(flag);
        surfaceBackFaceCB.setEnabled(flag);
        
        mSurfacePaint.setEnabled( flag );
        if ( ((SurfaceRender)parentScene).getSurfaceTexture() != null )
        {
            ((SurfaceRender)parentScene).getSurfaceTexture().setEnabled( flag );
        }
    }

    /**
     * Create and initialize the nine lights in the scene graph. The first eight lights are positioned at the eight
     * vertices of the cube [-1,1]^3. Light 0 is at (-1,-1,-1) and light 7 is at (1,1,1), both enabled by default. The
     * default color for all lights is white. The default intensity is 1. All surfaces in the scene are illuminated by
     * all enabled lights. The ninth light is ambient light, light that seems to come from all directions.
     *
     * @param  surfaceRoot  Transform group to attach lights to.
     */
    private void setupLights(TransformGroup surfaceRoot) {
        GeneralLight kGeneralLight;

        for (int i = 0; i < 8; i++) {
            kGeneralLight = m_kLightsControl.getGeneralLight(m_aiMapIndexToJPanelLightsIndex[i]);

            lightArray[i] = kGeneralLight.createJava3dLight();
            m_akLights[i] = kGeneralLight;

            // illuminate everything in voxel space
            lightArray[i].setInfluencingBounds(parentScene.bounds);
            lightArrayBG[i] = new BranchGroup();
            lightArrayBG[i].setCapability(BranchGroup.ALLOW_DETACH);
            lightArrayBG[i].addChild(lightArray[i]);
            surfaceRoot.addChild(lightArrayBG[i]);
        }

        kGeneralLight = m_kLightsControl.getGeneralLight(m_aiMapIndexToJPanelLightsIndex[8]);
        lightArray[8] = kGeneralLight.createJava3dLight();
        m_akLights[8] = kGeneralLight;
        lightArray[8].setInfluencingBounds(parentScene.bounds);
        lightArrayBG[8] = new BranchGroup();
        lightArrayBG[8].setCapability(BranchGroup.ALLOW_DETACH);
        lightArrayBG[8].addChild(lightArray[8]);
        surfaceRoot.addChild(lightArrayBG[8]);

        kGeneralLight = m_kLightsControl.getGeneralLight(m_aiMapIndexToJPanelLightsIndex[9]);
        lightArray[9] = kGeneralLight.createJava3dLight();
        m_akLights[9] = kGeneralLight;
        lightArray[9].setInfluencingBounds(parentScene.bounds);
        lightArrayBG[9] = new BranchGroup();
        lightArrayBG[9].setCapability(BranchGroup.ALLOW_DETACH);
        lightArrayBG[9].addChild(lightArray[9]);
        staticLightBG.addChild(lightArrayBG[9]);
    }


    /**
     * Takes the image's default transformation and transforms the surface by that transformation. This can be changed
     * in the future so that the user can have a better way of setting the transform.
     */
    private void transformSurface() {

        double[][] transform;

        transform = parentScene.getImageA().getMatrix().getArray();

        int selected = surfaceList.getSelectedIndex();

        if (selected == -1) {
            return;
        }

        BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).getBranch();

        root.detach();

        for (int j = 0; j < root.numChildren(); j++) {
            Shape3D shape = (Shape3D) root.getChild(j);
            ((ModelTriangleMesh) shape.getGeometry(0)).affineTransform(transform);
        }

        surfaceRootBG.addChild(root);
    }


    //~ Inner Classes --------------------------------------------------------------------------------------------------

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
            parentScene.getParentFrame().setFlythruColor(color);
        }
    }
}
