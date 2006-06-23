package gov.nih.mipav.view.renderer.surfaceview;


import gov.nih.mipav.model.file.*;
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
import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.border.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * A Java3D-based dialog for surfaces represented as triangle meshes. The surfaces are displayed in the 3D viewer that
 * already contains a 3D image. The mouse behavior is the same for the image as it is for the surfaces, so that when the
 * user zooms or rotates, it looks like the surfaces are part of the tri-image.
 *
 * @author  David Eberly
 * @author  Neva Cherniavsky
 */
public class JPanelSurface extends JPanelRendererBase
        implements ListSelectionListener, MouseListener, MouseMotionListener, ChangeListener { // for slider changes
                                                                                               // (LOD change)

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4600563188022683359L;

    /** Axis orientation unknown. */
    public static final int ORI_UNKNOWN_TYPE = 0;

    /** Axis orientation Right to Left. */
    public static final int ORI_R2L_TYPE = 1;

    /** Axis orientation Left to Right. */
    public static final int ORI_L2R_TYPE = 2;

    /** Axis orientation Posterior to Anterior. */
    public static final int ORI_P2A_TYPE = 3;

    /** Axis orientation Anterior to Posterior. */
    public static final int ORI_A2P_TYPE = 4;

    /** Axis orientation Inferior to Superior. */
    public static final int ORI_I2S_TYPE = 5;

    /** Axis orientation Superior to Inferior. */
    public static final int ORI_S2I_TYPE = 6;

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

    /** Surface vertext conectin array. */
    protected int[] m_aiConnect;

    /** Bitset mask to recording the surface boundary. */
    protected BitSet m_aiMask;

    /** Surface vertex array. */
    protected Point3f[] m_akVertex;

    /** Product of the image dimensions. */
    protected int m_iQuantity;

    /** DOCUMENT ME! */
    protected int m_iTQuantity;

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

    /** Surface load button for the ascii surface file format. */
    private JButton loadASCButton;

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

    /** per-vertex color for the m_aiMask:. */
    private Color3f[] m_akMaskColor = null;

    /** Dimensions of image A. */
    private int m_iXBound, m_iYBound, m_iZBound;

    /** The material options button, which launches the material editor window. */
    private JButton m_kAdvancedMaterialOptionsButton;

    /** For drawing the geodesic lines on the triangle mesh:. */
    private BranchGroup m_kGeodesicGroup = null;

    /** Light dialog for when the user clicks the light button. */
    private JPanelLights m_kLightsControl;

    /** For drawing the flythru path in the triangle mesh:. */
    private TransformGroup m_kPathPositionTG = null;

    /** Stereo render button, launches the JStereoWindow for viewing the ModelTriangleMesh in stereo:. */
    private JButton m_kStereoButton;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kSurface = null;

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

    /**
     * The colors for the first six surfaces are fixed. For the seventh and later surface, the colors are randomly
     * generated. This member provides the random number generator.
     */
    private Random randomGen;

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
    private BitSet surfaceMask;

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

    /** Parent frame box value. */
    private float xBox, yBox, zBox;

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
    public JPanelSurface(SurfaceRender parent, Canvas3D canvas, TransformGroup surfaceRoot, float xBox, float yBox,
                         float zBox) {

        super(parent);
        parentScene = parent;
        this.xBox = xBox;
        this.yBox = yBox;
        this.zBox = zBox;
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
        int iterations;
        boolean volumeLimit;
        float volumePercent;
        String command = event.getActionCommand();
        float[] resolution = parentScene.getImageA().getFileInfo(0).getResolutions();
        int[] extents = parentScene.getImageA().getExtents();
        float[] box = new float[3];

        box[0] = extents[0] * resolution[0];
        box[1] = extents[1] * resolution[1];
        box[2] = extents[2] * resolution[2];

        float maxBox = Math.max(box[0], Math.max(box[1], box[2]));

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

            /* For each file in the selected list, launch the stereo viewer: */
            int[] aiSelected = surfaceList.getSelectedIndices();

            for (int i = 0; i < aiSelected.length; i++) {
                int iIndex = aiSelected[i];

                if (!((SurfaceAttributes) surfaceVector.get(iIndex)).isVOIPt) {

                    /* Use the current view transform: */
                    Transform3D kTransform = new Transform3D();
                    parentScene.getSceneRootTG().getTransform(kTransform);

                    /* Pass in a new copy of the triangle mesh: */
                    new JStereoWindow(new ModelTriangleMesh(m_kSurface.getVertexCopy(), m_kSurface.getNormalCopy(),
                                                            m_kSurface.getIndexCopy()), kTransform);
                }
            }
        } else if (command.equals("AdvancedMaterialOptions")) {

            /* For each file in the selected list, launch the material editor window */
            int[] aiSelected = surfaceList.getSelectedIndices();

            for (int i = 0; i < aiSelected.length; i++) {
                int iIndex = aiSelected[i];

                if (!((SurfaceAttributes) surfaceVector.get(iIndex)).isVOIPt) {
                    BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;
                    Shape3D shape = (Shape3D) root.getChild(0);

                    /* Pass in the current surface material and transparency values */
                    Appearance appearance = shape.getAppearance();
                    Material material = appearance.getMaterial();
                    TransparencyAttributes tap = appearance.getTransparencyAttributes();
                    float fOpacity = tap.getTransparency();

                    /* Launch the material editor: */
                    new JFrameSurfaceMaterialProperties(this, iIndex, m_akLights, fOpacity, material);
                }
            }
        } else if (command.equals("ChangeLight")) {

            if (m_kLightsControl == null) {
                m_kLightsControl = new JPanelLights(this, parentScene);
            }

            m_kLightsControl.setVisible(true);
        } else if (command.equals("SurfacePickable")) {
            int selected = surfaceList.getSelectedIndex();

            if (selected == -1) {
                return;
            }

            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;

            root.setPickable(surfacePickableCB.isSelected());
        } else if (command.equals("backface")) {
            int selected = surfaceList.getSelectedIndex();

            if (selected == -1) {
                return;
            }

            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;

            if (root != null) {
                root.setCollidable(surfaceBackFaceCB.isSelected());

                Shape3D shape = (Shape3D) root.getChild(0);

                if (surfaceBackFaceCB.isSelected()) {
                    shape.getAppearance().getPolygonAttributes().setCullFace(PolygonAttributes.CULL_BACK);
                } else {
                    shape.getAppearance().getPolygonAttributes().setCullFace(PolygonAttributes.CULL_NONE);
                }
            }
        } else if (command.equals("Clipping") && ((SurfaceRender) parentScene).getDisplayMode3D()) {
            int selected = surfaceList.getSelectedIndex();

            if (selected == -1) {
                return;
            }

            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;

            if (root != null) {
                root.setAlternateCollisionTarget(surfaceClipCB.isSelected());

                if (surfaceClipCB.isSelected()) {
                    parentScene.getClipDialog().addToModelClip(root);
                } else {
                    parentScene.getClipDialog().removeFromModelClip(root);
                }
            }
        } else if (command.equals("ChangePolyMode")) {
            int mode;

            switch (polygonModeCB.getSelectedIndex()) {

                case 1:
                    mode = PolygonAttributes.POLYGON_LINE;
                    break;

                case 2:
                    mode = PolygonAttributes.POLYGON_POINT;
                    break;

                case 0:
                default:
                    mode = PolygonAttributes.POLYGON_FILL;
                    break;
            }

            changePolyMode(mode);
        } else if (command.equals("ImportA")) {
            importA();
        } else if (command.equals("LevelS") || command.equals("LevelV")) {
            int selected = surfaceList.getSelectedIndex();

            if (selected == -1) {
                MipavUtil.displayError("Select a surface to save.");

                return;
            }

            BranchGroup surfaceBG = ((SurfaceAttributes) surfaceVector.get(selected)).surface;

            ModelTriangleMesh[] meshes = new ModelTriangleMesh[surfaceBG.numChildren()];

            for (int j = 0; j < surfaceBG.numChildren(); j++) {
                Shape3D shape = (Shape3D) surfaceBG.getChild(j);

                meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
            }

            Color3f color = new Color3f();

            ((Shape3D) (surfaceBG.getChild(0))).getAppearance().getMaterial().getDiffuseColor(color);
            saveSingleMesh(meshes, command.equals("LevelS"), color);
        } else if (command.equals("LevelW")) {
            int selected, i, j;
            String extension;
            int[] selectedList = surfaceList.getSelectedIndices();
            String name = getFileName(false);

            if (name == null) {
                return;
            }

            i = name.lastIndexOf('.');

            if ((i > 0) && (i < (name.length() - 1))) {
                extension = name.substring(i + 1).toLowerCase();

                if (!extension.equals("wrl")) {
                    MipavUtil.displayError("Extension must be .wrl");

                    return;
                }
            } else {
                name = name + ".wrl";
            }

            try {
                PrintWriter kOut = new PrintWriter(new FileWriter(name));

                kOut.println("#VRML V2.0 utf8");
                kOut.println("#MIPAV");
                kOut.println("#Number of shapes = " + selectedList.length);

                kOut.print("#flip { ");

                if (true) {
                    kOut.print(1);
                } else {
                    kOut.print(0);
                }

                kOut.print(" }\n");

                float[] startLocation = parentScene.getImageA().getFileInfo(0).getOrigin();

                resolution = parentScene.getImageA().getFileInfo(0).getResolutions();
                extents = parentScene.getImageA().getExtents();
                box = new float[3];
                box[0] = extents[0] * resolution[0];
                box[1] = extents[1] * resolution[1];
                box[2] = extents[2] * resolution[2];
                maxBox = Math.max(box[0], Math.max(box[1], box[2]));

                int[] axisOrientation = parentScene.getImageA().getFileInfo(0).getAxisOrientation();
                int[] direction = new int[] { 1, 1, 1 };

                for (i = 0; i <= 2; i++) {

                    if ((axisOrientation[i] == ORI_L2R_TYPE) || (axisOrientation[i] == ORI_P2A_TYPE) ||
                            (axisOrientation[i] == ORI_S2I_TYPE)) {
                        direction[i] = -1;
                    }
                }

                kOut.print("#direction { ");
                kOut.print(direction[0]);
                kOut.print(' ');
                kOut.print(direction[1]);
                kOut.print(' ');
                kOut.print(direction[2]);
                kOut.print(" }\n");

                kOut.print("#startLocation { ");
                kOut.print(startLocation[0]);
                kOut.print(' ');
                kOut.print(startLocation[1]);
                kOut.print(' ');
                kOut.print(startLocation[2]);
                kOut.print(" }\n");

                kOut.print("#box { ");
                kOut.print(box[0]);
                kOut.print(' ');
                kOut.print(box[1]);
                kOut.print(' ');
                kOut.print(box[2]);
                kOut.print(" }\n");

                for (i = 0; i < selectedList.length; i++) {
                    selected = selectedList[i];

                    if (selected == -1) {
                        MipavUtil.displayError("Select a surface to save.");

                        return;
                    }

                    BranchGroup surfaceBG = ((SurfaceAttributes) surfaceVector.get(selected)).surface;
                    ModelTriangleMesh[] meshes = new ModelTriangleMesh[surfaceBG.numChildren()];

                    for (j = 0; j < surfaceBG.numChildren(); j++) {
                        Shape3D shape = (Shape3D) surfaceBG.getChild(j);

                        meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
                    }

                    Color3f color = new Color3f();

                    ((Shape3D) (surfaceBG.getChild(0))).getAppearance().getMaterial().getDiffuseColor(color);
                    savePortableMesh(meshes, kOut, color);
                }

                kOut.close();
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        } else if (command.equals("LevelXML")) {
            int iIndex = surfaceList.getSelectedIndex();

            if (iIndex == -1) {
                MipavUtil.displayError("Select a surface to save.");

                return;
            }

            if (!((SurfaceAttributes) surfaceVector.get(iIndex)).isVOIPt) {
                BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;
                Shape3D shape = (Shape3D) root.getChild(0);
                ModelTriangleMesh kMesh = (ModelTriangleMesh) shape.getGeometry(0);
                writeTriangleMeshXML(kMesh, getMaterial(iIndex));
            }
        } else if (command.equals("saveSurface")) {

            // save surface
            int selected = surfaceList.getSelectedIndex();
            BranchGroup surfaceBG = ((SurfaceAttributes) surfaceVector.get(selected)).surface;

            ModelTriangleMesh[] meshes = new ModelTriangleMesh[surfaceBG.numChildren()];

            for (int j = 0; j < surfaceBG.numChildren(); j++) {
                Shape3D shape = (Shape3D) surfaceBG.getChild(j);

                meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
            }

            Color3f color = new Color3f();

            ((Shape3D) (surfaceBG.getChild(0))).getAppearance().getMaterial().getDiffuseColor(color);

            String fName = ((SurfaceAttributes) surfaceVector.get(selected)).name;

            saveSingleMesh(fName, meshes, command.equals("LevelS"), color);

            // Load surface
            File file = new File(getCurrentFileName(fName));
            Color4f surfaceColor = new Color4f();
            int index = surfaceVector.size();

            surfaceColor.w = 1.0f;

            if ((index < fixedColor.length) && (selected < fixedColor.length)) {

                // Use the fixed colors for the first six surfaces.
                surfaceColor.x = fixedColor[selected].x;
                surfaceColor.y = fixedColor[selected].y;
                surfaceColor.z = fixedColor[selected].z;
            } else {

                // Use randomly generated colors for the seventh and
                // later surfaces.
                surfaceColor.x = 0.5f * (1.0f + randomGen.nextFloat());
                surfaceColor.y = 0.5f * (1.0f + randomGen.nextFloat());
                surfaceColor.z = 0.5f * (1.0f + randomGen.nextFloat());
            }

            addSurface(file, surfaceColor, fName, 0.5f, selected, false);

            surfaceList.setSelectedIndex(selected);

            // smooth surface
            actionPerformed(new ActionEvent(this, 1, "Smoothies"));
        } else if (command.equals("Smoothies")) {

            int selected = surfaceList.getSelectedIndex();

            if (selected == -1) {
                MipavUtil.displayError("Select a surface to smooth.");

                return;
            }

            // Popup smooth dialog 3
            JDialogSurfaceSmooth3 dialog = new JDialogSurfaceSmooth3(((SurfaceRender) renderBase).getParentFrame());
            float lambda, mu;

            if (dialog.isCancelled()) {
                return;
            } else {
                iterations = dialog.getIterations();
                lambda = dialog.getLambda();
                mu = dialog.getMu();
            }

            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;
            ModelTriangleMesh[] meshes = new ModelTriangleMesh[root.numChildren()];

            for (int j = 0; j < root.numChildren(); j++) {
                Shape3D shape = (Shape3D) root.getChild(j);

                meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
            }

            root.detach();

            ModelClodMesh clod = null;
            int numTriangles = 0;
            float volume = 0;
            float area = 0;

            for (int j = 0; j < meshes.length; j++) {

                if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                    clod = (ModelClodMesh) meshes[j].getGenerator();
                    clod.setLOD(clod.getMaximumLOD());
                    meshes[j] = clod.getMesh();
                }

                meshes[j].smoothThree(iterations, lambda, mu, true);
                meshes[j].computeNormals();

                if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                    clod.setVerticies(meshes[j].getVertexCopy());
                }

                numTriangles += meshes[j].getIndexCount();
                volume += meshes[j].volume();
                area += meshes[j].area();
            }

            root = createSurface(meshes, ((SurfaceAttributes) surfaceVector.get(selected)).color,
                                 ((SurfaceAttributes) surfaceVector.get(selected)).polygonMode);
            root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            root.setCapability(Group.ALLOW_CHILDREN_READ);
            root.setCapability(Group.ALLOW_CHILDREN_WRITE);
            root.setCapability(BranchGroup.ALLOW_DETACH);
            root.setCapability(Node.ALLOW_PICKABLE_READ);
            root.setCapability(Node.ALLOW_PICKABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_READ);
            root.setPickable(false);
            surfacePickableCB.setSelected(false);

            if (((SurfaceRender) parentScene).getDisplayMode3D()) {
                surfaceClipCB.setSelected(false);
            }

            root.compile();
            surfaceRootBG.addChild(root);
            ((SurfaceAttributes) surfaceVector.get(selected)).surface = root;
            ((SurfaceAttributes) surfaceVector.get(selected)).volume = volume;
            ((SurfaceAttributes) surfaceVector.get(selected)).area = area;
            ((SurfaceAttributes) surfaceVector.get(selected)).triangles = numTriangles / 3;

            if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                detailSlider.setValue(((SurfaceAttributes) surfaceVector.get(selected)).detailLevel);
                stateChanged(new ChangeEvent(detailSlider));
            }

            triangleText.setText(String.valueOf(numTriangles / 3));

            // One length across the extracted surface changes from -1 to 1
            // while one length across the actual volume changes by ((dim-1)*res)max
            volumeText.setText(String.valueOf(volume * maxBox * maxBox * maxBox / 8.0f));
            areaText.setText(String.valueOf(area * maxBox * maxBox));
        } else if (command.equals("Smooth")) {
            int selected = surfaceList.getSelectedIndex();

            if (selected == -1) {
                MipavUtil.displayError("Select a surface to smooth.");

                return;
            }

            JDialogSurfaceSmooth dialog = new JDialogSurfaceSmooth(((SurfaceRender) renderBase).getParentFrame());
            float alpha;

            if (dialog.isCancelled()) {
                return;
            } else {
                alpha = dialog.getAlpha();
                iterations = dialog.getIterations();
                volumeLimit = dialog.getVolumeLimit();
                volumePercent = dialog.getVolumePercent();
            }

            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;
            ModelTriangleMesh[] meshes = new ModelTriangleMesh[root.numChildren()];

            for (int j = 0; j < root.numChildren(); j++) {
                Shape3D shape = (Shape3D) root.getChild(j);

                meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
            }

            root.detach();

            ModelClodMesh clod = null;

            ((SurfaceAttributes) surfaceVector.get(selected)).surface = root;

            int numTriangles = 0;
            float volume = 0;
            float area = 0;

            for (int j = 0; j < meshes.length; j++) {

                if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                    clod = (ModelClodMesh) meshes[j].getGenerator();
                    clod.setLOD(clod.getMaximumLOD());
                    meshes[j] = clod.getMesh();
                }

                meshes[j].smoothMesh(iterations, alpha, volumeLimit, volumePercent, true);
                meshes[j].computeNormals();

                if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                    clod.setVerticies(meshes[j].getVertexCopy());
                }

                numTriangles += meshes[j].getIndexCount();
                volume += meshes[j].volume();
                area += meshes[j].area();
            }

            root = createSurface(meshes, ((SurfaceAttributes) surfaceVector.get(selected)).color,
                                 ((SurfaceAttributes) surfaceVector.get(selected)).polygonMode);
            root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            root.setCapability(Group.ALLOW_CHILDREN_READ);
            root.setCapability(Group.ALLOW_CHILDREN_WRITE);
            root.setCapability(BranchGroup.ALLOW_DETACH);
            root.setCapability(Node.ALLOW_PICKABLE_READ);
            root.setCapability(Node.ALLOW_PICKABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_READ);
            root.setPickable(false);
            surfacePickableCB.setSelected(false);

            if (((SurfaceRender) parentScene).getDisplayMode3D()) {
                surfaceClipCB.setSelected(false);
            }

            root.compile();
            surfaceRootBG.addChild(root);
            ((SurfaceAttributes) surfaceVector.get(selected)).surface = root;
            ((SurfaceAttributes) surfaceVector.get(selected)).volume = volume;
            ((SurfaceAttributes) surfaceVector.get(selected)).area = area;
            ((SurfaceAttributes) surfaceVector.get(selected)).triangles = numTriangles / 3;

            if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                detailSlider.setValue(((SurfaceAttributes) surfaceVector.get(selected)).detailLevel);
                stateChanged(new ChangeEvent(detailSlider));
            }

            triangleText.setText(String.valueOf(numTriangles / 3));

            // One length across the extracted surface changes from -1 to 1
            // while one length across the actual volume changes by ((dim-1)*res)max
            volumeText.setText(String.valueOf(volume * maxBox * maxBox * maxBox / 8.0f));
            areaText.setText(String.valueOf(area * maxBox * maxBox));
        } else if (command.equals("Smooth2")) {
            int selected = surfaceList.getSelectedIndex();

            if (selected == -1) {
                MipavUtil.displayError("Select a surface to smooth.");

                return;
            }

            // Smooth dialog 2
            JDialogSurfaceSmooth2 dialog = new JDialogSurfaceSmooth2(((SurfaceRender) renderBase).getParentFrame());
            float stiffness;

            if (dialog.isCancelled()) {
                return;
            } else {
                iterations = dialog.getIterations();
                stiffness = dialog.getStiffness();
                volumeLimit = dialog.getVolumeLimit();
                volumePercent = dialog.getVolumePercent();
            }

            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;
            ModelTriangleMesh[] meshes = new ModelTriangleMesh[root.numChildren()];

            for (int j = 0; j < root.numChildren(); j++) {
                Shape3D shape = (Shape3D) root.getChild(j);

                meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
            }

            root.detach();

            ModelClodMesh clod = null;
            int numTriangles = 0;
            float volume = 0;
            float area = 0;

            for (int j = 0; j < meshes.length; j++) {

                if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                    clod = (ModelClodMesh) meshes[j].getGenerator();
                    clod.setLOD(clod.getMaximumLOD());
                    meshes[j] = clod.getMesh();
                }

                meshes[j].smoothTwo(iterations, stiffness, volumeLimit, volumePercent, true);
                meshes[j].computeNormals();

                if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                    clod.setVerticies(meshes[j].getVertexCopy());
                }

                numTriangles += meshes[j].getIndexCount();
                volume += meshes[j].volume();
                area += meshes[j].area();
            }

            root = createSurface(meshes, ((SurfaceAttributes) surfaceVector.get(selected)).color,
                                 ((SurfaceAttributes) surfaceVector.get(selected)).polygonMode);
            root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            root.setCapability(Group.ALLOW_CHILDREN_READ);
            root.setCapability(Group.ALLOW_CHILDREN_WRITE);
            root.setCapability(BranchGroup.ALLOW_DETACH);
            root.setCapability(Node.ALLOW_PICKABLE_READ);
            root.setCapability(Node.ALLOW_PICKABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_READ);
            root.setPickable(false);
            surfacePickableCB.setSelected(false);

            if (((SurfaceRender) parentScene).getDisplayMode3D()) {
                surfaceClipCB.setSelected(false);
            }

            root.compile();
            surfaceRootBG.addChild(root);
            ((SurfaceAttributes) surfaceVector.get(selected)).surface = root;
            ((SurfaceAttributes) surfaceVector.get(selected)).volume = volume;
            ((SurfaceAttributes) surfaceVector.get(selected)).area = area;
            ((SurfaceAttributes) surfaceVector.get(selected)).triangles = numTriangles / 3;

            if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                detailSlider.setValue(((SurfaceAttributes) surfaceVector.get(selected)).detailLevel);
                stateChanged(new ChangeEvent(detailSlider));
            }

            triangleText.setText(String.valueOf(numTriangles / 3));

            // One length across the extracted surface changes from -1 to 1
            // while one length across the actual volume changes by ((dim-1)*res)max
            volumeText.setText(String.valueOf(volume * maxBox * maxBox * maxBox / 8.0f));
            areaText.setText(String.valueOf(area * maxBox * maxBox));
        } else if (command.equals("Smooth3")) {
            int selected = surfaceList.getSelectedIndex();

            if (selected == -1) {
                MipavUtil.displayError("Select a surface to smooth.");

                return;
            }

            JDialogSurfaceSmooth3 dialog = new JDialogSurfaceSmooth3(((SurfaceRender) renderBase).getParentFrame());
            float lambda, mu;

            if (dialog.isCancelled()) {
                return;
            } else {
                iterations = dialog.getIterations();
                lambda = dialog.getLambda();
                mu = dialog.getMu();
            }

            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;
            ModelTriangleMesh[] meshes = new ModelTriangleMesh[root.numChildren()];

            for (int j = 0; j < root.numChildren(); j++) {
                Shape3D shape = (Shape3D) root.getChild(j);

                meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
            }

            root.detach();

            ModelClodMesh clod = null;
            int numTriangles = 0;
            float volume = 0;
            float area = 0;

            for (int j = 0; j < meshes.length; j++) {

                if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                    clod = (ModelClodMesh) meshes[j].getGenerator();
                    clod.setLOD(clod.getMaximumLOD());
                    meshes[j] = clod.getMesh();
                }

                meshes[j].smoothThree(iterations, lambda, mu, true);
                meshes[j].computeNormals();

                if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                    clod.setVerticies(meshes[j].getVertexCopy());
                }

                numTriangles += meshes[j].getIndexCount();
                volume += meshes[j].volume();
                area += meshes[j].area();
            }

            root = createSurface(meshes, ((SurfaceAttributes) surfaceVector.get(selected)).color,
                                 ((SurfaceAttributes) surfaceVector.get(selected)).polygonMode);
            root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            root.setCapability(Group.ALLOW_CHILDREN_READ);
            root.setCapability(Group.ALLOW_CHILDREN_WRITE);
            root.setCapability(BranchGroup.ALLOW_DETACH);
            root.setCapability(Node.ALLOW_PICKABLE_READ);
            root.setCapability(Node.ALLOW_PICKABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_READ);
            root.setPickable(false);
            surfacePickableCB.setSelected(false);

            if (((SurfaceRender) parentScene).getDisplayMode3D()) {
                surfaceClipCB.setSelected(false);
            }

            root.compile();
            surfaceRootBG.addChild(root);
            ((SurfaceAttributes) surfaceVector.get(selected)).surface = root;
            ((SurfaceAttributes) surfaceVector.get(selected)).volume = volume;
            ((SurfaceAttributes) surfaceVector.get(selected)).area = area;
            ((SurfaceAttributes) surfaceVector.get(selected)).triangles = numTriangles / 3;

            if (((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh == true) {
                detailSlider.setValue(((SurfaceAttributes) surfaceVector.get(selected)).detailLevel);
                stateChanged(new ChangeEvent(detailSlider));
            }

            triangleText.setText(String.valueOf(numTriangles / 3));

            // One length across the extracted surface changes from -1 to 1
            // while one length across the actual volume changes by ((dim-1)*res)max
            volumeText.setText(String.valueOf(volume * maxBox * maxBox * maxBox / 8.0f));
            areaText.setText(String.valueOf(area * maxBox * maxBox));
        } else if (command.equals("Decimate")) {
            decimate();
        }

    }

    /**
     * Add any attached surfaces the current image has in its file info (if the file info is in the xml format).
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
                    String surfaceName = surfaceFile.getName();
                    float surfaceOpacity = fileInfo.getSurface(surfaceFileName).getOpacity();

                    int index = surfaceVector.size();

                    Color4f surfaceColor = new Color4f();

                    surfaceColor.w = 1.0f;

                    if (index < fixedColor.length) {

                        // Use the fixed colors for the first six surfaces.
                        surfaceColor.x = fixedColor[index].x;
                        surfaceColor.y = fixedColor[index].y;
                        surfaceColor.z = fixedColor[index].z;
                    } else {

                        // Use randomly generated colors for the seventh and
                        // later surfaces.
                        surfaceColor.x = 0.5f * (1.0f + randomGen.nextFloat());
                        surfaceColor.y = 0.5f * (1.0f + randomGen.nextFloat());
                        surfaceColor.z = 0.5f * (1.0f + randomGen.nextFloat());
                    }

                    addSurface(surfaceFile, surfaceColor, surfaceName, surfaceOpacity, -1, true);
                    addedNames.add(surfaceName);

                    /* Tell the surfaceRenderer to add the triangle mesh surface: */
                    ((SurfaceRender) renderBase).surfaceAdded();
                } else {
                    MipavUtil.displayError("Cannot add two surfaces with the same name.");
                }
            }

            surfaceList.setListData(addedNames);
        }
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
            m_kSurface = kMesh;
            surColor = Color.RED;

            /** mask the surface added. */
            maskInsideVoxels(getSurface(), true);

            /* Tell the surfaceRenderer to add the triangle mesh surface: */
            ((SurfaceRender) renderBase).branchSurfaceAdded();
            ((SurfaceRender) renderBase).branchSetColor(new Color4f(1, 0, 0, 1));
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

        /* Find the original mesh, kOld, and remove it from the surfaceVector
         * and surfaceList, adding the new mesh kNew in it's place: */
        boolean bMeshFound = false;
        BranchGroup kSurfaceBG;

        for (int i = 0; i < surfaceList.getModel().getSize(); i++) {
            kSurfaceBG = ((SurfaceAttributes) surfaceVector.get(i)).surface;

            for (Enumeration e = kSurfaceBG.getAllChildren(); e.hasMoreElements();) {
                Object kObj = e.nextElement();

                if (kObj instanceof Shape3D) {
                    Shape3D kShape = (Shape3D) kObj;

                    if (kShape.getGeometry() instanceof ModelTriangleMesh) {
                        ModelTriangleMesh kChildMesh = (ModelTriangleMesh) (kShape.getGeometry());

                        if (kChildMesh == kOld) {
                            bMeshFound = true;

                            break;
                        }
                    }
                }
            }
        }

        if (!bMeshFound) {
            return;
        }

        int index = surfaceVector.size();

        /* Determine the surface color: */
        Color4f surfaceColor = new Color4f();

        surfaceColor.w = 1.0f;

        if (index < fixedColor.length) {

            /* Use the fixed colors for the first six surfaces. */
            surfaceColor.x = fixedColor[index].x;
            surfaceColor.y = fixedColor[index].y;
            surfaceColor.z = fixedColor[index].z;
        } else {

            /* Use randomly generated colors for the seventh and later
             * surfaces. */
            surfaceColor.x = 0.5f * (1.0f + randomGen.nextFloat());
            surfaceColor.y = 0.5f * (1.0f + randomGen.nextFloat());
            surfaceColor.z = 0.5f * (1.0f + randomGen.nextFloat());
        }

        ModelTriangleMesh[] akComponent = new ModelTriangleMesh[1];

        akComponent[0] = kNew;

        /* Create a new surface mesh, and add it to the BranchGroup: */
        BranchGroup root = createSurface(akComponent, surfaceColor, PolygonAttributes.POLYGON_FILL);

        root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        root.setCapability(Group.ALLOW_CHILDREN_READ);
        root.setCapability(Group.ALLOW_CHILDREN_WRITE);
        root.setCapability(BranchGroup.ALLOW_DETACH);
        root.setCapability(Node.ALLOW_PICKABLE_READ);
        root.setCapability(Node.ALLOW_PICKABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_READ);
        root.setPickable(true);
        root.compile();

        /* Add the new BranchGroup and mesh to the scene: */
        surfaceRootBG.addChild(root);

        /* Set the m_kSurface data member: */
        m_kSurface = kNew;

        /* Add the mesh information to the surfaceVector: */
        int iNumTriangles = kNew.getIndexCount() / 3;
        SurfaceAttributes kSurfaceAttributes = new SurfaceAttributes(root, "", kName, surfaceColor, 64, 100,
                                                                     PolygonAttributes.POLYGON_FILL, iNumTriangles,
                                                                     1.0f, 1.0f, false, 1.0f, new Point3f(), null);

        surfaceVector.add(kSurfaceAttributes);

        /* Add to the surfaceList: */
        int iNameIndex = 0;
        int iIndex = 0;
        Vector surfaceNames = new Vector();

        for (Enumeration en = surfaceVector.elements(); en.hasMoreElements();) {
            String kElementName = ((SurfaceAttributes) en.nextElement()).name;

            surfaceNames.addElement(kElementName);

            if (kElementName.equals(kName)) {
                iNameIndex = iIndex;
            }

            iIndex++;
        }

        surfaceList.setListData(surfaceNames);
        surfaceList.setSelectedIndex(iNameIndex);

        /* Update the other renderers: */
        if (((SurfaceRender) renderBase).getProbeDialog() != null) {
            ((SurfaceRender) renderBase).getProbeDialog().updateTargetList();
        }

        /* Tell the surfaceRenderer to add the triangle mesh surface: */
        ((SurfaceRender) renderBase).surfaceAdded();

        int[] aiSelected = surfaceList.getSelectedIndices();

        for (iIndex = 0; iIndex < aiSelected.length; iIndex++) {
            ((SurfaceRender) parentScene).setColor(aiSelected[iIndex],
                                                   ((SurfaceAttributes) surfaceVector.get(aiSelected[iIndex])).color);
        }
    }

    /**
     * Add surface to the volume image.
     */
    public void addSurface() {

        try {
            addSurfaces();

            if (((SurfaceRender) renderBase).getProbeDialog() != null) {
                ((SurfaceRender) renderBase).getProbeDialog().updateTargetList();
            }

            if (surfaceVector.size() > 0) {
                ((SurfaceRender) renderBase).getGeodesicPanel().setEnabled(true);
            }

            int[] aiSelected = surfaceList.getSelectedIndices();

            for (int iIndex = 0; iIndex < aiSelected.length; iIndex++) {
                ((SurfaceRender) parentScene).setColor(aiSelected[iIndex],
                                                       ((SurfaceAttributes) surfaceVector.get(aiSelected[iIndex])).color);
            }
        } catch (IOException e) { }

    }

    /**
     * Adding the surface with specific directory and file name.
     *
     * @param   dir   directory name
     * @param   file  file name
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void addSurfaces(String dir, File file) throws IOException {

        ViewUserInterface.getReference().setDefaultDirectory(dir);
        surfaceDirectoryName = dir;

        // only add surfaces that are not already in the list
        boolean bNameAdded = false;

        // for ( int i = 0; i < files.length; i++ ) {
        String kName = file.getName();

        bNameAdded = true;

        int index = surfaceVector.size();

        Color4f color = new Color4f();

        color.w = 1.0f;

        if (index < fixedColor.length) {

            // Use the fixed colors for the first six surfaces.
            color.x = fixedColor[index].x;
            color.y = fixedColor[index].y;
            color.z = fixedColor[index].z;
        } else {

            // Use randomly generated colors for the seventh and ater surfaces.
            color.x = 0.5f * (1.0f + randomGen.nextFloat());
            color.y = 0.5f * (1.0f + randomGen.nextFloat());
            color.z = 0.5f * (1.0f + randomGen.nextFloat());
        }

        // add the surface to the scene graph
        addSurface(file, color, kName, 0.5f, -1, false);

        // add the surface to the image's file info if it is XML (so it can be saved)
        if (parentScene.getParentFrame().getImageOriginal().getFileInfo()[0] instanceof FileInfoImageXML) {

            for (int s = 0; s < parentScene.getParentFrame().getImageOriginal().getExtents()[2]; s++) {
                ((FileInfoImageXML) parentScene.getParentFrame().getImageOriginal().getFileInfo()[s]).addSurface(surfaceDirectoryName +
                                                                                                                 kName);
            }
        }

        if (surfaceList.getSelectedIndices().length == 1) {
            setElementsEnabled(true);
        }

        /* Tell the surfaceRenderer to add the triangle mesh surface: */
        ((SurfaceRender) renderBase).surfaceAdded();

        // if new surfaces were loaded, update the list data
        if (bNameAdded) {
            Vector surfaceNames = new Vector();

            for (Enumeration en = surfaceVector.elements(); en.hasMoreElements();) {
                surfaceNames.addElement(((SurfaceAttributes) en.nextElement()).name);
            }

            surfaceList.setListData(surfaceNames);

            int[] aiSelected = surfaceList.getSelectedIndices();

            if (aiSelected.length == 0) {
                iSelect = 0;
                surfaceList.setSelectedIndex(0);
            }
        }

        if (surfaceVector.size() > 0) {
            ((SurfaceRender) renderBase).getGeodesicPanel().setEnabled(true);
        }
    }

    /**
     * Disable back face culling.
     */
    public void disableBackFaceCulling() {
        int selected = surfaceList.getSelectedIndex();

        if (selected == -1) {
            return;
        }

        BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;

        if (root != null) {
            root.setCollidable(surfaceBackFaceCB.isSelected());

            Shape3D shape = (Shape3D) root.getChild(0);

            shape.getAppearance().getPolygonAttributes().setCullFace(PolygonAttributes.CULL_NONE);
        }
    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        parentScene = null;
        surfaceVector = null;
        loadASCButton = null;
        levelSButton = null;
        levelVButton = null;
        levelWButton = null;
        decimateButton = null;
        polygonModeCB = null;
        surfaceList = null;
        colorButton = null;
        colorLabel = null;
        m_kAdvancedMaterialOptionsButton = null;
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
        randomGen = null;
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

        m_aiMask = null;
        m_akMaskColor = null;
        m_akVertex = null;
        m_aiConnect = null;

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

        if (!kEvent.isShiftDown() && parentScene.getParentFrame().isGeodesicEnable()) {
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
            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;
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
     * Get the current added surface bit mask.
     *
     * @return  BitSet surface bit set mask
     */
    public BitSet getMask() {
        return m_aiMask;
    }

    /**
     * Get the current added surface mask color.
     *
     * @return  Color3f[]
     */
    public Color3f[] getMaskColor() {
        return m_akMaskColor;
    }

    /**
     * Get the surface material reference.
     *
     * @param   iIndex  material index.
     *
     * @return  Material material reference.
     */
    public Material getMaterial(int iIndex) {

        if (!((SurfaceAttributes) surfaceVector.get(iIndex)).isVOIPt) {
            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;
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
    public ModelTriangleMesh getSurface() {
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
     * Get the current added surface bit mask.
     *
     * @return  BitSet surface volume bit set mask.
     */
    public BitSet getSurfaceMask() {
        return (BitSet) (surfaceMask.clone());
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
                ((SurfaceRender) renderBase).updateProbe(true, true);
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

        /* Tell the surfaceRenderer to remove the triangle mesh surface
         * (default mesh #0): */
        if (bRemoveMesh) {
            ((SurfaceRender) renderBase).branchSurfaceRemoved();
        }
    }

    /**
     * Remove the surface from the volume render.
     */
    public void removeSurface() {

        /* Tell the surfaceRenderer to remove the triangle mesh surface,
         * must be done before the surfaceList is updated: */
        int[] aiSelected = surfaceList.getSelectedIndices();

        for (int iIndex = 0; iIndex < aiSelected.length; iIndex++) {
            ((SurfaceRender) renderBase).surfaceRemoved(aiSelected[iIndex]);
        }

        removeSurfaces();

        if (((SurfaceRender) renderBase).getProbeDialog() != null) {
            ((SurfaceRender) renderBase).getProbeDialog().updateTargetList();
        }

        if (surfaceVector.size() <= 0) {
            ((SurfaceRender) renderBase).getGeodesicPanel().setEnabled(false);
        }

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
     * ReplaceMesh is used by the Geodesic when a mesh is cut along either an open or closed geodesic curve, the
     * original mesh is changed, but not deleted and no new mesh is added. The orginal mesh, kOld, is replced by the new
     * mesh, kNew
     *
     * @param  kOld  ModelTriangleMesh old mesh reference
     * @param  kNew  ModelTriangleMesh new mesh reference
     */
    public void replaceMesh(ModelTriangleMesh kOld, ModelTriangleMesh kNew) {

        /* Find the original mesh, kOld, and remove it from the surfaceVector
         * and surfaceList, adding the new mesh kNew in it's place: */
        BranchGroup kSurfaceBG;

        for (int i = 0; i < surfaceList.getModel().getSize(); i++) {
            kSurfaceBG = ((SurfaceAttributes) surfaceVector.get(i)).surface;

            for (Enumeration e = kSurfaceBG.getAllChildren(); e.hasMoreElements();) {
                Object kObj = e.nextElement();

                if (kObj instanceof Shape3D) {
                    Shape3D kShape = (Shape3D) kObj;

                    if (kShape.getGeometry() instanceof ModelTriangleMesh) {
                        ModelTriangleMesh kChildMesh = (ModelTriangleMesh) (kShape.getGeometry());

                        if (kChildMesh == kOld) {
                            kShape.removeGeometry(kOld);
                            kShape.addGeometry(kNew);

                            break;
                        }
                    }
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
     * Called from the JPanelSurfaceMAterialProperties.java dialog when the dialog is used to change the material
     * properties of a surface. The surface is determined by the index iIndex. The color button is set to the Material
     * diffuse color.
     *
     * @param  kMaterial  Material reference
     * @param  iIndex     int material index
     */
    public void setMaterial(Material kMaterial, int iIndex) {

        if (!((SurfaceAttributes) surfaceVector.get(iIndex)).isVOIPt) {
            BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;
            Shape3D shape = (Shape3D) root.getChild(0);

            /* Set the new surface material values: */
            Appearance kAppearance = shape.getAppearance();
            Material kMaterialOriginal = kAppearance.getMaterial();

            Color3f kAmbient = new Color3f();
            Color3f kEmissive = new Color3f();
            Color3f kDiffuse = new Color3f();
            Color3f kSpecular = new Color3f();
            kMaterial.getAmbientColor(kAmbient);
            kMaterial.getEmissiveColor(kEmissive);
            kMaterial.getDiffuseColor(kDiffuse);
            kMaterial.getSpecularColor(kSpecular);

            float fShininess = kMaterial.getShininess();

            kMaterialOriginal.setAmbientColor(kAmbient);
            kMaterialOriginal.setEmissiveColor(kEmissive);
            kMaterialOriginal.setDiffuseColor(kDiffuse);
            kMaterialOriginal.setSpecularColor(kSpecular);
            kMaterialOriginal.setShininess(fShininess);

            Color4f kColor = new Color4f(kDiffuse.get());
            colorButton.setBackground(kDiffuse.get());
            ((SurfaceRender) parentScene).setColor(iIndex, kColor);
            ((SurfaceAttributes) surfaceVector.get(iIndex)).color = kColor;

            kAmbient = null;
            kEmissive = null;
            kDiffuse = null;
            kSpecular = null;
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

                if (((SurfaceAttributes) surfaceVector.get(aiSelected[i])).isVOIPt == false) {
                    int numTriangles = 0;
                    float volume = 0;
                    float area = 0;
                    BranchGroup root = ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).surface;

                    ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).detailLevel = iValue;

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
                    ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).triangles = numTriangles;
                    ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).volume = volume;
                    ((SurfaceAttributes) surfaceVector.get(aiSelected[i])).area = area;
                }
            }
        } else if (event.getSource() == opacitySlider) {

            // change the opacity for the selected items
            int[] aiSelected = surfaceList.getSelectedIndices();
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

            for (int i = 0; i < aiSelected.length; i++) {
                int iIndex = aiSelected[i];
                int iValue = opacitySlider.getValue();

                ((SurfaceAttributes) surfaceVector.get(iIndex)).opacity = iValue / 100.0f;

                // change the object opacity
                if (((SurfaceAttributes) surfaceVector.get(iIndex)).isVOIPt) {
                    BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;

                    for (int k = 0; k < root.numChildren(); k++) {
                        Shape3D shape = (Shape3D)
                                            (((Sphere) (((TransformGroup) (root.getChild(k))).getChild(0))).getShape());
                        Appearance appearance = shape.getAppearance();
                        TransparencyAttributes tap = new TransparencyAttributes();

                        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_READ);
                        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);

                        if ((1 - (iValue / 100.0f)) == 0) {
                            tap.setTransparencyMode(TransparencyAttributes.NONE);
                        } else {
                            tap.setTransparencyMode(TransparencyAttributes.BLENDED);
                        }

                        tap.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA);
                        tap.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA);
                        tap.setTransparency(1 - (iValue / 100.0f)); // 0 = Opaque
                        appearance.setTransparencyAttributes(tap);
                    }
                } else {
                    BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;
                    Shape3D shape = (Shape3D) root.getChild(0);
                    Appearance appearance = shape.getAppearance();
                    TransparencyAttributes tap = new TransparencyAttributes();

                    tap.setCapability(TransparencyAttributes.ALLOW_VALUE_READ);
                    tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);

                    if ((1 - (iValue / 100.0f)) == 0) {
                        tap.setTransparencyMode(TransparencyAttributes.NONE);
                    } else {
                        tap.setTransparencyMode(TransparencyAttributes.BLENDED);
                    }

                    tap.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA);
                    tap.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA);
                    tap.setTransparency(1 - (iValue / 100.0f)); // 0 = Opaque
                    appearance.setTransparencyAttributes(tap);
                }
            }

        }
    }

    /**
     * Toggle between wireframe and filled polygon mode.
     */
    public void toggleWireframe() {
        int iMode = polygonModeCB.getSelectedIndex();

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
                BranchGroup root = ((SurfaceAttributes) surfaceVector.get(index)).surface;

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
                    Color4f color = attributes.color;

                    colorButton.setBackground(color.get());
                    setElementsEnabled(true);
                } else {

                    // multiple surfaces were selected (not all colors the same)
                    colorButton.setBackground(getBackground());
                    colorButton.setEnabled(false);
                    colorLabel.setEnabled(false);

                }

                int mode = 0;

                switch (((SurfaceAttributes) surfaceVector.get(index)).polygonMode) {

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
                triangleText.setText("" + attributes.triangles);
                volumeText.setText("" + attributes.volume);
                areaText.setText("" + attributes.area);
                detailSlider.setValue(attributes.detailLevel);
                opacitySlider.setValue((int) (attributes.opacity * 100));

                boolean enable = true;

                for (int i = 0; i < indices.length; i++) {

                    if (!((SurfaceAttributes) surfaceVector.get(indices[i])).isClodMesh) {
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
     * Identify voxels enclosed by the brain surface by using a flood fill. The flood fill is nonrecursive to avoid
     * overflowing the program stack.
     *
     * @param  iX  the x-value of the seed point for the fill
     * @param  iY  the y-value of the seed point for the fill
     * @param  iZ  the z-value of the seed point for the fill
     */
    protected void floodFill(int iX, int iY, int iZ) {

        // Allocate the maximum amount of space needed.   An empty stack has
        // iTop == -1.
        int[] aiXStack = new int[m_iQuantity];
        int[] aiYStack = new int[m_iQuantity];
        int[] aiZStack = new int[m_iQuantity];

        // An empty stack has iTop = -1.  Push seed point onto stack.  All
        // points pushed onto stack have background color zero.
        int iTop = 0;

        aiXStack[iTop] = iX;
        aiYStack[iTop] = iY;
        aiZStack[iTop] = iZ;

        while (iTop >= 0) { // stack is not empty

            // Read top of stack.  Do not pop since we need to return to this
            // top value later to restart the fill in a different direction.
            iX = aiXStack[iTop];
            iY = aiYStack[iTop];
            iZ = aiZStack[iTop];

            // fill the pixel
            surfaceMask.set(getIndex(iX, iY, iZ));

            int iXp1 = iX + 1;

            if ((iXp1 < m_iXBound) && !surfaceMask.get(getIndex(iXp1, iY, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXp1;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iXm1 = iX - 1;

            if ((0 <= iXm1) && !surfaceMask.get(getIndex(iXm1, iY, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXm1;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iYp1 = iY + 1;

            if ((iYp1 < m_iYBound) && !surfaceMask.get(getIndex(iX, iYp1, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYp1;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iYm1 = iY - 1;

            if ((0 <= iYm1) && !surfaceMask.get(getIndex(iX, iYm1, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYm1;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iZp1 = iZ + 1;

            if ((iZp1 < m_iZBound) && !surfaceMask.get(getIndex(iX, iY, iZp1))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZp1;

                continue;
            }

            int iZm1 = iZ - 1;

            if ((0 <= iZm1) && !surfaceMask.get(getIndex(iX, iY, iZm1))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZm1;

                continue;
            }

            // Done in all directions, pop and return to search other
            // directions.
            iTop--;

        }

        aiXStack = null;
        aiYStack = null;
        aiZStack = null;
    }

    /**
     * A convenience function for mapping the 3D voxel position (iX,iY,iZ) to a 1D array index. The images are stored as
     * 1D arrays, so this function is used frequently.
     *
     * @param   iX  the x-value of the voxel position
     * @param   iY  the y-value of the voxel position
     * @param   iZ  the z-value of the voxel position
     *
     * @return  the 1D array index corresponding to (iX,iY,iZ)
     */
    protected final int getIndex(int iX, int iY, int iZ) {
        return iX + (m_iXBound * (iY + (m_iYBound * iZ)));

    }

    /**
     * Compute the point of intersection between a line (0,iY,iZ)+t(1,0,0) and the triangle defined by the three input
     * points. All calculations are in voxel coordinates and the x-value of the intersection point is truncated to an
     * integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iY   the y-value of the origin of the line
     * @param   iZ   the z-value of the origin of the line
     *
     * @return  the x-value of the intersection
     */
    protected float getIntersectX(Point3f kV0, Point3f kV1, Point3f kV2, float iY, float iZ) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iY - kV0.y, fPv = iZ - kV0.z;
        float fE1u = kV1.y - kV0.y, fE1v = kV1.z - kV0.z;
        float fE2u = kV2.y - kV0.y, fE2v = kV2.z - kV0.z;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.x) + (fC1 * kV1.x) + (fC2 * kV2.x)) / fDet;
    }

    /**
     * Compute the point of intersection between a line (iX,0,iZ)+t(0,1,0) and the triangle defined by the three input
     * points. All calculations are in voxel coordinates and the y-value of the intersection point is truncated to an
     * integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iX   the x-value of the origin of the line
     * @param   iZ   the z-value of the origin of the line
     *
     * @return  the y-value of the intersection
     */
    protected float getIntersectY(Point3f kV0, Point3f kV1, Point3f kV2, float iX, float iZ) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iX - kV0.x, fPv = iZ - kV0.z;
        float fE1u = kV1.x - kV0.x, fE1v = kV1.z - kV0.z;
        float fE2u = kV2.x - kV0.x, fE2v = kV2.z - kV0.z;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.y) + (fC1 * kV1.y) + (fC2 * kV2.y)) / fDet;
    }

    /**
     * Compute the point of intersection between a line (iX,iY,0)+t(0,0,1) and the triangle defined by the three input
     * points. All calculations are in voxel coordinates and the z-value of the intersection point is truncated to an
     * integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iX   the x-value of the origin of the line
     * @param   iY   the y-value of the origin of the line
     *
     * @return  the z-value of the intersection
     */
    protected float getIntersectZ(Point3f kV0, Point3f kV1, Point3f kV2, float iX, float iY) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iX - kV0.x, fPv = iY - kV0.y;
        float fE1u = kV1.x - kV0.x, fE1v = kV1.y - kV0.y;
        float fE2u = kV2.x - kV0.x, fE2v = kV2.y - kV0.y;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.z) + (fC1 * kV1.z) + (fC2 * kV2.z)) / fDet;
    }

    /**
     * Mask the tumor surface volume in voxels.
     *
     * @param  kMesh            ModelTriangleMesh Tumor surface
     * @param  bHasVertexColor  boolean
     */
    protected void maskInsideVoxels(ModelTriangleMesh kMesh, boolean bHasVertexColor) {

        float iX, iY, iZ;

        /* The plane coordinate x,y dimensions: */
        float m_fX0;
        float m_fY0;
        float m_fZ0;
        float m_fX1;
        float m_fY1;
        float m_fZ1;

        int[] extents = parentScene.getImageA().getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];

        m_iQuantity = xDim * yDim * zDim;

        m_aiMask = new BitSet(m_iQuantity);
        m_akMaskColor = new Color3f[m_iQuantity];

        // initialize the surface mask
        surfaceMask = new BitSet(m_iQuantity);

        m_iXBound = xDim;
        m_iYBound = yDim;
        m_iZBound = zDim;

        float[] resols = parentScene.getImageA().getFileInfo()[0].getResolutions();

        // local x, y, z box viarables.  Those local viarables make sure that xBox, yBox
        // and zBox not changed in local method.
        float xB, yB, zB, maxB;

        xB = xBox;
        yB = yBox;
        zB = zBox;
        maxB = xBox;

        if (yB > maxB) {
            maxB = yB;
        }

        if (zB > maxB) {
            maxB = zB;
        }

        // Normalize the size
        // xBox range between 0 - 1.
        xB = xB / maxB;
        yB = yBox / maxB;
        zB = zBox / maxB;

        m_fX0 = -xB;
        m_fY0 = -yB;
        m_fX1 = xB;
        m_fY1 = yB;
        m_fZ0 = -zB;
        m_fZ1 = zB;

        maxB = xB;

        if (yB > maxB) {
            maxB = yB;
        }

        if (zB > maxB) {
            maxB = zB;
        }

        if (zB > maxB) {
            m_fZ0 = -1f;
            m_fZ1 = 1f;
        }

        if (kMesh == null) {
            return;
        }

        // Get the non-resampled surface volume voxels.
        m_akVertex = kMesh.getVertexCopy();
        m_aiConnect = kMesh.getIndexCopy();
        m_iTQuantity = (int) (m_aiConnect.length / 3);

        int iVQuantity = kMesh.getVertexCount();
        Color3f[] kTriColors = new Color3f[iVQuantity];

        if (bHasVertexColor && kMesh.getCapability(GeometryArray.ALLOW_COLOR_READ)) {

            for (int iC = 0; iC < iVQuantity; iC++) {
                kTriColors[iC] = new Color3f();
            }

            kMesh.getColors(0, kTriColors);
        }

        Point3f tV0 = new Point3f();
        Point3f tV1 = new Point3f();
        Point3f tV2 = new Point3f();

        Point3f kV0 = new Point3f();
        Point3f kV1 = new Point3f();
        Point3f kV2 = new Point3f();

        Color3f kC0, kC1, kC2;

        for (int iT = 0; iT < m_iTQuantity; iT++) {

            // get the vertices of the triangle
            tV0 = m_akVertex[m_aiConnect[3 * iT]];
            tV1 = m_akVertex[m_aiConnect[(3 * iT) + 1]];
            tV2 = m_akVertex[m_aiConnect[(3 * iT) + 2]];

            kV0.x = tV0.x;
            kV0.y = tV0.y;
            kV0.z = tV0.z;
            kV1.x = tV1.x;
            kV1.y = tV1.y;
            kV1.z = tV1.z;
            kV2.x = tV2.x;
            kV2.y = tV2.y;
            kV2.z = tV2.z;

            Color3f kTriColor = null;

            kC0 = kTriColors[m_aiConnect[3 * iT]];
            kC1 = kTriColors[m_aiConnect[(3 * iT) + 1]];
            kC2 = kTriColors[m_aiConnect[(3 * iT) + 2]];

            if ((kC0 != null) && (kC1 != null) && (kC2 != null)) {
                kTriColor = new Color3f((kC0.x + kC1.x + kC2.x) / 3.0f, (kC0.y + kC1.y + kC2.y) / 3.0f,
                                        (kC0.z + kC1.z + kC2.z) / 3.0f);
            }

            kV0.x = ((kV0.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
            kV0.y = ((kV0.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
            kV0.z = ((kV0.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

            kV1.x = ((kV1.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
            kV1.y = ((kV1.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
            kV1.z = ((kV1.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

            kV2.x = ((kV2.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
            kV2.y = ((kV2.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
            kV2.z = ((kV2.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

            kV0.z = zDim - 1 - kV0.z;
            kV1.z = zDim - 1 - kV1.z;
            kV2.z = zDim - 1 - kV2.z;

            kV0.y = yDim - 1 - kV0.y;
            kV1.y = yDim - 1 - kV1.y;
            kV2.y = yDim - 1 - kV2.y;

            // compute the axis-aligned bounding box of the triangle
            float fXMin = kV0.x, fXMax = fXMin;
            float fYMin = kV0.y, fYMax = fYMin;
            float fZMin = kV0.z, fZMax = fZMin;

            if (kV1.x < fXMin) {
                fXMin = kV1.x;
            } else if (kV1.x > fXMax) {
                fXMax = kV1.x;
            }

            if (kV1.y < fYMin) {
                fYMin = kV1.y;
            } else if (kV1.y > fYMax) {
                fYMax = kV1.y;
            }

            if (kV1.z < fZMin) {
                fZMin = kV1.z;
            } else if (kV1.z > fZMax) {
                fZMax = kV1.z;
            }

            if (kV2.x < fXMin) {
                fXMin = kV2.x;
            } else if (kV2.x > fXMax) {
                fXMax = kV2.x;
            }

            if (kV2.y < fYMin) {
                fYMin = kV2.y;
            } else if (kV2.y > fYMax) {
                fYMax = kV2.y;
            }

            if (kV2.z < fZMin) {
                fZMin = kV2.z;
            } else if (kV2.z > fZMax) {
                fZMax = kV2.z;
            }

            // Rasterize the triangle.  The rasterization is repeated in all
            // three coordinate directions to make sure that floating point
            // round-off errors do not cause any holes in the rasterized
            // surface.
            float iXMin = fXMin, iXMax = fXMax;
            float iYMin = fYMin, iYMax = fYMax;
            float iZMin = fZMin, iZMax = fZMax;
            int ptr;
            int end = m_aiMask.size();

            for (iY = iYMin; iY < iYMax; iY = iY + 0.1f) {

                for (iZ = iZMin; iZ < iZMax; iZ = iZ + 0.1f) {
                    iX = getIntersectX(kV0, kV1, kV2, iY, iZ);

                    if (iX != -1) {
                        ptr = getIndex((int) Math.round(iX), (int) Math.round(iY), (int) Math.round(iZ));

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                            m_akMaskColor[ptr] = kTriColor;
                            surfaceMask.set(ptr);
                        }

                    }
                }
            }

            for (iX = iXMin; iX < iXMax; iX = iX + 0.1f) {

                for (iZ = iZMin; iZ < iZMax; iZ = iZ + 0.1f) {
                    iY = getIntersectY(kV0, kV1, kV2, iX, iZ);

                    if (iY != -1) {
                        ptr = getIndex((int) Math.round(iX), (int) Math.round(iY), (int) Math.round(iZ));

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                            m_akMaskColor[ptr] = kTriColor;
                            surfaceMask.set(ptr);
                        }
                    }
                }
            }

            for (iX = iXMin; iX < iXMax; iX = iX + 0.1f) {

                for (iY = iYMin; iY < iYMax; iY = iY + 0.1f) {
                    iZ = getIntersectZ(kV0, kV1, kV2, iX, iY);

                    if (iZ != -1) {
                        ptr = getIndex((int) Math.round(iX), (int) Math.round(iY), (int) Math.round(iZ));

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                            m_akMaskColor[ptr] = kTriColor;
                            surfaceMask.set(ptr);
                        }
                    }
                }
            }
        }

        // compute centroid of the surface voxels to act as flood fill seed
        float fXC = 0.0f, fYC = 0.0f, fZC = 0.0f;
        int iCount = 0;

        for (iZ = 1; iZ < (m_iZBound - 1); iZ++) {

            for (iY = 1; iY < (m_iYBound - 1); iY++) {

                for (iX = 1; iX < (m_iXBound - 1); iX++) {

                    if (m_aiMask.get(getIndex((int) iX, (int) iY, (int) iZ))) {
                        fXC += (float) iX;
                        fYC += (float) iY;
                        fZC += (float) iZ;
                        iCount++;
                    }
                }
            }
        }

        float fInvCount = 1.0f / iCount;

        fXC *= fInvCount;
        fYC *= fInvCount;
        fZC *= fInvCount;

        floodFill((int) fXC, (int) fYC, (int) fZC);

        int v = 0;

        for (int i = 0; i < surfaceMask.size(); i++) {

            if (surfaceMask.get(i)) {
                v++;
            }
        }

        System.err.println("voxels = " + v);
        System.err.println("volume = " + (v * resols[0] * resols[1] * resols[2]));

        /*
         * byte[] volumeMask = new byte[m_aiMask.size()];
         *
         * for (int i = 0; i < m_aiMask.size(); i++ ) { if ( surfaceMask.get(i) ) { volumeMask[i] = 1; } else {
         * volumeMask[i] = 0; } }
         */

        /*
         * ModelImage newImage = new ModelImage( parentScene.getImageA().getType(),
         * parentScene.getImageA().getExtents(), "Mask Image", parentScene.getImageA().getUserInterface() ); try {
         * newImage.importData( 0, surfaceMask, true ); newImage.setFileInfo( parentScene.getImageA().getFileInfo() ); }
         * catch ( IOException er ) { return; } new ViewJFrameImage( newImage, null, new Dimension( 610, 200 ),
         * parentScene.getUserInterface() );
         */
    }

    /**
     * Reads lines of the file until a nonnull String results or the end of the file is reached.
     *
     * @param      file  the file to read from
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private static String readLine(RandomAccessFile file) throws IOException {
        String tempString = null;
        boolean foundEOF = false;

        while ((tempString == null) && (file.getFilePointer() < (file.length() - 1)) && (!foundEOF)) {

            try {
                tempString = file.readLine();
            } catch (EOFException error) {
                tempString = null;
                foundEOF = true;
            } catch (IOException error) {
                throw (error);
            }

            if (tempString != null) {

                if (tempString.length() == 0) {
                    tempString = null;
                }
            }
        } // while

        return tempString;
    }

    /**
     * Adds a new triangle mesh and material to the scene graph.
     *
     * @param   kMesh      ModelTriangleMesh old surface mesh
     * @param   kMaterial  Material reference
     * @param   file       File file reference.
     * @param   name       String file name
     * @param   opacity    float opacity value
     *
     * @return  int surface index
     */
    private int addModelTriangleMesh(ModelTriangleMesh kMesh, Material kMaterial, File file, String name,
                                     float opacity) {
        Color3f kDiffuse = new Color3f();
        kMaterial.getDiffuseColor(kDiffuse);

        Color4f kDiffuse4 = new Color4f(kDiffuse.x, kDiffuse.y, kDiffuse.z, 1.0f);

        ModelTriangleMesh[] akMeshes = new ModelTriangleMesh[1];
        akMeshes[0] = kMesh;

        BranchGroup root = createSurface(akMeshes, kDiffuse4, PolygonAttributes.POLYGON_FILL);

        root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        root.setCapability(Group.ALLOW_BOUNDS_READ);
        root.setCapability(Group.ALLOW_CHILDREN_READ);
        root.setCapability(Group.ALLOW_CHILDREN_WRITE);
        root.setCapability(BranchGroup.ALLOW_DETACH);
        root.setCapability(Node.ALLOW_PICKABLE_READ);
        root.setCapability(Node.ALLOW_PICKABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_READ);
        root.setCapability(BranchGroup.ALLOW_LOCAL_TO_VWORLD_READ);
        root.setPickable(false);

        // Allow Java to optimize subtree.  Attach to the scene graph.
        root.compile();
        surfaceRootBG.addChild(root);

        /** mask the surface added. */
        surColor = kDiffuse.get();
        m_kSurface = kMesh;
        maskInsideVoxels(getSurface(), false);

        int numTriangles = kMesh.getIndexCount();
        float volume = kMesh.volume();
        float area = kMesh.area();
        SurfaceAttributes surface = new SurfaceAttributes(root, file.getPath(), name, kDiffuse4, 64, 100,
                                                          PolygonAttributes.POLYGON_FILL, numTriangles, volume, area,
                                                          true, opacity, kMesh.center(), getSurfaceMask());

        surfaceVector.add(surface);
        triangleText.setText(String.valueOf(numTriangles));
        volumeText.setText(String.valueOf(volume));
        areaText.setText(String.valueOf(area));

        return surfaceVector.indexOf(surface);
    }

    /**
     * Load a triangle mesh from the specified file and assign to it the specified color. A Shape3D object is created
     * whose geometry is the triangle mesh and whose appearance contains a material. The material's diffuse and specular
     * colors are set to the specified color. A BranchGroup node is created as the parent of the Shape3D object. The
     * parent node is necessary to allow dynamic modification of a compiled scene graph.
     *
     * <p>Create outVoxels and outerVoxels byte arrays with sizes equal to the size of the volume and all the values
     * initialized to zero. The surface is designed to go thru the voxel center so that the target points will be
     * enclosed. Calculate the surface point in standard voxel space that goes from 0 to xDim - 1, 0 to yDim - 1, and 0
     * to zDim - 1. Find the floor and ceilings for these x,y, and z values - this gives a total of 8 voxels. Set these
     * 8 voxels to 1 in the outVoxels array. The verticies for a triangle in this standard voxel space may be too far
     * apart and crate gaps. To prevent gaps from occurring recursively divide a triangle into 4 subtriangles if any 2
     * of the verticies are a distance >= 1 apart. In creating 4 subtriangles the midpoints of the original triangle
     * segments are added to the outVoxels array. Set all edge voxels of outVoxels that do not equal 1 to 2. Recursively
     * set all zero valued outVoxels voxels that have a 6-connected neighbor with a value of 2 to 2. Then, in outVoxels
     * all voxels outside the surface will have a value of 2, all voxels serving as floors or ceilings to the surface
     * points will have a value of 1, and all voxels inside the surface will have a value of 0. For all the 1 values in
     * outVoxels that represent a rounding toward the inside of the surface, set the corresponding values in outerVoxels
     * to 1.</p>
     *
     * @param  file       The triangle mesh file to load.
     * @param  color      The diffuse and specular color for the surface material.
     * @param  name       file name
     * @param  opacity    opacity value
     * @param  idx        surface voxel index
     * @param  isVisible  The progress bar is visible or not.
     */
    private void addSurface(File file, Color4f color, String name, float opacity, int idx, boolean isVisible) {

        // open the file containing one or more meshes
        RandomAccessFile in;
        int iType, iQuantity;
        int numTriangles = 0;
        float volume = 0;
        float area = 0;
        boolean isSur = true;
        int[] direction;
        float[] startLocation;
        Point3f[] akVertex;
        Point3f center = new Point3f();
        int[] aiConnect;
        Point3f[][] akTriangle;
        ModelImage imageA = parentScene.getImageA();
        int[] extents = imageA.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];

        surColor = color.get();

        Point3f s1 = new Point3f();
        Point3f s2 = new Point3f();
        Point3f s3 = new Point3f();
        float sx1, sy1, sz1, sx2, sy2, sz2, sx3, sy3, sz3;

        float[] resols = imageA.getFileInfo()[0].getResolutions();
        float xBox = (xDim - 1) * resols[0];
        float yBox = (yDim - 1) * resols[1];
        float zBox = (zDim - 1) * resols[2];
        float xBoxTrans, yBoxTrans, zBoxTrans, maxBoxTrans;
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));
        int i, j, k;
        int iV1, iV2, iV3;
        float d1, d2, d3;

        if (file.getName().endsWith("sur")) {

            try {
                in = new RandomAccessFile(file, "r");
                iType = in.readInt();
                iQuantity = in.readInt();

                isSur = true;
            } catch (IOException e) {
                return;
            }
        } else if (file.getName().endsWith("xml")) {

            try {
                String filePath = file.getPath();

                String fileName = file.getName();
                ModelImage image = parentScene.getImageA();
                FileVOI fileVOI = new FileVOI(fileName, surfaceDirectoryName, image);

                Sphere[] spheres;
                TransformGroup[] transforms;
                Color3f black = new Color3f(Color.black);

                VOI[] voi = fileVOI.readVOI();

                BranchGroup sphereRoot = new BranchGroup();
                sphereRoot.setCapability(BranchGroup.ALLOW_DETACH);
                sphereRoot.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
                sphereRoot.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
                sphereRoot.setCapability(Group.ALLOW_CHILDREN_EXTEND);
                sphereRoot.setCapability(Group.ALLOW_CHILDREN_READ);
                sphereRoot.setCapability(Group.ALLOW_CHILDREN_WRITE);
                sphereRoot.setCapability(Node.ALLOW_PICKABLE_READ);
                sphereRoot.setCapability(Node.ALLOW_PICKABLE_WRITE);
                sphereRoot.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
                sphereRoot.setCapability(Node.ALLOW_COLLIDABLE_READ);

                maxBoxTrans = xBox;

                if (yBox > maxBoxTrans) {
                    maxBoxTrans = yBox;
                }

                if (zBox > maxBoxTrans) {
                    maxBoxTrans = zBox;
                }

                // Normalize the size
                // xBox range between 0 - 1.
                xBoxTrans = xBox / maxBoxTrans;
                yBoxTrans = yBox / maxBoxTrans;
                zBoxTrans = zBox / maxBoxTrans;

                iType = 0;

                for (i = 0; i < voi.length; i++) {
                    Point3Df[] points;
                    int numPoints = voi[i].getNumPoints();

                    points = new Point3Df[numPoints];

                    for (int h = 0; h < numPoints; h++) {
                        points[h] = new Point3Df();
                    }

                    points = voi[i].exportAllPoints();
                    spheres = new Sphere[numPoints];
                    transforms = new TransformGroup[numPoints];

                    for (int w = 0; w < points.length; w++) {
                        Transform3D t = new Transform3D();
                        float x1 = -xBoxTrans + (2 * ((float) points[w].x / (xDim - 1)) * xBoxTrans);
                        float y1 = yBoxTrans - (2 * ((float) points[w].y / (yDim - 1)) * yBoxTrans);
                        float z1 = zBoxTrans - (2 * ((float) (points[w].z + 0.5f) / (zDim - 1)) * zBoxTrans);

                        t.set(0.02, new Vector3d(x1, y1, z1));
                        transforms[i] = new TransformGroup(t);
                        transforms[i].setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
                        transforms[i].setCapability(BranchGroup.ALLOW_CHILDREN_READ);
                        transforms[i].setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);

                        Appearance app = new Appearance();
                        Material mat = new Material(black, new Color3f(voi[i].getColor()), black, black, 80f);

                        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
                        app.setMaterial(mat);
                        app.setCapability(Appearance.ALLOW_MATERIAL_READ);
                        app.setCapability(Appearance.ALLOW_MATERIAL_WRITE);
                        app.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);
                        app.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ);

                        TransparencyAttributes tap = new TransparencyAttributes();

                        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_READ);
                        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
                        app.setTransparencyAttributes(tap);

                        spheres[i] = new Sphere(.5f, app);
                        spheres[i].setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
                        spheres[i].getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
                        spheres[i].getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
                        spheres[i].getShape().setAppearanceOverrideEnable(true);
                        spheres[i].getShape().setCapability(Geometry.ALLOW_INTERSECT);

                        try {
                            PickCanvas.setCapabilities(spheres[i].getShape(), PickTool.INTERSECT_FULL);
                        } catch (RestrictedAccessException error) { }

                        transforms[i].addChild(spheres[i]);

                        sphereRoot.addChild(transforms[i]);
                    }

                    surfaceRootBG.addChild(sphereRoot);

                    SurfaceAttributes surface = new SurfaceAttributes(sphereRoot, filePath, fileName,
                                                                      new Color4f(voi[i].getColor()), 64, 100,
                                                                      PolygonAttributes.POLYGON_FILL, numTriangles,
                                                                      volume, area, (iType != 0), opacity, true,
                                                                      getSurfaceMask());

                    if (idx == -1) {
                        surfaceVector.add(surface);
                    } else {
                        surfaceVector.set(idx, surface);
                    }

                    image.registerVOI(voi[i]);
                }

                image.notifyImageDisplayListeners();
                isSur = false;
            } catch (IOException e) {
                return;
            }

            return;
        } else {

            try {
                in = new RandomAccessFile(file, "r");
                iType = 0;
                iQuantity = ModelTriangleMesh.parseVRMLMesh(in);
                in.seek(0);
                isSur = false;
            } catch (NoSuchElementException e) {
                MipavUtil.displayError("Only load VRML file specifically written by MIPAV!");

                return;
            } catch (IOException e) {
                return;
            }
        }

        ModelTriangleMesh[] akComponent = new ModelTriangleMesh[iQuantity];
        ViewJProgressBar progress = new ViewJProgressBar("Loading surface", "Loading surface", 0, 100, false, null,
                                                         null);
        progress.setVisible(isVisible);

        try {

            if (iType == 0) {

                // meshes are type TriangleMesh
                for (i = 0; i < iQuantity; i++) {

                    if (isSur == true) {
                        akComponent[i] = ModelTriangleMesh.loadTMesh(in, progress, i * 100 / iQuantity, iQuantity,
                                                                     true);
                    } else {

                        if (i == 0) {
                            akComponent[i] = ModelTriangleMesh.loadVRMLMesh(in, progress, i * 100 / iQuantity,
                                                                            iQuantity, true);
                        } else if (i > 0) {
                            akComponent[i] = ModelTriangleMesh.loadVRMLMesh(in, progress, i * 100 / iQuantity,
                                                                            iQuantity, false);
                        }
                    }

                    if (akComponent[i] == null) {
                        MipavUtil.displayError("Error while reading in triangle mesh.");

                        return;
                    }

                    direction = ModelTriangleMesh.getDirection();
                    startLocation = ModelTriangleMesh.getStartLocation();
                    akVertex = akComponent[i].getVertexCopy();
                    aiConnect = akComponent[i].getIndexCopy();
                    akTriangle = new Point3f[aiConnect.length / 3][3];

                    for (j = 0; j < (aiConnect.length / 3); j++) {

                        for (k = 0; k < 3; k++) {
                            akTriangle[j][k] = new Point3f();
                        }
                    }

                    for (j = 0; j < aiConnect.length;) {
                        iV1 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV1, s1);
                        sx1 = (s1.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy1 = (s1.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz1 = (s1.z - startLocation[2]) / (resols[2] * direction[2]);

                        iV2 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV2, s2);
                        sx2 = (s2.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy2 = (s2.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz2 = (s2.z - startLocation[2]) / (resols[2] * direction[2]);

                        iV3 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV3, s3);
                        sx3 = (s3.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy3 = (s3.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz3 = (s3.z - startLocation[2]) / (resols[2] * direction[2]);

                        d1 = (float) Math.sqrt(((sx1 - sx2) * (sx1 - sx2)) + ((sy1 - sy2) * (sy1 - sy2)) +
                                               ((sz1 - sz2) * (sz1 - sz2)));
                        d2 = (float) Math.sqrt(((sx1 - sx3) * (sx1 - sx3)) + ((sy1 - sy3) * (sy1 - sy3)) +
                                               ((sz1 - sz3) * (sz1 - sz3)));
                        d3 = (float) Math.sqrt(((sx2 - sx3) * (sx2 - sx3)) + ((sy2 - sy3) * (sy2 - sy3)) +
                                               ((sz2 - sz3) * (sz2 - sz3)));

                        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
                            subTriangles(sx1, sy1, sz1, sx2, sy2, sz2, sx3, sy3, sz3);
                        }
                    }

                    for (j = 0; j < akVertex.length; j++) {

                        // The mesh files save the verticies as
                        // pt.x*resX*direction[0] + startLocation
                        // The loaded vertices go from -1 to 1
                        // The loaded vertex is at (2.0f*pt.x*xRes - (xDim-1)*xRes)/((dim-1)*res)max
                        akVertex[j].x = ((2.0f * (akVertex[j].x - startLocation[0]) / direction[0]) -
                                         ((xDim - 1) * resols[0])) / maxBox;
                        akVertex[j].y = ((2.0f * (akVertex[j].y - startLocation[1]) / direction[1]) -
                                         ((yDim - 1) * resols[1])) / maxBox;
                        akVertex[j].z = ((2.0f * (akVertex[j].z - startLocation[2]) / direction[2]) -
                                         ((zDim - 1) * resols[2])) / maxBox;
                    }

                    float xSum = 0f, ySum = 0f, zSum = 0f;

                    for (j = 0; j < akVertex.length; j++) {
                        xSum += akVertex[j].x;
                        ySum += akVertex[j].y;
                        zSum += akVertex[j].z;
                    }

                    center = new Point3f(xSum / akVertex.length, ySum / akVertex.length, zSum / akVertex.length);

                    // Make sure the volume is calculated when in the original file units.
                    volume += akComponent[i].volume();
                    area += akComponent[i].area();
                    akComponent[i].setVerticies(akVertex);
                    numTriangles += akComponent[i].getIndexCount();
                }
            } else {

                // meshes are type ClodMesh
                for (i = 0; i < iQuantity; i++) {
                    ModelClodMesh kClod = ModelClodMesh.loadCMesh(in, progress, i * 100 / iQuantity, iQuantity);

                    direction = ModelClodMesh.getDirection();
                    startLocation = ModelClodMesh.getStartLocation();
                    akVertex = kClod.getMesh().getVertexCopy();
                    aiConnect = kClod.getMesh().getIndexCopy();
                    akTriangle = new Point3f[aiConnect.length / 3][3];

                    for (j = 0; j < (aiConnect.length / 3); j++) {

                        for (k = 0; k < 3; k++) {
                            akTriangle[j][k] = new Point3f();
                        }
                    }

                    akComponent[i] = kClod.getMesh();
                    kClod.setLOD(kClod.getLOD() + 1);

                    for (j = 0; j < aiConnect.length;) {
                        iV1 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV1, s1);
                        sx1 = (s1.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy1 = (s1.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz1 = (s1.z - startLocation[2]) / (resols[2] * direction[2]);

                        iV2 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV2, s2);
                        sx2 = (s2.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy2 = (s2.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz2 = (s2.z - startLocation[2]) / (resols[2] * direction[2]);

                        iV3 = aiConnect[j++];
                        akComponent[i].getCoordinate(iV3, s3);
                        sx3 = (s3.x - startLocation[0]) / (resols[0] * direction[0]);
                        sy3 = (s3.y - startLocation[1]) / (resols[1] * direction[1]);
                        sz3 = (s3.z - startLocation[2]) / (resols[2] * direction[2]);

                        d1 = (float) Math.sqrt(((sx1 - sx2) * (sx1 - sx2)) + ((sy1 - sy2) * (sy1 - sy2)) +
                                               ((sz1 - sz2) * (sz1 - sz2)));
                        d2 = (float) Math.sqrt(((sx1 - sx3) * (sx1 - sx3)) + ((sy1 - sy3) * (sy1 - sy3)) +
                                               ((sz1 - sz3) * (sz1 - sz3)));
                        d3 = (float) Math.sqrt(((sx2 - sx3) * (sx2 - sx3)) + ((sy2 - sy3) * (sy2 - sy3)) +
                                               ((sz2 - sz3) * (sz2 - sz3)));

                        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
                            subTriangles(sx1, sy1, sz1, sx2, sy2, sz2, sx3, sy3, sz3);
                        }
                    }

                    for (j = 0; j < akVertex.length; j++) {

                        // The mesh files save the verticies as
                        // pt.x*resX*direction[0] + startLocation
                        // The loaded vertices go from -1 to 1
                        // The loaded vertex is at (2.0f*pt.x*resX - (xDim-1)*resX)/((dim-1)*res)max
                        akVertex[j].x = ((2.0f * (akVertex[j].x - startLocation[0]) / direction[0]) -
                                         ((xDim - 1) * resols[0])) / maxBox;
                        akVertex[j].y = ((2.0f * (akVertex[j].y - startLocation[1]) / direction[1]) -
                                         ((yDim - 1) * resols[1])) / maxBox;
                        akVertex[j].z = ((2.0f * (akVertex[j].z - startLocation[2]) / direction[2]) -
                                         ((zDim - 1) * resols[2])) / maxBox;
                    }

                    float xSum = 0f, ySum = 0f, zSum = 0f;

                    for (j = 0; j < akVertex.length; j++) {
                        xSum += akVertex[j].x;
                        ySum += akVertex[j].y;
                        zSum += akVertex[j].z;
                    }

                    center = new Point3f(xSum / akVertex.length, ySum / akVertex.length, zSum / akVertex.length);

                    kClod.setLOD(kClod.getMaximumLOD());
                    akComponent[i] = kClod.getMesh();

                    // Make sure the volume is calculated when in the original file units.
                    volume += akComponent[i].volume();
                    area += akComponent[i].area();
                    akComponent[i].setVerticies(akVertex);
                    kClod.setVerticies(akVertex);
                    numTriangles += akComponent[i].getIndexCount();
                }
            }
        } catch (IOException e) {
            return;
        }

        progress.dispose();

        BranchGroup root = createSurface(akComponent, color, PolygonAttributes.POLYGON_FILL);

        root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        root.setCapability(Group.ALLOW_CHILDREN_READ);
        root.setCapability(Group.ALLOW_CHILDREN_WRITE);
        root.setCapability(BranchGroup.ALLOW_DETACH);
        root.setCapability(Node.ALLOW_PICKABLE_READ);
        root.setCapability(Node.ALLOW_PICKABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_READ);
        root.setPickable(false);

        // Allow Java to optimize subtree.  Attach to the scene graph.
        root.compile();
        surfaceRootBG.addChild(root);
        numTriangles = numTriangles / 3;

        maskInsideVoxels(getSurface(), false);

        SurfaceAttributes surface = new SurfaceAttributes(root, file.getPath(), name, color, 64, 100,
                                                          PolygonAttributes.POLYGON_FILL, numTriangles, volume, area,
                                                          (iType != 0), opacity, center, getSurfaceMask());

        if (idx == -1) {
            surfaceVector.add(surface);
        } else {
            surfaceVector.set(idx, surface);
        }

        triangleText.setText(String.valueOf(numTriangles));
        volumeText.setText(String.valueOf(volume));
        areaText.setText(String.valueOf(area));
    }

    /**
     * The action taken when the Add button is clicked in the list box. A file dialog is launched that allows the user
     * to select new surfaces to load from disk.
     *
     * @throws  IOException  if there is a problem loading the chosen surface file
     */
    private void addSurfaces() throws IOException {
        File[] files = null;

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();

        chooser.setMultiSelectionEnabled(true);
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal = chooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            files = chooser.getSelectedFiles();
        } else {
            return;
        }

        ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
                                                             File.separatorChar);
        surfaceDirectoryName = ViewUserInterface.getReference().getDefaultDirectory();

        // only add surfaces that are not already in the list
        boolean bNameAdded = false;

        /* For parsing and storing surface XML file information: */
        FileSurfaceXML kSurfaceXML = null;
        FileInfoSurfaceXML kFileInfo = null;

        for (int i = 0; i < files.length; i++) {
            String kName = files[i].getName();

            bNameAdded = true;

            int index = surfaceVector.size();

            Color4f color = new Color4f();

            color.w = 1.0f;

            if (index < fixedColor.length) {

                // Use the fixed colors for the first six surfaces.
                color.x = fixedColor[index].x;
                color.y = fixedColor[index].y;
                color.z = fixedColor[index].z;
            } else {

                // Use randomly generated colors for the seventh and
                // later surfaces.
                color.x = 0.5f * (1.0f + randomGen.nextFloat());
                color.y = 0.5f * (1.0f + randomGen.nextFloat());
                color.z = 0.5f * (1.0f + randomGen.nextFloat());
            }

            // add the surface to the scene graph
            System.err.println(kName);

            if ((kName.indexOf(".sur") != -1) || (kName.indexOf(".wrl") != -1)) {
                addSurface(files[i], color, kName, 0.5f, -1, true);

                /* Tell the surfaceRenderer to add the triangle mesh surface: */
                ((SurfaceRender) renderBase).surfaceAdded();
            }
            /* Read the xml file and add to the scene graph: */
            else if (kName.indexOf(".xml") != -1) {
                kSurfaceXML = new FileSurfaceXML(ViewUserInterface.getReference(), kName, files[i].getParent(), false);
                kFileInfo = kSurfaceXML.readSurfaceXML(kName, files[i].getParent());

                if (kFileInfo != null) {
                    int iSurface = addModelTriangleMesh(kFileInfo.getMesh(), kFileInfo.getMaterial(), files[i], kName,
                                                        1.0f);

                    /* Tell the surfaceRenderer to add the triangle mesh
                     * surface: */
                    ((SurfaceRender) renderBase).surfaceAdded();
                    ((SurfaceRender) renderBase).getGeodesicPanel().setEnabled(true);
                    setMaterial(kFileInfo.getMaterial(), iSurface);
                } else {
                    throw new IOException();
                }
            }

            // add the surface to the image's file info if it is XML (so it can be saved)
            if (parentScene.getParentFrame().getImageOriginal().getFileInfo()[0] instanceof FileInfoImageXML) {

                for (int s = 0; s < parentScene.getParentFrame().getImageOriginal().getExtents()[2]; s++) {
                    ((FileInfoImageXML) parentScene.getParentFrame().getImageOriginal().getFileInfo()[s]).addSurface(surfaceDirectoryName +
                                                                                                                     kName);
                }
            }
        }

        if (surfaceList.getSelectedIndices().length == 1) {
            setElementsEnabled(true);
        }

        // if new surfaces were loaded, update the list data
        if (bNameAdded) {
            Vector surfaceNames = new Vector();

            for (Enumeration en = surfaceVector.elements(); en.hasMoreElements();) {
                surfaceNames.addElement(((SurfaceAttributes) en.nextElement()).name);
            }

            surfaceList.setListData(surfaceNames);

            int[] aiSelected = surfaceList.getSelectedIndices();

            if (aiSelected.length == 0) {
                iSelect = 0;
                surfaceList.setSelectedIndex(0);
            }
        }

    }

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
        loadASCButton = toolbarBuilder.buildButton("ImportA", "Import FreeSurfer (.asc)", "openasc");

        toolBar.add(smooth1Button);
        toolBar.add(smooth2Button);
        toolBar.add(smooth3Button);
        toolBar.add(decimateButton);
        toolBar.add(levelSButton);
        toolBar.add(levelVButton);
        toolBar.add(levelWButton);
        toolBar.add(levelXMLButton);
        toolBar.add(loadASCButton);

        mainPanel.add(toolBar, BorderLayout.NORTH);
    }

    /**
     * Changes the polygon mode of the selected surface by detaching it, calling the appropriate method, and reattaching
     * it.
     *
     * @param  mode  The new polygon mode to set.
     */
    private void changePolyMode(int mode) {
        int[] indices;
        BranchGroup surface;

        indices = surfaceList.getSelectedIndices();

        for (int i = 0; i < indices.length; i++) {
            surface = ((SurfaceAttributes) surfaceVector.get(indices[i])).surface;
            ((SurfaceAttributes) surfaceVector.get(indices[i])).polygonMode = mode;
            surface.detach();

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

            surfaceRootBG.addChild(surface);
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
    private BranchGroup createSurface(ModelTriangleMesh[] meshes, Color4f color, int mode) {

        // create a material with the desired color
        Material material = new Material();

        material.setCapability(Material.ALLOW_COMPONENT_READ);
        material.setCapability(Material.ALLOW_COMPONENT_WRITE);
        material.setDiffuseColor(color.x, color.y, color.z, color.w);
        material.setSpecularColor(color.x, color.y, color.z);
        material.setAmbientColor(color.x, color.y, color.z);
        // material.setShininess(0);

        // set the mesh's render state
        Appearance appearance = new Appearance();

        appearance.setCapability(Appearance.ALLOW_MATERIAL_READ);
        appearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
        appearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);
        appearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);
        appearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ);

        TransparencyAttributes tap = new TransparencyAttributes();

        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_READ);
        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);

        appearance.setTransparencyAttributes(tap);
        appearance.setMaterial(material);

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

        m_kSurface = meshes[0];

        return root;
    }

    /**
     * Decimate the surface.
     */
    private void decimate() {
        int selected = surfaceList.getSelectedIndex();

        if (selected == -1) {
            MipavUtil.displayError("Select a surface to smooth.");

            return;
        }

        BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;
        ModelTriangleMesh[] meshes = new ModelTriangleMesh[root.numChildren()];

        for (int j = 0; j < root.numChildren(); j++) {
            Shape3D shape = (Shape3D) root.getChild(j);

            meshes[j] = (ModelTriangleMesh) shape.getGeometry(0);
        }

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

        ViewJProgressBar progressBar = new ViewJProgressBar("Decimating surface", "Decimating surface", 0, 100, false,
                                                            null, null);

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
        root.detach();
        root = createSurface(meshes, ((SurfaceAttributes) surfaceVector.get(selected)).color,
                             ((SurfaceAttributes) surfaceVector.get(selected)).polygonMode);
        root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        root.setCapability(Group.ALLOW_CHILDREN_READ);
        root.setCapability(Group.ALLOW_CHILDREN_WRITE);
        root.setCapability(BranchGroup.ALLOW_DETACH);
        root.setCapability(Node.ALLOW_PICKABLE_READ);
        root.setCapability(Node.ALLOW_PICKABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        root.setCapability(Node.ALLOW_COLLIDABLE_READ);
        root.setPickable(false);
        surfacePickableCB.setSelected(false);

        if (((SurfaceRender) parentScene).getDisplayMode3D()) {
            surfaceClipCB.setSelected(false);
        }

        root.compile();
        surfaceRootBG.addChild(root);
        ((SurfaceAttributes) surfaceVector.get(selected)).surface = root;
        ((SurfaceAttributes) surfaceVector.get(selected)).isClodMesh = true;
        decimateButton.setEnabled(false);
        detailSlider.setEnabled(true);
        detailLabel.setEnabled(true);

        for (int i = 0; i < detailSliderLabels.length; i++) {
            detailSliderLabels[i].setEnabled(true);
        }

        progressBar.dispose();

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

            ((SurfaceRender) parentScene).setColor(iIndex, new Color4f(color));

            /** when surface color changed, record it here. */
            surColor = color;

            Color4f newColor = new Color4f(color);

            ((SurfaceAttributes) surfaceVector.get(iIndex)).color = newColor;

            if (((SurfaceAttributes) surfaceVector.get(iIndex)).isVOIPt) {
                BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;

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
            } else {

                // change the material color
                BranchGroup root = ((SurfaceAttributes) surfaceVector.get(iIndex)).surface;
                Shape3D shape = (Shape3D) root.getChild(0);
                Appearance appearance = shape.getAppearance();
                Material material = appearance.getMaterial();

                material.setDiffuseColor(newColor.x, newColor.y, newColor.z);
                material.setSpecularColor(newColor.x, newColor.y, newColor.z);
                material.setAmbientColor(newColor.x, newColor.y, newColor.z);
            }
        }
    }

    /**
     * Calls a dialog to get a file name.
     *
     * @param   fName  if<code>true</code>, make it a load dialog.
     *
     * @return  File name.
     */
    private String getCurrentFileName(String fName) {
        String name;

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();

        chooser.setVisible(false);
        chooser.setMultiSelectionEnabled(false);

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        name = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar + fName; // chooser.getSelectedFile().getName();
        ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
                                                             File.separatorChar);

        return name;
    }

    /**
     * Calls a dialog to get a file name.
     *
     * @param   load  if <code>true</code>, make it a load dialog.
     *
     * @return  File name.
     */
    private String getFileName(boolean load) {
        String name;

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();

        chooser.setMultiSelectionEnabled(false);
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal;

        if (load) {
            returnVal = chooser.showOpenDialog(this);
        } else {
            returnVal = chooser.showSaveDialog(this);
        }

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            name = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar +
                   chooser.getSelectedFile().getName();
        } else {
            return null;
        }

        ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
                                                             File.separatorChar);

        return name;
    }

    /**
     * Calls a file chooser then loads the triangle mesh from that file.
     */
    private void importA() {
        String fileName;
        String directory;
        ViewJProgressBar progress = null;
        String lineString = null;
        String[] parseString = null;
        int vertexNumber, numTriangles;
        Point3f[] vertex = null;
        int[] triangle = null;
        int i;
        float volume = 0.0f;
        float area = 0.0f;
        int iType = 0; // mesh is triangle mesh
        int updateNumber;
        int updatePoint;
        int currentValue;
        RandomAccessFile raFile = null;

        // file dialog to select FreeSurfer ascii files (*.asc)
        JFileChooser chooser = new JFileChooser();

        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FREESURFER));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal = chooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = chooser.getCurrentDirectory() + "" + File.separatorChar;
        } else {
            return;
        }

        ViewUserInterface.getReference().setDefaultDirectory(directory);

        try {
            raFile = new RandomAccessFile(directory + fileName, "r");
            lineString = readLine(raFile);

            while ((lineString == null) || (lineString.charAt(0) == '#')) {
                lineString = readLine(raFile);
            }

            parseString = parse(lineString);

            if (parseString.length != 2) {

                if (progress != null) {
                    progress.dispose();
                }

                raFile.close();
                MipavUtil.displayError("First noncommented line was " + lineString);
            }

            vertexNumber = Integer.valueOf(parseString[0]).intValue();
            numTriangles = Integer.valueOf(parseString[1]).intValue();
            System.out.println("vertex number = " + vertexNumber + " triangleNumber = " + numTriangles);
            progress = new ViewJProgressBar("Loading triangle mesh", "Loading vertices", 0, 100, false, null, null);
            progress.setLocation(200, 200);
            progress.setVisible(true);
            vertex = new Point3f[vertexNumber];
            updatePoint = vertexNumber / 25;
            updateNumber = 0;
            currentValue = 0;

            for (i = 0; i < vertexNumber; i++) {
                lineString = readLine(raFile);
                parseString = parse(lineString);

                if (parseString.length < 3) {

                    if (progress != null) {
                        progress.dispose();
                    }

                    raFile.close();
                    MipavUtil.displayError("vertex[" + i + "] has " + lineString);
                }

                // Need to align with standard X R->L, Y S->I, Z P->A coronal of
                // standard COR volume image file
                vertex[i] = new Point3f();
                vertex[i].x = Float.valueOf(parseString[0]).floatValue() / 128.0f;
                vertex[i].z = -Float.valueOf(parseString[1]).floatValue() / 128.0f;
                vertex[i].y = Float.valueOf(parseString[2]).floatValue() / 128.0f;
                updateNumber++;

                if (updateNumber == updatePoint) {
                    updateNumber = 0;
                    currentValue++;
                    progress.updateValueImmed(currentValue);
                }
            } // for (i = 0; i < vertexNumber; i++)

            progress.setMessage("Loading triangles");
            currentValue = 25;
            progress.updateValueImmed(currentValue);
            triangle = new int[3 * numTriangles];
            updatePoint = numTriangles / 25;
            updateNumber = 0;

            for (i = 0; i < numTriangles; i++) {
                lineString = readLine(raFile);
                parseString = parse(lineString);

                if (parseString.length < 3) {

                    if (progress != null) {
                        progress.dispose();
                    }

                    raFile.close();
                    MipavUtil.displayError("triangle[" + i + "] has " + lineString);
                }

                triangle[3 * i] = Integer.valueOf(parseString[0]).intValue();
                triangle[(3 * i) + 1] = Integer.valueOf(parseString[1]).intValue();
                triangle[(3 * i) + 2] = Integer.valueOf(parseString[2]).intValue();
                updateNumber++;

                if (updateNumber == updatePoint) {
                    updateNumber = 0;
                    currentValue++;
                    progress.updateValueImmed(currentValue);
                }
            } // for (i = 0; i < numTriangles; i++)

            raFile.close();
            progress.setMessage("Creating ModelTriangleMesh");
            progress.updateValueImmed(50);

            ModelTriangleMesh[] akComponent = new ModelTriangleMesh[1];

            akComponent[0] = new ModelTriangleMesh(vertex, triangle);
            volume = akComponent[0].volume();
            area = akComponent[0].area();

            int index = surfaceVector.size();

            Color4f color = new Color4f();

            color.w = 1.0f;

            if (index < fixedColor.length) {

                // Use the fixed colors for the first six surfaces.
                color.x = fixedColor[index].x;
                color.y = fixedColor[index].y;
                color.z = fixedColor[index].z;
            } else {

                // Use randomly generated colors for the seventh and
                // later surfaces.
                color.x = 0.5f * (1.0f + randomGen.nextFloat());
                color.y = 0.5f * (1.0f + randomGen.nextFloat());
                color.z = 0.5f * (1.0f + randomGen.nextFloat());
            }

            progress.setMessage("Creating surface");
            progress.updateValueImmed(75);

            BranchGroup root = createSurface(akComponent, color, PolygonAttributes.POLYGON_FILL);

            root.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            root.setCapability(Group.ALLOW_CHILDREN_READ);
            root.setCapability(Group.ALLOW_CHILDREN_WRITE);
            root.setCapability(BranchGroup.ALLOW_DETACH);
            root.setCapability(Node.ALLOW_PICKABLE_READ);
            root.setCapability(Node.ALLOW_PICKABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
            root.setCapability(Node.ALLOW_COLLIDABLE_READ);
            root.setPickable(false);

            // Allow Java to optimize subtree.  Attach to the scene graph.
            root.compile();
            surfaceRootBG.addChild(root);
            numTriangles = numTriangles / 3;

            SurfaceAttributes surface = new SurfaceAttributes(root, directory, fileName, color, 64, 100,
                                                              PolygonAttributes.POLYGON_FILL, numTriangles, volume,
                                                              area, (iType != 0), getSurfaceMask());

            surfaceVector.add(surface);
            triangleText.setText(String.valueOf(numTriangles));
            volumeText.setText(String.valueOf(volume));
            areaText.setText(String.valueOf(area * maxBox * maxBox));

            if (surfaceList.getSelectedIndices().length == 1) {
                setElementsEnabled(true);
            }

            // update the list data
            Vector surfaceNames = new Vector();

            for (Enumeration en = surfaceVector.elements(); en.hasMoreElements();) {
                surfaceNames.addElement(((SurfaceAttributes) en.nextElement()).name);
            }

            surfaceList.setListData(surfaceNames);

            int[] aiSelected = surfaceList.getSelectedIndices();

            if (aiSelected.length == 0) {
                iSelect = 0;
                surfaceList.setSelectedIndex(0);
            }

            progress.dispose();
        } catch (IOException e) {

            if (progress != null) {
                progress.dispose();
            }

            MipavUtil.displayError("Load of " + directory + fileName + " failed.");
        } finally {

            try {

                if (raFile != null) {
                    raFile.close();
                }
            } catch (IOException ioe) {
                // do nothing
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
        randomGen = new Random(367);
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

        colorPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        sliderPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        cbPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        colorPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        sliderPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        cbPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        JPanel optionsPanel = new JPanel();

        optionsPanel.setLayout(new BorderLayout());
        optionsPanel.add(colorPanel, BorderLayout.NORTH);
        optionsPanel.add(sliderPanel, BorderLayout.CENTER);
        optionsPanel.add(cbPanel, BorderLayout.SOUTH);
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
     * Parse a line of surface data.
     *
     * @param   inString  surface data
     *
     * @return  String[] string tokens
     */
    private String[] parse(String inString) {
        String[] tmpString = new String[4];
        String[] outString;
        int i;
        int sNum = 0;
        int firstEl = 0;

        for (i = 0; i < inString.length(); i++) {

            if (inString.charAt(i) <= 0x20) {

                if (firstEl != i) {
                    tmpString[sNum++] = inString.substring(firstEl, i);
                }

                firstEl = i + 1;
            }
        }

        if (firstEl != i) {
            tmpString[sNum++] = inString.substring(firstEl, i);
        }

        if (sNum == 0) {
            outString = new String[1];
            outString[0] = inString;
        } else {
            outString = new String[sNum];

            for (i = 0; i < (sNum); i++) {
                outString[i] = tmpString[i];
            }
        }

        return outString;
    }

    /**
     * The action taken when the Remove button is clicked in the list box. All selected surfaces in the list box are
     * removed from the scene graph.
     */
    private void removeSurfaces() {

        // construct the lists of items to be removed
        int[] aiSelected = surfaceList.getSelectedIndices();
        Vector removeSurfaces = new Vector();

        int i;

        for (i = 0; i < aiSelected.length; i++) {
            int iIndex = aiSelected[i];

            removeSurfaces.add(surfaceVector.get(iIndex));
        }

        // remove the items
        for (i = 0; i < aiSelected.length; i++) {
            removeSurface(((SurfaceAttributes) removeSurfaces.get(i)).surface);
            surfaceVector.remove(removeSurfaces.get(i));

            // remove the surface from the image's file info if it is XML (so that it won't be saved with it)
            if (parentScene.getParentFrame().getImageOriginal().getFileInfo()[0] instanceof FileInfoImageXML) {

                for (int s = 0; s < parentScene.getParentFrame().getImageOriginal().getExtents()[2]; s++) {
                    ((FileInfoImageXML) parentScene.getParentFrame().getImageOriginal().getFileInfo()[s]).removeSurface(((SurfaceAttributes)
                                                                                                                             removeSurfaces.get(i)).fullPath);
                }
            }
        }

        Vector surfaceNames = new Vector();

        for (Enumeration en = surfaceVector.elements(); en.hasMoreElements();) {
            surfaceNames.addElement(((SurfaceAttributes) en.nextElement()).name);
        }

        surfaceList.setListData(surfaceNames);

        // determine the new selected list item
        if (surfaceNames.size() > 0) {

            // highlight another list item when current one is removed
            if (iSelect >= surfaceNames.size()) {
                surfaceList.setSelectedIndex(surfaceNames.size() - 1);
            } else {
                surfaceList.setSelectedIndex(iSelect);
            }
        } else {

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
     * Saves a single level of detail to a mesh file.
     *
     * @param   meshes  ModelTriangleMesh[] The triangle meshes that make up that level of detail surface.
     * @param   kOut    PrintWriter File output reference
     * @param   color   Color3f surface color
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void savePortableMesh(ModelTriangleMesh[] meshes, PrintWriter kOut, Color3f color) throws IOException {
        int i;
        ModelTriangleMesh[] meshesCopy = new ModelTriangleMesh[meshes.length];

        ModelTriangleMesh meshCopy;

        if (kOut != null) {
            float[] startLocation = parentScene.getImageA().getFileInfo(0).getOrigin();
            float[] resolution = parentScene.getImageA().getFileInfo(0).getResolutions();
            int[] extents = parentScene.getImageA().getExtents();
            float[] box = new float[3];

            box[0] = extents[0] * resolution[0];
            box[1] = extents[1] * resolution[1];
            box[2] = extents[2] * resolution[2];

            float maxBox = Math.max(box[0], Math.max(box[1], box[2]));
            int[] axisOrientation = parentScene.getImageA().getFileInfo(0).getAxisOrientation();
            int[] direction = new int[] { 1, 1, 1 };
            int j;

            for (i = 0; i <= 2; i++) {

                if ((axisOrientation[i] == ORI_L2R_TYPE) || (axisOrientation[i] == ORI_P2A_TYPE) ||
                        (axisOrientation[i] == ORI_S2I_TYPE)) {
                    direction[i] = -1;
                }
            }

            Point3f[] akVertex;

            for (i = 0; i < meshes.length; i++) {
                meshCopy = new ModelTriangleMesh(meshes[i].getVertexCopy(), meshes[i].getNormalCopy(),
                                                 meshes[i].getIndexCopy());
                akVertex = meshCopy.getVertexCopy();

                // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                // The mesh files must save the verticies as
                // pt.x*resX*direction[0] + startLocation
                for (j = 0; j < akVertex.length; j++) {
                    akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) + startLocation[0];
                    akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) + startLocation[1];
                    akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) + startLocation[2];

                    // flip y and z
                    akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                    akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;
                }

                meshCopy.setVerticies(akVertex);
                meshesCopy[i] = meshCopy;
            }

            ModelTriangleMesh.saveAsPortableVRML(kOut, meshesCopy, true, direction, startLocation, box, color);
        }
    }

    /**
     * Saves a single level of detail to a mesh file.
     *
     * @param  meshes  The triangle meshes that make up that level of detail surface.
     * @param  isSur   true if .sur file, otherwise .wrl file
     * @param  color   surface color
     */
    private void saveSingleMesh(ModelTriangleMesh[] meshes, boolean isSur, Color3f color) {
        int i;
        String extension;
        TransMatrix dicomMatrix;
        TransMatrix inverseDicomMatrix = null;
        double[][] inverseDicomArray = null;
        float[] coord;
        float[] tCoord;

        String name = getFileName(false);

        if (name == null) {
            return;
        }

        ModelTriangleMesh[] meshesCopy = new ModelTriangleMesh[meshes.length];

        ModelTriangleMesh meshCopy;

        i = name.lastIndexOf('.');

        if ((i > 0) && (i < (name.length() - 1))) {
            extension = name.substring(i + 1).toLowerCase();

            if (isSur && !extension.equals("sur")) {
                MipavUtil.displayError("Extension must be .sur");

                return;
            } else if (!isSur && !extension.equals("wrl")) {
                MipavUtil.displayError("Extension must be .wrl");

                return;
            }
        } else if (isSur) {
            name = name + ".sur";
        } else {
            name = name + ".wrl";
        }

        if (name != null) {

            try {
                float[] startLocation = parentScene.getImageA().getFileInfo(0).getOrigin();
                float[] resolution = parentScene.getImageA().getFileInfo(0).getResolutions();
                int[] extents = parentScene.getImageA().getExtents();
                float[] box = new float[3];

                box[0] = extents[0] * resolution[0];
                box[1] = extents[1] * resolution[1];
                box[2] = extents[2] * resolution[2];

                float maxBox = Math.max(box[0], Math.max(box[1], box[2]));
                int[] axisOrientation = parentScene.getImageA().getFileInfo(0).getAxisOrientation();
                int[] direction = new int[] { 1, 1, 1 };
                int j;

                for (i = 0; i <= 2; i++) {

                    if ((axisOrientation[i] == ORI_L2R_TYPE) || (axisOrientation[i] == ORI_P2A_TYPE) ||
                            (axisOrientation[i] == ORI_S2I_TYPE)) {
                        direction[i] = -1;
                    }
                }

                Point3f[] akVertex;

                for (i = 0; i < meshes.length; i++) {
                    meshCopy = new ModelTriangleMesh(meshes[i].getVertexCopy(), meshes[i].getNormalCopy(),
                                                     meshes[i].getIndexCopy());
                    akVertex = meshCopy.getVertexCopy();

                    // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                    // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                    // The mesh files must save the verticies as
                    // pt.x*resX*direction[0] + startLocation
                    if (isSur &&
                            (parentScene.getImageA().getFileInfo()[0].getTransformID() ==
                                 FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL)) {

                        // Get the DICOM transform that describes the transformation from
                        // axial to this image orientation
                        dicomMatrix = (TransMatrix) (parentScene.getImageA().getMatrix().clone());
                        inverseDicomMatrix = (TransMatrix) (parentScene.getImageA().getMatrix().clone());
                        inverseDicomMatrix.invert();
                        inverseDicomArray = inverseDicomMatrix.getMatrix();
                        inverseDicomMatrix = null;
                        coord = new float[3];
                        tCoord = new float[3];

                        for (j = 0; j < akVertex.length; j++) {
                            akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) +
                                            startLocation[0];
                            akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) +
                                            startLocation[1];
                            akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) +
                                            startLocation[2];

                            // flip y and z
                            akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                            akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;

                            // Change the voxel coordinate into millimeter space
                            coord[0] = (akVertex[j].x - startLocation[0]) / direction[0];
                            coord[1] = (akVertex[j].y - startLocation[1]) / direction[1];
                            coord[2] = (akVertex[j].z - startLocation[2]) / direction[2];

                            // Convert the point to axial millimeter DICOM space
                            dicomMatrix.transform(coord, tCoord);

                            // Add in the DICOM origin
                            tCoord[0] = tCoord[0] + startLocation[0];
                            tCoord[1] = tCoord[1] + startLocation[1];
                            tCoord[2] = tCoord[2] + startLocation[2];
                            akVertex[j] = new Point3f(tCoord[0], tCoord[1], tCoord[2]);
                        }
                    } else {

                        for (j = 0; j < akVertex.length; j++) {
                            akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) +
                                            startLocation[0];
                            akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) +
                                            startLocation[1];
                            akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) +
                                            startLocation[2];

                            // flip y and z
                            akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                            akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;
                        }
                    }

                    meshCopy.setVerticies(akVertex);
                    meshesCopy[i] = meshCopy;
                }

                if (isSur == true) {
                    ModelTriangleMesh.save(name, meshesCopy, true, direction, startLocation, box, inverseDicomArray);
                } else {
                    ModelTriangleMesh.saveAsVRML(name, meshesCopy, true, direction, startLocation, box, color);
                }
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
    }

    /**
     * Saves a single level of detail to a mesh file.
     *
     * @param  fName   DOCUMENT ME!
     * @param  meshes  The triangle meshes that make up that level of detail surface.
     * @param  isSur   true if .sur file, otherwise .wrl file
     * @param  color   surface color, not used for now.
     */
    private void saveSingleMesh(String fName, ModelTriangleMesh[] meshes, boolean isSur, Color3f color) {
        int i;
        TransMatrix dicomMatrix;
        TransMatrix inverseDicomMatrix = null;
        double[][] inverseDicomArray = null;
        float[] coord;
        float[] tCoord;

        String name = getCurrentFileName(fName);

        if (name == null) {
            return;
        }

        ModelTriangleMesh[] meshesCopy = new ModelTriangleMesh[meshes.length];

        ModelTriangleMesh meshCopy;

        if (name != null) {

            try {
                float[] startLocation = parentScene.getImageA().getFileInfo(0).getOrigin();
                float[] resolution = parentScene.getImageA().getFileInfo(0).getResolutions();
                int[] extents = parentScene.getImageA().getExtents();
                float[] box = new float[3];

                box[0] = extents[0] * resolution[0];
                box[1] = extents[1] * resolution[1];
                box[2] = extents[2] * resolution[2];

                float maxBox = Math.max(box[0], Math.max(box[1], box[2]));
                int[] axisOrientation = parentScene.getImageA().getFileInfo(0).getAxisOrientation();
                int[] direction = new int[] { 1, 1, 1 };
                int j;

                for (i = 0; i <= 2; i++) {

                    if ((axisOrientation[i] == ORI_L2R_TYPE) || (axisOrientation[i] == ORI_P2A_TYPE) ||
                            (axisOrientation[i] == ORI_S2I_TYPE)) {
                        direction[i] = -1;
                    }
                }

                Point3f[] akVertex;

                for (i = 0; i < meshes.length; i++) {
                    meshCopy = new ModelTriangleMesh(meshes[i].getVertexCopy(), meshes[i].getNormalCopy(),
                                                     meshes[i].getIndexCopy());
                    akVertex = meshCopy.getVertexCopy();

                    // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                    // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                    // The mesh files must save the verticies as
                    // pt.x*resX*direction[0] + startLocation
                    if (isSur &&
                            (parentScene.getImageA().getFileInfo()[0].getTransformID() ==
                                 FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL)) {

                        // Get the DICOM transform that describes the transformation from
                        // axial to this image orientation
                        dicomMatrix = (TransMatrix) (parentScene.getImageA().getMatrix().clone());
                        inverseDicomMatrix = (TransMatrix) (parentScene.getImageA().getMatrix().clone());
                        inverseDicomMatrix.invert();
                        inverseDicomArray = inverseDicomMatrix.getMatrix();
                        inverseDicomMatrix = null;
                        coord = new float[3];
                        tCoord = new float[3];

                        for (j = 0; j < akVertex.length; j++) {
                            akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) +
                                            startLocation[0];
                            akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) +
                                            startLocation[1];
                            akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) +
                                            startLocation[2];

                            // flip y and z
                            akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                            akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;

                            // Change the voxel coordinate into millimeter space
                            coord[0] = (akVertex[j].x - startLocation[0]) / direction[0];
                            coord[1] = (akVertex[j].y - startLocation[1]) / direction[1];
                            coord[2] = (akVertex[j].z - startLocation[2]) / direction[2];

                            // Convert the point to axial millimeter DICOM space
                            dicomMatrix.transform(coord, tCoord);

                            // Add in the DICOM origin
                            tCoord[0] = tCoord[0] + startLocation[0];
                            tCoord[1] = tCoord[1] + startLocation[1];
                            tCoord[2] = tCoord[2] + startLocation[2];
                            akVertex[j] = new Point3f(tCoord[0], tCoord[1], tCoord[2]);
                        }
                    } else {

                        for (j = 0; j < akVertex.length; j++) {
                            akVertex[j].x = ((((akVertex[j].x * maxBox) + box[0]) / 2.0f) * direction[0]) +
                                            startLocation[0];
                            akVertex[j].y = ((((akVertex[j].y * maxBox) + box[1]) / 2.0f) * direction[1]) +
                                            startLocation[1];
                            akVertex[j].z = ((((akVertex[j].z * maxBox) + box[2]) / 2.0f) * direction[2]) +
                                            startLocation[2];

                            // flip y and z
                            akVertex[j].y = (2 * startLocation[1]) + (box[1] * direction[1]) - akVertex[j].y;
                            akVertex[j].z = (2 * startLocation[2]) + (box[2] * direction[2]) - akVertex[j].z;
                        }
                    }

                    meshCopy.setVerticies(akVertex);
                    meshesCopy[i] = meshCopy;
                }

                ModelTriangleMesh.save(name, meshesCopy, true, direction, startLocation, box, inverseDicomArray);

            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
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
        loadASCButton.setEnabled(flag);

        colorLabel.setEnabled(flag);
        m_kAdvancedMaterialOptionsButton.setEnabled(flag);
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
     * Subdivide the triangle.
     *
     * @param  sx1  float
     * @param  sy1  float
     * @param  sz1  float
     * @param  sx2  float
     * @param  sy2  float
     * @param  sz2  float
     * @param  sx3  float
     * @param  sy3  float
     * @param  sz3  float
     */
    private void subTriangles(float sx1, float sy1, float sz1, float sx2, float sy2, float sz2, float sx3, float sy3,
                              float sz3) {
        ModelImage imageA = parentScene.getImageA();
        int[] extents = imageA.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];
        float sx4, sy4, sz4;
        float sx5, sy5, sz5;
        float sx6, sy6, sz6;
        int sxf4, syf4, szf4;
        int sxf5, syf5, szf5;
        int sxf6, syf6, szf6;
        float d1, d2, d3;

        sx4 = (sx1 + sx3) / 2.0f;
        sy4 = (sy1 + sy3) / 2.0f;
        sz4 = (sz1 + sz3) / 2.0f;
        sx5 = (sx1 + sx2) / 2.0f;
        sy5 = (sy1 + sy2) / 2.0f;
        sz5 = (sz1 + sz2) / 2.0f;
        sx6 = (sx2 + sx3) / 2.0f;
        sy6 = (sy2 + sy3) / 2.0f;
        sz6 = (sz2 + sz3) / 2.0f;

        sxf4 = Math.max(0, (int) Math.floor(sx4));
        syf4 = Math.max(0, (int) Math.floor(sy4));
        szf4 = Math.max(0, (int) Math.floor(sz4));
        sxf4 = Math.min(xDim - 1, sxf4);
        syf4 = Math.min(yDim - 1, syf4);
        szf4 = Math.min(zDim - 1, szf4);

        sxf5 = Math.max(0, (int) Math.floor(sx5));
        syf5 = Math.max(0, (int) Math.floor(sy5));
        szf5 = Math.max(0, (int) Math.floor(sz5));
        sxf5 = Math.min(xDim - 1, sxf5);
        syf5 = Math.min(yDim - 1, syf5);
        szf5 = Math.min(zDim - 1, szf5);

        sxf6 = Math.max(0, (int) Math.floor(sx6));
        syf6 = Math.max(0, (int) Math.floor(sy6));
        szf6 = Math.max(0, (int) Math.floor(sz6));
        sxf6 = Math.min(xDim - 1, sxf6);
        syf6 = Math.min(yDim - 1, syf6);
        szf6 = Math.min(zDim - 1, szf6);

        d1 = (float) Math.sqrt(((sx1 - sx4) * (sx1 - sx4)) + ((sy1 - sy4) * (sy1 - sy4)) + ((sz1 - sz4) * (sz1 - sz4)));
        d2 = (float) Math.sqrt(((sx1 - sx5) * (sx1 - sx5)) + ((sy1 - sy5) * (sy1 - sy5)) + ((sz1 - sz5) * (sz1 - sz5)));
        d3 = (float) Math.sqrt(((sx4 - sx5) * (sx4 - sx5)) + ((sy4 - sy5) * (sy4 - sy5)) + ((sz4 - sz5) * (sz4 - sz5)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles(sx1, sy1, sz1, sx4, sy4, sz4, sx5, sy5, sz5);
        }

        d1 = (float) Math.sqrt(((sx4 - sx5) * (sx4 - sx5)) + ((sy4 - sy5) * (sy4 - sy5)) + ((sz4 - sz5) * (sz4 - sz5)));
        d2 = (float) Math.sqrt(((sx4 - sx6) * (sx4 - sx6)) + ((sy4 - sy6) * (sy4 - sy6)) + ((sz4 - sz6) * (sz4 - sz6)));
        d3 = (float) Math.sqrt(((sx5 - sx6) * (sx5 - sx6)) + ((sy5 - sy6) * (sy5 - sy6)) + ((sz5 - sz6) * (sz5 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles(sx4, sy4, sz4, sx5, sy5, sz5, sx6, sy6, sz6);
        }

        d1 = (float) Math.sqrt(((sx3 - sx4) * (sx3 - sx4)) + ((sy3 - sy4) * (sy3 - sy4)) + ((sz3 - sz4) * (sz3 - sz4)));
        d2 = (float) Math.sqrt(((sx3 - sx6) * (sx3 - sx6)) + ((sy3 - sy6) * (sy3 - sy6)) + ((sz3 - sz6) * (sz3 - sz6)));
        d3 = (float) Math.sqrt(((sx4 - sx6) * (sx4 - sx6)) + ((sy4 - sy6) * (sy4 - sy6)) + ((sz4 - sz6) * (sz4 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles(sx3, sy3, sz3, sx4, sy4, sz4, sx6, sy6, sz6);
        }

        d1 = (float) Math.sqrt(((sx2 - sx5) * (sx2 - sx5)) + ((sy2 - sy5) * (sy2 - sy5)) + ((sz2 - sz5) * (sz2 - sz5)));
        d2 = (float) Math.sqrt(((sx2 - sx6) * (sx2 - sx6)) + ((sy2 - sy6) * (sy2 - sy6)) + ((sz2 - sz6) * (sz2 - sz6)));
        d3 = (float) Math.sqrt(((sx5 - sx6) * (sx5 - sx6)) + ((sy5 - sy6) * (sy5 - sy6)) + ((sz5 - sz6) * (sz5 - sz6)));

        if ((d1 >= 1.0f) || (d2 >= 1.0f) || (d3 >= 1.0f)) {
            subTriangles(sx2, sy2, sz2, sx5, sy5, sz5, sx6, sy6, sz6);
        }
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

        BranchGroup root = ((SurfaceAttributes) surfaceVector.get(selected)).surface;

        root.detach();

        for (int j = 0; j < root.numChildren(); j++) {
            Shape3D shape = (Shape3D) root.getChild(j);

            ((ModelTriangleMesh) shape.getGeometry(0)).affineTransform(transform);
        }

        surfaceRootBG.addChild(root);
    }

    /**
     * Writes a ModelTriangleMesh and Material to disk in the xml format, based on surface.xsd.
     *
     * @param  kMesh      ModelTriangleMesh surface mesh
     * @param  kMaterial  Material material reference.
     */
    private void writeTriangleMeshXML(ModelTriangleMesh kMesh, Material kMaterial) {

        /* Dialog: Prompt the user to select the filename: */
        String name = getFileName(false);

        if (name == null) {
            return;
        }

        /* Check the filename extension: */
        int i = name.lastIndexOf('.');

        if ((i > 0) && (i < (name.length() - 1))) {
            String extension = name.substring(i + 1).toLowerCase();

            if (!extension.equals("xml")) {
                MipavUtil.displayError("Extension must be .xml");

                return;
            }
        } else {
            name = name + ".xml";
        }

        i = name.lastIndexOf(File.separator);

        String dir = name.substring(0, i + 1);
        name = name.substring(i + 1);

        /* Create the FileSurfaceXML to write the mesh: */
        FileSurfaceXML kSurfaceXML = new FileSurfaceXML(ViewUserInterface.getReference(), null, null, false);

        try {
            kSurfaceXML.writeHeader(name, dir, kMesh, kMaterial);
        } catch (IOException kError) { }
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
