package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.J3D.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Used to generate a scene node item that that represents the rendering of the volume using texture maps.
 */
public class NodeAlignedVolumeTextureRender extends NodeVolumeTextureRender {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Used to select which nodes in the switch is selected. */
    private static final int SLICES_FINE = 0;

    /** DOCUMENT ME! */
    private static final int SLICES_COARSE = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Maximum size of any side of cube for rendering slices. */
    private final float m_fCubeSize;

    /** Size of each axis in the original volume. */
    private final float m_fSizeX, m_fSizeY, m_fSizeZ;

    /** DOCUMENT ME! */
    private float m_fSliceSpacingCoarse = 1.50f;

    /** Spacing between slices. */
    private float m_fSliceSpacingFine = 1.0f;


    /** The same appearance properties used to render each slice. */
    private Appearance m_kAppearance = new Appearance();

    /** DOCUMENT ME! */
    private OrderedGroup m_kOrderedGroupSlicesCoarse = new OrderedGroup();

    /**
     * The slicing through the volume will be contained in an OrderedGroup to ensure that the slices are rendered
     * back-to-front for correct blending effect. One set each is available for fine and coarse resolution quality
     * rendering.
     */
    private OrderedGroup m_kOrderedGroupSlicesFine = new OrderedGroup();

    /**
     * This is the only node attached to this TransformGroup. It will enable either the selection of the "fine" or
     * "coarse" resolution slice sampling.
     */
    private Switch m_kSwitch = new Switch();

    /** DOCUMENT ME! */
    private Transform3D m_kTempLocalViewTransform = new Transform3D();

    /** DOCUMENT ME! */
    private Matrix3d m_kTempRotateMatrix = new Matrix3d();

    /** These are declared in the class to avoid allocating temporary instances each call into methods of this class. */
    private Transform3D m_kTempWorldToLocalTransform = new Transform3D();

    /** Keep track of the texture instance. */
    private VolumeTexture m_kVolumeTexture;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create the scene graph node for the rendering of the volume data, everything except the textures from the volume
     * data to be applied to the slices through the volume.
     *
     * @param  image           description of the parameters for the volumes which can be rendered with this node, where
     *                         all volumes must shared the same description parameters
     * @param  kVolumeTexture  VolumeTexture-derived instance which contains the texture properties. This description
     *                         parameters for the volume associated with this instance must match the parameters for
     *                         which this node and its geometry.
     */
    public NodeAlignedVolumeTextureRender(ModelImage image, VolumeTexture kVolumeTexture) {
        super(image, kVolumeTexture);

        m_kVolumeTexture = kVolumeTexture;

        // Setup the switch for displaying either the "fine" or "coarse" slicing.
        m_kSwitch.setCapability(Switch.ALLOW_SWITCH_WRITE);
        m_kSwitch.setCapability(Switch.ALLOW_CHILDREN_WRITE);
        m_kSwitch.setCapability(Switch.ALLOW_CHILDREN_READ);
        m_kOrderedGroupSlicesFine.setCapability(OrderedGroup.ALLOW_CHILDREN_READ);
        m_kOrderedGroupSlicesFine.setCapability(OrderedGroup.ALLOW_CHILDREN_WRITE);
        m_kOrderedGroupSlicesFine.setCapability(OrderedGroup.ALLOW_CHILDREN_EXTEND);
        m_kOrderedGroupSlicesCoarse.setCapability(OrderedGroup.ALLOW_CHILDREN_READ);
        m_kOrderedGroupSlicesCoarse.setCapability(OrderedGroup.ALLOW_CHILDREN_WRITE);
        m_kOrderedGroupSlicesCoarse.setCapability(OrderedGroup.ALLOW_CHILDREN_EXTEND);

        m_kSwitch.addChild(m_kOrderedGroupSlicesFine);
        m_kSwitch.addChild(m_kOrderedGroupSlicesCoarse);
        addChild(m_kSwitch);

        // Allow the transform to be read (relative to world coordinates)
        // and updated.  This is done in the updateView method.
        setCapability(TransformGroup.ALLOW_LOCAL_TO_VWORLD_READ);
        setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);

        // Appearance: RenderingAttributes
        // Do not waste time rendering when alpha is zero.
        RenderingAttributes kRenderingAttributes = new RenderingAttributes();
        kRenderingAttributes.setAlphaTestValue(0.0f);
        kRenderingAttributes.setAlphaTestFunction(RenderingAttributes.GREATER);
        m_kAppearance.setRenderingAttributes(kRenderingAttributes);

        // Appearance: PolygonAttributes
        // Do not perform any backface/frontbace polygon culling
        // since we will want to render either side of a slice face.
        PolygonAttributes kPolygonAttributes = new PolygonAttributes();
        kPolygonAttributes.setCullFace(PolygonAttributes.CULL_NONE);
        kPolygonAttributes.setPolygonMode(PolygonAttributes.POLYGON_FILL);
        m_kAppearance.setPolygonAttributes(kPolygonAttributes);

        // Appearance: Material
        // Disable lighting so that the color information comes from
        // the texture maps.
        Material kMaterial = new Material();
        kMaterial.setLightingEnable(false);
        m_kAppearance.setMaterial(kMaterial);

        // Appearance: TransparencyAttributes
        // Use blended transparency mode which has the default blending
        // operation set to alpha_src*src + (1-alpha_src)*dst.  This works
        // only for back to front ordering.
        TransparencyAttributes kTransparencyAttr = new TransparencyAttributes();
        kTransparencyAttr.setTransparencyMode(TransparencyAttributes.BLENDED);
        kTransparencyAttr.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        kTransparencyAttr.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA);
        kTransparencyAttr.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA);
        kTransparencyAttr.setTransparency(0.0f);
        m_kAppearance.setTransparencyAttributes(kTransparencyAttr);
        m_kAppearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ);
        m_kAppearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);

        // Appearance: TextureAttributes
        // Use Replace mode because we don't want to have to worry about
        // what color the slice is rendered as before the texture is
        // applied to it.
        TextureAttributes kTextureAttr = new TextureAttributes();
        kTextureAttr.setTextureMode(TextureAttributes.REPLACE);
        m_kAppearance.setTextureAttributes(kTextureAttr);

        // Appearance: Texture
        // Allow the Texture attribute to be modified.
        m_kAppearance.setTexture(kVolumeTexture.getTexture());
        m_kAppearance.setCapability(Appearance.ALLOW_TEXTURE_WRITE);

        // Appearance: TexCoordGeneration
        m_kAppearance.setTexCoordGeneration(kVolumeTexture.getTexCoordGeneration());

        // Enable this block to disable texture rendering and to see
        // an outline of the volume slices.
        if (false) {
            ColoringAttributes kColoringAttributes = new ColoringAttributes();
            kColoringAttributes.setColor(1.0f, 1.0f, 1.0f);
            m_kAppearance.setColoringAttributes(kColoringAttributes);
            kTransparencyAttr.setTransparencyMode(TransparencyAttributes.NONE);
            kPolygonAttributes.setPolygonMode(PolygonAttributes.POLYGON_LINE);
            m_kAppearance.setTexture(null);
        }

        // Get the dimensions of each axis in the volume.
        int iDimX = image.getExtents()[0];
        int iDimY = image.getExtents()[1];
        int iDimZ = image.getExtents()[2];

        // Get the spacing between samples along each axis in the volume.
        float fSpacingX = image.getFileInfo(0).getResolutions()[0];
        float fSpacingY = image.getFileInfo(0).getResolutions()[1];
        float fSpacingZ = image.getFileInfo(0).getResolutions()[2];

        // Determine the minimum spacing.
        float fSliceSpacing = fSpacingX;

        if (fSpacingY < fSliceSpacing) {
            fSliceSpacing = fSpacingY;
        }

        if (fSpacingZ < fSliceSpacing) {
            fSliceSpacing = fSpacingZ;
        }

        // Determine the length of each axis.
        m_fSizeX = (float) iDimX * fSpacingX;
        m_fSizeY = (float) iDimY * fSpacingY;
        m_fSizeZ = (float) iDimZ * fSpacingZ;

        // What is the largest axis.
        float fSizeMax = m_fSizeX;

        if (fSizeMax < m_fSizeY) {
            fSizeMax = m_fSizeY;
        }

        if (fSizeMax < m_fSizeZ) {
            fSizeMax = m_fSizeZ;
        }

        // Determine the maximum length of any side of the cube so that
        // when the slices are rotated at any orientation, the texture
        // volume is still contained inside the slice volume.
        m_fCubeSize = (float) Math.sqrt(3.0) * fSizeMax;

        // Setup default "fine" and "coarse" slice sampling based on the minimum
        // slice spacing within the original volume.  By default, use the
        // "coarse" slice sampling.
        setSliceSpacingFine(fSliceSpacing);
        setSliceSpacingCoarse(fSliceSpacing);
        useSliceSpacingCoarse();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory of this class.
     */
    public void dispose() {
        System.out.println("NodeVolumeTextuerRender");

        m_kAppearance.setTexture(null);
        m_kAppearance.setTexCoordGeneration(null);
        m_kAppearance.setTransparencyAttributes(null);
        m_kAppearance = null;
        m_kVolumeTexture = null;

        disposeSlices(m_kOrderedGroupSlicesCoarse);
        disposeSlices(m_kOrderedGroupSlicesFine);
        m_kOrderedGroupSlicesCoarse = null;
        m_kOrderedGroupSlicesFine = null;
        m_kSwitch = null;

        m_kTempWorldToLocalTransform = null;
        m_kTempRotateMatrix = null;
        m_kTempLocalViewTransform = null;
    }

    /**
     * Retrieve the current "coarse" spacing between slices.
     *
     * @return  float Current "coarse" spacing between slices.
     */
    public float getSliceSpacingCoarse() {
        return m_fSliceSpacingCoarse;
    }

    /**
     * Retrieve the current "fine" spacing between slices.
     *
     * @return  float Current "fine" spacing between slices.
     */
    public float getSliceSpacingFine() {
        return m_fSliceSpacingFine;
    }

    /**
     * Retrieves whether the "coarse" slice sampling is currently selected.
     *
     * @return  boolean Returns true if "coarse" slice sampling is currently selected.
     */
    public boolean isUsingSliceSpacingCoarse() {
        return SLICES_COARSE == m_kSwitch.getWhichChild();
    }

    /**
     * Retrieves whether the "fine" slice sampling is currently selected.
     *
     * @return  boolean Returns true if "fine" slice sampling is currently selected.
     */
    public boolean isUsingSliceSpacingFine() {
        return SLICES_FINE == m_kSwitch.getWhichChild();
    }

    /**
     * Set the desired "coarse" spacing between slices.
     *
     * @param  fSpacing  float Desired "coarse" spacing between slices.
     */
    public void setSliceSpacingCoarse(float fSpacing) {
        disposeSlices(m_kOrderedGroupSlicesCoarse);
        createSlices(m_kOrderedGroupSlicesCoarse, fSpacing);
        m_fSliceSpacingCoarse = fSpacing;
    }

    /**
     * Set the desired "fine" spacing between slices.
     *
     * @param  fSpacing  float Desired "fine" spacing between slices.
     */
    public void setSliceSpacingFine(float fSpacing) {
        disposeSlices(m_kOrderedGroupSlicesFine);
        createSlices(m_kOrderedGroupSlicesFine, fSpacing);
        m_fSliceSpacingFine = fSpacing;
    }

    /**
     * Not used at the moment. Update the opacity from the relate opacity change slider
     *
     * @param  iValue  Opacity value from the opacity slider
     */
    public void updateOpacity(int iValue) {
        System.out.println("NodeVolumeTextureRender.updateOpacity iValue = " + iValue);

        TransparencyAttributes tap = m_kAppearance.getTransparencyAttributes();
        tap.setTransparency(1.0f - (iValue / 100.0f)); // 0 = Opaque
    }

    /**
     * Automatically call selectSlices with the correct axis (X, Y, or Z) and increasing/decreasing coordinate order
     * flag based on the specified view direction vector such that the slices are rendered back to front for the plane
     * which is most parallel to the view plane.
     *
     * @param  kViewTransform  current view transform in virtual world coordinates
     */
    public void updateView(Transform3D kViewTransform) {

        // Get the transform from local coordinates of the texture
        // volume renderer node to virtual world coordinates.
        // What we actually need is the transform from view coordinates
        // to local coordinates.
        try {
            m_kTempWorldToLocalTransform.setIdentity();
            setTransform(m_kTempWorldToLocalTransform);
            getLocalToVworld(m_kTempWorldToLocalTransform);
            m_kTempWorldToLocalTransform.invert();
        } catch (RestrictedAccessException e) { }

        // Get rotational component of the view transform in local coordinates.
        m_kTempLocalViewTransform.mul(kViewTransform, m_kTempWorldToLocalTransform);
        m_kTempLocalViewTransform.get(m_kTempRotateMatrix);
        m_kTempLocalViewTransform.set(m_kTempRotateMatrix);

        // Rotate the slices so that they are aligned with the view plane.
        setTransform(m_kTempLocalViewTransform);

        // Transform the plane equations for the texture coordinate
        // generation by the inverse of the view transformation in
        // local coordinates.
        m_kTempLocalViewTransform.invert();
        m_kVolumeTexture.setTransform(m_kTempLocalViewTransform);
    }

    /**
     * Select the "coarse" slice sampling for rendering.
     */
    public void useSliceSpacingCoarse() {
        m_kSwitch.setWhichChild(SLICES_COARSE);
    }

    /**
     * Select the "fine" slice sampling for rendering.
     */
    public void useSliceSpacingFine() {
        m_kSwitch.setWhichChild(SLICES_FINE);
    }

    /**
     * Used to call dispose of this class to clean up memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        dispose();
        super.finalize();
    }

    /**
     * Create the z-axis aligned slices through the volume so that any rotation of the volume does not result in the
     * slices getting clipped by the texture volume.
     *
     * @param  kOrderedGroupSlices  Node containing the sequenced ordering of the slices for correct back to front
     *                              rendering when looking down the -Z axis.
     * @param  fSpacing             float Spacing between slices
     */
    private void createSlices(OrderedGroup kOrderedGroupSlices, float fSpacing) {

        // These are the coordinate bounds of the volume
        double dX0 = -m_fCubeSize / m_fSizeX;
        double dY0 = -m_fCubeSize / m_fSizeY;
        double dZ0 = -m_fCubeSize / m_fSizeZ;
        double dX1 = +m_fCubeSize / m_fSizeX;
        double dY1 = +m_fCubeSize / m_fSizeY;
        double dZ1 = +m_fCubeSize / m_fSizeZ;

        // Create the shapes (geometry and appearance) for slicing the volume.
        // Each slice is generated for the increasing order for the
        // corresponding OrderedGroup.
        int iNumSlices = (int) Math.ceil(m_fCubeSize / fSpacing);
        double incSliceAmt = (dZ1 - dZ0) / iNumSlices;
        double offset = dZ0 + (incSliceAmt / 2);

        for (int iSlice = 0; iSlice < iNumSlices; ++iSlice) {

            QuadArray kGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_3);
            kGeometry.setCoordinate(0, new Point3d(dX0, dY0, offset));
            kGeometry.setCoordinate(1, new Point3d(dX1, dY0, offset));
            kGeometry.setCoordinate(2, new Point3d(dX1, dY1, offset));
            kGeometry.setCoordinate(3, new Point3d(dX0, dY1, offset));
            offset = offset + incSliceAmt;

            // Create the shape (geometry+appearance) for this slice.
            // Allow the appearance to be read.
            Shape3D kShape = new Shape3D(kGeometry, m_kAppearance);
            kShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);

            BranchGroup bg = new BranchGroup();
            bg.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
            bg.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
            bg.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
            bg.setCapability(BranchGroup.ALLOW_DETACH);
            bg.addChild(kShape);

            // kOrderedGroupSlices.addChild(kShape);
            kOrderedGroupSlices.addChild(bg);
        }
    }

    /**
     * Remove all children (geometries) of the specified ordered group node.
     *
     * @param  kOrderedGroupSlices  OrderedGroup Node which is to be cleared of all children.
     */
    private void disposeSlices(OrderedGroup kOrderedGroupSlices) {
        int iNumSlices = kOrderedGroupSlices.numChildren();

        for (int iSlice = 0; iSlice < iNumSlices; ++iSlice) {

            // Shape3D kShape = (Shape3D) kOrderedGroupSlices.getChild(iSlice);
            Shape3D kShape = (Shape3D) (((BranchGroup) (kOrderedGroupSlices.getChild(iSlice))).getChild(0));
            kShape.removeAllGeometries();
        }

        kOrderedGroupSlices.removeAllChildren();
    }
}
