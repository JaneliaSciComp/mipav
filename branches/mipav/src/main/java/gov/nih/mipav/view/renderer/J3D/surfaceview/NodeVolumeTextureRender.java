package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.J3D.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Used to generate a scene node item that that represents the rendering of the volume using texture maps.
 */
public class NodeVolumeTextureRender extends TransformGroup {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Used to represent undefined AXIS_*. */
    private static final int AXIS_UNDEFINED = -1;

    /** Render volume YZ planes in order of increasing/decreasing X. Parameter to selectSlices method. */
    private static final int AXIS_X = 0;

    /** Render volume ZX planes in order of increasing/decreasing Y. Parameter to selectSlices method. */
    private static final int AXIS_Y = 1;

    /** Render volume XY planes in order of increasing/decreasing Z. Parameter to selectSlices method. */
    private static final int AXIS_Z = 2;

    /** Render selected planes in order of increasing/decreasing coordinate. */
    private static final int AXIS_INC = 0;

    /** DOCUMENT ME! */
    private static final int AXIS_DEC = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Maximum voxel resolution of X, Y, Z. */
    private float fMax;

    /** Voxel resolution in the X direction. */
    private float fMaxX;

    /** Voxel resolution in the Y direction. */
    private float fMaxY;

    /** Voxel resolution in the Z direction. */
    private float fMaxZ;

    /** Max Dimension of X, Y, and Z. */
    private int iDimMax;

    /** X Dimension. */
    private int iDimX;

    /** Y Dimension. */
    private int iDimY;

    /** Z Dimension. */
    private int iDimZ;

    /**
     * This contains the childIndexOrder arrays for the axis' OrderedGroup, one for each possible way to slice the
     * volume (by axis and by direction). The first array loops over slice axis (AXIS_[XYZ] constants) and the second
     * array loops over direction (increasing or decreasing coordinate based on AXIS_INC or AXIS_DEC constant). the
     * third array contains the childIndexOrder array which has dimension equal to the number of samples in the volume
     * for the corresponding axis (X, Y, or Z).
     */
    private final int[][][] m_aaaiOrderedGroupChildIndexOrder = new int[3][2][];

    /**
     * The contents of each slice that can be rendered will be contained in an OrderedGroup node. An OrderedGroup is
     * chosen because it ensures the order which the nodes in the group are rendered.
     */
    private final OrderedGroup[] m_akOrderedGroupNodeSlices = new OrderedGroup[3];

    /** Render the slices by increasing or decreasing coordinate. */
    private boolean m_bIncreasingOrder;

    /**
     * Keep track of which direction along the currently selected slicing axis has been selected for rendering the
     * slices.
     */
    private int m_iAxisDirection = AXIS_UNDEFINED;

    /** Keep track of which axis has been currently selected for slicing the volume for rendering. */
    private int m_iAxisSlice = AXIS_UNDEFINED;

    /** The same appearance properties used to render each slice. */
    private Appearance m_kAppearance = new Appearance();

    /**
     * This is the only node attached to the TransformGroup. This node is a container of OrderedGroups, one OrderedGroup
     * instance for each axis. Only one child node is selected at any time.
     */
    private Switch m_kSwitch = new Switch();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create the scene graph node for the rendering of the volume data, everything except the textures from the volume
     * data to be applied to the slices through the volume.
     *
     * @param  image           reference to the image volume (data and header information) the same description
     *                         parameters
     * @param  kVolumeTexture  VolumeTexture-derived instance which contains the texture properties. This description
     *                         parameters for the volume associated with this instance must match the parameters for
     *                         which this node and its geometry.
     */
    public NodeVolumeTextureRender(ModelImage image, VolumeTexture kVolumeTexture) {
        super();
        init(image, kVolumeTexture);
    }

    /**
     * Create the scene graph node for the rendering of the volume data, everything except the textures from the volume
     * data to be applied to the slices through the volume.
     *
     * @param  image             reference to the image volume (data and header information) the same description
     *                           parameters
     * @param  kVolumeTexture    VolumeTexture-derived instance which contains the texture properties. This description
     *                           parameters for the volume associated with this instance must match the parameters for
     *                           which this node and its geometry.
     * @param  iAxis             one of AXIS_X, AXIS_Y, or AXIS_Z which selects the axis of slicing
     * @param  bIncreasingOrder  true to select order of slices by increasing coordinate; false to select decreasing
     */
    public NodeVolumeTextureRender(ModelImage image, VolumeTexture kVolumeTexture, int iAxis,
                                   boolean bIncreasingOrder) {
        super();
        init(image, kVolumeTexture);
        selectSlices(iAxis, bIncreasingOrder);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory of this class.
     */
    public void dispose() {

        // System.out.println("NodeVolumeTextuerRender");
        for (int iSliceX = 0; iSliceX < iDimX; ++iSliceX) {
            Shape3D kShape = (Shape3D) m_akOrderedGroupNodeSlices[AXIS_X].getChild(iSliceX);
            kShape.removeAllGeometries();
        }

        m_akOrderedGroupNodeSlices[AXIS_X].removeAllChildren();
        m_akOrderedGroupNodeSlices[AXIS_X] = null;

        for (int iSliceY = 0; iSliceY < iDimY; ++iSliceY) {
            Shape3D kShape = (Shape3D) m_akOrderedGroupNodeSlices[AXIS_Y].getChild(iSliceY);
            kShape.removeAllGeometries();
        }

        m_akOrderedGroupNodeSlices[AXIS_Y].removeAllChildren();
        m_akOrderedGroupNodeSlices[AXIS_Y] = null;

        for (int iSliceZ = 0; iSliceZ < iDimZ; ++iSliceZ) {
            Shape3D kShape = (Shape3D) m_akOrderedGroupNodeSlices[AXIS_Z].getChild(iSliceZ);
            kShape.removeAllGeometries();
        }

        m_akOrderedGroupNodeSlices[AXIS_Z].removeAllChildren();
        m_akOrderedGroupNodeSlices[AXIS_Z] = null;

        m_aaaiOrderedGroupChildIndexOrder[AXIS_X][AXIS_INC] = null;
        m_aaaiOrderedGroupChildIndexOrder[AXIS_X][AXIS_DEC] = null;
        m_aaaiOrderedGroupChildIndexOrder[AXIS_Y][AXIS_INC] = null;
        m_aaaiOrderedGroupChildIndexOrder[AXIS_Y][AXIS_DEC] = null;
        m_aaaiOrderedGroupChildIndexOrder[AXIS_Z][AXIS_INC] = null;
        m_aaaiOrderedGroupChildIndexOrder[AXIS_Z][AXIS_DEC] = null;
        m_kSwitch.removeAllChildren();
        m_kSwitch = null;
        m_kAppearance.setTexture(null);
        m_kAppearance.setTexCoordGeneration(null);
        m_kAppearance.setTransparencyAttributes(null);
        m_kAppearance = null;
    }

    /**
     * Return which axis has been currently selected for slicing the volume for rendering.
     *
     * @return  the current axis for slicing the volume for rendering.
     */
    public int getAxisSlice() {
        return m_iAxisSlice;
    }

    /**
     * Return the increasing order flag.
     *
     * @return  the increasing/decreasing slice order flag.
     */
    public boolean getIncreasingOrder() {
        return m_bIncreasingOrder;
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
     * @param  kViewDir  view direction vector in virtual world coordinates
     */
    public void updateSlicing(Vector3d kViewDir) {

        // Get the transform from local coordinates of the texture
        // volume renderer node to virtual world coordinates.
        // This call should not result in the returned transformation
        // including the sample-to-local coordinate transform for the
        // the transformation set at this TransformGroup.
        Transform3D kTransform = new Transform3D();

        try {
            getLocalToVworld(kTransform);
        } catch (RestrictedAccessException e) { }

        // What we actually need is the transform from view coordinates
        // to local coordinates.
        kTransform.invert();

        // Convert the view direction vector to local coordinates.
        Vector3d kViewDirLocal = new Vector3d();
        kTransform.transform(kViewDir, kViewDirLocal);

        // Determine which axis is most parallel to the viewing direction.
        // That would be the one which has the largest magnitude.
        // Then if the sign of that view vector for that axis is positive
        // (negative), then select the slices in decreasing (increasing)
        // order in order to get back to front rendering.
        double dMagX = Math.abs(kViewDirLocal.x);
        double dMagY = Math.abs(kViewDirLocal.y);
        double dMagZ = Math.abs(kViewDirLocal.z);

        if ((dMagX >= dMagY) && (dMagX >= dMagZ)) {
            selectSlices(AXIS_X, kViewDirLocal.x < 0.0);
        } else if ((dMagY >= dMagX) && (dMagY >= dMagZ)) {
            selectSlices(AXIS_Y, kViewDirLocal.y < 0.0);
        } else {
            selectSlices(AXIS_Z, kViewDirLocal.z < 0.0);
        }
    }

    /**
     * Automatically call selectSlices with the correct axis (X, Y, or Z) and increasing/decreasing coordinate order
     * flag based on the specified view direction vector such that the slices are rendered back to front for the plane
     * which is most parallel to the view plane.
     *
     * @param  kViewTransform  current view transform in virtual world coordinates
     */
    public void updateView(Transform3D kViewTransform) {

        // Before projection, the viewing direction vector is (0,0,-1).
        // Transform this vector by the current viewing transform.
        Vector3d kViewDir = new Vector3d();
        kViewTransform.transform(new Vector3d(0.0, 0.0, -1.0), kViewDir);

        // Get the transform from local coordinates of the texture
        // volume renderer node to virtual world coordinates.
        // This call should not result in the returned transformation
        // including the sample-to-local coordinate transform for the
        // the transformation set at this TransformGroup.
        Transform3D kTransform = new Transform3D();

        try {
            getLocalToVworld(kTransform);
        } catch (RestrictedAccessException e) { }

        // What we actually need is the transform from view coordinates
        // to local coordinates.
        kTransform.invert();

        // Convert the view direction vector to local coordinates.
        Vector3d kViewDirLocal = new Vector3d();
        kTransform.transform(kViewDir, kViewDirLocal);

        // Determine which axis is most parallel to the viewing direction.
        // That would be the one which has the largest magnitude.
        // Then if the sign of that view vector for that axis is positive
        // (negative), then select the slices in decreasing (increasing)
        // order in order to get back to front rendering.
        double dMagX = Math.abs(kViewDirLocal.x);
        double dMagY = Math.abs(kViewDirLocal.y);
        double dMagZ = Math.abs(kViewDirLocal.z);

        if ((dMagX >= dMagY) && (dMagX >= dMagZ)) {
            selectSlices(AXIS_X, kViewDirLocal.x < 0.0);
        } else if ((dMagY >= dMagX) && (dMagY >= dMagZ)) {
            selectSlices(AXIS_Y, kViewDirLocal.y < 0.0);
        } else {
            selectSlices(AXIS_Z, kViewDirLocal.z < 0.0);
        }
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
     * Initialize the scene graph node for the rendering of the volume data, everything except the textures from the
     * volume data to be applied to the slices through the volume.
     *
     * @param  image           reference to the image volume (data and header information) the same description
     *                         parameters
     * @param  kVolumeTexture  VolumeTexture-derived instance which contains the texture properties. This description
     *                         parameters for the volume associated with this instance must match the parameters for
     *                         which this node and its geometry.
     */
    private void init(ModelImage image, VolumeTexture kVolumeTexture) {

        // Make note of the dimensions of each axis in the volume.
        iDimX = image.getExtents()[0];
        iDimY = image.getExtents()[1];
        iDimZ = image.getExtents()[2];
        iDimMax = iDimX;

        /* Changed to iDim -1 so that the values are consistent with
         * VolumeTexture: */
        fMaxX = (float) (iDimX - 1) * image.getFileInfo(0).getResolutions()[0];
        fMaxY = (float) (iDimY - 1) * image.getFileInfo(0).getResolutions()[1];
        fMaxZ = (float) (iDimZ - 1) * image.getFileInfo(0).getResolutions()[2];
        fMax = fMaxX;

        if (fMaxY > fMax) {
            fMax = fMaxY;
        }

        if (fMaxZ > fMax) {
            fMax = fMaxZ;

        }

        if (iDimY > iDimMax) {
            iDimMax = iDimY;
        }

        if (iDimZ > iDimMax) {
            iDimMax = iDimZ;
        }

        iDimX = iDimMax;
        iDimY = iDimMax;
        iDimZ = iDimMax;

        // Setup the switch node which will contains the three axes
        // to slice the volume for rendering.  Only one of these sets
        // of axis slices can be displayed at a time.  Allow the
        // following items to be modified once the scene graph is compiled:
        // (1) the selected item in the switch node, and(2) the index order
        // in each each OrderedGroup (to allow for always rendering in
        // back to front order).  Also, allow the children in the ordered
        // group to be read once the scene graph is compiled, and allow
        // the node to have its current local to virtual-world transform read.
        for (int iAxis = 0; iAxis < 3; ++iAxis) {
            m_akOrderedGroupNodeSlices[iAxis] = new OrderedGroup();
            m_akOrderedGroupNodeSlices[iAxis].setCapability(OrderedGroup.ALLOW_CHILD_INDEX_ORDER_WRITE);
            m_akOrderedGroupNodeSlices[iAxis].setCapability(Group.ALLOW_CHILDREN_READ);
            m_akOrderedGroupNodeSlices[iAxis].setCapability(Group.ALLOW_CHILDREN_WRITE);
            m_akOrderedGroupNodeSlices[iAxis].setCapability(BranchGroup.ALLOW_DETACH);
            m_kSwitch.addChild(m_akOrderedGroupNodeSlices[iAxis]);
        }

        m_kSwitch.setWhichChild(Switch.CHILD_NONE);
        m_kSwitch.setCapability(Switch.ALLOW_SWITCH_WRITE);
        m_kSwitch.setCapability(Switch.ALLOW_CHILDREN_WRITE);
        m_kSwitch.setCapability(Switch.ALLOW_CHILDREN_READ);
        addChild(m_kSwitch);
        setCapability(Node.ALLOW_LOCAL_TO_VWORLD_READ);
        setCapability(Group.ALLOW_CHILDREN_READ);
        setCapability(Group.ALLOW_CHILDREN_WRITE);
        setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);

        // Allocate for the array to store the childIndex.
        // The last dimension will be sized based on the number of
        // samples in the volume for that axis.  Then the array
        // will be filled in with the numbers 0,...,N-1 or
        // N-1,...,0 based on whether the direction is increasing
        // or decreasing.
        m_aaaiOrderedGroupChildIndexOrder[AXIS_X][AXIS_INC] = new int[iDimX];
        m_aaaiOrderedGroupChildIndexOrder[AXIS_X][AXIS_DEC] = new int[iDimX];
        m_aaaiOrderedGroupChildIndexOrder[AXIS_Y][AXIS_INC] = new int[iDimY];
        m_aaaiOrderedGroupChildIndexOrder[AXIS_Y][AXIS_DEC] = new int[iDimY];
        m_aaaiOrderedGroupChildIndexOrder[AXIS_Z][AXIS_INC] = new int[iDimZ];
        m_aaaiOrderedGroupChildIndexOrder[AXIS_Z][AXIS_DEC] = new int[iDimZ];

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
        kTransparencyAttr.setTransparency(1.0f);
        kTransparencyAttr.setTransparencyMode(TransparencyAttributes.BLENDED);
        kTransparencyAttr.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        kTransparencyAttr.setCapability(TransparencyAttributes.ALLOW_MODE_WRITE);
        kTransparencyAttr.setCapability(TransparencyAttributes.ALLOW_BLEND_FUNCTION_WRITE);
        kTransparencyAttr.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA); // Good
        kTransparencyAttr.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA); // Good
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

        // Appearance: (TexCoordGeneration)
        // Allow the TexCoordGeneration attribute to be modified.
        m_kAppearance.setTexCoordGeneration(kVolumeTexture.getTexCoordGeneration());
        m_kAppearance.setCapability(Appearance.ALLOW_TEXGEN_WRITE);

        // These are the coordinate bounds of the volume
        double dX0 = -fMaxX / fMax;
        double dY0 = -fMaxY / fMax;
        double dZ0 = -fMaxZ / fMax;
        double dX1 = fMaxX / fMax;
        double dY1 = fMaxY / fMax;
        double dZ1 = fMaxZ / fMax;

        // Create the shapes (geometry and appearance) for each possible
        // axis of slicing the geometry.  Each slice is generated
        // for the increasing order for the corresponding OrderedGroup.
        // The default ordering of the OrderedGroup (specified by setting
        // setChildIndexOrder(null)) must be used in order to insert into
        // the begining of the order for the decreasing OrderedGroup.
        // Fill in the childIndexOrder array for each axis/direction
        // combination.

        float offset = -fMaxX / fMax;
        float range = 2.0f * -offset;
        float incSliceAmt = range / iDimX;
        offset = offset + (incSliceAmt / 2.0f);

        for (int iSliceX = 0; iSliceX < iDimX; ++iSliceX) {
            QuadArray kGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_3);
            kGeometry.setCoordinate(0, new Point3d(offset, dY0, dZ0));
            kGeometry.setCoordinate(1, new Point3d(offset, dY1, dZ0));
            kGeometry.setCoordinate(2, new Point3d(offset, dY1, dZ1));
            kGeometry.setCoordinate(3, new Point3d(offset, dY0, dZ1));
            offset = offset + incSliceAmt;

            // Create the shape (geometry+appearance) for this slice.
            // Allow the appearance to be read.
            Shape3D kShape = new Shape3D(kGeometry, m_kAppearance);
            kShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            kShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            kShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
            m_akOrderedGroupNodeSlices[AXIS_X].addChild(kShape);

            // Assign the slice index for increasing/decreasing order
            // which is used to achieve back-to-front ordering.
            m_aaaiOrderedGroupChildIndexOrder[AXIS_X][AXIS_INC][iSliceX] = iSliceX;
            m_aaaiOrderedGroupChildIndexOrder[AXIS_X][AXIS_DEC][iSliceX] = iDimX - 1 - iSliceX;
        }

        offset = -fMaxY / fMax;
        range = 2.0f * -offset;
        incSliceAmt = range / iDimY;
        offset = offset + (incSliceAmt / 2.0f);

        for (int iSliceY = 0; iSliceY < iDimY; ++iSliceY) {

            // Setup the geometry of the rectangular slice.
            QuadArray kGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_3);
            kGeometry.setCoordinate(0, new Point3d(dX0, offset, dZ0));
            kGeometry.setCoordinate(1, new Point3d(dX0, offset, dZ1));
            kGeometry.setCoordinate(2, new Point3d(dX1, offset, dZ1));
            kGeometry.setCoordinate(3, new Point3d(dX1, offset, dZ0));
            offset = offset + incSliceAmt;

            // Create the shape (geometry+appearance) for this slice.
            // Allow the appearance to be read.
            Shape3D kShape = new Shape3D(kGeometry, m_kAppearance);
            kShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            kShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            m_akOrderedGroupNodeSlices[AXIS_Y].addChild(kShape);

            // Assign the slice index for increasing/decreasing order
            // which is used to achieve back-to-front ordering.
            m_aaaiOrderedGroupChildIndexOrder[AXIS_Y][AXIS_INC][iSliceY] = iSliceY;
            m_aaaiOrderedGroupChildIndexOrder[AXIS_Y][AXIS_DEC][iSliceY] = iDimY - 1 - iSliceY;
        }

        offset = -fMaxZ / fMax;
        range = 2.0f * -offset;
        incSliceAmt = range / iDimZ;
        offset = offset + (incSliceAmt / 2.0f);

        for (int iSliceZ = 0; iSliceZ < iDimZ; ++iSliceZ) {
            QuadArray kGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_3);
            kGeometry.setCoordinate(0, new Point3d(dX0, dY0, offset));
            kGeometry.setCoordinate(1, new Point3d(dX1, dY0, offset));
            kGeometry.setCoordinate(2, new Point3d(dX1, dY1, offset));
            kGeometry.setCoordinate(3, new Point3d(dX0, dY1, offset));
            offset = offset + incSliceAmt;

            // Create the shape (geometry+appearance) for this slice.
            // Allow the appearance to be read.
            Shape3D kShape = new Shape3D(kGeometry, m_kAppearance);
            kShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            kShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            m_akOrderedGroupNodeSlices[AXIS_Z].addChild(kShape);

            // Assign the slice index for increasing/decreasing order
            // which is used to achieve back-to-front ordering.
            m_aaaiOrderedGroupChildIndexOrder[AXIS_Z][AXIS_INC][iSliceZ] = iSliceZ;
            m_aaaiOrderedGroupChildIndexOrder[AXIS_Z][AXIS_DEC][iSliceZ] = iDimZ - 1 - iSliceZ;
        }
    }

    /**
     * Select along which axis and in which direction slicing in the volume is to be performed. THIS METHOD SHOULD BE
     * MADE PRIVATE IN THE FINAL RELEASE.
     *
     * @param   iAxis             one of AXIS_X, AXIS_Y, or AXIS_Z which selects the axis of slicing
     * @param   bIncreasingOrder  true to select order of slices by increasing coordinate; false to select decreasing
     *
     * @throws  java.lang.IllegalArgumentException  DOCUMENT ME!
     */
    private void selectSlices(int iAxis, boolean bIncreasingOrder) {

        // Select the axis of slices, but only change if the axis is
        // different.
        boolean bChangedAxis = false;

        switch (iAxis) {

            case AXIS_X:
            case AXIS_Y:
            case AXIS_Z:
                if (iAxis != m_iAxisSlice) {
                    m_kSwitch.setWhichChild(iAxis);
                    m_iAxisSlice = iAxis;
                    bChangedAxis = true;
                }

                break;

            default:
                throw new java.lang.IllegalArgumentException("iAxis");
        }

        m_bIncreasingOrder = bIncreasingOrder;

        // Select the ordering of the slices along that axis.
        // Only update if the direction along the same axis changed
        // or if the axis itself changed.
        int iAxisDir = AXIS_DEC;

        if (bIncreasingOrder) {
            iAxisDir = AXIS_INC;
        }

        // This needs to be done each time.
        // if (bChangedAxis || (iAxisDir != m_iAxisDirection)) {
        try {
            m_akOrderedGroupNodeSlices[iAxis].setChildIndexOrder(m_aaaiOrderedGroupChildIndexOrder[iAxis][iAxisDir]);
            m_iAxisDirection = iAxisDir;
        } catch (IllegalArgumentException e) { }
        // }
    }


}
