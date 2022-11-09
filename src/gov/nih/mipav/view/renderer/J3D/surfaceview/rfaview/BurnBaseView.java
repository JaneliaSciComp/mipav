package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * The default probe burning type view. The class define the default burning point's geometry shape, and burning path
 * mark behavior.
 *
 * @author  Ruida Cheng
 */
public class BurnBaseView {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Burning point surface color. */
    protected Color4f burnColor;

    /** Current burning point index. */
    protected int burnIndex = -1;

    /** Burning point branch group array, used to switch between different buring point. */
    protected BranchGroup[] burningBG;

    /** The root branch group the default burning point. */
    protected BranchGroup burnRootParentBG;

    /** Burning type. */
    protected int burnType;

    /** Volume difference in voxels for the tumor surface and burning sphere, ellipsoid packings. */
    protected int diffVoxels;

    /** DOCUMENT ME! */
    protected float[] m_afLength;

    /** DOCUMENT ME! */
    protected int[] m_aiConnect;

    /** The 3D MRI image stored as a 1D array. The mapping from (x,y,z) to 1D is: index = x + xbound*(y + ybound*z). */
    protected int[] m_aiImage;

    /** DOCUMENT ME! */
    protected BitSet m_aiMask;

    /** DOCUMENT ME! */
    protected UnorderedSetInt[] m_akAdjacent;

    /** DOCUMENT ME! */
    protected Point3f[] m_akVertex;

    /** The size of a voxel, in voxel units. */
    protected float m_fXDelta, m_fYDelta, m_fZDelta;

    /** DOCUMENT ME! */
    protected int m_iBackThreshold;

    /** DOCUMENT ME! */
    protected int m_iBrightThreshold;

    /** DOCUMENT ME! */
    protected int m_iDMax = 0;

    /** DOCUMENT ME! */
    protected int m_iEQuantity;

    /** DOCUMENT ME! */
    protected int m_iMaxThreshold;

    /** DOCUMENT ME! */
    protected int m_iMedianIntensity;

    /** histogram parameters. */
    protected int m_iMinThreshold;

    /** DOCUMENT ME! */
    protected int m_iTQuantity;

    /** mesh data. */
    protected int m_iVQuantity;

    /** The MRI image bounds and quantity of voxels. */
    protected int m_iXBound, m_iYBound, m_iZBound, m_iQuantity;

    /** DOCUMENT ME! */
    protected Point3f m_kCenter;

    /** DOCUMENT ME! */
    protected HashMap<Edge,Integer> m_kEMap; // map<Edge,int>

    /** DOCUMENT ME! */
    protected Matrix3f m_kRotate;

    /** Reference to Mask burning attribute. */
    protected Vector<MaskBurnAttribute> maskBurnVector = null; 

    /** Initialized the number of burning point. */
    protected int numBurn = 100;

    /** Agent to set the burning point sphere pickable. */
    protected PickCanvas pickCanvas;

    /** JPanelProbe reference. */
    protected JPanelProbe probePanel;

    /** Burning sphere radius, semiX, Y, Z in point format. */
    protected Point3f radiusPt;

    /** Flag to show the burning point label. */
    protected boolean showLabels;

    /** Default burning point array. */
    protected BranchGroup[] sphereBG;

    /** Burning point sphere. */
    protected Sphere spheres;

    /** Surface Render reference. */
    protected SurfaceRender surfaceRender;

    /** Burning point label array. */
    protected BranchGroup[] textLabelBG;

    /** Burning point center. */
    protected Vector3f translate;

    /** Reference to burning point voluem mask. */
    protected BitSet volumeMask;

    /** Default volume space burning point radius. */
    protected float volumeSpaceRadius = 0.05f;

    /** Define attenuation lighting colors. */
    Color3f ambientColor = new Color3f(1.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    Color3f black = new Color3f(Color.black);

    /** DOCUMENT ME! */
    Color3f blue = new Color3f(Color.blue);

    /** DOCUMENT ME! */
    Color3f cyan = new Color3f(Color.cyan);

    /** DOCUMENT ME! */
    Color3f diffuseColor = new Color3f(1.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    Color3f emissiveColor = new Color3f(0.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    Color3f green = new Color3f(Color.green);

    /** vector stack to record the burning index. */
    Vector<Integer> indexVector = new Vector<Integer>();

    /** DOCUMENT ME! */
    Color3f orange = new Color3f(new Color(225, 50, 0));

    /** DOCUMENT ME! */
    Color4f orangeColor = new Color4f(new Color(225, 50, 0));

    /** DOCUMENT ME! */
    Color3f pink = new Color3f(Color.pink);

    /** DOCUMENT ME! */
    Color3f purple = new Color3f(new Color(128, 0, 128));

    /** DOCUMENT ME! */
    Color4f purpleColor = new Color4f(new Color(128, 0, 128));

    /** Define colors. */
    Color3f red = new Color3f(Color.red);

    /** DOCUMENT ME! */
    Color3f sepcualarColor = new Color3f(1.0f, 1.0f, 1.0f);

    /** DOCUMENT ME! */
    Color3f silver = new Color3f(Color.darkGray);

    /** DOCUMENT ME! */
    Color3f yellow = new Color3f(Color.yellow);

    /** DOCUMENT ME! */
    private Shape3D[] m_akSurfaceShape;

    /** factor above median at which edge values are taken to zero. */
    private int nVoxels = 0;

    /** image space burning point radius. */
    private float radius;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor. Setup the burning point related branch groups.
     *
     * @param  _surfaceRender     SurfaceRender refernece.
     * @param  _probePanel        JPanelProbe probe control panel reference.
     * @param  _burnRootParentBG  BranchGroup root of the burning point.
     */
    public BurnBaseView(SurfaceRender _surfaceRender, JPanelProbe _probePanel, BranchGroup _burnRootParentBG) {
        surfaceRender = _surfaceRender;
        probePanel = _probePanel;
        burnRootParentBG = _burnRootParentBG;
        burningBG = new BranchGroup[numBurn];
        sphereBG = new BranchGroup[numBurn];
        textLabelBG = new BranchGroup[numBurn];
        radiusPt = new Point3f();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   t3D  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static Vector3f getRotAngle(Transform3D t3D) {
        Matrix3d m1 = new Matrix3d();
        double c, tRx, tRy;
        Vector3f Angles = new Vector3f();

        t3D.get(m1);
        Angles.y = (float) Math.asin(m1.getElement(0, 2));
        c = Math.cos(Angles.y);

        if (Math.abs(c) > 0.00001) {
            tRx = m1.getElement(2, 2) / c;
            tRy = -m1.getElement(1, 2) / c;
            Angles.x = (float) Math.atan2(tRy, tRx);

            tRx = m1.getElement(0, 0) / c;
            tRy = -m1.getElement(0, 1) / c;
            Angles.z = (float) Math.atan2(tRy, tRx);
        } else {
            Angles.x = (float) 0.0;

            tRx = m1.getElement(1, 1) / c;
            tRy = m1.getElement(1, 0) / c;
            Angles.z = (float) Math.atan2(tRy, tRx);
        }

        if (Angles.x < 0.0) {
            Angles.x += 2 * Math.PI;
        } else if (Angles.x > (2 * Math.PI)) {
            Angles.x -= 2 * Math.PI;
        }

        if (Angles.y < 0.0) {
            Angles.y += 2 * Math.PI;
        } else if (Angles.y > (2 * Math.PI)) {
            Angles.y -= 2 * Math.PI;
        }

        if (Angles.z < 0.0) {
            Angles.z += 2 * Math.PI;
        } else if (Angles.z > (2 * Math.PI)) {
            Angles.z -= 2 * Math.PI;
        }

        if ((Angles.x < 0.001) && (Angles.x > -0.001)) {
            Angles.x = (float) 0.0;
        }

        if ((Angles.y < 0.001) && (Angles.y > -0.001)) {
            Angles.y = (float) 0.0;
        }

        if ((Angles.z < 0.001) && (Angles.z > -0.001)) {
            Angles.z = (float) 0.0;
        }

        if (Angles.x == 0.0) {
            Angles.x = Math.abs(Angles.x);
        }

        if (Angles.y == 0.0) {
            Angles.y = Math.abs(Angles.y);
        }

        if (Angles.z == 0.0) {
            Angles.z = Math.abs(Angles.z);
        }

        if (Angles.x == (2 * Math.PI)) {
            Angles.x = (float) 0.0;
        }

        if (Angles.y == (2 * Math.PI)) {
            Angles.y = (float) 0.0;
        }

        if (Angles.z == (2 * Math.PI)) {
            Angles.z = (float) 0.0;
        }

        return (Angles);
    }

    /**
     * Build the image scene graph structure.
     *
     * @param  radius  burning point sphere radius
     * @param  time    burning point burning time duration.
     */
    public void buildBurnSceneGraph(float radius, float time) {
        burningBG[burnIndex] = new BranchGroup();

        burningBG[burnIndex].setCapability(Group.ALLOW_CHILDREN_EXTEND);
        burningBG[burnIndex].setCapability(Group.ALLOW_CHILDREN_READ);
        burningBG[burnIndex].setCapability(Group.ALLOW_CHILDREN_WRITE);
        burningBG[burnIndex].setCapability(Node.ALLOW_PICKABLE_READ);
        burningBG[burnIndex].setCapability(Node.ALLOW_PICKABLE_WRITE);
        burningBG[burnIndex].setCapability(BranchGroup.ALLOW_DETACH);
        burningBG[burnIndex].setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        burningBG[burnIndex].setCapability(Node.ALLOW_COLLIDABLE_READ);

        Transform3D trans = new Transform3D();

        trans.setScale(0.1);

        TransformGroup transforms = new TransformGroup(trans);

        transforms.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        transforms.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
        transforms.setCapability(BranchGroup.ALLOW_DETACH);

        Transform3D pos = probePanel.getProbeBase().getCoordinate();

        pos.setScale(0.1);
        transforms.setTransform(pos);
        burningBG[burnIndex].addChild(transforms);
    }

    /**
     * Only used by the default burning point type to reset the image sence graph when the burning sphere radius and
     * time changes.
     *
     * @param  radius  burning sphere radius
     * @param  time    burning sphere duration time
     * @param  color   burning sphere color
     * @param  index   selected burning sphere index
     */
    public void buildBurnSceneGraph(float radius, float time, Color3f color, int index) {
        Transform3D prevTrans = new Transform3D();

        ((TransformGroup) (burningBG[index].getChild(1))).getTransform(prevTrans);
        burningBG[index].removeAllChildren();
        burningBG[index] = null;

        burningBG[index] = new BranchGroup();

        burningBG[index].setCapability(Group.ALLOW_CHILDREN_EXTEND);
        burningBG[index].setCapability(Group.ALLOW_CHILDREN_READ);
        burningBG[index].setCapability(Group.ALLOW_CHILDREN_WRITE);
        burningBG[index].setCapability(Node.ALLOW_PICKABLE_READ);
        burningBG[index].setCapability(Node.ALLOW_PICKABLE_WRITE);
        burningBG[index].setCapability(BranchGroup.ALLOW_DETACH);
        burningBG[index].setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        burningBG[index].setCapability(Node.ALLOW_COLLIDABLE_READ);

        Transform3D trans = new Transform3D();

        trans.setScale(0.1);

        TransformGroup transforms = new TransformGroup(trans);

        transforms.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        transforms.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
        transforms.setCapability(BranchGroup.ALLOW_DETACH);

        // Transform3D pos = probePanel.getProbeBase().getCoordinate();

        // pos.setScale( 0.1 );
        transforms.setTransform(prevTrans);
        // translate = new Vector3f();

        burningBG[index].addChild(transforms);
        // sphereBG[burnIndex].addChild( burningBG[burnIndex] );

    }

    /**
     * Calculate the burning points total volume.
     *
     * @param  surface    SurfaceAttributes reference
     * @param  treatment  TreatmentInformation reference
     */
    public void calcTotalVolume(SurfaceAttributes surface, TreatmentInformation treatment) {
        int i;
        nVoxels = 0;
        diffVoxels = 0;

        BitSet surfaceMask = new BitSet();

        BitSet diffMask = new BitSet();

        surfaceMask = surface.getMask();

        // System.err.println("surface.surfaceMask = " + surface.surfaceMask.size());
        BitSet burnMask = new BitSet(surfaceMask.size());

        for (i = 0; i < treatment.getNumBurns(); i++) {

            // System.err.println("treatment.getBurn(i).burnMask = " + treatment.getBurn(i).burnMask.size());
            burnMask.or(treatment.getBurn(i).burnMask);
        }

        diffMask = (BitSet) (surfaceMask.clone());
        diffMask.xor(burnMask);
        System.err.println("diffMask = " + diffMask.size() + " burnMask = " + burnMask.size());

        for (i = 0; i < burnMask.size(); i++) {

            if (burnMask.get(i)) {
                nVoxels++;
            }

            if (diffMask.get(i)) {
                diffVoxels++;
            }

        }
        /*
         *  System.err.println("nVoxels = " + nVoxels + " diffVoxels = " + diffVoxels); ModelImage newImage = new
         * ModelImage( surfaceRender.getImageA().getType(),         surfaceRender.getImageA().getExtents(), "Mask
         * Image", surfaceRender.getUserInterface() ); try {     newImage.importData( 0, diffMask, true );
         * newImage.setFileInfo( surfaceRender.getImageA().getFileInfo() ); } catch ( IOException er ) {     return; }
         * new ViewJFrameImage( newImage, null, new Dimension( 610, 200 ) );
         */

        // System.err.println("nVoxels = " + nVoxels + " diffVoxels = " + diffVoxels);
        /*
         * for ( i = 0; i < maskBurnVector.size(); i++ ) {    maskBurn =
         * (MaskBurnAttribute)(maskBurnVector.elementAt(i));    if ( maskBurn.burnType == BurnBase.DEFAULTBURN ) {
         *  reGenerateSphereMesh( 5, maskBurn.center,  maskBurn.centerTransform);    } else {        if (
         * maskBurn.burnType == BurnBase.REGULARBURN ) {            reGenerateEllipsoidMesh( 5, maskBurn.tipLen, true,
         * maskBurn.center, maskBurn.centerTransform );        } else {            reGenerateEllipsoidMesh( 5,
         * maskBurn.tipLen, false, maskBurn.center, maskBurn.centerTransform );        }    }    calcVoxels(); }
         *
         * for ( i = 0; i < volumeMask.size(); i++ ) {    if ( volumeMask.get(i) ) {        nVoxels++;    } } //
         * surfacemask is null. this needs some more work before we can compare the target surface vs. the burns
         *
         * surfaceMaskVector = surfaceRender.getSurfaceDialog().getSurfaceMask(); for ( i = 0; i <
         * surfaceMaskVector.size(); i++ ) {    surfaceMask.or( (BitSet)(surfaceMaskVector.elementAt(i))); } int
         * diffVolume= 0; // burnMask.xor(surfaceMask); surfaceMask.xor(volumeMask); for ( i = 0; i <
         * surfaceMask.size(); i++ ) {    if (surfaceMask.get(i)) {        diffVolume++;    } }
         * System.err.println("diffVolume = " + diffVolume);
         */
    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        burnColor = null;
        sphereBG = null;
        spheres = null;
        pickCanvas = null;
        probePanel = null;
        burnRootParentBG = null;
        surfaceRender = null;
        translate = null;
        burningBG = null;
        textLabelBG = null;
        indexVector = null;
        m_kCenter = null;
        m_akSurfaceShape = null;
        m_aiMask = null;
        m_aiImage = null;

        m_akVertex = null;
        m_aiConnect = null;
        m_akAdjacent = null;
        m_kEMap = null;

        m_kRotate = null;
        m_afLength = null;

        volumeMask = null;
        maskBurnVector = null;
    }

    /**
     * Attach or detach burn labels.
     *
     * @param  flag  whether to show the labels
     */
    public void enableBurnLabels(boolean flag) {
        int index;

        for (int i = 0; i < indexVector.size(); i++) {
            index = ((Integer) (indexVector.elementAt(i))).intValue();

            if (flag) {

                if (textLabelBG[index] == null) {
                    buildText(index, " burn" + index, getTranslate());
                }

                sphereBG[index].addChild(textLabelBG[index]);
            } else {

                if (textLabelBG[index] != null) {
                    textLabelBG[index].detach();
                }
            }
        }

    }

    /**
     * Check whether the burning sphere is picked or not.
     *
     * @param   pickedShape  sphere burning shape.
     *
     * @return  int burning point index
     */
    public int findBurnPoint(Shape3D pickedShape) {
        BranchGroup burnRoot = burnRootParentBG;

        for (int j = 0; j < burnRoot.numChildren(); j++) {
            BranchGroup root = (BranchGroup) (burnRoot.getChild(j));
            BranchGroup meshRoot = (BranchGroup) (root.getChild(0));

            for (int i = 0; i < meshRoot.numChildren(); i++) {

                if (meshRoot.getChild(i) == pickedShape) {
                    return j;
                }
            }
        }

        return -1;
    }

    /**
     * Get the burnning point ( sphere packing or ellipsoid packing ) mask.
     *
     * @return  BitSet Burning point volume mask.
     */
    public BitSet getBurnMask() {
        return (BitSet) (m_aiMask.clone());
    }

    /**
     * Get the buring point surface color.
     *
     * @return  Color4f burning color.
     */
    public Color4f getColor() {
        return burnColor;
    }

    /**
     * Get the volume difference btw tumor surface and burning sphere packings.
     *
     * @return  float differenct volume in mm space.
     */
    public final float getDiffVolume() {
        return diffVoxels * m_fXDelta * m_fYDelta * m_fZDelta;
    }

    /**
     * Get the radius( semiX, semiY, semiZ).
     *
     * @return  Point3f radius
     */
    public Point3f getRadius() {
        return radiusPt;
    }

    /**
     * Get the current burning point brach group.
     *
     * @return  BranchGroup burning point branch group reference.
     */
    public BranchGroup getSphereBG() {
        return sphereBG[burnIndex];
    }

    /**
     * Get the burning point center coordinates. Called by the probe control panel to show the current burning point
     * center.
     *
     * @return  Vector3f burning point center coordinate.
     */
    public Vector3f getTranslate() {
        return translate;
    }

    /**
     * Get the total tumor voxels of the current treatment in mm^3.
     *
     * @return  float tumor volume in mm space.
     */
    public final float getVolume() {
        return nVoxels * m_fXDelta * m_fYDelta * m_fZDelta;
    }


    /**
     * Analyze the histogram of the 10-bit binned 3D MRI. The function computes a minimum threshold, a maximum
     * threshold, and a background threshold that are used in the image term of the surface evolution. A brightness
     * threshold is also computed that is used for determining the initial ellipsoid that approximates the brain
     * surface.
     */
    public void histogramAnalysis() {

        // compute histogram
        int[] aiHistogram = new int[1024];

        Arrays.fill(aiHistogram, 0);

        int i;
        int j;

        for (i = 0; i < m_iQuantity; i++) {
            aiHistogram[m_aiImage[i]]++;
        }

        // Eliminate a large chunk of background.  The four parameters below
        // were selected based on empirical studies.
        double dMinFactor = 0.03;
        double dMaxFactor = 0.98;
        double dBrightFactor = 0.95;

        int iMax = 64;
        int iMinCutoff = (int) (dMinFactor * m_iQuantity);
        int iMaxCutoff = (int) (dMaxFactor * m_iQuantity);

        // Find background - i.e. the value with the most counts
        float maxCount = -1;
        int maxCountIndex = 0;

        for (j = 0; j < iMax; j++) {

            if (aiHistogram[j] > maxCount) {
                maxCount = aiHistogram[j];
                maxCountIndex = j;
            }
        }

        // maxCountIndex = iMax;
        int iAccum = 0;

        if (maxCountIndex == iMax) {

            // unable to find background from above - use cummalitive histogram method
            for (i = 0; i < 64; i++) {
                iAccum += aiHistogram[i];

                if (iAccum <= iMaxCutoff) {
                    m_iMaxThreshold = i;
                }

                if (iAccum <= iMinCutoff) {
                    m_iMinThreshold = i;
                }
            }

            m_iBackThreshold = Math.round((0.9f * m_iMinThreshold) + (0.1f * m_iMaxThreshold));
        } else {
            m_iBackThreshold = maxCountIndex + 1;
        }

        int iReducedQuantity = m_iQuantity;

        for (j = 0; j <= maxCountIndex; j++) {
            iReducedQuantity -= aiHistogram[j];
        }

        // compute brightness thresholds
        iAccum = 0;

        int iBrightCutoff = (int) (dBrightFactor * iReducedQuantity);

        for (i = m_iBackThreshold; i < 1024; i++) {
            iAccum += aiHistogram[i];

            if (iBrightCutoff >= iAccum) {
                m_iBrightThreshold = i;
            } // Used to estimate ellipsoid !
        }

        if (m_iBackThreshold == 0) {
            m_iBackThreshold = 1;
            m_iMinThreshold = 0;
        } else if (m_iBackThreshold == 1) {
            m_iMinThreshold = 0;
        } else {
            m_iMinThreshold = (int) Math.floor(0.5f * m_iBackThreshold);
        }

        if (m_iMinThreshold == m_iBackThreshold) {
            m_iBackThreshold++;
        }
    }

    /**
     * Start burning. Show the burning sphere and probing path.
     *
     * @param  tipLen  the length of the probe tip (cm)
     * @param  time    the time spent burning
     * @param  _index  index of the new burn
     */
    public void startBurn(float tipLen, float time, int _index) {

        // when tipLen = 3 (default) then make radius = .05, like it was before the switch to using tip length
        float sphereRadius = tipLen / 60f;

        burnIndex = _index;
        volumeSpaceRadius = sphereRadius;
        indexVector.add(new Integer(_index));
        burnType = BurnBase.DEFAULTBURN;

        burnColor = new Color4f(Color.red);

        Appearance app = new Appearance();
        Material mat = new Material(emissiveColor, emissiveColor, red, sepcualarColor, 50.0f);

        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);
        app.setCapability(Appearance.ALLOW_MATERIAL_READ);
        app.setCapability(Appearance.ALLOW_MATERIAL_WRITE);
        app.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ);
        app.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);
        app.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
        app.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);

        TransparencyAttributes tap = new TransparencyAttributes();

        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
        tap.setTransparencyMode(TransparencyAttributes.BLENDED);
        tap.setTransparency(0.5f);
        app.setTransparencyAttributes(tap);

        // No back-face culling.  Supports double-sided meshes which can
        // regularly occur for level surfaces (open surfaces).
        PolygonAttributes kPAttr = new PolygonAttributes();

        kPAttr.setCapability(PolygonAttributes.ALLOW_CULL_FACE_WRITE);
        kPAttr.setCapability(PolygonAttributes.ALLOW_CULL_FACE_READ);
        kPAttr.setCullFace(PolygonAttributes.CULL_BACK);

        // kPAttr.setPolygonMode(mode);
        // kPAttr.setPolygonMode( PolygonAttributes.POLYGON_LINE );
        app.setPolygonAttributes(kPAttr);

        spheres = new Sphere(sphereRadius, app);
        spheres.setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
        spheres.setCapability(Sphere.ALLOW_CHILDREN_EXTEND);
        spheres.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        spheres.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        spheres.getShape().setAppearanceOverrideEnable(true);
        spheres.getShape().setCapability(Geometry.ALLOW_INTERSECT);

        try {
            PickTool.setCapabilities(spheres.getShape(), PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        histogramAnalysis();
        estimateEllipsoid();

        // generate the mesh in volume space to add into the scene graph
        generateSphereMesh(5, false);

        ModelTriangleMesh[] kMesh = new ModelTriangleMesh[1];

        kMesh[0] = new ModelTriangleMesh(m_akVertex, m_aiConnect);

        for (int j = 0; j < kMesh.length; j++) {
            kMesh[j].computeNormals();
        }

        // calculate the volume in the image space
        generateSphereMesh(5, true);

        // the binary mask for voxels inside the brain surface
        m_aiMask = new BitSet(m_iQuantity);
        getInsideVoxels();

        BranchGroup root = createSurface(kMesh, burnColor, PolygonAttributes.POLYGON_FILL);

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

        sphereBG[burnIndex] = new BranchGroup();

        sphereBG[burnIndex].setCapability(Group.ALLOW_CHILDREN_EXTEND);
        sphereBG[burnIndex].setCapability(Group.ALLOW_CHILDREN_READ);
        sphereBG[burnIndex].setCapability(Group.ALLOW_CHILDREN_WRITE);
        sphereBG[burnIndex].setCapability(Node.ALLOW_PICKABLE_READ);
        sphereBG[burnIndex].setCapability(Node.ALLOW_PICKABLE_WRITE);
        sphereBG[burnIndex].setCapability(BranchGroup.ALLOW_DETACH);
        sphereBG[burnIndex].setCapability(Node.ALLOW_COLLIDABLE_WRITE);
        sphereBG[burnIndex].setCapability(Node.ALLOW_COLLIDABLE_READ);

        sphereBG[burnIndex].addChild(root);

        Transform3D pos = probePanel.getProbeBase().getCoordinate();

        translate = new Vector3f();

        pos.get(translate);

        sphereBG[burnIndex].addChild(markProbe());
        sphereBG[burnIndex].addChild(buildEntryPoint());

        burnRootParentBG.addChild(sphereBG[burnIndex]);
        buildBurnSceneGraph(sphereRadius, time);

        if (textLabelBG[burnIndex] == null) {
            buildText(burnIndex, " burn" + burnIndex, getTranslate());
        }

        if (probePanel.getBurnLabelFlag()) {
            sphereBG[burnIndex].addChild(textLabelBG[burnIndex]);
        }
    }

    /**
     * Build the entry point image scene graph structure.
     *
     * @return  BranchGroup the entry point root branch group.
     */
    protected BranchGroup buildEntryPoint() {

        // Build entry point
        BranchGroup greenBG = new BranchGroup();

        greenBG.setCapability(BranchGroup.ALLOW_DETACH);
        greenBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        greenBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        greenBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        Point3f entryPointVector = probePanel.getEntryPoint();

        Material mat;
        Appearance app = new Appearance();

        mat = new Material(green, green, green, green, 80f);
        mat.setCapability(Material.ALLOW_COMPONENT_READ);
        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);

        Sphere greenSphere = new Sphere(.01f, app);

        greenSphere.setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
        greenSphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        greenSphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        greenSphere.getShape().setAppearanceOverrideEnable(true);
        greenSphere.getShape().setCapability(Geometry.ALLOW_INTERSECT);

        Transform3D trans = new Transform3D();

        trans.setTranslation(new Vector3f(entryPointVector.x, entryPointVector.y, entryPointVector.z));

        TransformGroup greenSphereTG = new TransformGroup(trans);

        greenSphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        greenSphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        greenSphereTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        greenSphereTG.addChild(greenSphere);
        greenBG.addChild(greenSphereTG);

        return greenBG;
    }

    /**
     * Attach some 3D text to a BranchGroup or TransformGroup.
     *
     * @param  index  the group to attach the text to
     * @param  str    the text to show
     * @param  pos    where to place the text
     */
    protected void buildText(int index, String str, Vector3f pos) {
        textLabelBG[index] = new BranchGroup();
        textLabelBG[index].setCapability(Group.ALLOW_CHILDREN_EXTEND);
        textLabelBG[index].setCapability(Group.ALLOW_CHILDREN_READ);
        textLabelBG[index].setCapability(Group.ALLOW_CHILDREN_WRITE);
        textLabelBG[index].setCapability(BranchGroup.ALLOW_DETACH);

        TransformGroup textTG = new TransformGroup();

        textTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        textTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        textTG.setCapability(Group.ALLOW_CHILDREN_READ);
        textLabelBG[index].addChild(textTG);

        Font3D f3d = new Font3D(new Font("Serif", Font.PLAIN, 1), new FontExtrusion());
        Text3D txt = new Text3D(f3d, str);

        txt.setAlignment(Text3D.ALIGN_FIRST);

        Shape3D sh = new Shape3D();
        Appearance app = new Appearance();
        Material mm = new Material();

        mm.setLightingEnable(true);
        app.setMaterial(mm);
        sh.setGeometry(txt);
        sh.setAppearance(app);
        textTG.addChild(sh);

        Transform3D textTransform = new Transform3D();

        textTransform.setScale(0.1);
        textTransform.setTranslation(new Vector3f(pos.x, pos.y, pos.z));
        textTG.setTransform(textTransform);
    }

    /**
     * Identify all voxels that are inside or on the mesh that represents the brain surface. The surface voxels are
     * constructed by rasterizing the triangles of the mesh in 3D. The centroid of these voxels is used as a seed point
     * for a flood fill of the region enclosed by the surface.
     */
    protected void calcVoxels() {
        m_aiMask.clear();

        int i, iX, iY, iZ;
        //int[] extents = surfaceRender.getImageA().getExtents();
        //int xDim = extents[0];
        //int yDim = extents[1];
        //int zDim = extents[2];

        for (int iT = 0; iT < m_iTQuantity; iT++) {

            // get the vertices of the triangle
            Point3f kV0 = m_akVertex[m_aiConnect[3 * iT]];
            Point3f kV1 = m_akVertex[m_aiConnect[(3 * iT) + 1]];
            Point3f kV2 = m_akVertex[m_aiConnect[(3 * iT) + 2]];

            /*
             * kV0.z = (float) zDim - 1f - kV0.z; kV1.z = (float) zDim - 1f - kV1.z; kV2.z = (float) zDim - 1f - kV2.z;
             *
             * kV0.y = (float) yDim - 1f - kV0.y; kV1.y = (float) yDim - 1f - kV1.y; kV2.y = (float) yDim - 1f - kV2.y;
             */

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
            int iXMin = (int) fXMin, iXMax = (int) fXMax;
            int iYMin = (int) fYMin, iYMax = (int) fYMax;
            int iZMin = (int) fZMin, iZMax = (int) fZMax;
            int ptr;
            int end = m_aiMask.size();

            for (iY = iYMin; iY <= iYMax; iY++) {

                for (iZ = iZMin; iZ <= iZMax; iZ++) {
                    iX = getIntersectX(kV0, kV1, kV2, iY, iZ);

                    if (iX != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                        }
                    }
                }
            }

            for (iX = iXMin; iX <= iXMax; iX++) {

                for (iZ = iZMin; iZ <= iZMax; iZ++) {
                    iY = getIntersectY(kV0, kV1, kV2, iX, iZ);

                    if (iY != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                        }
                    }
                }
            }

            for (iX = iXMin; iX <= iXMax; iX++) {

                for (iY = iYMin; iY <= iYMax; iY++) {
                    iZ = getIntersectZ(kV0, kV1, kV2, iX, iY);

                    if (iZ != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                        }
                    }
                }
            }
        }

        // m_kCenter.y = yDim - 1 - m_kCenter.y;
        // m_kCenter.z = zDim - 1 - m_kCenter.z;
        floodFill((int) m_kCenter.x, (int) m_kCenter.y, (int) m_kCenter.z);
        volumeMask.or(m_aiMask);
    }

    /**
     * Creates a surface in the scene graph from an array of triangle meshes.
     *
     * @param   meshes  Triangle meshes that make up surface
     * @param   color   Color of surface
     * @param   mode    DOCUMENT ME!
     *
     * @return  Parent node of surface.
     */
    protected BranchGroup createSurface(ModelTriangleMesh[] meshes, Color4f color, int mode) {

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
        appearance.setCapability(Appearance.ALLOW_MATERIAL_WRITE);
        appearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
        appearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);
        appearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);
        appearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ);
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
        m_akSurfaceShape = new Shape3D[meshes.length];

        for (int i = 0; i < meshes.length; i++) {
            m_akSurfaceShape[i] = new Shape3D(meshes[i], appearance);

            m_akSurfaceShape[i].setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            m_akSurfaceShape[i].setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            m_akSurfaceShape[i].setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
            m_akSurfaceShape[i].setCapability(Shape3D.ALLOW_GEOMETRY_READ);
            m_akSurfaceShape[i].setCapability(Geometry.ALLOW_INTERSECT);

            try {

                // pickCanvas.setCapabilities( shape, PickTool.INTERSECT_FULL );
                PickTool.setCapabilities(m_akSurfaceShape[i], PickCanvas.INTERSECT_FULL);
            } catch (RestrictedAccessException error) { }

            root.addChild(m_akSurfaceShape[i]);
        }

        // m_kSurface = meshes[0];
        return root;
    }

    /**
     * Approximate the brain surface by an ellipsoid. The approximation is based on locating all voxels of intensity
     * larger than a brightness threshold and that are part of the upper-half of the head. The idea is that the scalp
     * voxels in the upper-half form lie approximately on an ellipsoidal surface.<br>
     * <br>
     *
     * <p>NOTE. The assumption is that the traversal from bottom to top of head is in the y-direction of the 3D image.
     * It does not matter if the top of the head has y-values smaller/larger than those for the bottom of the head. If
     * this assumption is not met, the image should be permuted OR this code must be modified to attempt to recognize
     * the orientation of the head</p>
     */
    protected void estimateEllipsoid() {

        // center-orient-length format for ellipsoid
        m_kRotate = new Matrix3f();
        m_afLength = new float[3];

        m_kRotate.setIdentity();

        m_iXBound = surfaceRender.getImageA().getExtents()[0];
        m_iYBound = surfaceRender.getImageA().getExtents()[1];
        m_iZBound = surfaceRender.getImageA().getExtents()[2];
        m_iQuantity = m_iXBound * m_iYBound * m_iZBound;
        m_fXDelta = surfaceRender.getImageA().getFileInfo()[0].getResolutions()[0];
        m_fYDelta = surfaceRender.getImageA().getFileInfo()[0].getResolutions()[1];
        m_fZDelta = surfaceRender.getImageA().getFileInfo()[0].getResolutions()[2];

        // The histogram analysis is best performed by binning into 8-bit
        // data.  The image term in the evolution scheme depends only on a
        // few intensity threshold values, so the method appears not to be
        // sensitive to number of bits in the image data.
        m_aiImage = new int[m_iQuantity];

        float fMax, fMin;

        fMin = (float) surfaceRender.getImageA().getMin();
        fMax = (float) surfaceRender.getImageA().getMax();

        // Remap image data to 0 - 1023
        float fMult = 1023.0f / (fMax - fMin);

        for (int i = 0; i < m_iQuantity; i++) {
            m_aiImage[i] = (int) (fMult * (surfaceRender.getImageA().getFloat(i) - fMin));
        }
    }

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
            m_aiMask.set(getIndex(iX, iY, iZ));

            int iXp1 = iX + 1;

            if ((iXp1 < m_iXBound) && !m_aiMask.get(getIndex(iXp1, iY, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXp1;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iXm1 = iX - 1;

            if ((0 <= iXm1) && !m_aiMask.get(getIndex(iXm1, iY, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXm1;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iYp1 = iY + 1;

            if ((iYp1 < m_iYBound) && !m_aiMask.get(getIndex(iX, iYp1, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYp1;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iYm1 = iY - 1;

            if ((0 <= iYm1) && !m_aiMask.get(getIndex(iX, iYm1, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYm1;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iZp1 = iZ + 1;

            if ((iZp1 < m_iZBound) && !m_aiMask.get(getIndex(iX, iY, iZp1))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZp1;

                continue;
            }

            int iZm1 = iZ - 1;

            if ((0 <= iZm1) && !m_aiMask.get(getIndex(iX, iY, iZm1))) {

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
     * Tessellate a unit sphere centered at the origin. Start with an octahedron and subdivide. The final mesh is then
     * affinely mapped to the initial ellipsoid produced by estimateEllipsoid(). The subdivision scheme is described in
     * BrainExtraction.pdf.
     *
     * @param  burnType       the type of the burn
     * @param  tipLen         the length of the probe tip, in cm
     * @param  iSubdivisions  the number of levels to subdivide the ellipsoid
     * @param  isImageSpace   whether to get the mesh in image space
     */
    protected void generateEllipsoidMesh(int burnType, float tipLen, int iSubdivisions, boolean isImageSpace) {

        // Compute the number of vertices, edges, and triangles for an
        // octahedron subdivided to the specified level.  The recursions are
        // V1 = V0 + E0
        // E1 = 2*E0 + 3*T0
        // T1 = 4*T0
        System.err.println("tipLen = " + tipLen);

        float semiAxisZ = 0f;
        float semiAxisX = 0f;

        m_iVQuantity = 6;
        m_iEQuantity = 12;
        m_iTQuantity = 8;

        int iStep;

        m_kRotate = new Matrix3f();
        m_kRotate.setIdentity();

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {
            m_iVQuantity = m_iVQuantity + m_iEQuantity;
            m_iEQuantity = (2 * m_iEQuantity) + (3 * m_iTQuantity);
            m_iTQuantity = 4 * m_iTQuantity;
        }

        Transform3D pos = probePanel.getProbeBase().getCoordinate();

        // coordinates of the burn center, where the cross-hairs on
        // the probe are positioned.  Is volume (Java 3D) space.
        Vector3f centerTrans = new Vector3f();

        pos.get(centerTrans);
        // -1 <= centerTrans.x, .y, .z <= 1

        m_kCenter = new Point3f(0.0f, 0.0f, 0.0f);

        int[] extents = surfaceRender.getImageA().getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];

        float[] resols = surfaceRender.getImageA().getFileInfo()[0].getResolutions();

        /**
         * Volume space -- two pts distance:  Pt ( 0, 0, 0 ) and Pt ( 1, 1, 1) = squreRoot(3) What's the pts distance in
         * the image space.
         */
        float x1 = -1, y1 = 1, z1 = 1, x2 = 1, y2 = 1, z2 = 1;

        if (tipLen == 2f) {
            semiAxisX = 20f / 2f;
            semiAxisZ = 29f / 2f;
        } else if (tipLen == 2.5f) {
            semiAxisX = 32f / 2f;
            semiAxisZ = 46f / 2f;
        } else if (tipLen == 3f) {
            semiAxisX = 22f / 2f;
            semiAxisZ = 39f / 2f;
        } else {
            MipavUtil.displayError("Illegal tip length value for the CoolTip probe.");

            return;
        }

        if (!isImageSpace) {
            x1 = 0;
            y1 = 1;
            z1 = 1;
            x2 = semiAxisX;
            y2 = 1;
            z2 = 1;

            x1 = ((x1 / ((xDim - 1) * resols[0])) * 2f) - 1f;
            y1 = ((y1 / ((yDim - 1) * resols[1])) * 2f) - 1f;
            z1 = ((z1 / ((zDim - 1) * resols[2])) * 2f) - 1f;

            x2 = ((x2 / ((xDim - 1) * resols[0])) * 2f) - 1f;
            y2 = ((y2 / ((yDim - 1) * resols[1])) * 2f) - 1f;
            z2 = ((z2 / ((zDim - 1) * resols[2])) * 2f) - 1f;

            semiAxisX = (float) Math.sqrt((double) (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) +
                                                    ((z2 - z1) * (z2 - z1))));

            x1 = 0;
            y1 = 1;
            z1 = 1;
            x2 = semiAxisZ;
            y2 = 1;
            z2 = 1;

            x1 = ((x1 / ((xDim - 1) * resols[0])) * 2f) - 1f;
            y1 = ((y1 / ((yDim - 1) * resols[1])) * 2f) - 1f;
            z1 = ((z1 / ((zDim - 1) * resols[2])) * 2f) - 1f;

            x2 = ((x2 / ((xDim - 1) * resols[0])) * 2f) - 1f;
            y2 = ((y2 / ((yDim - 1) * resols[1])) * 2f) - 1f;
            z2 = ((z2 / ((zDim - 1) * resols[2])) * 2f) - 1f;
            semiAxisZ = (float) Math.sqrt((double) (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) +
                                                    ((z2 - z1) * (z2 - z1))));
        }

        System.out.println("semiAxisX = " + semiAxisX + " semiAxisZ = " + semiAxisZ);

        if (isImageSpace) {

            // m_afLength[0], [1], and [2] are in mm space
            m_afLength[0] = semiAxisX;
            m_afLength[1] = semiAxisX;
            m_afLength[2] = semiAxisZ;

            radiusPt.x = semiAxisX;
            radiusPt.y = semiAxisX;
            radiusPt.z = semiAxisZ;

            // Transform the center point from the volume space into the image space.
            float xBox, yBox, zBox, maxBox;
            float m_fX0;
            float m_fY0;
            float m_fZ0;
            float m_fX1;
            float m_fY1;
            float m_fZ1;

            // Box is really the FOV for each dimension in mm space
            xBox = (xDim - 1) * resols[0];
            yBox = (yDim - 1) * resols[1];
            zBox = (zDim - 1) * resols[2];
            maxBox = xBox;

            if (yBox > maxBox) {
                maxBox = yBox;
            }

            if (zBox > maxBox) {
                maxBox = zBox;
            }

            // Normalize the size
            // xBox range between 0 - 1.
            xBox = xBox / maxBox;
            yBox = yBox / maxBox;
            zBox = zBox / maxBox;

            // m_fX0, Y0, Z0 are the coordinates of one corner of the normalized
            // FOV box, and m_fX1, Y1, Z1 are the coordinates of the diagonal
            // corner of the normalized box
            m_fX0 = -xBox;
            m_fY0 = -yBox;
            m_fX1 = xBox;
            m_fY1 = yBox;

            m_fZ0 = -zBox;
            m_fZ1 = zBox;

            // this should never be true.  Why is it here?
            if (zBox > maxBox) {
                m_fZ0 = -1f;
                m_fZ1 = 1f;
            }

            // transform from volume space to image space
            // using the bounding box approach
            // m_kCenter is the burn center in pixel coordinates
            m_kCenter.x = ((centerTrans.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
            m_kCenter.y = ((centerTrans.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
            m_kCenter.z = ((centerTrans.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

            // flip the z and y coordinate
            m_kCenter.z = zDim - 1 - m_kCenter.z;
            m_kCenter.y = yDim - 1 - m_kCenter.y;

            maskBurnVector.add(new MaskBurnAttribute(new Point3f(centerTrans.x, centerTrans.y, centerTrans.z),
                                                     new Transform3D(pos), burnType, tipLen));
        } else { // in volume space
            m_afLength[0] = semiAxisX;
            m_afLength[1] = semiAxisX;
            m_afLength[2] = semiAxisZ;

            m_kCenter.x = centerTrans.x;
            m_kCenter.y = centerTrans.y;
            m_kCenter.z = centerTrans.z;
        }

        // See BrainExtraction.pdf for a description of the subdivision
        // algorithm.  The use of the HashMap m_kEMap is to store midpoint
        // information for edges so that triangles sharing an edge know what
        // the new vertices are for replacing themselves with subtriangles.
        m_akVertex = new Point3f[m_iVQuantity];
        m_aiConnect = new int[3 * m_iTQuantity];
        m_akAdjacent = new UnorderedSetInt[m_iVQuantity];

        int i;

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i] = new Point3f();
            m_akAdjacent[i] = new UnorderedSetInt(6, 1);
        }

        m_akVertex[0].set(+1.0f, 0.0f, 0.0f);
        m_akVertex[1].set(-1.0f, 0.0f, 0.0f);
        m_akVertex[2].set(0.0f, +1.0f, 0.0f);
        m_akVertex[3].set(0.0f, -1.0f, 0.0f);
        m_akVertex[4].set(0.0f, 0.0f, +1.0f);
        m_akVertex[5].set(0.0f, 0.0f, -1.0f);

        m_aiConnect[0] = 4;
        m_aiConnect[1] = 0;
        m_aiConnect[2] = 2;
        m_aiConnect[3] = 4;
        m_aiConnect[4] = 2;
        m_aiConnect[5] = 1;
        m_aiConnect[6] = 4;
        m_aiConnect[7] = 1;
        m_aiConnect[8] = 3;
        m_aiConnect[9] = 4;
        m_aiConnect[10] = 3;
        m_aiConnect[11] = 0;
        m_aiConnect[12] = 5;
        m_aiConnect[13] = 2;
        m_aiConnect[14] = 0;
        m_aiConnect[15] = 5;
        m_aiConnect[16] = 1;
        m_aiConnect[17] = 2;
        m_aiConnect[18] = 5;
        m_aiConnect[19] = 3;
        m_aiConnect[20] = 1;
        m_aiConnect[21] = 5;
        m_aiConnect[22] = 0;
        m_aiConnect[23] = 3;

        m_kEMap = new HashMap<Edge,Integer>();

        Integer kInvalid = new Integer(-1);

        m_kEMap.put(new Edge(0, 4), kInvalid);
        m_kEMap.put(new Edge(1, 4), kInvalid);
        m_kEMap.put(new Edge(2, 4), kInvalid);
        m_kEMap.put(new Edge(3, 4), kInvalid);
        m_kEMap.put(new Edge(0, 5), kInvalid);
        m_kEMap.put(new Edge(1, 5), kInvalid);
        m_kEMap.put(new Edge(2, 5), kInvalid);
        m_kEMap.put(new Edge(3, 5), kInvalid);
        m_kEMap.put(new Edge(0, 2), kInvalid);
        m_kEMap.put(new Edge(2, 1), kInvalid);
        m_kEMap.put(new Edge(1, 3), kInvalid);
        m_kEMap.put(new Edge(3, 0), kInvalid);

        int iPNext = 6, iTSubQuantity = 8, iCNext = 24;
        int i0, i1, i2, iP0, iP1, iP2, iT;

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {

            // generate midpoints of edges
            Iterator kEIter = m_kEMap.entrySet().iterator();
            Map.Entry kEntry = null;

            while (kEIter.hasNext()) {
                kEntry = (Map.Entry) kEIter.next();

                Edge kE = (Edge) kEntry.getKey();
                Point3f kP0 = m_akVertex[kE.m_i0];
                Point3f kP1 = m_akVertex[kE.m_i1];
                Point3f kPMid = m_akVertex[iPNext];

                kPMid.add(kP0, kP1);

                float fInvLen = 1.0f /
                                    (float) Math.sqrt((kPMid.x * kPMid.x) + (kPMid.y * kPMid.y) + (kPMid.z * kPMid.z));

                kPMid.scale(fInvLen);
                kEntry.setValue(new Integer(iPNext));
                iPNext++;
            }

            // replace triangle by four subtriangles
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];

                Edge kE01 = new Edge(iP0, iP1);
                Edge kE12 = new Edge(iP1, iP2);
                Edge kE20 = new Edge(iP2, iP0);
                int iM01 = ((Integer) m_kEMap.get(kE01)).intValue();
                int iM12 = ((Integer) m_kEMap.get(kE12)).intValue();
                int iM20 = ((Integer) m_kEMap.get(kE20)).intValue();

                // add new edges

                // replace current triangle by middle triangle
                m_aiConnect[i0] = iM01;
                m_aiConnect[i1] = iM12;
                m_aiConnect[i2] = iM20;

                // append remaining subtriangles
                m_aiConnect[iCNext++] = iP0;
                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iM20;

                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iP1;
                m_aiConnect[iCNext++] = iM12;

                m_aiConnect[iCNext++] = iM20;
                m_aiConnect[iCNext++] = iM12;
                m_aiConnect[iCNext++] = iP2;
            }

            iTSubQuantity *= 4;

            // remove old edges
            m_kEMap.clear();

            // add new edges
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];
                m_kEMap.put(new Edge(iP0, iP1), kInvalid);
                m_kEMap.put(new Edge(iP1, iP2), kInvalid);
                m_kEMap.put(new Edge(iP2, iP0), kInvalid);
            }
        }

        // generate vertex adjacency
        for (iT = 0; iT < m_iTQuantity; iT++) {
            iP0 = m_aiConnect[3 * iT];
            iP1 = m_aiConnect[(3 * iT) + 1];
            iP2 = m_aiConnect[(3 * iT) + 2];

            m_akAdjacent[iP0].insert(iP1);
            m_akAdjacent[iP0].insert(iP2);
            m_akAdjacent[iP1].insert(iP0);
            m_akAdjacent[iP1].insert(iP2);
            m_akAdjacent[iP2].insert(iP0);
            m_akAdjacent[iP2].insert(iP1);
        }

        Transform3D probeTform = probePanel.getProbeBase().getProbeTransform();
        Vector3f rotAngle = new Vector3f();
        Transform3D trans = new Transform3D();
        trans.setIdentity();

        Matrix3f rotationMatrix = new Matrix3f();

        if (probeTform != null) {

            // making sure that rotation is along the origin.
            rotAngle = getRotAngle(probeTform);
            trans.rotX(rotAngle.x);
            trans.rotY(rotAngle.y);
            trans.rotZ(rotAngle.z);
            trans.getRotationScale(rotationMatrix);
            System.err.println("rotationMatrix = " + rotationMatrix.toString());

            // might be null if the probe hasn't been moved..
            probeTform.getRotationScale(m_kRotate);
            System.err.println("m_kRotate = " + m_kRotate.toString());
        }

        System.out.println("m_afLength[0] = " + m_afLength[0] + " m_afLength[1] = " + m_afLength[1] +
                           " m_afLength[2] = " + m_afLength[2]);

        // Notes
        // m_afLength is in pixel coordinates if we are in imageSpace
        for (i = 0; i < m_iVQuantity; i++) {

            // scale the sphere vertices to make ellipsoid vertices in pixel coords if we are in imageSpace
            m_akVertex[i].x *= m_afLength[0];
            m_akVertex[i].y *= m_afLength[1];
            m_akVertex[i].z *= m_afLength[2];

            // Transform for equal resolution units
            // rotate the elliposoid
            m_kRotate.transform(m_akVertex[i]);

            // If image space, rotate the ellipsoid from the volume space to image space.
            // Coincide the volume space coordinate with the image space coordinate.
            if (isImageSpace) {
                rotationMatrix.rotX((float) Math.PI);
                rotationMatrix.transform(m_akVertex[i]);
            }

            if (isImageSpace) {

                // convert ellipsoid into pixel coordinates
                m_akVertex[i].x /= resols[0];
                m_akVertex[i].y /= resols[1];
                m_akVertex[i].z /= resols[2];
            }

            // translate all the points to the center of mass
            m_akVertex[i].add(m_kCenter);
        }
    }

    /**
     * Tessellate a unit sphere centered at the origin. Start with an octahedron and subdivide. The final mesh is then
     * affinely mapped to the initial ellipsoid produced by estimateEllipsoid(). The subdivision scheme is described in
     * BrainExtraction.pdf.
     *
     * @param  iSubdivisions  the number of levels to subdivide the ellipsoid
     * @param  isImageSpace   DOCUMENT ME!
     */
    protected void generateSphereMesh(int iSubdivisions, boolean isImageSpace) {
        float ratio;

        // Compute the number of vertices, edges, and triangles for an
        // octahedron subdivided to the specified level.  The recursions are
        // V1 = V0 + E0
        // E1 = 2*E0 + 3*T0
        // T1 = 4*T0
        m_iVQuantity = 6;
        m_iEQuantity = 12;
        m_iTQuantity = 8;

        int iStep;

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {
            m_iVQuantity = m_iVQuantity + m_iEQuantity;
            m_iEQuantity = (2 * m_iEQuantity) + (3 * m_iTQuantity);
            m_iTQuantity = 4 * m_iTQuantity;
        }

        Transform3D pos = probePanel.getProbeBase().getCoordinate();
        Vector3f centerTrans = new Vector3f();

        pos.get(centerTrans);

        m_kCenter = new Point3f(0.0f, 0.0f, 0.0f);

        int[] extents = surfaceRender.getImageA().getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];

        float[] resols = surfaceRender.getImageA().getFileInfo()[0].getResolutions();

        /**
         * Volume space -- two pts distance:  Pt ( 0, 0, 0 ) and Pt ( 1, 1, 1) = squreRoot(3) What's the pts distance in
         * the image space.
         */
        float x1 = 1, y1 = 1, z1 = 1, x2 = 10, y2 = 10, z2 = 10;
        double volSpaceLength = Math.sqrt((double) (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) +
                                                    ((z2 - z1) * (z2 - z1))));

        // compute C0, C1, C2, and max{C0, C1, C2}
        // these are the physical space voxel dimensions
        float fC0 = xDim * m_fXDelta;
        float fC1 = yDim * m_fYDelta;
        float fC2 = zDim * m_fZDelta;
        float fMax;

        // fMax is the largest of the three physical space dimensions
        fMax = (fC0 < fC1) ? fC1 : fC0;
        fMax = (fMax < fC2) ? fC2 : fMax;

        // map into *physical* space
        // max{C0,C1,C2}*xk + Ck
        // yk =    ---------------------
        // 2
        x1 = ((x1 * fMax) + fC0) / 2;
        y1 = ((y1 * fMax) + fC1) / 2;
        z1 = ((z1 * fMax) + fC2) / 2;

        // now want to get into image space, so divide by resolutions
        x1 = x1 / m_fXDelta;
        y1 = y1 / m_fYDelta;
        z1 = z1 / m_fZDelta;

        x2 = ((x2 * fMax) + fC0) / 2;
        y2 = ((y2 * fMax) + fC1) / 2;
        z2 = ((z2 * fMax) + fC2) / 2;

        // now want to get into image space, so divide by resolutions
        x2 = x2 / m_fXDelta;
        y2 = y2 / m_fYDelta;
        z2 = z2 / m_fZDelta;

        double imgSpaceLength = Math.sqrt((double) (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) +
                                                    ((z2 - z1) * (z2 - z1))));

        ratio = (float) (volSpaceLength / imgSpaceLength);

        if (isImageSpace) {

            radius = volumeSpaceRadius / ratio;

            m_afLength[0] = radius; // * 0.4f;
            m_afLength[1] = radius; // * 0.4f;
            m_afLength[2] = radius * resols[0] / resols[2]; // ( radius * 0.4f ) * ( m_fXDelta / m_fZDelta );

            radiusPt.x = radius;
            radiusPt.y = radius;
            radiusPt.z = radius * resols[0] / resols[2];
            ;

            m_kCenter.x = ((centerTrans.x + 1) / 2) * (xDim - 1);
            m_kCenter.y = ((2 - (centerTrans.y + 1)) / 2) * (yDim - 1);

            // m_kCenter.z = ( ( centerTrans.z + 1 ) / 2 ) * ( ( zDim - 1 ) );
            m_kCenter.z = ((2 - (centerTrans.z + 1)) / 2) * (zDim - 1);

            maskBurnVector.add(new MaskBurnAttribute(new Point3f(centerTrans.x, centerTrans.y, centerTrans.z),
                                                     new Transform3D(pos), burnType, 0));
        } else {
            radius = volumeSpaceRadius;
            m_afLength[0] = radius; // * 0.4f;
            m_afLength[1] = radius; // * 0.4f;
            m_afLength[2] = radius; // ( radius * 0.4f ) * ( m_fXDelta / m_fZDelta );

            m_kCenter.x = centerTrans.x;
            m_kCenter.y = centerTrans.y;
            m_kCenter.z = centerTrans.z;
        }

        // System.err.println("m_kCenter.x = " + m_kCenter.x + " m_kCenter.y = " + m_kCenter.y + " m_kCenter.z = " +
        // m_kCenter.z);

        // See BrainExtraction.pdf for a description of the subdivision
        // algorithm.  The use of the HashMap m_kEMap is to store midpoint
        // information for edges so that triangles sharing an edge know what
        // the new vertices are for replacing themselves with subtriangles.
        m_akVertex = new Point3f[m_iVQuantity];
        m_aiConnect = new int[3 * m_iTQuantity];
        m_akAdjacent = new UnorderedSetInt[m_iVQuantity];

        int i;

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i] = new Point3f();
            m_akAdjacent[i] = new UnorderedSetInt(6, 1);
        }

        m_akVertex[0].set(+1.0f, 0.0f, 0.0f);
        m_akVertex[1].set(-1.0f, 0.0f, 0.0f);
        m_akVertex[2].set(0.0f, +1.0f, 0.0f);
        m_akVertex[3].set(0.0f, -1.0f, 0.0f);
        m_akVertex[4].set(0.0f, 0.0f, +1.0f);
        m_akVertex[5].set(0.0f, 0.0f, -1.0f);

        m_aiConnect[0] = 4;
        m_aiConnect[1] = 0;
        m_aiConnect[2] = 2;
        m_aiConnect[3] = 4;
        m_aiConnect[4] = 2;
        m_aiConnect[5] = 1;
        m_aiConnect[6] = 4;
        m_aiConnect[7] = 1;
        m_aiConnect[8] = 3;
        m_aiConnect[9] = 4;
        m_aiConnect[10] = 3;
        m_aiConnect[11] = 0;
        m_aiConnect[12] = 5;
        m_aiConnect[13] = 2;
        m_aiConnect[14] = 0;
        m_aiConnect[15] = 5;
        m_aiConnect[16] = 1;
        m_aiConnect[17] = 2;
        m_aiConnect[18] = 5;
        m_aiConnect[19] = 3;
        m_aiConnect[20] = 1;
        m_aiConnect[21] = 5;
        m_aiConnect[22] = 0;
        m_aiConnect[23] = 3;

        m_kEMap = new HashMap<Edge,Integer>();

        Integer kInvalid = new Integer(-1);

        m_kEMap.put(new Edge(0, 4), kInvalid);
        m_kEMap.put(new Edge(1, 4), kInvalid);
        m_kEMap.put(new Edge(2, 4), kInvalid);
        m_kEMap.put(new Edge(3, 4), kInvalid);
        m_kEMap.put(new Edge(0, 5), kInvalid);
        m_kEMap.put(new Edge(1, 5), kInvalid);
        m_kEMap.put(new Edge(2, 5), kInvalid);
        m_kEMap.put(new Edge(3, 5), kInvalid);
        m_kEMap.put(new Edge(0, 2), kInvalid);
        m_kEMap.put(new Edge(2, 1), kInvalid);
        m_kEMap.put(new Edge(1, 3), kInvalid);
        m_kEMap.put(new Edge(3, 0), kInvalid);

        int iPNext = 6, iTSubQuantity = 8, iCNext = 24;
        int i0, i1, i2, iP0, iP1, iP2, iT;

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {

            // generate midpoints of edges
            Iterator kEIter = m_kEMap.entrySet().iterator();
            Map.Entry kEntry = null;

            while (kEIter.hasNext()) {
                kEntry = (Map.Entry) kEIter.next();

                Edge kE = (Edge) kEntry.getKey();
                Point3f kP0 = m_akVertex[kE.m_i0];
                Point3f kP1 = m_akVertex[kE.m_i1];
                Point3f kPMid = m_akVertex[iPNext];

                kPMid.add(kP0, kP1);

                float fInvLen = 1.0f /
                                    (float) Math.sqrt((kPMid.x * kPMid.x) + (kPMid.y * kPMid.y) + (kPMid.z * kPMid.z));

                kPMid.scale(fInvLen);
                kEntry.setValue(new Integer(iPNext));
                iPNext++;
            }

            // replace triangle by four subtriangles
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];

                Edge kE01 = new Edge(iP0, iP1);
                Edge kE12 = new Edge(iP1, iP2);
                Edge kE20 = new Edge(iP2, iP0);
                int iM01 = ((Integer) m_kEMap.get(kE01)).intValue();
                int iM12 = ((Integer) m_kEMap.get(kE12)).intValue();
                int iM20 = ((Integer) m_kEMap.get(kE20)).intValue();

                // add new edges

                // replace current triangle by middle triangle
                m_aiConnect[i0] = iM01;
                m_aiConnect[i1] = iM12;
                m_aiConnect[i2] = iM20;

                // append remaining subtriangles
                m_aiConnect[iCNext++] = iP0;
                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iM20;

                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iP1;
                m_aiConnect[iCNext++] = iM12;

                m_aiConnect[iCNext++] = iM20;
                m_aiConnect[iCNext++] = iM12;
                m_aiConnect[iCNext++] = iP2;
            }

            iTSubQuantity *= 4;

            // remove old edges
            m_kEMap.clear();

            // add new edges
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];
                m_kEMap.put(new Edge(iP0, iP1), kInvalid);
                m_kEMap.put(new Edge(iP1, iP2), kInvalid);
                m_kEMap.put(new Edge(iP2, iP0), kInvalid);
            }
        }

        // generate vertex adjacency
        for (iT = 0; iT < m_iTQuantity; iT++) {
            iP0 = m_aiConnect[3 * iT];
            iP1 = m_aiConnect[(3 * iT) + 1];
            iP2 = m_aiConnect[(3 * iT) + 2];

            m_akAdjacent[iP0].insert(iP1);
            m_akAdjacent[iP0].insert(iP2);
            m_akAdjacent[iP1].insert(iP0);
            m_akAdjacent[iP1].insert(iP2);
            m_akAdjacent[iP2].insert(iP0);
            m_akAdjacent[iP2].insert(iP1);
        }

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i].x *= m_afLength[0];
            m_akVertex[i].y *= m_afLength[1];
            m_akVertex[i].z *= m_afLength[2];
            m_akVertex[i].add(m_kCenter);
        }
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
     * Identify all voxels that are inside or on the mesh that represents the brain surface. The surface voxels are
     * constructed by rasterizing the triangles of the mesh in 3D. The centroid of these voxels is used as a seed point
     * for a flood fill of the region enclosed by the surface.
     */
    protected void getInsideVoxels() {
        m_aiMask = new BitSet(m_iQuantity);

        int i, iX, iY, iZ;
        //int[] extents = surfaceRender.getImageA().getExtents();
        //int xDim = extents[0];
        //int yDim = extents[1];
        //int zDim = extents[2];

        for (int iT = 0; iT < m_iTQuantity; iT++) {

            // get the vertices of the triangle
            Point3f kV0 = m_akVertex[m_aiConnect[3 * iT]];
            Point3f kV1 = m_akVertex[m_aiConnect[(3 * iT) + 1]];
            Point3f kV2 = m_akVertex[m_aiConnect[(3 * iT) + 2]];

            /*
             * kV0.z = (float) zDim - 1f - kV0.z; kV1.z = (float) zDim - 1f - kV1.z; kV2.z = (float) zDim - 1f - kV2.z;
             *
             * kV0.y = (float) yDim - 1f - kV0.y; kV1.y = (float) yDim - 1f - kV1.y; kV2.y = (float) yDim - 1f - kV2.y;
             */

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
            int iXMin = (int) fXMin, iXMax = (int) fXMax;
            int iYMin = (int) fYMin, iYMax = (int) fYMax;
            int iZMin = (int) fZMin, iZMax = (int) fZMax;
            int ptr;
            int end = m_aiMask.size();

            for (iY = iYMin; iY <= iYMax; iY++) {

                for (iZ = iZMin; iZ <= iZMax; iZ++) {
                    iX = getIntersectX(kV0, kV1, kV2, iY, iZ);

                    if (iX != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                        }
                    }
                }
            }

            for (iX = iXMin; iX <= iXMax; iX++) {

                for (iZ = iZMin; iZ <= iZMax; iZ++) {
                    iY = getIntersectY(kV0, kV1, kV2, iX, iZ);

                    if (iY != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                        }
                    }
                }
            }

            for (iX = iXMin; iX <= iXMax; iX++) {

                for (iY = iYMin; iY <= iYMax; iY++) {
                    iZ = getIntersectZ(kV0, kV1, kV2, iX, iY);

                    if (iZ != -1) {
                        ptr = getIndex(iX, iY, iZ);

                        if ((ptr >= 0) && (ptr < end)) {
                            m_aiMask.set(ptr);
                        }
                    }
                }
            }
        }

        // m_kCenter.y = yDim - 1 - m_kCenter.y;
        // m_kCenter.z = zDim - 1 - m_kCenter.z;
        floodFill((int) m_kCenter.x, (int) m_kCenter.y, (int) m_kCenter.z);

        nVoxels = 0;

        for (i = 0; i < m_aiMask.size(); i++) {

            if (m_aiMask.get(i)) {
                nVoxels++;
            }
        }
        // System.err.println( "aka  brain = " + nVoxels );

        // volumeMask.or(m_aiMask);
        /*
         * ModelImage newImage = new ModelImage( surfaceRender.getImageA().getType(),
         * surfaceRender.getImageA().getExtents(), "Mask Image", surfaceRender.getUserInterface() ); try {
         * newImage.importData( 0, m_aiMask, true );    newImage.setFileInfo( surfaceRender.getImageA().getFileInfo() );
         * } catch ( IOException er ) {    return; } new ViewJFrameImage( newImage, null, new Dimension( 610, 200 ) );
         */
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
    protected int getIntersectX(Point3f kV0, Point3f kV1, Point3f kV2, int iY, int iZ) {

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

        return (int) (((fC0 * kV0.x) + (fC1 * kV1.x) + (fC2 * kV2.x)) / fDet);
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
    protected int getIntersectY(Point3f kV0, Point3f kV1, Point3f kV2, int iX, int iZ) {

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

        int iY = (int) (((fC0 * kV0.y) + (fC1 * kV1.y) + (fC2 * kV2.y)) / fDet);

        return iY;
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
    protected int getIntersectZ(Point3f kV0, Point3f kV1, Point3f kV2, int iX, int iY) {

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

        int iZ = (int) (((fC0 * kV0.z) + (fC1 * kV1.z) + (fC2 * kV2.z)) / fDet);

        return iZ;
    }

    /**
     * Get the shape burning point surface.
     *
     * @param   root  Burning point branchGroup
     *
     * @return  Shape3D Shape object
     */
    protected Shape3D getShape(BranchGroup root) {
        BranchGroup meshRoot = (BranchGroup) ((BranchGroup) (root.getChild(0)));

        return (Shape3D) (meshRoot.getChild(0));
    }

    /**
     * Mark the probing guide line.
     *
     * @return  BranchGroup return the guide line root reference.
     */
    protected BranchGroup markProbe() {
        Vector3f startPtTransVector = probePanel.getStartPoint();
        Vector3f burnTransVector = probePanel.getBurnPoint();

        LineArray la = new LineArray(2, LineArray.COORDINATES | LineArray.COLOR_3);
        Point3f[] pts = new Point3f[2];

        pts[0] = new Point3f(burnTransVector.x, burnTransVector.y, burnTransVector.z);
        pts[1] = new Point3f(startPtTransVector.x, startPtTransVector.y, startPtTransVector.z);

        Color3f[] clrs = new Color3f[2];

        clrs[0] = new Color3f(1.0f, 0.0f, 0.0f);
        clrs[1] = new Color3f(1.0f, 0.0f, 0.0f);
        la.setCoordinates(0, pts);
        la.setColors(0, clrs);

        Shape3D shape = new Shape3D(la, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            PickTool.setCapabilities(shape, PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        BranchGroup lineBG = new BranchGroup();

        lineBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        lineBG.setCapability(Group.ALLOW_CHILDREN_READ);
        lineBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        lineBG.setCapability(BranchGroup.ALLOW_DETACH);
        lineBG.addChild(shape);

        return lineBG;
    }


    /**
     * Tessellate a unit sphere centered at the origin. Start with an octahedron and subdivide. The final mesh is then
     * affinely mapped to the initial ellipsoid produced by estimateEllipsoid(). The subdivision scheme is described in
     * BrainExtraction.pdf.
     *
     * @param  iSubdivisions    the number of levels to subdivide the ellipsoid
     * @param  tipLen           float the tip length
     * @param  isImageSpace     is image space, always true
     * @param  center           Point3f burning center point coordinate
     * @param  centerTransform  burning center transform
     */
    protected void reGenerateEllipsoidMesh(int iSubdivisions, float tipLen, boolean isImageSpace, Point3f center,
                                           Transform3D centerTransform) {

        // Compute the number of vertices, edges, and triangles for an
        // octahedron subdivided to the specified level.  The recursions are
        // V1 = V0 + E0
        // E1 = 2*E0 + 3*T0
        // T1 = 4*T0
        float semiAxisZ = 0f;
        float semiAxisX = 0f;

        m_iVQuantity = 6;
        m_iEQuantity = 12;
        m_iTQuantity = 8;

        int iStep;

        m_kRotate = new Matrix3f();
        m_kRotate.setIdentity();

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {
            m_iVQuantity = m_iVQuantity + m_iEQuantity;
            m_iEQuantity = (2 * m_iEQuantity) + (3 * m_iTQuantity);
            m_iTQuantity = 4 * m_iTQuantity;
        }

        m_kCenter = new Point3f(0.0f, 0.0f, 0.0f);

        int[] extents = surfaceRender.getImageA().getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];

        float[] resols = surfaceRender.getImageA().getFileInfo()[0].getResolutions();

        /**
         * Volume space -- two pts distance:  Pt ( 0, 0, 0 ) and Pt ( 1, 1, 1) = squreRoot(3) What's the pts distance in
         * the image space.
         */
        float x1 = -1, y1 = 1, z1 = 1, x2 = 1, y2 = 1, z2 = 1;

        if (tipLen == 2f) {
            semiAxisX = 20f / 2f;
            semiAxisZ = 29f / 2f;
        } else if (tipLen == 2.5f) {
            semiAxisX = 32f / 2f;
            semiAxisZ = 46f / 2f;
        } else if (tipLen == 3f) {
            semiAxisX = 22f / 2f;
            semiAxisZ = 39f / 2f;
        } else {
            MipavUtil.displayError("Illegal tip length value for the CoolTip probe.");

            return;
        }

        if (!isImageSpace) {
            x1 = 0;
            y1 = 1;
            z1 = 1;
            x2 = semiAxisX;
            y2 = 1;
            z2 = 1;

            x1 = ((x1 / ((xDim - 1) * resols[0])) * 2f) - 1f;
            y1 = ((y1 / ((yDim - 1) * resols[1])) * 2f) - 1f;
            z1 = ((z1 / ((zDim - 1) * resols[2])) * 2f) - 1f;

            x2 = ((x2 / ((xDim - 1) * resols[0])) * 2f) - 1f;
            y2 = ((y2 / ((yDim - 1) * resols[1])) * 2f) - 1f;
            z2 = ((z2 / ((zDim - 1) * resols[2])) * 2f) - 1f;

            semiAxisX = (float) Math.sqrt((double) (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) +
                                                    ((z2 - z1) * (z2 - z1))));

            x1 = 0;
            y1 = 1;
            z1 = 1;
            x2 = semiAxisZ;
            y2 = 1;
            z2 = 1;

            x1 = ((x1 / ((xDim - 1) * resols[0])) * 2f) - 1f;
            y1 = ((y1 / ((yDim - 1) * resols[1])) * 2f) - 1f;
            z1 = ((z1 / ((zDim - 1) * resols[2])) * 2f) - 1f;

            x2 = ((x2 / ((xDim - 1) * resols[0])) * 2f) - 1f;
            y2 = ((y2 / ((yDim - 1) * resols[1])) * 2f) - 1f;
            z2 = ((z2 / ((zDim - 1) * resols[2])) * 2f) - 1f;
            semiAxisZ = (float) Math.sqrt((double) (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) +
                                                    ((z2 - z1) * (z2 - z1))));
        }

        System.out.println("semiAxisX = " + semiAxisX + " semiAxisZ = " + semiAxisZ);

        if (isImageSpace) {

            // m_afLength[0], [1], and [2] are in mm space
            m_afLength[0] = semiAxisX;
            m_afLength[1] = semiAxisX;
            m_afLength[2] = semiAxisZ;

            // Transform the center point from the volume space into the image space.
            float xBox, yBox, zBox, maxBox;
            float m_fX0;
            float m_fY0;
            float m_fZ0;
            float m_fX1;
            float m_fY1;
            float m_fZ1;

            // Box is really the FOV for each dimension in mm space
            xBox = (xDim - 1) * resols[0];
            yBox = (yDim - 1) * resols[1];
            zBox = (zDim - 1) * resols[2];
            maxBox = xBox;

            if (yBox > maxBox) {
                maxBox = yBox;
            }

            if (zBox > maxBox) {
                maxBox = zBox;
            }

            // Normalize the size
            // xBox range between 0 - 1.
            xBox = xBox / maxBox;
            yBox = yBox / maxBox;
            zBox = zBox / maxBox;

            // m_fX0, Y0, Z0 are the coordinates of one corner of the normalized
            // FOV box, and m_fX1, Y1, Z1 are the coordinates of the diagonal
            // corner of the normalized box
            m_fX0 = -xBox;
            m_fY0 = -yBox;
            m_fX1 = xBox;
            m_fY1 = yBox;

            m_fZ0 = -zBox;
            m_fZ1 = zBox;

            // this should never be true.  Why is it here?
            if (zBox > maxBox) {
                m_fZ0 = -1f;
                m_fZ1 = 1f;
            }

            // transform from volume space to image space
            // using the bounding box approach
            // m_kCenter is the burn center in pixel coordinates
            m_kCenter.x = ((center.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
            m_kCenter.y = ((center.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
            m_kCenter.z = ((center.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

            // flip the z and y coordinate
            m_kCenter.z = zDim - 1 - m_kCenter.z;
            m_kCenter.y = yDim - 1 - m_kCenter.y;

        } else { // in volume space
            m_afLength[0] = semiAxisX;
            m_afLength[1] = semiAxisX;
            m_afLength[2] = semiAxisZ;

            m_kCenter.x = center.x;
            m_kCenter.y = center.y;
            m_kCenter.z = center.z;
        }

        // See BrainExtraction.pdf for a description of the subdivision
        // algorithm.  The use of the HashMap m_kEMap is to store midpoint
        // information for edges so that triangles sharing an edge know what
        // the new vertices are for replacing themselves with subtriangles.
        m_akVertex = new Point3f[m_iVQuantity];
        m_aiConnect = new int[3 * m_iTQuantity];
        m_akAdjacent = new UnorderedSetInt[m_iVQuantity];

        int i;

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i] = new Point3f();
            m_akAdjacent[i] = new UnorderedSetInt(6, 1);
        }

        m_akVertex[0].set(+1.0f, 0.0f, 0.0f);
        m_akVertex[1].set(-1.0f, 0.0f, 0.0f);
        m_akVertex[2].set(0.0f, +1.0f, 0.0f);
        m_akVertex[3].set(0.0f, -1.0f, 0.0f);
        m_akVertex[4].set(0.0f, 0.0f, +1.0f);
        m_akVertex[5].set(0.0f, 0.0f, -1.0f);

        m_aiConnect[0] = 4;
        m_aiConnect[1] = 0;
        m_aiConnect[2] = 2;
        m_aiConnect[3] = 4;
        m_aiConnect[4] = 2;
        m_aiConnect[5] = 1;
        m_aiConnect[6] = 4;
        m_aiConnect[7] = 1;
        m_aiConnect[8] = 3;
        m_aiConnect[9] = 4;
        m_aiConnect[10] = 3;
        m_aiConnect[11] = 0;
        m_aiConnect[12] = 5;
        m_aiConnect[13] = 2;
        m_aiConnect[14] = 0;
        m_aiConnect[15] = 5;
        m_aiConnect[16] = 1;
        m_aiConnect[17] = 2;
        m_aiConnect[18] = 5;
        m_aiConnect[19] = 3;
        m_aiConnect[20] = 1;
        m_aiConnect[21] = 5;
        m_aiConnect[22] = 0;
        m_aiConnect[23] = 3;

        m_kEMap = new HashMap<Edge,Integer>();

        Integer kInvalid = new Integer(-1);

        m_kEMap.put(new Edge(0, 4), kInvalid);
        m_kEMap.put(new Edge(1, 4), kInvalid);
        m_kEMap.put(new Edge(2, 4), kInvalid);
        m_kEMap.put(new Edge(3, 4), kInvalid);
        m_kEMap.put(new Edge(0, 5), kInvalid);
        m_kEMap.put(new Edge(1, 5), kInvalid);
        m_kEMap.put(new Edge(2, 5), kInvalid);
        m_kEMap.put(new Edge(3, 5), kInvalid);
        m_kEMap.put(new Edge(0, 2), kInvalid);
        m_kEMap.put(new Edge(2, 1), kInvalid);
        m_kEMap.put(new Edge(1, 3), kInvalid);
        m_kEMap.put(new Edge(3, 0), kInvalid);

        int iPNext = 6, iTSubQuantity = 8, iCNext = 24;
        int i0, i1, i2, iP0, iP1, iP2, iT;

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {

            // generate midpoints of edges
            Iterator kEIter = m_kEMap.entrySet().iterator();
            Map.Entry kEntry = null;

            while (kEIter.hasNext()) {
                kEntry = (Map.Entry) kEIter.next();

                Edge kE = (Edge) kEntry.getKey();
                Point3f kP0 = m_akVertex[kE.m_i0];
                Point3f kP1 = m_akVertex[kE.m_i1];
                Point3f kPMid = m_akVertex[iPNext];

                kPMid.add(kP0, kP1);

                float fInvLen = 1.0f /
                                    (float) Math.sqrt((kPMid.x * kPMid.x) + (kPMid.y * kPMid.y) + (kPMid.z * kPMid.z));

                kPMid.scale(fInvLen);
                kEntry.setValue(new Integer(iPNext));
                iPNext++;
            }

            // replace triangle by four subtriangles
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];

                Edge kE01 = new Edge(iP0, iP1);
                Edge kE12 = new Edge(iP1, iP2);
                Edge kE20 = new Edge(iP2, iP0);
                int iM01 = ((Integer) m_kEMap.get(kE01)).intValue();
                int iM12 = ((Integer) m_kEMap.get(kE12)).intValue();
                int iM20 = ((Integer) m_kEMap.get(kE20)).intValue();

                // add new edges

                // replace current triangle by middle triangle
                m_aiConnect[i0] = iM01;
                m_aiConnect[i1] = iM12;
                m_aiConnect[i2] = iM20;

                // append remaining subtriangles
                m_aiConnect[iCNext++] = iP0;
                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iM20;

                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iP1;
                m_aiConnect[iCNext++] = iM12;

                m_aiConnect[iCNext++] = iM20;
                m_aiConnect[iCNext++] = iM12;
                m_aiConnect[iCNext++] = iP2;
            }

            iTSubQuantity *= 4;

            // remove old edges
            m_kEMap.clear();

            // add new edges
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];
                m_kEMap.put(new Edge(iP0, iP1), kInvalid);
                m_kEMap.put(new Edge(iP1, iP2), kInvalid);
                m_kEMap.put(new Edge(iP2, iP0), kInvalid);
            }
        }

        // generate vertex adjacency
        for (iT = 0; iT < m_iTQuantity; iT++) {
            iP0 = m_aiConnect[3 * iT];
            iP1 = m_aiConnect[(3 * iT) + 1];
            iP2 = m_aiConnect[(3 * iT) + 2];

            m_akAdjacent[iP0].insert(iP1);
            m_akAdjacent[iP0].insert(iP2);
            m_akAdjacent[iP1].insert(iP0);
            m_akAdjacent[iP1].insert(iP2);
            m_akAdjacent[iP2].insert(iP0);
            m_akAdjacent[iP2].insert(iP1);
        }

        Transform3D probeTform = centerTransform;
        Vector3f rotAngle = new Vector3f();
        Transform3D trans = new Transform3D();
        trans.setIdentity();

        Matrix3f rotationMatrix = new Matrix3f();

        if (probeTform != null) {

            // making sure that rotation is along the origin.
            rotAngle = getRotAngle(probeTform);
            trans.rotX(rotAngle.x);
            trans.rotY(rotAngle.y);
            trans.rotZ(rotAngle.z);
            trans.getRotationScale(rotationMatrix);
            System.err.println("rotationMatrix = " + rotationMatrix.toString());

            // might be null if the probe hasn't been moved..
            probeTform.getRotationScale(m_kRotate);
            System.err.println("m_kRotate = " + m_kRotate.toString());
        }

        System.out.println("m_afLength[0] = " + m_afLength[0] + " m_afLength[1] = " + m_afLength[1] +
                           " m_afLength[2] = " + m_afLength[2]);

        // Notes
        // m_afLength is in pixel coordinates if we are in imageSpace
        for (i = 0; i < m_iVQuantity; i++) {

            // scale the sphere vertices to make ellipsoid vertices in pixel coords if we are in imageSpace
            m_akVertex[i].x *= m_afLength[0];
            m_akVertex[i].y *= m_afLength[1];
            m_akVertex[i].z *= m_afLength[2];

            // Transform for equal resolution units
            // rotate the elliposoid
            m_kRotate.transform(m_akVertex[i]);

            // If image space, rotate the ellipsoid from the volume space to image space.
            // Coincide the volume space coordinate with the image space coordinate.
            if (isImageSpace) {
                rotationMatrix.rotX((float) Math.PI);
                rotationMatrix.transform(m_akVertex[i]);
            }

            if (isImageSpace) {

                // convert ellipsoid into pixel coordinates
                m_akVertex[i].x /= resols[0];
                m_akVertex[i].y /= resols[1];
                m_akVertex[i].z /= resols[2];
            }

            // translate all the points to the center of mass
            m_akVertex[i].add(m_kCenter);
        }

    }


    /**
     * Tessellate a unit sphere centered at the origin. Start with an octahedron and subdivide. The final mesh is then
     * affinely mapped to the initial ellipsoid produced by estimateEllipsoid(). The subdivision scheme is described in
     * BrainExtraction.pdf.
     *
     * @param  iSubdivisions    the number of levels to subdivide the ellipsoid
     * @param  center           Point3f burning center point coordinate
     * @param  centerTransform  burning center transform
     */
    protected void reGenerateSphereMesh(int iSubdivisions, Point3f center, Transform3D centerTransform) {
        float ratio;

        // Compute the number of vertices, edges, and triangles for an
        // octahedron subdivided to the specified level.  The recursions are
        // V1 = V0 + E0
        // E1 = 2*E0 + 3*T0
        // T1 = 4*T0
        m_iVQuantity = 6;
        m_iEQuantity = 12;
        m_iTQuantity = 8;

        int iStep;

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {
            m_iVQuantity = m_iVQuantity + m_iEQuantity;
            m_iEQuantity = (2 * m_iEQuantity) + (3 * m_iTQuantity);
            m_iTQuantity = 4 * m_iTQuantity;
        }

        Transform3D pos = centerTransform;
        Vector3f centerTrans = new Vector3f();

        pos.get(centerTrans);

        m_kCenter = new Point3f(0.0f, 0.0f, 0.0f);

        int[] extents = surfaceRender.getImageA().getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim = extents[2];

        float[] resols = surfaceRender.getImageA().getFileInfo()[0].getResolutions();

        ratio = (20f / xDim) / 10f;
        radius = volumeSpaceRadius / ratio;

        m_afLength[0] = radius; // * 0.4f;
        m_afLength[1] = radius; // * 0.4f;
        m_afLength[2] = radius * resols[0] / resols[2]; // ( radius * 0.4f ) * ( m_fXDelta / m_fZDelta );

        m_kCenter.x = ((center.x + 1) / 2) * (xDim - 1);
        m_kCenter.y = ((2 - (center.y + 1)) / 2) * (yDim - 1);

        // m_kCenter.z = ( ( center.z + 1 ) / 2 ) * ( ( zDim - 1 ) );
        m_kCenter.z = ((2 - (center.z + 1)) / 2) * (zDim - 1);

        // System.err.println("m_kCenter.x = " + m_kCenter.x + " m_kCenter.y = " + m_kCenter.y + " m_kCenter.z = " +
        // m_kCenter.z);

        // See BrainExtraction.pdf for a description of the subdivision
        // algorithm.  The use of the HashMap m_kEMap is to store midpoint
        // information for edges so that triangles sharing an edge know what
        // the new vertices are for replacing themselves with subtriangles.
        m_akVertex = new Point3f[m_iVQuantity];
        m_aiConnect = new int[3 * m_iTQuantity];
        m_akAdjacent = new UnorderedSetInt[m_iVQuantity];

        int i;

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i] = new Point3f();
            m_akAdjacent[i] = new UnorderedSetInt(6, 1);
        }

        m_akVertex[0].set(+1.0f, 0.0f, 0.0f);
        m_akVertex[1].set(-1.0f, 0.0f, 0.0f);
        m_akVertex[2].set(0.0f, +1.0f, 0.0f);
        m_akVertex[3].set(0.0f, -1.0f, 0.0f);
        m_akVertex[4].set(0.0f, 0.0f, +1.0f);
        m_akVertex[5].set(0.0f, 0.0f, -1.0f);

        m_aiConnect[0] = 4;
        m_aiConnect[1] = 0;
        m_aiConnect[2] = 2;
        m_aiConnect[3] = 4;
        m_aiConnect[4] = 2;
        m_aiConnect[5] = 1;
        m_aiConnect[6] = 4;
        m_aiConnect[7] = 1;
        m_aiConnect[8] = 3;
        m_aiConnect[9] = 4;
        m_aiConnect[10] = 3;
        m_aiConnect[11] = 0;
        m_aiConnect[12] = 5;
        m_aiConnect[13] = 2;
        m_aiConnect[14] = 0;
        m_aiConnect[15] = 5;
        m_aiConnect[16] = 1;
        m_aiConnect[17] = 2;
        m_aiConnect[18] = 5;
        m_aiConnect[19] = 3;
        m_aiConnect[20] = 1;
        m_aiConnect[21] = 5;
        m_aiConnect[22] = 0;
        m_aiConnect[23] = 3;

        m_kEMap = new HashMap<Edge,Integer>();

        Integer kInvalid = new Integer(-1);

        m_kEMap.put(new Edge(0, 4), kInvalid);
        m_kEMap.put(new Edge(1, 4), kInvalid);
        m_kEMap.put(new Edge(2, 4), kInvalid);
        m_kEMap.put(new Edge(3, 4), kInvalid);
        m_kEMap.put(new Edge(0, 5), kInvalid);
        m_kEMap.put(new Edge(1, 5), kInvalid);
        m_kEMap.put(new Edge(2, 5), kInvalid);
        m_kEMap.put(new Edge(3, 5), kInvalid);
        m_kEMap.put(new Edge(0, 2), kInvalid);
        m_kEMap.put(new Edge(2, 1), kInvalid);
        m_kEMap.put(new Edge(1, 3), kInvalid);
        m_kEMap.put(new Edge(3, 0), kInvalid);

        int iPNext = 6, iTSubQuantity = 8, iCNext = 24;
        int i0, i1, i2, iP0, iP1, iP2, iT;

        for (iStep = 1; iStep <= iSubdivisions; iStep++) {

            // generate midpoints of edges
            Iterator kEIter = m_kEMap.entrySet().iterator();
            Map.Entry kEntry = null;

            while (kEIter.hasNext()) {
                kEntry = (Map.Entry) kEIter.next();

                Edge kE = (Edge) kEntry.getKey();
                Point3f kP0 = m_akVertex[kE.m_i0];
                Point3f kP1 = m_akVertex[kE.m_i1];
                Point3f kPMid = m_akVertex[iPNext];

                kPMid.add(kP0, kP1);

                float fInvLen = 1.0f /
                                    (float) Math.sqrt((kPMid.x * kPMid.x) + (kPMid.y * kPMid.y) + (kPMid.z * kPMid.z));

                kPMid.scale(fInvLen);
                kEntry.setValue(new Integer(iPNext));
                iPNext++;
            }

            // replace triangle by four subtriangles
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];

                Edge kE01 = new Edge(iP0, iP1);
                Edge kE12 = new Edge(iP1, iP2);
                Edge kE20 = new Edge(iP2, iP0);
                int iM01 = ((Integer) m_kEMap.get(kE01)).intValue();
                int iM12 = ((Integer) m_kEMap.get(kE12)).intValue();
                int iM20 = ((Integer) m_kEMap.get(kE20)).intValue();

                // add new edges

                // replace current triangle by middle triangle
                m_aiConnect[i0] = iM01;
                m_aiConnect[i1] = iM12;
                m_aiConnect[i2] = iM20;

                // append remaining subtriangles
                m_aiConnect[iCNext++] = iP0;
                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iM20;

                m_aiConnect[iCNext++] = iM01;
                m_aiConnect[iCNext++] = iP1;
                m_aiConnect[iCNext++] = iM12;

                m_aiConnect[iCNext++] = iM20;
                m_aiConnect[iCNext++] = iM12;
                m_aiConnect[iCNext++] = iP2;
            }

            iTSubQuantity *= 4;

            // remove old edges
            m_kEMap.clear();

            // add new edges
            for (iT = 0; iT < iTSubQuantity; iT++) {
                i0 = 3 * iT;
                i1 = i0 + 1;
                i2 = i1 + 1;
                iP0 = m_aiConnect[i0];
                iP1 = m_aiConnect[i1];
                iP2 = m_aiConnect[i2];
                m_kEMap.put(new Edge(iP0, iP1), kInvalid);
                m_kEMap.put(new Edge(iP1, iP2), kInvalid);
                m_kEMap.put(new Edge(iP2, iP0), kInvalid);
            }
        }

        // generate vertex adjacency
        for (iT = 0; iT < m_iTQuantity; iT++) {
            iP0 = m_aiConnect[3 * iT];
            iP1 = m_aiConnect[(3 * iT) + 1];
            iP2 = m_aiConnect[(3 * iT) + 2];

            m_akAdjacent[iP0].insert(iP1);
            m_akAdjacent[iP0].insert(iP2);
            m_akAdjacent[iP1].insert(iP0);
            m_akAdjacent[iP1].insert(iP2);
            m_akAdjacent[iP2].insert(iP0);
            m_akAdjacent[iP2].insert(iP1);
        }

        for (i = 0; i < m_iVQuantity; i++) {
            m_akVertex[i].x *= m_afLength[0];
            m_akVertex[i].y *= m_afLength[1];
            m_akVertex[i].z *= m_afLength[2];
            m_akVertex[i].add(m_kCenter);
        }
    }

    /**
     * Initialize the mask burning vector.
     *
     * @param  _vector  MaskBurnVector reference.
     */
    protected void setMaskBurnVector(Vector<MaskBurnAttribute> _vector) {
        maskBurnVector = _vector;
    }

    /**
     * Initial the volume mask from burnBase.
     *
     * @param  _volumeMask  byte[] array volume mask array.
     */
    protected void setVolumeMask(BitSet _volumeMask) {
        volumeMask = _volumeMask;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     */
    public class UnorderedSetInt {

        /** The array storage for the set. */
        protected int[] m_aiElement;

        /** On a reallocation, the old maximum quantity is incremented by this value. */
        protected int m_iGrow;

        /** The maximum number of elements in the array. It is always the case that m_iQuantity <= m_iMaxQuantity. */
        protected int m_iMaxQuantity;

        /** Support for remove and removeAt. */
        protected int m_iOldIndex, m_iNewIndex;

        /** The number of valid elements in the array. The valid indices are 0 <= i < m_iQuantity. */
        protected int m_iQuantity;

        /**
         * The default growth value for reallocations of the array representing the set. The application can change this
         * to whatever is appropriate for its purposes.
         */
        private int DEFAULT_GROW = 8;

        /**
         * Construct an empty unordered set. The initial maximum quantity and growth values are DEFAULT_GROW. When
         */
        public UnorderedSetInt() {
            reset();
        }

        /**
         * Create an unordered set that is a deep copy of the input set.
         *
         * @param  kSet  The input set to copy.
         */
        public UnorderedSetInt(UnorderedSetInt kSet) {
            copy(kSet);
        }

        /**
         * Construct an empty unordered set with the specified maximum quantity and growth values.
         *
         * @param  iMaxQuantity  The initial number of elements in the array. If the value is nonpositive, the initial
         *                       number is DEFAULT_GROW.
         * @param  iGrow         The growth amount for a reallocation. If a reallocation occurs, the new number of
         *                       elements is the current maximum quantity plus the growth value. If the input value is
         *                       nonpositive, the growth is set to DEFAULT_GROW.
         */
        public UnorderedSetInt(int iMaxQuantity, int iGrow) {
            reset(iMaxQuantity, iGrow);
        }

        /**
         * Append an element to the end of the storage array.
         *
         * @param   iElement  The element to append.
         *
         * @return  The array location that contains the newly appended element. A side effect of this call is
         *          reallocation of the storage array, if necessary.
         */
        public int append(int iElement) {

            if (m_iQuantity == m_iMaxQuantity) {
                int iNewMaxQuantity = m_iMaxQuantity + m_iGrow;
                int[] aiNewElement = new int[iNewMaxQuantity];

                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iMaxQuantity);
                m_iMaxQuantity = iNewMaxQuantity;
                m_aiElement = aiNewElement;
            }

            int iLocation = m_iQuantity++;

            m_aiElement[iLocation] = iElement;

            return iLocation;
        }

        /**
         * Use exactly the amount of array storage for the current elements in the set. After the call, getQuantity()
         * and getMaximumQuantity() return the same value. This call does cause a reallocation.
         */
        public void compactify() {

            if (m_iQuantity > 0) {

                // Try Catch - Matt
                int[] aiNewElement = new int[m_iQuantity];

                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iQuantity);
                m_iMaxQuantity = m_iQuantity;
                m_aiElement = aiNewElement;
            } else {
                reset();
            }
        }

        /**
         * Make a deep copy of the input set.
         *
         * @param  kSet  The set to make a deep copy of.
         */
        public void copy(UnorderedSetInt kSet) {
            m_iQuantity = kSet.m_iQuantity;
            m_iMaxQuantity = kSet.m_iMaxQuantity;
            m_iGrow = kSet.m_iGrow;
            m_aiElement = new int[m_iMaxQuantity];
            System.arraycopy(kSet.m_aiElement, 0, m_aiElement, 0, m_iMaxQuantity);
        }

        /**
         * Search the set to see if the input element currently exists.
         *
         * @param   iElement  The element to search for.
         *
         * @return  The value is true if and only if the element is found in the set.
         */
        public boolean exists(int iElement) {

            for (int i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    return true;
                }
            }

            return false;
        }

        /**
         * Retrieve the element in the array location i. It is necessary that 0 <= i < getQuantity() in order to read
         * valid elements.
         *
         * @param   i  The array location whose element is to be retrieved.
         *
         * @return  The element in array location i.
         */
        public final int get(int i) {
            return m_aiElement[i];
        }

        /**
         * The growth value for reallocations. If a reallocation must occur, the new maximum quantity is the current
         * maximum quantity plus the growth amount.
         *
         * @return  The growth value.
         */
        public final int getGrow() {
            return m_iGrow;
        }

        /**
         * The maximum quantity of elements in the set. Not all elements are necessarily used. The used quantity is
         * provided by getQuantity().
         *
         * @return  The maximum quantity of elements in the set.
         */
        public final int getMaxQuantity() {
            return m_iMaxQuantity;
        }

        /**
         * On a call to remove or removeAt, the last element in the array is potentially moved to the array location
         * vacated by the removed element. The new location of the last element is retrived by this function. However,
         * if the last element is the one that was removed, this function returns -1. If you need the value, you must
         * call this function before the next call to remove or removeAt.
         *
         * @return  The new location of the last element that was moved.
         */
        public final int getNewIndex() {
            return m_iNewIndex;
        }

        /**
         * On a call to remove or removeAt, the last element in the array is moved to the array location vacated by the
         * removed element. The old location of the last element is retrived by this function. If you need the value,
         * you must call this function before the next call to remove or removeAt.
         *
         * @return  The old location of the last element that was moved.
         */
        public final int getOldIndex() {
            return m_iOldIndex;
        }

        /**
         * The current number of valid elements in the array. This number is less than or equal to the maximum quantity.
         * The elements with indices 0 through getQuantity()-1 are the valid ones.
         *
         * @return  The current number of valid elements.
         */
        public final int getQuantity() {
            return m_iQuantity;
        }

        /**
         * Insert an element into the set.
         *
         * @param   iElement  The element to insert.
         *
         * @return  The value is true if and only if the element is inserted. The input element is not inserted if it
         *          already exists in the set. A side effect of this call is reallocation of the storage array, if
         *          necessary.
         */
        public boolean insert(int iElement) {
            int i;

            for (i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    return false;
                }
            }

            if (m_iQuantity == m_iMaxQuantity) {
                int iNewMaxQuantity = m_iMaxQuantity + m_iGrow;
                int[] aiNewElement = new int[iNewMaxQuantity];

                System.arraycopy(m_aiElement, 0, aiNewElement, 0, m_iMaxQuantity);
                m_iMaxQuantity = iNewMaxQuantity;
                m_aiElement = aiNewElement;
            }

            m_aiElement[m_iQuantity++] = iElement;

            return true;
        }

        /**
         * Remove the specified element from the set.
         *
         * @param   iElement  The element to remove.
         *
         * @return  The value is true if and only if the element existed and was removed. The last element is
         *          potentially moved into the slot vacated by the specified element. If needed, the old and new
         *          locations of the last element can be retrieved by calls to getOldIndex() and getNewIndex(). If the
         *          last element was the one removed, getNewIndex() returns -1.
         */
        public boolean remove(int iElement) {

            for (int i = 0; i < m_iQuantity; i++) {

                if (iElement == m_aiElement[i]) {
                    m_iQuantity--;
                    m_iOldIndex = m_iQuantity;

                    if (i != m_iQuantity) {
                        m_aiElement[i] = m_aiElement[m_iQuantity];
                        m_iNewIndex = i;
                    } else {
                        m_iNewIndex = -1;
                    }

                    return true;
                }
            }

            return false;
        }

        /**
         * Remove the element from the set in the specified location.
         *
         * @param   i  The array location whose element is to be removed.
         *
         * @return  The value is true if and only if the input location is within the valid index range 0 <= i <
         *          getQuantity(). The last element is potentially moved into the slot vacated by the specified element.
         *          If needed, the old and new locations of the last element can be retrieved by calls to getOldIndex()
         *          and getNewIndex(). If the last element was the one removed, getNewIndex() returns -1.
         */
        public boolean removeAt(int i) {

            if ((0 <= i) && (i < m_iQuantity)) {
                m_iQuantity--;
                m_iOldIndex = m_iQuantity;

                if (i != m_iQuantity) {
                    m_aiElement[i] = m_aiElement[m_iQuantity];
                    m_iNewIndex = i;
                } else {
                    m_iNewIndex = -1;
                }

                return true;
            }

            return false;
        }

        /**
         * Reset the unordered set to its initial state. The old array is deleted. The new array has a maximum quantity
         * of DEFAULT_GROW and the growth value is DEFAULT_GROW.
         */
        public void reset() {
            reset(0, 0);
        }

        /**
         * Reset the unordered set to the specified state. The old array is deleted. The new array has a maximum
         * quantity and growth value as specified by the inputs.
         *
         * @param  iMaxQuantity  The new maximum quantity for the array.
         * @param  iGrow         The new growth value.
         */
        public void reset(int iMaxQuantity, int iGrow) {

            if (iMaxQuantity <= 0) {
                iMaxQuantity = DEFAULT_GROW;
            }

            if (iGrow <= 0) {
                iGrow = DEFAULT_GROW;
            }

            m_iQuantity = 0;
            m_iMaxQuantity = iMaxQuantity;
            m_iGrow = iGrow;
            m_aiElement = new int[m_iMaxQuantity];
        }

        /**
         * Assign the specified element to array location i. It is necessary that 0 <= i < getMaxQuantity().
         *
         * @param  i         The array location to assign to.
         * @param  iElement  The element to assign to array location i.
         */
        public final void set(int i, int iElement) {
            m_aiElement[i] = iElement;
        }
    }

    /**
     * A representation of an edge for the vertex-edge-triangle table. This class stores the pair of vertex indices for
     * the end points of the edge. The edges <V0,V1> and <V1,V0> are considered to be identical. To simplify
     * comparisons, the class stores the ordered indices. The class extends Object to obtain support for hashing into a
     * map of edges.
     */
    protected class Edge extends Object {

        /** DOCUMENT ME! */
        public int m_i0, m_i1;

        /**
         * Constructs an edge in the table.
         *
         * @param  i0  a vertex index for an end point
         * @param  i1  a vertex index for an end point
         */
        public Edge(int i0, int i1) {

            if (i0 < i1) {

                // i0 is minimum
                m_i0 = i0;
                m_i1 = i1;
            } else {

                // i1 is minimum
                m_i0 = i1;
                m_i1 = i0;
            }
        }

        /**
         * Support for hashing into a map of edges.
         *
         * @param   kObject  an edge for comparison to the current one
         *
         * @return  true iff the edges are identical. Because the class stores ordered indices, it is not necessary to
         *          use the more expensive test (i0 == other.i0 && i1 == other.i1) || (i0 == other.i1 && i1 ==
         *          other.i0).
         */
        public boolean equals(Object kObject) {
            Edge kE = (Edge) kObject;

            return (m_i0 == kE.m_i0) && (m_i1 == kE.m_i1);
        }

        /**
         * Support for hashing into a map of edges.
         *
         * @return  the hash key for the edge
         */
        public int hashCode() {
            return (m_i0 << 16) | m_i1;
        }
    }

}
