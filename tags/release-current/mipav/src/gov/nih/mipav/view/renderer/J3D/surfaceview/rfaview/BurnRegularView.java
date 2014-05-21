package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * The default probe burning type view. The class define the regular probe burning point's geometry shape, and probing
 * path mark behavior.
 *
 * @author  Ruida Cheng
 */
public class BurnRegularView extends BurnBaseView {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor. Setup the burning point related branch groups.
     *
     * @param  _surfaceRender     SurfaceRender refernece.
     * @param  _probePanel        JPanelProbe probe control panel reference.
     * @param  _burnRootParentBG  BranchGroup root of the burning point.
     */
    public BurnRegularView(SurfaceRender _surfaceRender, JPanelProbe _probePanel, BranchGroup _burnRootParentBG) {
        super(_surfaceRender, _probePanel, _burnRootParentBG);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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

        trans.setScale(0.05);

        TransformGroup transforms = new TransformGroup(trans);

        transforms.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        transforms.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        transforms.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
        transforms.setCapability(BranchGroup.ALLOW_DETACH);

        Transform3D pos = probePanel.getProbeBase().getCoordinate();

        pos.setScale(0.05);
        transforms.setTransform(pos);

        Vector3d scale = new Vector3d();
        transforms.getTransform(trans);
        trans.getScale(scale);
        scale.set(scale.x * 2, scale.y * 2, scale.z * 1.5f);
        trans.setScale(scale);
        transforms.setTransform(trans);

        burningBG[burnIndex].addChild(transforms);
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
        indexVector.add(new Integer(_index));
        burnType = BurnBase.REGULARBURN;

        burnColor = purpleColor;

        Appearance app = new Appearance();
        Material mat = new Material(emissiveColor, emissiveColor, purple, sepcualarColor, 50.0f);

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
        app.setPolygonAttributes(kPAttr);

        spheres = new Sphere(sphereRadius, app);
        spheres.setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
        spheres.setCapability(Sphere.ALLOW_CHILDREN_EXTEND);
        spheres.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        spheres.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        spheres.getShape().setAppearanceOverrideEnable(true);
        spheres.getShape().setCapability(Geometry.ALLOW_INTERSECT);

        try {
            PickCanvas.setCapabilities(spheres.getShape(), PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        histogramAnalysis();
        estimateEllipsoid();

        // generate the mesh in volume space to add into the scene graph
        generateEllipsoidMesh(BurnBase.REGULARBURN, tipLen, 5, false);

        ModelTriangleMesh[] kMesh = new ModelTriangleMesh[1];
        kMesh[0] = new ModelTriangleMesh(m_akVertex, m_aiConnect);

        for (int j = 0; j < kMesh.length; j++) {
            kMesh[j].computeNormals();
        }

        // calculate the volume in the image space
        generateEllipsoidMesh(BurnBase.REGULARBURN, tipLen, 5, true);

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
}
