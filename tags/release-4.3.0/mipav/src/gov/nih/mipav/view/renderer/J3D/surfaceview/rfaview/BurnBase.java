package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * <p>Title: BurnBase</p>
 *
 * <p>Description: This class defined the basic bunring point image sence graph structure.</p>
 *
 * @author  Ruida Cheng
 */
public class BurnBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Define the default burn type value. */
    public static int DEFAULTBURN = 0;

    /** Define the heat burn type value. */
    public static int HEATBURN = 1;

    /** Define the regular burn type value. */
    public static int REGULARBURN = 2;

    /** Define the CoolTip burn type value. */
    public static int COOLTIPBURN = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Current burning point index. */
    protected int burnIndex = -1;

    /** image extents. */
    int extentX, extentY, extentZ;

    /** The root branch group of the burning point. */
    private BranchGroup burnRootParentBG;

    /** Burning type flag, default to the default burning point type. */
    private int burnType = DEFAULTBURN;

    /** CoolTip burning point reference that represents the CoolTip probe ablation type. */
    private BurnCoolTipView coolTipBurn;

    /** Default burning point reference that represents the default probe ablation type. */
    private BurnBaseView defaultBurn;

    /** Heat burning point reference that represents the thermal probe ablation type. */
    private BurnHeatView heatBurn;

    /** Mask burning center attribute. */
    private Vector maskBurnVector;

    /** Regular burning point reference that represents the regular probe ablation type. */
    private BurnRegularView regularBurn;

    /** Surface renderer reference. */
    private SurfaceRender surfaceRender;

    /** burning point volume mask. */
    private BitSet volumeMask;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor. Initializes the burning point image scene graph structure and builds three types of burning point.
     *
     * @param  _surfaceRender  Surface Renderer reference.
     * @param  _probePanel     JPanelProbe reference.
     */
    public BurnBase(SurfaceRender _surfaceRender, JPanelProbe _probePanel) {
        surfaceRender = _surfaceRender;
        init();
        extentX = surfaceRender.getImageA().getExtents()[0];
        extentY = surfaceRender.getImageA().getExtents()[1];
        extentZ = surfaceRender.getImageA().getExtents()[2];
        volumeMask = new BitSet(extentX * extentY * extentZ);

        defaultBurn = new BurnBaseView(surfaceRender, _probePanel, burnRootParentBG);
        heatBurn = new BurnHeatView(surfaceRender, _probePanel, burnRootParentBG);
        regularBurn = new BurnRegularView(surfaceRender, _probePanel, burnRootParentBG);
        coolTipBurn = new BurnCoolTipView(surfaceRender, _probePanel, burnRootParentBG);

        defaultBurn.setVolumeMask(volumeMask);
        heatBurn.setVolumeMask(volumeMask);
        regularBurn.setVolumeMask(volumeMask);
        coolTipBurn.setVolumeMask(volumeMask);

        maskBurnVector = new Vector();
        defaultBurn.setMaskBurnVector(maskBurnVector);
        heatBurn.setMaskBurnVector(maskBurnVector);
        regularBurn.setMaskBurnVector(maskBurnVector);
        coolTipBurn.setMaskBurnVector(maskBurnVector);

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Active the current burning point type.
     *
     * @param  type  burning point type value.
     */
    public void activeBurn(int type) {
        burnType = type;
    }


    /**
     * Calculate the current burning point volume.
     *
     * @param   surface    DOCUMENT ME!
     * @param   treatment  DOCUMENT ME!
     *
     * @return  float burning point volume in voxels.
     */
    public float calcVolume(SurfaceAttributes surface, TreatmentInformation treatment) {

        if (burnType == DEFAULTBURN) {
            defaultBurn.calcTotalVolume(surface, treatment);
        } else if (burnType == HEATBURN) {
            heatBurn.calcTotalVolume(surface, treatment);
        } else if (burnType == REGULARBURN) {
            regularBurn.calcTotalVolume(surface, treatment);
        } else if (burnType == COOLTIPBURN) {
            coolTipBurn.calcTotalVolume(surface, treatment);
        }

        return 0f;

    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        surfaceRender = null;

        if (defaultBurn != null) {
            defaultBurn.dispose();
            defaultBurn = null;
        }

        if (heatBurn != null) {
            heatBurn.dispose();
            heatBurn = null;
        }

        if (regularBurn != null) {
            regularBurn.dispose();
            regularBurn = null;
        }

        if (coolTipBurn != null) {
            coolTipBurn.dispose();
            coolTipBurn = null;
        }

        volumeMask = null;
        maskBurnVector = null;
    }

    /**
     * Attach or detach burn labels.
     *
     * @param  flag  <code>true</code> attach burn label, <code>false</code> not attach burn label.
     */
    public void enableBurnLabels(boolean flag) {
        defaultBurn.enableBurnLabels(flag);
        heatBurn.enableBurnLabels(flag);
        regularBurn.enableBurnLabels(flag);
        coolTipBurn.enableBurnLabels(flag);
    }

    /**
     * Check whether the burning sphere is picked or not.
     *
     * @param   pickedShape  sphere burning shape.
     *
     * @return  int burning point index
     */
    public int findBurnPoint(Shape3D pickedShape) {
        int index;
        index = defaultBurn.findBurnPoint(pickedShape);

        if (index != -1) {
            return index;
        } else {
            index = heatBurn.findBurnPoint(pickedShape);

            if (index != -1) {
                return index;
            }
        }

        return -1;
    }

    /**
     * Get the root of the burning point.
     *
     * @return  BranchGroup The root burning point branch group.
     */
    public BranchGroup getBurnRootParentBG() {
        return burnRootParentBG;
    }

    /**
     * Get the current burning point color.
     *
     * @return  Color4f burning point surface color appearance.
     */
    public Color4f getColor() {

        if (burnType == DEFAULTBURN) {
            return defaultBurn.getColor();
        } else if (burnType == HEATBURN) {
            return heatBurn.getColor();
        } else if (burnType == REGULARBURN) {
            return regularBurn.getColor();
        } else if (burnType == COOLTIPBURN) {
            return coolTipBurn.getColor();
        }

        return null;
    }

    /**
     * Get the volume difference btw the tumor surface and burning sphere packings.
     *
     * @return  volume volume difference in mm^3.
     */
    public float getDiffVolume() {

        if (burnType == DEFAULTBURN) {
            return defaultBurn.getDiffVolume();
        } else if (burnType == HEATBURN) {
            return heatBurn.getDiffVolume();
        } else if (burnType == REGULARBURN) {
            return regularBurn.getDiffVolume();
        } else if (burnType == COOLTIPBURN) {
            return coolTipBurn.getDiffVolume();
        }

        return 0f;
    }


    /**
     * Get the burning sphere or ellipsoid mask.
     *
     * @return  BitSet mask burning point mask.
     */
    public BitSet getMask() {

        if (burnType == DEFAULTBURN) {
            return defaultBurn.getBurnMask();
        } else if (burnType == HEATBURN) {
            return heatBurn.getBurnMask();
        } else if (burnType == REGULARBURN) {
            return regularBurn.getBurnMask();
        } else if (burnType == COOLTIPBURN) {
            return coolTipBurn.getBurnMask();
        }

        return null;
    }

    /**
     * Get the burning sphere radius as a point format ( semiX, Y, Z ).
     *
     * @return  Point3f sphere radius.
     */
    public Point3f getRadius() {

        if (burnType == DEFAULTBURN) {
            return defaultBurn.getRadius();
        } else if (burnType == HEATBURN) {
            return heatBurn.getRadius();
        } else if (burnType == REGULARBURN) {
            return regularBurn.getRadius();
        } else if (burnType == COOLTIPBURN) {
            return coolTipBurn.getRadius();
        }

        return null;
    }

    /**
     * Get the shape of the burning point surface.
     *
     * @param   root  BranchGroup
     *
     * @return  Shape3D
     */
    public Shape3D getShape(BranchGroup root) {

        if (burnType == DEFAULTBURN) {
            return defaultBurn.getShape(root);
        } else if (burnType == HEATBURN) {
            return heatBurn.getShape(root);
        } else if (burnType == REGULARBURN) {
            return regularBurn.getShape(root);
        } else if (burnType == COOLTIPBURN) {
            return coolTipBurn.getShape(root);
        }

        return null;

    }

    /**
     * The the burning point geometry sphere branch group.
     *
     * @return  BranchGroup branch group of the sphere.
     */
    public BranchGroup getSphereBG() {

        if (burnType == DEFAULTBURN) {
            return defaultBurn.getSphereBG();
        } else if (burnType == HEATBURN) {
            return heatBurn.getSphereBG();
        } else if (burnType == REGULARBURN) {
            return regularBurn.getSphereBG();
        } else if (burnType == COOLTIPBURN) {
            return coolTipBurn.getSphereBG();
        }

        return null;

    }

    /**
     * Get the burning point center coordinates. Called by the probe control panel to show the current burning point
     * center.
     *
     * @return  Vector3f burning point center coordinate.
     */
    public Vector3f getTranslate() {

        if (burnType == DEFAULTBURN) {
            return defaultBurn.getTranslate();
        } else if (burnType == HEATBURN) {
            return heatBurn.getTranslate();
        } else if (burnType == REGULARBURN) {
            return regularBurn.getTranslate();
        } else if (burnType == COOLTIPBURN) {
            return coolTipBurn.getTranslate();
        }

        return null;

    }

    /**
     * Get the burning point volume.
     *
     * @return  volume burning point volume.
     */
    public float getVolume() {

        if (burnType == DEFAULTBURN) {
            return defaultBurn.getVolume();
        } else if (burnType == HEATBURN) {
            return heatBurn.getVolume();
        } else if (burnType == REGULARBURN) {
            return regularBurn.getVolume();
        } else if (burnType == COOLTIPBURN) {
            return coolTipBurn.getVolume();
        }

        return 0f;
    }

    /**
     * Remove all the burning point centers.
     */
    public void removeAllBurnCenters() {
        maskBurnVector.removeAllElements();
    }

    /**
     * Remove the burning center element.
     *
     * @param  index  removed burning center index
     */
    public void removeBurnCenter(int index) {
        maskBurnVector.removeElementAt(index);
    }

    /**
     * Show the the burning procedure.
     *
     * @param  tipLen  the length of the probe tip in cm
     * @param  time    burning procedure time duration.
     */
    public void startBurn(float tipLen, float time) {
        burnIndex++;

        if (burnType == DEFAULTBURN) {
            defaultBurn.startBurn(tipLen, time, burnIndex);
        } else if (burnType == HEATBURN) {
            heatBurn.startBurn(tipLen, time, burnIndex);
        } else if (burnType == REGULARBURN) {
            regularBurn.startBurn(tipLen, time, burnIndex);
        } else if (burnType == COOLTIPBURN) {
            coolTipBurn.startBurn(tipLen, time, burnIndex);
        }
    }

    /**
     * Build the root of the burning point branch group, and attached the root to the parent scene root transform group.
     */
    private void createImageSceneGraph() {

        /** burn root branch group */
        burnRootParentBG = new BranchGroup();
        burnRootParentBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        burnRootParentBG.setCapability(Group.ALLOW_CHILDREN_READ);
        burnRootParentBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        burnRootParentBG.setCapability(Node.ALLOW_PICKABLE_READ);
        burnRootParentBG.setCapability(Node.ALLOW_PICKABLE_WRITE);
        burnRootParentBG.setCapability(BranchGroup.ALLOW_DETACH);
        surfaceRender.getSceneRootTG().addChild(burnRootParentBG);
    }

    /**
     * Setup the initial image scene graph structure.
     */
    private void init() {
        createImageSceneGraph();
    }
}
