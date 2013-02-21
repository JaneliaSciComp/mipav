package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * <p>Title: MaskBurnAtribute</p>
 *
 * <p>Description: Recording the masked burning information, such as burning center, burning center transform, tip
 * length, etc.</p>
 */
public class MaskBurnAttribute {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Burning type, default, regular, heat, etc. */
    public int burnType;

    /** Burning center coordinate. */
    public Point3f center;

    /** Burning center tranform. */
    public Transform3D centerTransform;

    /** Burning probe tip length. */
    public float tipLen;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for the burn attributes.
     *
     * @param  _center           Point3f burning sphere packing center.
     * @param  _centerTransform  Transform3D burning sphere transform
     * @param  _burnType         int burn type.
     * @param  _tipLen           float tip length.
     */
    public MaskBurnAttribute(Point3f _center, Transform3D _centerTransform, int _burnType, float _tipLen) {
        center = _center;
        centerTransform = _centerTransform;
        burnType = _burnType;
        tipLen = _tipLen;
    }

}
