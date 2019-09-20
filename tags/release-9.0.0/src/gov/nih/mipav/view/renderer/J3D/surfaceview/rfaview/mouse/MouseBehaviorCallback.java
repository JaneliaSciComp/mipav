package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.mouse;


import javax.media.j3d.*;


/**
 * Also added this line: public final static int SELECTION=4; And this method: public void transformClicked Also added
 * this line: public final static int PROPERTIES=5; Added this method: public void transformDoubleClicked.
 */
public interface MouseBehaviorCallback {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int ROTATE = 0;

    /** DOCUMENT ME! */
    int TRANSLATE = 1;

    /** DOCUMENT ME! */
    int ZOOM = 2;

    /** DOCUMENT ME! */
    int SELECTION = 4;

    /** DOCUMENT ME! */
    int PROPERTIES = 5;

    /** DOCUMENT ME! */
    int ORBIT = 6;

    /** Added so that Probe Rotation with the left and right mouse buttons could be distinguised. */
    int ROTATE_LEFTBUTTON = 7;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Classes implementing this interface that are registered with one of the MouseBehaviors will be called every time
     * the behavior updates the Transform.
     *
     * @param  type       will be one of ROTATE, TRANSLATE or ZOOM
     * @param  transform  DOCUMENT ME!
     */
    void transformChanged(int type, Transform3D transform);

}
