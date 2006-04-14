package gov.nih.mipav.view.renderer.surfaceview.rfaview.mouse;

import javax.media.j3d.Transform3D;


/*
        Also added this line:
        public final static int SELECTION=4;

        And this method:
        public void transformClicked

        Also added this line:
        public final static int PROPERTIES=5;

        Added this method:
        public void transformDoubleClicked

 */

public interface MouseBehaviorCallback
{

	public final static int ROTATE=0;
	public final static int TRANSLATE=1;
	public final static int ZOOM=2;
	public final static int SELECTION=4;
	public final static int PROPERTIES=5;
	public final static int ORBIT=6;
        /* Added so that Probe Rotation with the left and right mouse buttons
         * could be distinguised */
	public final static int ROTATE_LEFTBUTTON=7;

	/*
	 * Classes implementing this interface that are registered with
	 * one of the MouseBehaviors will be called every time the
	 * behavior updates the Transform
	 * @param type will be one of ROTATE, TRANSLATE or ZOOM
	 */
	public void transformChanged( int type, Transform3D transform );

}
