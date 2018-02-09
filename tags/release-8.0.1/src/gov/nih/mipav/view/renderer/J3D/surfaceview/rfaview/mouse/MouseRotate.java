package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.mouse;


import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * MouseRotate is a Java3D behavior object that lets users control the rotation of an object via a mouse.
 *
 * <p>To use this utility, first create a transform group that this rotate behavior will operate on. Then,
 *
 * <blockquote>
 * <pre>

     MouseRotate behavior = new MouseRotate();
     behavior.setTransformGroup(objTrans);
     objTrans.addChild(behavior);
     behavior.setSchedulingBounds(bounds);

 *</pre>
 * </blockquote>
 *
 * The above code will add the rotate behavior to the transform group. The user can rotate any object attached to the
 * objTrans. Also this: < id = event[i].getID(); < if ((id == MouseEvent.MOUSE_DRAGGED) && <
 * !((MouseEvent)event[i]).isMetaDown() && < !((MouseEvent)event[i]).isAltDown()){ > id = event[i].getID(); > if ((id ==
 * MouseEvent.MOUSE_DRAGGED) && > ((MouseEvent)event[i]).isMetaDown() && > !((MouseEvent)event[i]).isAltDown()){ Note:
 * By changing the bangs (!), you control which of the mouse buttons are filtered using the Alt and Meta key
 * nomenclature. The old if statement says if the mouse is dragged and neither the right nor middle button are pressed .
 * The new if statement says if the mouse is dragged and the right button and not the middle button is pressed. Made
 * Additions under the mouse conditions for picking Added the algorithm to rotate the object by multiplying the inverse
 * of the viewers rotation then applying the users rotation (just like when looking normal to XY plane) then multiply
 * back in the original users rotation to put object back with the newly applied rotations.
 */
public class MouseRotate extends MouseBehavior {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    double x_angle, y_angle;

    /** DOCUMENT ME! */
    double x_factor = 0.001; // .03;

    /** DOCUMENT ME! */
    double y_factor = 0.001; // .03;

    /** DOCUMENT ME! */
    private MouseBehaviorCallback callback = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a default mouse rotate behavior.
     */
    public MouseRotate() {
        super(0);
    }

    /**
     * Creates a rotate behavior given the transform group.
     *
     * @param  transformGroup  The transformGroup to operate on.
     */
    public MouseRotate(TransformGroup transformGroup) {
        super(transformGroup);
    }

    /**
     * Creates a rotate behavior. Note that this behavior still needs a transform group to work on (use
     * setTransformGroup(tg)) and the transform group must add this behavior.
     *
     * @param  flags  interesting flags (wakeup conditions).
     */
    public MouseRotate(int flags) {
        super(flags);
    }

    /**
     * Same as above but with boolean fix.
     *
     * @param  flags        DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseRotate(int flags, boolean behaviorfix) {
        super(flags, behaviorfix);
    }

    /**
     * Same as above but with Viewer Transform Group and boolean fix.
     *
     * @param  flags        DOCUMENT ME!
     * @param  VPTG         DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseRotate(int flags, TransformGroup VPTG, boolean behaviorfix) {
        super(flags, VPTG, behaviorfix);
    }

    /**
     * Creates a new MouseRotate object.
     *
     * @param  transformGroup  DOCUMENT ME!
     * @param  VPTG            DOCUMENT ME!
     * @param  behaviorfix     DOCUMENT ME!
     */
    public MouseRotate(TransformGroup transformGroup, TransformGroup VPTG, boolean behaviorfix) {
        super(transformGroup, VPTG, behaviorfix);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return the x-axis movement multipler.
     *
     * @return  DOCUMENT ME!
     */
    public double getXFactor() {
        return x_factor;
    }

    /**
     * Return the y-axis movement multipler.
     *
     * @return  DOCUMENT ME!
     */
    public double getYFactor() {
        return y_factor;
    }

    /**
     * DOCUMENT ME!
     */
    public void initialize() {
        super.initialize();
        x_angle = 0;
        y_angle = 0;

        if ((flags & INVERT_INPUT) == INVERT_INPUT) {
            invert = true;
            x_factor *= -1;
            y_factor *= -1;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  criteria  DOCUMENT ME!
     */
    public void processStimulus(Enumeration criteria) {
        WakeupCriterion wakeup;
        AWTEvent[] event;
        int id;
        int dx, dy;
        int iCallback = -1; /* set to -1 to indicate neither left or right
                             * mouse rotation occurred, so no need to call the
                             * user-defined callback */
        System.err.println("process stimulus MouseRotate");
        while (criteria.hasMoreElements()) {
            wakeup = (WakeupCriterion) criteria.nextElement();

            if (wakeup instanceof WakeupOnAWTEvent) {
                event = ((WakeupOnAWTEvent) wakeup).getAWTEvent();

                for (int i = 0; i < event.length; i++) {
                    processMouseEvent((MouseEvent) event[i]);

                    if (((buttonPress) && ((flags & MANUAL_WAKEUP) == 0)) ||
                            ((wakeUp) && ((flags & MANUAL_WAKEUP) != 0))) {
                        id = event[i].getID();

                        if (
                                /* RightMouse Rotation: */
                                ((id == MouseEvent.MOUSE_DRAGGED) && ((MouseEvent) event[i]).isMetaDown() &&
                                     !((MouseEvent) event[i]).isAltDown()) ||

                                /* LeftMouse Rotation: */
                                ((id == MouseEvent.MOUSE_DRAGGED) && !((MouseEvent) event[i]).isMetaDown() && 
                                		((MouseEvent) event[i]).isControlDown() &&
                                     !((MouseEvent) event[i]).isAltDown())) {
                            x = ((MouseEvent) event[i]).getX();
                            y = ((MouseEvent) event[i]).getY();

                            dx = x - x_last;
                            dy = y - y_last;

                            if (!reset) {
                                x_angle = dy * y_factor;
                                y_angle = dx * x_factor;

                                transformX.rotX(x_angle);
                                transformY.rotY(y_angle);

                                transformGroup.getTransform(currXform);

                                // Use this to hold original matrix
                                Matrix4d mat = new Matrix4d();

                                // Remember old matrix
                                currXform.get(mat);

                                /* The Probe is rotated with a trackball
                                 * rotation model. Because the probe is displayed within the object space of the 3
                                 * orthogonal slices, which are also rotated by trackball rotation, the two trackball
                                 * transformations must be independent of each other, so that the probe rotation works
                                 * regardless of whether it and the 3 orthogonal slices have been rotated or
                                 * not. */
                                Transform3D VPTG_T3D = new Transform3D();

                                if (behaviorfix == true) {

                                    // Use this Transform3D to hold Viewer's Transform Group data
                                    Transform3D kIncremental = new Transform3D();

                                    // Get the T3D of the Viewer's TG
                                    ViewerTG.getTransform(VPTG_T3D);

                                    currXform.mul(VPTG_T3D, currXform);
                                }

                                // Set Translation to origin so multiplications don't impact it
                                currXform.setTranslation(new Vector3d(0.0, 0.0, 0.0));

                                // Perform rotations like we were looking normal to the XY plane
                                if (invert) {
                                    currXform.mul(currXform, transformX);
                                    currXform.mul(currXform, transformY);
                                } else {
                                    currXform.mul(transformX, currXform);
                                    currXform.mul(transformY, currXform);
                                }

                                if (behaviorfix == true) {

                                    // Now that the rotations are applied correctly we need to reapply Viewer's T3D
                                    // Invert the Viewer's T3D so we can add it back in to the objects Transform
                                    VPTG_T3D.invert();

                                    // Multiply the original Viewer T3D to factor it back in to the objects Transform
                                    currXform.mul(VPTG_T3D, currXform);
                                }

                                // Set old translation back
                                Vector3d translation = new Vector3d(mat.m03, mat.m13, mat.m23);

                                currXform.setTranslation(translation);

                                // Update xform
                                transformGroup.setTransform(currXform);

                                transformChanged(currXform);

                                if ((id == MouseEvent.MOUSE_DRAGGED) && !((MouseEvent) event[i]).isMetaDown() &&
                                        !((MouseEvent) event[i]).isAltDown()) {

                                    /* If this is rotation with the LeftMouse,
                                     * pass that along: */
                                    iCallback = MouseBehaviorCallback.ROTATE_LEFTBUTTON;
                                } else if ((id == MouseEvent.MOUSE_DRAGGED) && ((MouseEvent) event[i]).isMetaDown() &&
                                               !((MouseEvent) event[i]).isAltDown()) {
                                    iCallback = MouseBehaviorCallback.ROTATE;
                                }
                            } else {
                                reset = false;
                            }

                            x_last = x;
                            y_last = y;
                        }

                        if (id == MouseEvent.MOUSE_PRESSED) {
                            x_last = ((MouseEvent) event[i]).getX();
                            y_last = ((MouseEvent) event[i]).getY();
                        }
                    }
                }

                /* Call the callback only when all the mouse events have been
                 * processed -- so the mouse doesn't get too far ahead of the
                 * rotation callback, which is expensive and slow. */
                if ((callback != null) && (iCallback != -1)) {
                    callback.transformChanged(iCallback, currXform);
                }

            }
        }

        wakeupOn(mouseCriterion);
    }

    /**
     * Set the x-axis amd y-axis movement multipler with factor.
     *
     * @param  factor  DOCUMENT ME!
     */
    public void setFactor(double factor) {
        x_factor = y_factor = factor;
    }

    /**
     * Set the x-axis amd y-axis movement multipler with xFactor and yFactor respectively.
     *
     * @param  xFactor  DOCUMENT ME!
     * @param  yFactor  DOCUMENT ME!
     */
    public void setFactor(double xFactor, double yFactor) {
        x_factor = xFactor;
        y_factor = yFactor;
    }

    /**
     * The transformChanged method in the callback class will be called every time the transform is updated.
     *
     * @param  callback  DOCUMENT ME!
     */
    public void setupCallback(MouseBehaviorCallback callback) {
        this.callback = callback;
    }

    /**
     * Users can overload this method which is called every time the Behavior updates the transform. Default
     * implementation does nothing
     *
     * @param  transform  DOCUMENT ME!
     */
    public void transformChanged(Transform3D transform) { }
}
