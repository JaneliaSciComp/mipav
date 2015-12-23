package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.mouse;


import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * MouseTranslate is a Java3D behavior object that lets users control the translation (X, Y) of an object via a mouse
 * drag motion with the third mouse button (alt-click on PC). See MouseRotate for similar usage info. Also this: < id =
 * event[i].getID(); < if ((id == MouseEvent.MOUSE_DRAGGED) && < !((MouseEvent)event[i]).isAltDown() && <
 * ((MouseEvent)event[i]).isMetaDown()){ > id = event[i].getID(); > if ((id == MouseEvent.MOUSE_DRAGGED) && >
 * ((MouseEvent)event[i]).isAltDown() && > !((MouseEvent)event[i]).isMetaDown()){ Note: By changing the bangs (!), you
 * control which of the mouse buttons are filtered using the Alt and Meta key nomenclature. The old if statement says if
 * the mouse is dragged and the right button and not the middle button is pressed. The new if statement says if the
 * mouse is dragged and the middle button and not the right button is pressed. Added condition for the mouse pressed
 * Adjusted the scale factors from 0.02 to 0.01 double x_factor = .01; double y_factor = .01; Added the ability to
 * translate based on user rotations and positions
 */
public class MouseTranslation extends MouseBehavior {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    Vector3d translation = new Vector3d();

    /** DOCUMENT ME! */
    double x_factor = 0.05;

    /** DOCUMENT ME! */
    double y_factor = 0.05;

    /** DOCUMENT ME! */
    private MouseBehaviorCallback callback = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a default translate behavior.
     */
    public MouseTranslation() {
        super(0);
    }

    /**
     * Creates a mouse translate behavior given the transform group.
     *
     * @param  transformGroup  The transformGroup to operate on.
     */
    public MouseTranslation(TransformGroup transformGroup) {
        super(transformGroup);
    }

    /**
     * Creates a translate behavior. Note that this behavior still needs a transform group to work on (use
     * setTransformGroup(tg)) and the transform group must add this behavior.
     *
     * @param  flags  DOCUMENT ME!
     */
    public MouseTranslation(int flags) {
        super(flags);
    }

    /**
     * Same as above but with boolean fix.
     *
     * @param  flags        DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseTranslation(int flags, boolean behaviorfix) {
        super(flags, behaviorfix);
    }

    /**
     * Same as above but with Viewer Transform Group and boolean fix.
     *
     * @param  flags        DOCUMENT ME!
     * @param  VPTG         DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseTranslation(int flags, TransformGroup VPTG, boolean behaviorfix) {
        super(flags, VPTG, behaviorfix);
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

        while (criteria.hasMoreElements()) {
            wakeup = (WakeupCriterion) criteria.nextElement();

            if (wakeup instanceof WakeupOnAWTEvent) {
                event = ((WakeupOnAWTEvent) wakeup).getAWTEvent();

                for (int i = 0; i < event.length; i++) {
                    processMouseEvent((MouseEvent) event[i]);

                    if (((buttonPress) && ((flags & MANUAL_WAKEUP) == 0)) ||
                            ((wakeUp) && ((flags & MANUAL_WAKEUP) != 0))) {
                        id = event[i].getID();

                        if ((id == MouseEvent.MOUSE_DRAGGED) && ((MouseEvent) event[i]).isShiftDown() &&
                                !((MouseEvent) event[i]).isMetaDown()) {
                            x = ((MouseEvent) event[i]).getX();
                            y = ((MouseEvent) event[i]).getY();

                            dx = x - x_last;
                            dy = y - y_last;

                            if ((!reset) && ((Math.abs(dy) < 50) && (Math.abs(dx) < 50))) {

                                // System.out.println("dx " + dx + " dy " + dy);
                                transformGroup.getTransform(currXform);

                                // Use this Transform3D to hold Viewer's Transform Group data
                                Transform3D VPTG_T3D = new Transform3D();

                                if (behaviorfix == true) {

                                    // Get the T3D of the Viewer's TG
                                    ViewerTG.getTransform(VPTG_T3D);

                                    // Set Translation to origin so multiplications don't impact it
                                    VPTG_T3D.setTranslation(new Vector3d(0.0, 0.0, 0.0));

                                    // Invert the Viewer's T3D so we can remove it from the objects Transform
                                    VPTG_T3D.invert();

                                    // Multiply the inverted Viewer T3D to factor it out of the objects Transform
                                    currXform.mul(VPTG_T3D, currXform);

                                    // Vector3f Angles = Chat3D.getRotAngle(VPTG_T3D);

                                    // System.out.println("\nViewer:"); Chat3D.printTransformGroups_T3D(ViewerTG);
                                    // System.out.println("\nAngles:"); System.out.println(Angles.x + " " + Angles.y + "
                                    // " + Angles.z); System.out.println("\nObject");
                                    // Chat3D.printTransformGroups_T3D(transformGroup);

                                    // If y axis rotation is gimbal lock if ((Angles.y == (float)(Math.PI/2.0)) ||
                                    // (Angles.y == -(float)(Math.PI/2.0))) { translation.x =
                                    // dx*x_factor*(Math.cos(-Angles.y)*Math.cos(-Angles.z)) +
                                    // dy*y_factor*(-Math.sin(-Angles.z)); translation.y =
                                    // dx*x_factor*Math.sin(-Angles.z) +
                                    // dy*y_factor*(Math.cos(-Angles.x)*Math.cos(-Angles.z)); translation.z =
                                    // dx*x_factor*Math.sin(-Angles.y) + dy*y_factor*Math.sin(-Angles.x); } else  // if
                                    // not gimbal lock { translation.x =
                                    // dx*x_factor*(Math.cos(-Angles.y)*Math.cos(-Angles.z)) +
                                    // dy*y_factor*(-Math.sin(-Angles.z)); translation.y =
                                    // -dx*x_factor*Math.sin(-Angles.z) +
                                    // -dy*y_factor*(Math.cos(-Angles.x)*Math.cos(-Angles.z)); translation.z =
                                    // dx*x_factor*Math.sin(-Angles.y) + dy*y_factor*Math.sin(-Angles.x); }

                                    // translation.x = dx*x_factor*Math.cos(-Angles.y)*Math.cos(-Angles.z) +
                                    // dy*y_factor*(-Math.sin(-Angles.z)); translation.y =
                                    // -dx*x_factor*Math.sin(-Angles.z) +
                                    // -dy*y_factor*Math.cos(-Angles.x)*Math.cos(-Angles.z); translation.z =
                                    // dx*x_factor*Math.sin(-Angles.y) + dy*y_factor*Math.sin(-Angles.x);
                                }

                                // Perform rotations like we were looking normal to the XY plane
                                translation.x = dx * x_factor;
                                translation.y = -dy * y_factor;
                                transformX.set(translation);

                                if (invert) {
                                    currXform.mul(currXform, transformX);
                                } else {
                                    currXform.mul(transformX, currXform);
                                }

                                if (behaviorfix == true) {

                                    // Now that the rotations are applied correctly we need to reapply Viewer's T3D
                                    // Invert the Viewer's T3D so we can add it back in to the objects Transform
                                    VPTG_T3D.invert();

                                    // Multiply the original Viewer T3D to factor it back in to the objects Transform
                                    currXform.mul(VPTG_T3D, currXform);
                                }

                                transformGroup.setTransform(currXform);

                                transformChanged(currXform);

                                if (callback != null) {
                                    callback.transformChanged(MouseBehaviorCallback.TRANSLATE, currXform);
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
