package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.mouse;


import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * DOCUMENT ME!
 */
public class MouseOrbit extends MouseBehavior {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    double x_angle, y_angle;

    /** DOCUMENT ME! */
    double x_factor = 0.01; // .03;

    /** DOCUMENT ME! */
    double y_factor = 0.01; // .03;

    /** DOCUMENT ME! */
    private MouseBehaviorCallback callback = null;

    /** DOCUMENT ME! */
    private boolean sameflag = false;

    /** DOCUMENT ME! */
    private TransformGroup tg;

    /** DOCUMENT ME! */
    private TransformGroup tg_ghost;

    /** DOCUMENT ME! */
    private TransformGroup VPTG_ghost;

    /** DOCUMENT ME! */
    private Transform3D VPTG_ghost_T3D;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new MouseOrbit object.
     */
    public MouseOrbit() {
        super(0);
    }

    /**
     * Creates a new MouseOrbit object.
     *
     * @param  transformGroup  DOCUMENT ME!
     */
    public MouseOrbit(TransformGroup transformGroup) {
        super(transformGroup);
    }

    /**
     * Creates a new MouseOrbit object.
     *
     * @param  flags  DOCUMENT ME!
     */
    public MouseOrbit(int flags) {
        super(flags);
    }

    /**
     * Creates a new MouseOrbit object.
     *
     * @param  flags        DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseOrbit(int flags, boolean behaviorfix) {
        super(flags, behaviorfix);
    }

    /**
     * Creates a new MouseOrbit object.
     *
     * @param  flags        DOCUMENT ME!
     * @param  VPTG         DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseOrbit(int flags, TransformGroup VPTG, boolean behaviorfix) {
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

        while (criteria.hasMoreElements()) {
            wakeup = (WakeupCriterion) criteria.nextElement();

            if (wakeup instanceof WakeupOnAWTEvent) {
                event = ((WakeupOnAWTEvent) wakeup).getAWTEvent();

                for (int i = 0; i < event.length; i++) {
                    processMouseEvent((MouseEvent) event[i]);

                    if (((buttonPress) && ((flags & MANUAL_WAKEUP) == 0)) ||
                            ((wakeUp) && ((flags & MANUAL_WAKEUP) != 0))) {
                        id = event[i].getID();

                        if ((id == MouseEvent.MOUSE_DRAGGED) && ((MouseEvent) event[i]).isMetaDown() &&
                                !((MouseEvent) event[i]).isAltDown()) {
                            x = ((MouseEvent) event[i]).getX();
                            y = ((MouseEvent) event[i]).getY();

                            dx = x - x_last;
                            dy = y - y_last;

                            if (!reset) {
                                Transform3D tempT3D = new Transform3D();
                                Transform3D orbitT3D = new Transform3D();

                                tempT3D.rotX(-dy * y_factor);
                                orbitT3D.mul(tempT3D);
                                tempT3D.rotY(-dx * x_factor);
                                orbitT3D.mul(tempT3D);

                                Transform3D tg_ghost_T3D = new Transform3D();

                                tg_ghost.getTransform(tg_ghost_T3D);

                                Vector3f tg_ghost_vec3f = new Vector3f();

                                tg_ghost_T3D.get(tg_ghost_vec3f); // Save the tg_ghost_vec3f for later

                                Matrix4d tg_ghost_mat4d = new Matrix4d();

                                tg_ghost_T3D.get(tg_ghost_mat4d); // Save the tg_ghost_mat4d for later

                                Transform3D VPTG_ghost_T3D_inverted = new Transform3D();
                                Transform3D VPTG_ghost_T3D_noninverted = new Transform3D();

                                (super.ViewerTG).getTransform(VPTG_ghost_T3D_inverted);
                                (super.ViewerTG).getTransform(VPTG_ghost_T3D_noninverted);
                                VPTG_ghost_T3D_inverted.setTranslation(new Vector3d(0.0, 0.0, 0.0));
                                VPTG_ghost_T3D_noninverted.setTranslation(new Vector3d(0.0, 0.0, 0.0));

                                // Invert the Viewer's T3D so we can remove it from the objects Transform
                                VPTG_ghost_T3D_inverted.invert();

                                // Multiply the inverted Viewer Ghost T3D to factor it out of the objects Transform
                                // orbitT3D.mul(VPTG_ghost_T3D_inverted, orbitT3D);
                                tg_ghost_T3D.mul(VPTG_ghost_T3D_inverted, tg_ghost_T3D);

                                tg_ghost_T3D.setTranslation(new Vector3d(0.0, 0.0, 0.0));

                                if (invert) {
                                    tg_ghost_T3D.mul(tg_ghost_T3D, orbitT3D);
                                } else {
                                    tg_ghost_T3D.mul(orbitT3D, tg_ghost_T3D);
                                }

                                // Multiply the noninverted Viewer Ghost T3D to factor it back into the objects
                                // Transform
                                tg_ghost_T3D.mul(VPTG_ghost_T3D_noninverted, tg_ghost_T3D);

                                tg_ghost_T3D.setTranslation(tg_ghost_vec3f);
                                tg_ghost.setTransform(tg_ghost_T3D); // Needs to accumulate for next drag

                                // tg.setTransform(tg_ghost_T3D);

                                // Now do this for the viewer

                                VPTG_ghost_T3D = new Transform3D();
                                (super.ViewerTG).getTransform(VPTG_ghost_T3D);

                                Vector3f VPTG_ghost_vec3f = new Vector3f();

                                VPTG_ghost_T3D.get(VPTG_ghost_vec3f); // Save the VPTG_ghost_vec3f for later

                                Vector3f temp_vec3f = new Vector3f();

                                temp_vec3f.x = VPTG_ghost_vec3f.x - tg_ghost_vec3f.x;
                                temp_vec3f.y = VPTG_ghost_vec3f.y - tg_ghost_vec3f.y;
                                temp_vec3f.z = VPTG_ghost_vec3f.z - tg_ghost_vec3f.z;
                                VPTG_ghost_T3D.setTranslation(temp_vec3f);

                                VPTG_ghost_T3D.mul(VPTG_ghost_T3D_inverted, VPTG_ghost_T3D);

                                if (invert) {
                                    VPTG_ghost_T3D.mul(VPTG_ghost_T3D, orbitT3D);
                                } else {
                                    VPTG_ghost_T3D.mul(orbitT3D, VPTG_ghost_T3D);
                                }

                                VPTG_ghost_T3D.mul(VPTG_ghost_T3D_noninverted, VPTG_ghost_T3D);

                                VPTG_ghost_T3D.get(temp_vec3f);
                                temp_vec3f.x = temp_vec3f.x + tg_ghost_vec3f.x;
                                temp_vec3f.y = temp_vec3f.y + tg_ghost_vec3f.y;
                                temp_vec3f.z = temp_vec3f.z + tg_ghost_vec3f.z;
                                VPTG_ghost_T3D.setTranslation(temp_vec3f);

                                (super.ViewerTG).setTransform(VPTG_ghost_T3D);

                                transformChanged(currXform);

                                if (callback != null) {
                                    callback.transformChanged(MouseBehaviorCallback.ORBIT, currXform);
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
     * DOCUMENT ME!
     *
     * @param  tg    DOCUMENT ME!
     * @param  VPTG  DOCUMENT ME!
     */
    public void setTransformGroups(TransformGroup tg, TransformGroup VPTG) {
        this.tg = tg;
        super.ViewerTG = VPTG;

        tg_ghost = new TransformGroup();

        Transform3D tgT3D = new Transform3D();

        tg.getTransform(tgT3D);
        tg_ghost.setTransform(tgT3D); // Make a ghost TG since no transform on object is to occur
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
