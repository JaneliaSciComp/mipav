package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.mouse;


import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.media.j3d.*;


/**
 * Converting a copy of MouseRotation to be used to with picking (PickSelection). Major changes in mouse event criteria
 * and added condition to call callback.transformClicked Used this to try the Properties behaviors then reverted back
 * since it worked
 */
public class MouseSelection extends MouseBehavior {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    double x_angle, y_angle;

    /** DOCUMENT ME! */
    double x_factor = .03;

    /** DOCUMENT ME! */
    double y_factor = .03;

    /** DOCUMENT ME! */
    private MouseBehaviorCallback callback = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a default mouse Selection behavior.
     */
    public MouseSelection() {
        super(0);
    }

    /**
     * Creates a rotate behavior given the transform group.
     *
     * @param  transformGroup  The transformGroup to operate on.
     */
    public MouseSelection(TransformGroup transformGroup) {
        super(transformGroup);
    }

    /**
     * Creates a Selection behavior. Note that this behavior still needs a transform group to work on (use
     * setTransformGroup(tg)) and the transform group must add this behavior.
     *
     * @param  flags  interesting flags (wakeup conditions).
     */
    public MouseSelection(int flags) {
        super(flags);
    }

    /**
     * Same as above but with boolean fix.
     *
     * @param  flags        DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseSelection(int flags, boolean behaviorfix) {
        super(flags, behaviorfix);
    }

    /**
     * Same as above but with Viewer Transform Group and boolean fix.
     *
     * @param  flags        DOCUMENT ME!
     * @param  VPTG         DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseSelection(int flags, TransformGroup VPTG, boolean behaviorfix) {
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

                        if ((id == MouseEvent.MOUSE_DRAGGED) && !((MouseEvent) event[i]).isMetaDown() &&
                                !((MouseEvent) event[i]).isAltDown()) {

                            if (!reset) {
                                transformChanged(currXform);

                                if (callback != null) {
                                    callback.transformChanged(MouseBehaviorCallback.SELECTION, currXform);
                                }
                            } else {
                                reset = false;
                            }

                            x_last = ((MouseEvent) event[i]).getX();
                            y_last = ((MouseEvent) event[i]).getY();
                        } else if ((id == MouseEvent.MOUSE_PRESSED) && !((MouseEvent) event[i]).isMetaDown() &&
                                       !((MouseEvent) event[i]).isAltDown()) {

                            /*
                             * if (!reset) { if (callback!=null) { callback.transformClicked(
                             * MouseBehaviorCallback.SELECTION, currXform); } } else { reset = false; } */
                            if (reset) {
                                reset = false;
                            }

                            x_last = ((MouseEvent) event[i]).getX();
                            y_last = ((MouseEvent) event[i]).getY();
                        } else if (id == MouseEvent.MOUSE_PRESSED) {
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
