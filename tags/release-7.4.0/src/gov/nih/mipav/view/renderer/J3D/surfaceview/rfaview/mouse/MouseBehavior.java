package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.mouse;


import java.awt.event.*;

import java.util.*;

import javax.media.j3d.*;


/**
 * <p>Title: MouseBehavior</p>
 *
 * <p>Description: Base class for all mouse manipulators (see MouseRotate, MouseZoom and MouseTranslate for examples of
 * how to extend this base class). Added protected TransformGroup ViewerTG to hold the viewers position</p>
 */
public abstract class MouseBehavior extends Behavior {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Set this flag if you want to manually wakeup the behavior. */
    public static final int MANUAL_WAKEUP = 0x1;

    /**
     * Set this flag if you want to invert the inputs. This is useful when the transform for the view platform is being
     * changed instead of the transform for the object.
     */
    public static final int INVERT_INPUT = 0x2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public boolean behaviorfix;

    /** DOCUMENT ME! */
    protected boolean buttonPress = false;

    /** DOCUMENT ME! */
    protected Transform3D currXform;

    /** DOCUMENT ME! */
    protected int flags = 0;

    /** DOCUMENT ME! */
    protected boolean invert = false;

    /** DOCUMENT ME! */
    protected WakeupOr mouseCriterion;

    /** DOCUMENT ME! */
    protected WakeupCriterion[] mouseEvents;

    /** DOCUMENT ME! */
    protected boolean reset = false;

    /** DOCUMENT ME! */
    protected TransformGroup transformGroup;

    /** DOCUMENT ME! */
    protected Transform3D transformX;

    /** DOCUMENT ME! */
    protected Transform3D transformY;

    /** DOCUMENT ME! */
    protected TransformGroup ViewerTG;

    /** DOCUMENT ME! */
    protected boolean wakeUp = false;

    /** DOCUMENT ME! */
    protected int x, y;

    /** DOCUMENT ME! */
    protected int x_last, y_last;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a mouse behavior object with a given transform group.
     *
     * @param  transformGroup  The transform group to be manipulated.
     */
    public MouseBehavior(TransformGroup transformGroup) {
        super();

        // need to remove old behavior from group
        this.transformGroup = transformGroup;
        currXform = new Transform3D();
        transformX = new Transform3D();
        transformY = new Transform3D();
        reset = true;
    }

    /**
     * Initializes standard fields. Note that this behavior still needs a transform group to work on (use
     * setTransformGroup(tg)) and the transform group must add this behavior.
     *
     * @param  format  flags
     */
    public MouseBehavior(int format) {
        super();
        flags = format;
        currXform = new Transform3D();
        transformX = new Transform3D();
        transformY = new Transform3D();
        reset = true;
    }

    /**
     * Same as constructor above but with behaviorfix.
     *
     * @param  format       DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseBehavior(int format, boolean behaviorfix) {
        super();
        flags = format;
        this.behaviorfix = behaviorfix;
        currXform = new Transform3D();
        transformX = new Transform3D();
        transformY = new Transform3D();
        reset = true;
    }

    /**
     * Creates a new MouseBehavior object.
     *
     * @param  format       DOCUMENT ME!
     * @param  VPTG         DOCUMENT ME!
     * @param  behaviorfix  DOCUMENT ME!
     */
    public MouseBehavior(int format, TransformGroup VPTG, boolean behaviorfix) {
        super();
        flags = format;
        ViewerTG = VPTG;
        this.behaviorfix = behaviorfix;
        currXform = new Transform3D();
        transformX = new Transform3D();
        transformY = new Transform3D();
        reset = true;
    }

    /**
     * Creates a new MouseBehavior object.
     *
     * @param  transformGroup  DOCUMENT ME!
     * @param  VPTG            DOCUMENT ME!
     * @param  behaviorfix     DOCUMENT ME!
     */
    public MouseBehavior(TransformGroup transformGroup, TransformGroup VPTG, boolean behaviorfix) {
        super();
        this.transformGroup = transformGroup;
        ViewerTG = VPTG;
        this.behaviorfix = behaviorfix;
        currXform = new Transform3D();
        transformX = new Transform3D();
        transformY = new Transform3D();
        reset = true;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * All mouse manipulators must implement this.
     *
     * @param  criteria  DOCUMENT ME!
     */
    public abstract void processStimulus(Enumeration criteria);

    /**
     * Initializes the behavior.
     */
    public void initialize() {
        mouseEvents = new WakeupCriterion[3];
        mouseEvents[0] = new WakeupOnAWTEvent(MouseEvent.MOUSE_DRAGGED);
        mouseEvents[1] = new WakeupOnAWTEvent(MouseEvent.MOUSE_PRESSED);
        mouseEvents[2] = new WakeupOnAWTEvent(MouseEvent.MOUSE_RELEASED);
        mouseCriterion = new WakeupOr(mouseEvents);
        wakeupOn(mouseCriterion);
        x = 0;
        y = 0;
        x_last = 0;
        y_last = 0;
    }

    /**
     * Handles mouse events.
     *
     * @param  evt  DOCUMENT ME!
     */
    public void processMouseEvent(MouseEvent evt) {

        if (evt.getID() == MouseEvent.MOUSE_PRESSED) {
            buttonPress = true;

            return;
        } else if (evt.getID() == MouseEvent.MOUSE_RELEASED) {
            buttonPress = false;
            wakeUp = false;
        } else if (evt.getID() == MouseEvent.MOUSE_MOVED) { // Process mouse move event
        }
    }

    /**
     * Swap a new transformGroup replacing the old one. This allows manipulators to operate on different nodes.
     *
     * @param  transformGroup  The *new* transform group to be manipulated.
     */
    public void setTransformGroup(TransformGroup transformGroup) {

        // need to remove old behavior from group
        this.transformGroup = transformGroup;
        currXform = new Transform3D();
        transformX = new Transform3D();
        transformY = new Transform3D();
        reset = true;
    }

    /**
     * Manually wake up the behavior. If MANUAL_WAKEUP flag was set upon creation, you must wake up this behavior each
     * time it is handled.
     */
    public void wakeup() {
        wakeUp = true;
    }

}
