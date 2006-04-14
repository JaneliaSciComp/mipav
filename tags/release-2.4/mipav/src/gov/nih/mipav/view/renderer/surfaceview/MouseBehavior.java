package gov.nih.mipav.view.renderer.surfaceview;


import java.awt.event.*;
import java.awt.*;

import com.sun.j3d.utils.behaviors.vp.*;
import com.sun.j3d.utils.behaviors.mouse.MouseBehaviorCallback;
import javax.media.j3d.*;
import javax.vecmath.Vector3f;
import java.awt.geom.Point2D.Float;


/**
 *  Abstract class which implements much of the event tracking and
 *  state updating shared between Fly and Pointer behavior.
 *
 *  Canvas3D size changes are tracked
 *
 *  @author Paul Byrne
 */
public abstract class MouseBehavior extends ViewPlatformBehavior {

    /** JPanel view reference. */
    protected JPanelView parent;

    /** Awt wake up condition .*/
    protected WakeupOr awtCondition;

    /** wake up condition for awt, component and mouse. */
    protected WakeupOr bothCondition;

    /** Canvas boundary. */
    protected Rectangle canvasBounds;

    /** Canvas center. */
    protected Float canvasCenter;

    /** Target transform. */
    protected Transform3D targetTransform;

    /** Mouse behavior callback. */
    protected MouseBehaviorCallback mouseCallback = null;

    /** Maxium rotation angle. */
    protected float MAX_ANGLE = (float) Math.toRadians(1);

    /** Maxium rotation velocity. */
    protected float MAX_VELOCITY = 1f;

    /** Motion, standard, fly thrugh, point. */
    protected boolean motion = false;

    /** flag to invoke the mouse motion event or not. */
    protected boolean ignoreMouseMotion = false;

    /** Transfrom velocity. */
    protected Transform3D velocityTransform;

    /** Yaw transform. */
    protected Transform3D yawTransform;

    /** Spin transform. */
    protected Transform3D pitchTransform;

    /** Row transform. */
    protected Transform3D spinTransform;

    /** New target transform. */
    protected Transform3D newTargetTransform = new Transform3D();

    /** Rotation yaw angle. */
    protected float yawAngle = 0f;

    /** Rotation row angle. */
    protected float spinAngle = 0f;

    /** Rotation pitch angle. */
    protected float pitchAngle = 0f;

    /** Velocity vector. */
    protected Vector3f velocity = new Vector3f();

    /** Target transform group. */
    protected TransformGroup targetTG;

    /** Canvas rescale factor. */
    protected float deadFactor;

    /** Canvas resacled X size. */
    protected float deadXSize;

    /** Canvs rescaled Y size. */
    protected float deadYSize;

    /**
     *  Creates new parent class with the dialog set
     *  for easy access between the user and the behaviors.
     *  @param parent   Parent dialog.
     */
    public MouseBehavior(JPanelView parent) {
        this.parent = parent;
        init();
        canvasCenter = new Float();
    }

    /**
     *  Sets the maximum velocity of the mouse movement.
     *	@param max	Value to set maximum to.
     */
    public void setMaximumVelocity(float max) {
        MAX_VELOCITY = max;
    }

    /**
     *	Gets the canvas center.  Needed for the mouse control buttons to
     *	properly set up the mouse event.
     *	@return	The canvas center.
     */
    public Float getCanvasCenter() {
        return canvasCenter;
    }

    /**
     *	Sets the maximum angle of the mouse movement.
     *	@param angle	Angle to set maximum to.
     */
    public void setMaximumAngle(float angle) {
        MAX_ANGLE = angle;
    }

    /**
     *  Sets the TransformGroup on which this behavior acts.
     *  @param targetTG Target transform group to set to.
     */
    public void setTarget(TransformGroup targetTG) {
        this.targetTG = targetTG;
    }

    /**
     *  Returns the TransformGroup this behavior acts on.
     *  @return The transform group this behavior acts on.
     */
    public TransformGroup getTarget() {
        return targetTG;
    }

    /**
     *  Initializes wake up events.
     */
    private void init() {

        // For some reason having MOUSE_MOTION and MOUSE events in the same wakeup
        // results in MOUSE events being lost
        WakeupOnAWTEvent awt1 = new WakeupOnAWTEvent(
                AWTEvent.MOUSE_MOTION_EVENT_MASK | AWTEvent.COMPONENT_EVENT_MASK);
        WakeupOnAWTEvent awt2 = new WakeupOnAWTEvent(AWTEvent.MOUSE_EVENT_MASK);
        WakeupOnAWTEvent awt3 = new WakeupOnAWTEvent(AWTEvent.KEY_EVENT_MASK);
        WakeupOnElapsedFrames frameCondition = new WakeupOnElapsedFrames(0);

        bothCondition = new WakeupOr(
                new WakeupCriterion[] { awt1, awt2, awt3, frameCondition });
        awtCondition = new WakeupOr(new WakeupCriterion[] { awt1, awt2, awt3 });
    }

    /**
     *  Required to be a ViewPlatformBehavior.  Creates new transforms
     *  and calls wakeupOn.
     */
    public void initialize() {
        pitchTransform = new Transform3D();
        yawTransform = new Transform3D();
        spinTransform = new Transform3D();
        targetTransform = new Transform3D();
        velocityTransform = new Transform3D();
        wakeupOn(awtCondition);
    }

    /**
     *  Required to be a ViewPlatformBehavior.  Processes a list of
     *  events, passing them down to the processMouseEvent and integrateTransforms
     *  methods.
     *  @param en List of events to process.
     */
    public void processStimulus(java.util.Enumeration en) {
        while (en.hasMoreElements()) {
            WakeupCondition wakeup = (WakeupCondition) en.nextElement();

            if (wakeup instanceof WakeupOnAWTEvent) {
                processAWTEvents(((WakeupOnAWTEvent) wakeup).getAWTEvent());
            } else if (wakeup instanceof WakeupOnElapsedFrames) {
                integrateTransforms();
            }
        }

        if (motion) {
            wakeupOn(bothCondition);
        } else {
            wakeupOn(awtCondition);
        }
    }

    /**
     *  Processes an array of AWT events by calling processMouseEvent;
     *  or, if the user pressed ESC, disables certain behaviors.
     *  @param events   List of AWT events to process.
     */
    private void processAWTEvents(java.awt.AWTEvent[] events) {

        for (int i = 0; i < events.length; i++) {
            if (events[i] instanceof MouseEvent) {
                if ((events[i].getID() == MouseEvent.MOUSE_DRAGGED
                        || events[i].getID() == MouseEvent.MOUSE_PRESSED
                        || events[i].getID() == MouseEvent.MOUSE_RELEASED)
                                && !ignoreMouseMotion) {
                    processMouseEvent((MouseEvent) events[i]);
                } else if (events[i].getID() == MouseEvent.MOUSE_ENTERED) {
                    Component component = ((MouseEvent) events[i]).getComponent();

                    if (component instanceof javax.media.j3d.Canvas3D) {
                        canvasBounds = component.getBounds(canvasBounds);
                        canvasCenter.x = canvasBounds.width / 2;
                        canvasCenter.y = canvasBounds.height / 2;
                        deadXSize = deadFactor / canvasCenter.x;
                        deadYSize = deadFactor / canvasCenter.y;
                        if (!ignoreMouseMotion) {
                            component.setCursor(
                                    Cursor.getPredefinedCursor(
                                            Cursor.CROSSHAIR_CURSOR));
                        }
                    }
                } else if (events[i].getID() == MouseEvent.MOUSE_EXITED) {
                    Component component = ((MouseEvent) events[i]).getComponent();

                    if (component instanceof javax.media.j3d.Canvas3D) {
                        component.setCursor(Cursor.getDefaultCursor());
                    }
                }
            } else if (events[i] instanceof KeyEvent) {
                int keyCode = ((KeyEvent) events[i]).getKeyCode();
                int id = ((KeyEvent) events[i]).getID();

                if (keyCode == KeyEvent.VK_ESCAPE && id == KeyEvent.KEY_RELEASED) {
                    ignoreMouseMotion = !ignoreMouseMotion;
                    Component component = ((KeyEvent) events[i]).getComponent();

                    if (ignoreMouseMotion) {
                        component.setCursor(Cursor.getDefaultCursor());
                        parent.setFlyEnabled(false);
                    } else {
                        component.setCursor(
                                Cursor.getPredefinedCursor(
                                        Cursor.CROSSHAIR_CURSOR));
                        parent.setFlyEnabled(true);
                    }
                }
            } else if (events[i] instanceof ComponentEvent) {
                if (events[i].getID() == ComponentEvent.COMPONENT_RESIZED) {
                    canvasBounds = ((ComponentEvent) events[i]).getComponent().getBounds(
                            canvasBounds);
                    canvasCenter.x = canvasBounds.width / 2f;
                    canvasCenter.y = canvasBounds.height / 2f;
                    deadXSize = deadFactor / canvasCenter.x;
                    deadYSize = deadFactor / canvasCenter.y;
                }
            }
        }
    }

    /**
     *  Process the mouse event.  This is called every time an event
     *  arrives.
     *  @param evt  Event to process.
     */
    protected abstract void processMouseEvent(MouseEvent evt);

    /**
     *	Indicates whether or not the canvas is ignoring mouse events.
     *	@return	<code>true</code> indicates the canvas is ignoring mouse events,
     *			<code>false</code> that they should be processed as usual.
     */
    public boolean isIgnoreMouseMotion() {
        return ignoreMouseMotion;
    }

    /**
     *   Creates a new transform and sets it in the canvas.
     */
    protected abstract void integrateTransforms();

    /**
     *  Sets who the behavior calls back to.
     *  @param callback Place to call back to.
     */
    public void setupCallback(MouseBehaviorCallback callback) {
        mouseCallback = callback;
    }

}

