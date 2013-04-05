package gov.nih.mipav.view.renderer.J3D.surfaceview;


import com.sun.j3d.utils.behaviors.mouse.*;

import java.awt.*;
import java.awt.event.*;

import javax.media.j3d.*;


/**
 * MouseRotateExt extends the MouseRotate class by providing public access to certain states of the MouseBehavior that
 * are stored as protected values with no other public accesss.
 *
 * <p>Also overrides the mouseReleased routine so that the callback can be notified one more time of the updated
 * transform, this time with the buttonPress flag being set to false.</p>
 */

public class MouseRotateExt extends MouseRotate {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Mouse behavior callback reference. */
    protected MouseBehaviorCallback callback = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a default mouse rotate behavior.
     */
    public MouseRotateExt() {
        super(0);
    }

    /**
     * Creates a rotate behavior given the transform group.
     *
     * @param  transformGroup  The transformGroup to operate on.
     */
    public MouseRotateExt(TransformGroup transformGroup) {
        super(transformGroup);
    }

    /**
     * Creates a rotate behavior. Note that this behavior still needs a transform group to work on (use
     * setTransformGroup(tg)) and the transform group must add this behavior.
     *
     * @param  flags  interesting flags (wakeup conditions).
     */
    public MouseRotateExt(int flags) {
        super(flags);
    }

    /**
     * Creates a rotate behavior that uses AWT listeners and behavior posts rather than WakeupOnAWTEvent. The behavior
     * is added to the specified Component. A null component can be passed to specify the behavior should use listeners.
     * Components can then be added to the behavior with the addListener(Component c) method.
     *
     * @param  c  The Component to add the MouseListener and MouseMotionListener to.
     *
     * @since  Java 3D 1.2.1
     */
    public MouseRotateExt(Component c) {
        super(c, 0);
    }

    /**
     * Creates a rotate behavior that uses AWT listeners and behavior posts rather than WakeupOnAWTEvent. The behaviors
     * is added to the specified Component and works on the given TransformGroup. A null component can be passed to
     * specify the behavior should use listeners. Components can then be added to the behavior with the
     * addListener(Component c) method.
     *
     * @param  c               The Component to add the MouseListener and MouseMotionListener to.
     * @param  transformGroup  The TransformGroup to operate on.
     *
     * @since  Java 3D 1.2.1
     */
    public MouseRotateExt(Component c, TransformGroup transformGroup) {
        super(c, transformGroup);
    }

    /**
     * Creates a rotate behavior that uses AWT listeners and behavior posts rather than WakeupOnAWTEvent. The behavior
     * is added to the specified Component. A null component can be passed to specify the behavior should use listeners.
     * Components can then be added to the behavior with the addListener(Component c) method. Note that this behavior
     * still needs a transform group to work on (use setTransformGroup(tg)) and the transform group must add this
     * behavior.
     *
     * @param  c      The Component to add the MouseListener and MouseMotionListener to.
     * @param  flags  interesting flags (wakeup conditions).
     *
     * @since  Java 3D 1.2.1
     */
    public MouseRotateExt(Component c, int flags) {
        super(c, flags);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return whether the button press state is set which can be used as an indication of whether or not the associated
     * TransformGroup instance is being interactively updated.
     *
     * @return  boolean True if the button press state is true.
     */
    public boolean isButtonPressed() {
        return super.buttonPress;
    }

    /**
     * Return indication of whether or not this behavior is enabled.
     *
     * @return  boolean True if this behavior is enabled.
     */
    public boolean isEnabled() {
        return super.enable;
    }

    /**
     * Override mouse release so that we can force another update of the current transform to the callback.
     *
     * @param  e  MouseEvent
     */
    public void mouseReleased(MouseEvent e) {

        super.mouseReleased(e);
        buttonPress = false;

        // Force another notification of an update.  This must be done
        // after the base class handles the event because it will the
        // callback will see the buttonPress flag clear.
        if ((callback != null) && !e.isAltDown() && !e.isMetaDown()) {
            callback.transformChanged(MouseBehaviorCallback.ROTATE, currXform);
        }
    }

    /**
     * The transformChanged method in the callback class will be called every time the transform is updated.
     *
     * @param  callback  mouse call back reference.
     */
    public void setupCallback(MouseBehaviorCallback callback) {
        this.callback = callback;
        super.setupCallback(callback);
    }
}
