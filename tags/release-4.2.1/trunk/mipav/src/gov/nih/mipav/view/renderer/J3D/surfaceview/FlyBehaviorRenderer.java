package gov.nih.mipav.view.renderer.J3D.surfaceview;


import java.awt.event.*;

import javax.swing.*;


/**
 * This behavior is supposed to emulate a pilot directing an aircraft. The three mouse buttons act like a joystick.
 * Rotation is controlled with the left mouse button and the middle mouse button. Zoom is controlled with the left mouse
 * button; translation is controlled with the right mouse button.
 *
 * <p>Here are the functions:<br>
 * Left mouse button:<br>
 * Up - zooms out<br>
 * Down - zooms in<br>
 * Right - spins to the left<br>
 * Left - spins to the right<br>
 * Right mouse button:<br>
 * Up - translates down<br>
 * Down - translates up<br>
 * Right - translates left<br>
 * Left - translates right<br>
 * Middle mouse button:<br>
 * Up - pitches up<br>
 * Down - pitches down<br>
 * Right - rolls right<br>
 * Left - rolls left<br>
 * </p>
 *
 * <p>A slider in the dialog controls the speed of these changes. Also, the further away from the center of the canvas
 * the mouse event is, the faster it will occur. So if the user points the mouse with the left mouse button at the very
 * top of the canvas, the image will zoom out quickly. If the user points the mouse with the left mouse button only
 * slightly above the center of the canvas, the image will zoom out slowly.</p>
 */
public class FlyBehaviorRenderer extends MouseBehaviorRenderer {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new behavior and initializes wake up events.
     *
     * @param  parent  The dialog that called this behavior.
     */
    public FlyBehaviorRenderer(JPanelView parent) {
        super(parent);
        deadFactor = 15f;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets up the new transform by creating transforms from the determined pitch, yaw, and roll angles and multiplying
     * these with the velocity.
     */
    protected void integrateTransforms() {
        yawTransform.rotY(yawAngle);
        pitchTransform.rotX(pitchAngle);
        spinTransform.rotZ(spinAngle);

        velocityTransform.set(velocity);
        velocityTransform.mul(yawTransform);
        velocityTransform.mul(pitchTransform);
        velocityTransform.mul(spinTransform);

        targetTG.getTransform(targetTransform);
        targetTransform.mul(yawTransform);
        targetTransform.mul(pitchTransform);
        targetTransform.mul(spinTransform);
        targetTransform.mul(velocityTransform);
        targetTG.setTransform(targetTransform);

        if (mouseCallback != null) {
            mouseCallback.transformChanged(-1, targetTransform);
        }
    }

    /**
     * Processes the mouse event, setting yaw, pitch, roll, and velocity depending on mouse button and position.
     *
     * @param  event  Mouse event to process.
     */
    protected void processMouseEvent(MouseEvent event) {
        float mouseX;
        float mouseY;
        float x;
        float y;

        mouseX = event.getX() - canvasCenter.x;
        mouseX /= canvasCenter.x;
        mouseY = event.getY() - canvasCenter.y;
        mouseY /= canvasCenter.y;

        x = 0;
        y = 0;

        if ((Math.abs(mouseX) - deadXSize) > 0f) {
            x = (float) Math.pow(Math.abs(mouseX) - deadXSize, 2f);
            motion = true;
        } else {
            x = 0;
        }

        if ((Math.abs(mouseY) - deadYSize) > 0f) {
            y = (float) Math.pow(Math.abs(mouseY) - deadYSize, 2f);
            motion = true;
        } else {
            y = 0;
        }

        if (mouseX > 0) {
            x = -x;
        }

        if (mouseY < 0) {
            y = -y;
        }

        if (SwingUtilities.isLeftMouseButton(event)) {

            if (event.getID() == MouseEvent.MOUSE_RELEASED) {
                yawAngle = 0;
                velocity.z = 0;
                motion = false;
            } else {
                yawAngle = MAX_ANGLE * x;
                velocity.z = MAX_VELOCITY * y;
            }
        } else if (SwingUtilities.isRightMouseButton(event)) {

            if (event.getID() == MouseEvent.MOUSE_RELEASED) {
                velocity.x = 0;
                velocity.y = 0;
                motion = false;
            } else {
                velocity.x = MAX_VELOCITY * x;
                velocity.y = MAX_VELOCITY * y;
            }
        } else if (SwingUtilities.isMiddleMouseButton(event)) {

            if (event.getID() == MouseEvent.MOUSE_RELEASED) {
                pitchAngle = 0;
                spinAngle = 0;
                motion = false;
            } else {
                pitchAngle = MAX_ANGLE * y;
                spinAngle = MAX_ANGLE * x;
            }
        } else {
            yawAngle = 0;
            pitchAngle = 0;
            spinAngle = 0;
            velocity.x = 0;
            velocity.z = 0;
            velocity.y = 0;
            motion = false;
        }
    }

}
