package gov.nih.mipav.view.renderer.J3D.surfaceview;


import java.awt.event.*;

import javax.swing.*;


/**
 * This behavior is supposed to "fly by pointer" - that is, zoom in, translate, and rotate to where the mouse is
 * currently pointing. The velocity shall be constant, and the user will be able to control it though a slider.
 */
public class PointerBehaviorRenderer extends MouseBehaviorRenderer {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new pointer behavior, sets parent dialog, and resets MAX_ANGLE to the true start value.
     *
     * @param  parent  Parent dialog.
     */
    public PointerBehaviorRenderer(JPanelView parent) {
        super(parent);
        MAX_ANGLE = (float) Math.toRadians(100);
        deadFactor = 15f;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a new transform for the image by multiplying the appropriate rotations and translations.
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
        targetTransform.mul(velocityTransform);
        targetTG.setTransform(targetTransform);

        if (mouseCallback != null) {
            mouseCallback.transformChanged(-1, targetTransform);
        }
    }

    /**
     * Processes mouse event. For the left mouse button, changes translation; for the right mouse button, changes spin
     * or pitch. If ALT key is down, zooms out.
     *
     * @param  evt  Mouse event to process.
     */
    protected void processMouseEvent(MouseEvent evt) {
        float offsetX = (evt.getX() - canvasCenter.x) / canvasCenter.x;
        float offsetY = (evt.getY() - canvasCenter.y) / canvasCenter.y;
        float factorX = 0f;
        float factorY = 0f;

        if ((Math.abs(offsetX) - deadXSize) > 0f) {
            factorX = (float) Math.abs(offsetX) - deadXSize;
            motion = true;
        } else {
            factorX = 0f;
        }

        if ((Math.abs(offsetY) - deadYSize) > 0f) {
            factorY = (float) Math.abs(offsetY) - deadYSize;
            motion = true;
        } else {
            factorY = 0f;
        }

        motion = true;

        if (offsetX > 0f) {
            factorX = -factorX;
        }

        factorX = -factorX;

        if (offsetY < 0f) {
            factorY = -factorY;
        }

        if (SwingUtilities.isLeftMouseButton(evt)) {
            spinAngle = 0;
            pitchAngle = 0;
            yawAngle = 0;

            if (evt.getID() == MouseEvent.MOUSE_RELEASED) {
                spinAngle = 0;
                pitchAngle = 0;
                yawAngle = 0;
                velocity.z = 0;
                motion = false;
            } else {
                spinAngle = 0;
                pitchAngle = 0;
                yawAngle = 0;

                if (factorX > 0) {
                    velocity.x = -.01f * MAX_VELOCITY;
                } else {
                    velocity.x = .01f * MAX_VELOCITY;
                }

                if (factorY > 0) {
                    velocity.y = .01f * MAX_VELOCITY;
                } else {
                    velocity.y = -.01f * MAX_VELOCITY;
                }

                if (factorX == 0) {
                    velocity.x = 0f;
                }

                if (factorY == 0) {
                    velocity.y = 0f;
                }

                velocity.z = .01f * MAX_VELOCITY;

                if (evt.isAltDown() == true) {
                    velocity.z = -velocity.z;
                }
            }
        } else if (SwingUtilities.isRightMouseButton(evt)) {
            velocity.x = 0;
            velocity.y = 0;
            velocity.z = 0;

            if (evt.getID() == MouseEvent.MOUSE_RELEASED) {
                spinAngle = 0;
                pitchAngle = 0;
                yawAngle = 0;
                motion = false;
            } else {

                if (offsetX > offsetY) {

                    if (factorX > 0) {
                        spinAngle = MAX_ANGLE * -.01f;
                    } else {
                        spinAngle = MAX_ANGLE * .01f;
                    }
                } else {

                    if (factorY > 0) {
                        pitchAngle = MAX_ANGLE * .01f;
                    } else {
                        pitchAngle = MAX_ANGLE * -.01f;
                    }
                }
            }
        } else {
            yawAngle = 0f;
            pitchAngle = 0f;
            spinAngle = 0f;
            velocity.x = 0;
            velocity.y = 0;
            velocity.z = 0;
            motion = false;
        }
    }

}
