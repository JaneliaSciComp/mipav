package gov.nih.mipav.view.renderer.surfaceview.rfaview.mouse;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.media.j3d.*;


/*
 *      Converting a copy of MouseSelection to be used to with DoubleClicking (PickProperties).
 *       Added callback.transformDoubleClicked
 */
public class MouseProperties extends MouseBehavior {
    double x_angle, y_angle;
    double x_factor = .03;
    double y_factor = .03;

    private MouseBehaviorCallback callback = null;

    /*
     * Creates a rotate behavior given the transform group.
     * @param transformGroup The transformGroup to operate on.
     */
    public MouseProperties( TransformGroup transformGroup ) {
        super( transformGroup );
    }

    /*
     * Creates a default mouse Selection behavior.
     */
    public MouseProperties() {
        super( 0 );
    }

    /*
     * Creates a Selection behavior.
     * Note that this behavior still needs a transform
     * group to work on (use setTransformGroup(tg)) and
     * the transform group must add this behavior.
     * @param flags interesting flags (wakeup conditions).
     */
    public MouseProperties( int flags ) {
        super( flags );
    }

    /*
     * Same as above but with Viewer Transform Group and boolean fix
     */
    public MouseProperties( int flags, TransformGroup VPTG, boolean behaviorfix ) {
        super( flags, VPTG, behaviorfix );
    }

    /*
     * Same as above but with boolean fix
     */
    public MouseProperties( int flags, boolean behaviorfix ) {
        super( flags, behaviorfix );
    }

    public void initialize() {
        super.initialize();
        x_angle = 0;
        y_angle = 0;
        if ( ( flags & INVERT_INPUT ) == INVERT_INPUT ) {
            invert = true;
            x_factor *= -1;
            y_factor *= -1;
        }
    }

    /*
     * Return the x-axis movement multipler.
     */
    public double getXFactor() {
        return x_factor;
    }

    /*
     * Return the y-axis movement multipler.
     */
    public double getYFactor() {
        return y_factor;
    }

    /*
     * Set the x-axis amd y-axis movement multipler with factor.
     */
    public void setFactor( double factor ) {
        x_factor = y_factor = factor;
    }

    /*
     * Set the x-axis amd y-axis movement multipler with xFactor and yFactor
     * respectively.
     */
    public void setFactor( double xFactor, double yFactor ) {
        x_factor = xFactor;
        y_factor = yFactor;
    }

    public void processStimulus( Enumeration criteria ) {
        WakeupCriterion wakeup;
        AWTEvent[] event;
        int id;
        int dx, dy;

        while ( criteria.hasMoreElements() ) {
            wakeup = (WakeupCriterion) criteria.nextElement();
            if ( wakeup instanceof WakeupOnAWTEvent ) {
                event = ( (WakeupOnAWTEvent) wakeup ).getAWTEvent();
                for ( int i = 0; i < event.length; i++ ) {
                    processMouseEvent( (MouseEvent) event[i] );

                    if ( ( ( buttonPress ) && ( ( flags & MANUAL_WAKEUP ) == 0 ) )
                            || ( ( wakeUp ) && ( ( flags & MANUAL_WAKEUP ) != 0 ) ) ) {
                        id = event[i].getID();
                        if ( ( id == MouseEvent.MOUSE_DRAGGED ) && !( (MouseEvent) event[i] ).isMetaDown()
                                && !( (MouseEvent) event[i] ).isAltDown() ) {
                            x_last = ( (MouseEvent) event[i] ).getX();
                            y_last = ( (MouseEvent) event[i] ).getY();
                        } else if ( ( id == MouseEvent.MOUSE_PRESSED ) && !( (MouseEvent) event[i] ).isMetaDown()
                                && !( (MouseEvent) event[i] ).isAltDown() ) {
                            if ( reset ) {
                                reset = false;
                            }
                            x_last = ( (MouseEvent) event[i] ).getX();
                            y_last = ( (MouseEvent) event[i] ).getY();
                        } else if ( id == MouseEvent.MOUSE_PRESSED ) {
                            x_last = ( (MouseEvent) event[i] ).getX();
                            y_last = ( (MouseEvent) event[i] ).getY();
                        }
                    }
                }
            }
        }
        wakeupOn( mouseCriterion );
    }

    /*
     * Users can overload this method  which is called every time
     * the Behavior updates the transform
     *
     * Default implementation does nothing
     */
    public void transformChanged( Transform3D transform ) {}

    /*
     * The transformChanged method in the callback class will
     * be called every time the transform is updated
     */
    public void setupCallback( MouseBehaviorCallback callback ) {
        this.callback = callback;
    }
}
