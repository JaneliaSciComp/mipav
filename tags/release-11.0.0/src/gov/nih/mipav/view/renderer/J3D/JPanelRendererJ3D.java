package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.view.*;

import java.awt.event.*;


import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.volumeview.*;

/**
 * This class is the base for all the other dialogs. It has two important functions that are used by almost all the
 * dialogs. It also implements all the listeners except for the action listener.
 *
 * @version  1.0 Aug 1, 1998
 * @author   Matthew J. McAuliffe, Ph.D. ( Primary )
 * @author   Ruida Cheng
 */
public abstract class JPanelRendererJ3D extends JPanelRendererBase
        implements KeyListener, ActionListener, FocusListener, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------
    /** Raycast based renderer reference, raycast renderer or shear warp renderer. */
    protected VolumeRenderer rayBasedRender;

    /** Render base. */
    protected RenderViewBase renderBase = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that sets the parent frame of the dialog and whether or not the dialog is modal. Also adds this as a
     * window listener and key listener to all dialogs.
     *
     * @param  parent  Parent frame.
     */
    public JPanelRendererJ3D(RenderViewBase parent) {
        renderBase = parent;
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;
        addKeyListener(this);
    }

    /**
     * Constructor that sets the parent frame of the dialog and whether or not the dialog is modal. Also adds this as a
     * window listener and key listener to all dialogs.
     *
     * @param  parent  Parent frame.
     */
    public JPanelRendererJ3D() {
        super();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * Clear memory.
     */
    public void disposeLocal() {
        renderBase = null;
        super.disposeLocal();
    }

    /**
     * Set the reference to ray based renderer, raycast renderer or shear warp renderer. This method set the clipping
     * dialog to control the both the 3D texture renderer and raycast based renderer.
     *
     * @param  _rayBasedRender  VolumeRenderer reference
     */
    public void setRayBasedRender(VolumeRenderer _rayBasedRender) {
        rayBasedRender = _rayBasedRender;
    }

    /**
     * Accessor to set the render base object This reference used to update volumes according to the render view base
     * instance type.
     *
     * @param  _renderBase  RenderViewBase
     */
    public void setSurfaceRender(RenderViewBase _renderBase) {
        renderBase = _renderBase;
    }

    /**
     * Accessor to set the render base object This reference used to update volumes according to the render view base
     * instance type.
     *
     * @param  _renderBase  RenderViewBase
     */
    public RenderViewBase getSurfaceRender() {
        return renderBase;
    }
}
