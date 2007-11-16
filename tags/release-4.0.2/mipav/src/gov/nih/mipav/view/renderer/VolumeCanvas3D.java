package gov.nih.mipav.view.renderer;


import java.awt.*;

import javax.media.j3d.*;


/**
 * Extension of Canvas3D to allow for customization as needed.
 */
public class VolumeCanvas3D extends Canvas3D {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5377755312667171175L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create extension of Canvas3D.
     *
     * @param  kGraphicsConfiguration  see the notes under Canvas3D for this parameter.
     */
    public VolumeCanvas3D(java.awt.GraphicsConfiguration kGraphicsConfiguration) {
        super(kGraphicsConfiguration);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Override for Canvas3D method which uses the paint callback to track when it is possible to render into the canvas
     * but makes sure that the default Toolkit's graphics state is synchronized just in case the window system has
     * buffered graphics events.
     *
     * @param  kGraphics  the graphics context
     */
    public void paint(Graphics kGraphics) {
        super.paint(kGraphics);
        Toolkit.getDefaultToolkit().sync();
    }

    /**
     * Check to see if 3D textures are supported by the graphics card.
     *
     * @return  boolean <code>true</code> supported,<code>false</code> not support
     */
    public boolean supportsTexture3D() {
        return queryBooleanProperty("texture3DAvailable");
    }

    /**
     * Override for Canvas3D method which uses the paint callback to track when it is possible to render into the canvas
     * but makes sure that the default Toolkit's graphics state is synchronized just in case the window system has
     * buffered graphics events.
     *
     * @param  kGraphics  the graphics context
     */
    public void update(Graphics kGraphics) {
        this.paint(kGraphics);
    }

    /**
     * Query the boolean state of the specified property in a Canvas3D.
     *
     * @param   kStrPropertyName  name of the Boolean value property to query
     *
     * @return  State of requested Canvas3D property
     */
    private boolean queryBooleanProperty(String kStrPropertyName) {
        return ((Boolean) (queryProperties().get(kStrPropertyName))).booleanValue();
    }

    /**
     * Query the integer state of the specified property in a Canvas3D.
     *
     * @param   kStrPropertyName  name of the Integer value property to query
     *
     * @return  State of requested Canvas3D property
     */
    private int queryIntegerProperty(String kStrPropertyName) {
        return ((Integer) (queryProperties().get(kStrPropertyName))).intValue();
    }
}
