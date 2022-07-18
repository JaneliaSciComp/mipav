package gov.nih.mipav.view.renderer.J3D.surfaceview.plotterview;


import java.io.*;

import javax.media.j3d.*;


/**
 * DOCUMENT ME!
 */
public class SceneStatePlotter implements Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5063961369724570657L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Current sceneRoot transform. */
    public Transform3D transform = null;

    /** DOCUMENT ME! */
    public int z;

    /** DOCUMENT ME! */
    public boolean zVisible;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new SceneStatePlotter object.
     *
     * @param  z         DOCUMENT ME!
     * @param  zVisible  DOCUMENT ME!
     */
    public SceneStatePlotter(int z, boolean zVisible) {
        this.z = z;
        this.zVisible = zVisible;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String toString() {
        return ("SceneStatePlotter[" + z + "," + zVisible + "]");
    }

}
