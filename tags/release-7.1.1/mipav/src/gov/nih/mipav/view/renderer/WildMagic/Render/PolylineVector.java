package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.io.Serializable;
import java.util.Vector;

import WildMagic.LibGraphics.SceneGraph.Polyline;

public class PolylineVector extends Vector<Polyline> implements Serializable
{
    /**  */
    private static final long serialVersionUID = -7579112007899203250L;
    public PolylineVector() {
        super();
    }
    public PolylineVector(int initialsize) {
        super(initialsize);
    }
}
