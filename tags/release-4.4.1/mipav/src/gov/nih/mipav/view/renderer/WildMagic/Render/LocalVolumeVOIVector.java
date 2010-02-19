package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.io.Serializable;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public class LocalVolumeVOIVector extends Vector<LocalVolumeVOI>
{
    /**  */
    private static final long serialVersionUID = 5551644349599464551L;
    public LocalVolumeVOIVector() {
        super();
    }
    public LocalVolumeVOIVector(int initialsize) {
        super(initialsize);
    }
}
