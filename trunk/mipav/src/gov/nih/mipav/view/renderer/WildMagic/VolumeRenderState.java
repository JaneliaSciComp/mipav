package gov.nih.mipav.view.renderer.WildMagic;
import java.io.Serializable;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class VolumeRenderState implements Serializable
{
    private static final long serialVersionUID = 4937933351322531830L;
    public VolumeRenderState() {};
    public Vector3f Center = new Vector3f();
    public float X, Y, Z;
}
