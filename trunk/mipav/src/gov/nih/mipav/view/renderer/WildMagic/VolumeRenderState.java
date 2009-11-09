package gov.nih.mipav.view.renderer.WildMagic;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;

import java.io.Serializable;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class VolumeRenderState implements Serializable
{
    private static final long serialVersionUID = 4937933351322531830L;
    public VolumeRenderState() {};
    public VolumeImage ImageA;
    public VolumeImage ImageB;
    public Vector3f Center = new Vector3f();
    public Vector<String> TabbedList = new Vector<String>();
}
