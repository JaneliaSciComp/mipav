package gov.nih.mipav.view.renderer.WildMagic;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;

import java.awt.Color;
import java.io.Serializable;
import java.util.Vector;

import javax.vecmath.Matrix4f;

import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class VolumeRenderState implements Serializable
{
    private static final long serialVersionUID = 4937933351322531830L;
    public VolumeRenderState() {};
    public VolumeImage ImageA;
    public VolumeImage ImageB;
    public Matrix3f View = new Matrix3f();
    public boolean ShowAxes = true;
    public boolean ShowCrossHairs = true;
    public boolean ShowVOI = false;
    public boolean Show4D = false;
    public boolean Radiological = true;
    
    public int[] Opacity = new int[3];
    public Color[] SliceColor = new Color[3];
    public boolean[] ShowSlice = new boolean[3];
    public boolean[] ShowSliceBox = new boolean[3];
    public Vector3f Center = new Vector3f();
    public Vector<String> TabbedList = new Vector<String>();
}
