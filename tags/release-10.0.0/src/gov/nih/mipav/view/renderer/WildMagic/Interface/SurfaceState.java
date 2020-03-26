package gov.nih.mipav.view.renderer.WildMagic.Interface;

import java.awt.Color;
import java.io.IOException;
import java.io.Serializable;

import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


public class SurfaceState implements Serializable
 {
    private static final long serialVersionUID = -489331431277320685L;
    public Color SurfaceColor = new Color( 255, 255, 255 );
    public MaterialState Material;
    public float Opacity = 1.0f;
    public WireframeState.FillMode Fill = WireframeState.FillMode.FM_FILL;
    public boolean Pickable = false;
    public boolean Clip = false;
    public boolean BackfaceCull = true;
    public boolean TransparencyOn = true;
    public String Name = "";
    public transient TriMesh Surface = null;

    public SurfaceState( ) {}
    public SurfaceState( TriMesh kMesh, String kName )
    {
        Name = new String(kName);
        Surface = kMesh;
    }
    
    public void dispose()
    {
    	SurfaceColor = null;
    	Material = null;
    	Fill = null;
    	Name = null;
    	Surface = null;
    }
    

    private void writeObject(java.io.ObjectOutputStream out)
    throws IOException 
    {
        out.writeObject(SurfaceColor);
        out.writeObject(Material);
        out.writeFloat(Opacity);
        out.writeObject(Fill);
        out.writeBoolean(Pickable);
        out.writeBoolean(Clip);
        out.writeBoolean(BackfaceCull);
        out.writeBoolean(TransparencyOn);
        out.writeObject(Name);
        out.writeObject(Surface.VBuffer);
        out.writeObject(Surface.IBuffer);
    }    
    private void readObject(java.io.ObjectInputStream in)
    throws IOException, ClassNotFoundException
    {
        SurfaceColor = (Color)in.readObject();
        Material = (MaterialState)in.readObject();
        Opacity = in.readFloat();
        Fill = (WireframeState.FillMode)in.readObject();
        Pickable = in.readBoolean();
        Clip = in.readBoolean();
        BackfaceCull = in.readBoolean();
        TransparencyOn = in.readBoolean();
        Name = (String)in.readObject();
        VertexBuffer kVBuffer = (VertexBuffer)in.readObject();
        IndexBuffer kIBuffer = (IndexBuffer)in.readObject();
        Surface = new TriMesh( kVBuffer, kIBuffer );
        Surface.SetName(Name);        
    }
}
