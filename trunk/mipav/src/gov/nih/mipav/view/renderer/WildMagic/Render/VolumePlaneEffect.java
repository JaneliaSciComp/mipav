package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Effects.*;

/** The VolumePlaneEffect ShaderEffect creates shaders for mapping the volume
 * data onto the planes for the 3-orthogonal planes displayed in the
 * VolumeViewer and for the PlaneRender objects.
 * @see GPUVolumeRender.java
 * @see VolumeViewer.java
 * @see PlaneRender.java
 */
public class VolumePlaneEffect extends ShaderEffect
    implements StreamInterface
{
    private final static String[] m_akClip =
        new String[]{ "clipXInv", "clipX", "clipYInv", "clipY", "clipZInv", "clipZ" };


    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kVolumeImageA, the VolumeImage containing shared data and
     * textures for rendering.
     * @param bUnique, when true the shader program must be unique.
     */
    public VolumePlaneEffect ( VolumeImage kVolumeImageA, boolean bUnique )
    {
        m_kVolumeImageA = kVolumeImageA;
        Init( bUnique );
    }
    
    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kVolumeImageA, the VolumeImage containing shared data and
     * textures for rendering.
     */
    public VolumePlaneEffect ( VolumeImage kVolumeImageA )
    {
        m_kVolumeImageA = kVolumeImageA;
        Init( false );
    }
    
    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_afBlend = null;
        m_afShowSurface = null;
        super.dispose();
    }

    /** Initializes the ShaderEffect vertex and pixel shader programs. */
    private void Init ( boolean bUnique )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);

        SetVShader(0,new VertexShader("Color_Opacity_Texture"));
        SetPShader(0,new PixelShader("Color_Opacity_Texture", bUnique));
        GetPShader(0).SetTextureQuantity(3);
        GetPShader(0).SetImageName(0,"VolumeImageA");
        GetPShader(0).SetTexture(0, m_kVolumeImageA.GetVolumeTarget() );
        GetPShader(0).SetImageName(1, "ColorMapA");
        GetPShader(0).SetTexture(1, m_kVolumeImageA.GetColorMapTarget() );
        GetPShader(0).SetImageName(2, "SurfaceImage");
        GetPShader(0).SetTexture(2, m_kVolumeImageA.GetSurfaceTarget() );
    }

    /** This function is called in LoadPrograms once the shader programs are
     * created.  It gives the ShaderEffect-derived classes a chance to do
     * any additional work to hook up the effect with the low-level objects.
     * @param iPass the ith rendering pass
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        Blend(1);
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("IsColor") != null ) 
        {
            if ( m_kVolumeImageA.GetImage().isColorImage() )
            {
                pkProgram.GetUC("IsColor").SetDataSource(new float[]{1,0,0,0});
            }
            else
            {
                pkProgram.GetUC("IsColor").SetDataSource(new float[]{0,0,0,0});
            }
        }       
    }
 
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend, blend factor (range = 0-1).
     */
    public float GetBlend()
    {
        return m_afBlend[0];
    }
    
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend, blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {
        m_afBlend[0] = fBlend;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram != null && pkProgram.GetUC("blend") != null ) 
        {
            pkProgram.GetUC("blend").SetDataSource(m_afBlend);
        }
    }
    
    public void ShowSurface( boolean bOn )
    {
        m_afShowSurface[0] = bOn ? 1 : 0;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram != null && pkProgram.GetUC("ShowSurface") != null ) 
        {
            pkProgram.GetUC("ShowSurface").SetDataSource(m_afShowSurface);
        } 
    }

    /**
     * Sets the BackgroundColor shader parameter.
     * @param kColor, new BackgroundColor.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("BackgroundColor") != null ) 
        {
            m_afBackgroundColor[0] = kColor.R;
            m_afBackgroundColor[1] = kColor.G;
            m_afBackgroundColor[2] = kColor.B;
            m_afBackgroundColor[3] = kColor.A;
            pkProgram.GetUC("BackgroundColor").SetDataSource(m_afBackgroundColor);
        }
    }


    /**
     * Registers this object with the input Stream parameter. All objects
     * streamed to disk are registered with the Stream so that a unique list
     * of objects is maintained.
     * @param rkStream, the Stream where the child objects are stored.
     * @return true if this object is registered, false if the object has
     * already been registered.
     */
    public boolean Register (Stream rkStream)
    {
        if (!super.Register(rkStream))
        {
            return false;
        }
        return true;
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        //rkStream.Write(m_iPassQuantity);

        // link data
//         for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
//         {
//             rkStream.Write(m_kVShader.get(iPass).GetID());
//             rkStream.Write(m_kPShader.get(iPass).GetID());
//         }
//         int iQuantity = m_kAlphaState.size();
//         rkStream.Write(iQuantity);
//         for (int i = 0; i < iQuantity; i++)
//         {
//             rkStream.Write(m_kAlphaState.get(i).GetID());
//         }

    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        int iSize = super.GetDiskUsed(rkVersion);

        return iSize;
    }

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("VolumePlaneEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));

        return pkTree;
    }

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageA;

    /** stores the blend function */
    private float[] m_afBlend = new float[]{1f,0,0,0};
    /** turns surface display on/off */
    private float[] m_afShowSurface = new float[]{0f,0,0,0};
    /** stores the background color */
    private float[] m_afBackgroundColor = new float[4];
}
