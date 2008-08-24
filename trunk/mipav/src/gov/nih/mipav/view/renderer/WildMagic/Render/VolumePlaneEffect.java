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
        m_aafClipData = null;
        m_afClipEyeData = null;
        m_afClipEyeInvData = null;
        m_afClipArbData = null;
        m_afBlend = null;

        m_afGradientMagnitude = null;

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
        GetPShader(0).SetImageName(2, "OpacityMapA");
        GetPShader(0).SetTexture(2, m_kVolumeImageA.GetOpacityMapTarget() );
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
     * Reload the current shader programs from disk, compile and parse and
     * send to the GPU.
     * @param kRenderer, the Renderer object displaying the scene-graph which
     * will apply the shader programs.
     */
    public void Reload( WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        Program kVProgram = GetVProgram(0);
        kVProgram.Release();
        VertexProgramCatalog.GetActive().Remove(kVProgram);

        Program kPProgram = GetPProgram(0);
        kPProgram.Release();
        PixelProgramCatalog.GetActive().Remove(kPProgram);

        VertexShader pkVShader = GetVShader(0);
        pkVShader.OnReleaseProgram();
        PixelShader pkPShader = GetPShader(0);
        pkPShader.OnReleaseProgram();

        LoadPrograms(kRenderer, 0,kRenderer.GetMaxColors(),
                     kRenderer.GetMaxTCoords(),
                     kRenderer.GetMaxVShaderImages(),
                     kRenderer.GetMaxPShaderImages());
    }

    /**
     * Init the axis-aligned clip planes.
     * @param afData, the axis-aligned clip plane default positions.
     */
    public void InitClip( float[] afData )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("DoClip") != null ) 
        {
            pkProgram.GetUC("DoClip").SetDataSource(new float[]{0,0,0,0});
        }       

        for ( int i = 0; i < 6; i++ )
        {
            if ( pkProgram.GetUC(m_akClip[i]) != null ) 
            {
                pkProgram.GetUC(m_akClip[i]).SetDataSource(new float[]{afData[i],0,0,0});
                m_aafClipData[i][0] = afData[i];
            }       
        }
    }

    /**
     * Reset the axis-aligned clip planes, eye, inverse-eye and arbitrary clip
     * planes to neutral.
     */
    private void ResetClip()
    {
        for ( int i = 0; i < 6; i++ )
        {
            if ( m_aafClipData[i] != null )
            {
                SetClip( i, m_aafClipData[i] );
            }
        }
        if ( m_afClipEyeData != null )
        {
            SetClipEye(m_afClipEyeData);
        }
        if ( m_afClipEyeInvData != null )
        {
            SetClipEyeInv(m_afClipEyeInvData);
        }
        if ( m_afClipArbData != null )
        {
            SetClipArb(m_afClipArbData);
        }
    }

    /**
     * Enable and set the axis-aligned clip plane.
     * @param iWhich, one of 6 clip-planes to set.
     * @param data, the distance to the clip-plane.
     */
    public void SetClip(int iWhich, float[] data)
    {
        m_aafClipData[iWhich] = data;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC(m_akClip[iWhich]) != null ) 
        {
            pkProgram.GetUC(m_akClip[iWhich]).SetDataSource(data);
        }
    }
    /**
     * Enable clipping.
     */
    private void EnableClip()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("DoClip") != null ) 
        {
            pkProgram.GetUC("DoClip").SetDataSource(new float[]{1,0,0,0});
        }       
    }

    /**
     * Enable and set the eye clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipEye(float[] afEquation)
    {
        m_afClipEyeData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipEye") != null ) 
        {
            pkProgram.GetUC("clipEye").SetDataSource(afEquation);
        }
    }

    /**
     * Enable and set the inverse-eye clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipEyeInv(float[] afEquation)
    {
        m_afClipEyeInvData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipEyeInv") != null ) 
        {
            pkProgram.GetUC("clipEyeInv").SetDataSource(afEquation);
        }
    }

    /**
     * Enable and set the arbitrary clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipArb(float[] afEquation)
    {
        m_afClipArbData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipArb") != null ) 
        {
            pkProgram.GetUC("clipArb").SetDataSource(afEquation);
        }
    }

    /**
     * Returns the current pixel program.
     * @return the current pixel program.
     */
    public Program GetPProgram()
    {
        return GetPProgram(0);
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
        if ( pkProgram.GetUC("blend") != null ) 
        {
            pkProgram.GetUC("blend").SetDataSource(m_afBlend);
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
     * Enables/Disables gradient magnitude filter.
     * @param bShow, gradient magnitude filter on/off.
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        m_afGradientMagnitude[0] = 0;
        if ( bShow )
        {
            m_afGradientMagnitude[0] = 1;
        }
        SetGradientMagnitude();
    }

    /**
     * Sets the GradientMagnitude shader parameter.
     */
    private void SetGradientMagnitude()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("GradientMagnitude") != null ) 
        {
            pkProgram.GetUC("GradientMagnitude").SetDataSource(m_afGradientMagnitude);
        }
    }

    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object
     * for linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
    }

    /**
     * Copies this objects children objects from the input Stream's HashTable,
     * based on the LinkID of the child stored in the pkLink paramter.
     * @param rkStream, the Stream where the child objects are stored.
     * @param pkLink, the Link class from which the child object IDs are read.
     */
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
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

    /** stores the axis-aligned clip plane information: */
    private float[][] m_aafClipData = new float[6][4];
    /** stores the eye clip plane information: */
    private float[] m_afClipEyeData = null;
    /** stores the inverse-eye clip plane information: */
    private float[] m_afClipEyeInvData = null;
    /** stores the arbitrary clip plane information: */
    private float[] m_afClipArbData = null;
    /** stores the blend function */
    private float[] m_afBlend = new float[]{1f,0,0,0};
    /** stores the background color */
    private float[] m_afBackgroundColor = new float[4];
    /** stores the gradient magnitude filter on/off value: */
    private float[] m_afGradientMagnitude = new float[]{0,0,0,0};
}
