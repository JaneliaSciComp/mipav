// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Restricted Libraries source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4RestrictedLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

/** The shader effect is a manager of the vertex and pixel shaders.  It
 * reimplements the user-relevant interfaces for the managed objects as a
 * convenience to avoid long expressions involving pointer dereferencing.
 */
public class ShaderEffect extends Effect
    implements NameIdInterface, StreamInterface
{

    /** Creates a ShaderEffect with the specified number of rendering passes.
     * @param iPassQuantity The number of vertex/pixel shader pairs.
     */
    public ShaderEffect (int iPassQuantity)
    {
        assert(iPassQuantity > 0);

        SetPassQuantity(iPassQuantity);
    }

    /** The number of vertex/pixel shader pairs.  The Set* call reallocates the
     * vertex shader, pixel shader, and alpha state arrays.
     * @param iPassQuantity The number of vertex/pixel shader pairs.
     */
    public void SetPassQuantity (int iPassQuantity)
    {
        assert(iPassQuantity > 0);
        m_iPassQuantity = iPassQuantity;

        m_kVShader.setSize(m_iPassQuantity);
        m_kPShader.setSize(m_iPassQuantity);
        m_kAlphaState.setSize(m_iPassQuantity);
        SetDefaultAlphaState();
    }

    /**
     * @return The number of vertex/pixel shader pairs.
     */
    public int GetPassQuantity ()
    {
        return m_iPassQuantity;
    }

    /** Blending modes for multipass effects.  Mode i specifies how the pixel
     * colors from pass i-1 and pass i are blended.  For a single effect
     * attached to a Geometry object, blending mode 0 is irrelevant in that
     * the source mode is equivalent to SBF_ONE and the destination mode is
     * equivalent to SDF_ZERO; that is, the frame buffer values are replaced
     * with the pixel values from the shader.  If multiple effects are
     * attached to a Geometry object, blending mode 0 specifies how the
     * current effect is blended with the frame buffer.
     * @param iPass the ith rendering pass
     * @return the Alpha state associated with the rendering pass
     */
    public AlphaState GetBlending (int iPass)
    {
        // A derived class might want to use alpha state for selecting shader
        // programs for doing a single-pass renderering.  The class can resize
        // the alpha state array to contain more items than the number of passes.
        // Thus, the assertion here does not compare iPass to m_iPassQuantity.
        assert(0 <= iPass && iPass < (int)m_kAlphaState.size());
        return m_kAlphaState.get(iPass);
    }

    /** Set any global state needed by the pass and restore it later.  The
     * base class enables and disables blending as needed for multipass
     * and multieffect drawing.
     * @param iPass the ith rendering pass
     * @param pkRenderer the Rendering object
     * @param bPriaryEffect
     */
    public void SetGlobalState (int iPass, Renderer pkRenderer,
                                boolean bPrimaryEffect)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity && (pkRenderer != null));

        // Enable all passes after the first one to blend with the previous
        // passes.
        if (!bPrimaryEffect || iPass > 0)
        {
            m_kAlphaState.get(iPass).BlendEnabled = true;

            AlphaState spkSave = pkRenderer.GetAlphaState();
            pkRenderer.SetAlphaState(m_kAlphaState.get(iPass));
            m_kAlphaState.set(iPass, spkSave);
        }
    }

    /** Set any global state needed by the pass and restore it later.  The
     * base class enables and disables blending as needed for multipass
     * and multieffect drawing.
     * @param iPass the ith rendering pass
     * @param pkRenderer the Rendering object
     * @param bPriaryEffect
     */
    public void RestoreGlobalState (int iPass, Renderer pkRenderer,
                                    boolean bPrimaryEffect)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity && (pkRenderer != null));

        if (!bPrimaryEffect || iPass > 0)
        {
            AlphaState spkSave = pkRenderer.GetAlphaState();
            pkRenderer.SetAlphaState(m_kAlphaState.get(iPass));
            m_kAlphaState.set(iPass, spkSave);
        }
    }

    /** Sets the Vertex shader for the ith Pass
     * @param iPass the ith rendering pass
     * @param pkVShader VertexShader
     */
    public void SetVShader (int iPass, VertexShader pkVShader)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        m_kVShader.set(iPass, pkVShader);
    }

    /** Returns the Vertex shader for the ith Pass
     * @param iPass the ith rendering pass
     * @return the ith VertexShader
     */
    public VertexShader GetVShader (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kVShader.get(iPass);
    }

    /** Returns the Vertex program for the ith Pass
     * @param iPass the ith rendering pass
     * @return the ith VertexProgram
     */
    public VertexProgram GetVProgram (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kVShader.get(iPass).GetProgram();
    }

    /** Returns the Vertex Shader Name for the ith Pass
     * @param iPass the ith rendering pass
     * @return the ith Vertex Shader Name
     */
    public String GetVShaderName (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kVShader.get(iPass).GetShaderName();
    }

    /** Returns the number of User Constants for the ith VertexShader
     * @param iPass the ith rendering pass
     * @return the number of User Constants for the ith VertexShader
     */
    public int GetVConstantQuantity (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        Program pkVProgram = m_kVShader.get(iPass).GetProgram();
        return (pkVProgram != null) ? pkVProgram.GetUCQuantity() : 0;
    }

    /** Returns the ith User Constants for the ith VertexShader
     * @param iPass the ith rendering pass
     * @param i the ith User Constant
     * @return the ith User Constants for the ith VertexShader
     */
    public UserConstant GetVConstant (int iPass, int i)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        Program pkVProgram = m_kVShader.get(iPass).GetProgram();
        if (pkVProgram != null)
        {
            assert(0 <= i && i < pkVProgram.GetUCQuantity());
            return pkVProgram.GetUC(i);
        }
        return null;
    }

    /** Returns the User Constants with the specified name for the ith VertexShader
     * @param iPass the ith rendering pass
     * @param rkName the name of the User Constant
     * @return the User Constants with the specified name for the ith
     * VertexShader
     */
    public UserConstant GetVConstant (int iPass, String rkName)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        Program pkVProgram = m_kVShader.get(iPass).GetProgram();
        return (pkVProgram != null) ? pkVProgram.GetUC(rkName) : null;
    }

    /** Returns the number of Textures for the ith VertexShader
     * @param iPass the ith rendering pass
     * @return the number of Textures for the ith VertexShader
     */
    public int GetVTextureQuantity (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kVShader.get(iPass).GetTextureQuantity();
    }

    /** Returns the ith Texture for the ith VertexShader
     * @param iPass the ith rendering pass
     * @param i the ith Texture
     * @return the ith Texture for the ith VertexShader
     */
    public Texture GetVTexture (int iPass, int i)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kVShader.get(iPass).GetTexture(i);
    }

    /** Returns the Texture with the specified name for the ith VertexShader
     * @param iPass the ith rendering pass
     * @param rkName the name of the texture
     * @return the Texture with the specified name for the ith VertexShader
     */
    public Texture GetVTexture (int iPass, String rkName)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kVShader.get(iPass).GetTexture(rkName);
    }

    /** Sets the ith Image Name for the ith VertexShader
     * @param iPass the ith rendering pass
     * @param i the ith Image Name
     * @param rkName the new Image Name
     */
    public void SetVImageName (int iPass, int i, String rkName)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        m_kVShader.get(iPass).SetImageName(i,rkName);
    }

    /** Returns the ith Image Name for the ith VertexShader
     * @param iPass the ith rendering pass
     * @param i the ith Image Name
     * @return the Image Name
     */
    public String GetVImageName (int iPass, int i)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kVShader.get(iPass).GetImageName(i);
    }

    /** Sets the Pixel shader for the ith Pass
     * @param iPass the ith rendering pass
     * @param pkPShader PixelShader
     */
    public void SetPShader (int iPass, PixelShader pkPShader)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        m_kPShader.set(iPass, pkPShader);
    }

    /** Returns the Pixel shader for the ith Pass
     * @param iPass the ith rendering pass
     * @return PixelShader
     */
    public PixelShader GetPShader (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kPShader.get(iPass);
    }

    /** Returns the PixelProgram for the ith Pass
     * @param iPass the ith rendering pass
     * @return the ith PixelProgram
     */
    public PixelProgram GetPProgram (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kPShader.get(iPass).GetProgram();
    }

    /** Returns the Pixel Shader Name for the ith Pass
     * @param iPass the ith rendering pass
     * @return the ith Pixel Shader Name
     */
    public String GetPShaderName (int iPass) 
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kPShader.get(iPass).GetShaderName();
    }

    /** Returns the number of User Constants for the ith PixelShader
     * @param iPass the ith rendering pass
     * @return the number of User Constants for the ith PixelShader
     */
    public int GetPConstantQuantity (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        Program pkPProgram = m_kPShader.get(iPass).GetProgram();
        return (pkPProgram != null) ? pkPProgram.GetUCQuantity() : 0;
    }

    /** Returns the ith User Constants for the ith PixelShader
     * @param iPass the ith rendering pass
     * @param i the ith User Constant
     * @return the ith User Constants for the ith PixelShader
     */
    public UserConstant GetPConstant (int iPass, int i)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        Program pkPProgram = m_kPShader.get(iPass).GetProgram();
        if (pkPProgram != null)
        {
            assert(0 <= i && i < pkPProgram.GetUCQuantity());
            return pkPProgram.GetUC(i);
        }
        return null;
    }

    /** Returns the User Constants with the specified name for the ith PixelShader
     * @param iPass the ith rendering pass
     * @param rkName the name of the User Constant
     * @return the User Constants with the specified name for the ith
     * PixelShader
     */
    public UserConstant GetPConstant (int iPass, String rkName)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        Program pkPProgram = m_kPShader.get(iPass).GetProgram();
        return (pkPProgram != null) ? pkPProgram.GetUC(rkName) : null;
    }

    /** Returns the number of Textures for the ith PixelShader
     * @param iPass the ith rendering pass
     * @return the number of Textures for the ith PixelShader
     */
    public int GetPTextureQuantity (int iPass)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        Program pkPProgram = m_kPShader.get(iPass).GetProgram();
        return (pkPProgram != null) ? pkPProgram.GetSIQuantity() : 0;
    }

    /** Returns the ith Texture for the ith PixelShader
     * @param iPass the ith rendering pass
     * @param i the ith Texture
     * @return the ith Texture for the ith PixelShader
     */
    public Texture GetPTexture (int iPass, int i)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kPShader.get(iPass).GetTexture(i);
    }

    /** Returns the Texture with the specified name for the ith PixelShader
     * @param iPass the ith rendering pass
     * @param rkName the name of the texture
     * @return the Texture with the specified name for the ith PixelShader
     */
    public Texture GetPTexture (int iPass, String rkName)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kPShader.get(iPass).GetTexture(rkName);
    }

    /** Sets the ith Image Name for the ith PixelShader
     * @param iPass the ith rendering pass
     * @param i the ith Image Name
     * @param rkName the new Image Name
     */
    public void SetPImageName (int iPass, int i, String rkName)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        m_kPShader.get(iPass).SetImageName(i,rkName);
    }

    /** Returns the ith Image Name for the ith PixelShader
     * @param iPass the ith rendering pass
     * @param i the ith Image Name
     * @return the Image Name
     */
    public String GetPImageName (int iPass, int i)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        return m_kPShader.get(iPass).GetImageName(i);
    }

    /** The functions are called by Renderer::LoadResources and
     * Renderer::ReleaseResources for Geometry and Effect objects.
     * Loads the programs into the shader objects and video memory
     * @param pkRenderer the Rederer object
     * @param pkGeometry the Geometry the ShaderEffect is applied to
     */
    public void LoadResources (Renderer pkRenderer, Geometry pkGeometry)
    {
        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            // Load the programs to the shader objects.
            LoadPrograms(iPass,pkRenderer.GetMaxColors(),
                         pkRenderer.GetMaxTCoords(),pkRenderer.GetMaxVShaderImages(),
                         pkRenderer.GetMaxPShaderImages());

            // Load the programs into video memory.
            VertexProgram pkVProgram = m_kVShader.get(iPass).GetProgram();
            pkRenderer.LoadVProgram(pkVProgram);
            pkRenderer.LoadPProgram(m_kPShader.get(iPass).GetProgram());

            // Load the textures into video memory.
            final int iPTQuantity = GetPTextureQuantity(iPass);
            for (int i = 0; i < iPTQuantity; i++)
            {
                pkRenderer.LoadTexture(m_kPShader.get(iPass).GetTexture(i));
            }

            if (pkGeometry != null)
            {
                // Load the vertex buffer into video memory.
                Attributes rkIAttr = pkVProgram.GetInputAttributes();
                Attributes rkOAttr = pkVProgram.GetOutputAttributes();
                pkRenderer.LoadVBuffer(rkIAttr,rkOAttr,pkGeometry.VBuffer);
            }
        }
    }

    /** The functions are called by Renderer::LoadResources and
     * Renderer::ReleaseResources for Geometry and Effect objects.
     * Releases the programs froma the shader objects and video memory
     * @param pkRenderer the Rederer object
     * @param pkGeometry the Geometry the ShaderEffect is applied to
     */
    public void ReleaseResources (Renderer pkRenderer,
                                  Geometry pkGeometry)
    {
        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            // Release the textures from video memory.
            final int iPTQuantity = GetPTextureQuantity(iPass);
            for (int i = 0; i < iPTQuantity; i++)
            {
                pkRenderer.ReleaseTexture(m_kPShader.get(iPass).GetTexture(i));
            }

            // Release the programs from video memory.
            pkRenderer.ReleasePProgram(m_kVShader.get(iPass).GetProgram());
            pkRenderer.ReleaseVProgram(m_kPShader.get(iPass).GetProgram());

            // Release the programs from the shader objects.
            ReleasePrograms(iPass);
        }
    }

    /** Create default alpha states. The Renderer enables this on a multieffect
     * drawing operation.  The first pass uses the default alpha state
     * (SBF_SRC_ALPHA, DBF_ONE_MINUS_SRC_ALPHA).  All other passes use
     * modulation and all are enabled.  These may be overridden by your
     * application code by accessing the state via effect->GetBlending(pass).
     */
    protected void SetDefaultAlphaState ()
    {
        m_kAlphaState.set(0, new AlphaState());
        m_kAlphaState.get(0).BlendEnabled = true;
        for (int i = 1; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_DST_COLOR;
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendMode.DBF_ZERO;
        }
    }

    /** The number of passes */
    protected int m_iPassQuantity;
    /** The VertexShaders */
    protected Vector<VertexShader> m_kVShader = new Vector<VertexShader>();
    /** The PixelShaders */
    protected Vector<PixelShader> m_kPShader = new Vector<PixelShader>();
    /** The AlphaState blending modes */
    protected Vector<AlphaState> m_kAlphaState = new Vector<AlphaState>();

    // internal use
    /** The renderers call this to load the shader programs from disk.  If
     * a program load fails, the shader object is replaced by the default
     * shader object.  Conditions for failure:
     * (1) The shader program file is not found.
     * (2) The output of the vertex program and the input of the pixel
     *     program are not compatible.
     * (3) The shader program requires more color parameters than supported
     *     by the renderer.
     * (4) The pixel program requires more texture iamge units than supported
     *     by the renderer.
     * (5) The shader program requires more texture coordinate sets than
     *     supported by the renderer.
     *
     * @param iPass the ith rendering pass
     * @param iMaxColors the maximum colors supported by the renderer
     * @param iMaxTCoords the maximum texture coordinaets sets supported by the renderer
     * @param iMaxVShaderImages the maximum vertex-shader texture images sets supported by the renderer
     * @param iMaxPShaderImages the maximum pixel-shader texture images sets supported by the renderer
     */
    public void LoadPrograms (int iPass, int iMaxColors, int iMaxTCoords,
                              int iMaxVShaderImages, int iMaxPShaderImages)
    {
        assert(0 <= iPass && iPass < m_iPassQuantity);
        assert((m_kVShader.get(iPass) != null) &&
               (m_kPShader.get(iPass) != null));

        Program pkVProgram = m_kVShader.get(iPass).GetProgram();
        Program pkPProgram = m_kPShader.get(iPass).GetProgram();
        assert((pkVProgram != null) == (pkPProgram != null));
        if (pkVProgram != null)
        {
            // The programs have already been loaded.
            return;
        }

        VertexProgram spkVProgram =
            VertexProgramCatalog.GetActive().
            Find( m_kVShader.get(iPass).GetShaderName());
        assert(spkVProgram != null);

        PixelProgram spkPProgram =
            PixelProgramCatalog.GetActive().
            Find( m_kPShader.get(iPass).GetShaderName());
        assert(spkPProgram != null);

        // Ensure that the output of the vertex program and the input of the
        // pixel program are compatible.  Each vertex program always has a clip
        // position output.  This is not relevant to the compatibility check.
        String kDefault = new String("Default");
        Attributes rkVOAttr = spkVProgram.GetOutputAttributes();
        Attributes rkPIAttr = spkPProgram.GetInputAttributes();
        if (!rkVOAttr.Matches(rkPIAttr,false,true,true,true))
        {
            // The output attributes of the vertex program and the input
            // attributes of the pixel program are incompatible.  Use the default
            // shader objects.
            if (!spkVProgram.GetName().equals(kDefault))
            {
                m_kVShader.set(iPass, new VertexShader(kDefault));
                spkVProgram = VertexProgramCatalog.GetActive().Find(kDefault);
                assert(spkVProgram != null);
            }

            if (!spkPProgram.GetName().equals(kDefault))
            {
                m_kPShader.set(iPass, new PixelShader(kDefault));
                spkPProgram = PixelProgramCatalog.GetActive().Find(kDefault);
                assert(spkPProgram != null);
            }
        }

        // Verify the shader program does not require more resources than the
        // renderer can support.
        Attributes rkVIAttr = spkVProgram.GetInputAttributes();
        if (rkVIAttr.GetMaxColors()  > iMaxColors
            ||  rkVIAttr.GetMaxTCoords() > iMaxTCoords
            ||  rkVOAttr.GetMaxColors()  > iMaxColors
            ||  rkVOAttr.GetMaxTCoords() > iMaxTCoords
            ||  rkPIAttr.GetMaxColors()  > iMaxColors
            ||  rkPIAttr.GetMaxTCoords() > iMaxTCoords
            ||  spkVProgram.GetSIQuantity() > iMaxVShaderImages
            ||  spkPProgram.GetSIQuantity() > iMaxPShaderImages)
        {
            // The renderer cannot support the requested resources.
            if (!spkVProgram.GetName().equals(kDefault))
            {
                m_kVShader.set(iPass, new VertexShader(kDefault));
                spkVProgram = VertexProgramCatalog.GetActive().Find(kDefault);
                assert(spkVProgram != null);
            }

            if (!spkPProgram.GetName().equals(kDefault))
            {
                m_kPShader.set(iPass, new PixelShader(kDefault));
                spkPProgram = PixelProgramCatalog.GetActive().Find(kDefault);
                assert(spkPProgram != null);
            }
        }

        m_kVShader.get(iPass).OnLoadProgram(spkVProgram);
        m_kPShader.get(iPass).OnLoadProgram(spkPProgram);
        OnLoadPrograms(iPass,spkVProgram,spkPProgram);
    }

    /**
     * Releases the Vertex and Pixel programs
     * @param iPass the ith rendering pass
     */
    public void ReleasePrograms (int iPass)
    {
        OnReleasePrograms(iPass,m_kVShader.get(iPass).GetProgram(),
                          m_kPShader.get(iPass).GetProgram());
        m_kVShader.get(iPass).OnReleaseProgram();
        m_kPShader.get(iPass).OnReleaseProgram();
    }

    /** This function is called in LoadPrograms once the shader programs are
     * created.  It gives the ShaderEffect-derived classes a chance to do
     * any additional work to hook up the effect with the low-level objects.
     * @param iPass the ith rendering pass
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        // Stub for derived classes.
    }

    /** This function is called in ReleasePrograms once the shader programs
     * are ready to be released.  It gives the ShaderEffect-derived classes
     * a chance to do any additional work to unhook the low-level objects
     * from the effect.
     * @param iPass the ith rendering pass
     */
    public void OnReleasePrograms (int iPass, Program pkVProgram,
                                   Program pkPProgram)
    {
        // Stub for derived classes.
    }


    /** streaming constructor */
    public ShaderEffect ()
    {
        m_iPassQuantity = 0;
    }

    /**
     * Returns the GraphicsObject with the name that matches the input paramter, rkName.
     * @param rkName, the name of the object to return.
     * @return the GraphicsObject that matches the input name.
     */
    public GraphicsObject GetObjectByName (final String rkName)
    {
        GraphicsObject pkFound = super.GetObjectByName(rkName);
        if (pkFound != null)
        {
            return pkFound;
        }

        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            if (m_kVShader.get(iPass) != null)
            {
                pkFound = m_kVShader.get(iPass).GetObjectByName(rkName);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }

            if (m_kPShader.get(iPass) != null)
            {
                pkFound = m_kPShader.get(iPass).GetObjectByName(rkName);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        return null;
    }

    /**
     * Writes all GraphicsObjects with the name that matches the input
     * paramter, rkName into the Vector paramter rkObjects.
     * @param rkName, the name of the objects to return.
     * @param rkObjects, a Vector of all objects with the matching name.
     */
    public void GetAllObjectsByName (final String rkName,
                                     Vector<GraphicsObject> rkObjects)
    {
        super.GetAllObjectsByName(rkName,rkObjects);

        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            if (m_kVShader.get(iPass) != null)
            {
                m_kVShader.get(iPass).GetAllObjectsByName(rkName,rkObjects);
            }

            if (m_kPShader.get(iPass) != null)
            {
                m_kPShader.get(iPass).GetAllObjectsByName(rkName,rkObjects);
            }
        }
    }
    
    /**
     * Returns the GraphicsObject with the ID that matches the input paramter, uiID.
     * @param uiID, the ID of the object to return.
     * @return the GraphicsObject that matches the input name.
     */
    public GraphicsObject GetObjectByID (int uiID)
    {
        GraphicsObject pkFound = super.GetObjectByID(uiID);
        if (pkFound != null)
        {
            return pkFound;
        }

        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            if (m_kVShader.get(iPass) != null)
            {
                pkFound = m_kVShader.get(iPass).GetObjectByID(uiID);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }

            if (m_kPShader.get(iPass) != null)
            {
                pkFound = m_kPShader.get(iPass).GetObjectByID(uiID);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        return null;
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
        m_iPassQuantity = rkStream.ReadInt();
        m_kVShader.setSize(m_iPassQuantity);
        m_kPShader.setSize(m_iPassQuantity);

        // link data
        int iLinkID;
        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            iLinkID = rkStream.ReadInt();  // m_kVShader[iPass]
            pkLink.Add(iLinkID);
            iLinkID = rkStream.ReadInt();  // m_kPShader[iPass]
            pkLink.Add(iLinkID);
        }

        int iQuantity = rkStream.ReadInt();
        m_kAlphaState.setSize(iQuantity);

        for (int i = 0; i < iQuantity; i++)
        {
            iLinkID = rkStream.ReadInt();  // m_kAlphaState[i]
            pkLink.Add(iLinkID);
        }
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

        int iLinkID;
        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            iLinkID = pkLink.GetLinkID();
            m_kVShader.set(iPass, (VertexShader)rkStream.GetFromMap(iLinkID));
            iLinkID = pkLink.GetLinkID();
            m_kPShader.set(iPass, (PixelShader)rkStream.GetFromMap(iLinkID));
        }

        int iQuantity = m_kAlphaState.size();
        for (int i = 0; i < iQuantity; i++)
        {
            iLinkID = pkLink.GetLinkID();
            m_kAlphaState.set(i, (AlphaState)rkStream.GetFromMap(iLinkID));
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

        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            if (m_kVShader.get(iPass) != null)
            {
                m_kVShader.get(iPass).Register(rkStream);
            }

            if (m_kPShader.get(iPass) != null)
            {
                m_kPShader.get(iPass).Register(rkStream);
            }
        }

        for (int i = 0; i < m_kAlphaState.size(); i++)
        {
            if (m_kAlphaState.get(i) != null)
            {
                m_kAlphaState.get(i).Register(rkStream);
            }
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
        rkStream.Write(m_iPassQuantity);

        // link data
        for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
        {
            rkStream.Write(m_kVShader.get(iPass).GetID());
            rkStream.Write(m_kPShader.get(iPass).GetID());
        }

        int iQuantity = m_kAlphaState.size();
        rkStream.Write(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            rkStream.Write(m_kAlphaState.get(i).GetID());
        }
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(m_iPassQuantity) +
            m_iPassQuantity*Stream.SIZEOF_INT + //m_iPassQuantity*sizeof(m_kVShader[0]) +
            m_iPassQuantity*Stream.SIZEOF_INT + //m_iPassQuantity*sizeof(m_kPShader[0]) +
            Stream.SIZEOF_INT + //sizeof(int) +
            m_kAlphaState.size()*Stream.SIZEOF_INT; //((int)m_kAlphaState.size())*sizeof(m_kAlphaState[0]);
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
        pkTree.Append(StringTree.Format("ShaderEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("pass quantity =",m_iPassQuantity));

        // children
        if (m_iPassQuantity > 0)
        {
            StringTree pkCTree = new StringTree();
            pkCTree.Append(StringTree.Format("shaders"));
            for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
            {
                pkCTree.Append(m_kVShader.get(iPass).SaveStrings(null));
                pkCTree.Append(m_kPShader.get(iPass).SaveStrings(null));
            }
            pkTree.Append(pkCTree);
        }

        int iQuantity = m_kAlphaState.size();
        if (iQuantity > 0)
        {
            StringTree pkCTree = new StringTree();
            pkCTree.Append(StringTree.Format("blending states"));
            for (int i = 0; i < iQuantity; i++)
            {
                pkCTree.Append(m_kAlphaState.get(i).SaveStrings(null));
            }
            pkTree.Append(pkCTree);
        }

        return pkTree;
    }
}
