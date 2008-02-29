// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package gov.nih.mipav.view.WildMagic.LibGraphics.Shaders;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class Program extends Bindable
    implements StreamInterface
{
    public static int VERTEX = 0;
    public static int PIXEL = 1;
    
    /** Create a Program and GcGL context. */
    public Program ()
    {
        //m_kContext = CgGL.cgCreateContext();
     }

    /** Delete memory. Destroy the CgGL context. */
    public void Remove ()
    {
        //CgGL.cgDestroyContext( m_kContext );
        //m_kContext = null;
        //m_kCGProgram = null;
        m_kProgramText = null;
        if ( m_kInputAttributes != null )
        {
            m_kInputAttributes.dispose();
            m_kInputAttributes = null;
        }
        if ( m_kOutputAttributes != null )
        {
            m_kOutputAttributes.dispose();
            m_kOutputAttributes = null;
        }
        m_kRendererConstants.clear();
        m_kRendererConstants = null;
        m_kUserConstants.clear();
        m_kUserConstants = null;
        m_kSamplerInformation.clear();
        m_kSamplerInformation = null;

        super.Remove();
    }

    public void Reset ()
    {
        m_kInputAttributes = new Attributes();
        m_kInputAttributes = new Attributes();

        m_kRendererConstants.clear();
        m_kUserConstants.clear();
        m_kSamplerInformation.clear();
    }
    
    /** Return the program text. 
     * @return the program text. 
     */
    public final String GetProgramText ()
    {
        return m_kProgramText;
    }
    
    /** Set the program text. 
     * param the program text. 
     */
    public final void SetProgramText ( String kText )
    {
        m_kProgramText = null;
        m_kProgramText = new String(kText);
    }
    
    /** Return the program type string, its extension. 
     * @return the program type. 
     */
    public final String GetProgramType ()
    {
        return m_kProgramType;
    }    
    /** Return the program type string, its extension. 
     * @return the program type. 
     */
    public final void SetProgramType ( String kType )
    {
        m_kProgramType = null;
        m_kProgramType = new String(kType);
    }

    public boolean IsParsed()
    {
        return m_bParsed;
    }
    
    public void SetParsed()
    {
        m_bParsed = true;
    }
    
    public boolean IsCompiled()
    {
        return m_bCompiled;
    }
    
    public void SetCompiled( boolean bFlag )
    {
        m_bCompiled = bFlag;
    }
    
    public int GetShaderID()
    {
        return m_iShaderID;
    }
    
    public void SetShaderID( int iID )
    {
        m_iShaderID = iID;
    }
    
    public int GetProgramID()
    {
        return m_iProgramID;
    }
    
    public void SetProgramID( int iID )
    {
        m_iProgramID = iID;
    }
    
    /** Return the program input attributes. 
     * @return the program input attribtues. 
     */
    public final Attributes GetInputAttributes ()
    {
        return m_kInputAttributes;
    }
    
    /** Return the program output attributes. 
     * @return the program output attribtues. 
     */
    public final Attributes GetOutputAttributes ()
    {
        return m_kOutputAttributes;
    }
    
    /** Set the program output attributes. 
     * param the program output attribtues. 
     */
    public final void SetOutputAttributes (Attributes kAttributes)
    {
        m_kOutputAttributes = kAttributes;
    }    
    
    /** Set the program input attributes. 
     * param the program input attribtues. 
     */
    public final void SetInputAttributes (Attributes kAttributes)
    {
        m_kInputAttributes = kAttributes;
    }
    
    /** Access to renderer constants.
     * @return number of renderer constants.
     */
    public final int GetRCQuantity ()
    {
        return (int)m_kRendererConstants.size();
    }

    /** Access to renderer constants.
     * @param i, RendererConstant to return.
     * @return RendererConstant at position i.
     */
    public RendererConstant GetRC (int i)
    {
        if (0 <= i && i < (int)m_kRendererConstants.size())
        {
            return m_kRendererConstants.get(i);
        }

        assert(false);
        return null;
    }

    /** Access to renderer constants.
     * @param eType, type of RendererConstant to return.
     * @return RendererConstant matching input type.
     */
    public RendererConstant GetRC (RendererConstant.Type eType)
    {
        for (int i = 0; i < (int)m_kRendererConstants.size(); i++)
        {
            if (eType == m_kRendererConstants.get(i).GetType())
            {
                return m_kRendererConstants.get(i);
            }
        }

        assert(false);
        return null;
    }

    /** Access to renderer constants.
     * @param kRC
     */
    public void AddRendererConstant(RendererConstant kRC)
    {
        m_kRendererConstants.add(kRC);
    }

    /** Access to user constants.
     * @return number of use constants.
     */
    public final int GetUCQuantity ()
    {
        return (int)m_kUserConstants.size();
    }

    /** Access to user constants.
     * @param i, UserConstant to return.
     * @return UserConstant at position i.
     */
    public UserConstant GetUC (int i)
    {
        if (0 <= i && i < (int)m_kUserConstants.size())
        {
            return m_kUserConstants.get(i);
        }

        assert(false);
        return null;
    }

    /** Access to user constants.
     * @param rkName, name of the UserConstant to return.
     * @return UserConstant with the input name.
     */
    public UserConstant GetUC ( String rkName)
    {
        for (int i = 0; i < (int)m_kUserConstants.size(); i++)
        {
            if (rkName.equals(m_kUserConstants.get(i).GetName()))
            {
                return m_kUserConstants.get(i);
            }
        }

        assert(false);
        return null;
    }

    /** Access to user constants.
     * @param kUC
     */
    public void AddUserConstant(UserConstant kUC)
    {
        m_kUserConstants.add(kUC);
    }

    /** Access to samplers.
     * @return number of samplers.
     */
    public final int GetSIQuantity ()
    {
        return (int)m_kSamplerInformation.size();
    }

    /** Access to samplers.
     * @param i, sampler to return
     * @return Sampler at position i.
     */
    public SamplerInformation GetSI (int i)
    {
        if (0 <= i && i < (int)m_kSamplerInformation.size())
        {
            return m_kSamplerInformation.get(i);
        }

        assert(false);
        return null;
    }

    /** Access to samplers.
     * @param rkName, name of sampler to return
     * @return Sampler with input name.
     */
    public SamplerInformation GetSI ( String rkName)
    {
        for (int i = 0; i < (int)m_kSamplerInformation.size(); i++)
        {
            if (rkName.equals(m_kSamplerInformation.get(i).GetName()))
            {
                return m_kSamplerInformation.get(i);
            }
        }
        
        assert(false);
        return null;
    }

    /** Access to Sampler Information.
     * @param kSI
     */
    public void AddSamplerInformation(SamplerInformation kSI)
    {
        m_kSamplerInformation.add(kSI);
    }
    
    /** The type of program, from file extension. */
    private String m_kProgramType = new String();

    /** The program as a text string. */
    private String m_kProgramText = new String();
    
    /** Flag indicating if the program is compiled or not. */
    private boolean m_bCompiled = false;
    private boolean m_bParsed = false;
    
    /** The shader ID for compiling programs. */
    private int m_iShaderID;
    /** The shader ID for compiling programs. */
    private int m_iProgramID = -1;

    /** The format of the input and output parameters to the shader program. */
    private Attributes m_kInputAttributes = new Attributes();
    /** The format of the input and output parameters to the shader program. */
    private Attributes m_kOutputAttributes = new Attributes();

    /** The renderer constants required by the shader program. */
    private Vector<RendererConstant> m_kRendererConstants = new Vector<RendererConstant>();

    /** The user constants required by the shader program.  These are set by
     * the applications as needed. */
    private Vector<UserConstant> m_kUserConstants = new Vector<UserConstant>();

    /** Information about the sampler units required by a shader program. */
    private Vector<SamplerInformation> m_kSamplerInformation = new Vector<SamplerInformation>();

    /** For use by the constructor for loading and parsing a shader
     * program. */
    protected static final String ms_kSampler1DStr = new String("sampler1D");
    protected static final String ms_kSampler2DStr = new String("sampler2D");
    protected static final String ms_kSampler3DStr = new String("sampler3D");
    protected static final String ms_kSamplerCubeStr = new String("samplerCUBE");
    protected static final String ms_kSamplerProjStr = new String("sampler2DSHADOW");
    protected static final String ms_kPositionStr = new String("POSITION");
    protected static final String ms_kNormalStr = new String("NORMAL");
    protected static final String ms_kColorStr = new String("COLOR");
    protected static final String ms_kColor0Str = new String("COLOR0");
    protected static final String ms_kColor1Str = new String("COLOR1");
    protected static final String ms_kTexCoordStr = new String("TEXCOORD");
    protected static final String ms_kInStr = new String("in");
    protected static final String ms_kEOL = new String("\n");
    protected static final String ms_kTexUnitString = new String("texunit");
    protected static final String ms_kUserString = new String("c");

    
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
        return super.Register(rkStream);
    }


    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
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
        pkTree.Append(StringTree.Format("Program",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}
