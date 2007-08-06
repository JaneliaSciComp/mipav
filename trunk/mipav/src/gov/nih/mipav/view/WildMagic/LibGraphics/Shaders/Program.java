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

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;
import java.io.FileNotFoundException;

import com.sun.opengl.cg.*;

import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
public abstract class Program extends Bindable
    implements StreamInterface
{
    
    public Program ()
    {
        m_kContext = CgGL.cgCreateContext();
     }

    public void finalize ()
    {
        CgGL.cgDestroyContext( m_kContext );
        super.finalize();
    }
    
    // Member read-only access.
    public String GetProgramText ()
    {
        return m_kProgramText;
    }
    
    public Attributes GetInputAttributes ()
    {
        return m_kInputAttributes;
    }
    
    public Attributes GetOutputAttributes ()
    {
        return m_kOutputAttributes;
    }
    
    // Access to renderer constants.
    public int GetRCQuantity ()
    {
        return (int)m_kRendererConstants.size();
    }

    public RendererConstant GetRC (int i)
    {
        if (0 <= i && i < (int)m_kRendererConstants.size())
        {
            return m_kRendererConstants.get(i);
        }

        assert(false);
        return null;
    }

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


    // Access to numerical constants.
    public int GetNCQuantity ()
    {
        return (int)m_kNumericalConstants.size();
    }

    public NumericalConstant GetNC (int i)
    {
        if (0 <= i && i < (int)m_kNumericalConstants.size())
        {
            return m_kNumericalConstants.get(i);
        }

        assert(false);
        return null;
    }


    // Access to user constants.
    public int GetUCQuantity ()
    {
        return (int)m_kUserConstants.size();
    }

    public UserConstant GetUC (int i)
    {
        if (0 <= i && i < (int)m_kUserConstants.size())
        {
            return m_kUserConstants.get(i);
        }

        assert(false);
        return null;
    }

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


    // Access to samplers.
    public int GetSIQuantity ()
    {
        return (int)m_kSamplerInformation.size();
    }

    public SamplerInformation GetSI (int i)
    {
        if (0 <= i && i < (int)m_kSamplerInformation.size())
        {
            return m_kSamplerInformation.get(i);
        }

        assert(false);
        return null;
    }

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

    /**
     * Checks and prints Cg error messages:
     */
    protected void CheckCgError()
    {
        int err = CgGL.cgGetError();
        
        if (err != CgGL.CG_NO_ERROR) {
            System.err.println("CG error: " + CgGL.cgGetErrorString(err));
            System.err.println( CgGL.cgGetLastListing(m_kContext) );
            System.exit(1);
        }
    }

    private void RecurseParams( CGparameter param )
    {
        if (param == null)
            return;
        do
        {
            switch( CgGL.cgGetParameterType(param) )
            {
            case CgGL.CG_STRUCT :
                RecurseParams( CgGL.cgGetFirstStructParameter(param) );
                break;
                
            case CgGL.CG_ARRAY :
                {
                    int ArraySize = CgGL.cgGetArraySize(param, 0);
                    int i;
                    
                    for(i=0; i < ArraySize; ++i)
                        RecurseParams( CgGL.cgGetArrayParameter(param, i));
                }
                break;
                
            default :
                String kParamSemantic = CgGL.cgGetParameterSemantic(param);
                String kParamName = CgGL.cgGetParameterName(param);
                int iBaseRegister = (int)CgGL.cgGetParameterResourceIndex(param);
                String kParamType = CgGL.cgGetTypeString( CgGL.cgGetParameterType(param) );
                String kParamResource = CgGL.cgGetResourceString( CgGL.cgGetParameterResource(param) );
                                        
//                 System.err.println( CgGL.cgGetEnumString( CgGL.cgGetParameterDirection(param) ) + " " + 
//                                     CgGL.cgGetEnumString( CgGL.cgGetParameterVariability(param) ) + " " + 
//                                     kParamType + " " +
//                                     kParamName + " " + 
//                                     kParamSemantic + " " + 
//                                     kParamResource + " " +
//                                     iBaseRegister + " " +
//                                     CgGL.cgGetResourceString(CgGL.cgGetParameterBaseResource(param) ) );

                Attributes kInOutAttributes;
                int iNumFloats = 1;

                switch ( CgGL.cgGetParameterType(param) )
                {
                case CgGL.CG_FLOAT2: iNumFloats = 2; break;
                case CgGL.CG_FLOAT3: iNumFloats = 3; break;
                case CgGL.CG_FLOAT4: iNumFloats = 4; break;
                case CgGL.CG_FLOAT4x4: iNumFloats = 16; break;
                default: break;
                }
                int iRegisterQuantity = iNumFloats/4;
                if (iRegisterQuantity == 0)
                {
                    iRegisterQuantity = 1;
                }

                boolean bIn = true;
                if ( CgGL.cgGetParameterDirection(param) == CgGL.CG_IN )
                {
                    kInOutAttributes = m_kInputAttributes;
                }
                else
                {
                    bIn = false;
                    kInOutAttributes = m_kOutputAttributes;
                }
                if ( kParamSemantic.equals( ms_kPositionStr ) )
                {
                    if (bIn)
                        kInOutAttributes.SetPChannels(3);
                    else
                        kInOutAttributes.SetPChannels(iNumFloats);
                }
                else if ( kParamSemantic.equals( ms_kNormalStr ) )
                {
                    if (bIn)
                        kInOutAttributes.SetNChannels(3);
                    else
                        kInOutAttributes.SetNChannels(iNumFloats);
                }
                else if ( kParamSemantic.equals( ms_kColorStr ) ||
                          kParamSemantic.equals( ms_kColor0Str )   )
                {
                    kInOutAttributes.SetCChannels(0,iNumFloats);
                }
                else if ( kParamSemantic.equals( ms_kColor1Str ) )
                {
                    kInOutAttributes.SetCChannels(1,iNumFloats);
                }
                else if ( kParamSemantic.startsWith( ms_kTexCoordStr ) )
                {
                    int iUnit = 0;
                    if ( !kParamSemantic.equals( ms_kTexCoordStr ) )
                    {
                        iUnit = (int)kParamSemantic.charAt(8) - '0';
                        
                    }
                    kInOutAttributes.SetTChannels(iUnit,iNumFloats);
                }
                else if ( kParamResource.startsWith( ms_kTexUnitString ) )
                {
                    SamplerInformation.Type eSType = SamplerInformation.Type.MAX_SAMPLER_TYPES;
                    if (kParamType.equals( ms_kSampler1DStr ))
                    {
                        eSType = SamplerInformation.Type.SAMPLER_1D;
                    }
                    else if (kParamType.equals( ms_kSampler2DStr ))
                    {
                        eSType = SamplerInformation.Type.SAMPLER_2D;
                    }
                    else if (kParamType.equals( ms_kSampler3DStr ))
                    {
                        eSType = SamplerInformation.Type.SAMPLER_3D;
                    }
                    else if (kParamType.equals( ms_kSamplerCubeStr ))
                    {
                        eSType = SamplerInformation.Type.SAMPLER_CUBE;
                    }
                    else if (kParamType.equals( ms_kSamplerProjStr ))
                    {
                        eSType = SamplerInformation.Type.SAMPLER_PROJ;
                    }

                    int iUnit = iBaseRegister;
                    SamplerInformation kSU = new SamplerInformation(kParamName,eSType,iUnit);
                    m_kSamplerInformation.add(kSU);
                }
                else if ( kParamResource.equals( ms_kUserString ) )
                {
                    // The variable is either a render state or user-defined.
                    RendererConstant.Type eRCType = RendererConstant.GetType(kParamName);
                    if (eRCType != RendererConstant.Type.MAX_TYPES)
                    {
                        // renderer constant
                        RendererConstant kRC = new RendererConstant(eRCType,iBaseRegister,
                                                                    iRegisterQuantity);
                        m_kRendererConstants.add(kRC);
                    }
                    else
                    {
                        // user-defined constant
                        UserConstant kUC = new UserConstant(GetName(), kParamName,iBaseRegister,
                                                            iRegisterQuantity,iNumFloats);
                        m_kUserConstants.add(kUC);
                    }
                }

            }
        } while((param = CgGL.cgGetNextParameter(param)) != null);
    }

    protected void RecurseParamsInProgram( )
    {
        RecurseParams( CgGL.cgGetFirstParameter( m_kCGProgram, CgGL.CG_PROGRAM ) );
    }

    protected CGprogram m_kCGProgram = null;

    // The program as a text string.
    protected String m_kProgramText = new String();

    // The format of the input and output parameters to the shader program.
    protected Attributes m_kInputAttributes = new Attributes();
    protected Attributes m_kOutputAttributes = new Attributes();

    // The renderer constants required by the shader program.
    protected Vector<RendererConstant> m_kRendererConstants = new Vector<RendererConstant>();

    // The numerical constants required by the shader program.
    protected Vector<NumericalConstant> m_kNumericalConstants = new Vector<NumericalConstant>();

    // The user constants required by the shader program.  These are set by
    // the applications as needed.
    protected Vector<UserConstant> m_kUserConstants = new Vector<UserConstant>();

    // Information about the sampler units required by a shader program.
    protected Vector<SamplerInformation> m_kSamplerInformation = new Vector<SamplerInformation>();

    // For use by the constructor for loading and parsing a shader program.
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

    /** Cg Context */
    protected CGcontext m_kContext = null;
    
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);
    } 

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("Program",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}
