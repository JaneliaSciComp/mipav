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
// Version: 4.0.1 (2006/08/24)

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

    protected static boolean Load (String rkFilename, char cCommentChar,
                                   Program pkProgram)
    {
        BufferedReader kIStr;
        try { 
            kIStr = new BufferedReader( new FileReader(rkFilename) );
        } catch ( FileNotFoundException e ) {
            return false;
        }

        String kLine, kVarType, kVarName, kVarIO, kVarSemantic;
        String kRegister, kData;
        int uiBegin, uiEnd, uiSave;
        int iNumFloats, iUnit, iBaseRegister, iRegisterQuantity;
        SamplerInformation.Type eSType;
        RendererConstant.Type eRCType;

        int iLine = 0;
        try {
            kLine = kIStr.readLine();
        } catch ( IOException e ) {
            System.err.println( "Program.java.Load: readLine() error " + iLine );
            return false;
        }
        while ( kLine != null )
        {
            // The information needed by Program is contained in the shader
            // program comment lines.  All other lines are assumed to be needed
            // by the graphics API.
            if (kLine.charAt(0) != cCommentChar)
            {
                pkProgram.m_kProgramText = pkProgram.m_kProgramText.concat( kLine + ms_kEOL);

                try {
                    iLine++;
                    kLine = kIStr.readLine();
                } catch ( IOException e ) {
                    System.err.println( "Program.java.Load: readLine() error " + iLine );
                    return false;
                }
                continue;
            }

            uiBegin = kLine.indexOf("var",1);
            if (uiBegin != -1)
            {
                // Skip over "var".
                uiBegin += 3;

                // Skip over white space.
                kLine = kLine.substring(uiBegin);
                kLine = kLine.trim();
                uiBegin = 0;
                if (kLine == null)
                {
                    assert(false);
                    return false;
                }

                // Get the variable's data type.
                iNumFloats = 0;
                eSType = SamplerInformation.Type.MAX_SAMPLER_TYPES;
                uiEnd = kLine.indexOf(" ");
                kVarType = kLine.substring(0,uiEnd);
                if (kVarType.equals( ms_kFloatStr ))
                {
                    iNumFloats = 1;
                }
                else if (kVarType.equals( ms_kFloat2Str ))
                {
                    iNumFloats = 2;
                }
                else if (kVarType.equals( ms_kFloat3Str ))
                {
                    iNumFloats = 3;
                }
                else if (kVarType.equals( ms_kFloat4Str ))
                {
                    iNumFloats = 4;
                }
                else if (kVarType.equals( ms_kFloat4x4Str ))
                {
                    iNumFloats = 16;
                }
                else if (kVarType.equals( ms_kSampler1DStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_1D;
                }
                else if (kVarType.equals( ms_kSampler2DStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_2D;
                }
                else if (kVarType.equals( ms_kSampler3DStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_3D;
                }
                else if (kVarType.equals( ms_kSamplerCubeStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_CUBE;
                }
                else if (kVarType.equals( ms_kSamplerProjStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_PROJ;
                }
                else
                {
                    // The data type is not supported by Wild Magic.
                    assert(false);
                    return false;
                }

                // Skip over white space.
                kLine = kLine.substring(uiEnd+1);
                kLine = kLine.trim();
                uiBegin = 0;
                if (kLine == null)
                {
                    assert(false);
                    return false;
                }

                // Get the variable's name.
                uiEnd = kLine.indexOf(" ");
                kVarName = kLine.substring(0,uiEnd);

                // Skip over white space.
                kLine = kLine.substring(uiEnd+1);
                kLine = kLine.trim();
                uiBegin = 0;
                if (kLine == null)
                {
                    assert(false);
                    return false;
                }

                // Get sampler information (if relevant).
                if (eSType != SamplerInformation.Type.MAX_SAMPLER_TYPES)
                {
                    // Skip over "texunit".
                    uiBegin = kLine.indexOf("texunit",0);
                    if (uiBegin == -1)
                    {
                        assert(false);
                        return false;
                    }
                    uiBegin += 7;
                    if (kLine.charAt(uiBegin) != ' ')
                    {
                        assert(false);
                        return false;
                    }

                    // Get the texture unit.
                    uiBegin++;
                    if (!('0' <= kLine.charAt(uiBegin) && kLine.charAt(uiBegin) <= '7'))
                    {
                        assert(false);
                        return false;
                    }
                    iUnit = (int)kLine.charAt(uiBegin) - '0';

                    SamplerInformation kSU = new SamplerInformation(kVarName,eSType,iUnit);
                    pkProgram.m_kSamplerInformation.add(kSU);

                    try {
                        iLine++;
                        kLine = kIStr.readLine();
                    } catch ( IOException e ) {
                        System.err.println( "Program.java.Load: readLine() error " + iLine );
                        return false;
                    }
                    continue;
                }

                // Get the variable's I/O status.
                uiSave = uiBegin;
                uiBegin = kLine.indexOf("$",uiSave);
                if (uiBegin != -1)
                {
                    // The variable is either an input or output variable.
                    uiBegin += 2;
                    uiEnd = kLine.indexOf(".",uiBegin);
                    kVarIO = kLine.substring(uiBegin,uiEnd);

                    // Get the variable's semantic.
                    uiBegin = uiEnd+1;
                    uiEnd = kLine.indexOf(" ",uiBegin);
                    kVarSemantic = kLine.substring(uiBegin,uiEnd);

                    if (kVarIO.equals( ms_kInStr ))
                    {
                        if (kVarSemantic.equals( ms_kPositionStr ))
                        {
                            // Wild Magic only supplies (x,y,z) positions.
                            pkProgram.m_kInputAttributes.SetPChannels(3);
                        }
                        else if (kVarSemantic.equals( ms_kNormalStr ))
                        {
                            // Wild Magic only supplies (x,y,z) normals.
                            pkProgram.m_kInputAttributes.SetNChannels(3);
                        }
                        else if ( kVarSemantic.equals( ms_kColorStr )
                                 || kVarSemantic.equals( ms_kColor0Str ) )
                        {
                            pkProgram.m_kInputAttributes.SetCChannels(0,iNumFloats);
                        }
                        else if (kVarSemantic.equals( ms_kColor1Str ))
                        {
                            pkProgram.m_kInputAttributes.SetCChannels(1,iNumFloats);
                        }
                        else // texture coordinate
                        {
                            if (!kVarSemantic.substring(0,8).equals( ms_kTexCoordStr ))
                            {
                                assert(false);
                                return false;
                            }
                            iUnit = (int)kVarSemantic.charAt(8) - '0';
                            pkProgram.m_kInputAttributes.SetTChannels(iUnit,iNumFloats);
                        }
                    }
                    else  // kVarIO == String("out")
                    {
                        if (kVarSemantic.equals( ms_kPositionStr ))
                        {
                            pkProgram.m_kOutputAttributes.SetPChannels(iNumFloats);
                        }
                        else if (kVarSemantic.equals( ms_kNormalStr ))
                        {
                            pkProgram.m_kOutputAttributes.SetNChannels(iNumFloats);
                        }
                        else if (kVarSemantic.equals( ms_kColorStr )
                                 || kVarSemantic.equals( ms_kColor0Str ))
                        {
                            pkProgram.m_kOutputAttributes.SetCChannels(0,iNumFloats);
                        }
                        else if (kVarSemantic.equals( ms_kColor1Str ))
                        {
                            pkProgram.m_kOutputAttributes.SetCChannels(1,iNumFloats);
                        }
                        else // texture coordinate
                        {
                            if (!kVarSemantic.substring(0,8).equals( ms_kTexCoordStr ))
                            {
                                assert(false);
                                return false;
                            }
                            iUnit = (int)kVarSemantic.charAt(8) - '0';
                            pkProgram.m_kOutputAttributes.SetTChannels(iUnit,iNumFloats);
                        }
                    }
                }
                else
                {
                    // The variable is stored in a constant register.
                    uiBegin = kLine.indexOf("c[",uiSave);
                    if (uiBegin == -1)
                    {
                        assert(false);
                        return false;
                    }
                    uiEnd = kLine.indexOf("]",uiBegin);
                    uiBegin += 2;
                    kRegister = kLine.substring(uiBegin,uiEnd);
                    iBaseRegister = (new Integer(kRegister)).intValue();

                    iRegisterQuantity = iNumFloats/4;
                    if (iRegisterQuantity == 0)
                    {
                        iRegisterQuantity = 1;
                    }

                    // The variable is either a render state or user-defined.
                    eRCType = RendererConstant.GetType(kVarName);
                    if (eRCType != RendererConstant.Type.MAX_TYPES)
                    {
                        // renderer constant
                        RendererConstant kRC = new RendererConstant(eRCType,iBaseRegister,
                                                                    iRegisterQuantity);
                        pkProgram.m_kRendererConstants.add(kRC);
                    }
                    else
                    {
                        // user-defined constant
                        UserConstant kUC = new UserConstant(pkProgram.GetName(), kVarName,iBaseRegister,
                                                            iRegisterQuantity,iNumFloats);
                        pkProgram.m_kUserConstants.add(kUC);
                    }
                }

                try {
                    iLine++;
                    kLine = kIStr.readLine();
                } catch ( IOException e ) {
                    System.err.println( "Program.java.Load: readLine() error " + iLine );
                    return false;
                }
                continue;
            }

            uiBegin = kLine.indexOf("const",1);
            if (uiBegin != -1)
            {
                // A numerical constant register has been found.
                uiBegin = kLine.indexOf("c[");
                if (uiBegin == -1)
                {
                    assert(false);
                    return false;
                }
                uiEnd = kLine.indexOf("]",uiBegin);
                uiBegin += 2;
                kRegister = kLine.substring(uiBegin,uiEnd);
                iBaseRegister = (new Integer(kRegister)).intValue();

                // Get the constant's data, which occurs after the equality.
                float afData[] = new float[]{ 0.0f, 0.0f, 0.0f, 0.0f };
                uiBegin = kLine.indexOf("=");
                if (uiBegin == -1)
                {
                    assert(false);
                    return false;
                }
                uiBegin++;
                kLine = kLine.substring(uiBegin);
                kLine = kLine.trim();
                uiBegin = 0;
                if (kLine == null)
                {
                    assert(false);
                    return false;
                }

                for (int i = 0; i < 4; i++)
                {
                    uiEnd = kLine.indexOf(" ");
                    if (uiEnd == -1)
                    {
                        afData[i] = (new Float(kLine)).floatValue();
                        break;
                    }

                    kData = kLine.substring(0,uiEnd);
                    afData[i] = (new Float(kData)).floatValue();
                    kLine = kLine.substring(uiEnd);
                    kLine = kLine.trim();
                    uiBegin = 0;
                    if (kLine == null)
                    {
                        assert(false);
                        return false;
                    }
                }

                NumericalConstant kNC = new NumericalConstant(iBaseRegister,afData);
                pkProgram.m_kNumericalConstants.add(kNC);
            }

            try {
                iLine++;
                kLine = kIStr.readLine();
            } catch ( IOException e ) {
                System.err.println( "Program.java.Load: readLine() error " + iLine );
                return false;
            }
        }

        try {
            kIStr.close();
        } catch ( IOException e ) {
            System.err.println( "Program.java.Load: close() error" );
        }
        pkProgram.m_kProgramText = pkProgram.m_kProgramText.concat(ms_kEOL);
        return true;
    }


    protected static boolean LoadCg (String kProgramString, char cCommentChar,
                                     Program pkProgram)
    {
        String kLine, kVarType, kVarName, kVarIO, kVarSemantic;
        String kRegister, kData;
        int uiBegin, uiEnd, uiSave;
        int iNumFloats, iUnit, iBaseRegister, iRegisterQuantity;
        SamplerInformation.Type eSType;
        RendererConstant.Type eRCType;

        int iLine = 0;
        int iBegin = 0;
        int iEnd = kProgramString.indexOf( ms_kEOL );
        if ( iEnd != -1 )
        {
            kLine = kProgramString.substring( iBegin, iEnd );
        }
        else
        {
            kLine = null;
        }
        while ( kLine != null )
        {
            // The information needed by Program is contained in the shader
            // program comment lines.  All other lines are assumed to be needed
            // by the graphics API.
            if (kLine.charAt(0) != cCommentChar)
            {
                pkProgram.m_kProgramText = pkProgram.m_kProgramText.concat( kLine + ms_kEOL);

                iLine++;
                iBegin = iEnd+1;
                iEnd = kProgramString.indexOf( ms_kEOL, iBegin );
                if ( iEnd != -1 )
                {
                    kLine = kProgramString.substring( iBegin, iEnd );
                }
                else
                {
                    kLine = null;
                }

                continue;
            }

            uiBegin = kLine.indexOf("var",1);
            if (uiBegin != -1)
            {
                // Skip over "var".
                uiBegin += 3;

                // Skip over white space.
                kLine = kLine.substring(uiBegin);
                kLine = kLine.trim();
                uiBegin = 0;
                if (kLine == null)
                {
                    assert(false);
                    return false;
                }

                // Get the variable's data type.
                iNumFloats = 0;
                eSType = SamplerInformation.Type.MAX_SAMPLER_TYPES;
                uiEnd = kLine.indexOf(" ");
                kVarType = kLine.substring(0,uiEnd);
                if (kVarType.equals( ms_kFloatStr ))
                {
                    iNumFloats = 1;
                }
                else if (kVarType.equals( ms_kFloat2Str ))
                {
                    iNumFloats = 2;
                }
                else if (kVarType.equals( ms_kFloat3Str ))
                {
                    iNumFloats = 3;
                }
                else if (kVarType.equals( ms_kFloat4Str ))
                {
                    iNumFloats = 4;
                }
                else if (kVarType.equals( ms_kFloat4x4Str ))
                {
                    iNumFloats = 16;
                }
                else if (kVarType.equals( ms_kSampler1DStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_1D;
                }
                else if (kVarType.equals( ms_kSampler2DStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_2D;
                }
                else if (kVarType.equals( ms_kSampler3DStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_3D;
                }
                else if (kVarType.equals( ms_kSamplerCubeStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_CUBE;
                }
                else if (kVarType.equals( ms_kSamplerProjStr ))
                {
                    eSType = SamplerInformation.Type.SAMPLER_PROJ;
                }
                else
                {
                    // The data type is not supported by Wild Magic.
                    assert(false);
                    return false;
                }

                // Skip over white space.
                kLine = kLine.substring(uiEnd+1);
                kLine = kLine.trim();
                uiBegin = 0;
                if (kLine == null)
                {
                    assert(false);
                    return false;
                }

                // Get the variable's name.
                uiEnd = kLine.indexOf(" ");
                kVarName = kLine.substring(0,uiEnd);

                // Skip over white space.
                kLine = kLine.substring(uiEnd+1);
                kLine = kLine.trim();
                uiBegin = 0;
                if (kLine == null)
                {
                    assert(false);
                    return false;
                }

                // Get sampler information (if relevant).
                if (eSType != SamplerInformation.Type.MAX_SAMPLER_TYPES)
                {
                    // Skip over "texunit".
                    uiBegin = kLine.indexOf("texunit",0);
                    if (uiBegin == -1)
                    {
                        assert(false);
                        return false;
                    }
                    uiBegin += 7;
                    if (kLine.charAt(uiBegin) != ' ')
                    {
                        assert(false);
                        return false;
                    }

                    // Get the texture unit.
                    uiBegin++;
                    if (!('0' <= kLine.charAt(uiBegin) && kLine.charAt(uiBegin) <= '7'))
                    {
                        assert(false);
                        return false;
                    }
                    iUnit = (int)kLine.charAt(uiBegin) - '0';

                    SamplerInformation kSU = new SamplerInformation(kVarName,eSType,iUnit);
                    pkProgram.m_kSamplerInformation.add(kSU);

                    iLine++;
                    iBegin = iEnd+1;
                    iEnd = kProgramString.indexOf( ms_kEOL, iBegin );
                    if ( iEnd != -1 )
                    {
                        kLine = kProgramString.substring( iBegin, iEnd );
                    }
                    else
                    {
                        kLine = null;
                    }

                    continue;
                }

                // Get the variable's I/O status.
                uiSave = uiBegin;
                uiBegin = kLine.indexOf("$",uiSave);
                if (uiBegin != -1)
                {
                    // The variable is either an input or output variable.
                    uiBegin += 2;
                    uiEnd = kLine.indexOf(".",uiBegin);
                    kVarIO = kLine.substring(uiBegin,uiEnd);

                    // Get the variable's semantic.
                    uiBegin = uiEnd+1;
                    uiEnd = kLine.indexOf(" ",uiBegin);
                    kVarSemantic = kLine.substring(uiBegin,uiEnd);

                    if (kVarIO.equals( ms_kInStr ))
                    {
                        if (kVarSemantic.equals( ms_kPositionStr ))
                        {
                            // Wild Magic only supplies (x,y,z) positions.
                            pkProgram.m_kInputAttributes.SetPChannels(3);
                        }
                        else if (kVarSemantic.equals( ms_kNormalStr ))
                        {
                            // Wild Magic only supplies (x,y,z) normals.
                            pkProgram.m_kInputAttributes.SetNChannels(3);
                        }
                        else if ( kVarSemantic.equals( ms_kColorStr )
                                 || kVarSemantic.equals( ms_kColor0Str ) )
                        {
                            pkProgram.m_kInputAttributes.SetCChannels(0,iNumFloats);
                        }
                        else if (kVarSemantic.equals( ms_kColor1Str ))
                        {
                            pkProgram.m_kInputAttributes.SetCChannels(1,iNumFloats);
                        }
                        else // texture coordinate
                        {
                            if (!kVarSemantic.substring(0,8).equals( ms_kTexCoordStr ))
                            {
                                assert(false);
                                return false;
                            }
                            iUnit = (int)kVarSemantic.charAt(8) - '0';
                            pkProgram.m_kInputAttributes.SetTChannels(iUnit,iNumFloats);
                        }
                    }
                    else  // kVarIO == String("out")
                    {
                        if (kVarSemantic.equals( ms_kPositionStr ))
                        {
                            pkProgram.m_kOutputAttributes.SetPChannels(iNumFloats);
                        }
                        else if (kVarSemantic.equals( ms_kNormalStr ))
                        {
                            pkProgram.m_kOutputAttributes.SetNChannels(iNumFloats);
                        }
                        else if (kVarSemantic.equals( ms_kColorStr )
                                 || kVarSemantic.equals( ms_kColor0Str ))
                        {
                            pkProgram.m_kOutputAttributes.SetCChannels(0,iNumFloats);
                        }
                        else if (kVarSemantic.equals( ms_kColor1Str ))
                        {
                            pkProgram.m_kOutputAttributes.SetCChannels(1,iNumFloats);
                        }
                        else // texture coordinate
                        {
                            if (!kVarSemantic.substring(0,8).equals( ms_kTexCoordStr ))
                            {
                                assert(false);
                                return false;
                            }
                            iUnit = (int)kVarSemantic.charAt(8) - '0';
                            pkProgram.m_kOutputAttributes.SetTChannels(iUnit,iNumFloats);
                        }
                    }
                }
                else
                {
                    // The variable is stored in a constant register.
                    uiBegin = kLine.indexOf("c[",uiSave);
                    if (uiBegin == -1)
                    {
                        assert(false);
                        return false;
                    }
                    uiEnd = kLine.indexOf("]",uiBegin);
                    uiBegin += 2;
                    kRegister = kLine.substring(uiBegin,uiEnd);
                    iBaseRegister = (new Integer(kRegister)).intValue();

                    iRegisterQuantity = iNumFloats/4;
                    if (iRegisterQuantity == 0)
                    {
                        iRegisterQuantity = 1;
                    }

                    // The variable is either a render state or user-defined.
                    eRCType = RendererConstant.GetType(kVarName);
                    if (eRCType != RendererConstant.Type.MAX_TYPES)
                    {
                        // renderer constant
                        RendererConstant kRC = new RendererConstant(eRCType,iBaseRegister,
                                                                    iRegisterQuantity);
                        pkProgram.m_kRendererConstants.add(kRC);
                    }
                    else
                    {
                        // user-defined constant
                        UserConstant kUC = new UserConstant(pkProgram.GetName(), kVarName,iBaseRegister,
                                                            iRegisterQuantity,iNumFloats);
                        pkProgram.m_kUserConstants.add(kUC);
                    }
                }

                iLine++;
                iBegin = iEnd+1;
                iEnd = kProgramString.indexOf( ms_kEOL, iBegin );
                if ( iEnd != -1 )
                {
                    kLine = kProgramString.substring( iBegin, iEnd );
                }
                else
                {
                    kLine = null;
                }

                continue;
            }

            uiBegin = kLine.indexOf("const",1);
            if (uiBegin != -1)
            {
                // A numerical constant register has been found.
                uiBegin = kLine.indexOf("c[");
                if (uiBegin == -1)
                {
                    assert(false);
                    return false;
                }
                uiEnd = kLine.indexOf("]",uiBegin);
                uiBegin += 2;
                kRegister = kLine.substring(uiBegin,uiEnd);
                iBaseRegister = (new Integer(kRegister)).intValue();

                // Get the constant's data, which occurs after the equality.
                float afData[] = new float[]{ 0.0f, 0.0f, 0.0f, 0.0f };
                uiBegin = kLine.indexOf("=");
                if (uiBegin == -1)
                {
                    assert(false);
                    return false;
                }
                uiBegin++;
                kLine = kLine.substring(uiBegin);
                kLine = kLine.trim();
                uiBegin = 0;
                if (kLine == null)
                {
                    assert(false);
                    return false;
                }

                for (int i = 0; i < 4; i++)
                {
                    uiEnd = kLine.indexOf(" ");
                    if (uiEnd == -1)
                    {
                        afData[i] = (new Float(kLine)).floatValue();
                        break;
                    }

                    kData = kLine.substring(0,uiEnd);
                    afData[i] = (new Float(kData)).floatValue();
                    kLine = kLine.substring(uiEnd);
                    kLine = kLine.trim();
                    uiBegin = 0;
                    if (kLine == null)
                    {
                        assert(false);
                        return false;
                    }
                }

                //NumericalConstant kNC = new NumericalConstant(iBaseRegister,afData);
                //pkProgram.m_kNumericalConstants.add(kNC);
            }

            iLine++;
            iBegin = iEnd+1;
            iEnd = kProgramString.indexOf( ms_kEOL, iBegin );
            if ( iEnd != -1 )
            {
                kLine = kProgramString.substring( iBegin, iEnd );
            }
            else
            {
                kLine = null;
            }
        }

        pkProgram.m_kProgramText = pkProgram.m_kProgramText.concat(ms_kEOL);

        /*
        System.err.println( "IAttr = " );
        System.err.println( pkProgram.m_kInputAttributes.GetChannelQuantity() + " " +
                            pkProgram.m_kInputAttributes.GetPOffset() + " " +
                            pkProgram.m_kInputAttributes.GetPChannels() + " " +
                            pkProgram.m_kInputAttributes.GetNOffset() + " " +
                            pkProgram.m_kInputAttributes.GetNChannels() + " " +
                            pkProgram.m_kInputAttributes.GetMaxColors() + " " +
                            pkProgram.m_kInputAttributes.GetCOffset(0) + " " +
                            pkProgram.m_kInputAttributes.GetCChannels(0) + " " +
                            pkProgram.m_kInputAttributes.HasColor(0) + " " +
                            pkProgram.m_kInputAttributes.GetMaxTCoords() + " " +
                            pkProgram.m_kInputAttributes.GetTOffset(0) + " " +
                            pkProgram.m_kInputAttributes.GetTChannels(0) + " " +
                            pkProgram.m_kInputAttributes.HasTCoord(0) );
        
        System.err.println( "OAttr = " );
        System.err.println( pkProgram.m_kOutputAttributes.GetChannelQuantity() + " " +
                            pkProgram.m_kOutputAttributes.GetPOffset() + " " +
                            pkProgram.m_kOutputAttributes.GetPChannels() + " " +
                            pkProgram.m_kOutputAttributes.GetNOffset() + " " +
                            pkProgram.m_kOutputAttributes.GetNChannels() + " " +
                            pkProgram.m_kOutputAttributes.GetMaxColors() + " " +
                            pkProgram.m_kOutputAttributes.GetCOffset(0) + " " +
                            pkProgram.m_kOutputAttributes.GetCChannels(0) + " " +
                            pkProgram.m_kOutputAttributes.HasColor(0) + " " +
                            pkProgram.m_kOutputAttributes.GetMaxTCoords() + " " +
                            pkProgram.m_kOutputAttributes.GetTOffset(0) + " " +
                            pkProgram.m_kOutputAttributes.GetTChannels(0) + " " +
                            pkProgram.m_kOutputAttributes.HasTCoord(0) );
        
        for ( int i = 0; i < pkProgram.m_kRendererConstants.size(); i++ )
        {
            RendererConstant pkRendererC = pkProgram.m_kRendererConstants.get(i);
            System.err.println( "RC: " + pkRendererC.GetType().Name() + " Register = " + pkRendererC.GetBaseRegister() + " "+  pkRendererC.GetRegisterQuantity() );
        }

        for ( int i = 0; i < pkProgram.m_kNumericalConstants.size(); i++ )
        {
            NumericalConstant pkNC = pkProgram.m_kNumericalConstants.get(i);
            System.err.println( "NC: " + pkNC.GetRegister() );
        }

        for ( int i = 0; i < pkProgram.m_kUserConstants.size(); i++ )
        {
            UserConstant pkUser = pkProgram.m_kUserConstants.get(i);
            System.err.println( "UC: " + pkUser.GetName() + " size = " + pkUser.GetDataSize() );
        }

        for ( int i = 0; i < pkProgram.m_kSamplerInformation.size(); i++ )
        {
            SamplerInformation pkSI = pkProgram.m_kSamplerInformation.get(i);
             System.err.println( "SI: " + pkSI.GetName() + " " +
                                 pkSI.GetType().Value() + " " +
                                 pkSI.GetTextureUnit() + " " +
                                 pkSI.GetDimension() );
        }
        */
        return true;
    }

    public Program () {}


    private static void RecurseParams( Program pkProgram, CGparameter param )
    {
        if (param == null)
            return;
        do
        {
            switch( CgGL.cgGetParameterType(param) )
            {
            case CgGL.CG_STRUCT :
                RecurseParams( pkProgram, CgGL.cgGetFirstStructParameter(param) );
                break;
                
            case CgGL.CG_ARRAY :
                {
                    int ArraySize = CgGL.cgGetArraySize(param, 0);
                    int i;
                    
                    for(i=0; i < ArraySize; ++i)
                        RecurseParams( pkProgram, CgGL.cgGetArrayParameter(param, i));
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
                    kInOutAttributes = pkProgram.m_kInputAttributes;
                }
                else
                {
                    bIn = false;
                    kInOutAttributes = pkProgram.m_kOutputAttributes;
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
                    pkProgram.m_kSamplerInformation.add(kSU);
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
                        pkProgram.m_kRendererConstants.add(kRC);
                    }
                    else
                    {
                        // user-defined constant
                        UserConstant kUC = new UserConstant(pkProgram.GetName(), kParamName,iBaseRegister,
                                                            iRegisterQuantity,iNumFloats);
                        pkProgram.m_kUserConstants.add(kUC);
                    }
                }

            }
        } while((param = CgGL.cgGetNextParameter(param)) != null);
    }

    protected static void RecurseParamsInProgram( Program pkProgram, CGprogram program )
    {
        RecurseParams( pkProgram, CgGL.cgGetFirstParameter( program, CgGL.CG_PROGRAM ) );

        /*
        System.err.println( "IAttr = " );
        System.err.println( pkProgram.m_kInputAttributes.GetChannelQuantity() + " " +
                            pkProgram.m_kInputAttributes.GetPOffset() + " " +
                            pkProgram.m_kInputAttributes.GetPChannels() + " " +
                            pkProgram.m_kInputAttributes.GetNOffset() + " " +
                            pkProgram.m_kInputAttributes.GetNChannels() + " " +
                            pkProgram.m_kInputAttributes.GetMaxColors() + " " +
                            pkProgram.m_kInputAttributes.GetCOffset(0) + " " +
                            pkProgram.m_kInputAttributes.GetCChannels(0) + " " +
                            pkProgram.m_kInputAttributes.HasColor(0) + " " +
                            pkProgram.m_kInputAttributes.GetMaxTCoords() + " " +
                            pkProgram.m_kInputAttributes.GetTOffset(0) + " " +
                            pkProgram.m_kInputAttributes.GetTChannels(0) + " " +
                            pkProgram.m_kInputAttributes.HasTCoord(0) );
        
        System.err.println( "OAttr = " );
        System.err.println( pkProgram.m_kOutputAttributes.GetChannelQuantity() + " " +
                            pkProgram.m_kOutputAttributes.GetPOffset() + " " +
                            pkProgram.m_kOutputAttributes.GetPChannels() + " " +
                            pkProgram.m_kOutputAttributes.GetNOffset() + " " +
                            pkProgram.m_kOutputAttributes.GetNChannels() + " " +
                            pkProgram.m_kOutputAttributes.GetMaxColors() + " " +
                            pkProgram.m_kOutputAttributes.GetCOffset(0) + " " +
                            pkProgram.m_kOutputAttributes.GetCChannels(0) + " " +
                            pkProgram.m_kOutputAttributes.HasColor(0) + " " +
                            pkProgram.m_kOutputAttributes.GetMaxTCoords() + " " +
                            pkProgram.m_kOutputAttributes.GetTOffset(0) + " " +
                            pkProgram.m_kOutputAttributes.GetTChannels(0) + " " +
                            pkProgram.m_kOutputAttributes.HasTCoord(0) );


        for ( int i = 0; i < pkProgram.m_kRendererConstants.size(); i++ )
        {
            RendererConstant pkRendererC = pkProgram.m_kRendererConstants.get(i);
            System.err.println( "RC: " + pkRendererC.GetType().Name() + " Register = " + pkRendererC.GetBaseRegister() + " "+  pkRendererC.GetRegisterQuantity() );
        }

        for ( int i = 0; i < pkProgram.m_kNumericalConstants.size(); i++ )
        {
            NumericalConstant pkNC = pkProgram.m_kNumericalConstants.get(i);
            System.err.println( "NC: " + pkNC.GetRegister() );
        }

        for ( int i = 0; i < pkProgram.m_kUserConstants.size(); i++ )
        {
            UserConstant pkUser = pkProgram.m_kUserConstants.get(i);
            System.err.println( "UC: " + pkUser.GetName() + " size = " + pkUser.GetDataSize() );
        }

        for ( int i = 0; i < pkProgram.m_kSamplerInformation.size(); i++ )
        {
            SamplerInformation pkSI = pkProgram.m_kSamplerInformation.get(i);
            System.err.println( "SI: " + pkSI.GetName() + " " +
                                pkSI.GetType().Value() + " " +
                                pkSI.GetTextureUnit() + " " +
                                pkSI.GetDimension() );
        }
        */
    }


    // The program itself, stored as a text string.
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
    protected static final String ms_kFloatStr = new String("float");
    protected static final String ms_kFloat2Str = new String("float2");
    protected static final String ms_kFloat3Str = new String("float3");
    protected static final String ms_kFloat4Str = new String("float4");
    protected static final String ms_kFloat4x4Str = new String("float4x4");
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
    protected static CGcontext ms_kContext         = null;

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
