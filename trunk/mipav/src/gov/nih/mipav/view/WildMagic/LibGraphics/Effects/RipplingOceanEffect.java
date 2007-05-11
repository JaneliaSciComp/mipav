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

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class RipplingOceanEffect extends ShaderEffect
    implements StreamInterface
{
    /** Creates a new RipplingOceanEffect 
     * @param acBumpName the name for the BumpMap image file
     * @param acWaterName the name for the Water image file
     * @param acEnvName the name for the Environment image file
     */
    public RipplingOceanEffect (final String acBumpName, final String acWaterName,
                                final String acEnvName)
    {
        super(1);
        m_kVShader.set(0, new VertexShader("RipplingOcean"));
        m_kPShader.set(0, new PixelShader("RipplingOcean"));

        m_kPShader.get(0).SetTextureQuantity(3);
        m_kPShader.get(0).SetImageName(0,acBumpName);
        m_kPShader.get(0).SetImageName(1,acWaterName);
        m_kPShader.get(0).SetImageName(2,acEnvName);

        Texture pkBump = m_kPShader.get(0).GetTexture(0);
        pkBump.SetFilterType(Texture.FilterType.LINEAR);
        pkBump.SetWrapType(0,Texture.WrapType.REPEAT);
        pkBump.SetWrapType(1,Texture.WrapType.REPEAT);

        Texture pkWater = m_kPShader.get(0).GetTexture(1);
        pkWater.SetFilterType(Texture.FilterType.LINEAR);
        pkWater.SetWrapType(0,Texture.WrapType.CLAMP);
        pkWater.SetWrapType(1,Texture.WrapType.CLAMP);

        Texture pkEnv = m_kPShader.get(0).GetTexture(2);
        pkEnv.SetFilterType(Texture.FilterType.LINEAR);
    }

    /** Sets the Light direction
     * @param rkLightDir the light direction vector.
     */
    public void SetLightDir (final Vector3f rkLightDir)
    {
        m_afLightDir[0] = rkLightDir.X();
        m_afLightDir[1] = rkLightDir.Y();
        m_afLightDir[2] = rkLightDir.Z();
    }

    /** Returns the Light direction
     * @return the light direction vector.
     */
    public Vector3f GetLightDir ()
    {
        return new Vector3f(m_afLightDir[0],m_afLightDir[1],m_afLightDir[2]);
    }

    /** Sets the Wave x-direction
     * @param afValue the Wave x-direction values.
     */
    public void SetWaveDirX (float[] afValue)
    {
        m_afWaveDirX[0] = afValue[0];
        m_afWaveDirX[1] = afValue[1];
        m_afWaveDirX[2] = afValue[2];
        m_afWaveDirX[3] = afValue[3];
    }

    /** Returns the Wave x-direction
     * @param afValue return parameter for the Wave x-direction values.
     */
    public void GetWaveDirX (float[] afValue)
    {
        afValue[0] = m_afWaveDirX[0];
        afValue[1] = m_afWaveDirX[1];
        afValue[2] = m_afWaveDirX[2];
        afValue[3] = m_afWaveDirX[3];
    }

    /** Sets the Wave y-direction
     * @param afValue the Wave y-direction values.
     */
    public void SetWaveDirY (float[] afValue)
    {
        m_afWaveDirY[0] = afValue[0];
        m_afWaveDirY[1] = afValue[1];
        m_afWaveDirY[2] = afValue[2];
        m_afWaveDirY[3] = afValue[3];
    }

    /** Returns the Wave y-direction
     * @param afValue return paramter for the Wave y-direction values.
     */
    public void GetWaveDirY (float[] afValue)
    {
        afValue[0] = m_afWaveDirY[0];
        afValue[1] = m_afWaveDirY[1];
        afValue[2] = m_afWaveDirY[2];
        afValue[3] = m_afWaveDirY[3];
    }

    /** Sets the Wave speed values
     * @param afValue the Wave speed values.
     */
    public void SetWaveSpeed (float[] afValue)
    {
        m_afWaveSpeed[0] = afValue[0];
        m_afWaveSpeed[1] = afValue[1];
        m_afWaveSpeed[2] = afValue[2];
        m_afWaveSpeed[3] = afValue[3];
    }

    /** Returns the Wave speed values
     * @param afvalue -- return paramter the Wave speed values.
     */
    public void GetWaveSpeed (float[] afValue)
    {
        afValue[0] = m_afWaveSpeed[0];
        afValue[1] = m_afWaveSpeed[1];
        afValue[2] = m_afWaveSpeed[2];
        afValue[3] = m_afWaveSpeed[3];
    }

    /** Sets the Wave offset values
     * @param afValue the Wave offset values.
     */
    public void SetWaveOffset (float[] afValue)
    {
        m_afWaveOffset[0] = afValue[0];
        m_afWaveOffset[1] = afValue[1];
        m_afWaveOffset[2] = afValue[2];
        m_afWaveOffset[3] = afValue[3];
    }

    /** Returns the Wave offset values
     * @param afvalue -- return paramter the Wave offset values.
     */
    public void GetWaveOffset (float[] afValue)
    {
        afValue[0] = m_afWaveOffset[0];
        afValue[1] = m_afWaveOffset[1];
        afValue[2] = m_afWaveOffset[2];
        afValue[3] = m_afWaveOffset[3];
    }

    /** Sets the Wave height values
     * @param afValue the Wave height values.
     */
    public void SetWaveHeight (float[] afValue)
    {
        m_afWaveHeight[0] = afValue[0];
        m_afWaveHeight[1] = afValue[1];
        m_afWaveHeight[2] = afValue[2];
        m_afWaveHeight[3] = afValue[3];
    }

    /** Returns the Wave height values
     * @param afValue return paramter for the Wave height values.
     */
    public void GetWaveHeight (float[] afValue)
    {
        afValue[0] = m_afWaveHeight[0];
        afValue[1] = m_afWaveHeight[1];
        afValue[2] = m_afWaveHeight[2];
        afValue[3] = m_afWaveHeight[3];
    }

    /** Sets the Bump speed values
     * @param afValue the Bump speed values.
     */
    public void SetBumpSpeed (float[] afValue)
    {
        m_afBumpSpeed[0] = afValue[0];
        m_afBumpSpeed[1] = afValue[1];
        m_afBumpSpeed[2] = afValue[2];
        m_afBumpSpeed[3] = afValue[3];
    }

    /** Returns the Bump speed values
     * @param afValue return parameter for the Bump speed values.
     */
    public void GetBumpSpeed (float[] afValue)
    {
        afValue[0] = m_afBumpSpeed[0];
        afValue[1] = m_afBumpSpeed[1];
        afValue[2] = m_afBumpSpeed[2];
        afValue[3] = m_afBumpSpeed[3];
    }

    /** Sets the Average DuDxDvDy value
     * @param fValue the Average DuDxDvDy value
     */
    public void SetAverageDuDxDvDy (float fValue)
    {
        m_afConstants[0] = fValue;
    }

    /** Returns the Average DuDxDvDy value
     * @return the Average DuDxDvDy value
     */
    public float GetAverageDuDxDvDy ()
    {
        return m_afConstants[0];
    }

    /** Sets the Ambient value
     * @param fValue the Ambient value
     */
    public void SetAmbient (float fValue)
    {
        m_afConstants[1] = fValue;
    }

    /** Returns the Ambient value
     * @return the Ambient value
     */
    public float GetAmbient ()
    {
        return m_afConstants[1];
    }

    /** Sets the Texture repeat value
     * @param fValue the Texture repeat value
     */
    public void SetTextureRepeat (float fValue)
    {
        m_afConstants[2] = fValue;
    }

    /** Returns the Texture repeat value
     * @return the Texture repeat value
     */
    public float GetTextureRepeat ()
    {
        return m_afConstants[2];
    }

    /** Sets the animation time value
     * @param fValue the animation time value
     */
    public void SetTime (float fValue)
    {
        m_afConstants[3] = fValue;
    }

    /** Returns the animation time value
     * @return the animation time value
     */
    public float GetTime ()
    {
        return m_afConstants[3];
    }

    /** Exchange information between the effect and the programs.
     * @param iPass, the rendering pass
     * @param pkVProgram the Vertex Program
     * @param pkPProgram the Pixel Program
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        // vertex program processing
        pkVProgram.GetUC("LightDir").SetDataSource(m_afLightDir);
        pkVProgram.GetUC("WaveDirX").SetDataSource(m_afWaveDirX);
        pkVProgram.GetUC("WaveDirY").SetDataSource(m_afWaveDirY);
        pkVProgram.GetUC("WaveSpeed").SetDataSource(m_afWaveSpeed);
        pkVProgram.GetUC("WaveOffset").SetDataSource(m_afWaveOffset);
        pkVProgram.GetUC("WaveHeight").SetDataSource(m_afWaveHeight);
        pkVProgram.GetUC("BumpSpeed").SetDataSource(m_afBumpSpeed);
        pkVProgram.GetUC("Constants").SetDataSource(m_afConstants);
    }

    /** The light direction is a 3-tuple.  The last component is unused. */
    protected float[] m_afLightDir = new float[4];

    /** The rippling ocean is made up of 4 waves. */
    protected float[] m_afWaveDirX = new float[4];
    protected float[] m_afWaveDirY = new float[4];
    protected float[] m_afWaveSpeed = new float[4];
    protected float[] m_afWaveOffset = new float[4];
    protected float[] m_afWaveHeight = new float[4];
    protected float[] m_afBumpSpeed = new float[4];

    /** Index 0 is averageDuDxDvDy, index 1 is ambient, index 2 is
     * texture repeat, and index 3 is time.
     */
    protected float[] m_afConstants = new float[4];

    /** streaming constructor */
    public RipplingOceanEffect () {}

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
        rkStream.Read(3,m_afLightDir);
        rkStream.Read(4,m_afWaveDirX);
        rkStream.Read(4,m_afWaveDirY);
        rkStream.Read(4,m_afWaveSpeed);
        rkStream.Read(4,m_afWaveOffset);
        rkStream.Read(4,m_afWaveHeight);
        rkStream.Read(4,m_afBumpSpeed);
        rkStream.Read(4,m_afConstants);
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

        // native data
        rkStream.Write(3,m_afLightDir);
        rkStream.Write(4,m_afWaveDirX);
        rkStream.Write(4,m_afWaveDirY);
        rkStream.Write(4,m_afWaveSpeed);
        rkStream.Write(4,m_afWaveOffset);
        rkStream.Write(4,m_afWaveHeight);
        rkStream.Write(4,m_afBumpSpeed);
        rkStream.Write(4,m_afConstants);
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
            3 * Stream.SIZEOF_FLOAT + //3*sizeof(m_afLightDir[0]) +
            4 * Stream.SIZEOF_FLOAT + //4*sizeof(m_afWaveDirX[0]) +
            4 * Stream.SIZEOF_FLOAT + //4*sizeof(m_afWaveDirY[0]) +
            4 * Stream.SIZEOF_FLOAT + //4*sizeof(m_afWaveSpeed[0]) +
            4 * Stream.SIZEOF_FLOAT + //4*sizeof(m_afWaveOffset[0]) +
            4 * Stream.SIZEOF_FLOAT + //4*sizeof(m_afWaveHeight[0]) +
            4 * Stream.SIZEOF_FLOAT + //4*sizeof(m_afBumpSpeed[0]) +
            4 * Stream.SIZEOF_FLOAT; //4*sizeof(m_afConstants[0]);
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
        pkTree.Append(StringTree.Format("RipplingOceanEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        Vector3f kDirection = new Vector3f(m_afLightDir[0],m_afLightDir[1],m_afLightDir[2]);
        pkTree.Append(StringTree.Format("light direction =",kDirection));
        pkTree.Append(StringTree.Format("wave0 x-direction =",m_afWaveDirX[0]));
        pkTree.Append(StringTree.Format("wave1 x-direction =",m_afWaveDirX[1]));
        pkTree.Append(StringTree.Format("wave2 x-direction =",m_afWaveDirX[2]));
        pkTree.Append(StringTree.Format("wave3 x-direction =",m_afWaveDirX[3]));
        pkTree.Append(StringTree.Format("wave0 y-direction =",m_afWaveDirY[0]));
        pkTree.Append(StringTree.Format("wave1 y-direction =",m_afWaveDirY[1]));
        pkTree.Append(StringTree.Format("wave2 y-direction =",m_afWaveDirY[2]));
        pkTree.Append(StringTree.Format("wave3 y-direction =",m_afWaveDirY[3]));
        pkTree.Append(StringTree.Format("wave0 speed =",m_afWaveSpeed[0]));
        pkTree.Append(StringTree.Format("wave1 speed =",m_afWaveSpeed[1]));
        pkTree.Append(StringTree.Format("wave2 speed =",m_afWaveSpeed[2]));
        pkTree.Append(StringTree.Format("wave3 speed =",m_afWaveSpeed[3]));
        pkTree.Append(StringTree.Format("wave0 offset =",m_afWaveOffset[0]));
        pkTree.Append(StringTree.Format("wave1 offset =",m_afWaveOffset[1]));
        pkTree.Append(StringTree.Format("wave2 offset =",m_afWaveOffset[2]));
        pkTree.Append(StringTree.Format("wave3 offset =",m_afWaveOffset[3]));
        pkTree.Append(StringTree.Format("wave0 bump speed =",m_afBumpSpeed[0]));
        pkTree.Append(StringTree.Format("wave1 bump speed =",m_afBumpSpeed[1]));
        pkTree.Append(StringTree.Format("wave2 bump speed =",m_afBumpSpeed[2]));
        pkTree.Append(StringTree.Format("wave3 bump speed =",m_afBumpSpeed[3]));
        pkTree.Append(StringTree.Format("average DuDxDvDy =",m_afConstants[0]));
        pkTree.Append(StringTree.Format("ambient =",m_afConstants[1]));
        pkTree.Append(StringTree.Format("texture repeat =",m_afConstants[2]));
        pkTree.Append(StringTree.Format("time =",m_afConstants[3]));

        return pkTree;
    }
}
