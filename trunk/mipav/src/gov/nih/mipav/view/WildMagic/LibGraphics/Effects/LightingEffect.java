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

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class LightingEffect extends ShaderEffect
    implements StreamInterface
{

    /** Creates a LightingEffect */
    public LightingEffect ()
    {
        super(1);
        // If no lights are attached, the Configure() call will turn the effect
        // off.
    }

    /** Returns the number of lights in this Effect 
     * @return the number of lights
     */
    public int GetLightQuantity ()
    {
        return (int)m_kLights.size();
    }

    /** Returns the ith Light in this Effect 
     * @return the ith Light in this Effect 
     */
    public Light GetLight (int i)
    {
        assert(0 <= i && i < (int)m_kLights.size());
        return m_kLights.get(i);
    }

    /** Attaches a new Light to this Effect 
     * @param pkLight a new Light to add to this Effect 
     */
    public void AttachLight (Light pkLight)
    {
        assert(pkLight != null);

        // Check if the light is already in the list.
        for (int i = 0; i < (int)m_kLights.size(); i++)
        {
            if (m_kLights.get(i) == pkLight)
            {
                // The light already exists, so do nothing.
                return;
            }
        }

        // The light is not in the current list, so add it.
        m_kLights.add(pkLight);
    }

    /** Removes the Light from this Effect 
     * @param pkLight the Light to remove from this Effect 
     */
    public void DetachLight (Light pkLight)
    {
        for ( int i = 0; i < m_kLights.size(); i++ )
        {
            if (pkLight == m_kLights.get(i))
            {
                m_kLights.remove(i);
                return;
            }
        }
    }

    /** Removes all Lights from this Effect*/
    public void DetachAllLights ()
    {
        m_kLights.clear();
    }


    /** After attaching and/or detaching lights, call Configure() to activate
     * the correct shader program for the current set of lights.
     */
    public void Configure ()
    {
        if (m_kLights.size() == 0)
        {
            SetPassQuantity(1);
            m_kVShader.set(0, new VertexShader("Material"));
            m_kPShader.set(0, new PixelShader("PassThrough4"));
            return;
        }

        // Use a bucket sort on the lights to arrange them in the order:
        // ambient, directional, point, spot.
        Vector<Vector<Light>> m_akBucket = new Vector<Vector<Light>>();
        for ( int i = 0; i < 4; i++ )
        {
            m_akBucket.add(new Vector<Light>());
        }
        int iLQuantity = (int)m_kLights.size();
        int i, iType;
        for (i = 0; i < iLQuantity; i++)
        {
            iType = (int)m_kLights.get(i).Type.Value();
            m_akBucket.get(iType).add(m_kLights.get(i));
        }

        // For multipass rendering.  The default is to use additive blending, but
        // you can change the blending modes, if so desired, in your application
        // code.
        SetPassQuantity(iLQuantity);
        for (i = 1; i < iLQuantity; i++)
        {
            AlphaState pkAS = m_kAlphaState.get(i);
            pkAS.BlendEnabled = true;
            pkAS.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
            pkAS.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        }

        final char[] acType = new char[]{ 'a', 'd', 'p', 's' };
        //String kShaderName = new String("L1$");
        String kShaderName = new String("L1");
        int iLight = 0;
        for (iType = 0; iType < 4; iType++)
        {
            for (i = 0; i < m_akBucket.get(iType).size(); i++)
            {
                //kShaderName[2] = acType[iType];
                m_kVShader.set(iLight, new VertexShader(new String(kShaderName + acType[iType])));
                m_kPShader.set(iLight, new PixelShader("PassThrough4"));
                iLight++;
            }
        }
    }


    /** Enable and disable lights for multipass drawing.  The first pass is
     * responsible for handling the emissive lighting.
     * @param iPass, the current rendering pass
     * @param pkRenderer the Renderer object
     * @param bPrimaryEffect when true this Effect is the primary Effect
     */
    public void SetGlobalState (int iPass, Renderer pkRenderer,
                                boolean bPrimaryEffect)
    {
        super.SetGlobalState(iPass,pkRenderer,bPrimaryEffect);

        MaterialState pkMS = pkRenderer.GetMaterialState();
        if (iPass == 0)
        {
            m_kSaveEmissive = pkMS.Emissive;
        }
        else
        {
            pkMS.Emissive = ColorRGB.BLACK;
        }

        pkRenderer.SetLight(0,m_kLights.get(iPass));
    }

    /** Enable and disable lights for multipass drawing.  The first pass is
     * responsible for handling the emissive lighting.
     * @param iPass, the current rendering pass
     * @param pkRenderer the Renderer object
     * @param bPrimaryEffect when true this Effect is the primary Effect
     */
    public void RestoreGlobalState (int iPass, Renderer pkRenderer,
                                    boolean bPrimaryEffect)
    {
        super.RestoreGlobalState(iPass,pkRenderer,bPrimaryEffect);

        pkRenderer.SetLight(0,null);

        if (iPass == m_iPassQuantity - 1)
        {
            MaterialState pkMS = pkRenderer.GetMaterialState();
            pkMS.Emissive = m_kSaveEmissive;
        }
    }

    /** The Emissive Material color for Emissive Lighting Effects. */
    protected ColorRGB m_kSaveEmissive;

    /** The list of Lights in this Effect: */
    protected Vector<Light> m_kLights = new Vector<Light>();

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
        pkTree.Append(StringTree.Format("LightingEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}

