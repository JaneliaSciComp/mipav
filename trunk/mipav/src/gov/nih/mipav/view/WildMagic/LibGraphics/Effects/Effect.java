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

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public abstract class Effect extends WmObject
    implements StreamInterface
{
    /** Override this function to obtain whatever drawing behavior your effect
     * requires.  If this is not overridden, the default behavior is to
     * draw all the Geometry objects in akVisible.
     * @param pkRenderer
     * @param pkGlobalObject
     * @param iMin, the first VisibleObject to draw
     * @param iMax, the last VisibleObject to draw
     * @param akVisible, the list VisibleObjects to draw
     */
    public void Draw (Renderer pkRenderer, Spatial pkGlobalObject,
                      int iMin, int iMax, VisibleObject[] akVisible)
    {
        // The default drawing function for global effects.  Essentially, this is
        // a local effect applied to all the visible leaf geometry.
        for (int i = iMin; i <= iMax; i++)
        {
            if (akVisible[i].IsDrawable())
            {
                Geometry pkGeometry = (Geometry)akVisible[i].Object;
                pkGeometry.AttachEffect(this);
                pkRenderer.Draw(pkGeometry);
                pkGeometry.DetachEffect(this);
            }
        }
    }


    /** Override these to allow loading and releasing of any resources your
     * effect requires.  The defaults are to do nothing.  The functions are
     * called by Renderer::LoadResources and Renderer::ReleaseResources for
     * Geometry and Effect objects.
     * @param pkRenderer
     * @param pkGeometry
     */
    public void LoadResources (Renderer pkRenderer, Geometry pkGeometry)
    {
        // Stub for derived classes.
    }

    /** Override these to allow loading and releasing of any resources your
     * effect requires.  The defaults are to do nothing.  The functions are
     * called by Renderer::LoadResources and Renderer::ReleaseResources for
     * Geometry and Effect objects.
     * @param pkRenderer
     * @param pkGeometry
     */
    public void ReleaseResources (Renderer pkRenderer, Geometry pkGeometry)
    {
        // Stub for derived classes.
    }

    /** Streaming constructor: */
    public Effect () {}

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
        pkTree.Append(StringTree.Format("Effect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}

