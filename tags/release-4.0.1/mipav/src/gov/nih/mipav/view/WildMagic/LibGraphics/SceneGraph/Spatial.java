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
package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import java.util.HashMap;
import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public abstract class Spatial extends GraphicsObject
    implements NameIdInterface, StreamInterface
{

    /** Map culling mode to type integer value.*/
    private static HashMap<Integer,CullingMode> ms_pkCullingModeMap = new HashMap<Integer,CullingMode>();

    /** Delete memory. */
    public void finalize ()
    {
        DetachAllGlobalStates();
        DetachAllLights();
        DetachAllEffects();
        m_pkParent = null;
        if ( Local != null )
        {
            Local.finalize();
            Local = null;
        }
        if ( World != null )
        {
            World.finalize();
            World = null;
        }
        super.finalize();
    }

    /** Local transforms. */
    public Transformation Local = new Transformation();
    /** World transforms.  In some situations you might need to set
     * the world transform directly and bypass the Spatial::Update()
     * mechanism.  If World is set directly, the WorldIsCurrent flag should
     * be set to true.  For example, inverse kinematic controllers and skin
     * controllers need this capability.
     */
    public Transformation World = new Transformation();
    /** Set when World transform is set directly. */
    public boolean WorldIsCurrent;

    /** World bound access.  In some situations you might want to set the
     * world bound directly and bypass the Spatial::UpdateGS() mechanism.  If
     * WorldBound is set directly, the WorldBoundIsCurrent flag should be set
     * to true. */
    public BoundingVolume WorldBound;
    /** If WorldBound is set directly, the WorldBoundIsCurrent flag should be
     * set to true. */
    public boolean WorldBoundIsCurrent;

    /** Culling parameters. */
    public enum CullingMode
    {
        // Determine visibility state by comparing the world bounding volume
        // to culling planes.
        CULL_DYNAMIC ("CULL_DYNAMIC"),

        // Force the object to be culled.  If a Node is culled, its entire
        // subtree is culled.
        CULL_ALWAYS ("CULL_ALWAYS"),

        // Never cull the object.  If a Node is never culled, its entire
        // subtree is never culled.  To accomplish this, the first time such
        // a Node is encountered, the bNoCull parameter is set to 'true' in
        // the recursive chain GetVisibleSet/OnGetVisibleSet.
        CULL_NEVER ("CULL_NEVER"),

        MAX_CULLING_MODE ("MAX_CULLING_MODE");

        private String m_kName;
        private int m_iValue;
        private static int m_iInitValue = 0;
        CullingMode( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkCullingModeMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
    };

    /** Initializes static Culling enum. */
    private static CullingMode ms_eCullingModeStatic = CullingMode.MAX_CULLING_MODE;

    /** Type of culling. */
    public CullingMode Culling;

    /** Update of geometric state and controllers.  The UpdateGS function
     * computes world transformations on the downward pass and world bounding
     * volumes on the upward pass.
     */
    public void UpdateGS ( ) 
    {
        UpdateWorldData(Float.MIN_VALUE);
        UpdateWorldBound();
        PropagateBoundToRoot();
    }

    /** Update of geometric state and controllers.  The UpdateGS function
     * computes world transformations on the downward pass and world bounding
     * volumes on the upward pass.
     * @param dAppTime, animation time step from application.
     * @param bInitiator, when true propagate bound to root.
     */
    public void UpdateGS (double dAppTime, boolean bInitiator)
    {
        UpdateWorldData(dAppTime);
        UpdateWorldBound();
        if (bInitiator)
        {
            PropagateBoundToRoot();
        }
    }

    /** Update of geometric state and controllers. The UpdateBS function just
     * computes the world bounding volumes on an upward pass.  This is useful
     * if model data changes, causing the model and world bounds to change,
     * but no transformations need recomputing.
     */
    public void UpdateBS ()
    {
        UpdateWorldBound();
        PropagateBoundToRoot();
    }

    /** global state size
     * @return global state size
     */
    public int GetGlobalStateQuantity ()
    {
        return (int)m_kGlobalStates.size();
    }

    /** Get global state at position i.
     * @param i, position of GlobalState to get.
     * @return global state at position i.
     */
    public GlobalState GetGlobalState (int i) 
    {
        assert(0 <= i && i < (int)m_kGlobalStates.size());
        return m_kGlobalStates.get(i);
    }

    /** Get global state by type.
     * @param eType, type of GlobalState to get.
     * @return global state for the input type.
     */
    public GlobalState GetGlobalState (GlobalState.StateType eType)
    {
        // check if type of state already exists
        for (int i = 0; i < (int)m_kGlobalStates.size(); i++)
        {
            if (m_kGlobalStates.get(i).GetStateType() == eType)
            {
                // type of state exists, return it
                return m_kGlobalStates.get(i);
            }
        }
        return null;
    }

    /** Attach the input global state to this object.
     * @param pkState state to attach.
     */
    public void AttachGlobalState (GlobalState pkState)
    {
        assert(pkState != null);

        // Check if this type of state is already in the list.
        for (int i = 0; i < (int)m_kGlobalStates.size(); i++)
        {
            if (m_kGlobalStates.get(i).GetStateType() == pkState.GetStateType())
            {
                // This type of state already exists, so replace it.
                m_kGlobalStates.set(i, pkState);
                return;
            }
        }

        // This type of state is not in the current list, so add it.
        m_kGlobalStates.add(pkState);
    }

    /** Detach the global state specified by the input type.
     * @param eType type of global state to detach.
     */
    public void DetachGlobalState (GlobalState.StateType eType)
    {
        for ( int i = 0; i < m_kGlobalStates.size(); i++ )
        {
            if ( m_kGlobalStates.get(i).GetStateType() == eType )
            {
                m_kGlobalStates.remove(i);
                return;
            }
        }
    }

    /** Detach all global states.  */
    public void DetachAllGlobalStates ()
    {
        m_kGlobalStates.clear();
    }


    /** light state. Return the number of attached lights.
     * @return the number of attached lights.  */
    public int GetLightQuantity ()
    {
        return (int)m_kLights.size();
    }

    /** Return the light specified by the input index.
     * @param i, the index of the light to return.
     * @return the light at index i.  */
    public Light GetLight (int i)
    {
        assert(0 <= i && i < (int)m_kLights.size());
        return (Light)(m_kLights.get(i));
    }

    /** Attach the input light.
     * @param pkLight, the light to attach.
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

    /** Detach the input light.
     * @param pkLight, the light to detach.
     */
    public void DetachLight (Light pkLight)
    {
        for ( int i = 0; i < m_kLights.size(); i++ )
        {
            if ( m_kLights.get(i) == pkLight )
            {
                m_kLights.remove(i);
                return;
            }
        }
    }

    /** Detach all lights. */
    public void DetachAllLights ()
    {
        m_kLights.clear();
    }

    /** Get the number of attached effects.
     * @return the number of attached effects. */
    public int GetEffectQuantity ()
    {
        return (int)m_kEffects.size();
    }

    /** Get the effect at index.
     * @param i, index of the effect to return.
     * @return the effect at index i. */
    public Effect GetEffect (int i)
    {
        assert(0 <= i && i < (int)m_kEffects.size());
        return (Effect)(m_kEffects.get(i));
    }

    /** Attach a new effect.
     * @param pkEffect, the effect to attach. */
    public void AttachEffect (Effect pkEffect)
    {
        assert(pkEffect != null);

        // Check if the effect is already in the list.
        for (int i = 0; i < (int)m_kEffects.size(); i++)
        {
            if (m_kEffects.get(i) == pkEffect)
            {
                // The effect already exists, so do nothing.
                return;
            }
        }

        // The effect is not in the current list, so add it.
        m_kEffects.add(pkEffect);
    }

    /** Detach the specified effect.
     * @param pkEffect, the effect to detach. */
    public void DetachEffect (Effect pkEffect)
    {
        for ( int i = 0; i < m_kEffects.size(); i++ )
        {
            if ( m_kEffects.get(i) == pkEffect )
            {
                m_kEffects.remove(i);
                return;
            }
        }
    }

    /** Detach all effects. */
    public void DetachAllEffects ()
    {
        m_kEffects.clear();
    }

    /** Set the start effect to the specified index.
     * @param i, the index of the start effect.
     */
    public void SetStartEffect (int i)
    {
        assert(0 <= i && i < (int)m_kEffects.size());
        m_iStartEffect = i;
    }

    /** Get the start effect index.
     * @return the start effect index.
     */
    public int GetStartEffect () 
    {
        return m_iStartEffect;
    }

    /** Update render state. */
    public void UpdateRS ( )
    {
        UpdateRS(null,null);
    }

    /** update of render state 
     * @param akGStack, global states.
     * @param akLStack, lights.
     */
    public void UpdateRS ( Vector<Vector<GlobalState>> akGStack,
                           Vector<Light> pkLStack)
    {
        boolean bInitiator = (akGStack == null);

        if (bInitiator)
        {
            // The order of preference is
            //   (1) Default global states are used.
            //   (2) Geometry can override them, but if global state FOOBAR
            //       has not been pushed to the Geometry leaf node, then
            //       the current FOOBAR remains in effect (rather than the
            //       default FOOBAR being used).
            //   (3) Effect can override default or Geometry render states.
            akGStack = new Vector<Vector<GlobalState>>();
            for (int i = 0; i < GlobalState.StateType.MAX_STATE_TYPE.Value(); i++)
            {
                Vector<GlobalState> kVector = new Vector<GlobalState>();
                kVector.add(null);
                akGStack.add(kVector);
            }

            // stack has no lights initially
            pkLStack = new Vector<Light>();

            // traverse to root and push states from root to this node
            PropagateStateFromRoot(akGStack,pkLStack);
        }
        else
        {
            // push states at this node
            PushState(akGStack,pkLStack);
        }

        // propagate the new state to the subtree rooted here
        UpdateState(akGStack,pkLStack);

        if (bInitiator)
        {
            akGStack = null;
            pkLStack = null;
        }
        else
        {
            // pop states at this node
            PopState(akGStack,pkLStack);
        }
    }


    /** Return parent node.
     * @return parent.
     */
    public Spatial GetParent ()
    {
        return m_pkParent;
    }

    /** Default constructor. */
    public Spatial ()
    {
        WorldBound = new SphereBV();
        Culling = CullingMode.CULL_DYNAMIC;
        WorldIsCurrent = false;
        WorldBoundIsCurrent = false;
        m_pkParent = null;
    }


    /** geometric updates
     * @param dAppTime, animation time step from application.
     */
    protected void UpdateWorldData (double dAppTime)
    {
        // update any controllers associated with this object
        //UpdateControllers(dAppTime);

//         for (int i = 0; i < (int)m_kGlobalStates.size(); i++)
//         {
//             m_kGlobalStates[i].UpdateControllers(dAppTime);
//         }

//         for (int i = 0; i < (int)m_kLights.size(); i++)
//         {
//             m_kLights[i].UpdateControllers(dAppTime);
//         }

        // update world transforms
        if (!WorldIsCurrent)
        {
            if (m_pkParent != null)
            {
                World.Product(m_pkParent.World,Local);
            }
            else
            {
                World = Local;
            }
        }
    }

    /** Update world bound. */
    protected abstract void UpdateWorldBound ();

    /** Propagate bound to root. */
    protected void PropagateBoundToRoot ()
    {
        if (m_pkParent != null)
        {
            m_pkParent.UpdateWorldBound();
            m_pkParent.PropagateBoundToRoot();
        }
    }


    /** render state updates
     * @param akGStack, global states.
     * @param akLStack, lights.
     */
    protected void PropagateStateFromRoot ( Vector<Vector<GlobalState>> akGStack,
                                            Vector<Light> pkLStack)
    {
        // traverse to root to allow downward state propagation
        if (m_pkParent != null)
        {
            m_pkParent.PropagateStateFromRoot(akGStack,pkLStack);
        }

        // push states onto current render state stack
        PushState(akGStack,pkLStack);
    }

    /** Push state onto current render state stack.
     * @param akGStack, global states.
     * @param akLStack, lights.
     */
    protected void PushState ( Vector<Vector<GlobalState>> akGStack,
                               Vector<Light> pkLStack)
    {
        int i;
        for (i = 0; i < (int)m_kGlobalStates.size(); i++)
        {
            GlobalState.StateType eType = m_kGlobalStates.get(i).GetStateType();
            akGStack.get(eType.Value()).add(m_kGlobalStates.get(i));
        }

        for (i = 0; i < (int)m_kLights.size(); i++)
        {
            Light pkLight = (Light)(m_kLights.get(i));
            pkLStack.add(pkLight);
        }
    }

    /** Pop state from current render state stack.
     * @param akGStack, global states.
     * @param akLStack, lights.
     */
    protected void PopState ( Vector<Vector<GlobalState>> akGStack,
                              Vector<Light> pkLStack)
    {
        int i;
        for (i = 0; i < (int)m_kGlobalStates.size(); i++)
        {
            GlobalState.StateType eType = m_kGlobalStates.get(i).GetStateType();
            akGStack.get(eType.Value()).remove( akGStack.get(eType.Value()).lastElement() );
        }

        for (i = 0; i < (int)m_kLights.size(); i++)
        {
            pkLStack.remove( pkLStack.lastElement() );
        }
    }

    /** Update state.
     * @param akGStack, global states.
     * @param akLStack, lights.
     */
    protected abstract void UpdateState ( Vector<Vector<GlobalState>> akGStack,
                                          Vector<Light> pkLStack);

    /** support for hierarchical scene graph */
    protected Spatial m_pkParent;

    /** global render state */
    protected Vector<GlobalState> m_kGlobalStates = new Vector<GlobalState>();

    /** light state */
    protected Vector<GraphicsObject> m_kLights =
        new Vector<GraphicsObject>();

    /** Effect state.  If the effect is attached to a Geometry object, it
     * applies to that object alone.  If the effect is attached to a Node
     * object, it applies to all Geometry objects in the subtree rooted at
     * the Node. */
    protected Vector<Effect> m_kEffects = new Vector<Effect>();

    /** Normally, all effects are applied to an object.  To allow overriding
     * some of the effects, a starting index may be specified by the
     * application.  This is useful for complex effects, where the current
     * effects must be ignored and another effect is used instead.  Without
     * this mechanism, you would have to detach and save the current effects,
     * attach the desired effect, draw, detach the desired effect, and
     * reattach the old effects.  With this mechanism, you attach the desired
     * effect, set the starting index to that of the desired effect, draw,
     * reset the starting index to zero, and detach the desired effect. */
    protected int m_iStartEffect;

    /** Parent access (Node calls this during attach/detach of children)
     * @param pkParent, set parent. */
    public void SetParent (Spatial pkParent)
    {
        m_pkParent = pkParent;
    }

    /** Culling callback.
     * @param rkCuller, Culler object.
     * @param bNoCull, when true set to no-cull.
     */
    public void OnGetVisibleSet (Culler rkCuller, boolean bNoCull)
    {
        if (Culling == CullingMode.CULL_ALWAYS)
        {
            return;
        }

        if (Culling == CullingMode.CULL_NEVER)
        {
            bNoCull = true;
        }

        int uiSavePlaneState = rkCuller.GetPlaneState();
        if (bNoCull || rkCuller.IsVisible(WorldBound))
        {
            GetVisibleSet(rkCuller,bNoCull);
        }
        rkCuller.SetPlaneState(uiSavePlaneState);
    }

    /** Get visible set. 
     * @param rkCuller, Culler
     * @param bNoCull, when true set to no-cull.
     */
    protected abstract void GetVisibleSet (Culler rkCuller, boolean bNoCull);

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

        if (WorldBound != null)
        {
            pkFound = WorldBound.GetObjectByName(rkName);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        int i;
        for (i = 0; i < (int)m_kGlobalStates.size(); i++)
        {
            if (m_kGlobalStates.get(i) != null)
            {
                pkFound = m_kGlobalStates.get(i).GetObjectByName(rkName);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        for (i = 0; i < (int)m_kLights.size(); i++)
        {
            if (m_kLights.get(i) != null)
            {
                pkFound = m_kLights.get(i).GetObjectByName(rkName);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        for (i = 0; i < (int)m_kEffects.size(); i++)
        {
            if (m_kEffects.get(i) != null)
            {
                pkFound = m_kEffects.get(i).GetObjectByName(rkName);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        // To avoid cycles in a recursive search of a scene graph for an object
        // with the specified name, the member m_pkParent is not checked.
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

        if (WorldBound != null)
        {
            WorldBound.GetAllObjectsByName(rkName,rkObjects);
        }

        int i;
        for (i = 0; i < (int)m_kGlobalStates.size(); i++)
        {
            if (m_kGlobalStates.get(i) != null)
            {
                m_kGlobalStates.get(i).GetAllObjectsByName(rkName,rkObjects);
            }
        }

        for (i = 0; i < (int)m_kLights.size(); i++)
        {
            if (m_kLights.get(i) != null)
            {
                m_kLights.get(i).GetAllObjectsByName(rkName,rkObjects);
            }
        }

        for (i = 0; i < (int)m_kEffects.size(); i++)
        {
            if (m_kEffects.get(i) != null)
            {
                m_kEffects.get(i).GetAllObjectsByName(rkName,rkObjects);
            }
        }

        // To avoid cycles in a recursive search of a scene graph for an object
        // with the specified name, the member m_pkParent is not checked.
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

        if (WorldBound != null)
        {
            pkFound = WorldBound.GetObjectByID(uiID);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        int i;
        for (i = 0; i < (int)m_kGlobalStates.size(); i++)
        {
            if (m_kGlobalStates.get(i) != null)
            {
                pkFound = m_kGlobalStates.get(i).GetObjectByID(uiID);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        for (i = 0; i < (int)m_kLights.size(); i++)
        {
            if (m_kLights.get(i) != null)
            {
                pkFound = m_kLights.get(i).GetObjectByID(uiID);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        for (i = 0; i < (int)m_kEffects.size(); i++)
        {
            if (m_kEffects.get(i) != null)
            {
                pkFound = m_kEffects.get(i).GetObjectByID(uiID);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        // To avoid cycles in a recursive search of a scene graph for an object
        // with the specified id, the member m_pkParent is not checked.
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
        rkStream.Read(Local);
        rkStream.Read(World);
        int iCulling = rkStream.ReadInt();
        Culling = ms_pkCullingModeMap.get(iCulling);

        WorldIsCurrent = rkStream.ReadBoolean();
        WorldBoundIsCurrent = rkStream.ReadBoolean();

        // link data
        int iLinkID = rkStream.ReadInt();  // m_spkWorldBound
        pkLink.Add(iLinkID);

        int i, iQuantity;
        iQuantity = rkStream.ReadInt();
        m_kGlobalStates.setSize(iQuantity);
        for (i = 0; i < iQuantity; i++)
        {
            iLinkID = rkStream.ReadInt();  // m_kGlobalStates[i]
            pkLink.Add(iLinkID);
        }

        iQuantity = rkStream.ReadInt();
        m_kLights.setSize(iQuantity);
        for (i = 0; i < iQuantity; i++)
        {
            iLinkID = rkStream.ReadInt();  // m_kLights[i]
            pkLink.Add(iLinkID);
        }

        iQuantity = rkStream.ReadInt();
        m_kEffects.setSize(iQuantity);
        for (i = 0; i < iQuantity; i++)
        {
            iLinkID = rkStream.ReadInt();  // m_kEffects[i]
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

        int iLinkID = pkLink.GetLinkID();
        WorldBound = (BoundingVolume)rkStream.GetFromMap(iLinkID);

        for (int i = 0; i < m_kGlobalStates.size(); i++)
        {
            iLinkID = pkLink.GetLinkID();
            m_kGlobalStates.set( i, (GlobalState)rkStream.GetFromMap(iLinkID) );
        }

        for (int i = 0; i < m_kLights.size(); i++)
        {
            iLinkID = pkLink.GetLinkID();
            m_kLights.set( i, (Light)rkStream.GetFromMap(iLinkID));
        }

        for (int i = 0; i < (int)m_kEffects.size(); i++)
        {
            iLinkID = pkLink.GetLinkID();
            m_kEffects.set( i, (Effect)rkStream.GetFromMap(iLinkID));
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

        if (WorldBound != null)
        {
            WorldBound.Register(rkStream);
        }

        for (int i = 0; i < m_kGlobalStates.size(); i++)
        {
            if (m_kGlobalStates.get(i) != null)
            {
                m_kGlobalStates.get(i).Register(rkStream);
            }
        }

        for (int i = 0; i < m_kLights.size(); i++)
        {
            if (m_kLights.get(i) != null)
            {
                m_kLights.get(i).Register(rkStream);
            }
        }

        for (int i = 0; i < m_kEffects.size(); i++)
        {
            if (m_kEffects.get(i) != null)
            {
                m_kEffects.get(i).Register(rkStream);
            }
        }

        // m_pkParent need not be registered since the parent itself must have
        // initiated the Register call to its children, 'this' being one of them.
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
        rkStream.Write(Local);
        rkStream.Write(World);
        rkStream.Write(Culling.Value());
        rkStream.Write(WorldIsCurrent);
        rkStream.Write(WorldBoundIsCurrent);

        // link data
        rkStream.Write(WorldBound.GetID());

        int iQuantity = m_kGlobalStates.size();
        rkStream.Write(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            rkStream.Write(m_kGlobalStates.get(i).GetID());
        }

        iQuantity = m_kLights.size();
        rkStream.Write(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            rkStream.Write(m_kLights.get(i).GetID());
        }

        iQuantity = m_kEffects.size();
        rkStream.Write(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            rkStream.Write(m_kEffects.get(i).GetID());
        }

        // m_pkParent need not be saved since 'this' will be attached as a
        // child in Node::Link.
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
            Transformation.DISK_USED +  // Local
            Transformation.DISK_USED +  // World
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // WorldIsCurrent
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // WorldBoundIsCurrent
            Stream.SIZEOF_INT + //sizeof(WorldBound) +
            Stream.SIZEOF_INT + //sizeof(int) + // Culling
            Stream.SIZEOF_INT + (m_kGlobalStates.size())*Stream.SIZEOF_INT + //sizeof(GlobalStatePtr) +
            Stream.SIZEOF_INT + (m_kLights.size())*Stream.SIZEOF_INT + //sizeof(LightPtr) +
            Stream.SIZEOF_INT + (m_kEffects.size())*Stream.SIZEOF_INT; //sizeof(EffectPtr);
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
        pkTree.Append(StringTree.Format("Spatial",GetName()));
        pkTree.Append(super.SaveStrings(null));
        if (Local.IsRSMatrix())
        {
            pkTree.Append(StringTree.Format("local rotate =",Local.GetRotate()));
        }
        else
        {
            pkTree.Append(StringTree.Format("local matrix =",Local.GetMatrix()));
        }

        pkTree.Append(StringTree.Format("local trans =",Local.GetTranslate()));

        if (Local.IsRSMatrix())
        {
            pkTree.Append(StringTree.Format("local scale =",Local.GetScale()));
        }
        else
        {
            pkTree.Append(StringTree.Format("local scale part of matrix"));
        }

        if (World.IsRSMatrix())
        {
            pkTree.Append(StringTree.Format("world rotate =",World.GetRotate()));
        }
        else
        {
            pkTree.Append(StringTree.Format("world matrix =",World.GetMatrix()));
        }

        pkTree.Append(StringTree.Format("world trans =",World.GetTranslate()));

        if (World.IsRSMatrix())
        {
            pkTree.Append(StringTree.Format("world scale =",World.GetScale()));
        }
        else
        {
            pkTree.Append(StringTree.Format("world scale part of matrix"));
        }

        pkTree.Append(StringTree.Format("world bound =",WorldBound));
        pkTree.Append(StringTree.Format("culling =",Culling.Name()));
        pkTree.Append(StringTree.Format("start effect =",m_iStartEffect));

        // children
        int iQuantity = (int)m_kGlobalStates.size();
        for (int i = 0; i < iQuantity; i++)
        {
            pkTree.Append(m_kGlobalStates.get(i).SaveStrings(null));
        }

        iQuantity = (int)m_kLights.size();
        for (int i = 0; i < iQuantity; i++)
        {
            pkTree.Append(m_kLights.get(i).SaveStrings(null));
        }

        iQuantity = (int)m_kEffects.size();
        for (int i = 0; i < iQuantity; i++)
        {
            pkTree.Append(m_kEffects.get(i).SaveStrings(null));
        }

        return pkTree;
    }
}
