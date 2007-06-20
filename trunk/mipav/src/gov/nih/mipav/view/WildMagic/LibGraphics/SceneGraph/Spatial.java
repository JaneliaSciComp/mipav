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
// Version: 4.0.1 (2006/08/07)

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import java.util.HashMap;
import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public abstract class Spatial extends GraphicsObject
    implements NameIdInterface, StreamInterface
{

    private static HashMap<Integer,CullingMode> ms_pkCullingModeMap = new HashMap<Integer,CullingMode>();

    public void finalize ()
    {
        DetachAllGlobalStates();
        DetachAllLights();
        DetachAllEffects();
        super.finalize();
    }

    // Local and world transforms.  In some situations you might need to set
    // the world transform directly and bypass the Spatial::Update()
    // mechanism.  If World is set directly, the WorldIsCurrent flag should
    // be set to true.  For example, inverse kinematic controllers and skin
    // controllers need this capability.
    public Transformation Local = new Transformation();
    public Transformation World = new Transformation();
    public boolean WorldIsCurrent;

    // World bound access.  In some situations you might want to set the
    // world bound directly and bypass the Spatial::UpdateGS() mechanism.  If
    // WorldBound is set directly, the WorldBoundIsCurrent flag should be set
    // to true.
    //public BoundingVolumePtr WorldBound;
    public BoundingVolume WorldBound;
    public boolean WorldBoundIsCurrent;

    // Culling parameters.
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

    private static CullingMode ms_eCullingModeStatic = CullingMode.MAX_CULLING_MODE;

    public CullingMode Culling;

    // Update of geometric state and controllers.  The UpdateGS function
    // computes world transformations on the downward pass and world bounding
    // volumes on the upward pass.  The UpdateBS function just computes the
    // world bounding volumes on an upward pass.  This is useful if model
    // data changes, causing the model and world bounds to change, but no
    // transformations need recomputing.
    public void UpdateGS ( ) 
    {
        UpdateWorldData(Float.MIN_VALUE);
        UpdateWorldBound();
        PropagateBoundToRoot();
    }

    public void UpdateGS (double dAppTime, boolean bInitiator)
    {
        UpdateWorldData(dAppTime);
        UpdateWorldBound();
        if (bInitiator)
        {
            PropagateBoundToRoot();
        }
    }

    public void UpdateBS ()
    {
        UpdateWorldBound();
        PropagateBoundToRoot();
    }

    // global state
    public int GetGlobalStateQuantity ()
    {
        return (int)m_kGlobalStates.size();
    }

    public GlobalState GetGlobalState (int i) 
    {
        assert(0 <= i && i < (int)m_kGlobalStates.size());
        return m_kGlobalStates.get(i);
    }

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

    public void DetachAllGlobalStates ()
    {
        m_kGlobalStates.clear();
    }


    // light state
    public int GetLightQuantity ()
    {
        return (int)m_kLights.size();
    }

    public Light GetLight (int i)
    {
        assert(0 <= i && i < (int)m_kLights.size());
        return (Light)(m_kLights.get(i));
    }

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

    public void DetachAllLights ()
    {
        m_kLights.clear();
    }

    // effect state
    public int GetEffectQuantity ()
    {
        return (int)m_kEffects.size();
    }

    public Effect GetEffect (int i)
    {
        assert(0 <= i && i < (int)m_kEffects.size());
        return (Effect)(m_kEffects.get(i));
    }

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

    public void DetachAllEffects ()
    {
        m_kEffects.clear();
    }

    public void SetStartEffect (int i)
    {
        assert(0 <= i && i < (int)m_kEffects.size());
        m_iStartEffect = i;
    }

    public int GetStartEffect () 
    {
        return m_iStartEffect;
    }

    public void UpdateRS ( )
    {
        UpdateRS(null,null);
    }

    // update of render state
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


    // parent access
    public Spatial GetParent ()
    {
        return m_pkParent;
    }

    public Spatial ()
    {
        WorldBound = new SphereBV();
        Culling = CullingMode.CULL_DYNAMIC;
        WorldIsCurrent = false;
        WorldBoundIsCurrent = false;
        m_pkParent = null;
    }


    // geometric updates
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

    protected abstract void UpdateWorldBound ();

    protected void PropagateBoundToRoot ()
    {
        if (m_pkParent != null)
        {
            m_pkParent.UpdateWorldBound();
            m_pkParent.PropagateBoundToRoot();
        }
    }


    // render state updates
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

    protected abstract void UpdateState ( Vector<Vector<GlobalState>> akGStack,
                                          Vector<Light> pkLStack);

    // support for hierarchical scene graph
    protected Spatial m_pkParent;

    // global render state
    protected Vector<GlobalState> m_kGlobalStates = new Vector<GlobalState>();

    // light state
    protected Vector<GraphicsObject> m_kLights =
        new Vector<GraphicsObject>();

    // Effect state.  If the effect is attached to a Geometry object, it
    // applies to that object alone.  If the effect is attached to a Node
    // object, it applies to all Geometry objects in the subtree rooted at
    // the Node.
    protected Vector<Effect> m_kEffects = new Vector<Effect>();

    // Normally, all effects are applied to an object.  To allow overriding
    // some of the effects, a starting index may be specified by the
    // application.  This is useful for complex effects, where the current
    // effects must be ignored and another effect is used instead.  Without
    // this mechanism, you would have to detach and save the current effects,
    // attach the desired effect, draw, detach the desired effect, and
    // reattach the old effects.  With this mechanism, you attach the desired
    // effect, set the starting index to that of the desired effect, draw,
    // reset the starting index to zero, and detach the desired effect.
    protected int m_iStartEffect;

    // internal use
    // parent access (Node calls this during attach/detach of children)
    public void SetParent (Spatial pkParent)
    {
        m_pkParent = pkParent;
    }


    // culling
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

    protected abstract void GetVisibleSet (Culler rkCuller, boolean bNoCull);

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
