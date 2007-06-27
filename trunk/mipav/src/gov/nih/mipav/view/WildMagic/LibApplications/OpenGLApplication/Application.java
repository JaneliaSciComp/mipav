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
// Version: 4.0.1 (2006/10/28)

package gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public abstract class Application
{
    /** The unique application object */
    public static Application TheApplication;

    /** TestStreaming. For testing the disk-streaming and string-tree systems.  The input
     * object is streamed to disk and then loaded.  In a debug build, this
     * will allow 'assert' statements to trigger when there are mismatches
     * in saving and loading of data.  After loading, LaunchTreeControl is
     * called as a test of the string-tree system.  This function currently
     * is supported only on the Microsoft Windows platform.
     * @param pkScene the root-node of the scene graph object.
     * @param acFileName the name of the output scene-graph file.
     */
    public void TestStreaming (Spatial pkScene, final String acFilename)
    {
        Stream kOStream = new Stream();
        kOStream.Insert(pkScene);
        kOStream.Save(acFilename);

        Stream kIStream = new Stream();
        kIStream.Load(acFilename);
        Spatial spkScene = (Spatial)(kIStream.GetObjectAt(0));
        spkScene.SetName(acFilename);

        StringTree pkRoot = spkScene.SaveStrings( null );
        pkRoot.Save( new String( acFilename + "_StringTree.txt" ), 4 );
        new StringTreeGUI( pkRoot );
    }
    
    public void reloadShaders() {}
}
