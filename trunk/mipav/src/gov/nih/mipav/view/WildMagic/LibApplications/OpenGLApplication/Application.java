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

package gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public abstract class Application
{

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
        kOStream.finalize();
        kOStream = null;

        Stream kIStream = new Stream();
        kIStream.Load(acFilename);
        Spatial spkScene = (Spatial)(kIStream.GetObjectAt(0));
        kIStream.finalize();
        kIStream = null;
        spkScene.SetName(acFilename);

        StringTree pkRoot = spkScene.SaveStrings( null );
        pkRoot.Save( new String( acFilename + "_StringTree.txt" ), 4 );
        new StringTreeGUI( pkRoot );
    }
    
    /** Stub for derived classes: */
    public void reloadShaders() {}
}
