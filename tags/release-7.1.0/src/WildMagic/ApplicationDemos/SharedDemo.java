/**
 * Copyright 2010 JogAmp Community. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 * 
 *    1. Redistributions of source code must retain the above copyright notice, this list of
 *       conditions and the following disclaimer.
 * 
 *    2. Redistributions in binary form must reproduce the above copyright notice, this list
 *       of conditions and the following disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY JogAmp Community ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JogAmp Community OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * The views and conclusions contained in the software and documentation are those of the
 * authors and should not be interpreted as representing official policies, either expressed
 * or implied, of JogAmp Community.
 */
 
package WildMagic.ApplicationDemos;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.Preferences.OperatingSystem;

import java.util.List;

import javax.media.opengl.GL3;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLCapabilitiesImmutable;
import javax.media.opengl.GLDrawableFactory;
import javax.media.opengl.GLException;
import javax.media.opengl.GLOffscreenAutoDrawable;
import javax.media.opengl.GLProfile;
import javax.media.opengl.awt.GLCanvas;
import javax.swing.JPanel;

import com.jogamp.opengl.util.Animator;

public class SharedDemo extends Thread {
    static GLProfile glp;
    static GLCapabilities caps;
    static int width, height;
    GLOffscreenAutoDrawable sharedDrawable;
    DemoBase sharedDemo;
    static boolean init = initClass();

    static long duration = 500000; // ms

    public static boolean initClass() {
        glp = GLProfile.getMaxProgrammable(true);
        caps = new GLCapabilities(glp);
        caps.setAlphaBits(8);
        caps.setHardwareAccelerated(true);
        width  = 512;
        height = 512;
        return true;
    }

    public static void main(String args[]) 
    {
    	
    	 SharedDemo test = new SharedDemo();
    	 test.initShared();
    	 GLCanvas glCanvas;

         glCanvas = new GLCanvas(caps, test.sharedDrawable.getContext());
         Iridescence.main( glCanvas, test.sharedDemo.GetScene(), true );
         //Iridescence.main( args);
    	 
         glCanvas = new GLCanvas(caps, test.sharedDrawable.getContext());
         Iridescence.main( glCanvas, test.sharedDemo.GetScene(), true );
         //Iridescence.main( args);
    	 
         glCanvas = new GLCanvas(caps, test.sharedDrawable.getContext());
         //Iridescence.main( glCanvas, test.sharedDemo.GetScene(), true );
         Iridescence.main( args);
    }
    
    private void initShared() {
    	sharedDrawable = GLDrawableFactory.getFactory(glp).createOffscreenAutoDrawable(null, caps, null, width, height, null);
       
        sharedDemo = new Iridescence();
        sharedDrawable.addGLEventListener(sharedDemo);
        // init and render one frame, which will setup the Gears display lists
        sharedDrawable.display();
        
        // check hardward for stereo ability...
        if ( Preferences.OperatingSystem.getOS() != OperatingSystem.OS_MAC )
        {
        	caps.setStereo(true);
        }
    }

    protected void releaseShared() {
        sharedDrawable.destroy();
    }

}
