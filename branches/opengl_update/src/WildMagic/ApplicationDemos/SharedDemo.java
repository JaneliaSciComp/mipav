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
//         Iridescence.main( glCanvas, test.sharedDemo.GetScene(), true );
         Iridescence.main( args);

//         glCanvas = new GLCanvas(caps);
//         Iridescence.main(glCanvas, null, false );
//
//         final Animator animator1 = new Animator();
//         final Animator animator2 = new Animator();
//         final Animator animator3 = new Animator();
//         JFrame mainWin = new JFrame( "Shared Gears AWT Test" );
//         mainWin.setLayout( new GridLayout( 2, 2 ) );
//         mainWin.setSize( width*2, height*2 );
//         JPanel f1 = test.runTestGL(animator1, 0, 0, true);
//         JPanel f2 = test.runTestGL(animator2, width, 0, true);
//         JPanel f3 = test.runTestGL(animator3, 0, height, false);
//         mainWin.add(f1);
//         mainWin.add(f2);
//         mainWin.add(f3);
//         animator1.setRunAsFastAsPossible(true);
//         animator2.setRunAsFastAsPossible(true);
//         animator3.setRunAsFastAsPossible(true);
//
//         mainWin.addWindowListener(new WindowAdapter() {
//     		@Override
//			public void windowClosing(WindowEvent e) {
//     			// Run this on another thread than the AWT event queue to
//     			// avoid deadlocks on shutdown on some platforms
//     			new Thread(new Runnable() {
//     				@Override
//					public void run() {
//     					animator1.stop();
//     					animator2.stop();
//     					animator3.stop();
//     			        //releaseShared();
//     					System.exit(0);
//     				}
//     			}).start();
//     		}
//     	});
//         mainWin.setVisible(true);
//         animator1.start();
//         animator2.start();
//         animator3.start();
    }
    
    private void initShared() {
    	sharedDrawable = GLDrawableFactory.getFactory(glp).createOffscreenAutoDrawable(null, caps, null, width, height, null);
       
        sharedDemo = new Iridescence();
        sharedDrawable.addGLEventListener(sharedDemo);
        // init and render one frame, which will setup the Gears display lists
        sharedDrawable.display();
        
        caps.setStereo(true);
    }

    protected void releaseShared() {
        sharedDrawable.destroy();
    }

    protected JPanel runTestGL(Animator animator, int x, int y, boolean useShared) {
    	JPanel frame = new JPanel();
        
        GLCanvas glCanvas = new GLCanvas(caps, useShared ? sharedDrawable.getContext() : null);
        frame.add(glCanvas);
        frame.setSize(width, height);
        frame.setLocation(x, y);

        Iridescence demo = null;
        if(useShared) {
        	//demo = new Lattice(glCanvas, sharedDemo.GetScene(), useShared);
        	demo = new Iridescence(glCanvas, sharedDemo.GetScene(), useShared);
        }
        else
        {
        	//demo = new Lattice(glCanvas, null, useShared);
        	demo = new Iridescence(glCanvas, null, useShared);
        }
        glCanvas.addGLEventListener(demo);

        animator.add(glCanvas);

        frame.setVisible(true);

        return frame;
    }
}
