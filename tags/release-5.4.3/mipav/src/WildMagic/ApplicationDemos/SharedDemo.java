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
import java.awt.GridLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLDrawableFactory;
import javax.media.opengl.GLPbuffer;
import javax.media.opengl.GLProfile;
import javax.media.opengl.awt.GLCanvas;
import javax.swing.JFrame;
import javax.swing.JPanel;

import com.jogamp.opengl.util.Animator;

public class SharedDemo extends Thread {
    static GLProfile glp;
    static GLCapabilities caps;
    static int width, height;
    GLPbuffer sharedDrawable;
    Lattice sharedDemo;
    static boolean init = initClass();

    static long duration = 500000; // ms

    public static boolean initClass() {
        GLProfile.initSingleton(true);
        glp = GLProfile.getDefault();
        caps = new GLCapabilities(glp);
        width  = 512;
        height = 512;
        return true;
    }

    public static void main(String args[]) {
    	if ( args != null )
    	{
    		for(int i=0; i<args.length; i++) {
    			if(args[i].equals("-time")) {
    				i++;
    				try {
    					duration = Integer.parseInt(args[i]);
    				} catch (Exception ex) { ex.printStackTrace(); }
    			}
    		}
    	}
    	
    	 SharedDemo test = new SharedDemo();
    	 test.initShared();

         final Animator animator = new Animator();
         JFrame mainWin = new JFrame( "Shared Gears AWT Test" );
         mainWin.setLayout( new GridLayout( 2, 2 ) );
         mainWin.setSize( width*2, height*2 );
         JPanel f1 = test.runTestGL(animator, 0, 0, true);
         JPanel f2 = test.runTestGL(animator, width, 0, true);
         JPanel f3 = test.runTestGL(animator, 0, height, false);
         mainWin.add(f1);
         mainWin.add(f2);
         mainWin.add(f3);
         animator.setRunAsFastAsPossible(true);

         mainWin.addWindowListener(new WindowAdapter() {
     		@Override
			public void windowClosing(WindowEvent e) {
     			// Run this on another thread than the AWT event queue to
     			// avoid deadlocks on shutdown on some platforms
     			new Thread(new Runnable() {
     				@Override
					public void run() {
     					animator.stop();
     			        //releaseShared();
     					System.exit(0);
     				}
     			}).start();
     		}
     	});
         mainWin.setVisible(true);
         animator.start();
    }
    
    private void initShared() {
        sharedDrawable = GLDrawableFactory.getFactory(glp).createGLPbuffer(null, caps, null, width, height, null);
        sharedDemo = new Lattice();
        sharedDrawable.addGLEventListener(sharedDemo);
        // init and render one frame, which will setup the Gears display lists
        sharedDrawable.display();
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

        Lattice demo = null;
        if(useShared) {
        	demo = new Lattice(glCanvas, sharedDemo.GetScene(), useShared);
        }
        else
        {
        	demo = new Lattice(glCanvas, null, useShared);
        }
        glCanvas.addGLEventListener(demo);

        animator.add(glCanvas);

        frame.setVisible(true);

        return frame;
    }
}
