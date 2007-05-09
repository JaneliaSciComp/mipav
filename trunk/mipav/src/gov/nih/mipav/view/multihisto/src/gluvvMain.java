
package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.io.*;
import javax.imageio.stream.*;
import java.util.*;
import java.nio.*;
import java.awt.event.*;
import javax.swing.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;
import com.sun.opengl.util.BufferUtil;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

public class gluvvMain
    implements KeyListener, MouseListener, MouseMotionListener, GLEventListener {

  //----------------------------------------------------------------
//---- the global data structures --------------------------------
  public gluvvGlobal gluvv; //the global gluvv data structure (gluvv.h)
  public Trackball tball; //global trackball
  public Trackball lball; //for moving the light (now using light widget)
  public gluvvPrimitive renderables; //all renderables attatch to this one
  public gluvvPrimitive widgets; //all renderables attatch to this one
  public TFWindow tfwin; // separate transfer function window (not used??)
  public LTWidgetRen ltwr; //need the light widget to be accesable to other things
  private VectorMath math = new VectorMath();

  public int blurnorms;

  private GLU glu = new GLU();
  private GLUT glut = new GLUT();

  private boolean picking = false;
  private static GLCanvas canvas;


  public gluvvMain() {
    gluvv = new gluvvGlobal();
  }

  public static void main(String[] args) {

    final JFrame frame = new JFrame("Render demo");
    canvas = new GLCanvas();
    gluvvMain render = new gluvvMain();
    render.setup(args);
    canvas.addGLEventListener(render);
    canvas.addKeyListener(render);
    canvas.addMouseListener(render);
    canvas.addMouseMotionListener(render);
    canvas.setSize(new Dimension(512, 512));
    frame.add(canvas);
    frame.setSize(550, 550);
    // Animator serves the purpose of the idle function, calls display:
    final Animator animator = new Animator(canvas);
    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        // Run this on another thread than the AWT event queue to
        // avoid deadlocks on shutdown on some platforms
        new Thread(new Runnable() {
          public void run() {
            animator.stop();
            frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
          }
        }).start();
      }
    });
    frame.show();
    animator.start();
  }

  //================================================= the Main func.
//===============================================================
  public void setup(String[] argv) {
    //===== Initialization ========================
    //------------------------------------init gluvv
    initGluvv();

    //------------------------------------parse
    if (parse(argv.length, argv) == 1) {
      return;
    }

    //------------------------------------init data
    if (initData() == 1) {
      System.err.println(
          "Data read failed.  Make sure you have the correct path specified" +
          " Hit any key to exit");
      return;
    }
    gluvv.mv.cacheTStep();

    //------------------------------------init glut
    // initGLUT(argv.length, argv);

    if (checkExts() == 0) {
      System.err.println("Sorry, this platform is not supported");
      System.err.println(
          "  Please make sure you have a GeForce 3 or Radeon 8K with the correct driver");
      return;
    }

    //------------------------------------create renderables
    //------------------------------- create volume renderer
    /*
               if(WILDCAT){ // wildcat                   ????????????????????????????????????????????????
            VolumeRenderable volr = new VolumeRenderable(gluvv);
            renderables.setNext((gluvvPrimitive)volr);
               }
     */
    else if (gluvv.plat == gluvvPlatform.GPNV202D) { //GF3 with 2D textures only
      NV20VolRen nv20vr = new NV20VolRen(gluvv);
      renderables.setNext( (gluvvPrimitive) nv20vr);
    }

    else if (gluvv.plat == gluvvPlatform.GPNV20) { //standard renderer
      if (gluvv.light.shadow == 0) {
        NV20VolRen3D nv20vr = new NV20VolRen3D(gluvv);
        renderables.setNext( (gluvvPrimitive) nv20vr);
      }
      else { //create the shadow renderer
        NV20VolRen3D2 nv20vr = new NV20VolRen3D2(gluvv);
        renderables.setNext( (gluvvPrimitive) nv20vr);
      }
    }

    //------------------------------- create widgets

    TFWidgetRen tfwr = new TFWidgetRen(gluvv);
    renderables.setNext( (gluvvPrimitive) tfwr);
    //widgets.setNext((gluvvPrimitive*)tfwr);

    CPWidgetRen cpwr = new CPWidgetRen(gluvv);
    renderables.setNext( (gluvvPrimitive) cpwr);
    //widgets.setNext((gluvvPrimitive*)cpwr);

    DPWidgetRen dpwr = new DPWidgetRen(gluvv);
    renderables.setNext( (gluvvPrimitive) dpwr);
    //widgets.setNext((gluvvPrimitive*)dpwr);

    ltwr = new LTWidgetRen(gluvv);
    renderables.setNext( (gluvvPrimitive) ltwr);
    //widgets.setNext((gluvvPrimitive*)&ltwr);
    //------------------------------------init renderables
    // - always after init glut!!
    // initRenderables();

    //------------------------------------start
    System.err.println("Entering Main Loop"); ;

    // glutMainLoop();

    System.err.println("Buh Bye!");
    return;
  }

  //=================================================================
//=================================================================

//=============================================== init Gluvv
//==========================================================
  public void initGluvv() { //"Constructor for gluvv data structures"
    //-------------------- initialize global state ---------------
    gluvv.mv = null;
    gluvv.mv1 = null;
    gluvv.mv2 = null;
    gluvv.mv3 = null;

    gluvv.debug = 0;
    gluvv.dmode = gluvvDataMode.GDM_V1;
    renderables = new gluvvPrimitive();
    renderables.setName("Dummy Node");
    gluvv.picking = 0;
    gluvv.mprobe = 1;
    gluvv.shade = gluvvShade.gluvvShadeFaux;

    gluvv.reblend = gluvvBlend.GB_NONE;

    //------------------------------------window
    gluvv.win.width = 512; //size
    gluvv.win.height = 512;
    gluvv.win.xPos = 100; //position
    gluvv.win.yPos = 100;
    //------------------------------------environment
    gluvv.env.eye[0] = 0; //eye
    gluvv.env.eye[1] = 0;
    gluvv.env.eye[2] = -7;
    gluvv.env.at[0] = 0; //at
    gluvv.env.at[1] = 0;
    gluvv.env.at[2] = 0;
    gluvv.env.up[0] = 0; //up
    gluvv.env.up[1] = 1;
    gluvv.env.up[2] = 0;
    gluvv.env.frustum[0] = -.2f; //left
    gluvv.env.frustum[1] = .2f; //right
    gluvv.env.frustum[2] = -.2f; //bottom
    gluvv.env.frustum[3] = .2f; //top
    gluvv.env.clip[0] = 1; //front
    gluvv.env.clip[1] = 20; //back
    gluvv.env.bgColor = 0; //white background
    //------------------------------------mouse
    gluvv.mouse.alt = 0;
    gluvv.mouse.ctrl = 0;
    gluvv.mouse.shift = 0;
    //------------------------------------lights & lighting
    gluvv.light.pos[0] = gluvv.light.startpos[0] = 0;
    gluvv.light.pos[1] = gluvv.light.startpos[1] = 0;
    gluvv.light.pos[2] = gluvv.light.startpos[2] = -5;
    gluvv.light.buffsz[0] = 1024; //shadow buffer size
    gluvv.light.buffsz[1] = 1024;
    gluvv.light.shadow = 0; //shadows off
    gluvv.light.softShadow = 1; //soft shadows
    gluvv.light.showView = 0; //show the view from the light
    gluvv.light.sill = 0; //show silhouette edges
    gluvv.light.amb = .05f; //shadow strenght
    gluvv.light.intens = .75f;
    gluvv.light.latt = 0;
    gluvv.light.lattLimits[0] = .5f; //attinuation limits (start)
    gluvv.light.lattLimits[1] = -.5f; //attinuation limits (finish)
    gluvv.light.lattThresh = 1; //amount of attinuation
    gluvv.light.gShadowQual = .5f; //good shadow quality
    gluvv.light.iShadowQual = .20f; //interactive shadow quality
    gluvv.light.shadowTF = 0; //not using a second, shadow tf
    gluvv.light.showShadowTF = 0; //don't show us the shadow tf
    gluvv.light.csz = 32; //size of the cube map
    gluvv.light.cubeKey[0] = 0; //x+ cube key not created yet
    gluvv.light.cubeKey[1] = 0; //x- cube key not created yet
    gluvv.light.cubeKey[2] = 0; //y+ cube key not created yet
    gluvv.light.cubeKey[3] = 0; //y- cube key not created yet
    gluvv.light.cubeKey[4] = 0; //z+ cube key not created yet
    gluvv.light.cubeKey[5] = 0; //z- cube key not created yet
    gluvv.light.load = 1; //load lighting info
    gluvv.light.gload = 1; //load the good lighting
    gluvv.light.fog = 0; //fog is off
    gluvv.light.fogThresh = .5f; //fog blend threshold
    gluvv.light.fogLimits[0] = .5f; //near fog ramp (start)
    gluvv.light.fogLimits[1] = -.5f; //far fog ramp (finish)
    //------------------------------------render info
    math.identityMatrix(gluvv.rinfo.xform); //init the rotation
    gluvv.rinfo.scale = 1; //identity for scale
    gluvv.rinfo.trans[0] = 0; //no translation
    gluvv.rinfo.trans[1] = 0;
    gluvv.rinfo.trans[2] = 0;
    //------------------------------------volume rendering stuff
    gluvv.volren.interactSamp = .6f; //Interactive sample rate
    gluvv.volren.goodSamp = 2.5f; //High quality sample ..
    gluvv.volren.sampleRate = gluvv.volren.goodSamp; //samples per voxel
    gluvv.volren.shade = 0; //shade off (this flag no longer used)
    gluvv.volren.tlut = null; //not used?? (1D tf, old school)
    gluvv.volren.deptex = null; //1st & 2nd axies transfer function
    gluvv.volren.deptex2 = null; //3rd axis transfer function
    gluvv.volren.deptex3 = null; //scattering (auxilary 2D) transfer
    gluvv.volren.loadTLUT = 0; //no need to load the tf
    gluvv.volren.scaleAlphas = 1; //scale alphas to the sample rate
    gluvv.volren.gamma = 1; //gamma identitiy
    gluvv.volren.timestep = 0; //current timestep
    gluvv.volren.usePostCNorm = 0; //use pre-computed Normals to start with
    gluvv.volren.loadNorms = 0; //no need to reload normals
    //------------------------------------transfer function
    gluvv.tf.tfWindow = 0; //not in separate window
    gluvv.tf.tfwin = tfwin; //if it was this would handle the window
    gluvv.tf.loadme = 0; //no need to load the tf
    gluvv.tf.paintme = 0; //nothing to paint into it
    gluvv.tf.dropme = 0; //not widgets to drop
    gluvv.tf.clearpaint = 0; //already cleared
    gluvv.tf.brushon = 0; //brush is off
    gluvv.tf.slider1 = 1; //3rd axis slider high
    gluvv.tf.slider1hi = 1; //these are sliders for special cases
    gluvv.tf.slider1lo = 0;
    gluvv.tf.slider2 = 1;
    gluvv.tf.histOn = 1; //histogram off
    gluvv.tf.ptexsz[0] = 256; //size of the transfer function axies
    gluvv.tf.ptexsz[1] = 256;
    gluvv.tf.ptexsz[2] = 1; //3rd axis handled sparately
    gluvv.tf.numelts = 4; // RGBA colors
    //------------------------------------clip plane
    gluvv.clip.mousepnt[0] = 0;
    gluvv.clip.mousepnt[1] = 0;
    gluvv.clip.mousepnt[2] = 0;
    gluvv.clip.ortho = 1;
    gluvv.clip.oaxis = VolRenMajorAxis.VolRenAxisXPos;
    //------------------------------------data probe
    gluvv.probe.brush = gluvvBrush.NoBrush;
    //------------------------------------trackball
    tball = new Trackball();
    tball.clear();
    lball = new Trackball();
    lball.clear();

    //------------------------------------misc
    blurnorms = 0;
  }

  //================================================ init GLUT
//==========================================================
  public void init(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    initRenderables(drawable);
    String displaymode = new String();
    if (gluvv.plat == gluvvPlatform.GPOctane) {
      displaymode = "rgba=8 depth";
    }
    else if (gluvv.plat == gluvvPlatform.GPOctane2) {
      displaymode = "rgba=8 double depth";
    }
    else if (gluvv.plat == gluvvPlatform.GPInfinite) {
      displaymode = "double rgba depth"; ;
    }
    else {
      displaymode = "double rgba depth";
    }

    // glutInit(&argc, argv);
    // glutInitDisplayString(displaymode);
    //   //glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH);
    // glutInitWindowSize(gluvv.win.width, gluvv.win.height);
    // glutInitWindowPosition(gluvv.win.xPos, gluvv.win.yPos);
    // gluvv.mainWindow = glutCreateWindow("GL Utilites for Virtual Reality Volume Rendering");

    //print out all of the extensions for the card!

    System.err.println(">>>>>>>>>>>>> GL Extensions <<<<<<<<<<<<<<");
    System.err.println("------------------------------------------");
    // QueryExtension("Print");
    System.err.println("------------------------------------------");

    // LoadAllExtensions();


    // print out some window attributes for our (ribbed) pleasure.
    {
      int[] r = new int[1];
      int[] g = new int[1];
      int[] b = new int[1];
      int[] a = new int[1];
      int[] ar = new int[1];
      int[] ag = new int[1];
      int[] ab = new int[1];
      int[] aa = new int[1];
      int[] d = new int[1];
      int[] s = new int[1];
      byte[] db = new byte[1];
      byte[] st = new byte[1];
      byte[] aux = new byte[1];

      gl.glGetIntegerv(GL.GL_RED_BITS, r, 0);
      gl.glGetIntegerv(GL.GL_GREEN_BITS, g, 0);
      gl.glGetIntegerv(GL.GL_BLUE_BITS, b, 0);
      gl.glGetIntegerv(GL.GL_ALPHA_BITS, a, 0);
      gl.glGetIntegerv(GL.GL_ACCUM_RED_BITS, ar, 0);
      gl.glGetIntegerv(GL.GL_ACCUM_GREEN_BITS, ag, 0);
      gl.glGetIntegerv(GL.GL_ACCUM_BLUE_BITS, ab, 0);
      gl.glGetIntegerv(GL.GL_ACCUM_ALPHA_BITS, aa, 0);
      gl.glGetIntegerv(GL.GL_DEPTH_BITS, d, 0);
      gl.glGetIntegerv(GL.GL_STENCIL_BITS, s, 0);
      gl.glGetBooleanv(GL.GL_DOUBLEBUFFER, db, 0);
      gl.glGetBooleanv(GL.GL_STEREO, st, 0);
      gl.glGetBooleanv(GL.GL_AUX_BUFFERS, aux, 0);

      System.err.println("Window Attributes");
      System.err.println("-----------------");
      System.err.print("color:    " + (r[0] + g[0] + b[0] + a[0]) + " bits");
      System.err.println(" (" + r[0] + "," + g[0] + "," + b[0] + "," + a[0] +
                         ")");
      System.err.print("accum:    " + (ar[0] + ag[0] + ab[0] + aa[0]) + " bits");
      System.err.println(" (" + ar[0] + "," + ag[0] + "," + ab[0] + "," + aa[0] +
                         ")");
      System.err.println("depth:    " + d[0] + " bits");
      System.err.println("stencil:  " + s[0] + " bits");
      System.err.print("double:   ");
      if (db[0] == 1) {
        System.err.println("YES");
      }
      else {
        System.err.println("NO");
      }
      System.err.print("aux:      ");
      if (aux[0] == 1) {
        System.err.println("YES");
      }
      else {
        System.err.println("NO");
      }
      System.err.print("stereo :  ");
      if (st[0] == 1) {
        System.err.println("YES");
      }
      else {
        System.err.println("NO");
      }
      System.err.println("-----------------");
    }

    initGL(drawable);

    // glutDisplayFunc(glutdisp);
    //    // GLUI_Master.set_glutReshapeFunc(reshape);
    // glutReshapeFunc(reshape);
    //    // GLUI_Master.set_glutMouseFunc(mouse);
    // glutMouseFunc(mouse);
    // glutMotionFunc(motion);
    //    // GLUI_Master.set_glutKeyboardFunc(glutkey);
    // glutKeyboardFunc(glutkey);
    //    // GLUI_Master.set_glutSpecialFunc(glutspecial);
    // glutSpecialFunc(glutspecial);
    //    // GLUI_Master.set_glutIdleFunc(glutidle);
    // glutIdleFunc(glutidle);

    return;
  }

  //=================================================== init GL
  //===========================================================
  public void initGL(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    /* white background and interpolated shading. */
    gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    gl.glShadeModel(GL.GL_SMOOTH);

    /* Set up lighting. mostly for widgets */
    float[] ambient = {
        0.4f, 0.4f, 0.4f, 4.0f};
    float[] diffuse = {
        0.5f, 1.0f, 1.0f, 1.0f};
    float[] front_mat_shininess = {
        60.0f};
    float[] front_mat_specular = {
        0.2f, 0.2f, 0.2f, 1.0f};
    float[] front_mat_diffuse = {
        0.5f, 0.28f, 0.38f, 1.0f};
    float[] lmodel_ambient = {
        0.2f, 0.2f, 0.2f, 1.0f};
    float[] lmodel_twoside = {
        GL.GL_FALSE};
    gl.glEnable(GL.GL_LIGHT0);
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_AMBIENT, ambient, 0);
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_DIFFUSE, diffuse, 0);
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, gluvv.light.startpos, 0);
    gl.glLightModelfv(GL.GL_LIGHT_MODEL_AMBIENT, lmodel_ambient, 0);
    gl.glLightModelfv(GL.GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside, 0);

    gl.glEnable(GL.GL_LIGHTING);
    gl.glEnable(GL.GL_COLOR_MATERIAL);

    gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_SHININESS, front_mat_shininess,
                    0);
    gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_SPECULAR, front_mat_specular, 0);
    gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_DIFFUSE, front_mat_diffuse, 0);

    gl.glEnable(GL.GL_DEPTH_TEST);
    gl.glDepthFunc(GL.GL_LEQUAL);
    gl.glDisable(GL.GL_CULL_FACE);

    //g.glEnable(GL.GL_LINE_SMOOTH);
    gl.glLineWidth(3);

    System.err.println("gluvv::" + " initGL()");

    gluvv.env.diff[0] = front_mat_diffuse[0];
    gluvv.env.diff[1] = front_mat_diffuse[1];
    gluvv.env.diff[2] = front_mat_diffuse[2];
    gluvv.env.diff[3] = front_mat_diffuse[3];
    gluvv.env.spec[0] = front_mat_specular[0];
    gluvv.env.spec[1] = front_mat_specular[1];
    gluvv.env.spec[2] = front_mat_specular[2];
    gluvv.env.spec[3] = front_mat_specular[3];

  }

  //=================================================================
//=================================================================

//========================================== init Renderables
//===========================================================
  private void initRenderables(GLAutoDrawable drawable) {
    gluvvPrimitive tmp = renderables.getNext();
    while (tmp != null) {
      if (tmp instanceof LTWidgetRen) {
        ( (LTWidgetRen) tmp).init(drawable);
      }
      else if (tmp instanceof CPWidgetRen) {
        ( (CPWidgetRen) tmp).init(drawable);
      }
      else if (tmp instanceof DPWidgetRen) {
        ( (DPWidgetRen) tmp).init();
      }
      else if (tmp instanceof TFWidgetRen) {
        ( (TFWidgetRen) tmp).init();
      }
      else if (tmp instanceof NV20VolRen) {
        ( (NV20VolRen) tmp).init(drawable);
      }
      else if (tmp instanceof NV20VolRen3D) {
        ( (NV20VolRen3D) tmp).init(drawable);
      }
      else if (tmp instanceof NV20VolRen3D2) {
        ( (NV20VolRen3D2) tmp).init(drawable);
      }

      tmp = tmp.getNext();
    }
  }

//=================================================================
//=================================================================

  //================================================= glut Disp
  //===========================================================
  public void display(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();

    gluvvPrimitive prim;
    if ( picking == true ) {

      if (pick(drawable) == 1 ) {
        if (gluvv.pick.prim != null) {
          prim = gluvv.pick.prim;
          // // gluvv.pick.prim.mousePressed(e);
          // gluvv.pick.prim.pick(drawable);

          if (prim instanceof LTWidgetRen) {
            ( (LTWidgetRen) prim).pick();
          }
          else if (prim instanceof CPWidgetRen) {
            ( (CPWidgetRen) prim).pick();
          }
          else if (prim instanceof DPWidgetRen) {
            ( (DPWidgetRen) prim).pick();
          }
          else if (prim instanceof TFWidgetRen) {
            ( (TFWidgetRen) prim).pick();
          }
        }
        // return;
      }
      picking = false;
    }

    gl.glMatrixMode(GL.GL_MODELVIEW);
    gl.glLoadIdentity();
    glu.gluLookAt(gluvv.env.eye[0], //eye
                  gluvv.env.eye[1],
                  gluvv.env.eye[2],
                  gluvv.env.at[0], //at
                  gluvv.env.at[1],
                  gluvv.env.at[2],
                  gluvv.env.up[0],
                  gluvv.env.up[1],
                  gluvv.env.up[2]); //up

    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    gl.glFrustum(gluvv.env.frustum[0], //left
                 gluvv.env.frustum[1], //right
                 gluvv.env.frustum[2], //top
                 gluvv.env.frustum[3], //bottom
                 gluvv.env.clip[0], //front
                 gluvv.env.clip[1]); //back

    gl.glMatrixMode(GL.GL_MODELVIEW);
    //glDrawBuffer(GL.GL_FRONT);
    //glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
    gl.glDrawBuffer(GL.GL_BACK);
    gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
    gl.glDepthFunc(GL.GL_LESS);
    gl.glDisable(GL.GL_BLEND);

    displayLocal(drawable); // our local display function

    gl.glFlush();

    GlErr(drawable, "gluvv:", " glutdisp()");
  }


  private int GlErr(GLAutoDrawable drawable, String location, String glfuncname)
  {
    int errCode;
    String errString;
    GL gl = drawable.getGL();

    if((errCode = gl.glGetError()) != GL.GL_NO_ERROR){
      errString = glu.gluErrorString(errCode);
      System.err.println("OpenGL ERROR : " + location + "::" + glfuncname + " : "
           + errString);
      return 1;
    }
    return 0;
  }

  //=================================================================
//=================================================================

  public void displayChanged(GLAutoDrawable drawable,
                             boolean modeChanged,
                             boolean deviceChanged) {}

//================================================== Display
//==========================================================
  public void displayLocal(GLAutoDrawable drawable) {

    GL gl = drawable.getGL();
    gl.glPushMatrix();
    {
      /*
                      //build current transformation-------------
                      gl.glTranslatef(gl.gluvv.rinfo.trans[0],
                              gl.gluvv.rinfo.trans[1],
                              gluvv.rinfo.trans[2]);
                      glMultMatrixf(gluvv.rinfo.xform);
                      float scale[16];
                      identityMatrix(scale);
                      scale[0]  = gluvv.rinfo.scale;
                      scale[5]  = gluvv.rinfo.scale;
                      scale[10] = gluvv.rinfo.scale;
                      glMultMatrixf(scale);
                      //done with transformation-----------------
       */

      //cycle through scene graph ---------------
      gluvvPrimitive tmp = renderables.getNext();
      /*
                 while (tmp != null) {
        tmp.draw();
        tmp = tmp.getNext();
                 }
       */

      while (tmp != null) {
        if (tmp instanceof LTWidgetRen) {
          ( (LTWidgetRen) tmp).draw(drawable);
        }
        else if (tmp instanceof CPWidgetRen) {
          ( (CPWidgetRen) tmp).draw(drawable);
        }
        else if (tmp instanceof DPWidgetRen) {
          ( (DPWidgetRen) tmp).draw(drawable);
        }
        else if (tmp instanceof TFWidgetRen) {
          ( (TFWidgetRen) tmp).draw(drawable);
        }
        else if (tmp instanceof NV20VolRen) {
          ( (NV20VolRen) tmp).draw(drawable);
        }
        else if (tmp instanceof NV20VolRen3D) {
          ( (NV20VolRen3D) tmp).draw(drawable);
        }
        else if (tmp instanceof NV20VolRen3D2) {
          ( (NV20VolRen3D2) tmp).draw(drawable);
        }
        else if ( tmp instanceof VolumeRenderable ) {
          System.err.println("ruida: displayLocal 1");
        }

        tmp = tmp.getNext();
      }

      if (gluvv.reblend == gluvvBlend.GB_UNDER) { //have to do this for front to back volumes
        tmp = renderables.getNext();
        /*
                     while (tmp != null) {
          tmp.draw();
          tmp = tmp.getNext();
                     }
         */
        while (tmp != null) {
          if (tmp instanceof LTWidgetRen) {
            ( (LTWidgetRen) tmp).draw(drawable);
          }
          else if (tmp instanceof CPWidgetRen) {
            ( (CPWidgetRen) tmp).draw(drawable);
          }
          else if (tmp instanceof DPWidgetRen) {
            ( (DPWidgetRen) tmp).draw(drawable);
          }
          else if (tmp instanceof TFWidgetRen) {
            ( (TFWidgetRen) tmp).draw(drawable);
          }
          else if (tmp instanceof NV20VolRen) {
            ( (NV20VolRen) tmp).draw(drawable);
          }
          else if (tmp instanceof NV20VolRen3D) {
            ( (NV20VolRen3D) tmp).draw(drawable);
          }
          else if (tmp instanceof NV20VolRen3D2) {
            ( (NV20VolRen3D2) tmp).draw(drawable);
          }
          else if ( tmp instanceof VolumeRenderable ) {
              System.err.println("ruida: displayLocal 2");
          }


          tmp = tmp.getNext();
        }

        if (gluvv.env.bgColor == 0) {
          gl.glDisable(GL.GL_LIGHTING);
          gl.glEnable(GL.GL_BLEND);
          gl.glBlendFunc(GL.GL_ONE_MINUS_DST_ALPHA, GL.GL_ONE);
          gluvv.reblend = gluvvBlend.GB_NONE;
          gl.glDisable(GL.GL_DEPTH_TEST);
          gl.glColor4f(1f, 1f, 1f, 1f);
          gl.glBegin(GL.GL_QUADS);
          {
            gl.glVertex3f( -10f, -10f, -2f);
            gl.glVertex3f( -10f, 10f, -2f);
            gl.glVertex3f(10f, 10f, -2f);
            gl.glVertex3f(10f, -10f, -2f);
          }
          gl.glEnd();
          gl.glEnable(GL.GL_DEPTH_TEST);
          gl.glEnable(GL.GL_LIGHTING);
          gl.glDisable(GL.GL_BLEND);
        }

      }

      gluvv.reblend = gluvvBlend.GB_NONE;

    }
    gl.glPopMatrix();

    // glutSwapBuffers();
    drawable.swapBuffers();
  }

  // Methods required for the implementation of MouseListener
  public void mouseEntered(MouseEvent e) {}

  public void mouseExited(MouseEvent e) {}

  public void mouseMoved(MouseEvent e) {}

  public void mouseReleased(MouseEvent e) {

    int x, y;
    gluvvPrimitive prim;
    picking = false;
    int button = e.getButton();
    int modifiers = e.getModifiers();
    x = e.getX();
    y = e.getY();

    gluvvPrimitive tmp = renderables.getNext();
    while (tmp != null) {
      if (tmp instanceof LTWidgetRen) {
          ( (LTWidgetRen) tmp).mouseReleased(e);
      }
      else if (tmp instanceof CPWidgetRen) {
          ( (CPWidgetRen) tmp).mouseReleased(e);
      }
      else if (tmp instanceof DPWidgetRen) {
          ( (DPWidgetRen) tmp).mouseReleased(e);
      }
      else if (tmp instanceof TFWidgetRen) {
          ( (TFWidgetRen) tmp).mouseReleased(e);
      }
      tmp = tmp.getNext();
    }

    gluvv.mouse.button = -1;

    switch (button) {
      case MouseEvent.BUTTON1:
        gluvv.volren.sampleRate = gluvv.volren.goodSamp;
        break;
      case MouseEvent.BUTTON3:
        if (gluvv.mouse.ctrl == 1) {
          gluvv.volren.sampleRate = gluvv.volren.goodSamp;
        }
        else {
          gluvv.volren.sampleRate = gluvv.volren.goodSamp;
        }
        break;
      case MouseEvent.BUTTON2:
        gluvv.volren.sampleRate = gluvv.volren.goodSamp;
        break;
      default:
        break;
    }
    // gluvv.pick.prim = null;

  }

  public void mouseClicked(MouseEvent e) {}

  public void mousePressed(MouseEvent e) {
    int x, y;
    picking = true;
    gluvvPrimitive prim;
    int button = e.getButton();
    int modifiers = e.getModifiers();
    x = e.getX();
    y = e.getY();

    if ( (modifiers & e.CTRL_MASK) != 0) {
      gluvv.mouse.ctrl = 1;
    }
    else {
      gluvv.mouse.ctrl = 0;
    }

    if ( (modifiers & e.SHIFT_MASK) != 0) {
      gluvv.mouse.shift = 1;
    }
    else {
      gluvv.mouse.shift = 0;
    }

    if ( (modifiers & e.ALT_MASK) != 0) {
      gluvv.mouse.alt = 1;
    }
    else {
      gluvv.mouse.alt = 0;
    }

    gluvv.pick.pos[0] = x;
    gluvv.pick.pos[1] = gluvv.win.height - y;

    gluvv.mouse.button = button;

    prim = gluvv.pick.prim;
    if (gluvv.pick.prim != null) {
      // gluvv.pick.prim.mousePressed(e);
      prim = gluvv.pick.prim;
      if (prim instanceof LTWidgetRen) {
        ( (LTWidgetRen) prim).mousePressed(e);
      }
      else if (prim instanceof CPWidgetRen) {
        ( (CPWidgetRen) prim).mousePressed(e);
      }
      else if (prim instanceof DPWidgetRen) {
        ( (DPWidgetRen) prim).mousePressed(e);
      }
      else if (prim instanceof TFWidgetRen) {
        ( (TFWidgetRen) prim).mousePressed(e);
      }
      return;
    }



     //                                          ???????????????????????????????  picking
     /*
     if (pick(drawable)) {
       if (gluvv.pick.prim != null) {
         gluvv.pick.prim.mousePressed(e);
         if (gluvv.pick.prim.pick(drawable) == 1 ) {

         }
       }
     }
     */

    switch (button) {
      case MouseEvent.BUTTON1:
        gluvv.mouse.button = MouseEvent.BUTTON1;
        if (gluvv.mouse.ctrl == 1 && gluvv.mouse.shift == 1) {
          // glut.glutSetWindowTitle("Rotate LIGHT");
          lball.start( (2.0f * x - gluvv.win.width) / gluvv.win.width,
                      (2.0f * y - gluvv.win.height) / gluvv.win.height);
          gluvv.mouse.pos[0] = gluvv.mouse.last[0] = x;
          gluvv.mouse.pos[1] = gluvv.mouse.last[1] = y;
          gluvv.mouse.ctrl = 1;
          gluvv.mouse.shift = 1;
        }
        else {
          // glut.glutSetWindowTitle("Rotate");
          tball.start( (2.0f * x - gluvv.win.width) / gluvv.win.width,
                      (2.0f * y - gluvv.win.height) / gluvv.win.height);
          gluvv.mouse.pos[0] = gluvv.mouse.last[0] = x;
          gluvv.mouse.pos[1] = gluvv.mouse.last[1] = y;
          gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        }
        break;
      case MouseEvent.BUTTON3:
        gluvv.mouse.button = MouseEvent.BUTTON3;
        if (gluvv.mouse.ctrl == 1) {
          // glutSetWindowTitle("Translate");
          gluvv.mouse.pos[0] = gluvv.mouse.last[0] = x;
          gluvv.mouse.pos[1] = gluvv.mouse.last[1] = y;
          gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        }
        else {
          // glutSetWindowTitle("Zoom");
          gluvv.mouse.pos[0] = gluvv.mouse.last[0] = x;
          gluvv.mouse.pos[1] = gluvv.mouse.last[1] = y;
          gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        }
        break;
      case MouseEvent.BUTTON2:
        gluvv.mouse.button = MouseEvent.BUTTON2;
        // glutSetWindowTitle("Translate");
        gluvv.mouse.pos[0] = gluvv.mouse.last[0] = x;
        gluvv.mouse.pos[1] = gluvv.mouse.last[1] = y;
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        break;
      default:
        break;
    }

  }

  public void mouseDragged(MouseEvent e) {
    int x, y;
    int button = e.getButton();
    int modifiers = e.getModifiers();
    gluvvPrimitive prim;
    x = e.getX();
    y = e.getY();
    float dx, dy;
    /*
             if(gluvv.pick.prim != null ){ //send events to a picked object       // ????????????????????????????????
            if(gluvv.pick.prim.move(x, y))
                    return;
      }
     */

    if (gluvv.pick.prim != null) {
      // gluvv.pick.prim.mousePressed(e);
      prim = gluvv.pick.prim;
      if (prim instanceof LTWidgetRen) {
        ( (LTWidgetRen) prim).mouseDragged(e);
      }
      else if (prim instanceof CPWidgetRen) {
        ( (CPWidgetRen) prim).mouseDragged(e);
      }
      else if (prim instanceof DPWidgetRen) {
        ( (DPWidgetRen) prim).mouseDragged(e);
      }
      else if (prim instanceof TFWidgetRen) {
        ( (TFWidgetRen) prim).mouseDragged(e);
      }
      return;
    }

    switch (gluvv.mouse.button) {
      case MouseEvent.BUTTON1:
        if (gluvv.mouse.ctrl == 1 && gluvv.mouse.shift == 1) {
          lball.update( (2.0f * x - gluvv.win.width) / gluvv.win.width,
                       (2.0f * y - gluvv.win.height) / gluvv.win.height);
          float[] lxf = new float[16];
          lball.buildRotMatrix(lxf);
          math.translateV3(gluvv.light.pos, lxf, gluvv.light.startpos);
          // glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, gluvv.light.pos);           // ????????????????????????????
        }
        else {
          tball.update( (2.0f * x - gluvv.win.width) / gluvv.win.width,
                       (2.0f * y - gluvv.win.height) / gluvv.win.height);
          tball.buildRotMatrix(gluvv.rinfo.xform);
        }
        gluvv.mouse.last[0] = x;
        gluvv.mouse.last[1] = y;
        break;
      case MouseEvent.BUTTON3:
        dx = (x - gluvv.mouse.last[0]) / (float) gluvv.win.width;
        dy = (y - gluvv.mouse.last[1]) / (float) gluvv.win.height;

        if (Math.abs(dy) > Math.abs(dx)) { //scale object
          gluvv.rinfo.scale += 5.0f * dy;
          if (gluvv.rinfo.scale < .05f) {
            gluvv.rinfo.scale = .05f;
          }
          if (gluvv.rinfo.scale > 10.0f) {
            gluvv.rinfo.scale = 10f;
          }
        }
        else { //zoom in
          gluvv.env.frustum[0] *= 1f + dx;
          gluvv.env.frustum[1] *= 1f + dx;
          gluvv.env.frustum[2] *= 1f + dx;
          gluvv.env.frustum[3] *= 1f + dx;
        }

        gluvv.mouse.last[0] = x;
        gluvv.mouse.last[1] = y;
        break;
      case MouseEvent.BUTTON2:
        dx = (x - gluvv.mouse.last[0]) / (float) gluvv.win.width;
        dy = (y - gluvv.mouse.last[1]) / (float) gluvv.win.height;
        gluvv.rinfo.trans[0] += dx *
            (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
            gluvv.env.eye[2] / gluvv.env.clip[0];
        gluvv.rinfo.trans[1] += dy *
            (gluvv.env.frustum[3] - gluvv.env.frustum[2]) *
            gluvv.env.eye[2] / gluvv.env.clip[0];
        gluvv.mouse.last[0] = x;
        gluvv.mouse.last[1] = y;
        break;
      default:
        break;
    }

  }

  public void keyPressed(KeyEvent e) {
    char key = e.getKeyChar();
    int keyCode = e.getKeyCode();
    gluvvPrimitive prim;

    if (key == 27) { //excape key
      gluvvQuit();
    }

    prim = gluvv.pick.prim;

    if (gluvv.pick.prim != null) {
      // gluvv.pick.prim.mousePressed(e);
      prim = gluvv.pick.prim;
      if (prim instanceof CPWidgetRen) {
        ( (CPWidgetRen) prim).keyPressed(e);
      }
      else if (prim instanceof DPWidgetRen) {
        ( (DPWidgetRen) prim).keyPressed(e);
      }
      else if (prim instanceof TFWidgetRen) {
        ( (TFWidgetRen) prim).keyPressed(e);
      }
      return;
    }


    switch (keyCode) {

      case KeyEvent.VK_F1: //open tf window
        if (gluvv.tf.tfWindow == 0) {
          tfwin.create();
        }
        else {
          tfwin.destroy();
        }
        break;
      case KeyEvent.VK_F9: //ambiant shade mode
        gluvv.shade = gluvvShade.gluvvShadeAmb;
        gluvv.tf.loadme = 1;
        break;
      case KeyEvent.VK_F10: //shaded
        gluvv.shade = gluvvShade.gluvvShadeDSpec;
        //gluvv.tf.loadme = 1;
        break;
      case KeyEvent.VK_F11: //faux shaded
        gluvv.shade = gluvvShade.gluvvShadeFaux;
        gluvv.tf.loadme = 1;
        break;
      case KeyEvent.VK_F12: //????
        gluvv.shade = gluvvShade.gluvvShadeDiff;
        //gluvv.tf.loadme = 1;
        break;
      default:
        break;

    }

    switch (key) { ///////////////////////////////////
      case 'm':

        /*
                                 if(gui.visible()) gui.hide();
                                 else gui.show();*/
        break;
      case ',': //decrease interactive sample rate
        gluvv.volren.interactSamp *= .95;
        System.err.println("*Interactive Sample Rate = " +
                           gluvv.volren.interactSamp);
        break;
      case '.': //increase interactive sample rate
        gluvv.volren.interactSamp *= 1f / .95f;
        System.err.println("*Interactive Sample Rate = " +
                           gluvv.volren.interactSamp);
        break;
      case '<': //decrease good sample rate
        gluvv.volren.goodSamp *= .95f;
        System.err.println("*High Quality Sample Rate = " +
                           gluvv.volren.goodSamp);
        break;
      case '>': //increase good sample rate
        gluvv.volren.goodSamp *= 1f / .95f;
        System.err.println("*High Quality Sample Rate = " +
                           gluvv.volren.goodSamp);
        break;
        /*
         case 'a':  //enable/disable alpha scale based on sample rate
                 if(gluvv.volren.scaleAlphas) gluvv.volren.scaleAlphas = 0;
                 else gluvv.volren.scaleAlphas = 1;
                 gluvv.volren.loadTLUT = 1;
                 glutPostRedisplay();
                 break;
         */
      case 'c': //enable/disable clipping plane
        if (gluvv.clip.on == 1) {
          gluvv.clip.on = 0;
        }
        else {
          gluvv.clip.on = 1;
        }

        // glutPostRedisplay();
        break;
      case 'k':
        gluvv.tf.brushon = 0;
        gluvv.tf.loadme = 1;

        // glutPostRedisplay();
        break;
      case 'b': //enable/disable transfer function brush
        if (gluvv.tf.brushon == 1 &&
            (gluvv.probe.brush == gluvvBrush.EllipseBrush)) {
          gluvv.tf.brushon =
              0;
        }
        else {
          gluvv.tf.brushon = 1;
        }
        gluvv.probe.brush = gluvvBrush.EllipseBrush;
        gluvv.tf.loadme = 1;

        // glutPostRedisplay();
        break;
      case 't': //automaticaly sized triangle brush
        if (gluvv.tf.brushon == 1 &&
            (gluvv.probe.brush == gluvvBrush.TriangleBrush)) {
          gluvv.tf.brushon =
              0;
        }
        else {
          gluvv.tf.brushon = 1;
        }
        gluvv.probe.slider = .5f;
        gluvv.probe.brush = gluvvBrush.TriangleBrush;
        gluvv.tf.loadme = 1;

        // glutPostRedisplay();
        break;
      case 'a': //automaticly sized ellipse brush
        if (gluvv.tf.brushon == 1 &&
            (gluvv.probe.brush == gluvvBrush.AutoEllipseBrush)) {
          gluvv.tf.
              brushon = 0;
        }
        else {
          gluvv.tf.brushon = 1;
        }
        gluvv.probe.slider = .5f;
        gluvv.probe.brush = gluvvBrush.AutoEllipseBrush;
        gluvv.tf.loadme = 1;

        // glutPostRedisplay();
        break;
      case 'o': //auto one d brush
        if (gluvv.tf.brushon == 1 &&
            (gluvv.probe.brush == gluvvBrush.AutoOneDBrush)) {
          gluvv.tf.brushon =
              0;
        }
        else {
          gluvv.tf.brushon = 1;
        }
        gluvv.probe.slider = .5f;
        gluvv.probe.brush = gluvvBrush.AutoOneDBrush;
        gluvv.tf.loadme = 1;

        // glutPostRedisplay();
        break;
      case 'O': //one d brush
        if (gluvv.tf.brushon == 1 &&
            ( (gluvv.probe.brush == gluvvBrush.AutoOneDBrush) ||
             (gluvv.probe.brush == gluvvBrush.OneDBrush))) {
          gluvv.tf.brushon = 0;
        }
        else {
          gluvv.tf.brushon = 1;
        }
        gluvv.probe.slider = .5f;
        gluvv.probe.brush = gluvvBrush.OneDBrush;
        gluvv.tf.loadme = 1;

        // glutPostRedisplay();
        break;
      case 'h': //toggle histogram
        if (gluvv.tf.histOn == 1) {
          gluvv.tf.histOn = 0;
        }
        else {
          gluvv.tf.histOn = 1;
        }

        // glutPostRedisplay();
        break;
      case 's': //toggle shade modes
      case 'S':
        if (gluvv.shade == gluvvShade.gluvvShadeAmb) {
          gluvv.shade = gluvvShade.
              gluvvShadeDiff;
        }
        else if (gluvv.shade == gluvvShade.gluvvShadeDiff) {
          gluvv.shade =
              gluvvShade.gluvvShadeDSpec;
        }
        else if (gluvv.shade == gluvvShade.gluvvShadeDSpec) {
          gluvv.shade =
              gluvvShade.gluvvShadeFaux;
        }
        else if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
          gluvv.shade =
              gluvvShade.gluvvShadeAmb;
        }
        gluvv.tf.loadme = 1;

        // glutPostRedisplay();
        break;
      case ' ': //paint action
        gluvv.tf.paintme = 1;

        // glutPostRedisplay();
        break;
      case 'd': //drop action
        gluvv.tf.dropme = 1;

        // glutPostRedisplay();
        break;
      case 'n': //clear painted tf
        gluvv.tf.clearpaint = 1;

        // glutPostRedisplay();
        break;
      case 'l': //toggle light widget on and off
        ltwr.onoff();

        // glutPostRedisplay();
        break;
      case '-':
      case '_': //change the current timestep - down
        gluvv.volren.timestep -= 1;
        if (gluvv.volren.timestep < gluvv.mv.tstart) {
          gluvv.volren.timestep = gluvv.mv.tstart;
          System.err.println("We are at the first time step : " +
                             gluvv.volren.timestep);
        }
        else {
          System.err.println("Current time step: " + gluvv.volren.timestep);
          if (gluvv.mv.swapTStep(gluvv.volren.timestep) == 0) {
            gluvv.mv.readAll(gluvv.volren.timestep);
            if (gluvv.mv1 != null) {
              gluvv.mv1.readAll(gluvv.volren.timestep);
            }
            if (gluvv.mv2 != null) {
              gluvv.mv2.readAll(gluvv.volren.timestep);
            }
            if (gluvv.mv3 != null) {
              gluvv.mv3.readAll(gluvv.volren.timestep);
            }
            initData();
            gluvv.mv.cacheTStep();
          }
        }

        // glutPostRedisplay();
        break;
      case '=':
      case '+': //change the current timesetp - up
        gluvv.volren.timestep += 1;
        if (gluvv.volren.timestep > gluvv.mv.tstop) {
          gluvv.volren.timestep = gluvv.mv.tstop;
          System.err.println("We are at the last time step :" +
                             gluvv.volren.timestep);
        }
        else {
          System.err.println("Current time step: " + gluvv.volren.timestep);
          if (gluvv.mv.swapTStep(gluvv.volren.timestep) == 0) {
            gluvv.mv.readAll(gluvv.volren.timestep);
            if (gluvv.mv1 != null) {
              gluvv.mv1.readAll(gluvv.volren.timestep);
            }
            if (gluvv.mv2 != null) {
              gluvv.mv2.readAll(gluvv.volren.timestep);
            }
            if (gluvv.mv3 != null) {
              gluvv.mv3.readAll(gluvv.volren.timestep);
            }
            initData();
            gluvv.mv.cacheTStep();
          }
        }

        // glutPostRedisplay();
        break;
      default:
        System.err.println("Unused Key press: '" + (char) key + "' = " +
                           (int) key
                           + " at ");
        break;
    } //////////////////////////////////////////////

  }

  /**
   * keyReleased.
   *
   * Invoked when a key has been released. See the class description for
   * KeyEvent for a definition of a key released event.
   */
  public void keyReleased(KeyEvent e) {

  }

  /**
   * keyTyped.
   *
   * Invoked when a key has been typed. See the class description for
   * KeyEvent for a definition of a key typed event.
   */
  public void keyTyped(KeyEvent e) {}

  //================================================== Reshape
//==========================================================
  public void reshape(GLAutoDrawable drawable, int x, int y, int w, int h) {
    GL gl = drawable.getGL();
    // w = 512; h = 512;
    System.err.println("new window : " + w + ", " + h);
    gl.glViewport(0, 0, (int) w - 1, (int) h - 1);
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    gluvv.env.frustum[0] *= (float) w / (float) gluvv.win.width;
    gluvv.env.frustum[1] *= (float) w / (float) gluvv.win.width;
    gluvv.env.frustum[2] *= (float) h / (float) gluvv.win.height;
    gluvv.env.frustum[3] *= (float) h / (float) gluvv.win.height;

    gl.glFrustum(gluvv.env.frustum[0],
                 gluvv.env.frustum[1],
                 gluvv.env.frustum[2],
                 gluvv.env.frustum[3],
                 gluvv.env.clip[0],
                 gluvv.env.clip[1]);

    gl.glMatrixMode(GL.GL_MODELVIEW);

    gluvv.win.width = w;
    gluvv.win.height = h;
  }

  //=================================================== Pick
//========================================================
//================================================== begin
//--- lesson in picking ---------------------------------------
// rules for gluvv picking say that first you push your
//  'this' pointer (1 int for 32 bit, 2 for 64)
//       -(must be a subclass of gluvvPrimitive!)
//  finaly push three (3) ints on the stack
//       -(sub object names - anything you want)
// to recap: you push 4 ints (total) in 32 bit, and 5 (total)in 64
//--------------------------------------------------------------

  private int pick(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gluvv.pick.data1 = 0;
    gluvv.pick.data2 = 0;
    gluvv.pick.name32 = 0;
    gluvv.pick.name64 = 0;
    gluvv.pick.z = 0xffffffff;
    if (gluvv.pick.prim != null) {
          if (gluvv.pick.prim instanceof LTWidgetRen) {
            ( (LTWidgetRen) gluvv.pick.prim).release();
          }
          else if (gluvv.pick.prim  instanceof CPWidgetRen) {
            ( (CPWidgetRen) gluvv.pick.prim ).release();
          }
          else if (gluvv.pick.prim instanceof DPWidgetRen) {
            ( (DPWidgetRen) gluvv.pick.prim).release();
          }
          else if (gluvv.pick.prim instanceof TFWidgetRen) {
            ( (TFWidgetRen) gluvv.pick.prim).release();
          }
          gluvv.pick.prim.release();
          gluvv.pick.prim = null;
    }

    int[] viewport = new int[4];
    gl.glGetIntegerv(GL.GL_VIEWPORT, viewport, 0);

    float[] projection = new float[16];
    gl.glGetFloatv(GL.GL_PROJECTION_MATRIX, projection, 0);

    int pickBufferSize = 128;
    // int[] pickBuffer = new int[pickBufferSize];
    IntBuffer selectionBuffer;

    ByteBuffer byteBuffer = ByteBuffer.allocateDirect(
        BufferUtil.SIZEOF_INT * pickBufferSize);
    byteBuffer.order(ByteOrder.nativeOrder());
    selectionBuffer = byteBuffer.asIntBuffer();

    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glPushMatrix();
    {
      gl.glLoadIdentity();
      double pickbox = 4;
      glu.gluPickMatrix( (double) gluvv.pick.pos[0], (double) gluvv.pick.pos[1],
                        pickbox, pickbox, viewport, 0);

      gl.glMultMatrixf(projection, 0);
      gl.glMatrixMode(GL.GL_MODELVIEW);

      gl.glSelectBuffer(selectionBuffer.capacity(), selectionBuffer);
      gl.glRenderMode(GL.GL_SELECT);

      gl.glInitNames();
      gl.glPushName(0);
      gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

      // ---- RENDER -------------------------------
      //cycle through scene graph ------------------
      gluvv.picking = 1;
      displayLocal(drawable);
      gluvv.picking = 0;
    }
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glPopMatrix();
    gl.glMatrixMode(GL.GL_MODELVIEW);

    gl.glFlush();

    // now see what we got!
    int hits = gl.glRenderMode(GL.GL_RENDER);
    //cerr << "hits = " << hits;
    int nnames, z, minz = 0xffffffff;
    int gotone = 0;

    if (hits > 0) {


      //i tracks the number of hits, j tracks postion in pick buffer
      int i, j;
      for (i = 0, j = 0; (j < pickBufferSize && i < hits); i++) {
        //first item on stack is the number of items
        nnames = selectionBuffer.get(j++);
        // System.err.println("number names = " + nnames);
        z = selectionBuffer.get(j++); //second is the z value for this hit
        if (nnames > 1 && z <= minz) {
          gotone = 1;
          minz = z;
          gluvv.pick.z = minz;

          j += nnames - 3;
          gluvv.pick.name32 = selectionBuffer.get(j++);
          gluvv.pick.data1 = selectionBuffer.get(j++);
          gluvv.pick.data2 = selectionBuffer.get(j++);
          gluvv.pick.data3 = selectionBuffer.get(j++);
          gluvv.pick.prim = getPickedObject(gluvv.pick.name32);
          /*
          System.err.println("name32 = " + gluvv.pick.name32 );
          System.err.println("gluvv.pick.data1 = " + gluvv.pick.data1 );
          System.err.println("gluvv.pick.data2 = " + gluvv.pick.data2 );
          System.err.println("gluvv.pick.data3 = " + gluvv.pick.data3 );
          */
        }
        else {
          j += nnames + 1;
        }
      }
    }

    // pickBuffer = null;

    if (gotone == 1) {
      return 1;
    }

    return 0;

  }

  private gluvvPrimitive getPickedObject(int hashCode) {
    gluvvPrimitive tmp = renderables.getNext();
    while (tmp != null) {
      if (tmp instanceof LTWidgetRen) {
        if ( ( (LTWidgetRen) tmp).hashCode() == hashCode) {
          // System.err.println("LTWidgetRen");
          return ( (LTWidgetRen) tmp);
        }
      }
      else if (tmp instanceof CPWidgetRen) {
        if ( ( (CPWidgetRen) tmp).hashCode() == hashCode) {
          // System.err.println("CPWidgetRen");
          return ( (CPWidgetRen) tmp);
        }
      }
      else if (tmp instanceof DPWidgetRen) {
        if ( ( (DPWidgetRen) tmp).hashCode() == hashCode) {
          // System.err.println("DPWidgetRen");
          return ( (DPWidgetRen) tmp);
        }
      }
      else if (tmp instanceof TFWidgetRen) {
        if ( ( (TFWidgetRen) tmp).hashCode() == hashCode) {
          // System.err.println("TFWidgetRen");
          return ( (TFWidgetRen) tmp);
        }
      }
      else if (tmp instanceof NV20VolRen) {
        if ( ( (NV20VolRen) tmp).hashCode() == hashCode) {
          // System.err.println("NV20VolRen");
          return  ( (NV20VolRen) tmp);
        }
      }
      else if (tmp instanceof NV20VolRen3D) {
        if ( ( (NV20VolRen3D) tmp).hashCode() == hashCode) {
          // System.err.println("NV20VolRen3D");
          return ( (NV20VolRen3D) tmp);
        }
      }
      else if (tmp instanceof NV20VolRen3D2) {
        if ( ( (NV20VolRen3D2) tmp).hashCode() == hashCode) {
          // System.err.println("NV20VolRen3D2");
          return ( (NV20VolRen3D2) tmp);
        }
      }

      tmp = tmp.getNext();
    }
    return null;
  }


  //=================================================== Pick
//========================================================
//==================================================== end

//======================================= check extensions
//========================================================
  private int checkExts() {
    //use this to check if we can support the things we want to do
    // in the future this should check to see what render modes
    //  that we can use.
    // Many thanks to Nate Robins for testing this on his GF2!!!!!

    // gluvv.plat = (gluvvPlatform) CardProfile();
    gluvv.plat = gluvvPlatform.GPNV20;
    /*
                         if(!glTexImage3DEXT)
           return 0;
                         if(!glActiveTexture)
           return 0;
     */
    //if(!glCombinerParameterfvNV)
    //return 0;

    return 1;
  }

  //=================================== end check extensions
//========================================================

//============================================== init Data
//========================================================
  private int initData() {
    int ret = 0;

    System.err.println("Initializing Data");
    if (gluvv.dmode == gluvvDataMode.GDM_V1 ||
        gluvv.dmode == gluvvDataMode.GDM_V1G ||
        gluvv.dmode == gluvvDataMode.GDM_V2 ||
        gluvv.dmode == gluvvDataMode.GDM_V2G ||
        gluvv.dmode == gluvvDataMode.GDM_V3 ||
        gluvv.dmode == gluvvDataMode.GDM_V3G ||
        gluvv.dmode == gluvvDataMode.GDM_V4) {
      ret += gluvv.mv.mergeMV(blurnorms, 1, 0, gluvv.mv1, gluvv.mv2, gluvv.mv3);
    }
    else if (gluvv.dmode == gluvvDataMode.GDM_V1GH ||
             gluvv.dmode == gluvvDataMode.GDM_V2GH) {
      ret += gluvv.mv.mergeMV(blurnorms, 1, 1, gluvv.mv1, gluvv.mv2, gluvv.mv3);
    }
    else if (gluvv.dmode == gluvvDataMode.GDM_VGH ||
             gluvv.dmode == gluvvDataMode.GDM_VGH_VG ||
             gluvv.dmode == gluvvDataMode.GDM_VGH_V) {
      gluvv.mv.normalsVGH(blurnorms, 1);
    }

    int bricksz = 128 * 128 * 256;

    if (gluvv.dmode == gluvvDataMode.GDM_V1 ||
        gluvv.dmode == gluvvDataMode.GDM_VGH_V) {
      bricksz = 128 * 128 * 256;
    }
    else if (gluvv.dmode == gluvvDataMode.GDM_V1G ||
             gluvv.dmode == gluvvDataMode.GDM_V2 ||
             gluvv.dmode == gluvvDataMode.GDM_VGH_VG) {
      bricksz = 128 * 128 * 256;
    }
    else {
      bricksz = 128 * 128 * 256;
    }

    gluvv.mv.padOut();
    gluvv.mv.brick(bricksz);
    gluvv.mv.printInfo();

    System.err.println(" -done with data initialization");
    return ret;
  }

  //================================================== Parse
//========================================================
  private int parse(int argc, String[] argv) {
    //Yeah yeah, I should use lex and yack for this, so shoot me...
    if (argc < 2) {
      System.err.println(argv[0] + "  Usage:\n");
      System.err.println(" File flags:\n");
      System.err.println("     -t   [file name.trex] Trex file\n");
      System.err.println(
          "    <-t2  [file name.trex]> Optional 2nd Channel Trex file\n");
      System.err.println(
          "    <-t3  [file name.trex]> Optional 3rd Channel Trex file\n");
      System.err.println(
          "    <-t4  [file name.trex]> Optional 4th Channel Trex file\n");
      System.err.println(
          "    <-addG>                 Add a gradient measure to N channel data\n");
      System.err.println(
          "    <-addH>                 Add a hessian measure to N channel data\n");
      System.err.println("     -n   [file name.nrrd]  Nrrd file\n");
      System.err.println("     -vgh [file name.nrrd]  VGH Nrrd file\n");
      System.err.println(
          "    <-useVG>                Only use V&G from VGH Nrrd file (2B)\n");
      System.err.println(
          "    <-useV>                 Only use V from VGH Nrrd file (1B)\n");
      System.err.println(" Other options:\n");
      System.err.println("    <-blurNormals>          Blurr normals\n");
      //cerr << " Platform flags:\n";
      //cerr << "     -w       Intense3D Wildcat\n";
      //cerr << "     -o       SGI Octane\n";
      //cerr << "     -o2      SGI Octane2\n";
      //cerr << "     -i       SGI Infinite Reality\n";
      //cerr << "     -nv15    nVidia GeForce 2\n";
      //cerr << "     -nv20    nVidia GeForce 3\n (DEFAULT)";
      //cerr << "     -nv202D  nVidia GeForce 3 with 2D textures only\n";
      gluvvQuit();
    }
    for (int i = 0; i < argc; ++i) { //////////////////////// for
      //-------------- trex flags ------------------------------
      if (argv[i].equals("-t")) { //parse a trex file**********************************
        ++i;
        if (i >= argc) {
          System.err.println("parse Error: -t specified without a file name \n");
          return 1;
        }
        System.err.println("  Loading TREX file " + argv[i]);
        gluvv.mv = new MetaVolume(argv[i], 0);
        if (gluvv.mv.readAll(gluvv.mv.tstart) == 0) {
          return 1;
        }
        gluvv.volren.timestep = gluvv.mv.tstart;
      }
      else if (argv[i].equals("-t2")) { //parse a trex file****************************
        ++i;
        if (i >= argc) {
          System.err.println(
              "parse Error: -t2 specified without a file name \n");
          return 1;
        }
        System.err.println("  Loading TREX file " + argv[i]);
        gluvv.mv1 = new MetaVolume(argv[i], 0);
        if (gluvv.mv1.readAll(gluvv.mv1.tstart) == 0) {
          return 1;
        }
        gluvv.dmode = gluvvDataMode.GDM_V2;
      }
      else if (argv[i].equals("-t3")) { //parse a trex file****************************
        ++i;
        if (i >= argc) {
          System.err.println(
              "parse Error: -t3 specified without a file name \n");
          return 1;
        }
        System.err.println("  Loading TREX file " + argv[i]);
        gluvv.mv2 = new MetaVolume(argv[i], 0);
        if (gluvv.mv2.readAll(gluvv.mv2.tstart) == 0) {
          return 1;
        }
        gluvv.dmode = gluvvDataMode.GDM_V3;
      }
      else if (argv[i].equals("-t4")) { //parse a trex file****************************
        ++i;
        if (i >= argc) {
          System.err.println(
              "parse Error: -t4 specified without a file name \n");
          return 1;
        }
        System.err.println("  Loading TREX file " + argv[i]);
        gluvv.mv3 = new MetaVolume(argv[i], 0);
        if (gluvv.mv3.readAll(gluvv.mv3.tstart) == 0) {
          return 1;
        }
        gluvv.dmode = gluvvDataMode.GDM_V4;
      }
      else if (argv[i].equals("-addG")) { //add a gradient comp************************
        if (gluvv.dmode == gluvvDataMode.GDM_V1) {
          gluvv.dmode = gluvvDataMode.GDM_V1G;
        }
        else if (gluvv.dmode == gluvvDataMode.GDM_V2) {
          gluvv.dmode = gluvvDataMode.GDM_V2G;
        }
        else if (gluvv.dmode == gluvvDataMode.GDM_V3) {
          gluvv.dmode = gluvvDataMode.GDM_V3G;
        }
        else {
          System.err.println(
              "parse Error: you cannot add gradient measure to this data type");
          return 1;
        }
      }
      else if (argv[i].equals("-addH")) { //add a hessian comp***************************
        if (gluvv.dmode == gluvvDataMode.GDM_V1G) {
          gluvv.dmode = gluvvDataMode.GDM_V1GH;
        }
        else if (gluvv.dmode == gluvvDataMode.GDM_V2G) {
          gluvv.dmode = gluvvDataMode.GDM_V2GH;
        }
        else {
          System.err.println(
              "parse Error: you cannot add hessian measure to this data type");
          return 1;
        }
      }
      //----------------- Nrrd flags --------------------------
      else if (argv[i].equals("-n")) { //parse a nrrd file******************************
        ++i;
        if (i >= argc) {
          System.err.println("parse Error: -n specified without a file name \n");
          return 1;
        }
        System.err.println("  Loading NRRD file " + argv[i]);
        gluvv.mv = new MetaVolume(argv[i], 1);
        if (gluvv.mv.readNrrd(gluvv.debug) == 0) {
          return 1;
        }
        if (gluvv.mv.nelts == 1) {
          gluvv.dmode = gluvvDataMode.GDM_V1;
        }
        else if (gluvv.mv.nelts == 2) {
          gluvv.dmode = gluvvDataMode.GDM_V2;
        }
        else if (gluvv.mv.nelts == 3) {
          gluvv.dmode = gluvvDataMode.GDM_V3;
        }
        else if (gluvv.mv.nelts == 4) {
          gluvv.dmode = gluvvDataMode.GDM_V4;
        }
      }
      else if (argv[i].equals("-vgh")) { //parse an old school vgh file****************
        ++i;
        if (i >= argc) {
          System.err.println(
              "parse Error: -vgh specified without a file name \n");
          return 1;
        }
        System.err.println("  Loading NRRD file " + argv[i]);
        gluvv.mv = new MetaVolume(argv[i], 1);
        if (gluvv.mv.readNrrd(gluvv.debug) == 0) {
          return 1;
        }
        if ( (gluvv.dmode != gluvvDataMode.GDM_VGH_VG) &&
            (gluvv.dmode != gluvvDataMode.GDM_VGH_V)) {
          gluvv.dmode = gluvvDataMode.GDM_VGH;
        }
        if (gluvv.mv.nelts == 1) { //dont let idiots load a vgh file that isnt one
          if (gluvv.dmode == gluvvDataMode.GDM_VGH_V) {
            gluvv.dmode = gluvvDataMode.GDM_V1;
          }
          if (gluvv.dmode == gluvvDataMode.GDM_VGH_VG) {
            gluvv.dmode = gluvvDataMode.GDM_V1G;
          }
          else {
            gluvv.dmode = gluvvDataMode.GDM_V1GH;
          }
        }
        else if (gluvv.mv.nelts != 3) { //I cannot recover from this one, explode!!
          System.err.println(
              "parse Error: -vgh flag is for nrrd volume with pre-computed VGH");
          return 1;
        }
      }
      else if (argv[i].equals("-useVG")) { //only use VG from a VGH file*****
        gluvv.dmode = gluvvDataMode.GDM_VGH_VG;
      }
      else if (argv[i].equals("-useV")) { //only use VG from a VGH file******
        gluvv.dmode = gluvvDataMode.GDM_VGH_V;
      }
      //------------------ misc flags --------------------
      else if (argv[i].equals("-blurNormals") || //blur normals************
               argv[i].equals("-blurnormals")) {
        blurnorms = 1;
      }
      else if (argv[i].equals("-shadow")) {
        gluvv.light.shadow = 1;
      }
      //------------------ platform flags --------------------
      else if (argv[i].equals("-w")) { //wildcat card************************
        gluvv.plat = gluvvPlatform.GPWildcat;
      }
      else if (argv[i].equals("-o")) { //ocatain*****************************
        gluvv.plat = gluvvPlatform.GPOctane;
      }
      else if (argv[i].equals("-o2")) { //octain2*****************************
        gluvv.plat = gluvvPlatform.GPOctane2;
      }
      else if (argv[i].equals("-i")) { //ir**********************************
        gluvv.plat = gluvvPlatform.GPInfinite;
      }
      else if (argv[i].equals("-nv15")) { //Geforce2*************************
        gluvv.plat = gluvvPlatform.GPNV15;
      }
      else if (argv[i].equals("-nv20")) { //Geforce3*************************
        gluvv.plat = gluvvPlatform.GPNV20;
      }
      else if (argv[i].equals("-nv202D")) { //Geforce3 w/2D textures*********
        gluvv.plat = gluvvPlatform.GPNV202D;
      }
      else if (argv[i].equals("-d")) { //Print some debug info***************
        gluvv.debug = 1;
      }
    } /////////////////////////////////////////////////// for
    return 0;
  }

//=================================================================
//=================================================================

//================================================== gluvv Quit
//==== delete crap here =======================================
  private void gluvvQuit() {

    System.err.println("cleaning up\n");

    System.err.println("Exiting, by by\n");
    System.exit(0); //clean exit
  }

}
