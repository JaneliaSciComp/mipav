package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;
import java.awt.event.*;
import java.io.*;
import javax.imageio.stream.*;
import java.util.*;
import java.nio.*;

public class TFWindow
    extends gluvvPrimitive implements KeyListener, MouseListener,
    MouseMotionListener {

  public static int COLOR_MAP_NAME = 66666;
  public static int ALPHA_MAP_NAME = 66667;
  public static int POINTS_NAME = 66668;
  public static int HIQ_SAMP_NAME = 66669;
  public static int LOQ_SAMP_NAME = 66670;

  public int width;
  public int height;

  private float[] bots = new float[3];
  private float[] tops = new float[3];

  private float pntsHeight;
  private float[][] exts = new float[2][2];

  private int[] deptexName = new int[1]; // unsigned

  private int data1;
  private int data2;
  private int data3;
  private int name32;
  private long name64;
  private int zpick;
  private int picking;

  private int[] pickPos = new int[2];
  private int butt;

  private TFPoint points;

  private GLUT glut = new GLUT();
  private GLU glu = new GLU();

  private gluvvGlobal gluvv;
  private VectorMath math = new VectorMath();

  private boolean picked = false;

  public TFWindow() {
    // gluvv = _gluvv;
    width = 512;
    height = 256;
    picking = 0;

    points = new TFPoint();

    points.setBin( -99999); //dummy node, never select it!!!

    TFPoint tp;
    tp = new TFPoint(0, 0f, .1f, 0f);
    tp.cpick.reset(.6f, 1f, .5f);
    points.insert(tp);
    tp = new TFPoint(128, .5f, 1f, 0f);
    tp.cpick.reset(.3f, 1f, .5f);
    points.insert(tp);
    tp = new TFPoint(255, 1f, .1f, 0f);
    tp.cpick.reset(0f, 1f, .5f);
    points.insert(tp);

    deptexName[0] = 0;

    data1 = 0;
    data2 = 0;
    data3 = 0;
    name32 = 0;
    name64 = 0;
    zpick = 0xffffffff;

    exts[0][0] = -.03f;
    exts[0][1] = 1.03f;
    exts[1][0] = 1.03f;
    exts[1][1] = -.03f;
  }

  public void setGluvvGlobal( gluvvGlobal _gluvv) {   // ???????????????????????????????????????????
    gluvv = _gluvv;
  }

  //--------- functions to make c & c++ callbacks play nice together
  /*
     public void tfwinstaticdisplay(){
          if(gluvv.tf.tfwin){
                  gluvv.tf.tfwin.display();
          }
     }

     public void tfwinstaticmotion(int x, int y){
          if(gluvv.tf.tfwin){
                  gluvv.tf.tfwin.motion(x,y);
          }
     }

     public void tfwinstaticmouse(int button, int state, int x, int y)
     {
          if(gluvv.tf.tfwin){
                  gluvv.tf.tfwin.mouse(button, state, x, y);
          }
     }

     public void tfwinstaticreshape(int w, int h)
     {
          if(gluvv.tf.tfwin){
                  gluvv.tf.tfwin.reshape(w,h);
          }
     }

     public void tfwinstatickey(byte key, int x, int y)
     {
          if(gluvv.tf.tfwin){
                  gluvv.tf.tfwin.key(key, x, y);
          }
     }

     public void tfwinstaticspecial(int key, int x, int y)
     {
          if(gluvv.tf.tfwin){
                  gluvv.tf.tfwin.special(key, x, y);
          }
     }

     public void tfwinstaticidle(){
          if(gluvv.tf.tfwin){
                  gluvv.tf.tfwin.idle();
          }
     }
   */

  public void create() {
    String displaymode = new String();
    //char displaymode[] = "1";
    /*
         if(gluvv.plat == GPOctane){
      displaymode = "rgba=8 depth";
         }
         else if(gluvv.plat == GPOctane2){
      displaymode = "rgba=8 double depth";
         }
         else if(gluvv.plat == GPInfinite)
      displaymode = "double rgba depth";
         else{
      displaymode = "double rgba depth";
         }

         //glutInitDisplayString(displaymode);
         //glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH | GLUT_ACCUM);
         glutInitWindowSize(width, height);
         glutInitWindowPosition(200, 400);
         gluvv.tf.tfWindow= glutCreateWindow("Vanilla Transfer Function");

         glutDisplayFunc(tfwinstaticdisplay);
         glutReshapeFunc(tfwinstaticreshape);
         glutMouseFunc(tfwinstaticmouse);
         glutMotionFunc(tfwinstaticmotion);
         glutKeyboardFunc(tfwinstatickey);
         glutSpecialFunc(tfwinstaticspecial);
         glutIdleFunc(tfwinstaticidle);
     */
  }

  public void init(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    gl.glShadeModel(GL.GL_SMOOTH);

    /* Set up lighting. mostly for widgets */
    float[] ambient = {0.1f, 0.1f, 0.1f, 1.0f};
    float[] diffuse = {0.5f, 1.0f, 1.0f, 1.0f};
    float[] front_mat_shininess = {60.0f};
    float[] front_mat_specular = {0.2f, 0.2f, 0.2f, 1.0f};
    float[] front_mat_diffuse = {0.5f, 0.28f, 0.38f, 1.0f};
    float[] lmodel_ambient = {0.2f, 0.2f, 0.2f, 1.0f};
    float[] lmodel_twoside = {GL.GL_FALSE};

    FloatBuffer ambientBuf = FloatBuffer.wrap(ambient);
    ambientBuf.rewind();
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_AMBIENT, ambientBuf);

    FloatBuffer diffuseBuf = FloatBuffer.wrap(diffuse);
    diffuseBuf.rewind();
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_DIFFUSE, diffuseBuf);

    FloatBuffer lightBuf = FloatBuffer.wrap(gluvv.light.pos);
    lightBuf.rewind();
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, lightBuf);
    gl.glEnable(GL.GL_LIGHT0);

    FloatBuffer m_ambientBuf = FloatBuffer.wrap(lmodel_ambient);
    m_ambientBuf.rewind();
    gl.glLightModelfv(GL.GL_LIGHT_MODEL_AMBIENT, m_ambientBuf);

    FloatBuffer m_twosideBuf = FloatBuffer.wrap(lmodel_twoside);
    m_twosideBuf.rewind();
    gl.glLightModelfv(GL.GL_LIGHT_MODEL_TWO_SIDE, m_twosideBuf);
    gl.glEnable(GL.GL_LIGHTING);

    gl.glEnable(GL.GL_COLOR_MATERIAL);
    FloatBuffer front_mat_shininessBuf = FloatBuffer.wrap(front_mat_shininess);
    front_mat_shininessBuf.rewind();
    gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_SHININESS,
                    front_mat_shininessBuf);

    FloatBuffer front_mat_specularBuf = FloatBuffer.wrap(front_mat_specular);
    front_mat_specularBuf.rewind();
    gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_SPECULAR, front_mat_specularBuf);

    FloatBuffer front_mat_diffuseBuf = FloatBuffer.wrap(front_mat_diffuse);
    front_mat_diffuseBuf.rewind();
    gl.glMaterialfv(GL.GL_FRONT_AND_BACK, GL.GL_DIFFUSE, front_mat_diffuseBuf);

    gl.glEnable(GL.GL_DEPTH_TEST);
    gl.glDepthFunc(GL.GL_LEQUAL);
    gl.glDisable(GL.GL_CULL_FACE);

    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    gl.glOrtho(0, 1, 0, 1, -1, 1);

    gl.glMatrixMode(GL.GL_MODELVIEW);
    gl.glLoadIdentity();

    System.out.println("TFWindow::" + "init()");

    gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
  }

  public void destroy() {
    // glut.glutDestroyWindow(gluvv.tf.tfWindow);
    gluvv.tf.tfWindow = 0;
    deptexName = null;
  }

  public void display(GLAutoDrawable drawable) {

    if ( picked == true ) {
      pick(drawable);
    }
    GL gl = drawable.getGL();
    gl.glDrawBuffer(GL.GL_BACK);
    gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

    gl.glColor4f(1, 0, 0, 1);
    //glutSolidSphere(1,10,10);

    gl.glColor4f(1, 1, 1, 1); //put a border around it
    gl.glBegin(GL.GL_LINE_STRIP);
    {
      gl.glVertex3f( -.01f, -.01f, 0f);
      gl.glVertex3f(1.01f, -.01f, 0f);
      gl.glVertex3f(1.01f, 1.01f, 0f);
      gl.glVertex3f( -.01f, 1.01f, 0f);
      gl.glVertex3f( -.01f, -.01f, 0f);
    }
    gl.glEnd();

    if (gluvv.volren.deptex != null) {
      draw2Dtf(drawable);
    }
    else if (gluvv.volren.tlut != null) {
      draw1Dtf(drawable);
    }

    // glutSwapBuffers();
    drawable.swapBuffers();

  }

  public void idle() {

  }

  public void reshape(GLAutoDrawable drawable, int w, int h) {
    GL gl = drawable.getGL();
    width = w;
    height = h;

    gl.glViewport(0, 0, w, h);
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    gl.glOrtho( -.03f, 1.03f, -.03f, 1.03f, -1, 1);

    gl.glMatrixMode(GL.GL_MODELVIEW);
    gl.glLoadIdentity();

    System.out.println(" - is current window vs " + gluvv.tf.tfWindow);

    GlErr(drawable, "TFWindow::", "reshape()");
    // glutPostRedisplay();
  }

  private void draw2Dtf(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    if (deptexName != null) {

      gl.glGenTextures(1, deptexName, 0);
      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);

      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER,
                         GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER,
                         GL.GL_LINEAR);

      ByteBuffer bBuffer = ByteBuffer.wrap(gluvv.volren.deptex);
      bBuffer.rewind();

      gl.glTexImage2D(GL.GL_TEXTURE_2D,
                      0,
                      GL.GL_RGBA8,
                      256,
                      256,
                      0,
                      GL.GL_RGBA,
                      GL.GL_UNSIGNED_BYTE,
                      bBuffer);
      gl.glFlush();
    }

    //cerr << "drawing 2d tf" << endl;
    gl.glEnable(GL.GL_BLEND);
    gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ZERO);

    gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);
    gl.glBegin(GL.GL_QUADS);
    {
      gl.glTexCoord2f(0, 0);
      gl.glVertex3f(0, 0, -1);
      gl.glTexCoord2f(1, 0);
      gl.glVertex3f(1, 0, -1);
      gl.glTexCoord2f(1, 1);
      gl.glVertex3f(1, 1, -1);
      gl.glTexCoord2f(0, 1);
      gl.glVertex3f(0, 1, -1);
    }
    gl.glEnd();

    gl.glDisable(GL.GL_BLEND);

  }

  private void draw1Dtf(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glEnable(GL.GL_DEPTH_TEST);

    TLUT tl = gluvv.volren.tlut;
    if (tl == null) {
      return;
    }

    float top = .2f; //set up dimensions for color band
    float bottom = 0f;
    float dx = 1.0f / 256.0f;

    float[] cv = tl.GetRGBA(0);
    float[] c = new float[4];

    gl.glPushName(this.hashCode());
    gl.glPushName( -1);
    gl.glPushName( -1);
    gl.glPushName( -1);

    gl.glEnable(GL.GL_BLEND); //blend color band
    gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ZERO);

    gl.glLoadName(COLOR_MAP_NAME); //draw color band
    gl.glBegin(GL.GL_QUADS);
    int index = 0;
    for (int i = 0; i < 256; ++i) {
      c[0] = cv[index + 0];
      c[1] = cv[index + 1];
      c[2] = cv[index + 2];
      c[3] = cv[index + 3];
      gl.glColor4fv(c, 0);
      gl.glVertex3f(i * dx, bottom, 0);
      gl.glVertex3f( (i + 1) * dx, bottom, 0);
      gl.glVertex3f( (i + 1) * dx, top, 0);
      gl.glVertex3f(i * dx, top, 0);
      // cv += 4;
      index += 4;
    }

    gl.glEnd();

    gl.glLoadName( -1);

    gl.glDisable(GL.GL_BLEND); //dont blend no more

    top = 1; //set up dimensions for alpha band
    bottom = .2f;
    pntsHeight = top - bottom;

    gl.glLoadName(ALPHA_MAP_NAME); //draw a back drop for picking
    gl.glBegin(GL.GL_QUADS);
    gl.glColor4f(0, 0, 0, 1);
    gl.glVertex3f(0f, bottom, -.1f);
    gl.glVertex3f(1f, bottom, -.1f);
    gl.glVertex3f(1f, top, -.1f);
    gl.glVertex3f(0f, top, -.1f);
    gl.glEnd();

    gl.glLoadName( -1);
    gl.glPopName(); //set up point names
    gl.glLoadName(POINTS_NAME);
    gl.glPushName( -1);

    TFPoint p;
    float[] v0 = {0f, bottom, 0f}; //set up some vertex & color buffers
    float[] v1 = {1f, bottom, 0f};
    float[] c0 = {0f, 0f, 0f};
    float[] c1 = {0f, 0f, 0f};

    for (int i = 0; i < 256; ++i) { //check each bin for a point
      p = points.findPoint(i);
      if (p != null) { //found a point
        gl.glPushMatrix();
        v1[0] = p.operator(0);

        v1[1] = (float)math.affine(0, p.operator(1), 1, bottom, top);

        gl.glColor3f(1f, 1f, 1f); //connect points with colored lines
        p.cpick.getColor(c1);
        gl.glBegin(GL.GL_LINES);

        gl.glColor3fv(c0, 0);
        gl.glVertex3fv(v0, 0);
        gl.glColor3fv(c1, 0);
        gl.glVertex3fv(v1, 0);
        gl.glEnd();

        float ballsz = .01f; //set up ball size

        gl.glLoadName(i); //point name
        gl.glTranslatef(v1[0], v1[1], v1[2]); //spot to draw it
        if ( (data2 == POINTS_NAME) && (data3 == i)) {
          gl.glColor3f(1, 0, 0);
        }
        else {
          gl.glColor3f(1, 1, 1);
        }
        gl.glBegin(GL.GL_QUADS); //draw box behind it
        gl.glVertex3f( -ballsz * 1.2f, ballsz * 1.2f, 0f);
        gl.glVertex3f(ballsz * 1.2f, ballsz * 1.2f, 0f);
        gl.glVertex3f(ballsz * 1.2f, -ballsz * 1.2f, 0f);
        gl.glVertex3f( -ballsz * 1.2f, -ballsz * 1.2f, 0f);
        gl.glEnd();
        gl.glColor3fv(c1, 0);
        glut.glutSolidSphere(ballsz, 10, 10); //draw the ball (colored)
        gl.glLoadName( -1);

        v0[0] = v1[0];
        v0[1] = v1[1];
        v0[2] = v1[2]; //fix buffers
        c0[0] = c1[0];
        c0[1] = c1[1];
        c0[2] = c1[2];
        gl.glPopMatrix();
      }
    }

    v1[0] = 1; //set up last line
    v1[1] = bottom;
    c1[0] = c1[1] = c1[2] = 0;
    gl.glPopName();
    gl.glLoadName( -1);
    gl.glPushName( -1);

    gl.glBegin(GL.GL_LINES); //draw last line
    gl.glColor3fv(c0, 0);
    gl.glVertex3fv(v0, 0);
    gl.glColor3fv(c1, 0);
    gl.glVertex3fv(v1, 0);
    gl.glEnd();

    gl.glPushMatrix(); //sample rate sliders!
    gl.glLoadName(LOQ_SAMP_NAME);
    gl.glColor3f(.5f, 0.f, 7f);
    gl.glTranslatef(gluvv.volren.interactSamp / 6, 0, 0);
    glut.glutSolidSphere(.015d, 10, 10); //draw the ball (colored)
    gl.glPopMatrix();

    gl.glPushMatrix();
    gl.glLoadName(HIQ_SAMP_NAME);
    gl.glColor3f(.7f, 0.f, 5f);
    gl.glTranslatef(gluvv.volren.goodSamp / 6, 0, 0);
    glut.glutSolidSphere(.02d, 10, 10); //draw the ball (colored)
    gl.glPopMatrix();

  }

  public int pick(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    data1 = 0;
    data2 = 0;
    data3 = 0;
    name32 = 0;
    name64 = 0;
    zpick = 0xffffffff;

    int[] viewport = new int[4];
    gl.glGetIntegerv(GL.GL_VIEWPORT, viewport, 0);

    float[] projection = new float[16];
    gl.glGetFloatv(GL.GL_PROJECTION_MATRIX, projection, 0);

    int pickBufferSize = 128;
    // unsigned int array
    // int[] pickBuffer = new int[pickBufferSize];
    IntBuffer selectionBuffer;

    ByteBuffer byteBuffer = ByteBuffer.allocateDirect(
        BufferUtil.SIZEOF_INT * pickBufferSize);
    byteBuffer.order(ByteOrder.nativeOrder());
    selectionBuffer = byteBuffer.asIntBuffer();


    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glPushMatrix();

    gl.glLoadIdentity();
    double pickbox = 4;
    glu.gluPickMatrix( (double) pickPos[0], (double) pickPos[1],
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
    draw1Dtf(drawable);
    gluvv.picking = 0;

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
        z = selectionBuffer.get(j++); //second is the z value for this hit
        if (nnames > 1 && z <= minz) {
          gotone = 1;
          minz = z;
          gluvv.pick.z = minz;

          j += nnames - 3;
          name32 = selectionBuffer.get(j++);
          data1 = selectionBuffer.get(j++);
          data2 = selectionBuffer.get(j++);
          data3 =selectionBuffer.get(j++);
          // #endif
          //cerr << "here: " << gluvv.pick.name32  << " " << gluvv.pick.data1
          //   << " " << gluvv.pick.data2
          //   << " " << gluvv.pick.data3 << endl;
        }
        else {
          j += nnames + 1;
        }
      }
    }

    selectionBuffer = null;

    if (gotone == 1) {
      return 1;
    }

    return 0;

  }

  private void updateColor() {
    float[] c0 = {
        0, 0, 0};
    float a0 = 0;
    float[] c1 = {
        0, 0, 0};
    float a1 = 0;
    int last = 0;
    TFPoint p;
    for (int i = 0; i < 256; ++i) {
      p = points.findPoint(i);
      if (p != null) {
        p.cpick.getColor(c1);
        a1=p.operator(1);
        gluvv.volren.tlut.alphaRamp(last, i, a0, a1);
        gluvv.volren.tlut.redRamp(last, i, c0[0], c1[0]);
        gluvv.volren.tlut.greenRamp(last, i, c0[1], c1[1]);
        gluvv.volren.tlut.blueRamp(last, i, c0[2], c1[2]);
        c0[0] = c1[0];
        c0[1] = c1[1];
        c0[2] = c1[2];
        a0 = a1;
        last = i;
      }
    }
    c1[0] = c1[1] = c1[2] = a1 = 0;
    gluvv.volren.tlut.alphaRamp(last, 255, a0, a1);
    gluvv.volren.tlut.redRamp(last, 255, c0[0], c1[0]);
    gluvv.volren.tlut.greenRamp(last, 255, c0[1], c1[1]);
    gluvv.volren.tlut.blueRamp(last, 255, c0[2], c1[2]);

  }

  private void updateVolume() {
    // glutSetWindow(gluvv.mainWindow);
    // glutPostRedisplay();
    // glutSetWindow(gluvv.tf.tfWindow);
  }

  // Methods required for the implementation of MouseListener
  public void mouseEntered(MouseEvent e) {}

  public void mouseExited(MouseEvent e) {}

  public void mouseMoved(MouseEvent e) {}

  public void mouseReleased(MouseEvent e) {
    picked = false;
    gluvv.volren.sampleRate = gluvv.volren.goodSamp;
    // glut.glutSetWindowTitle("Vanilla Transfer Function");
    updateVolume();

  }

  public void mouseClicked(MouseEvent e) {}

  public void mousePressed(MouseEvent e) {
    int x, y;
    int button = e.getButton();
    picked = true;
    x = e.getX();
    y = e.getY();
    pickPos[0] = x;
    pickPos[1] = height - y;
    butt = button;


    if ( (button == MouseEvent.BUTTON1) &&
        (data3 == ALPHA_MAP_NAME)) { //insert a newpoint
      //this still isn't quite right!
      float xp = (float) x / (float) width;
      float yp = ( (height - y) / (float) (height) - (1 - pntsHeight)) / pntsHeight;
      xp = xp <= 1 ? (xp >= 0 ? xp : 0) : 1;
      yp = yp <= 1 ? (yp >= 0 ? yp : 0) : 1;
      TFPoint p = new TFPoint(0, xp * 255, xp, yp);
      points.insert(p);
      // glutPostRedisplay();
      data3 = p.getBin(); //now make the new point the picked point
      data2 = POINTS_NAME;
    }

  }

  public void mouseDragged(MouseEvent e) {
    int x, y;
    int button = e.getButton();
    x = e.getX();
    y = e.getY();

    int idx = x - pickPos[0];
    int idy = (height - y) - pickPos[1];
    float fdx = idx / (float) width;
    float fdy = idy / (float) height;

    if (data2 == POINTS_NAME) {

      if (button == MouseEvent.BUTTON1) {
        TFPoint p = points.getPoint(data3);
        if (p == null) {
          return;
        }

        p.setPosU(p.operator(0) + (fdx * (exts[1][0] - exts[0][0])));
        p.setPosU(p.operator(0) < 1 ? (p.operator(0) >= 0 ? p.operator(0) : 0) : 1);
        p.setPosV(p.operator(1) + (fdy / pntsHeight * (exts[0][1] - exts[1][1])));
        p.setPosV(p.operator(1) < 1 ? (p.operator(1) >= 0 ? p.operator(1) : 0) : 1);
        p.setBin((int)math.affine(0, p.operator(0), 1, 0, 255));

        data3 = p.getBin();
        points.insert(p);
        updateColor();
        gluvv.volren.loadTLUT = 1;
        // glutPostRedisplay();
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        updateVolume();
      }
      else if (button == MouseEvent.BUTTON3) {
        TFPoint p;
        p = points.findPoint(data3);
        if (p == null) {
          return;
        }
        p.cpick.updateHL(fdx, fdy);
        p.setBin(data3);
        updateColor();
        gluvv.volren.loadTLUT = 1;
        // glutPostRedisplay();
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        updateVolume();
      }
      else if (button == MouseEvent.BUTTON2) {

      }
    }
    else if (data2 == -1) { //every thing else!
      if (data3 == HIQ_SAMP_NAME) {
        String title = new String();
        gluvv.volren.goodSamp += fdx * 6;
        title = "High Quality Sample Rate = " + gluvv.volren.goodSamp;
        // glut.glutSetWindowTitle(title);
        // glutPostRedisplay();
        updateVolume();
      }
      else if (data3 == LOQ_SAMP_NAME) {
        String title = new String();
        gluvv.volren.interactSamp += fdx * 6;
        title = "Low Quality Sample Rate = " + gluvv.volren.interactSamp;
        // glutSetWindowTitle(title);
        // glutPostRedisplay();
        updateVolume();
      }
    }

    pickPos[0] = x;
    pickPos[1] = (height - y);

  }

  public void keyPressed(KeyEvent e) {
    char key = e.getKeyChar();

    if (key == 27) { //escape key
      destroy();
    }
    switch (key) {
      case 127: //delete key
        if (data2 == POINTS_NAME) {
          TFPoint p;
          p = points.getPoint(data3);
          if (p != null) {
            // delete p;
            updateColor();
            // glutPostRedisplay();
            updateVolume();
            data2 = -1;
            data3 = -1;
          }
        }
        break;

      default:
        System.err.println("key = " + (char) key + " == (int)" + (int) key);
    }
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


}
