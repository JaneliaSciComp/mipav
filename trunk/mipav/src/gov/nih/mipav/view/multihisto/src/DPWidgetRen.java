package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.nio.*;
import java.awt.event.*;

public class DPWidgetRen extends gluvvPrimitive
    implements KeyListener, MouseListener, MouseMotionListener
{
   private static int dpwrUnknown = 0;
   private static int mainbar = 1;
   private static int topball = 2;
   private static int slider1 = 3;
   private static int slider2 = 4;
   private static int point = 5;


   private float[] pos = new float[3];
   private float length;
   private float sliderpos;

   private Trackball tball;

   private int pickedObj;

   private int[] winpos = new int[2];
   private int[] lastwinpos = new int[2];

   private float pntfac;

   private float[] xform = new float[16];

   private VectorMath math = new VectorMath();

   private gluvvGlobal gluvv;

   private GLU glu = new GLU();

   //--- global widget display properties
   protected GLUquadric qobj;
   protected double barRad;
   protected double barSlice, barStack;
   protected double ballRad;
   protected double ballSlice;
   protected double coneRad; //NOTE: cone uses barSlice & barStacks rendering

   //---- axis enums for default glyph renderers ------------------------
   public static int WdgAxisUnknown = 0;
   public static int WdgXAxis = 1;
   public static int WdgYAxis = 2;
   public static int WdgZAxis = 3;
  public int WdgRotAxis = -1;

   public DPWidgetRen(gluvvGlobal _gluvv)
   {
     gluvv = _gluvv;
     tball = new Trackball();
     widgetInit();
     transparent();
     pos[0] = 0.0f;
     pos[1] = 0.0f;
     pos[2] = 0.0f;

     length = .5f;
     sliderpos = .8f;
           gluvv.probe.slider = sliderpos;

     pntfac = .1f;

     pickedObj = 0;

     math.identityMatrix(xform);

     if(gluvv.mv != null)
       update_pos();
   }

   public void draw(GLAutoDrawable drawable)
   {
     GL gl = drawable.getGL();
     if(gluvv.mv == null) return;
     if(gluvv.picking == 1) fill();

     gl.glEnable(GL.GL_LIGHTING);
     gl.glEnable(GL.GL_DEPTH_TEST);
     //glDisable(GL_TEXTURE_3D_EXT);

     if (gluvv.reblend == gluvvBlend.GB_NONE) {
       gl.glDisable(GL.GL_BLEND);
       gl.glDisable(GL.GL_LIGHTING);
     }
     else if (gluvv.reblend == gluvvBlend.GB_UNDER) {
       gl.glEnable(GL.GL_BLEND);
       gl.glBlendFunc(GL.GL_ONE_MINUS_DST_ALPHA, GL.GL_ONE);
       gl.glDepthFunc(GL.GL_EQUAL);
     }

     defaultMaterial(drawable);

     gl.glPushMatrix();{

     gluvvPushName(drawable);
     gl.glPushName(11);
     gl.glPushName(111);
     gl.glPushName(1111);

     move_pos();
     gl.glTranslatef(pos[0], pos[1], pos[2]);
     gl.glMultMatrixf(xform, 0);
     gl.glRotatef(-90f, 1f, 0f, 0f);
       //glDrawBuffer(GL_BACK);
      float[] spos = {0f,0f,(float)(length+length*pntfac)};

      if(gluvv.reblend == 0) {
         // gl.glColor4f(0f,0f,0f,0f);
         gl.glColor4f(.1f, .07f, .57f, 1f);
      } else {
         gl.glColor4f(.1f, .07f, .57f, 1f);                   //balls
      }
      drawSphere(drawable, spos, topball);
      float[] slpos = {0f, 0f, (float)(length*pntfac + sliderpos*(length - ballRad))};
      drawSlider(drawable, 0f, WdgZAxis, slpos, slider1);      //slider
      float[] bpos = {0f, 0f, length*pntfac};
      if(gluvv.reblend == 0) {
          // gl.glColor4f(0f,0f,0f,0f);
          gl.glColor4f(.09f, .34f, .55f, 1f);
      } else {
          gl.glColor4f(.09f, .34f, .55f, 1f);                  //bar
      }
      drawBar(drawable, 0f, WdgZAxis, length, bpos, mainbar);
      float[] cpos = {0f,0f,0f};
      drawCone(drawable, 0, WdgZAxis, length*pntfac, cpos, point);

       //glDrawBuffer(GL_FRONT);
       //glColor4f(.1, .07, .57, 1);                   //ball
       //drawSphere(spos, topball);
       //drawSlider(0, WdgZAxis, slpos, slider1);      //slider
       //glColor4f(.09, .34, .55, 1);                  //bars
       //drawBar(0, WdgZAxis, length, bpos, mainbar);
       //drawCone(0, WdgZAxis, length*pntfac, cpos, point);

       gluvvPopNames(drawable);
     }
     gl.glPopMatrix();


     gl.glDepthFunc(GL.GL_LESS);
     gl.glDisable(GL.GL_BLEND);
     DPWidgetRenGlErr(drawable, "draw");

     if(gluvv.picking == 1 )
       transparent();
   }

   //========================================================== pick
//===============================================================
   public int pick(int data1, int data2, int data3, float x, float y, float z)
   {
     fill();
     pickedObj = data3;
     update_pos();
     // glutSetWindowTitle("Data Probe Widget");
     gluvv.mprobe = 0;
     return 1;
   }

   public int pick()
   {
     fill();
     pickedObj = gluvv.pick.data3;
     update_pos();
     gluvv.mprobe = 0;
     return 1;
   }


  //======================================================= Release
//===============================================================
  public int release()
  {
    transparent();
    //cerr << "DPWidget released" << endl;
    return 0;
  }



//============================================== DPWidgetRenGlErr
//===============================================================
  int DPWidgetRenGlErr(GLAutoDrawable drawable, String place)
  {
    int errCode;
    GL gl = drawable.getGL();
    String errString = new String();

    if((errCode = gl.glGetError()) != GL.GL_NO_ERROR){
      errString = glu.gluErrorString(errCode);
      System.err.println("OpenGL error : DPWidgetRen::" + place + " : "  + errString);
      return 1;
    }
    return 0;
}


   private void update_pos()
   {
     //move the point tip to volume space
     float[]  m1 = new float[16];
     math.identityMatrix(m1);
     m1[12] = gluvv.rinfo.trans[0];
     m1[13] = gluvv.rinfo.trans[1];
     m1[14] = gluvv.rinfo.trans[2];
     float[] m2 = new float[16];
     math.matrixMult(m2, m1, gluvv.rinfo.xform);
     float[] scale = new float[16];
     math.identityMatrix(scale);
     scale[0] = gluvv.rinfo.scale;
     scale[5] = gluvv.rinfo.scale;
     scale[10]= gluvv.rinfo.scale;
     float[] m3 = new float[16];
     math.matrixMult(m3, m2, scale);
     float[] m4 = new float[16];
     math.identityMatrix(m4);
     m4[12] = -.5f * gluvv.mv.xfSize;
     m4[13] = -.5f * gluvv.mv.yfSize;
     m4[14] = -.5f * gluvv.mv.zfSize;
     float[] m5 = new float[16];
     math.matrixMult(m5, m3, m4);
     float[] m6 = {
         gluvv.mv.xfSize, 0, 0, 0,
         0, gluvv.mv.yfSize, 0, 0,
         0, 0, gluvv.mv.zfSize, 0,
         0, 0, 0, 1f};
     float[] volm = new float[16];
     math.matrixMult(volm, m5, m6);
     float[] invVolm = new float[16];
     math.inverseMatrix(invVolm, volm);

     float[] vpos = new float[3];
     math.translateV3W(vpos, invVolm, pos);
     gluvv.probe.vpos[0] = vpos[0];
     gluvv.probe.vpos[1] = vpos[1];
     gluvv.probe.vpos[2] = vpos[2];
     // System.err.println("pos = " + vpos[0] + " " + vpos[1] + " " + vpos[2]);

   }


   private void move_pos()
   {
     float[]  m1 = new float[16];
     // System.err.println("move_pos");
     math.identityMatrix(m1);
     m1[12] = gluvv.rinfo.trans[0];
     m1[13] = gluvv.rinfo.trans[1];
     m1[14] = gluvv.rinfo.trans[2];
     float[] m2 = new float[16];
     math.matrixMult(m2, m1, gluvv.rinfo.xform);
     float[] scale = new float[16];
     math.identityMatrix(scale);
     scale[0] = gluvv.rinfo.scale;
     scale[5] = gluvv.rinfo.scale;
     scale[10]= gluvv.rinfo.scale;
     float[] m3 = new float[16];
     math.matrixMult(m3, m2, scale);
     float[] m4 = new float[16];
     math.identityMatrix(m4);
     m4[12] = -.5f*gluvv.mv.xfSize;
     m4[13] = -.5f*gluvv.mv.yfSize;
     m4[14] = -.5f*gluvv.mv.zfSize;
     float[] m5 = new float[16];
     math.matrixMult(m5, m3, m4);
     float[] m6 = {
         gluvv.mv.xfSize, 0, 0, 0,
         0, gluvv.mv.yfSize, 0, 0,
         0, 0, gluvv.mv.zfSize, 0,
         0, 0, 0, 1};
     float[] volm = new float[16];
     math.matrixMult(volm, m5, m6);
     math.translateV3W(pos, volm, gluvv.probe.vpos);
     // System.err.println("pos = " + gluvv.probe.vpos[0] + " " + gluvv.probe.vpos[1] + " " + gluvv.probe.vpos[2]);


    }

    // Methods required for the implementation of MouseListener
    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e) {}

    public void mouseMoved(MouseEvent e) {}

    public void mouseReleased(MouseEvent e) {
      gluvv.volren.sampleRate = gluvv.volren.goodSamp;
      // glutSetWindowTitle("Data Probe Widget");
      // glutPostRedisplay();

    }

    public void mouseClicked(MouseEvent e) {}


    public void mousePressed(MouseEvent e) {
      int x, y;

      int button = e.getButton();
      x = e.getX();
      y = e.getY();
      winpos[0] = lastwinpos[0] = x;
      winpos[1] = lastwinpos[1] = y;
      if ( gluvv.mouse.button == MouseEvent.BUTTON1 ) {
        if ( pickedObj == topball ) {
            tball.start( (2.0f * x - gluvv.win.width) / gluvv.win.width,
                        (2.0f * y - gluvv.win.height) / gluvv.win.height);
         }
      } else if ( gluvv.mouse.button ==  MouseEvent.BUTTON2 ||
                  gluvv.mouse.button ==  MouseEvent.BUTTON3 ) {


      }

    }

    public void mouseDragged(MouseEvent e) {
      int x, y;
      int button = e.getButton();
      x = e.getX();
      y = e.getY();

      // System.err.println("DPWidgetRen::mouseDragged:  button = " + button);

      float dx = (float)(x - lastwinpos[0])/(float)gluvv.win.width;
      float dy = (float)(y - lastwinpos[1])/(float)gluvv.win.height;
      float[] d = new float[3];
      math.subV3(d, pos, gluvv.env.eye);
      float dist = math.normV3(d);
      dx *= (gluvv.env.frustum[1]-gluvv.env.frustum[0]) * dist/gluvv.env.clip[0];
      dy *= (gluvv.env.frustum[3]-gluvv.env.frustum[2]) * dist/gluvv.env.clip[0];

      if((gluvv.mouse.button == MouseEvent.BUTTON1) &&     //down
         (pickedObj == topball)){                        //topball
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        tball.update((2.0f * x - gluvv.win.width) /gluvv.win.width,
                     (2.0f * y - gluvv.win.height) /gluvv.win.height);
        tball.buildRotMatrix(xform);
        // glutPostRedisplay();
      }
      else if((gluvv.mouse.button == MouseEvent.BUTTON1) &&     //down
         (pickedObj == slider1)){                        //slider1
        sliderpos += ((float)(x - lastwinpos[0])/(float)gluvv.win.width -
                      (float)(y - lastwinpos[1])/(float)gluvv.win.height);
        sliderpos = (sliderpos > 1) ? 1 : ((sliderpos < 0) ? 0 : sliderpos);
                    gluvv.probe.slider = sliderpos;
                    gluvv.tf.loadme = 1;
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        // glutPostRedisplay();
      }
      else if((gluvv.mouse.button == MouseEvent.BUTTON1)){ //down
        pos[0] -= dx;                                    //any
        pos[1] -= dy;
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        // glutPostRedisplay();
      }
      else if((gluvv.mouse.button == MouseEvent.BUTTON2)){//down
        pos[0] -= dx;                                     //any
        pos[2] += dy;
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        // glutPostRedisplay();
      }
      else if((gluvv.mouse.button == MouseEvent.BUTTON3)){ //down
        pos[2] -= dx;                                     //any
        pos[1] -= dy;
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        // glutPostRedisplay();
      }

      update_pos();

      lastwinpos[0] = x;
      lastwinpos[1] = y;

    }

    public void keyPressed(KeyEvent e) {
      char key = e.getKeyChar();

      if(key == 32){ //space bar
        gluvv.tf.paintme = 1;
        // glutPostRedisplay();
        return; //key used
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

    public void widgetInit() {
      barRad = .015f;
      barSlice = 10;
      barStack = 2;
      ballRad = (float) (barRad * 1.50);
      ballSlice = 10;

      coneRad = barRad;

      qobj = glu.gluNewQuadric();
      glu.gluQuadricDrawStyle(qobj, GLU.GLU_FILL);
      glu.gluQuadricNormals(qobj, GLU.GLU_SMOOTH);
    }
    //-----------------------------------------------------------------------
      public void transparent()
      {
        glu.gluQuadricDrawStyle(qobj, GLU.GLU_SILHOUETTE);
        ballSlice = 5;
        barSlice = 3;
      }

      //-----------------------------------------------------------------------
      public void fill() {
        glu.gluQuadricDrawStyle(qobj, GLU.GLU_FILL);
        ballSlice = 10;
        barSlice = 10;
      }

      public void defaultMaterial(GLAutoDrawable drawable) {
        GL gl = drawable.getGL();
        float wamb[] = {
             (float) 0.3, (float) 0.3, (float) 0.3, (float) 1.0};
        float wspec[] = {
            1.0f, 1.0f, 1.0f, 1.0f};
        float wdiff[] = {
            0.5f, 0.5f, 0.5f, 1.0f};
        float wshine[] = {
            50.0f};

        gl.glEnable(GL.GL_COLOR_MATERIAL);
        FloatBuffer wambf = FloatBuffer.wrap(wamb);
        wambf.rewind();
        gl.glMaterialfv(GL.GL_FRONT, GL.GL_AMBIENT, wambf);
        FloatBuffer wdifff = FloatBuffer.wrap(wdiff);
        wdifff.rewind();
        gl.glMaterialfv(GL.GL_FRONT, GL.GL_DIFFUSE, wdifff);
        FloatBuffer wspecf = FloatBuffer.wrap(wspec);
        wspecf.rewind();
        gl.glMaterialfv(GL.GL_FRONT, GL.GL_SPECULAR, wspecf);
        FloatBuffer wshinef = FloatBuffer.wrap(wshine);
        wshinef.rewind();
        gl.glMaterialfv(GL.GL_FRONT, GL.GL_SHININESS, wshinef);

        gl.glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    }

    public void drawSphere(GLAutoDrawable drawable, float[] pos, int name) {
      GL gl = drawable.getGL();
      gl.glPushMatrix();
      gl.glLoadName(name);
      gl.glTranslatef(pos[0], pos[1], pos[2]);

      glu.gluSphere(qobj, ballRad, (int) ballSlice, (int) ballSlice);

      gl.glPopMatrix();
    }

    public void drawSlider(GLAutoDrawable drawable, float rot, int axis,
                           float[] pos, int name) {
      GL gl = drawable.getGL();
      gl.glPushMatrix();
      gl.glLoadName(name);
      gl.glTranslatef(pos[0], pos[1], pos[2]);

      if (axis == WdgXAxis) gl.glRotatef(rot, 1f, 0f, 0f);
      else if (axis == WdgYAxis) gl.glRotatef(rot, 0f, 1f, 0f);
      else gl.glRotatef(rot, 0f, 0f, 1f);

      glu.gluCylinder(qobj, ballRad * 1.1, ballRad * 1.1, ballRad * 1.1,
                      (int) barSlice, 2);
      glu.gluDisk(qobj, 0, ballRad * 1.1, (int) barSlice, 1);
      gl.glTranslatef( (float) 0, (float) 0, (float) (ballRad * 1.1));
      glu.gluDisk(qobj, 0, ballRad * 1.1, (int) barSlice, 1);

      gl.glPopMatrix();
  }

  public void drawBar(GLAutoDrawable drawable, float rot, int axis,
                      double len, float[] pos, int name) {
    GL gl = drawable.getGL();
    gl.glPushMatrix(); //bottom front bar
    gl.glLoadName(name);
    gl.glTranslatef(pos[0], pos[1], pos[2]);

    if (axis == WdgXAxis) gl.glRotatef(rot, 1.0f, 0.0f, 0.0f);
    else if (axis == WdgYAxis) gl.glRotatef(rot, 0.0f, 1.0f, 0.0f);
    else gl.glRotatef(rot, 0.0f, 0.0f, 1.0f);

    glu.gluCylinder(qobj, barRad, barRad, len, (int) barSlice, (int) barStack);

    gl.glPopMatrix();
   }

   public void drawCone(GLAutoDrawable drawable, float rot, int axis,
                        float len, float[] pos, int name) {
     GL gl = drawable.getGL();
     gl.glPushMatrix();
     gl.glLoadName(name);
     gl.glTranslatef(pos[0], pos[1], pos[2]);

     if (axis == WdgXAxis) gl.glRotatef(rot, 1f, 0f, 0f);
     else if (axis == WdgYAxis) gl.glRotatef(rot, 0f, 1f, 0f);
     else gl.glRotatef(rot, 0f, 0f, 1f);

     glu.gluCylinder(qobj, 0, coneRad, len, (int) barSlice, (int) barStack);

     gl.glPopMatrix();
   }



}
