
package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.nio.*;
import java.awt.event.*;

public class CPWidgetRen
    extends gluvvPrimitive implements KeyListener, MouseListener,
    MouseMotionListener {

  private static int cpObjectUnknown = 0;
  private static int cpbar = 1;
  private static int cpball = 2;
  private static int alphaslide = 3;
  private static int plane = 4;

  private float[] pos = new float[3];
  private float[] vpos = new float[3];
  private float[] xform = new float[16];

  private float[] mousepnt = new float[3];

  private int[] lastwinpos = new int[3];
  private int pickedObj;

  private float width;
  private float screenwidth;
  private float height;
  private float screenheight;

  private float screenbar;
  private float screenball;

  private float aslider;

  private Trackball tball;

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

  //============================================================= CPWidgetRen
//=========================================================================
  // public CPWidgetRen(GLAutoDrawable drawable, gluvvGlobal _gluvv) {
  public CPWidgetRen(gluvvGlobal _gluvv) {
    // GL gl = drawable.getGL();

    gluvv = _gluvv;

    vpos[0] = .5f;
    vpos[1] = .5f;
    vpos[2] = .5f;

    pos[0] = 0;
    pos[1] = 0;
    pos[2] = 0;

    math.identityMatrix(xform);
    math.matrixEqual(gluvv.clip.xform, xform);

    screenwidth = .5f;
    screenheight = .5f;

    float[] d = new float[3];
    math.subV3(d, pos, gluvv.env.eye);
    float dist = math.normV3(d);
    width = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
             dist / gluvv.env.clip[0]) * screenwidth;
    height = ( (gluvv.env.frustum[3] - gluvv.env.frustum[2]) *
              dist / gluvv.env.clip[0]) * screenheight;

    screenbar = .01f;
    screenball = screenbar * 1.5f;

    //  gl.glEnable(GL.GL_DEPTH_TEST);                        ???????????????????????
    widgetInit();
    transparent();

    aslider = .3f;
    gluvv.clip.pname[0] = plane;

    if (gluvv.mv != null) {
      set_info();
    }
  }

  public void init(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    tball = new Trackball();
    gl.glEnable(GL.GL_DEPTH_TEST);
  }

  //==================================================================== draw
//=========================================================================
  public void draw(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    if (gluvv.mv == null) {
      return;
    }

    update_pos();
    set_info();

    float dist = pos[2] - gluvv.env.eye[2];

    width = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
             dist / gluvv.env.clip[0]) * screenwidth;
    height = ( (gluvv.env.frustum[3] - gluvv.env.frustum[2]) *
              dist / gluvv.env.clip[0]) * screenheight;

    barRad = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
              dist / gluvv.env.clip[0]) * screenbar;

    ballRad = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
               dist / gluvv.env.clip[0]) * screenball;

    //glDisable(GL_TEXTURE_3D_EXT);
    gl.glEnable(GL.GL_DEPTH_TEST);
    gl.glEnable(GL.GL_LIGHTING);

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
    gluvvPushName(drawable);
    gl.glPushName(22);
    gl.glPushName(222);
    gl.glPushName(2222);

    gl.glPushMatrix();
    gl.glTranslatef(pos[0], pos[1], pos[2]);
    gl.glMultMatrixf(xform, 0);
    gl.glTranslatef( -width / 2.0f, -height / 2.0f, 0f);

    //glEnable(GL_DEPTH_TEST);
    //glEnable(GL_LIGHTING);

    //glDrawBuffer(GL_BACK);
    draw_widget(drawable);

    //glDrawBuffer(GL_FRONT);
    //draw_widget();
    gl.glPopMatrix();

    if (pickedObj == plane) {
      gl.glPushMatrix();

      if (gluvv.reblend == 0) {
        gl.glColor4f(0, 0, 0, 0);
      }
      else {
        gl.glColor4f(0, 1f, 0, 1f);
      }
      ballRad = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) * dist /
                 gluvv.env.clip[0]) * screenball / 5f;
      drawSphere(drawable, mousepnt, plane); //pick point
      gl.glTranslatef(mousepnt[0], mousepnt[1], mousepnt[2]);
      gl.glColor4f(1f, 1f, 1f, 1f);
      gl.glBegin(GL.GL_LINES);
      {
        gl.glVertex3f( -10, 0, 0);
        gl.glVertex3f(10, 0, 0);
        gl.glVertex3f(0, -10, 0);
        gl.glVertex3f(0, 10, 0);
      }
      gl.glEnd();

      gl.glPopMatrix();
    }
    gluvvPopNames(drawable);

    if (gluvv.picking == 1) {
      transparent();
    }
  }

//==================================================================== pick
//=========================================================================
  public int pick(int data1, int data2, int data3, float x, float y, float z) {
    pickedObj = gluvv.pick.data3;
    fill();
    // glutSetWindowTitle("Clipping Plane Widget");
    return 1;
  }

  public int pick() {
    pickedObj = gluvv.pick.data3;
    // glutSetWindowTitle("Clipping Plane Widget");
    fill();
    return 1;
  }

//================================================================= release
//=========================================================================
  public int release() {
    transparent();
    pickedObj = -1;
    return 1;
  }

  //============================================================ GL Error Chk
//=========================================================================
  public int CPWidgetRenGlErr(GLAutoDrawable drawable, String place) {
    int errCode;
    GL gl = drawable.getGL();
    String errString;

    if ( (errCode = gl.glGetError()) != GL.GL_NO_ERROR) {
      errString = glu.gluErrorString(errCode);
      System.err.println("OpenGL error : CPWidgetRen::" + place + " : " +
                         errString);
      return 1;
    }
    return 0;
  }

  //============================================================= draw widget
//=========================================================================
  private void draw_widget(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    if (gluvv.clip.on == 0) {
      return;
    }

    if (gluvv.picking == 1) {
      fill();
      gl.glLoadName(plane);
      gl.glBegin(GL.GL_QUADS);
      gl.glVertex3f(0f, 0f, 0f);
      gl.glVertex3f(width, 0f, 0f);
      gl.glVertex3f(width, height, 0f);
      gl.glVertex3f(0f, height, 0f);
      gl.glEnd();
    }

    if (gluvv.reblend == 0) {
      // gl.glColor4f(0f, 0f, 0f, 0f);
      gl.glColor4f(.1f, .07f, .57f, 1f);
    }
    else {
      gl.glColor4f(.1f, .07f, .57f, 1f); //----balls
    }
    float[] spos = {0f, 0f, 0f};
    drawSphere(drawable, spos, cpball); //bottom left
    spos[0] = width;
    drawSphere(drawable, spos, cpball); //bottom right
    spos[1] = height;
    drawSphere(drawable, spos, cpball); //top right
    spos[0] = 0;
    drawSphere(drawable, spos, cpball); //top left
    // System.err.println("gluvv.reblend = " + gluvv.reblend);
    if (gluvv.reblend == 0) {
      // gl.glColor4f(0, 0, 0, 0);
      gl.glColor4f(.7f, .07f, .57f, 1f);
    }
    else {
      gl.glColor4f(.7f, .07f, .57f, 1f); //----slider
    }

    float[] slpos = {
        (float) (ballRad + (width - 2f * ballRad) * aslider), 0f, 0f};
    drawSlider(drawable, 90, WdgYAxis, slpos, alphaslide); //slider

    if (gluvv.reblend == 0) {
      // gl.glColor4f(0, 0, 0, 0);
      gl.glColor4f(.09f, .34f, .55f, 1f);
    }
    else {
      gl.glColor4f(.09f, .34f, .55f, 1f); //----bars
    }
    float[] bpos = {
        0f, 0f, 0f};
    drawBar(drawable, -90, WdgXAxis, height, bpos, cpbar); //left
    drawBar(drawable, 90, WdgYAxis, width, bpos, cpbar); //bottom
    bpos[1] = height;
    drawBar(drawable, 90, WdgYAxis, width, bpos, cpbar); //top
    bpos[0] = width;
    bpos[1] = 0;
    drawBar(drawable, -90, WdgXAxis, height, bpos, cpbar); //right

  }

//================================================================ set info
//=========================================================================
  private void set_info() {
    gluvv.clip.alpha = aslider;

    math.copyV3(gluvv.clip.pos, pos);
    math.matrixEqual(gluvv.clip.xform, xform);
    float[] dir = {
        0, 0, 1};
    math.translateV3(gluvv.clip.dir, xform, dir);

    //now put the corners in volume space
    float[] m1 = new float[16];
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
    scale[10] = gluvv.rinfo.scale;
    float[] m3 = new float[16];
    math.matrixMult(m3, m2, scale);
    float[] m4 = new float[16];
    math.identityMatrix(m4);
    m4[12] = -.5f * gluvv.mv.xfSize;
    m4[13] = -.5f * gluvv.mv.yfSize;
    m4[14] = -.5f * gluvv.mv.zfSize;
    float[] m5 = new float[16];
    math.matrixMult(m5, m3, m4);
    float[] m6 = new float[16];
    math.identityMatrix(m6);
    float[] volm = new float[16];
    math.matrixMult(volm, m5, m6);
    float[] invm = new float[16];
    math.inverseMatrix(invm, volm);

    float[] m7 = {
        1f / gluvv.mv.xfSize, 0, 0, 0,
        0, 1f / gluvv.mv.yfSize, 0, 0,
        0, 0, 1f / gluvv.mv.zfSize, 0,
        0, 0, 0, 1f};

    float[] invVolm = new float[16];
    math.matrixMult(invVolm, m7, invm);

    if (pickedObj == plane) {
      math.translateV3W(gluvv.clip.mousepnt, invVolm, mousepnt);
    }

    float[] cliptrans = new float[16];
    math.identityMatrix(cliptrans);
    cliptrans[12] = pos[0];
    cliptrans[13] = pos[1];
    cliptrans[14] = pos[2];
    float[] clip2wld = new float[16];
    math.matrixMult(clip2wld, cliptrans, xform);

    float[] m8 = {
        (float) gluvv.mv.xfSize, 0, 0, 0,
        0, (float) gluvv.mv.yfSize, 0, 0,
        0, 0, (float) gluvv.mv.zfSize, 0,
        0, 0, 0, 1f};

    float[] invVolm2 = new float[16];
    math.matrixMult(invVolm2, m8, invVolm);

    float[] clip2vol = new float[16];
    math.matrixMult(clip2vol, invVolm2, clip2wld);

    float[] v = {
         -width / 2, -height / 2, 0};
    math.translateV3W(gluvv.clip.corners[0], clip2vol, v);
    v[0] += width;
    math.translateV3W(gluvv.clip.corners[1], clip2vol, v);
    v[1] += height;
    math.translateV3W(gluvv.clip.corners[2], clip2vol, v);
    v[0] -= width;
    math.translateV3W(gluvv.clip.corners[3], clip2vol, v);

  }

//============================================================== update_pos
//=========================================================================
  private void update_pos() {
    if (gluvv.clip.ortho == 1) {
      float[] axis = new float[3];
      float[] xf = new float[16];
      math.identityMatrix(xf);

      if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXPos ||
          gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXNeg) {
        axis[0] = 0;
        axis[1] = 1;
        axis[2] = 0;
        math.axis2Rot(xf, axis, math.V_PI / 2.0f);
      }
      else if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYPos ||
               gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYNeg) {
        axis[0] = 1;
        axis[1] = 0;
        axis[2] = 0;
        math.axis2Rot(xf, axis, math.V_PI / 2.0f);
      }
      else if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZPos ||
               gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZNeg) {
      }

      //update orientation
      math.matrixMult(gluvv.clip.xform, gluvv.rinfo.xform, xf);
      math.matrixMult(xform, gluvv.rinfo.xform, xf);

      //update position
      float[] m1 = new float[16];
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
      scale[10] = gluvv.rinfo.scale;
      float[] m3 = new float[16];
      math.matrixMult(m3, m2, scale);
      float[] m4 = new float[16];
      math.identityMatrix(m4);
      m4[12] = -.5f * gluvv.mv.xfSize;
      m4[13] = -.5f * gluvv.mv.yfSize;
      m4[14] = -.5f * gluvv.mv.zfSize;
      float[] m5 = new float[16];
      math.matrixMult(m5, m3, m4);
//			float m6[16] = {gluvv.mv->xfSize, 0,              0,              0,
//				              0,              gluvv.mv->yfSize, 0,              0,
//				              0,              0,              gluvv.mv->zfSize, 0,
//				              0,              0,              0,              1};
      float[] volm = new float[16];
//			matrixMult(volm, m5, m6);
      math.matrixEqual(volm, m5);
      float[] tpos = new float[3];
      //cerr << "pos0  " << vpos[0] <<  " " << vpos[1] << " " << vpos[2] << endl;
      math.translateV3W(tpos, volm, vpos);
      math.copyV3(pos, tpos);
      math.copyV3(gluvv.clip.vpos, vpos);
      //cerr << "pos1  " << pos[0] <<  " " << pos[1] << " " << pos[2] << endl;

    }
  }

  //============================================================= mouse2plane
//=========================================================================
  private void mouse2plane(int x, int y) {
    // assume we are always aligned with z axis
    // should be based on up vector and (at-eye)xup vector!!!!!
    float fx = 1 - (float) x / (float) gluvv.win.width;
    float fy = (float) (gluvv.win.height - y) / (float) gluvv.win.height;
    float[] vdir = new float[3];
    math.subV3(vdir, gluvv.env.at, gluvv.env.eye);
    math.normalizeV3(vdir);
    math.scaleV3(gluvv.env.clip[0], vdir);
    float[] fcp = new float[3];
    math.addV3(fcp, gluvv.env.eye, vdir);
    float[] cpnt = new float[3];
    cpnt[0] = ( (fcp[0] + gluvv.env.frustum[0]) +
               (gluvv.env.frustum[1] - gluvv.env.frustum[0]) * fx);
    cpnt[1] = ( (fcp[1] + gluvv.env.frustum[2]) +
               (gluvv.env.frustum[3] - gluvv.env.frustum[2]) * fy);
    cpnt[2] = fcp[2];
    mousepnt[0] = cpnt[0];
    mousepnt[1] = cpnt[1];
    mousepnt[2] = cpnt[2];
    float[] ep = new float[3];
    math.subV3(ep, gluvv.env.eye, pos);
    float[] me = new float[3];
    math.subV3(me, gluvv.env.eye, cpnt);
    float t = math.dotV3(gluvv.clip.dir, ep) / math.dotV3(gluvv.clip.dir, me);
    math.negateV3(me);
    math.scaleV3(t, me);
    math.addV3(mousepnt, me, gluvv.env.eye);
    set_info();
  }

  // Methods required for the implementation of MouseListener
  public void mouseEntered(MouseEvent e) {}

  public void mouseExited(MouseEvent e) {}

  public void mouseMoved(MouseEvent e) {}

  public void mouseReleased(MouseEvent e) {
    gluvv.volren.sampleRate = gluvv.volren.goodSamp;
  }

  public void mouseClicked(MouseEvent e) {}

  public void mousePressed(MouseEvent e) {
    int x, y;
    x = e.getX();
    y = e.getY();

    lastwinpos[0] = e.getX();
    lastwinpos[1] = e.getY();
    int button = e.getButton();


    if (button == MouseEvent.BUTTON1) {
      if (pickedObj == cpball) {
        if (gluvv.clip.ortho == 1) {
          return;
        }
        tball.start( (2.0f * x - gluvv.win.width) / gluvv.win.width,
                    (2.0f * y - gluvv.win.height) / gluvv.win.height);
      }
      else if (pickedObj == cpbar) {
        // glutSetWindowTitle("Clipping Plane Widget: TRANSLATE {X,Y}");
      }
      else if (pickedObj == plane) {
        mouse2plane(x, y);
        gluvv.mprobe = 1;
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        // glutPostRedisplay();
        // glutSetCursor(GLUT_CURSOR_FULL_CROSSHAIR);
      }

    }
    else if (button == MouseEvent.BUTTON2) {
      // glutSetWindowTitle("Clipping Plane Widget: Scale");

    }
    else if (button == MouseEvent.BUTTON3) {
      if (gluvv.mouse.ctrl == 1) {
        gluvv.mouse.button = MouseEvent.BUTTON2;
        // glutSetWindowTitle("Clipping Plane Widget: Scale");
      }
    }

  }

  public void mouseDragged(MouseEvent e) {
    int x, y;
    x = e.getX();
    y = e.getY();

    float dx = (float) (x - lastwinpos[0]) / (float) gluvv.win.width;
    float dy = (float) (y - lastwinpos[1]) / (float) gluvv.win.height;
    float[] d = new float[3];
    math.subV3(d, pos, gluvv.env.eye);
    float dist = math.normV3(d);
    dx *= (gluvv.env.frustum[1] - gluvv.env.frustum[0]) * dist /
        gluvv.env.clip[0];
    dy *= (gluvv.env.frustum[3] - gluvv.env.frustum[2]) * dist /
        gluvv.env.clip[0];

    if (
        (gluvv.mouse.button == MouseEvent.BUTTON1) && //down
        (pickedObj == cpball)) { //ball
      if (gluvv.clip.ortho == 1) {
        return;
      }
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      tball.update( (2.0f * x - gluvv.win.width) / gluvv.win.width,
                   (2.0f * y - gluvv.win.height) / gluvv.win.height);
      tball.buildRotMatrix(xform);
      // glutPostRedisplay();

    }
    else if ( (gluvv.mouse.button == MouseEvent.BUTTON1) && //down
             (pickedObj == cpbar)) { //bar
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;

      if (gluvv.clip.ortho == 1) {
        float[] nv = new float[3];
        float[] ov = {
            dx, dy, 0};
        float[] imv = new float[16];
        math.inverseMatrix(imv, gluvv.rinfo.xform);
        math.translateV3(nv, imv, ov);

        if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXPos ||
            gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXNeg) {
          vpos[1] -= nv[1];
          vpos[2] -= nv[2];
        }
        else if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYPos ||
                 gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYNeg) {
          vpos[0] -= nv[0];
          vpos[2] -= nv[2];
        }
        else if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZPos ||
                 gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZNeg) {
          vpos[0] -= nv[0];
          vpos[1] -= nv[1];
        }

      }
      else {
        pos[0] -= dx;
        pos[1] -= dy;
      }
      // glutPostRedisplay();
    }
    else if ( (gluvv.mouse.button == MouseEvent.BUTTON3) && //down
             (pickedObj == cpbar)) { //bar
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;

      if (gluvv.clip.ortho == 1) {
        //float nv[3];
        //float ov[3] = {dx, dy, 0};
        //float imv[16];
        //inverseMatrix(imv, gluvv.rinfo.xform);
        //translateV3(nv, imv, ov);

        if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXPos ||
            gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXNeg) {
          vpos[0] += dy;
        }
        else if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYPos ||
                 gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYNeg) {
          vpos[1] += dy;
        }
        else if (gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZPos ||
                 gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZNeg) {
          vpos[2] += dy;
        }

      }
      else {
        pos[2] += dy;
      }
      // glutPostRedisplay();
    }
    else if ( (gluvv.mouse.button == MouseEvent.BUTTON2) && //down
             (pickedObj == cpball)) { //ball
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      screenwidth -= dy;
      screenheight -= dy;
      // glutPostRedisplay();
    }
    else if ( (gluvv.mouse.button == MouseEvent.BUTTON1) && //down
             (pickedObj == alphaslide)) { //alpha slider
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      aslider -= dy / width;
      aslider = aslider < 0 ? 0 : (aslider > 1 ? 1 : aslider);
      // glutPostRedisplay();
      String str = new String();
      // sprintf(str, "Clipping Plane Widget: ALPHA = %f", aslider);
      str = "Clipping Plane Widget: ALPHA = " + aslider;
      // glutSetWindowTitle(str);

    }
    else if ( (gluvv.mouse.button == MouseEvent.BUTTON1) && //down
             (pickedObj == plane)) { //plane
      mouse2plane(x, y);
      set_info();
      String str = new String();
      str = "Clipping Plane Widget: Volume Position " +
          gluvv.clip.mousepnt[0] + " " + gluvv.clip.mousepnt[1] + " " +
          gluvv.clip.mousepnt[2];
      // glutSetWindowTitle(str);
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      // glutPostRedisplay();
    }

    lastwinpos[0] = x;
    lastwinpos[1] = y;

  }

  /**
   * keyTyped.
   *
   * Invoked when a key has been typed. See the class description for
   * KeyEvent for a definition of a key typed event.
   */
  public void keyTyped(KeyEvent e) {}

  /**
   * keyPressed.
   *
   * Invoked when a key has been pressed. See the class description for
   * KeyEvent for a definition of a key pressed event.
   */
  public void keyPressed(KeyEvent e) {
    char key = e.getKeyChar();
    if (key == 32) { //space bar
      gluvv.tf.paintme = 1;
      // glutPostRedisplay();
      return; //key used
    }
    switch (key) {
      case '1':
        gluvv.clip.oaxis = VolRenMajorAxis.VolRenAxisXPos;

        // glutPostRedisplay();
        return;
      case '2':
        gluvv.clip.oaxis = VolRenMajorAxis.VolRenAxisXNeg;

        // glutPostRedisplay();
        return;
      case '3':
        gluvv.clip.oaxis = VolRenMajorAxis.VolRenAxisYPos;

        // glutPostRedisplay();
        return;
      case '4':
        gluvv.clip.oaxis = VolRenMajorAxis.VolRenAxisYNeg;

        // glutPostRedisplay();
        return;
      case '5':
        gluvv.clip.oaxis = VolRenMajorAxis.VolRenAxisZPos;

        // glutPostRedisplay();
        return;
      case '6':
        gluvv.clip.oaxis = VolRenMajorAxis.VolRenAxisZNeg;

        // glutPostRedisplay();
        return;
        /*
           #if 0
                        case 'o':
                        case 'O':
         if((gluvv.plat != GPNV20) && (gluvv.plat != GPNV202D)){
         if(gluvv.clip.ortho) gluvv.clip.ortho = 0;
                                        else gluvv.clip.ortho = 1;
                                        glutPostRedisplay();
                                }
                                return 1;
           #endif
         */
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

  // **********************  Widget **************************************
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

  public void transparent() {
    glu.gluQuadricDrawStyle(qobj, GLU.GLU_SILHOUETTE);
    ballSlice = 5;
    barSlice = 3;
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

    if (axis == WdgXAxis) {
      gl.glRotatef(rot, 1f, 0f, 0f);
    }
    else if (axis == WdgYAxis) {
      gl.glRotatef(rot, 0f, 1f, 0f);
    }
    else {
      gl.glRotatef(rot, 0f, 0f, 1f);
    }

    glu.gluCylinder(qobj, ballRad * 1.1, ballRad * 1.1, ballRad * 1.1,
                    (int) barSlice, 2);
    glu.gluDisk(qobj, 0, ballRad * 1.1, (int) barSlice, 1);
    gl.glTranslatef( (float) 0, (float) 0, (float) (ballRad * 1.1));
    glu.gluDisk(qobj, 0, ballRad * 1.1, (int) barSlice, 1);

    gl.glPopMatrix();
  }

  //-----------------------------------------------------------------------
  public void fill() {
    glu.gluQuadricDrawStyle(qobj, GLU.GLU_FILL);
    ballSlice = 10;
    barSlice = 10;
  }

  public void drawBar(GLAutoDrawable drawable, float rot, int axis,
                      double len, float[] pos, int name) {
    GL gl = drawable.getGL();
    gl.glPushMatrix(); //bottom front bar
    gl.glLoadName(name);
    gl.glTranslatef(pos[0], pos[1], pos[2]);

    if (axis == WdgXAxis) {
      gl.glRotatef(rot, 1.0f, 0.0f, 0.0f);
    }
    else if (axis == WdgYAxis) {
      gl.glRotatef(rot, 0.0f, 1.0f, 0.0f);
    }
    else {
      gl.glRotatef(rot, 0.0f, 0.0f, 1.0f);
    }

    glu.gluCylinder(qobj, barRad, barRad, len, (int) barSlice, (int) barStack);

    gl.glPopMatrix();
  }

}
