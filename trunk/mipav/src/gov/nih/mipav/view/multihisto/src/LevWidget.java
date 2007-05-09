package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.nio.*;
import java.awt.event.*;

public class LevWidget
    extends gluvvPrimitive implements MouseListener, MouseMotionListener {

  public static int LEVWIDGNAME = 22223222;

  public int on; //rasterize widget
  public int drawOn; //draw widget
  public int id = 0; //this widgets id/name
  public static int ids = 0;

  private int mode;
  private int type;

  private float depth;
  private float[][] verts = new float[3][2];
  private float[] thresh = new float[2];

  private float ballsz;
  private float barsz;
  private float width;
  private float height;

  private int pickedObj;
  private int picked;

  private HSLPickWidget cpick;
  private float[] color = new float[3];
  private float scatter;
  private float alpha;
  private float be;

  private LevWidget next;

  private int[] lastwinpos = new int[2];

  private static int objUnknown = 0;
  private static int bball = 1;
  private static int lball = 2;
  private static int rball = 3;
  private static int tbar = 4;
  private static int slider = 5;
  private static int trap = 6;

  private static int modeUnknown = 0;
  private static int LWcolorMode = 1;
  private static int LWscatterMode = 2;
  private static int LWalphaMode = 3;
  private static int LWclearMode = 4;

  private static int typeUknown = 0;
  private static int LWtriangle = 1; //levoy triangle
  private static int LWsquare = 2; //square, actualy ellipse
  private static int LW1d = 3; //1d tf style
  private static int LWdef = 4; //default tf style
  private static int LWcircle = 5;

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

  //============================================================ LevWidget
//======================================================================
  public LevWidget(gluvvGlobal _gluvv) {
    gluvv = _gluvv;

    cpick = new HSLPickWidget(gluvv);

    on = 1;
    drawOn = 1;
    next = null;
    widgetInit();

    id = ++ids;

    for (int i = 0; i < 3; ++i) {
      for (int j = 0; j < 2; ++j) {
        verts[i][j] = 0;
      }
    }

    pickedObj = -1;
    picked = 0;
    depth = 0;

    cpick.getColor(color);

    mode = LWclearMode;
    type = LWtriangle;

    alpha = .5f;

    scatter = .5f;

    be = 1;
  }

  public LevWidget(LevWidget lw) {

    cpick = lw.cpick;
    on = 1;
    drawOn = 1;
    next = null;
    widgetInit();
    id = ++ids;

    for (int i = 0; i < 3; ++i) {
      for (int j = 0; j < 2; ++j) {
        verts[i][j] = lw.verts[i][j];
      }
    }

    pickedObj = -1;
    picked = 0;
    depth = lw.depth;

    cpick.getColor(color);

    mode = lw.mode;
    type = lw.type;

    alpha = lw.alpha;
    be = lw.be;

    thresh[0] = lw.thresh[0];
    thresh[1] = lw.thresh[1];

  }

  //================================================================= draw
//======================================================================
  public void draw(GLAutoDrawable drawable) {
    if (on == 0) {
      if (next != null) {
        next.draw(drawable);
      }
      return;
    }

    if (gluvv.picking == 1) {
      if (next != null) {
        next.draw(drawable);
      }
    }

    if ( (gluvv.dmode == gluvvDataMode.GDM_V1) ||
        (gluvv.dmode == gluvvDataMode.GDM_VGH_V)) {
      verts[0][1] = 0;
      verts[1][1] = 1;
      verts[2][1] = 1;
      type = LW1d;
    }

    defaultMaterial(drawable);

    if (type == LWtriangle) {
      drawTriangle(drawable);
    }
    if ( (type == LWsquare) || (type == LWdef) || (type == LW1d)) {
      drawSquare(drawable);
    }

    if (gluvv.picking == 0) {
      if (next != null) {
        next.draw(drawable);
      }
    }
    LevWidgetGlErr(drawable, "draw");
  }

  //========================================================= Gl error chk
//======================================================================
  private int LevWidgetGlErr(GLAutoDrawable drawable, String place) {
    int errCode;
    GL gl = drawable.getGL();
    String errString = new String();

    if ( (errCode = gl.glGetError()) != GL.GL_NO_ERROR) {
      errString = glu.gluErrorString(errCode);
      System.out.println("OpenGL error : LevWidget::" + place + " : " +
                         errString);
      return 1;
    }
    return 0;
  }

  //======================================================== draw triangle
//======================================================================
  private void drawTriangle(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glDisable(GL.GL_BLEND);
    //glDisable(GL_TEXTURE_3D_EXT);
    gl.glEnable(GL.GL_DEPTH_TEST);

    float[] bpos = {
        verts[0][0] * width, verts[0][1] * height, 0};
    float[] lpos = {
        verts[1][0] * width, verts[1][1] * height, 0};
    float[] rpos = {
        verts[2][0] * width, verts[2][1] * height, 0};

    float tval = thresh[1] / verts[1][1];

    float[] tpos = {
         (verts[0][0] +
          (verts[2][0] - verts[0][0]) * tval) * width,
        (verts[0][1] +
         (verts[2][1] - verts[0][1]) * tval) * height,
        0};
    float tleft = (verts[0][0] + (verts[1][0] - verts[0][0]) * tval) * width;


    if (drawOn == 1) {
      gl.glColor4f(.1f, .07f, .57f, 1f); //balls
      LWdrawSphere(drawable, bpos, bball); //bottom
      LWdrawSphere(drawable, rpos, rball); //right

      if (picked == 1) {
        gl.glColor4f(.7f, .34f, .55f, 1f); //bars
      }
      else {
        gl.glColor4f(.09f, .34f, .55f, 1f); //bars
      }
      LWdrawBar(drawable, 90, WdgYAxis, (verts[2][0] - verts[1][0]) * width,
                lpos, tbar);
      float tmpb = ballsz;
      ballsz = barsz;
      LWdrawSphere(drawable, lpos, tbar);
      ballsz = tmpb;

      gl.glColor4f(.5f, .07f, .57f, 1f); //threshold color
      gl.glEnable(GL.GL_LIGHTING);
      tpos[0] += ballsz; //offset ball pos right by ball size
      LWdrawSphere(drawable, tpos, slider); //threshold ball
      tpos[0] -= ballsz;

      gl.glDisable(GL.GL_LIGHTING);
      gl.glPopName(); //messy names!!!
      gl.glPopName();
      gl.glLoadName(id);
      gl.glPushName(tbar); //same as top bar
      gl.glPushName(LEVWIDGNAME);
      gl.glBegin(GL.GL_LINES);
      { //threshold
        gl.glVertex3f(tpos[0], tpos[1], tpos[2]);
        gl.glVertex3f( (verts[0][0] + (verts[1][0] - verts[0][0]) * tval) *
                      width, tpos[1], tpos[2]);
      }
      gl.glEnd();

    } //if draw on

    if (drawOn == 1 || gluvv.picking == 1) {
      gl.glPopName(); //messy names!!!
      gl.glPopName();
      gl.glLoadName(id);
      gl.glPushName(trap);
      gl.glPushName(LEVWIDGNAME);
      gl.glEnable(GL.GL_BLEND);
      gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);

      if (mode == LWcolorMode) {
        gl.glColor4f(color[0], color[1], color[2], 1);
      }
      else {
        gl.glColor4f(1, 1, 1, 0);
      }
      gl.glBegin(GL.GL_QUADS);
      { //trapizoid
        gl.glVertex3f(tpos[0], tpos[1], tpos[2]);
        gl.glVertex3f(tleft, tpos[1], tpos[2]);
        gl.glVertex3f(lpos[0], lpos[1], lpos[2]);
        gl.glVertex3f(rpos[0], rpos[1], rpos[2]);

      }
      gl.glEnd();
      gl.glDisable(GL.GL_BLEND);
    }

    if (drawOn == 1) {
      gl.glColor4f(.09f, .34f, .55f, 1f); //bars
      gl.glBegin(GL.GL_LINE_STRIP);
      { //triangle boundry
        gl.glVertex3d(lpos[0], lpos[1], lpos[2]);
        gl.glVertex3d(bpos[0], bpos[1], bpos[2]);
        gl.glVertex3d(rpos[0], rpos[1], rpos[2]);
      }
      gl.glEnd();
    }

    gl.glEnable(GL.GL_LIGHTING);
  }

  //========================================================== draw square
//======================================================================
  private void drawSquare(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glDisable(GL.GL_BLEND);
    //glDisable(GL_TEXTURE_3D_EXT);
    gl.glEnable(GL.GL_DEPTH_TEST);

    float[] lpos = {
        verts[1][0] * width, verts[1][1] * height + barsz, 0};
    float[] rpos = {
        verts[2][0] * width, verts[2][1] * height + barsz, 0};
    float[] bpos = {
        verts[2][0] * width, verts[0][1] * height, 0};
    float[] tpos = {
        thresh[0] * width, thresh[1] * height, rpos[2]};

    if (drawOn == 1) {
      gl.glColor4f(.1f, .07f, .57f, 1f); //balls
      //rpos[1] += ballsz;
      LWdrawSphere(drawable, rpos, rball); //right
      //rpos[1] -= ballsz;
      LWdrawSphere(drawable, bpos, bball); //bottom

      if (picked == 1) {
        gl.glColor4f(.7f, .34f, .55f, 1f);
      }
      else {
        gl.glColor4f(.09f, .34f, .55f, 1f); //top bar
      }
      //lpos[1] += barsz;
      LWdrawBar(drawable, 90, WdgYAxis, (verts[2][0] - verts[1][0]) * width,
                lpos, tbar);
      float tmpb = ballsz;
      ballsz = barsz;
      LWdrawSphere(drawable, lpos, tbar);
      ballsz = tmpb;
      //lpos[1] -=barsz;

      gl.glColor4f(.5f, .07f, .57f, 1f); //threshold color
      gl.glEnable(GL.GL_LIGHTING);
      LWdrawSphere(drawable, tpos, slider); //threshold ball

      gl.glDisable(GL.GL_BLEND);
      gl.glDisable(GL.GL_LIGHTING);
      gl.glPopName(); //messy names!!!
      gl.glPopName();
      gl.glLoadName(id);
      gl.glPushName(tbar); //same as top bar
      gl.glPushName(LEVWIDGNAME);
      gl.glColor4f(.09f, .34f, .55f, 1f); //box
      gl.glBegin(GL.GL_LINE_STRIP);
      { //threshold
        gl.glVertex3f(rpos[0], rpos[1], rpos[2]);
        gl.glVertex3f(rpos[0], bpos[1], bpos[2]);
        gl.glVertex3f(lpos[0], bpos[1], bpos[2]);
        gl.glVertex3f(lpos[0], lpos[1], lpos[2]);
      }
      gl.glEnd();

    } //if draw on
    if (drawOn == 1 || gluvv.picking == 1) {
      gl.glPopName(); //messy names!!!
      gl.glPopName();
      gl.glLoadName(id);
      gl.glPushName(trap);
      gl.glPushName(LEVWIDGNAME);
      gl.glEnable(GL.GL_BLEND);
      gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);

      if (mode == LWcolorMode) {
        gl.glColor4f(color[0], color[1], color[2], 1);
      }
      else {
        gl.glColor4f(1, 1, 1, 0);
      }
      gl.glBegin(GL.GL_QUADS);
      { //trapizoid
        gl.glVertex3f(rpos[0], bpos[1], bpos[2]);
        gl.glVertex3f(lpos[0], bpos[1], bpos[2]);
        gl.glVertex3f(lpos[0], lpos[1], lpos[2]);
        gl.glVertex3f(rpos[0], rpos[1], rpos[2]);

      }
      gl.glEnd();
      gl.glDisable(GL.GL_BLEND);
    }

    gl.glEnable(GL.GL_LIGHTING);
  }

  //========================================================= LWdrawSphere
//======================================================================
  private void LWdrawSphere(GLAutoDrawable drawable, float[] pos, int name) {
    GL gl = drawable.getGL();
    gl.glPushMatrix();
    gl.glPopName();
    gl.glPopName();
    gl.glLoadName(id);
    gl.glPushName(name);
    gl.glPushName(LEVWIDGNAME);
    gl.glTranslatef(pos[0], pos[1], pos[2]);
    glu.gluSphere(qobj, (double) ballsz, (int) ballSlice, (int) ballSlice);
    gl.glPopMatrix();
  }

//============================================================ LWdrawBar
//======================================================================
  public void LWdrawBar(GLAutoDrawable drawable, float rot, int axis,
                        float len, float[] pos, int name) {
    GL gl = drawable.getGL();
    gl.glPushMatrix(); //bottom front bar
    gl.glPopName();
    gl.glPopName();
    gl.glLoadName(id);
    gl.glPushName(name);
    gl.glPushName(LEVWIDGNAME);
    gl.glTranslatef(pos[0], pos[1], pos[2]);

    if (axis == WdgXAxis) {
      gl.glRotatef(rot, 1f, 0f, 0f);
    }
    else if (axis == WdgYAxis) {
      gl.glRotatef(rot, 0f, 1f, 0f);
    }
    else {
      gl.glRotatef(rot, 0, 0, 1);
    }

    glu.gluCylinder(qobj, (double) barsz, (double) barsz, (double) len,
                    (int) barSlice, (int) barStack);
    gl.glPopMatrix();
  }

  //============================================================ LWdrawBar
//======================================================================
  public void LWdrawSlider(GLAutoDrawable drawable, float rot, int axis,
                           float[] pos, int name) {
    GL gl = drawable.getGL();
    gl.glPushMatrix();
    {
      gl.glPopName();
      gl.glPopName();
      gl.glLoadName(id);
      gl.glPushName(name);

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

      glu.gluCylinder(qobj, (double) (ballRad * 1.1), (double) (ballRad * 1.1),
                      (double) (ballRad * 1.1), (int) (barSlice), 2);
      glu.gluDisk(qobj, (double) 0, (double) (ballRad * 1.1), (int) (barSlice),
                  (int) 1);
      gl.glTranslatef(0f, 0f, (float) (ballRad * 1.1));
      glu.gluDisk(qobj, (double) 0, (double) ballRad * 1.1, (int) barSlice, 1);
    }
    gl.glPopMatrix();
  }

  //================================================================= pick
//======================================================================
  public int pick(int data1, int data2, int data3, float x, float y, float z) {
    if (data1 != id) {
      if (next != null) {
        return next.pick(data1, data2, data3, x, y, z);
      }
      return 0;
    }
    pickedObj = data2;
    picked = 1;
    //cerr << "picked levoy #" << id << endl;
    return 1;
  }

  public int pick() {
    if (gluvv.pick.data1 != id) {
      if (next != null) {
        return next.pick();
      }
      return 0;
    }
    pickedObj = gluvv.pick.data2;
    picked = 1;
    return 1;
  }

  //============================================================== release
//======================================================================
  public int release() {
    picked = 0;
    pickedObj = -1;
    if (next != null) {
      return next.release();
    }

    return 0;
  }

  //============================================================ rasterize
//======================================================================
  public void rasterize(byte[] tex, byte[] auxTex) {
    if (auxTex != null) {
      System.err.println("aux tex getting rasterized");
    }
    if ( (gluvv.dmode == gluvvDataMode.GDM_V1) ||
        (gluvv.dmode == gluvvDataMode.GDM_VGH_V)) {
      verts[0][1] = 0;
      verts[1][1] = 1;
      verts[2][1] = 1;
      type = LW1d;
    }

    if (on == 0) {
      if (next != null) {
        next.rasterize(tex, null);
      }
      return;
    }
    if (next != null) {
      next.rasterize(tex, null);
    }

    //for rasterizing triangles
    int sh = gluvv.tf.ptexsz[2]; //sizes along dimensions
    int sg = gluvv.tf.ptexsz[1];
    int sv = gluvv.tf.ptexsz[0];
    int H = (int) (verts[1][1] * sg) - 1; //[0..height-1]

    int sth = sg * sv * 4; //strides
    int stg = sv * 4;
    int stv = 4; //rgba

    int base = (int) (thresh[1] * sg);

    //--------------------- triangle -------------------------------------
    //--------------------------------------------------------------------
    if (type == LWtriangle) {
      for (int k = 0; k < sh; ++k) { //for each sheet of h
        float alphaScale;
        if (k != 1) {
          alphaScale = be;
        }
        else {
          alphaScale = 1;
        }
        for (int i = base; i < H + 1; ++i) { //for each scan line in sheet
          //start = upper left + proportion of distance to center v/s height
          // x = (y-b)/m
          int start = (int) ( (verts[0][0] + (i / (float) sg) *
                               (verts[1][0] - verts[0][0]) / verts[1][1]) * sv);
          int fin = (int) ( (verts[0][0] + (i / (float) sg) *
                             (verts[2][0] - verts[0][0]) / verts[1][1]) * sv) +
              1;

          fin -= start; //just the width please

          for (int j = 0; j < fin; ++j) { //for each pixel in line

            int offset = k * sth + i * stg + (start + j) * stv; //the kth sheet,
            //ith line,
            //jth pixel
            //NOTE:right here would change if blend operation changed
            float tmpa = (float) math.affine( -1, j, (fin), -1, 1); //rampval
            tmpa = tmpa < 0 ? (1.0f + tmpa) : (1.0f - tmpa);
            float cs = tmpa;
            tmpa *= alpha; //new alpha value
            if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa * cs) * color[0]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa * cs) * color[1]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa * cs) * color[2]) / (tmpta + tmpa) *
                                 255f).byteValue();
            }
            else {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa) * color[0]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa) * color[1]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa) * color[2]) / (tmpta + tmpa) * 255f).
                  byteValue();
            }
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset + 0] = new Integer(255).byteValue(); //((scatter * tmpa/(tmpa + tex[offset+3]/255.0)) +
              auxTex[offset + 1] = new Integer(255).byteValue(); //((scatter * tmpa/(tmpa + tex[offset+3]/255.0)) +
              auxTex[offset + 2] = new Integer(255).byteValue(); //((scatter * tmpa/(tmpa + tex[offset+3]/255.0)) +
              auxTex[offset + 3] = new Integer(255).byteValue(); //((scatter * tmpa/(tmpa + tex[offset+3]/255.0)) +
              //(auxTex[offset + 0]/255.0 * tex[offset+3]/(tmpa*255 + tex[offset + 3]))) *255;
            }
            tex[offset +
                3] = new Double(Math.max(tmpa * 255f, tex[offset + 3]) * (double) alphaScale).
                byteValue();
            //(tmpa*255 + (1.0-tmpa)*tex[offset+3])*alphaScale;
          }
        }
      }
    }
    //--------------------- ellipse -------------------------------------
    //--------------------------------------------------------------------
    else if ( (type == LWsquare)) { //elipse inside square
      //clean this shit up!!!!
      int W = (int) ( (verts[2][0] - verts[1][0]) * sv); //width
      int h = (int) ( (verts[0][1] - verts[1][1]) * sg); //real height
      int hc = (int) ( (thresh[0] - verts[1][0]) * sv); //horisontal center
      int VC = (int) ( (thresh[1]) * sg); //vertical center
      float maxd;
      float scaleh;
      float scalew;
      if (W * W < h * h) {
        maxd = (W / 2) * (W / 2);
        scalew = 1.0f;
        scaleh = (W / 2 * W / 2) / (float) (h / 2 * h / 2);
      }
      else {
        maxd = (h / 2) * (h / 2);
        scaleh = 1.0f;
        scalew = (h / 2 * h / 2) / (float) (W / 2 * W / 2);
      }

      for (int k = 0; k < sh; ++k) {
        float alphaScale;
        if (k != 1) {
          alphaScale = be;
        }
        else {
          alphaScale = 1;
        }
        for (int i = (int) (verts[0][1] * sg); i < (int) (thresh[1] * sg); ++i) {
          for (int j = 0; j < hc; ++j) {
            int offset = k * sth + i * stg +
                ( (int) (verts[1][0] * sv) + j) * stv;
            float d = ( (i - VC) * (i - VC) * scaleh) +
                ( (j - hc) * (j - hc) * scalew);
            float tmpa = (float) (math.affine(0, d, maxd, 1, 0));
            float cs = tmpa;
            tmpa = tmpa > 0 ? (tmpa < 1 ? tmpa * tmpa * alpha : alpha) : 0;
            if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset + 0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                            (tmpa * cs) * color[0]) /
                                          (tmpta + tmpa) * 255f).byteValue();
              tex[offset + 1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                            (tmpa * cs) * color[1]) /
                                          (tmpta + tmpa) * 255f).byteValue();
              tex[offset + 2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                            (tmpa * cs) * color[2]) /
                                          (tmpta + tmpa) * 255f).byteValue();
            }
            else {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa) * color[0]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa) * color[1]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa) * color[2]) / (tmpta + tmpa) * 255f).
                  byteValue();
            }
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset +
                  0] = new Float( ( (scatter * tmpa /
                                     (tmpa + tex[offset + 3] / 255.0f)) +
                                   (auxTex[offset + 0] / 255f * tex[offset +
                                    3] / (tmpa * 255f + tex[offset + 3]))) *
                                 255).byteValue();
            }
            tex[offset +
                3] = new Float( (tmpa * 255f + (1.0f - tmpa) * tex[offset + 3]) *
                               alphaScale).byteValue();
          }
          for (int j = hc; j < W; ++j) {
            int offset = k * sth + i * stg +
                ( (int) (verts[1][0] * sv) + j) * stv;
            float d = ( (i - VC) * (i - VC) * scaleh) +
                ( (j - hc) * (j - hc) * scalew);
            float tmpa = (float) (math.affine(0, d, maxd, 1, 0));
            float cs = tmpa;
            tmpa = tmpa > 0 ? (tmpa < 1 ? tmpa * tmpa * alpha : alpha) : 0;
            if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa * cs) * color[0]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa * cs) * color[1]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa * cs) * color[2]) / (tmpta + tmpa) *
                                 255f).byteValue();
            }
            else {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa) * color[0]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa) * color[1]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa) * color[2]) / (tmpta + tmpa) * 255f).
                  byteValue();
            }
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset +
                  0] = new Float( ( (scatter * tmpa /
                                     (tmpa + tex[offset + 3] / 255.0f)) +
                                   (auxTex[offset + 0] / 255.0f * tex[offset +
                                    3] / (tmpa * 255f + tex[offset + 3]))) *
                                 255f).byteValue();
            }
            tex[offset +
                3] = new Float( (tmpa * 255f + (1.0f - tmpa) * tex[offset + 3]) *
                               alphaScale).byteValue();
          }
        }
        int start = (int) (thresh[1] * sg);
        //if(i == start)
        //start += 1;
        for (int i = start; i < H + 1; ++i) {
          for (int j = 0; j < hc; ++j) {
            int offset = k * sth + i * stg +
                ( (int) (verts[1][0] * sv) + j) * stv;
            float d = ( (i - VC) * (i - VC) * scaleh) +
                ( (j - hc) * (j - hc) * scalew);
            float tmpa = (float) (math.affine(0, d, maxd, 1, 0));
            float cs = tmpa;
            tmpa = tmpa > 0 ? (tmpa < 1 ? tmpa * tmpa * alpha : alpha) : 0;
            if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa * cs) * color[0]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa * cs) * color[1]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa * cs) * color[2]) / (tmpta + tmpa) *
                                 255f).byteValue();
            }
            else {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa) * color[0]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa) * color[1]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa) * color[2]) / (tmpta + tmpa) * 255f).
                  byteValue();
            }
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset +
                  0] = new Float( ( (scatter * tmpa /
                                     (tmpa + tex[offset + 3] / 255.0f)) +
                                   (auxTex[offset + 0] / 255.0f * tex[offset +
                                    3] /
                                    (tmpa * 255f + tex[offset + 3]))) * 255f).
                  byteValue();
            }
            tex[offset +
                3] = new Float( (tmpa * 255f + (1.0f - tmpa) * tex[offset + 3]) *
                               alphaScale).byteValue();
          }
          for (int j = hc; j < W; ++j) {
            int offset = k * sth + i * stg +
                ( (int) (verts[1][0] * sv) + j) * stv;
            float d = ( (i - VC) * (i - VC) * scaleh) +
                ( (j - hc) * (j - hc) * scalew);
            float tmpa = (float) (math.affine(0, d, maxd, 1, 0));
            float cs = tmpa;
            tmpa = tmpa > 0 ? (tmpa < 1 ? tmpa * tmpa * alpha : alpha) : 0;
            if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa * cs) * color[0]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa * cs) * color[1]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa * cs) * color[2]) / (tmpta + tmpa) *
                                 255f).byteValue();
            }
            else {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0 +
                                   (tmpa) * color[0]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0 +
                                   (tmpa) * color[1]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0 +
                                   (tmpa) * color[2]) / (tmpta + tmpa) * 255f).
                  byteValue();
            }
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset +
                  0] = new Float( ( (scatter * tmpa /
                                     (tmpa + tex[offset + 3] / 255.0f)) +
                                   (auxTex[offset + 0] / 255f * tex[offset + 3] /
                                    (tmpa * 255f + tex[offset + 3]))) * 255f).
                  byteValue();
            }
            tex[offset +
                3] = new Float( (tmpa * 255 + (1.0f - tmpa) * tex[offset + 3]) *
                               alphaScale).byteValue();
          }
        }
      }
    }
    //--------------------- 1D -------------------------------------------
    //--------------------------------------------------------------------
    else if (type == LW1d) { //1D style tf
      int W = (int) ( (verts[2][0] - verts[1][0]) * sv); //width
      int h = (int) ( (verts[0][1] - verts[1][1]) * sg); //real height
      int hc = (int) ( (thresh[0] - verts[1][0]) * sv); //horisontal center
      float vthresh = (thresh[1] - verts[0][1]) / (verts[1][1] - verts[0][1]);

      // System.err.println("vthresh = " + vthresh + "   hthresh = " + hc);

      for (int k = 0; k < sh; ++k) { //for each sheet of h
        for (int i = (int) (verts[0][1] * sg); i < H + 1; ++i) { //for each scan line in sheet
          int start = (int) (verts[1][0] * sv);
          int dist = (int) (verts[2][0] * sv) - start;

          int hc0 = (int) (hc * (1.0 - vthresh)) + 1;

          for (int j = 0; j < hc0; ++j) { //for each pixel in line (first part)

            int offset = k * sth + i * stg + (start + j) * stv; //the kth sheet,
            //ith line,
            //jth pixel
            //NOTE:right here would change if blend operation changed
            float tmpa = (float) math.affine(0, j, (hc0), 0, 1); //rampval
            //tmpa = tmpa < 0 ? (1.0 + tmpa) : (1.0 - tmpa);
            float cs = tmpa;
            tmpa *= alpha; //new alpha value
            if (tmpa > 1) {
              System.err.println("bang" + tmpa); //debug check (removeme)
            }
            if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa * cs) * color[0]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa * cs) * color[1]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa * cs) * color[2]) / (tmpta + tmpa) *
                                 255f).byteValue();
            }
            else {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa) * color[0]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa) * color[1]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa) * color[2]) / (tmpta + tmpa) * 255f).
                  byteValue();
            }
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset +
                  0] = new Float( ( (scatter * tmpa /
                                     (tmpa + tex[offset + 3] / 255.0f)) +
                                   (auxTex[offset + 0] / 255.0f * tex[offset +
                                    3] / (tmpa * 255 + tex[offset + 3]))) *
                                 255f).byteValue();
            }
            tex[offset +
                3] = new Float( (tmpa * 255f + (1.0f - tmpa) * tex[offset + 3])).
                byteValue();
          }

          int hc1 = dist - (int) ( (dist - hc) * (1.0 - vthresh)) + 1;

          for (int j = hc0; j < hc1; ++j) { //for each pixel in line (middle part)

            int offset = k * sth + i * stg + (start + j) * stv; //the kth sheet,
            //ith line,
            //jth pixel
            //NOTE:right here would change if blend operation changed
            float tmpa = 1; //rampval
            //tmpa = tmpa < 0 ? (1.0 + tmpa) : (1.0 - tmpa);
            float cs = tmpa;
            tmpa *= alpha; //new alpha value
            if (tmpa > 1) {
              System.err.println("bang" + tmpa); //debug check (removeme)
            }
            if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa * cs) * color[0]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa * cs) * color[1]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa * cs) * color[2]) / (tmpta + tmpa) *
                                 255f).byteValue();
            }
            else {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa) * color[0]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa) * color[1]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa) * color[2]) / (tmpta + tmpa) * 255f).
                  byteValue();
            }
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset +
                  0] = new Float( ( (scatter * tmpa /
                                     (tmpa + tex[offset + 3] / 255.0f)) +
                                   (auxTex[offset + 0] / 255.0f * tex[offset +
                                    3] /
                                    (tmpa * 255f + tex[offset + 3]))) * 255f).
                  byteValue();
            }
            tex[offset +
                3] = new Float(tmpa * 255f + (1.0f - tmpa) * tex[offset + 3]).
                byteValue();
          }

          for (int j = hc1; j < dist; ++j) { //for each pixel in line (second part)

            int offset = k * sth + i * stg + (start + j) * stv; //the kth sheet,
            //ith line,
            //jth pixel
            //NOTE:right here would change if blend operation changed
            float tmpa = (float) math.affine(hc1, j, (dist), 1, 0); //rampval
            //tmpa = tmpa < 0 ? (1.0 + tmpa) : (1.0 - tmpa);
            float cs = tmpa;
            tmpa *= alpha; //new alpha value
            if (tmpa > 1) {
              System.err.println("bang" + tmpa); //debug check (removeme)
            }
            if (gluvv.shade == gluvvShade.gluvvShadeFaux) {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa * cs) * color[0]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa * cs) * color[1]) / (tmpta + tmpa) *
                                 255f).byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa * cs) * color[2]) / (tmpta + tmpa) *
                                 255f).byteValue();
            }
            else {
              float tmpta = tex[offset + 3] / 255.0f;
              tex[offset +
                  0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                   (tmpa) * color[0]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                   (tmpa) * color[1]) / (tmpta + tmpa) * 255f).
                  byteValue();
              tex[offset +
                  2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                   (tmpa) * color[2]) / (tmpta + tmpa) * 255f).
                  byteValue();
            }
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset +
                  0] = new Float( ( (scatter * tmpa /
                                     (tmpa + tex[offset + 3] / 255.0f)) +
                                   (auxTex[offset + 0] / 255.0f * tex[offset +
                                    3] /
                                    (tmpa * 255f + tex[offset + 3]))) * 255f).
                  byteValue();
            }
            tex[offset +
                3] = new Float(tmpa * 255f + (1.0f - tmpa) * tex[offset + 3]).
                byteValue();
          }
        }
      }

    }
    //--------------------- default --------------------------------------
    //--------------------------------------------------------------------
    else if (type == LWdef) {
      int W = (int) (verts[2][0] - verts[1][0]) * sv; //width
      int h = (int) (verts[0][1] - verts[1][1]) * sg; //real height
      int hc = (int) (thresh[0] - verts[1][0]) * sv; //horisontal center

      float m = (thresh[1] - verts[0][1]) / (verts[1][1] - verts[0][1]);

      for (int k = 0; k < sh; ++k) { //for each sheet of h
        float alphaScale;
        if (k != 1) {
          alphaScale = be;
        }
        else {
          alphaScale = 1;
        }

        int start = (int) (verts[1][0] * sv);
        int fin = (int) (verts[2][0] * sv);
        fin -= start; //just the width please

        float dc = 1 / ( (verts[1][0] - verts[2][0]) * sv - 1);

        for (int i = (int) (verts[0][1] * sg); i < H + 1; ++i) { //for each scan line in sheet
          float tmpa = ( ( (i / 255.0f) - verts[0][1]) /
                        (m + (i / 255.0f) - verts[0][1]));
          tmpa *= alpha; //new alpha value
          tmpa = tmpa > 1 ? 1 : (tmpa < 0 ? 0 : tmpa);
          float cs = tmpa;
          if (tmpa > 1) {
            System.err.println("bang" + tmpa); //debug check (removeme)
          }
          cpick.reset(0f, 1f, .5f); //reset the color

          for (int j = 0; j < fin; ++j) { //for each pixel in line
            float[] cl = new float[3];
            cpick.updateHL(dc, 0);
            cpick.getColor(cl);
            int offset = k * sth + i * stg + (start + j) * stv; //the kth sheet,
            //ith line,
            //jth pixel
            //NOTE:right here would change if blend operation changed
            float tmpta = tex[offset + 3] / 255.0f;
            tex[offset +
                0] = new Float( (tmpta * tex[offset + 0] / 255.0f +
                                 (tmpa) * cl[0]) / (tmpta + tmpa) * 255f).
                byteValue();
            tex[offset +
                1] = new Float( (tmpta * tex[offset + 1] / 255.0f +
                                 (tmpa) * cl[1]) / (tmpta + tmpa) * 255f).
                byteValue();
            tex[offset +
                2] = new Float( (tmpta * tex[offset + 2] / 255.0f +
                                 (tmpa) * cl[2]) / (tmpta + tmpa) * 255f).
                byteValue();
            if (auxTex != null) {
              //albedo, sOut = s1 * (a1/(a1+a2)) + s2 * (a2/(a1+a2))
              auxTex[offset +
                  0] = new Float( ( (scatter * tmpa /
                                     (tmpa + tex[offset + 3] / 255.0f)) +
                                   (auxTex[offset + 0] / 255.0f * tex[offset +
                                    3] /
                                    (tmpa + tex[offset + 3] / 255.0f))) * 255f).
                  byteValue();
            }
            tex[offset +
                3] = new Float( (tmpa * 255 + (1.0f - tmpa) * tex[offset + 3]) *
                               alphaScale).byteValue();
          }
        }
      }

    }
    //if(next) next->rasterize(tex);
  }

  //================================================== set ball & bar size
//======================================================================
  public void setBallBar(float ballsize, float barsize) {
    ballsz = ballsize;
    barsz = barsize;
    if (next != null) {
      next.setBallBar(ballsize, barsize);
    }
  }

//=================================================== set width & height
//======================================================================
  public void setWH(float width, float height) {
    this.width = width;
    this.height = height;
    if (next != null) {
      next.setWH(width, height);
    }
  }

  //  left +-----o right         left ------o right
  //       |  o  |  <--thresh         \    /
  //       +-----o bottom              \--o thresh
  //                                     o bottom

  //============================================================== set pos
//======================================================================
  public void setPos(float[] bottom, float[] left, float[] right, float tw,
                     float th) {
    verts[0][0] = bottom[0] > 0 ?
        (bottom[0] < 1 ? bottom[0] : 1) : 0;
    verts[0][1] = bottom[1] > 0 ?
        (bottom[1] < 1 ? bottom[1] : 1) : 0;
    verts[1][0] = left[0] > 0 ?
        (left[0] < 1 ? left[0] : 1) : 0;
    verts[1][1] = left[1] < 1 ?
        (left[1] > 0 ? left[1] : 0) : 1;
    verts[2][0] = right[0] > 0 ?
        (right[0] < 1 ? right[0] : 1) : 0;
    verts[2][1] = right[1] < 1 ?
        (right[1] > 0 ? right[1] : 0) : 1;
    if (th == -10) {
      thresh[1] = verts[0][1] + (verts[1][1] - verts[0][1]) / 2;
    }
    else {
      thresh[1] = th > verts[0][1] ?
          (th < verts[1][1] ? th : verts[1][1]) : verts[0][1];
    }
    if (tw == -10) {
      thresh[0] = verts[1][0] + (verts[2][0] - verts[1][0]) / 2;
    }
    else {
      thresh[0] = tw < verts[2][0] ?
          (tw > verts[1][0] ? tw : verts[1][0]) : verts[2][0];
    }
  }

  //============================================================ set depth
//======================================================================
  public void setDepth(float d) {
    depth = d;
    if (next != null) {
      next.setDepth(d);
    }
  }

  //==================================================== get boundary emph
//======================================================================
  public float getBE() {
    return be;
  }

//==================================================== set boundary emph
//======================================================================
  public void setBE(float boundEm) {
    be = boundEm > 0 ? (boundEm < 1 ? boundEm : 1) : 0;
  }

  //=============================================================== insert
//======================================================================
  public int insert(LevWidget n) {
    if (next != null) {
      n.next = next;
      next = n;
    }
    else {
      next = n;
    }
    return id;
  }

//================================================================== get
//======================================================================
  public LevWidget get(int ident) {
    if (id == ident) {
      return this;
    }
    else {
      if (next != null) {
        return next.get(ident);
      }
      else {
        return null;
      }
    }
  }

  //=============================================================== remove
//======================================================================
  public LevWidget remove(int ident) {
    if (next != null) {
      if (next.id == ident) {
        LevWidget tmp = next;
        next = next.next;
        return tmp;
      }
      else {
        return next.remove(ident);
      }
    }
    else {
      return null;
    }
  }

  //============================================================ set alpha
//======================================================================
  public void setAlpha(float a) {
    alpha = a;
  }

//============================================================ alphaMode
//======================================================================
  public void alphaMode() {
    mode = LWalphaMode;
    if (next != null) {
      next.alphaMode();
    }
  }

//============================================================ colorMode
//======================================================================
  public void colorMode() {
    mode = LWcolorMode;
    if (next != null) {
      next.colorMode();
    }
  }

//============================================================ clearMode
//======================================================================
  public void clearMode() {
    mode = LWclearMode;
    if (next != null) {
      next.clearMode();
    }
  }

//======================================================== triangle type
//======================================================================
  public void triangleType() {
    // System.err.println("triangle\n");
    type = LWtriangle;
  }

//========================================================== square type
//======================================================================
  public void squareType() {
    // System.err.println("square\n");
    type = LWsquare;
  }

//========================================================== square type
//======================================================================
  public void oneDType() {
    type = LW1d;
  }

  // Methods required for the implementation of MouseListener
  public void mouseEntered(MouseEvent e) {}

  public void mouseExited(MouseEvent e) {}

  public void mouseMoved(MouseEvent e) {}

  public void mouseReleased(MouseEvent e) {
    if (picked == 0) {
      if (next != null) {
        next.mouseReleased(e);
      }
    }
    gluvv.volren.sampleRate = gluvv.volren.goodSamp;
    mode = LWclearMode;
  }

  public void mouseClicked(MouseEvent e) {}

  public void mousePressed(MouseEvent e) {

    int x, y;
    // int button = e.getButton();
    x = e.getX();
    y = e.getY();

    if (picked == 0) {
      if (next != null) {
        next.mousePressed(e);
      }
    }

    lastwinpos[0] = x;
    lastwinpos[1] = y;

    if ( (gluvv.mouse.button == MouseEvent.BUTTON1) &&
        /* ((e.getModifiersEx() & MouseEvent.SHIFT_DOWN_MASK ) == 1  && */gluvv.
        mouse.shift == 1) { // ?????????????????????? modifier
      if (type == LWtriangle) {
        type = LWsquare;
        verts[0][1] = thresh[1];
        thresh[1] = verts[0][1] + (verts[2][1] - verts[0][1]) / 2;
        thresh[0] = verts[1][0] + (verts[2][0] - verts[1][0]) / 2;
        gluvv.tf.loadme = 1;
      }
      else if (type == LWsquare) {
        type = LWdef;
        gluvv.tf.loadme = 1;
      }
      else if (type == LWdef) {
        type = LW1d;
        gluvv.tf.loadme = 1;
      }
      else if (type == LW1d) {
        type = LWtriangle;
        verts[0][0] = verts[1][0] + (verts[2][0] - verts[1][0]) / 2.0f;
        thresh[1] = verts[0][1];
        verts[0][1] = 0;
        gluvv.tf.loadme = 1;
      }
    }

    if ( (gluvv.mouse.button == MouseEvent.BUTTON1) && (pickedObj == trap) &&
        (gluvv.mouse.ctrl == 0)) {
      mode = LWcolorMode;
    }
    if ( (gluvv.mouse.button == MouseEvent.BUTTON2) &&
        (pickedObj == trap) &&
        (gluvv.mouse.ctrl == 0)) {
      mode = LWscatterMode;
    }
    if ( (gluvv.mouse.button == MouseEvent.BUTTON3) &&
        (pickedObj == trap)) {
      mode = LWclearMode;
    }

  }

  public void mouseDragged(MouseEvent e) {
    int x, y;
    int button = e.getButton();
    x = e.getX();
    y = e.getY();

    if (picked == 0) {
      if (next != null) {
        next.mouseDragged(e);
      }
      return;
    }

    float sdx = (float) (x - lastwinpos[0]) / (float) gluvv.win.width;
    float sdy = (float) (lastwinpos[1] - y) / (float) gluvv.win.height;
    //had to change this, should be smarter
    float[] d = new float[3];
    //HACK AFTER HACK
    float[] pos = {
        0f, 0f, depth};
    math.subV3(d, pos, gluvv.env.eye);
    //float dist = normV3(d);
    float dist = pos[2] - gluvv.env.eye[2];
    float dx = sdx *
        (gluvv.env.frustum[1] - gluvv.env.frustum[0]) * dist / gluvv.env.clip[0];
    float dy = sdy *
        (gluvv.env.frustum[3] - gluvv.env.frustum[2]) * dist / gluvv.env.clip[0];
    //--------------------------------------------------------------------
    if ( (pickedObj == rball) && //right ball
        (gluvv.mouse.button == MouseEvent.BUTTON1)) { //down
      float tmpwr = verts[2][0]; //save width just in case
      float tmpwl = verts[1][0];
      verts[2][0] += dx / width; //move both balls
      verts[2][0] = verts[2][0] < 1 ? //in oposite dirs
          (verts[2][0] > verts[1][0] ? verts[2][0] : verts[1][0]) : 1;
      verts[1][0] -= dx / width;
      verts[1][0] = verts[1][0] > 0 ?
          (verts[1][0] < verts[2][0] ? verts[1][0] : verts[2][0]) : 0;
      if ( (type == LWsquare) || (type == LWdef) || (type == LW1d)) {
        float tmph = verts[2][1]; //save height to adjust thresh
        verts[2][1] += dy / height;
        verts[2][1] = verts[2][1] < 1 ?
            (verts[2][1] > thresh[1] ? verts[2][1] : thresh[1]) : 1;
        verts[1][1] = verts[2][1]; //move both balls verticaly
        thresh[1] = verts[0][1] + //move threshold proportionaly
            (thresh[1] - verts[0][1]) / (tmph - verts[0][1]) *
            (verts[2][1] - verts[0][1]);
        thresh[0] = verts[1][0] +
            (thresh[0] - tmpwl) / (tmpwr - tmpwl) * (verts[2][0] - verts[1][0]);
      }

      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      gluvv.tf.loadme = 1;
    } //-------------------------------------------------------------------
    else if ( (pickedObj == bball) && //bottom ball
             (gluvv.mouse.button == MouseEvent.BUTTON1)) { //down

      float tmpwr = verts[2][0]; //save width just in case
      float tmpwl = verts[1][0];
      verts[0][0] += dx / width; //update height
      verts[0][0] = verts[0][0] < 1.0 ? //clamp it
          (verts[0][0] > 0 ? verts[0][0] : 0) : 1.0f;
      verts[2][0] += dx / width;
      verts[2][0] = verts[2][0] < 1 ?
          (verts[2][0] > verts[1][0] ? verts[2][0] : verts[1][0]) : 1;

      if (type == LWtriangle) {

        verts[1][0] += dx / width; //move top bar horisontal
        verts[1][0] = verts[1][0] > 0 ? //clamp width
            (verts[1][0] < verts[2][0] ? verts[1][0] : verts[2][0]) : 0;
      }

      if ( (type == LWsquare) || (type == LWdef) || (type == LW1d)) {
        float tmp = verts[0][1];
        verts[0][1] += dy / height;
        verts[0][1] = verts[0][1] > 0 ?
            (verts[0][1] < thresh[1] ? verts[0][1] : thresh[1]) : 0;
        // t = bottom + (t-oldb)/(top - oldb) * (top - newb)
        thresh[1] = verts[0][1] +
            (thresh[1] - tmp) / (verts[1][1] - tmp) * (verts[1][1] - verts[0][1]);
        thresh[0] = verts[1][0] +
            (thresh[0] - tmpwl) / (tmpwr - tmpwl) * (verts[2][0] - verts[1][0]);
      }

      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      gluvv.tf.loadme = 1;
    } //----------------------------------------------------------------
    else if ( (pickedObj == tbar) && //topbar
             (gluvv.mouse.button == MouseEvent.BUTTON1)) { //left
      float w = (verts[2][0] - verts[1][0]) / 2.0f;
      float h = verts[2][1];

      verts[1][0] += dx / width; //move left ball
      verts[1][1] += dy / height;
      verts[1][1] = verts[1][1] < 1.0 ? //clamp left height
          (verts[1][1] > thresh[1] ? verts[1][1] : thresh[1]) : 1.0f;
      verts[2][0] += dx / width; //move right ball
      verts[2][1] += dy / height;
      verts[2][1] = verts[2][1] < 1.0 ? //clamp right height
          (verts[2][1] > thresh[1] ? verts[2][1] : thresh[1]) : 1.0f;

      if ( (type == LWsquare) || (type == LWdef) || (type == LW1d)) {
        verts[0][1] += dy / height;
        verts[0][1] = verts[0][1] > 0 ?
            (verts[0][1] < thresh[1] ? verts[0][1] : thresh[1]) : 0;
      }

      float W = verts[2][1] * (w / h); //scale width of bar

      if (type == LWtriangle) {
        verts[1][0] -= W - w;
        verts[2][0] += W - w;
      }

      verts[1][0] = verts[1][0] < 1.0 ? //clamp left width
          (verts[1][0] > 0 ? verts[1][0] : 0) : 1.0f;
      verts[2][0] = verts[2][0] < 1.0 ? //clamp right width
          (verts[2][0] > 0 ? verts[2][0] : 0) : 1.0f;
      //if square, thresh moves with bar
      if ( (type == LWsquare) || (type == LWdef) || (type == LW1d)) {
        thresh[1] += dy / height;
        thresh[1] = thresh[1] > 0 ? //clamp threshhold
            (thresh[1] < verts[2][1] ? thresh[1] : verts[2][1]) : 0;
        thresh[0] += dx / width;
        thresh[0] = thresh[0] > verts[1][0] ?
            (thresh[0] < verts[2][0] ? thresh[0] : verts[2][0]) : verts[1][0];
      }

      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      gluvv.tf.loadme = 1;
    } //-----------------------------------------------------------------
    else if ( (pickedObj == slider) && //thresh slider
             (gluvv.mouse.button == MouseEvent.BUTTON1)) { //down
      thresh[1] += dy / height;
      thresh[1] = thresh[1] < verts[1][1] ?
          (thresh[1] > verts[0][1] ? thresh[1] : verts[0][1]) : verts[1][1];
      thresh[0] += dx / width;
      thresh[0] = thresh[0] > verts[1][0] ?
          (thresh[0] < verts[2][0] ? thresh[0] : verts[2][0]) : verts[1][0];

      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      gluvv.tf.loadme = 1;
    } //------------------------------------------------------------------
    else if ( (pickedObj == trap) && //trapizoid
             (gluvv.mouse.button == MouseEvent.BUTTON1)) { //down
    //   System.err.println("trap button1");
    alpha += (lastwinpos[1] - y) / (float) gluvv.win.height;
   alpha = alpha > 1 ? 1 : (alpha < 0 ? 0 : alpha);


      cpick.updateHL( (x - lastwinpos[0]) / (float) gluvv.win.width,
                     (lastwinpos[1] - y) / (float) gluvv.win.height);
      cpick.getColor(color); //color
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      gluvv.tf.loadme = 1;

    } //-----------------------------------------------------------------
    else if ( (pickedObj == trap) && //trapizoid
             (gluvv.mouse.button == MouseEvent.BUTTON3) &&
             (gluvv.mouse.ctrl == 0)) { //NO ctrl
      alpha += (lastwinpos[1] - y) / (float) gluvv.win.height;
      alpha = alpha > 1 ? 1 : (alpha < 0 ? 0 : alpha);
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      gluvv.tf.loadme = 1;
    } //----------------------------------------------------------------
    else if ( (pickedObj == trap) && //trapizoid
             (gluvv.mouse.button == MouseEvent.BUTTON3) && //right
             (gluvv.mouse.ctrl == 1)) { //NO ctrl
 // System.err.println("trap button3 control");

      scatter += (lastwinpos[1] - y) / (float) gluvv.win.height;
      scatter = scatter > 1 ? 1 : (scatter < 0 ? 0 : scatter);
      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      gluvv.tf.loadme = 1;
    } //----------------------------------------------------------------
    lastwinpos[0] = x;
    lastwinpos[1] = y;
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

}
