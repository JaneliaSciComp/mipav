package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import javax.media.opengl.glu.GLU;
import java.nio.*;

public class Widget implements WidgetInterface {

  /** glu places, from original?????? */
  private GLU glu = new GLU();

  //---- Widget names --------------------------------------------------
// names to push during a pick, part one (1) above
  public static int I_AM_A_WIDGET = 0x98765432;
  public static int I_AM_A_WIDGET_GROUP = 0x98765111;
  public static int EMPTY_WIDGET_UINT = 0x12345678;

//---- Mouse State enums ---------------------------------------------
// mouse button enums for callbacks
  public static int WdgMouseUnknown = 0;
  public static int WdgM_BLeft = 1;
  public static int WdgM_BMiddle = 2;
  public static int WdgM_BRight = 3;
  public int WdgMouseButton = -1;

  public static int WdgM_SUnknown = 4;
  public static int WdgM_SUp = 5;
  public static int WdgM_SDown = 6;
  public int WdgMouseState = -1;

//---- axis enums for default glyph renderers ------------------------
  public static int WdgAxisUnknown = 0;
  public static int WdgXAxis = 1;
  public static int WdgYAxis = 2;
  public static int WdgZAxis = 3;
  public int WdgRotAxis = -1;

  public boolean busy;

  /* ---------- protected     ------------------------*/
  //--- global widget values -----------
  protected float[] position = new float[3];
  protected float[] orient = new float[16];

  //--- global widget display properties
  protected GLUquadric qobj;
  protected double barRad;
  protected double barSlice, barStack;
  protected double ballRad;
  protected double ballSlice;
  protected double coneRad; //NOTE: cone uses barSlice & barStacks rendering

  //--- environment variables ----------
  protected int winWidth;
  protected int winHeight;
  protected float[] xform = new float[16];
  protected float[] trans = new float[3];
  protected float scale;

  //--- global widget bounds -----------
  //assume axis aligned for now! for cliping to volumes
  protected float[] LLbound = new float[3]; //Lower Left Bound  [0,0,0]
  protected float[] URbound = new float[3]; //Upper right Bound [1,1,1]
  protected float[] maxBound = new float[3]; //global Upper Right bound
  protected float[] minBound = new float[3]; //global Lower Left bound

  //--- selection stuff ----------------
  protected int pickedObj;
  protected float[] lastPos = new float[3];
  protected int but; // WdgMouseButton
  protected int m_state; // WdgMouseState

  public Widget() {
     busy = false;
  }

  public void widgetInit() {
    barRad = (float) .015;
    barSlice = 10;
    barStack = 2;
    ballRad = (float) (barRad * 1.50);
    ballSlice = 10;

    coneRad = barRad;

    qobj = glu.gluNewQuadric();
    glu.gluQuadricDrawStyle(qobj, GLU.GLU_FILL);
    glu.gluQuadricNormals(qobj, GLU.GLU_SMOOTH);
  }

  public void widgetFree()
  {
     glu.gluDeleteQuadric(qobj);
  }

  public void draw()
  {
  /*I don't do anything, yet*/
  }

  //=======================================================================
//----- callbacks should return 0 when the object wants to be released---
//-----------------------------------------------------------------------
  public int pickcb(int data1, int data2, float x, float y, float z)
  {
    return 0;
  }

//-----------------------------------------------------------------------
  public int movecb(float x, float y, float z)
  {
    return 1;
  }

//-----------------------------------------------------------------------
  public int keycb(char key)
  {
    return 1;
  }

//-----------------------------------------------------------------------
  public int Xkeycb(int ks)
  {
    return 1;
  }

//-----------------------------------------------------------------------
  public int mousecb(int button,  int state, float x, float y, float z)
  {
    return 1;
  }

//-----------------------------------------------------------------------
  public void resize(int x, int y)
  {
    winWidth = x;
    winHeight = y;
  }

//-----------------------------------------------------------------------
  public int releasecb()
  {
    return 0;
  }

//-----------------------------------------------------------------------
  public void transparent()
  {
    glu.gluQuadricDrawStyle(qobj, GLU.GLU_SILHOUETTE);
    ballSlice = 5;
    barSlice = 3;
  }

//-----------------------------------------------------------------------
  public void fill()
  {
    glu.gluQuadricDrawStyle(qobj, GLU.GLU_FILL);
    ballSlice = 10;
    barSlice = 10;
  }

  /**
   *
   * @param x float
   * @param y float
   * @param z float
   */
  public void SetPos(float x, float y, float z) {
    position[0] = x;
    position[1] = y;
    position[2] = z;
  }

  /**
   *
   * @return float[]
   */
  public float[] GetPos() {
    return position;
  }

  /**
   * set the transform of the widget
   */
  public void SetXform(float[] Mat, float[] Trans, float Scale) {
    int i;
    for (i = 0; i < 16; ++i)
      xform[i] = Mat[i];
    for (i = 0; i < 3; ++i)
      trans[i] = Trans[i];
    scale = Scale;
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
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_AMBIENT, wambf);
    FloatBuffer wdifff = FloatBuffer.wrap(wdiff);
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_DIFFUSE, wdifff);
    FloatBuffer wspecf = FloatBuffer.wrap(wspec);
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_SPECULAR, wspecf);
    FloatBuffer wshinef = FloatBuffer.wrap(wshine);
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_SHININESS, wshinef);

    gl.glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
  }

  public void ident(float[] m) {
    m[0] = 1;
    m[4] = 0;
    m[8] = 0;
    m[12] = 0;
    m[1] = 0;
    m[5] = 1;
    m[9] = 0;
    m[13] = 0;
    m[2] = 0;
    m[6] = 0;
    m[10] = 1;
    m[14] = 0;
    m[3] = 0;
    m[7] = 0;
    m[11] = 0;
    m[15] = 1;
  }

  public void pushName(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glPushName(I_AM_A_WIDGET);
    // ?????
    gl.glPushName(this.hashCode());

  }

  public void popNames(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glPopName();
    gl.glPopName();
    gl.glPopName();
    gl.glPopName();
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

  public void drawSphere(GLAutoDrawable drawable, float[] pos, int name) {
    GL gl = drawable.getGL();
    gl.glPushMatrix();
    gl.glLoadName(name);
    gl.glTranslatef(pos[0], pos[1], pos[2]);

    glu.gluSphere(qobj, ballRad, (int) ballSlice, (int) ballSlice);

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

  public void setClips(GLAutoDrawable drawable, float[] m, float[] trans,
                       float scale) {
    //this will set clipping planes based on the upper right and lower left
    // bounds.  if the bounds of a sub regon are inside the max and min
    // bounds the geometry will be clipped, other wise not.  This is useful
    // for parallel compositing.  This widget may only belong to a small sub-
    // region, it should be clipped where it abutts other sub-regions
    GL gl = drawable.getGL();
    double[] yup = {
        0, 1, 0, 0};
    double[] ydn = {
        0, -1, 0, 0};
    double[] xup = {
        1, 0, 0, 0};
    double[] xdn = {
         -1, 0, 0, 0};
    double[] zup = {
        0, 0, 1, 0};
    double[] zdn = {
        0, 0, -1, 0};

    gl.glPushMatrix();
    {
      gl.glTranslatef(trans[0], trans[1], trans[2]);
      gl.glMultMatrixf(m, 0);
      gl.glScalef(scale, scale, scale);
      gl.glTranslatef( -.5f, -.5f, -.5f); //querk of the data representation

      if (LLbound[0] > minBound[0]) {
        gl.glEnable(GL.GL_CLIP_PLANE1);
        gl.glPushMatrix();

        gl.glTranslatef(LLbound[0], 0, 0);
        gl.glClipPlane(GL.GL_CLIP_PLANE1, xup, 0);

        gl.glPopMatrix();
      }

      if (URbound[0] < maxBound[0]) {
        gl.glEnable(GL.GL_CLIP_PLANE2);
        gl.glPushMatrix();

        gl.glTranslatef(URbound[0], 0, 0);
        gl.glClipPlane(GL.GL_CLIP_PLANE2, xdn, 0);

        gl.glPopMatrix();
      }

      if (LLbound[1] > minBound[1]) {
        gl.glEnable(GL.GL_CLIP_PLANE3);
        gl.glPushMatrix();

        gl.glTranslatef(0, LLbound[1], 0);
        gl.glClipPlane(GL.GL_CLIP_PLANE3, yup, 0);

        gl.glPopMatrix();
      }

      if (URbound[1] < maxBound[1]) {
        gl.glEnable(GL.GL_CLIP_PLANE4);
        gl.glPushMatrix();

        gl.glTranslatef(0, URbound[1], 0);
        gl.glClipPlane(GL.GL_CLIP_PLANE4, ydn, 0);

        gl.glPopMatrix();
      }

      if (LLbound[2] > minBound[2]) {
        gl.glEnable(GL.GL_CLIP_PLANE5);
        gl.glPushMatrix();

        gl.glTranslatef(0, 0, LLbound[2]);
        gl.glClipPlane(GL.GL_CLIP_PLANE5, zup, 0);

        gl.glPopMatrix();
      }

      if (URbound[2] < maxBound[2]) {
        gl.glEnable(GL.GL_CLIP_PLANE0);
        gl.glPushMatrix();

        gl.glTranslatef(0, 0, URbound[2]);
        gl.glClipPlane(GL.GL_CLIP_PLANE0, zdn, 0);

        gl.glPopMatrix();
      }

    }
    gl.glPopMatrix();
  }

  public void killClips(GLAutoDrawable drawable)
  {
     GL gl = drawable.getGL();
     gl.glDisable(GL.GL_CLIP_PLANE1);
     gl.glDisable(GL.GL_CLIP_PLANE2);
     gl.glDisable(GL.GL_CLIP_PLANE3);
     gl.glDisable(GL.GL_CLIP_PLANE4);
     gl.glDisable(GL.GL_CLIP_PLANE5);
     gl.glDisable(GL.GL_CLIP_PLANE0);
  }



}

