
package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;
import java.nio.*;
import java.awt.event.*;

public class TFWidgetRen extends gluvvPrimitive
    implements KeyListener, MouseListener, MouseMotionListener {

  public static int TFobjUnknown = 0;
  public static int tlball = 1;      //top left ball
  public static int trball = 2;      //top right ball
  public static int blball = 3;      //bottom left ball
  public static int brball = 4;      //bottom right ball
  public static int lbar = 5;        //left bar
  public static int rbar = 6;        //right bar
  public static int tbar = 7;        //top bar
  public static int bbar = 8;        //bottom bar
  public static int tfsheet0 = 9;    //transfer function sheet 0
  public static int tfsheet1 = 10;
  public static int tfsheet2 = 11;
  public static int dpoint = 12;       //data point from pick (probe or clip click)
  public static int gslide = 13;       //good sample rate slider
  public static int islide = 14;       //interactive sample rate slider
  public static int aslide = 15;       //alpha scale slider (gamma)
  public static int a3slide = 16;      //3rd axis emphasis slider
  public static int a3slidehi = 17;    //ditto
  public static int a3slidelo = 18;    //ditto
  public static int beslider  = 19;

  private float width, height;
  private float screenwidth, screenheight;
  private float[] screenpos = new float[2];
  private float screenbar, screenball;
  private float[]   mousepnt = new float[3];  // position of mouse click on tf plane
  private float[]   lastppos = new float[3];  // last probe position in volume

  private int[]   lastwinpos = new int[2];

  private float[] lastSlide1Pos = new float[2]; //one for hi and lo

  private LevWidget tris;      // levoy widgets
  private LevWidget shadTris;  // levoy widget for a shadow transfer function
  private int        pickedTri; //
  private LevWidget brush;     // levoy square brush
  private float lwbs;           // levoy widget ball size

  private byte[] paintex; //the painted transfer function

  private byte[] histo;        //histogram of the volume
  private int[]   histName = new int[1];     //texture name of the histogram

  //---- axis enums for default glyph renderers ------------------------
  public static int WdgAxisUnknown = 0;
  public static int WdgXAxis = 1;
  public static int WdgYAxis = 2;
  public static int WdgZAxis = 3;
  public int WdgRotAxis = -1;

 private int pickedObj;

  /*
  public byte[] don = new byte[69]; // "don greenburg" color bind color map 23*(rgb)
                                // the don[0] value is black, for the background
                              // don[1]=blue, don[11,12]=white, don[22]=orange
  */
  private VectorMath math = new VectorMath();

  private gluvvGlobal gluvv;

  private GLU glu = new GLU();
  private GLUT glut = new GLUT();


  //--- global widget display properties
  protected GLUquadric qobj;
  protected double barRad;
  protected double barSlice, barStack;
  protected double ballRad;
  protected double ballSlice;
  protected double coneRad; //NOTE: cone uses barSlice & barStacks rendering


  /* ---------- protected     ------------------------*/
  //--- global widget values -----------
  protected float[] position = new float[3];
  protected float[] orient = new float[16];



  //==========================================================================
//============= *** static color map definitions *** =======================
//==========================================================================

//==========================================================================
// the "don greenburg" color bind color map, outright stolen from GLK
//   from: <~gk/usr/local/src/bane/test/pvg.c>
  public  int[]   don = {0,   0,   0,     /* background: black */
                             /* 1 */ 0,   107, 255,   /* start: blue */
                                           51,  104, 255,
                                           103, 117, 255,
                                           123, 124, 255,
                                           141, 130, 255,
                                           156, 132, 255,
                                           166, 131, 245,
                                           174, 131, 231,
                                           181, 130, 216,
                                           187, 130, 201,
                            /* 11 */ 255, 255, 255,       /* middle: white */
                            /* 12 */ 255, 255, 255,
                                           187, 130, 201,
                                           192, 129, 186,
                                           197, 128, 172,
                                           200, 128, 158,
                                           204, 127, 142,
                                           210, 126, 113,
                                           212, 126, 98,
                                           213, 126, 84,
                                           216, 126, 49,
                            /* 22 */ 220, 133, 0};  /* end: orange */
//===== end "don greenburg" color map ======================================
//==========================================================================


  public TFWidgetRen(gluvvGlobal _gluvv) {
    gluvv = _gluvv;

    ballRad = .1f;
    barRad = .05f;
    position[0] = -1.3f;
    position[1] = -1.3f;
    position[2] = -1f;

    screenwidth = .9f;
    screenheight = .2f;
    screenpos[0] = .45f;
    screenpos[1] = -.46f;

    screenbar = .01f;
    screenball = screenbar * 1.5f;

    //gluvv.tf.ptexsz[0] = 256;
    //gluvv.tf.ptexsz[1] = 256;
    //gluvv.tf.ptexsz[2] = 1;
    //gluvv.tf.numelts   = 4;

    gluvv.tf.loadme = 0;

    lastppos[0] = lastppos[1] = lastppos[2] = 0;
    lastSlide1Pos[0] = 1;
    lastSlide1Pos[1] = 0;

    histo = null;
  }

  //================================================================= draw
//======================================================================
  public void draw(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    if (gluvv.reblend == 1)return;
    if (gluvv.mv == null)return;

    gl.glEnable(GL.GL_LIGHTING);

    float dist = position[2] - gluvv.env.eye[2];

    width = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
             dist / gluvv.env.clip[0]) * screenwidth;
    height = ( (gluvv.env.frustum[3] - gluvv.env.frustum[2]) *
              dist / gluvv.env.clip[0]) * screenheight;

    position[0] = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
                   dist / gluvv.env.clip[0]) * screenpos[0];
    position[1] = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
                   dist / gluvv.env.clip[0]) * screenpos[1];

    barRad = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
              dist / gluvv.env.clip[0]) * screenbar;

    ballRad = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
               dist / gluvv.env.clip[0]) * screenball;

    tris.setWH((float)(width - ballRad * 2f), (float)(height - ballRad * 2f));
    brush.setWH((float)(width - ballRad * 2f), (float)(height - ballRad * 2f));
    tris.setBallBar((float)(ballRad / 2.0f), (float)(barRad / 2.0f));
    brush.setBallBar((float)(ballRad / 2.0f), (float)(barRad / 2.0f));

    fill();
    gluvvPushName(drawable);
    gl.glPushName(22);
    gl.glPushName(222);
    gl.glPushName(2222);

    gl.glPushMatrix();
    {
      gl.glTranslatef(position[0], position[1], position[2]);
      gl.glRotatef(180f, 0f, 1f, 0f);
      drawFrame(drawable);
    }
    gl.glPopMatrix();

    gluvvPopNames(drawable);

    GlErr(drawable, "TFWidgetRen draw()");
  }

  //============================================== DPWidgetRenGlErr
//===============================================================
    private int GlErr(GLAutoDrawable drawable, String place)
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

    //=========================================================== draw frame
//======================================================================
    public void  drawFrame(GLAutoDrawable drawable)
    {
      GL gl = drawable.getGL();
      gl.glColor4f(.1f, .07f, .57f, 1f);                  //balls
      float[] spos = {0,0,0};                       // spos[3]
      drawSphere(drawable, spos, blball);                    //bottom left
      spos[0] = width;
      drawSphere(drawable, spos, brball);                    //bottom right
      spos[1] = height;
      drawSphere(drawable, spos, trball);                    //top    right
      spos[0] = 0;
      drawSphere(drawable, spos, tlball);                    //top    left

      gl.glColor4f(.09f, .34f, .55f, 1f);                 //bars
      float[] bpos = {0,0,0};                      // bpos[3]
      drawBar(drawable, -90, WdgXAxis, height, bpos, lbar);  //left
      drawBar(drawable, 90, WdgYAxis, width, bpos, bbar);    //bottom
      bpos[1] = height;
      drawBar(drawable, 90, WdgYAxis, width, bpos, tbar);    //top
      bpos[0] = width;
      bpos[1] = 0;
      drawBar(drawable, -90, WdgXAxis, height, bpos, rbar);  //right

      gl.glColor3f(.8f, .5f, .1f);                       //sliders
      //interactive sample rate
      float[] ispos = {(float)(ballRad) + (float)(gluvv.volren.interactSamp/6.0f)*(float)(width-2*ballRad), 0,0};
      drawSlider(drawable, 90,WdgYAxis,ispos,islide);
      gl.glColor3f(.8f, .2f, .1f);
      //good sample rate
      float[] gspos = {(float)(ballRad) + (float)(gluvv.volren.goodSamp/6.0f)*(float)(width-2*ballRad), 0,0};
      drawSlider(drawable, 90,WdgYAxis,gspos,gslide);
      gl.glColor3f(.6f, .2f, .8f);
      //alpha gamma slider
      float[] aspos = {0, (float)(ballRad + (gluvv.volren.gamma/10.0f)*(height-2*ballRad)), 0};
      drawSlider(drawable, 90, WdgXAxis,aspos,aslide);
      gl.glColor3f(.5f, .6f, .8f);

      if (gluvv.dmode == gluvvDataMode.GDM_V2G ) { //multiple channel
        float[] a3spos = { (float)(ballRad + (gluvv.tf.slider1hi) * (width - 2 * ballRad)), (float)height, 0f};
        drawSlider(drawable, 90, WdgYAxis, a3spos, a3slidehi);
        gl.glColor3f(.2f, .6f, .8f);
        a3spos[0] = (float)(ballRad) + (float)((gluvv.tf.slider1lo) * (width - 2 * ballRad));
        drawSlider(drawable, 90, WdgYAxis, a3spos, a3slidelo);
        if ( (lastSlide1Pos[0] != gluvv.tf.slider1hi) ||
            (lastSlide1Pos[1] != gluvv.tf.slider1lo)) {
          rasterizevgH(gluvv.volren.deptex2);
          gluvv.volren.loadTLUT = 1;
        }
        lastSlide1Pos[0] = gluvv.tf.slider1hi;
        lastSlide1Pos[1] = gluvv.tf.slider1lo;
      }

      if ( (gluvv.dmode == gluvvDataMode.GDM_VGH) || (gluvv.dmode == gluvvDataMode.GDM_V1GH)) { //vgh
        if (lastSlide1Pos[0] != gluvv.tf.slider1hi) {
          rasterizevgH(gluvv.volren.deptex2);
          gluvv.volren.loadTLUT = 1;
        }
        lastSlide1Pos[0] = gluvv.tf.slider1hi;
        float[] a3spos = { (float)(ballRad + (gluvv.tf.slider1hi) * (width - 2 * ballRad)), (float)height, 0f};
        drawSlider(drawable, 90, WdgYAxis, a3spos, a3slidehi);
      }


      gl.glDisable(GL.GL_LIGHTING);
      gl.glColor4f(0,0,0,1);                          //transfer function map depth
      gl.glLoadName(tfsheet1);
      gl.glBindTexture(GL.GL_TEXTURE_2D, gluvv.volren.deptexName[0]);
      gl.glBegin(GL.GL_QUADS);{  //draw the transfer function
      gl.glVertex3f((float)(barRad*1.5f),    (float)(barRad*1.5f),     0);
      gl.glVertex3f((float)width-(float)(barRad*1.5f), (float)(barRad*1.5f),     0);
      gl.glVertex3f((float)width-(float)(barRad*1.5f), (float)(height-barRad*1.5f), 0);
      gl.glVertex3f((float)(barRad*1.5f),    (float)(height-barRad*1.5f),0);
      }
      gl.glEnd();
      gl.glDisable(GL.GL_BLEND);
      gl.glDisable(GL.GL_TEXTURE_2D);
      gl.glEnable(GL.GL_LIGHTING);                       //---------------------------


      drawProbe(drawable);

      //-------------- load new pixel texture ----------------------------------
      if (gluvv.tf.clearpaint == 1) { //clear the paint texture
        clearPtex(paintex);
        gluvv.tf.clearpaint = 0;
        gluvv.tf.loadme = 1;
      }

      //gluvvShade oldshade;
      if (gluvv.tf.paintme == 1) { //add to the paint texture
          if ( gluvv.probe.brush == gluvvBrush.EllipseBrush ||
               gluvv.probe.brush == gluvvBrush.AutoEllipseBrush ||
               gluvv.probe.brush == gluvvBrush.OneDBrush ||
               gluvv.probe.brush == gluvvBrush.AutoOneDBrush ) {

            //oldshade = gluvv.shade; //faux shading is bad for painting
            //gluvv.shade = gluvvShadeAmb;
            brush.rasterize(paintex, null);

            //gluvv.shade = oldshade;
            gluvv.tf.paintme = 0;
            gluvv.tf.loadme = 1;
          } else if ( gluvv.probe.brush ==  gluvvBrush.TriangleBrush ) {
            LevWidget lwtmp = new LevWidget(brush);
            tris.insert(lwtmp);
            gluvv.tf.paintme = 0;
            gluvv.tf.loadme = 1;
          }

      }

      if (gluvv.tf.dropme == 1) {
        LevWidget lwtmp = new LevWidget( brush);
        tris.insert(lwtmp);
        gluvv.tf.dropme = 0;
        gluvv.tf.loadme = 1;
        // glutPostRedisplay();
      }

      if (gluvv.tf.loadme == 1) { //generate a new transfer function
        // clearPtex();
        clearPtex(gluvv.volren.deptex3);
        copyPtex(gluvv.volren.deptex, paintex);
        // System.out.println("tris ");
        tris.rasterize(gluvv.volren.deptex, gluvv.volren.deptex3);
        if (gluvv.tf.brushon == 1) {
          brush.rasterize(gluvv.volren.deptex, gluvv.volren.deptex3);
          // System.out.println("brush ");
        }
        loadPtex(drawable);
        gluvv.tf.loadme = 0;
        gluvv.volren.loadTLUT = 1;
      }

      //----------------------------------------------------------------------

      gl.glColor4f(1f, 1f, 1f, 1f); //transfer function color
      gl.glLoadName(tfsheet1);
      gl.glActiveTexture(GL.GL_TEXTURE0);
      gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glEnable(GL.GL_BLEND);
      gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE);
      gl.glDisable(GL.GL_LIGHTING);
      gl.glDepthFunc(GL.GL_EQUAL);

      if ( histo == null) {
        loadHist2D(drawable);
      }

      if (gluvv.tf.histOn == 1) { //histogram
        gl.glBindTexture(GL.GL_TEXTURE_2D, histName[0]);
        gl.glDisable(GL.GL_BLEND);
        gl.glBegin(GL.GL_QUADS);
        { //draw the transfer function
          gl.glMultiTexCoord2f(GL.GL_TEXTURE0, 0, 0);
          gl.glVertex3f((float)(barRad * 1.5f), (float)(barRad * 1.5f), 0f);
          gl.glMultiTexCoord2f(GL.GL_TEXTURE0, 1, 0);
          gl.glVertex3f((float)(width - barRad * 1.5f), (float)(barRad * 1.5f), 0);
          gl.glMultiTexCoord2f(GL.GL_TEXTURE0, 1, 1);
          gl.glVertex3f((float)(width - barRad * 1.5f), (float)(height - barRad * 1.5f), 0);
          gl.glMultiTexCoord2f(GL.GL_TEXTURE0, 0, 1);
          gl.glVertex3f((float)(barRad * 1.5f), (float)(height - barRad * 1.5f), 0);
        }
        gl.glEnd();
        gl.glEnable(GL.GL_BLEND);
        gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
      }
      else {
        gl.glEnable(GL.GL_BLEND);
        gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE);
      }

      //transfer function
      gl.glBindTexture(GL.GL_TEXTURE_2D, gluvv.volren.deptexName[0]);
      gl.glBegin(GL.GL_QUADS);
      { //draw the transfer function
        gl.glMultiTexCoord2f(GL.GL_TEXTURE0, 0, 0);
        gl.glVertex3f((float)(barRad * 1.5f), (float)(barRad * 1.5f), 0);
        gl.glMultiTexCoord2f(GL.GL_TEXTURE0, 1, 0);
        gl.glVertex3f((float)(width - barRad * 1.5f), (float)(barRad * 1.5f), 0);
        gl.glMultiTexCoord2f(GL.GL_TEXTURE0, 1, 1);
        gl.glVertex3f((float)(width - barRad * 1.5f), (float)(height - barRad * 1.5f), 0);
        gl.glMultiTexCoord2f(GL.GL_TEXTURE0, 0, 1);
        gl.glVertex3f((float)(barRad * 1.5f), (float)(height - barRad * 1.5f), 0);
      }
      gl.glEnd();

      gl.glDisable(GL.GL_BLEND);
      gl.glDisable(GL.GL_TEXTURE_2D);
      gl.glEnable(GL.GL_LIGHTING); //---------------------------
      gl.glDepthFunc(GL.GL_LESS);

      gl.glPushMatrix();
      { //draw levoy triangles
        gl.glTranslatef((float)(ballRad), (float)(ballRad), (float)(ballRad));
        tris.draw(drawable);
        brush.draw(drawable);
      }
      gl.glPopMatrix();

    }

    //=========================================================== draw probe
  //======================================================================
    public void drawProbe(GLAutoDrawable drawable) {
      GL gl = drawable.getGL();
      int xi = gluvv.mv.xiSize; //size of volume (axies)
      int yi = gluvv.mv.yiSize;
      int zi = gluvv.mv.ziSize;
      int px, py, pz;
      float[] fpos = new float[3];
      int i, j, k;


      if (gluvv.mprobe == 0) { //probe
        px = (int)gluvv.probe.vpos[0] * xi; //integer position in volume
        py = (int)gluvv.probe.vpos[1] * yi; //(least)
        pz = (int)gluvv.probe.vpos[2] * zi;
        fpos[0] = gluvv.probe.vpos[0] * xi; //floating point position in volume
        fpos[1] = gluvv.probe.vpos[1] * yi; //
        fpos[2] = gluvv.probe.vpos[2] * zi;
        if ( (lastppos[0] != gluvv.probe.vpos[0]) ||
            (lastppos[1] != gluvv.probe.vpos[1]) ||
            (lastppos[2] != gluvv.probe.vpos[2]))
          if (gluvv.tf.brushon == 1)
            gluvv.tf.loadme = 1;
        lastppos[0] = gluvv.probe.vpos[0];
        lastppos[1] = gluvv.probe.vpos[1];
        lastppos[2] = gluvv.probe.vpos[2];
      }
      else { //clip
        px = (int)gluvv.clip.mousepnt[0] * xi; //integer position in volume
        py = (int)gluvv.clip.mousepnt[1] * yi; //(least)
        pz = (int)gluvv.clip.mousepnt[2] * zi;
        fpos[0] = gluvv.clip.mousepnt[0] * xi; //floating point position in volume
        fpos[1] = gluvv.clip.mousepnt[1] * yi;
        fpos[2] = gluvv.clip.mousepnt[2] * zi;
        if ( (lastppos[0] != gluvv.clip.mousepnt[0]) ||
            (lastppos[1] != gluvv.clip.mousepnt[1]) ||
            (lastppos[2] != gluvv.clip.mousepnt[2]))
          if (gluvv.tf.brushon == 1)
            gluvv.tf.loadme = 1;
        lastppos[0] = gluvv.clip.mousepnt[0];
        lastppos[1] = gluvv.clip.mousepnt[1];
        lastppos[2] = gluvv.clip.mousepnt[2];
      }

      if ( (px < 1) || (px > (xi - 2)) || //are we within the bounds
          (py < 1) || (py > (yi - 2)) || //of the volume???
          (pz < 1) || (pz > (zi - 2))) {
        float[] tmpp = {0, 0};
        brush.setPos(tmpp, tmpp, tmpp, -10f, -10f);
        //gluvv.tf.loadme = 1;
        return; //no, nothing to do here
      }

      byte[] dp = gluvv.mv.volumes[0].currentData;
      //I need to handle bricked volumes !!!!
      if ( (gluvv.mv.numSubVols != 1) && (gluvv.mv.wholeVol != null)) {
        dp = gluvv.mv.wholeVol.currentData;
      }
      //unsigned char *gp = (unsigned char*)gluvv.mv->volumes[0].currentData1;
      float[][] vpnts = new float[8][3]; //voxel corners, tf space
      float[][] vvals = new float[8][3]; //voxel values, [0..1]

      int sx = gluvv.mv.xiSize;
      int sy = gluvv.mv.yiSize;
      int sz = gluvv.mv.ziSize;

      //initialize values
      for (i = 0; i < 2; ++i) { //z = i
        for (j = 0; j < 2; ++j) { //y = j
          for (k = 0; k < 2; ++k) { //x = k
            vpnts[i * 4 + j * 2 + k][0] = 0;
            vpnts[i * 4 + j * 2 + k][1] = 0;
            vvals[i * 4 + j * 2 + k][0] = 0;
            vvals[i * 4 + j * 2 + k][1] = 0;
          }
        }
      }
      int ne = gluvv.mv.nelts;
      float hacktmp;
      for (i = 0; i < 2; ++i) { //z = i
        for (j = 0; j < 2; ++j) { //y = j
          for (k = 0; k < 2; ++k) { //x = k

              if ( gluvv.dmode == gluvvDataMode.GDM_V1 ) {
                vpnts[i * 4 + j * 2 + k][0] = (float) ballRad + //get 1st value
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne)] /
                    255.0f * (width - 2 * (float) ballRad);
                vvals[i * 4 + j * 2 + k][0] = //draw in center
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne)] / 255.0f;
                vpnts[i * 4 + j * 2 + k][1] = (float) ballRad + .5f * (height - 2 * (float) ballRad); ;

              } else if ( gluvv.dmode == gluvvDataMode.GDM_V1G ||
                          gluvv.dmode == gluvvDataMode.GDM_V2 ||
                          gluvv.dmode == gluvvDataMode.GDM_V2G ) {
                vpnts[i * 4 + j * 2 + k][0] = (float)(ballRad) + //get 1st value
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne)] /
                    255.0f * (float)(width - 2 * (float)(ballRad));
                vvals[i * 4 + j * 2 + k][0] = dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne)] / 255.0f;
                vpnts[i * 4 + j * 2 + k][1] = (float)(ballRad) + //get 2nd value
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 1] /
                    255.0f * (height - 2 * (float)(ballRad));
                vvals[i * 4 + j * 2 + k][1] = dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 1] / 255.0f;
              } else if ( gluvv.dmode == gluvvDataMode.GDM_V2GH ||
                          gluvv.dmode == gluvvDataMode.GDM_V3 ||
                          gluvv.dmode == gluvvDataMode.GDM_V3G || //sigh what am I going to do about 4 value volumes
                          gluvv.dmode == gluvvDataMode.GDM_V4 ) {
                vpnts[i * 4 + j * 2 + k][0] = (float) ballRad + //get 1st value
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne)] /
                    255.0f * (width - 2 * (float) ballRad);
                vvals[i * 4 + j * 2 + k][0] =
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne)] / 255.0f;
                vpnts[i * 4 + j * 2 + k][1] = (float) ballRad + //get 2nd value
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 1] /
                    255.0f * (height - 2 * (float) ballRad);
                vvals[i * 4 + j * 2 + k][1] = dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 1] / 255.0f;
                vpnts[i * 4 + j * 2 + k][2] = (float) ballRad + //get 3rd value
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 2] /
                    255.0f * (width - 2 * (float) ballRad);
                vvals[i * 4 + j * 2 + k][2] = dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 2] / 255.0f;
              } else if ( gluvv.dmode == gluvvDataMode.GDM_VGH ||
                          gluvv.dmode == gluvvDataMode.GDM_V1GH ||
                          gluvv.dmode == gluvvDataMode.GDM_VGH_VG ||
                          gluvv.dmode == gluvvDataMode.GDM_VGH_V ) {
                vpnts[i * 4 + j * 2 + k][0] = (float)ballRad + //get 1st value
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne)] /
                    255.0f * (width - 2 * (float)ballRad);
                vvals[i * 4 + j * 2 + k][0] =
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne)] / 255.0f;
                vpnts[i * 4 + j * 2 + k][1] = (float)ballRad + //get 2nd value
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 1] /
                    255.0f * (height - 2 * (float)ballRad);
                vvals[i * 4 + j * 2 + k][1] =
                    dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 1] / 255.0f;

                //get hessian
                hacktmp = dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 2] / 85.0f - 1f;
                hacktmp = hacktmp > 0 ? (float)Math.sqrt(hacktmp) : -(float)Math.sqrt( -hacktmp);
                vpnts[i * 4 + j * 2 + k][2] = (hacktmp + 1) / 2.0f;
                vvals[i * 4 + j * 2 + k][2] = dp[ ( (pz + i) * sx * sy * ne) + ( (py + j) * sx * ne) + ( (px + k) * ne) + 2] / 169.0f;
              }


          }
        }
      }
      //done getting voxel corner values

      if (gluvv.picking == 0) {
        gl.glDisable(GL.GL_LIGHTING);
        //glEnable(GL_BLEND);
        gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
        //glDisable(GL_DEPTH_TEST);
        //glDepthFunc(GL_EQUAL);
        gl.glColor4f(1f, 1f, 1f, .3f);

        gl.glBegin(GL.GL_LINE_STRIP);
        {
          gl.glVertex3f(vpnts[0][0], vpnts[0][1], .01f);
          gl.glVertex3f(vpnts[1][0], vpnts[1][1], .01f);
          gl.glVertex3f(vpnts[3][0], vpnts[3][1], .01f);
          gl.glVertex3f(vpnts[2][0], vpnts[2][1], .01f);
          gl.glVertex3f(vpnts[0][0], vpnts[0][1], .01f);
          gl.glVertex3f(vpnts[4][0], vpnts[4][1], .01f);
          gl.glVertex3f(vpnts[5][0], vpnts[5][1], .01f);
          gl.glVertex3f(vpnts[7][0], vpnts[7][1], .01f);
          gl.glVertex3f(vpnts[6][0], vpnts[6][1], .01f);
          gl.glVertex3f(vpnts[4][0], vpnts[4][1], .01f);
        }
        gl.glEnd();

        gl.glBegin(GL.GL_LINES);
        {
          gl.glVertex3f(vpnts[1][0], vpnts[1][1], .01f);
          gl.glVertex3f(vpnts[5][0], vpnts[5][1], .01f);

          gl.glVertex3f(vpnts[3][0], vpnts[3][1], .01f);
          gl.glVertex3f(vpnts[7][0], vpnts[7][1], .01f);

          gl.glVertex3f(vpnts[2][0], vpnts[2][1], .01f);
          gl.glVertex3f(vpnts[6][0], vpnts[6][1], .01f);
        }
        gl.glEnd();

        gl.glEnable(GL.GL_DEPTH_TEST);
        gl.glDisable(GL.GL_BLEND);
      } //if not picking

      float[] val = new float[3];
      triLerpV3(val, vvals, fpos); //trilinear interp for values

      //------------ Paint Brush ---------------------------------------
      if (gluvv.tf.brushon == 1) { //wow clean this up!!!
        if (gluvv.probe.brush == gluvvBrush.EllipseBrush) {
          float bsz = (1.0f - gluvv.probe.slider) / 4.0f;
          float[] plpos = {val[0] - bsz, val[1] + bsz}; //paint positions
          float[] prpos = {val[0] + bsz, val[1] + bsz};
          float[] pbpos = {val[0] + bsz, val[1] - bsz};
          brush.setPos(pbpos, plpos, prpos, val[0], val[1]);
          brush.drawOn = 0;
          brush.squareType();
          gl.glPushMatrix();
          { //draw brush
            gl.glTranslatef((float)ballRad, (float)ballRad, (float)ballRad);
            brush.draw(drawable);
          }
          gl.glPopMatrix();
        }
        else if ( (gluvv.probe.brush == gluvvBrush.TriangleBrush) ||
                 (gluvv.probe.brush == gluvvBrush.AutoEllipseBrush)) {
          float maxx = -1000;
          float maxy = -1000;
          float minx = 1000;
          float miny = 1000;
          for (i = 0; i < 8; ++i) {
            maxx = Math.max(maxx, vvals[i][0]);
            maxy = Math.max(maxy, vvals[i][1]);
            minx = Math.min(minx, vvals[i][0]);
            miny = Math.min(miny, vvals[i][1]);
          }
          float bsz = (1.0f - gluvv.probe.slider) * 2;
          float w = (float)Math.max( (maxx - minx) / 2.0f, .01f);
          float h = (float)Math.max( (maxy - miny) / 2.0f, .01f);
          float[] plpos = {val[0] - w * bsz, val[1] + h * bsz};
          float[] prpos = {val[0] + w * bsz, val[1] + h * bsz};
          float[] pbpos = {val[0], 0};
          if (gluvv.probe.brush == gluvvBrush.TriangleBrush) {
            brush.setPos(pbpos, plpos, prpos, 0, val[1] - h * bsz);
            brush.drawOn = 1;
            brush.triangleType();
          }
          else {
            pbpos[0] = val[0] + w * bsz;
            pbpos[1] = val[1] - h * bsz;
            brush.setPos(pbpos, plpos, prpos, val[0], val[1]);
            brush.drawOn = 0;
            brush.squareType();
            gl.glPushMatrix();
            { //draw brush
              gl.glTranslatef((float)ballRad, (float)ballRad, (float)ballRad);
              brush.draw(drawable);
            }
            gl.glPopMatrix();
          }
        }
        if ( (gluvv.dmode == gluvvDataMode.GDM_V1) || (gluvv.dmode == gluvvDataMode.GDM_VGH_V) ||
            (gluvv.probe.brush == gluvvBrush.OneDBrush) ||
            (gluvv.probe.brush == gluvvBrush.AutoOneDBrush)) {
          float maxx = -1000;
          float minx = 1000;
          for (i = 0; i < 8; ++i) {
            maxx = Math.max(maxx, vvals[i][0]);
            minx = Math.min(minx, vvals[i][0]);
          }
          float bsz = (1.0f - gluvv.probe.slider) * 2;
          float w = Math.max( (maxx - minx) / 2.0f, .01f);
          float[] plpos = {val[0] - w * bsz, 1};
          float[] prpos = {val[0] + w * bsz, 1};
          float[] pbpos = {val[0], 0};
          if (gluvv.probe.brush == gluvvBrush.OneDBrush) {
            plpos[0] = val[0] + (1.0f - gluvv.probe.slider) / 4.0f;
            prpos[0] = val[0] - (1.0f - gluvv.probe.slider) / 4.0f;
          }
          brush.setPos(pbpos, plpos, prpos, val[0], val[1]);
          brush.oneDType();
        }

      }
      //------------ end Paint Brush ------------------------------------

      if (gluvv.picking == 0) {
        //position of point in tf space:
        gl.glPushMatrix();
        {
          gl.glTranslatef((float)(ballRad + val[0] * (width - 2 * ballRad)),
                       (float)(ballRad + val[1] * (height - 2 * ballRad)),
                       .01f);
          gl.glColor4f(1, 0, 0, 1);
          gl.glLoadName(dpoint);
          glut.glutSolidSphere(ballRad / 2.5f, 20, 20);
        }
        gl.glPopMatrix();
        //draw voxel points in data domain
        for (i = 0; i < 8; ++i) {
          gl.glPushMatrix();
          {
            gl.glTranslatef(vpnts[i][0], vpnts[i][1], .01f);
            float[] bcol = {1, 1, 1};
            if (gluvv.dmode == gluvvDataMode.GDM_VGH) {
              bcol[0] = don[ ( (int) (vpnts[i][2] * 22) + 1) * 3 + 0] / 255.0f;
              bcol[1] = don[ ( (int) (vpnts[i][2] * 22) + 1) * 3 + 1] / 255.0f;
              bcol[2] = don[ ( (int) (vpnts[i][2] * 22) + 1) * 3 + 2] / 255.0f;
            }
            gl.glColor4f(bcol[0], bcol[1], bcol[2], 1);
            glut.glutSolidSphere(ballRad / 3.5f, 20, 20);
          }
          gl.glPopMatrix();
        }
      }
      gl.glEnable(GL.GL_LIGHTING);

    }

    //=========================================================== trilerp v3
//======================================================================
    // float val[3], float voxels[8][3], float pos[3]
    private void triLerpV3(float[] val, float[][] voxels, float[] pos)
    {
      //fractional values
      float fx = pos[0] - (int)pos[0];
      float fy = pos[1] - (int)pos[1];
      float fz = pos[2] - (int)pos[2];

      //now interpolate to find the data value at our point
      //first allong x direction
      for(int i=0; i<3; ++i){
        float x1 = voxels[0][i] + (float)(voxels[1][i] - voxels[0][i]) * fx;
        float x2 = voxels[2][i] + (float)(voxels[3][i] - voxels[2][i]) * fx;
        float x3 = voxels[4][i] + (float)(voxels[5][i] - voxels[4][i]) * fx;
        float x4 = voxels[6][i] + (float)(voxels[7][i] - voxels[6][i]) * fx;
        //then along y direction
        float xy1 = x1 + (x2 - x1) * fy;
        float xy2 = x3 + (x4 - x3) * fy;
        //finaly allong z direction
        val[i] = (xy1 + (xy2 - xy1) * fz);
      }

    }


    //================================================================= init
//======================================================================
    public void init()
    {
       widgetInit();

       //-------- Levoy triangles -----------
      tris = new LevWidget(gluvv);
      tris.setWH(width-(float)ballRad*2, height-(float)ballRad*2);
      float[] b = {.5f,0f}; float[] l={.3f,.7f}; float[] r = {.7f,.7f};
      tris.setPos(b,l,r, -10f, -10f);
      tris.setBallBar((float)ballRad/2.0f, (float)barRad/2.0f);
      tris.setDepth(position[2]);
      tris.squareType();
      tris.on = 0;  //turn root triangle off

      shadTris = new LevWidget(gluvv);
      shadTris.setWH(width-(float)ballRad*2, height-(float)ballRad*2);
      shadTris.setPos(b,l,r, -10f, -10f);
      shadTris.setBallBar((float)ballRad/2.0f, (float)barRad/2.0f);
      shadTris.setDepth(position[2]);
      shadTris.squareType();
      shadTris.on = 0;  //turn root triangle off

      brush= new LevWidget(gluvv);
      brush.setWH(width-(float)ballRad*2, height-(float)ballRad*2);
      brush.setPos(b,l,r, -10f, -10f);
      brush.setBallBar((float)ballRad/2.0f, (float)barRad/2.0f);
      brush.setDepth(position[2]);
      brush.setAlpha(.7f);
      brush.squareType();
      brush.on = 1;     //turn paint brush on
      brush.drawOn = 0; //but dont actualy draw the widget

      paintex = new byte[gluvv.tf.ptexsz[0]*gluvv.tf.ptexsz[1]*gluvv.tf.ptexsz[2]*gluvv.tf.numelts];
      clearPtex(paintex);

    }


    //============================================================ load Hist
//======================================================================
    private void loadHist2D(GLAutoDrawable drawable) {
      GL gl = drawable.getGL();
      histo = new byte[256 * 256];
      if (gluvv.mv.hist2D(histo) == 0) {
        histo = null;
        return;
      }

      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glGenTextures(1, histName, 0); //the dep tex that we use for the tf
      gl.glBindTexture(GL.GL_TEXTURE_2D, histName[0]);

      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      ByteBuffer bBuffer = ByteBuffer.wrap(histo);
      bBuffer.rewind();
      gl.glTexImage2D(GL.GL_TEXTURE_2D,
                   0,
                   GL.GL_LUMINANCE,
                   256,
                   256,
                   0,
                   GL.GL_LUMINANCE,
                   GL.GL_UNSIGNED_BYTE,
                   bBuffer);
      gl.glFlush();

      gl.glDisable(GL.GL_TEXTURE_2D);

    }


    //================================================================= pick
//======================================================================
    public int  pick(int data1, int data2, int data3, float x, float y, float z)
    {
            //cerr << "hi you picked the tf widget" << endl;
            return 1;
    }

    public int pick() {
      fill();
      pickedObj = gluvv.pick.data3;

      if (pickedObj == LevWidget.LEVWIDGNAME) {
        tris.release();
        pickedTri = gluvv.pick.data1;

        if (pickedTri == brush.id) {
          return brush.pick();
        } else {
          return tris.pick();
        }

      }

      return 1;
    }



    //============================================================== release
  //======================================================================
    public int release() {
      return 0;
    }

  //============================================================ clearPtex
  //======================================================================
    private void clearPtex() {
      int sz = (gluvv.tf.numelts *
                gluvv.tf.ptexsz[0] *
                gluvv.tf.ptexsz[1] *
                gluvv.tf.ptexsz[2]);

      for (int i = 0; i < sz; ++i) {
        gluvv.volren.deptex[i] = 0;
      }
    }

    private void clearPtex(byte[] ptex) {
      if (ptex == null ) return;
      int sz = (gluvv.tf.numelts *
                gluvv.tf.ptexsz[0] *
                gluvv.tf.ptexsz[1] *
                gluvv.tf.ptexsz[2]);
      for (int i = 0; i < sz; ++i) {
        ptex[i] = 0;
      }
    }

  //============================================================ copyPtex
  //======================================================================
    private void copyPtex(byte[] ptexout, byte[] ptexin) {
      if ( (ptexout == null) || (ptexin == null))return;
      int sz = (gluvv.tf.numelts *
                gluvv.tf.ptexsz[0] *
                gluvv.tf.ptexsz[1] *
                gluvv.tf.ptexsz[2]);
      for (int i = 0; i < sz; ++i) {
        ptexout[i] = ptexin[i];
      }
    }

    //======================================================== rasterize vgH
//======================================================================
    private void rasterizevgH(byte[] ptex)
    {
            int i, j;
            if(ptex == null) return;
            //vgh style
            if((gluvv.dmode == gluvvDataMode.GDM_VGH) || (gluvv.dmode == gluvvDataMode.GDM_V1GH)){
                    int cent = (int)(gluvv.tf.ptexsz[0]/3.0f);
                    int sti = gluvv.tf.ptexsz[0]*4;
                    int stj = 4;

                    float b = 255f-20f*cent*(1f-gluvv.tf.slider1hi);
                    float m = Math.abs(255-b)/(float)cent;

                    for(i=0; i<gluvv.tf.ptexsz[1]; ++i){
                            for(j=0; j<cent; ++j){
                                    ptex[i*sti + j*stj + 3] = new Double(math.CLAMP_ARB(0,j*m + b,255)).byteValue();
                            }
                    }

                    b = 255;
                    m = -m;
                    for(i=0; i<gluvv.tf.ptexsz[1]; ++i){
                            //I added 2 extra vertical lines here to handle interpolation artifacts
                            for(j=1; j<cent+1; ++j){
                                    ptex[i*sti + (j+cent)*stj + 3] = new Double(math.CLAMP_ARB(0,j*m + b,255)).byteValue();
                            }
                    }
            }
            if((gluvv.dmode == gluvvDataMode.GDM_V2G) || (gluvv.dmode == gluvvDataMode.GDM_V2GH)){
                    int sti = gluvv.tf.ptexsz[0]*4;
                    int stj = 4;

                    //up to the bottom slider
                    for(i=0; i<gluvv.tf.ptexsz[1]; ++i){  //all the way to the top
                            for(j=0; j<(int)(gluvv.tf.slider1lo*255); ++j){
                                    ptex[i*sti + j*stj + 3] = 0;
                            }
                            //bottom slider to top slider
                            for(j=(int)(gluvv.tf.slider1lo*255); j<(int)(gluvv.tf.slider1hi*255); ++j){
                                    ptex[i*sti + j*stj + 3] = new Float((j-(int)(gluvv.tf.slider1lo*255f))/((gluvv.tf.slider1hi-gluvv.tf.slider1lo)*255f)*255f).byteValue();;
                            }
                            //top slider to end
                            for(j=(int)(gluvv.tf.slider1hi*255); j<gluvv.tf.ptexsz[0]; ++j){
                                    ptex[i*sti + j*stj + 3] = new Integer(255).byteValue();
                            }
                    }
            }
      }


      //============================================================= loadPtex
    //======================================================================
    // this is just the visible texture, the volume renderer will load the actual tf
      public void loadPtex(GLAutoDrawable drawable) {
        GL gl = drawable.getGL();
        gl.glEnable(GL.GL_TEXTURE_2D);
        gl.glBindTexture(GL.GL_TEXTURE_2D, gluvv.volren.deptexName[0]);

        gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
        gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
        gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
        gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
        gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

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

        gl.glDisable(GL.GL_TEXTURE_2D);
      }

      //======================================================== mouse 2 plane
//======================================================================
      private void mouse2plane(int x, int y)
      {
        // assume we are always aligned with z axis
        // should be based on up vector and (at-eye)xup vector!!!!!
        float fx = 1 - (float)x/(float)gluvv.win.width;
        float fy = (float)(gluvv.win.height - y)/(float)gluvv.win.height;
        float[] vdir = new float[3];
        math.subV3(vdir, gluvv.env.at, gluvv.env.eye);
        math.normalizeV3(vdir);
        math.scaleV3(gluvv.env.clip[0], vdir);
        float[] fcp = new float[3]; //position of front clipping plane
        math.addV3(fcp, gluvv.env.eye, vdir);
        float[] cpnt = new float[3];//clip plane point (projecton of screen point on clip plane)
        cpnt[0] = ((fcp[0] + gluvv.env.frustum[0]) +
                   (gluvv.env.frustum[1] - gluvv.env.frustum[0]) * fx);
        cpnt[1] = ((fcp[1] + gluvv.env.frustum[2]) +
                   (gluvv.env.frustum[3] - gluvv.env.frustum[2]) * fy);
        cpnt[2] = fcp[2]; //remember, assume z aligned view direction

        float[] ep = new float[3];  //eye - pos
        math.subV3(ep, gluvv.env.eye, position);
        float[] me = new float[3];  //eye - clipplane point
        math.subV3(me, gluvv.env.eye, cpnt);
        float[] dir = {0,0,-1};
        float t = math.dotV3(dir, ep)/math.dotV3(dir, me);
        math.negateV3(me);
        math.scaleV3(t,me);
        math.addV3(mousepnt, me, gluvv.env.eye);
      }

      // Methods required for the implementation of MouseListener
      public void mouseEntered(MouseEvent e) {}

      public void mouseExited(MouseEvent e) {}

      public void mouseMoved(MouseEvent e) {}

      public void mouseReleased(MouseEvent e) {
        gluvv.volren.sampleRate = gluvv.volren.goodSamp;
        if (pickedObj == LevWidget.LEVWIDGNAME) {
          if (pickedTri == brush.id) {
            brush.mouseReleased(e);
            // return;
          }
          else {
            tris.mouseReleased(e);
            // return;
          }
        }
        brush.mouseReleased(e);
        tris.mouseReleased(e);
      }

      public void mouseClicked(MouseEvent e) {}


      public void mousePressed(MouseEvent e) {
        int x, y;
        // int button = e.getButton();
        x = e.getX();
        y = e.getY();

        if (pickedObj == LevWidget.LEVWIDGNAME) {
          if (pickedTri == brush.id) {
            brush.mousePressed(e);
            return;
          } else {
            tris.mousePressed(e);
            return;
          }
        }

          lastwinpos[0] = x;
          lastwinpos[1] = y;
            if ( gluvv.mouse.button ==  MouseEvent.BUTTON1 ) {
              if ( pickedObj == tfsheet0 ||
                   pickedObj == tfsheet1 ||
                   pickedObj == tfsheet2 ) {

                  //mode = color;

              }

            } else if ( gluvv.mouse.button ==  MouseEvent.BUTTON2 ) {
              if ( pickedObj == tfsheet0 ||
                   pickedObj == tfsheet1 ||
                   pickedObj == tfsheet2 ) {


                  //mode = colorBlend;

              }
            } else if ( gluvv.mouse.button ==  MouseEvent.BUTTON3 ) {
              LevWidget lwtmp = new LevWidget(gluvv);
              mouse2plane(x, y);
              float[] pnt = {1 - (mousepnt[0] + position[0]) / width,
                  (mousepnt[1] - position[1]) / height};
              float[] b = {pnt[0], 0};
              float[] l = {pnt[0] - .1f, pnt[1]};
              float[] r = {pnt[0] + .1f, pnt[1]};
              lwtmp.setPos(b, l, r, l[1] * .5f, -10.0f);
              lwtmp.setDepth(position[2]);
              tris.insert(lwtmp);
              gluvv.tf.loadme = 1;

            }
          // lastwinpos[0] = x;
          // lastwinpos[1] = y;

      }

      public void mouseDragged(MouseEvent e) {
        int x, y;
        int button = e.getButton();
        x = e.getX();
        y = e.getY();

        String str;
        if (pickedObj == LevWidget.LEVWIDGNAME) {
          if (pickedTri == brush.id) {
            brush.mouseDragged(e);
            return;
          } else {
            tris.mouseDragged(e);
            return;
          }
        }


        float sdx = (float) (x - lastwinpos[0]) / (float) gluvv.win.width;
        float sdy = (float) (y - lastwinpos[1]) / (float) gluvv.win.height;
        //had to change this should be smarter
        float[] d = new float[3];
        math.subV3(d, position, gluvv.env.eye);
        //float dist = normV3(d);
        float dist = position[2] - gluvv.env.eye[2];
        float dx = sdx *
            (gluvv.env.frustum[1] - gluvv.env.frustum[0]) * dist / gluvv.env.clip[0];
        float dy = sdy *
            (gluvv.env.frustum[3] - gluvv.env.frustum[2]) * dist / gluvv.env.clip[0];

           if ( gluvv.mouse.button ==  MouseEvent.BUTTON1 ) {
                if ( pickedObj == gslide ) {
                    gluvv.volren.goodSamp += sdx * 6;
                } else if (  pickedObj == islide ) {
                    gluvv.volren.interactSamp += sdx * 6;
                } else if ( pickedObj == aslide ) {
                    gluvv.volren.gamma -= sdy / height * 10;
                    gluvv.volren.gamma = gluvv.volren.gamma < 0 ? 0 : gluvv.volren.gamma;
                } else if (  pickedObj == a3slidehi ) {
                    gluvv.tf.slider1hi += sdx;
                    gluvv.tf.slider1hi = gluvv.tf.slider1hi < 0 ? 0 :
                        (gluvv.tf.slider1hi > 1 ? 1 : gluvv.tf.slider1hi);
                    if (gluvv.tf.slider1hi < gluvv.tf.slider1lo) gluvv.tf.slider1lo =
                        gluvv.tf.slider1hi;
                } else if ( pickedObj ==  a3slidelo ) {
                    gluvv.tf.slider1lo += sdx;
                    gluvv.tf.slider1lo = gluvv.tf.slider1lo < 0 ? 0 :
                        (gluvv.tf.slider1lo > 1 ? 1 : gluvv.tf.slider1lo);
                    if (gluvv.tf.slider1hi < gluvv.tf.slider1lo) gluvv.tf.slider1hi =
                        gluvv.tf.slider1lo;
                } else if ( pickedObj == bbar ||
                            pickedObj == tbar ||
                            pickedObj == lbar ||
                            pickedObj == rbar ) {
                    screenpos[0] -= sdx;
                    screenpos[1] -= sdy;
                    gluvv.volren.sampleRate = gluvv.volren.interactSamp;
                } else if ( pickedObj == blball ) {
                    screenpos[0] -= sdx;
                    screenpos[1] -= sdy;
                    screenwidth -= sdx;
                    screenheight += sdy;
                    gluvv.volren.sampleRate = gluvv.volren.interactSamp;
                } else if ( pickedObj ==  brball ) {
                    screenpos[1] -= sdy;
                    screenwidth += sdx;
                    screenheight += sdy;
                    gluvv.volren.sampleRate = gluvv.volren.interactSamp;
                } else if ( pickedObj == tlball ) {
                    screenpos[0] -= sdx;
                    screenwidth -= sdx;
                    screenheight -= sdy;
                    gluvv.volren.sampleRate = gluvv.volren.interactSamp;
                    // glutSetWindowTitle("Transfer Function Widget: RESIZE");
                    // glutPostRedisplay();
                } else if ( pickedObj ==  trball ) {
                    screenwidth += sdx;
                    screenheight -= sdy;
                    gluvv.volren.sampleRate = gluvv.volren.interactSamp;
                    // glutSetWindowTitle("Transfer Function Widget: RESIZE");
                    // glutPostRedisplay();
                }
                    /*
                     #if 0
                         case beslider:
                                                   if(pickedTri){
                                                           LevWidget *tt;
                     if(tt = tris->get(pickedTri)){
                     float slider = tt->getBE();
                     slider += dx/width;
                     tt->setBE(slider);
                     glutSetWindowTitle("Transfer Function Widget: Boundary Emphasis");
                     gluvv.volren.sampleRate = gluvv.volren.interactSamp;
                     glutPostRedisplay();
                     gluvv.tf.loadme = 1;
                                                           }
                                                   }
                                                   break;
                         #endif
                     */

              } else if ( gluvv.mouse.button ==  MouseEvent.BUTTON3 ) {
                if ( pickedObj == bbar ||
                     pickedObj == tbar ||
                     pickedObj == lbar ||
                     pickedObj == rbar ) {
                    position[0] -= dx;
                    position[2] -= dy;
                    gluvv.volren.sampleRate = gluvv.volren.interactSamp;

                }
              } else if ( gluvv.mouse.button == MouseEvent.BUTTON2 ) {

              }


        lastwinpos[0] = x;
        lastwinpos[1] = y;
      }


      public void keyPressed(KeyEvent e) {
        char key = e.getKeyChar();
        int keyCode = e.getKeyCode();

        if(keyCode == KeyEvent.VK_DELETE){ //delete key
                if(pickedObj == LevWidget.LEVWIDGNAME){ //you must mean delete a triangle
                        if(pickedTri == tris.id){ //can't delete the root triangle!!!
                                tris.on = 0;
                                gluvv.tf.loadme = 1;
                        }
                        LevWidget  tmplw = tris.remove(pickedTri);
                        tmplw = null;
                        gluvv.tf.loadme = 1;
                }
        }
        switch(key){
        case 'B':
                if(brush.on == 1){
                        if(brush.drawOn == 1) brush.drawOn = 0;
                        else brush.drawOn = 1;
                }
                break;
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


      //-----------------------------------------------------------------------
        public void fill()
        {
          glu.gluQuadricDrawStyle(qobj, GLU.GLU_FILL);
          ballSlice = 10;
          barSlice = 10;
        }

        public void drawSphere(GLAutoDrawable drawable, float[] pos, int name) {
          GL gl = drawable.getGL();
          gl.glPushMatrix();
          gl.glLoadName(name);
          gl.glTranslatef(pos[0], pos[1], pos[2]);

          glu.gluSphere(qobj, ballRad, (int) ballSlice, (int) ballSlice);

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
