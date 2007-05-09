package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;
import java.awt.event.*;

public class LTWidgetRen extends gluvvPrimitive
            implements MouseListener, MouseMotionListener{

  private float[] screenpos = new float[2];
  private float screenrad;

  private float[] position = new float[3];
  private float[] lastpos = new float[3];

  private Trackball tball;

  private int[] lastwinpos = new int[2];

  private int drawOn;

  private static int start = 1;

  private gluvvGlobal gluvv;

  private GLUT glut = new GLUT();
  private GLU glu = new GLU();
  private boolean dragFlag = false;

  private VectorMath math = new VectorMath();

  public LTWidgetRen(gluvvGlobal _gluvv) {
    screenpos[0] = -.4f;
    screenpos[1] = -.1f;
    screenrad = .05f;
    position[2] = -1f;
    drawOn = 0;
    gluvv = _gluvv;
  }

  //=============================================================== draw
//====================================================================
  public void draw(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    start = 1;
    if (drawOn == 0)return;

    if ( dragFlag == true ) {
      genXForm(drawable);
    }

    if (start == 1) {
      gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, gluvv.light.pos, 0);
      start = 0;
    }

    float dist = position[2] - gluvv.env.eye[2];

    position[0] = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
                   dist / gluvv.env.clip[0]) * screenpos[0];
    position[1] = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
                   dist / gluvv.env.clip[0]) * screenpos[1];

    float ballRad = ( (gluvv.env.frustum[1] - gluvv.env.frustum[0]) *
                     dist / gluvv.env.clip[0]) * screenrad;

    gluvvPushName(drawable);
    gl.glPushName(22);
    gl.glPushName(222);
    gl.glPushName(2222);

    gl.glPushMatrix();
    {
      gl.glTranslatef(position[0], position[1], position[2]);
      glut.glutSolidSphere(ballRad, 20, 20);
    }
    gl.glPopMatrix();

    gluvvPopNames(drawable);

    System.err.println("TFWidgetRen" + "draw()");
  }

  //=============================================================== init
//====================================================================
  public void init(GLAutoDrawable drawable)
  {
    GL gl = drawable.getGL();
    float[] lxf = new float[16];
    tball = new Trackball();
    tball.buildRotMatrix(lxf);
    math.translateV3(gluvv.light.pos, lxf, gluvv.light.startpos);
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, gluvv.light.pos, 0);
    math.copyV3(lastpos, gluvv.light.pos);
    tball.clear();
    genXForm(drawable);
  }

  //=============================================================== pick
//====================================================================
  public int pick(int data1, int data2, int data3, float x, float y, float z)
  {
          return 1;
  }

  //=============================================================== pick
//====================================================================
  public int pick()
  {
          return 1;
  }

  //================================================================ key
//====================================================================
  public int key(char key, int x, int y)
  {

          return 0;
  }


  //============================================================ release
//====================================================================
  public int release()
  {
          return 0;
  }

  public int onoff()
  {
          if(drawOn == 1) drawOn = 0;
          else drawOn = 1;
          return drawOn;
  }

  //============================================= generate the Transform
//====================================================================
  public void genXForm(GLAutoDrawable drawable)
  {
    GL gl = drawable.getGL();
    float d0 = math.normV3(gluvv.light.pos);

    //cerr << "norm " << d0 << " div " << 1.0/(d0) << endl;
    //projection matrix
    gluvv.light.pj[0] = 1;
    gluvv.light.pj[1] = 0;
    gluvv.light.pj[2] = 0;
    gluvv.light.pj[3] = 0;
    gluvv.light.pj[4] = 0;
    gluvv.light.pj[5] = 1;
    gluvv.light.pj[6] = 0;
    gluvv.light.pj[7] = 0;
    gluvv.light.pj[8] = 0;
    gluvv.light.pj[9] = 0;
    gluvv.light.pj[10] = 0;
    gluvv.light.pj[11] = 1.0f / (d0);
    gluvv.light.pj[12] = 0;
    gluvv.light.pj[13] = 0;
    gluvv.light.pj[14] = 0;
    gluvv.light.pj[15] = 1;
    //cerr << "projection:" << endl;
    //printMatrix(gluvv.light.pj);
    //look at/ modelview
    gl.glPushMatrix();
    { //joe, why are you so damn lazy??? I will never know
      gl.glLoadIdentity();
      float[] ltdir = new float[3];
      math.copyV3(ltdir, gluvv.light.pos);
      math.negateV3(ltdir);
      math.normalizeV3(ltdir);

      glu.gluLookAt(ltdir[0], //eye
                ltdir[1],
                ltdir[2],
                0, //at
                0,
                0,
                0, //up
                1,
                0);
      gl.glGetFloatv(GL.GL_MODELVIEW_MATRIX, gluvv.light.mv, 0); //save modelview matrix
      gluvv.light.mv[14] = -gluvv.light.mv[14];
    }
    gl.glPopMatrix();
    //cerr << "lookat" << endl;
    //printMatrix(gluvv.light.mv);

    //the whole transform
    math.matrixMult(gluvv.light.xf, gluvv.light.pj, gluvv.light.mv);

    gluvv.light.load = 1;

    //cerr << "the transform:" << endl;
    //printMatrix(gluvv.light.xf);
  }

  // Methods required for the implementation of MouseListener
  public void mouseEntered(MouseEvent e) {}

  public void mouseExited(MouseEvent e) {}

  public void mouseMoved(MouseEvent e) {}

  public void mouseReleased(MouseEvent e) {
    dragFlag = false;
    int button = e.getButton();

    if ( (button == MouseEvent.BUTTON1) || (button == MouseEvent.BUTTON3)) {
          gluvv.light.gload = 1;
        }
        gluvv.volren.sampleRate = gluvv.volren.goodSamp;
      // glutPostRedisplay();
      // glutSetWindowTitle("Light Widget");
  }

  public void mouseClicked(MouseEvent e) {}


  public void mousePressed(MouseEvent e) {
    int x, y;
    int button = e.getButton();
    x = e.getX();
    y = e.getY();

      lastwinpos[0] = x;
      lastwinpos[1] = y;
      if (button == MouseEvent.BUTTON1) {
        float sx = (float) ( (gluvv.win.width - x) / (float) gluvv.win.width - .5 -
                            screenpos[0]) / screenrad;
        float sy = (float) ( (gluvv.win.height - y) / (float) gluvv.win.width -
                            .5 - screenpos[1]) / screenrad;
        sx = -sx * .5f;
        sy = -sy * .5f;
        tball.start(sx, sy);
        // glutSetWindowTitle("Light Widget - rotate");
      }
      else if (button == MouseEvent.BUTTON3) {
        // glutSetWindowTitle("Light Widget - translate");
      }
      else if (button == MouseEvent.BUTTON2) {
        // glutSetWindowTitle("Light Widget - radius");
      }


  }

  public void mouseDragged(MouseEvent e) {
    int x, y;
    int button = e.getButton();
    x = e.getX();
    y = e.getY();

    dragFlag = true;
    //              Drawable ???????????????????????????????????????????????????????????????????????
    // GL gl = drawable.getGL();
    float sdx = (float) (x - lastwinpos[0]) / (float) gluvv.win.width;
    float sdy = (float) (y - lastwinpos[1]) / (float) gluvv.win.height;

    if (gluvv.mouse.button == MouseEvent.BUTTON1) {
      x = x > (int) gluvv.win.width ? gluvv.win.width : (x < 0 ? 0 : x);
      y = y > (int) gluvv.win.height ? gluvv.win.height : (y < 0 ? 0 : y);
      float sx = (float) ( (gluvv.win.width - x) / (float) gluvv.win.width - .5 -
                          screenpos[0]) / screenrad;
      float sy = (float) ( (gluvv.win.height - y) / (float) gluvv.win.width - .5 -
                          screenpos[1]) / screenrad;
      sx = -sx * .5f;
      sy = -sy * .5f;
      tball.update(sx, sy);
      float[] lxf = new float[16];
      tball.buildRotMatrix(lxf);
      math.normalizeV3(gluvv.light.startpos);
      math.scaleV3(math.normV3(gluvv.light.pos), gluvv.light.startpos);
      math.translateV3(gluvv.light.pos, lxf, gluvv.light.startpos);
      // gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, gluvv.light.pos, 0);
      // genXForm(drawable);

      gluvv.volren.sampleRate = gluvv.volren.interactSamp;
      // glutSetWindowTitle("Light Widget: Rotate light");
      // glutPostRedisplay();
    }
    else if (gluvv.mouse.button == MouseEvent.BUTTON3) {
      if (gluvv.mouse.ctrl == 1 || gluvv.mouse.shift == 1) {
        gluvv.light.intens = math.CLAMP(gluvv.light.intens - sdy);
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        // glutSetWindowTitle("Light Widget: Intensity");
        // glutPostRedisplay();
        System.err.println("light intensity: " + gluvv.light.intens);
        gluvv.light.load = 1;
      }
      else {
        screenpos[0] -= sdx;
        screenpos[1] -= sdy;
        gluvv.volren.sampleRate = gluvv.volren.interactSamp;
        // glutSetWindowTitle("Light Widget: MOVE");
        // glutPostRedisplay();
      }
    }
    else if (gluvv.mouse.button == MouseEvent.BUTTON2) {
      if (sdy < 0) {
        math.scaleV3(1.0f + sdy, gluvv.light.pos);
      }
      else {
        math.scaleV3(1.0f - sdy, gluvv.light.pos);
      }
      // glutSetWindowTitle("Light Widget: RADIUS");
      System.err.println("light rad = " + math.normV3(gluvv.light.pos));
      // genXForm(drawable);            // ??????????????????????????????????????????
      // glutPostRedisplay();
    }

    lastwinpos[0] = x;
    lastwinpos[1] = y;


  }

}

