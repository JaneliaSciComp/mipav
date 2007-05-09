
package gov.nih.mipav.view.gpu.src;

import java.io.*;
import javax.media.opengl.*;
import com.sun.opengl.util.*;
import java.awt.*;
import java.util.*;
import java.lang.*;

public class TransferEdit {
  public static int NUM_TE = 5;
  public static int NUM_TE_ENTRIES = 256;

  public static int BOXSIZE = 8;
  public static int CONST_VAL = 20;
  public static int SHIFT_VAL_STEP = 4;

  /* Maximum value that can be returned by the rand function. */

  public static int RAND_MAX = 0x7fff;

  byte[][] dataTE;

  boolean focus;
  boolean hidden;
  int drawOffsetX;
  int drawOffsetY;
  int[] viewport = new int[4];
  int[] activeCh;
  float[][] colorTE = {
      {
      1.0f, 0.0f, 0.0f}, {
      0.0f, 1.0f, 0.0f}, {
      0.0f, 0.0f, 1.0f}, {
      1.0f, 1.0f, 1.0f}, {
      0.0f, 1.0f, 1.0f}
  };
  int[] lastMpos = new int[2];
  boolean RANDOM = false;

  public TransferEdit() {

  }

  void initTE(GLAutoDrawable drawable) {
    int idx, idi;
    GL gl = drawable.getGL();

    dataTE = new byte[NUM_TE][];
    activeCh = new int[NUM_TE];
    for (idx = 0; idx < NUM_TE; idx++) {
      dataTE[idx] = new byte[NUM_TE_ENTRIES];
      activeCh[idx] = 1;
    }

    /* Init TFs */
    for (idx = 0; idx < NUM_TE; idx++) {
      for (idi = 0; idi < NUM_TE_ENTRIES; idi++) {
        /* Linear transfer function */
        if ( !RANDOM ) {
          if (idx < 4) {
            dataTE[idx][idi] = new Integer(idi).byteValue();
          }
          else {
            dataTE[idx][idi] = new Integer(128).byteValue();
          }
        } else {
          dataTE[idx][idi] = new Float( (float) Math.random() / RAND_MAX * (NUM_TE_ENTRIES - 1)).byteValue();
        }
      }
    }

    drawOffsetX = 10;
    drawOffsetY = 10;
    hidden = true;
    focus = false;

    gl.glGetIntegerv(GL.GL_VIEWPORT, viewport, 0);
  }

  public void destTE() {
    int idx;

    for (idx = 0; idx < NUM_TE; idx++) {
      if (dataTE[idx] != null) {
        dataTE[idx] = null;
      }
    }
    dataTE = null;
  }

  public void drawTE(GLAutoDrawable drawable) {
    int idx, idi;
    int val;
    GL gl = drawable.getGL();

    if (hidden) {
      return;
    }

    gl.glPushAttrib(GL.GL_ENABLE_BIT | GL.GL_COLOR_BUFFER_BIT | GL.GL_TRANSFORM_BIT);

    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glPushMatrix();
    gl.glLoadIdentity();
    gl.glOrtho(0, viewport[2], 0, viewport[3], -1., 1.);

    gl.glMatrixMode(GL.GL_MODELVIEW);
    gl.glPushMatrix();
    gl.glLoadIdentity();
    gl.glTranslatef( (float) drawOffsetX, (float) drawOffsetY, 0.0f);

    gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
    gl.glEnable(GL.GL_BLEND);
    gl.glBegin(GL.GL_QUADS);
    gl.glColor4f(0.0f, 0.0f, 0.0f, 0.2f);
    gl.glVertex2f(0.0f, 0.0f);
    gl.glVertex2f(255.0f, 0.0f);
    gl.glVertex2f(255.0f, 255.0f);
    gl.glVertex2f(0.0f, 255.0f);
    gl.glEnd();

    gl.glBegin(GL.GL_QUADS);
    for (idx = 0; idx < NUM_TE; idx++) {
      if (activeCh[idx] == 1) {
        gl.glColor3fv(colorTE[idx], 0);
        gl.glVertex2i(idx * 2 * BOXSIZE + BOXSIZE, 8);
        gl.glVertex2i(idx * 2 * BOXSIZE + 2 * BOXSIZE, 8);
        gl.glVertex2i(idx * 2 * BOXSIZE + 2 * BOXSIZE, 8 + BOXSIZE);
        gl.glVertex2i(idx * 2 * BOXSIZE + BOXSIZE, 8 + BOXSIZE);
      }
    }
    gl.glEnd();

    gl.glDisable(GL.GL_BLEND);

    for (idx = 0; idx < NUM_TE; idx++) {
      gl.glBegin(GL.GL_LINE_STRIP);
      gl.glColor3fv(colorTE[idx], 0);
      for (idi = 0; idi < NUM_TE_ENTRIES; idi++) {
        val = new Byte(dataTE[idx][idi]).intValue();
        val &= 0x000000ff;
        gl.glVertex2i(idi,  val);
      }
      gl.glEnd();
    }

    gl.glPopMatrix();
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glPopMatrix();
    gl.glPopAttrib();
  }

  boolean mouseIsInside(int x, int y) {
    return (((drawOffsetX <= x) && (x <= drawOffsetX + NUM_TE_ENTRIES)) &&
                  ((drawOffsetY <= y) && (y <= drawOffsetY + NUM_TE_ENTRIES)));
  }

  int mouseTE(int x, int y) {
    if ( (hidden) || (!mouseIsInside(x, viewport[3] - y))) {
      focus = false;
      return 0;
    }

    lastMpos[0] = x - drawOffsetX;
    lastMpos[1] = viewport[3] - ( y + drawOffsetY );
    focus = true;
    return 1;
  }

  int motionTE(int x, int y) {
    int[] actuMpos = new int[2];
    int startY, i, idt;
    float m;

    if ( (hidden) || (!focus)) {
      return 0;
    }

    actuMpos[0] = x - drawOffsetX;
    actuMpos[1] = viewport[3] - ( y + drawOffsetY );

    /* clamping */
    for (i = 0; i < 2; i++) {
      if (actuMpos[i] < 0) {
        actuMpos[i] = 0;
      }
      if (actuMpos[i] > (NUM_TE_ENTRIES - 1)) {
        actuMpos[i] = NUM_TE_ENTRIES - 1;
      }
    }

    if (actuMpos[0] < lastMpos[0]) {
      m = (lastMpos[1] - actuMpos[1]) / (float) (lastMpos[0] - actuMpos[0]);
      startY = actuMpos[1];
    }
    else {
      m = (actuMpos[1] - lastMpos[1]) / (float) (actuMpos[0] - lastMpos[0]);
      startY = lastMpos[1];
    }

    for (idt = 0; idt < NUM_TE; idt++) {
      if (activeCh[idt] == 1) {
        int idx;
        for (idx = 0; idx <= Math.abs(actuMpos[0] - lastMpos[0]); idx++) {
          dataTE[idt][Math.min(actuMpos[0], lastMpos[0]) + idx] =
              new Integer( ( startY + (int) Math.floor(idx * m) ) & 0x000000ff ).byteValue();
        }
      }
    }
    lastMpos[0] = actuMpos[0];
    lastMpos[1] = actuMpos[1];

    return 1;
  }

  void setIdentity() {
    int i, j;
    for (i = 0; i < NUM_TE_ENTRIES; i++) {
      for (j = 0; j < NUM_TE; j++) {
        if (activeCh[j] == 1) {
          dataTE[j][i] = new Integer(i).byteValue();
        }
      }
    }
  }

  void setInvert() {
    int i, j;
    int val;
    for (i = 0; i < NUM_TE_ENTRIES; i++) {
      for (j = 0; j < NUM_TE; j++) {
        if (activeCh[j] == 1) {
          val = new Byte(dataTE[j][i]).intValue();
          dataTE[j][i] = new Integer(NUM_TE_ENTRIES - 1 - val).byteValue();
        }
      }
    }
  }

  void setConst() {
    int i, j;

    for (i = 0; i < NUM_TE_ENTRIES; i++) {
      for (j = 0; j < NUM_TE; j++) {
        if (activeCh[j] == 1) {
          dataTE[j][i] = new Integer(CONST_VAL).byteValue();
        }
      }
    }
  }

  void shiftFunc(int delta) {
    int i, j, temp;
    int val;
    for (i = 0; i < NUM_TE_ENTRIES; i++) {
      for (j = 0; j < NUM_TE; j++) {
        if (activeCh[j] == 1) {
          val = new Byte(dataTE[j][i]).intValue();
          temp = val + delta;
          if (temp < 0) {
            dataTE[j][i] = new Integer(0).byteValue();
          }
          else if (temp > 255) {
            dataTE[j][i] = new Integer(255).byteValue();
          }
          else {
            dataTE[j][i] = new Integer(temp).byteValue();
          }
        }
      }
    }
  }

  int keyTE(char k) {
    if ( (hidden) ) {
      return 0;
    }

    switch (k) {
      case 'r':
        activeCh[0] = 1 - activeCh[0];
        break;
      case 'g':
        activeCh[1] = 1 - activeCh[1];
        break;
      case 'b':
        activeCh[2] = 1 - activeCh[2];
        break;
      case 'a':
        activeCh[3] = 1 - activeCh[3];
        break;
      case 'o':
        activeCh[4] = 1 - activeCh[4];
        break;
      case 't':
        toggleHidden();
        break;
      case 'i':
        setIdentity();
        break;
      case 'm':
        setInvert();
        break;
      case 'c':
        setConst();
        break;
      case 'h':
        shiftFunc(SHIFT_VAL_STEP);
        break;
      case 'n':
        shiftFunc( -SHIFT_VAL_STEP);
        break;
      default:
        return 0;
    }

    return 1;
  }

  void setOffsets(int offsetX, int offsetY)
  {
          drawOffsetX = offsetX;
          drawOffsetY = offsetY;
  }

  void setHidden(boolean h)
  {
          hidden = h;
  }

  void toggleHidden()
  {
          hidden = !hidden;
  }

  void resizeTE(GLAutoDrawable drawable)
  {
       GL gl = drawable.getGL();
       gl.glGetIntegerv(GL.GL_VIEWPORT, viewport, 0);
  }

  byte[][] getDataTE()
  {
          return dataTE;
  }


}
