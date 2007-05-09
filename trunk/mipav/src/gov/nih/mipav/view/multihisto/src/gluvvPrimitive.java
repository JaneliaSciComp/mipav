package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;

public class gluvvPrimitive {

  private gluvvPrimitive next;
  private String name;

  public gluvvPrimitive() {
      next = null;
      name = null;
  }

  public void init() {}

  public void draw(){}

  public int key(char k, int x, int y)
  {
    return 0;
  }

  public int special(int k, int x, int y)
  {
    return 0;
  }

  public int pick(int data1, int data2, int data3, float x, float y, float z)
  {
    return 1;
  }

  public int pick()
  {
    return 1;
  }

  public int mouse(int button, int state, int x, int y)
  {
    return 1;
  }


  public int move(int x, int y)
  {
    return 1;
  }

  public int release()
  {
    return 0;
  }

  public void setNext(gluvvPrimitive p)
  {
    p.next = next;
    next = p;
  }


  public gluvvPrimitive getNext()
  {
    return next;
  }

  public void remove(gluvvPrimitive p)
  {
    if(next == p)
      next = p.next;
    else
      next.remove(p);
  }

  public void setName(String PName)
  {
    name = PName;
  }

  public void gluvvPushName(GLAutoDrawable drawable)
  {
    GL gl = drawable.getGL();
    // ??????????????
    gl.glPushName(this.hashCode());
    // System.err.println("test = " + this.hashCode());
  }

  public void gluvvPopNames(GLAutoDrawable drawable)
  {
   GL gl = drawable.getGL();
   gl.glPopName();
   gl.glPopName();
   gl.glPopName();
   gl.glPopName();
  }

}
