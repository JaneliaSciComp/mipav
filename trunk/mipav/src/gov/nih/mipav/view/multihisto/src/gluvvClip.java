package gov.nih.mipav.view.multihisto.src;

public class gluvvClip {
  //clipping plane widget
  public int on; //flag for clip on(1) or off(0).
  public int ortho; //ortho graphic mode (axis aligned)
  public int oaxis; //ortho graphic axis to render along
  public float[] xform = new float[16]; //float xform[16], orientation of clip plane
  public int[] pname = new int[1]; //unsigned int pname, name of plane
  public float alpha; //how to blend it with the volume
  public float[] pos = new float[3]; //float pos[3], position of clip plane (world coords)
  public float[] vpos = new float[3]; //  float vpos[3],    '' in volume space
  public float[] dir = new float[3]; //direction of clip plane normal
  public float[] mousepnt = new float[3]; //a selected point
  public float[][] corners = new float[4][3]; //corners[4][3], corners of plane widget

  public gluvvClip() {
  }

}
