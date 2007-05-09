package gov.nih.mipav.view.multihisto.src;


public class gluvvEnv {
  //GL rendering Environment parameters
  public float[] eye = new float[3]; // eye[3] location
  public float[] at = new float[3]; // at[3] lookat point
  public float[] up = new float[3]; // up[3], up vector
  // frustum[0] = left,   frustum[1] = right
  public float[] frustum = new float[4]; //frustum[4],  frustum[2] = bottom, frustum[3] = top
  public float[] clip = new float[2]; // clip[2], clip[0] = front, clip[1] = back
  public float[] diff = new float[4]; // diff[4], diffuse color (material)
  public float[] spec = new float[4]; // spec[4], speculare color (material)
  public int bgColor; // what color is the back ground? 0==white 1==black

  public gluvvEnv() {

  }
}
