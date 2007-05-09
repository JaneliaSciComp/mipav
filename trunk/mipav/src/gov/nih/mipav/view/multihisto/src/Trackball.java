package gov.nih.mipav.view.multihisto.src;

public class Trackball {

  private static float trackballSize = 0.9f;
  private static int renormCount = 97;

  private float[] curQuat = new float[4];
  private float[] lastQuat = new float[4];
  private float lastX;
  private float lastY;
  static int count;

  public Trackball() {
    lastX = 0.0f;
    lastY = 0.0f;
    vZero(lastQuat);
    vZero(curQuat);
    lastQuat[3] = 1.0f;
    curQuat[3] = 1.0f;
  }

  public void start(float x, float y) {
    lastX = x;
    lastY = y;
    vZero(lastQuat);
    lastQuat[3] = 1.0f;
  }

// Pass the x and y coordinates of the last and current positions of
// the mouse, scaled so they are from (-1.0 ... 1.0).
//
// The resulting rotation is returned as a quaternion rotation in the
// first paramater.
  public void update(float x, float y) {
    float[] a = new float[3]; /* Axis of rotation */
    float phi;  /* how much to rotate about axis */
    float[] p1 = new float[3];
    float[] p2 = new float[3];
    float[] d  = new float[3];
    float t;

    //  Might just be able to return here.
    if (lastX == x && lastY == y) {
      /* Zero rotation */
      vZero(lastQuat);
      lastQuat[3] = 1.0f;
      return;
    }

    /*
     * First, figure out z-coordinates for projection of P1 and P2 to
     * deformed sphere
     */
    vSet(p1, lastX, lastY, projectToSphere(trackballSize, lastX, lastY));
    vSet(p2, x,     y,     projectToSphere(trackballSize,  x,    y));

    /*
     *  Now, we want the cross product of P1 and P2
     */
    vCross(p2, p1, a);

    /*
     *  Figure out how much to rotate around that axis.
     */
    vSub(p1,p2,d);
    t = (float)(vLength(d) / (2.0*trackballSize));

    /*
     * Avoid problems with out-of-control values...
     */
    if (t > 1.0) t = 1.0f;
    if (t < -1.0) t = -1.0f;
    phi = (float)(2.0 * Math.asin(t));

    axisToQuat(a, phi, lastQuat);

    lastX = x;
    lastY = y;

    addQuats(lastQuat, curQuat, curQuat);
  }

// Given two quaternions, add them together to get a third quaternion.
// Adding quaternions to get a compound rotation is analagous to adding
// translations to get a compound translation.  When incrementally
// adding rotations, the first argument here should be the new
// rotation, the second and third the total rotation (which will be
// over-written with the resulting new total rotation).
  public void addQuats(float[] q1, float[] q2, float[] dest) {
    count=0;                 // static ??????????????????
    float[] t1 = new float[4];
    float[] t2 = new float[4];
    float[] t3 = new float[4];
    float[] tf = new float[4];

    vCopy(q1,t1);
    vScale(t1,q2[3]);

    vCopy(q2,t2);
    vScale(t2,q1[3]);

    vCross(q2,q1,t3);
    vAdd(t1,t2,tf);
    vAdd(t3,tf,tf);
    tf[3] = q1[3] * q2[3] - vDot(q1,q2);

    dest[0] = tf[0];
    dest[1] = tf[1];
    dest[2] = tf[2];
    dest[3] = tf[3];

    if (++count > renormCount) {
      count = 0;
      normalizeQuat(dest);
    }
  }

// A useful function, builds a rotation matrix in Matrix based on
// given quaternion.
  public void buildRotMatrix(float result[]) {
    float[][] m = new float[4][4];
    result[0] = m[0][0] = (float)(1.0 - 2.0 * (curQuat[1] * curQuat[1] + curQuat[2] * curQuat[2]));
    result[1] = m[0][1] = (float)(2.0 * (curQuat[0] * curQuat[1] - curQuat[2] * curQuat[3]));
    result[2] = m[0][2] = (float)(2.0 * (curQuat[2] * curQuat[0] + curQuat[1] * curQuat[3]));
    result[3] = m[0][3] = (float)(0.0);

    result[4] = m[1][0] = (float)(2.0 * (curQuat[0] * curQuat[1] + curQuat[2] * curQuat[3]));
    result[5] = m[1][1]= (float)(1.0 - 2.0 * (curQuat[2] * curQuat[2] + curQuat[0] * curQuat[0]));
    result[6] = m[1][2] = (float)(2.0 * (curQuat[1] * curQuat[2] - curQuat[0] * curQuat[3]));
    result[7] = m[1][3] = (float)(0.0);

    result[8] = m[2][0] = (float)(2.0 * (curQuat[2] * curQuat[0] - curQuat[1] * curQuat[3]));
    result[9] = m[2][1] = (float)(2.0 * (curQuat[1] * curQuat[2] + curQuat[0] * curQuat[3]));
    result[10] = m[2][2] = (float)(1.0 - 2.0 * (curQuat[1] * curQuat[1] + curQuat[0] * curQuat[0]));
    result[11] = m[2][3] = (float)(0.0);

    result[12] = m[3][0] = (float)(0.0);
    result[13] = m[3][1] = (float)(0.0);
    result[14] = m[3][2] = (float)(0.0);
    result[15] = m[3][3] = (float)(1.0);
  }

// This function computes a quaternion based on an axis (defined by
// the given vector) and an angle about which to rotate.  The angle is
// expressed in radians.  The result is put into the third argument.
  public void axisToQuat(float[] a, float phi, float[] q) {
    vNormal(a);
    vCopy(a,q);
    vScale(q,(float)(Math.sin(phi/2.0)));
    q[3] = (float)(Math.cos(phi/2.0));
  }

  public void reapply() {
    addQuats(lastQuat, curQuat, curQuat);
  }

// This function was added by joe kniss to facilitate a home key!
  public void clear() {
    lastX = 0.0f;
    lastY = 0.0f;
    vZero(lastQuat);
    vZero(curQuat);
    lastQuat[3] = 1.0f;
    curQuat[3] = 1.0f;
  }

// This funciton allow arbitary views
  public void setMatrix(float[] rot) {

  }

  /*****************************   Private ***********************************/
  private float projectToSphere(float r, float x, float y) {
    float d, t, z;

    d = (float)(Math.sqrt(x*x + y*y));
    if (d < r * 0.70710678118654752440) {    /* Inside sphere */
      z = (float)(Math.sqrt(r*r - d*d));
    } else {           /* On hyperbola */
      t = (float)(r / 1.41421356237309504880);
      z = t*t / d;
    }
  return z;
  }

  private void  normalizeQuat(float[] q) {
    float  mag = (q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3]);
    for (int i = 0; i < 4; i++)
      q[i] /= mag;
  }

  //  Vector math.  Have to move somewhere else eventually.
  private void   vZero(float[] v) {
    v[0] = 0.0f;
    v[1] = 0.0f;
    v[2] = 0.0f;
  }

  private void   vSet(float[] v, float x, float y, float z) {
    v[0] = x;
    v[1] = y;
    v[2] = z;
  }

  private void vSub(float[] src1, float[] src2, float[] dst) {
    dst[0] = src1[0] - src2[0];
    dst[1] = src1[1] - src2[1];
    dst[2] = src1[2] - src2[2];
  }

  private void   vCopy(float[] v1, float[] v2) {
    for (int i = 0 ; i < 3 ; i++)
      v2[i] = v1[i];
  }

  private void   vCross(float[] v1, float[] v2, float[] cross) {
    float[] temp = new float[3];
    temp[0] = (v1[1] * v2[2]) - (v1[2] * v2[1]);
    temp[1] = (v1[2] * v2[0]) - (v1[0] * v2[2]);
    temp[2] = (v1[0] * v2[1]) - (v1[1] * v2[0]);
    vCopy(temp, cross);
  }

  private  float  vLength(float[] v) {
    return (float)(Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]));
  }

  private void   vScale(float[] v, float div) {
   v[0] *= div;
   v[1] *= div;
   v[2] *= div;
  }

  private void  vNormal(float[] v) {
    vScale(v,(float)(1.0f/vLength(v)));
  }

  private float  vDot(float[] v1, float[] v2) {
    return v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2];
  }

  private void   vAdd(float[] src1, float[] src2, float[] dst) {
    dst[0] = src1[0] + src2[0];
    dst[1] = src1[1] + src2[1];
    dst[2] = src1[2] + src2[2];
  }


}
