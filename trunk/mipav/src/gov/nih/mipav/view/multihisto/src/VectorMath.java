package gov.nih.mipav.view.multihisto.src;

public class VectorMath {

  /**
   * //matrix representation for mat[16], same as OpenGl matrix rep

     +-          -+
     |0  4  8   12|
     |1  5  9   13|
     |2  6  10  14|
     |3  7  11  15|
     +-          -+

   */

  public static float EPSILON = 2e-07f;
  public static float V_PI = 3.1415926535897932384626433832795f;

  public static short SHRT_MIN  = (-32768);        /* minimum (signed) short value */
  public static short SHRT_MAX  =   32767;         /* maximum (signed) short value */
  public static short USHRT_MAX    = (short)0xffff;        /* maximum unsigned short value */
  public static int INT_MIN    =  (-2147483647 - 1); /* minimum (signed) int value */
  public static int INT_MAX   =   2147483647;    /* maximum (signed) int value */
  public static int UINT_MAX  =   0xffffffff;    /* maximum unsigned int value */
  public static long LONG_MIN  =  (-2147483647L - 1); /* minimum (signed) long value */
  public static long LONG_MAX  =   2147483647L;   /* maximum (signed) long value */
  public static long ULONG_MAX   =  0xffffffff;  /* maximum unsigned long value */

  public VectorMath() {

  }

  /*
   ******** NRRD_AFFINE(i,x,I,o,O)
   **
   ** given intervals [i,I], [o,O] and a value x which may or may not be
   ** inside [i,I], return the value y such that y stands in the same
   ** relationship to [o,O] that x does with [i,I].  Or:
   **
   **    y - o         x - i
   **   -------   =   -------
   **    O - o         I - i
   **
   ** It is the callers responsibility to make sure I-i and O-o are
   ** both greater than zero
   */

  public float CLAMP(float x) {
    return ( ( ( (x) > 0) ? ( ( (x) < 1) ? x : 1) : 0));
  }

  public float MAX(float x, float y) {
    return ( ( (x) > (y)) ? (x) : (y));
  }

  public float MIN(float x, float y) {
    return ( ( (x) < (y)) ? (x) : (y));
  }

  public float ABS(float x) {
    return ( (x) < 0 ? ( -x) : (x));
  }

  public double CLAMP_ARB(double c, double x, double C) {
    return ( ( ( (x) > c) ? ( ( (x) < (C)) ? x : (C)) : c));
  }

  public double affine(double i, double x, double I, double o, double O) {
    return ( ( (O) - (o)) * ( (x) - (i)) / ( (I) - (i)) + (o));
  }

  // make a vector all zeros -----------------------------------------
  public void zeroV3(float[] in) {
    in[0] = in[1] = in[2] = 0;
  }

  // make a vector all zeros -----------------------------------------
  public void zeroV3(float[] in, int index) {
    in[0+index] = in[1+index] = in[2+index] = 0;
  }


  public void zeroV3(double[] in) {
    in[0] = in[1] = in[2] = 0;
  }

// negate all components of a vector -------------------------------
  public void negateV3(float[] in) {
    in[0] = -in[0];
    in[1] = -in[1];
    in[2] = -in[2];
  }

  public void negateV3(double[] in) {
    in[0] = -in[0];
    in[1] = -in[1];
    in[2] = -in[2];
  }

  // copy vector 'out' = 'in';---------------------------------------
  public void copyV3(float[] out, float[] in) {
    out[0] = in[0];
    out[1] = in[1];
    out[2] = in[2];
  }

  // copy vector 'out' = 'in';---------------------------------------
  public void copyV3(float[] out, int oIndex, float[] in, int iIndex) {
    out[0+oIndex] = in[0+iIndex];
    out[1+oIndex] = in[1+iIndex];
    out[2+oIndex] = in[2+iIndex];
  }


  public void copyV3(double[] out, double[] in) {
    out[0] = in[0];
    out[1] = in[1];
    out[2] = in[2];
  }

  public void copyV3(float[] out, double[] in) {
    out[0] = (float) in[0];
    out[1] = (float) in[1];
    out[2] = (float) in[2];
  }

  public void copyV3(double[] out, float[] in) {
    out[0] = in[0];
    out[1] = in[1];
    out[2] = in[2];
  }

  // out = inl - inr -----------------------------------------------
  public void subV3(float[] out, float[] inl, float[] inr) {
    out[0] = inl[0] - inr[0];
    out[1] = inl[1] - inr[1];
    out[2] = inl[2] - inr[2];
  }

  public void subV3(double[] out, double[] inl, double[] inr) {
    out[0] = inl[0] - inr[0];
    out[1] = inl[1] - inr[1];
    out[2] = inl[2] - inr[2];
  }

// out = inl + inr ---------------------------------------------
  public void addV3(float[] out, float[] inl, float[] inr) {
    out[0] = inl[0] + inr[0];
    out[1] = inl[1] + inr[1];
    out[2] = inl[2] + inr[2];
  }

  public void addV3(double[] out, double[] inl, double[] inr) {
    out[0] = inl[0] + inr[0];
    out[1] = inl[1] + inr[1];
    out[2] = inl[2] + inr[2];
  }

  // one *= s --------------------------------------------------
  public void scaleV3(float s, float[] one) {
    one[0] *= s;
    one[1] *= s;
    one[2] *= s;
  }

  // one *= s --------------------------------------------------
  public void scaleV3(float s, float[] one, int index) {
    one[0+index] *= s;
    one[1+index] *= s;
    one[2+index] *= s;
  }

  public void scaleV3(double s, double[] one) {
    one[0] *= s;
    one[1] *= s;
    one[2] *= s;
  }

// out = in * s --------------------------------------------
  public void cscaleV3(float[] out, float s, float[] in) {
    out[0] = s * in[0];
    out[1] = s * in[1];
    out[2] = s * in[2];
  }

  public void cscaleV3(double[] out, double s, double[] in) {
    out[0] = s * in[0];
    out[1] = s * in[1];
    out[2] = s * in[2];
  }

  // set a matrix 'm' to the identity -----------------------
  public void identityMatrix(float[] m) {
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

  public void identityMatrix(double[] m) {
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

// vector matrix multiplication [4] vector ----------------------
// out = mat * in
  public void translateV4(float[] out, float[] mat, float[] in) {
    out[0] = mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2] + mat[12] * in[3];
    out[1] = mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2] + mat[13] * in[3];
    out[2] = mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2] + mat[14] * in[3];
    out[3] = mat[3] * in[0] + mat[7] * in[1] + mat[11] * in[2] + mat[15] * in[3];
  }

//3 vector with implict w=1 mult matrix to 4 vector
  public void translateV4_3W(float[] out, float[] mat, float[] in) {
    out[0] = mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2] + mat[12];
    out[1] = mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2] + mat[13];
    out[2] = mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2] + mat[14];
    out[3] = mat[3] * in[0] + mat[7] * in[1] + mat[11] * in[2] + mat[15];
  }

  public void translateV4(double[] out, double[] mat, double[] in) {
    out[0] = mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2] + mat[12] * in[3];
    out[1] = mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2] + mat[13] * in[3];
    out[2] = mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2] + mat[14] * in[3];
    out[3] = mat[3] * in[0] + mat[7] * in[1] + mat[11] * in[2] + mat[15] * in[3];
  }

// vector matrix multiplicaiton [3] vector with no translation ---
// (only rotation) out = mat * in;
  public void translateV3(float[] out, float[] mat, float[] in) {
    out[0] = mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2]; // + mat[12];
    out[1] = mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2]; // + mat[13];
    out[2] = mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2]; // + mat[14];
  }

  public void translateV3(double[] out, double[] mat, double[] in) {
    out[0] = mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2]; // + mat[12];
    out[1] = mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2]; // + mat[13];
    out[2] = mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2]; // + mat[14];
  }

  public void translateV3(float[] out, double[] mat, float[] in) {
    out[0] = (float) (mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2]); // + mat[12];
    out[1] = (float) (mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2]); // + mat[13];
    out[2] = (float) (mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2]); // + mat[14];
  }

  // [3] vector * matrix --------------------------------------------
// out = mat * in (with translation)
  public void translateV3W(float[] out, float[] mat, float[] in) {
    out[0] = mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2] + mat[12];
    out[1] = mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2] + mat[13];
    out[2] = mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2] + mat[14];
  }

  public void translateV3W(float[] out, double[] mat, float[] in) {
    out[0] = (float) (mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2] + mat[12]);
    out[1] = (float) (mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2] + mat[13]);
    out[2] = (float) (mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2] +
                      mat[14]);
  }

  public void translateV3W(double[] out, double[] mat, double[] in) {
    out[0] = mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2] + mat[12];
    out[1] = mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2] + mat[13];
    out[2] = mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2] + mat[14];
  }

//transformation of 3 vector with implicit w=1 and homoginization
  public void translateV3WD(float[] out, float[] mat, float[] in) {
    out[0] = (float) (mat[0] * in[0] + mat[4] * in[1] + mat[8] * in[2] + mat[12]);
    out[1] = (float) (mat[1] * in[0] + mat[5] * in[1] + mat[9] * in[2] + mat[13]);
    out[2] = (float) (mat[2] * in[0] + mat[6] * in[1] + mat[10] * in[2] +
                      mat[14]);
    float w = (float) (mat[3] * in[0] + mat[7] * in[1] + mat[11] * in[2] +
                       mat[15]);
    out[0] /= w;
    out[1] /= w;
    out[2] /= w;
  }

  // legacy call
  public void transMatrixV3(float[] out, float[] mat, float[] in)
  {

     out[0] = mat[0]*in[0] + mat[4]*in[1] + mat[8]* in[2] + mat[12];
     out[1] = mat[1]*in[0] + mat[5]*in[1] + mat[9]* in[2] + mat[13];
     out[2] = mat[2]*in[0] + mat[6]*in[1] + mat[10]*in[2] + mat[14];
  }

// dot product of two [4] vecotrs --------------------------
  public float dotV4(float[] one, float[] two)
  {
     return one[0]*two[0] + one[1]*two[1] + one[2]*two[2] + one[3]*two[3];
  }

  public double dotV4(double[] one, double[] two)
  {
     return one[0]*two[0] + one[1]*two[1] + one[2]*two[2] + one[3]*two[3];
  }

// dot product of two [3] vectors ------------------------
  public float dotV3(float[] one, float[] two)
  {
     return one[0]*two[0] + one[1]*two[1] + one[2]*two[2];
  }

  public double dotV3(double[] one, double[] two)
  {
     return one[0]*two[0] + one[1]*two[1] + one[2]*two[2];
  }

// compute the length of a [3] vector --------------------
  public float normV3(float[] one)
  {
     return (float)Math.sqrt( one[0]*one[0] + one[1]*one[1] + one[2]*one[2]);
  }

  public float normV3(float[] one, int index)
  {
     return (float)Math.sqrt( one[0+index]*one[0+index] + one[1+index]*one[1+index] + one[2+index]*one[2+index]);
  }



  public double normV3(double[] one)
  {
     return Math.sqrt( one[0]*one[0] + one[1]*one[1] + one[2]*one[2]);
  }

// normalize a [4] vector --------------------------------
  public void normalizeV4(float[] v)
  {
     float len = (float)Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2] + v[3]*v[3]);
     if(len > 0){
        v[0] /= len;
        v[1] /= len;
        v[2] /= len;
        v[3] /= len;
     }
  }

  public void normalizeV4(double[] v)
  {
     float len = (float)Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2] + v[3]*v[3]);
     if(len > 0){
        v[0] /= len;
        v[1] /= len;
        v[2] /= len;
        v[3] /= len;
     }
  }

// normalize a [3] vector ---------------------------------
  public void normalizeV3(float[] v)
  {
     float len = (float)Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
     if(len > 0){
        v[0] /= len;
        v[1] /= len;
        v[2] /= len;
     }
  }

  public void normalizeV3(float[] v, int index)
  {
     float len = (float)Math.sqrt(v[0+index]*v[0+index] + v[1+index]*v[1+index] + v[2+index]*v[2+index]);
     if(len > 0){
        v[0+index] /= len;
        v[1+index] /= len;
        v[2+index] /= len;
     }
  }

  public void normalizeV3(double[] v)
  {
     float len = (float)Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
     if(len > 0){
        v[0] /= len;
        v[1] /= len;
        v[2] /= len;
     }
  }

// divide out the 'W' part of a [4] vector ----------------
  public void homogV4(float[] v)
  {
     v[0] /= v[3];
     v[1] /= v[3];
     v[2] /= v[3];
     v[3] = 1;
  }

  public void homogV4(double[] v)
  {
     v[0] /= v[3];
     v[1] /= v[3];
     v[2] /= v[3];
     v[3] = 1;
  }

// compute cross product of [3] vector -------------------
// out = one X two;
  public void crossV3(float[] out, float[] one, float[] two)
  {
     out[0] = one[1]*two[2] - one[2]*two[1];
     out[1] = one[2]*two[0] - one[0]*two[2];
     out[3] = one[0]*two[1] - one[1]*two[0];
  }

  public void crossV3(double[] out, double[] one, double[] two)
  {
     out[0] = one[1]*two[2] - one[2]*two[1];
     out[1] = one[2]*two[0] - one[0]*two[2];
     out[3] = one[0]*two[1] - one[1]*two[0];
  }

// copy two matricies -----------------------------------
  public void matrixEqual(float[] me, float[] m)
  {
     for(int i=0; i< 16; ++i) me[i] = m[i];
  }

  public void matrixEqual(double[] me, double[] m)
  {
     for(int i=0; i< 16; ++i) me[i] = m[i];
  }

// maxb = ma * mb --------------------------------------
  public void matrixMult( float[] maxb, float[] ma, float[] mb )
  {
    maxb[0] = ma[0]*mb[0] + ma[4]*mb[1] + ma[8]*mb[2] + ma[12]*mb[3];
    maxb[1] = ma[1]*mb[0] + ma[5]*mb[1] + ma[9]*mb[2] + ma[13]*mb[3];
    maxb[2] = ma[2]*mb[0] + ma[6]*mb[1] + ma[10]*mb[2] + ma[14]*mb[3];
    maxb[3] = ma[3]*mb[0] + ma[7]*mb[1] + ma[11]*mb[2] + ma[15]*mb[3];

    maxb[4] = ma[0]*mb[4] + ma[4]*mb[5] + ma[8]*mb[6] + ma[12]*mb[7];
    maxb[5] = ma[1]*mb[4] + ma[5]*mb[5] + ma[9]*mb[6] + ma[13]*mb[7];
    maxb[6] = ma[2]*mb[4] + ma[6]*mb[5] + ma[10]*mb[6] + ma[14]*mb[7];
    maxb[7] = ma[3]*mb[4] + ma[7]*mb[5] + ma[11]*mb[6] + ma[15]*mb[7];

    maxb[8] = ma[0]*mb[8] + ma[4]*mb[9] + ma[8]*mb[10] + ma[12]*mb[11];
    maxb[9] = ma[1]*mb[8] + ma[5]*mb[9] + ma[9]*mb[10] + ma[13]*mb[11];
    maxb[10] = ma[2]*mb[8] + ma[6]*mb[9] + ma[10]*mb[10] + ma[14]*mb[11];
    maxb[11] = ma[3]*mb[8] + ma[7]*mb[9] + ma[11]*mb[10] + ma[15]*mb[11];

    maxb[12] = ma[0]*mb[12] + ma[4]*mb[13] + ma[8]*mb[14] + ma[12]*mb[15];
    maxb[13] = ma[1]*mb[12] + ma[5]*mb[13] + ma[9]*mb[14] + ma[13]*mb[15];
    maxb[14] = ma[2]*mb[12] + ma[6]*mb[13] + ma[10]*mb[14] + ma[14]*mb[15];
    maxb[15] = ma[3]*mb[12] + ma[7]*mb[13] + ma[11]*mb[14] + ma[15]*mb[15];
  }

  public void matrixMult( double[] maxb, double[] ma, double[] mb )
  {
    maxb[0] = ma[0]*mb[0] + ma[4]*mb[1] + ma[8]*mb[2] + ma[12]*mb[3];
    maxb[1] = ma[1]*mb[0] + ma[5]*mb[1] + ma[9]*mb[2] + ma[13]*mb[3];
    maxb[2] = ma[2]*mb[0] + ma[6]*mb[1] + ma[10]*mb[2] + ma[14]*mb[3];
    maxb[3] = ma[3]*mb[0] + ma[7]*mb[1] + ma[11]*mb[2] + ma[15]*mb[3];

    maxb[4] = ma[0]*mb[4] + ma[4]*mb[5] + ma[8]*mb[6] + ma[12]*mb[7];
    maxb[5] = ma[1]*mb[4] + ma[5]*mb[5] + ma[9]*mb[6] + ma[13]*mb[7];
    maxb[6] = ma[2]*mb[4] + ma[6]*mb[5] + ma[10]*mb[6] + ma[14]*mb[7];
    maxb[7] = ma[3]*mb[4] + ma[7]*mb[5] + ma[11]*mb[6] + ma[15]*mb[7];

    maxb[8] = ma[0]*mb[8] + ma[4]*mb[9] + ma[8]*mb[10] + ma[12]*mb[11];
    maxb[9] = ma[1]*mb[8] + ma[5]*mb[9] + ma[9]*mb[10] + ma[13]*mb[11];
    maxb[10] = ma[2]*mb[8] + ma[6]*mb[9] + ma[10]*mb[10] + ma[14]*mb[11];
    maxb[11] = ma[3]*mb[8] + ma[7]*mb[9] + ma[11]*mb[10] + ma[15]*mb[11];

    maxb[12] = ma[0]*mb[12] + ma[4]*mb[13] + ma[8]*mb[14] + ma[12]*mb[15];
    maxb[13] = ma[1]*mb[12] + ma[5]*mb[13] + ma[9]*mb[14] + ma[13]*mb[15];
    maxb[14] = ma[2]*mb[12] + ma[6]*mb[13] + ma[10]*mb[14] + ma[14]*mb[15];
    maxb[15] = ma[3]*mb[12] + ma[7]*mb[13] + ma[11]*mb[14] + ma[15]*mb[15];
  }

// compute the inverse of a matrix
// invm = m^(-1)
  public void inverseMatrix( float[] invm, float[] m )
  {
    float det =
      m[0]*m[5]*m[10]-
      m[0]*m[6]*m[9]-
      m[1]*m[4]*m[10]+
      m[1]*m[6]*m[8]+
      m[2]*m[4]*m[9]-
      m[2]*m[5]*m[8];

    invm[0] = (m[5]*m[10]-m[6]*m[9])/det;
    invm[1] = (-m[1]*m[10]+m[2]*m[9])/det;
    invm[2] = (m[1]*m[6]-m[2]*m[5])/det;
    invm[3] = 0.0f;

    invm[4] = (-m[4]*m[10]+m[6]*m[8])/det;
    invm[5] = (m[0]*m[10]-m[2]*m[8])/det;
    invm[6] = (-m[0]*m[6]+m[2]*m[4])/det;
    invm[7] = 0.0f;

    invm[8] = (m[4]*m[9]-m[5]*m[8])/det;
    invm[9] = (-m[0]*m[9]+m[1]*m[8])/det;
    invm[10] = (m[0]*m[5]-m[1]*m[4])/det;
    invm[11] = 0.0f;

    invm[12] = (-m[4]*m[9]*m[14]+m[4]*m[13]*m[10]+
                m[5]*m[8]*m[14]-m[5]*m[12]*m[10]-
                m[6]*m[8]*m[13]+m[6]*m[12]*m[9])/det;
    invm[13] = (m[0]*m[9]*m[14]-m[0]*m[13]*m[10]-
                m[1]*m[8]*m[14]+m[1]*m[12]*m[10]+
                m[2]*m[8]*m[13]-m[2]*m[12]*m[9])/det;
    invm[14] = (-m[0]*m[5]*m[14]+m[0]*m[13]*m[6]+
                m[1]*m[4]*m[14]-m[1]*m[12]*m[6]-
                m[2]*m[4]*m[13]+m[2]*m[12]*m[5])/det;
    invm[15] = 1.0f;
  }

  public void inverseMatrix( double[] invm, double[] m )
  {
    float det = (float)(
      m[0]*m[5]*m[10]-
      m[0]*m[6]*m[9]-
      m[1]*m[4]*m[10]+
      m[1]*m[6]*m[8]+
      m[2]*m[4]*m[9]-
      m[2]*m[5]*m[8]);

    invm[0] = (m[5]*m[10]-m[6]*m[9])/det;
    invm[1] = (-m[1]*m[10]+m[2]*m[9])/det;
    invm[2] = (m[1]*m[6]-m[2]*m[5])/det;
    invm[3] = 0.0;

    invm[4] = (-m[4]*m[10]+m[6]*m[8])/det;
    invm[5] = (m[0]*m[10]-m[2]*m[8])/det;
    invm[6] = (-m[0]*m[6]+m[2]*m[4])/det;
    invm[7] = 0.0;

    invm[8] = (m[4]*m[9]-m[5]*m[8])/det;
    invm[9] = (-m[0]*m[9]+m[1]*m[8])/det;
    invm[10] = (m[0]*m[5]-m[1]*m[4])/det;
    invm[11] = 0.0;

    invm[12] = (-m[4]*m[9]*m[14]+m[4]*m[13]*m[10]+
                m[5]*m[8]*m[14]-m[5]*m[12]*m[10]-
                m[6]*m[8]*m[13]+m[6]*m[12]*m[9])/det;
    invm[13] = (m[0]*m[9]*m[14]-m[0]*m[13]*m[10]-
                m[1]*m[8]*m[14]+m[1]*m[12]*m[10]+
                m[2]*m[8]*m[13]-m[2]*m[12]*m[9])/det;
    invm[14] = (-m[0]*m[5]*m[14]+m[0]*m[13]*m[6]+
                m[1]*m[4]*m[14]-m[1]*m[12]*m[6]-
                m[2]*m[4]*m[13]+m[2]*m[12]*m[5])/det;
    invm[15] = 1.0;
  }

//transpose a matrix
  public void transposeMatrix(float[] m)
  {
    float tmp;
    tmp = m[1];
    m[1] = m[4];
    m[4] = tmp;
    tmp = m[2];
    m[2] = m[8];
    m[8] = tmp;
    tmp = m[3];
    m[3] = m[12];
    m[12] = tmp;
    tmp = m[6];
    m[6] = m[9];
    m[9] = tmp;
    tmp = m[7];
    m[7] = m[13];
    m[13] = tmp;
    tmp = m[11];
    m[11] = m[14];
    m[14] = tmp;

  }

  public void transposeMatrix(double[] m)
  {
    double tmp;
    tmp = m[1];
    m[1] = m[4];
    m[4] = tmp;
    tmp = m[2];
    m[2] = m[8];
    m[8] = tmp;
    tmp = m[3];
    m[3] = m[12];
    m[12] = tmp;
    tmp = m[6];
    m[6] = m[9];
    m[9] = tmp;
    tmp = m[7];
    m[7] = m[13];
    m[13] = tmp;
    tmp = m[11];
    m[11] = m[14];
    m[14] = tmp;

  }

//ane and axis to a rotation matrix ----------------------------
  public void axis2Rot( float[] m, float[] k, float theta )
  {
    float c = (float)Math.cos(theta);
    float s = (float)Math.sin(theta);
    float v = 1 - c;

    m[0] = k[0]*k[0]*v + c;
    m[4] = k[0]*k[1]*v - k[2]*s;
    m[8] = k[0]*k[2]*v + k[1]*s;

    m[1] = k[0]*k[1]*v + k[2]*s;
    m[5] = k[1]*k[1]*v + c;
    m[9] = k[1]*k[2]*v - k[0]*s;

    m[2] = k[0]*k[2]*v - k[1]*s;
    m[6] = k[1]*k[2]*v + k[0]*s;
    m[10] = k[2]*k[2]*v + c;
  }

  public void axis2Rot( double[] m, double[] k, double theta )
  {
    float c = (float)Math.cos(theta);
    float s = (float)Math.sin(theta);
    float v = 1 - c;

    m[0] = k[0]*k[0]*v + c;
    m[4] = k[0]*k[1]*v - k[2]*s;
    m[8] = k[0]*k[2]*v + k[1]*s;

    m[1] = k[0]*k[1]*v + k[2]*s;
    m[5] = k[1]*k[1]*v + c;
    m[9] = k[1]*k[2]*v - k[0]*s;

    m[2] = k[0]*k[2]*v - k[1]*s;
    m[6] = k[1]*k[2]*v + k[0]*s;
    m[10] = k[2]*k[2]*v + c;
  }

  // Inverse angle-axis formula.-----------------------------------

  public void rot2Axis( float[] k, float[] theta, float[] m )
  {
    float c = (float)(0.5 * (m[0] + m[5] + m[10] - 1.0));
    float r1 = m[6] - m[9];
    float r2 = m[8] - m[2];
    float r3 = m[1] - m[4];
    float s = (float)(0.5 * Math.sqrt(r1*r1+r2*r2+r3*r3));

    theta[0] = (float)Math.atan2(s, c);

    if( Math.abs(s) > EPSILON )
      {
        c = (float)(2.0*s);

        k[0] = r1 / c;
        k[1] = r2 / c;
        k[2] = r3 / c;
      }
    else
      {
        if( c > 0 ) // theta = 0
          {
            k[0] = 0;
            k[1] = 0;
            k[2] = 1;
          }
        else // theta = +-pi: Shepperd procedure
          {
            float k0_2 = (m[0] + 1)/2;
            float k1_2 = (m[5] + 1)/2;
            float k2_2 = (m[10] + 1)/2;

            if( k0_2 > k1_2 )
              {
                if( k0_2 > k2_2 ) // k0 biggest
                  {
                    k[0] = (float)Math.sqrt(k1_2);
                    k[1] = (m[1] + m[4])/(4*k[0]);
                    k[2] = (m[2] + m[8])/(4*k[0]);
                  }
                else // k2 biggest
                  {
                    k[2] = (float)Math.sqrt(k2_2);
                    k[0] = (m[2] + m[8])/(4*k[2]);
                    k[1] = (m[6] + m[9])/(4*k[2]);
                  }
              }
            else
              {
                if( k1_2 > k2_2 ) // k1 biggest
                  {
                    k[1] = (float)Math.sqrt(k1_2);
                    k[0] = (m[1] + m[4])/(4*k[1]);
                    k[2] = (m[6] + m[9])/(4*k[1]);
                  }
                else // k2 biggest
                  {
                    k[2] = (float)Math.sqrt(k2_2);
                    k[0] = (m[2] + m[8])/(4*k[2]);
                    k[1] = (m[6] + m[9])/(4*k[2]);
                  }
              }
          }
      }
  }

//quaternion to rotation matrix

  public void q2R( float[] m, float[] q )
  {
    m[0] = q[0]*q[0] + q[1]*q[1] - q[2]*q[2] - q[3]*q[3];
    m[1] = 2*q[1]*q[2] + 2*q[0]*q[3];
    m[2] = 2*q[1]*q[3] - 2*q[0]*q[2];

    m[4] = 2*q[1]*q[2] - 2*q[0]*q[3];
    m[5] = q[0]*q[0] + q[2]*q[2] - q[1]*q[1] - q[3]*q[3];
    m[6] = 2*q[2]*q[3] + 2*q[0]*q[1];

    m[8] = 2*q[1]*q[3] + 2*q[0]*q[2];
    m[9] = 2*q[2]*q[3] - 2*q[0]*q[1];
    m[10]= q[0]*q[0] + q[3]*q[3] - q[1]*q[1] - q[2]*q[2];
  }

//angle axis to quaternion

  public void axis2q(float[] q, float theta, float[] a)
  {
     normalizeV3(a);
     copyV3(q, a);
     scaleV3((float)Math.sin(theta/2.0), q);
     q[3] = (float)Math.cos(theta/2.0);
  }

//normalize a quaternion

  public void normalizeQ(float[] q)
  {
     float  mag = (q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3]);
     for (int i = 0; i < 4; i++)
       q[i] /= mag;
  }

  /*
   * Prints matrix to stderr.
   */
  public void printMatrix( float[] m )
  {
    int i, j;

    for( i=0; i<4; i++ )
      {
        for( j=0; j<4; j++ )
            System.out.print(m[i+j*4]);
        System.out.print("\n");
      }
    System.out.print("\n");
  }

  public void printMatrix( double[] m )
  {
    int i, j;

    for( i=0; i<4; i++ )
      {
        for( j=0; j<4; j++ )
          System.out.print(m[i+j*4] + " ");
        System.out.print("\n");
      }
    System.out.print("\n");
  }

  public void printV3(double[] v)
  {
          System.out.println( " " +  v[0] +  "," +  v[1] +  "," + v[2]);
  }

  public void printV3(float[] v)
  {
          System.out.println(" " + v[0] + "," + v[1] + "," + v[2]);
  }

  public void printV4(float[] v)
  {
          System.out.println(" " + v[0] + "," + v[1] + "," + v[2] + "," + v[3]);
  }

  public void buildLookAt(float[] m, float[] eye, float[] at, float[] up)
  {
     //I am not 100% certain that my cross products are in the right order
     float[] tmp1 = new float[3];
     float[] tmp2 = new float[3];
     float[] tmp3 = new float[3];

     subV3(tmp1, eye, at);
     float norm;
     norm = normV3(tmp1);
     tmp1[0] /=norm;
     tmp1[1] /=norm;
     tmp1[2] /=norm;

     m[2]     =tmp1[0];
     m[6]     =tmp1[1];
     m[10]    =tmp1[2];

     crossV3(tmp2, up, tmp1);
     norm = normV3(tmp2);
     tmp2[0] /=norm;
     tmp2[1] /=norm;
     tmp2[2] /=norm;

     m[0]     =tmp2[0];
     m[4]     =tmp2[1];
     m[8]     =tmp2[2];

     crossV3(tmp3, tmp1, tmp2);
     norm = normV3(tmp3);
     tmp3[0] /=norm;
     tmp3[1] /=norm;
     tmp3[2] /=norm;

     m[1]     =tmp3[0];
     m[5]     =tmp3[1];
     m[9]     =tmp3[2];

     m[12]= -eye[0];
     m[13]= -eye[1];
     m[14]= -eye[2];
     m[15]= 1;

     m[3]  = 0;
     m[7]  = 0;
     m[11] = 0;
  }


//======================== calculus ===================================

//standard scalar derivative measure
  public void derivative3D(byte[] magV1, float[] gradV3, int sx, int sy, int sz, byte[] dat)
  {
          float[] tmp = new float[sx*sy*sz];
          float maxm = 0;
          int i;
          for(i = 0; i < sz; ++i){
                  for(int j = 0; j < (sy); ++j){
                          for(int k = 0; k < (sx); ++k){
                                  if((k<2)||(k>sx-3)||(j<2)||(j>sy-3)||(i<2)||(i>sz-3)){
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] = 0f;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] = 0f;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] = 0f;
                                          tmp[i*sx*sy + j*sx + k] = 0f;
                                  }
                                  else {
                                          int dx = dat[i*sx*sy + j*sx + (k+1)] - dat[i*sx*sy + j*sx + (k-1)];
                                          int dy = dat[i*sx*sy + (j+1)*sx + k] - dat[i*sx*sy + (j-1)*sx + k];
                                          int dz = dat[(i+1)*sx*sy + j*sx + k] - dat[(i-1)*sx*sy + j*sx + k];
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] = (float)dx;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] = (float)dy;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] = (float)dz;
                                          tmp[i*sx*sy + j*sx + k] = (float)Math.sqrt(dx*dx + dy*dy + dz*dz);
                                          maxm = maxm < tmp[i*sx*sy + j*sx + k] ? tmp[i*sx*sy + j*sx + k] : maxm;
                                  }
                          }
                  }
          }
          for( i = 0; i < sz; ++i){
                  for(int j = 0; j < sy; ++j){
                          for(int k = 0; k < sx; ++k){
                                  magV1[i*sx*sy + j*sx + k] = new Float((tmp[i*sx*sy + j*sx + k]/maxm)*255).byteValue();
                          }
                  }
          }
          tmp = null;
  }


//standard scalar derivative measure - vgh volume (value grad hessian)
  public void derivative3DVGH(float[] gradV3, int sx, int sy, int sz, byte[] dat)
  {
          //cerr << "hi!!!!!!!!!! " << sz << " " << sy << " " << sx << endl;
          for(int i = 0; i < sz; ++i){
                  for(int j = 0; j < (sy); ++j){
                          for(int k = 0; k < (sx); ++k){
                                  if((k<1)||(k>sx-2)||(j<1)||(j>sy-2)||(i<1)||(i>sz-2)){
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] = 0f;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] = 0f;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] = 0f;
                                  }
                                  else {
                                          int dx = dat[i*sx*sy*3     + j*sx*3     +(k+1)*3] - dat[i*sx*sy*3     + j*sx*3     + (k-1)*3];
                                          int dy = dat[i*sx*sy*3     + (j+1)*sx*3 + k*3]    - dat[i*sx*sy*3     + (j-1)*sx*3 + k*3];
                                          int dz = dat[(i+1)*sx*sy*3 + j*sx*3     + k*3]    - dat[(i-1)*sx*sy*3 + j*sx*3     + k*3];
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] = (float)dx;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] = (float)dy;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] = (float)dz;
                                  }
                          }
                  }
          }
  }

  public void addDer(byte[] magV1, float[] gradV3, int sx, int sy, int sz, byte[] dat1, byte[] dat2)
  {
          int i, j, k;
          float[] tmp = new float[sx*sy*sz];
          float maxm = 0;
          for(i = 0; i < sz; ++i){
                  for(j = 0; j < (sy); ++j){
                          for(k = 0; k < (sx); ++k){
                                  if((k<2)||(k>sx-3)||(j<2)||(j>sy-3)||(i<2)||(i>sz-3)){
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] = 0f;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] = 0f;
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] = 0f;
                                          tmp[i*sx*sy + j*sx + k] = 0;
                                  }
                                  else {
                                          int dx1 = dat1[i*sx*sy + j*sx + (k+1)] - dat1[i*sx*sy + j*sx + (k-1)];
                                          int dy1 = dat1[i*sx*sy + (j+1)*sx + k] - dat1[i*sx*sy + (j-1)*sx + k];
                                          int dz1 = dat1[(i+1)*sx*sy + j*sx + k] - dat1[(i-1)*sx*sy + j*sx + k];

                                          int dx2 = dat2[i*sx*sy + j*sx + (k+1)] - dat2[i*sx*sy + j*sx + (k-1)];
                                          int dy2 = dat2[i*sx*sy + (j+1)*sx + k] - dat2[i*sx*sy + (j-1)*sx + k];
                                          int dz2 = dat2[(i+1)*sx*sy + j*sx + k] - dat2[(i-1)*sx*sy + j*sx + k];

                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] = (float)(dx1 + dx2);
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] = (float)(dy1 + dy2);
                                          gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] = (float)(dz1 + dz2);
                                          tmp[i*sx*sy + j*sx + k] = (float)Math.sqrt((dx1+dx2)*(dx1+dx2) + (dy1+dy2)*(dy1+dy2) + (dz1+dz2)*(dz1+dz2));
                                          maxm = maxm < tmp[i*sx*sy + j*sx + k] ? tmp[i*sx*sy + j*sx + k] : maxm;
                                  }
                          }
                  }
          }
          for( i = 0; i < sz; ++i){
                  for(j = 0; j < sy; ++j){
                          for(k = 0; k < sx; ++k){
                                  magV1[i*sx*sy + j*sx + k] = new Float((tmp[i*sx*sy + j*sx + k]/maxm)*255).byteValue();
                          }
                  }
          }
          tmp = null;
  }


  //compute an additive gradient
  public int AGradArb(float[] gradV3,           //put it in here, create your data first
                      int sx, int sy, int sz,  //size of each axis
                      int n,                   //first n elements of data to compute grad with
                      int elts,                //number of elements in the data
                      byte[] data)                 //the data
  {
    /*
  #if 0
          T **minmax = new T*[elts];
          for(int e=0; e<elts; ++e){
                  minmax[e] = new T[2];
                  minmax[e][0] = (T)10000000000;
                  minmax[e][1] = (T)-10000000000;
          }

          for(int i = 0; i < sz; ++i){
                  for(int j = 0; j < (sy); ++j){
                          for(int k = 0; k < (sx); ++k){
                                  for(int e=0; e<n; ++e){
                                          minmax[e][0] = MIN(data[i*sx*sy*elts + j*sx*elts + (k+1)*elts + e], minmax[e][0]);
                                          minmax[e][1] = MAX(data[i*sx*sy*elts + j*sx*elts + (k+1)*elts + e], minmax[e][1]);
                                  }
                          }
                  }
          }

          for(e=0; e<elts; ++e){
                  cerr << "Volume " << e << " , min: " << (float)minmax[e][0] << "   max: " << (float)minmax[e][1] << endl;
          }
  #endif
   */
   for(int i = 0; i < sz; ++i)
    {
      for(int j = 0; j < (sy); ++j)
      {
        for(int k = 0; k < (sx); ++k)
        {
           if((k<1)||(k>sx-2)||(j<1)||(j>sy-2)||(i<1)||(i>sz-2))
          {
            gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] = 0f;
            gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] = 0f;
            gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] = 0f;
          }
            else
          {
            gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] = 0f;
            gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] = 0f;
            gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] = 0f;
            for(int e=0; e<n; ++e)
            {
                 gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0] += data[i*sx*sy*elts + j*sx*elts + (k+1)*elts + e] -
                                                         data[i*sx*sy*elts + j*sx*elts + (k-1)*elts + e];
                 gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1] += data[i*sx*sy*elts + (j+1)*sx*elts + k*elts + e] -
                                                         data[i*sx*sy*elts + (j-1)*sx*elts + k*elts + e];
                 gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2] += data[(i+1)*sx*sy*elts + j*sx*elts + k*elts + e] -
                                                         data[(i-1)*sx*sy*elts + j*sx*elts + k*elts + e];
             }
           } // end else
        }  // end for k
       } // end for j
      }	// end for i
      return 0;
}


 public void GMag(byte[] gmag, int sx, int sy, int sz, float[] grad){
         int i;
         float maxm = 0;
         float[] tmp = new float[sx*sy*sz];
         for(i = 0; i < sz; ++i){
                 for(int j = 0; j < (sy); ++j){
                         for(int k = 0; k < (sx); ++k){
                                 tmp[i*sx*sy + j*sx + k] = normV3(grad, i*sx*sy*3 + j*sx*3 + k*3);
                                 maxm = MAX(tmp[i*sx*sy + j*sx + k], maxm);
                         }
                 }
         }
         for(i = 0; i < sz; ++i){
                 for(int j = 0; j < (sy); ++j){
                         for(int k = 0; k < (sx); ++k){
                                 gmag[i*sx*sy + j*sx + k] = new Float(tmp[i*sx*sy + j*sx + k]/maxm * 255.0).byteValue();
                         }
                 }
         }
         tmp = null;
 }

 public void GMagHess(byte[] gmag, byte[] hess, int sx, int sy, int sz, float[] gradV3){
         int i;
         float maxm = 0;
         //find the gradient magnitude and it's max val
         float[] tmpg = new float[sx*sy*sz];
         for(i = 0; i < sz; ++i){
                 for(int j = 0; j < (sy); ++j){
                         for(int k = 0; k < (sx); ++k){
                                 tmpg[i*sx*sy + j*sx + k] = normV3(gradV3, i*sx*sy*3 + j*sx*3 + k*3);
                                 maxm = MAX(tmpg[i*sx*sy + j*sx + k], maxm);
                         }
                 }
         }

         float hmax = -100000000;
         float hmin = 100000000;
         float[] tmph = new float[sx*sy*sz];

         //compute the 2nd derivative in the gradient direction
         //  a mask would probably help out here??
         for(i = 0; i < sz; ++i){
                 for(int j = 0; j < (sy); ++j){
                         for(int k = 0; k < (sx); ++k){
                                 if((k<1)||(k>sx-2)||(j<1)||(j>sy-2)||(i<1)||(i>sz-2)){
                                         tmph[i*sx*sy + j*sx + k] = 0f;
                                 }
                                 else {
                                         float[] h = new float[9];
                                         h[0] = gradV3[i*sx*sy*3 + j*sx*3 + (k+1)*3 + 0] - gradV3[i*sx*sy*3 + j*sx*3 + (k-1)*3 + 0];
                                         h[1] = gradV3[i*sx*sy*3 + (j+1)*sx*3 + k*3 + 0] - gradV3[i*sx*sy*3 + (j-1)*sx*3 + k*3 + 0];
                                         h[2] = gradV3[(i+1)*sx*sy*3 + j*sx*3 + k*3 + 0] - gradV3[(i-1)*sx*sy*3 + j*sx*3 + k*3 + 0];
                                         h[3] = gradV3[i*sx*sy*3 + j*sx*3 + (k+1)*3 + 1] - gradV3[i*sx*sy*3 + j*sx*3 + (k-1)*3 + 1];
                                         h[4] = gradV3[i*sx*sy*3 + (j+1)*sx*3 + k*3 + 1] - gradV3[i*sx*sy*3 + (j-1)*sx*3 + k*3 + 1];
                                         h[5] = gradV3[(i+1)*sx*sy*3 + j*sx*3 + k*3 + 1] - gradV3[(i-1)*sx*sy*3 + j*sx*3 + k*3 + 1];
                                         h[6] = gradV3[i*sx*sy*3 + j*sx*3 + (k+1)*3 + 2] - gradV3[i*sx*sy*3 + j*sx*3 + (k-1)*3 + 2];
                                         h[7] = gradV3[i*sx*sy*3 + (j+1)*sx*3 + k*3 + 2] - gradV3[i*sx*sy*3 + (j-1)*sx*3 + k*3 + 2];
                                         h[8] = gradV3[(i+1)*sx*sy*3 + j*sx*3 + k*3 + 2] - gradV3[(i-1)*sx*sy*3 + j*sx*3 + k*3 + 2];
                                         float[] tv = new float[3];
                                         float[] tg = {gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 0]/tmpg[i*sx*sy + j*sx + k],
                                                        gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 1]/tmpg[i*sx*sy + j*sx + k],
                                                        gradV3[i*sx*sy*3 + j*sx*3 + k*3 + 2]/tmpg[i*sx*sy + j*sx + k]};

                                         tv[0] = tg[0] * h[0] +
                                                       tg[1] * h[1] +
                                                                         tg[2] * h[2];
                                         tv[1] = tg[0] * h[3] +
                                                       tg[1] +
                                                                         tg[2] * h[5];
                                         tv[2] = tg[0] * h[6] +
                                                       tg[1] * h[7] +
                                                                         tg[2] * h[8];
                                         tmph[i*sx*sy + j*sx + k] = tg[0] * tv[0] +
                                                                          tg[1] * tv[1] +
                                                                                            tg[2] * tv[2];
                                         hmax = MAX(hess[i*sx*sy + j*sx + k], hmax);
                                         hmin = MIN(hess[i*sx*sy + j*sx + k], hmin);

                                 }
                         }
                 }
         }

         //now quantize to chars
         for(i = 0; i < sz; ++i){
                 for(int j = 0; j < (sy); ++j){
                         for(int k = 0; k < (sx); ++k){
                                 gmag[i*sx*sy + j*sx + k] = new Float(tmpg[i*sx*sy + j*sx + k]/maxm * 255.0).byteValue();
                                 if(hess[i*sx*sy + j*sx + k] < 0){
                                                 float th = (float)affine(hmin, tmph[i*sx*sy + j*sx + k], 0, 0, 1);
                                                 //th = (float)(1.0-((1.0-th)*(1.0-th)*(1.0-th)));
                                                 hess[i*sx*sy + j*sx + k] = new Double(affine(0,th,1, 0, 255/3)).byteValue();
                                 } else {
                                                 float th = (float)affine(0, tmph[i*sx*sy + j*sx + k], hmax, 0, 1);
                                                 //th = (float)(1.0-((1.0-th)*(1.0-th)*(1.0-th)));
                                                 hess[i*sx*sy + j*sx + k] = new Double(affine(0,th,1,255/3,255/3*2)).byteValue();
                                 }
                         }
                 }
         }
         tmpg = null;
         tmph = null;
 }

//float vector scale and bias to 8bit unsigned char
 public void scalebias(byte[] ucgradV3, float[] gradV3, int sx, int sy, int sz){
         int stx = 3;
         int sty = sx*3;
         int stz = sy*sx*3;
         for(int i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 ucgradV3[i*stz + j*sty + k*stx + 0] = new Float(gradV3[i*stz + j*sty + k*stx + 0]*128 + 128).byteValue();
                                 ucgradV3[i*stz + j*sty + k*stx + 1] = new Float(gradV3[i*stz + j*sty + k*stx + 1]*128 + 128).byteValue();
                                 ucgradV3[i*stz + j*sty + k*stx + 2] = new Float(gradV3[i*stz + j*sty + k*stx + 2]*128 + 128).byteValue();
                         }
                 }
         }

 }

//float vector normalize, scale, and bias to 8bit unsigned char
 public void scalebiasN(byte[] ucgradV3, float[] gradV3, int sx, int sy, int sz){
         int stx = 3;
         int sty = sx*3;
         int stz = sy*sx*3;
         for(int i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 normalizeV3(gradV3, i*stz + j*sty + k*stx + 0);
                                 ucgradV3[i*stz + j*sty + k*stx + 0] = new Float(gradV3[i*stz + j*sty + k*stx + 0]*128 + 128).byteValue();
                                 ucgradV3[i*stz + j*sty + k*stx + 1] = new Float(gradV3[i*stz + j*sty + k*stx + 1]*128 + 128).byteValue();
                                 ucgradV3[i*stz + j*sty + k*stx + 2] = new Float(gradV3[i*stz + j*sty + k*stx + 2]*128 + 128).byteValue();
                         }
                 }
         }

 }

 public void scaleBiasHiLoUC(byte[] ucgradV2, float[] gradV3, int sx, int sy, int sz){
         int stx = 3;
         int sty = sx*3;
         int stz = sy*sx*3;
         for(int i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 normalizeV3(gradV3, i*stz + j*sty + k*stx + 0);
                                 if(gradV3[i*stz + j*sty + k*stx + 2] < 0){
                                         gradV3[i*stz + j*sty + k*stx + 0] = -gradV3[i*stz + j*sty + k*stx + 0];
                                         gradV3[i*stz + j*sty + k*stx + 1] = -gradV3[i*stz + j*sty + k*stx + 1];
                                 }
                                 ucgradV2[i*sx*sy*2 + j*sx*2 + k*2 + 0] = new Float(gradV3[i*stz + j*sty + k*stx + 0]*128 + 128).byteValue();
                                 ucgradV2[i*sx*sy*2 + j*sx*2 + k*2 + 1] = new Float(gradV3[i*stz + j*sty + k*stx + 1]*128 + 128).byteValue();
                         }
                 }
         }

 }

//copy scale and add vector

 public void csaddV3(float[] out, float s, float[] in)
 {
   out[0] += s*in[0];
   out[1] += s*in[1];
   out[2] += s*in[2];
 }

 public void csaddV3(float[] out, int oIndex, float s, float[] in, int iIndex)
{
  out[oIndex + 0] += s*in[iIndex + 0];
  out[oIndex + 1] += s*in[iIndex + 1];
  out[oIndex + 2] += s*in[iIndex + 2];
}


 public void csaddVARB(float[] out, float s, float[] in, int elts)
 {
         for(int e=0; e<elts; ++e){
                 out[e] += s*in[e];
         }
 }

 public void csaddVARB(float[] out, float s, byte[] in, int elts)
 {
         for(int e=0; e<elts; ++e){
                 out[e] += (float)(s*(in[e]/255.0f));
         }
 }

//blurr a 3D 3 vector field using these weights
//				------------
//       /w3 /w2 /w3 /
//      /-----------/
//	   /w2 /w1 /w2 /        <-- z+1
//    /-----------/
//   /w3 /w2 /w3 /
//  /-----------/
//				------------
//       /w2 /w1 /w2 / y+1
//      /-----------/
//	   /w1 /w0 /w1 / y      <-- z
//    /-----------/
//   /w2 /w1 /w2 / y-1
//  /-----------/
//				------------
//       /w3 /w2 /w3 /
//      /-----------/
//	   /w2 /w1 /w2 /        <-- z-1
//    /-----------/
//   /w3 /w2 /w3 /
//  /-----------/
//   x-1  x  x+1

 public void blurV3D(float[] gradV3, float w0, float w1, float w2, float w3, int sx, int sy, int sz){
         int i;
         float[] tmp = new float[sx*sy*sz*3];
         int stx = 3;
         int sty = sx*3;
         int stz = sy*sx*3;
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 zeroV3(tmp, i*stz + j*sty + k*stx + 0);
                         }
                 }
         }

         for(i = 1; i<sz-1; ++i){
                 for(int j = 1; j<sy-1; ++j){
                         for(int k = 1; k<sx-1; ++k){
                                 int idx = i*stz + j*sty + k*stx + 0;
                                 csaddV3(tmp, i*stz + j*sty + k*stx + 0, w0, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + j*sty + k*stx + 0, w1, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j+1)*sty + k*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j+1)*sty + (k+1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j+1)*sty + (k-1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j-1)*sty + k*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j-1)*sty + (k+1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j-1)*sty + (k-1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + j*sty + (k+1)*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + j*sty + (k-1)*stx + 0, w2, gradV3, idx);

                                 csaddV3(tmp, (i-1)*stz + j*sty + k*stx + 0, w1, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, (i-1)*stz + (j+1)*sty + k*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j+1)*sty + (k+1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j+1)*sty + (k-1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j-1)*sty + k*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j-1)*sty + (k+1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j-1)*sty + (k-1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + j*sty + (k+1)*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + j*sty + (k-1)*stx + 0, w2, gradV3, idx);

                                 csaddV3(tmp, i*stz + (j+1)*sty + k*stx + 0, w1, gradV3, idx);
                                 csaddV3(tmp, i*stz + (j+1)*sty + (k+1)*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, i*stz + (j+1)*sty + (k-1)*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, i*stz + (j-1)*sty + k*stx + 0, w1, gradV3, idx);
                                 csaddV3(tmp, i*stz + (j-1)*sty + (k+1)*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, i*stz + (j-1)*sty + (k-1)*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, i*stz + j*sty + (k+1)*stx + 0, w1, gradV3, idx);
                                 csaddV3(tmp, i*stz + j*sty + (k-1)*stx + 0, w1, gradV3, idx);
                         }
                 }
         }

         float div = w0 + 6*w1 + 12*w2 + 8*w3;

         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 gradV3[i*stz + j*sty + k*stx + 0] = tmp[i*stz + j*sty + k*stx + 0]/div;
                                 gradV3[i*stz + j*sty + k*stx + 1] = tmp[i*stz + j*sty + k*stx + 1]/div;
                                 gradV3[i*stz + j*sty + k*stx + 2] = tmp[i*stz + j*sty + k*stx + 2]/div;
                         }
                 }
         }

        tmp = null;
 }

 public void blurVARB(byte[] dataV, float w0, float w1, float w2, float w3, int sx, int sy, int sz, int elts){

         //cerr << "BLUR :   " << sx << "  " << sy  << "  " << sz << " * " << elts << endl;
         int i;
         float[] tmp = new float[sx*sy*sz*elts];
         int stx = elts;
         int sty = sx*elts;
         int stz = sy*sx*elts;
                 //cerr << "   stride " << stx << " " << sty << " " << stz << endl;



         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 for(int e = 0; e< elts; ++e){
                                         tmp[i*stz + j*sty + k*stx + e] = 0.0f;
                                 }
                         }
                 }
         }

         for(i = 1; i<sz-1; ++i){
                 for(int j = 1; j<sy-1; ++j){
                         for(int k = 1; k<sx-1; ++k){
                                 for(int e = 0; e<elts; ++ e){
                                         int idx = i*stz + j*sty + k*stx + e;
                                         float v0 = (float)(dataV[idx]/255.0f*w0);
                                         float v1 = (float)(dataV[idx]/255.0f*w1/6.0f);
                                         float v2 = (float)(dataV[idx]/255.0f*w2/12.0f);
                                         float v3 = (float)(dataV[idx]/255.0f*w3/8.0f);

                                   tmp[i*stz + j*sty + k*stx + e] += v0; //1
                                   tmp[(i+1)*stz + j*sty + k*stx + e] += v1; //1
                                   tmp[(i+1)*stz + (j+1)*sty + k*stx + e] += v2; //1
                                   tmp[(i+1)*stz + (j+1)*sty + (k+1)*stx + e] += v3;	//1
                                   tmp[(i+1)*stz + (j+1)*sty + (k-1)*stx + e] += v3;	//2
                                   tmp[(i+1)*stz + (j-1)*sty + k*stx + e] += v2; //2
                                   tmp[(i+1)*stz + (j-1)*sty + (k+1)*stx + e] += v3; //3
                                   tmp[(i+1)*stz + (j-1)*sty + (k-1)*stx + e] += v3; //4
                                   tmp[(i+1)*stz + j*sty + (k+1)*stx + e] += v2; //3
                                   tmp[(i+1)*stz + j*sty + (k-1)*stx + e] += v2; //4

                                   tmp[(i-1)*stz + j*sty + k*stx + e] += v1; //2
                                   tmp[(i-1)*stz + (j+1)*sty + k*stx + e] += v2; //5
                                   tmp[(i-1)*stz + (j+1)*sty + (k+1)*stx + e] += v3;	//5
                                   tmp[(i-1)*stz + (j+1)*sty + (k-1)*stx + e] += v3;	//6
                                   tmp[(i-1)*stz + (j-1)*sty + k*stx + e] += v2; //6
                                   tmp[(i-1)*stz + (j-1)*sty + (k+1)*stx + e] += v3; //7
                                   tmp[(i-1)*stz + (j-1)*sty + (k-1)*stx + e] += v3; //8
                                   tmp[(i-1)*stz + j*sty + (k+1)*stx + e] += v2; //7
                                   tmp[(i-1)*stz + j*sty + (k-1)*stx + e] += v2; //8

                                   tmp[i*stz + (j+1)*sty + k*stx + e] += v1; //3
                                   tmp[i*stz + (j+1)*sty + (k+1)*stx + e] += v2; //9
                                   tmp[i*stz + (j+1)*sty + (k-1)*stx + e] += v2; //10
                                   tmp[i*stz + (j-1)*sty + k*stx + e] += v1; //4
                                   tmp[i*stz + (j-1)*sty + (k+1)*stx + e] += v2; //11
                                   tmp[i*stz + (j-1)*sty + (k-1)*stx + e] += v2; //12
                                   tmp[i*stz + j*sty + (k+1)*stx + e] += v1; //5
                                   tmp[i*stz + j*sty + (k-1)*stx + e] += v1; //6
                                 }
                         }
                 }
         }

         //float div = w0 + 6*w1 + 12*w2 + 8*w3;
         float div = w0 + w1 + w2 + w3;

         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 for(int e =0; e<elts; ++e){
                                         dataV[i*stz + j*sty + k*stx + e] = new Float(CLAMP((tmp[i*stz + j*sty + k*stx + e]/div))*255f).byteValue();
                                 }
                         }
                 }
         }

         tmp = null;  //why does this explode
 }

//same as above but normalize too

 public void blurV3DN(float[] gradV3, float w0, float w1, float w2, float w3, int sx, int sy, int sz){
         int i, j, k;
         float[] tmp = new float[sx*sy*sz*3];
         int stx = 3;
         int sty = sx*3;
         int stz = sy*sx*3;
         for(i = 0; i<sz; ++i){
                 for(j = 0; j<sy; ++j){
                         for(k = 0; k<sx; ++k){
                                 zeroV3(tmp, i*stz + j*sty + k*stx + 0);
                         }
                 }
         }

         for(i = 1; i<sz-1; ++i){
                 for(j = 1; j<sy-1; ++j){
                         for(k = 1; k<sx-1; ++k){
                                 int idx = i*stz + j*sty + k*stx + 0;
                                 csaddV3(tmp, i*stz + j*sty + k*stx + 0, w0, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + j*sty + k*stx + 0, w1, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j+1)*sty + k*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j+1)*sty + (k+1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j+1)*sty + (k-1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j-1)*sty + k*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j-1)*sty + (k+1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + (j-1)*sty + (k-1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + j*sty + (k+1)*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i+1)*stz + j*sty + (k-1)*stx + 0, w2, gradV3, idx);

                                 csaddV3(tmp, (i-1)*stz + j*sty + k*stx + 0, w1, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, (i-1)*stz + (j+1)*sty + k*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j+1)*sty + (k+1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j+1)*sty + (k-1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j-1)*sty + k*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j-1)*sty + (k+1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + (j-1)*sty + (k-1)*stx + 0, w3, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + j*sty + (k+1)*stx + 0, w2, gradV3, idx);
                                 csaddV3(tmp, (i-1)*stz + j*sty + (k-1)*stx + 0, w2, gradV3, idx);

                                 csaddV3(tmp, i*stz + (j+1)*sty + k*stx + 0, w1, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, i*stz + (j+1)*sty + (k+1)*stx + 0, w2, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, i*stz + (j+1)*sty + (k-1)*stx + 0, w2, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, i*stz + (j-1)*sty + k*stx + 0, w1, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, i*stz + (j-1)*sty + (k+1)*stx + 0, w2, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, i*stz + (j-1)*sty + (k-1)*stx + 0, w2, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, i*stz + j*sty + (k+1)*stx + 0, w1, gradV3, i*stz + j*sty + k*stx + 0);
                                 csaddV3(tmp, i*stz + j*sty + (k-1)*stx + 0, w1, gradV3, i*stz + j*sty + k*stx + 0);

                         }
                 }
         }

         float div = w0 + 6*w1 + 12*w2 + 8*w3;

         for(i = 0; i<sz; ++i){
                 for(j = 0; j<sy; ++j){
                         for(k = 0; k<sx; ++k){
                                 scaleV3(1/div, tmp, i*stz + j*sty + k*stx + 0);
                                 normalizeV3(tmp, i*stz + j*sty + k*stx + 0);
                                 copyV3(gradV3, i*stz + j*sty + k*stx + 0, tmp, i*stz + j*sty + k*stx + 0);
                         }
                 }
         }

         tmp = null;

 }

//=====================================================================================
//---------------------- Quantize -----------------------------------------------------
//=====================================================================================
 /************    Unsigned !!!!!!!!!!!!!!!!!!!!!! ********************************/
 /*
 public void quantize(byte[] dout, int sx, int sy, int sz, short[] din){

         short max = 0;
         short min = USHRT_MAX;
         for(int i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 max = MAX(new Short(max, new Short(din[i*sx*sy + j*sx + k]);
                                 min = MIN(new Short(min, new Short(din[i*sx*sy + j*sx + k]);

                         }
                 }
         }
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 dout[i*sx*sy + j*sx + k] = new Double(affine(new Short(min).doubleValue(),
                                                                               new Short(din[i*sx*sy + j*sx + k]).doubleValue(),
                                                                               new Short(max).doubleValue(),
                                                                               0,
                                                                               255)).byteValue();
                         }
                 }
         }
 }
 */

 public void quantize(byte[] dout, int sx, int sy, int sz, short[] din){
         int i;
         short max = SHRT_MIN;
         short min = SHRT_MAX;
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 max = new Float(MAX(new Short(max).floatValue(), new Short(din[i*sx*sy + j*sx + k]).floatValue())).shortValue();
                                 min = new Float(MIN(new Short(min).floatValue(), new Short(din[i*sx*sy + j*sx + k]).floatValue())).shortValue();

                         }
                 }
         }
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 dout[i*sx*sy + j*sx + k] = new Double(affine(new Short(min).doubleValue(),
                                                                              new Short(din[i*sx*sy + j*sx + k]).doubleValue(),
                                                                              new Short(max).doubleValue(),
                                                                              0,
                                                                              255)).byteValue();
                         }
                 }
         }
 }

/*
 public void quantize(byte[] dout, int sx, int sy, int sz, int[] din){

         int max = INT_MIN;
         int min = INT_MAX;
         for(int i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 max = MAX(max, din[i*sx*sy + j*sx + k]);
                                 min = MIN(min, din[i*sx*sy + j*sx + k]);

                         }
                 }
         }
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 dout[i*sx*sy + j*sx + k] = new Double(affine(min,din[i*sx*sy + j*sx + k], max, 0, 255)).byteValue();
                         }
                 }
         }
 }
 */

 public void quantize(byte[] dout, int sx, int sy, int sz, int[] din){

         int i;
         int max = 0;
         int min = UINT_MAX;
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 max = Math.max(max, din[i*sx*sy + j*sx + k]);
                                 min = Math.min(min, din[i*sx*sy + j*sx + k]);

                         }
                 }
         }
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 dout[i*sx*sy + j*sx + k] = new Double(affine(min,din[i*sx*sy + j*sx + k], max, 0, 255)).byteValue();
                         }
                 }
         }
 }

 public void quantize(byte[] dout, int sx, int sy, int sz, float[] din){

         int i;
         float max = -10000000000f;
         float min = 10000000000f;
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 max = MAX(max, din[i*sx*sy + j*sx + k]);
                                 min = MIN(min, din[i*sx*sy + j*sx + k]);
                         }
                 }
         }
         for(i = 0; i<sz; ++i){
                 for(int j = 0; j<sy; ++j){
                         for(int k = 0; k<sx; ++k){
                                 dout[i*sx*sy + j*sx + k] = new Double(affine(min, din[i*sx*sy + j*sx + k], max, 0, 255)).byteValue();
                         }
                 }
         }
 }

//=====================================================================================
//------------------------ endian conversions -----------------------------------------
//=====================================================================================

 public void _nrrdSwapShortEndian(short[] data, int N) {
   short s, fix;
   int I;

   if (data != null ) {
     for (I=0; I<=N-1; I++) {
       s = data[I];
       fix =  (short)(s & 0x00FF);
       fix = (short)(((s & 0xFF00) >> 0x08) | (fix << 0x08));
       data[I] = fix;
     }
   }
 }


 public void _nrrdSwapWordEndian(int[] data, int N) {
   int w, fix;
   int I;

   if (data != null ) {
     for (I=0; I<=N-1; I++) {
       w = data[I];
       fix =  (w & 0x000000FF);
       fix = ((w & 0x0000FF00) >> 0x08) | (fix << 0x08);
       fix = ((w & 0x00FF0000) >> 0x10) | (fix << 0x08);
       fix = ((w & 0xFF000000) >> 0x18) | (fix << 0x08);
       data[I] = fix;
     }
   }
 }


/*
 public void _nrrdSwapLongLongWordEndian(long[] _data, int N) {
   long[] data, l, fix;
   int I;

   if (_data) {
     data = _data;
     for (I=0; I<=N-1; I++) {
       l = data[I];
       fix =  (l & 0x00000000000000FF);
       fix = ((l & 0x000000000000FF00) >> 0x08) | (fix << 0x08);
       fix = ((l & 0x0000000000FF0000) >> 0x10) | (fix << 0x08);
       fix = ((l & 0x00000000FF000000) >> 0x18) | (fix << 0x08);
       fix = ((l & 0x000000FF00000000) >> 0x20) | (fix << 0x08);
       fix = ((l & 0x0000FF0000000000) >> 0x28) | (fix << 0x08);
       fix = ((l & 0x00FF000000000000) >> 0x30) | (fix << 0x08);
       fix = ((l & 0xFF00000000000000) >> 0x38) | (fix << 0x08);
       data[I] = fix;
     }
   }
 }
 */





}
