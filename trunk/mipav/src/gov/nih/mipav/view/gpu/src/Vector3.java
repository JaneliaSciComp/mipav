package gov.nih.mipav.view.gpu.src;

public class Vector3 {

  static float EPS = 1e-5f;

  float x, y, z;
  public Vector3() {
    x = 0;
    y = 0;
    z = 0;
  };

  public Vector3 Vector3_new(float x, float y, float z) {
    // Vector3 result;
    Vector3 result = new Vector3();

    result.x = x;
    result.y = y;
    result.z = z;

    return result;
  }

  public Vector3 Vector3_add(Vector3 u, Vector3 v) {
    // Vector3 result;
    Vector3 result = new Vector3();

    result.x = u.x + v.x;
    result.y = u.y + v.y;
    result.z = u.z + v.z;

    return result;
  }

  public Vector3 Vector3_sub(Vector3 u, Vector3 v) {
    // Vector3 result;
    Vector3 result = new Vector3();

    result.x = u.x - v.x;
    result.y = u.y - v.y;
    result.z = u.z - v.z;

    return result;
  }

  public Vector3 Vector3_smult(float s, Vector3 v) {
    // Vector3 result;
    Vector3 result = new Vector3();

    result.x = s * v.x;
    result.y = s * v.y;
    result.z = s * v.z;

    return result;
  }

  public Vector3 Vector3_mult(Vector3 u, Vector3 v) {
    // Vector3 result;
    Vector3 result = new Vector3();

    result.x = u.x * v.x;
    result.y = u.y * v.y;
    result.z = u.z * v.z;

    return result;
  }

  public Vector3 Vector3_cross(Vector3 u, Vector3 v) {
    // Vector3 result;
    Vector3 result = new Vector3();

    result.x = u.y * v.z - u.z * v.y;
    result.y = u.z * v.x - u.x * v.z;
    result.z = u.x * v.y - u.y * v.x;

    return result;
  }

  public float Vector3_dot(Vector3 u, Vector3 v) {
    return u.x * v.x + u.y * v.y + u.z * v.z;
  }

  /**
   *
   * @param v Vector3[] pointer
   * @return float
   */
  public float Vector3_normalize(Vector3 v)
  {
         float d = (float)Math.sqrt(v.x*v.x + v.y*v.y + v.z*v.z);

         if (d > EPS) {
                 v.x /= d;
                 v.y /= d;
                 v.z /= d;
                 return d;
         } else {
                 v.x = v.y = v.z = 0.f;
                 return 0.f;
         }
  }


  public void Vector3_stderr(Vector3 v)
  {
        System.err.println("x = " +  v.x + " y = " +  v.y + " z = " +  v.z);
  }

}
