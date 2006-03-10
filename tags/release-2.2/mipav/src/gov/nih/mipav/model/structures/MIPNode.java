package gov.nih.mipav.model.structures;

/**
 * Structure to store the maximum intensity projection value for a 3d point.
 */
public class MIPNode {
  public float x, y, z;
  public float intensity;

  public MIPNode() {} // end MIPNode()

  public MIPNode(int _x, int _y, int _z, float val) {
    x = _x;
    y = _y;
    z = _z;
    intensity = val;
  } // end MIPNode(...)

} // end class MIPNode
