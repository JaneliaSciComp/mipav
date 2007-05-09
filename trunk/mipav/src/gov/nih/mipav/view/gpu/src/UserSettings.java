package gov.nih.mipav.view.gpu.src;

import java.io.*;

public class UserSettings implements Cloneable, Serializable
{
  float stepSize;
  float sliceThickness;
  float gradScale;
  float gradOffset;
  float texCoordScale;
  float isoValue;
  float clipIsoValue;
  int numIterations;
  int wireframe;
  int drawLight;
  float dummy; /* for file comatibility */
  float scatteringScale;
  float[] lightPos = new float[4];
  float[] translate = new float[3];
  float camZ;
  Quaternion camRot = new Quaternion();
  int backgroundImage;
  byte backgroundGrayVal;  // unsigned char

  /** Use serialVersionUID for interoperability. */
  // private static final long serialVersionUID = 7436115594993339188L;

  /**
   * Copies the object that extends this class. Can be slow sometimes because it actually copies (streams) to the hard
   * drive.
   *
   * @return  DOCUMENT ME!
   */
  public Object clone() {

      try {
          ByteArrayOutputStream bout = new ByteArrayOutputStream();
          ObjectOutputStream out = new ObjectOutputStream(bout);
          out.writeObject(this);

          ByteArrayInputStream bin = new ByteArrayInputStream(bout.toByteArray());
          ObjectInputStream in = new ObjectInputStream(bin);

          Object ret = in.readObject();
          in.close();
          out.close();

          bout.close();
          bin.close();

          return ret;

      } catch (Exception e) {
        System.err.println("Error UserSettings clone " + this.getClass().getName() +" :\n" + e);
            return null;
      }
  }

  /**
   * Clone itself in order to save memory.
   *
   * @return  Object
   */
  public Object cloneItself() {

      try {
          return super.clone();
      } catch (CloneNotSupportedException e) {
          System.err.println("\nException reported :\n" + e + "\n        in: \n" + this.getClass().getName());
          return null;
      }
    }

}
