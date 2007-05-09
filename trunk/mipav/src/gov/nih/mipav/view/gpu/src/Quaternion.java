
package gov.nih.mipav.view.gpu.src;

import java.io.*;

public class Quaternion implements Cloneable, Serializable
{
    float x, y, z, w;
    // static float EPS = 1e-5f;
    public Quaternion()
    {
      x = 0; y = 0; z = 0; w = 0;
    }

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
          System.err.println("Quaternion " + this.getClass().getName() +" :\n" + e);
              return null;
        }
  }

    Quaternion Quaternion_new(float w, float x, float y, float z)
    {
            Quaternion result = new Quaternion();

            result.x = w;
            result.y = x;
            result.z = y;
            result.w = z;

            return result;
    }



    /**
     *
     * @param angle float
     * @param axis Vector3  axis[0];
     * @return Quaternion
     */
    Quaternion Quaternion_fromAngleAxis(float angle, Vector3 axis)
    {
            Quaternion q = new Quaternion();
            Vector3 v = new Vector3();
            float l = v.Vector3_normalize(axis);

            if(l > 1e-5f){
                    l = (float)Math.sin(0.5f * angle) / l;
                    q.x = axis.x * l;
                    q.y = axis.y * l;
                    q.z = axis.z * l;
                    q.w = (float)Math.cos(0.5f * angle);
            } else {
                    q.x = 0.f;
                    q.y = 0.f;
                    q.z = 0.f;
                    q.w = 1.f;
            }

            return q;
    }

    Quaternion Quaternion_mult(Quaternion p, Quaternion q)
    {
            Quaternion result = new Quaternion();

            result.w = p.w * q.w - (p.x * q.x + p.y * q.y + p.z * q.z);
            result.x = p.w * q.x + q.w * p.x + p.y * q.z - p.z * q.y;
            result.y = p.w * q.y + q.w * p.y + p.z * q.x - p.x * q.z;
            result.z = p.w * q.z + q.w * p.z + p.x * q.y - p.y * q.x;

            return result;
    }


    /**
     *
     * @param q Quaternion
     * @param angle float[] pointer
     * @param axis Vector3[] pointer
     */
    void Quaternion_getAngleAxis(Quaternion q, float[] angle, Vector3 axis)
    {
            float d = (float)Math.sqrt(q.x*q.x + q.y*q.y + q.z*q.z);

            if(d > 1e-5f){
                    d = 1.f / d;
                    axis.x = q.x * d;
                    axis.y = q.y * d;
                    axis.z = q.z * d;
                    angle[0]   = 2.f * (float)Math.acos(q.w);
            } else {
                    axis.x = 0.f;
                    axis.y = 0.f;
                    axis.z = 1.f;
                    angle[0]  = 0.f;
            }
    }

    /**
     *
     * @param q Quaternion[] pointer
     */
    void Quaternion_normalize(Quaternion q)
    {
            float d = (float)Math.sqrt(q.w*q.w + q.x*q.x + q.y*q.y + q.z*q.z);
            if (d > 1e-5f) {
                    d = 1.f / d;
                    q.w *= d;
                    q.x *= d;
                    q.y *= d;
                    q.z *= d;
            } else {
                    q.w = 1.f;
                    q.x = q.y = q.z = 0.f;
            }
    }

    Quaternion Quaternion_inverse(Quaternion q)
    {
            Quaternion result = new Quaternion();
            float d = q.w*q.w + q.x*q.x + q.y*q.y + q.z*q.z;
            if (d > 1e-5f) {
                    d = 1.f / (float)Math.sqrt(d);
                    result.w = q.w * d;
                    result.x = -q.x * d;
                    result.y = -q.y * d;
                    result.z = -q.z * d;
            } else {
                    result.w = 1.f;
                    result.x = result.y = result.z = 0.f;
            }
            return result;
    }

    Vector3 Quaternion_multVector3(Quaternion q, Vector3 v)
    {
            Vector3 result = new Vector3();
            Vector3 u = new Vector3();
            Vector3 temp = new Vector3();
            float uu, uv;

            u.x = q.x;
            u.y = q.y;
            u.z = q.z;

            uu = temp.Vector3_dot(u, u);
            uv = temp.Vector3_dot(u,v);

            result = temp.Vector3_smult(2.f, temp.Vector3_add(temp.Vector3_smult(uv, u),
                                                       temp.Vector3_smult(q.w, temp.Vector3_cross(u, v))));
            result = temp.Vector3_add(result, temp.Vector3_smult(q.w*q.w - uu, v));

            return result;
    }


    void Quaternion_stderr(Quaternion q)
    {
            System.err.println("w = " +  q.w + " x = " +  q.x + " y = " + q.y + " z = " + q.z);
    }

}
