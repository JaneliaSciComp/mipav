package dtioverlay.utils;


import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Dec 6, 2005
 * Time: 4:44:08 PM
 * To change this template use Options | File Templates.
 */
public class XYZ extends PT {
    //public float x,y,z;
    public XYZ() {
        super(0f,0f,0f);
    }
    public XYZ(float X, float Y, float Z) {
        super(X,Y,Z);
    }
    public static XYZ read(LEFileReader fp) throws IOException {
        float x,y,z;
        x = fp.readFloat();
        y = fp.readFloat();
        z = fp.readFloat();
        return new XYZ(x,y,z);
    }

}
