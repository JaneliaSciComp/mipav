package dtioverlay.utils;

import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Dec 6, 2005
 * Time: 4:44:08 PM
 * To change this template use Options | File Templates.
 */
public class RGB {
    public byte r, g, b;
    public RGB(byte R, byte G, byte B) {
        r= R; g=G; b=B;
    }
    public static RGB read(LEFileReader fp) throws IOException {
        byte r,g,b;
        r = (byte)fp.readByte();
        g = (byte)fp.readByte();
        b = (byte)fp.readByte();
        
        return new RGB(r,g,b);
    }
}
