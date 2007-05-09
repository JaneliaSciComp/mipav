package gov.nih.mipav.view.gpu.src;

import java.util.*;
import java.io.File;
import java.awt.*;
import java.awt.image.*;
import java.math.*;
import java.io.*;

public class PPM {

  int sx; //width
  int sy; //height
  int[] data; //the image data: one int per pixel

  /**
   * return a shade of gray
   * @param v the brightness. 0<=v<256.
   */
  public static int gray(int v, int alpha) {
    return (alpha << 24) + (v << 16) + (v << 8) + v;
  }

  private final static int CALPHA = 0, CRED = 1, CGREEN = 2, CBLUE = 3;
  private static int[] cshift = {
      24, 16, 8, 0};
  private static int[] cmask = {
      ~ ( (1 << 24) - 1), (1 << 24) - (1 << 16), (1 << 16) - 256, 255};

  /**
   * the amount of red in the pixelvalue.
   * @return a value between of 0, 255 or inbetween
   */
  public static int red(int v) {
    return (v & cmask[CRED]) >>> cshift[CRED];
  }

  /**
   * the amount of green in the pixelvalue.
   * @return a value between of 0, 255 or inbetween
   */
  public static int green(int v) {
    return (v & cmask[CGREEN]) >>> cshift[CGREEN];
  }

  /**
   * the amount of blue in the pixelvalue.
   * @return a value between of 0, 255 or inbetween
   */
  public static int blue(int v) {
    return (v & cmask[CBLUE]) >>> cshift[CBLUE];
  }

  /**
   * the amount of transparency in the pixelvalue.
   * @return a value between of 0, 255 or inbetween
   */
  public static int alpha(int v) {
    return (v & cmask[CALPHA]) >>> cshift[CALPHA];
  }

  private static int range(int min, int max, double dv) {
    int v = (int) Math.round(dv);
    if (v < min)
      return min;
    return (v >= max) ? max - 1 : (int) v;
  }

  private static int rangemod(int max, double dv) {
    int v = ( (int) Math.round(dv)) % max;
    if (v < 0)
      v += max;
    return v;
  }

  /**
   * return the pixelvalue for the indicated color
   * @param r the amount of red: 0(no red) to 1(full red)
   * @param g the amount of green: 0(no green) to 1(full green)
   * @param b the amount of red: 0(no red) to 1(full blue)
   * @param a the amount of alpha:
   *	0(completely transparent) to 1(fully opaque)
   */
  public static int dcolor(double r, double g, double b, double a) {
    return color(range(0, 256, r * 255), range(0, 256, g * 255),
                 range(0, 256, b * 255), range(0, 256, a * 255));
  }

  private static int b2i(int v) {
    return v < 0 ? v + 256 : v;
  }

  /**
   * create a pixel value
   * @param r the amount of red. 0<=r<256.
   * @param g the amount of green. 0<=g<256.
   * @param b the amount of blue. 0<=b<256.
   * @param a the amount of opacity. 0<=a<256. 0 is
   * completely transparent, 255 completely opaque.
   */
  public static int color(int r, int g, int b, int alpha) {
    int result = (alpha << 24) + (r << 16) + (g << 8) + b;
    return result;
  }

  /**
   * load an Image stored as portable pixelmap in File f.
   */
  public static PPMFile loadppm(File f) {
    int dataSize;
    // byte[] data;
    try {
      RandomAccessFile raf = new RandomAccessFile(f, "r");
      if (raf.readByte() != 'P' || raf.readByte() != '6') {
        System.out.println(f + " is not a binary PPM file");
        return null;
      }
      int sx = getnum(raf);
      int sy = getnum(raf);
      int colors = getnum(raf);
      if (colors != 255)
        System.out.println("#colors is " + colors + " but should be 255");
      int r, g, b;
      PPMFile im = new PPMFile(sx, sy, colors);
      dataSize = sx * sy * 3;
      // data = new byte[dataSize];
      im.data = new byte[dataSize];
      raf.read(im.data);

      /*
      for ( int i = 0; i < dataSize; i++ ) {
        im.data[i] = (short)(data[i] & 0xff);
      }
      */
      /*
      for (int i = 0; i < im.data.length; i++) {
        r = b2i(raf.readByte());
        g = b2i(raf.readByte());
        b = b2i(raf.readByte());
        im.data[i] = color(r, g, b, 255);
      } */

      raf.close();
      return im;
    }
    catch (Exception e) {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * store the image in file f in portable pixmap (ppm) format
   */
  public void storeppm(File f) {
    try {
      if (f.exists())
        f.delete();
      FileOutputStream raf = new FileOutputStream(f);
      raf.write( ("P6\n#mipav\n" + sx + " " + sy + "\n255\n").getBytes());
      byte[] buffer = new byte[3 * sx];
      int c = 0, d = 0;
      for (int j = 0; j < sy; j++) {
        c = 0;
        for (int i = 0; i < sx; i++) {
          buffer[c++] = (byte) red(data[d]);
          buffer[c++] = (byte) green(data[d]);
          buffer[c++] = (byte) blue(data[d++]);
        }
        raf.write(buffer);
      }
      raf.close();
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  //get a ascii integer for ppm file.
  private static int getnum(RandomAccessFile raf) throws IOException {
    String s = "";
    int c = raf.readByte();
    while (c < '0' || c > '9') {
      if (c == '#')
        while (c != '\n')
          c = raf.readByte();
      c = raf.readByte();
    }
    while (c >= '0' && c <= '9') {
      s += (char) c;
      c = raf.readByte();
    }
    return Integer.parseInt(s);
  }

}
