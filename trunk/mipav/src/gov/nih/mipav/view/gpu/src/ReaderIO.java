package gov.nih.mipav.view.gpu.src;

import java.io.*;
import com.sun.opengl.util.*;
import java.awt.*;
import java.util.*;
import java.lang.*;

public class ReaderIO
{
  public static final int DATRAW_UCHAR = 0;
  public static final int DATRAW_FLOAT = 1;
  public static final int DATRAW_USHORT = 2;

  private ReaderObject model;

  private int[] sizes = new int[3];
  private float[] dists = new float[3];
  public int dataType;
  public int nComponents = 0;
  private byte[] image;
  // private byte[] data;

  public ReaderIO(String fName) {
    readData(fName);
    model = new ReaderObject(fName, sizes, dists, dataType, nComponents, image);
  }

  public ReaderObject getModel() {
    return model;
  }

  public void readData(String fileName)
  {
    String cp;
    String line;  // 100
    String rawFileName = null;  // 100
    String dirName;
    int size, index;
    int parseError, dataTypeSize = 0;
    FileInputStream inRaw;
    RandomAccessFile raFile;


    BufferedReader in = null;
    StringTokenizer st;
    String command;
    String formatType;
    try {
      in = new BufferedReader(new FileReader(fileName));

      while ( (line = in.readLine()) != null) {
        st = new StringTokenizer(line);
        command = st.nextToken();
        if ( command.equals("ObjectFileName:") ) {
           rawFileName = st.nextToken();
        } else if ( command.equals("Resolution:") ) {
           sizes[0] = Integer.parseInt(st.nextToken());
           sizes[1] = Integer.parseInt(st.nextToken());
           sizes[2] = Integer.parseInt(st.nextToken());
        } else if ( command.equals("SliceThickness:") ) {
           dists[0] = Float.parseFloat(st.nextToken());
           dists[1] = Float.parseFloat(st.nextToken());
           dists[2] = Float.parseFloat(st.nextToken());
        } else if ( command.equals("Format:")) {
           formatType = st.nextToken();
           if ( formatType.equals("UCHAR") ) {
             dataType = DATRAW_UCHAR;
             nComponents = 1;
             dataTypeSize = 1;
           } else if ( formatType.equals("USHORT") ) {
             dataType = DATRAW_USHORT;
             nComponents = 1;
             dataTypeSize = 2;
           } else if ( formatType.equals("FLOAT") ) {
             dataType = DATRAW_FLOAT;
             nComponents = 1;
             dataTypeSize = 4;
           }
        }
      }
        in.close();
        in = null;

        size = sizes[0] * sizes[1] * sizes[2];
        image = new byte[size * nComponents * dataTypeSize];
        // data = new byte[size * nComponents * dataTypeSize];
        index = fileName.lastIndexOf("/");
        rawFileName = fileName.substring(0, index+1) + rawFileName;
        // inRaw = new FileInputStream(rawFileName);
        // inRaw.read(image);
        raFile = new RandomAccessFile(rawFileName, "rw");
        raFile.read(image);
        raFile.close();
        int val;
        for ( int i = 0; i < size; i++ ) {
          // System.out.println("prev = " + (int)image[i]);
          val = new Byte(image[i]).intValue();
          val += 128;
          image[i] = new Integer(val).byteValue();

          // System.out.println("after = " + (int)image[i]);
        }

        raFile = null;
        // inRaw.close();
        // inRaw = null;


    } catch ( IOException e ) {
        System.out.println("finia");
    }
    System.out.println("rawFileName = " + rawFileName);

  }


  public int getDataTypeSize(int dataType)
  {
          switch(dataType) {
                  case DATRAW_UCHAR:
                          return 1;
                  case DATRAW_USHORT:
                          return 2;
                  default:
                       break;
          }
          return 0;
}
  /*
  public void main(String[] argv)
  {
      readData("head256.dat");
  }
  */

}
