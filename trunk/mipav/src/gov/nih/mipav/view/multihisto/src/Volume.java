package gov.nih.mipav.view.multihisto.src;

import java.io.*;

public class Volume {


  public int subVolNum;
  public static int INT_MAX =     2147483647;
  public int xiSize, yiSize, ziSize;
  public int xiVSize, yiVSize, ziVSize; // size of the original volume (before padding etc)
  public float xfSize, yfSize, zfSize;
  public float xfVSize, yfVSize, zfVSize; // size of the original volume (before padding etc)

  public int xiPos, yiPos, ziPos;
  public float xfPos, yfPos, zfPos;

  // unsigned char * dbData[2];
  public byte[][] dbData = new byte[2][]; //points to two blocks of data
  //unsigned char *dbData1[2];     //two blocks for gradient magnitude (secondary data)
  //unsigned char *dbData2[2];     //two blocks for gradient magnitude (secondary data)
  //unsigned char *dbData3[2];     //two blocks for gradient magnitude (secondary data)
  // unsigned char * dbGrad[2];
  public byte[][] dbGrad = new byte[2][]; // two blocks for gradients (Vectors)

  // unsigned char * currentData;
  public byte[] currentData; //points to the right block of data
  //unsigned char *currentData1; //points to the right gradient
  //unsigned char *currentData2; //points to the right gradient
  //unsigned char *currentData3; //points to the right gradient
  // unsigned char * currentGrad;
  public byte[] currentGrad;

  // unsigned char * * oldData;
  public byte[][] oldData; //ring buffer for cached data

  // unsigned char * * oldGrad;
  public byte[][] oldGrad; //ring buffer for cached gradients

  // int * oldTimeStep;
  public int[] oldTimeStep;
  public int tstepCache;
  public int oldTSpos;

  public int extradivs;

  // void * nativeData;
  public byte[] nativeData;

  public int timestep;

  public Volume() {
    xiSize = INT_MAX;
    yiSize = INT_MAX;
    ziSize = INT_MAX;
    xiVSize = INT_MAX;
    yiVSize = INT_MAX;
    ziVSize = INT_MAX;
    xfSize = 0.0f;
    yfSize = 0.0f;
    zfSize = 0.0f;
    xfVSize = 0.0f;
    yfVSize = 0.0f;
    zfVSize = 0.0f;
    xiPos = 0;
    yiPos = 0;
    ziPos = 0;
    xfPos = 0.0f;
    yfPos = 0.0f;
    zfPos = 0.0f;
    subVolNum = 0;
    dbData[0] = null;
    dbData[1] = null;
    dbGrad[0] = null;
    dbGrad[1] = null;
    currentData = null;
    currentGrad = null;
    extradivs = 0;
    oldData = null;
    oldTimeStep = null;
    tstepCache = 0;
  }

  public Volume(Volume v) {
    xiSize = v.xiSize;
    yiSize = v.yiSize;
    ziSize = v.ziSize;
    xiVSize = v.xiVSize;
    yiVSize = v.yiVSize;
    ziVSize = v.ziVSize;
    xfSize = v.xfSize;
    yfSize = v.yfSize;
    zfSize = v.zfSize;
    xfVSize = v.xfVSize;
    yfVSize = v.yfVSize;
    zfVSize = v.zfVSize;
    xiPos = v.xiPos;
    yiPos = v.yiPos;
    ziPos = v.ziPos;
    xfPos = v.xfPos;
    yfPos = v.yfPos;
    zfPos = v.zfPos;
    subVolNum = v.subVolNum;
    dbData[0] = null;
    dbData[1] = null;
    dbGrad[0] = null;
    dbGrad[1] = null;
    currentData = null;
    currentGrad = null;
    extradivs = 0;
    oldData = null;
    oldTimeStep = null;
    tstepCache = v.tstepCache;
  }

  public int writeVol(FileOutputStream fileOut, String filename) {
     FileOutputStream out;
    // FileOutputStream fileOut;
    PrintStream printOut;
    try {

      // fileOut = new FileOutputStream(file);
      // Connect print stream to the output stream
      printOut = new PrintStream(fileOut);
      printOut.println("SubVolume {");
      printOut.println("\tSize int:        " + xiSize + " " + yiSize + " " + ziSize);
      printOut.println("\tSize float:      " + xfSize + " " + yfSize + " " + zfSize);
      printOut.println("\tPos int:         " + xiPos + " " + yiPos + " " + ziPos);
      printOut.println("\tPos float:       " + xfPos + " " + yfPos + " " + zfPos);
      printOut.println("}");
      printOut.close();
    }
    catch (Exception e) {
      System.err.println("Write volume file I/O error 1.");
      return 0;
    }



    int size = xiSize*yiSize*ziSize;
    try {
      out = new FileOutputStream(filename);
      out.write(currentData, 0, size);
      out.close();
      out = null;
    }
    catch (Exception e) {
      System.err.println("Write volume file I/O error 2.");
      return 0;
    }
    return 1;
  }
  /*
     unsigned int Volume::writeVol(FILE *trex, char *filename)
     {
          fprintf(trex, "SubVolume {\n");
   fprintf(trex, "\tSize int:        %d, %d, %d\n", xiSize, yiSize, ziSize);
   fprintf(trex, "\tSize float:      %f, %f, %f\n", xfSize, yfSize, zfSize);
   fprintf(trex, "\tPos int:         %d, %d, %d\n", xiPos, yiPos, ziPos);
   fprintf(trex, "\tPos float:       %f, %f, %f\n", xfPos, yfPos, zfPos);
          fprintf(trex, "}\n");

          FILE *f;
          if(!(f = fopen(filename, "wb"))){
             cerr << "Error: MetaVolume::writeAll, failed to open "
                          << filename << " for writing" << endl;
             return 0;
          }

          unsigned int n;
   n= fwrite((void *) currentData, sizeof(unsigned char), xiSize*yiSize*ziSize, f);
          if( n != (unsigned int)(xiSize*yiSize*ziSize)){
                  cerr << "Error: MetaVolume:writeVol, write failed " << n << " of " << xiSize*yiSize*ziSize << endl;
                  return n;
          }

          cerr << "   " << filename << " successfully written" << endl;

          fclose(f);

          return n;
     }
   */

}
