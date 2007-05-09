package gov.nih.mipav.view.multihisto.src;

import java.io.*;
import java.nio.*;
import java.io.BufferedReader;
import java.util.*;

public class MetaVolume {

  public String[] pipes = new String[32]; //which pipes are we useing??
  public int npipes; //how many pipes are available??

  public int valid; //is this metavolume ready to go??

  public int dataType; //uses enum above
  public int dataEndian;
  public String dataSetName; //not sure if this is used?
  public String nativeDataSetName;

  public int append; //append subvolume & timestep extensions
  public String path; //the path to the actual data
  public String TLUTfile; //is there a saved tlut??

  public String baneFile; //is there a semi-automatic tf for this
  public String nrrdFile; //you can test this to see if it came from a nrrd

  public int tsteps; //number of time steps
  public int tstart; //starting time step
  public int tstop; //ending timestep
  public int currentTStep; //what time step are we on?
  public int tstepCache; //number of timesteps to cache


  public int xiSize, yiSize, ziSize; //size of the whole volume in voxels
  public int xiVSize, yiVSize, ziVSize; // size of the original volume (before padding etc)
  public float xfSize, yfSize, zfSize; //size of the whole volume in volume space
  public float xfVSize, yfVSize, zfVSize; //size of the whole volume in volume space (before ...)
  public float xSpc, ySpc, zSpc; //spacings between voxels
  public int numSubVols; //number of sub-volumes (#of *volumes[]*)
  public int nelts; //number of elements in each data buffer (usualy 1)
  public int nVelts; //number of elements in original volume
  public int dims; //????

  public Volume[] volumes; //all sub volumes are kept here
  public Volume wholeVol; //the whole volume - unbricked

  public static int _DOUBLE = 0;//8byte
  public static int _FLOAT = 1; //4byte
  public static int _INT = 2;   //4byte
  public static int _UINT = 3;  //4byte
  public static int _SHORT = 4; //2byte
  public static int _USHORT = 5;//2byte
  public static int _UCHAR = 6; //1byte
  public static int _BIG_ENDIAN = 7;
  public static int _LITTLE_ENDIAN = 8;

  public static int INT_MAX =     2147483647;
  private VectorMath math = new VectorMath();

  //========================================================================
//============================= Meta Volume ==============================
//========================================================================

  public MetaVolume(String filename, int isnrrd) {
    dataSetName = null;
    nativeDataSetName = null;
    path = null;
    TLUTfile = null;
    baneFile = null;
    nrrdFile = null;
    tsteps = 0;
    tstart = 0;
    tstop = 0;
    currentTStep = 0;
    tstepCache = 0;
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
    xSpc = 1.0f;
    ySpc = 1.0f;
    zSpc = 1.0f;
    npipes = 0;
    numSubVols = 0;
    nelts = 1;
    nVelts = 1;
    dims = 3;

    wholeVol = null;

    if (isnrrd == 0) {
      if (parse(filename) < 0)
        valid = 0;
      else
        valid = 1;
    }
    else {
      nrrdFile = filename;
    }
  }

  public MetaVolume() {
    dataSetName = null;
    nativeDataSetName = null;
    path = null;
    TLUTfile = null;
    baneFile = null;
    nrrdFile = null;
    tsteps = 0;
    tstart = 0;
    tstop = 0;
    currentTStep = 0;
    tstepCache = 0;
    xiSize = 0;
    yiSize = 0;
    ziSize = 0;
    xiVSize = 0;
    yiVSize = 0;
    ziVSize = 0;
    xfSize = 0.0f;
    yfSize = 0.0f;
    zfSize = 0.0f;
    xfVSize = 0.0f;
    yfVSize = 0.0f;
    zfVSize = 0.0f;
    xSpc = 1.0f;
    ySpc = 1.0f;
    zSpc = 1.0f;
    npipes = 0;
    numSubVols = 0;
    nelts = 1;
    nVelts = 1;
    dims = 3;
    wholeVol = null;
  }

  //=============================================================== PARSE
//======================================================================

  public int parse(String filename) {
    /*
    FILE f;
    if (! (f = fopen(filename, "r"))) {
      cerr << "Error :: MetaVolume() : Could not open '"
          << filename << "' for reading.\n";
      return -1;
    }
    */
    FileReader fileIn;
    BufferedReader bufferIn;

    try {
      fileIn = new FileReader(filename);
      bufferIn = new BufferedReader(fileIn);
      String str = new String();
      String token = new String();
      String typeToken = new String();
      StringTokenizer tmp; //  = new StringTokenizer(str);

      int subv = 0;

      //default data type.
      dataType = _UCHAR;
      append = 1;

      //init pipe info
      npipes = 0;

      while ( ( str = bufferIn.readLine()) != null ) {
         tmp = new StringTokenizer(str, ":{\n");
         typeToken = tmp.nextToken();
         if ( typeToken != null  ) {
           if ( typeToken.equals("Data Type") ) {
             token = tmp.nextToken();
             if (token == null) {
               System.err.println("Data Type not read, syntax error\n");
             }
             else {
               if (token.equals("float") || token.equals("FLOAT") || token.equals("Float")) {
                 dataType = _FLOAT;
               } else if (token.equals("double") || token.equals("DOUBLE") || token.equals("Double")) {
                 dataType = _DOUBLE;
               } else if (token.equals("int") || token.equals("INT") || token.equals("Int")) {
                 dataType = _INT;
               } else if (token.equals("uint") || token.equals("UINT") || token.equals("UInt")) {
                 dataType = _UINT;
               } else if (token.equals("short") || token.equals("SHORT") || token.equals("Short")) {
                 dataType = _SHORT;
               } else if (token.equals("ushort") || token.equals("USHORT") || token.equals("UShort")) {
                 dataType = _USHORT;
               } else if (token.equals("uchar") || token.equals("UCHAR") || token.equals("Uchar")) {
                 dataType = _UCHAR;
               }
             }
           } else if ( typeToken.equals("Time Step Cache") || typeToken.equals("Time step cache") || typeToken.equals("time step cache") ) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Time Step Cache not read, syntax error\n");
             else {
               tstepCache = new Integer(token).intValue();
               System.err.println("I will cache " + tstepCache + " time steps");
             }
           } else if ( typeToken.equals("ENDIAN") || typeToken.equals("Endian") || typeToken.equals("endian") ) {
              token = tmp.nextToken();
              if ( token == null ) System.err.println("Endian not read, syntax error\n");
              else {
                if ( token.equals("BIG") || token.equals("big") || token.equals("Big") ) {
                  dataEndian = _BIG_ENDIAN;
                }
                if ( token.equals("LITTLE") || token.equals("little") || token.equals("Little") ) {
                  dataEndian = _LITTLE_ENDIAN;
                }
              }
           } else if ( typeToken.equals("Displays") ) {
             while ( tmp.hasMoreTokens() ) {
               token = tmp.nextToken();
               System.out.println(" Display requested: " + token);
               pipes[npipes++] = token;
             }
           } else if ( typeToken.equals("Don't append numbers") ) {
             append = 0;
             System.out.println("No knowledge of pre-bricking or time series");
           } else if ( typeToken.equals("Data Set Name")) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Data Set Name not read, syntax error\n");
             else {
               dataSetName = token;
             }
           } else if ( typeToken.equals("Native Data Set Name")) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Native Data Set Name not read, syntax error");
             else {
               nativeDataSetName = token;
             }
           } else if ( typeToken.equals("Data Set Files") ) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Data Set Files not read, syntax error\n");
             else {
               path = token;
             }
           } else if ( typeToken.equals("Number of Time Steps")) {
             token = tmp.nextToken();
             if ( token == null ) {
               System.err.println("Nubmer of Time Steps not read, syntax error\n");
             } else {
               tsteps = new Integer(token).intValue();
             }
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Time Step Start not read, syntax error\n");
             else {
               tstart = new Integer(token).intValue();
             }
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Time Step Stop not read, syntax error");
             else {
               tstop = new Integer(token).intValue();
             }
           } else if (  typeToken.equals("TLUT File")) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("TLUT file not read, syntax error");
             else {
               TLUTfile = token;
             }
           } else if ( typeToken.equals("Bane File") ) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Bane file not read, syntax error");
             else {
               baneFile = token;
             }
           } else if ( typeToken.equals("Nrrd File") ) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Nrrd file not read, syntax error");
             else {
               nrrdFile = token;
             }
           } else if ( typeToken.equals("Volume Size int") ) {
             token = tmp.nextToken();
             if ( token == null ) {
               System.err.println("Volume size int ( x ) net read, syntax error\n");
             } else {
               xiVSize = xiSize = new Integer(token).intValue();
             }
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Volume size int (y) not read, syntax error");
             else {
               yiVSize = yiSize = new Integer(token).intValue();
             }
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Volume size int (z) not read, syntax error");
             else {
               ziVSize = ziSize = new Integer(token).intValue();
             }
           } else if ( typeToken.equals("Volume Size float") ) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Volume size float ( x ) not read, syntax error");
             else {
               xfVSize = xfSize = new Float(token).floatValue();
             }
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Volume size float ( y ) not read, syntax error");
             else {
               yfVSize = yfSize = new Float(token).floatValue();
             }
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Volume isze float ( z ) not read, syntax error");
             else {
               zfVSize = zfSize = new Float(token).floatValue();
             }
           } else if ( typeToken.equals("Number of Sub Volumes")) {
             token = tmp.nextToken();
             if ( token == null ) System.err.println("Number of Sub Volumes not read, syntax error\n");
             else {
               numSubVols = new Integer(token).intValue();
               volumes = new Volume[numSubVols];
             }
           } else if ( typeToken.equals("SubVolume") ) {
              if ( (str = bufferIn.readLine()) == null ) {
                System.err.println("Error parsing " + filename + ": SubVolume {");
                return 0;
              }
              // tmp = str;
              if ( volumes == null ) {
                System.err.println("Error: Number of subvolumes not known");
                return 0;
              }
              while ( !str.equals("}") ) {
                tmp = new StringTokenizer(str, ":");
                typeToken = tmp.nextToken();
                if (typeToken.equals("Size int")) {
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Size int ( x ) not read, syntax error\n");
                  else {
                    volumes[subv].xiVSize = volumes[subv].xiSize = new Integer(
                        token).intValue();
                  }
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Size int ( y ) not read, syntax error\n");
                  else {
                    volumes[subv].yiVSize = volumes[subv].yiSize = new Integer(
                        token).intValue();
                  }
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Size int ( z) not read, syntex error\n");
                  else {
                    volumes[subv].ziVSize = volumes[subv].ziSize = new Integer(
                        token).intValue();
                  }
                }
                else if (typeToken.equals("Size float")) {
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Size float ( x ) not read, syntax error\n");
                  else {
                    volumes[subv].xfVSize = volumes[subv].xfSize = new Float(
                        token).floatValue();
                  }
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Size float ( y ) not read, syntax error\n");
                  else {
                    volumes[subv].yfVSize = volumes[subv].yfSize = new Float(
                        token).floatValue();
                  }
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Size float ( z) not read, syntex error\n");
                  else {
                    volumes[subv].zfVSize = volumes[subv].zfSize = new Float(
                        token).floatValue();
                  }
                }
                else if (typeToken.equals("Pos int")) {
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Pos int ( x ) not read, syntax error\n");
                  else {
                    volumes[subv].xiPos = new Integer(token).intValue();
                  }
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Pos int ( y ) not read, syntax error\n");
                  else {
                    volumes[subv].yiPos = new Integer(token).intValue();
                  }
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Pos int ( z ) not read, syntax error\n");
                  else {
                    volumes[subv].ziPos = new Integer(token).intValue();
                  }
                }
                else if (typeToken.equals("Pos float")) {
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Pos float ( x ) not read, syntax error\n");
                  else {
                    volumes[subv].xfPos = new Float(token).floatValue();
                  }
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Pos float ( y ) not read, syntax error\n");
                  else {
                    volumes[subv].yfPos = new Float(token).floatValue();
                  }
                  token = tmp.nextToken();
                  if (token == null) System.err.println(
                      "Pos float ( z ) not read, syntax error\n");
                  else {
                    volumes[subv].zfPos = new Float(token).floatValue();
                  }
                }
                else {
                  if (typeToken.equals("#") || typeToken.equals("\0") || typeToken.equals("\n")) {
                    System.err.println(
                        "Error: metaVolume:: parse() : unknown argment : '" +
                        typeToken + "'");
                  }
                }
                if ( ( str = bufferIn.readLine() ) == null) {
                  System.err.println("Error parsing " + filename + ": SubVolume {");
                  return 0;
                }
              } // While ! '}'
              ++subv;
           } // SubVolume
           else {
                  if (typeToken.equals("#") || typeToken.equals("\0") || typeToken.equals("\n")) {
                    System.err.println("Error: metaVolume:: parse() : unknown argment : '" + typeToken + "'");
                  }

           }

         }


      } // while !eof
      fileIn.close();
      for ( int v = 0; v < numSubVols; ++v ) {
        volumes[v].oldData = new byte[tstepCache][];
        volumes[v].oldGrad = new byte[tstepCache][];
        volumes[v].oldTimeStep = new int[tstepCache];
        volumes[v].tstepCache = tstepCache;
        volumes[v].oldTSpos = 0;
        for ( int t = 0; t < tstepCache; ++t ) {
          volumes[v].oldData[t] = null;
          volumes[v].oldTimeStep[t] = -1;
        }
      }

    } catch ( Exception e ) {

    }
    return 1;
  }


 //=============================================================== output
//======================================================================

 public int output(String outfile) {
   FileOutputStream fileOut;
   PrintStream printOut;
   try {

     fileOut = new FileOutputStream(outfile);
     // Connect print stream to the output stream
     printOut = new PrintStream(fileOut);

     printOut.print("Data Set Name:\t\t\t" + dataSetName + "\n");
     printOut.print("Data Set Files:\t\t\t" + path + "\n");
     printOut.print("Number of Time Steps:\t\t" + tsteps + " " + tstart + " " + tstop + "\n");
     printOut.print("TLUT File:\t\t\t" + TLUTfile + "\n");
     printOut.print("Volume Size int:\t\t" + xiSize + " " + yiSize + " " + ziSize + "\n");
     printOut.print("Volume Size float:\t\t" + xfSize + " " + yfSize + " " + zfSize + "\n\n");

     printOut.print("Number of Sub Volumes:\t" + numSubVols + "\n");

     for (int i = 0; i < numSubVols; ++i) {
       Volume v = volumes[i];
       printOut.print("\nSubVolume{\n");
       printOut.print("\tSize int:\t" + v.xiSize + " " +  v.yiSize + " " +  v.ziSize + "\n");
       printOut.print("\tSize float:\t" +  v.xfSize + " " + v.yfSize + " " +   v.zfSize + "\n");
       printOut.print("\tPos int:\t" + v.xiPos + " " + v.yiPos + " " +  v.ziPos + "\n");
       printOut.print("\tPos float:\t" + v.xfPos + " " +  v.yfPos + " " +  v.zfPos + "\n");
       printOut.print("}\n");
     }

     printOut.print("\n\n#file generated automaticly by MetaVolume.java\n");

   } catch (Exception e) {
     System.err.println("Error :: MetaVolume() : Could not open " + outfile + " for reading.\n");
     return 1;
   }
   return 0;
 }

 //=========================================================== print info
//======================================================================

 public void printInfo()
 {
    System.err.println();
    System.err.println("******** Volume Information ********");
    if(dataSetName != null)
        System.err.println("  Data Set Name: " + dataSetName);
    if(path != null)
        System.err.println("  Path to data : " + path);
    if(TLUTfile != null)
        System.err.println("  TLUT file    : " + TLUTfile);
    if(nrrdFile != null)
        System.err.println("  NRRD file    : " + nrrdFile);

    System.err.println("  Time Steps   : " + tsteps);
    System.err.println("       Start   : " + tstart);
    System.err.println("       Stop    : " + tstop);
    System.err.println("  Voxel Dim    : " + xiSize + ", " + yiSize + ", " + ziSize);
    System.err.println("  Volume Size  : " + xfSize + ", " + yfSize + ", " + zfSize);
    System.err.println("  Number of Elements : " + nelts);
    System.err.println("  Sub Volumes  : " + numSubVols);
    System.err.println();

    for(int i=0 ; i< numSubVols ; ++i){
       Volume v = volumes[i];
       System.err.println("    Sub Volume : " + i);
       System.err.println("         Voxel Dim    : " + v.xiSize + ", " + v.yiSize  + ", " + v.ziSize);
       System.err.println("         Volume Size  : " + v.xfSize + ", " + v.yfSize  + ", " + v.zfSize);
       System.err.println("         Voxel Pos    : " + v.xiPos + ", " + v.yiPos + ", " + v.ziPos);
       System.err.println("         Volume Pos   : " + v.xfPos + ", " + v.yfPos + ", " + v.zfPos);
       System.err.println();
    }
    System.err.println("************************************");
    System.err.println();
 }


 //============================================================ read Vol
//======================================================================

 public int readVol(int volNum, int timestep)
 {

         Volume volume = volumes[volNum];
         if( volume.dbData[0] == null ){ //create the volume if we haven't already
            volume.dbData[0] = new byte[(volume.xiVSize*volume.yiVSize*volume.ziVSize)];
            //cache this new volume
         } else {  //start over if we have
                 //delete[] volume->dbData[0];
                 volume.dbData[0] = new byte[(volume.xiVSize*volume.yiVSize*volume.ziVSize)];
                 xiSize = xiVSize;
                 yiSize = yiVSize;
                 ziSize = ziVSize;
                 volume.xiSize = volume.xiVSize;
                 volume.yiSize = volume.yiVSize;
                 volume.ziSize = volume.ziVSize;
                 xfSize = xfVSize;
                 yfSize = yfVSize;
                 zfSize = zfVSize;
                 volume.xfSize = volume.xfVSize;
                 volume.yfSize = volume.yfVSize;
                 volume.zfSize = volume.zfVSize;
                 nelts = nVelts;
         }


         currentTStep = timestep;

         //see if we need to append the subvolume number
         String   subfile = new String();
         if(append == 1)
                 subfile =  path + timestep + volNum;
         else
                 subfile = path;
         System.err.println("Reading: " +  subfile);
         //open the file, no special direct i/o yet
         int size = volume.xiVSize * volume.yiVSize * volume.ziVSize;

         FileInputStream in;


          try {
            in = new FileInputStream(subfile);

            //read for a specific data type
            if (dataType == _UCHAR) {
              System.err.println("--Reading single byte volume (" + size + " bytes)");
              in.read( volume.dbData[0], 0, size);
              in.close();
              byte[] d = volume.dbData[0];
              System.err.println(subfile + " was succesfully read.");
             //  volume.currentData = volume.dbData[0];
             System.arraycopy(volume.dbData[0], 0, volume.currentData, 0, size);
              return 1;
            }
            else if ( (dataType == _USHORT) || (dataType == _SHORT)) {
              size *= 2;
              System.err.println("--Reading double byte volume (" + size + " bytes)");
              volume.nativeData = new byte[size];
              int n = in.read(volume.nativeData, 0, size);
              in.close();
              System.err.println(subfile + " was succesfully read (" + n + " bytes)");
              ByteBuffer bBuffer  = ByteBuffer.wrap(volume.nativeData);
              ShortBuffer sBuffer = bBuffer.asShortBuffer();
              short[] sBuf = new short[size/2];
              sBuf = sBuffer.array();
              if (dataEndian == _BIG_ENDIAN) {
                System.err.println("Changing Endian ...");
                math._nrrdSwapShortEndian(sBuf, (size / 2));
                System.err.println("Done");
              }
              if (dataType == _USHORT)
                math.quantize(volume.dbData[0], volume.xiVSize, volume.yiVSize,volume.ziVSize, sBuf);
              else if (dataType == _SHORT)
                math.quantize(volume.dbData[0], volume.xiVSize, volume.yiVSize, volume.ziVSize, sBuf);
              // volume.currentData = volume.dbData[0];
              System.arraycopy(volume.dbData[0], 0, volume.currentData, 0, size);
              return 1;
            }
            else if ( (dataType == _UINT) || (dataType == _INT) || (dataType == _FLOAT)) {
              size *= 4;
              System.err.println("--Reading quad byte volume (" + size + " bytes)");
              volume.nativeData = new byte[size];
              int n = in.read(volume.nativeData, 0, size);
              in.close();
              System.err.println(subfile + " was succesfully read (" + n + " bytes)");
              ByteBuffer bBuffer = ByteBuffer.wrap(volume.nativeData);
              IntBuffer iBuffer = bBuffer.asIntBuffer();
              int[] iBuf = iBuffer.array();

              if (dataEndian == _BIG_ENDIAN) {
                System.err.println("Changing Endian ...");
                math._nrrdSwapWordEndian(iBuf, (size / 4));
                System.err.println("Done");
              }

              if (dataType == _FLOAT)
                math.quantize(volume.dbData[0], volume.xiVSize, volume.yiVSize, volume.ziVSize, iBuf);
              // volume.currentData = volume.dbData[0];
              System.arraycopy(volume.dbData[0], 0, volume.currentData, 0, size);
              return 1;
            }
            else if (dataType == _DOUBLE) {
              // This is a bug, even in the C++ version.
              size *= 8;
              System.err.println("--Reading 8 byte volume (" + size + " bytes)");
              volume.nativeData = new byte[size];
              int n = in.read(volume.dbData[0], 0, size);
              in.close();
              System.err.println(subfile + " was succesfully read (" +  n + " bytes)");
              // volume.currentData = volume.dbData[0];
              System.arraycopy(volume.dbData[0], 0, volume.currentData, 0, size);
              return 1;
            }
          } catch ( Exception e ) {
              System.err.println("Error: Reader::readVolume, UC read failed\n");
              return 0;
          }
         return 0;
 }


 //============================================================= read All
//======================================================================

 public int readAll(int timestep) {
   int ret = 0;
   for (int i = 0; i < numSubVols; ++i) {
     ret += readVol(i, timestep);
     if (ret == 0)return 0;
   }
   return ret;
 }

 //============================================================ swapTStep
//======================================================================
 public int swapTStep(int timestep) { //try to swap a cached timestep (0 = need to read)
   for (int i = 0; i < numSubVols; ++i) {
     int gotone = -1;
     for (int t = 0; t < tstepCache; ++t) { //search for the timestep
       if (volumes[i].oldTimeStep[t] == timestep)
         gotone = t;
     }
     if (gotone == -1) {
       System.err.println("Time step not cached");
       return 0;
     }
     else {
       if (volumes[i].oldData[gotone] == null) {
         System.err.println("BANG! should have been cached but it's not!!!");
         return 0;
       }
       System.err.println("Time step cached - swaping data :" + (int) gotone);
       volumes[i].currentData = volumes[i].oldData[gotone];           // System.arraycopy ???????????????????????????????????????
       volumes[i].currentGrad = volumes[i].oldGrad[gotone];
       volumes[i].dbData[0] = volumes[i].oldData[gotone];
       currentTStep = timestep;
     }
   }
   return 1;
 }

 //=========================================================== cacheTStep
//======================================================================
   public int cacheTStep() { //try to cache the current timestep (0=didn't happen)
   if (tstepCache > 0) {

     for (int i = 0; i < numSubVols; ++i) {
       int gotone = -1;
       for (int t = 0; t < tstepCache; ++t) { //search for the timestep
         System.err.println(" t= " +  t + " oldTimeStepVal = " + volumes[i].oldTimeStep[t]);
         if (volumes[i].oldTimeStep[t] == currentTStep) {
           gotone = t;
         }
       }
       if (gotone == -1) {
         System.err.println("Caching a time step " + volumes[i].oldTSpos);;
         volumes[i].oldTimeStep[volumes[i].oldTSpos] = currentTStep;
         volumes[i].oldData[volumes[i].oldTSpos] = volumes[i].currentData;
         volumes[i].oldGrad[volumes[i].oldTSpos] = volumes[i].currentGrad;
         volumes[i].oldTSpos = (volumes[i].oldTSpos + 1) % tstepCache;
       }
       else {
         System.err.println("Time step is already cached in " + gotone);
       }
     }
   }
   return 1;
 }



 //============================================================= writeAll
//======================================================================

 public int writeAll(String filename) {
   FileOutputStream fileOut;
   PrintStream printOut;
   String subfile = new String();
   subfile = filename;
   try {
     fileOut = new FileOutputStream(subfile);
     // Connect print stream to the output stream
     printOut = new PrintStream(fileOut);

     printOut.print("#  Meta Volume  #\n");
     printOut.print("\n\n#    Global info\n");
     printOut.print("Data Set Name:         " + filename + "\n");
     printOut.print(
         "#  NOTE: Data Set Files uses an implicit extension: filename.timestep.subvol\n");
     printOut.print("Data Set Files:        " + filename + "\n");
     //change this for time varying
     printOut.print("Number of Time Steps:  1, 0, 0\n");
     printOut.print("Volume Size int:       " + xiSize + ", " + yiSize + ", " +
                    ziSize + "\n");
     printOut.print("Volume Size float:     " + xfSize + ", " + yfSize + ", " +
                    zfSize + "\n");
     printOut.print("\n\n#    Subvolume info\n");
     printOut.print("Number of Sub Volumes: " + numSubVols + "\n");

     int n = 0;

     for (int i = 0; i < numSubVols; ++i) {
       String subvolfile = new String();
       if (append == 1) //change this for time varying
         subvolfile = filename + "." + 0 + "." + i;
       else
         subvolfile = filename;
       System.err.println("writing " + filename + " sub-volume " + i + "  as " +
                          subvolfile);
       n += volumes[i].writeVol(fileOut, subvolfile);
     }

   }
   catch (Exception e) {
     System.err.println("Error: MetaVolume::writeAll, failed to open " +
                        subfile + " for writing");
     return 0;
   }
     return 1;
  }



 //============================================================ read Nrrd
//======================================================================

 public int readNrrd(int debug)
 {
         FileReader fileIn;
         FileInputStream file;
         BufferedReader  bufferIn;
         try {
           fileIn = new FileReader(nrrdFile);
           file = new FileInputStream(nrrdFile);
           bufferIn = new BufferedReader(fileIn);
           String line;
           if (debug == 1)
             System.err.println("Nrrd info:");
           int ret = 0;
           /*
           while (strlen(fgets(line, 256, f)) > 1) {
             if (debug)
               cerr << line;
             ret += parseNrrd(line);
           } */
           int count = 0;
           while ( ( line =  bufferIn.readLine() ) != null  && line.length() != 0 ) {
              System.err.println(line);
              count += line.length();
              ret += parseNrrd(line);
           }
           count++;
           System.err.println("count = " + count);
           bufferIn.close();

           if (ret != 0)return -1;

           int n;
           //check to see if we have unsigned chars
           if (dataType == _USHORT) {
             System.out.println("WARNING: unsigned short quantization only supports scalar data");
             short[] data = new short[xiSize * yiSize * ziSize * nelts];
             byte[] bData = new byte[xiSize * yiSize * ziSize * nelts * 2];
             int size = xiSize * yiSize * ziSize * nelts * 2;
             n = file.read(bData, 0, size);  // bufferIn ??????????????????????????????????????????????
             bufferIn.close();
             ByteBuffer bBuffer = ByteBuffer.wrap(bData);
             ShortBuffer sBuffer = bBuffer.asShortBuffer();
             data = sBuffer.array();
             System.out.println("read : " + n + " bytes ");
             byte[] ucdata = new byte[xiSize * yiSize * ziSize * nelts];
             math.quantize(ucdata, xiSize, yiSize, ziSize, data);

             xfSize = xSpc * xiSize;
             yfSize = ySpc * yiSize;
             zfSize = zSpc * ziSize;
             float max = Math.max(xfSize, yfSize);
             max = Math.max(max, zfSize);
             xfSize /= max;
             yfSize /= max;
             zfSize /= max;

             numSubVols = 1;
             volumes = new Volume[1];
             volumes[0].currentData = volumes[0].dbData[0] = ucdata;
             volumes[0].xiSize = xiSize;
             volumes[0].yiSize = yiSize;
             volumes[0].ziSize = ziSize;
             volumes[0].xfSize = xfSize;
             volumes[0].yfSize = yfSize;
             volumes[0].zfSize = zfSize;
           }



           if (dataType == _UCHAR) {
             //read it
             int size = xiSize * yiSize * ziSize * nelts;
             byte[] data = new byte[xiSize * yiSize * ziSize * nelts];
             byte[] dataByte = new byte[xiSize * yiSize * ziSize * nelts];
             file.read(dataByte, 0, count);
             n = file.read(dataByte, 0, size);


             xfSize = xSpc * xiSize;
             yfSize = ySpc * yiSize;
             zfSize = zSpc * ziSize;
             float max = Math.max(xfSize, yfSize);
             max = Math.max(max, zfSize);
             xfSize /= max;
             yfSize /= max;
             zfSize /= max;

             numSubVols = 1;
             volumes = new Volume[1];
             volumes[0] = new Volume();
             // volumes[0].currentData = volumes[0].dbData[0] = data;
             volumes[0].dbData[0] = new byte[size];
             volumes[0].currentData = new byte[size];
             System.arraycopy(dataByte, 0, volumes[0].dbData[0], 0, size);
             System.arraycopy(dataByte, 0, volumes[0].currentData, 0, size);
             volumes[0].xiSize = xiSize;
             volumes[0].yiSize = yiSize;
             volumes[0].ziSize = ziSize;
             volumes[0].xfSize = xfSize;
             volumes[0].yfSize = yfSize;
             volumes[0].zfSize = zfSize;

             return n;
           }
         } catch ( Exception e ) {
           System.err.println("Error: Reader::readNrrd, failed to open "  + nrrdFile + " for reading ");
            return 0;

         }
         return 0;
  }



  public int isValid() {
    return valid;
  };


  //============================================================ Merge MVs
//======================================================================
  public int mergeMV(int blurnorms, int addG, int addH,            //add grad and hessian??
                                    MetaVolume mv1,           //combine metavolumes
                                    MetaVolume mv2,
                                    MetaVolume mv3)
  {
          int ne = nelts;
          if(addG == 1) ++ne;
          if(addH == 1){
                  if(addG == 0){
                          System.err.println("ERROR MetaVolume::mergeMV() adding hessian without grad not supported");
                          return 0;
                  }
                  ++ne;
          }
          if(mv1 != null){
                  if(numSubVols != mv1.numSubVols){
                          System.err.println("ERROR MetaVolume::mergeMV() number of Sub Volumes do not match");
                          return 0;
                  }
                  ne += mv1.nelts;
          }
          if(mv2 != null ){
                  if(numSubVols != mv2.numSubVols){
                          System.err.println("ERROR MetaVolume::mergeMV() number of Sub Volumes do not match");
                          return 0;
                  }
                  ne += mv2.nelts;
          }
          if(mv3 != null ){
                  if(numSubVols != mv3.numSubVols){
                          System.err.println("ERROR MetaVolume::mergeMV() number of Sub Volumes do not match");
                          return 0;
                  }
                  ne += mv3.nelts;
          }

          System.err.println("number of elements in new volume : " + ne);

          //copy the data from the metavolumes into new data
          for(int v=0; v<numSubVols; ++v){
                  Volume tv = volumes[v];
                  int sx = tv.xiSize;
                  int sy = tv.yiSize;
                  int sz = tv.ziSize;
                  byte[] d0 = tv.currentData;
                  byte[] d1 = null;
                  byte[] d2 = null;
                  byte[] d3 = null;
                  if(mv1 != null){
                          d1 = mv1.volumes[v].currentData;
                  }
                  if(mv2 != null){
                          d2 = mv2.volumes[v].currentData;
                  }
                  if(mv3 != null){
                          d3 = mv3.volumes[v].currentData;
                  }
                  byte[] d =  new byte[tv.xiSize*tv.yiSize*tv.ziSize*ne];
                  tv.currentData = new byte[tv.xiSize*tv.yiSize*tv.ziSize*ne];
                  d = tv.currentData;
                  // byte[] d = tv.currentData = new unsigned char[tv->xiSize*tv->yiSize*tv->ziSize*ne]; // ???????????????????????????????????

                  // copy data
                  for(int i=0; i<sz; ++i){
                          for(int j=0; j<sy; ++j){
                                  for(int k=0; k<sx; ++k){
                                          int offs = 0;  //current element offset into new data
                                          for(int e=0; e<nelts; ++e){ //copy our data
                                                  d[i*sx*sy*ne + j*sx*ne + k*ne + e] = d0[i*sx*sy*nelts + j*sx*nelts + k*nelts + e];
                                          }
                                          offs += nelts; //end our data
                                          if(mv1 != null){ //copy mv1's data
                                                  int nelts1 = mv1.nelts;
                                                  for(int e=0; e<mv1.nelts; ++e){
                                                          d[i*sx*sy*ne + j*sx*ne + k*ne + offs + e] = d1[i*sx*sy*nelts1 + j*sx*nelts1 + k*nelts1 + e];
                                                  }
                                                  offs += mv1.nelts;
                                          } //end mv1
                                          if(mv2 != null){ //copy mv2's data
                                                  int nelts2 = mv2.nelts;
                                                  for(int e=0; e<mv2.nelts; ++e){
                                                          d[i*sx*sy*ne + j*sx*ne + k*ne + offs + e] = d2[i*sx*sy*nelts2 + j*sx*nelts2 + k*nelts2 + e];
                                                  }
                                                  offs += mv2.nelts;
                                          } //end mv2
                                          if(mv3 != null){ //copy mv3's data
                                                  int nelts3 = mv3.nelts;
                                                  for(int e=0; e<mv3.nelts; ++e){
                                                          d[i*sx*sy*ne + j*sx*ne + k*ne + offs + e] = d3[i*sx*sy*nelts3 + j*sx*nelts3 + k*nelts3 + e];
                                                  }
                                                  offs += mv3.nelts;
                                          } //end mv3
                                  }//end for x
                          }//end for y
                  }//end for z
          }//end for v


          float[] grad = null;
          if(addG == 1){
                  if(numSubVols != 1){
                          System.err.println("Warning MetaVolume::mergeMV() un-bricking required, ");
                          System.err.println("       gradients will be wrong at brick boundaries");
                  }
                  for(int v=0; v< numSubVols; ++v){
                          Volume tv = volumes[v];
                          int sx = tv.xiSize;
                          int sy = tv.yiSize;
                          int sz = tv.ziSize;
                          grad = new float[sx*sy*sz*3];
                          if(addH == 1){ //gradient magnitude and hessian
                                 math.AGradArb(grad, sx, sy, sz, ne-2, ne, tv.currentData);
                                  byte[] gmag = new byte[sx*sy*sz];
                                  byte[] hess = new byte[sx*sy*sz];
                                  math.GMagHess(gmag, hess, sx, sy, sz, grad);
                                  for(int i=0; i<sz; ++i){
                                          for(int j=0; j<sy; ++j){
                                                  for(int k=0; k<sx; ++k){
                                                          tv.currentData[i*sx*sy*ne + j*sx*ne + k*ne + ne-2 + 0] =
                                                                    gmag[i*sx*sy + j*sx + k];
                                                          tv.currentData[i*sx*sy*ne + j*sx*ne + k*ne + ne-2 + 1] =
                                                                    hess[i*sx*sy + j*sx + k];
                                                  }
                                          }
                                  }
                                  gmag = null;
                                  hess = null;
                          }
                          else{ //just gradient magnitude
                                  math.AGradArb(grad, sx, sy, sz, ne-1, ne, tv.currentData);
                                  byte[] gmag = new byte[sx*sy*sz];
                                  math.GMag(gmag, sx, sy, sz, grad);
                                  for(int i=0; i<sz; ++i){
                                          for(int j=0; j<sy; ++j){
                                                  for(int k=0; k<sx; ++k){
                                                          tv.currentData[i*sx*sy*ne + j*sx*ne + k*ne + ne-1 + 0] =
                                                                    gmag[i*sx*sy + j*sx + k];
                                                  }
                                          }
                                  }
                                  gmag = null;
                          }
                          //now get the normals for shading
                          tv.currentGrad = tv.dbGrad[0] = new byte[sx*sy*sz*3];
                          if(blurnorms == 1){
                                  System.err.println("  bluring normals");
                                  math.blurV3D(grad,(float)1,(float).3,(float).2,(float).1,sx,sy,sz);
                                  System.err.println(" -done");
                          }
                          math.scalebiasN(tv.currentGrad, grad, sx, sy, sz);
                          grad = null;
                  }

          }
          if(numSubVols == 1){
                  wholeVol = volumes[0];
          }

          nelts = ne;

          return 0;
   }

   //============================================================== normals
//======================================================================

   public void normalsVGH(int blurnorms, int only1st)
   {
           /*
           if(nelts == 1){
                   for(int v =0; v< numSubVols; ++v){
                           int sx = volumes[v].xiSize;
                           int sy = volumes[v].yiSize;
                           int sz = volumes[v].ziSize;
                           if(volumes[v].currentData1){ //extra data
                                   float[] fgrad = new float[sx*sy*sz*3];      //temp float grad
                                   volumes[v].currentGrad  = new byte[sx*sy*sz*3];
                                   volumes[v].currentData2 = new byte[sx*sy*sz];
                                   //compute derivative
                                   addDer(volumes[v].currentData2,fgrad,sx,sy,sz,volumes[v].currentData,volumes[v].currentData1);
                                   scalebiasN(volumes[v].currentGrad, fgrad,sx,sy,sz);          //normalize+scale&bias
                                   delete[] fgrad;

                           } else {  //scalar volume
                                   volumes[v].currentGrad  = new byte[sx*sy*sz*3];	  //need to get a grad
                                   volumes[v].currentData1 = new byte[sx*sy*sz];      //need to get a grad mag
                                   float[] fgrad = new float[sx*sy*sz*3];      //temp float grad
                                   derivative3D(volumes[v].currentData1, fgrad, sx, sy, sz, volumes[v].currentData); //compute derivative
                                   scalebiasN(volumes[v].currentGrad, fgrad,sx,sy,sz);         //normalize+scale&bias
                                   delete[] fgrad;                           //done with temp
                           }
                   }
           } else {
              */

                   if(only1st == 1){ //VGH volume???
                           for(int v=0; v< numSubVols; ++v){
                                   int sx = volumes[v].xiSize;
                                   int sy = volumes[v].yiSize;
                                   int sz = volumes[v].ziSize;
                                   volumes[v].currentGrad = new byte[sx*sy*sz*3];
                                   float[] fgrad = new float[sx*sy*sz*3];      //temp float grad
                                   math.derivative3DVGH(fgrad,sx,sy,sz,volumes[v].currentData);
                                   if(blurnorms == 1){
                                           System.err.println("  bluring normals");
                                           math.blurV3D(fgrad,(float)1,(float).3,(float).2,(float).1,sx,sy,sz);
                                           System.err.println(" -done");
                                   }
                                   math.scalebiasN(volumes[v].currentGrad, fgrad,sx,sy,sz);
                                   fgrad = null;
                           }
                   } else {

                   }
           //}

    }

    //=============================================================== padOut
//======================================================================

    public void padOut()
    {
            for(int i =0; i<numSubVols; ++i){
                    int[] sx = new int[1];
                    int[] sy = new int[1];
                    int[] sz = new int[1];
                    sx[0] = volumes[i].xiSize;
                    sy[0] = volumes[i].yiSize;
                    sz[0] = volumes[i].ziSize;
                    if(  isPow2(sx[0]) == 0 || isPow2(sx[0]) == 0 || isPow2(sx[0]) == 0){
                            System.err.println("warning!!!! dataset is not a power of two");
                            System.err.println(" padding out...");

                            byte[] newdata0 = null;
                            byte[] newgrad  = null;

                            pow2Celing(newdata0,
                                    newgrad,
                                    volumes[i].currentData,
                                    volumes[i].currentGrad,
                                    sx,sy,sz);

                            xfSize *= (float)sx[0]/(float)xiSize;
                            yfSize *= (float)sy[0]/(float)yiSize;
                            zfSize *= (float)sz[0]/(float)ziSize;
                            xiSize = sx[0];
                            yiSize = sy[0];
                            ziSize = sz[0];
                            volumes[i].currentData  = volumes[i].dbData[i] = newdata0;
                            volumes[i].currentGrad  = volumes[i].dbGrad[i] = newgrad;
                            volumes[i].xiSize = xiSize;
                            volumes[i].yiSize = yiSize;
                            volumes[i].ziSize = ziSize;
                            volumes[i].xfSize = xfSize;
                            volumes[i].yfSize = yfSize;
                            volumes[i].zfSize = zfSize;
                    }
            }
    }

    //================================================================ Brick
//======================================================================

    public void brick(int maxsz)
    {
            if(numSubVols == 1){
                    if((xiSize*yiSize*ziSize) > maxsz){ //we have to rebrick
                            int xd = 1;
                            int yd = 1;
                            int zd = 1;
                            int sx = xiSize;
                            int sy = yiSize;
                            int sz = ziSize;
                            while(((sx/(float)xd)*(sy/(float)yd)*(sz/(float)zd)) > maxsz){
                                    if(zd > yd){
                                            if(yd > xd)
                                                    xd*=2;
                                            else
                                                    yd*=2;
                                    } else
                                            zd*=2;
                            }
                            System.err.println("NEW brick dims " + sx/(float)xd + " " + sy/(float)yd + " " + sz/(float)zd);
                            Volume tempv = volumes[0];
                            wholeVol = tempv;
                            byte[] od = tempv.currentData; //old data
                            volumes = new Volume[xd*yd*zd];   //create new volumes
                            numSubVols = xd*yd*zd;
                            int nsx = (int)(sx/(float)xd);    //new sizes
                            int nsy = (int)(sy/(float)yd);
                            int nsz = (int)(sz/(float)zd);
                            System.err.print("  Creating sub-volume : ");
                            for(int i=0; i<zd; ++i){
                                    for(int j=0; j<yd; ++j){
                                            for(int k=0; k<xd; ++k){
                                                    int cv = i*yd*xd + j*xd + k; //current (sub)volume
                                                   // System.err.print(cv + " ");
                                                   volumes[cv] = new Volume();
                                                    volumes[cv].currentData = volumes[cv].dbData[0] = new byte[nsx*nsy*nsz*nelts];
                                                    if(tempv.currentGrad != null)
                                                            volumes[cv].currentGrad = volumes[cv].dbGrad[0] = new byte[nsx*nsy*nsz*3];
                                                    volumes[cv].xiSize = nsx;
                                                    volumes[cv].yiSize = nsy;
                                                    volumes[cv].ziSize = nsz;
                                                    volumes[cv].xfSize = tempv.xfSize*(nsx/(float)sx);
                                                    volumes[cv].yfSize = tempv.yfSize*(nsy/(float)sy);
                                                    volumes[cv].zfSize = tempv.zfSize*(nsz/(float)sz);
                                                    volumes[cv].xfPos  = volumes[cv].xfSize*k;
                                                    volumes[cv].yfPos  = volumes[cv].yfSize*j;
                                                    volumes[cv].zfPos  = volumes[cv].zfSize*i;
                                                    volumes[cv].xiPos  = volumes[cv].xiSize*k;
                                                    volumes[cv].yiPos  = volumes[cv].yiSize*j;
                                                    volumes[cv].ziPos  = volumes[cv].ziSize*i;

                                                    for(int z=0; z<nsz; ++z){
                                                            for(int y=0; y<nsy; ++y){
                                                                    for(int x=0; x<nsx; ++x){
                                                                            for(int e=0; e<nelts; ++e){
                                                                                    volumes[cv].currentData[z*nsy*nsx*nelts + y*nsx*nelts + x*nelts + e] =
                                                                                      tempv.currentData[(i*nsz + z)*sx*sy*nelts + (j*nsy + y)*sx*nelts + (k*nsx + x)*nelts + e];
                                                                            }//elts
                                                                    }//x
                                                            }//y
                                                    }//z----------------------

                                                    if(tempv.currentGrad != null ){
                                                            for(int z=0; z<nsz; ++z){
                                                                    for(int y=0; y<nsy; ++y){
                                                                            for(int x=0; x<nsx; ++x){
                                                                                    for(int e=0; e<3; ++e){
                                                                                            volumes[cv].currentGrad[z*nsy*nsx*3 + y*nsx*3 + x*3 + e] =
                                                                                                    tempv.currentGrad[(i*nsz + z)*sx*sy*3 + (j*nsy + y)*sx*3 + (k*nsx + x)*3 + e];
                                                                                    }//elts
                                                                            }//x
                                                                    }//y
                                                            }//z----------------------
                                                    }


                                            }//xd
                                    }//yd
                            }//zd--------------------
                            System.err.println();
                    } //end if we had to rebrick
            } else {
                    System.err.println("Warning::Re-bricking is needed!!!, auto-re-bricking is not supported...YET");
            }
    }


    public void brick(int bx, int by, int bz)
    {
            //this doesn't copy auxilary data
            if(numSubVols == 1){
                    int xd = (int)(xiSize/(float)bx);
                    int yd = (int)(yiSize/(float)by);
                    int zd = (int)(ziSize/(float)bz);
                    int sx = xiSize;
                    int sy = yiSize;
                    int sz = ziSize;
                    Volume tempv = volumes[0];
                    wholeVol = tempv;
                    byte[] od = tempv.currentData; //old data
                    volumes = new Volume[xd*yd*zd];   //create new volumes
                    numSubVols = xd*yd*zd;
                    int nsx = bx;    //new sizes
                    int nsy = by;
                    int nsz = bz;
                    System.err.print("  Creating sub-volume : ");
                    for(int i=0; i<zd; ++i){
                            for(int j=0; j<yd; ++j){
                                    for(int k=0; k<xd; ++k){
                                            int cv = i*yd*xd + j*xd + k; //current (sub)volume
                                            System.err.print(cv + " ");
                                            volumes[cv].currentData = volumes[cv].dbData[0] = new byte[nsx*nsy*nsz*nelts];

                                            if(tempv.currentGrad != null)
                                                    volumes[cv].currentGrad = volumes[cv].dbGrad[0] = new byte[nsx*nsy*nsz*3];
                                            volumes[cv].xiSize = nsx;
                                            volumes[cv].yiSize = nsy;
                                            volumes[cv].ziSize = nsz;
                                            volumes[cv].xfSize = tempv.xfSize*(nsx/(float)sx);
                                            volumes[cv].yfSize = tempv.yfSize*(nsy/(float)sy);
                                            volumes[cv].zfSize = tempv.zfSize*(nsz/(float)sz);
                                            volumes[cv].xfPos  = volumes[cv].xfSize*k;
                                            volumes[cv].yfPos  = volumes[cv].yfSize*j;
                                            volumes[cv].zfPos  = volumes[cv].zfSize*i;
                                            volumes[cv].xiPos  = volumes[cv].xiSize*k;
                                            volumes[cv].yiPos  = volumes[cv].yiSize*j;
                                            volumes[cv].ziPos  = volumes[cv].ziSize*i;

                                            for(int z=0; z<nsz; ++z){
                                                    for(int y=0; y<nsy; ++y){
                                                            for(int x=0; x<nsx; ++x){
                                                                    for(int e=0; e<nelts; ++e){
                                                                            volumes[cv].currentData[z*nsy*nsx*nelts + y*nsx*nelts + x*nelts + e] =
                                                                                    tempv.currentData[(i*nsz + z)*sx*sy*nelts + (j*nsy + y)*sx*nelts + (k*nsx + x)*nelts + e];
                                                                    }//elts
                                                            }//x
                                                    }//y
                                            }//z----------------------

                                    } //xd
                            }//yd
                    }//zd
            } else {
                    System.err.println("Warning::Re-bricking is needed!!!, auto-re-bricking is not supported...YET");
            }

    }


    //=========================================================== parse NRRD
//======================================================================

    public int parseNrrd(String line)
    {
            StringTokenizer tmp;
            StringTokenizer spcTmp;
            String typeToken = new String();
            String token = new String();
            tmp = new StringTokenizer(line, ":");
            typeToken = tmp.nextToken();
            if(typeToken.equals("dimension")){
                    token = tmp.nextToken().trim();
                    dims = new Integer(token).intValue();
            }
            else if(typeToken.equals("sizes")){
                    // spcTmp = new StringTokenizer(tmp.trim(), " ");
                    token = tmp.nextToken().trim();
                    spcTmp = new StringTokenizer(token, " ");
                    if(dims == 3){
                          //   sscanf(tmp,"%d %d %d", &xiSize, &yiSize, &ziSize);  // ????????????????????
                          token = spcTmp.nextToken().trim();
                          xiSize = new Integer(token).intValue();
                          token = spcTmp.nextToken().trim();
                          yiSize = new Integer(token).intValue();
                          token = spcTmp.nextToken().trim();
                          ziSize = new Integer(token).intValue();
                    }
                    else if(dims == 4){
                           //  sscanf(tmp, "%d %d %d %d", &nelts, &xiSize, &yiSize, &ziSize);   ?????????????????????????
                         token = spcTmp.nextToken().trim();
                         nelts = new Integer(token).intValue();
                         token = spcTmp.nextToken().trim();
                         xiSize = new Integer(token).intValue();
                         token = spcTmp.nextToken().trim();
                         yiSize = new Integer(token).intValue();
                         token = spcTmp.nextToken().trim();
                         ziSize = new Integer(token).intValue();

                    } else {
                            System.err.println("Error parsing nrrd file: incorrect dimension: " + dims);
                      return 1;
                    }
            }
            else if(typeToken.equals("spacings")){
                    // tmp = strtok(NULL, "\n");
                    token = tmp.nextToken().trim();
                    spcTmp = new StringTokenizer(token, " ");
                    if(dims == 3){
                            // sscanf(tmp,"%f %f %f", &xSpc, &ySpc, &zSpc);
                          token = spcTmp.nextToken().trim();
                          xSpc = new Float(token).floatValue();
                          token = spcTmp.nextToken().trim();
                          ySpc = new Float(token).floatValue();
                          token = spcTmp.nextToken().trim();
                          zSpc = new Float(token).floatValue();

                    }
                    else if(dims == 4){
                            // tmp = strtok(tmp, " ");
                            // tmp = strtok(NULL, "\n");
                            // sscanf(tmp, "%f %f %f", &xSpc, &ySpc, &zSpc);  ????????????????????????
                         spcTmp.nextToken().trim();
                         token = spcTmp.nextToken().trim();
                         xSpc = new Float(token).floatValue();
                         token = spcTmp.nextToken().trim();
                         ySpc = new Float(token).floatValue();
                         token = spcTmp.nextToken().trim();
                         zSpc = new Float(token).floatValue();

                    } else {
                            System.err.println("Error parsing nrrd file: incorrect dimension: " + dims);
                            return 1;
                    }
            }
            else if(typeToken.equals("type")){
                    // tmp = strtok(NULL, "\n");
                    token = tmp.nextToken().trim();
                    if(token.equals("unsigned char") || token.equals(" unsigned char")){
                            dataType = _UCHAR;
                    }
                    else if(token.equals("unsigned short") || token.equals(" unsigned short")){
                            dataType = _USHORT;
                    }
                    else{
                            System.err.println("Error parsing nrrd file: only unsigned char/short data type supported");
                            return 1;
                    }
            }
            return 0;
     }



    //======================================================== is Power of 2
//======================================================================


    private int isPow2(int sz)
    {
            double tmp = sz/2.0;
            while(tmp > 1.0){
                    tmp = tmp/2.0;
            }
            if(tmp != 1.0) return 0;
            else return 1;
    }

    private int getPow2(int sz)
    {
            double tmp = sz/2.0;
            int i = 2;
            while(tmp > 1.0){
                    i *= 2;
                    tmp = tmp/2.0;
            }
            return i;
    }

//==================================================== make pow 2 celing
//======================================================================

    public void pow2Celing(byte[] data0,   //there must be a better way!!!!
                           byte[] grad,
                           byte[] d0,
                           byte[] g,
                           int[] sx, int[] sy, int[] sz)
    {
            int i;
            int nx = getPow2(sx[0]);
            int ny = getPow2(sy[0]);
            int nz = getPow2(sz[0]);
            System.err.println("new sizes = " + nx + " " + ny + " " + nz);
            System.err.println("old sizes = " + sx[0] + " " + sy[0] + " " + sz[0]);

            data0 = new byte[nx*ny*nz*nelts];
            if(g != null )  grad  = new byte[nx*ny*nz*3];

            for(i=0; i<nz; ++i){
                    for(int j=0; j<ny; ++j)
                            for(int k=0; k<nx; ++k)
                                    for(int l=0; l<nelts; ++l)
                                            data0[i*ny*nx*nelts + j*nx*nelts + k*nelts + l] = 0;
            }

            if(g != null ){
                    for(i=0; i<nz; ++i)
                            for(int j=0; j<ny; ++j)
                                    for(int k=0; k<nx; ++k)
                                            for(int l=0; l<3; ++l)
                                                    grad[i*ny*nx*3 + j*nx*3 + k*3 + l] = 0;
            }

            for(i=0; i<sz[0]; ++i){
                    for(int j=0; j<sy[0]; ++j)
                            for(int k=0; k<sx[0]; ++k)
                                    for(int l=0; l<nelts; ++l)
                                            data0[i*ny*nx*nelts + j*nx*nelts + k*nelts + l] =
                                                    d0[i*sy[0]*sx[0]*nelts + j*sx[0]*nelts + k*nelts + l];
            }

            if(g != null ){
                    for(i=0; i<sz[0]; ++i)
                            for(int j=0; j<sy[0]; ++j)
                                    for(int k=0; k<sx[0]; ++k)
                                            for(int l=0; l<3; ++l)
                                                    grad[i*ny*nx*3 + j*nx*3 + k*3 + l] =
                                                            g[i*sy[0]*sx[0]*3 + j*sx[0]*3 + k*3 + l];
            }


            sx[0] = nx;
            sy[0] = ny;
            sz[0] = nz;

    }


    public int hist2D(byte[] hist)
    {
            float[] ih = new float[256*256];
            int i;
            int index;
            short d1, d2;
            byte b1, b2;
            for(i=0; i<256*256; ++i){
                    ih[i] = 0;
            }
            short a1, a2;
            if((nelts >= 2) && volumes[0].currentData != null){
                    for(int v=0; v< numSubVols; ++v){
                            for(i=0; i<volumes[v].ziSize; ++i){
                                    for(int j=0; j<volumes[v].yiSize; ++j){
                                            for(int k=0; k<volumes[v].xiSize; ++k){
                                                    byte[] d = volumes[v].currentData;
                                                    // ih[new Byte(d[vox]).intValue() + new Byte(d[vox+1]).intValue()*256] += 1;
                                                    int vox = i*volumes[v].xiSize*volumes[v].yiSize*nelts + j*volumes[v].xiSize*nelts + k*nelts;
                                                    /*
                                                    b1 = d[vox];
                                                    d1 = (short)b1;
                                                    d1 = (short)(d1 & 0x00ff);

                                                    b2 = d[vox+1];
                                                    d2 = (short)b2;
                                                    d2 = (short)(d2 & 0x00ff);
                                                    // if ( d1 < 0 || d2 < 0 ) System.err.println("ruida d1 = " + d1 + " d2 = " + d2);
                                                    // System.err.println("vox = " + vox + "  d[vox] = " + d[vox] + "  d[vox+1] = " + d[vox+1]);
                                                    index  = (int)d1 + (int)d2 * 256;
                                                    // index = d[vox] + d[vox+1]*(byte)256;

                                                    ih[index] += 1;
                                                     */
                                                    /*
                                                    val = new Byte((byte)data[i]).intValue();
                   val += 128;
                   dataByte[i] =  new Integer(val).byteValue();
                                                    */

                                                    // if ( index == 0 ) ih[index] = 0;

                                                    a1 = (short)(d[vox]);
                                                    a1 = (short)(a1 & 0x00ff);
                                                    // a2 = new Byte((byte)(d[vox+1])).intValue();
                                                    // if ( a2 < 0 ) a2 = -a2;
                                                    // if ( a2 < 0 ) a2 += 128; else a2 += 127;
                                                    a2 = (short)(d[vox+1]);
                                                    a2 = (short)(a2 & 0x00ff);
                                                    ih[a1 +  a2 * 256] += 1;


                                            }
                                    }
                            }
                    }
            }

            else {
                    System.err.println("MetaVolume::hist2D, sorry this type of histogram is not implemented");
                    ih = null;
                    return 0;
            }

            float max = 0;

            for(i = 0; i< 256*256; ++i){
                    // if ( i == 0 || i == 1 )
                    // System.out.println("ih[" + i + "] = " + ih[i]);
                    ih[i] = (float)Math.log(ih[i]);
                    max = Math.max(ih[i], max);
            }

            for(i=0; i< 256*256; ++i){
                    hist[i] = new Float(ih[i]/(float)max*255f).byteValue();
            }
            ih = null;
            return 1;
    }

//-- remove leading & trailing white space -----------------------
    /*
    private char *wtspc(String str)
    {
      if (str == NULL)
        return NULL;

      // first trailing (walk to the end of the string)
      int i = 0;
      while(str[i] != '\0')  // Why not just use strlen...
        ++i;

      --i;

      while((str[i] == ' ') || (str[i] == '\t') || (str[i] == '\n')){
        str[i] = '\0';
        --i;
        if (i < 0)
          break;
      }

      // then leading
      i = 0;
      while((str[i] == ' ') || (str[i] == '\t'))
        ++i;
      return &str[i];
    }
    */
//-- remove trailing white spc ----------------------------------

    private void spcwt(String str)
    {
      if(str == null)
        return;
    }




  // public int output(String outfile); //write out the current metavolume

  // public void printInfo(); //print metavolume info to std err

}
