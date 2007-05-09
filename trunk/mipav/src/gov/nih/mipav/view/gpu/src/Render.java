package gov.nih.mipav.view.gpu.src;

import java.io.*;
import javax.imageio.stream.*;
import java.util.*;
import java.nio.*;
import java.awt.event.*;
import javax.swing.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;
import gov.nih.mipav.view.gpu.src.common.*;
import gov.nih.mipav.view.gpu.src.util.*;
import gov.nih.mipav.view.gpu.src.gleem.*;
import gov.nih.mipav.view.gpu.src.gleem.linalg.*;
import gov.nih.mipav.view.gpu.src.hdr.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

public class Render
    implements GLEventListener, KeyListener, MouseListener, MouseMotionListener {

  private int DEBUG_OPENGL = 0;

  private static float M_PI = 3.141592765f;

  private float EPS = 1e-5f;

  private String TEXTURE_MARKER = "#!";

  private String VOLUME_SHADERS_DIR = "./volumeShaders";
  private String SHADERS_DIR = "./shaders";
  private String BACKGROUND_IMAGE = "./backgrounds/chess.ppm";
  private String SPHEREMAP = "./spheremaps/grace_probe.ppm";

  private int FORCE_POWER_OF_TWO_TEXTURE = 0;
  private int NORMALIZE_VOLUME_DATA = 1;
  private int WRITE_FLOAT_GRADIENTS = 0;

  private int SOBEL = 1;
  private int GRAD_FILTER_SIZE = 5;
  private float SIGMA2 = 5.0f;

  private float MOUSE_SCALE = 10.0f;

  private String FRAG_PROG_EXT = ".fp";
  private String AA_FRAG_PROG_EXT = ".fpa";
  private String VOL_FILE_EXT = ".dat";
  private String SETTINGS_EXT = ".stg";
  private String GRADIENTS_EXT = ".grd";

  private int MAX_FRAMES = 50;

  private int WINDOW_WIDTH = 512;
  private int WINDOW_HEIGHT = 512;

  private float FOVY = 60.0f;
  private float NEAR_CLIP = 0.1f;
  private float FAR_CLIP = 3.0f;

  private float SLICE_STEP = 0.1f;
  private float MAX_SLICE_THICKNESS = 10.0f;
  private float MIN_SLICE_THICKNESS = SLICE_STEP;

  private float STEPSIZE_STEP = 0.001f;
  private float MAX_STEPSIZE = 1.0f;
  private float MIN_STEPSIZE = 0.0f;

  private float GRAD_SCALE_STEP = 0.01f;
  private float GRAD_OFFSET_STEP = 0.1f;

  private float TEX_COORD_SCALE_STEP = 0.05f;

  private float ISO_VALUE_STEP = 0.005f;

  private float SCATTERING_STEP = 0.5f;

  private float LIGHT_POS_STEP = 1.0f;

  private float AA_THRESHOLD_STEP = 0.005f;

  private String TEXTURE_TRANSFERFUNCTION = "TRANSFERFUNCTION";
  private String TEXTURE_OPTICALDENSITY = "OPTICALDENSITY";
  private String TEXTURE_BACKGROUND = "BACKGROUND";
  private String TEXTURE_CLIPVOLUME = "CLIPVOLUME";
  private String TEXTURE_SCATTERING = "SCATTERING";
  private String TEXTURE_SPHEREMAP = "SPHEREMAP";
  private String TEXTURE_VOLUME = "VOLUME";

  /* Determine the current framerate continously */
  private int CONTINOUS_FPS = 0;

  public static int MOUSE_ROTATE = 0;
  public static int MOUSE_TRANSLATE = 1;
  public static int MOUSE_DOLLY = 2;
  public static int MOUSE_MOVE_LIGHT_XY = 3;
  public static int MOUSE_MOVE_LIGHT_Z = 4;

  private GObject g;

  private ReaderIO rio;
  private ReaderObject model;

  private GLUT glut = new GLUT();
  private GLU glu = new GLU();

  private TransferEdit tf = new TransferEdit();

  private float FLT_MAX = 3.402823466e+38F;

  static int MIN_INT_STEPS = 20;
  static int ADD_LOOKUP_STEPS = 1;

  ARBFPPipeline pipeline;

  int isosurface_clipped, isosurface_scattering, isosurface_shadowed;
  int isosurface_sm, isosurface_spheremap, isosurface_transparent;
  int volume_isosurface, volume_mip, volume_refraction, volume_sm;

  String volumeFileName;
  String clipFileName;

  int volumeTexture;
  int clipTexture;
  int textureBackground;
  int spheremapTexture;
  int densityTexture;
  int scatteringTexture;
  int transferTexture;

  private int listName;
  private static int initialOp = 0;
  private static int initTETexture = 0;
  private static int initialBg = 0;

  private PPMFile imageBg = null;

  int[] volumeTextureID = new int[1];
  int[] TETextureID = new int[1];
  int[] ClipTextureID = new int[1];
  int[] BackgroundTextureID = new int[1];
  int[] SphereMapTextureID = new int[1];
  int[] OptDensityTextureID = new int[1];
  int[] ScatteringTextureID = new int[1];
  int[] temp7 = new int[1];
  int[] temp8 = new int[1];
  int[] temp9 = new int[1];
  int[] temp10 = new int[1];
  int[] temp11 = new int[1];
  int[] temp12 = new int[1];
  int[] temp13 = new int[1];
  int[] temp14 = new int[1];
  int[] temp15 = new int[1];
  int[] temp16 = new int[1];

  private boolean isUpdatePreIntTable = false;
  private boolean isUpdateOptDensityTexture = false;
  private boolean isLoadVolumeShaders = false;
  private boolean isLoadBackgroundTexture = false;
  private boolean isSaveSettings = false;
  private boolean isLoadSettings = false;


  public double LERP(double x, double y, double z) {
    return ( (x) * (z) + (y) * (1.0 - (z)));
  }

  private int getNextPowerOfTwo(int n) {
    int i;
    i = 1;
    while (i < n) {
      i *= 2;
    }
    return i;
  }

  /* get unsigned char type value */
  private byte getVoxel8(int x, int y, int z) {
    int index = z * g.numSlices[0] * g.numSlices[1] + y * g.numSlices[0] + x;
    return g.volData[index];
  }

  /* get unsigned short type value. */
  private byte getVoxel16(int x, int y, int z) {
    int index = z * g.numSlices[0] * g.numSlices[1] + y * g.numSlices[0] + x;
    return g.volData[index];
  }

  /*
  public short getUnsignedByte(ByteBuffer buf, int position) {
    return ( (short) (buf.get(position) & (short) 0xff));
  }

  public int getUnsignedShort(ByteBuffer buf, int position) {
    return (buf.getShort(position) & 0xffff);
  }
  */
  private float getVoxel(int x, int y, int z, int dataType) {
    switch (dataType) {
      case ReaderIO.DATRAW_UCHAR:
        return new Byte(getVoxel8(x, y, z)).floatValue();
      case ReaderIO.DATRAW_USHORT:
        return new Byte(getVoxel16(x, y, z)).floatValue();
      default:
        System.err.println("Unsupported data type");
        break;
    }
    return 0.0f;
  }

  private String getGradientsFilename() {
    String filename = new String();
    filename = g.basename + GRADIENTS_EXT;
    return filename;
  }

  private int loadGradients(byte[] gradients, int[] sizes, int dataType) {
    String filename;
    int size;
    FileInputStream in;

    filename = getGradientsFilename();
    if (filename == null) {
      return 0;
    }
    size = 3 * sizes[0] * sizes[1] * sizes[2] * rio.getDataTypeSize(dataType);
    try {
      in = new FileInputStream(filename);
      in.read(gradients, 0, size);
      in.close();
      in = null;
    }
    catch (Exception e) {
      System.err.println("Read gradient file I/O error.");
      return 0;
    }
    return 1;
  }

  private int saveGradients(byte[] gradients, int[] sizes, int dataType) {
    String filename;
    int size;
    FileOutputStream out;

    filename = getGradientsFilename();

    if (filename == null) {
      return 0;
    }
    size = 3 * sizes[0] * sizes[1] * sizes[2] * rio.getDataTypeSize(dataType);
    try {
      out = new FileOutputStream(filename);
      out.write(gradients, 0, size);
      out.close();
      out = null;
    }
    catch (Exception e) {
      System.err.println("Read gradient file I/O error.");
    }
    return 1;
  }

  private String getFloatGradientsFilename() {
    String floatExt = "_float";
    String filename = new String();
    filename.concat(g.basename + floatExt + GRADIENTS_EXT);
    return filename;
  }

  private void saveFloatGradients(float[] gradients, int[] sizes) {
    String filename;
    int size;
    FileOutputStream out;

    size = 3 * sizes[0] * sizes[1] * sizes[2];
    byte[] buffer = new byte[size * 4];
    int tmp;

    filename = getFloatGradientsFilename();
    try {
      out = new FileOutputStream(filename);
      for (int j = 0, i = 0; j < size; j++, i += 4) {
        tmp = Float.floatToRawIntBits(gradients[j]);
        buffer[i] = (byte) tmp;
        buffer[i + 1] = (byte) (tmp >> 8);
        buffer[i + 2] = (byte) (tmp >> 16);
        buffer[i + 3] = (byte) (tmp >> 24);
      }
      out.write(buffer, 0, size * 4);
      out.close();
      out = null;
    }
    catch (Exception e) {
      System.err.println("Read gradient file I/O error.");
    }
  }

  private void computeGradients(float[] gradients, int[] sizes, int dataType) {
    int i, j, k, dir, di, vdi, idz, idy, idx;
    int gpIndex;

    // int[][][][] weights = new int[3][3][3][3];
    int[][][][] weights = {
        {{{-1, -3, -1},
          {-3, -6, -3},
          {-1, -3, -1}},
         {{ 0,  0,  0},
          { 0,  0,  0},
          { 0,  0,  0}},
         {{ 1,  3,  1},
          { 3,  6,  3},
          { 1,  3,  1}}},
        {{{-1, -3, -1},
          { 0,  0,  0},
          { 1,  3,  1}},
         {{-3, -6, -3},
          { 0,  0,  0},
          { 3,  6,  3}},
         {{-1, -3, -1},
          { 0,  0,  0},
          { 1,  3,  1}}},
        {{{-1,  0,  1},
          {-3,  0,  3},
          {-1,  0,  1}},
         {{-3,  0,  3},
          {-6,  0,  6},
          {-3,  0,  3}},
         {{-1,  0,  1},
          {-3,  0,  3},
          {-1,  0,  1}}}
    };

    System.err.println("computing gradients ... may take a while\n");

    gpIndex = 0;
    // gp = gradients;   // Change gp to gradients.
    for (idz = 0; idz < sizes[2]; idz++) {
      for (idy = 0; idy < sizes[1]; idy++) {
        for (idx = 0; idx < sizes[0]; idx++) {
          if (SOBEL == 1) {
            if (idx > 0 && idx < sizes[0] - 1 &&
                idy > 0 && idy < sizes[1] - 1 &&
                idz > 0 && idz < sizes[2] - 1) {

              for (dir = 0; dir < 3; dir++) {
                gradients[gpIndex + dir] = 0.0f;
                for (i = -1; i < 2; i++) {
                  for (j = -1; j < 2; j++) {
                    for (k = -1; k < 2; k++) {
                      gradients[gpIndex + dir] += (float)weights[dir][i + 1][j + 1][k + 1] *
                          getVoxel(idx + i, idy + j, idz + k, dataType);
                    }
                  }
                }

                gradients[gpIndex + dir] /= 2.0f * g.sliceDists[dir];
              }
            }
            else {
              /* X-direction */
              if (idx < 1) {
                gradients[gpIndex + 0] = (getVoxel(idx + 1, idy, idz, dataType) -
                                          getVoxel(idx, idy, idz, dataType)) /
                    (g.sliceDists[0]);
              }
              else {
                gradients[gpIndex + 0] = (getVoxel(idx, idy, idz, dataType) -
                                          getVoxel(idx - 1, idy, idz, dataType)) /
                    (g.sliceDists[0]);
              }

              /* Y-direction */
              if (idy < 1) {
                gradients[gpIndex + 1] = (getVoxel(idx, idy + 1, idz, dataType) -
                                          getVoxel(idx, idy, idz, dataType)) /
                    (g.sliceDists[1]);
              }
              else {
                gradients[gpIndex + 1] = (getVoxel(idx, idy, idz, dataType) -
                                          getVoxel(idx, idy - 1, idz, dataType)) /
                    (g.sliceDists[1]);
              }

              /* Z-direction */
              if (idz < 1) {
                gradients[gpIndex + 2] = (getVoxel(idx, idy, idz + 1, dataType) -
                                          getVoxel(idx, idy, idz, dataType)) /
                    (g.sliceDists[2]);
              }
              else {
                gradients[gpIndex + 2] = (getVoxel(idx, idy, idz, dataType) -
                                          getVoxel(idx, idy, idz - 1, dataType)) /
                    (g.sliceDists[2]);
              }
            }
          }
          else {   // SOBEL != 1
            /* X-direction */
            if (idx < 1) {
              gradients[gpIndex + 0] = (getVoxel(idx + 1, idy, idz, dataType) -
                                        getVoxel(idx, idy, idz, dataType)) /
                  (g.sliceDists[0]);
            }
            else if (idx > g.numSlices[0] - 1) {
              gradients[gpIndex + 0] = (getVoxel(idx, idy, idz, dataType) -
                                        getVoxel(idx - 1, idy, idz, dataType)) /
                  (g.sliceDists[0]);
            }
            else {
              gradients[gpIndex + 0] = (getVoxel(idx + 1, idy, idz, dataType) -
                                        getVoxel(idx - 1, idy, idz, dataType)) /
                  (2.0f * g.sliceDists[0]);
            }

            /* Y-direction */
            if (idy < 1) {
              gradients[gpIndex + 1] = (getVoxel(idx, idy + 1, idz, dataType) -
                                        getVoxel(idx, idy, idz, dataType)) /
                  (g.sliceDists[1]);
            }
            else if (idy > g.numSlices[1] - 1) {
              gradients[gpIndex + 1] = (getVoxel(idx, idy, idz, dataType) -
                                        getVoxel(idx, idy - 1, idz, dataType)) /
                  (g.sliceDists[1]);
            }
            else {
              gradients[gpIndex + 1] = (getVoxel(idx, idy + 1, idz, dataType) -
                                        getVoxel(idx, idy - 1, idz, dataType)) /
                  (2.0f * g.sliceDists[1]);
            }

            /* Z-direction */
            if (idz < 1) {
              gradients[gpIndex + 2] = (getVoxel(idx, idy, idz + 1, dataType) -
                                        getVoxel(idx, idy, idz, dataType)) /
                  (g.sliceDists[2]);
            }
            else if (idz > g.numSlices[2] - 1) {
              gradients[gpIndex + 2] = (getVoxel(idx, idy, idz, dataType) -
                                        getVoxel(idx, idy, idz - 1, dataType)) /
                  (g.sliceDists[2]);
            }
            else {
              gradients[gpIndex + 2] = (getVoxel(idx, idy, idz + 1, dataType) -
                                        getVoxel(idx, idy, idz - 1, dataType)) /
                  (2.0f * g.sliceDists[2]);
            }
          } // endif
          gpIndex += 3;
        }
      }
    }

  }

  private void filterGradients(float[] gradients, int[] sizes) {
    int i, j, k, idz, idy, idx, gi, ogi, filterWidth, n;
    int[] borderDist = new int[3];
    float sum;
    float[] filteredGradients;
    float[][][][] filter;
    int size;

    System.err.println("filtering gradients ... may also take a while.");

    // since float array, not need to multiply by 4
    size = sizes[0] * sizes[1] * sizes[2] * 3;
    filteredGradients = new float[size];

    filter = new float[ (GRAD_FILTER_SIZE / 2 + 1)][GRAD_FILTER_SIZE][GRAD_FILTER_SIZE][GRAD_FILTER_SIZE];

    filterWidth = GRAD_FILTER_SIZE / 2;

    /* Compute the filter kernels */
    for (n = 0; n <= filterWidth; n++) {
      sum = 0.0f;
      for (k = -filterWidth; k <= filterWidth; k++) {
        for (j = -filterWidth; j <= filterWidth; j++) {
          for (i = -filterWidth; i <= filterWidth; i++) {
            filter[n][filterWidth + k][filterWidth + j][filterWidth + i] =
                    (float) Math.exp( - (i * i + j * j + k * k) / SIGMA2);
            sum += filter[n][filterWidth + k][filterWidth + j][filterWidth + i];

          }
        }
      }
      for (k = -filterWidth; k <= filterWidth; k++) {
        for (j = -filterWidth; j <= filterWidth; j++) {
          for (i = -filterWidth; i <= filterWidth; i++) {
            filter[n][filterWidth + k][filterWidth + j][filterWidth + i] /= sum;
          }
        }
      }
    }

    gi = 0;
    /* Filter the gradients */
    for (idz = 0; idz < sizes[2]; idz++) {
      for (idy = 0; idy < sizes[1]; idy++) {
        for (idx = 0; idx < sizes[0]; idx++) {
          borderDist[0] = Math.min(idx, sizes[0] - idx - 1);
          borderDist[1] = Math.min(idy, sizes[1] - idy - 1);
          borderDist[2] = Math.min(idz, sizes[2] - idz - 1);

          filterWidth = Math.min(GRAD_FILTER_SIZE / 2,
                                 Math.min(Math.min(borderDist[0], borderDist[1]),
                                          borderDist[2]));

          for (n = 0; n < 3; n++) {
            filteredGradients[gi] = 0.0f;
            for (k = -filterWidth; k <= filterWidth; k++) {
              for (j = -filterWidth; j <= filterWidth; j++) {
                for (i = -filterWidth; i <= filterWidth; i++) {
                  ogi = ( ( (idz + k) * sizes[1] + (idy + j)) * sizes[0] +
                         (idx + i)) * 3 + n;
                  filteredGradients[gi] += filter[filterWidth][filterWidth +
                      k][filterWidth + j][filterWidth + i] *
                      gradients[ogi];
                }
              }
            }
            gi++;
          }
        }
      }
    }

    System.arraycopy(filteredGradients, 0, gradients, 0, size);

    filteredGradients = null;
    filter = null;
  }

  private void quantize8(float[] grad, byte[] data, int index) {
    float len;
    int i;
    int index0 = index + 0;
    int index1 = index + 1;
    int index2 = index + 2;

    len = (float) Math.sqrt(grad[index0] * grad[index0] +
                            grad[index1] * grad[index1] +
                            grad[index2] * grad[index2]);

    if (len < EPS) {
      grad[index0] = grad[index1] = grad[index2] = 0.0f;
    }
    else {
      grad[index0] /= len;
      grad[index1] /= len;
      grad[index2] /= len;
    }
    /*
    for (i = 0; i < 3; i++) {
                data[i] = (unsigned char)((grad[i] + 1.0)/2.0 * UCHAR_MAX);
        }
     */

    for (i = 0; i < 3; i++) {
      data[index + i] = new Float( ( (grad[index + i] + 1.0f) / 2.0f * (0xff))).byteValue();
      // data[i] = ((char)(((grad[i] + 1.0f)/2.0f | 0xff)));
    }

  }

  private void quantize16(float[] grad, byte[] data, int index) {
    float len;
    int i;
    int index0 = index + 0;
    int index1 = index + 1;
    int index2 = index + 2;

    len = (float) Math.sqrt(grad[index0] * grad[index0] +
                            grad[index1] * grad[index1] +
                            grad[index2] * grad[index2]);

    if (len < EPS) {
      grad[index0] = grad[index1] = grad[index2] = 0.0f;
    }
    else {
      grad[index0] /= len;
      grad[index1] /= len;
      grad[index2] /= len;
    }

    for (i = 0; i < 3; i++) {
      data[index + i] = (byte) ( (grad[index + i] + 1.0f) / 2.0f * 0xffff);
      // data[i] = ((short)(((grad[i] + 1.0)/2.0 | 0xffff)));
    }
  }

  private void quantizeGradients(float[] gradientsIn, byte[] gradientsOut, int[] sizes,
                         int dataType) {
    int idx, idy, idz, di;

    di = 0;
    for (idz = 0; idz < sizes[2]; idz++) {
      for (idy = 0; idy < sizes[1]; idy++) {
        for (idx = 0; idx < sizes[0]; idx++) {
          switch (dataType) {
            case ReaderIO.DATRAW_UCHAR:
              quantize8(gradientsIn, gradientsOut, di);
              break;
            case ReaderIO.DATRAW_USHORT:
              quantize16(gradientsIn, gradientsOut, di);
              break;
            default:
              System.err.println("unsupported data type\n");
              break;
          }
          di += 3;
        }
      }
    }
  }

  private void addTexture(int texId, int texTarget, String texName) {
    int i;
    Texture[] tempTexture;

    if ( g.textures != null ) {
      tempTexture = new Texture[g.numTextures];
      for (i = 0; i < g.numTextures; i++) {
        tempTexture[i] = new Texture();
        tempTexture[i].name = g.textures[i].name;
        tempTexture[i].target = g.textures[i].target;
        tempTexture[i].id = g.textures[i].id;
        // tempTexture[i] = g.textures[i];
      }
      g.numTextures++;
      g.textures = new Texture[g.numTextures];
      for ( i = 0 ; i < g.numTextures-1; i++ ) {
         g.textures[i] = new Texture();
         g.textures[i].name = tempTexture[i].name;
         g.textures[i].target = tempTexture[i].target;
         g.textures[i].id = tempTexture[i].id;
         // g.textures[i] = tempTexture[i];
      }
    } else {   // g.texture == null;
      g.numTextures++;
      g.textures = new Texture[g.numTextures];
      g.textures[g.numTextures-1] = new Texture();
    }

    g.textures[g.numTextures - 1] = new Texture();
    g.textures[g.numTextures - 1].name = texName;
    g.textures[g.numTextures - 1].target = texTarget;
    g.textures[g.numTextures - 1].id = texId;

  }

  private Texture getTexture(String texName) {
    Texture texture;
    int i;

    for (i = 0; i < g.numTextures; i++) {
      texture = g.textures[i];
      if (texture.name == texName) {
        return texture;
      }
    }
    return null;
  }

  private void loadVolumeTexture8(GLAutoDrawable drawable) {
    // unsigned char *voldata = (unsigned char*)g.volData;
    // unsigned char *data, *gradients, *gp, min, max;
    byte[] voldata = g.volData;
    byte[] data;
    byte[] gradients;
    byte min, max;
    byte val;
    int di, vdi, idz, idy, idx, haveGradients;
    int textureSize, gradientSize;
    float[] tempGradients = null;
    int size;
    int gpindex;

    textureSize = g.volTexSizes[0] * g.volTexSizes[1] * g.volTexSizes[2] * 4 *
        rio.getDataTypeSize(ReaderIO.DATRAW_UCHAR);
    data = new byte[textureSize];
    gradientSize = g.numSlices[0] * g.numSlices[1] * g.numSlices[2] * 3 *
        rio.getDataTypeSize(ReaderIO.DATRAW_UCHAR);
    gradients = new byte[gradientSize];
    haveGradients = loadGradients(gradients, g.numSlices, ReaderIO.DATRAW_UCHAR);

    // haveGradients = 0;

    if (haveGradients == 0) {
      // Since it is float type array, does not multiply by 4.
      size = g.numSlices[0] * g.numSlices[1] * g.numSlices[2] * 3;
      tempGradients = new float[size];

      computeGradients(tempGradients, g.numSlices, ReaderIO.DATRAW_UCHAR);
      filterGradients(tempGradients, g.numSlices);
      if (WRITE_FLOAT_GRADIENTS == 1) {
        saveFloatGradients(tempGradients, g.numSlices);
      }
      quantizeGradients(tempGradients, gradients, g.numSlices, ReaderIO.DATRAW_UCHAR);
      saveGradients(gradients, g.numSlices, ReaderIO.DATRAW_UCHAR);

      tempGradients = null;
    }

    /* find min and max of scalar values */
    di = 0;
    min = (byte)0xff;
    max = 0;
    int cnt = 0;
    for (idz = 0; idz < g.numSlices[2]; idz++) {
      for (idy = 0; idy < g.numSlices[1]; idy++) {
        for (idx = 0; idx < g.numSlices[0]; idx++) {
          /*
          if ( idz == g.numSlices[2] / 2 || idy == g.numSlices[1] / 2 || idx == g.numSlices[0] / 2 ) {
             voldata[di] = (byte)255;
          } else {
            voldata[di] = 0;
          } */
          // System.out.println("voldata[" + di + "]=" + voldata[di]);
          val = voldata[di];
          // gradients[cnt++] = 0;
          // gradients[cnt++] = 0;
          // gradients[cnt++] = 0;
          if (val > max) {
            max = val;
          }
          if (val < min) {
            min = val;
          }
          di++;
        }
      }
    }

    System.err.println("min = " + (int)min + " max = " + (int)max);

    di = 0;
    vdi = 0;
    // gp = gradients;
    gpindex = 0;
    /* Pack the gradients and scalar values into a single texture */

    for (idz = 0; idz < g.volTexSizes[2]; idz++) {
      for (idy = 0; idy < g.volTexSizes[1]; idy++) {
        for (idx = 0; idx < g.volTexSizes[0]; idx++) {
          if (idx < g.numSlices[0] &&
              idy < g.numSlices[1] &&
              idz < g.numSlices[2]) {
            // memcpy( & data[di], gp, 3);
            // gp should be gradients.
            data[di] = gradients[gpindex];
            data[di + 1] = gradients[gpindex + 1];
            data[di + 2] = gradients[gpindex + 2];
            // gp += 3;
            gpindex += 3;

            if (NORMALIZE_VOLUME_DATA == 1) {
              data[di + 3] = new Float( (voldata[vdi++] - min) / (float) (max - min) * 0xff).byteValue();
            }
            else {
              data[di + 3] = new Float(voldata[vdi++]).byteValue();
            }
          }
          di += 4;
        }
      }
    }

    gradients = null;

    GL gl = drawable.getGL();
    ByteBuffer buf = ByteBuffer.wrap(data);
    // buf.order(ByteOrder.nativeOrder());
    buf.rewind();

    gl.glGenTextures(1, volumeTextureID, 0);
    gl.glBindTexture(GL.GL_TEXTURE_3D, volumeTextureID[0]);
    gl.glTexImage3D(GL.GL_TEXTURE_3D, 0, GL.GL_RGBA, g.volTexSizes[0],
                    g.volTexSizes[1], g.volTexSizes[2], 0, GL.GL_RGBA,
                    GL.GL_UNSIGNED_BYTE, buf);

    addTexture(volumeTextureID[0], GL.GL_TEXTURE_3D, TEXTURE_VOLUME);

    gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_R, GL.GL_CLAMP_TO_EDGE);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP_TO_EDGE);

    data = null;
  }

 /*
  private ShortBuffer setupBuffer(ByteBuffer buf) {
    buf.order(ByteOrder.nativeOrder());
    return buf.asShortBuffer();
  }
*/
  /** Takes the glAllocateMemoryNV memory and sets the byteOrder for the
     * ByteBuffer, then converts the Buffer into a FloatBuffer... */
    private FloatBuffer setupBuffer(ByteBuffer buf) {
        buf.order(ByteOrder.nativeOrder());
        return buf.asFloatBuffer();
    }


  private void loadVolumeTexture16(GLAutoDrawable drawable) {
    byte[] voldata = g.volData;
    byte[] data;
    byte[] gradients;
    // byte[] gp;
    int min, max, val;
    int di, vdi, idz, idy, idx, haveGradients;

    int textureSize;
    int gradientSize;
    float[] tempGradients = null;
    int size;
    int gpindex = 0;

    textureSize = g.volTexSizes[0] * g.volTexSizes[1] * g.volTexSizes[2] * 4 *
        rio.getDataTypeSize(ReaderIO.DATRAW_USHORT);

    data = new byte[textureSize];
    gradientSize = g.numSlices[0] * g.numSlices[1] * g.numSlices[2] * 3 *
        rio.getDataTypeSize(ReaderIO.DATRAW_USHORT);
    gradients = new byte[gradientSize];

    haveGradients = loadGradients(gradients, g.numSlices,
                                  ReaderIO.DATRAW_USHORT);

    if (haveGradients == 0) {
      size = g.numSlices[0] * g.numSlices[1] * g.numSlices[2] * 3;
      tempGradients = new float[size];

      computeGradients(tempGradients, g.numSlices, ReaderIO.DATRAW_USHORT);
      filterGradients(tempGradients, g.numSlices);
      if (WRITE_FLOAT_GRADIENTS == 1) {
        saveFloatGradients(tempGradients, g.numSlices);
      }
      quantizeGradients(tempGradients, gradients, g.numSlices, ReaderIO.DATRAW_USHORT);
      saveGradients(gradients, g.numSlices, ReaderIO.DATRAW_USHORT);

      tempGradients = null;
    }
    /* find min and max of scalar values */
    di = 0;
    min = (short) 0xffff;
    max = 0;
    for (idz = 0; idz < g.numSlices[2]; idz++) {
      for (idy = 0; idy < g.numSlices[1]; idy++) {
        for (idx = 0; idx < g.numSlices[0]; idx++) {
          val = voldata[di];
          if (val > max) {
            max = val;
          }
          if (val < min) {
            min = val;
          }
          di++;
        }
      }
    }
    System.err.println("min = " + min + " max = " + max);

    /* Pack the gradients and scalar values into a single texture */
    di = 0;
    vdi = 0;
    // gp = gradients;
    for (idz = 0; idz < g.volTexSizes[2]; idz++) {
      for (idy = 0; idy < g.volTexSizes[1]; idy++) {
        for (idx = 0; idx < g.volTexSizes[0]; idx++) {
          if (idx < g.numSlices[0] &&
              idy < g.numSlices[1] &&
              idz < g.numSlices[2]) {
            // confuse ????????????????
            // memcpy(&data[di], gp, 6);
            data[di] = gradients[gpindex];
            data[di + 1] = gradients[gpindex + 1];
            data[di + 2] = gradients[gpindex + 2];
            data[di + 3] = gradients[gpindex + 3];
            data[di + 4] = gradients[gpindex + 4];
            data[di + 5] = gradients[gpindex + 5];

            // gp += 3;
            gpindex += 3;
            val = voldata[vdi++];
            if (NORMALIZE_VOLUME_DATA == 1) {
              data[di + 3] = (byte) ( (val - min) / (max - min));
            }
            else {
              data[di + 3] = (byte) val;
            }
          }
          di += 4;
        }
      }
    }

    gradients = null;

    GL gl = drawable.getGL();

    ByteBuffer buf = ByteBuffer.wrap(data);
    buf.order(ByteOrder.nativeOrder());
    buf.rewind();

    gl.glGenTextures(1, volumeTextureID, 0);
    gl.glBindTexture(GL.GL_TEXTURE_3D, volumeTextureID[0]);
    gl.glTexImage3D(GL.GL_TEXTURE_3D, 0, GL.GL_RGBA16, g.volTexSizes[0],
                    g.volTexSizes[1], g.volTexSizes[2], 0, GL.GL_RGBA,
                    GL.GL_UNSIGNED_BYTE, buf);

    addTexture(volumeTextureID[0], GL.GL_TEXTURE_3D, TEXTURE_VOLUME);

    gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_S,
                       GL.GL_CLAMP_TO_EDGE);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_R,
                       GL.GL_CLAMP_TO_EDGE);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_T,
                       GL.GL_CLAMP_TO_EDGE);

    data = null;
  }

  private void loadVolumeTexture(String filename, GLAutoDrawable drawable) {
    int i, numComponents = 0, maxVolTexSize;
    float[] volSizes = new float[3];
    float maxVolSize;
    int size;
    int dataType = 0;
    /*
           readData(filename, g.numSlices, g.sliceDists, g.volData,
                         &dataType, &numComponents);
     */
    rio = new ReaderIO(filename);
    model = rio.getModel();
    g.numSlices = model.sizes;
    g.sliceDists = model.dists;

    // g.volData = ByteBuffer.allocate(model.data.length);
    // g.volData = ByteBuffer.wrap(model.data);

    g.volData = model.data;


    dataType = rio.dataType;
    numComponents = rio.nComponents;

    for (i = 0; i < 3; i++) {
      if (FORCE_POWER_OF_TWO_TEXTURE == 1) {
        g.volTexSizes[i] = getNextPowerOfTwo(g.numSlices[i]);
      }
      else {
        g.volTexSizes[i] = g.numSlices[i];
      }
    }

    size = g.volTexSizes[0] * g.volTexSizes[1] * g.volTexSizes[2];
    /*
    int val;
    for ( i = 0; i < size ; i++ ) {
      val = (int)(g.volData[i] & 0xff);
      if ( val > 15 && val < 25 ) {
        System.out.println("model.data[" + i + "] = " + val);
        break;
      }
    }
    */


    System.out.println("volume: " + g.volTexSizes[0] + " " + g.volTexSizes[1] + " " + g.volTexSizes[2]);

    if (numComponents != 1) {
      System.out.println("invalid volume data set.");
    }

    switch (dataType) {
      case ReaderIO.DATRAW_UCHAR:
        loadVolumeTexture8(drawable);
        break;
      case ReaderIO.DATRAW_USHORT:
        loadVolumeTexture16(drawable);
        break;
      default:
        System.out.println("Only 8 bit and 16 bit data format supported.");
        break;
    }

    for (i = 0; i < 3; i++) {
      volSizes[i] = g.numSlices[i] * g.sliceDists[i];
    }

    maxVolSize = Math.max(Math.max(volSizes[0], volSizes[1]), volSizes[2]);
    maxVolTexSize = Math.max(Math.max(g.volTexSizes[0], g.volTexSizes[1]),
                             g.volTexSizes[2]);

    for (i = 0; i < 3; i++) {
      g.scaleFactors[i] = maxVolSize / (g.volTexSizes[i] * g.sliceDists[i]);
      g.extents[i] = (g.numSlices[i] * g.sliceDists[i]) / maxVolSize;
      g.scaleFactorsInv[i] = 1.0f / g.scaleFactors[i];
      g.center[i] = g.extents[i] / 2.0f;
    }
    g.scaleFactorsInv[3] = 1.0f;
    g.scaleFactors[3] = 0.0f;
    g.center[3] = 0.0f;
  }

  private void updateTransferFunction() {
    int i, j, index;
    int val;

    index = 0;
    for (j = 0; j < TransferEdit.NUM_TE_ENTRIES; j++) {
      /* First four arrays are RGBA */
      for (i = 0; i < 4; i++) {
        g.tfFunc[index++] = g.dataTE[i][j];
      }
    }
  }


  private void loadTETexture(GLAutoDrawable drawable) {

    GL gl = drawable.getGL();
    if (initTETexture == 0) {
      gl.glGenTextures(1, TETextureID, 0);
      addTexture(TETextureID[0], GL.GL_TEXTURE_2D, TEXTURE_TRANSFERFUNCTION);
      initTETexture = 1;
    }

    gl.glBindTexture(GL.GL_TEXTURE_2D, TETextureID[0]);

    ByteBuffer buf = ByteBuffer.wrap(g.preIntTable);
    buf.rewind();

    gl.glTexImage2D(GL.GL_TEXTURE_2D, 0, GL.GL_RGBA,
                    TransferEdit.NUM_TE_ENTRIES, TransferEdit.NUM_TE_ENTRIES,
                    0, GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, buf);

    gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP_TO_EDGE);

  }

  private void loadClipTexture(String filename, GLAutoDrawable drawable) {
    int numComponents, i;
    int[] sizes = new int[3];
    int[] volTexSizes = new int[3];
    byte[] data;
    byte noClipData = new Integer(255).byteValue();
    int volTexSize;

    ReaderIO rin = null;
    ReaderObject md = null; ;

    int idx, idy, idz;

    if (filename == null) {
      data = new byte[1];
      data[0] = noClipData;
      for (i = 0; i < 3; i++) {
        volTexSizes[i] = 1;
      }
    }
    else {
      rin = new ReaderIO(filename);
      md = rin.getModel();

      for (i = 0; i < 3; i++) {
        if (FORCE_POWER_OF_TWO_TEXTURE == 1) {
          volTexSizes[i] = getNextPowerOfTwo(md.sizes[i]);
        }
        else {
          volTexSizes[i] = md.sizes[i];
        }
      }

      volTexSize = volTexSizes[0] * volTexSizes[1] * volTexSizes[2];

      data = new byte[volTexSize];

      if (FORCE_POWER_OF_TWO_TEXTURE == 1) {
        i = 0;
        for (idz = 0; idz < volTexSizes[2]; idz++) {
          for (idy = 0; idy < volTexSizes[1]; idy++) {
            for (idx = 0; idx < volTexSizes[0]; idx++) {
              if (idx < sizes[0] && idy < sizes[1] && idz < sizes[2]) {
                data[i] = md.data[ (idz * sizes[1] + idy) * sizes[0] + idx];
              }
              else {
                data[i] = 0;
              }
              i++;
            }
          }
        }
      }
      else {
        // memcpy(data, temp, sizes[0] * sizes[1] * sizes[2]);
        System.arraycopy(md.data, 0, data, 0,
                         volTexSizes[0] * volTexSizes[1] * volTexSizes[2]);
      }
    }


    GL gl = drawable.getGL();
    gl.glGenTextures(1, ClipTextureID, 0);
    gl.glBindTexture(GL.GL_TEXTURE_3D, ClipTextureID[0]);

    ByteBuffer buf = ByteBuffer.wrap(data);
    // buf.order(ByteOrder.nativeOrder());
    buf.rewind();
    gl.glTexImage3D(GL.GL_TEXTURE_3D, 0, GL.GL_LUMINANCE, volTexSizes[0],
                    volTexSizes[1], volTexSizes[2], 0, GL.GL_LUMINANCE,
                    GL.GL_UNSIGNED_BYTE, buf);

    addTexture(ClipTextureID[0], GL.GL_TEXTURE_3D, TEXTURE_CLIPVOLUME);

    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_R, GL.GL_CLAMP_TO_EDGE);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP_TO_EDGE);

    if (filename != null) {
      data = null;
    }
  }

  private void loadBackgroundTexture(GLAutoDrawable drawable) {
    File f = new File(BACKGROUND_IMAGE);

    ByteBuffer bBuf;

    GL gl = drawable.getGL();

    if (initialBg == 0) {
      gl.glGenTextures(1, BackgroundTextureID, 0);
      addTexture(BackgroundTextureID[0], GL.GL_TEXTURE_2D, TEXTURE_BACKGROUND);
      imageBg = PPM.loadppm(f);
      // ppmRead(BACKGROUND_IMAGE, &image);
      initialBg = 1;
    }

    gl.glBindTexture(GL.GL_TEXTURE_2D, BackgroundTextureID[0]);

    if (g.u.backgroundImage == 1) {
      bBuf = ByteBuffer.wrap(imageBg.data);
      // bBuf.order(ByteOrder.nativeOrder());
      bBuf.rewind();
      gl.glTexImage2D(GL.GL_TEXTURE_2D, 0, GL.GL_RGB, imageBg.width, imageBg.height,
                      0, GL.GL_RGB, GL.GL_UNSIGNED_BYTE, bBuf);
    }
    else {
      byte[] rgb = new byte[3];
      byte gray = g.u.backgroundGrayVal;
      // memset(rgb, g.u.backgroundGrayVal, 3);
      rgb[0] = gray;
      rgb[1] = gray;
      rgb[2] = gray;
      bBuf = ByteBuffer.wrap(rgb);
      // bBuf.order(ByteOrder.nativeOrder());
      // bBuf.order(ByteOrder.nativeOrder());
      bBuf.rewind();
      gl.glTexImage2D(GL.GL_TEXTURE_2D, 0, GL.GL_RGB, 1, 1,
                      0, GL.GL_RGB, GL.GL_UNSIGNED_BYTE, bBuf);
    }

    System.out.println("background image = " + g.u.backgroundImage);

    gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_REPEAT);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_REPEAT);
  }

  private void loadSphereMapTexture(GLAutoDrawable drawable) {
    PPMFile image = null;

    File f = new File(SPHEREMAP);
    ByteBuffer buf;

    GL gl = drawable.getGL();
    gl.glGenTextures(1, SphereMapTextureID, 0);
    image = PPM.loadppm(f);

    buf = ByteBuffer.wrap(image.data);
    // buf.order(ByteOrder.nativeOrder());
    buf.rewind();

    gl.glBindTexture(GL.GL_TEXTURE_2D, SphereMapTextureID[0]);
    gl.glTexImage2D(GL.GL_TEXTURE_2D, 0, GL.GL_RGB, image.width, image.height,
                    0, GL.GL_RGB, GL.GL_UNSIGNED_BYTE, buf);

    addTexture(SphereMapTextureID[0], GL.GL_TEXTURE_2D, TEXTURE_SPHEREMAP);

    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_REPEAT);
    gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_REPEAT);

    System.out.println("finish sphere map.");
  }

  private void vertex(float x, float y, float z, GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glMultiTexCoord3f(GL.GL_TEXTURE0, x, y, z);
    gl.glVertex3f(x, y, z);
  }

  private void vertexv(double[] coords, GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    DoubleBuffer dbuf = DoubleBuffer.wrap(coords);
    dbuf.rewind();
    gl.glMultiTexCoord3dv(GL.GL_TEXTURE0, coords, 0);
    gl.glVertex3dv(dbuf);
  }

  private void drawQuads(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();

    // gl.glColor3f(1.0f, 0.0f, 0.0f);
    gl.glBegin(GL.GL_QUADS);
    /* Back side */
    gl.glNormal3f(0.0f, 0.0f, -1.0f);
    gl.glMultiTexCoord4f(GL.GL_TEXTURE5, 0.0f, 0.0f, -1.0f, 0.0f);

    vertex(0.0f, 0.0f, 0.0f, drawable);
    vertex(0.0f, g.extents[1], 0.0f, drawable);
    vertex(g.extents[0], g.extents[1], 0.0f, drawable);
    vertex(g.extents[0], 0.0f, 0.0f, drawable);

    /* Front side */
   // gl.glColor3f(0.0f, 1.0f, 0.0f);
    gl.glNormal3f(0.0f, 0.0f, 1.0f);
    gl.glMultiTexCoord4f(GL.GL_TEXTURE5, 0.0f, 0.0f, 1.0f, 0.0f);
    vertex(0.0f, 0.0f, g.extents[2], drawable);
    vertex(g.extents[0], 0.0f, g.extents[2], drawable);
    vertex(g.extents[0], g.extents[1], g.extents[2], drawable);
    vertex(0.0f, g.extents[1], g.extents[2], drawable);

    /* Top side */
    // gl.glColor3f(0.0f, 0.0f, 0.0f);
    gl.glNormal3f(0.0f, 1.0f, 0.0f);
    gl.glMultiTexCoord4f(GL.GL_TEXTURE5, 0.0f, 1.0f, 0.0f, 0.0f);
    vertex(0.0f, g.extents[1], 0.0f, drawable);
    vertex(0.0f, g.extents[1], g.extents[2], drawable);
    vertex(g.extents[0], g.extents[1], g.extents[2], drawable);
    vertex(g.extents[0], g.extents[1], 0.0f, drawable);

    /* Bottom side */
    gl.glNormal3f(0.0f, -1.0f, 0.0f);
    gl.glMultiTexCoord4f(GL.GL_TEXTURE5, 0.0f, -1.0f, 0.0f, 0.0f);
    vertex(0.0f, 0.0f, 0.0f, drawable);
    vertex(g.extents[0], 0.0f, 0.0f, drawable);
    vertex(g.extents[0], 0.0f, g.extents[2], drawable);
    vertex(0.0f, 0.0f, g.extents[2], drawable);

    /* Left side */
    gl.glNormal3f( -1.0f, 0.0f, 0.0f);
    gl.glMultiTexCoord4f(GL.GL_TEXTURE5, -1.0f, 0.0f, 0.0f, 0.0f);
    vertex(0.0f, 0.0f, 0.0f, drawable);
    vertex(0.0f, 0.0f, g.extents[2], drawable);
    vertex(0.0f, g.extents[1], g.extents[2], drawable);
    vertex(0.0f, g.extents[1], 0.0f, drawable);

    /* Right side */
    gl.glNormal3f(1.0f, 0.0f, 0.0f);
    gl.glMultiTexCoord4f(GL.GL_TEXTURE5, 1.0f, 0.0f, 0.0f, 0.0f);
    vertex(g.extents[0], 0.0f, 0.0f, drawable);
    vertex(g.extents[0], g.extents[1], 0.0f, drawable);
    vertex(g.extents[0], g.extents[1], g.extents[2], drawable);
    vertex(g.extents[0], 0.0f, g.extents[2], drawable);
    gl.glEnd();
  }

  private void drawLight(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glDisable(GL.GL_LIGHTING);

    gl.glColor4f(1.0f, 1.0f, 0.0f, 1.0f);

    gl.glDisable(GL.GL_CULL_FACE);

    gl.glPushMatrix();
    gl.glLoadIdentity();
    gl.glTranslatef(0.0f, 0.0f, -g.u.camZ);
    gl.glBegin(GL.GL_LINES);
    gl.glVertex3f(0.0f, 0.0f, 0.0f);
    gl.glVertex3f(g.u.lightPos[0], g.u.lightPos[1], g.u.lightPos[2]);
    gl.glEnd();
    gl.glTranslatef(g.u.lightPos[0], g.u.lightPos[1], g.u.lightPos[2]);
    glut.glutSolidSphere(0.1d, 8, 8);
    gl.glPopMatrix();
  }

  private void drawWireframe(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_LINE);

    gl.glDisable(GL.GL_CULL_FACE);

    gl.glColor3f(2.0f, 3.0f, 5.0f);
    drawQuads(drawable);

    gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_FILL);
  }

  private void setLight(GLAutoDrawable drawable) {
    float[] transLight = new float[4];
    float[] invMVM = new float[16];
    Vector3 axis = new Vector3();
    float[] angle = new float[1];
    GL gl = drawable.getGL();
    Quaternion q = new Quaternion();

    q.Quaternion_getAngleAxis(g.u.camRot, angle, axis);

    gl.glPushMatrix();
    gl.glLoadIdentity();
    gl.glTranslatef(g.center[0], g.center[1], g.center[2]);
    gl.glRotatef( -angle[0] * 180.0f / M_PI, axis.x, axis.y, axis.z);
    gl.glRotatef( -g.animatedAngle, 0.0f, 1.0f, 0.0f);
    gl.glTranslatef(0.0f, 0.0f, g.u.camZ);
    gl.glTranslatef( -g.u.translate[0], -g.u.translate[1], -g.u.translate[2]);

    gl.glGetFloatv(GL.GL_MODELVIEW_MATRIX, invMVM, 0);

    gl.glPopMatrix();

    transLight[0] = invMVM[0] * g.u.lightPos[0] +
        invMVM[4] * g.u.lightPos[1] +
        invMVM[8] * g.u.lightPos[2] +
        invMVM[12];
    transLight[1] = invMVM[1] * g.u.lightPos[0] +
        invMVM[5] * g.u.lightPos[1] +
        invMVM[9] * g.u.lightPos[2] +
        invMVM[13];
    transLight[2] = invMVM[2] * g.u.lightPos[0] +
        invMVM[6] * g.u.lightPos[1] +
        invMVM[10] * g.u.lightPos[2] +
        invMVM[14];
    transLight[3] = 1.0f;

    gl.glPushMatrix();
    gl.glLoadIdentity();
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, transLight, 0);
    gl.glPopMatrix();
  }

  private void raycastBackground(int destinationBuffer, GLAutoDrawable drawable) {
    double[] modelview = new double[16];
    double[] projection = new double[16];
    double[][] vertizes = new double[4][3];
    int[] viewport = new int[4];
    double x, y, z;
    int i;
    GL gl = drawable.getGL();

    float texelHeight = 1.0f / (float) g.windowHeight;
    float texelWidth = 1.0f / (float) g.windowWidth;

    activateARBProg(g.currentFragProg, drawable);

    gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_FILL);
    gl.glEnable(GL.GL_CULL_FACE);

    gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB, 1,
                                    g.u.texCoordScale, 0.0f, 0.0f, 0.0f);

    gl.glProgramLocalParameter4fvARB(GL.GL_FRAGMENT_PROGRAM_ARB, 2,
                                     g.center, 0);

    gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB, 3,
                                    g.extents[0] * g.scaleFactors[0],
                                    g.extents[1] * g.scaleFactors[1],
                                    g.extents[2] * g.scaleFactors[2],
                                    0.0f);

    gl.glProgramLocalParameter4fvARB(GL.GL_FRAGMENT_PROGRAM_ARB, 5,
                                     g.scaleFactors, 0);

    gl.glProgramLocalParameter4fvARB(GL.GL_FRAGMENT_PROGRAM_ARB, 6,
                                     g.scaleFactorsInv, 0);

    gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB, 7,
                                    (float) (1.0 - texelWidth) /
                                    (float) g.windowWidth,
                                    (float) (1.0 - texelHeight) /
                                    (float) g.windowHeight,
                                    0.5f * texelWidth, 0.5f * texelHeight);

    gl.glGetIntegerv(GL.GL_VIEWPORT, viewport, 0);
    gl.glGetDoublev(GL.GL_MODELVIEW_MATRIX, modelview, 0);
    gl.glGetDoublev(GL.GL_PROJECTION_MATRIX, projection, 0);

    for (i = 0; i < 4; i++) {
      x = ( (i & 1) >> 0) * viewport[2];
      y = ( (i & 2) >> 1) * viewport[3];
      z = .5;

      glu.gluUnProject(x, y, z, modelview, 0, projection, 0, viewport, 0, vertizes[i], 0);
    }

    gl.glBegin(GL.GL_QUADS);
    vertexv(vertizes[0], drawable);
    vertexv(vertizes[1],drawable);
    vertexv(vertizes[3], drawable);
    vertexv(vertizes[2], drawable);
    gl.glEnd();

    deactivateARBProg(g.currentFragProg, drawable);

    gl.glDrawBuffer(GL.GL_BACK);
  }

  private void renderVolume(GLAutoDrawable drawable) {
    float texelHeight = 1.0f / (float) g.windowHeight;
    float texelWidth = 1.0f / (float) g.windowWidth;

    GL gl = drawable.getGL();

    gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB, 1,
                                    g.u.texCoordScale, g.u.numIterations,
                                    g.u.isoValue, 0);

    gl.glProgramLocalParameter4fvARB(GL.GL_FRAGMENT_PROGRAM_ARB, 2,
                                     g.center, 0);

    gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB, 3,
                                    g.extents[0] * g.scaleFactors[0],
                                    g.extents[1] * g.scaleFactors[1],
                                    g.extents[2] * g.scaleFactors[2],
                                    0.0f);

    gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB, 4,
                                    g.u.scatteringScale, g.u.clipIsoValue,
                                    0.0f, 0.0f);

    gl.glProgramLocalParameter4fvARB(GL.GL_FRAGMENT_PROGRAM_ARB, 5,
                                     g.scaleFactors, 0);

    gl.glProgramLocalParameter4fvARB(GL.GL_FRAGMENT_PROGRAM_ARB, 6,
                                     g.scaleFactorsInv, 0);

    gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB, 0, g.u.stepSize,
                                    g.u.gradScale, g.u.gradOffset, -5.0f);

    gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB, 7,
                                    (float) (1.0 - texelWidth) / (float) g.windowWidth,
                                    (float) (1.0 - texelHeight) /(float) g.windowHeight,
                                    0.5f * texelWidth, 0.5f * texelHeight);

    drawQuads(drawable);

  }

  private void SetDisplayList(GLAutoDrawable drawable)
  {
         float dist;
         int i;
         int iWidth = g.volTexSizes[0];
         int iHeight = g.volTexSizes[1];
         int iDepth = g.volTexSizes[2];
         int nHSlices =
             (int)(1.3d*
                   (Math.sqrt((double)(iWidth*iWidth +
                                       iHeight*iHeight +
                                       iDepth*iDepth)))/4.0d);
         int nSlices = 2*nHSlices + 1;
         int nElements = nSlices*4;
         float[] vPoints = new float[3*nElements];
         float dDist = (float)(Math.sqrt(3.0)/nSlices);
         float s = 0.5f;
         int count = 0;

         GL gl = drawable.getGL();
         for (i = -nHSlices; i <= nHSlices; i++) {
             dist     = i*dDist;

             vPoints[count++] = -s;
             vPoints[count++] = -s;
             vPoints[count++] =  dist;

             vPoints[count++] =  s;
             vPoints[count++] = -s;
             vPoints[count++] =  dist;

             vPoints[count++] =  s;
             vPoints[count++] =  s;
             vPoints[count++] =  dist;

             vPoints[count++] = -s;
             vPoints[count++] =  s;
             vPoints[count++] =  dist;
         }

         gl.glEnableClientState(GL.GL_VERTEX_ARRAY);
         FloatBuffer vBuf = setupBuffer(gl.glAllocateMemoryNV(3*nElements*4, 0, 0, 1.f));
         vBuf.put(vPoints, 0, 3*nElements);
         vBuf.rewind();
         gl.glVertexPointer(3, GL.GL_FLOAT, 0, vBuf);
         listName = gl.glGenLists(1);
         gl.glNewList(listName, GL.GL_COMPILE);
         gl.glDrawArrays(GL.GL_QUADS, 0, nElements);
         gl.glEndList();
         vPoints = null;
         System.gc();
    }

  private void raycastVolume(GLAutoDrawable drawable) {
    activateARBProg(g.currentFragProg, drawable);
    GL gl = drawable.getGL();
    gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_FILL);
    gl.glDisable(GL.GL_DEPTH_TEST);
    gl.glEnable(GL.GL_CULL_FACE);

    renderVolume(drawable);

    deactivateARBProg(g.currentFragProg, drawable);
  }

  public void display(GLAutoDrawable drawable) {
    int frameCount = 0;
    double t0, t1;
    Vector3 axis = new Vector3();
    float[] angle = new float[1];
    Quaternion q = new Quaternion();
    GL gl = drawable.getGL();

    if ( isLoadSettings ) {
      loadSettings(drawable);
    }

    if ( isSaveSettings ) {
      saveSettings();
    }
    if ( isLoadBackgroundTexture ) {
      loadBackgroundTexture(drawable);
    }
    if ( isLoadVolumeShaders ) {
       loadVolumeShaders(drawable);
    }

    if (  isUpdateOptDensityTexture ) {
      updateOptDensityTexture(drawable);
    }
    if ( isUpdatePreIntTable ) {
      // updateOptDensityTexture(drawable);
      updatePreIntTable(drawable);
    }
    if (g.initialized == 0) {
      gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
      gl.glViewport(0, 0, g.windowWidth, g.windowHeight);

      gl.glMatrixMode(GL.GL_PROJECTION);
      gl.glLoadIdentity();

      if ( true ) {
        glu.gluPerspective(FOVY, (float) g.windowWidth / (float) g.windowHeight,
                           NEAR_CLIP, FAR_CLIP);
      } else {
        glu.gluPerspective(FOVY, (float) g.windowWidth / (float) g.windowHeight,
                        g.u.camZ - 1.0, g.u.camZ + 1.0);
      }
      tf.resizeTE(drawable);
      // CHECK_FOR_OGL_ERROR();
      g.initialized = 1;
    }

    // CHECK_FOR_OGL_ERROR();

    if (frameCount == 0) {
      //   t0 = timer();
    }

    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();

    if ( true ) {
      glu.gluPerspective(FOVY, (float) g.windowWidth / (float) g.windowHeight,
                         NEAR_CLIP, FAR_CLIP);
    } else {
      glu.gluPerspective(FOVY, (float) g.windowWidth / (float) g.windowHeight,
                         g.u.camZ - 1.0, g.u.camZ + 1.0);
    }
    gl.glMatrixMode(GL.GL_MODELVIEW);
    gl.glLoadIdentity();

    gl.glTranslatef(g.u.translate[0], g.u.translate[1], g.u.translate[2]);
    gl.glTranslatef(0.0f, 0.0f, -g.u.camZ);
    gl.glRotatef(g.animatedAngle, 0.0f, 1.0f, 0.0f);
    q.Quaternion_getAngleAxis(g.u.camRot, angle, axis);
    // System.out.println("axis.x = " + axis.x + "  axis.y = " + axis.y + "  axis.z = " + axis.z);
    gl.glRotatef(angle[0] * 180.0f / M_PI, axis.x, axis.y, axis.z);
    gl.glTranslatef( -g.center[0], -g.center[1], -g.center[2]);

    setLight(drawable);

    // CHECK_FOR_OGL_ERROR();
    gl.glDisable(GL.GL_DEPTH_TEST);
    gl.glClear(GL.GL_DEPTH_BUFFER_BIT);
    raycastBackground(GL.GL_BACK, drawable);
    raycastVolume(drawable);

    if (g.u.wireframe == 1) {
      drawWireframe(drawable);
    }

    if (g.u.drawLight == 1) {
      drawLight(drawable);
    }

    /* Draw transfer function editor */
    tf.drawTE(drawable); // tf reference.
    drawable.swapBuffers();
    // glut.glutSwapBuffers();

    frameCount++;

  }

  private void updatePreIntTable(GLAutoDrawable drawable) {
    updateTransferFunction();

    calcPreIntTable(TransferEdit.NUM_TE_ENTRIES, g.u.sliceThickness, g.tfFunc,
                    g.preIntTable);

    loadTETexture(drawable);
  }

  public void calcPreIntTable(int imgsize, float thickness, byte[] data,
                              byte[] table) {
    int sb, sf, i, j, n, base1, base2, offset;
    double stepWidth, s, temp;
    double[] rgba = new double[4];
    double[] rgbac = new double[4];

    byte val;
    for (sb = 0; sb < imgsize; sb++) {
      for (sf = 0; sf <= sb; sf++) {
        n = MIN_INT_STEPS + ADD_LOOKUP_STEPS * Math.abs(sb - sf);

        stepWidth = thickness / n;
        // memset(rgba, 0, sizeof(rgba));
        rgba = new double[4];
        for (i = 0; i < n; i++) {
          s = sf + (sb - sf) * (double) i / n;
          // offset = ( sf != sb );
          if (sf != sb) {
            offset = 1;
          }
          else {
            offset = 0;
          }

          // System.err.println("sb = " + sb + "sf = " + sf + " n = " + n + " s = " + s);

          // data[(int)(s) * 4 + i]

          rgbac[3] = stepWidth / 255.0 *
              LERP(  new Byte(data[ (int) (s) * 4 + 3]).doubleValue(),
                   new Byte(data[ (int) (s + offset) * 4 + 3]).doubleValue(), s - Math.floor(s));

          // System.err.println("lerp = " +  LERP((double)data[(int)(s) * 4 + 3], (double)data[(int)(s+1) * 4 + 3], s - Math.floor(s))/255.0);

          /* Standard optical model: RGB densities are multiplied with
           * opacity density */
          temp = Math.exp( -rgba[3]) * rgbac[3] / 255.0;

          // System.err.println("exp = " +  Math.exp(-rgba[3]), rgba[3]);

          for (j = 0; j < 3; j++) {
            rgbac[j] = temp *
                LERP( new Byte(data[ (int) (s) * 4 + j]).doubleValue(),
                      new Byte(data[ (int) (s + offset) * 4 + j]).doubleValue(),
                     s - Math.floor(s));
            rgba[j] += rgbac[j];
          }
          rgba[3] += rgbac[3];
        }

        base1 = (sb * imgsize + sf) * 4;
        base2 = (sf * imgsize + sb) * 4;
        for (i = 0; i < 3; i++) {
          if (rgba[i] > 1.0) {
            rgba[i] = 1.0;
          }
          // table[base1++] = table[base2++] = (unsigned char)(rgba[i] * 255.99);
          val = new Double( (rgba[i] * 255.99)).byteValue();
          table[base1++] = table[base2++] = val;
        }
        // table[base1] = table[base2] = (unsigned char)((1.0 - exp(-rgba[3])) * 255.99);
        val = new Double( (1.0 - Math.exp( -rgba[3])) * 255.99).byteValue();
        table[base1] = table[base2] = val;
      }
    }
  }




  private void updateOptDensityTexture(GLAutoDrawable drawable) {

    GL gl = drawable.getGL();
    if (initialOp == 0) {
      gl.glGenTextures(1, OptDensityTextureID, 0);
      addTexture(OptDensityTextureID[0], GL.GL_TEXTURE_1D, TEXTURE_OPTICALDENSITY);
      initialOp = 1;
    }

    // Ruida, arraycopy starting address
    // memcpy(g.optDensity, g.dataTE[4], TransferEdit.NUM_TE_ENTRIES);
    System.arraycopy(g.dataTE[4], 0, g.optDensity, 0, TransferEdit.NUM_TE_ENTRIES);

    ByteBuffer buf = ByteBuffer.wrap(g.optDensity);
    buf.rewind();


    gl.glBindTexture(GL.GL_TEXTURE_1D, OptDensityTextureID[0]);
    gl.glTexImage1D(GL.GL_TEXTURE_1D, 0, GL.GL_LUMINANCE, TransferEdit.NUM_TE_ENTRIES,
                 0, GL.GL_LUMINANCE, GL.GL_UNSIGNED_BYTE, buf);
    gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
    gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);

    gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);
    // System.out.println("finish update density texture.");
  }

  private void initScatteringTexture(GLAutoDrawable drawable) {
    String line;
    byte[] image = null;

    GL gl = drawable.getGL();
    int numEntries = 0;

    BufferedReader in = null;
    String filename = "./colortables/colortable_wax.tab";
    String command;
    StringTokenizer st;
    FileInputStream infileByte;

    try {
      in = new BufferedReader(new FileReader(filename));
      infileByte = new FileInputStream(filename);
      line = in.readLine();
      st = new StringTokenizer(line);
      command = st.nextToken();
      numEntries = Integer.parseInt(command);
      image = new byte[numEntries * 3];
      infileByte.read(image, 0, numEntries * 3);
      in.close();
      infileByte.close();
    }
    catch (IOException e) {
      System.err.println("reading scattering colortable failed.");
    }

    in = null;
    gl.glGenTextures(1, ScatteringTextureID, 0);
    gl.glBindTexture(GL.GL_TEXTURE_1D, ScatteringTextureID[0]);

    ByteBuffer buf = ByteBuffer.wrap(image);
    // buf.order(ByteOrder.nativeOrder());
    buf.rewind();

    gl.glTexImage1D(GL.GL_TEXTURE_1D, 0, GL.GL_RGB, numEntries, 0, GL.GL_RGB,
                    GL.GL_UNSIGNED_BYTE, buf);

    addTexture(ScatteringTextureID[0], GL.GL_TEXTURE_1D, TEXTURE_SCATTERING);
    gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);

  }

  private void printARBProgs(int[] numProgs, ARBProgram[] progs) {
    ARBProgram prog;
    int i, j;

    for (i = 0; i < numProgs[0] && i < 12; i++) {
      prog = progs[i];
      System.out.println( (i + 1) + "  " + prog.filename + "   " + prog.size +
                         "  type = " + prog.programType);

      for (j = 0; j < prog.numTextureObjects; j++) {
        System.out.println("texture unit = " +
                           prog.textureObjects[j].texUnit + "   name = " +
                           prog.textureObjects[j].texture.name);
      }

    }

    System.out.println("\n");
  }

  private void texActivate(int numTextureObjects,
                           TextureObject[] textureObjects, GLAutoDrawable drawable) {
    int i;
    GL gl = drawable.getGL();
    // System.out.println("numTextureObjects = " + numTextureObjects);
    for (i = 0; i < numTextureObjects; i++) {
      gl.glActiveTexture(GL.GL_TEXTURE0 + textureObjects[i].texUnit);
      // gl.glEnable(textureObjects[i].texture.target);
      gl.glBindTexture(textureObjects[i].texture.target,
                       textureObjects[i].texture.id);
    }
  }

  private void texDeactivate(int numTextureObjects,
                             TextureObject[] textureObjects, GLAutoDrawable drawable) {
    int i;
    GL gl = drawable.getGL();
    for (i = 0; i < numTextureObjects; i++) {
      gl.glActiveTexture(GL.GL_TEXTURE0 + textureObjects[i].texUnit);
      // gl.glDisable(textureObjects[i].texture.target);
    }
  }

  private void activateARBProg(ARBProgram prog, GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    texActivate(prog.numTextureObjects, prog.textureObjects, drawable);
    gl.glEnable(prog.programType);
    gl.glBindProgramARB(prog.programType, prog.id[0]);
  }

  private void deactivateARBProg(ARBProgram prog, GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    if (prog == null) {
      return;
    }
    texDeactivate(prog.numTextureObjects, prog.textureObjects, drawable);
    gl.glDisable(prog.programType);
    gl.glBindProgramARB(prog.programType, 0);
  }

  private void loadARBProgs(String directory, String extension,
                            int numTextures, Texture[] textures,
                            int[] numProgs, ARBProgram[] progs, GLAutoDrawable drawable) {
    // progs = new ARBProgram[10];
    File dir = new File(directory);
    String[] fileNames;
    fileNames = dir.list();

    numProgs[0] = 10;

    for (int i = 0; i < 10; i++) {
      progs[i] = loadARBProg(fileNames[i], numTextures, textures, drawable);
    }

  }

  private ARBProgram loadARBProg(String filename, int numTextures,
                                 Texture[] textures, GLAutoDrawable drawable) {
    String fileNm = VOLUME_SHADERS_DIR + "/" + filename;
    FileInputStream in;
    ARBProgram[] prog = new ARBProgram[1];
    prog[0] = new ARBProgram();
    try {
      in = new FileInputStream(fileNm);
      prog[0].prog  = FileUtils.loadStreamIntoString(in);
    } catch (IOException e) {
      e.printStackTrace();
      throw new RuntimeException("Error loading shaders", e);
    }

    // System.out.println(prog[0].prog);
    // prog[0].prog = generateFragmentProgram(VOLUME_SHADERS_DIR, filename);
    prog[0].size = prog[0].prog.length();
    prog[0].id = new int[1];
    prog[0].filename = filename;
    // System.out.println("filename = " + filename);
    getFragmentTexture(prog, filename, numTextures, textures);
    loadProgram(prog, numTextures, textures, filename, drawable);

    return prog[0];

  }

  private void getFragmentTexture(ARBProgram[] prog, String filename,
                                  int numTextures, Texture[] textures) {
    String fileName;
    prog[0].numTextureObjects = 0;
    // prog[0].textureObjects = null;

    BufferedReader in = null;
    fileName = VOLUME_SHADERS_DIR + "/" + filename;
    String line;
    StringTokenizer st;
    String textureName;
    int textureUnit;
    int index;
    int found;
    int i;

    try {
      in = new BufferedReader(new FileReader(fileName));
      while ( (line = in.readLine()) != null) {
        if (line.startsWith(TEXTURE_MARKER)) {
          prog[0].numTextureObjects++;
          index = prog[0].numTextureObjects - 1;
          st = new StringTokenizer(line);
          st.nextToken();
          textureName = st.nextToken();
          st.nextToken();
          textureUnit = Integer.parseInt(st.nextToken());

          prog[0].textureObjects[index] = new TextureObject();
          prog[0].textureObjects[index].texUnit = textureUnit;

          found = 0;
          for (i = 0; i < numTextures; i++) {
            if (textureName.equals(textures[i].name)) {
              prog[0].textureObjects[index].texture = textures[i];
              found = 1;
              break;
            }
          }

          if (found != 1) {
            System.err.println("invavlid texture name " + textureName +
                               " in program " + prog[0].filename);
            // System.exit(1);
          }
        }
      }
      in.close();
    }
    catch (IOException e) {
      e.printStackTrace();
      System.err.println("reading scattering colortable failed.");
    }
    in = null;
  }

  private int loadProgram(ARBProgram[] prog, int numTextures,
                          Texture[] textures, String filename, GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    String errMsg;
    int[] errPos = new int[1];
    int index;
    String name;

    prog[0].programType = GL.GL_FRAGMENT_PROGRAM_ARB;

    /*
         if ( prog[0].programType == GL.GL_FRAGMENT_PROGRAM_ARB) {
            int[] isNative = new int[1];
            gl.glGetProgramivARB(GL.GL_FRAGMENT_PROGRAM_ARB,
                                 GL.GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB,
                                 isNative, 0);
            if (isNative[0] != 1) {
              System.out.println(
                  "WARNING: fragment program is over native resource limits");
              Thread.dumpStack();
            }
         }
     */
    gl.glEnable(prog[0].programType);

    index = filename.indexOf(".");
    name = filename.substring(0, index);

    gl.glGenProgramsARB(1, prog[0].id, 0);

    // System.out.println("prog[0].id = "+ prog[0].id[0]);

    gl.glBindProgramARB(prog[0].programType, prog[0].id[0]);
    gl.glProgramStringARB(prog[0].programType, GL.GL_PROGRAM_FORMAT_ASCII_ARB,
                          prog[0].size, prog[0].prog);
    gl.glGetIntegerv(GL.GL_PROGRAM_ERROR_POSITION_ARB, errPos, 0);

    if (errPos[0] >= 0) {
      errMsg = gl.glGetString(GL.GL_PROGRAM_ERROR_STRING_ARB);
      if (errMsg == null) {
        System.out.println("[No error message available]");
      }
      else {
        System.out.println("Error message: \"" + errMsg + "\"");
      }
      System.out.println("Error occurred at position " + errPos[0] +
                         " in program:");
      int endPos = errPos[0];
      while (endPos < prog[0].prog.length() &&
             prog[0].prog.charAt(endPos) != '\n') {
        ++endPos;
      }
      System.out.println(prog[0].prog.substring(errPos[0], endPos));
      return 0;
    }
    gl.glDisable(prog[0].programType);
    return 1;
  }

  private void loadVolumeShaders(GLAutoDrawable drawable) {

    g.fragProgs = new ARBProgram[10];
    loadARBProgs(VOLUME_SHADERS_DIR, FRAG_PROG_EXT,
                 g.numTextures, g.textures,
                 g.numFragProgs, g.fragProgs, drawable);
    // printARBProgs(g.numFragProgs, g.fragProgs);

    g.currentFragProg = g.fragProgs[0];
    // System.out.println(" id = " + g.currentFragProg.prog);
  }

  private String generateFragmentProgram(String dir, String fgName) {
    BufferedReader in = null;
    String filename = dir + "/" + fgName;
    StringBuffer buf = new StringBuffer();
    String line;
    int index;

    try {
      in = new BufferedReader(new FileReader(filename));
      while ( (line = in.readLine()) != null) {
        buf.append( line + "\n");
      }
      in.close();
    }
    catch (IOException e) {
      e.printStackTrace();
      System.err.println("reading fragment program failed.");
    }
    in = null;
    return buf.toString();
  }

  /*
     private int loadProgram(GL gl, int target, String code) {
    int prog_id;
    int[] tmp = new int[1];
    gl.glGenProgramsARB(1, tmp, 0);
    prog_id = tmp[0];
    gl.glBindProgramARB(target, prog_id);
    int size = code.length();
    gl.glProgramStringARB(target, GL.GL_PROGRAM_FORMAT_ASCII_ARB, code.length(), code);
    int[] errPos = new int[1];
    gl.glGetIntegerv(GL.GL_PROGRAM_ERROR_POSITION_ARB, errPos, 0);
    if (errPos[0] >= 0) {
      String kind = "Program";
      if (target == GL.GL_VERTEX_PROGRAM_ARB) {
        kind = "Vertex program";
      } else if (target == GL.GL_FRAGMENT_PROGRAM_ARB) {
        kind = "Fragment program";
      }
      System.out.println(kind + " failed to load:");
      String errMsg = gl.glGetString(GL.GL_PROGRAM_ERROR_STRING_ARB);
      if (errMsg == null) {
        System.out.println("[No error message available]");
      } else {
        System.out.println("Error message: \"" + errMsg + "\"");
      }
   System.out.println("Error occurred at position " + errPos[0] + " in program:");
      int endPos = errPos[0];
      while (endPos < code.length() && code.charAt(endPos) != '\n') {
        ++endPos;
      }
      System.out.println(code.substring(errPos[0], endPos));
      throw new GLException("Error loading " + kind);
    } else {
      if (target == GL.GL_FRAGMENT_PROGRAM_ARB) {
        int[] isNative = new int[1];
        gl.glGetProgramivARB(GL.GL_FRAGMENT_PROGRAM_ARB,
                             GL.GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB,
                             isNative, 0);
        if (isNative[0] != 1) {
   System.out.println("WARNING: fragment program is over native resource limits");
          Thread.dumpStack();
        }
      }
    }
    return prog_id;
     }
   */

  private void freeVolumeShaders() {
    /*
       ARBProgram *prog;
       int i;

       for (i = 0; i < g.numFragProgs; i++) {
               prog = &g.fragProgs[i];
               glDeleteProgramsARB(1, &prog->id);
               free(prog->filename);
               free(prog->prog);
       }
       free(g.fragProgs);
     */
  }

  private void initGObject() {
    int idx;
    g = new GObject();
    g.initialized = 0;
    g.numTextures = 0;
    // g.textures;
    /*
    for (int i = 0; i < 2000; i++) {
      g.textures[i] = new Texture();
    }
    */
    g.mouseMode = MOUSE_ROTATE;
    g.u = new UserSettings();
    g.u.camZ = 0.90625f;
    g.u.camRot.x = -0.43f;
    g.u.camRot.y = 0.67f;
    g.u.camRot.z = -0.6f;
    g.u.camRot.w = 1.0f;
    g.windowWidth = WINDOW_WIDTH;
    g.windowHeight = WINDOW_HEIGHT;
    g.u.translate[0] = -0.02f;
    g.u.translate[1] = 0.02f;
    g.u.translate[2] = 0.0f;
    g.u.wireframe = 0;
    g.u.drawLight = 0;
    g.u.stepSize = 1.0f / 128.0f; /*.02;*/
    /*1.0/512.0;*/
    g.u.sliceThickness = 1.0f;
    g.u.gradOffset = .9f;
    g.u.gradScale = .2f;
    g.u.texCoordScale = .45f;
    /* max. # of iterations supported for fragment programs on NV40 */
    g.u.numIterations = 255;
    g.u.isoValue = 0.5f;
    g.isoValueStep = ISO_VALUE_STEP;
    g.u.clipIsoValue = 0.22f;
    g.u.scatteringScale = 6.0f;
    g.u.lightPos[0] = 2.0f;
    g.u.lightPos[1] = 2.0f;
    g.u.lightPos[2] = 2.0f;
    g.u.lightPos[3] = 1.0f;
    g.u.backgroundImage = 0;
    g.u.backgroundGrayVal = 0;
    g.animatedAngle = 0.0f;
    g.animated = false;
    g.minFps = FLT_MAX;
    g.fpsSum = 0.0f;
    g.maxFps = 0.0f;
    g.basename = new String();
    g.basename = volumeFileName;
    g.currentFragProg = new ARBProgram();

    if (g.basename.endsWith(VOL_FILE_EXT)) {
      idx = g.basename.lastIndexOf('.');
      g.basename = g.basename.substring(0, idx);
      // *strrchr(g.basename, '.') = 0;
    }
    System.err.println(g.basename);

    g.preIntTable = new byte[TransferEdit.NUM_TE_ENTRIES *
        TransferEdit.NUM_TE_ENTRIES * 4];

    g.tfFunc = new byte[4 * TransferEdit.NUM_TE_ENTRIES];

    g.optDensity = new byte[TransferEdit.NUM_TE_ENTRIES];


  }

  private void checkExtension(GL gl, String glExtensionName) {
    if (!gl.isExtensionAvailable(glExtensionName)) {
      unavailableExtension("Unable to initialize " + glExtensionName +
                           " OpenGL extension");
    }
  }

  private void unavailableExtension(String message) {
    JOptionPane.showMessageDialog(null, message, "Unavailable extension",
                                  JOptionPane.ERROR_MESSAGE);
    shutdownDemo();
    throw new GLException(message);
  }

  public void shutdownDemo() {
    // ManipManager.getManipManager().unregisterWindow(drawableRef);
    // drawableRef.removeGLEventListener(this);
  }

  private void initOpenGL(GLAutoDrawable drawable) {
    float lightSpecular[] = {
        1.0f, 1.0f, 1.0f, 1.0f};
    float lightAmbient[] = {
        0.3f, 0.3f, 0.3f, 1.0f};
    float lightDiffuse[] = {
        0.7f, 0.7f, 0.7f, 1.0f};

    GL gl = drawable.getGL();

    // print out all the GL extension, very important.
    // System.err.println(gl.glGetString(GL.GL_EXTENSIONS));
    checkExtension(gl, "GL_EXT_texture3D"); // For multitexture
    checkExtension(gl, "GL_ARB_fragment_program");
    checkExtension(gl, "GL_ARB_multitexture");
    checkExtension(gl, "GL_ARB_vertex_program");
    checkExtension(gl, "GL_NV_occlusion_query");
    checkExtension(gl, "GL_NV_fragment_program2");
    checkExtension(gl, "GL_NV_vertex_program3");

    tf.initTE(drawable);

    g.dataTE = tf.getDataTE();

    loadVolumeTexture(volumeFileName, drawable);
    loadClipTexture(clipFileName,drawable);
    loadBackgroundTexture(drawable);
    loadSphereMapTexture(drawable);
    updateOptDensityTexture(drawable);
    initScatteringTexture(drawable);
    updatePreIntTable(drawable);

    loadVolumeShaders(drawable);

    // System.out.println("g.numTextures = " + g.numTextures);

    gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    gl.glDisable(GL.GL_DEPTH_TEST);

    gl.glLightfv(GL.GL_LIGHT0, GL.GL_AMBIENT, lightAmbient, 0);
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_DIFFUSE, lightDiffuse, 0);
    gl.glLightfv(GL.GL_LIGHT0, GL.GL_SPECULAR, lightSpecular, 0);

  }

  public void init(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    initGObject();
    initOpenGL(drawable);
    // SetDisplayList(drawable);
  }

  private void setOrthoProjection(GL gl, int x, int y, int w, int h) {
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    gl.glOrtho(0, w, 0, h, -1.0, 1.0);
    gl.glMatrixMode(GL.GL_TEXTURE);
    gl.glLoadIdentity();
    gl.glMatrixMode(GL.GL_MODELVIEW);
    gl.glLoadIdentity();
    gl.glViewport(x, y, w, h);
  }

  public void reshape(GLAutoDrawable drawable, int x, int y, int w, int h) {
    GL gl = drawable.getGL();
    g.windowWidth = w;
    g.windowHeight = h;
    // setOrthoProjection(drawable.getGL(), x, y, w, h);
    if (h == 0) {
      return;
    }
    g.initialized = 0;

  }

  public void displayChanged(GLAutoDrawable drawable,
                             boolean modeChanged,
                             boolean deviceChanged) {}

  /**
   * keyTyped.
   *
   * Invoked when a key has been typed. See the class description for
   * KeyEvent for a definition of a key typed event.
   */
  public void keyTyped(KeyEvent e) {}

  /**
   * keyPressed.
   *
   * Invoked when a key has been pressed. See the class description for
   * KeyEvent for a definition of a key pressed event.
   */
  public void keyPressed(KeyEvent e) {
    char key = e.getKeyChar();
    int keyCode = e.getKeyCode();
    int num;

    if (keyCode >= KeyEvent.VK_F1 && keyCode <= KeyEvent.VK_F12) {
      num = keyCode - KeyEvent.VK_F1;

      // System.out.println("g.numFragProgs[0] = " + g.numFragProgs[0]);

      if (num < g.numFragProgs[0]) {
        g.currentFragProg = g.fragProgs[num];
        System.out.println("activated fragment program " +
                           g.currentFragProg.filename);
      }
    }

    int already_handled;
    already_handled = tf.keyTE(key);
    if (already_handled == 1) {
        // System.out.println("update");
        isUpdateOptDensityTexture = true;
        return;
    }


    switch (key) {
      case 27:
      case 'q':
        System.exit(0);
        break;
      case '!':
        g.initialized = 0;
        break;
      case 'w':
        g.u.wireframe = 1 - g.u.wireframe;
        break;
      case 't':
        tf.toggleHidden();
        break;
      case 'p':
        isUpdatePreIntTable = true;
        break;
      case '+':
        g.u.sliceThickness += SLICE_STEP;
        if (g.u.sliceThickness > MAX_SLICE_THICKNESS) {
          g.u.sliceThickness = MAX_SLICE_THICKNESS;
        }
        break;
      case '-':
        g.u.sliceThickness -= SLICE_STEP;
        if (g.u.sliceThickness < MIN_SLICE_THICKNESS) {
          g.u.sliceThickness = MIN_SLICE_THICKNESS;
        }
        break;
      case '>':
        g.u.stepSize += STEPSIZE_STEP;
        if (g.u.stepSize > MAX_STEPSIZE) {
          g.u.stepSize = MAX_STEPSIZE;
        }
        break;
      case '<':
        g.u.stepSize -= STEPSIZE_STEP;
        if (g.u.stepSize < MIN_STEPSIZE) {
          g.u.stepSize = MIN_STEPSIZE;
        }
        break;
      case ']':
        g.u.stepSize *= 2.0;
        if (g.u.stepSize > MAX_STEPSIZE) {
          g.u.stepSize = MAX_STEPSIZE;
        }
        break;
      case '[':
        g.u.stepSize /= 2.0;
        if (g.u.stepSize < MIN_STEPSIZE) {
          g.u.stepSize = MIN_STEPSIZE;
        }
        break;
      case 's':
        g.u.gradScale += GRAD_SCALE_STEP;
        System.err.println("gradient: scale " + g.u.gradScale + " offset " +
                           g.u.gradOffset);
        break;
      case 'x':
        g.u.gradScale -= GRAD_SCALE_STEP;
        break;
      case 'd':
        g.u.gradOffset += GRAD_OFFSET_STEP;
        break;
      case 'c':
        g.u.gradOffset -= GRAD_OFFSET_STEP;
        break;
      case 'f':
        g.u.texCoordScale += TEX_COORD_SCALE_STEP;
        break;
      case 'v':
        g.u.texCoordScale -= TEX_COORD_SCALE_STEP;
        break;
      case 'l':
        g.u.drawLight = 1 - g.u.drawLight;
        break;
      case 'j':

        /*
                     if (glut.glutGetModifiers() & GL.GLUT_ACTIVE_ALT) {
          g.isoValueStep *= 2.0f;
          if (g.isoValueStep > 1.0) {
            g.isoValueStep = 1.0f;
          }
                     }
                     else { */
        g.u.isoValue += g.isoValueStep;
        if (g.u.isoValue > 1.0) {
          g.u.isoValue = 1.0f;
        }

        // }
        break;
      case 'm':

        /*
                     if (glutGetModifiers() & GLUT_ACTIVE_ALT) {
          g.isoValueStep /= 2.0;
                     }
                     else { */
        g.u.isoValue -= g.isoValueStep;
        if (g.u.isoValue < 0.0) {
          g.u.isoValue = 0.0f;
        }

        // }
        break;
      case 'J':
        g.u.clipIsoValue += g.isoValueStep;
        if (g.u.clipIsoValue > 1.0) {
          g.u.clipIsoValue = 1.0f;
        }
        break;
      case 'M':
        g.u.clipIsoValue -= g.isoValueStep;
        if (g.u.clipIsoValue < 0.0) {
          g.u.clipIsoValue = 0.0f;
        }
        break;
      case 'r':
        freeVolumeShaders();
        isLoadVolumeShaders = true;
        break;
      case '.':
        g.u.scatteringScale += SCATTERING_STEP;
        break;
      case ',':
        g.u.scatteringScale -= SCATTERING_STEP;
        if (g.u.scatteringScale < 0.0) {
          g.u.scatteringScale = 0.0f;
        }
        break;
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6': {
        int coord = (key - '1') / 2;
        if ( (key - '1') % 2 == 0) {
          g.u.lightPos[coord] += LIGHT_POS_STEP;
        }
        else {
          g.u.lightPos[coord] -= LIGHT_POS_STEP;
        }
        break;
      }
      case 'S':
        isSaveSettings = true;
        break;
      case 'L':
        isLoadSettings = true;
        break;
      case 'k':
        g.u.backgroundImage = 1 - g.u.backgroundImage;
        isLoadBackgroundTexture = true;
        break;
      case '}':
        if (g.u.backgroundGrayVal < 255) {
          g.u.backgroundGrayVal++;
          isLoadBackgroundTexture = true;
        }
        break;
      case '{':
        if (g.u.backgroundGrayVal > 0) {
          g.u.backgroundGrayVal--;
          isLoadBackgroundTexture = true;
        }
        break;
      case 'i':
        printSettings();
        break;
      case ' ':
        g.animated = !g.animated;
        g.animatedFrames = 0;
        g.minFps = FLT_MAX;
        g.fpsSum = 0.0f;
        g.maxFps = 0.0f;
        g.animatedAngle = 0.0f;
        break;
      default:
        break;
    }
  }

  public void printSettings() {
    Vector3 axis = new Vector3();
    float[] angle = new float[1];
    Quaternion q = new Quaternion();
    System.err.println("step size ........................: " + g.u.stepSize);
    System.err.println("slice thickness ..................: " +
                       g.u.sliceThickness);
    System.err.println("gradient scale ...................: " + g.u.gradScale);
    System.err.println("gradient offset ..................: " + g.u.gradOffset);
    System.err.println("texture coordinate scale .........: " +
                       g.u.texCoordScale);
    System.err.println("isovalue of volume ...............: " + g.u.isoValue);
    System.err.println("isovalue of clipping volume ......: " +
                       g.u.clipIsoValue);
    System.err.println("isovalue step ....................: " + g.isoValueStep);
    System.err.println("number of iterations per loop ....: " +
                       g.u.numIterations);
    System.err.println("wireframe mode ...................: " +
                       (g.u.wireframe == 1 ? "yes" : "no"));
    System.err.println("light show mode ..................: " +
                       (g.u.drawLight == 1 ? "yes" : "no"));
    System.err.println("scattering scale .................: " +
                       g.u.scatteringScale);
    System.err.println("light source position ............: " + " " +
                       g.u.lightPos[0] + " " + g.u.lightPos[1] + " " +
                       g.u.lightPos[2]);
    System.err.println("object translation ...............: " + " " +
                       g.u.translate[0] + " " + g.u.translate[1] + " " +
                       g.u.translate[2]);
    System.err.println("camera z-value ...................: " + g.u.camZ);
    q.Quaternion_getAngleAxis(g.u.camRot, angle, axis);
    System.err.println("camera rotation ..................: " + axis.x + " " +
                       axis.y + " " + axis.z + " degree: " + angle[0]);
    System.err.println("background image mode ............: " +
                       (g.u.backgroundImage == 1 ? "yes" : "no"));
    System.err.println("background gray value ............: " +
                       g.u.backgroundGrayVal);
    System.err.println("");
  }

  /**
   * keyReleased.
   *
   * Invoked when a key has been released. See the class description for
   * KeyEvent for a definition of a key released event.
   */
  public void keyReleased(KeyEvent e) {
    isUpdateOptDensityTexture = false;
    isUpdatePreIntTable = false;
    isLoadVolumeShaders = false;
    isLoadBackgroundTexture = false;
    isLoadSettings = false;
    isSaveSettings = false;
  }

  // Methods required for the implementation of MouseListener
  public void mouseEntered(MouseEvent e) {}

  public void mouseExited(MouseEvent e) {}

  public void mousePressed(MouseEvent e) {
    int already_handled;

    already_handled = tf.mouseTE(e.getX(), e.getY());

    if ( already_handled == 0 ) {
      mouseMouseInteract(e);
    }

    // drawableRef.swapBuffers();
  }

  private void mouseMouseInteract(MouseEvent e) {
    int modifiers = e.getModifiers();
    int button = e.getButton();

    g.mousePosLast[0] = e.getX();
    g.mousePosLast[1] = e.getY();

    switch (button) {
      case MouseEvent.BUTTON1:
        if ( (modifiers & e.CTRL_MASK) != 0) {
          g.mouseMode = MOUSE_MOVE_LIGHT_XY;
        }
        else {
          g.mouseMode = MOUSE_ROTATE;
        }
        break;
      case MouseEvent.BUTTON2:
        if ( (modifiers & e.CTRL_MASK) != 0) {
        }
        else {
          g.mouseMode = MOUSE_TRANSLATE;
        }
        break;
      case MouseEvent.BUTTON3:
        if ( (modifiers & e.CTRL_MASK) != 0) {
          g.mouseMode = MOUSE_MOVE_LIGHT_Z;
        }
        else {
          g.mouseMode = MOUSE_DOLLY;
        }
        break;
      default:
        break;
    }

  }

  public void mouseReleased(MouseEvent e) {
    isUpdateOptDensityTexture = false;
  }

  public void mouseClicked(MouseEvent e) {}


  private void motionMouseInteract(MouseEvent e) {
    Vector3 mouseVector = new Vector3();
    Vector3 view = new Vector3();
    Vector3 rotAxis = new Vector3();
    Quaternion rot = new Quaternion();

    float dx = MOUSE_SCALE * (e.getX() - g.mousePosLast[0]) /
           (float) g.windowWidth;
       float dy = MOUSE_SCALE * (g.mousePosLast[1] - e.getY()) /
           (float) g.windowHeight;

       mouseVector.x = dx;
       mouseVector.y = dy;
       mouseVector.z = 0.0f;

       view.x = 0.0f;
       view.y = 0.0f;
       view.z = -1.0f;

       rotAxis = view.Vector3_cross(mouseVector, view);
       rotAxis.Vector3_normalize(rotAxis);

       switch (g.mouseMode) {
         case 2:
           g.u.camZ += dy;
           break;
         case 0:
           rot = rot.Quaternion_fromAngleAxis(dx * dx + dy * dy, rotAxis);
           g.u.camRot = rot.Quaternion_mult(rot, g.u.camRot);
           rot.Quaternion_normalize(g.u.camRot);
           break;
         case 1:
           g.u.translate[0] += mouseVector.x;
           g.u.translate[1] += mouseVector.y;
           g.u.translate[2] += mouseVector.z;
           break;
         case 3:
           g.u.lightPos[0] += dx;
           g.u.lightPos[1] += dy;
           System.err.println("lightPos= " + g.u.lightPos[0] + " " +
                              g.u.lightPos[1] + " " + g.u.lightPos[2]);
           break;
         case 4:
           g.u.lightPos[2] -= dy;
           System.err.println("lightPos= " + g.u.lightPos[0] + " " +
                              g.u.lightPos[1] + " " + g.u.lightPos[2]);
           break;
         default:
           break;
       }

    g.mousePosLast[0] = e.getX();
    g.mousePosLast[1] = e.getY();
  }

  // Methods required for the implementation of MouseMotionListener
  public void mouseDragged(MouseEvent e) {

    int already_handled;
    float x, y;

    already_handled = tf.motionTE(e.getX(), e.getY());

    if (already_handled == 1) {
      isUpdateOptDensityTexture = true;
    } else {
      motionMouseInteract(e);
    }

    // drawableRef.swapBuffers();
  }

  public void mouseMoved(MouseEvent e) {}

  private String getSettingsFilename() {
    int index = 0;
    String fgname = g.currentFragProg.filename;
    index = fgname.indexOf(FRAG_PROG_EXT);
    String fileName = new String();
    fileName += (g.basename + "_" +
                    g.currentFragProg.filename.substring(0, index) +
                    SETTINGS_EXT);
    return fileName;
  }

  public void saveSettings() {

        String filename;
        int i;

        filename = getSettingsFilename() + ".txt";
        try {
          UserSettings u = (UserSettings)g.u.clone();
          FileOutputStream fout = new FileOutputStream(filename);
          ObjectOutputStream outStream = new ObjectOutputStream(fout);
          outStream.writeObject(u);

          for (i = 0; i < TransferEdit.NUM_TE; i++) {
            outStream.write(g.dataTE[i]);
          }

          outStream.close();
        } catch (Exception e) {
            System.err.println("Error save setting " + g.u.getClass().getName() +" :\n" + e);
            return;
        }


     /*/
        if (! (fp = fopen(filename, "wb"))) {
                perror("cannot open settings file for writing");
                return;
        }

        if (fwrite(&g.u, sizeof(UserSettings), 1, fp) != 1) {
                fprintf(stderr, "saving user settings failed\n");
                exit(1);
        }

        for (i = 0; i < NUM_TE; i++) {
                if (fwrite(g.dataTE[i], NUM_TE_ENTRIES * sizeof(DataTypeTE),
                                   1, fp) != 1) {
                        fprintf(stderr, "saving transfer functions failed\n");
                        exit(1);
                }
        }

        fclose(fp);

        free(filename);
     */
  }

  private void loadSettings(GLAutoDrawable drawable) {
          String filename;

          int i;
          byte[] data;
          filename = getSettingsFilename() + ".txt";

          System.out.println("loading settings from file " + filename);

          FileInputStream in;

          if (filename == null) {
            return;
          }

          try {
            FileInputStream fout = new FileInputStream(filename);
            ObjectInputStream inStream = new ObjectInputStream(fout);
            g.u = (UserSettings) inStream.readObject();
            for (i = 0; i < TransferEdit.NUM_TE; i++) {
              inStream.read(g.dataTE[i]);
            }
            inStream.close();
          }
          catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
          }

          /*
          for (i = 0; i < NUM_TE; i++) {
            if (fread(g.dataTE[i], NUM_TE_ENTRIES * sizeof(DataTypeTE), 1, fp) != 1) {
              fprintf(stderr, "loading transfer functions failed\n");
              exit(1);
            }
          }
          */
          updateOptDensityTexture(drawable);
          loadBackgroundTexture(drawable);
          updatePreIntTable(drawable);
  }

  public static void main(String[] args) {

    final JFrame frame = new JFrame("Render demo");
    GLCanvas canvas = new GLCanvas();
    Render render = new Render();
    render.setup(args);
    canvas.addGLEventListener(render);
    canvas.addKeyListener(render);
    canvas.addMouseListener(render);
    canvas.addMouseMotionListener(render);
    canvas.setSize(new Dimension(512, 512));
    frame.add(canvas);
    frame.setSize(550, 550);
    // Animator serves the purpose of the idle function, calls display:
    final Animator animator = new Animator(canvas);
    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        // Run this on another thread than the AWT event queue to
        // avoid deadlocks on shutdown on some platforms
        new Thread(new Runnable() {
          public void run() {
            animator.stop();
            frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
          }
        }).start();
      }
    });
    frame.show();
    animator.start();
  }

  public void setup(String[] args) {
    if ( (args == null) || (args.length == 0)) {
      args = defaultArgs;
    }
    if (args.length < 2) {
      return;
    }

    volumeFileName = args[0];
    clipFileName = null; // args[1];

  }

  private static String[] defaultArgs = {
      "./data/head256.dat",
      "./data/cylinder256x256x225.dat"
  };

}
