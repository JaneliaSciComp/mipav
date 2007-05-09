package gov.nih.mipav.view.gpu.src;

public class GObject
{
  int[] numSlices = new int[3];
  float[] sliceDists = new float[3];
  int[]  volTexSizes = new int[3];
  byte[] volData;   // void *

  float[] scaleFactors = new float[4];
  float[] scaleFactorsInv = new float[4];
  float[] extents = new float[3];
  float[] center = new float[4];

  String basename;    // char *

  byte[][] dataTE; // DataTypeTE **
  int teTex;   // GLuint

  byte[] preIntTable;  // unsigned char *
  byte[] tfFunc; // unsigned char *

  byte[] optDensity; // unsigned char *

  int framebuffer; // GLuint
  int depthbuffer;  // GLuint
  Texture[] colorImageTex;  // Texture *
  Texture[] hitPointsImageTex;   // Texture *
  Texture[] startPosImageTex;   // Texture *
  Texture[] debugImageTex;      // Texture *

  ARBProgram copyBufferProg;
  ARBProgram renderHitPointsVProg;
  ARBProgram fillStartPosHolesProg;
  ARBProgram initHitPointsProg;
  ARBProgram initHitPointsBackProg;

  float isoValueStep;
  boolean animated;
  int animatedFrames;
  float minFps, fpsSum, maxFps;
  float animatedAngle;

  int[] numFragProgs = new int[1];
  ARBProgram[] fragProgs;  // ARBProgram *
  ARBProgram currentFragProg;  // ARBProgram *

  int[] mousePosLast = new int[2];
  int mouseMode;
  int windowWidth, windowHeight;

  Texture[] textures;  // Texture *
  int numTextures;

  int initialized;

  UserSettings u;

}
