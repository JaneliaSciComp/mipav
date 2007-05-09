package gov.nih.mipav.view.multihisto.src;

public class gluvvVolRen {
  //volume rendering paramters
  public float sampleRate; //current sample rate for rendering
  public float interactSamp; //interactive sample rate
  public float goodSamp; //high quality sample rate
  public int shade; //render the volume shaded
  public TLUT  tlut; //texture lookup table
  public byte[] deptex; //unsigned char * deptex, 2d dependent texture 256x256xRGBA [gb]
  public int[] deptexName = new int[1]; //unsigned int deptexName,  '' name [gb]
  public byte[] deptex2; //unsigned char * deptex2, 2d dependent texture ... [ar]
  public int[] deptex2Name = new int[1]; //unsigned int deptex2Name,  '' name [ar]
  public byte[] deptex3; //unsigned char * deptex3, 2d dependent texture for shadows
  public int[] deptex3Name = new int[1]; //unsigned int deptex3Name,  '' name shadows
  public int loadTLUT; //reload the LUT if flag == 1
  public int scaleAlphas; //scale alphas with sample rate
  public float gamma; //constant alpha scale
  public int timestep; //what timestep are we on?
  public int usePostCNorm; //using post classification normals???
  public int loadNorms; //reload the normals

  public gluvvVolRen() {
    tlut = new TLUT();
  }

}
