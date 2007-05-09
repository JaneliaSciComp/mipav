package gov.nih.mipav.view.multihisto.src;

public class gluvvLight {

  public float[] pos = new float[3]; //pos[3], light position
  public float[] startpos = new float[3]; // startpos[3]
  public float[] color = new float[3]; // color[3], light color
  public float amb; //(ambient contribution)
  public float intens;
  public float[] mv = new float[16]; //mv[16], the light model view
  public float[] pj = new float[16]; //pj[16], the light projection
  public float[] xf = new float[16]; //xf[16], pj*mv
  public int[] buffsz = new int[2]; //buffsz[2], x&y sizes of shadow buffer
  public int shadow; //are shadows enabled??
  public int softShadow; //are soft shadows enabled??
  public int showView; //show the view from the light
  public int shadowTF; //use a separate shadow transfer function
  public int showShadowTF; //render the shadow tf from your view
  public int sill; //do silhouette edges
  public float gShadowQual; //good shadow quality
  public float iShadowQual; //interactive shadow quality
  public int[] cubeName = new int[1]; //unsigned int, cube map name
  public float[] cubeKey = new float[6]; //cubeKey[6], decode the cubemap normals
  public int csz; //dimension of one axis of the cube map
  public int load; //reload the lighting info
  public int gload; //this is the good load, not interacting anymore
  public int fog; //is fog on?
  public float[] fogColor = new float[3]; //fogColor[3], fog color
  public float fogThresh; //fog blend threshold
  public float[] fogLimits = new float[2]; //fogLimits[2], limits of fog calculations
  public int latt; //light attinuation is on?
  public float lattThresh; //attinuation threshold
  public float[] lattLimits = new float[2]; //lattLimits[2], limits of attinuation calculations

  public gluvvLight() {

  }

}
