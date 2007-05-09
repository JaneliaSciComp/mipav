package gov.nih.mipav.view.multihisto.src;

public class gluvvShade {
  //Gluvv Shade modes (shadows are handled by the light struct)
  public static int gluvvShadeUnknown = 0;
  public static int gluvvShadeAmb = 1; //ambient
  public static int gluvvShadeDiff = 2; //diffuse
  public static int gluvvShadeDSpec = 3; //diffuse + specular
  public static int gluvvShadeFaux = 4; //Faux shading
  public static int gluvvShadeArb = 5; //arbitrary shading via pixel texture
  public static int gluvvShadeMIP = 6;

  public gluvvShade() {

  }
}
