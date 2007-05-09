
package gov.nih.mipav.view.multihisto.src;

public class gluvvDataMode {
  //Gluvv Data modes                  #of bytes in data texture
  public static int GDM_V1 = 1; //1 value only, scalar                (1B)
  public static int GDM_V1G = 2; //1 value and gradient                (2B)
  public static int GDM_V1GH = 3; //1 value, gradient, and hessian      (3B)
  public static int GDM_V2 = 4; //2 values                            (2B)
  public static int GDM_V2G = 5; //2 values with gradient              (3B)
  public static int GDM_V2GH = 6; //2 values with gradient and hessian  (4B)
  public static int GDM_V3 = 7; //3 values                            (3B)
  public static int GDM_V3G = 8; //3 values with gradient              (4B)
  public static int GDM_V4 = 9; //4 values                            (4B)
  public static int GDM_VGH = 10; //VGH data (pre-computed)             (3B)
  public static int GDM_VGH_VG = 11; //VGH data - only use VG              (2B)
  public static int GDM_VGH_V = 12; //VGH data - only use V               (1B)
  public static int GDM_UNKNOWN = 13; //I have no idea what the data type is(?B)

  public gluvvDataMode() {

  }

}
