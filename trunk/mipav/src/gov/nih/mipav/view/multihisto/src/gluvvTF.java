package gov.nih.mipav.view.multihisto.src;

public class gluvvTF {
  public int tfWindow; //idenftifier for transfer function window
  public TFWindow tfwin; //the tf window class
  public int loadme; //reload the tf
  public int paintme; //paint the brush into the tf
  public int dropme; //drop the brush widget into the tf
  public int clearpaint; //clear the painted tf
  public int[] ptexsz = new int[3];   //int ptexsz[3]
  public int numelts; //RGBA ??
  public int brushon; //brush on or off?
  public float slider1; //slider for third axis emphasis
  public float slider1hi;
  public float slider1lo;
  public float slider2; //slider for fourth axis emphasis
  public int histOn; //use the histogram??

  public gluvvTF() {
    tfwin = new TFWindow();
  }

}
