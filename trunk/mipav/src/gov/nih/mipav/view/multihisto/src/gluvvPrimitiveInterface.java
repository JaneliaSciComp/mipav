package gov.nih.mipav.view.multihisto.src;


public interface gluvvPrimitiveInterface {
  public  void          init();
  public  void          draw();
  public  int           key(char k, int x, int y);
  public  int           special(int k, int x, int y);
  public  int           pick(int data1, int data2, int data3, float x, float y, float z);
  public  int           pick();
  public  int           mouse(int button, int state, int x, int y);
  public  int           move(int x, int y);
  public  int           release();   //tells widget it is done

}
