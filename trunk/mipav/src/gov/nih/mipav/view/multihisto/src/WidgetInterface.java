package gov.nih.mipav.view.multihisto.src;


public interface WidgetInterface {
  public void draw();

  public int pickcb(int data1, int data2, float x, float y, float z);

  public int movecb(float x, float y, float z);

  public int keycb(char key);

  public int Xkeycb(int ks);

  public int mousecb(int button,  int state, float x, float y, float z);

  public void resize(int x, int y);

  public int releasecb();
}
