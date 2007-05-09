package gov.nih.mipav.view.multihisto.src;

public class gluvvGlobal {
  //------ GLUVV GLOBAL -----------------------------------
  public int debug;
  public gluvvWindow win; //widnow
  public gluvvEnv env; //environment
  public gluvvLight light; //light
  public gluvvRInfo rinfo; //render info
  public gluvvMouse mouse; //mouse
  public int plat; //plaform
  public int picking; //picking toggle
  public gluvvPick pick; //picked object
  public gluvvVolRen volren; //volume rendering stuff
  public gluvvTF tf; //transfer function
  public gluvvClip clip; //clipping plane tf
  public gluvvProbe probe; //data probe widget
  public int mprobe; //using data probe or clip plane for probing??
  public MetaVolume mv; //the volumes
  public MetaVolume mv1;
  public MetaVolume mv2;
  public MetaVolume mv3;
  public int mainWindow; //mainWindow Identifier
  public int shade; //shading enable/disable flag
  public int dmode; //what data mode are we in?
  public int reblend; //re-render scene with blend
  public gluvvPert pert; //perturbation parameters

  public gluvvGlobal () {
    win = new gluvvWindow();
    env = new gluvvEnv();
    light = new gluvvLight();
    rinfo = new gluvvRInfo();
    mouse = new gluvvMouse();
    pick = new gluvvPick();
    volren = new gluvvVolRen();
    tf = new gluvvTF();
    clip = new gluvvClip();
    probe = new gluvvProbe();
    mv = new MetaVolume();
    mv1 = new MetaVolume();
    mv2 = new MetaVolume();
    mv3 = new MetaVolume();
    shade = 0;
    dmode = 0;
    // reblend = new gluvvBlend();
    pert = new gluvvPert();
  }

}
