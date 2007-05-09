package gov.nih.mipav.view.multihisto.src;

public class gluvvPick {
  //picking information
  public int name32; //name for 32 bit compiles
  public int name64; //name for 64 bit compiles
  public int data1; //data pushed on with name
  public int data2; // ''
  public int data3; // ''
  public int[] pos = new int[2]; //pos[2], screen space coords
  public int z; //unsigned, depth of pick
  public gluvvPrimitive prim; //primitive pointer (same as name*)

  public gluvvPick() {
    prim = new gluvvPrimitive();
  }

}
