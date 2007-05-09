package gov.nih.mipav.view.gpu.src;

public class ARBProgram
{
  TextureObject[] textureObjects;
  int numTextureObjects;
  int programType;
  String filename;
  String prog;
  int[] id;
  int size;

  public ARBProgram() {
     textureObjects = new TextureObject[10];
     filename = new String();
     prog = new String();
     id = new int[1];
  }

}
