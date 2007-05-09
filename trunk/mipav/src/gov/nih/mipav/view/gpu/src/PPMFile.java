package gov.nih.mipav.view.gpu.src;


public class PPMFile
{
  // unsigned char *data;
  byte[] data;
  int width, height;
  int maxVal;

  public PPMFile(int x, int y, int maxColorVal) {
    width = x;
    height = y;
    maxVal = maxColorVal;
    data = new byte[width * height];
    for (int i = 0; i < data.length; i++)
      data[i] = 0;
  }

}
