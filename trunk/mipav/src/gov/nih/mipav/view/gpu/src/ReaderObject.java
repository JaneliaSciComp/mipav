package gov.nih.mipav.view.gpu.src;


public class ReaderObject
{
   public String fileName;
   public int[] sizes;
   public float[] dists;
   public int type;
   public int numComponents;
   public byte[] data;

   public ReaderObject(String _fileName, int[] _sizes, float[] _dists, int _type, int _numComponents, byte[] _data)
   {
         fileName = _fileName;
         sizes = new int[_sizes.length];
         sizes = _sizes;
         dists = new float[_dists.length];
         dists = _dists;
         type = _type;
         numComponents = _numComponents;
         data = new byte[_data.length];
         data = _data;
   }
}
