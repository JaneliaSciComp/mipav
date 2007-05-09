package gov.nih.mipav.view.gpu.src;


public class preint {

 static int MIN_INT_STEPS = 20;
 static int ADD_LOOKUP_STEPS = 1;

 byte[] data;

 public double VAL(double s, int i)
 {
    // (g.data[(int)(s) * 4 + i])
    return data[(int)(s) * 4 + i];
 }

 public double LERP(double x,double y, double z)
 {
   return ((x) * (z) + (y) * (1.0 - (z)));
 }
 /**
  *
  * @param imgsize int
  * @param thickness float
  * @param data byte[] unsigned char pointer
  * @param table byte[] unsigned char pointer
  */
 public void calcPreIntTable(int imgsize, float thickness, byte[] _data, byte[] table)
{
        int sb, sf, i, j, n, base1, base2, offset;
        double stepWidth, s, temp;
        double[] rgba = new double[4];
        double[]  rgbac = new double[4];

        data = _data;
        byte val;
        for (sb = 0; sb < imgsize; sb++) {
                for (sf = 0; sf <= sb; sf++) {
                        n = MIN_INT_STEPS + ADD_LOOKUP_STEPS * Math.abs(sb - sf);

                        stepWidth = thickness/n;
                        // memset(rgba, 0, sizeof(rgba));

                        for (i = 0; i < n; i++) {
                                s = sf + (sb - sf) * (double)i/n;
                                // offset = sf != sb;
                                if ( sf != sb ) {
                                  offset = 0;
                                } else {
                                  offset = 1;
                                }

                                System.err.println("sb = " + sb + "sf = " + sf + " n = " + n + " s = " + s);

                                rgbac[3] = stepWidth/255.0 *
                                                   LERP(VAL(s, 3), VAL(s + offset, 3), s - Math.floor(s));

                                System.err.println("lerp = " +  LERP(VAL(s, 3), VAL(s + 1, 3), s - Math.floor(s))/255.0);

                                /* Standard optical model: RGB densities are multiplied with
                                 * opacity density */
                                temp = Math.exp(-rgba[3]) * rgbac[3]/255.0;

                                // System.err.println("exp = " +  Math.exp(-rgba[3]), rgba[3]);

                                for (j = 0; j < 3; j++) {
                                        rgbac[j] = temp *  LERP(VAL(s, j), VAL(s + offset, j), s - Math.floor(s));
                                        rgba[j] += rgbac[j];
                                }
                                rgba[3] += rgbac[3];
                        }

                        base1 = (sb * imgsize + sf) * 4;
                        base2 = (sf * imgsize + sb) * 4;
                        for (i = 0; i < 3; i ++) {
                                if (rgba[i] > 1.0) {
                                        rgba[i] = 1.0;
                                }
                                // table[base1++] = table[base2++] = (unsigned char)(rgba[i] * 255.99);
                                val = (byte)((double)(rgba[i] * 255.99));
                                table[base1++] = table[base2++] = (byte)val;
                        }
                        // table[base1] = table[base2] = (unsigned char)((1.0 - exp(-rgba[3])) * 255.99);
                        val = (byte)(((1.0 - Math.exp(-rgba[3])) * 255.99));
                        table[base1] = table[base2] = (byte)val;
                }
        }
}


}
