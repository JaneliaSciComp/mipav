package gov.nih.mipav.model.algorithms;


/**
 * Sinc function windowed with Hanning window function used for interpolation This function will create images with
 * greater maximums and lower minimums than the original image, so if the input image type is BYTE or UBYTE, the output
 * image type should be a SHORT. Notice that clamping to 0 and 255 is used with ARGB images.
 *
 * <p>This is an example of a cardinal basis function. The cardinal basis function centered at a given original grid
 * point is the definition of how an image that is 1 at that grid point and 0 at all others is to be interpolated into
 * intermediate points. The final image is then constructed by adding together all the basis functions from each
 * original grid point, each one scaled by the intensity at its original grid point. Since a cardinal basis function is
 * zero at all other original grid points except the one at which it is based, interpolation that falls exactly onto an
 * original grid point will always preserve the original value.</p>
 */

public class AlgorithmWSinc extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float alphaMax = 255.0f;

    /** DOCUMENT ME! */
    private float alphaMin = 0.0f;

    /** DOCUMENT ME! */
    private float blueMax = 255.0f;

    /** DOCUMENT ME! */
    private float blueMin = 0.0f;

    /** DOCUMENT ME! */
    private boolean clip;

    /** DOCUMENT ME! */
    private float greenMax = 255.0f;

    /** DOCUMENT ME! */
    private float greenMin = 0.0f;

    /** DOCUMENT ME! */
    private float inputMax;

    /** DOCUMENT ME! */
    private float inputMin;

    /** DOCUMENT ME! */
    private float redMax = 255.0f;

    /** DOCUMENT ME! */
    private float redMin = 0.0f;

    /** DOCUMENT ME! */
    private int rX, rY, rZ; // Number used in interpolation along each axis is 2 + 2*r;

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float scaleZ;

    /** DOCUMENT ME! */
    private int sliceSize;

    /** Global variables. */
    private float[] volume = null;

    /** DOCUMENT ME! */
    private float[] wSincXT = null;

    /** DOCUMENT ME! */
    private float[] wSincYT = null;

    /** DOCUMENT ME! */
    private float[] wSincZT = null;

    /** DOCUMENT ME! */
    private int xD, yD, zD;

    /** DOCUMENT ME! */
    private int xdim, ydim, zdim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmWSinc - default constructor.
     */
    public AlgorithmWSinc() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        volume = null;
        wSincXT = null;
        wSincYT = null;
        wSincZT = null;
        System.gc();
        super.finalize();
    }

    /**
     * Default method that is not really appropiate for this class but must be defined because this class extends
     * AlgorithmBase.
     */
    public void runAlgorithm() { }

    /**
     * Setup 2D wSinc.
     *
     * @param  vol      volume comprising control points for the wSinc
     * @param  extents  vol extents (xdim, ydim)
     * @param  clip     if true clip output to range of input image
     */
    public void setup2DWSinc(float[] vol, int[] extents, boolean clip) {
        int i;
        float invX, invY;
        float diffX, diffY;
        double argX, argY;

        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        this.clip = clip;
        rX = 3; // to third zero crossing
        rY = 3;
        wSincXT = new float[1000];
        wSincYT = new float[1000];
        scaleX = 999.0f / (rX + 1.0f);
        scaleY = 999.0f / (rY + 1.0f);
        invX = 1.0f / scaleX;
        invY = 1.0f / scaleY;
        wSincXT[0] = 1.0f;
        wSincYT[0] = 1.0f;

        for (i = 1; i < 1000; i++) {
            diffX = i * invX;
            argX = Math.PI * diffX;
            wSincXT[i] = (float) ((Math.sin(argX) / argX) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffY = i * invY;
            argY = Math.PI * diffY;
            wSincYT[i] = (float) ((Math.sin(argY) / argY) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
        }

        if (clip) {
            inputMin = volume[0];
            inputMax = volume[0];

            for (i = 1; i < volume.length; i++) {

                if (volume[i] < inputMin) {
                    inputMin = volume[i];
                }

                if (volume[i] > inputMax) {
                    inputMax = volume[i];
                }
            }
        } // if (clip)
    }

    /**
     * Setup 2D wSinc.
     *
     * @param  vol      volume comprising control points for the wSinc
     * @param  extents  vol extents (xdim, ydim)
     * @param  range    determines number of pixels used along each axis
     * @param  clip     if true clip output to range of input image
     */
    public void setup2DWSinc(float[] vol, int[] extents, int[] range, boolean clip) {
        int i;
        float invX, invY;
        float diffX, diffY;
        double argX, argY;

        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        rX = range[0];
        rY = range[1];
        wSincXT = new float[1000];
        wSincYT = new float[1000];
        scaleX = 999.0f / (rX + 1.0f);
        scaleY = 999.0f / (rY + 1.0f);
        invX = 1.0f / scaleX;
        invY = 1.0f / scaleY;
        wSincXT[0] = 1.0f;
        wSincYT[0] = 1.0f;

        for (i = 1; i < 1000; i++) {
            diffX = i * invX;
            argX = Math.PI * diffX;
            wSincXT[i] = (float) ((Math.sin(argX) / argX) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffY = i * invY;
            argY = Math.PI * diffY;
            wSincYT[i] = (float) ((Math.sin(argY) / argY) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
        }

        inputMin = volume[0];
        inputMax = volume[0];

        for (i = 1; i < volume.length; i++) {

            if (volume[i] < inputMin) {
                inputMin = volume[i];
            }

            if (volume[i] > inputMax) {
                inputMax = volume[i];
            }
        }
    }

    /**
     * Setup 2D wSinc for coor image.
     *
     * @param  vol      volume comprising control points for the wSinc
     * @param  extents  vol extents (xdim, ydim)
     * @param  argbMax  maximum possible color value
     * @param  clip     if true clip output to range of input image
     */
    public void setup2DWSincC(float[] vol, int[] extents, float argbMax, boolean clip) {
        int i;
        float invX, invY;
        float diffX, diffY;
        double argX, argY;

        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        alphaMax = argbMax;
        redMax = argbMax;
        greenMax = argbMax;
        blueMax = argbMax;
        this.clip = clip;
        rX = 3; // to third zero crossing
        rY = 3;
        wSincXT = new float[1000];
        wSincYT = new float[1000];
        scaleX = 999.0f / (rX + 1.0f);
        scaleY = 999.0f / (rY + 1.0f);
        invX = 1.0f / scaleX;
        invY = 1.0f / scaleY;
        wSincXT[0] = 1.0f;
        wSincYT[0] = 1.0f;

        for (i = 1; i < 1000; i++) {
            diffX = i * invX;
            argX = Math.PI * diffX;
            wSincXT[i] = (float) ((Math.sin(argX) / argX) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffY = i * invY;
            argY = Math.PI * diffY;
            wSincYT[i] = (float) ((Math.sin(argY) / argY) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
        }

        if (clip) {
            alphaMin = volume[0];
            alphaMax = volume[0];
            redMin = volume[1];
            redMax = volume[1];
            greenMin = volume[2];
            greenMax = volume[2];
            blueMin = volume[3];
            blueMax = volume[3];

            for (i = 1; i < (volume.length / 4); i++) {

                if (volume[4 * i] < alphaMin) {
                    alphaMin = volume[4 * i];
                }

                if (volume[4 * i] > alphaMax) {
                    alphaMax = volume[4 * i];
                }

                if (volume[(4 * i) + 1] < redMin) {
                    redMin = volume[(4 * i) + 1];
                }

                if (volume[(4 * i) + 1] > redMax) {
                    redMax = volume[(4 * i) + 1];
                }

                if (volume[(4 * i) + 2] < greenMin) {
                    greenMin = volume[(4 * i) + 2];
                }

                if (volume[(4 * i) + 2] > greenMax) {
                    greenMax = volume[(4 * i) + 2];
                }

                if (volume[(4 * i) + 3] < blueMin) {
                    blueMin = volume[(4 * i) + 3];
                }

                if (volume[(4 * i) + 3] > blueMax) {
                    blueMax = volume[(4 * i) + 3];
                }
            } // for (i = 1; i < (volume.length/4); i++)
        } // if (clip)
    }

    /**
     * Setup 3D wSinc.
     *
     * @param  vol      volume comprising control points for the wSinc
     * @param  extents  vol extents (xdim, ydim, zdim)
     * @param  clip     if true clip output to range of input image
     */
    public void setup3DWSinc(float[] vol, int[] extents, boolean clip) {
        int i;
        float invX, invY, invZ;
        float diffX, diffY, diffZ;
        double argX, argY, argZ;

        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        zdim = extents[2];
        this.clip = clip;
        xD = xdim - 1;
        yD = ydim - 1;
        zD = zdim - 1;
        sliceSize = xdim * ydim;
        rX = 3; // to third zero crossing
        rY = 3;
        rZ = 3;
        wSincXT = new float[1000];
        wSincYT = new float[1000];
        wSincZT = new float[1000];
        scaleX = 999.0f / (rX + 1.0f);
        scaleY = 999.0f / (rY + 1.0f);
        scaleZ = 999.0f / (rZ + 1.0f);
        invX = 1.0f / scaleX;
        invY = 1.0f / scaleY;
        invZ = 1.0f / scaleZ;
        wSincXT[0] = 1.0f;
        wSincYT[0] = 1.0f;
        wSincZT[0] = 1.0f;

        for (i = 1; i < 1000; i++) {
            diffX = i * invX;
            argX = Math.PI * diffX;
            wSincXT[i] = (float) ((Math.sin(argX) / argX) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffY = i * invY;
            argY = Math.PI * diffY;
            wSincYT[i] = (float) ((Math.sin(argY) / argY) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffZ = i * invZ;
            argZ = Math.PI * diffZ;
            wSincZT[i] = (float) ((Math.sin(argZ) / argZ) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
        }

        if (clip) {
            inputMin = volume[0];
            inputMax = volume[0];

            for (i = 1; i < volume.length; i++) {

                if (volume[i] < inputMin) {
                    inputMin = volume[i];
                }

                if (volume[i] > inputMax) {
                    inputMax = volume[i];
                }
            }
        } // if (clip)
    }

    /**
     * Setup 3D wSinc.
     *
     * @param  vol      volume comprising control points for the wSinc
     * @param  extents  vol extents (xdim, ydim, zdim)
     * @param  range    determines number of pixels used along each axis
     * @param  clip     if true clip output to range of input image
     */
    public void setup3DWSinc(float[] vol, int[] extents, int[] range, boolean clip) {
        int i;
        float invX, invY, invZ;
        float diffX, diffY, diffZ;
        double argX, argY, argZ;

        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        zdim = extents[2];
        this.clip = clip;
        xD = xdim - 1;
        yD = ydim - 1;
        zD = zdim - 1;
        sliceSize = xdim * ydim;
        rX = range[0];
        rY = range[1];
        rZ = range[2];
        wSincXT = new float[1000];
        wSincYT = new float[1000];
        wSincZT = new float[1000];
        scaleX = 999.0f / (rX + 1.0f);
        scaleY = 999.0f / (rY + 1.0f);
        scaleZ = 999.0f / (rZ + 1.0f);
        invX = 1.0f / scaleX;
        invY = 1.0f / scaleY;
        invZ = 1.0f / scaleZ;
        wSincXT[0] = 1.0f;
        wSincYT[0] = 1.0f;
        wSincZT[0] = 1.0f;

        for (i = 1; i < 1000; i++) {
            diffX = i * invX;
            argX = Math.PI * diffX;
            wSincXT[i] = (float) ((Math.sin(argX) / argX) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffY = i * invY;
            argY = Math.PI * diffY;
            wSincYT[i] = (float) ((Math.sin(argY) / argY) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffZ = i * invZ;
            argZ = Math.PI * diffZ;
            wSincZT[i] = (float) ((Math.sin(argZ) / argZ) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
        }

        inputMin = volume[0];
        inputMax = volume[0];

        for (i = 1; i < volume.length; i++) {

            if (volume[i] < inputMin) {
                inputMin = volume[i];
            }

            if (volume[i] > inputMax) {
                inputMax = volume[i];
            }
        }
    }

    /**
     * Setup 3D wSinc for color.
     *
     * @param  vol      volume comprising control points for the wSinc
     * @param  extents  vol extents (xdim, ydim, zdim)
     * @param  argbMax  maximum possible color value
     * @param  clip     if true clip output to range of input image
     */
    public void setup3DWSincC(float[] vol, int[] extents, float argbMax, boolean clip) {
        int i;
        float invX, invY, invZ;
        float diffX, diffY, diffZ;
        double argX, argY, argZ;

        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        zdim = extents[2];
        alphaMax = argbMax;
        redMax = argbMax;
        greenMax = argbMax;
        blueMax = argbMax;
        this.clip = clip;
        xD = xdim - 1;
        yD = ydim - 1;
        zD = zdim - 1;
        sliceSize = xdim * ydim;
        rX = 3; // to third zero crossing
        rY = 3;
        rZ = 3;
        wSincXT = new float[1000];
        wSincYT = new float[1000];
        wSincZT = new float[1000];
        scaleX = 999.0f / (rX + 1.0f);
        scaleY = 999.0f / (rY + 1.0f);
        scaleZ = 999.0f / (rZ + 1.0f);
        invX = 1.0f / scaleX;
        invY = 1.0f / scaleY;
        invZ = 1.0f / scaleZ;
        wSincXT[0] = 1.0f;
        wSincYT[0] = 1.0f;
        wSincZT[0] = 1.0f;

        for (i = 1; i < 1000; i++) {
            diffX = i * invX;
            argX = Math.PI * diffX;
            wSincXT[i] = (float) ((Math.sin(argX) / argX) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffY = i * invY;
            argY = Math.PI * diffY;
            wSincYT[i] = (float) ((Math.sin(argY) / argY) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
            diffZ = i * invZ;
            argZ = Math.PI * diffZ;
            wSincZT[i] = (float) ((Math.sin(argZ) / argZ) * 0.5 * (1.0 + Math.cos(Math.PI * i / 999.0)));
        }

        if (clip) {
            alphaMin = volume[0];
            alphaMax = volume[0];
            redMin = volume[1];
            redMax = volume[1];
            greenMin = volume[2];
            greenMax = volume[2];
            blueMin = volume[3];
            blueMax = volume[3];

            for (i = 1; i < (volume.length / 4); i++) {

                if (volume[4 * i] < alphaMin) {
                    alphaMin = volume[4 * i];
                }

                if (volume[4 * i] > alphaMax) {
                    alphaMax = volume[4 * i];
                }

                if (volume[(4 * i) + 1] < redMin) {
                    redMin = volume[(4 * i) + 1];
                }

                if (volume[(4 * i) + 1] > redMax) {
                    redMax = volume[(4 * i) + 1];
                }

                if (volume[(4 * i) + 2] < greenMin) {
                    greenMin = volume[(4 * i) + 2];
                }

                if (volume[(4 * i) + 2] > greenMax) {
                    greenMax = volume[(4 * i) + 2];
                }

                if (volume[(4 * i) + 3] < blueMin) {
                    blueMin = volume[(4 * i) + 3];
                }

                if (volume[(4 * i) + 3] > blueMax) {
                    blueMax = volume[(4 * i) + 3];
                }
            } // for (i = 1; i < (volume.length/4); i++)
        } // if (clip)
    }

    /**
     * 2D windowed sinc function.
     *
     * @param   x  float point index
     * @param   y  float point index
     *
     * @return  the wSinc2D interpolated data point
     */
    public float wSinc2D(float x, float y) {

        int xbase, ybase;
        int j0, j1;
        int l0, l1;
        float diffX, diffY;
        float wSincX, wSincY;
        float sum;
        int indexX, indexY;

        xbase = (int) x;
        ybase = (int) y;

        sum = 0.0f;

        for (j0 = xbase - rX; j0 <= (xbase + 1 + rX); j0++) {
            l0 = Math.max(Math.min(j0, xdim - 1), 0);
            diffX = x - j0;
            indexX = (int) Math.abs(scaleX * diffX);
            wSincX = wSincXT[indexX];

            for (j1 = ybase - rY; j1 <= (ybase + 1 + rY); j1++) {
                l1 = Math.max(Math.min(j1, ydim - 1), 0);
                diffY = y - j1;
                indexY = (int) Math.abs(scaleY * diffY);
                wSincY = wSincYT[indexY];
                sum += wSincX * wSincY * volume[(l1 * xdim) + l0];
            } // for (j1 = ybase - rY; j1 <= ybase + 1 + rY; j1++)
        } // for (j0 = xbase - rX; j0 <= xbase + 1 + rX; j0++)

        if (clip) {
            sum = Math.max(Math.min(sum, inputMax), inputMin);
        }

        return sum;
    }

    /**
     * 2D windowed sinc function for color.
     *
     * @param   x  float point index
     * @param   y  float point index
     *
     * @return  the wSinc2D interpolated data point
     */
    public float[] wSinc2DC(float x, float y) {

        int xbase, ybase;
        int j0, j1;
        int l0, l1;
        float diffX, diffY;
        float wSincX, wSincY;
        float[] sum = new float[4];
        int offset;
        float wXY;
        int indexX, indexY;

        xbase = (int) x;
        ybase = (int) y;

        sum[0] = 0.0f;
        sum[1] = 0.0f;
        sum[2] = 0.0f;
        sum[3] = 0.0f;

        for (j0 = xbase - rX; j0 <= (xbase + 1 + rX); j0++) {
            l0 = Math.max(Math.min(j0, xdim - 1), 0);
            diffX = x - j0;
            indexX = (int) Math.abs(scaleX * diffX);
            wSincX = wSincXT[indexX];

            for (j1 = ybase - rY; j1 <= (ybase + 1 + rY); j1++) {
                l1 = Math.max(Math.min(j1, ydim - 1), 0);
                diffY = y - j1;
                indexY = (int) Math.abs(scaleY * diffY);
                wSincY = wSincYT[indexY];
                offset = 4 * ((l1 * xdim) + l0);
                wXY = wSincX * wSincY;
                sum[0] += wXY * volume[offset];
                sum[1] += wXY * volume[offset + 1];
                sum[2] += wXY * volume[offset + 2];
                sum[3] += wXY * volume[offset + 3];
            } // for (j1 = ybase - rY; j1 <= ybase + 1 + rY; j1++)
        } // for (j0 = xbase - rX; j0 <= xbase + 1 + rX; j0++)

        // Since color is usually stored as ARGB with values limited to ranges from 0 to
        // 255, clamp the values between 0 and 255 if clip is false or restrict further if
        // clip is true
        sum[0] = Math.max(Math.min(sum[0], alphaMax), alphaMin);
        sum[1] = Math.max(Math.min(sum[1], redMax), redMin);
        sum[2] = Math.max(Math.min(sum[2], greenMax), greenMin);
        sum[3] = Math.max(Math.min(sum[3], blueMax), blueMin);

        return sum;
    }

    /**
     * 3D windowed sinc function.
     *
     * @param   x  float point index
     * @param   y  float point index
     * @param   z  float point index
     *
     * @return  the wSinc3D interpolated data point
     */
    public final float wSinc3D(float x, float y, float z) {

        int xbase, ybase, zbase;
        int j0, j1, j2;
        int l0, l1, l2;
        int endX, endY, endZ;
        int stX, stY, stZ;
        float wSincX, wXY;
        float sum;
        int offset;

        xbase = (int) x;
        ybase = (int) y;
        zbase = (int) z;

        // 15% - 20% faster since Math.max and Math.min are function calls
        // I also replaced the Math.abs but saw no speed improvement.
        sum = 0.0f;
        stX = xbase - rX;
        endX = xbase + 2 + rX;
        stY = ybase - rY;
        endY = ybase + 2 + rY;
        stZ = zbase - rZ;
        endZ = zbase + 2 + rZ;

        for (j0 = stX; j0 < endX; j0++) {

            // l0 = Math.max(Math.min(j0,xdim - 1),0);
            l0 = xD; // xdim - 1

            if (j0 < l0) {
                l0 = j0;
            }

            if (l0 < 0) {
                l0 = 0;
            }

            wSincX = wSincXT[(int) Math.abs(scaleX * (x - j0))];

            for (j1 = stY; j1 < endY; j1++) {

                // l1 = Math.max(Math.min(j1,ydim - 1),0);
                l1 = yD; // ydim-1;

                if (j1 < l1) {
                    l1 = j1;
                }

                if (l1 < 0) {
                    l1 = 0;
                }

                wXY = wSincX * wSincYT[(int) Math.abs(scaleY * (y - j1))];
                offset = (l1 * xdim) + l0;

                for (j2 = stZ; j2 < endZ; j2++) {

                    // l2 = Math.max(Math.min(j2,zdim - 1),0);
                    l2 = zD; // zdim-1;

                    if (j2 < l2) {
                        l2 = j2;
                    }

                    if (l2 < 0) {
                        l2 = 0;
                    }

                    sum += wXY * wSincZT[(int) Math.abs(scaleZ * (z - j2))] * volume[offset + (l2 * sliceSize)];
                } // for (j2 = zbase - rZ; j2 < zbase + 2 + rZ; j2++)
            } // for (j1 = ybase - rY; j1 < ybase + 2 + rY; j1++)
        } // for (j0 = xbase - rX; j0 < xbase + 2 + rX; j0++)

        if (clip) {
            sum = Math.max(Math.min(sum, inputMax), inputMin);
        }

        return sum;
    }

    /**
     * 3D windowed sinc function for color (3 channel images).
     *
     * @param   x  float point index
     * @param   y  float point index
     * @param   z  float point index
     *
     * @return  the wSinc3D interpolated data point
     */
    public float[] wSinc3DC(float x, float y, float z) {

        int xbase, ybase, zbase;
        int j0, j1, j2;
        int l0, l1, l2;
        float diffX, diffY, diffZ;
        float wSincX, wSincY, wSincZ;
        float[] sum = new float[4];
        int offset, offset2;
        int sliceSize = xdim * ydim;
        float wXY, wXYZ;
        int indexX, indexY, indexZ;

        xbase = (int) x;
        ybase = (int) y;
        zbase = (int) z;

        sum[0] = 0.0f;
        sum[1] = 0.0f;
        sum[2] = 0.0f;
        sum[3] = 0.0f;

        for (j0 = xbase - rX; j0 <= (xbase + 1 + rX); j0++) {
            l0 = Math.max(Math.min(j0, xdim - 1), 0);
            diffX = x - j0;
            indexX = (int) Math.abs(scaleX * diffX);
            wSincX = wSincXT[indexX];

            for (j1 = ybase - rY; j1 <= (ybase + 1 + rY); j1++) {
                l1 = Math.max(Math.min(j1, ydim - 1), 0);
                diffY = y - j1;
                indexY = (int) Math.abs(scaleY * diffY);
                wSincY = wSincYT[indexY];
                wXY = wSincX * wSincY;
                offset = (l1 * xdim) + l0;

                for (j2 = zbase - rZ; j2 <= (zbase + 1 + rZ); j2++) {
                    l2 = Math.max(Math.min(j2, zdim - 1), 0);
                    diffZ = z - j2;
                    indexZ = (int) Math.abs(scaleZ * diffZ);
                    wSincZ = wSincZT[indexZ];
                    offset2 = 4 * (offset + (sliceSize * l2));
                    wXYZ = wXY * wSincZ;
                    sum[0] += wXYZ * volume[offset2];
                    sum[1] += wXYZ * volume[offset2 + 1];
                    sum[2] += wXYZ * volume[offset2 + 2];
                    sum[3] += wXYZ * volume[offset2 + 3];
                } // for (j2 = zbase - rZ; j2 <= zbase + 1 + rZ; j2++)
            } // for (j1 = ybase - rY; j1 <= ybase + 1 + rY; j1++)
        } // for (j0 = xbase - rX; j0 <= xbase + 1 + rX; j0++)

        // Since color is usually stored as ARGB with values limited to ranges from 0 to
        // 255, clamp the values between 0 and 255 if clip is false or restrict further
        // if clip is true
        sum[0] = Math.max(Math.min(sum[0], alphaMax), alphaMin);
        sum[1] = Math.max(Math.min(sum[1], redMax), redMin);
        sum[2] = Math.max(Math.min(sum[2], greenMax), greenMin);
        sum[3] = Math.max(Math.min(sum[3], blueMax), blueMin);

        return sum;
    }

}
