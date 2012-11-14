package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class ViewJComponentColocalizationEM extends ViewJComponentBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6577659651877049839L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Opacity value used by the paint brush. value = 1.0 - opaque value = 0.25 - default (mostly see through) */
    public float OPACITY = 0.25f;


    /** frame - frame where the component image is displayed. */
    protected ViewJFrameBase frame;

    /** imageA - model for image A. */
    protected ModelImage imageA;

    /** imageB - model for image B. */
    protected ModelImage imageB;

    /** Creates histogram with ellipses around most prominent clusters. */
    protected int interpMode = SMOOTH;

    /** true if selected red, green, blue components present in RGB image. */
    protected ModelRGB RGBTA;

    /** DOCUMENT ME! */
    protected ModelRGB RGBTB;

    /** DOCUMENT ME! */
    private int bin2;

    /** DOCUMENT ME! */
    private int bottomPad;

    /** DOCUMENT ME! */
    private int brightness; // offset ranging from -255 to 255 add to each
                            // scaled red, green, and blue

    /** DOCUMENT ME! */
    private float contrast; // scale factor ranging from 0.1 to 10.0
                            // by which to multiply each red, green, and blue

    /** DOCUMENT ME! */
    private ModelImage destImage;

    /** DOCUMENT ME! */
    private double[] halfMajor;

    /** DOCUMENT ME! */
    private double[] halfMinor;

    /** DOCUMENT ME! */
    private boolean haveFiltered = false; // whether or not the brightness/
                                          // contrast filter has been invoked

    /** DOCUMENT ME! */
    private float[] imageBufferDest = null;

    /** DOCUMENT ME! */
    private Image imgDest;

    /** DOCUMENT ME! */
    private int leftPad;

    /** DOCUMENT ME! */
    private boolean logMagDisplay = false;


    /** DOCUMENT ME! */
    private int[] lutBufferRemappedDest = null;

    /** LUTa - lookup table for destImage. */
    private ModelLUT LUTdest;

    /** DOCUMENT ME! */
    private double[][] mean;

    /** DOCUMENT ME! */
    private MemoryImageSource memImageDest;

    /** DOCUMENT ME! */
    private double min1, max1, min2, max2;

    /** Buffer used to indicate if the pixel location is painted (true) or unpainted (false). */
    private BitSet paintBitmapDest;

    /** Buffer that displays the combined paintBitmap and pixBuffer buffers. */
    private int[] paintBufferDest = null;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    private int[] pixBufferDest = null;

    /** DOCUMENT ME! */
    private int red, green, blue;

    /** DOCUMENT ME! */
    private float resX, resY; // x and y resolutions

    /** DOCUMENT ME! */
    private int rightPad;

    /** DOCUMENT ME! */
    private double scale1, scale2;

    /** DOCUMENT ME! */
    private double[] theta;

    /** DOCUMENT ME! */
    private int topPad;

    /** DOCUMENT ME! */
    private boolean useBlue;

    /** DOCUMENT ME! */
    private boolean useGreen;

    /** DOCUMENT ME! */
    private boolean useRed;

    /** DOCUMENT ME! */
    private float zoomX = 1; // magnification, here zoomX = zoomY

    /** DOCUMENT ME! */
    private float zoomY = 1; // and zoom is always a power of 2

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame           frame where the controls are obtained
     * @param  _imageA          Model of the image that will be displayed
     * @param  _imageB          Model of the image that will be displayed
     * @param  destImage        Image with histogram information
     * @param  LUTdest          LUT used to display destImage
     * @param  imageBufferDest  storage buffer used to display destImage
     * @param  useRed           DOCUMENT ME!
     * @param  useGreen         DOCUMENT ME!
     * @param  useBlue          DOCUMENT ME!
     * @param  min1             DOCUMENT ME!
     * @param  max1             DOCUMENT ME!
     * @param  min2             DOCUMENT ME!
     * @param  max2             DOCUMENT ME!
     * @param  scale1           DOCUMENT ME!
     * @param  scale2           DOCUMENT ME!
     * @param  pixBufferDest    storage buffer used to build a displayable image
     * @param  paintBufferDest  DOCUMENT ME!
     * @param  zoom             initial magnification of image
     * @param  extents          initial display dimensions of the image
     * @param  logMagDisplay    display log magnitude of image
     * @param  leftPad          DOCUMENT ME!
     * @param  rightPad         DOCUMENT ME!
     * @param  bottomPad        DOCUMENT ME!
     * @param  topPad           DOCUMENT ME!
     * @param  mean             DOCUMENT ME!
     * @param  halfMajor        DOCUMENT ME!
     * @param  halfMinor        DOCUMENT ME!
     * @param  theta            DOCUMENT ME!
     */
    public ViewJComponentColocalizationEM(ViewJFrameBase _frame, ModelImage _imageA, ModelImage _imageB,
                                          ModelImage destImage, ModelLUT LUTdest, float[] imageBufferDest,
                                          boolean useRed, boolean useGreen, boolean useBlue, double min1, double max1,
                                          double min2, double max2, double scale1, double scale2, int[] pixBufferDest,
                                          int[] paintBufferDest, float zoom, int[] extents, boolean logMagDisplay,
                                          int leftPad, int rightPad, int bottomPad, int topPad, double[][] mean,
                                          double[] halfMajor, double[] halfMinor, double[] theta) {

        super(new Dimension(destImage.getExtents()[0], destImage.getExtents()[1]));

        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        this.destImage = destImage;
        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;


        this.min1 = min1;
        this.max1 = max1;
        this.min2 = min2;
        this.max2 = max2;
        this.scale1 = scale1;
        this.scale2 = scale2;

        this.leftPad = leftPad;
        this.rightPad = rightPad;
        this.bottomPad = bottomPad;
        this.topPad = topPad;
        bin2 = destImage.getExtents()[1] - bottomPad - topPad;

        resX = 1.0f;
        resY = 1.0f;
        setSize(Math.round(imageDim.width * resX), Math.round(imageDim.height * resY));

        this.LUTdest = LUTdest;
        lutBufferRemappedDest = new int[256];

        this.imageBufferDest = imageBufferDest;
        this.pixBufferDest = pixBufferDest;
        this.paintBufferDest = paintBufferDest;
        paintBitmapDest = destImage.getMask();
        this.logMagDisplay = logMagDisplay;
        this.mean = mean;
        this.halfMajor = halfMajor;
        this.halfMinor = halfMinor;
        this.theta = theta;
        setZoom(zoom, zoom);
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Shows the 2D histogram image.
     *
     * @param   _LUTdest   LUTdest - to change to new LUT for imageA else null
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean buildImageDestObject(ModelLUT _LUTdest, boolean forceShow) {

        float rangeA = 0;
        float imageMinA = 0, imageMaxA = 0;
        int xDim, yDim;
        int bufferSize;
        int lutHeightA = 0;
        int index;
        int Ra, Ga, Ba;
        int pix;
        float opacityPrime;
        int i;

        if (destImage == null) {
            return false;
        }

        if (LUTdest == null) {
            return false;
        }

        if (_LUTdest != null) {
            LUTdest = _LUTdest;
        }

        lutHeightA = LUTdest.getExtents()[1];

        xDim = destImage.getExtents()[0];
        yDim = destImage.getExtents()[1];

        if (lutHeightA != lutBufferRemappedDest.length) {

            try {
                lutBufferRemappedDest = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ViewJComponentColocalizationEM.buildImageObject");

                return false;
            }
        }

        LUTdest.exportIndexedLUT(lutBufferRemappedDest);

        bufferSize = xDim * yDim;

        if (logMagDisplay) {
            imageMinA = (float) (0.4342944819 * Math.log(1 + destImage.getMin()));
            imageMaxA = (float) (0.4342944819 * Math.log(1 + destImage.getMax()));
        } else {
            imageMinA = (float) destImage.getMin();
            imageMaxA = (float) destImage.getMax();
        }


        if (((imageMaxA - imageMinA) < 256) && (LUTdest.getLUTType() == ModelLUT.STRIPED)) {
            
        } else {
            rangeA = imageMaxA - imageMinA;

            if (rangeA == 0) {
                rangeA = 1;
            }

            
        }

        if (frame.getControls() != null) {
            OPACITY = frame.getControls().getTools().getOpacity();
            red = Math.round(frame.getControls().getTools().getPaintColor().getRed() * OPACITY);
            green = Math.round(frame.getControls().getTools().getPaintColor().getGreen() * OPACITY);
            blue = Math.round(frame.getControls().getTools().getPaintColor().getBlue() * OPACITY);
        } else {
            red = 128;
            green = 0;
            blue = 0;
        }

        opacityPrime = 1 - OPACITY;

        if (forceShow == true) {

            try {

                destImage.exportSliceXY(0, imageBufferDest);
            } catch (IOException error) {
                MipavUtil.displayError("" + error); // Need to fix this

                return false;
            }
        }


        if (logMagDisplay) {

            for (i = 0; i < imageBufferDest.length; i++) {
                imageBufferDest[i] = (float) (0.4342944819 * Math.log(1 + imageBufferDest[i]));
            }
        }

        int value;
        pix = 0;

        for (index = 0; index < bufferSize; index++) {
            TransferFunction tf_imgA = LUTdest.getTransferFunction();
            pix = (int) (tf_imgA.getRemappedValue(imageBufferDest[index], 256) + 0.5f);

            if (paintBitmapDest.get(index) == true) {
                value = lutBufferRemappedDest[pix];
                Ra = (value & 0x00ff0000) >> 16;
                Ga = (value & 0x0000ff00) >> 8;
                Ba = (value & 0x000000ff);
                value = 0xff000000 |
                            (((int) ((Ra * opacityPrime) + red) << 16) |
                                 (((int) ((Ga * opacityPrime) + green) << 8) | ((int) ((Ba * opacityPrime) + blue))));
                paintBufferDest[index] = value;
                pixBufferDest[index] = lutBufferRemappedDest[pix];
            } else {
                paintBufferDest[index] = pixBufferDest[index] = lutBufferRemappedDest[pix];
            }
        }

        importImageDest(paintBufferDest);

        return true;
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  gcFlag  if true garbage collector should be called.
     */
    public void dispose(boolean gcFlag) {

        lutBufferRemappedDest = null;
        pixBufferDest = null;
        paintBufferDest = null;
        paintBitmapDest = null;
        frame = null;

        img = null;

        if (gcFlag == true) {
            System.gc();
            System.runFinalization();
            System.gc();
        }
    }


    /**
     * Clean up some resources!
     */
    public void finalizeLocal() { }

    /**
     * Accessor that returns int blue.
     *
     * @return  blue
     */
    public int getblue() {
        return blue;
    }

    /**
     * Accessor that returns int green.
     *
     * @return  green
     */
    public int getgreen() {
        return green;
    }

    /**
     * Accessor that returns the imageA.
     *
     * @return  imageA
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Accessor that returns float OPACITY.
     *
     * @return  OPACITY
     */
    public float getOPACITY() {
        return OPACITY;
    }

    /**
     * Accessor that returns BitSet paintBitmap.
     *
     * @return  paintBitmap
     */
    public BitSet getpaintBitmapDest() {
        return paintBitmapDest;
    }

    /**
     * Size set to object size.
     *
     * @return  dimension with the size
     */
    public Dimension getPreferredSize() {

        try {
            return new Dimension(Math.round(zoomX * imageDim.width * resX), Math.round(zoomY * imageDim.height * resY));
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getPreferredSize");

            return null;
        }
    }


    /**
     * Accessor that returns int red.
     *
     * @return  red
     */
    public int getred() {
        return red;
    }

    /**
     * Gets the size of the object taking into account the zoom.
     *
     * @param   wh  dimension
     *
     * @return  dimension with the size
     */
    public Dimension getSize(Dimension wh) {

        try {

            if (wh == null) {
                return new Dimension(Math.round(zoomX * imageDim.width * resX),
                                     Math.round(zoomY * imageDim.height * resY));
            } else {
                wh.setSize(Math.round(zoomX * imageDim.width * resX), Math.round(zoomY * imageDim.height * resY));

                return wh;
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getSize");

            return null;
        }
    }


    /**
     * Magnification in the x - dimension.
     *
     * @return  magnificaiton in the x - dimension
     */
    public float getZoomX() {
        return zoomX;
    }

    /**
     * Magnification in the y - dimension.
     *
     * @return  magnificaiton in the y - dimension
     */
    public float getZoomY() {
        return zoomY;
    }


    /**
     * Creates a Image object form an array of ints that have been formatted (packed) properly (i.e. aRGB)
     *
     * @param  data  Data (image) to be displayed that has been formatted (packed) properly (i.e. aRGB)
     */
    public void importImageDest(int[] data) {
        // If the MemoryImageSource and createImage steps are separated, then animate displays only the last image.
        // createImage must be executed right after MemoryImageSource.

        if (data != null) {
            memImageDest = null;

            try {
                memImageDest = new MemoryImageSource(imageDim.width, imageDim.height, data, 0, imageDim.width);

                if (!haveFiltered) {
                    imgDest = createImage(memImageDest);
                } else {
                    imgDest = createImage(new FilteredImageSource(memImageDest,
                                                                  new ViewJFilterAnimate(brightness, contrast)));
                }

            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ViewJComponentColocalizationEM.importImage.");
            }

        }
    }

    /**
     * Paints the image and border.
     *
     * @param  g  Graphics handle
     */
    /* Since paintComponent is used rather than paintAnimate or some other name, then in addition to each
     * direct call to paintComponent in updateImages, 1 or 2 calls to paintComponent maybe made by Jcomponent.paint. */
    public void paintComponent(Graphics g) {
        int leftX, bottomY, rightX, topY;
        int bottomSPos, leftSPos;
        int min1X, min1Y;
        Color color1, color2;
        double ch1, ch2;
        double halfBig, halfSmall;
        int j;

        // Draw ellipses with with half axis lengths equal to
        // sigma, 2*sigma, and 3*sigma.
        Ellipse2D.Double ellipse;
        Ellipse2D.Double ellipse2;
        Ellipse2D.Double ellipse3;
        Graphics2D g2D;
        double xComp, yComp;

        try {

            if (g == null) {
                return;
            }

            if (imgDest != null) {
                g.setClip(getVisibleRect());

                if (((zoomX * resX) != 1.0f) || ((zoomY * resY) != 1.0f)) {
                    g.drawImage(imgDest, 0, 0, (int) ((zoomX * imageDim.width * resX) + 0.5),
                                (int) ((zoomY * imageDim.height * resY) + 0.5), 0, 0, imageDim.width, imageDim.height,
                                this);
                } else {
                    g.drawImage(imgDest, 0, 0, this);
                }

                for (j = 0; j < mean.length; j++) {
                    ch1 = ((mean[j][0] - min1) * scale1) + leftPad;
                    ch2 = ((mean[j][1] - min2) * scale2) + topPad;

                    // invert  ch2
                    ch2 = (2 * topPad) + bin2 - 1.0 - ch2;

                    // theta is the angle to the major axis
                    xComp = scale1 * Math.cos(theta[j]) * zoomX;
                    yComp = scale2 * Math.sin(theta[j]) * zoomY;
                    halfBig = halfMajor[j] * Math.sqrt((xComp * xComp) + (yComp * yComp));
                    xComp = scale1 * Math.sin(theta[j]) * zoomX;
                    ;
                    yComp = scale2 * Math.cos(theta[j]) * zoomY;
                    ;
                    halfSmall = halfMinor[j] * Math.sqrt((xComp * xComp) + (yComp * yComp));
                    g2D = (Graphics2D) g;
                    g2D.setColor(Color.yellow);

                    AffineTransform old = g2D.getTransform();
                    g2D.translate(ch1 * zoomX, ch2 * zoomY);
                    g2D.rotate(theta[j]);
                    ellipse = new Ellipse2D.Double((int) (-halfBig + 0.5), (int) (-halfSmall + 0.5),
                                                   (int) ((2.0 * halfBig) + 0.5), (int) ((2.0 * halfSmall) + 0.5));
                    g2D.draw(ellipse);
                    g2D.setColor(Color.green);
                    ellipse2 = new Ellipse2D.Double((int) ((-2.0 * halfBig) + 0.5), (int) ((-2.0 * halfSmall) + 0.5),
                                                    (int) ((4.0 * halfBig) + 0.5), (int) ((4.0 * halfSmall) + 0.5));
                    g2D.draw(ellipse2);
                    g2D.setColor(Color.red);
                    ellipse3 = new Ellipse2D.Double((int) ((-3.0 * halfBig) + 0.5), (int) ((-3.0 * halfSmall) + 0.5),
                                                    (int) ((6.0 * halfBig) + 0.5), (int) ((6.0 * halfSmall) + 0.5));
                    g2D.draw(ellipse3);
                    g2D.setTransform(old);
                }

                rightX = (int) (((imageDim.width - 1 - rightPad) * zoomX * resX) + 0.5);
                topY = (int) ((topPad * zoomY * resY) + 0.5);

                leftX = (int) (((leftPad - 20) * zoomX * resX) + 0.5);
                leftSPos = Math.max(leftX - 35, 0);
                bottomY = (int) (((imageDim.height - 1 - (bottomPad - 20)) * zoomY * resY) + 0.5);
                bottomSPos = Math.min(bottomY + 20, (int) (((imageDim.height - 1) * zoomY * resY) + 0.5));
                min1X = (int) ((leftPad * zoomX * resX) + 0.5);
                min1Y = (int) (((imageDim.height - 1 - bottomPad) * zoomY * resY) + 0.5);

                if (useRed && useGreen) {
                    color1 = Color.red;
                    color2 = Color.green;
                } else if (useRed && useBlue) {
                    color1 = Color.red;
                    color2 = Color.blue;
                } else if (useGreen && useBlue) {
                    color1 = Color.green;
                    color2 = Color.blue;
                } else {
                    color1 = Color.white;
                    color2 = Color.white;
                }

                g.setColor(color1);
                g.drawLine(leftX, bottomY, rightX, bottomY);
                g.drawLine(min1X, bottomY - 2, min1X, bottomY + 2);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(min1)), min1X, bottomSPos);
                } else {
                    g.drawString(String.valueOf(min1), min1X, bottomSPos);
                }

                g.drawLine(rightX, bottomY - 2, rightX, bottomY + 2);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(max1)), rightX, bottomSPos);
                } else {
                    g.drawString(String.valueOf(max1), rightX, bottomSPos);
                }

                g.setColor(color2);
                g.drawLine(leftX, bottomY, leftX, topY);
                g.drawLine(leftX - 2, min1Y, leftX + 2, min1Y);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(min2)), leftSPos, min1Y);
                } else {
                    g.drawString(String.valueOf(min2), leftSPos, min1Y);
                }

                g.drawLine(leftX - 2, topY, leftX + 2, topY);

                if (imageA.isColorImage()) {
                    g.drawString(String.valueOf(Math.round(max2)), leftSPos, topY);
                } else {
                    g.drawString(String.valueOf(max2), leftSPos, topY);
                }
            } // if (img != null)
        } // try
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentColocalizationEM.paint.");
        }
    }


    /**
     * Method to set the brightness and contrast of the animate images.
     *
     * @param  brightness  int going from -255 to 255
     * @param  contrast    float scale factor
     */
    public void setBrightness(int brightness, float contrast) {
        this.brightness = brightness;
        this.contrast = contrast;
        haveFiltered = true;
        buildImageDestObject(null, true);
        paintComponent(getGraphics());
    }

    /**
     * DOCUMENT ME!
     *
     * @param  logMagDisplay  DOCUMENT ME!
     */
    public void setLogMagDisplay(boolean logMagDisplay) {
        this.logMagDisplay = logMagDisplay;
    }

    /**
     * Accessor that sets the paint mask.
     *
     * @param  mask  DOCUMENT ME!
     */
    public void setPaintMaskDest(BitSet mask) {
        paintBitmapDest = mask;
    }


    // The following 2 functions set the RGB tables for ARGB images A and B.
    /**
     * Sets the RGB table for the ARGB image A.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTA(ModelRGB RGBT) {
        RGBTA = RGBT;
    }

    /**
     * Sets the RGB table for the ARGB image B.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {
        RGBTB = RGBT;
    }

    /**
     * Sets the magnification in both x and y directions.
     *
     * @param  zX  zoom in the x direction
     * @param  zY  zoom in the y direction
     */
    public void setZoom(float zX, float zY) {
        zoomX = zX;
        zoomY = zY;
        setSize(Math.round(zX * imageDim.width * resX), Math.round(zY * imageDim.height * resY));
    }

    /**
     * Method to call paint without erasing background this reduces flicker!
     */
    public void update() {
        paintComponent(getGraphics());
    }


    /**
     * Method to call paint without erasing background this reduces flicker!
     *
     * @param  g  Graphics handle
     */
    public void update(Graphics g) {
        paintComponent(g);
    }

}
