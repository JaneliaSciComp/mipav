package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.*;
import java.io.IOException;
import java.awt.*;
import java.awt.image.*;

/**
*   Preview image displayed when the user clicks on an image file in
*   the directory tree.  For black and white, a default LUT is created.
*   The image is resized if the panel holding it is resized.
*
*   @author     Neva Cherniavsky
*   @version    1.0
*   @see        ViewImageDirectory
*/
public class ViewJComponentPreviewImage extends ViewJComponentBase {

    private ModelImage  image;
    private float[]     imageBuffer;
    private int[]       paintBuffer;
    private int         bufferSize;
    private int         imageSize;
    private int         panelWidth;
    private int         panelHeight;
    private int         imgWidth;
    private int         imgHeight;
    private PreviewImageContainer parent;

    private MemoryImageSource memImage;

    private int brightness; // offset ranging from -255 to 255 add to each
                                             // scaled red, green, and blue
    private float contrast; // scale factor ranging from 0.1 to 10.0
                                           // by which to multiply each red, green, and blue

    /**
    *   Creates new preview image from the model image.
    *   The extents will only be 2D.  The parent is
    *   needed because of the order in which Java calls
    *   methods when a component is resized.
    *   @param _image   Model image to create preview image from.
    *   @param extents  X and Y dimensions of image
    *   @param _parent  Frame that called this
    */
    public ViewJComponentPreviewImage(ModelImage _image, int[] extents, PreviewImageContainer _parent) {
        super(extents, NA, false, _image.getFileInfo(0).getAxisOrientation());
        image        = _image;
        imageSize    = extents[0] * extents[1];
        if (image.isColorImage())   bufferSize = imageSize*4;
        else                        bufferSize = imageSize;

        imageBuffer = new float[bufferSize];
        paintBuffer = new int[imageSize];
        parent = _parent;
    }

    /**
    *   Creates the Java image to be displayed from the
    *   model image.  Makes it from the appropriate slice.
    *   @param slice    Slice of image to create java image from.
    *   @return         Flag indicating success or failure.
    */
    public boolean createImg(int slice) {
        float redMapped, greenMapped, blueMapped;

        if (image.isColorImage()) {

            try {
                image.exportData(slice*bufferSize, bufferSize, imageBuffer);
            }
            catch (IOException error) {
                MipavUtil.displayError("" + error);
                return false;
            }
            for (int index=0, j=0; j < imageSize; index += 4, j++) {
                redMapped   = imageBuffer[index+1];
                greenMapped = imageBuffer[index+2];
                blueMapped  = imageBuffer[index+3];
                paintBuffer[j] =  0xff000000 |
                                  (((int)(redMapped)   << 16) |
                                  (((int)(greenMapped) << 8)  |
                                  ((int)(blueMapped))));
            }

        }
        else {
            int   extentsLUT[]  = new int[] {4, 256};
            ModelLUT LUT        = new ModelLUT(ModelLUT.GRAY, 256, extentsLUT);
            int[]    lutBuffer  = new int[256];

            float min, max;
            if (image.getType() == ModelStorageBase.UBYTE) {
                min = 0;
                max = 255;
            }
            else if (image.getType() == ModelStorageBase.BYTE) {
                min = -128;
                max = 127;
            }
            else {
                min = (float)image.getMin();
                max = (float)image.getMax();
            }
            float imgMin = (float)image.getMin();
            float imgMax = (float)image.getMax();
            LUT.resetTransferLine(min, imgMin, max, imgMax);

            LUT.exportIndexedLUT(lutBuffer);

            float remapConst  = 255f/(max-min);

            try {
                if (image.getType() == ModelStorageBase.COMPLEX) {
                    image.exportComplexSliceXY(slice, imageBuffer, image.getLogMagDisplay());
                }
                else {
                    image.exportSliceXY(slice, imageBuffer);
                }
            }
            catch (IOException error) {
                MipavUtil.displayError("" + error);    // Need to fix this
                return false;
            }
            int pix = 0;
            TransferFunction tf_img = LUT.getTransferFunction();
            for (int index=0; index < bufferSize; index++) {
                pix = (int)(tf_img.getRemappedValue(imageBuffer[index], 256) + 0.5f);
                try {
                    paintBuffer[index] = lutBuffer[pix];
                }
                catch(ArrayIndexOutOfBoundsException e) {

                    Preferences.debug("error = " + e + "\n");
                    Preferences.debug("index = " + index + " pix = " + pix + "\n");
                }
            }
        }
        importImage(paintBuffer);
        return true;

    }

    /**
     * Gets the size of the image (width * height)
     * @return the size of the image
     */
    public int getImageSize() {
        return this.imageSize;
    }

    /**
     *   Creates a Image object form an array of ints that
     *   have been formatted (packed) properly (i.e. aRGB)
     *   @param data    Data (image) to be displayed that has been formatted (packed)
     *                  properly (i.e. aRGB)
     */

    public void importImage(int data[], boolean haveFilter) {
      // If the MemoryImageSource and createImage steps are separated, then animate displays only
      // the last image.  createImage must be executed right after MemoryImageSource
      if (data != null) {
        memImage = null;
        try {
          memImage = new MemoryImageSource(imageDim.width,
                                           imageDim.height,
                                           data,
                                           0,
                                           imageDim.width);
          if (haveFilter)
          {
              img = createImage(new FilteredImageSource(memImage, new ViewJFilterAnimate(brightness, contrast)));
          }
          else
          {
              img = Toolkit.getDefaultToolkit().createImage(memImage);
          }
        }
        catch (OutOfMemoryError error) {
          System.gc();
          MipavUtil.displayError("Out of memory: ComponentBase.importImage.");
        }

      }
    }

    /**
     *    Method to set the brightness and contrast of the animate slice
     *    @param  brightness int going from -255 to 255
     *    @param contrast float scale factor
     */
    public void setSliceBrightness(int brightness, float contrast) {
      this.brightness = brightness;
      this.contrast = contrast;
      importImage(paintBuffer, true);
      if (getGraphics() != null) {
        paintComponent(getGraphics());
      }
    }


    /**
    *   Sets buffers to null.
    *   @param gc   Flag indicating if the garbage
    *               collector should be called.
    */
    public void dispose(boolean gc) {
        this.disposeLocal();
        if (image != null) {
            image.disposeLocal();
            image = null;
        }
        imageBuffer = null;
        paintBuffer = null;
        memImage = null;

        if (gc) System.gc();
    }

    /**
    *   Sets the size of the image to be painted.
    *   @param width    Width of panel where image will be placed.
    *   @param height   Height of panel where image will be placed.
    */
    public void setImgSize(int width, int height) {
        this.panelWidth  = width - 10;
        this.panelHeight = height - 10;
        int w = img.getWidth(this);
        int h = img.getHeight(this);

        float fracX = (float) panelWidth / w;
        // find fraction y such that height*y = 200
        float fracY = (float) panelHeight / h;
        // min(fractX, fractY) will size image within 400 x 200 rectangle
        float min = (fracX < fracY) ? fracX : fracY;
        imgWidth = Math.round(min * w);
        imgHeight = Math.round(min * h);
    }

    /**
    *   Paints the component using the previous set
    *   image width and height.
    *   @param g    Graphics to draw image in.
    */
    public void paintComponent(Graphics g) {

        g.setClip(getVisibleRect());
        g.drawImage(img,
                    0,
                    0,
                    imgWidth,
                    imgHeight,
                    0, 0, img.getWidth(this), img.getHeight(this), null);
    }

    /**
    *   Size set to object size.  Need to get real panel
    *   size first from the parent, then reset the image size.
    *   @return     Dimension with the size
    */
    public Dimension getPreferredSize() {
      Dimension size = null;
      size = parent.getPanelSize();
      setImgSize(size.width, size.height);
        try {
            return new Dimension(imgWidth, imgHeight);
        }
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getPreferredSize");
            return null;
        }
    }
}
