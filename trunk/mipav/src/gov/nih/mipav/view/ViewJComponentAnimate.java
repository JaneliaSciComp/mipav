package gov.nih.mipav.view;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.image.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class ViewJComponentAnimate extends ViewJComponentBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2707021974594065054L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Opacity value used by the paint brush. value = 1.0 - opaque value =
     * 0.25 - default (mostly see through) */
    public float OPACITY = 0.25f;

    /** alphaBlending values for compositing two images. */
    protected float alphaBlend = 0.5f;

    /** frame - frame where the component image is displayed. */
    protected ViewJFrameBase frame;

    /** imageA - model for image A. */
    protected ModelImage imageA;

    /** imageB - model for image B. */
    protected ModelImage imageB;

    /** the image rendered in 2D graphics: */
    protected Image[] img;

    private int[] paintImageBuffer = null;

    /** current slice for 3D images: */
    protected int slice = -99;

    /** offset ranging from -255 to 255 add to each scaled red, green, and
     * blue */
    private int brightness; 

    /** scale factor ranging from 0.1 to 10.0 by which to multiply each red,
     * green, and blue */
    private float contrast; 

    /** whether or not to dispose of imageA and imageB true unless
     * ViewJFrameAnimate was passed an unscaled 3D image */
    private boolean disposeImage; 

    /** whether or not the brightness/contrast filter has been invoked */
    private boolean haveFiltered = false; 

    /** set true if deleteSlice hit in ViewJFrameAnimate */
    private boolean[] ignoreSlice; 

    /** Which image is currently active */
    private ModelImage imageActive = null;

    /** DOCUMENT ME! */
    private MediaTracker mt = null;

    /** in 4D row and column numbers for z slices */
    private int nRow, nColumn; 

    /** number of vois */
    private int nVOI; 

    /** the number of z slices in a 4D image equal to zDim before the
     * JDialogAnimate 4D to 3D conversion */
    private int originalZDim; 

    /** Buffer used to indicate if the pixel location is painted (true) or
     * unpainted (false). */
    private BitSet paintBitmap;

    /** Buffer that displays the combined paintBitmap and pixBuffer
     * buffers. */
    private int[] paintBuffer = null;

    /** Border color, red, green, and blue components: */
    private int red, green, blue;

    /** whether to number each z slice for 4D */
    private boolean showNumbers; 

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;

    /** image x, y, z dimensions in FileCoordinates */
    private int xDim, yDim, zDim;

    /** in 4D x location of slice numbering string */
    private int[] xLabel; 

    /** in 4D y location of slice numbering string */
    private int[] yLabel; 

    /** string for displaying slice number */
    private String[] zString; 

    /** PatientSlice contains all the Patient Coordinate system view-specific
     * data for rendering this component: */
    private PatientSlice m_kPatientSlice;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame         frame where the controls are obtained
     * @param  _imageA        Model of the image that will be displayed
     * @param  _LUTa          LUT used to display imageA
     * @param  imgBufferA     storage buffer used to display image A
     * @param  _imageB        Model of the image that will be displayed
     * @param  _LUTb          LUT used to display imageB
     * @param  imgBufferB     storage buffer used to display image B
     * @param  pixelBuffer    storage buffer used to build a displayable image
     * @param  zoom           initial magnification of image
     * @param  extents        initial display dimensions of the image
     * @param  logMagDisplay  display log magnitude of image
     * @param  alphaBl        alphaBlend
     * @param  disposeImage   if true dispose of imageA and imageB
     */
    public ViewJComponentAnimate(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                                 ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB, int[] pixelBuffer, float zoom,
                                 int[] extents, boolean logMagDisplay, float alphaBl, boolean disposeImage) {

        super(new Dimension(_imageA.getExtents()[0], _imageA.getExtents()[1]));

        interpMode = SMOOTH;

        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        VOIs = imageActive.getVOIs();

        if (VOIs != null) {
            nVOI = VOIs.size();
        } else {
            nVOI = 0;
        }

        this.disposeImage = disposeImage;
        zDim = imageA.getExtents()[2];
        img = new Image[zDim];

        // Before delete slice is pressed in ViewJFrameAnimate don't ignore any slices
        ignoreSlice = new boolean[zDim];

        for (int i = 0; i < zDim; i++) {
            ignoreSlice[i] = false;
        }

        xDim = imageA.getExtents()[0];
        yDim = imageA.getExtents()[1];

        resolutionX = imageA.getFileInfo(0).getResolutions()[0];
        resolutionY = imageA.getFileInfo(0).getResolutions()[1];

        if ((resolutionX <= 0.0f) || (resolutionY <= 0.0f)) {
            resolutionX = 1.0f;
            resolutionY = 1.0f;
        }

        if (resolutionX >= resolutionY) {
            resolutionX = resolutionX / resolutionY;
            resolutionY = 1.0f;
        } else if (resolutionY > resolutionX) {
            resolutionY = resolutionY / resolutionX;
            resolutionX = 1.0f;
        }

        setSize(Math.round(imageDim.width * resolutionX), Math.round(imageDim.height * resolutionY));

        alphaBlend = alphaBl;
        alphaBlend = frame.getAlphaBlend();
        System.err.println( alphaBlend );

        paintBitmap = imageA.getMask();

        /* create the slice renderer for this orientation: */
        m_kPatientSlice = new PatientSlice( imageA, _LUTa,
                                            imageB, _LUTb,
                                            FileInfoBase.UNKNOWN_ORIENT );
        setZoom(zoom, zoom);
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Shows the image.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   _LUTa      LUTa - to change to new LUT for imageA else null
     * @param   _LUTb      LUTb - to change to new LUT for imageB else null
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean buildImageObject(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow) {

        if (interpMode > -1) {
            setInterpolationMode(interpMode);
        }

        m_kPatientSlice.setLUTa( _LUTa );
        m_kPatientSlice.setLUTb( _LUTb );
        m_kPatientSlice.updateSlice( zSlice );
        slice = zSlice;
        if ( !ignoreSlice[slice] )
        {
            if ( m_kPatientSlice.showUsingOrientation( tSlice, paintBuffer, null, forceShow, false ) )
            {
                importImage(paintBuffer);
            }
        }
        return true;
    }

    /**
     * In 4D whether to show numbers for each z slice.
     *
     * @param  showNumbers  if true display numbers for each z slice in 4D
     */
    public void displayNumbers(boolean showNumbers) {
        this.showNumbers = showNumbers;
    }


    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  gcFlag  if true garbage collector should be called.
     */
    public void dispose(boolean gcFlag) {
        paintBuffer = null;
        paintBitmap = null;
        imageActive = null;
        frame = null;

        if (disposeImage) {

            if (imageA != null) {
                imageA.disposeLocal();
            }

            if (imageB != null) {
                imageB.disposeLocal();
            }
        }

        mt = null;

        if (img != null) {

            for (int i = 0; i < img.length; i++) {

                if (img[i] != null) {
                    img[i].flush();
                    img[i] = null;
                }
            }

            img = null;
        }

        if (gcFlag == true) {
            System.gc();
            System.runFinalization();
            System.gc();
        }
    }


    /**
     * Clean up some resources!
     */
    public void finalizeLocal() {

        if (img != null) {
            for (int i = 0; i < img.length; i++) {

                if (img[i] != null) {
                    img[i].flush();
                    img[i] = null;
                }
            }
            img = null;
        }
    }


    /**
     * Accessor that returns the active image.
     *
     * @return  active image
     */
    public ModelImage getActiveImage() {
        return imageActive;
    }

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
     * Gets Java image.
     *
     * @return  Java image
     */
    public Image getImage() {
        return img[slice];
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
    public BitSet getpaintBitmap() {
        return paintBitmap;
    }

    /**
     * Size set to object size.
     *
     * @return  dimension with the size
     */
    public Dimension getPreferredSize() {

        try {
            return new Dimension(Math.round(zoomX * imageDim.width * resolutionX), Math.round(zoomY * imageDim.height * resolutionY));
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
                return new Dimension(Math.round(zoomX * imageDim.width * resolutionX),
                                     Math.round(zoomY * imageDim.height * resolutionY));
            } else {
                wh.setSize(Math.round(zoomX * imageDim.width * resolutionX), Math.round(zoomY * imageDim.height * resolutionY));

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
     * Method to ensure img[slice] is not displayed.
     */
    public void ignoreSlice() {
        ignoreSlice[slice] = true;
        setLabelZ();
    }

    /**
     * Creates a Image object form an array of ints that have been formatted (packed) properly (i.e. aRGB)
     *
     * @param  data  Data (image) to be displayed that has been formatted (packed) properly (i.e. aRGB)
     */
    public void importImage(int[] data) {
        // If the MemoryImageSource and createImage steps are separated, then animate displays only the last image.
        // createImage must be executed right after MemoryImageSource.

        if (data != null) {
            mt = null;
            mt = new MediaTracker(this);
            memImageA = null;

            try {
                memImageA = new MemoryImageSource(imageDim.width, imageDim.height, data, 0, imageDim.width);
                memImageA.setAnimated(false);

                if (!haveFiltered) {
                    img[slice] = createImage(memImageA);
                } else {
                    img[slice] = createImage(new FilteredImageSource(memImageA,
                                                                     new ViewJFilterAnimate(brightness, contrast)));
                }

            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentBase.importImage.");
            }


            try {
                mt.addImage(img[slice], slice);
                mt.waitForID(slice);
            } catch (InterruptedException e) {
                System.gc();
                MipavUtil.displayError("Interrutped Exception: ComponentBase.importImage.");
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
        int i;

        try {

            if (g == null) {
                return;
            }

            if (img != null) {
                if ((paintImageBuffer == null) || (paintImageBuffer.length != (imageDim.width * imageDim.height))) {
                    paintImageBuffer = new int[imageDim.width * imageDim.height]; // make the buffer that will hold the
                    // paint
                } else {
                    Arrays.fill(paintImageBuffer, 0); // ensure erasure of old image, otherwise ghosting occurs
                }
                // build the paint image that will be blended on-screen
                makePaintImage(paintImageBuffer, paintBitmap, null, slice, frame, (imageA.getNDims() < 3));
                if (Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)) {
                    makePaintBitmapBorder(paintImageBuffer, paintBitmap, slice, frame);
                }

                int zoomedWidth = Math.round(zoomX * img[slice].getWidth(this) * resolutionX);
                int zoomedHeight = Math.round(zoomY * img[slice].getHeight(this) * resolutionY);

                g.setClip(getVisibleRect());

                if (((zoomX * resolutionX) != 1.0f) || ((zoomY * resolutionY) != 1.0f)) {
                    g.drawImage(img[slice], 0, 0, (int) (zoomedWidth + 0.5), (int) (zoomedHeight + 0.5),
                                0, 0, imageDim.width, imageDim.height, this);
                } else {
                    g.drawImage(img[slice], 0, 0, this);
                }

                memImageA = new MemoryImageSource(imageDim.width, imageDim.height, paintImageBuffer, 0, imageDim.width);
                Image paintImage = createImage(memImageA); // the image representing the paint mask
                // change rendering hint back from BILINEAR to nearest neighbor so that
                // all other painting will not be in interpolated mode
//                 g.setRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
//                                                        RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
//                 g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));
                g.drawImage(paintImage, 0, 0, zoomedWidth, zoomedHeight, 0, 0, img[slice].getWidth(this),
                            img[slice].getHeight(this), null);


                if (showNumbers) {
                    g.setFont(MipavUtil.font12);
                    g.setColor(Color.white);

                    for (i = 0; i < originalZDim; i++) {
                        g.drawString(zString[i], (int) ((zoomX * resolutionX * xLabel[i]) + 0.5),
                                     (int) ((zoomY * resolutionY * yLabel[i]) + 0.5));
                    }
                }

                if (showSliceNumber) {
                    g.setFont(MipavUtil.font12);
                    g.setColor(Color.white);
                    g.drawString(zString[slice], 5, (int) ((zoomY * resolutionY * imageDim.height) + 0.5) - 5);
                }

                for (i = nVOI - 1; i >= 0; i--) {
                    /*  DRAWING DISABLED
                    VOIs.VOIAt(i).drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0, 0, imageActive.getFileInfo(0).getResolutions(),
                                           imageActive.getFileInfo(0).getUnitsOfMeasure(), slice, FileInfoBase.UNKNOWN_ORIENT, g);
                */
                }
            } // if (img != null)
        } // try
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentAnimate.paint.");
        }
    }

    /**
     * Specifications that are only relevant to 4D images.
     *
     * @param  originalZDim  number of z slices
     * @param  nColumn       the number of columns of z slices
     * @param  nRow          the number of rows of z slices
     */
    public void set4DSpecs(int originalZDim, int nColumn, int nRow) {
        this.originalZDim = originalZDim;
        this.nColumn = nColumn;
        this.nRow = nRow;
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(int value) {
        alphaBlend = value / 100.0f;
    }

    /**
     * Sets color of the border surrounding each z slice in 4D images.
     *
     * @param  borderCol  border color surounding each z slice
     */
    public void setBorderCol(Color borderCol) {
        red = borderCol.getRed();
        green = borderCol.getGreen();
        blue = borderCol.getBlue();
    }

    /**
     * Method to set the brightness and contrast of the animate images.
     *
     * @param  brightness  int going from -255 to 255
     * @param  contrast    float scale factor
     */
    public void setBrightness(int brightness, float contrast) {
        int i;
        this.brightness = brightness;
        this.contrast = contrast;
        haveFiltered = true;
        int curSlice = slice;

        for (i = 0; ((i < zDim) && (!ignoreSlice[i])); i++) {
            buildImageObject(0, i, null, null, true);
        }

        slice = curSlice;
        paintComponent(getGraphics());
    }


    /**
     * The frame in which the image(s) is displayed, allocates the memory and uses this method to pass the references to
     * the buffers.
     *
     * @param  imgBufferA  storage buffer used to display image A
     * @param  imgBufferB  storage buffer used to display image B
     * @param  pixBuff     storage buffer used to build a displayable image
     * @param  paintBuff   storage buffer used to display the combined paintBitmap and pixBuffer buffers
     */
    public void setBuffers(float[] imgBufferA, float[] imgBufferB, int[] pixBuff, int[] paintBuff) {
        paintBuffer = paintBuff;
        m_kPatientSlice.setBuffers( imgBufferA, imgBufferB );
    }


    /**
     * For 4D sets the numbering string of each z slice and its x and y positions.
     */
    public void setLabelXY() {
        int originalXDim, originalYDim;
        int colNumber, rowNumber;
        int i;
        originalXDim = (xDim - (9 * nColumn) + 3) / nColumn;
        originalYDim = (yDim - (9 * nRow) + 3) / nRow;
        xLabel = new int[originalZDim];
        yLabel = new int[originalZDim];
        zString = new String[originalZDim];

        for (i = 0; i < originalZDim; i++) {
            colNumber = i % nColumn;
            rowNumber = i / nColumn;
            xLabel[i] = 5 + (colNumber * (originalXDim + 9));
            yLabel[i] = originalYDim - 2 + (rowNumber * (originalYDim + 9));
            zString[i] = String.valueOf(i);
        } // end of for (i = 0; i < originalZDim; i++)
    }

    /**
     * setlabelZ - For 3D sets the numbering string of each frame.
     */
    public void setLabelZ() {
        int i;
        int k = 0;
        zString = new String[zDim];

        for (i = 0; (i < zDim); i++) {

            // Only produce strings for nondeleted slices
            if (!ignoreSlice[i]) {
                zString[i] = String.valueOf(k);
                k++;
            }
        }
    }

    /**
     * Sets the paintBitmap to produce colored borders for 4D images.
     */
    // Note that separating 2 z slices are 3 black pixels, followed by 3 colored pixels,
    // followed by 3 colored pixels.  */
    public void setPaintMask() {
        int originalXDim, originalYDim;
        int colNumber, rowNumber;
        int i, j, k, l, offset;

        /* The originalXDim and originalYDim are the first and second dimensions in the
         *image before the 4D to 3D conversion in JDialogAnimate */
        originalXDim = (xDim - (9 * nColumn) + 3) / nColumn;
        originalYDim = (yDim - (9 * nRow) + 3) / nRow;

        for (i = 0; i < originalZDim; i++) {
            colNumber = i % nColumn;
            rowNumber = i / nColumn;

            for (j = 0; j < zDim; j++) {
                offset = j * xDim * yDim;

                for (k = 0; k < 3; k++) {

                    for (l = rowNumber * (originalYDim + 9); l < ((rowNumber * (originalYDim + 9)) + originalYDim + 6);
                             l++) {
                        paintBitmap.set(offset + k + (colNumber * (originalXDim + 9)) + (l * xDim));
                        paintBitmap.set(offset + k + originalXDim + 3 + (colNumber * (originalXDim + 9)) + (l * xDim));
                    }

                    for (l = colNumber * (originalXDim + 9); l < ((colNumber * (originalXDim + 9)) + originalXDim + 6);
                             l++) {
                        paintBitmap.set(offset + ((k + (rowNumber * (originalYDim + 9))) * xDim) + l);
                        paintBitmap.set(offset + ((k + originalYDim + 3 + (rowNumber * (originalYDim + 9))) * xDim) +
                                        l);
                    }
                } // end of for (k = 0; k < 3; k++)
            } // end of (j = 0; j < zDim; j++)
        } // end of for (i = 0; i < originalZDim; i++)
    }

    /**
     * Accessor that sets the paint mask.
     *
     * @param  mask  DOCUMENT ME!
     */
    public void setPaintMask(BitSet mask) {
        paintBitmap = mask;
    }

    // The following 2 functions set the RGB tables for ARGB images A and B.
    /**
     * Sets the RGB table for the ARGB image A.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTA(ModelRGB RGBT) {
        m_kPatientSlice.setRGBTA(RGBT);
    }

    /**
     * Sets the RGB table for the ARGB image B.
     *
     * @param  RGBT  RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {
        m_kPatientSlice.setRGBTB(RGBT);
    }

    /**
     * Accessor that sets the slice of the image.
     *
     * @param  _slice  image slice to be displayed
     */
    public void setSlice(int _slice) {
        slice = _slice;
        m_kPatientSlice.updateSlice(slice);
    }

    /**
     * Method to set the brightness and contrast of the animate slice.
     *
     * @param  brightness  int going from -255 to 255
     * @param  contrast    float scale factor
     */
    public void setSliceBrightness(int brightness, float contrast) {
        this.brightness = brightness;
        this.contrast = contrast;
        haveFiltered = true;
        buildImageObject(0, slice, null, null, true);
        paintComponent(getGraphics());
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
        setSize(Math.round(zX * imageDim.width * resolutionX), Math.round(zY * imageDim.height * resolutionY));
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
