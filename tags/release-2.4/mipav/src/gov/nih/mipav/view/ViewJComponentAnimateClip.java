package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.image.*;
import java.lang.System;
import java.io.*;
import javax.imageio.*;

public class ViewJComponentAnimateClip extends ViewJComponentBase {

    protected   int               interpMode = SMOOTH;
    protected	Image 		      img[];
    protected   String            string;
    private     float             zoomX = 1; // magnification, here zoomX = zoomY
    private     float             zoomY = 1; // and zoom is always a power of 2

    private     int               xDim,yDim,zDim;
    private     int               originalZDim; // the number of z slices in a 4D image
                                  // equal to zDim before the JDialogAnimate 4D to 3D conversion
    private     int               nRow,nColumn;  // in 4D row and column numbers for z slices
    private     int               xLabel[]; // in 4D x location of slice numbering string
    private     int               yLabel[]; // in 4D y location of slice numbering string
    private     String            zString[]; // string for displaying slice number
    private     boolean           showSliceNumber; // whether to number complete frame for 3D
    private     boolean           showNumbers; // whether to number each z slice for 4D

    private     float             resX,resY; // x and y resolutions

    private MediaTracker        mt = null;

    /**
    *   mode - used to describe the cursor mode
    */
    protected   int                 mode;

    protected   long                time;
    protected   int                 slice = -99;
    protected   int                 timeSlice = 0;

    /**
    *   alphaBlending values for compositing two images
    */
    protected   float               alphaBlend = 0.5f;
    protected   float               alphaPrime = 0.5f;

    private     int                 red,green,blue;

    private     boolean             ignoreSlice[];  // set true if deleteSlice hit in ViewJFrameAnimate

    /**
    *   Opacity value used by the paint brush.
    *             value = 1.0   - opaque
    *             value = 0.25  - default (mostly see through)
    */
    public float OPACITY             = 0.25f;

    private BufferedImage copy;


    /**
    *   Constructor: ImageA and ImageB are expected
    *                         to be of the same dimensionality !!
    *   @param _frame        frame where the controls are obtained
    *   @param _imageA      Model of the image that will be displayed
    *   @param _LUTa        LUT used to display imageA
    *   @param imgBufferA   storage buffer used to display image A
    *   @param _imageB      Model of the image that will be displayed
    *   @param _LUTb        LUT used to display imageB
    *   @param imgBufferB   storage buffer used to display image B
    *   @param pixelBuffer  storage buffer used to build a displayable image
    *   @param zoom         initial magnification of image
    *   @param extents      initial display dimensions of the image
    *   @param logMagDisplay   display log magnitude of image
    *   @param alphaBl      alphaBlend
    *   @param disposeImage if true dispose of imageA and imageB
    */
    public ViewJComponentAnimateClip(float zoom, int nRow, int nColumn, int nImage){

        super(new Dimension(nRow, nColumn));

        zDim        = nImage;
        img         = new Image[zDim];
        // Before delete slice is pressed in ViewJFrameAnimate don't ignore any slices
        ignoreSlice = new boolean[zDim];
        for (int i = 0; i < zDim; i++) {
            ignoreSlice[i] = false;
        }

        string      = "0";
        xDim = nRow;
        yDim = nColumn;

        resX = 1.0f;
        resY = 1.0f;
        setSize(Math.round(xDim), Math.round(yDim));

        alphaPrime = 1 - alphaBlend;

        setZoom(zoom, zoom);
        setVisible(true);
    }

    /**
    *  Accessor that returns the imageA
    *  @return     imageA
    */
    public ModelImage getImageA(){ return null;}

    /**
    *  Accessor that returns int red
    *  @return  red
    */
    public int getred() {return red;}

    /**
    *  Accessor that returns int green
    *  @return  green
    */
    public int getgreen() {return green;}

    /**
    *  Accessor that returns int blue
    *  @return   blue
    */
    public int getblue() {return blue;}

    /**
    *  Accessor that returns float OPACITY
    *  @return      OPACITY
    */
    public float getOPACITY() {return OPACITY;}

    /**
    *  In 4D whether to show numbers for each z slice
    *  @param showNumbers if true display numbers for each z slice in 4D
    */
    public void displayNumbers(boolean showNumbers) {
        this.showNumbers = showNumbers;
    }

    /**
    *  In 3D whether to show number for complete frame
    *  @param flag          if true show number for complete frame
    */
    public void setShowSliceNumber(boolean flag){
        showSliceNumber = flag;
    }

    /**
    *  Specifications that are only relevant to 4D images
    *  @param   originalZDim number of z slices
    *  @param   nColumn      the number of columns of z slices
    *  @param   nRow         the number of rows of z slices
    */
    public void set4DSpecs(int originalZDim, int nColumn, int nRow) {
        this.originalZDim = originalZDim;
        this.nColumn = nColumn;
        this.nRow = nRow;
    }

    /**
    *  For 4D sets the numbering string of each z slice and its x and y positions
    */
    public void setLabelXY() {
        int originalXDim,originalYDim;
        int colNumber, rowNumber;
        int i;
        originalXDim = (xDim - 9*nColumn + 3)/nColumn;
        originalYDim = (yDim - 9*nRow + 3)/nRow;
        xLabel = new int[originalZDim];
        yLabel = new int[originalZDim];
        zString = new String[originalZDim];
        for (i = 0; i < originalZDim; i++) {
            colNumber = i%nColumn;
            rowNumber = i/nColumn;
            xLabel[i]= 5 + colNumber*(originalXDim + 9);
            yLabel[i] = originalYDim - 2 + rowNumber*(originalYDim + 9);
            zString[i] = String.valueOf(i+1);
        } // end of for (i = 0; i < originalZDim; i++)
    }

    /**
    *  setlabelZ - For 3D sets the numbering string of each frame
    */
    public void setLabelZ() {
        int i;
        int k = 1;
        zString = new String[zDim];
        for (i = 0; (i < zDim);i++) {
            // Only produce strings for nondeleted slices
            if (!ignoreSlice[i]) {
              zString[i] = String.valueOf(k);
              k++;
            }
        }
    }

    /**
    *   Sets the alpha blending of parameter for two image displaying
    *   @param value   amount [0,100] that is the percentage of Image A to be displayed
    */
    public void setAlphaBlend(int value) {
        alphaBlend    = value/100.0f;
        alphaPrime    = 1 - alphaBlend;
    }


    /**
    *  Accessor that sets the slice of the image
    *  @param  _slice  image slice to be displayed
    */
    public void setSlice(int _slice) { slice = _slice;}



    /**
    *  For generating the display of 1 or 2 RGB images
    *  @param tSlice     t (time) slice to show
    *  @param zSlice     z slice to show
    *  @param forceShow  forces this method to import image and recalculate java image
    *  @return           boolean to indicate if the show was successful
    */
    public boolean buildImageObject(int zSlice, String dir) {
        slice = zSlice;
        importImage(dir + "captureImage" + slice + "." + "jpg");
         paintComponent(getGraphics());
        return true;
    }


    /**
    *  Sets all variables to null, disposes, and garbage collects
    *  @param gcFlag      if true garbage collector should be called.
    */
    public void dispose(boolean gcFlag){

        mt = null;

        if (img != null) {
            for (int i = 0; i < img.length; i++){
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
    *   Clean up some resources!
    */
    public void finalizeLocal(){
        if (img != null) {
            for (int i = 0; i < img.length; i++){
              if (img[i] != null) {
                img[i].flush();
                img[i] = null;
              }
            }
            img = null;
        }
        string      = null;
    }

    /**
    *  Sets the magnification in both x and y directions.
    *  @param zX  zoom in the x direction
    *  @param zY  zoom in the y direction
    */
    public void setZoom(float zX, float zY) {
        zoomX = zX;
        zoomY = zY;
        setSize(Math.round(zX*xDim*resX), Math.round(zY*yDim*resY));
        // setSize(Math.round(xDim), Math.round(yDim));
    }


    /**
    * Gets Java image
    * @return    Java image
    */
    public Image getImage() { return img[slice];}

    /**
    * Magnification in the x - dimension
    * @return    magnificaiton in the x - dimension
    */
    public float getZoomX() { return zoomX;}

    /**
    * Magnification in the y - dimension
    * @return    magnificaiton in the y - dimension
    */
    public float getZoomY() { return zoomY;}


    /**
    *  Sets the interpolation mode
    *  @param mode         mode to set it to
    */
    public void setInterpMode(int mode){
        interpMode = mode;
    }

    /**
    *  Gets the interpolation mode
    *  @return             returns the interpolation mode
    */
    public int getInterpMode(){
        return interpMode;
    }


    /**
    *   Creates a Image object form an array of ints that
    *   have been formatted (packed) properly (i.e. aRGB)
    *   @param data    Data (image) to be displayed that has been formatted (packed)
    *                  properly (i.e. aRGB)
    */
    public void importImage(String fileName){
    // If the MemoryImageSource and createImage steps are separated, then animate displays only
    // the last image.  createImage must be executed right after MemoryImageSource.
            File file;
            mt = new MediaTracker(this);

            file = new File(fileName);
            if ( !file.exists() ) {
                return;
            }

            try {
                if ( copy == null ) {
                    copy = new BufferedImage( xDim, yDim, BufferedImage.TYPE_INT_RGB );
                }
                copy = ImageIO.read(file);
                img[slice] = createImage(copy.getSource());
                mt.addImage(img[slice],slice);
                mt.waitForID(slice);
            }
            catch(InterruptedException e) {
                System.gc();
                MipavUtil.displayError("Interrutped Exception: ComponentBase.importImage.");
            }
            catch ( IOException e ) {
                e.printStackTrace();
            }
            mt = null;
            copy.flush();
            copy = null;
            file = null;
    }

    public Image[] getImageArray() {
        return img;
    }

    /**
    *   Method to ensure img[slice] is not displayed
    */
    public void ignoreSlice() {
        ignoreSlice[slice] = true;
        setLabelZ();
    }

    /**
    *  Method to call paint without erasing background this reduces flicker!
    *  @param g  Graphics handle
    */
    public void update(Graphics g) {
        paintComponent(g);
    }

    /**
    *  Method to call paint without erasing background
    *           this reduces flicker!
    */
    public void update() {
        paintComponent(getGraphics());
    }

    /**
    *   Paints the image and border
    *   @param g             Graphics handle
    */
    /* Since paintComponent is used rather than paintAnimate or some other name, then in addition to each
    direct call to paintComponent in updateImages, 1 or 2 calls to paintComponent may
    be made by Jcomponent.paint. */
    public void paintComponent(Graphics g) {
       int i;
        try {
            if (g == null) {return;}

            if (img != null) {
                g.setClip(getVisibleRect());
                if ((zoomX*resX != 1.0f) || (zoomY*resY != 1.0f)) {
                    g.drawImage(img[slice], 0,0,
                                (int)(zoomX*imageDim.width*resX+0.5),
                                (int)(zoomY*imageDim.height*resY+0.5),
                                 0, 0, imageDim.width, imageDim.height, this);
                }
                else {
                    g.drawImage(img[slice], 0,0, this);
                }

                if (showNumbers) {
                  g.setFont(MipavUtil.font12);
                  g.setColor(Color.white);
                  for (i = 0; i < originalZDim; i++) {
                      g.drawString(zString[i],(int)(zoomX*resX*xLabel[i]+0.5),
                                              (int)(zoomY*resY*yLabel[i]+0.5));
                  }
                }

                if (showSliceNumber) {
                    g.setFont(MipavUtil.font12);
                    g.setColor(Color.white);
                    g.drawString(zString[slice],5,(int)(zoomY*resY*imageDim.height+0.5) - 5);
                }
            } // if (img != null)
        } // try
        catch (OutOfMemoryError error ) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentAnimate.paint.");
        }
    }

    /**
    *   Gets the size of the object taking into account the zoom
    *   @param  wh  dimension
    *   @return     dimension with the size
    */
    public Dimension getSize(Dimension wh) {

        try{
            if ( wh == null) {
                return new Dimension(Math.round(zoomX*imageDim.width*resX),
                                     Math.round(zoomY*imageDim.height*resY));
            }
            else {
                wh.setSize(Math.round(zoomX*imageDim.width*resX),
                           Math.round(zoomY*imageDim.height*resY));
                return wh;
            }
        }
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getSize");
            return null;
        }
    }

    /**
    *   Size set to object size
    *   @return            dimension with the size
    */
    public Dimension getPreferredSize() {

        try {
            return new Dimension(Math.round(zoomX*imageDim.width*resX),
                                 Math.round(zoomY*imageDim.height*resY));
        }
        catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentBase.getPreferredSize");
            return null;
        }
    }

}

