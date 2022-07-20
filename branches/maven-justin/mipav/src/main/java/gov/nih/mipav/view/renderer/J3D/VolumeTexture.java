package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.awt.image.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Defines 3D texture node for a volume of data. There is a large amount of commented out code. We were trying to have
 * the texture image updated dynamically (i.e., without having to remove the texture map from the scene, updating the
 * texture and putting back into the scene. This is slow and causes flicker. However, it turned out there is a bug in
 * Java3D that makes it impossible to update the texture dynamically. Therefore we left in the code until the bug has
 * been fixed.
 */
public class VolumeTexture {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Reference to the image as ModelImage instance. */
    protected ModelImage image;

    /** This are the actual dimensions of the texture associated with each dimension of the volume. */
    protected int m_iTextureSizeX;

    /** DOCUMENT ME! */
    protected int m_iTextureSizeY;

    /** DOCUMENT ME! */
    protected int m_iTextureSizeZ;

    // There is a bug that when fixed in a later version of Java3D that will allow
    // dynamically updating the texture map without the need to remove and add the
    // the texture map to the
    // private BufferedImage []  bufArray;
    // private Updater          updater;// = new Updater();

    /**
     * These vectors are used for mapping each coordinate from its real range into the range [0,1] where real coordinate
     * 0 maps to texture coordinate 0, and real coordinate at the maximum sample index along that axis maps to 1.
     */
    protected Vector4f m_kCoordMapX;

    /** DOCUMENT ME! */
    protected Vector4f m_kCoordMapY;

    /** DOCUMENT ME! */
    protected Vector4f m_kCoordMapZ;

    /** These are declared in the class to avoid allocating temporary instances each call into methods of this class. */
    protected Vector4f m_kTempPlane = new Vector4f();

    /** Image buffer for each image slice. */
    private BufferedImage[] m_akImage;

    /** Image component 3D reference which contains the 3D texture volume. */
    private ImageComponent3D m_kImageComponent;

    /** Texture coordinate generation function definition for slicing. */
    /** in any direction. (s,t,r) <--> (X,Y,Z). */
    private TexCoordGeneration m_kTexCoordGeneration;

    /** Default axis-aligned texture coordinate generation function. */
    private TexCoordGeneration m_kTexCoordGenerationAxisAligned;

    /** 3D texture of the entire volume. */
    private Texture3D m_kTexture;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create texture(s) and automatic texture coordinate generation parameters for each slicing of the specified volume
     * along each of the three axes.
     *
     * @param  kVolume  volume for which to create the associated texture information
     */
    public VolumeTexture(ModelImage kVolume) {

        // Keep track of the information which describes the volume.
        image = kVolume;

        resampleImage();
        createTexture();
        // updater = new Updater(m_kImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets all variables to null, disposes.
     */
    public void disposeLocal() {
        m_kImageComponent = null;
        m_kTempPlane = null;
        m_kTexCoordGeneration = null;
        m_kTexCoordGenerationAxisAligned = null;

        m_kTexture = null;
        m_akImage = null;
        m_kCoordMapX = null;
        m_kCoordMapY = null;
        m_kCoordMapZ = null;

        m_kTempPlane = null;
        image = null;
    }

    /*
     * private class Updater  implements ImageComponent3D.Updater {
     *
     * int imgBuffer[]; //BufferedImage[] bufImages = null;
     *
     * public Updater(BufferedImage m_kImage) { imgBuffer = ( (DataBufferInt) m_kImage.getRaster().getDataBuffer()).
     * getData(); //this.bufImages = bufImg; }
     *
     * public void updateData(ImageComponent3D imageComponent,                      int index,                      int x,
     *                      int y,                      int width,                      int height) {
     *
     * if (index == 0)   System.out.println("updater Comp = " + imageComponent); if (index == 0)
     * System.out.println("updater ImageX = " + imageComponent.getImage(index)); if (index == 0)
     * System.out.println("updater image = " + m_kImage);
     *
     * for (int i = 0; i < imgBuffer.length; i++) {   ( (DataBufferInt)
     * bufArray[index].getRaster().getDataBuffer()).getData()[       i] = imgBuffer[i]; }
     *
     * //int temp = ((DataBufferInt)imageComponent.getImage(index).getRaster().getDataBuffer()).getData()[i];
     * //System.out.println("Hey before  [" + i + "] = " + temp); //System.out.println("Image before  [" + i + "] = " +
     * imgBuffer[i]); //((DataBufferInt)imageComponent.getImage(index).getRaster().getDataBuffer()).getData()[i] =
     * imgBuffer[i]; //temp = ((DataBufferInt)imageComponent.getImage(index).getRaster().getDataBuffer()).getData()[i];
     * //System.out.println("Hey after  [" + i + "] = " + temp  + "\n");
     *
     * }
     *
     *} */


    /**
     * Access texture data from the BufferedImage with given z slice number.
     *
     * @param   sliceZ  indicates the image slice that is to be set.
     *
     * @return  reference to integer array instance of texture data.
     */
    public int[] getBufferedRaster(int sliceZ) {
        return ((DataBufferInt) m_akImage[sliceZ].getRaster().getDataBuffer()).getData();
    }

    /**
     * Access functions for Mapping from texture coordinates to model coordinates and vice versa: getCoordMapX:
     *
     * @return  DOCUMENT ME!
     */
    public Vector4f getCoordMapX() {
        return m_kCoordMapX;
    }

    /**
     * getCoordMapY:
     *
     * @return  DOCUMENT ME!
     */
    public Vector4f getCoordMapY() {
        return m_kCoordMapY;
    }

    /**
     * getCoordMapZ:
     *
     * @return  DOCUMENT ME!
     */
    public Vector4f getCoordMapZ() {
        return m_kCoordMapZ;
    }

    /**
     * Get the current model image.
     *
     * @return  image Current model image.
     */
    public ModelImage getImage() {
        return image;
    }

    /**
     * Access automatic texture coordinate generation parameters associated with volume for a 3D texture.
     *
     * @return  requested automatic texture coordinate generation parameters
     */
    public TexCoordGeneration getTexCoordGeneration() {
        return m_kTexCoordGeneration;
    }

    /**
     * Access 3D texture object.
     *
     * @return  Texture3D instance of the 3D texture of the image
     */
    public Texture3D getTexture() {
        return m_kTexture;
    }


    /**
     * Sets the image component3D to import changed slice data.
     *
     * @param  sliceZ  indicates the image slice that is to be set.
     */
    public void setImageComponent(int sliceZ) {

        m_kImageComponent.set(sliceZ, m_akImage[sliceZ]);

    }

    /**
     * Apply the specified transform to the axis aligned texture.
     *
     * @param  kTransform  Transform3D
     */
    public void setTransform(Transform3D kTransform) {
        m_kTexCoordGenerationAxisAligned.getPlaneQ(m_kTempPlane);
        kTransform.transform(m_kTempPlane);
        m_kTexCoordGeneration.setPlaneQ(m_kTempPlane);

        m_kTexCoordGenerationAxisAligned.getPlaneR(m_kTempPlane);
        kTransform.transform(m_kTempPlane);
        m_kTexCoordGeneration.setPlaneR(m_kTempPlane);

        m_kTexCoordGenerationAxisAligned.getPlaneS(m_kTempPlane);
        kTransform.transform(m_kTempPlane);
        m_kTexCoordGeneration.setPlaneS(m_kTempPlane);

        m_kTexCoordGenerationAxisAligned.getPlaneT(m_kTempPlane);
        kTransform.transform(m_kTempPlane);
        m_kTexCoordGeneration.setPlaneT(m_kTempPlane);
    }

    /**
     * Convert the dimensions to Power of 2.
     *
     * @param   dim  Dimension value.
     *
     * @return  dim Converted dimention value.
     */
    protected int dimPowerOfTwo(int dim) {

        // 128^3 x 4 is 8MB
        // 256^3 x 4 is 64MB
        // 512^3 x 4 is 512MB
        if (dim <= 16) {
            return 16;
        } else if (dim <= 32) {
            return 32;
        } else if (dim <= 64) {

            if (dim > 40) {
                return 64;
            } else {
                return 32;
            }
        } else if (dim <= 128) {

            if (dim > 80) {
                return 128;
            } else {
                return 64;
            }
        } else if (dim <= 256) {

            if (dim > 160) {
                return 256;
            } else {
                return 128;
            }
        } else if (dim <= 512) {

            if (dim > 448) {
                return 512;
            } else {
                return 256;
            }
        } else {
            return 512;
        }
    }

    /**
     * Call dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }


    /**
     * Ressample images to Power of 2.
     */
    protected void resampleImage() {

        int[] extents = image.getExtents();
        float[] res = image.getFileInfo(0).getResolutions();
        int[] volExtents = new int[3];
        float[] newRes = new float[3];
        int volSize = 1;

        boolean originalVolPowerOfTwo = true;
        AlgorithmTransform transformImg;

        for (int i = 0; i < extents.length; i++) {

            volExtents[i] = dimPowerOfTwo(extents[i]);
            volSize *= volExtents[i];

            if (volExtents[i] != extents[i]) {
                originalVolPowerOfTwo = false;

            }

            newRes[i] = (res[i] * extents[i]) / volExtents[i];
        }

        // The image should have really been resampled before opening the surface viewer.
        if (originalVolPowerOfTwo == false) {
            // Resample image into volume that is a power of two !
            // Preferences.debug("ViewJFrameSurfaceRenderer.buildTexture: Volume resampled.");

            transformImg = new AlgorithmTransform(image, new TransMatrix(4), AlgorithmTransform.CUBIC_LAGRANGIAN,
                                                  newRes[0], newRes[1], newRes[2], volExtents[0],

                                                  // AlgorithmTransform.TRILINEAR, newRes[0], newRes[1], newRes[2],
            // volExtents[0],
                                                  volExtents[1], volExtents[2], false, true, false);

            transformImg.setRunningInSeparateThread(false);
            transformImg.run();

            if (transformImg.isCompleted() == false) { }

            image = transformImg.getTransformedImage();
            image.calcMinMax();

            // new ViewJFrameImage(resampledImage, null, new Dimension(200, 200), image.getUserInterface());

            transformImg.disposeLocal();
            transformImg = null;
        }

        m_iTextureSizeX = volExtents[0];
        m_iTextureSizeY = volExtents[1];
        m_iTextureSizeZ = volExtents[2];

        /* Changed to -1 to match NodeVolumeTextureRender and
         * TextureSculptor: */
        float fMaxX = (float) (m_iTextureSizeX - 1) * newRes[0];
        float fMaxY = (float) (m_iTextureSizeY - 1) * newRes[1];
        float fMaxZ = (float) (m_iTextureSizeZ - 1) * newRes[2];
        float fMax = fMaxX;

        if (fMaxY > fMax) {
            fMax = fMaxY;
        }

        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }

        // Define vectors used for mapping each coordinate from its
        // real range X=[0,xdim-1], Y=[0,ydim-1], Z=[0,zdim-1] into
        // the (s,t,r) texture coordinates where each texture coordinate
        // is defined over the range [0,1].
        // Note that integral screen coordinates represent pixel centers
        // whereas integral texture coordinates represent texel boundaries.
        // Therefore, X maps to s=(X+0.5)/xdim, and correspondingly
        // for Y and Z mapping to t and r, respectively.
        // m_kCoordMapX = new Vector4f (1.0f/fMaxX, 0.0f,       0.0f,       0.5f/fMaxX);
        // m_kCoordMapY = new Vector4f (0.0f,       1.0f/fMaxY, 0.0f,       0.5f/fMaxY);
        // m_kCoordMapZ = new Vector4f (0.0f,       0.0f,       1.0f/fMaxZ, 0.5f/fMaxZ);
        m_kCoordMapX = new Vector4f((fMax / fMaxX) / 2.0f, 0.0f, 0.0f, 0.5f);
        m_kCoordMapY = new Vector4f(0.0f, (fMax / fMaxY) / 2.0f, 0.0f, 0.5f);
        m_kCoordMapZ = new Vector4f(0.0f, 0.0f, -(fMax / fMaxZ) / 2.0f, 0.5f);

        extents = null;
        res = null;
        volExtents = null;
        newRes = null;
    }

    /**
     * Create a single 3D texture coordinate generation function.
     */
    private void createTexture() {

        // Create a single 3D texture coordinate generation function
        // (s,t,r) <--> (X,Y,Z)

        // Create a single 3D texture coordinate generation function which
        // is axis aligned and then a second one which is the result of
        // a transform being applied to it.
        m_kTexCoordGenerationAxisAligned = new TexCoordGeneration(TexCoordGeneration.OBJECT_LINEAR,
                                                                  TexCoordGeneration.TEXTURE_COORDINATE_3, m_kCoordMapX,
                                                                  m_kCoordMapY, m_kCoordMapZ);
        m_kTexCoordGeneration = new TexCoordGeneration(TexCoordGeneration.OBJECT_LINEAR,
                                                       TexCoordGeneration.TEXTURE_COORDINATE_3, m_kCoordMapX,
                                                       m_kCoordMapY, m_kCoordMapZ);
        m_kTexCoordGeneration.setCapability(TexCoordGeneration.ALLOW_PLANE_WRITE);

        // Create an image for storing XY slices of the 3D texture.
        // BufferedImage m_kImageX = new BufferedImage(m_iTextureSizeX, m_iTextureSizeY,
        // BufferedImage.TYPE_INT_ARGB);
        int iNumSlices = image.getExtents()[2];
        m_akImage = new BufferedImage[iNumSlices];
        // int [] aiImageDataX =     ((DataBufferInt)m_kImageX.getRaster().getDataBuffer()).getData();


        // Create the 3D image component for storing in the texture.
        m_kImageComponent = new ImageComponent3D(ImageComponent.FORMAT_RGBA, m_iTextureSizeX, m_iTextureSizeY,
                                                 m_iTextureSizeZ, true, false);
        m_kImageComponent.setCapability(ImageComponent3D.ALLOW_IMAGE_WRITE);
        m_kImageComponent.setCapability(ImageComponent3D.ALLOW_IMAGE_READ);
        m_kImageComponent.setCapability(ImageComponent3D.ALLOW_SIZE_READ);
        m_kImageComponent.setCapability(ImageComponent3D.ALLOW_FORMAT_READ);

        // Loop through each Z coordinate and write the XY slice of
        // volume data to the raster and then write that raster into
        // the 3D image component.
        // System.out.println("Hey = " + image.getExtents()[2]);

        // bufArray = new BufferedImage[image.getExtents()[2]];

        //int bufferSize = image.getSliceSize();

        for (int iSliceZ = 0; iSliceZ < iNumSlices; ++iSliceZ) {
            m_akImage[iSliceZ] = new BufferedImage(m_iTextureSizeX, m_iTextureSizeY, BufferedImage.TYPE_INT_ARGB);

            /*
             * m_kImageX = new BufferedImage(m_iTextureSizeX, m_iTextureSizeY,
             * BufferedImage.TYPE_INT_ARGB); if (iSliceZ == 0) System.out.println("first ImageX = " + m_kImageX);
             * aiImageDataX = ((DataBufferInt)m_kImageX.getRaster().getDataBuffer()).getData(); try {
             * image.exportData(bufferSize*iSliceZ, bufferSize, aiImageDataX);   }   catch (IOException error) {
             * MipavUtil.displayError("" + error);    // Need to fix this   } for (int i = 0; i < aiImageDataX.length;
             * i++) {   aiImageDataX[i] = (60 << 24) | (((int)(100)   << 16) | (((int)(5*iSliceZ) << 8) | ((int)(0))));
             * } bufArray[iSliceZ] = m_kImageX;
             */
            setImageComponent(iSliceZ);
        }
        // updater = new Updater(bufArray);

        // Create a single 3D texture.  Setup the parameters which
        // define filtering and the handling at the borders.
        // Anything outside the volume is defined to be transparent.
        m_kTexture = new Texture3D(Texture.BASE_LEVEL, Texture.RGBA, m_iTextureSizeX, m_iTextureSizeY, m_iTextureSizeZ);
        m_kTexture.setEnable(true);
        m_kTexture.setMinFilter(Texture.BASE_LEVEL_LINEAR);
        m_kTexture.setMagFilter(Texture.NICEST);

        // m_kTexture.setAnisotropicFilterMode(Texture.ANISOTROPIC_SINGLE_VALUE);
        // m_kTexture.setAnisotropicFilterDegree(5);
        m_kTexture.setBoundaryModeS(Texture.CLAMP_TO_BOUNDARY);
        m_kTexture.setBoundaryModeT(Texture.CLAMP_TO_BOUNDARY);
        m_kTexture.setBoundaryModeR(Texture.CLAMP_TO_BOUNDARY);
        m_kTexture.setBoundaryColor(0.0f, 0.0f, 0.0f, 0.0f);
        m_kTexture.setImage(0, m_kImageComponent);
        m_kTexture.setCapability(Texture3D.ALLOW_IMAGE_WRITE);
        m_kTexture.setCapability(Texture3D.ALLOW_IMAGE_READ);
        m_kTexture.setCapability(Texture3D.ALLOW_ENABLE_WRITE);
    }
}
