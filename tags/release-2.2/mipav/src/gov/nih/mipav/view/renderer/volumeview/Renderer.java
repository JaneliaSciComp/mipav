package gov.nih.mipav.view.renderer.volumeview;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.*;

import javax.vecmath.*;
import javax.media.j3d.*;

import java.awt.*;
import java.util.Arrays;


/**
 * A ray tracer for 3D images.  Either a parallel or perspective camera
 * model can be selected to form the rays.  In the parallel case, zooming is
 * accomplished by changing the size of the viewport.  In the perspective
 * case, zooming is accomplished by moving the eye point.  The line segment
 * of intersection (if it exists) of a ray with the bounding box of the image
 * is computed.  The image is trilinearly interpolated to allow subvoxel evaluations.
 *
 * The camera coordinate system has eye point is E = (0,0,z).  The direction
 * vector is D = (0,0,1), the up vector is U = (0,1,0), and the right vector
 * is R = (1,0,0).  Only the eye point is allowed to change.  Since the 3D
 * image can be arbitrarily rotated (via a virtual trackball), any portion of
 * the image can be viewed either close up or far away.
 *
 * The view plane has normal D and origin at E+n*D (n=near).  The view
 * frustum is orthogonal and has no far plane.  For a perspective camera, the
 * field of view is given as an angle A subtended at the eye point.  In
 * camera coordinates, the view port is the square [-e,e]^2 where
 * e = n*tan(A/2).  In world coordinates, the corners of the square are
 * E+n*D+s*e*U+t*e*R where |s| = |t| = 1 (four choices on sign).  For a
 * parallel camera, there is no field of view.
 *
 * The mapping between the viewport [-e,e]^2 and the B-by-B render image is
 * the following.  If (i,j) is a pixel in the image, then the corresponding
 * viewport point is (r,u) = (-e+2*e*i/(B-1),-e+2*e*j/(B-1)).
 */

public abstract class Renderer {

    /**
     * The constructor for the renderer.
     *
     * @param kImage the 3D image
     * @param iRBound the dimension of the square 2D renderer image
     * @param aiRImage The rendered image data stored in row-major order.
     *   Each integer pixel represents an RGB color in the format
     *   B | (G << 8) | (R << 16).
     */
    protected Renderer( ModelImage kImage, int iRBound, int[] aiRImage ) {

        // input volume
        m_kImage = kImage;
        m_iXBound = kImage.getExtents()[0];
        m_iYBound = kImage.getExtents()[1];
        m_iZBound = kImage.getExtents()[2];
        m_iXBoundM1 = m_iXBound - 1;
        m_iYBoundM1 = m_iYBound - 1;
        m_iZBoundM1 = m_iZBound - 1;
        m_iXBoundM2 = m_iXBound - 2;
        m_iYBoundM2 = m_iYBound - 2;
        m_iZBoundM2 = m_iZBound - 2;
        m_iXYProduct = m_iXBound * m_iYBound;
        m_iXYZProduct = m_iXYProduct * m_iZBound;

        // output image
        m_aiRImage = aiRImage;
        m_iRBound = iRBound;
        m_fRScaleX = 2.0f / ( m_iRBound - 1 );
        m_fRScaleY = 2.0f / ( m_iRBound - 1 );
        m_fFMult = 1.0f / ( m_iRBound - 1 );
        m_fGamma = 1.0f;

        // min/max of the bounds
        m_iMaxBound = m_iXBound;
        if ( m_iYBound > m_iMaxBound ) {
            m_iMaxBound = m_iYBound;
        }
        if ( m_iZBound > m_iMaxBound ) {
            m_iMaxBound = m_iZBound;
        }
        m_iMinBound = m_iXBound;
        if ( m_iYBound < m_iMinBound ) {
            m_iMinBound = m_iYBound;
        }
        if ( m_iZBound < m_iMinBound ) {
            m_iMinBound = m_iZBound;
        }

        // camera parameters
        m_bParallel = true;
        m_kBackgroundColor = Color.black;
        setEyeDist( -1.75f * m_iMaxBound );
        m_fNear = 1.0f * m_iMaxBound;
        m_fAngle = (float) ( Math.PI / 3.0 );
        if ( m_bParallel ) {
            m_fExtreme = 0.625f * m_iMaxBound;
        } else {
            m_fExtreme = 0.625f * m_fNear * (float) Math.tan( 0.5f * m_fAngle );
        }

        // initial zooming and panning
        m_fZoom = 1.0f;
        m_fXTrn = 0.0f;
        m_fYTrn = 0.0f;

        // for trackball rotation
        m_akAxis = new Vector3f[3];
        for ( int i = 0; i < 3; i++ ) {
            m_akAxis[i] = new Vector3f();
        }
        m_kRotate = new Matrix3f();
    }



    /**
     * Return indication as to whether or not the input image data has been
     * specified yet.
     * @return boolean True if the input image data has been specified.
     */
    public abstract boolean hasInputData();

    /*
     * set the m_bReloadInput member variable.
     * this is used to force the renderer to use new volume data.
     * @param bReload boolean bReload = true will cause the data to be
     * reloaded.
     */
    public void reloadInputData( boolean bReload )
    {
        m_bReloadInput = bReload;
    }

    /*
     *  Accessor that returns whether or not to reload the volume data.
     *  @return m_bReloadInput.
     */
    public boolean reloadInputData()
    {
        return m_bReloadInput;
    }
    /**
     * The x-dimension of the image.
     * @return the x-dimension
     */
    public int getXBound() {
        return m_iXBound;
    }

    /**
     * The y-dimension of the image.
     * @return the y-dimension
     */
    public int getYBound() {
        return m_iYBound;
    }

    /**
     * The z-dimension of the image.
     * @return the z-dimension
     */
    public int getZBound() {
        return m_iZBound;
    }

    /**
     * The minimum of the x-, y-, and z-dimensions of the image.
     * @return the minimuim dimension
     */
    public int getMinBound() {
        return m_iMinBound;
    }

    /**
     * The maximum of the x-, y-, and z-dimensions of the image.
     * @return the maximuim dimension
     */
    public int getMaxBound() {
        return m_iMaxBound;
    }

    /**
     * Set the background color for the rendered image.
     * @param color Color RGBA color to use for the image background.
     */
    public void setBackgroundColor( Color color ) {
        m_kBackgroundColor = color;
    }

    /**
     * Read the background color used for rendering images.
     * @return Color RGBA color in use for the image background.
     */
    public Color getBackgroundColor() {
        return m_kBackgroundColor;
    }

    /**
     * Specify the array of normal vectors for each voxel in the volume.
     * @param akNormal Vector3f[] Array of normal vectors.
     */
    public void setNormals( Vector3f[] akNormal ) {
        m_akNormal = akNormal;
    }

    /**
     * Return indication as to whether or not the normal vectors for each
     * voxel in the volume has been specified.
     * @return boolean True if such an array of normal vectors has been defined.
     */
    public boolean hasNormals() {
        return null != m_akNormal;
    }

    /**
     * Return indication as to whether or not the particular renderer uses
     * normal vectors as part of its implementation.
     * @return boolean True if the implementation uses normals.
     */
    public abstract boolean usesNormals();

    /**
     * Setup the specified set of lights to use for rendering.
     * @param kLightSet SoftwareLightSet Set of world/model lights.
     * @param kMaterial SoftwareMaterial Set of default material
     * properties to use when applying the lighting.
     */
    public void setLighting(SoftwareLightSet kLightSet, SoftwareMaterial kMaterial) {
        m_kLightSet = kLightSet;
        m_kMaterial = kMaterial;
    }

    /**
     * Change the z value of the eye point (0,0,z).  VolumeRenderer uses
     * this to zoom the image for a perspective camera.
     * @param fDist the new z component of the eye point
     */
    public void setEyeDist( float fDist ) {
        m_kEyeWorld.set( 0.0f, 0.0f, fDist );
    }

    /**
     * Read the z value of the eye point (0,0,z).  VolumeRenderer uses
     * this to zoom the image for a perspective camera.
     * @return the current z component of the eye point
     */
    public float getEyeDist() {
        return m_kEyeWorld.z;
    }

    /**
     * Return the world space coordinates for the eye point.
     * @return Point3f world space coordinates of the eye point.
     */
    public Point3f getEyePoint() {
        return m_kEyeWorld;
    }

    /**
     * Change the distance from the eye point to the view plane.  For a
     * perspective camera, the current field of view angle and the new
     * near value are used to compute the new extreme value.
     * @param fNear the new distance, a number N satisfying the constraint
     *   N > 0
     */
    public void setNear( float fNear ) {
        m_fNear = fNear;
        if ( !m_bParallel ) {
            m_fExtreme = m_fNear * (float) Math.tan( 0.5f * m_fAngle );
        }
    }

    /**
     * Read the distance from the eye point to the view plane.
     * @return the current distance
     */
    public float getNear() {
        return m_fNear;
    }

    /**
     * Change the field of view angle.  For a perspective camera, the
     * current near value and the new angle are used to compute the new
     * extreme value.
     * @param fAngle the new field of view angle, a number A satisfying
     *   the constraints 0 < A < pi
     */
    public void setAngle( float fAngle ) {
        m_fAngle = fAngle;
        if ( !m_bParallel ) {
            m_fExtreme = m_fNear * (float) Math.tan( 0.5f * m_fAngle );
        }
    }

    /**
     * Read the field of view angle.
     * @return m_fAngle the current field of view angle
     */
    public float getAngle() {
        return m_fAngle;
    }

    /**
     * Change the near distance and the field of view angle.  For a
     * perspective camera, the new values are used to compute the new
     * extreme value.
     *
     * @param fNear the new distance N, a number satisfying the constraint
     *   N > 0
     * @param fAngle the new field of view angle, a number A satisfying
     *   the constraints 0 < A < pi
     */
    public void setNearAndAngle( float fNear, float fAngle ) {
        m_fNear = fNear;
        m_fAngle = fAngle;
        if ( !m_bParallel ) {
            m_fExtreme = m_fNear * (float) Math.tan( 0.5f * m_fAngle );
        }
    }

    /**
     * Change the view port extreme value e.  VolumeRenderer uses this to
     * zoom the image for a parallel camera.  The method does not allow a
     * change to e for perspective cameras.  This should be done indirectly
     * via calls to setNear, setAngle, or setNearAndAngle.
     *
     * @param fExtreme the new view port extreme
     */
    public void setExtreme( float fExtreme ) {
        m_fExtreme = fExtreme;
    }

    /**
     * Read the view port extreme value e.  VolumeRenderer uses this to
     * zoom the image for a parallel camera.
     *
     * @return the current view port extreme
     */
    public float getExtreme() {
        return m_fExtreme;
    }

    /**
     * Change the camera model.
     *
     * @param bParallel true for a parallel camera, false for a perspective
     *   camera
     */
    public void setParallel( boolean bParallel ) {
        m_bParallel = bParallel;
        if ( m_bParallel ) {
            m_fExtreme = 0.625f * m_iMaxBound;
        } else {
            m_fExtreme = 0.625f * m_fNear * (float) Math.tan( 0.5f * m_fAngle );
        }
    }

    /**
     * Read the current camera model.
     *
     * @return true for a parallel camera, false for a perspective camera
     */
    public boolean getParallel() {
        return m_bParallel;
    }

    /**
     * Set the zoom factor.  The value must be positive.  The smaller the
     * value, the closer the volume data appears to the viewer.
     *
     * @param fZoom the zoom factor
     */
    public void setZoom( float fZoom ) {
        m_fZoom = fZoom;
    }

    /**
     * Get the zoom factor.
     *
     * @return the zoom factor
     */
    public float getZoom() {
        return m_fZoom;
    }

    /**
     * Set the gamma correction.  The value must be nonnegative.  A color C0
     * in [0,255] is normalized to C1 in [0,1], replaced by
     * C2 = pow(C1,gamma), then remapped to C3 in [0,255].  The calculations
     * are done in the construction of the final rendered image from the
     * computed color channels.
     *
     * @param fGamma the gamma correction
     */
    public void setGamma( float fGamma ) {
        m_fGamma = fGamma;
    }

    /**
     * Get the gamma correction.
     *
     * @return the gamma correction
     */
    public float getGamma() {
        return m_fGamma;
    }

    /**
     * Set the x translation.  The (x,y) vector is used to translate the
     * cener of the rendered image on the screen.
     *
     * @param fXTrn the x translation
     */
    public void setXTranslate( float fXTrn ) {
        m_fXTrn = fXTrn;
    }

    /**
     * Get the x translation.
     *
     * @return the x translation
     */
    public float getXTranslate() {
        return m_fXTrn;
    }

    /**
     * Set the y translation.  The (x,y) vector is used to translate the
     * cener of the rendered image on the screen.
     *
     * @param fYTrn the y translation
     */
    public void setYTranslate( float fYTrn ) {
        m_fYTrn = fYTrn;
    }

    /**
     * Get the y translation.
     *
     * @return the y translation
     */
    public float getYTranslate() {
        return m_fYTrn;
    }

    /**
     * Setup the X Negative clipping plane position.
     * @param value   position of the X negative clip slider.
     */
    public abstract void setXBoundNeg( float value );

    /**
     * Setup the Y Negative clipping plane position.
     * @param value  position of the Y negative clip slider.
     */
    public abstract void setYBoundNeg( float value );

    /**
     * Setup the Z negative clipping plane position.
     * @param value  position of the Z negative clip slider.
     */
    public abstract void setZBoundNeg( float value );

    /**
     * Setup the X positive clipping plane position
     * @param value  position of the X positive clip slider.
     */
    public abstract void setXBoundPos( float value );

    /**
     * Setup the Y positive clipping plane position.
     * @param value   positin of the Y positve clip slider.
     */
    public abstract void setYBoundPos( float value );

    /**
     * Setup the Z positive clipping plane position.
     * @param value  position of the Z positive clip slider.
     */
    public abstract void setZBoundPos( float value );

    /**
     * In order to map line integrals of image intensity to RGB colors where
     * each color channel is 8 bits, it is necessary to make sure that the
     * integrals are in [0,255].  Producing a theoretical maximum value of
     * a line integral is not tractable in an application.  This method
     * constructs an approximate maximum by integrating along each line of
     * voxels in the image with line directions parallel to the coordinate
     * axes.  The 'processRay' call adjusts the line integrals using the
     * estimate, but still clamps the integrals to 255 since the estimate
     * might not be the true maximum.
     *
     * @param acImage byte[] Input volume to use in computing the integral
     * normalization factor.
     * @return float Integral normalization factor.
     */
    protected float computeIntegralNormalizationFactor( byte[] acImage ) {

        // compute image normalization factor
        int iX, iY, iZ, iBase, iSteps;
        float fMaxIntegral = 0.0f;
        float fTStep, fIntegral;

        // fix y and z, integrate over x
        for ( iY = 0; iY < m_iYBound; iY++ ) {
            for ( iZ = 0; iZ < m_iZBound; iZ++ ) {
                iBase = m_iXBound * ( iY + m_iYBound * iZ );
                iSteps = m_iXBound - 1;
                fIntegral = 0.5f * ( ( acImage[iBase] & 0x0ff ) + ( acImage[iBase + iSteps] & 0x0ff ) );
                fTStep = 1.0f / iSteps;
                for ( iX = 1; iX < iSteps; iX++ ) {
                    fIntegral += ( acImage[iBase + iX] & 0x0ff );
                }
                fIntegral *= fTStep;
                if ( fIntegral > fMaxIntegral ) {
                    fMaxIntegral = fIntegral;
                }
            }
        }

        // fix x and z, integrate over y
        for ( iX = 0; iX < m_iXBound; iX++ ) {
            for ( iZ = 0; iZ < m_iZBound; iZ++ ) {
                iBase = iX + m_iXYProduct * iZ;
                iSteps = m_iYBound - 1;
                fIntegral = 0.5f * ( ( acImage[iBase] & 0x0ff ) + ( acImage[iBase + m_iXBound * iSteps] & 0x0ff ) );
                fTStep = 1.0f / iSteps;
                for ( iY = 1; iY < iSteps; iY++ ) {
                    fIntegral += ( acImage[iBase + m_iXBound * iY] & 0x0ff );
                }
                fIntegral *= fTStep;
                if ( fIntegral > fMaxIntegral ) {
                    fMaxIntegral = fIntegral;
                }
            }
        }

        // fix x and y, integrate over z
        for ( iX = 0; iX < m_iXBound; iX++ ) {
            for ( iY = 0; iY < m_iYBound; iY++ ) {
                iBase = iX + m_iXBound * iY;
                iSteps = m_iZBound - 1;
                fIntegral = 0.5f * ( ( acImage[iBase] & 0x0ff ) + ( acImage[iBase + m_iXYProduct * iSteps] & 0x0ff ) );
                fTStep = 1.0f / iSteps;
                for ( iZ = 1; iZ < iSteps; iZ++ ) {
                    fIntegral += ( acImage[iBase + m_iXYProduct * iZ] & 0x0ff );
                }
                fIntegral *= fTStep;
                if ( fIntegral > fMaxIntegral ) {
                    fMaxIntegral = fIntegral;
                }
            }
        }

        return ( fMaxIntegral > 0.0f ) ? ( 1.0f / fMaxIntegral ) : 0.00f;
    }

    /**
     * Called at the beginning of the trace methods
     */
    protected void traceInit() {

        // initialize image to desired background color
        Arrays.fill( m_aiRImage, m_kBackgroundColor.getRGB() );

        // compute the location of the eye in world and model coordinates
        convertWorldToModel( m_kEyeWorld, m_kEyeModel );
    }

    /**
     * Convert coordinates from world space to model space.
     * @param kWorld Tuple3f input world space coordinates
     * @param kModel Tuple3f output world space coordinates
     */
    protected final void convertWorldToModel( Tuple3f kWorld, Tuple3f kModel ) {
        m_kVgen.x = kWorld.x;
        m_kVgen.y = kWorld.y;
        m_kVgen.z = kWorld.z;

        kModel.set( m_kVgen.dot( m_akAxis[0] ), m_kVgen.dot( m_akAxis[1] ), m_kVgen.dot( m_akAxis[2] ) );
    }

    /**
     * The axis vectors for the oriented bounding box are rotated in place.
     * After many rotations, numerical errors can cause the axes to be
     * signicantly skewed so that they are no longer a good approximation
     * to a right-handed orthonormal coordinate system.  This method uses
     * Gram-Schmidt orthonormalization to avoid the accumulative errors and
     * is called after each rotation is applied to the axes.
     */
    protected static void orthonormalize( Vector3f[] akVector ) {
        // If the input vectors are v0, v1, and v2, then the Gram-Schmidt
        // orthonormalization produces vectors u0, u1, and u2 as follows,
        //
        // u0 = v0/|v0|
        // u1 = (v1-(u0*v1)u0)/|v1-(u0*v1)u0|
        // u2 = (v2-(u0*v2)u0-(u1*v2)u1)/|v2-(u0*v2)u0-(u1*v2)u1|
        //
        // where |A| indicates length of vector A and A*B indicates dot
        // product of vectors A and B.

        // compute u0
        akVector[0].normalize();

        // compute u1
        float fDot0 = akVector[0].dot( akVector[1] );

        akVector[1].scaleAdd( -fDot0, akVector[0], akVector[1] );
        akVector[1].normalize();

        // compute u2
        float fDot1 = akVector[1].dot( akVector[2] );

        fDot0 = akVector[0].dot( akVector[2] );
        akVector[2].scaleAdd( -fDot0, akVector[0], akVector[2] );
        akVector[2].scaleAdd( -fDot1, akVector[1], akVector[2] );
        akVector[2].normalize();
    }



    /**
     *  Calls dispose
     */
    protected void finalize() throws Throwable {
       disposeLocal();
       super.finalize();
    }

    /**
     * Set the texture material shininess value.
     * @param value   shininess value.
     */
    public void setMaterialShininess(float value) {
        m_kMaterial.shininess = value;
    }

    /**
     *   Disposes of image memory and associated objects.
     */
    public void disposeLocal() {
       System.out.println(" Renderer.disposeLocal");
       m_kImage = null;
       m_akNormal = null;
       m_kLightSet = null;
       m_kMaterial = null;
       m_aiRImage = null;
       m_kEyeWorld = null;
       m_kEyeModel = null;

       m_akAxis[0] = null;
       m_akAxis[1] = null;
       m_akAxis[2] = null;
       m_akAxis = null;
       m_kRotate = null;
       m_kVgen = null;
    }


    public abstract void rotateBy( AxisAngle4f kAxisAngle );
    public abstract void rotateFrameBy( Transform3D transform );

    public abstract Vector3f getAxis( int i );

    /** General purpose Vectore3f - allocated once and reused */
    private Vector3f m_kVgen = new Vector3f();

    /** Input volume */
    protected ModelImage m_kImage;
    protected int m_iXBound, m_iYBound, m_iZBound;
    protected int m_iXBoundM1, m_iYBoundM1, m_iZBoundM1;
    protected int m_iXBoundM2, m_iYBoundM2, m_iZBoundM2;
    protected int m_iXYProduct, m_iXYZProduct;
    protected int m_iMinBound, m_iMaxBound;

    // unit-length normal vectors at the voxels
    protected Vector3f[] m_akNormal = null;

    // used for lighting-based rendering
    protected SoftwareLightSet m_kLightSet = new SoftwareLightSet();
    protected SoftwareMaterial m_kMaterial = new SoftwareMaterial();

    /** Left-handed camera coordinate system that is initially set up to view
       the z=0 slice of the 3D image.
       E = (0,0,eye), initially eye < 0
       D = (0,0,1) = direction
       U = (0,1,0) = up
       R = (1,0,0) = right.

      The camera can be parallel or perspective.

      The view plane has normal D and origin at E+n*D (n=near).  The view
      frustum is orthogonal and has no far plane.  The field of view is
      given as an angle A subtended at the eye point.  The view port is the
      square [-e,e]^2 where e = n*tan(A/2). */
    protected boolean m_bParallel;
    protected float m_fNear, m_fAngle, m_fExtreme;

    /** eye point in world and model space */
    protected Point3f m_kEyeWorld = new Point3f();
    protected Point3f m_kEyeModel = new Point3f();


    /** Final rendered image (24-bit RGB stored in int). 2D: m_iRBound-by-m_iRBound image */
    protected int[] m_aiRImage;
    protected int m_iRBound;
    protected float m_fRScaleX, m_fRScaleY;
    protected float m_fGamma, m_fFMult;

    /** Background color of rendered image */
    protected Color m_kBackgroundColor;

    /** For zooming and panning */
    protected float m_fZoom, m_fXTrn, m_fYTrn;

    /** For track ball rotation */
    protected Vector3f[/* 3*/] m_akAxis;
    protected Matrix3f m_kRotate;

    /* For indicating that the volume input data should be reloaded */
    boolean m_bReloadInput = false;
}
