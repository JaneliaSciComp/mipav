package gov.nih.mipav.view.renderer.volumeview;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.ViewJFrameVolumeView;

import javax.vecmath.*;
import javax.media.j3d.*;

import java.awt.*;

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

public abstract class RayCastRenderer extends Renderer {

    /**
     * The constructor for the ray tracer.  Currently, the only client of
     * this class is VolumeRenderer.
     *
     * @param kImage the 3D image
     * @param iRBound the dimension of the square 2D renderer image
     * @param aiRImage The rendered image data stored in row-major order.
     *   Each integer pixel represents an RGB color in the format
     *   B | (G << 8) | (R << 16).
     */
    protected RayCastRenderer( ModelImage kImage, int iRBound, int[] aiRImage ) {
        super( kImage, iRBound, aiRImage );

        // bounding box of image
        m_kExtent = new Vector3f();

        float fXDelta = kImage.getFileInfo( 0 ).getResolutions()[0];
        float fYDelta = kImage.getFileInfo( 0 ).getResolutions()[1];
        float fZDelta = kImage.getFileInfo( 0 ).getResolutions()[2];

        if ( ( fXDelta <= 0.0f ) || ( fYDelta <= 0.0f ) || ( fZDelta <= 0.0f ) ) {
            fXDelta = 1.0f;
            fYDelta = 1.0f;
            fZDelta = 1.0f;
        }

        float minRes = fXDelta;

        if ( fYDelta < minRes ) {
            minRes = fYDelta;
        }
        if ( fZDelta < minRes ) {
            minRes = fZDelta;
        }

        fXDelta = fXDelta / minRes;
        fYDelta = fYDelta / minRes;
        fZDelta = fZDelta / minRes;

        m_kExtent.x = 0.5f * fXDelta * ( m_iXBound - 1 );
        m_kExtent.y = 0.5f * fYDelta * ( m_iYBound - 1 );
        m_kExtent.z = 0.5f * fZDelta * ( m_iZBound - 1 );

        this.fXDelta = fXDelta;
        this.fYDelta = fYDelta;
        this.fZDelta = fZDelta;
        m_akAxis[0].set( -1.0f, 0.0f, 0.0f );
        m_akAxis[1].set( 0.0f, 1.0f, 0.0f );
        m_akAxis[2].set( 0.0f, 0.0f, 1.0f );

        m_fInvXDelta = 1.0f / fXDelta;
        m_fInvYDelta = 1.0f / fYDelta;
        m_fInvZDelta = 1.0f / fZDelta;

        // integration parameters
        Vector3f tmpExtents = new Vector3f();

        tmpExtents.x = m_iXBound - 1;
        tmpExtents.y = m_iYBound - 1;
        tmpExtents.z = m_iZBound - 1;

        // allocation of class objects
        m_kV = new Vector3f();
        m_kWOrig = new Vector3f();
        m_kWDir = new Vector3f();
        m_kMOrig = new Point3f();
        m_kMDir = new Vector3f();
        m_kP0 = new Point3f();
        m_kP1 = new Point3f();
        m_kPDiff = new Vector3f();
        m_kP = new Point3f();
        m_kRotate = new Matrix3f();

        tempImage = new int[m_aiRImage.length];
    }

    /**
     * Setup the X Negative clipping plane position.
     * @param value   position of the X negative clip slider.
     */
    public void setXBoundNeg( float value ) {
        clipRegionXNeg = m_kExtent.x * 2 - ( 1.0f - value ) * fXDelta * m_iXBoundM1;
    }

    /**
     * Setup the Y Negative clipping plane position.
     * @param value  position of the Y negative clip slider.
     */
    public void setYBoundNeg( float value ) {
        clipRegionYNeg = m_kExtent.y * 2 - ( 1.0f - value ) * fYDelta * m_iYBoundM1;
    }

    /**
     * Setup the Z negative clipping plane position.
     * @param value  position of the Z negative clip slider.
     */
    public void setZBoundNeg( float value ) {
        clipRegionZNeg = m_kExtent.z * 2 - ( 1.0f - value ) * fZDelta * m_iZBoundM1;
    }

    /**
     * Setup the X positive clipping plane position
     * @param value  position of the X positive clip slider.
     */
    public void setXBoundPos( float value ) {
        clipRegionXPos = m_kExtent.x * 2 - value * fXDelta * m_iXBoundM1;

    }

    /**
     * Setup the Y positive clipping plane position.
     * @param value   positin of the Y positve clip slider.
     */
    public void setYBoundPos( float value ) {
        clipRegionYPos = m_kExtent.y * 2 - value * fYDelta * m_iYBoundM1;
    }

    /**
     * Setup the Z positive clipping plane position.
     * @param value  position of the Z positive clip slider.
     */
    public void setZBoundPos( float value ) {
        clipRegionZPos = m_kExtent.z * 2 - value * fZDelta * m_iZBoundM1;
    }

    /**
     * Add here.
     *
     * @param
     */
    public void setOpacityFunctions( ModelLUT opacityFunctionA, ModelLUT opacityFunctionB ) {
        int j;

        opacityFunctA = opacityFunctionA;
        opacityFunctB = opacityFunctionB;
        int nPtsLUTFun;

        nPtsLUTFun = opacityFunctionA.getTransferFunction().size();
        for ( j = 0; j < nPtsLUTFun; j++ ) {
            xLUTa[j] = ( (Point2Df) ( opacityFunctionA.getTransferFunction().getPoint( j ) ) ).x;
            yLUTa[j] = 100 - ( (Point2Df) ( opacityFunctionA.getTransferFunction().getPoint( j ) ) ).y;
            // System.out.println("xLUT = " +  xLUTa[j] + " yLUT = " +  yLUTa[j]);
        }

        if ( opacityFunctionB != null ) {
            nPtsLUTFun = opacityFunctionB.getTransferFunction().size();
            for ( j = 0; j < nPtsLUTFun; j++ ) {
                xLUTb[j] = ( (Point2Df) ( opacityFunctionB.getTransferFunction().getPoint( j ) ) ).x;
                yLUTb[j] = 100 - ( (Point2Df) ( opacityFunctionB.getTransferFunction().getPoint( j ) ) ).y;
            }
        }
    }

    /**
     * Change an axis of the oriented bounding box of the 3D image.
     *
     * @param i the axis index (0, 1, or 2)
     * @param kAxis the new axis vector at index i
     */
    public void setAxis( int i, Vector3f kAxis ) {
        m_akAxis[i].set( kAxis );
    }

    /**
     * Read the current axis at index i.
     *
     * @param i the axis index (0, 1, or 2)
     * @return the current axis vector at index i
     */
    public Vector3f getAxis( int i ) {
        return m_akAxis[i];
    }

    /**
     * Rotate the oriented bounding box of the 3D image about the specified
     * axis with the specified angle.
     *
     * @param kAxisAngle the axis and angle formulation for the rotation
     */
    public synchronized void rotateBy( AxisAngle4f kAxisAngle ) {
        m_kRotate.set( kAxisAngle );
        for ( int i = 0; i < 3; i++ ) {
            m_kRotate.transform( m_akAxis[i] );
        }
        orthonormalize( m_akAxis );
    }

    /**
     * Rotate the oriented bounding box of the 3D image about the specified
     * axis with the specified angle.
     *
     * @param kAxisAngle the axis and angle formulation for the rotation
     */
    public synchronized void rotateFrameBy( Transform3D transform ) {
        Matrix3f matrix = new Matrix3f();

        transform.get( matrix );

        m_kRotate.set( matrix );

        m_akAxis[0] = new Vector3f( -1.0f, 0.0f, 0.0f );
        m_akAxis[1] = new Vector3f( 0.0f, 1.0f, 0.0f );
        m_akAxis[2] = new Vector3f( 0.0f, 0.0f, 1.0f );
        for ( int i = 0; i < 3; i++ ) {
            m_kRotate.transform( m_akAxis[i] );
        }
        orthonormalize( m_akAxis );
    }

    /**
     * Ray trace the 3D image in its current orientation as determined by
     * the oriented bounding box.  The rendered image is initialized to the
     * background color.  A pixel is overwritten only when a ray intersects
     * the bounding box of the image, that pixel corresponding to the given
     * ray.
     *
     * @param iSpacing  A positive value indicating how often to sample the
     *   rendered image.  The typical value is 1 indicating that all rays
     *   corresponding to all pixels should be processed.  If the value is
     *   2, rays corresponding to pixels (x,y) with (x mod 2) = 0 and
     *   (y mod 2) = 0 are processed.  The other pixels are assigned values
     *   of the modulo 2 neighbor pixel.  Similar reduced sampling occurs
     *   with larger values of iSpacing.  VolumeRenderer sets the spacing
     *   to be 2 when the image is being rotated by the virtual track ball.
     *   Once the image is finished rotating, a final trace is made with a
     *   spacing of 1.
     */


    public final synchronized void trace( int rayTraceStepSize, int iSpacing ) {
        long startTime = 0, now = 0;
        double elapsedTime = 0d;
        int length = m_iRBound * m_iRBound;
        int iY;
        int mod;

        startTime = System.currentTimeMillis();
        traceInit();

        mod = m_iRBound / 10;
        for ( iY = 0; iY < m_iRBound; iY += iSpacing ) {
            if ( ( iY ) % mod == 0 || iY == m_iRBound - 1 ) {
                if ( iY == m_iRBound - 1 ) {
                    ViewJFrameVolumeView.getRendererProgressBar().setValue( 100 );
                } else {
                    ViewJFrameVolumeView.getRendererProgressBar().setValue( Math.round( (float) ( iY ) / ( m_iRBound - 1 ) * 100 ) );
                    ViewJFrameVolumeView.getRendererProgressBar().update( ViewJFrameVolumeView.getRendererProgressBar().getGraphics() );
                }
            }
            float fU = m_fExtreme * ( m_fRScaleY * iY - 1.0f );
            int iY_renBound = m_iRBound * iY;

            for ( int iX = 0; iX < m_iRBound; iX += iSpacing ) {
                float fR = m_fExtreme * ( m_fRScaleX * iX - 1.0f );

                m_kV.set( -fR, fU, m_fNear ); // = n*D+r*R+u*U

                // Compute world ray.  It is E+V+t*D for parallel drawing,
                // E+V+t*V for perspective drawing, t >= 0.
                m_kWOrig.add( m_kEyeWorld, m_kV );
                if ( m_bParallel ) {
                    m_kWDir.set( 0.0f, 0.0f, 1.0f );
                } else {
                    m_kWDir.set( m_kV );
                }

                // convert world ray to box (model) coordinates
                convertWorldToModel( m_kWOrig, m_kMOrig );
                convertWorldToModel( m_kWDir, m_kMDir );

                // Clip world ray against box.  The intersection points, if
                // any, are stored in m_kP0 and m_kP1 in image coordinates.
                if ( intersectsBox() ) {
                    // undo the anisotropy
                    m_kP0.x *= m_fInvXDelta;
                    m_kP0.y *= m_fInvYDelta;
                    m_kP0.z *= m_fInvZDelta;
                    m_kP1.x *= m_fInvXDelta;
                    m_kP1.y *= m_fInvYDelta;
                    m_kP1.z *= m_fInvZDelta;

                    int iIndex = iX + iY_renBound;

                    processRay( iIndex, rayTraceStepSize );

                    if ( iSpacing > 1 ) {
                        for ( int iDY = 0; iDY < iSpacing; iDY++ ) {
                            int tmp = iIndex + m_iRBound * iDY;

                            for ( int iDX = 0; iDX < iSpacing; iDX++ ) {
                                int iDIndex = tmp + iDX;

                                if ( iDIndex < length ) {
                                    m_aiRImage[iDIndex] = m_aiRImage[iIndex];
                                }
                            }
                        }
                    }
                }
            }
        }
        if ( bluring ) {
          // Blur the final 2D image
          int vCenter, vLeft, vRight, vTop, vBottom;
          float fSrcR, fSrcG, fSrcB;
          float fTrgR, fTrgG, fTrgB;
          int iIndex;
          for (iY = 1; iY < m_iRBound - 1; iY++) {
            int iY_renBound = m_iRBound * iY;
            for (int iX = 1; iX < m_iRBound - 1; iX++) {
              iIndex = iX + iY_renBound;
              vCenter = m_aiRImage[iIndex];
              vLeft = m_aiRImage[iIndex - 1];
              vRight = m_aiRImage[iIndex + 1];
              vTop = m_aiRImage[iX + (m_iRBound * (iY - 1))];
              vBottom = m_aiRImage[iX + (m_iRBound * (iY + 1))];

              // composite the color values
              // vCenter
              fSrcR = ( (vCenter >> 16) & 0xff);
              fSrcG = ( (vCenter >> 8) & 0xff);
              fSrcB = ( (vCenter) & 0xff);

              fTrgR = 0;
              fTrgG = 0;
              fTrgB = 0;

              fTrgR += fSrcR;
              fTrgG += fSrcG;
              fTrgB += fSrcB;

              // vLeft
              fSrcR = ( (vLeft >> 16) & 0xff);
              fSrcG = ( (vLeft >> 8) & 0xff);
              fSrcB = ( (vLeft) & 0xff);

              fTrgR += fSrcR;
              fTrgG += fSrcG;
              fTrgB += fSrcB;

              // vRight
              fSrcR = ( (vRight >> 16) & 0xff);
              fSrcG = ( (vRight >> 8) & 0xff);
              fSrcB = ( (vRight) & 0xff);

              fTrgR += fSrcR;
              fTrgG += fSrcG;
              fTrgB += fSrcB;

              // vTop
              fSrcR = ( (vTop >> 16) & 0xff);
              fSrcG = ( (vTop >> 8) & 0xff);
              fSrcB = ( (vTop) & 0xff);

              fTrgR += fSrcR;
              fTrgG += fSrcG;
              fTrgB += fSrcB;

              // vBottom
              fSrcR = ( (vBottom >> 16) & 0xff);
              fSrcG = ( (vBottom >> 8) & 0xff);
              fSrcB = ( (vBottom) & 0xff);

              fTrgR += fSrcR;
              fTrgG += fSrcG;
              fTrgB += fSrcB;

              fTrgR /= 5;
              fTrgG /= 5;
              fTrgB /= 5;

              tempImage[iIndex] =
                  //(((int)255   & 0xff) << 24) |
                  ( ( (int) (fTrgR) & 0xff) << 16) |
                  ( ( (int) (fTrgG) & 0xff) << 8) |
                  ( ( (int) (fTrgB) & 0xff));

            }
          }
          for (int i = 0; i < tempImage.length; i++) {
            m_aiRImage[i] = tempImage[i];
          }
        }
        now = System.currentTimeMillis();
        elapsedTime = (double) ( now - startTime );
        if ( elapsedTime <= 0 ) {
            elapsedTime = (double) 0.0;
        }
        //System.out.println( "Raycast elapse time = " + (double) ( elapsedTime / 1000.0 ) ); // in seconds
        Preferences.debug( "Raycast elapse time = " + (double) ( elapsedTime / 1000.0 ) + "\n" );
    }

    /**
     * Blur the resulting image or not
     * @param flag  true blur the image, false not blur.
     */
    public void setBlurFlag( boolean flag ) {
      bluring = flag;
    }

    /**
     * Set vertex material diffuse color
     * @param flag  true blur the image, false not blur.
     */
    public void setDiffuse( Color color ) {
      vertexDiffuse = new Color3f( color );
    }

    /**
     * Set vertex material specular color
     * @param flag  true blur the image, false not blur.
     */
    public void setSpecular(Color color) {
      vertexSpecular = new Color3f( color );
    }

    /**
     * Support for clipping line segments against coordinate planes.
     * The function computes the line segment parameter corresponding to the
     * intersection of the segment with a coordinate plane.  However, the
     * expensive division is performed only if in fact there is an
     * intersection.  The deferred division speeds up the ray tracing a lot
     * since this method is called up to 6 times per ray.
     *
     * @param fDen the denominator of the line segment parameter of the
     *   intersection
     * @param fNum the numerator of the line segment parameter of the
     *   intersection
     * @return true if the entire line segment is entirely clipped by the
     *   current plane (segment is "outside" the plane), false if the segment
     *   intersects the plane or is contained "inside" the plane.
     */
    private final boolean clipped( float fDen, float fNum ) {
        if ( fDen > 0.0f ) {
            if ( fNum > fDen * m_fT1 ) {
                return true;
            }
            if ( fNum > fDen * m_fT0 ) {
                m_fT0 = fNum / fDen;
            }
            return false;
        } else if ( fDen < 0.0f ) {
            if ( fNum > fDen * m_fT0 ) {
                return true;
            }
            if ( fNum > fDen * m_fT1 ) {
                m_fT1 = fNum / fDen;
            }
            return false;
        } else {
            return fNum > 0.0f;
        }
    }

    /**
     * Clip the current ray against the oriented bounding box of the 3D
     * image.  The trace method sets up the ray in world coordinates.  The
     * intersector converts the ray to box coordinates so that the ray is
     * clipped against an axis-aligned box.
     *
     * The end points of the intersecting line segment are stored in class
     * members m_kP0 and m_kP1 as image coordinates.  The bounding box is
     * centered at the origin.  The image coordinates are just a translation
     * of box coordinates so that the center of the box becomes the center
     * of the image.  The class members m_fT0 and m_fT1 are the corresponding
     * line segment parameters.  They are class members only because Java
     * does not support call-by-reference semantics.  In another language,
     * the parameters can be local and passed to 'clipped' with the potential
     * to be changed by 'clipped'.
     *
     * @return true if and only if the ray intersects the box
     */
    private final boolean intersectsBox() {
        // clip the ray to the box
        m_fT0 = 0.0f;
        m_fT1 = Float.POSITIVE_INFINITY;

        if ( clipped( +m_kMDir.x, -m_kMOrig.x - m_kExtent.x + clipRegionXNeg ) ) {
            return false;
        }

        if ( clipped( -m_kMDir.x, +m_kMOrig.x - m_kExtent.x + clipRegionXPos ) ) {
            return false;
        }

        if ( clipped( +m_kMDir.y, -m_kMOrig.y - m_kExtent.y + clipRegionYNeg ) ) {
            return false;
        }

        if ( clipped( -m_kMDir.y, +m_kMOrig.y - m_kExtent.y + clipRegionYPos ) ) {
            return false;
        }

        if ( clipped( +m_kMDir.z, -m_kMOrig.z - m_kExtent.z + clipRegionZNeg ) ) {
            return false;
        }

        if ( clipped( -m_kMDir.z, +m_kMOrig.z - m_kExtent.z + clipRegionZPos ) ) {
            return false;
        }

        if ( m_fT0 == 0.0f && m_fT1 == Float.POSITIVE_INFINITY ) {
            return false;
        }

        // The ray and box intersect.  Return *image* coordinates since the
        // image interpolation requires these.
        m_kMOrig.add( m_kExtent );
        m_kP0.scaleAdd( m_fT0, m_kMDir, m_kMOrig );
        m_kP1.scaleAdd( m_fT1, m_kMDir, m_kMOrig );
        return true;
    }

    /**
     * Trilinear interpolation of the image to produce image values at
     * points P that are not at the integer voxel locations.
     *
     * @param kP the spatial point to be assigned an interpolated value
     * @param acImage The array of byte values to be interpolated
     * @return The interpolated value for the point.  If kP is not within the
     *   bounding box of the image, a value of -1 is returned.
     */
    protected final int interpolate( Point3f kP, byte[] acImage ) {
        int iX = (int) kP.x;

        if ( iX < 0 || iX > m_iXBoundM2 ) {
            return -1;
        }

        int iY = (int) kP.y;

        if ( iY < 0 || iY > m_iYBoundM2 ) {
            return -1;
        }

        int iZ = (int) kP.z;

        if ( iZ < 0 || iZ > m_iZBoundM2 ) {
            return -1;
        }

        int i000 = iX + m_iXBound * iY + m_iXYProduct * iZ;
        int i100 = i000 + 1;
        int i010 = i000 + m_iXBound;
        int i110 = i100 + m_iXBound;
        int i001 = i000 + m_iXYProduct;
        int i101 = i100 + m_iXYProduct;
        int i011 = i010 + m_iXYProduct;
        int i111 = i110 + m_iXYProduct;
        float fF000 = ( acImage[i000] & 0x0ff );
        float fF100 = ( acImage[i100] & 0x0ff );
        float fF010 = ( acImage[i010] & 0x0ff );
        float fF110 = ( acImage[i110] & 0x0ff );
        float fF001 = ( acImage[i001] & 0x0ff );
        float fF101 = ( acImage[i101] & 0x0ff );
        float fF011 = ( acImage[i011] & 0x0ff );
        float fF111 = ( acImage[i111] & 0x0ff );

        float fDX = kP.x - iX, fDY = kP.y - iY, fDZ = kP.z - iZ;
        float fOmDX = 1.0f - fDX, fOmDY = 1.0f - fDY, fOmDZ = 1.0f - fDZ;

        float fInterp = fOmDZ * ( fOmDY * ( fOmDX * fF000 + fDX * fF100 ) + fDY * ( fOmDX * fF010 + fDX * fF110 ) )
                + fDZ * ( fOmDY * ( fOmDX * fF001 + fDX * fF101 ) + fDY * ( fOmDX * fF011 + fDX * fF111 ) );

        return (int) ( fInterp + 0.5f );
    }

    /**
     * Process a ray that has intersected the oriented bounding box of the
     * 3D image.  The method is only called if there is a line segment of
     * intersection.  The 'intersectsBox' stores the end points of the line
     * segment in the class members m_kP0 and m_kP1 in image coordinates.
     *
     * The function sets the color of the pixel corresponding to the
     * processed ray.  The RGB value is stored as an integer in the format
     * B | (G << 8) | (R << 16).  This method always returns a gray scale
     * value (B = G = R).  However, the function can be overridden in a
     * subclass to produce other rendering effects.  For example, the color
     * can be set to a non-gray value if the ray intersects a level region
     * bounded by a level surface.  See SurfaceRayTrace.java for an
     * example.
     *
     * @param the index of the pixel corresponding to the processed ray
     */
    protected abstract void processRay( int iIndex, int rayTraceStepSize );

    /**
     *  Calls dispose
     */
    protected void finalize()
        throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     *  Clean memory.
     *  @param flag is true call the super.disposeLocal
     */
    public void disposeLocal(boolean flag) {
        //System.out.println(" RayCastRenderer.disposeLocal");
        xLUTa = null;
        yLUTa = null;
        xLUTb = null;
        yLUTb = null;
        if ( opacityFunctA != null ) {
            opacityFunctA.disposeLocal();
            opacityFunctA = null;
        }
        if ( opacityFunctB != null ) {
            opacityFunctB.disposeLocal();
            opacityFunctB = null;
        }

        m_kV = null;
        m_kExtent = null;
        m_kWOrig = null;
        m_kWDir = null;
        m_kMOrig = null;
        m_kMDir = null;
        m_kP0 = null;
        m_kP1 = null;

        m_kPDiff = null;

        if (flag == true)
           super.disposeLocal();
    }

    /** x, y, z delta value */
    float fXDelta, fYDelta, fZDelta;

    /** The clip plane position in the box. */
    float clipRegionXNeg = 0.0f, clipRegionXPos = 0.0f;
    float clipRegionYNeg = 0.0f, clipRegionYPos = 0.0f;
    float clipRegionZNeg = 0.0f, clipRegionZPos = 0.0f;

    /** Used to store the transfer function points for remapping to the LUT */
    protected float[] xLUTa = new float[25];
    protected float[] yLUTa = new float[25];

    protected float[] xLUTb = new float[25];
    protected float[] yLUTb = new float[25];
    protected ModelLUT opacityFunctA;
    protected ModelLUT opacityFunctB;

    /** The inverse delta values are for scaling back to original coordinates
        for correct interpolation of the image. */
    protected float m_fInvXDelta, m_fInvYDelta, m_fInvZDelta;

    /** Oriented bounding box of image, centered at (0,0,0), axes are stored
        as the columns of a rotation matrix, and half-widths are specified. */
    protected Vector3f m_kExtent;

    /** Intersection of ray with view plane */
    protected Vector3f m_kV;

    /** Ray in world coordinates */
    protected Vector3f m_kWOrig;
    protected Vector3f m_kWDir;

    /** Ray in box (model) coordinates */
    protected Point3f m_kMOrig;
    protected Vector3f m_kMDir;

    /** the parameters for clipped rays */
    private float m_fT0, m_fT1;

    /** the intersection points of ray with box */
    protected Point3f m_kP0, m_kP1;
    protected Vector3f m_kPDiff;

    /** temporary variables to avoid 'new' call */
    protected Point3f m_kP;
    protected float red, green, blue;
    private int[] tempImage;

    /** Blur the final image to reduce voxel contrast. */
    private boolean bluring = false;

    /** Vertex material diffuse color. */
    protected Color3f vertexDiffuse = new Color3f( Color.white );

    /** Vertex material specular color. */
    protected Color3f vertexSpecular = new Color3f( Color.white );

}
