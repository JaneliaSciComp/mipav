package gov.nih.mipav.view.renderer;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.surfaceview.rfaview.mouse.*;

import com.sun.j3d.utils.universe.*;
import com.sun.j3d.utils.geometry.*;

import javax.media.j3d.*;
import javax.vecmath.*;

import java.awt.image.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.io.*;


/*
 *  Class PlaneRender: renders a single dimension of the ModelImage data as a
 *  texture-mapped polygon.
 *
 *  The PlaneRender class keeps track of whether it is rendering the Axial,
 *  Sagital, or Coronal view of the data. Depending on which view is rendered,
 *  and on the ModelImage Orientation, the ModelImage data is transformed to a
 *  standard x,y,z space that is then operated on in the same manner for each
 *  view, Axial, Sagital, or Coronal. The mapping of ModelImage x,y,z to
 *  rendered x,y,z in the PlaneRender class is done in the setORientation
 *  function. Once the mapping is determined, all operations are in the local
 *  x,y,z space.
 *
 *  When a surface is added to the representation it is also mapped into the
 *  local x,y,z coordinates, based on the same transformation of ModelImage
 *  coordinates. Surfaces are displayed as the intersection of the
 *  ModelTriangleMesh with the rendered z-slice. The intersection is performed
 *  with a ModelClip object setting the z clipping planes to be at positions
 *  m_iSlice-1 and m_iSlice+1.
 */

public class PlaneRender extends VolumeCanvas3D
    implements MouseMotionListener, MouseListener, MouseBehaviorCallback {

    /**
     *
     *   @param ViewJFrameVolumeView ViewJFrameVolumeView - reference to
     *   parent frame.
     *   @param ModleImage First image to display, cannot be null.
     *   @param ModelLUT LUT of the imageA (if null grayscale LUT is constructed).
     *   @param ModelImage Second loaded image, may be null.
     *   @param ModelLUT LUT of the imageB, may be null.
     *   @param GraphicsConfiguration GraphicsConfiguration
     *   @param int Image dimension to be displayed.
     *   @param boolean, when true store all the data in memory, when false,
     *   write textues as the slices change
     *
     */
    public PlaneRender( ViewJFrameVolumeView kParent,
            ModelImage kImageA, ModelLUT kLUTa,
            ModelImage kImageB, ModelLUT kLUTb,
            GraphicsConfiguration kConfig, int iPlane,
            boolean bMemory ) {
        super( kConfig );
        m_kParent = kParent;
        m_iPlane = iPlane;
        m_bMemoryUsage = bMemory;

        m_kConfig = kConfig;
        m_kImageA = kImageA;
        m_kImageB = kImageB;
        m_kLUTa = kLUTa;
        m_kLUTb = kLUTb;
        m_bColorMap_initialized = false;
        m_kImageA.setImageOrder( ModelImage.IMAGE_A );
        if ( m_kImageB != null ) {
            m_kImageB.setImageOrder( ModelImage.IMAGE_B );
        }

        /* Determine the image orientation for the Model Image */
        m_iImageOrientation = m_kImageA.getImageOrientation();
        m_aiOrientation = new int[3];
        m_aiOrientation = m_kImageA.getFileInfo()[0].getAxisOrientation();
        if ( m_aiOrientation[0] == FileInfoBase.ORI_UNKNOWN_TYPE ) {
            if ( m_iImageOrientation == FileInfoBase.AXIAL ) {
                m_aiOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                m_aiOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                m_aiOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            } else if ( m_iImageOrientation == FileInfoBase.CORONAL ) {
                m_aiOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                m_aiOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                m_aiOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else {
                m_aiOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                m_aiOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                m_aiOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            }
        }

        /* Set the default buffer factors for indexing into the ModelImage
         * data: */
        m_aiBuffFactors = new int[3];
        m_aiBuffFactors[0] = 1;
        m_aiBuffFactors[1] = m_kImageA.getExtents()[ 0 ];
        m_aiBuffFactors[2] = m_kImageA.getExtents()[ 0 ] * m_kImageA.getExtents()[ 1 ];

        /* call setOrientation to set the actual x,y,z parameters based on the
         * m_aiOrientation values */
        setOrientation();

        /* Set up the canvas: */
        m_kCanvas = new VolumeCanvas3D( m_kConfig );
        m_kCanvas.addMouseMotionListener( this );
        m_kCanvas.addMouseListener( this );
        m_kCanvas.setCursor( new Cursor( Cursor.CROSSHAIR_CURSOR ) );
        m_kCanvas.setBackground( Color.white );

        /* Create the scene graph and initialize the rendering */
        init();
    }

    /**
     *  Creates the scene graph and initializes the SimpleUniverse and the
     * Viewing transformations */
    private void init() {
        createImageSceneGraph();

        m_kUniverse = new SimpleUniverse( m_kCanvas );

        View kView = m_kUniverse.getViewer().getView();

        kView.setProjectionPolicy( View.PARALLEL_PROJECTION );
        kView.setScreenScalePolicy( View.SCALE_EXPLICIT );

        /* This will move the ViewPlatform back a bit so the objects in the
         scene can be viewed. */
        m_kUniverse.getViewingPlatform().setNominalViewingTransform();

        m_kObjRootBG.compile();
        m_kUniverse.addBranchGraph( m_kObjRootBG );
        setVisible( true );
        m_kCanvas.update( m_kCanvas.getGraphics() );

        /* set the ViewScreenScale based on the transformation: */
        updateViewScreenScale( m_kCurrentTransform );
        initAxes();

        m_fMinA = (float) m_kImageA.getMin();
        m_fMaxA = (float) m_kImageA.getMax();
        if ( m_kImageB != null ) {
            m_fMinB = (float) m_kImageB.getMin();
            m_fMaxB = (float) m_kImageB.getMax();
        }

    }

    /**
     * Creates the scene graph, made up of a branch group parent, a transform
     * group under that which applies mouse behaviors to the scene, and a
     * branch groups under the transform group for the texture-mapped polygon.
     */
    private void createImageSceneGraph() {

        m_kCurrentTransform = new Transform3D();
        m_kCurrentTransform.setTranslation( new Vector3f( 0f, 0f, -2.5f ) );

        /* Initialize the Root Branch Group*/
        m_kObjRootBG = new BranchGroup();
        m_kObjRootBG.setCapability( BranchGroup.ALLOW_DETACH );
        m_kObjRootBG.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        m_kObjRootBG.setCapability( Group.ALLOW_CHILDREN_READ );
        m_kObjRootBG.setCapability( Group.ALLOW_CHILDREN_WRITE );

        m_kOrderedGroup = new OrderedGroup();
        m_kOrderedGroup.setCapability( Group.ALLOW_CHILDREN_EXTEND );
        m_kOrderedGroup.setCapability( Group.ALLOW_CHILDREN_READ );
        m_kOrderedGroup.setCapability( Group.ALLOW_CHILDREN_WRITE );

        m_kObjRootBG.addChild( m_kOrderedGroup );

        Background kBackground = new Background( new Color3f( Color.black ) );
        BoundingSphere kBounds = new BoundingSphere( new Point3d( 0.0f, 0.0f, 0.0f ), 100.0f );

        kBackground.setApplicationBounds( kBounds );
        m_kOrderedGroup.addChild( kBackground );

        /* Initialize the Zoom/Translate Transform Group: */
        m_kTranslationTG = new TransformGroup();
        m_kTranslationTG.setTransform( m_kCurrentTransform );
        m_kTranslationTG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        m_kTranslationTG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        m_kTranslationTG.setCapability( TransformGroup.ALLOW_CHILDREN_WRITE );
        m_kTranslationTG.setCapability( TransformGroup.ALLOW_CHILDREN_READ );
        m_kTranslationTG.setCapability( TransformGroup.ALLOW_CHILDREN_EXTEND );
        m_kOrderedGroup.addChild( m_kTranslationTG );

        /* Setup appearance attributes common for the texture mapped polygon. */
        Appearance kDefaultAppearance = new Appearance();
        RenderingAttributes kRenderingAttributes = new RenderingAttributes();

        kRenderingAttributes.setAlphaTestValue( 0.0f );
        kRenderingAttributes.setAlphaTestFunction( RenderingAttributes.GREATER );
        kDefaultAppearance.setRenderingAttributes( kRenderingAttributes );

        // Default appearance: (PolygonAttributes)
        PolygonAttributes kPolygonAttributes = new PolygonAttributes();

        kPolygonAttributes.setCapability( PolygonAttributes.ALLOW_OFFSET_WRITE );
        kDefaultAppearance.setPolygonAttributes( kPolygonAttributes );

        // Default appearance: (Material)
        // Disable lighting so that the color information comes from
        // the texture maps.
        Material kMaterial = new Material();

        kMaterial.setLightingEnable( false );
        kDefaultAppearance.setMaterial( kMaterial );

        // Default appearance: (TransparencyAttributes)
        // Use blended transparency mode which has the default blending
        // operation set to alpha_src*src + (1-alpha_src)*dst.  This works
        // only for back to front ordering.
        TransparencyAttributes kTransparencyAttr = new TransparencyAttributes();

        kDefaultAppearance.setTransparencyAttributes( kTransparencyAttr );
        kTransparencyAttr.setTransparency( 1.0f );
        kTransparencyAttr.setTransparencyMode( TransparencyAttributes.BLENDED );

        // Default appearance: (TextureAttributes)
        // Use Replace mode because we don't want to have to worry about
        // what color the slice is rendered as before the texture is
        // applied to it.
        TextureAttributes kTextureAttr = new TextureAttributes();

        kDefaultAppearance.setTextureAttributes( kTextureAttr );
        kTextureAttr.setTextureMode( TextureAttributes.REPLACE );

        // Default appearance: (Texture)
        // Allow the Texture attribute to be modified.
        kDefaultAppearance.setCapability( Appearance.ALLOW_TEXTURE_WRITE );

        // Default appearance: (TexCoordGeneration)
        // Allow the TexCoordGeneration attribute to be modified.
        kDefaultAppearance.setCapability( Appearance.ALLOW_TEXGEN_WRITE );

        QuadArray kGeometry = new QuadArray( 4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_2 );

        /* Setup the normalized polygon coordinates and texture coordinates:*/

        m_fX0 = -m_fXBox / m_fMaxBox;
        m_fX1 = m_fXBox / m_fMaxBox;
        m_fY0 = -m_fYBox / m_fMaxBox;
        m_fY1 = m_fYBox / m_fMaxBox;

        m_fXRange = m_fX1 - m_fX0;
        m_fYRange = m_fY1 - m_fY0;

        kGeometry.setCoordinate( 0, new Point3d( m_fX0, m_fY0, 0 ) );
        kGeometry.setCoordinate( 1, new Point3d( m_fX1, m_fY0, 0 ) );
        kGeometry.setCoordinate( 2, new Point3d( m_fX1, m_fY1, 0 ) );
        kGeometry.setCoordinate( 3, new Point3d( m_fX0, m_fY1, 0 ) );

        Vector4f kCoordMapX = new Vector4f( ( m_fMaxBox / m_fXBox ) / 2.0f, 0.0f, 0.0f, 0.5f );
        Vector4f kCoordMapY = new Vector4f( 0.0f, ( m_fMaxBox / m_fYBox ) / 2.0f, 0.0f, 0.5f );
        TexCoordGeneration kTexCoordGeneration = new TexCoordGeneration( TexCoordGeneration.OBJECT_LINEAR,
                TexCoordGeneration.TEXTURE_COORDINATE_2, kCoordMapX, kCoordMapY );

        /* The initial display is the middle of the range: */
        m_iSlice = ( m_iZBound - 1 ) / 2;
        m_kDisplayedImage = new ImageComponent2D( ImageComponent.FORMAT_RGBA, m_iXBound, m_iYBound );
        m_kDisplayedImage.setCapability( ImageComponent2D.ALLOW_IMAGE_WRITE );
        m_kDisplayedImage.setCapability( ImageComponent2D.ALLOW_IMAGE_READ );
        m_kDisplayedImage.setCapability( ImageComponent2D.ALLOW_SIZE_READ );
        m_kDisplayedImage.setCapability( ImageComponent2D.ALLOW_FORMAT_READ );
        if ( m_bMemoryUsage ) {
            writeTextures();
        } else {
            writeTexture();
        }

        m_kTexture = new Texture2D( Texture.BASE_LEVEL, Texture.RGBA, m_iXBound, m_iYBound );
        m_kTexture.setEnable( true );
        m_kTexture.setMinFilter( Texture.BASE_LEVEL_LINEAR );
        m_kTexture.setMagFilter( Texture.BASE_LEVEL_LINEAR );
        m_kTexture.setBoundaryModeS( Texture.CLAMP_TO_EDGE );
        m_kTexture.setBoundaryModeT( Texture.CLAMP_TO_EDGE );
        m_kTexture.setImage( 0, m_kDisplayedImage );
        m_kTexture.setCapability( Texture2D.ALLOW_IMAGE_WRITE );
        m_kTexture.setCapability( Texture2D.ALLOW_IMAGE_READ );
        m_kTexture.setCapability( Texture2D.ALLOW_ENABLE_WRITE );

        kDefaultAppearance.setTexture( m_kTexture );
        kDefaultAppearance.setTexCoordGeneration( kTexCoordGeneration );

        // Create the shape (geometry+appearance) for this slice.
        // Allow the appearance to be read.
        Shape3D kShape = new Shape3D( kGeometry, kDefaultAppearance );

        m_kTranslationTG.addChild( kShape );

        /* Setup the MouseZoom behavior: */
        m_kMouseZoom = new MouseZoomBehavior( m_kTranslationTG );
        m_kMouseZoom.setupCallback( this );
        m_kMouseZoom.setSchedulingBounds( kBounds );
        m_kMouseZoom.setFactor( 0.005 );
        m_kTranslationTG.addChild( m_kMouseZoom );

        /* Setup the MouseTranslate behavior: */
        m_kMouseTranslate = new MouseTranslation( m_kTranslationTG );
        m_kMouseTranslate.setupCallback( this );
        m_kMouseTranslate.setSchedulingBounds( kBounds );
        m_kMouseTranslate.setFactor( 0.005 );
        m_kTranslationTG.addChild( m_kMouseTranslate );

        /* Center and draw the X and Y bars: */
        m_fCenterX = ( m_fX0 + m_fX1 ) / 2.0f;
        m_fCenterY = ( m_fY0 + m_fY1 ) / 2.0f;
        drawLabels();

        /* Delete local variables: */
        kBackground = null;
        kBounds = null;
        kDefaultAppearance = null;
        kRenderingAttributes = null;
        kPolygonAttributes = null;
        kMaterial = null;
        kTransparencyAttr = null;
        kTextureAttr = null;
        kGeometry = null;
        kCoordMapX = null;
        kCoordMapY = null;
        kTexCoordGeneration = null;
        kShape = null;
    }

    /**
     *  Accessor that returns the reference to image A.
     *  @return Image A.
     */
    public ModelImage getImageA() {
        return m_kImageA;
    }

    /**
     *  Accessor that returns the reference to image B.
     *  @return Image B.
     */
    public ModelImage getImageB() {
        return m_kImageB;
    }

    /**
     *  Accessor that sets the LUT for image A.
     *  @param LUT  The LUT.
     */
    public void setLUTa( ModelLUT LUT ) {
        m_kLUTa = LUT;
        m_bColorMap_initialized = false;
        if ( m_bMemoryUsage ) {
            writeTextures();
        } else {
            writeTexture();
        }
    }

    /**
     * Accessor that sets the RGB lookup table for image A.
     * @param RGBT
     */
    public void setRGBTA( ModelRGB RGBT ) {
        m_kRGBTA = RGBT;
        m_bColorMap_initialized = false;
        if ( m_bMemoryUsage ) {
            writeTextures();
        } else {
            writeTexture();
        }
    }

    /**
     *  Accessor that sets the LUT for image B.
     *  @param LUT  The LUT.
     */
    public void setLUTb( ModelLUT LUT ) {
        m_kLUTb = LUT;
        m_bColorMap_initialized = false;
        if ( m_bMemoryUsage ) {
            writeTextures();
        } else {
            writeTexture();
        }
    }

    /**
     * Accessor that sets the RGB lookup table for image A.
     * @param RGBT
     */
    public void setRGBTB( ModelRGB RGBT ) {
        m_kRGBTB = RGBT;
        m_bColorMap_initialized = false;
        if ( m_bMemoryUsage ) {
            writeTextures();
        } else {
            writeTexture();
        }
    }

    /**
     *  Cleans  memory.
     */
    protected void finalize()
        throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     *   Closes the frame.
     */
    public void close() {
        disposeLocal();
    }

    /**
     *  Clean memory.
     *  @param flag is true call the super.disposeLocal
     */
    public void disposeLocal() {
        m_kImageA = null;
        m_kImageB = null;

        for ( int iColor = 0; iColor < 3; iColor++ ) {
            m_akXColors[iColor] = null;
            m_akYColors[iColor] = null;
            m_akZColors[iColor] = null;
        }
        m_kLabelX = null;
        m_kLabelY = null;

        m_aiOrientation = null;
        m_aiBuffFactors = null;

        m_kCanvas = null;
        if ( m_kUniverse != null ) {
            m_kUniverse.removeAllLocales();
            m_kUniverse = null;
        }
        m_kCurrentTransform = null;
        m_kObjRootBG = null;
        m_kOrderedGroup = null;

        m_kTranslationTG = null;

        if ( m_bMemoryUsage ) {
            for ( int iZ = 0; iZ < m_iZBound; iZ++ ) {
                m_akImageComponent[iZ] = null;
            }
            m_akImageComponent = null;
        }
        m_kDisplayedImage = null;
        m_kTexture = null;

        m_kMouseZoom = null;
        m_kMouseTranslate = null;

        for ( int iChild = 0; iChild < m_iNumChildren; iChild++ ) {
            removeSurface( iChild );
        }

        m_kTextBranchGroup = null;
        m_kTextTransformGroup = null;

        m_aiLutBufferRemappedA = null;

        m_aiLutBufferRemappedB = null;
        m_RGBIndexBufferA = null;
        m_RGBIndexBufferB = null;

        if ( m_kRFA_BranchGroup != null ) {
            m_kRFA_BranchGroup = null;
        }

        m_afXWin = null;
        m_afYWin = null;

    }

    /**
     *	Tells the mouse dialog that the transform has changed.
     *	@param type Type of transform.
     *	@param transform Transform that was changed to.
     */
    public void transformChanged( int type, Transform3D transform ) {

        /* The View is set to parallel, so zooming changes the
         * View.ScreenScale value, calculated by calling
         * updateViewScreenScale: */
        if ( MouseBehaviorCallback.ZOOM == type ) {
            updateViewScreenScale( transform );

            /* Undo zoom for the Axes so the remain in place: */
            float fXTrans = m_fX0 * 0.85f / m_fZoomScale;
            float fYTrans = m_fY1 * 0.85f / m_fZoomScale;

            if ( !m_bInvertY ) {
                fYTrans = -fYTrans;
            }
            Transform3D kTextTransform = new Transform3D();

            kTextTransform.setTranslation( new Vector3f( fXTrans, fYTrans, 0.0f ) );
            kTextTransform.setScale( 0.01f / m_fZoomScale );
            m_kTextTransformGroup.setTransform( kTextTransform );
            kTextTransform = null;
        }

        /* Translation: */
        if ( MouseBehaviorCallback.TRANSLATE == type ) {

            /* Keep track of the x,y translation for mouse movements and for
             * when a surface is added to the scene: */
            Matrix4d kMatrixZoomTranslate = new Matrix4d();

            transform.get( kMatrixZoomTranslate );
            m_fXTranslate = (float) ( kMatrixZoomTranslate.m03 );
            m_fYTranslate = (float) ( kMatrixZoomTranslate.m13 );
            kMatrixZoomTranslate = null;
        }
    }

    /**
     *  This function calculates the scale factor for zooming in parallel
     *  projection.   The scenario is to calculate the distance between
     *  the origin boxframe center and tranformed boxframe center.   This
     *  distance is used to compute the screen scale factor.
     *  @param kTransform     The tranformation matrix from tranformChanged().
     */
    private void updateViewScreenScale( Transform3D kTransform ) {
        float dRadius = m_fX1 - m_fX0;

        if ( ( m_fY1 - m_fY0 ) > dRadius ) {
            dRadius = m_fY1 - m_fY0;
        }
        BoundingSphere kBounds = new BoundingSphere( new Point3d( 0, 0, 0 ), dRadius );

        kBounds.transform( kBounds, kTransform );

        Point3d kVolumeCenterPoint = new Point3d();

        kBounds.getCenter( kVolumeCenterPoint );
        float dDist = Math.abs( (float) kVolumeCenterPoint.z );

        View kView = m_kUniverse.getViewer().getView();
        float dFieldOfView = (float) kView.getFieldOfView();
        float dViewWidth = 16.0f * dDist * (float) Math.tan( dFieldOfView / 15.0f );

        Screen3D kScreen = m_kCanvas.getScreen3D();

        float dNewScreenScale = (float) kScreen.getPhysicalScreenWidth() / dViewWidth;

        kView.setScreenScale( dNewScreenScale );

        /* Calculate and store the original screenscale: */
        dViewWidth = 16.0f * 2 * (float) Math.tan( dFieldOfView / 15.0f );
        float dOriginalScreenScale = (float) kScreen.getPhysicalScreenWidth() / dViewWidth;

        m_fZoomScale = (float) ( dNewScreenScale / dOriginalScreenScale );
        m_fZoomScale /= dFieldOfView;

        kBounds = null;
        kVolumeCenterPoint = null;
    }

    /**
     * Sets the background color for the frame and rendered image.
     * @param color RGBA color to use as the background color.
     */
    public void setBackgroundColor( Color color ) {
        m_kCanvas.setBackground( color );
        return;
    }

    /**
     * Calculate the center of the mouse in plane coordinates, taking
     * into account zoom and translate:
     * @param iX mouse x coordinate value
     * @param iY mouse y coordinate value
     * @param afCenter center in plane coordinates
     */
    private void getCenter( int iX, int iY, float[] afCenter ) {
        afCenter[0] = (float) ( iX - ( m_iCanvasWidth - 1 ) / 2.0f ) / m_fDivX;
        afCenter[1] = (float) ( ( m_iCanvasHeight - 1 ) - iY - ( m_iCanvasHeight - 1 ) / 2.0f ) / m_fDivY;

        afCenter[0] /= m_fZoomScale;
        afCenter[1] /= m_fZoomScale;

        afCenter[0] -= m_fXTranslate;
        afCenter[1] -= m_fYTranslate;

        /* Bounds checking: */
        if ( afCenter[0] < m_fX0 ) {
            afCenter[0] = m_fX0;
        }
        if ( afCenter[0] > m_fX1 ) {
            afCenter[0] = m_fX1;
        }
        if ( afCenter[1] < m_fY0 ) {
            afCenter[1] = m_fY0;
        }
        if ( afCenter[1] > m_fY1 ) {
            afCenter[1] = m_fY1;
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener.  This function is
     * invoked when a mouse button is held down and the mouse is dragged in
     * the active window area.
     * @param kEvent the mouse event generated by a mouse drag
     */
    public void mouseDragged( MouseEvent kEvent ) {

        /* If the right mouse button is pressed and
         * dragged. processRightMouseDrag updates the HistoLUT window and
         * level (contrast and brightness) */
        if ( m_bRightMousePressed && !kEvent.isShiftDown() ) {
            processRightMouseDrag( kEvent );
        }

        /* Dragging the mouse with the left-mouse button held down changes the
         * positions of the X and Y cross bars, and therefor the ZSlice positions
         * of the associated PlaneRender objects and the TriPlanar Surface. The
         * new positions are calculated and passed onto the parent frame.
         */
        if ( m_bLeftMousePressed && !kEvent.isShiftDown() ) {
            processLeftMouseDrag( kEvent );
        }
    }

    /**
     * If the right mouse button is pressed and dragged. processRightMouseDrag
     * updates the HistoLUT window and level (contrast and brightness)
     * @param kEvent the mouse event generated by a mouse drag
     */
    private void processRightMouseDrag( MouseEvent kEvent ) {

        /* Get the coordinates of the mouse position in local coordinates: */
        float[] afCenter = new float[2];

        getCenter( kEvent.getX(), kEvent.getY(), afCenter );
        float fX = ( afCenter[0] - m_fX0 ) / m_fXRange;
        float fY = ( afCenter[1] - m_fY0 ) / m_fYRange;

        /* If this is the first time the mouse is dragged after the right
         * mouse button has been pressed, setup the member variables to change
         * the HistoLUT. This setup happens each time after the right mouse
         * button is pressed and relased: */
        if ( m_bFirstRightDrag ) {
            m_bFirstRightDrag = false;

            try {
                Image kImg = MipavUtil.getIconImage( "qkwinlevel.gif" );

                Cursor kWinLevelCursor = Toolkit.getDefaultToolkit().createCustomCursor( kImg, new Point( 12, 12 ),
                        "WinLevel" );

                /* Set the cursor icon: */
                m_kCanvas.setCursor( kWinLevelCursor );
            } catch ( FileNotFoundException error ) {}

            /* Get which LUT is active, either m_kLUTa or m_kLUTb: */
            m_kActiveLUT = null;

            /* Get which image is active, either m_kImageA or m_kImageB: */
            m_kActiveImage = m_kParent.getHistoLUTActiveImage();
            if ( m_kActiveImage == null ) {
                m_kActiveImage = m_kImageA;
            }
            if ( ( m_kActiveImage == m_kImageA ) && ( m_kLUTa != null ) ) {
                m_kActiveLUT = m_kLUTa;
                m_fMin = m_fMinA;
                m_fMax = m_fMaxA;
            } else if ( ( m_kActiveImage == m_kImageB ) && ( m_kLUTb != null ) ) {
                m_kActiveLUT = m_kLUTb;
                m_fMin = m_fMinB;
                m_fMax = m_fMaxB;
            }

            /* Reset the transferline: */
            if ( ( m_kActiveImage != null ) && ( m_kActiveLUT != null ) ) {
                m_kActiveLUT.resetTransferLine( m_fMin, m_fMax );
                m_kActiveLUT.getTransferFunction().exportArrays( m_afXWin, m_afYWin );

                m_afXWin[1] = m_afXWin[0];
                m_afXWin[2] = m_afXWin[3];
                m_afYWin[1] = m_afYWin[0];
                m_afYWin[2] = m_afYWin[3];

                m_kActiveLUT.getTransferFunction().importArrays( m_afXWin, m_afYWin, 4 );

                /* Keep track if the mouse position changed: */
                m_fOldX = fX;
                m_fOldY = fY;
            }
        } /* Dragging has been initialized on the previous call, this changes
         * the HistoLUT: */ else if ( ( m_kActiveImage != null ) && ( m_kActiveLUT != null )
                && ( ( m_fOldX != fX ) || ( m_fOldY != fY ) ) ) {

            /* Determine the HistoLUT window image size based on the
             * ModelImage: */
            float fMinImageWin;
            float fMaxImageWin;

            if ( m_kActiveImage.getType() == ModelStorageBase.UBYTE ) {
                fMinImageWin = 0;
                fMaxImageWin = 255;
            } else if ( m_kActiveImage.getType() == ModelStorageBase.BYTE ) {
                fMinImageWin = -128;
                fMaxImageWin = 127;
            } else {
                fMinImageWin = m_fMin;
                fMaxImageWin = m_fMax;
            }

            /* The new window value is based on the x coordinate position of
             * the mouse in the PlaneRender window: */
            m_fWindow = 2.0f * fX * ( fMaxImageWin - fMinImageWin );
            if ( m_fWindow > 2.0f * ( fMaxImageWin - fMinImageWin ) ) {
                m_fWindow = 2.0f * ( fMaxImageWin - fMinImageWin );
            } else if ( m_fWindow < 0 ) {
                m_fWindow = 0;
            }

            /* The new level value is based on the y coordinate of the mouse
             * in the PlaneRender window: */
            m_fLevel = fY * ( fMaxImageWin - fMinImageWin );
            if ( m_fLevel > fMaxImageWin ) {
                m_fLevel = fMaxImageWin;
            } else if ( m_fLevel < fMinImageWin ) {
                m_fLevel = fMinImageWin;
            }

            /* The new x positions, and y positions of the middle points on
             * the transfer line: */
            m_afXWin[2] = m_fLevel + m_fWindow / 2.0f;
            m_afXWin[1] = m_fLevel - m_fWindow / 2.0f;
            m_afYWin[2] = m_afYWin[3];
            m_afYWin[1] = m_afYWin[0];
            if ( m_afXWin[2] > fMaxImageWin ) {
                m_afYWin[2] = 255.0f * ( m_afXWin[2] - fMaxImageWin ) / m_fWindow;
                if ( m_afYWin[2] > 255.0f ) {
                    m_afYWin[2] = 255.0f;
                }
                m_afXWin[2] = fMaxImageWin;
            }
            if ( m_afXWin[1] < fMinImageWin ) {
                m_afYWin[1] = 255.0f - 255.0f * ( fMinImageWin - m_afXWin[1] ) / m_fWindow;
                if ( m_afYWin[1] < 0.0f ) {
                    m_afYWin[1] = 0.0f;
                }
                m_afXWin[1] = fMinImageWin;
            }

            /* Update the HistoLUT and the renderers: */
            m_kActiveLUT.getTransferFunction().importArrays( m_afXWin, m_afYWin, 4 );
            m_kParent.updateSurRenderWinlevel(false);
            m_kActiveImage.notifyImageDisplayListeners( m_kActiveLUT, false );
            if ( m_kActiveLUT == m_kLUTa ) {
                m_kParent.getLUTDialog().setLUTA( m_kActiveLUT );
            } else {
                m_kParent.getLUTDialog().setLUTB( m_kActiveLUT );
            }

            /* Store old change in X,Y positions: */
            m_fOldX = fX;
            m_fOldY = fY;
        }
    }

    /**
     *  Dragging the mouse with the left-mouse button held down changes the
     * positions of the X and Y cross bars, and therefor the ZSlice positions
     * of the associated PlaneRender objects and the TriPlanar Surface. The
     * new positions are calculated and passed onto the parent frame.
     * @param kEvent the mouse event generated by a mouse drag
     */
    private void processLeftMouseDrag( MouseEvent kEvent ) {

        /* If the RFA point is enabled, then the mouse is used to select
         * the Probe point, not to move the slice positions: */
        if ( m_bEntryPointSelect ) {
            return;
        }

        /* Calculate the center of the mouse in plane coordineates, taking
         * into account zoom and translate: */
        float[] afCenter = new float[2];

        getCenter( kEvent.getX(), kEvent.getY(), afCenter );
        m_fCenterX = afCenter[0];
        m_fCenterY = afCenter[1];
        afCenter = null;

        /* Draw the new label positions: */
        drawLabels();

        /* Tell the ViewJFrameVolumeView parent to update the other
         * PlaneRenders and the SurfaceRender with the changed Z position
         * of the planes with color matching the moved bar: */
        float fCenterX = ( m_fCenterX - m_fX0 ) / m_fXRange;

        if ( m_bInvertX ) {
            fCenterX = 1 - fCenterX;
        }
        m_kParent.setSlice( m_kXSliceHairColor, fCenterX );
        m_kParent.setBar( m_kXSliceHairColor, fCenterX );

        float fCenterY = ( m_fCenterY - m_fY0 ) / m_fYRange;

        if ( !m_bInvertY ) {
            fCenterY = 1 - fCenterY;
        }
        m_kParent.setSlice( m_kYSliceHairColor, fCenterY );
        m_kParent.setBar( m_kYSliceHairColor, fCenterY );
        m_kParent.setAbsPositionLabels();
    }

    /**
     * One of the overrides necessary to be a MouseListener.
     *
     * If the left mouse button is pressed, the function sets the
     * m_bLeftMousePressed to be true, and records the current canvas width and
     * height.
     *
     * @param kEvent the mouse event generated by a mouse press
     */
    public void mousePressed( MouseEvent kEvent ) {

        /* If the button pressed is the left mouse button: */
        if ( kEvent.getButton() == MouseEvent.BUTTON1 && !kEvent.isShiftDown() ) {
            m_bLeftMousePressed = true;
        }
        if ( kEvent.getButton() == MouseEvent.BUTTON3 && !kEvent.isShiftDown() ) {
            m_bRightMousePressed = true;
        }
        if ( m_bLeftMousePressed || m_bRightMousePressed ) {

            /* Calculate the current zoom and translate factors for
             * transforming mouse position to plane coordinates: */
            m_iCanvasWidth = m_kCanvas.getWidth();
            m_iCanvasHeight = m_kCanvas.getHeight();

            m_fDivX = (float) m_iCanvasWidth / 2.0f;
            m_fDivY = (float) m_iCanvasWidth / 2.0f;
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener.
     * @param kEvent the mouse event generated by a mouse release
     */
    public void mouseReleased( MouseEvent kEvent ) {

        /* If the button pressed is the left mouse button turn off the
         * m_bLeftMousePressed flag: */
        if ( kEvent.getButton() == MouseEvent.BUTTON3 && !kEvent.isShiftDown() ) {
        	m_kParent.updateSurRenderWinlevel(true);
            m_kActiveImage.notifyImageDisplayListeners( m_kActiveLUT, false );
            m_bRightMousePressed = false;
            m_bFirstRightDrag = true;
            m_kCanvas.setCursor( new Cursor( Cursor.CROSSHAIR_CURSOR ) );
        }
        if ( kEvent.getButton() == MouseEvent.BUTTON1 && !kEvent.isShiftDown() ) {
            m_bLeftMousePressed = false;

            /* If the RFA probe point is being set by the mouse, then
             * calculate the mouse position in ModelImage coordinates and pass
             * the information to the parent class: */
            if ( m_bEntryPointSelect ) {

                /* Calculate the center of the mouse in plane coordineates, taking
                 * into account zoom and translate: */
                float[] afCenter = new float[2];

                getCenter( kEvent.getX(), kEvent.getY(), afCenter );
                float fCenterX = afCenter[0];
                float fCenterY = afCenter[1];

                afCenter = null;

                /* Convert the mouse position into ModelImage coordinates: */
                fCenterX = ( fCenterX - m_fX0 ) / m_fXRange;
                if ( m_bInvertX ) {
                    fCenterX = 1 - fCenterX;
                }

                fCenterY = ( fCenterY - m_fY0 ) / m_fYRange;
                if ( !m_bInvertY ) {
                    fCenterY = 1 - fCenterY;
                }

                float fZ = (float) ( m_iSlice ) / (float) ( m_iZBound - 1 );

                if ( m_bInvertZ ) {
                    fZ = 1 - fZ;
                }

                float[] fPoint = new float[3];

                fPoint[ m_iXIndex ] = fCenterX;
                fPoint[ m_iYIndex ] = fCenterY;
                fPoint[ m_iZIndex ] = (float) fZ;
                Point3f kRFAPoint = new Point3f();

                kRFAPoint.x = fPoint[0];
                kRFAPoint.y = fPoint[1];
                kRFAPoint.z = fPoint[2];

                /* Tell the parent to draw the RFA point: */
                m_kParent.drawRFAPoint( kRFAPoint );
                fPoint = null;
            }

        }
    }

    /**
     * One of the overrides necessary to be a MouseListener. This function is
     * called when there is a double-click event.
     * @param kEvent the mouse event generated by a mouse clicked
     */
    public void mouseClicked( MouseEvent kEvent ) {/* stub */}

    /**
     * One of the overrides necessary to be a MouseListener. This function is
     * called when the mouse enters the active area.
     * @param kEvent the mouse event generated by a mouse entered
     */
    public void mouseEntered( MouseEvent kEvent ) {/* stub */}

    /**
     * One of the overrides necessary to be a mouselistener. This function is
     * called when the mouse leaves the active area.
     * @param kEvent the mouse event generated by a mouse exit
     */
    public void mouseExited( MouseEvent kEvent ) {/* stub */}

    /**
     * One of the overrides necessary to be a MouseMotionListener. This
     * function is called when the mouse is moved (without holding any buttons
     * down).
     * @param kEvent the event generated by a mouse movement
     */
    public void mouseMoved( MouseEvent kEvent ) {}

    /**
     * Changes the displayed texture based on the new value for m_iSlice.
     * @param fSlice The relative position along the actual m_iZBound dimension of the new slice.
     */
    public void setSlice( float fSlice ) {
        int iSlice = Math.round( ( m_iZBound - 1 ) * fSlice );

        /* If the ModelImage data is to be inverted in Z:*/
        if ( m_bInvertZ ) {
            iSlice = (int) ( ( m_iZBound - 1 ) * ( 1 - fSlice ) - 0.5f );
        }

        /* Check bounds: */
        if ( iSlice > ( m_iZBound - 1 ) ) {
            iSlice = m_iZBound - 1;
        }
        if ( iSlice < 0 ) {
            iSlice = 0;
        }
        if ( iSlice != m_iSlice ) {
            m_iSlice = iSlice;

            /* Set the new texture image: */
            if ( m_bMemoryUsage ) {
                m_kDisplayedImage.set( m_akImageComponent[ m_iSlice ].getImage() );
            } else {
                writeTexture();
            }
        }
    }

    /**
     * Sets the new location of the XBar
     * @param fSlice The new position of the XBar in plane coordinates:
     */
    public void setXBar( float fSlice ) {
        if ( m_bLeftMousePressed ) {
            return;
        }
        if ( m_bInvertX ) {
            fSlice = 1 - fSlice;
        }
        m_fCenterX = fSlice * ( m_fX1 - m_fX0 ) + m_fX0;
        drawLabels();
    }

    /**
     * Sets the new location of the YBar
     * @param fSlice The new position of the YBar in plane coordinates:
     */
    public void setYBar( float fSlice ) {
        if ( m_bLeftMousePressed ) {
            return;
        }
        if ( !m_bInvertY ) {
            fSlice = 1 - fSlice;
        }
        m_fCenterY = fSlice * ( m_fY1 - m_fY0 ) + m_fY0;
        drawLabels();
    }

    /**
     *  Returns the X bar color.
     *  @return color of the x color bar
     */
    public Color3f getXBarColor() {
        return m_kXSliceHairColor;
    }

    /**
     *  Returns the Y bar color.
     *  @return color of the y color bar
     */
    public Color3f getYBarColor() {
        return m_kYSliceHairColor;
    }

    /**
     * Returns the Z bar color.
     * @return color of the z color bar
     */
    public Color3f getColor() {
        return m_kZSliceHairColor;
    }

    /**
     * Sets the default color for the XSliceHairColor
     * @param kColor set the hair color to this color
     */
    public void setXSliceHairColor( Color3f kColor ) {
        m_akXColors[0] = kColor;
        m_akXColors[2] = kColor;
        m_akZColors[1] = kColor;

        m_kXSliceHairColor = m_akXColors[ m_iPlane ];
        m_kYSliceHairColor = m_akYColors[ m_iPlane ];
        m_kZSliceHairColor = m_akZColors[ m_iPlane ];

        initAxes();
    }

    /**
     * Sets the default color fot the YSliceHairColor
     * @param kColor set the hair color to this color
     */
    public void setYSliceHairColor( Color3f kColor ) {
        m_akXColors[1] = kColor;
        m_akYColors[0] = kColor;
        m_akZColors[2] = kColor;

        m_kXSliceHairColor = m_akXColors[ m_iPlane ];
        m_kYSliceHairColor = m_akYColors[ m_iPlane ];
        m_kZSliceHairColor = m_akZColors[ m_iPlane ];

        initAxes();
    }

    /**
     * Sets the default color fot the ZSliceHairColor
     * @param kColor set the hair color to this color
     */
    public void setZSliceHairColor( Color3f kColor ) {
        m_akYColors[1] = kColor;
        m_akYColors[2] = kColor;
        m_akZColors[0] = kColor;

        m_kXSliceHairColor = m_akXColors[ m_iPlane ];
        m_kYSliceHairColor = m_akYColors[ m_iPlane ];
        m_kZSliceHairColor = m_akZColors[ m_iPlane ];

        initAxes();
    }

    /**
     * Called when the flythru view changes position, sets
     * the m_iSlice based on the z-value of the path position.
     * @param kPosition the 3d position of a point used to set the slices
     */
    public void setPathPosition( Point3f kPosition ) {
        float[] fPosition = new float[3];

        fPosition[0] = kPosition.x;
        fPosition[1] = kPosition.y;
        fPosition[2] = kPosition.z;

        float fZSlice = fPosition[ m_iZIndex ];

        int iSlice = (int) ( ( m_iZBound - 1 ) * fZSlice );

        /* If the ModelImage data is to be inverted in Z:*/
        if ( m_bInvertZ ) {
            iSlice = ( m_iZBound - 1 ) - iSlice;
        }

        /* Check bounds: */
        if ( iSlice > ( m_iZBound - 1 ) ) {
            iSlice = m_iZBound - 1;
        }
        if ( iSlice < 0 ) {
            iSlice = 0;
        }
        if ( iSlice == m_iSlice ) {
            return;
        }

        if ( fZSlice < 0 ) {
            fZSlice = 0;
        } else if ( fZSlice > 1 ) {
            fZSlice = 1;
        }
        if ( m_bInvertZ ) {
            fZSlice = 1 - fZSlice;
        }
        m_kParent.setSlice( m_kZSliceHairColor, fZSlice );

        fPosition = null;
    }

    /**
     * Called when the color is changed in JPanelSurface, sets the
     * new color for the ModelTriangleMesh
     * @param iIndex
     * @param kColor
     */
    public void setColor( int iIndex, Color4f kColor ) {
        Color currColor = new Color( 0, 0, 0 );

        currColor = kColor.get();
        if ( iIndex == m_iBranchSurface ) {
            iIndex++;
        }
        if ( m_kColorVector.elementAt( iIndex ) != null ) {
            m_kColorVector.setElementAt( currColor, iIndex );
        }
        writeTexture();
    }

    /**
     * Sets
     * @param kColor Color4f
     */
    public void setBranchColor( Color4f kColor ) {
        Color currColor = new Color( 0, 0, 0 );

        currColor = kColor.get();
        if ( m_kColorVector.elementAt( m_iBranchSurface ) != null ) {
            m_kColorVector.setElementAt( currColor, m_iBranchSurface );
        }
        writeTexture();
    }

    /*
     * Adds a surface to the PlaneRender view. The surface is displayed as the
     * intersection of the trianglemesh with the current Z-Slice
     * (m_iSlice). The intersection is done with a ModelClip object, setting
     * the z-clipping planes to be m_iSlice-1 and m_iSlice+1.
     *
     * The surface is first transformed into local x,y,z coordinates.  The
     * surface color is set to match the Z-Slice color for this PlaneRender
     * object.
     *
     * The surface is first transformed into local x,y,z coordinates.  The
     * surface color is set to match the Z-Slice color for this PlaneRender
     * object.
     * @param kMask BitSet   surface bit set mask
     * @param kColor Color   surface color
     */
    public void setSurface( BitSet kMask, Color3f[] kMaskColors, Color kColor ) {
        m_kColorVector.addElement( kColor );
        m_kMaskVector.add( kMask );
        m_kMaskColorVector.add( kMaskColors );

        writeTexture();
    }

    /**
     * Update the plane render surface boundary with the given mask and color.
     * @param kMask
     * @param kMaskColors
     * @param kColor
     */
    public void setBranchSurface( BitSet kMask, Color3f[] kMaskColors, Color kColor ) {
        m_iBranchSurface = m_kColorVector.size();

        m_kColorVector.addElement( kColor );
        m_kMaskVector.add( kMask );
        m_kMaskColorVector.add( kMaskColors );

        writeTexture();
    }

    /**
     * Called when the surface is removed from the JPanelSurfaces. Detaches
     * the MeshBranch group from the m_kOrderedGroup, and deletes it and it's
     * associated variables:
     * @param iIndex
     */
    public void removeSurface( int iIndex ) {
        if ( iIndex == m_iBranchSurface ) {
            iIndex++;
        } else if ( iIndex < m_iBranchSurface ) {
            m_iBranchSurface--;
        }
        removeSurfaceColor( iIndex );
    }

    /**
     * Removes the branch surface boundary.
     */
    public void removeBranchSurface() {
        removeSurfaceColor( m_iBranchSurface );
    }

    /**
     * When surface is removed from the surface list, remove the color and mask
     * associate with that surface.
     * @param iIndex int surface index
     */
    private void removeSurfaceColor( int iIndex ) {
        m_kMaskVector.removeElementAt( iIndex );
        m_kMaskColorVector.removeElementAt( iIndex );
        m_kColorVector.removeElementAt( iIndex );
        writeTexture();
    }

    /**
     * Turns displaying the Axis labes on or off:
     * @param bShow
     */
    public void showAxes( boolean bShow ) {
        if ( m_bDrawAxes != bShow ) {
            m_bDrawAxes = bShow;
            if ( m_bDrawAxes ) {
                m_kOrderedGroup.addChild( m_kTextBranchGroup );
            } else {
                m_kOrderedGroup.removeChild( m_kTextBranchGroup );
            }
        }
    }

    /**
     *  Turns displaying the X and Y bars on or off:
     *  @param bShow
     */
    public void showXHairs( boolean bShow ) {
        m_bDrawXHairs = bShow;
    }

    /** Causes the labels to be redrawn: */
    public void update() {
        drawLabels();
        m_kCanvas.repaint();
    }

    /**
     *  Causes new data to be loaded from the ModelImage into the textures and
     * redisplayed on the texture-mapped polygon:
     */
    public void updateData() {
        if ( m_bMemoryUsage ) {
            writeTextures();
        } else {
            writeTexture();
        }
        drawLabels();
    }

    /**
     * Causes the data to be redrawn with new LUT values:
     * @param LUTa
     * @param LUTb
     */
    public void updateLut( ModelLUT LUTa, ModelLUT LUTb ) {
        if ( LUTa != null ) {
            m_kLUTa = LUTa;
            m_bColorMap_initialized = false;
        }
        if ( LUTb != null ) {
            m_kLUTb = LUTb;
            m_bColorMap_initialized = false;
        }
        if ( m_bMemoryUsage ) {
            for ( int iZ = 0; iZ < m_iZBound; iZ++ ) {
                m_akImageComponent[iZ] = null;
            }
            m_akImageComponent = null;
            writeTextures();
        } else {
            writeTexture();
        }
    }

    /**
     *  Causes the data to be redrawn with new RGBTA values:
     *  @param RGBT
     */
    public void updateRGBTA( ModelRGB RGBT ) {
        m_kRGBTA = RGBT;
        m_bColorMap_initialized = false;
        if ( m_bMemoryUsage ) {
            for ( int iZ = 0; iZ < m_iZBound; iZ++ ) {
                m_akImageComponent[iZ] = null;
            }
            m_akImageComponent = null;
            writeTextures();
        } else {
            writeTexture();
        }
    }

    /** Causes the data to be redrawn with new RGBTA values:
     * @param RGBT
     */
    public void updateRGBTB( ModelRGB RGBT ) {
        m_kRGBTB = RGBT;
        m_bColorMap_initialized = false;
        if ( m_bMemoryUsage ) {
            for ( int iZ = 0; iZ < m_iZBound; iZ++ ) {
                m_akImageComponent[iZ] = null;
            }
            m_akImageComponent = null;
            writeTextures();
        } else {
            writeTexture();
        }
    }

    /* Returns the VolumeCanvas3D object. */
    public VolumeCanvas3D getCanvas() {
        return m_kCanvas;
    }

    /** Draws the Z box, the X bar and the Y bar: */
    private void drawLabels() {
        for ( int iNode = 3; iNode < m_kTranslationTG.numChildren(); iNode++ ) {
            m_kTranslationTG.removeChild( iNode );
        }

        LineArray kBox = new LineArray( 8, GeometryArray.COORDINATES | GeometryArray.COLOR_3 );

        kBox.setCoordinate( 0, new Point3d( m_fX0, m_fY0, 0.1 ) );
        kBox.setColor( 0, m_kZSliceHairColor );
        kBox.setCoordinate( 1, new Point3d( m_fX1, m_fY0, 0.1 ) );
        kBox.setColor( 1, m_kZSliceHairColor );
        kBox.setCoordinate( 2, new Point3d( m_fX1, m_fY0, 0.1 ) );
        kBox.setColor( 2, m_kZSliceHairColor );
        kBox.setCoordinate( 3, new Point3d( m_fX1, m_fY1, 0.1 ) );
        kBox.setColor( 3, m_kZSliceHairColor );
        kBox.setCoordinate( 4, new Point3d( m_fX1, m_fY1, 0.1 ) );
        kBox.setColor( 4, m_kZSliceHairColor );
        kBox.setCoordinate( 5, new Point3d( m_fX0, m_fY1, 0.1 ) );
        kBox.setColor( 5, m_kZSliceHairColor );
        kBox.setCoordinate( 6, new Point3d( m_fX0, m_fY1, 0.1 ) );
        kBox.setColor( 6, m_kZSliceHairColor );
        kBox.setCoordinate( 7, new Point3d( m_fX0, m_fY0, 0.1 ) );
        kBox.setColor( 7, m_kZSliceHairColor );

        Shape3D kBoxShape = new Shape3D();

        kBoxShape.addGeometry( kBox );
        kBoxShape.setPickable( false );

        TransformGroup kBoxTransformGroup = new TransformGroup( new Transform3D() );

        kBoxTransformGroup.addChild( kBoxShape );

        BranchGroup kBoxBranchGroup = new BranchGroup();

        kBoxBranchGroup.setCapability( BranchGroup.ALLOW_DETACH );
        kBoxBranchGroup.addChild( kBoxTransformGroup );
        kBoxBranchGroup.compile();

        m_kTranslationTG.addChild( kBoxBranchGroup );

        TransformGroup kTransformGroup = new TransformGroup( new Transform3D() );

        if ( m_bDrawXHairs ) {
            LineArray kYBar = new LineArray( 4, GeometryArray.COORDINATES | GeometryArray.COLOR_3 );

            kYBar.setCoordinate( 0, new Point3d( m_fX0, m_fCenterY, 0.1 ) );
            kYBar.setColor( 0, m_kYSliceHairColor );
            kYBar.setCoordinate( 1, new Point3d( m_fCenterX - .10, m_fCenterY, 0.1 ) );
            kYBar.setColor( 1, m_kYSliceHairColor );

            kYBar.setCoordinate( 2, new Point3d( m_fCenterX + .10, m_fCenterY, 0.1 ) );
            kYBar.setColor( 2, m_kYSliceHairColor );
            kYBar.setCoordinate( 3, new Point3d( m_fX1, m_fCenterY, 0.1 ) );
            kYBar.setColor( 3, m_kYSliceHairColor );

            /* Create the Shape3D object to contain the LineArray: */
            Shape3D kYBarShape = new Shape3D();

            kYBarShape.addGeometry( kYBar );
            kYBarShape.setPickable( false );

            LineArray kXBar = new LineArray( 4, GeometryArray.COORDINATES | GeometryArray.COLOR_3 );

            kXBar.setCoordinate( 0, new Point3d( m_fCenterX, m_fY0, 0.1 ) );
            kXBar.setColor( 0, m_kXSliceHairColor );
            kXBar.setCoordinate( 1, new Point3d( m_fCenterX, m_fCenterY - 0.1, 0.1 ) );
            kXBar.setColor( 1, m_kXSliceHairColor );

            kXBar.setCoordinate( 2, new Point3d( m_fCenterX, m_fCenterY + .10, 0.1 ) );
            kXBar.setColor( 2, m_kXSliceHairColor );
            kXBar.setCoordinate( 3, new Point3d( m_fCenterX, m_fY1, 0.1 ) );
            kXBar.setColor( 3, m_kXSliceHairColor );

            /* Create the Shape3D object to contain the LineArray: */
            Shape3D kXBarShape = new Shape3D();

            kXBarShape.addGeometry( kXBar );
            kXBarShape.setPickable( false );

            kTransformGroup.addChild( kXBarShape );
            kTransformGroup.addChild( kYBarShape );

            kXBar = null;
            kXBarShape = null;
            kYBar = null;
            kYBarShape = null;
        }

        BranchGroup kBranchGroup = new BranchGroup();

        kBranchGroup.setCapability( BranchGroup.ALLOW_DETACH );
        kBranchGroup.addChild( kTransformGroup );
        kBranchGroup.compile();

        m_kTranslationTG.addChild( kBranchGroup );

        kBox = null;
        kBoxShape = null;
        kBoxTransformGroup = null;
        kBoxBranchGroup = null;
        kTransformGroup = null;
        kBranchGroup = null;
    }

    /**
     *  Initializes the Axis labels based on the ModelImage orientation. Axes
     * are displayed with 3D text objects and arrows drawn as polygons. They
     * are colored and labeled to match the axes they represent.
     */
    private void initAxes() {
        if ( m_kTextBranchGroup != null ) {
            m_kOrderedGroup.removeChild( m_kTextBranchGroup );
            m_kTextBranchGroup = null;
            m_kTextTransformGroup = null;
        }

        Text3D kXText = new Text3D( new Font3D( MipavUtil.courier12B, new FontExtrusion() ), m_kLabelX,
                new Point3f( 25f, 1f, 0f ) );

        Text3D kYText = new Text3D( new Font3D( MipavUtil.courier12B, new FontExtrusion() ), m_kLabelY,
                new Point3f( -2f, -25f, 0f ) );

        if ( m_bPatientOrientation && ( m_iPlane != 0 ) ) {
            kYText.setPosition( new Point3f( -2f, 31f, 0f ) );
        }

        QuadArray kXGeometry = new QuadArray( 4, QuadArray.COORDINATES | QuadArray.COLOR_3 );

        kXGeometry.setColor( 0, m_kXSliceHairColor );
        kXGeometry.setColor( 1, m_kXSliceHairColor );
        kXGeometry.setColor( 2, m_kXSliceHairColor );
        kXGeometry.setColor( 3, m_kXSliceHairColor );
        kXGeometry.setCoordinate( 0, new Point3d( 2f, 4f, 0.5f ) );
        kXGeometry.setCoordinate( 1, new Point3d( 18f, 4f, 0.5f ) );
        kXGeometry.setCoordinate( 2, new Point3d( 18f, 6f, 0.5f ) );
        kXGeometry.setCoordinate( 3, new Point3d( 2f, 6f, 0.5f ) );

        TriangleArray kXTri = new TriangleArray( 3, TriangleArray.COORDINATES | TriangleArray.COLOR_3 );

        kXTri.setColor( 0, m_kXSliceHairColor );
        kXTri.setColor( 1, m_kXSliceHairColor );
        kXTri.setColor( 2, m_kXSliceHairColor );
        kXTri.setCoordinate( 0, new Point3d( 18f, 9f, 0.5f ) );
        kXTri.setCoordinate( 1, new Point3d( 18f, 1f, 0.5f ) );
        kXTri.setCoordinate( 2, new Point3d( 23f, 5f, 0.5f ) );

        QuadArray kYGeometry = new QuadArray( 4, QuadArray.COORDINATES | QuadArray.COLOR_3 );

        kYGeometry.setColor( 0, m_kYSliceHairColor );
        kYGeometry.setColor( 1, m_kYSliceHairColor );
        kYGeometry.setColor( 2, m_kYSliceHairColor );
        kYGeometry.setColor( 3, m_kYSliceHairColor );
        if ( m_bPatientOrientation && ( m_iPlane != 0 ) ) {
            kYGeometry.setCoordinate( 0, new Point3d( 2f, 4f, 0.5f ) );
            kYGeometry.setCoordinate( 1, new Point3d( 0f, 4f, 0.5f ) );
            kYGeometry.setCoordinate( 2, new Point3d( 0f, 22f, 0.5f ) );
            kYGeometry.setCoordinate( 3, new Point3d( 2f, 22f, 0.5f ) );
        } else {
            kYGeometry.setCoordinate( 0, new Point3d( 2f, 6f, 0.5f ) );
            kYGeometry.setCoordinate( 1, new Point3d( 0f, 6f, 0.5f ) );
            kYGeometry.setCoordinate( 2, new Point3d( 0f, -12f, 0.5f ) );
            kYGeometry.setCoordinate( 3, new Point3d( 2f, -12f, 0.5f ) );
        }

        TriangleArray kYTri = new TriangleArray( 3, TriangleArray.COORDINATES | TriangleArray.COLOR_3 );

        kYTri.setColor( 0, m_kYSliceHairColor );
        kYTri.setColor( 1, m_kYSliceHairColor );
        kYTri.setColor( 2, m_kYSliceHairColor );
        if ( m_bPatientOrientation && ( m_iPlane != 0 ) ) {
            kYTri.setCoordinate( 0, new Point3d( 5f, 22f, 0.5f ) );
            kYTri.setCoordinate( 1, new Point3d( -4f, 22f, 0.5f ) );
            kYTri.setCoordinate( 2, new Point3d( 1f, 27f, 0.5f ) );
        } else {
            kYTri.setCoordinate( 0, new Point3d( 5f, -12f, 0.5f ) );
            kYTri.setCoordinate( 1, new Point3d( -4f, -12f, 0.5f ) );
            kYTri.setCoordinate( 2, new Point3d( 1f, -17f, 0.5f ) );
        }

        PolygonAttributes kPolygonAttributes = new PolygonAttributes();

        kPolygonAttributes.setCullFace( PolygonAttributes.CULL_NONE );

        Material kXMaterial = new Material( m_kXSliceHairColor, m_kXSliceHairColor, m_kXSliceHairColor,
                m_kXSliceHairColor, 1.0f );
        Material kYMaterial = new Material( m_kYSliceHairColor, m_kYSliceHairColor, m_kYSliceHairColor,
                m_kYSliceHairColor, 1.0f );

        Appearance kXAppearance = new Appearance();

        kXAppearance.setMaterial( kXMaterial );
        kXAppearance.setPolygonAttributes( kPolygonAttributes );

        Appearance kYAppearance = new Appearance();

        kYAppearance.setMaterial( kYMaterial );
        kYAppearance.setPolygonAttributes( kPolygonAttributes );

        Shape3D kXBox = new Shape3D( kXGeometry, kXAppearance );
        Shape3D kXArrows = new Shape3D( kXTri, kXAppearance );
        Shape3D kYBox = new Shape3D( kYGeometry, kYAppearance );
        Shape3D kYArrows = new Shape3D( kYTri, kYAppearance );
        Shape3D kXAxisLabel = new Shape3D( kXText, kXAppearance );
        Shape3D kYAxisLabel = new Shape3D( kYText, kYAppearance );

        float fXTrans = m_fX0 * 0.85f / m_fZoomScale;
        float fYTrans = m_fY1 * 0.85f / m_fZoomScale;

        if ( m_bPatientOrientation && ( m_iPlane != 0 ) ) {
            fYTrans = -fYTrans;
        }

        Transform3D kTextTransform = new Transform3D();

        kTextTransform.setScale( 0.01f / m_fZoomScale );
        kTextTransform.setTranslation( new Vector3f( fXTrans, fYTrans, 0.0f ) );
        m_kTextTransformGroup = new TransformGroup();
        m_kTextTransformGroup.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        m_kTextTransformGroup.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        m_kTextTransformGroup.setCapability( TransformGroup.ALLOW_CHILDREN_WRITE );
        m_kTextTransformGroup.setCapability( TransformGroup.ALLOW_CHILDREN_READ );
        m_kTextTransformGroup.setCapability( TransformGroup.ALLOW_CHILDREN_EXTEND );
        m_kTextTransformGroup.setTransform( kTextTransform );

        m_kTextTransformGroup.addChild( kXBox );
        m_kTextTransformGroup.addChild( kXArrows );
        m_kTextTransformGroup.addChild( kYBox );
        m_kTextTransformGroup.addChild( kYArrows );
        m_kTextTransformGroup.addChild( kXAxisLabel );
        m_kTextTransformGroup.addChild( kYAxisLabel );

        m_kTextBranchGroup = new BranchGroup();
        m_kTextBranchGroup.setCapability( BranchGroup.ALLOW_DETACH );
        m_kTextBranchGroup.addChild( m_kTextTransformGroup );
        m_kTextBranchGroup.compile();

        m_kOrderedGroup.addChild( m_kTextBranchGroup );

        /* Delete local variables: */
        kXText = null;
        kYText = null;
        kXGeometry = null;
        kXTri = null;
        kYGeometry = null;
        kYTri = null;
        kPolygonAttributes = null;
        kXMaterial = null;
        kYMaterial = null;
        kXAppearance = null;
        kYAppearance = null;

        kXBox = null;
        kXArrows = null;
        kYBox = null;
        kYArrows = null;
        kXAxisLabel = null;
        kYAxisLabel = null;
        kTextTransform = null;
    }

    /**
     * Given a point in ModelImage coordinates, draw it in the local
     * coordinates with a red sphere:
     * @param kPoint  RFA indicator point coordinate
     */
    public void drawRFAPoint( Point3f kPoint ) {

        /* Convert the point to local PlaneRender coordinates: */
        float[] fPoint = new float[3];

        fPoint[ 0 ] = kPoint.x;
        fPoint[ 1 ] = kPoint.y;
        fPoint[ 2 ] = kPoint.z;

        Point3f kRFAPoint = new Point3f();

        kRFAPoint.x = fPoint[ m_iXIndex ];
        kRFAPoint.y = fPoint[ m_iYIndex ];
        kRFAPoint.z = 0f;

        if ( m_bInvertX ) {
            kRFAPoint.x = 1 - kRFAPoint.x;
        }
        if ( !m_bInvertY ) {
            kRFAPoint.y = 1 - kRFAPoint.y;
        }

        kRFAPoint.x = kRFAPoint.x * m_fXRange + m_fX0;
        kRFAPoint.y = kRFAPoint.y * m_fYRange + m_fY0;

        /* If this is the first time drawing, create the BranchGroup to hold
         * the sphere representation: */
        if ( m_kRFA_BranchGroup == null ) {
            Shape3D kSphere = new Sphere( 0.025f ).getShape();

            kSphere.getAppearance().getMaterial().setEmissiveColor( new Color3f( 1f, 0f, 0f ) );
            kSphere.setPickable( false );

            Transform3D kTransform = new Transform3D();

            kTransform.set( new Vector3f( kRFAPoint ) );
            TransformGroup kTransformGroup = new TransformGroup();

            kTransformGroup.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
            kTransformGroup.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
            kTransformGroup.setTransform( kTransform );
            kTransformGroup.addChild( kSphere.cloneTree() );
            m_kRFA_BranchGroup = new BranchGroup();
            m_kRFA_BranchGroup.setCapability( BranchGroup.ALLOW_DETACH );
            m_kRFA_BranchGroup.setCapability( BranchGroup.ALLOW_CHILDREN_READ );
            m_kRFA_BranchGroup.setCapability( BranchGroup.ALLOW_CHILDREN_WRITE );
            m_kRFA_BranchGroup.addChild( kTransformGroup );
            m_kRFA_BranchGroup.compile();
            m_kOrderedGroup.addChild( m_kRFA_BranchGroup );
        } /* Otherwise, update the position of the existing sphere: */ else {
            Transform3D kTransform = new Transform3D();

            kTransform.set( new Vector3f( kRFAPoint ) );
            TransformGroup kTransformGroup = (TransformGroup) ( m_kRFA_BranchGroup.getChild( 0 ) );

            kTransformGroup.setTransform( kTransform );
        }

        /* delete temporary variables: */
        fPoint = null;
        kRFAPoint = null;
    }

    /**
     *  Based on the orientaion of the ModelImage, sets up the index
     * parameters, m_iXBound, m_iYBound, and m_iZBound, the drawing colors for
     * the z box, x and y bars, and the invert flags.
     *
     * Once setup everything is rendered into an x,y plane where x,y may be
     * any of the original x,y, or z dimensions in the original ModelImage.
     */
    private void setOrientation() {
        if ( m_iImageOrientation != FileInfoBase.UNKNOWN_ORIENT ) {
            for ( int iOrient = 0; iOrient < 3; iOrient++ ) {
                if ( m_aiOrientation[ iOrient ] == FileInfoBase.ORI_R2L_TYPE ) {
                    if ( ( m_iPlane == 0 ) || ( m_iPlane == 2 ) ) {
                        m_iXIndex = iOrient;
                    } else {
                        m_iZIndex = iOrient;
                    }
                } else if ( m_aiOrientation[ iOrient ] == FileInfoBase.ORI_L2R_TYPE ) {
                    if ( ( m_iPlane == 0 ) || ( m_iPlane == 2 ) ) {
                        m_iXIndex = iOrient;
                        m_bInvertX = true;
                    } else {
                        m_iZIndex = iOrient;
                        m_bInvertZ = true;
                    }

                } else if ( m_aiOrientation[ iOrient ] == FileInfoBase.ORI_A2P_TYPE ) {
                    if ( m_iPlane == 0 ) {
                        m_iYIndex = iOrient;
                    } else if ( m_iPlane == 1 ) {
                        m_iXIndex = iOrient;
                    } else {
                        m_iZIndex = iOrient;
                    }
                } else if ( m_aiOrientation[ iOrient ] == FileInfoBase.ORI_P2A_TYPE ) {
                    if ( m_iPlane == 0 ) {
                        m_iYIndex = iOrient;
                        m_bInvertY = true;
                    } else if ( m_iPlane == 1 ) {
                        m_iXIndex = iOrient;
                        m_bInvertX = true;
                    } else {
                        m_iZIndex = iOrient;
                        m_bInvertZ = true;
                    }
                } else if ( m_aiOrientation[ iOrient ] == FileInfoBase.ORI_S2I_TYPE ) {
                    if ( m_iPlane == 0 ) {
                        m_iZIndex = iOrient;
                        m_bInvertZ = true;
                    } else {
                        m_iYIndex = iOrient;
                    }
                } else if ( m_aiOrientation[ iOrient ] == FileInfoBase.ORI_I2S_TYPE ) {
                    if ( m_iPlane == 0 ) {
                        m_iZIndex = iOrient;
                    } else {
                        m_iYIndex = iOrient;
                        m_bInvertY = true;
                    }
                }
            }
        } else if ( m_iPlane == 1 ) {
            m_iXIndex = 2;
            m_iZIndex = 0;
        } else if ( m_iPlane == 2 ) {
            m_iYIndex = 2;
            m_iZIndex = 1;
        }

        m_iXBound = m_kImageA.getExtents()[ m_iXIndex ];
        m_iYBound = m_kImageA.getExtents()[ m_iYIndex ];
        m_iZBound = m_kImageA.getExtents()[ m_iZIndex ];
        m_iQuantity = m_iXBound * m_iYBound * m_iZBound;

        float[] afResolutions = new float[3];

        afResolutions[0] = Math.abs( m_kImageA.getFileInfo()[0].getResolutions()[ m_iXIndex ] );
        afResolutions[1] = Math.abs( m_kImageA.getFileInfo()[0].getResolutions()[ m_iYIndex ] );
        afResolutions[2] = Math.abs( m_kImageA.getFileInfo()[0].getResolutions()[ m_iZIndex ] );

        /* if the slice spacing value is greater than the z-res, use the slice spacing instead */
        if ( afResolutions[2] < m_kImageA.getFileInfo( 0 ).getSliceSpacing() ) {
            afResolutions[2] = m_kImageA.getFileInfo( 0 ).getSliceSpacing();
        }

        if ( ( afResolutions[0] == 0.0f ) || ( afResolutions[1] == 0.0f ) || ( afResolutions[2] == 0.0f ) ) {
            afResolutions[0] = 1.0f;
            afResolutions[1] = 1.0f;
            afResolutions[2] = 1.0f;
        }

        m_fXBox = (float) ( m_iXBound - 1 ) * afResolutions[0];
        m_fYBox = (float) ( m_iYBound - 1 ) * afResolutions[1];

        m_fMaxBox = m_fXBox;
        if ( m_fYBox > m_fMaxBox ) {
            m_fMaxBox = m_fYBox;
        }

        m_kXSliceHairColor = m_akXColors[ m_iPlane ];
        m_kYSliceHairColor = m_akYColors[ m_iPlane ];
        m_kZSliceHairColor = m_akZColors[ m_iPlane ];

        if ( ( m_iPlane == 1 ) && ( m_iImageOrientation == FileInfoBase.UNKNOWN_ORIENT ) ) {
            m_kXSliceHairColor = new Color3f( 0, 0, 1 );
            m_kYSliceHairColor = new Color3f( 0, 1, 0 );
        }

        m_iXFactor = m_aiBuffFactors[ m_iXIndex ];
        m_iYFactor = m_aiBuffFactors[ m_iYIndex ];
        m_iZFactor = m_aiBuffFactors[ m_iZIndex ];

        if ( m_iImageOrientation == FileInfoBase.UNKNOWN_ORIENT ) {
            m_bPatientOrientation = false;
            if ( ( m_iPlane == 0 ) || ( m_iPlane == 2 ) ) {
                m_kLabelX = new String( "X" );
            } else {
                m_kLabelX = new String( "Z" );
            }
            if ( ( m_iPlane == 0 ) || ( m_iPlane == 1 ) ) {
                m_kLabelY = new String( "Y" );
            } else {
                m_kLabelY = new String( "Z" );
            }
        } else {
            if ( ( m_iPlane == 0 ) || ( m_iPlane == 2 ) ) {
                m_kLabelX = new String( "L" );
            } else {
                m_kLabelX = new String( "P" );
            }
            if ( ( m_iPlane == 1 ) || ( m_iPlane == 2 ) ) {
                m_kLabelY = new String( "S" );
            } else {
                m_kLabelY = new String( "P" );
            }
        }

        afResolutions = null;
    }

    /** Stores the ModelImage data as an array of texture maps, with LUT or RGBA color lookup: */
    private void writeTextures() {
        if ( m_bColorMap_initialized == false ) {
            m_bColorMap_initialized = true;
            initColorMaps();
        }

        m_akImageComponent = new ImageComponent2D[ m_iZBound ];

        int iPixColor = 0;
        TransferFunction tf_imgA = null;
        TransferFunction tf_imgB = null;

        if ( m_kLUTa != null ) {
            tf_imgA = m_kLUTa.getTransferFunction();
        }

        if ( ( m_kImageB != null ) && ( m_kLUTb != null ) ) {
            tf_imgB = m_kLUTb.getTransferFunction();
        }

        for ( int iZ = 0; iZ < m_iZBound; iZ++ ) {
            BufferedImage kBImage = new BufferedImage( m_iXBound, m_iYBound, BufferedImage.TYPE_INT_ARGB );

            for ( int iX = 0; iX < m_iXBound; iX++ ) {
                for ( int iY = 0; iY < m_iYBound; iY++ ) {
                    int iXIndex = iX;
                    int iYIndex = iY;
                    int iZIndex = iZ;

                    if ( m_bInvertX ) {
                        iXIndex = ( m_iXBound - 1 ) - iX;
                    }
                    if ( m_bInvertY ) {
                        iYIndex = ( m_iYBound - 1 ) - iY;
                    }
                    if ( m_bInvertZ ) {
                        iZIndex = ( m_iZBound - 1 ) - iZ;
                    }
                    int iIndex = ( iZIndex * m_iZFactor ) + ( iYIndex * m_iYFactor ) + ( iXIndex * m_iXFactor );

                    if ( m_kLUTa != null ) {
                        float fValue = m_kImageA.getFloat( iIndex );

                        iPixColor = (int) ( tf_imgA.getRemappedValue( fValue, 256 ) + 0.5f );
                        int iColor = m_aiLutBufferRemappedA[ iPixColor ];

                        if ( ( m_kImageB != null ) && ( m_kLUTb != null ) ) {
                            fValue = m_kImageB.getFloat( iIndex );
                            iPixColor = (int) ( tf_imgB.getRemappedValue( fValue, 256 ) + 0.5f );
                            int iColorB = m_aiLutBufferRemappedB[ iPixColor ];

                            iColor = blend( iColor, iColorB );
                        }
                        kBImage.setRGB( iX, iY, iColor );
                    } else if ( m_kRGBTA != null ) {
                        int iNewColor = getRGBColora( iIndex );

                        if ( ( m_kImageB != null ) && ( m_kRGBTB != null ) ) {
                            iNewColor = blend( iNewColor, getRGBColorb( iIndex ) );
                        }
                        kBImage.setRGB( iX, iY, iNewColor );
                    } else {
                        int iNewColor = m_kImageA.getPackedColor( iIndex );

                        if ( m_kImageB != null ) {
                            iNewColor = blend( iNewColor, m_kImageB.getPackedColor( iIndex ) );
                        }
                        kBImage.setRGB( iX, iY, iNewColor );
                    }
                }
            }
            m_akImageComponent[ iZ ] = new ImageComponent2D( ImageComponent.FORMAT_RGBA, m_iXBound, m_iYBound );
            m_akImageComponent[ iZ ].setCapability( ImageComponent2D.ALLOW_IMAGE_WRITE );
            m_akImageComponent[ iZ ].setCapability( ImageComponent2D.ALLOW_IMAGE_READ );
            m_akImageComponent[ iZ ].setCapability( ImageComponent2D.ALLOW_SIZE_READ );
            m_akImageComponent[ iZ ].setCapability( ImageComponent2D.ALLOW_FORMAT_READ );
            m_akImageComponent[ iZ ].set( kBImage );

            kBImage = null;
        }

        /* Setup the texture map: */
        m_kDisplayedImage.set( m_akImageComponent[ m_iSlice ].getImage() );
    }

    /**
     *  Stores the ModelImage data as a single texture map, with LUT or RGBA color lookup:
     */
    private void writeTexture() {
        initColorMaps();

        int iZ = m_iSlice;

        if ( kBImage == null ) {
            kBImage = new BufferedImage( m_iXBound, m_iYBound, BufferedImage.TYPE_INT_ARGB );
        }

        TransferFunction tf_imgA = null;
        TransferFunction tf_imgB = null;

        if ( m_kLUTa != null ) {
            tf_imgA = m_kLUTa.getTransferFunction();
        }

        if ( ( m_kImageB != null ) && ( m_kLUTb != null ) ) {
            tf_imgB = m_kLUTb.getTransferFunction();
        }
        int iPixColor = 0;

        for ( int iX = 0; iX < m_iXBound; iX++ ) {
            for ( int iY = 0; iY < m_iYBound; iY++ ) {
                int iXIndex = iX;
                int iYIndex = iY;
                int iZIndex = iZ;

                if ( m_bInvertX ) {
                    iXIndex = ( m_iXBound - 1 ) - iX;
                }
                if ( m_bInvertY ) {
                    iYIndex = ( m_iYBound - 1 ) - iY;
                }
                if ( m_bInvertZ ) {
                    iZIndex = ( m_iZBound - 1 ) - iZ;
                }
                int iIndex = ( iZIndex * m_iZFactor ) + ( iYIndex * m_iYFactor ) + ( iXIndex * m_iXFactor );

                if ( m_kLUTa != null ) {
                    float fValue = m_kImageA.getFloat( iIndex );

                    iPixColor = (int) ( tf_imgA.getRemappedValue( fValue, 256 ) + 0.5f );
                    int iColor = m_aiLutBufferRemappedA[ iPixColor ];

                    if ( ( m_kImageB != null ) && ( m_kLUTb != null ) ) {
                        fValue = m_kImageB.getFloat( iIndex );
                        iPixColor = (int) ( tf_imgB.getRemappedValue( fValue, 256 ) + 0.5f );
                        int iColorB = m_aiLutBufferRemappedB[iPixColor];

                        iColor = blend( iColor, iColorB );
                    }
                    kBImage.setRGB( iX, iY, iColor );
                } else if ( m_kRGBTA != null ) {
                    int iNewColor = getRGBColora( iIndex );

                    if ( ( m_kImageB != null ) && ( m_kRGBTB != null ) ) {
                        iNewColor = blend( iNewColor, getRGBColorb( iIndex ) );
                    }
                    kBImage.setRGB( iX, iY, iNewColor );
                } else {
                    int iNewColor = m_kImageA.getPackedColor( iIndex );

                    if ( m_kImageB != null ) {
                        iNewColor = blend( iNewColor, m_kImageB.getPackedColor( iIndex ) );
                    }
                    kBImage.setRGB( iX, iY, iNewColor );
                }

                int iSurfaceColor = 0;

                for ( int iMask = 0; iMask < m_kMaskVector.size(); iMask++ ) {
                    BitSet kMaskSet = (BitSet) m_kMaskVector.elementAt( iMask );

                    if ( kMaskSet != null && kMaskSet.get( iIndex ) ) {
                        iSurfaceColor = ( (Color) m_kColorVector.elementAt( iMask ) ).getRGB();

                        Color3f[] kMaskColors = (Color3f[]) m_kMaskColorVector.elementAt( iMask );
                        Color3f kColor = kMaskColors[iIndex];

                        if ( kColor != null ) {
                            iSurfaceColor = kColor.get().getRGB();
                        }
                        kBImage.setRGB( iX, iY, iSurfaceColor );
                    }
                }
            }
        }

        m_kDisplayedImage.set( kBImage );
    }

    /**
     * This function is called when one of the colormaps changes, it
     * initializes the colormap data structures.
     */
    private void initColorMaps() {
        if ( ( m_kImageA != null ) && ( m_kRGBTA != null ) ) {
            initRGBa();
        }
        if ( ( m_kImageA != null ) && ( m_kLUTa != null ) ) {
            initLUTa();
        }
        if ( ( m_kImageB != null ) && ( m_kRGBTB != null ) ) {
            initRGBb();
        }
        if ( ( m_kImageB != null ) && ( m_kLUTb != null ) ) {
            initLUTb();
        }
    }

    /**
     *  Called when the RGB colortable changes for ImageA, intializes the
     *  colormap variables for ImageA and stores them.
     */
    private void initRGBa() {
        m_RGBIndexBufferA = m_kRGBTA.exportIndexedRGB();

        if ( m_kImageA.getMinR() < 0.0 ) {
            m_maxColorA = (float) ( m_kImageA.getMaxR() - m_kImageA.getMinR() );
            m_offsetAR = (float) ( -m_kImageA.getMinR() );
        } else {
            m_maxColorA = (float) m_kImageA.getMaxR();
        }
        if ( m_kImageA.getMinG() < 0.0 ) {
            m_maxColorA = Math.max( (float) ( m_kImageA.getMaxG() - m_kImageA.getMinG() ), m_maxColorA );
            m_offsetAG = (float) ( -m_kImageA.getMinG() );
        } else {
            m_maxColorA = Math.max( (float) m_kImageA.getMaxG(), m_maxColorA );
        }
        if ( m_kImageA.getMinB() < 0.0 ) {
            m_maxColorA = Math.max( (float) ( m_kImageA.getMaxB() - m_kImageA.getMinB() ), m_maxColorA );
            m_offsetAB = (float) ( -m_kImageA.getMinB() );
        } else {
            m_maxColorA = Math.max( (float) m_kImageA.getMaxB(), m_maxColorA );
        }
        m_normColorA = 255 / m_maxColorA;

    }

    /**
     * Called when the RGB colortable changes for ImageB, intializes the
     * colormap variables for ImageB and stores them.
     */
    private void initRGBb() {
        m_RGBIndexBufferB = m_kRGBTB.exportIndexedRGB();

        if ( m_kImageB.getMinR() < 0.0 ) {
            m_maxColorB = (float) ( m_kImageB.getMaxR() - m_kImageB.getMinR() );
            m_offsetBR = (float) ( -m_kImageB.getMinR() );
        } else {
            m_maxColorB = (float) m_kImageB.getMaxR();
        }
        if ( m_kImageB.getMinG() < 0.0 ) {
            m_maxColorB = Math.max( (float) ( m_kImageB.getMaxG() - m_kImageB.getMinG() ), m_maxColorB );
            m_offsetBG = (float) ( -m_kImageB.getMinG() );
        } else {
            m_maxColorB = Math.max( (float) m_kImageB.getMaxG(), m_maxColorB );
        }
        if ( m_kImageB.getMinB() < 0.0 ) {
            m_maxColorB = Math.max( (float) ( m_kImageB.getMaxB() - m_kImageB.getMinB() ), m_maxColorB );
            m_offsetBB = (float) ( -m_kImageB.getMinB() );
        } else {
            m_maxColorB = Math.max( (float) m_kImageB.getMaxB(), m_maxColorB );
        }
        m_normColorB = 255 / m_maxColorB;

    }

    /**
     *  Called when the LUT color lookup table changes for m_kImageA,
     *  intializes the colormap variables for m_kImageA and stores them.
     */
    private void initLUTa() {
        m_iLutHeightA = m_kLUTa.getExtents()[1];
        m_aiLutBufferRemappedA = new int[ m_iLutHeightA ];
        m_kLUTa.exportIndexedLUT( m_aiLutBufferRemappedA );
    }

    /**
     * Called when the LUT color lookup table changes for m_kImageB,
     * intializes the colormap variables for m_kImageB and stores them.
     */
    private void initLUTb() {
        m_iLutHeightB = m_kLUTb.getExtents()[1];
        m_aiLutBufferRemappedB = new int[ m_iLutHeightB ];
        m_kLUTb.exportIndexedLUT( m_aiLutBufferRemappedB );
    }

    /**
     *  Gets the RGB color value for the input float value for m_kImageA and m_kRGBTA
     *  @param iIndex
     *  @return
     */
    private final int getRGBColora( int iIndex ) {
        float redMapped = 0;
        float greenMapped = 0;
        float blueMapped = 0;

        if ( m_kRGBTA.getROn() ) {
            redMapped = ( m_RGBIndexBufferA[ (int)
                    ( ( m_kImageA.getFloat( 4 * iIndex + 1 ) + m_offsetAR ) * m_normColorA )] & 0x00ff0000 ) >> 16;
        } else {
            redMapped = 0;
        }
        if ( m_kRGBTA.getGOn() ) {
            greenMapped = ( m_RGBIndexBufferA[ (int)
                    ( ( m_kImageA.getFloat( 4 * iIndex + 2 ) + m_offsetAG ) * m_normColorA )] & 0x0000ff00 ) >> 8;
        } else {
            greenMapped = 0;
        }
        if ( m_kRGBTA.getBOn() ) {
            blueMapped = ( m_RGBIndexBufferA[ (int)
                    ( ( m_kImageA.getFloat( 4 * iIndex + 3 ) + m_offsetAB ) * m_normColorA )] & 0x000000ff );
        } else {
            blueMapped = 0;
        }

        int iRed = (int) redMapped;
        int iGreen = (int) greenMapped;
        int iBlue = (int) blueMapped;

        iRed = iRed << 16;
        iGreen = iGreen << 8;
        int iNewColor = ( iRed & 0x00ff0000 ) | ( iGreen & 0x0000ff00 ) | ( iBlue & 0x000000ff );

        iNewColor = ( iNewColor | 0xff000000 );
        return iNewColor;
    }

    /**
     * Gets the RGB color value for the input float value for ImageB and RGBTB.
     * @param  pixel index.
     * @return  corresponding RGB color
     */
    private final int getRGBColorb( int iIndex ) {
        float redMapped = 0;
        float greenMapped = 0;
        float blueMapped = 0;

        if ( m_kRGBTB.getROn() ) {
            redMapped = ( m_RGBIndexBufferB[ (int)
                    ( ( m_kImageB.getFloat( 4 * iIndex + 1 ) + m_offsetBR ) * m_normColorB )] & 0x00ff0000 ) >> 16;
        } else {
            redMapped = 0;
        }
        if ( m_kRGBTB.getGOn() ) {
            greenMapped = ( m_RGBIndexBufferB[ (int)
                    ( ( m_kImageB.getFloat( 4 * iIndex + 2 ) + m_offsetBG ) * m_normColorB )] & 0x0000ff00 ) >> 8;
        } else {
            greenMapped = 0;
        }
        if ( m_kRGBTB.getBOn() ) {
            blueMapped = ( m_RGBIndexBufferB[ (int)
                    ( ( m_kImageB.getFloat( 4 * iIndex + 3 ) + m_offsetBB ) * m_normColorB )] & 0x000000ff );
        } else {
            blueMapped = 0;
        }

        int iRed = (int) redMapped;
        int iGreen = (int) greenMapped;
        int iBlue = (int) blueMapped;

        iRed = iRed << 16;
        iGreen = iGreen << 8;
        int iNewColor = ( iRed & 0x00ff0000 ) | ( iGreen & 0x0000ff00 ) | ( iBlue & 0x000000ff );

        iNewColor = ( iNewColor | 0xff000000 );
        return iNewColor;
    }


    /**
     * Blends between two packed int colors. First separates the RGB
     * components, then multiplies each by opacity value from the parent's
     * blending slider and adds them together individually,
     * the final result is an RGB packed int.
     * @param iColor1  pixel color from imageA.
     * @param iColor2  pixel color from imageB.
     * @return blended pixel color
     */
    private final int blend( int iColor1, int iColor2 ) {
        float alphaBlend = ( 100.0f - (float) m_kParent.getBlendValue() ) / 100.0f;
        float alphaPrime = 1 - alphaBlend;

        int iColor1Red = ( iColor1 >> 16 & 0xFF );
        int iColor1Green = ( iColor1 >> 8 & 0xFF );
        int iColor1Blue = ( iColor1 & 0xFF );

        iColor1Red = (int) ( ( (float) iColor1Red ) * alphaBlend );
        iColor1Green = (int) ( ( (float) iColor1Green ) * alphaBlend );
        iColor1Blue = (int) ( ( (float) iColor1Blue ) * alphaBlend );

        int iColor2Red = ( iColor2 >> 16 & 0xFF );
        int iColor2Green = ( iColor2 >> 8 & 0xFF );
        int iColor2Blue = ( iColor2 & 0xFF );

        iColor2Red = (int) ( ( (float) iColor2Red ) * alphaPrime );
        iColor2Green = (int) ( ( (float) iColor2Green ) * alphaPrime );
        iColor2Blue = (int) ( ( (float) iColor2Blue ) * alphaPrime );

        int iRed = ( iColor1Red + iColor2Red );
        int iGreen = ( iColor1Green + iColor2Green );
        int iBlue = ( iColor1Blue + iColor2Blue );

        iRed = iRed << 16;
        iGreen = iGreen << 8;
        int iNewColor = ( iRed & 0x00ff0000 ) | ( iGreen & 0x0000ff00 ) | ( iBlue & 0x000000ff );

        iNewColor = ( iNewColor | 0xff000000 );
        return iNewColor;
    }

    /**
     *  Enable or disable target point for the RFA probe from within the plane renderer:
     *  @param bEnable  true enable target point, false not.
     */
    public void enableTargetPointPicking( boolean bEnable ) {
        if ( m_bEntryPointSelect == bEnable ) {
            return;
        }
        m_bEntryPointSelect = bEnable;

        /* If point selection is disabled, then detach the display group: */
        if ( !bEnable && ( m_kRFA_BranchGroup != null ) ) {
            m_kOrderedGroup.removeChild( m_kRFA_BranchGroup );
        } /* If point selection is enabled, then draw the display group: */ else if ( bEnable
                && ( m_kRFA_BranchGroup != null ) ) {
            m_kOrderedGroup.addChild( m_kRFA_BranchGroup );
        }
    }

    /** Data members for the PlaneRender class: */

    /** Reference to the parent frame: */
    private ViewJFrameVolumeView m_kParent;

    /** Current image A. */
    private ModelImage m_kImageA;

    /** Current image B. */
    private ModelImage m_kImageB;

    /** Current active image for manipulating the LUT by dragging with the
     * right-mouse down. */
    private ModelImage m_kActiveImage;

    /** Current active LUT for manipulating the LUT by dragging with the
     * right-mouse down. */
    private ModelLUT m_kActiveLUT;

    /** Reference to LUT for image A */
    private ModelLUT m_kLUTa;

    /** RGB table for image A. */
    private ModelRGB m_kRGBTA;

    /** Reference to LUT for image B */
    private ModelLUT m_kLUTb;

    /** RGB table for image B. */
    private ModelRGB m_kRGBTB;

    /** Which dimension of the ModelImage to render */
    private int m_iPlane = 0;

    /** Group dictating how the plane is translated. */
    private TransformGroup m_kTranslationTG;
    private Transform3D m_kCurrentTransform;

    /** Numbers dicatating the size of the plane based on the extents and
     * resolutions of the image. */
    private float m_fXBox, m_fYBox, m_fMaxBox;

    /** Dimensions of image A. */
    private int m_iXBound, m_iYBound, m_iZBound;

    /** Image component 2D. Used to store the ModelImage data as textures. */
    private ImageComponent2D[] m_akImageComponent;

    /** The current displayed texture, based on the value of m_iSlice */
    private ImageComponent2D m_kDisplayedImage;

    /** Whether to store all the data in ImageComponent2D array or not: */
    private boolean m_bMemoryUsage;

    /** Which slice is currently displayed in the XY plane. */
    private int m_iSlice;

    /** The 2D texture for the texture-mapped polygon. */
    private Texture2D m_kTexture;

    /** Mouse zoom behavior. */
    private MouseZoomBehavior m_kMouseZoom;

    /** Mouse translate behavior. */
    private MouseTranslation m_kMouseTranslate;

    /** True when the left mouse has been pressed, set to false when the left
     * mouse button is released */
    private boolean m_bLeftMousePressed = false;
    private boolean m_bRightMousePressed = false;
    private boolean m_bFirstRightDrag = true;

    /**
     *  The SimpleUniverse object which is the parent of everything else in the scene.
     */
    private SimpleUniverse m_kUniverse;

    /**
     *  The Canvas3D object on which the plane is drawn.
     */
    private VolumeCanvas3D m_kCanvas;

    /** Canvas width .*/
    private int m_iCanvasWidth;

    /** Canvas height. */
    private int m_iCanvasHeight;

    /** Canvas width in half. */
    private float m_fDivX;

    /** Canvas height in half. */
    private float m_fDivY;

    /** X direction mouse translation. */
    private float m_fXTranslate = 0.0f;

    /** Y direction mouse translatioin. */
    private float m_fYTranslate = 0.0f;

    /**
     *	The BranchGroup root of the scene managed by the simple
     *	universe.  The root has a single child, a TransformGroup,
     *	that manages all of the actual objects.
     */
    private BranchGroup m_kObjRootBG;

    /**
     * Root order group of the image scene graph.
     */
    private OrderedGroup m_kOrderedGroup;

    /** The config used to extends the Canvas3D class. */
    private GraphicsConfiguration m_kConfig;

    /* Set of colors used to draw the X and Y Bars and the Z box: */
    private Color3f[] m_akXColors = { new Color3f( 1, 1, 0 ), new Color3f( 0, 1, 0 ), new Color3f( 1, 1, 0 ) };

    private Color3f[] m_akYColors = { new Color3f( 0, 1, 0 ), new Color3f( 1, 0, 0 ), new Color3f( 1, 0, 0 ) };

    private Color3f[] m_akZColors = { new Color3f( 1, 0, 0 ), new Color3f( 1, 1, 0 ), new Color3f( 0, 1, 0 ) };

    /* Mapping of colors to actual x,y,z */
    private Color3f m_kXSliceHairColor;
    private Color3f m_kYSliceHairColor;
    private Color3f m_kZSliceHairColor;

    /* The center of the X,Y bar cross hairs, in plane coordinates: */
    private float m_fCenterX, m_fCenterY;

    /* The plane coordinate x,y dimensions: */
    private float m_fX0;
    private float m_fY0;
    private float m_fX1;
    private float m_fY1;
    private float m_fXRange;
    private float m_fYRange;

    /** Radialogical orientation of the image: */
    private int[] m_aiOrientation;

    /** Determines whether the image data is inverted when displayed in the
     * texture: */
    private boolean m_bInvertX = false;
    private boolean m_bInvertY = false;
    private boolean m_bInvertZ = false;

    /** The image dimensions in x,y,z: */
    private int[] m_aiBuffFactors;

    /** Mapping of image dimensions to actual x,y,z*/
    private int m_iXFactor;
    private int m_iYFactor;
    private int m_iZFactor;

    /** Mapping of Image index to actual x,y,z index */
    private int m_iXIndex = 0;
    private int m_iYIndex = 1;
    private int m_iZIndex = 2;

    /** Turns on drawing of the X,Y bars and the Axis labels: */
    private boolean m_bDrawXHairs = true;
    private boolean m_bDrawAxes = true;

    private String m_kLabelX = new String( "X" );
    private String m_kLabelY = new String( "Y" );

    /** Image orientation .*/
    private int m_iImageOrientation = 3;

    /** Actual image orietation. */
    private boolean m_bPatientOrientation = true;

    /** Image scaling from Zoom: */
    private float m_fZoomScale = 1.0f;

    /**
     * Transform group to hold the texture images.
     */
    private TransformGroup m_kTextTransformGroup;

    /**
     * Branch group to hold the texture image.
     */
    private BranchGroup m_kTextBranchGroup;

    /**
     * number of surface in the surface list.
     */
    private int m_iNumChildren = 0;

    /** Image size */
    protected int m_iQuantity;

    /** Surface color vector. */
    private Vector m_kColorVector = new Vector();

    /** Surface mask vector. */
    private Vector m_kMaskVector = new Vector();

    /** Surface mask color vector. */
    private Vector m_kMaskColorVector = new Vector();

    /** min and max float values for m_kImageA and m_kImageB: */
    private float m_fMinA = Float.MAX_VALUE;
    private float m_fMaxA = Float.MIN_VALUE;
    private float m_fMinB = Float.MAX_VALUE;
    private float m_fMaxB = Float.MIN_VALUE;
    private float m_fMin = Float.MAX_VALUE;
    private float m_fMax = Float.MIN_VALUE;

    /** Colormap, these store the information for the imageA and imageB
     * colormaps, they are initialized when the colormap changes: */
    private boolean m_bColorMap_initialized = false;

    /** Height of LUTA. */
    private int m_iLutHeightA = 0;

    /** imageA remapped buffer. */
    private int[] m_aiLutBufferRemappedA = null;

    /** Height of LUTB. */
    private int m_iLutHeightB = 0;

    /** imageB remapped buffer. */
    private int[] m_aiLutBufferRemappedB = null;

    /** Max color value for imageA. */
    private float m_maxColorA = 255;

    /** normalized color value for imageA. */
    private float m_normColorA = 1;

    /** RGB Offset color value for imageA. */
    private float m_offsetAR = 0.0f;
    private float m_offsetAG = 0.0f;
    private float m_offsetAB = 0.0f;

    /** ImageA RGB image buffer reference. */
    private int[] m_RGBIndexBufferA = new int[256];

    /** Max color value for imageB. */
    private float m_maxColorB = 255;

    /** normalized color value for imageB. */
    private float m_normColorB = 1;

    /** RGB Offset color value for imageB. */
    private float m_offsetBR = 0.0f;
    private float m_offsetBG = 0.0f;
    private float m_offsetBB = 0.0f;

    /** ImageB RGB image buffer reference. */
    private int[] m_RGBIndexBufferB = new int[256];

    /** number of surfaces. */
    private int m_iBranchSurface = -1;

    /* Boolean to turn on/off the RFA probe entry point selection with
     * mouse: */
    private boolean m_bEntryPointSelect = false;

    /** Branch group for the RFA indicator point. */
    private BranchGroup m_kRFA_BranchGroup = null;

    /** painted image. */
    private BufferedImage kBImage = null;

    /** Member variables used to adjust the winow and level (contrast and
     * bringtness) by dragging with the right-mouse button: */
    private float[] m_afXWin = new float[4];
    private float[] m_afYWin = new float[4];
    private float m_fWindow;
    private float m_fLevel;
    private float m_fOldX;
    private float m_fOldY;
}
