package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;

import javax.swing.*;
import javax.vecmath.*;

import javax.media.j3d.*;
import javax.media.j3d.Material;
import com.sun.j3d.utils.universe.SimpleUniverse;

/**
 * JFrameRegistrationMosaic is a window/gui that enables the user to manually
 * align two images with the mouse and then call the OAR 2D registration
 * algorithm to create a mosaic image with the two aligned images. Multiple
 * images can be added to the mosaic and aligned one at a time.
 */
public class JFrameRegistrationMosaic extends JFrame
    implements ActionListener,      /* Button events */
               MouseListener,       /* Mouse press and release events */
               MouseMotionListener  /* Mouse drag events*/
{
    /**
     *   Creates new window for manual (mouse-based) registration of two
     *   images.
     */
    public JFrameRegistrationMosaic()
    {
        super("Mosaic Registration");
        init();
    }

    /** 
     * Removes member variables.
     */
    public void dispose()
    {
        closeAllImages();
        super.dispose();
    }

    /**
     *	Initializes GUI toolbar and buttons and displays the registration
     *	window.
     */
    private void init() {

        /* ToolBar for displaying the buttons for the GUI: */
        JToolBar kToolBar = new JToolBar();

        /* The reference image open button: */
        m_kOpenReferenceButton = new JButton( "Open Reference Image" );
        m_kOpenReferenceButton.setActionCommand( "OpenReference" );
        m_kOpenReferenceButton.addActionListener( this );
        kToolBar.add( m_kOpenReferenceButton );

        /* The registration image open button: */
        m_kOpenTileButton = new JButton( "Add tile image" );
        m_kOpenTileButton.setActionCommand( "OpenTile" );
        m_kOpenTileButton.addActionListener( this );
        m_kOpenTileButton.setEnabled( false );
        kToolBar.add( m_kOpenTileButton );

        /* Toggle which image is selected and manipulated by the mouse: */
        m_kToggleSelectedButton = new JButton( "Toggle Selected Image" );
        m_kToggleSelectedButton.setActionCommand( "ToggleSelected" );
        m_kToggleSelectedButton.addActionListener( this );
        m_kToggleSelectedButton.setEnabled( false );
        kToolBar.add( m_kToggleSelectedButton );

        /* Call the registration algorithm, using the user-manipulated
         * transformation as the initial guess: */
        m_kRegisterButton = new JButton( "Register Images" );
        m_kRegisterButton.setActionCommand( "Register" );
        m_kRegisterButton.addActionListener( this );
        m_kRegisterButton.setEnabled( false );
        kToolBar.add( m_kRegisterButton );

        /* Save button - saves the new mosaic image */
        m_kSaveButton = new JButton( "Save Mosaic" );
        m_kSaveButton.setActionCommand( "SaveMosaic" );
        m_kSaveButton.addActionListener( this );
        m_kSaveButton.setEnabled( false );
        kToolBar.add( m_kSaveButton );

        m_kCloseAllButton = new JButton( "Close All" );
        m_kCloseAllButton.setActionCommand( "CloseAll" );
        m_kCloseAllButton.addActionListener( this );
        m_kCloseAllButton.setEnabled( false );
        kToolBar.add( m_kCloseAllButton );

        /* Add the toolbar to the display: */
        kToolBar.validate();
        kToolBar.setVisible( true );
        getContentPane().add( kToolBar, BorderLayout.NORTH );

        /* Create a new canvas and add it to the display: */
        JPanel displayPanel = new JPanel( new BorderLayout() );
        m_kCanvas = createCanvas( displayPanel );
        getContentPane().add( displayPanel, BorderLayout.CENTER );

        /* Display the window: */
        pack();
        setVisible(true);
        setSize( Toolkit.getDefaultToolkit().getScreenSize().width,
                 Toolkit.getDefaultToolkit().getScreenSize().height );
    }

    /**
     * JButton events:
     *    @param event button event
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();
       
        if ( command.equals( "OpenReference" ) )
        {
            /* Open and store the reference image: */
            createMosaicOpenDialog();
           
            m_kOpenReferenceButton.setEnabled( false );
            m_kOpenTileButton.setEnabled( true );
            m_kCloseAllButton.setEnabled( true );
        }
        else if ( command.equals( "OpenTile" ) )
        {
            /* Open and store the tile image: */
            createMosaicOpenDialog();
            m_kOpenTileButton.setEnabled( false );
            m_kToggleSelectedButton.setEnabled( true );
            m_kRegisterButton.setEnabled( true );
        }
        else if ( command.equals( "ToggleSelected" ) )
        {
            /* Toggle which image is selected -- which image the user
             * manipulates with the mouse: */
            toggleSelectedImage();
        }
        else if ( command.equals( "Register" ) )
        {
            /* Call the registration algorithm */
            registerImages();
            m_kSaveButton.setEnabled( true );
            m_kOpenTileButton.setEnabled( true );
        }
        else if ( command.equals( "SaveMosaic" ) )
        {
            /* Save the new mosaic image */
            saveMosaic();
        }
        else if ( command.equals( "CloseAll" ) )
        {
            /* close all images: */
            closeAllImages();
            m_kOpenReferenceButton.setEnabled( true );
            m_kOpenTileButton.setEnabled( false );
            m_kToggleSelectedButton.setEnabled( false );
            m_kRegisterButton.setEnabled( false );
            m_kSaveButton.setEnabled( false );
            m_kCloseAllButton.setEnabled( false );
        }
    }

    /**
     * Creates the Canvas3D for rendering the images.
     * @param kPanel, the JPanel that contains the Canvas3D in the frame
     * @return Canvas3D the new canvas
     */
    private Canvas3D createCanvas( JPanel kPanel )
    {
        /* Create the scene-graph for this canvas: */
        m_kScene = new BranchGroup();
        m_kScene.setCapability( BranchGroup.ALLOW_CHILDREN_READ );
        m_kScene.setCapability( BranchGroup.ALLOW_CHILDREN_WRITE );
        m_kScene.setCapability( BranchGroup.ALLOW_CHILDREN_EXTEND );

        /* BoundingShpere: */
        BoundingSphere kBounds =
            new BoundingSphere( new Point3d(0.0f, 0.0f, 0.0f), 100.0f );

        /* Add a white background: */
        Background kBackground = new Background( new Color3f( Color.white ) );
        kBackground.setApplicationBounds( kBounds );
        m_kScene.addChild( kBackground );

        /* Light the scene: */
        Color3f kLightColor = new Color3f(1, 1, 1);
        Vector3f kLightDir = new Vector3f(-.5f, -.5f, -1f);
        DirectionalLight kLight = new DirectionalLight( kLightColor, kLightDir );
        kLight.setInfluencingBounds( kBounds );
        m_kScene.addChild( kLight );

        /* Create the VolumeCanvas3D and SimpleUniverse: */
        GraphicsConfiguration kConfig = SimpleUniverse.getPreferredConfiguration();
        Canvas3D kCanvas = new Canvas3D(kConfig);
        kCanvas.addMouseListener( this );
        kCanvas.addMouseMotionListener( this );

        SimpleUniverse kUniverse = new SimpleUniverse( kCanvas );
        kUniverse.getViewingPlatform().setNominalViewingTransform();
        kUniverse.addBranchGraph( m_kScene );

        kCanvas.getView().setProjectionPolicy( View.PARALLEL_PROJECTION );
        kPanel.add( kCanvas, BorderLayout.CENTER );

        /* Return the new canvas: */
        return kCanvas;
    }

    /** 
     * Creates a texture-mapped polygon with the BufferedImage displayed as
     * the texture. The texture-mapped polygon is created so that the
     * displayed texture and size of the polygon match the size in pixels of
     * the original image loaded from file -- even when the original image
     * size is not a power of two. The displayed image must match the original
     * data image so that the registration is accurate.
     *
     * @param kTransformGroup, the TransformGroup which will contain the new
     * textured polygon in the scene graph
     * @param kImage, the BufferedImage, power or two size image, padded if
     * necessary, containing the original image data.
     * @param iWidth, the original image width
     * @param iHeight, the original image height
     * @param iWidthPow2, the next-largest power of two width
     * @param iHeightPow2, the next-largest power of two height
     */
    private void createTexturedPolygon( TransformGroup kTransformGroup,
                                        BufferedImage kImage,
                                        int iWidth, int iHeight,
                                        int iWidthPow2, int iHeightPow2  )
    {
        /* Create the new texture from the input BufferedImage: */
        ImageComponent2D kDisplayedImage =
            new ImageComponent2D( ImageComponent.FORMAT_RGBA, kImage );
        Texture2D kTexture =
            new Texture2D( Texture.BASE_LEVEL, Texture.RGBA, iWidthPow2, iHeightPow2 );
        kTexture.setEnable( true );
        kTexture.setMinFilter( Texture.BASE_LEVEL_LINEAR );
        kTexture.setMagFilter( Texture.BASE_LEVEL_LINEAR );
        kTexture.setBoundaryModeS( Texture.CLAMP_TO_EDGE );
        kTexture.setBoundaryModeT( Texture.CLAMP_TO_EDGE );
        kTexture.setImage( 0, kDisplayedImage );

        /* Setup appearance attributes for the texture mapped polygon. */
        Appearance kImageAppearance = new Appearance();

        /* Disable lighting so that the color information comes from the
           texture maps. */
        Material kMaterial = new Material();
        kMaterial.setLightingEnable( false );
        kImageAppearance.setMaterial( kMaterial );

        /* Use Replace mode so all color comes from the texture. */
        TextureAttributes kTextureAttr = new TextureAttributes();
        kTextureAttr.setTextureMode( TextureAttributes.REPLACE );
        kImageAppearance.setTextureAttributes( kTextureAttr );
        kImageAppearance.setTexture( kTexture );

        /* Partially transparent so multiple images can overlap: */
        TransparencyAttributes kTransparency =
            new TransparencyAttributes( TransparencyAttributes.BLENDED, 0.15f );
        kImageAppearance.setTransparencyAttributes( kTransparency );

        /* The texture-mapped polygon geometry: */
        double dWidth = (double)iWidth /(double)m_kCanvas.getWidth();
        double dHeight = (double)iHeight /(double)m_kCanvas.getWidth();
        float fWidthTextureScale = (float)iWidth/(float)iWidthPow2;
        float fHeightTextureScale = (float)iHeight/(float)iHeightPow2; 
        QuadArray kGeometry =
            new QuadArray( 4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_2 );
        kGeometry.setCoordinate( 0, new Point3d( -dWidth, -dHeight, 0 ) );
        kGeometry.setCoordinate( 1, new Point3d(  dWidth, -dHeight, 0 ) );
        kGeometry.setCoordinate( 2, new Point3d(  dWidth,  dHeight, 0 ) );
        kGeometry.setCoordinate( 3, new Point3d( -dWidth,  dHeight, 0 ) );

        /* Texture coordinates: */
        kGeometry.setTextureCoordinate( 0, 0, new TexCoord2f( 0, 0 ) );
        kGeometry.setTextureCoordinate( 0, 1, new TexCoord2f( fWidthTextureScale, 0 ) );
        kGeometry.setTextureCoordinate( 0, 2, new TexCoord2f( fWidthTextureScale,
                                                              fHeightTextureScale ) );
        kGeometry.setTextureCoordinate( 0, 3, new TexCoord2f( 0, fHeightTextureScale ) );

        /* Create the rendered shape, allow the appearance to be read. */
        Shape3D kImageShape = new Shape3D( kGeometry, kImageAppearance );
        kTransformGroup.addChild( kImageShape );

        /* The border outline: when the image is selected the border is red: */        
        QuadArray kBorderGeometry =
            new QuadArray( 4, QuadArray.COORDINATES | QuadArray.COLOR_3 );
        kBorderGeometry.setCoordinate( 0, new Point3d( -dWidth, -dHeight, 0 ) );
        kBorderGeometry.setCoordinate( 1, new Point3d(  dWidth, -dHeight, 0 ) );
        kBorderGeometry.setCoordinate( 2, new Point3d(  dWidth,  dHeight, 0 ) );
        kBorderGeometry.setCoordinate( 3, new Point3d( -dWidth,  dHeight, 0 ) );
        kBorderGeometry.setColor( 0, new Color3f( 1, 0, 0 ) );
        kBorderGeometry.setColor( 1, new Color3f( 1, 0, 0 ) );
        kBorderGeometry.setColor( 2, new Color3f( 1, 0, 0 ) );
        kBorderGeometry.setColor( 3, new Color3f( 1, 0, 0 ) );
        kBorderGeometry.setCapability( GeometryArray.ALLOW_COLOR_WRITE );

        /* Line thinkness in pixels: */
        LineAttributes kLineAttributes =
            new LineAttributes( 2f, LineAttributes.PATTERN_SOLID, true );
        /* Display the outline of the box: */
        PolygonAttributes kPolygonAttributes =
            new PolygonAttributes( PolygonAttributes.POLYGON_LINE,
                                   PolygonAttributes.CULL_NONE, 0f );
        /* Appearance: */
        Appearance kBorderAppearance = new Appearance();
        kBorderAppearance.setLineAttributes( kLineAttributes );
        kBorderAppearance.setPolygonAttributes( kPolygonAttributes );
        /* Create the Shape3D object to contain the border: */
        Shape3D kBorderShape = new Shape3D( kBorderGeometry, kBorderAppearance );
        kBorderShape.setCapability( Shape3D.ALLOW_GEOMETRY_READ );
        kBorderShape.setCapability( Shape3D.ALLOW_GEOMETRY_WRITE );
        kTransformGroup.addChild( kBorderShape );

        /* Store the images for toggle selected and mouse manipulations: */
        if ( m_akBorderShapes == null )
        {
            m_akBorderShapes = new Shape3D[2];
            m_akImageTransforms = new TransformGroup[2];
            m_iSelected = 0;
            m_iOpen = 1;
        }
        else
        {
            int iTemp = m_iOpen;
            m_iOpen = m_iSelected;
            m_iSelected = iTemp;
            for ( int i = 0; i < 4; i++ )
            {
                ((QuadArray)m_akBorderShapes[ m_iOpen ].getGeometry())
                    .setColor( i, new Color3f( 0, 0, 1 ) );
            }
        }
        m_akBorderShapes[ m_iSelected ] = kBorderShape;
        m_akImageTransforms[ m_iSelected ] = kTransformGroup;
    }

    /** 
     * storeImage creates a BufferedImage from the ModelImage data where the
     * BufferedImage's size is the next-largest power of two from the
     * ModelImage size. The BufferedImage is then passed to the
     * createTexturedPolygon function for display in the scene graph.
     * @param kImage, the input ModelImage containing the image data.
     */
    private void storeImage( ModelImage kImage )
    {
        int iWidth = kImage.getExtents()[ 0 ];
        int iHeight = kImage.getExtents()[ 1 ];

        /* Determine the next-largest size that is a power of two, to create
         * the texture: */
        double dLog2Width = Math.log((double)iWidth)/Math.log(2.0);
        double dLog2Height = Math.log((double)iHeight)/Math.log(2.0);
        int iWidthPow2 = (int)Math.pow( 2, Math.ceil( dLog2Width ) );
        int iHeightPow2 = (int)Math.pow( 2, Math.ceil( dLog2Height ) );

        /* Create the new BufferedImage and write the ModelImage data into
         * it: */
        BufferedImage kBuffer =
            new BufferedImage( iWidthPow2, iHeightPow2, BufferedImage.TYPE_INT_ARGB );
        for ( int iY = 0; iY < iHeight; iY++ )
        {
            for ( int iX = 0; iX < iWidth; iX++ )
            {
                kBuffer.setRGB( iX, (iHeightPow2 - iHeight) + iY,
                                kImage.getPackedColor( iY * iWidth + iX ) &
                                0xaaffffff
                                );
            }
        }
        /* Create the textured polygon and store the data in the scene
         * graph: */
        Transform3D kTransform = new Transform3D();
        TransformGroup kTransformGroup = new TransformGroup( kTransform );
        kTransformGroup.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        kTransformGroup.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        createTexturedPolygon( kTransformGroup, kBuffer,
                               iWidth, iHeight,
                               iWidthPow2, iHeightPow2 );
        BranchGroup kBranch = new BranchGroup();
        kBranch.setCapability( BranchGroup.ALLOW_DETACH );
        kBranch.addChild( kTransformGroup );

        m_kScene.addChild( kBranch );
    }

    /**
     * Creates a file open dialog for image files (.jpg, tiff, etc.). If a new
     * file is opened it is mapped onto a polygon and placed in the scene
     * graph:
     * @return boolean, success or failure for the file open
     */
    private boolean createMosaicOpenDialog()
    {
        ModelImage image = null;
        FileIO fileIO = new FileIO();
        String fileName = null;
        String extension = null;
        String directory = null;
        ViewImageFileFilter vFilter = null;
        int i;

        try {
            ViewFileChooserBase fileChooser =
                new ViewFileChooserBase( ViewUserInterface.getReference(), true, false );
            if ( !fileChooser.useAWT() ) {
                JFileChooser chooser = fileChooser.getFileChooser();
                if ( ViewUserInterface.getReference().getDefaultDirectory() != null ) {
                    chooser.setCurrentDirectory( new File( ViewUserInterface.getReference().getDefaultDirectory() ) );
                } else {
                    chooser.setCurrentDirectory( new File( System.getProperties().getProperty( "user.dir" ) ) );
                }
                chooser.addChoosableFileFilter( new ViewImageFileFilter( ViewImageFileFilter.GEN ) );
                int returnVal = chooser.showOpenDialog( this );
                if ( returnVal == JFileChooser.APPROVE_OPTION ) {
                    fileName = chooser.getSelectedFile().getName();
                    i = fileName.lastIndexOf( '.' );
                    if ( i > 0 && i < fileName.length() - 1 ) {
                        extension = fileName.substring( i + 1 ).toLowerCase();
                        vFilter = new ViewImageFileFilter( ViewImageFileFilter.GEN );
                        if ( !vFilter.accept( extension ) ) {
                            MipavUtil.displayError( "Extension does not match filter type" );
                            return false;
                        }
                    } 
                    directory = String.valueOf( chooser.getCurrentDirectory() ) + File.separatorChar;
                    ViewUserInterface.getReference().setDefaultDirectory( directory );
                } else {
                    return false;
                }
            } else {
                fileName = fileChooser.getFileName();
                directory = fileChooser.getDirectory();
                if ( fileName == null || directory == null ) {
                    return false;
                }
            }
            image = fileIO.readImage(fileName, directory, false, null);
            storeImage( image );
        } catch ( OutOfMemoryError error ) {
            MipavUtil.displayError( "Out of memory: JFrameRegistrationMosaic" );
            Preferences.debug( "Out of memory: JFrameRegistrationMosaic\n", 3 );
            return false;
        }

        return true;
    }

    /**
     * Toggles which image is currently selected. Changes the color of the
     * image borders.
     */
    private void toggleSelectedImage()
    {
        int iTemp = m_iOpen;
        m_iOpen = m_iSelected;
        m_iSelected = iTemp;
        for ( int i = 0; i < 4; i++ )
        {
            ((QuadArray)m_akBorderShapes[ m_iOpen ].getGeometry())
                .setColor( i, new Color3f( 0, 0, 1 ) );
            ((QuadArray)m_akBorderShapes[ m_iSelected ].getGeometry())
                .setColor( i, new Color3f( 1, 0, 0 ) );
        }
    }


    /**
     * Calls the registration algorithm and creates a new mosaic image with
     * the result of the registration:
     */
    private void registerImages()
    {}

    /**
     * Opens a save dialog and saves the mosaic image in the selected file
     * format.
     */
    private void saveMosaic()
    {}

    /**
     * closeAllImages{} : clears the scenegraph of all displayed images and
     * deletes references to the images:
     */
    private void closeAllImages()
    {
        while( m_kScene.numChildren() > 2 )
        {
            m_kScene.removeChild( 2 );
        }
        
        for ( int i = 0; i < 2; i++ )
        {
            m_akImageTransforms[i] = null;
            m_akBorderShapes[i] = null;
        }
        m_akImageTransforms = null;
        m_akBorderShapes = null;
    }



    /**
     * mouseDragged
     *
     * @param e MouseEvent
     */
    public void mouseDragged(MouseEvent e)
    {
        /* Left mouse button, rotation: */
        if ( m_kMouseEvent.getButton() == MouseEvent.BUTTON1 ) {
            if ( e.getX() != m_iXClick )
            {
                /* The angle rotation is based on the mouse x position on the
                 * screen: */
                double dAngle = (e.getX() - m_iXClick) * 
                    ((2.0 * Math.PI)/(double)m_kCanvas.getWidth());
                Transform3D kRotate = new Transform3D();
                kRotate.setRotation( new AxisAngle4d( 0, 0, 1, -dAngle ) );

                /* rotation is about the center of the selected image, so
                 * concatenate a translation to the origin, rotation,
                 * translation from the origin into the matrix: */
                Vector3d kTransV = new Vector3d();
                m_kOldTransform.get( kTransV );
                Transform3D kTranslateInv = new Transform3D();
                kTranslateInv.setTranslation( new Vector3d( -kTransV.x,
                                                            -kTransV.y,
                                                            -kTransV.z  ) );
                Transform3D kTranslate = new Transform3D();
                kTranslate.setTranslation( kTransV );
                kRotate.mul( kTranslateInv );
                kTranslate.mul( kRotate );
                m_kCurrentTransform = new Transform3D( kTranslate );

            }
        }
        /* Middle mouse button, scale: */
        if ( m_kMouseEvent.getButton() == MouseEvent.BUTTON2 ) {
            if ( e.getY() != m_iYClick )
            {
                /* scale is based on the mouse y movement, each time the mouse
                 * moves in y the image is scaled larger (up in y) or smaller
                 * (down in y): */
                double dScale = 1.0;
                if ( e.getY() > m_iYClick )
                {
                    dScale = 1.01;
                }
                else if ( e.getY() < m_iYClick )
                {
                    dScale = 1.0/1.01;
                }
                Transform3D kScale = new Transform3D();
                kScale.setScale( dScale );
                m_kCurrentTransform.mul( kScale );
                m_iXClick = e.getX();
                m_iYClick = e.getY();
            }
        }
        /* Right mouse button, translation: */
        else if ( m_kMouseEvent.getButton() == MouseEvent.BUTTON3 )
        {
            /* translation is based on the mouse x-y position, the center of
             * the image is placed exactly at the mouse: */
            Matrix4d kNewMatrix = new Matrix4d();
            m_kCurrentTransform.get( kNewMatrix );
            kNewMatrix.m03 =
                ((double)(e.getX() - (m_iXClick))) *
                (2.0/(double)m_kCanvas.getWidth());
            kNewMatrix.m13 =
                -((double)(e.getY() - (m_iYClick))) * 
                (2.0/(double)m_kCanvas.getWidth());
            m_kCurrentTransform.set( kNewMatrix );
        }       

        /* Concatenate the current transformations with the previous
         * (accumulated) mouseDragged transformation: */
        Transform3D kNewTransform = new Transform3D( m_kCurrentTransform );
        kNewTransform.mul( m_kOldTransform );
        m_akImageTransforms[m_iSelected].setTransform( kNewTransform );
    }

    /**
     * mouseMoved
     *
     * @param e MouseEvent
     */
    public void mouseMoved(MouseEvent e) {}
    
    /**
     * mouseClicked
     *
     * @param e MouseEvent
     */
    public void mouseClicked(MouseEvent e) {}
    
    /**
     * mouseEntered
     *
     * @param e MouseEvent
     */
    public void mouseEntered(MouseEvent e) {}
    
    /**
     * mouseExited
     *
     * @param e MouseEvent
     */
    public void mouseExited(MouseEvent e) {}

    /**
     * mousePressed, store the current transformation for the selected image
     * so the new transformations calculated in the mouseDragged function can
     * be concatenated onto the current transform.
     *
     * @param e MouseEvent
     */
    public void mousePressed(MouseEvent kMouseEvent )
    {
        /* Save the location of the mouse press: */
        m_iXClick = kMouseEvent.getX();
        m_iYClick = kMouseEvent.getY();
        /* Store the event, for determining which button is pressed during
         * drag: */
        m_kMouseEvent = kMouseEvent;

        /* Store the current transform in m_kOldTransform, it will be
         * concatenated during mouseDrag: */
        m_kCurrentTransform = new Transform3D();
        m_kOldTransform = new Transform3D();
        m_akImageTransforms[m_iSelected].getTransform( m_kOldTransform );
    }

    /**
     * mouseReleased
     *
     * @param e MouseEvent
     */
    public void mouseReleased(MouseEvent e) {}

    /* GUI buttons: */
    /** Open reference image: */
    private JButton m_kOpenReferenceButton;
    /** Open tile image: */
    private JButton m_kOpenTileButton;
    /** Toggle which image is currently selected: */
    private JButton m_kToggleSelectedButton;
    /** Initialize and start the registration based on how the user positioned
     * the two images: */
    private JButton m_kRegisterButton;
    /** Save the mosaic image: */
    private JButton m_kSaveButton;
    /** Close all images and remove them from the scene: */
    private JButton m_kCloseAllButton;

    /* Display/Transform: */
    /** Drawing canvas: */
    private Canvas3D m_kCanvas;
    /** Scene graph root node: */
    private BranchGroup m_kScene;
    /** Reference and tile image transformations: */
    private TransformGroup[] m_akImageTransforms = null;
    /** Reference to the border shape data structure for changing the color
     * based on which image is selected: */
    private Shape3D[] m_akBorderShapes = null;
    /** index of the non-selected image: */
    private int m_iOpen = 1;
    /** index of the selected image:*/
    private int m_iSelected = 0;
    /** Reference to the mousePressed event: */
    private MouseEvent m_kMouseEvent = null;
    /** x,y positions of the mouse when one of the mouse buttons is pressed: */
    private int m_iXClick, m_iYClick;
    /** current transformation based on mouseDragged event: */
    private Transform3D m_kCurrentTransform;
    /** Accumulated transformation prior to current mouseDrag: */
    private Transform3D m_kOldTransform;
}
