package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import com.sun.j3d.utils.picking.*;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.media.j3d.*;
import javax.swing.*;
import javax.vecmath.*;


/*
 * SurfacePaint class performs paint operations on a ModelTriangleMesh
 * surfaces. When the mouse is moved over the surface, the PickCanvas is used
 * to retrieve the picked triangle in the mesh. The triangle vertex colors are
 * set to a user-specified color.
 * 
 * @see SurfaceAttributes.java
 * @see JPanelSurface.java
 */
public class SurfacePaint
    implements ActionListener,
               MouseListener,
               MouseMotionListener
{
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Paint the ModelTriangleMesh vertex color: */
    public static final int VERTEX = 0;

    /** Paint into the 3D texture map: */
    public static final int TEXTURE = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Curent paint mode. */
    private int m_PaintMode = VERTEX;

    /** Enables painting */
    private boolean m_bEnabled = false;

    /** PickCanvas, for triangle picking. */
    private PickCanvas m_kPickCanvas = null;

    /** PickShape, for picking */
    private PickShape mPickShape = null;

    /** PickDirection, the direction of the PickShape. */
    private Vector3d kPickDirection = null;

    /** Reference to the JPanelSurface: */
    private JPanelSurface m_kPanel = null;
    
    /** Paint user-interface ToolBar */
    private JToolBar mPaintToolBar;

    /** Button group for paint functions:*/
    private ButtonGroup mButtonGroup;

    /** Paint brush button */
    private JToggleButton mPaintBrushButton;
    /** Paint dropper button */
    private JToggleButton mDropperButton;
    /** Paint can button */
    private JToggleButton mPaintCanButton;
    /** Eraser paint button */
    private JToggleButton mEraserButton;
    /** Erase all button */
    private JButton mEraseAllButton;
    /** Paint brush size text field */
    private JTextField mBrushSizeText;
    /** current paint brush size */
    private int mBrushSize = 1;
    /** Color selection button */
    private JButton mColorPaintButton;

    /** Current paint color */
    private Color4f mPaintColor = new Color4f( 1f, 0f, 0f, 1f );

    /** Opacity paint button */
    private JButton mOpacityPaintButton;
    /** Current paint opacity */
    private float mOpacity = 1f;

    /** Color Chooser dialog. */
    private JColorChooser mColorChooser;

    /** Paint Grow Dialog. */
    private JDialogPaintGrow mPaintGrowDialog = null;


    /** Rotation transform for transforming the triangle normal into world coordinates. */
    private TransformGroup mMouseRotate;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /** Default Constructor */
    public SurfacePaint( JPanelSurface parent, SurfaceRender parentScene )
    {
        m_kPanel = parent;
        setPickCanvas( m_kPanel.getPickCanvas() );
        mMouseRotate = parentScene.getSceneRootTG();
        init();
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    private void init()
    {
    	
    	ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);
        mPaintToolBar = ViewToolBarBuilder.initToolBar();
        mPaintToolBar.setSize(320, 30);
        mPaintToolBar.setBounds(0, 0, 340, 30);

        mButtonGroup = new ButtonGroup();

        mPaintBrushButton = toolbarBuilder.buildToggleButton("PaintBrush", "Draw using a brush.", "brush", mButtonGroup);
        mPaintToolBar.add( mPaintBrushButton );
        mButtonGroup.add( mPaintBrushButton );

        mDropperButton = toolbarBuilder.buildToggleButton("Dropper", "Picks up a color from the image.", "dropper", mButtonGroup);
        mPaintToolBar.add( mDropperButton );
        mButtonGroup.add( mDropperButton );

        mPaintCanButton = toolbarBuilder.buildToggleButton("PaintCan", "Fills an area with desired color.", "paintcan", mButtonGroup);
        mPaintToolBar.add( mPaintCanButton );
        mButtonGroup.add( mPaintCanButton );
        
        mEraserButton = toolbarBuilder.buildToggleButton("Eraser", "Erases paint.", "eraser", mButtonGroup);
        mPaintToolBar.add( mEraserButton );
        mButtonGroup.add( mEraserButton );        
        
        mEraseAllButton = toolbarBuilder.buildButton("EraseAll", "Erases all paint.", "clear");
        mPaintToolBar.add( mEraseAllButton );
        
        mPaintToolBar.add( ViewToolBarBuilder.makeSeparator() );

        JLabel brushSizeLabel = new JLabel("Brush size:");
        brushSizeLabel.setForeground(Color.black);
        brushSizeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        mBrushSizeText = new JTextField( "1", 2 );
        mBrushSizeText.setEditable(true);
        mBrushSizeText.setAlignmentX(Component.LEFT_ALIGNMENT);
        mBrushSizeText.addActionListener(this);
        mBrushSizeText.setActionCommand("BrushSizeChanged");
        mBrushSizeText.setEnabled(false );
        mPaintToolBar.add( brushSizeLabel );
        mPaintToolBar.add( mBrushSizeText );

        mPaintToolBar.add( ViewToolBarBuilder.makeSeparator() );

        mColorPaintButton = new JButton( MipavUtil.getIcon( "colorpaint.gif" ) );
        mColorPaintButton.addActionListener( this );
        mColorPaintButton.setActionCommand( "ColorPaint" );
        mColorPaintButton.setToolTipText( "Change paint color." );
        mColorPaintButton.setBackground( mPaintColor.get() );
        mColorPaintButton.setEnabled( false );
        mPaintToolBar.add( mColorPaintButton);

        mOpacityPaintButton = new JButton("Opacity");
        mOpacityPaintButton.addActionListener(this);
        mOpacityPaintButton.setToolTipText("Change opacity of paint.");
        mOpacityPaintButton.setFont(MipavUtil.font12B);
        mOpacityPaintButton.setMinimumSize(new Dimension(20, 20));
        mOpacityPaintButton.setMargin(new Insets(2, 7, 2, 7));
        mOpacityPaintButton.setActionCommand("OpacityPaint");
        mOpacityPaintButton.setEnabled(false );
        mPaintToolBar.add( mOpacityPaintButton);

        mColorChooser = new JColorChooser(mColorPaintButton.getBackground());
    }

    /** Enables/disables the user-interface
     * @param flag when true the user-interface is enabled, when false the
     * user-interface is disabled.
     */
    public void setEnabled( boolean flag )
    {
        mPaintBrushButton.setEnabled( flag );
        mDropperButton.setEnabled( flag );
        mEraserButton.setEnabled( flag );
        mEraseAllButton.setEnabled( flag );
        mBrushSizeText.setEnabled( flag );
        mColorPaintButton.setEnabled( flag );
        mOpacityPaintButton.setEnabled( flag );
        if ( flag == false )
        {
            mPaintCanButton.setEnabled( flag );
        }
    }

    /** Enables/disables the Paint Can user-interface
     * @param flag when true Paint Can is enabled, when false the Paint Can
     * is disabled.
     */
    public void enableSurfacePaintCan( boolean flag )
    {
        mPaintCanButton.setEnabled( flag );
    }

    /** Enables/disables the Surface per-vertex paint user-interface
     * @param flag when true per-vertex paint is enabled, when false the per-vertex paint is disabled
     * is disabled.
     */
    public void enableSurfacePaint( boolean flag )
    {
        mPaintBrushButton.setEnabled( flag );
        mDropperButton.setEnabled( flag );
        mEraserButton.setEnabled( flag );
        mBrushSizeText.setEnabled( flag );
    }

    /**
     * Returns true if the user has enabled the paint brush.
     * @return the enabled/disbled status of the paint brush.
     */
    public boolean getEnabled()
    {
        return m_bEnabled;
    }

    /**
     * Returns the ModelImage to paint into.
     * @return paint/texture ModelImage
     */
    public ModelImage getPaintImage()
    {
        return m_kPanel.getTextureImage();
    }

    /**
     * actionPerformed, listens for interface events.
     * @param event ActionEvent generated by the interface.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if ( command.equals( "PaintBrush" ) )
        {
	        if ( !m_kPanel.isSurfacePickableSelected() ) {
		        String[] possibilities = {"OK" };
		        int result = JOptionPane.showOptionDialog(null,
		                      "Please, enable Surface Pickable.\nAnd, Ctrl + Mouse to paint.",
		                      null, JOptionPane.YES_NO_OPTION,
		                      JOptionPane.INFORMATION_MESSAGE, null, possibilities,
		                      new Integer(0));
	        }
		       
	        mPaintColor.set( mColorChooser.getColor() );
	        mPaintColor.w = mOpacity;
	        setPaintMode( SurfacePaint.VERTEX );
	        
        }
        else if ( command.equals( "PaintCan" ) )
        {
            mPaintGrowDialog = new JDialogPaintGrow(null, this, null);
        }
        else if ( command.equals( "Eraser" ) )
        {
            setPaintMode( SurfacePaint.VERTEX );
        }
        else if ( command.equals( "EraseAll" ) )
        {
            setPaintMode( SurfacePaint.VERTEX );
            m_kPanel.restoreVertexColors();
        }
        else if ( command.equals( "ColorPaint" ) )
        {
            JDialog kDialog = JColorChooser.createDialog(new Frame(), "Set Paint Color", true, mColorChooser,
                                                         this, this);
            kDialog.setVisible(true);
        }
        else if ( command.equals( "OpacityPaint" ) )
        {
            new JDialogOpacityControls( null, this, mOpacity );
        }
        else if ( command.equals( "BrushSizeChanged" ) ) 
        {
            mBrushSize = Integer.parseInt( mBrushSizeText.getText() );
            if ( mBrushSize <= 0 )
            {
                mBrushSize = 1;
                mBrushSizeText.setText( "1" );
            }
        }
        else if ( command.equals( "OK" ) )
        {
            mColorPaintButton.setBackground( mColorChooser.getColor() );
            mPaintColor.set( mColorChooser.getColor() );
            mPaintColor.w = mOpacity;
        }
        m_bEnabled = mPaintBrushButton.isSelected() |
            mDropperButton.isSelected() |
            mPaintCanButton.isSelected() |
            mEraserButton.isSelected();
    }

    public JToolBar getToolBar() {
        return mPaintToolBar;
    }


    /**
     * Sets the type of paint, either vertex-color or texture value:
     * @param mode SurfacePaint.TEXTURE or SurfacePaint.VERTEX
     */
    public void setPaintMode( int mode )
    {
        m_PaintMode = mode;
    }

    /**
     * Deletes all member variables, clean memory.
     */
    public void dispose() {}


    /**
     * One of the overrides necessary to be a MouseListener. This function is
     * invoked when a button has been pressed and released.
     *
     * @param kMouseEvent the mouse event generated by a mouse clicked
     */
    public void mouseClicked(MouseEvent kMouseEvent) {}

    /**
     * mouseDragged.
     *
     * @param  kMouseEvent  MouseEvent
     */
    public void mouseDragged(MouseEvent kMouseEvent) {}


    /**
     * One of the overrides necessary to be a MouseListener. Invoked when the
     * mouse enters a component.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse entered
     */
    public void mouseEntered(MouseEvent kMouseEvent) {}

    /**
     * One of the overrides necessary to be a MouseListener. Invoked when the
     * mouse leaves a component.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse exit
     */
    public void mouseExited(MouseEvent kMouseEvent) {}


    /**
     * Invoked when the mouse moves.
     * @param  kMouseEvent  MouseEvent
     */
    public void mouseMoved(MouseEvent kMouseEvent)
    {
        /* Only capture mouse events when enabled, and only when the control
         * key is down and the left mouse button is pressed. */
        if (m_bEnabled &&
            kMouseEvent.isControlDown() )
        {
            PickResult kPickResult = getPickResult( kMouseEvent );
            if ( kPickResult == null )
            {
                return;
            }

            /* Pick the first intersection since we executed a pick
             * closest. */
            PickIntersection kPick = kPickResult.getIntersection(0);
            
            /* Get the coordinates of the picked point on the mesh. */
            Point3f kPickPoint = null;
            try {
                kPickPoint = new Point3f(kPick.getPointCoordinates());
            } catch ( java.lang.RuntimeException e ) {
                return;
            }
            if ( kPickPoint == null )
            {
                return;
            }
                
            Vector3f kPickNormal = new Vector3f(kPick.getPointNormal());
            Transform3D kTransform = new Transform3D();
            mMouseRotate.getTransform( kTransform );
            kTransform.transform( kPickNormal );
            kPickNormal.normalize();
            
            Vector3f kDirection = new Vector3f( (float)kPickDirection.x, (float)kPickDirection.y, (float)kPickDirection.z );
            kDirection.normalize();
            // If the PickShape is not a PickBounds, then we can test
            // the dot-product:
            if ( !(mPickShape instanceof PickBounds) &&
                 (kDirection.dot( kPickNormal ) > 0) )
            {
                return;
            }
            if ( m_PaintMode == SurfacePaint.VERTEX )
            {
                /* Get the coordinates of the picked point on the mesh. */
                ModelTriangleMesh kMesh = (ModelTriangleMesh) kPickResult.getGeometryArray();
                int[] indices = kPick.getPrimitiveCoordinateIndices();
                Point3f vert0 = new Point3f();
                kMesh.getCoordinate( indices[0], vert0 );
                Point3f vert1 = new Point3f();
                kMesh.getCoordinate( indices[1], vert1 );
                Point3f vert2 = new Point3f();
                kMesh.getCoordinate( indices[2], vert2 );
                
                float dist0 = kPickPoint.distance( vert0 );
                float dist1 = kPickPoint.distance( vert1 );
                float dist2 = kPickPoint.distance( vert2 );
                int closest = indices[0];
                Point3f kClosestPoint = vert0;
                if ( dist1 < dist0 )
                {
                    closest = indices[1];
                    dist0 = dist1;
                    kClosestPoint = vert1;
                }
                if ( dist2 < dist0 )
                {
                    closest = indices[2];
                    dist0 = dist2;
                    kClosestPoint = vert2;
                }
                
                /* Erase */
                float fOpacity = 1;
                if ( mEraserButton.isSelected() )
                {
                    Shape3D kShape = (Shape3D) kPickResult.getNode( PickResult.SHAPE3D );
                    if ( kShape != null )
                    {
                        fOpacity = 1 - kShape.getAppearance().getTransparencyAttributes().getTransparency();
                    }
                    if ( m_kPanel.getTextureStatus() != JPanelSurfaceTexture.NONE )
                    {
                        mPaintColor =
                            m_kPanel.getSurfaceMask().getModelImageColor( m_kPanel.getTextureImage(), fOpacity,
                                                                          m_kPanel.getSurfaceMask().getModelImagePoint( kClosestPoint ));
                    }
                    else if ( kShape != null )
                    {
                        Color3f kColor = new Color3f();
                        kShape.getAppearance().getMaterial().getDiffuseColor( kColor );
                        mPaintColor.set( kColor.get() );
                    }
                }         
                /* Dropper */
                if ( mDropperButton.isSelected() )
                {
                    kMesh.getColor( closest, mPaintColor );
                    mColorPaintButton.setBackground( mPaintColor.get() );
                    mOpacity = mPaintColor.w;
                }
                else if ( mPaintCanButton.isSelected() && (mPaintGrowDialog != null) )
                {
                    getModelColor( kPickPoint );
                }
                else
                {                            
                    kMesh.setColor( closest, mPaintColor );
                    Point3f kVert = new Point3f();
                    if ( mBrushSize > 1 )
                    {
                        boolean update = false;
                        for ( int v = 0; v < kMesh.getVertexCount(); v++ )
                        {
                            kMesh.getCoordinate( v, kVert );
                            if ( kVert.distance( kPickPoint ) < (mBrushSize/100.0f) )
                            {
                                // if erase mode and the mesh is volume
                                // per-vertex color, get the original color:
                                if ( mEraserButton.isSelected() &&
                                     (m_kPanel.getTextureStatus() != JPanelSurfaceTexture.NONE) )
                                {
                                    mPaintColor =
                                        m_kPanel.getSurfaceMask().getModelImageColor( m_kPanel.getTextureImage(), fOpacity,
                                                                                      m_kPanel.getSurfaceMask().getModelImagePoint( kVert ));
                                }
                                // Sets the color, but does not update the geometry yet (faster):
                                kMesh.setColorDelay( v, mPaintColor );
                                update = true;
                            }
                        }
                        // updates the geometry all at once (faster):
                        if ( update )
                        {
                            kMesh.setColorUpdate();
                        }
                    }
                }
            }
        }
    }

    /**
     * One of the overrides necessary to be a MouseListener. Invoked when a mouse button is pressed.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent kMouseEvent) {}


    /**
     * One of the overrides necessary to be a MouseListener. Invoked when a mouse button is released.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse release
     */
    public void mouseReleased(MouseEvent kMouseEvent)
    {
        /* Only capture mouse events when enabled, and only when the control
         * key is down and the left mouse button is pressed. */
        if (m_bEnabled &&
            kMouseEvent.isControlDown() &&
            mPaintCanButton.isSelected() &&
            (mPaintGrowDialog != null) )
        {
            
            PickResult kPickResult = getPickResult( kMouseEvent );
            if ( kPickResult == null )
            {
                return;
            }

            /* Pick the first intersection since we executed a pick
             * closest. */
            PickIntersection kPick = kPickResult.getIntersection(0);
            
            /* Get the coordinates of the picked point on the mesh. */
            Point3f kPickPoint = null;
            try {
                kPickPoint = new Point3f(kPick.getPointCoordinates());
            } catch ( java.lang.RuntimeException e )  {}
            if ( kPickPoint == null )
            {
                return;
            }
            
            Vector3f kPickNormal = new Vector3f(kPick.getPointNormal());
            Transform3D kTransform = new Transform3D();
            mMouseRotate.getTransform( kTransform );
            kTransform.transform( kPickNormal );
            
            Vector3f kDirection = new Vector3f( (float)kPickDirection.x, (float)kPickDirection.y, (float)kPickDirection.z );
            // If the PickShape is not a PickBounds, then we can test
            // the dot-product:
            if ( !(mPickShape instanceof PickBounds) &&
                 (kDirection.dot( kPickNormal ) > 0) )
            {
                return;
            }
            if ( m_PaintMode == SurfacePaint.VERTEX )
            {
                /* Get the coordinates of the picked point on the mesh. */
                Point3f modelPoint = m_kPanel.getSurfaceMask().getModelImagePoint( kPickPoint );
                ModelImage kImage = m_kPanel.getTextureImage();
                regionGrow( kImage, modelPoint );
            }
        }
    }

    /** 
     * Returns the pick result based on the input mouse event.
     * @param kMouseEvent the current mouse position.
     * @return kPickResult, the picked object.
     */
    private PickResult getPickResult( MouseEvent kMouseEvent )
    {
        /* If the pickCanvas is null, then do not try to pick */
        if (m_kPickCanvas == null) {
            System.err.println( "SurfacePaint.mouseDragged: pickCanvas is null" );
            return null;
        }
        
        /* Set the location for picking that was stored when the mouse was
         * presed: */
        m_kPickCanvas.setTolerance( 1.0f );
        m_kPickCanvas.setShapeLocation( kMouseEvent );
        mPickShape = m_kPickCanvas.getPickShape();
        kPickDirection = new Vector3d();
        if ( mPickShape instanceof PickRay )
        {
            Point3d kStart = new Point3d();
            ((PickRay)mPickShape).get( kStart, kPickDirection );
        }
        else if ( mPickShape instanceof PickCone )
        {
            Point3d kPickStart = new Point3d();
            ((PickCone)mPickShape).getDirection( kPickDirection );
            ((PickCone)mPickShape).getOrigin( kPickStart );
            PickRay kPickRay = new PickRay( kPickStart, kPickDirection );
            m_kPickCanvas.setShape( kPickRay, kPickStart );
        }
        else if ( mPickShape instanceof PickCylinder )
        {
            ((PickCylinder)mPickShape).getDirection( kPickDirection );
        }
        else if ( mPickShape instanceof PickSegment )
        {
            Point3d kStart = new Point3d();
            Point3d kEnd = new Point3d();
            ((PickSegment)mPickShape).get( kStart, kEnd );
            kPickDirection.sub( kEnd, kStart );
        }
        
        PickResult kPickResult = null;
        
        /* Try to get the closest picked polygon, catch the
         * javax.media.j3d.CapabilityNotSetException. */
        try {
            kPickResult = m_kPickCanvas.pickClosest();
        } catch (javax.media.j3d.CapabilityNotSetException e) {
            System.err.println("pickClosest failed: " + e.getMessage());
            return null;
        }
        return kPickResult;
    }

    

    /**
     * Access function to set the pickCanvas.
     *
     * @param  kPickCanvas  PickCanvas
     */
    public void setPickCanvas(PickCanvas kPickCanvas) {
        m_kPickCanvas = kPickCanvas;
        m_kPickCanvas.getCanvas().addMouseListener(this);
        m_kPickCanvas.getCanvas().addMouseMotionListener(this);
    }

    /**
     * Sets the opacity of the paint.
     * @param opacity paint opacity.
     */
    public void setOpacity( float opacity )
    {
        mOpacity = opacity;
        mPaintColor.w = mOpacity;
    }

    /**
     * Display the ModelImage color in the JDialogPaintGrow interface.
     * @param kPickPoint the model triangle mesh point under the mouse.
     */
    private void getModelColor( Point3f kPickPoint  )
    {
        ModelImage kImage = m_kPanel.getTextureImage();

        /* Get the coordinates of the picked point on the mesh. */
        Point3f modelPoint = m_kPanel.getSurfaceMask().getModelImagePoint( kPickPoint );
        Color4f modelColor =  m_kPanel.getSurfaceMask().getModelImageColor( kImage, 1, modelPoint );
        mPaintGrowDialog.setPositionText("  X: " + String.valueOf(modelPoint.x) +
                                         " Y: " + String.valueOf(modelPoint.y) +
                                         " Z: " + String.valueOf(modelPoint.z) + "  Color:  " +
                                         modelColor.x * 255.0f + " " + modelColor.y * 255.0f + " " + modelColor.z * 255.0f );
    }


    /**
     * Grows a region based on a starting point supplied. A voxel is added to
     * the the paintMask mask if its intensity is between the the bounds which
     * are also supplied.
     *
     * @param  kImage the image to grow the region in
     * @param  kSeedPoint the starting point in the image
     */
    public void regionGrow( ModelImage kImage, Point3f kSeedPoint )
    {
        //Cursor cursor = getCursor();
        //setCursor(MipavUtil.waitCursor);
        
        if ((mPaintGrowDialog.getFuzzyThreshold() == -2.0f) ||
            (mPaintGrowDialog.getMaxSize() == -2) || (mPaintGrowDialog.getMaxDistance() == -2)) {
            return;
        }
        BitSet paintMask = new BitSet();
        try {

            int[] imageExtents = kImage.getExtents();
            
            Color4f modelColor =  m_kPanel.getSurfaceMask().getModelImageColor( kImage, 1, kSeedPoint );
            modelColor.x *= 255.0f;
            modelColor.y *= 255.0f;
            modelColor.z *= 255.0f;
            modelColor.w *= 255.0f;

            Point3D seed = new Point3D( (short)kSeedPoint.x,
                                          (short)kSeedPoint.y,
                                          (short)kSeedPoint.z );

            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(kImage, 1.0f, 1.0f);
            regionGrowAlgo.setRunningInSeparateThread(false);

            CubeBounds regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);

            if ( !kImage.isColorImage() )
            {
                float less = mPaintGrowDialog.getLowerBound();
                float more = mPaintGrowDialog.getUpperBound();
                if (kImage.getType() == ModelStorageBase.BOOLEAN) {
                    less = 0;
                    more = 0;
                }
                regionGrowAlgo.regionGrow3D( paintMask, seed,
                                             mPaintGrowDialog.getFuzzyThreshold(),
                                             false,
                                             mPaintGrowDialog.getDisplayFuzzy(),
                                             mPaintGrowDialog,
                                             modelColor.x - less, modelColor.x + more,
                                             mPaintGrowDialog.getMaxSize(),
                                             mPaintGrowDialog.getMaxDistance(),
                                             mPaintGrowDialog.getVariableThresholds(),
                                             0, regionGrowBounds);
            }
            else
            {
                regionGrowAlgo.regionGrow3D( paintMask, seed,
                                             mPaintGrowDialog.getFuzzyThreshold(),
                                             false,
                                             mPaintGrowDialog.getDisplayFuzzy(),
                                             mPaintGrowDialog,
                                             modelColor.x - mPaintGrowDialog.getLowerBoundR(),
                                             modelColor.x + mPaintGrowDialog.getUpperBoundR(),
                                             modelColor.y - mPaintGrowDialog.getLowerBoundG(),
                                             modelColor.y + mPaintGrowDialog.getUpperBoundG(),
                                             modelColor.z - mPaintGrowDialog.getLowerBoundB(),
                                             modelColor.z + mPaintGrowDialog.getUpperBoundB(),
                                             mPaintGrowDialog.getMaxSize(),
                                             mPaintGrowDialog.getMaxDistance(),
                                             0, regionGrowBounds);
            }
            m_kPanel.updateVolumeTexture( paintMask, mPaintColor );
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }
        
        //setCursor(cursor);
    }
}
