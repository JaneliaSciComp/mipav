package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.view.renderer.WildMagic.*;
import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


/*
 * SurfacePaint class performs paint operations on a TriMesh
 * surfaces. When the mouse is moved over the surface, the PickCanvas is used
 * to retrieve the picked triangle in the mesh. The triangle vertex colors are
 * set to a user-specified color.
 * 
 * @see SurfaceAttributes.java
 * @see JPanelSurface.java
 */
public class SurfacePaint_WM extends JInterfaceBase
{
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * 
     */
    private static final long serialVersionUID = -3767599197318881261L;

    /** Paint the TriMesh vertex color: */
    public static final int VERTEX = 0;

    /** Paint into the 3D texture map: */
    public static final int TEXTURE = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Enables painting */
    private boolean m_bEnabled = false;

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
    private ColorRGBA mPaintColor = new ColorRGBA( 1f, 0f, 0f, 1f );

    /** Opacity paint button */
    private JButton mOpacityPaintButton;
    /** Current paint opacity */
    private float mOpacity = 1f;

    /** Color Chooser dialog. */
    private JColorChooser mColorChooser;

    /** Paint Grow Dialog. */
    private JDialogPaintGrow mPaintGrowDialog = null;
    private JPanelSurface_WM m_kPanel;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /** Default Constructor */
    public SurfacePaint_WM( JPanelSurface_WM parent, VolumeTriPlanarInterface kVolumeViewer )
    {
        super(kVolumeViewer);
        m_kPanel = parent;
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
        mColorPaintButton.setBackground( new Color(mPaintColor.R, mPaintColor.G, mPaintColor.B) );
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
     * @param flag, when true the user-interface is enabled, when false the
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
     * @param flag, when true Paint Can is enabled, when false the Paint Can
     * is disabled.
     */
    public void enableSurfacePaintCan( boolean flag )
    {
        mPaintCanButton.setEnabled( flag );
    }

    /** Enables/disables the Surface per-vertex paint user-interface
     * @param flag, when true per-vertex paint is enabled, when false the per-vertex paint is disabled
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

//     /**
//      * Returns the ModelImage to paint into.
//      * @return paint/texture ModelImage
//      */
//     public ModelImage getPaintImage()
//     {
//         return m_kPanel.getTextureImage();
//     }

    /**
     * actionPerformed, listens for interface events.
     * @param event, ActionEvent generated by the interface.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if ( command.equals( "PaintBrush" ) )
        {
            if ( !m_kPanel.isSurfacePickableSelected() ) {
                String[] possibilities = {"OK" };
                JOptionPane.showOptionDialog(null,
                        "Please, enable Surface Pickable.\nAnd, Ctrl + Mouse to paint.",
                        null, JOptionPane.YES_NO_OPTION,
                        JOptionPane.INFORMATION_MESSAGE, null, possibilities,
                        new Integer(0));
	        }
            Color color = mColorChooser.getColor();
            mPaintColor.Set( color.getRed()/255.0f, 
                    color.getGreen()/255.0f,
                    color.getBlue()/255.0f, mOpacity );
        }
        else if ( command.equals( "PaintCan" ) )
        {
            mPaintGrowDialog = new JDialogPaintGrow(null, this, null);
        }
        else if ( command.equals( "Eraser" ) )
        {
        }
        else if ( command.equals( "EraseAll" ) )
        {
            m_kVolumeViewer.eraseAllPaint();
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
            Color color = mColorChooser.getColor();
            mColorPaintButton.setBackground( color ); 
            mPaintColor.Set( color.getRed()/255.0f, 
                    color.getGreen()/255.0f,
                    color.getBlue()/255.0f, mOpacity );
        }
        m_bEnabled = mPaintBrushButton.isSelected() |
            mDropperButton.isSelected() |
            mPaintCanButton.isSelected() |
            mEraserButton.isSelected();

        m_kVolumeViewer.enablePaint(mPaintColor, mBrushSize, m_bEnabled, mPaintBrushButton.isSelected(),
                mDropperButton.isSelected(), mPaintCanButton.isSelected(), mEraserButton.isSelected());
    }

    public JToolBar getToolBar() {
        return mPaintToolBar;
    }

    /**
     * Returns the ModelImage to paint into.
     * @return paint/texture ModelImage
     */
    public ModelImage getPaintImage()
    {
        return m_kVolumeViewer.getImageA();
    }
    /**
     * Deletes all member variables, clean memory.
     */
    public void dispose() {}

    /**
     * Sets the opacity of the paint.
     * @param opacity paint opacity.
     */
    public void setOpacity( float opacity )
    {
        mOpacity = opacity;
        mPaintColor.A = mOpacity;
    }
    
    public void setDropperColor( ColorRGBA kDropperColor, Vector3f kPickPoint )
    {
        if (  mPaintCanButton.isSelected() && (mPaintGrowDialog != null) )
        {
            regionGrow( m_kVolumeViewer.getImageA(), kPickPoint, kDropperColor );
        }
        else
        {
            mColorPaintButton.setBackground( new Color(kDropperColor.R, kDropperColor.G, kDropperColor.B) );
            mOpacity = kDropperColor.A;
            mPaintColor.Copy(kDropperColor);
        }
    }

    
    /**
     * Grows a region based on a starting point supplied. A voxel is added to
     * the the paintMask mask if its intensity is between the the bounds which
     * are also supplied.
     *
     * @param  kImage the image to grow the region in
     * @param  kSeedPoint the starting point in the image
     */
    public void regionGrow( ModelImage kImage, Vector3f kSeedPoint, ColorRGBA kSeedColor  )
    {
        
        mPaintGrowDialog.setPositionText("  X: " + String.valueOf(kSeedPoint.X) +
                                         " Y: " + String.valueOf(kSeedPoint.Y) +
                                         " Z: " + String.valueOf(kSeedPoint.Z) + "  Color:  " +
                                         kSeedColor.R * 255.0f + " " + kSeedColor.G * 255.0f + " " + kSeedColor.B * 255.0f );
        //Cursor cursor = getCursor();
        //setCursor(MipavUtil.waitCursor);
        
        if ((mPaintGrowDialog.getFuzzyThreshold() == -2.0f) ||
            (mPaintGrowDialog.getMaxSize() == -2) || (mPaintGrowDialog.getMaxDistance() == -2)) {
            return;
        }
        BitSet paintMask = new BitSet();
        try {

            int[] imageExtents = kImage.getExtents();
            Point3D seed = new Point3D( (short)kSeedPoint.X,
                                          (short)kSeedPoint.Y,
                                          (short)kSeedPoint.Z );

            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(kImage, 1.0f, 1.0f);
            regionGrowAlgo.setRunningInSeparateThread(false);

            CubeBounds regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);
            int iCount = 0;
            if ( !kImage.isColorImage() )
            {
                float less = mPaintGrowDialog.getLowerBound();
                float more = mPaintGrowDialog.getUpperBound();
                if (kImage.getType() == ModelStorageBase.BOOLEAN) {
                    less = 0;
                    more = 0;
                }
                iCount = regionGrowAlgo.regionGrow3D( paintMask, seed,
                                             mPaintGrowDialog.getFuzzyThreshold(),
                                             false,
                                             mPaintGrowDialog.getDisplayFuzzy(),
                                             mPaintGrowDialog,
                                             kSeedColor.R * 255.0f - less, kSeedColor.R * 255.0f + more,
                                             mPaintGrowDialog.getMaxSize(),
                                             mPaintGrowDialog.getMaxDistance(),
                                             mPaintGrowDialog.getVariableThresholds(),
                                             0, regionGrowBounds);
            }
            else
            {
                iCount = regionGrowAlgo.regionGrow3D( paintMask, seed,
                                             mPaintGrowDialog.getFuzzyThreshold(),
                                             false,
                                             mPaintGrowDialog.getDisplayFuzzy(),
                                             mPaintGrowDialog,
                                             kSeedColor.R * 255.0f - mPaintGrowDialog.getLowerBoundR(),
                                             kSeedColor.R * 255.0f + mPaintGrowDialog.getUpperBoundR(),
                                             kSeedColor.G * 255.0f - mPaintGrowDialog.getLowerBoundG(),
                                             kSeedColor.G * 255.0f + mPaintGrowDialog.getUpperBoundG(),
                                             kSeedColor.B * 255.0f - mPaintGrowDialog.getLowerBoundB(),
                                             kSeedColor.B * 255.0f + mPaintGrowDialog.getUpperBoundB(),
                                             mPaintGrowDialog.getMaxSize(),
                                             mPaintGrowDialog.getMaxDistance(),
                                             0, regionGrowBounds);
            }
            System.err.println( "regionGrow " + iCount );
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }
        //setCursor(cursor);
    }
    
    /**
     * Display the ModelImage color in the JDialogPaintGrow interface.
     * @param kPickPoint, the model triangle mesh point under the mouse.

    private void getModelColor( Point3f kPickPoint  )
    {
        ModelImage kImage = m_kPanel.getTextureImage();

        // Get the coordinates of the picked point on the mesh.
        Point3f modelPoint = m_kPanel.getSurfaceMask().getModelImagePoint( kPickPoint );
        Color4f modelColor =  m_kPanel.getSurfaceMask().getModelImageColor( kImage, 1, modelPoint );
        mPaintGrowDialog.setPositionText("  X: " + String.valueOf(modelPoint.x) +
                                         " Y: " + String.valueOf(modelPoint.y) +
                                         " Z: " + String.valueOf(modelPoint.z) + "  Color:  " +
                                         modelColor.x * 255.0f + " " + modelColor.y * 255.0f + " " + modelColor.z * 255.0f );
    }
*/
}
