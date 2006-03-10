package gov.nih.mipav.view.renderer;

import gov.nih.mipav.view.renderer.surfaceview.*;
import gov.nih.mipav.view.renderer.volumeview.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import gov.nih.mipav.model.file.*;

/**
 * <p>Title: JPanelSculptor </p>
 * <p>Description: Sculptor panel draws the sculptor and apply the sculptor to the whole volume region.</p>
 * @author Alexandra Bokinsky, Ph.D.
 */
public class JPanelSculptor extends JPanelRendererBase {

    /** SurfaceRender reference. */
    private SurfaceRender surRender;

    /** Control panel for volume sculpting */
    private JPanel mainPanel;

    /**
     * The scroll pane holding the panel content.  Useful when the screen is small.
     */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components */
    private DrawingPanel scrollPanel;

    /**
     *  Sculptor object enables, draws and sculpts the volume based on the
     *  outline region drawn on screen. Handles drawing the sculpt region.
     */
    private VolumeSculptor m_kVolumeSculptor;
    private TextureSculptor m_kTextureSculptor;

    /* Sculpturing interface */

    /** Toggle button is down while the mouse is used to draw the sculpt
     * outline */
    private JToggleButton m_kDrawOutlineButton;

    /** Button for clearing the sculpt region */
    private JButton m_kClearDrawOutlineButton;

    /** Button for inverting the sculpt region */
    private JButton m_kInvertOutlineButton;

    /** Button to apply sculpt region to the volume */
    private JButton m_kApplySculptButton;

    /** Button to undo the sculpt and restor the original volume */
    private JButton m_kUndoSculptButton;

    /** Button to save the sculpt image. */
    private JButton m_kSaveSculptButton;

    /** Window information for Sculpting: */
    int m_iSculptWidth = 0;
    int m_iSculptHeight = 0;

    /**
     * Contructor. Called from the surface render to create the sculptor control panel.
     * @param parent  surface render
     */
    public JPanelSculptor( RenderViewBase parent ) {
        super( parent );
        surRender = (SurfaceRender) parent;
        init();
        if (m_kTextureSculptor == null) {
          m_kTextureSculptor = new TextureSculptor(surRender);
        }

    }

    /**
      *  Calls disposeLocal
      */
     protected void finalize() throws Throwable {
         this.disposeLocal( false );
         super.finalize();
     }

     /**
      * Sets all variables to null, disposes, and garbage collects
      * @param flag dispose super or not, not used now.
      */
     public void disposeLocal( boolean flag ) {
         surRender = null;
         if ( m_kTextureSculptor != null ) {
           m_kTextureSculptor.disposeLocal(false);
           m_kTextureSculptor = null;
         }
         if ( m_kVolumeSculptor != null ) {
           m_kVolumeSculptor.disposeLocal(false);
           m_kVolumeSculptor = null;
         }
     }

    /**
     * Initialize the buttons layout
     */
    public void init() {
        /* initialize sculptors to null */
        m_kVolumeSculptor = null;
        m_kTextureSculptor = null;

        JToolBar viewToolBar = new JToolBar();
        viewToolBar.setBorderPainted(true);
        viewToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        viewToolBar.setLayout(new GridBagLayout());
        viewToolBar.setFloatable(false);

        mainPanel = new JPanel( new BorderLayout() );

        m_kDrawOutlineButton = new JToggleButton( MipavUtil.getIcon( "drawsculptor.gif" ) );
        m_kDrawOutlineButton.setMargin( new Insets( 0, 0, 0, 0 ) );
        m_kDrawOutlineButton.addActionListener( this );
        m_kDrawOutlineButton.setToolTipText( "Draw sculpt outline region" );
        m_kDrawOutlineButton.setActionCommand( "DrawSculptRegion" );
        m_kDrawOutlineButton.setBorderPainted( false );
        m_kDrawOutlineButton.setRolloverEnabled( true );
        m_kDrawOutlineButton.setRolloverIcon( MipavUtil.getIcon( "drawsculptorroll.gif" ) );
        m_kDrawOutlineButton.setFocusPainted( false );
        m_kDrawOutlineButton.setBorder( BorderFactory.createLoweredBevelBorder() );
        m_kDrawOutlineButton.setEnabled( true );
        viewToolBar.add( m_kDrawOutlineButton );


        m_kClearDrawOutlineButton = new JButton( MipavUtil.getIcon( "eraser.gif" ) );
        m_kClearDrawOutlineButton.addActionListener( this );
        m_kClearDrawOutlineButton.setActionCommand( "UndoDrawSculptRegion" );
        m_kClearDrawOutlineButton.setFont( MipavUtil.font12B );
        m_kClearDrawOutlineButton.setToolTipText( "Clear draw sculpt outline region" );
        m_kClearDrawOutlineButton.setBorderPainted( false );
        m_kClearDrawOutlineButton.setRolloverEnabled( true );
        m_kClearDrawOutlineButton.setRolloverIcon( MipavUtil.getIcon( "eraserroll.gif" ) );
        m_kClearDrawOutlineButton.setFocusPainted( false );
        m_kClearDrawOutlineButton.setEnabled( false );
        viewToolBar.add( m_kClearDrawOutlineButton );


        m_kInvertOutlineButton = new JButton( MipavUtil.getIcon( "inverseregion.gif" ) );
        m_kInvertOutlineButton.addActionListener( this );
        m_kInvertOutlineButton.setActionCommand( "InvertSculptRegion" );
        m_kInvertOutlineButton.setFont( MipavUtil.font12B );
        m_kInvertOutlineButton.setToolTipText( "Invert draw sculpt outline region" );
        m_kInvertOutlineButton.setBorderPainted( false );
        m_kInvertOutlineButton.setRolloverEnabled( true );
        m_kInvertOutlineButton.setRolloverIcon( MipavUtil.getIcon( "inverseregionroll.gif" ) );
        m_kInvertOutlineButton.setFocusPainted( false );
        m_kInvertOutlineButton.setEnabled( false );
        viewToolBar.add( m_kInvertOutlineButton );

        m_kApplySculptButton = new JButton( MipavUtil.getIcon( "applysculptor.gif" ) );
        m_kApplySculptButton.addActionListener( this );
        m_kApplySculptButton.setActionCommand( "ApplySculptRegion" );
        m_kApplySculptButton.setFont( MipavUtil.font12B );
        m_kApplySculptButton.setToolTipText( "Apply sculpt region to volume" );
        m_kApplySculptButton.setBorderPainted( false );
        m_kApplySculptButton.setRolloverEnabled( true );
        m_kApplySculptButton.setRolloverIcon( MipavUtil.getIcon( "applysculptorroll.gif" ) );
        m_kApplySculptButton.setFocusPainted( false );
        m_kApplySculptButton.setEnabled( false );
        viewToolBar.add( m_kApplySculptButton );

        m_kUndoSculptButton = new JButton( MipavUtil.getIcon( "sculptorundo.gif" ) );
        m_kUndoSculptButton.addActionListener( this );
        m_kUndoSculptButton.setActionCommand( "UndoApplySculptRegion" );
        m_kUndoSculptButton.setFont( MipavUtil.font12B );
        m_kUndoSculptButton.setToolTipText( "Undo apply sculpt region to volume" );
        m_kUndoSculptButton.setBorderPainted( false );
        m_kUndoSculptButton.setRolloverEnabled( true );
        m_kUndoSculptButton.setRolloverIcon( MipavUtil.getIcon( "sculptorundo.gif" ) );
        m_kUndoSculptButton.setFocusPainted( false );
        m_kUndoSculptButton.setEnabled( false );
        viewToolBar.add( m_kUndoSculptButton );

        m_kSaveSculptButton = new JButton( MipavUtil.getIcon( "save.gif" ) );
        m_kSaveSculptButton.addActionListener( this );
        m_kSaveSculptButton.setActionCommand( "SaveSculptImage" );
        m_kSaveSculptButton.setFont( MipavUtil.font12B );
        m_kSaveSculptButton.setToolTipText( "Save the sculpt region to image" );
        m_kSaveSculptButton.setBorderPainted( false );
        m_kSaveSculptButton.setRolloverEnabled( true );
        m_kSaveSculptButton.setRolloverIcon( MipavUtil.getIcon( "saveroll.gif" ) );
        m_kSaveSculptButton.setFocusPainted( false );
        m_kSaveSculptButton.setEnabled( false );
        viewToolBar.add( m_kSaveSculptButton );

        JPanel panel2 = new JPanel();
        panel2.add(viewToolBar);

        Box contentBox = new Box( BoxLayout.Y_AXIS );

        contentBox.add( panel2 );

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.add( contentBox, BorderLayout.NORTH );

        scroller = new JScrollPane( scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );

        mainPanel.add( scroller, BorderLayout.NORTH );
    }

    /**
     * Command processor to handle the geodesic button events.
     * @param e ActionEvent
     */
    public void actionPerformed( ActionEvent e ) {
        String command = e.getActionCommand();

        if ( command.equals( "DrawSculptRegion" ) ) {
            drawSculptRegion();
        } else if ( command.equals( "UndoDrawSculptRegion" ) ) {
            clearSculptRegion();
        } else if ( command.equals( "InvertSculptRegion" ) ) {
            invertSculptRegion();
        } else if ( command.equals( "ApplySculptRegion" ) ) {
            applySculptRegion();
        } else if ( command.equals( "UndoApplySculptRegion" ) ) {
            undoSculptRegion();
        } else if ( command.equals("SaveSculptImage") ) {
            boolean alreadySaved = false;
            if ( m_kTextureSculptor != null ) {
              alreadySaved = m_kTextureSculptor.save(new FileWriteOptions(true), -1);
            }
            if ( m_kVolumeSculptor != null && !alreadySaved) {
              m_kVolumeSculptor.save(new FileWriteOptions(true), -1);
            }
        }
    }

    /**
     *  Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     * @param panelWidth    panel width
     * @param frameHeight   panel height
     */
    public void resizePanel( int panelWidth, int frameHeight ) {
        scroller.setPreferredSize( new Dimension( panelWidth, frameHeight - 40 ) );
        scroller.setSize( new Dimension( panelWidth, frameHeight - 40 ) );
        scroller.revalidate();
    }

    /**
     * Initialize the raycast based volume render with the sculptor.
     * @param _rayBasedRender VolumeRenderer
     */
    public void setVolumeSculptor( VolumeRenderer _rayBasedRender ) {
        m_kVolumeSculptor = new VolumeSculptor( _rayBasedRender, m_iSculptWidth, m_iSculptHeight );
    }

    /**
     * Initialize the sculpt region size.  ViewJFrameVolumeView call this method to init the region.
     * @param imagePaneWidth  region width
     * @param imagePanelHeight  region height
     */
    public void setFrameSize( int width, int height ) {
        m_iSculptWidth = width;
        m_iSculptHeight = height;
        if ( m_kTextureSculptor != null ) {
            m_kTextureSculptor.initVolumeSculptor( m_iSculptWidth, m_iSculptHeight );
        }
        if ( m_kVolumeSculptor != null ) {
            m_kVolumeSculptor.initVolumeSculptor( m_iSculptWidth, m_iSculptHeight );
        }
    }

    /**
     * Get the main control panel.
     * @return mainPanel  main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /*
     *  drawSculptRegion: called when the "Draw Sculpt Outline" Button is pressed.
     */
    public void drawSculptRegion() {

        /* tell the m_kSculptor object that drawing is enabled */
        if ( m_kVolumeSculptor != null ) {
            m_kVolumeSculptor.enableSculpt( !m_kVolumeSculptor.getEnable() );
        }
        if ( m_kTextureSculptor != null ) {
            m_kTextureSculptor.enableSculpt( !m_kTextureSculptor.getEnable() );
        }

        /* enable the clear and apply sculpt buttons */
        m_kClearDrawOutlineButton.setEnabled( true );
        m_kInvertOutlineButton.setEnabled( true );
        m_kApplySculptButton.setEnabled( true );
    }

    /*
     *  clearwSculptRegion: called when the "Clear Outline" Button is pressed.
     */
    public void clearSculptRegion() {

        /* the m_kDrawOutlineButton is a toggle button, once clear is pressed,
         * un-toggle the draw button. */
        m_kDrawOutlineButton.setSelected( false );

        /* disable clear and apply sculpt buttons */
        m_kClearDrawOutlineButton.setEnabled( false );
        m_kInvertOutlineButton.setEnabled( false );
        m_kApplySculptButton.setEnabled( false );

        /* tell the m_kSculptor object to clear to draw outline */
        if ( m_kVolumeSculptor != null ) {
            m_kVolumeSculptor.clearSculpt();
        }

        /* tell the m_kSculptor object to clear to draw outline */
        if ( m_kTextureSculptor != null ) {
            m_kTextureSculptor.clearSculpt();
        }
    }

    /*
     *  clearwSculptRegion: called when the "Clear Outline" Button is pressed.
     */
    public void invertSculptRegion() {

        /* tell the m_kSculptor object to clear to draw outline */
        if ( m_kVolumeSculptor != null ) {
            m_kVolumeSculptor.invertSculpt();
        }
        if ( m_kTextureSculptor != null ) {
            m_kTextureSculptor.invertSculpt();
        }
    }


    /**
     * Cull the sculpt region through the 3D volume.
     */
    public void applySculptRegion() {
        /* the m_kDrawOutlineButton is a toggle button, once apply is pressed,
         * un-toggle the draw button. */
        m_kDrawOutlineButton.setSelected( false );
        m_kClearDrawOutlineButton.setEnabled( false );
        m_kInvertOutlineButton.setEnabled( false );
        m_kApplySculptButton.setEnabled( false );
        m_kUndoSculptButton.setEnabled( true );
        m_kSaveSculptButton.setEnabled( true );
        if ( m_kVolumeSculptor != null )
        {
            m_kVolumeSculptor.applySculpt();
        }
        if ( m_kTextureSculptor != null )
        {
            m_kTextureSculptor.applySculpt();
        }
        if ( m_kVolumeSculptor != null )
        {
            m_kVolumeSculptor.update();
            surRender.updateData();
            surRender.updateImages();
        }
        if ( m_kTextureSculptor != null )
        {
            m_kTextureSculptor.update();
            surRender.updateData();
            surRender.updateImages();
        }
    }

    /*
     *  undoSculptRegion: called when the "Undo Sculpt" Button is pressed.
     */
    public void undoSculptRegion() {
        /* the m_kDrawOutlineButton is a toggle button, once undo is pressed,
         * un-toggle the draw button. */
        m_kDrawOutlineButton.setSelected( false );
        m_kClearDrawOutlineButton.setEnabled( false );
        m_kInvertOutlineButton.setEnabled( false );
        m_kApplySculptButton.setEnabled( false );
        m_kUndoSculptButton.setEnabled( false );
        m_kSaveSculptButton.setEnabled( false );

        if ( m_kVolumeSculptor != null )
        {
            m_kVolumeSculptor.undoSculpt();
        }
        if ( m_kTextureSculptor != null )
        {
            m_kTextureSculptor.undoSculpt();
        }
        if ( m_kVolumeSculptor != null )
        {
            m_kVolumeSculptor.update();
            surRender.updateData();
            surRender.updateImages();
        }
        if ( m_kTextureSculptor != null )
        {
            m_kTextureSculptor.update();
            surRender.updateData();
            surRender.updateImages();
        }
    }


    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane
     */
    class DrawingPanel extends JPanel {
        protected void paintComponent( Graphics g ) {
            super.paintComponent( g );
        }
    }

}
