package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogLivewire;
import gov.nih.mipav.view.dialogs.JDialogMask;
import gov.nih.mipav.view.dialogs.JDialogOpacityControls;
import gov.nih.mipav.view.dialogs.JDialogVOIStats;
import gov.nih.mipav.view.renderer.WildMagic.Render.LocalVolumeVOI;
import gov.nih.mipav.view.renderer.WildMagic.Render.VOIPoint3D;
import gov.nih.mipav.view.renderer.WildMagic.Render.VOIPolyLineSlice3D;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JToolBar;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;


public class VOIManagerInterface implements ActionListener, KeyListener, VOIManagerListener, MouseListener
{
    /**
     * Pick up the selected color and call method to change the color.
     */
    class OkColorListener implements ActionListener {

        /** Color Button */
        JButton button;

        /**
         * Creates a new OkColorListener object.
         *
         * @param  _button  DOCUMENT ME!
         */
        OkColorListener(JButton _button) {
            super();
            button = _button;
        }

        /**
         * Get color from chooser and set button and color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            saveVOIs( "VOIColor" );

            Color color = colorChooser.getColor();

            button.setBackground(color);
            setButtonColor(button, color);
        }
    }
    private VOIManagerInterfaceListener m_kParent = null;
    private ModelImage m_kImageA;
    private ModelImage m_kImageB;
    private ModelImage[] m_akImageActive = new ModelImage[2];

    private byte[] m_aucBufferA;
    private byte[] m_aucBufferB;

    private ViewToolBarBuilder toolbarBuilder;

    private JToolBar m_kVOIToolbar;
    /** Reference to the color chooser. */
    private ViewJColorChooser colorChooser;
    private VOIManager[] m_kVOIManagers;
    private int m_iActive = 0;

    private int voiUID = 0;

    private VOI m_kCurrentVOIGroup = null;
    private Vector<String> m_kUndoCommands = new Vector<String>();
    private Vector<String> m_kRedoCommands = new Vector<String>();
    private float m_fOpacity = 1f;

    private boolean m_bGPURenderer = false;

    private JDialogVOIStats m_kVOIDialog;


    private Vector<LocalVolumeVOI> m_kCopyList = new Vector<LocalVolumeVOI>();

    private Vector<VOISaveState> m_kUndoList = new Vector<VOISaveState>();
    private Vector<VOISaveState> m_kRedoList = new Vector<VOISaveState>();

    private Object m_kImageAUndo = null;
    private Object m_kImageARedo = null;
    private Object m_kImageBUndo = null;
    private Object m_kImageBRedo = null;

    public VOIManagerInterface ( VOIManagerInterfaceListener kParent,
            ModelImage kImageA, ModelImage kImageB, int iNViews, boolean bGPU, ButtonGroup kVOIGroup )
    {
        m_kParent = kParent;
        m_kImageA = kImageA;
        m_kImageB = kImageB;        
        m_akImageActive[0] = kImageA;
        m_akImageActive[1] = kImageB;

        toolbarBuilder = new ViewToolBarBuilder(this);
        m_kVOIToolbar =
            toolbarBuilder.buildVolumeTriPlanarVOIToolBar( m_kImageA.getNDims(),
                    -1, bGPU, bGPU, kVOIGroup);
        m_kVOIToolbar.setVisible(false);
        m_kVOIManagers = new VOIManager[iNViews];
        Color kColor = toolbarBuilder.getVOIColorButton().getBackground();
        new ColorRGB( kColor.getRed()/255.0f,
                kColor.getGreen()/255.0f,
                kColor.getBlue()/255.0f );
        for ( int i = 0; i < iNViews; i++ )
        {
            m_kVOIManagers[i] = new VOIManager(this);
        }
        m_bGPURenderer = bGPU;
        initDataBuffer();
    }

    public void actionPerformed(ActionEvent event) {

        String command = event.getActionCommand();
        if ( command.equals("VOIColor") ) {
            colorChooser = new ViewJColorChooser(null, "Pick surface color", 
                    new OkColorListener(toolbarBuilder.getVOIColorButton()),
                    null);
        } else if (command.equals("NewVOI") ) {
            newVOI(true);
        } else if ( command.equals("LiveWireVOI") )
        {
            final JDialogLivewire dialog = new JDialogLivewire(null);
            if ( !dialog.isCancelled()) {
                boolean iActive = false;
                for (int i = 0; i < m_kVOIManagers.length; i++) {
                    m_kVOIManagers[i].liveWire( dialog.getSelection() );
                    iActive |= m_kVOIManagers[i].isActive();
                }
                m_kParent.PointerActive(iActive);
            }
        } else if (command.equals("undoVOI") ) {
            undoVOI();
        }  else if (command.equals("redoVOI") ) {
            redoVOI();
        } else if (command.equals("OpacityPaint")) {
            new JDialogOpacityControls(null, this, m_fOpacity);
        } else if ( command.equals("QuickMask" ) ) {
            saveImage("QuickMask");
            if ( m_akImageActive[0] != null ) {
                createMask(m_akImageActive[0], true);
            }
            if ( m_akImageActive[1] != null ) {
                createMask(m_akImageActive[1], true);
            }
        } else if ( command.equals("QuickMaskReverse" ) ) {
            saveImage("QuickMaskReverse");
            if ( m_akImageActive[0] != null ) {
                createMask(m_akImageActive[0], false);
            }
            if ( m_akImageActive[1] != null ) {
                createMask(m_akImageActive[1], false);
            }
        } else if (command.equals("3DVOIIntersect") ) {
            m_kParent.create3DVOI(true);
        } else if (command.equals("3DVOIUnion") ) {
            m_kParent.create3DVOI(false);
        } else if (command.equals("VOIProperties")) {
            showVOIProperties();
        } else {
            doVOI(command);
        }

    }


    public void addVOI( LocalVolumeVOI kNew, boolean bUpdate )
    {
        ModelImage kActive = m_kParent.getActiveImage();
        if ( kActive != null )
        {
            addVOI( kActive, kNew, bUpdate );
            if ( kActive.isRegistered( m_kCurrentVOIGroup ) == -1 )
            {
                kActive.register3DVOI( m_kCurrentVOIGroup );
            }            
        }
    }

    private void addVOI( ModelImage kImage, LocalVolumeVOI kNew, boolean bUpdate )
    {       
        if ( kNew.getGroup() == null )
        {
            saveVOIs("addVOI");
            if ( (m_kCurrentVOIGroup != null) && 
                    ( (m_kCurrentVOIGroup.getCurveType() != kNew.getType() ) 
                            || (kImage.isRegistered( m_kCurrentVOIGroup ) == -1)) )
            {
                m_kCurrentVOIGroup = null;
                newVOI(false);
            }
            if ( m_kCurrentVOIGroup == null )
            {
                short sID = (short)(kImage.get3DVOIs().size() + 1);
                m_kCurrentVOIGroup = new VOI( sID, kNew.getName(), kImage.getExtents()[2], kNew.getType(), -1f );
                m_kCurrentVOIGroup.setOpacity(1f);
                kImage.register3DVOI( m_kCurrentVOIGroup );
            }    
            kNew.setGroup( m_kCurrentVOIGroup );
            if ( kNew instanceof VOIPoint3D &&  kNew.getType() == VOI.POLYLINE_SLICE_3D )
            {
                VOIPoint3D kNewPoint = (VOIPoint3D)kNew;
                boolean bAdded = false;
                for ( int i = 0; i < m_kCurrentVOIGroup.getCurves()[0].size(); i++ )
                {
                    if ( m_kCurrentVOIGroup.getCurves()[0].get(i).isActive() )
                    {
                        ((VOIPolyLineSlice3D)m_kCurrentVOIGroup.getCurves()[0].get(i)).add(kNewPoint);
                        bAdded = true;
                        break;
                    }                    
                }
                if ( m_kCurrentVOIGroup.getCurves()[0].size() > 0 )
                {
                    ((VOIPolyLineSlice3D)m_kCurrentVOIGroup.getCurves()[0].get(0)).add(kNewPoint);
                    bAdded = true;
                }
                if ( !bAdded )
                {
                    m_kCurrentVOIGroup.getCurves()[0].add( new VOIPolyLineSlice3D(kNewPoint) );             
                }
            }
            else
            {
                m_kCurrentVOIGroup.getCurves()[0].add(kNew);
            }
        }
        else
        {
            System.err.println("Group not null");
            m_kCurrentVOIGroup = kNew.getGroup();
        }
        setCurrentColor();
        if ( bUpdate )
        {
            updateDisplay();
        }
    }


    public void createMask( ModelImage kActive, boolean bInside )
    {

        if (kActive.get3DVOIs().size() < 1) {
            MipavUtil.displayWarning("Must have at least one VOI to perform quick mask");
            return;
        }


        //ModelImage kImage = new ModelImage( ModelStorageBase.INTEGER, 
        //       m_kVolumeImage.GetImage().getExtents(), "Temp" );
        int iSize = kActive.getSize();
        if ( kActive.isColorImage() )
        {
            iSize /= 4;
        }
        kActive.createMask(iSize);
        boolean bMask = true;
        for (int i = 0; i < m_kVOIManagers.length; i++) {
            bMask &= make3DVOI( false, kActive, kActive, kActive.getMask(), i);
        }
        if ( !bMask )
        {
            return;
        }
        if ( !bInside )
        {
            kActive.getMask().flip(0, iSize );
        }
        kActive.useMask(true);
        m_kParent.updateData(false);
        //new ViewJFrameImage(kImage);

        if ( !m_bGPURenderer )
        {
            new JDialogMask(kActive, false, false, false);
        }
    }

    public void deleteVOI(LocalVolumeVOI kOld) {
        m_kCurrentVOIGroup = null;
        VOI kGroup = kOld.getGroup();
        kGroup.getCurves()[0].remove(kOld);
        if ( kGroup.isEmpty() )
        {
            if ( m_akImageActive[0] != null ) { m_akImageActive[0].unregister3DVOI(kGroup); }
            if ( m_akImageActive[1] != null ) { m_akImageActive[1].unregister3DVOI(kGroup); }
        }
        updateDisplay();
    }

    public void dispose() 
    {
        deleteAllVOI();
        m_kParent = null;
        m_kImageA = null;
        m_kImageB = null;
        m_akImageActive = null;
        toolbarBuilder = null;
        m_kVOIToolbar = null;
        colorChooser = null;
        m_kUndoCommands = null;
        m_kRedoCommands = null;

        for ( int i = 0; i < m_kVOIManagers.length; i++ )
        {
            m_kVOIManagers[i].dispose();
            m_kVOIManagers[i] = null;
        }
        m_kVOIManagers = null;   

        m_kImageAUndo = null;
        m_kImageARedo = null;        
        m_kImageBUndo = null;
        m_kImageBRedo = null;
    }

    public void doVOI( String kCommand )
    {
        boolean bDraw = isDrawCommand(kCommand);
        m_kParent.enableBoth(!bDraw);
        
        if ( kCommand.equals("PropVOIUp") )
        {
            saveVOIs("propUp");
            copy();
            Vector3f kCenter = m_kParent.PropUp(m_iActive);
            setCenter( kCenter );
            paste();
        }
        else if ( kCommand.equals("PropVOIDown") )
        {
            saveVOIs("propDown");
            copy();
            Vector3f kCenter = m_kParent.PropDown(m_iActive);
            setCenter( kCenter );
            paste();
        }
        else if (kCommand.equals("PropVOIAll") ) {
            saveVOIs("propAll");
            cut();
            pasteAll();
        }
        else if ( kCommand.equals("deleteVOI") ) 
        {
            saveVOIs( "deleteActiveVOI" );
            deleteActiveVOI();
        }
        else if ( kCommand.equals("deleteVOIActivePt") ) 
        {
            saveVOIs( "deleteVOIActivePt" );
            deleteVOIActivePt();
        }
        else if (kCommand.equals("selectAllVOIs") )
        {
            selectAllVOI(true);
        }
        else if (kCommand.equals("cutVOI") ) {
            saveVOIs("cut");
            cut();
        } 
        else if (kCommand.equals("copyVOI") ) {
            copy();
        } 
        else if (kCommand.equals("pasteVOI") ) {
            saveVOIs( "paste" );
            paste();
        }
        else if ( kCommand.equals("MoveUP") )
        {
            saveVOIs( "MoveUP" );
            moveVOI( new Vector3f( 0, 1, 0 ) );
        }
        else if ( kCommand.equals("MoveDown") )
        {
            saveVOIs( "MoveDown" );
            moveVOI( new Vector3f( 0,-1, 0 ) );
        }
        else if ( kCommand.equals("MoveLeft") )
        {
            saveVOIs( "MoveLeft" );
            moveVOI( new Vector3f(-1, 0, 0 ) );
        }
        else if ( kCommand.equals("MoveRight") )
        {
            saveVOIs( "MoveRight" );
            moveVOI( new Vector3f( 1, 0, 0 ) );
        }        


        boolean iActive = false;
        for (int i = 0; i < m_kVOIManagers.length; i++) {
            m_kVOIManagers[i].doVOI( kCommand, bDraw );
            iActive |= m_kVOIManagers[i].isActive();
        }
        m_kParent.PointerActive(iActive);
    }



    private boolean isDrawCommand( String kCommand )
    {
        if ( kCommand.equals("protractor") || kCommand.equals("LiveWireVOI") || kCommand.equals("SplitVOI") ||
                kCommand.equals("Line") || kCommand.equals("Polyslice") || kCommand.equals("Point") ||
                kCommand.equals("TextVOI") || kCommand.equals("Rect3DVOI") || kCommand.equals("RectVOI") ||
                kCommand.equals("EllipseVOI") || kCommand.equals("Polyline") || kCommand.equals("LevelSetVOI") ) {
            return true;
        } 
        return false;
    }
    
    
    public void showVOIProperties() {

        if (m_kVOIDialog == null) {
            //m_kVOIDialog = new JDialogVOIStats( m_kParent.getFrame(), m_kImageA, m_kCurrentVOIGroup );
            //m_kVOIDialog = new JDialogVOIStats( m_kImageA.getParentFrame(), m_kImageA, m_kCurrentVOIGroup );
            //addVOIUpdateListener(m_kVOIDialog);
        }

        if (m_kVOIDialog != null) {
            m_kVOIDialog.setVisible(true);
        }
    }

    public JToolBar getToolBar()
    {
        return m_kVOIToolbar;
    }

    public VOIManager getVOIManager(int i)
    {
        if ( i < m_kVOIManagers.length )
        {
            return m_kVOIManagers[i];
        }
        return null;
    }

    public void keyPressed(KeyEvent arg0) {
        // TODO Auto-generated method stub

    }

    public void keyReleased(KeyEvent arg0) {
        // TODO Auto-generated method stub

    }

    public void keyTyped(KeyEvent arg0) {
        // TODO Auto-generated method stub

    }


    public boolean make3DVOI( boolean bIntersection, ModelImage kVolume  )
    {
        boolean bCreated = true;
        if ( m_akImageActive[0] != null )
        {
            for (int i = 0; i < m_kVOIManagers.length; i++) {
                bCreated &= make3DVOI(bIntersection, m_akImageActive[0], kVolume, null, i);
            }
        }
        if ( m_akImageActive[1] != null )
        {
            for (int i = 0; i < m_kVOIManagers.length; i++) {
                bCreated &= make3DVOI(bIntersection, m_akImageActive[1], kVolume, null, i);
            }
        }
        return bCreated;
    }

    private boolean make3DVOI( boolean bIntersection, ModelImage kSrc, ModelImage kVolume, BitSet kMask, int iValue )
    {
        VOIVector kVOIs = kSrc.get3DVOIs();

        boolean bCreated = false;
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves()[0].size(); j++ )
            {
                LocalVolumeVOI kCurrentVOI = (LocalVolumeVOI)kCurrentGroup.getCurves()[0].get(j);
                VOIManager kManager = m_kVOIManagers[0];
                if ( kCurrentVOI.getOrientation() == m_kVOIManagers[1].getOrientation() )
                {
                    kManager = m_kVOIManagers[1];
                }
                else if ( kCurrentVOI.getOrientation() == m_kVOIManagers[2].getOrientation() )
                {
                    kManager = m_kVOIManagers[2];
                }
                if ( kCurrentVOI.isActive() )
                {
                    bCreated = true;
                    kManager.fillVolume(  kCurrentVOI, kVolume, kMask, bIntersection, iValue );     
                }
            }
        }
        updateDisplay();        
        return bCreated;
    }


    public void mouseClicked(MouseEvent event) {

        if (event.getButton() == MouseEvent.BUTTON3) {

            if (event.getSource() instanceof AbstractButton) {
                AbstractButton btnSource = (AbstractButton) event.getSource();
                if ( btnSource.getActionCommand().equals("QuickMask")
                        || btnSource.getActionCommand().equals("QuickMaskReverse")) {
                    ViewJFrameBase.handleMaskPopupMenu((Component) event.getSource(), event);
                } 
            }
        }
    }

    public void mouseEntered(MouseEvent e) {
        // TODO Auto-generated method stub

    }


    public void mouseExited(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    public void mousePressed(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    public void mouseReleased(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    public void newVOI( boolean bPropagate )
    {
        selectAllVOI(false);
        if ( bPropagate )
        {
            doVOI("NewVOI");
        }
        //voiUID = m_kVOIManagers[m_iActive].getVOICount() + 1;
        m_kCurrentVOIGroup = null;
        voiUID++;
        toolbarBuilder.getVOIColorButton().setVOIColor(voiUID);
        setButtonColor(toolbarBuilder.getVOIColorButton(), 
                toolbarBuilder.getVOIColorButton().getBackground() );
    }

    public void pasteVOI(LocalVolumeVOI kNew)
    {
        kNew.getGroup().getCurves()[0].add(kNew);
        for ( int i = 0; i < m_akImageActive.length; i++ )
        {
            if ( m_akImageActive[i] != null )
            {
                if ( m_akImageActive[i].isRegistered(kNew.getGroup()) == -1 )
                {
                    m_akImageActive[i].register3DVOI(kNew.getGroup());
                }
            }
        }
        updateDisplay();
    }

    public void setActive( VOIManager kManager )
    {
        for ( int i = 0; i < m_kVOIManagers.length; i++ )
        {
            if ( kManager == m_kVOIManagers[i] )
            {
                m_iActive = i;
                break;
            }
        }
        m_akImageActive = m_kVOIManagers[m_iActive].getActiveImage();
        if ( m_akImageActive[0] != null && m_akImageActive[1] != null )
        {
        }
        else if ( m_akImageActive[0] != null )
        {
            m_kParent.setActiveImage( ViewJComponentBase.IMAGE_A );
        }
        else if ( m_akImageActive[1] != null )
        {
            m_kParent.setActiveImage( ViewJComponentBase.IMAGE_B );
        }
    }

    /**
     * Set the color of the button. Derived classes may also perform other functions.
     * @param _button button.
     * @param _color color.
     */
    public void setButtonColor(JButton _button, Color _color)
    {
        if ( (_button != null) && (_color != null) )
        {
            _button.setBackground(_color);
        }
        new ColorRGB( _color.getRed()/255.0f,
                _color.getGreen()/255.0f,
                _color.getBlue()/255.0f );
        if ( m_kCurrentVOIGroup != null )
        {
            m_kCurrentVOIGroup.setColor( _color );
            for ( int i = 0; i < m_kCurrentVOIGroup.getCurves()[0].size(); i++ )
            {
                LocalVolumeVOI kVOI3D = ((LocalVolumeVOI)m_kCurrentVOIGroup.getCurves()[0].get(i));
                kVOI3D.update( new ColorRGBA( _color.getRed()/255f, _color.getGreen()/255f, _color.getBlue()/255f, 
                        m_kCurrentVOIGroup.getOpacity()));
            }
        }
        updateDisplay();
    }

    public void setCenter( Vector3f center )
    {
        for (int i = 0; i < m_kVOIManagers.length; i++)
        {
            m_kVOIManagers[i].setCenter(center);
        }
    }

    public void setCurrentColor( )
    {
        setButtonColor(toolbarBuilder.getVOIColorButton(), 
                toolbarBuilder.getVOIColorButton().getBackground() );
    }


    public void setCursor(Cursor kCursor) {
        m_kParent.setCursor(kCursor);    
    }

    public void setDefaultCursor() {
        m_kParent.setDefaultCursor();     
        doVOI("");
    }

    public void setOpacity( float fVal )
    {
        m_fOpacity = fVal;
        if ( m_kCurrentVOIGroup != null )
        {
            m_kCurrentVOIGroup.setOpacity( m_fOpacity );
        }
    }

    public void setSelectedVOI( VOI kSelected, boolean bSelectAll )
    {
        if ( kSelected == null )
        {
            return;
        }
        if ( (m_kCurrentVOIGroup != null) && (m_kCurrentVOIGroup != kSelected) )
        {
            m_kCurrentVOIGroup.setAllActive(false);
        }
        m_kCurrentVOIGroup = kSelected;
        m_kCurrentVOIGroup.setAllActive(bSelectAll);
        m_kCurrentVOIGroup.setActive(true);
        setButtonColor(toolbarBuilder.getVOIColorButton(), 
                m_kCurrentVOIGroup.getColor());


        m_fOpacity = m_kCurrentVOIGroup.getOpacity();
    }

    public void undoVOI()
    {
        if ( m_kUndoCommands.isEmpty() )
        {
            return;
        }        
        String lastCommand = m_kUndoCommands.remove( m_kUndoCommands.size() -1 );
        m_kRedoCommands.add(lastCommand);
        //System.err.println(lastCommand);
        if ( lastCommand.equals( "QuickMask" ) || lastCommand.equals( "QuickMaskReverse" ) )
        {
            if ( m_bGPURenderer )
            {
                if ( m_akImageActive[0] != null ) { m_akImageActive[0].useMask(false); }
                if ( m_akImageActive[1] != null ) { m_akImageActive[1].useMask(false); }
            }
            else
            {
                undoImage();
            }
            m_kParent.updateData(false);
        }
        else
        {
            undoVOIs();
        }
    }

    public void redoVOI()
    {
        if ( m_kRedoCommands.isEmpty() )
        {
            return;
        }        
        String lastCommand = m_kRedoCommands.remove( m_kRedoCommands.size() -1 );
        //System.err.println(lastCommand);
        m_kUndoCommands.add(lastCommand);
        if ( lastCommand.equals( "QuickMask" ) || lastCommand.equals( "QuickMaskReverse" ) )
        {
            if ( m_bGPURenderer )
            {                
                if ( m_akImageActive[0] != null ) { m_akImageActive[0].useMask(true); }
                if ( m_akImageActive[1] != null ) { m_akImageActive[1].useMask(true); }
            }
            else
            {
                redoImage();
            }
            m_kParent.updateData(false);
        }
        else
        {
            redoVOIs();
        }
    }

    public void updateDisplay() {
        m_kParent.setModified(); 
    }

    private void deleteAllVOI()
    {
        if ( m_akImageActive[0] != null ) { m_akImageActive[0].unregisterAll3DVOIs(); }
        if ( m_akImageActive[1] != null ) { m_akImageActive[1].unregisterAll3DVOIs(); }
    }

    private void cut()
    {
        copy();
        deleteActiveVOI();
    }

    private void copy()
    {
        m_kCopyList.clear();
        for ( int iImage = 0; iImage < m_akImageActive.length; iImage++ )
        {
            if ( m_akImageActive[iImage] == null )
            {
                continue;
            }
            VOIVector kVOIs = m_akImageActive[iImage].get3DVOIs();
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                for ( int j = 0; j < kCurrentGroup.getCurves()[0].size(); j++ )
                {
                    LocalVolumeVOI kCurrentVOI = (LocalVolumeVOI)kCurrentGroup.getCurves()[0].get(j);
                    if ( kCurrentVOI.isActive() && !m_kCopyList.contains(kCurrentVOI) )
                    {
                        m_kCopyList.add(kCurrentVOI);
                    }
                }
            }
        }
    }

    private void paste()
    {
        for ( int i = 0; i < m_kCopyList.size(); i++ )
        {
            LocalVolumeVOI kCurrentVOI = m_kCopyList.get(i); 
            VOIManager kManager = m_kVOIManagers[0];
            if ( kCurrentVOI.getOrientation() == m_kVOIManagers[1].getOrientation() )
            {
                kManager = m_kVOIManagers[1];
            }
            else if ( kCurrentVOI.getOrientation() == m_kVOIManagers[2].getOrientation() )
            {
                kManager = m_kVOIManagers[2];
            }

            kManager.pasteVOI( kCurrentVOI );  
        }
    }

    private void pasteAll()
    {
        for ( int i = 0; i < m_kCopyList.size(); i++ )
        {
            LocalVolumeVOI kCurrentVOI = m_kCopyList.get(i); 
            VOIManager kManager = m_kVOIManagers[0];
            if ( kCurrentVOI.getOrientation() == m_kVOIManagers[1].getOrientation() )
            {
                kManager = m_kVOIManagers[1];
            }
            else if ( kCurrentVOI.getOrientation() == m_kVOIManagers[2].getOrientation() )
            {
                kManager = m_kVOIManagers[2];
            }
            kManager.pasteAllVOI( kCurrentVOI );  
        }
    }

    private void deleteVOIActivePt()
    {
        Vector<LocalVolumeVOI> activeList = new Vector<LocalVolumeVOI>();

        for ( int iImage = 0; iImage < m_akImageActive.length; iImage++ )
        {
            if ( m_akImageActive[iImage] == null )
            {
                continue;
            }
            VOIVector kVOIs = m_akImageActive[iImage].get3DVOIs();
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                for ( int j = 0; j < kCurrentGroup.getCurves()[0].size(); j++ )
                {
                    LocalVolumeVOI kCurrentVOI = (LocalVolumeVOI)kCurrentGroup.getCurves()[0].get(j);
                    if ( kCurrentVOI.isActive() && !activeList.contains(kCurrentVOI) )
                    {
                        activeList.add(kCurrentVOI);
                    }
                }
            }
        }
        
        Vector<LocalVolumeVOI> deleteList = new Vector<LocalVolumeVOI>();
        for ( int i = 0; i < activeList.size(); i++ )
        {
            LocalVolumeVOI kCurrentVOI = activeList.get(i);
            VOIManager kManager = m_kVOIManagers[0];
            if ( kCurrentVOI.getOrientation() == m_kVOIManagers[1].getOrientation() )
            {
                kManager = m_kVOIManagers[1];
            }
            else if ( kCurrentVOI.getOrientation() == m_kVOIManagers[2].getOrientation() )
            {
                kManager = m_kVOIManagers[2];
            }
            if ( kManager.deleteVOIActivePt( kCurrentVOI ) <= 0 )
            {
                deleteList.add( kCurrentVOI );
            }            
        }

        while ( deleteList.size() > 0 )
        {
            deleteVOI( deleteList.remove(0) );
        }
        updateDisplay();
    }

    private void deleteActiveVOI()
    {
        Vector<LocalVolumeVOI> deleteList = new Vector<LocalVolumeVOI>();

        for ( int iImage = 0; iImage < m_akImageActive.length; iImage++ )
        {
            if ( m_akImageActive[iImage] == null )
            {
                continue;
            }
            VOIVector kVOIs = m_akImageActive[iImage].get3DVOIs();
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                for ( int j = 0; j < kCurrentGroup.getCurves()[0].size(); j++ )
                {
                    LocalVolumeVOI kCurrentVOI = (LocalVolumeVOI)kCurrentGroup.getCurves()[0].get(j);
                    if ( kCurrentVOI.isActive() && !deleteList.contains(kCurrentVOI) )
                    {
                        deleteList.add(kCurrentVOI);
                    }
                }
            }
        }
        while ( deleteList.size() > 0 )
        {
            deleteVOI( deleteList.remove(0) );
        }
    }


    private void selectAllVOI(boolean bActive)
    {
        for ( int iImage = 0; iImage < m_akImageActive.length; iImage++ )
        {
            if ( m_akImageActive[iImage] != null )
            {
                VOIVector kVOIs = m_akImageActive[iImage].get3DVOIs();
                for ( int i = 0; i < kVOIs.size(); i++ )
                {
                    VOI kCurrentGroup = kVOIs.get(i);
                    kCurrentGroup.setAllActive(bActive);
                }
            }
        }
    }
    
    
    public void moveVOI( Vector3f kDiff )
    {
        Vector3f[] akBounds = new Vector3f[2];
        akBounds[0] = new Vector3f();
        akBounds[1] = new Vector3f();
        boolean bFirst = true;
        Vector<LocalVolumeVOI> activeList = new Vector<LocalVolumeVOI>();
        for ( int iImage = 0; iImage < m_akImageActive.length; iImage++ )
        {
            if ( m_akImageActive[iImage] == null )
            {
                continue;
            }

            VOIVector kVOIs = m_akImageActive[iImage].get3DVOIs();
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                for ( int j = 0; j < kCurrentGroup.getCurves()[0].size(); j++ )
                {
                    LocalVolumeVOI kCurrentVOI = (LocalVolumeVOI)kCurrentGroup.getCurves()[0].get(j);
                    if ( kCurrentVOI.isActive() && !activeList.contains(kCurrentVOI) )
                    {
                        activeList.add( kCurrentVOI );
                        Vector3f[] kBounds = kCurrentVOI.getBoundingBox();
                        if ( bFirst )
                        {
                            bFirst = false;
                            akBounds[0].Copy(kBounds[0]);
                            akBounds[1].Copy(kBounds[1]);
                        }
                        akBounds[0].Min(kBounds[0]);
                        akBounds[1].Max(kBounds[1]);
                    }
                }
            }
        }

        for ( int i = 0; i < activeList.size(); i++ )
        {
            activeList.get(i).move( kDiff, akBounds );
        }

        updateDisplay();
    }

    private void initDataBuffer()
    {
        int iSize = m_kImageA.getExtents()[0]*m_kImageA.getExtents()[1]*m_kImageA.getExtents()[2];
        m_aucBufferA = new byte[iSize];
        if ( m_kImageA.isColorImage() )
        {
            iSize *= 4;
            byte[] aucTemp = new byte[iSize];
            try {
                m_kImageA.exportData( 0, iSize, aucTemp );
            } catch (IOException e) {
                e.printStackTrace();
            }
            for ( int i = 0; i < m_aucBufferA.length; i++ )
            {
                m_aucBufferA[i] = (byte)((aucTemp[i*4 + 1] + aucTemp[i*4 + 2] + aucTemp[i*4 + 3])/3.0f);
            }
        }
        else
        {
            try {
                m_kImageA.exportData( 0, iSize, m_aucBufferA );
            } catch (IOException e) {
                e.printStackTrace();
            } 
        }
        if ( m_kImageB != null )
        {
            iSize = m_kImageB.getExtents()[0]*m_kImageB.getExtents()[1]*m_kImageB.getExtents()[2];
            m_aucBufferB = new byte[iSize];
            if ( m_kImageB.isColorImage() )
            {
                iSize *= 4;
                byte[] aucTemp = new byte[iSize];
                try {
                    m_kImageB.exportData( 0, iSize, aucTemp );
                } catch (IOException e) {
                    e.printStackTrace();
                }
                for ( int i = 0; i < m_aucBufferB.length; i++ )
                {
                    m_aucBufferB[i] = (byte)((aucTemp[i*4 + 1] + aucTemp[i*4 + 2] + aucTemp[i*4 + 3])/3.0f);
                }
            }
            else
            {
                try {
                    m_kImageB.exportData( 0, iSize, m_aucBufferB );
                } catch (IOException e) {
                    e.printStackTrace();
                } 
            }
        }
        for ( int i = 0; i < m_kVOIManagers.length; i++ )
        {
            m_kVOIManagers[i].setDataBuffers( m_aucBufferA, m_aucBufferB );
        }
    }


    public VOISaveState getVOIState( )
    {
        VOISaveState kVOIState = new VOISaveState();
        kVOIState.voiVectorA = m_kImageA.get3DVOIsCopy();
        if ( m_kImageB != null )
        {
            kVOIState.voiVectorB = m_kImageB.get3DVOIsCopy();
        }
        if ( m_kCurrentVOIGroup != null )
        {
            kVOIState.currentVOI = m_kImageA.isRegistered( m_kCurrentVOIGroup );
        }
        else
        {
            kVOIState.currentVOI = -1;
        }        
        kVOIState.currentCenter.Copy( m_kParent.getCenterPt() );
        return kVOIState;
    }

    public void setVOIState( VOISaveState kVOIState )
    {
        m_kImageA.unregisterAll3DVOIs();
        m_kImageA.set3DVOIs( kVOIState.voiVectorA );
        if ( m_kImageB != null )
        {
            m_kImageB.unregisterAll3DVOIs();
            m_kImageB.set3DVOIs( kVOIState.voiVectorB );
        }
        if ( kVOIState.currentVOI != -1 )
        {
            m_kCurrentVOIGroup = m_kImageA.get3DVOIs().get(kVOIState.currentVOI);
            setSelectedVOI( m_kCurrentVOIGroup, m_kCurrentVOIGroup.isAllActive() );
        }
        else
        {
            m_kCurrentVOIGroup = null;
        }        
        m_kParent.setCenter( new Vector3f( kVOIState.currentCenter ) );
    }

    public void saveVOIs( String kCommand )
    {
        m_kUndoCommands.add( kCommand );
        m_kUndoList.add( getVOIState() );
        m_kRedoCommands.clear();
        m_kRedoList.clear();
    }


    public void saveImage( String kCommand )
    {
        m_kUndoCommands.add( kCommand );

        if ( m_kImageAUndo == null )
        {
            try {
                m_kImageAUndo = m_kImageA.exportData(0, m_kImageA.getSize() );
            } catch (IOException e) {}
        }
        if ( m_kImageB != null )
        {
            if ( m_kImageBUndo == null )
            {
                try {
                    m_kImageBUndo = m_kImageB.exportData(0, m_kImageB.getSize() );
                } catch (IOException e) {}
            }
        }

        m_kRedoCommands.clear();
        m_kRedoList.clear();
    }

    public void undoImage( )
    {
        if ( m_kImageAUndo == null )
        {
            return;
        }
        setCursor( MipavUtil.waitCursor );
        try {
            m_kImageARedo = m_kImageA.exportData(0, m_kImageA.getSize() );
            m_kImageA.importData(m_kImageAUndo);
            if ( m_kImageB != null && m_kImageBUndo != null )
            {
                m_kImageBRedo = m_kImageB.exportData(0, m_kImageB.getSize() );
                m_kImageB.importData(m_kImageBUndo);
            }
        } catch (IOException e) {}
        updateDisplay();
    }

    public void redoImage( )
    {
        if ( m_kImageARedo == null )
        {
            return;
        }
        setCursor( MipavUtil.waitCursor );
        try {
            m_kImageA.importData(m_kImageARedo);
            if ( m_kImageB != null && m_kImageBRedo != null)
            {
                m_kImageB.importData(m_kImageBRedo);
            }
        } catch (IOException e) {}
        m_kImageARedo = null;
        m_kImageBRedo = null;
        updateDisplay();
    }

    private void undoVOIs()
    {
        if ( m_kUndoList.isEmpty() )
        {
            return;
        }
        m_kRedoList.add( getVOIState() );
        setVOIState( m_kUndoList.remove( m_kUndoList.size() - 1) );
        updateDisplay();
    }

    private void redoVOIs()
    {
        if ( m_kRedoList.isEmpty() )
        {
            return;
        }
        m_kUndoList.add( getVOIState() );
        setVOIState( m_kRedoList.remove( m_kRedoList.size() - 1) );
        updateDisplay();
    }


    /*
     * 
    public LocalVolumeVOI getCurrentVOI()
    {
        if ( m_kVOIManager != null )
        {
            return m_kVOIManager.getCurrentVOI();
        }
        return null;
    }

    public void setCurrentVOI( LocalVolumeVOI kCurrentVOI )
    {
        if ( m_kVOIManager != null )
        {
            m_kVOIManager.setCurrentVOI( kCurrentVOI );
        }
        doVOI("");
    }

    public LocalVolumeVOIVector[] getVOICopy()
    {
        if ( m_kVOIManager != null )
        {
            return m_kVOIManager.getVOICopy();
        }
        return null;
    }

    public void setVOICopy(LocalVolumeVOIVector[] kList)
    {       
        if ( m_kVOIManager != null )
        {
            m_kVOIManager.setVOICopy(kList);
        }
        doVOI("");
    }
     */
}
