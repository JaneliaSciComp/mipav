package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.dialogs.JDialogLivewire;
import gov.nih.mipav.view.dialogs.JDialogMask;
import gov.nih.mipav.view.dialogs.JDialogOpacityControls;
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
import java.util.BitSet;
import java.util.Vector;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JToolBar;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
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
            Color color = colorChooser.getColor();

            button.setBackground(color);
            setButtonColor(button, color);
        }
    }
    private VOIManagerInterfaceListener m_kParent = null;
    private ModelImage m_kImage;
    private ViewToolBarBuilder toolbarBuilder;

    private JToolBar m_kVOIToolbar;
    /** Reference to the color chooser. */
    private ViewJColorChooser colorChooser;
    private VOIManager[] m_kVOIManagers;
    private int m_iActive = 0;

    private int voiUID = 0;

    private VOI m_kCurrentVOIGroup = null;
    private Vector<String> m_kCommands = new Vector<String>();
    private float m_fOpacity = 1f;

    private boolean m_bGPURenderer = false;

    public VOIManagerInterface ( VOIManagerInterfaceListener kParent,
            ModelImage kImage, int iNViews, boolean bGPU )
    {
        m_kParent = kParent;
        m_kImage = kImage;

        toolbarBuilder = new ViewToolBarBuilder(this);
        m_kVOIToolbar =
            toolbarBuilder.buildVolumeTriPlanarVOIToolBar( m_kImage.getNDims(),
                    -1);
        m_kVOIToolbar.setVisible(false);
        m_kVOIManagers = new VOIManager[iNViews];
        Color kColor = toolbarBuilder.getVOIColorButton().getBackground();
        new ColorRGB( kColor.getRed()/255.0f,
                kColor.getGreen()/255.0f,
                kColor.getBlue()/255.0f );
        for ( int i = 0; i < iNViews; i++ )
        {
            m_kVOIManagers[i] = new VOIManager(this, m_kImage);
        }
        m_bGPURenderer = bGPU;
    }

    public void actionPerformed(ActionEvent event) {

        String command = event.getActionCommand();
        m_kCommands.add( command );
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
        } else if (command.equals("deleteAllVOI") ) {
            deleteAllVOI();
        }  else if (command.equals("undoVOI") ) {
            undoVOI();
        }  else if (command.equals("RectVOI") ) {
            doVOI(command);
        } else if (command.equals("EllipseVOI") ) {
            doVOI(command);
        } else if (command.equals("Polyline") ) {
            doVOI(command);
        }  else if (command.equals("VOIColor") ) {
            doVOI(command);
        } else if (command.equals("OpacityPaint")) {
            new JDialogOpacityControls(null, this, m_fOpacity);
        } else if (command.equals("LevelSetVOI") ) {
            doVOI(command);
        } else if (command.equals("deleteAllVOI") ) {
            doVOI(command);
        } else if (command.equals("deleteVOI") ) {
            doVOI(command);
        }  else if (command.equals("cutVOI") ) {
            doVOI(command);
        } else if (command.equals("copyVOI") ) {
            doVOI(command);
        } else if (command.equals("pasteVOI") ) {
            doVOI(command);
        } else if (command.equals("PropVOIUp") ) {
            doVOI(command);
        } else if (command.equals("PropVOIDown") ) {
            doVOI(command);
        } else if (command.equals("PropVOIAll") ) {
            doVOI(command);
        } else if (command.equals("Pointer") ) {
            doVOI(command);
        } else if (command.equals("Default") ) {
            doVOI(command);
        } else if ( command.equals("QuickMask" ) ) {
            createMask(true);
        } else if ( command.equals("QuickMaskReverse" ) ) {
            createMask(false);
        } else if (command.equals("3DVOIIntersect") ) {
            m_kParent.create3DVOI(true);
        } else if (command.equals("3DVOIUnion") ) {
            m_kParent.create3DVOI(false);
        } else {
            doVOI(command);
        }

    }


    public void addVOI( LocalVolumeVOI kNew, boolean bUpdate )
    {
        if ( kNew.getGroup() == null )
        {
            if ( (m_kCurrentVOIGroup != null) &&  (m_kCurrentVOIGroup.getCurveType() != kNew.getType()) )
            {
                m_kCurrentVOIGroup = null;
                newVOI(false);
            }
            if ( m_kCurrentVOIGroup == null )
            {
                short sID = (short)(m_kImage.get3DVOIs().size() + 1);
                m_kCurrentVOIGroup = new VOI( sID, kNew.getName(), m_kImage.getExtents()[2], kNew.getType(), -1f );
                m_kCurrentVOIGroup.setOpacity(1f);
                m_kImage.register3DVOI( m_kCurrentVOIGroup );
            }    
            kNew.setGroup( m_kCurrentVOIGroup );
            if ( kNew instanceof VOIPoint3D &&  kNew.getType() == VOI.POLYLINE_SLICE )
            {
                VOIPoint3D kNewPoint = (VOIPoint3D)kNew;
                boolean bAdded = false;
                for ( int i = 0; i < m_kCurrentVOIGroup.getCurves()[0].size(); i++ )
                {
                    if ( m_kCurrentVOIGroup.getCurves()[0].get(i).isActive() )
                    {
                        ((VOIPolyLineSlice3D)m_kCurrentVOIGroup.getCurves()[0].get(i)).add(kNewPoint);
                        bAdded = true;
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
        else if ( kNew.getGroup().getCurveType() != kNew.getType() )
        {
            m_kCurrentVOIGroup = kNew.getGroup();
            m_kCurrentVOIGroup.getCurves()[0].remove(kNew);
            kNew.setGroup(null);
            addVOI(kNew, bUpdate);
        }
        setCurrentColor();
        if ( bUpdate )
        {
            updateDisplay();
        }
    }



    public void createMask( boolean bInside )
    {

        if (m_kImage.get3DVOIs().size() < 1) {
            MipavUtil.displayWarning("Must have at least one VOI to perform quick mask");
            return;
        }

        
        //ModelImage kImage = new ModelImage( ModelStorageBase.INTEGER, 
        //       m_kVolumeImage.GetImage().getExtents(), "Temp" );
        int iSize = m_kImage.getSize();
        if ( m_kImage.isColorImage() )
        {
            iSize /= 4;
        }
        m_kImage.createMask(iSize);
        for (int i = 0; i < m_kVOIManagers.length; i++) {
            make3DVOI( false, m_kImage, m_kImage.getMask(), i);
        }
        if ( !bInside )
        {
            m_kImage.getMask().flip(0, iSize );
        }
        m_kImage.useMask(true);
        m_kParent.updateData(false);
        //new ViewJFrameImage(kImage);
        
        if ( !m_bGPURenderer )
        {
            new JDialogMask(m_kImage, false, false, false);
        }
    }

    public void deleteVOI(LocalVolumeVOI kOld) {
        m_kCurrentVOIGroup = null;
        VOI kGroup = kOld.getGroup();
        kGroup.getCurves()[0].remove(kOld);
        if ( kGroup.isEmpty() )
        {
            m_kImage.unregister3DVOI(kGroup);
        }
        updateDisplay();
    }

    public void doVOI( String kCommand )
    {
        if ( kCommand.equals("PropVOIUp") )
        {
            m_kVOIManagers[m_iActive].doVOI( "copyVOI" );
            Vector3f kCenter = m_kParent.PropUp(m_iActive);
            setCenter( kCenter );
            m_kVOIManagers[m_iActive].doVOI( "pasteVOI" );
        }
        else if ( kCommand.equals("PropVOIDown") )
        {
            m_kVOIManagers[m_iActive].doVOI( "copyVOI" );
            Vector3f kCenter = m_kParent.PropDown(m_iActive);
            setCenter( kCenter );
            m_kVOIManagers[m_iActive].doVOI( "pasteVOI" );
        }
        boolean iActive = false;
        for (int i = 0; i < m_kVOIManagers.length; i++) {
            m_kVOIManagers[i].doVOI( kCommand );
            iActive |= m_kVOIManagers[i].isActive();
        }
        m_kParent.PointerActive(iActive);
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


    public void make3DVOI( boolean bIntersection, ModelImage kVolume  )
    {
        for (int i = 0; i < m_kVOIManagers.length; i++) {
            make3DVOI(bIntersection, kVolume, null, i);
        }
    }

    public void make3DVOI( boolean bIntersection, ModelImage kVolume, BitSet kMask, int iValue )
    {

        VOIVector kVOIs = m_kImage.get3DVOIs();
        while ( kVOIs.size() > 0 )
        {
            VOI kCurrentGroup = kVOIs.remove(0);
            while ( kCurrentGroup.getCurves()[0].size() > 0 )
            {
                LocalVolumeVOI kCurrentVOI = (LocalVolumeVOI)kCurrentGroup.getCurves()[0].remove(0); 
                VOIManager kManager = m_kVOIManagers[0];
                if ( kCurrentVOI.getOrientation() == m_kVOIManagers[1].getOrientation() )
                {
                    kManager = m_kVOIManagers[1];
                }
                else if ( kCurrentVOI.getOrientation() == m_kVOIManagers[2].getOrientation() )
                {
                    kManager = m_kVOIManagers[2];
                }

                kManager.fillVolume(  kCurrentVOI, kVolume, kMask, bIntersection, iValue );     
                deleteVOI( kCurrentVOI );
            }
            kCurrentGroup = null;
        }
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
        //m_kParent.setVOIColor( kColor );
        if ( m_kCurrentVOIGroup != null )
        {
            m_kCurrentVOIGroup.setColor( _color );
            for ( int i = 0; i < m_kCurrentVOIGroup.getCurves()[0].size(); i++ )
            {
                LocalVolumeVOI kVOI3D = ((LocalVolumeVOI)m_kCurrentVOIGroup.getCurves()[0].get(i));
                kVOI3D.update();
            }
        }
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
        m_kCommands.remove( m_kCommands.size() -1 );
        if ( m_kCommands.size() > 0 ) 
        {
            String lastCommand = m_kCommands.remove( m_kCommands.size() -1 );
            if ( lastCommand.equals( "QuickMask" ) || lastCommand.equals( "QuickMaskReverse" ) )
            {
                System.err.println( "undo Mask" );
                m_kImage.useMask(false);
                m_kParent.updateData(false);
            }
        }
    }

    public void updateDisplay() {
        m_kParent.setModified(); 
    }

    private void deleteAllVOI()
    {
        m_kImage.unregisterAll3DVOIs();
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
