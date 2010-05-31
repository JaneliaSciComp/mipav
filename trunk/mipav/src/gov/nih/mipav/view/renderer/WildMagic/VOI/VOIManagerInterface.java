package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.UpdateVOIEvent;
import gov.nih.mipav.model.structures.UpdateVOISelectionListener;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIPolyLineSlice;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.VOIHandlerInterface;
import gov.nih.mipav.view.ViewControlsImage;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewJPopupPt;
import gov.nih.mipav.view.ViewJPopupVOI;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogLivewire;
import gov.nih.mipav.view.dialogs.JDialogMask;
import gov.nih.mipav.view.dialogs.JDialogOpacityControls;
import gov.nih.mipav.view.dialogs.JDialogPointArea;
import gov.nih.mipav.view.dialogs.JDialogVOIStats;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.event.EventListenerList;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;



//import com.mentorgen.tools.profile.runtime.Profile;
// -javaagent:E:\MagicConsulting\mipav\src\lib\profile.jar
// -Dprofile.properties=E:\MagicConsulting\mipav\src\lib\profile.properties


public class VOIManagerInterface implements ActionListener, VOIManagerListener, VOIHandlerInterface
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
            saveVOIs( CustomUIBuilder.PARAM_VOI_COLOR.getActionCommand() );

            Color color = colorChooser.getColor();

            button.setBackground(color);
            setButtonColor(button, color);
        }
    }
    private VOIManagerInterfaceListener m_kParent = null;
    private ModelImage m_kImageA;
    private ModelImage m_kImageB;

    private ModelStorageBase m_kLUTa;
    private ModelStorageBase m_kLUTb;


    private ViewToolBarBuilder toolbarBuilder;

    private JToolBar m_kVOIToolbar;
    /** Reference to the color chooser. */
    protected ViewJColorChooser colorChooser;
    private VOIManager[] m_kVOIManagers;
    private int m_iActive = 0;

    private int voiUID = 0;

    private VOI m_kCurrentVOIGroup = null;
    private Vector<String> m_kUndoCommands = new Vector<String>();
    private Vector<String> m_kRedoCommands = new Vector<String>();
    private float m_fOpacity = 1f;

    private boolean m_bGPURenderer = false;

    private JDialogVOIStats m_kVOIDialog;


    private Vector<VOIBase> m_kCopyList = new Vector<VOIBase>();

    private Vector<VOISaveState> m_kUndoList = new Vector<VOISaveState>();
    private Vector<VOISaveState> m_kRedoList = new Vector<VOISaveState>();

    private Object m_kImageAUndo = null;
    private Object m_kImageARedo = null;
    private Object m_kImageBUndo = null;
    private Object m_kImageBRedo = null;

    private Vector3f[] m_akBounds = new Vector3f[]{ new Vector3f(), new Vector3f() };

    private boolean m_bEnabled = true;

    /** Flag to indicate if DICOM overlay should be displayed. */
    protected boolean overlayOn = false;


    /**
     * created to handle VOI updates. Must fireVOIUpdate(...) to get listeners
     * to handle the update. Perhaps better location for the VOIupdate is in
     * <code>ViewJCompoenentEditImage</code>, but this listenerlist will handle
     * listeners of more than one type.
     */
    protected EventListenerList listenerList = new EventListenerList();




    /** Popup Menu for VOIs (non-point). */
    protected ViewJPopupVOI popup = null;

    /** Popup Menu for VOIPoints. */
    protected ViewJPopupPt popupPt = null;

    protected Color currentColor = null;
    protected VOI saveGroup = null;
    
    private Vector<VOIBase> m_kActiveList = new Vector<VOIBase>();

    private JToggleButton m_kPointerButton = null;
    
    
    public VOIManagerInterface ( VOIManagerInterfaceListener kParent,
            ModelImage kImageA, ModelStorageBase kLUTa, ModelImage kImageB, ModelStorageBase kLUTb, int iNViews, boolean bGPU, ButtonGroup kVOIGroup )
    {
        m_kParent = kParent;
        m_kImageA = kImageA;
        m_kImageB = kImageB;        

        m_kLUTa = kLUTa;
        m_kLUTb = kLUTb;

        toolbarBuilder = new ViewToolBarBuilder(this);
        m_kVOIToolbar =
            toolbarBuilder.buildVolumeTriPlanarVOIToolBar( m_kImageA.getNDims(),
                    -1, bGPU, bGPU, kVOIGroup);
        m_kVOIToolbar.setVisible(false);
        m_kPointerButton = toolbarBuilder.getPointerButton();
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


        /**
         * Create Popup Dialogs for VOIs and VOI points
         */
        popup = new ViewJPopupVOI(this);

        if (m_kParent.getActiveImage().getNDims() < 3) {
            popup.setEnabledPropagate(false);
        }

        popupPt = new ViewJPopupPt(this);

        if (m_kParent.getActiveImage().getNDims() < 3) {
            popupPt.setEnabledGraph(false);
            popupPt.setEnabledProp(false);
        }

        for ( int i = 0; i < iNViews; i++ )
        {
            m_kVOIManagers[i].setPopupVOI(popup);
            m_kVOIManagers[i].setPopupPt(popupPt);
        }
    }

    public void actionPerformed(ActionEvent event) {

        String command = event.getActionCommand();
        if ( command.equals(CustomUIBuilder.PARAM_VOI_COLOR.getActionCommand()) ) {
            showColorDialog();
            setDefaultCursor();
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_NEW.getActionCommand()) ) {
            newVOI(true, false);
            setDefaultCursor();
        } else if ( command.equals(CustomUIBuilder.PARAM_VOI_LIVEWIRE.getActionCommand()) )
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
            else
            {
                setDefaultCursor();
            }
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_UNDO.getActionCommand()) ) {
            undoVOI();
            setDefaultCursor();
        }  else if (command.equals(CustomUIBuilder.PARAM_VOI_REDO.getActionCommand()) ) {
            redoVOI();
            setDefaultCursor();
        } else if (command.equals("OpacityPaint")) {
            new JDialogOpacityControls(null, this, m_fOpacity);
        } else if ( command.equals(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand() ) ) {
            saveImage(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand());
            createMask( CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand() );
            setDefaultCursor();
        } else if ( command.equals(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand() ) ) {
            saveImage(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand());
            createMask( CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand() );
            setDefaultCursor();
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_3D_INTERSECTION.getActionCommand()) ) {
            m_kParent.create3DVOI(true);
            setDefaultCursor();
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_3D_UNION.getActionCommand()) ) {
            m_kParent.create3DVOI(false);
            setDefaultCursor();
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_PROPERTIES.getActionCommand())) {
            showVOIProperties();
            setDefaultCursor();
        } else {
            doVOI(command);
        }

    }


    public void addVOI( VOIBase kNew, boolean bUpdate )
    {
        ModelImage kActive = m_kParent.getActiveImage();
        if ( kActive != null )
        {
            addVOI( kActive, kNew, bUpdate );
            if ( kActive.isRegistered( m_kCurrentVOIGroup ) == -1 )
            {
                kActive.registerVOI( m_kCurrentVOIGroup );
            }            
        }
    }

    /**
     * adds the update listener.
     * 
     * @param listener
     *            DOCUMENT ME!
     */
    public void addVOIUpdateListener(UpdateVOISelectionListener listener) {
        listenerList.add(UpdateVOISelectionListener.class, listener);
    }

    public void calcPLineSliceDistances() {    
    }


    public void changeVOIOrder(boolean doContour, int direction) {
        if ((direction != VOI.FORWARD) && (direction != VOI.BACKWARD)
                && (direction != VOI.FRONT) && (direction != VOI.BACK))
        {
            return;
        }

        ViewVOIVector VOIs = m_kParent.getActiveImage().getVOIs();
        for (int i = 0; i < VOIs.size(); i++) {

            if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                if (!doContour) {
                    if ( (i == VOIs.size()-1) && (direction == VOI.FORWARD  || direction == VOI.FRONT) )
                    {
                        return;
                    }
                    if ( i == 0 && (direction == VOI.BACKWARD || direction == VOI.BACK ) )
                    {
                        return;
                    }
                    VOI kVOI = VOIs.remove(i);
                    if ( direction == VOI.FORWARD )
                    {
                        VOIs.add( i+1, kVOI );
                        //compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                        return;
                    }
                    if ( direction == VOI.BACKWARD )
                    {
                        VOIs.add( i-1, kVOI );
                        //compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                        return;                        
                    }
                    if ( direction == VOI.FRONT )
                    {
                        VOIs.add( kVOI );
                        //compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                        return;
                    }
                    VOIs.add( 0, kVOI );
                    //compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                    return;                      
                }

                Vector<VOIBase> curves = VOIs.VOIAt(i).getCurves();

                for (int j = 0; j < curves.size(); j++) {
                    if (curves.get(j).isActive()) {

                        if ( (j == curves.size()-1) && (direction == VOI.FORWARD  || direction == VOI.FRONT) )
                        {
                            return;
                        }
                        if ( j == 0 && (direction == VOI.BACKWARD || direction == VOI.BACK ) )
                        {
                            return;
                        }
                        VOIBase kVOI = curves.remove(j);
                        if ( direction == VOI.FORWARD )
                        {
                            curves.add( j+1, kVOI );
                            //compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                            return;
                        }
                        if ( direction == VOI.BACKWARD )
                        {
                            curves.add( j-1, kVOI );
                            //compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                            return;                        
                        }
                        if ( direction == VOI.FRONT )
                        {
                            curves.add( kVOI );
                            //compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                            return;
                        }
                        curves.add( 0, kVOI );
                        //compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                        return;                      
                    }
                }
            }
        }
        MipavUtil.displayWarning("Please select a VOI!");
    }

    public boolean checkForVOICompatibility(VOIVector VOIs, int type,
            ViewControlsImage controls) {
        return false;
    }

    public boolean convertPointToPoly() {
        return false;
    }

    public void copyVOIforUndo() {
    }

    public boolean copyVOItoClipBrd() {
        copy();
        return (m_kCopyList.size() > 0);
    }

    public void createMask( String command )
    {
        if (getActiveImage().getVOIs().size() < 1) {
            MipavUtil.displayWarning("Must have at least one VOI to perform quick mask");
            return;
        }
        //System.err.println( "start" );
        //long time = System.currentTimeMillis();
        if ( m_bGPURenderer )
        {
            createMask( getActiveImage(), command.equals(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand()) );
        }
        else
        {
            new JDialogMask(getActiveImage(), false, command.equals(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand()));
        }
        //time = System.currentTimeMillis() - time;
        //System.err.println( "done " + time );
    }
    
    public void createMask( ModelImage kActive, boolean bInside )
    {
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
    }


    /**
     * Deletes selected VOIs or VOI contours (boolean).
     * 
     * @param contoursOnly
     *            boolean (true = only delete selected contours, false = delete
     *            entire selected VOI)
     */
    public void deleteSelectedVOI(boolean contoursOnly)
    {
        ModelImage kActive = m_kParent.getActiveImage();
        ViewVOIVector VOIs = kActive.getVOIs();
        for ( int i = VOIs.size() -1; i >= 0 ; i-- )
        {
            if ( VOIs.get(i).isActive() && !contoursOnly )
            {
                if ( m_kImageA != null ) { m_kImageA.unregisterVOI(VOIs.get(i)); }
                if ( m_kImageB != null ) { m_kImageB.unregisterVOI(VOIs.get(i)); }
            }
            else if ( contoursOnly )
            {
                for ( int j = VOIs.get(i).getCurves().size()-1; j >= 0; j-- )
                {
                    if ( VOIs.get(i).getCurves().get(j).isActive() )
                    {
                        VOIs.get(i).getCurves().remove(j);
                    }
                }
            }
        }
        updateDisplay();
    }


    public void deleteVOI(VOIBase kOld) {
        m_kCurrentVOIGroup = null;
        VOI kGroup = kOld.getGroup();
        if ( kGroup != null )
        {
            kGroup.getCurves().remove(kOld);
            if ( kGroup.isEmpty() )
            {
                if ( m_kImageA != null ) { m_kImageA.unregisterVOI(kGroup); }
                if ( m_kImageB != null ) { m_kImageB.unregisterVOI(kGroup); }
            }
        }
        updateDisplay();
    }


    public void deleteVOIActivePt()
    {
        Vector<VOIBase> activeList = new Vector<VOIBase>();

        VOIVector kVOIs = m_kParent.getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() && !activeList.contains(kCurrentVOI) )
                {
                    activeList.add(kCurrentVOI);
                }
            }
        }

        Vector<VOIBase> deleteList = new Vector<VOIBase>();
        for ( int i = 0; i < activeList.size(); i++ )
        {
            VOIBase kCurrentVOI = activeList.get(i);
            VOIManager kManager = m_kVOIManagers[0];
            for ( int j = 0; j < m_kVOIManagers.length; j++ )
            {
                if ( kCurrentVOI.getPlane() == m_kVOIManagers[j].getPlane() )
                {
                    kManager = m_kVOIManagers[j];
                    break;
                }
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



    public void deleteVOIs()
    {
        saveVOIs("deleteVOIs");
        deleteAllVOI();
    }


    public void disposeLocal(boolean flag)
    {
        if (popup != null) {
            popup = null;
        }

        if (popupPt != null) {
            popupPt = null;
        }

        deleteAllVOI();
        m_kParent = null;
        m_kImageA = null;
        m_kImageB = null;

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

        listenerList = null;
    }

    public void doVOI( String kCommand )
    {        
        boolean bDraw = isDrawCommand(kCommand);
        m_kParent.enableBoth(!bDraw);
        
        if ( kCommand.equals("quickLUT") )
        {
            saveGroup = m_kCurrentVOIGroup;
            m_kCurrentVOIGroup = null;
            currentColor = toolbarBuilder.getVOIColorButton().getBackground();
            toolbarBuilder.getVOIColorButton().setBackground( Color.yellow );
        }

        if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_PROPAGATE_UP.getActionCommand()) )
        {
            saveVOIs(kCommand);
            copy();
            Vector3f kCenter = m_kParent.PropUp(m_iActive);
            setCenter( kCenter );
            paste();
            setDefaultCursor();
        }
        else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_PROPAGATE_DOWN.getActionCommand()) )
        {
            saveVOIs(kCommand);
            copy();
            Vector3f kCenter = m_kParent.PropDown(m_iActive);
            setCenter( kCenter );
            paste();
            setDefaultCursor();
        }
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL.getActionCommand()) ) {
            saveVOIs(kCommand);
            cut();
            pasteAll();
            setDefaultCursor();
        }
        else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_POINT_DELETE.getActionCommand()) ) 
        {
            saveVOIs(kCommand);
            deleteActiveVOI();
        }
        else if ( kCommand.equals("deleteVOIActivePt") ) 
        {
            saveVOIs(kCommand);
            deleteVOIActivePt();
        }
        else if (kCommand.equals("selectAllVOIs") )
        {
            selectAllVOIs(true);
        }
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_CUT.getActionCommand()) ) {
            saveVOIs(kCommand);
            cut();
            setDefaultCursor();
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_COPY.getActionCommand()) ) {
            copy();
            setDefaultCursor();
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_PASTE.getActionCommand()) ) {
            saveVOIs(kCommand);
            paste();
            setDefaultCursor();
        }
        else if ( kCommand.equals("MoveUP") )
        {
            saveVOIs(kCommand);
            moveVOI( m_kVOIManagers[m_iActive], new Vector3f( 0, 1, 0 ), -1, true );
        }
        else if ( kCommand.equals("MoveDown") )
        {
            saveVOIs(kCommand);
            moveVOI( m_kVOIManagers[m_iActive], new Vector3f( 0,-1, 0 ), -1, true  );
        }
        else if ( kCommand.equals("MoveLeft") )
        {
            saveVOIs(kCommand);
            moveVOI( m_kVOIManagers[m_iActive], new Vector3f(-1, 0, 0 ), -1, true  );
        }
        else if ( kCommand.equals("MoveRight") )
        {
            saveVOIs(kCommand);
            moveVOI( m_kVOIManagers[m_iActive], new Vector3f( 1, 0, 0 ), -1, true  );
        }        
        else
        {
            boolean iActive = false;
            for (int i = 0; i < m_kVOIManagers.length; i++) {
                m_kVOIManagers[i].doVOI( kCommand, bDraw );
                iActive |= m_kVOIManagers[i].isActive();
            }
            m_kParent.PointerActive(iActive);
        }
    }




    /**
     * Fires a VOI selection change event based on the VOI.
     * 
     * @param voi
     */
    public void fireVOISelectionChange(VOI voi) {
        fireVOISelectionChange(voi, null);
    }

    /**
     * Fires a VOI selection change event based on the VOI and curve.
     * 
     * @param voi
     *            DOCUMENT ME!
     * @param curve
     *            DOCUMENT ME!
     */
    public void fireVOISelectionChange(VOI voi, VOIBase curve) {
        try {

            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(UpdateVOISelectionListener.class) == 0) {
                return;
            }
        } catch (NullPointerException npe) {
            return;
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        UpdateVOIEvent voiUpdate = new UpdateVOIEvent(this, voi, curve);

        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();

        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == UpdateVOISelectionListener.class) {
                ((UpdateVOISelectionListener) listeners[i + 1])
                .selectionChanged(voiUpdate);
            }
        }
        updateDisplay();
    }



    public ModelImage getActiveImage() {
        return m_kParent.getActiveImage();
    }

    public int getActiveVOICount()
    {
        ModelImage kActive = m_kParent.getActiveImage();
        ViewVOIVector VOIs = kActive.getVOIs();
        int nActive = 0;
        for (int i = 0; i < VOIs.size(); i++) {
            if (VOIs.VOIAt(i).isActive()) {
                nActive++;
            }
        }
        return nActive;
    }

    public Point getAnchorPt() {
        return null;
    }

    public Component getComponentImage() {
        return m_kVOIManagers[m_iActive].getComponent();
    }

    public JFrame getFrame() {
        return m_kParent.getFrame();
    }

    public float[] getImageGraphBuffer() { 
        return null;
    }

    
    public JToggleButton getPointerButton( )
    {
        return m_kPointerButton;
    }


    public ViewJPopupPt getPopupPt() {
        return this.popupPt;
    }

    public ViewJPopupVOI getPopupVOI() {
        return this.popup;
    }


    public int getSlice() {
        return (int)m_kParent.getCenterPt().Z;
    }

    public JToolBar getToolBar()
    {
        return m_kVOIToolbar;
    }


    public int getVOI_ID() {
        return 0;
    }



    public VOIManager getVOIManager(int i)
    {
        if ( i < m_kVOIManagers.length )
        {
            return m_kVOIManagers[i];
        }
        return null;
    }

    public VOISaveState getVOIState( )
    {
        VOISaveState kVOIState = new VOISaveState();
        kVOIState.voiVectorA = m_kImageA.getVOIsCopy();
        if ( m_kImageB != null )
        {
            kVOIState.voiVectorB = m_kImageB.getVOIsCopy();
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

    /**
     * Generates and displays a 1D graph of the average or total intensity of
     * 2.5 VOI of 2.5D image (3D).
     * 
     * @param totalIntensity
     *            if true calculates total sum of the intensity else calculates
     *            the average pixel intensity
     * @param useThreshold
     *            whether or not to threshold this intensity plot
     * @param threshold
     *            the threshold value to use, if thresholding.
     */
    public void graph25VOI_CalcInten(boolean totalIntensity,
            boolean useThreshold, float threshold) {

        int i, j, s;
        int nVOI;
        ViewVOIVector VOIs;
        VOI v;
        float intensitySum;
        float[] position;
        float[] intensity;
        float[][] rgbPositions;
        float[][] rgbIntensities;
        int numPixels;
        ViewUserInterface.getReference();


        ModelImage kImage = m_kParent.getActiveImage();

        if (kImage.getNDims() == 3) {

            if (kImage.isColorImage() == true) {

                try {
                    rgbPositions = new float[3][kImage
                                                .getExtents()[2]];
                    rgbIntensities = new float[3][kImage
                                                  .getExtents()[2]];

                    VOIs = kImage.getVOIs();
                    nVOI = VOIs.size();

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).isActive()
                                && (VOIs.VOIAt(i).isVisible() == true)) {
                            v = VOIs.VOIAt(i);

                            Vector<VOIBase>[] curves = v.getSortedCurves( VOIBase.ZPLANE, kImage.getExtents()[2]);
                            for ( s = 0; s < kImage.getExtents()[2]; s++ )
                            {
                                for (int c = 0; c < 3; c++) {
                                    numPixels = 0;
                                    intensitySum = 0;
                                    if ( curves[s] != null )
                                    {                                            
                                        for (j = 0; j < curves[s].size(); j++)
                                        {                                       
                                            if (useThreshold) {
                                                intensitySum += curves[s].elementAt(j).calcRGBIntensityThreshold(
                                                                kImage,
                                                                c,
                                                                threshold);
                                            } else {
                                                intensitySum += curves[s].elementAt(j).calcRGBIntensity(kImage, c);
                                            }

                                            numPixels += curves[s].elementAt(j).getLastNumPixels();
                                        }
                                    }
                                    rgbPositions[c][s] = s;

                                    if (totalIntensity
                                            || (numPixels == 0)) {
                                        rgbIntensities[c][s] = intensitySum;
                                    } else {
                                        rgbIntensities[c][s] = intensitySum
                                        / numPixels;
                                    }
                                }
                            }

                            ViewJFrameGraph contourGraph = new ViewJFrameGraph(
                                    rgbPositions,
                                    rgbIntensities,
                                    "Intensity Graph",
                                    v,
                                    FileInfoBase
                                    .getUnitsOfMeasureAbbrevStr(kImage
                                            .getFileInfo(0)
                                            .getUnitsOfMeasure(2)));

                            contourGraph
                            .setDefaultDirectory(ViewUserInterface
                                    .getReference()
                                    .getDefaultDirectory());
                            v.setContourGraph(contourGraph);
                            contourGraph.setVisible(true);
                            return;
                        }
                    }

                    if (i == nVOI) {
                        MipavUtil.displayError("Please select a contour VOI!");
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil
                    .displayError("Out of memory: ComponentEditImage.graphVOI");

                    return;
                }
            } else {

                try {
                    VOIs = kImage.getVOIs();
                    nVOI = VOIs.size();

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible() &&
                                (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ) {

                            position = new float[kImage.getExtents()[2]];
                            intensity = new float[kImage.getExtents()[2]];
                            v = VOIs.VOIAt(i);

                            Vector<VOIBase>[] curves = v.getSortedCurves( VOIBase.ZPLANE, kImage.getExtents()[2]);
                            for ( s = 0; s < kImage.getExtents()[2]; s++ )
                            {
                                numPixels = 0;
                                intensitySum = 0;
                                if ( curves[s] != null )
                                {
                                    for (j = 0; j < curves[s].size(); j++)
                                    {
                                        if (useThreshold) {
                                            intensitySum += curves[s].elementAt(j).calcIntensityThreshold( kImage, threshold );

                                        } else {
                                            intensitySum += curves[s].elementAt(j).calcIntensity( kImage );
                                        }

                                        numPixels += curves[s].elementAt(j).getLastNumPixels();                                        
                                    }
                                }

                                position[s] = s;

                                if (totalIntensity || (numPixels == 0)) {
                                    intensity[s] = intensitySum;
                                } else {
                                    intensity[s] = intensitySum
                                    / numPixels;
                                }
                            }

                            ViewJFrameGraph contourGraph = new ViewJFrameGraph(
                                    position,
                                    intensity,
                                    "Intensity Graph",
                                    v,
                                    FileInfoBase
                                    .getUnitsOfMeasureAbbrevStr(kImage
                                            .getFileInfo(0)
                                            .getUnitsOfMeasure(0)),null);

                            contourGraph
                            .setDefaultDirectory(ViewUserInterface
                                    .getReference()
                                    .getDefaultDirectory());
                            v.setContourGraph(contourGraph);
                            contourGraph.setVisible(true);
                            //v.setTotalIntensity(totalIntensity);
                            //v.setPosition(position);
                            //v.setIntensity(intensity);

                            return;
                        }
                    }
                    if (i == nVOI) {
                        MipavUtil.displayError("Please select a contour VOI!");
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil
                    .displayError("Out of memory: ComponentEditImage.graphVOI");

                    return;
                }
            }
        }
    }

    public void graphPointVOI(VOI v, VOIPoint voiPt, int j,
            boolean useFrameRefTime) {

        /** Buffer for holding intensities at specific points. */
        float[] ptIntensity;

        /** buffer used when graphing a VOIPoint on a grayscale image. */
        float[] ptPosition;

        /** Buffer for holding RGB intensities [0,1,2] at specific points. */
        float[][] ptRGBIntensities = null;

        /** Buffer used for graphing VOIPoints for an RGB image. */
        float[][] ptRGBPositions = null;


        int t, s;
        Vector3f pt;
        FileInfoDicom fileInfo;
        String frameRefTimeString;
        int frameReferenceTime;

        ModelImage kImage = m_kParent.getActiveImage();

        if ((kImage.getNDims() != 3)
                && (kImage.getNDims() != 4)) {
            return;
        }

        if (kImage.getNDims() == 3) {

            if (kImage.isColorImage() == true) {
                ptRGBPositions = new float[3][kImage
                                              .getExtents()[2]];
                ptRGBIntensities = new float[3][kImage
                                                .getExtents()[2]];
                pt = voiPt.exportPoint();

                for (s = 0; s < kImage.getExtents()[2]; s++) {

                    for (int c = 0; c < 3; c++) {
                        ptRGBPositions[c][s] = s;
                        ptRGBIntensities[c][s] = kImage
                        .getFloat(
                                ((4 * ((s * kImage
                                        .getSliceSize())
                                        + ((int) pt.Y * kImage
                                                .getExtents()[0]) + (int) pt.X))
                                                + c + 1));
                    }
                }

                if (v.getContourGraph() == null) {
                    ViewJFrameGraph contourGraph = new ViewJFrameGraph(
                            ptRGBPositions, ptRGBIntensities,
                            "Intensity Graph", v, FileInfoBase
                            .getUnitsOfMeasureAbbrevStr(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(0)));

                    contourGraph.setDefaultDirectory(ViewUserInterface
                            .getReference().getDefaultDirectory());
                    contourGraph.setVisible(false);
                    v.setContourGraph(contourGraph);
                } else {
                    v.getContourGraph().setUnitsInLabel(
                            FileInfoBase.getUnitsOfMeasureAbbrevStr(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(0)));
                    v.getContourGraph().saveNewFunction(ptRGBPositions,
                            ptRGBIntensities, j);
                }

                return;
            }
            try {
                ptPosition = new float[kImage.getExtents()[2]];
                ptIntensity = new float[kImage.getExtents()[2]];

                for (s = 0; s < kImage.getExtents()[2]; s++) {

                    pt = voiPt.exportPoint();
                    ptPosition[s] = s;
                    ptIntensity[s] = kImage.getFloat(
                            (int) ((s * kImage
                                    .getSliceSize())
                                    + (pt.Y * kImage
                                            .getExtents()[0]) + pt.X));
                }

                if (v.getContourGraph() == null) {
                    ViewJFrameGraph contourGraph = new ViewJFrameGraph(
                            ptPosition, ptIntensity, "Intensity Graph", v,
                            FileInfoBase
                            .getUnitsOfMeasureAbbrevStr(kImage
                                    .getFileInfo(0)
                                    .getUnitsOfMeasure(0)),null);

                    contourGraph.setDefaultDirectory(ViewUserInterface
                            .getReference().getDefaultDirectory());
                    contourGraph.setVisible(false);
                    v.setContourGraph(contourGraph);
                } else if (useFrameRefTime) {
                    v.getContourGraph().setUnitsInLabel(
                            FileInfoBase
                            .getUnitsOfMeasureAbbrevStr(kImage
                                    .getFileInfo(0)
                                    .getUnitsOfMeasure(0)));
                    v.getContourGraph().update(ptPosition,
                            ptIntensity, j);
                }  else {
                    v.getContourGraph().setUnitsInLabel(
                            FileInfoBase
                            .getUnitsOfMeasureAbbrevStr(kImage
                                    .getFileInfo(0)
                                    .getUnitsOfMeasure(0)));
                    v.getContourGraph().saveNewFunction(ptPosition,
                            ptIntensity, j);
                }

                return;
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil
                .displayError("Out of memory: ComponentEditImage.graphVOI");

                return;
            }
        } else if (kImage.getNDims() == 4) {
            int xDim = kImage.getExtents()[0];
            int yDim = kImage.getExtents()[1];
            int zDim = kImage.getExtents()[2];

            try {
                ptPosition = new float[kImage.getExtents()[3]];
                ptIntensity = new float[kImage.getExtents()[3]];

                for (t = 0; t < kImage.getExtents()[3]; t++) {

                    pt = voiPt.exportPoint();
                    if (useFrameRefTime) {
                        fileInfo = (FileInfoDicom) (kImage
                                .getFileInfo(t * zDim));
                        frameRefTimeString = ((String) fileInfo.getTagTable()
                                .getValue("0054,1300")).trim();
                        if (frameRefTimeString != null) {
                            try {
                                frameReferenceTime = new Integer(
                                        frameRefTimeString).intValue();
                            } catch (NumberFormatException e) {
                                MipavUtil
                                .displayError("Number format excepton from frame Reference Time String = "
                                        + frameRefTimeString);
                                return;
                            }

                            ptPosition[t] = frameReferenceTime;
                        } // if (frameRefTimeString != null)
                        else {
                            MipavUtil
                            .displayError("Frame reference time string is null");
                            return;
                        }
                    } // if (useFrameRefTime)
                    else {
                        ptPosition[t] = t;
                    }
                    ptIntensity[t] = kImage
                    .getFloat(
                            (int) ((t * xDim * yDim * zDim)
                                    + (pt.Z * xDim * yDim)
                                    + (pt.Y * xDim) + pt.X));
                }

                if (v.getContourGraph() == null) {
                    ViewJFrameGraph contourGraph = new ViewJFrameGraph(
                            ptPosition, ptIntensity, "Intensity Graph", v,
                            FileInfoBase.getUnitsOfMeasureAbbrevStr(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(3)),null);
                    contourGraph.setDefaultDirectory(ViewUserInterface
                            .getReference().getDefaultDirectory());
                    contourGraph.setVisible(false);
                    v.setContourGraph(contourGraph);
                } else if (useFrameRefTime) {
                    v.getContourGraph().setUnitsInLabel(
                            FileInfoBase.getUnitsOfMeasureAbbrevStr(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(3)));
                    v.getContourGraph().replaceFunction(ptPosition,
                            ptIntensity, null, v, j);
                } else {
                    v.getContourGraph().setUnitsInLabel(
                            FileInfoBase.getUnitsOfMeasureAbbrevStr(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(3)));
                    v.getContourGraph().saveNewFunction(ptPosition,
                            ptIntensity, j);
                }

                return;

            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil
                .displayError("Out of memory: ComponentEditImage.graphVOI");

                return;
            }
        }

    }




    public void graphVOI()
    {
        VOIVector kVOIs = m_kParent.getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() )
                {
                    showIntensityGraph( kCurrentVOI );
                }
            }
        }
    }

    public boolean isLivewireNull() {   
        return false;
    }

    public boolean isNewVoiNeeded(int voiType) {
        return false;
    }

    public boolean make3DVOI( boolean bIntersection, ModelImage kVolume  )
    {
        boolean bCreated = true;
        for (int i = 0; i < m_kVOIManagers.length; i++) {
            bCreated &= make3DVOI(bIntersection, m_kParent.getActiveImage(), kVolume, null, i);
        }
        return bCreated;
    }

    public void mouseClicked(MouseEvent event) {

        if (event.getButton() == MouseEvent.BUTTON3) {

            if (event.getSource() instanceof AbstractButton) {
                AbstractButton btnSource = (AbstractButton) event.getSource();
                if ( btnSource.getActionCommand().equals(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand())
                        || btnSource.getActionCommand().equals(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand())) {
                    ViewJFrameBase.handleMaskPopupMenu((Component) event.getSource(), event);
                } 
            }
        }
    }


    public void mouseDragged(MouseEvent arg0) {      
    }

    public void mouseEntered(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    public void mouseExited(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    public void mouseMoved(MouseEvent arg0) {
    }

    public void mousePressed(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    public void mouseReleased(MouseEvent e) {
        // TODO Auto-generated method stub

    }



    public void moveVOI( VOIManager kActive, Vector3f kDiff, int iPlane, boolean bFirstMove )
    {
        if ( bFirstMove )
        {
            boolean bFirst = true;
            m_kActiveList = new Vector<VOIBase>();
            VOIVector kVOIs = m_kParent.getActiveImage().getVOIs();
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
                {
                    VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                    if ( kCurrentVOI.isActive() && !m_kActiveList.contains(kCurrentVOI) )
                    {
                        m_kActiveList.add( kCurrentVOI );
                        Vector3f[] kBounds = kCurrentVOI.getImageBoundingBox();
                        if ( bFirst )
                        {
                            bFirst = false;
                            m_akBounds[0].Copy(kBounds[0]);
                            m_akBounds[1].Copy(kBounds[1]);
                        }
                        m_akBounds[0].Min(kBounds[0]);
                        m_akBounds[1].Max(kBounds[1]);
                    }
                }
            }
        }

        if ( kActive.testMove( kDiff, m_akBounds ) )
        {
            for ( int i = 0; i < m_kActiveList.size(); i++ )
            {
                VOIBase kCurrentVOI = m_kActiveList.get(i);
                if ( iPlane == (iPlane & kCurrentVOI.getPlane()) || (iPlane == -1) )
                {
                    kActive.move( kCurrentVOI, kDiff );
                }
            }
        }

        updateDisplay();
    }

    public void newVOI( boolean bPropagate, boolean bSplit )
    {
        if ( !bSplit )
        {
            selectAllVOIs(false);
        }
        if ( bPropagate )
        {
            doVOI(CustomUIBuilder.PARAM_VOI_NEW.getActionCommand());
        }
        //voiUID = m_kVOIManagers[m_iActive].getVOICount() + 1;
        m_kCurrentVOIGroup = null;
        voiUID++;
        toolbarBuilder.getVOIColorButton().setVOIColor(voiUID);
        setButtonColor(toolbarBuilder.getVOIColorButton(), 
                toolbarBuilder.getVOIColorButton().getBackground() );
    }

    public void pasteVOI() {
        paste();         
    }

    public void pasteVOI(VOIBase kNew)
    {
        kNew.getGroup().getCurves().add(kNew);
        if (m_kParent.getActiveImage().isRegistered(kNew.getGroup()) == -1 )
        {
            m_kParent.getActiveImage().registerVOI(kNew.getGroup());
        }
        updateDisplay();
    }

    public boolean propVOI(int direction, boolean active)
    {
        if ( direction > 0 )
        {
            copy();
            if ( m_kCopyList.size() == 0 )
            {
                return false;
            }
            saveVOIs(CustomUIBuilder.PARAM_VOI_PROPAGATE_UP.getActionCommand());
            Vector3f kCenter = m_kParent.PropUp(m_iActive);
            setCenter( kCenter );
            paste();
        }
        else
        {
            copy();
            if ( m_kCopyList.size() == 0 )
            {
                return false;
            }
            saveVOIs(CustomUIBuilder.PARAM_VOI_PROPAGATE_DOWN.getActionCommand());
            Vector3f kCenter = m_kParent.PropDown(m_iActive);
            setCenter( kCenter );
            paste();
        }
        updateDisplay();
        return true;
    }

    public boolean propVOIAll() {
        copy();
        if ( m_kCopyList.size() == 0 )
        {
            return false;
        }
        saveVOIs(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL.getActionCommand());
        deleteActiveVOI();
        pasteAll();
        return true;        
    }

    public void quickLUT( VOIBase kLUT )
    {
        Vector3f[] kBounds = kLUT.getImageBoundingBox();
        m_akBounds[0].Copy(kBounds[0]);
        m_akBounds[1].Copy(kBounds[1]);        
        deleteVOI( kLUT );
        if (m_kParent.getActiveImage().isColorImage() == false) {

            if (m_kImageA == m_kParent.getActiveImage()) {
                quickLUT( m_akBounds, m_kImageA, (ModelLUT)m_kLUTa );
                m_kImageA.notifyImageDisplayListeners((ModelLUT)m_kLUTa, true);
            } else if ( (m_kImageB != null) && (m_kImageB == m_kParent.getActiveImage())) {
                quickLUT( m_akBounds, m_kImageB, (ModelLUT)m_kLUTb );
                m_kImageB.notifyImageDisplayListeners((ModelLUT)m_kLUTb, true);
            }
        } else { // RGB image
        }

        if ( (getActiveImage().isColorImage()) && (getActiveImage().getHistoRGBFrame() != null)) {
            getActiveImage().getHistoRGBFrame().update();
        } else if (getActiveImage().getHistoLUTFrame() != null) {
            getActiveImage().getHistoLUTFrame().update();
        }    

        toolbarBuilder.getVOIColorButton().setBackground( currentColor );
        m_kCurrentVOIGroup = saveGroup;
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

    public void redoVOI()
    {
        if ( m_kRedoCommands.isEmpty() )
        {
            return;
        }        
        String lastCommand = m_kRedoCommands.remove( m_kRedoCommands.size() -1 );
        //System.err.println(lastCommand);
        m_kUndoCommands.add(lastCommand);
        if ( lastCommand.equals( CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand() ) ||
                lastCommand.equals( CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand() ) )
        {
            if ( m_bGPURenderer )
            {                
                m_kParent.getActiveImage().useMask(true); 
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

    /**
     * removes the update listener.
     * 
     * @param listener
     *            DOCUMENT ME!
     */
    public void removeVOIUpdateListener(UpdateVOISelectionListener listener) {
        listenerList.remove(UpdateVOISelectionListener.class, listener);
    }

    public void resetLivewire() 
    {
        for (int i = 0; i < m_kVOIManagers.length; i++) {
            m_kVOIManagers[i].doVOI( "ResetVOI", false );
        }
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


    public void saveVOIs( String kCommand )
    {
        m_kUndoCommands.add( kCommand );
        m_kUndoList.add( getVOIState() );
        m_kRedoCommands.clear();
        m_kRedoList.clear();
    }

    public void selectAllContours() {
        VOIVector kVOIs = m_kParent.getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            if ( kCurrentGroup.isActive() )
            {
                kCurrentGroup.setAllActive(true);
            }
        }
        updateDisplay();            
    }


    public void selectAllVOIs(boolean bActive)
    {
        VOIVector kVOIs = m_kParent.getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            kCurrentGroup.setAllActive(bActive);
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
                if ( m_kVOIManagers[i].getActiveImage() != m_kParent.getActiveImage() )
                {
                    m_kParent.setActiveImage( m_kVOIManagers[i].getActiveImage() );
                }
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
        if ( m_kCurrentVOIGroup != null )
        {
            ColorRGBA kColor = new ColorRGBA( _color.getRed()/255.0f,
                    _color.getGreen()/255.0f,
                    _color.getBlue()/255.0f, 
                    m_kCurrentVOIGroup.getOpacity() );
            m_kCurrentVOIGroup.setColor( _color );
            for ( int i = 0; i < m_kCurrentVOIGroup.getCurves().size(); i++ )
            {
                VOIBase kVOI3D = (m_kCurrentVOIGroup.getCurves().get(i));
                kVOI3D.update( kColor );
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

    public void setCenter( Vector3f center, boolean bParent )
    {
        if ( bParent )
        {
            m_kParent.setCenter(center);
            return;
        }
        setCenter(center);
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
        toolbarBuilder.setPointerSelected();
        actionPerformed( new ActionEvent ( this, 0, CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER.getActionCommand()) );
        //m_kParent.setDefaultCursor();     
        //doVOI( CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER.getActionCommand() );
    }

    public void setEnabled( boolean flag )
    {
        m_bEnabled = flag;
    }

    public void setGraphVisible() { 
        int nVOI;
        ViewVOIVector VOIs;
        ViewUserInterface UI = ViewUserInterface.getReference();

        ModelImage kImage = m_kParent.getActiveImage();
        VOIs = kImage.getVOIs();
        nVOI = VOIs.size();

        boolean useFrameRefTime = false;
        FileInfoDicom fileInfo = null;
        String frameRefTimeString = null;
        int frameReferenceTime = 0;

        if ((kImage.getNDims() == 4)
                && (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
            boolean frameRefTimeFound = false;
            fileInfo = (FileInfoDicom) kImage.getFileInfo(0);
            frameRefTimeString = ((String) fileInfo.getTagTable().getValue(
            "0054,1300")).trim();
            if (frameRefTimeString != null) {
                try {
                    frameReferenceTime = new Integer(frameRefTimeString)
                    .intValue();
                    frameRefTimeFound = true;
                    Preferences.debug("Frame reference time = "
                            + frameReferenceTime + "\n");
                } catch (NumberFormatException e) {
                    Preferences
                    .debug("Number format excepton from frame Reference Time String = "
                            + frameRefTimeString + "\n");
                }

                if (frameRefTimeFound) {
                    int response = JOptionPane.showConfirmDialog(
                            UI.getMainFrame(),
                            new String(
                            "Do you wish to use the frame reference time for the graph x axis?"),
                            "Frame Reference Time?",
                            JOptionPane.YES_NO_OPTION,
                            JOptionPane.QUESTION_MESSAGE);
                    if (response == JOptionPane.YES_OPTION) {
                        useFrameRefTime = true;
                    }
                } // if (frameRefTimeFound)
            } // if (frameRefTimeString != null)
        } // if if ((compImage.getActiveImage().getNDims() == 4)

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                if ((VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                        && (VOIs.VOIAt(i).getContourGraph() != null)) {
                    if (kImage.getNDims() == 4) {
                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(
                                FileInfoBase
                                .getUnitsOfMeasureAbbrevStr(kImage
                                        .getFileInfo(0)
                                        .getUnitsOfMeasure(3)));
                    } else {
                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(
                                FileInfoBase
                                .getUnitsOfMeasureAbbrevStr(kImage
                                        .getFileInfo(0)
                                        .getUnitsOfMeasure(0)));
                    }

                    if (useFrameRefTime
                            || (kImage.isColorImage() == true)) {

                        for (int j = 0; j < VOIs.VOIAt(i).getCurves().size(); j++) {
                            if (((VOIPoint) (VOIs.VOIAt(i).getCurves().elementAt(j))).isActive()) {
                                graphPointVOI(VOIs.VOIAt(i), (VOIPoint) (VOIs
                                        .VOIAt(i).getCurves().elementAt(j)), j,
                                        useFrameRefTime);
                            }
                        }
                    }

                    VOIs.VOIAt(i).getContourGraph().setVisible(true);
                } else if ((VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                        && (VOIs.VOIAt(i).getContourGraph() == null)) {

                    for (int j = 0; j < VOIs.VOIAt(i).getCurves().size(); j++) {
                        graphPointVOI(VOIs.VOIAt(i),
                                (VOIPoint) (VOIs.VOIAt(i).getCurves().elementAt(j)), j,
                                useFrameRefTime);
                    }

                    VOIs.VOIAt(i).getContourGraph().setVisible(true);
                }
            }
        }

    }

    public void setImageGraphBuffer(float[] buf) {      
    }

    public void setMode(int mode) {
    }

    public void setModeLivewire(int selection) {      
    }

    public void setOpacity( float fVal )
    {
        m_fOpacity = fVal;
        if ( m_kCurrentVOIGroup != null )
        {
            m_kCurrentVOIGroup.setOpacity( m_fOpacity );
        }
    }

    /**
     * Sets whether or not to show the overlay.
     * 
     * @param flag
     *            boolean that tells whether or not to show the overlay
     */
    public void setOverlay(boolean flag) {
        overlayOn = flag;
    }

    public void setPAAIGraphVisible() {   
        int nVOI;
        ViewVOIVector VOIs;

        VOIs = m_kParent.getActiveImage().getVOIs();
        nVOI = VOIs.size();

        Vector3f pt;

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                // get the x,y coords for each active VOIPoint and open up a
                // JDialogPointArea
                for (int j = 0; j < VOIs.VOIAt(i).getCurves().size(); j++) {

                    if (((VOIPoint) (VOIs.VOIAt(i).getCurves().elementAt(j))).isActive()) {
                        pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves().elementAt(j))).exportPoint();
                        new JDialogPointArea(m_kParent.getFrame(),
                                m_kParent.getActiveImage(), (int) pt.X, (int) pt.Y, true);
                    }
                }
            }
        }

    }

    /**
     * Sets the hue that will be used by rubberband if >= 0.0.
     * 
     * @param presetHue
     *            the hue to be used
     */
    public void setPresetHue(float presetHue) {
        int colorIncrement = Preferences.getVOIColorIncrement();
        float hue;
        if (presetHue >= 0.0f) {
            hue = presetHue;
        } else {
            hue = (float) ((((voiUID++ + colorIncrement) * 35) % 360) / 360.0);
        }

        Color color = Color.getHSBColor(hue, 1.0f, 1.0f);
        setButtonColor(toolbarBuilder.getVOIColorButton(), color );
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
        fireVOISelectionChange(m_kCurrentVOIGroup);
    }

    public void setVOI_ID(int ID) {                
    }

    public void setVOI_IDs(int ID, int UID) { 
    }

    public void setVOIState( VOISaveState kVOIState )
    {
        m_kImageA.unregisterAllVOIs();
        m_kImageA.setVOIs( kVOIState.voiVectorA );
        if ( m_kImageB != null )
        {
            m_kImageB.unregisterAllVOIs();
            m_kImageB.setVOIs( kVOIState.voiVectorB );
        }
        if ( kVOIState.currentVOI != -1 )
        {
            m_kCurrentVOIGroup = m_kImageA.getVOIs().get(kVOIState.currentVOI);
            setSelectedVOI( m_kCurrentVOIGroup, m_kCurrentVOIGroup.isAllActive() );
        }
        else
        {
            m_kCurrentVOIGroup = null;
        }        
        m_kParent.setCenter( new Vector3f( kVOIState.currentCenter ) );
    }

    public void showColorDialog()
    {
        colorChooser = new ViewJColorChooser(null, "Pick surface color", 
                new OkColorListener(toolbarBuilder.getVOIColorButton()),
                null);
    }

    public void showIntensityGraph( VOIBase kVOI ) {
        ViewJFrameGraph lineGraph;

        ModelImage kImage = m_kParent.getActiveImage();
        int[] unitsOfMeasure = kImage.getUnitsOfMeasure();
        float[] resolutions = kImage.getResolutions(0);

        kVOI.getLengthPtToPt(resolutions);
        Vector<Vector3f> positions = new Vector<Vector3f>();
        Vector<ColorRGB> colors = new Vector<ColorRGB>();

        int pts = kVOI.findPositionAndIntensity( kImage, positions, colors );

        if ( kImage.isColorImage() )
        {
            float[][] rgbPos = new float[3][pts];
            float[][] rgbColors = new float[3][pts];

            float rgbMeanIntenR = 0;
            float rgbMeanIntenG = 0;
            float rgbMeanIntenB = 0;

            for (int m = 0; m < pts; m++) {
                rgbPos[0][m] = positions.get(m).Z;
                rgbPos[1][m] = positions.get(m).Z;
                rgbPos[2][m] = positions.get(m).Z;

                rgbColors[0][m] = colors.get(m).R;
                rgbColors[1][m] = colors.get(m).G;
                rgbColors[2][m] = colors.get(m).B;
                rgbMeanIntenR += rgbColors[0][m];
                rgbMeanIntenG += rgbColors[1][m];
                rgbMeanIntenB += rgbColors[2][m];
            }
            rgbMeanIntenR /= pts;
            rgbMeanIntenG /= pts;
            rgbMeanIntenB /= pts;


            float rgbStdDevIntenR = 0;
            float rgbStdDevIntenG = 0;
            float rgbStdDevIntenB = 0;
            for (int m = 0; m < pts; m++) {
                float diff = rgbColors[0][m] - rgbMeanIntenR;
                rgbStdDevIntenR += diff * diff;
                diff = rgbColors[1][m] - rgbMeanIntenG;
                rgbStdDevIntenG += diff * diff;
                diff = rgbColors[2][m] - rgbMeanIntenB;
                rgbStdDevIntenB += diff * diff;
            }
            rgbStdDevIntenR = (float)Math.sqrt(rgbStdDevIntenR/pts);
            rgbStdDevIntenG = (float)Math.sqrt(rgbStdDevIntenG/pts);
            rgbStdDevIntenB = (float)Math.sqrt(rgbStdDevIntenB/pts);

            if (kVOI.getGroup().getContourGraph() == null) {
                ViewJFrameGraph contourGraph = new ViewJFrameGraph(rgbPos, rgbColors, "Intensity Graph", kVOI.getGroup(),
                        FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]));

                contourGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                contourGraph.setVisible(true);
                kVOI.getGroup().setContourGraph(contourGraph);
                contourGraph.setVOI(kVOI.getGroup());
            } else {
                kVOI.getGroup().getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[1]));
                kVOI.getGroup().getContourGraph().saveNewFunction(rgbPos, rgbColors, 0);
            }

            ViewUserInterface.getReference().setDataText(
                    "Line\tmean \tstandard deviation " + "\n");
            ViewUserInterface.getReference()
            .setDataText(
                    "Red\t" + rgbMeanIntenR + "\t"
                    + rgbStdDevIntenR + "\n");
            ViewUserInterface.getReference().setDataText(
                    "Green\t" + rgbMeanIntenG + "\t" + rgbStdDevIntenG
                    + "\n");
            ViewUserInterface.getReference().setDataText(
                    "Blue\t" + rgbMeanIntenB + "\t" + rgbStdDevIntenB
                    + "\n");
        } else {
            float[] pos = new float[pts];
            float[] inten = new float[pts];
            int[][] xyCoords = new int[pts][2];

            float min = Float.MAX_VALUE;
            float max = Float.MIN_VALUE;
            float totalInten = 0;
            float rgbMeanIntenR = 0;
            for (int m = 0; m < pts; m++) {
                xyCoords[m][0] = (int)positions.get(m).X;
                xyCoords[m][1] = (int)positions.get(m).Y;
                pos[m] = positions.get(m).Z;
                inten[m] = colors.get(m).R;

                totalInten += inten[m];
                rgbMeanIntenR += inten[m];
                min = Math.min( inten[m], min );
                max = Math.max( inten[m], max );
            }
            rgbMeanIntenR /= pts;
            float rgbStdDevIntenR = 0;
            for (int m = 0; m < pts; m++) {
                float diff = inten[m] - rgbMeanIntenR;
                rgbStdDevIntenR += diff * diff;
            }
            rgbStdDevIntenR = (float)Math.sqrt(rgbStdDevIntenR/pts);

            if (kVOI.getGroup().getContourGraph() == null) {
                lineGraph = new ViewJFrameGraph(pos, inten, "Line VOI Graph", kVOI.getGroup(),
                        FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]),xyCoords);
                lineGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                lineGraph.setVisible(true);
                kVOI.getGroup().setContourGraph(lineGraph);
                lineGraph.setVOI(kVOI.getGroup());
            } else {
                kVOI.getGroup().getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[1]));
                kVOI.getGroup().getContourGraph().replaceFunction(pos, inten, xyCoords, kVOI.getGroup(), 0);
            }

            ViewUserInterface.getReference().setDataText(
                    "Line\tmin \tmax \ttotal \tmean \tstandard deviation " + "\n");
            ViewUserInterface.getReference().setDataText(
                    "\t" + min + "\t" + max + "\t" + totalInten + "\t" + rgbMeanIntenR + "\t" + rgbStdDevIntenR + "\n");
        }
    }

    public void showVOIProperties() {

        if (m_kVOIDialog == null) {
            m_kVOIDialog = new JDialogVOIStats( this, m_kParent.getActiveImage(), m_kCurrentVOIGroup );
            addVOIUpdateListener(m_kVOIDialog);
        }

        if (m_kVOIDialog != null) {
            m_kVOIDialog.setVisible(true);
            m_kVOIDialog.updateVOI(m_kCurrentVOIGroup, m_kParent.getActiveImage() );
        }
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

    public void undoVOI()
    {
        if ( m_kUndoCommands.isEmpty() )
        {
            return;
        }        
        String lastCommand = m_kUndoCommands.remove( m_kUndoCommands.size() -1 );
        m_kRedoCommands.add(lastCommand);
        //System.err.println(lastCommand);
        if ( lastCommand.equals( CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand() ) ||
                lastCommand.equals( CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand() ) )
        {
            if ( m_bGPURenderer )
            {
                m_kParent.getActiveImage().useMask(false);
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

    public void updateDisplay() {
        m_kParent.setModified(); 
    }


    public void updateGraph( VOIBase kVOI )
    {
        if ( kVOI.getType() == VOI.LINE )
        {
            showIntensityGraph(kVOI);
        }
        else if ( kVOI.getType() == VOI.POINT )
        {
            graphPointVOI( kVOI.getGroup(), (VOIPoint)kVOI, kVOI.getGroup().getCurves().indexOf(kVOI),
                    true);
        }
    }

    public void updateVOIColor(Color voiColor, int voiUID) {
        //System.err.println( "updateVOIColor");    
    }

    private void addVOI( ModelImage kImage, VOIBase kNew, boolean bUpdate )
    {       
        if ( kNew.getGroup() == null )
        {
            saveVOIs("addVOI");
            if ( (m_kCurrentVOIGroup != null) && (kImage.isRegistered( m_kCurrentVOIGroup ) == -1))
            {
                findCompatibleType(kImage, kNew);
            }
            if ( (m_kCurrentVOIGroup != null) && (m_kCurrentVOIGroup.getCurveType() != kNew.getType()) )
            {
                m_kCurrentVOIGroup = null;
                newVOI(false, kNew.isSplit());
            }
            if ( m_kCurrentVOIGroup == null )
            {
                short sID = (short)(kImage.getVOIs().size() + 1);
                String kName = kNew.getClass().getName();
                int index = kName.lastIndexOf('.') + 1;
                kName = kName.substring(index);
                m_kCurrentVOIGroup = new VOI( sID,  kName + "_" + sID, 1, kNew.getType(), -1f );
                m_kCurrentVOIGroup.setOpacity(1f);
                kImage.registerVOI( m_kCurrentVOIGroup );
            }    
            kNew.setGroup( m_kCurrentVOIGroup );
            if ( kNew instanceof VOIPoint &&  kNew.getType() == VOI.POLYLINE_SLICE )
            {
                VOIPoint kNewPoint = (VOIPoint)kNew;
                boolean bAdded = false;
                for ( int i = 0; i < m_kCurrentVOIGroup.getCurves().size(); i++ )
                {
                    if ( m_kCurrentVOIGroup.getCurves().get(i).isActive() )
                    {
                        ((VOIPolyLineSlice)m_kCurrentVOIGroup.getCurves().get(i)).add(kNewPoint);
                        bAdded = true;
                        break;
                    }                    
                }
                if ( !bAdded && m_kCurrentVOIGroup.getCurves().size() > 0 )
                {
                    ((VOIPolyLineSlice)m_kCurrentVOIGroup.getCurves().lastElement()).add(kNewPoint);
                    bAdded = true;
                }
                if ( !bAdded )
                {
                    m_kCurrentVOIGroup.getCurves().add( new VOIPolyLineSlice(kNewPoint) );             
                }
            }
            else
            {
                m_kCurrentVOIGroup.getCurves().add(kNew);
            }
        }
        else
        {
            m_kCurrentVOIGroup = kNew.getGroup();
        }
        setCurrentColor();
        if ( bUpdate )
        {
            updateDisplay();
        }
    }

    private void copy()
    {
        m_kCopyList.clear();
        VOIVector kVOIs = m_kParent.getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() && !m_kCopyList.contains(kCurrentVOI) )
                {
                    m_kCopyList.add(kCurrentVOI);
                }
            }
        }
    }

    private void cut()
    {
        copy();
        deleteActiveVOI();
    }

    private void deleteActiveVOI()
    {
        Vector<VOIBase> deleteList = new Vector<VOIBase>();

        VOIVector kVOIs = m_kParent.getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() && !deleteList.contains(kCurrentVOI) )
                {
                    deleteList.add(kCurrentVOI);
                }
            }
        }
        while ( deleteList.size() > 0 )
        {
            deleteVOI( deleteList.remove(0) );
        }
    }

    private void deleteAllVOI()
    {
        if ( m_kImageA != null ) { m_kImageA.unregisterAllVOIs(); }
        if ( m_kImageB != null ) { m_kImageB.unregisterAllVOIs(); }
    }

    private void findCompatibleType( ModelImage kImage, VOIBase kNew)
    {
        VOIVector kVOIs = kImage.getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            if ( kVOIs.get(i).isActive() && kVOIs.get(i).getCurveType() == kNew.getType() )
            {
                m_kCurrentVOIGroup = kVOIs.get(i);
                return;
            }
        }
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            if ( kVOIs.get(i).getCurveType() == kNew.getType() )
            {
                m_kCurrentVOIGroup = kVOIs.get(i);
                return;
            }
        }
        m_kCurrentVOIGroup = null;
    }
    
    private boolean isDrawCommand( String kCommand )
    {
        if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_PROTRACTOR.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_VOI_LIVEWIRE.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_VOI_SPLITTER.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_VOI_LINE.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_VOI_POLY_SLICE.getActionCommand()) || 
                kCommand.equals(CustomUIBuilder.PARAM_VOI_POINT.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_VOI_TEXT.getActionCommand()) || 
                kCommand.equals(CustomUIBuilder.PARAM_VOI_3D_RECTANGLE.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_VOI_RECTANGLE.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_VOI_ELLIPSE.getActionCommand()) || 
                kCommand.equals(CustomUIBuilder.PARAM_VOI_POLYGON.getActionCommand()) || 
                kCommand.equals(CustomUIBuilder.PARAM_VOI_LEVELSET.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_LUT_QUICK.getActionCommand() ) 
                || kCommand.equals("quickLUT") ) {
            return true;
        } 
        return false;
    }

    private boolean make3DVOI( boolean bIntersection, ModelImage kSrc, ModelImage kVolume, BitSet kMask, int iValue )
    {
        VOIVector kVOIs = kSrc.getVOIs();

        boolean bCreated = false;
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() )
                {
                    bCreated = true;
                    kCurrentVOI.fillVolume( kVolume, kMask, bIntersection, iValue );     
                }
            }
        }
        updateDisplay();        
        return bCreated;
    }

    private void paste()
    {
        for ( int i = 0; i < m_kCopyList.size(); i++ )
        {
            VOIBase kCurrentVOI = m_kCopyList.get(i); 
            VOIManager kManager = m_kVOIManagers[0];
            for ( int j = 0; j < m_kVOIManagers.length; j++ )
            {
                if ( kCurrentVOI.getPlane() == m_kVOIManagers[j].getPlane() )
                {
                    kManager = m_kVOIManagers[j];
                    break;
                }
            }

            kManager.pasteVOI( kCurrentVOI );  
        }
    }

    private void pasteAll()
    {
        for ( int i = 0; i < m_kCopyList.size(); i++ )
        {
            VOIBase kCurrentVOI = m_kCopyList.get(i); 
            VOIManager kManager = m_kVOIManagers[0];
            for ( int j = 0; j < m_kVOIManagers.length; j++ )
            {
                if ( kCurrentVOI.getPlane() == m_kVOIManagers[j].getPlane() )
                {
                    kManager = m_kVOIManagers[j];
                    break;
                }
            }
            kManager.pasteAllVOI( kCurrentVOI );  
        }
    }

    private void quickLUT(Vector3f[] akMinMax, ModelImage image, ModelLUT LUT) {
        if ( !(((akMinMax[1].Z - akMinMax[0].Z) > 5) ||
                ((akMinMax[1].Y - akMinMax[0].Y) > 5) ||
                ((akMinMax[1].X - akMinMax[0].X) > 5) )  )
        {
            return;
        }        
        float min = Float.MAX_VALUE;
        float max = -100000000;
        Number val;
        for ( int z = (int)akMinMax[0].Z; z <= akMinMax[1].Z; z++ )
        {
            for ( int y = (int)akMinMax[0].Y; y <= akMinMax[1].Y; y++ )
            {
                for ( int x = (int)akMinMax[0].X; x <= akMinMax[1].X; x++ )
                {
                    val = image.get(x,y,z);
                    if ( val.floatValue() < min )
                    {
                        min = val.floatValue();
                    }
                    if ( val.floatValue() > max )
                    {
                        max = val.floatValue();
                    }                        
                }
            }
        }

        float[] x = new float[4];
        float[] y = new float[4];
        new Dimension(256, 256);
        float minImage, maxImage;

        if (image.getType() == ModelStorageBase.UBYTE) {
            minImage = 0;
            maxImage = 255;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            minImage = -128;
            maxImage = 127;
        } else {
            minImage = (float) image.getMin();
            maxImage = (float) image.getMax();
        }

        // Set LUT min max values;
        x[0] = minImage;
        x[1] = min;
        x[2] = max;
        x[3] = maxImage;

        y[0] = 255;
        y[1] = 255;
        y[2] = 0;
        y[3] = 0;

        LUT.getTransferFunction().importArrays(x, y, 4);
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
