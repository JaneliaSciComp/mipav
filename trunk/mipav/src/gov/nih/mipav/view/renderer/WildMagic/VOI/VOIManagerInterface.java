package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FilePaintBitmap;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionMaskToPaint;
import gov.nih.mipav.model.scripting.actions.ActionMaskToVOI;
import gov.nih.mipav.model.scripting.actions.ActionOpenAllVOIs;
import gov.nih.mipav.model.scripting.actions.ActionOpenVOI;
import gov.nih.mipav.model.scripting.actions.ActionPaintToVOI;
import gov.nih.mipav.model.scripting.actions.ActionSaveAllVOIs;
import gov.nih.mipav.model.scripting.actions.ActionSaveVOIIntensities;
import gov.nih.mipav.model.scripting.actions.ActionSelectAllVOIs;
import gov.nih.mipav.model.scripting.actions.ActionVOIToMask;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.UpdateVOISelectionListener;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIPolyLineSlice;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.model.structures.event.VOIVectorEvent;
import gov.nih.mipav.model.structures.event.VOIVectorListener;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.VOIHandlerInterface;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJPopupPt;
import gov.nih.mipav.view.ViewJPopupVOI;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.ViewOpenVOIUI;
import gov.nih.mipav.view.ViewOpenPaintUI;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.dialogs.JDialogAGVF;
import gov.nih.mipav.view.dialogs.JDialogBSmooth;
import gov.nih.mipav.view.dialogs.JDialogBSnake;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogBoundingVOIs;
import gov.nih.mipav.view.dialogs.JDialogConvexHull2D;
import gov.nih.mipav.view.dialogs.JDialogDualContourSearch;
import gov.nih.mipav.view.dialogs.JDialogEditCircleDiameter;
import gov.nih.mipav.view.dialogs.JDialogEditSquareLength;
import gov.nih.mipav.view.dialogs.JDialogEvolveBoundaryManual;
import gov.nih.mipav.view.dialogs.JDialogFlip;
import gov.nih.mipav.view.dialogs.JDialogGVF;
import gov.nih.mipav.view.dialogs.JDialogIntensityHistogram;
import gov.nih.mipav.view.dialogs.JDialogIntensityThreshold;
import gov.nih.mipav.view.dialogs.JDialogLivewire;
import gov.nih.mipav.view.dialogs.JDialogMask;
import gov.nih.mipav.view.dialogs.JDialogOpacityControls;
import gov.nih.mipav.view.dialogs.JDialogPointArea;
import gov.nih.mipav.view.dialogs.JDialogSaveMergedVOIs;
import gov.nih.mipav.view.dialogs.JDialogSnake;
import gov.nih.mipav.view.dialogs.JDialogSurfaceReconstruction;
import gov.nih.mipav.view.dialogs.JDialogTransformVOI;
import gov.nih.mipav.view.dialogs.JDialogTrim;
import gov.nih.mipav.view.dialogs.JDialogVOIBoolean;
import gov.nih.mipav.view.dialogs.JDialogVOIHausdorffDistance;
import gov.nih.mipav.view.dialogs.JDialogVOILogicalOperations;
import gov.nih.mipav.view.dialogs.JDialogVOIShapeInterpolation;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics;
import gov.nih.mipav.view.dialogs.JDialogVOIStats;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRenderBase;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.*;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Polygon;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.event.EventListenerList;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * VOIManagerInterface manages all the VOI user-interaction for classes that display the ModelImage.
 * ViewJFrameImage, ViewJFrameTriImage, ViewJFrameRegistration, ViewJFrameRegistrationTool, and
 * the VolumeTriPlanarInterface class for the GPU-based volume renderer.
 * 
 * This class further divides the VOI user-interaction into actions that occur within a single canvas and actions
 * that are shared across canvases.  All user-interaction that occurs within a single canvas, for example
 * direct user-manipulation of a single VOI contour, adding points, moving points, etc. are managed by the 
 * VOIManager class.  The ViewJFrameTriImage has three canvases when displaying a single ModelImage. Each
 * canvas is tied to a separate VOIManager class, which handles all the direct user-manipulation of the VOIs 
 * it displays. The VOIManagerInterface class contains the three VOIManagers and handles the communication 
 * between the VOIManager and the VOI, the ModelImage containing the VOI, and the class displaying the ModelImage. 
 * @see VOIManager.
 * 
 * In addition to managing the multiple VOIManager objects, this class handles all the VOI ActionEvents
 * generated by the VOI toolbars, VOI menu, and VOI popup interfaces.  Although this class provides many
 * public functions, the best way to communicate to this class is through the actionPerformed(ActionEvent)
 * function. Button parameters and ActionCommands are defined in the CustomUIBuilder class.
 * @see CustomUIBuilder. 
 *
 */
public class VOIManagerInterface implements ActionListener, VOIHandlerInterface, VOIListener, VOIVectorListener
{
    /**
     * Pick up the selected color and call method to change the color.
     */
    class OkColorListener implements ActionListener {

        /** Color Button */
        JButton button;

        /**
         * Creates a new OkColorListener object.
         * @param  color button
         */
        OkColorListener(JButton _button) {
            super();
            button = _button;
        }

        /**
         * Get color from chooser and set button and color.
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            saveVOIs( CustomUIBuilder.PARAM_VOI_COLOR.getActionCommand() );

            Color color = colorChooser.getColor();

            button.setBackground(color);
            setButtonColor(button, color);
        }
    }
    
    /** Reference to the parent frame. */
    protected VOIManagerInterfaceListener m_kParent = null;
    /** Reference to imageA */
    protected ModelImage m_kImageA;
    /** Reference to imageB */
    protected ModelImage m_kImageB;
    /** The toolbar builder that constructs the VOI toolbar -- it stores the VOI color button. */
    private ViewToolBarBuilder toolbarBuilder;
    /** Reference to the VOI toolbar. */
    private JToolBar m_kVOIToolbar;
    /** Reference to the color chooser. */
    protected ViewJColorChooser colorChooser;
    /** List of VOIManagers, one per canvas displayed. */
    protected Vector<VOIManager> m_kVOIManagers;
    /** The index of the current active VOIManager. */
    private int m_iActive = 0;
    /** VOI ID used to set the VOI color button on a call to newVOI() */
    private int voiUID = 0;
    /** The current active VOI */
    protected VOI m_kCurrentVOIGroup = null;
    /** List of undo commands */
    private Vector<String> m_kUndoCommands = new Vector<String>();
    /** List of re-do commands */
    private Vector<String> m_kRedoCommands = new Vector<String>();
    /** Opacity for setting the JDialogOpacityControls, used for VOI opacity. */
    private float m_fOpacity = 1f;
    /** Set to true if this VOIManagerInterface is used for the GPU-based Volume Renderer */
    private boolean m_bGPURenderer = false;
    protected VolumeTriPlanarRenderBase renderer;
    /** VOI Properties dialog -- from the popup menu or drop-down menu. */
    protected JDialogVOIStats m_kVOIDialog;
    
    private JDialogVOILogicalOperations m_kVOILogicalOperationsDialog;
    private JDialogVOIHausdorffDistance m_kVOIHausdorffDistanceDialog;
    
    private int m_iMaxUndo = 1000;
    /** Saved VOI states for undo. */
    protected Vector<VOISaveState> m_kUndoList = new Vector<VOISaveState>();
    /** Saved VOI states for re-do. */
    protected Vector<VOISaveState> m_kRedoList = new Vector<VOISaveState>();

    /** Single-level undo/redo for image masks, undo-imageA: */
    private Object m_kImageAUndo = null;
    /** Single-level undo/redo for image masks, re-do-imageA: */
    private Object m_kImageARedo = null;
    /** Single-level undo/redo for image masks, undo-imageB: */
    private Object m_kImageBUndo = null;
    /** Single-level undo/redo for image masks, re-do-imageB: */
    private Object m_kImageBRedo = null;

    /** Bounding box for a group of selected VOIs for group-move. */
    private Vector3f[] m_akBounds = new Vector3f[]{ new Vector3f(), new Vector3f() };

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

    /** Restores the VOI color button after QuickLUT. */
    protected Color currentColor = null;
    /** Restores the current VOI after QuickLUT. */
    protected VOI saveGroup = null;

    /** List of active VOIBase for a moving several selected VOIs. */
    private Vector<VOIBase> m_kActiveList = new Vector<VOIBase>();

    /** The Default pointer button is set from outside this class if the 
     * default pointer button is not part of the VOI toolbar, or if the VOI toolbar is not displayed.
     * The VOIManagers query the default pointer button to determine if VOI interaction is enabled. */
    private JToggleButton m_kPointerButton = null;
    
    /** Statistics dialog VOI->Statistics generator... */
    protected JDialogVOIStatistics imageStatList;
    
    
    private float presetHue = -1.0f;
    
    private boolean m_bDefaultImage;
    private ModelImage m_kTempImage = null;
    
    protected JMenu voiMenu;
    protected ViewMenuBuilder voiMenuBuilder;

    /**
     * Creates a VOIManagerInterface object.
     * @param kParent the parent frame, must be a VOIManagerInterfaceListener
     * @param kImageA imageA
     * @param kImageB imageB
     * @param iNViews number of views displayed in the parent.
     * @param bGPU set to true if this VOIManagerInterface is part of the GPU-based Volume Renderer.
     * @param kVOIGroup for ViewJFrameImage and ViewJFrameTriImage, so the VOI Toolbar can be part of a larger button group.
     */
    public VOIManagerInterface ( VOIManagerInterfaceListener kParent,
            ModelImage kImageA, ModelImage kImageB, int iNViews, boolean bGPU, ButtonGroup kVOIGroup )
    {
        m_kParent = kParent;
        m_kImageA = kImageA;
        m_kImageB = kImageB;        

        toolbarBuilder = new ViewToolBarBuilder(this);
        m_kVOIToolbar =
            toolbarBuilder.buildVolumeTriPlanarVOIToolBar( m_kImageA.getNDims(),
                    -1, bGPU, bGPU, kVOIGroup);
        m_kVOIToolbar.setVisible(false);
        toolbarBuilder.getVOIUndoButton().setEnabled(false);
        toolbarBuilder.getVOIRedoButton().setEnabled(false);
        m_kPointerButton = toolbarBuilder.getPointerButton();
        Color kColor = toolbarBuilder.getVOIColorButton().getBackground();
        new ColorRGB( kColor.getRed()/255.0f,
                kColor.getGreen()/255.0f,
                kColor.getBlue()/255.0f );
        m_bGPURenderer = bGPU;


        /**
         * Create Popup Dialogs for VOIs and VOI points
         */
        popup = new ViewJPopupVOI(this);

        if (getActiveImage().getNDims() < 3) {
            popup.setEnabledPropagate(false);
        }

        popupPt = new ViewJPopupPt(this);

        if (getActiveImage().getNDims() < 3) {
            popupPt.setEnabledGraph(false);
            popupPt.setEnabledProp(false);
        }

        if ( m_kImageA != null && m_kImageA.getVOIs() != null )
        {
            m_kImageA.getVOIs().addVectorListener(this);
            VOIVector kVOIs = m_kImageA.getVOIs();
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                kVOIs.elementAt(i).addVOIListener(this);
            }
        }
        if ( m_kImageB != null && m_kImageB.getVOIs() != null )
        {
            m_kImageB.getVOIs().addVectorListener(this);
            VOIVector kVOIs = m_kImageB.getVOIs();
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                kVOIs.elementAt(i).addVOIListener(this);
            }
        }
       
        initVOIManagers(iNViews);
    }

    /* 
     * Handles all VOI Action commands from the VOI toolbar and VOI Menu.
     * @param event ActionEvent
     */
    public void actionPerformed(ActionEvent event) {

        String command = event.getActionCommand();
        if ( (command != null) && ViewUserInterface.getReference().isShorcutRecording()) {
        	ViewUserInterface.getReference().setShortcutRecording(false);
            Preferences.addShortcut(command);
            ViewUserInterface.getReference().showShortcutEditor(true);

            return;
        }

        //System.err.println( command );
        if ( command.equals(CustomUIBuilder.PARAM_VOI_COLOR.getActionCommand()) ) {
            showColorDialog();
            setDefaultCursor();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_NEW.getActionCommand()) ) {
            newVOI(true, false);
            setDefaultCursor();
        } 
        else if ( command.equals(CustomUIBuilder.PARAM_VOI_LIVEWIRE.getActionCommand()) )
        {
            final JDialogLivewire dialog = new JDialogLivewire(null);
            if ( !dialog.isCancelled()) {
                boolean iActive = false;
                for (int i = 0; i < m_kVOIManagers.size(); i++) {
                    m_kVOIManagers.elementAt(i).liveWire( dialog.getSelection() );
                    iActive |= m_kVOIManagers.elementAt(i).isActive();
                }
                m_kParent.PointerActive(iActive);
            }
            else {
                setDefaultCursor();
            }
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_UNDO.getActionCommand()) ) {
            undoVOI();
            setDefaultCursor();
        }  
        else if (command.equals(CustomUIBuilder.PARAM_VOI_REDO.getActionCommand()) ) {
            redoVOI();
            setDefaultCursor();
        } 
        else if (command.equals("OpacityPaint")) {
            new JDialogOpacityControls(null, this, m_fOpacity);
        } 
        else if ( command.equals(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand() ) ) {
            saveImage(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand());
            createMask( CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand() );
            setDefaultCursor();
        } 
        else if ( command.equals(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand() ) ) {
            saveImage(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand());
            createMask( CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand() );
            setDefaultCursor();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_3D_INTERSECTION.getActionCommand()) ) {
            m_kParent.create3DVOI(true);
            setDefaultCursor();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_3D_UNION.getActionCommand()) ) {
            m_kParent.create3DVOI(false);
            setDefaultCursor();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_PROPERTIES.getActionCommand())) {
            showVOIProperties();
            setDefaultCursor();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_IMPORT_VOI_POLYGON.getActionCommand())) {
        	importVOI();
        }
        else if (command.equals(CustomUIBuilder.PARAM_OPEN_VOI.getActionCommand())) {

            boolean success = openVOI(false, false);

            if (success) {
                ScriptRecorder.getReference().addLine(new ActionOpenVOI(getActiveImage()));
                ProvenanceRecorder.getReference().addLine(new ActionOpenVOI(getActiveImage()));
            }else {
            	MipavUtil.displayError("VOI failed to open for this image");
            }
        } 
        else if (command.equals(CustomUIBuilder.PARAM_OPEN_VOI_IMAGEJ.getActionCommand())) {
        	// get the voi directory
            String fileName = null;
            String directory = null;
            String voiDir = null;

            final JFileChooser chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

            final int returnVal = chooser.showOpenDialog(m_kParent.getFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName);
                System.err.println("Opening ImageJ voi file " + voiDir );
                readImageJ( voiDir, fileName, getActiveImage());
            }
        } 
        else if (command.equals("NewVOIOtherOrientation")) {
        	boolean success = openOtherOrientationVOI(false);
        	if (!success) {
        		MipavUtil.displayError("VOI failed to open for this image");	
        	}
        }
        else if (command.equals(CustomUIBuilder.PARAM_OPEN_VOI_ALL.getActionCommand())) {
            loadAllVOIs(false);

            ScriptRecorder.getReference().addLine(new ActionOpenAllVOIs(getActiveImage()));
            ProvenanceRecorder.getReference().addLine(new ActionOpenAllVOIs(getActiveImage()));
        } 
        else if (command.equals(CustomUIBuilder.PARAM_OPEN_VOI_ALL_FROM.getActionCommand())) {

            // get the voi directory
            String fileName = null;
            String directory = null;
            String voiDir = null;

            final JFileChooser chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

            final int returnVal = chooser.showOpenDialog(m_kParent.getFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName + File.separator);
                loadAllVOIsFrom(voiDir, false);
            }
        } 
        else if (command.equals(CustomUIBuilder.PARAM_OPEN_VOI_IMAGEJ_ALL_FROM.getActionCommand())) {

            // get the voi directory
            String fileName = null;
            String directory = null;
            String voiDir = null;

            final JFileChooser chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

            final int returnVal = chooser.showOpenDialog(m_kParent.getFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName + File.separator);
                loadAllImageJVOIsFrom(voiDir, false, null, true);
            }
        } 
        else if (command.equals(CustomUIBuilder.PARAM_OPEN_VOI_LABEL.getActionCommand())) {
            openVOI(false, true);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_OPEN_VOI_CSV.getActionCommand())) {

            // get the voi directory
            String fileName = null;
            String directory = null;
            String voiDir = null;

            final JFileChooser chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

            final int returnVal = chooser.showOpenDialog(m_kParent.getFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName);
                System.err.println("Opening csv file " + voiDir );
                readCSV( voiDir, getActiveImage() );
            }
        }
        else if (command.equals(CustomUIBuilder.PARAM_OPEN_PAINT.getActionCommand())) {

            boolean success = openPaint(false);

            if (success) {
                ScriptRecorder.getReference().addLine(new ActionOpenVOI(getActiveImage()));
                ProvenanceRecorder.getReference().addLine(new ActionOpenVOI(getActiveImage()));
            }else {
            	MipavUtil.displayError("Paint bitmap failed to open for this image");
            }
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_SELECTED_CONTOURS.getActionCommand())) {
            saveVOI(false);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_EXPORT_SELECTED_CONTOURS_AS_POLYGON.getActionCommand())) {
            exportSelectedVOIs();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_SELECTED_CONTOURS_AS.getActionCommand())) {
            saveVOIAs(false);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_VOI.getActionCommand())) {
            saveVOI(true);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_VOI_AS.getActionCommand())) {
            saveVOIAs(true);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_ALL_VOI.getActionCommand())) {
            saveAllVOIs();

            ScriptRecorder.getReference().addLine(new ActionSaveAllVOIs(getActiveImage()));
            ProvenanceRecorder.getReference().addLine(new ActionSaveAllVOIs(getActiveImage()));
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_ALL_VOI_TO.getActionCommand())) {

            // get the voi directory
            String fileName = null;
            String directory = null;
            String voiDir = null;
            JPanel accessoryPanel = new JPanel();
            ButtonGroup VOIGroup;
            JRadioButton saveVOILPSButton;
            JRadioButton saveVOIVoxelButton;

            final JFileChooser chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setAccessory(accessoryPanel);
            accessoryPanel.setBorder(BorderFactory.createLineBorder(Color.black));
            accessoryPanel.setLayout(new BorderLayout());
            
            PanelManager optionsPanelManager = new PanelManager("Options");
            VOIGroup = new ButtonGroup();
            saveVOILPSButton = new JRadioButton("Save VOIs in LPS mm. coordinates", 
            		Preferences.is(Preferences.PREF_VOI_LPS_SAVE));
            saveVOILPSButton.setFont(MipavUtil.font12);
            saveVOILPSButton.setForeground(Color.black);
            saveVOILPSButton.addActionListener(this);
            saveVOILPSButton.setToolTipText("If selected, VOIs will be saved in LPS mm. coordinates.");
            VOIGroup.add(saveVOILPSButton);
            optionsPanelManager.add(saveVOILPSButton);
            
            saveVOIVoxelButton = new JRadioButton("Save VOIs in voxel coordinates", 
            		!Preferences.is(Preferences.PREF_VOI_LPS_SAVE));
            saveVOIVoxelButton.setFont(MipavUtil.font12);
            saveVOIVoxelButton.setForeground(Color.black);
            saveVOIVoxelButton.addActionListener(this);
            saveVOIVoxelButton.setToolTipText("If selected, VOIs will be saved in voxel coordinates.");
            VOIGroup.add(saveVOIVoxelButton);
            optionsPanelManager.addOnNextLine(saveVOIVoxelButton);
            accessoryPanel.add(optionsPanelManager.getPanel(), BorderLayout.CENTER);

            final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, String.valueOf(saveVOILPSButton.isSelected()));
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName + File.separator);
                saveAllVOIsTo(voiDir);

                ScriptRecorder.getReference().addLine(new ActionSaveAllVOIs(getActiveImage(), voiDir));
                ProvenanceRecorder.getReference().addLine(new ActionSaveAllVOIs(getActiveImage(), voiDir));
            }
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_VOI_INTENSITIES.getActionCommand())) {
            saveVOIIntensities();
            ScriptRecorder.getReference().addLine(new ActionSaveVOIIntensities(getActiveImage()));
            ProvenanceRecorder.getReference().addLine(new ActionSaveVOIIntensities(getActiveImage()));
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_VOI_INTENSITIES_TO.getActionCommand())) {

            // get the voi directory
            String fileName = null;
            String directory = null;
            String filePathName = null;
            
            final JFileChooser chooser = new JFileChooser();
	        chooser.setDialogTitle("Save intensities in VOI as");
	        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
	
	        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".txt"}));
	
	        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());
	
	        if (returnVal == JFileChooser.APPROVE_OPTION) {
	            fileName = chooser.getSelectedFile().getName();
	            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
	            
	
	        } else {
	            return;
	        }


            if (fileName != null) {
                filePathName = new String(directory + fileName);
                saveVOIIntensitiesTo(filePathName);

                ScriptRecorder.getReference().addLine(new ActionSaveVOIIntensities(getActiveImage(), directory));
                ProvenanceRecorder.getReference().addLine(new ActionSaveVOIIntensities(getActiveImage(), directory));
            }
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_VOI_CSV.getActionCommand())) {
            // get the voi directory
            String fileName = null;
            String directory = null;
            String filePathName = null;
            
            final JFileChooser chooser = new JFileChooser();
	        chooser.setDialogTitle("Save annotations as csv...");
	        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
	
	        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".csv"}));
	
	        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());
	
	        if (returnVal == JFileChooser.APPROVE_OPTION) {
	            fileName = chooser.getSelectedFile().getName();
	            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
	            
	
	        } else {
	            return;
	        }


            if (fileName != null) {
            	VOIVector vois = getActiveImage().getVOIs();
            	
            	for ( int i = 0; i < vois.size(); i++ )
            	{
            		if ( vois.elementAt(i).getCurveType() == VOI.ANNOTATION )
            		{
            			LatticeModel.saveAnnotationsAsCSV(directory, fileName,  vois.elementAt(i));
            			break;
            		}
            	}
            }
        }
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_PAINT.getActionCommand())) {
            savePaint();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_PAINT_AS.getActionCommand())) {
            savePaintAs();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_SELECTED_LABEL.getActionCommand())) {
            saveLabels(false);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_SAVE_ALL_LABEL.getActionCommand())) {
            saveLabels(true);
        } 
        /*else if (command.equals("XOR")) {
            if( event.getSource() instanceof JCheckBoxMenuItem )
            {
                ViewUserInterface.getReference().setUseVOIXOR(((JCheckBoxMenuItem)event.getSource()).isSelected());
            }
        } */
        else if (command.equals("PaintMask")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            getActiveImage().setMask(getActiveImage().generateVOIMask(false, true));
            m_kParent.setPaintMask(getActiveImage().getMask());
            updateDisplay();
            getActiveImage().notifyImageDisplayListeners();

        } 
        else if (command.equals("BinaryMask")) {
            if (getActiveImage().getVOIs().size() == 0) {
                MipavUtil.displayWarning("There are no VOIs in this image");
                return;
            }
            ModelImage maskImage = null;

            try {

                if (getActiveVOICount() == 0) {
                    selectAllVOIs(true);
                }
                maskImage = getActiveImage().generateBinaryImage(false, false);
               
                if (maskImage != null) {
                    maskImage.setImageName(getActiveImage().getImageName() + "_bmask");
                    maskImage.getMatrixHolder().replaceMatrices(getActiveImage().getMatrixHolder().getMatrices());
                    maskImage.getFileInfo(0).setOrigin(getActiveImage().getFileInfo(0).getOrigin());
                    new ViewJFrameImage(maskImage, null, new Dimension(610, 200), false);
                }
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");

                if (maskImage != null) {
                    maskImage.disposeLocal();
                }

                maskImage = null;

                return;
            }

            ScriptRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), maskImage, ActionVOIToMask.MASK_BINARY));
            ProvenanceRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), maskImage, ActionVOIToMask.MASK_BINARY));
        } else if (command.equals("ShortMask")) {
            if (getActiveImage().getVOIs().size() == 0) {
                MipavUtil.displayWarning("There are no VOIs in this image");
                return;
            }
            ModelImage shortImage = null;

            try {

                if (getActiveVOICount() == 0) {
                    selectAllVOIs(true);
                }
                shortImage = getActiveImage().generateShortImage(1, false, false);

                if (shortImage != null) {
                    shortImage.setImageName(getActiveImage().getImageName() + "_smask");
                    shortImage.getMatrixHolder().replaceMatrices(getActiveImage().getMatrixHolder().getMatrices());
                    shortImage.getFileInfo(0).setOrigin(getActiveImage().getFileInfo(0).getOrigin());
                    new ViewJFrameImage(shortImage, null, new Dimension(610, 200), false);
                }
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");

                if (shortImage != null) {
                    shortImage.disposeLocal();
                }

                shortImage = null;

                return;
            }

            ScriptRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), shortImage, ActionVOIToMask.MASK_SHORT));
            ProvenanceRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), shortImage, ActionVOIToMask.MASK_SHORT));
        } else if (command.equals("UnsignedByteMask")) {
            if (getActiveImage().getVOIs().size() == 0) {
                MipavUtil.displayWarning("There are no VOIs in this image");
                return;
            }
            ModelImage uByteImage = null;

            try {

                if (getActiveVOICount() == 0) {
                    selectAllVOIs(true);
                }
                uByteImage = getActiveImage().generateUnsignedByteImage(1, false, false);

                if (uByteImage != null) {
                    uByteImage.setImageName(getActiveImage().getImageName() + "_ubmask");
                    uByteImage.getMatrixHolder().replaceMatrices(getActiveImage().getMatrixHolder().getMatrices());
                    uByteImage.getFileInfo(0).setOrigin(getActiveImage().getFileInfo(0).getOrigin());
                    new ViewJFrameImage(uByteImage, null, new Dimension(610, 200), false);
                }
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");

                if (uByteImage != null) {
                    uByteImage.disposeLocal();
                }

                uByteImage = null;

                return;
            }

            ScriptRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), uByteImage, ActionVOIToMask.MASK_UBYTE));
            ProvenanceRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), uByteImage, ActionVOIToMask.MASK_UBYTE));
        } else if (command.equals("BinaryMaskSelected")) {

            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select at least 1 VOI!");
                return;
            }

            ModelImage maskImage = null;

            try {

                maskImage = getActiveImage().generateBinaryImage(false, true);

                if (maskImage != null) {
                    maskImage.setImageName(getActiveImage().getImageName() + "_bmask");
                    maskImage.getMatrixHolder().replaceMatrices(getActiveImage().getMatrixHolder().getMatrices());
                    maskImage.getFileInfo(0).setOrigin(getActiveImage().getFileInfo(0).getOrigin());
                    new ViewJFrameImage(maskImage, null, new Dimension(610, 200), false);
                }
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");

                if (maskImage != null) {
                    maskImage.disposeLocal();
                }

                maskImage = null;

                return;
            }

            ScriptRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), maskImage, ActionVOIToMask.MASK_BINARY));
            ProvenanceRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), maskImage, ActionVOIToMask.MASK_BINARY));
        } else if (command.equals("ShortMaskSelected")) {

            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select at least 1 VOI!");
                return;
            }

            ModelImage shortImage = null;

            try {

                shortImage = getActiveImage().generateShortImage(1, false, true);

                if (shortImage != null) {
                    shortImage.setImageName(getActiveImage().getImageName() + "_smask");
                    shortImage.getMatrixHolder().replaceMatrices(getActiveImage().getMatrixHolder().getMatrices());
                    shortImage.getFileInfo(0).setOrigin(getActiveImage().getFileInfo(0).getOrigin());
                    new ViewJFrameImage(shortImage, null, new Dimension(610, 200), false);
                }
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");

                if (shortImage != null) {
                    shortImage.disposeLocal();
                }

                shortImage = null;

                return;
            }

            ScriptRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), shortImage, ActionVOIToMask.MASK_SHORT));
            ProvenanceRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), shortImage, ActionVOIToMask.MASK_SHORT));
        } else if (command.equals("UnsignedByteMaskSelected")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select at least 1 VOI!");
                return;
            }

            ModelImage uByteImage = null;

            try {

                uByteImage = getActiveImage().generateUnsignedByteImage(1, false, true);

                if (uByteImage != null) {
                    uByteImage.setImageName(getActiveImage().getImageName() + "_ubmask");
                    uByteImage.getMatrixHolder().replaceMatrices(getActiveImage().getMatrixHolder().getMatrices());
                    uByteImage.getFileInfo(0).setOrigin(getActiveImage().getFileInfo(0).getOrigin());
                    new ViewJFrameImage(uByteImage, null, new Dimension(610, 200), false);
                }
            } catch (final OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");

                if (uByteImage != null) {
                    uByteImage.disposeLocal();
                }

                uByteImage = null;

                return;
            }

            ScriptRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), uByteImage, ActionVOIToMask.MASK_UBYTE));
            ProvenanceRecorder.getReference().addLine(
                    new ActionVOIToMask(getActiveImage(), uByteImage, ActionVOIToMask.MASK_UBYTE));
        } 
        else if (command.equals("MaskToVOI")) {
        	boolean wholeImage = true;
        	int originalType = getActiveImage().getType();
        	double originalMin = getActiveImage().getMin();
        	double originalMax = getActiveImage().getMax();
        	if (getActiveImage().getNDims() == 2) { 
                AlgorithmMorphology2D idObjectsAlgo2D;
                int method = AlgorithmMorphology2D.ID_OBJECTS;
                
                idObjectsAlgo2D = new AlgorithmMorphology2D(getActiveImage(), 0, 0, method, 0, 0, 0, 0, wholeImage);
                idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
                idObjectsAlgo2D.run();
                idObjectsAlgo2D.finalize();
                idObjectsAlgo2D = null;
        	}
        	else { 
                AlgorithmMorphology3D idObjectsAlgo3D;
                int method = AlgorithmMorphology3D.ID_OBJECTS;
                
                idObjectsAlgo3D = new AlgorithmMorphology3D(getActiveImage(), 0, 0, method, 0, 0, 0, 0, wholeImage);
                idObjectsAlgo3D.setMinMax(1, Integer.MAX_VALUE);
                idObjectsAlgo3D.run();
                idObjectsAlgo3D.finalize();
                idObjectsAlgo3D = null;
        	}
        	getActiveImage().calcMinMax();
            final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(getActiveImage());

            ViewJProgressBar progressBar = new ViewJProgressBar(getActiveImage().getImageName(), "Extracting VOI ...", 0, 100, true);
            progressBar.setSeparateThread(false);
            VOIExtractionAlgo.addProgressChangeListener(progressBar);
            VOIExtractionAlgo.setProgressValues(0, 100);

            // VOIExtractionAlgo.setActiveImage(false);
            VOIExtractionAlgo.run();
            
            if (originalType != getActiveImage().getType()) {
                AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(getActiveImage(), originalType,
                		              getActiveImage().getMin(), getActiveImage().getMax(), originalMin, originalMax, false);
                changeTypeAlgo.run();
                changeTypeAlgo.finalize();
                changeTypeAlgo = null;
            }
            selectAllVOIs(true);

            ScriptRecorder.getReference().addLine(new ActionMaskToVOI(getActiveImage()));
            ProvenanceRecorder.getReference().addLine(new ActionMaskToVOI(getActiveImage()));
            updateDisplay();
        } else if (command.equals("MaskToPaint")) {
            m_kParent.maskToPaint();
        }
        else if (command.equals("PaintToVOI")) {
            saveVOIs(command);
            paintToVOI();
        } 
        else if (command.equals("PaintToUbyteMask")) {
            m_kParent.paintToUbyteMask();
        } else if (command.equals("PaintToShortMask")) {
            m_kParent.paintToShortMask();
        } 
        
        else if (command.equals("Snake") || command.equals("AGVF") || 
        		command.equals("GVF") || command.equals("BSnake") || command.equals("EvolveConstant")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            evolveBoundary2D(command);
        } else if (command.equals("DCSearch")) {
        	evolveBoundary2D(command);	
        } else if (command.equals("SmoothVOI")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            saveVOIs(command);
            new JDialogBSmooth(m_kParent.getFrame(), this, getActiveImage(), getSlice());
        } else if (command.equals("ConvexHull2D")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            saveVOIs(command);
            new JDialogConvexHull2D(m_kParent.getFrame(), this, getActiveImage());
        } else if (command.equals("BooleanVOIs")) {
            saveVOIs(command);
            new JDialogVOIBoolean(m_kParent.getFrame(), this, getActiveImage());
        } else if (command.equals("IndentationVOIs2D")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            saveVOIs(command);
            indentationVOIs2D();
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_FLIPY.getActionCommand())) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            saveVOIs(command);
            final JDialogFlip flip = new JDialogFlip(m_kParent.getFrame(), getActiveImage(), AlgorithmFlip.Y_AXIS,
                    AlgorithmFlip.VOI_TYPE);
            flip.callAlgorithm();
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_FLIPX.getActionCommand())) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            saveVOIs(command);
            final JDialogFlip flip = new JDialogFlip(m_kParent.getFrame(), getActiveImage(), AlgorithmFlip.X_AXIS,
                    AlgorithmFlip.VOI_TYPE);
            flip.callAlgorithm();
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_FLIPZ.getActionCommand())) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            saveVOIs(command);
            final JDialogFlip flip = new JDialogFlip(m_kParent.getFrame(), getActiveImage(), AlgorithmFlip.Z_AXIS,
                    AlgorithmFlip.VOI_TYPE);
            flip.callAlgorithm();
        } else if (command.equals("interpolateVOIs")) {
            saveVOIs(command);
            interpolateVOIs();
        } 
        else if (command.equals("Trim")) {
            saveVOIs(command);
            final JDialogTrim trimSettings = new JDialogTrim(m_kParent.getFrame(), getActiveImage());

            trimSettings.setVisible(true);
        }
        else if (command.equals("BoundingVOIs")) {
        	m_bDefaultImage = true;
        	m_kTempImage = m_kVOIManagers.get(m_iActive).getLocalImage();
            if ( m_kTempImage != getActiveImage() )
            {
            	m_bDefaultImage = false;
            	m_kTempImage = (ModelImage)m_kTempImage.clone();
            }
            saveVOIs(command);
            JDialogBoundingVOIs kBound = new JDialogBoundingVOIs(m_kParent.getFrame(), m_kTempImage);
            kBound.setVOIManager(this);
        }
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_OPEN.getActionCommand())) {
            new ViewJFrameGraph("Graph", true);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_BOUNDARY_CURVATURE.getActionCommand())) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            saveVOIs(command);
            int reply = JOptionPane.showConfirmDialog(ViewUserInterface.getReference().getMainFrame(),
                    "Do you want graph a smoothed copy of the VOI?",
                    "Graph Smoothed VOI", JOptionPane.YES_NO_OPTION,
                    JOptionPane.QUESTION_MESSAGE);
            boolean smooth;
            if (reply == JOptionPane.YES_OPTION) {
                smooth = true;
            }
            else {
                smooth = false;
            }
            graphVOICurvature(smooth);
        }
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_BOUNDARY_INTENSITY.getActionCommand())) {
            graphVOI();
        }
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_INTENSITY_HISTOGRAM.getActionCommand())) {
            m_bDefaultImage = true;
            m_kTempImage = m_kVOIManagers.get(m_iActive).getLocalImage();
            if ( m_kTempImage != getActiveImage() )
            {
                m_bDefaultImage = false;
                m_kTempImage = (ModelImage)m_kTempImage.clone();
            } 
            new JDialogIntensityHistogram(m_kParent.getFrame(), m_kTempImage);    
        }
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_TOTAL_INTENSITY.getActionCommand())) {
            graph25VOI_CalcInten(true, false, 0);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_AVERAGE_INTENSITY.getActionCommand())) {
            graph25VOI_CalcInten(false, false, 0);
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_TOTAL_INTENSITY_THRESHOLD.getActionCommand())) {
            new JDialogIntensityThreshold(getFrame(), this,
                                          false);
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_AVERAGE_INTENSITY_THRESHOLD.getActionCommand())) {
            new JDialogIntensityThreshold(getFrame(), this,
                                          true);
        }
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_SHOW.getActionCommand())) {
            setGraphVisible();
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_GRAPH_PAAI.getActionCommand())) {
            setPAAIGraphVisible();        
        }
        else if (command.equals(CustomUIBuilder.PARAM_VOI_STATISTICS.getActionCommand())) {
            showStatisticsCalculator();
        }else if(command.equals(CustomUIBuilder.PARAM_VOI_EDIT_CIRCLE_DIAM.getActionCommand())) {
        	VOIManager voiManager = m_kVOIManagers.elementAt(m_iActive);
        	
        	VOIVector kVOIs = getActiveImage().getVOIs();
        	VOIBase activeVOI = null;
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
                {
                    VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                    if ( kCurrentVOI.isActive() )
                    {
                        activeVOI = kCurrentVOI;
                        break;
                    }
                }
            }

        	Vector3f kMin = activeVOI.getImageBoundingBox()[0];
            Vector3f kMax = activeVOI.getImageBoundingBox()[1];
            int width = (int) ((kMax.X  - kMin.X ) + 0.5f);

            float measuredWidth = (width) * getActiveImage().getResolutions(0)[0];
            DecimalFormat nf = new DecimalFormat( "0.0#" );
            
            String xUnitsString = Unit.getUnitFromLegacyNum(getActiveImage().getUnitsOfMeasure()[0]).getAbbrev();
            
            String measuredWidthString = String.valueOf(nf.format(measuredWidth));
            String widthString = String.valueOf(width);
        	
        	new JDialogEditCircleDiameter((Component)m_kParent, widthString,measuredWidthString,xUnitsString, getActiveImage().getResolutions(0), activeVOI, voiManager);
        	
        } else if(command.equals(CustomUIBuilder.PARAM_VOI_EDIT_SQUARE_LENGTH.getActionCommand())) {
        	VOIManager voiManager = m_kVOIManagers.elementAt(m_iActive);
        	
        	VOIVector kVOIs = getActiveImage().getVOIs();
        	VOIBase activeVOI = null;
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
                {
                    VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                    if ( kCurrentVOI.isActive() )
                    {
                        activeVOI = kCurrentVOI;
                        break;
                    }
                }
            }

        	Vector3f kMin = activeVOI.getImageBoundingBox()[0];
            Vector3f kMax = activeVOI.getImageBoundingBox()[1];
            int width = (int) ((kMax.X  - kMin.X ) + 0.5f);

            float measuredWidth = (width) * getActiveImage().getResolutions(0)[0];
            DecimalFormat nf = new DecimalFormat( "0.0#" );
            
            String xUnitsString = Unit.getUnitFromLegacyNum(getActiveImage().getUnitsOfMeasure()[0]).getAbbrev();
            
            String measuredWidthString = String.valueOf(nf.format(measuredWidth));
            String widthString = String.valueOf(width);
        	
        	new JDialogEditSquareLength((Component)m_kParent, widthString,measuredWidthString,xUnitsString, getActiveImage().getResolutions(0), activeVOI, voiManager);
        	
        }
        else if (command.equals(CustomUIBuilder.PARAM_VOI_LOGICAL_OPERATIONS.getActionCommand())) {
        	if ( (getActiveImage().getVOIs() != null) && (getActiveImage().getVOIs().size() >= 1 )) {
        		m_kVOILogicalOperationsDialog = new JDialogVOILogicalOperations(this,getActiveImage().getVOIs());

                 //addVOIUpdateListener(m_kVOILogicalOperationsDialog);
                 
                 
                 m_kVOILogicalOperationsDialog.setVisible(true);
                // addVOIUpdateListener(imageStatList); // i'd rather not do it this way...
            } else {
                MipavUtil.displayError("At least 1 VOI must be present to perform Logical Operations");
            }
        } 
        else if (command.equals(CustomUIBuilder.PARAM_VOI_HAUSDORFF_DISTANCE.getActionCommand())) {
        	if ( (getActiveImage().getVOIs() != null) && (getActiveImage().getVOIs().size() >= 2 )) {
        		m_kVOIHausdorffDistanceDialog = new JDialogVOIHausdorffDistance(this,getActiveImage().getVOIs());

                 //addVOIUpdateListener(m_kVOIHausdorffDistanceDialog);
                 
                 
                 m_kVOIHausdorffDistanceDialog.setVisible(true);
            } else {
                MipavUtil.displayError("At least 2 VOIs must be present to perform Hausdorff Distance");
            }
        } 
        else if ( command.equals("TransformVOI")){
        	new JDialogTransformVOI(m_kImageA);
        }
        else {
            doVOI(command);
        }

    }
	
    @Override
    public void addedCurve(VOIEvent added) {
        if ( m_kVOIDialog != null )
        {
            m_kVOIDialog.updateVOIPanel( added.getVOI(), getActiveImage() );
            m_kVOIDialog.updateTree();
        }
    }
    
    @Override
    public void addedVOI(VOIVectorEvent newVOIselection) {
        //System.err.println( "addedVOI " + this );
        newVOIselection.getVOI().addVOIListener(this);
        if ( m_kVOIDialog != null )
        {
            m_kVOIDialog.updateVOIPanel( newVOIselection.getVOI(), getActiveImage() );
            m_kVOIDialog.updateTree();
        }
        
    }
    
    /**
     * Add a new VOIBase. This function should only be called from VOIManager when a new VOIBase
     * is created with the mouse.
     * @param kNew the new VOIBase to add.
     * @param bQuickLUT true if this is a QuickLUT VOI.
     * @param bUpdate when true call updateDisplay() after the VOIBase is added
     * @param isFinished true if this VOIBase is complete, used for open contours.
     */
    public void addVOI( VOIBase kNew, boolean bQuickLUT, boolean bUpdate, boolean isFinished )
    {
        ModelImage kActive = getActiveImage();
        if ( kActive != null )
        {
            addVOI( kActive, kNew, bQuickLUT, bUpdate, isFinished );
            if (presetHue >= 0.0f) {
            	kNew.getGroup().setColor(presetHue);
            }
            if ( kActive.isRegistered( m_kCurrentVOIGroup ) == -1 )
            {
                kActive.registerVOI( m_kCurrentVOIGroup );
            }
        }
    }
    
    public VOIManager addVOIManager(ModelImage kImageA, ModelImage kImageB, Component kComponent, 
            ScreenCoordinateListener kContext, int iOrientation)
    {
        VOIManager kVOIManager = new VOIManager(this);
        kVOIManager.init(getFrame(), kImageA, kImageB,
                    kComponent, kContext,
                    iOrientation);
        kVOIManager.setPopupVOI(popup);
        kVOIManager.setPopupPt(popupPt);
        m_kVOIManagers.add(kVOIManager);
        return kVOIManager;
    }

    /**
     * Adds a UpdateVOISelectionListener.
     * @param listener will receive VOI selection events.
     */
    public void addVOIUpdateListener(UpdateVOISelectionListener listener) {
        listenerList.add(UpdateVOISelectionListener.class, listener);
    }

    public void algorithmPerformed()
    {
    	if ( !m_bDefaultImage && (m_kTempImage != null))
    	{
            int[] axisA = getActiveImage().getAxisOrientation();
            int[] axisB = m_kTempImage.getAxisOrientation();
            int[] axisOrder = { 0, 1, 2, 3 };
            boolean[] axisFlip = { false, false, false, false };
            if ( MipavCoordinateSystems.matchOrientation( axisA, axisB, axisOrder, axisFlip ) )
            {
                AlgorithmRotate rotateAlgo = new AlgorithmRotate( m_kTempImage, axisOrder, axisFlip );
                rotateAlgo.setRunningInSeparateThread(false);
                rotateAlgo.run();
                m_kTempImage = rotateAlgo.returnImage();
                getActiveImage().unregisterAllVOIs();            
                getActiveImage().setVOIs( m_kTempImage.getVOIs() );
                

                if ( getActiveImage() != null && getActiveImage().getVOIs() != null )
                {
                    getActiveImage().getVOIs().addVectorListener(this);
                    VOIVector kVOIs = getActiveImage().getVOIs();
                    for ( int i = 0; i < kVOIs.size(); i++ )
                    {
                        kVOIs.elementAt(i).addVOIListener(this);
                    }
                }
            }
            m_bDefaultImage = true;
            m_kTempImage = null;
    	}

        if ( m_kVOIDialog != null )
        {
            ModelImage kActive = getActiveImage();
            ViewVOIVector VOIs = kActive.getVOIs();
            for (int i = 0; i < VOIs.size(); i++) {
                if (VOIs.VOIAt(i).isActive()) {
                    m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(i), kActive );
                    m_kVOIDialog.updateTree();
                    return;
                }
            }
            if (VOIs.size() > 0) {
                m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(0), kActive );
                m_kVOIDialog.updateTree();
            }
        }
    	updateDisplay();
    }

    @Override
    public void colorChanged(Color c) { }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#deleteSelectedVOI(boolean)
     */
    public void deleteSelectedVOI(boolean contoursOnly)
    {
        ModelImage kActive = getActiveImage();
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



    /**
     * Deletes the input VOIBase from the VOI. If it is the only VOIBase in
     * the VOI, that VOI is removed from the ModelImage.
     * This function is called from VOIManager.
     * @param kOld the VOIBase to remove.
     */
    public void deleteVOI(VOIBase kOld, boolean update) {
        m_kCurrentVOIGroup = null;
        if ( kOld == null )
        {
            return;
        }
        VOI kGroup = kOld.getGroup();
        if ( kGroup != null )
        {
            kGroup.getCurves().remove(kOld);
            if ( kGroup.isEmpty() )
            {
                getActiveImage().unregisterVOI(kGroup);
                if ( m_kImageA != null ) { m_kImageA.unregisterVOI(kGroup); }
                if ( m_kImageB != null ) { m_kImageB.unregisterVOI(kGroup); }
            }
        }
        if ( update )
        {
        	updateDisplay();
        }
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#deleteVOIs()
     */
    public void deleteVOIs()
    {
        saveVOIs("deleteVOIs");
        deleteAllVOI();
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#disposeLocal(boolean)
     */
    public void disposeLocal(boolean flag)
    {
        
        if ( m_kImageA != null && m_kImageA.getVOIs() != null )
        {
            VOIVector kVOIs = m_kImageA.getVOIs();
            kVOIs.removeVectorListener(this);
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                kVOIs.elementAt(i).removeVOIListener(this);
            }
        }
        if ( m_kImageB != null && m_kImageB.getVOIs() != null )
        {
            VOIVector kVOIs = m_kImageB.getVOIs();
            kVOIs.removeVectorListener(this);
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                kVOIs.elementAt(i).removeVOIListener(this);
            }
        }
        
        if (popup != null) {
            popup = null;
        }

        if (popupPt != null) {
            popupPt = null;
        }

        m_kImageA = null;
        m_kImageB = null;

        toolbarBuilder = null;
        m_kVOIToolbar = null;
        colorChooser = null;
        m_kUndoCommands = null;
        m_kRedoCommands = null;

        if ( m_kUndoList != null )
        {
        	clearList( m_kUndoList, m_kUndoList.size() );
        	m_kUndoList = null;
        }
        if ( m_kRedoList != null )
        {
        	clearList( m_kRedoList, m_kRedoList.size() );
        	m_kRedoList = null;
        }
        
        for ( int i = m_kVOIManagers.size() - 1; i >= 0; i-- )
        {
            VOIManager kManager = m_kVOIManagers.remove(i);
            kManager.dispose();
        }
        m_kVOIManagers = null;   

        m_kImageAUndo = null;
        m_kImageARedo = null;        
        m_kImageBUndo = null;
        m_kImageBRedo = null;

        listenerList = null;
        m_kParent = null;
    }



    /**
     * Performs the VOI Action Command. See CustomUIBuilder.
     * This function should be replaced withe actionPerformed(ActionEvent).
     * @param kCommand VOI action command.
     */
    public void doVOI( String kCommand )
    {        
        boolean bDraw = isDrawCommand(kCommand);

        if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_QUICK.getActionCommand()) )
        {
            saveGroup = m_kCurrentVOIGroup;
            m_kCurrentVOIGroup = null;
            currentColor = toolbarBuilder.getVOIColorButton().getBackground();
        }

        if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_PROPAGATE_UP.getActionCommand()) )
        {
            if ( copy() > 0 )
            {
                saveVOIs(kCommand);
                propagate(1);
            }
            setDefaultCursor();
        }
        else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_PROPAGATE_DOWN.getActionCommand()) )
        {
            if ( copy() > 0 )
            {
                saveVOIs(kCommand);
                propagate(-1);
            }
            setDefaultCursor();
        }
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL.getActionCommand()) ) {
            if ( copy() > 0 )
            {
                saveVOIs(kCommand);
                pasteAll();
            }
            setDefaultCursor();
        }
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_PROPAGATE_3D_LEVELSET.getActionCommand()) ) {
            if ( copy() > 0 )
            {
                saveVOIs(kCommand);
                levelSet3D();
            }
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
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_SELECT_ALL.getActionCommand()) )
        {
            selectAllVOIs(true);
            ScriptRecorder.getReference().addLine(new ActionSelectAllVOIs(getActiveImage()));
        } else if(kCommand.equals(CustomUIBuilder.PARAM_CONTOUR_SELECT_ALL.getActionCommand())) {
            setSelectedVOI(m_kCurrentVOIGroup, true, false);
        } else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_SELECT_NONE.getActionCommand())) {
            selectAllVOIs(false);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_GROUP.getActionCommand())) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select VOIs!");
                return;
            }
            saveVOIs(kCommand);
            getActiveImage().groupVOIs();
            fireVOISelectionChange(null);
        } else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_UNGROUP.getActionCommand())) {
            saveVOIs(kCommand);
            getActiveImage().ungroupVOIs();
            fireVOISelectionChange(null);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_DELETE.getActionCommand()) ) {
            saveVOIs(kCommand);
            deleteSelectedVOI(false);
            setDefaultCursor();
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_CUT.getActionCommand()) ) {
            cut();
            setDefaultCursor();
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_COPY.getActionCommand()) ) {
            copy();
            setDefaultCursor();
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_PASTE.getActionCommand()) ) {
            paste();
            setDefaultCursor();
        }
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_SHOW_CONTOUR_BOUNDING_BOX.getActionCommand()) ) {
        	VOIVector kVOIs = getActiveImage().getVOIs();
        	
            for ( int i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
                {
                    VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                    if ( kCurrentVOI.isActive() )
                    {
                    	 boolean flag = true;
                    	 if (kCurrentGroup.getBoundingBoxFlag() == true) {
                    		 flag = false;
                    		 kCurrentGroup.setBoundingBoxFlag(flag);
                         } else {
                        	 flag = true;
                        	 kCurrentGroup.setBoundingBoxFlag(flag);
                         }
                    	 updateDisplay();
                    	 if(m_kVOIDialog != null) {
                    		 m_kVOIDialog.setCheckboxBoundingBox(flag);
                    	 }
                        break;
                    }
                }
            }
            setDefaultCursor();
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_FRONT.getActionCommand())) {
            changeVOIOrder(false, VOI.FRONT);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_BACK.getActionCommand())) {
            changeVOIOrder(false, VOI.BACK);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_FORWARD.getActionCommand())) {
            changeVOIOrder(false, VOI.FORWARD);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_BACKWARD.getActionCommand())) {
            changeVOIOrder(false, VOI.BACKWARD);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_CONTOUR_FRONT.getActionCommand())) {
            changeVOIOrder(true, VOI.FRONT);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_CONTOUR_BACK.getActionCommand())) {
            changeVOIOrder(true, VOI.BACK);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_CONTOUR_FORWARD.getActionCommand())) {
            changeVOIOrder(true, VOI.FORWARD);
        } 
        else if (kCommand.equals(CustomUIBuilder.PARAM_CONTOUR_BACKWARD.getActionCommand())) {
            changeVOIOrder(true, VOI.BACKWARD);
        } 
        else
        {
        	if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_3D_RECTANGLE.getActionCommand()) )
        	{
        		selectAllVOIs(false);
        	}
            boolean iActive = false;
            for (int i = 0; i < m_kVOIManagers.size(); i++) {
                m_kVOIManagers.elementAt(i).doVOI( kCommand, bDraw );
                iActive |= m_kVOIManagers.elementAt(i).isActive();
            }
            if ( m_kParent != null )
            {
            	m_kParent.PointerActive(iActive);
            }
        }
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#fireVOISelectionChange(gov.nih.mipav.model.structures.VOI)
     */
    public void fireVOISelectionChange(VOI voi) {
        //fireVOISelectionChange(voi, null);
        //System.err.println( "fireVOISelectionChange 1" );
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#fireVOISelectionChange(gov.nih.mipav.model.structures.VOI, gov.nih.mipav.model.structures.VOIBase)
     */
    public void fireVOISelectionChange(VOI voi, VOIBase curve) {
        //System.err.println( "fireVOISelectionChange 2" );
        /*
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
        */
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#getActiveImage()
     */
    public ModelImage getActiveImage() {
    	
    	if ( m_kParent == null )
    	{
    		return m_kImageA;
    	}
    	if ( m_kParent.getActiveImage() == null )
    	{
    		return m_kImageA;
    	}
        return m_kParent.getActiveImage();
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#getComponentImage()
     */
    public Component getComponentImage() {
        return m_kVOIManagers.elementAt(m_iActive).getComponent();
    }


    /**
     * Returns the pointer button, used by VOIManager to determine
     * if the pointer button is active.
     * @return current pointer button.
     */
    public JToggleButton getPointerButton( )
    {
        return m_kPointerButton;
    }

    /**
     * Returns the VOIToolbar.
     * @return VOIToolbar.
     */
    public JToolBar getToolBar()
    {
        return m_kVOIToolbar;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#getVOI_ID()
     */
    public int getVOI_ID() {
        return 0;
    }

    /**
     * Returns the VOIManager at the given position.
     * @param i the index of the VOIManager to return.
     * @return VOIManager.
     */
    public VOIManager getVOIManager(int i)
    {
        if ( i < m_kVOIManagers.size() )
        {
            return m_kVOIManagers.elementAt(i);
        }
        return null;
    }
    
    /**
     * Returns the number of VOIManagers controlled by this VOIHandler.
     */
    public int getVOIManagerNum()
    {
        return m_kVOIManagers.size();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#graph25VOI_CalcInten(boolean, boolean, float)
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


        ModelImage kImage = getActiveImage();

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
                                    Unit.getUnitFromLegacyNum(getActiveImage().getUnitsOfMeasure(2)).getAbbrev());

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
                                            intensitySum += curves[s].elementAt(j).calcIntensityThreshold( kImage, threshold, 0 );

                                        } else {
                                            intensitySum += curves[s].elementAt(j).calcIntensity( kImage, 0 );
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
                                    Unit.getUnitFromLegacyNum(getActiveImage().getUnitsOfMeasure(0)).getAbbrev(),
                                    null);

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
        
        else if (kImage.getNDims() == 4) {
            int zDim = kImage.getExtents()[2];
            int tDim = kImage.getExtents()[3];
            boolean useFrameRefTime = false;
            FileInfoDicom fileInfo = null;
            String frameRefTimeString = null;
            int frameReferenceTime = 0;
       
            if (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM) {
                boolean frameRefTimeFound = false;
                fileInfo = (FileInfoDicom) (kImage.getFileInfo(0)); 
                frameRefTimeString = ((String) fileInfo.getTagTable().getValue("0054,1300")).trim();
                if (frameRefTimeString != null) {
                    try {
                        frameReferenceTime = new Integer(frameRefTimeString).intValue();
                        frameRefTimeFound = true;
                        Preferences.debug("Frame reference time = " + frameReferenceTime + "\n");
                    } catch (NumberFormatException e) {
                        Preferences.debug("Number format excepton from frame Reference Time String = " +
                                          frameRefTimeString + "\n");
                    }
                    
                    if (frameRefTimeFound) {
                        int response = JOptionPane.showConfirmDialog(ViewUserInterface.getReference().getMainFrame(),
                                                                     new String("Do you wish to use the frame reference time for the graph x axis?"),
                                                                     "Frame Reference Time?",
                                                                     JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE); 
                        if (response == JOptionPane.YES_OPTION) {
                            useFrameRefTime = true;    
                        }
                    } // if (frameRefTimeFound)
                } // if (frameRefTimeString != null)
            } // if (compImage.getActiveImage().getFileInfo()[0].getFileFormat() == FileUtility.DICOM)

            try {

                VOIs = kImage.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible() &&
                            (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ) {

                        v = VOIs.VOIAt(i);


                        position = new float[tDim];
                        intensity = new float[tDim];

                        for (int t = 0; t < tDim; t++) {
                        	numPixels = 0;
                        	intensitySum = 0;

                            Vector<VOIBase>[] curves = v.getSortedCurves( VOIBase.ZPLANE, kImage.getExtents()[2]);
                            for ( s = 0; s < kImage.getExtents()[2]; s++ )
                            {
                            	if ( curves[s] != null )
                            	{
                            		for (j = 0; j < curves[s].size(); j++)
                            		{
                            			if (useThreshold) {
                            				intensitySum += curves[s].elementAt(j).calcIntensityThreshold( kImage, threshold, t );

                            			} else {
                            				intensitySum += curves[s].elementAt(j).calcIntensity( kImage, t );
                            			}

                            			numPixels += curves[s].elementAt(j).getLastNumPixels();                                        
                            		}
                            	}
                            }

                            if (useFrameRefTime) {
                                fileInfo = (FileInfoDicom) (kImage.getFileInfo(t * zDim)); 
                                frameRefTimeString = ((String) fileInfo.getTagTable().getValue("0054,1300")).trim();
                                if (frameRefTimeString != null) {
                                    try {
                                        frameReferenceTime = new Integer(frameRefTimeString).intValue();
                                    } catch (NumberFormatException e) {
                                        MipavUtil.displayError("Number format excepton from frame Reference Time String = " +
                                                          frameRefTimeString);
                                        return;
                                    }
                                    
                                    position[t] = frameReferenceTime;
                                } // if (frameRefTimeString != null) 
                                else {
                                    MipavUtil.displayError("Frame reference time string is null");
                                    return;
                                }
                            } // if (useFrameRefTime)
                            else {
                                position[t] = t;
                            }

                            if (totalIntensity || (numPixels == 0)) {
                                intensity[t] = intensitySum;
                            } else {
                                intensity[t] = intensitySum / numPixels;
                            }
                        }

                        ViewJFrameGraph contourGraph = new ViewJFrameGraph(
                                position,
                                intensity,
                                "Intensity Graph",
                                v,
                                Unit.getUnitFromLegacyNum(getActiveImage().getUnitsOfMeasure(0)).getAbbrev(),null);

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
                MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                return;
            }
        }
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#isNewVoiNeeded(int)
     */
    public boolean isNewVoiNeeded(int voiType) {
        return false;
    }


    /**
     * Creates a 3D Surface out of the VOIs.
     * @param bIntersection when true the intersection of the VOIs is used to create the surface, when false
     * a union of the VOIs is used to create the surface.
     * @param kVolume ModelImage, the union or intersection is written into the input ModelImage.
     * @return true if success, false if no voxels are in the union or intersection.
     */
    public boolean make3DVOI( boolean bIntersection, ModelImage kVolume  )
    {
        boolean bCreated = true;
        for (int i = 0; i < m_kVOIManagers.size(); i++) {
            bCreated &= make3DVOI(bIntersection, getActiveImage(), kVolume, null, 
            		m_kVOIManagers.elementAt(i), i);
        }
        return bCreated;
    }

    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
     */
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


    /* (non-Javadoc)
     * @see java.awt.event.MouseMotionListener#mouseDragged(java.awt.event.MouseEvent)
     */
    public void mouseDragged(MouseEvent arg0) {}



    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
     */
    public void mouseEntered(MouseEvent e) {}


    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
     */
    public void mouseExited(MouseEvent e) {}

    /* (non-Javadoc)
     * @see java.awt.event.MouseMotionListener#mouseMoved(java.awt.event.MouseEvent)
     */
    public void mouseMoved(MouseEvent arg0) {}

    
    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
     */
    public void mousePressed(MouseEvent e) {}
    
    /* (non-Javadoc)
     * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
     */
    public void mouseReleased(MouseEvent e) {}
    
    public void moveVOI( String kCommand, float fScale )
    {
        if ( fScale < 1 )
        {
            fScale = 1;
        }
        if ( kCommand.equals("MoveUP") )
        {
            moveVOI( m_kVOIManagers.elementAt(m_iActive), new Vector3f( 0, -fScale, 0 ), -1, true, false );
        }
        else if ( kCommand.equals("MoveDown") )
        {
            moveVOI( m_kVOIManagers.elementAt(m_iActive), new Vector3f( 0, fScale, 0 ), -1, true, false  );
        }
        else if ( kCommand.equals("MoveLeft") )
        {
            moveVOI( m_kVOIManagers.elementAt(m_iActive), new Vector3f(-fScale, 0, 0 ), -1, true, false  );
        }
        else if ( kCommand.equals("MoveRight") )
        {
            moveVOI( m_kVOIManagers.elementAt(m_iActive), new Vector3f(fScale, 0, 0 ), -1, true, false  );
        }        
    }

    /**
     * Called from the VOIManager class when multiple contours are selected and moved as
     * a group. This function calculates the group bounding box and tests the move to ensure
     * the contours are not moved out of the image bounds.
     * @param kActive VOIManager initiating the call.
     * @param kDiff the move distance
     * @param iPlane the plane of the VOIManager (equivalent to Axial, Coronal, Sagittal)
     * @param bFirstMove true if this is the first move for the selected group of contours.
     */
    public void moveVOI( VOIManager kActive, Vector3f kDiff, int iPlane, boolean bFirstMove, boolean bUseMouse )
    {
        if ( bFirstMove )
        {
            boolean bFirst = true;
            m_kActiveList = new Vector<VOIBase>();
            VOIVector kVOIs = getActiveImage().getVOIs();
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
                            m_akBounds[0].copy(kBounds[0]);
                            m_akBounds[1].copy(kBounds[1]);
                        }
                        m_akBounds[0].min(kBounds[0]);
                        m_akBounds[1].max(kBounds[1]);
                    }
                }
            }
        }

        if ( kActive.testMove( kDiff, m_akBounds, bUseMouse ) )
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
    
    /**
     * Initiate a new VOI.
     * @param bPropagate when true propagate the newVOI command to the VOIManagers.
     * @param bSplit true if this is a new SplitVOI.
     */
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
        
        advanceVOIUID();
        short sID = (short)(getActiveImage().getVOIs().getUniqueID());
        m_kCurrentVOIGroup = new VOI( sID,  new String( "_" + sID ) );
        m_kCurrentVOIGroup.addVOIListener(this);
        if (presetHue >= 0.0) {
        	m_kCurrentVOIGroup.setColor(presetHue);
        }
        m_kCurrentVOIGroup.setOpacity(1f);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#newVOI(float)
     */
    public void newVOI( float presetHue )
    {
        selectAllVOIs(false);
        doVOI(CustomUIBuilder.PARAM_VOI_NEW.getActionCommand());
        m_kCurrentVOIGroup = null;
        setPresetHue(presetHue);
        advanceVOIUID();
        short sID = (short)(getActiveImage().getVOIs().getUniqueID());
        m_kCurrentVOIGroup = new VOI( sID,  new String( "_" + sID ) );
        m_kCurrentVOIGroup.addVOIListener(this);
        m_kCurrentVOIGroup.setOpacity(1f);
    }

    /**
     * Paste the input VOIBase.
     * @param kNew the enw VOIBase to paste.
     */
    public void pasteVOI(VOIBase kNew)
    {
    	PrintStream out = System.out;
        if ( kNew.getGroup() == null )
        {
            if ( m_kCurrentVOIGroup == null )
            {
                newVOI(false, false);
            }
            kNew.setGroup(m_kCurrentVOIGroup);
        }
        kNew.getGroup().getCurves().add(kNew);
        if (getActiveImage().isRegistered(kNew.getGroup()) == -1 )
        {
            getActiveImage().registerVOI(kNew.getGroup());
        }

        if ( (m_kUndoCommands.size() > 0) && (m_kUndoCommands.lastElement() != null) && 
                !m_kUndoCommands.lastElement().equals(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL.getActionCommand()) )
        {
            setCenter(kNew.getGeometricCenter(), true );
        }
        out.println("Interface");
        updateDisplay();

    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#propVOI(int, boolean)
     */
    public boolean propVOI(int direction, boolean active)
    {
        if ( copy() <= 0 )
        {
            return false;
        }
        if ( direction > 0 )
        {
            saveVOIs(CustomUIBuilder.PARAM_VOI_PROPAGATE_UP.getActionCommand());
        }
        else
        {
            saveVOIs(CustomUIBuilder.PARAM_VOI_PROPAGATE_DOWN.getActionCommand());
        }
        propagate(direction);
        updateDisplay();
        return true;
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#propVOIAll()
     */
    public boolean propVOIAll() {
        if ( copy() <= 0 )
        {
            return false;
        }
        saveVOIs(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL.getActionCommand());
        deleteActiveVOI();
        pasteAll();
        return true;        
    }

    /**
     * Called from the VOIManager class when a user has drawn the QuickLUT rectangle.
     * This function calculates the new LUT and updates the images.
     * @param kLUT VOIBase defining the QuickLUT rectangle.
     */
    public void quickLUT( VOIBase kLUT )
    {
    	if ( kLUT == null )
    	{
    		return;
    	}
        kLUT.update();
        Vector3f[] kBounds = kLUT.getImageBoundingBox();
        m_akBounds[0].copy(kBounds[0]);
        m_akBounds[1].copy(kBounds[1]);        
        deleteVOI( kLUT, true );
        if (getActiveImage().isColorImage() == false) {
            quickLUT( m_akBounds, getActiveImage(), m_kParent.getActiveLUT() );           
        } else { // RGB image
            quickRGB( m_akBounds, getActiveImage(), m_kParent.getActiveRGB() );
        }

        getActiveImage().notifyImageDisplayListeners(null,true);
        if ( getActiveImage().getHistogramFrame() != null )
        {
            getActiveImage().getHistogramFrame().redrawFrames();
        }

        toolbarBuilder.getVOIColorButton().setBackground( currentColor );
        m_kCurrentVOIGroup = saveGroup;
    }

    @Override
    public void removedCurve(VOIEvent removed) { 
        //System.err.println( "removedCurve " + this );
        if ( m_kVOIDialog != null )
        {
            ModelImage kActive = getActiveImage();
            ViewVOIVector VOIs = kActive.getVOIs();
            for (int i = 0; i < VOIs.size(); i++) {
                if (VOIs.VOIAt(i).isActive()) {
                    m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(i), kActive );
                    m_kVOIDialog.updateTree();
                    return;
                }
            }
            if (VOIs.size() > 0) {
                m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(0), kActive );
                m_kVOIDialog.updateTree();
            }
        }
     }

    @Override
    public void removedVOI(VOIVectorEvent removed) {
        //System.err.println( "removedVOI " + this );
        if ( m_kVOIDialog != null )
        {
            ModelImage kActive = getActiveImage();
            ViewVOIVector VOIs = kActive.getVOIs();
            for (int i = 0; i < VOIs.size(); i++) {
                if (VOIs.VOIAt(i).isActive()) {
                    m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(i), kActive );
                    m_kVOIDialog.updateTree();
                    return;
                }
            }
            if (VOIs.size() > 0) {
                m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(0), kActive );
                m_kVOIDialog.updateTree();
            }
        }
    }

    public void removeVOIManager( VOIManager kVOIManager )
    {
        if ( kVOIManager != null )
        {
            m_kVOIManagers.remove(kVOIManager);
            kVOIManager.dispose();
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#removeVOIUpdateListener(gov.nih.mipav.model.structures.UpdateVOISelectionListener)
     */
    public void removeVOIUpdateListener(UpdateVOISelectionListener listener) {
        listenerList.remove(UpdateVOISelectionListener.class, listener);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#resetLivewire()
     */
    public void resetLivewire() 
    {
        for (int i = 0; i < m_kVOIManagers.size(); i++) {
            m_kVOIManagers.elementAt(i).doVOI( "ResetLiveWire", false );
        }
    }
    
    /**
     * Save the current VOIState to the undo/re-do list.
     * @param kCommand the VOI Action Command about to be issued.
     */
    public void saveVOIs( String kCommand )
    {    	
//    	System.err.println("saveVOIs " + kCommand );
        if ( m_kUndoList.size() > m_iMaxUndo )
        {
        	while ( m_kUndoList.size() > m_iMaxUndo )
        	{
        		VOISaveState state = m_kUndoList.remove(0);
        		state.dispose();
        		state = null;

                m_kUndoCommands.remove( 0 );
        	}
        	System.gc();
        }
        m_kUndoCommands.add( kCommand );
        m_kUndoList.add( getVOIState() );
        m_kRedoCommands.clear();
    	clearList( m_kRedoList, m_kRedoList.size() );
        toolbarBuilder.getVOIUndoButton().setEnabled(true);
        toolbarBuilder.getVOIRedoButton().setEnabled(false);
    }
    
    public void clearUndoRedo()
    {
        m_kUndoCommands.clear();
        m_kUndoList.clear();
        m_kRedoCommands.clear();
        m_kRedoList.clear();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#selectAllVOIs(boolean)
     */
    public void selectAllVOIs(boolean bActive)
    {
        VOIVector kVOIs = getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            kCurrentGroup.setAllActive(bActive);
            m_kCurrentVOIGroup = kCurrentGroup;
        }
        if ( m_kParent != null && getActiveImage() != null &&
        		getActiveImage().getParentFrame() != null )
        {
            if(getActiveImage().getTriImageFrame() != null && 
                    m_kParent.equals(getActiveImage().getTriImageFrame())) {
                getActiveImage().getTriImageFrame().toFront();
            } else {
                getActiveImage().getParentFrame().toFront();
            }
        }
        if ( m_kParent != null && m_kParent.getFrame() != null )
        {
        	m_kParent.getFrame().toFront();
        }
        updateDisplay();
    }


    @Override
    public void selectedVOI(VOIEvent selection) {
    	//System.out.println("selected voi");
        //System.err.println( "VOIManagerInterface.selectedVOI" );
        if ( m_kVOIDialog != null )
        {
            m_kVOIDialog.updateVOIPanel( selection.getVOI(), getActiveImage() );
            m_kVOIDialog.updateTree();
        }
        if ( m_kVOILogicalOperationsDialog != null )
        {
        	m_kVOILogicalOperationsDialog.updateVOI( selection.getVOI(), getActiveImage() );
            
        }
    }

    /**
     * Called from the VOIManager. Sets the current active VOIManager.
     * @param kManager calling VOIManager.
     */
    public void setActive( VOIManager kManager, ModelImage kActiveImage )
    {
        for ( int i = 0; i < m_kVOIManagers.size(); i++ )
        {
            if ( kManager == m_kVOIManagers.elementAt(i) )
            {
                m_iActive = i;
                if ( kActiveImage != getActiveImage() )
                {
                    m_kParent.setActiveImage( kActiveImage );
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

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#setCenter(WildMagic.LibFoundation.Mathematics.Vector3f)
     */
    public void setCenter( Vector3f center )   { }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#setCenter(WildMagic.LibFoundation.Mathematics.Vector3f, boolean)
     */
    public void setCenter( Vector3f center, boolean bParent )
    {
        if ( bParent )
        {
            m_kParent.setCenter(center);
            return;
        }
    }

    /**
     * Called from VOIManager to change the mouse cursor.
     * @param kCursor new Cursor.
     */
    public void setCursor(Cursor kCursor) {
        m_kParent.setCursor(kCursor);    
    }

    /**
     * Called from VOIManager to set the default mouse cursor.
     * This function triggers events in the parent frame.
     */
    public void setDefaultCursor() {         
        toolbarBuilder.setPointerSelected();
        actionPerformed( new ActionEvent ( this, 0, CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER.getActionCommand()) );
    }
    
    /**
     * Currently not used.
     * @param flag
     */
    public void setEnabled( @SuppressWarnings("unused") boolean flag ) {}

    /**
     * Called from JDialogOpacityControls. Sets the VOI opacity.
     * @param fVal VOI opacity.
     */
    public void setOpacity( float fVal )
    {
        m_fOpacity = fVal;
        if ( m_kCurrentVOIGroup != null )
        {
            m_kCurrentVOIGroup.setOpacity( m_fOpacity );
        }
    }


    /**
     * Called when the Default Pointer Button is not part of the default VOI Toolbar.
     * @param button default pointer button.
     */
    public void setPointerButton( JToggleButton button )
    {
        m_kPointerButton = button;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#setPresetHue(float)
     */
    public void setPresetHue(float presetHue) {
    	this.presetHue = presetHue;
    	if ( presetHue < 0 )
    	{
    	    return;
    	}
        Color color = Color.getHSBColor(presetHue, 1.0f, 1.0f);
        setButtonColor(toolbarBuilder.getVOIColorButton(), color );
    }
    
    public void setRenderer( VolumeTriPlanarRenderBase renderer ) {
    	this.renderer = renderer;
    }

    /**
     * Called from the VOIManager when a new VOI is selected by the mouse.
     * @param kSelected selected VOI.
     * @param bSelectAll when true select all contours in the VOI (usually shift-key is down).
     * @param bUnSelectVOI when true unselect the previously selected VOI 
     * (if the control-key is down multiple different VOIs can be selected).
     */
    public void setSelectedVOI( VOI kSelected, boolean bSelectAll, boolean bUnSelectVOI )
    {
        if ( kSelected == null )
        {
            return;
        }
        if ( bUnSelectVOI )
        {
            selectAllVOIs(false);
        }
        m_kCurrentVOIGroup = kSelected;
        if ( bUnSelectVOI || bSelectAll )
        {
            m_kCurrentVOIGroup.setAllActive(bSelectAll);
        }
        setButtonColor(toolbarBuilder.getVOIColorButton(), 
                m_kCurrentVOIGroup.getColor());
        m_fOpacity = m_kCurrentVOIGroup.getOpacity();
    }
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#setVOI_IDs(int, int)
     */
    public void setVOI_IDs(int ID, int UID) {}

    public void setVOITools( JMenu voiMenu, ViewMenuBuilder voiMenuBuilder )
    {
    	this.voiMenu = voiMenu;
    	this.voiMenuBuilder = voiMenuBuilder;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.VOIHandlerInterface#showColorDialog()
     */
    public void showColorDialog()
    {
        colorChooser = new ViewJColorChooser(null, "Pick surface color", 
                new OkColorListener(toolbarBuilder.getVOIColorButton()),
                null);
    }

    /**
     * Called from the VOIManager.
     * Displays the Curvature Graph for the input VOIBase.
     * @param kVOI VOIBase to graph.
     * @param m_iPlane the plane on which to show intensity info
     * @param showGraph
     * @param smooth
     */
    public void showCurvatureInfo( VOIBase kVOI, int m_iPlane, boolean showGraph, boolean smoothCurvature) {
        ViewJFrameGraph curvatureGraph;

        ModelImage kImage = getActiveImage();
        int[] unitsOfMeasure = kImage.getUnitsOfMeasure();
        float[] resolutions = kImage.getResolutions(0);

        double length = kVOI.getLengthPtToPt(resolutions);
        DecimalFormat dec = new DecimalFormat("0.##");
        
        
        String contourName = kVOI.getName();
        int sliceNum = kVOI.slice(m_iPlane);
        int curveNum = kVOI.getContourID();
        
        Vector<Vector3f> positions = new Vector<Vector3f>();
        Vector<Float> curvature = new Vector<Float>();

        double meanCurvature[] = new double[1];
        double stdDevCurvature[] = new double[1];
        double meanNegativeCurvature[] = new double[1];
        double negativeHysteresisFraction = 0.25;
        double positiveHysteresisFraction = 0.25;
        int numberOfIndentations[] = new int[1];
        int consecutiveNegativeNeeded = 2;
        double negativeCurvatureNeeded = 0.0;
        double totalLength[] = new double[1];
        int pts = kVOI.findPositionAndCurvature( positions, curvature, smoothCurvature, meanCurvature, stdDevCurvature,
                                                 meanNegativeCurvature, negativeHysteresisFraction, positiveHysteresisFraction,
                                                 numberOfIndentations, consecutiveNegativeNeeded, negativeCurvatureNeeded,
                                                 totalLength);

        
        float[] pos = new float[pts];
        float[] curv = new float[pts];
        int[][] xyCoords = new int[pts][2];

        float min = Float.MAX_VALUE;
        float max = Float.MIN_VALUE;
        float totalCurv = 0;
        for (int m = 0; m < pts; m++) {
            xyCoords[m][0] = (int)positions.get(m).X;
            xyCoords[m][1] = (int)positions.get(m).Y;
            pos[m] = positions.get(m).Z;
            curv[m] = curvature.get(m);

            totalCurv += curv[m];
            min = Math.min( curv[m], min );
            max = Math.max( curv[m], max );
        }

        if(showGraph) {
            if (kVOI.getGroup().getContourGraph() == null) {
                curvatureGraph = new ViewJFrameGraph(pos, curv, "VOI Curvature Graph", kVOI.getGroup(),
                        Unit.getUnitFromLegacyNum(unitsOfMeasure[0]).getAbbrev(),xyCoords);
                curvatureGraph.setLabelY("Curvature");
                curvatureGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                curvatureGraph.setVisible(true);
                kVOI.getGroup().setContourGraph(curvatureGraph);
                curvatureGraph.setVOI(kVOI.getGroup());
            } else {
                kVOI.getGroup().getContourGraph().setUnitsInLabel(Unit.getUnitFromLegacyNum(unitsOfMeasure[1]).getAbbrev());
                kVOI.getGroup().getContourGraph().replaceFunction(pos, curv, xyCoords, kVOI.getGroup(), 0);
            }
        }

        ViewUserInterface.getReference().setDataText(
                "Contour\tname\tslice\tnumber\tmin \tmax \ttotal \tmean \tstandard deviation\tlength " + "\n");
        ViewUserInterface.getReference().setDataText(
                "\t" + contourName + "\t" + sliceNum + "\t" + curveNum + "\t" + min + "\t" + max + "\t" + totalCurv + "\t" + 
                 meanCurvature[0] + "\t" + stdDevCurvature[0] + "\t" + dec.format(length) +"\n");
    }
    
    /**
     * Called from the VOIManager.
     * Displays the Intensity Graph for the input VOIBase.
     * @param kVOI VOIBase to graph.
     * @param m_iPlane the plane on which to show intensity info
     */
    public void showIntensityInfo( VOIBase kVOI, int m_iPlane, boolean showGraph) {
        ViewJFrameGraph lineGraph;

        ModelImage kImage = getActiveImage();
        int nDims = kImage.getNDims();
        int zDim = 1;
        if (nDims > 2) {
        	zDim = kImage.getExtents()[2];
        }
        float resolutions[][] = new float[zDim][];
        for (int z = 0; z < zDim; z++) {
        	resolutions[z] = kImage.getResolutions(z);
        }
        int[] unitsOfMeasure = kImage.getUnitsOfMeasure();

        double length = kVOI.getLengthPtToPt(resolutions);
        DecimalFormat dec = new DecimalFormat("0.##");
        
        
        String lineName = kVOI.getName();
        int sliceNum = kVOI.slice(m_iPlane);
        int curveNum = kVOI.getContourID();
        
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
                rgbPos[0][m] = positions.get(m).X;
                rgbPos[1][m] = positions.get(m).Y;
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

            if(showGraph) {
                if (kVOI.getGroup().getContourGraph() == null) {
                    ViewJFrameGraph contourGraph = new ViewJFrameGraph(rgbPos, rgbColors, "Intensity Graph", kVOI.getGroup(),
                    		Unit.getUnitFromLegacyNum(unitsOfMeasure[0]).getAbbrev());
    
                    contourGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                    contourGraph.setVisible(true);
                    kVOI.getGroup().setContourGraph(contourGraph);
                    contourGraph.setVOI(kVOI.getGroup());
                } else {
                    kVOI.getGroup().getContourGraph().setUnitsInLabel(Unit.getUnitFromLegacyNum(unitsOfMeasure[1]).getAbbrev());
                    kVOI.getGroup().getContourGraph().saveNewFunction(rgbPos, rgbColors, 0);
                }
            }

            ViewUserInterface.getReference().setDataText(
                    "Line\tname\tslice\tnumber\tmean \tstandard deviation\tlength " + "\n");
            ViewUserInterface.getReference()
            .setDataText(
                    "Red\t" + lineName + "\t" + sliceNum + "\t" + curveNum + "\t" + rgbMeanIntenR + "\t"
                    + rgbStdDevIntenR + "\t" + dec.format(length) + "\n");
            ViewUserInterface.getReference().setDataText(
                    "Green\t" + lineName + "\t" + sliceNum + "\t" + curveNum + "\t" + rgbMeanIntenG + "\t" + rgbStdDevIntenG
                    + "\t" + dec.format(length) +"\n");
            ViewUserInterface.getReference().setDataText(
                    "Blue\t" + lineName + "\t" + sliceNum + "\t" + curveNum + "\t" + rgbMeanIntenB + "\t" + rgbStdDevIntenB
                    + "\t" + dec.format(length) +"\n");
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

            if(showGraph) {
                if (kVOI.getGroup().getContourGraph() == null) {
                    lineGraph = new ViewJFrameGraph(pos, inten, "Line VOI Graph", kVOI.getGroup(),
                    		Unit.getUnitFromLegacyNum(unitsOfMeasure[0]).getAbbrev(),xyCoords);
                    lineGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                    lineGraph.setVisible(true);
                    kVOI.getGroup().setContourGraph(lineGraph);
                    lineGraph.setVOI(kVOI.getGroup());
                } else {
                    kVOI.getGroup().getContourGraph().setUnitsInLabel(Unit.getUnitFromLegacyNum(unitsOfMeasure[1]).getAbbrev());
                    kVOI.getGroup().getContourGraph().replaceFunction(pos, inten, xyCoords, kVOI.getGroup(), 0);
                }
            }

            ViewUserInterface.getReference().setDataText(
                    "Line\tname\tslice\tnumber\tmin \tmax \ttotal \tmean \tstandard deviation\tlength " + "\n");
            ViewUserInterface.getReference().setDataText(
            		"\t" + lineName + "\t" + sliceNum + "\t" + curveNum + "\t" + min + "\t" + max + "\t" + totalInten + "\t" + rgbMeanIntenR + "\t" + rgbStdDevIntenR + "\t" + dec.format(length) +"\n");
        }
    }
    
    /**
     * Called from VOIManager. Causes the parent frame to redraw the ModelImage and VOIs.
     */
    public void updateDisplay()
    {
    	if ( m_kParent != null )
    	{
    		m_kParent.setModified();
    	}
    }

    /**
     * Called from the VOIManager when the VOIBase that is currently being graphed has changed.
     * @param kVOI the modified VOIBase.
     */
    public void updateGraph( VOIBase kVOI, int m_iPlane)
    {
        if ( kVOI.getType() == VOI.LINE )
        {
            showIntensityInfo(kVOI, m_iPlane, true);
        }
        else if ( kVOI.getType() == VOI.POINT )
        {
            graphPointVOI( kVOI.getGroup(), (VOIPoint)kVOI, kVOI.getGroup().getCurves().indexOf(kVOI),
                    true);
        }
    }

    @Override
    public void vectorSelected(VOIVectorEvent selection) {
        //System.err.println( "VOIManagerInterface.vectorSelected" );
        if ( m_kVOIDialog != null )
        {
            m_kVOIDialog.updateVOIPanel( selection.getVOI(), getActiveImage() );
            m_kVOIDialog.updateTree();
        }        
    }

    private void addVOI( ModelImage kImage, VOIBase kNew, boolean bQuickLUT, boolean bUpdate, boolean isFinished )
    {       
        if ( kNew.getGroup() == null )
        {
            saveVOIs("addVOI");
            findCompatibleType(kImage, kNew, isFinished);
            if ( m_kCurrentVOIGroup == null )
            {
                short sID = (short)(kImage.getVOIs().getUniqueID());
                String kName = kNew.getClass().getName();
                int index = kName.lastIndexOf('.') + 1;
                kName = kName.substring(index);
                m_kCurrentVOIGroup = new VOI( sID,  kName + "_" + sID, kNew.getType(), presetHue );
                m_kCurrentVOIGroup.addVOIListener(this);
                m_kCurrentVOIGroup.setOpacity(1f);
                kImage.registerVOI( m_kCurrentVOIGroup );
            }   

            else if ( m_kCurrentVOIGroup.getName().indexOf( "_" ) == 0 )
            {
                String kName = kNew.getClass().getName();
                int index = kName.lastIndexOf('.') + 1;
                kName = kName.substring(index);
                m_kCurrentVOIGroup.setName( kName + m_kCurrentVOIGroup.getName() );
            }
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
        if ( !bQuickLUT )
        {
            setCurrentColor();
        }
        else
        {
            m_kCurrentVOIGroup.setColor( Color.yellow );
        }
        if ( bUpdate )
        {
            updateDisplay();
        }
    }


    /**
     * Increments the VOI uid for internal tracking, used during both creation and loading of a new VOI
     */
    private void advanceVOIUID() {
        m_kCurrentVOIGroup = null;
        voiUID++;
        if (presetHue >= 0.0) {
            Color color = Color.getHSBColor(presetHue, 1.0f, 1.0f);
            toolbarBuilder.getVOIColorButton().setVOIColor(color);
            toolbarBuilder.getVOIColorButton().setBackground(color);
        }
        else {
            toolbarBuilder.getVOIColorButton().setVOIColor(voiUID);
        }
        setButtonColor(toolbarBuilder.getVOIColorButton(), 
                toolbarBuilder.getVOIColorButton().getBackground() );
    }

    private void changeVOIOrder(boolean doContour, int direction) {
        if ((direction != VOI.FORWARD) && (direction != VOI.BACKWARD)
                && (direction != VOI.FRONT) && (direction != VOI.BACK))
        {
            return;
        }

        ViewVOIVector VOIs = getActiveImage().getVOIs();
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
    
    private boolean checkForActiveVOIs() {
        boolean foundActive = false;
        ViewVOIVector VOIs;
        int nVOI;

        VOIs = getActiveImage().getVOIs();
        nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                foundActive = true;
                break;
            }
        }

        return foundActive;
    }

    private void clearList( Vector<VOISaveState> list, int limit )
    {
    	int size = Math.min( limit, list.size() );
		for ( int i = 0; i < size; i++ )
		{
			VOISaveState kState = list.remove(0);
			kState.dispose();
			kState = null;
		}
		list.clear();
    }

    private int copy()
    {        
        // Get the reference to the Global copy list:
        Vector<VOIBase> copyList = ViewUserInterface.getReference().getCopyVOIs();
        Vector<String> copyNameList = ViewUserInterface.getReference().getCopyVOINames();
        copyList.clear();
        copyNameList.clear();
        VOIVector kVOIs = getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() && !copyList.contains(kCurrentVOI) )
                {
                    copyList.add(kCurrentVOI);
                    copyNameList.add(kCurrentGroup.getName());
                }
            }
        }
        return copyList.size();
    }

    private void createMask( ModelImage kActive, boolean bInside )
    {
        int iSize = kActive.getSize();
        if ( kActive.isColorImage() )
        {
            iSize /= 4;
        }
        kActive.createMask(iSize);
        boolean bMask = false;
        for (int i = 0; i < m_kVOIManagers.size(); i++) {
            bMask |= make3DVOI( false, kActive, kActive, kActive.getMask(), 
            		m_kVOIManagers.elementAt(i), i);
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
        m_kParent.updateData();
    }

    private void createMask( String command )
    {
        if (getActiveImage().getVOIs().size() < 1) {
            MipavUtil.displayWarning("Must have at least one VOI to perform quick mask");
            return;
        }
        if ( m_bGPURenderer )
        {
            createMask( getActiveImage(), command.equals(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand()) );
        }
        else
        {
            new JDialogMask(getActiveImage(), false, command.equals(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand()));
        }
    }

    private int cut()
    {
        int count = copy();
        if ( count > 0 )
        {            
            saveVOIs(CustomUIBuilder.PARAM_VOI_CUT.getActionCommand());
            deleteActiveVOI();
        }
        return count;
    }

    private void deleteActiveVOI()
    {
        Vector<VOIBase> deleteList = new Vector<VOIBase>();

        VOIVector kVOIs = getActiveImage().getVOIs();
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
            deleteVOI( deleteList.remove(0), false );
        }
    	updateDisplay();
    }

    private void deleteAllVOI()
    {
        if ( m_kImageA != null ) { m_kImageA.unregisterAllVOIs(); }
        if ( m_kImageB != null ) { m_kImageB.unregisterAllVOIs(); }
    }
    
    private void deleteVOIActivePt()
    {
        Vector<VOIBase> activeList = new Vector<VOIBase>();

        VOIVector kVOIs = getActiveImage().getVOIs();
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

            int iPos = kCurrentVOI.getSelectedPoint();
            if ( iPos >= 0 )
            {
            	kCurrentVOI.delete( kCurrentVOI.getSelectedPoint() );      
            	if ( kCurrentVOI.size() == 0 )
            	{
            		deleteList.add( kCurrentVOI );
            	}            
            	else if ( (kCurrentVOI.getType() == VOI.LINE) && (kCurrentVOI.size() <= 1) )
            	{
            		deleteList.add( kCurrentVOI );
            	}            
            	else if ( (kCurrentVOI.getType() == VOI.ANNOTATION) && (kCurrentVOI.size() <= 2) )
            	{
            		deleteList.add( kCurrentVOI );
            	}            
            	else if ( (kCurrentVOI.getType() == VOI.PROTRACTOR) && (kCurrentVOI.size() <= 2) )
            	{
            		deleteList.add( kCurrentVOI );
            	}
            }
        }

        while ( deleteList.size() > 0 )
        {
            deleteVOI( deleteList.remove(0), false );
        }
        updateDisplay();
    }

    private void evolveBoundary2D(String command)
    {
    	m_bDefaultImage = true;
    	m_kTempImage = m_kVOIManagers.get(m_iActive).getLocalImage();
        if ( m_kTempImage != getActiveImage() )
        {
        	m_bDefaultImage = false;
        	m_kTempImage = (ModelImage)m_kTempImage.clone();
        }
        saveVOIs(command);
    	if (command.equals("Snake") ) {
    		JDialogSnake kEvolve = new JDialogSnake(m_kParent.getFrame(), m_kTempImage);
    		kEvolve.setVOIManager(this);
    	} 
    	else if (command.equals("AGVF")) {
    		JDialogAGVF kEvolve = new JDialogAGVF(m_kParent.getFrame(), m_kTempImage);
    		kEvolve.setVOIManager(this);
    	} 
    	else if (command.equals("GVF")) {
    		JDialogGVF kEvolve = new JDialogGVF(m_kParent.getFrame(), m_kTempImage);
    		kEvolve.setVOIManager(this);
    	}
        else if (command.equals("BSnake")) {
            JDialogBSnake kEvolve = new JDialogBSnake(m_kParent.getFrame(), m_kTempImage);
            kEvolve.setVOIManager(this);
        }
        else if (command.equals("EvolveConstant")) {
            JDialogEvolveBoundaryManual kEvolve = new JDialogEvolveBoundaryManual(m_kParent.getFrame(), m_kTempImage);
            kEvolve.setVOIManager(this);
        }
        else if (command.equals("DCSearch")) {
        	JDialogDualContourSearch kEvolve = new JDialogDualContourSearch(m_kParent.getFrame(), m_kTempImage);
            kEvolve.setVOIManager(this);	
        }
    }


    private void findCompatibleType( ModelImage kImage, VOIBase kNew, boolean isFinished)
    {
        if ( kNew.isQuickLUT() )
        {
            return;
        }
        if ( m_kCurrentVOIGroup != null && (m_kCurrentVOIGroup.getCurveType() == kNew.getType() || !isFinished))
        {
            return;
        }
        if ( m_kCurrentVOIGroup != null && m_kCurrentVOIGroup.isEmpty() )
        {
            short sID = (short)(kImage.getVOIs().getUniqueID());
            getActiveImage().unregisterVOI(m_kCurrentVOIGroup);
            String kName = kNew.getClass().getName();
            int index = kName.lastIndexOf('.') + 1;
            kName = kName.substring(index);
            m_kCurrentVOIGroup.setName( kName + "_" + sID );
            m_kCurrentVOIGroup.setCurveType(kNew.getType());
            return;
        }
        VOIVector kVOIs = kImage.getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            if ( kVOIs.get(i).isActive() && matches(kVOIs.get(i).getCurveType(), kNew.getType(), isFinished) )
            {
                m_kCurrentVOIGroup = kVOIs.get(i);
                if (presetHue >= 0.0f) {
                	m_kCurrentVOIGroup.setColor(presetHue);
                }
                setSelectedVOI(m_kCurrentVOIGroup, false, true);
                return;
            }
        }
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            if ( matches(kVOIs.get(i).getCurveType(), kNew.getType(), isFinished) )
            {
                m_kCurrentVOIGroup = kVOIs.get(i);
                if (presetHue >= 0.0f) {
                	m_kCurrentVOIGroup.setColor(presetHue);
                }
                setSelectedVOI(m_kCurrentVOIGroup, false, true);
                return;
            }
        }
        if ( m_kCurrentVOIGroup != null )
        {
            newVOI(false, kNew.isSplit());
        }
        m_kCurrentVOIGroup = null;
    }

    private int getActiveVOICount()
    {
        ModelImage kActive = getActiveImage();
        ViewVOIVector VOIs = kActive.getVOIs();
        int nActive = 0;
        for (int i = 0; i < VOIs.size(); i++) {
            if (VOIs.VOIAt(i).isActive()) {
                nActive++;
            }
        }
        return nActive;
    }


    private JFrame getFrame() {
        return m_kParent.getFrame();
    }

    private int getSlice() {
        return (int)m_kParent.getCenterPt().Z;
    }

    protected VOISaveState getVOIState( )
    {
        //System.out.println(Runtime.getRuntime().maxMemory() / 1048576);
        //System.out.println(Runtime.getRuntime().totalMemory() / 1048576);
        //System.out.println(Runtime.getRuntime().freeMemory() / 1048576);
        //if ( Runtime.getRuntime().totalMemory() >= (Runtime.getRuntime().maxMemory() / 10) )
        //{
        //	System.err.println( "Clearing undo/redo list to free up memory" );
        //	clearList( m_kUndoList );
        //	clearList( m_kRedoList );        	
        //}
    	try { 
    		VOISaveState kVOIState = new VOISaveState();

//    		VOIVector vois = m_kImageA.getVOIs();
//			System.err.println( "getVOIState" );
//    		for (int i = 0; i < vois.size(); i++) {
//    			System.err.println( i + "   " + vois.elementAt(i).getName() );
//    		}
    		
    		
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
    		if ( m_kParent != null )
    		{
    			kVOIState.currentCenter.copy( m_kParent.getCenterPt() );
    		}
            return kVOIState;
    	} catch ( OutOfMemoryError e )
    	{
    		//System.err.println( "Too much memory, clearing undo/redo lists" );
        	clearList( m_kUndoList, 10 );
        	clearList( m_kRedoList, 10 );
        	System.gc();
        	try { 
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
        		kVOIState.currentCenter.copy( m_kParent.getCenterPt() );
                return kVOIState;
        	} catch ( OutOfMemoryError e2 )
        	{
        		MipavUtil.displayError( "Memory error. All VOIs will be saved to file then deleted." );
                saveAllVOIs();
                deleteAllVOI();
        	}
    	}
    	return null;
    }


    private void graphPointVOI(VOI v, VOIPoint voiPt, int j,
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

        ModelImage kImage = getActiveImage();
        if(useFrameRefTime && !(kImage.getFileInfo(0) instanceof FileInfoDicom)) {
        	useFrameRefTime = false; //frame ref time is only defined for dicom images
        }

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
                            "Intensity Graph", v, 
                            Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(0)).getAbbrev());

                    contourGraph.setDefaultDirectory(ViewUserInterface
                            .getReference().getDefaultDirectory());
                    contourGraph.setVisible(false);
                    v.setContourGraph(contourGraph);
                } else {
                    v.getContourGraph().setUnitsInLabel(
                    		Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(0)).getAbbrev());
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
                            Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(0)).getAbbrev(),null);

                    contourGraph.setDefaultDirectory(ViewUserInterface
                            .getReference().getDefaultDirectory());
                    contourGraph.setVisible(false);
                    v.setContourGraph(contourGraph);
                } else if (useFrameRefTime) {
                    v.getContourGraph().setUnitsInLabel(
                    		Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(0)).getAbbrev());
                    v.getContourGraph().update(ptPosition,
                            ptIntensity, j);
                }  else {
                    v.getContourGraph().setUnitsInLabel(Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                            .getUnitsOfMeasure(0)).getAbbrev());
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
                            Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(3)).getAbbrev(),null);
                    contourGraph.setDefaultDirectory(ViewUserInterface
                            .getReference().getDefaultDirectory());
                    contourGraph.setVisible(false);
                    v.setContourGraph(contourGraph);
                } else if (useFrameRefTime) {
                    v.getContourGraph().setUnitsInLabel(
                    		Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                                    .getUnitsOfMeasure(3)).getAbbrev());
                    v.getContourGraph().replaceFunction(ptPosition,
                            ptIntensity, null, v, j);
                } else {
                    v.getContourGraph().setUnitsInLabel(Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                            .getUnitsOfMeasure(3)).getAbbrev());
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

    private void graphVOI()
    {
        VOIVector kVOIs = getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() )
                {
                    showIntensityInfo( kCurrentVOI, kCurrentVOI.getPlane(), true );
                }
            }
        }
    }
    
    private void graphVOICurvature(boolean smooth)
    {
        VOIVector kVOIs = getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for ( int j = 0; j < kCurrentGroup.getCurves().size(); j++ )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() )
                {
                    showCurvatureInfo( kCurrentVOI, kCurrentVOI.getPlane(), true, smooth );
                }
            }
        }
    }
    

    private void indentationVOIs2D() {
        boolean displayIndentations = true;
        Vector<VOIBase>curves = new Vector<VOIBase>();
        VOIVector kVOIs = getActiveImage().getVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            for (int j = kCurrentGroup.getCurves().size()-1; j >= 0; j-- )
            {
                VOIBase kCurrentVOI = kCurrentGroup.getCurves().get(j);
                if ( kCurrentVOI.isActive() )
                {
                    int sliceNum = kCurrentVOI.slice(kCurrentVOI.getPlane());
                    kCurrentVOI.findVOIIndentations2D( getActiveImage(), sliceNum, curves, displayIndentations);
                    kCurrentGroup.removeCurve(kCurrentVOI);
                    for (int k = 0; k < curves.size(); k++) {
                        kCurrentGroup.importCurve(curves.get(k));
                    }
            
                }
            }
        } 
        updateDisplay();
    }
    
    
    
    


    
    private void interpolateVOIs()
    {
        ModelImage kImage = m_kVOIManagers.get(m_iActive).getLocalImage();
        if ( kImage == getActiveImage() )
        {
            new JDialogVOIShapeInterpolation(getActiveImage());
        }
        else
        {
        	kImage = (ModelImage)kImage.clone();
            kImage.calcMinMax();            
            new JDialogVOIShapeInterpolation(kImage, false);                  
            int[] axisA = getActiveImage().getAxisOrientation();
            int[] axisB = kImage.getAxisOrientation();
            int[] axisOrder = { 0, 1, 2, 3 };
            boolean[] axisFlip = { false, false, false, false };
            if ( MipavCoordinateSystems.matchOrientation( axisA, axisB, axisOrder, axisFlip ) )
            {
                AlgorithmRotate rotateAlgo = new AlgorithmRotate( kImage, axisOrder, axisFlip );
                rotateAlgo.setRunningInSeparateThread(false);
                rotateAlgo.run();
                kImage = rotateAlgo.returnImage();
                getActiveImage().unregisterAllVOIs();            
                getActiveImage().setVOIs( kImage.getVOIs() );
                
                if ( getActiveImage() != null && getActiveImage().getVOIs() != null )
                {
                    getActiveImage().getVOIs().addVectorListener(this);
                    VOIVector kVOIs = getActiveImage().getVOIs();
                    for ( int i = 0; i < kVOIs.size(); i++ )
                    {
                        kVOIs.elementAt(i).addVOIListener(this);
                    }
                }
            }
        }
        if ( imageStatList != null )
        {
            imageStatList.refreshVOIList(getActiveImage().getVOIs());
        }
        if ( m_kVOIDialog != null )
        {
            ModelImage kActive = getActiveImage();
            ViewVOIVector VOIs = kActive.getVOIs();
            for (int i = 0; i < VOIs.size(); i++) {
                if (VOIs.VOIAt(i).isActive()) {
                    m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(i), kActive );
                    m_kVOIDialog.updateTree();
                    return;
                }
            }
            if (VOIs.size() > 0) {
                m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(0), kActive );
                m_kVOIDialog.updateTree();
            }
        }
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
                kCommand.equals(CustomUIBuilder.PARAM_VOI_BSPLINE.getActionCommand()) || 
                kCommand.equals(CustomUIBuilder.PARAM_VOI_LEVELSET.getActionCommand()) ||
                kCommand.equals(CustomUIBuilder.PARAM_LUT_QUICK.getActionCommand() ) 
                || kCommand.equals("quickLUT") ) {
            return true;
        } 
        return false;
    }
    
    /**
     * This method loads all VOIs to the active image from the default VOI directory for that image.
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    private void loadAllVOIs(boolean quietMode) {
        ModelImage img = getActiveImage();

        String fileDir = img.getFileInfo(0).getFileDirectory();
        String imageName;

        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        if (img.isDicomImage()) {
            imageName = img.getFileInfo(0).getFileName();

            final int index = imageName.lastIndexOf(".");

            if (index > 0) {
                imageName = imageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = imageName.length();

            for (int i = imageName.length() - 1; i >= 0; i--) {
                final char myChar = imageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                imageName = new String("DICOM");
            } else {
                imageName = imageName.substring(0, newIndex);
            }
        } else {
            imageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = imageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        String voiDir = new String(fileDir + File.separator + "defaultVOIs_" + imageName + File.separator);

        loadAllVOIsFrom(voiDir, quietMode);

    } // end loadAllVOIs()

    /**
     * This method loads all VOIs to the active image from a given directory.
     * @param voiDir the directory to load voi's from
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    private void loadAllVOIsFrom(final String voiDir, boolean quietMode) {
    	loadAllVOIsFrom(voiDir, quietMode, null, true);
    }
    

    /**
     * This method loads all VOIs to the active image from a given directory.
     * @param voiDir the directory to load voi's from
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    protected void loadAllVOIsFrom(final String voiDir, boolean quietMode, VOIVector resultVector, boolean registerVOIs) {

        int i, j;
        VOI[] VOIs;
        FileVOI fileVOI;
        ModelImage currentImage = getActiveImage();

        try {

            // if voiDir does not exist, then return
            // if voiDir exists, then get list of voi's from directory (*.voi)
            final File voiFileDir = new File(voiDir);
            final Vector<String> filenames = new Vector<String>();
            final Vector<Boolean> isLabel = new Vector<Boolean>();

            if (voiFileDir.exists() && voiFileDir.isDirectory()) {

                // get list of files
                final File[] files = voiFileDir.listFiles();

                for (final File element : files) {

                    if (element.getName().endsWith(".voi") || element.getName().endsWith(".xml")) {
                        filenames.add(element.getName());
                        isLabel.add(false);
                    } else if (element.getName().endsWith(".lbl")) {
                        filenames.add(element.getName());
                        isLabel.add(true);
                    }
                }
            } else { // voiFileDir either doesn't exist, or isn't a directory

                if ( !quietMode) {
                    MipavUtil.displayError("No VOIs are found in directory: " + voiDir);
                }

                return;
            }

            // open each voi array, then register voi array to this image
            for (i = 0; i < filenames.size(); i++) {

                fileVOI = new FileVOI( (filenames.elementAt(i)), voiDir, currentImage);

                VOIs = fileVOI.readVOI(isLabel.get(i));

                if(m_kCurrentVOIGroup != null) {
                    advanceVOIUID();
                }
                for (j = 0; j < VOIs.length; j++) {
                    if(VOIs[j].getColor() == null) {
                        VOIs[j].setColor(toolbarBuilder.getVOIColorButton().getBackground());
                    }
                    if ( registerVOIs )
                    {   	
                    	currentImage.registerVOI(VOIs[j]);
                    }
                    VOIs[j].addVOIListener(this);
                    advanceVOIUID();
                    
                    if ( resultVector != null )
                    {
                    	resultVector.add(VOIs[j]);
                    }
                }
            }
            
            // when everything's done, notify the image listeners
            if ( registerVOIs )
            {   	
            	currentImage.notifyImageDisplayListeners();
            }

        } catch (final Exception error) {

            if ( !quietMode) {
                MipavUtil.displayError("Error loading all VOIs from " + voiDir + ": " + error);
            }
        }

    } // end loadAllVOIsFrom()
    
    /**
     * This method loads all ImageJ VOIs to the active image from a given directory.
     * @param voiDir the directory to load voi's from
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    protected void loadAllImageJVOIsFrom(final String voiDir, boolean quietMode, VOIVector resultVector, boolean registerVOIs) {

        int i;
        ModelImage currentImage = getActiveImage();

        try {

            // if voiDir does not exist, then return
            // if voiDir exists, then get list of voi's from directory (*.roi)
            final File voiFileDir = new File(voiDir);
            final Vector<String> filenames = new Vector<String>();

            if (voiFileDir.exists() && voiFileDir.isDirectory()) {

                // get list of files
                final File[] files = voiFileDir.listFiles();
                for (final File element : files) {

                    if (element.getName().endsWith(".roi")) {
                        filenames.add(element.getName());
                    }
                }
            } else { // voiFileDir either doesn't exist, or isn't a directory

                if ( !quietMode) {
                    MipavUtil.displayError("No VOIs are found in directory: " + voiDir);
                }

                return;
            }

            // open each voi array, then register voi array to this image
            for (i = 0; i < filenames.size(); i++) {
                readImageJ(voiDir+filenames.elementAt(i), filenames.elementAt(i), currentImage);
            }
            
            // when everything's done, notify the image listeners
            if ( registerVOIs )
            {   	
            	currentImage.notifyImageDisplayListeners();
            }

        } catch (final Exception error) {

            if ( !quietMode) {
                MipavUtil.displayError("Error loading all ImageJ VOIs from " + voiDir + ": " + error);
            }
        }

    } // end loadAllVOIsFrom()

    private boolean make3DVOI( boolean bIntersection, ModelImage kSrc, ModelImage kVolume, BitSet kMask, 
    		VOIManager kManager, int iValue )
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
                    for ( int m = 0; m < m_kVOIManagers.size(); m++ )
                    {
                    	int iPlane = m_kVOIManagers.elementAt(m).getPlane();
                        if ( iPlane == (iPlane & kCurrentVOI.getPlane()) )
                        {
                            if ( kManager == m_kVOIManagers.elementAt(m) )
                            {
                            	bCreated = true;
                                kCurrentVOI.fillVolume( kVolume, kMask, bIntersection, iValue ); 
                            }
                        }
                    }              	
                }
            }
        }
        updateDisplay();        
        return bCreated;
    }

    private boolean matches( int iType1, int iType2, boolean isFinished )
    {
        if ( iType1 == iType2 )
        {
            return true;
        }
        if ( isFinished )
        {
            return false;
        }
        if ( (iType1 == VOI.CONTOUR || iType1 == VOI.POLYLINE) &&
                (iType2 == VOI.CONTOUR || iType2 == VOI.POLYLINE) )
        {
            return true;
        }
        return false;   
    }


    /**
     * This method opens an existing VOI.
     * @param quietMode if true indicates that warnings should not be displayed.
     * @param doLabels DOCUMENT ME!
     * @return whether a VOI was successfully opened (ie - the dialog wasn't cancelled)
     */
    private boolean openVOI(boolean quietMode, final boolean doLabels) {
        ViewOpenVOIUI openVOI = null;

        try {
            openVOI = new ViewOpenVOIUI();
            VOI[] newVOIs = openVOI.open(getActiveImage(), doLabels);
            if ( newVOIs == null) {
                return false;
            }
            if(m_kCurrentVOIGroup != null) {
                advanceVOIUID();
            }
            for ( int i = 0; i < newVOIs.length; i++ )
            {
                if(newVOIs[i].getColor() == null) {
                    newVOIs[i].setColor(toolbarBuilder.getVOIColorButton().getBackground());
                }
                newVOIs[i].getGeometricCenter();
                newVOIs[i].addVOIListener(this);
                advanceVOIUID();
            }
        } catch (final OutOfMemoryError error) {

            if ( !quietMode) {
                MipavUtil.displayError("Out of memory: VOIManagerInterface.openVOI");
            }

            return false;
        }

        return true;
    }
    
    private boolean openOtherOrientationVOI(boolean quietMode) {
    	ViewOpenVOIUI openVOI = null;

        try {
            openVOI = new ViewOpenVOIUI();
            VOI[] newVOIs = openVOI.openOtherOrientation(getActiveImage());
            if ( newVOIs == null) {
                return false;
            }
            if(m_kCurrentVOIGroup != null) {
                advanceVOIUID();
            }
            for ( int i = 0; i < newVOIs.length; i++ )
            {
                if(newVOIs[i].getColor() == null) {
                    newVOIs[i].setColor(toolbarBuilder.getVOIColorButton().getBackground());
                }
                newVOIs[i].getGeometricCenter();
                newVOIs[i].addVOIListener(this);
                advanceVOIUID();
            }
        } catch (final OutOfMemoryError error) {

            if ( !quietMode) {
                MipavUtil.displayError("Out of memory: VOIManagerInterface.openOtherOrientationVOI");
            }

            return false;
        }
    	return true;
    }
    
    private boolean openPaint(boolean quietMode) {
        ViewOpenPaintUI openPaint = null;
        
        try {
        	openPaint = new ViewOpenPaintUI();
            BitSet paintBitmap = openPaint.open(getActiveImage());
        	if (paintBitmap == null) {
        		return false;
        	}
        }
        catch (final OutOfMemoryError error) {

            if ( !quietMode) {
                MipavUtil.displayError("Out of memory: VOIManagerInterface.openPaint");
            }

            return false;
        }

        return true;
    }

    private void paintToVOI()
    {
        ModelImage kImage = m_kVOIManagers.get(m_iActive).getLocalImage();
        if ( kImage == getActiveImage() )
        {
            int xDim = kImage.getExtents().length > 0 ? kImage.getExtents()[0] : 1;
            int yDim = kImage.getExtents().length > 1 ? kImage.getExtents()[1] : 1;
            int zDim = kImage.getExtents().length > 2 ? kImage.getExtents()[2] : 1;

            short voiID = (short) kImage.getVOIs().size();

            final AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(kImage,
            		kImage.getMask(), xDim, yDim, zDim, voiID);

            algoPaintToVOI.setRunningInSeparateThread(false);
            algoPaintToVOI.run();

            ScriptRecorder.getReference().addLine(new ActionPaintToVOI(kImage));
            ProvenanceRecorder.getReference().addLine(new ActionMaskToPaint(kImage));
        }
        else
        {    
        	kImage = (ModelImage)kImage.clone();
            int xDim = kImage.getExtents().length > 0 ? kImage.getExtents()[0] : 1;
            int yDim = kImage.getExtents().length > 1 ? kImage.getExtents()[1] : 1;
            int zDim = kImage.getExtents().length > 2 ? kImage.getExtents()[2] : 1;

            short voiID = (short) kImage.getVOIs().size();

            final AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(kImage,
            		kImage.getMask(), xDim, yDim, zDim, voiID);

            algoPaintToVOI.setRunningInSeparateThread(false);
            algoPaintToVOI.run();

            ScriptRecorder.getReference().addLine(new ActionPaintToVOI(kImage));
            ProvenanceRecorder.getReference().addLine(new ActionMaskToPaint(kImage));
            
            int[] axisA = getActiveImage().getAxisOrientation();
            int[] axisB = kImage.getAxisOrientation();
            int[] axisOrder = { 0, 1, 2, 3 };
            boolean[] axisFlip = { false, false, false, false };
            if ( MipavCoordinateSystems.matchOrientation( axisA, axisB, axisOrder, axisFlip ) )
            {
                AlgorithmRotate rotateAlgo = new AlgorithmRotate( kImage, axisOrder, axisFlip );
                rotateAlgo.setRunningInSeparateThread(false);
                rotateAlgo.run();
                kImage = rotateAlgo.returnImage();
                getActiveImage().unregisterAllVOIs();            
                getActiveImage().setVOIs( kImage.getVOIs() );
                

                if ( getActiveImage() != null && getActiveImage().getVOIs() != null )
                {
                    getActiveImage().getVOIs().addVectorListener(this);
                    VOIVector kVOIs = getActiveImage().getVOIs();
                    for ( int i = 0; i < kVOIs.size(); i++ )
                    {
                        kVOIs.elementAt(i).addVOIListener(this);
                    }
                }
            }
        }

        if ( imageStatList != null )
        {
            imageStatList.refreshVOIList(getActiveImage().getVOIs());
        }
        if ( m_kVOIDialog != null )
        {
            ModelImage kActive = getActiveImage();
            ViewVOIVector VOIs = kActive.getVOIs();
            for (int i = 0; i < VOIs.size(); i++) {
                if (VOIs.VOIAt(i).isActive()) {
                    m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(i), kActive );
                    m_kVOIDialog.updateTree();
                    return;
                }
            }
            if (VOIs.size() > 0) {
                m_kVOIDialog.updateVOIPanel( VOIs.VOIAt(0), kActive );
                m_kVOIDialog.updateTree();
            }
        }
        updateDisplay();
    }

    private void paste()
    {
    	//PrintStream out = System.out;
        // Get the Global copy list:
        Vector<VOIBase> copyList = ViewUserInterface.getReference().getCopyVOIs();
        // Return if no contours to paste:
        if ( copyList.size() == 0 )
        {
            return;
        }
        saveVOIs(CustomUIBuilder.PARAM_VOI_PASTE.getActionCommand());
        // If the copy list is from another image:
        if ( copyList.elementAt(0).getGroup() != null && !copyList.elementAt(0).getGroup().hasListener(this) )
        {
        	//out.println("broken");
            pasteFromViewUserInterface();
            return;
        }
        // The copy list is from this image/manager: 
        for ( int i = 0; i < copyList.size(); i++ )
        {
            VOIBase kCurrentVOI = copyList.get(i); 
            VOIManager kManager = m_kVOIManagers.elementAt(0);
            for ( int j = 0; j < m_kVOIManagers.size(); j++ )
            {
            	int iPlane = m_kVOIManagers.elementAt(j).getPlane();
                if ( iPlane == (iPlane & kCurrentVOI.getPlane()) )
                {
                    kManager = m_kVOIManagers.elementAt(j);
                    break;
                }
            }

            kManager.pasteVOI( kCurrentVOI );  
        }
    }

    private void levelSet3D()
    {
        // Get the Global copy list:
        Vector<VOIBase> copyList = ViewUserInterface.getReference().getCopyVOIs();
        for (int i = 0; i < copyList.size(); i++ )
        {
            VOIBase kCurrentVOI = copyList.get(i);
            
            for (int j = 0; j < m_kVOIManagers.size(); j++ )
            {
            	int iPlane = m_kVOIManagers.elementAt(j).getPlane();
                if ( iPlane != (iPlane & kCurrentVOI.getPlane()) )
                {
                    VOIManager kManager = m_kVOIManagers.elementAt(j);

                    for ( int pt = 0; pt < kCurrentVOI.size(); pt++ )
                    {
                    	kManager.doLevelset(kCurrentVOI.elementAt(pt));
                    }
                }
            }
        }
    	
    }
    
    
    private void pasteAll()
    {
        int i;
        int j;
        int k;
        int iPlane = 0;
        short id;
        float xCurve[];
        float yCurve[];
        float zCurve[];
        VOIManager xManager = null;
        VOIManager yManager = null;
        VOIManager zManager = null;
        int xIndex;
        int yIndex;
        int zIndex;
        // Get the Global copy list:
        Vector<VOIBase> copyList = ViewUserInterface.getReference().getCopyVOIs();
        for (i = 0; i < copyList.size(); i++ )
        {
            VOIBase kCurrentVOI = copyList.get(i);
            VOIManager kManager = m_kVOIManagers.elementAt(0);
            
            for (j = 0; j < m_kVOIManagers.size(); j++ )
            {
            	iPlane = m_kVOIManagers.elementAt(j).getPlane();
            	//System.err.println( iPlane + " " + kCurrentVOI.getPlane() + " " + (iPlane & kCurrentVOI.getPlane()));
                if ( iPlane == (iPlane & kCurrentVOI.getPlane()) )
                {
                    kManager = m_kVOIManagers.elementAt(j);
                    break;
                }
            }
            kManager.pasteAllVOI( kCurrentVOI ); 
            /* In ViewJComponentTriImage if a rect3D is drawn in one window, then the same rect3d appears in the
             * other 2 windows.
             */
            if ((m_kVOIManagers.size() == 3) &&
                (m_kVOIManagers.elementAt(0).getComponent().toString().contains("ViewJComponentTriImage")) &&
                (m_kVOIManagers.elementAt(1).getComponent().toString().contains("ViewJComponentTriImage")) &&
                (m_kVOIManagers.elementAt(2).getComponent().toString().contains("ViewJComponentTriImage"))) {
                VOI voiXY;
                VOI voiZY;
                VOI voiXZ;
                float x[] = new float[2];
                float y[] = new float[2];
                float z[] = new float[2];
                for (k = 0; k < 3; k++) {
                    if (m_kVOIManagers.elementAt(k).getPlane() == 1) { // XPLANE
                        xManager = m_kVOIManagers.elementAt(k);    
                    }
                    else if (m_kVOIManagers.elementAt(k).getPlane() == 2) { // YPLANE
                        yManager = m_kVOIManagers.elementAt(k);
                    }
                    else {
                        zManager = m_kVOIManagers.elementAt(k);
                    }
                }
                kCurrentVOI.getBounds(x, y, z);
                if (iPlane == 1) { // XPLANE
                    x[0] = 0;
                    x[1] = xManager.getLocalImage().getExtents()[2] - 1;
                    if (yManager.getImage().getVOIs() != null) {
                        id = (short)yManager.getImage().getVOIs().size();
                    }
                    else {
                        id = (short)0;
                    }
                    voiXZ = new VOI(id, "VOIXZ", VOI.CONTOUR, 0.0f);
                    voiXZ.setColor(Color.red);
                    for (yIndex = Math.round(y[0]); yIndex <= Math.round(y[1]); yIndex++) {
                        xCurve = new float[4];
                        zCurve = new float[4];
                        yCurve = new float[4];
                        xCurve[0] = x[0];
                        zCurve[0] = z[0];
                        yCurve[0] = yIndex;
                        xCurve[1] = x[1];
                        zCurve[1] = z[0];
                        yCurve[1] = yIndex;
                        xCurve[2] = x[1];
                        zCurve[2] = z[1];
                        yCurve[2] = yIndex;
                        xCurve[3] = x[0];
                        zCurve[3] = z[1];
                        yCurve[3] = yIndex;
                        voiXZ.importCurve(xCurve, yCurve, zCurve);
                    }
                    yManager.getImage().registerVOI(voiXZ);
                    if (zManager.getImage().getVOIs() != null) {
                        id = (short)zManager.getImage().getVOIs().size();
                    }
                    else {
                        id = (short)0;
                    }
                    voiXY = new VOI(id, "VOIXY", VOI.CONTOUR, 0.0f);
                    voiXY.setColor(Color.red);
                    for (zIndex = Math.round(z[0]); zIndex <= Math.round(z[1]); zIndex++) {
                        xCurve = new float[4];
                        yCurve = new float[4];
                        zCurve = new float[4];
                        xCurve[0] = x[0];
                        yCurve[0] = y[0];
                        zCurve[0] = zIndex;
                        xCurve[1] = x[1];
                        yCurve[1] = y[0];
                        zCurve[1] = zIndex;
                        xCurve[2] = x[1];
                        yCurve[2] = y[1];
                        zCurve[2] = zIndex;
                        xCurve[3] = x[0];
                        yCurve[3] = y[1];
                        zCurve[3] = zIndex;
                        voiXY.importCurve(xCurve, yCurve, zCurve);
                    }
                    zManager.getImage().registerVOI(voiXY);
                }
                else if (iPlane == 2) { // YPLANE
                    y[0] = 0;
                    y[1] = yManager.getLocalImage().getExtents()[2] - 1;
                    if (xManager.getImage().getVOIs() != null) {
                        id = (short)xManager.getImage().getVOIs().size();
                    }
                    else {
                        id = (short)0;
                    }
                    voiZY = new VOI(id, "voiZY", VOI.CONTOUR, 0.0f);
                    voiZY.setColor(Color.red);
                    for (xIndex = Math.round(x[0]); xIndex <= Math.round(x[1]); xIndex++) {
                        zCurve = new float[4];
                        yCurve = new float[4];
                        xCurve = new float[4];
                        zCurve[0] = z[0];
                        yCurve[0] = y[0];
                        xCurve[0] = xIndex;
                        zCurve[1] = z[1];
                        yCurve[1] = y[0];
                        xCurve[1] = xIndex;
                        zCurve[2] = z[1];
                        yCurve[2] = y[1];
                        xCurve[2] = xIndex;
                        zCurve[3] = z[0];
                        yCurve[3] = y[1];
                        xCurve[3] = xIndex;
                        voiZY.importCurve(xCurve, yCurve, zCurve);
                    }
                    xManager.getImage().registerVOI(voiZY);
                    if (zManager.getImage().getVOIs() != null) {
                        id = (short)zManager.getImage().getVOIs().size();
                    }
                    else {
                        id = (short)0;
                    }
                    voiXY = new VOI(id, "VOIXY", VOI.CONTOUR, 0.0f);
                    voiXY.setColor(Color.red);
                    for (zIndex = Math.round(z[0]); zIndex <= Math.round(z[1]); zIndex++) {
                        xCurve = new float[4];
                        yCurve = new float[4];
                        zCurve = new float[4];
                        xCurve[0] = x[0];
                        yCurve[0] = y[0];
                        zCurve[0] = zIndex;
                        xCurve[1] = x[1];
                        yCurve[1] = y[0];
                        zCurve[1] = zIndex;
                        xCurve[2] = x[1];
                        yCurve[2] = y[1];
                        zCurve[2] = zIndex;
                        xCurve[3] = x[0];
                        yCurve[3] = y[1];
                        zCurve[3] = zIndex;
                        voiXY.importCurve(xCurve, yCurve, zCurve);
                    }
                    zManager.getImage().registerVOI(voiXY);
                }
                else if (iPlane == 4) { // ZPLANE
                    z[0] = 0;
                    z[1] = zManager.getLocalImage().getExtents()[2]-1;
                    if (xManager.getImage().getVOIs() != null) {
                        id = (short)xManager.getImage().getVOIs().size();
                    }
                    else {
                        id = (short)0;
                    }
                    voiZY = new VOI(id, "voiZY", VOI.CONTOUR, 0.0f);
                    voiZY.setColor(Color.red);
                    for (xIndex = Math.round(x[0]); xIndex <= Math.round(x[1]); xIndex++) {
                        zCurve = new float[4];
                        yCurve = new float[4];
                        xCurve = new float[4];
                        zCurve[0] = z[0];
                        yCurve[0] = y[0];
                        xCurve[0] = xIndex;
                        zCurve[1] = z[1];
                        yCurve[1] = y[0];
                        xCurve[1] = xIndex;
                        zCurve[2] = z[1];
                        yCurve[2] = y[1];
                        xCurve[2] = xIndex;
                        zCurve[3] = z[0];
                        yCurve[3] = y[1];
                        xCurve[3] = xIndex;
                        voiZY.importCurve(xCurve, yCurve, zCurve);
                    }
                    xManager.getImage().registerVOI(voiZY);
                    if (yManager.getImage().getVOIs() != null) {
                        id = (short)yManager.getImage().getVOIs().size();
                    }
                    else {
                        id = (short)0;
                    }
                    voiXZ = new VOI(id, "VOIXZ", VOI.CONTOUR, 0.0f);
                    voiXZ.setColor(Color.red);
                    for (yIndex = Math.round(y[0]); yIndex <= Math.round(y[1]); yIndex++) {
                        xCurve = new float[4];
                        zCurve = new float[4];
                        yCurve = new float[4];
                        xCurve[0] = x[0];
                        zCurve[0] = z[0];
                        yCurve[0] = yIndex;
                        xCurve[1] = x[1];
                        zCurve[1] = z[0];
                        yCurve[1] = yIndex;
                        xCurve[2] = x[1];
                        zCurve[2] = z[1];
                        yCurve[2] = yIndex;
                        xCurve[3] = x[0];
                        zCurve[3] = z[1];
                        yCurve[3] = yIndex;
                        voiXZ.importCurve(xCurve, yCurve, zCurve);
                    }
                    yManager.getImage().registerVOI(voiXZ);
                }
                
            }
        } // for ( int i = 0; i < copyList.size(); i++ )
    }

    private void pasteFromViewUserInterface()
    {
        ModelImage kActive = getActiveImage();
        //PrintStream out = System.out;
        
        int xDim = kActive.getExtents().length > 0 ? kActive.getExtents()[0] : 1;
        int yDim = kActive.getExtents().length > 1 ? kActive.getExtents()[1] : 1;
        int zDim = kActive.getExtents().length > 2 ? kActive.getExtents()[2] : 1;

        int[] xBounds = new int[2];
        int[] yBounds = new int[2];
        int[] zBounds = new int[2];
        

        // Get the Global copy list:
        Vector<VOIBase> copyList = ViewUserInterface.getReference().getCopyVOIs();
        Vector<String> copyNameList = ViewUserInterface.getReference().getCopyVOINames();
        
        // make new VOI groups:
        VOIVector newVOIs = new VOIVector();
        int iAdded = 0;
        short sID = (short)(kActive.getVOIs().getUniqueID());
        while ( iAdded < copyList.size() )
        {
        	// get last non-added contour:
            VOIBase kCurrentVOI = copyList.elementAt(iAdded);
            String kName = copyNameList.elementAt(iAdded++);
            // create a new VOI
            VOI kNewVOI = new VOI(sID++, kName);
            kNewVOI.setCurveType( kCurrentVOI.getType() );
            kNewVOI.setColor( kCurrentVOI.getGroup().getColor() );
                        
            // Put the current contours in the new group:
            VOI kOldGroup = kCurrentVOI.getGroup();
            kNewVOI.getCurves().add( kCurrentVOI.clone() );
            // Add all the other contours with the same group to this group
            for ( ; iAdded < copyList.size(); iAdded++ )
            {
                if ( copyList.elementAt(iAdded).getGroup() == kOldGroup )
                {
                    kNewVOI.getCurves().add( copyList.elementAt(iAdded).clone() );
                }
                else
                {
                    break;
                }
            }
            // add the new group.
            if ( kNewVOI.getCurves().size() > 0 )
            {
                newVOIs.add( kNewVOI );
            }
        }
        // At this point the contours are grouped together and ready to be copied...
        // Paste new VOI groups:
        
        //New additions to adjust position of pasted VOIs
        ViewJFrameImage fActive = kActive.getParentFrame();
        int currentSlice = fActive.getViewableSlice();
        int minSlice = 9999;
        for ( int i = 0; i < newVOIs.size(); i++ ){
        	for( int j = 0; j< newVOIs.VOIAt(i).getCurves().size(); j++) {
        		int slice = newVOIs.VOIAt(i).getCurves().elementAt(j).slice(4);
        		if (slice < minSlice) minSlice = slice;
        	}
        }
        
        for ( int i = 0; i < newVOIs.size(); i++ )
        {
            VOI currentVOI = newVOIs.get(i);
            currentVOI.getBounds( xBounds, yBounds, zBounds );
            if ( (xBounds[1] < xDim) && (yBounds[1] < yDim) && (zBounds[1] < zDim) )
            {
            	//out.println("Registers");
                kActive.registerVOI(currentVOI);
                currentVOI.addVOIListener(this);
            }
            else //This case for when VOIs go out of bounds on a new image set
            {
                // Copy the VOI and contents so we can modify it for this image.
                // May want to paste again into another image, so don't modify the original from the copy list.
                currentVOI = new VOI(currentVOI);
                for ( int j = currentVOI.getCurves().size()-1; j >= 0; j-- )
                {
                    VOIBase kCurrentVOI = currentVOI.getCurves().get(j);
                    Vector3f[] kBounds = kCurrentVOI.getImageBoundingBox();
                    
                    //Adjusts the old VOI grouping to the new image (as per Bug 496)
                    int newSlice = kCurrentVOI.slice(4) - minSlice + currentSlice;
                    kBounds[1].Z -= minSlice - currentSlice;
                    kBounds[0].Z -= minSlice - currentSlice ;
                    
                    if ( (kBounds[1].X - kBounds[0].X < xDim) && 
                         (kBounds[1].Y - kBounds[0].Y < yDim) && 
                         (kBounds[1].Z - kBounds[0].Z < zDim) )
                    {
                        for ( int k = 0; k < kCurrentVOI.size(); k++ )
                        {
                        	kCurrentVOI.elementAt(k).Z = newSlice;
                            if ( kBounds[1].X >= xDim )
                            {
                                kCurrentVOI.elementAt(k).X -= (kBounds[1].X - (xDim-1));
                            }
                            if ( kBounds[1].Y >= yDim )
                            {
                                kCurrentVOI.elementAt(k).Y -= (kBounds[1].Y - (yDim-1));
                            }
                            if ( kBounds[1].Z >= zDim )
                            {
                                kCurrentVOI.elementAt(k).Z -= (kBounds[1].Z - (zDim-1));
                            }
                        }
                        kCurrentVOI.update();
                    }
                    else
                    {
                        currentVOI.getCurves().remove(kCurrentVOI);
                    }
                }              
                if ( currentVOI.getCurves().size() > 0 )
                {               
                    kActive.registerVOI(currentVOI);
                    currentVOI.addVOIListener(this);
                }
            }  
        }
        updateDisplay();
    }

    private void propagate(int dir)
    {
        // Get the Global copy list:
        Vector<VOIBase> copyList = ViewUserInterface.getReference().getCopyVOIs();
        // Return if no contours to paste:
        if ( copyList.size() == 0 )
        {
            return;
        }
        // If the copy list is from another image:
        if ( copyList.elementAt(0).getGroup() != null && !copyList.elementAt(0).getGroup().hasListener(this) )
        {
            pasteFromViewUserInterface();
            return;
        }
        // The copy list is from this image/manager: 
        for ( int i = 0; i < copyList.size(); i++ )
        {
            VOIBase kCurrentVOI = copyList.get(i); 
            VOIManager kManager = m_kVOIManagers.elementAt(0);
            for ( int j = 0; j < m_kVOIManagers.size(); j++ )
            {
            	int iPlane = m_kVOIManagers.elementAt(j).getPlane();
                if ( iPlane == (iPlane & kCurrentVOI.getPlane()) )
                {
                    kManager = m_kVOIManagers.elementAt(j);
                    break;
                }
            }

            kManager.propagateVOI( kCurrentVOI, dir );  
        }
    }

    /**
     * method that performs the quick LUT operation
     * @param akMinMax
     * @param image
     * @param LUT
     */
    private void quickLUT(Vector3f[] akMinMax, ModelImage image, ModelLUT LUT) {
        if ( !(((akMinMax[1].Z - akMinMax[0].Z) > 5) ||
                ((akMinMax[1].Y - akMinMax[0].Y) > 5) ||
                ((akMinMax[1].X - akMinMax[0].X) > 5) )  )
        {
            return;
        }        
        float min = Float.MAX_VALUE;
        float max = -100000000;
        float val;
        int t = 0;
        if(image.is4DImage()) {
        	t = ViewUserInterface.getReference().getActiveImageFrame().getComponentImage().getTimeSlice();
        }
        for ( int z = (int)akMinMax[0].Z; z <= akMinMax[1].Z; z++ )
        {
            for ( int y = (int)akMinMax[0].Y; y <= akMinMax[1].Y; y++ )
            {
                for ( int x = (int)akMinMax[0].X; x <= akMinMax[1].X; x++ )
                {
                    if(image.is4DImage()) {
                    	val = image.getFloat(x,y,z,t);
                    }else {
                    	val = image.getFloat(x,y,z);
                    }
                    
                    if ( val < min )
                    {
                        min = val;
                    }
                    if ( val > max )
                    {
                        max = val;
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




    /**
     * DOCUMENT ME!
     * 
     * @param xS DOCUMENT ME!
     * @param wS DOCUMENT ME!
     * @param yS DOCUMENT ME!
     * @param hS DOCUMENT ME!
     * @param imageBuffer DOCUMENT ME!
     * @param image DOCUMENT ME!
     * @param RGB DOCUMENT ME!
     */
    private void quickRGB(final Vector3f[] akMinMax, final ModelImage image, final ModelRGB RGB) {
        if ( !(((akMinMax[1].Z - akMinMax[0].Z) > 5) ||
                ((akMinMax[1].Y - akMinMax[0].Y) > 5) ||
                ((akMinMax[1].X - akMinMax[0].X) > 5) )  )
        {
            return;
        }        
        final float[] minC = {Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE};
        final float[] maxC = { -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE};
        float val;

        for ( int z = (int)akMinMax[0].Z; z <= akMinMax[1].Z; z++ )
        {
            for ( int y = (int)akMinMax[0].Y; y <= akMinMax[1].Y; y++ )
            {
                for ( int x = (int)akMinMax[0].X; x <= akMinMax[1].X; x++ )
                {
                    for (int c = 1; c < 4; c++) {

                        val = image.getFloatC(x,y,z,c);     
                        if ( val < minC[c-1] )
                        {
                            minC[c-1] = val;
                        }
                        if ( val > maxC[c-1] )
                        {
                            maxC[c-1] = val;
                        }                        
                    }
                }
            }
        }
        final float[][] x = new float[3][4];
        final float[][] y = new float[3][4];
        final Dimension dim = new Dimension(256, 256);
        for (int i = 0; i < 3; i++) {
            x[i][1] = minC[i];
            x[i][2] = maxC[i];

            x[i][0] = 0;
            x[i][3] = 255;

            y[i][0] = dim.height - 1;
            y[i][1] = dim.height - 1;
            y[i][2] = 0;
            y[i][3] = 0;
        }
        RGB.getRedFunction().importArrays(x[0], y[0], 4);
        RGB.getGreenFunction().importArrays(x[1], y[1], 4);
        RGB.getBlueFunction().importArrays(x[2], y[2], 4);
        RGB.makeRGB( -1);
    }

    private void redoImage( )
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

    private void redoVOI()
    {
        if ( m_kRedoCommands.isEmpty() )
        {
            return;
        }        
        String lastCommand = m_kRedoCommands.remove( m_kRedoCommands.size() -1 );
        m_kUndoCommands.add(lastCommand);
        if ( lastCommand.equals( CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand() ) ||
                lastCommand.equals( CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand() ) )
        {
            if ( m_bGPURenderer )
            {                
                getActiveImage().useMask(true); 
            }
            else
            {
                redoImage();
            }
            m_kParent.updateData();
        }
        else
        {
            redoVOIs();
        }
        toolbarBuilder.getVOIUndoButton().setEnabled(true);
        toolbarBuilder.getVOIRedoButton().setEnabled(!m_kRedoCommands.isEmpty());
    }

    protected void redoVOIs()
    {
        if ( m_kRedoList.isEmpty() )
        {
            return;
        }
        m_kUndoList.add( getVOIState() );
        setVOIState( m_kRedoList.remove( m_kRedoList.size() - 1) );

        if ( imageStatList != null )
        {
            imageStatList.refreshVOIList(getActiveImage().getVOIs());
        }
        if (m_kVOIDialog != null) {
            m_kVOIDialog.updateVOIPanel(m_kCurrentVOIGroup, getActiveImage() );
        }
        updateDisplay();
    }

    /**
     * This method saves all VOIs for the active image to the default VOI directory for that image.
     */
    private void saveAllVOIs() {

        String fileDir;
        String tmpImageName;
        String imageName;
        String voiDir;
        ModelImage img = getActiveImage();
        fileDir = img.getFileInfo(0).getFileDirectory();

        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        if (img.isDicomImage()) {
            tmpImageName = img.getFileInfo(0).getFileName();

            final int index = tmpImageName.lastIndexOf(".");

            if (index > 0) {
                tmpImageName = tmpImageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = tmpImageName.length();

            for (int i = tmpImageName.length() - 1; i >= 0; i--) {
                final char myChar = tmpImageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                tmpImageName = new String("DICOM");
            } else {
                tmpImageName = tmpImageName.substring(0, newIndex);
            }
        } else {
            tmpImageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = tmpImageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        voiDir = new String(fileDir + File.separator + "defaultVOIs_" + imageName + File.separator);

        saveAllVOIsTo(voiDir);

    } // end saveAllVOIs()



    /**
     * This method saves all VOIs for the active image to a given directory.
     * @param voiDir directory that contains VOIs for this image.
     */
    private void saveAllVOIsTo(final String voiDir) {
        try {

            ModelImage currentImage = getActiveImage();
            ViewVOIVector VOIs = currentImage.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
            	String voiName = JDialogBase.makeImageName( VOIs.VOIAt(i).getName(), "");
                int index = voiName.lastIndexOf(File.separator);
                if ( index != -1 )
                {
                	voiName = voiName.substring(index+1, voiName.length() );
                }
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, currentImage);
                    fileVOI.writeVOI(VOIs.VOIAt(i), true);
                } else {
                    FileVOI fileVOI = new FileVOI(voiName + ".lbl", voiDir, currentImage);
                    fileVOI.writeAnnotationInVoiAsXML(voiName,true);
                }
            }

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }

    } // end saveAllVOIsTo()

    
    /**
     * Save the dicom matrix header info. The .ply file format can't save the dicom info. We decide to save the dicom
     * info when save the VOI file. So, the dicom info will be read when load the .ply surface into the volume render.
     * This ensures the correct scale of surface. The dicom matrix info is saved in the current image directory with the
     * .dicomMatrix suffix.
     */
    private void saveImage( String kCommand )
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
    
    private void saveLabels(final boolean saveAll) {
        String fileName;
        String directory;
        JFileChooser chooser;

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        boolean foundLabel = false;

        ModelImage kImage = getActiveImage();
        VOIs = kImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if ( (VOIs.VOIAt(i).isActive() || saveAll) && (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION)) {
                foundLabel = true;
            }
        }

        if ( !foundLabel) {
            MipavUtil.displayWarning("There are no labels on the image.");

            return;
        }

        chooser = new JFileChooser();
        chooser.setDialogTitle("Save label(s) as");

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".lbl"}));

        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            if ( !fileName.endsWith(".lbl")) {
                fileName += ".lbl";
            }
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);

        } else {
            return;
        }

        try {

            final FileVOI fileVOI = new FileVOI(fileName, directory, kImage);

            fileVOI.writeAnnotationXML(saveAll);

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing labels");
        }

    }

    /**
     * This method saves a selected VOI
     * @param saveAllContours if true all contours are saved
     */
    private void saveVOI(final boolean saveAllContours) {

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        FileVOI fileVOI;
        String extension = ".xml";

        try {

            ModelImage kImage = getActiveImage();
            VOIs = kImage.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive()) {
                    break;
                }
            }

            if (i == nVOI) {
                MipavUtil.displayError("Please select a VOI.");

                return;
            }

            if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                if (VOIs.VOIAt(i).getExtension().equals(".voi")) {
                    extension = ".voi";
                }
                fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + extension,
                        kImage.getFileInfo(0).getFileDirectory(), kImage);
                fileVOI.writeVOI(VOIs.VOIAt(i), saveAllContours);
            } else {
                fileVOI = new FileVOI(VOIs.VOIAt(i) + ".lbl", kImage.getFileInfo(0).getFileDirectory(), kImage);
                fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), saveAllContours);
            }
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing VOI" + error);
        }
    }

    
    /**
     * This method allows the user to choose how to save the VOI.
     * @param saveAllContours if true all contours are saved
     */
    private void saveVOIAs(final boolean saveAllContours) {
        String fileName;
        String directory;
        JFileChooser chooser;
        JPanel accessoryPanel = new JPanel();
        ButtonGroup VOIGroup;
        JRadioButton saveVOILPSButton;
        JRadioButton saveVOIVoxelButton;
        
        String voiName; 
        Vector<String> nameVector = new Vector<String>();

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        boolean doPoint = false, doAnnotation = false;


        ModelImage kImage = getActiveImage();
        VOIs = kImage.getVOIs();
        nVOI = VOIs.size();
        
        //New code
        for (int iter=0;iter<nVOI;iter++){
        	nameVector.add(VOIs.VOIAt(iter).getName());
        }
        //End new code

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {
                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("Please select a VOI.");

            return;
        }
        chooser = new JFileChooser();
        chooser.setDialogTitle("Save VOI as");
        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }
        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".xml",".lbl"}));
        chooser.setAccessory(accessoryPanel);
        accessoryPanel.setBorder(BorderFactory.createLineBorder(Color.black));
        accessoryPanel.setLayout(new BorderLayout());
        
        PanelManager optionsPanelManager = new PanelManager("Options");
        VOIGroup = new ButtonGroup();
        saveVOILPSButton = new JRadioButton("Save VOIs in LPS mm. coordinates", 
        		Preferences.is(Preferences.PREF_VOI_LPS_SAVE));
        saveVOILPSButton.setFont(MipavUtil.font12);
        saveVOILPSButton.setForeground(Color.black);
        saveVOILPSButton.addActionListener(this);
        saveVOILPSButton.setToolTipText("If selected, VOIs will be saved in LPS mm. coordinates.");
        VOIGroup.add(saveVOILPSButton);
        optionsPanelManager.add(saveVOILPSButton);
        
        saveVOIVoxelButton = new JRadioButton("Save VOIs in voxel coordinates", 
        		!Preferences.is(Preferences.PREF_VOI_LPS_SAVE));
        saveVOIVoxelButton.setFont(MipavUtil.font12);
        saveVOIVoxelButton.setForeground(Color.black);
        saveVOIVoxelButton.addActionListener(this);
        saveVOIVoxelButton.setToolTipText("If selected, VOIs will be saved in voxel coordinates.");
        VOIGroup.add(saveVOIVoxelButton);
        optionsPanelManager.addOnNextLine(saveVOIVoxelButton);
        accessoryPanel.add(optionsPanelManager.getPanel(), BorderLayout.CENTER);

        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);
            Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, String.valueOf(saveVOILPSButton.isSelected()));
        } else {
            return;
        }

        try {

            if (fileName.endsWith(".lbl") || VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION) {
                doAnnotation = true;
                if (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION && !fileName.endsWith(".lbl")) {
                    fileName += ".lbl";
                }
            } else if ((!fileName.endsWith(".xml")) && (!fileName.endsWith(".voi"))) {
                fileName += ".xml";
            }
            
          //New code
            if (fileName.endsWith(".voi")) {
            	voiName = fileName.substring(0, fileName.indexOf(".voi"));
            }
            else {
                voiName = fileName.substring(0, fileName.indexOf(".xml"));
            }
            String newName = voiName;
            int cnt = 0;
            while(nameVector.contains(newName)){
            	newName = voiName.concat("_" + String.valueOf(cnt));
            	cnt++;
            }
            VOIs.VOIAt(i).setName(newName);
            //End new code

            final FileVOI fileVOI = new FileVOI(fileName, directory, kImage);

            if ( !doPoint && !doAnnotation) {
            	
                // use the MIPAV VOI format (not Nauges) since we
                // need to save the curveType in order to correctly
                // rebuild the VOIs when reading the VOI files.
                fileVOI.writeVOI(VOIs.VOIAt(i), saveAllContours);
            } else if (doPoint) {
                fileVOI.writePointVOI(VOIs.VOIAt(i));
            } else if (doAnnotation) {
                fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), saveAllContours);
            }
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing VOI");
        }

    }
    
    
    

    private void exportSelectedVOIs() {
        String fileName;
        String directory;


        ModelImage kImage = getActiveImage();
        ViewVOIVector VOIs = kImage.getVOIs();
        int nVOI = VOIs.size();
        boolean bSelected = false;
        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {
            	bSelected = true;
                break;
            }
        }

        if (!bSelected) {
            MipavUtil.displayError("Please select a VOI.");
            return;
        }
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Export VOI Polygon as");
        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }        

        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();            
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);
        } else {
            return;
        }

        try {
            ObjectOutputStream objstream;
            int numSlices = getActiveImage().getExtents().length > 2 ? getActiveImage().getExtents()[2] : 1;
            for ( int z = 0; z < numSlices; z++ )
            {
            	for (int i = 0; i < nVOI; i++) {
            		if (VOIs.VOIAt(i).isActive()) {
            			Vector<Polygon> polys = VOIs.VOIAt(i).exportPolygon(z);
            			if ( polys != null )
            			{
            				for ( int j = 0; j < polys.size(); j++ )
            				{
            					Polygon poly = polys.elementAt(j);
            					objstream = new ObjectOutputStream(new FileOutputStream(directory + fileName + z + "_" + i + "_" + j));
            					objstream.writeObject(poly);
            					objstream.close();
            				}
            			}
            		}
            	}
            }
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing VOI");
        }
    }
    

    
    

    private void importVOI() {
        String fileName;
        String directory;


        ModelImage kImage = getActiveImage();

        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Import VOI Polygon");
        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }        

        final int returnVal = chooser.showOpenDialog(m_kParent.getFrame());

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();            
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);
        } else {
            return;
        }

        try {
            ObjectInputStream objstream;
            objstream = new ObjectInputStream(new FileInputStream(directory + fileName));
            Polygon poly = (Polygon)objstream.readObject();
            objstream.close();
            short sID = (short)(kImage.getVOIs().getUniqueID());
            VOI kVOI = new VOI(sID, fileName);
            kVOI.importPolygon( poly, getSlice() );
            kImage.registerVOI(kVOI);
            kImage.notifyImageDisplayListeners();
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing VOI");
        } catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }
    
    private void savePaint() {

        FilePaintBitmap filePaint;
        String extension = ".pbm";
        
        ModelImage kImage = getActiveImage();
        BitSet paintBitmap = kImage.getParentFrame().getComponentImage().getPaintBitmap();
        if (paintBitmap == null) {
        	return;
        }

        try {
            filePaint = new FilePaintBitmap(kImage.getImageName() + extension,
            		  kImage.getFileInfo(0).getFileDirectory(), kImage);
            filePaint.writePaintBitmap(paintBitmap);
            
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing paint bitmap" + error);
        }
    }

    
    /**
     * This method allows the user to choose how to save the paint bitmap.
     */
    private void savePaintAs() {
        String fileName;
        String directory;
        JFileChooser chooser;
        
        FilePaintBitmap filePaint;
        
        ModelImage kImage = getActiveImage();
        BitSet paintBitmap = kImage.getParentFrame().getComponentImage().getPaintBitmap();
        if (paintBitmap == null) {
        	return;
        }

        chooser = new JFileChooser();
        chooser.setDialogTitle("Save Paint as");
        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }
        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".pbm"}));

        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);
        } else {
            return;
        }

        try {

            if (!fileName.endsWith(".pbm")) {
                fileName += ".pbm";
            }
            filePaint = new FilePaintBitmap(fileName, directory, kImage);
            filePaint.writePaintBitmap(paintBitmap);
            
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing paint bitmap");
        }

    }
    
    /**
     * Save intensities in VOI to a text file of format x,y,z,intensity on each line if not color or complex. If color
     * use format x,y,z,a,r,g,b on each line and if complex use format x,y,z,real,imaginary on each line.
     */
    private void saveVOIIntensities() {
        String fileName;
        String directory;
        JFileChooser chooser;
        File textFile;
        RandomAccessFile raFile;
        ModelImage selectedImage = null;
        int imageSize;
        int nDims;
        BitSet mask;
        int xDim;
        int yDim;
        int zDim;
        int sliceSize;

        int nVOI;
        int i, j, k;
        ViewVOIVector VOIs = null;
        int x;
        int y;
        int z;
        double buffer[];
        byte entryBytes[];

        selectedImage = getActiveImage();
        VOIs = selectedImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {
                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("Please select a VOI.");

            return;
        }

        nDims = selectedImage.getNDims();
        xDim = selectedImage.getExtents()[0];
        yDim = selectedImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imageSize = sliceSize;
        if (nDims > 2) {
            zDim = selectedImage.getExtents()[2];
            imageSize *= zDim;
        } else {
            zDim = 1;
        }
        mask = new BitSet(imageSize);
        VOIs.VOIAt(i).createBinaryMask3D(mask, xDim, yDim, false, false);
        if (selectedImage.isColorImage()) {
            buffer = new double[4 * imageSize];
        } else if (selectedImage.isComplexImage()) {
            buffer = new double[2 * imageSize];
        } else {
            buffer = new double[imageSize];
        }

        try {
            selectedImage.exportData(0, buffer.length, buffer);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on selectedImage.exportData");
            return;
        }

        chooser = new JFileChooser();
        chooser.setDialogTitle("Save intensities in VOI as");

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".txt"}));

        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);
        } else {
            return;
        }

        if ( !fileName.endsWith(".txt")) {
            fileName += ".txt";
        }

        textFile = new File(directory + fileName);
        try {
            raFile = new RandomAccessFile(textFile, "rw");
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile = new RandomAccessFile");
            return;
        }
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        try {
            raFile.setLength(0);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.setLength(0)");
            return;
        }

        if (selectedImage.isColorImage()) {
            entryBytes = new String("x,y,z,a,red,green,blue\n").getBytes();
        } else if (selectedImage.isComplexImage()) {
            entryBytes = new String("x,y,z,real,imaginary\n").getBytes();
        } else {
            entryBytes = new String("x,y,z,intensity\n").getBytes();
        }
        try {
            raFile.write(entryBytes);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.write(entryBytes) for header line");
            return;
        }

        for (z = 0; z < zDim; z++) {
            k = z * sliceSize;
            for (y = 0; y < yDim; y++) {
                j = k + y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = j + x;
                    if (mask.get(i)) {
                        if (selectedImage.isColorImage()) {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[4 * i]) + ","
                                    + Double.toString(buffer[4 * i + 1]) + "," + Double.toString(buffer[4 * i + 2])
                                    + "," + Double.toString(buffer[4 * i + 3]) + "\n").getBytes();
                        } else if (selectedImage.isComplexImage()) {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[2 * i]) + ","
                                    + Double.toString(buffer[2 * i + 1]) + "\n").getBytes();
                        } else {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[i]) + "\n").getBytes();
                        }
                        try {
                            raFile.write(entryBytes);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on raFile.write(entryBytes");
                            return;
                        }
                    }
                }
            }
        }
        try {
            raFile.close();
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.close()");
        }

    }
    
    /**
     * Save intensities in VOI to a text file of format x,y,z,intensity on each line if not color or complex. If color
     * use format x,y,z,a,r,g,b on each line and if complex use format x,y,z,real,imaginary on each line.
     */
    private void saveVOIIntensitiesTo(String voiIntensitiesPath) {
        File textFile;
        RandomAccessFile raFile;
        ModelImage selectedImage = null;
        int imageSize;
        int nDims;
        BitSet mask;
        int xDim;
        int yDim;
        int zDim;
        int sliceSize;

        int nVOI;
        int i, j, k;
        ViewVOIVector VOIs = null;
        int x;
        int y;
        int z;
        double buffer[];
        byte entryBytes[];

        selectedImage = getActiveImage();
        VOIs = selectedImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {
                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("Please select a VOI.");

            return;
        }

        nDims = selectedImage.getNDims();
        xDim = selectedImage.getExtents()[0];
        yDim = selectedImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imageSize = sliceSize;
        if (nDims > 2) {
            zDim = selectedImage.getExtents()[2];
            imageSize *= zDim;
        } else {
            zDim = 1;
        }
        mask = new BitSet(imageSize);
        VOIs.VOIAt(i).createBinaryMask3D(mask, xDim, yDim, false, false);
        if (selectedImage.isColorImage()) {
            buffer = new double[4 * imageSize];
        } else if (selectedImage.isComplexImage()) {
            buffer = new double[2 * imageSize];
        } else {
            buffer = new double[imageSize];
        }

        try {
            selectedImage.exportData(0, buffer.length, buffer);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on selectedImage.exportData");
            return;
        }

        if ( !voiIntensitiesPath.endsWith(".txt")) {
            voiIntensitiesPath += ".txt";
        }

        System.out.println(voiIntensitiesPath);
        textFile = new File(voiIntensitiesPath);
        try {
            raFile = new RandomAccessFile(textFile, "rw");
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile = new RandomAccessFile");
            return;
        }
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        try {
            raFile.setLength(0);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.setLength(0)");
            return;
        }

        if (selectedImage.isColorImage()) {
            entryBytes = new String("x,y,z,a,red,green,blue\n").getBytes();
        } else if (selectedImage.isComplexImage()) {
            entryBytes = new String("x,y,z,real,imaginary\n").getBytes();
        } else {
            entryBytes = new String("x,y,z,intensity\n").getBytes();
        }
        try {
            raFile.write(entryBytes);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.write(entryBytes) for header line");
            return;
        }

        for (z = 0; z < zDim; z++) {
            k = z * sliceSize;
            for (y = 0; y < yDim; y++) {
                j = k + y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = j + x;
                    if (mask.get(i)) {
                        if (selectedImage.isColorImage()) {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[4 * i]) + ","
                                    + Double.toString(buffer[4 * i + 1]) + "," + Double.toString(buffer[4 * i + 2])
                                    + "," + Double.toString(buffer[4 * i + 3]) + "\n").getBytes();
                        } else if (selectedImage.isComplexImage()) {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[2 * i]) + ","
                                    + Double.toString(buffer[2 * i + 1]) + "\n").getBytes();
                        } else {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[i]) + "\n").getBytes();
                        }
                        try {
                            raFile.write(entryBytes);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on raFile.write(entryBytes");
                            return;
                        }
                    }
                }
            }
        }
        try {
            raFile.close();
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.close()");
        }

    }
    
    private void setCurrentColor( )
    {
        setButtonColor(toolbarBuilder.getVOIColorButton(), 
                toolbarBuilder.getVOIColorButton().getBackground() );
    }

    private void setGraphVisible() { 
        int nVOI;
        ViewVOIVector VOIs;

        ModelImage kImage = getActiveImage();
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
            "0054,1300"));
            if (frameRefTimeString != null) {
                try {
                    frameReferenceTime = new Integer(frameRefTimeString.trim())
                    .intValue();
                    frameRefTimeFound = true;
                    Preferences.debug("Frame reference time = "
                            + frameReferenceTime + "\n");
                } catch (NumberFormatException e) {
                    Preferences
                    .debug("Number format excepton from frame Reference Time String = "
                            + frameRefTimeString.trim() + "\n");
                }
                
                

                if (frameRefTimeFound) {
                    
                } // if (frameRefTimeFound)
            } // if (frameRefTimeString != null)
            
            frameRefTimeString = JOptionPane.showInputDialog("Enter value for the graph x axis:", 
                                                            frameRefTimeFound ? frameReferenceTime : 1);
            try {
                frameReferenceTime = new Integer(frameRefTimeString.trim());
                useFrameRefTime = true;
            } catch (NumberFormatException e) {
                Preferences
                .debug("Number format excepton from frame Reference Time String = "
                        + frameRefTimeString.trim() + "\n");
                useFrameRefTime = false;
            }
        } // if if ((compImage.getActiveImage().getNDims() == 4)

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                if ((VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                        && (VOIs.VOIAt(i).getContourGraph() != null)) {
                    if (kImage.getNDims() == 4) {
                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(
                        		Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                                        .getUnitsOfMeasure(3)).getAbbrev());
                    } else {
                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(
                        		Unit.getUnitFromLegacyNum(kImage.getFileInfo(0)
                                        .getUnitsOfMeasure(0)).getAbbrev());
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
    
    private void setPAAIGraphVisible() {   
        int nVOI;
        ViewVOIVector VOIs;

        VOIs = getActiveImage().getVOIs();
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
                                getActiveImage(), (int) pt.X, (int) pt.Y, true);
                    }
                }
            }
        }

    }

    /*
     * public void extractSurfaceFromVOIs() { JDialogBase extractSurfaceDialog = new JDialogExtractSurfaceVOIs(this);
     * extractSurfaceDialog.validate(); }
     */

    protected void setVOIState( VOISaveState kVOIState )
    {
        m_kImageA.unregisterAllVOIs();
        m_kImageA.restoreVOIs( kVOIState.voiVectorA );
        if ( m_kImageB != null )
        {
            m_kImageB.unregisterAllVOIs();
            m_kImageB.restoreVOIs( kVOIState.voiVectorB );
        }
        if ( kVOIState.currentVOI != -1 )
        {
            m_kCurrentVOIGroup = m_kImageA.getVOIs().get(kVOIState.currentVOI);
            if (presetHue >= 0.0) {
            	m_kCurrentVOIGroup.setColor(presetHue);
            }
            setSelectedVOI( m_kCurrentVOIGroup, m_kCurrentVOIGroup.isAllActive(), true );
        }
        else
        {
            m_kCurrentVOIGroup = null;
        }        
        if ( m_kParent != null )
        {
        	m_kParent.setCenter( new Vector3f( kVOIState.currentCenter ) );
        }
    }

    private void undoImage( )
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

    private void undoVOI()
    {
        if ( m_kUndoCommands.isEmpty() )
        {
            return;
        }        
        String lastCommand = m_kUndoCommands.remove( m_kUndoCommands.size() -1 );
//        System.err.println("undo " + lastCommand);
        m_kRedoCommands.add(lastCommand);
        if ( lastCommand.equals( CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand() ) ||
                lastCommand.equals( CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand() ) )
        {
            if ( m_bGPURenderer )
            {
                getActiveImage().useMask(false);
            }
            else
            {
                undoImage();
            }
            m_kParent.updateData();
        }
        else
        {
            undoVOIs();
        }
        toolbarBuilder.getVOIUndoButton().setEnabled(!m_kUndoCommands.isEmpty());
        toolbarBuilder.getVOIRedoButton().setEnabled(true);
    }

    protected void undoVOIs()
    {
        if ( m_kUndoList.size() <= 0 )
        {
            return;
        }
        m_kRedoList.add( getVOIState() );
        setVOIState( m_kUndoList.remove( m_kUndoList.size() - 1) );
        
        if ( imageStatList != null )
        {
            imageStatList.refreshVOIList(getActiveImage().getVOIs());
        }
        if (m_kVOIDialog != null) {
            m_kVOIDialog.updateVOIPanel(m_kCurrentVOIGroup, getActiveImage() );
        }
        updateDisplay();
    }

    /**
     * Opens a JDialogStatistics to allow computation ofROI statistics.
     */
    protected void showStatisticsCalculator() {
    	/* Tested just calculating the volume statistic:
    	boolean[] statsList = new boolean[VOIStatisticList.numberOfStatistics];
    	for ( int i = 0; i < VOIStatisticList.numberOfStatistics; i++ )
    	{
    		statsList[i] = false;
    		if ( VOIStatisticList.statisticDescription[i].equals( VOIStatisticList.volumeDescription) )
    		{
    			statsList[i] = true;
    		}
    	}
    	ViewVOIVector vector = new ViewVOIVector();
    	vector.add( getActiveImage().getVOIs().elementAt(0) );
    	AlgorithmVOIProps algProps = new AlgorithmVOIProps( getActiveImage(), AlgorithmVOIProps.PROCESS_PER_VOI, vector);
    	algProps.setSelectedStatistics(statsList);
    	algProps.setDistanceFlag(false);
    	algProps.setSliceDistanceFlag(false);
    	algProps.runAlgorithm();
    	float volume = algProps.getVolume();
    	System.err.println( volume );
    	*/
    	
        if (imageStatList == null) {
            
            if ( (getActiveImage().getVOIs() != null) && (getActiveImage().getVOIs().size() != 0)) {
                imageStatList = new JDialogVOIStatistics(getActiveImage(), getActiveImage().getVOIs());
                imageStatList.setVisible(true);
            } else {
                MipavUtil.displayError("A VOI must be present to use the statistics calculator");
            }
        } else {
            imageStatList.refreshVOIList(getActiveImage().getVOIs());
            imageStatList.setVisible(true);
        }
    }

    protected void showVOIProperties() {
        if (m_kVOIDialog == null) {
            m_kVOIDialog = new JDialogVOIStats( this, getActiveImage(), m_kCurrentVOIGroup );
            addVOIUpdateListener(m_kVOIDialog);
        }

        if (m_kVOIDialog != null) {
            m_kVOIDialog.setVisible(true);
            m_kVOIDialog.updateVOIPanel(m_kCurrentVOIGroup, getActiveImage() );
        }
    }
    
    protected void initVOIManagers(int iNViews) {
        m_kVOIManagers = new Vector<VOIManager>();
        for ( int i = 0; i < iNViews; i++ )
        {
            m_kVOIManagers.add(new VOIManager(this));
        }
        if ( (popup != null) && (popupPt != null) ) {
        	for ( int i = 0; i < iNViews; i++ )
        	{
        		m_kVOIManagers.elementAt(i).setPopupVOI(popup);
        		m_kVOIManagers.elementAt(i).setPopupPt(popupPt);
        	}
        }
    }
    
    private void readImageJ( String pathName, String fileName, ModelImage image)
	{
    	// header offsets
    	final int VERSION_OFFSET = 4;
    	final int TYPE = 6;
    	final int TOP = 8;
    	final int LEFT = 10;
    	final int BOTTOM = 12;
    	final int RIGHT = 14;
    	final int N_COORDINATES = 16;
    	final int X1 = 18;
    	final int Y1 = 22;
    	final int X2 = 26;
    	final int Y2 = 30;
    	final int XD = 18;
    	final int YD = 22;
    	final int WIDTHD = 26;
    	final int HEIGHTD = 30;
    	final int STROKE_WIDTH = 34;
    	final int SHAPE_ROI_SIZE = 36;
    	final int STROKE_COLOR = 40;
    	final int FILL_COLOR = 44;
    	final int SUBTYPE = 48;
    	final int OPTIONS = 50;
    	final int ARROW_STYLE = 52;
    	final int FLOAT_PARAM = 52; // ellipse ratio or rotated rect width
    	final int POINT_TYPE = 52;
    	final int ARROW_HEAD_SIZE = 53;
    	final int ROUNDED_RECT_ARC_SIZE = 54;
    	final int POSITION = 56;
    	final int HEADER2_OFFSET = 60;
    	final int COORDINATES = 64;
    	// header2 offsets
    	final int C_POSITION = 4;
    	final int Z_POSITION = 8;
    	final int T_POSITION = 12;
    	final int NAME_OFFSET = 16;
    	final int NAME_LENGTH = 20;
    	final int OVERLAY_LABEL_COLOR = 24;
    	final int OVERLAY_FONT_SIZE = 28; // short
    	final int AVAILABLE_BYTE1 = 30; // byte
    	final int IMAGE_OPACITY = 31; // byte
    	final int IMAGE_SIZE = 32; // int
    	final int FLOAT_STROKE_WIDTH = 36; // float
    	final int ROI_PROPS_OFFSET = 40;
    	final int ROI_PROPS_LENGTH = 44;
    	final int COUNTERS_OFFSET = 48;
    	// Types
    	final int polygon = 0;
    	final int rect = 1;
    	final int oval = 2;
    	final int line = 3;
    	final int freeline = 4;
    	final int polyline = 5;
    	final int noRoi = 6;
    	final int freehand = 7;
    	final int traced = 8;
    	final int angle = 9;
    	final int point = 10;
    	// Subtypes
    	final int TEXT = 1;
    	final int ARROW = 2;
    	final int ELLIPSE = 3;
    	final int IMAGE = 4;
    	final int ROTATED_RECT = 5;
    	// Options
    	final int SPLINE_FIT = 1;
    	final int DOUBLE_HEADED = 2;
    	final int OUTLINE = 4;
    	final int OVERLAY_LABELS = 8;
    	final int OVERLAY_NAMES = 16;
    	final int OVERLAY_BACKGROUNDS = 32;
    	final int OVERLAY_BOLD = 64;
    	final int SUB_PIXEL_RESOLUTION = 128;
    	final int DRAW_OFFSET = 256;
    	final int ZERO_TRANSPARENT = 512;
    	long lsize;
    	int size;
    	int ns;
    	float shapeArray[];
    	int i, j;
    	boolean bigEndian = true;
    	File file = null;
    	FileInputStream is = null;
    	byte data[] = null;
    	String roiName;
    	int roiNameOffset;
    	int roiNameLength;
    	char roiCharName[];
    	int xCenter = -1;
    	int yCenter = -1;
    	String sliceNumber;
    	if (!fileName.endsWith(".roi")) {
    		MipavUtil.displayError("This is not an ImageJ ROI file");
    		return;
    	}
        file = new File(pathName);
        if (!file.exists()) {
        	MipavUtil.displayError(pathName + " does not exist");
        	return;
        }
    	
        VOIManager currentManager = m_kVOIManagers.get(m_iActive);
        int nDims = image.getNDims();
        int[] extents = image.getExtents();
        int xDim = extents[0];
        int yDim = extents[1];
        int zDim;
        if (nDims > 2) {
            zDim = extents[2];
	    }
        else {
        	zDim = 1;
        }
        int tDim;
        if (nDims > 3) {
        	tDim = extents[3];
        }
        else {
        	tDim = 1;
        }
  

        float[] resols = image.getFileInfo()[0].getResolutions();
        
        try {
            lsize = file.length();
            if (lsize > 5242880) {
                MipavUtil.displayError("imageJ VOI file size = " + lsize + " > 5 MB");
                return;
            }
            size = (int)lsize;
            is = new FileInputStream(pathName);
            data = new byte[(int)size];
            int total = 0;
            while (total < size) {
            	total += is.read(data, total, size - total);
            }
            is.close();
            // Look for "Iout in first 4 bytes"
            if ((data[0] != 73) || (data[1] != 111) || (data[2] != 117) || (data[3] != 116)) {
                MipavUtil.displayError("First 4 bytes do not have required Iout for an ImageJ ROI file");
                return;
            }
            else {
            	Preferences.debug("First 4 bytes have required IOut\n", Preferences.DEBUG_FILEIO);
            }
            int version = getBufferShort(data, VERSION_OFFSET, bigEndian);
            Preferences.debug("Version = " + version + "\n", Preferences.DEBUG_FILEIO);
            int type = getByte(data, TYPE);
            switch (type) {
            case 0:
            	Preferences.debug("Type = polygon\n", Preferences.DEBUG_FILEIO);
            	break;
            case 1:
            	Preferences.debug("Type = rect\n", Preferences.DEBUG_FILEIO);
            	break;
            case 2:
            	Preferences.debug("Type = oval\n", Preferences.DEBUG_FILEIO);
            	break;
            case 3:
            	Preferences.debug("Type = line\n", Preferences.DEBUG_FILEIO);
            	break;
            case 4:
            	Preferences.debug("Type = freeline\n", Preferences.DEBUG_FILEIO);
            	break;
            case 5:
            	Preferences.debug("Type = polyline\n", Preferences.DEBUG_FILEIO);
            	break;
            case 6:
            	Preferences.debug("Type = noRoi\n", Preferences.DEBUG_FILEIO);
            	break;
            case 7:
            	Preferences.debug("Type = freehand\n", Preferences.DEBUG_FILEIO);
            	break;
            case 8:
            	Preferences.debug("Type = traced\n", Preferences.DEBUG_FILEIO);
            	break;
            case 9:
            	Preferences.debug("Type = angle\n", Preferences.DEBUG_FILEIO);
            	break;
            case 10:
            	Preferences.debug("Type = point\n", Preferences.DEBUG_FILEIO);
            	break;
            default:
            	Preferences.debug("Type has an illegal value = " + type + "\n", Preferences.DEBUG_FILEIO);
            }
            int subtype = getBufferShort(data, SUBTYPE, bigEndian);
            switch(subtype) {
            case 1:
            	Preferences.debug("Subtype = TEXT\n", Preferences.DEBUG_FILEIO);
            	break;
            case 2:
            	Preferences.debug("Subtye = ARROW\n", Preferences.DEBUG_FILEIO);
            	break;
            case 3:
            	Preferences.debug("Subtype = ELLIPSE\n", Preferences.DEBUG_FILEIO);
            	break;
            case 4:
            	Preferences.debug("Subtype = IMAGE\n", Preferences.DEBUG_FILEIO);
            	break;
            case 5:
            	Preferences.debug("Subtype = ROTATED_RECT\n", Preferences.DEBUG_FILEIO);
            	break;
            }
            int top = getBufferShort(data, TOP, bigEndian);
            Preferences.debug("Top = " + top + "\n", Preferences.DEBUG_FILEIO);
            int left = getBufferShort(data, LEFT, bigEndian);
            Preferences.debug("Left = " + left + "\n", Preferences.DEBUG_FILEIO);
            int bottom = getBufferShort(data, BOTTOM, bigEndian);
            Preferences.debug("Bottom = " + bottom + "\n", Preferences.DEBUG_FILEIO);
            int right = getBufferShort(data, RIGHT, bigEndian);
            Preferences.debug("Right = " + right + "\n", Preferences.DEBUG_FILEIO);
            int width = right - left;
            Preferences.debug("Width = " + width + "\n", Preferences.DEBUG_FILEIO);
            int height = bottom - top;
            Preferences.debug("Height = " + height + "\n", Preferences.DEBUG_FILEIO);
            int n = getBufferShort(data, N_COORDINATES, bigEndian);
            Preferences.debug("Number coordinates = " + n + "\n", Preferences.DEBUG_FILEIO);
            int options = getBufferShort(data, OPTIONS, bigEndian);
            Preferences.debug("Options = " + options + "\n", Preferences.DEBUG_FILEIO);
            int hdr2Offset = getBufferShort(data, HEADER2_OFFSET, bigEndian);
            Preferences.debug("Header 2 offset = " + hdr2Offset + "\n", Preferences.DEBUG_FILEIO);
            int channel = 0;
            int slice;
            int frame = 0;
            int overlayLabelColor = 0;
            int overlayFontSize = 0;
            int imageOpacity = 0;
            int imageSize = 0;
            if (nDims > 2) {
            	slice = -1;
            }
            else {
            	slice = 0;
            }
            boolean subPixelResolution = ((options & SUB_PIXEL_RESOLUTION) != 0) && version >= 222;
            if (subPixelResolution) {
            	Preferences.debug("Subpixel resolution present\n", Preferences.DEBUG_FILEIO);
            }
            boolean drawOffset = subPixelResolution && ((options & DRAW_OFFSET) != 0);
            if (drawOffset) {
            	Preferences.debug("Draw offset present\n", Preferences.DEBUG_FILEIO);
            }
            boolean subPixelRect = version >= 223 && subPixelResolution && ((type == rect)|| (type == oval));
            if (subPixelRect) {
            	Preferences.debug("Subpixel rect present\n", Preferences.DEBUG_FILEIO);
            }
            float xd = 0.0f;
            float yd = 0.0f;
            float widthd = 0.0f;
            float heightd = 0.0f;
            if(subPixelRect) {
            	xd = getBufferFloat(data, XD, bigEndian);
            	Preferences.debug("xd = " + xd + "\n", Preferences.DEBUG_FILEIO);
            	yd = getBufferFloat(data, YD, bigEndian);
            	Preferences.debug("yd = " + yd + "\n", Preferences.DEBUG_FILEIO);
            	widthd = getBufferFloat(data, WIDTHD, bigEndian);
            	Preferences.debug("widthd = " + widthd + "\n", Preferences.DEBUG_FILEIO);
            	heightd = getBufferFloat(data, HEIGHTD, bigEndian);
            	Preferences.debug("heightd = " + heightd + "\n", Preferences.DEBUG_FILEIO);
            } // if (subPixelRect)
            
            roiName = fileName;
            if ((hdr2Offset > 0) && (hdr2Offset + IMAGE_SIZE + 4 <= size)) {
                channel = getBufferInt(data, hdr2Offset + C_POSITION, bigEndian);
                Preferences.debug("Channel = " + channel + "\n", Preferences.DEBUG_FILEIO);
                slice = getBufferInt(data, hdr2Offset + Z_POSITION, bigEndian);
                Preferences.debug("Slice = " + slice + "\n", Preferences.DEBUG_FILEIO);
                frame = getBufferInt(data, hdr2Offset + T_POSITION, bigEndian);
                Preferences.debug("frame = " + frame + "\n", Preferences.DEBUG_FILEIO);
                overlayLabelColor = getBufferInt(data, hdr2Offset + OVERLAY_LABEL_COLOR, bigEndian);
                Preferences.debug("Overlay label color = " + overlayLabelColor + "\n", Preferences.DEBUG_FILEIO);
                overlayFontSize = getBufferShort(data, hdr2Offset+OVERLAY_FONT_SIZE, bigEndian);
                Preferences.debug("Overlay font size = " + overlayFontSize + "\n", Preferences.DEBUG_FILEIO);
                imageOpacity = getByte(data, hdr2Offset + IMAGE_OPACITY);
                Preferences.debug("Image opacity = " + imageOpacity + "\n", Preferences.DEBUG_FILEIO);
                imageSize = getBufferInt(data, hdr2Offset + IMAGE_SIZE, bigEndian);
                Preferences.debug("Image size = " + imageSize + "\n", Preferences.DEBUG_FILEIO);
                roiNameOffset = getBufferInt(data, hdr2Offset+NAME_OFFSET, bigEndian);
                Preferences.debug("Roi name offset = " + roiNameOffset + "\n", Preferences.DEBUG_FILEIO);
                roiNameLength = getBufferInt(data, hdr2Offset + NAME_LENGTH, bigEndian);
                Preferences.debug("Roi name length = " + roiNameLength + "\n", Preferences.DEBUG_FILEIO);
                if ((roiNameOffset > 0) && (roiNameLength > 0) && (roiNameOffset + 2*roiNameLength < size)) {
                    roiCharName = new char[roiNameLength];
                    for (i = 0; i < roiNameLength; i++) {
                    	roiCharName[i] = (char)getBufferShort(data, roiNameOffset+2*i, bigEndian);
                    }
                    roiName = new String(roiCharName);
                    Preferences.debug("Roi name = " + roiName + "\n", Preferences.DEBUG_FILEIO);
                }
            } // if ((hdr2Offset > 0) && (hdr2Offset + IMAGE_SIZE + 4 <= size))
            else {
            	// Must obtain slice from fileName
            	boolean isDigit = true;
            	int index = fileName.indexOf("-");
            	for (i = 0; i < index; i++) {
            	    char num = fileName.charAt(i);
                    if ((num < '0') || (num > '9')) {
                        isDigit = false;	
                    }
            	}
            	
            	if ((index >= 1) && (index <= 4) && isDigit) {
            		sliceNumber = fileName.substring(0, index);
            		try {
            	        slice = Integer.valueOf(sliceNumber).intValue();
            	        Preferences.debug("Slice = " + slice + " obtained from first number in file name\n", Preferences.DEBUG_FILEIO);
            		}
            		catch (NumberFormatException e) {
            			Preferences.debug("NumberFormatException in trying to obtain slice from roi file name first characters\n",
            					Preferences.DEBUG_FILEIO);
            			slice = -1;
            		}
            		if ((slice < 0) || (slice > zDim-1)) {
            			Preferences.debug("Slice number obtained from file is not in 0 to zDim-1 range\n",
            					Preferences.DEBUG_FILEIO);
            			slice = -1;
            		}
            		if (slice == -1) {
            			Preferences.debug("No slice number information so using slice = 0\n",
            					Preferences.DEBUG_FILEIO);
            			slice = 0;
            		}
            		else {
            			
            		}
            		int index2 = fileName.indexOf("-", index+1);
            		if ((index2 >= index + 2) && (index2 <= index + 5)) {
            			String yCenterString = fileName.substring(index+1, index2);
            			try {
            			    yCenter = Integer.valueOf(yCenterString).intValue();    	
            			}
            			catch (NumberFormatException e) {
                			Preferences.debug("NumberFormatException in trying to obtain yCenter from roi file name\n",
                					Preferences.DEBUG_FILEIO);
                			yCenter = -1;
                		}
            		}
            		if ((yCenter > top) && (yCenter < bottom)) {
            			Preferences.debug("yCenter = " + yCenter + " obtained from second number in file name\n");
            		}
            		else {
            			yCenter = -1;
            		}
            		int index3 = fileName.indexOf(".",index2+1);
            		if ((index3 >= index2 + 2) && (index2 <= index2 + 5)) {
            			String xCenterString = extractNumber(fileName.substring(index2+1, index3));
            			try {
            			    xCenter = Integer.valueOf(xCenterString).intValue();    	
            			}
            			catch (NumberFormatException e) {
                			Preferences.debug("NumberFormatException in trying to obtain xCenter from roi file name\n",
                					Preferences.DEBUG_FILEIO);
                			xCenter = -1;
                		}
            		}
            		if ((xCenter > left) && (xCenter < right)) {
            			Preferences.debug("xCenter = " + xCenter + " obtained from third number in file name\n");
            		}
            		else {
            			xCenter = -1;
            		}
            	} // if ((index >= 1) && (index <= 4) && isDigit)
            	else {
            		index = -1;
        			for (i = 0; (i < fileName.length()-2) && (index == -1); i++) {
        				if ((fileName.substring(i,i+1).equals("-")) && (fileName.substring(i+1,i+2).equalsIgnoreCase("S"))) {
        					index = i+2;
        				}
        			}
        			if (index == -1) {
        				for (i = 0; (i < fileName.length()-3) && (index == -1); i++) {
            				if ((fileName.substring(i,i+1).equals("-")) && (fileName.substring(i+1,i+2).equals(" ")) &&
            								(fileName.substring(i+2, i+3).equalsIgnoreCase("S"))) {
            					index = i+3;
            				}
            			}	
        			}
        			if (index != -1) {
	        			int index2 = -1;
	        			for (i = index; i < fileName.length() && (index2 == -1); i++) {
	        				if ((fileName.substring(i,i+1).equals("-")) || (fileName.substring(i,i+1).equals("."))) {
	        					index2 = i;
	        				}
	        			}
	        			isDigit = true;
	        			for (i = index; i < index2; i++) {
	        				char num = fileName.charAt(i);
	        				if ((num < '0') || (num > '9')) {
	        					isDigit = false;
	        				}
	        			}
	        			if (isDigit) {
	        			    sliceNumber = fileName.substring(index, index2);
		        			try {
		        				slice = Integer.valueOf(sliceNumber).intValue();	
		        				Preferences.debug("Slice = " + slice + " obtained from numbers after -S or - S in file name\n", 
		        						Preferences.DEBUG_FILEIO);
		        			}
		        			catch (NumberFormatException er) {
		            			Preferences.debug("NumberFormatException in trying to obtain slice from roi file name after -s\n",
		            					Preferences.DEBUG_FILEIO);
		        			}
	        			}
        			} // if (index != -1)
            	}
            }
            if (slice == -1) {
            	return;
            }
            
            boolean isComposite = (getBufferInt(data, SHAPE_ROI_SIZE, bigEndian) > 0);
            if (isComposite) {
            	Preferences.debug("Is composite\n", Preferences.DEBUG_FILEIO);
            	if (type != rect) {
            		MipavUtil.displayError("Invalid composite ROI type");
            		return;
            	}
            	ns = getBufferInt(data, SHAPE_ROI_SIZE, bigEndian);
            	shapeArray = new float[ns];
            	// handle the actual data: data are stored segment-wise, i.e.,
        		// the type of the segment followed by 0-6 control point coordinates.
            	for (i = 0; i < ns; i++) {
            		shapeArray[i] = getBufferFloat(data, COORDINATES + 4*i, bigEndian);
            	}
            	Vector<Vector<Integer>>xVector = new Vector<Vector<Integer>>();
            	Vector<Vector<Integer>>yVector = new Vector<Vector<Integer>>();
            	makeShapeFromArray(shapeArray, xVector, yVector);
            	for (i = 0; i < xVector.size(); i++) {
	            	int x[] = new int[xVector.get(i).size()];
	            	int y[] = new int[yVector.get(i).size()];
	            	for (j = 0; j < xVector.get(i).size(); j++) {
	            		x[j] = xVector.get(i).get(j);
	            		y[j] = yVector.get(i).get(j);
	            	}
	            	currentManager.createPolygonVOI(x, y, xVector.get(i).size(), slice);
            	} // for (i = 0; i < xVector.size(); i++)
            	return;
            } // if (isComposite)
            
            switch(type) {
            case oval:
            	if (subPixelRect) {
            	    //new OvalRoi(xd, yd, widthd, heightd);	
            	}
            	else {
            		//new OvalRoi(left, top, width, height);
            		currentManager.createOvalVOI(left, top, width, height, slice);
            	}
            	break;
            case polygon: case freehand: case traced: case polyline: case freeline: case angle: case point:
				//IJ.log("type: "+type);
				//IJ.log("n: "+n);
				//IJ.log("rect: "+left+","+top+" "+width+" "+height);
				if (n==0 || n<0) break;
				int[] x = new int[n];
				int[] y = new int[n];
				float[] xf = null;
				float[] yf = null;
				int base1 = COORDINATES;
				int base2 = base1+2*n;
				int xtmp, ytmp;
				for (i=0; i<n; i++) {
					xtmp = getBufferShort(data,base1+i*2,bigEndian);
					if (xtmp<0) xtmp = 0;
					ytmp = getBufferShort(data,base2+i*2,bigEndian);
					if (ytmp<0) ytmp = 0;
					x[i] = left+xtmp;
					y[i] = top+ytmp;
				}
				if (subPixelResolution) {
					xf = new float[n];
					yf = new float[n];
					base1 = COORDINATES+4*n;
					base2 = base1+4*n;
					for (i=0; i<n; i++) {
						xf[i] = getBufferFloat(data,base1+i*4,bigEndian);
						yf[i] = getBufferFloat(data,base2+i*4,bigEndian);
					}
				}
				if (type==point) {
					if (subPixelResolution) {
						//roi = new PointRoi(xf, yf, n);
					}
					else {
						//roi = new PointRoi(x, y, n);
					    currentManager.createPointVOI(x, y, n, slice);
					}
					if (version>=226) {
						//((PointRoi)roi).setPointType(getByte(POINT_TYPE));
						//((PointRoi)roi).setSize(getShort(STROKE_WIDTH));
					}
					//((PointRoi)roi).setShowLabels(!ij.Prefs.noPointLabels);
					break;
				}
				int roiType;
				if (type==polygon) {
					//roiType = Roi.POLYGON;
				}
				else if (type==freehand) {
					//roiType = Roi.FREEROI;
					if (subtype==ELLIPSE || subtype==ROTATED_RECT) {
						double ex1 = getBufferFloat(data,X1,bigEndian);		
						double ey1 = getBufferFloat(data,Y1,bigEndian);		
						double ex2 = getBufferFloat(data,X2,bigEndian);		
						double ey2 = getBufferFloat(data,Y2,bigEndian);
						double param = getBufferFloat(data,FLOAT_PARAM,bigEndian);
						if (subtype==ROTATED_RECT) {
							//roi = new RotatedRectRoi(ex1,ey1,ex2,ey2,param);
						}
						else {
							//roi = new EllipseRoi(ex1,ey1,ex2,ey2,param);
						}
						break;
					}
				} else if (type==traced) {
					//roiType = Roi.TRACED_ROI;
				}
				else if (type==polyline) {
					//roiType = Roi.POLYLINE;
				}
				else if (type==freeline) {
					//roiType = Roi.FREELINE;
				}
				else if (type==angle) {
					//roiType = Roi.ANGLE;
				}
				else {
					//roiType = Roi.FREEROI;
				}
				if (subPixelResolution) {
					//roi = new PolygonRoi(xf, yf, n, roiType);
					//roi.setDrawOffset(drawOffset);
				} else
					//roi = new PolygonRoi(x, y, n, roiType);
				    currentManager.createPolygonVOI(x, y, n, slice);
				break;
		default:
			Preferences.debug("Unrecognized ROI type: "+type + "\n", Preferences.DEBUG_FILEIO);
            }
            	
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
    
    
   
	
	private void makeShapeFromArray(float array[], Vector<Vector<Integer>> xVector, Vector<Vector<Integer>> yVector) {
		if (array == null) return;
		Shape s = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
		int index=0, type, len;
		float[] seg = new float[7];
		int moveToX = 0;
		int moveToY = 0;
		int vecIndex = -1;
		while (true) {
			len = getSegment(array, seg, index);
			if (len<0) break;
			index += len;
			type = (int)seg[0];
			switch(type) {
				case PathIterator.SEG_MOVETO:
					((GeneralPath)s).moveTo(seg[1], seg[2]);
					moveToX = (int)Math.round(seg[1]);
					moveToY = (int)Math.round(seg[2]);
					vecIndex++;
					xVector.add(new Vector<Integer>());
					yVector.add(new Vector<Integer>());
					xVector.get(vecIndex).add(moveToX);
					yVector.get(vecIndex).add(moveToY);
					break;
				case PathIterator.SEG_LINETO:
					((GeneralPath)s).lineTo(seg[1], seg[2]);
					xVector.get(vecIndex).add((int)Math.round(seg[1]));
					yVector.get(vecIndex).add((int)Math.round(seg[2]));
					break;
				case PathIterator.SEG_QUADTO:
					((GeneralPath)s).quadTo(seg[1], seg[2],seg[3], seg[4]);
					//P(t) = B(2,0)*CP + B(2,1)*P1 + B(2,2)*P2
					//0 <= t <= 1

				    //B(n,m) = mth coefficient of nth degree Bernstein polynomial
					//       = C(n,m) * t^(m) * (1 - t)^(n-m)
					// C(n,m) = Combinations of n things, taken m at a time
					// = n! / (m! * (n-m)!)
					break;
				case PathIterator.SEG_CUBICTO:
					((GeneralPath)s).curveTo(seg[1], seg[2], seg[3], seg[4], seg[5], seg[6]);
					// P(t) = B(3,0)*CP + B(3,1)*P1 + B(3,2)*P2 + B(3,3)*P3
					// 0 <= t <= 1

				    // B(n,m) = mth coefficient of nth degree Bernstein polynomial
					//        = C(n,m) * t^(m) * (1 - t)^(n-m)
				    // C(n,m) = Combinations of n things, taken m at a time
					//        = n! / (m! * (n-m)!)
					break;
				case PathIterator.SEG_CLOSE:
					((GeneralPath)s).closePath();
					xVector.get(vecIndex).add(moveToX);
					yVector.get(vecIndex).add(moveToY);
					break;
				default: break;
			}
		}
		return;
	}
    
    private int getSegment(float[] array, float[] seg, int index) {
		int len = array.length;
		if (index>=len) return -1; seg[0]=array[index++];
		int type = (int)seg[0];
		if (type==PathIterator.SEG_CLOSE) return 1;
		if (index>=len) return -1; seg[1]=array[index++];
		if (index>=len) return -1; seg[2]=array[index++];
		if (type==PathIterator.SEG_MOVETO||type==PathIterator.SEG_LINETO) return 3;
		if (index>=len) return -1; seg[3]=array[index++];
		if (index>=len) return -1; seg[4]=array[index++];
		if (type==PathIterator.SEG_QUADTO) return 5;
		if (index>=len) return -1; seg[5]=array[index++];
		if (index>=len) return -1; seg[6]=array[index++];
		if (type==PathIterator.SEG_CUBICTO) return 7;
		return -1;
	}
    
    public String extractNumber(final String str) {                

        if(str == null || str.isEmpty()) return "";

        StringBuilder sb = new StringBuilder();
        boolean found = false;
        for(char c : str.toCharArray()){
            if(Character.isDigit(c)){
                sb.append(c);
                found = true;
            } else if(found){
                // If we already found a digit before and this char is not a digit, stop looping
                break;                
            }
        }

        return sb.toString();
    }
    
    
    /**
     * Converts byte data to short data.
     * 
     * @param buffer Array of byte data.
     * @param index Index into array data.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return Short value extracted from byte array.
     */
    public final short getBufferShort(final byte[] buffer, final int index, final boolean bigEndian) {

        if (bigEndian) {
            return (short) ( ( (buffer[index] & 0xff) << 8) | (buffer[index + 1] & 0xff));
        } else {
            return (short) ( ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }
    
    public final short getByte(final byte[] buffer, final int index) {
    	return (short) (buffer[index] & 255);
    }
    
    /**
     * Converts byte data to int data.
     * 
     * @param buffer Array of byte data.
     * @param index Index into array data.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return Integer value extracted from byte array.
     */
    public final int getBufferInt(final byte[] buffer, final int index, final boolean bigEndian) {

        if (bigEndian) {
            return ( ( (buffer[index] & 0xff) << 24) | ( (buffer[index + 1] & 0xff) << 16)
                    | ( (buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));
        } else {
            return ( ( (buffer[index + 3] & 0xff) << 24) | ( (buffer[index + 2] & 0xff) << 16)
                    | ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    
    /**
     * Converts byte data to float data.
     * 
     * @param buffer Array of byte data.
     * @param index Index into array data.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return Float value extracted from byte array.
     */
    public final float getBufferFloat(final byte[] buffer, final int index, final boolean bigEndian) {
        int tmpInt;

        if (bigEndian) {
            tmpInt = ( ( (buffer[index] & 0xff) << 24) | ( (buffer[index + 1] & 0xff) << 16)
                    | ( (buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = ( ( (buffer[index + 3] & 0xff) << 24) | ( (buffer[index + 2] & 0xff) << 16)
                    | ( (buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    
	private void readCSV( String fileName, ModelImage image )
	{
		File file = new File(fileName);
		if ( file.exists() )
		{			
            short sID = (short)(image.getVOIs().getUniqueID());
			//        	System.err.println( fileName );
			FileReader fr;
			try {
				fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String line = br.readLine();
				line = line.toLowerCase();
				boolean contourType = false;
				if ( line.contains("contour") ) {
					line = br.readLine();
					contourType = true;
				}
				line = br.readLine();

				VOI annotationVOI;
				VOIContour contour = null;
				if ( !contourType ) {
					annotationVOI = new VOI( (short)sID, fileName, VOI.ANNOTATION, 0 );
				}
				else {
					annotationVOI = new VOI( (short)sID, fileName, VOI.CONTOUR, 0 );
					contour = new VOIContour(false);
					annotationVOI.getCurves().add(contour);
				}
				int count = 1;
				while ( line != null )
				{
					String[] parsed = line.split( "," );
					if ( parsed.length != 0 )
					{
						VOIText text = new VOIText();
						float x, y, z, r, g, b;
						if ( parsed.length > 6 )
						{
							// name, position and color
							int parsedIndex = 0;
							String name = String.valueOf( parsed[parsedIndex++] );
							text.setText(name);
							x    = (parsed.length > parsedIndex+0) ? (parsed[parsedIndex+0].length() > 0) ? Float.valueOf( parsed[parsedIndex+0] ) : 0 : 0; 
							y    = (parsed.length > parsedIndex+1) ? (parsed[parsedIndex+1].length() > 0) ? Float.valueOf( parsed[parsedIndex+1] ) : 0 : 0; 
							z    = (parsed.length > parsedIndex+2) ? (parsed[parsedIndex+2].length() > 0) ? Float.valueOf( parsed[parsedIndex+2] ) : 0 : 0;
							r    = (parsed.length > parsedIndex+3) ? (parsed[parsedIndex+3].length() > 0) ? Float.valueOf( parsed[parsedIndex+3] ) : 1 : 1;
							g    = (parsed.length > parsedIndex+4) ? (parsed[parsedIndex+4].length() > 0) ? Float.valueOf( parsed[parsedIndex+4] ) : 1 : 1;
							b    = (parsed.length > parsedIndex+5) ? (parsed[parsedIndex+5].length() > 0) ? Float.valueOf( parsed[parsedIndex+5] ) : 1 : 1;
//							System.err.println( name + " " + x + " " + y + " " + z + " " + r );
							text.add( new Vector3f( x, y, z ) );
							text.add( new Vector3f( x+1, y, z ) );
							text.setColor( new Color(r/255f,g/255f,b/255f) );
							annotationVOI.getCurves().add(text);
						}
						else if ( parsed.length >= 4 )
						{
							// name, position and radius:
							int parsedIndex = 0;
							String name = String.valueOf( parsed[parsedIndex++] );
							text.setText(name);
							x    = (parsed.length > parsedIndex+0) ? (parsed[parsedIndex+0].length() > 0) ? Float.valueOf( parsed[parsedIndex+0] ) : 0 : 0; 
							y    = (parsed.length > parsedIndex+1) ? (parsed[parsedIndex+1].length() > 0) ? Float.valueOf( parsed[parsedIndex+1] ) : 0 : 0; 
							z    = (parsed.length > parsedIndex+2) ? (parsed[parsedIndex+2].length() > 0) ? Float.valueOf( parsed[parsedIndex+2] ) : 0 : 0;
							r    = (parsed.length > parsedIndex+3) ? (parsed[parsedIndex+3].length() > 0) ? Float.valueOf( parsed[parsedIndex+3] ) : 1 : 1;
//							System.err.println( name + " " + x + " " + y + " " + z + " " + r );
							text.add( new Vector3f( x, y, z ) );
							text.add( new Vector3f( x+r, y, z ) );
							annotationVOI.getCurves().add(text);
						}
						else if ( parsed.length == 3 )
						{
							if ( contourType ) {
								x    = (parsed.length > 0) ? (parsed[0].length() > 0) ? Float.valueOf( parsed[0] ) : 0 : 0; 
								y    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Float.valueOf( parsed[1] ) : 0 : 0; 
								z    = (parsed.length > 2) ? (parsed[2].length() > 0) ? Float.valueOf( parsed[2] ) : 0 : 0;
								contour.add( new Vector3f(x,y,z) );
							}
							else {
								// z, x, y order - Ruida's request
								z    = (parsed.length > 0) ? (parsed[0].length() > 0) ? Float.valueOf( parsed[0] ) : 0 : 0; 
								x    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Float.valueOf( parsed[1] ) : 0 : 0; 
								y    = (parsed.length > 2) ? (parsed[2].length() > 0) ? Float.valueOf( parsed[2] ) : 0 : 0; 
								text.setText( "" );
								text.add( new Vector3f( x, y, z ) );
								text.add( new Vector3f( x+1, y, z ) );
								annotationVOI.getCurves().add(text);
							}
						}
						count++;
					}
					line = br.readLine();
				}
				fr.close();
				if ( count > 1 )
				{
//					if ( !contourType )
//						m_kParent.addSphereVOIs( annotationVOI );
//					else 
						image.registerVOI(annotationVOI);
				}
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

    
    
}
