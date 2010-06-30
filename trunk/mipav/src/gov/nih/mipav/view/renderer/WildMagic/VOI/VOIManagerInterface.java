package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionMaskToPaint;
import gov.nih.mipav.model.scripting.actions.ActionMaskToVOI;
import gov.nih.mipav.model.scripting.actions.ActionOpenAllVOIs;
import gov.nih.mipav.model.scripting.actions.ActionOpenVOI;
import gov.nih.mipav.model.scripting.actions.ActionSaveAllVOIs;
import gov.nih.mipav.model.scripting.actions.ActionVOIToMask;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
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
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJPopupPt;
import gov.nih.mipav.view.ViewJPopupVOI;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewOpenVOIUI;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogAGVF;
import gov.nih.mipav.view.dialogs.JDialogBSmooth;
import gov.nih.mipav.view.dialogs.JDialogBSnake;
import gov.nih.mipav.view.dialogs.JDialogFlip;
import gov.nih.mipav.view.dialogs.JDialogGVF;
import gov.nih.mipav.view.dialogs.JDialogIntensityThreshold;
import gov.nih.mipav.view.dialogs.JDialogLivewire;
import gov.nih.mipav.view.dialogs.JDialogMask;
import gov.nih.mipav.view.dialogs.JDialogOpacityControls;
import gov.nih.mipav.view.dialogs.JDialogPointArea;
import gov.nih.mipav.view.dialogs.JDialogSnake;
import gov.nih.mipav.view.dialogs.JDialogTrim;
import gov.nih.mipav.view.dialogs.JDialogVOIShapeInterpolation;
import gov.nih.mipav.view.dialogs.JDialogVOIStatistics;
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
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
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

    private String voiSavedFileName = null;
    
    private JDialogVOIStatistics imageStatList; 

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
        else if (command.equals("Open VOI")) {
            final boolean success = openVOI(false, false);

            if (success) {
                ScriptRecorder.getReference().addLine(new ActionOpenVOI(getActiveImage()));
                ProvenanceRecorder.getReference().addLine(new ActionOpenVOI(getActiveImage()));
            }
        } 
        else if (command.equals("Open all VOIs")) {
            loadAllVOIs(false);

            ScriptRecorder.getReference().addLine(new ActionOpenAllVOIs(getActiveImage()));
            ProvenanceRecorder.getReference().addLine(new ActionOpenAllVOIs(getActiveImage()));
        } 
        else if (command.equals("Open all VOIs from...")) {

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
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName + File.separator);
                loadAllVOIsFrom(voiDir, false);
            }
        } 
        else if (command.equals("Open labels")) {
            openVOI(false, true);
        } 
        else if (command.equals("SaveSelectedContours")) {
            saveVOI(false);
        } 
        else if (command.equals("SaveSelectedContoursAs")) {
            saveVOIAs(false);
        } 
        else if (command.equals("Save VOI")) {
            saveVOI(true);
        } 
        else if (command.equals("Save VOI as")) {
            saveVOIAs(true);
        } 
        else if (command.equals("Save all VOIs")) {
            saveAllVOIs();

            ScriptRecorder.getReference().addLine(new ActionSaveAllVOIs(getActiveImage()));
            ProvenanceRecorder.getReference().addLine(new ActionSaveAllVOIs(getActiveImage()));
        } 
        else if (command.equals("Save all VOIs to...")) {

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

            final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName + File.separator);
                saveAllVOIsTo(voiDir);

                ScriptRecorder.getReference().addLine(new ActionSaveAllVOIs(getActiveImage(), voiDir));
                ProvenanceRecorder.getReference().addLine(new ActionSaveAllVOIs(getActiveImage(), voiDir));
            }
        } 
        else if (command.equals("SaveVOIIntensities")) {
            saveVOIIntensities();
        } 
        else if (command.equals("SaveSelectedAnnotation")) {
            saveLabels(false);
        } 
        else if (command.equals("SaveAllAnnotations")) {
            saveLabels(true);
        } 
        else if (command.equals("XOR")) {
            System.err.println( event.getSource() );
            //ViewUserInterface.getReference().setUseVOIXOR(menuBuilder.isMenuItemSelected("Allow VOI holes (XOR)"));
        } 
        else if (command.equals("PaintMask")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            getActiveImage().setMask(getActiveImage().generateVOIMask(Preferences.is(Preferences.PREF_USE_VOI_XOR), true));
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
                maskImage = getActiveImage().generateBinaryImage(Preferences.is(Preferences.PREF_USE_VOI_XOR), false);
               
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
                shortImage = getActiveImage().generateShortImage(1, Preferences.is(Preferences.PREF_USE_VOI_XOR), false);

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
                uByteImage = getActiveImage().generateUnsignedByteImage(1, Preferences.is(Preferences.PREF_USE_VOI_XOR), false);

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

                maskImage = getActiveImage().generateBinaryImage(Preferences.is(Preferences.PREF_USE_VOI_XOR), true);

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

                shortImage = getActiveImage().generateShortImage(1, Preferences.is(Preferences.PREF_USE_VOI_XOR), true);

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

                uByteImage = getActiveImage().generateUnsignedByteImage(1, Preferences.is(Preferences.PREF_USE_VOI_XOR), true);

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
            final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(getActiveImage());

            ViewJProgressBar progressBar = new ViewJProgressBar(getActiveImage().getImageName(), "Extracting VOI ...", 0, 100, true);
            progressBar.setSeparateThread(false);
            VOIExtractionAlgo.addProgressChangeListener(progressBar);
            VOIExtractionAlgo.setProgressValues(0, 100);

            // VOIExtractionAlgo.setActiveImage(false);
            VOIExtractionAlgo.run();

            ScriptRecorder.getReference().addLine(new ActionMaskToVOI(getActiveImage()));
            ProvenanceRecorder.getReference().addLine(new ActionMaskToVOI(getActiveImage()));
            updateDisplay();
        } else if (command.equals("MaskToPaint")) {
            MipavUtil.displayWarning("MaskToPaint not current implemented for the TriPlanar View.");
/*
            // TODO: only runs with an imageB mask, not if imageA is a mask itself.
            final boolean success = handleMaskToPaint(true);

            if (success) {
                ScriptRecorder.getReference().addLine(new ActionMaskToPaint(getActiveImage()));
                ProvenanceRecorder.getReference().addLine(new ActionMaskToPaint(getActiveImage()));
            }
            */
        }
        else if (command.equals("Snake")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            new JDialogSnake(m_kParent.getFrame(), getActiveImage());
        } else if (command.equals("AGVF")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            new JDialogAGVF(m_kParent.getFrame(), getActiveImage());
        } else if (command.equals("GVF")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            new JDialogGVF(m_kParent.getFrame(), getActiveImage());

        } else if (command.equals("BSnake")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            new JDialogBSnake(m_kParent.getFrame(), getActiveImage());
        } else if (command.equals("SmoothVOI")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            new JDialogBSmooth(m_kParent.getFrame(), getActiveImage(), getSlice());
        } // Paint
        else if (command.equals("VOIFlipY")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }

            final JDialogFlip flip = new JDialogFlip(m_kParent.getFrame(), getActiveImage(), AlgorithmFlip.Y_AXIS,
                    AlgorithmFlip.VOI_TYPE);

            flip.callAlgorithm();
        } else if (command.equals("VOIFlipX")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            final JDialogFlip flip = new JDialogFlip(m_kParent.getFrame(), getActiveImage(), AlgorithmFlip.X_AXIS,
                    AlgorithmFlip.VOI_TYPE);

            flip.callAlgorithm();
        } else if (command.equals("VOIFlipZ")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select a VOI!");
                return;
            }
            final JDialogFlip flip = new JDialogFlip(m_kParent.getFrame(), getActiveImage(), AlgorithmFlip.Z_AXIS,
                    AlgorithmFlip.VOI_TYPE);

            flip.callAlgorithm();
        } else if (command.equals("interpolateVOIs")) {
            // dialog that is not visible...calls the algorithm immediately
            final JDialogVOIShapeInterpolation dialogVOIShapeInterp = new JDialogVOIShapeInterpolation(getActiveImage());
        } 
        else if (command.equals("Trim")) {
            final JDialogTrim trimSettings = new JDialogTrim(m_kParent.getFrame(), getActiveImage());

            trimSettings.setVisible(true);
        }
        else if (command.equals("OpenNewGraph")) {
            new ViewJFrameGraph("Graph", true);
        } 
        else if (command.equals("boundaryIntensity")) {
            graphVOI();
        } 
        else if (command.equals("totalIntensity")) {
            graph25VOI_CalcInten(true, false, 0);
        } 
        else if (command.equals("avgIntensity")) {
            graph25VOI_CalcInten(false, false, 0);
        } 
        else if (event.getActionCommand().equals("totalIntensityThreshold")) {
            new JDialogIntensityThreshold(getFrame(), this,
                                          false);
        } else if (event.getActionCommand().equals("avgIntensityThreshold")) {
            new JDialogIntensityThreshold(getFrame(), this,
                                          true);
        }
        else if (command.equals("VOIStatistics")) {
            showStatisticsCalculator();
        } 
        else {
            doVOI(command);
        }

    }


    public void addVOI( VOIBase kNew, boolean bQuickLUT, boolean bUpdate, boolean isFinished )
    {
        ModelImage kActive = m_kParent.getActiveImage();
        if ( kActive != null )
        {
            addVOI( kActive, kNew, bQuickLUT, bUpdate, isFinished );
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
        if ( m_bGPURenderer )
        {
            createMask( getActiveImage(), command.equals(CustomUIBuilder.PARAM_VOI_QUICK_AND_OP.getActionCommand()) );
        }
        else
        {
            new JDialogMask(getActiveImage(), false, command.equals(CustomUIBuilder.PARAM_VOI_QUICK_NOT_OP.getActionCommand()));
        }
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
                m_kParent.getActiveImage().unregisterVOI(kGroup);
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
            //toolbarBuilder.getVOIColorButton().setBackground( Color.yellow );
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
        else if (kCommand.equals("voiSelectNone")) {
            selectAllVOIs(false);
        } 
        else if (kCommand.equals("GroupVOIs")) {
            if ( !checkForActiveVOIs()) {
                MipavUtil.displayWarning("Please select VOIs!");
                return;
            }
            m_kParent.getActiveImage().groupVOIs();
            fireVOISelectionChange(null);
        } else if (kCommand.equals("UngroupVOIs")) {
            m_kParent.getActiveImage().ungroupVOIs();
            fireVOISelectionChange(null);
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
        else if (kCommand.equals("BringToFront")) {
            changeVOIOrder(false, VOI.FRONT);
        } 
        else if (kCommand.equals("SendToBack")) {
            changeVOIOrder(false, VOI.BACK);
        } 
        else if (kCommand.equals("BringForward")) {
            changeVOIOrder(false, VOI.FORWARD);
        } 
        else if (kCommand.equals("SendBackward")) {
            changeVOIOrder(false, VOI.BACKWARD);
        } 
        else if (kCommand.equals("BringContourToFront")) {
            changeVOIOrder(true, VOI.FRONT);
        } 
        else if (kCommand.equals("SendContourToBack")) {
            changeVOIOrder(true, VOI.BACK);
        } 
        else if (kCommand.equals("BringContourForward")) {
            changeVOIOrder(true, VOI.FORWARD);
        } 
        else if (kCommand.equals("SendContourBackward")) {
            changeVOIOrder(true, VOI.BACKWARD);
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
        m_kCurrentVOIGroup = null;
        voiUID++;
        toolbarBuilder.getVOIColorButton().setVOIColor(voiUID);
        setButtonColor(toolbarBuilder.getVOIColorButton(), 
                toolbarBuilder.getVOIColorButton().getBackground() );


        short sID = (short)(m_kParent.getActiveImage().getVOIs().size() + 1);
        String kName = new String();
        int index = kName.lastIndexOf('.') + 1;
        kName = kName.substring(index);
        m_kCurrentVOIGroup = new VOI( sID,  kName + "_" + sID );
        m_kCurrentVOIGroup.setOpacity(1f);
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
        kLUT.update();
        Vector3f[] kBounds = kLUT.getImageBoundingBox();
        m_akBounds[0].Copy(kBounds[0]);
        m_akBounds[1].Copy(kBounds[1]);        
        deleteVOI( kLUT );
        if (m_kParent.getActiveImage().isColorImage() == false) {
            quickLUT( m_akBounds, m_kParent.getActiveImage(), m_kParent.getActiveLUT() );
            m_kParent.getActiveImage().notifyImageDisplayListeners(m_kParent.getActiveLUT(), true);            
        } else { // RGB image
            quickRGB( m_akBounds, m_kParent.getActiveImage(), m_kParent.getActiveRGB() );
            m_kParent.getActiveImage().notifyImageDisplayListeners(true, 1, m_kParent.getActiveRGB());
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


    public void saveLabels(final boolean saveAll) {
        String fileName;
        String directory;
        JFileChooser chooser;

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        boolean foundLabel = false;

        ModelImage kImage = m_kParent.getActiveImage();
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
            final File file = new File(ViewUserInterface.getReference().getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
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

            this.voiSavedFileName = directory + fileName;

        } else {
            return;
        }

        try {

            final FileVOI fileVOI = new FileVOI(fileName, directory, kImage);

            fileVOI.writeAnnotationXML(saveAll);

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing labels");
        }

    }    /**
     * This method saves a selected VOI - should this be in VOI structure ??!!!
     * 
     * @param saveAllContours if true all contours are saved
     */
    public void saveVOI(final boolean saveAllContours) {

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        FileVOI fileVOI;
        String extension = ".xml";

        try {

            ModelImage kImage = m_kParent.getActiveImage();
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
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public String saveVOIAs() {
        saveVOIAs(true);

        return this.voiSavedFileName;
    }


    /**
     * This method allows the user to choose how to save the VOI.
     * 
     * @param saveAllContours if true all contours are saved
     */
    public void saveVOIAs(final boolean saveAllContours) {
        String fileName;
        String directory;
        JFileChooser chooser;

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        boolean doPoint = false, doAnnotation = false;


        ModelImage kImage = m_kParent.getActiveImage();
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

        chooser = new JFileChooser();
        chooser.setDialogTitle("Save VOI as");

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            final File file = new File(ViewUserInterface.getReference().getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }
        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".xml"}));

        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            if(!fileName.endsWith(".xml")) {
                MipavUtil.displayError("VOI files must end in .xml");
                return;
            }
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);

            this.voiSavedFileName = directory + fileName;

        } else {
            return;
        }

        try {

            if (fileName.endsWith(".voi") && (VOIs.VOIAt(i).getCurveType() == VOI.POINT)) {
                doPoint = true;
            } else if (fileName.endsWith(".lbl") || VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION) {
                doAnnotation = true;
                if (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION && !fileName.endsWith(".lbl")) {
                    fileName += ".lbl";
                }
            } else if ( !fileName.endsWith(".voi") && !fileName.endsWith(".xml")) {
                fileName += ".xml";
            }

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

    /**
     * Save intensities in VOI to a text file of format x,y,z,intensity on each line if not color or complex. If color
     * use format x,y,z,a,r,g,b on each line and if complex use format x,y,z,real,imaginary on each line.
     * 
     */
    public void saveVOIIntensities() {
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

        selectedImage = m_kParent.getActiveImage();
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
        VOIs.VOIAt(i).createBinaryMask3D(mask, xDim, yDim, selectedImage.getParentFrame().useXOR(), false);
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
            final File file = new File(ViewUserInterface.getReference().getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".txt"}));

        final int returnVal = chooser.showSaveDialog(m_kParent.getFrame());

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);

            this.voiSavedFileName = directory + fileName;

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

    public void saveVOIs( String kCommand )
    {
        m_kUndoCommands.add( kCommand );
        m_kUndoList.add( getVOIState() );
        m_kRedoCommands.clear();
        m_kRedoList.clear();
    }
    /**
     * This method saves all VOIs for the active image to the default VOI directory for that image.
     */
    public void saveAllVOIs() {

        String fileDir;
        String tmpImageName;
        String imageName;
        String voiDir;
        ModelImage img = m_kParent.getActiveImage();
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
     * 
     * @param voiDir directory that contains VOIs for this image.
     */
    public void saveAllVOIsTo(final String voiDir) {
        try {

            ModelImage currentImage = m_kParent.getActiveImage();
            ViewVOIVector VOIs = currentImage.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, currentImage);
                    fileVOI.writeVOI(VOIs.VOIAt(i), true);
                } else {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, currentImage);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(),true);
                }
            }

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }

    } // end saveAllVOIsTo()


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

    public void setSelectedVOI( VOI kSelected, boolean bSelectAll, boolean bUnSelectVOI )
    {
        if ( kSelected == null )
        {
            return;
        }
        if ( bUnSelectVOI && (m_kCurrentVOIGroup != null) && (m_kCurrentVOIGroup != kSelected) )
        {
            m_kCurrentVOIGroup.setAllActive(false);
        }
        m_kCurrentVOIGroup = kSelected;
        if ( bUnSelectVOI || bSelectAll )
        {
            m_kCurrentVOIGroup.setAllActive(bSelectAll);
        }
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
            setSelectedVOI( m_kCurrentVOIGroup, m_kCurrentVOIGroup.isAllActive(), true );
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

    private void addVOI( ModelImage kImage, VOIBase kNew, boolean bQuickLUT, boolean bUpdate, boolean isFinished )
    {       
        if ( kNew.getGroup() == null )
        {
            saveVOIs("addVOI");
            findCompatibleType(kImage, kNew, isFinished);
            if ( m_kCurrentVOIGroup == null )
            {
                short sID = (short)(kImage.getVOIs().size() + 1);
                String kName = kNew.getClass().getName();
                int index = kName.lastIndexOf('.') + 1;
                kName = kName.substring(index);
                m_kCurrentVOIGroup = new VOI( sID,  kName + "_" + sID, kNew.getType(), -1f );
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


    private boolean checkForActiveVOIs() {
        boolean foundActive = false;
        ViewVOIVector VOIs;
        int nVOI;

        VOIs = m_kParent.getActiveImage().getVOIs();
        nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                foundActive = true;
                break;
            }
        }

        return foundActive;
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

    private void findCompatibleType( ModelImage kImage, VOIBase kNew, boolean isFinished)
    {
        if ( m_kCurrentVOIGroup != null && m_kCurrentVOIGroup.getCurveType() == kNew.getType() )
        {
            return;
        }
        if ( m_kCurrentVOIGroup != null && m_kCurrentVOIGroup.isEmpty() )
        {
            short sID = (short)(kImage.getVOIs().size() + 1);
            m_kParent.getActiveImage().unregisterVOI(m_kCurrentVOIGroup);
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
                setSelectedVOI(m_kCurrentVOIGroup, false, true);
                return;
            }
        }
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            if ( matches(kVOIs.get(i).getCurveType(), kNew.getType(), isFinished) )
            {
                m_kCurrentVOIGroup = kVOIs.get(i);
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
     * This method loads all VOIs to the active image from the default VOI directory for that image.
     * 
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    public void loadAllVOIs(boolean quietMode) {
        ModelImage img = m_kParent.getActiveImage();

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
     * 
     * @param voiDir the directory to load voi's from
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    public void loadAllVOIsFrom(final String voiDir, boolean quietMode) {

        int i, j;
        VOI[] VOIs;
        FileVOI fileVOI;
        ModelImage currentImage = m_kParent.getActiveImage();

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

                for (j = 0; j < VOIs.length; j++) {
                    currentImage.registerVOI(VOIs[j]);
                }
            }

            // when everything's done, notify the image listeners
            currentImage.notifyImageDisplayListeners();

        } catch (final Exception error) {

            if ( !quietMode) {
                MipavUtil.displayError("Error loading all VOIs from " + voiDir + ": " + error);
            }
        }

    } // end loadAllVOIsFrom()




    /**
     * This method opens an existing VOI.
     * 
     * @param quietMode if true indicates that warnings should not be displayed.
     * @param doLabels DOCUMENT ME!
     * 
     * @return whether a VOI was successfully opened (ie - the dialog wasn't cancelled)
     */
    public boolean openVOI(boolean quietMode, final boolean doLabels) {
        ViewOpenVOIUI openVOI = null;

        try {
            openVOI = new ViewOpenVOIUI();

            if (openVOI.open(m_kParent.getActiveImage(), doLabels) == null) {
                return false;
            }
        } catch (final OutOfMemoryError error) {

            if ( !quietMode) {
                MipavUtil.displayError("Out of memory: ViewJFrameBase.openVOI");
            }

            return false;
        }

        return true;
    }

    /**
     * This method opens an existing VOI.
     * 
     * @param image image where VOI(s) are to registered
     * @param quietMode if true indicates that warnings should not be displayed.
     * 
     * @return the VOI(s)
     */
    public VOI[] openVOI(final ModelImage image, boolean quietMode) {
        ViewOpenVOIUI openVOI = null;
        VOI[] voi = null;

        try {
            openVOI = new ViewOpenVOIUI();
            voi = openVOI.open(image, false);
        } catch (final OutOfMemoryError error) {

            if ( !quietMode) {
                MipavUtil.displayError("Out of memory: ViewJFrameBase.openVOI");
            }

            return voi;
        }

        return voi;
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
        float val;
        for ( int z = (int)akMinMax[0].Z; z <= akMinMax[1].Z; z++ )
        {
            for ( int y = (int)akMinMax[0].Y; y <= akMinMax[1].Y; y++ )
            {
                for ( int x = (int)akMinMax[0].X; x <= akMinMax[1].X; x++ )
                {
                    val = image.getFloat(x,y,z);
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
    /**
     * Opens a JDialogStatistics to allow computation ofROI statistics.
     */
    public void showStatisticsCalculator() {

        if (imageStatList == null) {

            if ( (getActiveImage().getVOIs() != null) && (getActiveImage().getVOIs().size() != 0)) {
                imageStatList = new JDialogVOIStatistics(getActiveImage().getVOIs());
                imageStatList.setVisible(true);
                // addVOIUpdateListener(imageStatList); // i'd rather not do it this way...
            } else {
                MipavUtil.displayError("A VOI must be present to use the statistics calculator");
            }
        } else {
            imageStatList.refreshVOIList(getActiveImage().getVOIs());
            imageStatList.setVisible(true);
        }
    }
    /**
     * This method is provided for the user to convert a masked area back to a painted area. It only affects those areas
     * that were masked with the intensity value that is currently active.
     * 
     * @param showProgressBar DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
    public boolean handleMaskToPaint(final boolean showProgressBar) {

        boolean success = false;

        if (m_kImageB != null) {
            float[] intensityMapB;

            if (m_kImageB.isColorImage()) {
                intensityMapB = new float[ (m_kImageA.getExtents()[0] * m_kImageA.getExtents()[1]) * 4]; // make
                // intensity map
            } else {
                intensityMapB = new float[m_kImageA.getExtents()[0] * m_kImageA.getExtents()[1]]; // make intensity map
            }

            // same size as image
            // dimensions
            final BitSet bitSet = componentImage.getPaintMask(); // bitSet is for entire image volume
            ViewJProgressBar progressBar = null;

            if (showProgressBar) {
                progressBar = new ViewJProgressBar("Converting", "Converting mask to paint...", 0, 100, true, this,
                        this);
                MipavUtil.centerOnScreen(progressBar);
                progressBar.setVisible(showProgressBar);
            }

            try {
                final int numSlices = ( (m_kImageA.getNDims() > 2) ? m_kImageA.getExtents()[2] : 1);

                // iterate through slices
                for (int currentSlice = 0; currentSlice < numSlices; currentSlice++) {

                    // here is where we get the slice
                    m_kImageB.exportData(currentSlice * intensityMapB.length, intensityMapB.length, intensityMapB);

                    // examine every pixel and convert to paint if masked intensity is equal to the toolbar's
                    // selected intensity

                    final Color activeColor = getControls().getTools().getPaintColor();
                    final int activeRed = activeColor.getRed();
                    final int activeGreen = activeColor.getGreen();
                    final int activeBlue = activeColor.getBlue();

                    if (m_kImageB.isColorImage()) {

                        for (int k = 0; k <= (intensityMapB.length - 4); k = k + 4) {
                            int r, g, b;
                            r = (new Float(intensityMapB[k + 1])).intValue();
                            g = (new Float(intensityMapB[k + 2])).intValue();
                            b = (new Float(intensityMapB[k + 3])).intValue();

                            if ( (r == activeRed) && (g == activeGreen) && (b == activeBlue)) {
                                bitSet.set( (currentSlice * (intensityMapB.length / 4)) + (k / 4)); // turn the
                                // paint bit

                                // set index to ON
                                intensityMapB[k + 1] = 0; // erase the painted mask from this index
                                intensityMapB[k + 2] = 0; // erase the painted mask from this index
                                intensityMapB[k + 3] = 0; // erase the painted mask from this index
                            }
                        }
                    } else {

                        for (int i = 0; i < intensityMapB.length; i++) {

                            if (intensityMapB[i] == componentImage.intensityDropper) {
                                bitSet.set( (currentSlice * intensityMapB.length) + i); // turn the paint bit set
                                // index to ON
                                intensityMapB[i] = 0; // erase the painted mask from this index
                            }
                        }
                    }

                    // put the modified slice back into image
                    m_kImageB.importData(currentSlice * intensityMapB.length, intensityMapB, false);

                    if (progressBar != null) {
                        progressBar.updateValueImmed((int) ((float) (currentSlice + 1) / (float) numSlices * 100));
                    }
                }

                updateDisplay();
                success = true;
            } catch (final Exception ex) {

                // do nothing. the error will be displayed when this if block exits
                ex.printStackTrace();
                MipavUtil.displayError("Cannot complete the operation due to an internal error.");
            } finally {

                if (progressBar != null) {
                    progressBar.dispose();
                }
            }
        } else {

            // if we get here, there is no mask on the image.
            MipavUtil
            .displayError("This function is only useful when the image has a mask. To use this feature, please add a mask.");
        }

        return success;
    }

     */


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
