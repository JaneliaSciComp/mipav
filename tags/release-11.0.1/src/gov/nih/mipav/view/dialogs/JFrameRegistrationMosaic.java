package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR2D;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.io.*;

import javax.media.j3d.*;
import javax.swing.*;
import javax.vecmath.*;

import com.sun.j3d.utils.universe.SimpleUniverse;


/**
 * JFrameRegistrationMosaic is a window/gui that enables the user to manually align two images with the mouse and then
 * call the AlgorithmRegOAR2D registration algorithm to create a mosaic image with the two aligned images. Multiple
 * images can be added to the mosaic and aligned one at a time.
 */
public class JFrameRegistrationMosaic extends JFrame implements ActionListener, MouseListener, MouseMotionListener,
        AlgorithmInterface /* Registration Algorithm */{

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2954367270283496349L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** For masking, set to true if the reference image is written into the same location:. */
    private boolean[][] m_aabReference = null;

    /** backup:. */
    private boolean[][] m_aabReferenceBackup = null;

    /** Backup shape for Undo:. */
    private Shape3D[] m_akBackupBorders = null;

    /** Backup Image for Undo:. */
    private ModelImage[] m_akBackupImages = null;

    /** Backup shape for Undo:. */
    private Shape3D[] m_akBackupPolygons = null;

    /** Backup Transform for Undo:. */
    private TransformGroup[] m_akBackupTG = null;

    /** Reference to the border shape data structure for changing the color based on which image is selected:. */
    private Shape3D[] m_akBorderShapes = null;

    /** Reference to the model images:. */
    private ModelImage[] m_akImages = null;

    /** Reference and tile image transformations:. */
    private TransformGroup[] m_akImageTransforms = null;

    /** Reference to the texture-mapped polygon on which the image is displayed:. */
    private Shape3D[] m_akPolygonShapes = null;

    /** Brute force optimization parameters:. */
    /** Brute force default is off:. */
    private boolean m_bBruteForce = false;

    /** Display transfromed image in separate window: set to false (no display). */
    private boolean m_bDisplayTransform = false;

    /** Default subsample set to true:. */
    private boolean m_bDoSubsample = true;
    
    private boolean m_bDoMultiThread = true;

    /** Boolean to check that a file is loaded before mouse operations are allowed to occur. */
    private boolean m_bFileLoaded = false;

    /**
     * For blending the reference and tile images, reference image is not blended with background, reset when initData()
     * is called.
     */
    private boolean m_bFirst = true;

    /** To reset m_kReferenceAlpha:. */
    private boolean m_bResetAlpha = true;

    /** True when scale factor should be used:. */
    private boolean m_bSetScale = false;

    /** Default rotation coarse rate, set to 2 degrees:. */
    private float m_fCoarseRate = 2f;

    /** Default rotation fine rate, set to 1 degree:. */
    private float m_fFineRate = 1f;

    /** Default rotation start, set to negative 5 degrees:. */
    private float m_fRotateBegin = -5f;

    /** Default rotation end, set to postive 5 degrees:. */
    private float m_fRotateEnd = 5f;

    /** default rotation range (0). */
    private float m_fRotationRange = 0f;

    /** Scale factor for large images:. */
    private float m_fScale = 1.0f;

    /** default x scale range (0). */
    private float m_fXScaleRange = 0f;

    /** default y scale range (0). */
    private float m_fYScaleRange = 0f;

    /** Registration parameters, with defaults set:. */
    /** Default cost function, set to correlation ratio (smoothed, weighted):. */
    private int m_iCost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;

    /** Default degrees of freedom:. */
    private int m_iDOF = 6;

    /** Default image interpolation, set to be bilinear interpolation:. */
    private int m_iInterp = AlgorithmTransform.BILINEAR;

    /** Display parameters:. */
    /** Default interpolation for the transform algorithm:. */
    private int m_iInterp2 = AlgorithmTransform.BILINEAR;

    /** Default number of iteration set to 2:. */
    private int m_iMaxIterations = 2;

    /** Default number of minima to test from level 8 at level 4 (set to 3):. */
    private int m_iNumMinima = 3;

    /** index of the non-selected image:. */
    private int m_iOpen = 1;

    /** Backup of the open index:. */
    private int m_iOpenSave;

    /** Index of the reference image:. */
    private int m_iReference = 0;

    /** Backup of the reference index:. */
    private int m_iReferenceSave;

    /** default number of divisions for scale optimazation:. */
    private int m_iScaleSteps = 0;

    /** index of the selected image: */
    private int m_iSelected = 0;

    /** Index of the tile image:. */
    private int m_iTile = 1;

    /** default x,y translation range (0). */
    private int m_iTranslationRange = 0;

    /** x,y positions of the mouse when one of the mouse buttons is pressed:. */
    private int m_iXClick, m_iYClick;

    /** Launches the JDialogRegistrationOAR2D dialog to set registration options:. */
    private JButton m_kAdvancedOptionsButton;

    /* Display/Transform: */
    /** Drawing canvas:. */
    private Canvas3D m_kCanvas = null;

    /** Close all images and remove them from the scene:. */
    private JButton m_kCloseAllButton;

    /** current transformation based on mouseDragged event:. */
    private Transform3D m_kCurrentTransform;

    /** Reference to the mousePressed event:. */
    private MouseEvent m_kMouseEvent = null;

    /** Accumulated transformation prior to current mouseDrag:. */
    private Transform3D m_kOldTransform;

    /* GUI buttons: */
    /** Open reference image:. */
    private JButton m_kOpenReferenceButton;

    /** Open tile image:. */
    private JButton m_kOpenTileButton;

    /** For blending between refernce and transformed tile images:. */
    private ModelImage m_kReferenceAlpha = null;

    /** For blending between refernce and transformed tile images:. */
    private ModelImage m_kReferenceAlphaBackup = null;

    /** Initialize and start the registration based on how the user positioned the two images:. */
    private JButton m_kRegisterButton;

    /** Save the mosaic image:. */
    private JButton m_kSaveButton;

    /** Open the mosaic image help. */
    private JButton m_kHelpButton;

    /** Scene graph root node:. */
    private BranchGroup m_kScene = null;

    /** For blending between refernce and transformed tile images:. */
    private ModelImage m_kTileAlpha = null;

    /** Toggle which image is currently selected:. */
    private JButton m_kToggleSelectedButton;

    /** Undo the last registration for the mosaic image:. */
    private JButton m_kUndoButton;

    /** SimpleUniverse:. */
    private SimpleUniverse m_kUniverse = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * JFrameRegistrationMosaic - Creates new window for manual (mouse-based) registration of two images.
     */
    public JFrameRegistrationMosaic() {
        super("Mosaic Registration");
        try {
        	this.setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n", Preferences.DEBUG_FILEIO);
            error.printStackTrace();
        }

        initGUI();
        initData();
        this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - JButton events:
     * 
     * @param event button event
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OpenReference")) {

            /* Open and store the reference image: */
            if (createMosaicOpenDialog(false)) {
                m_kOpenReferenceButton.setEnabled(false);
                m_kOpenTileButton.setEnabled(true);
                m_kUndoButton.setEnabled(false);
                m_kCloseAllButton.setEnabled(true);
                repaintButtons();

                m_iReference = m_iSelected;
            }
        } else if (command.equals("OpenTile")) {

            /* Open and store the tile image: */
            if (createMosaicOpenDialog(false)) {
                m_kOpenTileButton.setEnabled(false);
                m_kToggleSelectedButton.setEnabled(true);
                m_kRegisterButton.setEnabled(true);
                m_kUndoButton.setEnabled(false);
                m_kAdvancedOptionsButton.setEnabled(true);
                repaintButtons();

                m_iTile = m_iSelected;
            }
        } else if (command.equals("ToggleSelected")) {

            /*
             * Toggle which image is selected -- which image the user manipulates with the mouse:
             */
            toggleSelectedImage();
        } else if (command.equals("Register")) {

            /* Call the registration algorithm */
            if (registerImages()) {
                m_kOpenTileButton.setEnabled(true);
                m_kToggleSelectedButton.setEnabled(false);
                m_kRegisterButton.setEnabled(false);
                m_kUndoButton.setEnabled(true);
                m_kSaveButton.setEnabled(true);
                m_kAdvancedOptionsButton.setEnabled(false);
                repaintButtons();
            }
        } else if (command.equals("UndoMosaic")) {

            /* Undoes the last registration: */
            closeAllImages(false);
            undoMosaic();
            m_kOpenReferenceButton.setEnabled(false);
            m_kOpenTileButton.setEnabled(false);
            m_kToggleSelectedButton.setEnabled(true);
            m_kRegisterButton.setEnabled(true);
            m_kUndoButton.setEnabled(false);
            m_kSaveButton.setEnabled(false);
            m_kCloseAllButton.setEnabled(true);
            m_kAdvancedOptionsButton.setEnabled(true);
            repaintButtons();
        } else if (command.equals("SaveMosaic")) {

            /* Save the new mosaic image */
            if (saveMosaic()) {
                m_kUndoButton.setEnabled(false);
                m_kAdvancedOptionsButton.setEnabled(false);
                repaintButtons();
            }
        } else if (command.equals("CloseAll")) {

            /* close all images: */
            closeAllImages(true);
            m_kOpenReferenceButton.setEnabled(true);
            m_kOpenTileButton.setEnabled(false);
            m_kToggleSelectedButton.setEnabled(false);
            m_kRegisterButton.setEnabled(false);
            m_kUndoButton.setEnabled(false);
            m_kSaveButton.setEnabled(false);
            m_kCloseAllButton.setEnabled(false);
            m_kAdvancedOptionsButton.setEnabled(false);
            repaintButtons();
        } else if (command.equals("AdvancedOptions")) {
            new JDialogRegistrationOAR2D(this, m_akImages[m_iReference], m_akImages[m_iTile], m_iCost, m_iDOF,
                    m_iInterp, m_fRotateBegin, m_fRotateEnd, m_fCoarseRate, m_fFineRate, m_bDoSubsample,
                    m_iMaxIterations, m_iNumMinima, m_iInterp2, m_bDisplayTransform, m_fRotationRange,
                    m_fXScaleRange, m_fYScaleRange, m_iScaleSteps, m_iTranslationRange);
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        }
    }

    private void repaintButtons() {
        m_kOpenReferenceButton.repaint();
        m_kOpenTileButton.repaint();
        m_kToggleSelectedButton.repaint();
        m_kRegisterButton.repaint();
        m_kUndoButton.repaint();
        m_kSaveButton.repaint();
        m_kHelpButton.repaint();
        m_kCloseAllButton.repaint();
        m_kAdvancedOptionsButton.repaint();
    }

    /**
     * algorithmPerformed - when the alignment algorithm finishes, the transformed tile is blended with the reference
     * image and displayed:
     * 
     * @param kAlgorithm AlgorithmBase
     */
    public void algorithmPerformed(AlgorithmBase kAlgorithm) {
        ModelImage kTransformedTile = null;
        ModelImage kTransformedTileAlpha = null;
        ModelImage kMosaic = null;
        AlgorithmTransform kAlgorithmTransform = null;

        int iXDim = m_akImages[m_iReference].getExtents()[0];
        int iYDim = m_akImages[m_iReference].getExtents()[1];
        int[] aiExtents = new int[2];
        aiExtents[0] = iXDim;
        aiExtents[1] = iYDim;

        int iReferenceBuffFactor = 1;

        if (m_akImages[m_iReference].isColorImage()) {
            iReferenceBuffFactor = 4;
        }

        int iTileBuffFactor = 1;

        if (m_akImages[m_iTile].isColorImage()) {
            iTileBuffFactor = 4;
        }

        int iMosaicBuffFactor = 1;

        if (iReferenceBuffFactor > iTileBuffFactor) {
            iMosaicBuffFactor = iReferenceBuffFactor;
            kMosaic = new ModelImage(m_akImages[m_iReference].getType(), aiExtents, "Mosaic");
        } else {
            iMosaicBuffFactor = iTileBuffFactor;
            kMosaic = new ModelImage(m_akImages[m_iTile].getType(), aiExtents, "Mosaic");
        }

        if (kAlgorithm instanceof AlgorithmRegOAR2D) {

            if ( ((AlgorithmRegOAR2D) kAlgorithm).isCompleted()) {

                String kName = JDialogBase.makeImageName(m_akImages[m_iTile].getImageName(), "_register");

                /* Transform the input tile image: */
                kAlgorithmTransform = new AlgorithmTransform(m_akImages[m_iTile], ((AlgorithmRegOAR2D) kAlgorithm)
                        .getTransform(), m_iInterp2, 1.0f, 1.0f, iXDim, iYDim, false, false, false);

                kAlgorithmTransform.setRunningInSeparateThread(true);
                kAlgorithmTransform.setUpdateOriginFlag(true);
                kAlgorithmTransform.run();
                kTransformedTile = kAlgorithmTransform.getTransformedImage();
                kAlgorithmTransform.finalize();
                kAlgorithmTransform.disposeLocal();
                kAlgorithmTransform = null;

                /* Transform the TileAlpha image: */
                kAlgorithmTransform = new AlgorithmTransform(m_kTileAlpha, ((AlgorithmRegOAR2D) kAlgorithm)
                        .getTransform(), AlgorithmTransform.BILINEAR, 1.0f, 1.0f, iXDim, iYDim, false, false, false);

                kAlgorithmTransform.setRunningInSeparateThread(true);
                kAlgorithmTransform.setUpdateOriginFlag(true);
                kAlgorithmTransform.run();
                kTransformedTileAlpha = kAlgorithmTransform.getTransformedImage();
                kAlgorithmTransform.finalize();
                kAlgorithmTransform.disposeLocal();
                kAlgorithmTransform = null;

                /* Blend result and reference images into final mosaic: */
                if ( (kTransformedTile != null) && (kTransformedTileAlpha != null)) {
                    int iXRange = kTransformedTile.getExtents()[0];
                    int iYRange = kTransformedTile.getExtents()[1];

                    for (int i = 0; i < iXRange; i++) {

                        for (int j = 0; j < iYRange; j++) {
                            float fRefAlpha = m_kReferenceAlpha.getFloat(i, j);
                            float fTTileAlpha = kTransformedTileAlpha.getFloat(i, j);
                            float fScale = fRefAlpha + fTTileAlpha;
                            m_kReferenceAlpha.set(i, j, fScale);

                            if (fScale != 0) {
                                m_aabReference[i][j] = true;

                                for (int c = 0; c < iMosaicBuffFactor; c++) {
                                    float fRefVal = 0;
                                    float fTTileVal = 0;

                                    if (iReferenceBuffFactor == 1) {
                                        fRefVal = m_akImages[m_iReference].getFloat(i, j);
                                    } else {
                                        fRefVal = m_akImages[m_iReference].getFloatC(i, j, c);
                                    }

                                    if (iTileBuffFactor == 1) {
                                        fTTileVal = kTransformedTile.getFloat(i, j);
                                    } else {
                                        fTTileVal = kTransformedTile.getFloatC(i, j, c);
                                    }

                                    if (kMosaic.isColorImage()) {
                                        kMosaic.setC(i, j, c, ( (fRefAlpha * fRefVal) + (fTTileAlpha * fTTileVal))
                                                / fScale);
                                    } else {
                                        kMosaic
                                                .set(i, j, ( (fRefAlpha * fRefVal) + (fTTileAlpha * fTTileVal))
                                                        / fScale);
                                    }
                                }
                            } else {

                                if (kMosaic.isColorImage()) {
                                    kMosaic.setC(i, j, 0, 0.0f);
                                    kMosaic.setC(i, j, 1, 255.0f);
                                    kMosaic.setC(i, j, 2, 255.0f);
                                    kMosaic.setC(i, j, 3, 255.0f);
                                } else {
                                    kMosaic.set(i, j, 255.0f);
                                }
                            }
                        }
                    }

                    /* Display mosaic in new window: */
                    kMosaic.calcMinMax();

                    if (m_bDisplayTransform) {

                        try {
                            new ViewJFrameImage((ModelImage) kMosaic.clone(kName), null, new Dimension(610, 200));
                        } catch (OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    }

                    /* replace two working images with the new mosaic: */
                    closeAllImages(false);
                    storeImage(kMosaic);
                    m_bFileLoaded = true;
                    m_bResetAlpha = false;
                    if (kTransformedTile != null) {
                        kTransformedTile.disposeLocal();
                        kTransformedTile = null;
                    }
                    if (kTransformedTileAlpha != null) {
                        kTransformedTileAlpha.disposeLocal();
                        kTransformedTileAlpha = null;
                    }
                } else {
                    MipavUtil.displayError("Registration failed, re-align images and try again");

                    actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "UndoMosaic"));
                }
            }
        }

        if ( ((AlgorithmRegOAR2D) kAlgorithm) != null) {
            ((AlgorithmRegOAR2D) kAlgorithm).disposeLocal();
            kAlgorithm = null;
        }
    }

    /**
     * dispose - Removes member variables.
     */
    public void dispose() {
        System.err.println("JFrameRegistrationMosaic: dispose");
        closeAllImages(true);

        if (m_akBackupTG != null) {

            for (int i = 0; i < 2; i++) {
                m_akBackupTG[i] = null;
            }
        }

        m_akBackupTG = null;

        if (m_akBackupBorders != null) {

            for (int i = 0; i < 2; i++) {
                m_akBackupBorders[i] = null;
            }
        }

        m_akBackupBorders = null;

        if (m_akBackupPolygons != null) {

            for (int i = 0; i < 2; i++) {
                m_akBackupPolygons[i] = null;
            }
        }

        m_akBackupPolygons = null;

        if (m_akBackupImages != null) {

            for (int i = 0; i < m_akBackupImages.length; i++) {
                if (m_akBackupImages[i] != null) {
                    m_akBackupImages[i].disposeLocal();
                    m_akBackupImages[i] = null;
                }
            }
        }

        m_akBackupImages = null;

        m_kUniverse.cleanup();
        m_kUniverse = null;
        m_kCanvas = null;
        m_kScene = null;
        m_kMouseEvent = null;
        m_kCurrentTransform = null;
        m_kOldTransform = null;
        m_aabReference = null;
        m_aabReferenceBackup = null;
        if (m_kReferenceAlpha != null) {
            m_kReferenceAlpha.disposeLocal();
            m_kReferenceAlpha = null;
        }
        if (m_kReferenceAlphaBackup != null) {
            m_kReferenceAlphaBackup.disposeLocal();
            m_kReferenceAlphaBackup = null;
        }
        if (m_kTileAlpha != null) {
            m_kTileAlpha.disposeLocal();
            m_kTileAlpha = null;
        }

        super.dispose();
    }

    /**
     * Called from inside the JDialogRegistrationOAR2D class when the user has set the parameters and closes the dialog.
     * 
     * @param kOptionsDialog the JDialogRegistrationOAR2D object containing the updated registration parameters
     * @param bCallAlgorithm boolean when true this function activates the registration algorithm, when false, the user
     *            must then press the "register images" button to register.
     */
    public void getVariablesFromDialog(JDialogRegistrationOAR2D kOptionsDialog, boolean bCallAlgorithm) {

        /* Get the registration parameters from the dialog: */
        m_iCost = kOptionsDialog.getCostChoice();
        m_iDOF = kOptionsDialog.getDOF();
        m_iInterp = kOptionsDialog.getInterp();
        m_fRotateBegin = kOptionsDialog.getCoarseBegin();
        m_fRotateEnd = kOptionsDialog.getCoarseEnd();
        m_fCoarseRate = kOptionsDialog.getCoarseRate();
        m_fFineRate = kOptionsDialog.getFineRate();
        m_bDoSubsample = kOptionsDialog.getSubsample();
        m_iMaxIterations = kOptionsDialog.getMaxIterations();
        m_iNumMinima = kOptionsDialog.getNumMinima();

        m_iInterp2 = kOptionsDialog.getInterp2();
        m_bDisplayTransform = kOptionsDialog.getDisplayTransform();

        m_bBruteForce = kOptionsDialog.getBruteForce();

        m_fRotationRange = kOptionsDialog.getRotationBruteForce();
        m_fXScaleRange = kOptionsDialog.getXScaleBruteForce();
        m_fYScaleRange = kOptionsDialog.getYScaleBruteForce();
        m_iScaleSteps = kOptionsDialog.getScaleStepsBruteForce();
        m_iTranslationRange = kOptionsDialog.getTranslationBruteForce();

        /* Call registration if so directed by the user: */
        if (bCallAlgorithm) {
            actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "Register"));
        }
    }

    /**
     * mouseClicked.
     * 
     * @param e MouseEvent
     */
    public void mouseClicked(MouseEvent e) {}

    /**
     * mouseDragged.
     * 
     * @param e MouseEvent
     */
    public void mouseDragged(MouseEvent e) {

        if ( !m_bFileLoaded) {
            return;
        }

        /* Left mouse button, rotation: */
        if (m_kMouseEvent.getButton() == MouseEvent.BUTTON1) {

            if (e.getX() != m_iXClick) {

                /*
                 * The angle rotation is based on the mouse x position on the screen:
                 */
                double dAngle = (e.getX() - m_iXClick) * ( (2.0 * Math.PI) / (double) m_kCanvas.getWidth());
                Transform3D kRotate = new Transform3D();
                kRotate.setRotation(new AxisAngle4d(0, 0, 1, -dAngle));

                /*
                 * rotation is about the center of the selected image, so concatenate a translation to the origin,
                 * rotation, translation from the origin into the matrix:
                 */
                Vector3d kTransV = new Vector3d();
                m_kOldTransform.get(kTransV);

                Transform3D kTranslateInv = new Transform3D();
                kTranslateInv.setTranslation(new Vector3d( -kTransV.x, -kTransV.y, -kTransV.z));

                Transform3D kTranslate = new Transform3D();
                kTranslate.setTranslation(kTransV);
                kRotate.mul(kTranslateInv);
                kTranslate.mul(kRotate);
                m_kCurrentTransform = new Transform3D(kTranslate);
            }
        }

        /* Middle mouse button, scale: */
        if (m_kMouseEvent.getButton() == MouseEvent.BUTTON2) {

            if (e.getY() != m_iYClick) {

                /*
                 * scale is based on the mouse y movement, each time the mouse moves in y the image is scaled larger (up
                 * in y) or smaller (down in y):
                 */
                double dScale = 1.0;

                if (e.getY() > m_iYClick) {
                    dScale = 1.01;
                } else if (e.getY() < m_iYClick) {
                    dScale = 1.0 / 1.01;
                }

                Transform3D kScale = new Transform3D();
                kScale.setScale(dScale);
                m_kCurrentTransform.mul(kScale);
                m_iXClick = e.getX();
                m_iYClick = e.getY();
            }
        }
        /* Right mouse button, translation: */
        else if (m_kMouseEvent.getButton() == MouseEvent.BUTTON3) {

            /*
             * translation is based on the mouse x-y position, the center of the image is placed exactly at the mouse:
             */
            Matrix4d kNewMatrix = new Matrix4d();
            m_kCurrentTransform.get(kNewMatrix);
            kNewMatrix.m03 = ((double) (e.getX() - (m_iXClick))) * (2.0 / (double) m_kCanvas.getWidth());
            kNewMatrix.m13 = - ((double) (e.getY() - (m_iYClick))) * (2.0 / (double) m_kCanvas.getWidth());
            m_kCurrentTransform.set(kNewMatrix);
        }

        /*
         * Concatenate the current transformations with the previous (accumulated) mouseDragged transformation:
         */
        Transform3D kNewTransform = new Transform3D(m_kCurrentTransform);
        kNewTransform.mul(m_kOldTransform);
        m_akImageTransforms[m_iSelected].setTransform(kNewTransform);
    }

    /**
     * mouseEntered.
     * 
     * @param e MouseEvent
     */
    public void mouseEntered(MouseEvent e) {}

    /**
     * mouseExited.
     * 
     * @param e MouseEvent
     */
    public void mouseExited(MouseEvent e) {}

    /**
     * mouseMoved.
     * 
     * @param e MouseEvent
     */
    public void mouseMoved(MouseEvent e) {}

    /**
     * mousePressed, store the current transformation for the selected image so the new transformations calculated in
     * the mouseDragged function can be concatenated onto the current transform.
     * 
     * @param kMouseEvent MouseEvent
     */
    public void mousePressed(MouseEvent kMouseEvent) {

        if ( !m_bFileLoaded) {
            return;
        }

        /* Save the location of the mouse press: */
        m_iXClick = kMouseEvent.getX();
        m_iYClick = kMouseEvent.getY();

        /*
         * Store the event, for determining which button is pressed during drag:
         */
        m_kMouseEvent = kMouseEvent;

        /*
         * Store the current transform in m_kOldTransform, it will be concatenated during mouseDrag:
         */
        m_kCurrentTransform = new Transform3D();
        m_kOldTransform = new Transform3D();
        m_akImageTransforms[m_iSelected].getTransform(m_kOldTransform);
    }

    /**
     * mouseReleased.
     * 
     * @param e MouseEvent
     */
    public void mouseReleased(MouseEvent e) {}

    /**
     * backupMosaic -- Backs up the reference & tile polygon shapes, borders, ModelImages, and TransformGroups before
     * the registration is called, so that registration can be undone by the user:
     */
    private void backupMosaic() {
        m_kScene.detach();

        /* Allocate backup storage: */
        m_akBackupTG = new TransformGroup[2];
        m_akBackupPolygons = new Shape3D[2];
        m_akBackupBorders = new Shape3D[2];
        m_akBackupImages = new ModelImage[2];

        /* Backup data: */
        for (int i = 0; i < 2; i++) {
            m_akBackupTG[i] = new TransformGroup();
            m_akBackupTG[i].setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
            m_akBackupTG[i].setCapability(TransformGroup.ALLOW_TRANSFORM_READ);

            Transform3D kTransform = new Transform3D();
            m_akImageTransforms[i].getTransform(kTransform);
            m_akBackupTG[i].setTransform(kTransform);

            m_akBackupPolygons[i] = (Shape3D) (m_akPolygonShapes[i].cloneNode(true));
            m_akBackupBorders[i] = (Shape3D) (m_akBorderShapes[i].cloneNode(true));
            m_akBackupImages[i] = m_akImages[i];
        }

        m_iReferenceSave = m_iReference;
        m_iOpenSave = m_iOpen;

        m_kUniverse.addBranchGraph(m_kScene);

        int iReferenceWidth = m_akImages[m_iReference].getExtents()[0];
        int iReferenceHeight = m_akImages[m_iReference].getExtents()[1];
        m_aabReferenceBackup = new boolean[iReferenceWidth][iReferenceHeight];

        for (int i = 0; i < iReferenceWidth; i++) {

            for (int j = 0; j < iReferenceHeight; j++) {

                if (m_aabReference != null) {
                    m_aabReferenceBackup[i][j] = m_aabReference[i][j];
                } else {
                    m_aabReferenceBackup[i][j] = true;
                }
            }
        }

        if (m_kReferenceAlpha != null) {
            m_kReferenceAlphaBackup = (ModelImage) m_kReferenceAlpha.clone();
        }
    }

    /**
     * closeAllImages -- clears the scenegraph of all displayed images and deletes references to the images:
     * 
     * @param bResetAlpha true if all images are closed, false if a new mosaic is created
     */
    private void closeAllImages(boolean bResetAlpha) {

        /* Delete SceneGraph: */
        while (m_kScene.numChildren() > 2) {
            m_kScene.removeChild(2);
        }

        /* Delete transform groups: */
        if (m_akImageTransforms != null) {

            for (int i = 0; i < 2; i++) {
                m_akImageTransforms[i] = null;
            }
        }

        m_akImageTransforms = null;

        /* delete outline shapes: */
        if (m_akBorderShapes != null) {

            for (int i = 0; i < 2; i++) {
                m_akBorderShapes[i] = null;
            }
        }

        m_akBorderShapes = null;

        /* delete textured polygons: */
        if (m_akPolygonShapes != null) {

            for (int i = 0; i < 2; i++) {
                m_akPolygonShapes[i] = null;
            }
        }

        m_akPolygonShapes = null;

        /* delete model images: */
        if (m_akImages != null) {

            for (int i = 0; i < m_akImages.length; i++) {
                if (m_akImages[i] != null) {
                    m_akImages[i].disposeLocal();
                    m_akImages[i] = null;
                }
            }
        }

        m_akImages = null;
        m_aabReference = null;

        if (bResetAlpha) {
            if (m_kReferenceAlpha != null) {
                m_kReferenceAlpha.disposeLocal();
                m_kReferenceAlpha = null;
            }
        }

        /* Reset initial state: */
        m_bFileLoaded = false;
        m_bResetAlpha = bResetAlpha;
        initData();
        System.gc();
    }

    /**
     * createCanvas - Creates the Canvas3D for rendering the images.
     * 
     * @param kPanel the JPanel that contains the Canvas3D in the frame
     * 
     * @return Canvas3D the new canvas
     */
    private Canvas3D createCanvas(JPanel kPanel) {

        /* Create the scene-graph for this canvas: */
        m_kScene = new BranchGroup();
        m_kScene.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        m_kScene.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        m_kScene.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
        m_kScene.setCapability(BranchGroup.ALLOW_DETACH);

        /* BoundingShpere: */
        BoundingSphere kBounds = new BoundingSphere(new Point3d(0.0f, 0.0f, 0.0f), 100.0f);

        /* Add a white background: */
        Background kBackground = new Background(new Color3f(Color.white));
        kBackground.setApplicationBounds(kBounds);
        m_kScene.addChild(kBackground);

        /* Light the scene: */
        Color3f kLightColor = new Color3f(1, 1, 1);
        Vector3f kLightDir = new Vector3f( -.5f, -.5f, -1f);
        DirectionalLight kLight = new DirectionalLight(kLightColor, kLightDir);
        kLight.setInfluencingBounds(kBounds);
        m_kScene.addChild(kLight);

        /* Create the VolumeCanvas3D and SimpleUniverse: */
        GraphicsConfiguration kConfig = SimpleUniverse.getPreferredConfiguration();
        Canvas3D kCanvas = new Canvas3D(kConfig);
        kCanvas.addMouseListener(this);
        kCanvas.addMouseMotionListener(this);

        m_kUniverse = new SimpleUniverse(kCanvas);
        m_kUniverse.getViewingPlatform().setNominalViewingTransform();
        m_kUniverse.addBranchGraph(m_kScene);

        kCanvas.getView().setProjectionPolicy(View.PARALLEL_PROJECTION);
        kPanel.add(kCanvas, BorderLayout.CENTER);

        /* Return the new canvas: */
        return kCanvas;
    }

    /**
     * createMosaicOpenDialog - Creates a file open dialog for image files (.jpg, tiff, etc.). If a new file is opened
     * it is mapped onto a polygon and placed in the scene graph:
     * 
     * @param bSave open file for saving (true) or open file for reading (false)
     * 
     * @return boolean, success or failure for the file open
     */
    private boolean createMosaicOpenDialog(boolean bSave) {
        ModelImage kImage = null;
        FileIO fileIO = new FileIO();
        String fileName = null;
        String directory = null;

        try {
            ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, bSave);

         
            JFileChooser chooser = fileChooser.getFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            int returnVal = -1;

            if (bSave == true) {
                returnVal = chooser.showSaveDialog(this);
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                returnVal = chooser.showOpenDialog(this);
            }

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();

                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                ViewUserInterface.getReference().setDefaultDirectory(directory);
            } else {
                return false;
            }
            

            if (bSave == false) {
                kImage = fileIO.readImage(fileName, directory, false, null);

                if (kImage != null) {
                    storeImage(kImage);
                    m_bFileLoaded = true;
                }
            } else if ( (m_akImages != null) && (bSave == true)) {

                if (m_akImages[m_iReference] != null) {
                    FileWriteOptions kOptions = new FileWriteOptions(true);
                    kOptions.setFileName(fileName);
                    kOptions.setFileDirectory(directory);

                    fileIO.writeImage(m_akImages[m_iReference], kOptions);
                }
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: JFrameRegistrationMosaic");
            Preferences.debug("Out of memory: JFrameRegistrationMosaic\n", Preferences.DEBUG_COMMS);

            return false;
        }

        return true;
    }

    /**
     * createTexturedPolygon - Creates a texture-mapped polygon with the BufferedImage displayed as the texture. The
     * texture-mapped polygon is created so that the displayed texture and size of the polygon match the size in pixels
     * of the original image loaded from file -- even when the original image size is not a power of two. The displayed
     * image must match the original data image so that the registration is accurate.
     * 
     * @param kTransformGroup the TransformGroup which will contain the new textured polygon in the scene graph
     * @param kImage the BufferedImage, power or two size image, padded if necessary, containing the original image
     *            data.
     * @param iWidth the original image width
     * @param iHeight the original image height
     * @param iWidthPow2 the next-largest power of two width
     * @param iHeightPow2 the next-largest power of two height
     */
    private void createTexturedPolygon(TransformGroup kTransformGroup, BufferedImage kImage, int iWidth, int iHeight,
            int iWidthPow2, int iHeightPow2) {

        /* Create the new texture from the input BufferedImage: */
        ImageComponent2D kDisplayedImage = new ImageComponent2D(ImageComponent.FORMAT_RGBA, kImage);
        Texture2D kTexture = new Texture2D(Texture.BASE_LEVEL, Texture.RGBA, iWidthPow2, iHeightPow2);
        kTexture.setEnable(true);
        kTexture.setMinFilter(Texture.BASE_LEVEL_LINEAR);
        kTexture.setMagFilter(Texture.BASE_LEVEL_LINEAR);
        kTexture.setBoundaryModeS(Texture.CLAMP_TO_EDGE);
        kTexture.setBoundaryModeT(Texture.CLAMP_TO_EDGE);
        kTexture.setImage(0, kDisplayedImage);

        /* Setup appearance attributes for the texture mapped polygon. */
        Appearance kImageAppearance = new Appearance();

        /*
         * Disable lighting so that the color information comes from the texture maps.
         */
        Material kMaterial = new Material();
        kMaterial.setLightingEnable(false);
        kImageAppearance.setMaterial(kMaterial);

        /* Use Replace mode so all color comes from the texture. */
        TextureAttributes kTextureAttr = new TextureAttributes();
        kTextureAttr.setTextureMode(TextureAttributes.REPLACE);
        kImageAppearance.setTextureAttributes(kTextureAttr);
        kImageAppearance.setTexture(kTexture);

        /* Partially transparent so multiple images can overlap: */
        TransparencyAttributes kTransparency = new TransparencyAttributes();

        if (m_bFirst == true) {
            m_bFirst = false;
            kTransparency.setTransparencyMode(TransparencyAttributes.NONE);
        } else {
            kTransparency.setTransparencyMode(TransparencyAttributes.BLENDED);
        }

        kImageAppearance.setTransparencyAttributes(kTransparency);

        /* The texture-mapped polygon geometry: */
        double dWidth = (double) iWidth / (double) m_kCanvas.getWidth();
        double dHeight = (double) iHeight / (double) m_kCanvas.getWidth();
        float fWidthTextureScale = (float) iWidth / (float) iWidthPow2;
        float fHeightTextureScale = (float) iHeight / (float) iHeightPow2;
        QuadArray kGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_2);
        kGeometry.setCoordinate(0, new Point3d( -dWidth, -dHeight, 0));
        kGeometry.setCoordinate(1, new Point3d(dWidth, -dHeight, 0));
        kGeometry.setCoordinate(2, new Point3d(dWidth, dHeight, 0));
        kGeometry.setCoordinate(3, new Point3d( -dWidth, dHeight, 0));

        /* Texture coordinates: */
        kGeometry.setTextureCoordinate(0, 0, new TexCoord2f(0, 0));
        kGeometry.setTextureCoordinate(0, 1, new TexCoord2f(fWidthTextureScale, 0));
        kGeometry.setTextureCoordinate(0, 2, new TexCoord2f(fWidthTextureScale, fHeightTextureScale));
        kGeometry.setTextureCoordinate(0, 3, new TexCoord2f(0, fHeightTextureScale));

        /* Create the rendered shape, allow the appearance to be read. */
        Shape3D kImageShape = new Shape3D(kGeometry, kImageAppearance);
        kTransformGroup.addChild(kImageShape);

        /* The border outline: when the image is selected the border is red: */
        QuadArray kBorderGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.COLOR_3);
        kBorderGeometry.setCoordinate(0, new Point3d( -dWidth, -dHeight, 0));
        kBorderGeometry.setCoordinate(1, new Point3d(dWidth, -dHeight, 0));
        kBorderGeometry.setCoordinate(2, new Point3d(dWidth, dHeight, 0));
        kBorderGeometry.setCoordinate(3, new Point3d( -dWidth, dHeight, 0));
        kBorderGeometry.setColor(0, new Color3f(1, 0, 0));
        kBorderGeometry.setColor(1, new Color3f(1, 0, 0));
        kBorderGeometry.setColor(2, new Color3f(1, 0, 0));
        kBorderGeometry.setColor(3, new Color3f(1, 0, 0));
        kBorderGeometry.setCapability(GeometryArray.ALLOW_COLOR_WRITE);

        /* Line thinkness in pixels: */
        LineAttributes kLineAttributes = new LineAttributes(2f, LineAttributes.PATTERN_SOLID, true);

        /* Display the outline of the box: */
        PolygonAttributes kPolygonAttributes = new PolygonAttributes(PolygonAttributes.POLYGON_LINE,
                PolygonAttributes.CULL_NONE, 0f);

        /* Appearance: */
        Appearance kBorderAppearance = new Appearance();
        kBorderAppearance.setLineAttributes(kLineAttributes);
        kBorderAppearance.setPolygonAttributes(kPolygonAttributes);

        /* Create the Shape3D object to contain the border: */
        Shape3D kBorderShape = new Shape3D(kBorderGeometry, kBorderAppearance);
        kBorderShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        kBorderShape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        kTransformGroup.addChild(kBorderShape);

        /* Store the images for toggle selected and mouse manipulations: */
        if (m_akBorderShapes == null) {
            m_akImageTransforms = new TransformGroup[2];
            m_akPolygonShapes = new Shape3D[2];
            m_akBorderShapes = new Shape3D[2];
            m_akImages = new ModelImage[2];
        } else {
            int iTemp = m_iOpen;
            m_iOpen = m_iSelected;
            m_iSelected = iTemp;

            for (int i = 0; i < 4; i++) {
                ((QuadArray) m_akBorderShapes[m_iOpen].getGeometry()).setColor(i, new Color3f(0, 0, 1));
            }
        }

        m_akPolygonShapes[m_iSelected] = kImageShape;
        m_akBorderShapes[m_iSelected] = kBorderShape;
        m_akImageTransforms[m_iSelected] = kTransformGroup;
    }

    /**
     * initData -- Initializes the selected, open, reference and tile indexes. Called on startup and after a
     * closeAllImages()
     */
    private void initData() {
        m_iSelected = 0;
        m_iOpen = 1;
        m_iReference = 0;
        m_iTile = 1;
        m_fScale = 1.0f;
        m_bSetScale = false;
        m_bFirst = true;
    }

    /**
     * initGUI - Initializes GUI toolbar and buttons and displays the registration window.
     */
    private void initGUI() {

        /* ToolBar for displaying the buttons for the GUI: */
        JToolBar kToolBar = new JToolBar();

        /* The reference image open button: */
        m_kOpenReferenceButton = new JButton("Open Reference Image");
        m_kOpenReferenceButton.setActionCommand("OpenReference");
        m_kOpenReferenceButton.addActionListener(this);
        kToolBar.add(m_kOpenReferenceButton);

        /* The registration image open button: */
        m_kOpenTileButton = new JButton("Add tile image");
        m_kOpenTileButton.setActionCommand("OpenTile");
        m_kOpenTileButton.addActionListener(this);
        m_kOpenTileButton.setEnabled(false);
        kToolBar.add(m_kOpenTileButton);

        /* Toggle which image is selected and manipulated by the mouse: */
        m_kToggleSelectedButton = new JButton("Toggle Selected Image");
        m_kToggleSelectedButton.setActionCommand("ToggleSelected");
        m_kToggleSelectedButton.addActionListener(this);
        m_kToggleSelectedButton.setEnabled(false);
        kToolBar.add(m_kToggleSelectedButton);

        /*
         * Call the registration algorithm, using the user-manipulated transformation as the initial guess:
         */
        m_kRegisterButton = new JButton("Register Images");
        m_kRegisterButton.setActionCommand("Register");
        m_kRegisterButton.addActionListener(this);
        m_kRegisterButton.setEnabled(false);
        kToolBar.add(m_kRegisterButton);

        /* Undo button - undoes the last registration */
        m_kUndoButton = new JButton("Undo Mosaic");
        m_kUndoButton.setActionCommand("UndoMosaic");
        m_kUndoButton.addActionListener(this);
        m_kUndoButton.setEnabled(false);
        kToolBar.add(m_kUndoButton);

        /* Save button - saves the new mosaic image */
        m_kSaveButton = new JButton("Save Mosaic");
        m_kSaveButton.setActionCommand("SaveMosaic");
        m_kSaveButton.addActionListener(this);
        m_kSaveButton.setEnabled(false);
        kToolBar.add(m_kSaveButton);

        m_kCloseAllButton = new JButton("Close All");
        m_kCloseAllButton.setActionCommand("CloseAll");
        m_kCloseAllButton.addActionListener(this);
        m_kCloseAllButton.setEnabled(false);
        kToolBar.add(m_kCloseAllButton);

        m_kAdvancedOptionsButton = new JButton("Advanced Options");
        m_kAdvancedOptionsButton.setActionCommand("AdvancedOptions");
        m_kAdvancedOptionsButton.addActionListener(this);
        m_kAdvancedOptionsButton.setEnabled(false);
        kToolBar.add(m_kAdvancedOptionsButton);

        m_kHelpButton = new JButton("Help");
        m_kHelpButton.setActionCommand("Help");
        m_kHelpButton.addActionListener(this);
        m_kHelpButton.setEnabled(true);
        kToolBar.add(m_kHelpButton);

        /* Add the toolbar to the display: */
        kToolBar.validate();
        kToolBar.setVisible(true);
        getContentPane().add(kToolBar, BorderLayout.NORTH);

        /* Create a new canvas and add it to the display: */
        JPanel displayPanel = new JPanel(new BorderLayout());
        m_kCanvas = createCanvas(displayPanel);
        getContentPane().add(displayPanel, BorderLayout.CENTER);

        /* Display the window: */
        pack();
        setVisible(true);
        setSize(Toolkit.getDefaultToolkit().getScreenSize().width, Toolkit.getDefaultToolkit().getScreenSize().height);
    }

    /**
     * registerImages - Registers the reference and tile images based on the how the user positions the images with the
     * mouse. Two new registered images are created, each containing one registered sub-image. The AlgorithmRegOAR2D is
     * then called on the two new registered images, to better refine the registration. Upon completion of the
     * registration algorithm, a new mosaic image is created.
     * 
     * @return boolean, sucess/failure of registeration
     */
    private boolean registerImages() {

        /*
         * Backup images and transforms in case the user chooses to undo this operation:
         */
        backupMosaic();

        m_akImages[m_iReference].calcMinMax();
        m_akImages[m_iTile].calcMinMax();

        /*
         * Get the reference image transform and invert it, so the reference image is at the origin, axis-aligned:
         */
        Transform3D kReferenceT = new Transform3D();
        m_akImageTransforms[m_iReference].getTransform(kReferenceT);
        kReferenceT.invert();

        /*
         * Get the tile image transform and concatenate it with the inverted reference image transform:
         */
        Transform3D kTileT = new Transform3D();
        m_akImageTransforms[m_iTile].getTransform(kTileT);
        kTileT.mul(kReferenceT, kTileT);

        /*
         * Swap the rotation transform (-sin for sin), and invert the y translation to get from polygon space to screen
         * pixel space:
         */
        Matrix4d kTileMatrix = new Matrix4d();
        kTileT.get(kTileMatrix);

        double temp = kTileMatrix.m01;
        kTileMatrix.m01 = kTileMatrix.m10;
        kTileMatrix.m10 = temp;
        kTileMatrix.m13 *= -1;
        kTileT.set(kTileMatrix);

        /*
         * Transform the polygon to image pixel space, at the four corners of the polygon:
         */
        int iTileWidth = m_akImages[m_iTile].getExtents()[0];
        int iTileHeight = m_akImages[m_iTile].getExtents()[1];
        double dWidth = (double) iTileWidth / (double) m_kCanvas.getWidth();
        double dHeight = (double) iTileHeight / (double) m_kCanvas.getWidth();
        int iReferenceWidth = m_akImages[m_iReference].getExtents()[0];
        int iReferenceHeight = m_akImages[m_iReference].getExtents()[1];

        /*
         * Since we're transforming into the reference image space, the reference size is the min/max bounds to begin:
         */
        double dMinX = -(double) iReferenceWidth / 2;
        double dMinY = -(double) iReferenceHeight / 2;
        double dMaxX = (double) iReferenceWidth / 2;
        double dMaxY = (double) iReferenceHeight / 2;

        /* polygon corners to be transformed: */
        Vector4d[] akCorners = new Vector4d[4];
        akCorners[0] = new Vector4d( -dWidth, -dHeight, 0, 1);
        akCorners[1] = new Vector4d(dWidth, -dHeight, 0, 1);
        akCorners[2] = new Vector4d(dWidth, dHeight, 0, 1);
        akCorners[3] = new Vector4d( -dWidth, dHeight, 0, 1);

        Transform3D kFinalTransform = new Transform3D(kTileT);

        for (int i = 0; i < 4; i++) {
            kFinalTransform.transform(akCorners[i]);
            akCorners[i].x *= ((double) m_kCanvas.getWidth() / 2.0);
            akCorners[i].y *= ((double) m_kCanvas.getWidth() / 2.0);

            /* Find the new max/min bounds in pixel space */
            if (akCorners[i].x < dMinX) {
                dMinX = akCorners[i].x;
            }

            if (akCorners[i].x > dMaxX) {
                dMaxX = akCorners[i].x;
            }

            if (akCorners[i].y < dMinY) {
                dMinY = akCorners[i].y;
            }

            if (akCorners[i].y > dMaxY) {
                dMaxY = akCorners[i].y;
            }
        }

        /* Image size, X/YRange and pixel offset to pad images: */
        int iXRange = (int) (dMaxX - dMinX);
        int iYRange = (int) (dMaxY - dMinY);

        /* Create new images and mask images for weighted registration: */
        int[] aiExtents = new int[2];
        aiExtents[0] = iXRange;
        aiExtents[1] = iYRange;

        /* new reference image, offset */
        ModelImage kReference = new ModelImage(m_akImages[m_iReference].getType(), aiExtents, "reference");
        ;

        ModelImage kReferenceReg = new ModelImage(ModelStorageBase.FLOAT, aiExtents, "ReferenceReg");
        ;

        /* reference mask, where the reference and tile images overlap: */
        ModelImage kReferenceMask = new ModelImage(ModelStorageBase.BYTE, aiExtents, "reference_mask");
        ;

        /* reference alpha: */
        if (m_kReferenceAlpha != null) {
            m_kReferenceAlpha.disposeLocal();
        }
        m_kReferenceAlpha = new ModelImage(ModelStorageBase.FLOAT, aiExtents, "reference_alpha");
        ;

        /* transformed tile image: */
        ModelImage kTile = new ModelImage(m_akImages[m_iTile].getType(), aiExtents, "tile");
        ;

        ModelImage kTileReg = new ModelImage(ModelStorageBase.FLOAT, aiExtents, "TileReg");
        ;

        /* tile mask (same as reference mask): */
        ModelImage kTileMask = new ModelImage(ModelStorageBase.BYTE, aiExtents, "tile_mask");
        ;

        /* tile alpha: */
        if (m_kTileAlpha != null) {
            m_kTileAlpha.disposeLocal();
        }
        m_kTileAlpha = new ModelImage(ModelStorageBase.FLOAT, aiExtents, "tile_alpha");
        ;

        /*
         * For masking, set to true if the reference image is written into the same location:
         */
        m_aabReference = new boolean[iXRange][iYRange];

        int iReferenceOffsetLeft = (int) ( ( -(double) iReferenceWidth / 2) - dMinX);
        int iReferenceOffsetTop = (int) ( ( -(double) iReferenceHeight / 2) - dMinY);

        int iReferenceBuffFactor = 1;

        if (m_akImages[m_iReference].isColorImage()) {
            iReferenceBuffFactor = 4;
        }

        int iTileBuffFactor = 1;

        if (m_akImages[m_iTile].isColorImage()) {
            iTileBuffFactor = 4;
        }

        /*
         * Loop over the new image pixels.
         * 
         * (1) Write the new reference image, positioning the reference data with offsets so that the two images are
         * aligned based on the user-positioned transform.
         * 
         * (2) Initialize The reference and tile mask images for weighted registration.
         * 
         * (3) apply the inverse transformation to the tile image pixels to determine where to place the it in the new
         * overlapped image:
         * 
         * (4) Write the reference and tile mask images where ever both reference and tile images overlap.
         */
        int iX, iY;
        float fXDistance, fYDistance;
        float fRefMinDimension = (float) Math.min(iReferenceWidth / 2.0, iReferenceHeight / 2.0);
        float fTileMinDimension = (float) Math.min(iTileWidth / 2.0, iTileHeight / 2.0);

        Transform3D kInverse = new Transform3D(kTileT);
        kInverse.invert();

        for (int i = 0; i < iXRange; i++) {

            for (int j = 0; j < iYRange; j++) {
                float fGrayScale = 0;
                iX = i - iReferenceOffsetLeft;
                iY = j - iReferenceOffsetTop;

                /* (1) Write new reference image: */
                for (int c = 0; c < iReferenceBuffFactor; c++) {

                    if ( (iX >= 0) && (iX < iReferenceWidth) && (iY >= 0) && (iY < iReferenceHeight)) {

                        if (iReferenceBuffFactor == 4) {
                            kReference.setC(i, j, c, m_akImages[m_iReference].getFloatC(iX, iY, c));
                        } else {
                            kReference.set(i, j, m_akImages[m_iReference].getFloat(iX, iY));
                        }

                        if (m_bResetAlpha) {
                            fXDistance = Math.min(iX, iReferenceWidth - iX);
                            fYDistance = Math.min(iY, iReferenceHeight - iY);

                            if (fXDistance < fYDistance) {
                                fXDistance /= fRefMinDimension;
                                m_kReferenceAlpha.set(i, j, fXDistance);
                            } else {
                                fYDistance /= fRefMinDimension;
                                m_kReferenceAlpha.set(i, j, fYDistance);
                            }
                        } else {
                            m_kReferenceAlpha.set(i, j, m_kReferenceAlphaBackup.getFloat(iX, iY));
                        }

                        m_aabReference[i][j] = m_aabReferenceBackup[iX][iY];

                        if (iReferenceBuffFactor == 1) {
                            fGrayScale += m_akImages[m_iReference].getFloat(iX, iY);
                        } else if (c > 0) {
                            fGrayScale += m_akImages[m_iReference].getFloatC(iX, iY, c);
                        }
                    } else {

                        if (iReferenceBuffFactor == 4) {
                            kReference.setC(i, j, c, 0.0f);
                        } else {
                            kReference.set(i, j, 0.0f);
                        }

                        m_aabReference[i][j] = false;
                    }
                }

                if ( (m_aabReference[i][j] == false) && m_bResetAlpha) {
                    m_kReferenceAlpha.set(i, j, 0.0f);
                }

                if (iReferenceBuffFactor == 4) {
                    fGrayScale /= 3.0f;
                }

                /* (2) Initialize the reference and tile masks */
                kReferenceMask.set(i, j, 0);
                kTileMask.set(i, j, 0);
                kReferenceReg.set(i, j, fGrayScale);
                fGrayScale = 0;

                /*
                 * (3) apply the inverse transformation to the tile image pixels to determine where to place the it in
                 * the new overlapped image:
                 */
                Vector4d kPoint = new Vector4d( (i - Math.abs(dMinX)) / ((double) m_kCanvas.getWidth() / 2.0),

                (j - Math.abs(dMinY)) / ((double) m_kCanvas.getWidth() / 2.0), 0, 1);
                kInverse.transform(kPoint);
                kPoint.x *= m_kCanvas.getWidth() / 2.0;
                kPoint.y *= m_kCanvas.getWidth() / 2.0;
                kPoint.x += iTileWidth / 2;
                kPoint.y += iTileHeight / 2;

                iX = (int) kPoint.x;
                iY = (int) kPoint.y;

                /* Write the transformed tile image: */
                for (int c = 0; c < iTileBuffFactor; c++) {

                    if ( (iX >= 0) && (iX < (iTileWidth - 1)) && (iY >= 0) && (iY < (iTileHeight - 1))) {

                        if (iTileBuffFactor == 4) {
                            kTile.setC(i, j, c, m_akImages[m_iTile].getFloatC(iX, iY, c));
                        } else {
                            kTile.set(i, j, m_akImages[m_iTile].getFloat(iX, iY));
                        }

                        if (iTileBuffFactor == 1) {
                            fGrayScale += m_akImages[m_iTile].getFloat(iX, iY);
                        } else if (c > 0) {
                            fGrayScale += m_akImages[m_iTile].getFloatC(iX, iY, c);
                        }

                        if (m_aabReference[i][j] == true) {
                            kReferenceMask.set(i, j, 1);
                            kTileMask.set(i, j, 1);
                        }

                        fXDistance = Math.min(iX, iTileWidth - iX);
                        fYDistance = Math.min(iY, iTileHeight - iY);

                        if (fXDistance < fYDistance) {
                            fXDistance /= fTileMinDimension;
                            m_kTileAlpha.set(i, j, fXDistance);
                        } else {
                            fYDistance /= fTileMinDimension;
                            m_kTileAlpha.set(i, j, fYDistance);
                        }
                    } else {

                        if (iTileBuffFactor == 4) {
                            kTile.setC(i, j, c, 0.0f);
                        } else {
                            kTile.set(i, j, 0.0f);
                        }

                        m_kTileAlpha.set(i, j, 0.0f);
                    }
                }

                /*
                 * (4) Write the reference and tile mask images where ever both reference and tile images overlap.
                 */
                if (iTileBuffFactor == 4) {
                    fGrayScale /= 3.0f;
                }

                kTileReg.set(i, j, fGrayScale);
            }
        }

        kReference.calcMinMax();
        kReferenceMask.calcMinMax();
        kReferenceReg.calcMinMax();
        kTile.calcMinMax();
        kTileMask.calcMinMax();
        kTileReg.calcMinMax();
        m_kReferenceAlpha.calcMinMax();
        m_kTileAlpha.calcMinMax();
        /*
         * try { new ViewJFrameImage(kReference, null, new Dimension(610, 200)); } catch (OutOfMemoryError error) {
         * MipavUtil.displayError( "Out of memory: unable to open new frame"); } try { new
         * ViewJFrameImage(kReferenceMask, null, new Dimension(610, 200)); } catch (OutOfMemoryError error) {
         * MipavUtil.displayError( "Out of memory: unable to open new frame"); } try { new ViewJFrameImage(kTile, null,
         * new Dimension(610, 200)); } catch (OutOfMemoryError error) { MipavUtil.displayError( "Out of memory: unable
         * to open new frame"); } try { new ViewJFrameImage(kTileMask, null, new Dimension(610, 200)); } catch
         * (OutOfMemoryError error) { MipavUtil.displayError( "Out of memory: unable to open new frame"); } try { new
         * ViewJFrameImage(kReferenceReg, null, new Dimension(610, 200)); } catch (OutOfMemoryError error) {
         * MipavUtil.displayError( "Out of memory: unable to open new frame"); } try { new ViewJFrameImage(kTileReg,
         * null, new Dimension(610, 200)); } catch (OutOfMemoryError error) { MipavUtil.displayError( "Out of memory:
         * unable to open new frame"); } try { new ViewJFrameImage((ModelImage)m_kReferenceAlpha.clone(), null, new
         * Dimension(610, 200)); } catch (OutOfMemoryError error) { MipavUtil.displayError( "Out of memory: unable to
         * open new frame"); } try { new ViewJFrameImage(m_kTileAlpha, null, new Dimension(610, 200)); } catch
         * (OutOfMemoryError error) { MipavUtil.displayError( "Out of memory: unable to open new frame"); }
         */

        /*
         * Save the transformed tile and reference image, for when the alignment algorithm is finished:
         */
        m_akImages[m_iReference] = kReference;
        m_akImages[m_iTile] = kTile;

        /* launch alignment: */
        if (m_iCost == AlgorithmCostFunctions2D.LEAST_SQUARES_SMOOTHED_WGT_COLOR) {
            kReferenceReg.disposeLocal();
            kReferenceReg = (ModelImage) kReference.clone();
            kTileReg.disposeLocal();
            kTileReg = (ModelImage) kTile.clone();
        }

        AlgorithmRegOAR2D kAlgorithmReg = new AlgorithmRegOAR2D(kReferenceReg, kTileReg, kReferenceMask, kTileMask,
                m_iCost, m_iDOF, m_iInterp, m_fRotateBegin, m_fRotateEnd, m_fCoarseRate, m_fFineRate, m_bDoSubsample,
                m_bDoMultiThread, m_iMaxIterations, m_iNumMinima);
        kAlgorithmReg.addListener(this);
        kAlgorithmReg.setRunningInSeparateThread(false);

        if (m_bBruteForce == true) {

            /*
             * Setup the brute force registration, and initialize brute force parameters:
             */
            kAlgorithmReg.setBruteForce(true, m_fRotationRange, m_fXScaleRange, m_fYScaleRange, m_iScaleSteps,
                    m_iTranslationRange);
        }

        kAlgorithmReg.run();
        kAlgorithmReg.disposeLocal();
        kAlgorithmReg = null;

        if (kReferenceReg != null) {
            kReferenceReg.disposeLocal();
            kReferenceReg = null;
        }

        if (kTileReg != null) {
            kTileReg.disposeLocal();
            kTileReg = null;
        }

        if (kReferenceMask != null) {
            kReferenceMask.disposeLocal();
            kReferenceMask = null;
        }

        if (kTileMask != null) {
            kTileMask.disposeLocal();
            kTileMask = null;
        }

        return true;
    }

    /**
     * saveMosaic -- Opens a save dialog and saves the mosaic image in the selected file format.
     * 
     * @return boolean, sucess/failure of file save
     */
    private boolean saveMosaic() {

        if (m_akImages != null) {
            return createMosaicOpenDialog(true);
        }

        return false;
    }

    /**
     * storeImage - creates a BufferedImage from the ModelImage data where the BufferedImage's size is the next-largest
     * power of two from the ModelImage size. The BufferedImage is then passed to the createTexturedPolygon function for
     * display in the scene graph.
     * 
     * @param kImage the input ModelImage containing the image data.
     */
    private void storeImage(ModelImage kImage) {
        int iWidth = kImage.getExtents()[0];
        int iHeight = kImage.getExtents()[1];

        kImage.calcMinMax();

        /*
         * Determine the next-largest size that is a power of two, to create the texture:
         */
        double dLog2Width = Math.log((double) iWidth) / Math.log(2.0);
        double dLog2Height = Math.log((double) iHeight) / Math.log(2.0);
        int iWidthPow2 = (int) Math.pow(2, Math.ceil(dLog2Width));
        int iHeightPow2 = (int) Math.pow(2, Math.ceil(dLog2Height));

        /*
         * Create the new BufferedImage and write the ModelImage data into it:
         */
        BufferedImage kBuffer = new BufferedImage(iWidthPow2, iHeightPow2, BufferedImage.TYPE_INT_ARGB);

        for (int iY = 0; iY < iHeight; iY++) {

            for (int iX = 0; iX < iWidth; iX++) {
                kBuffer.setRGB(iX, (iHeightPow2 - iHeight) + iY,
                        kImage.getPackedColor( (iY * iWidth) + iX) & 0xaaffffff);
            }
        }

        /*
         * Create the textured polygon and store the data in the scene graph:
         */
        Transform3D kTransform = new Transform3D();

        /* Scale large images: */
        int iWidthLimit = m_kCanvas.getWidth();
        int iHeightLimit = m_kCanvas.getHeight();

        if ( (iWidth > iWidthLimit) || (iHeight > iHeightLimit) || m_bSetScale) {

            if (m_bSetScale == false) {
                m_fScale = (float) Math.min( (iWidthLimit / (float) iWidth), (iHeightLimit / (float) iHeight));
            }

            kTransform.setScale(m_fScale);
            m_bSetScale = true;
        }

        TransformGroup kTransformGroup = new TransformGroup(kTransform);
        kTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        kTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        createTexturedPolygon(kTransformGroup, kBuffer, iWidth, iHeight, iWidthPow2, iHeightPow2);

        BranchGroup kBranch = new BranchGroup();
        kBranch.setCapability(BranchGroup.ALLOW_DETACH);
        kBranch.addChild(kTransformGroup);

        m_kScene.addChild(kBranch);

        m_akImages[m_iSelected] = kImage;
    }

    /**
     * toggleSelectedImage - Toggles which image is currently selected. Changes the color of the image borders.
     */
    private void toggleSelectedImage() {
        int iTemp = m_iOpen;
        m_iOpen = m_iSelected;
        m_iSelected = iTemp;

        for (int i = 0; i < 4; i++) {
            ((QuadArray) m_akBorderShapes[m_iOpen].getGeometry()).setColor(i, new Color3f(0, 0, 1));
            ((QuadArray) m_akBorderShapes[m_iSelected].getGeometry()).setColor(i, new Color3f(1, 0, 0));
        }
    }

    /**
     * undoMosaic -- Restores the backed-up ImageTransforms, PolygonShapes, BorderShapes, and ModelImages after an undo
     * button press:
     */
    private void undoMosaic() {

        /* Restore original data: */
        m_akImageTransforms = new TransformGroup[2];
        m_akPolygonShapes = new Shape3D[2];
        m_akBorderShapes = new Shape3D[2];
        m_akImages = new ModelImage[2];

        for (int i = 0; i < 2; i++) {

            /* Restore displayed objects: */
            m_akPolygonShapes[i] = m_akBackupPolygons[i];
            m_akBorderShapes[i] = m_akBackupBorders[i];

            /* restore ModelImage: */
            m_akImages[i] = m_akBackupImages[i];

            /* Restore TransformGroups: */
            m_akImageTransforms[i] = new TransformGroup();
            m_akImageTransforms[i].setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
            m_akImageTransforms[i].setCapability(TransformGroup.ALLOW_TRANSFORM_READ);

            Transform3D kTransform = new Transform3D();

            /* Add transforms, shapes to the TransformGroups: */
            m_akBackupTG[i].getTransform(kTransform);
            m_akImageTransforms[i].setTransform(kTransform);
            m_akImageTransforms[i].addChild(m_akPolygonShapes[i]);
            m_akImageTransforms[i].addChild(m_akBorderShapes[i]);

            /* Add restored objects to the scene graph: */
            BranchGroup kBranch = new BranchGroup();
            kBranch.setCapability(BranchGroup.ALLOW_DETACH);
            kBranch.addChild(m_akImageTransforms[i]);
            m_kScene.addChild(kBranch);
        }

        m_bFileLoaded = true;
        m_iReference = m_iReferenceSave;
        m_iTile = 0;

        if (m_iReference == 0) {
            m_iTile = 1;
        }

        m_iOpen = m_iOpenSave;
        m_iSelected = 0;

        if (m_iOpen == 0) {
            m_iSelected = 1;
        }

        int iReferenceWidth = m_akImages[m_iReference].getExtents()[0];
        int iReferenceHeight = m_akImages[m_iReference].getExtents()[1];
        m_aabReference = new boolean[iReferenceWidth][iReferenceHeight];

        for (int i = 0; i < iReferenceWidth; i++) {

            for (int j = 0; j < iReferenceHeight; j++) {

                if (m_aabReferenceBackup != null) {
                    m_aabReference[i][j] = m_aabReferenceBackup[i][j];
                } else {
                    m_aabReference[i][j] = true;
                }
            }
        }

        if (m_kReferenceAlphaBackup != null) {
            m_kReferenceAlpha = (ModelImage) m_kReferenceAlphaBackup.clone();
        }

    }
}
