package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;


import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;


import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.*;
import javax.media.opengl.awt.GLCanvas;
import com.jogamp.opengl.util.Animator;

/**
 * Class PlaneRenderWM: renders a single dimension of the ModelImage data as a texture-mapped polygon. The PlaneRenderWM
 * class keeps track of whether it is rendering the Axial, Sagittal, or Coronal view of the data.
 */
public class PlaneRenderProstate extends GPURenderBase implements GLEventListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    private class LocalVolumeVOI {
        public Vector<String> Name = new Vector<String>();

        public PolylineVector Local = new PolylineVector();

        public PolylineVector Volume = new PolylineVector();

        private final Vector3f m_kLocalPt = new Vector3f();

        private Vector3f m_kVolumePt;

        private int m_iCurrent;

        private final Vector3f m_kLocalCenter = new Vector3f();

        public LocalVolumeVOI(final Polyline kLocal, final String kName) {
            Name.add(kName);
            Local.add(kLocal);
            Volume.add(createVolumePolyline(kLocal.VBuffer));
            m_iCurrent = 0;
        }

        public void add(final Polyline kLocal, final String kName) {
            Name.add(kName);
            Local.add(kLocal);
            Volume.add(createVolumePolyline(kLocal.VBuffer));
            m_iCurrent++;
        }

        public void addPoint(final float fX, final float fY, final float fZ) {}

        public Polyline getLocal(final int i) {
            return Local.get(i);
        }

        public Polyline getVolume(final int i) {
            return Volume.get(i);
        }

        public int GetVertexQuantity(final int i) {
            return Local.get(i).VBuffer.GetVertexQuantity();
        }

        public void move(final Vector3f kDiff) {
            kDiff.Sub(m_kLocalCenter);

            final VertexBuffer kLocalVBuffer = Local.get(0).VBuffer;
            final VertexBuffer kVolumeVBuffer = Volume.get(0).VBuffer;
            final int iNumPoints = kLocalVBuffer.GetVertexQuantity();
            if (iNumPoints > 0) {
                for (int i = 0; i < iNumPoints; i++) {
                    Vector3f kPos = kLocalVBuffer.GetPosition3(i);
                    kPos.Add(kDiff);
                    kLocalVBuffer.SetPosition3(i, kPos);

                    kPos = VOIToFileCoordinates(kPos, true);
                    kVolumeVBuffer.SetPosition3(i, kPos);
                }
                kLocalVBuffer.Release();
                kVolumeVBuffer.Release();
                m_bUpdateVOI = true;
            }
        }

        public void Release() {
            Local.get(m_iCurrent).VBuffer.Release();
            Volume.get(m_iCurrent).VBuffer.Release();
        }

        public void setCenter(final float fX, final float fY, final float fZ) {
            m_kLocalCenter.Set(fX, fY, fZ);
        }

        public void SetPosition(final int iPos, final float fX, final float fY, final float fZ) {
            if (iPos < Local.get(m_iCurrent).VBuffer.GetVertexQuantity()) {
                m_kLocalPt.Set(fX, fY, fZ);
                Local.get(m_iCurrent).VBuffer.SetPosition3(iPos, m_kLocalPt);
                m_kVolumePt = VOIToFileCoordinates(m_kLocalPt, true);
                Volume.get(m_iCurrent).VBuffer.SetPosition3(iPos, m_kVolumePt);
                // System.err.println( iPos + " " + m_kVolumePt.ToString() );
            }
        }

        public int size() {
            return Local.size();
        }

        public int slice(final int i) {
            return (int) Local.get(i).VBuffer.GetPosition3(0).Z;
        }

        public void Update() {
            Local.get(m_iCurrent).UpdateGS();
            Local.get(m_iCurrent).UpdateRS();
            Volume.get(m_iCurrent).UpdateGS();
            Volume.get(m_iCurrent).UpdateRS();
        }

        private Polyline createVolumePolyline(final VertexBuffer kVBuffer) {
            final int iNumPoints = kVBuffer.GetVertexQuantity();
            final VertexBuffer kVolumeVBuffer = new VertexBuffer(m_kVOIAttr, iNumPoints);
            if (iNumPoints > 0) {
                for (int i = 0; i < iNumPoints; i++) {
                    final Vector3f kPos = kVBuffer.GetPosition3(i);
                    kVolumeVBuffer.SetPosition3(i, VOIToFileCoordinates(kPos, true));
                    kVolumeVBuffer.SetColor3(0, i, m_aakColors[m_iPlaneOrientation][2]);
                }
            }
            final Polyline kPoly = new Polyline(kVolumeVBuffer, true, true);
            kPoly.AttachEffect(new VertexColor3Effect());
            kPoly.AttachGlobalState(m_kZState);
            // kPoly.UpdateGS();
            // kPoly.UpdateRS();
            return kPoly;
        }
    }

    private class LocalVolumeVOIVector extends Vector<LocalVolumeVOI> {
        /**  */
        private static final long serialVersionUID = 5551644349599464551L;

        public LocalVolumeVOIVector() {
            super();
        }

        /*public LocalVolumeVOIVector(final int initialsize) {
            super(initialsize);
        }*/
    }

    private class PolylineVector extends Vector<Polyline> {
        /**  */
        private static final long serialVersionUID = -7579112007899203250L;

        public PolylineVector() {
            super();
        }

       /* public PolylineVector(final int initialsize) {
            super(initialsize);
        }*/
    }

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2025132936439496099L;
	/** Parent user-interface and display frame. */
    protected VolumeTriPlanarInterface m_kParent = null;

    /** Camera Locations, for rendering the different Axial, Sagittal and Coronal views. */
    Vector3f[] m_akCLoc = {new Vector3f( -1.0f, 0.0f, 0.0f), new Vector3f(0.0f, -1.0f, 0.0f),
            new Vector3f(0.0f, 0.0f, -1.0f)};

    /** Camera Direction, UP, and Right vectors, for rendering the different Axial, Sagittal and Coronal views. */
    Vector3f[] m_akCoords = {new Vector3f(Vector3f.UNIT_X), new Vector3f(Vector3f.UNIT_Y),
            new Vector3f(Vector3f.UNIT_Z)};

    /** Actual image orientation. */
    protected boolean m_bPatientOrientation = true;

    /** Which dimension of the ModelImage to render. */
    protected int m_iPlaneOrientation = 0;

    /** Window-level interface. */
    protected WindowLevel m_kWinLevel;

    /** The image dimensions in x,y,z:. */
    private int[] m_aiLocalImageExtents;

    /** The image dimension factors in x,y,z:. */
    private int[] m_aiLocalImageFactors;

    /** Set of colors used to draw the axis labels. */
    private ColorRGB[][] m_aakColors = { {new ColorRGB(1, 1, 0), new ColorRGB(0, 1, 0), new ColorRGB(1, 0, 0)},
            {new ColorRGB(1, 1, 0), new ColorRGB(1, 0, 0), new ColorRGB(0, 1, 0)},
            {new ColorRGB(0, 1, 0), new ColorRGB(1, 0, 0), new ColorRGB(1, 1, 0)}};

    /** Axis labels color assignments. */
    private int[][] m_aaiColorSwap = { {2, 1, 0}, {1, 2, 0}, {1, 0, 2}};

    /** when true, the axis labels (P-> L-> S->) will be drawn */
    private boolean m_bDrawAxes = true;

    /** Turns on drawing of the X,Y bars and the Axis labels:. */
    private boolean m_bDrawXHairs = true;

    /** Change the mouse cursor with the first mouseDrag event */
    private boolean m_bFirstDrag = true;

    /**
     * True when the left mouse has been pressed, set to false when the left mouse button is released.
     */
    private boolean m_bLeftMousePressed = false;

    /**
     * Flag indicating if the right mouse button is currently pressed down:
     */
    private boolean m_bRightMousePressed = false;

    /** The current active lookup table: */
    private ModelStorageBase m_kActiveLookupTable;

    /** lower x-bound of the texture-mapped polygon: */
    private float m_fX0;

    /** upper x-bound of the texture-mapped polygon: */
    private float m_fX1;

    /**
     * Numbers dictating the size of the plane based on the extents and resolutions of the image.
     */
    private float m_fXBox, m_fYBox, m_fMaxBox;

    /** Width of the texture-mapped polygon: */
    private float m_fXRange;

    /** lower y-bound of the texture-mapped polygon: */
    private float m_fY0;

    /** upper y-bound of the texture-mapped polygon: */
    private float m_fY1;

    /** Height of the texture-mapped polygon: */
    private float m_fYRange;

    /** Image scaling from Zoom:. */
    private float m_fZoomScale = 1.0f;

    /** Which slice is currently displayed in the XY plane. */
    private int m_iSlice;

    /**
     * Current active image for manipulating the LUT by dragging with the right-mouse down.
     */
    private ModelImage m_kActiveImage;

    /** x-axis label: */
    private String m_kLabelX = new String("X");

    private String m_kLabelXDisplay = new String("X");

    /** y-axis label: */
    private String m_kLabelY = new String("Y");

    /** x-axis arrow */
    private TriMesh[] m_kXArrow;

    /** y-axis arrow */
    private TriMesh[] m_kYArrow;

    /** Drawing the axis arrows in screen-space. */
    private Camera m_spkScreenCamera;

    private int m_iLabelX_SpacingX;

    private int m_iLabelX_SpacingY;

    private int m_iLabelY_SpacingX;

    private int m_iLabelY_SpacingY;

    private boolean m_bUpdateSpacing = false;

    /** ModelImage axis orientation. */
    private int[] m_aiAxisOrder;

    /** ModelImage axis flip. */
    private boolean[] m_abAxisFlip;

    /** For zooming with the mouse. */
    private float m_fMouseX;

    private float m_fMouseY;

    private boolean m_bShowSurface = false;

    private Camera m_spkVOICamera;

    private boolean m_bDrawVOI = false;

    private boolean m_bDrawRect = false;

    private boolean m_bDrawOval = false;

    private boolean m_bDrawPolyline = false;

    private boolean m_bDrawLevelSet = false;

    private boolean m_bUpdateVOI = true;

    private boolean m_bPointer = false;

    private boolean m_bSelected = false;

    private final int m_iCirclePts = 32;

    private final double[] m_adCos = new double[m_iCirclePts];

    private final double[] m_adSin = new double[m_iCirclePts];

    private TriMesh m_kBallPoint = null;

    private ZBufferState m_kZState = null;

    private Attributes m_kVOIAttr = null;

    private int m_iCurrentVOIPoint = -1;

    private final Vector3f m_kPatientPt = new Vector3f();

    private final Vector3f m_kVolumeScale = new Vector3f();

    private final Vector3f m_kVolumeScaleInv = new Vector3f();

    private LocalVolumeVOI m_kCurrentVOI = null;

    private LocalVolumeVOI m_kCopyVOI = null;

    private LocalVolumeVOIVector[] m_kVOIList = null;

    private boolean m_bFirstVOI = true;

    private int m_iVOICount = 0;

    private final Vector3f m_kCenter = new Vector3f();

    private final PointStack levelSetStack = new PointStack(500);

    private BitSet map = null;

    private final Stack<int[]> stack = new Stack<int[]>();

    private byte[] m_aucData;

    /**
     * Default PlaneRender interface.
     */
    public PlaneRenderProstate() {
        super();
    }

    /**
     * @param kParent
     * @param kAnimator
     * @param kVolumeImageA
     * @param iPlane
     */
    public PlaneRenderProstate(final VolumeTriPlanarInterface kParent, final Animator kAnimator,
            final VolumeImage kVolumeImageA, final VolumeImage kVolumeImageB, final int iPlane) {
        super();
        m_pkRenderer = new OpenGLRenderer(m_eFormat, m_eDepth, m_eStencil, m_eBuffering, m_eMultisampling, m_iWidth,
                m_iHeight);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addGLEventListener(this);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addKeyListener(this);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseListener(this);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseMotionListener(this);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseWheelListener(this);

        m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        m_kParent = kParent;
        m_iPlaneOrientation = iPlane;

        setOrientation();
        m_kWinLevel = new WindowLevel();
    }

    /**
     * Tests the distance for closeness; finds the length of the normal from (x,y) to the line (x1,y1) (x2,y2). Returns
     * true if the distance is shorter than tol.
     * 
     * @param x x coordinate of point to be tested
     * @param x1 x coordinate of first point in line
     * @param x2 x coordinate of second point in line
     * @param y y coordinate of point to be tested
     * @param y1 y coordinate of first point in line
     * @param y2 y coordinate of second point in line
     * @param tol distance to test against
     * 
     * @return true if the distance is shorter than tol.
     */
    public static synchronized boolean testDistance(final int x, final int x1, final int x2, final int y, final int y1,
            final int y2, final double tol) {

        // double hVx, hVy, aVx, aVy;
        double lenH, lenH2, lenA, lenO;
        lenH = MipavMath.distance(x1, x, y1, y);

        if (lenH <= 0) {
            return false;
        }

        lenH2 = MipavMath.distance(x, x2, y, y2);
        lenA = MipavMath.distance(x1, x2, y1, y2);

        if (lenA <= 0) {
            return false;
        }

        // hVx = x - x1;
        // hVy = y - y1;
        // aVx = x2 - x1;
        // aVy = y2 - y1;
        // theta = Math.acos((hVx*aVx + hVy*aVy)/(lenH*lenA));
        // lenO = lenH * Math.sin(theta);// * (lenH+lenH2+lenA);
        // The above reduces to:
        lenO = Math.abs( ( (y1 - y2) * x) + ( (x2 - x1) * y) + ( (x1 * y2) - (y1 * x2))) / lenA;

        if ( (lenO < tol) && (lenH < lenA) && (lenH2 < lenA)) {
            return true;
        }

        return false;
    }

    /**
     * Adds the VolumeSlices object to the display list for rendering.
     * 
     * @param kVolumeSlice.
     */
    public void addSlices(final VolumeSlices kVolumeSlice) {
        m_kDisplayList.add(kVolumeSlice);
        m_kTranslate = kVolumeSlice.GetTranslate();
    }

    /**
     * Closes the frame.
     */
    public void close() {
        disposeLocal();
    }

    /**
     * Determines if the supplied point can be found within the points that define the contour.
     * 
     * @return true if point is within the contour
     */
    public boolean contains(final int iX, final int iY, final VertexBuffer kVBuffer) {
        boolean isInside = false;
        final float fX = iX + 0.49f; // Matt add doc !!!
        final float fY = iY + 0.49f;

        final int iNumPoints = kVBuffer.GetVertexQuantity();
        int iLast = iNumPoints - 1;
        if (iNumPoints > 0) {
            for (int i = 0; i < iNumPoints; i++) {
                final Vector3f kPos = kVBuffer.GetPosition3(i);
                final Vector3f kPosL = kVBuffer.GetPosition3(iLast);

                if ( ( (kPosL.Y <= fY) && (fY < kPos.Y) && (areaTwice(kPos.X, kPos.Y, kPosL.X, kPosL.Y, fX, fY) >= 0))
                        || ( (kPos.Y <= fY) && (fY < kPosL.Y) && (areaTwice(kPosL.X, kPosL.Y, kPos.X, kPos.Y, fX, fY) >= 0))) {
                    isInside = !isInside;
                }

                iLast = i;
            }
        }
        return isInside;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.GLEventListener#display(javax.media.opengl.GLAutoDrawable)
     */
    public void display(final GLAutoDrawable arg0) {
        if ( !m_bModified) {
            return;
        }

        if (m_kVolumeImageA == null) {
            return;
        }
        if ( !m_bInit) {
            init(arg0);
        }

        m_bModified = false;
        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene()) {
            for (int i = 0; i < m_kDisplayList.size(); i++) {
                final boolean bDisplaySave = m_kDisplayList.get(i).GetDisplay();
                final Matrix3f kSave = new Matrix3f(m_kDisplayList.get(i).GetScene().Local.GetRotate());
                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(Matrix3f.IDENTITY);
                m_kDisplayList.get(i).SetDisplay(true);

                float fBlend = 1;
                final boolean[] bShowBoundingBox = new boolean[] {true, true, true};
                final boolean[] bShowSlice = new boolean[] {true, true, true};
                VolumeSlices kSlices = null;
                if (m_kDisplayList.get(i) instanceof VolumeSlices) {
                    kSlices = (VolumeSlices) m_kDisplayList.get(i);
                    fBlend = kSlices.GetSliceOpacity(m_iPlaneOrientation);
                    kSlices.SetSliceOpacity(m_iPlaneOrientation, 1);
                    kSlices.ShowSurface(m_bShowSurface);
                    for (int j = 0; j < 3; j++) {
                        bShowBoundingBox[j] = kSlices.GetShowBoundingBox(j);
                        bShowSlice[j] = kSlices.GetShowSlice(j);
                        kSlices.ShowBoundingBox(j, m_bDrawXHairs);
                        kSlices.ShowSlice(j, true);
                    }
                }
                if (m_kDisplayList.get(i) instanceof VolumeNode) {
                    final VolumeNode kNode = (VolumeNode) m_kDisplayList.get(i);
                    if (kNode.GetNode().GetChild(0) instanceof Polyline) {
                        final Polyline kPoly = (Polyline) kNode.GetNode().GetChild(0);
                        final Vector3f kPos = FileCoordinatesToVOI(kPoly.VBuffer.GetPosition3(0), true);
                        // System.err.print( m_iPlaneOrientation + " " + kPos.Z + " " + m_kCenter.Z );
                        if (Math.abs(kPos.Z - m_kCenter.Z) < Mathf.ZERO_TOLERANCE) {
                            kNode.Render(m_pkRenderer, m_kCuller, false, false);
                            // System.err.print(" Draw VOI" );
                        }
                        // System.err.println( "" );
                    }
                } else {
                    m_kDisplayList.get(i).Render(m_pkRenderer, m_kCuller, false, true);
                }

                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(kSave);
                m_kDisplayList.get(i).SetDisplay(bDisplaySave);

                if (kSlices != null) {
                    kSlices.SetSliceOpacity(m_iPlaneOrientation, fBlend);
                    for (int j = 0; j < 3; j++) {
                        kSlices.ShowBoundingBox(j, bShowBoundingBox[j]);
                        kSlices.ShowSlice(j, bShowSlice[j]);
                    }
                    kSlices.ShowSurface(false);
                }
            }
            drawAxes();
            drawVOI();
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();

    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#displayChanged(javax.media.opengl.GLAutoDrawable,
     *      boolean, boolean)
     */
    public void displayChanged(final GLAutoDrawable arg0, final boolean arg1, final boolean arg2) {
        m_bModified = true;
    }

    public void displaySurface(final boolean bOn) {
        m_bShowSurface = bOn;
    }

    /**
     * Clean memory.
     */
    public void disposeLocal() {
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                m_aakColors[i][j] = null;
            }
            m_aakColors[i] = null;
        }
        m_aakColors = null;

        m_akCLoc = null;
        m_akCoords = null;

        if (m_kWinLevel != null) {
            m_kWinLevel.disposeLocal();
            m_kWinLevel = null;
        }

        m_aiLocalImageExtents = null;

        m_aaiColorSwap = null;

        m_kActiveLookupTable = null;

        m_kActiveImage = null;
        m_kLabelX = null;
        m_kLabelXDisplay = null;
        m_kLabelY = null;

        if (m_kXArrow != null) {
            for (final TriMesh element : m_kXArrow) {
                element.dispose();
            }
            m_kXArrow = null;
        }

        if (m_kYArrow != null) {
            for (final TriMesh element : m_kYArrow) {
                element.dispose();
            }
            m_kYArrow = null;
        }

        if (m_spkScreenCamera != null) {
            m_spkScreenCamera.dispose();
            m_spkScreenCamera = null;
        }
        m_aiAxisOrder = null;
        m_abAxisFlip = null;

        super.dispose();
    }

    public void doVOI(final String kCommand) {
        if (m_bFirstVOI) {
            m_kZState = new ZBufferState();
            m_kZState.Compare = ZBufferState.CompareMode.CF_ALWAYS;
            m_kVOIAttr = new Attributes();
            m_kVOIAttr.SetPChannels(3);
            m_kVOIAttr.SetCChannels(0, 3);

            m_bFirstVOI = false;
            final StandardMesh kSDMesh = new StandardMesh(m_kVOIAttr);
            kSDMesh.SetInside(true);
            m_kBallPoint = kSDMesh.Box(1.0f / 100f, 1.0f / 100f, 1.0f / 100f);
            m_kBallPoint.AttachEffect(new VertexColor3Effect());
            for (int i = 0; i < m_kBallPoint.VBuffer.GetVertexQuantity(); i++) {
                m_kBallPoint.VBuffer.SetColor3(0, i, 1, 1, 1);
            }
            m_kBallPoint.AttachGlobalState(m_kZState);
            final WireframeState kWState = new WireframeState();
            kWState.Fill = WireframeState.FillMode.FM_LINE;
            m_kBallPoint.AttachGlobalState(kWState);
            m_kBallPoint.UpdateRS();

            for (int i = 0; i < m_iCirclePts; i++) {
                m_adCos[i] = Math.cos(Math.PI * 2.0 * i / m_iCirclePts);
                m_adSin[i] = Math.sin(Math.PI * 2.0 * i / m_iCirclePts);
            }
        }

        if (kCommand.equals("RectVOI")) {
            m_bDrawVOI = true;
            m_bPointer = false;
            m_kCurrentVOI = null;
            m_bSelected = false;
            m_bDrawRect = true;
            m_bDrawOval = false;
            m_bDrawPolyline = false;
            m_bDrawLevelSet = false;
        } else if (kCommand.equals("EllipseVOI")) {
            m_bDrawVOI = true;
            m_bPointer = false;
            m_kCurrentVOI = null;
            m_bSelected = false;
            m_bDrawRect = false;
            m_bDrawOval = true;
            m_bDrawPolyline = false;
            m_bDrawLevelSet = false;

        } else if (kCommand.equals("Polyline")) {
            m_bDrawVOI = true;
            m_bPointer = false;
            m_kCurrentVOI = null;
            m_bSelected = false;
            m_bDrawRect = false;
            m_bDrawOval = false;
            m_bDrawPolyline = true;
            m_bDrawLevelSet = false;
        } else if (kCommand.equals("VOIColor")) {} else if (kCommand.equals("LevelSetVOI")) {
            m_bDrawVOI = true;
            m_bPointer = false;
            m_kCurrentVOI = null;
            m_bSelected = false;
            m_bDrawRect = false;
            m_bDrawOval = false;
            m_bDrawPolyline = false;
            m_bDrawLevelSet = true;
        } else if (kCommand.equals("deleteAllVOI")) {
            deleteAllVOI();
        } else if (kCommand.equals("deleteVOI")) {
            deleteVOI();
        } else if (kCommand.equals("cutVOI")) {
            copyVOI();
            deleteVOI();
        } else if (kCommand.equals("copyVOI")) {
            copyVOI();
        } else if (kCommand.equals("pasteVOI")) {
            pasteVOI(m_iSlice);
        } else if (kCommand.equals("PropVOIUp")) {
            if (m_kCurrentVOI == null) {
                return;
            }
            if (m_iSlice + 1 < m_aiLocalImageExtents[2]) {
                copyVOI();
                pasteVOI(m_iSlice + 1);
                m_kPatientPt.Z++;
                final Vector3f volumePt = new Vector3f();
                MipavCoordinateSystems.patientToFile(m_kPatientPt, volumePt, m_kVolumeImageA.GetImage(),
                        m_iPlaneOrientation);
                m_kParent.setSliceFromPlane(volumePt);
            }
        } else if (kCommand.equals("PropVOIDown")) {
            if (m_kCurrentVOI == null) {
                return;
            }
            if (m_iSlice - 1 >= 0) {
                copyVOI();
                pasteVOI(m_iSlice - 1);
                m_kPatientPt.Z--;
                final Vector3f volumePt = new Vector3f();
                MipavCoordinateSystems.patientToFile(m_kPatientPt, volumePt, m_kVolumeImageA.GetImage(),
                        m_iPlaneOrientation);
                m_kParent.setSliceFromPlane(volumePt);
            }
        } else if (kCommand.equals("PropVOIAll")) {
            if (m_kCurrentVOI == null) {
                return;
            }
            copyVOI();
            for (int i = 0; i < m_aiLocalImageExtents[2]; i++) {
                if (i != m_iSlice) {
                    pasteVOI(i);
                }
            }
        } else if (kCommand.equals("Pointer")) {
            m_bDrawVOI = false;
            m_bPointer = true;
        } else if (kCommand.equals("Default")) {
            m_bDrawVOI = false;
            m_bPointer = false;
        }
        if (m_kCurrentVOI != null) {
            m_kCurrentVOI.Update();
        }
        m_bModified = true;
        GetCanvas().display();
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#GetCanvas()
     */
    public GLCanvas GetCanvas() {
        return ((OpenGLRenderer) m_pkRenderer).GetCanvas();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.GLEventListener#init(javax.media.opengl.GLAutoDrawable)
     */
    public void init(final GLAutoDrawable arg0) {
        if (m_kVolumeImageA == null) {
            return;
        }

        m_bInit = true;

        arg0.setAutoSwapBufferMode(false);

        ((OpenGLRenderer) m_pkRenderer).SetDrawable(arg0);
        ((OpenGLRenderer) m_pkRenderer).InitializeState();

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f, 1.0f, 0.1f, 100.0f);
        m_pkRenderer.OnFrustumChange();

        final Vector3f kCLoc = new Vector3f(m_akCLoc[m_aiAxisOrder[2]]);
        final Vector3f kCDir = new Vector3f(m_akCoords[m_aiAxisOrder[2]]);
        final Vector3f kCUp = new Vector3f(m_akCoords[m_aiAxisOrder[1]]);
        final Vector3f kCRight = new Vector3f(m_akCoords[m_aiAxisOrder[0]]);
        if (m_abAxisFlip[2]) {
            kCLoc.Scale( -1);
            kCDir.Scale( -1);
        }
        if (m_abAxisFlip[1]) {
            kCUp.Scale( -1);
        }
        if (m_abAxisFlip[0]) {
            kCRight.Scale( -1);
        }
        // invert y-axis
        kCUp.Scale( -1);
        m_spkCamera.SetFrame(kCLoc, kCDir, kCUp, kCRight);
        CreateScene();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);

        InitializeCameraMotion(.05f, 0.001f);
        InitializeObjectMotion(m_spkScene);

        m_kAnimator.add(GetCanvas());
        m_kParent.setSliceFromPlane(new Vector3f( (m_kVolumeImageA.GetImage().getExtents()[0] - 1) / 2.0f,
                (m_kVolumeImageA.GetImage().getExtents()[1] - 1) / 2.0f,
                (m_kVolumeImageA.GetImage().getExtents()[2] - 1) / 2.0f));
    }

    /**
     * keyPressed callback.
     * 
     * @param kKey the KeyEvent triggering the callback.
     */
    public void keyPressed(final KeyEvent kKey) {
        final char ucKey = kKey.getKeyChar();
        final int iKey = kKey.getKeyCode();
        if (ucKey == KeyEvent.VK_DELETE) {
            deleteVOI();
        }
    }

    public void make3DVOI(final boolean bIntersection, final ModelImage kVolume, final int iValue) {
        if (m_kVOIList != null) {
            for (int i = 0; i < m_aiLocalImageExtents[2]; i++) {
                if (m_kVOIList[i] != null) {
                    for (int j = 0; j < m_kVOIList[i].size(); j++) {
                        fillVolume(i, m_kVOIList[i].get(j).getLocal(0), kVolume, bIntersection, iValue);
                        m_kDisplayList.remove(m_kParent.removeNode(m_kVOIList[i].get(j).Name.get(0)));
                    }
                    m_kVOIList[i].clear();
                    m_kVOIList[i] = null;
                }
            }
        }
        m_kCopyVOI = null;
        m_kCurrentVOI = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseDragged(java.awt.event.MouseEvent)
     */
    public void mouseDragged(final MouseEvent kEvent) {
        // super.mouseDragged(kEvent);

        /*
         * If the right mouse button is pressed and dragged. processRightMouseDrag updates the HistoLUT window and level
         * (contrast and brightness)
         */

        if (m_bRightMousePressed && !kEvent.isShiftDown()) {
            processRightMouseDrag(kEvent);
            SetModified(true);

        }

        /*
         * Dragging the mouse with the left-mouse button held down changes the positions of the X and Y cross bars, and
         * therefor the ZSlice positions of the associated PlaneRenderWM objects and the TriPlanar Surface. The new
         * positions are calculated and passed onto the parent frame.
         */

        else if (m_bLeftMousePressed && !kEvent.isShiftDown()) {
            processLeftMouseDrag(kEvent);
        } else {
            if (kEvent.getY() < m_fMouseY) {
                m_fZoomScale += 0.05;
            } else if (kEvent.getY() > m_fMouseY) {
                m_fZoomScale -= 0.05;
            }
            zoom();
        }
    }

    public void mouseMoved(final MouseEvent kEvent) {
        if (m_bPointer) {
            showSelectedVOI(kEvent.getX(), kEvent.getY());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mousePressed(java.awt.event.MouseEvent)
     */
    public void mousePressed(final MouseEvent kEvent) {
        super.mousePressed(kEvent);
        m_fMouseX = kEvent.getX();
        m_fMouseY = kEvent.getY();
        // System.err.println( m_fMouseX + " " + m_fMouseY );
        /* If the button pressed is the left mouse button: */
        if ( (kEvent.getButton() == MouseEvent.BUTTON1) && !kEvent.isShiftDown()) {
            if (m_bPointer) {
                if (m_kParent.getCursor() == MipavUtil.crosshairCursor) {
                    moveVOIPoint(kEvent.getX(), kEvent.getY());
                } else if (m_kParent.getCursor() == MipavUtil.addPointCursor) {
                    addVOIPoint(kEvent.getX(), kEvent.getY());
                } else {
                    selectVOI(kEvent.getX(), kEvent.getY());
                }
            }

            m_bLeftMousePressed = true;
            processLeftMouseDrag(kEvent);
            m_bModified = true;
            GetCanvas().display();
        }

        if ( (kEvent.getButton() == MouseEvent.BUTTON3) && !kEvent.isShiftDown()) {
            m_bRightMousePressed = true;
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseReleased(java.awt.event.MouseEvent)
     */
    public void mouseReleased(final MouseEvent kEvent) {

        super.mousePressed(kEvent);
        /*
         * If the button pressed is the left mouse button turn off the m_bLeftMousePressed flag:
         */
        if ( (kEvent.getButton() == MouseEvent.BUTTON3) && !kEvent.isShiftDown()) {
            m_bRightMousePressed = false;
        }

        if ( (kEvent.getButton() == MouseEvent.BUTTON1) && !kEvent.isShiftDown()) {
            processLeftMouseDrag(kEvent);
            m_bLeftMousePressed = false;
        }
        m_bFirstDrag = true;
        if ( !m_bPointer) {
            m_kParent.setDefaultCursor();
        }
        m_bDrawVOI = false;
        if (m_bUpdateVOI) {
            saveVOI(kEvent.getX(), kEvent.getY(), m_iSlice);
            m_bModified = true;
            GetCanvas().display();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.MouseWheelListener#mouseWheelMoved(java.awt.event.MouseWheelEvent)
     */
    public void mouseWheelMoved(final MouseWheelEvent e) {
        if (e.getWheelRotation() == 1) {
            m_fZoomScale -= 0.05;
        } else {
            m_fZoomScale += 0.05;
        }
        zoom();
    }

    /**
     * Tests if a point is near the curve.
     * 
     * @param x x coordinate of point
     * @param y y coordinate of point
     * @param tol tolerance indicating the capture range to the line the point.
     * 
     * @return returns boolean result of test
     */
    public boolean nearLine(final int iX, final int iY, final VertexBuffer kVBuffer) {

        final Vector3f kVOIPoint = new Vector3f(iX, m_iHeight - iY, 0);
        int i;
        int x1, y1, x2, y2;
        final int isize = kVBuffer.GetVertexQuantity();
        for (i = 0; i < (isize - 1); i++) {
            final Vector3f kPos0 = kVBuffer.GetPosition3(i);
            x1 = MipavMath.round(kPos0.X);
            y1 = MipavMath.round(kPos0.Y);
            final Vector3f kPos1 = kVBuffer.GetPosition3(i + 1);
            x2 = MipavMath.round(kPos1.X);
            y2 = MipavMath.round(kPos1.Y);

            if (PlaneRenderProstate.testDistance((int) kVOIPoint.X, x1, x2, (int) kVOIPoint.Y, y1, y2, 3)) {
                m_iCurrentVOIPoint = i;
                return true;
            }
        }

        final Vector3f kPos0 = kVBuffer.GetPosition3(0);
        x1 = MipavMath.round(kPos0.X);
        y1 = MipavMath.round(kPos0.Y);
        final Vector3f kPos1 = kVBuffer.GetPosition3(isize - 1);
        x2 = MipavMath.round(kPos1.X);
        y2 = MipavMath.round(kPos1.Y);

        if (PlaneRenderProstate.testDistance((int) kVOIPoint.X, x1, x2, (int) kVOIPoint.Y, y1, y2, 3)) {
            m_iCurrentVOIPoint = i;
            return true;
        }
        m_iCurrentVOIPoint = -1;
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.nih.mipav.view.renderer.WildMagic.GPURenderBase#reshape(javax.media.opengl.GLAutoDrawable, int, int,
     *      int, int)
     */
    public void reshape(final GLAutoDrawable arg0, final int iX, final int iY, final int iWidth, final int iHeight) {
        if (m_kVolumeImageA == null) {
            return;
        }

        if (iWidth > 0 && iHeight > 0) {
            if (m_bUpdateSpacing) {
                m_iLabelX_SpacingX *= (float) iWidth / (float) m_iWidth;
                m_iLabelX_SpacingY *= (float) iHeight / (float) m_iHeight;
                m_iLabelY_SpacingX *= (float) iWidth / (float) m_iWidth;
                m_iLabelY_SpacingY *= (float) iHeight / (float) m_iHeight;
            }
            m_bUpdateSpacing = true;

            if (m_pkRenderer != null) {
                m_pkRenderer.Resize(iWidth, iHeight);
            }

            m_iWidth = iWidth;
            m_iHeight = iHeight;
            m_bModified = true;
            m_spkCamera.Perspective = false;
            float fRMax = (m_fZoomScale * Math.max(m_fX, m_fY)) / 2.0f;
            float fUMax = fRMax * iHeight / iWidth;
            m_spkCamera.SetFrustum( -fRMax, fRMax, -fUMax, fUMax, 1f, 5.0f);
            m_pkRenderer.OnFrustumChange();
        }

    }

    /**
     * Sets the background color for the frame and rendered image.
     * 
     * @param color RGBA color to use as the background color.
     */
    public void setBackgroundColor(final ColorRGBA kColor) {
        m_kBackgroundColor = kColor;
        return;
    }

    /**
     * setCenter sets the cursor and slice position for this PlaneRenderWM object, based on the 3D location of the three
     * intersecting ModelImage planes.
     * 
     * @param center the 3D center in FileCoordinates of the three intersecting ModelImage planes.
     */
    public void setCenter(final Vector3f center) {
        final ModelImage kImage = m_kVolumeImageA.GetImage();
        m_bModified = true;
        MipavCoordinateSystems.fileToPatient(center, m_kPatientPt, kImage, m_iPlaneOrientation);
        setSlice(m_kPatientPt.Z);
        GetCanvas().display();
        m_kCenter.Mult(m_kPatientPt, m_kVolumeScale);
    }

    /**
     * Causes re-display.
     * 
     * @param bModified
     */
    public void SetModified(final boolean bModified) {
        m_bModified = bModified;
        GetCanvas().display();
    }

    /**
     * Sets the view to Radiological (true) or Neurological (false) view.
     * 
     * @param bOn
     */
    public void setRadiologicalView(boolean bOn) {
        if (m_iPlaneOrientation == FileInfoBase.SAGITTAL) {
            return;
        }

        final Vector3f kCLoc = new Vector3f(m_akCLoc[m_aiAxisOrder[2]]);
        final Vector3f kCDir = new Vector3f(m_akCoords[m_aiAxisOrder[2]]);
        final Vector3f kCUp = new Vector3f(m_akCoords[m_aiAxisOrder[1]]);
        final Vector3f kCRight = new Vector3f(m_akCoords[m_aiAxisOrder[0]]);
        if (m_abAxisFlip[2]) {
            kCLoc.Scale( -1);
            kCDir.Scale( -1);
        }
        if (m_abAxisFlip[1]) {
            kCUp.Scale( -1);
        }
        if (m_abAxisFlip[0]) {
            kCRight.Scale( -1);
        }
        // invert y-axis
        kCUp.Scale( -1);
        if ( !bOn) {
            kCLoc.Scale( -1);
            kCDir.Scale( -1);
            kCRight.Scale( -1);
        }
        m_spkCamera.SetFrame(kCLoc, kCDir, kCUp, kCRight);
    }

    /**
     * Sets the color for the PlaneRender iView (AXIAL, SAGITTAL, CORONAL) slice.
     * 
     * @param iView (AXIAL, SAGITTAL, CORONAL)
     * @param kColor the new axis color attribute.
     */
    public void setSliceHairColor(final int iView, final ColorRGB kColor) {
        final int iX = 0;
        final int iY = 1;
        m_bModified = true;

        m_aakColors[m_iPlaneOrientation][m_aaiColorSwap[m_iPlaneOrientation][iView]] = kColor;
        final ColorRGB kXSliceHairColor = m_aakColors[m_iPlaneOrientation][iX];
        final ColorRGB kYSliceHairColor = m_aakColors[m_iPlaneOrientation][iY];

        for (int j = 0; j < 4; j++) {
            m_kXArrow[0].VBuffer.SetColor3(0, j, kXSliceHairColor);
            m_kYArrow[0].VBuffer.SetColor3(0, j, kYSliceHairColor);
        }
        for (int j = 0; j < 3; j++) {
            m_kXArrow[1].VBuffer.SetColor3(0, j, kXSliceHairColor);
            m_kYArrow[1].VBuffer.SetColor3(0, j, kYSliceHairColor);
        }
        for (int i = 0; i < 2; i++) {
            m_kXArrow[i].VBuffer.Release();
            m_kYArrow[i].VBuffer.Release();
        }
    }

    /**
     * Turns displaying the Axis labels on or off:
     * 
     * @param bShow when true display the axis lablels, when false hide the axis labels
     */
    public void showAxes(final boolean bShow) {
        m_bDrawAxes = bShow;
    }

    /**
     * Turns displaying the X and Y bars on or off:
     * 
     * @param bShow when true show the cross-hairs when false hide the cross-hairs
     */
    public void showXHairs(final boolean bShow) {
        if (m_bDrawXHairs != bShow) {
            m_bDrawXHairs = bShow;
        }
    }

    /**
     * fill: fill the sculpt outline drawn by the user. Pixels are determined to be inside or outside the sculpt region
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     * 
     * @param aaiCrossingPoints DOCUMENT ME!
     * @param aiNumCrossings DOCUMENT ME!
     */
    protected void fill(final int[][] aaiCrossingPoints, final int[] aiNumCrossings, final int iXMin, final int iYMin,
            final int iXMax, final int iYMax, final int iZ, final ModelImage kVolume, final boolean bIntersection,
            final int iValue) {
        final Vector3f kLocalPt = new Vector3f();
        Vector3f kVolumePt = new Vector3f();
        int iColumn = 0;
        // System.err.println( "fill " + iZ );
        /* Loop over the width of the sculpt region bounding-box: */
        for (int iX = iXMin; iX < iXMax; iX++) {
            boolean bInside = false;

            /* Loop over the height of the sculpt region bounding-box: */
            for (int iY = iYMin; iY < iYMax; iY++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {

                    if (iY == aaiCrossingPoints[iColumn][iCross]) {

                        /*
                         * Each time an edge is cross the point alternates from outside to inside:
                         */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {

                    /*
                     * The current pixel is inside the sculpt region. Get the image color from the canvas image and
                     * alpha-blend the sculpt color ontop, storing the result in the canvas image.
                     */
                    kLocalPt.Set(iX, iY, iZ);
                    kVolumePt = VOIToFileCoordinates(kLocalPt, false);
                    if (bIntersection) {
                        final int iTemp = kVolume.getInt((int) kVolumePt.X, (int) kVolumePt.Y, (int) kVolumePt.Z);
                        if (iValue == 0) {
                            kVolume.set((int) kVolumePt.X, (int) kVolumePt.Y, (int) kVolumePt.Z, 85);
                        } else if (iTemp != 0) {
                            kVolume.set((int) kVolumePt.X, (int) kVolumePt.Y, (int) kVolumePt.Z, 255);
                        }
                    } else {
                        kVolume.set((int) kVolumePt.X, (int) kVolumePt.Y, (int) kVolumePt.Z, 255);
                        // System.err.println( iZ );
                    }
                }
            }

            iColumn++;
        }
    }

    /**
     * This function computes the set of spans indicated by column crossings for the sculpt outline drawn by the user,
     * by doing a polygon scan conversion in gridded space. The outline must be closed with last point = first point.
     * 
     * @param aaiCrossingPoints DOCUMENT ME!
     * @param aiNumCrossings DOCUMENT ME!
     */
    protected void outlineRegion(final int[][] aaiCrossingPoints, final int[] aiNumCrossings, final int iXMin,
            final int iYMin, final int iXMax, final int iYMax, final Vector3f[] kVolumePts, final ModelImage kVolume) {
        final int iNumPts = kVolumePts.length;

        /*
         * nudge the vertices off of the exact integer coords by a factor of 0.1 to avoid vertices on pixel centers,
         * which would create spans of zero length
         */
        final double dNudge = 0.1;
        double[][][] aaadEdgeList = new double[iNumPts][2][2];

        for (int iPoint = 0; iPoint < (iNumPts - 1); iPoint++) {
            aaadEdgeList[iPoint][0][0] = kVolumePts[iPoint].X - dNudge;
            aaadEdgeList[iPoint][0][1] = kVolumePts[iPoint].Y - dNudge;
            aaadEdgeList[iPoint][1][0] = kVolumePts[iPoint + 1].X - dNudge;
            aaadEdgeList[iPoint][1][1] = kVolumePts[iPoint + 1].Y - dNudge;
        }

        /*
         * Compute the crossing points for this column and produce spans.
         */
        for (int iColumn = iXMin; iColumn <= iXMax; iColumn++) {
            final int iIndex = iColumn - iXMin;

            /*
             * for each edge, figure out if it crosses this column and add its crossing point to the list if so.
             */
            aiNumCrossings[iIndex] = 0;

            for (int iPoint = 0; iPoint < (iNumPts - 1); iPoint++) {
                final double dX0 = aaadEdgeList[iPoint][0][0];
                final double dX1 = aaadEdgeList[iPoint][1][0];
                final double dY0 = aaadEdgeList[iPoint][0][1];
                final double dY1 = aaadEdgeList[iPoint][1][1];
                final double dMinX = (dX0 <= dX1) ? dX0 : dX1;
                final double dMaxX = (dX0 > dX1) ? dX0 : dX1;

                if ( (dMinX < iColumn) && (dMaxX > iColumn)) {

                    /*
                     * The edge crosses this column, so compute the intersection.
                     */
                    final double dDX = dX1 - dX0;
                    final double dDY = dY1 - dY0;
                    final double dM = (dDX == 0) ? 0 : (dDY / dDX);
                    final double dB = (dDX == 0) ? 0 : ( ( (dX1 * dY0) - (dY1 * dX0)) / dDX);

                    final double dYCross = (dM * iColumn) + dB;
                    final double dRound = 0.5;
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (dYCross < 0) ? (int) (dYCross - dRound)
                            : (int) (dYCross + dRound);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }

        aaadEdgeList = null;
    }

    /**
     * Based on the orientation of the ModelImage, sets up the index parameters, m_aiLocalImageExtents[0],
     * m_aiLocalImageExtents[1], and m_aiLocalImageExtents[2], the drawing colors for the z box, x and y bars, and the
     * invert flags.
     * 
     * <p>
     * Once setup everything is rendered into an x,y plane where x,y may be any of the original x,y, or z dimensions in
     * the original ModelImage.
     * </p>
     */
    protected void setOrientation() {
        final ModelImage kImage = m_kVolumeImageA.GetImage();
        m_aiAxisOrder = MipavCoordinateSystems.getAxisOrder(kImage, m_iPlaneOrientation);
        m_abAxisFlip = MipavCoordinateSystems.getAxisFlip(kImage, m_iPlaneOrientation);
        m_aiLocalImageExtents = kImage.getExtents(m_iPlaneOrientation);
        m_aiLocalImageFactors = kImage.getExtentsSize(m_iPlaneOrientation);
        map = new BitSet(m_aiLocalImageExtents[0] * m_aiLocalImageExtents[1]);

        final float[] afResolutions = kImage.getResolutions(0, m_iPlaneOrientation);

        if ( (afResolutions[0] == 0.0f) || (afResolutions[1] == 0.0f) || (afResolutions[2] == 0.0f)) {
            afResolutions[0] = 1.0f;
            afResolutions[1] = 1.0f;
            afResolutions[2] = 1.0f;
        }

        m_fXBox = (m_aiLocalImageExtents[0] - 1) * afResolutions[0];
        m_fYBox = (m_aiLocalImageExtents[1] - 1) * afResolutions[1];

        m_fMaxBox = m_fXBox;

        if (m_fYBox > m_fMaxBox) {
            m_fMaxBox = m_fYBox;
        }

        final float fMaxZ = (m_aiLocalImageExtents[2] - 1) * afResolutions[2];
        float fMax = m_fMaxBox;
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        m_fX = m_fXBox / fMax;
        m_fY = m_fYBox / fMax;
        m_fZ = fMaxZ / fMax;

        if (kImage.getImageOrientation() != FileInfoBase.UNKNOWN_ORIENT) {
            if ( (m_iPlaneOrientation == FileInfoBase.AXIAL) || (m_iPlaneOrientation == FileInfoBase.CORONAL)) {
                m_kLabelX = new String("L");
            } else {
                m_kLabelX = new String("P");
            }

            if ( (m_iPlaneOrientation == FileInfoBase.SAGITTAL) || (m_iPlaneOrientation == FileInfoBase.CORONAL)) {
                m_kLabelY = new String("S");
            } else {
                m_kLabelY = new String("P");
            }
        } else {
            m_bPatientOrientation = false;
            if (m_iPlaneOrientation == FileInfoBase.SAGITTAL) {
                m_kLabelX = new String("Z");
                m_kLabelY = new String("Y");
            } else if (m_iPlaneOrientation == FileInfoBase.CORONAL) {
                m_kLabelX = new String("X");
                m_kLabelY = new String("Z");
            }
        }

        m_kLabelXDisplay = new String(m_kLabelX);
        if ( !m_kVolumeImageA.GetImage().getRadiologicalView() && (m_iPlaneOrientation != FileInfoBase.SAGITTAL)) {
            if ( !m_bPatientOrientation) {
                m_kLabelXDisplay = new String("-X");
            } else {
                m_kLabelXDisplay = new String("R");
            }
        }
        if (m_iPlaneOrientation == FileInfoBase.AXIAL) {
            m_iLabelX_SpacingX = 50;
            m_iLabelX_SpacingY = 20;
            m_iLabelY_SpacingX = 10;
            m_iLabelY_SpacingY = 68;
        } else {
            m_iLabelX_SpacingX = 50;
            m_iLabelX_SpacingY = 10;
            m_iLabelY_SpacingX = 10;
            m_iLabelY_SpacingY = 55;
        }

        final ModelImage kImageA = m_kVolumeImageA.GetImage();
        // System.err.println( m_iPlaneOrientation + " " + m_fX + " " + m_fY + " " + m_fZ + " " + fMax );

        m_kVolumeScale.Set(m_kVolumeImageA.GetScaleX() / (kImageA.getExtents()[0] - 1), m_kVolumeImageA.GetScaleY()
                / (kImageA.getExtents()[1] - 1), m_kVolumeImageA.GetScaleZ() / (kImageA.getExtents()[2] - 1));
        m_kVolumeScaleInv.Copy(m_kVolumeScale);
        m_kVolumeScaleInv.Invert();

        m_kCenter.Mult(m_kVolumeScale);

        initDataBuffer();
    }

    /**
     * Sorts the edge crossing points in place.
     * 
     * @param aiList list of positions
     * @param iNumElements number of positions.
     */
    protected void sortCrossingPoints(final int[] aiList, final int iNumElements) {
        boolean bDidSwap = true;

        while (bDidSwap) {
            bDidSwap = false;

            for (int iPoint = 0; iPoint < (iNumElements - 1); iPoint++) {

                if (aiList[iPoint] > aiList[iPoint + 1]) {
                    final int iTmp = aiList[iPoint];
                    aiList[iPoint] = aiList[iPoint + 1];
                    aiList[iPoint + 1] = iTmp;
                    bDidSwap = true;
                }
            }
        }
    }

    private void addVOIPoint(final int iX, final int iY) {
        if (m_kCurrentVOI == null) {
            return;
        }
        final float fY = m_iHeight - iY;
        final VertexBuffer kVBuffer = new VertexBuffer(m_kCurrentVOI.getLocal(0).VBuffer.GetAttributes(), m_kCurrentVOI
                .getLocal(0).VBuffer.GetVertexQuantity() + 1);
        final int iNumPoints = kVBuffer.GetVertexQuantity();
        if (iNumPoints > 0) {
            int i = 0;
            int iCount = 0;
            while (i < iNumPoints) {
                kVBuffer.SetColor3(0, i, m_aakColors[m_iPlaneOrientation][2]);
                kVBuffer.SetPosition3(i++, m_kCurrentVOI.getLocal(0).VBuffer.GetPosition3(iCount++));
                if (i == (m_iCurrentVOIPoint + 1)) {
                    kVBuffer.SetColor3(0, i, m_aakColors[m_iPlaneOrientation][2]);
                    kVBuffer.SetPosition3(i++, iX, fY, m_iSlice);
                }
            }
            m_bUpdateVOI = true;
            m_kVOIList[m_iSlice].remove(m_kCurrentVOI);
            m_kDisplayList.remove(m_kParent.removeNode(m_kCurrentVOI.Name.get(0)));

            final Polyline kRectVOI = new Polyline(kVBuffer, true, true);
            kRectVOI.AttachEffect(new VertexColor3Effect());
            kRectVOI.SetName(m_kCurrentVOI.getLocal(0).GetName());
            kRectVOI.AttachGlobalState(m_kZState);

            m_kCurrentVOI = new LocalVolumeVOI(kRectVOI, kRectVOI.GetName());
            m_kVOIList[m_iSlice].add(m_kCurrentVOI);

            final Node kNode = new Node();
            kNode.AttachChild(m_kCurrentVOI.Volume.get(0));
            kNode.SetName(m_kCurrentVOI.Name.get(0));
            m_kDisplayList.add(m_kParent.addNode(kNode));
            m_kParent.translateSurface(m_kCurrentVOI.Name.get(0), m_kTranslate);

            m_kCurrentVOI.Update();
        }
        m_iCurrentVOIPoint++;
        m_kParent.setCursor(MipavUtil.crosshairCursor);
    }

    /**
     * Calculates twice the area (cross product of two vectors) of a triangle given three points. This is a private
     * function only called by the function "contains".
     * 
     * @param ptAx x-coordinate of the first point of the triangle
     * @param ptAy y-coordinate of the first point of the triangle
     * @param ptBx x-coordinate of the second point of the triangle
     * @param ptBy y-coordinate of the second point of the triangle
     * @param ptCx x-coordinate of the third point of the triangle
     * @param ptCy y-coordinate of the third point of the triangle
     * 
     * @return twice the area of the triangle if CCw or -2*area if CW
     */
    private float areaTwice(final float ptAx, final float ptAy, final float ptBx, final float ptBy, final float ptCx,
            final float ptCy) {
        return ( ( (ptAx - ptCx) * (ptBy - ptCy)) - ( (ptAy - ptCy) * (ptBx - ptCx)));
    }

    private void copyVOI() {
        if (m_kCurrentVOI == null) {
            return;
        }
        m_kCopyVOI = new LocalVolumeVOI(createPolyline(m_kCurrentVOI.getLocal(0).VBuffer, m_iSlice), "VOI"
                + m_iVOICount++);
    }

    /**
     * Creates the TriMesh data structures for the axis arrows.
     */
    private void CreateLabels() {
        // The screen camera is designed to map (x,y,z) in [0,1]^3 to (x',y,'z')
        // in [-1,1]^2 x [0,1].
        m_spkScreenCamera = new Camera();
        m_spkScreenCamera.Perspective = false;
        m_spkScreenCamera.SetFrustum(0.0f, 1.0f, 0.0f, 1.0f, 0.0f, 1.0f);
        m_spkScreenCamera.SetFrame(Vector3f.ZERO, Vector3f.UNIT_Z, Vector3f.UNIT_Y, Vector3f.UNIT_X);

        final ZBufferState kZState = new ZBufferState();
        kZState.Compare = ZBufferState.CompareMode.CF_ALWAYS;

        m_kXArrow = new TriMesh[2];
        m_kYArrow = new TriMesh[2];

        final Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0, 3);
        VertexBuffer pkVBuffer = new VertexBuffer(kAttr, 4);
        pkVBuffer.SetPosition3(0, 0.05f, 0.05f, 0.5f);
        pkVBuffer.SetPosition3(1, 0.15f, 0.05f, 0.5f);
        pkVBuffer.SetPosition3(2, 0.15f, 0.06f, 0.5f);
        pkVBuffer.SetPosition3(3, 0.05f, 0.06f, 0.5f);
        pkVBuffer.SetColor3(0, 0, m_aakColors[m_iPlaneOrientation][0]);
        pkVBuffer.SetColor3(0, 1, m_aakColors[m_iPlaneOrientation][0]);
        pkVBuffer.SetColor3(0, 2, m_aakColors[m_iPlaneOrientation][0]);
        pkVBuffer.SetColor3(0, 3, m_aakColors[m_iPlaneOrientation][0]);
        IndexBuffer pkIBuffer = new IndexBuffer(6);
        int[] aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;
        aiIndex[1] = 1;
        aiIndex[2] = 2;
        aiIndex[3] = 0;
        aiIndex[4] = 2;
        aiIndex[5] = 3;
        m_kXArrow[0] = new TriMesh(new VertexBuffer(pkVBuffer), new IndexBuffer(pkIBuffer));
        m_kXArrow[0].AttachEffect(new VertexColor3Effect());
        m_kXArrow[0].AttachGlobalState(kZState);
        m_kXArrow[0].UpdateGS();
        m_kXArrow[0].UpdateRS();
        m_pkRenderer.LoadResources(m_kXArrow[0]);

        pkVBuffer = new VertexBuffer(kAttr, 3);
        pkVBuffer.SetPosition3(0, 0.15f, 0.04f, 0.5f);
        pkVBuffer.SetPosition3(1, 0.18f, 0.055f, 0.5f);
        pkVBuffer.SetPosition3(2, 0.15f, 0.07f, 0.5f);
        pkVBuffer.SetColor3(0, 0, m_aakColors[m_iPlaneOrientation][0]);
        pkVBuffer.SetColor3(0, 1, m_aakColors[m_iPlaneOrientation][0]);
        pkVBuffer.SetColor3(0, 2, m_aakColors[m_iPlaneOrientation][0]);
        pkIBuffer = new IndexBuffer(3);
        aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;
        aiIndex[1] = 1;
        aiIndex[2] = 2;
        m_kXArrow[1] = new TriMesh(new VertexBuffer(pkVBuffer), new IndexBuffer(pkIBuffer));
        m_kXArrow[1].AttachEffect(new VertexColor3Effect());
        m_kXArrow[1].AttachGlobalState(kZState);
        m_kXArrow[1].UpdateGS();
        m_kXArrow[1].UpdateRS();
        m_pkRenderer.LoadResources(m_kXArrow[1]);

        // YArrow:

        pkVBuffer = new VertexBuffer(kAttr, 4);
        pkVBuffer.SetPosition3(0, 0.05f, 0.05f, 0.5f);
        pkVBuffer.SetPosition3(1, 0.06f, 0.05f, 0.5f);
        pkVBuffer.SetPosition3(2, 0.06f, 0.15f, 0.5f);
        pkVBuffer.SetPosition3(3, 0.05f, 0.15f, 0.5f);
        pkVBuffer.SetColor3(0, 0, m_aakColors[m_iPlaneOrientation][1]);
        pkVBuffer.SetColor3(0, 1, m_aakColors[m_iPlaneOrientation][1]);
        pkVBuffer.SetColor3(0, 2, m_aakColors[m_iPlaneOrientation][1]);
        pkVBuffer.SetColor3(0, 3, m_aakColors[m_iPlaneOrientation][1]);
        pkIBuffer = new IndexBuffer(6);
        aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;
        aiIndex[1] = 1;
        aiIndex[2] = 2;
        aiIndex[3] = 0;
        aiIndex[4] = 2;
        aiIndex[5] = 3;
        m_kYArrow[0] = new TriMesh(new VertexBuffer(pkVBuffer), new IndexBuffer(pkIBuffer));
        m_kYArrow[0].AttachEffect(new VertexColor3Effect());
        m_kYArrow[0].AttachGlobalState(kZState);
        m_kYArrow[0].UpdateGS();
        m_kYArrow[0].UpdateRS();
        m_pkRenderer.LoadResources(m_kYArrow[0]);

        pkVBuffer = new VertexBuffer(kAttr, 3);
        pkVBuffer.SetPosition3(0, 0.04f, 0.15f, 0.5f);
        pkVBuffer.SetPosition3(1, 0.07f, 0.15f, 0.5f);
        pkVBuffer.SetPosition3(2, 0.055f, 0.18f, 0.5f);
        pkVBuffer.SetColor3(0, 0, m_aakColors[m_iPlaneOrientation][1]);
        pkVBuffer.SetColor3(0, 1, m_aakColors[m_iPlaneOrientation][1]);
        pkVBuffer.SetColor3(0, 2, m_aakColors[m_iPlaneOrientation][1]);
        pkIBuffer = new IndexBuffer(3);
        aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;
        aiIndex[1] = 1;
        aiIndex[2] = 2;
        m_kYArrow[1] = new TriMesh(new VertexBuffer(pkVBuffer), new IndexBuffer(pkIBuffer));
        m_kYArrow[1].AttachEffect(new VertexColor3Effect());
        m_kYArrow[1].AttachGlobalState(kZState);
        m_kYArrow[1].UpdateGS();
        m_kYArrow[1].UpdateRS();
        m_pkRenderer.LoadResources(m_kYArrow[1]);

        if (m_iPlaneOrientation == FileInfoBase.AXIAL) {
            final Vector3f kPosition = new Vector3f();
            final Vector3f kDiff = new Vector3f(0f, 0.9f, 0f);
            for (int j = 0; j < 4; j++) {
                m_kXArrow[0].VBuffer.GetPosition3(j, kPosition);
                kPosition.Add(kDiff);
                m_kXArrow[0].VBuffer.SetPosition3(j, kPosition);
            }
            for (int j = 0; j < 3; j++) {
                m_kXArrow[1].VBuffer.GetPosition3(j, kPosition);
                kPosition.Add(kDiff);
                m_kXArrow[1].VBuffer.SetPosition3(j, kPosition);
            }
            for (int i = 0; i < 2; i++) {
                m_kXArrow[i].VBuffer.Release();
                m_kXArrow[i].UpdateGS();
                m_kXArrow[i].UpdateRS();
                m_pkRenderer.LoadResources(m_kXArrow[i]);
            }

            pkVBuffer = m_kYArrow[0].VBuffer;
            pkVBuffer.SetPosition3(0, 0.05f, 0.85f, 0.5f);
            pkVBuffer.SetPosition3(1, 0.06f, 0.85f, 0.5f);
            pkVBuffer.SetPosition3(2, 0.06f, 0.95f, 0.5f);
            pkVBuffer.SetPosition3(3, 0.05f, 0.95f, 0.5f);
            pkVBuffer.Release();
            m_kYArrow[0].UpdateGS();
            m_kYArrow[0].UpdateRS();
            m_pkRenderer.LoadResources(m_kYArrow[0]);

            pkVBuffer = m_kYArrow[1].VBuffer;
            pkVBuffer.SetPosition3(0, 0.04f, 0.85f, 0.5f);
            pkVBuffer.SetPosition3(1, 0.055f, 0.82f, 0.5f);
            pkVBuffer.SetPosition3(2, 0.07f, 0.85f, 0.5f);
            pkVBuffer.Release();
            m_kYArrow[1].UpdateGS();
            m_kYArrow[1].UpdateRS();
            m_pkRenderer.LoadResources(m_kYArrow[1]);
        }
    }

    private Polyline createPolyline(final VertexBuffer kVBuffer, final int iZ) {
        final int iNumPoints = kVBuffer.GetVertexQuantity();
        if (iNumPoints > 0) {
            for (int i = 0; i < iNumPoints; i++) {
                final Vector3f kPos = kVBuffer.GetPosition3(i);
                kPos.Z = iZ;
                kVBuffer.SetPosition3(i, kPos);
            }
        }
        final Polyline kPoly = new Polyline(new VertexBuffer(kVBuffer), true, true);
        kPoly.AttachEffect(new VertexColor3Effect());
        kPoly.AttachGlobalState(m_kZState);
        kPoly.UpdateGS();
        kPoly.UpdateRS();
        return kPoly;
    }

    /**
     * Initializes the display parameters.
     */
    private void CreateScene() {
        m_fX0 = -m_fXBox / m_fMaxBox;
        m_fX1 = m_fXBox / m_fMaxBox;
        m_fY0 = -m_fYBox / m_fMaxBox;
        m_fY1 = m_fYBox / m_fMaxBox;

        m_fXRange = m_fX1 - m_fX0;
        m_fYRange = m_fY1 - m_fY0;

        m_iSlice = (m_aiLocalImageExtents[2]) / 2;
        m_kPatientPt.X = (m_aiLocalImageExtents[0]) / 2;
        m_kPatientPt.Y = (m_aiLocalImageExtents[1]) / 2;
        m_kPatientPt.Z = m_iSlice;

        CreateLabels();
    }

    private void createVOI(final int iX, final int iY) {
        final float fYStart = m_iHeight - m_fMouseY;
        final float fY = m_iHeight - iY;

        if (m_bDrawRect) {
            if (m_kCurrentVOI == null) {
                final VertexBuffer kVBuffer = new VertexBuffer(m_kVOIAttr, 4);
                for (int i = 0; i < 4; i++) {
                    kVBuffer.SetColor3(0, i, m_aakColors[m_iPlaneOrientation][2]);
                }
                kVBuffer.SetPosition3(0, m_fMouseX, fYStart, m_iSlice);
                kVBuffer.SetPosition3(1, iX, fYStart, m_iSlice);
                kVBuffer.SetPosition3(2, iX, fY, m_iSlice);
                kVBuffer.SetPosition3(3, m_fMouseX, fY, m_iSlice);

                final Polyline kRectVOI = new Polyline(kVBuffer, true, true);
                kRectVOI.AttachEffect(new VertexColor3Effect());
                kRectVOI.AttachGlobalState(m_kZState);

                m_kCurrentVOI = new LocalVolumeVOI(kRectVOI, "VOITemp" + m_iPlaneOrientation);

                final Node kNode = new Node();
                kNode.AttachChild(m_kCurrentVOI.Volume.get(0));
                kNode.SetName(m_kCurrentVOI.Name.get(0));
                m_kDisplayList.add(m_kParent.addNode(kNode));
                m_kParent.translateSurface(m_kCurrentVOI.Name.get(0), m_kTranslate);
            } else {
                m_kCurrentVOI.SetPosition(1, iX, fYStart, m_iSlice);
                m_kCurrentVOI.SetPosition(2, iX, fY, m_iSlice);
                m_kCurrentVOI.SetPosition(3, m_fMouseX, fY, m_iSlice);
                final VertexBuffer kVBuffer = m_kCurrentVOI.getLocal(0).VBuffer;
                for (int i = 0; i < 4; i++) {
                    System.err.println(kVBuffer.GetPosition3(i));
                }
                m_kCurrentVOI.Release();
            }
            m_kCurrentVOI.Update();
        } else if (m_bDrawOval) {
            final float fRadiusX = Math.abs(m_fMouseX - iX);
            final float fRadiusY = Math.abs(fYStart - fY);
            if (m_kCurrentVOI == null) {
                final VertexBuffer kVBuffer = new VertexBuffer(m_kVOIAttr, m_iCirclePts);
                for (int i = 0; i < m_iCirclePts; i++) {
                    kVBuffer.SetPosition3(i, (float) (m_fMouseX + fRadiusX * m_adCos[i]), (float) (fYStart + fRadiusY
                            * m_adSin[i]), m_iSlice);
                    kVBuffer.SetColor3(0, i, m_aakColors[m_iPlaneOrientation][2]);
                }

                final Polyline kRectVOI = new Polyline(kVBuffer, true, true);
                kRectVOI.AttachEffect(new VertexColor3Effect());
                kRectVOI.AttachGlobalState(m_kZState);

                m_kCurrentVOI = new LocalVolumeVOI(kRectVOI, "VOITemp" + m_iPlaneOrientation);

                final Node kNode = new Node();
                kNode.AttachChild(m_kCurrentVOI.Volume.get(0));
                kNode.SetName(m_kCurrentVOI.Name.get(0));
                m_kDisplayList.add(m_kParent.addNode(kNode));
                m_kParent.translateSurface(m_kCurrentVOI.Name.get(0), m_kTranslate);
            } else {
                for (int i = 0; i < m_iCirclePts; i++) {
                    m_kCurrentVOI.SetPosition(i, (float) (m_fMouseX + fRadiusX * m_adCos[i]),
                            (float) (fYStart + fRadiusY * m_adSin[i]), m_iSlice);
                }
                m_kCurrentVOI.Release();
            }
            m_kCurrentVOI.Update();
        } else if (m_bDrawLevelSet) {
            final LocalVolumeVOI kTemp = singleLevelSet(iX, fY);
            if (kTemp == null) {
                return;
            }
            if (m_kCurrentVOI != null) {
                final String kName = new String(m_kCurrentVOI.Name.get(0));
                m_kDisplayList.remove(m_kParent.removeNode(kName));
            }
            m_kCurrentVOI = kTemp;
            final Node kNode = new Node();
            kNode.AttachChild(m_kCurrentVOI.Volume.get(0));
            kNode.SetName(m_kCurrentVOI.Name.get(0));
            m_kDisplayList.add(m_kParent.addNode(kNode));
            m_kParent.translateSurface(m_kCurrentVOI.Name.get(0), m_kTranslate);
            m_kCurrentVOI.Update();
        } else if (m_bDrawPolyline) {

            final VertexBuffer kVBuffer = new VertexBuffer(m_kVOIAttr, 2);
            for (int i = 0; i < 2; i++) {
                kVBuffer.SetColor3(0, i, m_aakColors[m_iPlaneOrientation][2]);
            }
            kVBuffer.SetPosition3(0, m_fMouseX, fYStart, m_iSlice);
            kVBuffer.SetPosition3(1, iX, fY, m_iSlice);

            final Polyline kLine = new Polyline(kVBuffer, false, true);
            kLine.AttachEffect(new VertexColor3Effect());
            kLine.AttachGlobalState(m_kZState);

            if (m_kCurrentVOI == null) {
                m_kCurrentVOI = new LocalVolumeVOI(kLine, "VOITemp" + m_iPlaneOrientation);
            } else {
                m_kCurrentVOI.add(kLine, "VOITemp" + m_iPlaneOrientation + "_" + m_iVOICount++);
            }

            final Node kNode = new Node();
            kNode.AttachChild(m_kCurrentVOI.Volume.lastElement());
            kNode.SetName(m_kCurrentVOI.Name.lastElement());
            m_kDisplayList.add(m_kParent.addNode(kNode));
            m_kParent.translateSurface(m_kCurrentVOI.Name.lastElement(), m_kTranslate);

            m_kCurrentVOI.Update();

            m_fMouseX = iX;
            m_fMouseY = iY;
        }

        m_bModified = true;
        GetCanvas().display();
        m_bUpdateVOI = true;
    }

    private void deleteAllVOI() {
        if (m_kVOIList == null) {
            return;
        }
        m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        m_kCurrentVOI = null;
        for (final LocalVolumeVOIVector element : m_kVOIList) {
            if (element != null) {
                for (int j = 0; j < element.size(); j++) {
                    m_kDisplayList.remove(m_kParent.removeNode(element.get(j).Name.get(0)));
                }
                element.clear();
            }
        }
        m_bUpdateVOI = false;
        m_bModified = true;
        GetCanvas().display();
    }

    private void deleteVOI() {
        if ( (m_kCurrentVOI == null) || (m_kVOIList == null) || (m_kVOIList[m_iSlice] == null)) {
            return;
        }
        m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        m_kDisplayList.remove(m_kParent.removeNode(m_kCurrentVOI.Name.get(0)));
        m_kVOIList[m_iSlice].remove(m_kCurrentVOI);
        m_kCurrentVOI = null;
        m_bUpdateVOI = false;
        m_bModified = true;
        GetCanvas().display();
    }

    /**
     * Called from the display function. Draws the axis arrows.
     */
    private void drawAxes() {
        if (m_bDrawAxes) {

            final ColorRGBA kXSliceHairColor = new ColorRGBA(m_aakColors[m_iPlaneOrientation][0].R,
                    m_aakColors[m_iPlaneOrientation][0].G, m_aakColors[m_iPlaneOrientation][0].B, 1.0f);

            final ColorRGBA kYSliceHairColor = new ColorRGBA(m_aakColors[m_iPlaneOrientation][1].R,
                    m_aakColors[m_iPlaneOrientation][1].G, m_aakColors[m_iPlaneOrientation][1].B, 1.0f);

            if ( !m_kVolumeImageA.GetImage().getRadiologicalView() && (m_iPlaneOrientation != FileInfoBase.SAGITTAL)) {
                if ( !m_bPatientOrientation) {
                    m_kLabelXDisplay = new String("-X");
                } else {
                    m_kLabelXDisplay = new String("R");
                }
            } else if (m_iPlaneOrientation != FileInfoBase.SAGITTAL) {
                if ( !m_bPatientOrientation) {
                    m_kLabelXDisplay = new String("X");
                } else {
                    m_kLabelXDisplay = new String("L");
                }
            }
            if (m_iPlaneOrientation == FileInfoBase.AXIAL) {
                m_pkRenderer.Draw(m_iLabelX_SpacingX, m_iLabelX_SpacingY, kXSliceHairColor, m_kLabelXDisplay
                        .toCharArray());
                m_pkRenderer.Draw(m_iLabelY_SpacingX, m_iLabelY_SpacingY, kYSliceHairColor, m_kLabelY.toCharArray());
            } else {
                m_pkRenderer.Draw(m_iLabelX_SpacingX, m_iHeight - m_iLabelX_SpacingY, kXSliceHairColor,
                        m_kLabelXDisplay.toCharArray());
                m_pkRenderer.Draw(m_iLabelY_SpacingX, m_iHeight - m_iLabelY_SpacingY, kYSliceHairColor, m_kLabelY
                        .toCharArray());
            }
            m_pkRenderer.SetCamera(m_spkScreenCamera);
            m_pkRenderer.Draw(m_kXArrow[0]);
            m_pkRenderer.Draw(m_kXArrow[1]);
            m_pkRenderer.Draw(m_kYArrow[0]);
            m_pkRenderer.Draw(m_kYArrow[1]);
            m_pkRenderer.SetCamera(m_spkCamera);
        }
    }

    private void drawVOI() {
        if (m_kCurrentVOI != null) {
            final int iNumLines = m_kCurrentVOI.size();
            for (int i = 0; i < iNumLines; i++) {
                final int iNumPoints = m_kCurrentVOI.GetVertexQuantity(i);
                if (iNumPoints > 0) {
                    if (m_iSlice == m_kCurrentVOI.slice(i)) {
                        final Vector3f kTranslate = new Vector3f();
                        for (int j = 0; j < iNumPoints; j++) {
                            kTranslate.Copy(m_kCurrentVOI.getVolume(i).VBuffer.GetPosition3(j));
                            kTranslate.Add(m_kTranslate);
                            m_kBallPoint.Local.SetTranslate(kTranslate);
                            m_kBallPoint.UpdateGS();
                            m_pkRenderer.Draw(m_kBallPoint);
                        }
                    }
                }
            }
        }
    }

    private void fillVolume(final int iSlice, final Polyline kPoly, final ModelImage kVolume,
            final boolean bIntersection, final int iValue) {
        int iNumPoints = kPoly.VBuffer.GetVertexQuantity();
        // System.err.println( "fillVolume " + iSlice + " " + iNumPoints );
        if (iNumPoints == 0) {
            return;
        }
        final Vector3f[] kVolumePts = new Vector3f[iNumPoints + 1];
        int iXMin = Integer.MAX_VALUE;
        int iYMin = Integer.MAX_VALUE;
        int iXMax = Integer.MIN_VALUE;
        int iYMax = Integer.MIN_VALUE;
        for (int i = 0; i < iNumPoints; i++) {
            final Vector3f kPt = kPoly.VBuffer.GetPosition3(i);
            kPt.Z = iSlice;
            kVolumePts[i] = kPt;// VOIToFileCoordinates( kPt );
            iXMin = (int) Math.min(iXMin, kVolumePts[i].X);
            iYMin = (int) Math.min(iYMin, kVolumePts[i].Y);
            iXMax = (int) Math.max(iXMax, kVolumePts[i].X);
            iYMax = (int) Math.max(iYMax, kVolumePts[i].Y);
        }
        final Vector3f kPt = kPoly.VBuffer.GetPosition3(0);
        kPt.Z = iSlice;
        kVolumePts[iNumPoints] = kPt;// VOIToFileCoordinates( kPt );
        iXMin = (int) Math.min(iXMin, kVolumePts[iNumPoints].X);
        iYMin = (int) Math.min(iYMin, kVolumePts[iNumPoints].Y);
        iXMax = (int) Math.max(iXMax, kVolumePts[iNumPoints].X);
        iYMax = (int) Math.max(iYMax, kVolumePts[iNumPoints].Y);
        iNumPoints++;

        final int[][] aaiCrossingPoints = new int[iXMax - iXMin + 1][];
        final int[] aiNumCrossings = new int[iXMax - iXMin + 1];

        for (int i = 0; i < (iXMax - iXMin + 1); i++) {
            aaiCrossingPoints[i] = new int[iNumPoints];
        }

        outlineRegion(aaiCrossingPoints, aiNumCrossings, iXMin, iYMin, iXMax, iYMax, kVolumePts, kVolume);
        fill(aaiCrossingPoints, aiNumCrossings, iXMin, iYMin, iXMax, iYMax, (int) kVolumePts[0].Z, kVolume,
                bIntersection, iValue);

    }

    /*
     * Convert the position in LocalCoordinates (rendering space) into PatientCoordinates: @param localPt the current
     * point in LocalCoordinates @param patientPt transformed localPt in PatientCoordinates
     */
    private void LocalToPatient(final Vector3f localPt, final Vector3f patientPt) {
        patientPt.X = localPt.X * (m_aiLocalImageExtents[0] - 1);
        patientPt.Y = localPt.Y * (m_aiLocalImageExtents[1] - 1);
        patientPt.Z = localPt.Z * (m_aiLocalImageExtents[2] - 1);
    }

    private void PatientToLocal(final Vector3f patientPt, final Vector3f localPt) {
        localPt.X = patientPt.X / (m_aiLocalImageExtents[0] - 1);
        localPt.Y = patientPt.Y / (m_aiLocalImageExtents[1] - 1);
        localPt.Z = patientPt.Z / (m_aiLocalImageExtents[2] - 1);
    }

    private void moveVOI(final int iX, final int iY) {
        final float fY = m_iHeight - iY;
        if (m_kCurrentVOI == null) {
            return;
        }
        final Vector3f kDiff = new Vector3f(iX, fY, m_iSlice);
        m_kCurrentVOI.move(kDiff);
        m_kCurrentVOI.Update();

        m_kCurrentVOI.setCenter(iX, fY, m_iSlice);
        m_kParent.setCursor(MipavUtil.moveCursor);
        m_bModified = true;
        GetCanvas().display();
    }

    private void moveVOIPoint(final int iX, final int iY) {
        if (m_kCurrentVOI == null) {
            return;
        }
        final float fY = m_iHeight - iY;
        m_kCurrentVOI.SetPosition(m_iCurrentVOIPoint, iX, fY, m_iSlice);
        m_kCurrentVOI.Release();
        m_kCurrentVOI.Update();
        m_bUpdateVOI = true;
        m_bModified = true;
        GetCanvas().display();

        m_kParent.setCursor(MipavUtil.crosshairCursor);
    }

    private boolean nearPoint(final int iX, final int iY, final VertexBuffer kVBuffer) {
        final Vector3f kVOIPoint = new Vector3f(iX, m_iHeight - iY, m_iSlice);
        final int iNumPoints = kVBuffer.GetVertexQuantity();
        if (iNumPoints > 0) {
            for (int i = 0; i < iNumPoints; i++) {
                final Vector3f kPos = kVBuffer.GetPosition3(i);
                final Vector3f kDiff = new Vector3f();
                kDiff.Sub(kPos, kVOIPoint);
                if ( (Math.abs(kDiff.X) < 3) && (Math.abs(kDiff.Y) < 3) && (Math.abs(kDiff.Z) < 3)) {
                    m_iCurrentVOIPoint = i;
                    return true;
                }
            }
        }
        return false;
    }

    private void pasteVOI(final int iSlice) {
        if (m_kCopyVOI == null) {
            return;
        }
        m_kCurrentVOI = new LocalVolumeVOI(createPolyline(m_kCopyVOI.getLocal(0).VBuffer, iSlice), "VOI"
                + m_iVOICount++);
        m_bUpdateVOI = true;
        saveVOI(0, 0, iSlice);
    }

    /**
     * Dragging the mouse with the left-mouse button held down changes the positions of the X and Y cross bars, and
     * therefore the ZSlice positions of the associated PlaneRenderWM objects and the TriPlanar Surface. The new
     * positions are calculated and passed onto the parent frame.
     * 
     * @param kEvent the mouse event generated by a mouse drag
     */
    private void processLeftMouseDrag(final MouseEvent kEvent) {

        /*
         * Calculate the center of the mouse in local coordinates, taking into account zoom and translate:
         */
        final Vector3f localPt = new Vector3f();
        this.ScreenToLocal(kEvent.getX(), kEvent.getY(), m_iSlice, localPt);

        /*
         * Tell the ViewJFrameVolumeView parent to update the other PlaneRenderWMs and the SurfaceRender with the
         * changed Z position of the planes with color matching the moved bar:
         */
        final Vector3f patientPt = new Vector3f();
        this.LocalToPatient(localPt, patientPt);
        final Vector3f volumePt = new Vector3f();
        MipavCoordinateSystems.patientToFile(patientPt, volumePt, m_kVolumeImageA.GetImage(), m_iPlaneOrientation);
        if (m_bDrawVOI) {
            createVOI(kEvent.getX(), kEvent.getY());
        } else if (m_bPointer) {
            if (m_kParent.getCursor() == MipavUtil.crosshairCursor) {
                moveVOIPoint(kEvent.getX(), kEvent.getY());
            } else if (m_bSelected) {
                moveVOI(kEvent.getX(), kEvent.getY());
            }
        } else {
            m_kParent.setSliceFromPlane(volumePt);
        }
    }

    /**
     * If the right mouse button is pressed and dragged. processRightMouseDrag updates the HistoLUT window and level
     * (contrast and brightness)
     * 
     * @param kEvent the mouse event generated by a mouse drag
     */
    private void processRightMouseDrag(final MouseEvent kEvent) {
        // make the LUT panel the active panel.
        m_kParent.actionPerformed(new ActionEvent(this, 0, "HistoLUT"));
        /* Get the coordinates of the mouse position in local coordinates: */
        final Vector3f localPt = new Vector3f();
        this.ScreenToLocal(kEvent.getX(), kEvent.getY(), m_iSlice, localPt);
        m_kActiveLookupTable = null;

        /* Get which image is active, either m_kImageA or m_kImageB: */
        m_kActiveImage = m_kParent.getHistoLUTActiveImage();

        if (m_kActiveImage == null) {
            m_kActiveImage = m_kParent.getHistoRGBActiveImage();
        }
        if (m_kActiveImage == null) {
            m_kActiveImage = m_kVolumeImageA.GetImage();
        }

        m_kActiveLookupTable = m_kParent.getActiveLookupTable(m_kActiveImage);

        if (m_kWinLevel.updateWinLevel(localPt.X, localPt.Y, m_bFirstDrag, m_kActiveLookupTable, m_kActiveImage)) {
            if (m_kActiveImage == m_kVolumeImageA.GetImage()) {
                if (m_kVolumeImageA.GetImage().isColorImage()) {
                    m_kParent.getRGBDialog().setRGBTA((ModelRGB) m_kActiveLookupTable);
                    m_kParent.getRGBDialog().update();
                } else {
                    m_kParent.getLUTDialog().setLUTA((ModelLUT) m_kActiveLookupTable);
                }
            } else if ( (m_kVolumeImageB.GetImage() != null) && (m_kActiveImage == m_kVolumeImageB.GetImage())) {
                if (m_kVolumeImageB.GetImage().isColorImage()) {
                    m_kParent.getRGBDialog().setRGBTB((ModelRGB) m_kActiveLookupTable);
                    m_kParent.getRGBDialog().update();
                } else {
                    m_kParent.getLUTDialog().setLUTB((ModelLUT) m_kActiveLookupTable);
                }
            }
        }
        if (m_bFirstDrag) {
            try {
                final Image kImg = MipavUtil.getIconImage("qkwinlevel.gif");
                final Cursor kWinLevelCursor = Toolkit.getDefaultToolkit().createCustomCursor(kImg, new Point(12, 12),
                        "WinLevel");
                /* Set the cursor icon: */
                m_kParent.setCursor(kWinLevelCursor);
            } catch (final FileNotFoundException error) {}
            m_bFirstDrag = false;
        }
    }

    private void saveVOI(final int iX, final int iY, final int iSlice) {
        if (m_kCurrentVOI != null) {
            if (m_kCurrentVOI.size() == 0) {
                return;
            }
            if (m_kVOIList == null) {
                m_kVOIList = new LocalVolumeVOIVector[m_aiLocalImageExtents[2]];
            }
            if (m_kVOIList[iSlice] == null) {
                m_kVOIList[iSlice] = new LocalVolumeVOIVector();
            }
            if ( !m_kVOIList[iSlice].contains(m_kCurrentVOI)) {
                final String kName = new String(m_kCurrentVOI.Name.get(0));
                final int iNumLines = m_kCurrentVOI.size();
                if (iNumLines > 1) {
                    final VertexBuffer kVBuffer = new VertexBuffer(m_kCurrentVOI.getLocal(0).VBuffer.GetAttributes(),
                            iNumLines + 1);
                    for (int i = 0; i < iNumLines; i++) {
                        kVBuffer.SetPosition3(i, m_kCurrentVOI.getLocal(i).VBuffer.GetPosition3(0));
                        kVBuffer.SetColor3(0, i, m_aakColors[m_iPlaneOrientation][2]);
                        m_kDisplayList.remove(m_kParent.removeNode(m_kCurrentVOI.Name.get(i)));
                    }
                    kVBuffer.SetPosition3(iNumLines, m_kCurrentVOI.getLocal(iNumLines - 1).VBuffer.GetPosition3(1));
                    kVBuffer.SetColor3(0, iNumLines, m_aakColors[m_iPlaneOrientation][2]);
                    final Polyline kPoly = createPolyline(kVBuffer, m_iSlice);
                    m_kCurrentVOI = new LocalVolumeVOI(kPoly, kName);
                } else {
                    m_kDisplayList.remove(m_kParent.removeNode(kName));
                }
                final Node kNode = new Node();
                kNode.AttachChild(m_kCurrentVOI.Volume.get(0));
                kNode.SetName(kName);
                m_kDisplayList.add(m_kParent.addNode(kNode));
                m_kParent.translateSurface(kName, m_kTranslate);
                m_kVOIList[iSlice].add(m_kCurrentVOI);
            }
        }
        m_bUpdateVOI = false;
    }

    /**
     * Calculate the position of the mouse in the Local Coordinates, taking into account zoom and translate:
     * 
     * @param iX mouse x coordinate value
     * @param iY mouse y coordinate value
     * @param kLocal mouse position in Local Coordinates
     */
    private void ScreenToLocal(int iX, int iY, final int iZ, final Vector3f kLocal) {
        // System.err.println( "ScreenToLocal " + iX + " " + iY );
        iX = Math.min(m_iWidth, Math.max(0, iX));
        iY = Math.min(m_iHeight, Math.max(0, iY));
        // System.err.println( " " + iX + " " + iY );
        final float fHalfWidth = ((float) m_iWidth - 1) / 2.0f;
        final float fHalfHeight = ((float) m_iHeight - 1) / 2.0f;

        kLocal.X = (iX - fHalfWidth) / fHalfWidth;
        kLocal.Y = (iY - fHalfHeight) / fHalfWidth;
        // System.err.println( " " + kLocal.X + " " + kLocal.Y );

        kLocal.X *= m_fZoomScale;
        kLocal.Y *= m_fZoomScale;
        // System.err.println( " " + kLocal.X + " " + kLocal.Y );

        /* Bounds checking: */
        kLocal.X = Math.min(Math.max(kLocal.X, m_fX0), m_fX1);
        kLocal.Y = Math.min(Math.max(kLocal.Y, m_fY0), m_fY1);
        // System.err.println( " " + kLocal.X + " " + kLocal.Y + " " + m_fX0 + " " + m_fX1 + " " + m_fY0 + " " + m_fY1
        // );

        /* Normalize: */
        kLocal.X = (kLocal.X - m_fX0) / m_fXRange;
        kLocal.Y = (kLocal.Y - m_fY0) / m_fYRange;
        // System.err.println( " " + kLocal.X + " " + kLocal.Y );
        kLocal.Z = iZ / (float) (m_aiLocalImageExtents[2] - 1);
    }

    private void LocalToScreen(final Vector3f kLocal, final Vector3f kScreen) {
        kScreen.X = kLocal.X * m_fXRange + m_fX0;
        kScreen.Y = kLocal.Y * m_fYRange + m_fY0;
        kScreen.Z = kLocal.Z * (m_aiLocalImageExtents[2] - 1);

        kScreen.X /= m_fZoomScale;
        kScreen.Y /= m_fZoomScale;

        final float fHalfWidth = ((float) m_iWidth - 1) / 2.0f;
        final float fHalfHeight = ((float) m_iHeight - 1) / 2.0f;
        final float fX = kScreen.X * fHalfWidth + fHalfWidth;
        final float fY = kScreen.Y * fHalfWidth + fHalfHeight;
        kScreen.Set(fX, fY, m_iSlice);
    }

    private void selectVOI(final int iX, final int iY) {
        final float fY = m_iHeight - iY;
        m_bSelected = false;
        m_kCurrentVOI = null;
        if ( (m_kVOIList != null) && (m_kVOIList[m_iSlice] != null)) {
            for (int i = 0; i < m_kVOIList[m_iSlice].size(); i++) {
                if (contains(iX, (int) fY, m_kVOIList[m_iSlice].get(i).getLocal(0).VBuffer)) {
                    m_kCurrentVOI = m_kVOIList[m_iSlice].get(i);
                    m_kCurrentVOI.setCenter(iX, fY, m_iSlice);
                    // System.err.println( "Selected: " + kCurve.getName() );
                    m_bSelected = true;
                    break;
                }
            }
        }
    }

    /**
     * Sets the local slice value.
     * 
     * @param fSlice
     */
    private void setSlice(final float fSlice) {
        int iSlice = (int) fSlice;

        /* Check bounds: */
        if (iSlice > (m_aiLocalImageExtents[2] - 1)) {
            iSlice = m_aiLocalImageExtents[2] - 1;
        }

        if (iSlice < 0) {
            iSlice = 0;
        }

        if (iSlice != m_iSlice) {
            m_iSlice = iSlice;
        }
    }

    private void showSelectedVOI(final int iX, final int iY) {
        if (m_kCurrentVOI != null) {
            if (nearPoint(iX, iY, m_kCurrentVOI.getLocal(0).VBuffer)) {
                m_kParent.setCursor(MipavUtil.crosshairCursor);
                return;
            } else if (nearLine(iX, iY, m_kCurrentVOI.getLocal(0).VBuffer)) {
                m_kParent.setCursor(MipavUtil.addPointCursor);
                return;
            }
        }

        if ( (m_kVOIList != null) && (m_kVOIList[m_iSlice] != null)) {
            for (int i = 0; i < m_kVOIList[m_iSlice].size(); i++) {
                if (contains(iX, m_iHeight - iY, m_kVOIList[m_iSlice].get(i).getLocal(0).VBuffer)) {
                    m_kParent.setCursor(MipavUtil.moveCursor);
                    return;
                }
            }
        }
        m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

    private Vector3f VOIToFileCoordinates(final Vector3f kVOIPt, final boolean bScale) {
        final Vector3f localPt = new Vector3f();
        this.ScreenToLocal((int) kVOIPt.X, (int) (m_iHeight - kVOIPt.Y), (int) kVOIPt.Z, localPt);

        /*
         * Tell the ViewJFrameVolumeView parent to update the other PlaneRenderWMs and the SurfaceRender with the
         * changed Z position of the planes with color matching the moved bar:
         */
        final Vector3f patientPt = new Vector3f();
        this.LocalToPatient(localPt, patientPt);
        final Vector3f volumePt = new Vector3f();
        MipavCoordinateSystems.patientToFile(patientPt, volumePt, m_kVolumeImageA.GetImage(), m_iPlaneOrientation);
        if (bScale) {
            volumePt.Mult(m_kVolumeScale);
            // System.err.println( volumePt.ToString() );
        }
        return volumePt;
    }

    private Vector3f FileCoordinatesToVOI(final Vector3f volumePt, final boolean bScale) {
        final Vector3f kVOIPt = new Vector3f();
        if (bScale) {
            kVOIPt.Mult(volumePt, m_kVolumeScaleInv);
            // System.err.println( volumePt.ToString() );
        }
        final Vector3f patientPt = new Vector3f();
        MipavCoordinateSystems.fileToPatient(kVOIPt, patientPt, m_kVolumeImageA.GetImage(), m_iPlaneOrientation);

        final Vector3f localPt = new Vector3f();
        localPt.Mult(patientPt, m_kVolumeScale);
        return localPt;
    }

    private void zoom() {
        m_fZoomScale = Math.max(0, m_fZoomScale);
        float fRMax = (m_fZoomScale * Math.max(m_fX, m_fY)) / 2.0f;
        float fUMax = fRMax * m_iHeight / m_iWidth;
        m_spkCamera.SetFrustum( -fRMax, fRMax, -fUMax, fUMax, 1f, 5.0f);
        m_pkRenderer.OnFrustumChange();
        m_bModified = true;
        GetCanvas().display();
    }

    /**
     * This method calculates the average pixel value based on the four neighbors (N, S, E, W).
     * 
     * @param index the center pixel where the average pixel value is to be calculated.
     * 
     * @return the average pixel value as a float.
     */
    private float avgPix(final int iX, final int iY) {
        int index = m_aiIndexValues[iY][iX];
        // System.err.print( " " + index );
        final int[] extents = m_kVolumeImageA.GetImage().getExtents();
        if ( (index > extents[0]) && (index < (m_kVolumeImageA.GetImage().getSize() - extents[0]))) {

            int sum = (m_aucData[index] & 0x00ff);

            index = m_aiIndexValues[iY - 1][iX];
            sum += (m_aucData[index] & 0x00ff);

            index = m_aiIndexValues[iY][iX - 1];
            sum += (m_aucData[index] & 0x00ff);

            index = m_aiIndexValues[iY][iX + 1];
            sum += (m_aucData[index] & 0x00ff);

            index = m_aiIndexValues[iY + 1][iX];
            sum += (m_aucData[index] & 0x00ff);

            // System.err.println( " " + 5 + " " + sum );
            return sum / 5.0f;
        }
        // System.err.println( " " + (m_aucData[index] & 0x00ff));
        return (m_aucData[index] & 0x00ff);
    }

    /**
     * Generates the possible paths of the level set and pushes them onto a stack. Looks in the 8 neighborhood
     * directions for the possible paths.
     * 
     */
    private void paths(final int iX, final int iY, final int iZ, final int i, final float level) {

        int[] intPtr = null;

        try {
            intPtr = new int[1];
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.mouseDragged");

            return;
        }

        intPtr[0] = levelSetStack.size() - 1;

        // indexMM
        if ( (i != 0) && (m_afAverages[m_iM][m_iM] <= level) && (map.get(m_aiIndexValues[m_iM][m_iM]) == false)) {
            stack.push(intPtr);
        }
        // indexM_
        else if ( (i != 1) && (m_afAverages[m_iM][m_i_] <= level) && (map.get(m_aiIndexValues[m_iM][m_i_]) == false)) {
            stack.push(intPtr);
        }
        // indexMP
        else if ( (i != 2) && (m_afAverages[m_iM][m_iP] <= level) && (map.get(m_aiIndexValues[m_iM][m_iP]) == false)) {
            stack.push(intPtr);
        }
        // index_P
        else if ( (i != 3) && (m_afAverages[m_i_][m_iP] <= level) && (map.get(m_aiIndexValues[m_i_][m_iP]) == false)) {
            stack.push(intPtr);
        }
        // indexPP
        else if ( (i != 4) && (m_afAverages[m_iP][m_iP] <= level) && (map.get(m_aiIndexValues[m_iP][m_iP]) == false)) {
            stack.push(intPtr);
        }
        // indexP_
        else if ( (i != 5) && (m_afAverages[m_iP][m_i_] <= level) && (map.get(m_aiIndexValues[m_iP][m_i_]) == false)) {
            stack.push(intPtr);
        }
        // indexPM
        else if ( (i != 6) && (m_afAverages[m_iP][m_iM] <= level) && (map.get(m_aiIndexValues[m_iP][m_iM]) == false)) {
            stack.push(intPtr);
        }
        // index_M
        else if ( (i != 7) && (m_afAverages[m_i_][m_iM] <= level) && (map.get(m_aiIndexValues[m_i_][m_iM]) == false)) {
            stack.push(intPtr);
        }
    }

    /**
     * Creates a single level set. Takes a starting point and finds a closed path along the levelset back to the
     * starting point.
     * 
     * @param startPtX the start point
     * @param startPtY the start point
     * @param level the level of the level set
     */
    private LocalVolumeVOI singleLevelSet(float startPtX, float startPtY) {
        double distance;
        stack.removeAllElements();

        for (int i = 0; i < map.size(); i++) {
            map.clear(i);
        }

        final Vector3f kVOIPt = new Vector3f(startPtX, startPtY, m_iSlice);
        final Vector3f kVolumePt = VOIToFileCoordinates(kVOIPt, false);
        final Vector3f kPatientPt = new Vector3f();
        MipavCoordinateSystems.fileToPatient(kVolumePt, kPatientPt, m_kVolumeImageA.GetImage(), m_iPlaneOrientation);

        startPtX = kPatientPt.X;
        startPtY = kPatientPt.Y;
        int x = (int) (kPatientPt.X + 0.5);
        int y = (int) (kPatientPt.Y + 0.5);
        final int z = (int) kPatientPt.Z;

        setIndices(x, y, z);

        final int index = m_aiIndexValues[m_i_][m_i_];
        final int level = (m_aucData[index] & 0x00ff);

        levelSetStack.reset();
        levelSetStack.addPoint(x, y);
        map.set(m_aiIndexValues[m_i_][m_i_]);

        int dir = -1;
        float diff = 100000;
        int mapIndex = 0;

        do {

            if ( (x >= 2) && (x < (m_aiLocalImageExtents[0] - 2)) && (y >= 2) && (y < (m_aiLocalImageExtents[1] - 2))) {

                mapIndex = m_aiIndexValues[m_iM][m_i_];
                // System.err.println( map.get(mapIndex) );
                if ( (m_afAverages[m_iM][m_i_] >= level)
                        && ( (m_afAverages[m_iM][m_iP] < level) || (m_afAverages[m_i_][m_i_] < level)
                                || (m_afAverages[m_iM][m_iM] < level) || (m_afAverages[m_iMM][m_i_] < level))
                        && (map.get(mapIndex) == false)) {
                    dir = 1;
                    diff = Math.abs(m_afAverages[m_iM][m_i_] - m_afAverages[m_i_][m_i_]);
                }
                mapIndex = m_aiIndexValues[m_iM][m_iP];
                // System.err.println( map.get(mapIndex) );
                if ( (m_afAverages[m_iM][m_iP] >= level)
                        && ( (m_afAverages[m_iM][m_iPP] < level) || (m_afAverages[m_i_][m_iP] < level)
                                || (m_afAverages[m_iM][m_i_] < level) || (m_afAverages[m_iMM][m_iP] < level))
                        && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iM][m_iP] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 2;
                        diff = Math.abs(m_afAverages[m_iM][m_iP] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_i_][m_iP];
                // System.err.println( map.get(mapIndex) );
                if ( (m_afAverages[m_i_][m_iP] >= level)
                        && ( (m_afAverages[m_i_][m_iPP] < level) || (m_afAverages[m_iP][m_iP] < level)
                                || (m_afAverages[m_i_][m_i_] < level) || (m_afAverages[m_iM][m_iP] < level))
                        && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_i_][m_iP] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 3;
                        diff = Math.abs(m_afAverages[m_i_][m_iP] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_iP][m_iP];
                // System.err.println( map.get(mapIndex) );
                if ( (m_afAverages[m_iP][m_iP] >= level)
                        && ( (m_afAverages[m_iP][m_iPP] < level) || (m_afAverages[m_iPP][m_iP] < level)
                                || (m_afAverages[m_i_][m_iP] < level) || (m_afAverages[m_iP][m_i_] < level))
                        && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iP][m_iP] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 4;
                        diff = Math.abs(m_afAverages[m_iP][m_iP] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_iP][m_i_];
                // System.err.println( map.get(mapIndex) );
                if ( (m_afAverages[m_iP][m_i_] >= level)
                        && ( (m_afAverages[m_iP][m_iP] < level) || (m_afAverages[m_iPP][m_i_] < level)
                                || (m_afAverages[m_iP][m_iM] < level) || (m_afAverages[m_i_][m_i_] < level))
                        && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iP][m_i_] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 5;
                        diff = Math.abs(m_afAverages[m_iP][m_i_] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_iP][m_iM];
                // System.err.println( map.get(mapIndex) );
                if ( (m_afAverages[m_iP][m_iM] >= level)
                        && ( (m_afAverages[m_iP][m_i_] < level) || (m_afAverages[m_iPP][m_iM] < level)
                                || (m_afAverages[m_iP][m_iMM] < level) || (m_afAverages[m_i_][m_iM] < level))
                        && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iP][m_iM] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 6;
                        diff = Math.abs(m_afAverages[m_iP][m_iM] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_i_][m_iM];
                // System.err.println( map.get(mapIndex) );
                if ( (m_afAverages[m_i_][m_iM] >= level)
                        && ( (m_afAverages[m_i_][m_i_] < level) || (m_afAverages[m_iP][m_iM] < level)
                                || (m_afAverages[m_i_][m_iMM] < level) || (m_afAverages[m_iM][m_iM] < level))
                        && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_i_][m_iM] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 7;
                        diff = Math.abs(m_afAverages[m_i_][m_iM] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_iM][m_iM];
                // System.err.println( map.get(mapIndex) );
                if ( (m_afAverages[m_iM][m_iM] >= level)
                        && ( (m_afAverages[m_iM][m_i_] < level) || (m_afAverages[m_i_][m_iM] < level)
                                || (m_afAverages[m_iM][m_iMM] < level) || (m_afAverages[m_iMM][m_iM] < level))
                        && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iM][m_iM] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 0;
                        // diff = Math.abs(imageBufferActive[index-xDim-1] - imageBufferActive[index]);
                    }
                }

                diff = 1000000;

                if (dir == 1) {
                    mapIndex = m_aiIndexValues[m_iM][m_i_];
                    map.set(mapIndex);
                    paths(x, y, z, dir, level);
                    // x = x;
                    y = y - 1;
                    setIndices(x, y, z);
                } else if (dir == 2) {
                    mapIndex = m_aiIndexValues[m_iM][m_iP];
                    map.set(mapIndex);
                    paths(x, y, z, dir, level);
                    x = x + 1;
                    y = y - 1;
                    setIndices(x, y, z);
                } else if (dir == 3) {
                    mapIndex = m_aiIndexValues[m_i_][m_iP];
                    map.set(mapIndex);
                    paths(x, y, z, dir, level);
                    x = x + 1;
                    // y = y;
                    setIndices(x, y, z);
                } else if (dir == 4) {
                    mapIndex = m_aiIndexValues[m_iP][m_iP];
                    map.set(mapIndex);
                    paths(x, y, z, dir, level);
                    x = x + 1;
                    y = y + 1;
                    setIndices(x, y, z);
                } else if (dir == 5) {
                    mapIndex = m_aiIndexValues[m_iP][m_i_];
                    map.set(mapIndex);
                    paths(x, y, z, dir, level);
                    // x = x;
                    y = y + 1;
                    setIndices(x, y, z);
                } else if (dir == 6) {
                    mapIndex = m_aiIndexValues[m_iP][m_iM];
                    map.set(mapIndex);
                    paths(x, y, z, dir, level);
                    x = x - 1;
                    y = y + 1;
                    setIndices(x, y, z);
                } else if (dir == 7) {
                    mapIndex = m_aiIndexValues[m_i_][m_iM];
                    map.set(mapIndex);
                    paths(x, y, z, dir, level);
                    x = x - 1;
                    // y = y;
                    setIndices(x, y, z);
                } else if (dir == 0) {
                    mapIndex = m_aiIndexValues[m_iM][m_iM];
                    map.set(mapIndex);
                    paths(x, y, z, dir, level);
                    x = x - 1;
                    y = y - 1;
                    setIndices(x, y, z);
                } else {

                    if ( !stack.empty()) {
                        final int ptr = (stack.pop())[0];
                        x = levelSetStack.getPointX(ptr);
                        y = levelSetStack.getPointY(ptr);
                        levelSetStack.setIndex(ptr);
                        setIndices(x, y, z);
                    } else {
                        x = y = -1;
                    }
                }

                dir = -1;
            } else { // near edge of image
                levelSetStack.reset();

                break;
            }

            if ( (x == -1) || (y == -1)) {
                levelSetStack.reset();

                break;
            }

            levelSetStack.addPoint(x, y);
            distance = ( (x - startPtX) * (x - startPtX)) + ( (y - startPtY) * (y - startPtY));
            if ( (distance < 2.1) && (levelSetStack.size() < 10)) {
                distance = 10;
            }
        } while (distance > 2.1);

        if (levelSetStack.size() != 0) {
            final Vector3f kLocalPt = new Vector3f();
            final Vector3f kScreenPt = new Vector3f();

            final VertexBuffer kVBuffer = new VertexBuffer(m_kVOIAttr, levelSetStack.size() + 1);
            for (int i = 0; i < levelSetStack.size(); i++) {
                // System.err.print( levelSetStack.getPointX(i) + " " + levelSetStack.getPointY(i) );

                kPatientPt.Set(levelSetStack.getPointX(i), levelSetStack.getPointY(i), m_iSlice);
                PatientToLocal(kPatientPt, kLocalPt);
                LocalToScreen(kLocalPt, kScreenPt);
                kScreenPt.Y = m_iHeight - kScreenPt.Y;
                // kPatientPt.Mult( m_kVolumeScale );
                // System.err.println( " " + kScreenPt.X + " " + kScreenPt.Y );

                kVBuffer.SetPosition3(i, kScreenPt);
                kVBuffer.SetColor3(0, i, m_aakColors[m_iPlaneOrientation][2]);
            }
            kPatientPt.Set(levelSetStack.getPointX(0), levelSetStack.getPointY(0), m_iSlice);
            PatientToLocal(kPatientPt, kLocalPt);
            LocalToScreen(kLocalPt, kScreenPt);
            kScreenPt.Y = m_iHeight - kScreenPt.Y;
            // kPatientPt.Mult( m_kVolumeScale );
            kVBuffer.SetPosition3(levelSetStack.size(), kScreenPt);
            kVBuffer.SetColor3(0, levelSetStack.size(), m_aakColors[m_iPlaneOrientation][2]);
            final Polyline kPoly = createPolyline(kVBuffer, m_iSlice);
            return new LocalVolumeVOI(kPoly, "VOITemp" + m_iPlaneOrientation);
        }
        return null;
    }

    private void initDataBuffer() {
        int iSize = m_aiLocalImageExtents[0] * m_aiLocalImageExtents[1] * m_aiLocalImageExtents[2];
        m_aucData = new byte[iSize];
        if (m_kVolumeImageA.GetImage().isColorImage()) {
            iSize *= 4;
            final byte[] aucTemp = new byte[iSize];
            try {
                m_kVolumeImageA.GetImage().exportData(0, iSize, aucTemp);
            } catch (final IOException e) {
                e.printStackTrace();
            }
            for (int i = 0; i < m_aucData.length; i++) {
                m_aucData[i] = (byte) ( (aucTemp[i * 4 + 1] + aucTemp[i * 4 + 2] + aucTemp[i * 4 + 3]) / 3.0f);
            }
        } else {
            try {
                m_kVolumeImageA.GetImage().exportData(0, iSize, m_aucData);
            } catch (final IOException e) {
                e.printStackTrace();
            }
        }
    }

    private final Vector2f[][] m_akSteps = new Vector2f[7][7];

    private final int[][] m_aiIndexValues = new int[7][7];

    private final float[][] m_afAverages = new float[7][7];

    private final int m_iMM = 1;

    private final int m_iM = 2;

    private final int m_i_ = 3;

    private final int m_iP = 4;

    private final int m_iPP = 5;

    private void setIndices(final int iX, final int iY, final int iZ) {
        for (int i = 0; i < 7; i++) {
            for (int j = 0; j < 7; j++) {
                if (m_akSteps[i][j] == null) {
                    m_akSteps[i][j] = new Vector2f(iX + j - 3, iY + i - 3);
                } else {
                    m_akSteps[i][j].Set(iX + j - 3, iY + i - 3);
                }
            }
        }

        final int[] extents = m_kVolumeImageA.GetImage().getExtents();
        final Vector3f kVolumePt = new Vector3f();
        final Vector3f kPatientPt = new Vector3f();

        for (int i = 0; i < 7; i++) {
            for (int j = 0; j < 7; j++) {
                kPatientPt.Set(m_akSteps[i][j].X, m_akSteps[i][j].Y, iZ);
                MipavCoordinateSystems.patientToFile(kPatientPt, kVolumePt, m_kVolumeImageA.GetImage(),
                        m_iPlaneOrientation);
                m_aiIndexValues[i][j] = (int) (kVolumePt.Z * extents[0] * extents[1] + kVolumePt.Y * extents[0] + kVolumePt.X);
            }
        }

        for (int i = 1; i < 6; i++) {
            for (int j = 1; j < 6; j++) {
                m_afAverages[i][j] = avgPix(j, i);
            }
        }
    }

}
