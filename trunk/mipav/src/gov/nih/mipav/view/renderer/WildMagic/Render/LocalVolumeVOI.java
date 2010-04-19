package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.*;

import java.awt.Graphics;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;

import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Shaders.Program;


public abstract class LocalVolumeVOI extends VOIBase {
    private static final long serialVersionUID = 7912877738874526203L;

    protected static int ID = 0;

    protected ScreenCoordinateListener m_kDrawingContext = null;

    protected Vector3f m_kLocalCenter = new Vector3f();

    protected boolean m_bUpdateCenter = true;

    protected boolean m_bUpdateBounds = true;

    protected VOIManager m_kParent;

    protected int m_iVOIType;

    protected int m_iVOISpecialType;

    protected int m_iAnchorIndex = -1;

    protected boolean m_bClosed = true;

    protected int m_iOrientation;

    protected Vector3f[] m_akMinMax = new Vector3f[] {new Vector3f(), new Vector3f()};

    protected ColorRGBA m_kColor = new ColorRGBA();

    protected VolumeVOI m_kVolumeVOI;

    protected int m_iCirclePts = 32;

    protected double[] m_adCos = new double[m_iCirclePts];

    protected double[] m_adSin = new double[m_iCirclePts];

    protected Polyline m_kBallPoint = null;

    protected ZBufferState m_kZState = null;

    protected Attributes m_kVOIAttr = null;

    public LocalVolumeVOI(final VOIManager parent, final ScreenCoordinateListener kContext, final int iOrientation,
            final int iType, final int iSType, final Vector<Vector3f> kLocal, final int iZ) {
        m_kParent = parent;
        m_kDrawingContext = kContext;
        m_iOrientation = iOrientation;
        m_iVOIType = iType;
        name = new String("VOI" + LocalVolumeVOI.ID++);
        for (int i = 0; i < kLocal.size(); i++) {
            final Vector3f kPos = m_kParent.fileCoordinatesToPatient(kLocal.get(i));
            kPos.Z = iZ;
            // Local.add( kPos );
            add(parent.patientCoordinatesToFile(kPos));
        }
        lastPoint = size() - 1;
        m_iVOISpecialType = iSType;
        if ( (m_iVOISpecialType == VOIManager.LIVEWIRE)) {
            m_bClosed = false;
        }
    }

    public LocalVolumeVOI(final VOIManager parent, final ScreenCoordinateListener kContext, final int iOrientation,
            final int iType, final int iSType, final Vector<Vector3f> kLocal, final boolean bIsFile) {
        super();
        m_kParent = parent;
        m_kDrawingContext = kContext;
        m_iOrientation = iOrientation;
        m_iVOIType = VOI.CONTOUR_3D;
        name = new String("VOI" + LocalVolumeVOI.ID++);
        if (kLocal != null) {
            for (int i = 0; i < kLocal.size(); i++) {
                final Vector3f kPos = new Vector3f(kLocal.get(i));
                // Local.add( kPos );
                if (bIsFile) {
                    add(kPos);
                } else {
                    final Vector3f kVolumePt = new Vector3f();
                    m_kDrawingContext.screenToFile((int) kPos.X, (int) kPos.Y, (int) kPos.Z, kVolumePt);
                    add(kVolumePt);
                }
                // System.err.println( parent.screenToFileCoordinates( kPos ).ToString() );
            }
        }
        lastPoint = size() - 1;
        m_iVOISpecialType = iSType;
        if ( (m_iVOISpecialType == VOIManager.LIVEWIRE) || (m_iVOISpecialType == VOIManager.POLYLINE)
                || (m_iVOISpecialType == VOIManager.POLYPOINT)) {
            m_bClosed = false;
        }
    }

    public abstract boolean add(int iPos, Vector3f kNewPoint, boolean bIsFile);

    public abstract boolean add(Vector3f kNewPoint, boolean bIsFile);

    public Vector3f[] getBoundingBox() {
        if (m_bUpdateBounds) {
            for (int i = 0; i < size(); i++) {
                final Vector3f kLocalPt = m_kDrawingContext.fileToScreen(get(i));
                if (i == 0) {
                    m_akMinMax[0].Copy(kLocalPt);
                    m_akMinMax[1].Copy(kLocalPt);
                }
                m_akMinMax[0].Min(kLocalPt);
                m_akMinMax[1].Max(kLocalPt);
            }
            m_bUpdateBounds = false;
        }
        return m_akMinMax;
    }

    public LocalVolumeVOI(final LocalVolumeVOI kVOI) {
        super(kVOI);
        this.m_kDrawingContext = kVOI.m_kDrawingContext;

        this.m_kLocalCenter.Copy(kVOI.m_kLocalCenter);
        this.m_bUpdateCenter = kVOI.m_bUpdateCenter;
        this.m_bUpdateBounds = kVOI.m_bUpdateBounds;
        this.m_kParent = kVOI.m_kParent;
        this.m_iVOIType = kVOI.m_iVOIType;
        this.m_iVOISpecialType = kVOI.m_iVOISpecialType;
        this.m_iAnchorIndex = kVOI.m_iAnchorIndex;
        this.m_bClosed = kVOI.m_bClosed;
        this.m_iOrientation = kVOI.m_iOrientation;

        this.m_akMinMax[0].Copy(kVOI.m_akMinMax[0]);
        this.m_akMinMax[1].Copy(kVOI.m_akMinMax[1]);

        this.m_kVolumeVOI = null;
    }

    public LocalVolumeVOI(final LocalVolumeVOI kVOI, final int iZ) {
        super(kVOI);
        this.m_kDrawingContext = kVOI.m_kDrawingContext;

        this.m_kLocalCenter.Copy(kVOI.m_kLocalCenter);
        this.m_bUpdateCenter = kVOI.m_bUpdateCenter;
        this.m_bUpdateBounds = kVOI.m_bUpdateBounds;
        this.m_kParent = kVOI.m_kParent;
        this.m_iVOIType = kVOI.m_iVOIType;
        this.m_iVOISpecialType = kVOI.m_iVOISpecialType;
        this.m_iAnchorIndex = kVOI.m_iAnchorIndex;
        this.m_bClosed = kVOI.m_bClosed;
        this.m_iOrientation = kVOI.m_iOrientation;

        this.m_akMinMax[0].Copy(kVOI.m_akMinMax[0]);
        this.m_akMinMax[1].Copy(kVOI.m_akMinMax[1]);

        this.m_kVolumeVOI = null;
        clear();
        for (int i = 0; i < kVOI.size(); i++) {
            final Vector3f kPos = m_kParent.fileCoordinatesToPatient(kVOI.get(i));
            kPos.Z = iZ;
            add(m_kParent.patientCoordinatesToFile(kPos));
        }
    }

    public abstract LocalVolumeVOI clone();

    public abstract LocalVolumeVOI clone(int iZ);

    public boolean contains(final int x, final int y, final boolean forceReload) {
        return contains(m_iOrientation, x, y, slice());
    }

    public abstract boolean contains(int iOrientation, int iX, int iY, int iZ);

    public void cycleActivePt(final int keyCode) {

        if ( (lastPoint >= 0) && (lastPoint < this.size())) {
            int index = lastPoint;

            switch (keyCode) {

                case UP:
                case RIGHT:
                    index++;
                    break;

                case DOWN:
                case LEFT:
                    index--;
                    break;
            }

            if (index < 0) {
                index = this.size() - 1;
            } else if (index > (size() - 1)) {
                index = 0;
            }

            lastPoint = index;
        }
    }

    public void delete(final int iPos) {
        remove(iPos);
        lastPoint = Math.max(0, iPos - 1);
        m_bUpdateCenter = true;
        m_bUpdateBounds = true;

        if (m_kVolumeVOI != null) {
            m_kVolumeVOI.setVOI(this);
        }
    }

    public void dispose() {
        name = null;
    }

    public void draw(final float zoomX, final float zoomY, final float[] resolutions, final int[] unitsOfMeasure,
            final int slice, final int orientation, final Graphics g) {
        if ( (orientation != m_iOrientation) || (slice() != slice)) {
            return;
        }
        drawSelf(resolutions, unitsOfMeasure, g, slice, orientation);
    }

    public void draw(final PlaneRender_WM kDisplay, final Renderer kRenderer, final Culler kCuller,
            final float[] afResolutions, final int[] aiUnits, final int[] aiAxisOrder, final Vector3f kCenter,
            final int iSlice, final int iOrientation, final Vector3f kVolumeScale, final Vector3f kTranslate) {
        if (m_kVolumeVOI == null) {
            return;
        }

        // if ( iOrientation == m_iOrientation )
        {
            // if ( slice() == iSlice )
            {
                final boolean bDisplaySave = m_kVolumeVOI.GetDisplay();
                final Matrix3f kSave = new Matrix3f(m_kVolumeVOI.GetScene().Local.GetRotate());
                m_kVolumeVOI.GetScene().Local.SetRotateCopy(Matrix3f.IDENTITY);
                m_kVolumeVOI.SetDisplay(true);
                m_kVolumeVOI.showTextBox(false);
                m_kVolumeVOI.setZCompare(false);

                // System.err.println( aiAxisOrder[2] + " " + kCenter.Y );
                //
                if (aiAxisOrder[2] == 0) {
                    m_kVolumeVOI.setSlice(true, aiAxisOrder[2], kCenter.X);
                } else if (aiAxisOrder[2] == 1) {
                    m_kVolumeVOI.setSlice(true, aiAxisOrder[2], kCenter.Y);
                } else {
                    m_kVolumeVOI.setSlice(true, aiAxisOrder[2], kCenter.Z);
                }
                m_kVolumeVOI.Render(kRenderer, kCuller, false, true);
                // m_kVolumeVOI.setSlice(true, aiAxisOrder[2], kCenter.Y);
                // m_kVolumeVOI.Render( kRenderer, kCuller, false, true );
                // m_kVolumeVOI.setSlice(true, aiAxisOrder[2], kCenter.Z);
                // m_kVolumeVOI.Render( kRenderer, kCuller, false, true );

                m_kVolumeVOI.setZCompare(true);
                m_kVolumeVOI.showTextBox(true);
                m_kVolumeVOI.GetScene().Local.SetRotateCopy(kSave);
                m_kVolumeVOI.SetDisplay(bDisplaySave);
                m_kVolumeVOI.setSlice(false, 0, -1);

                drawVOI(kRenderer, iSlice, afResolutions, aiUnits, m_kVolumeVOI, kVolumeScale, kTranslate,
                        iOrientation, aiAxisOrder);
            }
        }
    }

    public boolean draw(final VolumeTriPlanarRender kDisplay, final Renderer kRenderer, final GLAutoDrawable kDrawable,
            final Camera kCamera, final VolumeImage kVolumeImage, final Vector3f kTranslate) {
        boolean bReturn = false;
        if (m_kVolumeVOI == null) {
            createVolumeVOI(kRenderer, kDrawable, kCamera, kVolumeImage, kTranslate);
            bReturn = true;
        }

        m_kVolumeVOI.showTextBox(true);
        m_kVolumeVOI.setZCompare(true);
        kDisplay.addVolumeVOI(m_kVolumeVOI);
        // m_kVolumeVOI.Render( kRenderer, kCuller, bPreRender, bSolid );
        return bReturn;
    }

    public void drawSelf(final float zoomX, final float zoomY, final float resolutionX, final float resolutionY,
            final float originX, final float originY, final float[] resols, final int[] unitsOfMeasure,
            final int orientation, final Graphics g, final boolean boundingBox, final FileInfoBase fileInfo,
            final int dim, final int thickness) {}

    public void drawSelf(final float zoomX, final float zoomY, final float resolutionX, final float resolutionY,
            final float originX, final float originY, final float[] resols, final int[] unitsOfMeasure,
            final int orientation, final Graphics g, final boolean boundingBox, final int thickness) {}

    public abstract void drawSelf(float[] resols, int[] unitsOfMeasure, Graphics g, int slice, int orientation);

    public Vector3f getActivePt() {
        Vector3f pt = null;
        if ( (lastPoint >= 0) && (lastPoint < this.size())) {
            pt = elementAt(lastPoint);
        }
        return pt;
    }

    public int getAnchor() {
        return m_iAnchorIndex;
    }

    public boolean getClosed() {
        return m_bClosed;
    }

    public int getContourID() {
        if (voiGroup != null) {
            return voiGroup.getCurves()[0].indexOf(this);
        }
        return -1;
    }

    public Vector3f getLocalCenter() {
        if (m_bUpdateCenter) {
            m_bUpdateCenter = false;
            m_kLocalCenter.Set(0, 0, 0);
            for (int i = 0; i < size(); i++) {
                m_kLocalCenter.Add(get(i));
            }
            m_kLocalCenter.Scale(1.0f / size());
            m_kLocalCenter = m_kDrawingContext.fileToScreen(m_kLocalCenter);
        }
        return m_kLocalCenter;
    }

    public int getNearPoint() {
        return nearPoint;
    }

    public int getOrientation() {
        return m_iOrientation;
    }

    public float[] getOrigin(final FileInfoBase fileInfo, final int dim, final float originX, final float originY,
            final float[] resols) {
        return null;
    }

    public int getSelectedPoint() {
        return lastPoint;
    }

    public int getSType() {
        return m_iVOISpecialType;
    }

    public int getType() {
        return m_iVOIType;
    }

    public int GetVertexQuantity() {
        return size();
    }

    public void importArrays(final float[] x, final float[] y, final float[] z, final int n) {
        this.removeAllElements();
        for (int i = 0; i < Math.min(Math.min(x.length, y.length), Math.min(z.length, n)); i++) {
            add(new Vector3f(x[i], y[i], z[i]));
        }
    }

    public void importArrays(final int[] x, final int[] y, final int[] z, final int n) {
        this.removeAllElements();
        for (int i = 0; i < Math.min(Math.min(x.length, y.length), Math.min(z.length, n)); i++) {
            add(new Vector3f(x[i], y[i], z[i]));
        }
    }

    public void importPoints(final Vector3f[] pt) {
        this.removeAllElements();
        for (final Vector3f element : pt) {
            add(new Vector3f(element));
        }
    }

    public void move(final Vector3f kDiff, final Vector3f[] akMinMax) {
        if (m_iVOISpecialType == VOIManager.LEVELSET) {
            return;
        }
        final Vector3f kTest = new Vector3f();
        final int iSlice = slice();

        final Vector3f kScreenMin = new Vector3f(akMinMax[0].X, akMinMax[0].Y, iSlice);
        kScreenMin.Add(kDiff);

        final Vector3f kScreenMax = new Vector3f(akMinMax[1].X, akMinMax[1].Y, iSlice);
        kScreenMax.Add(kDiff);

        if (m_kDrawingContext.screenToFile(kScreenMin, kTest) || m_kDrawingContext.screenToFile(kScreenMax, kTest)) {
            return;
        }
        final int iNumPoints = size();
        if (iNumPoints > 0) {
            for (int i = 0; i < iNumPoints; i++) {
                final Vector3f kPos = get(i);
                final Vector3f kLocal = m_kDrawingContext.fileToScreen(kPos);
                kLocal.Add(kDiff);
                if (i == 0) {
                    m_akMinMax[0].Copy(kLocal);
                    m_akMinMax[1].Copy(kLocal);
                }
                m_akMinMax[0].Min(kLocal);
                m_akMinMax[1].Max(kLocal);
                final Vector3f kVolumePt = new Vector3f();
                m_kDrawingContext.screenToFile((int) kLocal.X, (int) kLocal.Y, (int) kLocal.Z, kVolumePt);
                set(i, kVolumePt);
            }
        }
        m_bUpdateCenter = true;

        if (m_kVolumeVOI != null) {
            m_kVolumeVOI.setVOI(this);
        }
    }

    public void moveActivePt(final int keyCode, final int xDim, final int yDim) {
        if ( (this.size() > lastPoint) && (lastPoint >= 0)) {
            final Vector3f kPosFile = get(lastPoint);
            final Vector3f kPos = m_kDrawingContext.fileToScreen(kPosFile);

            float x = kPos.X;
            float y = kPos.Y;

            switch (keyCode) {

                case UP:
                    y -= 1;
                    break;

                case LEFT:
                    x -= 1;
                    break;

                case DOWN:
                    y += 1;
                    break;

                case RIGHT:
                    x += 1;
                    break;

                default:
                    return;
            }

            if ( (x >= 0) && (x < xDim) && (y >= 0) && (y < yDim)) {
                m_bUpdateBounds = true;
                m_kDrawingContext.screenToFile((int) x, (int) y, (int) kPos.Z, kPosFile);
                this.set(lastPoint, kPosFile);
            }
        }
    }

    public boolean nearLine(final int iX, final int iY, final int iZ) {
        final Vector3f kVOIPoint = new Vector3f(iX, iY, iZ);
        for (int i = 0; i < (size() - 1); i++) {
            Vector3f kPosFile = get(i);
            final Vector3f kPos0 = m_kDrawingContext.fileToScreen(kPosFile);

            kPosFile = get(i + 1);
            final Vector3f kPos1 = m_kDrawingContext.fileToScreen(kPosFile);

            final Vector3f kDir = new Vector3f();
            kDir.Sub(kPos1, kPos0);
            final float fLength = kDir.Normalize();
            final Segment3f kSegment = new Segment3f(kPos0, kDir, fLength);
            final DistanceVector3Segment3 kDist = new DistanceVector3Segment3(kVOIPoint, kSegment);
            final float fDist = kDist.Get();
            if (fDist < 3) {
                setNearPoint(i);
                return true;
            }
        }

        Vector3f kPosFile = get(0);
        final Vector3f kPos0 = m_kDrawingContext.fileToScreen(kPosFile);

        kPosFile = get(size() - 1);
        final Vector3f kPos1 = m_kDrawingContext.fileToScreen(kPosFile);

        final Vector3f kDir = new Vector3f();
        kDir.Sub(kPos1, kPos0);
        final float fLength = kDir.Normalize();
        final Segment3f kSegment = new Segment3f(kPos0, kDir, fLength);
        final DistanceVector3Segment3 kDist = new DistanceVector3Segment3(kVOIPoint, kSegment);
        final float fDist = kDist.Get();
        if (fDist < 3) {
            setNearPoint(size() - 1);
            return true;
        }
        return false;
    }

    public boolean nearPoint(final int iX, final int iY, final int iZ) {

        final Vector3f kVOIPoint = new Vector3f(iX, iY, iZ);
        for (int i = 0; i < size(); i++) {
            final Vector3f kFilePos = get(i);
            final Vector3f kPos = m_kDrawingContext.fileToScreen(kFilePos);
            final Vector3f kDiff = new Vector3f();
            kDiff.Sub(kPos, kVOIPoint);
            if ( (Math.abs(kDiff.X) < 3) && (Math.abs(kDiff.Y) < 3) && (Math.abs(kDiff.Z) < 3)) {
                setNearPoint(i);
                return true;
            }
        }
        return false;
    }

    public void setAnchor() {
        m_iAnchorIndex = size() - 1;
    }

    public void setClosed(final boolean bClosed) {
        m_bClosed = bClosed;
        if (m_bClosed) {
            m_iVOIType = VOI.CONTOUR_3D;
        } else {
            m_iVOIType = VOI.POLYLINE_3D;
        }
    }

    public void setNearPoint(final int i) {
        nearPoint = i;
    }

    public void setPosition(final VOIManager parent, final int iPos, final float fX, final float fY, final float fZ) {
        if (m_iVOISpecialType == VOIManager.LEVELSET) {
            return;
        }
        if (iPos < size()) {
            m_bUpdateCenter = true;
            m_bUpdateBounds = true;
            final Vector3f kPos = new Vector3f(fX, fY, fZ);
            final Vector3f kVolumePt = new Vector3f();
            m_kDrawingContext.screenToFile((int) kPos.X, (int) kPos.Y, (int) kPos.Z, kVolumePt);
            set(iPos, kVolumePt);
            lastPoint = iPos;

            if (m_kVolumeVOI != null) {
                m_kVolumeVOI.setVOI(this);
            }
        }

    }

    public void setPosition(final VOIManager parent, final int iPos, final Vector3f kPos) {
        if (m_iVOISpecialType == VOIManager.LEVELSET) {
            return;
        }
        if (iPos < size()) {
            m_bUpdateCenter = true;
            m_bUpdateBounds = true;
            final Vector3f kVolumePt = new Vector3f();
            m_kDrawingContext.screenToFile((int) kPos.X, (int) kPos.Y, (int) kPos.Z, kVolumePt);
            set(iPos, kVolumePt);
            lastPoint = iPos;

            if (m_kVolumeVOI != null) {
                m_kVolumeVOI.setVOI(this);
            }
        }
    }

    public void setSelectedPoint(final int i) {
        lastPoint = i;
        nearPoint = i;
    }

    public void setSType(final int iType) {
        m_iVOISpecialType = iType;
    }

    public int slice() {
        final Vector3f kPos = m_kParent.fileCoordinatesToPatient(get(0));
        return (int) kPos.Z;
    }

    public abstract LocalVolumeVOI split(Vector3f kStartPt, Vector3f kEndPt);

    public void update(final ColorRGBA kColor) {
        m_kColor.Copy(kColor);
        if (m_kVolumeVOI != null) {
            m_kVolumeVOI.update(kColor);
        }
    }

    protected float areaTwice(final float ptAx, final float ptAy, final float ptBx, final float ptBy, final float ptCx,
            final float ptCy) {
        return ( ( (ptAx - ptCx) * (ptBy - ptCy)) - ( (ptAy - ptCy) * (ptBx - ptCx)));
    }

    protected void createSelectedIcon(final int[] aiAxisOrder) {

        m_kZState = new ZBufferState();
        m_kZState.Compare = ZBufferState.CompareMode.CF_ALWAYS;
        m_kVOIAttr = new Attributes();
        m_kVOIAttr.SetPChannels(3);
        m_kVOIAttr.SetCChannels(0, 3);

        final VertexBuffer kBuffer = new VertexBuffer(m_kVOIAttr, 4);
        if (aiAxisOrder[2] == 2) {
            kBuffer.SetPosition3(0, -1.0f / 200f, -1.0f / 200f, 0);
            kBuffer.SetPosition3(1, 1.0f / 200f, -1.0f / 200f, 0);
            kBuffer.SetPosition3(2, 1.0f / 200f, 1.0f / 200f, 0);
            kBuffer.SetPosition3(3, -1.0f / 200f, 1.0f / 200f, 0);
        } else if (aiAxisOrder[2] == 1) {
            kBuffer.SetPosition3(0, -1.0f / 200f, 0, -1.0f / 200f);
            kBuffer.SetPosition3(1, 1.0f / 200f, 0, -1.0f / 200f);
            kBuffer.SetPosition3(2, 1.0f / 200f, 0, 1.0f / 200f);
            kBuffer.SetPosition3(3, -1.0f / 200f, 0, 1.0f / 200f);
        } else {
            kBuffer.SetPosition3(0, 0, -1.0f / 200f, -1.0f / 200f);
            kBuffer.SetPosition3(1, 0, 1.0f / 200f, -1.0f / 200f);
            kBuffer.SetPosition3(2, 0, 1.0f / 200f, 1.0f / 200f);
            kBuffer.SetPosition3(3, 0, -1.0f / 200f, 1.0f / 200f);
        }

        for (int i = 0; i < kBuffer.GetVertexQuantity(); i++) {
            kBuffer.SetColor3(0, i, 1, 1, 1);
        }

        m_kBallPoint = new Polyline(kBuffer, true, true);
        m_kBallPoint.AttachEffect(new VertexColor3Effect("ConstantColor", true));
        m_kBallPoint.AttachGlobalState(m_kZState);
        m_kBallPoint.UpdateRS();

        for (int i = 0; i < m_iCirclePts; i++) {
            m_adCos[i] = Math.cos(Math.PI * 2.0 * i / m_iCirclePts);
            m_adSin[i] = Math.sin(Math.PI * 2.0 * i / m_iCirclePts);
        }
    }

    protected void createVolumeVOI(final Renderer kRenderer, final GLAutoDrawable kDrawable, final Camera kCamera,
            final VolumeImage kVolumeImage, final Vector3f kTranslate) {
        m_kVolumeVOI = new VolumeVOI(kRenderer, kDrawable, kVolumeImage, kTranslate, kCamera, this, m_kColor);
    }

    protected void drawSelectedPoints(final Renderer kRenderer, final Vector3f kVolumeScale, final Vector3f kTranslate,
            final int iOrientation, final int[] aiAxisOrder) {
        if (m_kBallPoint == null && (iOrientation == m_iOrientation)) {
            createSelectedIcon(aiAxisOrder);
        }
        final Vector3f kLocalTranslate = new Vector3f();
        for (int j = 0; j < size(); j++) {
            kLocalTranslate.Copy(get(j));
            kLocalTranslate.Mult(kVolumeScale);
            kLocalTranslate.Add(kTranslate);
            m_kBallPoint.Local.SetTranslate(kLocalTranslate);
            m_kBallPoint.UpdateGS();

            final Program kProgram = ((ShaderEffect) m_kBallPoint.GetEffect(0)).GetCProgram(0);
            if (kProgram != null) {
                if (kProgram.GetUC("ConstantColor") != null) {
                    if (j == getSelectedPoint()) {
                        kProgram.GetUC("ConstantColor").GetData()[0] = 0;
                        kProgram.GetUC("ConstantColor").GetData()[1] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[2] = 0;
                    } else if (j == 0) {
                        kProgram.GetUC("ConstantColor").GetData()[0] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[1] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[2] = 0;
                    } else {
                        kProgram.GetUC("ConstantColor").GetData()[0] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[1] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[2] = 1;
                    }
                }

                if (kProgram.GetUC("UseConstantColor") != null) {
                    kProgram.GetUC("UseConstantColor").GetData()[0] = 1.0f;
                }
            }
            kRenderer.Draw(m_kBallPoint);
        }
    }

    protected void drawText(final Renderer kRenderer, final int iX, final int iY, final ColorRGBA kColor,
            final char[] acText) {
        kRenderer.Draw(iX, iY - 1, ColorRGBA.BLACK, acText);
        kRenderer.Draw(iX, iY + 1, ColorRGBA.BLACK, acText);
        kRenderer.Draw(iX - 1, iY, ColorRGBA.BLACK, acText);
        kRenderer.Draw(iX + 1, iY, ColorRGBA.BLACK, acText);

        kRenderer.Draw(iX, iY, kColor, acText);
    }

    protected void drawVOI(final Renderer kRenderer, final int iSlice, final float[] afResolutions,
            final int[] aiUnits, final VolumeVOI kVolumeVOI, final Vector3f kVolumeScale, final Vector3f kTranslate,
            final int iOrientation, final int[] aiAxisOrder) {
        if (isActive()) {
            final int iNumPoints = GetVertexQuantity();
            if (iNumPoints > 0) {
                if (iSlice == slice()) {

                    final Vector3f kCenter = getLocalCenter();

                    if ( (m_iVOISpecialType != VOIManager.SPLITLINE) || (m_iVOISpecialType != VOIManager.POLYPOINT)) {
                        String kMessage = new String("+");
                        char[] acText = kMessage.toCharArray();
                        final int[] aiSize = kRenderer.GetSizeOnScreen(acText);
                        drawText(kRenderer, (int) kCenter.X - aiSize[0] / 2, (int) kCenter.Y + aiSize[1] / 2,
                                kVolumeVOI.getColor(), acText);

                        int iContourID = getContourID();
                        if (iContourID != -1) {
                            iContourID++;
                            kMessage = String.valueOf(iContourID);
                            acText = kMessage.toCharArray();
                            drawText(kRenderer, (int) kCenter.X - aiSize[0] / 2 - 10, (int) kCenter.Y + aiSize[1] / 2
                                    - 5, kVolumeVOI.getColor(), acText);
                        }
                    }
                    drawSelectedPoints(kRenderer, kVolumeScale, kTranslate, iOrientation, aiAxisOrder);
                }
            }
        }
    }

    protected String getLengthString(final int iPos0, final int iPos1, final float[] afResolutions, final int[] aiUnits) {
        if (iPos0 >= size() || iPos1 >= size()) {
            return null;
        }
        final Vector3f kStart = m_kParent.fileCoordinatesToPatient(get(iPos0));
        final Vector3f kEnd = m_kParent.fileCoordinatesToPatient(get(iPos1));
        final float[] x = new float[2];
        x[0] = kStart.X;
        x[1] = kEnd.X;

        final float[] y = new float[2];
        y[0] = kStart.Y;
        y[1] = kEnd.Y;

        final double length = MipavMath.length(x, y, afResolutions);

        String tmpString = String.valueOf(length);
        final int i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        tmpString = tmpString + " " + FileInfoBase.getUnitsOfMeasureAbbrevStr(aiUnits[0]);
        return tmpString;
    }

    protected double getTotalLength(final float[] afResolutions) {
        final float[] x = new float[2];
        final float[] y = new float[2];
        double length = 0;
        for (int i = 0; i < size() - 1; i++) {
            final Vector3f kStart = m_kParent.fileCoordinatesToPatient(get(i));
            final Vector3f kEnd = m_kParent.fileCoordinatesToPatient(get(i + 1));
            x[0] = kStart.X;
            x[1] = kEnd.X;
            y[0] = kStart.Y;
            y[1] = kEnd.Y;

            length += MipavMath.length(x, y, afResolutions);
        }
        return length;
    }

    protected String getTotalLengthString(final float[] afResolutions, final int[] aiUnits) {
        final double length = getTotalLength(afResolutions);

        String tmpString = String.valueOf(length);
        final int i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        tmpString = tmpString + " " + FileInfoBase.getUnitsOfMeasureAbbrevStr(aiUnits[0]);
        return tmpString;

    }
}
