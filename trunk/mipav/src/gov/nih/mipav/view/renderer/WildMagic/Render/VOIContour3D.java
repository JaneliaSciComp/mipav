package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogVOISplitter;
import gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManager;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Polygon;
import java.text.DecimalFormat;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class VOIContour3D extends LocalVolumeVOI
{

    /**  */
    private static final long serialVersionUID = -3138943850520191084L;


    public VOIContour3D( VOIManager parent, ScreenCoordinateListener kContext, int iOrientation, int iType, int iSType, Vector<Vector3f> kLocal, int iZ )
    {
        super(parent, kContext, iOrientation, iType, iSType, kLocal, iZ );
    }        

    public VOIContour3D( VOIManager parent, ScreenCoordinateListener kContext, int iOrientation, int iType, Vector<Vector3f> kLocal, boolean bIsFile )
    {
        super(parent, kContext, iOrientation, iType, kLocal, bIsFile );
    }

    public void add( VOIManager parent, int iPos, Vector3f kNewPoint, boolean bIsFile  )
    {
        if ( m_iVOISpecialType == VOIManager.LEVELSET )
        {
            return;
        }
        Vector3f kFilePt = kNewPoint;
        if ( !bIsFile )
        {
            kFilePt = new Vector3f();
            m_kDrawingContext.screenToFile( (int)kNewPoint.X, (int)kNewPoint.Y, (int)kNewPoint.Z, kFilePt);
        }
        if ( (iPos + 1) < size() )
        {
            //Local.insertElementAt( kNewPoint, iPos + 1);
            insertElementAt( kFilePt, iPos + 1);
        }
        else
        {
            //Local.add( kNewPoint );
            add( kFilePt );
        }
        lastPoint = iPos + 1;        
        nearPoint = iPos + 1;
        m_bUpdateCenter = true;

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }

    public void add( VOIManager parent, Vector3f kNewPoint, boolean bIsFile  )
    {
        add( parent, size() - 1, kNewPoint, bIsFile );
    }
    
    public VOIContour3D Clone( )
    {
        return new VOIContour3D( m_kParent, m_kDrawingContext, m_iOrientation, m_iVOIType, this, true );
    }

    public VOIContour3D Clone( int iZ )
    {
        return new VOIContour3D( m_kParent, m_kDrawingContext, m_iOrientation, m_iVOIType, m_iVOISpecialType, this, iZ );
    }

    public boolean contains( int iOrientation, int iX, int iY, int iZ ) {
        if ( iZ != slice() || iOrientation != m_iOrientation )
        {
            return false;
        }
        boolean isInside = false;
        float fX = iX + 0.49f;
        float fY = iY + 0.49f;

        int iLast = size() -1;
        for ( int i = 0; i < size(); i++ )
        {
            Vector3f kFilePos = get(i);
            Vector3f kFilePosL = get(iLast);
            Vector3f kPos = m_kDrawingContext.fileToScreen( kFilePos );
            Vector3f kPosL = m_kDrawingContext.fileToScreen( kFilePosL );

            if (((kPosL.Y <= fY) && (fY < kPos.Y) && (areaTwice(kPos.X, kPos.Y, kPosL.X, kPosL.Y, fX, fY) >= 0)) ||
                    ((kPos.Y <= fY) && (fY < kPosL.Y) && (areaTwice(kPosL.X, kPosL.Y, kPos.X, kPos.Y, fX, fY) >= 0))) {
                isInside = !isInside;
            }

            iLast = i;
        }
        return isInside;
    }


    public void drawGeometricCenter(Graphics g) {
        int xS, yS;

        if (g == null) {
            MipavUtil
                    .displayError("VOIContour.drawGeometricCenter: grapics = null");

            return;
        }

        Vector3f gcPt = getLocalCenter();
        xS = (int)gcPt.X;
        yS = (int)gcPt.Y;
        g.drawLine(xS, yS - 3, xS, yS + 3);
        g.drawLine(xS - 3, yS, xS + 3, yS);

        int iContourID = getContourID();
        if ( iContourID != -1 )
        {
            iContourID++;
            label = String.valueOf(iContourID);
        }
        
        if (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && (name != null)) {
            g.drawString(name, xS - 10, yS - 5);
        } else if (label != null) {
            g.drawString(label, xS - 10, yS - 5);
        }
    }

    public void drawGeometricCenterLabel(float scaleX, float scaleY,
            float resolutionX, float resolutionY, Graphics g) {
        int xS, yS;

        if (g == null) {
            MipavUtil
                    .displayError("VOIContour.drawGeometricCenter: grapics = null");

            return;
        }

        Vector3f gcPt = getLocalCenter();
        xS = (int)gcPt.X;
        yS = (int)gcPt.Y;
        

        int iContourID = getContourID();
        if ( iContourID != -1 )
        {
            iContourID++;
            label = String.valueOf(iContourID);
        }

        if (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && (name != null)) {
            g.drawString(name, xS - 10, yS - 5);
        } else if (label != null) {
            g.drawString(label, xS - 10, yS - 5);
        }
    }


    public void drawSelf(float[] resols, int[] unitsOfMeasure, Graphics g, int slice, int orientation ) {
        Polygon gon = null;
        int j;
        new DecimalFormat(".##");

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawSelf: grapics = null");

            return;
        }

        if ( getGroup() != null )
        {
            g.setColor( getGroup().getColor() );
        }
        else
        {
            g.setColor( Color.yellow );
        }
        
        gon = scalePolygon();

        if (active) {
            if (getClosed()) {
                drawGeometricCenter(g);
            } else {
                //drawLength(g, zoomX, zoomY, unitsOfMeasure, resols);
            }
        } //else if (doGeometricCenterLabel && getClosed()) {
        //    drawGeometricCenterLabel(zoomX, zoomY, resolutionX, resolutionY, g);
        //}
        
        int thickness = 1;
        if (thickness == 1) {
            if (getClosed() == true) {
                g.drawPolygon(gon);

            } else {
                g.drawPolyline(gon.xpoints, gon.ypoints, gon.npoints);
            }
        } 
        if (active == true && getSType() != VOIManager.LEVELSET
                && getSType() != VOIManager.LIVEWIRE) {

            // if active draw little boxes at points
            for (j = 0; j < size(); j++) {

                if (lastPoint == j) { // Do not draw (dragging point)
                } else {
                    g.setColor(Color.white);
                    g.fillRect((int) (gon.xpoints[j] - 1.5 + 0.5f),
                            (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                    g.setColor(Color.black);
                    g.drawRect((int) (gon.xpoints[j] - 1.5 + 0.5f),
                            (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                }
            }

            // draw the 1st point only if not dragging the first point and if
            // the active point (lastPoint)
            // is not the first point
            if (lastPoint != 0) {
                g.setColor(Color.yellow);
                g.drawRect((int) (gon.xpoints[0] - 1.5 + 0.5f),
                        (int) (gon.ypoints[0] - 1.5 + 0.5f), 3, 3);
            }
            // draw the active point dragging is taking place
            if ((lastPoint >= 0) && (this.size() > lastPoint)) {
                g.setColor(Color.GREEN);
                g.fillRect((int) (gon.xpoints[lastPoint] - 1.5 + 0.5f),
                        (int) (gon.ypoints[lastPoint] - 1.5 + 0.5f), 3, 3);
            }
        }
    }

    public void setPosition( VOIManager parent, int iPos, float fX, float fY, float fZ )
    {
        if ( m_iVOISpecialType == VOIManager.LEVELSET )
        {
            return;
        }
        if ( iPos < size() )
        {
            m_bUpdateCenter = true;
            Vector3f kPos = new Vector3f( fX, fY, fZ );
            Vector3f kVolumePt = new Vector3f();
            m_kDrawingContext.screenToFile( (int)kPos.X, (int)kPos.Y, (int)kPos.Z, kVolumePt );
            set( iPos, kVolumePt );
            lastPoint = iPos;


            if ( m_kVolumeVOI != null )
            {
                m_kVolumeVOI.setVOI(this);
            }
        }

    }

    public void setPosition( VOIManager parent, int iPos, Vector3f kPos )
    {
        if ( m_iVOISpecialType == VOIManager.LEVELSET )
        {
            return;
        }
        if ( iPos < size() )
        {
            m_bUpdateCenter = true;
            Vector3f kVolumePt = new Vector3f();
            m_kDrawingContext.screenToFile( (int)kPos.X, (int)kPos.Y, (int)kPos.Z, kVolumePt );
            set( iPos, kVolumePt );
            lastPoint = iPos;

            if ( m_kVolumeVOI != null )
            {
                m_kVolumeVOI.setVOI(this);
            }
        }
    }
    
    
    public LocalVolumeVOI split ( Vector3f kStartPt, Vector3f kEndPt )
    {
        int iVOISlice = slice();
        int iPoints = size();
        Vector3f kFirstIntersectionPt = null;
        Vector3f kSecondIntersectionPt = null;
        int iFirstIndex = -1;
        int iSecondIndex = -1;
        for ( int iP = 0; iP < (iPoints - 1) && (kSecondIntersectionPt == null); iP++ )
        {
            Vector3f kLocal1 = m_kParent.fileCoordinatesToPatient(get(iP));
            Vector3f kLocal2 = m_kParent.fileCoordinatesToPatient(get(iP+1));
            Vector3f kIntersection = new Vector3f();

            if (JDialogVOISplitter.intersects( kLocal1, kLocal2, kStartPt, kEndPt, kIntersection )) {
                if (kFirstIntersectionPt == null)
                {
                    kFirstIntersectionPt = kIntersection;
                    iFirstIndex = iP;
                } 
                else 
                {
                    kSecondIntersectionPt = kIntersection;
                    iSecondIndex = iP;
                }
            }
        }
        if ( kSecondIntersectionPt == null )
        {
            Vector3f kLocal1 = m_kParent.fileCoordinatesToPatient(lastElement());
            Vector3f kLocal2 = m_kParent.fileCoordinatesToPatient(firstElement());
            //Vector3f kLocal1 = kVOI.Volume.lastElement();
            //Vector3f kLocal2 = kVOI.Volume.firstElement();
            Vector3f kIntersection = new Vector3f();

            if (JDialogVOISplitter.intersects( kLocal1, kLocal2, kStartPt, kEndPt, kIntersection )) {

                kSecondIntersectionPt = kIntersection;
                iSecondIndex = iPoints - 1;
            }
        }

        if (kFirstIntersectionPt != null && kSecondIntersectionPt != null) 
        {        
            kFirstIntersectionPt.Z = iVOISlice;
            kFirstIntersectionPt = m_kParent.patientCoordinatesToFile(kFirstIntersectionPt);
            kSecondIntersectionPt.Z = iVOISlice;
            kSecondIntersectionPt = m_kParent.patientCoordinatesToFile(kSecondIntersectionPt);


            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            kPositions.add( kSecondIntersectionPt );  
            //check if there are points from second index to 0-index, add those first
            for (int iP = iSecondIndex + 1; iP < iPoints; iP++) {
                kPositions.add(get(iP));
            }
            for (int iP = 0; iP < iFirstIndex + 1; iP++) {
                kPositions.add(get(iP));
            }
            kPositions.add( kFirstIntersectionPt );  

            for (int iP = 00; iP < kPositions.size(); iP++) {
                remove(kPositions.get(iP));
            }
            add(0, new Vector3f(kFirstIntersectionPt) );
            add(0, new Vector3f(kSecondIntersectionPt) );      


            if ( m_kVolumeVOI != null )
            {
                m_kVolumeVOI.setVOI(this);
            }

            return new VOIContour3D( m_kParent, m_kDrawingContext, m_iOrientation, VOI.CONTOUR, kPositions, true );
            //m_kParent.updateCurrentVOI(null, kNew);
        }
        return null;
    }
    

    private Polygon scalePolygon() {
        int i;
        int x;
        int y;
        Polygon scaledGon = null;

        try {
            scaledGon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            Vector3f kVolumePt = elementAt(i);
            Vector3f kScreen = m_kDrawingContext.fileToScreen( kVolumePt );
            
            x = (int) kScreen.X;
            y = (int) kScreen.Y;
            scaledGon.addPoint(x, y);
        }

        return scaledGon;
    }

}
