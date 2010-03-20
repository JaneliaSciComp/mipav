package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManager;

import java.awt.Color;
import java.awt.Graphics;
import java.util.Vector;

import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.Renderer;

public class VOIPolyLineSlice3D extends LocalVolumeVOI
{
    /**  */
    private static final long serialVersionUID = -7909604245705492799L;
    Vector< VOIPoint3D > m_kPoints = new Vector<VOIPoint3D>();

    public VOIPolyLineSlice3D( VOIPoint3D kPoint )
    {
        super(kPoint.m_kParent, null, kPoint.m_iOrientation, VOI.POLYLINE_SLICE_3D, -1, kPoint, true);
        m_kPoints.add(kPoint);
        setGroup(kPoint.getGroup());
        m_iVOIType = VOI.POLYLINE_SLICE_3D;
    }   

    public boolean add( int iPos, Vector3f kNewPoint, boolean bIsFile  ) 
    {
        return false;
    }

    public boolean add( Vector3f kNewPoint, boolean bIsFile) 
    {
        return false;
    }

    public void add( VOIPoint3D kPoint )
    {
        super.add( kPoint.get(0) );
        m_kPoints.add(kPoint);

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }


    public Vector3f[] getBoundingBox()
    {
        if ( m_bUpdateBounds )
        {
            for ( int i = 0; i < m_kPoints.size(); i++ )
            {
                Vector3f kLocalPt = m_kPoints.get(i).fileToScreen();
                if ( i == 0 )
                {
                    m_akMinMax[0].Copy(kLocalPt);
                    m_akMinMax[1].Copy(kLocalPt);
                }
                m_akMinMax[0].Min(kLocalPt);
                m_akMinMax[1].Max(kLocalPt);
            }
        }
        return m_akMinMax;
    }

    public VOIPolyLineSlice3D( VOIPolyLineSlice3D kVOI )
    {
        super( kVOI );
        for ( int i = 0; i < kVOI.m_kPoints.size(); i++ )
        {
            m_kPoints.add( new VOIPoint3D( kVOI.m_kPoints.get(i) ) );
        }
    }

    public VOIPolyLineSlice3D( VOIPolyLineSlice3D kVOI, int iZ )
    {
        super( kVOI, iZ );
        for ( int i = 0; i < kVOI.m_kPoints.size(); i++ )
        {
            m_kPoints.add( new VOIPoint3D( kVOI.m_kPoints.get(i) ) );
        }
    }

    public VOIPolyLineSlice3D clone() {
        return new VOIPolyLineSlice3D(this);
    }

    public VOIPolyLineSlice3D clone(int iZ) {
        return new VOIPolyLineSlice3D(this, iZ);
    }

    public boolean contains(int iOrientation, int iX, int iY, int iZ ) {
        getBoundingBox();
        if ( iZ < m_akMinMax[0].Z || iZ > m_akMinMax[1].Z )
        {
            return false;
        }
        boolean isInside = false;
        float fX = iX + 0.49f;
        float fY = iY + 0.49f;

        int iLast = m_kPoints.size() -1;
        for ( int i = 0; i < m_kPoints.size(); i++ )
        {
            Vector3f kPos = m_kPoints.get(i).fileToScreen();
            Vector3f kPosL = m_kPoints.get(iLast).fileToScreen( );

            if (((kPosL.Y <= fY) && (fY < kPos.Y) && (areaTwice(kPos.X, kPos.Y, kPosL.X, kPosL.Y, fX, fY) >= 0)) ||
                    ((kPos.Y <= fY) && (fY < kPosL.Y) && (areaTwice(kPosL.X, kPosL.Y, kPos.X, kPos.Y, fX, fY) >= 0))) {
                isInside = !isInside;
            }

            iLast = i;
        }
        if (isInside == true) {
            return isInside;
        }

        if ( nearLine( iX, iY, iZ ) )
        {
            return true;
        }
        return nearPoint( iX, iY, iZ );
    }
    
    public void delete( int iPos )
    {
        m_kPoints.remove(iPos);
        super.delete(iPos);
    }


    public void draw( float zoomX, float zoomY, 
            float[] resolutions, int[] unitsOfMeasure, int slice, int orientation,
            Graphics g ) 
    {
        drawSelf( resolutions, unitsOfMeasure, g, slice, orientation );
    }

    public void drawSelf( float[] resols, int[] unitsOfMeasure, Graphics g, int slice, int orientation ) {
        if ( g == null ) {
            MipavUtil.displayError( "VOIPoint.drawSelf: grapics = null" );
            return;
        }

        Color currentColor = g.getColor();
        
        String totalDistance = getTotalLengthString(resols, unitsOfMeasure);
        String dist = new String();
        for ( int i = 0; i < m_kPoints.size(); i++ )
        {
            if ( m_kPoints.get(i).slice() == slice && m_kPoints.get(i).getOrientation() == orientation )
            {
                dist = getLengthString( i, i+1, resols, unitsOfMeasure );
                m_kPoints.get(i).setFirstPoint( i==0, i==lastPoint, totalDistance, dist, i+1);
                m_kPoints.get(i).drawSelf(g,String.valueOf(i+1));
            }
        }

        g.setColor(currentColor);
        for ( int i = 0; i < m_kPoints.size()-1; i++ )
        {
            if ( (m_kPoints.get(i).slice() == slice && m_kPoints.get(i).getOrientation() == orientation) &&
                    (m_kPoints.get(i+1).slice() == slice && m_kPoints.get(i+1).getOrientation() == orientation) )
            {
                Vector3f kStart = m_kPoints.get(i).fileToScreen();
                Vector3f kEnd = m_kPoints.get(i+1).fileToScreen();
                g.drawLine((int)kStart.X, (int)kStart.Y, (int)kEnd.X, (int)kEnd.Y);
            }
        }
    }


    public void move( Vector3f kDiff, Vector3f[] akMinMax  )
    { 
        for ( int i = 0; i < m_kPoints.size(); i++ )
        {
            m_kPoints.get(i).move(kDiff, akMinMax);
            set( i, m_kPoints.get(i).get(0) );
        }
        m_bUpdateCenter = true;
        m_bUpdateBounds = true;

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }


    public boolean nearLine(int iX, int iY, int iZ )
    {
        Vector3f kVOIPoint = new Vector3f(iX, iY, iZ );
        for (int i = 0; i < (m_kPoints.size() - 1); i++)
        {
            Vector3f kPos0 = m_kPoints.get(i).fileToScreen();
            Vector3f kPos1 = m_kPoints.get(i+1).fileToScreen();

            Vector3f kDir = new Vector3f();
            kDir.Sub( kPos1, kPos0 );
            float fLength = kDir.Normalize();
            Segment3f kSegment = new Segment3f(kPos0, kDir, fLength);
            DistanceVector3Segment3 kDist = new DistanceVector3Segment3( kVOIPoint, kSegment );
            float fDist = kDist.Get();
            if ( fDist < 3 )
            {
                setNearPoint(i);
                return true;
            }
        }

        Vector3f kPos0 = m_kPoints.get(0).fileToScreen();
        Vector3f kPos1 = m_kPoints.get(m_kPoints.size() - 1).fileToScreen();

        Vector3f kDir = new Vector3f();
        kDir.Sub( kPos1, kPos0 );
        float fLength = kDir.Normalize();
        Segment3f kSegment = new Segment3f(kPos0, kDir, fLength);
        DistanceVector3Segment3 kDist = new DistanceVector3Segment3( kVOIPoint, kSegment );
        float fDist = kDist.Get();
        if ( fDist < 3 )
        {
            setNearPoint(size() - 1);
            return true;
        }
        return false;
    }

    public boolean nearPoint( int iX, int iY, int iZ) {

        Vector3f kVOIPoint = new Vector3f(iX, iY, iZ );
        for ( int i = 0; i < m_kPoints.size(); i++ )
        {
            Vector3f kPos = m_kPoints.get(i).fileToScreen();
            Vector3f kDiff = new Vector3f();
            kDiff.Sub( kPos, kVOIPoint );
            if ( (Math.abs( kDiff.X ) < 3) &&  (Math.abs( kDiff.Y ) < 3) && (Math.abs( kDiff.Z ) < 3) )
            {
                setNearPoint(i);
                return true;
            }
        }
        return false;
    }


    public void setActive(boolean active) {
        this.active = active;
        for ( int i = 0; i < m_kPoints.size(); i++ )
        {
            m_kPoints.get(i).setActive(active);
        }
        if ( active )
        {
            VOI kGroup = getGroup();
            if ( (kGroup.getCurves()[0].size() > 1) && (kGroup.getCurves()[0].indexOf(this) != 0) )
            {
                kGroup.getCurves()[0].remove(this);
                kGroup.getCurves()[0].add(0, this);
            }
        }
    }

    public void setPosition( VOIManager parent, int iPos, float fX, float fY, float fZ )
    {
        //super.setPosition(parent,iPos,fX,fY,fZ);
        if ( iPos < m_kPoints.size() )
        {            
            m_kPoints.get(iPos).setPosition(parent,0,fX,fY,fZ);
            set( iPos, m_kPoints.get(iPos).get(0) );
        }
        lastPoint = iPos;

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }


    public void setPosition( VOIManager parent, int iPos, Vector3f kPos )
    {
        //super.setPosition(parent, iPos, kPos);
        if ( iPos < m_kPoints.size() )
        {            
            m_kPoints.get(iPos).setPosition(parent, 0, kPos);
            set( iPos, m_kPoints.get(iPos).get(0) );
        }
        lastPoint = iPos;

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }


    public LocalVolumeVOI split ( Vector3f kStartPt, Vector3f kEndPt )
    {
        return null;
    }


    protected void drawVOI( Renderer kRenderer, int iSlice, float[] afResolutions, int[] aiUnits, VolumeVOI kVolumeVOI, Vector3f kVolumeScale, Vector3f kTranslate, int iOrientation, int[] aiAxisOrder )
    {             
        String totalDistance = getTotalLengthString(afResolutions, aiUnits);
        String dist = new String();
        for ( int i = 0; i < m_kPoints.size(); i++ )
        {
            dist = getLengthString( i, i+1, afResolutions, aiUnits );
            m_kPoints.get(i).setFirstPoint( i==0, i==lastPoint, totalDistance, dist, i+1);
            m_kPoints.get(i).drawVOI(kRenderer, iSlice, afResolutions, aiUnits, kVolumeVOI, kVolumeScale, kTranslate, iOrientation, aiAxisOrder);
        }
    }




}
