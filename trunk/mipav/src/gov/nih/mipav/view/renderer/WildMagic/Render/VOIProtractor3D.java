package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManager;

import java.awt.Color;
import java.awt.Graphics;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.Renderer;

public class VOIProtractor3D extends LocalVolumeVOI
{
    /**  */
    private static final long serialVersionUID = 4571749270028964479L;

    public VOIProtractor3D( VOIManager parent, ScreenCoordinateListener kContext, int iOrientation, int iType, int iSType, Vector<Vector3f> kLocal, int iZ )
    {
        super(parent, kContext, iOrientation, iType, iSType, kLocal, iZ );
        m_iVOIType = VOI.PROTRACTOR_3D;
        m_bClosed = false;
    }

    public VOIProtractor3D(VOIManager parent, ScreenCoordinateListener kContext, int iOrientation, int iType, Vector<Vector3f> kLocal, boolean bIsFile)
    {
        super(parent,kContext,iOrientation,iType, -1,kLocal,bIsFile);
        m_iVOIType = VOI.PROTRACTOR_3D;
        m_bClosed = false;
    }

    public VOIProtractor3D( VOIProtractor3D kVOI )
    {
        super(kVOI);
    }
    
    public VOIProtractor3D( VOIProtractor3D kVOI, int iZ )
    {
        super(kVOI, iZ);
    }
    
    public boolean add( int iPos, Vector3f kNewPoint, boolean bIsFile  ) 
    {
        return false;
    }

    public boolean add( Vector3f kNewPoint, boolean bIsFile) 
    {
        return false;
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

        // if the point is not inside the protractor, then check and
        // see if it's near the line (because, initially, the protractor is
        // drawn with angle = 0.
        if (isInside == true) {
            return isInside;
        }

        return nearLine( iX, iY, iZ );
    }

    public void drawSelf( float[] resols, int[] unitsOfMeasure, Graphics g, int slice, int orientation  ) {
        if (g == null) {
            MipavUtil.displayError("VOIProtractor.drawSelf: grapics = null");

            return;
        }

/*
        if ( getGroup() != null )
        {
            g.setColor( getGroup().getColor() );
        }
        else
        {
            g.setColor( Color.yellow );
        }     
        */
        
        Vector3f kStart = m_kDrawingContext.fileToScreen( get(0) );
        Vector3f kMiddle = m_kDrawingContext.fileToScreen( get(1) );
        Vector3f kEnd = m_kDrawingContext.fileToScreen( get(2) );
        //0 is middle, 1 is start, 2 is end:
        float[] x = new float[3];
        x[0] = kMiddle.X;
        x[1] = kStart.X;
        x[2] = kEnd.X;

        //0 is middle, 1 is start, 2 is end:
        float[] y = new float[3];
        y[0] = kMiddle.Y;
        y[1] = kStart.Y;
        y[2] = kEnd.Y;

        if (Math.abs(y[1] - y[0]) > Math.abs(x[1] - x[0])) {
            g.drawLine((int) (x[0] - 1), (int) y[0], (int) (x[1] - 1), (int) y[1]);
            g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
            g.drawLine((int) (x[0] + 1), (int) y[0], (int) (x[1] + 1), (int) y[1]);
        } else {
            g.drawLine((int) x[0], (int) (y[0] - 1), (int) x[1], (int) (y[1] - 1));
            g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
            g.drawLine((int) x[0], (int) (y[0] + 1), (int) x[1], (int) (y[1] + 1));
        }

        g.drawLine((int) x[0], (int) y[0], (int) x[2], (int) y[2]);

        if (active == true) {
            this.drawTickMarks(g, unitsOfMeasure, m_kDrawingContext.getWidth(), m_kDrawingContext.getHeight(), resols);
        } 
    }



    public void drawTickMarks(Graphics g, int[] unitsOfMeasure, int xD, int yD, float[] res)
    {
        int i;
        double slope;
        boolean close;
        float[] x2 = new float[2];
        float[] y2 = new float[2];

        if (g == null) {
            MipavUtil.displayError("VOIprotractor drawTickMarks: graphics = null");

            return;
        }
        
        Color currentColor = g.getColor();

        g.setPaintMode();
        g.setFont(MipavUtil.font12);
        
        Vector3f kStart = m_kDrawingContext.fileToScreen( get(0) );
        Vector3f kMiddle = m_kDrawingContext.fileToScreen( get(1) );
        Vector3f kEnd = m_kDrawingContext.fileToScreen( get(2) );
        

        if ( kStart.equals( kEnd ) )
        {
            return;
        }
        
        //0 is middle, 1 is start, 2 is end:
        float[] x = new float[3];
        x[0] = kMiddle.X;
        x[1] = kStart.X;
        x[2] = kEnd.X;

        //0 is middle, 1 is start, 2 is end:
        float[] y = new float[3];
        y[0] = kMiddle.Y;
        y[1] = kStart.Y;
        y[2] = kEnd.Y;

        if ((x[1] - x[0]) != 0) {
            slope = (y[1] - y[0]) / (x[1] - x[0]);
        } else {
            slope = Double.MAX_VALUE;
        }

        close = (((y[0] <= (yD / 2)) && (slope < 1) && (slope > -1)) || (x[0] >= (xD / 2)));
        float[] coords = new float[4];
        getCoords(x, y, .5, coords); // get coordinates for tick marks

        String degreeString = getAngleString( res );

        if (close == true) {

            if ((yD - y[0]) < 20) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 21);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 19);
                g.drawString(degreeString, (int) coords[0] - 21, (int) coords[1] - 20);
                g.drawString(degreeString, (int) coords[0] - 19, (int) coords[1] - 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 20);
            } else if ((xD - x[0]) < 20) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] - 51, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] - 49, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] - 19, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] - 21, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 20);
            }
        } else {

            if ((slope > 0) || (slope < -.5)) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] + 21, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] + 19, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 21);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 19);
                g.drawString(degreeString, (int) coords[0] - 41, (int) coords[1] - 20);
                g.drawString(degreeString, (int) coords[0] - 39, (int) coords[1] - 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 20);
            }
        }

        g.setColor( currentColor );

        for (i = 0; i < 2; i++) {
            getEndLines(x, y, i, coords);
            g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        }

        if ((x[2] - x[0]) != 0) {
            slope = (y[2] - y[0]) / (x[2] - x[0]);
        } else {
            slope = Double.MAX_VALUE;
        }

        close = (((y[2] <= (yD / 2)) && (slope < 1) && (slope > -1)) || (x[2] >= (xD / 2)));
        x2[0] = x[0];
        x2[1] = x[2];
        y2[0] = y[0];
        y2[1] = y[2];
        getCoords(x2, y2, .5, coords); // get coordinates for tick marks

        boolean showLengths = true;
        if (showLengths) {

            String lengthString = getLengthString( res, unitsOfMeasure );
            if (close == true) {

                if ((yD - y2[1]) < 20) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 21);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 19);
                    g.drawString(lengthString, (int) coords[0] - 21, (int) coords[1] - 20);
                    g.drawString(lengthString, (int) coords[0] - 19, (int) coords[1] - 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 20);
                } else if ((xD - x2[1]) < 20) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] - 51, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] - 49, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 20);
                } else {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] - 19, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] - 21, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 20);
                }
            } else {

                if ((slope > 0) || (slope < -.5)) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] + 21, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] + 19, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 20);
                } else {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 21);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 19);
                    g.drawString(lengthString, (int) coords[0] - 41, (int) coords[1] - 20);
                    g.drawString(lengthString, (int) coords[0] - 39, (int) coords[1] - 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 20);
                }
            }
        } // end of if (showLengths)

        g.setColor( currentColor );
        
        for (i = 0; i < 2; i++) {
            getEndLines(x2, y2, i, coords);
            g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        }
    }

    public LocalVolumeVOI split ( Vector3f kStartPt, Vector3f kEndPt )
    {
        return null;
    }

    protected void drawVOI( Renderer kRenderer, int iSlice, float[] afResolutions, int[] aiUnits, VolumeVOI kVolumeVOI, Vector3f kVolumeScale, Vector3f kTranslate, int iOrientation, int[] aiAxisOrder )
    {             
        if ( isActive() )
        {
            if ( iSlice == slice() )
            {

                getLocalCenter();

                drawVOIProtractorAngle( kRenderer, afResolutions );
                drawVOIProtractorLength( kRenderer, afResolutions, aiUnits );
                drawSelectedPoints( kRenderer, kVolumeScale, kTranslate, iOrientation, aiAxisOrder );
            }
        }
    }


    private void drawVOIProtractorAngle( Renderer kRenderer, float[] afResolutions )
    {
        Vector3f kStart = m_kParent.fileCoordinatesToPatient( get(1) );
        Vector3f kEnd1 = m_kParent.fileCoordinatesToPatient( get(0) );
        Vector3f kEnd2 = m_kParent.fileCoordinatesToPatient( get(2));
        float[] x = new float[3];
        x[0] = kStart.X;
        x[1] = kEnd1.X;
        x[2] = kEnd2.X;

        float[] y = new float[3];
        y[0] = kStart.Y;
        y[1] = kEnd1.Y;
        y[2] = kEnd2.Y;

        float slope;
        if ((x[1] - x[0]) != 0) {
            slope = (y[1] - y[0]) / (x[1] - x[0]);
        } else {
            slope = Float.MAX_VALUE;
        }
        double fAngle = MipavMath.angle(x, y, afResolutions );



        int iWidth = kRenderer.GetWidth();
        int iHeight = kRenderer.GetHeight();
        boolean close = (((y[0] <= (iHeight / 2)) && (slope < 1) && (slope > -1)) || (x[0] >= (iWidth / 2)));

        Vector3f kCenter = m_kDrawingContext.fileToScreen( get(1) );
        kCenter.Add( m_kDrawingContext.fileToScreen( get(0) ) );
        kCenter.Scale(0.5f);
        int stringX = (int) kCenter.X;
        int stringY = (int) kCenter.Y;
        String degreeString = String.valueOf(fAngle); // since y decreases going down
        int i = degreeString.indexOf('.');

        if (degreeString.length() >= (i + 3)) {
            degreeString = degreeString.substring(0, i + 3);
        }

        degreeString += " deg";

        if (close == true) {

            if ((iHeight - y[0]) < 20) {
                drawText( kRenderer, stringX - 20, stringY - 20, ColorRGBA.WHITE, degreeString.toCharArray() );
            } else if ((iWidth - x[0]) < 20) {
                drawText( kRenderer, stringX - 50, stringY + 20, ColorRGBA.WHITE, degreeString.toCharArray() );
            } else {
                drawText( kRenderer, stringX - 20, stringY + 20, ColorRGBA.WHITE, degreeString.toCharArray() );
            }
        } else {

            if ((slope > 0) || (slope < -.5)) {
                drawText( kRenderer, stringX + 20, stringY + 20, ColorRGBA.WHITE, degreeString.toCharArray() );
            } else {
                drawText( kRenderer, stringX - 40, stringY - 20, ColorRGBA.WHITE, degreeString.toCharArray() );
            }
        }
    }


    private void drawVOIProtractorLength( Renderer kRenderer, float[] afResolutions, int[] aiUnits )
    {
        Vector3f kStart = m_kParent.fileCoordinatesToPatient( get(1) );
        Vector3f kEnd = m_kParent.fileCoordinatesToPatient( get(2) );
        float[] x = new float[2];
        x[0] = kStart.X;
        x[1] = kEnd.X;

        float[] y = new float[2];
        y[0] = kStart.Y;
        y[1] = kEnd.Y;

        double length = MipavMath.length(x, y, afResolutions );

        float slope;
        if ((x[1] - x[0]) != 0) {
            slope = (y[1] - y[0]) / (x[1] - x[0]);
        } else {
            slope = Float.MAX_VALUE;
        }


        int iWidth = kRenderer.GetWidth();
        int iHeight = kRenderer.GetHeight();
        boolean close = (((y[0] <= (iHeight / 2)) && (slope < 1) && (slope > -1)) || (x[0] >= (iWidth / 2)));

        // g.setColor(Color.yellow);
        String tmpString = String.valueOf(length);
        int i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        tmpString = tmpString + " " + FileInfoBase.getUnitsOfMeasureAbbrevStr(aiUnits[0]);

        Vector3f kCenter = m_kDrawingContext.fileToScreen( get(1) );
        kCenter.Add( m_kDrawingContext.fileToScreen( get(2) ) );
        kCenter.Scale(0.5f);
        int stringX = (int) kCenter.X;
        int stringY = (int) kCenter.Y;

        if (close == true) {

            if ((iHeight - y[0]) < 20) {

                if ((stringY - 21) < 20) {
                    stringY += 45;
                }

                if ((stringX - 21) < 10) {
                    stringX += 25;
                }

                drawText( kRenderer, stringX - 20, stringY - 20, ColorRGBA.WHITE, tmpString.toCharArray() );
            } else if ((iWidth - x[0]) < 20) {
                drawText( kRenderer, stringX - 50, stringY + 20, ColorRGBA.WHITE, tmpString.toCharArray() );
            } else {
                drawText( kRenderer, stringX - 20, stringY + 20, ColorRGBA.WHITE, tmpString.toCharArray() );
            }
        } else {

            if ((slope > 0) || (slope < -.5)) {
                drawText( kRenderer, stringX + 20, stringY + 20, ColorRGBA.WHITE, tmpString.toCharArray() );
            } else {
                drawText( kRenderer, stringX - 40, stringY - 20, ColorRGBA.WHITE, tmpString.toCharArray() );
            }
        }
    }


    private String getAngleString( float[] afResolutions )
    {
        Vector3f kStart = m_kParent.fileCoordinatesToPatient( get(1) );
        Vector3f kEnd1 = m_kParent.fileCoordinatesToPatient( get(0) );
        Vector3f kEnd2 = m_kParent.fileCoordinatesToPatient( get(2));
        float[] x = new float[3];
        x[0] = kStart.X;
        x[1] = kEnd1.X;
        x[2] = kEnd2.X;

        float[] y = new float[3];
        y[0] = kStart.Y;
        y[1] = kEnd1.Y;
        y[2] = kEnd2.Y;

        if ((x[1] - x[0]) != 0) {
        } else {
        }
        double fAngle = MipavMath.angle(x, y, afResolutions );

        String degreeString = String.valueOf(fAngle); // since y decreases going down
        int i = degreeString.indexOf('.');

        if (degreeString.length() >= (i + 3)) {
            degreeString = degreeString.substring(0, i + 3);
        }

        degreeString += " deg";
        return degreeString;
    }

    private void getCoords(float[] x, float[] y, double fraction, float[] coords) {
        float x1, y1;
        double vector1, vector2, tmp;
        double length;
        x1 = (x[0] + x[1]) / 2;
        y1 = (y[0] + y[1]) / 2;

        if (fraction == .25) {
            x1 = (x[0] + x1) / 2;
            y1 = (y[0] + y1) / 2;
        } else if (fraction == .75) {
            x1 = (x1 + x[1]) / 2;
            y1 = (y1 + y[1]) / 2;
        }

        vector1 = (x[1] - x[0]);
        vector2 = (y[1] - y[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (x[1] - x[0]) / length;
        vector2 = (y[1] - y[0]) / length;
        tmp = vector1;
        vector1 = 5 * (-vector2);
        vector2 = 5 * tmp;
        coords[0] = (int) (x1 + vector1 + 0.5);
        coords[1] = (int) (y1 + vector2 + 0.5);
        coords[2] = (int) (x1 - vector1 + 0.5);
        coords[3] = (int) (y1 - vector2 + 0.5);
    }

    private void getEndLines(float[] x, float[] y, int line, float[] coords) {
        double vector1, vector2, tmp;
        double length;
        vector1 = (x[1] - x[0]);
        vector2 = (y[1] - y[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (x[1] - x[0]) / length;
        vector2 = (y[1] - y[0]) / length;
        tmp = vector1;
        vector1 = -10 * ((vector1 * 0.707) + (vector2 * 0.707));
        vector2 = 10 * ((tmp * 0.707) - (vector2 * 0.707));

        if (line == 0) {
            coords[0] = (int) (x[1]);
            coords[1] = (int) (y[1]);
            coords[2] = (int) (x[1] + vector1 + 0.5);
            coords[3] = (int) (y[1] + vector2 + 0.5);
        } else if (line == 1) {
            coords[0] = (int) (x[1]);
            coords[1] = (int) (y[1]);
            coords[2] = (int) (x[1] - vector2 + 0.5);
            coords[3] = (int) (y[1] + vector1 + 0.5);
        }
    }

    private String getLengthString(float[] afResolutions, int[] aiUnits)
    {
        Vector3f kStart = m_kParent.fileCoordinatesToPatient( get(1) );
        Vector3f kEnd = m_kParent.fileCoordinatesToPatient( get(2) );
        float[] x = new float[2];
        x[0] = kStart.X;
        x[1] = kEnd.X;

        float[] y = new float[2];
        y[0] = kStart.Y;
        y[1] = kEnd.Y;

        double length = MipavMath.length(x, y, afResolutions );

        String tmpString = String.valueOf(length);
        int i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        tmpString = tmpString + " " + FileInfoBase.getUnitsOfMeasureAbbrevStr(aiUnits[0]);
        return tmpString;
    }

    public VOIProtractor3D clone() {
        return new VOIProtractor3D(this);
    }

    public VOIProtractor3D clone(int iZ) {
        return new VOIProtractor3D(this, iZ);
    }
}
