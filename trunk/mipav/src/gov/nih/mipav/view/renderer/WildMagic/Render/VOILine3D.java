package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.renderer.WildMagic.VOI.ScreenCoordinateListener;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManager;

import java.awt.Color;
import java.awt.Graphics;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.Renderer;

public class VOILine3D extends LocalVolumeVOI
{
    /**  */
    private static final long serialVersionUID = 8548793315455034100L;

    public VOILine3D( VOIManager parent, ScreenCoordinateListener kContext, int iOrientation, int iType, int iSType, Vector<Vector3f> kLocal, int iZ )
    {
        super(parent, kContext, iOrientation, iType, iSType, kLocal, iZ );
        m_iVOIType = VOI.LINE_3D;
        m_bClosed = false;
    }

    public VOILine3D(VOIManager parent, ScreenCoordinateListener kContext, int iOrientation, int iType, Vector<Vector3f> kLocal, boolean bIsFile)
    {
        super(parent,kContext,iOrientation,iType,-1,kLocal,bIsFile);
        m_iVOIType = VOI.LINE_3D;
        m_bClosed = false;
    }
    
    public VOILine3D( VOILine3D kVOI )
    {
        super(kVOI);
    }
    
    public VOILine3D( VOILine3D kVOI, int iZ )
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

    public boolean contains(int iOrientation, int iX, int iY, int iZ ) {
        if ( iZ != slice() || iOrientation != m_iOrientation )
        {
            return false;
        }
        if ( nearLine( iX, iY, iZ ) )
        {
            return true;
        }
        return nearPoint( iX, iY, iZ );
    }

    public void drawSelf( float[] resols, int[] unitsOfMeasure, Graphics g, int slice, int orientation  ) {

        if (g == null) {
            MipavUtil.displayError("VOILine.drawSelf: graphics = null");

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
        Vector3f kEnd = m_kDrawingContext.fileToScreen( get(1) );
        float[] x = new float[2];
        x[0] = kStart.X;
        x[1] = kEnd.X;

        float[] y = new float[2];
        y[0] = kStart.Y;
        y[1] = kEnd.Y;


        MipavMath.length(x, y, resols );

        int thickness = 1;
        if (thickness == 1) {
            g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
        } else {

            int dX = (int) (x[1] - x[0]);
            int dY = (int) (y[1] - y[0]);
            // line length
            double lineLength = Math.sqrt(dX * dX + dY * dY);

            double scale = (thickness) / (2 * lineLength);

            // The x,y increments from an endpoint needed to create a rectangle...
            double ddx = -scale * dY;
            double ddy = scale * dX;
            ddx += (ddx > 0) ? 0.5 : -0.5;
            ddy += (ddy > 0) ? 0.5 : -0.5;
            int dx = (int)ddx;
            int dy = (int)ddy;

            // Now we can compute the corner points...
            int xPoints[] = new int[4];
            int yPoints[] = new int[4];

            xPoints[0] = (int)x[0] + dx; yPoints[0] = (int)y[0] + dy;
            xPoints[1] = (int)x[0] - dx; yPoints[1] = (int)y[0] - dy;
            xPoints[2] = (int)x[1] - dx; yPoints[2] = (int)y[1] - dy;
            xPoints[3] = (int)x[1] + dx; yPoints[3] = (int)y[1] + dy;

            g.fillPolygon(xPoints, yPoints, 4);



        }

        Color currentColor = g.getColor();

        if (active == true) {
            // draw the active point dragging is taking place
            if ((lastPoint >= 0) && (this.size() > lastPoint)) {
                g.setColor(Color.GREEN);
                g.fillRect((int) (x[lastPoint] - 1.5 + 0.5f), (int) (y[lastPoint] - 1.5 + 0.5f), 3, 3);
            }

            if ( getSType() != VOIManager.SPLITLINE )
            {
                drawTickMarks(g, currentColor, unitsOfMeasure, m_kDrawingContext.getWidth(), m_kDrawingContext.getHeight(), resols);
            }
        }
    }




    public void drawTickMarks(Graphics g, Color color, int[] unitsOfMeasure, int xD, int yD, float[] res) {

        if (g == null) {
            MipavUtil.displayError("VOILine drawTickMarks: grapics = null");

            return;
        }

        g.setFont(MipavUtil.font12);

        Vector3f kStart = m_kDrawingContext.fileToScreen( get(0) );
        Vector3f kEnd = m_kDrawingContext.fileToScreen( get(1) );
        if ( kStart.equals( kEnd ) )
        {
            return;
        }
        
        
        
        float[] x = new float[2];
        x[0] = kStart.X;
        x[1] = kEnd.X;

        float[] y = new float[2];
        y[0] = kStart.Y;
        y[1] = kEnd.Y;


        double length = MipavMath.length(x, y, res );

        float slope;
        if ((x[1] - x[0]) != 0) {
            slope = (y[1] - y[0]) / (x[1] - x[0]);
        } else {
            slope = Float.MAX_VALUE;
        }

        boolean close = (((y[0] <= (yD / 2)) && (slope < 1) && (slope > -1)) || (x[0] >= (xD / 2)));
        float[] coords = new float[4];
        getCoords(x, y, .5, coords); // get coordinates for tick marks

        // g.setColor(Color.yellow);
        String tmpString = String.valueOf(length);
        int i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        tmpString = tmpString + " " + FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]);
        int stringX = (int) coords[0];
        int stringY = (int) coords[1];
        boolean drawAngle = Preferences.is(Preferences.PREF_SHOW_LINE_ANGLE);

        double theta = 0;
        if ((x[1] > x[0]) && (y[0] > y[1])) {
            theta = 90.0 - ((180.0 / Math.PI) * Math.atan2((y[0] - y[1]), x[1] - x[0]));
        } else if ((x[1] > x[0]) && (y[1] > y[0])) {
            theta = -(90.0 + ((180.0 / Math.PI) * Math.atan2(y[0] - y[1], x[1] - x[0])));
        } else if ((x[0] > x[1]) && (y[0] > y[1])) {
            theta = -(90.0 - ((180.0 / Math.PI) * Math.atan2(y[0] - y[1], x[0] - x[1])));
        } else if ((x[0] > x[1]) && (y[1] > y[0])) {
            theta = 90.0 - ((180.0 / Math.PI) * Math.atan2(y[1] - y[0], x[0] - x[1]));
        } else if (x[0] == x[1]) {

            // zero angle
            theta = 0;
        } else if (y[0] == y[1]) {

            // 90deg angle
            theta = 90;
        }

        if (drawAngle) {
            String tmpString2 = String.valueOf(theta);
            i = tmpString2.indexOf('.');

            if (tmpString2.length() >= (i + 3)) {
                tmpString2 = tmpString2.substring(0, i + 3);
            }

            tmpString += ", " + tmpString2 + " deg";
        }

        if (close == true) {

            if ((yD - y[0]) < 20) {

                if ((stringY - 21) < 20) {
                    stringY += 45;
                }

                if ((stringX - 21) < 10) {
                    stringX += 25;
                }

                g.setColor(Color.black);
                g.drawString(tmpString, stringX - 20, stringY - 21);
                g.drawString(tmpString, stringX - 20, stringY - 19);
                g.drawString(tmpString, stringX - 21, stringY - 20);
                g.drawString(tmpString, stringX - 19, stringY - 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX - 20, stringY - 20);
            } else if ((xD - x[0]) < 20) {
                g.setColor(Color.black);
                g.drawString(tmpString, stringX - 50, stringY + 21);
                g.drawString(tmpString, stringX - 50, stringY + 19);
                g.drawString(tmpString, stringX - 51, stringY + 20);
                g.drawString(tmpString, stringX - 49, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX - 50, stringY + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(tmpString, stringX - 20, stringY + 21);
                g.drawString(tmpString, stringX - 20, stringY + 19);
                g.drawString(tmpString, stringX - 19, stringY + 20);
                g.drawString(tmpString, stringX - 21, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX - 20, stringY + 20);
            }
        } else {

            if ((slope > 0) || (slope < -.5)) {
                g.setColor(Color.black);
                g.drawString(tmpString, stringX + 20, stringY + 21);
                g.drawString(tmpString, stringX + 20, stringY + 19);
                g.drawString(tmpString, stringX + 21, stringY + 20);
                g.drawString(tmpString, stringX + 19, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX + 20, stringY + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(tmpString, stringX - 40, stringY - 21);
                g.drawString(tmpString, stringX - 40, stringY - 19);
                g.drawString(tmpString, stringX - 41, stringY - 20);
                g.drawString(tmpString, stringX - 39, stringY - 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX - 40, stringY - 20);
            }
        }

        g.setColor(color);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        getCoords(x, y, .25, coords);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        getCoords(x, y, .75, coords);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        g.setColor(color);

        for (i = 0; i < 4; i++) {
            getEndLines(x, y, i, coords);
            g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        }
    }

    public LocalVolumeVOI split ( Vector3f kStartPt, Vector3f kEndPt )
    {
        return null;
    }

    protected void drawVOI( Renderer kRenderer, int iSlice, float[] afResolutions, int[] aiUnits, VolumeVOI kVolumeVOI, Vector3f kVolumeScale, Vector3f kTranslate, int iOrientation, int[] aiAxisOrder )
    {             
        if ( isActive() && (getSType() != VOIManager.SPLITLINE))
        {
            if ( iSlice == slice() )
            {
                getLocalCenter();
                drawVOILine( kRenderer, afResolutions, aiUnits );
                drawSelectedPoints( kRenderer, kVolumeScale, kTranslate, iOrientation, aiAxisOrder );
            }
        }
    }

    private void drawVOILine( Renderer kRenderer, float[] afResolutions, int[] aiUnits )
    {
        Vector3f kStart = m_kParent.fileCoordinatesToPatient( get(0) );
        Vector3f kEnd = m_kParent.fileCoordinatesToPatient( get(1) );
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

        Vector3f kCenter = getLocalCenter();
        int stringX = (int) kCenter.X;
        int stringY = (int) kCenter.Y;
        boolean drawAngle = Preferences.is(Preferences.PREF_SHOW_LINE_ANGLE);
        double theta = 0;
        if ((x[1] > x[0]) && (y[0] > y[1])) {
            theta = 90.0 - ((180.0 / Math.PI) * Math.atan2((y[0] - y[1]), x[1] - x[0]));
        } else if ((x[1] > x[0]) && (y[1] > y[0])) {
            theta = -(90.0 + ((180.0 / Math.PI) * Math.atan2(y[0] - y[1], x[1] - x[0])));
        } else if ((x[0] > x[1]) && (y[0] > y[1])) {
            theta = -(90.0 - ((180.0 / Math.PI) * Math.atan2(y[0] - y[1], x[0] - x[1])));
        } else if ((x[0] > x[1]) && (y[1] > y[0])) {
            theta = 90.0 - ((180.0 / Math.PI) * Math.atan2(y[1] - y[0], x[0] - x[1]));
        } else if (x[0] == x[1]) {

            // zero angle
            theta = 0;
        } else if (y[0] == y[1]) {

            // 90deg angle
            theta = 90;
        }

        if (drawAngle) {
            String tmpString2 = String.valueOf(theta);
            i = tmpString2.indexOf('.');

            if (tmpString2.length() >= (i + 3)) {
                tmpString2 = tmpString2.substring(0, i + 3);
            }

            tmpString += ", " + tmpString2 + " deg";
        }

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

    private void getCoords(float[] linePtsX, float[] linePtsY, double fraction, float[] coords) {
        float x1, y1;
        double vector1, vector2, tmp;
        double length;
        x1 = (linePtsX[0] + linePtsX[1]) / 2;
        y1 = (linePtsY[0] + linePtsY[1]) / 2;

        if (fraction == .25) {
            x1 = (linePtsX[0] + x1) / 2;
            y1 = (linePtsY[0] + y1) / 2;
        } else if (fraction == .75) {
            x1 = (x1 + linePtsX[1]) / 2;
            y1 = (y1 + linePtsY[1]) / 2;
        }

        vector1 = (linePtsX[1] - linePtsX[0]);
        vector2 = (linePtsY[1] - linePtsY[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (linePtsX[1] - linePtsX[0]) / length;
        vector2 = (linePtsY[1] - linePtsY[0]) / length;
        tmp = vector1;
        vector1 = 5 * (-vector2);
        vector2 = 5 * tmp;
        coords[0] = (int) (x1 + vector1 + 0.5);
        coords[1] = (int) (y1 + vector2 + 0.5);
        coords[2] = (int) (x1 - vector1 + 0.5);
        coords[3] = (int) (y1 - vector2 + 0.5);
    }

    private void getEndLines(float[] linePtsX, float[] linePtsY, int line, float[] coords) {
        double vector1, vector2, tmp;
        double length;
        vector1 = (linePtsX[1] - linePtsX[0]);
        vector2 = (linePtsY[1] - linePtsY[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (linePtsX[1] - linePtsX[0]) / length;
        vector2 = (linePtsY[1] - linePtsY[0]) / length;
        tmp = vector1;
        vector1 = 10 * ((vector1 * 0.707) + (vector2 * 0.707));
        vector2 = 10 * ((-tmp * 0.707) + (vector2 * 0.707));

        if (line == 0) {
            coords[0] = (int) (linePtsX[1]);
            coords[1] = (int) (linePtsY[1]);
            coords[2] = (int) (linePtsX[1] + vector1 + 0.5);
            coords[3] = (int) (linePtsY[1] + vector2 + 0.5);
        } else if (line == 1) {
            coords[0] = (int) (linePtsX[1]);
            coords[1] = (int) (linePtsY[1]);
            coords[2] = (int) (linePtsX[1] - vector2 + 0.5);
            coords[3] = (int) (linePtsY[1] + vector1 + 0.5);
        } else if (line == 2) {
            coords[0] = (int) (linePtsX[0]);
            coords[1] = (int) (linePtsY[0]);
            coords[2] = (int) (linePtsX[0] - vector1 + 0.5);
            coords[3] = (int) (linePtsY[0] - vector2 + 0.5);
        } else if (line == 3) {
            coords[0] = (int) (linePtsX[0]);
            coords[1] = (int) (linePtsY[0]);
            coords[2] = (int) (linePtsX[0] + vector2 + 0.5);
            coords[3] = (int) (linePtsY[0] - vector1 + 0.5);
        }
    }
    


    public VOILine3D clone() {
        return new VOILine3D(this);
    }

    public VOILine3D clone(int iZ) {
        return new VOILine3D(this, iZ);
    }
}
