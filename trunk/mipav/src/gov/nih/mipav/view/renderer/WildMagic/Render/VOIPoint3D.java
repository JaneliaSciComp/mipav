package gov.nih.mipav.view.renderer.WildMagic.Render;

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
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Shaders.Program;

public class VOIPoint3D extends LocalVolumeVOI
{
    /**  */
    private static final long serialVersionUID = -3882960995263308396L;

    /** True if this point is the 1st point in a slice/frame of Polyline_Slice*/
    private boolean firstSlicePoint = false;


    /** Distance is calculated and passed in by VOI for polyline_slices */
    private String totalDistanceString = null;

    /** Distance between segments shown only for active point in polyline_slices*/
    private String distanceString = null;

    /** True only for the active point of a polyline_slice structure (display's coordinates)*/
    private boolean isActivePoint = false;
    
    private int m_iID = -1;


    public VOIPoint3D( VOIManager parent, ScreenCoordinateListener kContext, int iOrientation, int iType, int iSType, Vector<Vector3f> kLocal, int iZ )
    {
        super(parent, kContext, iOrientation, iType, iSType, kLocal, iZ );
        m_iVOIType = VOI.POINT;
        if ( iType == VOIManager.POLYPOINT )
        {
            m_iVOIType = VOI.POLYLINE_SLICE;
        }
        m_bClosed = false;
    }
    
    public VOIPoint3D(VOIManager parent, ScreenCoordinateListener kContext, int iOrientation, int iType, Vector<Vector3f> kLocal, boolean bIsFile)
    {
        super(parent,kContext,iOrientation,iType,kLocal,bIsFile);
        m_iVOIType = VOI.POINT;
        if ( iType == VOIManager.POLYPOINT )
        {
            m_iVOIType = VOI.POLYLINE_SLICE;
        }
        m_bClosed = false;
    }
    
    public void add( VOIManager parent, int iPos, Vector3f kNewPoint, boolean bIsFile  ) {}


    public void add(VOIManager parent, Vector3f kNewPoint, boolean bIsFile) {}

    public VOIPoint3D Clone( )
    {
        return new VOIPoint3D( m_kParent, m_kDrawingContext, m_iOrientation, m_iVOIType, this, true );
    }
    public VOIPoint3D Clone( int iZ )
    {
        return new VOIPoint3D( m_kParent, m_kDrawingContext, m_iOrientation, m_iVOIType, m_iVOISpecialType, this, iZ );
    }

    public boolean contains(int iOrientation, int iX, int iY, int iZ ) {
        if ( iZ != slice() || iOrientation != m_iOrientation )
        {
            return false;
        }
        return nearPoint( iX, iY, iZ );
    }

    public void drawSelf( float[] resols, int[] unitsOfMeasure, Graphics g, int slice, int orientation  ) {
        if ( g == null ) {
            MipavUtil.displayError( "VOIPoint.drawSelf: grapics = null" );
            return;
        }

        int iContourID = getContourID();
        if ( iContourID != -1 )
        {
            iContourID++;
            label = String.valueOf(iContourID);
        }
        drawSelf(g, label );
    }


    public void drawSelf( Graphics g, String label ) {


        if ( getGroup() != null )
        {
            g.setColor( getGroup().getColor() );
        }
        else
        {
            g.setColor( Color.yellow );
        }

        boolean doName = (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && name != null);

        Vector3f kFile = get(0);
        Vector3f kScreen = m_kDrawingContext.fileToScreen( kFile );
        int x = Math.round( kFile.X );
        int y = Math.round( kFile.Y );
        int xS = Math.round( kScreen.X );
        int yS = Math.round( kScreen.Y );

        String str;
        if ( m_iVOIType != VOI.POLYLINE_SLICE )
        {
            str = new String("(" + x + "," + y + ")");
            //Create the string that will be drawn for label and name
            if (doName) {
                str = new String(name + ": (" + x + "," + y + ")");
            } else if (label != null) {
                str = new String(label + ": (" + x + "," + y + ")");
            }

            int type = 0;
            String typeStr = Preferences.getProperty(Preferences.PREF_VOI_POINT_DRAW_TYPE);
            if (typeStr != null) {
                try {
                    type = Integer.parseInt(typeStr);
                    if (type < 0 || type > 3) {
                        type = 0;
                    }
                } catch (Exception ex) {}
            }

            switch (type) {
            case 0:
                g.drawLine(xS, yS - 4, xS, yS + 4);
                g.drawLine(xS - 4, yS, xS + 4, yS);
                break;
            case 1:
                g.drawLine(xS, yS - 4, xS, yS - 1);
                g.drawLine(xS, yS + 1, xS, yS + 4);
                g.drawLine(xS - 4, yS, xS - 1, yS);
                g.drawLine(xS + 1, yS, xS + 4, yS);
                break;
            case 2:
                g.drawLine(xS - 4, yS - 4, xS + 4, yS + 4);
                g.drawLine(xS - 4, yS + 4, xS + 4, yS - 4);
                break;
            case 3:
                g.drawLine(xS - 4, yS - 4, xS - 1, yS - 1);
                g.drawLine(xS + 1, yS + 1, xS + 4, yS + 4);
                g.drawLine(xS - 4, yS + 4, xS - 1, yS + 1);
                g.drawLine(xS + 1, yS - 1, xS + 4, yS - 4);
                break;
            }

            if (active == false) {
                if (doName) {
                    if (xS < 20) {
                        g.drawString(name, xS + 10, yS);
                    } else {
                        g.drawString(name, xS - 15, yS - 5);
                    }
                } else {
                    if (xS < 20) {
                        g.drawString(label, xS + 10, yS);
                    } else {
                        g.drawString(label, xS - 15, yS - 5);
                    }
                }
            } else {

                if (type != 1 && type != 3) {
                    g.setColor(Color.black);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                }

                g.setFont(MipavUtil.font12);
                g.setColor(Color.yellow);

                int xPos = xS;
                int yPos = yS;
                if (xS < 70) {
                    xPos += 10;
                } else {
                    xPos -= 60;
                }
                if (yS < 30) {
                    yPos += 20;
                } else {
                    yPos -= 10;
                }
                g.drawString(str, xPos, yPos);
            }
        }
        else
        {
            if (isActivePoint) {
                str = new String(label + ": (" + x + "," + y + ")");
            } else {
                str = new String(label);
            }

            if (active) {
                if (isActivePoint) {
                    g.setColor(Color.GREEN);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                } else {
                    g.setColor(Color.black);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                }
            } else {
                g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                g.setColor(Color.white);
                g.fillRect((xS), (yS), 1, 1);
            }

            if (active) {
                g.setFont(MipavUtil.font12);
                g.setColor(Color.yellow);

                int xPos = xS;
                int yPos = yS;

                boolean displaySegmentDistance = false;
                if (distanceString != null) {
                    displaySegmentDistance = true;
                    displaySegmentDistance = !(distanceString.startsWith("0.00"));
                }

                if (firstSlicePoint && isActivePoint) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                        g.drawString("total: " + totalDistanceString, xPos + 10, yPos - 18);
                        if (displaySegmentDistance)
                            g.drawString("segment: " + distanceString, xPos + 10, yPos - 31);


                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        g.drawString("total: " + totalDistanceString, xPos - 15, yPos - 18);
                        if (displaySegmentDistance)
                            g.drawString("segment: " + distanceString, xPos - 15, yPos - 31);

                    }
                } else if (firstSlicePoint) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                        g.drawString("total: " + totalDistanceString, xPos + 10, yPos - 18);
                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        g.drawString("total: " + totalDistanceString, xPos - 15, yPos - 18);
                    }
                } else if (isActivePoint) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);

                        if (displaySegmentDistance)
                            g.drawString("segment: " + distanceString, xPos + 10, yPos - 18);
                    }
                    else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        if (displaySegmentDistance)
                            g.drawString("segment: " + distanceString, xPos - 15, yPos - 18);
                    }
                } else {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                    }
                }
            } else {
                g.setFont(MipavUtil.font12);
                if (xS < 20) {
                    g.drawString(label, xS + 10, yS);
                } else {
                    g.drawString(label, xS - 15, yS - 5);
                }

            }

        }
    }


    public Vector3f fileToScreen()
    {
        return m_kDrawingContext.fileToScreen( get(0) );
    }

    public int getType()
    {
        return m_iVOIType;
    }

    public boolean nearLine(int iX, int iY, int iZ )
    {
        return false;
    } 



    /**
     * Tells the drawSelf how to draw each point
     * @param isFirst boolean is this the very first point of the polyline slice structure
     * @param isActPt boolean is this the active point (for drawing as green and showing segment length)
     * @param totalDistance String the total distance of the polyline slice
     * @param dist String the segment (active point to next point) distance of the polyline slice
     */
    public void setFirstPoint(boolean isFirst, boolean isActPt, String totalDistance, String dist, int iID) {
        this.firstSlicePoint = isFirst;
        this.isActivePoint = isActPt;

        this.totalDistanceString = totalDistance;
        this.distanceString = dist;
        
        m_iID = iID;
    }


    public LocalVolumeVOI split ( Vector3f kStartPt, Vector3f kEndPt )
    {
        return null;
    }

    protected void drawSelectedPoints( Renderer kRenderer, Vector3f kVolumeScale, Vector3f kTranslate,
            int iOrientation, int[] aiAxisOrder, int iID )
    {
        if ( m_kBallPoint == null && (iOrientation == m_iOrientation))
        {
            createSelectedIcon(aiAxisOrder);
        }
        Vector3f kLocalTranslate = new Vector3f();
        for ( int j = 0; j < size(); j++ )
        {
            kLocalTranslate.Copy( get(j) );
            kLocalTranslate.Mult( kVolumeScale );
            kLocalTranslate.Add( kTranslate );
            m_kBallPoint.Local.SetTranslate( kLocalTranslate );
            m_kBallPoint.UpdateGS();

            Program kProgram = ((ShaderEffect)m_kBallPoint.GetEffect(0)).GetCProgram(0);
            if ( kProgram != null )
            {
                if ( kProgram.GetUC("ConstantColor") != null )
                {
                    if ( isActivePoint && active )
                    {
                        kProgram.GetUC("ConstantColor").GetData()[0] = 0;
                        kProgram.GetUC("ConstantColor").GetData()[1] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[2] = 0;
                    }
                    else if ( iID == 1 && active )
                    {
                        kProgram.GetUC("ConstantColor").GetData()[0] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[1] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[2] = 0;                                
                    }
                    else if ( active )
                    {
                        kProgram.GetUC("ConstantColor").GetData()[0] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[1] = 1;
                        kProgram.GetUC("ConstantColor").GetData()[2] = 1;
                    }
                    else 
                    {
                        if ( getGroup() != null )
                        {
                            kProgram.GetUC("ConstantColor").GetData()[0] = getGroup().getColor().getRed()/255f;
                            kProgram.GetUC("ConstantColor").GetData()[1] = getGroup().getColor().getGreen()/255f;
                            kProgram.GetUC("ConstantColor").GetData()[2] = getGroup().getColor().getBlue()/255f;
                        }
                        else
                        {
                            kProgram.GetUC("ConstantColor").GetData()[0] = 1;
                            kProgram.GetUC("ConstantColor").GetData()[1] = 1;
                            kProgram.GetUC("ConstantColor").GetData()[2] = 0;           
                        }
                    }
                }

                if ( kProgram.GetUC("UseConstantColor") != null )
                {
                    kProgram.GetUC("UseConstantColor").GetData()[0] = 1.0f;
                }
            }
            kRenderer.Draw( m_kBallPoint );
        }
    }


    protected void drawVOI( Renderer kRenderer, int iSlice, float[] afResolutions, int[] aiUnits, VolumeVOI kVolumeVOI, Vector3f kVolumeScale, Vector3f kTranslate, int iOrientation, int[] aiAxisOrder )
    {             
        int iNumPoints = GetVertexQuantity();
        if ( iNumPoints > 0 )
        {
            if ( iSlice == slice() )
            {
                drawVOIPoint( kRenderer, kVolumeVOI );
                if ( !isActive() && (m_iVOIType != VOI.POLYLINE_SLICE))
                {
                    return;
                }
                if ( m_iVOIType != VOI.POLYLINE_SLICE )
                {
                    drawSelectedPoints( kRenderer, kVolumeScale, kTranslate, iOrientation, aiAxisOrder );
                }
                else
                {
                    drawSelectedPoints( kRenderer, kVolumeScale, kTranslate, iOrientation, aiAxisOrder, m_iID );
                }
            }
        }
    }
    
    private void drawVOIPoint( Renderer kRenderer, VolumeVOI kVolumeVOI )
    {
        Vector3f kScreen = m_kDrawingContext.fileToScreen( get(0) );
        Vector3f kStart = m_kParent.fileCoordinatesToPatient( get(0) );
        int xPos = (int)kScreen.X;
        int yPos = (int)kScreen.Y;

        String kLabel = new String( "1" );
        int iContourID = getContourID();
        if ( iContourID != -1 )
        {
            iContourID++;
            kLabel = String.valueOf(iContourID);
        }
        else if ( m_iID != -1 )
        {
            kLabel = String.valueOf(m_iID);
        }
        if ( m_iVOIType != VOI.POLYLINE_SLICE )
        {
            if ( isActive() )
            {
                kLabel = kLabel + ":(" + (int)kStart.X + "," + (int)kStart.Y + ")"; 
            }


            if ( !isActive() )
            {
                if (kScreen.X < 20) {
                    drawText( kRenderer, (int)(kScreen.X + 10), (int)kScreen.Y, kVolumeVOI.getColor(), kLabel.toCharArray() );
                } else {
                    drawText( kRenderer, (int)(kScreen.X - 15), (int)(kScreen.Y - 5), kVolumeVOI.getColor(), kLabel.toCharArray() );
                }
            }
            else
            {
                if (kScreen.X < 70) {
                    xPos += 10;
                } else {
                    xPos -= 60;
                }

                if (kScreen.Y < 30) {
                    yPos += 20;
                } else {
                    yPos -= 10;
                }

                drawText( kRenderer, xPos, yPos, new ColorRGBA(1,1,0,1), kLabel.toCharArray() );
            }
        }
        else
        {
            if (active) {
                ColorRGBA kColor = new ColorRGBA( 1, 1, 0, 1 );
                boolean displaySegmentDistance = false;
                if (distanceString != null) {
                    displaySegmentDistance = true;
                    displaySegmentDistance = !(distanceString.startsWith("0.00"));
                }

                if (firstSlicePoint && isActivePoint) {
                    if (xPos < 20) {
                        drawText( kRenderer, xPos + 10, yPos - 5, kColor, kLabel.toCharArray() );
                        drawText( kRenderer, xPos + 10, yPos - 18, kColor, new String("total: " + totalDistanceString).toCharArray() );
                        if (displaySegmentDistance)
                        {
                            drawText( kRenderer, xPos + 10, yPos - 31, kColor, new String("segment: " + distanceString).toCharArray() );
                        }
                    } else {
                        drawText( kRenderer, xPos - 15, yPos - 5, kColor, kLabel.toCharArray() );
                        drawText( kRenderer, xPos - 15, yPos - 18, kColor, new String("total: " + totalDistanceString).toCharArray() );
                        if (displaySegmentDistance)
                        {
                            drawText( kRenderer, xPos - 15, yPos - 31, kColor, new String("segment: " + distanceString).toCharArray() );
                        }
                    }
                } else if (firstSlicePoint) {
                    if (xPos < 20) {                        
                        drawText( kRenderer, xPos + 10, yPos - 5, kColor, kLabel.toCharArray() );
                        drawText( kRenderer, xPos + 10, yPos - 18, kColor, new String("total: " + totalDistanceString).toCharArray() );
                    } else {
                        drawText( kRenderer, xPos - 15, yPos - 5, kColor, kLabel.toCharArray() );
                        drawText( kRenderer, xPos - 15, yPos - 18, kColor, new String("total: " + totalDistanceString).toCharArray() );
                    }
                } else if (isActivePoint) {
                    if (xPos < 20) {
                        drawText( kRenderer, xPos + 10, yPos - 5, kColor, kLabel.toCharArray() );
                        if (displaySegmentDistance)
                        {
                            drawText( kRenderer, xPos + 10, yPos - 18, kColor, new String("segment: " + distanceString).toCharArray() );
                        }
                    }
                    else {
                        drawText( kRenderer, xPos - 15, yPos - 5, kColor, kLabel.toCharArray() );
                        if (displaySegmentDistance)
                        {
                            drawText( kRenderer, xPos - 15, yPos - 18, kColor, new String("segment: " + distanceString).toCharArray() );
                        }
                    }
                } else {
                    if (xPos < 20) {
                        drawText( kRenderer, xPos + 10, yPos - 5, kColor, kLabel.toCharArray() );
                    } else {
                        drawText( kRenderer, xPos - 15, yPos - 5, kColor, kLabel.toCharArray() );
                    }
                }
            } else {
                if (xPos < 20) {
                    drawText( kRenderer, xPos + 10, yPos, kVolumeVOI.getColor(), kLabel.toCharArray() );
                } else {
                    drawText( kRenderer, xPos - 15, yPos - 5, kVolumeVOI.getColor(), kLabel.toCharArray() );
                }

            }
        }


    }


}
