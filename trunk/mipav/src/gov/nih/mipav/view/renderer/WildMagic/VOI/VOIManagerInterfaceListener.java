package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;

import java.awt.Cursor;
import java.util.BitSet;

import javax.swing.JFrame;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public interface VOIManagerInterfaceListener
{
    public void create3DVOI( boolean bIntersection );
    public void enableBoth( boolean bEnable );
    public ModelImage getActiveImage();
    public ModelLUT getActiveLUT();
    public ModelRGB getActiveRGB();
    public Vector3f getCenterPt();
    public JFrame getFrame();    
    public VOIManagerInterface getVOIManager();
    public void maskToPaint();
    public void paintToShortMask();
    public void paintToUbyteMask();
    public void paintToVOI();
    public void PointerActive(boolean bActive);
    public Vector3f PropDown(int iActive);
    public Vector3f PropUp(int iActive);
    public void setActiveImage( int active );
    public void setActiveImage( ModelImage kImage );
    public void setCenter( Vector3f kCenter );
    public void setCursor( Cursor kCursor );
    public void setModified();
    public void setPaintMask(final BitSet mask);
    public void updateData(boolean bCopyToCPU);
}
