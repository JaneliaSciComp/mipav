package gov.nih.mipav.view.renderer.WildMagic.VOI;

import java.awt.Cursor;
import java.awt.Frame;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.ModelImage;

public interface VOIManagerInterfaceListener
{
    public void create3DVOI( boolean bIntersection );
    public void enableBoth( boolean bEnable );
    public Frame getFrame();
    public void PointerActive(boolean bActive);
    public Vector3f PropDown(int iActive);    
    public Vector3f PropUp(int iActive);
    public void setCenter( Vector3f kCenter );
    public Vector3f getCenterPt();
    public ModelImage getActiveImage();
    public void setActiveImage( int active );
    public void setCursor( Cursor kCursor );
    public void setDefaultCursor();
    public void setModified();
    public void updateData(boolean bCopyToCPU);
}
