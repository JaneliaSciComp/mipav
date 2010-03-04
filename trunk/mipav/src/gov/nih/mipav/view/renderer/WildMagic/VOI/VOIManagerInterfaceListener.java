package gov.nih.mipav.view.renderer.WildMagic.VOI;

import java.awt.Cursor;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public interface VOIManagerInterfaceListener
{
    public void create3DVOI( boolean bIntersection );
    public void PointerActive(boolean bActive);
    public Vector3f PropDown(int iActive);    
    public Vector3f PropUp(int iActive);
    public void setCursor( Cursor kCursor );
    public void setDefaultCursor();
    public void setModified();
    public void updateData(boolean bCopyToCPU);
}
