package gov.nih.mipav.view.renderer.WildMagic.VOI;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public interface ScreenCoordinateListener
{
    public boolean screenToFile(int iX, int iY, int iZ, Vector3f kVolumePt );
    public Vector3f fileToScreen( Vector3f kFile );
    public Vector3f patientToScreen( Vector3f kPt );
    public int getWidth();
    public int getHeight();
}
