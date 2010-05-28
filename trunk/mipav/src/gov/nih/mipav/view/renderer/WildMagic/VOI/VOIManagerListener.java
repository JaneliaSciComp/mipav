package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;

import java.awt.Cursor;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public interface VOIManagerListener
{
    public void addVOI(VOIBase kOld, boolean bUpdate );
    public void deleteVOI(VOIBase kOld);
    public void pasteVOI(VOIBase kNew);
    public void doVOI(String command);
    public void quickLUT(VOIBase kLUT);
    public void moveVOI( VOIManager kActive, Vector3f kDiff, int iPlane, boolean bFirstMove );
    public void newVOI( boolean bPropagate, boolean bSplit );
    public void saveVOIs( String kCommand );
    public void setActive( VOIManager kManager );
    public void setCurrentColor( );
    public void setCursor( Cursor kCursor );
    public void setDefaultCursor();
    public void setPresetHue(float presetHue);
    public void setSelectedVOI( VOI kSelected, boolean bSelectAll );
    public void showIntensityGraph( VOIBase kVOI ); 
    public void updateGraph( VOIBase kVOI ); 
    public void updateDisplay();
}
