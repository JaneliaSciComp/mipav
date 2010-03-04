package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.renderer.WildMagic.Render.LocalVolumeVOI;

import java.awt.Cursor;

public interface VOIManagerListener
{
    public void addVOI(LocalVolumeVOI kOld, boolean bUpdate );
    public void deleteVOI(LocalVolumeVOI kOld);
    public void doVOI(String command);
    public void newVOI( boolean bPropagate );
    public void setActive( VOIManager kManager );
    public void setCurrentColor( );
    public void setCursor( Cursor kCursor );
    public void setDefaultCursor();
    public void setSelectedVOI( VOI kSelected, boolean bSelectAll );
    public void updateDisplay();
}
