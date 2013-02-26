package gov.nih.mipav.view.renderer;

import gov.nih.mipav.view.renderer.ViewJComponentVolOpacityBase;


/**
 * Created so the JPanelVolOpacity can be used in more generic container classes.
 */
public interface ViewJComponentVolOpacityListener
{
    public void setAdjustersEnabled(boolean enabled);
    public void update(boolean isChanging);
    public void updateSlider(ViewJComponentVolOpacityBase childComponent);
}