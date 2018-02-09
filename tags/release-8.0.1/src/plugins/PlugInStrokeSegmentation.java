import gov.nih.mipav.plugins.*;


public class PlugInStrokeSegmentation implements PlugInGeneric {
    public static final String[] CATEGORY = {"Stroke"};
    
    public void run() {
        new PlugInDialogStrokeSegmentation();
    }
}
