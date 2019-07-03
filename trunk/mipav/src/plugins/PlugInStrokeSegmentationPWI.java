import gov.nih.mipav.plugins.*;


public class PlugInStrokeSegmentationPWI implements PlugInGeneric {
    public static final String[] CATEGORY = {"Stroke"};
    
    public void run() {
        new PlugInDialogStrokeSegmentationPWI();
    }
}
