import gov.nih.mipav.plugins.*;


public class PlugInStrokeSegmentationListenerPWI implements PlugInGeneric {
    public static final String[] CATEGORY = {"Stroke"};

    public void run() {
        new PlugInDialogStrokeSegmentationListenerPWI();
    }
}
