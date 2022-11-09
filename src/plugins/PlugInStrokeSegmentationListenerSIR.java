import gov.nih.mipav.plugins.*;


public class PlugInStrokeSegmentationListenerSIR implements PlugInGeneric {
    public static final String[] CATEGORY = {"Stroke"};

    public void run() {
        new PlugInDialogStrokeSegmentationListenerSIR();
    }
}
