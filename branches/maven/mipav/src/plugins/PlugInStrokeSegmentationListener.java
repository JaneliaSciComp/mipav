import gov.nih.mipav.plugins.*;


public class PlugInStrokeSegmentationListener implements PlugInGeneric {
    public static final String[] CATEGORY = {"Stroke"};

    public void run() {
        new PlugInDialogStrokeSegmentationListener();
    }
}
