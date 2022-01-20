import gov.nih.mipav.plugins.*;


public class PlugInStrokeSegmentationSIR implements PlugInGeneric {
    public static final String[] CATEGORY = {"Stroke"};
    
    public void run() {
        new PlugInDialogStrokeSegmentationSIR();
    }
}
