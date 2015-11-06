import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface


public class PlugInEyeTracking implements PlugInGeneric {
    public void run() {
        new PlugInDialogEyeTracking();
    }
}
