import gov.nih.mipav.plugins.PlugInAlgorithm;
import gov.nih.mipav.plugins.PlugInGeneric;

import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Frame;



public class PlugInNEIRetinalRegistration implements PlugInGeneric {
    
    public PlugInNEIRetinalRegistration() {
        
    }

    public void run() {

        // TODO Auto-generated method stub
        new PlugInDialogNEIRetinalRegistration(false);
    }
}