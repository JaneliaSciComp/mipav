/* package PlugInT2;
import gov.nih.mipav.plugins.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import java.awt.*;

public class PlugInT2 implements PlugInAlgorithm
{

	public void run(ViewUserInterface UI, Frame parentFrame, ModelImage image)
	{
		if(parentFrame instanceof ViewJFrameImage)
			new PlugInDialogT2(parentFrame,image);
		else
			MipavUtil.displayError("PlugIn T2 only runs on an image frame");		
	}

}
*/