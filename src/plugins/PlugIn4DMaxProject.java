import java.awt.Frame;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.PlugInAlgorithm;


public class PlugIn4DMaxProject implements PlugInAlgorithm {

	@Override
	public void run(Frame parentFrame, ModelImage image) {
		// TODO Auto-generated method stub

		new PlugInDialog4DMaxProject(parentFrame, image);
	}

}
