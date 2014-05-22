import java.awt.Frame;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.PlugInAlgorithm;


public class PlugInNeuronalOverlap implements PlugInAlgorithm {

	@Override
	public void run(Frame parentFrame, ModelImage image) {
		// TODO Auto-generated method stub
		new PlugInDialogNeuronalOverlap();
	}

}
