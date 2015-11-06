import java.awt.Frame;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.PlugInAlgorithm;
import gov.nih.mipav.view.ViewJFrameImage;


public class PlugInNeuronSegmentation implements PlugInAlgorithm {

	@Override
	public void run(Frame parentFrame, ModelImage image) {
		// TODO Auto-generated method stub
		try{
		new PlugInDialogNeuronSegmentation((ViewJFrameImage) parentFrame, image);
		}
		catch(Exception e){
			e.printStackTrace();
		}
	}

}
