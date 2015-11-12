import java.awt.event.ActionEvent;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDrosophilaStandardColumnRegistration_BatchProcessing implements PlugInGeneric {


	public PlugInDrosophilaStandardColumnRegistration_BatchProcessing() {
		
	}

	


	
	public void run() {
		
		
		new PlugInDialogDrosophilaStandardColumnRegistration_BatchProcessing(false);

	}

}
