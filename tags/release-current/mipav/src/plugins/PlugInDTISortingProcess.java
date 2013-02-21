import gov.nih.mipav.plugins.PlugInGeneric;

/**
 * @author pandyan
 * 
 * Entry point to the DTI sorting plugin
 * 
 * References: This algorithm was developed in concert with Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 */
public class PlugInDTISortingProcess implements PlugInGeneric {
	
	/**
	 * Constructor
	 *
	 */
	public PlugInDTISortingProcess() {

	}

	
	/**
	 * run method
	 */
	public void run() {
		// TODO Auto-generated method stub
		new PlugInDialogDTISortingProcess(false);
	}

}
