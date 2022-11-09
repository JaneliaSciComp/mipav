import gov.nih.mipav.plugins.PlugInGeneric;

/**
 * @author pandyan
 * 
 * Entry point to the DTI Par Rec Sorting Process Plug-In
 * 
 * References: This algorithm was developed in concert with Okan Irfanoglu, Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInDTIParRecSortingProcess implements PlugInGeneric {

	/**
	 * Constructor
	 *
	 */
	public PlugInDTIParRecSortingProcess() {

	}
	
	/**
	 * run method
	 */
	public void run() {
		// TODO Auto-generated method stub
		new PlugInDialogDTIParRecSortingProcess(false);
	}

}
