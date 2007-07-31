import gov.nih.mipav.plugins.PlugInGeneric;

/**
 * @author pandyan
 * 
 * This is the main dialog for the DTI Color Display
 * 
 * References: This algorithm was developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group and
 * Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL)
 * Division of Cumputational Bioscience (DCB)
 * Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInDTIColorDisplay implements PlugInGeneric {
	
	
	/**
	 * Constructor
	 */
	public PlugInDTIColorDisplay() {
		
	}

	/**
	 * run
	 */
	public void run() {
		new PlugInDialogDTIColorDisplay(false);

	}

}
