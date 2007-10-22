import gov.nih.mipav.plugins.PlugInGeneric;

/**
 * This is the entry point for the DTI Open List File Plug-In
 * This plugin opens a 4D DTI dataset by reading in the list file
 * @author pandyan
 * 
 * 
 * References: This algorithm was developed in concert with Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInDTIOpenListFile implements PlugInGeneric {
	
	
	/**
	 * Constructor
	 *
	 */
	public PlugInDTIOpenListFile() {

	}

	
	/**
	 * run
	 */
	public void run() {
		new PlugInDialogDTIOpenListFile(false);

	}

}
