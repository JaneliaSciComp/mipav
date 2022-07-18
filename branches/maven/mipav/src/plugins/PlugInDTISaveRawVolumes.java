import gov.nih.mipav.plugins.PlugInGeneric;

/**
 * 
 * @author pandyan
 * 
 * This is the mainentry for the DTI Save raw Volumes Plug-In
 * 
 * References: This algorithm was developed in concert with Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 *
 */
public class PlugInDTISaveRawVolumes implements PlugInGeneric {
	
	/**
	 * constructor
	 */
	public PlugInDTISaveRawVolumes() {
		
	}
	
	/**
	 * run
	 */
	public void run() {
		new PlugInDialogDTISaveRawVolumes(false);

	}

}
