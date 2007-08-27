import gov.nih.mipav.plugins.PlugInGeneric;

/**
 * @author pandyan
 * 
 * This is the main entry point for the DTI Color Display plugin
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
 * 
 *  * Publication Reference:
 * 
 * S. Pajevic and C. Pierpaoli, “Color Schemes to Represent the Orientation of Anisotropic Tissues from Diffusion Tensor Data: Application to White Matter Fiber Tract Mapping in the Human Brain,” Magnetic Resonance in Medicine, vol. 42, no. 3, pp. 526-540, 1999
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
