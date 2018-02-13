import gov.nih.mipav.plugins.PlugInGeneric;


/**
 * @author pandyan
 * 
 * This is the main entry point for the DTI Color Display plugin
 * 
 * References: Developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group, Lin-Ching Chang D.Sc., Carlo
 * Pierpaoli MD Ph.D., and Lindsay Walker MS from the the NIH/NICHD/LIMB/STBB group and Olga Vogt from the
 * NIH/CIT/DCB/ISL/BIRSS group:
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL) Biomedical Imaging Research Services Section (BIRSS) Imaging
 * Sciences Laboratory (ISL) Division of Cumputational Bioscience (DCB) Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB) Laboratory of Integrative and Medical Biophysics (LIMB) National
 * Institute of Child Health & Humann Development National Institutes of Health
 * 
 * 
 * Publication Reference:
 * 
 * S. Pajevic and C. Pierpaoli, "Color Schemes to Represent the Orientation of Anisotropic Tissues from Diffusion Tensor
 * Data: Application to White Matter Fiber Tract Mapping in the Human Brain," Magnetic Resonance in Medicine, vol. 42,
 * no. 3, pp. 526-540, 1999
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
