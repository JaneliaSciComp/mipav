package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * File information related to the Bruker/Biospin scanner format.
 */

public class FileInfoBRUKER extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 292865443840539139L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Whether the z resolution is set in the acqp or reco files*/
    private boolean haveZResol = false;

    /** The size of the reconstruction */
    private int recoSize = -1;
    
    /** The slice inversion time of the scan */
    private String sliceSeparationMode = null;

    /** The inversion time of an MR scan. */
    private double inversionTime;
    
    private String method = null;
    
    private double effectiveSpectralBandwidth = Double.NaN;
    
    private double echoTime = Double.NaN;
    
    private int numberOfSegments = -1;
    
    private double repetitionTime = Double.NaN;
    
    private double delayBetweenVolumes = Double.NaN;
    
    private int numberOfAverages = -1;
    
    private int numberOfRepetitions = -1;
    
    private String scanTime = null;
    
    private String deriveGains = null;
    
    private String diffusionPreparation = null;
    
    private double usedSliceThickness = Double.NaN;
    
    private String showAllParameters = null;
    
    private String refocusingPulseType = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoBRUKER(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogText dialog = (JDialogText) dlog;

        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        if (sliceSeparationMode != null) {
            dialog.append("Slice separation mode:\t" + sliceSeparationMode + "\n");
        }

        if (getSliceThickness() > 0.0f) {
            dialog.append("Slice thickness:\t" + getSliceThickness() + " mm\n");
        }
        
        if(inversionTime != 0) {
            dialog.append("Inversion time:\t\t" + inversionTime + " ms\n");
        }
        
        if (method != null) {
        	dialog.append("Method = " + method + "\n");
        }
        
        if (!Double.isNaN(effectiveSpectralBandwidth)) {
        	dialog.append("Effective bandwidth of data sampling \n\tduring the frequency encoding period:\t" +
               effectiveSpectralBandwidth + "\n");
        }
        
        if (!Double.isNaN(echoTime)) {
            dialog.append("Delay between the effective centre of the excitation pulse"
                                         + "\n\t(depends on pulse rephasing properties) and "
                                         + "\n\tthe acquisition of the k-space centre:\t "
                        		         + echoTime + "\n");	
        }
        
        if (numberOfSegments != -1) {
        	dialog.append("Number of segments in k-space:\t " + numberOfSegments + "\n");
        }
        
        if (!Double.isNaN(repetitionTime)) {
        	dialog.append("Delay between corresponding slices in consecutive volumes:\t " + repetitionTime + "\n");
        }
        
        if (!Double.isNaN(delayBetweenVolumes)) {
        	dialog.append("Delay between consecutive groups of slices (volume) " +
                          "\n\twhen repetitions > 1:\t " + delayBetweenVolumes + "\n");
        }
        
        if (numberOfAverages != -1) {
        	dialog.append("Number of accumulations which are averaged to increase\n\tthe signal-to-noise ratio of the spectra:\t " +
                numberOfAverages + "\n");
        }
        
        if (numberOfRepetitions != -1) {
        	dialog.append("Number of repetitions of experiments:\t " + numberOfRepetitions + "\n");
        }
        
        if (scanTime != null) {
        	dialog.append("Total duration of the experiment:\t " + scanTime + "\n");
        }
        
        if (deriveGains != null) {
        	dialog.append(deriveGains + "\n");
        }
        
        if (diffusionPreparation != null) {
        	dialog.append(diffusionPreparation + "\n");
        }
        
        if (!Double.isNaN(usedSliceThickness)) {
        	dialog.append("The slice thickness used to calculate the slice grFdients = " + usedSliceThickness + 
                                          "\n\tFor methods providing a Bandwidth Scaling factor" + 
                        		          "\n\tdifferent from 100% this value differs from PVM_SliceThick." +
                        		          "\n");
        }
        
        if (showAllParameters != null) {
        	dialog.append(showAllParameters + "\n");
        }
        
        if (refocusingPulseType != null) {
        	dialog.append("Refocusing pulse type:\t " + refocusingPulseType + "\n");
        }
    }

    /**
     * Accessor to get the flag for having a z resolution.
     *
     * @return  <code>true</code> if has a z resolution.
     */
    public boolean getHaveZResol() {
        return haveZResol;
    }

    /**
     * Gets the size of the reconstruction.
     *
     * @return  The reco size.
     */
    public int getRecoSize() {
        return recoSize;
    }
    
    /**
     * Gets the inversion time of the scan.
     * 
     * @return The inversion time.
     */
    public double getInversionTime() {
        return inversionTime;
    }

    /**
     * Accessor to set the flag for having a z resolution.
     *
     * @param  haveZResol  Flag to set.
     */
    public void setHaveZResol(boolean haveZResol) {
        this.haveZResol = haveZResol;
    }

    /**
     * Accessor to set the reco size.
     *
     * @param  recoSize  Value to set.
     */
    public void setRecoSize(int recoSize) {
        this.recoSize = recoSize;
    }

    /**
     * Accessor to set the slice separation mode.
     *
     * @param  sliceSeparationMode  Value to set.
     */
    public void setSliceSeparationMode(String sliceSeparationMode) {
        this.sliceSeparationMode = sliceSeparationMode;
    }

    /**
     * Accessor to set the inversion time of the scan
     * 
     * @param inversionTime the inversion time of the scan
     */
    public void setInversionTime(double inversionTime) {
        this.inversionTime = inversionTime;        
    }
    
    public void setMethod(String method) {
    	this.method = method;
    }
    
    /**
     * 
     * @param effectiveSpectralBandwidth
     */
    public void setEffectiveSpectralBandwidth(double effectiveSpectralBandwidth) {
    	this.effectiveSpectralBandwidth = effectiveSpectralBandwidth;
    }
    
    /**
     * 
     * @param echoTime
     */
    public void setEchoTime(double echoTime) {
    	this.echoTime = echoTime;
    }
    
    public void setNumberOfSegments(int numberOfSegments) {
    	this.numberOfSegments = numberOfSegments;
    }
    
    /**
     * 
     * @param repetitionTime
     */
    public void setRepetitionTime(double repetitionTime) {
    	this.repetitionTime = repetitionTime;
    }
    
    /**
     * 
     * @param delayBetweenVolumes
     */
    public void setDelayBetweenVolumes(double delayBetweenVolumes) {
    	this.delayBetweenVolumes = delayBetweenVolumes;
    }
    
    /**
     * 
     * @param numberOfAverages
     */
    public void setNumberOfAverages(int numberOfAverages) {
    	this.numberOfAverages = numberOfAverages;
    }
    
    /**
     * 
     * @param numberOfRepetitions
     */
    public void setNumberOfRepetitions(int numberOfRepetitions) {
    	this.numberOfRepetitions = numberOfRepetitions;
    }
    
    /**
     * 
     * @param scanTime
     */
    public void setScanTime(String scanTime) {
    	this.scanTime = scanTime;
    }
    
    /**
     * 
     * @param deriveGains
     */
    public void setDeriveGains(String deriveGains) {
    	this.deriveGains = deriveGains;
    }
    
    /**
     * 
     * @param diffusionPreparation
     */
    public void setDiffusionPreparation(String diffusionPreparation) {
    	this.diffusionPreparation = diffusionPreparation;
    }
    
    /**
     * 
     * @param usedSliceThickness
     */
    public void setUsedSliceThickness(double usedSliceThickness) {
    	this.usedSliceThickness = usedSliceThickness;
    }
    
    /**
     * 
     * @param showAllParameters
     */
    public void setShowAllParameters(String showAllParameters) {
    	this.showAllParameters = showAllParameters;
    }
    
    public void setRefocusingPulseType(String refocusingPulseType) {
    	this.refocusingPulseType = refocusingPulseType;
    }
}
