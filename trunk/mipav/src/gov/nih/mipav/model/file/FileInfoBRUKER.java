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
    
    private double sliceGradientDuration = Double.NaN;
    
    private double sliceGradient = Double.NaN;
    
    private double sliceGradientLimit = Double.NaN;
    
    private double TEsliceSpoilerGradientsDuration = Double.NaN;
    
    private double TESliceSpoilerGradientsAmplitude = Double.NaN;
    
    private double TESliceSpoilerGradientsLimit = Double.NaN;
    
    private double diffusionGradientDuration[] = null;
    
    private double diffusionGradientSeparation[] = null;
    
    private String directScaledSwitching = null;
    
    private String diffusionMeasurementMode = null;
    
    private int numberOfDiffusionDirections = -1;
    
    private int diffusionExperimentsPerDirection = -1;
    
    private int numberOfA0Images = -1;
    
    private String patientPosition = null;
    
    private double acqGradMat[][][] = null;

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
               effectiveSpectralBandwidth +
               "\n\tThe optimal bandwidth choice is a" + 
               "\n\ttrade-off between the field strength (to" + 
		       "\n\tdecrease chemical shift artifacts, increase" +
               "\n\tbandwidth), the S/N ratio (decrease bandwidth)" +
		       "\n\tand the minimum possible TE(increase bandwidth).\n");
        }
        
        if (!Double.isNaN(echoTime)) {
            dialog.append("Delay (abbreviation TE) between the effective centre of the excitation pulse"
                                         + "\n\t(depends on pulse rephasing properties) and "
                                         + "\n\tthe acquisition of the k-space centre:\t "
                        		         + echoTime +
                        		         "\n\tTE determines the T2 weighting of the spectra. The minimum" +
                                         "\n\tof TE depends on excitation pulse lengths and the" + 
                        		         "\n\tduration of spoiler gradients\n");	
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
        
        if (!Double.isNaN(sliceGradientDuration)) {
        	dialog.append("Slice gradient duration:\t " + sliceGradientDuration + 
                          "\n\tIt is possible to modify this duration within the limits of" +
                          "\n\t[min,min+4*risetime]. The minimum is defined by the actual" + 
                          "\n\tduration of the RF pulse. With the help of this parameter a" +
                          "\n\tcertain duration of the slice gradient plateau between the" +
                          "\n\tgradient switching event of the slice gradient and the start" +
                          "\n\tof the RF pulse can be specified. By default, the minimum" +
                          "\n\tduration is set.\n");
        }
        
        if (!Double.isNaN(sliceGradient)) {
        	dialog.append("Slice gradient:\t " + sliceGradient + "\n");
        }
        
        if (!Double.isNaN(sliceGradientLimit)) {
        	dialog.append("Slice gradient limit:\t " + sliceGradientLimit + "\n");
        }
        
        if (!Double.isNaN(TEsliceSpoilerGradientsDuration)) {
        	dialog.append("Duration of the TE slice spoilers gradient:\t " + TEsliceSpoilerGradientsDuration + "\n" +
        			      "\tAround the refocusing pulse in spin echo or in the echo periods" +
        			      "\n\tin stimulated echo preparation.  In stimulated echo the" +
        			      "\n\tduration of the TM spoiler gradient is fixed to be 3 times" +
        			      "\n\tthe duration of this TE spoiler duration and the gradient" +
        			      "\n\tamplitude of the TM spoiler is fixed to the value of the TE" +
        			      "\n\tspoilers. This should prevent the excitation of unwanted" +
        			      "\n\tcoherences of the other 3-Pulse signals in the stimulated" +
        			      "\n\techo preparation mode.\n");
        }
        
        if (!Double.isNaN(TESliceSpoilerGradientsAmplitude)) {
        	dialog.append("Amplitude of the TE slice spoiler gradients:\t " + TESliceSpoilerGradientsAmplitude +
                          "\n\t(expressed in% of max. gradient power)" +
        			      "\n\tIn stimulated echo the TM slice spolier gradient is fixed" +
                          "\n\tto the same value\n");
        }
        
        if (!Double.isNaN(TESliceSpoilerGradientsLimit)) {
        	dialog.append("TE slice spoiler gradients limit:\t " + TESliceSpoilerGradientsLimit + "\n");
        }
        
        if (diffusionGradientDuration != null) {
        	dialog.append("Array of duration of gradient pulses of the diffusion experiment:\n");
        	for (int i = 0; i < diffusionGradientDuration.length; i++) {
        	    dialog.append("Duration["+i+"]:\t " + diffusionGradientDuration[i] + "\n");	
        	}
        }
        
        if (diffusionGradientSeparation != null) {
        	dialog.append("Array of separation of gradient pulses of the diffusion experiment:\n");
        	for (int i = 0; i < diffusionGradientSeparation.length; i++) {
        	    dialog.append("Separation["+i+"]:\t " + diffusionGradientSeparation[i] + "\n");	
        	}
        }
        
        if (directScaledSwitching != null) {
        	dialog.append(directScaledSwitching + "\n");
        }
        
        if (diffusionMeasurementMode != null) {
        	dialog.append(diffusionMeasurementMode + "\n");
        }
        
        if (numberOfDiffusionDirections != -1) {
        	dialog.append("Number of different directions of the diffusion gradients:\t " + 
                                           numberOfDiffusionDirections + "\n");
        }
        
        if (diffusionExperimentsPerDirection != -1) {
        	dialog.append("Number of diffusion experiments sequentially performed in each" + 
                          "\n\tspecified direction:\t " + diffusionExperimentsPerDirection +
                          "\n\tEach diffusion experiment is characterized by a certain" +
                          "\n\tdiffusion gradient strength. The diffusion gradient strength" +
                          "\n\tis varied during the execution of the diffusion loop.\n");
        }
        
        if (numberOfA0Images != -1) {
        	dialog.append("Number of A0 images = " + numberOfA0Images +
        			      "\n\tNumber of Images performed without diffusion gradients." +
        			      "\n\tIn experiments with a large number of high b-values (e.g." +
                          "\n\thigh number of diffusion directions and 1 b-value per" +
        			      "\n\tdirection) the A0 reference point might be underestimated" +
                          "\n\tif set to 1.\n");
        }
        
        if (patientPosition != null) {
        	dialog.append("Patient position = " + patientPosition + "\n");
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
    
    /**
     * 
     * @param refocusingPulseType
     */
    public void setRefocusingPulseType(String refocusingPulseType) {
    	this.refocusingPulseType = refocusingPulseType;
    }
    
    /**
     * 
     * @param sliceGradientDuration
     */
    public void setSliceGradientDuration(double sliceGradientDuration) {
    	this.sliceGradientDuration = sliceGradientDuration;
    }
    
    /**
     * 
     * @param sliceGradient
     */
    public void setSliceGradient(double sliceGradient) {
    	this.sliceGradient = sliceGradient;
    }
    
    /**
     * 
     * @param sliceGradientLimit
     */
    public void setSliceGradientLimit(double sliceGradientLimit) {
    	this.sliceGradientLimit = sliceGradientLimit;
    }
    
    /**
     * 
     * @param TEsliceSpoilerGradientsDuration
     */
    public void setTESliceSpoilerGradientsDuration(double TEsliceSpoilerGradientsDuration) {
    	this.TEsliceSpoilerGradientsDuration = TEsliceSpoilerGradientsDuration;
    }
    
    /**
     * 
     * @param TEsliceSpoilerGradientsAmplitude
     */
    public void setTESliceSpoilerGradientsAmplitude(double TEsliceSpoilerGradientsAmplitude) {
    	this.TESliceSpoilerGradientsAmplitude = TEsliceSpoilerGradientsAmplitude;
    }
    
    /**
     * 
     * @param TEsliceSpoilerGradientsLimit
     */
    public void setTESliceSpoilerGradientsLimit(double TEsliceSpoilerGradientsLimit) {
    	this.TESliceSpoilerGradientsLimit = TEsliceSpoilerGradientsLimit;
    }
    
    /**
     * 
     * @param diffusionGradientDuration
     */
    public void setDiffusionGradientDuration(double[] diffusionGradientDuration) {
    	this.diffusionGradientDuration = diffusionGradientDuration;
    }
    
    /**
     * 
     * @param diffusionGradientSeparation
     */
    public void setDiffusionGradientSeparation(double[] diffusionGradientSeparation) {
    	this.diffusionGradientSeparation = diffusionGradientSeparation;
    }
    
    /**
     * 
     * @param directScaledSwitching
     */
    public void setDirectScaledSwitching(String directScaledSwitching) {
    	this.directScaledSwitching = directScaledSwitching;
    }
    
    /**
     * 
     * @param diffusionMeasurementMode
     */
    public void setDiffusionMeasurementMode(String diffusionMeasurementMode) {
    	this.diffusionMeasurementMode = diffusionMeasurementMode;
    }
    
    /**
     * 
     * @param numberOfDiffusionDirections
     */
    public void setNumberOfDiffusionDirections(int numberOfDiffusionDirections) {
    	this.numberOfDiffusionDirections = numberOfDiffusionDirections;
    }
    
    /**
     * 
     * @param diffusionExperimentsPerDirection
     */
    public void setDiffusionExperimentsPerDirection(int diffusionExperimentsPerDirection) {
    	this.diffusionExperimentsPerDirection = diffusionExperimentsPerDirection;
    }
    
    /**
     * 
     * @param numberOfA0Images
     */
    public void setNumberOfA0Images(int numberOfA0Images) {
        this.numberOfA0Images = numberOfA0Images;	
    }
    
    /**
     * 
     * @param patientPosition
     */
    public void setPatientPosition(String patientPosition) {
    	this.patientPosition = patientPosition;
    }
    
    /**
     * 
     * @param acqGradMat
     */
    public void setAcqGradMat(double[][][] acqGradMat) {
        this.acqGradMat = acqGradMat;	
    }
    
    /**
     * 
     * @return
     */
    public double[][][] getAcqGradMat() {
    	return acqGradMat;
    }
}
