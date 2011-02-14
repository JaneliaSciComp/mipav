package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 *
 * @see  FileNRRD
 */

public class FileInfoNRRD extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID = -5506021019885109431L;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** Version of the NRRD file format being used. */
    private float versionNumber = -1.0f;
    
    /** Data compression - raw, gzip */
    private String encodingString = null;
    
    /** Concise textual description of the information in the array */
    private String contentString = null;
    
    /** Axis names such as X, Y, or Z */
    private String labelString[] = null;
    
    private float sliceThickness = -1.0f;
    
    /** Diffusion weighted MRI is currently the only value that has been seen for modality 
     *  in our samples. */
    private String modalityString = null;
    
    /** "DWMRI_b-value:=b " : This key/value pair gives the (scalar) diffusion-weighting value,
     *  in units of s/mm^2. Example: "DWMRI_b-value:=1000". The effective
     *  magnitude of diffusion-weighting for each DWI is determined with some simple calculations based
     *  on the individual per-DWI gradient directions or B-matrices. */
    private String DWMRI_B_VALUE = null;
    
    /** For every index position NNNN along the DWI axis (whichever is the non-spatial axis identified
     *  by the "list" or "vector" kind field), either "DWMRI_gradient_NNNN:=x y z " or 
     *  "DWMRI_B-matrix_NNNN:=xx xy xz yy yz zz " must be given (except if "DWMRI_NEX_NNNN:= M " is used).  */
    private String dwmriGradient[][] = null;
    
    /** Possible values are "right-anterior-superior", "left-anterior-superior", "left-posterior-superior",
     *  "right-anterior-superior-time", "left-anterior-superior-time", "left-posterior-superior-time",
     *  "scanner-xyz", "scanner-xyz-time", "3d-right-handed", "3d-left-handed", 
     *  "3d-right-handed-time", "3d-left-handed-time" */
    private String spaceString = null;

    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoNRRD(String name, String directory, int format) {
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
        JDialogFileInfo dialog = (JDialogFileInfo) dlog;
        int[] extents;
        int i;
        int[] editorChoice = new int[1];
        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo(this); // setup layout in the dialog

        extents = super.getExtents();

        for (i = 0; i < extents.length; i++) {
            dialog.appendPrimaryData("Dimension " + i, Integer.toString(extents[i]));
        }

        dialog.appendPrimaryData("Type", ModelStorageBase.getBufferTypeStr(getDataType()));

        if (ModelImage.isColorImage(getDataType())) {
            dialog.appendPrimaryData("Min red", Double.toString(getMinR()));
            dialog.appendPrimaryData("Max red", Double.toString(getMaxR()));
            dialog.appendPrimaryData("Min green", Double.toString(getMinG()));
            dialog.appendPrimaryData("Max green", Double.toString(getMaxG()));
            dialog.appendPrimaryData("Min blue", Double.toString(getMinB()));
            dialog.appendPrimaryData("Max blue", Double.toString(getMaxB()));

        } else {
            dialog.appendPrimaryData("Min", Double.toString(getMin()));
            dialog.appendPrimaryData("Max", Double.toString(getMax()));
        }

        dialog.appendPrimaryData("Modality", FileInfoBase.getModalityStr(getModality()));

        if (extents.length > 2) {
            dialog.appendPrimaryData("Orientation", getImageOrientationStr(getImageOrientation()));
    
            switch (axisOrientation[0]) {
    
                case ORI_R2L_TYPE:
                    dialog.appendPrimaryData("X axis orientation", "right to left");
                    break;
    
                case ORI_L2R_TYPE:
                    dialog.appendPrimaryData("X axis orientation","left to right");
                    break;
    
                case ORI_A2P_TYPE:
                    dialog.appendPrimaryData("X axis orientation","anterior to posterior");
                    break;
    
                case ORI_P2A_TYPE:
                    dialog.appendPrimaryData("X axis orientation","posterior to anterior");
                    break;
    
                case ORI_I2S_TYPE:
                    dialog.appendPrimaryData("X axis orientation","inferior to superior");
                    break;
    
                case ORI_S2I_TYPE:
                    dialog.appendPrimaryData("X axis orientation","superior to inferior");
                    break;
    
                default:
                    dialog.appendPrimaryData("X axis orientation","unknown to unknown");
            }
            
            switch (axisOrientation[1]) {
            
                case ORI_R2L_TYPE:
                    dialog.appendPrimaryData("Y axis orientation", "right to left");
                    break;
    
                case ORI_L2R_TYPE:
                    dialog.appendPrimaryData("Y axis orientation","left to right");
                    break;
    
                case ORI_A2P_TYPE:
                    dialog.appendPrimaryData("Y axis orientation","anterior to posterior");
                    break;
    
                case ORI_P2A_TYPE:
                    dialog.appendPrimaryData("Y axis orientation","posterior to anterior");
                    break;
    
                case ORI_I2S_TYPE:
                    dialog.appendPrimaryData("Y axis orientation","inferior to superior");
                    break;
    
                case ORI_S2I_TYPE:
                    dialog.appendPrimaryData("Y axis orientation","superior to inferior");
                    break;
    
                default:
                    dialog.appendPrimaryData("Y axis orientation","unknown to unknown");
            }
            
            switch (axisOrientation[2]) {
            
                case ORI_R2L_TYPE:
                    dialog.appendPrimaryData("Z axis orientation", "right to left");
                    break;
    
                case ORI_L2R_TYPE:
                    dialog.appendPrimaryData("Z axis orientation","left to right");
                    break;
    
                case ORI_A2P_TYPE:
                    dialog.appendPrimaryData("Z axis orientation","anterior to posterior");
                    break;
    
                case ORI_P2A_TYPE:
                    dialog.appendPrimaryData("Z axis orientation","posterior to anterior");
                    break;
    
                case ORI_I2S_TYPE:
                    dialog.appendPrimaryData("Z axis orientation","inferior to superior");
                    break;
    
                case ORI_S2I_TYPE:
                    dialog.appendPrimaryData("Z axis orientation","superior to inferior");
                    break;
    
                default:
                    dialog.appendPrimaryData("Z axis orientation","unknown to unknown");
            }
    
            
        } // if (extents.length > 2)

        float[] resolutions; // = new float[5];
        resolutions = getResolutions();

        int[] measure; // = new int[5];
        measure = getUnitsOfMeasure();

        for (i = 0; i < extents.length; i++) {

            if (resolutions[i] > 0.0) {
                String pixelRes = "Pixel resolution " + i;
                dialog.appendPrimaryData(pixelRes,
                                         Float.toString(resolutions[i]) + " " + (Unit.getUnitFromLegacyNum(measure[i])).toString());
            } // end of if (resolutions[i] > 0.0)
        } // for (i=0; i < 5; i++)

        if (getEndianess() == FileBase.LITTLE_ENDIAN) {
            dialog.appendPrimaryData("Endianess", "Little Endian");
        } else {
            dialog.appendPrimaryData("Endianess", "Big Endian");
        }

        if (matrix != null) {

            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.appendPrimaryData("Matrix", matrix.matrixToString(10, 4));
        }
        
        if (versionNumber != -1) {
            dialog.appendSecondaryData("File format version", String.valueOf(versionNumber));
        }
        
        if (contentString != null) {
            dialog.appendSecondaryData("Content", contentString);
        }
        
        if (encodingString != null) {
            dialog.appendSecondaryData("Encoding", encodingString);
        }
        
        if (labelString != null) {
            for (i = 0; i < extents.length; i++) {
              if (labelString[i] != null) {
                  dialog.appendSecondaryData("Label " + i, labelString[i]);
              }
              else {
                  dialog.appendSecondaryData("Label " + i, "  ");
              }
            }
        } // if (labelString != null)
        
        if (sliceThickness > 0.0f) {
            dialog.appendSecondaryData("Slice thickness", String.valueOf(sliceThickness));
        }
        
        if (modalityString != null) {
            dialog.appendSecondaryData("Modality", modalityString);
        }
        
        if (DWMRI_B_VALUE != null) {
            dialog.appendSecondaryData("Scalar diffusion weighting b-value", DWMRI_B_VALUE + " sec/mm^2");
        }
        
        if (dwmriGradient != null) {
            for (i = 0; i < dwmriGradient.length; i++) {
                if (dwmriGradient[i] != null) {
                    if ((dwmriGradient[i][0] != null) && (dwmriGradient[i][1] != null)) {
                        dialog.appendSecondaryData(dwmriGradient[i][0], dwmriGradient[i][1]);
                    }
                }
            }
        } // if (dwmriGradient != null)
    
        if (spaceString != null) {
            dialog.appendSecondaryData("Space", spaceString);
        }
    }
    
    /**
     * Accessor setting versionNumber, version of NRRD file format being used.
     * @param versionNumber
     */
    public void setVersionNumber(float versionNumber) {
        this.versionNumber = versionNumber;
    }
    
    /**
     * Accessor setting encoding string telling data compression method
     * @param encodingString
     */
    public void setEncoding(String encodingString) {
        this.encodingString = encodingString;
    }
    
    /**
     * Accessor setting contentString giving textual description of information in the array
     * @param contentString
     */
    public void setContent(String contentString) {
        this.contentString = contentString;
    }
    
    /**
     * Accessor setting labelString for axis names
     * @param labelString
     */
    public void setLabels(String labelString[]) {
        this.labelString = labelString;
    }
    
    /**
     * Accessor setting sliceThickness
     * @param sliceThickness
     */
    public void setSliceThickness(float sliceThickness) {
        this.sliceThickness = sliceThickness;
    }
    
    /**
     * Accessor setting modalityString
     * @param modalityString
     */
    public void setModality(String modalityString) {
        this.modalityString = modalityString;
    }
    
    /**
     * Accessor setting DWMRI_B_VALUE
     * @param DWMRI_B_VALUE
     */
    public void setDWMRI_B_VALUE(String DWMRI_B_VALUE) {
        this.DWMRI_B_VALUE = DWMRI_B_VALUE;
    }
    
    /**
     * Accessor setting dwmriGradient
     * @param dwmriGradient
     */
    public void setDwmriGradient(String dwmriGradient[][]) {
        this.dwmriGradient = dwmriGradient;
    }
    
    /**
     * Accessor setting spaceString
     * @param spaceString
     */
    public void setSpace(String spaceString) {
        this.spaceString = spaceString;
    }
  
}
