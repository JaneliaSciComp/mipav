package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;


/**
 *
 * @see  FileNRRD
 */

public class FileInfoNRRD extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID = -5506021019885109431L;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    
    /** Transformation matrix. */
    private TransMatrix matrix = new TransMatrix(4);
    
    private int versionNumber = -1;
    
    /** Data compression - raw, gzip */
    private String encodingString = null;
    
    /** Concise textual description of the information in the array */
    private String contentString = null;
    
    private String labelString[] = null;
    
    private float sliceThickness = -1.0f;
    
    private String modalityString = null;
    
    private String DWMRI_B_VALUE = null;
    
    private String dwmriGradient[][] = null;

    
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

        dialog.appendPrimaryData("Orientation", getImageOrientationStr(getImageOrientation()));

        float[] resolutions; // = new float[5];
        resolutions = getResolutions();

        int[] measure; // = new int[5];
        measure = getUnitsOfMeasure();

        for (i = 0; i < extents.length; i++) {

            if (resolutions[i] > 0.0) {
                String pixelRes = "Pixel resolution " + i;
                dialog.appendPrimaryData(pixelRes,
                                         Float.toString(resolutions[i]) + " " + getUnitsOfMeasureStr(measure[i]));
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
                    if ((dwmriGradient[i][0] != null) && (dwmriGradient[i][1] != null))
                        dialog.appendSecondaryData(dwmriGradient[i][0], dwmriGradient[i][1]);
                    }
                }
            }
        } // if (dwmriGradient != null)
    
    /**
     * 
     * @param versionNumber
     */
    public void setVersionNumber(int versionNumber) {
        this.versionNumber = versionNumber;
    }
    
    /**
     * 
     * @param encodingString
     */
    public void setEncoding(String encodingString) {
        this.encodingString = encodingString;
    }
    
    /**
     * 
     * @param contentString
     */
    public void setContent(String contentString) {
        this.contentString = contentString;
    }
    
    /**
     * 
     * @param labelString
     */
    public void setLabels(String labelString[]) {
        this.labelString = labelString;
    }
    
    /**
     * 
     * @param sliceThickness
     */
    public void setSliceThickness(float sliceThickness) {
        this.sliceThickness = sliceThickness;
    }
    
    /**
     * 
     * @param modalityString
     */
    public void setModality(String modalityString) {
        this.modalityString = modalityString;
    }
    
    /**
     * 
     * @param DWMRI_B_VALUE
     */
    public void setDWMRI_B_VALUE(String DWMRI_B_VALUE) {
        this.DWMRI_B_VALUE = DWMRI_B_VALUE;
    }
    
    /**
     * 
     * @param dwmriGradient
     */
    public void setDwmriGradient(String dwmriGradient[][]) {
        this.dwmriGradient = dwmriGradient;
    }
  
}
