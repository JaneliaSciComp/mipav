package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a Fits image is stored on disk.
 */
public class FileInfoFits extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8285638648507573285L;
    
    private String origin = null;
    private String dateAcquired = null;
    private String timeAcquired = null;
    private String dateProcessed = null;
    private String timeProcessed = null;
    private String dateWritten = null;
    private String timeWritten = null;
    private String jobName = null;
    private String object = null;
    private String observer = null;
    private String instrument = null;
    private float focalRatio = -1.0f;
    private String CTYPE1 = null;
    private String CTYPE2 = null;
    private String CTYPE3 = null;
    private String CTYPE4 = null;
    private String CTYPE5 = null;
    private String CUNIT1 = null;
    private String CUNIT2 = null;
    private String CUNIT3 = null;
    private String CUNIT4 = null;
    private String CUNIT5 = null;
    private String BUNIT = null;
    private String author = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoFits(String name, String directory, int format) {
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
        
        if (BUNIT != null) {
            dialog.append("Pixel value units = " + BUNIT + "\n");
        }
        
        if (CTYPE1 != null) {
            dialog.append("CTYPE1, name along x-axis = " + CTYPE1 + "\n");
        }
        
        if (CUNIT1 != null) {
            dialog.append("CUNIT1, units along x-axis = " + CUNIT1 + "\n");
        }
        
        if (CTYPE2 != null) {
            dialog.append("CTYPE2, name along y-axis = " + CTYPE2 + "\n");
        }
        
        if (CUNIT2 != null) {
            dialog.append("CUNIT2, units along y-axis = " + CUNIT2 + "\n");
        }
        
        if (CTYPE3 != null) {
            dialog.append("CTYPE3, name along z-axis = " + CTYPE3 + "\n");
        }
        
        if (CUNIT3 != null) {
            dialog.append("CUNIT3, units along z-axis = " + CUNIT3 + "\n");
        }
        
        if (CTYPE4 != null) {
            dialog.append("CTYPE4, name along t-axis = " + CTYPE4 + "\n");
        }
        
        if (CUNIT4 != null) {
            dialog.append("CUNIT4, units along t-axis = " + CUNIT4 + "\n");
        }
        
        if (CTYPE5 != null) {
            dialog.append("CTYPE5, name along fifth-axis = " + CTYPE5 + "\n");
        }
        
        if (CUNIT5 != null) {
            dialog.append("CUNIT5, units along fifth-axis = " + CUNIT5 + "\n");
        }
        
        if (origin != null) {
            dialog.append("ORIGIN, installation where file is written = " + origin + "\n");
        }
        
        if (dateAcquired != null) {
            dialog.append("DATE data acquired = " + dateAcquired + "\n");
        }
        
        if (timeAcquired != null) {
            dialog.append("TIME data acquired = " + timeAcquired + "\n");
        }
        
        if (dateProcessed != null) {
            dialog.append("DATE data last processed = " + dateProcessed + "\n");
        }
        
        if (timeProcessed != null) {
            dialog.append("TIME data last processed = " + timeProcessed + "\n");
        }
        
        if (dateWritten != null) {
            dialog.append("DATE file written = " + dateWritten + "\n");
        }
        
        if (timeWritten != null) {
            dialog.append("TIME file written = " + timeWritten + "\n");
        }
        
        if (jobName != null) {
            dialog.append("JOB NAME = " + jobName + "\n");
        }
        
        if (author != null) {
            dialog.append("AUTHOR = " + author + "\n");
        }
        
        if (object != null) {
            dialog.append("OBJECT observed = " + object + "\n");
        }
        
        if (observer != null) {
            dialog.append("OBSERVER = " + observer + "\n");
        }
        
        if (instrument != null) {
            dialog.append("INSTRUMENT in use = " + instrument + "\n");
        }
        
        if (focalRatio > 0.0f) {
            dialog.append("FOCAL RATIO = " + focalRatio + "\n");
        }
        
    }
    
    public void setBUNIT(String BUNIT) {
        this.BUNIT = BUNIT;
    }
    
    public void setCTYPE1(String CTYPE1) {
        this.CTYPE1 = CTYPE1;
    }
    
    public void setCTYPE2(String CTYPE2) {
        this.CTYPE2 = CTYPE2;
    }
    
    public void setCTYPE3(String CTYPE3) {
        this.CTYPE3 = CTYPE3;
    }
    
    public void setCTYPE4(String CTYPE4) {
        this.CTYPE4 = CTYPE4;
    }
    
    public void setCTYPE5(String CTYPE5) {
        this.CTYPE5 = CTYPE5;
    }
    
    public void setCUNIT1(String CUNIT1) {
        this.CUNIT1 = CUNIT1;
    }
    
    public void setCUNIT2(String CUNIT2) {
        this.CUNIT2 = CUNIT2;
    }
    
    public void setCUNIT3(String CUNIT3) {
        this.CUNIT3 = CUNIT3;
    }
    
    public void setCUNIT4(String CUNIT4) {
        this.CUNIT4 = CUNIT4;
    }
    
    public void setCUNIT5(String CUNIT5) {
        this.CUNIT5 = CUNIT5;
    }
    
    public void setDateAcquired(String dateAcquired) {
        this.dateAcquired = dateAcquired;
    }
    
    public void setTimeAcquired(String timeAcquired) {
        this.timeAcquired = timeAcquired;
    }
    
    public void setDateProcessed(String dateProcessed) {
        this.dateProcessed = dateProcessed;
    }
    
    public void setTimeProcessed(String timeProcessed) {
        this.timeProcessed = timeProcessed;
    }
    
    public void setDateWritten(String dateWritten) {
        this.dateWritten = dateWritten;
    }
    
    public void setTimeWritten(String timeWritten) {
        this.timeWritten = timeWritten;
    }
    
    public void setJobName(String jobName) {
        this.jobName = jobName;
    }
    
    public void setOrigin(String origin) {
        this.origin = origin;
    }
    
    public void setObject(String object) {
        this.object = object;
    }
    
    public void setObserver(String observer) {
        this.observer = observer;
    }
    
    public void setInstrument(String instrument) {
        this.instrument = instrument;
    }
    
    public void setFocalRatio(float focalRatio) {
        this.focalRatio = focalRatio;
    }
    
    public void setAuthor(String author) {
        this.author = author;
    }
    
}
