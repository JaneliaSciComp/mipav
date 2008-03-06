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
    private String dateWritten = null;
    private String timeWritten = null;
    private String jobName = null;
    private String object = null;
    private String observer = null;

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
        
        if (origin != null) {
            dialog.append("ORIGIN, installation where file is written = " + origin + "\n");
        }
        
        if (dateAcquired != null) {
            dialog.append("DATE data acquired = " + dateAcquired + "\n");
        }
        
        if (timeAcquired != null) {
            dialog.append("TIME data acquired = " + timeAcquired + "\n");
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
        
        if (object != null) {
            dialog.append("OBJECT observed = " + object + "\n");
        }
        
        if (observer != null) {
            dialog.append("OBSERVER = " + observer + "\n");
        }
        
    }
    
    public void setDateAcquired(String dateAcquired) {
        this.dateAcquired = dateAcquired;
    }
    
    public void setTimeAcquired(String timeAcquired) {
        this.timeAcquired = timeAcquired;
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
}
