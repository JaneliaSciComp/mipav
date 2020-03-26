package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a Fits image is stored on disk.
 */
public class FileInfoJSON extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
    
    private String description = null;
    private String url = null;
    private String version = null;
    private String year = null;
    private String contributor = null;
    private String date_created = null;
	
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoJSON(String name, String directory, int format) {
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
        
        if (description != null) {
        	dialog.append("description = " + description + "\n");
        }
        
        if (url != null) {
        	dialog.append("url = " + url + "\n");
        }
        
        if (version != null) {
        	dialog.append("version = " + version + "\n");
        }
        
        if (year != null) {
        	dialog.append("year = " + year + "\n");
        }
        
        if (contributor != null) {
        	dialog.append("contributor = " + contributor + "\n");
        }
        
        if (date_created != null) {
        	dialog.append("date_created = " + date_created + "\n");
        }
        
    }
    
    public void setDescription(String description) {
    	this.description = description;
    }
    
    public void setURL(String url) {
    	this.url = url;
    }
    
    public void setVersion(String version) {
    	this.version = version;
    }
    
    public void setYear(String year) {
    	this.year = year;
    }
    
    public void setContributor(String contributor) {
    	this.contributor = contributor;
    }
    
    public void setDate_created(String date_created) {
    	this.date_created = date_created;
    }
    
}
