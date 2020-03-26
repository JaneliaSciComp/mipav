package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * File info storage container.
 */
public class FileInfoMATLAB extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
	String headerTextField = null;
	boolean level5Format = true;
	long subsystemSpecificDataOffset = 0L;
	int version = -1;
	String arrayName = null;
	String fieldNames[] = null;
	String sourceFile = null;
    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoMATLAB(String name, String directory, int format) {
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
        int i;
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");
        
        if (level5Format) {
        	dialog.append("The MATLAB file is in level 5 format\n");
        }
        else {
        	dialog.append("The MATLAB file is in level 4 format\n");
        }
        
        if (headerTextField != null) {
        	dialog.append("Header text field = " + headerTextField.trim() + "\n");
        }
        
        if (subsystemSpecificDataOffset == 0L) {
            dialog.append("No subsystem specific data is stored in the file\n");	
        }
        else {
        	dialog.append("Subsystem specific data is stored at location " + subsystemSpecificDataOffset + "\n");
        }
        
        if (version >= 0) {
        	dialog.append("Version = " + version + "\n");
        }
        
        if (arrayName != null) {
        	dialog.append("Array name = " + arrayName + "\n");
        }
        
        if (fieldNames != null) {
        	for (i = 0; i < fieldNames.length; i++) {
        		if (fieldNames[i] != null) {
        			dialog.append("Field name " + (i+1) + " = " + fieldNames[i] + "\n");
        		}
        	}
        }
        
        if (sourceFile != null) {
        	dialog.append("Source file = " + sourceFile + "\n");
        }
    }
    
    public void setHeaderTextField(String headerTextField) {
    	this.headerTextField = headerTextField;
    }
    
    public void setLevel5Format(boolean level5Format) {
    	this.level5Format = level5Format;
    }
    
    public void setSubsystemSpecificDataOffset(long subsystemSpecificDataOffset) {
    	this.subsystemSpecificDataOffset = subsystemSpecificDataOffset;
    }
    
    public void setVersion(int version) {
    	this.version = version;
    }
    
    public void setArrayName(String arrayName) {
    	this.arrayName = arrayName;
    }
    
    public String getArrayName() {
    	return arrayName;
    }
    
    public void setFieldNames(String fieldNames[]) {
    	this.fieldNames = fieldNames;
    }
    
    public void setSourceFile(String sourceFile) {
    	this.sourceFile = sourceFile;
    }
}
