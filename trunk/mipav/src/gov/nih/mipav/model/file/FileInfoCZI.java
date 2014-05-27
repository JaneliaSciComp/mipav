package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;


/**
 * File info storage container.
 */
public class FileInfoCZI extends FileInfoBase {
	
	//~ Static fields/initializers -------------------------------------------------------------------------------------
	private String focusPosition = null;
	private String acquisitionTime = null;
	private String stageXPosition = null;
	private String stageYPosition = null;
	private String validBitsPerPixel = null;
	private double timeStamps[] = null;
	private String imageName = null;
	private String author = null;
	private String userName = null;
	private String subType = null;
	private String title = null;
	private String creationDate = null;
	private String description = null;
	private String thumbnail = null;
	private String comment = null;
	private String rating = null;
	private String keywords = null;
	private String ID = null;
	private String displayName = null;
	private String firstName = null;
	private String middleName = null;
	private String lastName = null;
	private String email = null;
	private String institution = null;
	private String experimenterName = null;
	private String phone = null;
	private String fax = null;
	private String address = null;
	private String city = null;
	private String country = null;
	private String state = null;
		
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoCZI(String name, String directory, int format) {
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
        
        if (imageName != null) {
        	dialog.append("Image name = " + imageName + "\n");
        }
        
        if (author != null) {
        	dialog.append("Author = " + author + "\n");
        }
        
        if (userName != null) {
        	dialog.append("User name = " + userName + "\n");
        }
        
        if (subType != null) {
        	dialog.append("SubType = " + subType + "\n");
        }
        
        if (title != null) {
        	dialog.append("Title = " + title + "\n");
        }
        
        if (creationDate != null) {
        	dialog.append("Creation date = " + creationDate + "\n");
        }
        
        if (description != null) {
        	dialog.append("Description = " + description + "\n");
        }
        
        if (thumbnail != null) {
        	dialog.append("Thumbnail = " + thumbnail + "\n");
        }
        
        if (comment != null) {
        	dialog.append("Comment = " + comment + "\n");
        }
        
        if (rating != null) {
        	dialog.append("Rating = " + rating + "\n");
        }
        
        if (keywords != null) {
        	dialog.append("Keywords = " + keywords + "\n");
        }
        
        if (ID != null) {
        	dialog.append("ID = " + ID + "\n");
        }
        
        if (displayName != null) {
        	dialog.append("Display name = " + displayName + "\n");
        }
        
        if (firstName != null) {
        	dialog.append("First name = " + firstName + "\n");
        }
        
        if (middleName != null) {
        	dialog.append("Middle name = " + middleName + "\n");
        }
        
        if (lastName != null) {
        	dialog.append("Last name = " + lastName + "\n");
        }
        
        if (email != null) {
        	dialog.append("Email = " + email + "\n");
        }
        
        if (institution != null) {
        	dialog.append("Institution = " + institution + "\n");
        }
        
        if (experimenterName != null) {
        	dialog.append("Experimenter name = " + experimenterName + "\n");
        }
        
        if (phone != null) {
        	dialog.append("Phone = " + phone + "\n");
        }
        
        if (fax != null) {
        	dialog.append("Fax = " + fax + "\n");
        }
        
        if (address != null) {
        	dialog.append("Address = " + address + "\n");
        }
        
        if (city != null) {
        	dialog.append("City = " + city + "\n");
        }
        
        if (state != null) {
        	dialog.append("State = " + state + "\n");
        }
        
        if (country != null) {
        	dialog.append("Country = " + country + "\n");
        }
        
        if (focusPosition != null) {
        	dialog.append("Focus position in micrometers = " + focusPosition + "\n");
        }
        
        if (acquisitionTime != null) {
        	dialog.append("Acquisition time = " + acquisitionTime + "\n");
        }
        
        if (stageXPosition != null) {
        	dialog.append("Stage axis X position in micrometers = " + stageXPosition + "\n");
        }
        
        if (stageYPosition != null) {
        	dialog.append("Stage axis Y position in micrometers = " + stageYPosition + "\n");
        }
        
        if (validBitsPerPixel != null) {
        	dialog.append("Valid bits per pixel = " + validBitsPerPixel + "\n");
        }
        
        if (timeStamps != null) {
        	dialog.append("Time stamps in seconds relative to the start time of acquisition:\n");
        	for (i = 0; i < timeStamps.length; i++) {
        	    dialog.append("Time stamp  " + i + ":     " + timeStamps[i] + "\n");	
        	}
        }
    }
    
    /**
     * 
     * @param focusPosition
     */
    public void setFocusPosition(String focusPosition) {
    	this.focusPosition = focusPosition;
    }
    
    /**
     * 
     * @param acquisitionTime
     */
    public void setAcquisitionTime(String acquisitionTime) {
    	this.acquisitionTime = acquisitionTime;
    }
    
    /**
     * 
     * @param stageXPosition
     */
    public void setStageXPosition(String stageXPosition) {
    	this.stageXPosition = stageXPosition;
    }
    
    /**
     * 
     * @param stageYPosition
     */
    public void setStageYPosition(String stageYPosition) {
    	this.stageYPosition = stageYPosition;
    }
    
    /**
     * 
     * @param validBitsPerPixel
     */
    public void setValidBitsPerPixel(String validBitsPerPixel) {
    	this.validBitsPerPixel = validBitsPerPixel;
    }
    
    /**
     * 
     * @param timeStamps
     */
    public void setTimeStamps(double timeStamps[]) {
    	this.timeStamps = timeStamps;
    }
    
    /**
     * 
     * @param imageName
     */
    public void setImageName(String imageName) {
    	this.imageName = imageName;
    }
    
    /**
     * 
     * @param author
     */
    public void setAuthor(String author) {
    	this.author = author;
    }
    
    /**
     * 
     * @param userName
     */
    public void setUserName(String userName) {
    	this.userName = userName;
    }
    
    /**
     * 
     * @param subType
     */
    public void setSubType(String subType) {
    	this.subType = subType;
    }
    
    /**
     * 
     * @param title
     */
    public void setTitle(String title) {
    	this.title = title;
    }
    
    /**
     * 
     * @param creationDate
     */
    public void setCreationDate(String creationDate) {
    	this.creationDate = creationDate;
    }
    
    /**
     * 
     * @param description
     */
    public void setDescription(String description) {
    	this.description = description;
    }
    
    /**
     * 
     * @param thumbnail
     */
    public void setThumbnail(String thumbnail) {
    	this.thumbnail = thumbnail;
    }
    
    /**
     * 
     * @param comment
     */
    public void setComment(String comment) {
    	this.comment = comment;
    }
    
    /**
     * 
     * @param rating
     */
    public void setRating(String rating) {
    	this.rating = rating;
    }
    
    /**
     * 
     * @param keywords
     */
    public void setKeywords(String keywords) {
    	this.keywords = keywords;
    }
    
    /**
     * 
     * @param ID
     */
    public void setID(String ID) {
    	this.ID = ID;
    }
    
    /**
     * 
     * @param displayName
     */
    public void setDisplayName(String displayName) {
    	this.displayName = displayName;
    }
    
    /**
     * 
     * @param firstName
     */
    public void setFirstName(String firstName) {
    	this.firstName = firstName;
    }
    
    /**
     * 
     * @param middleName
     */
    public void setMiddleName(String middleName) {
    	this.middleName = middleName;
    }
    
    /**
     * 
     * @param lastName
     */
    public void setLastName(String lastName) {
    	this.lastName = lastName;
    }
    
    /**
     * 
     * @param email
     */
    public void setEmail(String email) {
    	this.email = email;
    }
    
    /**
     * 
     * @param institution
     */
    public void setInstitution(String institution) {
    	this.institution = institution;
    }
    
    /**
     * 
     * @param experimenterName
     */
    public void setExperimenterName(String experimenterName) {
    	this.experimenterName = experimenterName;
    }
    
    /**
     * 
     * @param phone
     */
    public void setPhone(String phone) {
    	this.phone = phone;
    }
    
    /**
     * 
     * @param fax
     */
    public void setFax(String fax) {
    	this.fax = fax;
    }
    
    /**
     * 
     * @param address
     */
    public void setAddress(String address) {
    	this.address = address;
    }
    
    /**
     * 
     * @param city
     */
    public void setCity(String city) {
    	this.city = city;
    }
    
    /**
     * 
     * @param state
     */
    public void setState(String state) {
    	this.state = state;
    }
    
    /**
     * 
     * @param country
     */
    public void setCountry(String country) {
    	this.country = country;
    }
}