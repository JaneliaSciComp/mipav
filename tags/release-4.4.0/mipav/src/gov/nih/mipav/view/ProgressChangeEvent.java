package gov.nih.mipav.view;

import javax.swing.event.ChangeEvent;

/**
 * ProgressChangeEvent is used to notify interested parties that progress
 * state has changed in the event source
 * @author Hailong Wang, Ph.D
 * @version 1.0
 * @created 30-Jun-2006 9:28:05 AM
 */
public class ProgressChangeEvent extends ChangeEvent{
    /** The message which will be shown with the progress bar */
	private String message;
    
    /** The title of the progress dialog */
	private String title;
    
    /** The value of the progress bar, the range is from 0~100 */
	private int value;

    /** The only constructor to create an instance of this <code>ProgressChangeEvent</code>
     * 
     * @param source   the event source object.
     * @param value    the value of progress bar.
     * @param title    the title progress dialog.
     * @param message  the message which is shown with the progress bar.
     */
	public ProgressChangeEvent(Object source, int value, String title, String message){
	    super(source);
        this.value = value;
        this.title = title;
        this.message = message;
	}

    /**
     * Returns the progress message
     * @return the progress message
     */
	public String getMessage(){
		return message;
	}

    /**
     * Returns the title of the progress dialog
     * @return the title of the progress dialog
     */
	public String getTitle(){
		return title;
	}

    /**
     * Returns the value of the progress bar.
     * @return the value of the progress bar.
     */
	public int getValue(){
		return value;
	}

}