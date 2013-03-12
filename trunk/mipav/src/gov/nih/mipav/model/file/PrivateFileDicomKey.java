package gov.nih.mipav.model.file;

/**
 * Associates a FileDicomKey with a particular publisher to denote the source of the FileDicomKey.  This publisher
 * is often printed in the (x, 0010) tag for any group.  When no publisher exists uses publisher NO_PUBLISHER
 * 
 * @author justinsenseney
 *
 */
public class PrivateFileDicomKey extends FileDicomKey {

	public static final String NO_PUBLISHER = "No publisher";
	
	/** The publisher for this tag. */
	private String publisher;

	public PrivateFileDicomKey(String keyStr) {
		this(NO_PUBLISHER, keyStr);
	}
	
	public PrivateFileDicomKey(String publisher, String keyStr) {
		super(keyStr);
		this.publisher = publisher;
	}

	public PrivateFileDicomKey(int group, int element)
			throws NumberFormatException {
		this(NO_PUBLISHER, group, element);
	}
	
	public PrivateFileDicomKey(String publisher, int group, int element)
			throws NumberFormatException {
		super(group, element);
		this.publisher = publisher;
	}
	
	public String getPublisher() {
		return publisher;
	}
	
	public void setPublisher(String publisher) {
		this.publisher = publisher;
	}

	/**
     * Returns the unique identifier.
     *
     * @return  The unique identifier.
     */
    public String toString() {
        return "Publisher: "+publisher+"\t"+key;
    }

}
