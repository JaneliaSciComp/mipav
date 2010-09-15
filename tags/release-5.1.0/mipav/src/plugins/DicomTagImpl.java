import javax.swing.JTextField;


/**
 * Dialogs which implement this interface are eligible to use the TagEditorDialog for selecting DICOM tags within a given image file.
 * This interface gives the parents tag text field which is changed by the TagEditorDialog.
 * 
 * @author senseneyj
 *
 */
public interface DicomTagImpl {
	public JTextField getTagListTextField(); 
}
