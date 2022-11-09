package gov.nih.mipav.view.dialogs;

import javax.swing.JTable;
import javax.swing.JTextField;

/**
 * Dialogs which implement this interface are eligible to use the JDialogDicomTagSelector for selecting DICOM tags within a given image file.
 * This interface gives the parent's tag text field which is changed by the JDialogDicomTagSelector.
 * 
 * @author senseneyj
 *
 */
public interface DicomTagSelectorImpl {
	public JTextField getTagListTextField(); 
	
	public JTable getTagTable();
}