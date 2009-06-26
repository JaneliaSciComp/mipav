import gov.nih.mipav.view.Preferences;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.JFileChooser;


public class PlugInDialogNINDSIdentificationTool extends
		PlugInDialogNINDSAnonymizationTool {
	
	public PlugInDialogNINDSIdentificationTool(boolean modal) {
        super(modal);
        subCustomize();
    }
	
	private void subCustomize() {
		setTitle("NINDS Identification Tool " + " v1.0");
	}

	/**
     * call algorithm
     */
    protected void callAlgorithm() {
        String inputDirectoryPath = inputDirectoryTextField.getText().trim();
        String outputDirectoryPath = outputDirectoryTextField.getText().trim();
        alg = new PlugInAlgorithmNINDSIdentificationTool(inputDirectoryPath, outputDirectoryPath, outputTextArea,
                errorMessageLabel, true, renameGrandParentDir, this, csvFilePath,newCSVFile);

        alg.addListener(this);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still
            // have user interface work fast.
            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                // MipavUtil.displayError("A thread is already running on this object");
                errorMessageLabel.setText("A thread is already running on this object");
            }
        } else {
            alg.start();
        }

    }
}
