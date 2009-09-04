

public class PlugInDialogNINDSIdentificationTool extends PlugInDialogNINDSAnonymizationTool {

    /** rename grandparent dir name - moved here because it was removed from PlugInDialogNINDSAnonymizationTool. */
    protected boolean renameGrandParentDir;

    public PlugInDialogNINDSIdentificationTool(final boolean modal) {
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
        final String inputDirectoryPath = inputDirectoryTextField.getText().trim();
        final String outputDirectoryPath = outputDirectoryTextField.getText().trim();
        alg = new PlugInAlgorithmNINDSIdentificationTool(inputDirectoryPath, outputDirectoryPath, outputTextArea,
                errorMessageLabel, true, renameGrandParentDir, this, csvFilePath, newCSVFile);

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
