import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import java.awt.event.*;
import java.awt.*;


/**
 * Brain statistics dialog.
 * @author Evan McCreedy
 */
public class PlugInDialogBrainStatistics extends JDialogBase implements AlgorithmInterface {
    private PlugInAlgorithmBrainStatistics brainAlgo = null;

    private ModelImage image;

    /**
     * Creates new dialog for brain classification statistics using a plugin.
     * @param theParentFrame Parent frame.
     * @param img Source image.
     */
    public PlugInDialogBrainStatistics(Frame theParentFrame, ModelImage img) {
        super(theParentFrame, true);
        if (img.getType() == ModelImage.BOOLEAN || img.isColorImage()) {
            MipavUtil.displayError("Source Image must NOT be Boolean or Color");
            dispose();
            return;
        }
        image = img;
        // don't actually do anything with the dialog. just run the algorithm
        setVisible(false);
        callAlgorithm();
    }

    /**
     * Placeholder.
     * @param event ActionEvent
     */
    public void actionPerformed(ActionEvent event) {}

    /**
     * Placeholder.
     * @param algo AlgorithmBase
     */
    public void algorithmPerformed(AlgorithmBase algo) {}

    /**
     * Start up the algorithm.
     */
    private void callAlgorithm() {
        brainAlgo = new PlugInAlgorithmBrainStatistics(image);
        brainAlgo.run();
    }
}
