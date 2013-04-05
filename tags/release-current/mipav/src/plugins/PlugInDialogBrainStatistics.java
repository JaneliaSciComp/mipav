import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;


/**
 * Brain statistics dialog.
 *
 * @author  Evan McCreedy
 */
public class PlugInDialogBrainStatistics extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4310604464314852560L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private PlugInAlgorithmBrainStatistics brainAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for brain classification statistics using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  img             Source image.
     */
    public PlugInDialogBrainStatistics(Frame theParentFrame, ModelImage img) {
        super(theParentFrame, false);

        if ((img.getType() == ModelImage.BOOLEAN) || img.isColorImage()) {
            MipavUtil.displayError("Source Image must NOT be Boolean or Color");
            dispose();

            return;
        }

        image = img;

        // don't actually do anything with the dialog. just run the algorithm
        setVisible(false);
        callAlgorithm();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Placeholder.
     *
     * @param  algo  AlgorithmBase
     */
    public void algorithmPerformed(AlgorithmBase algo) { }

    /**
     * Start up the algorithm.
     */
    protected void callAlgorithm() {
        brainAlgo = new PlugInAlgorithmBrainStatistics(image);
        createProgressBar(image.getImageName(), "Calculating brain statistics ...", brainAlgo);
        brainAlgo.run();
    }
}
