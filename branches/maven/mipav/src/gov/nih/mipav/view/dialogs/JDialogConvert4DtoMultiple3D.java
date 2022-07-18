package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Frame;


public class JDialogConvert4DtoMultiple3D extends JDialogScriptableBase implements AlgorithmInterface {

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogConvert4DtoMultiple3D() {}

    /**
     * Creates new dialog, but dialog is not visible.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogConvert4DtoMultiple3D(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
    }

    @Override
    public void callAlgorithm() {
        final int[] destExtents = new int[3];
        destExtents[0] = image.getExtents()[0];
        destExtents[1] = image.getExtents()[1];
        destExtents[2] = image.getExtents()[2];
        AlgorithmSubset subsetAlgo;
        final int tLength = image.getExtents()[3];
        final int maxNumDigits = String.valueOf(tLength).length();
        for (int i = 0; i < tLength; i++) {
            String padding = "";
            for (int z = 0; z < (maxNumDigits - String.valueOf(i).length()); z++) {
                padding += "0";
            }
            final String resultString = image.getImageName() + "_T" + padding + i;
            final ModelImage resultImage = new ModelImage(image.getType(), destExtents, resultString);
            subsetAlgo = new AlgorithmSubset(image, resultImage, AlgorithmSubset.REMOVE_T, i);
            subsetAlgo.run();
            new ViewJFrameImage(resultImage);
        }
        dispose();

    }

    @Override
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

    }

    @Override
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

    }

    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        // TODO Auto-generated method stub

    }

}
