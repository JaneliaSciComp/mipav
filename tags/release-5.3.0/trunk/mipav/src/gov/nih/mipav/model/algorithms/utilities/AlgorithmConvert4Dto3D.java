package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;


/**
 * The image is converted from a 4D to a 3D image.
 */
public class AlgorithmConvert4Dto3D extends AlgorithmBase {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new algorithm and sets source.
     *
     * @param  srcImg  source image model
     */
    public AlgorithmConvert4Dto3D(ModelImage srcImg) {
        super(null, srcImg);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        
        convert4Dto3D();
    }

    /**
     * Converts the 4D image to a 3D image.
     */
    private void convert4Dto3D() {

        int i;
        int length;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int newZDim = zDim * tDim;
        FileInfoBase[] fileInfo;
        int[] newExtents = new int[3];

        newExtents[0] = xDim;
        newExtents[1] = yDim;
        newExtents[2] = newZDim;
        length = xDim * yDim * newZDim;

        srcImage.createMask(length);
        srcImage.setExtents(newExtents);

        fileInfo = srcImage.getFileInfo();

        for (i = 0; i < newZDim; i++) {
            fileInfo[i].setExtents(newExtents);
        }

        
        setCompleted(true);
    }

}
