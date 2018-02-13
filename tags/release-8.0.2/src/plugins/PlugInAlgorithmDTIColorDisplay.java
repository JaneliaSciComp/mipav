import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.ViewJComponentDTIImage;

import java.io.IOException;


/**
 * @author pandyan
 * 
 * This is the main dialog for the DTI Color Display
 * 
 * References: Developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group, Lin-Ching Chang D.Sc., Carlo
 * Pierpaoli MD Ph.D., and Lindsay Walker MS from the the NIH/NICHD/LIMB/STBB group and Olga Vogt from the
 * NIH/CIT/DCB/ISL/BIRSS group:
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL) Biomedical Imaging Research Services Section (BIRSS) Imaging
 * Sciences Laboratory (ISL) Division of Cumputational Bioscience (DCB) Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB) Laboratory of Integrative and Medical Biophysics (LIMB) National
 * Institute of Child Health & Humann Development National Institutes of Health
 * 
 * 
 * Publication Reference:
 * 
 * S. Pajevic and C. Pierpaoli, "Color Schemes to Represent the Orientation of Anisotropic Tissues from Diffusion Tensor
 * Data: Application to White Matter Fiber Tract Mapping in the Human Brain," Magnetic Resonance in Medicine, vol. 42,
 * no. 3, pp. 526-540, 1999
 * 
 */
public class PlugInAlgorithmDTIColorDisplay extends AlgorithmBase {

    /** eigenvector src image * */
    private ModelImage eigvecSrcImage;

    /** the eigvecSrcImage without the last 6 time volumes * */
    private ModelImage decImage;

    /** result image extents * */
    private int[] destExtents;

    /** extraced 3D Model Images from image...red Image is [0], green image is [1], blue image is [2] * */
    private ModelImage[] channelImages;

    /** handle for Algorithm Subset * */
    private AlgorithmSubset subsetAlgo;

    /** result image * */
    private ModelImage resultImage;

    /** ref to AlgorithmRGBConcat* */
    private AlgorithmRGBConcat mathAlgo;

    /** boolean for remap * */
    private boolean remapMode = false;

    /** component image * */
    private ViewJComponentDTIImage componentImage;

    /** constructor * */
    public PlugInAlgorithmDTIColorDisplay(ModelImage eigvecSrcImage) {
        this.eigvecSrcImage = eigvecSrcImage;
    }

    /** run algorithm * */
    public void runAlgorithm() {
        createModelImage();
        setCompleted(true);
    }

    /**
     * create result model image
     * 
     */
    private void createModelImage() {
        // create the dest extents of the dec image...the 4th dim will only have 3 as the value
        destExtents = new int[4];
        destExtents[0] = eigvecSrcImage.getExtents()[0];
        destExtents[1] = eigvecSrcImage.getExtents()[1];
        destExtents[2] = eigvecSrcImage.getExtents()[2];
        destExtents[3] = 3;

        decImage = new ModelImage(ModelStorageBase.FLOAT, destExtents, eigvecSrcImage.getImageName() + "_DEC");

        // buffer
        float[] buffer;

        // determine length of dec image
        int length = eigvecSrcImage.getExtents()[0] * eigvecSrcImage.getExtents()[1] * eigvecSrcImage.getExtents()[2]
                * 3;
        buffer = new float[length];

        // export eigvecSrcImage into buffer based on length
        try {
            eigvecSrcImage.exportData(0, length, buffer);
        } catch (IOException error) {
            System.out.println("IO exception");
            return;
        }

        // import resultBuffer into decImage
        try {
            decImage.importData(0, buffer, true);
        } catch (IOException error) {
            System.out.println("IO exception");

            return;
        }

        // extract dec image into channel images
        destExtents = new int[3];
        destExtents[0] = decImage.getExtents()[0];
        destExtents[1] = decImage.getExtents()[1];
        destExtents[2] = decImage.getExtents()[2];
        channelImages = new ModelImage[decImage.getExtents()[3]];
        for (int i = 0; i < decImage.getExtents()[3]; i++) {
            int num = i + 1;
            String resultString = decImage.getImageName() + "_Vol=" + num;
            channelImages[i] = new ModelImage(decImage.getType(), destExtents, resultString);
            subsetAlgo = new AlgorithmSubset(decImage, channelImages[i], AlgorithmSubset.REMOVE_T, i);
            subsetAlgo.setRunningInSeparateThread(false);
            subsetAlgo.run();
        }

        // set up result image
        resultImage = new ModelImage(ModelImage.ARGB_FLOAT, channelImages[0].getExtents(), eigvecSrcImage
                .getImageName()
                + "_ColorDisplay");

        // cocatenate channel images into an RGB image
        mathAlgo = new AlgorithmRGBConcat(channelImages[0], channelImages[1], channelImages[2], resultImage, remapMode,
                true, 255.0f, false);
        mathAlgo.setRunningInSeparateThread(false);
        mathAlgo.run();

        // copy core file info over
        FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[resultImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(resultImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(eigvecSrcImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(eigvecSrcImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(eigvecSrcImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(resultImage.getExtents());
            fileInfoBases[i].setImageOrientation(eigvecSrcImage.getFileInfo()[0].getImageOrientation());
            fileInfoBases[i].setAxisOrientation(eigvecSrcImage.getFileInfo()[0].getAxisOrientation());
            fileInfoBases[i].setOrigin(eigvecSrcImage.getFileInfo()[0].getOrigin());
            fileInfoBases[i].setPixelPadValue(eigvecSrcImage.getFileInfo()[0].getPixelPadValue());
            fileInfoBases[i].setPhotometric(eigvecSrcImage.getFileInfo()[0].getPhotometric());
            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);
            fileInfoBases[i].setFileDirectory(eigvecSrcImage.getFileInfo()[0].getFileDirectory());
        }

        resultImage.setFileInfo(fileInfoBases);
        resultImage.calcMinMax();

        finalize();

    }

    /**
     * get Result Image
     * 
     * @return resultImage
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * get component image
     * 
     * @return
     */
    public ViewJComponentDTIImage getComponentImage() {
        return componentImage;
    }

    /**
     * finalize
     */
    public void finalize() {
        if (channelImages[0] != null) {
            channelImages[0].disposeLocal();
        }
        if (channelImages[1] != null) {
            channelImages[1].disposeLocal();
        }
        if (channelImages[2] != null) {
            channelImages[2].disposeLocal();
        }
        if (decImage != null) {
            decImage.disposeLocal();
        }

        channelImages[0] = null;
        channelImages[1] = null;
        channelImages[2] = null;
        decImage = null;

    }

}
