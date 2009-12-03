package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Converts a 3D dataset that is really a 4D dataset into a 4D dataset.
 */
public class AlgorithmConvert3Dto4D extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** resolution of the 3rd dimension. */
    private float resol3;

    /** resolution of the 4rd dimension. */
    private float resol4;

    /** units of measure for the 3rd dimension. */
    private int resolUnit3;

    /** units of measure for the 4th dimension. */
    private int resolUnit4;

    /** number of slices in the 3rd dimension. 4th dim length = sourceImage.3rd_dim / volumeLength */
    private int volumeLength = 1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new algorithm and sets source.
     *
     * @param  srcImg        source image model
     * @param  volumeLength  the 3D image will be chopped upto to volumes of this length volumeLength should divide
     *                       evenly (without remainder) into the 3rd dimension length of the original image.
     * @param  res3          resolution of the 3rd dimension
     * @param  res4          resolution of the 4rd dimension
     * @param  unit3         units of measure for the 3rd dimension
     * @param  unit4         units of measure for the 4rd dimension
     */
    public AlgorithmConvert3Dto4D(ModelImage srcImg, int volumeLength, float res3, float res4, int unit3, int unit4) {
        super(null, srcImg);

        this.volumeLength = volumeLength;
        resol3 = res3;
        resol4 = res4;

        resolUnit3 = unit3;
        resolUnit4 = unit4;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Returns result image.
     *
     * @return  destImage
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        
        convert3Dto4D();
    }

    /**
     * Converts a 3D dataset that is really a 4D dataset into a 4D dataset.
     */
    private void convert3Dto4D() {
        int t, z;
        int colorFactor;
        int length;
        float[] buffer;
        int[] extents;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = volumeLength;
        int tDim = srcImage.getExtents()[2] / volumeLength;

        FileInfoBase[] fileInfo;

        if (srcImage.isColorImage()) {
            colorFactor = 4;
        } else {
            colorFactor = 1;
        }

        try {
            extents = new int[4];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
            extents[3] = tDim;

            destImage = new ModelImage(srcImage.getType(), extents, srcImage.getImageName());
            length = colorFactor * xDim * yDim * zDim;
            buffer = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Converting from 3D to 4D ...");
        } catch (OutOfMemoryError e) {
            buffer = null;

            if (destImage != null) {
                destImage.disposeLocal();
            }

            destImage = null;
            System.gc();
            displayError("AlgorithmConvert3Dto4D: Out of memory");
            setCompleted(false);


            return;
        }

        int mod = length / 100; // mod is 1 percent of length


        for (t = 0; (t < tDim) && !threadStopped; t++) {
            fireProgressStateChanged(Math.round((float) t / (tDim - 1) * 100));

            try {
                srcImage.exportData(t * length, length, buffer);
            } catch (IOException error) {
                displayError("AlgorithmConvert3Dto4D: Image locked");

                if (destImage != null) {
                    destImage.disposeLocal();
                }

                destImage = null;
                buffer = null;
                setCompleted(false);


                return;
            }

            try {
                destImage.importData(t * length, buffer, false);
            } catch (IOException error) {
                displayError("AlgorithmConvert3Dto4D: Image(s) locked");

                if (destImage != null) {
                    destImage.disposeLocal();
                }

                destImage = null;
                setCompleted(false);


                return;
            }
        }

        if (threadStopped) {
            buffer = null;

            if (destImage != null) {
                destImage.disposeLocal();
            }

            destImage = null;
            finalize();

            return;
        }

        destImage.calcMinMax();
        
        destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());

        // copy all file infos.
        float[] resols = new float[4];
        int[] units = new int[4];
        float[] startLocs = new float[4];
        int sliceNumSrcImg;
        float zStartLoc;
        FileInfoBase[] fInfo;
        

        fileInfo = srcImage.getFileInfo();
        fInfo = ((FileInfoBase[]) (fileInfo.clone()));

        for (t = 0; t < tDim; t++) {

            for (z = 0; z < zDim; z++) {
                sliceNumSrcImg = (t * zDim) + z;
                destImage.setFileInfo(fInfo[sliceNumSrcImg], sliceNumSrcImg);

                // Fix these because they are now 4D

                for (int i = 0; i < 2; i++) {
                    resols[i] = destImage.getFileInfo(0).getResolutions()[i];
                    units[i] = destImage.getFileInfo(0).getUnitsOfMeasure()[i];
                    startLocs[i] = destImage.getFileInfo(0).getOrigin(i);
                }

                resols[2] = resol3;
                units[2] = resolUnit3;
                resols[3] = resol4;
                units[3] = resolUnit4;
                zStartLoc = (float) fileInfo[sliceNumSrcImg].getOrigin(2);

                if ((resolUnit3 >= 2) && (resolUnit3 <= 10)) {

                    // 3rd dimension is a length dimension
                    startLocs[2] = zStartLoc;

                    if ((resolUnit4 >= 2) && (resolUnit3 <= 10)) {

                        // 4th dimension is also a length dimension
                        startLocs[3] = zStartLoc;
                    } else {
                        startLocs[3] = t * resols[3];
                        // 4th dimension is a time dimension
                    }
                } else if ((resolUnit3 >= 11) && (resolUnit3 <= 17)) {

                    // 3rd dimension is a time dimension
                    startLocs[2] = t * resols[3];

                    // assume that 4th dimension is spatial
                    startLocs[3] = zStartLoc;
                } else {

                    // default
                    startLocs[2] = zStartLoc;
                    startLocs[3] = t * resols[3];
                }

                destImage.getFileInfo((t * zDim) + z).setResolutions(resols);
                destImage.getFileInfo((t * zDim) + z).setUnitsOfMeasure(units);

                // Why should extents be fileInfoBase ?
                destImage.getFileInfo((t * zDim) + z).setExtents(destImage.getExtents());
                destImage.getFileInfo((t * zDim) + z).setOrigin(startLocs);

                if (destImage.getFileInfo((t * zDim) + z) instanceof FileInfoXML) {
                    ((FileInfoImageXML) (destImage.getFileInfo((t * zDim) + z))).setMatrix(srcImage.getMatrix());
                }

            }

        }

        setCompleted(true);
    }

}
