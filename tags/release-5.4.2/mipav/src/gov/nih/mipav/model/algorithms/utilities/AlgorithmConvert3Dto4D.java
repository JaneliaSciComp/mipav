package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;


/**
 * Converts a 3D dataset that is really a 4D dataset into a 4D dataset.
 */
public class AlgorithmConvert3Dto4D extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

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

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new algorithm and sets source.
     * 
     * @param srcImg source image model
     * @param volumeLength the 3D image will be chopped upto to volumes of this length volumeLength should divide evenly
     *            (without remainder) into the 3rd dimension length of the original image.
     * @param res3 resolution of the 3rd dimension
     * @param res4 resolution of the 4rd dimension
     * @param unit3 units of measure for the 3rd dimension
     * @param unit4 units of measure for the 4rd dimension
     */
    public AlgorithmConvert3Dto4D(ModelImage srcImg, int volumeLength, float res3, float res4, int unit3, int unit4) {
        super(null, srcImg);

        this.volumeLength = volumeLength;
        resol3 = res3;
        resol4 = res4;

        resolUnit3 = unit3;
        resolUnit4 = unit4;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

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
     * @return destImage
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
        int[] extents;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = volumeLength;
        int tDim = srcImage.getExtents()[2] / volumeLength;

        FileInfoBase[] fileInfo;

        fireProgressStateChanged(srcImage.getImageName(), "Converting from 3D to 4D ...");

        try {
            extents = new int[4];
            extents[0] = xDim;
            extents[1] = yDim;
            extents[2] = zDim;
            extents[3] = tDim;

            destImage = ((ModelImage) srcImage.clone(srcImage.getImageName()));
            destImage.changeExtents(extents);

        } catch (OutOfMemoryError e) {

            if (destImage != null) {
                destImage.disposeLocal();
            }

            destImage = null;
            System.gc();
            displayError("AlgorithmConvert3Dto4D: Out of memory");
            setCompleted(false);

            return;
        }

        if (threadStopped) {

            if (destImage != null) {
                destImage.disposeLocal();
            }

            destImage = null;
            finalize();

            return;
        }

        destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());

        // copy all file infos.

        int sliceNumSrcImg;
        float zStartLoc;
        // FileInfoBase destFileInfo[] = null;
        float[] resols = new float[4];
        int[] units = new int[4];
        float[] startLocs = new float[4];
        int i;

        fileInfo = srcImage.getFileInfo();
        if (srcImage.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {

            FileInfoBase destFileInfo[] = null;
            int numInfos = zDim * tDim;
            FileInfoDicom oldDicomInfo = null;
            int j;
            double sliceResolution = 1.0;

            destFileInfo = new FileInfoBase[numInfos];
            int sliceCounter = 0; // Keeps track of every slice to populate tag

            // Most efficient way of creating DICOM tags for 4-D. Uses pointers based on srcimage dicom tags
            for (t = 0; t < tDim; t++) {
                for (z = 0; z < zDim; z++) {
                    j = (t * zDim) + z;
                    oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(j);
                    destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                            oldDicomInfo.getFileFormat(), (FileInfoDicom) srcImage.getFileInfo(j));

                    ((FileInfoDicom) destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type());

                    FileDicomTagTable newTagTable = ((FileInfoDicom) destFileInfo[j]).getTagTable();
                    if (newTagTable.getValue("0018,0088") != null) {
                        String sliceGapString = ((String) ((FileInfoDicom) destFileInfo[j]).getTagTable().getValue(
                                "0018,0088")).trim();
                        sliceResolution = new Double(sliceGapString.trim()).doubleValue();
                    }
                    fireProgressStateChanged( ( ( (100 * (t * 2))) / (destImage.getExtents()[2] + 1)));
                    resols[0] = srcImage.getFileInfo(0).getResolutions()[0];
                    resols[1] = srcImage.getFileInfo(0).getResolutions()[1];
                    resols[2] = srcImage.getFileInfo(0).getResolutions()[2];
                    resols[3] = (float)sliceResolution;
                    destFileInfo[sliceCounter].setResolutions(resols);
                    destFileInfo[sliceCounter].setExtents(destImage.getExtents());
                    destFileInfo[sliceCounter].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[0], 0);
                    destFileInfo[sliceCounter].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[1], 1);
                    destFileInfo[sliceCounter].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[2], 2);
                    destFileInfo[sliceCounter].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation()); 
                    ((FileInfoDicom) destFileInfo[j]).getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(j));
                    ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0011", new Short((short) xDim), 2); // columns
                    ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0010", new Short((short) yDim), 2); // rows
                    ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0020,0013",
                            Short.toString((short) (t + 1)), Short.toString((short) (t + 1)).length()); // instance
                                                                                                        // number
                    if (newTagTable.getValue("2001,1018") != null){
                        ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("2001,1018", null);        
                    }
                    sliceCounter++;

                }

            }

            destImage.setFileInfo(destFileInfo);

        }

        else {

            for (t = 0; t < tDim; t++) {

                for (z = 0; z < zDim; z++) {
                    sliceNumSrcImg = (t * zDim) + z;

                    // Fix these because they are now 4D

                    for (i = 0; i < 2; i++) {
                        resols[i] = destImage.getFileInfo(0).getResolutions()[i];
                        units[i] = destImage.getFileInfo(0).getUnitsOfMeasure()[i];
                        startLocs[i] = srcImage.getFileInfo(0).getOrigin(i);
                    }

                    resols[2] = resol3;
                    units[2] = resolUnit3;
                    resols[3] = resol4;
                    units[3] = resolUnit4;
                    zStartLoc = (float) fileInfo[sliceNumSrcImg].getOrigin(2);

                    if ( (resolUnit3 >= 2) && (resolUnit3 <= 10)) {

                        // 3rd dimension is a length dimension
                        startLocs[2] = zStartLoc;

                        if ( (resolUnit4 >= 2) && (resolUnit3 <= 10)) {

                            // 4th dimension is also a length dimension
                            startLocs[3] = zStartLoc;
                        } else {
                            startLocs[3] = t * resols[3];
                            // 4th dimension is a time dimension
                        }
                    } else if ( (resolUnit3 >= 11) && (resolUnit3 <= 17)) {

                        // 3rd dimension is a time dimension
                        startLocs[2] = t * resols[3];

                        // assume that 4th dimension is spatial
                        startLocs[3] = zStartLoc;
                    } else {

                        // default
                        startLocs[2] = zStartLoc;
                        startLocs[3] = t * resols[3];
                    }

                    destImage.getFileInfo( (t * zDim) + z).setResolutions(resols);
                    destImage.getFileInfo( (t * zDim) + z).setUnitsOfMeasure(units);

                    // Why should extents be fileInfoBase ?
                    destImage.getFileInfo( (t * zDim) + z).setExtents(destImage.getExtents());
                    destImage.getFileInfo( (t * zDim) + z).setOrigin(startLocs);
                    destImage.getFileInfo( (t * zDim) + z).setAxisOrientation(
                            srcImage.getFileInfo(0).getAxisOrientation());
                    if (destImage.getFileInfo( (t * zDim) + z) instanceof FileInfoXML) {
                        ((FileInfoImageXML) (destImage.getFileInfo( (t * zDim) + z))).setMatrix(srcImage.getMatrix());
                    }

                }

            }

            destImage.calcMinMax();
            destImage.setImageOrientation(srcImage.getImageOrientation());
        }
        setCompleted(true);
    }

}
