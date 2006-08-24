package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;


/**
 * The class reads MGH files.  The 4 by 4
 * matrix in MGH transforms i,j,k indexes to (x,y,z) coordinates where +x = Right, +y = Anterior, +z = Superior. In
 * MIPAV the 4 by 4 matrix does not imply the axis orientations.</p>
 * 
 * The MGH definition is:
 * [x]    [xr yr zr] [       xsize * i]   [-(width/2.0)*xisze]
 * [y] =  [xa ya za] [       ysize * j] + [-(height/2.0)*ysize]
 * [z]    [xs ys zs] [       zsize * k]   [-(depth/2.0)*zsize]
 * Now in going to MIPAV a change must occur.
 * MGH has X L->R and Y P->A while MIPAV uses X R->L and Y A->P, so this would cause
 * xr, yr, zr, -(width/2.0)*xsize, xa, ya, za, and -(height/2.0)*ysize to be multiplied by -1.
 
 </p>
 *
 * @see  FileIO
 * @see  FileInfoMGH
 * @see  FileRaw
 */

public class FileMGH extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] axisOrientation;

    /** DOCUMENT ME! */
    private short bitpix;

    /** DOCUMENT ME! */
    private byte[] bufferByte = null;

    /** DOCUMENT ME! */
    private short coord_code;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private File fileHeader;

    /** DOCUMENT ME! */
    private FileInfoMGH fileInfo = null;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private int freq_dim = 0;

    /** A 284 byte header always precedes the data buffer
     *  In version 1 if the goodRASFlag <= 0, then only
     *  the first 30 bytes are read.  If the goodRASFlag > 0,
     *  then the first 90 bytes are read */
    private int headerSize = 284;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private double imageMax;

    /** DOCUMENT ME! */
    private double imageMin;

    /** DOCUMENT ME! */
    private String intentName;

    /** DOCUMENT ME! */
    private char[] magic = new char[4];

    /** DOCUMENT ME! */
    private TransMatrix matrix = new TransMatrix(4);

    /** DOCUMENT ME! */
    private double newMax;

    /** DOCUMENT ME! */
    private double newMin;

    /** DOCUMENT ME! */
    private float[] newOrigin = new float[3];

    /** DOCUMENT ME! */
    private boolean oneFile;

    /** DOCUMENT ME! */
    private boolean oneFileStorage;

    /** DOCUMENT ME! */
    private float[] origin;
    
    private float[] LPSOrigin;

    /** DOCUMENT ME! */
    private int phase_dim = 0;

    /** DOCUMENT ME! */
    private float[] pixdim;

    /** DOCUMENT ME! */
    private float qfac;

    /** DOCUMENT ME! */
    private short qform_code;

    /** DOCUMENT ME! */
    private float qoffset_x;

    /** DOCUMENT ME! */
    private float qoffset_y;

    /** DOCUMENT ME! */
    private float qoffset_z;

    /** DOCUMENT ME! */
    private float quatern_a;

    /** DOCUMENT ME! */
    private float quatern_b;

    /** DOCUMENT ME! */
    private float quatern_c;

    /** DOCUMENT ME! */
    private float quatern_d;

    /** DOCUMENT ME! */
    private double r00, r01, r02;

    /** DOCUMENT ME! */
    private double r10, r11, r12;

    /** DOCUMENT ME! */
    private double r20, r21, r22;

    /** DOCUMENT ME! */
    private float[] resolutions;

    /** DOCUMENT ME! */
    private float scl_inter;

    /** DOCUMENT ME! */
    private float scl_slope;

    /** DOCUMENT ME! */
    private short sform_code;

    /** DOCUMENT ME! */
    private boolean showProgress = true;

    /** DOCUMENT ME! */
    private int slice_dim = 0;

    /** DOCUMENT ME! */
    private byte sliceCode;

    /** DOCUMENT ME! */
    private float sliceDuration;

    /** DOCUMENT ME! */
    private short sliceEnd;

    /** DOCUMENT ME! */
    private short sliceStart;

    /** DOCUMENT ME! */
    private short sourceBitPix;

    /** DOCUMENT ME! */
    private short sourceType;

    /** DOCUMENT ME! */
    private int spaceUnits = FileInfoNIFTI.NIFTI_UNITS_UNKNOWN;

    /** DOCUMENT ME! */
    private float[] srow_x;

    /** DOCUMENT ME! */
    private float[] srow_y;

    /** DOCUMENT ME! */
    private float[] srow_z;

    /** DOCUMENT ME! */
    private int timeUnits = FileInfoNIFTI.NIFTI_UNITS_UNKNOWN;

    /** DOCUMENT ME! */
    private float tOffset;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private float vox_offset = 0.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  _UI    User interface.
     * @param  fName  File name.
     * @param  fDir   File directory.
     * @param  show   Flag for showing the progress bar.
     */
    public FileMGH(ViewUserInterface _UI, String fName, String fDir, boolean show) {
        UI = _UI;
        fileName = fName;
        fileDir = fDir;
        showProgress = show;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

   

    /**
     * Returns the FileInfoMGH read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoMGH getFileInfo() {
        return fileInfo;
    }

    /**
     * Reads the NIFTI header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoNIFTI
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
        int i, j;
        int index;
        String fileHeaderName;
        boolean endianess;
        int[] niftiExtents = new int[5];
        int numDims = 0;
        float intentP1;
        float intentP2;
        float intentP3;
        short intentCode;
        int unitMeasure;
        int spatialDims;
        double a, b, c, d;

        bufferByte = new byte[headerSize];

        // index         = fileName.toLowerCase().indexOf(".img");
        index = fileName.lastIndexOf(".");

        if (fileName.substring(index + 1).equalsIgnoreCase("nii")) {
            oneFileStorage = true;
            fileHeaderName = fileName;
        } else {
            oneFileStorage = false;
            fileHeaderName = fileName.substring(0, index) + ".hdr";
        }

        fileHeader = new File(fileDir + fileHeaderName);

        if (fileHeader.exists() == false) {
            fileHeaderName = fileName.substring(0, index) + ".HDR";
            fileHeader = new File(fileDir + fileHeaderName);

            if (fileHeader.exists() == false) {
                return false;
            }
        }

        // Tagged for removal - Matt 4/17/2003
        // if (fileInfo == null) { // if the file info does not yet exist: make it
        // fileInfo = new FileInfoNIFTI(imageFileName, fileDir, FileBase.NIFTI);
        // if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) { // Why 3/20/2001
        // throw (new IOException(" NIFTI header file error"));
        // }
        // }

        raFile = new RandomAccessFile(fileHeader, "r");
        raFile.read(bufferByte);
        raFile.close();

        fileInfo.setEndianess(BIG_ENDIAN);
        fileInfo.setSizeOfHeader(getBufferInt(bufferByte, 0, BIG_ENDIAN));

        if (fileInfo.getSizeOfHeader() != headerSize) { // Set the endianess based on header size = 348 Big Endian
            fileInfo.setEndianess(LITTLE_ENDIAN); // or 1,543,569,408 Little endian
            fileInfo.setSizeOfHeader(getBufferInt(bufferByte, 0, LITTLE_ENDIAN));
            Preferences.debug("FileNIFTI:readHeader Endianess = Little endian.\n", 2);
        } else {
            Preferences.debug("FileNIFTI:readHeader Endianess = Big endian.\n", 2);
        }

        if (fileInfo.getSizeOfHeader() != headerSize) {
            Preferences.debug("FileNIFTI:readHeader NIFTI header length != 348.\n", 2);

            return false;
        }

        endianess = fileInfo.getEndianess();

        // bufferByte[39] is the dim_info byte
        freq_dim = (int) (bufferByte[39] & 0x03);

        switch (freq_dim) {

            case 0:
                Preferences.debug("No frequency encoding direction is present\n");
                break;

            case 1:
                Preferences.debug("Frequency encoding in the x direction\n");
                break;

            case 2:
                Preferences.debug("Frequency encoding in the y direction\n");
                break;

            case 3:
                Preferences.debug("Frequency encoding in the z direction\n");
                break;
        }

        fileInfo.setFreqDim(freq_dim);
        phase_dim = (int) ((bufferByte[39] >> 2) & 0x03);

        switch (phase_dim) {

            case 0:
                Preferences.debug("No phase encoding direction is present\n");
                break;

            case 1:
                Preferences.debug("Phase encoding in the x direction\n");
                break;

            case 2:
                Preferences.debug("Phase encoding in the y direction\n");
                break;

            case 3:
                Preferences.debug("Phase encoding in the z direction\n");
                break;
        }

        fileInfo.setPhaseDim(phase_dim);
        slice_dim = (int) ((bufferByte[39] >> 4) & 0x03);

        switch (slice_dim) {

            case 0:
                Preferences.debug("No slice acquisition direction is present\n");
                break;

            case 1:
                Preferences.debug("Slice acquisition in the x direction\n");
                break;

            case 2:
                Preferences.debug("Slice acquisition in the y direction\n");
                break;

            case 3:
                Preferences.debug("Slice acquisition in the z direction\n");
                break;
        }

        fileInfo.setSliceDim(slice_dim);

        // In NIFTI always have x,y,z as dimensions 1, 2, and 3, t as dimension 4,
        // and any other dimensions as 5, 6, and 7
        // so that a x, y, t image would have dim[3] = 1
        int dims = getBufferShort(bufferByte, 40, endianess);
        Preferences.debug("FileNIFTI:readHeader. Number of dimensions = " + dims + "\n", 2);

        for (i = 0; i < dims; i++) {
            niftiExtents[i] = getBufferShort(bufferByte, 42 + (2 * i), endianess);
            Preferences.debug("FileNIFTI:readHeader. Dimension " + (i + 1) + " = " + niftiExtents[i] + "\n", 2);

            if (niftiExtents[i] > 1) {
                numDims++;
            }
        }

        spatialDims = 0;

        for (i = 0; i < Math.min(dims, 3); i++) {

            if (niftiExtents[i] > 1) {
                spatialDims++;
            }
        }

        int[] extents = new int[numDims];

        for (i = 0, j = 0; i < dims; i++) {

            if (niftiExtents[i] > 1) {
                extents[j++] = niftiExtents[i];
            }
        }

        fileInfo.setExtents(extents);
        intentP1 = getBufferFloat(bufferByte, 56, endianess);
        fileInfo.setIntentP1(intentP1);
        Preferences.debug("FileNIFTI:readHeader. intentP1 = " + fileInfo.getIntentP1() + "\n");
        intentP2 = getBufferFloat(bufferByte, 60, endianess);
        fileInfo.setIntentP2(intentP2);
        Preferences.debug("FileNIFTI:readHeader. statPar2 = " + fileInfo.getIntentP2() + "\n");
        intentP3 = getBufferFloat(bufferByte, 64, endianess);
        fileInfo.setIntentP3(intentP3);
        Preferences.debug("FileNIFTI:readHeader. intentP3 = " + fileInfo.getIntentP3() + "\n");
        intentCode = getBufferShort(bufferByte, 68, endianess);
        fileInfo.setIntentCode(intentCode);
        Preferences.debug("FileNIFTI:readHeader. intentCode = " + intentCode + "\n");

        switch (intentCode) {

            case FileInfoNIFTI.NIFTI_INTENT_NONE:
                Preferences.debug("No intention\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CORREL:
                Preferences.debug("Correlation coefficient R\n");
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has the Correlation Coefficient R\n");
                    Preferences.debug("in the first data plane and degrees of freedom in the\n");
                    Preferences.debug("second data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST:
                Preferences.debug("Student t statistic\n");
                Preferences.debug("Degress of freedom = " + Math.round(intentP1) + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has the Student t statistic\n");
                    Preferences.debug("in the first data plane and degrees of freedom in the\n");
                    Preferences.debug("second data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST:
                Preferences.debug("Fisher F statistic\n");
                Preferences.debug("Numerator degrees of freedom = " + Math.round(intentP1) + "\n");
                Preferences.debug("Denominator degrees of freedom = " + Math.round(intentP2) + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Fisher F statistic\n");
                    Preferences.debug("in the first data plane, numerator degrees of freedom in the\n");
                    Preferences.debug("second data plane, and denominator degrees of freedom in the\n");
                    Preferences.debug("third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_ZSCORE:
                Preferences.debug("Standard normal - N(0,1) distributed\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ:
                Preferences.debug("Chi - squared\n");
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has Chi-squared\n");
                    Preferences.debug("in the first data plane and degrees of freedom in the\n");
                    Preferences.debug("second data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_BETA:
                Preferences.debug("Beta distribution\n");
                Preferences.debug("a parameter = " + intentP1 + "\n");
                Preferences.debug("b parameter = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Beta distribution\n");
                    Preferences.debug("in the first data plane, the a parameter in the\n");
                    Preferences.debug("second data plane, and the b parameter in the third\n");
                    Preferences.debug("third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_BINOM:
                Preferences.debug("Binomial distribution\n");
                Preferences.debug("Number of trials = " + Math.round(intentP1) + "\n");
                Preferences.debug("Probability per trial = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Binomial distribution\n");
                    Preferences.debug("in the first data plane, the number of trials in the\n");
                    Preferences.debug("second data plane, and the probability per trial in the\n");
                    Preferences.debug("third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_GAMMA:
                Preferences.debug("Gamma with PDF = x^(shape-1) * exp(-Scale*x)\n");
                Preferences.debug("for x >= 0\n");
                Preferences.debug("Shape = " + intentP1 + "\n");
                Preferences.debug("Scale = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has Gamma\n");
                    Preferences.debug("in the first data plane, shape in the\n");
                    Preferences.debug("second data plane, and scale in the third\n");
                    Preferences.debug("data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_POISSON:
                Preferences.debug("Poisson distribution\n");
                Preferences.debug("Mean = " + intentP1 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has the Poisson distribution\n");
                    Preferences.debug("in the first data plane and the mean in the\n");
                    Preferences.debug("second data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_NORMAL:
                Preferences.debug("Normal distribution\n");
                Preferences.debug("Mean = " + intentP1 + "\n");
                Preferences.debug("Standard deviation = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Normal distribution\n");
                    Preferences.debug("in the first data plane, the mean in the\n");
                    Preferences.debug("second data plane, and the standard deviation\n");
                    Preferences.debug("in the third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST_NONC:
                Preferences.debug("Noncentral F statistic\n");
                Preferences.debug("Numerator degrees of freedom = " + Math.round(intentP1) + "\n");
                Preferences.debug("Denominator degrees of freedom = " + Math.round(intentP2) + "\n");
                Preferences.debug("Numerator noncentrality parameter= " + intentP3 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 4)) {
                    Preferences.debug("Dimension " + numDims + " has the Noncentral F statistic\n");
                    Preferences.debug("in the first data plane, numerator degrees of freedom in the\n");
                    Preferences.debug("second data plane, denominator degrees of freedom in the\n");
                    Preferences.debug("third data plane, and the numerator noncentrality parameter\n");
                    Preferences.debug("in the fourth data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ_NONC:
                Preferences.debug("Noncentral chi-squared statistic\n");
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n");
                Preferences.debug("Noncentrality parameter = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Noncentral chi-squared\n");
                    Preferences.debug("statistic in the first data plane, degrees of freedom in the\n");
                    Preferences.debug("second data plane, and the noncentrality parameter in the\n");
                    Preferences.debug("third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOGISTIC:
                Preferences.debug("Logistic distribution\n");
                Preferences.debug("Location = " + intentP1 + "\n");
                Preferences.debug("Scale = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Logistic distribution\n");
                    Preferences.debug("in the first data plane, location in the second\n");
                    Preferences.debug("data plane, and scale in the third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_LAPLACE:
                Preferences.debug("Laplace distribution\n");
                Preferences.debug("Location = " + intentP1 + "\n");
                Preferences.debug("Scale = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Laplace distribution\n");
                    Preferences.debug("in the first data plane, location in the second\n");
                    Preferences.debug("data plane, and scale in the third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_UNIFORM:
                Preferences.debug("Uniform distribution\n");
                Preferences.debug("Start = " + intentP1 + "\n");
                Preferences.debug("End = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Uniform distribution\n");
                    Preferences.debug("in the first data plane, start in the second data\n");
                    Preferences.debug("plane, and end in the third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST_NONC:
                Preferences.debug("Noncentral t statistic\n");
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n");
                Preferences.debug("Noncentrality parameter = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Noncentral t statistic\n");
                    Preferences.debug("in the first data plane, degrees of freedom in the\n");
                    Preferences.debug("second data plane, and the noncentrality parameter in\n");
                    Preferences.debug("third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_WEIBULL:
                Preferences.debug("Weibull distribution\n");
                Preferences.debug("Location = " + intentP1 + "\n");
                Preferences.debug("Scale = " + intentP2 + "\n");
                Preferences.debug("Power = " + intentP3 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 4)) {
                    Preferences.debug("Dimension " + numDims + " has the Weibull distribution\n");
                    Preferences.debug("in the first data plane, location in the second\n");
                    Preferences.debug("data plane, scale in the third data plane, and power\n");
                    Preferences.debug("in the fourth data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHI:
                Preferences.debug("Chi distribution\n");
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n");

                int p1 = Math.round(intentP1);
                if (p1 == 1) {
                    Preferences.debug("dof = 1 = half normal distribution\n");
                } else if (p1 == 2) {
                    Preferences.debug("dof = 2 = Rayleigh distribution\n");
                } else if (p1 == 3) {
                    Preferences.debug("dof = 3 = Maxwell-Boltzmann distribution\n");
                }

                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has the Chi distribution\n");
                    Preferences.debug("in the first data plane and degrees of freedom in the\n");
                    Preferences.debug("second data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_INVGAUSS:
                Preferences.debug("Inverse Gaussian\n");
                Preferences.debug("Mu = " + intentP1 + "\n");
                Preferences.debug("Lambda = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Inverse Gaussian\n");
                    Preferences.debug("in the first data plane, mu in the second data\n");
                    Preferences.debug("plane, and lambda in the third data plane\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_EXTVAL:
                Preferences.debug("Extreme value type 1\n");
                Preferences.debug("Location = " + intentP1 + "\n");
                Preferences.debug("Scale = " + intentP2 + "\n");
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has Extreme value type 1\n");
                    Preferences.debug("in the first data plane, location in the second\n");
                    Preferences.debug("data plane, and scale in the third data plane\n");
                }

            case FileInfoNIFTI.NIFTI_INTENT_PVAL:
                Preferences.debug("Data is a p-value\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOGPVAL:
                Preferences.debug("Data is ln(p-value)\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOG10PVAL:
                Preferences.debug("Data is log10(p-value)\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_ESTIMATE:
                Preferences.debug("Each voxel is an estimate of some parameter\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LABEL:
                Preferences.debug("Each voxel is an index into some set of labels\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_NEURONAME:
                Preferences.debug("Each voxel is an index into the NeuroNames label set\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_GENMATRIX:
                Preferences.debug("Each voxel has a M x N matrix\n");
                MipavUtil.displayError("MIPAV cannot handle voxels with M X N matrices");

                return false;

            case FileInfoNIFTI.NIFTI_INTENT_SYMMATRIX:
                Preferences.debug("Each voxel has a NxN symmetric matrix\n");
                MipavUtil.displayError("MIPAV cannot handle voxels with NxN symmetric matrices");

                return false;

            case FileInfoNIFTI.NIFTI_INTENT_DISPVECT:
                Preferences.debug("Each voxel has a displacement vector\n");
                MipavUtil.displayError("MIPAV cannot handle voxels with displacement vectors");

                return false;

            case FileInfoNIFTI.NIFTI_INTENT_VECTOR:
                Preferences.debug("Each voxel has a vector\n");
                MipavUtil.displayError("MIPAV cannot handle voxels with vectors");

                return false;

            case FileInfoNIFTI.NIFTI_INTENT_POINTSET:
                Preferences.debug("Each voxel has a spatial coordinate\n");
                MipavUtil.displayError("MIPAV cannot handle voxels with spatial coordinates");

                return false;

            case FileInfoNIFTI.NIFTI_INTENT_TRIANGLE:
                Preferences.debug("Each voxel has a triple of indexes\n");
                MipavUtil.displayError("MIPAV cannot handle voxels with a triple of indexes");

                return false;

            case FileInfoNIFTI.NIFTI_INTENT_QUATERNION:
                Preferences.debug("Each voxel has a quarternion\n");
                MipavUtil.displayError("MIPAV cannot handle voxels with quarternions");

                return false;

            case FileInfoNIFTI.NIFTI_INTENT_DIMLESS:
                Preferences.debug("Each voxel is a dimensionless value\n");
                break;

            default:
                Preferences.debug("intentCode = " + intentCode + " is not a recognized value\n");
        }

        sourceType = getBufferShort(bufferByte, 70, endianess);
        fileInfo.setSourceType(sourceType);
        Preferences.debug("Original unscaled source data type:\n");

        switch (sourceType) {

            case FileInfoNIFTI.DT_UNKNOWN:
                Preferences.debug("Unknown data type\n");
                MipavUtil.displayError("Mipav cannot handle data type DT_UNKNOWN");

                return false;

            case FileInfoNIFTI.DT_BINARY:
                Preferences.debug("Binary data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT8:
                Preferences.debug("Signed byte data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT8:
                Preferences.debug("Unsigned byte data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT16:
                Preferences.debug("Signed short data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT16:
                Preferences.debug("Unsigned short data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT32:
                Preferences.debug("Signed integer data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT32:
                Preferences.debug("Unsigned integer data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT64:
                Preferences.debug("Signed long data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT64:
                Preferences.debug("Unsigned long data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT32:
                Preferences.debug("32 bit float data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT64:
                Preferences.debug("64 bit double data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT128:
                Preferences.debug("128 bit float data\n");
                MipavUtil.displayError("MIPAV cannot handle 128 bit floating point data\n");

                return false;

            case FileInfoNIFTI.NIFTI_TYPE_RGB24:
                Preferences.debug("RGB 24 bit data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX64:
                Preferences.debug("64 bit complex data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX128:
                Preferences.debug("128 bit DCOMPLEX data\n");
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX256:
                Preferences.debug("256 bit complex data\n");
                MipavUtil.displayError("MIPAV cannot handle 256 bit complex data\n");

                return false;

            default:
                Preferences.debug("Unknown datatype code = " + sourceType + "\n");
                MipavUtil.displayError("Unknown datatype code = " + sourceType);

                return false;
        }

        sourceBitPix = getBufferShort(bufferByte, 72, endianess);
        fileInfo.setSourceBitPix(sourceBitPix);
        Preferences.debug("FileNIFTI:readHeader. source bits per pixel = " + sourceBitPix + "\n", 2);

        sliceStart = getBufferShort(bufferByte, 74, endianess);

        pixdim = new float[dims + 1];
        resolutions = new float[numDims];

        for (i = 0, j = 0; i < (dims + 1); i++) {
            pixdim[i] = getBufferFloat(bufferByte, 76 + (4 * i), endianess);

            if ((i >= 1) && (niftiExtents[i - 1] > 1)) {
                resolutions[j] = Math.abs(pixdim[i]);
                Preferences.debug("FileNIFTI:readHeader. Resolutions " + (j + 1) + " = " + resolutions[j] + "\n", 2);
                j++;
            }
        }

        fileInfo.setResolutions(resolutions);

        vox_offset = getBufferFloat(bufferByte, 108, endianess);
        fileInfo.setVoxOffset(vox_offset);

        scl_slope = getBufferFloat(bufferByte, 112, endianess);
        fileInfo.setSclSlope(scl_slope);
        Preferences.debug("Data scaling slope = " + scl_slope + "\n");
        scl_inter = getBufferFloat(bufferByte, 116, endianess);
        fileInfo.setSclInter(scl_inter);
        Preferences.debug("Data offset = " + scl_inter + "\n");

        sliceEnd = getBufferShort(bufferByte, 120, endianess);

        sliceCode = bufferByte[122];

        if ((sliceCode > 0) && (sliceStart > 0)) {
            fileInfo.setSliceStart(sliceStart);
            Preferences.debug("Slice timing pattern starts with slice = " + (sliceStart + 1) + "\n");
        }

        if ((sliceCode > 0) && (sliceEnd > sliceStart)) {
            fileInfo.setSliceEnd(sliceEnd);
            Preferences.debug("Slice timing pattern ends with slice = " + (sliceEnd + 1) + "\n");
        }

        if (spatialDims == 0) {
            Preferences.debug("No x, y, or z dimensions are present\n");
        } else {
            spaceUnits = (int) (bufferByte[123] & 0x07);

            switch (spaceUnits) {

                case FileInfoNIFTI.NIFTI_UNITS_UNKNOWN:
                    Preferences.debug("Spatial units are unknown\n");
                    unitMeasure = FileInfoBase.UNKNOWN_MEASURE;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_METER:
                    Preferences.debug("Spatial units are meters\n");
                    unitMeasure = FileInfoBase.METERS;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_MM:
                    Preferences.debug("Spatial units are millimeters\n");
                    unitMeasure = FileInfoBase.MILLIMETERS;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_MICRON:
                    Preferences.debug("Spatial units are micrometers\n");
                    unitMeasure = FileInfoBase.MICROMETERS;
                    break;

                default:
                    Preferences.debug("Spatial units are an illegal " + spaceUnits + "\n");
                    unitMeasure = FileInfoBase.UNKNOWN_MEASURE;
                    break;
            }

            for (i = 0; i < spatialDims; i++) {
                fileInfo.setUnitsOfMeasure(unitMeasure, i);
            }
        }

        if ((dims >= 4) && (niftiExtents[3] > 1)) {
            timeUnits = bufferByte[123] & 0x38;

            switch (timeUnits) {

                case FileInfoNIFTI.NIFTI_UNITS_UNKNOWN:
                    Preferences.debug("Time units are unknown\n");
                    unitMeasure = FileInfoBase.UNKNOWN_MEASURE;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_SEC:
                    Preferences.debug("Time units are seconds\n");
                    unitMeasure = FileInfoBase.SECONDS;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_MSEC:
                    Preferences.debug("Time units are milliseconds\n");
                    unitMeasure = FileInfoBase.MILLISEC;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_USEC:
                    Preferences.debug("Time units are microseconds\n");
                    unitMeasure = FileInfoBase.MICROSEC;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_HZ:
                    Preferences.debug("Time units are hertz\n");
                    unitMeasure = FileInfoBase.HZ;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_PPM:
                    Preferences.debug("Time units are part per million\n");
                    unitMeasure = FileInfoBase.PPM;
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_RADS:
                    Preferences.debug("Time units are radians per second\n");
                    unitMeasure = FileInfoBase.RADS;
                    break;

                default:
                    Preferences.debug("Time units are an illegal = " + timeUnits + "\n");
                    unitMeasure = FileInfoBase.UNKNOWN_MEASURE;
            }

            fileInfo.setUnitsOfMeasure(unitMeasure, spatialDims);
        }

        fileInfo.setCalMax(getBufferFloat(bufferByte, 124, endianess));
        fileInfo.setCalMin(getBufferFloat(bufferByte, 128, endianess));
        sliceDuration = getBufferFloat(bufferByte, 132, endianess);

        if ((sliceDuration > 0) && (slice_dim > 0)) {
            fileInfo.setSliceDuration(sliceDuration);
            Preferences.debug("Time used to acquire 1 slice = " + sliceDuration + "\n");
        }

        if ((sliceCode > 0) && (slice_dim > 0) && (sliceDuration > 0)) {

            if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_SEQ_INC) {
                Preferences.debug("Slice timing order is sequentially increasing\n");
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_SEQ_DEC) {
                Preferences.debug("Slice timing order is sequentially decreasing\n");
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_ALT_INC) {
                Preferences.debug("Slice timing order is alternately increasing\n");
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_ALT_DEC) {
                Preferences.debug("Slice timing order is alternately decreasing\n");
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_ALT_INC2) {
                Preferences.debug("Slice timing order is alternately increasing #2\n");
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_ALT_DEC2) {
                Preferences.debug("Slice timing order is alternately decreasing #2\n");
            } else {
                Preferences.debug("slice code has an illegal value = " + sliceCode + "\n");
            }
        } else {
            Preferences.debug("Slice timing order is not specified\n");
        }

        tOffset = getBufferFloat(bufferByte, 136, endianess);
        fileInfo.setOrigin(tOffset, 3);
        Preferences.debug("tOffset = " + tOffset + "\n");

        switch (sourceType) {

            case FileInfoNIFTI.DT_NONE:
                return false;

            case FileInfoNIFTI.DT_BINARY:
                fileInfo.setDataType(ModelStorageBase.BOOLEAN);
                fileInfo.setBitPix((short) 1);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT8:
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                fileInfo.setBitPix((short) 8);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT16:
                fileInfo.setDataType(ModelStorageBase.SHORT);
                fileInfo.setBitPix((short) 16);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT32:
                fileInfo.setDataType(ModelStorageBase.INTEGER);
                fileInfo.setBitPix((short) 32);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT32:
                fileInfo.setDataType(ModelStorageBase.FLOAT);
                fileInfo.setBitPix((short) 32);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX64:
                fileInfo.setDataType(ModelStorageBase.COMPLEX);
                fileInfo.setBitPix((short) 64);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT64:
                fileInfo.setDataType(ModelStorageBase.DOUBLE);
                fileInfo.setBitPix((short) 64);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_RGB24:
                fileInfo.setDataType(ModelStorageBase.ARGB);
                fileInfo.setBitPix((short) 24);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT8:
                fileInfo.setDataType(ModelStorageBase.BYTE);
                fileInfo.setBitPix((short) 8);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT16:
                fileInfo.setDataType(ModelStorageBase.USHORT);
                fileInfo.setBitPix((short) 16);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT32:
                fileInfo.setDataType(ModelStorageBase.UINTEGER);
                fileInfo.setBitPix((short) 32);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT64:
                fileInfo.setDataType(ModelStorageBase.LONG);
                fileInfo.setBitPix((short) 64);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT64:
                fileInfo.setDataType(ModelStorageBase.LONG);
                fileInfo.setBitPix((short) 64);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX128:
                fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                fileInfo.setBitPix((short) 128);
                break;

            default:
                return false;
        }

        fileInfo.setDescription(new String(bufferByte, 148, 80));

        // update the fileInfo modality based on the description
        // if the description contains something other than modality, then
        // the modality will be set to unknown.
        fileInfo.setModality(FileInfoBase.getModalityFromStr(fileInfo.getDescription()));

        fileInfo.setAuxFile(new String(bufferByte, 228, 24));

        if (numDims >= 3) {
            qform_code = getBufferShort(bufferByte, 252, endianess);
            sform_code = getBufferShort(bufferByte, 254, endianess);

            if (pixdim[0] >= 0.0f) {
                qfac = 1.0f;
            } else {
                qfac = -1.0f;
            }

            if ((qform_code == 0) && (sform_code == 0)) {

                // No particular spatial orientation is assigned
                fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
                axisOrientation = new int[3];
                axisOrientation[0] = FileInfoBase.ORI_UNKNOWN_TYPE;
                axisOrientation[1] = FileInfoBase.ORI_UNKNOWN_TYPE;
                axisOrientation[2] = FileInfoBase.ORI_UNKNOWN_TYPE;
                fileInfo.setAxisOrientation(axisOrientation);
                matrix.setMatrix((double) resolutions[0], 0, 0);
                matrix.setMatrix((double) resolutions[1], 1, 1);
                matrix.setMatrix((double) resolutions[2], 2, 2);
            }

            // Both methods 2 and 3 could be present
            // However, MIPAV cannot handle 2 different transformation matrices
            // for the same image.  Method 2 should be the normal case, so give
            // it priority over method 3.
            Preferences.debug("qform_code = " + qform_code + "\n");
            Preferences.debug("sform_code = " + sform_code + "\n");

            if (qform_code > 0) {
                coord_code = qform_code;
            } else if (sform_code > 0) {
                coord_code = sform_code;
            }

            fileInfo.setCoordCode(coord_code);

            switch (coord_code) {

                case FileInfoNIFTI.NIFTI_XFORM_UNKNOWN:
                    Preferences.debug("Arbitrary X,Y,Z coordinate system\n", 2);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT:
                    Preferences.debug("Scanner based anatomical coordinates\n", 2);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_ALIGNED_ANAT:
                    Preferences.debug("Coordinates aligned to another file's or to anatomical truth\n", 2);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_TALAIRACH:
                    Preferences.debug("Talairach X,Y,Z coordinate system\n", 2);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_MNI_152:
                    Preferences.debug("MNI 152 normalized X,Y,Z coordinates\n", 2);
                    break;

                default:
                    Preferences.debug("Unknown coord_code = " + coord_code);
            }

            if (qform_code > 0) {
                quatern_b = getBufferFloat(bufferByte, 256, endianess);
                b = quatern_b;
                quatern_c = getBufferFloat(bufferByte, 260, endianess);
                c = quatern_c;
                quatern_d = getBufferFloat(bufferByte, 264, endianess);
                d = quatern_d;
                a = 1.0 - (b * b) - (c * c) - (d * d);

                if (a < 1.0e-7) {

                    // special case
                    a = 1.0 / Math.sqrt((b * b) + (c * c) + (d * d));

                    // normalize b,c,d vector;
                    b *= a;
                    c *= a;
                    d *= a;
                    a = 0.0;
                } else {
                    a = Math.sqrt(a);
                }

                r00 = (a * a) + (b * b) - (c * c) - (d * d);
                matrix.setMatrix(-r00 * resolutions[0], 0, 0);
                r01 = 2.0 * ((b * c) - (a * d));
                matrix.setMatrix(r01 * resolutions[1], 0, 1);
                r02 = 2.0 * ((b * d) + (a * c));
                matrix.setMatrix(-r02 * qfac *resolutions[2], 0, 2);
                r10 = 2.0 * ((b * c) + (a * d));
                matrix.setMatrix(-r10 * resolutions[0], 1, 0);
                r11 = (a * a) + (c * c) - (b * b) - (d * d);
                matrix.setMatrix(r11 * resolutions[1], 1, 1);
                r12 = 2.0 * ((c * d) - (a * b));
                matrix.setMatrix(-r12 * qfac * resolutions[2], 1, 2);
                r20 = 2.0 * ((b * d) - (a * c));
                matrix.setMatrix(r20 * resolutions[0], 2, 0);
                r21 = 2.0 * ((c * d) + (a * b));
                matrix.setMatrix(-r21 * resolutions[1], 2, 1);
                r22 = (a * a) + (d * d) - (c * c) - (b * b);
                matrix.setMatrix(r22 * qfac * resolutions[2], 2, 2);
                qoffset_x = getBufferFloat(bufferByte, 268, endianess);
                qoffset_y = getBufferFloat(bufferByte, 272, endianess);
                qoffset_z = getBufferFloat(bufferByte, 276, endianess);
                LPSOrigin = new float[3];
                LPSOrigin[0] = -qoffset_x;
                LPSOrigin[1] = qoffset_y;
                LPSOrigin[2] = qoffset_z;
                axisOrientation = getAxisOrientation(matrix);
                Preferences.debug("axisOrientation = " + axisOrientation[0] + "  " + axisOrientation[1] + "  " +
                                  axisOrientation[2] + "\n");
                fileInfo.setAxisOrientation(axisOrientation);

                if ((axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) ||
                        (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
                    fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                } else if ((axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) ||
                               (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
                    fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                } else {
                    fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                }

                origin = new float[3];
                for (j = 0; j < 3; j++) {

                    if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE ||
                        axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE){
                        origin[j] = LPSOrigin[0];
                       
                    }
                    else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE ||
                             axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE){
                        origin[j] = LPSOrigin[1];
                           
                    }
                    else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE ||
                             axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE){
                        origin[j] = LPSOrigin[2];
                           
                    }
                }

                
                fileInfo.setOrigin(origin);
                matrix.setMatrix((double) LPSOrigin[0], 0, 3);
                matrix.setMatrix((double) LPSOrigin[1], 1, 3);
                matrix.setMatrix((double) LPSOrigin[2], 2, 3);
                Preferences.debug("matrix = \n" + matrix + "\n");
                fileInfo.setMatrix(matrix);

                Preferences.debug("quatern_a = " + quatern_a + "\n");
                Preferences.debug("quatern_b = " + quatern_b + "\n");
                Preferences.debug("quatern_c = " + quatern_c + "\n");
                Preferences.debug("quatern_d = " + quatern_d + "\n");
                Preferences.debug("qoffset_x = " + qoffset_x + "\n");
                Preferences.debug("qoffset_y = " + qoffset_y + "\n");
                Preferences.debug("qoffset_z = " + qoffset_z + "\n");

            } // if (qform_code > 0)
            else if (sform_code > 0) {
                srow_x = new float[4];
                srow_y = new float[4];
                srow_z = new float[4];
                srow_x[0] = getBufferFloat(bufferByte, 280, endianess);
                srow_x[1] = getBufferFloat(bufferByte, 284, endianess);
                srow_x[2] = getBufferFloat(bufferByte, 288, endianess);
                srow_x[3] = getBufferFloat(bufferByte, 292, endianess);
                srow_y[0] = getBufferFloat(bufferByte, 296, endianess);
                srow_y[1] = getBufferFloat(bufferByte, 300, endianess);
                srow_y[2] = getBufferFloat(bufferByte, 304, endianess);
                srow_y[3] = getBufferFloat(bufferByte, 308, endianess);
                srow_z[0] = getBufferFloat(bufferByte, 312, endianess);
                srow_z[1] = getBufferFloat(bufferByte, 316, endianess);
                srow_z[2] = getBufferFloat(bufferByte, 320, endianess);
                srow_z[3] = getBufferFloat(bufferByte, 324, endianess);
                matrix.setMatrix((double) -srow_x[0], 0, 0);
                matrix.setMatrix((double) srow_x[1], 0, 1);
                matrix.setMatrix((double) -srow_x[2], 0, 2);
                matrix.setMatrix((double) -srow_y[0], 1, 0);
                matrix.setMatrix((double) srow_y[1], 1, 1);
                matrix.setMatrix((double) -srow_y[2], 1, 2);
                matrix.setMatrix((double) srow_z[0], 2, 0);
                matrix.setMatrix((double) -srow_z[1], 2, 1);
                matrix.setMatrix((double) srow_z[2], 2, 2);
                LPSOrigin = new float[3];
                LPSOrigin[0] = -srow_x[3];
                LPSOrigin[1] = srow_y[3];
                LPSOrigin[2] = srow_z[3];

                axisOrientation = getAxisOrientation(matrix);
                Preferences.debug("axisOrientation = " + axisOrientation[0] + "  " + axisOrientation[1] + "  " +
                                  axisOrientation[2] + "\n");
                fileInfo.setAxisOrientation(axisOrientation);

                if ((axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) ||
                        (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
                    fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                } else if ((axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) ||
                               (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
                    fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                } else {
                    fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                }

                origin = new float[3];
                for (j = 0; j < 3; j++) {

                    if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE ||
                        axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE){
                        origin[j] = LPSOrigin[0];
                       
                    }
                    else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE ||
                             axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE){
                        origin[j] = LPSOrigin[1];
                           
                    }
                    else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE ||
                             axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE){
                        origin[j] = LPSOrigin[2];
                           
                    }
                }
                
                fileInfo.setOrigin(origin);
                matrix.setMatrix((double) LPSOrigin[0], 0, 3);
                matrix.setMatrix((double) LPSOrigin[1], 1, 3);
                matrix.setMatrix((double) LPSOrigin[2], 2, 3);
                Preferences.debug("matrix = \n" + matrix + "\n");
                fileInfo.setMatrix(matrix);

                Preferences.debug("srow_x = " + srow_x[0] + "  " + srow_x[1] + "  " + srow_x[2] + "  " + srow_x[3] +
                                  "\n");
                Preferences.debug("srow_y = " + srow_y[0] + "  " + srow_y[1] + "  " + srow_y[2] + "  " + srow_y[3] +
                                  "\n");
                Preferences.debug("srow_z = " + srow_z[0] + "  " + srow_z[1] + "  " + srow_z[2] + "  " + srow_z[3] +
                                  "\n");
            } // else if (sform_code > 0)
        } // if (numDims >= 3)

        intentName = (new String(bufferByte, 328, 16));
        Preferences.debug("Name or meaning of data = " + intentName + "\n");
        fileInfo.setIntentName(intentName.trim());

        return true; // If it got this far, it has successfully read in the header
    }

    /**
     * Reads a NIFTI image file by reading the header then making a FileRaw to read the image for all filenames in the
     * file list. Only the one file directory (currently) supported.
     *
     * @param      one  flag indicating one image of a 3D dataset should be read in.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     The image.
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException, OutOfMemoryError {
        int offset;
        double m1, m2;
        boolean needFloat;
        int newType;
        boolean doChangeType;
        AlgorithmChangeType changeTypeAlgo;
        fileInfo = new FileInfoMGH(fileName, fileDir, FileBase.NIFTI);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" MGH header file error"));
        }

        int[] extents = null;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (int i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                       fileInfo.getFileName(), UI);
            } else {
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName(), UI);
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        updateUnitsOfMeasure(fileInfo, image);
        updateorigins(image.getFileInfo());
        image.setMatrix(matrix);

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, showProgress,
                                  FileBase.READ);

            if (oneFileStorage) {
                offset = (int) Math.abs(vox_offset);

                if (offset < headerSize) { // header length
                    offset = headerSize;
                }
            } else {
                offset = 0;
            }

            if (one) {

                if (fileInfo.getExtents().length > 2) {
                    offset = offset + getOffset(fileInfo);
                }
            }

            rawFile.readImage(image, offset);

            

            if ((scl_slope != 0.0) && ((scl_slope != 1.0f) || (scl_inter != 0.0f)) &&
                    (sourceType != FileInfoNIFTI.NIFTI_TYPE_COMPLEX64) &&
                    (sourceType != FileInfoNIFTI.NIFTI_TYPE_FLOAT64) &&
                    (sourceType != FileInfoNIFTI.NIFTI_TYPE_RGB24)) {
                image.calcMinMax();
                imageMin = image.getMin();
                imageMax = image.getMax();
                newType = image.getType();
                m1 = (scl_slope * imageMin) + scl_inter;
                m2 = (scl_slope * imageMax) + scl_inter;
                newMin = Math.min(m1, m2);
                newMax = Math.max(m1, m2);
                needFloat = false;
                doChangeType = false;

                if ((scl_slope != Math.round(scl_slope)) || (scl_inter != Math.round(scl_inter))) {
                    needFloat = true;
                }

                if (needFloat && (newMax <= Float.MAX_VALUE) && (newMin >= -Float.MAX_VALUE) &&
                        ((sourceType == FileInfoNIFTI.DT_BINARY) || (sourceType == FileInfoNIFTI.NIFTI_TYPE_INT8) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_INT16) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_INT32) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_UINT8) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_UINT16) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_UINT32))) {
                    newType = ModelStorageBase.FLOAT;
                    doChangeType = true;
                    fileInfo.setBitPix((short) 32);
                } else if (needFloat && (newMax <= Float.MAX_VALUE) && (newMin >= -Float.MAX_VALUE) &&
                               (sourceType == FileInfoNIFTI.NIFTI_TYPE_FLOAT32)) {
                    // do nothing
                } else if (needFloat) {
                    newType = ModelStorageBase.DOUBLE;
                    doChangeType = true;
                    fileInfo.setBitPix((short) 64);
                } else if ((newMax > imageMax) || (newMin < imageMin)) {

                    if ((newMin >= -128) && (newMax <= 127)) {
                        newType = ModelStorageBase.BYTE;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 8);
                    } else if ((newMin >= -32768) && (newMax <= 32767)) {
                        newType = ModelStorageBase.SHORT;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 16);
                    } else if ((newMin >= Integer.MIN_VALUE) && (newMax <= Integer.MAX_VALUE)) {
                        newType = ModelStorageBase.INTEGER;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 32);
                    } else if ((newMin >= Long.MIN_VALUE) && (newMax <= Long.MAX_VALUE)) {
                        newType = ModelStorageBase.LONG;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 64);
                    } else {
                        newType = ModelStorageBase.DOUBLE;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 64);
                    }
                }

                if (doChangeType) {

                    // Don't do scaling in changeTypeAlgo because scl_slope could be negative
                    changeTypeAlgo = new AlgorithmChangeType(image, newType, imageMin, imageMax, imageMin, imageMax,
                                                             false);
                    changeTypeAlgo.setProgressBarVisible(false);
                    changeTypeAlgo.run();
                    changeTypeAlgo.finalize();
                    changeTypeAlgo = null;
                } // if (doChangeType)

                scale(image);
            } // if ((scl_slope != 0.0) && ((scl_slope != 1.0f) || (scl_inter != 0.0f)) &&

            
            if (one) {
                fileInfo.setExtents(extents);
            }
        } catch (IOException error) {
            throw new IOException("FileNIFTI: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return image;
    }

    /**
     * Reads a NIFTI image file by reading the header then making a FileRaw to read the file. Image data is left in
     * buffer. If the fileInfo cannot be found, the header will be located and read first. Image is not 'flipped', and
     * neither units of measure nor orientation are set.
     *
     * @param      buffer  Image buffer to store image data into.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(float[] buffer) throws IOException, OutOfMemoryError {
        int i;
        int offset;

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoMGH(fileName, fileDir, FileBase.MGH);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
                throw (new IOException("Cannot read image because of MGH header file error"));
            }
        }

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, showProgress,
                                  FileBase.READ);

            if (oneFileStorage) {
                offset = (int) Math.abs(vox_offset);

                if (offset < headerSize) { // header length
                    offset = headerSize;
                }
            } else {
                offset = 0;
            }

            rawFile.readImage(buffer, offset, fileInfo.getSourceType());

            if (vox_offset < 0.0f) {

                for (i = 0; i < buffer.length; i++) {
                    buffer[i] = Math.abs(buffer[i]);
                }
            }

            if ((scl_slope != 0.0) && ((scl_slope != 1.0f) || (scl_inter != 0.0f))) {

                for (i = 0; i < buffer.length; i++) {
                    buffer[i] = (buffer[i] * scl_slope) + scl_inter;
                }
            } // if ((scl_slope != 0.0) && ((scl_slope != 1.0f) || (scl_inter != 0.0f)))

            
            rawFile.raFile.close();
        } catch (IOException error) {
            throw new IOException("FileMGH: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }

    /**
     * Scales image.
     *
     * @param   image  Image to scale.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void scale(ModelImage image) throws IOException {

        try {
            int nBuffers;
            int bufferSize;
            float[] buffer = null;
            float[] resultBuffer = null;

            if (image.getNDims() > 1) {
                bufferSize = image.getSliceSize();
            } else {
                bufferSize = image.getExtents()[0];
            }

            if (image.getNDims() == 5) {
                nBuffers = image.getExtents()[4] * image.getExtents()[3] * image.getExtents()[2];

            } else if (image.getNDims() == 4) {
                nBuffers = image.getExtents()[3] * image.getExtents()[2];
            } else if (image.getNDims() == 3) {
                nBuffers = image.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (image.isColorImage()) {

                buffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = image.getExtents()[0] * 4;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            buffer[(j * xDim) + i] = 255;
                            buffer[(j * xDim) + i + 1] = (scl_slope * buffer[(j * xDim) + i + 1]) + scl_inter;
                            buffer[(j * xDim) + i + 2] = (scl_slope * buffer[(j * xDim) + i + 2]) + scl_inter;
                            buffer[(j * xDim) + i + 3] = (scl_slope * buffer[(j * xDim) + i + 3]) + scl_inter;
                        }
                    }

                    image.importData(k * bufferSize, buffer, false);
                }
            } else {
                buffer = new float[bufferSize];

                int i, j, k;
                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            buffer[(j * xDim) + i] = (scl_slope * buffer[(j * xDim) + i]) + scl_inter;
                        }
                    }

                    image.importData(k * bufferSize, buffer, false);
                }
            }
        } catch (IOException error) {
            throw new IOException("FileNIFTI.scale: " + error);
        } catch (OutOfMemoryError error) {
            throw (error);
        }
    }

    

    /**
     * Return the 3 axis orientation codes that correspond to the closest standard anatomical orientation of the (i,j,k)
     * axes.
     *
     * @param   mat  4x4 matrix that transforms (i,j,k) indexes to x,y,z coordinates where +x =Left, +y = Posterior, +z
     *               = Superior Only the upper-left 3x3 corner of the matrix is used This routine finds the permutation
     *               of (x,y,z) which has the smallest angle to the (i,j,k) axes directions, which are columns of the
     *               input matrix Errors: The codes returned will be zero.
     *
     * @return  DOCUMENT ME!
     */
    private int[] getAxisOrientation(TransMatrix mat) {
        int[] axisOrientation = new int[3];
        double[][] array;
        double xi, xj, xk, yi, yj, yk, zi, zj, zk, val;
        Matrix Q;
        double detQ;
        double vbest;
        int ibest, jbest, kbest, pbest, qbest, rbest;
        int i, j, k, p, q, r;
        Matrix P;
        double detP;
        Matrix M;

        array = mat.getMatrix(0, 2, 0, 2).getArray();
        xi = array[0][0];
        xj = array[0][1];
        xk = array[0][2];
        yi = array[1][0];
        yj = array[1][1];
        yk = array[1][2];
        zi = array[2][0];
        zj = array[2][1];
        zk = array[2][2];

        int izero = 0;
        int jzero = 0;
        int kzero = 0;
        int xzero = 0;
        int yzero = 0;
        int zzero = 0;

        if (xi == 0.0) {
            izero++;
            xzero++;
        }

        if (yi == 0.0) {
            izero++;
            yzero++;
        }

        if (zi == 0.0) {
            izero++;
            zzero++;
        }

        if (xj == 0.0) {
            jzero++;
            xzero++;
        }

        if (yj == 0.0) {
            jzero++;
            yzero++;
        }

        if (zj == 0.0) {
            jzero++;
            zzero++;
        }

        if (xk == 0.0) {
            kzero++;
            xzero++;
        }

        if (yk == 0.0) {
            kzero++;
            yzero++;
        }

        if (zk == 0.0) {
            kzero++;
            zzero++;
        }

        if ((izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2)) {

            if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
            }

            return axisOrientation;
        } // if ((izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2))

        // Normalize column vectors to get unit vectors along each ijk-axis

        // Normalize i axis
        val = Math.sqrt((xi * xi) + (yi * yi) + (zi * zi));

        if (val == 0.0) {
            MipavUtil.displayError("xi = yi = zi = 0 in getAxisOrientation");

            return null;
        }

        xi /= val;
        yi /= val;
        zi /= val;

        // Normalize j axis
        val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj));

        if (val == 0.0) {
            MipavUtil.displayError("xj = yj = zj = 0 in getAxisOrientation");

            return null;
        }

        xj /= val;
        yj /= val;
        zj /= val;

        // Orthogonalize j axis to i axis, if needed
        val = (xi * xj) + (yi * yj) + (zi * zj); // dot product between i and j

        if (Math.abs(val) > 1.0e-4) {
            xj -= val * xi;
            yj -= val * yi;
            zj -= val * zi;
            val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj)); // Must renormalize

            if (val == 0.0) {
                MipavUtil.displayError("j was parallel to i in getAxisOrientation");

                return null;
            }

            xj /= val;
            yj /= val;
            zj /= val;
        }

        // Normalize k axis; if it is zero, make it the cross product i x j
        val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

        if (val == 0.0) {
            xk = (yi * zj) - (zi * yj);
            yk = (zi * xj) - (zj * xi);
            zk = (xi * yj) - (yi * xj);
        } else {
            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to i
        val = (xi * xk) + (yi * yk) + (zi * zk); // dot product between i and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xi;
            yk -= val * yi;
            zk -= val * zi;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to i");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to j
        val = (xj * xk) + (yj * yk) + (zj * zk); // dot product between j and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xj;
            yk -= val * yj;
            zk -= val * zj;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to j");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        array[0][0] = xi;
        array[0][1] = xj;
        array[0][2] = xk;
        array[1][0] = yi;
        array[1][1] = yj;
        array[1][2] = yk;
        array[2][0] = zi;
        array[2][1] = zj;
        array[2][2] = zk;

        // At this point, Q is the rotation matrix from the (i,j,k) to the (x,y,z) axes
        Q = new Matrix(array);
        detQ = Q.det();

        if (detQ == 0.0) {
            MipavUtil.displayError("detQ == 0.0 in getAxisOrientation");

            return null;
        }

        // Build and test all possible +1/-1 coordinate permutation matrices P;
        // then find the P such that the rotation matrix M=PQ is closest to the
        // identity, in the sense of M having the smallest total rotation angle

        // Despite the formidable looking 6 nested loops, there are
        // only 3*3*3*2*2*2 = 216 passes, which will run very quickly
        vbest = -Double.MAX_VALUE;
        pbest = 1;
        qbest = 1;
        rbest = 1;
        ibest = 1;
        jbest = 2;
        kbest = 3;

        for (i = 1; i <= 3; i++) { // i = column number to use for row #1

            for (j = 1; j <= 3; j++) { // j = column number to use for row #2

                if (i == j) {
                    continue;
                }

                for (k = 1; k <= 3; k++) { // k = column number to use for row #3

                    if ((i == k) || (j == k)) {
                        continue;
                    }

                    array[0][0] = 0.0;
                    array[0][1] = 0.0;
                    array[0][2] = 0.0;
                    array[1][0] = 0.0;
                    array[1][1] = 0.0;
                    array[1][2] = 0.0;
                    array[2][0] = 0.0;
                    array[2][1] = 0.0;
                    array[2][2] = 0.0;
                    P = new Matrix(array);

                    for (p = -1; p <= 1; p += 2) { // p,q,r are -1 or +1 and go into rows #1,2,3

                        for (q = -1; q <= 1; q += 2) {

                            for (r = -1; r <= 1; r += 2) {
                                P.set(0, i - 1, p);
                                P.set(1, j - 1, q);
                                P.set(2, k - 1, r);
                                detP = P.det();

                                // sign of permutation doesn't match sign of Q
                                if ((detP * detQ) <= 0.0) {
                                    continue;
                                }

                                M = P.times(Q);

                                // angle of M rotation = 2.0*acos(0.5*sqrt(1.0+trace(M)))
                                // we want largest trace(M) == smallest angle == M nearest to I
                                val = M.get(0, 0) + M.get(1, 1) + M.get(2, 2); // trace

                                if (val > vbest) {
                                    vbest = val;
                                    ibest = i;
                                    jbest = j;
                                    kbest = k;
                                    pbest = p;
                                    qbest = q;
                                    rbest = r;
                                }
                            }
                        }
                    }
                }
            }
        }

        // At this point ibest is 1 or 2 or 3; pbest is -1 or +1; etc.

        // The matrix P that corresponds is the best permutation approximation
        // to Q-inverse; that is, P (approximately) takes (x,y,z) coordinates
        // to the (i,j,k) axes

        // For example, the first row of P (which contains pbest in column ibest)
        // determines the way the i axis points relative to the anatomical
        // (x,y,z) axes.  If ibest is 2, then the i axis is along the yaxis,
        // which is direction P2A (if pbest < 0) or A2P (if pbest > 0).

        // So, using ibest and pbest, we can assign the output code for
        // the i axis.  The same also applies for the j and k axes.

        switch (ibest * pbest) {

            case -1:
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (jbest * qbest) {

            case -1:
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (kbest * rbest) {

            case -1:
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        return axisOrientation;
    }

    /**
     * Helper method to calculate the offset for getting only the middle NIFTI image slice from the 3D file.
     *
     * @param   fileInfo  File info.
     *
     * @return  offset
     */
    private int getOffset(FileInfoMGH fileInfo) {
        int offset = fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * (fileInfo.getExtents()[2] / 2);

        switch (fileInfo.getSourceType()) {

            case FileInfoNIFTI.DT_BINARY:
            case FileInfoNIFTI.NIFTI_TYPE_UINT8:
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT16:
            case FileInfoNIFTI.NIFTI_TYPE_UINT16:
                offset *= 2;
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT32:
            case FileInfoNIFTI.NIFTI_TYPE_INT32:
                offset *= 4;
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT64:
            case FileInfoNIFTI.NIFTI_TYPE_FLOAT64:
            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX64:
                offset *= 8;
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX128:
                offset *= 16;
                break;

            case FileInfoNIFTI.NIFTI_TYPE_RGB24:
                offset *= 3;
                break;
        }

        return offset;
    }

    /**
     * max column norm of 3x3 matrix.
     *
     * @param   A  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double mat33_colnorm(Matrix A) {
        double r1, r2, r3;
        r1 = Math.abs(A.get(0, 0)) + Math.abs(A.get(1, 0)) + Math.abs(A.get(2, 0));
        r2 = Math.abs(A.get(0, 1)) + Math.abs(A.get(1, 1)) + Math.abs(A.get(2, 1));
        r3 = Math.abs(A.get(0, 2)) + Math.abs(A.get(1, 2)) + Math.abs(A.get(2, 2));

        if (r1 < r2) {
            r1 = r2;
        }

        if (r1 < r3) {
            r1 = r3;
        }

        return r1;
    }

    /**
     * Polar decomposition of a 3x3 matrix: finds the closest orthogonal matrix to input A (in both Frobenius and L2
     * norms). Algorithm is that from NJ Higham, SIAM JSci Stat Comput, 7:1160-1174.
     *
     * @param   A  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Matrix mat33_polar(Matrix A) {
        Matrix X, Y, Z;
        double alp, bet, gam, gmi;
        double dif = 1.0;
        int k = 0;
        double val;

        X = A.copy();
        Z = new Matrix(3, 3);

        // force matrix to be nonsingular
        gam = X.det();

        while (gam == 0.0) { // perturb matrix
            gam = 0.00001 * (0.001 + mat33_rownorm(X));
            val = X.get(0, 0);
            X.set(0, 0, val + gam);
            val = X.get(1, 1);
            X.set(1, 1, val + gam);
            val = X.get(2, 2);
            X.set(2, 2, val + gam);
            gam = X.det();
        }

        while (true) {
            Y = X.inverse();

            if (dif > 0.3) { // far from convergence
                alp = Math.sqrt(mat33_rownorm(X) * mat33_colnorm(X));
                bet = Math.sqrt(mat33_rownorm(Y) * mat33_colnorm(Y));
                gam = Math.sqrt(bet / alp);
                gmi = 1.0 / gam;
            } else {
                gam = gmi = 1.0; // close to convergence
            }

            Z.set(0, 0, 0.5 * ((gam * X.get(0, 0)) + (gmi * Y.get(0, 0))));
            Z.set(0, 1, 0.5 * ((gam * X.get(0, 1)) + (gmi * Y.get(1, 0))));
            Z.set(0, 2, 0.5 * ((gam * X.get(0, 2)) + (gmi * Y.get(2, 0))));
            Z.set(1, 0, 0.5 * ((gam * X.get(1, 0)) + (gmi * Y.get(0, 1))));
            Z.set(1, 1, 0.5 * ((gam * X.get(1, 1)) + (gmi * Y.get(1, 1))));
            Z.set(1, 2, 0.5 * ((gam * X.get(1, 2)) + (gmi * Y.get(2, 1))));
            Z.set(2, 0, 0.5 * ((gam * X.get(2, 0)) + (gmi * Y.get(0, 2))));
            Z.set(2, 1, 0.5 * ((gam * X.get(2, 1)) + (gmi * Y.get(1, 2))));
            Z.set(2, 2, 0.5 * ((gam * X.get(2, 2)) + (gmi * Y.get(2, 2))));

            dif = Math.abs(Z.get(0, 0) - X.get(0, 0)) + Math.abs(Z.get(0, 1) - X.get(0, 1)) +
                  Math.abs(Z.get(0, 2) - X.get(0, 2)) + Math.abs(Z.get(1, 0) - X.get(1, 0)) +
                  Math.abs(Z.get(1, 1) - X.get(1, 1)) + Math.abs(Z.get(1, 2) - X.get(1, 2)) +
                  Math.abs(Z.get(2, 0) - X.get(2, 0)) + Math.abs(Z.get(2, 1) - X.get(2, 1)) +
                  Math.abs(Z.get(2, 2) - X.get(2, 2));

            k = k + 1;

            if ((k > 100) || (dif < 3.0e-6)) {
                break; // convergence or exhaustion
            }

            X = Z.copy();
        }

        return Z;
    }

    /**
     * max row norm of 3x3 matrix.
     *
     * @param   A  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double mat33_rownorm(Matrix A) {
        double r1, r2, r3;
        r1 = Math.abs(A.get(0, 0)) + Math.abs(A.get(0, 1)) + Math.abs(A.get(0, 2));
        r2 = Math.abs(A.get(1, 0)) + Math.abs(A.get(1, 1)) + Math.abs(A.get(1, 2));
        r3 = Math.abs(A.get(2, 0)) + Math.abs(A.get(2, 1)) + Math.abs(A.get(2, 2));

        if (r1 < r2) {
            r1 = r2;
        }

        if (r1 < r3) {
            r1 = r3;
        }

        return r1;
    }

    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    private void updateorigins(FileInfoBase[] fileInfo) {
        int axisOrient;

        float[] origin = (float[]) (fileInfo[0].getOrigin().clone());
        float[] resolutions = fileInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin);
                axisOrient = fileInfo[i].getAxisOrientation(2);

                if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                        (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                    origin[2] += resolutions[2];
                } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                    origin[2] -= resolutions[2];
                }
            }
        } else if (image.getNDims() == 4) {
            float tmp = origin[2];

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[(i * image.getExtents()[2]) + j].setOrigin(origin);
                    axisOrient = fileInfo[i].getAxisOrientation(2);

                    if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                            (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                        origin[2] += resolutions[2];
                    } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                        origin[2] -= resolutions[2];
                    }
                }

                origin[3] += resolutions[3];
                origin[2] = tmp;
            }
        }
        /*else if (image.getNDims() == 5) {
         * fileInfo = image.getFileInfo(); for (int i = 0;    i <    image.getExtents()[2] * image.getExtents()[3] *
         * image.getExtents()[4];    i++) { fileInfo[i].setorigins(startLocs); startLocs[4] += resolutions[4]; }  }*/
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fileInfo  -- a NIFTI file Info that has already been read
     * @param  image     -- a ModelImage that the fileInfo needs to be attached to
     */
    private void updateUnitsOfMeasure(FileInfoMGH fileInfo, ModelImage image) {

        int[] extents = fileInfo.getExtents();

        if (image.getNDims() == 2) {
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 2);

            for (int i = 0; i < extents[2]; i++) {
                FileInfoMGH newFileInfo = (FileInfoMGH) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 2);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MILLISEC, 3);

            for (int i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoMGH newFileInfo = (FileInfoMGH) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    } // end updateUnitsOfMeasure()

    
}
