package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;


/**
 * Some of this code is derived from FITS.java in ImageJ.
 * 
 * In A User's Guide for the Flexible Image Transport System (FITS) Version 4.0, Section 4.1 Indexes and Physical
 * Coordinates, the text says, "Historically, astronomers have generally assumed that the index point in a FITS file
 * represents the center of a pixel. This interpretation is endorsed by GC. It differs from the common practice in
 * computer graphics of treating the center of a pixel as a half-integral point." For CRPIXn in FITS: The center of the
 * first pixel is 1 and the center of the last pixel is NAXISn. In MIPAV the center of the first pixel is 0.5 and the
 * center of the last pixel is NAXISn - 0.5. So subtract 0.5 to go from FITS to MIPAV. " ...recommend that FITS writers
 * order pixels starting in the lower left hand corner of the image, with the first axis increasing to the right, as in
 * the rectangular coordinate x-axis, and the second increasing upward (the y-axis)." MIPAV uses the upper left hand
 * corner as the origin and has the y axis going downward. Therefore, the image is flipped after reading and flipped
 * before writing.
 * 
 * <hr>
 * <p>
 * ImageJ disclaimer:
 * </p>
 * 
 * <p>
 * ImageJ is being developed at the National Institutes of Health by an employee of the Federal Government in the course
 * of his official duties. Pursuant to Title 17, Section 105 of the United States Code, this software is not subject to
 * copyright protection and is in the public domain. ImageJ is an experimental system. NIH assumes no responsibility
 * whatsoever for its use by other parties, and makes no guarantees, expressed or implied, about its quality,
 * reliability, or any other characteristic.
 * </p>
 */
public class FileFits extends FileBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int[] imgExtents;

    /** DOCUMENT ME! */
    int nDimensions = 2;

    /** DOCUMENT ME! */
    int sourceType = ModelStorageBase.FLOAT;

    /** DOCUMENT ME! */
    private int BLANK = 0;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoFits fileInfo;

    private FileInfoFits fileInfoCopy;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private boolean haveBlank = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private int numberSlices; // 1 for 2D, zDim for 3D, and zDim * tDim for 4D

    private boolean isColorPlanar2D = false;

    private AlgorithmFlip flipAlgo;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * FITS reader/writer constructor.
     * 
     * @param fileName file name
     * @param fileDir file directory
     * 
     * @exception IOException if there is an error making the file
     */
    public FileFits(final String fileName, final String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileInfo = null;
        fileInfoCopy = null;
        file = null;
        image = null;
        imgExtents = null;
        LUT = null;
        if (flipAlgo != null) {
            flipAlgo.finalize();
            flipAlgo = null;
        }
        try {
            super.finalize();
        } catch (final Throwable er) {}
    }

    /**
     * returns LUT if defined.
     * 
     * @return the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * reads the FITS file header and data.
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @param one DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public ModelImage readImage(final boolean one) throws IOException {

        // Fixed format is required for the values of the required keywords and strongly
        // recommended for the other keywords. The required keywords are SIMPLE, BITPIX,
        // NAXIS, NAXISn, n=1,...,NAXIS(NAXIS = 0 -> NAXIS1 not present), and END.
        int i = 0;
        int j = 0;
        String s, firstS, subS;
        String dateString;
        String jobNameString;
        String timeString;
        int numApos;
        int aposLoc[];
        boolean haveTime;
        boolean readAgain;
        int count = 0;
        int bitsPerPixel;

        // data value = (FITS_value) X BSCALE + BZERO
        double BSCALE = 1.0;
        double BZERO = 0.0;
        long offset;
        float[] imgBuffer;
        double[] imgDBuffer;
        int bufferSize;
        // Be able to handle dimensions of length 1 in read in as long
        // as no more than 4 dimensions greater than length 1 are present
        final float[] imgResols = new float[] {1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f};
        final double[] scale = new double[] {1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
        // reference pixel position along axis
        final double[] crpix = new double[6];
        // coordinate of reference pixel
        final double[] crval = new double[6];
        final boolean[] haveCrpix = new boolean[6];
        final boolean[] haveCrval = new boolean[6];
        float minResol;
        float maxResol;
        int dimNumber;
        int[] reducedExtents;
        double[] reducedScale;
        double[] reducedCrpix;
        double[] reducedCrval;
        boolean[] reducedHaveCrpix;
        boolean[] reducedHaveCrval;
        float focalRatio;
        double origin[];
        TransMatrix matrix;
        char c;

        try {

            file = new File(fileDir + fileName);

            endianess = FileBase.BIG_ENDIAN; // true
            fileInfo = new FileInfoFits(fileName, fileDir, FileUtility.FITS);
            fileInfo.setEndianess(endianess);

            raFile = new RandomAccessFile(file, "r");

            readAgain = true;

            while (readAgain) { // looping for first required keyword of SIMPLE
                s = getString(80);
                count++;
                firstS = s.substring(0, 1);

                if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                    ;
                } else if ( !s.startsWith("SIMPLE")) {
                    raFile.close();

                    MipavUtil.displayError("Instead of SIMPLE first keyword starts with " + s);
                    throw new IOException();
                } else {
                    readAgain = false;
                    subS = s.substring(8, 10);

                    if ( !subS.equals("= ")) {
                        Preferences.debug("SIMPLE line does not have required =<sp> in cols 9 and 10\n", 
                        		Preferences.DEBUG_FILEIO);
                    }
                    subS = s.substring(29, 30);

                    if ( ( !subS.equals("T")) && ( !subS.equals("F"))) {
                        Preferences.debug("\nSIMPLE line does not have required T or F in column 30\n",
                        		Preferences.DEBUG_FILEIO);
                        if (subS.trim() != null) {
                            if (subS.trim().length() != 0) {
                                Preferences.debug("SIMPLE line has " + subS + " in column 30\n", Preferences.DEBUG_FILEIO);
                            }
                        }
                        i = s.indexOf("/");
                        if (i != -1) {
                            subS = s.substring(10, i).trim();
                        } else {
                            subS = s.substring(10).trim();
                        }
                        i = subS.length();
                        subS = subS.substring(i - 1);
                        if (subS.equals("T")) {
                            Preferences
                                    .debug("SIMPLE = T for file conformance to FITS standards in nonstandard column\n", 
                                    		Preferences.DEBUG_FILEIO);
                        } else if (subS.equals("F")) {
                            Preferences.debug("SIMPLE = F for departure from FITS standards in some significant way\n"
                                    + "in nonstandard column\n", Preferences.DEBUG_FILEIO);
                        }
                    } else if (subS.equals("T")) {
                        Preferences.debug("\nSIMPLE = T for file conformance to FITS standards\n", Preferences.DEBUG_FILEIO);
                    } else { // subs.equals("F")
                        Preferences.debug("\nSIMPLE = F for departure from FITS standards in some significant way\n", 
                        		Preferences.DEBUG_FILEIO);
                    }

                }
            } // while(readAgain) looping for first required keyword of SIMPLE

            readAgain = true;

            while (readAgain) { // looping for second required keyword of BITPIX
                s = getString(80);
                count++;
                firstS = s.substring(0, 1);

                if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                    ;
                } else if ( !s.startsWith("BITPIX")) {
                    raFile.close();

                    MipavUtil.displayError("Instead of BITPIX second keyword starts with " + s);
                    throw new IOException();
                } else {
                    readAgain = false;
                    subS = s.substring(8, 10);

                    if ( !subS.equals("= ")) {
                        Preferences.debug("BITPIX line does not have required =<sp> in cols 9 and 10\n", 
                        		Preferences.DEBUG_FILEIO);
                        subS = s.substring(8, 30);
                    } else {
                        subS = s.substring(10, 30);
                    }
                    subS = subS.trim();

                    try {
                        bitsPerPixel = Integer.parseInt(subS);
                    } catch (final NumberFormatException e) {
                        raFile.close();

                        MipavUtil.displayError("Instead of integer BITPIX line had " + subS);
                        throw new IOException();
                    }

                    switch (bitsPerPixel) {

                        case 8:
                            sourceType = ModelStorageBase.UBYTE;
                            Preferences.debug("sourceType = ModelStorageBase.UBYTE\n", Preferences.DEBUG_FILEIO);
                            break;

                        case 16:
                            sourceType = ModelStorageBase.SHORT;
                            Preferences.debug("sourceType = ModelStorageBase.SHORT\n", Preferences.DEBUG_FILEIO);
                            break;

                        case 32:
                            sourceType = ModelStorageBase.INTEGER;
                            Preferences.debug("sourceType = ModelStorageBase.INTEGER\n", Preferences.DEBUG_FILEIO);
                            break;

                        case -32:
                            sourceType = ModelStorageBase.FLOAT;
                            Preferences.debug("sourceType = ModelStorageBase.FLOAT\n", Preferences.DEBUG_FILEIO);
                            break;

                        case -64:
                            sourceType = ModelStorageBase.DOUBLE;
                            Preferences.debug("sourceType = ModelStorageBase.DOUBLE\n", Preferences.DEBUG_FILEIO);
                            break;

                        default:
                            raFile.close();

                            MipavUtil.displayError("BITPIX keyword had illegal value of " + bitsPerPixel);
                            throw new IOException();
                    }
                }
            } // while(readAgain) looping for second required keyword of BITPIX

            readAgain = true;

            while (readAgain) { // looping for third required keyword of NAXIS
                s = getString(80);
                count++;
                firstS = s.substring(0, 1);

                if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                    ;
                } else if ( !s.startsWith("NAXIS")) {
                    raFile.close();

                    MipavUtil.displayError("Instead of NAXIS third keyword starts with " + s);
                    throw new IOException();
                } else {
                    readAgain = false;
                    subS = s.substring(8, 10);

                    if ( !subS.equals("= ")) {
                        Preferences.debug("NAXIS line does not have required =<sp> in cols 9 and 10\n", 
                        		Preferences.DEBUG_FILEIO);
                        subS = s.substring(8, 30);
                    } else {
                        subS = s.substring(10, 30);
                    }
                    subS = subS.trim();

                    try {
                        nDimensions = Integer.parseInt(subS);
                    } catch (final NumberFormatException e) {
                        raFile.close();

                        MipavUtil.displayError("Instead of integer NAXIS line had " + subS);
                        throw new IOException();
                    }

                    Preferences.debug("NAXIS = " + nDimensions + "\n", Preferences.DEBUG_FILEIO);

                    if (nDimensions < 0) {
                        raFile.close();

                        MipavUtil.displayError("NAXIS had an illegal negative value of " + nDimensions);
                        throw new IOException();
                    } else if (nDimensions > 999) {
                        raFile.close();

                        MipavUtil.displayError("NAXIS exceeded maximum legal value of 999 with " + nDimensions);
                        throw new IOException();
                    } else if (nDimensions == 1) {
                        raFile.close();

                        MipavUtil.displayError("MIPAV cannot display an image with 1 dimension");
                        throw new IOException();
                    } else if (nDimensions == 0) {
                        raFile.close();

                        MipavUtil
                                .displayError("NAXIS value of 0 indicates no binary data matrix is associated with the header");
                        throw new IOException();
                    }
                }
            } // while (readAgain) looping for third required keyword of NAXIS

            imgExtents = new int[nDimensions];

            readAgain = true;

            while (readAgain) { // looping for fourth required keyword of NAXIS1
                s = getString(80);
                count++;
                firstS = s.substring(0, 1);

                if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                    ;
                } else if ( !s.startsWith("NAXIS1")) {
                    raFile.close();

                    MipavUtil.displayError("Instead of NAXIS1 fourth keyword starts with " + s);
                    throw new IOException();
                } else {
                    readAgain = false;
                    subS = s.substring(8, 10);

                    if ( !subS.equals("= ")) {
                        Preferences.debug("NAXIS1 line does not have required =<sp> in cols 9 and 10\n", 
                        		Preferences.DEBUG_FILEIO);
                        subS = s.substring(8, 30);
                    } else {
                        subS = s.substring(10, 30);
                    }
                    subS = subS.trim();

                    try {
                        imgExtents[0] = Integer.parseInt(subS);
                    } catch (final NumberFormatException e) {
                        raFile.close();

                        MipavUtil.displayError("Instead of integer NAXIS1 line had " + subS);
                        throw new IOException();
                    }

                    Preferences.debug("NAXIS1 = " + imgExtents[0] + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 0);

                    if (imgExtents[0] < 0) {
                        raFile.close();

                        MipavUtil.displayError("NAXIS1 had an illegal negative value of " + imgExtents[0]);
                        throw new IOException();
                    } else if (imgExtents[0] == 0) {
                        raFile.close();

                        MipavUtil.displayError("NAXIS1 value of 0 indicates no data is associated with the header");
                        throw new IOException();
                    }
                }
            } // while (readAgain) looping for fourth required keyword of NAXIS1

            readAgain = true;

            while (readAgain) { // looping for fifth required keyword of NAXIS2
                s = getString(80);
                count++;
                firstS = s.substring(0, 1);

                if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                    ;
                } else if ( !s.startsWith("NAXIS2")) {
                    raFile.close();

                    MipavUtil.displayError("Instead of NAXIS2 fifth keyword starts with " + s);
                    throw new IOException();
                } else {
                    readAgain = false;
                    subS = s.substring(8, 10);

                    if ( !subS.equals("= ")) {
                        Preferences.debug("NAXIS2 line does not have required =<sp> in cols 9 and 10\n", 
                        		Preferences.DEBUG_FILEIO);
                        subS = s.substring(8, 30);
                    } else {
                        subS = s.substring(10, 30);
                    }
                    subS = subS.trim();

                    try {
                        imgExtents[1] = Integer.parseInt(subS);
                    } catch (final NumberFormatException e) {
                        raFile.close();

                        MipavUtil.displayError("Instead of integer NAXIS2 line had " + subS);
                        throw new IOException();
                    }

                    Preferences.debug("NAXIS2 = " + imgExtents[1] + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 1);

                    if (imgExtents[1] < 0) {
                        raFile.close();

                        MipavUtil.displayError("NAXIS2 had an illegal negative value of " + imgExtents[1]);
                        throw new IOException();
                    } else if (imgExtents[1] == 0) {
                        raFile.close();

                        MipavUtil.displayError("NAXIS2 value of 0 indicates no data is associated with the header");
                        throw new IOException();
                    }
                }
            } // while (readAgain) looping for fifth required keyword of NAXIS2

            if (nDimensions >= 3) {
                readAgain = true;

                while (readAgain) { // looping for sixth required keyword of NAXIS3
                    s = getString(80);
                    count++;
                    firstS = s.substring(0, 1);

                    if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                        ;
                    } else if ( !s.startsWith("NAXIS3")) {
                        raFile.close();

                        MipavUtil.displayError("Instead of NAXIS3 sixth keyword starts with " + s);
                        throw new IOException();
                    } else {
                        readAgain = false;
                        subS = s.substring(8, 10);

                        if ( !subS.equals("= ")) {
                            Preferences.debug("NAXIS3 line does not have required =<sp> in cols 9 and 10\n", 
                            		Preferences.DEBUG_FILEIO);
                            subS = s.substring(8, 30);
                        } else {
                            subS = s.substring(10, 30);
                        }
                        subS = subS.trim();

                        try {
                            imgExtents[2] = Integer.parseInt(subS);
                        } catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of integer NAXIS3 line had " + subS);
                            throw new IOException();
                        }

                        Preferences.debug("NAXIS3 = " + imgExtents[2] + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 2);

                        if (imgExtents[2] < 0) {
                            raFile.close();

                            MipavUtil.displayError("NAXIS3 had an illegal negative value of " + imgExtents[2]);
                            throw new IOException();
                        } else if (imgExtents[2] == 0) {
                            raFile.close();

                            MipavUtil.displayError("NAXIS3 value of 0 indicates no data is associated with the header");
                            throw new IOException();
                        }
                    }
                } // while (readAgain) looping for sixth required keyword of NAXIS3
            } // if (nDimensions >= 3)

            if (nDimensions >= 4) {
                readAgain = true;

                while (readAgain) { // looping for seventh required keyword of NAXIS4
                    s = getString(80);
                    count++;
                    firstS = s.substring(0, 1);

                    if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                        ;
                    } else if ( !s.startsWith("NAXIS4")) {
                        raFile.close();

                        MipavUtil.displayError("Instead of NAXIS4 seventh keyword starts with " + s);
                        throw new IOException();
                    } else {
                        readAgain = false;
                        subS = s.substring(8, 10);

                        if ( !subS.equals("= ")) {
                            Preferences.debug("NAXIS4 line does not have required =<sp> in cols 9 and 10\n",
                            		Preferences.DEBUG_FILEIO);
                            subS = s.substring(8, 30);
                        } else {
                            subS = s.substring(10, 30);
                        }
                        subS = subS.trim();

                        try {
                            imgExtents[3] = Integer.parseInt(subS);
                        } catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of integer NAXIS4 line had " + subS);
                            throw new IOException();
                        }

                        Preferences.debug("NAXIS4 = " + imgExtents[3] + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 3);

                        if (imgExtents[3] < 0) {
                            raFile.close();

                            MipavUtil.displayError("NAXIS4 had an illegal negative value of " + imgExtents[3]);
                            throw new IOException();
                        } else if (imgExtents[3] == 0) {
                            raFile.close();

                            MipavUtil.displayError("NAXIS4 value of 0 indicates no data is associated with the header");
                            throw new IOException();
                        }
                    }
                } // while (readAgain) looping for seventh required keyword of NAXIS4
            } // if (nDimensions >= 4)

            if (nDimensions >= 5) {
                readAgain = true;

                while (readAgain) { // looping for eighth required keyword of NAXIS5
                    s = getString(80);
                    count++;
                    firstS = s.substring(0, 1);

                    if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                        ;
                    } else if ( !s.startsWith("NAXIS5")) {
                        raFile.close();

                        MipavUtil.displayError("Instead of NAXIS5 eighth keyword starts with " + s);
                        throw new IOException();
                    } else {
                        readAgain = false;
                        subS = s.substring(8, 10);

                        if ( !subS.equals("= ")) {
                            Preferences.debug("NAXIS5 line does not have required =<sp> in cols 9 and 10\n", 
                            		Preferences.DEBUG_FILEIO);
                            subS = s.substring(8, 30);
                        } else {
                            subS = s.substring(10, 30);
                        }
                        subS = subS.trim();

                        try {
                            imgExtents[4] = Integer.parseInt(subS);
                        } catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of integer NAXIS5 line had " + subS);
                            throw new IOException();
                        }

                        Preferences.debug("NAXIS5 = " + imgExtents[4] + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setUnitsOfMeasure(Unit.UNKNOWN_MEASURE.getLegacyNum(), 4);

                        if (imgExtents[4] < 0) {
                            raFile.close();

                            MipavUtil.displayError("NAXIS4 had an illegal negative value of " + imgExtents[4]);
                            throw new IOException();
                        } else if (imgExtents[4] == 0) {
                            raFile.close();

                            MipavUtil.displayError("NAXIS4 value of 0 indicates no data is associated with the header");
                            throw new IOException();
                        }
                    }
                } // while (readAgain) looping for eighth required keyword of NAXIS5
            } // if (nDimensions >= 5)

            if (nDimensions >= 6) {
                readAgain = true;

                while (readAgain) { // looping for ninth required keyword of NAXIS6
                    s = getString(80);
                    count++;
                    firstS = s.substring(0, 1);

                    if ( (firstS.equals(" ")) || (firstS.equals("/"))) {
                        ;
                    } else if ( !s.startsWith("NAXIS6")) {
                        raFile.close();

                        MipavUtil.displayError("Instead of NAXIS6 ninth keyword starts with " + s);
                        throw new IOException();
                    } else {
                        readAgain = false;
                        subS = s.substring(8, 10);

                        if ( !subS.equals("= ")) {
                            Preferences.debug("NAXIS6 line does not have required =<sp> in cols 9 and 10\n", 
                            		Preferences.DEBUG_FILEIO);
                            subS = s.substring(8, 30);
                        } else {
                            subS = s.substring(10, 30);
                        }
                        subS = subS.trim();

                        try {
                            imgExtents[5] = Integer.parseInt(subS);
                        } catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of integer NAXIS6 line had " + subS);
                            throw new IOException();
                        }

                        Preferences.debug("NAXIS6 = " + imgExtents[5] + "\n", Preferences.DEBUG_FILEIO);

                        if (imgExtents[5] < 0) {
                            raFile.close();

                            MipavUtil.displayError("NAXIS5 had an illegal negative value of " + imgExtents[5]);
                            throw new IOException();
                        } else if (imgExtents[5] == 0) {
                            raFile.close();

                            MipavUtil.displayError("NAXIS5 value of 0 indicates no data is associated with the header");
                            throw new IOException();
                        }
                    }
                } // while (readAgain) looping for ninth required keyword of NAXIS6
            } // if (nDimensions >= 6)

            do {
                s = getString(80);
                count++;

                if (s.startsWith("BSCALE")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("/");

                    if (i != -1) {
                        subS = subS.substring(0, i);
                        subS = subS.trim();
                    }

                    try {
                        BSCALE = Double.parseDouble(subS);
                    } catch (final NumberFormatException e) {
                        raFile.close();

                        MipavUtil.displayError("Instead of a float BSCALE line had = " + subS);
                        throw new IOException();
                    }

                    Preferences.debug("BSCALE = " + BSCALE + "\n", Preferences.DEBUG_FILEIO);
                } // if (s.startsWith("BSCALE"))
                else if (s.startsWith("BZERO")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("/");

                    if (i != -1) {
                        subS = subS.substring(0, i);
                        subS = subS.trim();
                    }

                    try {
                        BZERO = Double.parseDouble(subS);
                    } catch (final NumberFormatException e) {
                        raFile.close();

                        MipavUtil.displayError("Instead of a float BZERO line had = " + subS);
                        throw new IOException();
                    }

                    Preferences.debug("BZERO = " + BZERO + "\n", Preferences.DEBUG_FILEIO);
                } // else if (s.startsWith("BZERO"))
                else if (s.startsWith("BLANK")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("/");

                    if (i != -1) {
                        subS = subS.substring(0, i);
                        subS = subS.trim();
                    }

                    try {
                        BLANK = Integer.parseInt(subS);
                    } catch (final NumberFormatException e) {
                        raFile.close();

                        MipavUtil.displayError("Instead of an integer BLANK line had = " + subS);
                        throw new IOException();
                    }

                    Preferences.debug("BLANK = " + BLANK + "\n", Preferences.DEBUG_FILEIO);
                    haveBlank = true;
                } // else if (s.startsWith("BLANK"))
                else if (s.startsWith("CDELT")) {
                    c = s.charAt(5);
                    if ( (c >= '0') && (c <= '9')) {
                        dimNumber = Integer.parseInt(s.substring(5, 6));
                        subS = s.substring(10, 80);
                        subS = subS.trim();
                        i = subS.indexOf("/");

                        if (i != -1) {
                            subS = subS.substring(0, i);
                            subS = subS.trim();
                        }

                        try {
                            scale[dimNumber - 1] = Double.parseDouble(subS);
                        } catch (final NumberFormatException e) {

                            Preferences.debug("Instead of a float CDELT" + s.substring(5, 6) + " line had = " + subS, 
                            		Preferences.DEBUG_FILEIO);
                        }
                    } // if ((c >= '0') && (c <= '9'))
                } // else if (s.startsWith("CDELT"))
                else if (s.startsWith("CRPIX")) {
                    c = s.charAt(5);
                    if ( (c >= '0') && (c <= '9')) {
                        dimNumber = Integer.parseInt(s.substring(5, 6));
                        subS = s.substring(10, 80);
                        subS = subS.trim();
                        i = subS.indexOf("/");

                        if (i != -1) {
                            subS = subS.substring(0, i);
                            subS = subS.trim();
                        }

                        try {
                            crpix[dimNumber - 1] = Double.parseDouble(subS);
                            haveCrpix[dimNumber - 1] = true;
                            Preferences.debug("crpix[" + (dimNumber - 1) + "] = " + crpix[dimNumber - 1] + "\n", 
                            		Preferences.DEBUG_FILEIO);
                        } catch (final NumberFormatException e) {

                            Preferences.debug("Instead of a float CRPIX" + s.substring(5, 6) + " line had = " + subS, 
                            		Preferences.DEBUG_FILEIO);
                        }
                    } // if ((c >= '0') && (c <= '9'))
                } // else if (s.startsWith("CRPIX"))
                else if (s.startsWith("CRVAL")) {
                    c = s.charAt(5);
                    if ( (c >= '0') && (c <= '9')) {
                        dimNumber = Integer.parseInt(s.substring(5, 6));
                        subS = s.substring(10, 80);
                        subS = subS.trim();
                        i = subS.indexOf("/");

                        if (i != -1) {
                            subS = subS.substring(0, i);
                            subS = subS.trim();
                        }

                        try {
                            crval[dimNumber - 1] = Double.parseDouble(subS);
                            Preferences.debug("crval[" + (dimNumber - 1) + "] = " + crval[dimNumber - 1] + "\n", 
                            		Preferences.DEBUG_FILEIO);
                            haveCrval[dimNumber - 1] = true;
                        } catch (final NumberFormatException e) {

                            Preferences.debug("Instead of a float CRVAL" + s.substring(5, 6) + " line had = " + subS, 
                            		Preferences.DEBUG_FILEIO);
                        }
                    } // if ((c >= '0') && (c <= '9'))
                } // else if (s.startsWith("CRVAL"))
                else if (s.startsWith("CTYPE")) {
                    c = s.charAt(5);
                    if ( (c >= '0') && (c <= '9')) {
                        dimNumber = Integer.parseInt(s.substring(5, 6));
                        if (dimNumber <= 5) {
                            i = s.indexOf("'");
                            j = s.lastIndexOf("'");
                            if ( (i != -1) && (j != -1)) {
                                subS = s.substring(i + 1, j);
                                subS = subS.trim();
                                Preferences.debug("CTYPE" + s.substring(5, 6) + " = " + subS + "\n", Preferences.DEBUG_FILEIO);
                                if ( (subS.equals("RGB")) && (dimNumber == 3) && (imgExtents[2] == 3)
                                        && (nDimensions == 3)) {
                                    isColorPlanar2D = true;
                                } else if (subS.toUpperCase().equals(Unit.INCHES.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MILS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MILS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MILLIMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.CENTIMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.CENTIMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.METERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.KILOMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.KILOMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MILES.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MILES.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.ANGSTROMS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.ANGSTROMS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.NANOMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MICROMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.NANOSEC.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.NANOSEC.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MICROSEC.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MICROSEC.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MILLISEC.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.SECONDS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.SECONDS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MINUTES.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MINUTES.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.HOURS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.HOURS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().substring(0, Math.min(subS.length(), 3)).equals("DEG")) {
                                    fileInfo.setUnitsOfMeasure(Unit.DEGREES.getLegacyNum(), dimNumber - 1);
                                } else {
                                    if (dimNumber == 1) {
                                        fileInfo.setCTYPE1(subS);
                                    } else if (dimNumber == 2) {
                                        fileInfo.setCTYPE2(subS);
                                    } else if (dimNumber == 3) {
                                        fileInfo.setCTYPE3(subS);
                                    } else if (dimNumber == 4) {
                                        fileInfo.setCTYPE4(subS);
                                    } else if (dimNumber == 5) {
                                        fileInfo.setCTYPE5(subS);
                                    }
                                }
                            }
                        } // if (dimNumber <= 5)
                    } // if ((c >= '0') && (c <= '9'))
                } // else if (s.startsWith("CTYPE"))
                else if (s.startsWith("CUNIT")) {
                    c = s.charAt(5);
                    if ( (c >= '0') && (c <= '9')) {
                        dimNumber = Integer.parseInt(s.substring(5, 6));
                        if (dimNumber <= 5) {
                            i = s.indexOf("'");
                            j = s.lastIndexOf("'");
                            if ( (i != -1) && (j != -1)) {
                                subS = s.substring(i + 1, j);
                                subS = subS.trim();
                                Preferences.debug("CUNIT" + s.substring(5, 6) + " = " + subS + "\n", Preferences.DEBUG_FILEIO);

                                if (subS.toUpperCase().equals(Unit.INCHES.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MILS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MILS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MILLIMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.CENTIMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.CENTIMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.METERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.METERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.KILOMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.KILOMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MILES.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MILES.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.ANGSTROMS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.ANGSTROMS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.NANOMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.NANOMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MICROMETERS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MICROMETERS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.NANOSEC.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.NANOSEC.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MICROSEC.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MICROSEC.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MILLISEC.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.SECONDS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.SECONDS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.MINUTES.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.MINUTES.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().equals(Unit.HOURS.toString().toUpperCase())) {
                                    fileInfo.setUnitsOfMeasure(Unit.HOURS.getLegacyNum(), dimNumber - 1);
                                } else if (subS.toUpperCase().substring(0, Math.min(subS.length(), 3)).equals("DEG")) {
                                    fileInfo.setUnitsOfMeasure(Unit.DEGREES.getLegacyNum(), dimNumber - 1);
                                } else {
                                    if (dimNumber == 1) {
                                        fileInfo.setCUNIT1(subS);
                                    } else if (dimNumber == 2) {
                                        fileInfo.setCUNIT2(subS);
                                    } else if (dimNumber == 3) {
                                        fileInfo.setCUNIT3(subS);
                                    } else if (dimNumber == 4) {
                                        fileInfo.setCUNIT4(subS);
                                    } else if (dimNumber == 5) {
                                        fileInfo.setCUNIT5(subS);
                                    }
                                }
                            }
                        } // if (dimNumber <= 5)
                    } // if ((c >= '0') && (c <= '9'))
                } // else if (s.startsWith("CUNIT"))
                else if (s.startsWith("COMMENT")) {
                    subS = s.substring(8, 80);
                    subS = subS.trim();
                    Preferences.debug("COMMENT = " + subS + "\n", Preferences.DEBUG_FILEIO);
                } // else if (s.startsWith("COMMENT"))
                else if (s.startsWith("HISTORY")) {
                    subS = s.substring(8, 80);
                    subS = subS.trim();
                    Preferences.debug("HISTORY = " + subS + "\n", Preferences.DEBUG_FILEIO);
                } // else if (s.startsWith("HISTORY"))
                else if (s.startsWith("DATE-OBS")) {

                    // Date data was acquired
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    numApos = 0;
                    aposLoc = new int[6];
                    for (i = 0; i < subS.length(); i++) {
                        if (subS.substring(i, i + 1).indexOf("'") == 0) {
                            aposLoc[numApos++] = i;
                        }
                    }
                    if (numApos == 0) {
                        Preferences.debug("DATE-OBS, date data acquired = " + subS + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setDateAcquired(subS);
                    } else if (numApos == 2) {
                        dateString = subS.substring(aposLoc[0] + 1, aposLoc[1]).trim();
                        haveTime = false;
                        for (i = 0; i < subS.length(); i++) {
                            if (subS.substring(i, i + 1) == "T") {
                                haveTime = true;
                                j = i;
                            }
                        }
                        if (haveTime) {
                            timeString = dateString.substring(j + 1);
                            Preferences.debug("TIME data acquired = " + timeString + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setTimeAcquired(timeString);
                            dateString = dateString.substring(0, j);
                        }
                        Preferences.debug("DATE-OBS, date data acquired = " + dateString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setDateAcquired(dateString);
                    } else if (numApos == 4) {
                        dateString = subS.substring(aposLoc[0] + 1, aposLoc[1]).trim();
                        Preferences.debug("DATE-OBS, date data acquired = " + dateString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setDateAcquired(dateString);
                        timeString = subS.substring(aposLoc[2] + 1, aposLoc[3]).trim();
                        Preferences.debug("TIME data acquired = " + timeString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setTimeAcquired(timeString);
                    }
                } // else if (s.startsWith("DATE-OBS"))
                else if (s.startsWith("TIME-OBS")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("'");
                    j = subS.lastIndexOf("'");
                    if ( (i != -1) && (j != -1) && (i != j)) {
                        subS = subS.substring(i + 1, j);
                        subS = subS.trim();
                    }
                    Preferences.debug("TIME data acquired = " + subS + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setTimeAcquired(subS);
                } // else if (s.startsWith("TIME-OBS"))
                else if (s.startsWith("DATE-MAP")) {

                    // Date data was last processed
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    numApos = 0;
                    aposLoc = new int[6];
                    for (i = 0; i < subS.length(); i++) {
                        if (subS.substring(i, i + 1).indexOf("'") == 0) {
                            aposLoc[numApos++] = i;
                        }
                    }
                    if (numApos == 0) {
                        Preferences.debug("DATE-MAP, date data last processed = " + subS + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setDateProcessed(subS);
                    } else if (numApos == 2) {
                        dateString = subS.substring(aposLoc[0] + 1, aposLoc[1]).trim();
                        haveTime = false;
                        for (i = 0; i < subS.length(); i++) {
                            if (subS.substring(i, i + 1) == "T") {
                                haveTime = true;
                                j = i;
                            }
                        }
                        if (haveTime) {
                            timeString = dateString.substring(j + 1);
                            Preferences.debug("TIME data last processed = " + timeString + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setTimeProcessed(timeString);
                            dateString = dateString.substring(0, j);
                        }
                        Preferences.debug("DATE-MAP, date data last processed = " + dateString + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        fileInfo.setDateProcessed(dateString);
                    } else if (numApos == 4) {
                        dateString = subS.substring(aposLoc[0] + 1, aposLoc[1]).trim();
                        Preferences.debug("DATE-OBS, date data last processed = " + dateString + "\n",
                        		Preferences.DEBUG_FILEIO);
                        fileInfo.setDateProcessed(dateString);
                        timeString = subS.substring(aposLoc[2] + 1, aposLoc[3]).trim();
                        Preferences.debug("TIME data last processed = " + timeString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setTimeProcessed(timeString);
                    }
                } // else if (s.startsWith("DATE-MAP"))
                else if (s.startsWith("DATE")) {

                    // Date file was written
                    // Could have 'date' 'time' 'jobname'
                    // Could have 'dateTtime' with a capitalT separating date and time
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    numApos = 0;
                    aposLoc = new int[6];
                    for (i = 0; i < subS.length(); i++) {
                        if (subS.substring(i, i + 1).indexOf("'") == 0) {
                            aposLoc[numApos++] = i;
                        }
                    }
                    if (numApos == 0) {
                        Preferences.debug("DATE file written = " + subS + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setDateWritten(subS);
                    } else if (numApos == 2) {
                        dateString = subS.substring(aposLoc[0] + 1, aposLoc[1]).trim();
                        haveTime = false;
                        for (i = 0; i < subS.length(); i++) {
                            if (subS.substring(i, i + 1) == "T") {
                                haveTime = true;
                                j = i;
                            }
                        }
                        if (haveTime) {
                            timeString = dateString.substring(j + 1);
                            Preferences.debug("TIME file written = " + timeString + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setTimeWritten(timeString);
                            dateString = dateString.substring(0, j);
                        }
                        Preferences.debug("DATE file written = " + dateString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setDateWritten(dateString);
                    } else if (numApos == 4) {
                        dateString = subS.substring(aposLoc[0] + 1, aposLoc[1]).trim();
                        Preferences.debug("DATE file written = " + dateString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setDateWritten(dateString);
                        timeString = subS.substring(aposLoc[2] + 1, aposLoc[3]).trim();
                        Preferences.debug("TIME file written = " + timeString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setTimeWritten(timeString);
                    } else if (numApos == 6) {
                        dateString = subS.substring(aposLoc[0] + 1, aposLoc[1]).trim();
                        Preferences.debug("DATE file written = " + dateString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setDateWritten(dateString);
                        timeString = subS.substring(aposLoc[2] + 1, aposLoc[3]).trim();
                        Preferences.debug("TIME file written = " + timeString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setTimeWritten(timeString);
                        jobNameString = subS.substring(aposLoc[4] + 1, aposLoc[5]).trim();
                        Preferences.debug("JOB NAME = " + jobNameString + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setJobName(jobNameString);
                    }
                } // else if (s.startsWith("DATE"))
                else if (s.startsWith("ORIGIN")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("'");
                    j = subS.lastIndexOf("'");
                    if ( (i != -1) && (j != -1) && (i != j)) {
                        subS = subS.substring(i + 1, j);
                        subS = subS.trim();
                    }
                    Preferences.debug("ORIGIN, installation where file is written = " + subS + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setOrigin(subS);
                } // else if (s.startsWith("ORIGIN"))
                else if (s.startsWith("INSTRUME")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("'");
                    j = subS.lastIndexOf("'");
                    if ( (i != -1) && (j != -1) && (i != j)) {
                        subS = subS.substring(i + 1, j);
                        subS = subS.trim();
                    }
                    Preferences.debug("INSTRUMENT, data acquisition instrument = " + subS + "\n", Preferences.DEBUG_FILEIO);
                    if (subS.length() != 0) {
                        fileInfo.setInstrument(subS);
                    }
                } // else if (s.startsWith("INSTRUMEN"))
                else if (s.startsWith("OBSERVER")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("'");
                    j = subS.lastIndexOf("'");
                    if ( (i != -1) && (j != -1) && (i != j)) {
                        subS = subS.substring(i + 1, j);
                        subS = subS.trim();
                    }
                    Preferences.debug("OBSERVER = " + subS + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setObserver(subS);
                } // else if (s.startsWith("OBSERVER"))
                else if (s.startsWith("OBJECT")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("'");
                    j = subS.lastIndexOf("'");
                    if ( (i != -1) && (j != -1) && (i != j)) {
                        subS = subS.substring(i + 1, j);
                        subS = subS.trim();
                    }
                    Preferences.debug("OBJECT observed = " + subS + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setObject(subS);
                } // else if (s.startsWith("OBJECT"))
                else if (s.startsWith("AUTHOR")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("'");
                    j = subS.lastIndexOf("'");
                    if ( (i != -1) && (j != -1) && (i != j)) {
                        subS = subS.substring(i + 1, j);
                        subS = subS.trim();
                    }
                    Preferences.debug("AUTHOR = " + subS + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setAuthor(subS);
                } // else if (s.startsWith("AUTHOR")
                else if (s.startsWith("REFERENC")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    Preferences.debug("REFERENC = " + subS + "\n", Preferences.DEBUG_FILEIO);
                } // else if (s.startsWith("REFERENC"))
                else if (s.startsWith("F_RATIO")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("/");

                    if (i != -1) {
                        subS = subS.substring(0, i);
                        subS = subS.trim();
                    }

                    try {
                        focalRatio = Float.parseFloat(subS);
                        Preferences.debug("Focal ratio = " + focalRatio + "\n", Preferences.DEBUG_FILEIO);
                        fileInfo.setFocalRatio(focalRatio);
                    } catch (final NumberFormatException e) {

                        Preferences.debug("Instead of a float F_RATIO line had = " + subS, Preferences.DEBUG_FILEIO);
                    }
                } // else if (s.startsWith("F_RATIO"))
                else if (s.startsWith("BUNIT")) {
                    subS = s.substring(10, 80);
                    subS = subS.trim();
                    i = subS.indexOf("'");
                    j = subS.lastIndexOf("'");
                    if ( (i != -1) && (j != -1) && (i != j)) {
                        subS = subS.substring(i + 1, j);
                        subS = subS.trim();
                    }
                    Preferences.debug("BUNIT = " + subS + "\n", Preferences.DEBUG_FILEIO);
                    if (subS.length() != 0) {
                        fileInfo.setBUNIT(subS);
                    }
                } // else if (s.startsWith("BUNIT"))
            } while ( !s.startsWith("END"));

            for (i = nDimensions - 1; i >= 0; i--) {
                if (imgExtents[i] == 1) {
                    scale[i] = 1.0;
                    reducedExtents = new int[nDimensions - 1];
                    reducedScale = new double[nDimensions - 1];
                    reducedCrpix = new double[nDimensions - 1];
                    reducedHaveCrpix = new boolean[nDimensions - 1];
                    reducedCrval = new double[nDimensions - 1];
                    reducedHaveCrval = new boolean[nDimensions - 1];
                    for (j = 0; j < i; j++) {
                        reducedExtents[j] = imgExtents[j];
                        reducedScale[j] = scale[j];
                        reducedCrpix[j] = crpix[j];
                        reducedHaveCrpix[j] = haveCrpix[j];
                        reducedCrval[j] = crval[j];
                        reducedHaveCrval[j] = haveCrval[j];
                    }
                    for (j = i + 1; j < nDimensions; j++) {
                        reducedExtents[j - 1] = imgExtents[j];
                        reducedScale[j - 1] = scale[j];
                        reducedCrpix[j - 1] = crpix[j];
                        reducedHaveCrpix[j - 1] = haveCrpix[j];
                        reducedCrval[j - 1] = crval[j];
                        reducedHaveCrval[j - 1] = haveCrval[j];
                        if (j <= 5) {
                            fileInfo.setUnitsOfMeasure(j - 1, fileInfo.getUnitsOfMeasure(j));
                        } else {
                            fileInfo.setUnitsOfMeasure(j - 1, Unit.UNKNOWN_MEASURE.getLegacyNum());
                        }
                    }
                    nDimensions--;
                    imgExtents = new int[nDimensions];
                    for (j = 0; j < nDimensions; j++) {
                        imgExtents[j] = reducedExtents[j];
                        scale[j] = reducedScale[j];
                        crpix[j] = reducedCrpix[j];
                        haveCrpix[j] = reducedHaveCrpix[j];
                        crval[j] = reducedCrval[j];
                        haveCrval[j] = reducedHaveCrval[j];
                    }
                }
            }

            if (isColorPlanar2D || one) {
                reducedExtents = new int[2];
                reducedScale = new double[2];
                reducedCrpix = new double[2];
                reducedHaveCrpix = new boolean[2];
                reducedCrval = new double[2];
                reducedHaveCrval = new boolean[2];
                for (j = 0; j < 2; j++) {
                    reducedExtents[j] = imgExtents[j];
                    reducedScale[j] = scale[j];
                    reducedCrpix[j] = crpix[j];
                    reducedHaveCrpix[j] = haveCrpix[j];
                    reducedCrval[j] = crval[j];
                    reducedHaveCrval[j] = haveCrval[j];
                }
                nDimensions = 2;
                imgExtents = new int[2];
                for (j = 0; j < 2; j++) {
                    imgExtents[j] = reducedExtents[j];
                    scale[j] = reducedScale[j];
                    crpix[j] = reducedCrpix[j];
                    haveCrpix[j] = reducedHaveCrpix[j];
                    crval[j] = reducedCrval[j];
                    haveCrval[j] = reducedHaveCrval[j];
                }
            }

            if (nDimensions > 4) {
                raFile.close();

                MipavUtil.displayError("MIPAV cannot display an image with " + nDimensions + " dimensions");
                throw new IOException();
            } else if (nDimensions == 1) {
                raFile.close();

                MipavUtil.displayError("MIPAV cannot display an image with 1 dimension");
                throw new IOException();
            } else if (nDimensions == 0) {
                raFile.close();

                MipavUtil
                        .displayError("NAXIS value of 0 indicates no binary data matrix is associated with the header");
                throw new IOException();
            }

            for (i = 0; i < nDimensions; i++) {
                imgResols[i] = (float) Math.abs(scale[i]);
            }

            fileInfo.setExtents(imgExtents);

            // Note that in these files the imgResols values may differ by orders of magnitude
            // If the maximum and minimum imgResols differ by more than 10,
            // arbitrarily set all imgResols to 1.
            minResol = imgResols[0];
            maxResol = imgResols[0];

            for (i = 1; i < nDimensions; i++) {

                if (imgResols[i] > maxResol) {
                    maxResol = imgResols[i];
                }

                if (imgResols[i] < minResol) {
                    minResol = imgResols[i];
                }
            } // for (i = 1; i < nDimensions; i++)

            fileInfo.setResolutions(imgResols);

            origin = new double[nDimensions];
            for (i = 0; i < nDimensions; i++) {
                if (haveCrpix[i] && haveCrval[i]) {
                    origin[i] = crval[i] - (crpix[i] - 0.5) * scale[i];
                    fileInfo.setOrigin((float) origin[i], i);
                }
            }

            matrix = new TransMatrix(Math.min(4, nDimensions + 1));
            for (i = 0; i < Math.min(3, nDimensions); i++) {
                matrix.set(i, i, scale[i]);
                matrix.set(i, Math.min(nDimensions, 3), origin[i]);
            }
            matrix.setTransformID(TransMatrix.TRANSFORM_UNKNOWN);

            offset = 2880 + (2880 * ( (count * 80) / 2880));
            raFile.seek(offset);

            if ( ( (BSCALE != 1.0) || (BZERO != 0.0))
                    && ( (sourceType == ModelStorageBase.UBYTE) || (sourceType == ModelStorageBase.SHORT) || (sourceType == ModelStorageBase.INTEGER))) {

                if (one) {
                    image = new ModelImage(ModelStorageBase.FLOAT, new int[] {imgExtents[0], imgExtents[1]}, fileInfo
                            .getFileName());
                } else {
                    image = new ModelImage(ModelStorageBase.FLOAT, imgExtents, fileInfo.getFileName());
                }

                fileInfo.setDataType(ModelStorageBase.FLOAT);
            } else if ( (isColorPlanar2D) && (sourceType == ModelStorageBase.UBYTE)) {
                sourceType = ModelStorageBase.ARGB;
                image = new ModelImage(ModelStorageBase.ARGB, imgExtents, fileInfo.getFileName());
                fileInfo.setDataType(ModelStorageBase.ARGB);
            } else {

                if (one) {
                    image = new ModelImage(sourceType, new int[] {imgExtents[0], imgExtents[1]}, fileInfo.getFileName());
                } else {
                    image = new ModelImage(sourceType, imgExtents, fileInfo.getFileName());
                }

                fileInfo.setDataType(sourceType);
            }
            image.setMatrix(matrix);

            if ( (nDimensions == 2) || one) {
                numberSlices = 1;
            } else if (nDimensions == 3) {
                numberSlices = imgExtents[2];
            } else {
                numberSlices = imgExtents[2] * imgExtents[3];
            }

            if (one) {

                if (imgExtents.length > 2) {
                    int skip = imgExtents[0] * imgExtents[1] * ( (imgExtents[2] / 2) - 1);

                    if ( (sourceType == ModelStorageBase.SHORT) || (sourceType == ModelStorageBase.USHORT)) {
                        skip *= 2;
                    } else if ( (sourceType == ModelStorageBase.FLOAT) || (sourceType == ModelStorageBase.INTEGER)
                            || (sourceType == ModelStorageBase.UINTEGER) || (sourceType == ModelStorageBase.ARGB)) {
                        skip *= 4;
                    } else if (sourceType == ModelStorageBase.DOUBLE) {
                        skip *= 8;
                    }

                    raFile.seek(offset + skip);
                }
            }

            if (sourceType != ModelStorageBase.DOUBLE) {
                bufferSize = imgExtents[0] * imgExtents[1];
                if (image.isColorImage()) {
                    bufferSize *= 4;
                }
                imgBuffer = new float[bufferSize];

                image.setFileInfo(fileInfo, 0);
                readBuffer(0, imgBuffer, (float) BSCALE, (float) BZERO);
                image.importData(0, imgBuffer, false);
                if (image.getNDims() == 3) {
                    for (i = 1; i < numberSlices; i++) {
                        fileInfoCopy = (FileInfoFits) fileInfo.clone();
                        fileInfoCopy.setOrigin((float) (origin[2] + i * imgResols[2]), 2);
                        image.setFileInfo(fileInfoCopy, i);
                        readBuffer(i, imgBuffer, (float) BSCALE, (float) BZERO);
                        image.importData(i * bufferSize, imgBuffer, false);
                    }
                } else if (image.getNDims() > 3) {
                    for (i = 0; i < imgExtents[3]; i++) {
                        for (j = 0; j < imgExtents[2]; j++) {
                            if ( (i != 0) || (j != 0)) {
                                fileInfoCopy = (FileInfoFits) fileInfo.clone();
                                fileInfoCopy.setOrigin((float) (origin[2] + j * imgResols[2]), 2);
                                fileInfoCopy.setOrigin((float) (origin[3] + i * imgResols[3]), 3);
                                image.setFileInfo(fileInfoCopy, j + i * imgExtents[2]);
                                readBuffer(j + i * imgExtents[2], imgBuffer, (float) BSCALE, (float) BZERO);
                                image.importData( (j + i * imgExtents[2]) * bufferSize, imgBuffer, false);
                            }
                        }
                    }
                }
            } // if (sourceType != ModelStorageBase.DOUBLE)
            else { // sourceType == ModelStorageBase.DOUBLE
                bufferSize = imgExtents[0] * imgExtents[1];
                imgDBuffer = new double[bufferSize];
                image.setFileInfo(fileInfo, 0);
                readDBuffer(0, imgDBuffer, (float) BSCALE, (float) BZERO);
                image.importData(0, imgDBuffer, false);
                if (image.getNDims() == 3) {
                    for (i = 1; i < numberSlices; i++) {
                        fileInfoCopy = (FileInfoFits) fileInfo.clone();
                        fileInfoCopy.setOrigin((float) (origin[2] + i * imgResols[2]), 2);
                        image.setFileInfo(fileInfoCopy, i);
                        readDBuffer(i, imgDBuffer, (float) BSCALE, (float) BZERO);
                        image.importData(i * bufferSize, imgDBuffer, false);
                    }
                } else if (image.getNDims() > 3) {
                    for (i = 0; i < imgExtents[3]; i++) {
                        for (j = 0; j < imgExtents[2]; j++) {
                            if ( (i != 0) || (j != 0)) {
                                fileInfoCopy = (FileInfoFits) fileInfo.clone();
                                fileInfoCopy.setOrigin((float) (origin[2] + j * imgResols[2]), 2);
                                fileInfoCopy.setOrigin((float) (origin[3] + i * imgResols[3]), 3);
                                image.setFileInfo(fileInfoCopy, j + i * imgExtents[2]);
                                readDBuffer(j + i * imgExtents[2], imgDBuffer, (float) BSCALE, (float) BZERO);
                                image.importData( (j + i * imgExtents[2]) * bufferSize, imgDBuffer, false);
                            }
                        }
                    }
                }
            } // else sourceType == ModelStorageBase.DOUBLE

            image.calcMinMax();
            raFile.close();

            flipAlgo = new AlgorithmFlip(image, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE, true);
            flipAlgo.run();
            flipAlgo.finalize();
            flipAlgo = null;
            return image;
        } catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw new IOException();
        }
    }

    /**
     * Writes a FITS format type image.
     * 
     * @param image Image model of data to write.
     * @param options options such as starting and ending slices and times
     * 
     * @exception IOException if there is an error writing the file
     */
    public void writeImage(final ModelImage image, final FileWriteOptions options) throws IOException {
        final byte[] cardImage = new byte[80];
        int i, j;
        String axisSize;
        byte[] axisBytes;
        String resString;
        byte[] resBytes;
        byte[] byteBuffer;
        short[] shortBuffer;
        int[] intBuffer;
        float[] floatBuffer;
        double[] doubleBuffer;
        int sliceSize;
        int tmpInt;
        long tmpLong;
        int[] measure;
        String mString;
        byte[] mBytes;
        int sBegin; // first z slice to write
        int sEnd; // last z slice to write
        int tBegin; // first t time to write
        int tEnd; // last t time to write
        int z, t;
        int zDim;
        int count;
        // double array[][];

        flipAlgo = new AlgorithmFlip(image, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE, true);
        flipAlgo.run();
        flipAlgo.finalize();
        flipAlgo = null;

        if (image.getNDims() >= 3) {
            sBegin = options.getBeginSlice();
            sEnd = options.getEndSlice();
        } else {
            sBegin = 0;
            sEnd = 0;
        }

        if (image.getNDims() == 4) {
            tBegin = options.getBeginTime();
            tEnd = options.getEndTime();
        } else {
            tBegin = 0;
            tEnd = 0;
        }

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "rw");
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);

        endianess = FileBase.BIG_ENDIAN; // true

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 83; // S
        cardImage[1] = 73; // I
        cardImage[2] = 77; // M
        cardImage[3] = 80; // P
        cardImage[4] = 76; // L
        cardImage[5] = 69; // E
        cardImage[8] = 61; // =
        cardImage[29] = 84; // T
        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32;
        }

        cardImage[0] = 66; // B
        cardImage[1] = 73; // I
        cardImage[2] = 84; // T
        cardImage[3] = 80; // P
        cardImage[4] = 73; // I
        cardImage[5] = 88; // X
        cardImage[8] = 61; // =

        switch (image.getType()) {

            case ModelStorageBase.UBYTE:
            case ModelStorageBase.ARGB:

                // store as 8-bit unsigned
                cardImage[29] = 56; // 8
                break;

            case ModelStorageBase.BYTE:
            case ModelStorageBase.SHORT:

                // store as 16 bit signed short
                cardImage[28] = 49; // 1
                cardImage[29] = 54; // 6
                break;

            case ModelStorageBase.USHORT:
            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:

                // store as 32 bit signed integer
                cardImage[28] = 51; // 3
                cardImage[29] = 50; // 2
                break;

            case ModelStorageBase.FLOAT:

                // store as 32 bit IEEE float
                cardImage[27] = 45; // -
                cardImage[28] = 51; // 3
                cardImage[29] = 50; // 2
                break;

            case ModelStorageBase.DOUBLE:

                // store as 64 bit IEEE double precision float
                cardImage[27] = 45; // -
                cardImage[28] = 54; // 6
                cardImage[29] = 52; // 4
                break;

            default:
                raFile.close();

                MipavUtil.displayError("Cannot save this image type in FITS format");
                throw new IOException();
        } // switch(image.getType())

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 78; // N
        cardImage[1] = 65; // A
        cardImage[2] = 88; // X
        cardImage[3] = 73; // I
        cardImage[4] = 83; // S
        cardImage[8] = 61; // =

        switch (image.getNDims()) {

            case 2:
                if (image.getType() == ModelStorageBase.ARGB) {
                    cardImage[29] = 51; // 3
                } else {
                    cardImage[29] = 50; // 2
                }
                break;

            case 3:
                cardImage[29] = 51; // 3
                break;

            case 4:
                cardImage[29] = 52; // 4
                break;

            default:
                raFile.close();

                MipavUtil.displayError("Cannot save file with " + image.getNDims() + " dimensions");
                throw new IOException();
        } // switch(image.getNDims())

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 78; // N
        cardImage[1] = 65; // A
        cardImage[2] = 88; // X
        cardImage[3] = 73; // I
        cardImage[4] = 83; // S
        cardImage[5] = 49; // 1
        cardImage[8] = 61; // =

        axisSize = Integer.toString(image.getExtents()[0]);
        axisBytes = axisSize.getBytes();

        for (i = 0; i < axisSize.length(); i++) {
            cardImage[30 + i - axisSize.length()] = axisBytes[i];
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 78; // N
        cardImage[1] = 65; // A
        cardImage[2] = 88; // X
        cardImage[3] = 73; // I
        cardImage[4] = 83; // S
        cardImage[5] = 50; // 2
        cardImage[8] = 61; // =

        axisSize = Integer.toString(image.getExtents()[1]);
        axisBytes = axisSize.getBytes();

        for (i = 0; i < axisSize.length(); i++) {
            cardImage[30 + i - axisSize.length()] = axisBytes[i];
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        if ( (image.getNDims() >= 3) || ( (image.getNDims() == 2) && (image.getType() == ModelStorageBase.ARGB))) {
            cardImage[0] = 78; // N
            cardImage[1] = 65; // A
            cardImage[2] = 88; // X
            cardImage[3] = 73; // I
            cardImage[4] = 83; // S
            cardImage[5] = 51; // 3
            cardImage[8] = 61; // =

            if (image.getType() == ModelStorageBase.ARGB) {
                axisSize = "3";
            } else {
                axisSize = Integer.toString(sEnd - sBegin + 1);
            }
            axisBytes = axisSize.getBytes();

            for (i = 0; i < axisSize.length(); i++) {
                cardImage[30 + i - axisSize.length()] = axisBytes[i];
            }

        } // if ((image.getNDims() >= 3) || ((image.getNDims() == 2) && (image.getType() == ModelStorageBase.ARGB)))

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        if (image.getNDims() == 4) {
            cardImage[0] = 78; // N
            cardImage[1] = 65; // A
            cardImage[2] = 88; // X
            cardImage[3] = 73; // I
            cardImage[4] = 83; // S
            cardImage[5] = 52; // 4
            cardImage[8] = 61; // =

            axisSize = Integer.toString(tEnd - tBegin + 1);
            axisBytes = axisSize.getBytes();

            for (i = 0; i < axisSize.length(); i++) {
                cardImage[30 + i - axisSize.length()] = axisBytes[i];
            }
        } // if (image.getNDims() == 4)

        raFile.write(cardImage);

        final TransMatrix image_TM = image.getMatrix();

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 67; // C
        cardImage[1] = 68; // D
        cardImage[2] = 69; // E
        cardImage[3] = 76; // L
        cardImage[4] = 84; // T
        cardImage[5] = 49; // 1
        cardImage[8] = 61; // =

        resString = Float.toString(image.getFileInfo()[0].getResolutions()[0]);
        resBytes = resString.getBytes();

        for (i = 0; i < resString.length(); i++) {
            cardImage[30 + i - resString.length()] = resBytes[i];
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 67; // C
        cardImage[1] = 82; // R
        cardImage[2] = 80; // P
        cardImage[3] = 73; // I
        cardImage[4] = 88; // X
        cardImage[5] = 49; // 1
        cardImage[8] = 61; // =

        resString = Double.toString(0.5);
        resBytes = resString.getBytes();

        for (i = 0; i < resString.length(); i++) {
            cardImage[30 + i - resString.length()] = resBytes[i];
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 67; // C
        cardImage[1] = 82; // R
        cardImage[2] = 86; // V
        cardImage[3] = 65; // A
        cardImage[4] = 76; // L
        cardImage[5] = 49; // 1
        cardImage[8] = 61; // =

        resString = Double.toString(image_TM.get(0, Math.min(image.getNDims(), 3)));
        resBytes = resString.getBytes();

        for (i = 0; i < resString.length(); i++) {
            cardImage[30 + i - resString.length()] = resBytes[i];
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 67; // C
        cardImage[1] = 68; // D
        cardImage[2] = 69; // E
        cardImage[3] = 76; // L
        cardImage[4] = 84; // T
        cardImage[5] = 50; // 2
        cardImage[8] = 61; // =

        resString = Float.toString(image.getFileInfo()[0].getResolutions()[1]);
        resBytes = resString.getBytes();

        for (i = 0; i < resString.length(); i++) {
            cardImage[30 + i - resString.length()] = resBytes[i];
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 67; // C
        cardImage[1] = 82; // R
        cardImage[2] = 80; // P
        cardImage[3] = 73; // I
        cardImage[4] = 88; // X
        cardImage[5] = 50; // 2
        cardImage[8] = 61; // =

        resString = Double.toString(0.5);
        resBytes = resString.getBytes();

        for (i = 0; i < resString.length(); i++) {
            cardImage[30 + i - resString.length()] = resBytes[i];
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 67; // C
        cardImage[1] = 82; // R
        cardImage[2] = 86; // V
        cardImage[3] = 65; // A
        cardImage[4] = 76; // L
        cardImage[5] = 50; // 2
        cardImage[8] = 61; // =

        resString = Double.toString(image_TM.get(1, Math.min(image.getNDims(), 3)));
        resBytes = resString.getBytes();

        for (i = 0; i < resString.length(); i++) {
            cardImage[30 + i - resString.length()] = resBytes[i];
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }
        if ( (image.getNDims() >= 3) || ( (image.getNDims() == 2) && (image.getType() == ModelStorageBase.ARGB))) {

            cardImage[0] = 67; // C
            cardImage[1] = 68; // D
            cardImage[2] = 69; // E
            cardImage[3] = 76; // L
            cardImage[4] = 84; // T
            cardImage[5] = 51; // 3
            cardImage[8] = 61; // =

            if (image.getType() == ModelStorageBase.ARGB) {
                resString = "1.0";
            } else {
                resString = Float.toString(image.getFileInfo()[0].getResolutions()[2]);
            }
            resBytes = resString.getBytes();

            for (i = 0; i < resString.length(); i++) {
                cardImage[30 + i - resString.length()] = resBytes[i];
            }
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        if (image.getNDims() >= 3) {

            cardImage[0] = 67; // C
            cardImage[1] = 82; // R
            cardImage[2] = 80; // P
            cardImage[3] = 73; // I
            cardImage[4] = 88; // X
            cardImage[5] = 51; // 3
            cardImage[8] = 61; // =

            resString = Double.toString(0.5);
            resBytes = resString.getBytes();

            for (i = 0; i < resString.length(); i++) {
                cardImage[30 + i - resString.length()] = resBytes[i];
            }
        }

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        if (image.getNDims() >= 3) {

            cardImage[0] = 67; // C
            cardImage[1] = 82; // R
            cardImage[2] = 86; // V
            cardImage[3] = 65; // A
            cardImage[4] = 76; // L
            cardImage[5] = 51; // 3
            cardImage[8] = 61; // =

            resString = Double.toString(image_TM.get(2, Math.min(image.getNDims(), 3)));
            resBytes = resString.getBytes();

            for (i = 0; i < resString.length(); i++) {
                cardImage[30 + i - resString.length()] = resBytes[i];
            }

        } // if ((image.getNDims() >= 3)

        raFile.write(cardImage);

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        if (image.getNDims() == 4) {

            cardImage[0] = 67; // C
            cardImage[1] = 68; // D
            cardImage[2] = 69; // E
            cardImage[3] = 76; // L
            cardImage[4] = 84; // T
            cardImage[5] = 52; // 4
            cardImage[8] = 61; // =

            resString = Float.toString(image.getFileInfo()[0].getResolutions()[3]);
            resBytes = resString.getBytes();

            for (i = 0; i < resString.length(); i++) {
                cardImage[30 + i - resString.length()] = resBytes[i];
            }

        } // if (image.getNDims() == 4)

        raFile.write(cardImage);

        measure = image.getFileInfo()[0].getUnitsOfMeasure();

        for (i = 0; i < 4; i++) {

            for (j = 0; j < 80; j++) {
                cardImage[j] = 32; // fill with ascii spaces
            }

            if ( ( (image.getNDims() >= (i + 1)) && (image.getFileInfo()[0].getResolutions()[i] > 0.0))
                    || ( (i == 2) && (image.getType() == ModelStorageBase.ARGB) && (image.getNDims() == 2))) {

                if (image.getType() == ModelStorageBase.ARGB && (i == 2)) {
                    mString = "RGB";
                } else {
                    mString = Unit.getUnitFromLegacyNum(measure[i]).toString(); 
                }

                if ( (image.getType() == ModelStorageBase.ARGB) && (i == 2)) {
                    cardImage[0] = 67; // C
                    cardImage[1] = 84; // T
                    cardImage[2] = 89; // Y
                    cardImage[3] = 80; // P
                    cardImage[4] = 69; // E
                } else {
                    cardImage[0] = 67; // C
                    cardImage[1] = 85; // U
                    cardImage[2] = 78; // N
                    cardImage[3] = 73; // I
                    cardImage[4] = 84; // T
                }
                cardImage[5] = (byte) (49 + i); // 1 = 49, 2 = 50, 3 = 51, 4 = 52
                cardImage[8] = 61; // =
                cardImage[10] = 39; // '
                mBytes = mString.getBytes();

                for (j = 0; j < mString.length(); j++) {
                    cardImage[11 + j] = mBytes[j];
                }

                cardImage[11 + mString.length()] = 39; // '

            } // if (((image.getNDims() >= (i+1)) && (image.getFileInfo()[0].getResolutions()[i] > 0.0)) ||

            raFile.write(cardImage);

        } // for (i=0; i < 4; i++)

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }

        cardImage[0] = 69; // E
        cardImage[1] = 78; // N
        cardImage[2] = 68; // D
        raFile.write(cardImage);

        // The data starts at byte 2880 or after 36 80-byte lines have been written.
        // 22 have been written, so write 14 more.

        cardImage[0] = 32;
        cardImage[1] = 32;
        cardImage[2] = 32;

        for (i = 0; i < 14; i++) {
            raFile.write(cardImage);
        }

        if (image.getNDims() == 2) {
            numberSlices = 1;
            zDim = 1;
        } else if (image.getNDims() == 3) {
            numberSlices = sEnd - sBegin + 1;
            zDim = image.getExtents()[2];
        } else {
            numberSlices = (sEnd - sBegin + 1) * (tEnd - tBegin + 1);
            zDim = image.getExtents()[2];
        }

        sliceSize = image.getSliceSize();

        count = 0;

        switch (image.getType()) {

            case ModelStorageBase.UBYTE:

                // store as 8-bit unsigned
                byteBuffer = new byte[sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, byteBuffer);
                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.BYTE:
            case ModelStorageBase.SHORT:

                // store as 16 bit signed short
                shortBuffer = new short[sliceSize];
                byteBuffer = new byte[2 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, shortBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                            byteBuffer[ (2 * j) + 1] = (byte) (shortBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.USHORT:
            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:

                // store as 32 bit signed integer
                intBuffer = new int[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, intBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[4 * j] = (byte) (intBuffer[j] >>> 24);
                            byteBuffer[ (4 * j) + 1] = (byte) (intBuffer[j] >>> 16);
                            byteBuffer[ (4 * j) + 2] = (byte) (intBuffer[j] >>> 8);
                            byteBuffer[ (4 * j) + 3] = (byte) (intBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.FLOAT:

                // store as 32 bit float
                floatBuffer = new float[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, floatBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                            byteBuffer[ (4 * j) + 1] = (byte) (tmpInt >>> 16);
                            byteBuffer[ (4 * j) + 2] = (byte) (tmpInt >>> 8);
                            byteBuffer[ (4 * j) + 3] = (byte) (tmpInt);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.DOUBLE:

                // store as 64 bit double precision float
                doubleBuffer = new double[sliceSize];
                byteBuffer = new byte[8 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        fireProgressStateChanged(count * 100 / numberSlices);
                        image.exportSliceXY(i, doubleBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            tmpLong = Double.doubleToLongBits(doubleBuffer[j]);
                            byteBuffer[8 * j] = (byte) (tmpLong >>> 56);
                            byteBuffer[ (8 * j) + 1] = (byte) (tmpLong >>> 48);
                            byteBuffer[ (8 * j) + 2] = (byte) (tmpLong >>> 40);
                            byteBuffer[ (8 * j) + 3] = (byte) (tmpLong >>> 32);
                            byteBuffer[ (8 * j) + 4] = (byte) (tmpLong >>> 24);
                            byteBuffer[ (8 * j) + 5] = (byte) (tmpLong >>> 16);
                            byteBuffer[ (8 * j) + 6] = (byte) (tmpLong >>> 8);
                            byteBuffer[ (8 * j) + 7] = (byte) (tmpLong);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.ARGB:
                byteBuffer = new byte[sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = sBegin; z <= sEnd; z++, count++) {
                        i = (t * zDim) + z;
                        for (j = 1; j <= 3; j++) {
                            fireProgressStateChanged( (3 * count + (j - 1)) * 100 / (3 * numberSlices));
                            image.exportRGBData(j, 4 * i * sliceSize, sliceSize, byteBuffer);
                            raFile.write(byteBuffer);
                        }
                    } // for (z = sBegin; z <= sEnd; z++,count++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;
        } // switch(image.getType())

        raFile.close();

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     * 
     * @param slice offset into the file stored in the dataOffset array
     * @param buffer buffer where the info is stored
     * @param scaleFact if 1 data is unscaled
     * @param scaleOffset if 0 don't add offset
     * 
     * @exception IOException if there is an error reading the file
     */
    private void readBuffer(final int slice, final float[] buffer, final float scaleFact, final float scaleOffset)
            throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2, b3, b4;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt;

        // do not use image.getType() because the original type might be converted into float
        // because of scaleFact multiplication and scaleOffset addition
        switch (sourceType) {

            case ModelStorageBase.UBYTE:
                byteBuffer = new byte[buffer.length];
                nBytes = buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j] & 0xff;
                }

                break;

            case ModelStorageBase.SHORT:
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 2, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);

                    buffer[i] = (short) ( (b1 << 8) + b2);

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case ModelStorageBase.INTEGER:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    buffer[i] = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian

                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.FLOAT:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if ( ( (i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    tmpInt = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    buffer[i] = Float.intBitsToFloat(tmpInt);

                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.ARGB:
                if (isColorPlanar2D) {
                    byteBuffer = new byte[3 * buffer.length / 4];
                    nBytes = 3 * buffer.length / 4;
                    raFile.read(byteBuffer, 0, nBytes);
                    progress = slice * buffer.length;
                    progressLength = buffer.length * numberSlices;
                    mod = progressLength / 100;

                    for (j = 0; j < nBytes / 3; j++, i += 4) {

                        if ( ( (i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / (3 * progressLength) * 100));
                        }

                        buffer[i] = 255;
                        buffer[i + 1] = byteBuffer[j] & 0xff;
                    }

                    for (i = 0, j = nBytes / 3; j < 2 * nBytes / 3; j++, i += 4) {

                        if ( ( (i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / (3 * progressLength) * 100
                                    + 33.3f));
                        }

                        buffer[i + 2] = byteBuffer[j] & 0xff;
                    }

                    for (i = 0, j = 2 * nBytes / 3; j < nBytes; j++, i += 4) {

                        if ( ( (i + progress) % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) (i + progress) / (3 * progressLength) * 100
                                    + 66.7f));
                        }

                        buffer[i + 3] = byteBuffer[j] & 0xff;
                    }
                } // if (isColorPlanar2D)
                break;
        } // switch(brikDataType)

        if ( !haveBlank) {

            if (scaleFact != 1.0f) {

                for (i = 0; i < buffer.length; i++) {
                    buffer[i] *= scaleFact;
                }
            } // if (scaleFact != 1.0f)

            if (scaleOffset != 0.0f) {

                for (i = 0; i < buffer.length; i++) {
                    buffer[i] += scaleOffset;
                }
            } // if (scaleOffset != 0.0f)
        } // if (!haveBlank)
        else { // BLANK is used to indicate an undefined physical meaning in an integer array
            // BSCALE and BZERO are not applied to BLANK values

            if (scaleFact != 1.0f) {

                for (i = 0; i < buffer.length; i++) {

                    if (buffer[i] != (float) BLANK) {
                        buffer[i] *= scaleFact;
                    } else {
                        buffer[i] = (float) BLANK;
                    }
                }
            } // if (scaleFact != 1.0f)

            if (scaleOffset != 0.0f) {

                for (i = 0; i < buffer.length; i++) {

                    if (buffer[i] != (float) BLANK) {
                        buffer[i] += scaleOffset;
                    } else {
                        buffer[i] = (float) BLANK;
                    }
                }
            } // if (scaleOffset != 0.0f)
        }

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     * 
     * @param slice offset into the file stored in the dataOffset array
     * @param buffer buffer where the info is stored
     * @param scaleFact if 1 data is unscaled
     * @param scaleOffset if 0 don't add offset
     * 
     * @exception IOException if there is an error reading the file
     */
    private void readDBuffer(final int slice, final double[] buffer, final double scaleFact, final double scaleOffset)
            throws IOException {
        int i = 0;
        int j;
        int nBytes;
        long b1, b2, b3, b4, b5, b6, b7, b8;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        long tmpLong;

        byteBuffer = new byte[8 * buffer.length];
        nBytes = 8 * buffer.length;
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice * buffer.length;
        progressLength = buffer.length * numberSlices;
        mod = progressLength / 10;

        for (j = 0; j < nBytes; j += 8, i++) {

            if ( ( (i + progress) % mod) == 0) {
                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
            }

            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j + 1);
            b3 = getUnsignedByte(byteBuffer, j + 2);
            b4 = getUnsignedByte(byteBuffer, j + 3);
            b5 = getUnsignedByte(byteBuffer, j + 4);
            b6 = getUnsignedByte(byteBuffer, j + 5);
            b7 = getUnsignedByte(byteBuffer, j + 6);
            b8 = getUnsignedByte(byteBuffer, j + 7);

            tmpLong = ( (b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) | (b7 << 8) | b8); // Big
            // Endian
            buffer[i] = Double.longBitsToDouble(tmpLong);

        } // for (j =0; j < nBytes; j+=8, i++ )

        if (scaleFact != 1.0) {

            for (i = 0; i < buffer.length; i++) {
                buffer[i] *= scaleFact;
            }
        } // if (scaleFact != 1.0)

        if (scaleOffset != 0.0) {

            for (i = 0; i < buffer.length; i++) {
                buffer[i] += scaleOffset;
            }
        }

    }

}
