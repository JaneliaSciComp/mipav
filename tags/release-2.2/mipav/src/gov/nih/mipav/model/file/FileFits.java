package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.io.*;
import java.awt.Dialog.*;


/* Some of this code is derived from FITS.java in ImageJ */
public class FileFits extends FileBase {

    private String          fileName;
    private String          fileDir;
    private  ViewUserInterface   UI;
    private  ModelLUT       LUT = null;
    private File            file;
    private FileInfoFits    fileInfo;
    private  boolean        endianess;
    private ModelImage      image;
    int sourceType = ModelStorageBase.FLOAT;
    int nDimensions = 2;
    int imgExtents[];
    private int numberSlices; // 1 for 2D, zDim for 3D, and zDim * tDim for 4D
    private int BLANK = 0;
    private boolean haveBlank = false;


    private     ViewJProgressBar progressBar = null;



    /**
    *   FITS reader/writer constructor
    *   @param _UI              user interface reference
    *   @param fileName         file name
    *   @param fileDir          file directory
    *   @exception IOException  if there is an error making the file
    */
    public FileFits(ViewUserInterface _UI, String fileName, String fileDir) throws IOException {

        UI              = _UI;
        this.fileName   = fileName;
        this.fileDir    = fileDir;
    }

    /**
    *   returns LUT if defined
    *   @return        the LUT if defined else it is null
    */
    public ModelLUT getModelLUT(){ return LUT;}


    /*
    *   reads the FITS file header and data
    *   @return   returns a black and white image
    *   @exception IOException if there is an error reading the file
    */
    public ModelImage readImage(boolean one) throws IOException {
        // Fixed format is required for the values of the required keywords and strongly
        // recommended for the other keywords.  The required keywords are SIMPLE, BITPIX,
        // NAXIS, NAXISn, n=1,...,NAXIS(NAXIS = 0 -> NAXIS1 not present), and END.
        int i;
        String s,firstS,subS;
        boolean readAgain;
        int count = 0;
        int bitsPerPixel;

        // data value = (FITS_value) X BSCALE + BZERO
        double BSCALE = 1.0;
        double BZERO = 0.0;
        long offset;
        float imgBuffer[];
        double imgDBuffer[];
        int bufferSize;
        float imgResols[] = new float[] {1.0f,1.0f,1.0f,1.0f,1.0f};
        float minResol;
        float maxResol;
        int dimNumber;

        try {
            progressBar = new ViewJProgressBar(fileName, "Reading FITS file...",
                                            0, 100, false, null, null);

         progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2,
                                 50);
            setProgressBarVisible(!one);

            file = new File(fileDir + fileName);

            endianess = FileBase.BIG_ENDIAN; // true
            fileInfo = new FileInfoFits(fileName, fileDir, FileBase.FITS);
            fileInfo.setEndianess(endianess);

            raFile          = new RandomAccessFile(file, "r");

            readAgain = true;
            while(readAgain) { // looping for first required keyword of SIMPLE
                s = getString(80);
                count++;
                firstS = s.substring(0,1);
                if ((firstS.equals(" ")) || (firstS.equals("/")))
                  ;
                else if (!s.startsWith("SIMPLE")) {
                    raFile.close();
                    progressBar.dispose();
                    MipavUtil.displayError("Instead of SIMPLE first keyword starts with " + s);
                    throw new IOException();
                }
                else {
                    readAgain = false;
                    subS = s.substring(8,10);
                    if (!subS.equals("= ")) {
                        MipavUtil.displayError("SIMPLE line does not have required =<sp> in cols 9 and 10");
                    }
                    else {
                        subS = s.substring(29,30);
                        if ((!subS.equals("T")) && (!subS.equals("F"))) {
                            MipavUtil.displayError("SIMPLE line does not have required T or F in column 30");
                        }
                        else if (subS.equals("T")) {
                            Preferences.debug("SIMPLE = T for file conformance to FITS standards\n");
                        }
                        else { // subs.equals("F")
                            Preferences.debug(
                            "SIMPLE = F for departure from FITS standards in some significant way\n");
                        }

                    }

                }
            } // while(readAgain) looping for first required keyword of SIMPLE

            readAgain = true;
            while(readAgain) { // looping for second required keyword of BITPIX
                s = getString(80);
                count++;
                firstS = s.substring(0,1);
                if ((firstS.equals(" ")) || (firstS.equals("/")))
                  ;
                else if (!s.startsWith("BITPIX")) {
                    raFile.close();
                    progressBar.dispose();
                    MipavUtil.displayError("Instead of BITPIX second keyword starts with " + s);
                    throw new IOException();
                }
                else {
                    readAgain = false;
                    subS = s.substring(8,10);
                    if (!subS.equals("= ")) {
                        MipavUtil.displayError("BITPIX line does not have required =<sp> in cols 9 and 10");
                    }
                    else {
                        subS = s.substring(10,30);
                        subS = subS.trim();
                        try {
                            bitsPerPixel = Integer.parseInt(subS);
                        }
                        catch(NumberFormatException e) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("Instead of integer BITPIX line had " + subS);
                            throw new IOException();
                        }
                        switch(bitsPerPixel) {
                            case 8:
                                sourceType = ModelStorageBase.UBYTE;
                                Preferences.debug("sourceType = ModelStorageBase.UBYTE\n");
                                break;
                            case 16:
                                sourceType = ModelStorageBase.SHORT;
                                Preferences.debug("sourceType = ModelStorageBase.SHORT\n");
                                break;
                            case 32:
                                sourceType = ModelStorageBase.INTEGER;
                                Preferences.debug("sourceType = ModelStorageBase.INTEGER\n");
                                break;
                            case -32:
                                sourceType = ModelStorageBase.FLOAT;
                                Preferences.debug("sourceType = ModelStorageBase.FLOAT\n");
                                break;
                            case - 64:
                                sourceType = ModelStorageBase.DOUBLE;
                                Preferences.debug("sourceType = ModelStorageBase.DOUBLE\n");
                                break;
                            default:
                                raFile.close();
                                progressBar.dispose();
                                MipavUtil.displayError("BITPIX keyword had illegal value of " + bitsPerPixel);
                                throw new IOException();
                        }
                    }
                }
            } // while(readAgain) looping for second required keyword of BITPIX

            readAgain = true;
            while(readAgain) { // looping for third required keyword of NAXIS
                s = getString(80);
                count++;
                firstS = s.substring(0,1);
                if ((firstS.equals(" ")) || (firstS.equals("/")))
                  ;
                else if (!s.startsWith("NAXIS")) {
                    raFile.close();
                    progressBar.dispose();
                    MipavUtil.displayError("Instead of NAXIS third keyword starts with " + s);
                    throw new IOException();
                }
                else {
                    readAgain = false;
                    subS = s.substring(8,10);
                    if (!subS.equals("= ")) {
                        MipavUtil.displayError("NAXIS line does not have required =<sp> in cols 9 and 10");
                    }
                    else {
                        subS = s.substring(10,30);
                        subS = subS.trim();
                        try {
                            nDimensions = Integer.parseInt(subS);
                        }
                        catch(NumberFormatException e) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("Instead of integer NAXIS line had " + subS);
                            throw new IOException();
                        }
                        Preferences.debug("NAXIS = " + nDimensions + "\n");
                        if (nDimensions < 0) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("NAXIS had an illegal negative value of " + nDimensions);
                            throw new IOException();
                        }
                        else if (nDimensions > 999) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("NAXIS exceeded maximum legal value of 999 with " +
                                                   nDimensions);
                            throw new IOException();
                        }
                        else if (nDimensions > 4) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("MIPAV cannot display an image with " + nDimensions +
                                                   " dimensions");
                            throw new IOException();
                        }
                        else if (nDimensions == 1) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("MIPAV cannot display an image with 1 dimension");
                            throw new IOException();
                        }
                        else if (nDimensions == 0) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError
                            ("NAXIS value of 0 indicates no data is associated with the header");
                            throw new IOException();
                        }
                    }
                }
            } // while (readAgain) looping for third required keyword of NAXIS

            imgExtents = new int[nDimensions];

            readAgain = true;
            while(readAgain) { // looping for fourth required keyword of NAXIS1
                s = getString(80);
                count++;
                firstS = s.substring(0,1);
                if ((firstS.equals(" ")) || (firstS.equals("/")))
                  ;
                else if (!s.startsWith("NAXIS1")) {
                    raFile.close();
                    progressBar.dispose();
                    MipavUtil.displayError("Instead of NAXIS1 fourth keyword starts with " + s);
                    throw new IOException();
                }
                else {
                    readAgain = false;
                    subS = s.substring(8,10);
                    if (!subS.equals("= ")) {
                        MipavUtil.displayError("NAXIS1 line does not have required =<sp> in cols 9 and 10");
                    }
                    else {
                        subS = s.substring(10,30);
                        subS = subS.trim();
                        try {
                            imgExtents[0] = Integer.parseInt(subS);
                        }
                        catch(NumberFormatException e) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("Instead of integer NAXIS1 line had " + subS);
                            throw new IOException();
                        }
                        Preferences.debug("NAXIS1 = " + imgExtents[0] + "\n");
                        if (imgExtents[0] < 0) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("NAXIS1 had an illegal negative value of " + imgExtents[0]);
                            throw new IOException();
                        }
                        else if (imgExtents[0] == 0) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError
                            ("NAXIS1 value of 0 indicates no data is associated with the header");
                            throw new IOException();
                        }
                    }
                }
            } // while (readAgain) looping for fourth required keyword of NAXIS1

            readAgain = true;
            while(readAgain) { // looping for fifth required keyword of NAXIS2
                s = getString(80);
                count++;
                firstS = s.substring(0,1);
                if ((firstS.equals(" ")) || (firstS.equals("/")))
                  ;
                else if (!s.startsWith("NAXIS2")) {
                    raFile.close();
                    progressBar.dispose();
                    MipavUtil.displayError("Instead of NAXIS2 fifth keyword starts with " + s);
                    throw new IOException();
                }
                else {
                    readAgain = false;
                    subS = s.substring(8,10);
                    if (!subS.equals("= ")) {
                        MipavUtil.displayError("NAXIS2 line does not have required =<sp> in cols 9 and 10");
                    }
                    else {
                        subS = s.substring(10,30);
                        subS = subS.trim();
                        try {
                            imgExtents[1] = Integer.parseInt(subS);
                        }
                        catch(NumberFormatException e) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("Instead of integer NAXIS2 line had " + subS);
                            throw new IOException();
                        }
                        Preferences.debug("NAXIS2 = " + imgExtents[1] + "\n");
                        if (imgExtents[1] < 0) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError("NAXIS2 had an illegal negative value of " + imgExtents[1]);
                            throw new IOException();
                        }
                        else if (imgExtents[1] == 0) {
                            raFile.close();
                            progressBar.dispose();
                            MipavUtil.displayError
                            ("NAXIS2 value of 0 indicates no data is associated with the header");
                            throw new IOException();
                        }
                    }
                }
            } // while (readAgain) looping for fifth required keyword of NAXIS2

            if (nDimensions >= 3) {
                readAgain = true;
                while(readAgain) { // looping for sixth required keyword of NAXIS3
                    s = getString(80);
                    count++;
                    firstS = s.substring(0,1);
                    if ((firstS.equals(" ")) || (firstS.equals("/")))
                    ;
                    else if (!s.startsWith("NAXIS3")) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of NAXIS3 sixth keyword starts with " + s);
                        throw new IOException();
                    }
                    else {
                        readAgain = false;
                        subS = s.substring(8,10);
                        if (!subS.equals("= ")) {
                            MipavUtil.displayError("NAXIS3 line does not have required =<sp> in cols 9 and 10");
                        }
                        else {
                            subS = s.substring(10,30);
                            subS = subS.trim();
                            try {
                                imgExtents[2] = Integer.parseInt(subS);
                            }
                            catch(NumberFormatException e) {
                                raFile.close();
                                progressBar.dispose();
                                MipavUtil.displayError("Instead of integer NAXIS3 line had " + subS);
                                throw new IOException();
                            }
                            Preferences.debug("NAXIS3 = " + imgExtents[2] + "\n");
                            if (imgExtents[2] < 0) {
                                raFile.close();
                                progressBar.dispose();
                                MipavUtil.displayError("NAXIS3 had an illegal negative value of " + imgExtents[2]);
                                throw new IOException();
                            }
                            else if (imgExtents[2] == 0) {
                                raFile.close();
                                progressBar.dispose();
                                MipavUtil.displayError
                                ("NAXIS3 value of 0 indicates no data is associated with the header");
                                throw new IOException();
                            }
                        }
                    }
                } // while (readAgain) looping for sixth required keyword of NAXIS3
            } // if (nDimensions >= 3)

            if (nDimensions == 4) {
                readAgain = true;
                while(readAgain) { // looping for seventh required keyword of NAXIS4
                    s = getString(80);
                    count++;
                    firstS = s.substring(0,1);
                    if ((firstS.equals(" ")) || (firstS.equals("/")))
                    ;
                    else if (!s.startsWith("NAXIS4")) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of NAXIS4 seventh keyword starts with " + s);
                        throw new IOException();
                    }
                    else {
                        readAgain = false;
                        subS = s.substring(8,10);
                        if (!subS.equals("= ")) {
                            MipavUtil.displayError("NAXIS4 line does not have required =<sp> in cols 9 and 10");
                        }
                        else {
                            subS = s.substring(10,30);
                            subS = subS.trim();
                            try {
                                imgExtents[3] = Integer.parseInt(subS);
                            }
                            catch(NumberFormatException e) {
                                raFile.close();
                                progressBar.dispose();
                                MipavUtil.displayError("Instead of integer NAXIS4 line had " + subS);
                                throw new IOException();
                            }
                            Preferences.debug("NAXIS4 = " + imgExtents[3] + "\n");
                            if (imgExtents[3] < 0) {
                                raFile.close();
                                progressBar.dispose();
                                MipavUtil.displayError("NAXIS4 had an illegal negative value of " + imgExtents[3]);
                                throw new IOException();
                            }
                            else if (imgExtents[3] == 0) {
                                raFile.close();
                                progressBar.dispose();
                                MipavUtil.displayError
                                ("NAXIS4 value of 0 indicates no data is associated with the header");
                                throw new IOException();
                            }
                        }
                    }
                } // while (readAgain) looping for seventh required keyword of NAXIS4
            } // if (nDimensions == 4)
            fileInfo.setExtents(imgExtents);

            do {
                s = getString(80);
                count++;
                if (s.startsWith("BSCALE")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    i = subS.indexOf("/");
                    if (i != -1) {
                        subS = subS.substring(0,i);
                    }
                    try {
                        BSCALE = Double.parseDouble(subS);
                    }
                    catch (NumberFormatException e) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of a float BSCALE line had = " + subS);
                        throw new IOException();
                    }
                    Preferences.debug("BSCALE = " + BSCALE + "\n");
                } // if (s.startsWith("BSCALE"))
                else if (s.startsWith("BZERO")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    i = subS.indexOf("/");
                    if (i != -1) {
                        subS = subS.substring(0,i);
                    }
                    try {
                        BZERO = Double.parseDouble(subS);
                    }
                    catch (NumberFormatException e) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of a float BZERO line had = " + subS);
                        throw new IOException();
                    }
                    Preferences.debug("BZERO = " + BZERO + "\n");
                } // else if (s.startsWith("BZERO"))
                else if (s.startsWith("BLANK")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    i = subS.indexOf("/");
                    if (i != -1) {
                        subS = subS.substring(0,i);
                    }
                    try {
                        BLANK = Integer.parseInt(subS);
                    }
                    catch (NumberFormatException e) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of an integer BLANK line had = " + subS);
                        throw new IOException();
                    }
                    Preferences.debug("BLANK = " + BLANK + "\n");
                    haveBlank = true;
                } // else if (s.startsWith("BLANK"))
                else if (s.startsWith("CDELT1")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    i = subS.indexOf("/");
                    if (i != -1) {
                        subS = subS.substring(0,i);
                    }
                    try {
                        imgResols[0] = Math.abs(Float.parseFloat(subS));
                    }
                    catch (NumberFormatException e) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of a float CDELT1 line had = " + subS);
                        throw new IOException();
                    }
                    Preferences.debug("CDELT1 = " + imgResols[0] + "\n");
                } // else if (s.startsWith("CDELT1"))
                else if (s.startsWith("CDELT2")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    i = subS.indexOf("/");
                    if (i != -1) {
                        subS = subS.substring(0,i);
                    }
                    try {
                        imgResols[1] = Math.abs(Float.parseFloat(subS));
                    }
                    catch (NumberFormatException e) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of a float CDELT2 line had = " + subS);
                        throw new IOException();
                    }
                    Preferences.debug("CDELT2 = " + imgResols[1] + "\n");
                } // else if (s.startsWith("CDELT2"))
                else if (s.startsWith("CDELT3")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    i = subS.indexOf("/");
                    if (i != -1) {
                        subS = subS.substring(0,i);
                    }
                    try {
                        imgResols[2] = Math.abs(Float.parseFloat(subS));
                    }
                    catch (NumberFormatException e) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of a float CDELT3 line had = " + subS);
                        throw new IOException();
                    }
                    Preferences.debug("CDELT3 = " + imgResols[2] + "\n");
                } // else if (s.startsWith("CDELT3"))
                else if (s.startsWith("CDELT4")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    i = subS.indexOf("/");
                    if (i != -1) {
                        subS = subS.substring(0,i);
                    }
                    try {
                        imgResols[3] = Math.abs(Float.parseFloat(subS));
                    }
                    catch (NumberFormatException e) {
                        raFile.close();
                        progressBar.dispose();
                        MipavUtil.displayError("Instead of a float CDELT4 line had = " + subS);
                        throw new IOException();
                    }
                    Preferences.debug("CDELT4 = " + imgResols[3] + "\n");
                } // else if (s.startsWith("CDELT4"))
                else if (s.startsWith("CTYPE")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    subS = subS.substring(1,subS.length()-1); // Remove beginning and ending '
                    Preferences.debug("CTYPE" + s.substring(5,6)+ " = " + subS + "\n");
                    dimNumber = Integer.parseInt(s.substring(5,6));
                    if (subS.equals(FileInfoBase.INCHES_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.INCHES,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.MILLIMETERS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.CENTIMETERS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.CENTIMETERS,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.METERS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.METERS,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.KILOMETERS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.KILOMETERS,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.MILES_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILES,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.ANGSTROMS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.ANGSTROMS,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.NANOMETERS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.NANOMETERS,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.MICROMETERS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MICROMETERS,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.NANOSEC_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.NANOSEC,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.MICROSEC_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MICROSEC,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.MILLISEC_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLISEC,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.SECONDS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.SECONDS,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.MINUTES_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MINUTES,dimNumber-1);
                    }
                    else if (subS.equals(FileInfoBase.HOURS_STRING)) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.HOURS,dimNumber-1);
                    }
                } // else if (s.startsWith("CTYPE"))
                else if (s.startsWith("COMMENT")) {
                    subS = s.substring(8,80);
                    subS = subS.trim();
                    Preferences.debug("COMMENT = " + subS + "\n");
                } // else if (s.startsWith("COMMENT"))
                else if (s.startsWith("HISTORY")) {
                    subS = s.substring(8,80);
                    subS = subS.trim();
                    Preferences.debug("HISTORY = " + subS + "\n");
                } // else if (s.startsWith("HISTORY"))
                else if (s.startsWith("DATE")) {
                    // Date file was written
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    Preferences.debug("DATE file written = " + subS + "\n");
                } // else if (s.startsWith("DATE"))
                else if (s.startsWith("DATE-OBS")) {
                    // Date data was acquired
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    Preferences.debug("DATE-OBS data acquired = " + subS + "\n");
                } // else if (s.startsWith("DATE-OBS"))
                else if (s.startsWith("ORIGIN")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    Preferences.debug("ORIGIN, installation where file is written = " + "\n" +
                                          subS + "\n");
                } // else if (s.startsWith("ORIGIN"))
                else if (s.startsWith("INSTRUMEN")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    Preferences.debug("INSTRUMEN, data acquisition instrument = " + subS + "\n");
                } // else if (s.startsWith("INSTRUMEN"))
                else if (s.startsWith("OBSERVER")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    Preferences.debug("OBSERVER = " + subS + "\n");
                } // else if (s.startsWith("OBSERVER"))
                else if (s.startsWith("OBJECT")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    Preferences.debug("OBJECT observed = " + subS + "\n");
                } // else if (s.startsWith("OBJECT"))
                else if (s.startsWith("AUTHOR")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    Preferences.debug("AUTHOR = " + subS + "\n");
                } // else if (s.startsWith("AUTHOR")
                else if (s.startsWith("REFERENC")) {
                    subS = s.substring(10,80);
                    subS = subS.trim();
                    Preferences.debug("REFERENC = " + subS + "\n");
                } // else if (s.startsWith("REFERENC"))
            } while (!s.startsWith("END"));
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

            offset = 2880 + 2880*((count*80)/2880);
            raFile.seek(offset);

            if (((BSCALE != 1.0) || (BZERO != 0.0)) && ((sourceType == ModelStorageBase.UBYTE) ||
                 (sourceType == ModelStorageBase.SHORT) || (sourceType == ModelStorageBase.INTEGER))) {
                if (one) {
                    image = new ModelImage(ModelStorageBase.FLOAT, new int[] {imgExtents[0], imgExtents[1]}, fileInfo.getFileName(), UI);
                }
                else {
                    image = new ModelImage(ModelStorageBase.FLOAT, imgExtents, fileInfo.getFileName(), UI);
                }
                fileInfo.setDataType(ModelStorageBase.FLOAT);
            }
            else {
                if (one) {
                    image = new ModelImage(sourceType, new int[] {imgExtents[0], imgExtents[1]}, fileInfo.getFileName(), UI);
                }
                else {
                    image = new ModelImage(sourceType, imgExtents, fileInfo.getFileName(), UI);
                }
                fileInfo.setDataType(sourceType);
            }

            if (nDimensions == 2 || one) {
                numberSlices = 1;
            }
            else if (nDimensions == 3) {
                numberSlices = imgExtents[2];
            }
            else {
                numberSlices = imgExtents[2] * imgExtents[3];
            }
            if (one) {
                if (imgExtents.length > 2) {
                    int skip = imgExtents[0]*imgExtents[1]*(imgExtents[2]/2-1);
                    if (sourceType == ModelStorageBase.SHORT || sourceType == ModelStorageBase.USHORT) {
                        skip *= 2;
                    }
                    else if (sourceType == ModelStorageBase.FLOAT ||
                    sourceType == ModelStorageBase.INTEGER ||
                    sourceType == ModelStorageBase.UINTEGER) {
                        skip *= 4;
                    }
                    else if (sourceType == ModelStorageBase.DOUBLE) {
                        skip *= 8;
                    }
                    raFile.seek(offset + skip);
                }
            }

            if (sourceType != ModelStorageBase.DOUBLE) {
                bufferSize = imgExtents[0]*imgExtents[1];
                imgBuffer = new float[bufferSize];
                for (i = 0; i < numberSlices; i++) {
                    image.setFileInfo(fileInfo, i);
                    readBuffer(i, imgBuffer, (float)BSCALE, (float)BZERO);
                    image.importData(i*bufferSize, imgBuffer, false);
                }
            } // if (sourceType != ModelStorageBase.DOUBLE)
            else { // sourceType == ModelStorageBase.DOUBLE
                bufferSize = imgExtents[0]*imgExtents[1];
                imgDBuffer = new double[bufferSize];
                for (i = 0; i < numberSlices; i++) {
                    image.setFileInfo(fileInfo, i);
                    readDBuffer(i, imgDBuffer, BSCALE, BZERO);
                    image.importData(i*bufferSize, imgDBuffer, false);
                }
            } // else sourceType == ModelStorageBase.DOUBLE
            image.calcMinMax();
            raFile.close();
            progressBar.dispose();
            return image;
        }
        catch (Exception e) {
            if (image != null) {
                image.disposeLocal();
                image = null;
            }
            System.gc();
            throw new IOException();
        }
    }

    /**
    *   Reads a slice of data at a time and stores the results in the buffer
    *   @param slice            offset into the file stored in the dataOffset array
    *   @param buffer           buffer where the info is stored
    *   @param scaleFact        if 1 data is unscaled
    *   @param scaleOffset      if 0 don't add offset
    *   @exception IOException  if there is an error reading the file
    */
    private void readBuffer(int slice, float buffer[], float scaleFact, float scaleOffset) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2, b3, b4;
        byte [] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt;
        switch (sourceType) {
            case ModelStorageBase.UBYTE:
                byteBuffer =  new byte[buffer.length];
                nBytes = buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice*buffer.length;
                progressLength = buffer.length*numberSlices;
                mod = progressLength/100;
                progressBar.setVisible(isProgressBarVisible());
                for ( j = 0; j < nBytes; j++, i++) {
                    if ((i+progress)%mod==0) progressBar.updateValueImmed( Math.round((float)(i+progress)/
                                                              progressLength * 100));
                    buffer[i] = byteBuffer[j] & 0xff;
                }
                break;
            case ModelStorageBase.SHORT:
                byteBuffer =  new byte[2 * buffer.length];
                nBytes = 2 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice*buffer.length;
                progressLength = buffer.length*numberSlices;
                mod = progressLength/10;
                progressBar.setVisible(isProgressBarVisible());
                for (j = 0; j < nBytes; j+=2, i++ ) {
                    if ((i+progress)%mod==0) progressBar.updateValueImmed( Math.round((float)(i+progress)/
                                                                progressLength * 100));
                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j+1);

                    buffer[i] = (short)((b1 << 8) + b2);


                } // for (j = 0; j < nBytes; j+=2, i++ )
                break;
            case ModelStorageBase.INTEGER:
                byteBuffer =  new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice*buffer.length;
                progressLength = buffer.length*numberSlices;
                mod = progressLength/10;
                progressBar.setVisible(isProgressBarVisible());
                for (j =0; j < nBytes; j+=4, i++ ) {
                    if ((i+progress)%mod==0) progressBar.updateValueImmed( Math.round((float)(i+progress)/
                                                                progressLength * 100));
                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j+1);
                    b3 = getUnsignedByte(byteBuffer, j+2);
                    b4 = getUnsignedByte(byteBuffer, j+3);

                    buffer[i]=((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);  // Big Endian

                } // for (j =0; j < nBytes; j+=4, i++ )
                break;
            case ModelStorageBase.FLOAT:
                byteBuffer =  new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice*buffer.length;
                progressLength = buffer.length*numberSlices;
                mod = progressLength/10;
                progressBar.setVisible(isProgressBarVisible());
                for (j =0; j < nBytes; j+=4, i++ ) {
                    if ((i+progress)%mod==0) progressBar.updateValueImmed( Math.round((float)(i+progress)/
                                                                progressLength * 100));
                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j+1);
                    b3 = getUnsignedByte(byteBuffer, j+2);
                    b4 = getUnsignedByte(byteBuffer, j+3);

                    tmpInt =((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);  // Big Endian
                    buffer[i] = Float.intBitsToFloat(tmpInt);

                } // for (j =0; j < nBytes; j+=4, i++ )
                break;
        } // switch(brikDataType)
        if (!haveBlank) {
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
        else { // BLANK is used to indicated an undefined physical meaning in an integer array
               // BSCALE and BZERO are not applied to BLANK values
            if (scaleFact != 1.0f) {
                for (i = 0; i < buffer.length; i++) {
                    if (buffer[i] != (float)BLANK) {
                        buffer[i] *= scaleFact;
                    }
                    else {
                        buffer[i] = (float)BLANK;
                    }
                }
            } // if (scaleFact != 1.0f)
            if (scaleOffset != 0.0f) {
                for (i = 0; i < buffer.length; i++) {
                    if (buffer[i] != (float)BLANK) {
                        buffer[i] += scaleOffset;
                    }
                    else {
                        buffer[i] = (float)BLANK;
                    }
                }
            } // if (scaleOffset != 0.0f)
        }

    }

    /**
    *   Reads a slice of data at a time and stores the results in the buffer
    *   @param slice            offset into the file stored in the dataOffset array
    *   @param buffer           buffer where the info is stored
    *   @param scaleFact        if 1 data is unscaled
    *   @param scaleOffset      if 0 don't add offset
    *   @exception IOException  if there is an error reading the file
    */
    private void readDBuffer(int slice, double buffer[], double scaleFact, double scaleOffset) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        long b1, b2, b3, b4, b5 , b6, b7, b8;
        byte [] byteBuffer;
        int progress, progressLength, mod;
        long tmpLong;

        byteBuffer =  new byte[8 * buffer.length];
        nBytes = 8 * buffer.length;
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice*buffer.length;
        progressLength = buffer.length*numberSlices;
        mod = progressLength/10;
        progressBar.setVisible(isProgressBarVisible());
        for (j =0; j < nBytes; j+=8, i++ ) {
            if ((i+progress)%mod==0) progressBar.updateValueImmed( Math.round((float)(i+progress)/
                                                              progressLength * 100));
            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j+1);
            b3 = getUnsignedByte(byteBuffer, j+2);
            b4 = getUnsignedByte(byteBuffer, j+3);
            b5 = getUnsignedByte(byteBuffer, j+4);
            b6 = getUnsignedByte(byteBuffer, j+5);
            b7 = getUnsignedByte(byteBuffer, j+6);
            b8 = getUnsignedByte(byteBuffer, j+7);

            tmpLong=((b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) |
                       (b5 << 24) | (b6 << 16) | (b7 << 8) | b8);  // Big Endian
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

    /**
    *   Writes a FITS format type image.
    *   @param image      Image model of data to write.
    *   @param options    options such as starting and ending slices and times
    *   @exception IOException if there is an error writing the file
    */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        byte[] cardImage = new byte[80];
        int i,j;
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
        int sEnd; //last z slice to write
        int tBegin; // first t time to write
        int tEnd; // last t time to write
        int z,t;
        int zDim;
        int tDim;
        int count;
        progressBar = new ViewJProgressBar(fileName, "Writing FITS file...",
                                           0, 100, true, null, null);

        progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2,
                                50);
        progressBar.setVisible(true);

        if (image.getNDims() >= 3) {
            sBegin = options.getBeginSlice();
            sEnd = options.getEndSlice();
        }
        else {
            sBegin = 0;
            sEnd = 0;
        }

        if (image.getNDims() == 4) {
            tBegin = options.getBeginTime();
            tEnd = options.getEndTime();
        }
        else {
            tBegin = 0;
            tEnd = 0;
        }

        file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file,"rw");

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
        switch(image.getType()) {
            case ModelStorageBase.UBYTE:
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
            progressBar.dispose();
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
        switch(image.getNDims()) {
            case 2:
            cardImage[29] = 50; // 2
            break;
            case 3:
            cardImage[29] = 51; // 3
            break;
            case 4:
            cardImage[29] = 52; // 4
            break;
            default:
            raFile.close();
            progressBar.dispose();
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
        if (image.getNDims() >= 3) {
            cardImage[0] = 78; // N
            cardImage[1] = 65; // A
            cardImage[2] = 88; // X
            cardImage[3] = 73; // I
            cardImage[4] = 83; // S
            cardImage[5] = 51; // 3
            cardImage[8] = 61; // =

            axisSize = Integer.toString(sEnd - sBegin + 1);
            axisBytes = axisSize.getBytes();
            for (i = 0; i < axisSize.length(); i++) {
                cardImage[30 + i - axisSize.length()] = axisBytes[i];
            }
        } // if (image.getNDims() >= 3)
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
        if (image.getNDims() >= 3) {
            cardImage[0] = 67; // C
            cardImage[1] = 68; // D
            cardImage[2] = 69; // E
            cardImage[3] = 76; // L
            cardImage[4] = 84; // T
            cardImage[5] = 51; // 3
            cardImage[8] = 61; // =

            resString = Float.toString(image.getFileInfo()[0].getResolutions()[2]);
            resBytes = resString.getBytes();
            for (i = 0; i < resString.length(); i++) {
                cardImage[30 + i - resString.length()] = resBytes[i];
            }
        } // if (image.getNDims() >= 3)
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
        for (i=0; i < 4; i++) {
          for (j = 0; j < 80; j++) {
            cardImage[j] = 32; // fill with ascii spaces
          }
          if ((image.getNDims() >= (i+1)) && (image.getFileInfo()[0].getResolutions()[i] > 0.0)) {
            switch(measure[i]){
              case FileInfoBase.INCHES:
                mString = FileInfoBase.INCHES_STRING;
                break;
              case FileInfoBase.MILLIMETERS:
                mString = FileInfoBase.MILLIMETERS_STRING;
                break;
              case FileInfoBase.CENTIMETERS:
                mString = FileInfoBase.CENTIMETERS_STRING;
                break;
              case FileInfoBase.METERS:
                mString = FileInfoBase.METERS_STRING;
                break;
              case FileInfoBase.KILOMETERS:
                mString = FileInfoBase.KILOMETERS_STRING;
                break;
              case FileInfoBase.MILES:
                mString = FileInfoBase.MILES_STRING;
                break;
              case FileInfoBase.ANGSTROMS:
                mString = FileInfoBase.ANGSTROMS_STRING;
                break;
              case FileInfoBase.NANOMETERS:
                mString = FileInfoBase.MICROMETERS_STRING;
                break;
              case FileInfoBase.MICROMETERS:
                mString = FileInfoBase.NANOMETERS_STRING;
                break;
              case FileInfoBase.NANOSEC:
                mString = FileInfoBase.NANOSEC_STRING;
                break;
              case FileInfoBase.MICROSEC:
                mString = FileInfoBase.MICROSEC_STRING;
                break;
              case FileInfoBase.MILLISEC:
                mString = FileInfoBase.MILLISEC_STRING;
                break;
              case FileInfoBase.SECONDS:
                mString = FileInfoBase.SECONDS_STRING;
                break;
              case FileInfoBase.MINUTES:
                mString = FileInfoBase.MINUTES_STRING;
                break;
              case FileInfoBase.HOURS:
                mString = FileInfoBase.HOURS_STRING;
                break;
              default:
                mString = "Unknown";
                break;
            }  // end of switch(measure[i])
            cardImage[0] = 67; // C
            cardImage[1] = 84; // T
            cardImage[2] = 89; // Y
            cardImage[3] = 80; // P
            cardImage[4] = 69; // E
            cardImage[5] = (byte)(49 + i); // 1 = 49, 2 = 50, 3 = 51, 4 = 52
            cardImage[8] = 61; // =
            cardImage[10] = 39; // '
            mBytes = mString.getBytes();
            for (j = 0; j < mString.length(); j++) {
                cardImage[11+j] = mBytes[j];
            }
            cardImage[11+mString.length()] = 39; // '
          }  // if ((image.getNDims() >= (i+1)) && (image.getFileInfo()[0].getResolutions()[i] > 0.0))
          raFile.write(cardImage);
        }  // for (i=0; i < 4; i++)

        for (i = 0; i < 80; i++) {
            cardImage[i] = 32; // fill with ascii spaces
        }
        cardImage[0] = 69; // E
        cardImage[1] = 78; // N
        cardImage[2] = 68; // D
        raFile.write(cardImage);

        // The data starts at byte 2880 or after 36 80-byte lines have been written.
        // 16 have been written, so write 20 more.

        cardImage[0] = 32;
        cardImage[1] = 32;
        cardImage[2] = 32;
        for (i = 0; i < 20; i++) {
            raFile.write(cardImage);
        }

        if (image.getNDims() == 2) {
            numberSlices = 1;
            zDim = 1;
            tDim = 1;
        }
        else if (image.getNDims() == 3) {
            numberSlices = sEnd - sBegin + 1;
            zDim = image.getExtents()[2];
            tDim = 1;
        }
        else {
            numberSlices = (sEnd - sBegin + 1) * (tEnd - tBegin + 1);
            zDim = image.getExtents()[2];
            tDim = image.getExtents()[3];
        }
        sliceSize = image.getSliceSize();

        count = 0;
        switch(image.getType()) {
            case ModelStorageBase.UBYTE:
            // store as 8-bit unsigned
            byteBuffer = new byte[sliceSize];
            for (t = tBegin; t <= tEnd; t++) {
                for (z = sBegin; z <= sEnd; z++,count++) {
                    i = t*zDim + z;
                    progressBar.updateValue(count * 100/numberSlices, options.isActiveImage());
                    image.exportSliceXY(i,byteBuffer);
                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)
            break;
            case ModelStorageBase.BYTE:
            case ModelStorageBase.SHORT:
            // store as 16 bit signed short
            shortBuffer = new short[sliceSize];
            byteBuffer = new byte[2*sliceSize];
            for (t = tBegin; t <= tEnd; t++) {
                for (z = sBegin; z <= sEnd; z++,count++) {
                    i = t*zDim + z;
                    progressBar.updateValue(count * 100/numberSlices, options.isActiveImage());
                    image.exportSliceXY(i,shortBuffer);
                    for (j = 0; j < sliceSize; j++) {
                        byteBuffer[2*j] = (byte)(shortBuffer[j] >>> 8);
                        byteBuffer[2*j+1] = (byte)(shortBuffer[j]);
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
            byteBuffer = new byte[4*sliceSize];
            for (t = tBegin; t <= tEnd; t++) {
                for (z = sBegin; z <= sEnd; z++,count++) {
                    i = t*zDim + z;
                    progressBar.updateValue(count * 100/numberSlices, options.isActiveImage());
                    image.exportSliceXY(i,intBuffer);
                    for (j = 0; j < sliceSize; j++) {
                        byteBuffer[4*j] = (byte)(intBuffer[j] >>> 24);
                        byteBuffer[4*j+1] = (byte)(intBuffer[j] >>> 16);
                        byteBuffer[4*j+2] = (byte)(intBuffer[j] >>> 8);
                        byteBuffer[4*j+3] = (byte)(intBuffer[j]);
                    }
                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)
            break;
            case ModelStorageBase.FLOAT:
            // store as 32 bit float
            floatBuffer = new float[sliceSize];
            byteBuffer = new byte[4*sliceSize];
            for (t = tBegin; t <= tEnd; t++) {
                for (z = sBegin; z <= sEnd; z++,count++) {
                    i = t*zDim + z;
                    progressBar.updateValue(count * 100/numberSlices, options.isActiveImage());
                    image.exportSliceXY(i,floatBuffer);
                    for (j = 0; j < sliceSize; j++) {
                        tmpInt = Float.floatToIntBits(floatBuffer[j]);
                        byteBuffer[4*j] = (byte)(tmpInt >>> 24);
                        byteBuffer[4*j+1] = (byte)(tmpInt >>> 16);
                        byteBuffer[4*j+2] = (byte)(tmpInt >>> 8);
                        byteBuffer[4*j+3] = (byte)(tmpInt);
                    }
                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)
            break;
            case ModelStorageBase.DOUBLE:
            // store as 64 bit double precision float
            doubleBuffer = new double[sliceSize];
            byteBuffer = new byte[8*sliceSize];
            for (t = tBegin; t <= tEnd; t++) {
                for (z = sBegin; z <= sEnd; z++,count++) {
                    i = t*zDim + z;
                    progressBar.updateValue(count * 100/numberSlices, options.isActiveImage());
                    image.exportSliceXY(i,doubleBuffer);
                    for (j = 0; j < sliceSize; j++) {
                        tmpLong = Double.doubleToLongBits(doubleBuffer[j]);
                        byteBuffer[8*j] = (byte)(tmpLong >>> 56);
                        byteBuffer[8*j+1] = (byte)(tmpLong >>> 48);
                        byteBuffer[8*j+2] = (byte)(tmpLong >>> 40);
                        byteBuffer[8*j+3] = (byte)(tmpLong >>> 32);
                        byteBuffer[8*j+4] = (byte)(tmpLong >>> 24);
                        byteBuffer[8*j+5] = (byte)(tmpLong >>> 16);
                        byteBuffer[8*j+6] = (byte)(tmpLong >>> 8);
                        byteBuffer[8*j+7] = (byte)(tmpLong);
                    }
                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)
            break;
        } // switch(image.getType())
        raFile.close();
        progressBar.dispose();
    }



}
