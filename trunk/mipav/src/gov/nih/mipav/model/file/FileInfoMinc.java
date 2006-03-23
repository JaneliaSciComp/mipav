package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

import java.util.*;


/**
 * This class holds all the file information for a MINC file. In addition, it has three classes to organize the data.
 * <P>
 * MINC headers have three main parts: the dimensions, the global attributes, and the variables. There are typically
 * three dimensions, representing the x, y, and z dimensions of the image. There is usually one global attribute, the
 * history. There can be any number of variables.
 * <P>
 * Variables have within them any number of attributes. So for example, "rootvariable", the first variable in all the
 * MINC files I have seen so far, has the following attributes: varid (= "MINC standard variable"), vartype (=
 * "group________"), version (= "MINC version 1.0"), parent (blank), and children (image). Usually there is a variable
 * for each dimension, a variable for the image, a variable for image min, and a variable for image max. There can also
 * be variables containing patient information or anything else useful.
 * <P>
 * See the documentation for the classes MincDimElem, MincAttElem, and MincVarElem for further information. The classes
 * are structured exactly like a MINC file.
 * 
 * @version 1.0 July 1, 2000
 * @author Neva Cherniavsky
 * @see FileMinc
 */
public class FileInfoMinc extends FileInfoBase {
    /**
     * NetCDF defined data type - byte.
     */
    public static final int NC_BYTE = 1;

    /**
     * NetCDF defined data type - character.
     */
    public static final int NC_CHAR = 2;

    /**
     * NetCDF defined data type - short.
     */
    public static final int NC_SHORT = 3;

    /**
     * NetCDF defined data type - integer.
     */
    public static final int NC_INT = 4;

    /**
     * NetCDF defined data type - float.
     */
    public static final int NC_FLOAT = 5;

    /**
     * NetCDF defined data type - double.
     */
    public static final int NC_DOUBLE = 6;

    /**
     * NetCDF defined data type - dimension.
     */
    public static final int NC_DIMENSION = 10;

    /**
     * NetCDF defined data type - variable.
     */
    public static final int NC_VARIABLE = 11;

    /**
     * NetCDF defined data type - attribute.
     */
    public static final int NC_ATTRIBUTE = 12;

    public int offset = 0;

    public int numrecs;

    public double vmin = -1.0;

    public double vmax = -1.0;

    private boolean flipped = true;

    private FileMincDimElem[] dimArray;

    private FileMincAttElem[] gattArray;

    private FileMincVarElem[] varArray;

    /**
     * Minc var equivalents to anonymizable Dicom tags.
     * Must be in the same order as FileInfoDicom.anonymizeTagIDs.
     * @see FileInfoDicom#anonymizeTagIDs
     */
    public static final String[] dicomToMincVarMap = {
        "patient,full_name",// patient name
        "patient,identification",// patient ID
        "patient,birthdate",// patient's birth date
        null,// patient's birth time
        "patient,sex",// patient's sex
        null,// patient Insurance plan code sequence
        null,// other patient IDs
        null,// other patient names
        null,// patient's birth name
        "patient,age",// patient's age
        null,// patient's size
        null,// patient's weight
        null,// patient's address
        null,// patient's mother's birth name
        null,// medical record locator
        null,// patient's telephone numbers
        null,// patient ethnic group
        null,// occupation
        null,// additional patient's history
        null,// patient's last menstrual date
        null,// patient religious preference
        null,// patient comments
        
        null,// instance creator UID
        null,// SOP instance UID
        null,// accession number
        "study,institution",// institution name
        "study,institution",// institution address
        null,// referring physician's name
        null,// referring physician's address
        null,// referring physician's telephone numbers
        null,// station name
        null,// study description
        null,// series description
        null,// institutional department name
        null,// physician(s) of record
        null,// performing physician's name
        null,// name of physician reading study
        null,// operator's name
        null,// admitting diagnoses description
        null,// Referenced SOP instance UID
        null,// derivation description

        null,// device serial number
        null,// protocol name

        null,// study instance UID
        "study,acquisition_id",// series instance UID
        "study,study_id",// study ID
        null,// frame of reference UID
        null,// synchronization frame of reference UID
        null,// image comments

        //null,// request attributes sequence
        null,// UID
        null,// content sequence

        null,// storage media file-set UID

        null,// referenced frame of reference UID
        null,// related frame of reference UID
    };
    
    /**
     * MINC file information constructor
     * @param name file name
     * @param directory file directory
     * @param format format (in this case, MINC)
     */
    public FileInfoMinc(String name, String directory, int format) {
        super(name, directory, format);
    }

    /**
     * Displays important information about the image.
     * @param dlog where to display the info
     * @param matrix the transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");
        dialog.append("Dimension information:\n");
        for (int j = 0; j < dimArray.length; j++ ) {
            dialog.append(dimArray[j].toString());
        }
        dialog.append("\nGlobal Attribute information:\n");
        if (gattArray != null) {
            for (int j = 0; j < gattArray.length; j++ ) {
                dialog.append(gattArray[j].toString());
            }
        }
        dialog.append("\n\nVariable information:\n");
        for (int j = 0; j < varArray.length; j++ ) {
            dialog.append(varArray[j].toString());
        }
    }

    /**
     * In MINC images, "real" values for pixels are calculated by taking the given image min and image max and rescaling
     * the data accordingly. Image min and image max are given per slice.
     * @param rescaleIntercept Array of rescale intercepts to set
     * @param rescaleSlope Array of rescale slopes to set
     */
    public void calculateRescaleIntercept(double[] rescaleIntercept, double[] rescaleSlope) {
        int min_index = -1, max_index = -1;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals("image-min"))
                min_index = i;
            if (varArray[i].name.equals("image-max"))
                max_index = i;
        }
        try {
            for (int i = 0; i < rescaleSlope.length; i++ ) {
                rescaleSlope[i] = calculateSlope( ((Double) varArray[max_index].values.elementAt(i)).doubleValue(),
                        ((Double) varArray[min_index].values.elementAt(i)).doubleValue());
                rescaleIntercept[i] = calculateIntercept( ((Double) varArray[min_index].values.elementAt(i))
                        .doubleValue(), rescaleSlope[i]);
            }
        } catch (ArrayIndexOutOfBoundsException error) {
            for (int i = 0; i < rescaleSlope.length; i++ ) {
                rescaleSlope[i] = 1.0;
                rescaleIntercept[i] = 0.0;
            }
        }
    }

    /**
     * Calculates rescale slope given a min and a max value.
     * @param max Max value.
     * @param min Min value.
     * @return Rescale slope
     */
    public double calculateSlope(double max, double min) {
        return (max - min) / (vmax - vmin);
    }

    /**
     * Calculates rescale intercept given a min and a slope value.
     * @param min Min value.
     * @param slope Slope value.
     * @return Rescale intercept.
     */
    public double calculateIntercept(double min, double slope) {
        return (min - slope * vmin);
    }

    /**
     * Gets the array with the dimension information
     * @return the array
     */
    public FileMincDimElem[] getDimArray() {
        return dimArray;
    }

    /**
     * Gets the array with the attribute information
     * @return the array
     */
    public FileMincAttElem[] getGattArray() {
        return gattArray;
    }

    /**
     * Gets the array with the variable information
     * @return the array
     */
    public FileMincVarElem[] getVarArray() {
        return varArray;
    }

    /**
     * Constructs the dimension array with the specified length
     * @param length the length of the array
     */
    public void createDimArray(int length) {
        dimArray = new FileMincDimElem[length];
    }

    /**
     * Constructs the global attribute array with the specified length
     * @param length the length of the array
     */
    public void createGattArray(int length) {
        gattArray = new FileMincAttElem[length];
    }

    /**
     * Constructs the variable array with the specified length
     * @param length the length of the array
     */
    public void createVarArray(int length) {
        varArray = new FileMincVarElem[length];
    }

    /**
     * Puts a new dimension element with the specified parameters into the dimension array at the gievn index. The name
     * is usually xspace, yspace, or zspace; the length is the corresponding size of that dimension.
     * @param name the name of the dimension element
     * @param length the length of the dimension element
     * @param index the index to put this element into the array
     */
    public void addDimElem(String name, int length, int index) {
        FileMincDimElem elem = new FileMincDimElem(name, length);
        dimArray[index] = elem;
    }

    /**
     * Puts a new global attribute array element with the specified parameters into the global attribute array at the
     * given index. Usually there is only one element in the global attribute array: the history.
     * @param name the name of the global attribute element
     * @param type the data type, i.e. byte, char, int, etc (see statically defined types)
     * @param length the length of the element
     * @param index the index into the array to put this element at
     */
    public void addGattElem(String name, int type, int length, int index) {
        FileMincAttElem elem = new FileMincAttElem(name, type, length);
        gattArray[index] = elem;
    }

    /**
     * Sets the value of the attribute element; used for setting attribute of variables. A MincVarElem has an array of
     * attributes and this method is called on those attributes.
     * @param elem the attribute element to add the value to.
     * @param value the value to set the element to.
     * @param index the index into the value array to set this value at.
     * @see FileMincAttElem#setValue
     */
    public void addAttValue(FileMincAttElem elem, Object value, int index) {
        elem.setValue(value, index);
    }

    /**
     * Puts a new variable element with the specified paramenters into the array at the given index. There are other
     * attributes to set for variables, in particular the attribute array, but that gets set in a different place.
     * @param name the name of this variable element
     * @param nelems the number of elements in the attribute array
     * @param dimid the dimension id array, usually empty but for "image" containing information about the dimensions
     * @param index index into the variable array to put this variable
     * @see FileMincVarElem
     */
    public void addVarElem(String name, int nelems, int[] dimid, int index) {
        FileMincVarElem elem = new FileMincVarElem(name, nelems, dimid);
        varArray[index] = elem;
    }

    /**
     * Accessor that returns the dimension element at the indicated index in the dimension array
     * @param index index where the dimension element is located
     * @return dimArray[index]
     */
    public FileMincDimElem getDimElem(int index) {
        return dimArray[index];
    }

    /**
     * Accessor that returns the global attribute element at the indicated index in the global attribute array
     * @param index index where the global attribute element is located
     * @return gattArray[index]
     */
    public FileMincAttElem getGattElem(int index) {
        return gattArray[index];
    }

    /**
     * Accessor that returns the variable element at the indicated index in the variable array
     * @param index index where the variable element is located
     * @return varArray[index]
     */
    public FileMincVarElem getVarElem(int index) {
        return varArray[index];
    }

    /**
     * Gets the flipped flag.
     * @return <code>true</code> means image should be flipped because it is upside down.
     */
    public boolean isFlipped() {
        return flipped;
    }

    /**
     * Accessor that gets the "start" variable values, adjusted so that [0] holds the image x axis value, [1] the image
     * y axis value, and [2] the image z axis value. It's important that this is accessed AFTER setFlipInfo() and
     * setImportantImageInfo(). Otherwise it won't return the proper values.
     * @param begin slice to begin the start variable on.
     * @return The "start" values for the image.
     */
    public float[] getStart(int begin) {
        double x = 0;
        double y = 0;
        double z = 0;
        double zRes = 1;
        String spacex, spacey, spacez;
        spacex = getDimElem(2).name;
        spacey = getDimElem(1).name;
        spacez = getDimElem(0).name;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals(spacex)) {
                x = varArray[i].start;
            }
            if (varArray[i].name.equals(spacey)) {
                y = varArray[i].start;
            }
            if (varArray[i].name.equals(spacez)) {
                z = varArray[i].start;
                zRes = varArray[i].resolution;
            }
        }
        float[] res = new float[getExtents().length];
        if (res.length == 2) {
            res[0] = (float) x;
            res[1] = (float) y;
        } else if (res.length == 3) {
            res[0] = (float) x;
            res[1] = (float) y;
            if (axisOrientation[2] == ORI_L2R_TYPE || axisOrientation[2] == ORI_S2I_TYPE
                    || axisOrientation[2] == ORI_P2A_TYPE)
                res[2] = (float) (z + ( -zRes * begin));
            else
                res[2] = (float) (z + (zRes * begin));
        }
        return res;
    }

    /**
     * Sets start locations of each axis
     * @param origin the image origin
     */
    public void setStartLocations(float[] origin) {
        if (origin.length != 3) {
            Preferences.debug("Start locations array must be of length 3.\n");
            return;
        }
        for (int i = 0; i < origin.length; i++ )
            setStartLocation(origin[i], i);
        super.setOrigin(origin);
    }

    /**
     * Sets the start location of the specified axis.
     * @param originCoord origin coord.
     * @param axis Axis of orientation; x is 0, y is 1, z is 2.
     */
    public void setStartLocation(float originCoord, int axis) {
        if (axis < 0 || axis > 2) {
            Preferences.debug("Error: Axis must be 0, 1, or 2.\n");
            return;
        }
        String space;
        int index = 0;
        // opposite axis: if axis is 2 we need to getDimElem(0). See previous examples
        // of String spacez = getDimElem(0).name. Minc stores the x dimension in
        // 2, the y dimension in 1, and the z dimension in 0.
        space = getDimElem(Math.abs(axis - 2)).name;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals(space)) {
                index = i;
            }
        }
        varArray[index].start = originCoord;
    }

    /**
     * Accessor that gets the "step" variable values, adjusted so that [0] holds the image x axis value, [1] the image y
     * axis value, and [2] the image z axis value.
     * @return The "step" values for the image.
     */
    public float[] getStep() {
        double xRes = 1;
        double yRes = 1;
        double zRes = 1;
        String spacex, spacey, spacez;
        spacex = getDimElem(2).name;
        spacey = getDimElem(1).name;
        spacez = getDimElem(0).name;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals(spacex)) {
                xRes = varArray[i].resolution;
            }
            if (varArray[i].name.equals(spacey)) {
                yRes = varArray[i].resolution;
            }
            if (varArray[i].name.equals(spacez)) {
                zRes = varArray[i].resolution;
            }
        }
        float[] res = new float[getExtents().length];
        if (res.length == 2) {
            res[0] = (float) xRes;
            res[1] = (float) yRes;
        } else if (res.length == 3) {
            res[0] = (float) xRes;
            res[1] = (float) yRes;
            res[2] = (float) zRes;
        }
        return res;
    }

    /**
     * Accessor that gets the "step" variable values, as they are stored in the header file.
     * @return The "step" values for the image.
     */
    public float[] getStepMinc() {
        double xRes = 1;
        double yRes = 1;
        double zRes = 1;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals("xspace")) {
                xRes = varArray[i].resolution;
            }
            if (varArray[i].name.equals("yspace")) {
                yRes = varArray[i].resolution;
            }
            if (varArray[i].name.equals("zspace")) {
                zRes = varArray[i].resolution;
            }
        }
        float[] res = new float[getExtents().length];
        if (res.length == 2) {
            res[0] = (float) xRes;
            res[1] = (float) yRes;
        } else if (res.length == 3) {
            res[0] = (float) xRes;
            res[1] = (float) yRes;
            res[2] = (float) zRes;
        }
        return res;
    }

    /**
     * Accessor that gets the "start" variable values, with the "xspace" in 0, "yspace" in 1, and "zspace" in 2. This
     * differs from the other methods because it doesn't place the values so that they correspond to image x, y, and z.
     * Gets values as stored in MINC header, not modified to account for the different origin.
     * @return The "start" values for the image.
     */
    public float[] getStartMinc() {
        double x = 0;
        double y = 0;
        double z = 0;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals("xspace")) {
                x = varArray[i].trueStart;
            }
            if (varArray[i].name.equals("yspace")) {
                y = varArray[i].trueStart;
            }
            if (varArray[i].name.equals("zspace")) {
                z = varArray[i].trueStart;
            }
        }
        float[] res = new float[getExtents().length];
        if (res.length == 2) {
            res[0] = (float) x;
            res[1] = (float) y;
        } else if (res.length == 3) {
            res[0] = (float) x;
            res[1] = (float) y;
            res[2] = (float) z;
        }
        return res;
    }

    /**
     * Accessor that gets the "start" variable values based on the image orientation. Gets the "true" start, that is,
     * the start stored in the image file, not the one adjusted for flipping.
     * @return The "start" values for the image.
     */
    public float[] getStartOrient() {
        double x = 0;
        double y = 0;
        double z = 0;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals("xspace")) {
                x = varArray[i].trueStart;
            }
            if (varArray[i].name.equals("yspace")) {
                y = varArray[i].trueStart;
            }
            if (varArray[i].name.equals("zspace")) {
                z = varArray[i].trueStart;
            }
        }
        float[] res = new float[getExtents().length];
        for (int i = 0; i < 3; i++ ) {
            if (axisOrientation[i] == ORI_R2L_TYPE || axisOrientation[i] == ORI_L2R_TYPE)
                res[i] = (float) x;
            if (axisOrientation[i] == ORI_A2P_TYPE || axisOrientation[i] == ORI_P2A_TYPE)
                res[i] = (float) y;
            if (axisOrientation[i] == ORI_S2I_TYPE || axisOrientation[i] == ORI_I2S_TYPE)
                res[i] = (float) z;
        }
        return res;
    }

    /**
     * Sets the units of the dimensions, as in millimeters, inches, etc. Called after the header has been read in so the
     * values within the varArray have been set properly already.
     */
    public void setUnits() {
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].units != null) {
                if (varArray[i].name.equals("xspace")) {
                    if (varArray[i].units.equals("mm"))
                        setUnitsOfMeasure(MILLIMETERS, 0);
                    else if (varArray[i].units.equals("in"))
                        setUnitsOfMeasure(INCHES, 0);
                }
                if (varArray[i].name.equals("yspace")) {
                    if (varArray[i].units.equals("mm"))
                        setUnitsOfMeasure(MILLIMETERS, 1);
                    else if (varArray[i].units.equals("in"))
                        setUnitsOfMeasure(INCHES, 1);
                }
                if (varArray[i].name.equals("zspace")) {
                    if (varArray[i].units.equals("mm"))
                        setUnitsOfMeasure(MILLIMETERS, 2);
                    else if (varArray[i].units.equals("in"))
                        setUnitsOfMeasure(INCHES, 2);
                }
            }
        }
    }

    /**
     * Sets the resolutions from the variable array based on the orientation of the image. The "zspace" in MINC refers
     * to the inferior-superior axis, whereas in MIPAV the z resolution variable is the slices (space between slices)
     * regardless of the orientation. Therefore for proper display we must convert between them.
     * @param orientation The orientation of the image (sagittal, coronal, or axial).
     */
    public void setResolutions(int orientation) {
        double xRes = 1.0;
        double yRes = 1.0;
        double zRes = 1.0;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals("xspace"))
                xRes = varArray[i].resolution;
            if (varArray[i].name.equals("yspace"))
                yRes = varArray[i].resolution;
            if (varArray[i].name.equals("zspace"))
                zRes = varArray[i].resolution;
        }
        float[] res = new float[getExtents().length];
        if (res.length == 2) {
            res[0] = Math.abs((float) xRes);
            res[1] = Math.abs((float) yRes);
        } else if (res.length == 3) {
            switch (orientation) {
                case SAGITTAL:
                    res[2] = Math.abs((float) xRes);
                    res[0] = Math.abs((float) yRes);
                    res[1] = Math.abs((float) zRes);
                    break;
                case CORONAL:
                    res[0] = Math.abs((float) xRes);
                    res[2] = Math.abs((float) yRes);
                    res[1] = Math.abs((float) zRes);
                    break;
                case AXIAL:
                default:
                    res[0] = Math.abs((float) xRes);
                    res[1] = Math.abs((float) yRes);
                    res[2] = Math.abs((float) zRes);
                    break;
            }
        }
        setResolutions(res);
    }

    /**
     * Resets the start variable for the x and y dimension. All MINC images are stored "upside down". When the user
     * drags the mouse in a MINC image, the proper world dimensions show up in the message frame. For these to be
     * correct, the MIPAV y start value needs to be inverted. The MIPAV y depends on the orientation; for sagittal and
     * coronal images, the MIPAV y is the MINC z (superior-inferior). For axial images, the MIPAV y is the MINC y
     * (anterior-posterior).
     * <p>
     * For the x dimension, MINC almost always has the opposite sign for the start variable. This is because MINC images
     * consider the patient x to be left to right, the patient y to be posterior to anterior, and the patient z to be
     * inferior to superior. However, in MIPAV we use the DICOM patient orientation system, which considers the patient
     * x to be right to left, the patient y to be anterior to posterior, and the patient z to be inferior to superior.
     * So a "-33" in the y dimension of a MINC image is 33 in the posterior direction, whereas in MIPAV it is 33 in the
     * anterior direction. The only time they line up is for the patient z - inferior to superior. And it is very rare
     * for the patient z to be the image's x-axis.
     */
    public void setFlipInfo() {
        String spacex, spacey, spacez;
        int indexx = 0, indexy = 0, indexz = 0;
        double beginx = 0, beginy = 0, beginz = 0;
        double resy = 0;
        spacex = getDimElem(2).name;
        spacey = getDimElem(1).name;
        spacez = getDimElem(0).name;
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals(spacey)) {
                indexy = i;
                resy = varArray[i].resolution;
                beginy = varArray[i].start;
            }
            if (varArray[i].name.equals(spacex)) {
                indexx = i;
                beginx = varArray[i].start;
            }
            if (varArray[i].name.equals(spacez)) {
                indexz = i;
                beginz = varArray[i].start;
            }
        }
        int length = getExtents()[1];
        beginy = beginy + resy * (length - 1);
        if ( !spacex.equals("zspace"))
            varArray[indexx].start = -beginx;
        if ( !spacey.equals("zspace") && flipped == true)
            varArray[indexy].start = -beginy;
        else
            varArray[indexy].start = beginy;
        if ( !spacez.equals("zspace"))
            varArray[indexz].start = -beginz;
    }

    /**
     * Sets necessary image information.
     */
    public void setImportantImageInfo() {
        String spacex = getDimElem(2).name;
        Preferences.debug("spacex = " + spacex + "\n");
        String spacey = getDimElem(1).name;
        Preferences.debug("spacey = " + spacey + "\n");
        String spacez = getDimElem(0).name;
        Preferences.debug("spacez = " + spacez + "\n");
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals("image")) {
                offset = varArray[i].begin;
                Preferences.debug("Image offset = " + offset + "\n");
                switch (varArray[i].nc_type) {
                    case NC_BYTE:
                        if (varArray[i].signtype.equals("unsigned")) {
                            Preferences.debug("Data type = UBYTE\n");
                            setDataType(ModelStorageBase.UBYTE);
                        } else {
                            Preferences.debug("Data type = BYTE\n");
                            setDataType(ModelStorageBase.BYTE);
                        }
                        break;
                    case NC_SHORT:
                        if (varArray[i].signtype.equals("unsigned")) {
                            Preferences.debug("Data type = USHORT\n");
                            setDataType(ModelStorageBase.USHORT);
                        } else {
                            Preferences.debug("Data type = SHORT\n");
                            setDataType(ModelStorageBase.SHORT);
                        }
                        break;
                    case NC_INT:
                        if (varArray[i].signtype.equals("unsigned")) {
                            Preferences.debug("Data type = UINTEGER\n");
                            setDataType(ModelStorageBase.UINTEGER);
                        } else {
                            Preferences.debug("Data type = INTEGER\n");
                            setDataType(ModelStorageBase.INTEGER);
                        }
                        break;
                    case NC_FLOAT:
                        Preferences.debug("Data type = FLOAT\n");
                        setDataType(ModelStorageBase.FLOAT);
                        break;
                    case NC_DOUBLE:
                        Preferences.debug("Data type = DOUBLE\n");
                        setDataType(ModelStorageBase.DOUBLE);
                        break;
                    default:
                        Preferences.debug("varArray[" + i + "].nc_type illegally = " + varArray[i].nc_type + "\n");
                        MipavUtil.displayError("Invalid type in FileInfoMinc");
                }
                for (int j = 0; j < varArray[i].vattArray.length; j++ ) {
                    FileMincAttElem elem = varArray[i].vattArray[j];
                    if (elem.name.equals("valid_range")) {
                        switch (elem.nc_type) {
                            case NC_BYTE:
                                vmin = (double) ((Byte) elem.values[0]).byteValue();
                                vmax = (double) ((Byte) elem.values[1]).byteValue();
                                break;
                            case NC_CHAR:
                                vmin = (double) ((Character) elem.values[0]).charValue();
                                vmax = (double) ((Character) elem.values[1]).charValue();
                                break;
                            case NC_SHORT:
                                vmin = (double) ((Short) elem.values[0]).shortValue();
                                vmax = (double) ((Short) elem.values[1]).shortValue();
                                break;
                            case NC_INT:
                                vmin = (double) ((Integer) elem.values[0]).intValue();
                                vmax = (double) ((Integer) elem.values[1]).intValue();
                                break;
                            case NC_FLOAT:
                                vmin = (double) ((Float) elem.values[0]).floatValue();
                                vmax = (double) ((Float) elem.values[1]).floatValue();
                                break;
                            case NC_DOUBLE:
                                vmin = ((Double) elem.values[0]).doubleValue();
                                vmax = ((Double) elem.values[1]).doubleValue();
                        }
                        Preferences.debug("vmin = " + vmin + "\n");
                        Preferences.debug("vmax = " + vmax + "\n");
                    } else if (elem.name.equals("valid_max")) {
                        switch (elem.nc_type) {
                            case NC_BYTE:
                                vmax = (double) ((Byte) elem.values[0]).byteValue();
                                break;
                            case NC_CHAR:
                                vmax = (double) ((Character) elem.values[0]).charValue();
                                break;
                            case NC_SHORT:
                                vmax = (double) ((Short) elem.values[0]).shortValue();
                                break;
                            case NC_INT:
                                vmax = (double) ((Integer) elem.values[0]).intValue();
                                break;
                            case NC_FLOAT:
                                vmax = (double) ((Float) elem.values[0]).floatValue();
                                break;
                            case NC_DOUBLE:
                                vmax = ((Double) elem.values[0]).doubleValue();
                        }
                        Preferences.debug("vmax = " + vmax + "\n");
                    } else if (elem.name.equals("valid_min")) {
                        switch (elem.nc_type) {
                            case NC_BYTE:
                                vmin = (double) ((Byte) elem.values[0]).byteValue();
                                break;
                            case NC_CHAR:
                                vmin = (double) ((Character) elem.values[0]).charValue();
                                break;
                            case NC_SHORT:
                                vmin = (double) ((Short) elem.values[0]).shortValue();
                                break;
                            case NC_INT:
                                vmin = (double) ((Integer) elem.values[0]).intValue();
                                break;
                            case NC_FLOAT:
                                vmin = (double) ((Float) elem.values[0]).floatValue();
                                break;
                            case NC_DOUBLE:
                                vmin = ((Double) elem.values[0]).doubleValue();
                        }
                        Preferences.debug("vmin = " + vmin + "\n");
                    }
                }
            } else if (varArray[i].name.equals(spacex)) {
                // axisOrientation[0] = setOrientType(varArray[i].comments);
                // if (axisOrientation[0] == ORI_UNKNOWN_TYPE) {
                axisOrientation[0] = setOrientType(spacex, (varArray[i].resolution > 0));
                // }
                if (varArray[i].cosines != null) {
                    for (int j = 0; j < 3; j++ ) {
                        if (varArray[i].cosines[j] < 0) {
                            axisOrientation[0] = oppositeOrient(axisOrientation[0]);
                        }
                    }
                }
                switch (axisOrientation[0]) {
                    case ORI_UNKNOWN_TYPE:
                        Preferences.debug("axisOrientation[0] = ORI_UNKNOWN_TYPE\n");
                        break;
                    case ORI_R2L_TYPE:
                        Preferences.debug("axisOrientation[0] = ORI_R2L_TYPE\n");
                        break;
                    case ORI_L2R_TYPE:
                        Preferences.debug("axisOrientation[0] = ORI_L2R_TYPE\n");
                        break;
                    case ORI_P2A_TYPE:
                        Preferences.debug("axisOrientation[0] = ORI_P2A_TYPE\n");
                        break;
                    case ORI_A2P_TYPE:
                        Preferences.debug("axisOrientation[0] = ORI_A2P_TYPE\n");
                        break;
                    case ORI_I2S_TYPE:
                        Preferences.debug("axisOrientation[0] = ORI_I2S_TYPE\n");
                        break;
                    case ORI_S2I_TYPE:
                        Preferences.debug("axisOrientation[0] = ORI_S2I_TYPE\n");
                        break;
                }
            } else if (varArray[i].name.equals(spacey)) {
                // opposite because MINC's origin is at the bottom left and
                // ours is at the top left
                // axisOrientation[1] = oppositeOrient(setOrientType(varArray[i].comments));
                // if (axisOrientation[1] == ORI_UNKNOWN_TYPE) {
                // axisOrientation[1] = oppositeOrient(setOrientType(spacey, (varArray[i].resolution > 0)));
                axisOrientation[1] = setOrientType(spacey, (varArray[i].resolution > 0));
                // }
                if (varArray[i].cosines != null) {
                    for (int j = 0; j < 3; j++ ) {
                        if (varArray[i].cosines[j] < 0) {
                            axisOrientation[1] = oppositeOrient(axisOrientation[1]);
                        }
                    }
                }
            } else if (varArray[i].name.equals(spacez)) {
                // axisOrientation[2] = setOrientType(varArray[i].comments);
                // if (axisOrientation[2] == ORI_UNKNOWN_TYPE) {
                axisOrientation[2] = setOrientType(spacez, (varArray[i].resolution > 0));
                // }
                if (varArray[i].cosines != null) {
                    for (int j = 0; j < 3; j++ ) {
                        if (varArray[i].cosines[j] < 0) {
                            axisOrientation[2] = oppositeOrient(axisOrientation[2]);
                        }
                    }
                }
                switch (axisOrientation[2]) {
                    case ORI_UNKNOWN_TYPE:
                        Preferences.debug("axisOrientation[2] = ORI_UNKNOWN_TYPE\n");
                        break;
                    case ORI_R2L_TYPE:
                        Preferences.debug("axisOrientation[2] = ORI_R2L_TYPE\n");
                        break;
                    case ORI_L2R_TYPE:
                        Preferences.debug("axisOrientation[2] = ORI_L2R_TYPE\n");
                        break;
                    case ORI_P2A_TYPE:
                        Preferences.debug("axisOrientation[2] = ORI_P2A_TYPE\n");
                        break;
                    case ORI_A2P_TYPE:
                        Preferences.debug("axisOrientation[2] = ORI_A2P_TYPE\n");
                        break;
                    case ORI_I2S_TYPE:
                        Preferences.debug("axisOrientation[2] = ORI_I2S_TYPE\n");
                        break;
                    case ORI_S2I_TYPE:
                        Preferences.debug("axisOrientation[2] = ORI_S2I_TYPE\n");
                        break;
                }
            }
        }
        if ( (axisOrientation[1] == ORI_A2P_TYPE) || (axisOrientation[1] == ORI_S2I_TYPE)) {
            flipped = false;
        } else {
            flipped = true;
            axisOrientation[1] = oppositeOrient(axisOrientation[1]);
        }
        switch (axisOrientation[1]) {
            case ORI_UNKNOWN_TYPE:
                Preferences.debug("axisOrientation[1] = ORI_UNKNOWN_TYPE\n");
                break;
            case ORI_R2L_TYPE:
                Preferences.debug("axisOrientation[1] = ORI_R2L_TYPE\n");
                break;
            case ORI_L2R_TYPE:
                Preferences.debug("axisOrientation[1] = ORI_L2R_TYPE\n");
                break;
            case ORI_P2A_TYPE:
                Preferences.debug("axisOrientation[1] = ORI_P2A_TYPE\n");
                break;
            case ORI_A2P_TYPE:
                Preferences.debug("axisOrientation[1] = ORI_A2P_TYPE\n");
                break;
            case ORI_I2S_TYPE:
                Preferences.debug("axisOrientation[1] = ORI_I2S_TYPE\n");
                break;
            case ORI_S2I_TYPE:
                Preferences.debug("axisOrientation[1] = ORI_S2I_TYPE\n");
                break;
        }
    }

    /**
     * Checks if we need to reset the start locations or axis orientations. If so, file writer will call up a dialog
     * with all variables properly set, and this will call a different version of the write header. The idea is that a
     * MINC file which has been rotated or otherwise drastically changed should call the "Save As" version of the header
     * writer instead of the "Save" version. This method calculates what the original start locations and axis
     * orientations were and returns false if and only if both the start locations and the axis orientations are as they
     * were originally.
     * @return Flag indicating if the file writer should reset the start locations and orientations.
     */
    public boolean resetStartLocationsOrientations() {
        int indexx = 0, indexy = 0, indexz = 0;
        float beginx = 0, beginy = 0, beginz = 0;
        float resy = 1;
        String spacex = getDimElem(2).name;
        String spacey = getDimElem(1).name;
        String spacez = getDimElem(0).name;
        int[] orients = new int[3];
        boolean reorderAxis, resetStart;
        // Get original values for axis orientations and start locations
        for (int i = 0; i < varArray.length; i++ ) {
            if (varArray[i].name.equals(spacex)) {
                orients[0] = setOrientType(spacex, (varArray[i].resolution > 0));
                indexx = i;
                beginx = (float) varArray[i].trueStart;
            } else if (varArray[i].name.equals(spacey)) {
                // opposite because MINC's origin is at the bottom left and
                // ours is at the top left
                orients[1] = setOrientType(spacey, ! (varArray[i].resolution > 0));
                indexy = i;
                resy = (float) varArray[i].resolution;
                beginy = (float) varArray[i].trueStart;
            } else if (varArray[i].name.equals(spacez)) {
                orients[2] = setOrientType(spacez, (varArray[i].resolution > 0));
                indexz = i;
                beginz = (float) varArray[i].trueStart;
            }
        }
        int length = getExtents()[1];
        beginy = beginy + resy * (length - 1);
        if ( !spacex.equals("zspace"))
            beginx = -beginx;
        if ( !spacey.equals("zspace"))
            beginy = -beginy;
        if ( !spacez.equals("zspace"))
            beginz = -beginz;
        // now check that original orients are same as current orients;
        // if not, we will need to reorder things.
        if (orients[0] == axisOrientation[0] && orients[1] == axisOrientation[1] && orients[2] == axisOrientation[2])
            reorderAxis = false;
        else
            reorderAxis = true;
        // check if original starts are the same as the current starts.
        if (beginx == (float) varArray[indexx].start && beginy == (float) varArray[indexy].start
                && beginz == (float) varArray[indexz].start)
            resetStart = false;
        else
            resetStart = true;
        // if both are false, return false; otherwise, return true.
        return (resetStart || reorderAxis);
    }

    /**
     * Helper method to set the axis orientations.
     * @param comments String giving orientation information.
     * @return The proper axis orientation for that space.
     */
    private int setOrientType(String comments) {
        if (comments.indexOf("left to right") != -1)
            return ORI_L2R_TYPE;
        else if (comments.indexOf("right to left") != -1)
            return ORI_R2L_TYPE;
        else if (comments.indexOf("posterior to anterior") != -1)
            return ORI_P2A_TYPE;
        else if (comments.indexOf("anterior to posterior") != -1)
            return ORI_A2P_TYPE;
        else if (comments.indexOf("inferior to superior") != -1)
            return ORI_I2S_TYPE;
        else if (comments.indexOf("superior to inferior") != -1)
            return ORI_S2I_TYPE;
        else
            return ORI_UNKNOWN_TYPE;
    }

    /**
     * Helper method to set the axis orientations.
     * @param space The space - "xspace", "yspace", or "zspace".
     * @param positive Flag indicating if the space is moving in a positive direction.
     * @return The proper axis orientation for that space.
     */
    private int setOrientType(String space, boolean positive) {
        if (positive) {
            if (space.equals("xspace"))
                return ORI_L2R_TYPE;
            else if (space.equals("yspace"))
                return ORI_P2A_TYPE;
            else if (space.equals("zspace"))
                return ORI_I2S_TYPE;
        } else {
            if (space.equals("xspace"))
                return ORI_R2L_TYPE;
            else if (space.equals("yspace"))
                return ORI_A2P_TYPE;
            else if (space.equals("zspace"))
                return ORI_S2I_TYPE;
        }
        return ORI_UNKNOWN_TYPE;
    }

    /**
     * Removes requested identifying info.  Can remove info stored in the minc header
     * in a dicom-like structure and in a more minc-like info structure.
     * 
     * @param list the list of tags to remove; it MUST correspond to anonymizeTagIDs list (or spurious errors result),
     *            and it must be of the same length, or this will throw an IllegalArgumentException.
     */
    public final void anonymize(boolean[] list) {
        if (list.length != FileInfoDicom.anonymizeTagIDs.length) {
            throw new IllegalArgumentException("Anonymize list not of correct size!");
        }

        // DICOM type 2 fields. Existance is required, although value is not.
        for (int i = 0; i < 2; i++) {
            try {
                if (list[i]) {
                    for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {
                        //System.out.println("looking for " + "dicom_0x" + getDicomTag(FileInfoDicom.anonymizeTagIDs[i]));
                        //System.out.println("found " + getVarElem(varIndex).name);
                        if (getVarElem(varIndex).name.equals("dicom_0x" + getTagGroup(FileInfoDicom.anonymizeTagIDs[i]))) {
                            for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {
                                //System.out.println("looking for " + "el_0x" + getDicomElem(FileInfoDicom.anonymizeTagIDs[i]));
                                //System.out.println("found " + getVarElem(varIndex).getVattElem(attIndex).name);
                                if (getVarElem(varIndex).getVattElem(attIndex).name.equals("el_0x" + getTagElem(FileInfoDicom.anonymizeTagIDs[i]))) {
                                    //System.out.println("want to erase " + FileInfoDicom.anonymizeTagIDs[i] + " -- " + getVarElem(varIndex).getVattElem(attIndex).toString());
                                    getVarElem(varIndex).getVattElem(attIndex).nelems = 1;
                                    getVarElem(varIndex).getVattElem(attIndex).values = new Object[] {new Character(' ')};
                                    //System.out.println("value now " + FileInfoDicom.anonymizeTagIDs[i] + " -- " + getVarElem(varIndex).getVattElem(attIndex).toString());
                                }
                            }
                        }
                    }
                }
            } catch (NullPointerException npe) {
                // an IllegalArgumentException is probably not right here....
                throw new IllegalArgumentException("("+FileInfoDicom.anonymizeTagIDs[i]+") is a required type 2 tag.");
            }
        }
        
        // all other fields to anonymize are DICOM type 3 fields and 
        // are neither required to have an entry nor required to exist 
        // in the image info.
        for (int i = 2; i < FileInfoDicom.anonymizeTagIDs.length; i++) {
            // change each of the following tags to (empty)
            // if we are asked to anonymize this info and if the tag exists in the hashtable.
            
            if (list[i]) {
                for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {
                    //System.out.println("looking for " + "dicom_0x" + getDicomTag(FileInfoDicom.anonymizeTagIDs[i]));
                    //System.out.println("found " + getVarElem(varIndex).name);
                    if (getVarElem(varIndex).name.equals("dicom_0x" + getTagGroup(FileInfoDicom.anonymizeTagIDs[i]))) {
                        for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {
                            //System.out.println("looking for " + "el_0x" + getDicomElem(FileInfoDicom.anonymizeTagIDs[i]));
                            //System.out.println("found " + getVarElem(varIndex).getVattElem(attIndex).name);
                            if (getVarElem(varIndex).getVattElem(attIndex).name.equals("el_0x" + getTagElem(FileInfoDicom.anonymizeTagIDs[i]))) {
                                //System.out.println("want to erase " + FileInfoDicom.anonymizeTagIDs[i] + " -- " + getVarElem(varIndex).getVattElem(attIndex).toString());
                                getVarElem(varIndex).getVattElem(attIndex).nelems = 0;
                                getVarElem(varIndex).getVattElem(attIndex).values = new Object[] {};
                                //System.out.println("value now " + FileInfoDicom.anonymizeTagIDs[i] + " -- " + getVarElem(varIndex).getVattElem(attIndex).toString());
                            }
                        }
                    }
                }
            }
        }
        
        // *** anonymize fields which may be saved directly in the minc var format (but not converted from dicom tags)
        for (int i = 0; i < FileInfoDicom.anonymizeTagIDs.length; i++) {
            if (list[i] && FileInfoMinc.dicomToMincVarMap[i] != null) {
                for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {
                    //System.out.println("looking for " + getTagGroup(FileInfoMinc.dicomToMincVarMap[i]));
                    //System.out.println("found " + getVarElem(varIndex).name);
                    if (getVarElem(varIndex).name.equals(getTagGroup(FileInfoMinc.dicomToMincVarMap[i]))) {
                        for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {
                            //System.out.println("looking for " + getTagElem(FileInfoMinc.dicomToMincVarMap[i]));
                            //System.out.println("found " + getVarElem(varIndex).getVattElem(attIndex).name);
                            if (getVarElem(varIndex).getVattElem(attIndex).name.equals(getTagElem(FileInfoMinc.dicomToMincVarMap[i]))) {
                                //System.out.println("want to erase " + FileInfoMinc.dicomToMincVarMap + " -- " + getVarElem(varIndex).getVattElem(attIndex).toString());
                                getVarElem(varIndex).getVattElem(attIndex).nelems = 0;
                                getVarElem(varIndex).getVattElem(attIndex).values = new Object[] {};
                                //System.out.println("value now " + FileInfoMinc.dicomToMincVarMap + " -- " + getVarElem(varIndex).getVattElem(attIndex).toString());
                            }
                        }
                    }
                }
            }
        }
        
        // this fileInfo is now an expurgated/sanitised version
    }
    
    /**
     * Extract the group to which a dicom or minc element belongs to.
     * @param fullTag the full element identitfier
     * @return the element group
     */
    private static final String getTagGroup(String fullTag) {
        int index = fullTag.indexOf(",");
        if (index == -1) {
            return null;
        } else {
            return fullTag.substring(0, index);
        }
    }
    
    /**
     * Extract the element identifier (removing the group id).
     * @param fullTag the full element identitfier
     * @return the element id
     */
    private static final String getTagElem(String fullTag) {
        int index = fullTag.indexOf(",");
        if (index == -1) {
            return null;
        } else {
            return fullTag.substring(index + 1);
        }
    }
    
    /**
     * Returns all of the dicom-converted tags in the minc header as a tag-value hashtable.
     * @return a tag-value hashtable
     */
    public Hashtable convertTagsToTable() {
        Hashtable table = new Hashtable();
        
        String groupPrefix = "dicom_0x";
        String elemPrefix = "el_0x";
        
        String group, elem, data;
        int index;
        for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {
            index = getVarElem(varIndex).name.indexOf(groupPrefix);
            if (index != -1) {
                group = getVarElem(varIndex).name.substring(groupPrefix.length());
                for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {
                    index = getVarElem(varIndex).getVattElem(attIndex).name.indexOf(elemPrefix);
                    if (index != -1) {
                        elem = getVarElem(varIndex).getVattElem(attIndex).name.substring(elemPrefix.length());

                        data = new String("");
                        if (getVarElem(varIndex).getVattElem(attIndex).nc_type == FileInfoMinc.NC_CHAR) {
                            for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                data += getVarElem(varIndex).getVattElem(attIndex).values[i];
                            }
                        }
                        else {
                            for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                data += "" + getVarElem(varIndex).getVattElem(attIndex).values[i] + " ";
                            }
                        }
                        
                        table.put("(" + group + "," + elem + ")", data.trim());
                    }
                }
            }
        }
        
        return table;
    }
    
    /**
     * Get the value from the minc info based on a given dicom tag id.
     * @param dicomTag dicom tag (group,elem)
     * @return the value stored in the info (or null if it doesn't exist)
     */
    public String getDicomValue(String dicomTag) {
        String data;
        String groupPrefix = "dicom_0x";
        String elemPrefix = "el_0x";
        
        // look for the dicom tag
        for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {
            if (getVarElem(varIndex).name.equals(groupPrefix + getTagGroup(dicomTag))) {
                for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {
                    if (getVarElem(varIndex).getVattElem(attIndex).name.equals(elemPrefix + getTagElem(dicomTag))) {
                        data = new String("");
                        
                        if (getVarElem(varIndex).getVattElem(attIndex).nc_type == FileInfoMinc.NC_CHAR) {
                            for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                data += getVarElem(varIndex).getVattElem(attIndex).values[i];
                            }
                        }
                        else {
                            for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                data += "" + getVarElem(varIndex).getVattElem(attIndex).values[i] + " ";
                            }
                        }
                        
                        return data;
                    }
                }
            }
        }
        
        // look for the dicom tag's minc equivalent
        String mincTag = null;
        for (int i = 0; i < FileInfoDicom.anonymizeTagIDs.length; i++) {
            if (FileInfoDicom.anonymizeTagIDs[i].equals(dicomTag)) {
                mincTag = dicomToMincVarMap[i];
            }
        }
        
        // get the minc equivalent
        if (mincTag != null) {
            for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {
                if (getVarElem(varIndex).name.equals(getTagGroup(mincTag))) {
                    for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {
                        if (getVarElem(varIndex).getVattElem(attIndex).name.equals(getTagElem(mincTag))) {
                            data = new String("");
                            
                            if (getVarElem(varIndex).getVattElem(attIndex).nc_type == FileInfoMinc.NC_CHAR) {
                                for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                    data += getVarElem(varIndex).getVattElem(attIndex).values[i];
                                }
                            }
                            else {
                                for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                    data += "" + getVarElem(varIndex).getVattElem(attIndex).values[i] + " ";
                                }
                            }
                            
                            return data;
                        }
                    }
                }
            }
        }
        
        // didn't find the tag
        return null;
    }
}
