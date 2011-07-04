package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.Hashtable;


/**
 * This class holds all the file information for a MINC file. In addition, it has three classes to organize the data.
 * 
 * <P>
 * MINC headers have three main parts: the dimensions, the global attributes, and the variables. There are typically
 * three dimensions, representing the x, y, and z dimensions of the image. There is usually one global attribute, the
 * history. There can be any number of variables.
 * </P>
 * 
 * <P>
 * Variables have within them any number of attributes. So for example, "rootvariable", the first variable in all the
 * MINC files I have seen so far, has the following attributes: varid (= "MINC standard variable"), vartype (=
 * "group________"), version (= "MINC version 1.0"), parent (blank), and children (image). Usually there is a variable
 * for each dimension, a variable for the image, a variable for image min, and a variable for image max. There can also
 * be variables containing patient information or anything else useful.
 * </P>
 * 
 * <P>
 * See the documentation for the classes MincDimElem, MincAttElem, and MincVarElem for further information. The classes
 * are structured exactly like a MINC file.
 * </P>
 * 
 * @version 1.0 July 1, 2000
 * @see FileMinc
 */
public class FileInfoMinc extends FileInfoBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6804935230803731941L;

    /** NetCDF defined data type - byte. */
    public static final int NC_BYTE = 1;

    /** NetCDF defined data type - character. */
    public static final int NC_CHAR = 2;

    /** NetCDF defined data type - short. */
    public static final int NC_SHORT = 3;

    /** NetCDF defined data type - integer. */
    public static final int NC_INT = 4;

    /** NetCDF defined data type - float. */
    public static final int NC_FLOAT = 5;

    /** NetCDF defined data type - double. */
    public static final int NC_DOUBLE = 6;

    /** NetCDF defined data type - dimension. */
    public static final int NC_DIMENSION = 10;

    /** NetCDF defined data type - variable. */
    public static final int NC_VARIABLE = 11;

    /** NetCDF defined data type - attribute. */
    public static final int NC_ATTRIBUTE = 12;

    /**
     * Minc var equivalents to anonymizable Dicom tags. Must be in the same order as FileInfoDicom.anonymizeTagIDs.
     * 
     * @see FileInfoDicom#anonymizeTagIDs
     */
    public static final String[] dicomToMincVarMap = {"patient,full_name", // patient name
            "patient,identification", // patient ID
            "patient,birthdate", // patient's birth date
            null, // patient's birth time
            "patient,sex", // patient's sex
            null, // patient Insurance plan code sequence
            null, // other patient IDs
            null, // other patient names
            null, // patient's birth name
            "patient,age", // patient's age
            null, // patient's size
            null, // patient's weight
            null, // patient's address
            null, // patient's mother's birth name
            null, // medical record locator
            null, // patient's telephone numbers
            null, // patient ethnic group
            null, // occupation
            null, // additional patient's history
            null, // patient's last menstrual date
            null, // patient religious preference
            null, // patient comments

            null, // instance creator UID
            null, // SOP instance UID
            null, // accession number
            "study,institution", // institution name
            "study,institution", // institution address
            null, // referring physician's name
            null, // referring physician's address
            null, // referring physician's telephone numbers
            null, // station name
            null, // study description
            null, // series description
            null, // institutional department name
            null, // physician(s) of record
            null, // performing physician's name
            null, // name of physician reading study
            null, // operator's name
            null, // admitting diagnoses description
            null, // Referenced SOP instance UID
            null, // derivation description
            null, // device serial number
            null, // protocol name
            null, // study instance UID
            "study,acquisition_id", // series instance UID
            "study,study_id", // study ID
            null, // frame of reference UID
            null, // synchronization frame of reference UID
            null, // image comments

            // null,// request attributes sequence
            null, // UID
            null, // content sequence
            null, // storage media file-set UID
            null, // referenced frame of reference UID
            null, // related frame of reference UID
    };

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public int numrecs;

    /** Valid image maximum value. Default = -1. */
    public double vmax = -1.0;

    /** Valid image minimum value. Default = -1. */
    public double vmin = -1.0;

    /** DOCUMENT ME! */
    private FileMincDimElem[] dimArray;

    /** DOCUMENT ME! */
    private FileMincAttElem[] gattArray;

    /** DOCUMENT ME! */
    private FileMincVarElem[] varArray;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * MINC file information constructor.
     * 
     * @param name file name
     * @param directory file directory
     * @param format format (in this case, MINC)
     */
    public FileInfoMinc(final String name, final String directory, final int format) {
        super(name, directory, format);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Sets the value of the attribute element; used for setting attribute of variables. A MincVarElem has an array of
     * attributes and this method is called on those attributes.
     * 
     * @param elem the attribute element to add the value to.
     * @param value the value to set the element to.
     * @param index the index into the value array to set this value at.
     * 
     * @see FileMincAttElem#setValue
     */
    public void addAttValue(final FileMincAttElem elem, final Object value, final int index) {
        elem.setValue(value, index);
    }

    /**
     * Puts a new dimension element with the specified parameters into the dimension array at the gievn index. The name
     * is usually xspace, yspace, or zspace; the length is the corresponding size of that dimension.
     * 
     * @param name the name of the dimension element
     * @param length the length of the dimension element
     * @param index the index to put this element into the array
     */
    public void addDimElem(final String name, final int length, final int index) {
        final FileMincDimElem elem = new FileMincDimElem(name, length);
        dimArray[index] = elem;
    }

    /**
     * Puts a new global attribute array element with the specified parameters into the global attribute array at the
     * given index. Usually there is only one element in the global attribute array: the history.
     * 
     * @param name the name of the global attribute element
     * @param type the data type, i.e. byte, char, int, etc (see statically defined types)
     * @param length the length of the element
     * @param index the index into the array to put this element at
     */
    public void addGattElem(final String name, final int type, final int length, final int index) {
        final FileMincAttElem elem = new FileMincAttElem(name, type, length);
        gattArray[index] = elem;
    }

    /**
     * Puts a new variable element with the specified paramenters into the array at the given index. There are other
     * attributes to set for variables, in particular the attribute array, but that gets set in a different place.
     * 
     * @param name the name of this variable element
     * @param nelems the number of elements in the attribute array
     * @param dimid the dimension id array, usually empty but for "image" containing information about the dimensions
     * @param index index into the variable array to put this variable
     * 
     * @see FileMincVarElem
     */
    public void addVarElem(final String name, final int nelems, final int[] dimid, final int index) {
        final FileMincVarElem elem = new FileMincVarElem(name, nelems, dimid);
        varArray[index] = elem;
    }

    /**
     * Removes requested identifying info. Can remove info stored in the minc header in a dicom-like structure and in a
     * more minc-like info structure.
     * 
     * @param list the list of tags to remove; it MUST correspond to anonymizeTagIDs list (or spurious errors result),
     *            and it must be of the same length, or this will throw an IllegalArgumentException.
     * 
     * @throws IllegalArgumentException DOCUMENT ME!
     */
    public final void anonymize(final boolean[] list) {

        if (list.length != FileInfoDicom.anonymizeTagIDs.length) {
            throw new IllegalArgumentException("Anonymize list not of correct size!");
        }

        // DICOM type 2 fields. Existance is required, although value is not.
        for (int i = 0; i < 2; i++) {

            try {

                if (list[i]) {

                    for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {

                        // System.out.println("looking for " + "dicom_0x" +
                        // getDicomTag(FileInfoDicom.anonymizeTagIDs[i])); System.out.println("found " +
                        // getVarElem(varIndex).name);
                        if (getVarElem(varIndex).name.equals("dicom_0x"
                                + FileInfoMinc.getTagGroup(FileInfoDicom.anonymizeTagIDs[i]))) {

                            for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {

                                // System.out.println("looking for " + "el_0x" +
                                // getDicomElem(FileInfoDicom.anonymizeTagIDs[i])); System.out.println("found " +
                                // getVarElem(varIndex).getVattElem(attIndex).name);
                                if (getVarElem(varIndex).getVattElem(attIndex).name.equals("el_0x"
                                        + FileInfoMinc.getTagElem(FileInfoDicom.anonymizeTagIDs[i]))) {

                                    // System.out.println("want to erase " + FileInfoDicom.anonymizeTagIDs[i] + " -- "
                                    // + getVarElem(varIndex).getVattElem(attIndex).toString());
                                    getVarElem(varIndex).getVattElem(attIndex).nelems = 1;
                                    getVarElem(varIndex).getVattElem(attIndex).values = new Object[] {new Character(' ')};
                                    // System.out.println("value now " + FileInfoDicom.anonymizeTagIDs[i] + " -- " +
                                    // getVarElem(varIndex).getVattElem(attIndex).toString());
                                }
                            }
                        }
                    }
                }
            } catch (final NullPointerException npe) {

                // an IllegalArgumentException is probably not right here....
                throw new IllegalArgumentException("(" + FileInfoDicom.anonymizeTagIDs[i]
                        + ") is a required type 2 tag.");
            }
        }

        // all other fields to anonymize are DICOM type 3 fields and
        // are neither required to have an entry nor required to exist
        // in the image info.
        for (int i = 2; i < FileInfoDicom.anonymizeTagIDs.length; i++) {
            // change each of the following tags to (empty) if we are asked to anonymize this info and if the tag exists
            // in the hashtable.

            if (list[i]) {

                for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {

                    // System.out.println("looking for " + "dicom_0x" + getDicomTag(FileInfoDicom.anonymizeTagIDs[i]));
                    // System.out.println("found " + getVarElem(varIndex).name);
                    if (getVarElem(varIndex).name.equals("dicom_0x"
                            + FileInfoMinc.getTagGroup(FileInfoDicom.anonymizeTagIDs[i]))) {

                        for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {

                            // System.out.println("looking for " + "el_0x" +
                            // getDicomElem(FileInfoDicom.anonymizeTagIDs[i])); System.out.println("found " +
                            // getVarElem(varIndex).getVattElem(attIndex).name);
                            if (getVarElem(varIndex).getVattElem(attIndex).name.equals("el_0x"
                                    + FileInfoMinc.getTagElem(FileInfoDicom.anonymizeTagIDs[i]))) {

                                // System.out.println("want to erase " + FileInfoDicom.anonymizeTagIDs[i] + " -- " +
                                // getVarElem(varIndex).getVattElem(attIndex).toString());
                                getVarElem(varIndex).getVattElem(attIndex).nelems = 0;
                                getVarElem(varIndex).getVattElem(attIndex).values = new Object[] {};
                                // System.out.println("value now " + FileInfoDicom.anonymizeTagIDs[i] + " -- " +
                                // getVarElem(varIndex).getVattElem(attIndex).toString());
                            }
                        }
                    }
                }
            }
        }

        // *** anonymize fields which may be saved directly in the minc var format (but not converted from dicom tags)
        for (int i = 0; i < FileInfoDicom.anonymizeTagIDs.length; i++) {

            if (list[i] && (FileInfoMinc.dicomToMincVarMap[i] != null)) {

                for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {

                    // System.out.println("looking for " + getTagGroup(FileInfoMinc.dicomToMincVarMap[i]));
                    // System.out.println("found " + getVarElem(varIndex).name);
                    if (getVarElem(varIndex).name.equals(FileInfoMinc.getTagGroup(FileInfoMinc.dicomToMincVarMap[i]))) {

                        for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {

                            // System.out.println("looking for " + getTagElem(FileInfoMinc.dicomToMincVarMap[i]));
                            // System.out.println("found " + getVarElem(varIndex).getVattElem(attIndex).name);
                            if (getVarElem(varIndex).getVattElem(attIndex).name.equals(FileInfoMinc
                                    .getTagElem(FileInfoMinc.dicomToMincVarMap[i]))) {

                                // System.out.println("want to erase " + FileInfoMinc.dicomToMincVarMap + " -- " +
                                // getVarElem(varIndex).getVattElem(attIndex).toString());
                                getVarElem(varIndex).getVattElem(attIndex).nelems = 0;
                                getVarElem(varIndex).getVattElem(attIndex).values = new Object[] {};
                                // System.out.println("value now " + FileInfoMinc.dicomToMincVarMap + " -- " +
                                // getVarElem(varIndex).getVattElem(attIndex).toString());
                            }
                        }
                    }
                }
            }
        }

        // this fileInfo is now an expurgated/sanitised version
    }

    /**
     * Calculates rescale intercept given a min and a slope value.
     * 
     * @param min Min value.
     * @param slope Slope value.
     * 
     * @return Rescale intercept.
     */
    public static final double calculateIntercept(final double min, final double slope, final double validMin) {
        return (min - (slope * validMin));
    }

    /**
     * In MINC images, "real" values for pixels are calculated by taking the given image min and image max and rescaling
     * the data accordingly. Image min and image max are given per slice.
     * 
     * @param rescaleIntercept Array of rescale intercepts to set
     * @param rescaleSlope Array of rescale slopes to set
     */
    public void calculateRescaleIntercept(final double[] rescaleIntercept, final double[] rescaleSlope) {
        int min_index = -1, max_index = -1;

        for (int i = 0; i < varArray.length; i++) {

            if (varArray[i].name.equals("image-min")) {
                min_index = i;
            }

            if (varArray[i].name.equals("image-max")) {
                max_index = i;
            }
        }

        try {

            for (int i = 0; i < rescaleSlope.length; i++) {
                rescaleSlope[i] = FileInfoMinc.calculateSlope( ((Double) varArray[max_index].values.elementAt(i))
                        .doubleValue(), ((Double) varArray[min_index].values.elementAt(i)).doubleValue(), vmax, vmin);
                rescaleIntercept[i] = FileInfoMinc.calculateIntercept( ((Double) varArray[min_index].values
                        .elementAt(i)).doubleValue(), rescaleSlope[i], vmin);
            }
        } catch (final ArrayIndexOutOfBoundsException error) {

            for (int i = 0; i < rescaleSlope.length; i++) {
                rescaleSlope[i] = 1.0;
                rescaleIntercept[i] = 0.0;
            }
        }
    }

    /**
     * Calculates rescale slope given a min and a max value.
     * 
     * @param max Max value.
     * @param min Min value.
     * 
     * @return Rescale slope
     */
    public static final double calculateSlope(final double max, final double min, final double validMax,
            final double validMin) {

        if ( (validMax - validMin) != 0) {
            return (max - min) / (validMax - validMin);
        }

        return 1.0;
    }

    /**
     * Returns all of the dicom-converted tags in the minc header as a tag-value hashtable.
     * 
     * @return a tag-value hashtable
     */
    public Hashtable<String, String> convertTagsToTable() {
        final Hashtable<String, String> table = new Hashtable<String, String>();

        final String groupPrefix = "dicom_0x";
        final String elemPrefix = "el_0x";

        String group, elem, data;
        VR vr;
        int index;

        for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {
            index = getVarElem(varIndex).name.indexOf(groupPrefix);

            if (index != -1) {
                group = getVarElem(varIndex).name.substring(groupPrefix.length());

                for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {
                    index = getVarElem(varIndex).getVattElem(attIndex).name.indexOf(elemPrefix);

                    if (index != -1) {
                        elem = getVarElem(varIndex).getVattElem(attIndex).name.substring(elemPrefix.length());
                        vr = DicomDictionary.getVR(new FileDicomKey(group + "," + elem));
                        if (vr != null && vr.equals(VR.SQ)) {
                            continue;
                        }

                        data = new String("");

                        if (getVarElem(varIndex).getVattElem(attIndex).nc_type == FileInfoMinc.NC_CHAR) {

                            for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                data += getVarElem(varIndex).getVattElem(attIndex).values[i];
                            }
                        } else {

                            for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                data += "" + getVarElem(varIndex).getVattElem(attIndex).values[i] + " ";
                            }
                        }

                        table.put("(" + group.toUpperCase() + "," + elem.toUpperCase() + ")", data.trim());
                    }
                }
            }
        }

        return table;
    }

    /**
     * Constructs the dimension array with the specified length.
     * 
     * @param length the length of the array
     */
    public final void createDimArray(final int length) {
        dimArray = new FileMincDimElem[length];
    }

    /**
     * Constructs the global attribute array with the specified length.
     * 
     * @param length the length of the array
     */
    public final void createGattArray(final int length) {
        gattArray = new FileMincAttElem[length];
    }

    /**
     * Constructs the variable array with the specified length.
     * 
     * @param length the length of the array
     */
    public final void createVarArray(final int length) {
        varArray = new FileMincVarElem[length];
    }

    /**
     * Displays important information about the image.
     * 
     * @param dlog where to display the info
     * @param matrix the transformation matrix
     */
    public void displayAboutInfo(final JDialogBase dlog, final TransMatrix matrix) {
        final JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");
        dialog.append("Dimension information:\n");

        for (final FileMincDimElem element : dimArray) {
            dialog.append(element.toString());
        }

        dialog.append("\nGlobal Attribute information:\n");

        if (gattArray != null) {

            for (final FileMincAttElem element : gattArray) {
                dialog.append(element.toString());
            }
        }

        dialog.append("\n\nVariable information:\n");

        for (final FileMincVarElem element : varArray) {
            dialog.append(element.toString());
        }
    }

    /**
     * Accessor that gets the "start" variable values, adjusted so that [0] holds the image x axis value, [1] the image
     * y axis value, and [2] the image z axis value.
     * 
     * <p>
     * MINC positive axis is left to right; positive axis is posterior to anterior; postive axis is inferior to superior
     * </p>
     * 
     * <p>
     * DICOM positive axis is right to left; positive axis is anterior to posterior; postive axis is inferior to
     * superior
     * </p>
     * 
     * <p>
     * If the space's alignment attribute has the value 'centre', then subtract half the space's step value (doesn't
     * seem to apply to zspace for some reason..). Then transform the point by the inverse of the direction_cosines
     * matrix (extracted from the spaces). The result then has some of the signs of its components flipped (which is
     * determined by the image orientation) to get it from minc to dicom orientation.
     * </p>
     * 
     * @param slice slice to begin the start variable on.
     * 
     * @return The slice position in dicom (and mipav) space.
     */
    public final double[] getConvertStartLocationsToDICOM(final int slice) {
        double x = 0;
        double y = 0;
        double z = 0;
        double xStep = 1;
        double yStep = 1;
        double zStep = 1;

        boolean isXCentered = false;
        boolean isYCentered = false;
        @SuppressWarnings("unused")
        boolean isZCentered = false;

        String spacex, spacey, spacez;

        spacex = getDimElem(2).name;
        spacey = getDimElem(1).name;
        spacez = getDimElem(0).name;

        for (final FileMincVarElem element : varArray) {

            if (element.name.equals(spacex)) {
                x = element.start;
                xStep = element.step;

                for (final FileMincAttElem element2 : element.vattArray) {

                    if (element2.name.equals("alignment")) {
                        isXCentered = element2.getValueString().equals("centre");
                    }
                }
            }

            if (element.name.equals(spacey)) {
                y = element.start;
                yStep = element.step;

                for (final FileMincAttElem element2 : element.vattArray) {

                    if (element2.name.equals("alignment")) {
                        isYCentered = element2.getValueString().equals("centre");
                    }
                }
            }

            if (element.name.equals(spacez)) {
                z = element.start;
                zStep = element.step;

                for (final FileMincAttElem element2 : element.vattArray) {

                    if (element2.name.equals("alignment")) {
                        isZCentered = element2.getValueString().equals("centre");
                    }
                }
            }
        }

        // System.out.println("convert: begin res:\t" + xRes + " " + yRes + " " + zRes);

        final double[] startLocs = new double[getExtents().length];

        if (startLocs.length == 2) {
            startLocs[0] = x;
            startLocs[1] = y;
        } else {
            startLocs[0] = x;
            startLocs[1] = y;
            startLocs[2] = z + (zStep * slice);
        }

        // System.out.println("convert: locs:\t" + startLocs[0] + " " + startLocs[1] + " " + startLocs[2]);
        final TransMatrix matrix = new TransMatrix(Math.min(4, getExtents().length + 1));
        matrix.MakeIdentity();

        for (final FileMincVarElem element : varArray) {

            if (element.name.equals("xspace")) {

                if (element.cosines != null) {

                    for (int j = 0; j < element.cosines.length; j++) {
                        matrix.set(0, j, element.cosines[j]);
                    }
                }
            } else if (element.name.equals("yspace")) {

                if (element.cosines != null) {

                    for (int j = 0; j < element.cosines.length; j++) {
                        matrix.set(1, j, element.cosines[j]);
                    }
                }
            } else if (element.name.equals("zspace")) {

                if (element.cosines != null) {

                    for (int j = 0; j < element.cosines.length; j++) {
                        matrix.set(2, j, element.cosines[j]);
                    }
                }
            }
        }

        // System.out.println("convert: matrix:\t" + matrix.matrixToString(24, 16));

        matrix.Inverse();

        // System.out.println("convert: invmat:\t" + matrix.matrixToString(24, 16));

        final double[] transformedPt = new double[getExtents().length];

        if (isXCentered) {
            startLocs[0] -= (xStep / 2);
        }

        if (isYCentered) {
            startLocs[1] -= (yStep / 2);
        }

        // mni seems not to adjust the zstart by the zstep even when xspace has the attrib alignment=centre
        /*
         * if (isZCentered) { startLocs[2] -= (zStep / 2);}
         */

        if (getExtents().length == 2) {
            matrix.transform(startLocs[0], startLocs[1], transformedPt);
        } else if (getExtents().length == 3) {
            matrix.transform(startLocs[0], startLocs[1], startLocs[2], transformedPt);
        }

        // System.out.println("convert: trans:\t" + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2]);

        if (startLocs.length == 3) {

            if (getImageOrientation() == FileInfoBase.SAGITTAL) {
                transformedPt[0] = -transformedPt[0];
                transformedPt[2] = -transformedPt[2];
            } else if (getImageOrientation() == FileInfoBase.AXIAL) {
                transformedPt[0] = -transformedPt[0];
                transformedPt[1] = -transformedPt[1];
            } else if (getImageOrientation() == FileInfoBase.CORONAL) {
                transformedPt[0] = -transformedPt[0];
                transformedPt[2] = -transformedPt[2];
            }
        }

        // System.out.println("convert: result:\t" + transformedPt[0] + " " + transformedPt[1] + " " +
        // transformedPt[2]);

        return transformedPt;
    }

    /**
     * Get the value from the minc info based on a given dicom tag id.
     * 
     * @param dicomTag dicom tag (group,elem)
     * 
     * @return the value stored in the info (or null if it doesn't exist)
     */
    public String getDicomValue(final String dicomTag) {
        String data;
        final String groupPrefix = "dicom_0x";
        final String elemPrefix = "el_0x";

        // look for the dicom tag
        for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {

            if (getVarElem(varIndex).name.equals(groupPrefix + FileInfoMinc.getTagGroup(dicomTag))) {

                for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {

                    if (getVarElem(varIndex).getVattElem(attIndex).name.equals(elemPrefix
                            + FileInfoMinc.getTagElem(dicomTag))) {
                        data = new String("");

                        if (getVarElem(varIndex).getVattElem(attIndex).nc_type == FileInfoMinc.NC_CHAR) {

                            for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                data += getVarElem(varIndex).getVattElem(attIndex).values[i];
                            }
                        } else {

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
                mincTag = FileInfoMinc.dicomToMincVarMap[i];
            }
        }

        // get the minc equivalent
        if (mincTag != null) {

            for (int varIndex = 0; varIndex < getVarArray().length; varIndex++) {

                if (getVarElem(varIndex).name.equals(FileInfoMinc.getTagGroup(mincTag))) {

                    for (int attIndex = 0; attIndex < getVarElem(varIndex).vattArray.length; attIndex++) {

                        if (getVarElem(varIndex).getVattElem(attIndex).name.equals(FileInfoMinc.getTagElem(mincTag))) {
                            data = new String("");

                            if (getVarElem(varIndex).getVattElem(attIndex).nc_type == FileInfoMinc.NC_CHAR) {

                                for (int i = 0; i < getVarElem(varIndex).getVattElem(attIndex).nelems; i++) {
                                    data += getVarElem(varIndex).getVattElem(attIndex).values[i];
                                }
                            } else {

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

    /**
     * Gets the array with the dimension information.
     * 
     * @return the array
     */
    public final FileMincDimElem[] getDimArray() {
        return dimArray;
    }

    /**
     * Accessor that returns the dimension element at the indicated index in the dimension array.
     * 
     * @param index index where the dimension element is located
     * 
     * @return dimArray[index]
     */
    public final FileMincDimElem getDimElem(final int index) {
        return dimArray[index];
    }

    /**
     * Gets the array with the attribute information.
     * 
     * @return the array
     */
    public final FileMincAttElem[] getGattArray() {
        return gattArray;
    }

    /**
     * Accessor that returns the global attribute element at the indicated index in the global attribute array.
     * 
     * @param index index where the global attribute element is located
     * 
     * @return gattArray[index]
     */
    public final FileMincAttElem getGattElem(final int index) {
        return gattArray[index];
    }

    /**
     * Accessor that gets the "start" variable values, with the "xspace" in 0, "yspace" in 1, and "zspace" in 2. This
     * differs from the other methods because it doesn't place the values so that they correspond to image x, y, and z.
     * Gets values as stored in MINC header, not modified to account for the different origin.
     * 
     * @return The "start" values for the image.
     */
    public final float[] getStartLocations() {
        double x = 0;
        double y = 0;
        double z = 0;

        for (final FileMincVarElem element : varArray) {

            if (element.name.equals("xspace")) {
                x = element.trueStart;
            }

            if (element.name.equals("yspace")) {
                y = element.trueStart;
            }

            if (element.name.equals("zspace")) {
                z = element.trueStart;
            }
        }

        final float[] start = new float[getExtents().length];

        if (start.length == 2) {
            start[0] = (float) x;
            start[1] = (float) y;
        } else if (start.length == 3) {
            start[0] = (float) x;
            start[1] = (float) y;
            start[2] = (float) z;
        }

        return start;
    }

    /**
     * Gets the array with the variable information.
     * 
     * @return the array
     */
    public final FileMincVarElem[] getVarArray() {
        return varArray;
    }

    /**
     * Accessor that returns the variable element at the indicated index in the variable array.
     * 
     * @param index index where the variable element is located
     * 
     * @return varArray[index]
     */
    public final FileMincVarElem getVarElem(final int index) {
        return varArray[index];
    }

    /**
     * Sets necessary image information.
     */
    public final void setImportantImageInfo() {
        int ix = 0, iy = 0, iz = 0;

        final String firstDim = getDimElem(0).name;
        Preferences.debug("firstDim = " + firstDim + "\n", Preferences.DEBUG_FILEIO);

        final String secondDim = getDimElem(1).name;
        Preferences.debug("secondDim = " + secondDim + "\n", Preferences.DEBUG_FILEIO);

        final String thirdDim = getDimElem(2).name;
        Preferences.debug("thirdDim = " + thirdDim + "\n", Preferences.DEBUG_FILEIO);

        for (int i = 0; i < varArray.length; i++) {

            if (varArray[i].name.equals("image")) {
                setOffset(varArray[i].begin);
                Preferences.debug("Image offset = " + getOffset() + "\n", Preferences.DEBUG_FILEIO);

                switch (varArray[i].nc_type) {

                    case NC_BYTE:
                        if (varArray[i].signtype.equals("unsigned")) {
                            Preferences.debug("Data type = UBYTE\n", Preferences.DEBUG_FILEIO);
                            setDataType(ModelStorageBase.UBYTE);
                        } else {
                            Preferences.debug("Data type = BYTE\n", Preferences.DEBUG_FILEIO);
                            setDataType(ModelStorageBase.BYTE);
                        }

                        break;

                    case NC_SHORT:
                        if (varArray[i].signtype.equals("unsigned")) {
                            Preferences.debug("Data type = USHORT\n", Preferences.DEBUG_FILEIO);
                            setDataType(ModelStorageBase.USHORT);
                        } else {
                            Preferences.debug("Data type = SHORT\n", Preferences.DEBUG_FILEIO);
                            setDataType(ModelStorageBase.SHORT);
                        }

                        break;

                    case NC_INT:
                        if (varArray[i].signtype.equals("unsigned")) {
                            Preferences.debug("Data type = UINTEGER\n", Preferences.DEBUG_FILEIO);
                            setDataType(ModelStorageBase.UINTEGER);
                        } else {
                            Preferences.debug("Data type = INTEGER\n", Preferences.DEBUG_FILEIO);
                            setDataType(ModelStorageBase.INTEGER);
                        }

                        break;

                    case NC_FLOAT:
                        Preferences.debug("Data type = FLOAT\n", Preferences.DEBUG_FILEIO);
                        setDataType(ModelStorageBase.FLOAT);
                        break;

                    case NC_DOUBLE:
                        Preferences.debug("Data type = DOUBLE\n", Preferences.DEBUG_FILEIO);
                        setDataType(ModelStorageBase.DOUBLE);
                        break;

                    default:
                        Preferences.debug("varArray[" + i + "].nc_type illegally = " + varArray[i].nc_type + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        MipavUtil.displayError("Invalid type in FileInfoMinc");
                }

                for (final FileMincAttElem elem : varArray[i].vattArray) {
                    if (elem.name.equals("valid_range")) {

                        switch (elem.nc_type) {

                            case NC_BYTE:
                                vmin = ((Byte) elem.values[0]).byteValue();
                                vmax = ((Byte) elem.values[1]).byteValue();
                                break;

                            case NC_CHAR:
                                vmin = ((Character) elem.values[0]).charValue();
                                vmax = ((Character) elem.values[1]).charValue();
                                break;

                            case NC_SHORT:
                                vmin = ((Short) elem.values[0]).shortValue();
                                vmax = ((Short) elem.values[1]).shortValue();
                                break;

                            case NC_INT:
                                vmin = ((Integer) elem.values[0]).intValue();
                                vmax = ((Integer) elem.values[1]).intValue();
                                break;

                            case NC_FLOAT:
                                vmin = ((Float) elem.values[0]).floatValue();
                                vmax = ((Float) elem.values[1]).floatValue();
                                break;

                            case NC_DOUBLE:
                                vmin = ((Double) elem.values[0]).doubleValue();
                                vmax = ((Double) elem.values[1]).doubleValue();
                        }

                        Preferences.debug("vmin = " + vmin + "\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("vmax = " + vmax + "\n", Preferences.DEBUG_FILEIO);
                    } else if (elem.name.equals("valid_max")) {

                        switch (elem.nc_type) {

                            case NC_BYTE:
                                vmax = ((Byte) elem.values[0]).byteValue();
                                break;

                            case NC_CHAR:
                                vmax = ((Character) elem.values[0]).charValue();
                                break;

                            case NC_SHORT:
                                vmax = ((Short) elem.values[0]).shortValue();
                                break;

                            case NC_INT:
                                vmax = ((Integer) elem.values[0]).intValue();
                                break;

                            case NC_FLOAT:
                                vmax = ((Float) elem.values[0]).floatValue();
                                break;

                            case NC_DOUBLE:
                                vmax = ((Double) elem.values[0]).doubleValue();
                        }

                        Preferences.debug("vmax = " + vmax + "\n", Preferences.DEBUG_FILEIO);
                    } else if (elem.name.equals("valid_min")) {

                        switch (elem.nc_type) {

                            case NC_BYTE:
                                vmin = ((Byte) elem.values[0]).byteValue();
                                break;

                            case NC_CHAR:
                                vmin = ((Character) elem.values[0]).charValue();
                                break;

                            case NC_SHORT:
                                vmin = ((Short) elem.values[0]).shortValue();
                                break;

                            case NC_INT:
                                vmin = ((Integer) elem.values[0]).intValue();
                                break;

                            case NC_FLOAT:
                                vmin = ((Float) elem.values[0]).floatValue();
                                break;

                            case NC_DOUBLE:
                                vmin = ((Double) elem.values[0]).doubleValue();
                        }

                        Preferences.debug("vmin = " + vmin + "\n", Preferences.DEBUG_FILEIO);
                    }
                }
            } else if (varArray[i].name.equals(thirdDim)) {
                axisOrientation[0] = FileInfoMinc.setOrientType(thirdDim, (varArray[i].step > 0));
                ix = i;
            } else if (varArray[i].name.equals(secondDim)) {
                axisOrientation[1] = FileInfoMinc.setOrientType(secondDim, (varArray[i].step > 0));
                iy = i;
            } else if (varArray[i].name.equals(firstDim)) {
                axisOrientation[2] = FileInfoMinc.setOrientType(firstDim, (varArray[i].step > 0));
                iz = i;
            }
        }

        if ( (varArray[ix].cosines != null) && (varArray[iy].cosines != null) && (varArray[iz].cosines != null)) {
            final TransMatrix mat = new TransMatrix(3);
            mat.set(0, 0, varArray[ix].cosines[0]);
            mat.set(1, 0, varArray[ix].cosines[1]);
            mat.set(2, 0, varArray[ix].cosines[2]);
            mat.set(0, 1, varArray[iy].cosines[0]);
            mat.set(1, 1, varArray[iy].cosines[1]);
            mat.set(2, 1, varArray[iy].cosines[2]);
            mat.set(0, 2, varArray[iz].cosines[0]);
            mat.set(1, 2, varArray[iz].cosines[1]);
            mat.set(2, 2, varArray[iz].cosines[2]);
            axisOrientation = FileInfoMinc.getAxisOrientation(mat);

            if (varArray[ix].step < 0) {
                axisOrientation[0] = FileInfoBase.oppositeOrient(axisOrientation[0]);
            }

            if (varArray[iy].step < 0) {
                axisOrientation[1] = FileInfoBase.oppositeOrient(axisOrientation[1]);
            }

            if (varArray[iz].step < 0) {
                axisOrientation[2] = FileInfoBase.oppositeOrient(axisOrientation[2]);
            }
        }

        for (int i = 0; i < axisOrientation.length; i++) {

            switch (axisOrientation[i]) {

                case ORI_UNKNOWN_TYPE:
                    Preferences.debug("axisOrientation[" + i + "] = ORI_UNKNOWN_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case ORI_R2L_TYPE:
                    Preferences.debug("axisOrientation[" + i + "] = ORI_R2L_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case ORI_L2R_TYPE:
                    Preferences.debug("axisOrientation[" + i + "] = ORI_L2R_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case ORI_P2A_TYPE:
                    Preferences.debug("axisOrientation[" + i + "] = ORI_P2A_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case ORI_A2P_TYPE:
                    Preferences.debug("axisOrientation[" + i + "] = ORI_A2P_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case ORI_I2S_TYPE:
                    Preferences.debug("axisOrientation[" + i + "] = ORI_I2S_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case ORI_S2I_TYPE:
                    Preferences.debug("axisOrientation[" + i + "] = ORI_S2I_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;
            }
        }
    }

    /**
     * Sets the image modality based on the.
     */
    public void setModality() {

        for (final FileMincVarElem element : varArray) {

            if (element.name.equals("study")) {

                for (final FileMincAttElem element2 : element.vattArray) {

                    if (element2.name.equals("modality")) {
                        final String modality = element2.getValueString();

                        if (modality.equals("PET__")) {
                            setModality(FileInfoBase.POSITRON_EMISSION_TOMOGRAPHY);
                        } else if (modality.equals("MRI__")) {
                            setModality(FileInfoBase.MAGNETIC_RESONANCE);
                        } else if (modality.equals("SPECT")) {
                            setModality(FileInfoBase.SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY);
                        } else if (modality.equals("GAMMA")) {
                            // setModality(FileInfoBase.);
                        } else if (modality.equals("MRS__")) {
                            setModality(FileInfoBase.MAGNETIC_RESONANCE_SPECTROSCOPY);
                        } else if (modality.equals("MRA__")) {
                            setModality(FileInfoBase.MAGNETIC_RESONANCE_ANGIOGRAPHY);
                        } else if (modality.equals("CT___")) {
                            setModality(FileInfoBase.COMPUTED_TOMOGRAPHY);
                        } else if (modality.equals("DSA__")) {
                            // setModality(FileInfoBase.);
                        } else if (modality.equals("DR___")) {
                            setModality(FileInfoBase.DIGITAL_RADIOGRAPHY);
                        }
                    }
                }
            }
        }
    }

    /**
     * Sets the resolutions from the variable array based on the orientation of the image. The "zspace" in MINC refers
     * to the inferior-superior axis, whereas in MIPAV the z resolution variable is the slices (space between slices)
     * regardless of the orientation. Therefore for proper display we must convert between them.
     * 
     * @param orientation The orientation of the image (sagittal, coronal, or axial).
     */
    public void setResolutions(final int orientation) {
        double xStep = 1.0;
        double yStep = 1.0;
        double zStep = 1.0;
        final double thickness = getMincSliceThickness();

        for (final FileMincVarElem element : varArray) {

            if (element.name.equals("xspace")) {
                xStep = element.step;
            }

            if (element.name.equals("yspace")) {
                yStep = element.step;
            }

            if (element.name.equals("zspace")) {
                zStep = element.step;
            }
        }

        setSliceThickness(Math.abs((float) thickness));

        if (getExtents().length == 2) {
            setResolutions(Math.abs((float) xStep), 0);
            setResolutions(Math.abs((float) yStep), 1);
        } else if (getExtents().length == 3) {

            switch (orientation) {

                case SAGITTAL:
                    setResolutions(Math.abs((float) xStep), 2);
                    setResolutions(Math.abs((float) yStep), 0);
                    setResolutions(Math.abs((float) zStep), 1);
                    break;

                case CORONAL:
                    setResolutions(Math.abs((float) xStep), 0);
                    setResolutions(Math.abs((float) yStep), 2);
                    setResolutions(Math.abs((float) zStep), 1);
                    break;

                case AXIAL:
                default:
                    setResolutions(Math.abs((float) xStep), 0);
                    setResolutions(Math.abs((float) yStep), 1);
                    setResolutions(Math.abs((float) zStep), 2);
                    break;
            }
        }
    }

    /**
     * Sets start locations of each axis.
     * 
     * @param origin the image origin
     */
    public final void setStartLocations(final double[] origin) {

        if (origin.length != 3) {
            Preferences.debug("Start locations array must be of length 3.\n", Preferences.DEBUG_FILEIO);

            return;
        }

        final float[] fOrigin = new float[origin.length];

        for (int i = 0; i < origin.length; i++) {
            setStartLocation(origin[i], i);
            fOrigin[i] = (float) origin[i];
        }

        super.setOrigin(fOrigin);
    }

    /**
     * Sets the units of the dimensions, as in millimeters, inches, etc. Called after the header has been read in so the
     * values within the varArray have been set properly already.
     */
    public final void setUnits() {

        for (final FileMincVarElem element : varArray) {

            if (element.units != null) {

                if (element.name.equals("xspace")) {

                    if (element.units.equals("mm")) {
                        setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                    } else if (element.units.equals("in")) {
                        setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), 0);
                    }
                }

                if (element.name.equals("yspace")) {

                    if (element.units.equals("mm")) {
                        setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                    } else if (element.units.equals("in")) {
                        setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), 1);
                    }
                }

                if (element.name.equals("zspace")) {

                    if (element.units.equals("mm")) {
                        setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
                    } else if (element.units.equals("in")) {
                        setUnitsOfMeasure(Unit.INCHES.getLegacyNum(), 2);
                    }
                }
            }
        }
    }

    /**
     * Extract the element identifier (removing the group id).
     * 
     * @param fullTag the full element identitfier
     * 
     * @return the element id
     */
    private static String getTagElem(final String fullTag) {
        final int index = fullTag.indexOf(",");

        if (index == -1) {
            return null;
        }

        return fullTag.substring(index + 1);
    }

    /**
     * Extract the group to which a dicom or minc element belongs to.
     * 
     * @param fullTag the full element identitfier
     * 
     * @return the element group
     */
    private static String getTagGroup(final String fullTag) {
        final int index = fullTag.indexOf(",");

        if (index == -1) {
            return null;
        }

        return fullTag.substring(0, index);
    }

    /**
     * Return the 3 axis orientation codes that correspond to the closest standard anatomical orientation of the (i,j,k)
     * axes.
     * 
     * @param mat 4x4 matrix that transforms (i,j,k) indexes to x,y,z coordinates where +x = Right, +y = Anterior, +z =
     *            Superior Only the upper-left 3x3 corner of the matrix is used This routine finds the permutation of
     *            (x,y,z) which has the smallest angle to the (i,j,k) axes directions, which are columns of the input
     *            matrix Errors: The codes returned will be zero.
     * 
     * @return codes
     */
    public static int[] getAxisOrientation(final TransMatrix mat) {
        final int[] axisOrientation = new int[3];
        double xi, xj, xk, yi, yj, yk, zi, zj, zk, val;
        double detQ;
        double vbest;
        int ibest, jbest, kbest, pbest, qbest, rbest;
        int i, j, k, p, q, r;
        double detP;

        xi = mat.Get(0, 0);
        xj = mat.Get(0, 1);
        xk = mat.Get(0, 2);
        yi = mat.Get(1, 0);
        yj = mat.Get(1, 1);
        yk = mat.Get(1, 2);
        zi = mat.Get(2, 0);
        zj = mat.Get(2, 1);
        zk = mat.Get(2, 2);

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

        if ( (izero == 2) && (jzero == 2) && (kzero == 2) && (xzero == 2) && (yzero == 2) && (zzero == 2)) {

            if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yk > 0.0) {
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
        val = Math.sqrt( (xi * xi) + (yi * yi) + (zi * zi));

        if (val == 0.0) {
            // MipavUtil.displayError("xi = yi = zi = 0 in getAxisOrientation");
            Preferences.debug("xi = yi = zi = 0 in getAxisOrientation", Preferences.DEBUG_FILEIO);
            return null;
        }

        xi /= val;
        yi /= val;
        zi /= val;

        // Normalize j axis
        val = Math.sqrt( (xj * xj) + (yj * yj) + (zj * zj));

        if (val == 0.0) {
            // MipavUtil.displayError("xj = yj = zj = 0 in getAxisOrientation");
            Preferences.debug("xj = yj = zj = 0 in getAxisOrientation", Preferences.DEBUG_FILEIO);
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
            val = Math.sqrt( (xj * xj) + (yj * yj) + (zj * zj)); // Must renormalize

            if (val == 0.0) {
                // MipavUtil.displayError("j was parallel to i in getAxisOrientation");
                Preferences.debug("j was parallel to i in getAxisOrientation", Preferences.DEBUG_FILEIO);

                return null;
            }

            xj /= val;
            yj /= val;
            zj /= val;
        }

        // Normalize k axis; if it is zero, make it the cross product i x j
        val = Math.sqrt( (xk * xk) + (yk * yk) + (zk * zk));

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
            val = Math.sqrt( (xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                // MipavUtil.displayError("val == 0 when orthogonalizing k to i");
                Preferences.debug("val == 0 when orthogonalizing k to i in getAxisOrientation",
                        Preferences.DEBUG_FILEIO);
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
            val = Math.sqrt( (xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                // MipavUtil.displayError("val == 0 when orthogonalizing k to j");
                Preferences.debug("val == 0 when orthogonalizing k to j in getAxisOrientation",
                        Preferences.DEBUG_FILEIO);
                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        if ( ( (Math.abs(xi) > 0.9) || (Math.abs(yi) > 0.9) || (Math.abs(zi) > 0.9))
                && ( (Math.abs(xj) > 0.9) || (Math.abs(yj) > 0.9) || (Math.abs(zj) > 0.9))
                && ( (Math.abs(xk) > 0.9) || (Math.abs(yk) > 0.9) || (Math.abs(zk) > 0.9))) {

            if (Math.abs(xi) < 0.9) {
                xi = 0;
            }

            if (Math.abs(yi) < 0.9) {
                yi = 0;
            }

            if (Math.abs(zi) < 0.9) {
                zi = 0;
            }

            if (Math.abs(xj) < 0.9) {
                xj = 0;
            }

            if (Math.abs(yj) < 0.9) {
                yj = 0;
            }

            if (Math.abs(zj) < 0.9) {
                zj = 0;
            }

            if (Math.abs(xk) < 0.9) {
                xk = 0;
            }

            if (Math.abs(yk) < 0.9) {
                yk = 0;
            }

            if (Math.abs(zk) < 0.9) {
                zk = 0;
            }

            if (xi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zi > 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zi < 0.0) {
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zj > 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zj < 0.0) {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            }

            if (xk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            } else if (xk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
            } else if (yk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (yk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
            } else if (zk > 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            } else if (zk < 0.0) {
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
            }

            return axisOrientation;
        }

        mat.set(0, 0, xi);
        mat.set(0, 1, xj);
        mat.set(0, 2, xk);
        mat.set(1, 0, yi);
        mat.set(1, 1, yj);
        mat.set(1, 2, yk);
        mat.set(2, 0, zi);
        mat.set(2, 1, zj);
        mat.set(2, 2, zk);

        // At this point, Q is the rotation matrix from the (i,j,k) to the (x,y,z) axes
        final TransMatrix Q = new TransMatrix(mat);
        detQ = Q.Determinant();
        final TransMatrix P = new TransMatrix(mat.getDim());
        final TransMatrix M = new TransMatrix(mat.getDim());

        if (detQ == 0.0) {
            // MipavUtil.displayError("detQ == 0.0 in getAxisOrientation");
            Preferences.debug("detQ == 0.0 in getAxisOrientation", Preferences.DEBUG_FILEIO);
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

                    if ( (i == k) || (j == k)) {
                        continue;
                    }

                    P.MakeZero();

                    for (p = -1; p <= 1; p += 2) { // p,q,r are -1 or +1 and go into rows #1,2,3

                        for (q = -1; q <= 1; q += 2) {

                            for (r = -1; r <= 1; r += 2) {
                                P.set(0, i - 1, p);
                                P.set(1, j - 1, q);
                                P.set(2, k - 1, r);
                                detP = P.Determinant();

                                // sign of permutation doesn't match sign of Q
                                if ( (detP * detQ) <= 0.0) {
                                    continue;
                                }

                                M.Copy(P);
                                M.Mult(Q);

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
        // (x,y,z) axes. If ibest is 2, then the i axis is along the yaxis,
        // which is direction A2P (if pbest < 0) or P2A (if pbest > 0).

        // So, using ibest and pbest, we can assign the output code for
        // the i axis. The same also applies for the j and k axes.

        switch (ibest * pbest) {

            case -1:
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 1:
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case -2:
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case 2:
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 3:
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
                break;

            case -3:
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        switch (jbest * qbest) {

            case -1:
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 1:
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case -2:
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case 2:
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 3:
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
                break;

            case -3:
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        switch (kbest * rbest) {

            case -1:
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case 1:
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case -2:
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case 2:
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 3:
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                break;

            case -3:
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        return axisOrientation;
    }

    /**
     * Return the slice thickness value stored in the minc header (var = 'acquisition', attrib = 'slice_thickness').
     * 
     * @return The slice thickness, if it is stored in the minc header (0 otherwise).
     */
    private double getMincSliceThickness() {

        for (final FileMincVarElem element : varArray) {

            if (element.name.equals("acquisition")) {

                for (final FileMincAttElem element2 : element.vattArray) {

                    if (element2.name.equals("slice_thickness")) {
                        return ((Double) element2.values[0]).doubleValue();
                    }
                }
            }
        }

        return 0;
    }

    /**
     * Helper method to set the axis orientations.
     * 
     * @param space The space - "xspace", "yspace", or "zspace".
     * @param positive Flag indicating if the space is moving in a positive direction.
     * 
     * @return The proper axis orientation for that space.
     */
    public static int setOrientType(final String space, final boolean positive) {

        if (positive) {

            if (space.equals("xspace")) {
                return FileInfoBase.ORI_L2R_TYPE;
            } else if (space.equals("yspace")) {
                return FileInfoBase.ORI_P2A_TYPE;
            } else if (space.equals("zspace")) {
                return FileInfoBase.ORI_I2S_TYPE;
            }
        } else {

            if (space.equals("xspace")) {
                return FileInfoBase.ORI_R2L_TYPE;
            } else if (space.equals("yspace")) {
                return FileInfoBase.ORI_A2P_TYPE;
            } else if (space.equals("zspace")) {
                return FileInfoBase.ORI_S2I_TYPE;
            }
        }

        return FileInfoBase.ORI_UNKNOWN_TYPE;
    }

    /**
     * Sets the start location of the specified axis.
     * 
     * @param originCoord origin coord.
     * @param axis Axis of orientation; x is 0, y is 1, z is 2.
     */
    private void setStartLocation(final double originCoord, final int axis) {

        String space;
        int index = 0;

        if ( (axis < 0) || (axis > 2)) {
            Preferences.debug("Error: Axis must be 0, 1, or 2.\n", Preferences.DEBUG_FILEIO);

            return;
        }

        // opposite axis: if axis is 2 we need to getDimElem(0). See previous examples
        // of String spacez = getDimElem(0).name. Minc stores the x dimension in
        // 2, the y dimension in 1, and the z dimension in 0.
        space = getDimElem(Math.abs(axis - 2)).name;

        for (int i = 0; i < varArray.length; i++) {

            if (varArray[i].name.equals(space)) {
                index = i;
            }
        }

        varArray[index].start = originCoord;
    }
}
