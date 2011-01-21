package gov.nih.mipav.model.file;


import java.util.*;


/**
 * Class designed to hold pre-loaded information for opening RAW images.  This is used when running scripts
 * (either through GUI or command line)... two specific constructors meant for commandline/gui scripts
 * @author linkb
 *
 */
public class RawImageInfo {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean bigEndian;

    /** DOCUMENT ME! */
    private int[] dims;

    /** DOCUMENT ME! */
    private int offset;

    /** DOCUMENT ME! */
    private float[] res;

    /** DOCUMENT ME! */
    private int type;

    /** DOCUMENT ME! */
    private int[] units;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used when running from command line.  Parses the command line's string into the correct variables
     */
    public RawImageInfo(String consoleString) {
        StringTokenizer tokens = new StringTokenizer(consoleString, ";");
        String typeStr = tokens.nextToken();
        String extStr = tokens.nextToken();
        String resStr = tokens.nextToken();
        String unitsStr = tokens.nextToken();
        String endianStr = tokens.nextToken();
        String offsetStr = tokens.nextToken();

        type = Integer.parseInt(typeStr);

        // get extents
        tokens = new StringTokenizer(extStr, ",");

        int firstDim = Integer.parseInt(tokens.nextToken());
        int secondDim = Integer.parseInt(tokens.nextToken());
        int thirdDim = 1;
        int fourthDim = 1;

        try {
            thirdDim = Integer.parseInt(tokens.nextToken());
            fourthDim = Integer.parseInt(tokens.nextToken());
        } catch (Exception e) {
            // nada
        }

        if (thirdDim > 1) {

            if (fourthDim > 1) {
                dims = new int[4];
                dims[3] = fourthDim;
            } else {
                dims = new int[3];
            }

            dims[2] = thirdDim;
        } else {
            dims = new int[2];
        }

        dims[0] = firstDim;
        dims[1] = secondDim;

        // get resolutions
        tokens = new StringTokenizer(resStr, ",");

        res = new float[dims.length];

        for (int n = 0; n < res.length; n++) {
            res[n] = Float.parseFloat(tokens.nextToken());
        }

        // get units of measure
        units = new int[dims.length];
        tokens = new StringTokenizer(unitsStr, ",");

        for (int n = 0; n < units.length; n++) {
            units[n] = Integer.parseInt(tokens.nextToken());
        }

        bigEndian = new Boolean(endianStr).booleanValue();

        offset = Integer.parseInt(offsetStr);
    }

    /**
     * Constructor used when running scripts from the GUI
     *
     * @param  type       image type
     * @param  dims       image extents
     * @param  res        image resolutions
     * @param  units      image units
     * @param  offset     image offset
     * @param  bigEndian  image's big endian order
     */
    public RawImageInfo(int type, int[] dims, float[] res, int[] units, int offset, boolean bigEndian) {
        this.type = type;
        this.dims = dims;
        this.res = res;
        this.units = units;
        this.offset = offset;
        this.bigEndian = bigEndian;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getDataType() {
        return this.type;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getEndianess() {
        return this.bigEndian;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getExtents() {
        return this.dims;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getOffset() {
        return this.offset;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getResolutions() {
        return this.res;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getUnitsOfMeasure() {
        return this.units;
    }
}
