package gov.nih.mipav.model.structures;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewList;

import java.awt.Color;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Random;
import java.util.Vector;

import javax.swing.event.EventListenerList;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This the Volume Of Interest (VOI) structure. An image can have 32565 different VOIs. A VOI can have multiple contours
 * in a single slice or in other slices. VOIs can be additive or subtractive so that doughnut like objects can be made.
 * A VOI can be 5 different types: point, line, protractor, polyline and contour.
 *
 * @version  0.1 Oct 27, 1997
 */
public class VOI extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4045424525937515770L;
    

    /** Static Variables for VOI and VOI Contour movement. */
    public static final int FORWARD = 0;

    /** DOCUMENT ME! */
    public static final int BACKWARD = 1;

    /** DOCUMENT ME! */
    public static final int FRONT = 2;

    /** DOCUMENT ME! */
    public static final int BACK = 3;
    
    

    /** Indicates only the boundary of the VOI should be displayed. */
    public static final int BOUNDARY = 0; // Not filled in

    /** Indicates that the VOI should be displayed as a solid and not just a boundary. */
    public static final int SOLID = 1; // Filled in

    /** Mask as ones - i.e. put ones to indicate VOI location */
    public static final int ADDITIVE = 1;

    /** Mask as zeros - i.e. put zeros to indicate where the VOI is located */
    public static final int SUBTRACTIVE = 2;

    /** Indicates that the VOI is of type CONTOUR. */
    public static final int CONTOUR = 0; // Contour type of contour - closed

    /** Indicates that the VOI is of type POLYLINE type of contour - end points NOT connected. */
    public static final int POLYLINE = 1;

    /** Indicates that the VOI is of type Line made up of two points. */
    public static final int LINE = 2; // Line

    /** Indicates that the POINT is of type Line made up of a single point. */
    public static final int POINT = 3; // Point

    /** Indicates that the VOI is of type PROTRACTOR used to measure angles. */
    public static final int PROTRACTOR = 4; // Protractor

    /** Indicates that the VOI is of type ANNOTATION for adding text annotations to an image. */
    public static final int ANNOTATION = 5; // annotation (text)

    /** Indicates that the VOI is of type CARDIOLOGY, special VOI for specific tasks needed in cardiology. */
    public static final int CARDIOLOGY = 6; // cardiology VOI

    /** Indicates that the VOI is of type POLYLINE that will go through more than one slice. */
    public static final int POLYLINE_SLICE = 7;

    /** DOCUMENT ME! */
    public static final float NOT_A_LEVELSET = Float.MIN_VALUE;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** int indictating that no point was found. */
    public final int NOT_A_POINT = -99;

    /** If true indicates tha the VOI is selected (active). */
    private boolean active;

    /** DOCUMENT ME! */
    private int activePolylineSlicePoint = 0;

    /** If true the bounding box of the VOI should be displayed. */
    private boolean boundingBox = false;

    /** Indicates the color or the VOI. */
    private Color color;

    /** The thickness of the VOI lines*/
    private int thickness = 1;

    /** DOCUMENT ME! */
    private transient ViewJFrameGraph contourGraph = null;

    /** A vector array of curves per slice. */
    private Vector<VOIBase> curves;

    /** Indicates the type of VOI (i.e. CONTOUR, POLYLINE, LINE, POINT, PROTRACTOR, etc. ... ) */
    private int curveType;

    /** Indicates if the VOI should be shown as a boundary or a solid. */
    private int displayMode;

    /** The label (numbering) of the curve, displayed when curve is highlighted. */
    private short elementLabel = 1;

    /** If true the VOI cannot be moved, if false this VOI can be moved. */
    private boolean fixed = false;

    /** ID of the VOI, also used when choosing the display color. */
    private short ID;

    /** DOCUMENT ME! */
    private float ignoreMax = -Float.MAX_VALUE;

    /** DOCUMENT ME! */
    private float ignoreMin = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    private float level = NOT_A_LEVELSET;

    /** DOCUMENT ME! */
    private transient EventListenerList listenerList;

    /** Name of the VOI stored as a string. */
    private String name;

    /** DOCUMENT ME! */
    private int nearBoundPoint = NOT_A_POINT;

    /** When in the solid display mode indicates how opaque the VOI should be. Zero is transparent and one is opaque. */
    private float opacity;

    /** DOCUMENT ME! */
    private int pLineSliceIndex = 0;

    /** Indicates if the VOI should mask ones or mask as zeros. */
    private int polarity;

    /** DOCUMENT ME! */
    private int polygonIndex;

    /** If true this flag indicates that the VOI should be included (applied) when processing the image. */
    private boolean process;

    /** DOCUMENT ME! */
    private int resizeIndex;

    /** DOCUMENT ME! */
    private ViewList[] stats = null;

    /** Unique ID for saving & retrieving. */
    private int UID;

    /** If true the VOI is visible. */
    private boolean visible;

    /** DOCUMENT ME! */
    private VOIEvent voiUpdate;

    /** Used to objects a label or ID inconjuction with the watershed algorithm. */
    private short watershedID;

    /** The x - dimension bounds: xBounds [0] = min and xBounds[1] = max. */
    private float[] xBounds = new float[2];

    /** The y - dimension bounds: yBounds [0] = min and yBounds[1] = max. */
    private float[] yBounds = new float[2];

    /** The z - dimension bounds: zBounds [0] = min and zBounds[1] = max. */
    private float[] zBounds = new float[2];

    /** extension of voi file name of voi was read in through file **/
    private String extension = "";
    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Constructs a Volume of Interest (VOI).
     *
     * @param  id    identifier of VOI
     * @param  name  name of the VOI
     * @param  zDim  slice (2D image - slice = 1)
     */
    public VOI(short id, String name, int zDim) {
        float hue;
        this.name = name;
        this.ID = id;
        this.watershedID = id;
        this.active = false;
        this.process = true;
        curves = new Vector<VOIBase>();


        displayMode = BOUNDARY;
        polarity = ADDITIVE;
        visible = true;
        opacity = 0.3f;
        hue = (float) ((((ID) * 35) % 360) / 360.0);
        setColor(Color.getHSBColor(hue, (float) 1.0, (float) 1.0)); // important to use the access method

        this.thickness = Preferences.getVOIThickness();

        // this ensures that color gets set in
        // all protractor contours.
        stats = new ViewList[12];
        stats[0] = new ViewList("No. of voxels", false);
        stats[1] = new ViewList("Volume", false);
        stats[2] = new ViewList("Area", true);
        stats[3] = new ViewList("Average voxel intensity", false);
        stats[4] = new ViewList("Std. dev. of voxel intensity", false);
        stats[5] = new ViewList("Sum Intensities", false);
        stats[6] = new ViewList("Geometric center", false);
        stats[7] = new ViewList("Center of Mass", false);
        stats[8] = new ViewList("Principal axis (only 2D)", false);
        stats[9] = new ViewList("Eccentricity (only 2D)", false);
        stats[10] = new ViewList("Major axis length (only 2D)", false);
        stats[11] = new ViewList("Minor axis length (only 2D)", false);
    }


    /**
     * Constructs a Volume of Interest (VOI).
     *
     * @param  id         identifier of VOI
     * @param  name       name of the VOI
     * @param  zDim       number of slices (2D image: slice = 1)
     * @param  curveType  type of curve, either a line or a contour
     * @param  presetHue  If presetHue >= 0.0, use this value as the hue
     */
    public VOI(short id, String name, int zDim, int curveType, float presetHue) {
        float hue;

        if (curveType != POINT) {
            this.UID = this.hashCode();
            // System.out.println("Non Point VOI with UID: " + this.UID);
        }

        this.name = name;
        this.ID = id;
        this.watershedID = id;
        this.active = false;
        this.process = true;
        this.curveType = curveType;
        curves = new Vector<VOIBase>();

        displayMode = BOUNDARY;
        polarity = ADDITIVE;
        visible = true;
        opacity = 0.3f;

        int colorIncrement = Preferences.getVOIColorIncrement();

        if (presetHue >= 0.0f) {
            hue = presetHue;
            // System.err.println("Setting hue to preset: " + hue);
        } else {
            hue = (float) ((((ID + colorIncrement) * 35) % 360) / 360.0);
            // System.err.println("Setting hue to calculated: " + hue);
        }

        setColor(Color.getHSBColor(hue, (float) 1.0, (float) 1.0)); // important to use the access method

        this.thickness = Preferences.getVOIThickness();

        // this ensures that color gets set in
        // all protractor contours.
        stats = new ViewList[12];
        stats[0] = new ViewList("No. of voxels", false);
        stats[1] = new ViewList("Volume", false);
        stats[2] = new ViewList("Area", true);
        stats[3] = new ViewList("Average voxel intensity", false);
        stats[4] = new ViewList("Std. dev. of voxel intensity", false);
        stats[5] = new ViewList("Sum Intensities", false);
        stats[6] = new ViewList("Geometric center", false);
        stats[7] = new ViewList("Center of Mass", false);
        stats[8] = new ViewList("Principal axis (only 2D)", false);
        stats[9] = new ViewList("Eccentricity (only 2D)", false);
        stats[10] = new ViewList("Major axis length (only 2D)", false);
        stats[11] = new ViewList("Minor axis length (only 2D)", false);
    }

    /**
     * Copies the VOI into a new VOI object.
     * @param kVOI VOI to copy.
     */
    public VOI( VOI kVOI )
    {
        this.active = kVOI.active;
        this.activePolylineSlicePoint = kVOI.activePolylineSlicePoint;
        this.boundingBox = kVOI.boundingBox;
        this.color = new Color( kVOI.color.getRed(), kVOI.color.getBlue(), kVOI.color.getGreen() );
        this.thickness = kVOI.thickness;
        this.contourGraph = kVOI.contourGraph;
        this.curves = new Vector<VOIBase>();
        for ( int j = 0; j < kVOI.curves.size(); j++ )
        {
            VOIBase kContour = kVOI.curves.get(j).clone();
            kContour.setGroup(this);
            this.curves.add( kContour );
        }

        this.curveType = kVOI.curveType;
        this.displayMode = kVOI.displayMode;
        this.elementLabel = kVOI.elementLabel;
        this.fixed = kVOI.fixed;
        this.ID = kVOI.ID;
        this.ignoreMax = kVOI.ignoreMax;
        this.ignoreMin = kVOI.ignoreMin;
        this.level = kVOI.level;

        if ( kVOI.listenerList != null) {
            kVOI.listenerList.getListenerCount(VOIListener.class);
            VOIListener [] voiList = kVOI.listenerList.getListeners(VOIListener.class);

            for (int i = 0; i < voiList.length; i++) {
                this.addVOIListener(voiList[i]);
            }
        }


        this.name = new String( kVOI.name );
        this.nearBoundPoint = kVOI.nearBoundPoint;
        this.opacity = kVOI.opacity;
        this.pLineSliceIndex = kVOI.pLineSliceIndex;
        this.polarity = kVOI.polarity;
        this.polygonIndex = kVOI.polygonIndex;

        this.process = kVOI.process;
        this.resizeIndex = kVOI.resizeIndex;


        if ( kVOI.stats != null )
        {
            this.stats = new ViewList[kVOI.stats.length];
            for ( int i = 0; i < kVOI.stats.length; i++ )
            {
                this.stats[i] = new ViewList( kVOI.stats[i].getString(), kVOI.stats[i].getState() );
            }
        }
        this.UID = kVOI.UID;
        this.visible = kVOI.visible;

        if ( kVOI.voiUpdate != null )
        {
            this.voiUpdate = new VOIEvent( (VOI)kVOI.voiUpdate.getSource(), kVOI.voiUpdate.getBase() );
        }

        this.watershedID = kVOI.watershedID;
        this.xBounds[0] = kVOI.xBounds[0];
        this.xBounds[1] = kVOI.xBounds[1];
        this.yBounds[0] = kVOI.yBounds[0];
        this.yBounds[1] = kVOI.yBounds[1];
        this.zBounds[0] = kVOI.zBounds[0];
        this.zBounds[1] = kVOI.zBounds[1];
        this.extension = new String( kVOI.extension );
    }

    // --------- Event-handling routines:
    // to add this object to send out events for listening
    // objects, at least the following 3 methods must be
    // present: addListener, removeListener, fireEvent as
    // present below.
    /**
     * adds the update listener.
     *
     * @param  listener  DOCUMENT ME!
     */
    public void addVOIListener(VOIListener listener) {

        if (listenerList == null) {
            listenerList = new EventListenerList();
        }

        listenerList.add(VOIListener.class, listener);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Finds the area of the entire VOI of the VOIContour type only.
     *
     * @return  returns the area
     */
    public int area() {
        if ( curveType != CONTOUR ) {
            return 0;
        }
        int totalArea = 0;
        for (int i = 0; i < curves.size(); i++) {
            totalArea += curves.elementAt(i).area();
        }

        return totalArea;
    }

    /**
     * Calculate the distance of the largest line segment contained entirely within the VOI
     * @param xDim
     * @param yDim
     * @param xRes
     * @param yRes
     * @param zRes
     * @return largestDistance
     */
    public double calcLargestDistance(int xDim, int yDim, float xRes, float yRes, float zRes) {
        int i;
        int j;
        float xPts[];
        float yPts[];
        float zPts[];
        int nGons;
        int nPts = 0;
        int index = 0;
        int len;

        if (curveType != CONTOUR) {
            MipavUtil.displayError("curveType is not the required CONTOUR for calcLargestDistance");
            return 0;
        }

        nGons = curves.size(); 
        for (i = 0; i < nGons; i++) {
            nPts += curves.elementAt(i).size();
        }

        xPts = new float[nPts];
        yPts = new float[nPts];
        zPts = new float[nPts];

        nGons = curves.size();
        for (i = 0; i < nGons; i++) {
            len = curves.elementAt(i).size();
            for (j = 0; j < len; j++) {
                xPts[index] = (((VOIContour) (curves.elementAt(i))).elementAt(j)).X;
                yPts[index] = (((VOIContour) (curves.elementAt(i))).elementAt(j)).Y;
                zPts[index++] = (((VOIContour) (curves.elementAt(i))).elementAt(j)).Z;
            }
        }

        long time = System.currentTimeMillis();
        double avg = 0, max = 0;
        double[] maxAvg = determineMaxAvg(xRes, yRes, zRes, xPts, yPts, zPts);
        max = maxAvg[0];
        avg = maxAvg[1];

        Preferences.debug("Traverse points\n");
        time = System.currentTimeMillis();
        double a = 0, lowerBound = Double.MAX_VALUE, upperBound = Double.MAX_VALUE;
        int iter = 0;
        boolean terminal = false;
        while(a == 0 && !terminal) {
            ArrayList<Integer> orig = new ArrayList<Integer>();
            ArrayList<Integer> term = new ArrayList<Integer>();
            upperBound = lowerBound;
            lowerBound = Math.max(0, max-iter*(avg/Math.max(1, (int)Math.pow(xPts.length, 0.3)-9)));
            gatherBoundedPoints(orig, term, lowerBound-1, upperBound, xRes, yRes, zRes, xPts, yPts, zPts);
            time = System.currentTimeMillis();        
            Preferences.debug("Completed points in "+(System.currentTimeMillis() - time)+"\tsize: "+orig.size()+"\n");
            a = getLargest(orig, term, xRes, yRes, zRes, xPts, yPts, zPts);
            if(lowerBound == 0.0) {
                terminal = true;
            }
            iter++;
        }
        return a;
    }

    /**
     * Prints out the distances between segments, displacement (1st to last point) and total distance in 2D and 3D.
     *
     * @param  fileInfo  FileInfoBase
    public void calcPLineDistances(FileInfoBase fileInfo) {

        if (curveType != POLYLINE_SLICE) {
            return;
        }

        int unitsOfMeasure = fileInfo.getUnitsOfMeasure(0);
        float[] resols = fileInfo.getResolutions();
        int i;

        Vector distanceVector = new Vector();
        Vector3f firstPoint, secondPoint;

        String unitsStr = new String(" " + FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure) );
        int numSlices = fileInfo.getExtents()[2];
        int pointNumber = 0;

        for (i = 0; i < curves.size(); i++) {
            try {
                pointNumber = Integer.parseInt(((VOIPoint) curves.elementAt(i)).getLabel());
                firstPoint = ((VOIPoint) curves.elementAt(i)).exportPoint();
                distanceVector.add(new PolyPointHolder(firstPoint, pointNumber));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        Collections.sort(distanceVector, new PointComparator());

        double distance2D = 0;
        double distance3D = 0;
        double totalDistance2D = 0;
        double totalDistance3D = 0;
        double displacement3D = 0;
        double displacement2D = 0;

        ViewUserInterface.getReference().getMessageFrame().append("Number of points in polyline slice VOI: " +
                distanceVector.size() + "\n", ViewJFrameMessage.DATA);

        DecimalFormat n = new DecimalFormat("0.00");

        for (i = 0; i < (distanceVector.size() - 1); i++) {
            firstPoint = ((PolyPointHolder) distanceVector.elementAt(i)).getPoint();
            secondPoint = ((PolyPointHolder) distanceVector.elementAt(i + 1)).getPoint();
            distance3D = MipavMath.distance(firstPoint.X, firstPoint.Y, firstPoint.Z, secondPoint.X, secondPoint.Y,
                    secondPoint.Z, resols);
            distance2D = MipavMath.distance(firstPoint.X, firstPoint.Y, 0, secondPoint.X, secondPoint.Y, 0, resols);
            totalDistance3D += distance3D;
            totalDistance2D += distance2D;

            ViewUserInterface.getReference().getMessageFrame().append("\tpoint " + Integer.toString(i + 1) +
                    " to point " + Integer.toString(i + 2) + ": " +
                    n.format(distance2D) + unitsStr + " (2D), " +
                    n.format(distance3D) + unitsStr + " (3D)\n",
                    ViewJFrameMessage.DATA);

        }

        firstPoint = ((PolyPointHolder) distanceVector.elementAt(0)).getPoint();
        secondPoint = ((PolyPointHolder) distanceVector.elementAt(distanceVector.size() - 1)).getPoint();

        displacement2D = MipavMath.distance(firstPoint.X, firstPoint.Y, 0, secondPoint.X, secondPoint.Y, 0, resols);
        displacement3D = MipavMath.distance(firstPoint.X, firstPoint.Y, firstPoint.Z, secondPoint.X, secondPoint.Y,
                secondPoint.Z, resols);

        // print displacement
        ViewUserInterface.getReference().getMessageFrame().append("Displacement (first point to last point): " +
                n.format(displacement2D) + unitsStr + " (2D), " +
                n.format(displacement3D) + unitsStr + " (3D)\n",
                ViewJFrameMessage.DATA);


        // print total distance
        ViewUserInterface.getReference().getMessageFrame().append("Total distance: " + n.format(totalDistance2D) +
                unitsStr + " (2D), " + n.format(totalDistance3D) +
                unitsStr + " (3D)\n", ViewJFrameMessage.DATA);

    }
     */

    /**
     * Finds convex hull for all contours in VOI independently NOT a 3D convex hull.
    public void convexHull() {
        if ( curveType != CONTOUR ) {
            return;
        }
        for (int i = 0; i < curves.size(); i++) {
            ((VOIContour)curves.elementAt(i)).convexHull();
        }
    }
     */

    /**
     * Clone function that calls the super (modelserializable) clone and then manually copies references 
     * to the transient VOIListeners (eventlistenerlist)
     */
    public Object clone() {
        //Object obj = super.clone();
        Object obj = new VOI(this);

        if (listenerList != null) {
            VOIListener [] voiList = listenerList.getListeners(VOIListener.class);
            for (int i = 0; i < voiList.length; i++) {
                ((VOI)obj).addVOIListener(voiList[i]);
            }
        }

        return obj;
    }

    /**
     * Finds convex hull of a specific contour.
     *
     * @param  slice  DOCUMENT ME!
     * @param  index  DOCUMENT ME!
     * @deprecated
    public void convexHull(int slice, int index) {

        if (curveType == CONTOUR) {
            ((VOIContour) (curves.elementAt(index))).convexHull();
        }
    }
     */

    /**
     * Creates a binary mask at every slice.
     *
     * @param  mask  the binary mask
     * @param  xDim  x dimension, used to index into the image
     * @param  yDim  y dimension, used to index into the image
     */
    public void createActiveContourBinaryMask(BitSet mask, int xDim, int yDim) {
        createBinaryMask3D( mask, xDim, yDim, false, true );
    }


    /**
     * Forms a binary representation of the VOI into the image.
     *
     * @param  image       boolean image where VOI bits are to be set.
     * @param  XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param  onlyActive  Only mask regions that are active (i.e. selected )
     */
    public void createBinaryImage(ModelImage image, boolean XOR, boolean onlyActive) {
        int xDim = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
        int yDim = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
        int zDim = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
        int length = xDim*yDim*zDim;
        int i;
        int t;
        int tDim = 1;

        // System.err.println("creating binary image");
        if (image.getType() != ModelStorageBase.BOOLEAN) {
            return;
        }

        if (image.getNDims() >= 4) {
            tDim = image.getExtents()[3];
        }

        createBinaryMask3D(image.getMask(), xDim, yDim, XOR, onlyActive);

        BitSet mask = image.getMask();

        for (t = 0; t < tDim; t++) {
            for (i = 0; i < length; i++) {

                if (mask.get(i) == true) {
                    image.set(t*length + i, true);
                } else {
                    image.set(t* length + i, false);
                }
            }
        }
    }

    /**
     * Creates a binary mask at a slice for <i>that</i> slice only.
     *
     * @param   xDim   x dimension, used to index into the image
     * @param   yDim   y dimension, used to index into the image
     * @param   slice  slice to create mask at
     *
     * @return  mask the binary mask
     */
    public BitSet createBinaryMask(int xDim, int yDim, int slice) {
        BitSet mask;

        // System.err.println("creating binary mask");
        try {
            mask = new BitSet(xDim * yDim); // bitset with a rectangular size
        } catch (OutOfMemoryError oome) {
            return null;
        }

        int x, y;
        int i, nGons;
        int offset; // from corner
        nGons = curves.size();

        if ((process == true) && (curveType == CONTOUR)) {

            for (i = 0; i < nGons; i++) {
                ((VOIContour) (curves.elementAt(i))).contains(0, 0, slice);
                getBounds(xBounds, yBounds, zBounds);

                int xbs = (int) xBounds[0];
                int xbe = (int) xBounds[1];
                int ybs = (int) yBounds[0];
                int ybe = (int) yBounds[1];

                for (y = ybs; y < ybe; y++) {
                    offset = y * xDim; // a horizontal offset

                    for (x = xbs; x < xbe; x++) {

                        if (((VOIContour) (curves.elementAt(i))).contains(x, y, slice) &&
                                (polarity == ADDITIVE)) {

                            if (mask.get(offset + x)) {

                                // System.err.println("clearing");
                                mask.clear(offset + x);
                            } else {
                                mask.set(offset + x);
                            }
                        } else if (((VOIContour) (curves.elementAt(i))).contains(x, y, slice) &&
                                (polarity == SUBTRACTIVE)) {

                            // System.err.println("doing subtractive");
                            mask.clear(offset + x);
                        } else {
                            // mask[0].clear(offset + x);
                        }
                    }
                }
            }
        } else if ((process == true) && (curveType == POINT)) {
            Vector3f pt;
            int size = curves.size();

            for (i = 0; i < size; i++) {
                pt = ((VOIPoint) (curves.elementAt(i))).exportPoint();
                if ( pt.Z == slice )
                {
                    offset = MipavMath.round((pt.Y * xDim) + pt.X);

                    if (polarity == ADDITIVE) {

                        if (mask.get(offset)) {
                            mask.clear();
                        } else {
                            mask.set(offset);
                        }
                    } else if (polarity == SUBTRACTIVE) {
                        mask.clear(offset);
                    } else {
                        // mask[0].clear(offset + x);
                    }
                }
            }
        }

        return mask;
    }

    /**
     * Creates a binary mask at a slice.
     *
     * @param  xDim        x dimension, used to index into the image
     * @param  yDim        y dimension, used to index into the image
     * @param  slice       slice to create mask at
     * @param  mask        the binary mask
     * @param  XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param  onlyActive  Only mask regions that are active (i.e. selected )
     * @deprecated
     */
    public void createBinaryMask(int xDim, int yDim, int slice, BitSet mask, boolean XOR, boolean onlyActive) {

        // System.err.println("doing create binary mask");
        int x = 0, y;
        int i, nGons;
        int offset;
        int offsetZ;
        nGons = curves.size();
        offsetZ = slice * xDim * yDim;

        if ((process == true) && (curveType == CONTOUR)) {
            //System.err.println("XOR is " + XOR);
            //System.err.println("polarity additive?: " + (polarity == ADDITIVE));
            for (i = 0; i < nGons; i++) {

                if (!onlyActive || ((VOIContour) (curves.elementAt(i))).isActive()) {
                    ((VOIContour) (curves.elementAt(i))).contains(0, 0);
                    ((VOIContour) (curves.elementAt(i))).getBounds(xBounds, yBounds, zBounds);

                    // Keep the next four lines!!
                    int xbs = (int) xBounds[0];
                    int xbe = (int) xBounds[1];
                    int ybs = (int) yBounds[0];
                    int ybe = (int) yBounds[1];

                    // System.err.println("Xbounds 0 = " + xBounds[0] + " Xbounds 1 = " + xBounds[1]);
                    for (y = ybs; y < ybe; y++) {
                        offset = offsetZ + (y * xDim);

                        for (x = xbs; x < xbe; x++) {

                            if (((VOIContour) (curves.elementAt(i))).contains(x, y) &&
                                    (polarity == ADDITIVE)) {

                                if (XOR && mask.get(offset + x)) {
                                    mask.clear(offset + x);
                                } else {
                                    mask.set(offset + x);
                                }
                            } else if (((VOIContour) (curves.elementAt(i))).contains(x, y) &&
                                    (polarity == SUBTRACTIVE)) {

                                // System.err.println("doing subtractive");
                                mask.clear(offset + x);
                            } else {
                                // mask[0].clear(offset + x);
                            }
                        }
                    }
                }
            }
        } else if ((process == true) && (curveType == POINT)) {
            Vector3f pt;
            int size = curves.size();

            for (i = 0; i < size; i++) {
                pt = ((VOIPoint) (curves.elementAt(i))).exportPoint();

                if (!onlyActive || ((VOIPoint) (curves.elementAt(i))).isActive()) {
                    offset = MipavMath.round((slice * xDim * yDim) + (pt.Y * xDim) + pt.X);

                    if (polarity == ADDITIVE) {

                        if (XOR && mask.get(offset)) {
                            mask.clear(offset);
                        } else {
                            mask.set(offset);
                        }
                    } else if (polarity == SUBTRACTIVE) {
                        mask.clear(offset);
                    } else {
                        // mask[0].clear(offset + x);
                    }
                }
            }
        }
    }

    /**
     * Creates a binary mask at a slice for <i>that</i> slice and element only.
     *
     * @param   xDim     x dimension, used to index into the image
     * @param   yDim     y dimension, used to index into the image
     * @param   slice    slice to create mask at
     * @param   element  element of VOI
     *
     * @return  mask the binary mask
     */
    public BitSet createBinaryMask(int xDim, int yDim, int slice, int element) {
        BitSet mask;

        // System.err.println("creating binary mask");
        try {
            mask = new BitSet(xDim * yDim); // bitset with a rectangular size
        } catch (OutOfMemoryError oome) {
            return null;
        }

        int x, y;
        int nGons;
        int offset; // from corner
        nGons = curves.size();

        if (element >= nGons) {
            MipavUtil.displayError("element " + element + " does not exist");

            return null;
        }

        if ((process == true) && (curveType == CONTOUR)) {
            ((VOIContour) (curves.elementAt(element))).contains(0, 0, slice);
            getBounds(xBounds, yBounds, zBounds);

            int xbs = (int) xBounds[0];
            int xbe = (int) xBounds[1];
            int ybs = (int) yBounds[0];
            int ybe = (int) yBounds[1];

            for (y = ybs; y < ybe; y++) {
                offset = y * xDim; // a horizontal offset

                for (x = xbs; x < xbe; x++) {

                    if (((VOIContour) (curves.elementAt(element))).contains(x, y, slice) &&
                            (polarity == ADDITIVE)) {

                        if (mask.get(offset + x)) {

                            // System.err.println("clearing");
                            mask.clear(offset + x);
                        } else {
                            mask.set(offset + x);
                        }
                    } else if (((VOIContour) (curves.elementAt(element))).contains(x, y, slice) &&
                            (polarity == SUBTRACTIVE)) {

                        // System.err.println("doing subtractive");
                        mask.clear(offset + x);
                    } else {
                        // mask[0].clear(offset + x);
                    }
                }
            }
        } else if ((process == true) && (curveType == POINT)) {
            Vector3f pt;
            curves.size();
            pt = ((VOIPoint) (curves.elementAt(element))).exportPoint();
            if ( pt.Z == slice )
            {
                offset = MipavMath.round((pt.Y * xDim) + pt.X);

                if (polarity == ADDITIVE) {

                    if (mask.get(offset)) {
                        mask.clear();
                    } else {
                        mask.set(offset);
                    }
                } else if (polarity == SUBTRACTIVE) {
                    mask.clear(offset);
                } else {
                    // mask[0].clear(offset + x);
                }
            }
        }

        return mask;
    }

    public void createBinaryMask3D(BitSet mask, int xDim, int yDim, boolean XOR, boolean onlyActive) {
        int nGons = curves.size();

        if ((process == true) && (curveType == CONTOUR)) {
            for (int i = 0; i < nGons; i++) {

                if (!onlyActive || ((VOIContour) (curves.elementAt(i))).isActive()) {
                    ((VOIContour) (curves.elementAt(i))).getBounds(xBounds, yBounds, zBounds);

                    int xbs = (int) xBounds[0];
                    int xbe = (int) xBounds[1];
                    int ybs = (int) yBounds[0];
                    int ybe = (int) yBounds[1];
                    int zbs = (int) zBounds[0];
                    int zbe = (int) zBounds[1];
                    
                    for (int z = zbs; z <= zbe; z++) {
                        for (int y = ybs; y <= ybe; y++) {
                            for (int x = xbs; x <= xbe; x++) {
                                int offset = z * xDim * yDim + y * xDim + x;                                
                                if (curves.elementAt(i).contains(x, y, z) )
                                {
                                    if (polarity == ADDITIVE) {
                                        if (XOR && mask.get(offset)) {
                                            mask.clear(offset);
                                        } else {
                                            mask.set(offset);
                                        }
                                    }
                                    else if (polarity == SUBTRACTIVE) {
                                        mask.clear(offset);
                                    } 
                                } 
                            }
                        }
                    }
                }
            }
        } else if ((process == true) && (curveType == POINT)) {
            Vector3f pt;
            int size = curves.size();

            for (int i = 0; i < size; i++) {
                pt = ((VOIPoint) (curves.elementAt(i))).exportPoint();

                if (!onlyActive || ((VOIPoint) (curves.elementAt(i))).isActive()) {
                    int offset = MipavMath.round(pt.Z * xDim * yDim + pt.Y * xDim + pt.X);
                    if (polarity == ADDITIVE) {

                        if (XOR && mask.get(offset)) {
                            mask.clear(offset);
                        } else {
                            mask.set(offset);
                        }
                    } else if (polarity == SUBTRACTIVE) {
                        mask.clear(offset);
                    } else {
                        // mask[0].clear(offset + x);
                    }
                }
            }
        }
    }

    /**
     * Creates a short image of the VOI. Positions within the VOI are set to the VOI's watershed ID.
     *
     * @param  image       short image where VOI labels are to be set.
     * @param  offset      value added to watershedID - normally 1
     * @param  XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param  onlyActive  Only mask regions that are active (i.e. selected )
     */
    public void createShortImage(ModelImage image, int offset, boolean XOR, boolean onlyActive) {
        int xDim = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
        int yDim = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
        int zDim = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
        int length = xDim*yDim*zDim;
        int i;
        int tDim = 1;
        int t;

        if (image.getNDims() >= 4) {
            tDim = image.getExtents()[3];
        }
        createBinaryMask3D(image.getMask(), xDim, yDim, XOR, onlyActive);

        BitSet mask = image.getMask();

        for (t = 0; t < tDim; t++) {
            for (i = 0; i < length; i++) {

                if (mask.get(i) == true) {
                    image.set(t*length + i, watershedID + offset);
                }
            }
        }
    }

    /**
     * Creates a short mask at a slice.
     *
     * @param   xDim  x dimension, used to index into the image
     * @param   yDim  y dimension, used to index into the image
     * @param   mask  the short mask
     *
     * @return  returns the mask
     *
     * @param   XOR   indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     */
    public short[] createShortMask(short[] mask, int xDim, int yDim, int zDim, boolean XOR) {
        int length = xDim * yDim * zDim;
        int i;
        BitSet binaryMask = null;

        try {
            binaryMask = new BitSet(length);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to make mask.");

            return null;
        }

        createBinaryMask3D(binaryMask, xDim, yDim, XOR, false);

        if (mask == null) {
            return null;
        }

        // this used to be != instead of >
        if (mask.length > binaryMask.size()) {
            return null;
        }

        for (i = 0; i < length; i++) {

            if (binaryMask.get(i) == true) {
                mask[i] = this.ID;
            }
        }

        return mask;
    }

    /**
     * Creates a short image of the VOI. Positions within the VOI are set to the VOI's watershed ID.
     *
     * @param  image       short image where VOI labels are to be set.
     * @param  offset      value added to watershedID - normally 1
     * @param  XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param  onlyActive  Only mask regions that are active (i.e. selected )
     */
    public void createUByteImage(ModelImage image, int offset, boolean XOR, boolean onlyActive) {
        int xDim = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
        int yDim = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
        int zDim = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
        int length = xDim*yDim*zDim;
        int i;
        int tDim = 1;
        int t;

        if (image.getNDims() >= 4) {
            tDim = image.getExtents()[3];    
        }
        createBinaryMask3D(image.getMask(), xDim, yDim, XOR, onlyActive);

        BitSet mask = image.getMask();

        for (t = 0; t < tDim; t++) {
            for (i = 0; i < length; i++) {

                if (mask.get(i) == true) {
                    image.set(t*length + i, watershedID + offset);
                }
            }
        }
    }

    /**
     * two VOIs are the same if they have the same name.
     *
     * @param   str  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean equals(String str) {
        return (name.equals(str));
    }

    /**
     * Get Vector3fs from the VOI; can only use with Point.
     *
     * @return  array of points at the slice
     */
    public Vector3f[] exportAllPoints() {
        Vector3f[] points;

        if (curveType != POINT) {
            return null;
        }

        int len = curves.size();

        try {
            points = new Vector3f[len];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to export Points.");
            return null;
        }

        for (int i = 0; i < curves.size(); i++) {
            points[i] = ((VOIPoint) (curves.elementAt(i))).exportPoint();
        }
        return points;
    }


    /**
     * Draws the VOI of type VOIContour as a blending of the image and the VOI.
     *
     * @param  zoomX        zoom for the x coordinate
     * @param  zoomY        zoom for the y coordinate
     * @param  resolutionX  X resolution (aspect ratio)
     * @param  resolutionY  Y resolution (aspect ratio)
     * @param  slice        index of slice in curve array
     * @param  pixBuffer    pixel buffer of image
     * @param  g            the graphics context where the VOI is to be drawn
     * @param  xDim         x dimension maximum
     * @param  yDim         y dimension maximum
    public void drawBlendSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, int slice,
            int[] pixBuffer, Graphics g, int xDim, int yDim) {
        int i;

        if (displayMode == VOI.BOUNDARY) {
            return;
        }

        for (i = 0; i < curves.size(); i++) {

            // System.err.println("drawing blend self");
            if (visible) {
                g.setColor(color);

                if (curveType == CONTOUR && curves.elementAt(i).slice() == slice) {
                    ((VOIContour) (curves.elementAt(i))).drawBlendSelf(zoomX, zoomY, resolutionX, resolutionY, g,
                            xDim, yDim, pixBuffer, opacity, color);
                }
            }
        }
    }
     */

    /**
     * Returns the first point.
     *
     * @return  return the first point from the VOIPoint object
     */
    public Vector3f exportPoint() {
        if (curveType != POINT) {
            return null;
        }

        return ((VOIPoint) (curves.elementAt(0))).exportPoint();
    }

    /**
     * Get Vector3fs from the VOI; can only use with Point.
     *
     * @param   slice  index of slice
     *
     * @return  array of points at the slice
     * @deprecated
     */
    public Vector3f[] exportPoints(int slice) {

        if (curveType != POINT) {
            return null;
        }

        Vector<Vector3f> points = new Vector<Vector3f>();
        for (int i = 0; i < curves.size(); i++) {

            if (curves.elementAt(i).slice() == slice) {
                points.add(((VOIPoint) (curves.elementAt(i))).exportPoint());
            }
        }

        return (Vector3f[])points.toArray();
    }

    /**
     * Gets line arrays from the VOI; can only use with LINE, POINT, and PROTRACTOR.
     *
     * @param  x      array of x coordinates of line
     * @param  y      array of y coordinates of line
     * @param  z      array of z coordinates of line
     * @param  slice  index of slice
     * @deprecated
    public void exportArrays(float[] x, float[] y, float[] z, int slice) {

        for (int i = 0; i < curves.size(); i++) {
            exportArrays(x, y, z, slice, i);
        }
    }
     */

    /**
     * Gets line arrays from the VOI; can only use with LINE, POINT, and PROTRACTOR.
     *
     * @param  x        array of x coordinates of line
     * @param  y        array of y coordinates of line
     * @param  z        array of z coordinates of line
     * @param  slice    index of slice
     * @param  element  the element whose arrays should be exported
     * @deprecated
    public void exportArrays(float[] x, float[] y, float[] z, int slice, int element) {

        if (curveType == LINE) {
            ((VOILine) (curves.elementAt(element))).exportArrays(x, y, z);
        } else if (curveType == PROTRACTOR) {
            ((VOIProtractor) (curves.elementAt(element))).exportArrays(x, y, z);
        }
    }
     */

    /**
     * @return
     */
    public Polygon[] exportPolygons() {
        if ((curveType == LINE) || (curveType == POINT) || (curveType == PROTRACTOR) || (curveType == ANNOTATION) ||
                (curveType == POLYLINE_SLICE)) {
            return null;
        }

        Vector<Polygon> polygons = new Vector<Polygon>();

        for (int i = 0; i < curves.size(); i++) {
            polygons.add( ((VOIContour) (curves.elementAt(i))).exportPolygon(1, 1, 1, 1) );
        }

        return (Polygon[])polygons.toArray();
    }

    /**
     * Gets polygons from the VOI; can only use with Contour.
     *
     * @param   slice  index of slice
     *
     * @return  array of polygons at the slice
     */
    public Polygon[] exportPolygons(int slice) {
        if ((curveType == LINE) || (curveType == POINT) || (curveType == PROTRACTOR) || (curveType == ANNOTATION) ||
                (curveType == POLYLINE_SLICE)) {
            return null;
        }

        Vector<Polygon> polygons = new Vector<Polygon>();

        for (int i = 0; i < curves.size(); i++) {
            if ( curves.elementAt(i).slice() == slice)
            {
                polygons.add( ((VOIContour) (curves.elementAt(i))).exportPolygon(1, 1, 1, 1) );
            }
        }

        return (Polygon[])polygons.toArray();
    }

    /**
     * Prepares the class for cleanup.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {
        int i;

        if (curves != null) {
            for (int j = curves.size() - 1; j >= 0; j--) {
                VOIBase base = curves.remove(j);
                base.clear();
            }
            curves = null;
        }

        stats = null;
        xBounds = null;
        yBounds = null;
        zBounds = null;

        super.finalize();
    }
    
    /**
     * Finds the active Contour and returns it.
     *
     * @param   slice  indicates slice where active Contour can be found.
     *
     * @return  Contour that is active or null if no contour is active in the indicated slice.
     */
    public VOIBase getActiveContour(int slice) {
        if ((curveType != CONTOUR) && (curveType != POLYLINE)) {
            return null;
        }

        for (int i = 0; i < curves.size(); i++) {
            if (curves.elementAt(i).isActive() && curves.elementAt(i).slice() == slice) {
                return (curves.elementAt(i));
            }
        }

        return null;
    }


    /**
     * DOCUMENT ME!
     *
     * @param   slice  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
    public Vector3f exportPSlicePoint(int slice) {

        if (curveType == POLYLINE_SLICE) {

            try {
                return ((VOIPoint) (curves.elementAt(pLineSliceIndex))).exportPoint();
            } catch (Exception e) {
                return null;
            }

        }


        return null;
    }
     */

    /**
     * Finds the active Contour and returns it.
     *
     * @param   slice  indicates slice where active Contour can be found.
     *
     * @return  index of Contour that is active or -1 if not contour is active in the indicated slice.
     */
    public int getActiveContourIndex(int slice) {
        if ((curveType != CONTOUR) && (curveType != POLYLINE) && (curveType != POINT)) {
            return -1;
        }

        for (int i = 0; i < curves.size(); i++) {
            if (curves.elementAt(i).isActive() && curves.elementAt(i).slice() == slice) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Finds the position and intensity for this VOI if it's a line.
     *
     * @param   slice        slice where the line is located
     * @param   contourNo    the contour within the image slice
     * @param   position     position of the line (x-coordinates)
     * @param   intensity    intensity of the line (y-coordinates)
     * @param   imageBuffer  image buffer
     * @param   resolutions  image resolutions
     * @param   xDim         x dimension
     * @param   yDim         y dimension
     * @param   xyCoords     actual x,y coords of the boundary go in here if not null
     *
     * @return  number of valid points in the array
    public int findPositionAndIntensity(int slice, int contourNo, float[] position, float[] intensity,
            float[] imageBuffer, float[] resolutions, int xDim, int yDim, int[][] xyCoords) {

        if (curveType == LINE) {
            return ((VOILine) (curves.elementAt(contourNo))).findPositionAndIntensity(position, intensity,
                    imageBuffer, resolutions,
                    xDim, yDim,xyCoords);
        }

        if (curveType == PROTRACTOR) {
            return 0;
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            return ((VOIContour) (curves.elementAt(contourNo))).findPositionAndIntensity(position, intensity,
                    imageBuffer,
                    resolutions, xDim,
                    yDim,xyCoords);
        } else if (curveType == POINT) {
            return 0;
        } else {
            return 0;
        }
    }
     */

    /**
     * Accessor that returns the bounding box flag.
     *
     * @return  the process
     */
    public boolean getBoundingBoxFlag() {
        return boundingBox;
    }

    /**
     * Calculates the extents or boundary of the voi in x, y, and z.
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(float[] x, float[] y, float[] z) {

        Vector3f[] akBounds = new Vector3f[2];
        akBounds[0] = new Vector3f();
        akBounds[1] = new Vector3f();
        boolean bFirst = true;

        for ( int j = 0; j < curves.size(); j++ )
        {
            VOIBase kCurrentVOI = curves.get(j);
            Vector3f[] kBounds = kCurrentVOI.getImageBoundingBox();
            if ( bFirst )
            {
                bFirst = false;
                akBounds[0].Copy(kBounds[0]);
                akBounds[1].Copy(kBounds[1]);
            }
            akBounds[0].Min(kBounds[0]);
            akBounds[1].Max(kBounds[1]);
        }

        x[0] = akBounds[0].X;
        x[1] = akBounds[1].X;
        y[0] = akBounds[0].Y;
        y[1] = akBounds[1].Y;
        z[0] = akBounds[0].Z;
        z[1] = akBounds[1].Z;
    }

    /**
     * Calculates the extents or boundary of the voi in x, y, and z.
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(int[] x, int[] y, int[] z) {
        float[] fX = new float[]{0,0};
        float[] fY = new float[]{0,0};
        float[] fZ = new float[]{0,0};
        getBounds(fX,fY,fZ);
        x[0] = MipavMath.round(fX[0]);
        x[1] = MipavMath.round(fX[1]);
        y[0] = MipavMath.round(fY[0]);
        y[1] = MipavMath.round(fY[1]);
        z[0] = MipavMath.round(fZ[0]);
        z[1] = MipavMath.round(fZ[1]);
    }

    /**
     * Accessor that returns the VOI's color.
     *
     * @return  the color
     */
    public Color getColor() {
        return color;
    }

    /**
     * Returns the contour graph associated with this voi.
     *
     * @return  DOCUMENT ME!
     */
    public ViewJFrameGraph getContourGraph() {
        return contourGraph;
    }

    /**
     * Accessor that returns the curves making up the VOI.
     *
     * @return  the curves
     */
    public Vector<VOIBase> getCurves() {
        return curves;
    }
    
    /**
     * Accessor that returns the curves making up the VOI.
     *
     * @return  the curves
     */
    public Vector<VOIBase>[] getCurvesTemp() {
        Vector<VOIBase>[] kTemp = new Vector[1];
        kTemp[0] = curves;
        /*
        Vector<VOIBase>[] kTemp = new Vector[zDim];
        for ( int i = 0; i < curves.size(); i++ )
        {
            if ( (curves.get(i).getPlane() == VOIBase.ZPLANE) )
            {
                int slice = (int)curves.get(i).get(0).Z;
                if ( kTemp[slice] == null )
                {
                    kTemp[slice] = new Vector<VOIBase>();
                    kTemp[slice].add(curves.get(i) );
                }
            }
        }
        */
        return kTemp;
    }

    public Vector<VOIBase>[] getSortedCurves( int iPlane, int iDim ) {
        Vector<VOIBase>[] kTemp = new Vector[iDim];
        for ( int i = 0; i < iDim; i++ )
        {
            kTemp[i] = new Vector<VOIBase>();
        }      
                
        for ( int i = 0; i < curves.size(); i++ )
        {
            if ( (curves.elementAt(i).getPlane() & iPlane) == iPlane )
            {
                int slice = curves.elementAt(i).slice();
                kTemp[slice].add( curves.elementAt(i) );
            }
        }
        
        return kTemp;
    }

    /**
     * Caclulates the geometric center for all the contours on a particular slice. (This should find for seperate, but
     * grouped, contours as well, but this hasn't been proven!)
     *
     * @param   slice  DOCUMENT ME!
     *
     * @return  returns the geometric center
    public Vector3f getGeometricCenter(int slice) {
        int ncurves = 0;
        Vector3f tempPt = new Vector3f(0, 0, 0);
        float sumX = (float) 0.0;
        float sumY = (float) 0.0;
        float sumZ = (float) 0.0;

        // ignore 1D structures.
        if ((curveType == LINE) || (curveType == POLYLINE) || (curveType == PROTRACTOR)) {
            return null;
        }

        // this is a 2D structure and so long as it has curves, calculate.
        for (int i = 0; i < curves.size(); i++) {
            if ( curves.elementAt(i).slice() == slice ) {
                ncurves++;
                tempPt = ((VOIContour) (curves.elementAt(i))).getGeometricCenter();
                sumX += tempPt.X;
                sumY += tempPt.Y;
                sumZ += tempPt.Z;
            }
        }

        return (new Vector3f(sumX / ncurves, sumY / ncurves, sumZ / ncurves));
    }
     */


    /**
     * Accessor that returns the curve type.
     *
     * @return  the curve type
     */
    public int getCurveType() {
        return curveType;
    }

    /**
     * Accessor that returns the display mode.
     *
     * @return  the display mode
     */
    public int getDisplayMode() {
        return displayMode;
    }

    /**
     * @return
     */
    public String getExtension() {
        return extension;
    }

    /**
     * Returns the geometric center of the VOI (only contour).
     *
     * @return  returns the geometric center
     */
    public Vector3f getGeometricCenter() {
        int ncurves = 0;
        Vector3f tempPt = new Vector3f(0, 0, 0);
        float sumX = (float) 0.0;
        float sumY = (float) 0.0;
        float sumZ = (float) 0.0;

        if ((curveType == LINE) || (curveType == POLYLINE) || (curveType == PROTRACTOR)) {
            return null;
        }

        for (int i = 0; i < curves.size(); i++) {
            tempPt = ((VOIContour) (curves.elementAt(i))).getGeometricCenter();
            ncurves++;
            sumX += tempPt.X;
            sumY += tempPt.Y;
            sumZ += tempPt.Z;
        }
        return (new Vector3f(sumX / ncurves, sumY / ncurves, sumZ / ncurves));
    }

    /**
     * Accessor that returns the ID.
     *
     * @return  the ID
     */
    public short getID() {
        return ID;
    }

    /**
     * Accessor that returns the maximum of the range of intensities to ignore.
     *
     * @return  The maximum.
     */
    public float getMaximumIgnore() {
        return ignoreMax;
    }

    /**
     * Accessor that returns the intensity array to the parameter.
     *
     * @return  DOCUMENT ME!
    public float[] getIntensity() {
        return intensity;
    }
     */

    /**
     * Accessor that returns the level of the levelset VOI.
     *
     * @return  The level
    public float getLevel() {
        return level;
    }
     */

    /**
     * Accessor that returns the minimum of the range of intensities to ignore.
     *
     * @return  The minimum.
     */
    public float getMinimumIgnore() {
        return ignoreMin;
    }

    /**
     * Accessor that returns the name of the VOI.
     *
     * @return  the name
     */
    public String getName() {
        return name;
    }

    /**
     * Get the number of points in the VOI file.
     *
     * @return  numPoints number of points
     */
    public int getNumPoints() {
        if (curveType != POINT) {
            return -1;
        }
        return curves.size();
    }

    /**
     * Accessor that returns the opacity of the VOI.
     *
     * @return  the opacity
     */
    public float getOpacity() {
        return opacity;
    }

    /**
     * This method is used to get the coordinates of a single VOIPoint.
     *
     * @param  coord  this float array contains the x-coordinate in the first element and the y coordinate in the second
     *                element
     */
    public void getPointCoordinates(float[] coord) {
        for (int i = 0; i < curves.size(); i++) {
            ((VOIPoint) (curves.elementAt(i))).getCoordinates(coord);
        }
    }

    /**
     * Accessor that returns.
     *
     * @return  the polarity
     */
    public int getPolarity() {
        return polarity;
    }

    /**
     * Gets the position and intensity for this VOI if it's a line.
     *
     * @param   slice        slice where the line is located
     * @param   contourNo    the contour within the image slice
     * @param   position     position of the line (x-coordinates)
     * @param   intensity    intensity of the line (y-coordinates)
     * @param   imageBuffer  image buffer
     * @param   xDim         x dimension
     *
     * @return  DOCUMENT ME!
     */
    public int getPositionAndIntensity(int slice, int contourNo, Vector3f[] position, float[] intensity,
            float[] imageBuffer, int xDim) {

        for (int i = 0; i < position.length; i++) {
            position[i] = new Vector3f();
            position[i].Z = slice;
        }

        if (curveType == LINE) {
            return ((VOILine) (curves.elementAt(0))).getPositionAndIntensity(position, intensity, imageBuffer,
                    xDim);
        } else if (curveType == PROTRACTOR) {
            return 0;
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            return ((VOIContour) (curves.elementAt(contourNo))).getPositionAndIntensity(position, intensity,
                    imageBuffer, xDim);
        } else if (curveType == POINT) {
            return 0;
        } else {
            return 0;
        }
    }

    /**
     * Accessor that returns the position array to the parameter.
     *
     * @return  DOCUMENT ME!
    public float[] getPosition() {
        return position;
    }
     */

    /**
     * Accessor that returns the process.
     *
     * @return  the process
     */
    public boolean getProcess() {
        return process;
    }

    /**
     * Gets the position and intensity for this VOI if it's a line.
     *
     * @param   slice        slice where the line is located
     * @param   contourNo    the contour within the image slice
     * @param   position     position of the line (x-coordinates)
     * @param   intensity    intensity of the line (y-coordinates)
     * @param   imageBuffer  image buffer
     * @param   xDim         x dimension
     *
     * @return  DOCUMENT ME!
    public int getPositionAndIntensityIndex(int slice, int contourNo, int[] position, float[] intensity,
            float[] imageBuffer, int xDim) {

        if (curveType == LINE) {
            return ((VOILine) (curves.elementAt(0))).getPositionAndIntensityIndex(position, intensity,
                    imageBuffer, xDim);
        }

        if (curveType == PROTRACTOR) {
            return 0;
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            return ((VOIContour) (curves.elementAt(contourNo))).getPositionAndIntensityIndex(position, intensity,
                    imageBuffer, xDim);
        } else if (curveType == POINT) {
            return 0;
        } else {
            return 0;
        }
    }
     */

    /**
     * Accessor that returns the statistic list used to indicate which statistics are to be calculated for the VOI.
     *
     * @return  statistics list
     */
    public ViewList[] getStatisticList() {
        return stats;
    }

    /**
     * Accessor that returns the rgb intensity array to the parameter.
     *
     * @return  DOCUMENT ME!
    public float[][] getRGBIntensities() {
        return rgbIntensities;
    }
     */

    /**
     * Accessor that returns the rgb position array to the parameter.
     *
     * @return  DOCUMENT ME!
    public float[][] getRGBPositions() {
        return rgbPositions;
    }
     */

    /**
     * Returns the thickness of the VOI
     * @return
     */
    public int getThickness() {
        return this.thickness;
    }

    /**
     * Accessor that returns the Unique ID (original hash code for object).
     *
     * @return  the unique ID
     */
    public int getUID() {
        return UID;
    }

    /**
     * Returns the number of z slices actually containing part of the voi
     * @return
     */
    public int getVOISlices() {
        int[] iX = new int[]{0,0};
        int[] iY = new int[]{0,0};
        int[] iZ = new int[]{0,0};
        getBounds(iX,iY,iZ);
        return (iZ[1] - iZ[0] + 1);
    }

    /**
     * getWatershedID - accessor that returns the watershedID.
     *
     * @return  the ID
     */
    public short getWatershedID() {
        return watershedID;
    }

    /**
     * Accessor that returns number of slices.
     * 
     * @return
     */
    public int getZDim() {
        return 1;
    }

    public VOIBase importCurve(float[] x, float[] y, float[] z) {
        VOIBase curve;

        if (curveType == LINE) {
            curve = new VOILine();
        } else if (curveType == CONTOUR) {
            curve = new VOIContour(true);
        } else if (curveType == POLYLINE) {
            curve = new VOIContour(false);
        } else if (curveType == POINT) {
            curve = new VOIPoint();
        } else if (curveType == PROTRACTOR) {
            curve = new VOIProtractor();
        } else if (curveType == ANNOTATION) {
            curve = new VOIText();
        } else if (curveType == POLYLINE_SLICE) {
            curve = new VOIPolyLineSlice();
        } else {
            return null;
        }

        curve.importArrays(x, y, z, x.length);
        curve.setGroup(this);
        curves.addElement(curve);

        if ((curveType == POINT) || (curveType == POLYLINE_SLICE)) {
            ((VOIPoint) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));

            if (curveType == POLYLINE_SLICE) {

                try {
                    activePolylineSlicePoint = Integer.parseInt(((VOIPoint) (curves.lastElement())).getLabel()) -
                    2;
                } catch (Exception e) {
                    activePolylineSlicePoint = 0;
                }

                polygonIndex = curves.size() - 2;
            }
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        }
        return curve;
    }

    /**
     * Imports the curve into the VOI, testing for which type.
     *
     * @param  x      array of x coordinates to import
     * @param  y      array of y coordinates to import
     * @param  z      array of z coordinates to import
     * @param  slice  index of slice of curve
     * @deprecated
     */
    public void importCurve(float[] x, float[] y, float[] z, int slice) {
        VOIBase curve;

        if (curveType == LINE) {
            curve = new VOILine();
        } else if (curveType == CONTOUR) {
            curve = new VOIContour(true);
        } else if (curveType == POLYLINE) {
            curve = new VOIContour(false);
        } else if (curveType == POINT) {
            curve = new VOIPoint();
        } else if (curveType == PROTRACTOR) {
            curve = new VOIProtractor();
        } else if (curveType == ANNOTATION) {
            curve = new VOIText();
        } else if (curveType == POLYLINE_SLICE) {
            curve = new VOIPolyLineSlice();
        } else {
            return;
        }

        curve.importArrays(x, y, z, x.length);
        curve.setGroup(this);
        curves.addElement(curve);

        if ((curveType == POINT) || (curveType == POLYLINE_SLICE)) {
            ((VOIPoint) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));

            if (curveType == POLYLINE_SLICE) {

                try {
                    activePolylineSlicePoint = Integer.parseInt(((VOIPoint) (curves.lastElement())).getLabel()) -
                    2;
                } catch (Exception e) {
                    activePolylineSlicePoint = 0;
                }

                polygonIndex = curves.size() - 2;
            }
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    public void importCurve(int[] x, int[] y, int[] z) {
        VOIBase curve;

        if (curveType == LINE) {
            curve = new VOILine();
        } else if (curveType == CONTOUR) {
            curve = new VOIContour(true);
        } else if (curveType == POLYLINE) {
            curve = new VOIContour(false);
        } else if (curveType == POINT) {
            curve = new VOIPoint();
        } else if (curveType == PROTRACTOR) {
            curve = new VOIProtractor();
        } else {
            return;
        }

        curve.importArrays(x, y, z, x.length);
        curve.setGroup(this);
        curves.addElement(curve);

        if (curveType == POINT) {
            ((VOIPoint) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    /**
     * Imports the curve into the VOI, testing for which type.
     *
     * @param  x      array of x coordinates to import
     * @param  y      array of y coordinates to import
     * @param  z      array of z coordinates to import
     * @param  slice  index of slice of curve
     * @deprecated
     */
    public void importCurve(int[] x, int[] y, int[] z, int slice) {
        VOIBase curve;

        if (curveType == LINE) {
            curve = new VOILine();
        } else if (curveType == CONTOUR) {
            curve = new VOIContour(true);
        } else if (curveType == POLYLINE) {
            curve = new VOIContour(false);
        } else if (curveType == POINT) {
            curve = new VOIPoint();
        } else if (curveType == PROTRACTOR) {
            curve = new VOIProtractor();
        } else {
            return;
        }

        curve.importArrays(x, y, z, x.length);
        curve.setGroup(this);
        curves.addElement(curve);

        if (curveType == POINT) {
            ((VOIPoint) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }


    /**
     * Imports the curve into the VOI, testing for which type.
     *
     * @param  pt     array of 3D points to import
     * @param  slice  index of slice of curve
     */
    public void importCurve(Vector3f[] pt) {
        VOIBase curve;

        if (curveType == LINE) {
            curve = new VOILine();
        } else if (curveType == CONTOUR) {
            curve = new VOIContour(true);
        } else if (curveType == POLYLINE) {
            curve = new VOIContour(false);
        } else if (curveType == POINT) {
            curve = new VOIPoint();
        } else if (curveType == PROTRACTOR) {
            curve = new VOIProtractor();
        } else {
            return;
        }

        curve.importPoints(pt);
        curve.setGroup(this);
        curves.addElement(curve);

        if (curveType == POINT) {
            ((VOIPoint) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    /**
     * Imports the curve into the VOI.
     *
     * @param  curve  curve to import
     * @param  slice  index of slice of curve
     */
    public void importCurve(VOIBase curve) {
        curve.setGroup(this);
        curves.addElement(curve.clone());

        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    /**
     * Imports the curve into the VOI.
     *
     * @param  curve  curve to import
     * @param  slice  index of slice of curve
     * @deprecated
     */
    public void importCurve(VOIContour curve, int slice) {
        curve.setGroup(this);
        curves.addElement(curve.clone());

        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    /**
     * Imports just the VOIs in a slice into this VOI.
     *
     * @param  slice     slice indicates the slice where the contour(s) is to be located
     * @param  voiSlice  voiSlice indicates the slice where the contour(s) is to be copied from
     * @param  voi       added to VOI
     * @param  newZDim   indicates the new Z dimenions to be set
     * @deprecated
     */
    public void importNewVOI(int slice, int voiSlice, VOI voi, int newZDim, boolean resize) {
        /*
        zDim = newZDim;

        float[] x, y, z;
        int n = 2;

        try {

            if (curveType == PROTRACTOR) {
                n = 3;
            } else if (curveType == LINE) {
                n = 2;
            }

            x = new float[n];
            y = new float[n];
            z = new float[n];

            if (resize) {
                curves = new Vector[zDim];

                for (int i = 0; i < zDim; i++) {
                    curves[i] = new Vector();
                }

            }

            elementLabel = 1;
        } catch (OutOfMemoryError e) {
            System.gc();
            throw e;
        }

        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            Polygon[] gons = voi.exportPolygons(voiSlice);

            for (int i = 0; i < gons.length; i++) {
                importPolygon(gons[i], slice);
            }
        } else if (curveType == POINT) {

            Vector3f [] pts = voi.exportPoints(voiSlice);
            for (int i = 0; i < pts.length; i++) {
                System.err.println("i: " + i + ", pt: " + pts[i]);
            }

            importPoints(pts, slice);
        } else if (curveType == ANNOTATION) {
            if (isEmpty()) {
                curves[slice].addElement(voi.getCurves()[voiSlice].elementAt(0));
            }
        } else if (curveType == LINE) {
            voi.exportArrays(x, y, z, voiSlice);
            importCurve(x, y, z, slice);
        } else if (curveType == PROTRACTOR) {
            voi.exportArrays(x, y, z, voiSlice);
            importCurve(x, y, z, slice);
        }
        */
    }


    /**
     * @param point
     */
    public void importPoint(Vector3f point) {
        VOIPoint voiPt = null;

        if (curveType == POINT) {
            voiPt = new VOIPoint();
        } else {
            return;
        }

        voiPt.set(0, point);
        voiPt.setGroup(this);
        curves.addElement(voiPt);
        ((VOIPoint) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
    }


    /**
     * Imports the polygon into the VOI (must be a contour).
     *
     * @param  gon    polygon to import
     * @param  slice  index of slice of new polygon
     */
    public void importPolygon(Polygon gon, int slice) {
        VOIContour contour = null;

        if (curveType == CONTOUR) {
            contour = new VOIContour(true);
        } else if (curveType == POLYLINE) {
            contour = new VOIContour(false);
        } else {
            return;
        }

        contour.importPolygon(gon, slice);
        contour.setGroup(this);
        curves.addElement(contour);
        ((VOIContour) (curves.lastElement())).setLabel(String.valueOf(elementLabel++));
    }


    /**
     * Accessor that tells if the VOI is active.
     *
     * @return  boolean active
     */
    public boolean isActive() {
        return active;
    }

    /**
     * Returns true iff all contours in this VOI are active.
     * @return true iff all contours in this VOI are active.
     */
    public boolean isAllActive() {
        if ( !active )
            return false;

        boolean allActive = true;
        for (int i = 0; i < curves.size(); i++) {
            allActive &= (curves.elementAt(i)).isActive();
        }
        return allActive;
    }




    /**
     * Test whether or not the VOI is empty.
     *
     * @return  boolean result of test
     */
    public boolean isEmpty() {
        return curves.isEmpty();
    }

    /**
     * Accessor that tells if VOI is fixed.
     *
     * @return  boolean fixed
     */
    public boolean isFixed() {
        return fixed;
    }

    /**
     * Determines if the given BitSet binary mask is true for all points that are within the VOI.
     *
     * @param   mask  BitSet
     * @param   xDim  x dimension, used to index into the image
     * @param   yDim  y dimension, used to index into the image
     *
     * @return  boolean does the mask the VOI area
    public boolean isBinaryMaskContained(BitSet mask, int xDim, int yDim) {
        int z;

        for (z = 0; z < zDim; z++) {

            if (!isBinaryMaskContained(xDim, yDim, z, mask)) {
                return false;
            }
        }

        return true;
    }
     */

    /**
     * Accessor that tells if the VOI is visible.
     *
     * @return  boolean visible
     */
    public boolean isVisible() {
        return visible;
    }

    /**
     * Finds 2 points that form the maximum width of the VOI.
     *
     * @return  returns the two points.
     */
    public Vector3f[] maxWidth( boolean bOnlyActive ) {

        if (curveType != CONTOUR) {
            return null;
        }
        
        int i;
        int j, conSize;
        double[] maxDistance = new double[1];
        Vector3f[] points = new Vector3f[2];
        Vector3f point;

        for (i = 0; i < curves.size(); i++) {
            if ( curves.elementAt(i).isActive() || !bOnlyActive )
            {
                conSize = ((VOIContour) (curves.elementAt(i))).size();

                for (j = 0; j < conSize; j++) {
                    point = findMaxWidth(((VOIContour) (curves.elementAt(i))).elementAt(j),
                            maxDistance, bOnlyActive);

                    if (point != null) {
                        points[0] = ((VOIContour) (curves.elementAt(i))).elementAt(j);
                        points[1] = point;
                    }
                }
            }
        }

        return points;
    }

    /**
     * Move VOI to a new position.
     *
     * @param  slice  slice where the VOI is located
     * @param  xDim   x dimension maximum
     * @param  yDim   y dimension maximum
     * @param  zDim   z dimension maximum
     * @param  xM     amount in pixels to move the line in the x direction
     * @param  yM     amount in pixels to move the line in the y direction
     * @param  zM     amount in pixels to move the line in the z direction
     */
    public void moveVOI(int slice, int xDim, int yDim, int zDim, int xM, int yM, int zM) {
        if (fixed || curveType != POINT) {
            return;
        }
        if (slice == -1) {
            getBounds(xBounds, yBounds, zBounds);

            if (((xBounds[0] + xM) >= xDim) || ((xBounds[0] + xM) < 0)) {
                return;
            }

            if (((xBounds[1] + xM) >= xDim) || ((xBounds[1] + xM) < 0)) {
                return;
            }

            if (((yBounds[0] + yM) >= yDim) || ((yBounds[0] + yM) < 0)) {
                return;
            }

            if (((yBounds[1] + yM) >= yDim) || ((yBounds[1] + yM) < 0)) {
                return;
            }

            for (int j = 0; j < curves.size(); j++) {
                ((VOIPoint) (curves.elementAt(j))).moveVOIPoint(xM, yM, zM, xDim, yDim, zDim);
            }
        } else {

            for (int i = 0; i < curves.size(); i++) {
                ((VOIPoint) (curves.elementAt(i))).moveVOIPoint(xM, yM, zM, xDim, yDim, zDim);
            }
        }
    }

    /**
     * Tests if a point is near a line.
     *
     * @param   x      x coordinate of line
     * @param   y      y coordinate of line
     * @param   slice  index of slice in curve array
     *
     * @return  result of test
     */
    public boolean nearLine(int x, int y, int slice) {

        if (isEmpty()) {
            return false;
        }
        for (int i = 0; i < curves.size(); i++) {
            if (((VOIContour) (curves.elementAt(i))).nearLine(x, y, slice, 3)) {
                polygonIndex = i;

                return true;
            }
        }
        polygonIndex = -99;
        return false;
    }

    /**
     * Tests if a point is near an outer point of a protractor.
     *
     * @param   x            x coordinate of point
     * @param   y            y coordinate of point
     * @param   slice        index of slice in curve array
     * @param   element      element of slice
     * @param   zoom         magnification of image
     * @param   resolutionX  X resolution (aspect ratio)
     * @param   resolutionY  Y resolution (aspect ratio)
     *
     * @return  result of test
     * @deprecated
    public boolean nearOuterPoint(int x, int y, int slice, int element, float zoom, float resolutionX,
            float resolutionY) {

        if (isEmpty()) {
            return false;
        }

        if ((curveType == PROTRACTOR) && (curves.size() > 0)) {

            if (((VOIProtractor) (curves.elementAt(element))).isActive() &&
                    ((VOIProtractor) (curves.elementAt(element))).nearOuterPoint(x, y, zoom, resolutionX,
                            resolutionY)) {
                polygonIndex = element;

                return true;
            }
        }

        polygonIndex = -99;
        resizeIndex = -99;

        return false;
    }

     */
    /**
     * DOCUMENT ME!
     *
     * @param   x      int
     * @param   y      int
     * @param   slice  int
     *
     * @return  boolean
    public boolean nearCardioLine(int x, int y, int slice) {

        if (curveType != CARDIOLOGY) {
            return false;
        }

        return ((VOICardiology) curves[slice].elementAt(0)).nearCardioLine(x, y);
    }
     */

    /**
     * DOCUMENT ME!
     *
     * @param   x            int
     * @param   y            int
     * @param   slice        int
     * @param   zoom         float
     * @param   resolutionX  float
     * @param   resolutionY  float
     *
     * @return  int
    public int nearCardioPoint(int x, int y, int slice, float zoom, float resolutionX, float resolutionY) {

        if (curveType != CARDIOLOGY) {
            return VOIBase.NOT_A_POINT;
        }

        return ((VOICardiology) curves[slice].elementAt(0)).nearCardioPoint(x, y, zoom, resolutionX, resolutionY);
    }
     */

    /**
     * Tests if a point is near a point.
     *
     * @param   x            x coordinate of point
     * @param   y            y coordinate of point
     * @param   slice        index of slice in curve array
     * @param   zoom         magnification of image
     * @param   resolutionX  X resolution (aspect ratio)
     * @param   resolutionY  Y resolution (aspect ratio)
     *
     * @return  result of test
     */
    public boolean nearPoint(int x, int y, int slice, float zoom, float resolutionX, float resolutionY) {

        if (isEmpty()) {
            return false;
        }
        if ( curveType != POINT )
        {
            return false;
        }

        for (int i = 0; i < curves.size(); i++)
        {
            if (((VOIPoint) (curves.elementAt(i))).nearPoint(x, y, slice)) {
                polygonIndex = i;

                return true;
            }
        }

        polygonIndex = -99;
        resizeIndex = -99;

        return false;
    }


    /**
     * Finds the minimum distance from a point to a contour. If the point is in a contour, the distance is given a
     * negative sign. If the point is outside contours, the distance is given a positive sign. The contour points are
     * connected by straight line segments between the points so simply find the distances from the point to each line
     * segment comprising the contours.
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     * @param   z  DOCUMENT ME!
     *
     * @return  minDistance
     */
    public float pointToContour(int x, int y, int z) {
        float minDistance = Float.MAX_VALUE;
        float distance;
        boolean insideContour = true;
        for (int i = 0; i < curves.size(); i++) {
            distance = curves.elementAt(i).distanceToVOI(x,y,z);
            if ( distance < minDistance )
            {
                minDistance = distance;
                insideContour = curves.elementAt(i).contains(x,y,z);
            }
        }
        if (insideContour) {
            minDistance = -minDistance;
        }
        return minDistance;
    }

    /**
     * Clears VOI of a curve or point (VOIPoint)at an index in a slice.
     *
     * @param  index  slice of curve
     * @param  slice  index of slice of curve
     * @deprecated
     */
    public void removeCurve(int index, int slice) {
        short pointID, tmpID, contourID;
        int i, nCurves;

        if (curveType == POINT) {

            if (((VOIPoint) curves.elementAt(index)).getLabel() != null) {
                pointID = Short.valueOf(((VOIPoint) curves.elementAt(index)).getLabel()).shortValue();
                curves.removeElementAt(index);

                nCurves = curves.size();

                    for (i = 0; i < nCurves; i++) {
                        tmpID = Short.valueOf(((VOIPoint) curves.elementAt(i)).getLabel()).shortValue();

                        if (tmpID > pointID) {
                            ((VOIPoint) (curves.elementAt(i))).setLabel(String.valueOf(tmpID - 1));
                        }
                    }

                elementLabel--;

                if ((contourGraph != null) && (elementLabel == 1)) {
                    contourGraph.deleteFunct((elementLabel) - 1);
                }
            } else {
                curves.removeElementAt(index);
            }
        } else if (curveType == ANNOTATION) {
            curves.removeAllElements();
        } else {

            if (curveType == CONTOUR) {

                if (((VOIContour) curves.elementAt(index)).getLabel() != null) {
                    contourID = Short.valueOf(((VOIContour) curves.elementAt(index)).getLabel()).shortValue();
                    curves.removeElementAt(index);

                    nCurves = curves.size();

                        for (i = 0; i < nCurves; i++) {

                            try {
                                tmpID = Short.valueOf(((VOIContour) curves.elementAt(i)).getLabel()).shortValue();

                                if (tmpID > contourID) {
                                    ((VOIContour) (curves.elementAt(i))).setLabel(String.valueOf(tmpID - 1));
                                }
                            } catch (NumberFormatException nfe) {
                                // in case a VOI doesn't have a label, we don't want this thing choking
                            }
                        }

                    elementLabel--;

                    return;
                }
            }

            curves.removeElementAt(index);
        }
    }

    /**
     * Tests if a point is near a point.
     *
     * @param   x            x coordinate of point
     * @param   y            y coordinate of point
     * @param   slice        index of slice in curve array
     * @param   element      element of slice
     * @param   zoom         magnification of image
     * @param   resolutionX  X resolution (aspect ratio)
     * @param   resolutionY  Y resolution (aspect ratio)
     *
     * @return  result of test
     * @deprecated
    public boolean nearPoint(int x, int y, int slice, int element, float zoom, float resolutionX, float resolutionY) {

        if (isEmpty()) {
            return false;
        }

        if (curveType == LINE) {

            if (((VOILine) (curves.elementAt(element))).isActive() &&
                    ((VOILine) (curves.elementAt(element))).nearPoint(x, y, slice)) {
                polygonIndex = element;

                return true;
            }
        } else if (curveType == PROTRACTOR) {

            if (((VOIProtractor) (curves.elementAt(element))).isActive() &&
                    ((VOIProtractor) (curves.elementAt(element))).nearPoint(x, y, slice)) {
                polygonIndex = element;

                return true;
            }
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.elementAt(element))).getBounds(xBounds, yBounds, zBounds);
            ((VOIContour) (curves.elementAt(element))).setBounds(xBounds, yBounds, zBounds);

            if (((VOIContour) (curves.elementAt(element))).isActive() && (boundingBox == true)) {
                nearBoundPoint = ((VOIContour) (curves.elementAt(element))).nearBoundPoint(x, y, zoom,
                        resolutionX,
                        resolutionY);

                if (nearBoundPoint != NOT_A_POINT) {
                    polygonIndex = -99;
                    resizeIndex = element;

                    return true;
                }
            }

            if (((VOIContour) (curves.elementAt(element))).isActive() &&
                    ((VOIContour) (curves.elementAt(element))).nearPoint(x, y, slice)) {
                polygonIndex = element;
                resizeIndex = -99;

                return true;
            }
        } else if (curveType == VOI.POINT) {

            if (((VOIPoint) (curves.elementAt(element))).nearPoint(x, y, slice)) {
                polygonIndex = element;

                return true;
            }
        }

        polygonIndex = -99;
        resizeIndex = -99;

        return false;
    }
     */

    /**
     * Clears VOI of all curves at a slice.
     *
     * @param  slice  index of slice of curves to remove
     * @deprecated
     */
    public void removeCurves(int slice) {
        curves.removeAllElements();
        elementLabel = 1;
    }
    
    /**
     * Clears VOI of all curves at a slice.
     *
     * @param  slice  index of slice of curves to remove
     */
    public void removeCurve(VOIBase kCurve) {
        curves.removeElement(kCurve);
    }
    
    /**
     * Clears VOI of all curves at a slice.
     *
     * @param  slice  index of slice of curves to remove
     */
    public void removeCurves() {
        curves.removeAllElements();
        elementLabel = 1;
    }



    /**
     * removes the update listener.
     *
     * @param  listener  DOCUMENT ME!
     */
    public void removeVOIListener(VOIListener listener) {

        if (listenerList == null) {
            listenerList = new EventListenerList();
        }

        listenerList.remove(VOIListener.class, listener);
    }

    /**
     * Draws a rubberband around the VOI (only contour).
     *
     * @param  x        x coordinate
     * @param  y        y coordinate
     * @param  slice    index of slice in curve array
     * @param  xDim     DOCUMENT ME!
     * @param  yDim     DOCUMENT ME!
     * @param  doRound  round point coordinates to integers
     * @deprecated
    public void rubberbandVOI(int x, int y, int slice, int xDim, int yDim, boolean doRound) {
        float oldXMin, oldXMax, oldYMin, oldYMax;
        float newXMin, newXMax, newYMin, newYMax;
        float xCenter, yCenter;
        float translateX, translateY, scaleX, scaleY;
        boolean change = true;

        if (fixed) {
            return;
        }

        if ((polygonIndex >= 0) && (polygonIndex < curves.size())) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

                // System.err.println("x: " + x + "y: " + y);
                ((VOIContour) (curves.elementAt(polygonIndex))).movePt(x, y);
                ((VOIContour) (curves.elementAt(polygonIndex))).reloadPoints();
            } else if (curveType == LINE) {
                ((VOILine) (curves.elementAt(polygonIndex))).movePt(x, y, slice, xDim, yDim);
            } else if (curveType == PROTRACTOR) {
                ((VOIProtractor) (curves.elementAt(polygonIndex))).movePt(x, y);
                //} else if (curveType == CARDIOLOGY) {
                //    ((VOICardiology) (curves[slice].elementAt(0))).movePt(x, y);
            }
        } else if ((resizeIndex >= 0) && (resizeIndex < curves.size())) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                ((VOIContour) (curves.elementAt(resizeIndex))).getBounds(xBounds, yBounds, zBounds);
                oldXMin = xBounds[0];
                oldXMax = xBounds[1];
                oldYMin = yBounds[0];
                oldYMax = yBounds[1];
                newXMin = xBounds[0];
                newXMax = xBounds[1];
                newYMin = yBounds[0];
                newYMax = yBounds[1];

                if ((nearBoundPoint == 1) || (nearBoundPoint == 4) || (nearBoundPoint == 8)) {

                    if (x < newXMax) {
                        newXMin = x;
                    } else {
                        change = false;
                    }
                }

                if ((nearBoundPoint == 2) || (nearBoundPoint == 3) || (nearBoundPoint == 6)) {

                    if (x > newXMin) {
                        newXMax = x;
                    } else {
                        change = false;
                    }
                }

                if ((nearBoundPoint == 1) || (nearBoundPoint == 2) || (nearBoundPoint == 5)) {

                    if (y < newYMax) {
                        newYMin = y;
                    } else {
                        change = false;
                    }
                }

                if ((nearBoundPoint == 3) || (nearBoundPoint == 4) || (nearBoundPoint == 7)) {

                    if (y > newYMin) {
                        newYMax = y;
                    } else {
                        change = false;
                    }
                }

                if (change) {

                    // translateX = (newXMin + newXMax - oldXMin - oldXMax) / 2.0f;
                    // translateY = (newYMin + newYMax - oldYMin - oldYMax) / 2.0f;
                    // System.out.println("This is a test = " + translateX + " translateY = " + translateY);
                    scaleX = (newXMax - newXMin) / (oldXMax - oldXMin);
                    scaleY = (newYMax - newYMin) / (oldYMax - oldYMin);

                    // If it is a corner lets keep aspect ratio
                    if ((nearBoundPoint == 1) || (nearBoundPoint == 2) || (nearBoundPoint == 3) ||
                            (nearBoundPoint == 4)) {

                        if (scaleX < scaleY) {
                            scaleX = scaleY;
                        } else {
                            scaleY = scaleX;
                        }
                    }

                    translateX = 0;
                    translateY = 0;

                    // Prevent the bounding box from expanding past the
                    // bounds of the image.
                    xCenter = (oldXMax + oldXMin) / 2.0f;
                    yCenter = (oldYMax + oldYMin) / 2.0f;
                    newXMax = (scaleX * oldXMax) + (xCenter * (1 - scaleX));

                    if (newXMax > (xDim - 1)) {
                        return;
                    }

                    newXMin = (scaleX * oldXMin) + (xCenter * (1 - scaleX));

                    if (newXMin < 0.0f) {
                        return;
                    }

                    newYMax = (scaleY * oldYMax) + (yCenter * (1 - scaleY));

                    if (newYMax > (yDim - 1)) {
                        return;
                    }

                    newYMin = (scaleY * oldYMin) + (yCenter * (1 - scaleY));

                    if (newYMin < 0.0f) {
                        return;
                    }

                    transformVOI(0.0f, 0.0f, 0.0f, translateX, translateY, 0.0f, scaleX, scaleY, 1.0f, slice,
                            resizeIndex, doRound);
                    ((VOIContour) (curves.elementAt(resizeIndex))).contains(-1, -1, true);
                } // end of if (change)
            }
        }
    }

     */
    
    /**
     * Sets whether or not the VOI is active.
     *
     * @param  act  boolean to set active to
     */
    public void setActive(boolean act) {
        this.active = act;
        //fireVOIselection();
    }

    /**
     * Sets all contours in the VOI as active or inactive.
     *
     * @param  flag  boolean to set VOI active or inactive
     */
    public void setAllActive(boolean flag) {
        int i;
        this.active = flag;

        for (i = 0; i < curves.size(); i++) {
            (curves.elementAt(i)).setActive(flag);
        }
    }

    /**
     * RubberBand (move pt) function created for POLYLINE/CONTOUR so that the screen does not update all points as a
     * single point is drawn.
     *
     * @param  x            int x coordinate
     * @param  y            int y coordinate
     * @param  slice        int slice of image
     * @param  xDim         int x dimension of image
     * @param  yDim         int y dimension of image
     * @param  doRound      boolean whether or not to use rounding
     * @param  zoomX        float current X zoom level
     * @param  zoomY        float current Y zoom level
     * @param  resolutionX  float X-aspect ratio
     * @param  resolutionY  float Y-aspect ratio
     * @param  g            Graphics the graphics for drawing
     * @deprecated
    public void rubberbandVOI(int x, int y, int slice, int xDim, int yDim, boolean doRound, float zoomX, float zoomY,
            float resolutionX, float resolutionY, Graphics g) {
        float oldXMin, oldXMax, oldYMin, oldYMax;
        float newXMin, newXMax, newYMin, newYMax;
        float xCenter, yCenter;
        float translateX, translateY, scaleX, scaleY;
        boolean change = true;

        if (fixed) {
            return;
        }

        if ((polygonIndex >= 0) && (polygonIndex < curves.size())) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                ((VOIContour) (curves.elementAt(polygonIndex))).movePt(x, y);
                ((VOIContour) (curves.elementAt(polygonIndex))).reloadPoints();
            } else if (curveType == LINE) {
                ((VOILine) (curves.elementAt(polygonIndex))).movePt(x, y, slice, xDim, yDim);
            } else if (curveType == PROTRACTOR) {
                ((VOIProtractor) (curves.elementAt(polygonIndex))).movePt(x, y);
                //} else if (curveType == CARDIOLOGY) {
                //    ((VOICardiology) (curves[slice].elementAt(0))).movePt(x, y);
            } else if (curveType == POLYLINE_SLICE) {
                ((VOIPoint) (curves.elementAt(polygonIndex))).movePt(x, y);
                //this.markPSlicePt(slice);
            } else if (curveType == ANNOTATION) {
                ((VOIText) (curves.elementAt(polygonIndex))).moveMarkerPoint(x, y, slice, xDim, yDim, zDim);
            }
        } else if ((resizeIndex >= 0) && (resizeIndex < curves.size())) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                ((VOIContour) (curves.elementAt(resizeIndex))).getBounds(xBounds, yBounds, zBounds);
                oldXMin = xBounds[0];
                oldXMax = xBounds[1];
                oldYMin = yBounds[0];
                oldYMax = yBounds[1];
                newXMin = xBounds[0];
                newXMax = xBounds[1];
                newYMin = yBounds[0];
                newYMax = yBounds[1];

                if ((nearBoundPoint == 1) || (nearBoundPoint == 4) || (nearBoundPoint == 8)) {

                    if (x < newXMax) {
                        newXMin = x;
                    } else {
                        change = false;
                    }
                }

                if ((nearBoundPoint == 2) || (nearBoundPoint == 3) || (nearBoundPoint == 6)) {

                    if (x > newXMin) {
                        newXMax = x;
                    } else {
                        change = false;
                    }
                }

                if ((nearBoundPoint == 1) || (nearBoundPoint == 2) || (nearBoundPoint == 5)) {

                    if (y < newYMax) {
                        newYMin = y;
                    } else {
                        change = false;
                    }
                }

                if ((nearBoundPoint == 3) || (nearBoundPoint == 4) || (nearBoundPoint == 7)) {

                    if (y > newYMin) {
                        newYMax = y;
                    } else {
                        change = false;
                    }
                }

                if (change) {

                    // translateX = (newXMin + newXMax - oldXMin - oldXMax) / 2.0f;
                    // translateY = (newYMin + newYMax - oldYMin - oldYMax) / 2.0f;
                    // System.out.println("This is a test = " + translateX + " translateY = " + translateY);
                    scaleX = (newXMax - newXMin) / (oldXMax - oldXMin);
                    scaleY = (newYMax - newYMin) / (oldYMax - oldYMin);

                    // If it is a corner lets keep aspect ratio
                    if ((nearBoundPoint == 1) || (nearBoundPoint == 2) || (nearBoundPoint == 3) ||
                            (nearBoundPoint == 4)) {

                        if (scaleX < scaleY) {
                            scaleX = scaleY;
                        } else {
                            scaleY = scaleX;
                        }
                    }

                    translateX = 0;
                    translateY = 0;

                    // Prevent the bounding box from expanding past the
                    // bounds of the image.
                    xCenter = (oldXMax + oldXMin) / 2.0f;
                    yCenter = (oldYMax + oldYMin) / 2.0f;
                    newXMax = (scaleX * oldXMax) + (xCenter * (1 - scaleX));

                    if (newXMax > (xDim - 1)) {
                        return;
                    }

                    newXMin = (scaleX * oldXMin) + (xCenter * (1 - scaleX));

                    if (newXMin < 0.0f) {
                        return;
                    }

                    newYMax = (scaleY * oldYMax) + (yCenter * (1 - scaleY));

                    if (newYMax > (yDim - 1)) {
                        return;
                    }

                    newYMin = (scaleY * oldYMin) + (yCenter * (1 - scaleY));

                    if (newYMin < 0.0f) {
                        return;
                    }

                    transformVOI(0.0f, 0.0f, 0.0f, translateX, translateY, 0.0f, scaleX, scaleY, 1.0f, slice,
                            resizeIndex, doRound);
                    ((VOIContour) (curves.elementAt(resizeIndex))).contains(-1, -1, true);
                } // end of if (change)
            }
        }
    }
     */



    /**
     * Finds the second oder attributes and prints to the command line. Only for contour.
     *
     * @param  xDim    DOCUMENT ME!
     * @param  yDim    DOCUMENT ME!
     * @param  xRes    DOCUMENT ME!
     * @param  yRes    DOCUMENT ME!
     * @param  xUnits  DOCUMENT ME!
     * @param  yUnits  DOCUMENT ME!
    public void secondOrderAttributes(int xDim, int yDim, float xRes, float yRes, int xUnits, int yUnits) {
        int i, z;
        float[] pAxis = null;
        float[] eccentricity = null;
        float[] majorAxis = null;
        float[] minorAxis = null;

        if ((curveType == LINE) || (curveType == POLYLINE) || (curveType == PROTRACTOR)) {
            return;
        }

        try {
            pAxis = new float[1];
            eccentricity = new float[1];
            majorAxis = new float[1];
            minorAxis = new float[1];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: VOI secondOrderAttributes");

            return;
        }
        for (i = 0; i < curves.size(); i++) {
            ((VOIContour) (curves.elementAt(i))).secondOrderAttributes(xDim, yDim, xRes, yRes, xUnits,
                    yUnits, pAxis, eccentricity,
                    majorAxis, minorAxis);
            Preferences.debug(" Theta = " + pAxis[0] + " ecc = " + eccentricity[0] + " Major axis = " +
                    majorAxis[0] + " Minor Axis = " + minorAxis[0] + "\n");
        }
    }
     */

    /**
     * Accessor that sets the flag to the parameter.
     *
     * @param  flag  the visible flag
     */
    public void setBoundingBoxFlag(boolean flag) {
        this.boundingBox = flag;
    }

    /**
     * Accessor that sets the color to the parameter.
     *
     * @param  color  the color
     */
    public void setColor(Color color) {

        // System.err.println("Setting color externally to: " + color);
        this.color = color;

        if (listenerList != null) {
            Object[] listeners = listenerList.getListenerList();
            // Process the listeners last to first, notifying
            // those that are interested in this event
            for (int i = listeners.length - 2; i >= 0; i -= 2) {

                if (listeners[i] == VOIListener.class) {
                    ((VOIListener) listeners[i + 1]).colorChanged(color);
                }
            }
        }
    }

    /**
     * Accessor that sets the color to the parameter.
     *
     * @param  hue  the color of the VOI
     */
    public void setColor(float hue) {

        // System.err.println("Setting hue externally to: " + hue);
        this.setColor(Color.getHSBColor(hue, (float) 1.0, (float) 1.0));

        if (listenerList != null) {
            Object[] listeners = listenerList.getListenerList();
            // Process the listeners last to first, notifying
            // those that are interested in this event
            for (int i = listeners.length - 2; i >= 0; i -= 2) {

                if (listeners[i] == VOIListener.class) {
                    ((VOIListener) listeners[i + 1]).colorChanged(color);
                }
            }
        }

    }

    /**
     * Accessor that sets the contourGraph to the parameter.
     *
     * @param  newGraph  the graph
     */
    public void setContourGraph(ViewJFrameGraph newGraph) {
        this.contourGraph = newGraph;
    }

    public void setCurves(Vector<VOIBase> newCurves) {
        this.curves = newCurves;
    }

    public void setCurvesTemp(Vector<VOIContour>[] newCurves) {
        //this.curves = newCurves[0];
    }

    /**
     * Accessor that sets the curveType to the parameter.
     *
     * @param  curveType  the curve type
     */
    public void setCurveType(int curveType) {
        this.curveType = curveType;
    }

    /**
     * Accessor that sets the display mode to the parameter.
     *
     * @param  mode  the display mode
     */
    public void setDisplayMode(int mode) {
        this.displayMode = mode;
    }

    /**
     * @param extension
     */
    public void setExtension(String extension) {
        this.extension = extension;
    }

    /**
     * Sets whether or not the VOI is fixed.
     *
     * @param  fixed  boolean to set fixed to
     */
    public void setFixed(boolean fixed) {
        this.fixed = fixed;
    }

    /**
     * Accessor that sets the ID to the parameter.
     *
     * @param  ID  the ID
     */
    public void setID(short ID) {
        this.ID = ID;

        if (getCurveType() != ANNOTATION) {
            float hue;
            int colorIncrement = Preferences.getVOIColorIncrement();
            hue = (float) ((((ID + colorIncrement) * 35) % 360) / 360.0);
            this.setColor(Color.getHSBColor(hue, (float) 1.0, (float) 1.0));
        }
    }

    /**
     * Accessor that sets the levelset VOI's level.
     *
     * @param  lev  The level.
     */
    public void setLevel(float lev) {
        level = lev;
    }

    /**
     * Accessor that sets the intensity array to the parameter.
     *
     * @param  inten  DOCUMENT ME!
    public void setIntensity(float[] inten) {
        this.intensity = inten;
    }
     */

    /**
     * Accessor that sets the maximum of the range of intensities to ignore.
     *
     * @param  max  The maximum.
     */
    public void setMaximumIgnore(float max) {
        ignoreMax = max;
    }

    /**
     * Accessor that sets the minimum of the range of intensities to ignore.
     *
     * @param  min  The minimum.
     */
    public void setMinimumIgnore(float min) {
        ignoreMin = min;
    }

    /**
     * Accessor that sets the VOI's name to the parameter.
     *
     * @param  name  the name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Accessor that sets the ID to the parameter.
     *
     * @param  ID  the ID
     */
    public void setOnlyID(short ID) {
        this.ID = ID;
    }

    /**
     * Accessor that sets the opacity to the parameter.
     *
     * @param  opacity  the opacity
     */
    public void setOpacity(float opacity) {
        this.opacity = opacity;
    }

    /**
     * Accessor that sets the polarity to the parameter.
     *
     * @param  polarity  the polarity
     */
    public void setPolarity(int polarity) {
        this.polarity = polarity;
    }

    /**
     * Accessor that sets the flag to the parameter.
     *
     * @param  flag  the process flag
     */
    public void setProcess(boolean flag) {
        this.process = flag;
    }

    /**
     * Accessor that sets the position array to the parameter.
     *
     * @param  pos  DOCUMENT ME!
    public void setPosition(float[] pos) {
        this.position = pos;
    }
     */

    /**
     * Accessor that sets the statisitic list.
     *
     * @param  stats  list of statistics
     */
    public void setStatisticList(ViewList[] stats) {
        this.stats = stats;
    }

    /**
     * Accessor that sets the rgb intensity array to the parameter.
     *
     * @param  inten  DOCUMENT ME!
    public void setRGBIntensities(float[][] inten) {
        this.rgbIntensities = inten;
    }
     */

    /**
     * Accessor that sets the rgb position array to the parameter.
     *
     * @param  pos  DOCUMENT ME!
    public void setRGBPositions(float[][] pos) {
        this.rgbPositions = pos;
    }
     */

    /**
     * Sets the thickness of the VOI
     * @param newThickness the new thickness
     */
    public void setThickness(int newThickness) {
        this.thickness = newThickness;
    }
    /**
     * Accessor that returns that whether to calculate total sum of the intensity (true) else calculate the average
     * pixel intensity (used when plotting an intensity graph of a voi).
     *
     * @return  DOCUMENT ME!
    public boolean getTotalIntensity() {
        return totalIntensity;
    }
     */

    /**
     * Accessor that sets whether to calculate total sum of the intensity (true) else calculate the average pixel
     * intensity (used when plotting an intensity graph of a voi).
     *
     * @param  total  DOCUMENT ME!
    public void setTotalIntensity(boolean total) {
        this.totalIntensity = total;
    }
     */

    /**
     * Sets the unique ID (for saving/retreiving treatment details).
     *
     * @param  uid  - unique ID
     */
    public void setUID(int uid) {
        this.UID = uid;
    }

    /**
     * Accessor that sets the flag to the parameter.
     *
     * @param  flag  the visible flag
    public void setVisibleFlag(boolean flag) {
        this.visible = flag;
    }
     */

    /**
     * Accessor that sets the ID to the parameter.
     *
     * @param  wID  the ID
     */
    public void setWatershedID(short wID) {
        this.watershedID = wID;
    }

    /**
     * Accessor that sets xDim and yDim.
     *
     * @param  xDim  x dimension
     * @param  yDim  y dimension
     */
    public void setXYDim(int xDim, int yDim) {
    }


    /**
     * Accessor that returns the name of the VOI.
     *
     * @return  the name
     */
    public String toString() {
        return name;
    }

    /**
     * Transforms self.
     *
     * @param  tMatrix  transformation matrix
    public void transformVOI(TransMatrix tMatrix) {
        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            for (int i = 0; i < curves.size(); i++) {
                ((VOIContour) (curves.elementAt(i))).transformContour(tMatrix, false);
            }
        }
    }
     */

    /**
     * Transforms self.
     *
     * @param  rotX  rotation in x in degrees
     * @param  rotY  rotation in y in degrees
     * @param  rotZ  rotation in z in degrees
     * @param  tX    translation in x
     * @param  tY    translation in y
     * @param  tZ    translation in z
     * @param  sX    zoom in x
     * @param  sY    zoom in y
     * @param  sZ    zoom in z
    public void transformVOI(float rotX, float rotY, float rotZ, float tX, float tY, float tZ, float sX, float sY,
            float sZ) {
        TransMatrix tMatrix = null;
        Vector3f gcPt = null;

        try {
            gcPt = new Vector3f();
            tMatrix = new TransMatrix(4);
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: VOI exportPolygons.");

            return;
        }

        gcPt = getGeometricCenter();
        tMatrix.setTranslate((gcPt.X + tX), (gcPt.Y + tY), (gcPt.Z + tZ));
        tMatrix.setRotate(rotX, rotY, rotZ, TransMatrix.DEGREES);
        tMatrix.setZoom(sX, sY, sZ);
        tMatrix.setTranslate(-gcPt.X, -gcPt.Y, -gcPt.Z);

        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            for (int i = 0; i < curves.size(); i++) {
                ((VOIContour) (curves.elementAt(i))).transformContour(tMatrix, false);
            }
        }
    }
     */

    /**
     * Transforms self.
     *
     * @param  rotX     rotation in x in degrees
     * @param  rotY     rotation in y in degrees
     * @param  rotZ     rotation in z in degrees
     * @param  tX       translation in x
     * @param  tY       translation in y
     * @param  tZ       translation in z
     * @param  sX       zoom in x
     * @param  sY       zoom in y
     * @param  sZ       zoom in z
     * @param  slice    DOCUMENT ME!
     * @param  element  DOCUMENT ME!
     * @param  doRound  if true round point coordinates to integers
     * @deprecated
     */
    public void transformVOI(float rotX, float rotY, float rotZ, float tX, float tY, float tZ, float sX, float sY,
            float sZ, int slice, int element, boolean doRound) {
        TransMatrix tMatrix = null;
        Vector3f gcPt = null;

        try {
            gcPt = new Vector3f();
            tMatrix = new TransMatrix(4);
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: VOI exportPolygons.");

            return;
        }

        gcPt = ((VOIContour) (curves.elementAt(element))).getGeometricCenter();
        tMatrix.setTranslate((gcPt.X + tX), (gcPt.Y + tY), (gcPt.Z + tZ));
        tMatrix.setRotate(rotX, rotY, rotZ, TransMatrix.DEGREES);
        tMatrix.setZoom(sX, sY, sZ);
        tMatrix.setTranslate(-gcPt.X, -gcPt.Y, -gcPt.Z);

        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves.elementAt(element))).transformContour(tMatrix, doRound);
        }
    }

    // Notify all listeners that have registered interest for
    // notification on this event type. The event instance
    // is lazily created using the parameters passed into
    // the fire method.
    /**
     * Fires a VOI event based on the VOI. calls the listener's <code>addedVOI()</code> method.
     *
     * @param  curve  DOCUMENT ME!
    protected void fireVOIBaseAdded(VOIBase curve) {

        try {

            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(VOIListener.class) == 0) {
                return;
            }
        } catch (NullPointerException npe) {
            listenerList = new EventListenerList();
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        voiUpdate = new VOIEvent(this, curve);

        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();

        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == VOIListener.class) {
                ((VOIListener) listeners[i + 1]).addedCurve(voiUpdate);
            }
        }
    }
     */

    /**
     * Fires a VOI event based on the VOI. calls the listener's <code>removedVOI()</code> method.
     *
     * @param  curve  DOCUMENT ME!
    protected void fireVOIBaseRemoved(VOIBase curve) {

        try {

            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(VOIListener.class) == 0) {
                return;
            }
        } catch (NullPointerException npe) {
            listenerList = new EventListenerList();
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        voiUpdate = new VOIEvent(this, curve);

        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();

        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == VOIListener.class) {
                ((VOIListener) listeners[i + 1]).removedCurve(voiUpdate);
            }
        }
    }
     */

    /**
     * Fires a VOI event based on the VOI. calls the listener's <code>removedVOI()</code> method.
    protected void fireVOIselection() {

        try {

            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(VOIListener.class) == 0) {
                return;
            }
        } catch (NullPointerException npe) {
            listenerList = new EventListenerList();
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        voiUpdate = new VOIEvent(this);

        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == VOIListener.class) {
                ((VOIListener) listeners[i + 1]).selectedVOI(voiUpdate);
            }
        }
    }
     */

    public void trim()
    {
        for ( int i = 0; i < curves.size(); i++ )
        {
            if ( curves.get(i).isActive() )
            {
                curves.get(i).trimPoints(Preferences.getTrim(),
                        Preferences.getTrimAdjacient());
            }
        }
    }
    
    /**
     * Gathers max and average statistics to build guessing intervals
     */
    private double[] determineMaxAvg(float xRes, float yRes, float zRes,
            float[] xPts, float[] yPts, float zPts[]) {
        double max = 0, avg = 0;
        int n = xPts.length;
        Random r = new Random();
        //note that a stop that yields a non-representative average is fine, since worst case is more computations
        int stop = n < 100 ? (int)(n*0.50) : (int)(n*0.10);
        int[] search = new int[stop];
        int[] end = new int[stop];
        double startX, startY, startZ;
        double endX, endY, endZ;
        double delX, delY, delZ;
        double distX, distY, distZ;
        double distanceSq;
        int i, j;
        for(i = 0; i<stop; i++) {
            search[i] = r.nextInt(n);
            end[i] = r.nextInt(n);
        }

        int numBegin = 0, numEnd = 0;;
        for (i = 0; i < search.length; i++) {

            numBegin = search[i];
            startX = xPts[numBegin];
            startY = yPts[numBegin];
            startZ = zPts[numBegin];
            for (j = 0; j < end.length; j++) {
                numEnd = end[j];
                endX = xPts[numEnd];
                endY = yPts[numEnd];
                endZ = zPts[numEnd];
                delX = endX - startX;
                delY = endY - startY;
                delZ = endZ - startZ;
                distX = xRes * delX;
                distY = yRes * delY;
                distZ = zRes * delZ;
                distanceSq = distX*distX + distY*distY + distZ*distZ;
                avg = avg + distanceSq;
                if (distanceSq > max) {
                    max = distanceSq;
                } // if (distanceSq > largsestDistanceSq)
            } // for (j = i+1; j < xPts.length; j++)
        } // for (i = 0; i < xPts.length; i++)
        avg = avg / (end.length * search.length);
        return new double[]{max, avg};
    }



    /**
     * This is used by the method maxWidth.
     *
     * @param   pt           calculate the distance from this point to any point on any of the contours which form the
     *                       VOI.
     * @param   maxDistance  single array value to keep track of the largest
     *
     * @return  the maximum width of the VOI
     */
    private Vector3f findMaxWidth(Vector3f pt, double[] maxDistance, boolean bOnlyActive) {

        if (curveType != CONTOUR) {
            return null;
        }
        
        int i;
        int j, conSize;
        double distance;
        Vector3f point = null;

        for (i = 0; i < curves.size(); i++) {

            if (curves.elementAt(i).isActive() || !bOnlyActive ) {
                conSize = ((VOIContour) (curves.elementAt(i))).size();

                for (j = 0; j < conSize; j++) {
                    distance = MipavMath.distance(pt,
                            ((VOIContour) (curves.elementAt(i))).elementAt(j));

                    if (distance > maxDistance[0]) {
                        maxDistance[0] = distance;
                        point = ((VOIContour) (curves.elementAt(i))).elementAt(j);
                    }
                }
            }
        }

        return point;
    }






    /**
     * Gathers the points that fall within the required bounds
     */
    private void gatherBoundedPoints(ArrayList<Integer> orig, ArrayList<Integer> term, 
            double lowerBound, double upperBound, 
            float xRes, float yRes, float zRes, 
            float[] xPts, float[] yPts, float zPts[]) {
        int i, j;
        double startX, startY, startZ;
        double endX, endY, endZ;
        double delX, delY, delZ;
        double distX, distY, distZ;
        double distanceSq;
        for (i = 0; i < xPts.length; i++) {
            startX = xPts[i];
            startY = yPts[i];
            startZ = zPts[i];
            for (j = i+1; j < xPts.length; j++) {
                endX = xPts[j];
                endY = yPts[j];
                endZ = zPts[j];
                delX = endX - startX;
                delY = endY - startY;
                delZ = endZ - startZ;
                distX = xRes * delX;
                distY = yRes * delY;
                distZ = zRes * delZ;
                distanceSq = distX*distX + distY*distY + distZ*distZ;
                if (distanceSq > lowerBound && distanceSq < upperBound) {
                    orig.add(Integer.valueOf(i));
                    term.add(Integer.valueOf(j));
                } // if (distanceSq > largsestDistanceSq)
            } // for (j = i+1; j < xPts.length; j++)
        } // for (i = 0; i < xPts.length; i++)
    }

    /**
     * Determines if the given BitSet binary mask is true for all points that are within the VOI.
     *
     * @param   xDim   x dimension, used to index into the image
     * @param   yDim   y dimension, used to index into the image
     * @param   slice  int the z slice in which to look
     * @param   mask   BitSet the mask to check
     *
     * @return  boolean is the mask true for at least all area covered by VOI
    private boolean isBinaryMaskContained(int xDim, int yDim, int slice, BitSet mask) {
        int x, y;
        int i, nGons;
        int offset;
        int offsetZ;
        nGons = curves[slice].size();
        offsetZ = slice * xDim * yDim;

        if ((process == true) && (curveType == CONTOUR)) {

            for (i = 0; i < nGons; i++) {
                ((VOIContour) (curves[slice].elementAt(i))).contains(0, 0, true);
                ((VOIContour) (curves[slice].elementAt(i))).getBounds(xBounds, yBounds, zBounds);

                int xbs = (int) xBounds[0];
                int xbe = (int) xBounds[1];
                int ybs = (int) yBounds[0];
                int ybe = (int) yBounds[1];

                for (y = ybs; y < ybe; y++) {
                    offset = offsetZ + (y * xDim);

                    for (x = xbs; x < xbe; x++) {

                        if (((VOIContour) (curves[slice].elementAt(i))).contains(x, y, false) &&
                                (polarity == ADDITIVE)) {

                            // if point was in VOI but not already set in mask, not contained
                            if (!mask.get(offset + x)) {
                                return false;
                            }
                        }
                    }
                }
            }
        }

        return true;
    }
     */


    /**
     * Gets a sorted Vector of PolyPoints (for use in the VOI copy/pasting)
     * @return Vector, sorted
     * @deprecated
    public Vector getSortedPolyPoints() {
        Vector distanceVector = new Vector();
        Vector3f firstPoint;
        int i;


        int sliceCounter;
        int pointNumber = 0;

        for (sliceCounter = 0; sliceCounter < zDim; sliceCounter++) {

            for (i = 0; i < curves[sliceCounter].size(); i++) {

                try {
                    pointNumber = Integer.parseInt(((VOIPoint) curves[sliceCounter].elementAt(i)).getLabel());
                    firstPoint = ((VOIPoint) curves[sliceCounter].elementAt(i)).exportPoint();

                    firstPoint.Z = sliceCounter;

                    distanceVector.add(new PolyPointHolder(firstPoint, pointNumber));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        Collections.sort(distanceVector, new PointComparator());
        return distanceVector;
    }
     */


    /**
     * Finds the largest line that lies within entirely within the VOI.
     */  
    private double getLargest(ArrayList<Integer> orig, ArrayList<Integer> term, 
            float xRes, float yRes, float zRes, 
            float[] xPoints, float[] yPoints, float zPoints[]) {
        int origPoint, termPoint;
        double largestDistanceSq = 0, distanceSq;
        double startX, startY, startZ;
        double endX, endY, endZ;
        double delX, delY, delZ;
        double distX, distY, distZ;
        double x, y, z;
        double slope, slope2;
        int xRound, yRound, zRound;
        int j, k;
        boolean okay;
        forj:
            for(j=0; j<orig.size(); j++) {
                origPoint = orig.get(j).intValue();
                startX = xPoints[origPoint];
                startY = yPoints[origPoint];
                startZ = zPoints[origPoint];
                termPoint = term.get(j).intValue();
                endX = xPoints[termPoint];
                endY = yPoints[termPoint];
                endZ = zPoints[termPoint];
                delX = endX - startX;
                delY = endY - startY;
                delZ = endZ - startZ;
                distX = xRes * delX;
                distY = yRes * delY;
                distZ = zRes * delZ;
                distanceSq = distX*distX + distY*distY + distZ*distZ;
                if(distanceSq > largestDistanceSq) {
                    if ((Math.abs(delX) >= Math.abs(delY)) && (Math.abs(delX) >= Math.abs(delZ))) {
                        slope = delY/delX;
                        slope2 = delZ/delX;
                        if (endX >= startX) {
                            for (x = startX + 0.5, y = startY + 0.5 * slope, z = startZ + 0.5 * slope2; x < endX; x += 0.5, y += 0.5 * slope,
                            z += 0.5 * slope2) {
                                xRound = (int)Math.round(x);
                                yRound = (int)Math.round(y);
                                zRound = (int)Math.round(z);
                                okay = false;
                                for (k = 0; (k < curves.size()) && (!okay); k++) {
                                    if (((VOIContour) (curves.elementAt(k))).contains(xRound, yRound, zRound)) { 
                                        okay = true;  
                                    }
                                }
                                if (!okay) {
                                    continue forj;
                                }
                            } // for (x = startX + 0.5, y = startY + 0.5 * slope, z = startZ + 0.5 * slope2; x < endX; x += 0.5, y += 0.5 * slope,
                        } // if (endX >= startX)
                        else { // endX < startX
                            for (x = startX - 0.5, y = startY - 0.5 * slope, z = startZ - 0.5 * slope2; x > endX; x -= 0.5, y -= 0.5 * slope,
                            z -= 0.5 * slope2) {
                                xRound = (int)Math.round(x);
                                yRound = (int)Math.round(y);
                                zRound = (int)Math.round(z);
                                okay = false;
                                for (k = 0; (k < curves.size()) && (!okay); k++) {
                                    if (((VOIContour) (curves.elementAt(k))).contains(xRound, yRound, zRound)) { 
                                        okay = true;  
                                    }
                                }
                                if (!okay) {
                                    continue forj;
                                }
                            } // for (x = startX - 0.5, y = startY - 0.5 * slope, z = startZ - 0.5 * slope2; x > endX; x -= 0.5, y -= 0.5 * slope,
                        } // else endX < startX
                    } // if ((Math.abs(delX) >= Math.abs(delY)) && (Math.abs(delX) >= Math.abs(delZ)))
                    else if ((Math.abs(delY) >= Math.abs(delX)) && (Math.abs(delY) >= Math.abs(delZ))){ 
                        slope = delX/delY;
                        slope2 = delZ/delY;
                        if (endY >= startY) {
                            for (y = startY + 0.5, x = startX + 0.5 * slope, z = startX + 0.5 * slope2; y < endY; y += 0.5, x += 0.5 * slope,
                            z += 0.5 * slope2) {
                                xRound = (int)Math.round(x);
                                yRound = (int)Math.round(y);
                                zRound = (int)Math.round(z);
                                okay = false;
                                for (k = 0; (k < curves.size()) && (!okay); k++) {
                                    if (((VOIContour) (curves.elementAt(k))).contains(xRound, yRound, zRound)) { 
                                        okay = true;  
                                    }
                                }
                                if (!okay) {
                                    continue forj;
                                }
                            } // for (y = startY + 0.5, x = startX + 0.5 * slope, z = startX + 0.5 * slope2; y < endY; y += 0.5, x += 0.5 * slope,
                        } // if (endX >= startX)
                        else { // endX < startX
                            for (y = startY - 0.5, x = startX - 0.5 * slope, z = startZ - 0.5 * slope2; y > endY; y -= 0.5, x -= 0.5 * slope,
                            z -= 0.5 * slope2) {
                                xRound = (int)Math.round(x);
                                yRound = (int)Math.round(y);
                                zRound = (int)Math.round(z);
                                okay = false;
                                for (k = 0; (k < curves.size()) && (!okay); k++) {
                                    if (((VOIContour) (curves.elementAt(k))).contains(xRound, yRound, zRound)) { 
                                        okay = true;  
                                    }
                                }
                                if (!okay) {
                                    continue forj;
                                }
                            } // for (y = startY - 0.5, x = startX - 0.5 * slope, z = startZ - 0.5 * slope2; y > endY; y -= 0.5, x -= 0.5 * slope,
                        } // else endX < startX    
                    } // else if ((Math.abs(delY) >= Math.abs(delX)) && (Math.abs(delY) >= Math.abs(delZ)))
                    else { // ((Math.abs(delZ) >= Math.abs(delX)) && (Math.abs(delZ) >= Math.abs(delY))) 
                        slope = delX/delZ;
                        slope2 = delY/delZ;
                        if (endZ >= startZ) {
                            for (z = startZ + 0.5, x = startX + 0.5 * slope, y = startY + 0.5 * slope2; z < endZ; z += 0.5, x += 0.5 * slope,
                            y += 0.5 * slope2) {
                                xRound = (int)Math.round(x);
                                yRound = (int)Math.round(y);
                                zRound = (int)Math.round(z);
                                okay = false;
                                for (k = 0; (k < curves.size()) && (!okay); k++) {
                                    if (((VOIContour) (curves.elementAt(k))).contains(xRound, yRound, zRound)) { 
                                        okay = true;  
                                    }
                                }
                                if (!okay) {
                                    continue forj;
                                }
                            } // for (z = startZ + 0.5, x = startX + 0.5 * slope, y = startY + 0.5 * slope2; z < endZ; z += 0.5, x += 0.5 * slope,   
                        } // if (endZ >= startZ)
                        else { // endZ < startZ
                            for (z = startZ - 0.5, x = startX - 0.5 * slope, y = startY - 0.5 * slope2; z > endZ; z -= 0.5, x -= 0.5 * slope,
                            y -= 0.5 * slope2) {
                                xRound = (int)Math.round(x);
                                yRound = (int)Math.round(y);
                                zRound = (int)Math.round(z);
                                okay = false;
                                for (k = 0; (k < curves.size()) && (!okay); k++) {
                                    if (((VOIContour) (curves.elementAt(k))).contains(xRound, yRound, zRound)) { 
                                        okay = true;  
                                    }
                                }
                                if (!okay) {
                                    continue forj;
                                }
                            } // for (z = startZ - 0.5, x = startX - 0.5 * slope, y = startY - 0.5 * slope2; z > endZ; z -= 0.5, x -= 0.5 * slope,
                        } // else (endZ < startZ)
                    } // else ((Math.abs(delZ) >= Math.abs(delX)) && (Math.abs(delZ) >= Math.abs(delY))) 
                    largestDistanceSq = distanceSq;
                }
            }
        return Math.sqrt(largestDistanceSq);
    }

    /**
     * Calculates the extents or boundary of the voi for a specified slice in x and y.
     *
     * @param  slice  the examined slice
     * @param  x      two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in
     *                the x dimension
     * @param  y      two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in
     *                the y dimension
     */
    private void getSliceBounds(int slice, float[] x, float[] y) {

        Vector3f[] akBounds = new Vector3f[2];
        akBounds[0] = new Vector3f();
        akBounds[1] = new Vector3f();
        boolean bFirst = true;

        for ( int j = 0; j < curves.size(); j++ )
        {
            VOIBase kCurrentVOI = curves.get(j);
            if ( kCurrentVOI.slice() == slice )
            {
                Vector3f[] kBounds = kCurrentVOI.getImageBoundingBox();
                if ( bFirst )
                {
                    bFirst = false;
                    akBounds[0].Copy(kBounds[0]);
                    akBounds[1].Copy(kBounds[1]);
                }
                akBounds[0].Min(kBounds[0]);
                akBounds[1].Max(kBounds[1]);
            }
        }
        x[0] = akBounds[0].X;
        x[1] = akBounds[1].X;
        y[0] = akBounds[0].Y;
        y[1] = akBounds[1].Y;
    }



    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    //private class PointComparator implements Comparator {

        /**
         * DOCUMENT ME!
         *
         * @param   o1  DOCUMENT ME!
         * @param   o2  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        //public int compare(Object o1, Object o2) {
        ////    int a = ((PolyPointHolder) o1).getNumber();
        //    int b = ((PolyPointHolder) o2).getNumber();

        //    return ((new Integer(a)).compareTo(new Integer(b)));
       // }

    //}

    /**
     * DOCUMENT ME!
     */
    //public class PolyPointHolder {

        /** DOCUMENT ME! */
        //private int number;

        /** DOCUMENT ME! */
        //private Vector3f pt;

        /**
         * Creates a new PolyPointHolder object.
         *
         * @param  p  DOCUMENT ME!
         * @param  n  DOCUMENT ME!
         */
        //public PolyPointHolder(Vector3f p, int n) {
        //    this.pt = p;
        //    this.number = n;
        //}

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        //public int getNumber() {
        //    return this.number;
        //}

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        //public Vector3f getPoint() {
        //    return this.pt;
        //}
    //}



}
