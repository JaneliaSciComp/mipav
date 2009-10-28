package gov.nih.mipav.model.structures;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.event.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.text.*;

import java.util.*;

import javax.swing.event.*;


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
    private Vector<VOIBase>[] curves;

    /** Indicates the type of VOI (i.e. CONTOUR, POLYLINE, LINE, POINT, PROTRACTOR, etc. ... ) */
    private int curveType;

    /** Indicates if the VOI should be shown as a boundary or a solid. */
    private int displayMode;

    /** The label (numbering) of the curve, displayed when curve is highlighted. */
    private short elementLabel = 1;

    /** If true the VOI can be moved, if false this VOI cannot be moved. */
    private boolean fixed = false;

    /** ID of the VOI, also used when choosing the display color. */
    private short ID;

    /** DOCUMENT ME! */
    private float ignoreMax = -Float.MAX_VALUE;

    /** DOCUMENT ME! */
    private float ignoreMin = Float.MAX_VALUE;

    /** DOCUMENT ME! */
    private float[] intensity = null;

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

    /** DOCUMENT ME! */
    private float[] position = null;

    /** If true this flag indicates that the VOI should be included (applied) when processing the image. */
    private boolean process;

    /** DOCUMENT ME! */
    private int resizeIndex;

    /** DOCUMENT ME! */
    private float[][] rgbIntensities = null;

    /** DOCUMENT ME! */
    private float[][] rgbPositions = null;

    /** DOCUMENT ME! */
    private ViewList[] stats = null;

    /** DOCUMENT ME! */
    private boolean totalIntensity;

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

    /** DOCUMENT ME! */
    private int zDim;

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
        curves = new Vector[zDim];

        for (int i = 0; i < zDim; i++) {
            curves[i] = new Vector();
        }

        displayMode = BOUNDARY;
        polarity = ADDITIVE;
        visible = true;
        opacity = 0.3f;
        hue = (float) ((((ID) * 35) % 360) / 360.0);
        setColor(Color.getHSBColor(hue, (float) 1.0, (float) 1.0)); // important to use the access method

        this.thickness = Preferences.getVOIThickness();
        
        // this ensures that color gets set in
        // all protractor contours.
        this.zDim = zDim;
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
        curves = new Vector[zDim];

        for (int i = 0; i < zDim; i++) {
            curves[i] = new Vector();
        }

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
        this.zDim = zDim;
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
     * Returns the number of z slices actually containing part of the voi
     * @return
     */
    public int getVOISlices() {
        int z;
        int voiSlices = 0;
        for (z = 0; z < zDim; z++) {
            if (curves[z].size() > 0) {
                voiSlices++;
            }
        }
        return voiSlices;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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

    /**
     * Finds the area of the entire VOI of the VOIContour type only.
     *
     * @return  returns the area
     */
    public int area() {
        int z;
        int totalArea = 0;

        for (z = 0; z < zDim; z++) {

            for (int i = 0; i < curves[z].size(); i++) {

                if (curveType == CONTOUR) {
                    totalArea += ((VOIContour) (curves[z].elementAt(i))).area();
                }
            }
        }

        return totalArea;
    }

    /**
     * Prints out the distances between segments, displacement (1st to last point) and total distance in 2D and 3D.
     *
     * @param  fileInfo  FileInfoBase
     */
    public void calcPLineDistances(FileInfoBase fileInfo) {

        if (curveType != POLYLINE_SLICE) {
            return;
        }

        int unitsOfMeasure = fileInfo.getUnitsOfMeasure(0);
        float[] resols = fileInfo.getResolutions();
        int i;

        Vector distanceVector = new Vector();
        Vector3f firstPoint, secondPoint;

        String unitsStr = new String();

        switch (unitsOfMeasure) {

            case FileInfoBase.INCHES:
                unitsStr = " in";
                break;

            case FileInfoBase.ANGSTROMS:
                unitsStr = " A";
                break;

            case FileInfoBase.NANOMETERS:
                unitsStr = " nm";
                break;

            case FileInfoBase.MICROMETERS:
                unitsStr = " um";
                break;

            case FileInfoBase.MILLIMETERS:
                unitsStr = " mm";
                break;

            case FileInfoBase.CENTIMETERS:
                unitsStr = " cm";
                break;

            case FileInfoBase.METERS:
                unitsStr = " m";
                break;

            case FileInfoBase.KILOMETERS:
                unitsStr = " km";
                break;

            case FileInfoBase.MILES:
                unitsStr = " miles";
                break;

            default:
                unitsStr = " Unknown";
        }


        int numSlices = fileInfo.getExtents()[2];
        int sliceCounter;
        int pointNumber = 0;

        for (sliceCounter = 0; sliceCounter < numSlices; sliceCounter++) {

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

    /**
     * Finds convex hull for all contours in VOI independantly NOT a 3D convex hull.
     */
    public void convexHull() {
        int z, i;

        for (z = 0; z < zDim; z++) {

            for (i = 0; i < curves[z].size(); i++) {

                if (curveType == CONTOUR) {
                    ((VOIContour) (curves[z].elementAt(i))).convexHull();
                }
            }
        }
    }

    /**
     * Clone function that calls the super (modelserializable) clone and then manually copies references 
     * to the transient VOIListeners (eventlistenerlist)
     */
    public Object clone() {
    	Object obj = super.clone();
    	
    	if (listenerList != null) {
    		int listeners = listenerList.getListenerCount(VOIListener.class);
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
     */
    public void convexHull(int slice, int index) {

        if (curveType == CONTOUR) {
            ((VOIContour) (curves[slice].elementAt(index))).convexHull();
        }
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
        double largestDistanceSq = 0.0f;
        int i;
        int j;
        int slice;
        float xPts[];
        float yPts[];
        int zPts[];
        int nGons;
        int nPts = 0;
        int index = 0;
        int len;
        
        if (curveType != CONTOUR) {
            MipavUtil.displayError("curveType is not the required CONTOUR for calcLargestDistance");
            return 0;
        }
        
        for (slice = 0; slice < zDim; slice++) {
            nGons = curves[slice].size(); 
            for (i = 0; i < nGons; i++) {
                nPts += curves[slice].elementAt(i).size();
            }
        }
        
        xPts = new float[nPts];
        yPts = new float[nPts];
        zPts = new int[nPts];
        
        for (slice = 0; slice < zDim; slice++) {
            nGons = curves[slice].size();
            for (i = 0; i < nGons; i++) {
                len = curves[slice].elementAt(i).size();
                for (j = 0; j < len; j++) {
                   xPts[index] = ((Vector3f) ((VOIContour) (curves[slice].elementAt(i))).elementAt(j)).X;
                   yPts[index] = ((Vector3f) ((VOIContour) (curves[slice].elementAt(i))).elementAt(j)).Y;
                   zPts[index++] = slice;
                }
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
     * Gathers max and average statistics to build guessing intervals
     */
    private double[] determineMaxAvg(float xRes, float yRes, float zRes,
    								float[] xPts, float[] yPts, int zPts[]) {
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
     * Gathers the points that fall within the required bounds
     */
    private void gatherBoundedPoints(ArrayList<Integer> orig, ArrayList<Integer> term, 
    							double lowerBound, double upperBound, 
    							float xRes, float yRes, float zRes, 
    							float[] xPts, float[] yPts, int zPts[]) {
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
	 * Finds the largest line that lies within entirerly within the VOI.
     */  
    private double getLargest(ArrayList<Integer> orig, ArrayList<Integer> term, 
    							float xRes, float yRes, float zRes, 
    							float[] xPoints, float[] yPoints, int zPoints[]) {
    	int origPoint, termPoint;
    	double largestDistanceSq = 0, distanceSq;
    	double startX, startY, startZ;
    	double endX, endY, endZ;
    	double delX, delY, delZ;
    	double distX, distY, distZ;
    	double x, y, z;
        double slope, slope2;
        int xRound, yRound, zRound;
        int j, k, r;
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
	                        if ((zRound < 0) || (zRound >= zDim)) {
	                            continue forj;
	                        }
	                        okay = false;
	                        for (k = 0; (k < curves[zRound].size()) && (!okay); k++) {
	                            if (((VOIContour) (curves[zRound].elementAt(k))).contains(xRound, yRound, false)) { 
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
	                        if ((zRound < 0) || (zRound >= zDim)) {
	                            continue forj;
	                        }
	                        okay = false;
	                        for (k = 0; (k < curves[zRound].size()) && (!okay); k++) {
	                            if (((VOIContour) (curves[zRound].elementAt(k))).contains(xRound, yRound, false)) { 
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
	                        if ((zRound < 0) || (zRound >= zDim)) {
	                            continue forj;
	                        }
	                        okay = false;
	                        for (k = 0; (k < curves[zRound].size()) && (!okay); k++) {
	                            if (((VOIContour) (curves[zRound].elementAt(k))).contains(xRound, yRound, false)) { 
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
	                        if ((zRound < 0) || (zRound >= zDim)) {
	                            continue forj;
	                        }
	                        okay = false;
	                        for (k = 0; (k < curves[zRound].size()) && (!okay); k++) {
	                            if (((VOIContour) (curves[zRound].elementAt(k))).contains(xRound, yRound, false)) { 
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
	                        if ((zRound < 0) || (zRound >= zDim)) {
	                            continue forj;
	                        }
	                        okay = false;
	                        for (k = 0; (k < curves[zRound].size()) && (!okay); k++) {
	                            if (((VOIContour) (curves[zRound].elementAt(k))).contains(xRound, yRound, false)) { 
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
	                        if ((zRound < 0) || (zRound >= zDim)) {
	                            continue forj;
	                        }
	                        okay = false;
	                        for (k = 0; (k < curves[zRound].size()) && (!okay); k++) {
	                            if (((VOIContour) (curves[zRound].elementAt(k))).contains(xRound, yRound, false)) { 
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
     * Creates a binary mask at every slice.
     *
     * @param  mask  the binary mask
     * @param  xDim  x dimension, used to index into the image
     * @param  yDim  y dimension, used to index into the image
     */
    public void createActiveContourBinaryMask(BitSet mask, int xDim, int yDim) {
        int z;

        for (z = 0; z < zDim; z++) {
            createActiveContourBinaryMask(xDim, yDim, z, mask, false);
        }
    }

    /**
     * Creates a binary mask at a slice.
     *
     * @param  xDim   x dimension, used to index into the image
     * @param  yDim   y dimension, used to index into the image
     * @param  slice  slice to create mask at
     * @param  mask   the binary mask
     * @param  twoD   DOCUMENT ME!
     */
    public void createActiveContourBinaryMask(int xDim, int yDim, int slice, BitSet mask, boolean twoD) {
        int x, y;
        int i, nGons;
        int offset;
        int offsetZ;
        nGons = curves[slice].size();
        offsetZ = slice * xDim * yDim;

        if (twoD == true) {
            offsetZ = 0;
        }

        if ((process == true) && (curveType == CONTOUR)) {

            for (i = 0; i < nGons; i++) {

                if (((VOIContour) (curves[slice].elementAt(i))).isActive()) {
                    ((VOIContour) (curves[slice].elementAt(i))).contains(0, 0, true);
                    getBounds(xBounds, yBounds, zBounds);

                    int xbs = (int) xBounds[0];
                    int xbe = (int) xBounds[1];
                    int ybs = (int) yBounds[0];
                    int ybe = (int) yBounds[1];

                    for (y = ybs; y < ybe; y++) {
                        offset = offsetZ + (y * xDim);

                        for (x = xbs; x < xbe; x++) {

                            if (((VOIContour) (curves[slice].elementAt(i))).contains(x, y, false) &&
                                    (polarity == ADDITIVE)) {
                                mask.set(offset + x);
                            } else if (((VOIContour) (curves[slice].elementAt(i))).contains(x, y, false) &&
                                           (polarity == SUBTRACTIVE)) {
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
            int size = curves[slice].size();

            for (i = 0; i < size; i++) {
                pt = ((VOIPoint) (curves[slice].elementAt(i))).exportPoint();
                offset = MipavMath.round((slice * xDim * yDim) + (pt.Y * xDim) + pt.X);

                if (polarity == ADDITIVE) {
                    mask.set(offset);
                } else if (polarity == SUBTRACTIVE) {
                    mask.clear(offset);
                } else {
                    // mask[0].clear(offset + x);
                }
            }
        }
    }

    /**
     * Forms a binary representation of the VOI into the image.
     *
     * @param  image       boolean image where VOI bits are to be set.
     * @param  XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param  onlyActive  Only mask regions that are active (i.e. selected )
     */
    public void createBinaryImage(ModelImage image, boolean XOR, boolean onlyActive) {
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int length = xDim * yDim * zDim;
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

        createBinaryMask(image.getMask(), xDim, yDim, XOR, onlyActive);

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
     * Creates a binary mask defined by the VOI.
     *
     * @param  mask  object storing the masked regions
     * @param  xDim  x dimension, used to index into the image
     * @param  yDim  y dimension, used to index into the image
     */
    public void createBinaryMask(BitSet mask, int xDim, int yDim) {
        this.createBinaryMask(mask, xDim, yDim, false, false);
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
        nGons = curves[slice].size();

        if ((process == true) && (curveType == CONTOUR)) {

            for (i = 0; i < nGons; i++) {
                ((VOIContour) (curves[slice].elementAt(i))).contains(0, 0, true);
                getBounds(xBounds, yBounds, zBounds);

                int xbs = (int) xBounds[0];
                int xbe = (int) xBounds[1];
                int ybs = (int) yBounds[0];
                int ybe = (int) yBounds[1];

                for (y = ybs; y < ybe; y++) {
                    offset = y * xDim; // a horizontal offset

                    for (x = xbs; x < xbe; x++) {

                        if (((VOIContour) (curves[slice].elementAt(i))).contains(x, y, false) &&
                                (polarity == ADDITIVE)) {

                            if (mask.get(offset + x)) {

                                // System.err.println("clearing");
                                mask.clear(offset + x);
                            } else {
                                mask.set(offset + x);
                            }
                        } else if (((VOIContour) (curves[slice].elementAt(i))).contains(x, y, false) &&
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
            int size = curves[slice].size();

            for (i = 0; i < size; i++) {
                pt = ((VOIPoint) (curves[slice].elementAt(i))).exportPoint();
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
        nGons = curves[slice].size();

        if (element >= nGons) {
            MipavUtil.displayError("element " + element + " does not exist");

            return null;
        }

        if ((process == true) && (curveType == CONTOUR)) {
            ((VOIContour) (curves[slice].elementAt(element))).contains(0, 0, true);
            getBounds(xBounds, yBounds, zBounds);

            int xbs = (int) xBounds[0];
            int xbe = (int) xBounds[1];
            int ybs = (int) yBounds[0];
            int ybe = (int) yBounds[1];

            for (y = ybs; y < ybe; y++) {
                offset = y * xDim; // a horizontal offset

                for (x = xbs; x < xbe; x++) {

                    if (((VOIContour) (curves[slice].elementAt(element))).contains(x, y, false) &&
                            (polarity == ADDITIVE)) {

                        if (mask.get(offset + x)) {

                            // System.err.println("clearing");
                            mask.clear(offset + x);
                        } else {
                            mask.set(offset + x);
                        }
                    } else if (((VOIContour) (curves[slice].elementAt(element))).contains(x, y, false) &&
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
            int size = curves[slice].size();
            pt = ((VOIPoint) (curves[slice].elementAt(element))).exportPoint();
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

        return mask;
    }

    /**
     * Creates a binary mask defined by the VOI.
     *
     * @param  mask        object storing the masked regions
     * @param  xDim        x dimension, used to index into the image
     * @param  yDim        y dimension, used to index into the image
     * @param  XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param  onlyActive  Only mask regions that are active (i.e. selected )
     */
    public void createBinaryMask(BitSet mask, int xDim, int yDim, boolean XOR, boolean onlyActive) {
        int z;

        for (z = 0; z < zDim; z++) {
            createBinaryMask(xDim, yDim, z, mask, XOR, onlyActive);
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
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int length = xDim * yDim * zDim;
        int i;
        int tDim = 1;
        int t;
        
        if (image.getNDims() >= 4) {
            tDim = image.getExtents()[3];
        }
        createBinaryMask(image.getMask(), xDim, yDim, XOR, onlyActive);

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
    public short[] createShortMask(int xDim, int yDim, short[] mask, boolean XOR) {
        int length = xDim * yDim * zDim;
        int i;
        BitSet binaryMask = null;

        try {
            binaryMask = new BitSet(length);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to make mask.");

            return null;
        }

        createBinaryMask(binaryMask, xDim, yDim, XOR, false);

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
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int length = xDim * yDim * zDim;
        int i;
        int tDim = 1;
        int t;
        
        if (image.getNDims() >= 4) {
            tDim = image.getExtents()[3];    
        }
        createBinaryMask(image.getMask(), xDim, yDim, XOR, onlyActive);

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
     * Cycles the active point of a Polyline slice.
     *
     * @param  slice      int the slice on which to cycle
     * @param  direction  int up/down/left/right
     */
    public void cyclePSlicePt(int slice, int direction) {

        if (curveType == POLYLINE_SLICE) {
            int index = pLineSliceIndex;

            switch (direction) {

                case VOIBase.UP:
                case VOIBase.RIGHT:
                    pLineSliceIndex++;
                    break;

                case VOIBase.DOWN:
                case VOIBase.LEFT:
                    pLineSliceIndex--;
                    break;
            }

            if (pLineSliceIndex < 0) {
                pLineSliceIndex = curves[slice].size() - 1;
            } else if (pLineSliceIndex > (curves[slice].size() - 1)) {
                pLineSliceIndex = 0;
            }

            polygonIndex = pLineSliceIndex;

            // redundant but will set the correct label number
            this.markPSlicePt(slice);
        }
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
     */
    public void drawBlendSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, int slice,
                              int[] pixBuffer, Graphics g, int xDim, int yDim) {
        int i;

        if (displayMode == VOI.BOUNDARY) {
            return;
        }

        for (i = 0; i < curves[slice].size(); i++) {

            // System.err.println("drawing blend self");
            if (visible) {
                g.setColor(color);

                if (curveType == CONTOUR) {
                    ((VOIContour) (curves[slice].elementAt(i))).drawBlendSelf(zoomX, zoomY, resolutionX, resolutionY, g,
                                                                              xDim, yDim, pixBuffer, opacity, color);
                }
            }
        }
    }

    /**
     * Draws the VOI, using the curveType to do so.
     *
     * @param  zoomX           zoom for the x coordinate
     * @param  zoomY           zoom for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  originX         start location of X origin
     * @param  originY         start location of Y origin
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  slice           index of slice in curve array
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               the graphics context where the VOI is to be drawn
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int slice, int orientation, Graphics g) {
        int i;
        boolean isSolid = false;

        if (displayMode == VOI.SOLID) {
            isSolid = true;
        }

        if (curveType == POLYLINE_SLICE) {
            double distance = 0;
            Vector distanceVector = new Vector();
            Vector3f firstPoint, secondPoint;
            // first calculate the total distance from pt->pt (even in !visible Points)

            for (i = 0; i < curves[slice].size(); i++) {

                firstPoint = ((VOIPoint) curves[slice].elementAt(i)).exportPoint();
                firstPoint.Z = slice;
                distanceVector.add(firstPoint);
            }

            for (i = 0; i < (distanceVector.size() - 1); i++) {
                firstPoint = (Vector3f) distanceVector.elementAt(i);
                secondPoint = (Vector3f) distanceVector.elementAt(i + 1);

                distance += MipavMath.distance(firstPoint.X, firstPoint.Y, firstPoint.Z, secondPoint.X, secondPoint.Y,
                                               secondPoint.Z, resols);
            }

            // next draw lines between the points of the visible slice

            // to do....

            // finally draw the points
            for (i = 0; i < curves[slice].size(); i++) {

                if (visible) {
                    ((VOIPoint) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY, originX,
                                                                       originY, resols, unitsOfMeasure, orientation, g,
                                                                       isSolid, thickness);
                }
            }
            // System.err.println("DISTANCE IS: " + distance);

        } else {

            for (i = 0; i < curves[slice].size(); i++) {

                if (visible) {
                    g.setColor(color);

                    if (curveType == CONTOUR) {

                        // getBounds(xBounds, yBounds, zBounds);
                        ((VOIContour) (curves[slice].elementAt(i))).getBounds(xBounds, yBounds, zBounds);
                        ((VOIContour) (curves[slice].elementAt(i))).setBounds(xBounds, yBounds, zBounds);
                        ((VOIContour) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                             originX, originY, resols, unitsOfMeasure,
                                                                             orientation, g, boundingBox, thickness);
                    } else if (curveType == LINE) {
                        ((VOILine) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                          originX, originY, resols, unitsOfMeasure,
                                                                          orientation, g, isSolid, thickness);
                    } else if (curveType == POINT) {
                        ((VOIPoint) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                           originX, originY, resols, unitsOfMeasure,
                                                                           orientation, g, isSolid, thickness);
                    } else if (curveType == PROTRACTOR) {
                        ((VOIProtractor) (curves[slice].elementAt(i))).setColor(color);
                        ((VOIProtractor) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                                originX, originY, resols,
                                                                                unitsOfMeasure, orientation, g,
                                                                                isSolid, thickness);
                    } else if (curveType == ANNOTATION) {
                        ((VOIText) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                          originX, originY, resols, unitsOfMeasure,
                                                                          orientation, g, isSolid, thickness);
                    } else if (curveType == CARDIOLOGY) {

                        // System.err.println("got here");
                        ((VOICardiology) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                                originX, originY, resols,
                                                                                unitsOfMeasure, orientation, g,
                                                                                isSolid, thickness);
                    }
                }
            }
        }
    }

    /**
     * Draws the VOI, using the curveType to do so.
     *
     * @param  zoomX           zoom for the x coordinate
     * @param  zoomY           zoom for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  originX         start location of X origin
     * @param  originY         start location of Y origin
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  slice           index of slice in curve array
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  fileInfo        DOCUMENT ME!
     * @param  dim             DOCUMENT ME!
     * @param  g               the graphics context where the VOI is to be drawn
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int slice, int orientation, FileInfoBase fileInfo,
                         int dim, Graphics g) {
        int i;
        boolean isSolid = false;

        if (displayMode == VOI.SOLID) {
            isSolid = true;
        }


        if (curveType == POLYLINE_SLICE) {

            try {
                // break this into other function (for calculating distance...2.5D or 3D

                boolean is25D = fileInfo.getIs2_5D();
                double distance = 0;
                Vector distanceVector = new Vector();
                Vector3f firstPoint, secondPoint;

                // first calculate the total distance from pt->pt (even in !visible Points)

                int numSlices = fileInfo.getExtents()[2];
                int sliceCounter;
                int pointNumber = 0;

                for (sliceCounter = 0; sliceCounter < numSlices; sliceCounter++) {

                    for (i = 0; i < curves[sliceCounter].size(); i++) {

                        try {
                            pointNumber = Integer.parseInt(((VOIPoint) curves[sliceCounter].elementAt(i)).getLabel());
                            firstPoint = ((VOIPoint) curves[sliceCounter].elementAt(i)).exportPoint();

                            if (is25D) {
                                firstPoint.Z = 0;
                            } else {
                                firstPoint.Z = sliceCounter;
                            }

                            distanceVector.add(new PolyPointHolder(firstPoint, pointNumber));
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }

                Collections.sort(distanceVector, new PointComparator());

                double dTemp = 0;
                String sDistStr = "";
                double sDist = 0;

                for (i = 0; i < (distanceVector.size() - 1); i++) {
                    firstPoint = ((PolyPointHolder) distanceVector.elementAt(i)).getPoint();
                    secondPoint = ((PolyPointHolder) distanceVector.elementAt(i + 1)).getPoint();
                    dTemp = MipavMath.distance(firstPoint.X, firstPoint.Y, firstPoint.Z, secondPoint.X, secondPoint.Y,
                                               secondPoint.Z, resols);
                    distance += dTemp;

                    if (i == activePolylineSlicePoint) {
                        sDist = dTemp;
                    }

                }

                // next draw lines between the points of the visible slice
                if (visible) {
                    int xS, yS, xS2, yS2;
                    int firstPointLabel;
                    int secondPointLabel;

                    for (i = 0; i < (curves[slice].size() - 1); i++) {
                        g.setColor(color);

                        firstPointLabel = 0;
                        secondPointLabel = 0;

                        try {
                            firstPointLabel = Integer.parseInt(((VOIPoint) curves[slice].elementAt(i)).getLabel());
                            secondPointLabel = Integer.parseInt(((VOIPoint) curves[slice].elementAt(i + 1)).getLabel());
                        } catch (Exception e) {
                            e.printStackTrace();
                        }

                        if ((secondPointLabel - firstPointLabel) == 1) {
                            firstPoint = ((VOIPoint) curves[slice].elementAt(i)).exportPoint();
                            secondPoint = ((VOIPoint) curves[slice].elementAt(i + 1)).exportPoint();

                            if ((orientation == VOIBase.NA) || (orientation == VOIBase.XY)) {

                                // 1 -> imageDim instead of 0 -> imageDim - 1
                                xS = Math.round(firstPoint.X * zoomX * resolutionX);
                                yS = Math.round(firstPoint.Y * zoomY * resolutionY);

                                xS2 = Math.round(secondPoint.X * zoomX * resolutionX);
                                yS2 = Math.round(secondPoint.Y * zoomY * resolutionY);

                            } else if (orientation == VOIBase.ZY) {

                                // 1 -> imageDim instead of 0 -> imageDim - 1
                                xS = Math.round(firstPoint.Z * zoomX * resolutionX);
                                yS = Math.round(firstPoint.Y * zoomY * resolutionY);

                                xS2 = Math.round(secondPoint.Z * zoomX * resolutionX);
                                yS2 = Math.round(secondPoint.Y * zoomY * resolutionY);

                            } else if (orientation == VOIBase.XZ) {

                                // 1 -> imageDim instead of 0 -> imageDim - 1
                                xS = Math.round(firstPoint.X * zoomX * resolutionX);
                                yS = Math.round(firstPoint.Z * zoomY * resolutionY);

                                xS2 = Math.round(secondPoint.X * zoomX * resolutionX);
                                yS2 = Math.round(secondPoint.Z * zoomY * resolutionY);

                            } else {

                                // 1 -> imageDim instead of 0 -> imageDim - 1
                                xS = Math.round(firstPoint.X * zoomX * resolutionX);
                                yS = Math.round(firstPoint.Y * zoomY * resolutionY);

                                xS2 = Math.round(secondPoint.X * zoomX * resolutionX);
                                yS2 = Math.round(secondPoint.Y * zoomY * resolutionY);
                            }

                            g.drawLine(xS, yS, xS2, yS2);
                        }
                    }
                }

                DecimalFormat n = new DecimalFormat("0.00");
                sDistStr = n.format(sDist);

                String tmpString = n.format(distance);

                switch (unitsOfMeasure[0]) {

                    case FileInfoBase.INCHES:
                        tmpString = tmpString + " in";
                        break;

                    case FileInfoBase.ANGSTROMS:
                        tmpString = tmpString + " A";
                        break;

                    case FileInfoBase.NANOMETERS:
                        tmpString = tmpString + " nm";
                        break;

                    case FileInfoBase.MICROMETERS:
                        tmpString = tmpString + " um";
                        break;

                    case FileInfoBase.MILLIMETERS:
                        tmpString = tmpString + " mm";
                        break;

                    case FileInfoBase.CENTIMETERS:
                        tmpString = tmpString + " cm";
                        break;

                    case FileInfoBase.METERS:
                        tmpString = tmpString + " m";
                        break;

                    case FileInfoBase.KILOMETERS:
                        tmpString = tmpString + " km";
                        break;

                    case FileInfoBase.MILES:
                        tmpString = tmpString + " miles";
                        break;

                    default:
                        tmpString += " Unknown";
                }

                sDistStr += tmpString.substring(tmpString.indexOf(" ") + 1, tmpString.length());

                // finally draw the points
                for (i = 0; i < curves[slice].size(); i++) {

                    if (visible) {
                        g.setColor(color);

                        if ((i == 0) && (i == pLineSliceIndex)) {
                            ((VOIPoint) (curves[slice].elementAt(i))).setFirstPoint(true, true, tmpString, sDistStr);
                        } else if (i == 0) {
                            ((VOIPoint) (curves[slice].elementAt(i))).setFirstPoint(true, false, tmpString, null);
                        } else if (i == pLineSliceIndex) {
                            ((VOIPoint) (curves[slice].elementAt(i))).setFirstPoint(false, true, null, sDistStr);
                        } else {
                            ((VOIPoint) (curves[slice].elementAt(i))).setFirstPoint(false, false, null, null);
                        }

                        ((VOIPoint) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                           originX, originY, resols, unitsOfMeasure,
                                                                           orientation, g, isSolid, thickness);

                    }
                }
                // System.err.println("Distance IS: " + distance);
            } catch (Exception e) {
                System.err.println("ERROR ERROR ERROR.....exception thrown in VOI drawSelf");
                e.printStackTrace();
                System.err.println("CURVES @ slice size: " + curves[slice].size());
            }
        } else {

            for (i = 0; i < curves[slice].size(); i++) {

                // gon = ((Contour)(curves[slice].elementAt(i))).exportPolygon(scaleX, scaleY);
                // gon = ((Contour)(curves[slice].elementAt(i))).generatePolygon(0,0,0,0,0,0,scaleX, scaleY,scaleY);
                if (visible) {
                    g.setColor(color);

                    if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

                        // getBounds(xBounds, yBounds, zBounds);
                        ((VOIContour) (curves[slice].elementAt(i))).getBounds(xBounds, yBounds, zBounds);
                        ((VOIContour) (curves[slice].elementAt(i))).setBounds(xBounds, yBounds, zBounds);

                        ((VOIContour) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                             originX, originY, resols, unitsOfMeasure,
                                                                             orientation, g, boundingBox, fileInfo,
                                                                             dim, thickness);
                    } else if (curveType == LINE) {
                        ((VOILine) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                          originX, originY, resols, unitsOfMeasure,
                                                                          orientation, g, isSolid, thickness);
                    } else if (curveType == POINT) {
                        ((VOIPoint) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                           originX, originY, resols, unitsOfMeasure,
                                                                           orientation, g, isSolid, thickness);
                    } else if (curveType == PROTRACTOR) {
                        ((VOIProtractor) (curves[slice].elementAt(i))).setColor(color);
                        ((VOIProtractor) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                                originX, originY, resols,
                                                                                unitsOfMeasure, orientation, g,
                                                                                isSolid, thickness);
                    } else if (curveType == ANNOTATION) {

                        // System.err.println("Text: " + ((VOIText)(curves[slice].elementAt(i))).getText());
                        ((VOIText) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                          originX, originY, resols, unitsOfMeasure,
                                                                          orientation, g, isSolid, thickness);
                    } else if (curveType == CARDIOLOGY) {

                        // System.err.println("got here");
                        ((VOICardiology) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                                originX, originY, resols,
                                                                                unitsOfMeasure, orientation, g,
                                                                                isSolid, thickness);
                    }
                }
            }
        }
    }

    /**
     * Draws the Vertices of the VOI, using the curveType to do so.
     *
     * @param  zoomX           zoom for the x coordinate
     * @param  zoomY           zoom for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  slice           index of slice in curve array
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               the graphics context where the vertices are to be drawn
     */
    public void drawVertices(float zoomX, float zoomY, float resolutionX, float resolutionY, float[] resols,
                             int[] unitsOfMeasure, int slice, int orientation, Graphics g) {
        int i;
        boolean isSolid = false;

        if (displayMode == VOI.SOLID) {
            isSolid = true;
        }

        for (i = 0; i < curves[slice].size(); i++) {

            if (visible) {
                g.setColor(color);

                if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                    ((VOIContour) (curves[slice].elementAt(i))).drawVertices(zoomX, zoomY, resolutionX, resolutionY, g,
                                                                             boundingBox);
                } else if (curveType == LINE) {
                    ((VOILine) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0f, 0f,
                                                                      resols, unitsOfMeasure, orientation, g, isSolid, thickness);
                } else if (curveType == POINT) {
                    ((VOIPoint) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0f, 0f,
                                                                       resols, unitsOfMeasure, orientation, g, isSolid, thickness);
                } else if (curveType == PROTRACTOR) {
                    ((VOIProtractor) (curves[slice].elementAt(i))).setColor(color);
                    ((VOIProtractor) (curves[slice].elementAt(i))).drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0f,
                                                                            0f, resols, unitsOfMeasure, orientation, g,
                                                                            isSolid, thickness);
                }
            }
        }
    }

    /**
     * Draws the Vertices of the VOI, using the curveType to do so.
     *
     * @param  zoomX           zoom for the x coordinate
     * @param  zoomY           zoom for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  slice           index of slice in curve array
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               the graphics context where the vertices are to be drawn
     * @param  element         element in slice
     */
    public void drawVertices(float zoomX, float zoomY, float resolutionX, float resolutionY, float[] resols,
                             int[] unitsOfMeasure, int slice, int orientation, Graphics g, int element) {
        boolean isSolid = false;

        if (displayMode == VOI.SOLID) {
            isSolid = true;
        }

        if (visible) {
            g.setColor(color);

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

                // getBounds and setBounds are not called because they are called in nearPoint
                // and in ViewJComponentEditImage a single contour call to nearPoint precedes a
                // single contour call to drawVertices
                ((VOIContour) (curves[slice].elementAt(element))).drawVertices(zoomX, zoomY, resolutionX, resolutionY,
                                                                               g, boundingBox);
            } else if (curveType == LINE) {
                ((VOILine) (curves[slice].elementAt(element))).drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0f, 0f,
                                                                        resols, unitsOfMeasure, orientation, g,
                                                                        isSolid, thickness);
            } else if (curveType == POINT) {
                ((VOIPoint) (curves[slice].elementAt(element))).drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0f, 0f,
                                                                         resols, unitsOfMeasure, orientation, g,
                                                                         isSolid, thickness);
            } else if (curveType == PROTRACTOR) {
                ((VOIProtractor) (curves[slice].elementAt(element))).setColor(color);
                ((VOIProtractor) (curves[slice].elementAt(element))).drawSelf(zoomX, zoomY, resolutionX, resolutionY,
                                                                              0f, 0f, resols, unitsOfMeasure,
                                                                              orientation, g, isSolid, thickness);
            }
        }
    }

    /**
     * Special function to draw tick marks on the VOILine object.
     *
     * @param  slice        slice where the line is located
     * @param  g            graphics to draw in
     * @param  res          resolutions of the image
     * @param  units        units of measure (mm or inches)
     * @param  xDim         DOCUMENT ME!
     * @param  yDim         DOCUMENT ME!
     * @param  zoomX        zoom in the x direction
     * @param  zoomY        zoom in the y direction
     * @param  resolutionX  image resolution X (aspect ratio)
     * @param  resolutionY  image resolution Y (aspect ratio)
     */
    public void drawVOISpecial(int slice, Graphics g, float[] res, int[] units, int xDim, int yDim, float zoomX,
                               float zoomY, float resolutionX, float resolutionY) {
        drawVOISpecial(slice, g, res, units, xDim, yDim, zoomX, zoomY, resolutionX, resolutionY, 0);
    }

    /**
     * Special function to draw tick marks on the VOILine object.
     *
     * @param  slice        slice where the line is located
     * @param  g            graphics to draw in
     * @param  res          resolutions of the image
     * @param  units        units of measure (mm or inches)
     * @param  xDim         DOCUMENT ME!
     * @param  yDim         DOCUMENT ME!
     * @param  zoomX        zoom in the x direction
     * @param  zoomY        zoom in the y direction
     * @param  resolutionX  image resolution X (aspect ratio)
     * @param  resolutionY  image resolution Y (aspect ratio)
     * @param  element      which curve element to update
     */
    public void drawVOISpecial(int slice, Graphics g, float[] res, int[] units, int xDim, int yDim, float zoomX,
                               float zoomY, float resolutionX, float resolutionY, int element) {

        if (curveType == LINE) {
            ((VOILine) (curves[slice].elementAt(element))).drawTickMarks(g, color, units, xDim, yDim, res, zoomX, zoomY,
                                                                         resolutionX, resolutionY);
        } else if (curveType == PROTRACTOR) {
            ((VOIProtractor) (curves[slice].elementAt(element))).setColor(color);
            ((VOIProtractor) (curves[slice].elementAt(element))).showProtractorWithAngle(g, res, units, xDim, yDim,
                                                                                         zoomX, zoomY, resolutionX,
                                                                                         resolutionY, false, thickness);

            return;
        } else if (curveType == CONTOUR) {
            return;
        } else if (curveType == POINT) { }
        else {
            return;
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
        int i, j, k;

        if (curveType != POINT) {
            return null;
        }

        int len = 0;

        for (i = 0; i < curves.length; i++) {
            len += curves[i].size();
        }

        try {
            points = new Vector3f[len];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to export Points.");

            return null;
        }

        if (curveType != POINT) {
            return null;
        }

        k = 0;

        for (j = 0; j < curves.length; j++) {

            for (i = 0; i < curves[j].size(); i++) {
                points[k++] = ((VOIPoint) (curves[j].elementAt(i))).exportPoint();
            }
        }

        return points;
    }

    /**
     * Gets line arrays from the VOI; can only use with LINE, POINT, and PROTRACTOR.
     *
     * @param  x      array of x coordinates of line
     * @param  y      array of y coordinates of line
     * @param  z      array of z coordinates of line
     * @param  slice  index of slice
     */
    public void exportArrays(float[] x, float[] y, float[] z, int slice) {

        for (int i = 0; i < curves[slice].size(); i++) {
            exportArrays(x, y, z, slice, i);
        }
    }

    /**
     * Gets line arrays from the VOI; can only use with LINE, POINT, and PROTRACTOR.
     *
     * @param  x        array of x coordinates of line
     * @param  y        array of y coordinates of line
     * @param  z        array of z coordinates of line
     * @param  slice    index of slice
     * @param  element  the element whose arrays should be exported
     */
    public void exportArrays(float[] x, float[] y, float[] z, int slice, int element) {

        if (curveType == LINE) {
            ((VOILine) (curves[slice].elementAt(element))).exportArrays(x, y, z);
        } else if (curveType == PROTRACTOR) {
            ((VOIProtractor) (curves[slice].elementAt(element))).exportArrays(x, y, z);
        }
    }

    /**
     * This method no longer makes sense!!! must remove exportPoint - if VOI is of POINT type then return it.
     *
     * @return  return the request point from the VOIPoint object
     */
    public Vector3f exportPoint() {
        int i, j;

        if (curveType != POINT) {
            return null;
        }

        for (j = 0; j < zDim; j++) {

            for (i = 0; i < curves[j].size(); i++) {

                if (curveType == POINT) {
                    return ((VOIPoint) (curves[j].elementAt(i))).exportPoint();
                }
            }
        }

        return null;
    }

    /**
     * Get Vector3fs from the VOI; can only use with Point.
     *
     * @param   slice  index of slice
     *
     * @return  array of points at the slice
     */
    public Vector3f[] exportPoints(int slice) {
        Vector3f[] points;

        if (curveType != POINT) {
            return null;
        }

        try {
            points = new Vector3f[curves[slice].size()];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to export Points.");

            return null;
        }

        for (int i = 0; i < curves[slice].size(); i++) {

            if (curveType == POINT) {
                points[i] = ((VOIPoint) (curves[slice].elementAt(i))).exportPoint();
            }
        }

        return points;
    }

    /**
     * Gets polygons from the VOI; can only use with Contour.
     *
     * @param   slice  index of slice
     *
     * @return  array of polygons at the slice
     */
    public Polygon[] exportPolygons(int slice) {
        Polygon[] polygons;

        try {
            polygons = new Polygon[curves[slice].size()];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to export Polygon.");

            return null;
        }

        if ((curveType == LINE) || (curveType == POINT) || (curveType == PROTRACTOR) || (curveType == ANNOTATION) ||
                (curveType == POLYLINE_SLICE)) {
            return null;
        }

        for (int i = 0; i < curves[slice].size(); i++) {
            polygons[i] = ((VOIContour) (curves[slice].elementAt(i))).exportPolygon(1, 1, 1, 1);
        }

        return polygons;
    }

    /**
     * Exports polygons of contour.
     *
     * @param   tMatrix  transformation matrix
     *
     * @return  returns the polygon
     */
    public Polygon[][] exportPolygons(TransMatrix tMatrix) {
        int i, z;
        Polygon[][] transformedGon = null;

        if (curveType == CONTOUR) {

            for (z = 0; z < zDim; z++) {

                for (i = 0; i < curves[z].size(); i++) {
                    transformedGon[z][i] = ((VOIContour) (curves[z].elementAt(i))).exportPolygon(tMatrix);
                }
            }
        }

        return transformedGon;
    }


    /**
     * Exports the polygon of the contour with some transformation.
     *
     * @param   thetaX  rotation in x in degrees
     * @param   thetaY  rotation in y in degrees
     * @param   thetaZ  rotation in z in degrees
     * @param   tX      translation in x
     * @param   tY      translation in y
     * @param   tZ      translation in z
     * @param   scaleX  zoom in x
     * @param   scaleY  zoom in y
     * @param   scaleZ  zoom in z
     *
     * @return  returns polygon
     */
    public Polygon[][] exportPolygons(float thetaX, float thetaY, float thetaZ, float tX, float tY, float tZ,
                                      float scaleX, float scaleY, float scaleZ) {
        int i;
        int z;
        Polygon[][] transformedGon = null;
        Vector3f gcPt = null;
        TransMatrix tMatrix = null;

        try {
            gcPt = new Vector3f();
            tMatrix = new TransMatrix(4);
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: VOI exportPolygons.");

            return null;
        }

        gcPt = getGeometricCenter();

        // construct transMatrix object
        tMatrix.setTranslate((gcPt.X + tX), (gcPt.Y + tY), (gcPt.Z + tZ));
        tMatrix.setRotate(thetaX, thetaY, thetaZ, TransMatrix.DEGREES);
        tMatrix.setZoom(scaleX, scaleY, scaleZ);
        tMatrix.setTranslate(-gcPt.X, -gcPt.Y, -gcPt.Z);

        if (curveType == CONTOUR) {

            for (z = 0; z < zDim; z++) {

                for (i = 0; i < curves[z].size(); i++) {
                    transformedGon[z][i] = ((VOIContour) (curves[z].elementAt(i))).exportPolygon(tMatrix);
                }
            }
        }

        return transformedGon;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   slice  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f exportPSlicePoint(int slice) {

        if (curveType == POLYLINE_SLICE) {

            try {
                return ((VOIPoint) (curves[slice].elementAt(pLineSliceIndex))).exportPoint();
            } catch (Exception e) {
                return null;
            }

        }


        return null;
    }

    /**
     * Prepares the class for cleanup.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {
        int i;

        if (curves != null) {

            for (i = 0; i < curves.length; i++) {
                Vector cv = curves[i];

                for (int j = cv.size() - 1; j >= 0; j--) {
                    VOIBase base = (VOIBase) cv.remove(j);
                    base.clear();
                }

                cv.clear();
                cv = null;
            }

            curves = null;
        }

        stats = null;
        xBounds = null;
        yBounds = null;
        zBounds = null;
        position = null;
        intensity = null;

        if (rgbPositions != null) {

            for (i = 0; i < rgbPositions.length; i++) {
                rgbPositions[i] = null;
            }

            rgbPositions = null;
        }

        if (rgbIntensities != null) {

            for (i = 0; i < rgbIntensities.length; i++) {
                rgbIntensities[i] = null;
            }

            rgbIntensities = null;
        }

        super.finalize();
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
     *
     * @return  number of valid points in the array
     */
    public int findPositionAndIntensity(int slice, int contourNo, float[] position, float[] intensity,
                                        float[] imageBuffer, float[] resolutions, int xDim, int yDim) {

        if (curveType == LINE) {
            return ((VOILine) (curves[slice].elementAt(contourNo))).findPositionAndIntensity(position, intensity,
                                                                                             imageBuffer, resolutions,
                                                                                             xDim, yDim);
        }

        if (curveType == PROTRACTOR) {
            return 0;
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            return ((VOIContour) (curves[slice].elementAt(contourNo))).findPositionAndIntensity(position, intensity,
                                                                                                imageBuffer,
                                                                                                resolutions, xDim,
                                                                                                yDim);
        } else if (curveType == POINT) {
            return 0;
        } else {
            return 0;
        }
    }

    /**
     * Finds the active Contour and returns it.
     *
     * @param   slice  indicates slice where active Contour can be found.
     *
     * @return  Contour that is active or null if no contour is active in the indicated slice.
     */
    public VOIBase getActiveContour(int slice) {
        int i;

        for (i = 0; i < curves[slice].size(); i++) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

                if (((VOIContour) (curves[slice].elementAt(i))).isActive()) {
                    return (VOIContour) (curves[slice].elementAt(i));
                }
            }
        }

        return null;
    }

    /**
     * Finds the active Contour and returns it.
     *
     * @param   slice  indicates slice where active Contour can be found.
     *
     * @return  index of Contour that is active or -1 if not contour is active in the indicated slice.
     */
    public int getActiveContourIndex(int slice) {
        int i;

        for (i = 0; i < curves[slice].size(); i++) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

                if (((VOIContour) (curves[slice].elementAt(i))).isActive()) {
                    return i;
                }
            } else if (curveType == POINT) {

                if (((VOIPoint) (curves[slice].elementAt(i))).isActive()) {
                    return i;
                }
            }
        }

        return -1;
    }

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
    public void getBounds(int[] x, int[] y, int[] z) {
        int i, slice;
        int nContours;
        int xStart = 10000, xEnd = -1;
        int yStart = 10000, yEnd = -1;
        int zStart = -1, zEnd = -1;

        for (slice = 0; slice < zDim; slice++) {

            if ((zStart == -1) && (curves[slice].size() != 0)) {
                zStart = slice;
            }

            if ((zStart != -1) && (curves[slice].size() != 0)) {
                zEnd = slice;
            }

            nContours = curves[slice].size();

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

                for (i = 0; i < nContours; i++) {
                    ((VOIContour) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            }

            if (curveType == LINE) {

                for (i = 0; i < nContours; i++) {
                    ((VOILine) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            }

            if (curveType == PROTRACTOR) {

                for (i = 0; i < nContours; i++) {
                    ((VOIProtractor) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            }

            if (curveType == POINT) {

                for (i = 0; i < nContours; i++) {
                    ((VOIPoint) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            }
        }

        x[0] = xStart;
        x[1] = xEnd;
        y[0] = yStart;
        y[1] = yEnd;
        z[0] = zStart;
        z[1] = zEnd;
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
        int i, slice;
        int nContours;
        float xStart = 10000, xEnd = -1;
        float yStart = 10000, yEnd = -1;
        float zStart = -1, zEnd = -1;

        for (slice = 0; slice < zDim; slice++) {

            if ((zStart == -1) && (curves[slice].size() != 0)) {
                zStart = slice;
            }

            if ((zStart != -1) && (curves[slice].size() != 0)) {
                zEnd = slice;
            }

            nContours = curves[slice].size();

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

                for (i = 0; i < nContours; i++) {
                    ((VOIContour) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            } else if (curveType == LINE) {

                for (i = 0; i < nContours; i++) {
                    ((VOILine) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            } else if (curveType == PROTRACTOR) {

                for (i = 0; i < nContours; i++) {
                    ((VOIProtractor) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            } else if ((curveType == POINT) || (curveType == POLYLINE_SLICE)) {

                for (i = 0; i < nContours; i++) {
                    ((VOIPoint) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            } else if (curveType == ANNOTATION) {

                for (i = 0; i < nContours; i++) {
                    ((VOIText) (curves[slice].elementAt(i))).getBounds(x, y, z);

                    if (x[0] < xStart) {
                        xStart = x[0];
                    }

                    if (x[1] > xEnd) {
                        xEnd = x[1];
                    }

                    if (y[0] < yStart) {
                        yStart = y[0];
                    }

                    if (y[1] > yEnd) {
                        yEnd = y[1];
                    }
                }
            }
        }

        x[0] = xStart;
        x[1] = xEnd;
        y[0] = yStart;
        y[1] = yEnd;
        z[0] = zStart;
        z[1] = zEnd;
    }

    /**
     * Returns the geometric center of the VOI (only contour).
     *
     * @return  returns the geometric center
     */
    public Vector3f getGeometricCenter() {
        int i, z;
        int ncurves = 0;
        Vector3f tempPt = new Vector3f(0, 0, 0);
        float sumX = (float) 0.0;
        float sumY = (float) 0.0;
        float sumZ = (float) 0.0;

        if ((curveType == LINE) || (curveType == POLYLINE) || (curveType == PROTRACTOR)) {
            return null;
        }

        for (z = 0; z < zDim; z++) {

            if (curves[z].size() != 0) {

                for (i = 0; i < curves[z].size(); i++) {
                    tempPt = ((VOIContour) (curves[z].elementAt(i))).getGeometricCenter();
                    ncurves++;
                    sumX += tempPt.X;
                    sumY += tempPt.Y;
                    sumZ += tempPt.Z;
                }
            }
        }

        return (new Vector3f(sumX / ncurves, sumY / ncurves, sumZ / ncurves));
    }

    /**
     * Caclulates the geometric center for all the contours on a particular slice. (This should find for seperate, but
     * grouped, contours as well, but this hasn't been proven!)
     *
     * @param   slice  DOCUMENT ME!
     *
     * @return  returns the geometric center
     */
    public Vector3f getGeometricCenter(int slice) {
        int i;
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
        if (curves[slice].size() != 0) {

            for (i = 0; i < curves[slice].size(); i++) {
                ncurves++;
                tempPt = ((VOIContour) (curves[slice].elementAt(i))).getGeometricCenter();
                sumX += tempPt.X;
                sumY += tempPt.Y;
                sumZ += tempPt.Z;
            }
        }

        return (new Vector3f(sumX / ncurves, sumY / ncurves, sumZ / ncurves));
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
    public Vector<VOIBase>[] getCurves() {
        return curves;
    }

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
     * Accessor that returns the ID.
     *
     * @return  the ID
     */
    public short getID() {
        return ID;
    }

    /**
     * Accessor that returns the intensity array to the parameter.
     *
     * @return  DOCUMENT ME!
     */
    public float[] getIntensity() {
        return intensity;
    }

    /**
     * Accessor that returns the level of the levelset VOI.
     *
     * @return  The level
     */
    public float getLevel() {
        return level;
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
        int i;

        if (curveType != POINT) {
            return -1;
        }

        int len = 0;

        for (i = 0; i < curves.length; i++) {
            len += curves[i].size();
        }

        return len;
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
        int i, slice;
        int nContours;

        for (slice = 0; slice < zDim; slice++) {
            nContours = curves[slice].size();

            for (i = 0; i < nContours; i++) {
                ((VOIPoint) (curves[slice].elementAt(i))).getCoordinates(coord);
            }
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
     * Accessor that returns the position array to the parameter.
     *
     * @return  DOCUMENT ME!
     */
    public float[] getPosition() {
        return position;
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
            return ((VOILine) (curves[slice].elementAt(0))).getPositionAndIntensity(position, intensity, imageBuffer,
                                                                                    xDim);
        } else if (curveType == PROTRACTOR) {
            return 0;
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            return ((VOIContour) (curves[slice].elementAt(contourNo))).getPositionAndIntensity(position, intensity,
                                                                                               imageBuffer, xDim);
        } else if (curveType == POINT) {
            return 0;
        } else {
            return 0;
        }
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
    public int getPositionAndIntensityIndex(int slice, int contourNo, int[] position, float[] intensity,
                                            float[] imageBuffer, int xDim) {

        if (curveType == LINE) {
            return ((VOILine) (curves[slice].elementAt(0))).getPositionAndIntensityIndex(position, intensity,
                                                                                         imageBuffer, xDim);
        }

        if (curveType == PROTRACTOR) {
            return 0;
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            return ((VOIContour) (curves[slice].elementAt(contourNo))).getPositionAndIntensityIndex(position, intensity,
                                                                                                    imageBuffer, xDim);
        } else if (curveType == POINT) {
            return 0;
        } else {
            return 0;
        }
    }

    /**
     * Accessor that returns the process.
     *
     * @return  the process
     */
    public boolean getProcess() {
        return process;
    }

    /**
     * Accessor that returns the rgb intensity array to the parameter.
     *
     * @return  DOCUMENT ME!
     */
    public float[][] getRGBIntensities() {
        return rgbIntensities;
    }

    /**
     * Accessor that returns the rgb position array to the parameter.
     *
     * @return  DOCUMENT ME!
     */
    public float[][] getRGBPositions() {
        return rgbPositions;
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
    public void getSliceBounds(int slice, float[] x, float[] y) {
        int i;
        int nContours;
        float[] z = new float[2];
        float xStart = 10000, xEnd = -1;
        float yStart = 10000, yEnd = -1;
        nContours = curves[slice].size();

        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

            for (i = 0; i < nContours; i++) {
                ((VOIContour) (curves[slice].elementAt(i))).getBounds(x, y, z);

                if (x[0] < xStart) {
                    xStart = x[0];
                }

                if (x[1] > xEnd) {
                    xEnd = x[1];
                }

                if (y[0] < yStart) {
                    yStart = y[0];
                }

                if (y[1] > yEnd) {
                    yEnd = y[1];
                }
            }
        }

        if (curveType == LINE) {

            for (i = 0; i < nContours; i++) {
                ((VOILine) (curves[slice].elementAt(i))).getBounds(x, y, z);

                if (x[0] < xStart) {
                    xStart = x[0];
                }

                if (x[1] > xEnd) {
                    xEnd = x[1];
                }

                if (y[0] < yStart) {
                    yStart = y[0];
                }

                if (y[1] > yEnd) {
                    yEnd = y[1];
                }
            }
        }

        if (curveType == PROTRACTOR) {

            for (i = 0; i < nContours; i++) {
                ((VOIProtractor) (curves[slice].elementAt(i))).getBounds(x, y, z);

                if (x[0] < xStart) {
                    xStart = x[0];
                }

                if (x[1] > xEnd) {
                    xEnd = x[1];
                }

                if (y[0] < yStart) {
                    yStart = y[0];
                }

                if (y[1] > yEnd) {
                    yEnd = y[1];
                }
            }
        }

        if ((curveType == POINT) || (curveType == POLYLINE_SLICE)) {

            for (i = 0; i < nContours; i++) {
                ((VOIPoint) (curves[slice].elementAt(i))).getBounds(x, y, z);

                if (x[0] < xStart) {
                    xStart = x[0];
                }

                if (x[1] > xEnd) {
                    xEnd = x[1];
                }

                if (y[0] < yStart) {
                    yStart = y[0];
                }

                if (y[1] > yEnd) {
                    yEnd = y[1];
                }
            }
        }

        x[0] = xStart;
        x[1] = xEnd;
        y[0] = yStart;
        y[1] = yEnd;
        z = null;
    }

    /**
     * Accessor that returns the statistic list used to indicate which statistics are to be calculated for the VOI.
     *
     * @return  statistics list
     */
    public ViewList[] getStatisticList() {
        return stats;
    }

    /**
     * Returns the thickness of the VOI
     * @return
     */
    public int getThickness() {
    	return this.thickness;
    }
    
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
     */
    public boolean getTotalIntensity() {
        return totalIntensity;
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
		return zDim;
	}

	/**
     * Imports the curve into the VOI.
     *
     * @param  curve  curve to import
     */
    public void importCurve(VOICardiology curve) {
        curves[0].addElement(curve);
        ((VOICardiology) (curves[0].lastElement())).setLabel(String.valueOf(elementLabel++));
    }

    public void importCurve(VOIText curve, int slice) {
    	
    	for (int i = 0; i < zDim; i++) {
    		this.removeCurves(i);
    	}
    	
    	Vector3f pt1 = (Vector3f)curve.elementAt(0);
    	Vector3f pt2 = (Vector3f)curve.elementAt(1);

    	pt1.Z = slice;
    	pt2.Z = slice;
    	curve.removeAllElements();
    	curve.addElement(pt1);
    	curve.addElement(pt2);
    	
    	curves[slice].addElement(curve);
    }
    
    /**
     * Imports the curve into the VOI.
     *
     * @param  curve  curve to import
     * @param  slice  index of slice of curve
     */
    public void importCurve(VOIContour curve, int slice) {
        curve.setName(name);
        curves[slice].addElement((VOIContour)curve.clone());

        if (curveType == PROTRACTOR) {

            for (int i = 0; i < curves[slice].size(); i++) {
                ((VOIProtractor) (curves[slice].elementAt(i))).setColor(color);
            }
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    /**
     * Imports the curve into the VOI, testing for which type.
     *
     * @param  pt     array of 3D points to import
     * @param  slice  index of slice of curve
     */
    public void importCurve(Vector3f[] pt, int slice) {
        VOIBase curve;

        if (curveType == LINE) {
            curve = new VOILine(name);
        } else if (curveType == CONTOUR) {
            curve = new VOIContour(name, true);
        } else if (curveType == POLYLINE) {
            curve = new VOIContour(name, false);
        } else if (curveType == POINT) {
            curve = new VOIPoint(name);
        } else if (curveType == PROTRACTOR) {
            curve = new VOIProtractor();
        } else {
            return;
        }

        curve.importPoints(pt);
        curves[slice].addElement(curve);

        if (curveType == POINT) {
            ((VOIPoint) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
        } else if (curveType == PROTRACTOR) {

            for (int i = 0; i < curves[slice].size(); i++) {
                ((VOIProtractor) (curves[slice].elementAt(i))).setColor(color);
            }
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    /**
     * Imports the curve into the VOI, testing for which type.
     *
     * @param  x      array of x coordinates to import
     * @param  y      array of y coordinates to import
     * @param  z      array of z coordinates to import
     * @param  slice  index of slice of curve
     */
    public void importCurve(int[] x, int[] y, int[] z, int slice) {
        VOIBase curve;

        if (curveType == LINE) {
            curve = new VOILine(name);
        } else if (curveType == CONTOUR) {
            curve = new VOIContour(name, true);
        } else if (curveType == POLYLINE) {
            curve = new VOIContour(name, false);
        } else if (curveType == POINT) {
            curve = new VOIPoint(name);
        } else if (curveType == PROTRACTOR) {
            curve = new VOIProtractor();
        } else {
            return;
        }

        curve.importArrays(x, y, z, x.length);
        curves[slice].addElement(curve);

        if (curveType == POINT) {
            ((VOIPoint) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
        } else if (curveType == PROTRACTOR) {

            for (int i = 0; i < curves[slice].size(); i++) {
                ((VOIProtractor) (curves[slice].elementAt(i))).setColor(color);
            }
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    /**
     * Imports the curve into the VOI, testing for which type.
     *
     * @param  x      array of x coordinates to import
     * @param  y      array of y coordinates to import
     * @param  z      array of z coordinates to import
     * @param  slice  index of slice of curve
     */
    public void importCurve(float[] x, float[] y, float[] z, int slice) {
        VOIBase curve;

        if (curveType == LINE) {
            curve = new VOILine(name);
        } else if (curveType == CONTOUR) {
            curve = new VOIContour(name, true);
        } else if (curveType == POLYLINE) {
            curve = new VOIContour(name, false);
        } else if (curveType == POINT) {
            curve = new VOIPoint(name);
        } else if (curveType == PROTRACTOR) {
            curve = new VOIProtractor();
        } else if (curveType == ANNOTATION) {
            curve = new VOIText();
        } else if (curveType == POLYLINE_SLICE) {
            curve = new VOIPoint(name, true);
        } else {
            return;
        }

        curve.importArrays(x, y, z, x.length);
        curves[slice].addElement(curve);

        if ((curveType == POINT) || (curveType == POLYLINE_SLICE)) {
            ((VOIPoint) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));

            if (curveType == POLYLINE_SLICE) {

                try {
                    activePolylineSlicePoint = Integer.parseInt(((VOIPoint) (curves[slice].lastElement())).getLabel()) -
                                               2;
                } catch (Exception e) {
                    activePolylineSlicePoint = 0;
                }

                polygonIndex = curves[slice].size() - 2;
            }
        } else if (curveType == PROTRACTOR) {

            for (int i = 0; i < curves[slice].size(); i++) {
                ((VOIProtractor) (curves[slice].elementAt(i))).setColor(color);
            }
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
        }
    }

    /**
     * Imports just the VOIs in a slice into this VOI.
     *
     * @param  slice     slice indicates the slice where the contour(s) is to be located
     * @param  voiSlice  voiSlice indicates the slice where the contour(s) is to be copied from
     * @param  voi       added to VOI
     * @param  newZDim   indicates the new Z dimenions to be set
     */
    public void importNewVOI(int slice, int voiSlice, VOI voi, int newZDim, boolean resize) {
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
    }


    /**
     * Imports the point into the VOI (must be a VOI.POINT).
     *
     * @param  point  point to import
     * @param  slice  index of slice of new point
     */
    public void importPoint(Vector3f point, int slice) {
        VOIPoint voiPt = null;

        if (curveType == POINT) {
            voiPt = new VOIPoint(name);
        } else {
            return;
        }

        voiPt.importPoint(point, slice);
        curves[slice].addElement(voiPt);
        ((VOIPoint) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
    }

    /**
     * Imports the point into the VOI (must be a VOI.POINT).
     *
     * @param  points  point to import
     * @param  slice   index of slice of new point
     */
    public void importPoints(Vector3f[] points, int slice) {
        VOIPoint[] voiPts = new VOIPoint[points.length];
        int i;

        if (curveType == POINT) {

            for (i = 0; i < points.length; i++) {
                voiPts[i] = new VOIPoint(name);
            }
        } else {
            return;
        }
System.err.println("VOI.importPoints() slice is: " + slice);
        for (i = 0; i < points.length; i++) {
            voiPts[i].importPoint(points[i], slice);
            curves[slice].addElement(voiPts[i]);
            ((VOIPoint) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
        }
        if (isEmpty()) {
        	System.err.println("IS EMPTY AFTER IMPORTING");
        }
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
            contour = new VOIContour(name, true);
        } else if (curveType == POLYLINE) {
            contour = new VOIContour(name, false);
        } else {
            return;
        }

        contour.importPolygon(gon, slice);
        curves[slice].addElement(contour);
        ((VOIContour) (curves[slice].lastElement())).setLabel(String.valueOf(elementLabel++));
    }

    /**
     * Only for POLYSLICE_LINE types. Inserts a new point into the structure and re-labels numbers
     *
     * @param  point  Vector3f the new point to create a VOIPoint from
     * @param  slice  int the slice into which this new VOIPoint will be inserted
     */
    public void insertPSlicePt(Vector3f point, int slice) {

        if (curveType != POLYLINE_SLICE) {
            return;
        }

        if ((polygonIndex != -99) && (point != null)) {
System.err.println("curves size: " );
            if (curves[slice].size() > polygonIndex) {
            	System.err.println("yo2:");
                VOIPoint vPt = new VOIPoint(name, true);
                vPt.addElement(point);

                // System.err.println("polygon index (for pline slice): " + polygonIndex);

                // label num is 1 + previous pts label (inserting between)
                int labelNum = Integer.parseInt(((VOIPoint) curves[slice].elementAt(polygonIndex)).getLabel()) + 1;

                vPt.setLabel(Integer.toString(labelNum));

                // relabel all VOIPoints past the polygonIndex

                int currentLabel = -1;

                for (int i = 0; i < curves.length; i++) {

                    for (int j = 0; j < curves[i].size(); j++) {

                        currentLabel = Integer.parseInt(((VOIPoint) curves[i].elementAt(j)).getLabel());

                        if (currentLabel >= labelNum) {
                            ((VOIPoint) curves[i].elementAt(j)).incrementLabel(1);
                        }
                    }
                }


                // finally insert the new VOIPoint into the structure
                curves[slice].insertElementAt(vPt, polygonIndex + 1);

                elementLabel++;
            }
        }

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
     * Determines if the given BitSet binary mask is true for all points that are within the VOI.
     *
     * @param   mask  BitSet
     * @param   xDim  x dimension, used to index into the image
     * @param   yDim  y dimension, used to index into the image
     *
     * @return  boolean does the mask the VOI area
     */
    public boolean isBinaryMaskContained(BitSet mask, int xDim, int yDim) {
        int z;

        for (z = 0; z < zDim; z++) {

            if (!isBinaryMaskContained(xDim, yDim, z, mask)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Test whether or not the VOI is empty.
     *
     * @return  boolean result of test
     */
    public boolean isEmpty() {
        int i;
//System.err.println("zDim is: " + zDim);
        for (i = 0; i < zDim; i++) {

            if (!curves[i].isEmpty()) {
                return false;
            }
        }

        return true;
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
     * Accessor that tells if the VOI is visible.
     *
     * @return  boolean visible
     */
    public boolean isVisible() {
        return visible;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  slice  DOCUMENT ME!
     */
    public void markPSlicePt(int slice) {

        if (curveType == POLYLINE_SLICE) {
            this.pLineSliceIndex = polygonIndex;

            try {
                activePolylineSlicePoint = Integer.parseInt(((VOIPoint) (curves[slice].elementAt(polygonIndex)))
                                                                .getLabel()) - 1;
            } catch (Exception e) {
                activePolylineSlicePoint = 0;
            }
        }
    }

    /**
     * Finds 2 points that form the maximum width of the VOI.
     *
     * @return  returns the two points.
     */
    public Vector3f[] maxWidth() {
        int z, i;
        int j, conSize;
        double[] maxDistance = new double[1];
        Vector3f[] points = new Vector3f[2];
        Vector3f point;

        for (z = 0; z < zDim; z++) {

            for (i = 0; i < curves[z].size(); i++) {

                if (curveType == CONTOUR) {
                    conSize = ((VOIContour) (curves[z].elementAt(i))).size();

                    for (j = 0; j < conSize; j++) {
                        point = findMaxWidth((Vector3f) ((VOIContour) (curves[z].elementAt(i))).elementAt(j),
                                             maxDistance);

                        if (point != null) {
                            points[0] = (Vector3f) ((VOIContour) (curves[z].elementAt(i))).elementAt(j);
                            points[1] = point;
                        }
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
        int i;

        if (fixed) {
            return;
        }

        if ((curveType != POINT) && (curveType != ANNOTATION) && (curveType != POLYLINE_SLICE)) {

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

                int j;

                for (i = 0; i < curves.length; i++) {

                    for (j = 0; j < curves[i].size(); j++) {

                        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                            ((VOIContour) (curves[i].elementAt(j))).translate(xM, yM);
                        } else if (curveType == PROTRACTOR) {
                            ((VOIProtractor) (curves[i].elementAt(j))).translate(xM, yM);
                        } else if (curveType == LINE) {
                            ((VOILine) (curves[i].elementAt(j))).translate(xM, yM);
                        }
                    }
                }

                return;
            } else {
                getSliceBounds(slice, xBounds, yBounds);

                for (i = 0; i < curves[slice].size(); i++) {
                    boolean isActive = false;

                    if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                        isActive = ((VOIContour) (curves[slice].elementAt(i))).isActive();
                    } else if (curveType == PROTRACTOR) {
                        isActive = ((VOIProtractor) (curves[slice].elementAt(i))).isActive();
                    } else if (curveType == LINE) {
                        isActive = ((VOILine) (curves[slice].elementAt(i))).isActive();
                    }

                    if (isActive) {

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

                        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                            ((VOIContour) (curves[slice].elementAt(i))).translate(xM, yM);
                        } else if (curveType == PROTRACTOR) {
                            ((VOIProtractor) (curves[slice].elementAt(i))).translate(xM, yM);
                        } else if (curveType == LINE) {
                            ((VOILine) (curves[slice].elementAt(i))).translate(xM, yM);
                        }
                    }
                }

                return;
            }
        } else if (curveType == POLYLINE_SLICE) {
            boolean doWhole = (zM == 0);

            if (!doWhole) {

                if ((pLineSliceIndex < curves[slice].size()) && (pLineSliceIndex >= 0)) {
                    ((VOIPoint) (curves[slice].elementAt(pLineSliceIndex))).moveActivePt(zM, xDim, yDim);
                }

            } else {
                getSliceBounds(slice, xBounds, yBounds);

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

                // use trick to do either active point moving or whole visible VOI moving
                for (i = 0; i < curves[slice].size(); i++) {

                    if (((VOIPoint) (curves[slice].elementAt(i))).isActive()) {
                        ((VOIPoint) (curves[slice].elementAt(i))).moveVOIPoint(xM, yM, zM, xDim, yDim, zDim);
                    }
                }
            }
        } else if (curveType == POINT) {

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

                int j;

                for (i = 0; i < curves.length; i++) {

                    for (j = 0; j < curves[i].size(); j++) {
                        ((VOIPoint) (curves[i].elementAt(j))).moveVOIPoint(xM, yM, zM, xDim, yDim, zDim);
                    }
                }
            } else {

                for (i = 0; i < curves[slice].size(); i++) {

                    if (((VOIPoint) (curves[slice].elementAt(i))).isActive()) {
                        ((VOIPoint) (curves[slice].elementAt(i))).moveVOIPoint(xM, yM, zM, xDim, yDim, zDim);
                    }
                }
            }
        } else if (curveType == ANNOTATION) {

            if (slice == -1) {
                getBounds(xBounds, yBounds, zBounds);

                /*
                 * if (xBounds[0] + xM >= xDim || xBounds[0] + xM < 0) return; if (xBounds[1] + xM >= xDim || xBounds[1]
                 * + xM < 0) return; if (yBounds[0] + yM >= yDim || yBounds[0] + yM < 0) return; if (yBounds[1] + yM >=
                 * yDim || yBounds[1] + yM < 0) return;
                 */
                int j;

                for (i = 0; i < curves.length; i++) {

                    for (j = 0; j < curves[i].size(); j++) {
                        ((VOIText) (curves[i].elementAt(j))).moveVOIPoint(xM, yM, zM, xDim, yDim, zDim);
                    }
                }
            } else {

                for (i = 0; i < curves[slice].size(); i++) {

                    if (((VOIText) (curves[slice].elementAt(i))).isActive()) {
                        ((VOIText) (curves[slice].elementAt(i))).moveVOIPoint(xM, yM, zM, xDim, yDim, zDim);
                    }
                }
            }
        } else {
            return;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x      int
     * @param   y      int
     * @param   slice  int
     *
     * @return  boolean
     */
    public boolean nearCardioLine(int x, int y, int slice) {

        if (curveType != CARDIOLOGY) {
            return false;
        }

        return ((VOICardiology) curves[slice].elementAt(0)).nearCardioLine(x, y);
    }

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
     */
    public int nearCardioPoint(int x, int y, int slice, float zoom, float resolutionX, float resolutionY) {

        if (curveType != CARDIOLOGY) {
            return VOIBase.NOT_A_POINT;
        }

        return ((VOICardiology) curves[slice].elementAt(0)).nearCardioPoint(x, y, zoom, resolutionX, resolutionY);
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

        int i;

        if (curveType == CONTOUR) {

            for (i = 0; i < curves[slice].size(); i++) {

                if (((VOIContour) (curves[slice].elementAt(i))).nearLine(x, y, 3)) {
                    polygonIndex = i;

                    return true;
                }
            }
        } else if (curveType == POLYLINE) {

            for (i = 0; i < curves[slice].size(); i++) {

                if (((VOIContour) (curves[slice].elementAt(i))).nearLine(x, y, 3)) {
                    polygonIndex = i;

                    return true;
                }
            }
        } else if (curveType == LINE) {

            for (i = 0; i < curves[slice].size(); i++) {

                if (((VOILine) (curves[slice].elementAt(i))).nearLine(x, y, 5)) {
                    polygonIndex = i;

                    return true;
                }
            }
        } else if (curveType == PROTRACTOR) {

            for (i = 0; i < curves[slice].size(); i++) {

                if (((VOIProtractor) (curves[slice].elementAt(i))).nearLine(x, y, 10)) {
                    polygonIndex = i;

                    return true;
                }
            }
        } else if (curveType == POLYLINE_SLICE) {
            Vector3f pt1;
            Vector3f pt2;
            int x1, y1, x2, y2;

            try {

                for (i = 0; i < (curves[slice].size() - 1); i++) {
                    pt1 = ((VOIPoint) curves[slice].elementAt(i)).exportPoint();
                    pt2 = ((VOIPoint) curves[slice].elementAt(i + 1)).exportPoint();

                    x1 = MipavMath.round(pt1.X);
                    y1 = MipavMath.round(pt1.Y);
                    x2 = MipavMath.round(pt2.X);
                    y2 = MipavMath.round(pt2.Y);

                    if (VOIBase.testDistance(x, x1, x2, y, y1, y2, 3)) {
                        polygonIndex = i;

                        return true;
                    }
                }
            } catch (Exception e) {
                return false;
            }
        }

        polygonIndex = -99;

        return false;
    }

    /**
     * Tests if a point is near an edge point of a line.
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
     */
    public boolean nearLinePoint(int x, int y, int slice, int element, float zoom, float resolutionX,
                                 float resolutionY) {

        if (isEmpty()) {
            return false;
        }

        if ((curveType == LINE) && (curves[slice].size() > 0)) {

            if (((VOILine) (curves[slice].elementAt(element))).isActive() &&
                    ((VOILine) (curves[slice].elementAt(element))).nearLinePoint(x, y, zoom, resolutionX,
                                                                                     resolutionY)) {
                polygonIndex = element;

                return true;
            }
        }

        polygonIndex = -99;
        resizeIndex = -99;

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
     */
    public boolean nearOuterPoint(int x, int y, int slice, int element, float zoom, float resolutionX,
                                  float resolutionY) {

        if (isEmpty()) {
            return false;
        }

        if ((curveType == PROTRACTOR) && (curves[slice].size() > 0)) {

            if (((VOIProtractor) (curves[slice].elementAt(element))).isActive() &&
                    ((VOIProtractor) (curves[slice].elementAt(element))).nearOuterPoint(x, y, zoom, resolutionX,
                                                                                            resolutionY)) {
                polygonIndex = element;

                return true;
            }
        }

        polygonIndex = -99;
        resizeIndex = -99;

        return false;
    }

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

        int i;

        // System.err.println("doing nearPoint");
        for (i = 0; i < curves[slice].size(); i++) {

            if (curveType == LINE) {

                if (((VOILine) (curves[slice].elementAt(i))).isActive() &&
                        ((VOILine) (curves[slice].elementAt(i))).nearPoint(x, y, zoom, resolutionX, resolutionY)) {
                    polygonIndex = i;

                    return true;
                }
            } else if (curveType == PROTRACTOR) {

                if (((VOIProtractor) (curves[slice].elementAt(i))).isActive() &&
                        ((VOIProtractor) (curves[slice].elementAt(i))).nearPoint(x, y, zoom, resolutionX,
                                                                                     resolutionY)) {
                    polygonIndex = i;

                    return true;
                }
            } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                ((VOIContour) (curves[slice].elementAt(i))).getBounds(xBounds, yBounds, zBounds);
                ((VOIContour) (curves[slice].elementAt(i))).setBounds(xBounds, yBounds, zBounds);

                if (((VOIContour) (curves[slice].elementAt(i))).isActive() && (boundingBox == true)) {
                    nearBoundPoint = ((VOIContour) (curves[slice].elementAt(i))).nearBoundPoint(x, y, zoom, resolutionX,
                                                                                                resolutionY);

                    if (nearBoundPoint != NOT_A_POINT) {
                        polygonIndex = -99;
                        resizeIndex = i;

                        return true;
                    }
                }

                if (((VOIContour) (curves[slice].elementAt(i))).isActive() &&
                        ((VOIContour) (curves[slice].elementAt(i))).nearPoint(x, y, zoom, resolutionX, resolutionY)) {
                    polygonIndex = i;
                    resizeIndex = -99;

                    return true;
                }
            } else if ((curveType == VOI.POINT) || (curveType == VOI.POLYLINE_SLICE)) {
                if (((VOIPoint) (curves[slice].elementAt(i))).nearPoint(x, y, zoom, resolutionX, resolutionY)) {
                    polygonIndex = i;

                    return true;
                }
            } else if (curveType == VOI.ANNOTATION) {

            	//check to see if this is near the VOIText's arrow tip
                if (((VOIText) (curves[slice].elementAt(i))).nearMarkerPoint(x, y, zoom, resolutionX, resolutionY)) {
                    polygonIndex = i;
                    return true;
                }
            }
        }

        polygonIndex = -99;
        resizeIndex = -99;

        return false;
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
     */
    public boolean nearPoint(int x, int y, int slice, int element, float zoom, float resolutionX, float resolutionY) {

        if (isEmpty()) {
            return false;
        }

        if (curveType == LINE) {

            if (((VOILine) (curves[slice].elementAt(element))).isActive() &&
                    ((VOILine) (curves[slice].elementAt(element))).nearPoint(x, y, zoom, resolutionX, resolutionY)) {
                polygonIndex = element;

                return true;
            }
        } else if (curveType == PROTRACTOR) {

            if (((VOIProtractor) (curves[slice].elementAt(element))).isActive() &&
                    ((VOIProtractor) (curves[slice].elementAt(element))).nearPoint(x, y, zoom, resolutionX,
                                                                                       resolutionY)) {
                polygonIndex = element;

                return true;
            }
        } else if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves[slice].elementAt(element))).getBounds(xBounds, yBounds, zBounds);
            ((VOIContour) (curves[slice].elementAt(element))).setBounds(xBounds, yBounds, zBounds);

            if (((VOIContour) (curves[slice].elementAt(element))).isActive() && (boundingBox == true)) {
                nearBoundPoint = ((VOIContour) (curves[slice].elementAt(element))).nearBoundPoint(x, y, zoom,
                                                                                                  resolutionX,
                                                                                                  resolutionY);

                if (nearBoundPoint != NOT_A_POINT) {
                    polygonIndex = -99;
                    resizeIndex = element;

                    return true;
                }
            }

            if (((VOIContour) (curves[slice].elementAt(element))).isActive() &&
                    ((VOIContour) (curves[slice].elementAt(element))).nearPoint(x, y, zoom, resolutionX, resolutionY)) {
                polygonIndex = element;
                resizeIndex = -99;

                return true;
            }
        } else if (curveType == VOI.POINT) {

            if (((VOIPoint) (curves[slice].elementAt(element))).nearPoint(x, y, zoom, resolutionX, resolutionY)) {
                polygonIndex = element;

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
        int i, j;
        int slice;
        int conSize;
        float x1, x2, y1, y2;
        float dx, dy;
        float inprod;
        Vector3f point;
        float minDistance = Float.MAX_VALUE;
        float lineLengthSquared;
        float lineLength;
        float distance1, distance2;
        float distance;
        boolean insideContour = true;
        int iLast = -1;
        int sliceLast = -1;

        for (slice = 0; slice < zDim; slice++) {

            for (i = 0; i < curves[slice].size(); i++) {

                if (curveType == CONTOUR) {
                    conSize = ((VOIContour) (curves[slice].elementAt(i))).size();

                    for (j = 0; j < conSize; j++) {

                        // Find one end of the line segment
                        point = (Vector3f) ((VOIContour) (curves[slice].elementAt(i))).elementAt(j);
                        x1 = point.X;
                        y1 = point.Y;

                        // Find the other end of the line segment
                        if (j == (conSize - 1)) {
                            point = (Vector3f) ((VOIContour) (curves[slice].elementAt(i))).elementAt(0);
                        } else {
                            point = (Vector3f) ((VOIContour) (curves[slice].elementAt(i))).elementAt(j + 1);
                        }

                        x2 = point.X;
                        y2 = point.Y;

                        if ((x == x1) && (y == y1) && (z == slice)) {
                            return 0.0f;
                        } else if ((x == x2) && (y == y2) && (z == slice)) {
                            return 0.0f;
                        } else if ((x1 == x2) && (y1 == y2)) {
                            // 2 contour points coincide - do nothing
                        } else {
                            dx = x2 - x1;
                            dy = y2 - y1;
                            lineLengthSquared = (dx * dx) + (dy * dy);
                            lineLength = (float) Math.sqrt(lineLengthSquared);
                            inprod = (dx * (x - x1)) + (dy * (y - y1));

                            if ((inprod > 0) && (inprod < lineLengthSquared)) {

                                // perpindicular projection from point to line falls
                                // on the section of the line between (x1,y1) and (x2,y2)
                                distance = Math.abs(((y1 - y2) * x) + ((x2 - x1) * y) + ((x1 * y2) - (y1 * x2))) /
                                               lineLength;
                            } else {
                                distance1 = (float) Math.sqrt(((x - x1) * (x - x1)) + ((y - y1) * (y - y1)));
                                distance2 = (float) Math.sqrt(((x - x2) * (x - x2)) + ((y - y2) * (y - y2)));
                                distance = Math.min(distance1, distance2);
                            }

                            distance = (float) Math.sqrt((distance * distance) + ((slice - z) * (slice - z)));

                            if (distance < minDistance) {
                                minDistance = distance;

                                if (z != slice) {
                                    insideContour = false;
                                } else if ((i != iLast) || (slice != sliceLast)) {
                                    insideContour = ((VOIContour) (curves[slice].elementAt(i))).contains(x, y, true);
                                }

                                iLast = i;
                                sliceLast = slice;
                            }
                        }
                    }
                }
            }
        }

        if (insideContour) {
            minDistance = -minDistance;
        }

        return minDistance;
    }

    /**
     * Does nothing yet.
     */
    public void readCurves() {

        // Make fileContour object and read
    }

    /**
     * Does nothing yet.
     */
    public void readMask() {

        // Make fileMask object and read
    }

    /**
     * Clears VOI of a curve or point (VOIPoint)at an index in a slice.
     *
     * @param  index  slice of curve
     * @param  slice  index of slice of curve
     */
    public void removeCurve(int index, int slice) {
        short pointID, tmpID, contourID;
        int i, s, nCurves;

        if (curveType == POINT) {

            if (((VOIPoint) curves[slice].elementAt(index)).getLabel() != null) {
                pointID = Short.valueOf(((VOIPoint) curves[slice].elementAt(index)).getLabel()).shortValue();
                curves[slice].removeElementAt(index);

                for (s = 0; s < zDim; s++) {
                    nCurves = curves[s].size();

                    for (i = 0; i < nCurves; i++) {
                        tmpID = Short.valueOf(((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue();

                        if (tmpID > pointID) {
                            ((VOIPoint) (curves[s].elementAt(i))).setLabel(String.valueOf(tmpID - 1));
                        }
                    }
                }

                elementLabel--;

                if ((contourGraph != null) && ((int) elementLabel == 1)) {
                    contourGraph.deleteFunct(((int) elementLabel) - 1);
                }
            } else {
                curves[slice].removeElementAt(index);
            }
        } else if (curveType == ANNOTATION) {
            curves[slice].removeAllElements();
        } else {

            if (curveType == CONTOUR) {

                if (((VOIContour) curves[slice].elementAt(index)).getLabel() != null) {
                    contourID = Short.valueOf(((VOIContour) curves[slice].elementAt(index)).getLabel()).shortValue();
                    curves[slice].removeElementAt(index);

                    for (s = 0; s < zDim; s++) {
                        nCurves = curves[s].size();

                        for (i = 0; i < nCurves; i++) {

                            try {
                                tmpID = Short.valueOf(((VOIContour) curves[s].elementAt(i)).getLabel()).shortValue();

                                if (tmpID > contourID) {
                                    ((VOIContour) (curves[s].elementAt(i))).setLabel(String.valueOf(tmpID - 1));
                                }
                            } catch (NumberFormatException nfe) {
                                // in case a VOI doesn't have a label, we don't want this thing choking
                            }
                        }
                    }

                    elementLabel--;

                    return;
                }
            }

            curves[slice].removeElementAt(index);
        }
    }

    /**
     * Clears VOI of all curves at a slice.
     *
     * @param  slice  index of slice of curves to remove
     */
    public void removeCurves(int slice) {
        curves[slice].removeAllElements();
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
     */
    public void rubberbandVOI(int x, int y, int slice, int xDim, int yDim, boolean doRound) {
        float oldXMin, oldXMax, oldYMin, oldYMax;
        float newXMin, newXMax, newYMin, newYMax;
        float xCenter, yCenter;
        float translateX, translateY, scaleX, scaleY;
        boolean change = true;

        if (fixed) {
            return;
        }

        if ((polygonIndex >= 0) && (polygonIndex < curves[slice].size())) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

                // System.err.println("x: " + x + "y: " + y);
                ((VOIContour) (curves[slice].elementAt(polygonIndex))).movePt(x, y);
                ((VOIContour) (curves[slice].elementAt(polygonIndex))).reloadPoints();
            } else if (curveType == LINE) {
                ((VOILine) (curves[slice].elementAt(polygonIndex))).movePt(x, y, slice, xDim, yDim);
            } else if (curveType == PROTRACTOR) {
                ((VOIProtractor) (curves[slice].elementAt(polygonIndex))).movePt(x, y);
            } else if (curveType == CARDIOLOGY) {
                ((VOICardiology) (curves[slice].elementAt(0))).movePt(x, y);
            }
        } else if ((resizeIndex >= 0) && (resizeIndex < curves[slice].size())) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                ((VOIContour) (curves[slice].elementAt(resizeIndex))).getBounds(xBounds, yBounds, zBounds);
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
                    ((VOIContour) (curves[slice].elementAt(resizeIndex))).contains(-1, -1, true);
                } // end of if (change)
            }
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
     */
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

        if ((polygonIndex >= 0) && (polygonIndex < curves[slice].size())) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                ((VOIContour) (curves[slice].elementAt(polygonIndex))).movePt(x, y);
                ((VOIContour) (curves[slice].elementAt(polygonIndex))).reloadPoints();
            } else if (curveType == LINE) {
                ((VOILine) (curves[slice].elementAt(polygonIndex))).movePt(x, y, slice, xDim, yDim);
            } else if (curveType == PROTRACTOR) {
                ((VOIProtractor) (curves[slice].elementAt(polygonIndex))).movePt(x, y);
            } else if (curveType == CARDIOLOGY) {
                ((VOICardiology) (curves[slice].elementAt(0))).movePt(x, y);
            } else if (curveType == POLYLINE_SLICE) {
                ((VOIPoint) (curves[slice].elementAt(polygonIndex))).movePt(x, y);
                this.markPSlicePt(slice);
            } else if (curveType == ANNOTATION) {
                ((VOIText) (curves[slice].elementAt(polygonIndex))).moveMarkerPoint(x, y, slice, xDim, yDim, zDim);
            }
        } else if ((resizeIndex >= 0) && (resizeIndex < curves[slice].size())) {

            if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
                ((VOIContour) (curves[slice].elementAt(resizeIndex))).getBounds(xBounds, yBounds, zBounds);
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
                    ((VOIContour) (curves[slice].elementAt(resizeIndex))).contains(-1, -1, true);
                } // end of if (change)
            }
        }
    }

    /**
     * Does nothing yet.
     */
    public void saveCurves() {

        // Make fileContour object and save
    }

    /**
     * Does nothing yet.
     */
    public void saveMask() {

        // Make fileMask object and save
    }

    /**
     * Finds the second oder attributes and prints to the command line. Only for contour.
     *
     * @param  xDim    DOCUMENT ME!
     * @param  yDim    DOCUMENT ME!
     * @param  xRes    DOCUMENT ME!
     * @param  yRes    DOCUMENT ME!
     * @param  xUnits  DOCUMENT ME!
     * @param  yUnits  DOCUMENT ME!
     */
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

        int ncurves = 0;

        for (z = 0; z < zDim; z++) {

            if (curves[z].size() != 0) {
                ncurves++;

                for (i = 0; i < curves[z].size(); i++) {
                    ((VOIContour) (curves[z].elementAt(i))).secondOrderAttributes(xDim, yDim, xRes, yRes, xUnits,
                                                                                  yUnits, pAxis, eccentricity,
                                                                                  majorAxis, minorAxis);
                    Preferences.debug(" Theta = " + pAxis[0] + " ecc = " + eccentricity[0] + " Major axis = " +
                                      majorAxis[0] + " Minor Axis = " + minorAxis[0] + "\n");
                }
            }
        }
    }

    /**
     * Sets whether or not the VOI is active.
     *
     * @param  act  boolean to set active to
     */
    public void setActive(boolean act) {
        this.active = act;
        fireVOIselection();
    }

    /**
     * Sets all contours in the VOI as active or inactive.
     *
     * @param  flag  boolean to set VOI active or inactive
     */
    public void setAllActive(boolean flag) {
        int i, j;
        this.active = flag;

        for (j = 0; j < zDim; j++) {

            for (i = 0; i < curves[j].size(); i++) {
                ((VOIBase) (curves[j].elementAt(i))).setActive(flag);
            }
        }
        // fireVOIselection();
    }

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
        
        if (curveType == PROTRACTOR) {

            for (int j = 0; j < zDim; j++) {

                for (int i = 0; i < curves[j].size(); i++) {
                    ((VOIProtractor) (curves[j].elementAt(i))).setColor(color);
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

    /**
     * Accessor that sets the curveType to the parameter.
     *
     * @param  curveType  the curve type
     */
    public void setCurveType(int curveType) {
        this.curveType = curveType;
    }

    public void setCurves(Vector [] newCurves) {
    	this.curves = newCurves;
    	this.zDim = newCurves.length;
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
     * Accessor that sets the intensity array to the parameter.
     *
     * @param  inten  DOCUMENT ME!
     */
    public void setIntensity(float[] inten) {
        this.intensity = inten;
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

        // now must set the name in all VOIBases within
        for (int i = 0; i < curves.length; i++) {

            for (int j = 0; j < curves[i].size(); j++) {
                ((VOIBase) curves[i].elementAt(j)).setName(name);
            }
        }
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
     * Accessor that sets the position array to the parameter.
     *
     * @param  pos  DOCUMENT ME!
     */
    public void setPosition(float[] pos) {
        this.position = pos;
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
     * Accessor that sets the rgb intensity array to the parameter.
     *
     * @param  inten  DOCUMENT ME!
     */
    public void setRGBIntensities(float[][] inten) {
        this.rgbIntensities = inten;
    }

    /**
     * Accessor that sets the rgb position array to the parameter.
     *
     * @param  pos  DOCUMENT ME!
     */
    public void setRGBPositions(float[][] pos) {
        this.rgbPositions = pos;
    }

    /**
     * Accessor that sets the statisitic list.
     *
     * @param  stats  list of statistics
     */
    public void setStatisticList(ViewList[] stats) {
        this.stats = stats;
    }

    /**
     * Accessor that sets whether to calculate total sum of the intensity (true) else calculate the average pixel
     * intensity (used when plotting an intensity graph of a voi).
     *
     * @param  total  DOCUMENT ME!
     */
    public void setTotalIntensity(boolean total) {
        this.totalIntensity = total;
    }

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
     */
    public void setVisibleFlag(boolean flag) {
        this.visible = flag;
    }

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

        if (curveType == PROTRACTOR) {

            for (int j = 0; j < zDim; j++) {

                for (int i = 0; i < curves[j].size(); i++) {
                    ((VOIProtractor) (curves[j].elementAt(i))).setXYDim(xDim, yDim);
                }
            }
        } else if (curveType == LINE) {

            for (int j = 0; j < zDim; j++) {

                for (int i = 0; i < curves[j].size(); i++) {
                    ((VOILine) (curves[j].elementAt(i))).setXYDim(xDim, yDim);
                }
            }
        }
    }

    /**
     * Finds the minimum distance from a point to a contour in a slice. If the point is in a contour, the distance is
     * given a negative sign. If the point is outside contours, the distance is given a positive sign. The contour
     * points are connected by straight line segments between the points so simply find the distances from the point to
     * each line segment comprising the contours.
     *
     * @param   slice  DOCUMENT ME!
     * @param   x      DOCUMENT ME!
     * @param   y      DOCUMENT ME!
     *
     * @return  minDistance
     */
    public float slicePointToContour(int slice, int x, int y) {
        int i, j;
        int conSize;
        float x1, x2, y1, y2;
        float dx, dy;
        float inprod;
        Vector3f point;
        float minDistance = Float.MAX_VALUE;
        float lineLengthSquared;
        float lineLength;
        float distance1, distance2;
        float distance;
        boolean insideContour = true;
        int iLast = -1;

        for (i = 0; i < curves[slice].size(); i++) {

            if (curveType == CONTOUR) {
                conSize = ((VOIContour) (curves[slice].elementAt(i))).size();

                for (j = 0; j < conSize; j++) {

                    // Find one end of the line segment
                    point = (Vector3f) ((VOIContour) (curves[slice].elementAt(i))).elementAt(j);
                    x1 = point.X;
                    y1 = point.Y;

                    // Find the other end of the line segment
                    if (j == (conSize - 1)) {
                        point = (Vector3f) ((VOIContour) (curves[slice].elementAt(i))).elementAt(0);
                    } else {
                        point = (Vector3f) ((VOIContour) (curves[slice].elementAt(i))).elementAt(j + 1);
                    }

                    x2 = point.X;
                    y2 = point.Y;

                    if ((x == x1) && (y == y1)) {
                        return 0.0f;
                    } else if ((x == x2) && (y == y2)) {
                        return 0.0f;
                    } else if ((x1 == x2) && (y1 == y2)) {
                        // 2 contour points coincide - do nothing
                    } else {
                        dx = x2 - x1;
                        dy = y2 - y1;
                        lineLengthSquared = (dx * dx) + (dy * dy);
                        lineLength = (float) Math.sqrt(lineLengthSquared);
                        inprod = (dx * (x - x1)) + (dy * (y - y1));

                        if ((inprod > 0) && (inprod < lineLengthSquared)) {

                            // perpindicular projection from point to line falls
                            // on the section of the line between (x1,y1) and (x2,y2)
                            distance = Math.abs(((y1 - y2) * x) + ((x2 - x1) * y) + ((x1 * y2) - (y1 * x2))) /
                                           lineLength;
                        } else {
                            distance1 = (float) Math.sqrt(((x - x1) * (x - x1)) + ((y - y1) * (y - y1)));
                            distance2 = (float) Math.sqrt(((x - x2) * (x - x2)) + ((y - y2) * (y - y2)));
                            distance = Math.min(distance1, distance2);
                        }

                        if (distance < minDistance) {
                            minDistance = distance;

                            if (i != iLast) {
                                insideContour = ((VOIContour) (curves[slice].elementAt(i))).contains(x, y, true);
                            }

                            iLast = i;
                        }
                    }
                }
            }
        }

        if (insideContour) {
            minDistance = -minDistance;
        }

        return minDistance;
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
     */
    public void transformVOI(TransMatrix tMatrix) {
        int z;

        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {

            for (z = 0; z < zDim; z++) {

                for (int i = 0; i < curves[z].size(); i++) {
                    ((VOIContour) (curves[z].elementAt(i))).transformContour(tMatrix, false);
                }
            }
        }
    }

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
     */
    public void transformVOI(float rotX, float rotY, float rotZ, float tX, float tY, float tZ, float sX, float sY,
                             float sZ) {
        int z;
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

            for (z = 0; z < zDim; z++) {

                for (int i = 0; i < curves[z].size(); i++) {
                    ((VOIContour) (curves[z].elementAt(i))).transformContour(tMatrix, false);
                }
            }
        }
    }

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

        gcPt = ((VOIContour) (curves[slice].elementAt(element))).getGeometricCenter();
        tMatrix.setTranslate((gcPt.X + tX), (gcPt.Y + tY), (gcPt.Z + tZ));
        tMatrix.setRotate(rotX, rotY, rotZ, TransMatrix.DEGREES);
        tMatrix.setZoom(sX, sY, sZ);
        tMatrix.setTranslate(-gcPt.X, -gcPt.Y, -gcPt.Z);

        if ((curveType == CONTOUR) || (curveType == POLYLINE)) {
            ((VOIContour) (curves[slice].elementAt(element))).transformContour(tMatrix, doRound);
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
     */
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

    /**
     * Fires a VOI event based on the VOI. calls the listener's <code>removedVOI()</code> method.
     *
     * @param  curve  DOCUMENT ME!
     */
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

    /**
     * Fires a VOI event based on the VOI. calls the listener's <code>removedVOI()</code> method.
     */
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

    /**
     * Creates a binary mask at a slice.
     *
     * @param  xDim        x dimension, used to index into the image
     * @param  yDim        y dimension, used to index into the image
     * @param  slice       slice to create mask at
     * @param  mask        the binary mask
     * @param  XOR         indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
     * @param  onlyActive  Only mask regions that are active (i.e. selected )
     */
    public void createBinaryMask(int xDim, int yDim, int slice, BitSet mask, boolean XOR, boolean onlyActive) {

        // System.err.println("doing create binary mask");
        int x = 0, y;
        int i, nGons;
        int offset;
        int offsetZ;
        nGons = curves[slice].size();
        offsetZ = slice * xDim * yDim;

        if ((process == true) && (curveType == CONTOUR)) {
//System.err.println("XOR is " + XOR);
//System.err.println("polarity additive?: " + (polarity == ADDITIVE));
            for (i = 0; i < nGons; i++) {

                if (!onlyActive || ((VOIContour) (curves[slice].elementAt(i))).isActive()) {
                    ((VOIContour) (curves[slice].elementAt(i))).contains(0, 0, true);
                    ((VOIContour) (curves[slice].elementAt(i))).getBounds(xBounds, yBounds, zBounds);

                    // Keep the next four lines!!
                    int xbs = (int) xBounds[0];
                    int xbe = (int) xBounds[1];
                    int ybs = (int) yBounds[0];
                    int ybe = (int) yBounds[1];

                    // System.err.println("Xbounds 0 = " + xBounds[0] + " Xbounds 1 = " + xBounds[1]);
                    for (y = ybs; y < ybe; y++) {
                        offset = offsetZ + (y * xDim);

                        for (x = xbs; x < xbe; x++) {

                            if (((VOIContour) (curves[slice].elementAt(i))).contains(x, y, false) &&
                                    (polarity == ADDITIVE)) {

                                if (XOR && mask.get(offset + x)) {
                                    mask.clear(offset + x);
                                } else {
                                    mask.set(offset + x);
                                }
                            } else if (((VOIContour) (curves[slice].elementAt(i))).contains(x, y, false) &&
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
            int size = curves[slice].size();

            for (i = 0; i < size; i++) {
                pt = ((VOIPoint) (curves[slice].elementAt(i))).exportPoint();

                if (!onlyActive || ((VOIPoint) (curves[slice].elementAt(i))).isActive()) {
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
     * This is used by the method maxWidth.
     *
     * @param   pt           calculate the distance from this point to any point on any of the contours which form the
     *                       VOI.
     * @param   maxDistance  single array value to keep track of the largest
     *
     * @return  the maximum width of the VOI
     */
    private Vector3f findMaxWidth(Vector3f pt, double[] maxDistance) {
        int z, i;
        int j, conSize;
        double distance;
        Vector3f point = null;

        for (z = 0; z < zDim; z++) {

            for (i = 0; i < curves[z].size(); i++) {

                if (curveType == CONTOUR) {
                    conSize = ((VOIContour) (curves[z].elementAt(i))).size();

                    for (j = 0; j < conSize; j++) {
                        distance = MipavMath.distance(pt,
                                                      (Vector3f) ((VOIContour) (curves[z].elementAt(i))).elementAt(j));

                        if (distance > maxDistance[0]) {
                            maxDistance[0] = distance;
                            point = (Vector3f) ((VOIContour) (curves[z].elementAt(i))).elementAt(j);
                        }
                    }
                }
            }
        }

        return point;
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
     */
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

    
    /**
     * Gets a sorted Vector of PolyPoints (for use in the VOI copy/pasting)
     * @return Vector, sorted
     */
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
    
    
    public String getExtension() {
		return extension;
	}

	public void setExtension(String extension) {
		this.extension = extension;
	}
    
    
    
    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    private class PointComparator implements Comparator {

        /**
         * DOCUMENT ME!
         *
         * @param   o1  DOCUMENT ME!
         * @param   o2  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int compare(Object o1, Object o2) {
            int a = ((PolyPointHolder) o1).getNumber();
            int b = ((PolyPointHolder) o2).getNumber();

            return ((new Integer(a)).compareTo(new Integer(b)));
        }

    }

    /**
     * DOCUMENT ME!
     */
    public class PolyPointHolder {

        /** DOCUMENT ME! */
        private int number;

        /** DOCUMENT ME! */
        private Vector3f pt;

        /**
         * Creates a new PolyPointHolder object.
         *
         * @param  p  DOCUMENT ME!
         * @param  n  DOCUMENT ME!
         */
        public PolyPointHolder(Vector3f p, int n) {
            this.pt = p;
            this.number = n;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getNumber() {
            return this.number;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Vector3f getPoint() {
            return this.pt;
        }
    }

	

}
