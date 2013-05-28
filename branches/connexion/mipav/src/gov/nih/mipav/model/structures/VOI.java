package gov.nih.mipav.model.structures;

import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.util.MipavMath;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameGraph;

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
 * A VOI can be different types: annotation, point, line, protractor, polyline and contour.
 * 
 *  Event-handling routines:
 *  to add this object to send out events for listening objects, at least the following 3 methods must be present: addListener, removeListener, fireEvent as present below.
 * @version  0.1 Oct 27, 1997
 */
public class VOI extends ModelSerialCloneable {

	//~ Static fields/initializers -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = 4045424525937515770L;


	/** Static Variables for VOI and VOI Contour selection. */
	public static final int FORWARD = 0;

	/** Static Variables for VOI and VOI Contour selection. */
	public static final int BACKWARD = 1;

	/** Static Variables for VOI and VOI Contour selection. */
	public static final int FRONT = 2;

	/** Static Variables for VOI and VOI Contour selection. */
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

	//~ Instance fields ------------------------------------------------------------------------------------------------

	public static double calcLargestDistance(BitSet mask, int[] extents, float xRes, float yRes, float zRes,
			float[] xPts, float[] yPts, float zPts[], Vector3f kPos1, Vector3f kPos2 )
	{
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
			a = getLargest(mask, extents, orig, term, xRes, yRes, zRes, xPts, yPts, zPts, kPos1, kPos2 );
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
	private static double[] determineMaxAvg(float xRes, float yRes, float zRes,
			float[] xPts, float[] yPts, float zPts[]) {
		double max = 0, avg = 0;
		int n = xPts.length;
		Random r = new Random(1);
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
	private static void gatherBoundedPoints(ArrayList<Integer> orig, ArrayList<Integer> term, 
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
	 * Finds the largest line that lies within entirely within the VOI.
	 */  
	private static double getLargest(BitSet mask, int[] extents, ArrayList<Integer> orig, ArrayList<Integer> term, 
			float xRes, float yRes, float zRes, 
			float[] xPoints, float[] yPoints, float zPoints[], Vector3f kPos1, Vector3f kPos2) {
		int origPoint, termPoint;
		double largestDistanceSq = 0, distanceSq;
		double startX, startY, startZ;
		double endX, endY, endZ;
		double delX, delY, delZ;
		double distX, distY, distZ;
		double x, y, z;
		double slope, slope2;
		int xRound, yRound, zRound;
		int j;
		int maskIndex = 0;
		int xDim = extents.length > 0 ? extents[0] : 1;
		int yDim = extents.length > 1 ? extents[1] : 1;
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

				kPos1.X = (float)startX;
				kPos1.Y = (float)startY;
				kPos1.Z = (float)startZ;
				kPos2.X = (float)endX;
				kPos2.Y = (float)endY;
				kPos2.Z = (float)endZ;
				
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
								if ( xRound < endX && yRound < endY && zRound < endZ )
								{
									maskIndex = zRound * xDim * yDim + yRound * xDim + xRound;
									okay = mask.get( maskIndex );
									if (!okay) {
										continue forj;
									}
								}
							} // for (x = startX + 0.5, y = startY + 0.5 * slope, z = startZ + 0.5 * slope2; x < endX; x += 0.5, y += 0.5 * slope,
						} // if (endX >= startX)
							else { // endX < startX
								for (x = startX - 0.5, y = startY - 0.5 * slope, z = startZ - 0.5 * slope2; x > endX; x -= 0.5, y -= 0.5 * slope,
								z -= 0.5 * slope2) {
									xRound = (int)Math.round(x);
									yRound = (int)Math.round(y);
									zRound = (int)Math.round(z);
									if ( xRound < endX && yRound < endY && zRound < endZ )
									{
										maskIndex = zRound * xDim * yDim + yRound * xDim + xRound;
										okay = mask.get( maskIndex );
										if (!okay) {
											continue forj;
										}
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
								if ( xRound < endX && yRound < endY && zRound < endZ )
								{
									maskIndex = zRound * xDim * yDim + yRound * xDim + xRound;
									okay = mask.get( maskIndex );
									if (!okay) {
										continue forj;
									}
								}
							} // for (y = startY + 0.5, x = startX + 0.5 * slope, z = startX + 0.5 * slope2; y < endY; y += 0.5, x += 0.5 * slope,
						} // if (endX >= startX)
							else { // endX < startX
								for (y = startY - 0.5, x = startX - 0.5 * slope, z = startZ - 0.5 * slope2; y > endY; y -= 0.5, x -= 0.5 * slope,
								z -= 0.5 * slope2) {
									xRound = (int)Math.round(x);
									yRound = (int)Math.round(y);
									zRound = (int)Math.round(z);
									if ( xRound < endX && yRound < endY && zRound < endZ )
									{
										maskIndex = zRound * xDim * yDim + yRound * xDim + xRound;
										okay = mask.get( maskIndex );
										if (!okay) {
											continue forj;
										}
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
								if ( xRound < endX && yRound < endY && zRound < endZ )
								{
									maskIndex = zRound * xDim * yDim + yRound * xDim + xRound;
									okay = mask.get( maskIndex );
									if (!okay) {
										continue forj;
									}
								}
							} // for (z = startZ + 0.5, x = startX + 0.5 * slope, y = startY + 0.5 * slope2; z < endZ; z += 0.5, x += 0.5 * slope,   
						} // if (endZ >= startZ)
							else { // endZ < startZ
								for (z = startZ - 0.5, x = startX - 0.5 * slope, y = startY - 0.5 * slope2; z > endZ; z -= 0.5, x -= 0.5 * slope,
								y -= 0.5 * slope2) {
									xRound = (int)Math.round(x);
									yRound = (int)Math.round(y);
									zRound = (int)Math.round(z);
									if ( xRound < endX && yRound < endY && zRound < endZ )
									{
										maskIndex = zRound * xDim * yDim + yRound * xDim + xRound;
										okay = mask.get( maskIndex );
										if (!okay) {
											continue forj;
										}
									}
								} // for (z = startZ - 0.5, x = startX - 0.5 * slope, y = startY - 0.5 * slope2; z > endZ; z -= 0.5, x -= 0.5 * slope,
							} // else (endZ < startZ)
					} // else ((Math.abs(delZ) >= Math.abs(delX)) && (Math.abs(delZ) >= Math.abs(delY))) 
					largestDistanceSq = distanceSq;
					Preferences.debug("New points for largest distance: "+kPos1+"\t"+kPos2+"\n", 
					        Preferences.DEBUG_ALGORITHM);
				}
			}
		return Math.sqrt(largestDistanceSq);
	}

	/** int indicating that no point was found. */
	public final int NOT_A_POINT = -99;

	/** If true the bounding box of the VOI should be displayed. */
	private boolean boundingBox = false;

	/** Indicates the color or the VOI. */
	private Color color;

	/** The thickness of the VOI lines*/
	private int thickness = 1;

	/** ViewJFrameGraph for graphing contours in this VOI */
	private transient ViewJFrameGraph contourGraph = null;

	/** A vector array of curves per slice. */
	private VOIBaseVector curves;

	/** Indicates the type of VOI (i.e. CONTOUR, POLYLINE, LINE, POINT, PROTRACTOR, etc. ... ) */
	private int curveType;

	/** Indicates if the VOI should be shown as a boundary or a solid. */
	private int displayMode;


	/** If true the VOI cannot be moved, if false this VOI can be moved. */
	private boolean fixed = false;

	/** ID of the VOI, also used when choosing the display color. */
	private short ID;

	/** Sets the maximum intensity value for operations on  B & W images segmented by this VOI */
	private float ignoreMax = -Float.MAX_VALUE;

	/** Sets the minimum intensity value for operations on B & W images segmented by this VOI */
	private float ignoreMin = Float.MAX_VALUE;
	
	/** Sets the maximum red intensity value for operations on RGB images segmented by this VOI */
    private float ignoreMaxR = -Float.MAX_VALUE;

    /** Sets the minimum red intensity value for operations on RGB images segmented by this VOI */
    private float ignoreMinR = Float.MAX_VALUE;
    
    /** Sets the maximum green intensity value for operations on RGB images segmented by this VOI */
    private float ignoreMaxG = -Float.MAX_VALUE;

    /** Sets the minimum green intensity value for operations on RGB images segmented by this VOI */
    private float ignoreMinG = Float.MAX_VALUE;
    
    /** Sets the maximum blue intensity value for operations on RGB images segmented by this VOI */
    private float ignoreMaxB = -Float.MAX_VALUE;

    /** Sets the minimum blue intensity value for operations on RGB images segmented by this VOI */
    private float ignoreMinB = Float.MAX_VALUE;

	/** Name of the VOI stored as a string. */
	private String name;

	/** When in the solid display mode indicates how opaque the VOI should be. Zero is transparent and one is opaque. */
	private float opacity;

	/** Indicates if the VOI should mask ones or mask as zeros. */
	private int polarity = ADDITIVE;

	/** If true this flag indicates that the VOI should be included (applied) when processing the image. */
	private boolean process = true;

	/** Unique ID for saving & retrieving. */
	private int UID;

	/** If true the VOI is visible. */
	private boolean visible;

	/** Used to objects a label or ID inconjuction with the watershed algorithm. */
	private short watershedID;

	/** The x - dimension bounds: xBounds [0] = min and xBounds[1] = max. */
	private float[] xBounds = new float[2];

	/** The y - dimension bounds: yBounds [0] = min and yBounds[1] = max. */
	private float[] yBounds = new float[2];

	/** The z - dimension bounds: zBounds [0] = min and zBounds[1] = max. */
	private float[] zBounds = new float[2];

	//~ Constructors ---------------------------------------------------------------------------------------------------


	/** extension of voi file name of voi was read in through file **/
	private String extension = "";


	private boolean active = false;

	/** VOIListeners that are updated when this VOI changes */
	public transient EventListenerList listenerList;

	//~ Methods --------------------------------------------------------------------------------------------------------


	private ArrayList<String> comments = new ArrayList<String>();

	/**
	 * Create a VOI with the given id and name.
	 * @param  id         identifier of VOI
	 * @param  name       name of the VOI
	 */
	public VOI(short id, String name) {
		float hue;
		this.name = name;
		this.ID = id;
		this.watershedID = id;
		this.process = true;
		curves = new VOIBaseVector(this);


		displayMode = BOUNDARY;
		polarity = ADDITIVE;
		visible = true;
		opacity = 0.3f;
		hue = (float) ((((ID) * 35) % 360) / 360.0);
		setColor(Color.getHSBColor(hue, (float) 1.0, (float) 1.0)); // important to use the access method

		this.thickness = Preferences.getVOIThickness();
	}

    /**
	 * Constructs a Volume of Interest (VOI).
	 *
	 * @param  id         identifier of VOI
	 * @param  name       name of the VOI
	 * @param  curveType  type of curve, either a line or a contour
	 * @param  presetHue  If presetHue >= 0.0, use this value as the hue
	 */
	public VOI(short id, String name, int curveType, float presetHue) {
		float hue;

		if (curveType != POINT) {
			this.UID = this.hashCode();
			// System.out.println("Non Point VOI with UID: " + this.UID);
		}

		this.name = name;
		this.ID = id;
		this.watershedID = id;
		this.process = true;
		this.curveType = curveType;
		curves = new VOIBaseVector(this);

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
	}

	/**
	 * Copies the VOI into a new VOI object.
	 * @param kVOI VOI to copy.
	 */
	public VOI( VOI kVOI )
	{
		this.boundingBox = kVOI.boundingBox;
		this.color = new Color( kVOI.color.getRed(), kVOI.color.getGreen(), kVOI.color.getBlue() );
		this.thickness = kVOI.thickness;
		this.contourGraph = kVOI.contourGraph;
		this.curves = new VOIBaseVector(this);
		if (kVOI.curveType == CONTOUR) {
    		for ( int j = 0; j < kVOI.curves.size(); j++ )
    		{
    			//VOIBase kContour = kVOI.curves.get(j).clone();
    		    VOIBase kContour = new VOIContour((VOIContour)kVOI.curves.get(j));
    			// With clone this.curves.add( kContour) was seen to occur before the clone operation was complete
    			this.curves.add( kContour );
    		}
		}

		this.curveType = kVOI.curveType;
		this.displayMode = kVOI.displayMode;
		this.fixed = kVOI.fixed;
		this.ID = kVOI.ID;
		this.ignoreMax = kVOI.ignoreMax;
		this.ignoreMin = kVOI.ignoreMin;
		this.ignoreMaxR = kVOI.ignoreMaxR;
        this.ignoreMinR = kVOI.ignoreMinR;
        this.ignoreMaxG = kVOI.ignoreMaxG;
        this.ignoreMinG = kVOI.ignoreMinG;
        this.ignoreMaxB = kVOI.ignoreMaxB;
        this.ignoreMinB = kVOI.ignoreMinB;

		if ( kVOI.listenerList != null) {
			kVOI.listenerList.getListenerCount(VOIListener.class);
			VOIListener [] voiList = kVOI.listenerList.getListeners(VOIListener.class);

			for (int i = 0; i < voiList.length; i++) {
				this.addVOIListener(voiList[i]);
			}
		}


		this.name = new String( kVOI.name );
		this.opacity = kVOI.opacity;
		this.polarity = kVOI.polarity;

		this.process = kVOI.process;
		this.UID = kVOI.UID;
		this.visible = kVOI.visible;

		this.watershedID = kVOI.watershedID;
		this.xBounds[0] = kVOI.xBounds[0];
		this.xBounds[1] = kVOI.xBounds[1];
		this.yBounds[0] = kVOI.yBounds[0];
		this.yBounds[1] = kVOI.yBounds[1];
		this.zBounds[0] = kVOI.zBounds[0];
		this.zBounds[1] = kVOI.zBounds[1];
		this.extension = new String( kVOI.extension );

		//this.update();
	}

	/**
	 * adds the update listener.
	 *
	 * @param  listener  VOIListener.
	 */
	public void addVOIListener(VOIListener listener) {

		if (listenerList == null) {
			listenerList = new EventListenerList();
		}
		if ( !hasListener( listener ) )
		{
			listenerList.add(VOIListener.class, listener);        
		}    
	}

	/**
	 * Finds the area of the entire VOI of the VOIContour type only.
	 *
	 * @return  returns the area
	 */
	public double area() {
		if ( curveType != CONTOUR ) {
			return 0;
		}
		double totalArea = 0;
		for (int i = 0; i < curves.size(); i++) {
			totalArea += curves.elementAt(i).area();
		}

		return totalArea;
	}
	
	
	/**
	 * Calculate the distance of the largest line segment contained entirely within the VOI
	 * @param xRes
	 * @param yRes
	 * @param zRes
	 * @return largestDistance
	 */
	public double calcLargestDistance(BitSet mask, int[] extents, float[] res) {
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
		Vector3f kPos1 = new Vector3f();
		Vector3f kPos2 = new Vector3f();
		return calcLargestDistance( mask, extents, res[0], res[1], res[2], xPts, yPts, zPts, kPos1, kPos2 );
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
    public Object clone() {
        Object obj = new VOI(this);

        if (listenerList != null) {
            VOIListener [] voiList = listenerList.getListeners(VOIListener.class);
            for (int i = 0; i < voiList.length; i++) {
                ((VOI)obj).addVOIListener(voiList[i]);
            }
        }

        return obj;
    }
	 */

	/**
	 * Clone function that calls the super (modelserializable) clone and then manually copies references to the
	 * transient VOIListeners (eventlistenerlist)
	 */
	@Override
	public Object clone() {
		final Object obj = super.clone();

		if (listenerList != null) {
			listenerList.getListenerCount(VOIListener.class);
			final VOIListener[] voiList = listenerList.getListeners(VOIListener.class);

			for (final VOIListener element : voiList) {
				((VOI) obj).addVOIListener(element);
			}
		}

		return obj;
	}

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

		if ((process == true) && (curveType == CONTOUR)) {

			for (int i = 0; i < curves.size(); i++) {
				curves.elementAt(i).getBounds(xBounds, yBounds, zBounds);

				int xbs = (int) xBounds[0];
				int xbe = (int) xBounds[1];
				int ybs = (int) yBounds[0];
				int ybe = (int) yBounds[1];

				for (int y = ybs; y < ybe; y++) {
					int offset = y * xDim; // a horizontal offset

					for (int x = xbs; x < xbe; x++) {

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

			for (int i = 0; i < size; i++) {
				pt = ((VOIPoint) (curves.elementAt(i))).exportPoint();
				if ( pt.Z == slice )
				{
					int offset = MipavMath.round((pt.Y * xDim) + pt.X);

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
	 */
	public void createBinaryMask(int xDim, int yDim, int slice, BitSet mask, boolean XOR, boolean onlyActive) {

		int offsetZ = slice * xDim * yDim;

		if ((process == true) && (curveType == CONTOUR)) {
			//System.err.println("XOR is " + XOR);
			//System.err.println("polarity additive?: " + (polarity == ADDITIVE));
			for (int i = 0; i < curves.size(); i++) {

				if (!onlyActive || ((VOIContour) (curves.elementAt(i))).isActive()) {
					curves.elementAt(i).getBounds(xBounds, yBounds, zBounds);
					// Keep the next four lines!!
					int xbs = (int) xBounds[0];
					int xbe = (int) xBounds[1];
					int ybs = (int) yBounds[0];
					int ybe = (int) yBounds[1];

					// System.err.println("Xbounds 0 = " + xBounds[0] + " Xbounds 1 = " + xBounds[1]);
					for (int y = ybs; y < ybe; y++) {
						int offset = offsetZ + (y * xDim);

						for (int x = xbs; x < xbe; x++) {

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

			for (int i = 0; i < size; i++) {
				pt = ((VOIPoint) (curves.elementAt(i))).exportPoint();

				if (!onlyActive || ((VOIPoint) (curves.elementAt(i))).isActive()) {
					int offset = MipavMath.round((slice * xDim * yDim) + (pt.Y * xDim) + pt.X);

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
	 * @param   contour  element of VOI
	 *
	 * @return  mask the binary mask
	 */
	public BitSet createBinaryMask(int xDim, int yDim, int slice, VOIBase contour) {
		BitSet mask;

		// System.err.println("creating binary mask");
		try {
			mask = new BitSet(xDim * yDim); // bitset with a rectangular size
		} catch (OutOfMemoryError oome) {
			return null;
		}
		if ((process == true) && (curveType == CONTOUR)) {
			contour.getBounds(xBounds, yBounds, zBounds);

			int xbs = (int) xBounds[0];
			int xbe = (int) xBounds[1];
			int ybs = (int) yBounds[0];
			int ybe = (int) yBounds[1];

			for (int y = ybs; y < ybe; y++) {
				int offset = y * xDim; // a horizontal offset

				for (int x = xbs; x < xbe; x++) {

					if (contour.contains(x, y, slice) &&
							(polarity == ADDITIVE)) {

						if (mask.get(offset + x)) {

							// System.err.println("clearing");
							mask.clear(offset + x);
						} else {
							mask.set(offset + x);
						}
					} else if (contour.contains(x, y, slice) &&
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
			pt = ((VOIPoint)contour).exportPoint();
			if ( pt.Z == slice )
			{
				int offset = MipavMath.round((pt.Y * xDim) + pt.X);

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
	 * Creates a 3D binary mask. 
	 * @param mask mask written into
	 * @param xDim x-dimensions of the image.
	 * @param yDim y-dimensions of the images.
	 * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI 
	 * @param onlyActive Only mask regions that are active (i.e. selected )
	 */
	public void createBinaryMask3D(BitSet mask, int xDim, int yDim, boolean XOR, boolean onlyActive) {
		int nGons = curves.size();

		if ((process == true) && (curveType == CONTOUR)) {
			for (int i = 0; i < nGons; i++) {

				if (!onlyActive || ((VOIContour) (curves.elementAt(i))).isActive()) {

					curves.elementAt(i).setMask( mask, xDim, yDim, XOR, polarity );     
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
		} else if ((process == true) && (curveType == LINE)) {
			double x0, x1, y0, y1;
			double distance;
			int i, j;
			int indexX, indexY;
			double myY, myX, yInc, xInc;
			int len;
			VOIBase vBase;
			Vector3f vf0;
			Vector3f vf1;
			for (i = 0; i < curves.size(); i++) {
				vBase = curves.elementAt(i);
				vf0 = vBase.get(0);
				vf1 = vBase.get(1);
				x0 = vf0.X;
				y0 = vf0.Y;
				x1 = vf1.X;
				y1 = vf1.Y;
				distance = Math.sqrt(((x1 - x0) * (x1 - x0)) + ((y1 - y0) * (y1 - y0)));
				myY = y0;
				myX = x0;
				xInc = (x1 - x0) / (2 * distance);
				yInc = (y1 - y0) / (2 * distance);

				len = (int) Math.round(2 * distance);
				indexX = -1;
				indexY = -1;
				for (j = 0; j < len; j++) {

					if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
						indexY = (int) Math.round(myY);
						indexX = (int) Math.round(myX);
						int offset = Math.round(vf0.Z) * xDim * yDim + (indexY * xDim) + indexX;
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

					myX = myX + xInc;
					myY = myY + yInc;
				} // for (j = 0; j < len; j++)
			} // for (i = 0; i < curves.size(); i++)
		}
	}
	
	/**
	 * Creates a 3D binary mask. 
	 * @param mask mask written into
	 * @param xDim x-dimensions of the image.
	 * @param yDim y-dimensions of the images.
	 * @param XOR indicates that nested VOI contours will be exclusive ORed with other contours of the VOI 
	 * @param onlyActive Only mask regions that are active (i.e. selected )
	 */
	public void createOneElementBinaryMask3D(BitSet mask, int xDim, int yDim, boolean XOR, boolean onlyActive, int elementNum) {

		if ((process == true) && (curveType == CONTOUR)){

			if (!onlyActive || ((VOIContour) (curves.elementAt(elementNum))).isActive()) {

				curves.elementAt(elementNum).setMask( mask, xDim, yDim, XOR, polarity );     
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
		} else if ((process == true) && (curveType == LINE)) {
			double x0, x1, y0, y1;
			double distance;
			int i, j;
			int indexX, indexY;
			double myY, myX, yInc, xInc;
			int len;
			VOIBase vBase;
			Vector3f vf0;
			Vector3f vf1;
			for (i = 0; i < curves.size(); i++) {
				vBase = curves.elementAt(i);
				vf0 = vBase.get(0);
				vf1 = vBase.get(1);
				x0 = vf0.X;
				y0 = vf0.Y;
				x1 = vf1.X;
				y1 = vf1.Y;
				distance = Math.sqrt(((x1 - x0) * (x1 - x0)) + ((y1 - y0) * (y1 - y0)));
				myY = y0;
				myX = x0;
				xInc = (x1 - x0) / (2 * distance);
				yInc = (y1 - y0) / (2 * distance);

				len = (int) Math.round(2 * distance);
				indexX = -1;
				indexY = -1;
				for (j = 0; j < len; j++) {

					if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
						indexY = (int) Math.round(myY);
						indexX = (int) Math.round(myX);
						int offset = Math.round(vf0.Z) * xDim * yDim + (indexY * xDim) + indexX;
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

					myX = myX + xInc;
					myY = myY + yInc;
				} // for (j = 0; j < len; j++)
			} // for (i = 0; i < curves.size(); i++)
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
	 * @param   mask  the short mask
	 * @param   xDim  x dimension, used to index into the image
	 * @param   yDim  y dimension, used to index into the image
	 * @param   zDim  z dimension, used to index into the image
	 * @param   XOR   indicates that nested VOI contours will be exclusive ORed with other contours of the VOI
	 * @return  returns the mask
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

	public void dispose(){
		removeAllVOIListeners();
		if (curves != null) {
			for (int j = curves.size() - 1; j >= 0; j--) {
				VOIBase base = curves.remove(j);
				base.clear();
				base.dispose();
				base = null;
			}
			curves = null;
		}

		xBounds = null;
		yBounds = null;
		zBounds = null;
	}


	/**
	 * two VOIs are the same if they have the same name.
	 * @param   str  name of the VOI to compare this to.
	 * @return  true if the names of the VOIs are equal.
	 */
	public boolean equals(String str) {
		return (name.equals(str));
	}

	/**
	 * Get Vector3fs from the VOI; can only use with Point.
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
	 * Returns the first point.
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
	 */
	public Vector<Vector3f> exportPoints(int slice) {

		if (curveType != POINT) {
			return null;
		}

		Vector<Vector3f> points = new Vector<Vector3f>();
		for (int i = 0; i < curves.size(); i++) {

			if (curves.elementAt(i).slice() == slice) {
				points.add(((VOIPoint) (curves.elementAt(i))).exportPoint());
			}
		}

		return points;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	@Override
	public void finalize() throws Throwable {
		if (curves != null) {
			for (int j = curves.size() - 1; j >= 0; j--) {
				VOIBase base = curves.remove(j);
				base.clear();
			}
			curves = null;
		}

		xBounds = null;
		yBounds = null;
		zBounds = null;

		super.finalize();
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
				akBounds[0].copy(kBounds[0]);
				akBounds[1].copy(kBounds[1]);
			}
			akBounds[0].min(kBounds[0]);
			akBounds[1].max(kBounds[1]);
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

	public ArrayList<String> getComments() {
		return comments;
	}

	/**
	 * Returns the contour graph associated with this voi.
	 *
	 * @return  contour graph associated with this voi.
	 */
	public ViewJFrameGraph getContourGraph() {
		return contourGraph;
	}

	/**
	 * Accessor that returns the curves making up the VOI.
	 *
	 * @return  the curves
	 */
	public VOIBaseVector getCurves() {
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
	 * Accessor that returns the VOI extension.
	 * @return extension of voi file name of voi was read in through file
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

		if ((curveType == LINE) || (curveType == POLYLINE) || (curveType == POLYLINE_SLICE) || (curveType == PROTRACTOR) || (curveType == ANNOTATION)|| (curveType == POINT)) {
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
	 * Accessor that returns the minimum of the range of intensities to ignore.
	 *
	 * @return  The minimum.
	 */
	public float getMinimumIgnore() {
		return ignoreMin;
	}
	
	/**
     * Accessor that returns the maximum of the range of red intensities to ignore.
     *
     * @return  The maximum.
     */
    public float getMaximumIgnoreR() {
        return ignoreMaxR;
    }

    /**
     * Accessor that returns the minimum of the range of red intensities to ignore.
     *
     * @return  The minimum.
     */
    public float getMinimumIgnoreR() {
        return ignoreMinR;
    }
    
    /**
     * Accessor that returns the maximum of the range of green intensities to ignore.
     *
     * @return  The maximum.
     */
    public float getMaximumIgnoreG() {
        return ignoreMaxG;
    }

    /**
     * Accessor that returns the minimum of the range of green intensities to ignore.
     *
     * @return  The minimum.
     */
    public float getMinimumIgnoreG() {
        return ignoreMinG;
    }
    
    /**
     * Accessor that returns the maximum of the range of blue intensities to ignore.
     *
     * @return  The maximum.
     */
    public float getMaximumIgnoreB() {
        return ignoreMaxB;
    }

    /**
     * Accessor that returns the minimum of the range of blue intensities to ignore.
     *
     * @return  The minimum.
     */
    public float getMinimumIgnoreB() {
        return ignoreMinB;
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
	 * @return  the number of points in the position and intensity array that have a valid data.
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
	 * Accessor that returns whether voi is included in processing.
	 *
	 * @return  the process
	 */
	public boolean getProcess() {
	    boolean anyProcess = process;
        for (int i = 0; i < curves.size(); i++) {
            anyProcess |= (curves.elementAt(i)).getProcess();
        }
        return anyProcess;
	}

	/**
	 * Returns the number of contours in this VOI.
	 * @return number of contours in this VOI.
	 */
	public int getSize()
	{
		return curves.size();
	}


	/**
	 * Returns a list of contours on a given slice.
	 * @param iSlice the slice in the image.
	 * @return list of contours for the slice.
	 */
	public Vector<VOIBase> getSliceCurves( int iSlice )
	{
		return getSliceCurves( VOIBase.ZPLANE, iSlice );
	}

	/**
	 * Returns a list of contours for the given image orientation and slice.
	 * @param iPlane, the plane direction to slice the 3D image (x, y, or z).
	 * @param iSlice the slice number.
	 * @return list of contours.
	 */
	public Vector<VOIBase> getSliceCurves( int iPlane, int iSlice )
	{
		Vector<VOIBase> sliceCurves = new Vector<VOIBase>();      
		for ( int i = 0; i < curves.size(); i++ )
		{
			if ( (curves.elementAt(i).getPlane() & iPlane) == iPlane && curves.elementAt(i).slice(iPlane) == iSlice )
			{
				sliceCurves.add( curves.elementAt(i) );
			}
		}
		return sliceCurves;
	}

	/**
	 * Returns the number of contours for a given slice.
	 * @param iSlice the slice number in the 3D image.
	 * @return number of contours for the slice.
	 */
	public int getSliceSize( int iSlice )
	{
		return getSliceSize( VOIBase.ZPLANE, iSlice );
	}

	/**
	 * Returns the number of contours for a given image orientation and slice in a 3D image. 
	 * @param iPlane the plane direction to slice the 3D image (x,y, or z).
	 * @param iSlice the slice number.
	 * @return the number of contours.
	 */
	public int getSliceSize( int iPlane, int iSlice )
	{
		int sliceSize = 0;
		for ( int i = 0; i < curves.size(); i++ )
		{
			if ( (curves.elementAt(i).getPlane() & iPlane) == iPlane && curves.elementAt(i).slice(iPlane) == iSlice )
			{
				sliceSize++;
			}
		}
		return sliceSize;
	}

	/**
	 * Sorts the contours based on the image default orientation.
	 * @param iDim the number of slices in the image for the default z-direction of the image.
	 * @return an array of contour lists, one list per slice in the image.
	 */
	public Vector<VOIBase>[] getSortedCurves( int iDim ) {
		return getSortedCurves( VOIBase.ZPLANE, iDim );
	}

	/**
	 * Sorts the contours based on a given image orientation and the desired image depth for that orientation.
	 * @param iPlane the direction to slice the 3D image (x, y, or z).
	 * @param iDim the number of slice in the given orientation.
	 * @return an array of contour lists, one per slice in the image.
	 */
	@SuppressWarnings("unchecked")
	public Vector<VOIBase>[] getSortedCurves( int iPlane, int iDim ) {
		Vector<VOIBase>[] kTemp = new Vector[iDim];
		for ( int i = 0; i < iDim; i++ )
		{
			kTemp[i] = new Vector<VOIBase>();
		}      

		for ( int i = 0; i < curves.size(); i++ )
		{
			if ( curves.elementAt(i) instanceof VOIPoint )
			{
				if ( (iPlane == VOIBase.XPLANE) && curves.elementAt(i).elementAt(0).X < iDim )
				{
					kTemp[(int)curves.elementAt(i).elementAt(0).X].add( curves.elementAt(i) );
				}
				else if ( (iPlane == VOIBase.YPLANE) && curves.elementAt(i).elementAt(0).Y < iDim )
				{
					kTemp[(int)curves.elementAt(i).elementAt(0).Y].add( curves.elementAt(i) );
				}
				else if ( (iPlane == VOIBase.ZPLANE) && curves.elementAt(i).elementAt(0).Z < iDim )
				{
					kTemp[(int)curves.elementAt(i).elementAt(0).Z].add( curves.elementAt(i) );
				}
			}
			else if (iPlane == VOIBase.NOT_A_PLANE) {
			    if (curves.elementAt(i).getPlane() == VOIBase.NOT_A_PLANE) {
			        System.err.println("Contour " + i + " does not lie on an X, Y, or Z plane");
			    }
			}
			else if ( (curves.elementAt(i).getPlane() & iPlane) == iPlane )
			{
				int slice = curves.elementAt(i).slice(iPlane);
				kTemp[slice].add( curves.elementAt(i) );
			}
		}

		return kTemp;
	}

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

	public boolean hasListener( VOIListener listener )
	{
		if (listenerList.getListenerCount(VOIListener.class) == 0)
		{
			return false;
		}        
		// Guaranteed to return a non-null array
		Object[] listeners = listenerList.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length - 2; i >= 0; i -= 2) {
			if (listeners[i] == VOIListener.class && 
					((VOIListener) listeners[i + 1]) == listener) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Imports a new curve into this voi.
	 * @param x x-positions of the curve.
	 * @param y y-positions of the curve.
	 * @param z z-positions of the curve.
	 * @return the new VOIBase object created by the importCurve.
	 */
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
		curves.addElement(curve);
		return curve;
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
		curves.addElement(curve);
	}

	/**
	 * Imports the curve into the VOI, testing for which type.
	 * @param  pt     array of 3D points to import
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
		curves.addElement(curve);
	}


	/**
	 * Imports the curve into the VOI.
	 * @param  curve  curve to import
	 */
	public void importCurve(VOIBase curve) {
		curves.addElement(curve.clone());
	}

	/**
	 * Imports just the VOIs in a slice into this VOI.
	 *
	 * @param  slice     slice indicates the slice where the contour(s) is to be located
	 * @param  voiSlice  voiSlice indicates the slice where the contour(s) is to be copied from
	 * @param  voi       added to VOI
	 * @param  resize    when true clear the existing curves.
	 */
	public void importNewVOI(int slice, int voiSlice, VOI voi, boolean resize) {
		if ( resize )
		{
			curves.clear();
		}
		Vector<VOIBase> sliceCurves = voi.getSliceCurves(voiSlice);
		for ( int i = 0; i < sliceCurves.size(); i++ )
		{
			float[] x = new float[sliceCurves.elementAt(i).size()];
			float[] y = new float[sliceCurves.elementAt(i).size()];
			float[] z = new float[sliceCurves.elementAt(i).size()];
			float[] z2 = new float[sliceCurves.elementAt(i).size()];
			for ( int j = 0; j < sliceCurves.elementAt(i).size(); j++ )
			{
				z2[j] = slice;
			}
			sliceCurves.elementAt(i).exportArrays(x, y, z);
			importCurve(x, y, z2);
		}
	}

	/**
	 * Import a new point into this VOI. This VOI must be of type POINT.
	 * @param point
	 */
	public void importPoint(Vector3f point) {
		VOIPoint voiPt = null;

		if (curveType == POINT) {
			voiPt = new VOIPoint(POINT, point);
			curves.addElement(voiPt);
		} else {
			return;
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
			contour = new VOIContour(true);
		} else if (curveType == POLYLINE) {
			contour = new VOIContour(false);
		} else {
			return;
		}

		contour.importPolygon(gon, slice);
		curves.addElement(contour);
	}


	public Vector<Polygon> exportPolygon( int iSlice ) {

		if (curveType != CONTOUR) {
			return null;
		} 

		Vector<Polygon> polygons = new Vector<Polygon>();
		for ( int i = 0; i < curves.size(); i++ )
		{
			if ( curves.elementAt(i).isActive() )
			{
				if ( (curves.elementAt(i).getPlane() & VOIBase.ZPLANE) == VOIBase.ZPLANE && 
					  curves.elementAt(i).slice(VOIBase.ZPLANE) == iSlice )
				{

					polygons.add( ((VOIContour)curves.elementAt(i)).exportPolygon() );
				}
			}
		}
		return polygons;
	}



	/**
	 * Accessor that tells if the VOI is active.
	 *
	 * @return  boolean active
	 */
	public boolean isActive() {
		boolean anyActive = active;
		for (int i = 0; i < curves.size(); i++) {
			anyActive |= (curves.elementAt(i)).isActive();
		}
		return anyActive;
	}


	/**
	 * Returns true iff all contours in this VOI are active.
	 * @return true iff all contours in this VOI are active.
	 */
	public boolean isAllActive() {
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
	 * Accessor that tells if VOI is fixed. A fixed VOI cannot be modified.
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
				return true;
			}
		}
		return false;
	}

	/**
	 * Tests if a point is near a point.
	 *
	 * @param   x            x coordinate of point
	 * @param   y            y coordinate of point
	 * @param   slice        index of slice in curve array
	 * @return  result of test
	 */
	public boolean nearPoint(int x, int y, int slice) {

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
				return true;
			}
		}
		return false;
	}

	/**
	 * Finds the minimum distance from a point to a contour. If the point is in a contour, the distance is given a
	 * negative sign. If the point is outside contours, the distance is given a positive sign. The contour points are
	 * connected by straight line segments between the points so simply find the distances from the point to each line
	 * segment comprising the contours.
	 *
	 * @param   x  input x-position
	 * @param   y  input y-position
	 * @param   z  input z-position
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

	public void removeAllVOIListeners() {
    	if ( listenerList == null )
    	{
    		return;
    	}
    	VOIListener[] list = listenerList.getListeners(VOIListener.class);
    	for ( int i = 0; i < list.length; i++ )
    	{
    		listenerList.remove(VOIListener.class, list[i]);
    	}
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
	 * Clears VOI of all curves.
	 *
	 * @param  slice  index of slice of curves to remove
	 */
	public void removeCurves() {
		curves.removeAllElements();
	}

	/**
	 * Clears VOI of all curves at a slice.
	 *
	 * @param  slice  index of slice of curves to remove
	 * @deprecated
	 */
	@Deprecated
	public void removeCurves(int slice) {
		Vector<VOIBase> removeCurves = getSliceCurves(slice);
		for ( int i = 0; i < removeCurves.size(); i++ )
		{
			curves.removeElement(removeCurves.elementAt(i));
		}
	}

	/**
	 * removes the update listener.
	 *
	 * @param  listener  VOIListener to remove.
	 */
	public void removeVOIListener(VOIListener listener) {

		if (listenerList == null) {
			listenerList = new EventListenerList();
		}

		listenerList.remove(VOIListener.class, listener);
	}

	/**
	 * Sets whether or not the VOI is active.
	 *
	 * @param  act  boolean to set active to
	 */
	public void setActive(boolean act) {
		//System.err.println( getName() + " " + active + " -> " + act );
		if ( active != act )
		{
			this.active = act;
			if ( active )
			{
				fireVOIselection();
			}
		}
	}


	/**
	 * Sets all contours in the VOI as active or inactive.
	 *
	 * @param  flag  boolean to set VOI active or inactive
	 */
	public void setAllActive(boolean flag) {
		for (int i = 0; i < curves.size(); i++) {
			(curves.elementAt(i)).setActive(flag);
		}
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
		this.color = new Color( color.getRed(), color.getGreen(), color.getBlue() );
		fireVOIColorChange(color);
	}

	/**
	 * Accessor that sets the color to the parameter.
	 *
	 * @param  hue  the color of the VOI
	 */
	public void setColor(float hue) {
		this.setColor(Color.getHSBColor(hue, (float) 1.0, (float) 1.0));
	}

	public void setComments(String comment) {
		comments.add(comment);
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
	 * Sets the list of contours in this VOI to a new list.
	 * @param newCurves new list of VOIBase contours.
	 */
	public void setCurves(VOIBaseVector newCurves) {
		this.curves = newCurves;
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
	 * Sets the VOI extenstion of voi file name of voi was read in through file
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
     * Accessor that sets the maximum of the range of red intensities to ignore.
     *
     * @param  max  The maximum.
     */
    public void setMaximumIgnoreR(float max) {
        ignoreMaxR = max;
    }

    /**
     * Accessor that sets the minimum of the range of red intensities to ignore.
     *
     * @param  min  The minimum.
     */
    public void setMinimumIgnoreR(float min) {
        ignoreMinR = min;
    }
    
    /**
     * Accessor that sets the maximum of the range of green intensities to ignore.
     *
     * @param  max  The maximum.
     */
    public void setMaximumIgnoreG(float max) {
        ignoreMaxG = max;
    }

    /**
     * Accessor that sets the minimum of the range of green intensities to ignore.
     *
     * @param  min  The minimum.
     */
    public void setMinimumIgnoreG(float min) {
        ignoreMinG = min;
    }
    
    /**
     * Accessor that sets the maximum of the range of blue intensities to ignore.
     *
     * @param  max  The maximum.
     */
    public void setMaximumIgnoreB(float max) {
        ignoreMaxB = max;
    }

    /**
     * Accessor that sets the minimum of the range of blue intensities to ignore.
     *
     * @param  min  The minimum.
     */
    public void setMinimumIgnoreB(float min) {
        ignoreMinB = min;
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
		for(int i=0; i<curves.size(); i++) {
		    curves.get(i).setProcess(flag);
		}
	}
	
	/**
     * Allows VOIBase's to set their parent's process flag without affecting other contours.  If not all
     * contours are in the same state, this will not be set.
     *
     * @param  flag  the process flag
     */
	void notifyParentVOIProcess(boolean flag) {
	    for(int i=0; i<curves.size(); i++) {
            if(flag != curves.get(i).getProcess()) {
                return; //only set if all contours are in same state
            }
        }
	    this.process = flag;
	}

	/**
	 * Sets the thickness of the VOI
	 * @param newThickness the new thickness
	 */
	public void setThickness(int newThickness) {
		this.thickness = newThickness;
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
	 * @deprecated
	 */
	public void setXYDim(int xDim, int yDim) {	}

	@Override
	public String toString() {
		return name;
	}

	/**
	 * Trims all active contours in the VOI based on the Preferences getTrim and getTrimAdjacent values. 
	 */
	public void trim()
	{
		for ( int i = 0; i < curves.size(); i++ )
		{
			if ( curves.get(i).isActive() )
			{
				curves.get(i).trimPoints(Preferences.getTrimVoi(),
						Preferences.getTrimAdjacient());
			}
		}
	}


	public void update()
	{
		for ( int i = 0; i < curves.size(); i++ )
		{
			curves.elementAt(i).update();
		}
	}

	public void updateActive()
	{
		boolean anyActive = false;
		for (int i = 0; i < curves.size(); i++) {
			anyActive |= (curves.elementAt(i)).isActive();
		}
		setActive( anyActive );        
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



	// Notify all listeners that have registered interest for
	// notification on this event type. The event instance
	// is lazily created using the parameters passed into
	// the fire method.
	/**
	 * Fires a VOI event based on the VOI. calls the listener's <code>addedVOI()</code> method.
	 *
	 * @param  curve new curve added to this VOI.
	 */
	protected void fireVOIBaseAdded(VOIBase curve) {
		//System.err.println( "fireVOIBaseAdded" );
		for ( int i = 0; i < curves.size(); i++ )
		{
			curves.elementAt(i).getLabel();
		}

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
		VOIEvent voiUpdate = new VOIEvent(this, curve);

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
	 * @param  curve curve removed from this VOI.
	 */
	protected void fireVOIBaseRemoved(VOIBase curve) {
		//System.err.println( "fireVOIBaseRemoved" );

		for ( int i = 0; i < curves.size(); i++ )
		{
			curves.elementAt(i).getLabel();
		}

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
		VOIEvent voiUpdate = new VOIEvent(this, curve);

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


	protected void fireVOIColorChange(Color color)
	{
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
	 * Fires a VOI event based on the VOI. calls the listener's <code>removedVOI()</code> method.
	 */
	protected void fireVOIselection() {
		//System.err.println( "fireVOIselection" );
		for ( int i = 0; i < curves.size(); i++ )
		{
			curves.elementAt(i).getLabel();
		}
		//System.err.println( "VOI.fireVOIselection" );
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
		VOIEvent voiUpdate = new VOIEvent(this);

		// Guaranteed to return a non-null array
		Object[] listeners = listenerList.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length - 1; i >= 0; i--) {

			if (listeners[i] instanceof VOIListener) {
				((VOIListener) listeners[i]).selectedVOI(voiUpdate);
			}
		}
	}
}
