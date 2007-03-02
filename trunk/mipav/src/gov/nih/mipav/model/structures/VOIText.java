package gov.nih.mipav.model.structures;


import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.Preferences;

import java.awt.*;


/**
 * <p>Title: VOI Text</p>
 *
 * <p>Description: VOI Text is used to hold an annotation that will be displayed on screen with the chosen font size,
 * color, type, and descriptors (BOLD, ITALIC) it e</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */
public class VOIText extends VOIBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6408262409409720727L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * The descriptors for the font which start at PLAIN and are additive (PLAIN + BOLD = bold) (BOLD+ITALIC = bold and
     * italic) etc.
     */
    private int fontDescriptors = Font.PLAIN;

    /** The name (or type) of the font. */
    private String fontName = "Serif";

    /** The size of the font (half-sizes not allowed...int). */
    private int fontSize = 12;

    /** not used except as dummy float. */
    private float[] origin = new float[3];

    /** This must be kept separate (but parallel) to the VOI color. */
    private Color textColor = Color.WHITE;

    /** The color used to draw behind the main text (so that the text will stand out)*/
    private Color backgroundColor = Color.BLACK;
    
    /**
     * The font is stored here so that it does not have to be reallocated on each redraw. It is only new'd at the
     * beginning or if the fontDescriptors variable changes
     */
    private Font textFont = new Font(fontName, fontDescriptors, fontSize);

    /** The String to be displayed. */
    private String textString = new String();
    
    /** If this is set to true, a draggable arrow will be displayed */
    private boolean useMarker = true;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * default constructor.
     */
    public VOIText() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Does nothing.
     *
     * @param   _x           DOCUMENT ME!
     * @param   _y           DOCUMENT ME!
     * @param   forceReload  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean contains(int _x, int _y, boolean forceReload) {
        return false;
    }

    /**
     * Determines whether the point in question (_x, _y) is within the Text's bounds.
     *
     * @param   _x      int x location
     * @param   _y      int y location
     * @param   zoomX   float x zoom of the display
     * @param   zoomY   float y zoom of the display
     * @param   resols  float[] resolutions of image
     * @param   g       Graphics the gfx
     *
     * @return  boolean true if within, false if outside
     */
    public boolean contains(int _x, int _y, float zoomX, float zoomY, float[] resols, Graphics g) {

        int x, y;

        x = Math.round(((Point3Df) (elementAt(0))).x);
        y = Math.round(((Point3Df) (elementAt(0))).y);
        
        // the width tells us the width of the font as it has been drawn on screen (not the width of the font's
        // unzoomed size)
        int width = (int) (g.getFontMetrics(textFont).stringWidth(textString) / (zoomX * resols[0]));

        // the ascent value tells us how far the font travels from the baseline to the top of the text, for some reason
        // it needed    to be divided by two
        int ascentValue = (int) (g.getFontMetrics(textFont).getStringBounds(textString, g).getHeight() /
                                     (2 * zoomY * resols[1]));

        if ((_x < x) || (_x > (x + width)) || (_y < (y - ascentValue)) || (_y > y)) {
            return false;
        }

        return true;
    }

    /**
     * Determines if the given point (x,y) is within 3 pixels of the arrow's tip.
     * @param _x x coordinate
     * @param _y y coordinate
     * @param zoomX zoom level of the image
     * @param resolutionX the x resolution
     * @param resolutionY the y resolution
     * @return
     */
    public boolean nearMarkerPoint(int _x, int _y, float zoomX, float resolutionX, float resolutionY) {
    	int xS, yS;
    	
    	if (!useMarker) {
    		return false;
    	}
    	
    	xS = MipavMath.round(((Point3Df) (elementAt(1))).x * zoomX * resolutionX);
        yS = MipavMath.round(((Point3Df) (elementAt(1))).y * zoomX * resolutionY);
        
    	float dist;       
        dist = (float) MipavMath.distance(xS, _x, yS, _y);
        if (dist < 3) {
        	return true;
        }
    	
    	return false;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  direction  DOCUMENT ME!
     */
    public void cycleActivePt(int direction) { }

    /**
     * Contour draws itself.
     *
     * @param  zoomX           magnification for the x coordinate
     * @param  zoomY           magnification for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  originX         Start location of X origin
     * @param  originY         Start location of Y origin
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               graphics to paint in
     * @param  boundingBox     boolean that indicates if boundingBox is on or off
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean boundingBox) {
        int j;

        int xS, yS, xS2, yS2;

        xS = Math.round(((Point3Df) (elementAt(0))).x * zoomX * resolutionX);
        yS = Math.round(((Point3Df) (elementAt(0))).y * zoomY * resolutionY);
      
        xS2 = Math.round(((Point3Df) (elementAt(1))).x * zoomX * resolutionX);
        yS2 = Math.round(((Point3Df) (elementAt(1))).y * zoomY * resolutionY);
     
        
        // draw the arrow if useMarker is true
        if (useMarker) {
        // determine the width/height of the TEXT (for marker line location)
        int width = (int) (g.getFontMetrics(textFont).stringWidth(textString));
        int ascentValue = (int) (g.getFontMetrics(textFont).getStringBounds(textString, g).getHeight() / 2);
        
        int markerX = xS;
        int markerY = yS;
        
        if (xS2 > (xS + width)) {
        	markerX = xS + width;
        } else if (xS2 <= xS) {
        	markerX = xS - 2;
        } else {
        	markerX = xS + width/2;
        }
        
        if (yS2 > yS) {
        	markerY = yS + 3;
        } else if (yS2 <= (yS - ascentValue)) {
        	markerY = yS - ascentValue - 5;
        } else {
        	markerY = yS - ascentValue/2;
        }
        
        this.drawArrow((Graphics2D)g, markerX, markerY, xS2, yS2, .1f);
        } //arrow not off
        if ((textFont != null) && (textFont.getName() == fontName) && (textFont.getStyle() == fontDescriptors)) {
            textFont = textFont.deriveFont(fontSize * zoomX);

        } else {
            textFont = new Font(fontName, fontDescriptors, (int) (fontSize * zoomX));
        }

        Font previousFont = g.getFont();

        g.setFont(textFont);

        if (active == true) {
            g.setColor(Color.RED);            
            g.drawString(textString, xS, yS + 1);
            g.drawString(textString, xS + 1, yS);
        } else {
        	g.setColor(backgroundColor);
        	g.drawString(textString, xS + 1, yS);
            g.drawString(textString, xS - 1, yS);
            g.drawString(textString, xS, yS - 1);
            g.drawString(textString, xS, yS + 1);
        }

        
        g.setColor(textColor);
        g.drawString(textString, xS, yS);
        g.setFont(previousFont);

    }

    /**
     * Dummy function... will not be called
     *
     * @param  zoomX           DOCUMENT ME!
     * @param  zoomY           DOCUMENT ME!
     * @param  resolutionX     DOCUMENT ME!
     * @param  resolutionY     DOCUMENT ME!
     * @param  originX         DOCUMENT ME!
     * @param  originY         DOCUMENT ME!
     * @param  resols          DOCUMENT ME!
     * @param  unitsOfMeasure  DOCUMENT ME!
     * @param  orientation     DOCUMENT ME!
     * @param  g               DOCUMENT ME!
     * @param  boundingBox     DOCUMENT ME!
     * @param  fileInfo        DOCUMENT ME!
     * @param  dim             DOCUMENT ME!
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean boundingBox,
                         FileInfoBase fileInfo, int dim) {

        return;
    }

    /**
     * Method to access point VOI coordinate.
     *
     * @return  3d point
     */
    public Point3Df exportPoint() {
        return (Point3Df) (elementAt(0));
    }

    /**
     * Does nothing.
     *
     * @return  DOCUMENT ME!
     */
    public Point3Df getActivePt() {
        return null;
    }

    /**
     * Calculates the bounds of the point (i.e. returns the point)
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(float[] x, float[] y, float[] z) {
        Point3Df pt = exportPoint();
        x[0] = x[1] = pt.x;
        y[0] = y[1] = pt.y;
        z[0] = z[1] = pt.z;
    }

    /**
     * Gets the font color (for copy/paste operations).
     *
     * @return  Color font color
     */
    public Color getColor() {
        return this.textColor;
    }

    /**
     * Gets a string describing the font's color (for saving in XML header).
     *
     * @return  String formatted string for font's color
     */
    public String getColorString() {
        return new String(textColor.getRed() + "," + textColor.getGreen() + "," + textColor.getBlue());
    }

    /**
     * Gets the font's descriptors (PLAN, BOLD, ITALIC).
     *
     * @return  int font descriptors
     */
    public int getFontDescriptors() {
        return fontDescriptors;
    }

    /**
     * Gets the name (type) of the font.
     *
     * @return  String font name
     */
    public String getFontName() {
        return fontName;
    }

    /**
     * Returns the size of the font.
     *
     * @return  int font size
     */
    public int getFontSize() {
        return fontSize;
    }

    /**
     * Gets a string describing the text location (in the slice, does not include which slice).
     *
     * @return  String formatted location string
     */
    public String getLocationString() {
        return new String(((Point3Df) (elementAt(0))).x + "," + ((Point3Df) (elementAt(0))).y);
    }

    /**
     * Dummy function that should not be called.
     *
     * @param   fileInfo  DOCUMENT ME!
     * @param   dim       DOCUMENT ME!
     * @param   originX   DOCUMENT ME!
     * @param   originY   DOCUMENT ME!
     * @param   resols    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getOrigin(FileInfoBase fileInfo, int dim, float originX, float originY, float[] resols) {
        return origin;
    }

    /**
     * Gets the displayed text.
     *
     * @return  String on-screen text
     */
    public String getText() {
        return textString;
    }

    /**
     * Import points into the contour.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public void importArrays(int[] x, int[] y, int[] z, int n) {
        int i;

        try {
            this.removeAllElements();

            for (i = 0; i < n; i++) {
                this.addElement(new Point3Df(x[i], y[i], z[i]));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * Import points into the contour.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public void importArrays(float[] x, float[] y, float[] z, int n) {
        int i;

        try {
            this.removeAllElements();

            for (i = 0; i < n; i++) {
                this.addElement(new Point3Df(x[i], y[i], z[i]));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * Import point into point VOI.
     *
     * @param  pt  3D point
     */
    public void importPoint(Point3Df pt) {
        this.addElement(pt);
        System.err.println("size is: " + this.size());
    }

    /**
     * Import points into the contour.
     *
     * @param  pt  array of three dimensional points
     */
    public void importPoints(Point3Df[] pt) {

        this.removeAllElements();

        for (int i = 0; i < pt.length; i++) {
            this.addElement(pt[i]);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  d  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    public void moveActivePt(int d, int x, int y) { }


    /**
     * Moves a point on the contour.
     *
     * @param  x  x coordinate
     * @param  y  y coordinate
     */
    public void movePt(int x, int y) {
        replaceElement(x, y, ((Point3Df) (elementAt(nearPoint))).z);
    }

    /**
     * Moves the point to the new location. NO bounds checking is performed
     *
     * @param  xM    amount in pixels to move the line in the x direction
     * @param  yM    amount in pixels to move the line in the y direction
     * @param  zM    amount in pixels to move the line in the z direction
     * @param  xDim  x dimension maximum
     * @param  yDim  y dimension maximum
     * @param  zDim  z dimension maximum
     */
    public void moveVOIPoint(int xM, int yM, int zM, int xDim, int yDim, int zDim) {

        Point3Df pt = exportPoint();

        // removed the bounds checking here because the text
        // should be allowed to be displayed anywhere (not necessary for 1st letter onscreen)

        pt.x = pt.x + xM;
        pt.y = pt.y + yM;
        pt.z = pt.z + zM;
        
        //move the marker point
        Point3Df pt2 = (Point3Df)elementAt(1);
        pt2.x = pt2.x + xM;
        pt2.y = pt2.y + yM;
        pt2.z = pt2.z + zM;     
    }
    
    /**
     * Moves the point to the new location. NO bounds checking is performed
     *
     * @param  xM    amount in pixels to move the line in the x direction
     * @param  yM    amount in pixels to move the line in the y direction
     * @param  zM    amount in pixels to move the line in the z direction
     * @param  xDim  x dimension maximum
     * @param  yDim  y dimension maximum
     * @param  zDim  z dimension maximum
     */
    public void moveMarkerPoint(int xM, int yM, int zM, int xDim, int yDim, int zDim) {

        //move the marker point
        Point3Df pt2 = (Point3Df)elementAt(1);
        pt2.x = xM;
        pt2.y = yM;
       // pt2.z = pt2.z + zM;      
        
    }

    /**
     * Sets the color of the font.
     *
     * @param  color  Color font color
     */
    public void setColor(Color color) {
        this.textColor = color;
    }

    public void setBackgroundColor(Color color) {
    	this.backgroundColor = color;
    }
    
    public Color getBackgroundColor() {
    	return this.backgroundColor;
    }
    
    /**
     * Sets the font's descriptors.
     *
     * @param  fontDescriptors  int font descriptors
     */
    public void setFontDescriptors(int fontDescriptors) {
        this.fontDescriptors = fontDescriptors;
    }

    /**
     * Sets the name (type) of the font.
     *
     * @param  fontName  String font name
     */
    public void setFontName(String fontName) {
        this.fontName = fontName;
    }

    /**
     * Sets the size of the font.
     *
     * @param  fontSize  int font size
     */
    public void setFontSize(int fontSize) {
        this.fontSize = fontSize;
    }

    /**
     * Sets the arrow to be on or off
     * @param mark whether to draw the arrow
     */
    public void setUseMarker(boolean mark) {
    	this.useMarker = mark;
    }
    
    /**
     * gets whether or not the arrow should be drawn
     * @return
     */
    public boolean useMarker() {
    	return this.useMarker;
    }
    
    /**
     * Sets the displayed text.
     *
     * @param  textString  String text to be displayed
     */
    public void setText(String textString) {
        this.textString = textString;
    }


    /**
     * Translates a VOI by some amount in x and y.
     *
     * @param  xT  translate this amount in the x direction
     * @param  yT  translate this amount in the y direction
     */
    public void translate(float xT, float yT) {
        int i;

        for (i = 0; i < size(); i++) {
            ((Point3Df) (elementAt(i))).x += xT;
            ((Point3Df) (elementAt(i))).y += yT;
        }
    }
    
    /**
     * Function to draw an arrow between two given points
     * @param g2d the graphics with which to draw
     * @param xCenter the xcoord of start value
     * @param yCenter the ycoord of start value
     * @param x the arrow tip x value
     * @param y the arrow tip y value
     * @param stroke the size of the arrow
     */
    private void drawArrow(Graphics2D g2d, int xCenter, int yCenter, int x, int y, float stroke) {
        double aDir=Math.atan2(xCenter-x,yCenter-y);
        
        g2d.setColor(backgroundColor);
        g2d.drawLine(x + 1, y + 1, xCenter + 1, yCenter + 1);
        g2d.drawLine(x - 1, y - 1, xCenter - 1, yCenter - 1);
                
        				// make the arrow head solid even if dash pattern has been specified
        Polygon tmpPoly=new Polygon();
        Polygon backPoly1 = new Polygon();
        Polygon backPoly2 = new Polygon();
        Polygon backPoly3 = new Polygon();
        Polygon backPoly4 = new Polygon();
        
        
        int i1=12+(int)(stroke*2);
        int i2=6+(int)stroke;							// make the arrow head the same size regardless of the length length
        tmpPoly.addPoint(x,y);							// arrow tip
        backPoly1.addPoint(x + 1, y);
        backPoly2.addPoint(x - 1, y);
        backPoly3.addPoint(x, y + 1);
        backPoly4.addPoint(x, y - 1);
        
        int x2 = x+xCor(i1,aDir+.5);
        int y2 = y+yCor(i1,aDir+.5);
        tmpPoly.addPoint(x2, y2);
        backPoly1.addPoint(x2 + 1, y2);
        backPoly2.addPoint(x2 - 1, y2);
        backPoly3.addPoint(x2, y2 + 1);
        backPoly4.addPoint(x2, y2 - 1);
        
        
        int x3 = x+xCor(i2,aDir);
        int y3 = y+yCor(i2,aDir);
        tmpPoly.addPoint(x3, y3);
        backPoly1.addPoint(x3 + 1, y3);
        backPoly2.addPoint(x3 - 1, y3);
        backPoly3.addPoint(x3, y3 + 1);
        backPoly4.addPoint(x3, y3 - 1);
        
        int x4 = x+xCor(i1,aDir-.5);
        int y4 = y+yCor(i1,aDir-.5);
        tmpPoly.addPoint(x4, y4);
        backPoly1.addPoint(x4 + 1, y4 + 1);
        backPoly2.addPoint(x4 - 1, y4 - 1);
        backPoly1.addPoint(x4, y4 + 1);
        backPoly2.addPoint(x4, y4 - 1);
        
        tmpPoly.addPoint(x,y);							// arrow tip
        backPoly1.addPoint(x + 1, y + 1);
        backPoly2.addPoint(x - 1, y - 1);
        backPoly3.addPoint(x, y + 1);
        backPoly4.addPoint(x, y - 1);        
        
        g2d.setStroke(new BasicStroke(1f));	
        g2d.drawPolygon(backPoly1);
        //g2d.fillPolygon(backPoly1);
        g2d.drawPolygon(backPoly2);
        //g2d.fillPolygon(backPoly2);
        g2d.drawPolygon(backPoly3);
        g2d.drawPolygon(backPoly4);
        
        
        g2d.setColor(textColor);
        
        g2d.drawLine(x,y,xCenter,yCenter);
        
        
        g2d.drawPolygon(tmpPoly);
        g2d.fillPolygon(tmpPoly);						// remove this line to leave arrow head unpainted
     }
     private static int yCor(int len, double dir) {return (int)(len * Math.cos(dir));}
     private static int xCor(int len, double dir) {return (int)(len * Math.sin(dir));}

    
}
