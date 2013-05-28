package gov.nih.mipav.model.structures;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


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

    public static int xCor(int len, double dir) {return (int)(len * Math.sin(dir));}

    public static int yCor(int len, double dir) {return (int)(len * Math.cos(dir));}

    /**
     * The descriptors for the font which start at PLAIN and are additive (PLAIN + BOLD = bold) (BOLD+ITALIC = bold and
     * italic) etc.
     */
    private int fontDescriptors = Font.PLAIN;

    /** The name (or type) of the font. */
    private String fontName = "Serif";

    /** The size of the font (half-sizes not allowed...int). */
    private int fontSize = 12;
    
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
    
    /** The note stored in VOIText, has same font as textString*/
    private String noteString = new String();
    
    /** If this is set to true, a draggable arrow will be displayed */
    private boolean useMarker = true;


    private ArrayList<String> comments = new ArrayList<String>();
    
    /**
     * default constructor.
     */
    public VOIText() { 
        super( false, false );
    	String prefColor = Preferences.getProperty(Preferences.PREF_VOI_TEXT_BACKGROUND_COLOR);
        
        if (prefColor != null) {
        	this.backgroundColor = MipavUtil.extractColor(prefColor);
        } else {
            Preferences.setProperty(Preferences.PREF_VOI_TEXT_BACKGROUND_COLOR, MipavUtil.makeColorString(Color.black));
            this.backgroundColor = Color.black;
        }
        m_iVOIType = VOI.ANNOTATION;
    }

    /**
     * Constructor, sets positions of the annotation.
     * @param kPositions
     */
    public VOIText( Vector<Vector3f> kPositions )
    {
        super( false, false, kPositions );
        m_iVOIType = VOI.ANNOTATION;
    }

    /**
     * Copy contructor.
     * @param kVOI
     */
    public VOIText( VOIText kVOI )
    {
        super( kVOI );
        this.fontDescriptors = kVOI.fontDescriptors;
        this.fontName = new String( kVOI.fontName );
        this.fontSize = kVOI.fontSize;
        this.textColor = new Color( kVOI.textColor.getRed(), kVOI.textColor.getGreen(), kVOI.textColor.getBlue() );
        this.backgroundColor = new Color( kVOI.backgroundColor.getRed(), kVOI.backgroundColor.getGreen(), kVOI.backgroundColor.getBlue() );
        this.textFont = new Font(fontName, fontDescriptors, fontSize);
        this.textString = new String( kVOI.textString );
        this.noteString = new String( kVOI.noteString );
        this.useMarker = kVOI.useMarker;    
        this.comments = kVOI.comments;
        m_iVOIType = VOI.ANNOTATION;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.model.structures.VOIBase#clone()
     */
    @Override
	public VOIText clone() {
        return new VOIText(this);
    }

    /**
     * Method to access point VOI coordinate.
     *
     * @return  3d point
     */
    public Vector3f exportPoint() {
        return (elementAt(0));
    }

    /**
     * Gets background color.
     * @return background color.
     */
    public Color getBackgroundColor() {
    	return this.backgroundColor;
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
    
    public ArrayList<String> getComments() {
    	return comments;
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
        return new String(((elementAt(0))).X + "," + ((elementAt(0))).Y);
    }

    /**
     * Gets the contained note.
     *
     * @return  Contained note of the VOIText
     */
    public String getNote() {
        return noteString;
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
     * Returns the Font.
     * @return
     */
    public Font getTextFont()
    {
        return textFont;
    }

    public Vector3f getTextWidth()
    {
        if ( size() < 3 )
        {
            return null;
        }
        return elementAt(2);
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
    public void moveVOIPoint(int xM, int yM, int zM) {
        Vector3f pt = exportPoint();

        pt.X = pt.X + xM;
        pt.Y = pt.Y + yM;
        pt.Z = pt.Z + zM;        
    }
    
    /**
     * Sets background color
     * @param color
     */
    public void setBackgroundColor(Color color) {
    	this.backgroundColor = color;
    }
    
    /**
     * Sets the color of the font.
     *
     * @param  color  Color font color
     */
    public void setColor(Color color) {
        this.textColor = color;
    }


    public void setComments(String comment) {
    	comments.add(comment);
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
     * Sets the contained note.
     *
     * @param  noteString  String note stored in VOIText
     */
    public void setNote(String noteString) {
        this.noteString = noteString;
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
     * Sets Font.
     * @param font
     */
    public void setTextFont( Font font )
    {
        textFont = font;
    }
    
    public void setTextWidth( Vector3f kVolume )
    {
        if ( size() < 3 )
        {
            add(kVolume);
            return;
        }
        set(2,kVolume);
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
       
}
