package gov.nih.mipav.model.file;

import java.io.*;
import java.util.*;


/**
 * Class used to assist in the writing of XML files.  has common functions for doing the different types of tags
 * and now has the option of correctly writing Attributes (name="value") for each tag.
 *
 */
class XMLHelper  {
	
	/** The tab character (makes writing out files cleaner-looking). */
    private static final String TAB = "\t";

    /** The charset the XML file is written in. */
    private static final String XML_ENCODING = "UTF-8";
	
    /** The buffered writer */
    private BufferedWriter bw;
    
    
    /** int counter used to keep track of the tab level (for tags)*/
	private int tabLevel = 0;
    
	/**
	 * Main constructor
	 * @param b_writer the buffered writer
	 */
	public XMLHelper(BufferedWriter b_writer) {
		this.bw = b_writer;
	}
	
	/**
	 * Writes a closed tag (tag, end tag) with the addition of attributes (name="value")
	 * from within a Vector (can do any number of XMLAttributes ...class included below)
	 * @param tag the tag name
	 * @param val the tag's value
	 * @param attr vector of XMLAttributes
	 */
	public final void closedTag(String tag, String val, Vector<XMLAttributes> attr) {
    	
		try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            bw.write("<" + tag);
            
            String attrStr;
            for (int i = 0; i < attr.size(); i++) {
            	
            	attrStr = attr.elementAt(i).getValue().trim().replaceAll("&", "&amp;");
            	attrStr = attrStr.trim().replaceAll("\"", "&quot;");
            	attrStr = attrStr.trim().replaceAll("<", "&lt;");
            	attrStr = attrStr.trim().replaceAll(">", "&gt;");
            	attrStr = new String(attrStr.getBytes(XML_ENCODING));
            	
            	bw.write(" " + attr.elementAt(i).getName() + "=\"" + attrStr + "\"");
            }
            
            String writeVal = val.trim().replaceAll("&", "&amp;");
            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            writeVal = writeVal.trim().replaceAll("<", "&lt;");
            writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(XML_ENCODING));
            bw.write(">" + writeVal + "</" + tag + ">");

            bw.newLine();
        } catch (IOException ex) { }
		
		attr.clear();
    }

    /**
     * simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  tag  tag name
     * @param  val  tag value
     */
    public final void closedTag(String tag, String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML
            // charset
            String writeVal = val.trim().replaceAll("&", "&amp;");

            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            writeVal = writeVal.trim().replaceAll("<", "&lt;");
            writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(XML_ENCODING));

            bw.write("<" + tag + ">" + writeVal + "</" + tag + ">");
            bw.newLine();
        } catch (IOException ex) { }
    }
    
    /**
     * simple function to write an xml formatted open ended tag (value not included).
     *
     * @param  tag    tag name
     * @param  start  whether this is the start of a container tag
     */
    public final void openTag(String tag, boolean start) {

        try {

            if (!start) {

                // done with this container
                tabLevel--;
            }

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            if (start) {
                bw.write("<" + tag + ">");

                // indent the contained tags
                tabLevel++;
            } else {
                bw.write("</" + tag + ">");
            }

            bw.newLine();
        } catch (IOException ex) { }
    }
	
    /**
     * Class used to store an xml tag's attribute (name and value)
     *
     */
    public class XMLAttributes {
    	
    	private String name;
    	private String value;
    	
    	public XMLAttributes(String n, String v) {
    		name = n;
    		value = v;
    	}
    	
    	public String getName() {
    		return name;
    	}
    	public String getValue() {
    		return value;
    	}
    	
    }
    
}
