package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.model.file.FileSurfaceGiftiXML;
import gov.nih.mipav.model.file.FileUtility;


import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * Inherits from FileXML, reads gifti xml files based on the "gifti.xsd" file. Defines specific variables for
 * reading and writing gifti ascii xml files.  
 */
public class FileSurfaceGiftiXML_WM extends FileSurfaceGiftiXML {

	/** Gifti header string (for top of xml header). */
	protected static final String GIFTI_HEADER = "<!DOCTYPE GIFTI SYSTEM \"http://www.nitrc.org/frs/download.php/115/gifti.dtd\">";

	/** Intent value in the <DataArray> attributes. */
	private String intent;

	/** Coordinate vertex point counter. */
	private int vertexCount = 0;
	
	/** Connectivity index counter. */
	private int indexCount = 0;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	/**
	 * Used by the XML Parser to parse the Surface.XML header.
	 */
	private class SurfaceGifitXMLHandler extends DefaultHandler {

		/** Current XML keyword:. */
		String currentKey;

		/** Current buffer:. */
		String elementBuffer = new String();

		/** fileInfo data structure for reading the surface information into:. */
		FileInfoSurfaceGiftiXML_WM fileInfo;

		/**
		 * Creates a new SurfaceXMLHandler object.
		 *
		 * @param  fInfo  FileInfo for storing the file data into.
		 */
		public SurfaceGifitXMLHandler(FileInfoSurfaceGiftiXML_WM fInfo) {
			fileInfo = fInfo;
		}

		/**
		 * Text data callback from parser. If the parser is not validating, this method can report whitespace. We ignore
		 * strings that are entirely whitespace.
		 *
		 * @param  ch      Character array
		 * @param  start   Start of data in array.
		 * @param  length  Length of data in array.
		 */
		public void characters(char[] ch, int start, int length) {
			String s = new String(ch, start, length);

			if (s.trim().length() != 0) {
				elementBuffer += s;
			}
		}

		/**
		 * Do nothing.
		 *
		 * @throws  SAXException  never happens
		 */
		public void endDocument() throws SAXException {
		}
	
		/**
		 * Called by parser when the end of an element is reached in the document.
		 *
		 * @param   namespaceURI  the namespace uri
		 * @param   localName     the element name
		 * @param   qName         the qualified name
		 *
		 * @throws  SAXException  if a problem is encountered during parsing
		 */
		public void endElement(String namespaceURI, String localName,
				String qName) throws SAXException {
			currentKey = localName;
			if (currentKey.equals("GIFTI")) {
			} else if (currentKey.equals("Data")) {
				if ( intent.equals("NIFTI_INTENT_POINTSET") ) {   
					fileInfo.setCoordinate(getCoordinate(elementBuffer));
				} else if ( intent.equals("NIFTI_INTENT_TRIANGLE")){  
					fileInfo.setConnectivity(getConnectivity(elementBuffer));
				}
				
			} else if (currentKey.equals("DataArray")) {
			} else if (currentKey.equals("Dimensionality")) {
			} else if (currentKey.equals("Dim0")) {
			} else if (currentKey.equals("Dim1")) {
			} else if (currentKey.equals("Dim2")) {
			} else if (currentKey.equals("Dim3")) {
			} else if (currentKey.equals("Dim4")) {
			} else if (currentKey.equals("Dim5")) {
			} else if (currentKey.equals("Intent")) {
			} else if (currentKey.equals("ArrayIndexingOrder")) {
			} else if (currentKey.equals("DataType")) {
			} else if (currentKey.equals("Endian")) {
			} else if (currentKey.equals("ExternalFileName")) {
				// fileInfo.setSurfaceFileName(elementBuffer);
			} else if (currentKey.equals("ExternalFileOffset")) {
				// fileInfo.setSurfaceFileName(elementBuffer);
			} else if (currentKey.equals("Encoding")) {
			} else if (currentKey.equals("DataSpace")) {
			} else if (currentKey.equals("Label")) {
			} else if (currentKey.equals("LabelTable")) {
			} else if (currentKey.equals("MatrixData")) {
			} else if (currentKey.equals("MD")) {
			} else if (currentKey.equals("MetaData")) {
			} else if (currentKey.equals("TransformedSpace")) {
			} else if (currentKey.equals("Name")) {
			} else if (currentKey.equals("Value")) {
			} else if (currentKey.equals("CoordinateSystemTransformMatrix")) {
			} 	
		}
		
		/**
		 * Parse the coordinate point value from the <Data> section. 
		 * @param elementBuffer buffer hold the whole coordinate data
		 * @return coords    coordinate vector that hold the 3D positions.
		 */
		public Vector<Vector3f> getCoordinate(String elementBuffer) {
			Vector<Vector3f> coords = new Vector<Vector3f>();
			String[] strs = elementBuffer.split("\\s+");
            for(int i=0;i<strs.length;i+=3){
                try {
                	coords.add(new Vector3f(Float.parseFloat(strs[i]),
                                           	Float.parseFloat(strs[i+1]),
                                           	Float.parseFloat(strs[i+2]) ));
                	vertexCount++;
                 } catch(NumberFormatException e){
                    System.err.println("CANNOT FORMAT VERTS");
                    return null;
                }
            }		
			return coords;
		}
		
		/**
		 * Parse the connectivity index from the <Data> section. 
		 * @param elementBuffer
		 * @return
		 */
		public Vector<Integer> getConnectivity(String elementBuffer) {
			StringTokenizer str = new StringTokenizer(elementBuffer, " ");
			Vector<Integer> conn = new Vector<Integer>();
			int index;
			while ( str.hasMoreTokens() ) {
				index = Integer.valueOf(str.nextToken());
				conn.add(index); 	
			}
			indexCount = conn.size();
			return conn;
		}
		
		/**
		 * Called when writing the surface.xml file: Converts the input Color3f to a String for writing:
		 *
		 * @param   kColor  color object
		 *
		 * @return  the color in string format
		 */
		public String getColorString(ColorRGB kColor) {
			String kColorString = new String(kColor.R + " " + kColor.G + " "
					+ kColor.B);

			return kColorString;
		}

		/**
		 * Do nothing but show the entity we skipped.
		 *
		 * @param  name  the skipped entity name
		 */
		public void skippedEntity(String name) {
			System.out.println(name);
		}

		/**
		 * Parser calls this for the beginning of each element in the document.
		 *
		 * @param   namespaceURI  the namespace uri
		 * @param   localName     the element name
		 * @param   qName         the qualified name
		 * @param   atts          the attached attributes
		 *
		 * @throws  SAXException  if a problem is encountered during parsing
		 */
		public void startElement(String namespaceURI, String localName,
				String qName, org.xml.sax.Attributes atts) throws SAXException {
			int numAtts;
			elementBuffer = "";
		    if (localName.equals("DataArray")) {
		    	numAtts = atts.getLength();
		    	for ( int i = 0; i < numAtts; i++ ) {
		    		if ( atts.getQName(i).equals("Intent")) {
		    			intent = atts.getValue(i);
		    			break;
		    		}
		    	}
			} 
		}

		/**
		 * Called when reading the input surface.xml file: Parses the input string into a Color3f variable:
		 *
		 * @param   kParseString  input string containing the material color
		 *
		 * @return  the material color in Color3f format
		 */
		private ColorRGB getColor(String kParseString) {
			ColorRGB kColor = new ColorRGB();
			StringTokenizer kSt = new StringTokenizer(kParseString, " ");

			try {
				kColor.R = Float.parseFloat(kSt.nextToken());
				kColor.G = Float.parseFloat(kSt.nextToken());
				kColor.B = Float.parseFloat(kSt.nextToken());
			} catch (Exception nfex) {
				System.out.println(nfex.toString());
			}

			kSt = null;

			return kColor;
		}

	}

	/**
	 * Constructs new file object.
	 *
	 * @param  fName  File name.
	 * @param  fDir   File directory.
	 */
	public FileSurfaceGiftiXML_WM(String fName, String fDir) {
		super(fName, fDir);
		fileInfo = new FileInfoSurfaceGiftiXML_WM(fName, fDir,
				FileUtility.SURFACEREF_XML);
	}

	/**
	 * Prepares class for cleanup.
	 */
	public void finalize() {
		fileInfo = null;
		super.finalize();
	}

	/**
	 * Returns the FileInfoSurfaceRefXML read from the file.
	 *
	 * @return  File info read from file, or null if it has not been read.
	 */
	public FileInfoSurfaceGiftiXML_WM getFileInfo() {
		return (FileInfoSurfaceGiftiXML_WM) fileInfo;
	}

	/**
	 * Reads and parses a Gifti XML file.
	 *
	 * @param   headerFileName  file name of xml header
	 * @param   headerDir       directory
	 *
	 * @return  kMesh  Triangle mesh
	 *
	 * @throws  IOException  file exception
	 */
	public TriMesh readSurfaceXML(String headerFileName, String headerDir) {
		TriMesh kMesh = null;
		SurfaceGifitXMLHandler kHandler = null;

		// Set the ContentHandler of the XMLReader
		kHandler = new SurfaceGifitXMLHandler(
				(FileInfoSurfaceGiftiXML_WM) fileInfo);
		m_kHandler = kHandler;

		/* Pass the .xsd file to the base class for parsing: */
		if (super.readHeader(headerFileName, headerDir, "gifti.xsd") == null) {
			return null;
		}
		
		 //points
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetTChannels(0,3);
        kAttr.SetCChannels(0,4);
        VertexBuffer kVBuffer = new VertexBuffer( kAttr, vertexCount );
        Vector<Vector3f> coords = ((FileInfoSurfaceGiftiXML_WM)fileInfo).getCoordinate();
		for(int i=0;i<coords.size();i++){
            try {
            	Vector3f v = (Vector3f)coords.get(i);
                kVBuffer.SetPosition3( i,
                                       v.X,
                                       v.Y,
                                       v.Z );
                kVBuffer.SetColor4(0, i, 1f, 1f, 1f, 1f );
            } catch(NumberFormatException e){
                System.err.println("CANNOT FORMAT VERTS");
                return null;
            }
        }
		
		
		 //connection
        int count=0;
        int[] indices=new int[indexCount];
        Vector<Integer> conn = ((FileInfoSurfaceGiftiXML_WM)fileInfo).getConnectivity();
        for(int i=0;i<conn.size();i++){			
            try {
                indices[count++]= (Integer)conn.get(i);
            } catch(NumberFormatException e){
                System.err.println("CANNOT FORMAT INDICES");
                return null;
            }
        }
		
		 IndexBuffer kIBuffer = new IndexBuffer( indexCount, indices );
         kMesh=new TriMesh(kVBuffer, kIBuffer);
         kMesh.SetName(fileName);
         
         return kMesh;
         
	}

	/**
	 * Writes the XML file information, including the surface out to the given filename and path:
	 *
	 * @param   fileName   file name to write to
	 * @param   kMaterial    surface material
	 * @param   opacity      surface opacity
	 * @param   levelDetail  surface level of detail
	 * @return  if header write was successful
	 *
	 * @throws  IOException  if a file I/O problem is encountered while writing the header
	 */
	public boolean writeXMLsurface(String fileName, TriMesh kMesh)
			throws IOException {

		int dotIndex = fileName.lastIndexOf('.');
		int slashIndex = fileName.lastIndexOf('\\');
		String headerName = fileName.substring(slashIndex + 1, dotIndex);

		int pointCount = kMesh.VBuffer.GetVertexQuantity();
		int indexCount = kMesh.IBuffer.GetIndexQuantity();

		/* Create the SurfaceXMLHandler which processes the vertex, normal,
		 * connectivity arrays and colors for writing in the xml format: */
		SurfaceGifitXMLHandler kSurfaceXMLHandler = new SurfaceGifitXMLHandler(
				(FileInfoSurfaceGiftiXML_WM) fileInfo);

		/* Output file: */
		FileWriter fw;
		File headerFile;

		headerFile = new File(fileName);
		fw = new FileWriter(headerFile);
		bw = new BufferedWriter(fw);

		bw.write(XML_HEADER);
		bw.newLine();
		bw.write(GIFTI_HEADER);
		bw.newLine();

		// Open the GIFTI tag:
		openTag("GIFTI Version=\"" + 1.0 + "\" NumberOfDataArrays=\"" + 2
				+ "\"", true);

		/***************************************************************************************/
		// GIFIT->MetaData 
		openTag("MetaData", true);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("Caret-Version"));;
		closedTagWithCDATA("Value", getCDATA("5.512"));
		openTag("MD", false);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("date"));
		closedTagWithCDATA("Value", getCDATA("Tue Apr 15 09:18:46 2008"));
		openTag("MD", false);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("encoding"));
		closedTagWithCDATA("Value", getCDATA("XML"));
		openTag("MD", false);

		openTag("MetaData", false);

		// DataArray     3D point coordinates

		Vector<XMLAttributes> atVector = new Vector<XMLAttributes>();
		atVector = new Vector<XMLAttributes>();
		atVector.add(new XMLAttributes("Intent", "NIFTI_INTENT_POINTSET"));
		atVector.add(new XMLAttributes("DataType", "NIFTI_TYPE_FLOAT32"));
		atVector.add(new XMLAttributes("ArrayIndexingOrder", "RowMajorOrder"));
		atVector.add(new XMLAttributes("Dimensionality", "2"));
		atVector.add(new XMLAttributes("Dim0", new Integer(pointCount).toString()));
		atVector.add(new XMLAttributes("Dim1", "3"));
		atVector.add(new XMLAttributes("Encoding", "ASCII"));
		atVector.add(new XMLAttributes("Endian", "LittleEndian"));
		atVector.add(new XMLAttributes("ExternalFileName", ""));
		atVector.add(new XMLAttributes("ExternalFileOffset", ""));
		openTag("DataArray", atVector);

		// DataArray -> MetaData
		openTag("MetaData", true);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("AnatomicalStructurePrimary"));
		closedTagWithCDATA("Value", getCDATA("CortexLeft"));
		openTag("MD", false);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("AnatomicalStructureSecondary"));
		closedTagWithCDATA("Value", getCDATA("Pial"));
		openTag("MD", false);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("GeometricType"));
		closedTagWithCDATA("Value", getCDATA("Inflated"));
		openTag("MD", false);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("UniqueID"));
		closedTagWithCDATA("Value", getCDATA("{39e1e8f6-00fa-43be-b7c0-ec5cf54d6c57}"));
		openTag("MD", false);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("comment"));
		closedTagWithCDATA("Value", getCDATA(""));
		openTag("MD", false);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("pubmed_id"));
		closedTagWithCDATA("Value", getCDATA(""));
		openTag("MD", false);

		openTag("MetaData", false);

		/* CoordinateSystemTransformMatrix */
		openTag("CoordinateSystemTransformMatrix", true);
		closedTagWithCDATA("DataSpace", getCDATA("NIFTI_XFORM_TALAIRACH"));
		closedTagWithCDATA("TransformedSpace", getCDATA("NIFTI_XFORM_TALAIRACH"));
		StringBuffer buff = new StringBuffer();
		buff.append(String.format("\t%f %f %f %f\n", 1.0f, 0f, 0f, 0f));
		buff.append(String.format("\t%f %f %f %f\n", 0f, 1.0f, 0f, 0f));
		buff.append(String.format("\t%f %f %f %f\n", 0f, 0f, 1.0f, 0f));
		buff.append(String.format("\t%f %f %f %f\n", 0f, 0f, 0f, 1.0f));
		closedTagGifti("MatrixData", buff.toString());
		openTag("CoordinateSystemTransformMatrix", false);

		/* Data */
		Vector3f p = new Vector3f();
		buff = new StringBuffer();
		for (int i = 0; i < pointCount; i++) {
			kMesh.VBuffer.GetPosition3(i, p);
			// buff.append(String.format(" %f %f %f", p.X, p.Y, p.Z));
			buff.append(String.format("%.5f %.5f %.5f ", p.X,p.Y,p.Z));
		}
		closedTagGifti("Data", buff.toString());

		openTag("DataArray", false);

		//  DataArray,  connectivity
		atVector = new Vector<XMLAttributes>();
		atVector.add(new XMLAttributes("Intent", "NIFTI_INTENT_TRIANGLE"));
		atVector.add(new XMLAttributes("DataType", "NIFTI_TYPE_INT32"));
		atVector.add(new XMLAttributes("ArrayIndexingOrder", "RowMajorOrder"));
		atVector.add(new XMLAttributes("Dimensionality", "2"));
		atVector.add(new XMLAttributes("Dim0", new Integer(indexCount).toString()));
		atVector.add(new XMLAttributes("Dim1", "3"));
		atVector.add(new XMLAttributes("Encoding", "ASCII"));
		atVector.add(new XMLAttributes("Endian", "LittleEndian"));
		atVector.add(new XMLAttributes("ExternalFileName", ""));
		atVector.add(new XMLAttributes("ExternalFileOffset", ""));
		openTag("DataArray", atVector);

		openTag("MetaData", true);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("TopologicalType"));
		closedTagWithCDATA("Value", getCDATA("Closed"));
		openTag("MD", false);

		openTag("MD", true);
		closedTagWithCDATA("Name", getCDATA("UniqueID"));
		closedTagWithCDATA("Value", getCDATA("{245b82f9-647e-4d82-afab-d40bf71b49be}"));
		openTag("MD", false);

		openTag("MetaData", false);

		buff = new StringBuffer();
		int[] aiIndex = kMesh.IBuffer.GetData();
		for (int i = 0; i < indexCount; i += 3) {
			buff.append(aiIndex[i] + " " + aiIndex[i+1] + " " + aiIndex[i+2] + " ");
		}
		closedTagGifti("Data", buff.toString());

		openTag("DataArray", false);
		
		openTag("GIFTI", false);
		
		bw.close();

		kSurfaceXMLHandler = null;

		return true;
	}
	
	 
    /**
	 * Writes a closed tag where no value is specified, only attributes.
	 */
	public void openTag(String tag, Vector<XMLAttributes> attr) {
    	
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
            	if ( i == (attr.size()-1) ) {
            		bw.write(" " + attr.elementAt(i).getName() + "=\"" + attrStr + "\"");
            	} else {
            		bw.write(" " + attr.elementAt(i).getName() + "=\"" + attrStr + "\"");
            	}
            }
            
            bw.write(">");

            bw.newLine();
        } catch (IOException ex) { }
		
		attr.clear();
    }
    
	
	 /**
     * Simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  bw   write to use
     * @param  tag  tag name
     * @param  val  tag value
     */
    protected void closedTagGifti(String tag, String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML charset
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
     * Simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  bw   write to use
     * @param  tag  tag name
     * @param  val  tag value
     */
    protected void closedTagWithCDATA(String tag, String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML charset
            String writeVal = val.trim().replaceAll("&", "&amp;");
            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            // writeVal = writeVal.trim().replaceAll("<", "&lt;");
            // writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(XML_ENCODING));

            bw.write("<" + tag + ">" + writeVal + "</" + tag + ">");
            bw.newLine();
        } catch (IOException ex) { }
    }
    
    
	/**
	 * Converting string value to CDATA format.
	 * @param value string
	 * @return CDATA section format string
	 */
	public String getCDATA(String value) {
	    StringBuffer result =  new StringBuffer();
		result.append("<![CDATA[");
		result.append(value);
		result.append("]]>");
		return result.toString();
	}
	
	
}
