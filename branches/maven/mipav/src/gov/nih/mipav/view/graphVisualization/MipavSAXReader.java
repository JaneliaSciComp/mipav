package gov.nih.mipav.view.graphVisualization;

import gov.nih.mipav.model.file.XMLErrorHandler;
import gov.nih.mipav.view.MipavUtil;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.GraphSystem;

import java.io.IOException;
import java.net.URL;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.xerces.jaxp.JAXPConstants;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;


/**
 *  SAX Reader for the HyperGraph display. Sets up the parser, which parses the graph xml file and produces a Graph.
 */
public class MipavSAXReader {
	private GraphSystem graphSystem;
	private InputSource inputSource;
	private Graph graph;
	private XMLReader reader;


	/**
	 * Creates the MipavSAXReader which sets up the XML parser for the graph xml file.
	 * @param graphSystem GraphSystem creates the Graph.
	 * @param url Input File
	 * @throws IOException
	 */
	public MipavSAXReader(GraphSystem graphSystem, URL url) throws IOException {
		this.graphSystem = graphSystem;
		inputSource = new InputSource(url.openStream());
		inputSource.setSystemId(url.toString());
	}
	
	/**
	 * Returns the Graph.
	 * @return Graph
	 */
	public Graph getGraph() {
		return graph;
	}
	
	/**
	 * Returns the GraphSystem.
	 * @return GraphSystem.
	 */
	public GraphSystem getGraphSystem() {
		return graphSystem;
	}
	
	/**
	 * Parses the XML file with the Graph description. Returns the new Graph.
	 * @return new Graph from file.
	 * @throws SAXException
	 * @throws ParserConfigurationException
	 */
	public Graph parse() throws SAXException, ParserConfigurationException {
        SAXParserFactory parserFactory = SAXParserFactory.newInstance();
        SAXParser parser = parserFactory.newSAXParser();

        // Set the schema language if necessary
        parser.setProperty(JAXPConstants.JAXP_SCHEMA_LANGUAGE, "http://www.w3.org/2001/XMLSchema");
        
        // Set the schema source, if any.  See the JAXP
        // 1.2 maintenance update specification for more complex usages of
        // this feature.
        URL xsdURL = getClass().getClassLoader().getResource("MIPAVGraphXML.xsd");
        
        if (xsdURL == null) {
            MipavUtil.displayError("Unable to find XML schema: MIPAVGraphXML.xsd");
            
            return null;
        }
        
        parser.setProperty(JAXPConstants.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

        reader = parser.getXMLReader();
        MipavGraphXMLContentHandler contentHandler = new MipavGraphXMLContentHandler();
        contentHandler.setReader(this);
        reader.setContentHandler( contentHandler );
        // Set an ErrorHandler before parsing
        reader.setErrorHandler(new XMLErrorHandler());
        
        try {
        	reader.parse(inputSource);
        } catch (IOException e)
        {
        	System.err.println( e.getStackTrace() );
        }
        return getGraph();
    }
	
	/**
	 * Sets the Graph.
	 * @param graph new Graph
	 */
	public void setGraph(Graph graph) {
		this.graph = graph;
	}
}
