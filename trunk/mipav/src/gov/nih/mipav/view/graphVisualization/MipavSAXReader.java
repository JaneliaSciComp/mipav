package gov.nih.mipav.view.graphVisualization;

import java.io.IOException;
import java.net.URL;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.apache.xerces.jaxp.*;

import hypergraph.graphApi.Graph;
import hypergraph.graphApi.GraphSystem;
import gov.nih.mipav.model.file.XMLErrorHandler;
import gov.nih.mipav.view.MipavUtil;


public class MipavSAXReader {
	private GraphSystem graphSystem;
	private InputSource inputSource;
	private Graph graph;
	private XMLReader reader;

	public MipavSAXReader(GraphSystem graphSystem, String filename) {
		this.graphSystem = graphSystem; 
		inputSource = new InputSource(filename);
	}
	public MipavSAXReader(GraphSystem graphSystem, URL url) throws IOException {
		this.graphSystem = graphSystem;
		inputSource = new InputSource(url.openStream());
		inputSource.setSystemId(url.toString());
	}
	public GraphSystem getGraphSystem() {
		return graphSystem;
	}
	public XMLReader getReader() {
		return reader;
	}
	public void setGraph(Graph graph) {
		this.graph = graph;
	}
	public Graph getGraph() {
		return graph;
	}
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
}
