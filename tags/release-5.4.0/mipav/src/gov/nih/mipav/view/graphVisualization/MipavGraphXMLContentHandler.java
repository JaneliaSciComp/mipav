package gov.nih.mipav.view.graphVisualization;


import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Edge;
import hypergraph.graphApi.Element;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.GraphException;
import hypergraph.graphApi.Group;
import hypergraph.graphApi.Node;
import hypergraph.graphApi.io.CSSColourParser;
import hypergraph.visualnet.GraphPanel;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;


/**
 * Parses the MIPAX Graph XML file.
 */
public class MipavGraphXMLContentHandler extends DefaultHandler {

	private MipavSAXReader reader;
	protected Graph graph;
	protected Element currentElement;
	protected StringBuffer currentText;
	
	/**
     * Creates the content handler.
     */
    public MipavGraphXMLContentHandler() {
		super();
	}
	
	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
	 */
	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (currentText == null)
			currentText = new StringBuffer();
		currentText.append(ch, start, length);
	}
	
	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#endDocument()
	 */
	@Override
	public void endDocument() throws SAXException {
		reader.setGraph(graph);
	}
	
	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
		if (qName.equals("graph")) {
			endElementGraph();
			return;
		}
		if (qName.equals("node")) {
			endElementNode();
			return;
		}
		if (qName.equals("edge")) {
			endElementEdge();
			return;
		}
		if (qName.equals("label")) {
			endElementLabel();
			return;
		}
	}
	
	/**
	 * Returns the MipavSAXReader.
	 * @return
	 */
	public MipavSAXReader getReader() {
		return reader;
	}
	
	/**
	 * Sets the MipavSAXReader. 
	 * @param reader
	 */
	public void setReader(MipavSAXReader reader) {
		this.reader = reader;
	}
	
	/* (non-Javadoc)
	 * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
	 */
	@Override
	public void startElement(String namespaceURI, String localName, String qName, Attributes atts) throws SAXException {
		if (qName.equals("graph")) {
			startElementGraph(atts);
			return;
		}
		if (qName.equals("node")) {
			startElementNode(atts);
			return;
		}
		if (qName.equals("edge")) {
			startElementEdge(atts);
			return;
		}
		if (qName.equals("label")) {
			startElementLabel(atts);
			return;
		}
	}
	
	/**
	 * End Edge element. 
	 */
	protected void endElementEdge() {
		currentElement = null;
	}
	
	/**
	 * Ends the Graph.
	 */
	protected void endElementGraph() {}

	/**
	 * Called when the end of the Label element is found. 
	 */
	protected void endElementLabel() {
		if (currentText != null && currentElement != null) {
			if (currentElement instanceof Node) {
				((Node) currentElement).setLabel(currentText.toString());
			}
			if (currentElement instanceof Edge) {
				((Edge) currentElement).setLabel(currentText.toString());
			}
		}
	}
	
	/**
	 * Called when the end of the Node Element is found.
	 */
	protected void endElementNode() {
		currentElement = null;
	}
	
	/**
	 * Called when the Edge start element is found.
	 * @param atts
	 */
	protected void startElementEdge(Attributes atts) {
		String name = null;
		Group group = null;
		Node source = null;
		Node target = null;
		boolean isDirected = false;
		for (int i = 0; i < atts.getLength(); i++) {
			if (atts.getQName(i).equals("isDirected")) {
				String dirAttr = atts.getValue(i);
				if (dirAttr.compareToIgnoreCase("true") == 0)
					isDirected = true;
				continue;
			}
			if (atts.getQName(i).equals("name")) {
				name = atts.getValue(i);
				continue;
			}
			if (atts.getQName(i).equals("class")) {
				String groupName = atts.getValue(i);
				if (groupName != null && groupName.length() > 0) {
					group = (Group) graph.getElement(groupName);
					if (group == null)
						try {
							group = graph.createGroup(groupName);
						} catch (GraphException ge) {
							ge.printStackTrace();
						}
				}
				continue;
			}
			if (atts.getQName(i).equals("source")) {
				String sourceName = atts.getValue(i);
				source = (Node) graph.getElement(sourceName);
				if (source == null)
					try {
						source = graph.createNode(sourceName);
					} catch (GraphException ge) {
						ge.printStackTrace();
					}
				continue;
			}
			if (atts.getQName(i).equals("target")) {
				String targetName = atts.getValue(i);
				target = (Node) graph.getElement(targetName);
				if (target == null)
					try {
						target = graph.createNode(targetName);
					} catch (GraphException ge) {
						ge.printStackTrace();
					}
				continue;
			}
		}
		try {
			currentElement = graph.createEdge(name, source, target);
			((Edge) currentElement).setDirected(isDirected);
			if (group != null)
				currentElement.setGroup(group);
		} catch (GraphException ge) {
			ge.printStackTrace();
		}
	}
	
	/**
	 * Called when the Graph start element is found.
	 * @param atts
	 */
	protected void startElementGraph(Attributes atts) {
		graph = reader.getGraphSystem().createGraph();
	}
	
	/**
	 * Called when the start of the Label element is found.
	 * @param atts
	 */
	protected void startElementLabel(Attributes atts) {
		currentText = new StringBuffer();
	}
	
    /**
	 * Called when the start of a Node Element is found.
	 * @param atts
	 */
	protected void startElementNode(Attributes atts) {
        String name = null;
        Group group = null;
        String color = null;
        String label = null;
        String notes = null;
        for (int i=0; i<atts.getLength(); i++) {
            if (atts.getQName(i).equals("name")) {
                name = atts.getValue(i);
                continue;
            }
            if (atts.getQName(i).equals("class")) {
                String groupName = atts.getValue(i);
                if (groupName != null && groupName.length() > 0) {
                    group = (Group) graph.getElement(groupName);
                    if (group == null)
                        try {
                            group = graph.createGroup(groupName);
                        } catch (GraphException ge) {
                            ge.printStackTrace();
                        }
                }
                continue;
            }
            if (atts.getQName(i).equals("color")) {
            	color = atts.getValue(i);
                continue;
            }
            if (atts.getQName(i).equals("label")) {
            	label = atts.getValue(i);
                continue;
            }
            if (atts.getQName(i).equals("ANNOTATION")) {
            	notes = atts.getValue(i);
                continue;
            }
        }
        try {
            // the node may already exist because it has been created when an edge is defined.
            currentElement = graph.getElement(name);
            if (currentElement == null)
                currentElement = graph.createNode(name);
            if (group != null)
                currentElement.setGroup(group);
            if (color != null)
            {
        		AttributeManager attrMgr = graph.getAttributeManager();
				attrMgr.setAttribute( GraphPanel.NODE_FOREGROUND, currentElement,
						CSSColourParser.stringToColor(color) );		
            }
            if ( label != null )
            {
            	((Node)currentElement).setLabel(label);
            }
            if ( notes != null )
            {
        		AttributeManager attrMgr = graph.getAttributeManager();
				attrMgr.setAttribute( "ANNOTATION", currentElement, notes );		            	
            }
        } catch (GraphException ge) {
            ge.printStackTrace();
        }
    }

}
