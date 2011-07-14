package gov.nih.mipav.view.graphVisualization;

import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Node;
import hypergraph.graphApi.io.GraphXMLWriter;
import hypergraph.visualnet.GraphPanel;

import java.awt.Color;
import java.io.File;
import java.io.IOException;

/**
 * Writes the mipav graph to .xml format.
 * As the mipav graph format changes this is where new node attributes are written.
 */
public class MipavGraphXMLWriter extends GraphXMLWriter {
	
    /**
     * Writes the Graph in the Mipav Graph XML format.
     * @param outputFile output file.
     * @throws IOException
     */
    public MipavGraphXMLWriter(File outputFile) throws IOException {
        super(outputFile);
    }

    @Override
	public void writeFooter() throws IOException {
		writer.write("</graph>\n");
		writer.close();
	}

    @Override
	public void writeHeader() throws IOException {		
		writer.write("<?xml version=\"1.0\" encoding=\"" + "UTF-8" + "\"?>\n" );
		writer.write("<graph xmlns:xsi=\"" + "http://www.w3.org/2001/XMLSchema" + "-instance\">\n");
	}
	
    @Override
	public void writeNode(Node node) throws IOException {  	
		AttributeManager attrMgr = graph.getAttributeManager();
		Color kColor = (Color)attrMgr.getAttribute( GraphPanel.NODE_FOREGROUND, node );		
		String kColorString = null;
		if ( kColor != null )
		{
			kColorString = new String( "#" + Integer.toHexString(kColor.getRGB()).substring(2) );	
		}
		String kNotes = (String)attrMgr.getAttribute( "ANNOTATION", node );	
			
        writer.write(
                     "    <node " + 
                     "name=\"" + node.getName() + "\" ");
        if (node.getGroup() != null) 
            writer.write("class=\"" + node.getGroup().getName() + "\" ");
        if ( kColorString != null )
        {
        	writer.write("color=\"" + kColorString + "\" ");
        }
        if ( node.getLabel() != null )
        {
        	writer.write("label=\"" + node.getLabel() + "\" ");
        }
        if ( kNotes != null )
        {
        	writer.write("ANNOTATION=\"" + kNotes + "\" ");
        }
        writer.write(">\n");
        writer.write("    </node>\n");
    }
}
