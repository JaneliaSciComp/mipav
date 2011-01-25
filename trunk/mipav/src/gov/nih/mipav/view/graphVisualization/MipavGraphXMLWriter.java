package gov.nih.mipav.view.graphVisualization;

import gov.nih.mipav.model.file.FileVOI;
import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Node;
import hypergraph.graphApi.io.GraphXMLWriter;
import hypergraph.visualnet.GraphPanel;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.io.Writer;

public class MipavGraphXMLWriter extends GraphXMLWriter {
    public MipavGraphXMLWriter(File outputFile) throws IOException {
        super(outputFile);
    }
    public MipavGraphXMLWriter(String fileName) throws IOException {
        super(fileName);
    }
    public MipavGraphXMLWriter(Writer writer) throws IOException {
        super(writer);
    }

    @Override
	public void writeHeader() throws IOException {		
		writer.write("<?xml version=\"1.0\" encoding=\"" + "UTF-8" + "\"?>\n" );
		writer.write("<graph xmlns:xsi=\"" + "http://www.w3.org/2001/XMLSchema" + "-instance\">\n");
	}

    @Override
	public void writeFooter() throws IOException {
		writer.write("</graph>\n");
		writer.close();
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
        writer.write(">\n");
        writer.write("    </node>\n");
    }
}
