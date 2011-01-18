package gov.nih.mipav.view.graphVisualization;

import java.awt.event.*;
import java.util.Enumeration;
import java.util.Iterator;

import hypergraph.visualnet.GraphPanel;
import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Edge;
import hypergraph.graphApi.Element;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.Node;
import hypergraph.visualnet.DefaultNodeRenderer;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

public class MipavGraphPanel extends GraphPanel {

    /** */
	private static final long serialVersionUID = -5403703931672321741L;
	private ViewJFrameImage m_kImageFrame = null;
    public MipavGraphPanel( Graph kGraph, ViewJFrameImage kImageFrame )
    {
        super(kGraph);
        m_kImageFrame = kImageFrame;
        getEdgeRenderer().setLabelVisible(true);
        setSmallLogo(null);
        setLogo(null);
    }

    /** Called if the node is clicked.
     * @param iClickCount The number of clicks on the node.
     * @param kNode The node that has been clicked on.
     */
    public void nodeClicked(int iClickCount, Node kNode)
    {
        super.nodeClicked(iClickCount, kNode);
        /*
        System.err.println( kNode.getLabel() );
		AttributeManager attrMgr = getGraph().getAttributeManager();
		Iterator elements = attrMgr.getAttributeNames().iterator();
		while ( elements.hasNext() )
		{
			String kAttr = (String)elements.next();
    		if ( attrMgr.getAttribute( kAttr, kNode) != null )
    		{
    			System.err.println( kAttr + " " + 
    					attrMgr.getAttribute( kAttr, kNode) );
    		}
		}
         */
        if ( (iClickCount >= 2) && (m_kImageFrame != null) && (kNode != null) )
        {
        	if ( getGraph().getEdges(kNode).size() == 1 &&  kNode.getLabel() != null )
            {
                m_kImageFrame.actionPerformed( new ActionEvent( kNode, 0, kNode.getLabel() ) );
            }
        }
    }

    public void refreshText()
    {
    	setTextRenderer(null);
        if ( getNodeRenderer() != null )
        {
            ((DefaultNodeRenderer)getNodeRenderer()).setTextRenderer(null);
        }
    }

}
