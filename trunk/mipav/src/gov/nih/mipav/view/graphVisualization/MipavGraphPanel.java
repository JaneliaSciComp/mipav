package gov.nih.mipav.view.graphVisualization;

import java.awt.event.*;
import hypergraph.visualnet.GraphPanel;
import hypergraph.graphApi.AttributeManager;
import hypergraph.graphApi.Edge;
import hypergraph.graphApi.Element;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.Node;

import gov.nih.mipav.view.ViewJFrameImage;

public class MipavGraphPanel extends GraphPanel {

    /** */
	private static final long serialVersionUID = -5403703931672321741L;
	private ViewJFrameImage m_kImageFrame = null;
    public MipavGraphPanel( Graph kGraph, ViewJFrameImage kImageFrame )
    {
        super(kGraph);
        m_kImageFrame = kImageFrame;
        getEdgeRenderer().setLabelVisible(true);
    }

    /** Called if the node is clicked.
     * @param iClickCount The number of clicks on the node.
     * @param kNode The node that has been clicked on.
     */
    public void nodeClicked(int iClickCount, Node kNode)
    {
        super.nodeClicked(iClickCount, kNode);

        if ( (iClickCount >= 2) && (m_kImageFrame != null) && (kNode != null) )
        {
        	if ( getGraph().getEdges(kNode).size() == 1 &&  kNode.getLabel() != null )
            {
                m_kImageFrame.actionPerformed( new ActionEvent( kNode, 0, kNode.getLabel() ) );
            }
        }
    }

}
