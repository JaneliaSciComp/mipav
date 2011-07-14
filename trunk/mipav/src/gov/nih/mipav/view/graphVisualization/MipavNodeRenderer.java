package gov.nih.mipav.view.graphVisualization;

import hypergraph.graphApi.Node;
import hypergraph.hyperbolic.ModelPoint;
import hypergraph.visualnet.DefaultNodeRenderer;
import hypergraph.visualnet.GraphPanel;

import java.awt.Color;
import java.awt.Font;

/**
 * Overrides how the Nodes are displayed in the HyperGraph. Add/modify to change the Node display for MIPAV graphs. 
 */
public class MipavNodeRenderer extends DefaultNodeRenderer {

    /** generated serial id */
	private static final long serialVersionUID = 1686126148353845051L;

	@Override
	public void configure(GraphPanel c, ModelPoint mp, Node node) {
        super.configure(c, mp, node );
        // This sets each node so that it is highlighted, with the node clearly distinguishable from the
        // intersecting edge lines.
        if (getBackground() == null) {
            Color back = c.getBackground();
            setBackground(new Color(back.getRed(), back.getGreen(), back.getBlue(), 224));
        }
        if ( getFont() == null )
        {
        	setFont(c.getFont().deriveFont(Font.BOLD));
        }
        else
        {
        	setFont(getFont().deriveFont(Font.BOLD));
        }
    }
}
